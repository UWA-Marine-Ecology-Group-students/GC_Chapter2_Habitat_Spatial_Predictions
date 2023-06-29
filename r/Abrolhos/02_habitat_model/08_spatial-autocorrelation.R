###
# Project: SW Habitat & Fish 
# Data:    BRUVS, BOSS Habitat data
# Task:    Merging habitat data
# Author:  Claude Spencer
# Date:    May 2023
##

library(spdep)
library(sp)
library(gstat)
library(ncf)
library(tidyverse)
library(terra)

rm(list = ls())

albcrs <- "EPSG:9473" 
wgscrs <- "+proj=longlat +datum=WGS84 +south"

name <- "Abrolhos"

# Load in habitat
dat <- readRDS(paste(paste0('data/tidy/', name), 
                     'habitat-bathy-derivatives.rds', sep = "_")) %>%
  pivot_longer(cols = c("sand", "inverts", "rock", "macroalgae"),
               names_to = "classes", values_to = "count") %>%
  dplyr::mutate(count = count/broad.total.points.annotated) %>%
  glimpse()

# You have to transform the data into a format measured in metres
dat.v <- vect(dat %>% dplyr::select(-c(x, y)),  # lat and long in the original dataframe twice - once as 'lat' once as 'y'
              geom = c("longitude", "latitude"), crs = wgscrs)
plot(dat.v)
dat.t <- project(dat.v, albcrs) # Transform data to metres
plot(dat.t)

dat <- as.data.frame(dat.t, geom = "XY") %>%
  glimpse()

# Filter data for one taxa/species
taxa <- unique(dat$classes)
for (i in 1:length(taxa)) {
  print(taxa[i])
  
  # Filter the data for each species
  test.dat <- dat %>%
    dplyr::filter(classes %in% taxa[i])
  
  # Send data to spatialPointsDataFrame
  coordinates(test.dat) <- ~x + y
  coords <- coordinates(test.dat)
  # Neighbours list from an sp object
  nb <- tri2nb(coords, row.names = NULL)
  # Moran's I
  moran.i <- moran.test(test.dat$count, nb2listw(nb))
  
  # Plot of Moran's I vs. Distance
  species <- spline.correlog(x = coordinates(test.dat)[,1], y = coordinates(test.dat)[,2],
                            z = test.dat$count, resamp = 100, xmax = F, quiet = TRUE, latlon = F)
  x <- species$real$predicted$x
  y <- species$real$predicted$y
  lCI <- species$boot$boot.summary$predicted$y["0.025",]
  uCI <- species$boot$boot.summary$predicted$y["0.975",]
  xx <- c(x,rev(x))
  yy <- c(lCI,rev(uCI))
  png(filename = paste0("plots/Abrolhos/ac/", taxa[i], ".png"), width = 6, height = 4, units = "in", res = 300)
  plot(x,y, xlim = c(0,species$max.distance), ylim = c(-1, 1), type = "n", 
       xlab = "Distance (m)", ylab = "Moran's I", main = str_to_title(taxa[i]))
  polygon(xx,yy,col="grey",border=NA)
  abline(h=0,lty="dashed")
  lines(x,y)
  dev.off()
  if (i == 1) {
    spat.autocor <- data.frame(moran.i$estimate) %>%
      tibble::rownames_to_column("statistic") %>%
      pivot_wider(names_from = statistic, values_from = moran.i.estimate) %>%
      dplyr::mutate(taxa = taxa[i],
                    p.value = moran.i$p.value)
  }
  else {
    spat.autocor <- data.frame(moran.i$estimate) %>%
      tibble::rownames_to_column("statistic") %>%
      pivot_wider(names_from = statistic, values_from = moran.i.estimate) %>%
      dplyr::mutate(taxa = taxa[i],
                    p.value = moran.i$p.value) %>%
      bind_rows(spat.autocor)
  }
}

write.csv(spat.autocor, paste0("output/Abrolhos/", name, "_spatial-autocorrelation.csv"), 
          row.names = F)
