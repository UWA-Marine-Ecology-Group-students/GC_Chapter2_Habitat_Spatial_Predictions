###
# Project: ** GC_Ch2_Habitat_PtCloates**
# Data:    Bathymetry Data
# Task:    Prepare spatial layers for modelling
# Author:  Kingsley Griffin & Claude Spencer & Gabrielle Cummins
# Date:    **30/11/2022 **
## 

# This script formats bathymetry data and extracts bathymetry derivatives for modelling habitat and fish
# As this raw bathymetry data is often too large for GitHub, the raw files are hidden in the .gitnore
# You have to download data and create this folder directory yourself for the script to run!

# Clear your environment
rm(list = ls())

# Load libraries - some more to add here
library(sp)
library(terra)
library(sf)
library(stars)
library(raster)

library(starsExtra)

# Set your study name
name <- "PtCloates"                                                              # Change here

# Set CRS for bathymetry data
wgscrs <- "+proj=longlat +datum=WGS84 +south"                              # Latlong projection 


##GABBY TRIAL SCRIPT
#read in bathy
bathy <- rast("data/spatial/rasters/raw bathymetry/depth_195_50m_clipped.tif") %>%
  clamp(upper = -1, values = F) %>%
  trim()
plot(bathy)
summary(bathy)

#crop bathy
fbath_df <- as.data.frame(bathy, xy = TRUE, na.rm = T)                          # Convert to a dataframe
saveRDS(fbath_df, paste(paste0('data/spatial/rasters/',                         # Save it for use in the next scripts
                               name), 'multi_bathy.rds', sep = "_")) 


# cbaths <- c("data/spatial/rasters/raw bathymetry/depth_195_50m_clipped.tif")
# bath_r <- rast(cbaths)
# plot(bath_r)
# 
# G <- projectRaster(bath_r, proj4string(cbaths))
# new_bath_r <- spTransform(bath_r, CRS("init=epsg:4326"))

# # This next section uses coarse GA bathymetry, replace if you have better bathymetry data (ie. multibeam or LiDAR)
# # Read in and merge GA coarse bathy tiles from https://ecat.ga.gov.au/geonetwork/srv/eng/catalog.search#/metadata/67703
# cbaths      <- list.files("data/spatial/rasters/raw bathymetry",                # Bathymetry data too large for git stored here
#                      "*tile", full.names = TRUE)
# cbathy      <- lapply(cbaths,                                                   # Loads all of the tiles
#                  function(x){read.table(file = x, header = TRUE, sep = ",")})
# cbathy      <- do.call("rbind", lapply(cbathy, as.data.frame))                  # Turns the list into a data frame
# cbathy      <- cbathy[cbathy$Z <= 0 & cbathy$X < 117, ]                         # Get rid of topography data above 0m, general crop to speed life up
# bath_r      <- rast(cbathy)                                                      # Convert to a raster
# crs(bath_r) <- wgscrs                                                           # Set the CRS
# plot(bath_r)                                                                    # Plot to check everything looks ok
# 
# # Crop the bathymetry to the general study area
# tbath <- projectRaster(bath_r, crs = sppcrs)
# tbath_c <- crop(tbath, extent(c(105000, 165000, 6880000, 7000000)))
# lats <- read.csv("data/tidy/2021-05_PtCloates_synthesis_random-points_broad.habitat.csv") %>%
#   glimpse()
# 
# min(lats$latitude)
# max(lats$latitude)
# min(lats$longitude)
# max(lats$longitude)
# 
# tbath_c <- crop(bath_r, ext(c(113.2, 114.3,-23, -21.5)))
# plot(tbath_c)
# #points(lats[,c("longitude","latitude")], pch = 20, cex = 1, col = "red")
# fbath_df <- as.data.frame(tbath_c, xy = TRUE)                                   # Convert this to a dataframe
# saveRDS(fbath_df, paste(paste0('data/spatial/rasters/',                         # Save it for use in the next scripts
#                                name), 'ga_bathy.rds', sep = "_")) 

# Calculate terrain derivatives
preds <- terrain(bathy, neighbors = 8,
                 v = c("slope", "aspect", "TPI", "TRI", "roughness"),
                 unit = "degrees")         # Remove here as necessary
preds <- rast(list(bathy, preds))                                                  # Stack the derivatives with the bathymetry
plot(preds)
# Calculate detrended bathymetry
zstar <- st_as_stars(bathy)                                                   # Convert to a stars object
detre <- detrend(zstar, parallel = 8)                                           # Detrend bathymetry - This usually runs quite slow!
detre <- as(object = detre, Class = "SpatRaster")                                   # Convert it to a raster
names(detre) <- c("detrended", "lineartrend")
preds <- rast(list(preds, detre))                                                    # Make a rasterstack
plot(preds)
names(preds)[1] <- "Z"
preds <- terra::wrap(preds)

# Save the output
saveRDS(preds, paste(paste0('data/spatial/rasters/', name), 'multi_spatial_covariates.rds', sep = "_"))

# rstudioapi::navigateToFile("add script name here.R")
