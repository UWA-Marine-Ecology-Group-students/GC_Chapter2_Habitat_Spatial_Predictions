###
# Project: ** GC_PhD_Chapter 2 Habitat Spatial Predictions**
# Data:    BRUVS, BOSS Habitat data
# Task:    Merging habitat data
# Author:  Kingsley Griffin & Claude Spencer
# Date:    ** 14/09/2022 **
##

# This script takes formatted habitat data from TransectMeasure and joins it with bathymetry derivatives for modelling

# Clear your environment
rm(list = ls())

# Load libraries
library(reshape2)
library(dplyr)
library(terra)
library(sp)
library(ggplot2)

# Set your study name
name <- "SouthWest" # Change here

# Load in tidy data from the formatting scripts
hab <- read.csv("data/tidy/2020-2021_south-west_BOSS-BRUV.Habitat.csv") %>%
  mutate(id = paste(campaignid, sample, sep = "_"),
         depth = depth * -1) %>%
  glimpse()

hab_filtered <- hab %>% 
  filter(!campaignid %in% c("2021-03_West-Coast_BOSS"))
#boss <- read.csv("data/tidy/GC_2020-2021_south-west_BOSS.Habitat.csv") %>% # Need to type filename manually
 # dplyr::mutate(method = "BOSS") %>%                                            # Change here
  #dplyr::filter(location %in% "NPZ6") %>%                                       # Only Shallow Bank
#  glimpse()

#bruv <- read.csv("data/tidy/GC_2020_south-west_BRUVs.Habitat.csv") %>% # Need to type filename manually
 # dplyr::mutate(method = "BRUV") %>%                                            # Change here
  #dplyr::filter(location %in% "NPZ6") %>%                                       # Only Shallow Bank
#  glimpse()

#hab  <- bind_rows(boss, bruv)                                                   # Join together BOSS and BRUV data

# Extract bathy derivatives for modelling
# Set up CRS and load spatial covariates from 02_spatial_layers.R 
wgscrs <- "+proj=longlat +datum=WGS84 +south"                              # Latlong projection 
preds  <- readRDS(paste(paste0('data/spatial/rasters/', name), 
                       'spatial_covariates.rds', sep = "_"))
#preds <- rast(preds)

# Align crs and check samples over bathy and extract terrain data
allhab_sp <- vect(hab, geom = c("longitude", "latitude"), crs = wgscrs, keep = T)           # Convert to a spat vector

# allhab_t <- spTransform(allhab_sp, CRS = sppcrs)
plot(preds[[1]])                                                                # Plot the first bathymetry derivative
plot(allhab_sp, add = T)                                                        # Add the sampling points to check if they align
habt_df   <- as.data.frame(allhab_sp)                                           # Convert the habitat data back to a regular dataframe
habi_df   <- cbind(habt_df, terra::extract(preds, allhab_sp))                  # Extract the bathymetry derivatives and join on as a dataframe

# Rename columns and combine habitat columns for modelling
# Change this for your project needs!!
allhab <- habi_df %>%
  dplyr::rename(seagrasses = broad.seagrasses,                                            # Rename columns to simplify setting models
                macroalgae = broad.macroalgae,
                reef = broad.reef,
                sand = broad.unconsolidated,
                rock = broad.consolidated) %>%
  dplyr::mutate(inverts = broad.sponges + broad.octocoral.black +               # Make a sessile invertebrate column
                  broad.invertebrate.complex + broad.hydroids + 
                  broad.bryozoa + broad.ascidians + broad.crinoids + broad.true.anemones + broad.stony.corals) %>%
  glimpse()                                                                     # Preview data

# Save the output
saveRDS(allhab, paste(paste0('data/tidy/', name), 
                      'habitat-bathy-derivatives.rds', sep = "_"))

