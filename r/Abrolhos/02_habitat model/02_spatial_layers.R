###
# Project: ** **
# Data:    Bathymetry Data
# Task:    Prepare spatial layers for modelling
# Author:  Kingsley Griffin & Claude Spencer
# Date:    ** **
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

library(starsExtra)

# Set your study name
name <- "Abrolhos"                                                              # Change here

# Set CRS for bathymetry data
wgscrs <- "+proj=longlat +datum=WGS84 +south"                              # Latlong projection 
       
# This next section uses coarse GA bathymetry, replace if you have better bathymetry data (ie. multibeam or LiDAR)
# Read in and merge GA coarse bathy tiles from https://ecat.ga.gov.au/geonetwork/srv/eng/catalog.search#/metadata/67703
cbaths      <- list.files("data/spatial/rasters/raw bathymetry",                # Bathymetry data too large for git stored here
                     "*tile", full.names = TRUE) 
cbathy      <- lapply(cbaths,                                                   # Loads all of the tiles
                 function(x){read.table(file = x, header = TRUE, sep = ",")})    
cbathy      <- do.call("rbind", lapply(cbathy, as.data.frame))                  # Turns the list into a data frame
cbathy      <- cbathy[cbathy$Z <= 0 & cbathy$X < 117, ]                         # Get rid of topography data above 0m, general crop to speed life up
bath_r      <- rast(cbathy)                                                      # Convert to a raster
crs(bath_r) <- wgscrs                                                           # Set the CRS
plot(bath_r)                                                                    # Plot to check everything looks ok

# Crop the bathymetry to the general study area
# tbath <- projectRaster(bath_r, crs = sppcrs)
# tbath_c <- crop(tbath, extent(c(105000, 165000, 6880000, 7000000)))
tbath_c <- crop(bath_r, ext(c(112.978992634, 113.622204887,-28.146712369, -27.081850499)))
plot(tbath_c)
fbath_df <- as.data.frame(tbath_c, xy = TRUE)                                   # Convert this to a dataframe
saveRDS(fbath_df, paste(paste0('data/spatial/rasters/',                         # Save it for use in the next scripts
                               name), 'ga_bathy.rds', sep = "_")) 

# Calculate terrain derivatives
preds <- terrain(tbath_c, neighbors = 8,
                 v = c("slope", "aspect", "TPI", "TRI", "roughness"),
                 unit = "degrees")         # Remove here as necessary
preds <- rast(list(tbath_c, preds))                                                  # Stack the derivatives with the bathymetry

# Calculate detrended bathymetry
zstar <- st_as_stars(tbath_c)                                                   # Convert to a stars object
detre <- detrend(zstar, parallel = 8)                                           # Detrend bathymetry - This usually runs quite slow!
detre <- as(object = detre, Class = "SpatRaster")                                   # Convert it to a raster
names(detre) <- c("detrended", "lineartrend")
preds <- rast(list(preds, detre))                                                    # Make a rasterstack
plot(preds)
preds <- wrap(preds)

# Save the output
saveRDS(preds, paste(paste0('data/spatial/rasters/', name), 'spatial_covariates.rds', sep = "_"))

# rstudioapi::navigateToFile("add script name here.R")