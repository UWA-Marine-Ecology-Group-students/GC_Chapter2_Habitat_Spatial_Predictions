


# Clear your environment
rm(list = ls())

# Load libraries
library(dplyr)
library(reshape2)
library(mgcv)
library(ggplot2)
library(viridis)
library(terra)
#library(dismo)

# Set your study name
name <- "SouthWest"                                                              # Change here

# Set up WGSCRS and load spatial covariates from 02_spatial_layers.R 
wgscrs <- "+proj=longlat +datum=WGS84 +south"        






# read in spatial predictions from 'R/05_habitat_model.R'
spreddf <- readRDS(paste(paste0('output/SWC/', name),
                         'spatial_habitat_predictions.rds', sep = "_"))

write.csv(spreddf, file = paste(paste0('output/SWC/', name), 'spatial_habitat_predictions.csv', sep = "_"), row.names = FALSE)
