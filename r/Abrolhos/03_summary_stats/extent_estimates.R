#  Project: ** GC PhD Chapter 2 Habitat Summary Stats  **
# Data:    BRUVS, BOSS Habitat data
# Task:    Habitat summary stats for results table
# author:  Kingsley Griffin & Gabby Cummins
# date:    ** 05/10/2022 **

rm(list = ls())

# libraries
library(raster)

# load data

rastdf <- readRDS("output/Abrolhos/Abrolhos_spatial_habitat_predictions.rds")

test   <- rasterFromXYZ(rastdf, )
plot(test$pinverts)

names(test)
