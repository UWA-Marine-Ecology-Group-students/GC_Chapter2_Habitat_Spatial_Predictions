#seamap figure 
###
# Project: G Cummins Habitat Paper
# Data:    Seamap national benthic habitat layer
# Task:    seamap figure
# author:  Claude Spencer 
# date:    January 2023
##

# Clear your environment
rm(list = ls())

# Load libraries
library(dplyr)
library(sf)
library(rgeos)
library(rnaturalearth)
library(ggplot2)
library(metR)
library(stringr)
library(patchwork)
library(terra)
library(ggnewscale)
library(GlobalArchive)
library(tidyverse)
library(viridis)
library(geosphere)

# Set your study name
name <- "GC_Chapter2_Habitat_Spatial_Predictions" 

# Your data
depths <- c("<40", "40 - 100", ">100")
percentages <- c(89.35058734, 9.05870954, 1.590703122)


# Create a data frame
data_df <- data.frame(Depth = depths, Percentage = percentages)

# Set overall plot margins and adjust y-axis title white space
par(mar = c(5, 6, 2, 2))  # Adjust overall margins
par(mgp = c(4.7, 1, 0))     # Adjust the margin line for the axis title


# Bar chart with flipped axes
barplot(data_df$Percentage, names.arg = data_df$Depth, ylab = "Continental shelf water depth (meters)",
        xlab = "", col = "steelblue",
        beside = TRUE, horiz = TRUE, xlim = c(0,100), las = 1, cex.axis = 1)

# Add x-axis title manually at desired coordinates
mtext("Percentage of Australia's national benthic habitat map (%)", side = 1, line = 3, at = 50, cex = 1)


# # Add legend
# legend("topright", legend = c("Percentage"), fill = "steelblue")

# Display the plot
dev.off()  # If you're not working in a script, use this line to display the plot in RStudio or another IDE

