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
percentage1 <- c(89.35058734, 9.05870954, 1.590703122)
percentage2 <- c(10, 17, 73)


# Create a data frame
data_df <- data.frame(Depth = depths, Percentage1 = percentage1, Percentage2 = percentage2)

# Set overall plot margins and adjust y-axis title white space
par(mar = c(5, 6, 2, 2))  # Adjust overall margins
par(mgp = c(4.7, 1, 0))     # Adjust the margin line for the axis title


# Bar chart with flipped axes
barplot(data_df$Percentage1, names.arg = data_df$Depth, ylab = "Continental shelf water depth (meters)",
        xlab = "", col = "steelblue",
        beside = TRUE, horiz = TRUE, xlim = c(0,100), las = 1, cex.axis = 1)

# Add x-axis title manually at desired coordinates
mtext("Percentage of Australia's national benthic habitat map (%)", side = 1, line = 3, at = 50, cex = 1)


# # Add legend
# legend("topright", legend = c("Percentage"), fill = "steelblue")

# Display the plot
dev.off()  # If you're not working in a script, use this line to display the plot in RStudio or another IDE


# Creating the dataframe
# df <- data.frame(
#   Depth = c("<40", "<40", "40 - 100", "40 - 100", ">100", ">100"),
#   Percentage = c(percentage1, percentage2),
#   Cultivar = rep(c("Percentage1", "Percentage2"), times = 3))

# Your data
depths <- c("<40", "40 - 100", ">100")
percentage1 <- c(89.35058734, 9.05870954, 1.590703122)
percentage2 <- c(10, 17, 73)

# Creating the dataframe with fixed depths
df <- data.frame(
  Depth = c("<40", "40 - 100", ">100", "<40", "40 - 100", ">100"),
  Percentage = c(percentage1, percentage2),
  Cultivar = rep(c("Percentage1", "Percentage2"), each = 3)
)

# Create a data frame
# #data_df <- data.frame(Depth = rep(depths, each = 2),
#                       Percentage = c(percentage1, percentage2),
#                       Cultivar = rep(c("Percentage1", "Percentage2"), times = 3))

# data_df1 <- data.frame(Depth = depths, Percentage1 = percentage1, Percentage2 = percentage2)


# Set your study name
# name <- "GC_Chapter2_Habitat_Spatial_Predictions"

# Create a stacked bar chart using ggplot2
ggplot(df, aes(x = Depth, y = percentage1, fill = "Percentage1")) +
  geom_bar(stat = "identity") +
  geom_bar(aes(x = Depth, y = percentage2, fill = "Percentage2"), stat = "identity") +
  labs(x = "Continental shelf water depth (meters)",
       y = "Percentage of Australia's national benthic habitat map (%)",
       fill = "Cultivar") +
  theme_minimal() +
  theme(legend.position = "top") +
  coord_flip()

#######CHATGPT

# Your data
depths <- c("0 - 40", "40 - 100", "100 - 200")
percentage1 <- c(89.35058734, 9.05870954, 1.590703122)
percentage2 <- c(13, 57, 30)

# # Create the dataframe
# df <- data.frame(
#   Depth = depths,
#   Percentage1 = percentage1,
#   Percentage2 = percentage2
# )


# Create a dataframe
df1 <- data.frame(
  Depth = rep(depths, times = 2),
  Percentage = c(89.35058734, 9.05870954, 1.590703122, 13, 57, 30),
  Cultivar = rep(c("% of Australia's national benthic habitat map (Seamap)", "% of Australia's area in continental shelf waters (0-200m)"), each=3)
) %>% 
  mutate(Cultivar = as.factor(Cultivar),
         Depth = as.factor(Depth)) %>% 
  mutate(Depth = fct_relevel(Depth, "0 - 40", "40 - 100", "100 - 200"))

# # Create a stacked bar chart using ggplot2
# ggplot(df1, aes(x = Depth, y = Percentage, fill = Cultivar)) +
#   geom_col(position="stack") +
#   #geom_bar(aes(x = Depth, y = percentage2, fill = Percentage2), stat = "identity") +
#   labs(x = "Continental shelf water depth (meters)",
#        y = "Percentage (%)",
#        fill = "Cultivar") +
#   theme_minimal() +
#   theme(legend.position = "top", legend.title = element_blank()) +
#   coord_flip()
# Assuming Depth is a factor variable

###ATTEMPT 3 FRPM 
df1$Depth <- factor(df1$Depth, levels = c("100 - 200", "40 - 100", "0 - 40"))

ggplot(df1, aes(fill=Cultivar, y= Depth, x=Percentage))+
       
        
  geom_bar(position = "dodge", stat="identity")+
  labs(y = "Continental shelf water depth (meters)",
       x = "Percentage (%)",
       fill = "Cultivar") +
  theme(legend.position = "top", legend.title = element_blank()) +
  theme(axis.text.y = element_text(angle = 90, hjust =0.5)) +
  theme(axis.title.y = element_text(margin = margin(r = 20)))+
  theme(axis.title.x = element_text(margin = margin (t = 10)))+
  scale_x_continuous(limits = c(0, 100))+
  guides(fill = guide_legend(reverse = TRUE))

