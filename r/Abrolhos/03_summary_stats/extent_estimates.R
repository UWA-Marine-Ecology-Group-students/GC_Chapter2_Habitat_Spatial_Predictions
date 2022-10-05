#  Project: ** GC PhD Chapter 2 Habitat Summary Stats  **
# Data:    BRUVS, BOSS Habitat data
# Task:    Habitat summary stats for results table
# author:  Kingsley Griffin & Gabby Cummins
# date:    ** 05/10/2022 **

rm(list = ls())

# load data
rastdf <- readRDS("output/Abrolhos/Abrolhos_spatial_habitat_predictions.rds")

# set up df for calculation
sumstat          <- data.frame(
  "location" = c("Abrolhos"),
  "habtype"  = c("kelps", "macroalg", 
                 "sand", "rock", "inverts"),
  "phabname" = c("pkelps", "pmacroalg", "psand", 
                 "prock", "pinverts"))

# calculate summary
for(habi in unique(sumstat$habtype)){
  sumstat$ndom_cell[sumstat$habtype == habi] <- 
    nrow(rastdf[rastdf$dom_tag == habi, ])                                      # number of cells habitat x is dominant
  sumstat$pcellsum[sumstat$habtype == habi]  <- 
    sum(rastdf[ , paste0(sumstat$phabname[sumstat$habtype == habi])])           # sum of predicted distributon for habitat x
}

# convert units to area
cellarea         <- 250 * 250                                                   # set this to the dimensions of the raster (based on bathy data) in metres

sumstat[4:5]     <- sapply(sumstat[4:5], as.numeric)
sumstat$dom_area <- sumstat$ndom_cell * cellarea
sumstat$p_area   <- sumstat$pcellsum  * cellarea
sumstat$area_dif <- sumstat$dom_area - sumstat$p_area

# calculate totals
sumstat <- rbind(sumstat, c("Abrolhos", "total", "total", 
                            round(sum(sumstat$ndom_cell)), 
                            round(sum(sumstat$pcellsum)), 
                            round(sum(sumstat$dom_area)), 
                            round(sum(sumstat$p_area)),
                            round(sum(sumstat$dom_area) - 
                                    sum(sumstat$p_area))))
 
sumstat

