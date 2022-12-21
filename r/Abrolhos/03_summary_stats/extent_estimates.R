#  Project: ** GC PhD Chapter 2 Habitat Summary Stats  **
# Data:    BRUVS, BOSS Habitat data
# Task:    Habitat summary stats for results table
# author:  Kingsley Griffin & Gabby Cummins
# date:    ** 05/10/2022 **

rm(list = ls())

library(tidyverse)

# load data
rastdf <- readRDS("output/Abrolhos/Abrolhos_spatial_habitat_predictions.rds")

# set up df for calculation
sumstat          <- data.frame(
  "location" = c("Abrolhos"),
  "habtype"  = c("kelps", "macroalg", 
                 "sand", "rock", "inverts"),
  "phabname" = c("pkelps.fit", "pmacroalg.fit", "psand.fit", 
                 "prock.fit", "pinverts.fit"))

# calculate summary
for(habi in unique(sumstat$habtype)){
  sumstat$ndom_cell[sumstat$habtype == habi] <- 
    nrow(rastdf[rastdf$dom_tag == habi, ])                                      # number of cells habitat x is dominant
  sumstat$pcellsum[sumstat$habtype == habi]  <- 
    sum(rastdf[ , paste0(sumstat$phabname[sumstat$habtype == habi])])           # sum of predicted distributon for habitat x
}

#Ben's summary
# group_by(rastdf, dom_tag) %>% count() %>% mutate(area = n *250 *250)
# rastdf.2 <- mutate(.data = rastdf, pkelps.area = pkelps.fit * 250 * 250)  
# 
# mutate(.data = rastdf, 
#        pkelps.area = pkelps.fit * 250 * 250, 
#        psand.area = psand.fit * 250 * 250,
#        pmacroalg.area = pmacroalg.fit * 250 * 250,
#        prock.area = prock.fit * 250 * 250, 
#        pinverts.area = pinverts.fit * 250 * 250)  %>%
#   summarise(pkelps.area.total = sum(pkelps.area), 
#             psand.area.total = sum(psand.area),
#             pmacroalg.area.total = sum(pmacroalg.area),
#             prock.area.total = sum(prock.area),
#             pinverts.area.total = sum(pinverts.area))

#Gabby SE
# rastdf.3 <- mutate(.data = rastdf, pkelps.se.area = pkelps.se.fit * 250 * 250)  
# 
# mutate(.data = rastdf, 
#        pkelps.se.area = pkelps.se.fit * 250 * 250, 
#        psand.se.area = psand.se.fit * 250 * 250,
#        pmacroalg.se.area = pmacroalg.se.fit * 250 * 250,
#        prock.se.area = prock.se.fit * 250 * 250, 
#        pinverts.se.area = pinverts.se.fit * 250 * 250)  %>%
#   summarise(pkelps.se.area.total = sum(pkelps.se.area), 
#             psand.se.area.total = sum(psand.se.area),
#             pmacroalg.se.area.total = sum(pmacroalg.se.area),
#             prock.se.area.total = sum(prock.se.area),
#             pinverts.se.area.total = sum(pinverts.se.area))


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


#save the sumstat as .csv
write.table(sumstat, file = "sumstats.csv", sep = " ", quote = FALSE, row.names = T)

