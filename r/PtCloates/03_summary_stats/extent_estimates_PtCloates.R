#  Project: ** GC PhD Chapter 2 Habitat Summary Stats  **
# Data:    BRUVS, BOSS Habitat data
# Task:    Habitat summary stats for results table
# author:  Kingsley Griffin & Gabby Cummins
# date:    ** 05/10/2022 **

rm(list = ls())

library(tidyverse)

# Set your study name
name <- "PtCloates"    

# load data
rastdfgc <- readRDS("output/PtCloates/PtCloates_spatial_habitat_predictions.rds")

 rastdf <- rastdfgc %>%
  filter(Z >= -215, Z <= -71)

 #set up table for Ocean ecosystem account results
 OEA_results <- data.frame(
   habitat = c("p.sand", "p.inverts"),
   individual_area_m2 = rep(NA, 2),
   individual_area_km2 = rep(NA, 2),
   SE_area_m2 = rep(NA, 2),
   SE_area_km2 = rep(NA, 2),
   dom_area_m2 = rep(NA, 2),
   dom_area_km2 = rep(NA, 2),
   i_area_scaled_m2 = rep(NA, 2),
   i_area_scaled_km2 = rep(NA, 2)
 )
 
#Calculate individual habitat class areas
#group_by(rastdf, dom_tag) %>% count() %>% mutate(area = n *250 *250)
rastdf.2 <- rastdf %>%
  mutate(
    psand.area = psand.fit * 250 * 250,
    pinverts.area = pinverts.fit * 250 * 250
  )
  
# Calculate the sums of individual sand and individual invert habitat classes
total_p_sand_area <- sum(rastdf.2$psand.area)
total_p_invert_area <- sum(rastdf.2$pinverts.area)  
  
# Assign individual habitat class areas to the OEA_results dataframe
OEA_results$individual_area_m2 <- c(total_p_sand_area, total_p_invert_area)

# Calculate individual_area_km2 based on individual_area_m2
OEA_results$individual_area_km2 <- OEA_results$individual_area_m2 * 0.000001

#Calculating SE
rastdf.3 <- rastdf %>%
  mutate (
    psand.se.area = psand.se.fit * 250 * 250,
    pinverts.se.area = pinverts.se.fit * 250 * 250
  )
#Calculate the SE Sum
sesand_area <- sum(rastdf.3$psand.se.area)
seinvert_area <- sum(rastdf.3$pinverts.se.area) 

# Assign SE areas to the OEA_results dataframe
OEA_results$SE_area_m2 <- c(sesand_area, seinvert_area)

# Calculate SE_area_km2 based on SE_area_m2
OEA_results$SE_area_km2 <- OEA_results$SE_area_m2 * 0.000001

#DOMINANT Habitat calcs 2023 DEc
rastdf.4 <- rastdf.2 %>%
  mutate(
    dom.sand.area = if_else(dom_tag == "sand.fit", psand.area, if_else(dom_tag == "inverts.fit", 0, NA_real_)),
    dom.invert.area = if_else(dom_tag == "inverts.fit", pinverts.area, if_else(dom_tag == "sand.fit", 0, NA_real_))
  )

# Calculate the sum of dom.sand.area and dom.inver areas
total_dom_sand_area <- sum(rastdf.4$dom.sand.area)
total_dom_invert_area <- sum(rastdf.4$dom.invert.area)

# Assign dominant habitat class areas to the OEA_results dataframe
OEA_results$dom_area_m2 <- c(total_dom_sand_area, total_dom_invert_area)

# Calculate dom_area_km2 based on dom_area_m2
OEA_results$dom_area_km2 <- OEA_results$dom_area_m2 * 0.000001

#Calculate scaled probability weighted index based on individual habitat class
probabilitywindex <- rastdf.2 %>%
  mutate(percentsand.area = psand.area / (psand.area + pinverts.area))
       
PWI <- probabilitywindex %>%
  mutate(percentinverts.area = pinverts.area / (psand.area + pinverts.area)) %>%
  mutate(psand.area.scaled = (percentsand.area *250 *250))%>%
  mutate(pinvert.area.scaled = (percentinverts.area *250 *250))

#Calculate scaled results
totalsand.area.scaled <- sum(PWI$psand.area.scaled)
totalinvert.area.scaled <- sum(PWI$pinvert.area.scaled)

# Assign scaled areas to the OEA_results dataframe
OEA_results$i_area_scaled_m2 <- c(totalsand.area.scaled, totalinvert.area.scaled)
 
# Calculate scaled_area_km2 based on scaled_area_m2
OEA_results$i_area_scaled_km2 <- OEA_results$i_area_scaled_m2 * 0.000001

# Save the dataframe as a CSV file
write.csv(OEA_results, file = paste(paste0('output/PtCloates/', name), "OEA_results.csv", sep = "_"), row.names = FALSE)


# #Brookes Dom Tag way
# dom <- rastdf %>%
#   mutate(dom_area = case_when(
#     dom_tag == "inverts.fit" ~ pinverts.fit,
#     #dom_tag =="rock.fit" ~ prock.fit,
#     #dom_tag == "macroalg.fit" ~ pmacroalg.fit,
#     dom_tag == "sand.fit" ~ psand.fit
#   )) %>%
#   dplyr::group_by(dom_tag) %>%
#   dplyr::summarise(sum(dom_area))

# write.csv(dom, "output/Abrolhos/extent.csv")
# unique(rastdf$dom_tag)
# names(rastdf)

# # set up df for calculation
# sumstat          <- data.frame(
#   "location" = c("PtCloates"),
#   "class"  = c("psand.fit", "pinverts.fit"),
#   "ndomcell" = c(NA),
#   "total_p" = c(NA),
#   "lower_ci" = c(NA),
#   "upper_ci" = c(NA))
# head(sumstat)
# 
# # # calculate summary
# for(classi in unique(sumstat$class)){
#   sumstat$ndomcell[sumstat$class == classi] <-
#     nrow(rastdf[rastdf$dom_tag == paste0(classi, ".fit", sep = ""), ])                                      # number of cells habitat x is dominant
#   
#   print(classi)
#   sumstat$total_p[sumstat$class == classi] <-
#     sum(rastdf[ , paste0("p", classi, ".fit", sep = "")])           # sum of predicted distributon for habitat x
#   # sumstat$lower_ci[sumstat$class == classi] <-
#   #   sum(rastdf[ , paste0("p", classi, ".fit", sep = "")] -
#   #         rastdf[ , paste0("p", classi, ".se.fit", sep = "")])
#   # sumstat$upper_ci[sumstat$class == classi] <-
#   #   sum(rastdf[ , paste0("p", classi, ".fit", sep = "")] +
#   #         rastdf[ , paste0("p", classi, ".se.fit", sep = "")])
# }
# 
# sumstat
# 
# # convert units to area
# cellarea         <- 250 * 250                                                   # set this to the dimensions of the raster (based on bathy data) in metres
# 
# sumstat[4:5]     <- sapply(sumstat[4:5], as.numeric)
# sumstat$dom_area <- sumstat$ndomcell * cellarea
# sumstat$p_area   <- sumstat$total_p  * cellarea
# sumstat$lci_area <- sumstat$lower_ci * cellarea
# sumstat$uci_area <- sumstat$upper_ci * cellarea
# 
# sumstat$area_dif <- sumstat$dom_area - sumstat$p_area
# 
# sumstat
# 
# # calculate totals
# sumstat <- rbind(sumstat, c("Pt Cloates", "total", "total",
#                             round(sum(sumstat$ndom_cell)),
#                             round(sum(sumstat$pcellsum)),
#                             round(sum(sumstat$dom_area)),
#                             round(sum(sumstat$p_area)),
#                             round(sum(sumstat$dom_area) -
#                                     sum(sumstat$p_area))))
# 
# sumstat
# 
# 
# #save the sumstat as .csv
# write.table(sumstat, file = "PtCloatessumstats.csv", sep = " ", quote = FALSE, row.names = T)
