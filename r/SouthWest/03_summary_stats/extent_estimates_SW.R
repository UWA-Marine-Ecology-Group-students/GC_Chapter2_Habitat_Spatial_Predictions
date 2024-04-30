#  Project: ** GC PhD Chapter 2 Habitat Summary Stats  **
# Data:    BRUVS, BOSS Habitat data
# Task:    Habitat summary stats for results table
# author:  Kingsley Griffin & Gabby Cummins
# date:    ** 05/10/2022 **

rm(list = ls())

library(tidyverse)

# Set your study name
name <- "SouthWest"   

# load data
rastdf <- readRDS("output/SWC/SouthWest_spatial_habitat_predictions.rds")

#set up table for Ocean ecosystem account results
OEA_results <- data.frame(
  habitat = c("p.sand", "p.seagrass", "p.macroalg", "p.rock", "p.inverts"),
  individual_area_m2 = rep(NA, 5),
  individual_area_km2 = rep(NA, 5),
  SE_area_m2 = rep(NA, 5),
  SE_area_km2 = rep(NA, 5),
  dom_area_m2 = rep(NA, 5),
  dom_area_km2 = rep(NA, 5),
  i_area_scaled_m2 = rep(NA, 5),
  i_area_scaled_km2 = rep(NA, 5))

#Calculate individual habitat class extent areas
rastdf.2 <- rastdf %>%
  mutate(
    psand.area = psand.fit * 250 * 250,
    pseagrasses.area = pseagrasses.fit * 250 * 250,
    pmacroalg.area = pmacroalg.fit * 250 * 250,
    prock.area = prock.fit * 250 * 250,
    pinverts.area = pinverts.fit * 250 * 250 
      )
  
#Calculate the sums of the individual habitat classes
total_p_sand_area <- sum(rastdf.2$psand.area)
total_p_seagrasses_area <- sum(rastdf.2$pseagrasses.area)
total_p_macroalgae_area <- sum(rastdf.2$pmacroalg.area)
total_p_rock_area <- sum(rastdf.2$prock.area)
total_p_inverts_area <- sum(rastdf.2$pinverts.area) 

##Assign individual habitat class extents to the OEA_results df
OEA_results$individual_area_m2 <- c(total_p_sand_area, total_p_seagrasses_area, total_p_macroalgae_area, total_p_rock_area, total_p_inverts_area)

#Calculate individual area km2 based on individual area m2
OEA_results$individual_area_km2 <- OEA_results$individual_area_m2 * 0.000001

#Calculating SE
rastdf.3 <- rastdf %>%
  mutate(
    psand.se.area = psand.se.fit * 250 * 250,
    pseagrasses.se.area = pseagrasses.se.fit * 250 * 250,
    pmacroalg.se.area = pmacroalg.se.fit * 250 * 250,
    prock.se.area = prock.se.fit * 250 * 250,
    pinverts.se.area = pinverts.se.fit * 250 * 250
  )
  
#Calculate the sums of the SE individual habitat classes
total_p_sand_SE_area <- sum(rastdf.3$psand.se.area)
total_p_seagrasses_SE_area <- sum(rastdf.3$pseagrasses.se.area)
total_p_macroalgae_SE_area <- sum(rastdf.3$pmacroalg.se.area)
total_p_rock_SE_area <- sum(rastdf.3$prock.se.area)
total_p_inverts_SE_area <- sum(rastdf.3$pinverts.se.area) 

# Assign SE areas to the OEA_results dataframe
OEA_results$SE_area_m2 <- c(total_p_sand_SE_area, total_p_seagrasses_SE_area, total_p_macroalgae_SE_area, total_p_rock_SE_area, total_p_inverts_SE_area) 

# Calculate SE_area_km2 based on SE_area_m2
OEA_results$SE_area_km2 <- OEA_results$SE_area_m2 * 0.000001

##DOMINANT HABITAT Calcs
#Calculate dominant habitat class areas and table
group_by(rastdf, dom_tag) %>% count() %>% mutate(area = n *250 *250)

#Transcribe results from table above to below
total_dom_sand_area <- 1400937500
total_dom_seagrasses_area <- 375000
total_dom_macroalgae_area <- 892000000
total_dom_rock_area <- 84187500
total_dom_inverts_area <- 11687500

# Assign dominant habitat class areas to the OEA_results dataframe
OEA_results$dom_area_m2 <- c(total_dom_sand_area, total_dom_seagrasses_area, total_dom_macroalgae_area, total_dom_rock_area, total_dom_inverts_area)

# Calculate dom_area_km2 based on dom_area_m2
OEA_results$dom_area_km2 <- OEA_results$dom_area_m2 * 0.000001

#Calculate scaled probability weighted index based on individual habitat class
probabilitywindex <- rastdf.2 %>%
  mutate(percentsand.area = psand.area / (psand.area + pseagrasses.area + pmacroalg.area + prock.area + pinverts.area))%>%
  mutate(percentseagrass.area = pseagrasses.area / (psand.area + pseagrasses.area + pmacroalg.area + prock.area + pinverts.area)) %>%
  mutate(percentmacroalg.area = pmacroalg.area / (psand.area + pseagrasses.area + pmacroalg.area + prock.area + pinverts.area))%>%
  mutate(percentrock.area = prock.area / (psand.area + pseagrasses.area + pmacroalg.area + prock.area + pinverts.area))%>%
  mutate(percentinverts.area = pinverts.area / (psand.area + pseagrasses.area + pmacroalg.area + prock.area + pinverts.area))

PWI <- probabilitywindex %>%
  mutate(psand.area.scaled = percentsand.area *250 *250) %>%
  mutate(pseagrass.area.scaled = percentseagrass.area *250 *250) %>%
  mutate(pmacroalgae.area.scaled = percentmacroalg.area *250 *250) %>%
  mutate(prock.area.scaled = percentrock.area *250 *250) %>%
  mutate(pinverts.area.scaled = percentinverts.area *250 *250)

#Calculate scaled results
totalsand.area.scaled <- sum(PWI$psand.area.scaled)
totalseagrass.area.scaled <- sum(PWI$pseagrass.area.scaled)
totalmacroalgae.area.scaled <- sum(PWI$pmacroalgae.area.scaled)
totalrock.area.scaled <- sum(PWI$prock.area.scaled)
totalinvert.area.scaled <- sum(PWI$pinverts.area.scaled)

# Assign scaled areas to the OEA_results dataframe
OEA_results$i_area_scaled_m2 <- c(totalsand.area.scaled, totalseagrass.area.scaled, totalmacroalgae.area.scaled, totalrock.area.scaled, totalinvert.area.scaled) 

# Calculate scaled_area_km2 based on scaled_area_m2
OEA_results$i_area_scaled_km2 <- OEA_results$i_area_scaled_m2 * 0.000001

# Save the dataframe as a CSV file
write.csv(OEA_results, file = paste(paste0('output/SWC/', name), "OEA_results.csv", sep = "_"), row.names = FALSE)



# #GABBY DOMINANT WAY 2023 DEC
# rastdf.4 <- rastdf.2 %>%
#   mutate(
#     dom.sand.area = if_else(dom_tag == "sand.fit", psand.area, 0),
#     dom.macro.area = if_else(dom_tag == "macroalg.fit", pmacroalg.area, 0), 
#     dom.rock.area =if_else(dom_tag == "rock.fit", prock.area, 0),
#     dom.invert.area = if_else(dom_tag == "inverts.fit", pinverts.area, 0),
#     dom.seagrass.area = if_else(dom_tag == "seagrasses.fit", pseagrasses.area, 0)
#   )
# 
# # Calculate the sum of dom.sand.area
# total_dom_sand_area <- sum(rastdf.4$dom.sand.area)
# total_dom_macro_area <- sum(rastdf.4$dom.macro.area)
# total_dom_rock_area <- sum(rastdf.4$dom.rock.area)
# total_dom_invert_area <- sum(rastdf.4$dom.invert.area)
# total_dom_seagrass_area <- sum(rastdf.4$dom.seagrass.area)
# 
# print(total_dom_sand_area)
# print(total_dom_macro_area)
# print(total_dom_rock_area)
# print(total_dom_invert_area)
# print(total_dom_seagrass_area)
# 
# #Brookes Dom Tag way
# dom <- rastdf.2 %>%
#   mutate(dom_area = case_when(
#     dom_tag == "inverts.fit" ~ pinverts.fit,
#     dom_tag =="rock.fit" ~ prock.fit,
#     dom_tag == "macroalg.fit" ~ pmacroalg.fit,
#     dom_tag == "seagrasses.fit" ~ pseagrasses.fit,
#     dom_tag == "sand.fit" ~ psand.fit
#   )) %>%
#   dplyr::group_by(dom_tag) %>%
#   dplyr::summarise(sum(dom_area))
# 
# write.csv(dom, "output/SWC/extent.csv")
# unique(rastdf$dom_tag)
# names(rastdf)
