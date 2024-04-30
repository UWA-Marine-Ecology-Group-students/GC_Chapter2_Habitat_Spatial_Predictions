#  Project: ** GC PhD Chapter 2 Habitat Summary Stats  **
# Data:    BRUVS, BOSS Habitat data
# Task:    Habitat summary stats for results table
# author:  Kingsley Griffin & Gabby Cummins
# date:    ** 05/10/2022 **

rm(list = ls())

library(tidyverse)

# Set your study name
name <- "Abrolhos"  

# load data
rastdf <- readRDS("output/Abrolhos/Abrolhos_spatial_habitat_predictions.rds")

#set up table for Ocean ecosystem account results
OEA_results <- data.frame(
  habitat = c("p.sand", "pmacroalg", "p.rock", "p.inverts"),
  individual_area_m2 = rep(NA, 4),
  individual_area_km2 = rep(NA, 4),
  SE_area_m2 = rep(NA, 4),
  SE_area_km2 = rep(NA, 4),
  dom_area_m2 = rep(NA, 4),
  dom_area_km2 = rep(NA, 4),
  i_area_scaled_m2 = rep(NA, 4),
  i_area_scaled_km2 = rep(NA, 4)
)

# # set up df for calculation
# sumstat          <- data.frame(
#   "location" = c("Abrolhos"),
#   "class"  = c("pmacroalg.fit", 
#                  "psand.fit", "prock.fit", "pinverts.fit"),
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
# sumstat <- rbind(sumstat, c("Abrolhos", "total", "total",
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
# write.table(sumstat, file = "sumstats.csv", sep = " ", quote = FALSE, row.names = T)


#DOM TAG 
#group_by(rastdf, dom_tag) %>% count() %>% mutate(area = n *250 *250)


#Calculate individual habitat class areas
rastdf.2 <- rastdf %>%
    mutate(
      psand.area = psand.fit * 250 * 250,
      pmacroalg.area = pmacroalg.fit * 250 * 250,
      prock.area = prock.fit * 250 * 250,
      pinverts.area = pinverts.fit * 250 * 250 
      )

#Calculate the sums of the individual habitat classes
total_p_sand_area <- sum(rastdf.2$psand.area)
total_p_macroalgae_area <- sum(rastdf.2$pmacroalg.area)
total_p_rock_area <- sum(rastdf.2$prock.area)
total_p_inverts_area <- sum(rastdf.2$pinverts.area) 

# Assign individual habitat class areas to the OEA_results dataframe
OEA_results$individual_area_m2 <- c(total_p_sand_area, total_p_macroalgae_area, total_p_rock_area, total_p_inverts_area)

# Calculate individual_area_km2 based on individual_area_m2
OEA_results$individual_area_km2 <- OEA_results$individual_area_m2 * 0.000001

#Calculating SE
rastdf.3 <- rastdf %>%
  mutate(
    psand.se.area = psand.se.fit * 250 * 250,
    pmacroalg.se.area = pmacroalg.se.fit * 250 * 250,
    prock.se.area = prock.se.fit * 250 * 250,
    pinverts.se.area = pinverts.se.fit * 250 * 250 
    )

#Calculate the sums of the SE individual habitat classes
total_p_sand_SE_area <- sum(rastdf.3$psand.se.area)
total_p_macroalgae_SE_area <- sum(rastdf.3$pmacroalg.se.area)
total_p_rock_SE_area <- sum(rastdf.3$prock.se.area)
total_p_inverts_SE_area <- sum(rastdf.3$pinverts.se.area) 

#Assign SE areas to the OEA results df
OEA_results$SE_area_m2 <- c(total_p_sand_SE_area, total_p_macroalgae_SE_area, total_p_rock_SE_area, total_p_inverts_SE_area)

# Calculate SE_area_km2 based on SE_area_m2
OEA_results$SE_area_km2 <- OEA_results$SE_area_m2 * 0.000001

##DOMINANT HABITAT CALCS
group_by(rastdf, dom_tag) %>% count() %>% mutate(area = n *250 *250)

##write the nums from above calc table for dom habitat classes below
total_dom_sand_area <- 397125000
total_dom_macroalgae_area <- 132250000
total_dom_rock_area <- 27000000
total_dom_inverts_area <- 113625000

# Assign dominant habitat class areas to the OEA_results dataframe
OEA_results$dom_area_m2 <- c(total_dom_sand_area, total_dom_macroalgae_area, total_dom_rock_area,total_dom_inverts_area)

# Calculate dom_area_km2 based on dom_area_m2
OEA_results$dom_area_km2 <- OEA_results$dom_area_m2 * 0.000001

#Calculate scaled probability weighted index based on individual habitat class
probabilitywindex <- rastdf.2 %>%
  mutate(percentsand.area = psand.area / (psand.area + pmacroalg.area + prock.area + pinverts.area))%>%
  mutate(percentmacroalg.area = pmacroalg.area / (psand.area + pmacroalg.area + prock.area + pinverts.area))%>%
  mutate(percentrock.area = prock.area / (psand.area + pmacroalg.area + prock.area + pinverts.area))%>%
  mutate(percentinverts.area = pinverts.area / (psand.area + pmacroalg.area + prock.area + pinverts.area))

PWI <- probabilitywindex %>%
  mutate(psand.area.scaled = percentsand.area *250 *250) %>%
  mutate(pmacroalgae.area.scaled = percentmacroalg.area *250 *250) %>%
  mutate(prock.area.scaled = percentrock.area *250 *250) %>%
  mutate(pinverts.area.scaled = percentinverts.area *250 *250)

#Calculate scaled results
totalsand.area.scaled <- sum(PWI$psand.area.scaled)
totalmacroalg.area.scaled <- sum(PWI$pmacroalgae.area.scaled)
totalrock.area.scaled <- sum(PWI$prock.area.scaled)
totalinvert.area.scaled <- sum(PWI$pinverts.area.scaled)

# Assign scaled areas to the OEA_results dataframe
OEA_results$i_area_scaled_m2 <- c(totalsand.area.scaled, totalmacroalg.area.scaled, totalrock.area.scaled, totalinvert.area.scaled)

# Calculate scaled_area_km2 based on scaled_area_m2
OEA_results$i_area_scaled_km2 <- OEA_results$i_area_scaled_m2 * 0.000001

# Save the dataframe as a CSV file
write.csv(OEA_results, file = paste(paste0('output/Abrolhos/', name), "OEA_results.csv", sep = "_"), row.names = FALSE)


# #Brookes Dom Tag way
# dom <- rastdf %>%
#   mutate(dom_area = case_when(
#     dom_tag == "inverts.fit" ~ pinverts.fit,
#     dom_tag =="rock.fit" ~ prock.fit,
#     dom_tag == "macroalg.fit" ~ pmacroalg.fit,
#     dom_tag == "sand.fit" ~ psand.fit
#   )) %>%
#   dplyr::group_by(dom_tag) %>%
#   dplyr::summarise(sum(dom_area))
# 
# write.csv(dom, "output/Abrolhos/extent.csv")
# unique(rastdf$dom_tag)
# names(rastdf)

# 
# #GABBY DOMINANT WAY 2023 DEC
# rastdf.4 <- rastdf.2 %>%
#   mutate(
#     dom.sand.area = if_else(dom_tag == "sand.fit", psand.area, 0),
#     dom.macro.area = if_else(dom_tag == "macroalg.fit", pmacroalg.area, 0), 
#     dom.rock.area =if_else(dom_tag == "rock.fit", prock.area, 0),
#     dom.invert.area = if_else(dom_tag == "inverts.fit", pinverts.area, 0)
#   )
# 
# # Calculate the sum of dom.sand.area
# total_dom_sand_area <- sum(rastdf.4$dom.sand.area)
# total_dom_macro_area <- sum(rastdf.4$dom.macro.area)
# total_dom_rock_area <- sum(rastdf.4$dom.rock.area)
# # total_dom_invert_area <- sum(rastdf.4$dom.invert.area)