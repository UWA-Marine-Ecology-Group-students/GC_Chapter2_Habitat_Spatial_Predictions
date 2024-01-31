#  Project: ** GC PhD Chapter 2 Habitat Summary Stats  **
# Data:    BRUVS, BOSS Habitat data
# Task:    Habitat summary stats for results table
# author:  Kingsley Griffin & Gabby Cummins
# date:    ** 05/10/2022 **

rm(list = ls())

library(tidyverse)

# load data
rastdf <- readRDS("output/SWC/SouthWest_spatial_habitat_predictions.rds")

# set up df for calculation
sumstat          <- data.frame(
  "location" = c("SouthWest"),
  "class"  = c("pmacroalg.fit", 
                 "psand.fit", "prock.fit", "pinverts.fit", "pseagrasses.fit"),
  "ndomcell" = c(NA),
  "total_p" = c(NA),
  "lower_ci" = c(NA),
  "upper_ci" = c(NA))
head(sumstat)

# # calculate summary
for(classi in unique(sumstat$class)){
  sumstat$ndomcell[sumstat$class == classi] <-
    nrow(rastdf[rastdf$dom_tag == paste0(classi, ".fit", sep = ""), ])                                      # number of cells habitat x is dominant
  
  print(classi)
  sumstat$total_p[sumstat$class == classi] <-
    sum(rastdf[ , paste0("p", classi, ".fit", sep = "")])           # sum of predicted distributon for habitat x
  # sumstat$lower_ci[sumstat$class == classi] <-
  #   sum(rastdf[ , paste0("p", classi, ".fit", sep = "")] -
  #         rastdf[ , paste0("p", classi, ".se.fit", sep = "")])
  # sumstat$upper_ci[sumstat$class == classi] <-
  #   sum(rastdf[ , paste0("p", classi, ".fit", sep = "")] +
  #         rastdf[ , paste0("p", classi, ".se.fit", sep = "")])
}

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


#Ben's summary
group_by(rastdf, dom_tag) %>% count() %>% mutate(area = n *250 *250)
rastdf.2 <- rastdf %>%
  mutate(
    psand.area = psand.fit * 250 * 250,
    pmacroalg.area = pmacroalg.fit * 250 * 250,
    prock.area = prock.fit * 250 * 250,
    pinverts.area = pinverts.fit * 250 * 250, 
    pseagrasses.area = pseagrasses.fit * 250 * 250
  )
  
#Calculate the sums of the individual habitat classes
total_p_sand_area <- sum(rastdf.2$psand.area)
total_p_macroalgae_area <- sum(rastdf.2$pmacroalg.area)
total_p_rock_area <- sum(rastdf.2$prock.area)
total_p_inverts_area <- sum(rastdf.2$pinverts.area) 
total_p_seagrasses_area <- sum(rastdf.2$pseagrasses.area)

print(total_p_inverts_area
    )
print(total_p_sand_area)
print(total_p_macroalgae_area)
print(total_p_rock_area)
print(total_p_seagrasses_area)



#Gabby SE
rastdf.3 <- rastdf %>%
  mutate(
    psand.se.area = psand.se.fit * 250 * 250,
    pmacroalg.se.area = pmacroalg.se.fit * 250 * 250,
    prock.se.area = prock.se.fit * 250 * 250,
    pinverts.se.area = pinverts.se.fit * 250 * 250, 
    pseagrasses.se.area = pseagrasses.se.fit * 250 * 250
  )
  
  
#Calculate the sums of the SE individual habitat classes
total_p_sand_SE_area <- sum(rastdf.3$psand.se.area)
total_p_macroalgae_SE_area <- sum(rastdf.3$pmacroalg.se.area)
total_p_rock_SE_area <- sum(rastdf.3$prock.se.area)
total_p_inverts_SE_area <- sum(rastdf.3$pinverts.se.area) 
total_p_seagrasses_SE_area <- sum(rastdf.3$pseagrasses.se.area)

print(total_p_inverts_SE_area
)
print(total_p_sand_SE_area)
print(total_p_macroalgae_SE_area)
print(total_p_rock_SE_area)
print(total_p_seagrasses_SE_area)


#GABBY DOMINANT WAY 2023 DEC
rastdf.4 <- rastdf.2 %>%
  mutate(
    dom.sand.area = if_else(dom_tag == "sand.fit", psand.area, 0),
    dom.macro.area = if_else(dom_tag == "macroalg.fit", pmacroalg.area, 0), 
    dom.rock.area =if_else(dom_tag == "rock.fit", prock.area, 0),
    dom.invert.area = if_else(dom_tag == "inverts.fit", pinverts.area, 0),
    dom.seagrass.area = if_else(dom_tag == "seagrasses.fit", pseagrasses.area, 0)
  )

# Calculate the sum of dom.sand.area
total_dom_sand_area <- sum(rastdf.4$dom.sand.area)
total_dom_macro_area <- sum(rastdf.4$dom.macro.area)
total_dom_rock_area <- sum(rastdf.4$dom.rock.area)
total_dom_invert_area <- sum(rastdf.4$dom.invert.area)
total_dom_seagrass_area <- sum(rastdf.4$dom.seagrass.area)

print(total_dom_sand_area)
print(total_dom_macro_area)
print(total_dom_rock_area)
print(total_dom_invert_area)
print(total_dom_seagrass_area)

#Brookes Dom Tag way
dom <- rastdf.2 %>%
  mutate(dom_area = case_when(
    dom_tag == "inverts.fit" ~ pinverts.fit,
    dom_tag =="rock.fit" ~ prock.fit,
    dom_tag == "macroalg.fit" ~ pmacroalg.fit,
    dom_tag == "seagrasses.fit" ~ pseagrasses.fit,
    dom_tag == "sand.fit" ~ psand.fit
  )) %>%
  dplyr::group_by(dom_tag) %>%
  dplyr::summarise(sum(dom_area))

write.csv(dom, "output/SWC/extent.csv")
unique(rastdf$dom_tag)
names(rastdf)
