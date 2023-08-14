#  Project: ** GC PhD Chapter 2 Habitat Summary Stats  **
# Data:    BRUVS, BOSS Habitat data
# Task:    Habitat summary stats for results table
# author:  Kingsley Griffin & Gabby Cummins
# date:    ** 05/10/2022 **

rm(list = ls())

library(tidyverse)

# load data
rastdf <- readRDS("output/PtCloates/PtCloates_spatial_habitat_predictions.rds")

# set up df for calculation
sumstat          <- data.frame(
  "location" = c("PtCloates"),
  "class"  = c("psand.fit", "pinverts.fit"),
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

sumstat

# convert units to area
cellarea         <- 250 * 250                                                   # set this to the dimensions of the raster (based on bathy data) in metres

sumstat[4:5]     <- sapply(sumstat[4:5], as.numeric)
sumstat$dom_area <- sumstat$ndomcell * cellarea
sumstat$p_area   <- sumstat$total_p  * cellarea
sumstat$lci_area <- sumstat$lower_ci * cellarea
sumstat$uci_area <- sumstat$upper_ci * cellarea

sumstat$area_dif <- sumstat$dom_area - sumstat$p_area

sumstat

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


#Ben's summary
group_by(rastdf, dom_tag) %>% count() %>% mutate(area = n *250 *250)
rastdf.2 <- mutate(.data = rastdf, psand.area = psand.fit * 250 * 250)

mutate(.data = rastdf,
       # pkelps.area = pkelps.fit * 250 * 250,
       psand.area = psand.fit * 250 * 250,
       #pmacroalg.area = pmacroalg.fit * 250 * 250,
       #prock.area = prock.fit * 250 * 250,
       pinverts.area = pinverts.fit * 250 * 250)  %>%
  summarise(psand.area.total = sum(psand.area),
            #pmacroalg.area.total = sum(pmacroalg.area),
            #prock.area.total = sum(prock.area),
            pinverts.area.total = sum(pinverts.area))


#Gabby SE
rastdf.3 <- mutate(.data = rastdf, psand.se.area = psand.se.fit * 250 * 250)

mutate(.data = rastdf,
       psand.se.area = psand.se.fit * 250 * 250,
       #pmacroalg.se.area = pmacroalg.se.fit * 250 * 250,
       #prock.se.area = prock.se.fit * 250 * 250,
       pinverts.se.area = pinverts.se.fit * 250 * 250)  %>%
  summarise(psand.se.area.total = sum(psand.se.area),
            #pmacroalg.se.area.total = sum(pmacroalg.se.area),
            #prock.se.area.total = sum(prock.se.area),
            pinverts.se.area.total = sum(pinverts.se.area))

#Brookes Dom Tag way
dom <- rastdf %>%
  mutate(dom_area = case_when(
    dom_tag == "inverts.fit" ~ pinverts.fit,
    #dom_tag =="rock.fit" ~ prock.fit,
    #dom_tag == "macroalg.fit" ~ pmacroalg.fit,
    dom_tag == "sand.fit" ~ psand.fit
  )) %>%
  dplyr::group_by(dom_tag) %>%
  dplyr::summarise(sum(dom_area))

write.csv(dom, "output/Abrolhos/extent.csv")
unique(rastdf$dom_tag)
names(rastdf)
