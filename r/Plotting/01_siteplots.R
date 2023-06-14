###
# Project: G Cummins Habitat Paper
# Data:    Sample Metadata, Geoscience Australia 250m res bathy
# Task:    Output WA wide plot including all sample metadata
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

# Set CRS for transformations
wgscrs <- "+proj=longlat +datum=WGS84"
gdacrs <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

# Set cropping extent - larger than most zoomed out plot
e <- ext(109.5, 121,-37, -19.5) 

# Load necessary spatial files
sf_use_s2(F)                                                                    # Switch off spatial geometry for cropping
# Australian outline and state and commonwealth marine parks
aus    <- st_read("data/spatial/shapefiles/cstauscd_r.mif") %>%                 # Geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
  dplyr::filter(FEAT_CODE %in% c("mainland", "island"))
st_crs(aus) <- gdacrs
ausc <- st_crop(aus, e)

# Commonwealth parks
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp") %>%    # All aus mpas
  dplyr::mutate(ZoneName = ifelse(ZoneName %in% "Special Purpose Zone (Mining Exclusion)", "Special Purpose Zone", ZoneName))
mpa <- st_crop(aumpa, e)                                                        # Crop to the study area
# Reorder levels so everything plots nicely
unique(mpa$ZoneName)
# mpa$ZoneName <- factor(mpa$ZoneName, levels = c("Multiple Use Zone", 
#                                                 "Special Purpose Zone (Mining Exclusion)",
#                                                 "Habitat Protection Zone",
#                                                 "National Park Zone"))
npz <- mpa[mpa$ZoneName %in% "National Park Zone", ]                            # Just National Park Zones

# State parks
wampa <- st_read("data/spatial/shapefiles/WA_MPA_2020.shp")
st_crs(wampa) <- gdacrs
# Simplify names for plot legend
wampa$waname <- gsub("( \\().+(\\))", "", wampa$ZONE_TYPE)
wampa$waname <- gsub(" [1-4]", "", wampa$waname)
wampa$waname[wampa$NAME == "Hamelin Pool"]     <- "Marine Nature Reserve"
wampa$waname[wampa$NAME == "Abrolhos Islands"] <- "Fish Habitat Protection Area"
wampa$waname <- dplyr::recode(wampa$waname, 
                               "General Use" = "General Use Zone",
                               "Special Purpose Zone (Shore Based Activities)" = 
                               "Special Purpose Zone\n(Shore Based Activities)",
                               "Special Purpose Zone (Seagrass Protection) (IUCN IV)" = 
                               "Special Purpose Zone",
                              "MMA" = 'Marine Management Area' )

wampa <- st_crop(wampa, e)                                                      # Crop to the study area
wasanc <- wampa[wampa$waname %in% "Sanctuary Zone", ]

# Terrestrial parks
terrnp <- st_read("data/spatial/shapefiles/Legislated_Lands_and_Waters_DBCA_011.shp") %>%  # Terrestrial reserves
  dplyr::filter(leg_catego %in% c("Nature Reserve", "National Park"))
terrnp <- st_crop(terrnp, e)       

# Coastal waters limit
cwatr <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp")       # Coastal waters limit
cwatr_c <- st_crop(cwatr, e)

# Bathymetry data
bathy <- rast("data/spatial/rasters/raw bathymetry/bath_250_good.tif")
bath_c <- crop(bathy, e)
bathdf <- as.data.frame(bath_c, xy = T, na.rm = T) %>%
  dplyr::rename(Z = bath_250_good) %>%
  glimpse()

terr_fills <- scale_fill_manual(values = c("National Park" = "#c4cea6",          # Set the colours for terrestrial parks
                                            "Nature Reserve" = "#e4d0bb"),
                                 guide = "none")

# Assign MPA colours
# Commonwealth
nmpa_cols <- scale_color_manual(values = c("Habitat Protection Zone" = "#fff8a3",
                                           "National Park Zone" = "#7bbc63",
                                           "Multiple Use Zone" = "#b9e6fb",
                                           "Special Purpose Zone (Mining Exclusion)" = "#c5bcc9",
                                           "Special Purpose Zone" = "#c5bcc9"),
                                name = "Australian Marine Parks")

# State
wampa_cols <- scale_colour_manual(values = c(
                                            # "Marine Management Area" = "#b7cfe1",
                                            # "Conservation Area" = "#b3a63d",
                                            "Sanctuary Zone" = "#bfd054",
                                            "General Use Zone" = "#bddde1",
                                            # "Recreation Area" = "#f4e952",
                                            "Special Purpose Zone" = "#c5bcc9"
                                            # "Marine Nature Reserve" = "#bfd054"
                                              ),
                                  name = "State Marine Parks")

# Assign MPA fills
# Commonwealth
nmpa_fills <- scale_fill_manual(values = c("Habitat Protection Zone" = "#fff8a3",
                                                      "National Park Zone" = "#7bbc63",
                                                      "Multiple Use Zone" = "#b9e6fb",
                                                      # "Special Purpose Zone (Mining Exclusion)" = "#c5bcc9",
                                           "Special Purpose Zone" = "#c5bcc9",
                                           "Recreational Use Zone" = "#ffb36b"),
                                           name = "Australian Marine Parks"
)

# State
wampa_fills <- scale_fill_manual(values = c(
  # "Marine Management Area" = "#b7cfe1",
  # "Conservation Area" = "#b3a63d",
  "Sanctuary Zone" = "#bfd054",
  "General Use Zone" = "#bddde1",
  # "Recreation Area" = "#f4e952",
  "Special Purpose Zone" = "#c5bcc9"
    # "Marine Nature Reserve" = "#bfd054",
),
name = "State Marine Parks")

# Study metadata/sampling locations
ninbruvs <- read_csv("data/tidy/PtCloates_BRUVS_random-points_broad.habitat.csv") %>%
  dplyr::select(sample, longitude, latitude) %>%
  dplyr::filter(!is.na(latitude)) %>%    
  dplyr::mutate(sample= as.character(sample)) %>%
  glimpse()

ninboss <- read_csv("data/tidy/PtCloates_BOSS_random-points_broad.habitat.csv") %>%
  dplyr::select(sample, longitude, latitude) %>%
  dplyr::mutate(sample= as.character(sample)) %>%
  glimpse()

nin  <- bind_rows(ninbruvs, ninboss)  


abrboss <- read.csv("data/tidy/2021-05_Abrolhos_BOSS_random-points_broad.habitat.csv") %>%
  dplyr::filter(location %in% "NPZ6") %>%  
  dplyr::select(sample, longitude, latitude) %>%
    glimpse()

abrbruvs <- read.csv("data/tidy/2021-05_Abrolhos_BRUVs_random-points_broad.habitat.csv") %>%
  dplyr::filter(location %in% "NPZ6") %>%  
  dplyr::select(sample, longitude, latitude) %>%
  glimpse()

abr <- bind_rows(abrboss, abrbruvs) 

swc <- read.csv("data/tidy/2020-2021_south-west_BOSS-BRUV.Habitat.csv") %>%
  dplyr::select(sample, longitude, latitude) %>%
  glimpse()


# Make plots
# Inset 1 - Ningaloo
i1 <- ggplot() +
  # geom_contour_filled(data = bathdf, aes(x = x, y = y, z = Z,
  #                                       fill = after_stat(level)),
  #                     breaks = c(0, -30, -70, -200, -700, -2000, -4000, -10000), alpha = 4/5) +
  # scale_fill_grey(start = 1, end = 0.5 , guide = "none") +
  # geom_contour(data = bathdf, aes(x = x, y = y, z = Z),
  #              breaks = c(0, -30, -70, -200, -700, -2000, -4000, -10000), colour = "white", alpha = 1, size = 0.2) +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  labs(fill = "State Managed Areas") +
  terr_fills +
  new_scale_fill() +
  geom_sf(data = mpa, aes(fill = ZoneName), alpha = 3/5, colour = NA) +
  nmpa_fills +
  labs(x = NULL, y = NULL, fill = "Australian Marine Parks") +
  guides(fill = guide_legend(override.aes = list(size = 0.25), ncol = 2)) +
  new_scale_fill() +
  geom_sf(data = wampa, aes(fill = waname), alpha = 2/5, colour = NA) +
  wampa_fills +
  labs(fill = "State Marine Parks") +
  guides(fill = guide_legend(override.aes = list(size = 0.25), ncol = 2)) +
  new_scale_fill() +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  geom_point(data = nin, aes(longitude, latitude),
             alpha = 1, shape = 10, size = 0.5) +
  coord_sf(xlim = c(113.4, 113.75), 
           ylim = c(-22.65, -22.85)) +                           
  theme_minimal() +
  theme(axis.text = element_text(size = 3),
        legend.title = element_text(size = 4),
        legend.text = element_text(size = 4))
i1 

# Inset 2 - Abrolhos
i2 <- ggplot() +
  # geom_contour_filled(data = bathdf, aes(x = x, y = y, z = Z,
  #                                       fill = after_stat(level)),
  #                     breaks = c(0, -30, -70, -200, -700, -2000, -4000, -10000), alpha = 4/5) +
  # scale_fill_grey(start = 1, end = 0.5 , guide = "none") +
  # geom_contour(data = bathdf, aes(x = x, y = y, z = Z),
  #              breaks = c(0, -30, -70, -200, -700, -2000, -4000, -10000), colour = "white", alpha = 1, size = 0.2) +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA, show.legend = F) +
  terr_fills +
  new_scale_fill() +
  geom_sf(data = mpa, aes(fill = factor(ZoneName)), alpha = 3/5, colour = NA) +
  nmpa_fills +
  labs(x = NULL, y = NULL, fill = "Australian Marine Parks") +
  guides(fill = guide_legend(override.aes = list(size = 0.25), ncol = 2)) +
  new_scale_fill() +
  geom_sf(data = wampa, aes(fill = factor(waname)), alpha = 2/5, colour = NA) +
  wampa_fills +
  labs(fill = "State Marine Parks") +
  guides(fill = guide_legend(override.aes = list(size = 0.25), ncol = 2)) +
  new_scale_fill() +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  geom_point(data = abr, aes(longitude, latitude),
             alpha = 1, shape = 10, size = 0.5) +
  coord_sf(xlim = c(113.2, 113.8), 
           ylim = c(-28.0, -28.35)) +                           
  theme_minimal() +
  theme(axis.text = element_text(size = 3),
        legend.title = element_text(size = 4),
        legend.text = element_text(size = 4)) 
i2

# Inset 3 - SwC
i3 <- ggplot() +
  # geom_contour_filled(data = bathdf, aes(x = x, y = y, z = Z,
  #                                       fill = after_stat(level)),
  #                     breaks = c(0, -30, -70, -200, -700, -2000, -4000, -10000), alpha = 4/5) +
  # scale_fill_grey(start = 1, end = 0.5 , guide = "none") +
  # geom_contour(data = bathdf, aes(x = x, y = y, z = Z),
  #              breaks = c(0, -30, -70, -200, -700, -2000, -4000, -10000), colour = "white", alpha = 1, size = 0.2) +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA, show.legend = F) +
  labs(fill = "State Managed Areas") +
  terr_fills +
  new_scale_fill() +
  geom_sf(data = mpa, aes(fill = ZoneName), alpha = 3/5, colour = NA) +
  nmpa_fills +
  labs(x = NULL, y = NULL, fill = "Australian Marine Parks") +
  guides(fill = guide_legend(override.aes = list(size = 0.25), ncol = 2)) +
  new_scale_fill() +
  geom_sf(data = wampa, aes(fill = waname), alpha = 2/5, colour = NA) +
  wampa_fills +
  labs(fill = "State Marine Parks") +
  guides(fill = guide_legend(override.aes = list(size = 0.25), ncol = 2)) +
  new_scale_fill() +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  geom_point(data = swc, aes(longitude, latitude),
             alpha = 1, shape = 10, size = 0.5) +
  coord_sf(xlim = c(min(swc$longitude), 115.1), 
           ylim = c(min(swc$latitude), max(swc$latitude))) +                           
  theme_minimal() +
  theme(axis.text = element_text(size = 3),
        legend.title = element_text(size = 4),
        legend.text = element_text(size = 4))
i3

# Aus inset
i4 <- ggplot(data = aus) +
  geom_sf(fill = "seashell1", colour = "grey90", size = 0.05, alpha = 4/5) +
  geom_sf(data = aumpa, alpha = 5/6, colour = "grey85", size = 0.02) +
  coord_sf(xlim = c(106, 160), ylim = c(-46, -9)) +
  annotate("rect", xmin = 108, xmax = 125, ymin = -37, ymax = -13,   # Change here 
           colour = "grey25", fill = "white", alpha = 1/5, size = 0.2) +
  theme_bw() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "grey70"))
i4

# Wa inset
i5 <- ggplot(data = aus) +
  geom_sf(fill = "seashell1", colour = "grey90", size = 0.05, alpha = 4/5) +
  geom_sf(data = aumpa, alpha = 5/6, colour = "grey85", size = 0.02) +
  coord_sf(xlim = c(108, 125), ylim = c(-37, -13)) +
  annotate("rect", xmin = 113.4, xmax = 113.8, ymin = -22.5, ymax = -22.9,   # Ningaloo
           colour = "grey25", fill = "white", alpha = 1/5, size = 0.2) +
  annotate("rect", xmin = 113, xmax = 114.3, ymin = -28.1213, ymax = -27.0953,   # Abrolhos
           colour = "grey25", fill = "white", alpha = 1/5, size = 0.2) +
  annotate("rect", xmin = 114.4664, xmax = 115.1, ymin = -34.1487, ymax = -33.66369,   # SwC
           colour = "grey25", fill = "white", alpha = 1/5, size = 0.2) +
  theme_bw() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "grey70"))
i5

design <- "
14
24
35
"

inset1 <- i5 + inset_element(i4, left = 0.01, bottom = 0.75, right = 0.4, top = 1)
inset1

layout <- i1 + i2 + i3 + inset1 + guide_area() + plot_layout(design = design, guides = "collect")
layout

ggsave("plots/overall-sampling-locations.png", dpi = 300, width = 4, height = 6)

