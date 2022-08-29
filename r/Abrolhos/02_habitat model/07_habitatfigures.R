###
# Project: ** Add here **
# Data:    BRUVS, BOSS Habitat data
# Task:    Habitat figures
# author:  Kingsley Griffin & Claude Spencer
# date:    ** Add here **
##

# This script makes report ready figures and plots

# CONTENTS
# Figure 1. Dominant habitat class map
# Figure 2. Individual habitat class predictions - facetted plot
# Figure 3. Bathymetry derivatives
# Figure 4. Predicted Relief
# Figure 5. Relief spatial random effect
# Figure 6. National Reef Model predictions

# Clear your environment
rm(list = ls())

# Load libraries
library(reshape2)
library(ggplot2)
library(viridis)
library(raster)
library(patchwork)
library(sf)
library(dplyr)

# Set your study name
name <- "Abrolhos"                                                              # Change here

# Set CRS for shapefiles
wgscrs <- CRS("+proj=longlat +datum=WGS84")                                     # Lat long projection
sppcrs <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")       # UTM projection - check your UTM Zone!

# Bring in spatial layers
# Load aus outline, state and commonwealth marine parks
aus     <- st_read("data/spatial/shapefiles/cstauscd_r.mif")                    # Geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
aus     <- aus[aus$FEAT_CODE == "mainland", ]                                   # Add islands here if needed
aumpa   <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")   # All aus mpas
st_crs(aus) <- st_crs(aumpa)                                                    # Set CRS to match - WGS84 and GDA94 effectively the same
e <- extent(112, 116, -30, -26)                                                 # Change your extent here
mpa <- st_crop(aumpa, e)                                                        # All commonwealth zones in the study area
npz <- mpa[mpa$ZoneName %in% "National Park Zone", ]                            # Only National Park Zones in the study area

# Load Jac's National Reef Model
# jacmap  <- raster("data/spatial/rasters/ecosystem-types-19class-naland.tif")    # Jac's aus habitat map
# jacmap  <- crop(jacmap, e)                                                      # Crop to the general study area

# Load tidy habitat data from 03_mergedata
habi    <- readRDS(paste(paste0('data/tidy/', name), 
                         'habitat-bathy-derivatives.rds', sep = "_")) %>%
  glimpse()

# Load terrestrial parks
# terrnp <- st_read("data/spatial/shp/Legislated_Lands_and_Waters_DBCA_011.shp") %>%  # Terrestrial reserves
#   dplyr::filter(leg_catego %in% c("Nature Reserve", "National Park"))
# terrnp <- st_crop(terrnp, xmin = 113, ymin = -30, xmax = 116, ymax = -26)       # Just abrolhos area

# Load the coastal waters boundary
cwatr  <- st_read('data/spatial/shapefiles/amb_coastal_waters_limit.shp')              

# Load bathymetry data
bathdf <- readRDS(paste(paste0('data/spatial/rasters/',                         
                               name), 'ga_bathy.rds', sep = "_"))

# read in spatial predictions from 'R/05_habitat_model.R'
spreddf <- readRDS(paste(paste0('output/fssgam - habitat/', name),
                         'spatial_habitat_predictions.rds', sep = "_")) %>%
  dplyr::mutate(dom_tag = as.factor(dom_tag)) %>%                               # Factorise
  dplyr::mutate(dom_tag = dplyr::recode(dom_tag,                                # Tidy names for plot legend
                                 kelps = "Kelp",
                                 macroalg = "Macroalgae",
                                 rock = "Rock",
                                 sand = "Sand",
                                 inverts = "Sessile invertebrates")) %>%
  glimpse()

# Figure 1: Categorical habitat maps ----
# Assign habitat class colours
hab_cols <- scale_fill_manual(values = c("Kelp" = "goldenrod1",
                                         "Macroalgae" = "darkgoldenrod4",
                                         "Rock" = "grey40",
                                         "Sand" = "wheat",
                                         "Sessile invertebrates" = "plum"
))

#Build plot elements for site 1
p1 <- ggplot() +
  geom_tile(data = spreddf, aes(x, y, fill = dom_tag)) +
  hab_cols +                                                                    # Class colours
  geom_sf(data = npz, fill = NA, colour = "#7bbc63") +                          # Add national park zones
  geom_contour(data = bathdf, aes(x = x, y = y, z = Z),                         # Contour lines
               breaks = c(0, - 30, -70, - 200),                                 # Contour breaks - change to binwidth for regular contours
               colour = "grey54",
               alpha = 1, size = 0.5) +                                         # Transparency and linewidth
  coord_sf(xlim = c(113.169637818, 113.592952023),                              # Set plot limits
           ylim = c(-28.147530872, -27.951387525)) +
  labs(x = NULL, y = NULL, fill = "Habitat",                                    # Labels  
       colour = NULL, title = "Shallow Bank") +
  annotate("text", x = c(113.428836237, 113.388204915, 113.255153069),          # Add contour labels manually
           y = c(-28.078038504, -28.078038504, -28.078038504), 
           label = c("30m", "70m", "200m"),
           size = 2, colour = "grey54") +
  theme_minimal()
png(filename = paste(paste("plots/habitat", name, sep = "/"),                   # Save output
                     "dominant_habitat.png", sep = "_"),
    width = 8, height = 4, res = 300, units = "in")                             # Change the dimensions here as necessary
p1
dev.off()

# Figure 2. Individual habitat class predictions ----
# Melt classes for faceting
widehabit <- spreddf %>%
  tidyr::pivot_longer(cols = starts_with("p"),                                  # Careful here that you don't have any other columns starting with 'p'
                      values_to = "value", names_to = "variable") %>%
  dplyr::mutate(variable = dplyr::recode(variable,                              # Tidy variable names
                pkelps = "Kelp",
                pmacroalg = "Macroalgae",
                prock = "Rock",
                psand = "Sand",
                pinverts = "Sessile invertebrates")) %>%
  glimpse()

# Make a dataframe for your contour line annotations - doesn't work otherwise for facetted plots
dep_ann <- data.frame(x = c(113.433617268, 113.392982290, 113.255855826),                            
                      y = c(-28.087180374, -28.087180374, -28.087180374),
                      label = c("30m", "70m", "200m"))

# Build the plot for the first site
p22 <- ggplot() +
  geom_tile(data = widehabit, 
            aes(x, y, fill = value)) +
  scale_fill_viridis(direction = -1, limits = c(0, max(widehabit$value))) +
  geom_sf(data = npz, fill = NA, colour = "#7bbc63") +                          # National park zones
  geom_contour(data = bathdf, aes(x, y, z = Z),                                 # Contour lines
               breaks = c(0, -30, -70, -200), colour = "grey54",
               alpha = 1, size = 0.5) +
  geom_text(data = dep_ann,aes(x,y,label = label),
            inherit.aes = F, size = 2, colour = "grey36") +
  coord_sf(xlim = c(113.169637818, 113.592952023),                              # Set plot limits
           ylim = c(-28.147530871, -27.951387524)) +
  labs(x = NULL, y = NULL, fill = "Habitat (p)", title = "Shallow Bank") +      # Labels
  theme_minimal() +
  facet_wrap(~variable, ncol = 1)                                               # Facet for each variable

png(filename = paste(paste("plots/habitat", name, sep = "/"),                   # Save the output
                     "habitat_class_predicted.png", sep = "_"),
    width = 5, heigh = 10, res = 300, units = "in")                             # Change the dimensions here as necessary
p22
dev.off()

# Figure 3. Bathymetry derivatives ----
# depth
pd <- ggplot() +
  geom_tile(data = spreddf[spreddf$sitens == 1, ], aes(x, y, fill = depth)) +
  scale_fill_viridis(option = "A", direction = -1,
                     limits = c(0, max(spreddf$depth))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 3, ], fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL, title = "Big Bank") +
  guides(fill = "none") +
  theme_minimal()

pdb <- ggplot() +
  geom_tile(data = spreddf[spreddf$sitens == 0, ], aes(x, y, fill = depth)) +
  scale_fill_viridis(option = "A", direction = -1,
                     limits = c(0, max(spreddf$depth))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 2, ], fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL,
       fill = "Depth", title = "Shallow Bank") +
  theme_minimal()
pd + pdb

# tpi
pt <- ggplot() +
  geom_tile(data = spreddf[spreddf$sitens == 1, ], aes(x, y, fill = tpi)) +
  scale_fill_viridis(option = "D", direction = 1,
                     limits = c( min(spreddf$tpi), max(spreddf$tpi))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 3, ], fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL) +
  guides(fill = "none") +
  theme_minimal()
ptb <- ggplot() +
  geom_tile(data = spreddf[spreddf$sitens == 0, ], aes(x, y, fill = tpi)) +
  scale_fill_viridis(option = "D", direction = 1,
                     limits = c( min(spreddf$tpi), max(spreddf$tpi))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 2, ], fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL,
       fill = "TPI") +
  theme_minimal()
pt + ptb

# roughness
pr <- ggplot() +
  geom_tile(data = spreddf[spreddf$sitens == 1, ], aes(x, y, fill = roughness)) +
  scale_fill_viridis(option = "D", direction = 1,
                     limits = c( min(spreddf$roughness), max(spreddf$roughness))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 3, ], fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL) +
  guides(fill = "none") +
  theme_minimal() 
prb <- ggplot() +
  geom_tile(data = spreddf[spreddf$sitens == 0, ], aes(x, y, fill = roughness)) +
  scale_fill_viridis(option = "D", direction = 1,
                     limits = c(min(spreddf$roughness), max(spreddf$roughness))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 2, ], fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL,
       fill = "Roughness") +
  theme_minimal()
pr + prb

# detrended
pdt <- ggplot() +
  geom_tile(data = spreddf[spreddf$sitens == 1, ], aes(x, y, fill = detrended)) +
  scale_fill_viridis(option = "D", direction = 1,
                     limits = c( min(spreddf$detrended), max(spreddf$detrended))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 3, ], fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL) +
  guides(fill = "none") +
  theme_minimal()
pdtb <- ggplot() +
  geom_tile(data = spreddf[spreddf$sitens == 0, ], aes(x, y, fill = detrended)) +
  scale_fill_viridis(option = "D", direction = 1,
                     limits = c(min(spreddf$detrended), max(spreddf$detrended))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 2, ], fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL,
       fill = "Detrended") +
  theme_minimal() 
pdt + pdtb

# Figure 4. Predicted relief ----
pcelldf <- readRDS('output/predicted_relief_site.rds')
pcelldf$sitens <- ifelse(pcelldf$y > 6940000, 1, 0)
pcelldf$prelief[pcelldf$prelief < 0] <- 0

p4 <- ggplot() +
  geom_tile(data = pcelldf[pcelldf$sitens == 1, ], aes(x, y, fill = prelief)) +
  scale_fill_viridis(option = "C", direction = -1, 
                     limits = c(0, max(pcelldf$prelief))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 3, ], fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL, 
       fill = "p. relief") +
  guides(fill = "none") +
  theme_minimal()

p42 <- ggplot() +
  geom_tile(data = pcelldf[pcelldf$sitens == 0, ], aes(x, y, fill = prelief)) +
  scale_fill_viridis(option = "C", direction = -1, 
                     limits = c(0, max(pcelldf$prelief))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 2, ], fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL, 
       fill = "Relief score") +
  theme_minimal()

# relief only
p4 + p42 + plot_layout(widths = c(0.44, 0.56))
ggsave("plots/spatial/site_relief_p.png", width = 10, height = 6, dpi = 160)

# combined spatial layers

(pd + pdb)   /
  (pt + ptb)   /
  (pr + prb)   /
  (pdt + pdtb) /
  (p4 + p42)   +
  plot_layout(widths = c(0.44, 0.56)) &
  theme(text = element_text(size = 8))
ggsave("plots/spatial/site_spatial_layers.png", width = 10, height = 12, dpi = 160)


# Figure 5. Relief spatial random effect
p5 <- ggplot() +
  geom_tile(data = pcelldf[pcelldf$sitens == 1, ], aes(x, y, fill = p_sp)) +
  scale_fill_viridis(option = "B", 
                     limits = c(min(pcelldf$p_sp), max(pcelldf$p_sp))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 3, ], fill = NA, colour = "#7bbc63") +
  geom_point(data = habi[habi$ns == 1, ], aes(longitude.1, latitude.1), 
             alpha = 0.7, colour = "grey70", size = 1, shape = 3) +
  labs(x= NULL, y = NULL, title = "Big Bank") +
  guides(fill = "none") +
  theme_minimal()

p52 <- ggplot() +
  geom_tile(data = pcelldf[pcelldf$sitens == 0, ], aes(x, y, fill = p_sp)) +
  scale_fill_viridis(option = "B", 
                     limits = c(min(pcelldf$p_sp), max(pcelldf$p_sp))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 2, ], fill = NA, colour = "#7bbc63") +
  geom_point(data = habi[habi$ns == 0, ], aes(longitude.1, latitude.1), 
             alpha = 0.7, colour = "grey70", size = 2, shape = 3) +
  labs(x= NULL, y = NULL, 
       fill = "Spatial\ndependence", title = "Shallow Bank") +
  theme_minimal()

p5 + p52 + plot_layout(widths = c(0.44, 0.56))
ggsave("plots/spatial/site_relief_spatialeffect.png", 
       width = 10, height = 3, dpi = 160)

# Figure 6. National Reef Model predictions
# jac's map, eh
# sort out the classes
sitebathy <- readRDS('output/ga_bathy_fine.rds')                                # finer bathy
colnames(sitebathy)[3] <- "Depth"
# sitebathy <- sitebathy[sitebathy$Depth > -1000, ]                               # trim to reduce legend
sitebathy <- sitebathy[sitebathy$x > 112.7 & sitebathy$x < 114.4, ] 
sitebathy <- sitebathy[sitebathy$y > -28.4 & sitebathy$y < -26.6, ]

jlevs  <- ratify(jacmap)
jclass <- levels(jlevs)[[1]]
jclass[["class"]] <- c("Shelf.unvegetated.soft.sediments",
                       "Upper.slope.unvegetated.soft.sediments", 
                       "Mid.slope.sediments",
                       "Lower.slope.reef.and.sediments",
                       "Abyssal.reef.and.sediments", 
                       "Seamount.soft.sediments", 
                       "Shelf.vegetated.sediments", 
                       "Shallow.coral.reefs.less.than.30.m.depth", 
                       "Mesophotic.coral.reefs", 
                       "Rariophotic.shelf.reefs", 
                       "Upper.slope.rocky.reefs.shelf.break.to.700.m.depth", 
                       "Mid.slope.reef", 
                       "Artificial.reefs.pipelines.and.cables")                 # The class names
levels(jacmap) <- jclass

jmap_df <- as.data.frame(jacmap, xy = TRUE, na.rm = TRUE)
colnames(jmap_df)[3] <- "classname"
jmap_df$classname <- gsub("\\.", " ", jmap_df$classname)                        # Replace . with space in names

jcls_cols <- scale_fill_manual(values = c(
  "Shallow coral reefs less than 30 m depth" = "coral2", 
  "Shelf unvegetated soft sediments" = "cornsilk1",
  "Shelf vegetated sediments" = "seagreen3",
  "Mesophotic coral reefs" = "darkorange3",
  "Rariophotic shelf reefs" = "steelblue3",
  "Upper slope unvegetated soft sediments" = "wheat1",
  "Mid slope sediments" = "#f7d29c"))  # navajowhite1 - made slightly darker

waterr_cols <- scale_fill_manual(values = c("National Park" = "#c4cea6",
                                            "Nature Reserve" = "#e4d0bb"),
                                 guide = "none")

# assign mpa colours - full levels are saved at end of script for future ref
nmpa_cols <- scale_color_manual(breaks = c("National Park Zone", 
                                           "Multiple Use Zone", 
                                           "Special Purpose Zone"), 
                                values = c("National Park Zone" = "#7bbc63",
                                          "Multiple Use Zone" = "#b9e6fb",
                                          "Special Purpose Zone" = "#6daff4"
))

ab_nmp$ZoneName <- factor(ab_nmp$ZoneName, levels = c("Multiple Use Zone", "Special Purpose Zone",
                                                      "Habitat Protection Zone","National Park Zone"
                                                      ))


p7 <- ggplot() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  waterr_cols +
  new_scale_fill() +  
  geom_tile(data = jmap_df, aes(x, y, fill = classname)) +
  jcls_cols +
  geom_contour(data = sitebathy, aes(x = x, y = y, z = Depth),
               breaks = c(-30, -70, -200, - 700, - 7000), colour = "black", alpha = 1, size = 0.18) +
  geom_sf(data = ab_nmp, fill = NA, aes(colour = ZoneName)) +
  nmpa_cols+
  labs(color = "Australian Marine Parks") +
  new_scale_color() +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  # annotate("rect", xmin = 113.02, xmax = 113.29, ymin = -27.19, ymax = -27.08,
  #          colour = "grey25", fill = "white", alpha = 1/5, size = 0.2) +
  # annotate("text", x = 113.15, y = -27.05, size = 3, 
  #          colour = "grey20", label = "swabrnpz09") +
  # annotate("rect", xmin = 113.24, xmax = 113.58, ymin = -28.13, ymax = -28.02,
  #          colour = "grey25", fill = "white", alpha = 1/5, size = 0.2) +
  # annotate("text", x = 113.42, y = -27.99, size = 3,
  #          colour = "grey20", label = "swabrnpz06") +
  annotate("text", y = c(-27.875, -27.875,-27.875,-27.875, -26.87), 
           x = c(114.07,  113.32, 113.15, 112.79, 113.167), 
           label = c("30m", "70m", "200m", "700m", "70m"), size = 2) +
  coord_sf(xlim = c(112.8, 114.3), ylim = c(-28.25, -26.7)) +
  labs(fill = "Habitat classification", x = NULL, y = NULL) +
  annotate("point", y = c(-27.7115), x = c(114.1714), size = 0.75) +
  annotate("text", y = c(-27.7115), x = c(114.275),
           label = c("Kalbarri"), size = 3) +
  theme_minimal()

png(filename = "plots/spatial/site_jmonk_natmap.png", width = 8, height = 6,
    units = "in", res = 200)
p7
dev.off()
