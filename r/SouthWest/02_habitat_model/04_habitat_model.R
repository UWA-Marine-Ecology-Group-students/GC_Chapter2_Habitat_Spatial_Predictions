###
# Project: ** GC PhD Chapter 2 Habitat Spatial Predictions  **
# Data:    BRUVS, BOSS Habitat data
# Task:    Habitat spatial prediction
# author:  Kingsley Griffin & Claude Spencer
# date:    ** 14/09/2022 **
##

# This script spatially predicts key habitat classes using bathymetry derivatives
# Models are chosen using the FSS-GAM selection process in 04_modelselect.R

# Clear your environment
rm(list = ls())

# Load libraries
library(dplyr)
library(reshape2)
library(mgcv)
library(ggplot2)
library(viridis)
library(terra)

# Set your study name
name <- "SouthWest"                                                              # Change here

# Set up WGSCRS and load spatial covariates from 02_spatial_layers.R 
wgscrs <- "+proj=longlat +datum=WGS84 +south"                              # Latlong projection 

# read in
habi   <- readRDS("data/tidy/SouthWest_habitat-bathy-derivatives.rds")           # Merged data from 'R/03_mergedata.R'
preds  <- readRDS("data/spatial/rasters/SouthWest_spatial_covariates.rds")       # Spatial covs from 'R/02_spatial_layers.R'
#preds <- rast(preds)
preddf <- as.data.frame(preds, xy = TRUE, na.rm = TRUE)
preddf$depth <- preddf$Z                                                        #  Converts depth to absolute value - make sure you aren't predicting onto data with negative values!

# reduce predictor space to fit survey area
# habisp <- SpatialPointsDataFrame(coords = cbind(habi$longitude, 
#                                                 habi$latitude), data = habi,
#                                  proj4string = wgscrs)
habisp <- vect(habi, geom = c("longitude", "latitude"), crs = wgscrs, keep = T)  

sbuff  <- terra::buffer(habisp, 10000)                                         # Buffer should be in metres
plot(sbuff)
# Use formula from top model from '3_modelselect.R'
m_seagrasses <- gam(cbind(seagrasses, broad.total.points.annotated - seagrasses) ~ 
                 s(depth,     k = 5, bs = "cr")  + 
                 s(detrended, k = 5, bs = "cr") + 
                 s(roughness, k = 5, bs = "cr"), 
               data = habi, method = "REML", family = binomial("logit"))
summary(m_seagrasses)

m_macro <- gam(cbind(macroalgae, broad.total.points.annotated - macroalgae) ~
                 s(depth,     k = 5, bs = "cr")  +
                 s(detrended, k = 5, bs = "cr") +
                 s(roughness, k = 5, bs = "cr"),
               data = habi, method = "REML", family = binomial("logit"))
summary(m_macro)

# m_macro <- gam(cbind(macroalgae, broad.total.points.annotated - macroalgae) ~ 
#                  # s(depth,     k = 5, bs = "cr"),   
#                  s(detrended, k = 5, bs = "cr") +
#                  s(slope, k = 5, bs = "cr"),
#                data = habi, method = "REML", family = binomial("logit"))
# summary(m_macro)

# m_reef <- gam(cbind(reef, broad.total.points.annotated - reef) ~ 
#                  s(depth,     k = 5, bs = "cr")  + 
#                  s(detrended, k = 5, bs = "cr") + 
#                  s(slope, k = 5, bs = "cr"), 
#                data = habi, method = "REML", family = binomial("logit"))
# summary(m_reef)

m_inverts <- gam(cbind(inverts, broad.total.points.annotated - inverts) ~ 
            s(depth,     k = 5, bs = "cr") + 
            s(detrended, k = 5, bs = "cr") + 
            s(roughness,       k = 5, bs = "cr"), 
          data = habi, method = "REML", family = binomial("logit"))
summary(m_inverts)

m_sand <- gam(cbind(sand, broad.total.points.annotated - sand) ~ 
                s(depth,     k = 5, bs = "cr") + 
                s(detrended, k = 5, bs = "cr") + 
                s(roughness,       k = 5, bs = "cr"), 
              data = habi, method = "REML", family = binomial("logit"))
summary(m_sand)

m_rock <- gam(cbind(rock, broad.total.points.annotated - rock) ~ 
                s(depth, k = 5, bs = "cr") + 
                s(detrended,  k = 5, bs = "cr") + 
                s(roughness,    k = 5, bs = "cr"), 
              data = habi, method = "REML", family = binomial("logit"))
summary(m_rock)


ggplot() +
geom_point(data = habi, aes(x = depth, y = sand))

# predict, rasterise and plot
preddf <- cbind(preddf, 
                "pseagrasses" = predict(m_seagrasses, preddf, type = "response", se.fit = T),
                "pmacroalg" = predict(m_macro, preddf, type = "response", se.fit = T),
                # "preef" = predict(m_reef, preddf, type = "response"),
                "psand" = predict(m_sand, preddf, type = "response", se.fit = T),
                "prock" = predict(m_rock, preddf, type = "response", se.fit = T),
                "pinverts" = predict(m_inverts, preddf, type = "response", se.fit = T))

# reduce prediction area to within sampled range
preddf <- preddf[preddf$depth > min(habi$Z) & 
                   preddf$depth < max(habi$Z), ]

prasts <- rast(preddf) 
plot(prasts)

# subset to 10km from sites only
sprast <- mask(prasts, sbuff)
plot(sprast)

# Tidy and output data as a dataframe #AND filter pipe for depth less than 30m
rastdf         <- as.data.frame(prasts, xy = TRUE, na.rm = T) #%>%
  #dplyr::filter(Z <- -30)

#GC ATTEMPT
rastdf$dom_tag <- apply(rastdf%>% dplyr::select(pseagrasses.fit, pmacroalg.fit, psand.fit, prock.fit, pinverts.fit), 1,
                        FUN = function(x){names(which.max(x))}) 
rastdf$dom_tag <- sub('.', '', rastdf$dom_tag)
head(rastdf)  

# # # Add a colum that categorises the dominant habitat class
#  rastdf$dom_tag <- apply(rastdf[12:15], 1, # Set columns manually here
#                          FUN = function(x){names(which.max(x))})
#  rastdf$dom_tag <- sub('.', '', rastdf$dom_tag)                          # Removes the p but not really sure why haha
#  head(rastdf)                                                             # Check to see if it all looks ok

# Save the output
saveRDS(rastdf, paste(paste0('output/SWC/', name), 'spatial_habitat_predictions.rds', sep = "_"))

