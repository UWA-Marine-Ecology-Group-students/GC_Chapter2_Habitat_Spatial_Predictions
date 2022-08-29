###
# Project: ** Add here **
# Data:    BRUVS, BOSS Habitat data
# Task:    Habitat spatial prediction
# author:  Kingsley Griffin & Claude Spencer
# date:    ** Add here **
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
library(raster)

# Set your study name
name <- "Abrolhos"                                                              # Change here

# Set up CRS and load spatial covariates from 02_spatial_layers.R 
wgscrs <- CRS("+proj=longlat +datum=WGS84 +south")                              # Latlong projection 

# read in
habi   <- readRDS("data/tidy/Abrolhos_habitat-bathy-derivatives.rds")           # Merged data from 'R/03_mergedata.R'
preds  <- readRDS("data/spatial/rasters/Abrolhos_spatial_covariates.rds")       # Spatial covs from 'R/02_spatial_layers.R'
preddf <- as.data.frame(preds, xy = TRUE, na.rm = TRUE)
preddf$depth <- abs(preddf$Z)                                                   # Converts depth to absolute value - make sure you aren't predicting onto data with negative values!

# reduce predictor space to fit survey area
habisp <- SpatialPointsDataFrame(coords = cbind(habi$longitude, 
                                                habi$latitude), data = habi,
                                 proj4string = wgscrs)
sbuff  <- raster::buffer(habisp, 10000)                                         # Buffer should be in metres
plot(sbuff)
# Use formula from top model from '2_modelselect.R'
m_kelps <- gam(cbind(kelps, broad.total.points.annotated - kelps) ~ 
                 s(depth,     k = 5, bs = "cr")  + 
                 s(detrended, k = 5, bs = "cr") + 
                 s(roughness, k = 5, bs = "cr"), 
               data = habi, method = "REML", family = binomial("logit"))
summary(m_kelps)

m_macro <- gam(cbind(macroalgae, broad.total.points.annotated - macroalgae) ~ 
                 s(depth,     k = 5, bs = "cr")  + 
                 s(detrended, k = 5, bs = "cr") + 
                 s(roughness, k = 5, bs = "cr"), 
               data = habi, method = "REML", family = binomial("logit"))
summary(m_macro)

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

# predict, rasterise and plot
preddf <- cbind(preddf, 
                "pkelps" = predict(m_kelps, preddf, type = "response"),
                "pmacroalg" = predict(m_macro, preddf, type = "response"),
                "psand" = predict(m_sand, preddf, type = "response"),
                "prock" = predict(m_rock, preddf, type = "response"),
                "pinverts" = predict(m_inverts, preddf, type = "response"))

prasts <- rasterFromXYZ(preddf) 
plot(prasts)

# subset to 10km from sites only
sprast <- mask(prasts, sbuff)
plot(sprast)

# Tidy and output data as a dataframe
spreddf         <- as.data.frame(sprast, xy = TRUE, na.rm = T)

# Add a colum that categorises the dominant habitat class
spreddf$dom_tag <- apply(spreddf[12:16], 1, # Set columns manually here
                        FUN = function(x){names(which.max(x))})
spreddf$dom_tag <- sub('.', '', spreddf$dom_tag)                                # Removes the p but not really sure why haha
head(spreddf)                                                                   # Check to see if it all looks ok

# Save the output
saveRDS(spreddf, paste(paste0('output/fssgam - habitat/', name), 'spatial_habitat_predictions.rds', sep = "_"))
