###
# Project: ** GC PhD Chapter 2 Habitat Spatial Predictions  **
# Data:    BRUVS, BOSS Habitat data
# Task:    Habitat spatial prediction
# author:  Kingsley Griffin & Claude Spencer
# date:    ** 6/09/2022 **
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
name <- "PtCloates"                                                              # Change here

# Set up WGSCRS and load spatial covariates from 02_spatial_layers.R 
wgscrs <- "+proj=longlat +datum=WGS84 +south"                              # Latlong projection 

# read in
habi   <- readRDS("data/tidy/PtCloates_habitat-bathy-derivatives.rds")           # Merged data from 'R/03_mergedata.R'
preds  <- readRDS("data/spatial/rasters/PtCloates_spatial_covariates.rds")       # Spatial covs from 'R/02_spatial_layers.R'
preds <- rast(list(preds))
#preds <- rast(preds)
preds[[1]] <- clamp(preds[[1]], upper=-25, lower=-215, values=FALSE)
preds <- mask(preds,preds[[1]])
plot(preds)
preddf <- as.data.frame(preds, xy = TRUE, na.rm = TRUE)
preddf$depth <- abs(preddf$Z)                                                   # Converts depth to absolute value - make sure you aren't predicting onto data with negative values!

summary(habi$depth)
# reduce predictor space to fit survey area
# habisp <- SpatialPointsDataFrame(coords = cbind(habi$longitude, 
#                                                 habi$latitude), data = habi,
#                                  proj4string = wgscrs)
habisp <- vect(habi, geom = c("longitude", "latitude"), crs = wgscrs, keep = T)  

sbuff  <- terra::buffer(habisp, 10000)                                         # Buffer should be in metres
plot(sbuff)
# Use formula from top model from '2_modelselect.R'
# m_kelps <- gam(cbind(kelps, broad.total.points.annotated - kelps) ~                 #No kelp at Pt Cloates
#                  s(depth,     k = 5, bs = "cr")  + 
#                  s(detrended, k = 5, bs = "cr") + 
#                  s(roughness, k = 5, bs = "cr"), 
#                data = habi, method = "REML", family = binomial("logit"))
# summary(m_kelps)

# m_macro <- gam(cbind(macroalgae, broad.total.points.annotated - macroalgae) ~         #No macroalgae at Pt Cloates
#                  s(depth,     k = 5, bs = "cr")  + 
#                  s(detrended, k = 5, bs = "cr") + 
#                  s(roughness, k = 5, bs = "cr"), 
#                data = habi, method = "REML", family = binomial("logit"))
# summary(m_macro)

m_inverts <- gam(cbind(inverts, broad.total.points.annotated - inverts) ~ 
            s(depth,     k = 5, bs = "cr") + 
            s(detrended, k = 5, bs = "cr") + 
            s(TPI,       k = 5, bs = "cr"), 
          data = habi, method = "REML", family = binomial("logit"))
summary(m_inverts)

m_sand <- gam(cbind(sand, broad.total.points.annotated - sand) ~ 
                s(depth,     k = 5, bs = "cr") + 
                s(detrended, k = 5, bs = "cr") + 
                s(TPI,       k = 5, bs = "cr"), 
              data = habi, method = "REML", family = binomial("logit"))
summary(m_sand)

# m_rock <- gam(cbind(rock, broad.total.points.annotated - rock) ~ 
#                 s(depth, k = 5, bs = "cr") + 
#                 s(detrended,  k = 5, bs = "cr") + 
#                 s(roughness,    k = 5, bs = "cr"), 
#               data = habi, method = "REML", family = binomial("logit"))
# summary(m_rock)

# predict, rasterise and plot
preddf <- cbind(preddf, 
                #"pkelps" = predict(m_kelps, preddf, type = "response"),
                #"pmacroalg" = predict(m_macro, preddf, type = "response"),
                "psand" = predict(m_sand, preddf, type = "response", se.fit = T),
                #"prock" = predict(m_rock, preddf, type = "response", se.fit = T),
                "pinverts" = predict(m_inverts, preddf, type = "response", se.fit = T))

#min_Z <- min(habi$Z)
#max_Z <- max(habi$Z)

# reduce prediction area to within sampled range
preddf <- preddf %>%
  filter(Z >= -215, Z <= -71)


prasts <- rast(preddf, crs = wgscrs) 
plot(prasts)

# subset to 10km from sites only
sprast <- mask(prasts, sbuff)
plot(sprast)

# Tidy and output data as a dataframe #AND filter pipe for depth less than 30m
spreddf         <- as.data.frame(sprast, xy = TRUE, na.rm = T) #%>%
#  dplyr::filter(Z < -30)

# Add a colum that categorises the dominant habitat class
spreddf$dom_tag <- apply(spreddf%>%dplyr::select(psand.fit, pinverts.fit), 1, # Set columns manually here only 12 to 14 for Pt Cloates
                        FUN = function(x){names(which.max(x))})
spreddf$dom_tag <- sub('.', '', spreddf$dom_tag)                                # Removes the p but not really sure why haha
head(spreddf)                                                                   # Check to see if it all looks ok

# Save the output
saveRDS(spreddf, paste(paste0('output/PtCloates/', name), 'spatial_habitat_predictions.rds', sep = "_"))

write.csv(spreddf, file = paste(paste0('output/PtCloates/', name), 'spatial_habitat_predictions.csv', sep = "_"), row.names = FALSE)



