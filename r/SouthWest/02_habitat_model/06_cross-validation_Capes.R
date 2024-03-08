#### Project: GC CHapter 2 PhD 
# Data:    BRUVS, BOSS Habitat data
# Task:    Cross validation & Kappa stats
# Author:  Claude Spencer & Gabby Cummins
# Date:    Feb 2024
##

rm(list = ls())
gc()

# Load libraries
library(tidyverse)
library(raster)
library(terra)
library(sp)
library(sf)
sf_use_s2(T)
library(ggplot2)
 library(devtools)
  # install_github("bleutner/RStoolbox")
library(RStoolbox)
library(blockCV)
library(ggnewscale)
library(scales)
library(mgcv)
library(pROC)
library(caret)
library(dplyr)


# Set your study name
name <- "SouthWest"                                                    

# Load data
dat <- readRDS(paste(paste0('data/tidy/', name), 
                     'habitat-bathy-derivatives.rds', sep = "_")) %>%         
  rename(sampleold = sample) %>% 
  mutate(sample =paste(sampleold, campaignid, sep = "_")) %>%# From 
  mutate(x = longitude) %>%
  mutate(y = latitude) %>%
  glimpse()

unique(length(dat$sample))
# Read in and crop the rasters to your study extent
# the rasters should be the same as the as the values in the shapefile

stack <- readRDS(paste(paste0('data/spatial/rasters/', name),      # This is ignored - too big!
                       'spatial_covariates.rds', sep = "_")) %>%
  crop(ext(114.4, 115.1, -34.3, -33.5)) %>%
  brick() 

ext(stack)
rpc = rasterPCA(stack, nComp = 1 , spca = TRUE, nSamples = 5000) ### first PCA component of the raster stack to estimate spatial AC

poly_sf <- st_sf(geometry = st_as_sfc(st_bbox(stack)))   ###### convert the polygon to sf for later
aoi_r <- as(object = poly_sf, Class = "Spatial") ###### an sp polygon of your study areas
boss_sf <- dat %>%   ###### any csv with lat lon that has your samples and rasters extracted at those points 
  sf::st_as_sf(coords = c("x", "y"), crs = "epsg:4326")  ###### can use 'coords = c("x", "y")' instead of wkt if there is no geometry colomn

plot(stack[[1]])
plot(boss_sf["Z"],add = T)

length(unique(boss_sf$sample))
test <- boss_sf %>% group_by(sample) %>% summarise(n = n())
# THIS BIT IS HAVING SOME ISSUES -  NEED TO FIX

# AC range estimation

# range1 <- cv_spatial_autocor(
#   r = rpc$map,                                                            # For some reason even if you input spatraster the output is rasterbrick
#   num_sample = 5000,
#   progress = F,
#   plot = T
# )
# 
# range1$range

plot(rpc$map)

sb1 = cv_spatial(x = boss_sf,
                 r = rpc$map,
                 size = 5000,
                 k = 5,
                 selection = "random",
                 iteration = 100,
                 seed = 1,
                 biomod2 = TRUE,
                 hexagon = FALSE)
cv_plot(sb1)

test <- sb1$blocks

ggplot() +
  geom_sf(data = test)
# creating a dataframe for the RF model 

mod_df = as_Spatial(boss_sf) %>%
  data.frame() %>%
  add_column(block = sb1$folds_ids) %>%
  drop_na(roughness) ## drop any nas in your covariates

length(unique(mod_df$sample))

# # Loop through cross validation for each taxa (habitat) and each fold
habi <- mod_df %>%
  pivot_longer(cols = c("sand", "inverts", "macroalgae", "rock", "seagrasses"),
               names_to = "taxa",
               values_to = "value") %>%
  dplyr::select(-ID) %>%
  dplyr::rename(x = coords.x1, y = coords.x2) %>%
  glimpse()

length(unique(habi$sample))

resp.vars <- unique(habi$taxa)
blocks <- unique(habi$block)

preds  <- readRDS(paste(paste0('data/spatial/rasters/', name),      # This is ignored - too big!
                        'spatial_covariates.rds', sep = "_"))
preddf <- as.data.frame(preds, xy = TRUE, na.rm = TRUE)
wgscrs <- "+proj=longlat +datum=WGS84 +south" 



# Set the models manually here
resp.vars



# Loop through each taxa
# Train GAM off 4 folds, test against last fold
# Absolute value of the distance from the observed to predicted data
for (i in 1:length(resp.vars)) {
  use.dat <- habi[habi$taxa == resp.vars[i],]
  use.dat   <- as.data.frame(use.dat)
  use.dat$observed <- use.dat$value/use.dat$broad.total.points.annotated
  print(resp.vars[i])
  
  for (b in 1:length(blocks)) {
    print(blocks[b])
    train.dat <- use.dat %>% dplyr::filter(!block == blocks[b])
    test.dat  <- use.dat %>% dplyr::filter(block == blocks[b])
    
    # Sand
    mod1 <- gam(cbind(value, broad.total.points.annotated - value) ~ 
                  s(Z,     k = 5, bs = "cr")  +
                  s(detrended, k = 5, bs = "cr") + 
                  s(roughness, k = 5, bs = "cr"), 
                data = train.dat, method = "REML", family = binomial("logit"))
    
    # Inverts
    mod2 <- gam(cbind(value, broad.total.points.annotated - value) ~ 
                  s(Z,     k = 5, bs = "cr")  + 
                  s(detrended, k = 5, bs = "cr") +
                  s(roughness, k = 5, bs = "cr"), 
                data = train.dat, method = "REML", family = binomial("logit"))
    
    # Macroalgae
    mod3 <- gam(cbind(value, broad.total.points.annotated - value) ~ 
                  s(aspect,     k = 5, bs = "cr")  + 
                  s(Z, k = 5, bs = "cr") +
                  s(detrended, k = 5, bs = "cr"), 
                data = train.dat, method = "REML", family = binomial("logit"))
    
    # Rock
    mod4 <- gam(cbind(value, broad.total.points.annotated - value) ~ 
                  s(Z,     k = 5, bs = "cr")  +
                  s(detrended, k = 5, bs = "cr") +
                  s(roughness, k = 5, bs = "cr"), 
                data = train.dat, method = "REML", family = binomial("logit"))
    
    # # Seagrass
    mod5 <- gam(cbind(value, broad.total.points.annotated - value) ~
                  s(Z, k = 5, bs = "cr") +
                  s(detrended, k = 5, bs = "cr") +
                  s(roughness, k = 5, bs = "cr"),
                data = train.dat, method = "REML", family = binomial("logit"))

    mod <- list(mod1, mod2, mod3, mod4, mod5)
    
    modpred <- cbind(preddf, 
                     "predicted" = predict(mod[[i]], preddf, type = "response"))
    modpredr <- rast(modpred %>% dplyr::select(x, y, predicted))
    habi_sp <- vect(test.dat, geom = c("x", "y"), crs = wgscrs)
    habi_df   <- cbind(test.dat, terra::extract(modpredr, habi_sp)) %>%
      dplyr::filter(!is.na(predicted),
                    !is.na(observed))
    if (b == 1) {
      pearsons <- data.frame(taxa = resp.vars[i],
                             fold = blocks[b],
                             pcc = cor(habi_df$observed, habi_df$predicted, method = "pearson"))
      
      plot.dat.b <- habi_df
    }
    else {
      pearsons <- data.frame(taxa = resp.vars[i],
                             fold = blocks[b],
                             pcc = cor(habi_df$observed, habi_df$predicted, method = "pearson")) %>%
        bind_rows(pearsons)
      
      plot.dat.b <- habi_df %>%
        bind_rows(plot.dat.b)
    }
  }
  if (i == 1) {
    pearsons_table <- pearsons 
    
    plot.dat <- plot.dat.b
  }
  else {
    pearsons_table <- pearsons %>%
      bind_rows(pearsons_table)
    
    plot.dat <- plot.dat.b %>%
      bind_rows(plot.dat)
  }
}


pearsons_final <- pearsons_table %>%
  dplyr::mutate(fold = as.character(fold)) %>%
  group_by(taxa) %>%
  group_modify(~ add_row(.x, pcc = mean(.$pcc), fold = "mean")) %>%
  ungroup() %>%
  pivot_wider(names_from = taxa, values_from = pcc) %>%
  glimpse()

# Save output
write.csv(pearsons_final, paste0("output/crossval/", name, "_pearsons-coefficient.csv"),
          row.names = F)

# Loop through each taxa
# Train GAM off 4 folds, test against last fold
# Accuracy - number of True positives / total observations
###FOR KAPPA STATS

for (i in 1:length(resp.vars)) {
  use.dat <- habi[habi$taxa == resp.vars[i],] 
  use.dat   <- as.data.frame(use.dat)
  print(resp.vars[i])
  
  for (b in 1:length(blocks)) {
    train.dat <- use.dat %>% dplyr::filter(!block == blocks[b])
    test.dat  <- use.dat %>% dplyr::filter(block == blocks[b])
    print(blocks[b])
    
    # Sand
    mod1 <- gam(cbind(value, broad.total.points.annotated - value) ~ 
                  s(Z,     k = 5, bs = "cr")  +
                  s(detrended, k = 5, bs = "cr") + 
                  s(roughness, k = 5, bs = "cr"), 
                data = train.dat, method = "REML", family = binomial("logit"))
    
    # Inverts
    mod2 <- gam(cbind(value, broad.total.points.annotated - value) ~ 
                  s(Z,     k = 5, bs = "cr")  + 
                  s(detrended, k = 5, bs = "cr") +
                  s(roughness, k = 5, bs = "cr"), 
                data = train.dat, method = "REML", family = binomial("logit"))
    
    # Macroalgae
    mod3 <- gam(cbind(value, broad.total.points.annotated - value) ~ 
                  s(Z,     k = 5, bs = "cr")  + 
                  s(detrended, k = 5, bs = "cr") +
                  s(aspect, k = 5, bs = "cr"), 
                data = train.dat, method = "REML", family = binomial("logit"))
    
    # Rock
    mod4 <- gam(cbind(value, broad.total.points.annotated - value) ~ 
                  s(Z,     k = 5, bs = "cr")  +
                  s(detrended, k = 5, bs = "cr") +
                  s(roughness, k = 5, bs = "cr"), 
                data = train.dat, method = "REML", family = binomial("logit"))
    
    # # Seagrasses
    mod5 <- gam(cbind(value, broad.total.points.annotated - value) ~
                  s(Z, k = 5, bs = "cr") +
                  s(detrended, k = 5, bs = "cr") +
                  s(roughness, k = 5, bs = "cr"),
                data = train.dat, method = "REML", family = binomial("logit"))
    
    mod <- list(mod1, mod2, mod3, mod4, mod5)
    
    if (b == 1 & i == 1) {
      modpred <- cbind(preddf, "predicted" = predict(mod[[i]], preddf, type = "response")) %>%
        dplyr::mutate(block = blocks[b]) %>%
        dplyr::select(x, y, predicted, block) %>%
        dplyr::mutate(taxa = resp.vars[i])
      
    }
    else {
      modpred <-  cbind(preddf, "predicted" = predict(mod[[i]], preddf, type = "response")) %>%
        dplyr::mutate(block = blocks[b],
                      taxa = resp.vars[i]) %>%
        dplyr::select(x, y, predicted, block, taxa) %>%
        rbind(modpred)
    }
  }
}

# Save out so you don't need to keep running loop
saveRDS(modpred, "data/tidy/SouthWest_modpreds.rds")                           # Save out to stop re running mega loop -- ignored

# # Categorise raw data into habitat classes
# max.dat <- habi %>%
#   group_by(campaignid, sample, block) %>%
#   slice(which.max(value/broad.total.points.annotated)) %>%
#   ungroup() %>%
#   dplyr::select(x, y, sample, taxa, block) %>%
#   dplyr::rename(observed = taxa) %>%
#   glimpse()

# Categorise raw data into habitat classes
max.dat <- habi %>%
  group_by(sample, block) %>%
  slice(which.max(value/broad.total.points.annotated)) %>%
  ungroup() %>%
  dplyr::select(x, y, taxa, block) %>%
  dplyr::rename(observed = taxa) %>%
  glimpse()

length(unique(habi$sample))

# Categorise model predictions into habitat classes for each fold
modpred <- readRDS("data/tidy/SouthWest_modpreds.rds")

# Make a raster of predicted values for each taxa per fold
for (b in 1:5) {
  print(b)
  rastdat <- modpred %>%
    dplyr::filter(block == b)
  for (i in 1:length(resp.vars)) {
    print(resp.vars[i])
    tempdat <- rastdat %>% dplyr::filter(taxa %in% resp.vars[i]) %>% dplyr::select(x, y, predicted)
    colnames(tempdat)[3] = paste(b, resp.vars[i], sep = "_")
    taxarast <- rast(tempdat)
    if (b == 1 & i == 1) {
      testrast <- taxarast
    }
    else {
      testrast <- rast(list(testrast, 
                            taxarast))
    }}}

names(testrast)
plot(testrast)

# Convert raw data into spatial vector file
max.datv <- vect(max.dat, geom = c("x", "y"))
plot(testrast[[1]])
plot(max.datv, add = T) # Aligns correctly


GAB <-  readRDS(paste0("output/SWC/", name, "_spatial_habitat_predictions.rds"))

# Load in predicted habitat from 04_predict-gam
predhabdf <- readRDS(paste0("output/SWC/", name, "_spatial_habitat_predictions.rds")) %>%   # Ignored
  dplyr::select(x, y, dom_tag) %>%
  dplyr::mutate(dom_tag=case_when(dom_tag%in%"inverts.fit"~"inverts",
                                  dom_tag%in%"sand.fit"~"sand",
                                  dom_tag%in%"macroalg.fit"~"macroalgae",
                                  dom_tag%in%"rock.fit"~"rock",
                                  dom_tag%in%"seagrasses.fit"~"seagrasses"))
unique(max.dat$observed)
                unique(predhabdf$dom_tag)

roc.dat <- cbind(max.dat, terra::extract(testrast, max.datv)) %>%
  dplyr::select(-c(block)) %>%
  pivot_longer(cols = c(starts_with(c("1_", "2_", "3_", "4_", "5_"))), 
               names_to = "block", values_to = "predicted") %>%
  separate(block, into = c("block", "taxa"), sep = "_") %>%
  pivot_wider(names_from = "taxa", values_from = "predicted") %>%
  dplyr::filter(!is.na(sand)) %>% 
  glimpse()

roc.dat$predicted <- apply(roc.dat %>% dplyr::select(sand, inverts, macroalgae, rock, seagrasses), 1, 
                           FUN = function(x){names(which.max(x))})
unique(roc.dat$predicted)


# Loop through each spatial block
# Generate kappa and auc
for (i in 1:5) {
  test.dat <- roc.dat %>% 
    dplyr::filter(block == blocks[i]) %>%
    dplyr::mutate(observed = as.factor(observed),
                  predicted = as.factor(predicted))
  levels(test.dat$predicted) <- levels(test.dat$observed)
  
  test.matrix <- as.matrix(test.dat %>% 
                             dplyr::select(sand, rock, macroalgae, inverts, seagrasses))
  conf = confusionMatrix(test.dat$predicted, test.dat$observed)
  kapp = conf$overall[1]
  
  roc.hab <- multiclass.roc(response = test.dat$observed, predictor = test.matrix) # Second should be numeric - predicted value
  
  if (i == 1) {
    kappa.dat <- data.frame(kappa = kapp,
                            auc = roc.hab$auc) %>%
      dplyr::mutate(block = blocks[i])
  }
  else {
    kappa.dat <- data.frame(kappa = kapp,
                            auc = roc.hab$auc) %>%
      dplyr::mutate(block = blocks[i]) %>%
      rbind(kappa.dat) 
  }
  
}

kappa.dat <- kappa.dat %>%
  dplyr::mutate(block = as.character(block),
                auc = as.numeric(auc)) %>%
  add_row(block = "mean",
          kappa = mean(.$kappa),
          auc = mean(.$auc))

write.csv(kappa.dat, paste0("output/crossval/", name, "_kappa-auc.csv"),
          row.names = F)
