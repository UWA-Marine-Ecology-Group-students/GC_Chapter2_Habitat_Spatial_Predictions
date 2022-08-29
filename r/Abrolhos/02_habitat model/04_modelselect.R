###
# Project: ** Add here **
# Data:    BRUVS, BOSS Habitat data
# Task:    Model selection 
# Author:  Kingsley Griffin & Claude Spencer from @beckyfisher/FSSgam
# Date:    ** Add here **

# This script runs the FSS GAM model selection for broad habitat classes
# This has been adapted from https://github.com/beckyfisher/FSSgam

# Clear your environment
rm(list=ls())

# Load libraries
library(tidyr)
library(dplyr)
options(dplyr.width = Inf)                                                      # Enables head() to display all coloums
library(mgcv)
library(MuMIn)
library(car)
library(doBy)
library(gplots)
library(doSNOW)
library(gamm4)
library(RCurl)                                                                  # Needed to download data from GitHub
library(corrr)

# Install and load fssgam package----
# devtools::install_github("beckyfisher/FSSgam_package")                        # Run once
library(FSSgam)

# Set your study name
name <- "Abrolhos"                                                              # Change here

# Bring in and format the data----
dat <- readRDS(paste(paste0('data/tidy/', name),                                # Merged data from 'R/01_mergedata.R'
                     'habitat-bathy-derivatives.rds', sep = "_")) %>% 
  dplyr::select(sample, longitude, latitude, depth,                             # Select columns to keep
                tri, tpi, roughness, slope, aspect, detrended,                  # Bathymetry derivatives
                broad.total.points.annotated, kelps, rock, macroalgae, sand, inverts) %>% # Points annotated and habitat scores
  pivot_longer(cols = c("kelps", "rock", "macroalgae", "sand", "inverts"),      # Set your response columns here 
               names_to = "response", values_to = "number") %>%                 # Pivot habitat columns to long format for modelling
  glimpse()

# Set predictor variables---
pred.vars <- c("depth","tri", "tpi", "roughness", "slope", "aspect", "detrended") 

# Check for correlation of predictor variables- remove anything highly correlated (>0.95)---
# Full correlation table
round(cor(dat[ , pred.vars]), 2)

# Just correlations greater than your cutoff
correlate(dat[,pred.vars], use = "complete.obs") %>%  
  gather(-term, key = "colname", value = "cor") %>% 
  dplyr::filter(abs(cor) > 0.8) %>%                                             # Set your cutoff here
  dplyr::filter(row_number() %% 2 == 1)                                         # Removes duplicated rows

# Review of individual predictors for even distribution---
# Plot of likely transformations - Anna Cresswell loop
par(mfrow = c(3, 2))
for (i in pred.vars) {
  x<-dat[ , i]
  x = as.numeric(unlist(x))
  hist((x))
  plot((x), main = paste(i))
  hist(sqrt(x))
  plot(sqrt(x))
  hist(log(x + 1))
  plot(log(x + 1))
}

# # Re-set the predictors for modeling----
pred.vars <- c("depth","roughness", "detrended") 

# Check to make sure Response vector has no more than 80% zeros----
unique.vars = unique(as.character(dat$response))
# unique.vars.use=character()
# for(i in 1:length(unique.vars)){
#   temp.dat=dat[which(dat$response==unique.vars[i]),]
#   if(length(which(temp.dat$number==0))/nrow(temp.dat)<0.8){                     # This removes kelp for Abrolhos!
#     unique.vars.use=c(unique.vars.use,unique.vars[i])}
# }
# unique.vars.use
unique.vars.use = unique.vars
   
# Run the full subset model selection----
savedir   <- "output/fssgam - habitat"
resp.vars <- unique.vars.use
use.dat   <- as.data.frame(dat[dat$response %in% c(unique.vars.use), ])
str(use.dat)

# factor.vars=c()                                                               # Set factor variables here
out.all <- list()
var.imp <- list()

# Loop through the FSS function for each Taxa----
for(i in 1:length(resp.vars)){
  use.dat <- as.data.frame(dat[which(dat$response == resp.vars[i]), ])
  use.dat <- use.dat[!(use.dat$broad.total.points.annotated - use.dat$number < 0), ] # added to fix weird point
  # use.dat$Site <- as.factor(use.dat$Site)
  Model1  <- gam(cbind(number, (broad.total.points.annotated - number)) ~ 
                   s(depth, bs = 'cr'),
                 family = binomial("logit"),  data = use.dat)
  
  model.set <- generate.model.set(use.dat = use.dat,
                                  test.fit = Model1,
                                  pred.vars.cont = pred.vars,
                                  # pred.vars.fact=factor.vars,
                                  # linear.vars="Distance",
                                  # cyclic.vars = c("aspect"),
                                  k = 5,
                                  cov.cutoff = 0.7
                                  # null.terms = "s(Site, bs='re')"
  )
  out.list <- fit.model.set(model.set,
                            max.models = 600,
                            parallel = T)
  names(out.list)
  
  out.list$failed.models # examine the list of failed models
  mod.table <- out.list$mod.data.out  # look at the model selection table
  mod.table <- mod.table[order(mod.table$AICc), ]
  mod.table$cumsum.wi <- cumsum(mod.table$wi.AICc)
  out.i     <- mod.table[which(mod.table$delta.AICc <= 2), ]
  out.all   <- c(out.all, list(out.i))
  var.imp   <- c(var.imp, list(out.list$variable.importance$aic$variable.weights.raw))
  
  # plot the best models
  for(m in 1:nrow(out.i)){
    best.model.name <- as.character(out.i$modname[m])
    
    png(file = paste(savedir, paste(m, resp.vars[i], "mod_fits.png", sep = ""), sep = "/"))
    if(best.model.name != "null"){
      par(mfrow = c(3, 1), mar = c(9, 4, 3, 1))
      best.model = out.list$success.models[[best.model.name]]
      plot(best.model, all.terms = T, pages = 1, residuals = T, pch = 16)
      mtext(side = 2, text = resp.vars[i], outer = F)}  
    dev.off()
  }
}

# Model fits and importance---
names(out.all) <- resp.vars
names(var.imp) <- resp.vars
all.mod.fits <- do.call("rbind", out.all)
all.var.imp  <- do.call("rbind", var.imp)
write.csv(all.mod.fits[ , -2], file = paste(savedir,paste(name, "all.mod.fits.csv", sep = "."), sep = "/"))
write.csv(all.var.imp,         file = paste(savedir, paste(name, "all.var.imp.csv", sep = "."), sep = "/"))
