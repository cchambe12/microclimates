### Started 9 March 2020 by Cat
## New fake data and prior predictive checks
## Based off of Rethinking Chapter 4
## Using workflow from https://rpubs.com/adrbart/random_slope_simulation

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#### Questions to address:
## Compare GDDs between hobo loggers and weather station data
# GDDlo ~ urban + (urban|species) - do once with weather station data and once with hobo logger data

## Let's start with Question 1 first...
#library(rethinking)
library(RColorBrewer)
library(lme4)

## Let's load some real data to check out.
setwd("~/Documents/git/microclimates/analyses/")

ws <- read.csv("output/clean_gdd_chill_bbanddvr.csv")
hobo <- read.csv("output/clean_gdd_chill_bbanddvr_hobo.csv")

# We will focus on Harvard Forest to start
hf.ws <- ws[(ws$type=="Harvard Forest"),]
hf.hobo <- hobo[(hobo$type=="Harvard Forest"),]

mean(hf.ws$gdd_lo, na.rm=TRUE) ## 877.41
mean(hf.hobo$gdd_lo, na.rm=TRUE) ## 541.2
## Big difference between the two means!! Without looking at the data too much, I will assume the hobo logger data
# has less variance than the weather station data

set.seed(12221)

####################### NOW LET'S TRY TO ADDRESS QUESTION #2
# Compare urban effect using weather station data and then hobo logger data
# 2) GDDlo ~ urban + (urban|species) - do once with weather station data and once with hobo logger data

### Okay, now let's make some fake data using help Rethinking, Gelman, OSPREE and Geoff
#  1) Let's make the observations much higher than the actual data to build a good model.
nsp = 20 # number of species
ntot = 200 # numbers of obs per species. 

sample_a <- list(site.env = rbinom(1000, 1, 0.5))

model.parameters <- list(intercept = 400,
                         urban.coef = -150)

#  2) Now, we will make varying intercepts
env.samples <- sapply(sample_a, FUN = function(x){
  sample(x, size = nsp * ntot, replace = TRUE)})
mm <- model.matrix(~env.samples)
#mm <- mm[,-2]

#  4) We need to make a random intercept model for each species
parameters.temp <- matrix(unlist(model.parameters), ncol = length(model.parameters), nrow = nsp * ntot, byrow = TRUE)
# Which parameters are random?
random.regex <- grep(pattern = paste(c("intercept", "urban.coef"), collapse = "|"), x = names(model.parameters))
# Generate random parameters (by species)

parameters.temp[, 1] <- sapply(1:nsp, FUN = function(x){
  rep(rnorm(n = 1, mean = model.parameters[[random.regex[1]]], sd = 100), ntot)})
parameters.temp[, 2] <- sapply(1:nsp, FUN = function(x){
  rep(rnorm(n = 1, mean = model.parameters[[random.regex[2]]], sd = 50), ntot)})
# Calculate response
response <- sapply(1:nrow(env.samples), FUN = function(x){
  rnorm(n = 1, mean = mm[x, ] %*% parameters.temp[x, ], sd = 30)})

fakedata_ws_urb <- cbind(data.frame(species = as.vector(sapply(1:nsp, FUN = function(x) rep(x, ntot))),
                                    gdd = response, urban = env.samples[,1]))

write.csv(fakedata_ws_urb, file="output/fakedata_ws_urb.csv", row.names = FALSE)

#  7) Let's do a quick lmer model to test the fake data
modtest <- lmer(gdd ~ urban + (urban|species), data=fakedata_ws_urb) ## Quick look looks good!

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### Okay, now for hobo logger model ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

#  1) Let's make the observations much higher than the actual data to build a good model.
nsp = 20 # number of species
ntot = 200 # numbers of obs per species. 

sample_a <- list(site.env = rbinom(1000, 1, 0.5))

model.parameters <- list(intercept = 300,
                         urban.coef = -100)

#  2) Now, we will make varying intercepts
env.samples <- sapply(sample_a, FUN = function(x){
  sample(x, size = nsp * ntot, replace = TRUE)})
mm <- model.matrix(~env.samples)
#mm <- mm[,-2]

#  4) We need to make a random intercept model for each species
parameters.temp <- matrix(unlist(model.parameters), ncol = length(model.parameters), nrow = nsp * ntot, byrow = TRUE)
# Which parameters are random?
random.regex <- grep(pattern = paste(c("intercept", "urban.coef"), collapse = "|"), x = names(model.parameters))
# Generate random parameters (by species)

parameters.temp[, 1] <- sapply(1:nsp, FUN = function(x){
  rep(rnorm(n = 1, mean = model.parameters[[random.regex[1]]], sd = 75), ntot)})
parameters.temp[, 2] <- sapply(1:nsp, FUN = function(x){
  rep(rnorm(n = 1, mean = model.parameters[[random.regex[2]]], sd = 35), ntot)})
# Calculate response
response <- sapply(1:nrow(env.samples), FUN = function(x){
  rnorm(n = 1, mean = mm[x, ] %*% parameters.temp[x, ], sd = 30)})

fakedata_hl_urb <- cbind(data.frame(species = as.vector(sapply(1:nsp, FUN = function(x) rep(x, ntot))),
                                    gdd = response, urban = env.samples[,1]))

write.csv(fakedata_hl_urb, file="output/fakedata_hl_urb.csv", row.names = FALSE)

#  7) Let's do a quick lmer model to test the fake data
modtesthl <- lmer(gdd ~ urban + (urban|species), data=fakedata_hl_urb) ## Quick look looks good!


#################################################################################################
#################################    Now for CHILLING!   ########################################
#################################################################################################
mean(hf.ws$utah, na.rm=TRUE) ## 675.131
mean(hf.hobo$utah, na.rm=TRUE) ## 1036.485
## Big difference between the two means!! Without looking at the data too much, I will assume the hobo logger data
# has less variance than the weather station data

set.seed(12221)

####################### NOW LET'S TRY TO ADDRESS QUESTION #2
# Compare urban effect using weather station data and then hobo logger data
# 2) GDDlo ~ urban + (urban|species) - do once with weather station data and once with hobo logger data

### Okay, now let's make some fake data using help Rethinking, Gelman, OSPREE and Geoff
#  1) Let's make the observations much higher than the actual data to build a good model.
nsp = 20 # number of species
ntot = 200 # numbers of obs per species. 

sample_a <- list(site.env = rbinom(1000, 1, 0.5))

model.parameters <- list(intercept = 850,
                         urban.coef = -150)

#  2) Now, we will make varying intercepts
env.samples <- sapply(sample_a, FUN = function(x){
  sample(x, size = nsp * ntot, replace = TRUE)})
mm <- model.matrix(~env.samples)
#mm <- mm[,-2]

#  4) We need to make a random intercept model for each species
parameters.temp <- matrix(unlist(model.parameters), ncol = length(model.parameters), nrow = nsp * ntot, byrow = TRUE)
# Which parameters are random?
random.regex <- grep(pattern = paste(c("intercept", "urban.coef"), collapse = "|"), x = names(model.parameters))
# Generate random parameters (by species)

parameters.temp[, 1] <- sapply(1:nsp, FUN = function(x){
  rep(rnorm(n = 1, mean = model.parameters[[random.regex[1]]], sd = 100), ntot)})
parameters.temp[, 2] <- sapply(1:nsp, FUN = function(x){
  rep(rnorm(n = 1, mean = model.parameters[[random.regex[2]]], sd = 50), ntot)})
# Calculate response
response <- sapply(1:nrow(env.samples), FUN = function(x){
  rnorm(n = 1, mean = mm[x, ] %*% parameters.temp[x, ], sd = 30)})

fakedata_ws_urb_chill <- cbind(data.frame(species = as.vector(sapply(1:nsp, FUN = function(x) rep(x, ntot))),
                                    utah = response, urban = env.samples[,1]))

write.csv(fakedata_ws_urb_chill, file="output/fakedata_ws_urb_chill.csv", row.names = FALSE)

#  7) Let's do a quick lmer model to test the fake data
modtest <- lmer(utah ~ urban + (urban|species), data=fakedata_ws_urb_chill) ## Quick look looks good!

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### Okay, now for hobo logger model ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

#  1) Let's make the observations much higher than the actual data to build a good model.
nsp = 20 # number of species
ntot = 200 # numbers of obs per species. 

sample_a <- list(site.env = rbinom(1000, 1, 0.5))

model.parameters <- list(intercept = 1050,
                         urban.coef = -200)

#  2) Now, we will make varying intercepts
env.samples <- sapply(sample_a, FUN = function(x){
  sample(x, size = nsp * ntot, replace = TRUE)})
mm <- model.matrix(~env.samples)
#mm <- mm[,-2]

#  4) We need to make a random intercept model for each species
parameters.temp <- matrix(unlist(model.parameters), ncol = length(model.parameters), nrow = nsp * ntot, byrow = TRUE)
# Which parameters are random?
random.regex <- grep(pattern = paste(c("intercept", "urban.coef"), collapse = "|"), x = names(model.parameters))
# Generate random parameters (by species)

parameters.temp[, 1] <- sapply(1:nsp, FUN = function(x){
  rep(rnorm(n = 1, mean = model.parameters[[random.regex[1]]], sd = 150), ntot)})
parameters.temp[, 2] <- sapply(1:nsp, FUN = function(x){
  rep(rnorm(n = 1, mean = model.parameters[[random.regex[2]]], sd = 75), ntot)})
# Calculate response
response <- sapply(1:nrow(env.samples), FUN = function(x){
  rnorm(n = 1, mean = mm[x, ] %*% parameters.temp[x, ], sd = 30)})

fakedata_hl_urb_chill <- cbind(data.frame(species = as.vector(sapply(1:nsp, FUN = function(x) rep(x, ntot))),
                                    utah = response, urban = env.samples[,1]))

write.csv(fakedata_hl_urb_chill, file="output/fakedata_hl_urb_chill.csv", row.names = FALSE)

#  7) Let's do a quick lmer model to test the fake data
modtesthl <- lmer(utah ~ urban + (urban|species), data=fakedata_hl_urb_chill) ## Quick look looks good!
