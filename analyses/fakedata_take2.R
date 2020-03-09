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

ws <- read.csv("output/clean_gdd_bbanddvr.csv")
hobo <- read.csv("output/clean_gdd_bbanddvr_hobo.csv")

# We will focus on Harvard Forest to start
hf.ws <- ws[(ws$type=="Harvard Forest"),]
hf.hobo <- hobo[(hobo$type=="Harvard Forest"),]

mean(hf.ws$gdd_lo, na.rm=TRUE) ## 877.41
mean(hf.hobo$gdd_lo, na.rm=TRUE) ## 541.2
## Big difference between the two means!! Without looking at the data too much, I will assume the hobo logger data
# has less variance than the weather station data

### WEATHER STATION DATA MODEL SIMULATION
### Okay, now let's make some fake data using help Rethinking, Gelman, OSPREE and Geoff
#  1) Let's make the observations much higher than the actual data to build a good model.
nsp = 20 # number of species
ntot = 500 # numbers of obs per species. 

form<-as.formula(c("~urban+(urban|species)"))

set.seed(12221)
simdat <-data.frame(species=factor(rep(1:nsp,each=ntot)),urban=runif(nsp*ntot,0,1))

beta<-c(400,-150)
names(beta)<-c("(Intercept)","urban")

int.var <- 75
urb.var <- 50
err <- 10
inturb.cov <- 0.5*(sqrt(int.var*urb.var))

vcov<-matrix(c(int.var,inturb.cov,inturb.cov,urb.var), 2, 2)

theta<-c((chol(vcov)/sqrt(err))[1,1],
         (chol(vcov)/sqrt(err))[1,2],
         (chol(vcov)/sqrt(err))[2,2])
names(theta)<-c("species.(Intercept)","species.urban.(Intercept)","species.urban")

set.seed(12221)
gdd<-simulate(form,newdata=simdat,family=gaussian,
                   newparams=list(theta = theta,beta = beta, sigma = sqrt(err)))
simdat$gdd<-as.vector(gdd[,1])

model1<-lmer(gdd~urban+(urban|species),data=simdat)
summary(model1)

#### Okay, now let's compare this to DF's and EMW's method...
nsp = 20 # number of species
ntot = 500 # numbers of obs per species. 

species = as.numeric(as.factor(rep(1:nsp,each=ntot)))

#  with species  (note to self: This is not the best, better to draw from a distribution)
intermean <- 400 # mean for selecting intercept (days to BB) across all species
intersd <- 100 # SD for selecting species intercepts
spint <- rnorm(nsp, intermean, intersd)  # different intercepts by species

# now start building ...
testdat2 <- vector()

# assumptions:
# (a) predictors are centered
# (b) predictors are not correlated
# (c) only 2-way interactions between  force + photo + chill
# (d) each obs is a different set of treatments

# and some important points ...
# (z) the below draws treatments from distribution in such a way that there is a lot more variation than we have


for(i in 1:nsp){ # loop over species. i = 1
  
  # continuous predictors, generate level (if you will) for each observation
  urban = rnorm(ntot, 0, 100)
  
  # set up effect sizes
  urbancoef = -150 
  
  # SD for each treatment
  urbancoef.sd = 50
  
  # build model matrix 
  mm <- model.matrix(~(urban)^2, data.frame(urban))
  
  # coefficients need to match the order of the colums in the model matrix (mm)
  # so here, that's intercept, chill, force, photo
  coeff <- c(spint[i], 
             rnorm(1, urbancoef, urbancoef.sd)
  )
  
  gdd <- rnorm(n = ntot, mean = mm %*% coeff, sd = 0.1)
  
  testdatx2 <- data.frame(gdd, sp = i, 
                          urban)
  
  testdat2 <- rbind(testdat2, testdatx2)  
}

summary(lmer(gdd ~ urban + (urban|sp), data = testdat2)) # sanity check








sample_a <- list(int.env = rnorm(1000, 700, 200))

#  2) Now, we will make varying intercepts
int.samples <- sapply(sample_a, FUN = function(x){
  sample(x, size = nsp * ntot, replace = TRUE)})

#  4) We need to make a random intercept model for each species
baseinter <- list(intercept = mean(int.samples))
baseinter.mat <- matrix(unlist(baseinter), ncol = length(baseinter), nrow = nsp * ntot, byrow = TRUE)
## From Geoff's simulate-linear.R code in OSPREE: Which parameters are random?
random.inter <- grep(pattern = paste("intercept", collapse = "|"), x = names(baseinter))
# Generate random intercepts (by species) 
for(i in 1:length(random.inter)){
  baseinter.mat[, i] <- sapply(1:nsp, FUN = function(X){
    rep(rnorm(n = 1, mean = baseinter[[random.inter[i]]], sd = 200), ntot)})}

#  5) Calculate response
response <- sapply(1:nrow(int.samples), FUN = function(x){
  rnorm(n = 1, mean = baseinter.mat[x, ], sd = 200)})
#  6) Make a dataframe of fake data
fakedata_ws <- cbind(data.frame(species = as.vector(sapply(1:nsp, FUN = function(x) rep(x, ntot))),
                                gdd = response))

write.csv(fakedata_ws, file="output/fakedata_ws.csv", row.names = FALSE)

#  7) Let's do a quick lmer model to test the fake data
modtest <- lmer(gdd ~ 1 + (1|species), data=fakedata_ws) ## Quick look looks good!