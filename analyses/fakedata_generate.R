### Started 29 January 2020 by Cat
## New fake data and prior predictive checks
## Based off of Rethinking Chapter 4

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#### Questions to address:
## Compare GDDs between hobo loggers and weather station data
# 1) GDDlo ~ 1 + (1|species) - do once for HF weather station, once for hobo logger and repeat for Arboretum
# Compare urban effect using weather station data and then hobo logger data
# 2) GDDlo ~ urban + (urban|species) - do once with weather station data and once with hobo logger data

## Let's start with Question 1 first...
#library(rethinking)

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
ntot = 50 # numbers of obs per species. 

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

#  7) Let's do a quick lmer model to test the fake data
library(lme4)
modtest <- lmer(gdd ~ 1 + (1|species), data=fakedata_ws) ## Quick look looks good!

library(RColorBrewer)
my.pal <- rep(brewer.pal(n = 10, name = "Paired"), 2)
my.pch <- rep(15:16, each=10)
plot(gdd ~ species, col=my.pal[species], pch=my.pch[species], data = fakedata_ws)
abline(h=mean(fakedata_ws$gdd), lwd=3)

plot(density(fakedata_ws$gdd))
abline(v = mean(fakedata_ws$gdd), lwd = 2, col = "blue")

## PRIOR PREDICTIVE CHECK time!!
# Now I will follow the workflow from the Gabry et al., 2019 paper
## Using vague priors
nsims <- length(fakedata_ws$species)
alpha <- rnorm(20, 700, 200)
sigma <- runif(20, 0, 200)

data1 <- data.frame(
  gdd = fakedata_ws$gdd,
  sim = alpha[fakedata_ws$species] +
    rnorm(nsims, mean = 0, sd = sigma)
)
## sample_a <- rnorm(n = 10000, mean = mean(d2$height), sd = 100) # assume intercept is centered on mean height
## sample_b <- rnorm(n = 10000, mean = 0, sd = 10) # slope centered on 0
## sample_sigma <- runif(n = 10000, min = 0, max = 50)
## sample_weight <- sample(x = seq(4, 48, by = 1), size = 10000, replace = TRUE) # generate some random weights
## sample_mu <- sample_a + (sample_b * sample_weight) # generate mu using linear model and weights
## sample_height <- rnorm(n = 10000, mean = sample_mu, sd = sample_sigma)
## plot(density(sample_height))
## abline(v = mean(d2$height), lwd = 2, col = "blue")


fakedata_ws$LL <- sapply( 1:nrow(fakedata_ws) , function(i) sum( dnorm(
  fakedata_ws$gdd ,
  mean=mean(fakedata_ws$gdd),
  sd=sd(fakedata_ws$gdd),
  log=FALSE ) ) )
fakedata_ws$prod <- fakedata_ws$LL + dnorm( mean(fakedata_ws$gdd) , 700 , 200 , FALSE ) +
  dunif( sd(fakedata_ws$gdd) , 0 , 200 , FALSE )
fakedata_ws$prob <- exp( fakedata_ws$prod - max(fakedata_ws$prod) )

