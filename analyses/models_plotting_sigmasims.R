### Started 17 April 2020 by Cat
## Plotting sigma simulations

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Let's start with Question 1 first...
library(bayesplot) ## for plotting
library(egg) ## for plotting
library(shinystan)
library(rstanarm)
library(rstan)
library(brms)
library(RColorBrewer)
library(dplyr)

## Let's load some real data to check out.
setwd("~/Documents/git/microclimates/analyses/")

figpath <- "figures"

use.hobo = FALSE ### We expect less species variation using weather station data, so if use.hobo=TRUE, then sigma will be loaded on overall error not on species
use.urban = TRUE
use.provenance = FALSE
use.highsitevariation = FALSE ## Not sure if I will use these but here just in case
use.highprovvariation = FALSE

if (use.hobo==FALSE & use.urban==TRUE & use.provenance==FALSE &
    use.highsitevariation==FALSE & use.highprovvariation==FALSE){
  
  load("stan/ws_urban_stan_sims.Rdata")
  
  figpathmore <- "ws_urb"
  source("source/sigmasims_urban_muplot.R")
  
  df <- read.csv("output/fakedata_ws_urb.csv")  ### Make sure species column is called `species`
  
  y2 = 5
  
  modelhere <- ws_urb_fake
}

if (use.hobo==TRUE & use.urban==TRUE & use.provenance==FALSE &
    use.highsitevariation==FALSE & use.highprovvariation==FALSE){
  
  load("stan/hl_urban_stan_sims.Rdata")
  
  figpathmore <- "hl_urb"
  source("source/sigmasims_urban_muplot.R")
  
  df <- read.csv("output/fakedata_hl_urb.csv")
  
  y2 = 5
  
  modelhere <- hl_urb_fake
}

if (use.hobo==FALSE & use.urban==TRUE & use.provenance==TRUE &
    use.highsitevariation==FALSE & use.highprovvariation==FALSE){
  
  load("stan/ws_urban_prov_stan_sims.Rdata")
  
  figpathmore <- "ws_urb_prov"
  source("source/sigmasims_urban_prov_muplot.R")
  
  df <- read.csv("output/fakedata_ws_urb_prov.csv")
  
  y2 = 7
  
  modelhere <- ws_urb_prov_fake
}

if (use.hobo==TRUE & use.urban==TRUE & use.provenance==TRUE &
    use.highsitevariation==FALSE & use.highprovvariation==FALSE){
  
  load("stan/hl_urban_prov_stan_sims.Rdata")
  
  figpathmore <- "hl_urb_prov"
  source("source/sigmasims_urban_prov_muplot.R")
  
  df <- read.csv("output/fakedata_hl_urb_prov.csv")
  
  y2 = 7
  
  modelhere <- hl_urb_prov_fake
}

# Set up colors
cols <- adjustcolor("indianred3", alpha.f = 0.3) 
my.pal <- rep(brewer.pal(n = 11, name = "Spectral"), 4)
my.pal <- my.pal[my.pal!=c("#FFFFBF", "#FEE08B", "#E6F598")] ## removing light colors that are hard to see
my.pch <- rep(15:18, each=10)
alphahere = 0.4


muplotfx(modelhere, "", 7, 8, c(0,y2), c(-400, 700) , 750, y2)
