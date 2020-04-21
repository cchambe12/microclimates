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

use.sims = FALSE
use.hobo = FALSE ### We expect less species variation using weather station data, so if use.hobo=TRUE, then sigma will be loaded on overall error not on species
use.urban = TRUE
use.provenance = FALSE

if(use.urban==TRUE & use.provenance==TRUE){
  print("Error was made in flags!! Adjust accordingly!")
}

if(use.provenance==FALSE & use.urban==FALSE){
  print("Error was made in flags!! Adjust accordingly!")
}

if (use.sims==TRUE & use.hobo==FALSE & use.urban==TRUE & use.provenance==FALSE){
  
  load("stan/ws_urban_stan_sims.Rdata")
  
  figpathmore <- "ws_urb_sims"
  source("source/sigmasims_urban_muplot.R")
  
  df <- read.csv("output/fakedata_ws_urb.csv")  ### Make sure species column is called `species`
  
  y2 = 5
  
  modelhere <- ws_urb_fake
}

if (use.sims==TRUE & use.hobo==TRUE & use.urban==TRUE & use.provenance==FALSE){
  
  load("stan/hl_urban_stan_sims.Rdata")
  
  figpathmore <- "hl_urb_sims"
  source("source/sigmasims_urban_muplot.R")
  
  df <- read.csv("output/fakedata_hl_urb.csv")
  
  y2 = 5
  
  modelhere <- hl_urb_fake
}

if (use.sims==FALSE & use.hobo==FALSE & use.urban==TRUE & use.provenance==FALSE){
  
  load("stan/ws_urban_stan.Rdata")
  
  figpathmore <- "ws_urb"
  source("source/sigmasims_urban_muplot.R")
  
  ws <- read.csv("output/clean_gdd_chill_bbanddvr.csv")
  
  ws <- ws[!(ws$type=="Common Garden"),]
  
  ws$urban <- ifelse(ws$type=="Harvard Forest", 0, 1)
  
  ws$spp <- paste(ws$genus, ws$species, sep="_")
  
  gdd.stan <- subset(ws, select=c(gdd_bb, urban, spp))
  gdd.stan <- gdd.stan[complete.cases(gdd.stan),]
  
  gdd.stan <- gdd.stan[!duplicated(gdd.stan),]
  
  df <- gdd.stan
  df$species <- df$spp
  
  y2 = 5
  
  modelhere <- ws_urb
}

if (use.sims==FALSE & use.hobo==TRUE & use.urban==TRUE & use.provenance==FALSE){
  
  load("stan/hl_urban_stan.Rdata")
  
  figpathmore <- "hl_urb"
  source("source/sigmasims_urban_muplot.R")
  
  hl <- read.csv("output/clean_gdd_chill_bbanddvr_hobo.csv")
  
  hl <- hl[!(hl$type=="Common Garden"),]
  
  hl$urban <- ifelse(hl$type=="Harvard Forest", 0, 1)
  
  hl$spp <- paste(hl$genus, hl$species, sep="_")
  
  gdd.stan <- subset(hl, select=c(gdd_bb, urban, spp))
  gdd.stan <- gdd.stan[complete.cases(gdd.stan),]
  
  gdd.stan <- gdd.stan[!duplicated(gdd.stan),]
  
  df <- gdd.stan
  df$species <- df$spp
  
  y2 = 5
  
  modelhere <- hl_urb
}

if (use.sims==TRUE & use.hobo==FALSE & use.urban==FALSE & use.provenance==TRUE){
  
  load("stan/ws_prov_stan_sims.Rdata")
  
  figpathmore <- "ws_prov_sims"
  source("source/sigmasims_prov_muplot.R")
  
  df <- read.csv("output/fakedata_ws_prov.csv")
  
  y2 = 5
  
  modelhere <- ws_prov_fake
}

if (use.sims==TRUE & use.hobo==TRUE & use.urban==FALSE & use.provenance==TRUE){
  
  load("stan/hl_prov_stan_sims.Rdata")
  
  figpathmore <- "hl_prov_sims"
  source("source/sigmasims_prov_muplot.R")
  
  df <- read.csv("output/fakedata_hl_prov.csv")
  
  y2 = 5
  
  modelhere <- hl_prov_fake
}

if (use.sims==FALSE & use.hobo==FALSE & use.urban==FALSE & use.provenance==TRUE){
  
  load("stan/ws_prov_stan.Rdata")
  
  figpathmore <- "ws_prov"
  source("source/sigmasims_prov_muplot.R")
  
  ws <- read.csv("output/clean_gdd_chill_bbanddvr_hobo.csv")
  
  ws <- ws[!(ws$type=="Common Garden"),]
  
  ws$provenance <- as.numeric(ws$provenance.lat)
  
  ws$spp <- paste(ws$genus, ws$species, sep="_")
  
  gdd.stan <- subset(ws, select=c(gdd_bb, provenance, spp))
  gdd.stan <- gdd.stan[complete.cases(gdd.stan),]
  
  gdd.stan <- gdd.stan[!duplicated(gdd.stan),]
  
  df <- gdd.stan
  df$species <- df$spp
  
  y2 = 5
  
  modelhere <- ws_prov
}



if (use.sims==FALSE & use.hobo==TRUE & use.urban==FALSE & use.provenance==TRUE){
  
  load("stan/hl_prov_stan.Rdata")
  
  figpathmore <- "hl_prov"
  source("source/sigmasims_prov_muplot.R")
  
  hl <- read.csv("output/clean_gdd_chill_bbanddvr_hobo.csv")
  
  hl <- hl[!(hl$type=="Common Garden"),]
  
  hl$provenance <- as.numeric(hl$provenance.lat)
  
  hl$spp <- paste(hl$genus, hl$species, sep="_")
  
  gdd.stan <- subset(hl, select=c(gdd_bb, provenance, spp))
  gdd.stan <- gdd.stan[complete.cases(gdd.stan),]
  
  gdd.stan <- gdd.stan[!duplicated(gdd.stan),]
  
  df <- gdd.stan
  df$species <- df$spp
  
  y2 = 5
  
  modelhere <- hl_prov
}

# Set up colors
cols <- adjustcolor("indianred3", alpha.f = 0.3) 
my.pal <- rep(brewer.pal(n = 11, name = "Spectral"), 4)
my.pal <- my.pal[my.pal!=c("#FFFFBF", "#FEE08B", "#E6F598")] ## removing light colors that are hard to see
my.pch <- rep(15:18, each=10)
alphahere = 0.4


muplotfx(modelhere, "", 7, 8, c(0,y2), c(-400, 700) , 750, y2)
