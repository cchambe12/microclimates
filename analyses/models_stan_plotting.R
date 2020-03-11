### Started 30 January 2020 by Cat
## Let's try and build a model in stan!!

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#### Questions to address:
## Compare GDDs between hobo loggers and weather station data
# 1) GDDlo ~ 1 + (1|species) - do once for HF weather station, once for hobo logger and repeat for Arboretum
# Compare urban effect using weather station data and then hobo logger data
# 2) GDDlo ~ urban + (urban|species) - do once with weather station data and once with hobo logger data

## Let's start with Question 1 first...
library(bayesplot) ## for plotting
library(egg) ## for plotting
library(shinystan)
library(rstanarm)
library(rstan)
library(brms)

## Let's load some real data to check out.
setwd("~/Documents/git/microclimates/analyses/")

figpath <- "figures"
figpathmore <- "hobo_urb"

source("source/microurban_muplot.R")

# Set up colors
cols <- adjustcolor("indianred3", alpha.f = 0.3) 
my.pal <- rep(brewer.pal(n = 12, name = "Paired"), 4)
# display.brewer.all()
my.pch <- rep(15:18, each=12)
alphahere = 0.4


if(figpathmore=="ws_urb"){
ws_urb <- read.csv("output/clean_gdd_bbanddvr.csv")

ws_urb$urban <- NA
ws_urb$urban <- ifelse(ws_urb$type=="Harvard Forest", 0, ws_urb$urban)
ws_urb$urban <- ifelse(ws_urb$type=="Treespotters", 1, ws_urb$urban)
#ws_urb <- ws_urb[(ws_urb$year>=2019),]

ws_urb.stan <- subset(ws_urb, select=c(gdd_bb, urban, genus, species))
ws_urb.stan <- ws_urb.stan[(complete.cases(ws_urb.stan)),]
ws_urb.stan$spp <- paste(ws_urb.stan$genus, ws_urb.stan$species, sep="_")

ws_urb.stan <- ws_urb.stan[(ws_urb.stan$gdd_bb<=1000),]
} else if(figpathmore=="hobo_urb"){
  hobo_urb <- read.csv("output/clean_gdd_bbanddvr_hobo.csv")
  
  hobo_urb$urban <- NA
  hobo_urb$urban <- ifelse(hobo_urb$type=="Harvard Forest", 0, hobo_urb$urban)
  hobo_urb$urban <- ifelse(hobo_urb$type=="Treespotters", 1, hobo_urb$urban)
  #ws_urb <- ws_urb[(ws_urb$year>=2019),]
  
  hobo_urb.stan <- subset(hobo_urb, select=c(gdd_bb, urban, genus, species))
  hobo_urb.stan <- hobo_urb.stan[(complete.cases(hobo_urb.stan)),]
  hobo_urb.stan$spp <- paste(hobo_urb.stan$genus, hobo_urb.stan$species, sep="_")
  hobo_urb.stan <- hobo_urb.stan[(hobo_urb.stan$gdd_bb<=1000),]
}


# Load fitted stan model: no interactions
load("stan/hobo_urban_mod.Rdata") 
#load("stan/ws_urban_mod.Rdata") 
#load("stan/wsall_urban_mod.Rdata")

sumer.ws <- summary(hobo_urb_mod)$summary
sumer.ws[grep("mu_", rownames(sumer.ws)),]

unique(hobo_urb.stan$spp) # numbers are alphabetical
sort(unique(hobo_urb.stan$spp))

# m1.bb <- m2l.ni
modelhere <- hobo_urb_mod
muplotfx(modelhere, "", 7, 8, c(0,2), c(-400, 700) , 750, 2)


