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
library(RColorBrewer)
library(dplyr)

## Let's load some real data to check out.
setwd("~/Documents/git/microclimates/analyses/")

figpath <- "figures"
figpathmore <- "sims_sampfreq"

source("source/sampfreq_muplot.R")

# Set up colors
cols <- adjustcolor("indianred3", alpha.f = 0.3) 
my.pal <- rep(brewer.pal(n = 8, name = "Dark2"), 4)
my.pal <- my.pal[my.pal!=c("#FFFFBF", "#FEE08B", "#E6F598")]
# display.brewer.all()
my.pch <- rep(15:18, each=10)
alphahere = 0.4

unique(samptest$species) # numbers are alphabetical
sort(unique(samptest$species))

# m1.bb <- m2l.ni
modelhere <- sampfreq.mod
muplotfx(modelhere, "", 7, 8, c(0,2), c(-20, 40) , 42, 2)