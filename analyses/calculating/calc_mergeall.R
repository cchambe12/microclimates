### 24 January 2019 - Cat
## Fresh start to adding in climate data
# Trying to calculate chilling and forcing for budburst and leafout

## Updated 21 January 2019 with new TS data & 21 November 2018 for Climate data
### Weather data downloaded from... http://labs.arboretum.harvard.edu/weather/

## Data download: https://data.usanpn.org/observations/get-started
## Individual Phenometrics -> Set Date -> Set Partner Groups -> Output fields (ObservedBy Person ID, Multiple Observers)


# ## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Load Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(rstanarm)
library(geosphere)
library(anytime)
library(weathermetrics)
library(measurements)
library(lubridate)
library(egg)
library(chillR)


# Set Working Directory
setwd("~/Documents/git/microclimates/analyses")

d <- read.csv("output/clean_budburstandleafout.csv", header=TRUE)

# 1. Let's add in climate data first for forcing.
source("calculating/clean_addinclimate.R") ## takes a while to load all the data, brings in climate data
#write.csv(d, file="output/clean_addinclimate.csv", row.names=FALSE)

# 2. Let's add in Forcing data first. We will use February 15 as the start
# of calculating GDD. Easy to fix if necessary in the gdd.start column
source("calculating/calc_forceBB.R") 


# 3. Now let's add in forcing from budburst to leafout!! 
source("calculating/calc_forceBB.R") 
