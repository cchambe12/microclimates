### 24 January 2019 - Cat
## Fresh start to adding in climate data
# Trying to calculate chilling and forcing for budburst and leafout

## Updated 15 July 2019 with new TS data & 15 July 2019 for Climate data
### Weather data downloaded from... http://labs.arboretum.harvard.edu/weather/

## Data download: https://data.usanpn.org/observations/get-started
## Individual Phenometrics -> Set Date -> Set Partner Groups -> Output fields (ObservedBy Person ID, Multiple Observers)


# ## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Load Libraries
library(dplyr)
library(tidyr)
library(geosphere)
library(anytime)
library(weathermetrics)
library(measurements)
library(lubridate)
library(chillR)


# Set Working Directory
setwd("~/Documents/git/microclimates/analyses")

d <- read.csv("output/clean_budburstandleafout.csv", header=TRUE)

### For #1, must choose whether you want hobo logger data or main climate towers

# 1a. Let's add in climate data first for forcing.
#source("calculating/clean_addinclimate.R") ## takes a while to load all the data, brings in climate data
#write.csv(cc, file="output/clean_addinclimate.csv", row.names=FALSE)

# 1b. Let's add in climate data from each hobo logger.
#source("calculating/clean_addinclimate_loggers.R") ## takes a while to load all the data, brings in climate data
#write.csv(cc, file="output/clean_addinclimate.csv", row.names=FALSE)

# 2. Let's add in Forcing data first. We will use February 15 as the start
# of calculating GDD. Easy to fix if necessary in the gdd.start column
source("calculating/calc_forceBB.R") 


# 3. Now let's add in forcing from budburst to leafout!! And also add in false spring information
source("calculating/calc_forceDVR.R") 

## If using 1a)...
#write.csv(gdd.stan, file="output/clean_gdd_bbanddvr.csv", row.names = FALSE)

## If using 1b)...
#write.csv(gdd.stan, file="output/clean_gdd_bbanddvr_hobo.csv", row.names = FALSE)
