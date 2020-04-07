### 24 January 2019 - Cat
## Fresh start to adding in climate data
# Trying to calculate chilling and forcing for budburst and leafout

### Weather data for the Arboretum downloaded from... http://labs.arboretum.harvard.edu/weather/ 
##  not using hobo loggers

### Weather data for Harvard Forests downloaded from... http://harvardforest.fas.harvard.edu:8080/exist/apps/datasets/showData.html?id=hf001 
## and select 'hf001-10' - not using hobo loggers. Takes a while to download.

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

## Flags for question
use.hobos <- FALSE ## make false if want to use main station climate data rather than the hobo loggers

### For #1, must choose whether you want hobo logger data or main climate towers

if(use.hobos==FALSE){
# 1a. Let's add in climate data first for forcing.
source("calculating/clean_addinclimate.R") ## takes a while to load all the data, brings in climate data
#write.csv(cc, file="output/clean_addinclimate.csv", row.names=FALSE)
}

if(use.hobos==TRUE){
# 1b. Let's add in climate data from each hobo logger.
source("calculating/clean_addinclimate_loggers.R") ## takes a while to load all the data, brings in climate data
#write.csv(cc, file="output/clean_addinclimate.csv", row.names=FALSE)
}

# 2. Let's add in Forcing data first. We will use February 15 as the start
# of calculating GDD. Easy to fix if necessary in the gdd.start column
if(use.hobos==TRUE){
## Need to reset the working directory if use.hobos==TRUE.
setwd("~/Documents/git/microclimates/analyses")
}
source("calculating/calc_forceBB.R") ### This part can take a while depending on how many years of data you have and how many loggers

# 2. Let's add in Forcing data for leafoutnow. We will again use February 15 as the start
# of calculating GDD. Easy to fix if necessary in the gdd.start column
source("calculating/calc_forceLO.R") ### This part can take a while depending on how many years of data you have and how many loggers

# 3. Now let's add in forcing from budburst to leafout!! And also add in false spring information
source("calculating/calc_forceDVR.R") ### This part can take a while depending on how many years of data you have and how many loggers


# 4. Let's add in Chilling data for budburst now. We will use February 15 as the end
# of calculating chill and start with last observation from prev season. Easy to fix if necessary in the chill.startthis column
source("calculating/calc_chillports.R") ### This part can take a while depending on how many years of data you have and how many loggers

if(use.hobos==FALSE){
  # 4. Let's add in tmean for the growing season for each individual - does climate play a roll on growing season length?
  source("calculating/calc_gstmean.R")
}
if(use.hobos==FALSE){
  # 5. Finally, let's add in precip for the growing season for each individual - does precip play a roll on growing season length?
  source("calculating/calc_precip.R")
}

if(use.hobos==FALSE){
## If using 1a)...
write.csv(gdd.stan, file="output/clean_gdd_chill_bbanddvr.csv", row.names = FALSE)
}

if(use.hobos==TRUE){
## If using 1b)...
write.csv(gdd.stan, file="output/clean_gdd_chill_bbanddvr_hobo.csv", row.names = FALSE)
}
