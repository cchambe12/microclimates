### 24 January 2019 - Cat
## Fresh start to cleaning phenology data
# Then more scripts, to calculate chilling and forcing

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


# Set Working Directory
setwd("~/Documents/git/microclimates/analyses")

# 1. Get treespotters data - and clean!
d<-read.csv("input/individual_phenometrics_data.csv", header=TRUE) ## 24 Jan 2019: 7671

# 2. Clean treespotters data first for budburst and leafout  
source("cleaning/clean_TS.R") # 24 Jan 2019: 171 

# 3. Get Common Garden data - and clean!
cg <- read.csv("output/dvr_cg_2018.csv", header=TRUE) ## from CGtraits repo https://github.com/cchambe12/CGtraits

# 4. Clean CG data for budburst and leafout
source("cleaning/clean_CG.R") # 24 Jan 2019: 433

# 5. Get John O'Keefe data - and clean!
ok15<-read.csv("output/okeefe2015.csv", header=TRUE)
ok15$year <- 2015
ok15$JULIAN <- as.integer(ok15$JULIAN)
ok15$CIRCUIT <- as.integer(ok15$CIRCUIT)
ok15$BBRK <- as.integer(ok15$BBRK)
ok15$FOPN <- as.integer(ok15$FOPN)
ok15$L75 <- as.integer(ok15$L75)
ok15$L95 <- as.integer(ok15$L95)
ok16<-read.csv("output/okeefe2016.csv", header=TRUE)
ok16$year <- 2016
jok <- full_join(ok15, ok16)
ok17<-read.csv("output/okeefe2017.csv", header=TRUE)
ok17$year <- 2017
jok <- full_join(jok, ok17)
ok18<-read.csv("output/okeefe2018.csv", header=TRUE)
ok18$year <- 2018
ok18$FPST <- as.character(ok18$FPST)
ok18$LFIN <- as.character(ok18$LFIN)
jok <- full_join(jok, ok18)

# 6. Clean John O'Keefe's data for budburst and leafout data
source("cleaning/clean_HF.R") # 24 Jan 2019: 433



