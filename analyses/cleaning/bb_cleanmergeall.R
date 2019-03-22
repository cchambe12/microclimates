### 24 January 2019 - Cat
## Fresh start to cleaning phenology data
# Then more scripts, to calculate chilling and forcing

## Updated 21 January 2019 with new TS data & 21 November 2018 for Climate data
### Weld Hill weather data downloaded from... http://labs.arboretum.harvard.edu/weather/

## Tree Spotters Data download: https://data.usanpn.org/observations/get-started
## Individual Phenometrics -> Set Date -> Set Partner Groups -> Output fields (ObservedBy Person ID, Multiple Observers)
## * FOR MORE DETAILS SEE clean_TS.R

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
source("cleaning/clean_CG.R") # 24 Jan 2019: 433 ### NEED TO UPDATE THE LEAF DROP INFORMATION!!!!!

# 5. Get John O'Keefe data - and clean!
ok15<-read.csv("output/okeefe2015.csv", header=TRUE)
source("cleaning/HF_datareadin.R")

# 6. Clean John O'Keefe's data for budburst and leafout data
source("cleaning/clean_HF.R") # 24 Jan 2019: 662

write.csv(d, file="output/clean_budburstandleafout.csv", row.names=FALSE)





