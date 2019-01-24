### 24 January 2019 - Cat
## Fresh start to cleaning phenology data
# Then in another script, to calculate forcing and chilling

## Find Provenance Lat info and Age!
## Updated 21 January 2019 with new TS data & 21 November 2018 for Climate data
### Weather data downloaded from... http://labs.arboretum.harvard.edu/weather/

## Data download: 


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
setwd("~/Documents/git/treespotters/analysis")

## Get all that data!
## Start with phenology data and clean
bb<-read.csv("input/individual_phenometrics_data.csv", header=TRUE)
bb<-bb%>%
  rename(lat=Latitude)%>%
  rename(long=Longitude)%>%
  rename(elev=Elevation_in_Meters)%>%
  rename(year=First_Yes_Year)%>%
  rename(month=First_Yes_Month)%>%
  rename(day=First_Yes_Day)%>%
  rename(doy=First_Yes_DOY)%>%
  rename(numYs=Multiple_Observers)%>%
  rename(photo=Daylength) %>%
  rename(phase=Phenophase_Description)%>%
  rename(ID=Individual_ID)
bb.pheno<-dplyr::select(bb, Genus, Species, Common_Name, phase, lat, long, elev, year, doy, numYs, photo, ID)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Breaking leaf buds", "budburst", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Leaves", "leafout", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Flowers or flower buds", "flowers", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Falling leaves", "leaf drop", bb.pheno$phase)




