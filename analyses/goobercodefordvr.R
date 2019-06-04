### Thinking about flowers and fruits... 
# Started 3 June 2019 - Cat

rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Load Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(rstan)
library(brms)

# Set Working Directory
setwd("~/Documents/git/microclimates/analyses")

## Data!
dvr<-read.csv("output/clean_budburstandleafout.csv", header=TRUE)
prov <- read.csv("input/provenanceinfo.csv", header=TRUE)

## Now some climate data!
d <- dvr
source("calculating/clean_addinclimate.R")

dvr$risk <- dvr$leafout - dvr$budburst
dvr <- dvr[!is.na(dvr$risk),]
dvr$climatetype <- ifelse(dvr$type=="Treespotters" | dvr$type=="Common Garden", "weldhill", "harvardforest")

dvr$mst <- NA
springtemps <- vector()

meanspring <- function(x) {
for(i in c(1:nrow(dvr))) {
  for(j in c(1:nrow(cc))) 
  springtemps <- ifelse(dvr$year[i]==cc$year[j] & dvr$climatetype[i]==cc$climatetype[j],
                                             cc$tmean[(cc$doy>=60 & cc$doy<=151)], NA) # average spring temp from March 1-May 31
  dvr$mst[i]<- ifelse(dvr$year[i]==cc$year[j] & dvr$climatetype[i]==cc$climatetype[j], 
                   mean(springtemps, rm.na=TRUE), dvr$mst[i])


  
} 
  return(dvr)
}

meanspring(dvr)
  
springtemps <- function(year, climatetype) {

  springtemps <- cc$tmean[(cc$year==year & cc$climatetype==climatetype & cc$doy>=60 & cc$doy<=151)] # average spring temp from March 1-May 31
  springtemps <- springtemps[!is.na(springtemps)]
  dvr$mst<- ifelse(dvr$year==year & dvr$climatetype==climatetype, mean(springtemps), dvr$mst)

  return(dvr)
}

foo <- springtemps(years, clims)

dvr$idyear <- paste(dvr$id, dvr$year)

dvr$dvr.temp <- NA
dvrtemps <- vector()
climnew <- data.frame()
dvrnew <- data.frame()
for(i in 1:length(unique(dvr$idyear))) {
  for(j in 1:length(unique(cc$climatetype)))
  
  climnew <- cc[(cc$climatetype[j]),]
  dvrnew <- dvr[(length(unique(dvr$idyear))==i),]
  dvrtemps <- climnew$tmean[(climnew$doy>=dvrnew$budburst & climnew$doy<=dvrnew$leafout)]
  dvrtemps <- dvrtemps[!is.na(dvrtemps)]
  
  dvr$dvr.temp<- ifelse(length(unique(dvr$idyear))==i, mean(dvrtemps), dvr$dvr.temp)

}



