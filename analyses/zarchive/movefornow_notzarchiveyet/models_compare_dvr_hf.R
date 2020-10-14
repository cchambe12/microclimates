### Started 25 July 2019 - Cat
## Work on comparing gdds from weather station vs hobos for DVR!

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load libraries
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(tidyr)
library(brms)

# Set working directory
setwd("~/Documents/git/microclimates/analyses")

## Load the data
gdd.stan <- read.csv("output/clean_gdd_bbanddvr.csv", header=TRUE)
gdd.hobo <- read.csv("output/clean_gdd_bbanddvr_hobo.csv", header=TRUE)

if(FALSE){ ## Testing code
  gdd.stan$stationbb <- gdd.stan$gdd_bb
  gdd.stan$gdd_bb <- NULL
  gdd.stan$stationdvr <- gdd.stan$gdd_dvr
  gdd.stan$gdd_dvr <- NULL
  gdd <- left_join(gdd.hobo, gdd.stan)
  gdd$diffbb <- gdd$stationbb - gdd$gdd_bb
  gdd$diffdvr <- gdd$stationdvr - gdd$gdd_dvr
  range(gdd$diffdvr, na.rm=TRUE)
  range(gdd$diffbb, na.rm=TRUE)
}


gdd.stan <- gdd.stan[(gdd.stan$type=="Harvard Forest"),]
gdd.hobo <- gdd.hobo[(gdd.hobo$type=="Harvard Forest"),]

gdd.hobo$gdd_bb_hobo <- gdd.hobo$gdd_bb
gdd.hobo$gdd_dvr_hobo <- gdd.hobo$gdd_dvr
gdd.hobo$gdd_lo_hobo <- gdd.hobo$gdd_lo

gdd.hobo$gdd_bb <- NULL
gdd.hobo$gdd_dvr <- NULL
gdd.hobo$gdd_lo <- NULL

gdd.hobo$spp <- paste(substr(gdd.hobo$genus, 0, 3), substr(gdd.hobo$species, 0, 3), sep="")
gdd.stan$spp <- paste(substr(gdd.stan$genus, 0, 3), substr(gdd.stan$species, 0, 3), sep="")

commoncols <- c("id", "provenance.lat", "fs.count", "year", "budburst", "leafout", "spp", "climatetype")
gdd.hobo <- subset(gdd.hobo, select=c(commoncols, "gdd_bb_hobo", "gdd_dvr_hobo", "gdd_lo_hobo"))
gdd.stan <- subset(gdd.stan, select=c(commoncols, "gdd_bb", "gdd_dvr", "gdd_lo"))

gdd.hobo$hobonum <- gdd.hobo$climatetype
gdd.hobo$climatetype <- NULL

gdd <- full_join(gdd.stan, gdd.hobo)
gdd$dvr <- gdd$leafout - gdd$budburst
#gdd$leafout <- NULL

gdd <- gdd[(gdd$year==2019),]

gdd <- gdd[(gdd$gdd_bb<=1000),]
gdd <- gdd[(gdd$gdd_bb_hobo<=1000),]

station <- subset(gdd.stan, select=c("gdd_dvr", "provenance.lat", "spp"))
station <- na.omit(station)

hobo <- subset(gdd, select=c("gdd_dvr_hobo", "provenance.lat", "spp"))
hobo <- na.omit(hobo)

mod_station_dvr_hf <- brm(gdd_dvr ~ spp, data=station,
                          control=list(max_treedepth = 15,adapt_delta = 0.99),
                          iter=4000, warmup=2500)

save(mod_station_dvr_hf, file="stan/mod_station_dvr_hf.Rdata")

mod_hobo_dvr_hf <- brm(gdd_dvr_hobo ~ spp, data=hobo,
                       control=list(max_treedepth = 15,adapt_delta = 0.99),
                       iter=4000, warmup=2500)

save(mod_hobo_dvr_hf, file="stan/mod_hobo_dvr_hf.Rdata")



