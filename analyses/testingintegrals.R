rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(climwin)
library(lubridate)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd()))>0) { 
  setwd("~/Documents/GitHub/ospree/analyses/bb_analysis")
} else if(length(grep("lizzie", getwd()))>0) {
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")
} else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis")
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")


# Get budburst data across 45 sites for BETPEN
# Betula puendula data from PEP (both have has GDD from 1 Jan to leafout)
# bp has mat from March 1st to June 1st and mat.lo is 30 days before leafout (uses tg -- aka mean -- data from E-OBS)
# bpalt is similar, but calculated uses txtm -- aka min and max (and we caculate the mean ourselves from those values) -- data from E-OBS) ... we don't use this currently 
bp <- read.csv("PEP_climate/output/betpen_allchillsandgdds_45sites_mat_forsims.csv", header=TRUE)

bbsw <- subset(bp, select=c("year", "lo", "siteslist"))
bbsw$bb_date <- as.Date(bbsw$lo, origin=paste0(bbsw$year, "-01-01"))
bbsw$bb_date <- as.character(bbsw$bb_date)
bbsw$doy95 <- bbsw$lo - 4

bbsw <- subset(bbsw, select=c("year", "bb_date", "lo", "doy95", "siteslist"))
colnames(bbsw) <- c("Year", "bb_date", "bb_mean", "doy95", "spatial")
bbsw$bb_date <- as.character(bbsw$bb_date)

bbswpre <- bbsw[(bbsw$Year<=1960),]
bbswpost <- bbsw[(bbsw$Year>1960),]

bbswtest <- bbswpre[(bbswpre$spatial==1),]



foo <- approxfun(density(bbsw$bb_mean))
plot(density(bbsw$bb_mean))

min(bbsw$bb_mean) # 79
max(bbsw$bb_mean) # 144

integrate(foo, 79, 144)

bpest <- data.frame(siteslist=numeric(), integrals=character())

for(i in 1:length(unique(bbsw$spatial))) { # i=1
  goo <- approxfun(density(bbsw$bb_mean[(bbsw$spatial==i)]))
  min <- min(bbsw$bb_mean[(bbsw$spatial==i)])
  max <- max(bbsw$bb_mean[(bbsw$spatial==i)])
  integrals <- integrate(goo, min, max)
  integrals <- integrals$value
  
  bpestadd <- data.frame(siteslist=bbsw$spatial[(bbsw$spatial==i)], integrals=integrals)
  
  bpest <- rbind(bpest, bpestadd)
  
}

