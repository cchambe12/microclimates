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

testing <- bpest[!duplicated(bpest),]


### Okay now let's test the integral theory...
tall <- runif(100, min=90, max=100)
med <- runif(100, min=70, max=120)
short <- runif(100, min=50, max=140)


tall <- data.frame(bb=as.numeric(tall), site=1)
med <- data.frame(bb=as.numeric(med), site=2)
short <- data.frame(bb=as.numeric(short), site=3)

all <- rbind(tall, med, short)


bbvarest <- data.frame(siteslist=numeric(), integrals=numeric())

for(i in 1:length(unique(all$site))) { # i=1
  testfunc <- approxfun(density(all$bb[(all$site==i)]))
  min <- min(all$bb[(all$site==i)])
  max <- max(all$bb[(all$site==i)])
  integrals <- integrate(testfunc, min, max)
  integrals <- integrals$value
  
  bbvarestadd <- data.frame(siteslist=all$site[(all$site==i)], integrals=integrals)
  
  bbvarest <- rbind(bbvarest, bbvarestadd)
  
}

intsforsites <- bbvarest[!duplicated(bbvarest),]

quartz()
par(mfrow=c(2,2))
allplot <- plot(density(all$bb))
tallplot <- plot(density(tall$bb))
medplot <- plot(density(med$bb))
shortplot <- plot(density(short$bb))

#### siteslist integrals
# siteslist integrals
#   1       0.9102204
#   2       0.9062445
#   3       0.8819444


if(FALSE){
##### Now let's compare to kertosis
library(e1071)
kurtosis(tall$bb) ## -1.244842
kurtosis(med$bb) ## -1.065756
kurtosis(short$bb) ## -1.388553
}