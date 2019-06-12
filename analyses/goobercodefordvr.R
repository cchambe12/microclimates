### Thinking about flowers and fruits... 
# Started 3 June 2019 - Cat

rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Load Libraries
library(dplyr)
library(tidyr)
library(rstan)

# Set Working Directory
setwd("~/Documents/git/microclimates/analyses")

## Data!
dvr<-read.csv("output/clean_budburstandleafout.csv", header=TRUE)
prov <- read.csv("input/provenanceinfo.csv", header=TRUE)

## Now some climate data!
cc <- read.csv("output/clean_addinclimate.csv", header=TRUE)


### For this question, only need daily tmean so I will condense below
cc.dvr <- subset(cc, select=c("date", "year", "doy", "tmean", "climatetype"))
cc.dvr$tmean <- ave(cc.dvr$tmean, cc.dvr$date, cc.dvr$climatetype)
cc.dvr <- cc.dvr[!duplicated(cc.dvr),]

cc.dvr <- cc.dvr %>%
  spread(climatetype, tmean)

### Now let's prepare the dvr dataframe for merging climate data
dvr$risk <- dvr$leafout - dvr$budburst
dvr <- dvr[!is.na(dvr$risk),]

dvr$climatetype <- ifelse(dvr$type=="Treespotters" | dvr$type=="Common Garden", "weldhill", "harvardforest")


### Okay, now we can run the function to find mean spring temperature and mean temperature during DVR for each
# site, individual and year. Mean spring temp will be from March 1 - May 31

period<-2015:2019
#nsites <- ncol(cc.dvr) - 3 ## this is to subtract date, doy, and year columns from site count
sites <- arrange(as.data.frame(subset(dvr, select=c("climatetype"))), climatetype)
sites <- as.data.frame(sites[!duplicated(sites),])
sites <-na.omit(sites)
nsites <- length(unique(sites$`sites[!duplicated(sites), ]`))
sites$siteslist<-1:nsites
colnames(sites)<-c("climatetype", "siteslist")
dvr$siteslist <- NA
for(i in c(1:nrow(dvr))){
  for(j in c(1:nrow(sites)))
    dvr$siteslist[i] <- ifelse(dvr$climatetype[i]==sites$climatetype[j], sites$siteslist[j], dvr$siteslist[i])
}


individuals <- as.data.frame(subset(dvr, select=c("id")))
individuals <- as.data.frame(individuals[!duplicated(individuals),])
ninds <- nrow(individuals)
individuals$indslist<-1:ninds
colnames(individuals)<-c("id", "indslist")
dvr$indslist <- NA
for(i in c(1:nrow(dvr))){
  for(j in c(1:nrow(individuals)))
    dvr$indslist[i] <- ifelse(dvr$id[i]==individuals$id[j], individuals$indslist[j], dvr$indslist[i])
}



springtemps <- function(x) {
  
  #nyears<-length(period)
  sitesarray <- array(NA, dim=c(length(period), 1:nsites)) ## may need to tweak this once there are more sites...
  row.names(sitesarray)<-period
  colnames(sitesarray) <- "mst"
  
  for(i in 1:nsites){#i=1
    print(i)
    
    springtemps <- vector()
    dvrtemps <- vector()
    yearlyresults <- array(NA, dim=c(length(period), 1))
    
    for(j in period){#j=2016
        print(paste(i,j))
        yearj <- length(period)
        
        springtemps <- cc.dvr[(cc.dvr$doy>=60 & cc.dvr$doy<=151 & cc.dvr$year==j),] # average spring temp from March 1-May 31
        springtemps <- springtemps[,(i+3)] ### finding the correct column for the climate type we're using
        
        yearlyresults[which(period==j),1] <- mean(springtemps, na.rm=TRUE) 
        
    }
    
    sitesarray[,,i] <- yearlyresults
    
  }
  
  sitesdf <- as.data.frame(sitesarray)
  names(sitesdf) <- substring(names(sitesdf), 5)
  
  x$mst <- NA
  for(k in c(1:nrow(x))){#k=1
    x$mst[k]<-sitesdf[which(x$year[k]==row.names(sitesdf)),x$siteslist[k]]
  }
      
  return(x)  
  
}


dvr <- springtemps(dvr)

dvrtempfunc <- function(x) {
  
  x$dvr.temp <- NA
  
  for(i in c(1:nrow(x))) {#i=3 
    for(j in period){#j=2016
      for(k in 1:nsites){#k=1
        
        dvrtemps <- cc.dvr[(cc.dvr$year==j),] # average spring temp from March 1-May 31
        dvrtemps <- dvrtemps[,(k+3)] ### finding the correct column for the climate type we're using
        dvrtemps <- dvrtemps[(x$budburst[i]:x$leafout[i])]
  
        x$dvr.temp[i] <- mean(dvrtemps, na.rm=TRUE)
      }
    }
  }

  return(x)  
  
}        

dvr <- dvrtempfunc(dvr)

dvr$spp <- paste(substr(dvr$genus, 0,3), substr(dvr$species, 0,3), sep="")

dvr.stan <- subset(dvr, select=c("provenance.lat", "risk", "spp", "type", "dvr.temp", "id"))
dvr.stan <- dvr.stan[!duplicated(dvr.stan),]

dvr.stan <- na.omit(dvr.stan)
dvr.stan <- dvr.stan[(dvr.stan$risk>=0),]

datalist.dvr <- with(dvr.stan, 
                       list(y = risk, 
                            lat = provenance.lat, 
                            #site = as.numeric(as.factor(type)), 
                            dvrtemp = dvr.temp,
                            sp = as.numeric(as.factor(spp)),
                            N = nrow(dvr.stan),
                            n_sp = length(unique(dvr.stan$spp))
                            #n_site = length(unique(dvr.stan$type))
                       )
)


dvr.siteint = stan('stan/nointer_2level_dvrtemp.stan', data = datalist.dvr,
                   iter = 5000, warmup=3000, control=list(max_treedepth = 15,adapt_delta = 0.99)) ###
