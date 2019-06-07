### Thinking about flowers and fruits... 
# Started 3 June 2019 - Cat

rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Load Libraries
library(dplyr)
library(tidyr)

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
  
  nyears<-length(period)
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


foo <- springtemps(dvr)

dvrtempfunc <- function(period) {
  
  #ninds<-length(individuals$indslist)
  yearlyresults <- array(NA, dim=c(length(individuals$id), 1, length(period)))
  row.names(yearlyresults) <- individuals$indslist
  colnames(yearlyresults) <- "dvrtemps"
  
  for(i in period){#i=2015
    print(i)
    dvrtemps <- vector()
    indsarray <- array(NA, dim=c(length(individuals$id), 1))
    
    for(j in 1:ninds){#j=1
      print(paste(i,j))
      
      sitenum <- unique(dvr$siteslist[which(dvr$indslist==j)])
      
      dvrtemps <- cc.dvr[(cc.dvr$doy>=dvr$budburst[which(dvr$indslist==j & dvr$year==i)] & 
                              cc.dvr$doy<=dvr$leafout[which(dvr$indslist==j & dvr$year==i)] & cc.dvr$year==i),]
      dvrtemps <- dvrtemps[,(sitenum+3)]
        
      indsarray[which(individuals$indslist==j),1] <- mean(dvrtemps, na.rm=TRUE) 
    
    }
    
    yearlyresults[,,i] <- indsarray
    
  }
  
  indsdf <- as.data.frame(indsarray)
  names(indsdf) <- substring(names(indsdf), 10)
  
  dvr$dvrtemps <- NA
  for(k in c(1:nrow(dvr))){#k=1
    dvr$dvrtemps[k]<-indsdf[which(dvr$indslist[k]==row.names(indsdf)),dvr$year[k]]
  }
  
  return(dvr)  
  
}        

foo <- dvrtempfunc(period)
        
        
        for(k in 1:ninds){#k=2
          print(paste(j,k))
          indsk <- individuals$indslist[k]
          
          if(indsk==individuals$indslist[k])
            
            dvrnew <- vector()
            dvrnew <- dvr[(dvr$year==j & dvr$indslist==indsk),]          
        
            dvrtemps <- cc.dvr[(cc.dvr$year==j & cc.dvr$doy>=dvrnew$budburst & cc.dvr$doy<=dvrnew$leafout),]
            dvrtemps <- dvrtemps[,(sitesi+3)]
        
            dvr$dvr.temp <- ifelse(dvr$year==j & dvr$siteslist==sitesi, 
                               mean(dvrtemps, rm.na=TRUE), dvr$dvr.temp)
        
        
      }
    
  
}

meanspring(dvr)
  


climnew <- data.frame()
dvrnew <- data.frame()
for(i in 1:length(unique(dvr$idyear))) {
  for(j in 1:length(unique(cc$climatetype)))
  
  climnew <- cc[(cc$climatetype[j]),]
  dvrnew <- dvr[(length(unique(dvr$idyear))==i),]
  

}



