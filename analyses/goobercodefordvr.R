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
nsites <- ncol(cc.dvr) - 3 ## this is to subtract date, doy, and year columns from site count
sites <- as.data.frame(subset(dvr, select=c("climatetype")))
sites <- as.data.frame(sites[!duplicated(sites),])
sites$siteslist<-1:nsites
colnames(sites)<-c("climatetype", "siteslist")
dvr <- full_join(dvr, sites)

individuals <- as.data.frame(subset(dvr, select=c("id")))
individuals <- as.data.frame(individuals[!duplicated(individuals),])
ninds <- nrow(individuals)
individuals$indslist<-1:ninds
colnames(individuals)<-c("id", "indslist")
dvr <- full_join(dvr, individuals)



springtemps <- function(period) {
  
  nyears<-length(period)
  
  for(i in 1:nsites){#i=2
    print(i)
    sitesi<-sites$siteslist[i]
    
    dvr$mst <- NA
    dvr$dvr.temp <- NA
    
    springtemps <- vector()
    dvrtemps <- vector()
      
    for(j in period){#j=2018
        print(paste(i,j))
        
        springtemps <- cc.dvr[(cc.dvr$doy>=60 & cc.dvr$doy<=151 & cc.dvr$year==j),] # average spring temp from March 1-May 31
        springtemps <- springtemps[,(sitesi+3)] ### finding the correct column for the climate type we're using
        
        dvr$mst <- ifelse(dvr$year==j & dvr$siteslist==sitesi, 
                            mean(springtemps, rm.na=TRUE), dvr$mst)
        
      }
  }
      
      return(dvr)
  }
}
        
foo <- springtemps(period)        
        
        
        
        
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



