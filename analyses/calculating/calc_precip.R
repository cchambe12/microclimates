### Let's calculate Forcing from budburst to leafout
# 29 January 2019 - Cat
# based off calc_forceBB.R

# Load from calc_mergeall.R (including libraries)

if(is.data.frame(d)){
  
  climandpheno<-data_frame()
  days.btw<-array()
  
  d.precip <- d[!(d$last.obs<d$leafout),]
  d.precip <- d.precip[!is.na(d.precip$last.obs) | !is.na(d.precip$leafout),]
  days.btw <- Map(seq, d.precip$leafout, d.precip$last.obs, by=1)
  
  climandpheno <- data.frame(id_year_type = rep.int(d.precip$id_year_type, vapply(days.btw, length, 1L)), 
                             doy = do.call(c, days.btw))
  
  climandpheno <- separate(data = climandpheno, col = id_year_type, into = c("id", "year", "climatetype"), sep = "\\;")
  climandpheno$id_year_type<-paste(climandpheno$id, climandpheno$year, climandpheno$climatetype)
  
  addhrs <- as.vector(unique(paste(climandpheno$id_year_type, climandpheno$doy)))
  
  addhrs =
    data.frame(
      id_yr_day = sort(c(rep(addhrs, times=24))),
      hour = sort(c(rep(seq(1:24))))
    )
  
  climandpheno <- separate(data = addhrs, col = id_yr_day, into = c("id", "year", "climatetype", "doy"), sep = "\\ ")
  climandpheno$id <- as.character(climandpheno$id)
  climandpheno$year <- as.integer(climandpheno$year)
  climandpheno$climatetype <- as.character(climandpheno$climatetype)
  climandpheno$doy <- as.numeric(climandpheno$doy)
  
  climandpheno$leafout<-ave(climandpheno$doy, climandpheno$id, climandpheno$year, FUN=min) ## Warnings can be ignored - data is clean and checked
  climandpheno$last.obs<-ave(climandpheno$doy, climandpheno$id, climandpheno$year, FUN=max) ## Warnings can be ignored - data is clean and checked
  
  ## Add Climate data back in 
  if(use.hobos==FALSE){
    cc<-dplyr::select(cc, year, doy, tmean, hour, climatetype, precip)
  }
  if(use.hobos==TRUE){
    cc<-dplyr::select(cc, year, doy, tmean, hour, climatetype)
  }
  cc$hour <- as.numeric(cc$hour)
  
  climandpheno<-full_join(climandpheno, cc)
  climandpheno<-climandpheno[!duplicated(climandpheno),]
  
  #climandpheno$ID <- as.character(climandpheno$ID)
  
  bb_climandpheno<-full_join(climandpheno, d.precip)
  
  precip_all <- subset(bb_climandpheno, select=c("id", "doy", "year", "precip", 
                                                      "last.obs", "leafout", "id_year_type"))
  precip_all <-na.omit(precip_all)
  
  precip_all <- precip_all[(precip_all$year>yearlim),]
  
  #dvr_forcing_all$indyear <- paste(dvr_forcing_all$id, dvr_forcing_all$year, sep=";")
  precip_all$indyrnum <- as.numeric(as.factor(precip_all$id_year_type))
  indsyrlist <- length(unique(precip_all$indyrnum))
  hourtemps <- data.frame()
  precip_all$precipgs <- NA
  for(i in 1:indsyrlist){ #i=2
    
    hourtemps <- subset(precip_all[(precip_all$indyrnum==i),], select=c("year", "doy", "precip"))
    hourtemps <- hourtemps[order(hourtemps$doy),]
    hourtemps <- hourtemps[!duplicated(hourtemps),]
    hourtemps$precipgs <- ave(hourtemps$precip, FUN=sum)
    
    
    precip_all[which(precip_all$indyrnum==i), 9] <- unique(hourtemps$precipgs)
    
    
  }   
  
  precipgs <- subset(precip_all, select=c("id", "year", "last.obs", "leafout", "precipgs"))
  precipgs <- precipgs[!duplicated(precipgs),]
  
  gdd.stan <- left_join(gdd.stan, precipgs)
  
} else {
  print("Error: tmeangs not a data.frame.")
}

stop("Not an error, tmean for the growing season is now included in new dataframe ('gdd.stan'). Also, you can ignore the warning messages below -- is due to a bug in package (https://stackoverflow.com/questions/24282550/no-non-missing-arguments-warning-when-using-min-or-max-in-reshape2). 
     I have checked and rechecked the data")
