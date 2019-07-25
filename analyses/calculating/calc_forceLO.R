### Let's calculate Forcing for leafout
# 25 July 2019 - Cat

# Load from calc_mergeall.R (including libraries)

if(is.data.frame(d)){
  
  climandpheno<-data_frame()
  days.btw<-array()
  
  d <- d[!(d$gdd.start>d$leafout),]
  d <- d[!is.na(d$leafout) | !is.na(d$gdd.start),]
  
  days.btw <- Map(seq, d$gdd.start, d$leafout, by = 1)
  
  climandpheno <- data.frame(id_year_type = rep.int(d$id_year_type, vapply(days.btw, length, 1L)), 
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
  
  climandpheno$leafout<-ave(climandpheno$doy, climandpheno$id, climandpheno$year, FUN=max) ## Warnings can be ignored - data is clean and checked
  climandpheno$gdd.start<-46
  
  ## Add Climate data back in 
  cc<-dplyr::select(cc, year, doy, tmean, hour, climatetype)
  cc$hour <- as.numeric(cc$hour)
  
  climandpheno<-full_join(climandpheno, cc)
  climandpheno<-climandpheno[!duplicated(climandpheno),]
  
  #climandpheno$ID <- as.character(climandpheno$ID)
  
  lo_climandpheno<-full_join(climandpheno, d)
  
  lo_forcing_all <- subset(lo_climandpheno, select=c("id", "doy", "year", "tmean", 
                                                     "gdd.start", "leafout", "id_year_type"))
  lo_forcing_all <-na.omit(lo_forcing_all)
  
  lo_forcing_all <- lo_forcing_all[(lo_forcing_all$year>yearlim),]
  
  #bb_forcing_all$indyear <- paste(bb_forcing_all$id, bb_forcing_all$year, sep=";")
  lo_forcing_all$indyrnum <- as.numeric(as.factor(lo_forcing_all$id_year_type))
  indsyrlist <- length(unique(lo_forcing_all$indyrnum))
  hourtemps <- data.frame()
  lo_forcing_all$gdd_lo <- NA
  for(i in 1:indsyrlist){ #i=1
    
    hourtemps <- subset(lo_forcing_all[(lo_forcing_all$indyrnum==i),], select=c("year", "doy", "tmean"))
    hourtemps <- hourtemps[order(hourtemps$doy),]
    hourtemps$tmean <- ave(hourtemps$tmean, hourtemps$doy)
    hourtemps <- hourtemps[!duplicated(hourtemps),]
    hourtemps$gddcalc <- ifelse(hourtemps$tmean>0, hourtemps$tmean, 0)
    hourtemps$gdd_lo <- ave(hourtemps$gddcalc, FUN=sum)
    
    lo_forcing_all[which(lo_forcing_all$indyrnum==i), 9] <- unique(hourtemps$gdd_lo)
    
    
  }   
  
  forcelo <- subset(lo_forcing_all, select=c("id", "year", "leafout", "gdd_lo"))
  forcelo <- forcelo[!duplicated(forcelo),]
  
  
} else {
  print("Error: forcelo not a data.frame")
}

stop("Not an error, forcing for leafout is now included in new df ('forcelo'). Also, you can ignore the warning messages below -- is due to a bug in package (https://stackoverflow.com/questions/24282550/no-non-missing-arguments-warning-when-using-min-or-max-in-reshape2). 
     I have checked and rechecked the data")
