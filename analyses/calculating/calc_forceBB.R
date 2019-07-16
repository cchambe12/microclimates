### Let's calculate Forcing for budburst 
# 24 January 2019 - Cat

# Load from calc_mergeall.R (including libraries)

if(is.data.frame(d)){
  
  climandpheno<-data_frame()
  days.btw<-array()
  
  d <- d[!is.na(d$budburst),]
  d <- d[!is.na(d$leafout),]
  
  days.btw <- Map(seq, d$gdd.start, d$budburst, by = 1)
  
  climandpheno <- data.frame(id_year_type = rep.int(d$id_year_type, vapply(days.btw, length, 1L)), 
                             doy = do.call(c, days.btw))
  
  climandpheno <- separate(data = climandpheno, col = id_year_type, into = c("id", "year", "climatetype"), sep = "\\;")
  climandpheno$id_year<-paste(climandpheno$id, climandpheno$year)
  
  addhrs <- as.vector(unique(paste(climandpheno$id_year, climandpheno$doy)))
  
  addhrs =
    data.frame(
      id_yr_day = sort(c(rep(addhrs, times=24))),
      hour = sort(c(rep(seq(1:24))))
    )
  
  climandpheno <- separate(data = addhrs, col = id_yr_day, into = c("id", "year", "doy"), sep = "\\ ")
  climandpheno$id <- as.character(climandpheno$id)
  climandpheno$year <- as.integer(climandpheno$year)
  climandpheno$doy <- as.numeric(climandpheno$doy)
  
  climandpheno$budburst<-ave(climandpheno$doy, climandpheno$id, climandpheno$year, FUN=max) ## Warnings can be ignored - data is clean and checked
  climandpheno$gdd.start<-ave(climandpheno$doy, climandpheno$id, climandpheno$year, FUN=min) ## Warnings can be ignored - data is clean and checked
  
  ## Add Climate data back in 
  cc$hour <- as.numeric(cc$hour)
  
  climandpheno<-full_join(climandpheno, cc)
  climandpheno<-climandpheno[!duplicated(climandpheno),]
  
  #climandpheno$ID <- as.character(climandpheno$ID)
  
  bb_climandpheno<-full_join(climandpheno, d)
  
  bb_forcing_all <- subset(bb_climandpheno, select=c("id", "doy", "year", "tmean",
                                                     "gdd.start", "budburst"))
  bb_forcing_all <-na.omit(bb_forcing_all)
  
  bb_forcing_all <- bb_forcing_all[(bb_forcing_all$year>2015),]
  
  bb_forcing_all$indyear <- paste(bb_forcing_all$id, bb_forcing_all$year, sep=";")
  bb_forcing_all$indyrnum <- as.numeric(as.factor(bb_forcing_all$indyear))
  indsyrlist <- length(unique(bb_forcing_all$indyrnum))
  hourtemps <- data.frame()
  bb_forcing_all$gdd_bb <- NA
  for(i in 1:indsyrlist){ #i=35
    
    hourtemps <- subset(bb_forcing_all[(bb_forcing_all$indyrnum==i),], select=c("year", "doy", "tmean"))
    hourtemps <- hourtemps[order(hourtemps$doy),]
    hourtemps$tmean <- ave(hourtemps$tmean, hourtemps$doy)
    hourtemps <- hourtemps[!duplicated(hourtemps),]
    hourtemps$gddcalc <- ifelse(hourtemps$tmean>0, hourtemps$tmean, 0)
    hourtemps$gdd_bb <- ave(hourtemps$gddcalc, FUN=sum)
    
    bb_forcing_all[which(bb_forcing_all$indyrnum==i), 9] <- unique(hourtemps$gdd_bb)
    
    
  }   
  
  forcebb <- subset(bb_forcing_all, select=c("id", "year", "budburst", "gdd_bb"))
  forcebb <- forcebb[!duplicated(forcebb),]
                    
  
} else {
  print("Error: forcebb not a data.frame")
}

stop("Not an error, forcing for budburst is now included in new df ('forcebb'). Also, you can ignore the warning messages below -- is due to a bug in package (https://stackoverflow.com/questions/24282550/no-non-missing-arguments-warning-when-using-min-or-max-in-reshape2). 
     I have checked and rechecked the data")
  