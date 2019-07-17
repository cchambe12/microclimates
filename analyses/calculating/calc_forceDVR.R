### Let's calculate Forcing from budburst to leafout
# 29 January 2019 - Cat
# based off calc_forceBB.R

# Load from calc_mergeall.R (including libraries)

if(is.data.frame(d)){
  
  climandpheno<-data_frame()
  days.btw<-array()
  
  d.dvr <- d[!(d$budburst>d$leafout),]
  d.dvr <- d.dvr[!is.na(d.dvr$budburst) | !is.na(d.dvr$leafout),]
  days.btw <- Map(seq, d.dvr$budburst, d.dvr$leafout, by=1)
  
  climandpheno <- data.frame(id_year_type = rep.int(d.dvr$id_year_type, vapply(days.btw, length, 1L)), 
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
  climandpheno$budburst<-ave(climandpheno$doy, climandpheno$id, climandpheno$year, FUN=min) ## Warnings can be ignored - data is clean and checked
  
  ## Add Climate data back in 
  cc<-dplyr::select(cc, year, doy, tmean, hour, climatetype)
  cc$hour <- as.numeric(cc$hour)
  
  climandpheno<-full_join(climandpheno, cc)
  climandpheno<-climandpheno[!duplicated(climandpheno),]
  
  #climandpheno$ID <- as.character(climandpheno$ID)
  
  bb_climandpheno<-full_join(climandpheno, d.dvr)
  
  dvr_forcing_all <- subset(bb_climandpheno, select=c("id", "doy", "year", "tmean", 
                                                      "budburst", "leafout", "id_year_type"))
  dvr_forcing_all <-na.omit(dvr_forcing_all)
  
  dvr_forcing_all <- dvr_forcing_all[(dvr_forcing_all$year>yearlim),]
  
  #dvr_forcing_all$indyear <- paste(dvr_forcing_all$id, dvr_forcing_all$year, sep=";")
  dvr_forcing_all$indyrnum <- as.numeric(as.factor(dvr_forcing_all$id_year_type))
  indsyrlist <- length(unique(dvr_forcing_all$indyrnum))
  hourtemps <- data.frame()
  dvr_forcing_all$gdd_dvr <- NA
  dvr_forcing_all$fs.count <- NA
  for(i in 1:indsyrlist){ #i=2
    
    hourtemps <- subset(dvr_forcing_all[(dvr_forcing_all$indyrnum==i),], select=c("year", "doy", "tmean"))
    hourtemps <- hourtemps[order(hourtemps$doy),]
    hourtemps$tmean <- ave(hourtemps$tmean, hourtemps$doy)
    hourtemps <- hourtemps[!duplicated(hourtemps),]
    hourtemps$gddcalc <- ifelse(hourtemps$tmean>0, hourtemps$tmean, 0)
    hourtemps$gdd_dvr <- ave(hourtemps$gddcalc, FUN=sum)
    hourtemps$fs <- ifelse(hourtemps$tmean<=-2.2, 1, 0)
    hourtemps$fs.count <- ave(hourtemps$fs, FUN=sum)
    
    dvr_forcing_all[which(dvr_forcing_all$indyrnum==i), 9] <- unique(hourtemps$gdd_dvr)
    dvr_forcing_all[which(dvr_forcing_all$indyrnum==i), 10] <- unique(hourtemps$fs.count)
    
    
  }   
  
  forcedvr <- subset(dvr_forcing_all, select=c("id", "year", "budburst", "leafout", "gdd_dvr", "fs.count"))
  forcedvr <- forcedvr[!duplicated(forcedvr),]
  
  force <- left_join(forcebb, forcedvr)
  force <- dplyr::select(force, -leafout)
  
  gdd.stan <- left_join(d, force)
  
} else {
  print("Error: forcedvr not a data.frame.")
}

stop("Not an error, forcing from budburst to leafout is now included in new dataframe ('gdd.stan'). Also, you can ignore the warning messages below -- is due to a bug in package (https://stackoverflow.com/questions/24282550/no-non-missing-arguments-warning-when-using-min-or-max-in-reshape2). 
     I have checked and rechecked the data")
