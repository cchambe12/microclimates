### Let's calculate Forcing for budburst AND leafout
# 24 January 2019 - Cat

# Load from bb_cleanmergeall.R (including libraries)

if(is.data.frame(d)){
  
  ## Now it's time to bring in climate data...
  # Clean up the dataframe 
  cc<-read.csv("input/weldhill.csv", header=TRUE)
  cc<-cc%>%
    rename(date.time=Eastern.daylight.time)
  cc$date<-gsub("\\s* .*$", '', cc$date.time)
  cc$date<- as.Date(cc$date, "%m/%d/%Y")
  cc$date<-gsub("001", "201", cc$date)
  cc$year<-substr(cc$date, 0, 4)
  cc$doy<-yday(cc$date)
  cc$hour<-gsub("^.* \\s*|\\s*:.*$", '', cc$date.time)
  
  # Find data for the day and by hour
  cc<-dplyr::select(cc, Temp..F,
                    Rain.in, date, year, doy, hour)
  cc$tmin<-ave(cc$Temp..F, cc$date, FUN=min)
  cc$tmin<-fahrenheit.to.celsius(cc$tmin)
  cc$tmax<-ave(cc$Temp..F, cc$date, FUN=max)
  cc$tmax<-fahrenheit.to.celsius(cc$tmax)
  cc$tmean<-ave(cc$Temp..F, cc$hour)
  cc$tmean<-fahrenheit.to.celsius(cc$tmean)
  
  ## If we deem necessary later on in analysis we can add precip back in
  if(FALSE){
    cc$Rain.in<-ave(cc$Rain.in, cc$date, FUN=sum)
    cc$precip<-conv_unit(cc$Rain.in, "inch", "mm")
  }
  
  cc$year<-as.integer(cc$year)
  
  
  d$id_year<-paste(d$ID, d$year, sep="*")
  
  climandpheno<-data_frame()
  days.btw<-array()
  
  days.btw <- Map(seq, d$gdd.start, d$budburst, by = 1)
  
  climandpheno <- data.frame(id_year = rep.int(d$id_year, vapply(days.btw, length, 1L)), 
                             doy = do.call(c, days.btw))
  
  climandpheno <- separate(data = climandpheno, col = id_year, into = c("ID", "year"), sep = "\\*")
  climandpheno$id_year<-paste(climandpheno$ID, climandpheno$year)
  
  addhrs <- as.vector(unique(paste(climandpheno$id_year, climandpheno$doy)))
  
  addhrs =
    data.frame(
      id_yr_day = sort(c(rep(addhrs, times=24))),
      hour = sort(c(rep(seq(1:24))))
    )
  
  climandpheno <- separate(data = addhrs, col = id_yr_day, into = c("ID", "year", "doy"), sep = "\\ ")
  climandpheno$ID <- as.character(climandpheno$ID)
  climandpheno$year <- as.integer(climandpheno$year)
  climandpheno$doy <- as.numeric(climandpheno$doy)
  
  climandpheno$budburst<-ave(climandpheno$doy, climandpheno$ID, climandpheno$year, FUN=max) ## Warnings can be ignored - data is clean and checked
  climandpheno$gdd.start<-ave(climandpheno$doy, climandpheno$ID, climandpheno$year, FUN=min) ## Warnings can be ignored - data is clean and checked
  
  ## Add Climate data back in 
  cc<-dplyr::select(cc, year, doy, tmean, tmin, tmax, hour)
  cc$hour <- as.numeric(cc$hour)
  
  climandpheno<-full_join(climandpheno, cc)
  climandpheno<-climandpheno[!duplicated(climandpheno),]
  
  #climandpheno$ID <- as.character(climandpheno$ID)
  
  bb_climandpheno<-full_join(climandpheno, d)
  
  bb_forcing <- subset(bb_climandpheno, select=c("ID", "doy", "year", "tmean"))
  bb_forcing <-na.omit(bb_forcing)
  
  bb_forcing <- bb_forcing[(bb_forcing$year>2015),]
  tt <- as.data.frame(table(bb_forcing$ID, bb_forcing$year))
  rms <- subset(tt, tt$Freq==0)
  rms <- as.vector(unique(rms$Var1))
  
  bb_forcing <- subset(bb_forcing, !bb_forcing$ID %in% rms) ## need to do common garden inds after!!
  
  
  period<-2016:2018
  nyears <- length(period)
  ids<-bb_forcing[!duplicated(bb_forcing$ID),]
  ids <- ids[!duplicated(ids$ID),]
  ninds <- length(ids$ID)
  ids$idslist<-1:103
  
  idlisttomerge <- subset(ids, select=c("ID", "idslist"))
  bb_forcing <- full_join(bb_forcing, idlisttomerge)
  
  bb_forcing <- bb_forcing[!is.na(bb_forcing$doy),]
  
  extractforce<-function(tavg,period){
    
    for(i in 1:ninds){
      print(i)
      
      forcingyears<-data.frame(matrix(ncol=3, nrow=0))
      colnames(forcingyears)<-c("ID", "year", "GDD")
      
      yearlyresults<-data.frame(matrix(ncol=3, nrow=0))
      colnames(yearlyresults)<-c("ID", "year", "GDD")
      
      for(j in period){
        print(paste(i,j))
        
        days<-bb_forcing$doy[bb_forcing$idslist==i & bb_forcing$year==j] #number of days of climate data
       
        tavg <- bb_forcing$tmean[bb_forcing$idslist==i & bb_forcing$year==j]
      
        hrly.temp =
          data.frame(
            Temp = tavg,
            Year = j,
            JDay = sort(days)
          )

      
        chillcalc.mn<-chilling(hrly.temp, hrly.temp$JDay[1], hrly.temp$JDay[nrow(hrly.temp[1])]) 
        
        ### Not saving every output, just the last one that was done... 
        yearlyresults[i, 1] <- i
        yearlyresults[which(period==j),2] <- j
        yearlyresults[which(period==j),3] <- (chillcalc.mn$GDH[which(chillcalc.mn$End_year==j)])/24
        
        
      }
      forcingyears[i,]<-yearlyresults
    }
    
    return(forcingyears)
    
  }
  
  force_all <- extractforce(tavg, period)  
  
  
  
  
  
} else {
  print("Error: ts not a data.frame")
}

stop("Not an error, forcing for budburst is now included. Also, you can ignore the warning messages below -- is due to a bug in package (https://stackoverflow.com/questions/24282550/no-non-missing-arguments-warning-when-using-min-or-max-in-reshape2). 
     I have checked and rechecked the data")
  