### Let's calculate chilling!! 
# 29 January 2019 - Cat

# Load from calc_mergeall.R (including libraries)

if(is.data.frame(d)){
  
  climandpheno<-data_frame()
  days.btw<-array()  ### Starting here!! Need to figure out how to do chilling from one year to the next
  
  days.btw <- Map(seq, d$gdd.start, d$budburst, by = 1)
  
  climandpheno <- data.frame(id_year = rep.int(d$id_year, vapply(days.btw, length, 1L)), 
                             doy = do.call(c, days.btw))
  
  climandpheno <- separate(data = climandpheno, col = id_year, into = c("id", "year"), sep = "\\*")
  climandpheno$id_year<-paste(climandpheno$ID, climandpheno$year)
  
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
  cc<-dplyr::select(cc, year, doy, tmean, tmin, tmax, hour)
  cc$hour <- as.numeric(cc$hour)
  
  climandpheno<-full_join(climandpheno, cc)
  climandpheno<-climandpheno[!duplicated(climandpheno),]
  
  #climandpheno$ID <- as.character(climandpheno$ID)
  
  bb_climandpheno<-full_join(climandpheno, d)
  
  bb_forcing_all <- subset(bb_climandpheno, select=c("id", "doy", "year", "tmean"))
  bb_forcing_all <-na.omit(bb_forcing_all)
  
  bb_forcing_all <- bb_forcing_all[(bb_forcing_all$year>2015),]
  
  ### Start with Treespotters and Harvard Forest
  tt <- as.data.frame(table(bb_forcing_all$id, bb_forcing_all$year))
  rms <- subset(tt, tt$Freq==0)
  rms <- as.vector(unique(rms$Var1))
  
  bb_forcing <- subset(bb_forcing_all, !bb_forcing_all$id %in% rms) ## need to do common garden inds after!!
  
  period<-2016:2018
  nyears <- length(period)
  ids<-bb_forcing[!duplicated(bb_forcing$id),]
  ids <- ids[!duplicated(ids$id),]
  ninds <- length(ids$id)
  ids$idslist<-1:103
  
  idlisttomerge <- subset(ids, select=c("id", "idslist"))
  bb_forcing <- full_join(bb_forcing, idlisttomerge)
  
  #bb_forcing <- bb_forcing[!is.na(bb_forcing$doy),]
  
  extractforce<-function(tavg,period){
    
    forcingyears<-array(NA,dim=c(nyears, 1, ninds))
    row.names(forcingyears)<-period
    colnames(forcingyears)<-c("GDD")
    
    yearlyresults<-array(NA,dim=c(length(period),1))
    
    for(i in 1:ninds){
      print(i)
      
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
        
        yearlyresults[which(period==j),1] <- chillcalc.mn$GDH[which(chillcalc.mn$End_year==j)]/24
        
        
      }
      forcingyears[,,i]<-yearlyresults
    }
    
    return(forcingyears)
    
  }
  
  force_all <- extractforce(tavg, period)  
  
  allyears<-as.data.frame(force_all)
  
  allyears <- gather(allyears, idslist, GDD)
  allyears$year <- seq(2016, 2018, by=1)
  allyears$idslist <- as.numeric(substr(allyears$idslist, 5, 7))
  
  bb_forcing <- subset(bb_forcing, select=c("id", "year", "idslist"))
  bb_forcing <- bb_forcing[!duplicated(bb_forcing),]
  
  allyears <- full_join(allyears, bb_forcing)
  
  allyears <- dplyr::select(allyears, -idslist)
  
  #############################
  ### Now for Common Garden ###
  #############################
  bb_forcing_cg <- subset(bb_forcing_all, bb_forcing_all$id %in% rms)
  bb_forcing_cg <- subset(bb_forcing_cg, bb_forcing_cg$year==2018)
  
  period_cg<-2018
  nyears <- length(period_cg)
  ids_cg<-bb_forcing_cg[!duplicated(bb_forcing_cg$id),]
  ids_cg <- ids_cg[!duplicated(ids_cg$id),]
  ninds_cg <- length(ids_cg$id)
  ids_cg$idslist<-1:267
  
  idlisttomerge_cg <- subset(ids_cg, select=c("id", "idslist"))
  bb_forcing_cg <- full_join(bb_forcing_cg, idlisttomerge_cg)
  
  bb_forcing_cg <- bb_forcing_cg[!is.na(bb_forcing_cg$doy),]
  
  extractforce_cg<-function(ninds_cg,period_cg){
    
    forcingyears_cg<-array(NA,dim=c(nyears, 1, ninds_cg))
    row.names(forcingyears_cg)<-period_cg
    colnames(forcingyears_cg)<-c("GDD")
    
    yearlyresults_cg<-array(NA,dim=c(length(period_cg),1))
    
    for(i in 1:ninds_cg){
      print(i)
      
      for(j in period_cg){
        print(paste(i,j))
        
        days<-bb_forcing_cg$doy[bb_forcing_cg$idslist==i & bb_forcing_cg$year==j] #number of days of climate data
        
        tavg_cg <- bb_forcing_cg$tmean[bb_forcing_cg$idslist==i & bb_forcing_cg$year==j]
        
        hrly.temp =
          data.frame(
            Temp = tavg_cg,
            Year = j,
            JDay = sort(days)
          )
        
        
        chillcalc.mn<-chilling(hrly.temp, hrly.temp$JDay[1], hrly.temp$JDay[nrow(hrly.temp[1])]) 
        
        yearlyresults_cg[which(period_cg==j),1] <- chillcalc.mn$GDH[which(chillcalc.mn$End_year==j)]/24
        
        
      }
      forcingyears_cg[,,i]<-yearlyresults_cg
    }
    
    return(forcingyears_cg)
    
  }
  
  force_all_cg <- extractforce_cg(ninds_cg, period_cg)  
  
  oneyear<-as.data.frame(force_all_cg)
  oneyear <- gather(oneyear, idslist, GDD)
  oneyear$year <- 2018
  oneyear$idslist <- as.numeric(substr(oneyear$idslist, 5, 7))
  
  bb_forcing_cg <- subset(bb_forcing_cg, select=c("id", "year", "idslist"))
  bb_forcing_cg <- bb_forcing_cg[!duplicated(bb_forcing_cg),]
  
  oneyear <- full_join(oneyear, bb_forcing_cg)
  
  oneyear <- dplyr::select(oneyear, -idslist)
  
  forcebb <- full_join(allyears, oneyear)
  
} else {
  print("Error: forcebb not a data.frame")
}

stop("Not an error, forcing for budburst is now included.")
