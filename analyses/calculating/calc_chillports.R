### Let's calculate chilling!! 
# 29 January 2019 - Cat

# Load from calc_mergeall.R (including libraries)

if(is.data.frame(d)){
  
  climandpheno<-data_frame()
  days.btw<-array()
  
  leaps <- seq(2000, 2050, by=4)
  
  d$prevyear <- d$year-1
  d$chill.startprev <- NA
  d$chill.startprev <- ifelse(!is.na(d$last.obs) & d$prevyear%in%leaps, d$last.obs, 275)
  d$chill.startprev <- ifelse(!is.na(d$last.obs) & !d$prevyear%in%leaps, d$last.obs, 274)
  d$chill.endprev <- ifelse(d$prevyear%in%leaps, 366, 365)
  d$chill.startthis <- 1
  d$chill.endthis <- 31
  
  d <- d[!(d$chill.endthis>d$budburst),]
  d <- d[!is.na(d$budburst) | !is.na(d$chill.endthis),]
  
  days.btwprev <- Map(seq, d$chill.startprev, d$chill.endprev, by = 1)
  days.btwthis <- Map(seq, d$chill.startthis, d$chill.endthis, by = 1)
  
  d$id_prevyear_type <- paste(d$id, (d$year-1), d$climatetype, sep=";")
  
  climandphenoprev <- data.frame(id_prevyear_type = rep.int(d$id_prevyear_type, vapply(days.btwprev, length, 1L)), 
                             doy = do.call(c, days.btwprev))
  
  climandpheno <- data.frame(id_year_type = rep.int(d$id_year_type, vapply(days.btwthis, length, 1L)), 
                                 doy = do.call(c, days.btwthis))
  
  climandphenoprev <- separate(data = climandphenoprev, col = id_prevyear_type, into = c("id", "prevyear", "climatetype"), sep = "\\;")
  climandpheno <- separate(data = climandpheno, col = id_year_type, into = c("id", "year", "climatetype"), sep = "\\;")
  
  climandphenoprev$id_prevyear_type<-paste(climandphenoprev$id, climandphenoprev$prevyear, climandphenoprev$climatetype)
  climandphenoprev$year <- as.numeric(climandphenoprev$prevyear) + 1
  climandphenoprev$prevyear <- as.numeric(climandphenoprev$prevyear)
  climandpheno$id_year_type<-paste(climandpheno$id, climandpheno$year, climandpheno$climatetype)
  climandpheno$year <- as.numeric(climandpheno$year)
  climandpheno$prevyear <- climandpheno$year-1
  
  addhrs <- as.vector(unique(paste(climandpheno$id_year_type, climandpheno$doy)))
  
  addhrs =
    data.frame(
      id_yr_day = sort(c(rep(addhrs, times=24))),
      hour = sort(c(rep(seq(1:24))))
    )
  
  addhrsprev <- as.vector(unique(paste(climandphenoprev$id_prevyear_type, climandphenoprev$doy)))
  
  addhrsprev =
    data.frame(
      id_yr_day = sort(c(rep(addhrsprev, times=24))),
      hour = sort(c(rep(seq(1:24))))
    )
  
  climandpheno <- separate(data = addhrs, col = id_yr_day, into = c("id", "year", "climatetype", "doy"), sep = "\\ ")
  climandpheno$id <- as.character(climandpheno$id)
  climandpheno$year <- as.integer(climandpheno$year)
  climandpheno$climatetype <- as.character(climandpheno$climatetype)
  climandpheno$doy <- as.numeric(climandpheno$doy)
  
  climandpheno$chill.endthis<-31
  climandpheno$chill.startthis <- 1
  
  climandphenoprev <- separate(data = addhrsprev, col = id_yr_day, into = c("id", "prevyear", "climatetype", "doy"), sep = "\\ ")
  climandphenoprev$id <- as.character(climandphenoprev$id)
  climandphenoprev$prevyear <- as.integer(climandphenoprev$prevyear)
  climandphenoprev$climatetype <- as.character(climandphenoprev$climatetype)
  climandphenoprev$doy <- as.numeric(climandphenoprev$doy)
  
  climandphenoprev$chill.startprev <- ave(climandphenoprev$doy, climandphenoprev$id, climandphenoprev$prevyear, FUN=min)
  climandphenoprev$chill.endprev <- ifelse(climandphenoprev$prevyear%in%leaps, 366, 365)
  
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
  climandpheno$id_year <- paste(climandpheno$id, climandpheno$year, sep="_")
  
  climandphenoprev$year <- climandphenoprev$prevyear ### need to set up to get appropriate climate data
  climandphenoprev <- full_join(climandphenoprev, cc)
  climandphenoprev<-climandphenoprev[!duplicated(climandphenoprev),]
  climandphenoprev$year <- as.numeric(climandphenoprev$year) + 1 ### need to put it back on the same page with other climandpheno
  climandphenoprev$id_year <- paste(climandphenoprev$id, climandphenoprev$year, sep="_")
  
  climandphenoall <- full_join(climandpheno, climandphenoprev)
  
  bb_climandpheno<-full_join(climandphenoall, d)
  
  bb_chilling_all <- subset(bb_climandpheno, select=c("id", "doy", "prevyear", "year", "tmean", "id_year"))
  #bb_chilling_all <- na.omit(bb_chilling_all)
  bb_chilling_all <- bb_chilling_all[!is.na(bb_chilling_all$tmean),]
  bb_chilling_all <- bb_chilling_all[!is.na(bb_chilling_all$id),]
  
  bb_chilling_all <- bb_chilling_all[(bb_chilling_all$year>2015),]
  
  ### Start with Treespotters and Harvard Forest
  if(use.hobos==TRUE){
    rms <- subset(bb_chilling_all, bb_chilling_all$year==2018)
    rms <- unique(rms$id)
  }
  if(use.hobos==FALSE){
    tt <- as.data.frame(table(bb_chilling_all$id, bb_chilling_all$year))
    rms <- subset(tt, tt$Freq==0)
    rms <- as.vector(unique(rms$Var1))
  }
  
  bb_chilling <- subset(bb_chilling_all, !bb_chilling_all$id %in% rms) ## need to do common garden inds after!!
  
  period<-2019
  nyears <- length(period)
  ids<-bb_chilling[!duplicated(bb_chilling$id),]
  ids <- ids[!duplicated(ids$id),]
  ninds <- length(ids$id)
  ids$idslist<-1:ninds
  
  idlisttomerge <- subset(ids, select=c("id", "idslist"))
  bb_chilling <- full_join(bb_chilling, idlisttomerge)
  bb_chilling$doy <- as.numeric(bb_chilling$doy)
  
  bb_chilling$base <- ifelse(!is.na(bb_chilling$prevyear), 
                             ave(bb_chilling$doy, bb_chilling$id, bb_chilling$prevyear, FUN=min), NA)
  
  bb_chilling$doy2 <- NA
  bb_chilling$doy2 <- ifelse(!bb_chilling$prevyear%in%leaps & !is.na(bb_chilling$prevyear), bb_chilling$doy-273, bb_chilling$doy2)
  bb_chilling$doy2 <- ifelse(!bb_chilling$prevyear%in%leaps & !is.na(bb_chilling$prevyear), bb_chilling$doy-273, bb_chilling$doy2)
  bb_chilling$doy2 <- ifelse(is.na(bb_chilling$prevyear), bb_chilling$doy+92, bb_chilling$doy2)
  
  bb_chilling$tmean <- round(bb_chilling$tmean, digits=3)
  #bb_forcing <- bb_forcing[!is.na(bb_forcing$doy),]
  
  extractchill<-function(tavg,period){
    
    chillingyears<-array(NA,dim=c(nyears, 1, ninds))
    row.names(chillingyears)<-period
    colnames(chillingyears)<-c("utah")
    
    yearlyresults<-array(NA,dim=c(length(period),1))
    
    for(i in 1:ninds){ #i=2
      print(i)
      
      for(j in period){ #j=2019
        print(paste(i,j))
        
        days <- bb_chilling$doy2[bb_chilling$idslist==i & bb_chilling$year==j] #number of days of climate data
        
        tavg <- bb_chilling$tmean[bb_chilling$idslist==i & bb_chilling$year==j]
        
        hrly.temp =
          data.frame(
            Temp = tavg,
            Year = rep(j, length(tavg)),
            JDay = sort(days)
          )
        
        
        chillcalc.mn<-chilling(hrly.temp, hrly.temp$JDay[1], hrly.temp$JDay[nrow(hrly.temp[1])]) 
        
        yearlyresults[which(period==j),1] <- chillcalc.mn$Utah_Model[which(chillcalc.mn$End_year==j)]
        #yearlyresults[which(period==j),2] <- chillcalc.mn$Chill_portions[which(chillcalc.mn$End_year==j)]
        
        
      }
      chillingyears[,,i]<-yearlyresults
    }
    
    return(chillingyears)
    
  }
  
  chill_all <- extractchill(tavg, period)  
  
  allyears<-as.data.frame(chill_all)
  
  allyears <- gather(allyears, idslist, utah)
  allyears$year <- 2019
  allyears$idslist <- as.numeric(substr(allyears$idslist, 6, 8))
  
  bb_chilling <- subset(bb_chilling, select=c("id", "year", "idslist"))
  bb_chilling <- bb_chilling[!duplicated(bb_chilling),]
  
  allyears <- full_join(allyears, bb_chilling)
  
  allyears <- dplyr::select(allyears, -idslist)
  
  if(use.hobos==FALSE){
  #############################
  ### Now for Common Garden ###
  #############################
  bb_chilling_cg <- subset(bb_chilling_all, bb_chilling_all$id %in% rms)
  bb_chilling_cg <- subset(bb_chilling_cg, bb_chilling_cg$year%in%2018)
  
  period_cg<-2018
  nyears <- length(period_cg)
  ids_cg<-bb_chilling_cg[!duplicated(bb_chilling_cg$id),]
  ids_cg <- ids_cg[!duplicated(ids_cg$id),]
  ninds_cg <- length(ids_cg$id)
  ids_cg$idslist<-1:ninds_cg
  
  idlisttomerge_cg <- subset(ids_cg, select=c("id", "idslist"))
  bb_chilling_cg <- full_join(bb_chilling_cg, idlisttomerge_cg)
  bb_chilling_cg$doy <- as.numeric(bb_chilling_cg$doy)
  
  bb_chilling_cg$doy2 <- NA
  bb_chilling_cg$doy2 <- ifelse(bb_chilling_cg$prevyear%in%leaps, bb_chilling_cg$doy-274, bb_chilling_cg$doy2)
  bb_chilling_cg$doy2 <- ifelse(!bb_chilling_cg$prevyear%in%leaps & !is.na(bb_chilling_cg$prevyear), bb_chilling_cg$doy-273, bb_chilling_cg$doy2)
  bb_chilling_cg$doy2 <- ifelse(is.na(bb_chilling_cg$prevyear), bb_chilling_cg$doy+92, bb_chilling_cg$doy2)
  
  bb_chilling_cg$tmean <- round(bb_chilling_cg$tmean, digits=3)
  
  bb_chilling_cg <- bb_chilling_cg[!is.na(bb_chilling_cg$doy),]
  
  extractchill_cg<-function(ninds_cg,period_cg){
    
    chillingyears_cg<-array(NA,dim=c(nyears, 1, ninds_cg))
    row.names(chillingyears_cg)<-period_cg
    colnames(chillingyears_cg)<-c("utah")
    
    yearlyresults_cg<-array(NA,dim=c(length(period_cg),1))
    
    for(i in 1:ninds_cg){ #i=1
      print(i)
      
      for(j in period_cg){ #j=2018
        print(paste(i,j))
        
        days <- bb_chilling_cg$doy2[bb_chilling_cg$idslist==i & bb_chilling_cg$year==j] #number of days of climate data
        
        tavg_cg <- bb_chilling_cg$tmean[bb_chilling_cg$idslist==i & bb_chilling_cg$year==j]
        
        hrly.temp =
          data.frame(
            Temp = tavg_cg,
            Year = rep(j, length(days)),
            JDay = sort(days)
          )
        
        
        chillcalc.mn<-chilling(hrly.temp, hrly.temp$JDay[1], hrly.temp$JDay[nrow(hrly.temp[1])]) 
        
        yearlyresults_cg[which(period_cg==j),1] <- chillcalc.mn$Utah_Model[which(chillcalc.mn$End_year==j)]
        #yearlyresults[which(period==j),2] <- chillcalc.mn$Chill_portions[which(chillcalc.mn$End_year==j)]
        
        
      }
      chillingyears_cg[,,i]<-yearlyresults_cg
    }
    
    return(chillingyears_cg)
    
  }
  
  chill_all_cg <- extractchill_cg(ninds_cg, period_cg)  
  
  oneyear<-as.data.frame(chill_all_cg)
  oneyear <- gather(oneyear, idslist, utah)
  oneyear$year <- 2018
  oneyear$idslist <- as.numeric(substr(oneyear$idslist, 6, 8))
  
  bb_chilling_cg <- subset(bb_chilling_cg, select=c("id", "year", "idslist"))
  bb_chilling_cg <- bb_chilling_cg[!duplicated(bb_chilling_cg),]
  
  oneyear <- full_join(oneyear, bb_chilling_cg)
  
  oneyear <- dplyr::select(oneyear, -idslist)
  
  chillbb <- full_join(allyears, oneyear)
  }
  
  if(use.hobos==TRUE){
    chillbb <- allyears
  }
  
  gdd.stan <- left_join(gdd.stan, chillbb)
  
} else {
  print("Error: chillbb not a data.frame")
}

stop("Not an error, chilling for budburst is now included.")
