### Let's add in climate data for forcing
# 24 January 2019 - Cat

# Load from calc_mergeall.R (including libraries)

if(is.data.frame(d)){
  ## Now it's time to bring in climate data from Weld Hill...
  # Clean up the dataframe 
  cc.arb<-read.csv("input/weldhill.csv", header=TRUE)
  cc.arb<-cc.arb%>%
    rename(date.time=Eastern.daylight.time)
  cc.arb$date<-gsub("\\s* .*$", '', cc.arb$date.time)
  cc.arb$date<- as.Date(cc.arb$date, "%m/%d/%Y")
  cc.arb$date<-as.Date(gsub("001", "201", cc.arb$date))
  cc.arb$year<-substr(cc.arb$date, 0, 4)
  cc.arb$doy<-yday(cc.arb$date)
  cc.arb$hour<-gsub("^.* \\s*|\\s*:.*$", '', cc.arb$date.time)
  
  # Find data for the day and by hour
  cc.arb<-dplyr::select(cc.arb, Temp..F,
                    Rain.in, date, year, doy, hour)
  
  cc.arb$tmin<-ave(cc.arb$Temp..F, cc.arb$date, FUN=min)
  cc.arb$tmin<-fahrenheit.to.celsius(cc.arb$tmin)
  cc.arb$tmax<-ave(cc.arb$Temp..F, cc.arb$date, FUN=max)
  cc.arb$tmax<-fahrenheit.to.celsius(cc.arb$tmax)
  cc.arb$tmean<-ave(cc.arb$Temp..F, cc.arb$date, cc.arb$hour)
  cc.arb$tmean<-fahrenheit.to.celsius(cc.arb$tmean)
  cc.arb$Temp..F <- NULL
  cc.arb<-cc.arb[!duplicated(cc.arb),]
  
  ## If we deem necessary later on in analysis we can add precip back in
  if(FALSE){
    cc.arb$Rain.in<-ave(cc.arb$Rain.in, cc.arb$date, FUN=sum)
    cc.arb$precip<-conv_unit(cc.arb$Rain.in, "inch", "mm")
  }
  
  cc.arb$year<-as.integer(cc.arb$year)
  
  yearlim <- 2015
  cc.arb$climatetype <- "weldhill"
  cc.arb <- cc.arb[(cc.arb$year>yearlim),]
  
  #### And now time for climate data from Harvard Forest....
  cc.hf <- read.csv("input/hf001-10-15min-m.csv", header=TRUE)
  cc.hf$date <- as.Date(substr(cc.hf$datetime, 0, 10))
  cc.hf$hour <- substr(cc.hf$datetime, 12, 13)
  
  cc.hf$year<-substr(cc.hf$date, 0, 4)
  cc.hf$doy<-yday(cc.hf$date)
  
  cc.hf<-dplyr::select(cc.hf, airt,
                        prec, date, year, doy, hour)
  
  cc.hf$tmin<-ave(cc.hf$airt, cc.hf$date, FUN=min)
  cc.hf$tmax<-ave(cc.hf$airt, cc.hf$date, FUN=max)
  cc.hf$tmean<-ave(cc.hf$airt, cc.hf$date, cc.hf$hour)
  cc.hf<-cc.hf[!duplicated(cc.hf),]
  
  cc.hf$climatetype <- "harvardforest"
  
  ## If we deem necessary later on in analysis we can add precip back in
  if(FALSE){
    cc.hf$prec<-ave(cc.hf$prec, cc.arb$date, FUN=sum)
  }
  
  cc.hf$year<-as.integer(cc.hf$year)
  cc.hf <- cc.hf[(cc.hf$year>yearlim),]
  
  commoncols <- c("date", "year", "doy", "hour", "tmin", "tmax", "tmean", "climatetype")
  cc.arb <- subset(cc.arb, select=commoncols)
  cc.hf <- subset(cc.hf, select=commoncols)
  
  cc <- full_join(cc.arb, cc.hf)
  cc <- cc[!duplicated(cc),]
  
  d$climatetype <- NA
  d$climatetype <- ifelse(d$type=="Treespotters" | d$type=="Common Garden", "weldhill", d$climatetype)
  d$climatetype <- ifelse(d$type=="Harvard Forest", "harvardforest", d$climatetype)
  
  d$id_year_type <- paste(d$id, d$year, d$climatetype, sep=";")
  
  
} else {
  print("Error: d not a data.frame")
}

stop("Not an error, climate is now added to dataframe.")