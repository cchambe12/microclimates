### Let's add in climate data for forcing
# 24 January 2019 - Cat

# Load from calc_mergeall.R (including libraries)

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
  
} else {
  print("Error: d not a data.frame")
}

stop("Not an error, climate is now added to dataframe. Also, you can ignore the warning messages below -- is due to a bug in package (https://stackoverflow.com/questions/24282550/no-non-missing-arguments-warning-when-using-min-or-max-in-reshape2). 
     I have checked and rechecked the data")