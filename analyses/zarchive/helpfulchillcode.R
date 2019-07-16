## Now bring in climate data...
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

# Find data for the day
cc<-dplyr::select(cc, Temp..F,
                  Rain.in, date, year, doy, hour)
cc$tmin<-ave(cc$Temp..F, cc$date, FUN=min)
cc$tmin<-fahrenheit.to.celsius(cc$tmin)
cc$tmax<-ave(cc$Temp..F, cc$date, FUN=max)
cc$tmax<-fahrenheit.to.celsius(cc$tmax)
cc$tmean<-ave(cc$Temp..F, cc$hour)
cc$tmean<-fahrenheit.to.celsius(cc$tmean)

## If we deem necessary later on in analysis
if(FALSE){
  cc$Rain.in<-ave(cc$Rain.in, cc$date, FUN=sum)
  cc$precip<-conv_unit(cc$Rain.in, "inch", "mm")
}

cc$year<-as.integer(cc$year)




phenos$id_year<-paste(phenos$ID, phenos$year)

climandpheno<-data_frame()
days.btw<-array()

days.btw <- Map(seq, phenos$last.obs, phenos$leafout, by = 1)

climandpheno <- data.frame(id_year = rep.int(phenos$id_year, vapply(days.btw, length, 1L)), 
                           doy = do.call(c, days.btw))

climandpheno$ID<-as.integer(substr(climandpheno$id_year, 0, 6))
climandpheno$year<-as.integer(substr(climandpheno$id_year, 7,11))

addhrs <- as.vector(unique(paste(climandpheno$id_year, climandpheno$doy)))

addhrs =
  data.frame(
    id_yr_day = sort(c(rep(addhrs, times=24))),
    hour = sort(c(rep(seq(1:24))))
  )

climandpheno <- separate(data = addhrs, col = id_yr_day, into = c("ID", "year", "doy"), sep = "\\ ")
climandpheno$ID <- as.numeric(as.character(climandpheno$ID))
climandpheno$year <- as.integer(climandpheno$year)
climandpheno$doy <- as.numeric(climandpheno$doy)

climandpheno$leafout<-ave(climandpheno$doy, climandpheno$ID, climandpheno$year, FUN=max) ## Warnings can be ignored - data is clean and checked
climandpheno$last.obs<-ave(climandpheno$doy, climandpheno$ID, climandpheno$year, FUN=min) ## Warnings can be ignored - data is clean and checked

## Add Climate data back in 
cc<-dplyr::select(cc, year, doy, tmean, tmin, tmax, hour)
cc$hour <- as.numeric(cc$hour)

climandpheno<-full_join(climandpheno, cc)
climandpheno<-climandpheno[!duplicated(climandpheno),]

bb_climandpheno<-full_join(climandpheno, bb)