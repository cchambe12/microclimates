## Script to extract climate data and climate variation across Europe and N America
## 
# Based off Nacho's code "Get_range_nam_parallel.R" in lizzieinvancouver/ospree/analyses/ranges 
# Date: 22 Jan 2020 by Cat and Dan



# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## load packages
require(sp)
require(sf)
require(raster)
require(rgdal)
require(ncdf4)
require(abind)
require(chillR)
require(lubridate)


climatedrive = "/n/wolkovich_lab/Lab/Cat/" # Cat's climate drive
## load climate data rasters (these data are not currently in the ospree folder 
nafiles <- dir(climatedrive)[grep("princetonclimdata", dir(climatedrive))]

### Need to first build shapefiles...
pnw <- data.frame(long=c(-123,  -123,  -119, -119), lat= c(52, 46, 52, 46))
coordinates(pnw)=~long+lat
proj4string(pnw)<- CRS("+proj=longlat +datum=WGS84")
pnwshp<-spTransform(pnw,CRS("+proj=longlat"))

ne <- data.frame(long=c(-76,  -76,  -72, -72), lat= c(48, 42, 48, 42))
coordinates(ne)=~long+lat
proj4string(ne)<- CRS("+proj=longlat +datum=WGS84")
neshp<-spTransform(ne,CRS("+proj=longlat"))

eur <- data.frame(long=c(6,  6,  10, 10), lat= c(52, 46, 52, 46))
coordinates(eur)=~long+lat
proj4string(eur)<- CRS("+proj=longlat +datum=WGS84")
eurshp<-spTransform(eur,CRS("+proj=longlat"))

climshps <- list(c(pnw, ne, eur))


# define period
period<-1950:2016
#period<-2009:2010


## set function
extractchillforce<-function(spslist,tmin,tmax,period){
  
  ## define array to store results ## i=1
  nsps<-length(spslist) #nsps<-length(spslist)
  nyears<-length(period)
  chillforcespsyears<-array(NA,dim=c(nyears,9,nsps))
  row.names(chillforcespsyears)<-period
  colnames(chillforcespsyears)<-c("Mean Temp", "Sd Temp", "Variance Temp",
                                  "GDD", "GDD.sd", "UtahChill", "UtahChill.sd", 
                                  "ChillPortions","ChillPortions.sd")
  #dimnames(chillforcespsyears)<-spslist
  
  mins <- maxs <- vector()
  
  for(j in c(period)) { # j = 1980
    print(j)
    
    if(TRUE){
      tmaxthisyr <- list.files(path=paste(climatedrive,nafiles, sep="/"), pattern=paste0("tmax",j), full.names = TRUE)
      tmaxprev <- list.files(path=paste(climatedrive,nafiles, sep="/"), pattern=paste0("tmax",j-1), full.names = TRUE)
      tminthisyr <- list.files(path=paste(climatedrive,nafiles, sep="/"), pattern=paste0("tmin",j), full.names = TRUE)
      tminprev <- list.files(path=paste(climatedrive,nafiles, sep="/"), pattern=paste0("tmin",j-1), full.names = TRUE)
      #jx <- nc_open(tmax)
      tmax <- brick(tmaxthisyr)
      tmax <- rotate(tmax)
      tmaxprev <- brick(tmaxprev)
      tmaxprev <- rotate(tmaxprev)
      tmin <- brick(tminthisyr)
      tmin <- rotate(tmin)
      tminprev <- brick(tminprev)
      tminprev <- rotate(tminprev)
    }
    
    leapyears <- seq(1952, 2020, by=4)
    chillstart <- ifelse((j-1)%in%leapyears,275,274)
    chillend <- ifelse(j%in%leapyears,60,59)
    forcestart <- ifelse(j%in%leapyears,61,60)
    forceend <- ifelse(j%in%leapyears,152,151)
    yrend <- ifelse((j-1)%in%leapyears,366,365)
    
    
    yearlyresults<-array(NA,dim=c(length(period),6))
    ## commence loop  
    for (i in 1:nsps){#i=1 #spslist=ospreespslist[i]
      print(c(i, j))
      
      # load shapefile
      spsshape <- shapefile(spslist[i])
      
      e <- extent(spsshape)
      tmaxshpforce <- crop(tmax[[forcestart:forceend]], e)
      values(tmaxshpforce)<-values(tmaxshpforce)-273.15
      tminshpforce <- crop(tmin[[forcestart:forceend]], e)
      values(tminshpforce)<-values(tminshpforce)-273.15
      
      tmaxshpchill1 <- crop(tmaxprev[[chillstart:yrend]], e)
      tmaxshpchill2 <- crop(tmaxprev[[1:chillend]], e)
      tmaxshpchill <- stack(c(tmaxshpchill1, tmaxshpchill2))
      values(tmaxshpchill)<-values(tmaxshpchill)-273.15
      
      tminshpchill1 <- crop(tminprev[[chillstart:yrend]], e)
      tminshpchill2 <- crop(tminprev[[1:chillend]], e)
      tminshpchill <- stack(c(tminshpchill1, tminshpchill2))
      values(tminshpchill)<-values(tminshpchill)-273.15
      
      spsshapeproj <- spsshape
      proj4string(spsshapeproj) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 ")
      
      # extract values and format to compute means and sdevs
      tempsforcemin<-extract(tminshpforce,spsshapeproj,cellnumbers=TRUE)
      tempsforcemax<-extract(tmaxshpforce,spsshapeproj,cellnumbers=TRUE)
      tempschillmin<-extract(tminshpchill,spsshapeproj,cellnumbers=TRUE)
      tempschillmax<-extract(tmaxshpchill,spsshapeproj,cellnumbers=TRUE)
      #tempsforces<-extract(yearsforce,spsshapeproj,cellnumbers=T)
      
      chmin<-do.call("rbind",tempschillmin)
      chmin<-subset(chmin,!is.na(rowSums(chmin)))
      chmin<-as.data.frame(chmin)
      names(chmin) <- c("z", c(chillstart:yrend), c(1:chillend))
      chmax<-do.call("rbind",tempschillmax)
      chmax<-subset(chmax,!is.na(rowSums(chmax)))
      chmax<-as.data.frame(chmax)
      names(chmax) <- c("z", c(chillstart:yrend), c(1:chillend))
      
      # get coordinates and names
      chcoordmin<-coordinates(tminshpchill[[1]])[chmin[,1],]
      chcoordmax<-coordinates(tmaxshpchill[[1]])[chmax[,1],]
      chmin<-cbind(chcoordmin,chmin[,2:ncol(chmin)])
      chmax<-cbind(chcoordmax,chmax[,2:ncol(chmax)])
      
      if(nrow(chmin)!=nrow(chmax)){
        namcoo1<-apply(chcoordmin,1,function(x){return(paste(x[1],x[2],sep="_"))})  
        namcoo2<-apply(chcoordmax,1,function(x){return(paste(x[1],x[2],sep="_"))})  
        
        torem<-which(!namcoo1%in%namcoo2)
        torem2<-which(!namcoo2%in%namcoo1)
        
        
        if(length(torem)>0){
          chmin=chmin[-torem,]    
        }
        
        if(length(torem2)>0){
          chmax=chmax[-torem2,]    
        }
        
        
      }
      
      wamin<-do.call("rbind",tempsforcemin)
      wamin<-subset(wamin,!is.na(rowSums(wamin)))
      wamin<-as.data.frame(wamin)
      names(wamin) <- c("z",forcestart:forceend)
      wamax<-do.call("rbind",tempsforcemax)
      wamax<-subset(wamax,!is.na(rowSums(wamax)))
      wamax<-as.data.frame(wamax)
      names(wamax) <- c("z",forcestart:forceend)
      
      ffcoordmin<-coordinates(tminshpforce[[1]])[wamin[,1],]
      ffcoordmax<-coordinates(tmaxshpforce[[1]])[wamax[,1],]
      ffmin<-cbind(ffcoordmin,wamin[,2:ncol(wamin)])
      ffmax<-cbind(ffcoordmin,wamax[,2:ncol(wamax)])
      
      wamin<-wamin[,2:93]
      wamax<-wamax[,2:93]
      
      if(nrow(ffmin)!=nrow(ffmax)){
        namcoo1<-apply(ffcoordmin,1,function(x){return(paste(x[1],x[2],sep="_"))})  
        namcoo2<-apply(ffcoordmax,1,function(x){return(paste(x[1],x[2],sep="_"))})  
        
        torem<-which(!namcoo1%in%namcoo2)
        torem2<-which(!namcoo2%in%namcoo1)
        
        
        if(length(torem)>0){
          ffmin=ffmin[-torem,]    
        }
        
        if(length(torem2)>0){
          ffmax=ffmax[-torem2,]    
        }
        
        
      }
      
      minmaxtemp.warm<-abind(ffmin,ffmax, along = 3)
      dateswa<-as.Date(as.numeric(colnames(wamin)),origin=paste0(j,"-01-01"))
      
      warmunitseachcelleachday<-do.call(rbind,
                                        apply(minmaxtemp.warm,1,function(x){
                                          #x<-minmaxtemp[300,,]
                                          extracweathdf<-data.frame(
                                            Year=as.numeric(format(dateswa,"%Y")),
                                            Month=as.numeric(format(dateswa,"%m")),
                                            Day=as.numeric(format(dateswa,"%d")),
                                            Tmax=x[3:nrow(x),2],
                                            Tmin=x[3:nrow(x),1]
                                          )
                                          weather<-fix_weather(extracweathdf)
                                          hourtemps<-stack_hourly_temps(weather,latitude=x[2])
                                          wrm<-chilling(hourtemps,forcestart,forceend)
                                          
                                          return(wrm)
                                          
                                        }
                                        ))
      
      #chmin<-chmin[,3:151]
      #chmax<-chmax[,3:151]
      minmaxtemp<-abind(chmin,chmax, along = 3)
      chmindates<-chmin[,3:154]
      days<-as.numeric(colnames(chmindates))
      jfordates<-ifelse(days>=270, j-1, j)
      datesch<-as.Date(days,origin=paste0(jfordates,"-01-01"))
      
      chillunitseachcelleachday<-do.call(rbind,
                                         apply(minmaxtemp,1,function(x){
                                           #x<-minmaxtemp[300,,]
                                           extracweathdf<-data.frame(
                                             Year=as.numeric(format(datesch,"%Y")),
                                             Month=as.numeric(format(datesch,"%m")),
                                             Day=as.numeric(format(datesch,"%d")),
                                             Tmax=x[3:nrow(x),2],
                                             Tmin=x[3:nrow(x),1]
                                           )
                                           weather<-fix_weather(extracweathdf)
                                           hourtemps<-stack_hourly_temps(weather,latitude=x[2])
                                           chll<-chilling(hourtemps,275,60)
                                           
                                           return(chll)
                                           
                                         }
                                         ))
      
      
      
      yearlyresults[which(period==j),1]<-mean((warmunitseachcelleachday$GDH)/24,na.rm=T)
      yearlyresults[which(period==j),2]<-sd((warmunitseachcelleachday$GDH)/24,na.rm=T)
      yearlyresults[which(period==j),3]<-mean(chillunitseachcelleachday$Utah_Model,na.rm=T)
      yearlyresults[which(period==j),4]<-sd(chillunitseachcelleachday$Utah_Model,na.rm=T)
      yearlyresults[which(period==j),5]<-mean(chillunitseachcelleachday$Chill_portions,na.rm=T)
      yearlyresults[which(period==j),6]<-sd(chillunitseachcelleachday$Chill_portions,na.rm=T)
      
    }
    
    chillforcespsyears[,,i]<-yearlyresults
    
  }  
  
  return(chillforcespsyears)
  
}

## apply function (beware this function takes ~7mins per year, consider 
## parallelizing)
#climaterangecheck <- extractchillforce("Alnus_rubra", tmin, tmax, period)
Climate.in.range<-extractchillforce(ospreespslist[1],tmin,tmax,period)




## saving outputs
#save(Climate.in.range, file = paste("output/Climate.in.range",ospreespslist[4],
#                                    period[1],max(period),"RData",sep="."))


write.csv(Climate.in.range, file = paste("/n/wolkovich_lab/Lab/Cat/Climate.in.range",ospreespslist[i],
                                         period[1],max(period),"csv",sep="."))
if(FALSE){
  ## attempt to parallelize code
  n = 2 # modify according to your RAM memory
  cl <- makeCluster(n)
  registerDoParallel(cl)
  
  Sys.time()
  Climate.in.range.i<-foreach(spslist = ospreespslist[4:7], .packages=c("raster","ncdf4","abind","chillR"),
                              .verbose=T,.errorhandling="pass")  %dopar%  
    extractchillforce(spslist,tmin,tmax,period) ## profiling function only / and all paralell process
  Sys.time()
  
  
  ## saving outputs1         
  for(i in 1:length(Climate.in.range.i)){
    
    Climate.in.range = Climate.in.range.i[[i]][,,1]  
    #save(Climate.in.range, file = paste("output/Climate.in.range",ospreespslist[i],
    #                                     period[1],max(period),"RData",sep="."))
    write.csv(Climate.in.range, file = paste("output/Climate.in.range",ospreespslist[i],
                                             period[1],max(period),"csv",sep="."))
    
    
  }
  
  ## remove aux unnecessary files
  unlink("chorological_maps_dataset/*", recursive = T)
}
