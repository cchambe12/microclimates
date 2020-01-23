## Script to extract climate data and climate variation across Europe and N America
## 
# Based off Nacho's code "Get_range_nam_parallel.R" in lizzieinvancouver/ospree/analyses/ranges 
# Date: 22 Jan 2020 by Cat and Dan



# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## load packages
require(sp)
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
pnw <- data.frame(long=c(-123, -119, -119, -123, -123), lat= c(52, 52, 46, 46, 52))
ppnw = Polygon(pnw)
pspnw = Polygons(list(ppnw),1)
spspnw = SpatialPolygons(list(pspnw))
proj4string(spspnw) <- CRS("+proj=longlat +datum=WGS84")
pnwshp<-spTransform(spspnw,CRS("+proj=longlat +datum=WGS84"))

ne <- data.frame(long=c(-76, -72, -72, -76, -76), lat= c(48, 48, 42, 42, 48))
pne = Polygon(ne)
psne = Polygons(list(pne),1)
spsne = SpatialPolygons(list(psne))
proj4string(spsne) <- CRS("+proj=longlat +datum=WGS84")
neshp<-spTransform(spsne,CRS("+proj=longlat +datum=WGS84"))

eur <- data.frame(long=c(6, 10, 10,  6,  6), lat= c(52, 52, 46, 46, 52))
peur = Polygon(eur)
pseur = Polygons(list(peur),1)
spseur = SpatialPolygons(list(pseur))
proj4string(spseur) <- CRS("+proj=longlat +datum=WGS84")
eurshp<-spTransform(spseur,CRS("+proj=longlat +datum=WGS84"))

climshps <- list(c(pnwshp, neshp, eurshp))


# define period
period<-1980:2016
#period<-2009:2010


## set function
extractchillforce<-function(spslist,tmin,tmax,period){
  
  ## define array to store results ## i=1
  nsps<-length(spslist) #spslist=climshps[1]
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

    ## commence loop  
    for (i in 1:nsps){#i=1 #spslist=climshps[[i]]
      print(c(i, j))
      
      # load shapefile
      spsshape <- spslist[[i]]
      
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
      
      #turn into data frame and remove NAs
      chmin<-as.data.frame(tempschillmin)
      chmin<-subset(chmin,!is.na(rowSums(chmin)))
      names(chmin) <- c("z", c(chillstart:yrend), c(1:chillend))
      chmax<-as.data.frame(tempschillmax)
      chmax<-subset(chmax,!is.na(rowSums(chmax)))
      names(chmax) <- c("z", c(chillstart:yrend), c(1:chillend))
      
      chmin<-chmin[,2:153]
      chmax<-chmax[,2:153]
      
      ## calculate chilling
      chillunitseachcelleachdaymin<-apply(chmin,2,function(x){
        Tmin<-x
        return(Tmin)})
      tminchill<-chillunitseachcelleachdaymin[(as.numeric(rownames(chillunitseachcelleachdaymin))==i)]
      
      chillunitseachcelleachdaymax<-apply(chmax,2,function(x){
        Tmax<-x
        return(Tmax)})
      tmaxchill<-chillunitseachcelleachdaymax[(as.numeric(rownames(chillunitseachcelleachdaymax))==i)]
      
      meandaily <- (tminchill + tmaxchill)/2
      
      
      x <-as.Date(as.numeric(colnames(chillunitseachcelleachdaymin)),origin=paste0(j,"-01-01"))
      
      hrly.temp=
        data.frame(
          Temp = c(rep(meandaily, each = 24)),
          Year = c(rep(j, times=24)),
          #JDay = sort(c(rep(seq(1:length(colnames(meandaily))), times = 24)))
          JDay = sort(c(rep(yday(x), times=24)))
        )
      
      #turn into data frame and remove NAs
      wamin<-as.data.frame(tempsforcemin)
      wamin<-subset(wamin,!is.na(rowSums(wamin)))
      names(wamin) <- c("z", c(forcestart:forceend))
      wamax<-as.data.frame(tempsforcemax)
      wamax<-subset(wamax,!is.na(rowSums(wamax)))
      names(wamax) <- c("z", c(forcestart:forceend))
      
      wamin<-wamin[,2:93]
      wamax<-wamax[,2:93]
      
      ## calculate forcing (GDD)
      warmunitseachcelleachdaymin<-apply(wamin,2,function(x){
        Tmin.warm<-x
        return(Tmin.warm)})
      tminwarm<-warmunitseachcelleachdaymin[(as.numeric(rownames(warmunitseachcelleachdaymin))==i)]
      
      warmunitseachcelleachdaymax<-apply(wamax,2,function(x){
        Tmax.warm<-x
        return(Tmax.warm)})
      tmaxwarm<-warmunitseachcelleachdaymax[(as.numeric(rownames(warmunitseachcelleachdaymax))==i)]
      
      meandaily.warm <- (tminwarm + tmaxwarm)/2
      
      x.warm <-as.Date(as.numeric(colnames(warmunitseachcelleachdaymin)),origin=paste0(j,"-01-01"))
      
      hrly.temp.warm =
        data.frame(
          Temp = c(rep(meandaily.warm, each = 24)),
          Year = c(rep(j, times=24)),
          JDay = sort(c(rep(yday(x.warm), times = 24)))
        )
      
      chillcalc.mn<-chilling(hrly.temp, hrly.temp$JDay[1], hrly.temp$JDay[nrow(hrly.temp[1])])
      warmcalc.mn<-chilling(hrly.temp.warm, hrly.temp.warm$JDay[1], hrly.temp.warm$JDay[nrow(hrly.temp.warm[1])])
      
      yearlyresults[which(period==j),1]<-mean(hrly.temp.warm$Temp, na.rm=TRUE)
      yearlyresults[which(period==j),2]<-sd(hrly.temp.warm$Temp, na.rm=TRUE)
      yearlyresults[which(period==j),3]<-var(hrly.temp.warm$Temp, na.rm=TRUE)
      yearlyresults[which(period==j),4]<-mean((warmcalc.mn$GDH[which(warmcalc.mn$End_year==j)])/24, na.rm=TRUE)
      yearlyresults[which(period==j),5]<-sd((warmcalc.mn$GDH[which(warmcalc.mn$End_year==j)])/24, na.rm=TRUE)
      yearlyresults[which(period==j),6]<-mean(chillcalc.mn$Utah_Model[which(chillcalc.mn$End_year==j)], na.rm=TRUE)
      yearlyresults[which(period==j),7]<-sd(chillcalc.mn$Utah_Model[which(chillcalc.mn$End_year==j)], na.rm=TRUE)
      yearlyresults[which(period==j),8]<-mean(chillcalc.mn$Chill_portions[which(chillcalc.mn$End_year==j)], na.rm=TRUE)
      yearlyresults[which(period==j),9]<-sd(chillcalc.mn$Chill_portions[which(chillcalc.mn$End_year==j)], na.rm=TRUE)
      
    }
        
        climateyears[,,i]<-yearlyresults
        
      } 
      
  return(climateyears)
      
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
