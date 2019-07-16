### Let's add in climate data for different hobo loggers
# 16 July 2019 - Cat

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
  
  cc.arb$climatetype <- "weldhill"
  cc.arb <- cc.arb[(cc.arb$year>2015),]
  
  ## Now it's time for the Arb hobo loggers
  setwd("~/Documents/git/microclimates/analyses/output/arbclimdata/")
  mycsv = dir("~/Documents/git/microclimates/analyses/output/arbclimdata/", pattern=".csv")
  
  n <- length(mycsv)
  mylist <- vector("list", n)
  
  for(i in 1:n) mylist[[i]] <- read.csv(mycsv[i])
  
  # then open in environment separately:
  for (i in seq(mylist))
    assign(paste0("arb", i), mylist[[i]])
  
  #### And now time for climate data from Harvard Forest....
  setwd("~/Documents/git/microclimates/analyses/output/hfclimdata/")
  mycsv = dir("~/Documents/git/microclimates/analyses/output/hfclimdata/", pattern=".csv")
  
  n <- length(mycsv)
  mylist <- vector("list", n)
  
  for(i in 1:n) mylist[[i]] <- read.csv(mycsv[i])
  
  # then open in environment separately:
  for (i in seq(mylist))
    assign(paste0("hf", i), mylist[[i]])
  
  ## Skip 1 and 6 for now because missing data
  cc <- full_join(arb2, arb3)
  cc <- full_join(cc, arb4)
  cc <- full_join(cc, arb5)
  cc <- full_join(cc, arb7)
  cc <- full_join(cc, arb8)
  cc <- full_join(cc, arb9)
  cc <- full_join(cc, arb10)
  cc <- full_join(cc, arb11)
  cc <- full_join(cc, arb12)
  cc <- full_join(cc, arb13)
  cc <- full_join(cc, arb14)
  cc <- full_join(cc, arb15)
  
  cc <- full_join(cc, cc.arb)
  
  ## Skip 8 and 10 for now because missing data
  cc <- full_join(cc, hf1)
  cc <- full_join(cc, hf2)
  cc <- full_join(cc, hf3)
  cc <- full_join(cc, hf4)
  cc <- full_join(cc, hf5)
  cc <- full_join(cc, hf6)
  cc <- full_join(cc, hf7)
  cc <- full_join(cc, hf9)
  cc <- full_join(cc, hf11)
  cc <- full_join(cc, hf12)
  cc <- full_join(cc, hf13)
  cc <- full_join(cc, hf14)
  
  cc <- cc[!duplicated(cc),]
  
  cc$climatetype.hobo <- cc$climatetype
  
  ### Delineate individuals to loggers for Arb
  indslist <- read.csv("~/Documents/git/microclimates/analyses/input/individual_phenometrics_data.csv", header=TRUE)
  indslist <- subset(indslist, select=c("Site_ID", "Individual_ID", "Genus", "Species"))
  indslist$spp <- paste(substr(indslist$Genus, 0, 3), substr(indslist$Species, 0, 3), sep="")
  indslist$Genus <- NULL
  indslist$Species <- NULL
  indslist <- indslist[!duplicated(indslist),]
  names(indslist) <- c("route", "id", "spp")
  indslist$id <- as.character(indslist$id)
  
  indslist$climatetype.hobo <-NA
  
  ## Linden and North Woods Route (and some shrub route and some maple route)
  indslist$climatetype.hobo <- ifelse(indslist$id=="87763" | indslist$id=="166768", "hf1", indslist$climatetype.hobo)
  indslist$climatetype.hobo <- ifelse(indslist$id=="86290" | indslist$id=="87762" | indslist$id=="85758" |
                                        indslist$id=="166769", "hf2", indslist$climatetype.hobo)
  indslist$climatetype.hobo <- ifelse(indslist$id=="85756" | indslist$id=="85755" | indslist$id=="85751" |
                                        indslist$id=="166766" | indslist$id=="166767" |
                                        indslist$id=="166773" | indslist$id=="166774",
                                      "hf4", indslist$climatetype.hobo)
  
  ## Maple Route
  notthree <- c("166766", "166767")
  indslist$climatetype.hobo <- ifelse(indslist$spp=="Acesac" | indslist$spp=="Acerub" & !indslist$id%in%notthree,
                                      "hf3", indslist$climatetype.hobo)
  
  ## Part of Shrub route
  indslist$climatetype.hobo <- ifelse(indslist$id=="166775", "hf5", indslist$climatetype.hobo)
  
  ## Hickory Route (and some shrub route)
  indslist$climatetype.hobo <- ifelse(indslist$spp=="Carova" | indslist$id=="86262" | indslist$id=="169168" | 
                                        indslist$id=="169165" | indslist$id=="169166" | indslist$id=="169167", 
                                      "hf7", indslist$climatetype.hobo)
  indslist$climatetype.hobo <- ifelse(indslist$spp=="Cargla" & indslist$id!="86262", "hf8", indslist$climatetype.hobo)
  indslist$climatetype.hobo <- ifelse(indslist$id=="166776" | indslist$id=="166777" | 
                                        indslist$id=="166778", "hf8", indslist$climatetype.hobo)
  
  ## Oak Route
  indslist$climatetype.hobo <- ifelse(indslist$spp=="Quealb" | indslist$spp=="Querub", "hf9", indslist$climatetype.hobo)
  
  ## Beech Route (and some shrub route)
  indslist$climatetype.hobo <- ifelse(indslist$spp=="Faggra" | indslist$id=="166779" |
                                        indslist$id=="166780", "hf10", indslist$climatetype.hobo)
  
  indslist$climatetype.hobo <- ifelse(indslist$route==20206, "hf7", indslist$climatetype.hobo) ## Change to hf6 next year!
  
  # Peter's Hill
  indslist$climatetype.hobo <- ifelse(indslist$id=="169171" | indslist$id=="86289", "hf11", indslist$climatetype.hobo)
  indslist$climatetype.hobo <- ifelse(indslist$spp=="Popdel", "hf12", indslist$climatetype.hobo)
  indslist$climatetype.hobo <- ifelse(indslist$id=="92209", "hf13", indslist$climatetype.hobo)
  indslist$climatetype.hobo <- ifelse(indslist$id=="87756", "hf14", indslist$climatetype.hobo)
  indslist$climatetype.hobo <- ifelse(indslist$id=="87757" | indslist$id=="85757", "hf15", indslist$climatetype.hobo)
  
  indslist$route <-NULL
  indslist$spp <-NULL
  
  ### Delineate individuals to loggers for HF
  hflist <- subset(d, select=c("id", "genus", "species", "type"))
  hflist <- hflist[(hflist$type=="Harvard Forest"),]
  hflist <- hflist[!duplicated(hflist),]
  hflist$climatetype.hobo <- NA
  
  ## Hobo 1:
  hflist$climatetype.hobo <- ifelse(hflist$id=="ACSA-02" | hflist$id=="ARSP-03" |
                                      hflist$id=="FAGR-04" | hflist$id=="FRAM-03" |
                                      hflist$id=="ILVE-02" | hflist$id=="POTR-04" |
                                      hflist$id=="QUAL-03" | hflist$id=="SAPU-02", "hf1", hflist$climatetype.hobo)
  ## Hobo 2:
  hflist$climatetype.hobo <- ifelse(hflist$id=="BEPA-03" | hflist$id=="BEPA-04" |
                                      hflist$id=="COAL-03" | hflist$id=="FRAM-02", "hf2", hflist$climatetype.hobo)
  
  ## Hobo 3:
  hflist$climatetype.hobo <- ifelse(hflist$id=="BEPA-02" | hflist$id=="CRSP-02" |
                                      hflist$id=="PRSE-02" | hflist$id=="QURU-03", "hf3", hflist$climatetype.hobo)
  
  ## Hobo 4:
  hflist$climatetype.hobo <- ifelse(hflist$id=="HAVI-03" | hflist$id=="QUVE-03" |
                                      hflist$id=="QUVE-04" | hflist$id=="TSCA-05", "hf4", hflist$climatetype.hobo)
  
  ## Hobo 5: 
  hf5s <- c("ACRU-04", "BELE-03", "CRSP-03", "KAAN-03", "NEMU-03", "QUVE-02", "RHSP-03")
  
  ## Hobo 6:
  hf6s <- c("AMSP-03", "BEAL-03", "CADE-04", "FAGR-03", "ILVE-04", "KAAN-02", "KALA-02", 
            "NEMU-01", "NEMU-02", "NYSY-03", "TSCA-04", "VACO-03")
  
  ## Hobo 7:
  hf7s <- c("ACPE-04", "ACRU-03", "ARSP-01", "HAVI-02", "QUAL-02", "QURU-02", "QUVE-02", "VACO-04",
            "VIAL-03")
  
  ## Hobo 8: (to change to 9 for now)
  hf8s <- c("ACPE-03", "BEAL-02", "BELE-02", "HAVI-01", "VIAL-02", "VICA-03")
  
  ## Hobo 9:
  hf9s <- c("ACPE-02", "ACRU-02", "BEPA-01", "BEPO-01", "CADE-01", "CRSP-01", "KAAN-01",
            "KALA-01", "LYLI-01", "LYLI-03", "NYSY-04", "PIST-02", "RHSP-01")
  
  ## Hobo 10: (to change to 9 for now)
  
  d$climatetype.hobo <- NA
  d$climatetype.hobo <- ifelse(d$type=="Common Garden", "weldhill", d$climatetype.hobo)
  d$climatetype.hobo <- as.character(d$climatetype.hobo)
  d <- full_join(d, indslist)
  
  d$id_year_type <- paste(d$id, d$year, d$climatetype.hobo, sep=";")
  
  
} else {
  print("Error: d not a data.frame")
}

stop("Not an error, climate is now added to dataframe.")