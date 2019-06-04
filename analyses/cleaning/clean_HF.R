### 24 January 2019 - Cat
## Clean John O'Keefe data 

# Load from bb_cleanmergeall.R (including libraries)

if(is.data.frame(jok)){
  
  jok.bb<-jok%>%
    dplyr::select(JULIAN, TREEID, BBRK, year) %>%
    rename(doy=JULIAN)%>%
    rename(id=TREEID)%>%
    rename(budburst=BBRK)
  jok.bb$budburst<-ifelse(jok.bb$budburst>=75, jok.bb$budburst, NA)
  jok.bb<-jok.bb[!is.na(jok.bb$budburst),]
  
  jok.bb<-jok.bb %>% group_by(id, year) %>% filter(row_number(budburst) == 1)                   
  jok.bb$budburst<-jok.bb$doy
  jok.bb$doy <- NULL
  
  jok.lo<-jok%>%
    dplyr::select(JULIAN, TREEID, L95, year) %>%
    rename(doy=JULIAN)%>%
    rename(id=TREEID)%>%
    rename(leafout=L95)
  jok.lo$leafout<-ifelse(jok.lo$leafout>=50, jok.lo$leafout, NA)
  jok.lo<-jok.lo[!is.na(jok.lo$leafout),]
  
  jok.lo<-jok.lo %>% group_by(id, year) %>% filter(row_number(leafout) == 1)                   
  jok.lo$leafout<-jok.lo$doy
  jok.lo$doy <- NULL
  
  jok.pheno<-full_join(jok.bb, jok.lo)
  jok.pheno$spp<-substr(jok.pheno$id, 0, 4)
  
  jok.pheno$genus <- NA
  jok.pheno$species <- NA
  jok.pheno$genus<-ifelse(jok.pheno$spp=="ACPE", "Acer", jok.pheno$genus)
  jok.pheno$species<-ifelse(jok.pheno$spp=="ACPE", "pensylvanicum", jok.pheno$species)
  jok.pheno$genus<-ifelse(jok.pheno$spp=="ACRU", "Acer", jok.pheno$genus)
  jok.pheno$species<-ifelse(jok.pheno$spp=="ACRU", "rubra", jok.pheno$species)
  jok.pheno$genus<-ifelse(jok.pheno$spp=="ACSA", "Acer", jok.pheno$genus)
  jok.pheno$species<-ifelse(jok.pheno$spp=="ACSA", "saccharum", jok.pheno$species)
  jok.pheno$genus<-ifelse(jok.pheno$spp=="AMSP", "Amelanchier", jok.pheno$genus)
  jok.pheno$species<-ifelse(jok.pheno$spp=="AMSP", "canadensis", jok.pheno$species)
  jok.pheno$genus<-ifelse(jok.pheno$spp=="BEAL", "Betula", jok.pheno$genus)
  jok.pheno$species<-ifelse(jok.pheno$spp=="BEAL", "alleghaniensis", jok.pheno$species)
  jok.pheno$genus<-ifelse(jok.pheno$spp=="BELE", "Betula", jok.pheno$genus)
  jok.pheno$species<-ifelse(jok.pheno$spp=="BELE", "lenta", jok.pheno$species)
  jok.pheno$genus<-ifelse(jok.pheno$spp=="BEPA", "Betula", jok.pheno$genus)
  jok.pheno$species<-ifelse(jok.pheno$spp=="BEPA", "papyrifera", jok.pheno$species)
  jok.pheno$genus<-ifelse(jok.pheno$spp=="COAL", "Cornus", jok.pheno$genus)
  jok.pheno$species<-ifelse(jok.pheno$spp=="COAL", "alternifolia", jok.pheno$species)
  jok.pheno$genus<-ifelse(jok.pheno$spp=="CRSP", "Crataegus", jok.pheno$genus)
  jok.pheno$species<-ifelse(jok.pheno$spp=="CRSP", "sp.", jok.pheno$species)
  jok.pheno$genus<-ifelse(jok.pheno$spp=="FAGR", "Fagus", jok.pheno$genus)
  jok.pheno$species<-ifelse(jok.pheno$spp=="FAGR", "grandifolia", jok.pheno$species)
  jok.pheno$genus<-ifelse(jok.pheno$spp=="FRAM", "Fraxinus", jok.pheno$genus)
  jok.pheno$species<-ifelse(jok.pheno$spp=="FRAM", "americana", jok.pheno$species)
  jok.pheno$genus<-ifelse(jok.pheno$spp=="HAVI", "Hamamelis", jok.pheno$genus)
  jok.pheno$species<-ifelse(jok.pheno$spp=="HAVI", "virginiana", jok.pheno$species)
  jok.pheno$genus<-ifelse(jok.pheno$spp=="POTR", "Populus", jok.pheno$genus)
  jok.pheno$species<-ifelse(jok.pheno$spp=="POTR", "tremuloides", jok.pheno$species)
  jok.pheno$genus<-ifelse(jok.pheno$spp=="PRSE", "Prunus", jok.pheno$genus)
  jok.pheno$species<-ifelse(jok.pheno$spp=="PRSE", "serotina", jok.pheno$species)
  jok.pheno$genus<-ifelse(jok.pheno$spp=="QUAL", "Quercus", jok.pheno$genus)
  jok.pheno$species<-ifelse(jok.pheno$spp=="QUAL", "alba", jok.pheno$species)
  jok.pheno$genus<-ifelse(jok.pheno$spp=="QURU", "Quercus", jok.pheno$genus)
  jok.pheno$species<-ifelse(jok.pheno$spp=="QURU", "rubra", jok.pheno$species)
  jok.pheno$genus<-ifelse(jok.pheno$spp=="QUVE", "Quercus", jok.pheno$genus)
  jok.pheno$species<-ifelse(jok.pheno$spp=="QUVE", "velutina", jok.pheno$species)
  
  jok.pheno$type<-"Harvard Forest"
  
  ### Now need to get last observation date...
  fall15 <- read.csv("output/okeefe_fall2015.csv", header=TRUE)
  fall15$year <-2015
  fall16 <- read.csv("output/okeefe_fall2016.csv", header=TRUE)
  fall16$year <- 2016
  fall16$LF.COLOR <- as.integer(fall16$LF.COLOR)
  fall16$LF.FALL <- as.integer(fall16$LF.FALL)
  fall17 <- read.csv("output/okeefe_fall2017.csv", header=TRUE)
  fall17$year <- 2017
  fall18 <- read.csv("output/okeefe_fall2018.csv", header=TRUE)
  fall18$year <- 2018
  
  fall <- full_join(fall15, fall16)
  fall <- full_join(fall, fall17)
  fall <- full_join(fall, fall18)
  
  fall<-fall%>%
    dplyr::select(DATE, TREE.ID, LF.COLOR, year) %>%
    rename(date=DATE)%>%
    rename(id=TREE.ID)%>%
    rename(last.obs=LF.COLOR)
  fall$last.obs<-ifelse(fall$last.obs>=75, fall$last.obs, NA)
  fall<-fall[!is.na(fall$last.obs),]
  
  fall<-fall %>% group_by(id, year) %>% filter(row_number(last.obs) == 1)                   
  fall$doy <- yday(as.Date(fall$date, "%m/%d/%y"))
  fall$last.obs<-fall$doy
  
  fall <- subset(fall, select=c("id", "last.obs", "year"))
  
  hf <- full_join(jok.pheno, fall)
  hf$last.obs <- ifelse(is.na(hf$last.obs) & hf$year==2016, 274, hf$last.obs)
  hf$last.obs <- ifelse(is.na(hf$last.obs) & hf$year!=2016, 275, hf$last.obs)
  hf <- hf[!is.na(hf$genus),]
  
  hf$gdd.start <- 46
  hf$yr.end <- ifelse(hf$year==2016, 366, 365)
  hf$provenance.lat <- 42.531705
  hf$provenance.long <- -72.189920
  
  hf <- subset(hf, select=colstokeep)
  
  d <- full_join(d, hf)
  
} else {
  print("Error: hf not a data.frame")
}

stop("Not an error, Harvard Forest data is clean. Also, you can ignore the warning message below -- code converts a column to character, but the column is created in a different dataframe that is used and deleted in this source code and should not (that I can imagine) have any other impact.")

  
  
  
  