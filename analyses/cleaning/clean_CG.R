### 24 January 2019 - Cat
## Clean common garden data 

# Load from bb_cleanmergeall.R (including libraries)

if(is.data.frame(d)){
  
  ### first add in 2017 data
  cg17 <- read.csv("input/cg_datasheet_2017.csv", header=TRUE)
  cg17 <- cg17[(cg17$BBCH==9 |cg17$BBCH==10 | cg17$BBCH==19),]
  cg17 <- subset(cg17, select=c("Date", "ID", "Plot", "BBCH"))
  names(cg17) <- c("date", "ind", "plot", "phenophase")
  
  cg17$doy <- yday(as.Date(cg17$date, "%Y-%m-%d"))

  cg17 <- cg17[!is.na(cg17$ind),]
  cg17 <- separate(data = cg17, col = ind, into = c("spp", "site", "ind"), sep = "\\_")
  cg17$ind <- ifelse(is.na(cg17$ind), substr(cg17$spp, 7,8), cg17$ind)
  cg17$ind <- ifelse(cg17$ind=="", "XX", cg17$ind)
  cg17$spp <- substr(cg17$spp, 0, 6)
  cg17$year <- 2017
  
  cg17$phenophase <- ifelse(cg17$phenophase==10, 9, cg17$phenophase)
  
  cg17 <-cg17%>% 
    group_by(spp, site, ind, plot, phenophase, year) %>% 
    slice(which.min(doy))
  cg17<-cg17[!duplicated(cg17),]
  
  cg17$budburst <- ifelse(cg17$phenophase==9, cg17$doy, NA)
  cg17$leafout <- ifelse(cg17$phenophase==19, cg17$doy, NA)
  
  cg17 <- subset(cg17, select=c("spp", "year", "site", "ind", "budburst", "leafout", "plot"))
  cg17$plot <- as.character(cg17$plot)
  
  cg17 <- cg17[!duplicated(cg17),]
  
  cg17 <- cg17 %>% 
    group_by(spp, site, ind, plot, year) %>% 
    summarise_all(list(~first(na.omit(.))))
  cg17$risk <- cg17$leafout - cg17$budburst
  
  
  # 3. Get 2018 Common Garden data - and clean!
  cg18 <- read.csv("output/dvr_cg_2018.csv", header=TRUE) ## from CGtraits repo https://github.com/cchambe12/CGtraits
  cg18 <- cg18 %>% rename(id = Ind)

  cg18 <- separate(data = cg18, col = id, into = c("spp", "site", "ind", "plot"), sep = "\\_")
  
  ## fix warning
  cg18[is.na(cg18$plot), c("ind", "plot")] <- cg18[is.na(cg18$plot), c("plot", "ind")] 
  
  cg18$year <- 2018
  #cg18$last.obs <- 274
  cg18$Plot <- NULL
  
  
  ## Now let's clean 2019 data
  cg19 <- read.csv("input/2019_CG_datasheet.csv", header=TRUE)
  
  cg19$id <- paste(cg19$Ind, cg19$Plot, sep="_")
  cg19$Ind<-NULL
  cg19$Plot<-NULL
  cg19 <- gather(cg19, "date", "bbch", -id, -Phase)
  cg19 <- na.omit(cg19)
  cg19 <- cg19[!(cg19$bbch==""),]
  
  cg19$date <- gsub("X", "", cg19$date)
  cg19$date <- as.Date(cg19$date, format="%m.%d.%Y")
  cg19$doy <- yday(cg19$date)
  
  cg19 <- cg19[(cg19$Phase=="Leaves"),]
  
  cg19 <- subset(cg19, select=c("id", "doy", "bbch"))
  cg19 <- separate(data = cg19, col = id, into = c("spp", "site", "ind", "plot"), sep = "\\_")
  cg19$ind <- ifelse(is.na(cg19$ind), substr(cg19$spp, 7,8), cg19$ind)
  cg19$ind <- ifelse(cg19$ind=="", "XX", cg19$ind)
  cg19$spp <- substr(cg19$spp, 0, 6)
  cg19$year <- 2019
  
  cg19$bbch <- ifelse(cg19$bbch==10, 9, cg19$bbch)
  
  cg19 <-cg19%>% 
    group_by(spp, site, ind, plot, bbch, year) %>% 
    slice(which.min(doy))
  cg19<-cg19[!duplicated(cg19),]
  
  cg19$budburst <- ifelse(cg19$bbch==9, cg19$doy, NA)
  cg19$leafout <- ifelse(cg19$bbch==19, cg19$doy, NA)
  
  cg19 <- subset(cg19, select=c("spp", "year", "site", "ind", "budburst", "leafout", "plot"))
  cg19$plot <- as.character(cg19$plot)
  
  cg19 <- cg19[!duplicated(cg19),]
  
  cg19 <- cg19 %>% 
    group_by(spp, site, ind, plot, year) %>% 
    summarise_all(list(~first(na.omit(.))))
  cg19$risk <- cg19$leafout - cg19$budburst
  
  ## Let's combine the years
  cg <- full_join(cg17, cg18)
  cg <- full_join(cg, cg19)

  ### Let's clean up species names now
  cg$species<- NA
  cg$genus<- NA
  
  cg$genus<-ifelse(cg$spp=="ACEPEN", "Acer", cg$genus)
  cg$species<-ifelse(cg$spp=="ACEPEN", "pensylvanicum", cg$species)
  cg$genus<-ifelse(cg$spp=="ACESPI", "Acer", cg$genus)
  cg$species<-ifelse(cg$spp=="ACESPI", "spicatum", cg$species)
  cg$genus<-ifelse(cg$spp=="ALNINC", "Alnus", cg$genus)
  cg$species<-ifelse(cg$spp=="ALNINC", "incana", cg$species)
  cg$genus<-ifelse(cg$spp=="AMECAN", "Amelanchier", cg$genus)
  cg$species<-ifelse(cg$spp=="AMECAN", "canadensis", cg$species)
  cg$genus<-ifelse(cg$spp=="AROMEL", "Aronia", cg$genus)
  cg$species<-ifelse(cg$spp=="AROMEL", "melonacarpa", cg$species)
  cg$genus<-ifelse(cg$spp=="BETALL", "Betula", cg$genus)
  cg$species<-ifelse(cg$spp=="BETALL", "alleghaniensis", cg$species)
  cg$genus<-ifelse(cg$spp=="BETPAP", "Betula", cg$genus)
  cg$species<-ifelse(cg$spp=="BETPAP", "papyrifera", cg$species)
  cg$genus<-ifelse(cg$spp=="BETPOP", "Betula", cg$genus)
  cg$species<-ifelse(cg$spp=="BETPOP", "populifolia", cg$species)
  cg$genus<-ifelse(cg$spp=="BETPOPX", "Betula", cg$genus)
  cg$species<-ifelse(cg$spp=="BETPOPX", "populifolia", cg$species)
  cg$genus<-ifelse(cg$spp=="DIELON", "Diervilla", cg$genus)
  cg$species<-ifelse(cg$spp=="DIELON", "lonicera", cg$species)
  cg$genus<-ifelse(cg$spp=="MYRGAL", "Myrica", cg$genus)
  cg$species<-ifelse(cg$spp=="MYRGAL", "gale", cg$species)
  cg$genus<-ifelse(cg$spp=="QUERUB", "Quercus", cg$genus)
  cg$species<-ifelse(cg$spp=="QUERUB", "rubra", cg$species)
  cg$genus<-ifelse(cg$spp=="SAMRAC", "Sambucus", cg$genus)
  cg$species<-ifelse(cg$spp=="SAMRAC", "racemosa", cg$species)
  cg$genus<-ifelse(cg$spp=="SORAME", "Sorbus", cg$genus)
  cg$species<-ifelse(cg$spp=="SORAME", "americana", cg$species)
  cg$genus<-ifelse(cg$spp=="SPIALB", "Spiraea", cg$genus)
  cg$species<-ifelse(cg$spp=="SPIALB", "alba", cg$species)
  cg$genus<-ifelse(cg$spp=="SPITOM", "Spiraea", cg$genus)
  cg$species<-ifelse(cg$spp=="SPITOM", "tomentosa", cg$species)
  cg$genus<-ifelse(cg$spp=="VACMYR", "Vaccinium", cg$genus)
  cg$species<-ifelse(cg$spp=="VACMYR", "myrtilloides", cg$species)
  cg$genus<-ifelse(cg$spp=="VIBCAS", "Viburnum", cg$genus)
  cg$species<-ifelse(cg$spp=="VIBCAS", "cassinoides", cg$species)
  
  cg$gdd.start <- 46
  cg$yr.end <- 365
  cg$type <- "Common Garden"
  cg$id <- paste(cg$spp, cg$site, cg$ind, cg$plot, sep="_")
  
  
  ## And finally provenance
  cg$provenance.lat <- NA
  cg$provenance.long <- NA
  
  cg$provenance.lat <- ifelse(cg$site == "HF", 42.531705, cg$provenance.lat)
  cg$provenance.long <- ifelse(cg$site == "HF", -72.189920, cg$provenance.long)
  cg$provenance.lat <- ifelse(cg$site == "WM", 44.112337, cg$provenance.lat)
  cg$provenance.long <- ifelse(cg$site == "WM", -71.230138, cg$provenance.long)
  cg$provenance.lat <- ifelse(cg$site == "GR", 44.794942, cg$provenance.lat)
  cg$provenance.long <- ifelse(cg$site == "GR", -71.146683, cg$provenance.long)
  cg$provenance.lat <- ifelse(cg$site == "SH", 45.932675, cg$provenance.lat)
  cg$provenance.long <- ifelse(cg$site == "SH", -74.025070, cg$provenance.long)
  
  cg <- subset(cg, select=colstokeep)
  
  
  d <- full_join(d, cg)
  
} else {
  print("Error: cg not a data.frame")
}

stop("Not an error, common garden data is clean. Also, you can ignore the warning messages below -- it is addressed and fixed in the code.")

  
  
  



