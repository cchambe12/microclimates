### 24 January 2019 - Cat
## Clean common garden data 

# Load from bb_cleanmergeall.R (including libraries)

if(is.data.frame(cg)){
  
  cg <- cg %>% rename(ID = Ind)

  cg <- separate(data = cg, col = ID, into = c("spp", "site", "ind", "plot"), sep = "\\_")
  
  ## fix warning
  cg[is.na(cg$plot), c("ind", "plot")] <- cg[is.na(cg$plot), c("plot", "ind")] 

  ### Let's clean up species names now
  cg$Species<- NA
  cg$Genus<- NA
  
  cg$Genus<-ifelse(cg$spp=="ACEPEN", "Acer", cg$Genus)
  cg$Species<-ifelse(cg$spp=="ACEPEN", "pensylvanicum", cg$Species)
  cg$Genus<-ifelse(cg$spp=="ACESPI", "Acer", cg$Genus)
  cg$Species<-ifelse(cg$spp=="ACESPI", "spicatum", cg$Species)
  cg$Genus<-ifelse(cg$spp=="ALNINC", "Alnus", cg$Genus)
  cg$Species<-ifelse(cg$spp=="ALNINC", "incana", cg$Species)
  cg$Genus<-ifelse(cg$spp=="AMECAN", "Amelanchier", cg$Genus)
  cg$Species<-ifelse(cg$spp=="AMECAN", "canadensis", cg$Species)
  cg$Genus<-ifelse(cg$spp=="AROMEL", "Aronia", cg$Genus)
  cg$Species<-ifelse(cg$spp=="AROMEL", "melonacarpa", cg$Species)
  cg$Genus<-ifelse(cg$spp=="BETALL", "Betula", cg$Genus)
  cg$Species<-ifelse(cg$spp=="BETALL", "alleghaniensis", cg$Species)
  cg$Genus<-ifelse(cg$spp=="BETPAP", "Betula", cg$Genus)
  cg$Species<-ifelse(cg$spp=="BETPAP", "papyrifera", cg$Species)
  cg$Genus<-ifelse(cg$spp=="BETPOP", "Betula", cg$Genus)
  cg$Species<-ifelse(cg$spp=="BETPOP", "populifolia", cg$Species)
  cg$Genus<-ifelse(cg$spp=="BETPOPX", "Betula", cg$Genus)
  cg$Species<-ifelse(cg$spp=="BETPOPX", "populifolia", cg$Species)
  cg$Genus<-ifelse(cg$spp=="DIELON", "Diervilla", cg$Genus)
  cg$Species<-ifelse(cg$spp=="DIELON", "lonicera", cg$Species)
  cg$Genus<-ifelse(cg$spp=="MYRGAL", "Myrica", cg$Genus)
  cg$Species<-ifelse(cg$spp=="MYRGAL", "gale", cg$Species)
  cg$Genus<-ifelse(cg$spp=="QUERUB", "Quercus", cg$Genus)
  cg$Species<-ifelse(cg$spp=="QUERUB", "rubra", cg$Species)
  cg$Genus<-ifelse(cg$spp=="SAMRAC", "Sambucus", cg$Genus)
  cg$Species<-ifelse(cg$spp=="SAMRAC", "racemosa", cg$Species)
  cg$Genus<-ifelse(cg$spp=="SORAME", "Sorbus", cg$Genus)
  cg$Species<-ifelse(cg$spp=="SORAME", "americana", cg$Species)
  cg$Genus<-ifelse(cg$spp=="SPIALB", "Spiraea", cg$Genus)
  cg$Species<-ifelse(cg$spp=="SPIALB", "alba", cg$Species)
  cg$Genus<-ifelse(cg$spp=="SPITOM", "Spiraea", cg$Genus)
  cg$Species<-ifelse(cg$spp=="SPITOM", "tomentosa", cg$Species)
  cg$Genus<-ifelse(cg$spp=="VACMYR", "Vaccinium", cg$Genus)
  cg$Species<-ifelse(cg$spp=="VACMYR", "myrtilloides", cg$Species)
  cg$Genus<-ifelse(cg$spp=="VIBCAS", "Viburnum", cg$Genus)
  cg$Species<-ifelse(cg$spp=="VIBCAS", "cassinoides", cg$Species)
  
  
  cg$year <- 2018
  cg$last.obs <- 274
  cg$gdd.start <- 46
  cg$yr.end <- 365
  cg$type <- "Common Garden"
  cg$ID <- paste(cg$spp, cg$site, cg$ind, cg$plot, sep="_")
  
  
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

  
  
  



