### 24 January 2019 - Cat
## Clean Treespotters data


## How to download data:
# 1) Go to the NPN Data Downloader Tool: https://data.usanpn.org/observations
# And go to the Phenology Observation Portal
# 2) Select 'Individual Phenometrics' and press NEXT 
# 3) Set Date range applicable to your question and press 'Set Date' and NEXT
# 4) Select 'Partner Groups' tab on left: press the + next to 
# 'Botanic Gardens and Arboretums and select 'Arnold Arboretum - Tree Spotters'
# Press 'Set Groups' and NEXT
# 5) Select 'Output fields' tab on left: and select 'ObservedBy Person ID' and 'Multiple Observers'


# Load from bb_cleanmergeall.R (including libraries)

if(is.data.frame(d)){
  
  ### First let's do the obligatory cleaning checks with citizen scienece data
  d <- d[(d$Multiple_FirstY>=1 | d$Multiple_Observers>0),]
  d <- d[(d$NumYs_in_Series>=3),]
  d <- d[(d$NumDays_Since_Prior_No<=7),]
  
  # change from NPN output to more digestible column names
  bb<-d%>%
    rename(lat=Latitude)%>%
    rename(long=Longitude)%>%
    rename(elev=Elevation_in_Meters)%>%
    rename(year=First_Yes_Year)%>%
    rename(month=First_Yes_Month)%>%
    rename(day=First_Yes_Day)%>%
    rename(doy=First_Yes_DOY)%>%
    rename(numYs=Multiple_Observers)%>%
    rename(phase=Phenophase_Description)%>%
    rename(id=Individual_ID)%>%
    rename(genus=Genus)%>%
    rename(species=Species)
  bb.pheno<-dplyr::select(bb, genus, species, Common_Name, phase, lat, long, elev, year, doy, numYs, id)
  bb.pheno$phase<-ifelse(bb.pheno$phase=="Breaking leaf buds", "budburst", bb.pheno$phase)
  bb.pheno$phase<-ifelse(bb.pheno$phase=="Leaves", "leafout", bb.pheno$phase)
  bb.pheno$phase<-ifelse(bb.pheno$phase=="Flowers or flower buds", "flowers", bb.pheno$phase)
  bb.pheno$phase<-ifelse(bb.pheno$phase=="Falling leaves", "leaf drop", bb.pheno$phase)
  
  
  ### Now work on finding day of budburst, etc.
  bb.pheno<-filter(bb.pheno, numYs>0)
  # Below, I group each individual by phenophase and year to find the first observation (using the slice function), 
  ## so first day of budburst for that individual for that year
  doy_pheno<-bb.pheno%>% 
    group_by(id, phase, year) %>% 
    slice(which.min(doy))
  doy_pheno<-doy_pheno[!duplicated(doy_pheno),]
  
  ### Clean observation error!
  # QbyLizzie: How did you find this? - I found this by making initial raw data plots. There was a outlier and I went back 
  ## to check the data. It was a new volunteer who made a couple of mistakes. 
  doy_pheno$doy<-ifelse(doy_pheno$species=="alleghaniensis" & doy_pheno$year==2016 & doy_pheno$doy==59, NA, doy_pheno$doy)
  doy_pheno<-doy_pheno[!is.na(doy_pheno$doy),]
  
  #### Now start building a small data frame with phenophase info then add in climandpheno, chilling and photo
  colstokeep<-c("genus", "species", "id","year", "phase","lat", "long", "elev", "doy")
  phenos<-subset(doy_pheno, select=colstokeep)
  
  phenos<-phenos[!duplicated(phenos),]
  
  phenos<-phenos%>%tidyr::spread(phase, doy)
  
  phenos$fruits <- phenos$Fruits
  phenos$col.leaves<-phenos$`Colored leaves`
  phenos$leafdrop<-phenos$`leaf drop`

  phenos <- subset(phenos, select=c("genus", "species", "id", "year", "lat", "long", "elev", "budburst", 
                                    "flowers", "fruits", "leafout", "col.leaves", "leafdrop"))
    
  ### And now last observation for when to start calculating chilling
  phenos$last.obs<-ave(phenos$leafdrop, phenos$id, phenos$year, FUN=last)
  phenos$last.obs<-ifelse(is.na(phenos$last.obs), ave(phenos$col.leaves, phenos$id, phenos$year, FUN=last), phenos$last.obs)
  
  ## For gdd start and chilling end
  phenos$gdd.start<-46 # 15 February for each year - arbitrary, can change
  
  # write.csv(phenos, file="output/clean_treespotters_allphenodata.csv", row.names=FALSE)
  
  
  ### Now clean it up a bit
  phenos<-phenos[!is.na(phenos$budburst),]
  phenos<-phenos[!is.na(phenos$leafout),]
  phenos<-phenos[!is.na(phenos$last.obs),]
  
  phenos$yr.end <- ifelse(phenos$year==2016, 366, 365)
  
  ## And now add in individual information...
  phenos$type <- "Treespotters"
  
  prov <- read.csv("output/provenanceinfo.csv", header=TRUE)
  prov <- subset(prov, select= c("Individual_ID", "provenance.lat", "provenance.long"))
  prov <- prov %>% rename(id=Individual_ID)
  
  ts <- full_join(prov, phenos)
  
  colstokeep <- c("id", "provenance.lat", "provenance.long", "genus", "species", "year", "budburst", "leafout", "yr.end", "gdd.start", "type")
  d <- subset(ts, select=colstokeep)
  d <- d[!is.na(d$genus),]
  
  d$id <- as.character(d$id)
  
} else {
  print("Error: ts not a data.frame")
}

stop("Not an error, treespotters data is clean.")





