### 24 January 2019 - Cat
## Clean Treespotters data 

## How to download data:
# 1) Go to the NPN Data Downloader Tool: https://data.usanpn.org/observations/get-started
    # And go to the Phenology Observation Portal
# 2) Select 'Individual Phenometrics' and press NEXT 
# 3) Set Date range applicable to your question and press 'Set Date' and NEXT
# 4) Select 'Partner Groups' tab on left: press the + next to 
    # 'Botanic Gardens and Arboretums and select 'Arnold Arboretum - Tree Spotters'
    # Press 'Set Groups' and NEXT
# 5) Select 'Output fields' tab on left: and select 'ObservedBy Person ID' and 'Multiple Observers'


# Load from bb_cleanmergeall.R (including libraries)

if(is.data.frame(d)){

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
  rename(ID=Individual_ID)
bb.pheno<-dplyr::select(bb, Genus, Species, Common_Name, phase, lat, long, elev, year, doy, numYs, ID)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Breaking leaf buds", "budburst", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Leaves", "leafout", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Flowers or flower buds", "flowers", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Falling leaves", "leaf drop", bb.pheno$phase)


### Now work on finding day of budburst, etc.
bb.pheno<-filter(bb.pheno, numYs>0)
doy_pheno<-bb.pheno%>% 
  group_by(ID, phase, year) %>% 
  slice(which.min(doy))
doy_pheno$m.doy<-ave(doy_pheno$doy, doy_pheno$phase, doy_pheno$Genus, doy_pheno$year)
doy_pheno<-doy_pheno[!duplicated(doy_pheno),]

doy_pheno$photo<-as.numeric(geosphere::daylength(doy_pheno$lat, doy_pheno$doy))

### Clean observation error!
doy_pheno$doy<-ifelse(doy_pheno$Species=="alleghaniensis" & doy_pheno$year==2016 & doy_pheno$doy==59, NA, doy_pheno$doy)
doy_pheno<-doy_pheno[!is.na(doy_pheno$doy),]

#### Now start building a small data frame with phenophase info then add in climandpheno, chilling and photo
colstokeep<-c("Genus", "Species", "ID","year", "phase","lat", "long", "elev", "doy")
phenos<-subset(doy_pheno, select=colstokeep)

phenos<-phenos[!duplicated(phenos),]

phenos<-phenos%>%tidyr::spread(phase, doy)
phenos$budburst<-ave(phenos$budburst, phenos$ID, phenos$year, FUN=first)
phenos$leafout<-ave(phenos$leafout, phenos$ID, phenos$year, FUN=first)
phenos$flowers<-ave(phenos$flowers, phenos$ID, phenos$year, FUN=first)
phenos$fruits<-ave(phenos$Fruits, phenos$ID, phenos$year, FUN=first)
phenos$col.leaves<-ave(phenos$`Colored leaves`, phenos$ID, phenos$year, FUN=first)
phenos$leafdrop<-ave(phenos$`leaf drop`, phenos$ID, phenos$year, FUN=first)

### And now last observation for when to start calculating chilling
phenos$last.obs<-ave(phenos$`leaf drop`, phenos$ID, phenos$year, FUN=last)
phenos$last.obs<-ifelse(is.na(phenos$last.obs), ave(phenos$col.leaves, phenos$ID, phenos$year, FUN=last), phenos$last.obs)

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
prov <- prov %>% rename(ID=Individual_ID)

ts <- full_join(prov, phenos)

colstokeep <- c("ID", "provenance.lat", "provenance.long", "Genus", "Species", "year", "budburst", "leafout", "last.obs", "yr.end", "gdd.start", "type")
d <- subset(ts, select=colstokeep)
d <- d[!is.na(d$Genus),]

d$ID <- as.character(d$ID)

} else {
  print("Error: ts not a data.frame")
}

stop("Not an error, treespotters data is clean. Also, you can ignore the warning messages below -- is due to a bug in package (https://stackoverflow.com/questions/24282550/no-non-missing-arguments-warning-when-using-min-or-max-in-reshape2). 
     I have checked and rechecked the data")





