### Thinking about flowers and fruits... 

rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Load Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(rstanarm)
library(brms)

# Set Working Directory
setwd("~/Documents/git/microclimates/analyses")

## Data!
d<-read.csv("input/individual_phenometrics_data.csv", header=TRUE)
prov <- read.csv("input/provenanceinfo.")

#### Alright so the questions I have... 
# a) pollination syndrome and fruit type, how do they influence the time between flowering and fruit development?
# b) provenance? with species level effects
# c) climate stuffs... need to think about how to best think about this model
# d) DVR!! see if the time to develop leaves and how much PS impacts flowering to fruiting

### first determine the phenophases I'm most interested in...
sort(unique(d$Phenophase_Description))
#[1] "Breaking leaf buds"        "Colored leaves"            "Falling leaves"            "Flowers or flower buds"   
#[5] "Fruits"                    "Increasing leaf size"      "Leaves"                    "Open flowers"             
#[9] "Pollen release (flowers)"  "Recent fruit or seed drop" "Ripe fruits" 

## Let's start with Flowers or flower buds to Open flowers time, should we have budburst as another predictor?
ts <- subset(d, select=c("Genus", "Species", "Individual_ID", "Phenophase_Description", "First_Yes_Year",
                         "First_Yes_DOY"))
names(ts) <- c("genus", "species", "id", "phenophase", "year", "doy")

phenos <- c("Flowers or flower buds", "Open flowers", "Breaking leaf buds", "Leaves", "Fruits", "Ripe fruits")
ts <- ts[(ts$phenophase%in%phenos),]

ts$phenophase <- ifelse(ts$phenophase=="Flowers or flower buds", "flobudburst", ts$phenophase)
ts$phenophase <- ifelse(ts$phenophase=="Open flowers", "flowers", ts$phenophase)
ts$phenophase <- ifelse(ts$phenophase=="Breaking leaf buds", "budburst", ts$phenophase)
ts$phenophase <- ifelse(ts$phenophase=="Leaves", "leaves", ts$phenophase)
ts$phenophase <- ifelse(ts$phenophase=="Fruits", "fruits", ts$phenophase)
ts$phenophase <- ifelse(ts$phenophase=="Ripe fruits", "ripefruits", ts$phenophase)

ts <- ts[(ts$year>=2016),]

ts$budburst <- ifelse(ts$phenophase=="budburst", ave(ts$doy, ts$id, ts$year, FUN=first), NA)
ts$leaves <- ifelse(ts$phenophase=="leaves", ave(ts$doy, ts$id, ts$year, FUN=first), NA)
ts$flobudburst <- ifelse(ts$phenophase=="flobudburst", ave(ts$doy, ts$id, ts$year, FUN=first), NA)
ts$flowers <- ifelse(ts$phenophase=="flowers", ave(ts$doy, ts$id, ts$year, FUN=first), NA)
ts$fruits <- ifelse(ts$phenophase=="fruits", ave(ts$doy, ts$id, ts$year, FUN=first), NA)
ts$ripefruits <- ifelse(ts$phenophase=="ripefruits", ave(ts$doy, ts$id, ts$year, FUN=first), NA)

ts.stan <- subset(ts, select=c("genus", "species", "id", "year", "budburst", "leaves", "flobudburst", 
                               "flowers", "fruits", "ripefruits"))

ts.stan <- ts.stan[!duplicated(ts.stan),]

ts.stan <- ts.stan %>% 
  group_by(genus, species, id, year) %>% 
  summarise_all(funs(first(na.omit(.))))



