### Thinking about flowers and fruits... 
# Started 3 June 2019 - Cat

rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Load Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(rstan)
library(brms)
library(lubridate)
library(anytime)
library(weathermetrics)
library(measurements)

# Set Working Directory
setwd("~/Documents/git/microclimates/analyses")

## Data!
d<-read.csv("input/individual_phenometrics_data.csv", header=TRUE)
prov <- read.csv("input/provenanceinfo.csv", header=TRUE)
polsyn <- read.csv("flosandfruits/data/pollination.csv", header=TRUE)

prov$id <- prov$Individual_ID
prov <- subset(prov, select=c("id", "provenance.lat"))

### First let's do the obligatory cleaning checks with citizen scienece data
d <- d[(d$Multiple_FirstY>=1 | d$Multiple_Observers>0),]
d <- d[(d$NumYs_in_Series>=3),]
d <- d[(d$NumDays_Since_Prior_No<=7),]

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

ts <-ts%>% 
  group_by(id, phenophase, year) %>% 
  slice(which.min(doy))
ts<-ts[!duplicated(ts),]

ts$budburst <- ifelse(ts$phenophase=="budburst", ts$doy, NA)
ts$leaves <- ifelse(ts$phenophase=="leaves", ts$doy, NA)
ts$flobudburst <- ifelse(ts$phenophase=="flobudburst", ts$doy, NA)
ts$flowers <- ifelse(ts$phenophase=="flowers", ts$doy, NA)
ts$fruits <- ifelse(ts$phenophase=="fruits", ts$doy, NA)
ts$ripefruits <- ifelse(ts$phenophase=="ripefruits", ts$doy, NA)

ts.stan <- subset(ts, select=c("genus", "species", "id", "year", "budburst", "leaves", "flobudburst", 
                               "flowers", "fruits", "ripefruits"))

ts.stan <- ts.stan[!duplicated(ts.stan),]

ts.stan <- ts.stan %>% 
  group_by(genus, species, id, year) %>% 
  summarise_all(list(~first(na.omit(.))))

ts.stan$hys <- ifelse(ts.stan$budburst <= ts.stan$flobudburst, 0, 1)

polsyn$hys <- NULL
polsyn$flotype <- NULL

ts.stan <- left_join(ts.stan, polsyn)
ts.stan$pol.syn <- ifelse(ts.stan$pol.syn=="wind", 0, 1)

ts.stan$dvr <- ts.stan$leaves - ts.stan$budburst
ts.stan$flofruittime <- ts.stan$fruits - ts.stan$flobudburst

ts.stan <- left_join(ts.stan, prov)

##### Let's add in climate!!
d <- ts
d$climatetype <- "Treespotters"
source("calculating/clean_addinclimate.R")

ts.stan$mst <- NA
springtemps <- vector()
cc <- cc.arb
for(i in unique(cc$year)) {
  springtemps <- cc$tmean[(cc$year==i & cc$doy>=60 & cc$doy<=151)] # average spring temp from March 1-May 31
  springtemps <- springtemps[!is.na(springtemps)]
  ts.stan$mst<- ifelse(ts.stan$year==i, mean(springtemps), ts.stan$mst)
} 

ts.stan$idyear <- paste(ts.stan$id, ts.stan$year)

ts.stan$dvr.temp <- NA
dvrtemps <- vector()
for(i in c(1:nrow(ts.stan))) {
  dvrtemps <- cc$tmean[(cc$doy>=ts.stan$budburst[i] & cc$doy<=ts.stan$leaves[i])]
  dvrtemps <- dvrtemps[!is.na(dvrtemps)]
  ts.stan$dvr.temp[i]<- mean(dvrtemps)
}

ts.stan$flofruit.temp <- NA
flotemps <- vector()

for(i in c(1:nrow(ts.stan))) {
  flotemps <- cc$tmean[(cc$doy>=ts.stan$flobudburst[i] & cc$doy<=ts.stan$fruits[i])]
  flotemps <- flotemps[!is.na(flotemps)]
  ts.stan$flofruit.temp[i]<- mean(flotemps)
}



### Now some model prep!
flo.stan <- subset(ts.stan, select=c("flofruittime", "flowers", "fruits", "ripefruits", "leaves", "mst",
                                     "pol.syn", "provenance.lat", "genus", "species", "year", "flofruit.temp"))
flo.stan$spp <- paste(substr(flo.stan$genus, 0, 3), substr(flo.stan$species, 0, 3), sep="")
flo.stan <- flo.stan[!(flo.stan$flofruittime<=0),]

flo.stan$ripetime <- flo.stan$ripefruits - flo.stan$fruits

ripe.stan <- flo.stan[(flo.stan$ripetime>0),]

#flo.stan <- flo.stan[!(flo.stan$species=="hamvir"),]

#mod <- brm(flofruittime ~ leaves + mst + provenance.lat + (1|spp), 
 #          data = flo.stan, control=list(max_treedepth = 12,adapt_delta = 0.99) )

ripe.stan <- ripe.stan[!is.na(ripe.stan$ripetime),]
ripe.stan <- ripe.stan[!is.na(ripe.stan$leaves),]
ripe.stan <- ripe.stan[!is.na(ripe.stan$provenance.lat),]

mod.ripe <- brm(ripetime ~ provenance.lat + (provenance.lat|spp), data=ripe.stan,
                iter=4000, warmup=2500, control=list(max_treedepth = 15,adapt_delta = 0.99))




### Flowering to fruiting time - mst, leafout, prov.lat
fft.stan <- flo.stan[!is.na(flo.stan$flofruittime),]
fft.stan <- fft.stan[!is.na(fft.stan$mst),]
fft.stan <- fft.stan[!is.na(fft.stan$leaves),]
fft.stan <- fft.stan[!is.na(fft.stan$provenance.lat),]

mod.flo <- 



datalist.ff <- with(fft.stan, 
                       list(y = flofruittime, 
                            mst = mst, 
                            lo = leaves, 
                            lat = provenance.lat,
                            sp = as.numeric(as.factor(spp)),
                            N = nrow(fft.stan),
                            n_sp = length(unique(fft.stan$spp))
                       )
)

flofruit.norm = stan('stan/flofruit_normal.stan', data = datalist.ff,
                     iter = 2000, warmup=1500, control=list(max_treedepth = 12,adapt_delta = 0.99)) ## 7 divergent transitions


### Let's try to z-score now... 
fft.stan$mst.z <- (fft.stan$mst-mean(fft.stan$mst,na.rm=TRUE))/(sd(fft.stan$mst,na.rm=TRUE))
fft.stan$lo.z <- (fft.stan$leaves-mean(fft.stan$leaves,na.rm=TRUE))/(sd(fft.stan$leaves,na.rm=TRUE))
fft.stan$lat.z <- (fft.stan$provenance.lat-mean(fft.stan$provenance.lat,na.rm=TRUE))/(sd(fft.stan$provenance.lat,na.rm=TRUE))

datalist.ff <- with(fft.stan, 
                    list(y = flofruittime, 
                         mst = mst.z, 
                         lo = lo.z, 
                         lat = lat.z,
                         sp = as.numeric(as.factor(spp)),
                         N = nrow(flo.stan),
                         n_sp = length(unique(flo.stan$spp))
                    )
)

flofruit.z = stan('stan/flofruit_normal.stan', data = datalist.ff,
                     iter = 2000, warmup=1500, control=list(max_treedepth = 12,adapt_delta = 0.99)) ## 7 divergent transitions



