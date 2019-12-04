### Started 25 July 2019 - Cat
## Work on comparing gdds from weather station vs hobos for leafout!

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load libraries
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(tidyr)
library(brms)

# Set working directory
setwd("~/Documents/git/microclimates/analyses")

## Load the data
gdd.stan <- read.csv("output/clean_gdd_bbanddvr.csv", header=TRUE)
gdd.hobo <- read.csv("output/clean_gdd_bbanddvr_hobo.csv", header=TRUE)

if(FALSE){ ## Testing code
  gdd.stan$stationbb <- gdd.stan$gdd_bb
  gdd.stan$gdd_bb <- NULL
  gdd.stan$stationdvr <- gdd.stan$gdd_dvr
  gdd.stan$gdd_dvr <- NULL
  gdd <- left_join(gdd.hobo, gdd.stan)
  gdd$diffbb <- gdd$stationbb - gdd$gdd_bb
  gdd$diffdvr <- gdd$stationdvr - gdd$gdd_dvr
  range(gdd$diffdvr, na.rm=TRUE)
  range(gdd$diffbb, na.rm=TRUE)
}


gdd.stan <- gdd.stan[(gdd.stan$type=="Treespotters"),]
gdd.hobo <- gdd.hobo[(gdd.hobo$type=="Treespotters"),]

gdd.hobo$gdd_bb_hobo <- gdd.hobo$gdd_bb
gdd.hobo$gdd_dvr_hobo <- gdd.hobo$gdd_dvr
gdd.hobo$gdd_lo_hobo <- gdd.hobo$gdd_lo

gdd.hobo$gdd_bb <- NULL
gdd.hobo$gdd_dvr <- NULL
gdd.hobo$gdd_lo <- NULL

gdd.hobo$spp <- paste(substr(gdd.hobo$genus, 0, 3), substr(gdd.hobo$species, 0, 3), sep="")
gdd.stan$spp <- paste(substr(gdd.stan$genus, 0, 3), substr(gdd.stan$species, 0, 3), sep="")

commoncols <- c("id", "provenance.lat", "fs.count", "year", "budburst", "leafout", "spp", "climatetype")
gdd.hobo <- subset(gdd.hobo, select=c(commoncols, "gdd_bb_hobo", "gdd_dvr_hobo", "gdd_lo_hobo"))
gdd.stan <- subset(gdd.stan, select=c(commoncols, "gdd_bb", "gdd_dvr", "gdd_lo"))

gdd.hobo$hobonum <- gdd.hobo$climatetype
gdd.hobo$climatetype <- NULL

gdd <- full_join(gdd.stan, gdd.hobo)
gdd$dvr <- gdd$leafout - gdd$budburst
#gdd$leafout <- NULL

gdd <- gdd[(gdd$year==2019),]

gdd <- gdd[(gdd$gdd_bb<=1000),]
gdd <- gdd[(gdd$gdd_bb_hobo<=1000),]

station <- subset(gdd.stan, select=c("gdd_lo", "provenance.lat", "spp"))
station <- na.omit(station)

hobo <- subset(gdd, select=c("gdd_lo_hobo", "provenance.lat", "spp"))
hobo <- na.omit(hobo)

mod_station_lo <- brm(gdd_lo ~ spp, data=station,
                   control=list(max_treedepth = 15,adapt_delta = 0.99),
                   iter=4000, warmup=2500)

save(mod_station_lo, file="stan/mod_station_lo.Rdata")

mod_hobo_lo <- brm(gdd_lo_hobo ~ spp, data=hobo,
                control=list(max_treedepth = 15,adapt_delta = 0.99),
                iter=4000, warmup=2500)

save(mod_hobo_lo, file="stan/mod_hobo_lo.Rdata")


if(FALSE){
#### Now time for CG provenance.lat and year
### 
gdd.stan <- read.csv("output/clean_gdd_bbanddvr.csv", header=TRUE)
gdd.stan <- gdd.stan[(gdd.stan$type=="Common Garden"),]

gdd.stan <- gdd.stan[(gdd.stan$gdd_bb<1000),]
gdd.stan$spp <- paste(substr(gdd.stan$genus, 0, 3), substr(gdd.stan$species, 0, 3), sep="")
gdd.stan$spp <- ifelse(gdd.stan$spp=="NANA", "Quealb", gdd.stan$spp)

cg <- subset(gdd.stan, select=c("gdd_bb", "provenance.lat", "spp", "year"))
cg <- na.omit(cg)

gdd<-cg

mod_cg <- brm(gdd_bb ~ provenance.lat + (provenance.lat | spp), data=cg,
              control=list(max_treedepth = 15,adapt_delta = 0.99),
              iter=4000, warmup=2500)

save(mod_cg, file="stan/mod_cg_prov.Rdata")
}

###### 
library(broom)
station.mod<-as.data.frame(tidy(mod_station_lo, prob=0.5))
hobo.mod<-as.data.frame(tidy(mod_hobo_lo, prob=0.5))
#cg.mod<-as.data.frame(tidy(mod_cg, prob=0.5))
modoutput <- station.mod #modelhere

modoutput<-modoutput[c(2, 22:36),]
modoutput$lower <- modoutput$lower
modoutput$upper <- modoutput$upper
modoutput$term<-gsub(".*b_","",modoutput$term)
modoutput$term<-gsub(".*spp","",modoutput$term)
modoutput$term <- ifelse(grepl(",provenance.lat", modoutput$term), substr(modoutput$term, 2, 7), modoutput$term)
#modoutput$term <- ifelse(modoutput$term=="provenance.lat", "xlat", modoutput$term)
#modoutput$Jvar <- as.numeric(rev(as.factor(modoutput$term)))

#### Now for mu plots based of bb_analysis/models_stan_plotting.R ###
figpath <- "figures"
figpathmore <- "station_lo" ### change based on model

source("exp_muplot_brms.R")
cols <- adjustcolor("indianred3", alpha.f = 0.3) 
my.pal <- rep(brewer.pal(n = 8, name = "Dark2"), 8)
my.pch <- rep(15:16, each=8)
#my.pch <- my.pch[c(1:12, 14:29, 31:32, 34:48)]
# display.brewer.all()
alphahere = 0.4
xlab <- "Model estimate of change in \ngrowing degree days to budburst" ## change based on model

#sumer.ni <- summary()$summary
#sumer.ni[grep("mu_", rownames(sumer.ni)),]

#sort(unique(chill.stan$species)) # numbers are alphabetical

spp <- unique(station$spp)

modelhere <- mod_station_lo

if(FALSE){
  intercept <- coef(modelhere, prob=c(0.25, 0.75))$spp[, c(1, 3:4), 1] %>%
    as.data.frame() %>%
    round(digits = 2) %>% 
    rename(mean = Estimate) %>%
    rename(lower = Q25) %>%
    rename(upper = Q75) %>%
    dplyr::select(mean, lower, upper) 
  new.names<-NULL
  for(i in 1:length(spp)){
    new.names[i]<-paste("intercept", "[", i, "]", sep="")
  }
  intercept$parameter<-new.names
  
prov.lat <- coef(modelhere, prob=c(0.25, 0.75))$spp[, c(1, 3:4), 2] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  rename(mean = Estimate) %>%
  rename(lower = Q25) %>%
  rename(upper = Q75) %>%
  dplyr::select( mean, lower, upper) 
new.names<-NULL
for(i in 1:length(spp)){
  new.names[i]<-paste("prov.lat", "[", i, "]", sep="")
}
prov.lat$parameter<-new.names
mod.ranef<-prov.lat


#modoutput <- tidy(modelhere, prob=c(0.5))
#quartz()
#muplotfx(modelhere, "", 8, 8, c(0,5), c(-10, 10) , 10.5, 3.5)
muplotfx(modelhere, "", 8, 8, c(0,0), c(-5, 10) , 10.5, 1)
}
