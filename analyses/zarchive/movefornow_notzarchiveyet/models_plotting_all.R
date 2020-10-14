### Let's prepare for plotting
# 25 July 2019 - Cat
### There are so many species so I need to first tackle that...

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load libraries
library(RColorBrewer)
library(egg)
library(brms)
library(ggplot2)
library(viridis)

## Set working directory
setwd("~/Documents/git/microclimates/analyses")

gdd.stan <- read.csv("output/clean_gdd_bbanddvr.csv", header=TRUE)
#gdd.stan <- read.csv("/n/wolkovich_lab/Lab/Cat/clean_gdd_bbanddvr.csv")

gdd.stan$spp <- paste(substr(gdd.stan$genus, 0, 3), substr(gdd.stan$species,0,3), sep="")
gdd.stan$site <- NA
gdd.stan$site <- ifelse(gdd.stan$type=="Treespotters", "arb", gdd.stan$site)
gdd.stan$site <- ifelse(gdd.stan$type=="Common Garden", "cg", gdd.stan$site)
gdd.stan$site <- ifelse(gdd.stan$type=="Harvard Forest", "hf", gdd.stan$site)

gdd.stan <- subset(gdd.stan, select=c("id", "provenance.lat", "spp", "site", "genus", "species",
                                      "gdd_bb", "gdd_dvr", "fs.count", "gdd_lo"))

gdd.stan <- gdd.stan[!is.na(gdd.stan$provenance.lat),]
gdd.stan <- gdd.stan[!is.na(gdd.stan$gdd_bb),]
gdd.stan <- gdd.stan[!is.na(gdd.stan$gdd_dvr),]
gdd.stan <- gdd.stan[!is.na(gdd.stan$spp),]

gdd.stan <- gdd.stan[(gdd.stan$gdd_bb<1000),]
gdd.stan <- gdd.stan[(gdd.stan$gdd_dvr<1000),]

gdd.stan$spp <- ifelse(gdd.stan$spp=="NANA", "Quealb", gdd.stan$spp)

load("stan/htcglo.Rdata")
load("stan/gddbb_full_mixed.Rdata")

cgheight<-as.data.frame(tidy(ht.mod.lo, prob=0.5))
gddlo<-as.data.frame(tidy(mod.gddbb, prob=0.5))


cgspp <- c("Acer pensylvanicum", "Acer spicatum", "Alnus incana", "Amelanchier \ncanadensis", "Aronia melonacarpa",
        "Betula alleghaniensis", "Betula papyrifera", "Betula populifolia", "Diervilla lonicera", "Myrica gale",
        "Quercus alba", "Quercus rubra", "Sambucus racemosa", "Sorbus americana", "Spirea alba", "Spirea tomentosa",
        "Viburnum cassinoides")

allspp <- sort(unique(paste(gdd.stan$genus, gdd.stan$species, sep=" ")))
rmspp <- c("NA NA", "Acer rubra")
allspp <- allspp[!(allspp%in%rmspp)]


modoutput <- cgheight #modelhere
#modoutput <- gddlo #modelhere


modoutput<-modoutput[c(2:3, 28:61),]
#modoutput<-modoutput[c(2:4, 47:139),]
modoutput$lower <- modoutput$lower
modoutput$upper <- modoutput$upper
modoutput$term<-gsub(".*b_","",modoutput$term)
modoutput$term<-gsub(".*spp","",modoutput$term)
#modoutput$term <- ifelse(grepl(",provenance.lat", modoutput$term), substr(modoutput$term, 2, 7), modoutput$term)
#modoutput$term <- ifelse(grepl(",leafout", modoutput$term), substr(modoutput$term, 2, 7), modoutput$term)
#modoutput$term <- ifelse(modoutput$term=="provenance.lat", "xlat", modoutput$term)
#modoutput$Jvar <- as.numeric(rev(as.factor(modoutput$term)))

#### Now for mu plots based of bb_analysis/models_stan_plotting.R ###
figpath <- "figures"
figpathmore <- "cg_height" ### change based on model
#figpathmore <- "site_prov_all" ### change based on model

if(TRUE){
cols <- adjustcolor("indianred3", alpha.f = 0.3)
my.pal <- rep(viridis_pal(option="C")(9), times=9)
#my.pal <- rep(brewer.pal(n = 8, name = "Dark2"), 8)
my.pch <- rep(15:16, each=9)
#my.pch <- my.pch[c(1:12, 14:29, 31:32, 34:48)]
# display.brewer.all()
alphahere = 0.4
xlab <- "Model estimate of change in growth (cm)" ## change based on model

#sumer.ni <- summary()$summary
#sumer.ni[grep("mu_", rownames(sumer.ni)),]

#sort(unique(chill.stan$species)) # numbers are alphabetical

cg <- read.csv("output/clean_phenandtraits_growthform.csv", header=TRUE)

cg19 <- subset(cg, cg$year==2019)
cg19 <- subset(cg19, select=c("ht.diff", "spp", "provenance.lat", "budburst", "leafout", "risk"))
cg19 <- cg19[!duplicated(cg19),]
cg19.lo <- cg19[!is.na(cg19$leafout),]

spp <- unique(cg19.lo$spp)

modelhere <- ht.mod.lo

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
}
leafout <- coef(modelhere, prob=c(0.25, 0.75))$spp[, c(1, 3:4), 2] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  rename(mean = Estimate) %>%
  rename(lower = Q25) %>%
  rename(upper = Q75) %>%
  dplyr::select( mean, lower, upper) 
new.names<-NULL
for(i in 1:length(spp)){
  new.names[i]<-paste("leafout", "[", i, "]", sep="")
}
leafout$parameter<-new.names
mod.ranef<-leafout
prov.lat <- coef(modelhere, prob=c(0.25, 0.75))$spp[, c(1, 3:4), 3] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  rename(mean = Estimate) %>%
  rename(lower = Q25) %>%
  rename(upper = Q75) %>%
  dplyr::select( mean, lower, upper) 
new.names<-NULL
for(i in 1:length(spp)){
  new.names[i]<-paste("provenance.lat", "[", i, "]", sep="")
}
prov.lat$parameter<-new.names
mod.ranef<-full_join(leafout, prov.lat)

df <- cg19.lo ### dataframe here!!!!
speciesnames <- cgspp ### species here!!!

#modoutput <- tidy(modelhere, prob=c(0.5))
#quartz()
#muplotfx(modelhere, "", 8, 8, c(0,5), c(-10, 10) , 10.5, 3.5)
source("exp_muplot_brms.R")
muplotfx(modelhere, "", 8, 8, c(0,2), c(-8, 2) , 2.3, 2)
}


if(FALSE){
cols <- adjustcolor("indianred3", alpha.f = 0.3)
my.pal <- rep(viridis_pal(option="C")(11), times=11)
#my.pal <- rep(brewer.pal(n = 11, name = "Set3"), 11)
my.pch <- rep(15:17, each=11)
#my.pch <- my.pch[c(1:12, 14:29, 31:32, 34:48)]
# display.brewer.all()
alphahere = 0.4
xlab <- "Model estimate of change in \ngrowing degree days to leafout" ## change based on model

spp <- allspp

modelhere <- mod.gddbb

sitecg <- coef(modelhere, prob=c(0.25, 0.75))$spp[, c(1, 3:4), 2] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  rename(mean = Estimate) %>%
  rename(lower = Q25) %>%
  rename(upper = Q75) %>%
  dplyr::select( mean, lower, upper) 
new.names<-NULL
for(i in 1:length(spp)){
  new.names[i]<-paste("sitecg", "[", i, "]", sep="")
}
sitecg$parameter<-new.names
sitehf <- coef(modelhere, prob=c(0.25, 0.75))$spp[, c(1, 3:4), 3] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  rename(mean = Estimate) %>%
  rename(lower = Q25) %>%
  rename(upper = Q75) %>%
  dplyr::select( mean, lower, upper) 
new.names<-NULL
for(i in 1:length(spp)){
  new.names[i]<-paste("sitehf", "[", i, "]", sep="")
}
sitehf$parameter<-new.names
mod.ranef<-full_join(sitecg, sitehf)
prov.lat<- coef(modelhere, prob=c(0.25, 0.75))$spp[, c(1, 3:4), 4] %>%
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
mod.ranef<-full_join(mod.ranef, prov.lat)

df <- gdd.stan ### dataframe here!!!!

#allspp <- ifelse(allspp=="Amelanchier canadensis", "Amelanchier \ncanadensis", allspp)
#allspp <- ifelse(allspp=="Vaccinium corymbosum", "Vaccinium \ncorymbosum", allspp)
speciesnames <- allspp ### species here!!!


#modoutput <- tidy(modelhere, prob=c(0.5))
#quartz()
#muplotfx(modelhere, "", 8, 8, c(0,5), c(-10, 10) , 10.5, 3.5)
source("exp_muplot_all.R")
muplotfx(modelhere, "", 8, 8, c(0,3), c(-200, 250) , 270, 3)
}