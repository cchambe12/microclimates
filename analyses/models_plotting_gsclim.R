### Let's prepare for plotting effects on growing season length
# 26 July 2019 - Cat

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load libraries
library(RColorBrewer)
library(egg)
library(brms)
library(ggplot2)
library(viridis)
library(broom)
library(dplyr)
library(tidyr)

## Set working directory
setwd("~/Documents/git/microclimates/analyses")

gs <- read.csv("output/clean_gdd_bbanddvr.csv", header=TRUE)

gs$gs <- gs$last.obs - gs$leafout
gs.stan <- gs[!is.na(gs$gs),]
gs.stan <- gs.stan[!is.na(gs.stan$provenance.lat),]
gs.stan <- gs.stan[!is.na(gs.stan$gstmean),]
gs.stan <- gs.stan[!is.na(gs.stan$precipgs),]

gs.stan$spp <- paste(substr(gs.stan$genus, 0, 3), substr(gs.stan$species, 0, 3), sep="")

load("stan/modgsclim.Rdata")

figpath <- "figures"
figpathmore <- "muplot_gsclim" ### change based on model

cols <- adjustcolor("indianred3", alpha.f = 0.3)
my.pal <- rep(viridis_pal(option="C")(11), times=11)
#my.pal <- rep(brewer.pal(n = 11, name = "Set3"), 11)
my.pch <- rep(15:17, each=11)
#my.pch <- my.pch[c(1:12, 14:29, 31:32, 34:48)]
# display.brewer.all()
alphahere = 0.4
xlab <- "Model estimate of change in \ngrowing season length" ## change based on model

allspp <- unique(paste(gs.stan$genus, gs.stan$species))

spp <- allspp

modelhere <- modgsclim

gsclim<-as.data.frame(tidy(modgsclim, prob=0.5))
modoutput <- gsclim #modelhere

modoutput<-modoutput[c(2:4, 45:131),]
modoutput$lower <- modoutput$lower
modoutput$upper <- modoutput$upper
modoutput$term<-gsub(".*b_","",modoutput$term)
modoutput$term<-gsub(".*spp","",modoutput$term)

tmean <- coef(modelhere, prob=c(0.25, 0.75))$spp[, c(1, 3:4), 2] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  rename(mean = Estimate) %>%
  rename(lower = Q25) %>%
  rename(upper = Q75) %>%
  dplyr::select( mean, lower, upper) 
new.names<-NULL
for(i in 1:length(spp)){
  new.names[i]<-paste("gstmean", "[", i, "]", sep="")
}
tmean$parameter<-new.names
precip <- coef(modelhere, prob=c(0.25, 0.75))$spp[, c(1, 3:4), 3] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  rename(mean = Estimate) %>%
  rename(lower = Q25) %>%
  rename(upper = Q75) %>%
  dplyr::select( mean, lower, upper) 
new.names<-NULL
for(i in 1:length(spp)){
  new.names[i]<-paste("precipgs", "[", i, "]", sep="")
}
precip$parameter<-new.names
mod.ranef<-full_join(tmean, precip)
prov.lat<- coef(modelhere, prob=c(0.25, 0.75))$spp[, c(1, 3:4), 4] %>%
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
mod.ranef<-full_join(mod.ranef, prov.lat)

df <- gs.stan ### dataframe here!!!!

#allspp <- ifelse(allspp=="Amelanchier canadensis", "Amelanchier \ncanadensis", allspp)
#allspp <- ifelse(allspp=="Vaccinium corymbosum", "Vaccinium \ncorymbosum", allspp)
speciesnames <- allspp ### species here!!!


source("exp_muplot_all.R")
muplotfx(modelhere, "", 8, 8, c(0,3), c(-20, 20) , 22, 3)
