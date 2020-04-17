### Let's run some quick models on growing season and provenance latitude
# 26 July 2019 - Cat

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load libraries
library(rstan)
library(brms)
library(dplyr)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

## Set working directory
setwd("~/Documents/git/microclimates/analyses")


## Load the data
if(FALSE){
gs <- read.csv("output/clean_budburstandleafout.csv")

gs$gs <- gs$last.obs - gs$leafout
gs.stan <- gs[!is.na(gs$gs),]
gs.stan <- gs.stan[!is.na(gs.stan$provenance.lat),]

gs.stan$site <- NA
gs.stan$site <- ifelse(gs.stan$type=="Treespotters", "arb", gs.stan$site)
gs.stan$site <- ifelse(gs.stan$type=="Common Garden", "cg", gs.stan$site)
gs.stan$site <- ifelse(gs.stan$type=="Harvard Forest", "hf", gs.stan$site)

gs.stan$spp <- paste(substr(gs.stan$genus, 0, 3), substr(gs.stan$species, 0, 3), sep="")


modgsall <- brm(gs ~ site + provenance.lat + (site + provenance.lat|spp), data=gs.stan, 
                  control=list(max_treedepth = 15,adapt_delta = 0.99),
                  iter=4000, warmup=2500)

save(modgsall, file="stan/gsall.Rdata")
}


### Let's try a new one now...
gs <- read.csv("output/clean_gdd_bbanddvr.csv", header=TRUE)
#gs <- read.csv("/n/wolkovich_lab/Lab/Cat/clean_gdd_bbanddvr.csv", header=TRUE)

gs$gs <- gs$last.obs - gs$leafout
gs.stan <- gs[!is.na(gs$gs),]
gs.stan <- gs.stan[!is.na(gs.stan$provenance.lat),]
gs.stan <- gs.stan[!is.na(gs.stan$gstmean),]
gs.stan <- gs.stan[!is.na(gs.stan$precipgs),]

gs.stan$spp <- paste(substr(gs.stan$genus, 0, 3), substr(gs.stan$species, 0, 3), sep="")

gs.stan$sppsiteyr <- as.numeric(as.factor(paste(gs.stan$spp, gs.stan$type, gs.stan$year)))
sppsiteyr_count <-gs.stan %>% count(sppsiteyr)
gs.stan <- left_join(gs.stan, sppsiteyr_count)
gs.stan <- gs.stan[(gs.stan$n>1),]
gs.stan$sppsiteyr <- as.numeric(as.factor(paste(gs.stan$spp, gs.stan$type, gs.stan$year))) ## need to reevaluate since removing some groups
gsest <- data.frame(sppsiteyr=character(), spp=character(), integrals=numeric())

for(i in 1:length(unique(gs.stan$sppsiteyr))) { # i=3
  #for(j in 1:length(unique(gs.stan$spp))) { #j=2
  testfunc <- approxfun(density(gs.stan$gs[(gs.stan$sppsiteyr==i)])) #gs.stan$spp==j
  min <- min(gs.stan$gs[(gs.stan$sppsiteyr==i)])
  max <- max(gs.stan$gs[(gs.stan$sppsiteyr==i)])
  integrals <- integrate(testfunc, min, max)
  integrals <- integrals$value
  
  gsestadd <- data.frame(sppsiteyr=gs.stan$sppsiteyr[(gs.stan$sppsiteyr==i)], integrals=integrals)
  
  gsest <- rbind(gsest, gsestadd)
}

gsest <- gsest[!duplicated(gsest),]

gs.int.stan <- subset(gs.stan, select=c(spp, type, year, gs, gstmean, sppsiteyr))
gs.int.stan <- full_join(gs.int.stan, gsest)


modgsclim <- brm(integrals ~ gstmean + (gstmean | spp/type), data=gs.int.stan, 
                  control=list(max_treedepth = 15,adapt_delta = 0.99),
                  iter=4000, warmup=2500, cores=4)

save(modgsclim, file="stan/modgsclim.Rdata")
#save(modgsclim, file="/n/wolkovich_lab/Lab/Cat/modgsclim.Rdata")
