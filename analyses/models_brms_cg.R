### Started 25 July 2019 - Cat
## Building stan models to investigate growth

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load libraries
library(RColorBrewer)
library(rstan)
library(brms)
library(broom)
library(egg)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Set working directory
setwd("~/Documents/git/microclimates/analyses")

## Load the data
cg <- read.csv("output/clean_phenandtraits_growthform.csv", header=TRUE)

cg19 <- subset(cg, cg$year==2019)
cg19 <- subset(cg19, select=c("ht.diff", "spp", "Individual", "Plot", "provenance.lat", "budburst", "leafout", "risk"))
cg19 <- cg19[!duplicated(cg19),]
cg19$Individual <- paste(cg19$Individual, cg19$Plot, sep="_")

if(FALSE){
cg19.risk <- cg19[!is.na(cg19$risk),]
ht.mod <- brm(ht.diff ~ risk + provenance.lat + (risk + provenance.lat|spp), data=cg19.risk,
              control=list(max_treedepth = 15,adapt_delta = 0.99),
              iter=4000, warmup=2500)
}

if(TRUE){
cg19.lo <- cg19[!is.na(cg19$leafout),]
ht.mod.lo <- brm(ht.diff ~ leafout + provenance.lat + (leafout + provenance.lat|spp), data=cg19.lo,
              control=list(max_treedepth = 15,adapt_delta = 0.99),
              iter=4000, warmup=2500)

#ht.mod.lo_winter <- brm(ht.diff ~ leafout + provenance.lat + leafout:provenance.lat + (leafout + provenance.lat|spp), data=cg19.lo,
 #                control=list(max_treedepth = 15,adapt_delta = 0.99),
  #               iter=4000, warmup=2500)
#save(ht.mod.lo, file="stan/htcglo.Rdata")
}

if(FALSE){
cg19.bb <- cg19[!is.na(cg19$budburst),]
ht.mod.bb <- brm(ht.diff ~ budburst + provenance.lat + (budburst + provenance.lat|spp), data=cg19.bb,
                 control=list(max_treedepth = 15,adapt_delta = 0.99),
                 iter=4000, warmup=2500)

}

if(FALSE){
gdd <- read.csv("output/clean_gdd_bbanddvr.csv", header=TRUE)

gdd <- subset(gdd, gdd$type=="Common Garden")
gdd <- subset(gdd, gdd$year==2019)

#gdd$Individual <- gsub("_[^_]+$", "", gdd$id)
gdd$Individual <- gdd$id
gdd <- subset(gdd, select=c("Individual", "gdd_lo"))

cg19.gddlo <- left_join(cg19, gdd)

cg19.gddlo <- cg19.gddlo[!is.na(cg19.gddlo$gdd_lo),]
ht.mod.ggdlo <- brm(ht.diff ~ gdd_lo + provenance.lat + (budburst + provenance.lat|spp), data=cg19.gddlo,
                 control=list(max_treedepth = 15,adapt_delta = 0.99),
                 iter=4000, warmup=2500)

}
