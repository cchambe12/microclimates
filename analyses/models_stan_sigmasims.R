### Started 17 April 2020 by Cat
## Models testing where error is loaded

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#### Do some flagging to try all versions of the simulations

## Let's start with Question 1 first...
library(bayesplot) ## for plotting
library(egg) ## for plotting
library(shinystan)
library(rstanarm)
library(rstan)
library(brms)

## Let's load some real data to check out.
setwd("~/Documents/git/microclimates/analyses/")

source("source/stan_utility.R")

ws <- read.csv("output/clean_gdd_chill_bbanddvr.csv")
hobo <- read.csv("output/clean_gdd_chill_bbanddvr_hobo.csv")

mean(ws$gdd_bb, na.rm=TRUE) ## 300
mean(hobo$gdd_bb, na.rm=TRUE) ## 250

set.seed(12221)

use.sims = TRUE
use.hobo = FALSE ### We expect less species variation using weather station data, so if use.hobo=TRUE, then sigma will be loaded on overall error not on species
use.urban = TRUE
use.provenance = TRUE

if(use.urban==TRUE & use.provenance==TRUE){
  print("Error was made in flags!! Adjust accordingly!")
}

if(use.provenance==FALSE & use.urban==FALSE){
  print("Error was made in flags!! Adjust accordingly!")
}


########################################################################
if (use.sims==TRUE & use.hobo==FALSE & use.urban==TRUE & use.provenance==FALSE){
  
  gdd.stan <- read.csv("output/fakedata_ws_urb.csv")
  
  datalist.gdd <- with(gdd.stan, 
                      list(y = gdd, 
                           tx = urban,
                           sp = as.numeric(as.factor(species)),
                           N = nrow(gdd.stan),
                           n_sp = length(unique(gdd.stan$species))
                      )
  )
                      
                      
  ws_urb_fake = stan('stan/urbanmodel_stan_normal_ncp.stan', data = datalist.gdd,
                     iter = 5000, warmup=2000) ### 
  
  check_all_diagnostics(ws_urb_fake)
  
  ws_urb_fake.sum <- summary(ws_urb_fake)$summary
  ws_urb_fake.sum[grep("mu_", rownames(ws_urb_fake.sum)),]
  ws_urb_fake.sum[grep("sigma_", rownames(ws_urb_fake.sum)),]
  
  save(ws_urb_fake, file="~/Documents/git/microclimates/analyses/stan/ws_urban_stan_sims_ncp.Rdata")
  
}


########################################################################
if (use.sims==TRUE & use.hobo==TRUE & use.urban==TRUE & use.provenance==FALSE){
  
  gdd.stan <- read.csv("output/fakedata_hl_urb.csv")
  
  datalist.gdd <- with(gdd.stan, 
                       list(y = gdd, 
                            tx = urban,
                            sp = as.numeric(as.factor(species)),
                            N = nrow(gdd.stan),
                            n_sp = length(unique(gdd.stan$species))
                       )
  )
  
  
  hl_urb_fake = stan('stan/urbanmodel_stan_normal_ncp.stan', data = datalist.gdd,
                     iter = 5000, warmup=2000) ### 
  
  
  check_all_diagnostics(hl_urb_fake)
  
  hl_urb_fake.sum <- summary(hl_urb_fake)$summary
  hl_urb_fake.sum[grep("mu_", rownames(hl_urb_fake.sum)),]
  hl_urb_fake.sum[grep("sigma_", rownames(hl_urb_fake.sum)),]
  
  
  save(hl_urb_fake, file="~/Documents/git/microclimates/analyses/stan/hl_urban_stan_sims.Rdata")
                       
}



########################################################################
if (use.sims==TRUE & use.hobo==FALSE & use.urban==FALSE & use.provenance==TRUE){
  
  gdd.stan <- read.csv("output/fakedata_ws_prov.csv")
  
  datalist.gdd <- with(gdd.stan, 
                       list(y = gdd, 
                            prov = provenance,
                            sp = as.numeric(as.factor(species)),
                            N = nrow(gdd.stan),
                            n_sp = length(unique(gdd.stan$species))
                       )
  )
  
  
  ws_prov_fake = stan('stan/provmodel_stan_normal_ncp.stan', data = datalist.gdd,
                     iter = 5000, warmup=2000, control=list(max_treedepth = 15,adapt_delta = 0.99)) ### 
  
  check_all_diagnostics(ws_prov_fake)
  
  ws_prov_fake.sum <- summary(ws_prov_fake)$summary
  ws_prov_fake.sum[grep("mu_", rownames(ws_prov_fake.sum)),]
  ws_prov_fake.sum[grep("sigma_", rownames(ws_prov_fake.sum)),]
  
  save(ws_prov_fake, file="~/Documents/git/microclimates/analyses/stan/ws_prov_stan_sims.Rdata")
                       
}



########################################################################
if (use.sims==TRUE & use.hobo==TRUE & use.urban==FALSE & use.provenance==TRUE){
  
  gdd.stan <- read.csv("output/fakedata_hl_prov.csv")
  
  datalist.gdd <- with(gdd.stan, 
                       list(y = gdd, 
                            prov = provenance,
                            sp = as.numeric(as.factor(species)),
                            N = nrow(gdd.stan),
                            n_sp = length(unique(gdd.stan$species))
                       )
  )
  
  
  hl_prov_fake = stan('stan/provmodel_stan_normal_ncp.stan', data = datalist.gdd,
                          iter = 5000, warmup=2000) ### 
  
  check_all_diagnostics(hl_prov_fake)
  
  hl_prov_fake.sum <- summary(hl_prov_fake)$summary
  hl_prov_fake.sum[grep("mu_", rownames(hl_prov_fake.sum)),]
  hl_prov_fake.sum[grep("sigma_", rownames(hl_prov_fake.sum)),]
  
  save(hl_prov_fake, file="~/Documents/git/microclimates/analyses/stan/hl_prov_stan_sims.Rdata")
}


########################################################################
if (use.sims==FALSE & use.hobo==FALSE & use.urban==TRUE & use.provenance==FALSE){
  
  ws <- read.csv("output/clean_gdd_chill_bbanddvr.csv")
  
  ws <- ws[!(ws$type=="Common Garden"),]
  
  ws$urban <- ifelse(ws$type=="Harvard Forest", 0, 1)
  
  ws$spp <- paste(ws$genus, ws$species, sep="_")
  
  gdd.stan <- subset(ws, select=c(gdd_bb, urban, spp))
  gdd.stan <- gdd.stan[complete.cases(gdd.stan),]
  
  gdd.stan <- gdd.stan[!duplicated(gdd.stan),]
  
  datalist.gdd <- with(gdd.stan, 
                       list(y = gdd_bb, 
                            tx = urban,
                            sp = as.numeric(as.factor(spp)),
                            N = nrow(gdd.stan),
                            n_sp = length(unique(gdd.stan$spp))
                       )
  )
  
  
  ws_urb = stan('stan/urbanmodel_stan_normal_ncp.stan', data = datalist.gdd,
                iter = 5000, warmup=2000) ### 
  
  check_all_diagnostics(ws_urb)
  
  ws_urb.sum <- summary(ws_urb)$summary
  ws_urb.sum[grep("mu_", rownames(ws_urb.sum)),]
  ws_urb.sum[grep("sigma_", rownames(ws_urb.sum)),]
  
  save(ws_urb, file="~/Documents/git/microclimates/analyses/stan/ws_urban_stan.Rdata")
}


########################################################################
if (use.sims==FALSE & use.hobo==FALSE & use.urban==TRUE & use.provenance==FALSE){
  
  hl <- read.csv("output/clean_gdd_chill_bbanddvr_hobo.csv")
  
  hl <- hl[!(hl$type=="Common Garden"),]
  
  hl$urban <- ifelse(hl$type=="Harvard Forest", 0, 1)
  
  hl$spp <- paste(hl$genus, hl$species, sep="_")
  
  gdd.stan <- subset(hl, select=c(gdd_bb, urban, spp))
  gdd.stan <- gdd.stan[complete.cases(gdd.stan),]
  
  gdd.stan <- gdd.stan[!duplicated(gdd.stan),]
  
  datalist.gdd <- with(gdd.stan, 
                       list(y = gdd_bb, 
                            tx = urban,
                            sp = as.numeric(as.factor(spp)),
                            N = nrow(gdd.stan),
                            n_sp = length(unique(gdd.stan$spp))
                       )
  )
  
  
  hl_urb = stan('stan/urbanmodel_stan_normal_ncp.stan', data = datalist.gdd,
                iter = 5000, warmup=2000) ### 
  
  check_all_diagnostics(hl_urb)
  
  hl_urb.sum <- summary(hl_urb)$summary
  hl_urb.sum[grep("mu_", rownames(hl_urb.sum)),]
  hl_urb.sum[grep("sigma_", rownames(hl_urb.sum)),]
  
  save(hl_urb, file="~/Documents/git/microclimates/analyses/stan/hl_urban_stan.Rdata")
}


########################################################################
if (use.sims==FALSE & use.hobo==FALSE & use.urban==FALSE & use.provenance==TRUE){
  
  ws <- read.csv("output/clean_gdd_chill_bbanddvr.csv")
  
  ws <- ws[!(ws$type=="Common Garden"),]
  
  ws$provenance <- as.numeric(ws$provenance.lat)
  
  ws$spp <- paste(ws$genus, ws$species, sep="_")
  
  gdd.stan <- subset(ws, select=c(gdd_bb, provenance, spp))
  gdd.stan <- gdd.stan[complete.cases(gdd.stan),]
  
  gdd.stan <- gdd.stan[!duplicated(gdd.stan),]
  
  datalist.gdd <- with(gdd.stan, 
                       list(y = gdd_bb, 
                            prov = provenance,
                            sp = as.numeric(as.factor(spp)),
                            N = nrow(gdd.stan),
                            n_sp = length(unique(gdd.stan$spp))
                       )
  )
  
  
  ws_prov = stan('stan/provmodel_stan_normal_ncp.stan', data = datalist.gdd,
                 iter = 5000, warmup=2000) ### 
  
  check_all_diagnostics(ws_prov)
  
  ws_prov.sum <- summary(ws_prov)$summary
  ws_prov.sum[grep("mu_", rownames(ws_prov.sum)),]
  ws_prov.sum[grep("sigma_", rownames(ws_prov.sum)),]
  
  save(ws_prov, file="~/Documents/git/microclimates/analyses/stan/ws_prov_stan.Rdata")
}



########################################################################
if (use.sims==FALSE & use.hobo==TRUE & use.urban==FALSE & use.provenance==TRUE){
  
  hl <- read.csv("output/clean_gdd_chill_bbanddvr_hobo.csv")
  
  hl <- hl[!(hl$type=="Common Garden"),]
  
  hl$provenance <- as.numeric(hl$provenance.lat)
  
  hl$spp <- paste(hl$genus, hl$species, sep="_")
  
  gdd.stan <- subset(hl, select=c(gdd_bb, provenance, spp))
  gdd.stan <- gdd.stan[complete.cases(gdd.stan),]
  
  gdd.stan <- gdd.stan[!duplicated(gdd.stan),]
  
  datalist.gdd <- with(gdd.stan, 
                       list(y = gdd_bb, 
                            prov = provenance,
                            sp = as.numeric(as.factor(spp)),
                            N = nrow(gdd.stan),
                            n_sp = length(unique(gdd.stan$spp))
                       )
  )
  
  
  hl_prov = stan('stan/provmodel_stan_normal_ncp.stan', data = datalist.gdd,
                 iter = 5000, warmup=2000) ### 
  
  check_all_diagnostics(hl_prov)
  
  hl_prov.sum <- summary(hl_prov)$summary
  hl_prov.sum[grep("mu_", rownames(hl_prov.sum)),]
  hl_prov.sum[grep("sigma_", rownames(hl_prov.sum)),]
  
  save(hl_prov, file="~/Documents/git/microclimates/analyses/stan/hl_prov_stan.Rdata")
}

