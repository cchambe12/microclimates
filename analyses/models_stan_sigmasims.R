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
use.highsitevariation = FALSE ## Not sure if I will use these but here just in case
use.highprovvariation = FALSE

#check.diags = TRUE ## Do you want to check diagnostics?
#save.stan = TRUE  ## Do you want to save your model?

if(use.urban==FALSE & use.highsitevariation==TRUE){
  print("Error was made in flags!! Adjust accordingly!")
}

if(use.provenance==FALSE & use.highprovvariation==TRUE){
  print("Error was made in flags!! Adjust accordingly!")
}


########################################################################
if (use.sims==TRUE & use.hobo==FALSE & use.urban==TRUE & use.provenance==FALSE &
    use.highsitevariation==FALSE & use.highprovvariation==FALSE){
  
  gdd.stan <- read.csv("output/fakedata_ws_urb.csv")
  
  datalist.gdd <- with(gdd.stan, 
                      list(y = gdd, 
                           tx = urban,
                           sp = as.numeric(as.factor(species)),
                           N = nrow(gdd.stan),
                           n_sp = length(unique(gdd.stan$species))
                      )
  )
                      
                      
  ws_urb_fake = stan('stan/urbanmodel_stan_normal_weather.stan', data = datalist.gdd,
                     iter = 2000, warmup=1500) ### 
  
  check_all_diagnostics(ws_urb_fake)
  
  ws_urb_fake.sum <- summary(ws_urb_fake)$summary
  ws_urb_fake.sum[grep("mu_", rownames(ws_urb_fake.sum)),]
  ws_urb_fake.sum[grep("sigma_", rownames(ws_urb_fake.sum)),]
  
  save(ws_urb_fake, file="~/Documents/git/microclimates/analyses/stan/ws_urban_stan_sims.Rdata")
  
}


########################################################################
if (use.sims==TRUE & use.hobo==TRUE & use.urban==TRUE & use.provenance==FALSE &
    use.highsitevariation==FALSE & use.highprovvariation==FALSE){
  gdd.stan <- read.csv("output/fakedata_hl_urb.csv")
  
  datalist.gdd <- with(gdd.stan, 
                       list(y = gdd, 
                            tx = urban,
                            sp = as.numeric(as.factor(species)),
                            N = nrow(gdd.stan),
                            n_sp = length(unique(gdd.stan$species))
                       )
  )
  
  
  hl_urb_fake = stan('stan/urbanmodel_stan_normal_weather.stan', data = datalist.gdd,
                     iter = 2000, warmup=1500) ### 
  
  
  check_all_diagnostics(hl_urb_fake)
  
  hl_urb_fake.sum <- summary(hl_urb_fake)$summary
  hl_urb_fake.sum[grep("mu_", rownames(hl_urb_fake.sum)),]
  hl_urb_fake.sum[grep("sigma_", rownames(hl_urb_fake.sum)),]
  
  
  save(hl_urb_fake, file="~/Documents/git/microclimates/analyses/stan/hl_urban_stan_sims.Rdata")
                       
}



########################################################################
if (use.sims==TRUE & use.hobo==FALSE & use.urban==TRUE & use.provenance==TRUE &
    use.highsitevariation==FALSE & use.highprovvariation==FALSE){
  gdd.stan <- read.csv("output/fakedata_ws_urb_prov.csv")
  
  datalist.gdd <- with(gdd.stan, 
                       list(y = gdd, 
                            tx = urban,
                            prov = provenance,
                            sp = as.numeric(as.factor(species)),
                            N = nrow(gdd.stan),
                            n_sp = length(unique(gdd.stan$species))
                       )
  )
  
  
  ws_urb_prov_fake = stan('stan/urbanmodel_stan_normal_prov.stan', data = datalist.gdd,
                     iter = 5000, warmup=2000, control=list(max_treedepth = 15,adapt_delta = 0.99)) ### 
  
  check_all_diagnostics(ws_urb_prov_fake)
  
  ws_urb_prov_fake.sum <- summary(ws_urb_prov_fake)$summary
  ws_urb_prov_fake.sum[grep("mu_", rownames(ws_urb_prov_fake.sum)),]
  ws_urb_prov_fake.sum[grep("sigma_", rownames(ws_urb_prov_fake.sum)),]
  
  save(ws_urb_prov_fake, file="~/Documents/git/microclimates/analyses/stan/ws_urban_prov_stan_sims.Rdata")
                       
}



########################################################################
if (use.sims==TRUE & use.hobo==TRUE & use.urban==TRUE & use.provenance==TRUE &
    use.highsitevariation==FALSE & use.highprovvariation==FALSE){
  gdd.stan <- read.csv("output/fakedata_hl_urb_prov.csv")
  
  datalist.gdd <- with(gdd.stan, 
                       list(y = gdd, 
                            tx = urban,
                            prov = provenance,
                            sp = as.numeric(as.factor(species)),
                            N = nrow(gdd.stan),
                            n_sp = length(unique(gdd.stan$species))
                       )
  )
  
  
  hl_urb_prov_fake = stan('stan/urbanmodel_stan_normal_prov.stan', data = datalist.gdd,
                          iter = 5000, warmup=2000, control=list(max_treedepth = 15,adapt_delta = 0.99)) ### 
  
  check_all_diagnostics(hl_urb_prov_fake)
  
  hl_urb_prov_fake.sum <- summary(hl_urb_prov_fake)$summary
  hl_urb_prov_fake.sum[grep("mu_", rownames(hl_urb_prov_fake.sum)),]
  hl_urb_prov_fake.sum[grep("sigma_", rownames(hl_urb_prov_fake.sum)),]
  
  save(hl_urb_prov_fake, file="~/Documents/git/microclimates/analyses/stan/hl_urban_prov_stan_sims.Rdata")
}

########################################################################
if (use.sims==FALSE & use.hobo==FALSE & use.urban==TRUE & use.provenance==TRUE &
    use.highsitevariation==FALSE & use.highprovvariation==FALSE){
  
  ws <- read.csv("output/clean_gdd_chill_bbanddvr.csv")
  
  ws <- ws[!(ws$type=="Common Garden"),]
  
  ws$provenance <- as.numeric(ws$provenance.lat)
  ws$urban <- ifelse(ws$type=="Harvard Forest", 0, 1)
  
  ws$spp <- paste(ws$genus, ws$species, sep="_")
  
  gdd.stan <- subset(ws, select=c(gdd_bb, urban, provenance, spp))
  gdd.stan <- gdd.stan[complete.cases(gdd.stan),]
  
  gdd.stan <- gdd.stan[!duplicated(gdd.stan),]
  
  datalist.gdd <- with(gdd.stan, 
                       list(y = gdd_bb, 
                            tx = urban,
                            prov = provenance,
                            sp = as.numeric(as.factor(spp)),
                            N = nrow(gdd.stan),
                            n_sp = length(unique(gdd.stan$spp))
                       )
  )
  
  
  ws_urb_prov = stan('stan/urbanmodel_stan_normal_prov.stan', data = datalist.gdd,
                          iter = 5000, warmup=2000, control=list(max_treedepth = 15,adapt_delta = 0.99)) ### 
  #### This rstan model has lots of divergent transitions ~100-200 
  
  ws_urb_prov = stan('stan/urbanmodel_stan_normal_prov_ncp.stan', data = datalist.gdd,
                     iter = 5000, warmup=3000, control=list(max_treedepth = 15,adapt_delta = 0.99)) ###
  #### This rstan model is better! ~29 divergent transitions. Next step: add a vcov matrix?
  
  
  ws_urb_prov_brm <- brm(gdd_bb ~ urban + provenance + (urban + provenance|spp), data=gdd.stan,
                         iter=5000, warmup=2500, 
                         control=list(max_treedepth=15, adapt_delta=0.99))
  ### BRMS code has a few divergent transitions ~20
  
  check_all_diagnostics(ws_urb_prov)
  
  ws_urb_prov.sum <- summary(ws_urb_prov)$summary
  ws_urb_prov.sum[grep("mu_", rownames(ws_urb_prov.sum)),]
  ws_urb_prov.sum[grep("sigma_", rownames(ws_urb_prov.sum)),]
  
  save(hl_urb_prov_fake, file="~/Documents/git/microclimates/analyses/stan/hl_urban_prov_stan_sims.Rdata")
}

########################################################################
if (use.sims==FALSE & use.hobo==FALSE & use.urban==TRUE & use.provenance==FALSE &
    use.highsitevariation==FALSE & use.highprovvariation==FALSE){
  
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
                     iter = 5000, warmup=3000, control=list(max_treedepth = 15,adapt_delta = 0.99)) ### 
  #### This rstan model has lots of divergent transitions ~51 but adding ncp removes divergences!!
  
  
  ws_urb_brm <- brm(gdd_bb ~ urban + (urban|spp), data=gdd.stan,
                         iter=5000, warmup=2500, 
                         control=list(max_treedepth=15, adapt_delta=0.99))
  ### BRMS code is smooth sailing. Try ncp in stan code
  
  check_all_diagnostics(ws_urb_prov)
  
  hl_urb_prov_fake.sum <- summary(hl_urb_prov_fake)$summary
  hl_urb_prov_fake.sum[grep("mu_", rownames(hl_urb_prov_fake.sum)),]
  hl_urb_prov_fake.sum[grep("sigma_", rownames(hl_urb_prov_fake.sum)),]
  
  save(ws_urb, file="~/Documents/git/microclimates/analyses/stan/ws_urban_stan.Rdata")
}


########################################################################
if (use.sims==FALSE & use.hobo==FALSE & use.urban==TRUE & use.provenance==FALSE &
    use.highsitevariation==FALSE & use.highprovvariation==FALSE){
  
  hl <- read.csv("output/clean_gdd_chill_bbanddvr.csv")
  
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
                iter = 5000, warmup=3000, control=list(max_treedepth = 15,adapt_delta = 0.99)) ### 
  #### This rstan model has lots of divergent transitions ~67 but adding ncp removes!
  
  
  hl_urb_brm <- brm(gdd_bb ~ urban + (urban|spp), data=gdd.stan,
                    iter=5000, warmup=2500, 
                    control=list(max_treedepth=15, adapt_delta=0.99))
  ### BRMS code is smooth sailing. Try ncp in stan code
  
  check_all_diagnostics(hl_urb_prov)
  
  hl_urb_prov_fake.sum <- summary(hl_urb_prov_fake)$summary
  hl_urb_prov_fake.sum[grep("mu_", rownames(hl_urb_prov_fake.sum)),]
  hl_urb_prov_fake.sum[grep("sigma_", rownames(hl_urb_prov_fake.sum)),]
  
  save(hl_urb, file="~/Documents/git/microclimates/analyses/stan/hl_urban_stan.Rdata")
}

