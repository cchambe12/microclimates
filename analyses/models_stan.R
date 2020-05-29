### Started 30 January 2020 by Cat
## Let's try and build a model in stan!!

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#### Questions to address:
## Compare GDDs between hobo loggers and weather station data
# 1) GDDlo ~ 1 + (1|species) - do once for HF weather station, once for hobo logger and repeat for Arboretum
# Compare urban effect using weather station data and then hobo logger data
# 2) GDDlo ~ urban + (urban|species) - do once with weather station data and once with hobo logger data

## Let's start with Question 1 first...
library(shinystan)
library(rstan)

## Let's load some real data to check out.
setwd("~/Documents/git/microclimates/analyses/")

#source("source/stan_utility.R")

urb <- read.csv("output/testdata_urbmethod.csv")

#yreal <- urb$gdd

datalist.gdd <- with(urb, 
                     list(y = gdd, 
                          urban = urban,
                          method = method,
                          sp = as.numeric(as.factor(species)),
                          N = nrow(urb),
                          n_sp = length(unique(species))
                     )
)


urbmethod_fake = stan('stan/urbanmethod_normal_ncp.stan', data = datalist.gdd,
                        iter = 2000, warmup=1000)#, control=list(adapt_delta=0.99)) ### 
  
#check_all_diagnostics(ws_urb_buildfake)
  
urbmethod_fakesum <- summary(urbmethod_fake)$summary
urbmethod_fakesum[grep("mu_", rownames(urbmethod_fakesum)),]
urbmethod_fakesum[grep("sigma_", rownames(urbmethod_fakesum)),]

yraw <- urb$gdd

launch_shinystan(urbmethod_fake)  

#### Now with real data
ws <- read.csv("output/clean_gdd_bbanddvr.csv")
ws$method <- 1

ws_urb <- subset(ws, select=c("id", "type", "gdd_bb", "method", "year", "genus", "species"))
ws_urb <- ws_urb[(ws_urb$type!="Common Garden"),]
ws_urb <- ws_urb[(ws_urb$year=="2019"),]

hobo <- read.csv("output/clean_gdd_bbanddvr_hobo.csv")
hobo$method <- 0

hobo_urb <- subset(hobo, select=c("id", "type", "gdd_bb", "method", "year", "genus", "species"))
hobo_urb <- hobo_urb[(hobo_urb$type!="Common Garden"),]
hobo_urb <- hobo_urb[(hobo_urb$year=="2019"),]

bball <- dplyr::full_join(ws_urb, hobo_urb)

bball$urban <- NA
bball$urban <- ifelse(bball$type=="Harvard Forest", 0, bball$urban)
bball$urban <- ifelse(bball$type=="Treespotters", 1, bball$urban)


bball.stan <- subset(bball, select=c(gdd_bb, urban, method, genus, species))
bball.stan <- bball.stan[(complete.cases(bball.stan)),]
bball.stan <- bball.stan[!is.na(bball.stan$gdd_bb),]
bball.stan$spp <- paste(bball.stan$genus, bball.stan$species, sep="_")

bball.stan <- bball.stan[(bball.stan$gdd_bb<=1000),]

yraw <- bball$gdd_bb
yraw <- na.omit(yraw)

datalist.gdd <- with(bball.stan, 
                       list(y = gdd_bb, 
                            urban = urban, 
                            method = method,
                            sp = as.numeric(as.factor(spp)),
                            N = nrow(bball.stan),
                            n_sp = length(unique(bball.stan$spp))
                       )
)


urbmethod = stan('stan/urbanmethod_normal_ncp.stan', data = datalist.gdd,
                      iter = 4000, warmup=2500, control=list(adapt_delta=0.90)) ### 

#check_all_diagnostics(ws_urb_buildfake)

urbmethod_sum <- summary(urbmethod)$summary
urbmethod_sum[grep("mu_", rownames(urbmethod_sum)),]
urbmethod_sum[grep("sigma_", rownames(urbmethod_sum)),]

launch_shinystan(urbmethod)
