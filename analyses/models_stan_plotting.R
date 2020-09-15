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
library(bayesplot) ## for plotting
library(egg) ## for plotting
library(shinystan)
library(rstanarm)
library(rstan)
library(brms)
library(RColorBrewer)
library(viridis)
library(dplyr)

## Let's load some real data to check out.
setwd("~/Documents/git/microclimates/analyses/")

figpath <- "figures"
figpathmore <- "ws_method_urb_real"

source("source/microurban_muplot.R")
#source("source/microurban_phylo_muplot.R")

# Set up colors
cols <- adjustcolor("indianred3", alpha.f = 0.3) 
my.pal <-rep(viridis_pal(option="viridis")(9),2)
#my.pal <- rep(brewer.pal(n = 8, name = "Dark2"), 3)
#my.pal <- my.pal[my.pal!=c("#FFFFBF", "#FEE08B", "#E6F598")]
# display.brewer.all()
my.pch <- rep(15:18, each=10)
alphahere = 0.4

if(FALSE){
if(figpathmore=="ws_urb" | figpathmore=="ws_phylo_urb"){
ws_urb <- read.csv("output/clean_gdd_bbanddvr.csv")
ws_urb$species <- ifelse(ws_urb$genus=="Acer" & ws_urb$species=="rubra", "rubrum", ws_urb$species)

ws_urb$urban <- NA
ws_urb$urban <- ifelse(ws_urb$type=="Harvard Forest", 0, ws_urb$urban)
ws_urb$urban <- ifelse(ws_urb$type=="Treespotters", 1, ws_urb$urban)
ws_urb <- ws_urb[(ws_urb$year>=2019),]

ws_urb.stan <- subset(ws_urb, select=c(gdd_bb, urban, genus, species))
ws_urb.stan <- ws_urb.stan[(complete.cases(ws_urb.stan)),]
ws_urb.stan$spp <- paste(ws_urb.stan$genus, ws_urb.stan$species, sep="_")

ws_urb.stan <- ws_urb.stan[(ws_urb.stan$gdd_bb<=1000),]
} else if(figpathmore=="hobo_urb" | figpathmore=="hobo_phylo_urb"){
  hobo_urb <- read.csv("output/clean_gdd_bbanddvr_hobo.csv")
  hobo_urb$species <- ifelse(ws_urb$genus=="Acer" & ws_urb$species=="rubra", "rubrum", ws_urb$species)
  
  hobo_urb$urban <- NA
  hobo_urb$urban <- ifelse(hobo_urb$type=="Harvard Forest", 0, hobo_urb$urban)
  hobo_urb$urban <- ifelse(hobo_urb$type=="Treespotters", 1, hobo_urb$urban)
  hobo_urb <- hobo_urb[(hobo_urb$year>=2019),]
  
  hobo_urb.stan <- subset(hobo_urb, select=c(gdd_bb, urban, genus, species))
  hobo_urb.stan <- hobo_urb.stan[(complete.cases(hobo_urb.stan)),]
  hobo_urb.stan$spp <- paste(hobo_urb.stan$genus, hobo_urb.stan$species, sep="_")
  hobo_urb.stan <- hobo_urb.stan[(hobo_urb.stan$gdd_bb<=1000),]
}
}

# Load fitted stan model: no interactions
#load("stan/hobo_urban_mod.Rdata") 
#load("stan/ws_urban_mod.Rdata") 
#load("stan/wsall_urban_mod.Rdata")
#load("stan/ws_phylomod_urban.Rdata")
load("stan/urbmethod_inter.Rdata")

use.stan=TRUE

if(use.stan==TRUE){
  ws <- read.csv("output/clean_gdd_chill_bbanddvr.csv")
  ws$method <- 1
  
  ws_urb <- subset(ws, select=c("id", "type", "gdd_bb", "method", "year", "genus", "species", "utah"))
  ws_urb <- ws_urb[(ws_urb$type!="Common Garden"),]
  
  hobo <- read.csv("output/clean_gdd_chill_bbanddvr_hobo.csv")
  hobo$method <- 0
  
  hobo_urb <- subset(hobo, select=c("id", "type", "gdd_bb", "method", "year", "genus", "species", "utah"))
  hobo_urb <- hobo_urb[(hobo_urb$type!="Common Garden"),]
  
  bball <- dplyr::full_join(ws_urb, hobo_urb)
  
  bball$urban <- NA
  bball$urban <- ifelse(bball$type=="Harvard Forest", 0, bball$urban)
  bball$urban <- ifelse(bball$type=="Treespotters", 1, bball$urban)
  
  bball.stan <- bball[(bball$year=="2019"),]
  bball.stan <- subset(bball, select=c(gdd_bb, urban, method, genus, species))
  bball.stan <- bball.stan[(complete.cases(bball.stan)),]
  bball.stan <- bball.stan[!is.na(bball.stan$gdd_bb),]
  bball.stan$spp <- paste(bball.stan$genus, bball.stan$species, sep="_")
  
  bball.stan <- bball.stan[(bball.stan$gdd_bb<=1000),]
  
sumer.ws <- summary(urbmethod)$summary
sumer.ws[grep("mu_", rownames(sumer.ws)),]

unique(bball.stan$spp) # numbers are alphabetical
sort(unique(bball.stan$spp))

# m1.bb <- m2l.ni
modelhere <- urbmethod
muplotfx(modelhere, "", 7, 7, c(0,3), c(-150, 50), 60, 3)
}


if(use.stan==FALSE){
  modelhere <- model_phylo  
  df <- ws_urb.stan
  species <- unique(df$spp)

intercept <- coef(modelhere, prob=c(0.25, 0.75))$phylo[, c(1, 3:4), 1] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  rename(mean = Estimate) %>%
  rename(`25%` = Q25) %>%
  rename(`75%` = Q75) %>%
  dplyr::select( mean, `25%`, `75%`) 
new.names<-NULL
for(i in 1:length(species)){
  new.names[i]<-paste("intercept", "[", i, "]", sep="")
}
intercept$parameter<-new.names

urban <- coef(modelhere, prob=c(0.25, 0.75))$phylo[, c(1, 3:4), 2] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  rename(mean = Estimate) %>%
  rename(`25%` = Q25) %>%
  rename(`75%` = Q75) %>%
  dplyr::select(mean, `25%`, `75%`) 
new.names<-NULL
for(i in 1:length(species)){
  new.names[i]<-paste("urban", "[", i, "]", sep="")
}
urban$parameter<-new.names
mod.ranef <- full_join(intercept, urban)

modoutput <- tidy(modelhere, prob=c(0.5))
xlab = "Model estimate change in growing degree days to budburst"

#df <- ws_urb.stan

muplotfx(modelhere, "", 8, 8, c(0,2), c(-300, 600) , 640, 1.5)

}

