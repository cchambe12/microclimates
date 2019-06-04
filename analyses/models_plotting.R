### Started 1 April 2019 - Cat
## Building stan models to assess impact of False springs on DVR

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load libraries
library(RColorBrewer)
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Set working directory
setwd("~/Documents/git/microclimates/analyses")

## load the model

#flo.stan <- read.csv("output/clean_dvr_drought.csv", header=TRUE)

flo.stan$species.name <- NA
flo.stan$species.name <- ifelse(flo.stan$spp=="Acerub", "Acer rubrum", flo.stan$species.name)
flo.stan$species.name <- ifelse(flo.stan$spp=="Acesac", "Acer saccharm", flo.stan$species.name)
flo.stan$species.name <- ifelse(flo.stan$spp=="Aesfla", "Aesculus flava", flo.stan$species.name)
flo.stan$species.name <- ifelse(flo.stan$spp=="Betall", "Betula alleghaniensis", flo.stan$species.name)
flo.stan$species.name <- ifelse(flo.stan$spp=="Betnig", "Betula nigra", flo.stan$species.name)
flo.stan$species.name <- ifelse(flo.stan$spp=="Cargla", "Carya glabra", flo.stan$species.name)
flo.stan$species.name <- ifelse(flo.stan$spp=="Carova", "Carya ovata", flo.stan$species.name)
flo.stan$species.name <- ifelse(flo.stan$spp=="Faggra", "Fagus grandifolia", flo.stan$species.name)
flo.stan$species.name <- ifelse(flo.stan$spp=="Popdel", "Populus deltoides", flo.stan$species.name)
flo.stan$species.name <- ifelse(flo.stan$spp=="Quealb", "Quercus alba", flo.stan$species.name)
flo.stan$species.name <- ifelse(flo.stan$spp=="Querub", "Quercus rubra", flo.stan$species.name)
flo.stan$species.name <- ifelse(flo.stan$spp=="Tilame", "Tilia americana", flo.stan$species.name)
flo.stan$species.name <- ifelse(flo.stan$spp=="Vaccor", "Vaccinium \ncorymbosum", flo.stan$species.name)
flo.stan$species.name <- ifelse(flo.stan$spp=="Hamvir", "Hamamelis \nvirginiana", flo.stan$species.name)
flo.stan$species.name <- ifelse(flo.stan$spp=="Vibnud", "Viburnum nudum", flo.stan$species.name)


#### Now for mu plots based of bb_analysis/models_stan_plotting.R ###
figpath <- "figures"
figpathmore <- "flofruittime" ### change based on model

source("source/micro_muplot.R")
cols <- adjustcolor("indianred3", alpha.f = 0.3) 
my.pal <- rep(brewer.pal(n = 6, name = "Paired"), 2)
# display.brewer.all()
my.pch <- rep(15:16, each=6)
alphahere = 0.4
xlab <- "Model estimate of change in \nflowering to fruiting time" ## change based on model
specieshere <- flo.stan$spp
speciesnamehere <- flo.stan$species.name

sumer.ni <- summary(flofruit.z)$summary
sumer.ni[grep("mu_", rownames(sumer.ni)),]

sort(unique(flo.stan$spp)) # numbers are alphabetical


modelhere <- flofruit.z
#quartz()
muplotfx(specieshere, modelhere, "", 8, 8, c(0,3), c(-20, 20) , 21, 2, speciesnamehere)

