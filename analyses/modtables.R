## 24 March 2020 - Cat
# Setting up model output tables

rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(RColorBrewer)
library(rstan)
library(dplyr)
library(broom.mixed)

# Set Working Directory
setwd("~/Documents/git/microclimates/analyses")

## load the model
#load("stan/noisyws_sims.Rdata")
#load("stan/noisyhobo_sims.Rdata") 
#load("stan/micros_sims.Rdata")

#load("stan/urban_sims.Rdata")
load("stan/urbmethod_real.Rdata")
#load("stan/urbmethod_sims.Rdata")
#load("stan/prov_sims.Rdata")
#load("stan/provmethod_real.Rdata")

modname <- "urbmethod"

modoutput <- urbmethod

fit <- summary(modoutput)$summary
#summary(fit.z)# min n_ef: 1198 

tab<-as.data.frame(round(cbind(fit[1:9,1],fit[1:9,5],fit[1:9,7],fit[1:9,4],fit[1:9,8]),digits=2))
tab<-rbind(tab,c(length(fit[grep("a_sp", rownames(fit)),1])-2,"","","","",""))
rownames(tab)[10]<-"n_sp"


colnames(tab)<- c("mean","25%", "75%","2.5%","97.5%")

row.names(tab)<-c("$\\mu_{\\alpha}$","$\\mu_{site}$","$\\mu_{method}$",   
                                                 "$\\mu_{sitexmethod}$", "$\\sigma_{site}$"
                                                 , "$\\sigma_{method}$","$\\sigma_{sitexmethod}$","$\\sigma_{\\alpha}$", "$\\sigma_{y}$","$N_{sp}$") 
write.csv(tab, paste0("output/supptables/", modname, ".csv"))




