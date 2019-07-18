### Started 15 July 2019 - Cat
## Building stan models to assess site effects on GDDs

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
gdd.stan <- read.csv("output/clean_gdd_bbanddvr.csv", header=TRUE)

gdd.stan$spp <- paste(substr(gdd.stan$genus, 0, 3), substr(gdd.stan$species,0,3), sep="")
gdd.stan$site <- NA
gdd.stan$site <- ifelse(gdd.stan$type=="Treespotters", "arb", gdd.stan$site)
gdd.stan$site <- ifelse(gdd.stan$type=="Common Garden", "cg", gdd.stan$site)
gdd.stan$site <- ifelse(gdd.stan$type=="Harvard Forest", "hf", gdd.stan$site)

gdd.stan <- subset(gdd.stan, select=c("id", "provenance.lat", "spp", "site",
                                      "gdd_bb", "gdd_dvr", "fs.count"))

gdd.stan <- gdd.stan[!is.na(gdd.stan$provenance.lat),]
gdd.stan <- gdd.stan[!is.na(gdd.stan$gdd_bb),]
gdd.stan <- gdd.stan[!is.na(gdd.stan$gdd_dvr),]
gdd.stan <- gdd.stan[!is.na(gdd.stan$spp),]

gdd.stan <- gdd.stan[(gdd.stan$gdd_bb<1000),]
gdd.stan <- gdd.stan[(gdd.stan$gdd_dvr<1000),]

gdd.stan$spp <- ifelse(gdd.stan$spp=="NANA", "Quealb", gdd.stan$spp)

mod.gddbb <- brm(gdd_bb ~ site + provenance.lat, data=gdd.stan, control=list(max_treedepth = 15,adapt_delta = 0.99))
mod.gdddvr <- brm(gdd_dvr ~ spp*site, data=gdd.stan, control=list(max_treedepth = 15,adapt_delta = 0.99))
#mod.gddbb.prov <- brm(gdd_bb ~ spp*site + provenance.lat, data=gdd.stan)



############ Now compare to hobo logger data to weather station data ###########
gdd.hobo <- read.csv("output/clean_gdd_bbanddvr_hobo.csv", header=TRUE)

gdd.hobo$spp <- paste(substr(gdd.hobo$genus, 0, 3), substr(gdd.hobo$species,0,3), sep="")
gdd.hobo <- gdd.hobo[(gdd.hobo$type=="Treespotters"),]
gdd.hobo <- subset(gdd.hobo, select=c("id", "provenance.lat", "spp",
                                      "gdd_bb", "gdd_dvr", "fs.count"))

gdd.hobo <- gdd.hobo[!is.na(gdd.hobo$provenance.lat),]
gdd.hobo <- gdd.hobo[!is.na(gdd.hobo$gdd_bb),]
gdd.hobo <- gdd.hobo[!is.na(gdd.hobo$gdd_dvr),]
gdd.hobo <- gdd.hobo[!is.na(gdd.hobo$spp),]

gdd.hobo <- gdd.hobo[(gdd.hobo$gdd_bb<1000),]
gdd.hobo <- gdd.hobo[(gdd.hobo$gdd_dvr<1000),]

mod.hobobb <- brm(gdd_bb ~ spp + provenance.lat, data=gdd.hobo, control=list(max_treedepth = 15,adapt_delta = 0.99))
mod.hobodvr <- brm(gdd_dvr ~ spp, data=gdd.hobo, control=list(max_treedepth = 15,adapt_delta = 0.99))
#mod.gddbb.prov <- brm(gdd_bb ~ spp*site + provenance.lat, data=gdd.hobo)





ts.gdd <- read.csv("output/clean_gdd_bbanddvr_hobo.csv", header=TRUE)

ts.gdd$spp <- paste(substr(ts.gdd$genus, 0, 3), substr(ts.gdd$species,0,3), sep="")
ts.gdd <- ts.gdd[(ts.gdd$type=="Treespotters"),]
ts.gdd <- ts.gdd[(ts.gdd$year==2019),]
ts.gdd <- subset(ts.gdd, select=c("id", "provenance.lat", "spp",
                                      "gdd_bb", "gdd_dvr", "fs.count"))

ts.gdd <- ts.gdd[!is.na(ts.gdd$provenance.lat),]
ts.gdd <- ts.gdd[!is.na(ts.gdd$gdd_bb),]
ts.gdd <- ts.gdd[!is.na(ts.gdd$gdd_dvr),]
ts.gdd <- ts.gdd[!is.na(ts.gdd$spp),]

ts.gdd <- ts.gdd[(ts.gdd$gdd_bb<1000),]
ts.gdd <- ts.gdd[(ts.gdd$gdd_dvr<1000),]

mod.tsbb <- brm(gdd_bb ~ spp + provenance.lat, data=ts.gdd, control=list(max_treedepth = 15,adapt_delta = 0.99))
mod.tsdvr <- brm(gdd_dvr ~ spp, data=ts.gdd, control=list(max_treedepth = 15,adapt_delta = 0.99))
#mod.gddbb.prov <- brm(gdd_bb ~ spp*site + provenance.lat, data=ts.gdd)

#### Let's make side by side plots now...
library(broom)
sitemod<-as.data.frame(tidy(mod.gddbb, prob=0.5))
modoutput <- sitemod #modelhere

modoutput<-modoutput[2:4,]
modoutput$`25%` <- modoutput$lower
modoutput$`75%` <- modoutput$upper
modoutput$term<-gsub(".*b_","",modoutput$term)
modoutput$term<-gsub(".*spp","",modoutput$term)
modoutput$term <- ifelse(modoutput$term=="provenance.lat", "xlat", modoutput$term)
modoutput$Jvar <- as.numeric(rev(as.factor(modoutput$term)))

estimates<-c("Tree Spotters","Common Garden", "Harvard Forest")

#estimates<-c("Acer saccharum", "Aesculus flava", "Betula alleghaniense", "Betula nigra", "Carya glabra", "Carya ovata",
 #            "Fagus grandifolia", "Hamamelis virginiana", "Populus deltoides", "Quercus alba", "Quercus rubra", "Tilia americana",
  #           "Vaccinium corymbosum", "Viburnum nudum", "Provenance Latitude")
estimates<-rev(estimates)
modoutput <- modoutput[!is.na(modoutput$Jvar),]
muplot<-ggplot(modoutput, aes(x=`25%`, xend=`75%`, y=Jvar, yend=Jvar)) +
  geom_vline(xintercept=0, linetype="dotted") + geom_point(aes(x=estimate, y=Jvar), col="black") +
  geom_segment(arrow = arrow(length = unit(0.00, "npc"))) +
  guides(size=FALSE) +
  scale_y_discrete(limits = sort(unique(modoutput$term)), labels=estimates) +
  xlab("Change in growing degree days \nfrom budburst to leafout") + ylab("") + theme_linedraw() +
  theme(legend.text=element_text(size=5), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="sans"), legend.position = "none",
        legend.text.align = 0,
        plot.margin = unit(c(1,1,1,1), "lines")) +  #+ ggtitle("Original Parameters") +
  coord_cartesian(xlim=c(-150, 150), ylim=c(1,3), clip = 'off') #+ ggtitle("Hobo Loggers") 

station<-as.data.frame(tidy(mod.tsdvr, prob=0.5))
modoutput <- station #modelhere

modoutput<-modoutput[2:16,]
modoutput$`25%` <- modoutput$lower
modoutput$`75%` <- modoutput$upper
modoutput$term<-gsub(".*b_","",modoutput$term)
modoutput$term<-gsub(".*spp","",modoutput$term)
modoutput$term <- ifelse(modoutput$term=="provenance.lat", "xlat", modoutput$term)
modoutput$Jvar <- as.numeric(rev(as.factor(modoutput$term)))

estimates<-c("Acer saccharum", "Aesculus flava", "Betula alleghaniense", "Betula nigra", "Carya glabra", "Carya ovata",
             "Fagus grandifolia", "Hamamelis virginiana", "Populus deltoides", "Quercus alba", "Quercus rubra", "Tilia americana",
             "Vaccinium corymbosum", "Viburnum nudum", "Provenance Latitude")
estimates<-rev(estimates)
modoutput <- modoutput[!is.na(modoutput$Jvar),]
stationplot<-ggplot(modoutput, aes(x=`25%`, xend=`75%`, y=Jvar, yend=Jvar)) +
  geom_vline(xintercept=0, linetype="dotted") + geom_point(aes(x=estimate, y=Jvar), col="black") +
  geom_segment(arrow = arrow(length = unit(0.00, "npc"))) +
  guides(size=FALSE) +
  scale_y_discrete(limits = sort(unique(modoutput$term)), labels=estimates) +
  xlab("Change in growing degree days \nfrom budburst to leafout") + ylab("") + theme_linedraw() +
  theme(legend.text=element_text(size=5), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="sans"), legend.position = "none",
        legend.text.align = 0,
        plot.margin = unit(c(1,1,1,0), "lines")) +  #+ ggtitle("Original Parameters") +
  coord_cartesian(xlim=c(-200, 70), ylim=c(1,15), clip = 'off') + ggtitle("Weather Station") 


quartz()
ggarrange(muplot, stationplot, ncol=2)


#### Make some tables now...
library(dplyr)
hobobb<-as.data.frame(tidy(mod.hobobb, prob=0.9))
names(hobobb)<-c("term", "estimate", "error", "10%", "90%")
mod.hobobb50<-as.data.frame(tidy(mod.hobobb, prob=0.5))
names(mod.hobobb50)<-c("term", "estimate", "error", "25%", "75%")
hobobb <- full_join(hobobb, mod.hobobb50)
hobobb <- subset(hobobb, select=c("term", "estimate", "10%", "25%", "75%", "90%"))
write.csv(hobobb, file="output/ts_hobobb_modouput.csv", row.names=FALSE)

hobodvr<-as.data.frame(tidy(mod.hobodvr, prob=0.9))
names(hobodvr)<-c("term", "estimate", "error", "10%", "90%")
mod.hobodvr50<-as.data.frame(tidy(mod.hobodvr, prob=0.5))
names(mod.hobodvr50)<-c("term", "estimate", "error", "25%", "75%")
hobodvr <- full_join(hobodvr, mod.hobodvr50)
hobodvr <- subset(hobodvr, select=c("term", "estimate", "10%", "25%", "75%", "90%"))
write.csv(hobodvr, file="output/ts_hobodvr_modouput.csv", row.names=FALSE)

tsbb<-as.data.frame(tidy(mod.tsbb, prob=0.9))
names(tsbb)<-c("term", "estimate", "error", "10%", "90%")
mod.tsbb50<-as.data.frame(tidy(mod.tsbb, prob=0.5))
names(mod.tsbb50)<-c("term", "estimate", "error", "25%", "75%")
tsbb <- full_join(tsbb, mod.tsbb50)
tsbb <- subset(tsbb, select=c("term", "estimate", "10%", "25%", "75%", "90%"))
write.csv(tsbb, file="output/ts_bb_modouput.csv", row.names=FALSE)

tsdvr<-as.data.frame(tidy(mod.tsdvr, prob=0.9))
names(tsdvr)<-c("term", "estimate", "error", "10%", "90%")
mod.tsdvr50<-as.data.frame(tidy(mod.tsdvr, prob=0.5))
names(mod.tsdvr50)<-c("term", "estimate", "error", "25%", "75%")
tsdvr <- full_join(tsdvr, mod.tsdvr50)
tsdvr <- subset(tsdvr, select=c("term", "estimate", "10%", "25%", "75%", "90%"))
write.csv(tsbb, file="output/ts_dvr_modouput.csv", row.names=FALSE)

