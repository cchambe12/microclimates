### Plotting the model output for all gdds
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

## load the model
load("stan/mod_hobo_dvr_hf.Rdata") ### change based on time and location
load("stan/mod_station_dvr_hf.Rdata") ### change based on time and location

gddlostation<-as.data.frame(tidy(mod_station_dvr_hf, prob=0.5)) ### change based on time and location
gddlohobo<-as.data.frame(tidy(mod_hobo_dvr_hf, prob=0.5)) ### change based on time and location
modoutput <- gddlohobo #modelhere

modoutput<-modoutput[1:9,] ### [1:15,] for TS; [1:9,] for HF
modoutput$`25%` <- modoutput$lower
modoutput$`75%` <- modoutput$upper
modoutput$term<-gsub(".*b_","",modoutput$term)
modoutput$term<-gsub(".*spp","",modoutput$term)
#modoutput$term <- ifelse(modoutput$term=="provenance.lat", "xlat", modoutput$term)
modoutput$Jvar <- as.numeric(rev(as.factor(modoutput$term)))

modoutput$intercept <- modoutput$estimate[modoutput$term=="Intercept"]
modoutput$int.25 <- modoutput$`25%`[modoutput$term=="Intercept"]
modoutput$int.75 <- modoutput$`75%`[modoutput$term=="Intercept"]

modoutput$estclean <- NA
modoutput$estclean <- ifelse(modoutput$term!="Intercept",
                             modoutput$estimate + modoutput$intercept, modoutput$estimate)
modoutput$clean.25 <- ifelse(modoutput$term!="Intercept",
                             modoutput$`25%` + modoutput$int.25, modoutput$`25%`)
modoutput$clean.75 <- ifelse(modoutput$term!="Intercept",
                             modoutput$`75%` + modoutput$int.75, modoutput$`75%`)

#estimates<-c("Acer rubrum", "Acer saccharum", "Aesculus flava", "Betula alleghaniensis", "Betula nigra", "Carya glabra", "Carya ovata",
 #            "Fagus grandifolia", "Hamamelis virginiana", "Populus deltoides", "Quercus alba", "Quercus rubra", "Tilia americana",
  #           "Vaccinium corymbosum", "Viburnum nudum") #### For Tree Spotters

estimates<-c("Acer pensylvanicum", "Acer rubrum", "Acer saccharum", "Betula alleghaniensis",
             "Fagus grandifolia", "Fraxinus americana", "Hamamelis virginiana", "Quercus alba", "Quercus rubra") #### For Harvard Forest

estimates<-rev(estimates)
modoutput <- modoutput[!is.na(modoutput$Jvar),]
muplot.station<-ggplot(modoutput, aes(x=`clean.25`, xend=`clean.75`, y=Jvar, yend=Jvar)) +
  geom_vline(xintercept=0, linetype="dotted") + 
  geom_point(aes(x=estclean, y=Jvar), col="black") +
  geom_segment(arrow = arrow(length = unit(0.00, "npc"))) +
  guides(size=FALSE) +
  scale_y_discrete(limits = sort(unique(modoutput$term)), labels=estimates) +
  xlab("Model estimate of change in growing degree days \nfrom budburst to leafout") + ### change based on time
  ylab("") + theme_linedraw() +
  theme(legend.text=element_text(size=5), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="sans"), legend.position = "none",
        legend.text.align = 0,
        plot.margin = unit(c(1,0,1,1), "lines")) +  
  coord_cartesian(xlim=c(0, 600), clip = 'off') + ggtitle("A. Harvard Forest: \nWeather Station") ### change based on location
  #coord_cartesian(xlim=c(400, 1050) for HF; coord_cartesian(xlim=c(-300, 1200) for Arb
  #DVR: c(-400,800) Arb;(0, 600) HF

muplot.hobo<-ggplot(modoutput, aes(x=`clean.25`, xend=`clean.75`, y=Jvar, yend=Jvar)) +
  geom_vline(xintercept=0, linetype="dotted") + 
  geom_point(aes(x=estclean, y=Jvar), col="black") +
  geom_segment(arrow = arrow(length = unit(0.00, "npc"))) +
  guides(size=FALSE) +
  scale_y_discrete(limits = sort(unique(modoutput$term)), labels=estimates) +
  xlab("Model estimate of change in growing degree days \nfrom budburst to leafout") + ylab("") + theme_linedraw() +
  theme(legend.text=element_text(size=5), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="sans"), legend.position = "none",
        legend.text.align = 0,
        plot.margin = unit(c(1,1,1,0), "lines")) +  ### think 'trouble' 
  coord_cartesian(xlim=c(0, 600), clip = 'off') + ggtitle("B. Harvard Forest: \nHobo Loggers") ### change based on location
  #coord_cartesian(xlim=c(400, 1050) for HF; coord_cartesian(xlim=c(-300, 1200) for Arb
  #DVR: c(-400,800) Arb; c(0,600) HF
quartz()
ggarrange(muplot.station, muplot.hobo, ncol=2)
