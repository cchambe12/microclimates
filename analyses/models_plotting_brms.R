### Plotting the model output for all gdds
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load libraries
library(RColorBrewer)
library(brms)
library(broom)
library(egg)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Set working directory
setwd("~/Documents/git/microclimates/analyses")

## load the model
load("stan/mod_hobo.Rdata")
load("stan/mod_station.Rdata")


gddbb<-as.data.frame(tidy(mod_hobo, prob=0.5))
modoutput <- gddbb #modelhere

modoutput<-modoutput[1:15,]
modoutput$`25%` <- modoutput$lower
modoutput$`75%` <- modoutput$upper
modoutput$term<-gsub(".*b_","",modoutput$term)
modoutput$term<-gsub(".*spp","",modoutput$term)
modoutput$term <- ifelse(modoutput$term=="provenance.lat", "xlat", modoutput$term)
modoutput$Jvar <- as.numeric(rev(as.factor(modoutput$term)))

estimates<-c("Intercept", "Acer saccharum", "Aesculus flava", "Betula alleghaniense", "Betula nigra", "Carya glabra", "Carya ovata",
             "Fagus grandifolia", "Hamamelis virginiana", "Populus deltoides", "Quercus alba", "Quercus rubra", "Tilia americana",
             "Vaccinium corymbosum", "Viburnum nudum")
estimates<-rev(estimates)
modoutput <- modoutput[!is.na(modoutput$Jvar),]
muplot.station<-ggplot(modoutput, aes(x=`25%`, xend=`75%`, y=Jvar, yend=Jvar)) +
  geom_vline(xintercept=0, linetype="dotted") + geom_point(aes(x=estimate, y=Jvar), col="black") +
  geom_segment(arrow = arrow(length = unit(0.00, "npc"))) +
  guides(size=FALSE) +
  scale_y_discrete(limits = sort(unique(modoutput$term)), labels=estimates) +
  xlab("Change in growing degree days to budburst") + ylab("") + theme_linedraw() +
  theme(legend.text=element_text(size=5), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="sans"), legend.position = "none",
        legend.text.align = 0,
        plot.margin = unit(c(1,1,1,1), "lines")) +  
  coord_cartesian(xlim=c(-50, 450), clip = 'off') + ggtitle("A. Weather Station")

muplot.hobo<-ggplot(modoutput, aes(x=`25%`, xend=`75%`, y=Jvar, yend=Jvar)) +
  geom_vline(xintercept=0, linetype="dotted") + geom_point(aes(x=estimate, y=Jvar), col="black") +
  geom_segment(arrow = arrow(length = unit(0.00, "npc"))) +
  guides(size=FALSE) +
  scale_y_discrete(limits = sort(unique(modoutput$term)), labels=estimates) +
  xlab("Change in growing degree days to budburst") + ylab("") + theme_linedraw() +
  theme(legend.text=element_text(size=5), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="sans"), legend.position = "none",
        legend.text.align = 0,
        plot.margin = unit(c(1,1,1,1), "lines")) +  
  coord_cartesian(xlim=c(-50, 450), clip = 'off') + ggtitle("B. Hobo Loggers")

quartz()
ggarrange(muplot.station, muplot.hobo, ncol=2)
