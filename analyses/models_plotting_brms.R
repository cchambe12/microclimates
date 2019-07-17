### Plotting the model output for all gdds
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load libraries
library(RColorBrewer)
library(brms)
library(broom)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Set working directory
setwd("~/Documents/git/microclimates/analyses")

## load the model
load("output/stan/modgddbb.Rdata")


library(broom)
gddbb<-as.data.frame(tidy(modgddbb, prob=0.5))
modoutput <- gddbb #modelhere

modoutput<-modoutput[2:XXXX,]
modoutput$`25%` <- modoutput$lower
modoutput$`75%` <- modoutput$upper
modoutput$term<-gsub(".*b_","",modoutput$term)
modoutput$term<-gsub(".*spp","",modoutput$term)
modoutput$term <- ifelse(modoutput$term=="provenance.lat", "xlat", modoutput$term)
modoutput$Jvar <- as.numeric(rev(as.factor(modoutput$term)))

******estimates<-c("Acer saccharum", "Aesculus flava", "Betula alleghaniense", "Betula nigra", "Carya glabra", "Carya ovata",
             "Fagus grandifolia", "Hamamelis virginiana", "Populus deltoides", "Quercus alba", "Quercus rubra", "Tilia americana",
             "Vaccinium corymbosum", "Viburnum nudum", "Harvard Forest", "Common Garden", "Provenance Latitude")
estimates<-rev(estimates)
modoutput <- modoutput[!is.na(modoutput$Jvar),]
muplot<-ggplot(modoutput, aes(x=`25%`, xend=`75%`, y=Jvar, yend=Jvar)) +
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
        plot.margin = unit(c(0,1,1,1), "lines")) +  
  coord_cartesian(xlim=c(-200, 70), ylim=c(1,XXXX), clip = 'off') 

quartz()
muplot
