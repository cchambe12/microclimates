### Started 15 July 2019 - Cat
## Work on making some plots!

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load libraries
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

# Set working directory
setwd("~/Documents/git/microclimates/analyses")

## Load the data
gdd.stan <- read.csv("output/clean_gdd_bbanddvr.csv", header=TRUE)
gdd.hobo <- read.csv("output/clean_gdd_bbanddvr_hobo.csv", header=TRUE)

if(FALSE){ ## Testing code
gdd.stan$stationbb <- gdd.stan$gdd_bb
gdd.stan$gdd_bb <- NULL
gdd.stan$stationdvr <- gdd.stan$gdd_dvr
gdd.stan$gdd_dvr <- NULL
gdd <- left_join(gdd.hobo, gdd.stan)
gdd$diffbb <- gdd$stationbb - gdd$gdd_bb
gdd$diffdvr <- gdd$stationdvr - gdd$gdd_dvr
range(gdd$diffdvr, na.rm=TRUE)
range(gdd$diffbb, na.rm=TRUE)
}


gdd.stan <- gdd.stan[(gdd.stan$type=="Treespotters"),] ### CHANGE BASED ON TYPE
gdd.hobo <- gdd.hobo[(gdd.hobo$type=="Treespotters"),]

gdd.hobo$gdd_bb_hobo <- gdd.hobo$gdd_bb
gdd.hobo$gdd_dvr_hobo <- gdd.hobo$gdd_dvr

gdd.hobo$gdd_bb <- NULL
gdd.hobo$gdd_dvr <- NULL

gdd.hobo$spp <- paste(substr(gdd.hobo$genus, 0, 3), substr(gdd.hobo$species, 0, 3), sep="")
gdd.stan$spp <- paste(substr(gdd.stan$genus, 0, 3), substr(gdd.stan$species, 0, 3), sep="")

commoncols <- c("id", "provenance.lat", "fs.count", "year", "budburst", "leafout", "spp", "climatetype")
gdd.hobo <- subset(gdd.hobo, select=c(commoncols, "gdd_bb_hobo", "gdd_dvr_hobo"))
gdd.stan <- subset(gdd.stan, select=c(commoncols, "gdd_bb", "gdd_dvr"))

gdd.hobo$hobonum <- gdd.hobo$climatetype
gdd.hobo$climatetype <- NULL

gdd <- full_join(gdd.stan, gdd.hobo)
gdd$dvr <- gdd$leafout - gdd$budburst
gdd$leafout <- NULL

gdd <- gdd[(gdd$year==2019),]

gdd <- gdd[(gdd$gdd_bb<=1000),]
gdd <- gdd[(gdd$gdd_bb_hobo<=1000),]

gdd$gdd.diffdvr <- gdd$gdd_dvr_hobo - gdd$gdd_dvr ## mean=3.81; range=c(-1.24, 12.17); sd=3.77
gdd$gdd.diffbb <- gdd$gdd_bb_hobo - gdd$gdd_bb ## mean = 5.18; range=c(3.48, 9.95); sd=1.82

gddplot.bb <- subset(gdd, select=c("provenance.lat", "fs.count", "spp", "gdd_bb", "gdd_bb_hobo", "budburst", "hobonum", "dvr"))
gddplot.dvr <- subset(gdd, select=c("provenance.lat", "fs.count", "spp", "gdd_dvr", "gdd_dvr_hobo", "budburst", "hobonum", "dvr"))
gddplot.bb <- na.omit(gddplot.bb)
gddplot.dvr <- na.omit(gddplot.dvr)

gddplot.bb <- gather(gddplot.bb, "method", "gdd", -provenance.lat, -fs.count, -spp, -budburst, -hobonum, -dvr)
gddplot.dvr <- gather(gddplot.dvr, "method", "gdd", -provenance.lat, -fs.count, -spp, -budburst, -hobonum, -dvr)

gddplot.bb$method <- ifelse(gddplot.bb$method=="gdd_bb", gddplot.bb$method, gddplot.bb$hobonum)
gddplot.dvr$method <- ifelse(gddplot.dvr$method=="gdd_dvr", gddplot.dvr$method, gddplot.dvr$hobonum)

gddplot.bb$method.name <- ifelse(gddplot.bb$method!="gdd_bb", "hobo", gddplot.bb$method)

gddplot.bb$gddmean <- ave(gddplot.bb$gdd, gddplot.bb$spp, gddplot.bb$method)
gddplot.bb$gddsd <- ave(gddplot.bb$gdd, gddplot.bb$spp, gddplot.bb$method, FUN=sd)
gddplot.bb$ymin <- gddplot.bb$gddmean-gddplot.bb$gddsd
gddplot.bb$ymax <- gddplot.bb$gddmean+gddplot.bb$gddsd

gddplot.dvr$gddmean <- ave(gddplot.dvr$gdd, gddplot.dvr$spp, gddplot.dvr$method)
gddplot.dvr$gddsd <- ave(gddplot.dvr$gdd, gddplot.dvr$spp, gddplot.dvr$method, FUN=sd)
gddplot.dvr$ymin <- gddplot.dvr$gddmean-gddplot.dvr$gddsd
gddplot.dvr$ymax <- gddplot.dvr$gddmean+gddplot.dvr$gddsd

gddplot.bb$species.name <- NA
gddplot.bb$species.name <- ifelse(gddplot.bb$spp=="Acepen", "Acer pensylvanicum", gddplot.bb$species.name)
gddplot.bb$species.name <- ifelse(gddplot.bb$spp=="Acerub", "Acer rubrum", gddplot.bb$species.name)
gddplot.bb$species.name <- ifelse(gddplot.bb$spp=="Acesac", "Acer saccharum", gddplot.bb$species.name)
gddplot.bb$species.name <- ifelse(gddplot.bb$spp=="Aesfla", "Aesculus flava", gddplot.bb$species.name)
gddplot.bb$species.name <- ifelse(gddplot.bb$spp=="Betall", "Betula alleghaniensis", gddplot.bb$species.name)
gddplot.bb$species.name <- ifelse(gddplot.bb$spp=="Betnig", "Betula nigra", gddplot.bb$species.name)
gddplot.bb$species.name <- ifelse(gddplot.bb$spp=="Cargla", "Carya glabra", gddplot.bb$species.name)
gddplot.bb$species.name <- ifelse(gddplot.bb$spp=="Carova", "Carya ovata", gddplot.bb$species.name)
gddplot.bb$species.name <- ifelse(gddplot.bb$spp=="Faggra", "Fagus grandifolia", gddplot.bb$species.name)
gddplot.bb$species.name <- ifelse(gddplot.bb$spp=="Fraame", "Fraxinus americana", gddplot.bb$species.name)
gddplot.bb$species.name <- ifelse(gddplot.bb$spp=="Hamvir", "Hamamelis virginiana", gddplot.bb$species.name)
gddplot.bb$species.name <- ifelse(gddplot.bb$spp=="Popdel", "Populus deltoides", gddplot.bb$species.name)
gddplot.bb$species.name <- ifelse(gddplot.bb$spp=="Quealb", "Quercus alba", gddplot.bb$species.name)
gddplot.bb$species.name <- ifelse(gddplot.bb$spp=="Querub", "Quercus rubra", gddplot.bb$species.name)
gddplot.bb$species.name <- ifelse(gddplot.bb$spp=="Tilame", "Tilia americana", gddplot.bb$species.name)
gddplot.bb$species.name <- ifelse(gddplot.bb$spp=="Vaccor", "Vaccinium corymbosum", gddplot.bb$species.name)
gddplot.bb$species.name <- ifelse(gddplot.bb$spp=="Vibnud", "Viburnum nudum", gddplot.bb$species.name)

cols <- colorRampPalette(brewer.pal(3,"Dark2"))(2)
gddbarbb <- ggplot(gddplot.bb, aes(x=species.name, y=gddmean, fill=method.name)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=ymin, ymax=ymax),width = 0.2, position=position_dodge(0.9)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        #legend.position = "none",
        axis.text.x = element_text(angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  xlab("") + 
  ylab("Growing degree days to budburst") + 
  scale_fill_manual(name="Method", values=cols,
                    labels=c("Weather Station", "Hobo Logger")) + coord_cartesian(expand=c(0,0), ylim=c(0,600))
  
quartz()
gddbarbb

if(FALSE){
gddbardvr <- ggplot(gddplot.dvr, aes(x=spp, y=gddmean, fill=method)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=ymin, ymax=ymax),width = 0.2, position=position_dodge(0.9)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        #legend.position = "none",
        axis.text.x = element_text(angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  xlab("") + 
  ylab("Growing degree days from budburst to leafout") + 
  scale_fill_manual(name="Method", values=cols,
                    labels=c("Weather Station", "Hobo Logger")) + coord_cartesian(expand=c(0,0))

quartz()
gddbardvr
}

cols <- viridis_pal(option="C")(11) ### 10 for HF, 11 for Arb
#cols <-cols[-c(1:2)] ### for HF ONLY!
#cols <- colorRampPalette(brewer.pal(11, "Accent"))(11)
cols <- c(cols, "black")

if(FALSE){
gddplot.bb$method<-ifelse(gddplot.bb$method=="arb2", "arb02", gddplot.bb$method)
gddplot.bb$method<-ifelse(gddplot.bb$method=="arb3", "arb03", gddplot.bb$method)
gddplot.bb$method<-ifelse(gddplot.bb$method=="arb4", "arb04", gddplot.bb$method)
gddplot.bb$method<-ifelse(gddplot.bb$method=="arb7", "arb07", gddplot.bb$method)
gddplot.bb$method<-ifelse(gddplot.bb$method=="arb8", "arb08", gddplot.bb$method)
gddplot.bb$method<-ifelse(gddplot.bb$method=="arb9", "arb09", gddplot.bb$method)
}

gddplot.bb$method<-ifelse(gddplot.bb$method=="hf1", "hf01", gddplot.bb$method)
gddplot.bb$method<-ifelse(gddplot.bb$method=="hf3", "hf03", gddplot.bb$method)
gddplot.bb$method<-ifelse(gddplot.bb$method=="hf4", "hf04", gddplot.bb$method)
gddplot.bb$method<-ifelse(gddplot.bb$method=="hf5", "hf05", gddplot.bb$method)
gddplot.bb$method<-ifelse(gddplot.bb$method=="hf6", "hf06", gddplot.bb$method)
gddplot.bb$method<-ifelse(gddplot.bb$method=="hf7", "hf07", gddplot.bb$method)
gddplot.bb$method<-ifelse(gddplot.bb$method=="hf9", "hf09", gddplot.bb$method)

if(FALSE){
bb.loggers <- ggplot(gddplot.bb, aes(x=budburst, y=gdd, col=method, linetype=method)) + geom_point() +
  geom_line() + scale_color_manual(name="Method", values=cols,
                                   labels=c("arb02"="#02 - Lindens",
                                            "arb03"="#03 - Maples",
                                            "arb04"="#04 - Buckeyes",
                                            #"arb5"="#5 - Rose Collection",
                                            "arb07"="#07 - Centre Street Gate",
                                            "arb08"="#08 - Hickories",
                                            "arb09"="#09 - Oaks",
                                            "arb10"="#10 - Hemlock Hill", 
                                            "arb11"="#11 - Peter's Hill (Linden)",
                                            "arb12"="#12 - Cottonwoods", 
                                            "arb14"="#14 - Peter's Hill (Oak)",
                                            "arb15"="#15 - Peter's Hill (top)", 
                                            "gdd_bb"="Weather Station")) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        #legend.position = "none",
        #axis.text.x = element_text(angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  xlab("Day of budburst") + ylab("Growing degree days") + guides(linetype=FALSE) +
  coord_cartesian(ylim=c(100,700))

quartz()
bb.loggers
}

gddplot.bb$method <- ifelse(gddplot.bb$method=="gdd_bb", "zgdd_bb", gddplot.bb$method)
bb.loggers <- ggplot(gddplot.bb, aes(x=budburst, y=gdd, col=method, linetype=method)) + geom_point() +
  geom_line() + scale_color_manual(name="Method", values=cols,
                                   labels=c("hf01"="Hobo #01",
                                            "hf03"="Hobo #03",
                                            "hf04"="Hobo #04",
                                            "hf05"="Hobo #05",
                                            "hf06"="Hobo #06",
                                            "hf07"="Hobo #07",
                                            "hf09"="Hobo #09",
                                            "hf11"="Hobo #11", 
                                            "hf12"="Hobo #12",
                                            "hf13"="Hobo #13",
                                            "zgdd_bb"="Weather Station")) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        #legend.position = "none",
        #axis.text.x = element_text(angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  xlab("Day of budburst") + ylab("Growing degree days") + guides(linetype=FALSE) +
  coord_cartesian(ylim=c(100,700))

quartz()
bb.loggers

dvr.loggers <- ggplot(gddplot.dvr, aes(x=dvr, y=gdd, col=method, linetype=method)) + geom_point() +
  geom_line() + scale_color_manual(name="Method", values=cols, 
                                   labels=c("Hobo #10", "Hobo #11",
                                            "Hobo #12", "Hobo #14",
                                            "Hobo #15", "Hobo #2",
                                            "Hobo #3", "Hobo #4",
                                            "Hobo #7", "Hobo #8",
                                            "Hobo #9", "Weather Station")) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        #legend.position = "none",
        #axis.text.x = element_text(angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  xlab("Days from budburst to leafout") + ylab("Growing degree days") + guides(linetype=FALSE)

quartz()
dvr.loggers
