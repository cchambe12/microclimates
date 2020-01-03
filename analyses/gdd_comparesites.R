### Started 5 Dec 2019 - Cat
## Compare GDDs across sites within species

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
#gdd.stan <- read.csv("output/clean_gdd_bbanddvr.csv", header=TRUE)
gdd.hobo <- read.csv("output/clean_gdd_bbanddvr_hobo.csv", header=TRUE)

gdd.hobo <- gdd.hobo[(gdd.hobo$type=="Treespotters" | gdd.hobo$type=="Harvard Forest"),]

gdd.hobo$spp <- paste(substr(gdd.hobo$genus, 0, 3), substr(gdd.hobo$species, 0, 3), sep="")
#gdd.stan$spp <- paste(substr(gdd.stan$genus, 0, 3), substr(gdd.stan$species, 0, 3), sep="")

gdd.hobo$gddmean <- ave(gdd.hobo$gdd_lo, gdd.hobo$spp, gdd.hobo$type)
gdd.hobo$gddsd <- ave(gdd.hobo$gdd_lo, gdd.hobo$spp, gdd.hobo$type, FUN=sd)
gdd.hobo$ymin <- gdd.hobo$gddmean-gdd.hobo$gddsd
gdd.hobo$ymax <- gdd.hobo$gddmean+gdd.hobo$gddsd

gdd.hobo$species.name <- NA
gdd.hobo$species.name <- ifelse(gdd.hobo$spp=="Acepen", "Acer pensylvanicum", gdd.hobo$species.name)
gdd.hobo$species.name <- ifelse(gdd.hobo$spp=="Acerub", "Acer rubrum", gdd.hobo$species.name)
gdd.hobo$species.name <- ifelse(gdd.hobo$spp=="Acesac", "Acer saccharum", gdd.hobo$species.name)
gdd.hobo$species.name <- ifelse(gdd.hobo$spp=="Aesfla", "Aesculus flava", gdd.hobo$species.name)
gdd.hobo$species.name <- ifelse(gdd.hobo$spp=="Betall", "Betula alleghaniensis", gdd.hobo$species.name)
gdd.hobo$species.name <- ifelse(gdd.hobo$spp=="Betnig", "Betula nigra", gdd.hobo$species.name)
gdd.hobo$species.name <- ifelse(gdd.hobo$spp=="Cargla", "Carya glabra", gdd.hobo$species.name)
gdd.hobo$species.name <- ifelse(gdd.hobo$spp=="Carova", "Carya ovata", gdd.hobo$species.name)
gdd.hobo$species.name <- ifelse(gdd.hobo$spp=="Faggra", "Fagus grandifolia", gdd.hobo$species.name)
gdd.hobo$species.name <- ifelse(gdd.hobo$spp=="Fraame", "Fraxinus americana", gdd.hobo$species.name)
gdd.hobo$species.name <- ifelse(gdd.hobo$spp=="Hamvir", "Hamamelis virginiana", gdd.hobo$species.name)
gdd.hobo$species.name <- ifelse(gdd.hobo$spp=="Popdel", "Populus deltoides", gdd.hobo$species.name)
gdd.hobo$species.name <- ifelse(gdd.hobo$spp=="Quealb", "Quercus alba", gdd.hobo$species.name)
gdd.hobo$species.name <- ifelse(gdd.hobo$spp=="Querub", "Quercus rubra", gdd.hobo$species.name)
gdd.hobo$species.name <- ifelse(gdd.hobo$spp=="Tilame", "Tilia americana", gdd.hobo$species.name)
gdd.hobo$species.name <- ifelse(gdd.hobo$spp=="Vaccor", "Vaccinium corymbosum", gdd.hobo$species.name)
gdd.hobo$species.name <- ifelse(gdd.hobo$spp=="Vibnud", "Viburnum nudum", gdd.hobo$species.name)

samespp <- c("Acer rubrum", "Betula alleghaniensis","Fagus grandifolia", 
             "Hamamelis virginiana", "Quercus alba", "Quercus rubra")

gdd.hobo <- gdd.hobo[(gdd.hobo$species.name%in%samespp),]


cols <- colorRampPalette(brewer.pal(3,"Dark2"))(2)
gddcomparebb <- ggplot(gdd.hobo, aes(x=species.name, y=gddmean, fill=type)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=ymin, ymax=ymax),width = 0.2, position=position_dodge(0.9)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        #legend.position = "none",
        axis.text.x = element_text(angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  xlab("") + 
  ylab("Growing degree days to budburst") + 
  scale_fill_manual(name="Site", values=cols,
                    labels=c("Harvard Forest", "Treespotters")) + coord_cartesian(expand=c(0,0), ylim=c(0,700))

quartz()
gddcomparebb
