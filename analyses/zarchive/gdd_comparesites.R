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

bball.sub <- bball[(bball$year=="2019"),]
bball.sub <- subset(bball.sub, select=c(gdd_bb, urban, method, genus, species))
bball.sub <- bball.sub[(complete.cases(bball.sub)),]
bball.sub <- bball.sub[!is.na(bball.sub$gdd_bb),]
bball.sub$spp <- paste(bball.sub$genus, bball.sub$species, sep="_")

bball.sub <- bball.sub[(bball.sub$gdd_bb<=1000),]
bball.sub$species <- ifelse(bball.sub$genus=="Acer" & bball.sub$species=="rubra", "rubrum", bball.sub$species)

bball.sub$species.name <- paste(bball.sub$genus, bball.sub$species, sep=" ")


###########################################################
#### Now to first look at differences between sites... ####
###########################################################
samespp <- c("Acer rubrum", "Betula alleghaniensis","Fagus grandifolia", 
             "Hamamelis virginiana", "Quercus alba", "Quercus rubra")

bball.site <- bball.sub[(bball.sub$species.name%in%samespp),]

bball.site$type <- ifelse(bball.site$urban==1, "Arnold Arboretum", "Harvard Forest")
bball.site$codegdd <- reorder(bball.site$species.name, bball.site$gdd_bb)

#cols <- colorRampPalette(brewer.pal(3,"Dark2"))(2)
cols <- viridis_pal(option="plasma")(3)
gddcomparebb <- ggplot(bball.site, aes(x=codegdd, y=gdd_bb, fill=type)) + 
  geom_boxplot() +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        #legend.position = "none",
        axis.text.x = element_text(angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  xlab("") + 
  ylab("Growing degree days to budburst") + 
  scale_fill_manual(name="Site", values=cols,
                    labels=c("Arnold Arboretum", "Harvard Forest")) + coord_cartesian(expand=0, ylim=c(0,700))

pdf(file.path("~/Documents/git/microclimates/analyses/figures/", "gdd_sitecompare_boxplot.pdf"),
    width = 8, height = 6)
gddcomparebb
dev.off()

####################################
#### And now between methods... ####
####################################
badspp <- c("Carya glabra", "Carya ovata","Populus deltoides", 
             "Viburnum nudum")

bball.method <- bball.sub[!(bball.sub$species.name%in%badspp),]
bball.method$type <- ifelse(bball.method$method==1, "Weather Station", "Hobo Logger")
bball.method$codegdd <- reorder(bball.method$species.name, bball.method$gdd_bb)

cols <- viridis_pal(option="viridis")(3)
gddcomparebb <- ggplot(bball.method, aes(x=codegdd, y=gdd_bb, fill=type)) + 
  geom_boxplot() +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        #legend.position = "none",
        axis.text.x = element_text(angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        plot.margin = margin(0.5, 0.5, 0.5, 1, "cm")) +
  xlab("") + 
  ylab("Growing degree days to budburst") + 
  scale_fill_manual(name="Site", values=cols,
                    labels=c("Hobo Logger", "Weather Station")) + coord_cartesian(expand=0, ylim=c(0,700))

pdf(file.path("~/Documents/git/microclimates/analyses/figures/", "gdd_comparemethods.pdf"),
    width = 8, height = 6)
gddcomparebb
dev.off()

#########################################################################  
#### And finally, we want to plot the interaction between methods... #### 
#########################################################################
bball.site$methodtype <- ifelse(bball.site$method==1, "\nWeather \nStation", "\nHobo \nLogger")

cols <- viridis_pal(option="plasma")(3)
gddcomparebb <- ggplot(bball.site, aes(x=methodtype, y=gdd_bb, group=type, fill=type)) + 
  geom_ribbon(stat='smooth', method = "lm", se=TRUE, alpha=1, 
              aes(fill = type, group = type)) +
  geom_line(stat='smooth', method = "lm", alpha=1, col="black") +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.key = element_rect(colour = "transparent", fill = "white"),
        plot.margin = margin(0.5, 0.5, 0.5, 1, "cm")) +
  xlab("") + 
  ylab("Growing degree days to budburst") + 
  scale_fill_manual(name="Site", values=cols,
                    labels=c("Arnold Arboretum", "Harvard Forest")) + 
  coord_cartesian(expand=0, ylim=c(0,700))

pdf(file.path("~/Documents/git/microclimates/analyses/figures/", "gdd_comparemethodsandsites.pdf"),
    width = 8, height = 6)
gddcomparebb
dev.off()
