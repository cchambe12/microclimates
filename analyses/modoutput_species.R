## 19 May 2021 - Cat
# Hoping to disentangle model output of species differences in GDD until budburst

rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(egg)
library(viridis)
library(rstan)


# Set Working Directory
setwd("~/Documents/git/microclimates/analyses")

bball <- read.csv("output/cleanmicro_gdd_2019.csv")

bball$spp <- ifelse(bball$spp=="Acer_rubra", "Acer_rubrum", bball$spp)
bball$species <- ifelse(bball$species=="rubra", "rubrum", bball$species)

bball$species_name <- paste(bball$genus, bball$species, sep=" ")



bball$site <- ifelse(bball$urban==1, "arb", "hf")
bball$tx <- ifelse(bball$method==1, "ws", "hobo")


##### Method comparisons using real data...

bbhobo <- bball[(bball$tx=="hobo"),]
bbws <- bball[(bball$tx=="ws"),]

bbhobo$meanhobo <- ave(bbhobo$gdd, bbhobo$species_name, FUN=function(x) mean(x, na.rm=TRUE))
bbhobo$sdhobo <- ave(bbhobo$gdd, bbhobo$species_name, FUN=function(x) sd(x, na.rm=TRUE))/sqrt(length(unique(bbhobo$meanhobo)))
bbws$meanws <- ave(bbws$gdd, bbws$species_name, FUN=function(x) mean(x, na.rm=TRUE))
bbws$sdws <- ave(bbws$gdd, bbws$species_name, FUN=function(x) sd(x, na.rm=TRUE))/sqrt(length(unique(bbws$meanws)))

bbhobo <- subset(bbhobo , select=c("species_name", "meanhobo", "sdhobo"))
bbhobo <- bbhobo[!duplicated(bbhobo),]
bbws <- subset(bbws , select=c("species_name", "meanws", "sdws"))
bbws <- bbws[!duplicated(bbws),]

bbdiff <- full_join(bbhobo, bbws)

bbdiff$diff <- bbdiff$meanhobo - bbdiff$meanws
bbdiff$diff.sd <- bbdiff$sdhobo - bbdiff$sdws


bbdiff$ymin <- bbdiff$meanws - bbdiff$sdws
bbdiff$ymax <- bbdiff$meanws + bbdiff$sdws
bbdiff$xmin <- bbdiff$meanhobo - bbdiff$sdhobo
bbdiff$xmax <- bbdiff$meanhobo + bbdiff$sdhobo

bbdiff$diff.labels <- as.numeric(as.factor(bbdiff$diff))

cols <- viridis_pal(option="viridis")(17)
methods_real <- ggplot(bbdiff, aes(x=meanhobo, y=meanws, col=as.factor(species_name))) + 
  geom_point(aes(x=meanhobo, y=meanws)) + 
  geom_linerange(aes(ymin=ymin, ymax=ymax)) +
  geom_errorbarh(aes(xmin = xmin, xmax = xmax, height = 0)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.position = "none",
        legend.text = element_text(face="italic"),
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  xlab("GDDs until budburst \n(hobo logger)") + 
  ylab("GDDs until budburst \n(weather station)") + 
  geom_abline(slope=1, intercept=0, linetype="dashed", color="black") +
  ggtitle("c)") +
  scale_color_manual(name=expression(Delta*" in GDDs"), values=cols,
                   labels=c(sort(unique(bbdiff$species_name)))) +
  coord_cartesian(xlim=c(200, 600), ylim=c(200, 600))


##### Site comparisons using real data...

bburb <- bball[(bball$site=="arb"),]
bbhf <- bball[(bball$site=="hf"),]

bburb$meanurb <- ave(bburb$gdd, bburb$species_name, FUN=function(x) mean(x, na.rm=TRUE))
bburb$sdurb <- ave(bburb$gdd, bburb$species_name, FUN=function(x) sd(x, na.rm=TRUE))/sqrt(length(unique(bburb$meanurb)))
bbhf$meanhf <- ave(bbhf$gdd, bbhf$species_name, FUN=function(x) mean(x, na.rm=TRUE))
bbhf$sdhf <- ave(bbhf$gdd, bbhf$species_name, FUN=function(x) sd(x, na.rm=TRUE))/sqrt(length(unique(bbhf$meanhf)))

bburb <- subset(bburb , select=c("species_name", "meanurb", "sdurb"))
bburb <- bburb[!duplicated(bburb),]
bbhf <- subset(bbhf , select=c("species_name", "meanhf", "sdhf"))
bbhf <- bbhf[!duplicated(bbhf),]

bbdiff <- full_join(bburb, bbhf)

bbdiff <- na.omit(bbdiff)

bbdiff$diff <- bbdiff$meanurb - bbdiff$meanhf
bbdiff$diff.sd <- bbdiff$sdurb - bbdiff$sdhf


bbdiff$ymin <- bbdiff$meanhf - bbdiff$sdhf
bbdiff$ymax <- bbdiff$meanhf + bbdiff$sdhf
bbdiff$xmin <- bbdiff$meanurb - bbdiff$sdurb
bbdiff$xmax <- bbdiff$meanurb + bbdiff$sdurb

bbdiff$diff.labels <- as.numeric(as.factor(bbdiff$diff))

cols <- viridis_pal(option="viridis")(7)
sites_real <- ggplot(bbdiff, aes(x=meanurb, y=meanhf, col=as.factor(species_name))) + 
  geom_point(aes(x=meanurb, y=meanhf)) + 
  geom_linerange(aes(ymin=ymin, ymax=ymax)) +
  geom_errorbarh(aes(xmin = xmin, xmax = xmax, height = 0)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.position = "none",
        legend.text = element_text(face ="italic"),
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  xlab("GDDs until budburst \n(urban arboretum)") + 
  ylab("GDDs until budburst \n(rural forest)") + 
  geom_abline(slope=1, intercept=0, linetype="dashed", color="black") +
  ggtitle("a)") +
  scale_color_manual(name=expression(Delta*" in GDDs"), values=cols,
                     labels=c(sort(unique(bbdiff$species_name)))) +
  coord_cartesian(xlim=c(200, 600), ylim=c(200, 600))


pdf(file.path("~/Documents/git/microclimates/analyses/figures/", "zarchive/speciesdiffs_real.pdf"),
    width = 12, height = 6, onefile=FALSE)
ggarrange(sites_real, methods_real, ncol=2)
dev.off()


######## ##### Okay, on to the model output...
load("stan/urbmethod_real.Rdata")

urbmethod.sum <- summary(urbmethod)$summary

bball$sp.num <- as.numeric(as.factor(bball$spp))
bbhf <- bball[(bball$site=="hf"),]
unique(bbhf$sp.num)

sppbynum <- subset(bball, select=c("spp", "sp.num"))
sppbynum <- sppbynum[!duplicated(sppbynum),]

hfs <- rbind(urbmethod.sum["a_sp[3]", ], urbmethod.sum["a_sp[1]", ],  
                urbmethod.sum["a_sp[11]", ], urbmethod.sum["a_sp[9]", ],
             urbmethod.sum["a_sp[2]", ], urbmethod.sum["a_sp[5]", ],
             urbmethod.sum["a_sp[14]", ], urbmethod.sum["a_sp[10]", ],
             urbmethod.sum["a_sp[13]", ])

hfspp <- data.frame(hfs)
hfspp$site <- "hf"
hfspp$spp <- c("Acer saccharum", "Acer pensylvanicum", "Hamamelis virginiana", "Fagus grandifolia",
                  "Acer rubrum", "Betula alleghaniensis", "Quercus rubra", "Fraxinus americana", "Quercus alba")

bbarb <- bball[(bball$site=="arb"),]
unique(bbarb$sp.num)

urbs <- rbind((urbmethod.sum["a_sp[15]", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[15]", ]), 
              (urbmethod.sum["a_sp[5]", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[5]", ]),  
               (urbmethod.sum["a_sp[6]", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[6]", ]), 
              (urbmethod.sum["a_sp[3]", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[3]", ]), 
              (urbmethod.sum["a_sp[13]", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[13]", ]),
              (urbmethod.sum["a_sp[12]", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[12]", ]), 
               (urbmethod.sum["a_sp[4]", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[4]", ]), 
              (urbmethod.sum["a_sp[9]", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[9]", ]), 
              (urbmethod.sum["a_sp[7]", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[7]", ]), 
              (urbmethod.sum["a_sp[8]", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[8]", ]), 
               (urbmethod.sum["a_sp[14]", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[14]", ]), 
              (urbmethod.sum["a_sp[2]", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[2]", ]),
              (urbmethod.sum["a_sp[11]", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[11]", ]),
              (urbmethod.sum["a_sp[16]", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[16]", ]),
               (urbmethod.sum["a_sp[17]", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[17]", ]))

arbspp <- data.frame(urbs)
arbspp$site <- "arb"
arbspp$spp <- c("Tilia americana", "Betula alleghaniensis", "Betula nigra", "Acer saccharum",
               "Quercus alba", "Populus deltoides", "Populus deltoides", "Fagus grandifolia", "Carya glabra",
               "Carya ovata", "Quercus rubra", "Acer rubrum", "Hamamelis virginiana", "Vaccinium corymbosum",
               "Viburnum nudum")



bball <- data.frame(rbind(hfspp, arbspp))
bball <- subset(bball, select=c("mean", "sd", "site", "spp"))
colnames(bball) <- c("gdd", "gdd.sd", "site", "species")
rownames(bball) <- 1:nrow(bball)


bball$gdd <- as.numeric(bball$gdd)

bburb <- bball[(bball$site=="arb"),]
bbhf <- bball[(bball$site=="hf"),]

bburb$meanurb <- bburb$gdd
bburb$sdurb <- bburb$gdd.sd
bbhf$meanhf <- bbhf$gdd
bbhf$sdhf <- bbhf$gdd.sd

bburb <- subset(bburb , select=c("species", "meanurb", "sdurb"))
bburb <- bburb[!duplicated(bburb),]
bbhf <- subset(bbhf , select=c("species", "meanhf", "sdhf"))
bbhf <- bbhf[!duplicated(bbhf),]

bbdiff <- full_join(bburb, bbhf)
bbdiff <- na.omit(bbdiff)

bbdiff$diff <- bbdiff$meanurb - bbdiff$meanhf
bbdiff$diff.sd <- bbdiff$sdurb - bbdiff$sdhf


bbdiff$ymin <- bbdiff$meanhf - bbdiff$sdhf
bbdiff$ymax <- bbdiff$meanhf + bbdiff$sdhf
bbdiff$xmin <- bbdiff$meanurb - bbdiff$sdurb
bbdiff$xmax <- bbdiff$meanurb + bbdiff$sdurb

#bbdiff$diff.labels <- 1:4

cols <- viridis_pal(option="viridis")(7)
sites_mod <- ggplot(bbdiff, aes(x=meanurb, y=meanhf, col=species)) + 
  geom_point(aes(x=meanurb, y=meanhf)) + 
  geom_linerange(aes(ymin=ymin, ymax=ymax)) +
  geom_errorbarh(aes(xmin = xmin, xmax = xmax, height = 0)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.text = element_text(face = "italic"),
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  #geom_text(aes(label=species), vjust=2) + 
  xlab("GDDs until budburst \n(urban arboretum)") + 
  ylab("GDDs until budburst \n(rural forest)") + 
  ggtitle("b)") +
  geom_abline(intercept=0, slope=1, linetype="dashed") +
  scale_color_manual(name=expression(Delta*" in GDDs"), values=cols,
                     labels=c(sort(unique(bbdiff$species)))) +
  coord_cartesian(xlim=c(200, 600), ylim=c(200, 600))

pdf(file.path("~/Documents/git/microclimates/analyses/figures/", "zarchive/speciesdiffs_site.pdf"),
    width = 12, height = 6, onefile=FALSE)
ggarrange(sites_real, sites_mod, ncol=2)
dev.off()


######## ##### Okay, on to the model output for method...
bball <- read.csv("output/cleanmicro_gdd_2019.csv")

bball$spp <- ifelse(bball$spp=="Acer_rubra", "Acer_rubrum", bball$spp)
bball$species <- ifelse(bball$species=="rubra", "rubrum", bball$species)

bball$species_name <- paste(bball$genus, bball$species, sep=" ")



bball$site <- ifelse(bball$urban==1, "arb", "hf")
bball$tx <- ifelse(bball$method==1, "ws", "hobo")

bball$sp.num <- as.numeric(as.factor(bball$spp))
bbhobo <- bball[(bball$tx=="hobo"),]
sort(unique(bbhobo$sp.num))

sppbynum <- subset(bball, select=c("spp", "sp.num"))
sppbynum <- sppbynum[!duplicated(sppbynum),]

hobos <- rbind(urbmethod.sum["a_sp[1]", ], urbmethod.sum["a_sp[2]", ],  
             urbmethod.sum["a_sp[3]", ], urbmethod.sum["a_sp[4]", ],
             urbmethod.sum["a_sp[5]", ], urbmethod.sum["a_sp[6]", ],
             urbmethod.sum["a_sp[7]", ], urbmethod.sum["a_sp[8]", ],
             urbmethod.sum["a_sp[9]", ], urbmethod.sum["a_sp[10]", ],
             urbmethod.sum["a_sp[11]", ], urbmethod.sum["a_sp[12]", ],
             urbmethod.sum["a_sp[13]", ], urbmethod.sum["a_sp[14]", ],
             urbmethod.sum["a_sp[15]", ], urbmethod.sum["a_sp[16]", ],
             urbmethod.sum["a_sp[17]", ])

hobospp <- data.frame(hobos)
hobospp$site <- "hobo"
hobospp$spp <- sort(unique(bball$species_name))

bbws <- bball[(bball$tx=="ws"),]
unique(bbws$sp.num)

ws <- rbind((urbmethod.sum["a_sp[1]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[1]", ]), 
              (urbmethod.sum["a_sp[2]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[2]", ]),  
              (urbmethod.sum["a_sp[3]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[3]", ]), 
              (urbmethod.sum["a_sp[4]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[4]", ]), 
              (urbmethod.sum["a_sp[5]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[5]", ]),
              (urbmethod.sum["a_sp[6]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[6]", ]), 
              (urbmethod.sum["a_sp[7]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[7]", ]), 
              (urbmethod.sum["a_sp[8]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[8]", ]), 
              (urbmethod.sum["a_sp[9]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[9]", ]), 
              (urbmethod.sum["a_sp[10]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[10]", ]), 
              (urbmethod.sum["a_sp[11]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[11]", ]), 
              (urbmethod.sum["a_sp[12]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[12]", ]),
              (urbmethod.sum["a_sp[13]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[13]", ]),
              (urbmethod.sum["a_sp[14]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[14]", ]),
              (urbmethod.sum["a_sp[15]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[15]", ]),
              (urbmethod.sum["a_sp[16]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[16]", ]),
              (urbmethod.sum["a_sp[17]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[17]", ]))

wsspp <- data.frame(ws)
wsspp$site <- "ws"
wsspp$spp <- sort(unique(bball$species_name))



bball <- data.frame(rbind(hobospp, wsspp))
bball <- subset(bball, select=c("mean", "sd", "site", "spp"))
colnames(bball) <- c("gdd", "gdd.sd", "site", "species")
rownames(bball) <- 1:nrow(bball)


bball$gdd <- as.numeric(bball$gdd)

bbws <- bball[(bball$site=="ws"),]
bbhobo <- bball[(bball$site=="hobo"),]

bbws$meanws <- bbws$gdd
bbws$sdws <- bbws$gdd.sd
bbhobo$meanhobo <- bbhobo$gdd
bbhobo$sdhobo <- bbhobo$gdd.sd

bbws <- subset(bbws , select=c("species", "meanws", "sdws"))
bbws <- bbws[!duplicated(bbws),]
bbhobo <- subset(bbhobo , select=c("species", "meanhobo", "sdhobo"))
bbhobo <- bbhobo[!duplicated(bbhobo),]

bbdiff <- full_join(bbws, bbhobo)
bbdiff <- na.omit(bbdiff)

bbdiff$diff <- bbdiff$meanws - bbdiff$meanhobo
bbdiff$diff.sd <- bbdiff$sdws - bbdiff$sdhobo


bbdiff$ymin <- bbdiff$meanhobo - bbdiff$sdhobo
bbdiff$ymax <- bbdiff$meanhobo + bbdiff$sdhobo
bbdiff$xmin <- bbdiff$meanws - bbdiff$sdws
bbdiff$xmax <- bbdiff$meanws + bbdiff$sdws

#bbdiff$diff.labels <- 1:4

cols <- viridis_pal(option="viridis")(17)
method_mod <- ggplot(bbdiff, aes(x=meanws, y=meanhobo, col=species)) + 
  geom_point(aes(x=meanws, y=meanhobo)) + 
  geom_linerange(aes(ymin=ymin, ymax=ymax)) +
  geom_errorbarh(aes(xmin = xmin, xmax = xmax, height = 0)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.text = element_text(face = "italic"),
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  #geom_text(aes(label=species), vjust=2) + 
  xlab("GDDs until budburst \n(hobo logger)") + 
  ylab("GDDs until budburst \n(weather station)") + 
  ggtitle("d)") +
  geom_abline(intercept=0, slope=1, linetype="dashed") +
  scale_color_manual(name=expression(Delta*" in GDDs"), values=cols,
                     labels=c(sort(unique(bbdiff$species)))) +
  coord_cartesian(xlim=c(200, 600), ylim=c(200, 600))

pdf(file.path("~/Documents/git/microclimates/analyses/figures/", "zarchive/speciesdiffs_method.pdf"),
    width = 12, height = 6, onefile=FALSE)
ggarrange(methods_real, method_mod, ncol=2)
dev.off()



#### One big file:
pdf(file.path("~/Documents/git/microclimates/analyses/figures/", "speciesdiffs.pdf"),
    width = 12, height = 12, onefile=FALSE)
ggarrange(sites_real, sites_mod, methods_real, method_mod, ncol=2)
dev.off()
