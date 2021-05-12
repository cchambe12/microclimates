## 5 May 2021 - Cat
# Hoping to disentangle model output of tree and shrub differences in GDD until budburst

rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(rstan)

# Set Working Directory
setwd("~/Documents/git/microclimates/analyses")

bball <- read.csv("output/cleanmicro_gdd_2019.csv")

bball$spp <- ifelse(bball$spp=="Acer_rubra", "Acer_rubrum", bball$spp)

# List of species to classify:
sort(unique(bball$spp))
#[1] "Acer_pensylvanicum"    "Acer_rubra"            "Acer_rubrum"           "Acer_saccharum"       
#[5] "Aesculus_flava"        "Betula_alleghaniensis" "Betula_nigra"          "Carya_glabra"         
#[9] "Carya_ovata"           "Fagus_grandifolia"     "Fraxinus_americana"    "Hamamelis_virginiana" 
#[13] "Populus_deltoides"     "Quercus_alba"          "Quercus_rubra"         "Tilia_americana"      
#[17] "Vaccinium_corymbosum"  "Viburnum_nudum"   

shrubs <- c("Acer_pensylvanicum", "Hamamelis_virginiana", "Vaccinium_corymbosum", "Viburnum_nudum" )

bball$functype <- ifelse(bball$spp%in%shrubs, "shrub", "tree")

bball$site <- ifelse(bball$urban==1, "arb", "hf")
bball$tx <- ifelse(bball$method==1, "ws", "hobo")


##### Now for site x method comparisons using real data...
bball$sitemethod <- paste0(bball$site, bball$tx)

bbshrub <- bball[(bball$functype=="shrub"),]
bbtree <- bball[(bball$functype=="tree"),]

bbshrub$meanshrub <- ave(bbshrub$gdd, bbshrub$sitemethod, FUN=function(x) mean(x, na.rm=TRUE))
bbshrub$sdshrub <- ave(bbshrub$gdd, bbshrub$sitemethod, FUN=function(x) sd(x, na.rm=TRUE))/sqrt(length(unique(bbshrub$meanshrub)))
bbtree$meantree <- ave(bbtree$gdd, bbtree$sitemethod, FUN=function(x) mean(x, na.rm=TRUE))
bbtree$sdtree <- ave(bbtree$gdd, bbtree$sitemethod, FUN=function(x) sd(x, na.rm=TRUE))/sqrt(length(unique(bbtree$meantree)))

bbshrub <- subset(bbshrub , select=c("sitemethod", "meanshrub", "sdshrub"))
bbshrub <- bbshrub[!duplicated(bbshrub),]
bbtree <- subset(bbtree , select=c("sitemethod", "meantree", "sdtree"))
bbtree <- bbtree[!duplicated(bbtree),]

bbdiff <- full_join(bbshrub, bbtree)

bbdiff$diff <- bbdiff$meanshrub - bbdiff$meantree
bbdiff$diff.sd <- bbdiff$sdshrub - bbdiff$sdtree


bbdiff$ymin <- bbdiff$meantree - bbdiff$sdtree
bbdiff$ymax <- bbdiff$meantree + bbdiff$sdtree
bbdiff$xmin <- bbdiff$meanshrub - bbdiff$sdshrub
bbdiff$xmax <- bbdiff$meanshrub + bbdiff$sdshrub

bbdiff$diff.labels <- as.numeric(as.factor(bbdiff$diff))

cols <- viridis_pal(option="viridis")(4)
diffinter_real <- ggplot(bbdiff, aes(x=meanshrub, y=meantree, col=as.factor(sitemethod))) + 
  geom_point(aes(x=meanshrub, y=meantree, size=as.factor(sitemethod)), shape=21) + 
  geom_linerange(aes(ymin=ymin, ymax=ymax)) +
  geom_errorbarh(aes(xmin = xmin, xmax = xmax, height = 0)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  #geom_text(aes(label=species), vjust=2) + 
  xlab("GDDs until budburst \n(shrubs)") + 
  ylab("GDDs until budburst \n(trees)") + 
  geom_line(x=1, y=1, slope=1, linetype="dashed", color="black") +
  ggtitle("a)") +
  scale_color_manual(name=expression(Delta*" in GDDs"), values=cols,
                     labels=c("hfws"="Forest site: weather station \n(57.99)", "arbws"="Urban site: weather station \n(-95.45)",
                              "arbhobo"="Urban site: hobo logger \n(-32.6)", "hfhobo"="Forest site: hobo logger \n(-85.1)")) +
  scale_size_manual(name=expression(Delta*" in GDDs"), values=c("hfws"=2,
                                                                "arbws"=4,
                                                                "arbhobo"=1,
                                                                "hfhobo"=3),
                    labels=c("hfws"="Forest site: weather station \n(57.99)", "arbws"="Urban site: weather station \n(-95.45)",
                             "arbhobo"="Urban site: hobo logger \n(-32.6)", "hfhobo"="Forest site: hobo logger \n(-85.1)")) +
  #scale_size_continuous(name=expression(Delta*" in false spring risk")) + 
  coord_cartesian(xlim=c(250, 525), ylim=c(250, 525))

pdf(file.path("~/Documents/git/microclimates/analyses/figures/", "functype_real.pdf"),
    width = 8, height = 6)
diffinter_real
dev.off()

##### Okay, on to the model output...
load("stan/urbmethod_real.Rdata")

urbmethod.sum <- summary(urbmethod)$summary

bball$sp.num <- as.numeric(as.factor(bball$spp))
bbshrubs <- bball[(bball$functype=="shrub"),]
unique(bbshrubs$sp.num)

shrubs <- rbind(urbmethod.sum["a_sp[11]", ], urbmethod.sum["a_sp[16]", ],  
  urbmethod.sum["a_sp[17]", ], urbmethod.sum["a_sp[1]", ])
trees <- rbind(urbmethod.sum["a_sp[2]", ], urbmethod.sum["a_sp[3]", ],  
  urbmethod.sum["a_sp[4]", ], urbmethod.sum["a_sp[5]", ], urbmethod.sum["a_sp[6]", ], urbmethod.sum["a_sp[7]", ], 
  urbmethod.sum["a_sp[8]", ], urbmethod.sum["a_sp[9]", ], urbmethod.sum["a_sp[10]", ], urbmethod.sum["a_sp[12]", ], 
  urbmethod.sum["a_sp[13]", ], urbmethod.sum["a_sp[14]", ], urbmethod.sum["a_sp[15]", ], urbmethod.sum["a_sp[18]", ])

hfhobospp <- data.frame(rbind(shrubs, trees))
hfhobospp$sitemethod <- c(rep("hfhobo", each=18))
hfhobospp$site <- c(rep("hf", each=18))
hfhobospp$method <- c(rep("hobo", each=18))
hfhobospp$functype <- c(rep("shrub", each=4), rep("tree", each=14))

shrubshfws <- rbind((urbmethod.sum["a_sp[11]", ] + urbmethod.sum["mu_b_method_sp", ]), 
                    (urbmethod.sum["a_sp[16]", ] + urbmethod.sum["mu_b_method_sp", ]),  
                (urbmethod.sum["a_sp[17]", ] + urbmethod.sum["mu_b_method_sp", ]), 
                (urbmethod.sum["a_sp[1]", ] + urbmethod.sum["mu_b_method_sp", ]))

treeshfws <- rbind((urbmethod.sum["a_sp[2]", ] + urbmethod.sum["mu_b_method_sp", ]), 
                   (urbmethod.sum["a_sp[3]", ] + urbmethod.sum["mu_b_method_sp", ]),  
               (urbmethod.sum["a_sp[4]", ] + urbmethod.sum["mu_b_method_sp", ]), 
               (urbmethod.sum["a_sp[5]", ] + urbmethod.sum["mu_b_method_sp", ]), 
               (urbmethod.sum["a_sp[6]", ] + urbmethod.sum["mu_b_method_sp", ]), 
               (urbmethod.sum["a_sp[7]", ] + urbmethod.sum["mu_b_method_sp", ]), 
               (urbmethod.sum["a_sp[8]", ] + urbmethod.sum["mu_b_method_sp", ]), 
               (urbmethod.sum["a_sp[9]", ] + urbmethod.sum["mu_b_method_sp", ]), 
               (urbmethod.sum["a_sp[10]", ] + urbmethod.sum["mu_b_method_sp", ]), 
               (urbmethod.sum["a_sp[12]", ] + urbmethod.sum["mu_b_method_sp", ]), 
               (urbmethod.sum["a_sp[13]", ] + urbmethod.sum["mu_b_method_sp", ]), 
               (urbmethod.sum["a_sp[14]", ] + urbmethod.sum["mu_b_method_sp", ]), 
               (urbmethod.sum["a_sp[15]", ] + urbmethod.sum["mu_b_method_sp", ]), 
               (urbmethod.sum["a_sp[18]", ] + urbmethod.sum["mu_b_method_sp", ]))

sitemethod <- rep("hfws", each=18)
site <- rep("hf", each=18)
method <- rep("ws", each=18)
functype <- c(rep("shrub", each=4), rep("tree", each=14))

hfwsspp <- rbind(shrubshfws, treeshfws)
hfwsspp <- data.frame(cbind(hfwsspp, sitemethod, site, method, functype))


shrubsarbws <- rbind((urbmethod.sum["a_sp[11]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["mu_b_um_sp", ]), 
                    (urbmethod.sum["a_sp[16]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["mu_b_um_sp", ]),  
                    (urbmethod.sum["a_sp[17]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["mu_b_um_sp", ]), 
                    (urbmethod.sum["a_sp[1]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["mu_b_um_sp", ]))

treesarbws <- rbind((urbmethod.sum["a_sp[2]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["mu_b_um_sp", ]), 
                   (urbmethod.sum["a_sp[3]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["mu_b_um_sp", ]),  
                   (urbmethod.sum["a_sp[4]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["mu_b_um_sp", ]), 
                   (urbmethod.sum["a_sp[5]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["mu_b_um_sp", ]), 
                   (urbmethod.sum["a_sp[6]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["mu_b_um_sp", ]), 
                   (urbmethod.sum["a_sp[7]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["mu_b_um_sp", ]), 
                   (urbmethod.sum["a_sp[8]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["mu_b_um_sp", ]), 
                   (urbmethod.sum["a_sp[9]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["mu_b_um_sp", ]), 
                   (urbmethod.sum["a_sp[10]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["mu_b_um_sp", ]), 
                   (urbmethod.sum["a_sp[12]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["mu_b_um_sp", ]), 
                   (urbmethod.sum["a_sp[13]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["mu_b_um_sp", ]), 
                   (urbmethod.sum["a_sp[14]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["mu_b_um_sp", ]), 
                   (urbmethod.sum["a_sp[15]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["mu_b_um_sp", ]), 
                   (urbmethod.sum["a_sp[18]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["mu_b_um_sp", ]))

sitemethod <- rep("arbws", each=18)
site <- rep("arb", each=18)
method <- rep("ws", each=18)
functype <- c(rep("shrub", each=4), rep("tree", each=14))

arbwsspp <- rbind(shrubsarbws, treesarbws)
arbwsspp <- data.frame(cbind(arbwsspp, sitemethod, site, method, functype))


shrubsarbhobo <- rbind((urbmethod.sum["a_sp[11]", ] + urbmethod.sum["mu_b_urban_sp", ]), 
                       (urbmethod.sum["a_sp[16]", ] + urbmethod.sum["mu_b_urban_sp", ]),  
                       (urbmethod.sum["a_sp[17]", ] + urbmethod.sum["mu_b_urban_sp", ]), 
                       (urbmethod.sum["a_sp[1]", ] + urbmethod.sum["mu_b_urban_sp", ]))

treesarbhobo <- rbind((urbmethod.sum["a_sp[2]", ] + urbmethod.sum["mu_b_urban_sp", ]), 
                      (urbmethod.sum["a_sp[3]", ] + urbmethod.sum["mu_b_urban_sp", ]),  
                      (urbmethod.sum["a_sp[4]", ] + urbmethod.sum["mu_b_urban_sp", ]), 
                      (urbmethod.sum["a_sp[5]", ] + urbmethod.sum["mu_b_urban_sp", ]), 
                      (urbmethod.sum["a_sp[6]", ] + urbmethod.sum["mu_b_urban_sp", ]), 
                      (urbmethod.sum["a_sp[7]", ] + urbmethod.sum["mu_b_urban_sp", ]), 
                      (urbmethod.sum["a_sp[8]", ] + urbmethod.sum["mu_b_urban_sp", ]), 
                      (urbmethod.sum["a_sp[9]", ] + urbmethod.sum["mu_b_urban_sp", ]), 
                      (urbmethod.sum["a_sp[10]", ] + urbmethod.sum["mu_b_urban_sp", ]), 
                      (urbmethod.sum["a_sp[12]", ] + urbmethod.sum["mu_b_urban_sp", ]), 
                      (urbmethod.sum["a_sp[13]", ] + urbmethod.sum["mu_b_urban_sp", ]), 
                      (urbmethod.sum["a_sp[14]", ] + urbmethod.sum["mu_b_urban_sp", ]), 
                      (urbmethod.sum["a_sp[15]", ] + urbmethod.sum["mu_b_urban_sp", ]), 
                      (urbmethod.sum["a_sp[18]", ] + urbmethod.sum["mu_b_urban_sp", ]))

sitemethod <- rep("arbhobo", each=18)
site <- rep("arb", each=18)
method <- rep("hobo", each=18)
functype <- c(rep("shrub", each=4), rep("tree", each=14))

arbhobospp <- rbind(shrubsarbhobo, treesarbhobo)
arbhobospp <- data.frame(cbind(arbhobospp, sitemethod, site, method, functype))


bball <- data.frame(rbind(hfhobospp, hfwsspp, arbhobospp, arbwsspp))
bball <- subset(bball, select=c("mean", "sitemethod", "functype"))
colnames(bball) <- c("gdd", "sitemethod", "functype")
rownames(bball) <- 1:nrow(bball)

bball$gdd <- as.numeric(bball$gdd)

bbshrub <- bball[(bball$functype=="shrub"),]
bbtree <- bball[(bball$functype=="tree"),]

bbshrub$meanshrub <- ave(bbshrub$gdd, bbshrub$sitemethod, FUN=function(x) mean(x, na.rm=TRUE))
bbshrub$sdshrub <- ave(bbshrub$gdd, bbshrub$sitemethod, FUN=function(x) sd(x, na.rm=TRUE))/sqrt(length(unique(bbshrub$meanshrub)))
bbtree$meantree <- ave(bbtree$gdd, bbtree$sitemethod, FUN=function(x) mean(x, na.rm=TRUE))
bbtree$sdtree <- ave(bbtree$gdd, bbtree$sitemethod, FUN=function(x) sd(x, na.rm=TRUE))/sqrt(length(unique(bbtree$meantree)))

bbshrub <- subset(bbshrub , select=c("sitemethod", "meanshrub", "sdshrub"))
bbshrub <- bbshrub[!duplicated(bbshrub),]
bbtree <- subset(bbtree , select=c("sitemethod", "meantree", "sdtree"))
bbtree <- bbtree[!duplicated(bbtree),]

bbdiff <- full_join(bbshrub, bbtree)

bbdiff$diff <- bbdiff$meanshrub - bbdiff$meantree
bbdiff$diff.sd <- bbdiff$sdshrub - bbdiff$sdtree


bbdiff$ymin <- bbdiff$meantree - bbdiff$sdtree
bbdiff$ymax <- bbdiff$meantree + bbdiff$sdtree
bbdiff$xmin <- bbdiff$meanshrub - bbdiff$sdshrub
bbdiff$xmax <- bbdiff$meanshrub + bbdiff$sdshrub

#bbdiff$diff.labels <- 1:4

cols <- viridis_pal(option="viridis")(4)
diffinter <- ggplot(bbdiff, aes(x=meanshrub, y=meantree, col=sitemethod)) + 
  geom_jitter(aes(x=meanshrub, y=meantree), width=0.4) + 
  geom_linerange(aes(ymin=ymin, ymax=ymax)) +
  geom_errorbarh(aes(xmin = xmin, xmax = xmax, height = 0)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  #geom_text(aes(label=species), vjust=2) + 
  xlab("GDDs until budburst \n(shrubs)") + 
  ylab("GDDs until budburst \n(trees)") + 
  ggtitle("b)") +
  scale_color_manual(name=expression(Delta*" in GDDs"), values=cols,
                     labels=c("hfws"="Forest site: \nweather station", "arbws"="Urban site: \nweather station",
                              "arbhobo"="Urban site: \nhobo logger", "hfhobo"="Forest site: \nhobo logger")) +
  #scale_size_continuous(name=expression(Delta*" in false spring risk")) + 
  coord_cartesian(xlim=c(325, 475), ylim=c(325, 475))

pdf(file.path("~/Documents/git/microclimates/analyses/figures/", "functype_modoutput.pdf"),
    width = 8, height = 6)
diffinter
dev.off()

pdf(file.path("~/Documents/git/microclimates/analyses/figures/", "functype.pdf"),
    width = 12, height = 6, onefile=FALSE)
ggarrange(diffinter_real, diffinter, ncol=2)
dev.off()

