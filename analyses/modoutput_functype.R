## 5 May 2021 - Cat
# Hoping to disentangle model output of tree and shrub differences in GDD until budburst

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
  geom_point(aes(x=meanshrub, y=meantree)) + 
  geom_linerange(aes(ymin=ymin, ymax=ymax)) +
  geom_errorbarh(aes(xmin = xmin, xmax = xmax, height = 0)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.position = "none",
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  xlab("GDDs until budburst \n(shrubs)") + 
  ylab("GDDs until budburst \n(trees)") + 
  geom_abline(slope=1, intercept=0, linetype="dashed", color="black") +
  ggtitle("a)") +
  scale_color_manual(name=expression(Delta*" in GDDs"), values=cols,
                     labels=c("hfws"="Forest site: weather station", "arbws"="Urban site: weather station",
                              "arbhobo"="Urban site: hobo logger", "hfhobo"="Forest site: hobo logger")) +
  coord_cartesian(xlim=c(200, 600), ylim=c(200, 600))

pdf(file.path("~/Documents/git/microclimates/analyses/figures/", "zarchive/functype_real.pdf"),
    width = 8, height = 6)
diffinter_real
dev.off()

##### Okay, on to the model output...
load("stan/urbmethod_real.Rdata")

urbmethod.sum <- summary(urbmethod)$summary

bball$sp.num <- as.numeric(as.factor(bball$spp))
bbshrubs <- bball[(bball$functype=="shrub"),]
bbtrees <- bball[(bball$functype!="shrub"),]
unique(bbshrubs$sp.num)
unique(bbtrees$sp.num)

shrubs <- rbind(urbmethod.sum["a_sp[11]", ], urbmethod.sum["a_sp[16]", ],  
  urbmethod.sum["a_sp[17]", ], urbmethod.sum["a_sp[1]", ])
trees <- rbind(urbmethod.sum["a_sp[2]", ], urbmethod.sum["a_sp[3]", ],  
  urbmethod.sum["a_sp[4]", ], urbmethod.sum["a_sp[5]", ], urbmethod.sum["a_sp[6]", ], urbmethod.sum["a_sp[7]", ], 
  urbmethod.sum["a_sp[8]", ], urbmethod.sum["a_sp[9]", ], urbmethod.sum["a_sp[10]", ], urbmethod.sum["a_sp[12]", ], 
  urbmethod.sum["a_sp[13]", ], urbmethod.sum["a_sp[14]", ], urbmethod.sum["a_sp[15]", ])

hfhobospp <- data.frame(rbind(shrubs, trees))
hfhobospp$sitemethod <- c(rep("hfhobo", each=17))
hfhobospp$site <- c(rep("hf", each=17))
hfhobospp$method <- c(rep("hobo", each=17))
hfhobospp$functype <- c(rep("shrub", each=4), rep("tree", each=13))

shrubshfws <- rbind((urbmethod.sum["a_sp[11]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[11]", ]),
                    (urbmethod.sum["a_sp[16]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[16]", ]),
                (urbmethod.sum["a_sp[17]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[17]", ]),
                (urbmethod.sum["a_sp[1]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[1]", ]))

treeshfws <- rbind((urbmethod.sum["a_sp[2]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[2]", ]),
                   (urbmethod.sum["a_sp[3]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[3]", ]),
               (urbmethod.sum["a_sp[4]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[4]", ]),
               (urbmethod.sum["a_sp[5]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[5]", ]),
               (urbmethod.sum["a_sp[6]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[6]", ]),
               (urbmethod.sum["a_sp[7]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[7]", ]),
               (urbmethod.sum["a_sp[8]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[8]", ]),
               (urbmethod.sum["a_sp[9]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[9]", ]),
               (urbmethod.sum["a_sp[10]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[10]", ]),
               (urbmethod.sum["a_sp[12]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[12]", ]),
               (urbmethod.sum["a_sp[13]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[13]", ]),
               (urbmethod.sum["a_sp[14]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[14]", ]),
               (urbmethod.sum["a_sp[15]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[15]", ]))

sitemethod <- rep("hfws", each=17)
site <- rep("hf", each=17)
method <- rep("ws", each=17)
functype <- c(rep("shrub", each=4), rep("tree", each=13))

hfwsspp <- rbind(shrubshfws, treeshfws)
hfwsspp <- data.frame(cbind(hfwsspp, sitemethod, site, method, functype))


shrubsarbws <- rbind((urbmethod.sum["a_sp[11]", ] + urbmethod.sum["mu_b_method_sp", ]  + urbmethod.sum["b_method[11]", ] +
                      urbmethod.sum["mu_b_urban_sp", ]  + urbmethod.sum["b_urban[11]", ] + 
                        urbmethod.sum["mu_b_um_sp", ] + urbmethod.sum["b_um[11]", ]),
                    (urbmethod.sum["a_sp[16]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[16]", ] +
                       urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[16]", ] + 
                       urbmethod.sum["mu_b_um_sp", ] + urbmethod.sum["b_um[16]", ]),  
                    (urbmethod.sum["a_sp[17]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[17]", ] +
                       urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[17]", ] +
                       urbmethod.sum["mu_b_um_sp", ] + urbmethod.sum["b_um[17]", ]), 
                    (urbmethod.sum["a_sp[1]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[1]", ] +
                       urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[1]", ] +
                       urbmethod.sum["mu_b_um_sp", ] + urbmethod.sum["b_um[1]", ]))

treesarbws <- rbind((urbmethod.sum["a_sp[2]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[2]", ] +
                       urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[2]", ] +
                       urbmethod.sum["mu_b_um_sp", ] + urbmethod.sum["b_um[2]", ]), 
                   (urbmethod.sum["a_sp[3]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[3]", ] +
                       urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[3]", ] +
                       urbmethod.sum["mu_b_um_sp", ] + urbmethod.sum["b_um[3]", ]),  
                   (urbmethod.sum["a_sp[4]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[4]", ] +
                      urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[4]", ] +
                      urbmethod.sum["mu_b_um_sp", ] + urbmethod.sum["b_um[4]", ]), 
                   (urbmethod.sum["a_sp[5]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[5]", ] +
                      urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[5]", ] +
                      urbmethod.sum["mu_b_um_sp", ] + urbmethod.sum["b_um[5]", ]), 
                   (urbmethod.sum["a_sp[6]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[6]", ] +
                      urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[6]", ] +
                      urbmethod.sum["mu_b_um_sp", ] + urbmethod.sum["b_um[6]", ]), 
                   (urbmethod.sum["a_sp[7]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[7]", ] +
                      urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[7]", ] +
                      urbmethod.sum["mu_b_um_sp", ] + urbmethod.sum["b_um[7]", ]), 
                   (urbmethod.sum["a_sp[8]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[8]", ] +
                      urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[8]", ] +
                      urbmethod.sum["mu_b_um_sp", ] + urbmethod.sum["b_um[8]", ]), 
                   (urbmethod.sum["a_sp[9]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[9]", ] +
                      urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[9]", ] +
                      urbmethod.sum["mu_b_um_sp", ] + urbmethod.sum["b_um[9]", ]), 
                   (urbmethod.sum["a_sp[10]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[10]", ] +
                      urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[10]", ] +
                      urbmethod.sum["mu_b_um_sp", ] + urbmethod.sum["b_um[10]", ]), 
                   (urbmethod.sum["a_sp[12]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[12]", ] +
                      urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[12]", ] +
                      urbmethod.sum["mu_b_um_sp", ] + urbmethod.sum["b_um[12]", ]), 
                   (urbmethod.sum["a_sp[13]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[13]", ] +
                      urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[13]", ] +
                      urbmethod.sum["mu_b_um_sp", ] + urbmethod.sum["b_um[13]", ]), 
                   (urbmethod.sum["a_sp[14]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[14]", ] +
                      urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[14]", ] +
                      urbmethod.sum["mu_b_um_sp", ] + urbmethod.sum["b_um[14]", ]), 
                   (urbmethod.sum["a_sp[15]", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["b_method[15]", ] +
                      urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[15]", ] +
                      urbmethod.sum["mu_b_um_sp", ] + urbmethod.sum["b_um[15]", ]))

sitemethod <- rep("arbws", each=17)
site <- rep("arb", each=17)
method <- rep("ws", each=17)
functype <- c(rep("shrub", each=4), rep("tree", each=13))

arbwsspp <- rbind(shrubsarbws, treesarbws)
arbwsspp <- data.frame(cbind(arbwsspp, sitemethod, site, method, functype))


shrubsarbhobo <- rbind((urbmethod.sum["a_sp[11]", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[11]", ]), 
                       (urbmethod.sum["a_sp[16]", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[16]", ]),  
                       (urbmethod.sum["a_sp[17]", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[17]", ]), 
                       (urbmethod.sum["a_sp[1]", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[1]", ]))

treesarbhobo <- rbind((urbmethod.sum["a_sp[2]", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[2]", ]), 
                      (urbmethod.sum["a_sp[3]", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[3]", ]),  
                      (urbmethod.sum["a_sp[4]", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[4]", ]), 
                      (urbmethod.sum["a_sp[5]", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[5]", ]), 
                      (urbmethod.sum["a_sp[6]", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[6]", ]), 
                      (urbmethod.sum["a_sp[7]", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[7]", ]), 
                      (urbmethod.sum["a_sp[8]", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[8]", ]), 
                      (urbmethod.sum["a_sp[9]", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[9]", ]), 
                      (urbmethod.sum["a_sp[10]", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[10]", ]), 
                      (urbmethod.sum["a_sp[12]", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[12]", ]), 
                      (urbmethod.sum["a_sp[13]", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[13]", ]), 
                      (urbmethod.sum["a_sp[14]", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[14]", ]), 
                      (urbmethod.sum["a_sp[15]", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["b_urban[15]", ]))

sitemethod <- rep("arbhobo", each=17)
site <- rep("arb", each=17)
method <- rep("hobo", each=17)
functype <- c(rep("shrub", each=4), rep("tree", each=13))

arbhobospp <- rbind(shrubsarbhobo, treesarbhobo)
arbhobospp <- data.frame(cbind(arbhobospp, sitemethod, site, method, functype))


bball <- data.frame(rbind(hfhobospp, hfwsspp, arbhobospp, arbwsspp))
bball <- subset(bball, select=c("mean", "sd", "sitemethod", "functype"))
colnames(bball) <- c("gdd", "sd", "sitemethod", "functype")
rownames(bball) <- 1:nrow(bball)

bball$gdd <- as.numeric(bball$gdd)
bball$sd <- as.numeric(bball$sd)

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

bbdiff <- right_join(bbtree, bbshrub)
bbdiff <- bbdiff[!duplicated(bbdiff),]

bbdiff$diff <- bbdiff$meanshrub - bbdiff$meantree
bbdiff$diff.sd <- bbdiff$sdshrub - bbdiff$sdtree


bbdiff$ymin <- bbdiff$meantree - bbdiff$sdtree
bbdiff$ymax <- bbdiff$meantree + bbdiff$sdtree
bbdiff$xmin <- bbdiff$meanshrub - bbdiff$sdshrub
bbdiff$xmax <- bbdiff$meanshrub + bbdiff$sdshrub

#bbdiff$diff.labels <- 1:4

cols <- viridis_pal(option="viridis")(4)
diffinter <- ggplot(bbdiff, aes(x=meanshrub, y=meantree, col=sitemethod)) + 
  geom_point(aes(x=meanshrub, y=meantree)) + 
  geom_linerange(aes(ymin=ymin, ymax=ymax)) +
  geom_errorbarh(aes(xmin = xmin, xmax = xmax, height = 0)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  xlab("GDDs until budburst \n(shrubs)") + 
  ylab("GDDs until budburst \n(trees)") + 
  ggtitle("b)") +
  geom_abline(intercept=0, slope=1, linetype="dashed") +
  scale_color_manual(name=expression(Delta*" in GDDs"), values=cols,
                     labels=c("hfws"="Forest site: weather station", "arbws"="Urban site: weather station",
                              "arbhobo"="Urban site: hobo logger", "hfhobo"="Forest site: hobo logger")) + 
  coord_cartesian(xlim=c(200, 600), ylim=c(200, 600))

pdf(file.path("~/Documents/git/microclimates/analyses/figures/", "zarchive/functype_modoutput.pdf"),
    width = 8, height = 6)
diffinter
dev.off()

pdf(file.path("~/Documents/git/microclimates/analyses/figures/", "functype.pdf"),
    width = 12, height = 6, onefile=FALSE)
ggarrange(diffinter_real, diffinter, ncol=2)
dev.off()

