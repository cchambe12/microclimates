## Script to build a phylogeny containing the species in Ospree departing from 
## the vascular plant megatree by Zanne et al. (2014);Nature

## Started by Ignacio Morales-Castilla on November 2018

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(dplyr)
library(tidyr)
library(stringr)
library(ape)
library(phytools)
library(geiger)
library(pez)
library(caper)
library(phangorn)

## set your wd here:
setwd("~/Documents/git/microclimates")


# get the data 
gdd.stan <- read.csv("analyses/output/clean_gdd_bbanddvr.csv", header=TRUE)

gdd.stan$spp <- paste(substr(gdd.stan$genus, 0, 3), substr(gdd.stan$species,0,3), sep="")
gdd.stan$site <- NA
gdd.stan$site <- ifelse(gdd.stan$type=="Treespotters", "arb", gdd.stan$site)
gdd.stan$site <- ifelse(gdd.stan$type=="Common Garden", "cg", gdd.stan$site)
gdd.stan$site <- ifelse(gdd.stan$type=="Harvard Forest", "hf", gdd.stan$site)

gdd.stan <- subset(gdd.stan, select=c("id", "provenance.lat", "spp", "site",
                                      "gdd_bb", "gdd_dvr", "fs.count", "genus", "species"))

gdd.stan <- gdd.stan[!is.na(gdd.stan$provenance.lat),]
gdd.stan <- gdd.stan[!is.na(gdd.stan$gdd_bb),]
gdd.stan <- gdd.stan[!is.na(gdd.stan$gdd_dvr),]
gdd.stan <- gdd.stan[!is.na(gdd.stan$spp),]

gdd.stan <- gdd.stan[(gdd.stan$gdd_bb<1000),]
gdd.stan <- gdd.stan[(gdd.stan$gdd_dvr<1000),]

gdd.stan$spp <- ifelse(gdd.stan$spp=="NANA", "Quealb", gdd.stan$spp)


# geting a list of all species in ospree
gdd.stan$latbi <- paste(gdd.stan$genus, gdd.stan$species, sep="_")
#sort(unique(bb$latbi))

sps.list=sort(unique(gdd.stan$latbi))
genus.list=sort(unique(gdd.stan$genus))


## load phylogeny

## load phylo (from Zanne et al. 2014)
#phy.plants<-read.tree("data/phylogeny/Vascular_Plants_rooted.dated.tre")
phy.plants<-read.tree("~/Documents/git/microclimates/phylodata/Vascular_Plants_rooted.dated.tre")


## getting a list of genera in Zanne's phylo
phy.genera<-unlist(
  lapply(strsplit(phy.plants$tip.label, "_"),function(x){return(x[1])})
)

phy.genera.uniq<-sort(unique(phy.genera))



## how many microclimate genera are in the phylogeny?
microgenus.inphylo<-genus.list[which(genus.list%in%phy.genera.uniq)]


## first prune the phylogeny to include only these genera
phy.genera.micro<-drop.tip(phy.plants,
                            which(!phy.genera%in%microgenus.inphylo))


## we can add species that may not be present according to their genera
names.to.add=sps.list[which(!sps.list%in%phy.genera.micro$tip.label)]
phy.micro.clean<-congeneric.merge(phy.genera.micro,names.to.add,split="_")


## prunning the generated phylogeny to include micro species only
phy.plants.micro<-drop.tip(phy.micro.clean,
                            which(!phy.micro.clean$tip.label%in%sps.list))
# only 32 species are in the phylogeny

plot(phy.plants.micro,cex=.5)

## save phylogeny
write.tree(phy.plants.micro,"~/Documents/git/microclimates/phylodata/micro.phylogeny.tre")


### Let's build a prettier phylogeny now..
library(ggplot2)
library(ggtree)

phy.plants.micro <- read.tree("~/Documents/git/microclimates/phylodata/micro.phylogeny.tre")

newnames <- data.frame(tiplabel=phy.plants.micro$tip.label)
newnames$genus <- gsub("_.*", "", newnames$tiplabel)
newnames$species <- gsub(".*_", "", newnames$tiplabel)
newnames$labels <- paste(newnames$genus, newnames$species)
newnames$labels <- italic(newnames$labels)

ggtree(phy.plants.micro)  + geom_tiplab(size=3, label=newnames$labels, fontface='italic') + 
  theme(plot.margin = unit(c(0,1,1,1), "lines")) +
  xlim(c(0, 200))

