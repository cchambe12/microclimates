#### 18 July 2019 - Cat
## Thinking about building a phylogeny instead...
# Based off Nacho's code from ospree/analyses/phylogeny/models_phylo.R

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load libraries
library(shinystan)
library(caper)
library(brms)
library(pez)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Set working directory
setwd("~/Documents/git/microclimates/analyses")

## Load the data
gdd.stan <- read.csv("output/clean_gdd_bbanddvr.csv", header=TRUE)

gdd.stan$spp <- paste(substr(gdd.stan$genus, 0, 3), substr(gdd.stan$species,0,3), sep="")
gdd.stan$site <- NA
gdd.stan$site <- ifelse(gdd.stan$type=="Treespotters", 0, gdd.stan$site)
gdd.stan$site <- ifelse(gdd.stan$type=="Common Garden", 1, gdd.stan$site)
gdd.stan$site <- ifelse(gdd.stan$type=="Harvard Forest", 2, gdd.stan$site)

gdd.stan$site <- as.numeric(gdd.stan$site)

gdd.stan <- subset(gdd.stan, select=c("id", "provenance.lat", "spp", "site",
                                      "gdd_bb", "gdd_dvr", "fs.count", "genus", "species"))

gdd.stan <- gdd.stan[!is.na(gdd.stan$provenance.lat),]
gdd.stan <- gdd.stan[!is.na(gdd.stan$gdd_bb),]
gdd.stan <- gdd.stan[!is.na(gdd.stan$gdd_dvr),]
gdd.stan <- gdd.stan[!is.na(gdd.stan$spp),]

gdd.stan <- gdd.stan[(gdd.stan$gdd_bb<1000),]
gdd.stan <- gdd.stan[(gdd.stan$gdd_dvr<1000),]

gdd.stan$spp <- ifelse(gdd.stan$spp=="NANA", "Quealb", gdd.stan$spp)

####################################
#### Fitting Phylogenetic brms
####################################

## read and pre-process phylogeny
library(phytools)
phylo <- read.tree("../phylodata/micro.phylogeny.tre")
namesphy<-phylo$tip.label
namesdat<-unique(paste(gdd.stan$genus,gdd.stan$species,sep="_"))
phylo<-force.ultrametric(phylo, method="extend")
phylo$node.label<-seq(1,length(phylo$node.label),1)
is.ultrametric(phylo)


## get phylogenetic covariance matrix
library(MCMCglmm)
inv.phylo <- inverseA(phylo, nodes = "TIPS", scale = TRUE)
A <- solve(inv.phylo$Ainv)
rownames(A) <- rownames(inv.phylo$Ainv)
gdd.stan$phylo<-paste(gdd.stan$genus,gdd.stan$species,sep="_")
gdd.stan$spps<-gdd.stan$phylo

if(TRUE){
gdd.stan$site.name <- NA
gdd.stan$site.name <- ifelse(gdd.stan$site==0, "Arboretum", gdd.stan$site.name)
gdd.stan$site.name <- ifelse(gdd.stan$site==1, "Common Garden", gdd.stan$site.name)
gdd.stan$site.name <- ifelse(gdd.stan$site==2, "Harvard Forest", gdd.stan$site.name)

model_phylo_simple <- brm(
  gdd_bb ~ site.name +      ## fixed effs
    (site.name|phylo) + (site.name|species) ,  ## rnd effs 
  data = gdd.stan, 
  family = gaussian(), cov_ranef = list(phylo = A),
  sample_prior = TRUE, chains = 2, cores = 2, 
  iter = 2500, warmup = 1000
)
}


if(FALSE){
## fitting models for site for bb and dvr independently
### Site BB
### ## ad mean of predictor across species and within species
gdd.stan$species_mean <- 
  with(gdd.stan, sapply(split(site, phylo), mean)[phylo])

gdd.stan$within_species <- 
  gdd.stan$site - gdd.stan$species_mean

model_phylo <- brm(
  gdd_bb ~ species_mean + within_species +      ## fixed effs
    (1 + within_species|phylo) + (1|species),  ## rnd effs 
  data = gdd.stan, 
  family = gaussian(), cov_ranef = list(phylo = A),
  sample_prior = TRUE, chains = 2, cores = 2, 
  iter = 2500, warmup = 1000
)
}

library(broom)
phymod<-as.data.frame(tidy(model_phylo, prob=0.5))
modoutput <- phymod #modelhere




## BETALL, QUEALB, QUERUB
## Arb and HF
# ACERUB, ACESAC, BETALL, FAGGRA, QUEALB, QUERUB



#mod.gddbb <- brm(gdd_bb ~ site + provenance.lat, data=gdd.stan, control=list(max_treedepth = 15,adapt_delta = 0.99))
#mod.gdddvr <- brm(gdd_dvr ~ spp*site, data=gdd.stan, control=list(max_treedepth = 15,adapt_delta = 0.99))
#mod.gddbb.prov <- brm(gdd_bb ~ spp*site + provenance.lat, data=gdd.stan)






