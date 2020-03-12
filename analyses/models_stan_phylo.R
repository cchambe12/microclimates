## Started 12 March 2020 ##
## Based off code from Nacho (ospree/analyses/phylogeny/models_phylo.R) ##
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
setwd("~/Documents/git/microclimates/analyses/")

library(shinystan)
library(caper)
library(brms)
library(pez)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

use.hobos = FALSE
use.allyears = TRUE
use.allsites = FALSE

if(use.hobos==FALSE & use.allyears == FALSE){
  ws_urb <- read.csv("output/clean_gdd_bbanddvr.csv")
  ws_urb$species <- ifelse(ws_urb$genus=="Acer" & ws_urb$species=="rubra", "rubrum", ws_urb$species)
  
  ws_urb$urban <- NA
  ws_urb$urban <- ifelse(ws_urb$type=="Harvard Forest", 0, ws_urb$urban)
  ws_urb$urban <- ifelse(ws_urb$type=="Treespotters", 1, ws_urb$urban)
  
  ws_urb <- ws_urb[(ws_urb$year>=2019),]
  
  ws_urb.stan <- subset(ws_urb, select=c(gdd_bb, urban, genus, species))
  ws_urb.stan <- ws_urb.stan[(complete.cases(ws_urb.stan)),]
  ws_urb.stan$spp <- paste(ws_urb.stan$genus, ws_urb.stan$species, sep="_")
  
  gdd.stan <- ws_urb.stan[(ws_urb.stan$gdd_bb<=1000),]
}

if(use.allyears==TRUE & use.allsites==FALSE){
    ws_urb <- read.csv("output/clean_gdd_bbanddvr.csv")
    ws_urb$species <- ifelse(ws_urb$genus=="Acer" & ws_urb$species=="rubra", "rubrum", ws_urb$species)
    
    ws_urb$urban <- NA
    ws_urb$urban <- ifelse(ws_urb$type=="Harvard Forest", 0, ws_urb$urban)
    ws_urb$urban <- ifelse(ws_urb$type=="Treespotters", 1, ws_urb$urban)
    
    ws_urb.stan <- subset(ws_urb, select=c(gdd_bb, genus, species, urban, year))
    ws_urb.stan <- ws_urb.stan[(complete.cases(ws_urb.stan)),]
    ws_urb.stan$spp <- paste(ws_urb.stan$genus, ws_urb.stan$species, sep="_")
    
    gdd.stan <- ws_urb.stan[(ws_urb.stan$gdd_bb<=2000),]
  
} 

if(use.allyears==TRUE & use.allsites==TRUE){
  ws_urb <- read.csv("output/clean_gdd_bbanddvr.csv")
  ws_urb$species <- ifelse(ws_urb$genus=="Acer" & ws_urb$species=="rubra", "rubrum", ws_urb$species)
  
  ws_urb.stan <- subset(ws_urb, select=c(gdd_bb, genus, species, type))
  ws_urb.stan <- ws_urb.stan[(complete.cases(ws_urb.stan)),]
  ws_urb.stan$spp <- paste(ws_urb.stan$genus, ws_urb.stan$species, sep="_")
  
  gdd.stan <- ws_urb.stan[(ws_urb.stan$gdd_bb<=1000),]
  
} 

if(use.hobos==TRUE){
  hobo_urb <- read.csv("output/clean_gdd_bbanddvr_hobo.csv")
  ws_urb$species <- ifelse(ws_urb$genus=="Acer" & ws_urb$species=="rubra", "rubrum", ws_urb$species)
  
  hobo_urb$urban <- NA
  hobo_urb$urban <- ifelse(hobo_urb$type=="Harvard Forest", 0, hobo_urb$urban)
  hobo_urb$urban <- ifelse(hobo_urb$type=="Treespotters", 1, hobo_urb$urban)
  
  hobo_urb.stan <- subset(hobo_urb, select=c(gdd_bb, urban, genus, species))
  hobo_urb.stan <- hobo_urb.stan[(complete.cases(hobo_urb.stan)),]
  hobo_urb.stan$spp <- paste(hobo_urb.stan$genus, hobo_urb.stan$species, sep="_")
  gdd.stan <- hobo_urb.stan[(hobo_urb.stan$gdd_bb<=1000),]
}


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


use.urban = TRUE
## fitting models for GDD for weather station all years
if(use.urban = TRUE){ # Not sure how to add it variation among sites...
### ADD IN SITE PREDICTOR
### ## ad mean of predictor across species and within species
gdd.stan$species_mean <- 
  with(gdd.stan, sapply(split(urban, phylo), mean)[phylo])

gdd.stan$within_species <- 
  gdd.stan$urban - gdd.stan$species_mean

model_phylo <- brm(
  gdd_bb ~ urban +      ## fixed effs
    (1 +  urban|phylo) + (1|spps),  ## rnd effs 
  data = gdd.stan, 
  family = gaussian(), cov_ranef = list("phylo:urban" = A),
  prior = c(
    prior(normal(0, 400), "b"),
    prior(normal(0, 400), "Intercept"),
    prior(student_t(3, 0, 100), "sd"),
    prior(student_t(3, 0, 100), "sigma")
  )
  ,sample_prior = TRUE, chains = 4,
  control=list(max_treedepth = 15,adapt_delta = 0.99),
  iter = 5000, warmup = 2000
)

save(model_phylo, file="~/Documents/git/microclimates/analyses/stan/phylomod_urban.Rdata")

}

if(use.urban = FALSE){
model_phylo <- brm(
  gdd_bb ~ 1 +      ## fixed effs
    (1|phylo) + (1|spps),  ## rnd effs 
  data = gdd.stan, 
  family = gaussian(), cov_ranef = list("phylo:type" = A),
  prior = c(
    prior(normal(0, 400), "Intercept"),
    prior(student_t(3, 0, 100), "sd"),
    prior(student_t(3, 0, 100), "sigma")
  )
  ,sample_prior = TRUE, chains = 4,
  control=list(max_treedepth = 15,adapt_delta = 0.99),
  iter = 5000, warmup = 2000
)

save(model_phylo, file="stan/phylomod.Rdata")
}


### Now let's check out the data...
#sppeffs<-rbind(
siteeffs<-rbind(
  summary(model_phylo)$fixed,
  summary(model_phylo)$random$phylo,
  summary(model_phylo)$random$spps,
  summary(model_phylo)$spec_pars
)

write.csv(siteeffs,"output/site_effects.csv")
#write.csv(sppeffs,"output/species_effects.csv")


# Plot and save main results for posterior distributions of coefficients
## and ppcs

# site
plot(model_phylo, N = 5, ask = F)

#plot marginal effs
plot(marginal_effects(model_phylo), points = TRUE,ask=T) 


#plot ppcs
pp_check(model_phylo)


############################
###### Plot and save main results for posterior distributions of phylosignal
hyp.site <- paste(
  "(sd_phylo__Intercept^2)/", 
  "(sd_phylo__Intercept^2 
  + sd_spps__Intercept
  + sigma^2) = 0.0"
  )
(lambda.site <- hypothesis(model_phylo, hyp.site, class = NULL))


## site
hyp.site <- paste(
  "(sd_phylo__Intercept^2 + sd_phylo__urban^2)/", 
  "(sd_phylo__Intercept^2 
  + sd_phylo__urban^2
  + sd_spps__Intercept
  + cor_phylo__Intercept__urban^2 
  + sigma^2) = 0.0"
  )
(lambda.site <- hypothesis(model_phylo, hyp.site, class = NULL))

library(broom)
mod.summary <- tidy(model_phylo)
positions = grep(",Intercept]", mod.summary$term)
site.slopes <- mod.summary[positions,] 
site.slopes$phylo <- sort(unique(gdd.stan$spps))


##############################################################
############## Visualize phylogenetic structure ##############
##############################################################

site = site.slopes$estimate
names(site)=site.slopes$phylo
species <- unique(gdd.stan$spps)
pruned.tree<-drop.tip(phylo,phylo$tip.label[-match(species, phylo$tip.label)])
pruned.tree=multi2di(pruned.tree)
obj.site<-contMap(pruned.tree,site,plot=FALSE)
obj.site<-setMap(obj.site,invert=TRUE)
obj.site<-setMap(obj.site,colors=c("yellow","darkcyan","purple"))
#plot(obj.site,fsize=c(0.5,1),outline=FALSE,lwd=c(3,7),leg.txt="sites")


## plotting

## combined plot of three phylogenies and cues
par(mfrow=c(2,1))
layout.matrix <- matrix(c(1, 2))
layout(mat = layout.matrix,
  heights=c(1, 2))
## plot hypotheses on upper row
d <- density(lambda.site$samples[,1])
plot(d,main="",xlab="",
     xlim=c(0,1),col="darkblue")
polygon(d, col=adjustcolor("darkblue",0.4), border="darkblue")
abline(v=mean(lambda.site$samples[,1]),lty=2,col="blue")

plot.contMap(obj.site,type = "phylogram",legend = 0.6*max(nodeHeights(phylo)),
             fsize = c(0.75, 0.7), outline=FALSE,lwd=2,mar = c(1,1,2,1))
title("Site",xpd = T)
dev.off()



###########################################################
#### Checking phylogenetic structure with PGLS - caper ####
###########################################################

# generate a comparative data object for caper
sensitivities <- data.frame(site = site.slopes$estimate,
                            species = site.slopes$phylo)

phylo$node.label = 1:length(phylo$node.label)
sensitivities.compdat <- comparative.data(phylo,sensitivities,
                                          names.col= "species",
                                          vcv = T)

# fit pgls model for each cue
pgls.site.ml = pgls(site ~ 1,
                     data=sensitivities.compdat,lambda="ML")

summary(pgls.site.ml)



