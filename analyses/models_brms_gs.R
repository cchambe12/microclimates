### Let's run some quick models on growing season and provenance latitude
# 26 July 2019 - Cat

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load libraries
library(rstan)
library(brms)

## Set working directory
setwd("~/Documents/git/microclimates/analyses")


## Load the data
gs <- read.csv("output/clean_budburstandleafout.csv")

gs$gs <- gs$last.obs - gs$leafout
gs.stan <- gs[!is.na(gs$gs),]
gs.stan <- gs.stan[!is.na(gs.stan$provenance.lat),]

gs.stan$site <- NA
gs.stan$site <- ifelse(gs.stan$type=="Treespotters", "arb", gs.stan$site)
gs.stan$site <- ifelse(gs.stan$type=="Common Garden", "cg", gs.stan$site)
gs.stan$site <- ifelse(gs.stan$type=="Harvard Forest", "hf", gs.stan$site)

gs.stan$spp <- paste(substr(gs.stan$genus, 0, 3), substr(gs.stan$species, 0, 3), sep="")


modgsall <- brm(gs ~ site + provenance.lat + (site + provenance.lat|spp), data=gs.stan, 
                  control=list(max_treedepth = 15,adapt_delta = 0.99),
                  iter=4000, warmup=2500)

save(modgsall, file="stan/gsall.Rdata")
