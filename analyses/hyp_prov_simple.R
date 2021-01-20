#### Simulations code for Hypothesis B: hobo loggers better capture the actual GDD
# Simple, Noisy method, without microclimates

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(dplyr)
library(tidyr)

set.seed(12321)

# Step 1: Set up years, days per year, temperatures, sampling frequency, required GDD (fstar)
nspps <- 15 
ninds <- 18 
ninds_perprov <- 6 ## This means I want 6 individuals per species to have the same prov at the Arboretum to make it easier on the model
nobs <- nspps*ninds
nsites <- 2  ### Arboretum versus the Forest
nmicros <- 10  ### Number microsites per site so 20 total 
nmethods <- 2
ntot <- nobs * nmethods * nsites

### These are our fstar thresholds
fstar <- 300  ### mu_a_sp in model output
fstarspeciessd <- 50 ### sigma_a_sp in model output

## Sigma_y to be added at the end
sigma_y <- 2 

#### This is where I test our hypothesis. This doesn't come out of the model directly
prov_effect <- 5  ## provenance effect, this is saying that if sites are from 1 degree north, they require 5 fewer GDD
prov_sd <- 0.5 ## prov effect sd

#### Next I set up an fstar or a GDD threshold for each individual
#spind <- paste(rep(1:nspps, each=ninds), rep(1:ninds, nspps), sep="_")

fstarspp <- round(rnorm(nspps, fstar, fstarspeciessd), digits=0)
df.fstar <- as.data.frame(cbind(species=rep(1:nspps, each=ninds*nsites*nmethods), ind=rep(1:ninds, nmethods), 
                                fstarspp=rep(fstarspp, each=ninds*nsites*nmethods),
                                site=rep(c("arb", "hf"), each=ninds*nmethods),
                                method=rep(rep(c("ws", "hobo"), each=ninds), nsites*nspps)))

table(df.fstar$species, df.fstar$site, df.fstar$method) # emw -- checking

df.fstar$fstarspp <- as.numeric(df.fstar$fstarspp)
#df.fstar$sp_ind <- paste(df.fstar$species, df.fstar$ind, sep="_")

##### Now add in provenance so better able to compare to other simulations
spind <- paste(rep(c(1:nspps), each=ninds), rep(1:ninds, nspps), sep="_")
provenance.hf <- 42.5
provenance.arb <- round(rep(rnorm(nspps*(ninds/ninds_perprov), provenance.hf, 2),each=ninds_perprov), digits=2)

df.prov <- as.data.frame(cbind(sp_ind = rep(rep(spind, nsites),each=nmethods), 
                               site = rep(c("arb", "hf"), each=nobs*nmethods),
                               provenance = as.numeric(c(rep(provenance.arb, each=nmethods), rep(provenance.hf, nobs*nmethods))),
                               method = rep(c("ws", "hobo"), nsites*nobs)))
df.prov$species <- as.numeric(gsub("\\_.*" , "", df.prov$sp_ind))
df.prov$ind <- gsub(".*_" , "", df.prov$sp_ind)
df.prov$sp_ind <- NULL
df.fstar$species <- as.numeric(df.fstar$species)

df.bb <- full_join(df.fstar, df.prov)
df.bb$provenance <- as.numeric(df.bb$provenance)
df.bb$hyp_diff <- ifelse(df.bb$provenance==42.5, 0, (42.5-df.bb$provenance))

df.bb$gdd.noise <- df.bb$hyp_diff * rep(rnorm(n=nspps, mean=prov_effect, sd=prov_sd), each=ninds*nsites)

df.bb$gdd <- df.bb$fstarspp + df.bb$gdd.noise + rnorm(n=ntot, mean=0, sd=sigma_y)

##### Clean up the dataframe to prepare for analyses
df.bb <- subset(df.bb, select=c("site", "method", "species", "ind", "gdd.noise", "fstarspp", "gdd", "provenance")) # 

bball <- df.bb[!duplicated(df.bb),]


##### Now let's do some checks...
bball$gdd_accuracy <- bball$gdd - bball$fstarspp
bball$type <- ifelse(bball$method=="ws", 1, 0)

bball <- na.omit(bball)

