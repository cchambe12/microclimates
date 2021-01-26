#### Simulations code for Hypothesis B: hobo loggers better capture the actual GDD
# Simple, Noisy method, without microclimates

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(dplyr)
library(tidyr)

set.seed(12321)

# Step 1: Set up years, days per year, temperatures, sampling frequency, required GDD (fstar)
daysperyr <- 300 #### just to make sure we don't get any NAs
nspps <- 20 
ninds <- 10 
ninds_perprov <- 1 ## This means I want x individuals per species to have the same prov at the Arboretum to make it easier on the model
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
prov_effect <- -20  ## provenance effect, this is saying that if sites are from 1 degree north, they require 5 fewer GDD
prov_sd <- 5 ## prov effect sd

### Now the climate data 
dayz <- rep(1:daysperyr, nobs)
cc.arb <- 11
sigma.arb <- 5 
microarb.effect <- 2
microsigmaarb.effect <- 10   #### by keeping the sigmas the same for the microsites (line 94 & 99) we assume that the microclimatic effects are the same across sites

cc.hf <- 9
sigma.hf <- 5
microhf.effect <- 2
microsigmahf.effect <- 10  #### by keeping the sigmas the same for the microsites (line 94 & 99) we assume that the microclimatic effects are the same across sites

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
df.bb$species <- as.numeric(df.bb$species)
df.bb$provenance <- as.numeric(df.bb$provenance)
df.bb$prov.z <- (df.bb$provenance-mean(df.bb$provenance,na.rm=TRUE))/(2*sd(df.bb$provenance,na.rm=TRUE))

### # Step 2: find GDDs
#### Now I set up climate data for the Arboretum, this is the weather station data
arbclim <- data.frame(microsite=rep(rep(c(1:nmicros), each=daysperyr*nmethods),nspps),
                      ind=rep(rep(c(1:ninds), each=daysperyr*nmethods), nspps),
                      species = rep(c(1:nspps), each=daysperyr*nmicros*nmethods), 
                      day=rep(c(1:daysperyr), nmicros*nspps*nmethods),
                      method=rep(rep(c("ws", "hobo"), each=daysperyr), nspps*ninds),
                      site = as.character("arb"))

### This is how I get weather station data versus hobo logger data
arbclim$tmean <- ifelse(arbclim$method=="hobo", rnorm(as.numeric(arbclim$day), cc.arb + microarb.effect, sigma.arb + microsigmaarb.effect), 
                        rnorm(as.numeric(arbclim$day), cc.arb, sigma.arb)) 
### and now we draw from mean and sigma for each day to find daily temp for each microsite



#### Harvard Forest climate data, weather station data
hfclim <- data.frame(microsite=rep(rep(c(1:nmicros), each=daysperyr*nmethods),nspps),
                     ind=rep(rep(c(1:ninds), each=daysperyr*nmethods), nspps),
                     species = rep(c(1:nspps), each=daysperyr*nmicros*nmethods), 
                     day=rep(c(1:daysperyr), nmicros*nspps*nmethods),
                     method=rep(rep(c("ws", "hobo"), each=daysperyr),nspps*ninds),
                     site = as.character("hf"))

### Again, where I set up the difference between hobo logger and weather station
hfclim$tmean <- ifelse(hfclim$method=="hobo", rnorm(hfclim$day, cc.hf + as.numeric(microhf.effect), 
                                                    sigma.hf + microsigmahf.effect), 
                       rnorm(hfclim$day, cc.hf, sigma.hf)) 
### and now we draw from mean and sigma for each day to find daily temp for each microsite


##Step 3: Make a data frame and get the mean temp
df <- dplyr::full_join(arbclim, hfclim)
df$tmean <- as.numeric(df$tmean)

df$sp_ind <- paste(df$species, df$ind, sep="_")

### Calculate the OBSERVED GDDs!!!
df$gdd.obs <- ave(df$tmean, df$sp_ind, df$site, df$method, FUN=cumsum)


### Let's just tidy everything up
df$species <- as.numeric(df$species)
df$ind <- as.numeric(df$ind)
df.bb$ind <- as.numeric(df.bb$ind)
df <- left_join(df, df.bb)

df$spind_site_method <- paste0(df$sp_ind, df$site, df$method)

## Find the day of budburst to find the actual GDD versus the "observed GDD"
for(i in c(unique(df$spind_site_method))){ 
  
  bb <- which(df$gdd.obs[i==df$spind_site_method] >= df$fstarspp[i==df$spind_site_method])[1]
  df$bb[i==df$spind_site_method] <- bb
  
}

df.bb <- df[(df$bb==df$day),]

# Generate random parameters (by species)
df.bb$gdd.noise <- df.bb$prov.z * rep(rnorm(n=nspps, mean=prov_effect, sd=prov_sd), each=ninds*nsites)

df.bb$gdd <- df.bb$gdd.obs + df.bb$gdd.noise + rnorm(n=ntot, mean=0, sd=sigma_y)

##### Clean up the dataframe to prepare for analyses
df.bb <- subset(df.bb, select=c("site", "method", "species", "ind", "gdd.noise", "fstarspp", "gdd", "provenance")) # 

bball <- df.bb[!duplicated(df.bb),]


##### Now let's do some checks...
bball$gdd_accuracy <- bball$gdd - bball$fstarspp
bball$type <- ifelse(bball$method=="ws", 1, 0)

bball <- na.omit(bball)

