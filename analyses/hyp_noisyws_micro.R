#### Simulations code for Hypothesis B: hobo loggers better capture the actual GDD

### Micro, Noisy method: can apply noisy fstar threshold and then add noise to climate data


# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(dplyr)
library(tidyr)

set.seed(12321)

# Step 1: Set up years, days per year, temperatures, sampling frequency, required GDD (fstar)
daysperyr <- 100 #### just to make sure we don't get any NAs
nspps <- 20 
ninds <- 10 
nobs <- nspps*ninds
nsites <- 2  ### Arboretum versus the Forest
nmicros <- 10  ### Number microsites per site so 20 total 
nmethods <- 2

### These are our fstar thresholds
fstar <- 300  ### mu_a_sp in model output
fstarspeciessd <- 50 ### sigma_a_sp in model output

## Sigma_y to be added at the end
sigma_y <- 2

#### This is where I test our hypothesis. This doesn't come out of the model directly
ws_sd <- 20  ### adds variation to weather station estimates, rendering hobo loggers more accurate measures and therefore better able to capture urban effects

### Now the climate data 
dayz <- rep(1:daysperyr, nobs)
cc.arb <- 11
sigma.arb <- 5 
microarb.effect <- 0
microsigmaarb.effect <- 0   #### by keeping the sigmas the same for the microsites (line 94 & 99) we assume that the microclimatic effects are the same across sites

cc.hf <- 9
sigma.hf <- 5
microhf.effect <- 0
microsigmahf.effect <- 0  #### by keeping the sigmas the same for the microsites (line 94 & 99) we assume that the microclimatic effects are the same across sites


#### Next I set up an fstar or a GDD threshold for each individual
spind <- paste(rep(1:nspps, each=10), rep(1:ninds, 20), sep="_")

fstarspp <- round(rnorm(nspps, fstar, fstarspeciessd), digits=0)
df.fstar <- as.data.frame(cbind(species=rep(1:nspps, each=ninds*nsites*nmethods), inds=rep(1:ninds, nmethods), 
                                fstarspp=rep(fstarspp, each=ninds*nsites*nmethods),
                                site=rep(c("arb", "hf"), each=ninds*nmethods),
                                method=rep(rep(c("ws", "hobo"), each=ninds), nsites*nspps)))

table(df.fstar$species, df.fstar$site, df.fstar$method) # emw -- checking

df.fstar$fstarspp <- as.numeric(df.fstar$fstarspp)
df.fstar$sp_ind <- paste(df.fstar$species, df.fstar$inds, sep="_")


####### QUESTION HERE... AND ALSO ON LINES 133-140, do I add this here or after the climate data?
########################### ADDING IN HYPOTHESIS HERE! ################################
### I think this should just make the weather station less accurate...??? I hope.
df.fstar$hyp_b <- ifelse(df.fstar$method=="ws", 1, 0)  ## This won't be spit out of the model. If it's the weather station, make it a 1 if it's the hobo logger make it a 0
### Now, I am just adding more sigma to the weather station fstar values, seen by sd=ws_sd (which was 20) # emw -- deleted starter of df.fstar$gdd.noise + from above
df.fstar$gdd.noise <- df.fstar$hyp_b * rep(rnorm(n=nspps, mean=0, sd=ws_sd), each=ninds*nmethods) 

df.fstar$fstar.new <- df.fstar$fstarspp + df.fstar$gdd.noise + rnorm(nrow(df.fstar), mean=0, sd=sigma_y)



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
df$species <- as.character(df$species)
df <- left_join(df, df.fstar)

df$spind_site_method <- paste0(df$sp_ind, df$site, df$method)

## Find the day of budburst to find the actual GDD versus the "observed GDD"
for(i in c(unique(df$spind_site_method))){ 
  
  bb <- which(df$gdd.obs[i==df$spind_site_method] >= df$fstar.new[i==df$spind_site_method])[1]
  df$bb[i==df$spind_site_method] <- bb
  
}

df.bb <- df[(df$bb==df$day),]

if(FALSE){
####### QUESTION HERE... AND ALSO ON LINES 64-71, do I add this here or before the climate data?
## After a series of tests, it seems to be BEFORE the climate data
########################### ADDING IN HYPOTHESIS HERE! ################################
### I think this should just make the weather station less accurate...??? I hope.
df.bb$hyp_b <- ifelse(df.bb$method=="ws", 1, 0)  ## This won't be spit out of the model. If it's the weather station, make it a 1 if it's the hobo logger make it a 0
### Now, I am just adding more sigma to the weather station fstar values, seen by sd=ws_sd (which was 20) # emw -- deleted starter of df.fstar$gdd.noise + from above
df.bb$gdd.noise <- df.bb$hyp_b * rep(rnorm(n=nspps, mean=0, sd=ws_sd), each=ninds*nmethods) 

df.bb$gdd <- df.bb$gdd.obs + df.bb$gdd.noise + rnorm(nrow(df.bb), mean=0, sd=sigma_y)

df.bb$fstar.new <- NA
}

df.bb$gdd <- df.bb$gdd.obs

##### Now add in provenance so better able to compare to other simulations
spind <- paste(rep(c(1:nspps), each=ninds), rep(1:ninds, nspps), sep="_")
provenance.hf <- 42.5
provenance.arb <- round(rnorm(nobs, provenance.hf, 5), digits=2)

df.prov <- as.data.frame(cbind(sp_ind = rep(rep(spind, nsites),each=nmethods), 
                               site = rep(c("arb", "hf"), each=nobs*nmethods),
                               provenance = c(rep(provenance.arb, each=nmethods), rep(provenance.hf, 400)),
                               method = rep(c("ws", "hobo"), nsites*nobs)))
df.prov$species <- as.numeric(gsub("\\_.*" , "", df.prov$sp_ind))

df.fstar$species <- as.numeric(df.fstar$species)

df.bb <- full_join(df.fstar, df.prov)

df.bb <- subset(df.bb, select=c("site", "method", "species", "fstarspp", "inds", "gdd.noise", "fstar.new", "provenance"))
df.bb$species <- as.numeric(df.bb$species)

bball <- df.bb[!duplicated(df.bb),]
bball$gdd <- bball$fstar.new

