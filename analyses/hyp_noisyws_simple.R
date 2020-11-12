#### Simulations code for Hypothesis B: hobo loggers better capture the urban effects (or provenance effects)
## I will just examine the urban effect here, so comparing the Arboretum to the Forest
# Simple, Noisy method, without microclimates

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(dplyr)
library(tidyr)

set.seed(12321)

# Step 1: Set up years, days per year, temperatures, sampling frequency, required GDD (fstar)
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


########################### ADDING IN HYPOTHESIS HERE! ################################
### I think this should just make the weather station less accurate...??? I hope.
df.fstar$hyp_b <- ifelse(df.fstar$method=="ws", 1, 0)  ## This won't be spit out of the model. If it's the weather station, make it a 1 if it's the hobo logger make it a 0

### Now, I am just adding more sigma to the weather station fstar values, seen by sd=ws_sd (which was 20) # emw -- deleted starter of df.fstar$gdd.noise + from above
df.fstar$gdd.noise <- df.fstar$fstarspp + df.fstar$hyp_b * rep(rnorm(n=nspps, mean=0, sd=ws_sd), each=ninds*nmethods)

df.fstar$fstar.new <- df.fstar$fstarspp + df.fstar$gdd.noise + rnorm(n=ntot, mean=0, sd=sigma_y)


