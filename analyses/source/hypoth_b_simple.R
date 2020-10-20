#### Simulations code for Hypothesis B: hobo loggers better capture the urban effects (or provenance effects)
## I will just examine the urban effect here, so comparing the Arboretum to the Forest

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
fstarindsd <- 20 ## sigma_y in model output


#### This is where I test our hypothesis. This doesn't come out of the model directly
ws_sd <- 20  ### adds variation to weather station estimates, rendering hobo loggers more accurate measures and therefore better able to capture urban effects


### Now the climate data 
dayz <- rep(1:daysperyr, nobs)
cc.arb <- 11
sigma.arb <- 5 

cc.hf <- 9
sigma.hf <- 5  

#### Next I set up an fstar or a GDD threshold for each individual
spind <- paste(rep(1:nspps, each=10), rep(1:ninds, 20), sep="_")

fstarspp <- round(rnorm(nspps, fstar, fstarspeciessd), digits=0)
df.fstar <- as.data.frame(cbind(species=rep(1:nspps, each=ninds*nsites*nmethods), inds=rep(1:ninds, nmethods), 
                                fstarspp=rep(fstarspp, each=ninds*nsites*nmethods),
                                site=rep(c("arb", "hf"), each=ninds*nmethods),
                                method=rep(rep(c("ws", "hobo"), each=ninds), nsites*nspps)))

df.fstar$fstarspp <- as.numeric(df.fstar$fstarspp)
df.fstar$sp_ind <- paste(df.fstar$species, df.fstar$inds, sep="_")


########################### ADDING IN HYPOTHESIS HERE! ################################
### I think this should just make the weather station less accurate...??? I hope.
df.fstar$hyp_b <- ifelse(df.fstar$method=="ws", 1, 0)  ## This won't be spit out of the model. If it's the weather station, make it a 1 if it's the hobo logger make it a 0
df.fstar$gdd.noise <- df.fstar$gdd.noise + df.fstar$hyp_b * rep(rnorm(n=nspps, mean=0, sd=ws_sd), each=ninds*nmethods) ### Now, I am just adding more sigma to the weather station fstar values, seen by sd=ws_sd (which was 20)


df.fstar$fstar.new <- rnorm(df.fstar$inds, df.fstar$gdd.noise, fstarindsd)


### # Step 2: find GDDs
#### Now I set up climate data for the Arboretum, this is the weather station data
arbclim <- data.frame(microsite=rep(rep(c(1:nmicros), each=daysperyr*nmethods),nspps), ind=rep(rep(c(1:ninds), each=daysperyr*nmethods), nspps),
                      species = rep(c(1:nspps), each=daysperyr*nmicros*nmethods), 
                      day=rep(c(1:daysperyr), nmicros*nspps*nmethods),
                      method=rep(rep(c("ws", "hobo"), each=daysperyr), nspps*ninds),
                      site = as.character("arb"))



#### Harvard Forest climate data, weather station data
hfclim <- data.frame(microsite=rep(rep(c(1:nmicros), each=daysperyr*nmethods),nspps), ind=rep(rep(c(1:ninds), each=daysperyr*nmethods), nspps),
                     species = rep(c(1:nspps), each=daysperyr*nmicros*nmethods), 
                     day=rep(c(1:daysperyr), nmicros*nspps*nmethods),
                     method=rep(rep(c("ws", "hobo"), each=daysperyr),nspps*ninds),
                     site = as.character("hf"))


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

df.bb$gdd <- df.bb$gdd.obs

df.bb <- subset(df.bb, select=c("site", "method", "species", "ind", "bb", "gdd", "gdd.noise", "fstar.new"))
df.bb$species <- as.numeric(df.bb$species)

bball <- df.bb[!duplicated(df.bb),]

