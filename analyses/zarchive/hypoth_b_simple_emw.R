#### Simulations code for Hypothesis B: hobo loggers better capture the urban effects (or provenance effects)
## I will just examine the urban effect here, so comparing the Arboretum to the Forest
# Simple, Noisy method, without microclimates
### Micro, Noisy method: can apply noisy fstar threshold and then add noise to climate data

## Simple, provenance
## Provenance model with microclimates with no noise! And THEN have a provenance model with microclimates with noise to be able to detect noise
## Mega Provenance and micro hypothesis, need climate data, different trees in different microclimates require different fstars as another hypothesis. Add noise after finding day of year, then add noise to calculate gdd rather than manipulate fstar values beforehand
# permute the climate data to make one messier, then get observed GDDs. Can you tease out the differences in the model with provenance and microclimates

#### Urban model is SUPER simple, just adding a fixed effect if hypothesis is that urban trees get more chilling so ALL trees at the Arb would require fewer GDDs

# Once you believe in microclimates, then you know weather station data can't capture it!!!!!! Need to make sure this coded correctly.

### Add on factors/constants for the predictors!! Don't add like gdd.noise

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
fstarindsd <- 20 ## sigma_y in model output # emw -- not necessarily, sigma_y captures ALL unmodeled error in the model ... so you could have this as your only other error but to me usually sigma_y represents at least measurement error and is usually unique to each observation (it is modeled as unique to each observation)
# Can add a more traditional sigma_y at the end rather than using this, and use the sigma_y as the variation across sites

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

table(df.fstar$species, df.fstar$site, df.fstar$method) # emw -- checking

df.fstar$fstarspp <- as.numeric(df.fstar$fstarspp)
df.fstar$sp_ind <- paste(df.fstar$species, df.fstar$inds, sep="_")


########################### ADDING IN HYPOTHESIS HERE! ################################
### I think this should just make the weather station less accurate...??? I hope.
df.fstar$hyp_b <- ifelse(df.fstar$method=="ws", 1, 0)  ## This won't be spit out of the model. If it's the weather station, make it a 1 if it's the hobo logger make it a 0
df.fstar$gdd.noise <- df.fstar$hyp_b * rep(rnorm(n=nspps, mean=0, sd=ws_sd), each=ninds*nmethods) ### Now, I am just adding more sigma to the weather station fstar values, seen by sd=ws_sd (which was 20) # emw -- deleted starter of df.fstar$gdd.noise + from above


#### End this one here for simple, noisy method
# emw -- this takes a NEW random draw for each value -- centered at the df.fstar$gdd.noise value with an SD of 20 ... it adds a lot of noise that will be hard to understand, I don't think you need it
df.fstar$fstar.new <- rnorm(df.fstar$inds, df.fstar$gdd.noise, fstarindsd)
# emw -- why not just ...
df.fstar$fstar.new <- df.fstar$fstarspp + df.fstar$gdd.noise

# emw -- you could even just wrap up the data here with ...
sigma_y <- 2
df.fstar$fstar.new <- df.fstar$fstarspp + df.fstar$gdd.noise + rnorm(nrow(df.fstar), 0, sigma_y)
## emw -- do you need to model the weather? At this point above you have the data for the model I think (the sites are the same, but you could easily add in that effect) and I am not sure what you gain from the below. Though it should not harm anything though you would probably add the sigma_y then to the observed GDDs, not above. Logically from the hypothesis if you do want to add in the climate data I would add it in BEFORE the noise I think, but again, I am not sure it matters (if you want the below step I would definitely plot these fstar values against the GDD values below and make sure they agree). 

### # Step 2: find GDDs
#### Now I set up climate data for the Arboretum, this is the weather station data
arbclim <- data.frame(microsite=rep(rep(c(1:nmicros), each=daysperyr*nmethods),nspps),
                      ind=rep(rep(c(1:ninds), each=daysperyr*nmethods), nspps),
                      species = rep(c(1:nspps), each=daysperyr*nmicros*nmethods), 
                      day=rep(c(1:daysperyr), nmicros*nspps*nmethods),
                      method=rep(rep(c("ws", "hobo"), each=daysperyr), nspps*ninds),
                      site = as.character("arb"))



#### Harvard Forest climate data, weather station data
hfclim <- data.frame(microsite=rep(rep(c(1:nmicros), each=daysperyr*nmethods),nspps),
                     ind=rep(rep(c(1:ninds), each=daysperyr*nmethods), nspps),
                     species = rep(c(1:nspps), each=daysperyr*nmicros*nmethods), 
                     day=rep(c(1:daysperyr), nmicros*nspps*nmethods),
                     method=rep(rep(c("ws", "hobo"), each=daysperyr),nspps*ninds),
                     site = as.character("hf"))

# emw -- I am missing a step where the temp is added I think ...

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

