#### Simulations code for Hypothesis A: hobo loggers are less accurate measures of the same weather
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


### Here I integrate the model parameters
urbeffect <- -20      ### IF NEGATIVE, THEN THIS MEANS WE EXPECT THE ARBORETUM REQUIRES FEWER GDD! Maybe because chilling is higher?
urbsd <- 5 ## sigma_b_urban_sp in model output
methodeffect <- -25    ### IF NEGATIVE, THEN THIS MEANS WE EXPECT THE STATION MEASURES FEWER GDD! Maybe because it is cooler, thus accumulating GDD more slowly
methodsd <- 5 ## sigma_b_method_sp in model output
urbmethod <- -70 
urbmethodsd <- 10 ## sigma_b_um_sp in model 

#### This is where I test our hypothesis. This doesn't come out of the model directly
hobo_sd <- 20  ### adds variation to hobo logger estimates, rendering hobo loggers less accurate measures of same weather... I hope


### Now the climate data 
dayz <- rep(1:daysperyr, nobs)
cc.arb <- 11
sigma.arb <- 5 
microarb.effect <- 0  #### This is added or subtracted from the cc.arb climate value
microsigmaarb.effect <- 0   ### This is the sigma around the microclimate data

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

df.fstar$fstarspp <- as.numeric(df.fstar$fstarspp)
df.fstar$sp_ind <- paste(df.fstar$species, df.fstar$inds, sep="_")


##### Lines 123-130 add in the stan model estimates and parameters. We should get these back.
df.fstar$urbtx <- ifelse(df.fstar$site=="arb", 1, 0) ## This is just saying, if it's the arboretum then make it a 1, if it's the forest then make it a 0
df.fstar$gdd.noise  <- df.fstar$fstarspp + df.fstar$urbtx * rep(rnorm(n=nspps, mean=urbeffect, sd=urbsd), each=ninds*nmethods)  ## So, if it's the arboretum, we're adding that urban effect

df.fstar$tx <- ifelse(df.fstar$method=="ws", 1, 0) ### As above, this is just saying if it's the weather station, make it a 1, if it's the hobo logger make it a 0
df.fstar$gdd.noise <- df.fstar$gdd.noise + df.fstar$tx * rep(rnorm(n=nspps, mean=methodeffect, sd=methodsd), each=ninds*nmethods) ## And if it's the weather station then we're adding that method effect

df.fstar$urbmethodtx <- ifelse(df.fstar$method=="ws" & df.fstar$site=="arb", 1, 0) ### And now for the interaction, if it's the arboretum using weather station data, we're going to add even more of an effect
df.fstar$gdd.noise <- df.fstar$gdd.noise + df.fstar$urbmethodtx * rep(rnorm(n=nspps, mean=urbmethod, sd=urbmethodsd), each=ninds*nmethods)  

########################### ADDING IN HYPOTHESIS HERE! ################################
### I think this should just make the hobo logger less accurate...??? I hope.
  df.fstar$hyp_a <- ifelse(df.fstar$method=="hobo", 1, 0)  ## Similar to above but this won't be spit out of the model. If it's the hobo logger, make it a 1 if it's the weather station make it a 0
  df.fstar$gdd.noise <- df.fstar$gdd.noise + df.fstar$hyp_a * rep(rnorm(n=nspps, mean=0, sd=hobo_sd), each=ninds*nmethods) ### Now, instead of adding an effect, I am just adding more sigma to the hobo logger fstar values, seen by sd=hobo_sd (which was 20)
  

df.fstar$fstar.new <- rnorm(df.fstar$inds, df.fstar$gdd.noise, fstarindsd)


### # Step 2: find GDDs
#### Now I set up climate data for the Arboretum, this is the weather station data
arbclim <- data.frame(microsite=rep(rep(c(1:nmicros), each=daysperyr*nmethods),nspps), ind=rep(rep(c(1:ninds), each=daysperyr*nmethods), nspps),
                      species = rep(c(1:nspps), each=daysperyr*nmicros*nmethods), 
                      day=rep(c(1:daysperyr), nmicros*nspps*nmethods),
                      method=rep(rep(c("ws", "hobo"), each=daysperyr), nspps*ninds),
                      site = as.character("arb"))


### This is how I get the hobo logger data
arbclim$tmean <- if(arbclim$method=="hobo"){
  rnorm(as.numeric(arbclim$day), cc.arb + microarb.effect, sigma.arb + microsigmaarb.effect)
  }else if(arbclim$method=="ws")
    {rnorm(as.numeric(arbclim$day), cc.arb, sigma.arb)} ### and now we draw from mean and sigma for each day to find daily temp for each microsite


#### Harvard Forest climate data, weather station data
hfclim <- data.frame(microsite=rep(rep(c(1:nmicros), each=daysperyr*nmethods),nspps), ind=rep(rep(c(1:ninds), each=daysperyr*nmethods), nspps),
                     species = rep(c(1:nspps), each=daysperyr*nmicros*nmethods), 
                     day=rep(c(1:daysperyr), nmicros*nspps*nmethods),
                     method=rep(rep(c("ws", "hobo"), each=daysperyr),nspps*ninds),
                     site = as.character("hf"))


### Again, where I set up the hobo logger data
hfclim$tmean <- if(hfclim$method=="hobo"){
   rnorm(hfclim$day, cc.hf + as.numeric(microhf.effect), sigma.hf + microsigmahf.effect)
} else if(hfclim$method=="ws"){
   rnorm(hfclim$day, cc.hf, sigma.hf)} ### and now we draw from mean and sigma for each day to find daily temp for each microsite


# Step 3: Make a data frame and get the mean temp
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

