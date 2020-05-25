### Started 25 May 2020 by Cat
## Source file to build fake data simulations for urban versus provenance lat effects
## TAKE II: randomizing day of budburst and then calculate GDD from there

library(dplyr)
library(tidyr)

set.seed(12321)

if(use.urban==TRUE){
  
  provenance.arb <- round(rnorm(nobs, 42.5, 8), digits=2)
  provenance.hf <- 42.5
  
  doybb <- round(rnorm(nspps, doybb, doybbspeciessd), digits=0) ## day of year now
  df.doybb <- as.data.frame(cbind(species=rep(1:nspps, each=ninds*nsites), inds=1:ninds, doybb=rep(doybb, each=ninds*nsites),
                                  site=rep(c("arb", "hf"), each=ninds)))
  df.doybb$doybb <- as.numeric(df.doybb$doybb)
  df.doybb$sp_ind <- paste(df.doybb$species, df.doybb$inds, sep="_")
  
  df.doybb$doybb.new <- round(ifelse(df.doybb$site=="hf", rnorm(df.doybb$inds, df.doybb$doybb, doybbindsd), 
                                     rnorm(df.doybb$inds, df.doybb$doybb+urbeffect, doybbindsd)), digits=0)
  
  df.doybb$dayz <- df.doybb$doybb.new
  
}

if(use.provenance==TRUE){
  
  provenance.arb <- round(rnorm(nobs, 42.5, 10), digits=2)
  provenance.hf <- 42.5
  
  doybb <- round(rnorm(nspps, doybb, doybbspeciessd), digits=0)
  df.doybb <- as.data.frame(cbind(species=rep(1:nspps, each=ninds*nsites), inds=1:ninds, doybb=rep(doybb, each=ninds*nsites),
                                  site=rep(c("arb", "hf"), each=ninds)))
  df.doybb$doybb <- as.numeric(df.doybb$doybb)
  df.doybb$sp_ind <- paste(df.doybb$species, df.doybb$inds, sep="_")
  
  df.doybb$provenance <- ifelse(df.doybb$site=="hf", provenance.hf, provenance.arb)
  df.doybb$prov.adj <- ifelse(df.doybb$provenance!=provenance.hf, df.doybb$provenance-provenance.hf, 0)
  
  df.doybb$doybb.new <- round(ifelse(df.doybb$site=="hf", rnorm(df.doybb$inds, df.doybb$doybb, doybbindsd), 
                                     rnorm(df.doybb$inds, df.doybb$doybb+(df.doybb$prov.adj*proveffect), doybbindsd)), digits=0)
  
  df.doybb$dayz <- df.doybb$doybb.new
  
}


# Step 2: find GDDs
arb.doybb <- df.doybb[(df.doybb$site=="arb"),]
hf.doybb <- df.doybb[(df.doybb$site=="hf"),]

#arbmicromeans <- rnorm(nmicros, cc.arb, mean.microarb)  ### create mean temperature for each microsite
#arbmicrosigmas <- rnorm(nmicros, sigma.arb, sigma.microarb) ### and now sigma temperature for each microsite
arbclim <- data.frame(microsite=rep(rep(c(1:nmicros), each=daysperyr),nspps), ind=rep(rep(c(1:ninds), each=daysperyr), nspps),
                      species = as.character(rep(c(1:nspps), each=daysperyr*nmicros)), 
                      dayz=rep(arb.doybb$dayz, each=daysperyr),
                      #means=rep(rep(cc.arbmicro, each=daysperyr),nspps), ## just establishes mean for each site
                      #sigmas=rep(rep(sigma.arbmicro, each=daysperyr),nspps), ## and now sigma for each site
                      day=rep(c(1:daysperyr), nmicros*nspps),
                      site = as.character("arb"))


arbclim$tmean <- rnorm(arbclim$day, cc.arbmicro, sigma.arbmicro) ### and now we draw from mean and sigma for each day to find daily temp for each microsite
arbclim$tmean.ws <- rnorm(arbclim$day, cc.arb, sigma.arb)  ### and now we just do the same using the original cc estimates and sigma for the arb
arbclim <- arbclim[(arbclim$day<=arbclim$dayz),]


#hfmicromeans <- rnorm(nmicros, cc.hf, mean.microhf) ### create mean temperature for each microsite
#hfmicrosigmas <- rnorm(nmicros, sigma.hf, sigma.microhf) ### and now sigma temperature for each microsite
hfclim <- data.frame(microsite=rep(rep(c(1:nmicros), each=daysperyr),nspps), ind=rep(rep(c(1:ninds), each=daysperyr), nspps),
                      species = as.character(rep(c(1:nspps), each=daysperyr*nmicros)), 
                      dayz=rep(hf.doybb$dayz, each=daysperyr),
                      #means=rep(rep(cc.hfmicro, each=daysperyr),nspps), ## just establishes mean for each site
                      #sigmas=rep(rep(cc.sigmamicro, each=daysperyr),nspps), ## and now sigma for each site
                     day=rep(c(1:daysperyr), nmicros*nspps),
                      site = as.character("hf"))


hfclim$tmean <- rnorm(hfclim$day, cc.hfmicro, sigma.hfmicro) ### and now we draw from mean and sigma for each day to find daily temp for each microsite
hfclim$tmean.ws <- rnorm(hfclim$day, cc.hf, sigma.hf) ### and now we just do the same using the original cc estimates and sigma for the arb
hfclim <- hfclim[(hfclim$day<=hfclim$dayz),]

# Step 3: Make a data frame and get the mean temp per year (to double check the data)
df <- full_join(arbclim, hfclim)
df$tmean <- as.numeric(df$tmean)

df$microsite <- paste0(df$site, df$ind)

df$sp_ind <- paste(df$species, df$ind, sep="_")

df$gdd.ws <- ave(df$tmean.ws, df$sp_ind, df$site, FUN=cumsum)
df$gdd.hl <- ave(df$tmean, df$sp_ind, df$microsite, FUN=cumsum)

#### Now we simplify the dataset..
bball <- df[(df$day==df$dayz),]

bball <- subset(bball, select=c("site", "microsite", "ind", "species", "day", "gdd.ws", "gdd.hl"))
colnames(bball) <- c("site", "microsite", "ind", "species", "bb", "ws", "hl")

bball <- tidyr::gather(bball, "method", "gdd", -site, -microsite, -ind, -species, -bb)




