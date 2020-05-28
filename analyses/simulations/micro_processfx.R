### Started 25 May 2020 by Cat
## Source file to build fake data simulations for urban versus provenance lat effects
## TAKE II: randomizing day of budburst and then calculate GDD from there

library(dplyr)
library(tidyr)

set.seed(12321)

if(use.urban==TRUE){
  
  spind <- paste(rep(c(1:nspps), each=10), rep(1:ninds, 20), sep="_")
  provenance.hf <- 42.5
  provenance.arb <- round(rnorm(nobs, provenance.hf, 5), digits=2)
  
  df.prov <- as.data.frame(cbind(sp_ind = rep(rep(spind, nsites),each=nmethods), 
                                 site = rep(c("arb", "hf"), each=nobs*nmethods),
                                 provenance = c(rep(provenance.arb, each=nmethods), rep(provenance.hf, 400)),
                                 method = rep(c("ws", "hobo"), nsites*nobs)))
  df.prov$species <- as.numeric(gsub("\\_.*" , "", df.prov$sp_ind))
  
  df.doybb <- as.data.frame(cbind(species=rep(1:nspps, each=ninds*nsites*nmethods), inds=rep(1:ninds, nmethods), 
                                  doybb=NA,
                                  site=rep(c("arb", "hf"), each=ninds*nmethods),
                                  method=rep(rep(c("ws", "hobo"), each=ninds), nsites*nspps)))
  
  df.doybb$doybb <- round(rnorm(nrow(df.doybb), doybbfstar, doybbfstarsd), digits=0)
  df.doybb$sp_ind <- paste(df.doybb$species, df.doybb$inds, sep="_")
  
  df.doybb$dayz <- df.doybb$doybb
  
  
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
  
  df.doybb$dayz <- round(df.doybb$doybb.new, digits=0)
  
}


# Step 2: find GDDs
arb.doybb <- df.doybb[(df.doybb$site=="arb"),]
hf.doybb <- df.doybb[(df.doybb$site=="hf"),]

arbclim <- data.frame(microsite=rep(rep(c(1:nmicros), each=daysperyr*nmethods),nspps), ind=rep(rep(c(1:ninds), each=daysperyr*nmethods), nspps),
                      species = rep(c(1:nspps), each=daysperyr*nmicros*nmethods), 
                      dayz=rep(arb.doybb$dayz, each=daysperyr),
                      day=rep(c(1:daysperyr), nmicros*nspps*nmethods),
                      method=rep(rep(c("ws", "hobo"), each=daysperyr),nspps*ninds),
                      site = as.character("arb"))


arbclim$tmean <- ifelse(arbclim$method=="hobo", rnorm(arbclim$day, cc.arb + microarb.effect, sigma.arb + microsigmaarb.effect), rnorm(arbclim$day, cc.arb, sigma.arb)) ### and now we draw from mean and sigma for each day to find daily temp for each microsite

arbclim <- arbclim[(arbclim$day<=arbclim$dayz),]


hfclim <- data.frame(microsite=rep(rep(c(1:nmicros), each=daysperyr*nmethods),nspps), ind=rep(rep(c(1:ninds), each=daysperyr*nmethods), nspps),
                     species = rep(c(1:nspps), each=daysperyr*nmicros*nmethods), 
                     dayz=rep(hf.doybb$dayz, each=daysperyr),
                     day=rep(c(1:daysperyr), nmicros*nspps*nmethods),
                     method=rep(rep(c("ws", "hobo"), each=daysperyr),nspps*ninds),
                     site = as.character("hf"))


hfclim$tmean <- ifelse(hfclim$method=="hobo", rnorm(hfclim$day, cc.hf + microhf.effect, sigma.hf + microsigmahf.effect), rnorm(hfclim$day, cc.hf, sigma.hf)) ### and now we draw from mean and sigma for each day to find daily temp for each microsite
hfclim <- hfclim[(hfclim$day<=hfclim$dayz),]

# Step 3: Make a data frame and get the mean temp per year (to double check the data)
df <- dplyr::full_join(arbclim, hfclim)
df$tmean <- as.numeric(df$tmean)

df$sp_ind <- paste(df$species, df$ind, sep="_")

df$gdd <- ave(df$tmean, df$sp_ind, df$site, df$method, FUN=cumsum)

df.bb <- df[(df$day==df$dayz),]
df.bb$species <- as.numeric(df.bb$species)

bball <- full_join(df.bb, df.prov)

#### Now we simplify the dataset..
bball <- subset(bball, select=c("site", "method", "species", "ind", "provenance", "dayz", "gdd"))
colnames(bball) <- c("site", "method", "species", "ind", "provenance", "bb", "gdd")

bball <- bball[!duplicated(bball),]


