### Started 23 April 2020 by Cat
## Source file to build fake data simulations for urban versus provenance lat effects

library(dplyr)
library(tidyr)

set.seed(12321)

if(use.urban==TRUE){
  
  provenance.arb <- round(rnorm(nobs, 42.5, 8), digits=2)
  provenance.hf <- 42.5
  
  fstar <- round(rnorm(nspps, fstar, fstarspeciessd), digits=0)
  df.fstar <- as.data.frame(cbind(species=rep(1:nspps, each=ninds*nsites*nmethods), inds=rep(1:ninds, nmethods), fstar=rep(fstar, each=ninds*nsites*nmethods),
                                site=rep(c("arb", "hf"), each=ninds*nmethods),
                                method=rep(rep(c("ws", "hobo"), each=ninds), nsites*nspps)))
  df.fstar$fstar <- as.numeric(df.fstar$fstar)
  df.fstar$sp_ind <- paste(df.fstar$species, df.fstar$inds, sep="_")
 
  for(i in c(1:nrow(df.fstar))){
    if(df.fstar$site[i]=="hf" && df.fstar$method[i]=="hobo") {
    
      df.fstar$fstar.new[df.fstar$site=="hf" & df.fstar$method=="hobo"] <- 
          rnorm(df.fstar$inds[df.fstar$site=="hf" & df.fstar$method=="hobo"], 
              df.fstar$fstar[df.fstar$site=="hf" & df.fstar$method=="hobo"], fstarindsd)
    
    } else if (df.fstar$site[i]=="arb" && df.fstar$method[i]=="hobo") {
    
      df.fstar$fstar.new[df.fstar$site=="arb" & df.fstar$method=="hobo"] <- 
          rnorm(df.fstar$inds[df.fstar$site=="arb" & df.fstar$method=="hobo"], 
              df.fstar$fstar[df.fstar$site=="arb" & df.fstar$method=="hobo"] + urbeffect, fstarindsd + urbsd)
    
    } else if (df.fstar$site[i]=="hf" && df.fstar$method[i]=="ws") {
    
      df.fstar$fstar.new[df.fstar$site=="hf" & df.fstar$method=="ws"] <- 
          rnorm(df.fstar$inds[df.fstar$site=="hf" & df.fstar$method=="ws"], 
              df.fstar$fstar[df.fstar$site=="hf" & df.fstar$method=="ws"] + methodeffect, fstarindsd + methodsd)
    
    } else if (df.fstar$site[i]=="arb" && df.fstar$method[i]=="ws") {
    
      df.fstar$fstar.new[df.fstar$site=="arb" & df.fstar$method=="ws"] <- 
          rnorm(df.fstar$inds[df.fstar$site=="arb" & df.fstar$method=="ws"], 
              df.fstar$fstar[df.fstar$site=="arb" & df.fstar$method=="ws"] + methodeffect + urbeffect, fstarindsd + methodsd + urbsd)
    
    }
  }
}

if(use.provenance==TRUE){ ##### NEED TO REVAMP!!!
  
  provenance.arb <- round(rnorm(nobs, 42.5, 10), digits=2)
  provenance.hf <- 42.5
  
  fstar <- round(rnorm(nspps, fstar, fstarspeciessd), digits=0)
  df.fstar <- as.data.frame(cbind(species=rep(1:nspps, each=ninds*nsites), inds=1:ninds, fstar=rep(fstar, each=ninds*nsites),
                                  site=rep(c("arb", "hf"), each=ninds)))
  df.fstar$fstar <- as.numeric(df.fstar$fstar)
  df.fstar$sp_ind <- paste(df.fstar$species, df.fstar$inds, sep="_")
  
  df.fstar$provenance <- ifelse(df.fstar$site=="hf", provenance.hf, provenance.arb)
  df.fstar$prov.adj <- ifelse(df.fstar$provenance!=provenance.hf, df.fstar$provenance-provenance.hf, 0)
  
  df.fstar$fstar.new <- round(ifelse(df.fstar$site=="hf", rnorm(df.fstar$inds, df.fstar$fstar, fstarindsd), 
                                     rnorm(df.fstar$inds, df.fstar$fstar+(df.fstar$prov.adj*proveffect), fstarindsd)), digits=0)
  
  
}


# Step 2: find GDDs

  ## 2a) Arboretum climate data
arbclim <- data.frame(microsite=rep(rep(c(1:nmicros), each=daysperyr*nmethods),nspps), ind=rep(rep(c(1:ninds), each=daysperyr*nmethods), nspps),
                      species = as.character(rep(c(1:nspps), each=daysperyr*nmicros*nmethods)), 
                      day=rep(c(1:daysperyr), nmicros*nspps*nmethods),
                      method=rep(rep(c("ws", "hobo"), each=daysperyr),nspps*ninds),
                      site = as.character("arb"))


arbclim$tmean <- ifelse(arbclim$method=="hobo", rnorm(arbclim$day, cc.arbmicro, sigma.arbmicro), rnorm(arbclim$day, cc.arb, sigma.arb)) ### and now we draw from mean and sigma for each day to find daily temp for each microsite


  ## 2b) Harvard Forest climate data
hfclim <- data.frame(microsite=rep(rep(c(1:nmicros), each=daysperyr*nmethods),nspps), ind=rep(rep(c(1:ninds), each=daysperyr*nmethods), nspps),
                     species = as.character(rep(c(1:nspps), each=daysperyr*nmicros*nmethods)), 
                     day=rep(c(1:daysperyr), nmicros*nspps*nmethods),
                     method=rep(rep(c("ws", "hobo"), each=daysperyr),nspps*ninds),
                     site = as.character("hf"))


hfclim$tmean <- ifelse(hfclim$method=="hobo", rnorm(hfclim$day, cc.hfmicro, sigma.hfmicro), rnorm(hfclim$day, cc.hf, sigma.hf)) ### and now we draw from mean and sigma for each day to find daily temp for each microsite

# Step 3: Make a data frame and get the mean temp per year (to double check the data)
df <- full_join(arbclim, hfclim)
df$tmean <- as.numeric(df$tmean)

df$microsite <- paste0(df$site, df$ind)

df$sp_ind <- paste(df$species, df$ind, sep="_")

df$gdd <- ave(df$tmean, df$sp_ind, df$site, df$method, FUN=cumsum)

df.fstar.sub <- subset(df.fstar, select=c("site", "method", "sp_ind", "fstar.new"))
df <- full_join(df.fstar.sub, df)

#### Now we find budburst day..
df$group <- paste(df$site, df$method, df$sp_ind)
grouplist <- 1:length(unique(df$group))
grouplist <- data.frame(grouplist, group=unique(df$group))
df <- left_join(df, grouplist)

df$doybb <- NA
for(i in 1:length(unique(df$grouplist))){ #i=1
  df$doybb[df$grouplist==i] <- which(df$gdd[df$grouplist==i] >= df$fstar.new[df$grouplist==i])[1]
}

bball <- df[(df$day==df$doybb),]

bball <- subset(bball, select=c("site", "microsite", "method", "ind", "species", "day", "gdd"))
colnames(bball) <- c("site", "microsite", "method", "ind", "species", "bb", "gdd")

