### Started 6 Oct 2020 by Cat
## Source function to build data for the shiny app
### Need to eventually integrate hypothesis tests and provenance vs urban!

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(dplyr)
library(tidyr)

set.seed(12321)

if(FALSE){
  fstar.num <- 300  ## GDD threshold
  fstar.sd <- 50
  warming <- 1
}


warmfunc <- function(fstar.num, fstar.sd, warming){
  
  # Step 1: Set up years, days per year, temperatures, sampling frequency, required GDD (fstar)
  daysperyr <- 150 #### just to make sure we don't get any NAs
  nspps <- 15 
  ninds <- 10 
  nobs <- nspps*ninds
  
  ### These are our fstar thresholds
  fstar <- fstar.num  ### mu_a_sp in model output
  fstarspeciessd <- fstar.sd ### sigma_a_sp in model output
  
  ## Sigma_y to be added at the end
  sigma_y <- 2
  
  ### Now the climate data 
  dayz <- rep(1:daysperyr, nobs)
  cc <- 10
  sigma.cc <- 2
  warmcc <- warming

  
  #### Next I set up an fstar or a GDD threshold for each individual
  spind <- paste(rep(1:nspps, each=10), rep(1:ninds, 20), sep="_")
  
  fstarspp <- round(rnorm(nspps, fstar, fstarspeciessd), digits=0)
  df.fstar <- as.data.frame(cbind(species=rep(1:nspps, each=ninds), 
                                  fstarspp=rep(fstarspp, each=ninds)))
  
  df.fstar$fstarspp <- as.numeric(df.fstar$fstarspp)
  
  #### Here, I set up provenance for each individual
  ### # Step 2: find GDDs
  #### Now I set up climate data for the Arboretum, this is the weather station data
  cc <- data.frame(ind=rep(rep(c(1:ninds), each=daysperyr), nspps),
                        species = rep(c(1:nspps), each=daysperyr), 
                        day=rep(c(1:daysperyr), nspps))
  
  
  
  ##Step 3: Make a data frame and get the mean temp
  df <- cc
  df$tmean <- as.numeric(df$tmean)
  
  df$sp_ind <- paste(df$species, df$ind, sep="_")
  
  ### Calculate the OBSERVED GDDs!!!
  df$gdd.obs <- ave(df$tmean, df$sp_ind, FUN=cumsum)
  
  ### Let's just tidy everything up
  df$species <- as.numeric(as.factor(df$species))
  df.fstar$species <- as.numeric(as.factor(df.fstar$species))
  df <- full_join(df, df.fstar)
  df <- df[!duplicated(df),]
  
  df$spind_site_method <- paste0(df$sp_ind, df$site, df$method)
  
  ## Find the day of budburst to find the actual GDD versus the "observed GDD"
  for(i in c(unique(df$spind_site_method))){ # i="1_1arbws"
    
    bb <- which(df$gdd.obs[i==df$spind_site_method] >= df$fstarspp[i==df$spind_site_method])[1]
    df$bb[i==df$spind_site_method] <- bb
    
  }
  
  df.bb <- df[(df$bb==df$day),]
  
  df.bb <- subset(df.bb, select=c("site", "method", "species", "ind", "gdd.noise", "fstarspp", "gdd", "provenance", "bb")) # 
  
  bball <- df.bb[!duplicated(df.bb),]
  
  
  ##### Now let's do some checks...
  bball$gdd_accuracy <- bball$gdd - bball$fstarspp
  bball$type <- ifelse(bball$method=="ws", 1, 0)
  
  bball <- na.omit(bball)
  
  mylist <- list(bball, df, hfclim, arbclim)  
  
  return(mylist)
  
}

#bblist <- bbfunc(hypoth, hypoth.para, hypoth.mu, hypoth.sd, fstar.num, fstar.sd, arbclim, arbclim.sd, arbmicroclim, arbmicroclim.sd, hfclim, hfclim.sd, hfmicroclim, hfmicroclim.sd)

#bball <- bblist[[1]]
#df <- bblist[[2]]
#hfclim <- bblist[[3]]
#arbclim <- bblist[[4]]

if(FALSE){ ### This is to make the varying GDD plots... to move elsewhere later. 
  #bblist200 <- bbfunc(hypoth, question, hypoth.sd, fstar.num, urbeff, methodeff, urbmethod, arbclim, arbmicroclim, hfclim, hfmicroclim)
  #bblist250 <- bbfunc(hypoth, question, hypoth.sd, fstar.num, urbeff, methodeff, urbmethod, arbclim, arbmicroclim, hfclim, hfmicroclim)
  bblist300 <- bbfunc(hypoth, question, hypoth.sd, fstar.num, urbeff, methodeff, urbmethod, arbclim, arbmicroclim, hfclim, hfmicroclim)
  
  
  bb200 <- bblist200[[1]]
  bb250 <- bblist250[[1]]
  bb300 <- bblist300[[1]]
  
  bb200$gdd200 <- bb200$gdd
  bb200$gdd_accuracy200 <- bb200$gdd_accuracy
  bb200$gdd <- bb200$gdd_accuracy <- bb200$gdd.noise <- bb200$fstar.new <- bb200$bb <- NULL
  
  bb250$gdd250 <- bb250$gdd
  bb250$gdd_accuracy250 <- bb200$gdd_accuracy
  bb250$gdd <- bb250$gdd_accuracy <- bb250$gdd.noise <- bb250$fstar.new <- bb250$bb <- NULL
  
  bb <- full_join(bb200, bb250)
  
  bb300$gdd300 <- bb300$gdd
  bb300$gdd_accuracy300 <- bb300$gdd_accuracy
  bb300$gdd <- bb300$gdd_accuracy <- bb300$gdd.noise <- bb300$fstar.new <- bb300$bb <- NULL
  
  bb <- full_join(bb, bb300)
  
  
  ggplot(bb, aes(temp, gdd))
  
  
  
  
  
}

