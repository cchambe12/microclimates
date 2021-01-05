### Started 6 Oct 2020 by Cat
## Source function to build data for the shiny app
### Need to eventually integrate hypothesis tests and provenance vs urban!

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(dplyr)
library(tidyr)

set.seed(12321)

if(TRUE){
  hypoth = TRUE  ## if TRUE then hobos are less accurate, if FALSE then hobos are more precise
  question = TRUE ## if TRUE then testing urban effect, if FALSE then testing provenance
  hypoth.sd <- 10  ### This just adds that amount of imprecision to the hypothesis question
  fstar.num <- 300  ## GDD threshold
  urbeff <- -30   ### effect of arboretum
  methodeff <- -20  ### effect of weather station
  urbmethod <- -40  ### interaction
  arbclim <- 11   ### tmean arb climate
  arbmicroclim <- 1  ### tmean added to arb climate base, if positive then hobos are recording warmer temperatures
  hfclim <- 9  ## tmean hf climate
  hfmicroclim <- -1  ### tmean added to hf climate base, if positive then hobos are recording warmer temperatures
}


bbfunc <- function(hypoth, question, hypoth.sd, fstar.num, urbeff, methodeff, urbmethod, arbclim, arbmicroclim, hfclim, hfmicroclim){
  
  hypothA = hypoth  ### This is either TRUE or FALSE
  use.urban = question  ### This is either TRUE or FALSE
  
  # Step 1: Set up years, days per year, temperatures, sampling frequency, required GDD (fstar)
  daysperyr <- 100 #### just to make sure we don't get any NAs
  nspps <- 20 
  ninds <- 10 
  nobs <- nspps*ninds
  nsites <- 2  ### Arboretum versus the Forest
  nmicros <- 10  ### Number per site so 20 total 
  nmethods <- 2
  
  #urbeffect <- -20
  urbeffect <- urbeff ### mu_b_urban_sp      ### IF NEGATIVE, THEN THIS MEANS WE EXPECT THE ARBORETUM REQUIRES FEWER GDD! Maybe because chilling is higher?
  urbsd <- 5 ## sigma_b_urban_sp
  #methodeffect <- -25
  methodeffect <- methodeff ## mu_b_method_sp    ### IF NEGATIVE, THEN THIS MEANS WE EXPECT THE STATION MEASURES FEWER GDD! Maybe because it is cooler, thus accumulating GDD more slowly
  methodsd <- 5 ## sigma_b_method_sp 
  #urbmethod <- -70
  urbmethod <- urbmethod
  urbmethodsd <- 10
  
  
  fstar <- fstar.num  ### mu_a_sp
  fstarspeciessd <- 50 ### sigma_a_sp
  fstarindsd <- 20 ## sigma_y
  
  if(hypothA==TRUE){
    hobo_sd <- hypoth.sd
  } else if(hypothA==FALSE){
    ws_sd <- hypoth.sd
  }
  
  
  dayz <- rep(1:daysperyr, nobs)
  #cc.arb <- 11
  cc.arb <- arbclim ## based off weather station data
  #microarb.effect <- 0
  microarb.effect <- arbmicroclim
  sigma.arb <- 5 
  microsigmaarb.effect <- 0   #### by keeping the sigmas the same for the microsites (line 94 & 99) we assume that the microclimatic effects are the same across sites
  
  #cc.hf <- 9
  cc.hf <- hfclim ## based off weather station data
  #microhf.effect <- 0
  microhf.effect <- hfmicroclim
  sigma.hf <- 5  
  microsigmahf.effect <- 0  #### by keeping the sigmas the same for the microsites (line 94 & 99) we assume that the microclimatic effects are the same across sites
  
  
  
  ### Started 25 May 2020 by Cat
  ## Source file to build fake data simulations for urban versus provenance lat effects
  ## TAKE II: randomizing day of budburst and then calculate GDD from there
  
  
  library(dplyr)
  library(tidyr)
  
  set.seed(12321)
  
  if(use.urban==TRUE){
  #### Here, I set up provenance for each individual
  spind <- paste(rep(c(1:nspps), each=10), rep(1:ninds, 20), sep="_")
  provenance.hf <- 42.5
  provenance.arb <- round(rnorm(nobs, provenance.hf, 5), digits=2)
  
  df.prov <- as.data.frame(cbind(sp_ind = rep(rep(spind, nsites),each=nmethods), 
                                 site = rep(c("arb", "hf"), each=nobs*nmethods),
                                 provenance = c(rep(provenance.arb, each=nmethods), rep(provenance.hf, 400)),
                                 method = rep(c("ws", "hobo"), nsites*nobs)))
  df.prov$species <- as.numeric(gsub("\\_.*" , "", df.prov$sp_ind))
  
  
  #### Next I set up an fstar or a GDD threshold for each individual
  fstarspp <- round(rnorm(nspps, fstar, fstarspeciessd), digits=0)
  df.fstar <- as.data.frame(cbind(species=rep(1:nspps, each=ninds*nsites*nmethods), inds=rep(1:ninds, nmethods), 
                                  fstarspp=rep(fstarspp, each=ninds*nsites*nmethods),
                                  site=rep(c("arb", "hf"), each=ninds*nmethods),
                                  method=rep(rep(c("ws", "hobo"), each=ninds), nsites*nspps)))
  
  df.fstar$fstarspp <- as.numeric(df.fstar$fstarspp)
  df.fstar$sp_ind <- paste(df.fstar$species, df.fstar$inds, sep="_")
  
  
  ##### Lines 123-130 add in the stan model estimates and parameters. We should get these back.
  df.fstar$urbtx <- ifelse(df.fstar$site=="arb", 1, 0)
  df.fstar$gdd.noise  <- df.fstar$fstarspp + df.fstar$urbtx * rep(rnorm(n=nspps, mean=urbeffect, sd=urbsd), each=ninds*nmethods)  
  
  df.fstar$tx <- ifelse(df.fstar$method=="ws", 1, 0)
  df.fstar$gdd.noise <- df.fstar$gdd.noise + df.fstar$tx * rep(rnorm(n=nspps, mean=methodeffect, sd=methodsd), each=ninds*nmethods)
  
  df.fstar$urbmethodtx <- ifelse(df.fstar$method=="ws" & df.fstar$site=="arb", 1, 0)
  df.fstar$gdd.noise <- df.fstar$gdd.noise + df.fstar$urbmethodtx * rep(rnorm(n=nspps, mean=urbmethod, sd=urbmethodsd), each=ninds*nmethods)  
  
  if(hypothA==TRUE){ ### I think this should just make the hobo logger less accurate...??? I hope.
    
    df.fstar$hyp_a <- ifelse(df.fstar$method=="hobo", 1, 0)
    df.fstar$gdd.noise <- df.fstar$gdd.noise + df.fstar$hyp_a * rep(rnorm(n=nspps, mean=0, sd=hobo_sd), each=ninds*nmethods) 
  
  }
  
  if(hypothA==FALSE){ ### Whereas this will make the weather station less accurate/the hobo logger more precise
    
    df.fstar$hyp_b <- ifelse(df.fstar$method=="ws", 1, 0)
    df.fstar$gdd.noise <- df.fstar$fstarspp + df.fstar$hyp_b * rep(rnorm(nspps, mean=0, sd=ws_sd), each=ninds*nmethods)  
    
  }
  
  df.fstar$fstar.new <- rnorm(df.fstar$inds, df.fstar$gdd.noise, fstarindsd)
  
  df.fstar <- full_join(df.fstar, df.prov)
  }
  
  if(use.urban==FALSE){ ### This is the provenance model
    
    spind <- paste(rep(c(1:nspps), each=10), rep(1:ninds, 20), sep="_")
    provenance.hf <- 42.5
    provenance.arb <- round(rnorm(nobs, provenance.hf, 5), digits=2)
    
    df.prov <- as.data.frame(cbind(sp_ind = rep(rep(spind, nsites),each=nmethods), 
                                   site = rep(c("arb", "hf"), each=nobs*nmethods),
                                   provenance = c(rep(provenance.arb, each=nmethods), rep(provenance.hf, 400)),
                                   method = rep(c("ws", "hobo"), nsites*nobs)))
    df.prov$species <- as.character(as.numeric(gsub("\\_.*" , "", df.prov$sp_ind)))
    
    
    #### Next I set up an fstar or a GDD threshold for each individual
    fstarspp <- round(rnorm(nspps, fstar, fstarspeciessd), digits=0)
    df.fstar <- as.data.frame(cbind(species=rep(1:nspps, each=ninds*nsites*nmethods), inds=rep(1:ninds, nmethods), 
                                    fstarspp=rep(fstarspp, each=ninds*nsites*nmethods),
                                    site=rep(c("arb", "hf"), each=ninds*nmethods),
                                    method=rep(rep(c("ws", "hobo"), each=ninds), nsites*nspps)))
    
    df.fstar$fstarspp <- as.numeric(df.fstar$fstarspp)
    df.fstar$sp_ind <- paste(df.fstar$species, df.fstar$inds, sep="_")
    
    df.fstar <- full_join(df.fstar, df.prov)
    
    
    ##### Lines 123-130 add in the stan model estimates and parameters. We should get these back.
    df.fstar$urbtx <- ifelse(df.fstar$site=="arb", 1, 0)
    df.fstar$gdd.noise  <- df.fstar$fstarspp + df.fstar$urbtx * rep(rnorm(n=nspps, mean=urbeffect, sd=urbsd), each=ninds*nmethods)  
    
    df.fstar$tx <- ifelse(df.fstar$method=="ws", 1, 0)
    df.fstar$gdd.noise <- df.fstar$gdd.noise + df.fstar$tx * rep(rnorm(n=nspps, mean=methodeffect, sd=methodsd), each=ninds*nmethods)
    
    df.fstar$urbmethodtx <- ifelse(df.fstar$method=="ws" & df.fstar$site=="arb", 1, 0)
    df.fstar$gdd.noise <- df.fstar$gdd.noise + df.fstar$urbmethodtx * rep(rnorm(n=nspps, mean=urbmethod, sd=urbmethodsd), each=ninds*nmethods)  
    
    if(hypothA==TRUE){ ### I think this should just make the hobo logger less accurate... I hope! Should not come out of model directly
      
      df.fstar$hyp_a <- ifelse(df.fstar$method=="hobo", 1, 0)
      df.fstar$gdd.noise <- df.fstar$gdd.noise + df.fstar$hyp_a * rep(rnorm(n=nspps, mean=0, sd=hobo_sd), each=ninds*nmethods) 
      
    }
    
    if(hypothA==FALSE){ ### Whereas this will make the weather station less accurate/the hobo logger more precise
      
      df.fstar$hyp_b <- ifelse(df.fstar$method=="ws", 1, 0)
      df.fstar$gdd.noise <- df.fstar$fstarspp + df.fstar$hyp_b * rep(rnorm(nspps, mean=0, sd=ws_sd), each=ninds*nmethods)  
      
      
    }
    
    df.fstar$fstar.new <- rnorm(df.fstar$inds, df.fstar$gdd.noise, fstarindsd)
    
  }
  
  
  
  
  # Step 2: find GDDs
  #### Now I set up climate data for the Arboretum
  arbclim <- data.frame(microsite=rep(rep(c(1:nmicros), each=daysperyr*nmethods),nspps), ind=rep(rep(c(1:ninds), each=daysperyr*nmethods), nspps),
                        species = rep(c(1:nspps), each=daysperyr*nmicros*nmethods), 
                        #dayz=rep(arb.doybb$dayz, each=daysperyr),
                        day=rep(c(1:daysperyr), nmicros*nspps*nmethods),
                        method=rep(rep(c("ws", "hobo"), each=daysperyr), nspps*ninds),
                        site = as.character("arb"))
  
  
  ### This is how I get weather station data versus hobo logger data
  arbclim$tmean <- ifelse(arbclim$method=="hobo", rnorm(as.numeric(arbclim$day), cc.arb + microarb.effect, sigma.arb + microsigmaarb.effect), 
                          rnorm(as.numeric(arbclim$day), cc.arb, sigma.arb)) ### and now we draw from mean and sigma for each day to find daily temp for each microsite
  
  
  #### Harvard Forest climate data
  hfclim <- data.frame(microsite=rep(rep(c(1:nmicros), each=daysperyr*nmethods),nspps), ind=rep(rep(c(1:ninds), each=daysperyr*nmethods), nspps),
                       species = rep(c(1:nspps), each=daysperyr*nmicros*nmethods), 
                       #dayz=rep(hf.doybb$dayz, each=daysperyr),
                       day=rep(c(1:daysperyr), nmicros*nspps*nmethods),
                       method=rep(rep(c("ws", "hobo"), each=daysperyr),nspps*ninds),
                       site = as.character("hf"))
  
  
  ### Again, where I set up the difference between hobo logger and weather station
  hfclim$tmean <- ifelse(hfclim$method=="hobo", rnorm(hfclim$day, cc.hf + as.numeric(microhf.effect), sigma.hf + microsigmahf.effect), rnorm(hfclim$day, cc.hf, sigma.hf)) ### and now we draw from mean and sigma for each day to find daily temp for each microsite
  
  
  # Step 3: Make a data frame and get the mean temp per year (to double check the data)
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
  
  df.bb <- subset(df.bb, select=c("site", "method", "species", "ind", "bb", "gdd", "gdd.noise", "fstar.new", "provenance"))
  df.bb$species <- as.numeric(df.bb$species)
  
  bball <- df.bb[!duplicated(df.bb),]
  
  
  ##### Now let's do some checks...
  bball$gdd_accuracy <- bball$fstar.new - bball$gdd
  bball$type <- ifelse(bball$method=="ws", 1, 0)
  
  bball <- na.omit(bball)
  
  mylist <- list(bball, df)
  
  return(mylist)
  
}

bblist <- bbfunc(hypoth, question, hypoth.sd, fstar.num, urbeff, methodeff, urbmethod, arbclim, arbmicroclim, hfclim, hfmicroclim)

bball <- bblist[[1]]
df <- bblist[[2]]

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

