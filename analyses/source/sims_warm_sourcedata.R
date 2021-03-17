### Started 6 Oct 2020 by Cat
## Source function to build data for the shiny app
### Need to eventually integrate hypothesis tests and provenance vs urban!

#Load Libraries
library(dplyr)
library(tidyr)

if(FALSE){
  fstar.min <- 100  ## GDD threshold
  fstar.max <- 300
  warmmax <- 10
  meantemp <- 10
  basetemp <- 10
}


warmfunc <- function(fstar.min, fstar.max, warmmax, meantemp, basetemp){
  
  # Step 1: Set up years, days per year, temperatures, sampling frequency, required GDD (fstar)
  daysperyr <- 80 #### just to make sure we don't get any NAs
  nspps <- 12 
  ninds <- 10 
  nobs <- nspps*ninds
  
  ### Now the climate data 
  dayz <- rep(1:daysperyr, nobs)
  cc <- meantemp
  sigma.cc <- 2
  warmcc <- warmmax
  
  
  #### Next I set up an fstar or a GDD threshold for each individual

  df.fstar <- as.data.frame(cbind(species=rep((1:nspps), each=ninds), 
                                  fstarspp=rep(10*round(seq(fstar.min, fstar.max, 
                                                        by=(fstar.max-fstar.min)/(nspps-1))/10, digits=0), each=ninds))) ## Rather than randomizing the fstar thresholds, I want to make it consistent intervals.
  
  df.fstar$fstarspp <- as.numeric(df.fstar$fstarspp)
  
  #### Here, I set up provenance for each individual
  ### # Step 2: find GDDs
  #### Now I set up climate data for the Arboretum, this is the weather station data
  tmeanbase <- rnorm(daysperyr*ninds, cc + 0, sigma.cc)
  
  clim <- data.frame(day=rep(1:daysperyr, times=ninds),
                     fstarspp=rep(df.fstar$fstarspp, each=daysperyr),
                     ind=rep(1:ninds, each=daysperyr),
                     tmean0=rep(tmeanbase, times=nspps))
  
  clim$tmean1 <- clim$tmean0 + 1 
  clim$tmean2 <- clim$tmean0 + 2
  clim$tmean3 <- clim$tmean0 + 3
  clim$tmean4 <- clim$tmean0 + 4
  clim$tmean5 <- clim$tmean0 + 5
  clim$tmean6 <- clim$tmean0 + 6
  clim$tmean7 <- clim$tmean0 + 7
  clim$tmean8 <- clim$tmean0 + 8
  clim$tmean9 <- clim$tmean0 + 9
  clim$tmean10 <- clim$tmean0 + 10
  
  ##Step 3: Make a data frame and get the mean temp
  df <- clim
  df$tmean0 <- as.numeric(df$tmean0)
  df$tmean1 <- as.numeric(df$tmean1)
  df$tmean2 <- as.numeric(df$tmean2)
  df$tmean3 <- as.numeric(df$tmean3)
  df$tmean4 <- as.numeric(df$tmean4)
  df$tmean5 <- as.numeric(df$tmean5)
  df$tmean6 <- as.numeric(df$tmean6)
  df$tmean7 <- as.numeric(df$tmean7)
  df$tmean8 <- as.numeric(df$tmean8)
  df$tmean9 <- as.numeric(df$tmean9)
  df$tmean10 <- as.numeric(df$tmean10)
  
  df$tmean0 <- ifelse(df$tmean0>=basetemp, df$tmean0, 0)
  df$tmean1 <- ifelse(df$tmean1>=basetemp, df$tmean1, 0)
  df$tmean2 <- ifelse(df$tmean2>=basetemp, df$tmean2, 0)
  df$tmean3 <- ifelse(df$tmean3>=basetemp, df$tmean3, 0)
  df$tmean4 <- ifelse(df$tmean4>=basetemp, df$tmean4, 0)
  df$tmean5 <- ifelse(df$tmean5>=basetemp, df$tmean5, 0)
  df$tmean6 <- ifelse(df$tmean6>=basetemp, df$tmean6, 0)
  df$tmean7 <- ifelse(df$tmean7>=basetemp, df$tmean7, 0)
  df$tmean8 <- ifelse(df$tmean8>=basetemp, df$tmean8, 0)
  df$tmean9 <- ifelse(df$tmean9>=basetemp, df$tmean9, 0)
  df$tmean10 <- ifelse(df$tmean10>=basetemp, df$tmean10, 0)
  
  ### Calculate the OBSERVED GDDs!!!
  df$gdd.obs0 <- ave(df$tmean0, df$fstarspp, df$ind, FUN=cumsum)
  df$gdd.obs1 <- ave(df$tmean1, df$fstarspp, df$ind, FUN=cumsum)
  df$gdd.obs2 <- ave(df$tmean2, df$fstarspp, df$ind, FUN=cumsum)
  df$gdd.obs3 <- ave(df$tmean3, df$fstarspp, df$ind, FUN=cumsum)
  df$gdd.obs4 <- ave(df$tmean4, df$fstarspp, df$ind, FUN=cumsum)
  df$gdd.obs5 <- ave(df$tmean5, df$fstarspp, df$ind, FUN=cumsum)
  df$gdd.obs6 <- ave(df$tmean6, df$fstarspp, df$ind, FUN=cumsum)
  df$gdd.obs7 <- ave(df$tmean7, df$fstarspp, df$ind, FUN=cumsum)
  df$gdd.obs8 <- ave(df$tmean8, df$fstarspp, df$ind, FUN=cumsum)
  df$gdd.obs9 <- ave(df$tmean9, df$fstarspp, df$ind, FUN=cumsum)
  df$gdd.obs10 <- ave(df$tmean10, df$fstarspp, df$ind, FUN=cumsum)
  
  df$sp_ind <- paste(df$fstarspp, df$ind)
  
  ## Find the day of budburst to find the actual GDD versus the "observed GDD"
  for(i in c(unique(df$sp_ind))){ # i="1_1" i=1
    
    bb0 <- which(df$gdd.obs0[i==df$sp_ind] >= df$fstarspp[i==df$sp_ind])[1]
    df$bb0[i==df$sp_ind] <- bb0
    bb1 <- which(df$gdd.obs1[i==df$sp_ind] >= df$fstarspp[i==df$sp_ind])[1]
    df$bb1[i==df$sp_ind] <- bb1
    bb2 <- which(df$gdd.obs2[i==df$sp_ind] >= df$fstarspp[i==df$sp_ind])[1]
    df$bb2[i==df$sp_ind] <- bb2
    bb3 <- which(df$gdd.obs3[i==df$sp_ind] >= df$fstarspp[i==df$sp_ind])[1]
    df$bb3[i==df$sp_ind] <- bb3
    bb4 <- which(df$gdd.obs4[i==df$sp_ind] >= df$fstarspp[i==df$sp_ind])[1]
    df$bb4[i==df$sp_ind] <- bb4
    bb5 <- which(df$gdd.obs5[i==df$sp_ind] >= df$fstarspp[i==df$sp_ind])[1]
    df$bb5[i==df$sp_ind] <- bb5
    bb6 <- which(df$gdd.obs6[i==df$sp_ind] >= df$fstarspp[i==df$sp_ind])[1]
    df$bb6[i==df$sp_ind] <- bb6
    bb7 <- which(df$gdd.obs7[i==df$sp_ind] >= df$fstarspp[i==df$sp_ind])[1]
    df$bb7[i==df$sp_ind] <- bb7
    bb8 <- which(df$gdd.obs8[i==df$sp_ind] >= df$fstarspp[i==df$sp_ind])[1]
    df$bb8[i==df$sp_ind] <- bb8
    bb9 <- which(df$gdd.obs9[i==df$sp_ind] >= df$fstarspp[i==df$sp_ind])[1]
    df$bb9[i==df$sp_ind] <- bb9
    bb10 <- which(df$gdd.obs10[i==df$sp_ind] >= df$fstarspp[i==df$sp_ind])[1]
    df$bb10[i==df$sp_ind] <- bb10
    
  }
  
  #df <- df
  
  df.bb <- df[(df$bb0==df$day | df$bb1==df$day | df$bb2==df$day | df$bb3==df$day | df$bb4==df$day | df$bb5==df$day |
                 df$bb6==df$day | df$bb7==df$day | df$bb8==df$day | df$bb9==df$day | df$bb10==df$day),]
  
  df.bb$gdd.obs0 <- ifelse(df.bb$day==df.bb$bb0, df.bb$gdd.obs0, NA)
  df.bb$gdd.obs1 <- ifelse(df.bb$day==df.bb$bb1, df.bb$gdd.obs1, NA)
  df.bb$gdd.obs2 <- ifelse(df.bb$day==df.bb$bb2, df.bb$gdd.obs2, NA)
  df.bb$gdd.obs3 <- ifelse(df.bb$day==df.bb$bb3, df.bb$gdd.obs3, NA)
  df.bb$gdd.obs4 <- ifelse(df.bb$day==df.bb$bb4, df.bb$gdd.obs4, NA)
  df.bb$gdd.obs5 <- ifelse(df.bb$day==df.bb$bb5, df.bb$gdd.obs5, NA)
  df.bb$gdd.obs6 <- ifelse(df.bb$day==df.bb$bb6, df.bb$gdd.obs6, NA)
  df.bb$gdd.obs7 <- ifelse(df.bb$day==df.bb$bb7, df.bb$gdd.obs7, NA)
  df.bb$gdd.obs8 <- ifelse(df.bb$day==df.bb$bb8, df.bb$gdd.obs8, NA)
  df.bb$gdd.obs9 <- ifelse(df.bb$day==df.bb$bb9, df.bb$gdd.obs9, NA)
  df.bb$gdd.obs10 <- ifelse(df.bb$day==df.bb$bb10, df.bb$gdd.obs10, NA)
  
  df$gdd.obs0 <- round(ave(df$gdd.obs0), digits=0)
  df$gdd.obs1 <- round(ave(df$gdd.obs1), digits=0)
  df$gdd.obs2 <- round(ave(df$gdd.obs2), digits=0)
  df$gdd.obs3 <- round(ave(df$gdd.obs3), digits=0)
  df$gdd.obs4 <- round(ave(df$gdd.obs4), digits=0)
  df$gdd.obs5 <- round(ave(df$gdd.obs6), digits=0)
  df$gdd.obs6 <- round(ave(df$gdd.obs5), digits=0)
  df$gdd.obs7 <- round(ave(df$gdd.obs7), digits=0)
  df$gdd.obs8 <- round(ave(df$gdd.obs8), digits=0)
  df$gdd.obs9 <- round(ave(df$gdd.obs9), digits=0)
  df$gdd.obs10 <- round(ave(df$gdd.obs10), digits=0)
  
  
  df.bb <- subset(df.bb, select=c("fstarspp", "gdd.obs0",
                                  "gdd.obs1", "gdd.obs2", "gdd.obs3", "gdd.obs4", "gdd.obs5",
                                  "gdd.obs6", "gdd.obs7", "gdd.obs8", "gdd.obs9", "gdd.obs10")) 
  bball <- df.bb %>% tidyr::gather(warming, gdd, gdd.obs0:gdd.obs10, -fstarspp)
  bball$warming <- as.numeric(substr(bball$warming, 8, 9))
  
  bball <- bball[!is.na(bball$gdd),]
  
  
  ##### Now let's do some checks...
  bball$gdd_accuracy <- bball$gdd - bball$fstarspp
  bball$gdd_ratio <- bball$gdd/bball$fstarspp
  
  bball <- bball[(bball$warming<=warmmax),]
  
  bball <- na.omit(bball)
  
  mylist <- list(bball, df)  
  
  return(mylist)
  
}

if(FALSE){ ### This is to make the varying GDD plots... to move elsewhere later. 
  bblist300 <- warmfunc(fstar.num, fstar.sd, warming)
  
  bb300 <- bblist300[[1]]
  library(ggplot2)
  library(viridis)
  
  quartz()
  ggplot(bball, aes(as.numeric(warming), abs(gdd_accuracy), col=fstarspp, group=fstarspp)) +
    #geom_ribbon(stat='smooth', method = "lm", se=TRUE, alpha=1, 
    #           aes(fill = as.factor(fstarspp), group = as.factor(fstarspp))) +
    geom_line(stat='smooth', method = "loess", alpha=1, aes(col=fstarspp)) +
    theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
          legend.text.align = 0,
          legend.key = element_rect(colour = "transparent", fill = "white"),
          plot.margin = margin(0.5, 0.5, 0.5, 1, "cm")) + 
    xlab("Amount of warming") + ylab("GDD accuracy") +
    scale_x_continuous(breaks = sort(c(seq(0, 10, by=1)))) +
    scale_color_continuous(type="viridis")
  
  
  
  
  
}


