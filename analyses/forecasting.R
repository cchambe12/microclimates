## Started 11 April 2021 ##
## By Lizzie ##

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# load Libraries
library(dplyr)
library(tidyr)
library(ggplot2)

set.seed(12321)

#setwd("~/Documents/git/projects/others/cat/microclim_ms/gddaccuracy")
setwd("~/Documents/git/microclimates/analyses")


# Step 1: Set up years, days per year, temperatures, sampling frequency, required GDD (fstar)
daysperyr <- 500 #### just to make sure we don't get any NAs
nspps <- 500 

### Now the climate data

# Recommend run the below trying the full suite of 
# trying sigma.cc at 0.5, 1, 5 x basetemp at 0, 5, 10
fstarfunc <- function(sigma, basetemp){
    cc <- 12
    sigma.cc <- sigma
    basetemp <- basetemp
    
    fstar.min <- 100
    fstar.max <- 1000
    
    # fstars
    fstars <- seq(from=fstar.min, to=fstar.max, length.out=nspps)
    
    # observed climate
    tmeanbase <- rnorm(daysperyr, cc + 0, sigma.cc)
    tmeanbase0 <- ifelse(tmeanbase>=basetemp, tmeanbase-basetemp, 0)
    gdd <- ave(tmeanbase0, FUN=cumsum)
    
    # get GDD for each budburst DOY for each fstar
    observedgdd <- c()
    doy <- c()
    for(i in c(1:length(fstars))){
        doy[i] <- min(which(gdd >= fstars[i]))
        observedgdd[i] <- gdd[(min(which(gdd >= fstars[i])))]
        }
                   
    gddaccuracy <- observedgdd-fstars
    gddratio <- observedgdd/fstars
    
    gddstuff <- as.data.frame(cbind(doy, gddaccuracy, gddratio))
    
    mylist <- list(fstars, gddstuff)
    
    return(mylist)
    
}


sigma <- 0.1
basetemp <- 0
gddstuff <- fstarfunc(sigma,basetemp)[[2]]
fstars <- fstarfunc(sigma,basetemp)[[1]]
plotacc0s <- ggplot(gddstuff, aes(x=fstars, y=gddaccuracy)) +
    geom_point(aes(color=doy)) + ylab("GDD ratio \n(observed-expected)") + xlab("GDD Threshold") +
    labs(col="Day of year") +
    theme_minimal() + ggtitle("a) Base temperature of 0ºC, sigma 0.1") + coord_cartesian(ylim=c(0, 12))

plotratio0s <- ggplot(gddstuff, aes(x=fstars, y=gddratio)) +
    geom_point(aes(color=doy)) + ylab("GDD accuracy \n(observed/expected)") + xlab("GDD Threshold") +
    labs(col="Day of year") +
    theme_minimal() + ggtitle("a) Base temperature of 0ºC, sigma 0.1") + coord_cartesian(ylim=c(1, 1.1))


sigma <- 0.5
basetemp <- 0
gddstuff <- fstarfunc(sigma,basetemp)[[2]]
fstars <- fstarfunc(sigma,basetemp)[[1]]
plotacc0.5 <- ggplot(gddstuff, aes(x=fstars, y=gddaccuracy)) +
    geom_point(aes(color=doy)) + ylab("GDD ratio \n(observed-expected)") + xlab("GDD Threshold") +
    labs(col="Day of year") +
    theme_minimal() + ggtitle("b) Base temperature of 0ºC, sigma 0.5") + coord_cartesian(ylim=c(0, 12))

plotratio0.5 <- ggplot(gddstuff, aes(x=fstars, y=gddratio)) +
    geom_point(aes(color=doy)) + ylab("GDD accuracy \n(observed/expected)") + xlab("GDD Threshold") +
    labs(col="Day of year") +
    theme_minimal() + ggtitle("b) Base temperature of 0ºC, sigma 0.5") + coord_cartesian(ylim=c(1, 1.1))


sigma <- 0.1
basetemp <- 10
gddstuff <- fstarfunc(sigma,basetemp)[[2]]
fstars <- fstarfunc(sigma,basetemp)[[1]]
plotacc10 <- ggplot(gddstuff, aes(x=fstars, y=gddaccuracy)) +
    geom_point(aes(color=doy)) + ylab("GDD ratio \n(observed-expected)") + xlab("GDD Threshold") +
    labs(col="Day of year") +
    theme_minimal() + ggtitle("c) Base temperature of 10ºC, sigma 0.1") + coord_cartesian(ylim=c(0, 12))

plotratio10 <- ggplot(gddstuff, aes(x=fstars, y=gddratio)) +
    geom_point(aes(color=doy)) + ylab("GDD accuracy \n(observed/expected)") + xlab("GDD Threshold") +
    labs(col="Day of year") +
    theme_minimal() + ggtitle("c) Base temperature of 10ºC, sigma 0.1") + coord_cartesian(ylim=c(1, 1.1))


sigma <- 0.5
basetemp <- 10
gddstuff <- fstarfunc(sigma,basetemp)[[2]]
fstars <- fstarfunc(sigma,basetemp)[[1]]
plotacc10.5 <- ggplot(gddstuff, aes(x=fstars, y=gddaccuracy)) +
    geom_point(aes(color=doy)) + ylab("GDD ratio \n(observed-expected)") + xlab("GDD Threshold") +
    labs(col="Day of year") +
    theme_minimal() + ggtitle("d) Base temperature of 10ºC, sigma 0.5") + coord_cartesian(ylim=c(0, 12))

plotratio10.5 <- ggplot(gddstuff, aes(x=fstars, y=gddratio)) +
    geom_point(aes(color=doy)) + ylab("GDD accuracy \n(observed/expected)") + xlab("GDD Threshold") +
    labs(col="Day of year") +
    theme_minimal() + ggtitle("d) Base temperature of 10ºC, sigma 0.5") + coord_cartesian(ylim=c(1, 1.1))

library(egg)
plotacc <- ggarrange(plotacc0s, plotacc0.5, plotacc10, plotacc10.5, ncol=2, nrow=2)
plotratio <- ggarrange(plotratio0s, plotratio0.5, plotratio10, plotratio10.5, ncol=2, nrow=2)

pdf(file.path("figures/gddaccuracy_fstars.pdf"), width = 9, height = 6)
plotacc
dev.off()
pdf(file.path("figures/gddratio_fstars.pdf"), width = 9, height = 6)
plotratio
dev.off()

# Generally higher fstar means lower gddratio; this is because being off by a day is a small effect for higher fstar (and hence greater days) than for lower fstar, but it really depends on climate variability because high variability means some days you can get a big chunk of GDD and that can override the fstar trends. In reality temperature variance likely changes over the spring (or could?) so this is pretty tricky! 


###################################################################################################################################################
###################################################################################################################################################
###################################################################################################################################################
###################################################################################################################################################

# Recommend run the below trying the full suite of 
# trying sigma.cc at 0.5, 1, 5 x basetemp at 0, 5, 10
warmfunc <- function(sigma, basetemp){
    cc <- 12
    sigma.cc <- sigma
    basetemp <- basetemp
    
    warm.min <- 0
    warm.max <- 9
    
    fstars <- c(50, 250, 500, 750, 1000)
    
    # fstars
    warms <- seq(from=warm.min, to=warm.max, by=1)
    
    # observed climate
    tmeanbase <- c()
    tmean <- c()
    for(i in c(unique(warms))){
      tmeanbase <- rnorm(daysperyr, cc + i, sigma.cc) ## i=0
      tmean <- c(tmean, tmeanbase)
    }
    tmeangdd <- ifelse(tmean>=basetemp, tmean-basetemp, 0)
    warming <- rep(warms, each=(daysperyr))
    tmeanwarm <- data.frame(cbind(tmeangdd, warming))
    tmeanwarm$gdd <- ave(tmeanwarm$tmeangdd, tmeanwarm$warming, FUN=cumsum)
    tmeanwarm <- tmeanwarm[rep(seq_len(nrow(tmeanwarm)), times = length(fstars)), ]
    tmeanwarm$fstars <- rep(fstars, each = daysperyr * length(warms))
    
    # get GDD for each budburst DOY for each fstar
    tmeanwarm$observedgdd <- NA
    tmeanwarm$doy <- NA
    tmeanwarm$spwarm <- paste0(tmeanwarm$warming, tmeanwarm$fstars)
    for(i in c(unique(tmeanwarm$spwarm))){ #i=0
        tmeanwarm$doy[tmeanwarm$spwarm==i] <- min(which(tmeanwarm$gdd[tmeanwarm$spwarm==i] >= tmeanwarm$fstars[tmeanwarm$spwarm==i]))
        tmeanwarm$observedgdd[tmeanwarm$spwarm==i] <- tmeanwarm$gdd[(min(which(tmeanwarm$gdd[tmeanwarm$spwarm==i] >= tmeanwarm$fstars[tmeanwarm$spwarm==i])))]
      }
    
    tmeanwarm$gddaccuracy <- tmeanwarm$observedgdd - tmeanwarm$fstars
    tmeanwarm$gddratio <- tmeanwarm$observedgdd/tmeanwarm$fstars
    
    gddstuff <- subset(tmeanwarm, select=c("doy", "gddaccuracy", "gddratio", "warming", "fstars"))
    gddstuff <- gddstuff[!duplicated(gddstuff),]
    
    mylist <- list(fstars, gddstuff)
    
    return(mylist)
    
}


sigma <- 0.1
basetemp <- 0
gddstuff <- warmfunc(sigma,basetemp)[[2]]
#fstars <- warmfunc(sigma,basetemp)[[1]]
plotacc0s <- ggplot(gddstuff, aes(x=warming, y=gddaccuracy)) +
    geom_point(aes(color=doy)) + ylab("GDD ratio \n(observed-expected)") + xlab("Warming") +
    geom_line(aes(color=fstars, group=fstars)) +
    labs(col="Day of year") + coord_cartesian(ylim=c(-750, 0)) +
    theme_minimal() + ggtitle("a) Base temperature of 0ºC, sigma 0.1") 

plotratio0s <- ggplot(gddstuff, aes(x=warming, y=gddratio)) +
    geom_jitter(aes(color=fstars)) + ylab("GDD accuracy \n(observed/expected)") + xlab("Warming") +
    geom_line(aes(color=fstars, group=fstars)) +
    labs(col="GDD threshold") + coord_cartesian(ylim=c(0, 1.2)) +
    theme_minimal() + ggtitle("a) Base temperature of 0ºC, sigma 0.1") 


sigma <- 1
basetemp <- 0
gddstuff <- warmfunc(sigma,basetemp)[[2]]
#fstars <- warmfunc(sigma,basetemp)[[1]]
plotacc0.5 <- ggplot(gddstuff, aes(x=warming, y=gddaccuracy)) +
    geom_point(aes(color=doy)) + ylab("GDD ratio \n(observed-expected)") + xlab("Warming") +
  geom_line(aes(color=fstars, group=fstars)) +
    labs(col="Day of year") + coord_cartesian(ylim=c(-750, 0)) +
    theme_minimal() + ggtitle("b) Base temperature of 0ºC, sigma 1") 

plotratio0.5 <- ggplot(gddstuff, aes(x=warming, y=gddratio)) +
    geom_jitter(aes(color=doy)) + ylab("GDD accuracy \n(observed/expected)") + xlab("Warming") +
  geom_line(aes(color=fstars, group=fstars)) +
    labs(col="Day of year") + coord_cartesian(ylim=c(0, 1.2)) +
    theme_minimal() + ggtitle("b) Base temperature of 0ºC, sigma 1")


sigma <- 0.1
basetemp <- 10
gddstuff <- warmfunc(sigma,basetemp)[[2]]
#fstars <- warmfunc(sigma,basetemp)[[1]]
plotacc10 <- ggplot(gddstuff, aes(x=warming, y=gddaccuracy)) +
    geom_point(aes(color=doy)) + ylab("GDD ratio \n(observed-expected)") + xlab("Warming") +
  geom_line(aes(color=fstars, group=fstars)) +
    labs(col="Day of year") + coord_cartesian(ylim=c(-750, 0)) +
    theme_minimal() + ggtitle("c) Base temperature of 10ºC, sigma 0.1")

plotratio10 <- ggplot(gddstuff, aes(x=warming, y=gddratio)) +
    geom_jitter(aes(color=doy)) + ylab("GDD accuracy \n(observed/expected)") + xlab("Warming") +
  geom_line(aes(color=fstars, group=fstars)) +
    labs(col="Day of year") + coord_cartesian(ylim=c(0, 1.2)) +
    theme_minimal() + ggtitle("c) Base temperature of 10ºC, sigma 0.1")


sigma <- 1
basetemp <- 10
gddstuff <- warmfunc(sigma,basetemp)[[2]]
#fstars <- warmfunc(sigma,basetemp)[[1]]
plotacc10.5 <- ggplot(gddstuff, aes(x=warming, y=gddaccuracy)) +
    geom_point(aes(color=doy)) + ylab("GDD ratio \n(observed-expected)") + xlab("Warming") +
  geom_line(aes(color=fstars, group=fstars)) +
    labs(col="Day of year") + coord_cartesian(ylim=c(-750, 0)) +
    theme_minimal() + ggtitle("d) Base temperature of 10ºC, sigma 1") 

plotratio10.5 <- ggplot(gddstuff, aes(x=warming, y=gddratio)) +
    geom_jitter(aes(color=doy)) + ylab("GDD accuracy \n(observed/expected)") + xlab("Warming") +
  geom_line(aes(color=fstars, group=fstars)) +
    labs(col="Day of year") + coord_cartesian(ylim=c(0, 1.2)) +
    theme_minimal() + ggtitle("d) Base temperature of 10ºC, sigma 1") 

library(egg)
plotacc <- ggarrange(plotacc0s, plotacc0.5, plotacc10, plotacc10.5, ncol=2, nrow=2)
plotratio <- ggarrange(plotratio0s, plotratio0.5, plotratio10, plotratio10.5, ncol=2, nrow=2)

pdf(file.path("figures/gddaccuracy_warming.pdf"), width = 9, height = 6)
plotacc
dev.off()
pdf(file.path("figures/gddratio_warming.pdf"), width = 9, height = 6)
plotratio
dev.off()

# Generally higher fstar means lower gddratio; this is because being off by a day is a small effect for higher fstar (and hence greater days) than for lower fstar, but it really depends on climate variability because high variability means some days you can get a big chunk of GDD and that can override the fstar trends. In reality temperature variance likely changes over the spring (or could?) so this is pretty tricky! 








