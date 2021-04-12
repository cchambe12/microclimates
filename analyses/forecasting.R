## Started 11 April 2021 ##
## By Lizzie ##

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# load Libraries
library(dplyr)
library(tidyr)
library(ggplot2)

#setwd("~/Documents/git/projects/others/cat/microclim_ms/gddaccuracy")
setwd("~/Documents/git/microclimates/analyses")


# Step 1: Set up years, days per year, temperatures, sampling frequency, required GDD (fstar)
daysperyr <- 500 #### just to make sure we don't get any NAs
nspps <- 500 

### Now the climate data

# Recommend run the below trying the full suite of 
# trying sigma.cc at 0.5, 1, 5 x basetemp at 0, 5, 10
warmfunc <- function(sigma, basetemp){
    cc <- 10
    sigma.cc <- sigma
    warmcc <- 2
    basetemp <- basetemp
    
    fstar.min <- 100
    fstar.max <- 1000
    
    # fstars
    fstars <- seq(from=fstar.min, to=fstar.max, length.out=nspps)
    
    # observed climate
    tmeanbase <- rnorm(daysperyr, cc + 0, sigma.cc)
    tmeanbase0 <- ifelse(tmeanbase>=basetemp, tmeanbase, 0)
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


sigma <- 0
basetemp <- 0
gddstuff <- warmfunc(sigma,basetemp)[[2]]
fstars <- warmfunc(sigma,basetemp)[[1]]
plotacc0s <- ggplot(gddstuff, aes(x=fstars, y=gddaccuracy)) +
    geom_point(aes(color=doy)) + xlab("GDD ratio \n(observed/expected)") + ylab("GDD Threshold") +
    labs(col="Day of year") +
    theme_minimal() + ggtitle("a)")

plotratio0s <- ggplot(gddstuff, aes(x=fstars, y=gddratio)) +
    geom_point(aes(color=doy)) + xlab("GDD accuracy \n(observed-expected)") + ylab("GDD Threshold") +
    labs(col="Day of year") +
    theme_minimal() + ggtitle("a)")


sigma <- 0.5
basetemp <- 0
gddstuff <- warmfunc(sigma,basetemp)[[2]]
fstars <- warmfunc(sigma,basetemp)[[1]]
plotacc0.5 <- ggplot(gddstuff, aes(x=fstars, y=gddaccuracy)) +
    geom_point(aes(color=doy)) + xlab("GDD ratio \n(observed/expected)") + ylab("GDD Threshold") +
    labs(col="Day of year") +
    theme_minimal() + ggtitle("b)")

plotratio0.5 <- ggplot(gddstuff, aes(x=fstars, y=gddratio)) +
    geom_point(aes(color=doy)) + xlab("GDD accuracy \n(observed-expected)") + ylab("GDD Threshold") +
    labs(col="Day of year") +
    theme_minimal() + ggtitle("b)")


sigma <- 0
basetemp <- 10
gddstuff <- warmfunc(sigma,basetemp)[[2]]
fstars <- warmfunc(sigma,basetemp)[[1]]
plotacc10 <- ggplot(gddstuff, aes(x=fstars, y=gddaccuracy)) +
    geom_point(aes(color=doy)) + xlab("GDD ratio \n(observed/expected)") + ylab("GDD Threshold") +
    labs(col="Day of year") +
    theme_minimal() + ggtitle("c)")

plotratio10 <- ggplot(gddstuff, aes(x=fstars, y=gddratio)) +
    geom_point(aes(color=doy)) + xlab("GDD accuracy \n(observed-expected)") + ylab("GDD Threshold") +
    labs(col="Day of year") +
    theme_minimal() + ggtitle("c)")


sigma <- 0.5
basetemp <- 10
gddstuff <- warmfunc(sigma,basetemp)[[2]]
fstars <- warmfunc(sigma,basetemp)[[1]]
plotacc10.5 <- ggplot(gddstuff, aes(x=fstars, y=gddaccuracy)) +
    geom_point(aes(color=doy)) + xlab("GDD ratio \n(observed/expected)") + ylab("GDD Threshold") +
    labs(col="Day of year") +
    theme_minimal() + ggtitle("d)")

plotratio10.5 <- ggplot(gddstuff, aes(x=fstars, y=gddratio)) +
    geom_point(aes(color=doy)) + xlab("GDD accuracy \n(observed-expected)") + ylab("GDD Threshold") +
    labs(col="Day of year") +
    theme_minimal() + ggtitle("d)")

library(egg)
plotacc <- ggarrange(plotacc0s, plotacc0.5, plotacc10, plotacc10.5, ncol=2, nrow=2)
plotratio <- ggarrange(plotratio0s, plotratio0.5, plotratio10, plotratio10.5, ncol=2, nrow=2)

pdf(file.path("figures/gddaccuracy_basetemp.pdf"), width = 9, height = 6)
plotacc
dev.off()
pdf(file.path("figures/gddratio_basetemp.pdf"), width = 9, height = 6)
plotratio
dev.off()

# Generally higher fstar means lower gddratio; this is because being off by a day is a small effect for higher fstar (and hence greater days) than for lower fstar, but it really depends on climate variability because high variability means some days you can get a big chunk of GDD and that can override the fstar trends. In reality temperature variance likely changes over the spring (or could?) so this is pretty tricky! 
