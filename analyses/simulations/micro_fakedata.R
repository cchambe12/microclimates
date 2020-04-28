### Started 23 April 2020 by Cat
## Building new dataframe with fake data to try and better understand hobo logger data versus microclimate data

# Maybe I should use estimates for fstar from real models?

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#### Questions to address:
# GDDlo ~ urban + (urban|species) 

## Let's start with Question 1 first...
#library(rethinking)
library(RColorBrewer)
library(lme4)
library(ggplot2)
library(dplyr)

## Let's load some real data to check out.
setwd("~/Documents/git/microclimates/analyses/")

set.seed(12321)

ws <- read.csv("output/clean_gdd_chill_bbanddvr.csv")
mean(ws$gdd_bb, na.rm=TRUE) ## 292
sd(ws$gdd_bb, na.rm = TRUE) ## 116

cc <- read.csv("output/")

# Step 1: Set up years, days per year, temperatures, sampling frequency, required GDD (fstar)
daysperyr <- 80 
nspps <- 12 #12
ninds <- 10 
nobs <- nspps*ninds
nmicros <- 10

dayz <- rep(1:daysperyr, nobs)
cc.arb <- 11 ## based off weather station data
sigma.arb <- 10 ## based off weather station data

cc.hf <- 9  ## based off weather station data
sigma.hf <- 11  ## based off weather station data

provenance.arb <- round(rnorm(nobs, 42.5, 10), digits=2)
provenance.hf <- 42.5

fstar <- round(rnorm(nspps, 300, 75), digits=0)
df.fstar <- as.data.frame(cbind(species=1:nspps, fstar, bb=rep("Y", nspps)))
df.fstar$fstar <- as.numeric(df.fstar$fstar)

# Step 2: find GDDs
dailytemp.arb <- rnorm(daysperyr*nobs, cc.arb, sigma.arb) 
dailytemp.hf <- rnorm(daysperyr*nobs, cc.hf, sigma.hf) 

# Step 3: Make a data frame and get the mean temp per year (to double check the data)
df.arb <- data.frame(cbind(doy=dayz, tmean=round(dailytemp.arb, digits=2), 
                       species=as.character(rep(1:nspps, each=daysperyr)),
                       ind=as.character(rep(1:ninds, each=daysperyr*nspps)),
                       site="arb",
                       provenance = rep(provenance.arb, each=daysperyr)))

df.hf <- data.frame(cbind(doy=dayz, tmean=round(dailytemp.hf, digits=2), 
                           species=as.character(rep(1:nspps, each=daysperyr)),
                           ind=as.character(rep(1:ninds, each=daysperyr*nspps)),
                           site="hf",
                          provenance = provenance.hf))

df <- full_join(df.arb, df.hf)
df$tmean <- as.numeric(df$tmean)

df$microsite <- paste0(df$site, df$ind)
df$tmean.ws <- ave(df$tmean, df$doy, df$site)


df$sp_ind <- paste(df$species, df$ind, sep="_")

df$gdd.ws <- ave(df$tmean.ws, df$sp_ind, df$site, FUN=cumsum)
df$gdd.hl <- ave(df$tmean, df$sp_ind, df$microsite, FUN=cumsum)


#### Now we find budburst day...
# Step 4: Now, in a very slow, painful way I get the BB date
df$bb.ws <- NA

for(i in c(1:nrow(df))){ # This loop just makes a Yes/No vector for budburst
  for(j in c(1:nrow(df.fstar))) 
    if(df$species[i]==df.fstar$species[j] && df$gdd.ws[i]<df.fstar$fstar[j]){
      df$bb.ws[i] <- "N"
    } else if (df$species[i]==df.fstar$species[j] && df$gdd.ws[i]>=df.fstar$fstar[j]) {
      df$bb.ws[i] <- "Y"
    } else {
      df$bb.ws[i] <- df$bb.ws[i]
    }
}


df$bb.hl <- NA

for(i in c(1:nrow(df))){ # This loop just makes a Yes/No vector for budburst
  for(j in c(1:nrow(df.fstar))) 
    if(df$species[i]==df.fstar$species[j] && df$gdd.hl[i]<df.fstar$fstar[j]){
      df$bb.hl[i] <- "N"
    } else if (df$species[i]==df.fstar$species[j] && df$gdd.hl[i]>=df.fstar$fstar[j]) {
      df$bb.hl[i] <- "Y"
    } else {
      df$bb.hl[i] <- df$bb.hl[i]
    }
}

df$doy <- as.numeric(df$doy)

bbws <- df[(df$bb.ws=="Y"),]
bbws <- subset(bbws, select=c("species", "ind", "sp_ind", "site", "microsite", "doy", "gdd.ws", "bb.ws"))
bbws$bbws.doy <- ave(bbws$doy, bbws$sp_ind, bbws$site, FUN=min)
bbws$bbws.gdd <- ave(bbws$gdd.ws, bbws$sp_ind, bbws$site, FUN=min)
bbws$doy <- NULL
bbws$gdd.ws <- NULL
bbws$bb.ws <- NULL
bbws <- bbws[!duplicated(bbws),]

bbhl <- df[(df$bb.hl=="Y"),]
bbhl <- subset(bbhl, select=c("species", "ind", "sp_ind", "site", "microsite", "doy", "gdd.hl", "bb.hl"))
bbhl$bbhl.doy <- ave(bbhl$doy, bbhl$sp_ind, bbhl$microsite, FUN=min)
bbhl$bbhl.gdd <- ave(bbhl$gdd.hl, bbhl$sp_ind, bbhl$microsite, FUN=min)
bbhl$doy <- NULL
bbhl$gdd.hl <- NULL
bbhl$bb.hl <- NULL
bbhl <- bbhl[!duplicated(bbhl),]

bball <- full_join(bbws, bbhl)



quartz()
par(mfrow=c(1,2))
my.pal <- rep(brewer.pal(n = 8, name = "Dark2"), 2)
my.pch <- rep(15:16, each=10)
plot(bbws.gdd ~ species, col=my.pal[as.factor(bball$species)], pch=my.pch[as.factor(bball$species)], data = bball, main="Weather Station",
     ylab="GDD")
abline(h=mean(bball$bbws.gdd), lwd=3)

plot(bbhl.gdd ~ species, col=my.pal[as.factor(bball$species)], pch=my.pch[as.factor(bball$species)], data = bball, main="Hobo Logger",
     ylab="GDD")
abline(h=mean(bball$bbhl.gdd), lwd=3)

bball$urban <- ifelse(bball$site=="arb", 1, 0)
modtest <- lmer(bbws.gdd ~ urban + (urban|species), data=bball)
arm::display(modtest)

modtest.hl <- lmer(bbhl.gdd ~ urban + (urban|species), data=bball)
arm::display(modtest.hl)


if(FALSE){
  #### Before moving on, let's look at the data a bit
  cols <- brewer.pal(n = 8, name = "Dark2")
  
  ggplot(df, aes(x=tmean.ws)) + geom_histogram(aes(fill=site)) + theme_classic() +
    scale_fill_manual(name="Site", values=cols, labels=sort(unique(df$site)))
  
  ggplot(df, aes(x=tmean)) + geom_histogram(aes(fill=microsite)) + theme_classic() +
    scale_fill_manual(name="Site", values=cols, labels=sort(unique(df$microsite)))
}

################### Modelling time ######################
library(rstan)
bball$urban <- ifelse(bball$site=="arb", 1, 0)

datalist.gdd <- with(bball, 
                     list(y = bbws.gdd, 
                          tx = urban,
                          sp = as.numeric(as.factor(species)),
                          N = nrow(bball),
                          n_sp = length(unique(bball$species))
                     )
)


ws_urb_buildfake = stan('stan/urbanmodel_stan_normal_ncp.stan', data = datalist.gdd,
                   iter = 4000, warmup=2000) ### 

check_all_diagnostics(ws_urb_buildfake)

ws_urb_fake.sum <- summary(ws_urb_buildfake)$summary
ws_urb_fake.sum[grep("mu_", rownames(ws_urb_fake.sum)),]
ws_urb_fake.sum[grep("sigma_", rownames(ws_urb_fake.sum)),]

save(ws_urb_buildfake, file="~/Documents/git/microclimates/analyses/stan/ws_urban_stan_builtsims_ncp.Rdata")
