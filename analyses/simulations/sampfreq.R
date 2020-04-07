### Started 6 April 2020 by Cat
## Now work on changing observation frequency using simulation data
# Following Lizzie's code from ospree/bb_analysis/pep_sims/pepvarsimfxs.R and pepvarsim.R

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

## Let's load some real data to check out.
setwd("~/Documents/git/microclimates/analyses/")

ws <- read.csv("output/clean_gdd_chill_bbanddvr.csv")


# This controls how many runs of the whole thing you do ...
drawstotal <- 50
draws.sevendays <- c()

# Big outside loop ... 
for(j in 1:drawstotal){
  
  # Step 1: Set up years, days per year, temperatures, sampling frequency, required GDD (fstar)
  daysperyr <- 100
  yrstotal <- 40
  yrs <- rep(1:yrstotal, each=daysperyr)
  dayz <- rep(1:daysperyr, yrstotal)
  cc <- rnorm(length(dayz), 10, 3)
  sigma <- 20
  fstar <- 400
  samplefreq <- 7
  
  #cc <- c(rep("precc", yrstotal/2), rep("postcc", yrstotal/2))
  
  # Step 2: find GDDs
  dailytemp <- cc
  gdd <- c(gdd, cumsum(dailytemphere))
  
  # Step 3: Make a data frame and get the mean temp per year (to double check the data)
  df <- data.frame(cbind(yrs, dayz, dailytemp, gdd))
  df.meantemp <- aggregate(df["dailytemp"], df[c("yrs")], FUN=mean)
  plot(dailytemp ~ yrs, data=df.meantemp)
  
  # Step 4: Now, in a very slow, painful way I get the BB date
  df$bb.YN <- NA
  
  for (i in c(1:nrow(df))){ # This loop just makes a Yes/No vector for budburst
    if (df$gdd[i]<fstar) {
      df$bb.YN[i] <- "N"
    } else {
      df$bb.YN[i] <- "Y"
    }
  }
  
  # Step 5: Now we remove rows based on sampling frequency and then calculate the observed BB date
  df.sample <- df[seq(1, nrow(df), samplefreq),]
  
  bbdate <- c()
  
  for (i in 1:yrstotal){
    subby <- subset(df.sample, yrs==i & bb.YN=="Y")
    bbdate[i] <- subby$dayz[1]
  }
  
  # Step 6: Whoop! Now we can caculate temperature sensitivities.
  #dfcalc <- cbind(bbdate, df.meantemp, cc)
  #estprecc <- lm(bbdate~dailytemp, data=subset(dfcalc, cc=="precc"))
  #estpostcc <- lm(bbdate~dailytemp, data=subset(dfcalc, cc=="postcc"))
  
  #diffbefore.after <- coef(estprecc)[2]-coef(estpostcc)[2]
  # negative means a decline in sensitivity AFTER climate change
  
  draws.sevendays <- rbind(draws.sevendays, )
}

mean(draws)
hist(draws, breaks=10)




