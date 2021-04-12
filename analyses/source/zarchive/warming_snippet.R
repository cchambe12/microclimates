### Forecast code snippet for Lizzie
# Started 15 March 2021 by Cat

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#Load Libraries
library(dplyr)
library(tidyr)


# Step 1: Set up years, days per year, temperatures, sampling frequency, required GDD (fstar)
daysperyr <- 40 #### just to make sure we don't get any NAs
nspps <- 5 

### Now the climate data 
cc <- 10
sigma.cc <- 1
warmcc <- 2
basetemp <- 0

fstar.min <- 100
fstar.max <- 300


#### Next I set up an fstar or a GDD threshold for each individual
df.fstar <- as.data.frame(cbind(species=1:nspps, 
                                fstarspp=10*round(seq(fstar.min, fstar.max, 
                                                      by=(fstar.max-fstar.min)/(nspps-1))/10, digits=0)))

df.fstar$fstarspp <- as.numeric(df.fstar$fstarspp)

### # Step 2: find GDDs
#### Now I set up climate data 
tmeanbase <- rnorm(daysperyr, cc + 0, sigma.cc) ### Base temperature to be based off of for each individual

clim <- data.frame(day=rep(c(1:daysperyr), times=nspps),
                   tmean0=rep(tmeanbase, times=nspps),
                   fstarspp=rep(df.fstar$fstarspp, each=daysperyr))

clim$tmean1 <- clim$tmean0 + 1 ## Want to use the same temperature to find warming
clim$tmean2 <- clim$tmean0 + 2

##Step 3: Make a data frame and get the mean temp
df <- clim
df$tmean0 <- as.numeric(df$tmean0)
df$tmean1 <- as.numeric(df$tmean1)
df$tmean2 <- as.numeric(df$tmean2)

df$tmean0 <- ifelse(df$tmean0>=basetemp, df$tmean0, 0) ## This is just making sure were are accumulating temperatures based on the base temperature for our GDD equation
df$tmean1 <- ifelse(df$tmean1>=basetemp, df$tmean1, 0)
df$tmean2 <- ifelse(df$tmean2>=basetemp, df$tmean2, 0)

### Calculate the OBSERVED GDDs!!!
df$gdd.obs0 <- ave(df$tmean0, df$fstarspp, FUN=cumsum) ## Now we are simply adding up the temperatures for each fstar
df$gdd.obs1 <- ave(df$tmean1, df$fstarspp, FUN=cumsum) ## and for each degree of warming
df$gdd.obs2 <- ave(df$tmean2, df$fstarspp, FUN=cumsum)

## Find the day of budburst to find the actual GDD versus the "observed GDD"
for(i in c(unique(df$fstarspp))){ # i="50" i=1
  
  bb0 <- which(df$gdd.obs0[i==df$fstarspp] >= df$fstarspp[i==df$fstarspp])[1] ### I hope this is finding the first incidence that the observed GDD is equal to or greater than the fstar threshold for each individual
  df$bb0[i==df$fstarspp] <- bb0
  bb1 <- which(df$gdd.obs1[i==df$fstarspp] >= df$fstarspp[i==df$fstarspp])[1]
  df$bb1[i==df$fstarspp] <- bb1
  bb2 <- which(df$gdd.obs2[i==df$fstarspp] >= df$fstarspp[i==df$fstarspp])[1]
  df$bb2[i==df$fstarspp] <- bb2
  
}

df.bb <- df[(df$bb0==df$day | df$bb1==df$day | df$bb2==df$day),] ### This is narrowing down the dataframe

df.bb$gdd.obs0 <- ifelse(df.bb$day==df.bb$bb0, df.bb$gdd.obs0, NA) ## Again, just some data cleaning to remove the other GDDS, we just want the first observation!
df.bb$gdd.obs1 <- ifelse(df.bb$day==df.bb$bb1, df.bb$gdd.obs1, NA)
df.bb$gdd.obs2 <- ifelse(df.bb$day==df.bb$bb2, df.bb$gdd.obs2, NA)


df.bb <- subset(df.bb, select=c("fstarspp", "gdd.obs0",
                                "gdd.obs1", "gdd.obs2")) 
bball <- df.bb %>% tidyr::gather(warming, gdd, gdd.obs0:gdd.obs2, -fstarspp) ## reorganizing the dataframe to make it easier for plotting
bball$warming <- as.numeric(substr(bball$warming, 8, 9))

bball <- bball[!is.na(bball$gdd),]


##### Now for the accuracy calculations...
bball$gdd_accuracy <- bball$gdd - bball$fstarspp
bball$gdd_ratio <- bball$gdd/bball$fstarspp

bball <- na.omit(bball)

