### Started 23 April 2020 by Cat
## Building new dataframe with fake data to try and better understand hobo logger data versus microclimate data

# Maybe I should use estimates for fstar from real models?

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#### Overall model:
# GDD ~ urban + method + method*urban + (urban + method + method*urban|species) 

library(RColorBrewer)
library(viridis)
library(lme4)
library(ggplot2)
library(gridExtra)
library(rstan)

######### BEFORE WE START, HERE ARE THE HYPOTHESES:
 ## A) Hobos are less accurate measures of the same weather
      ### Hobos will have greater variation 

 ## B) Hobos better estimate true GDD -- capture microclimate and/or they capture treatments 
                  #(urban, prov) better…
      ## B1) Hobos capture urban effect better
      ## B2) Hobos capture provenance effect better
      ### The weather station data will have a greater variation in this instance. 
 

 
## Let's load some real data to check out.
setwd("~/Documents/git/microclimates/analyses/")

if(FALSE){
  ws <- read.csv("output/clean_gdd_chill_bbanddvr.csv")
  mean(ws$gdd_bb, na.rm=TRUE) ## 292
  sd(ws$gdd_bb, na.rm = TRUE) ## 116
  
  mean(ws$budburst) ## 109.15
  sd(ws$budburst) ## 14.22
  mean(ws$budburst[ws$type=="Harvard Forest"]) ## 132.75
  mean(ws$budburst[ws$type=="Treespotters"]) ## 112.45
}

use.urban = TRUE
use.provenance = FALSE
hypothA = TRUE
hypothB = FALSE

if(use.urban == TRUE & use.provenance == TRUE){
  print("Error has occurred. Can't have both urban and provenance equal TRUE!")
}

if(hypothA == TRUE & hypothB == TRUE){
  print("Error has occurred. Can't have both hypotheses equal TRUE!")
}


# Step 1: Set up years, days per year, temperatures, sampling frequency, required GDD (fstar)
daysperyr <- 100 #### just to make sure we don't get any NAs
nspps <- 20 
ninds <- 10 
nobs <- nspps*ninds
nsites <- 2  ### Arboretum versus the Forest
nmicros <- 10  ### Number per site so 20 total 
nmethods <- 2

if(use.urban==TRUE){
  urbeffect <- -50  ### mu_b_urban_sp      ### IF NEGATIVE, THEN THIS MEANS WE EXPECT THE ARBORETUM REQUIRES FEWER GDD! Maybe because chilling is higher?
  urbsd <- 10 ## sigma_b_urban_sp
  methodeffect <- -20 ## mu_b_method_sp    ### IF NEGATIVE, THEN THIS MEANS WE EXPECT THE STATION MEASURES FEWER GDD! Maybe because it is cooler, thus accumulating GDD more slowly
  methodsd <- 10 ## sigma_b_method_sp 
}


if(use.provenance==TRUE){
  proveffect <- -5
  provsd <- 0 ## sigma_b_tx_sp
  methodeffect <- 100 ## mu_b_method_sp
  methodsd <- 0 ## sigma_b_method_sp
}

fstar <- 300  ### mu_a_sp
fstarspeciessd <- 50 ### sigma_a_sp
fstarindsd <- 20 ## sigma_y
  

# FOR HYPOTH A, THE WEATHER DATA MUST BE IDENTICAL. LINE 91 AND LINE 96 SHOULD BE EQUAL AND LINE 92, 94, 97, 99 SHOULD BE ZERO!!
dayz <- rep(1:daysperyr, nobs)
cc.arb <- 11 ## based off weather station data
microarb.effect <- -4
sigma.arb <- 5 
microsigmaarb.effect <- 0   #### by keeping the sigmas the same for the microsites (line 94 & 99) we assume that the microclimatic effects are the same across sites

cc.hf <- 11  ## based off weather station data
microhf.effect <- -4
sigma.hf <- 5  
microsigmahf.effect <- 0  #### by keeping the sigmas the same for the microsites (line 94 & 99) we assume that the microclimatic effects are the same across sites

source("simulations/micro_databuildfx_doy.R") 

cols <-viridis_pal(option="viridis")(3)
## Just a quick check on GDDs
quartz(width=4, height=4)
ggplot(bball, aes(x=gdd)) + geom_histogram(aes(fill=site)) + theme_classic() +
  scale_fill_manual(name="Site", values=cols, labels=sort(unique(df$site))) 
  
  if(hypothA==TRUE){
    quartz(width=3, height=3)
    #par(mfrow=c(1,2))
    ggplot(bball, aes(x=gdd)) + geom_histogram(aes(fill=method)) + theme_classic() +
      scale_fill_manual(name="Method", values=cols, labels=sort(unique(df$method)))
  }


### Okay, first let's check on site level varition in temperature
#### Before moving on, let's look at the data a bit
ws <- ggplot(df[(df$method=="ws"),], aes(x=tmean)) + geom_histogram(aes(fill=site)) + theme_classic() +
  scale_fill_manual(name="Site", values=cols, labels=sort(unique(df$site))) + ggtitle("Weather Station") +
  coord_cartesian(xlim=c(-10, 25))

hl <- ggplot(df[(df$method=="hobo"),], aes(x=tmean)) + geom_histogram(aes(fill=site)) + theme_classic() +
  scale_fill_manual(name="Site", values=cols, labels=sort(unique(df$site))) + ggtitle("Hobo Logger") +
  coord_cartesian(xlim=c(-10, 25))

quartz(width=6, height=4)
grid.arrange(ws, hl, ncol=2)


### Now let's look at GDD differences between methods
quartz(width=6, height=5)
par(mfrow=c(1,2))
my.pal <- viridis_pal(option="magma")(20)
my.pch <- c(15:16)
plot(gdd ~ species, col=my.pal[as.factor(bball$species)], pch=my.pch[as.factor(bball$site)], data = bball[(bball$method=="ws"),], main="Weather Station",
     ylab="GDD", ylim=c(0, 600))
abline(h=mean(bball$gdd[bball$method=="ws"]), lwd=3)

plot(gdd ~ species, col=my.pal[as.factor(bball$species)], pch=my.pch[as.factor(bball$site)], data = bball[(bball$method=="hobo"),], main="Hobo Logger",
     ylab="GDD", ylim=c(0, 600))
abline(h=mean(bball$gdd[bball$method=="hobo"]), lwd=3)

### Finally, let's take a look at GDD accuracy...
quartz(width=8, height=5)
par(mfrow=c(1,3))
bball$type <- ifelse(bball$method=="ws", 1, 0)
plot(bball$type, bball$gdd_accuracy, col=cols[as.factor(bball$method)])
legend(0, -20, sort(unique(gsub("_", " ", bball$method))), pch=19,
       col=cols[as.factor(bball$method)],
       cex=1, bty="n")
plot(bball$fstar.new ~ bball$gdd, col=cols[as.factor(bball$method)])
legend(300, 150, sort(unique(gsub("_", " ", bball$method))), pch=19,
       col=cols[as.factor(bball$method)],
       cex=1, bty="n")
plot(bball$fstar.new ~ bball$gdd, col=cols[as.factor(bball$site)])
legend(300, 150, sort(unique(gsub("_", " ", bball$site))), pch=19,
       col=cols[as.factor(bball$site)],
       cex=1, bty="n")

if(FALSE){
### Next, we can take a quick glimpse at results
if(use.urban==TRUE){
  bball$urban <- ifelse(bball$site=="arb", 1, 0)
  bball$type <- ifelse(bball$method=="ws", 1, 0)
  
  modall <- lmer(gdd ~ urban + type + urban*type + (urban + type + urban*type|species), data=bball)
  arm::display(modall)
}

if(use.provenance==TRUE){
  bball$provenance <- as.numeric(bball$provenance)
  bball$type <- ifelse(bball$method=="ws", 1, 0)
  
  modall <- lmer(gdd ~ provenance + type + (provenance + type|species), data=bball)
  arm::display(modall)
}
} 

#####And finally... it's modelling time!
if(use.urban==TRUE){
  bball$urban <- ifelse(bball$site=="arb", 1, 0)
  bball$type <- ifelse(bball$method=="ws", 1, 0)

  datalist.gdd <- with(bball, 
                       list(y = gdd, 
                            urban = urban,
                            method = type,
                            sp = as.numeric(as.factor(species)),
                            N = nrow(bball),
                            n_sp = length(unique(bball$species))
                       )
)
}


urbmethod_fake = stan('stan/urbanmethod_normal_ncp_inter.stan', data = datalist.gdd,
                   iter = 2500, warmup=1500, chains=4)#, control=list(adapt_delta=0.99)) ### 

#check_all_diagnostics(ws_urb_buildfake)

fstar ## mu_a_sp
urbeffect ## mu_b_urban_sp
methodeffect ## mu_b_method_sp

fstarspeciessd ## sigma_a_sp
urbsd   ## sigma_b_urban_sp
methodsd  ## sigma_b_method_sp
fstarindsd  ## sigma_y_sp


urbmethod_fakesum <- summary(urbmethod_fake)$summary
urbmethod_fakesum[grep("mu_", rownames(urbmethod_fakesum)),]
urbmethod_fakesum[grep("sigma_", rownames(urbmethod_fakesum)),]

#save(ws_urb_buildfake, file="~/Documents/git/microclimates/analyses/stan/ws_urban_stan_builtsims_ncp.Rdata")

modpreds <- as.data.frame(rstan::extract(urbmethod_fake), permuted=TRUE)
yraw <- bball$gdd

if(FALSE){
###### Now, let's look at a lot of plots to look at model accuracy
post <- rstan::extract(urbmethod_fake)

str(post)
names(post)

plot(density(post$mu_a_sp))
plot(density(post$mu_b_urban_sp))
plot(density(post$mu_b_method_sp))
plot(density(post$mu_b_um_sp))

plot(density(data.frame(post$sigma_a_sp)[,1])) 
plot(density(data.frame(post$sigma_b_urban_sp)[,1])) 
plot(density(data.frame(post$sigma_b_method_sp)[,1])) 
plot(density(data.frame(post$sigma_b_um_sp)[,1])) 

plot(density(post$sigma_y))

#Predicted values
mean_y <- colMeans(data.frame(post$y_ppc))
sd_y <- apply(data.frame(post$y_ppc), 2, sd) 
str(sd_y)

ypreds <- data.frame(mean_y)
head(ypreds)
ypreds$upper_y <- mean_y + sd_y
ypreds$lower_y <- mean_y - sd_y

#plot predicted value against method
#black lines are the mean slopes, colored ones are sd

plot(ypreds$lower_y ~ bball$hobo, col="blue4")
points(ypreds$upper_y ~ bball$hobo, col="blue4")
points(ypreds$mean_y ~ bball$hobo, col="black")

#plot predicted values against empirical ones (not including sigma_y)
#black points are the mean values, colored ones are sd

plot(ypreds$lower_y, bball$gdd, col="red4", type = "p", pch = 16)
points(ypreds$upper_y, bball$gdd, col="red4", pch = 16)
points(ypreds$mean_y, bball$gdd, col="black", pch = 16)

#variety effects
a_sps <- data.frame(post$a_sp)
mean_asps <- colMeans(a_sps)
b_urbs <- data.frame(post$b_urban)
mean_burbs <- colMeans(b_urbs)
b_mets <- data.frame(post$b_method)
mean_mets <- colMeans(b_mets)

plot(mean_asps ~ mean_burbs)
plot(mean_asps ~ mean_mets)

#predictions for each variety 
library(bayesplot)
mcmc_intervals(a_sps) + geom_vline(xintercept = fstar, linetype="dotted", color = "grey")  #intercepts 
mcmc_intervals(b_urbs)+ geom_vline(xintercept = urbeffect, linetype="dotted", color = "grey") #urban preds
mcmc_intervals(b_mets)+ geom_vline(xintercept = methodeffect, linetype="dotted", color = "grey") #method preds


sigma_as <- data.frame(post$sigma_a_sp)
sigma_urbs <- data.frame(post$sigma_b_urban_sp)
sigma_methods <- data.frame(post$sigma_b_method_sp)

mcmc_intervals(sigma_as) + geom_vline(xintercept = fstar, linetype="dotted", color = "grey")  #intercepts 
mcmc_intervals(sigma_urbs)+ geom_vline(xintercept = urbeffect, linetype="dotted", color = "grey") #urban preds
mcmc_intervals(sigma_methods)+ geom_vline(xintercept = methodeffect, linetype="dotted", color = "grey") #method preds
}




##### Provenance Model!
if(use.provenance==TRUE){
  bball$provenance <- as.numeric(bball$provenance)
  bball$type <- ifelse(bball$method=="ws", 1, 0)
  
  datalist.gdd <- with(bball, 
                       list(y = bbws.gdd, 
                            prov = provenance,
                            method = type,
                            sp = as.numeric(as.factor(species)),
                            N = nrow(bball),
                            n_sp = length(unique(bball$species))
                       )
  )
} 
  
if(use.provenance==TRUE){
  provmethod_fake = stan('stan/provmethod_stan_normal_ncp.stan', data = datalist.gdd,
                          iter = 4000, warmup=2000, control=list(adapt_delta = 0.99)) ### BAD MODEL!!!
  
  #check_all_diagnostics(ws_prov_buildfake)
  
  provmethod_fake.sum <- summary(provmethod_fake)$summary
  provmethod_fake.sum[grep("mu_", rownames(provmethod_fake.sum)),]
  provmethod_fake.sum[grep("sigma_", rownames(provmethod_fake.sum)),]
  
  #save(ws_prov_buildfake, file="~/Documents/git/microclimates/analyses/stan/ws_prov_stan_builtsims_ncp.Rdata")
}  
  

