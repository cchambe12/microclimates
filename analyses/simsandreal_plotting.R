#### Replicating figures from Shiny App for MS
## 1 March 2021 by Cat

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())

#### Overall model:
# GDD ~ urban + method + method*urban + (urban + method + method*urban|species) 

library(RColorBrewer)
library(viridis)
library(ggplot2)
library(gridExtra)
library(rstan)

setwd("~/Documents/git/microclimates/analyses/")

source("source/sims_hypoth_sourcedata.R")
source("source/sims_hypoth_interxn_sourcedata.R")
#source("source/sims_params_sourcedata.R")

realgdd <- read.csv("output/cleanmicro_gdd_2019.csv")


###### Okay, we'll first start with the first hypothesis: Weather stations are less accurate measures of the same weather than hobo loggers.
## For the bbfunc(), we need to input a lot of information.
# 1. What is our question? Are we looking at the method effect? This would be "hobo" or the urban effect which is "urban" or finally the provenance effect which is "prov"
# 2. What parameter are we interested in? This can be "ws" or "hobo" if we select "hobo" as our question (#1)
# 3. What is the mu effect of our question? 
# 4. What is the sigma of our question?
# 5. What is the GDD threshold or fstar?
# 6. What is the sigma fstar?
# 7. Mean temperature, we keep this consistent across the two sites
# 8. Sigma temperature, we keep this consistent across the two sites as well
# 9. Sigma of microclimatic effect (so what is added to the sigma temperature at the two sites)

simsdat <- bbfunc("hobo", "ws", 0, 15, 300, 20, 10, 3, 0)

cols <-viridis_pal(option="viridis")(3)

bball <- simsdat[[1]]
clim <- simsdat[[2]]

    use.urban <- "urban"
    bball$treatmenttype <- if(use.urban=="urban"){ifelse(bball$site=="arb", 1, 0)}else if(use.urban=="prov"){
      as.numeric(bball$prov)}
    
    datalist.gdd <- with(bball, 
                         list(y = gdd, 
                              urban = treatmenttype,
                              method = type,
                              sp = as.numeric(as.factor(species)),
                              N = nrow(bball),
                              n_sp = length(unique(bball$species))
                         )
    )
    
    
    noisyws_fake = stan('stan/urbanmethod_normal_ncp_inter.stan', data = datalist.gdd,
                          iter = 3000, warmup=2500, chains=4)#, control=list(adapt_delta=0.99, max_treedepth=15))
    
    
    my.pal <-rep(viridis_pal(option="viridis")(9),2)
    my.pch <- rep(15:18, each=10)
    alphahere = 0.4
    
    modoutput <- summary(noisyws_fake)$summary
    noncps <- modoutput[!grepl("_ncp", rownames(modoutput)),]
    labs <- if(use.urban=="urban"){c("Site", "Method", "Site x Method",
                                     "Sigma Site", "Sigma Method", 
                                     "Sigma Interaction")}else if(use.urban=="prov"){
                                       c("Provenance", "Method", "Provenance x\nMethod",
                                         "Sigma Provenance", "Sigma \nMethod", 
                                         "Sigma Interaction")}
    
    modelhere <- noisyws_fake
    spnum <- length(unique(bball$species))

pdf("figures/muplot_noisyws.pdf", width=7, height=4)
    par(xpd=FALSE)
    par(mar=c(5,10,3,10))
    plot(x=NULL,y=NULL, xlim=c(-30,30), yaxt='n', ylim=c(0,6),
         xlab="Model estimate change in growing degree days to budburst", ylab="")
    axis(2, at=1:6, labels=rev(labs), las=1)
    abline(v=0, lty=2, col="darkgrey")
    rownameshere <- c("mu_b_urban_sp", "mu_b_method_sp", "mu_b_um_sp", "sigma_b_urban_sp",
                      "sigma_b_method_sp", "sigma_b_um_sp")
    for(i in 1:6){
      pos.y<-(6:1)[i]
      pos.x<-noncps[rownameshere[i],"mean"]
      lines(noncps[rownameshere[i],c("2.5%","97.5%")],rep(pos.y,2),col="lightgrey")
      lines(noncps[rownameshere[i],c("25%","75%")],rep(pos.y,2),col="darkgrey")
      points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
      for(spsi in 1:spnum){
        pos.sps.i<-which(grepl(paste0("[",spsi,"]"),rownames(noncps),fixed=TRUE))[c(2:4)]
        jitt<-(spsi/40) + 0.08
        pos.y.sps.i<-pos.y-jitt
        pos.x.sps.i<-noncps[pos.sps.i[i],"mean"]
        lines(noncps[pos.sps.i[i],c("2.5%","97.5%")],rep(pos.y.sps.i,2),
              col=alpha(my.pal[spsi], alphahere-0.2))
        lines(noncps[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
              col=alpha(my.pal[spsi], alphahere))
        points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
        
      }
    }
    par(xpd=TRUE) 
    dev.off()
    save(noisyws_fake, file="~/Documents/git/microclimates/analyses/stan/noisyws_sims.Rdata")

##########################################################################################################    
##########################################################################################################    
##########################################################################################################
##### Now, let's check out the simulations if we have a noisy hobo logger versus the weather station
## I will keep the parameters the exact same..
    
    simsdat <- bbfunc("hobo", "hobo", 0, 15, 300, 20, 10, 3, 0)
    
    cols <-viridis_pal(option="viridis")(3)
    
    bball <- simsdat[[1]]
    clim <- simsdat[[2]]
    
    use.urban <- "urban"
    bball$treatmenttype <- if(use.urban=="urban"){ifelse(bball$site=="arb", 1, 0)}else if(use.urban=="prov"){
      as.numeric(bball$prov)}
    
    datalist.gdd <- with(bball, 
                         list(y = gdd, 
                              urban = treatmenttype,
                              method = type,
                              sp = as.numeric(as.factor(species)),
                              N = nrow(bball),
                              n_sp = length(unique(bball$species))
                         )
    )
    
    noisyhobo_fake = stan('stan/urbanmethod_normal_ncp_inter.stan', data = datalist.gdd,
                          iter = 3000, warmup=2500, chains=4)#, control=list(adapt_delta=0.99, max_treedepth=15))
    
    
    my.pal <-rep(viridis_pal(option="viridis")(9),2)
    my.pch <- rep(15:18, each=10)
    alphahere = 0.4
    
    modoutput <- summary(noisyhobo_fake)$summary
    noncps <- modoutput[!grepl("_ncp", rownames(modoutput)),]
    labs <- if(use.urban=="urban"){c("Site", "Method", "Site x Method",
                                     "Sigma Site", "Sigma Method", 
                                     "Sigma Interaction")}else if(use.urban=="prov"){
                                       c("Provenance", "Method", "Provenance x\nMethod",
                                         "Sigma Provenance", "Sigma \nMethod", 
                                         "Sigma Interaction")}
    
    modelhere <- noisyhobo_fake
    spnum <- length(unique(bball$species))

pdf("figures/muplot_noisyhobo.pdf", width=7, height=4)
    par(xpd=FALSE)
    par(mar=c(5,10,3,10))
    plot(x=NULL,y=NULL, xlim=c(-30,30), yaxt='n', ylim=c(0,6),
         xlab="Model estimate change in growing degree days to budburst", ylab="")
    axis(2, at=1:6, labels=rev(labs), las=1)
    abline(v=0, lty=2, col="darkgrey")
    rownameshere <- c("mu_b_urban_sp", "mu_b_method_sp", "mu_b_um_sp", "sigma_b_urban_sp",
                      "sigma_b_method_sp", "sigma_b_um_sp")
    for(i in 1:6){
      pos.y<-(6:1)[i]
      pos.x<-noncps[rownameshere[i],"mean"]
      lines(noncps[rownameshere[i],c("2.5%","97.5%")],rep(pos.y,2),col="lightgrey")
      lines(noncps[rownameshere[i],c("25%","75%")],rep(pos.y,2),col="darkgrey")
      points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
      for(spsi in 1:spnum){
        pos.sps.i<-which(grepl(paste0("[",spsi,"]"),rownames(noncps),fixed=TRUE))[c(2:4)]
        jitt<-(spsi/40) + 0.08
        pos.y.sps.i<-pos.y-jitt
        pos.x.sps.i<-noncps[pos.sps.i[i],"mean"]
        lines(noncps[pos.sps.i[i],c("2.5%","97.5%")],rep(pos.y.sps.i,2),
              col=alpha(my.pal[spsi], alphahere-0.2))
        lines(noncps[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
              col=alpha(my.pal[spsi], alphahere))
        points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
        
      }
    }
    par(xpd=TRUE) 
    dev.off()
    save(noisyhobo_fake, file="~/Documents/git/microclimates/analyses/stan/noisyhobo_sims.Rdata")

####################################################################################################
####################################################################################################
####################################################################################################
#### Alright, so now we want to test if hobo data is a more accurate measure of the same weather - meaning, there might be microclimates!
## So I think the way to do this is to make sure that hobos are more accurate than the weather because they are picking up the temperature more precisely
# and then we need to add more sigma to the hobo loggers to simulate microclimates
simsdat <- bbfunc("NA", "NA", 0, 0, 300, 20, 20, 0, 15)

    cols <-viridis_pal(option="viridis")(3)
    
    bball <- simsdat[[1]]
    clim <- simsdat[[2]]


    use.urban <- "urban"
    bball$treatmenttype <- if(use.urban=="urban"){ifelse(bball$site=="arb", 1, 0)}else if(use.urban=="prov"){
      as.numeric(bball$prov)}
    
    datalist.gdd <- with(bball, 
                         list(y = gdd, 
                              urban = treatmenttype,
                              method = type,
                              sp = as.numeric(as.factor(species)),
                              N = nrow(bball),
                              n_sp = length(unique(bball$species))
                         )
    )
    
    micros_fake = stan('stan/urbanmethod_normal_ncp_inter.stan', data = datalist.gdd,
                          iter = 3000, warmup=2500, chains=4)#, control=list(adapt_delta=0.99, max_treedepth=15))
    
    
    my.pal <-rep(viridis_pal(option="viridis")(9),2)
    my.pch <- rep(15:18, each=10)
    alphahere = 0.4
    
    modoutput <- summary(micros_fake)$summary
    noncps <- modoutput[!grepl("_ncp", rownames(modoutput)),]
    labs <- if(use.urban=="urban"){c("Site", "Method", "Site x Method",
                                     "Sigma Site", "Sigma Method", 
                                     "Sigma Interaction")}else if(use.urban=="prov"){
                                       c("Provenance", "Method", "Provenance x\nMethod",
                                         "Sigma Provenance", "Sigma \nMethod", 
                                         "Sigma Interaction")}
    
    modelhere <- micros_fake
    spnum <- length(unique(bball$species))

  pdf("figures/muplot_micros.pdf", width=7, height=4)
      par(xpd=FALSE)
      par(mar=c(5,10,3,10))
      plot(x=NULL,y=NULL, xlim=c(-30,30), yaxt='n', ylim=c(0,6),
           xlab="Model estimate change in growing degree days to budburst", ylab="")
      axis(2, at=1:6, labels=rev(labs), las=1)
      abline(v=0, lty=2, col="darkgrey")
      rownameshere <- c("mu_b_urban_sp", "mu_b_method_sp", "mu_b_um_sp", "sigma_b_urban_sp",
                        "sigma_b_method_sp", "sigma_b_um_sp")
      for(i in 1:6){
        pos.y<-(6:1)[i]
        pos.x<-noncps[rownameshere[i],"mean"]
        lines(noncps[rownameshere[i],c("2.5%","97.5%")],rep(pos.y,2),col="lightgrey")
        lines(noncps[rownameshere[i],c("25%","75%")],rep(pos.y,2),col="darkgrey")
        points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
        for(spsi in 1:spnum){
          pos.sps.i<-which(grepl(paste0("[",spsi,"]"),rownames(noncps),fixed=TRUE))[c(2:4)]
          jitt<-(spsi/40) + 0.08
          pos.y.sps.i<-pos.y-jitt
          pos.x.sps.i<-noncps[pos.sps.i[i],"mean"]
          lines(noncps[pos.sps.i[i],c("2.5%","97.5%")],rep(pos.y.sps.i,2),
                col=alpha(my.pal[spsi], alphahere-0.2))
          lines(noncps[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
                col=alpha(my.pal[spsi], alphahere))
          points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
          
        }
      }
  par(xpd=TRUE)
  dev.off()
  save(micros_fake, file="~/Documents/git/microclimates/analyses/stan/micros_sims.Rdata")


####################################################################################################
####################################################################################################
####################################################################################################
#### Next, we are interested in testing the effect of provenance. Our hypothesis is that individuals from 
# higher provenances will require fewer GDDs 
simsdat <- bbfunc("urban", "NA", 20, 2, 300, 20, 10, 3, 0)

  cols <-viridis_pal(option="viridis")(3)
  
  bball <- simsdat[[1]]
  clim <- simsdat[[2]]

      use.urban <- "urban"
      bball$treatmenttype <- if(use.urban=="urban"){ifelse(bball$site=="arb", 1, 0)}else if(use.urban=="prov"){
        as.numeric(bball$prov)}
      
      datalist.gdd <- with(bball, 
                           list(y = gdd, 
                                urban = treatmenttype,
                                method = type,
                                sp = as.numeric(as.factor(species)),
                                N = nrow(bball),
                                n_sp = length(unique(bball$species))
                           )
      )
      
      urban_fake = stan('stan/urbanmethod_normal_ncp_inter.stan', data = datalist.gdd,
                            iter = 5000, warmup=4500, chains=4)#, control=list(adapt_delta=0.99, max_treedepth=15))
      
      
      my.pal <-rep(viridis_pal(option="viridis")(9),2)
      my.pch <- rep(15:18, each=10)
      alphahere = 0.4
      
      modoutput <- summary(urban_fake)$summary
      noncps <- modoutput[!grepl("_ncp", rownames(modoutput)),]
      labs <- if(use.urban=="urban"){c("Site", "Method", "Site x Method",
                                       "Sigma Site", "Sigma Method", 
                                       "Sigma Interaction")}else if(use.urban=="prov"){
                                         c("Provenance", "Method", "Provenance x\nMethod",
                                           "Sigma Provenance", "Sigma \nMethod", 
                                           "Sigma Interaction")}
      
      modelhere <- urban_fake
      spnum <- length(unique(bball$species))

  pdf("figures/muplot_urban.pdf", width=7, height=4)
      par(xpd=FALSE)
      par(mar=c(5,10,3,10))
      plot(x=NULL,y=NULL, xlim=c(-30,50), yaxt='n', ylim=c(0,6),
           xlab="Model estimate change in growing degree days to budburst", ylab="")
      axis(2, at=1:6, labels=rev(labs), las=1)
      abline(v=0, lty=2, col="darkgrey")
      rownameshere <- c("mu_b_urban_sp", "mu_b_method_sp", "mu_b_um_sp", "sigma_b_urban_sp",
                        "sigma_b_method_sp", "sigma_b_um_sp")
      for(i in 1:6){ #i=6
        pos.y<-(6:1)[i]
        pos.x<-noncps[rownameshere[i],"mean"]
        lines(noncps[rownameshere[i],c("2.5%","97.5%")],rep(pos.y,2),col="lightgrey")
        lines(noncps[rownameshere[i],c("25%","75%")],rep(pos.y,2),col="darkgrey")
        points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
        for(spsi in 1:spnum){
          pos.sps.i<-which(grepl(paste0("[",spsi,"]"),rownames(noncps),fixed=TRUE))[c(2:4)]
          jitt<-(spsi/40) + 0.08
          pos.y.sps.i<-pos.y-jitt
          pos.x.sps.i<-noncps[pos.sps.i[i],"mean"]
          lines(noncps[pos.sps.i[i],c("2.5%","97.5%")],rep(pos.y.sps.i,2),
                col=alpha(my.pal[spsi], alphahere-0.2))
          lines(noncps[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
                col=alpha(my.pal[spsi], alphahere))
          points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
          
        }
      }
  par(xpd=TRUE)
  dev.off()
  save(urban_fake, file="~/Documents/git/microclimates/analyses/stan/urban_sims.Rdata")



####################################################################################################
####################################################################################################
####################################################################################################
#### Now, it is finally time to work with real data. We want to build up the simulations to match the real data.
### One thing to note at this point (9 March 2021), prov is noted as "less accurate" when in fact it is more accurate
# With the methods, we can look to see which is "less accurate", which may in fact mean that the hobo logger is picking up microclimates
# But we can scrutinize this by looking at climate data. How do we address this with provenance?

  bball <- realgdd
  bball$site <- ifelse(bball$urban==1, "arb", "hf")
  bball$type <- bball$method
  bball$method <- ifelse(bball$type==1, "ws", "hobo")
  bball$genus <- bball$species <- NULL
  bball$species <- as.numeric(as.factor(bball$spp))
  
  cols <-viridis_pal(option="viridis")(3)
  
      ws <- ggplot(bball[(bball$method=="ws"),], aes(x=gdd)) + geom_histogram(aes(fill=site), alpha=0.3) + theme_classic() +
        scale_fill_manual(name="Site", values=cols, labels=c("Urban site", "Forest site")) + ggtitle("Weather Station") +
        coord_cartesian(xlim=c(100, 700)) + 
        geom_vline(xintercept=mean(bball$gdd[(bball$method=="ws" & bball$site=="arb")]), col=cols[[1]], linetype="dashed") +
        geom_vline(xintercept=mean(bball$gdd[(bball$method=="ws" & bball$site=="hf")]), col=cols[[2]], linetype="dashed") +
        xlab("Growing Degree Days (GDD)") + ylab("") +
        scale_y_continuous(expand = c(0, 0)) +
        #scale_x_continuous(breaks = seq(-20, 40, by=5)) +
        theme(legend.position="none")
      hobo <- ggplot(bball[(bball$method=="hobo"),], aes(x=gdd)) + geom_histogram(aes(fill=site), alpha=0.3) + theme_classic() +
        scale_fill_manual(name="Site", values=cols, labels=c("Urban site", "Forest site")) + ggtitle("Hobo Logger") +
        geom_vline(xintercept=mean(bball$gdd[(bball$method=="hobo" & bball$site=="arb")]), col=cols[[1]], linetype="dashed") +
        geom_vline(xintercept=mean(bball$gdd[(bball$method=="hobo" & bball$site=="hf")]), col=cols[[2]], linetype="dashed") +
        coord_cartesian(xlim=c(100, 700)) + 
        xlab("Growing Degree Days (GDD)") + ylab("") +
        scale_y_continuous(expand = c(0, 0)) 
  pdf("figures/gdd_methods_real.pdf", width=8, height=4, onefile=FALSE)
    egg::ggarrange(ws, hobo, ncol=2)
  dev.off()
  

      use.urban <- "prov"
      bball$prov.z <- (bball$provenance-mean(bball$provenance, na.rm=TRUE))/(sd(bball$provenance,na.rm=TRUE))
      
      datalist.gdd <- with(bball, 
                           list(y = gdd, 
                                urban = prov.z,
                                method = type,
                                sp = as.numeric(as.factor(species)),
                                N = nrow(bball),
                                n_sp = length(unique(bball$species))
                           )
      )
      
      provmethod = stan('stan/provmethod_normal_ncp_inter.stan', data = datalist.gdd,
                             iter = 12000, warmup=11500, chains=4, control=list(adapt_delta=0.99, max_treedepth=15))
      
      
      my.pal <-rep(viridis_pal(option="viridis")(9),2)
      my.pch <- rep(15:18, each=10)
      alphahere = 0.4
      
      modoutput <- summary(provmethod)$summary
      noncps <- modoutput[!grepl("_ncp", rownames(modoutput)),]
      labs <- if(use.urban=="urban"){c("Site", "Method", "Site x Method",
                                       "Sigma Site", "Sigma Method", 
                                       "Sigma Interaction")}else if(use.urban=="prov"){
                                         c("Provenance", "Method", "Provenance x\nMethod",
                                           "Sigma Provenance", "Sigma \nMethod", 
                                           "Sigma Interaction")}
      
      modelhere <- provmethod
      spnum <- length(unique(bball$species))
      
      pdf("figures/muplot_prov_real.pdf", width=7, height=4)
      par(xpd=FALSE)
      par(mar=c(5,10,3,10))
      plot(x=NULL,y=NULL, xlim=c(-50,100), yaxt='n', ylim=c(0,6),
           xlab="Model estimate change in growing degree days to budburst", ylab="")
      axis(2, at=1:6, labels=rev(labs), las=1)
      abline(v=0, lty=2, col="darkgrey")
      rownameshere <- c("mu_b_urban_sp", "mu_b_method_sp", "mu_b_um_sp", "sigma_b_urban_sp",
                        "sigma_b_method_sp", "sigma_b_um_sp")
      for(i in 1:6){ #i=6
        pos.y<-(6:1)[i]
        pos.x<-noncps[rownameshere[i],"mean"]
        lines(noncps[rownameshere[i],c("2.5%","97.5%")],rep(pos.y,2),col="lightgrey")
        lines(noncps[rownameshere[i],c("25%","75%")],rep(pos.y,2),col="darkgrey")
        points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
        for(spsi in 1:spnum){
          pos.sps.i<-which(grepl(paste0("[",spsi,"]"),rownames(noncps),fixed=TRUE))[c(2:4)]
          jitt<-(spsi/40) + 0.08
          pos.y.sps.i<-pos.y-jitt
          pos.x.sps.i<-noncps[pos.sps.i[i],"mean"]
          lines(noncps[pos.sps.i[i],c("2.5%","97.5%")],rep(pos.y.sps.i,2),
                col=alpha(my.pal[spsi], alphahere-0.2))
          lines(noncps[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
                col=alpha(my.pal[spsi], alphahere))
          points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
        }
      }
  par(xpd=TRUE) 
  legend(120, 6, sort(unique(gsub("_", " ", bball$spp))), pch=my.pch[1:spnum],
         col=alpha(my.pal[1:spnum], alphahere),
         cex=0.5, bty="n", text.font=3)
  dev.off()
  save(provmethod, file="~/Documents/git/microclimates/analyses/stan/provmethod_real.Rdata")

      use.urban <- "urban"
      
      datalist.gdd <- with(bball, 
                           list(y = gdd, 
                                urban = urban,
                                method = type,
                                sp = as.numeric(as.factor(species)),
                                N = nrow(bball),
                                n_sp = length(unique(bball$species))
                           )
      )
      
      urbmethod = stan('stan/urbanmethod_normal_ncp_inter.stan', data = datalist.gdd,
                             iter = 5000, warmup=4500, chains=4, control=list(adapt_delta=0.99, max_treedepth=15))
      
      
      my.pal <-rep(viridis_pal(option="viridis")(9),2)
      my.pch <- rep(15:18, each=10)
      alphahere = 0.4
      
      modoutput <- summary(urbmethod)$summary
      noncps <- modoutput[!grepl("_ncp", rownames(modoutput)),]
      labs <- if(use.urban=="urban"){c("Site", "Method", "Site x Method",
                                       "Sigma Site", "Sigma Method", 
                                       "Sigma Interaction")}else if(use.urban=="prov"){
                                         c("Provenance", "Method", "Provenance x\nMethod",
                                           "Sigma Provenance", "Sigma \nMethod", 
                                           "Sigma Interaction")}
      
      modelhere <- urbmethod
      spnum <- length(unique(bball$species))

  pdf("figures/muplot_urban_real.pdf", width=7, height=4)
      par(xpd=FALSE)
      par(mar=c(5,10,3,10))
      plot(x=NULL,y=NULL, xlim=c(-100,100), yaxt='n', ylim=c(0,6),
           xlab="Model estimate change in growing degree days to budburst", ylab="")
      axis(2, at=1:6, labels=rev(labs), las=1)
      abline(v=0, lty=2, col="darkgrey")
      rownameshere <- c("mu_b_urban_sp", "mu_b_method_sp", "mu_b_um_sp", "sigma_b_urban_sp",
                        "sigma_b_method_sp", "sigma_b_um_sp")
      for(i in 1:6){ #i=6
        pos.y<-(6:1)[i]
        pos.x<-noncps[rownameshere[i],"mean"]
        lines(noncps[rownameshere[i],c("2.5%","97.5%")],rep(pos.y,2),col="lightgrey")
        lines(noncps[rownameshere[i],c("25%","75%")],rep(pos.y,2),col="darkgrey")
        points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
        for(spsi in 1:spnum){
          pos.sps.i<-which(grepl(paste0("[",spsi,"]"),rownames(noncps),fixed=TRUE))[c(2:4)]
          jitt<-(spsi/40) + 0.08
          pos.y.sps.i<-pos.y-jitt
          pos.x.sps.i<-noncps[pos.sps.i[i],"mean"]
          lines(noncps[pos.sps.i[i],c("2.5%","97.5%")],rep(pos.y.sps.i,2),
                col=alpha(my.pal[spsi], alphahere-0.2))
          lines(noncps[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
                col=alpha(my.pal[spsi], alphahere))
          points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
        }
      }
  par(xpd=TRUE) 
  legend(120, 6, sort(unique(gsub("_", " ", bball$spp))), pch=my.pch[1:spnum],
         col=alpha(my.pal[1:spnum], alphahere),
         cex=0.5, bty="n", text.font=3)
  dev.off()
  save(urbmethod, file="~/Documents/git/microclimates/analyses/stan/urbmethod_real.Rdata")


###########################################################################################################
####################################################################################################
####################################################################################################
#### Cool, so now we know what the real data looks like, let's try and go back to our simulations
### I think we will need to have both an urban effect and a method effect for this to work
simsdat <- gddfunc("urban", "ws", -30, 10, 0, 10, 300, 20, 5, 5, 20, 0.5, 15, 0.5, -10, 5)

bball <- simsdat[[1]]
clim <- simsdat[[2]]

cols <-viridis_pal(option="viridis")(3)

      use.urban <- "urban"
      bball$treatmenttype <- if(use.urban=="urban"){ifelse(bball$site=="arb", 1, 0)}else if(use.urban=="prov"){
        as.numeric(bball$prov)}
      
      datalist.gdd <- with(bball, 
                           list(y = gdd, 
                                urban = treatmenttype,
                                method = type,
                                sp = as.numeric(as.factor(species)),
                                N = nrow(bball),
                                n_sp = length(unique(bball$species))
                           )
      )
      
      urbmethod_fake = stan('stan/urbanmethod_normal_ncp_inter.stan', data = datalist.gdd,
                             iter = 3000, warmup=2500, chains=4)#, control=list(adapt_delta=0.99, max_treedepth=15))
      
      
      my.pal <-rep(viridis_pal(option="viridis")(9),2)
      my.pch <- rep(15:18, each=10)
      alphahere = 0.4
      
      modoutput <- summary(urbmethod_fake)$summary
      noncps <- modoutput[!grepl("_ncp", rownames(modoutput)),]
      labs <- if(use.urban=="urban"){c("Site", "Method", "Site x Method",
                                       "Sigma Site", "Sigma Method", 
                                       "Sigma Interaction")}else if(use.urban=="prov"){
                                         c("Provenance", "Method", "Provenance x\nMethod",
                                           "Sigma Provenance", "Sigma \nMethod", 
                                           "Sigma Interaction")}
      
      modelhere <- urbmethod_fake
      spnum <- length(unique(bball$species))

  pdf("figures/muplot_urbws.pdf", width=7, height=4)
      par(xpd=FALSE)
      par(mar=c(5,10,3,10))
      plot(x=NULL,y=NULL, xlim=c(-100,100), yaxt='n', ylim=c(0,6),
           xlab="Model estimate change in growing degree days to budburst", ylab="")
      axis(2, at=1:6, labels=rev(labs), las=1)
      abline(v=0, lty=2, col="darkgrey")
      rownameshere <- c("mu_b_urban_sp", "mu_b_method_sp", "mu_b_um_sp", "sigma_b_urban_sp",
                        "sigma_b_method_sp", "sigma_b_um_sp")
      for(i in 1:6){ #i=6
        pos.y<-(6:1)[i]
        pos.x<-noncps[rownameshere[i],"mean"]
        lines(noncps[rownameshere[i],c("2.5%","97.5%")],rep(pos.y,2),col="lightgrey")
        lines(noncps[rownameshere[i],c("25%","75%")],rep(pos.y,2),col="darkgrey")
        points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
        for(spsi in 1:spnum){
          pos.sps.i<-which(grepl(paste0("[",spsi,"]"),rownames(noncps),fixed=TRUE))[c(2:4)]
          jitt<-(spsi/40) + 0.08
          pos.y.sps.i<-pos.y-jitt
          pos.x.sps.i<-noncps[pos.sps.i[i],"mean"]
          lines(noncps[pos.sps.i[i],c("2.5%","97.5%")],rep(pos.y.sps.i,2),
                col=alpha(my.pal[spsi], alphahere-0.2))
          lines(noncps[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
                col=alpha(my.pal[spsi], alphahere))
          points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
          
        }
      }
  par(xpd=TRUE) 
  dev.off()
  save(urbmethod_fake, file="~/Documents/git/microclimates/analyses/stan/urbmethod_sims.Rdata")
  
  
  
  ##########################################################################################################
  ##########################################################################################################
  ######################################### For the Supp ###################################################
  ##########################################################################################################
  ##########################################################################################################
  #### Next, we are interested in testing the effect of provenance. Our hypothesis is that individuals from 
  # higher provenances will require fewer GDDs 
  
  simsdat <- bbfunc("prov", "NA", -5, 0.5, 300, 20, 10, 3, 0)
  
  cols <-viridis_pal(option="viridis")(3)
  
  bball <- simsdat[[1]]
  clim <- simsdat[[2]]
  
  use.urban <- "prov"
  bball$prov.z <- (bball$provenance-mean(bball$provenance, na.rm=TRUE))/(2*sd(bball$provenance,na.rm=TRUE))
  
  datalist.gdd <- with(bball, 
                       list(y = gdd, 
                            urban = prov.z,
                            method = type,
                            sp = as.numeric(as.factor(species)),
                            N = nrow(bball),
                            n_sp = length(unique(bball$species))
                       )
  )
  
  provmethod_fake = stan('stan/urbanmethod_normal_ncp_inter.stan', data = datalist.gdd,
                         iter = 3000, warmup=2500, chains=4, control=list(adapt_delta=0.99, max_treedepth=15))
  
  
  my.pal <-rep(viridis_pal(option="viridis")(9),2)
  my.pch <- rep(15:18, each=10)
  alphahere = 0.4
  
  modoutput <- summary(provmethod_fake)$summary
  noncps <- modoutput[!grepl("_ncp", rownames(modoutput)),]
  labs <- if(use.urban=="urban"){c("Site", "Method", "Site x Method",
                                   "Sigma Site", "Sigma Method", 
                                   "Sigma Interaction")}else if(use.urban=="prov"){
                                     c("Provenance", "Method", "Provenance x\nMethod",
                                       "Sigma Provenance", "Sigma \nMethod", 
                                       "Sigma Interaction")}
  
  modelhere <- provmethod_fake
  spnum <- length(unique(bball$species))
  
  pdf("figures/muplot_prov.pdf", width=7, height=4)
  par(xpd=FALSE)
  par(mar=c(5,10,3,10))
  plot(x=NULL,y=NULL, xlim=c(-30,30), yaxt='n', ylim=c(0,6),
       xlab="Model estimate change in growing degree days to budburst", ylab="")
  axis(2, at=1:6, labels=rev(labs), las=1)
  abline(v=0, lty=2, col="darkgrey")
  rownameshere <- c("mu_b_urban_sp", "mu_b_method_sp", "mu_b_um_sp", "sigma_b_urban_sp",
                    "sigma_b_method_sp", "sigma_b_um_sp")
  for(i in 1:6){ #i=6
    pos.y<-(6:1)[i]
    pos.x<-noncps[rownameshere[i],"mean"]
    lines(noncps[rownameshere[i],c("2.5%","97.5%")],rep(pos.y,2),col="lightgrey")
    lines(noncps[rownameshere[i],c("25%","75%")],rep(pos.y,2),col="darkgrey")
    points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
    for(spsi in 1:spnum){
      pos.sps.i<-which(grepl(paste0("[",spsi,"]"),rownames(noncps),fixed=TRUE))[c(2:4)]
      jitt<-(spsi/40) + 0.08
      pos.y.sps.i<-pos.y-jitt
      pos.x.sps.i<-noncps[pos.sps.i[i],"mean"]
      lines(noncps[pos.sps.i[i],c("2.5%","97.5%")],rep(pos.y.sps.i,2),
            col=alpha(my.pal[spsi], alphahere-0.2))
      lines(noncps[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
            col=alpha(my.pal[spsi], alphahere))
      points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
      
    }
  }
  par(xpd=TRUE)
  dev.off()
  save(provmethod_fake, file="~/Documents/git/microclimates/analyses/stan/prov_sims.Rdata")

