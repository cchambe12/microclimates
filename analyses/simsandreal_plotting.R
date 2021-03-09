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
source("source/sims_params_sourcedata.R")
source("source/sims_warm_sourcedata.R")
source("source/fstarraw_accuracy.R")

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

simsdat <- bbfunc("hobo", "ws", 0, 10, 300, 50, 10, 3, 0)

  bball <- simsdat[[1]]
  xtext <- seq(1, 2, by=1)
  cols <-viridis_pal(option="viridis")(3)
  
pdf("figures/gddaccuracy_noisyws.pdf", width=6, height=4)
  plot(as.numeric(as.factor(bball$type)), as.numeric(bball$gdd_accuracy), 
       col=cols[as.factor(bball$method)], ylab="GDD accuracy", xaxt="none",xlab="")
  axis(side=1, at=xtext, labels = c("Hobo Logger", "Weather Station"))
  legend(0, -20, sort(unique(gsub("_", " ", bball$method))), pch=19,
         col=cols[as.factor(bball$method)],
         cex=1, bty="n")
dev.off()


pdf("figures/gddaccuracy_noisyws_sites.pdf", width=6, height=4)
  plot(as.numeric(as.factor(bball$site)), as.numeric(bball$gdd_accuracy), 
       col=cols[as.factor(bball$site)], xlab="", ylab="GDD accuracy", xaxt="none")
  axis(side=1, at=xtext, labels = c("Arnold Arboretum", "Harvard Forest"))
  legend(0, -20, sort(unique(gsub("_", " ", bball$site))), pch=19,
         col=cols[as.factor(bball$site)],
         cex=1, bty="n")
dev.off()

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
                          iter = 4000, warmup=3500, chains=4)#, control=list(adapt_delta=0.99, max_treedepth=15))
    
    
    my.pal <-rep(viridis_pal(option="viridis")(9),2)
    my.pch <- rep(15:18, each=10)
    alphahere = 0.4
    
    modoutput <- summary(urbmethod_fake)$summary
    noncps <- modoutput[!grepl("_ncp", rownames(modoutput)),]
    labs <- if(use.urban=="urban"){c("Arboretum", "Weather Station", "Arboretum x\nWeather Station",
                                     "Sigma Arboretum", "Sigma \nWeather Station", 
                                     "Sigma Interaction")}else if(use.urban=="prov"){
                                       c("Provenance", "Weather Station", "Provenance x\nWeather Station",
                                         "Sigma Provenance", "Sigma \nWeather Station", 
                                         "Sigma Interaction")}
    
    modelhere <- urbmethod_fake
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
      lines(noncps[rownameshere[i],c("25%","75%")],rep(pos.y,2),col="darkgrey")
      points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
      for(spsi in 1:spnum){
        pos.sps.i<-which(grepl(paste0("[",spsi,"]"),rownames(noncps),fixed=TRUE))[c(3,2,4)]
        jitt<-(spsi/40) + 0.08
        pos.y.sps.i<-pos.y-jitt
        pos.x.sps.i<-noncps[pos.sps.i[i],"mean"]
        lines(noncps[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
              col=alpha(my.pal[spsi], alphahere))
        points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
        
      }
    }
    par(xpd=TRUE) # so I can plot legend outside
    #legend(120, 6, sort(unique(gsub("_", " ", bball$species))), pch=my.pch[1:spnum],
    #      col=alpha(my.pal[1:spnum], alphahere),
    #     cex=1, bty="n", text.font=3)
    dev.off()

##########################################################################################################    
##########################################################################################################    
##########################################################################################################
##### Now, let's check out the simulations if we have a noisy hobo logger versus the weather station
## I will keep the parameters the exact same..
    
    simsdat <- bbfunc("hobo", "hobo", 0, 20, 300, 50, 10, 3, 0)
    
    bball <- simsdat[[1]]
    xtext <- seq(1, 2, by=1)
    cols <-viridis_pal(option="viridis")(3)
    
    pdf("figures/gddaccuracy_noisyhobo.pdf", width=6, height=4)
    plot(as.numeric(as.factor(bball$type)), as.numeric(bball$gdd_accuracy), 
         col=cols[as.factor(bball$method)], ylab="GDD accuracy", xaxt="none",xlab="")
    axis(side=1, at=xtext, labels = c("Hobo Logger", "Weather Station"))
    legend(0, -20, sort(unique(gsub("_", " ", bball$method))), pch=19,
           col=cols[as.factor(bball$method)],
           cex=1, bty="n")
    dev.off()
    
    
    pdf("figures/gddaccuracy_noisyhobo_sites.pdf", width=6, height=4)
    plot(as.numeric(as.factor(bball$site)), as.numeric(bball$gdd_accuracy), 
         col=cols[as.factor(bball$site)], xlab="", ylab="GDD accuracy", xaxt="none")
    axis(side=1, at=xtext, labels = c("Arnold Arboretum", "Harvard Forest"))
    legend(0, -20, sort(unique(gsub("_", " ", bball$site))), pch=19,
           col=cols[as.factor(bball$site)],
           cex=1, bty="n")
    dev.off()
    
    
    ### One thing I noticed is that the black dot, is consistently below the mean estimate of the weather station but varies strongly from the hobo logger...
    ## That consistent number relates the mean temperature, I think... let's see if we can come up with an equation
    
    bball <- fstarrawfunc(bball)
      
    pdf("figures/gddaccuracy_raw_noisyhobo.pdf", width=6, height=4)
    plot(as.numeric(as.factor(bball$type)), 
         as.numeric(bball$gdd_accuracy_raw), col=cols[as.factor(bball$method)], 
         ylab="Raw GDD accuracy", xaxt="none",xlab="")
    axis(side=1, at=xtext, labels = c("Hobo Logger", "Weather Station"))
    dev.off()
    
    pdf("figures/gddaccuracy_raw_noisyhobo_sites.pdf", width=6, height=4)
    plot(as.numeric(as.factor(bball$site)), 
         as.numeric(bball$gdd_accuracy_raw), col=cols[as.factor(bball$site)], 
         ylab="Raw GDD accuracy", xaxt="none",xlab="")
    axis(side=1, at=xtext, labels = c("Arboretum", "Harvard Forest"))
    dev.off()
    
    
    spcols <-viridis_pal(option="viridis")(15)
    gddbox<- ggplot(bball, aes(x=species, y=gdd, alpha=method)) + geom_boxplot(aes(alpha=as.factor(method), fill=as.factor(species), col=as.factor(species))) +
      scale_fill_manual(name="Species", values=spcols, labels=sort(unique(bball$species))) + 
      scale_color_manual(name="Species", values=spcols, labels=sort(unique(bball$species))) +  
      scale_y_continuous(expand = c(0, 0)) +
      theme_classic() + xlab("") +
      ylab("Growing Degree Days") +# coord_cartesian(ylim=c(50,165)) + 
      scale_alpha_manual(name="Method", values=c(0.2, 0.7),
                         labels=c("hobo"="Hobo Logger", "ws"="Weather Station")) +
      geom_point(aes(x=species, y=fstarspp), col="black") +
      geom_point(aes(x=(species+0.1), y=fstarspp_raw), col="red") +
      guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)
    pdf("figures/gddmethod_noisyhobo_boxplot_nominws.pdf", width=10, height=6)
    gddbox
    dev.off()
    
    if(FALSE){
    gddbox_sites<- ggplot(bball, aes(x=species, y=gdd, alpha=site)) + geom_boxplot(aes(alpha=as.factor(site), fill=as.factor(species), col=as.factor(species))) +
      scale_fill_manual(name="Species", values=spcols, labels=sort(unique(bball$species))) + 
      scale_color_manual(name="Species", values=spcols, labels=sort(unique(bball$species))) +  
      scale_y_continuous(expand = c(0, 0)) +
      theme_classic() + xlab("") +
      ylab("Growing Degree Days") +# coord_cartesian(ylim=c(50,165)) + 
      scale_alpha_manual(name="Site", values=c(0.2, 0.7),
                         labels=c("arb"="Arnold Arboretum", "hf"="Harvard Forest")) +
      geom_point(aes(x=species, y=fstarspp), col="black") +
      geom_point(aes(x=(species+0.1), y=fstarspp_raw), col="red") +
      guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)
    pdf("figures/gddsite_noisyhobo_boxplot.pdf", width=10, height=6)
    gddbox_sites
    dev.off()
    }
    
    
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
                          iter = 4000, warmup=3500, chains=4)#, control=list(adapt_delta=0.99, max_treedepth=15))
    
    
    my.pal <-rep(viridis_pal(option="viridis")(9),2)
    my.pch <- rep(15:18, each=10)
    alphahere = 0.4
    
    modoutput <- summary(urbmethod_fake)$summary
    noncps <- modoutput[!grepl("_ncp", rownames(modoutput)),]
    labs <- if(use.urban=="urban"){c("Arboretum", "Weather Station", "Arboretum x\nWeather Station",
                                     "Sigma Arboretum", "Sigma \nWeather Station", 
                                     "Sigma Interaction")}else if(use.urban=="prov"){
                                       c("Provenance", "Weather Station", "Provenance x\nWeather Station",
                                         "Sigma Provenance", "Sigma \nWeather Station", 
                                         "Sigma Interaction")}
    
    modelhere <- urbmethod_fake
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
      lines(noncps[rownameshere[i],c("25%","75%")],rep(pos.y,2),col="darkgrey")
      points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
      for(spsi in 1:spnum){
        pos.sps.i<-which(grepl(paste0("[",spsi,"]"),rownames(noncps),fixed=TRUE))[c(3,2,4)]
        jitt<-(spsi/40) + 0.08
        pos.y.sps.i<-pos.y-jitt
        pos.x.sps.i<-noncps[pos.sps.i[i],"mean"]
        lines(noncps[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
              col=alpha(my.pal[spsi], alphahere))
        points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
        
      }
    }
    par(xpd=TRUE) # so I can plot legend outside
    #legend(120, 6, sort(unique(gsub("_", " ", bball$species))), pch=my.pch[1:spnum],
     #      col=alpha(my.pal[1:spnum], alphahere),
      #     cex=1, bty="n", text.font=3)
dev.off()

##########################################################################################################
##########################################################################################################
##########################################################################################################
##### Now, let's check out the simulations if we have a noisy weather station AND microclimates #####
## I will keep the other parameters the exact same..

simsdat <- bbfunc("hobo", "ws", 0, 20, 300, 50, 10, 3, 5)

bball <- simsdat[[1]]
xtext <- seq(1, 2, by=1)
cols <-viridis_pal(option="viridis")(3)

pdf("figures/gddaccuracy_noisyws_micros.pdf", width=6, height=4)
plot(as.numeric(as.factor(bball$type)), as.numeric(bball$gdd_accuracy), 
     col=cols[as.factor(bball$method)], ylab="GDD accuracy", xaxt="none",xlab="")
axis(side=1, at=xtext, labels = c("Hobo Logger", "Weather Station"))
legend(0, -20, sort(unique(gsub("_", " ", bball$method))), pch=19,
       col=cols[as.factor(bball$method)],
       cex=1, bty="n")
dev.off()


pdf("figures/gddaccuracy_noisyws_micros_sites.pdf", width=6, height=4)
plot(as.numeric(as.factor(bball$site)), as.numeric(bball$gdd_accuracy), 
     col=cols[as.factor(bball$site)], xlab="", ylab="GDD accuracy", xaxt="none")
axis(side=1, at=xtext, labels = c("Arnold Arboretum", "Harvard Forest"))
legend(0, -20, sort(unique(gsub("_", " ", bball$site))), pch=19,
       col=cols[as.factor(bball$site)],
       cex=1, bty="n")
dev.off()


pdf("figures/gddsites_noisyws_micros.pdf", width=8, height=4)
par(mfrow=c(1,2))
my.pal <- viridis_pal(option="magma")(20)
my.pch <- c(15:16)
plot(as.numeric(bball$gdd) ~ as.numeric(as.factor(bball$species)), col=my.pal[as.factor(bball$species)], 
     pch=my.pch[as.factor(bball$site)], data = bball[(bball$method=="ws"),], main="Weather Station",
     ylab="GDD", ylim=c(0, 600), xlab="Species")
abline(h=mean(bball$gdd[bball$method=="ws"]), lwd=3)

plot(as.numeric(gdd) ~ as.numeric(as.factor(species)), col=my.pal[as.factor(bball$species)], 
     pch=my.pch[as.factor(bball$site)], data = bball[(bball$method=="hobo"),], main="Hobo Logger",
     ylab="GDD", ylim=c(0, 600), xlab="Species")
abline(h=mean(bball$gdd[bball$method=="hobo"]), lwd=3)
dev.off()

bball <- fstarrawfunc(bball)

pdf("figures/gddaccuracy_raw_noisyws_micros.pdf", width=6, height=4)
plot(as.numeric(as.factor(bball$type)), 
     as.numeric(bball$gdd_accuracy_raw), col=cols[as.factor(bball$method)], 
     ylab="Raw GDD accuracy", xaxt="none",xlab="")
axis(side=1, at=xtext, labels = c("Hobo Logger", "Weather Station"))
dev.off()

pdf("figures/gddaccuracy_raw_noisyws_sites.pdf", width=6, height=4)
plot(as.numeric(as.factor(bball$site)), 
     as.numeric(bball$gdd_accuracy_raw), col=cols[as.factor(bball$site)], 
     ylab="Raw GDD accuracy", xaxt="none",xlab="")
axis(side=1, at=xtext, labels = c("Arboretum", "Harvard Forest"))
dev.off()


spcols <-viridis_pal(option="viridis")(15)
gddbox<- ggplot(bball, aes(x=species, y=gdd, alpha=method)) + geom_boxplot(aes(alpha=as.factor(method), fill=as.factor(species), col=as.factor(species))) +
  scale_fill_manual(name="Species", values=spcols, labels=sort(unique(bball$species))) + 
  scale_color_manual(name="Species", values=spcols, labels=sort(unique(bball$species))) +  
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() + xlab("") +
  ylab("Growing Degree Days") +# coord_cartesian(ylim=c(50,165)) + 
  scale_alpha_manual(name="Method", values=c(0.2, 0.7),
                     labels=c("hobo"="Hobo Logger", "ws"="Weather Station")) +
  geom_point(aes(x=species, y=fstarspp), col="black") +
  geom_point(aes(x=(species+0.1), y=fstarspp_raw), col="red") +
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)
pdf("figures/gddmethod_noisyws_boxplot_micros.pdf", width=10, height=6)
gddbox
dev.off()

gddbox_sites<- ggplot(bball, aes(x=species, y=gdd, alpha=site)) + geom_boxplot(aes(alpha=as.factor(site), fill=as.factor(species), col=as.factor(species))) +
  scale_fill_manual(name="Species", values=spcols, labels=sort(unique(bball$species))) + 
  scale_color_manual(name="Species", values=spcols, labels=sort(unique(bball$species))) +  
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() + xlab("") +
  ylab("Growing Degree Days") +# coord_cartesian(ylim=c(50,165)) + 
  scale_alpha_manual(name="Site", values=c(0.2, 0.7),
                     labels=c("arb"="Arnold Arboretum", "hf"="Harvard Forest")) +
  geom_point(aes(x=species, y=fstarspp), col="black") +
  geom_point(aes(x=(species+0.1), y=fstarspp_raw), col="red") +
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)
pdf("figures/gddsite_noisyws_boxplot_micros.pdf", width=10, height=6)
gddbox_sites
dev.off()


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
                      iter = 4000, warmup=3500, chains=4)#, control=list(adapt_delta=0.99, max_treedepth=15))

my.pal <-rep(viridis_pal(option="viridis")(9),2)
my.pch <- rep(15:18, each=10)
alphahere = 0.4

modoutput <- summary(urbmethod_fake)$summary
noncps <- modoutput[!grepl("_ncp", rownames(modoutput)),]
labs <- if(use.urban=="urban"){c("Arboretum", "Weather Station", "Arboretum x\nWeather Station",
                                 "Sigma Arboretum", "Sigma \nWeather Station", 
                                 "Sigma Interaction")}else if(use.urban=="prov"){
                                   c("Provenance", "Weather Station", "Provenance x\nWeather Station",
                                     "Sigma Provenance", "Sigma \nWeather Station", 
                                     "Sigma Interaction")}

modelhere <- urbmethod_fake
spnum <- length(unique(bball$species))

pdf("figures/muplot_noisyws_micros.pdf", width=7, height=4)
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
  lines(noncps[rownameshere[i],c("25%","75%")],rep(pos.y,2),col="darkgrey")
  points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
  for(spsi in 1:spnum){
    pos.sps.i<-which(grepl(paste0("[",spsi,"]"),rownames(noncps),fixed=TRUE))[c(3,2,4)]
    jitt<-(spsi/40) + 0.08
    pos.y.sps.i<-pos.y-jitt
    pos.x.sps.i<-noncps[pos.sps.i[i],"mean"]
    lines(noncps[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
          col=alpha(my.pal[spsi], alphahere))
    points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
    
  }
}
par(xpd=TRUE) # so I can plot legend outside
#legend(120, 6, sort(unique(gsub("_", " ", bball$species))), pch=my.pch[1:spnum],
#      col=alpha(my.pal[1:spnum], alphahere),
#     cex=1, bty="n", text.font=3)
dev.off()

####################################################################################################
####################################################################################################
####################################################################################################
#### Interesting!!!! So with noisy weather station data and microclimatic effects, we're seeing a positive effect of weather station on GDD
### What happens to the muplot if we have noisy hobo logger data instead?
simsdat <- bbfunc("hobo", "hobo", 0, 20, 300, 50, 10, 3, 5)

bball <- simsdat[[1]]
xtext <- seq(1, 2, by=1)
cols <-viridis_pal(option="viridis")(3)

bball <- fstarrawfunc(bball)

pdf("figures/gddaccuracy_raw_noisyhobo_micros.pdf", width=6, height=4)
plot(as.numeric(as.factor(bball$type)), 
     as.numeric(bball$gdd_accuracy_raw), col=cols[as.factor(bball$method)], 
     ylab="Raw GDD accuracy", xaxt="none",xlab="")
axis(side=1, at=xtext, labels = c("Hobo Logger", "Weather Station"))
dev.off()

pdf("figures/gddaccuracy_raw_noisyhobo_sites.pdf", width=6, height=4)
plot(as.numeric(as.factor(bball$site)), 
     as.numeric(bball$gdd_accuracy_raw), col=cols[as.factor(bball$site)], 
     ylab="Raw GDD accuracy", xaxt="none",xlab="")
axis(side=1, at=xtext, labels = c("Arboretum", "Harvard Forest"))
dev.off()


spcols <-viridis_pal(option="viridis")(15)
gddbox<- ggplot(bball, aes(x=species, y=gdd, alpha=method)) + geom_boxplot(aes(alpha=as.factor(method), fill=as.factor(species), col=as.factor(species))) +
  scale_fill_manual(name="Species", values=spcols, labels=sort(unique(bball$species))) + 
  scale_color_manual(name="Species", values=spcols, labels=sort(unique(bball$species))) +  
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() + xlab("") +
  ylab("Growing Degree Days") +# coord_cartesian(ylim=c(50,165)) + 
  scale_alpha_manual(name="Method", values=c(0.2, 0.7),
                     labels=c("hobo"="Hobo Logger", "ws"="Weather Station")) +
  geom_point(aes(x=species, y=fstarspp), col="black") +
  geom_point(aes(x=(species+0.1), y=fstarspp_raw), col="red") +
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)
pdf("figures/gddmethod_noisyhobo_boxplot_micros.pdf", width=10, height=6)
gddbox
dev.off()

gddbox_sites<- ggplot(bball, aes(x=species, y=gdd, alpha=site)) + geom_boxplot(aes(alpha=as.factor(site), fill=as.factor(species), col=as.factor(species))) +
  scale_fill_manual(name="Species", values=spcols, labels=sort(unique(bball$species))) + 
  scale_color_manual(name="Species", values=spcols, labels=sort(unique(bball$species))) +  
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() + xlab("") +
  ylab("Growing Degree Days") +# coord_cartesian(ylim=c(50,165)) + 
  scale_alpha_manual(name="Site", values=c(0.2, 0.7),
                     labels=c("arb"="Arnold Arboretum", "hf"="Harvard Forest")) +
  geom_point(aes(x=species, y=fstarspp), col="black") +
  geom_point(aes(x=(species+0.1), y=fstarspp_raw), col="red") +
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)
pdf("figures/gddsite_noisyhobo_boxplot_micros.pdf", width=10, height=6)
gddbox_sites
dev.off()

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
                      iter = 4000, warmup=3500, chains=4)#, control=list(adapt_delta=0.99, max_treedepth=15))


my.pal <-rep(viridis_pal(option="viridis")(9),2)
my.pch <- rep(15:18, each=10)
alphahere = 0.4

modoutput <- summary(urbmethod_fake)$summary
noncps <- modoutput[!grepl("_ncp", rownames(modoutput)),]
labs <- if(use.urban=="urban"){c("Arboretum", "Weather Station", "Arboretum x\nWeather Station",
                                 "Sigma Arboretum", "Sigma \nWeather Station", 
                                 "Sigma Interaction")}else if(use.urban=="prov"){
                                   c("Provenance", "Weather Station", "Provenance x\nWeather Station",
                                     "Sigma Provenance", "Sigma \nWeather Station", 
                                     "Sigma Interaction")}

modelhere <- urbmethod_fake
spnum <- length(unique(bball$species))

pdf("figures/muplot_noisyhobo_micros.pdf", width=7, height=4)
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
  lines(noncps[rownameshere[i],c("25%","75%")],rep(pos.y,2),col="darkgrey")
  points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
  for(spsi in 1:spnum){
    pos.sps.i<-which(grepl(paste0("[",spsi,"]"),rownames(noncps),fixed=TRUE))[c(3,2,4)]
    jitt<-(spsi/40) + 0.08
    pos.y.sps.i<-pos.y-jitt
    pos.x.sps.i<-noncps[pos.sps.i[i],"mean"]
    lines(noncps[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
          col=alpha(my.pal[spsi], alphahere))
    points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
    
  }
}
par(xpd=TRUE) # so I can plot legend outside
#legend(120, 6, sort(unique(gsub("_", " ", bball$species))), pch=my.pch[1:spnum],
#      col=alpha(my.pal[1:spnum], alphahere),
#     cex=1, bty="n", text.font=3)
dev.off()

####################################################################################################
####################################################################################################
####################################################################################################
#### Alright, so now we want to test if hobo data is a more accurate measure of the same weather - meaning, there might be microclimates!
## So I think the way to do this is to make sure that hobos are more accurate than the weather because they are picking up the temperature more precisely
# and then we need to add more sigma to the hobo loggers to simulate microclimates
simsdat <- bbfunc("hobo", "ws", 0, 10, 300, 50, 10, 2, 2)

bball <- simsdat[[1]]
xtext <- seq(1, 2, by=1)
cols <-viridis_pal(option="viridis")(3)

bball <- fstarrawfunc(bball)

pdf("figures/gddaccuracy_raw_micros.pdf", width=6, height=4)
plot(as.numeric(as.factor(bball$type)), 
     as.numeric(bball$gdd_accuracy_raw), col=cols[as.factor(bball$method)], 
     ylab="Raw GDD accuracy", xaxt="none",xlab="")
axis(side=1, at=xtext, labels = c("Hobo Logger", "Weather Station"))
dev.off()

pdf("figures/gddaccuracy_micros.pdf", width=6, height=4)
plot(as.numeric(as.factor(bball$type)), 
     as.numeric(bball$gdd_accuracy), col=cols[as.factor(bball$method)], 
     ylab="GDD accuracy", xaxt="none",xlab="")
axis(side=1, at=xtext, labels = c("Hobo Logger", "Weather Station"))
dev.off()

pdf("figures/gddaccuracy_raw_sites.pdf", width=6, height=4)
plot(as.numeric(as.factor(bball$site)), 
     as.numeric(bball$gdd_accuracy_raw), col=cols[as.factor(bball$site)], 
     ylab="Raw GDD accuracy", xaxt="none",xlab="")
axis(side=1, at=xtext, labels = c("Arboretum", "Harvard Forest"))
dev.off()


spcols <-viridis_pal(option="viridis")(15)
gddbox<- ggplot(bball, aes(x=species, y=gdd, alpha=method)) + geom_boxplot(aes(alpha=as.factor(method), fill=as.factor(species), col=as.factor(species))) +
  scale_fill_manual(name="Species", values=spcols, labels=sort(unique(bball$species))) + 
  scale_color_manual(name="Species", values=spcols, labels=sort(unique(bball$species))) +  
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() + xlab("") +
  ylab("Growing Degree Days") +# coord_cartesian(ylim=c(50,165)) + 
  scale_alpha_manual(name="Method", values=c(0.2, 0.7),
                     labels=c("hobo"="Hobo Logger", "ws"="Weather Station")) +
  geom_point(aes(x=species, y=fstarspp), col="black") +
  geom_point(aes(x=(species+0.1), y=fstarspp_raw), col="red") +
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)
pdf("figures/gddmethod_boxplot_micros.pdf", width=10, height=6)
gddbox
dev.off()

gddbox_sites<- ggplot(bball, aes(x=species, y=gdd, alpha=site)) + geom_boxplot(aes(alpha=as.factor(site), fill=as.factor(species), col=as.factor(species))) +
  scale_fill_manual(name="Species", values=spcols, labels=sort(unique(bball$species))) + 
  scale_color_manual(name="Species", values=spcols, labels=sort(unique(bball$species))) +  
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() + xlab("") +
  ylab("Growing Degree Days") +# coord_cartesian(ylim=c(50,165)) + 
  scale_alpha_manual(name="Site", values=c(0.2, 0.7),
                     labels=c("arb"="Arnold Arboretum", "hf"="Harvard Forest")) +
  geom_point(aes(x=species, y=fstarspp), col="black") +
  geom_point(aes(x=(species+0.1), y=fstarspp_raw), col="red") +
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)
pdf("figures/gddsite_boxplot_micros.pdf", width=10, height=6)
gddbox_sites
dev.off()

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
                      iter = 4000, warmup=3500, chains=4, control=list(adapt_delta=0.99, max_treedepth=15))


my.pal <-rep(viridis_pal(option="viridis")(9),2)
my.pch <- rep(15:18, each=10)
alphahere = 0.4

modoutput <- summary(urbmethod_fake)$summary
noncps <- modoutput[!grepl("_ncp", rownames(modoutput)),]
labs <- if(use.urban=="urban"){c("Arboretum", "Weather Station", "Arboretum x\nWeather Station",
                                 "Sigma Arboretum", "Sigma \nWeather Station", 
                                 "Sigma Interaction")}else if(use.urban=="prov"){
                                   c("Provenance", "Weather Station", "Provenance x\nWeather Station",
                                     "Sigma Provenance", "Sigma \nWeather Station", 
                                     "Sigma Interaction")}

modelhere <- urbmethod_fake
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
  lines(noncps[rownameshere[i],c("25%","75%")],rep(pos.y,2),col="darkgrey")
  points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
  for(spsi in 1:spnum){
    pos.sps.i<-which(grepl(paste0("[",spsi,"]"),rownames(noncps),fixed=TRUE))[c(3,2,4)]
    jitt<-(spsi/40) + 0.08
    pos.y.sps.i<-pos.y-jitt
    pos.x.sps.i<-noncps[pos.sps.i[i],"mean"]
    lines(noncps[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
          col=alpha(my.pal[spsi], alphahere))
    points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
    
  }
}
par(xpd=TRUE) # so I can plot legend outside
#legend(120, 6, sort(unique(gsub("_", " ", bball$species))), pch=my.pch[1:spnum],
#      col=alpha(my.pal[1:spnum], alphahere),
#     cex=1, bty="n", text.font=3)
dev.off()


####################################################################################################
####################################################################################################
####################################################################################################
#### Next, we are interested in testing the effect of provenance. Our hypothesis is that individuals from 
# higher provenances will require fewer GDDs 
simsdat <- bbfunc("prov", "NA", -10, 2, 300, 50, 10, 2, 0)

bball <- simsdat[[1]]
xtext <- seq(1, 2, by=1)
cols <-viridis_pal(option="viridis")(3)

bball <- fstarrawfunc(bball)

pdf("figures/gddaccuracy_raw_prov.pdf", width=6, height=4)
plot(as.numeric(as.factor(bball$type)), 
     as.numeric(bball$gdd_accuracy_raw), col=cols[as.factor(bball$method)], 
     ylab="Raw GDD accuracy", xaxt="none",xlab="")
axis(side=1, at=xtext, labels = c("Hobo Logger", "Weather Station"))
dev.off()

pdf("figures/gddaccuracy_prov.pdf", width=6, height=4)
plot(as.numeric(as.factor(bball$type)), 
     as.numeric(bball$gdd_accuracy), col=cols[as.factor(bball$method)], 
     ylab="GDD accuracy", xaxt="none",xlab="")
axis(side=1, at=xtext, labels = c("Hobo Logger", "Weather Station"))
dev.off()

pdf("figures/gddaccuracy_raw_prov.pdf", width=6, height=4)
plot(as.numeric(as.factor(bball$site)), 
     as.numeric(bball$gdd_accuracy_raw), col=cols[as.factor(bball$site)], 
     ylab="Raw GDD accuracy", xaxt="none",xlab="")
axis(side=1, at=xtext, labels = c("Arboretum", "Harvard Forest"))
dev.off()


spcols <-viridis_pal(option="viridis")(15)
gddbox<- ggplot(bball, aes(x=species, y=gdd, alpha=method)) + geom_boxplot(aes(alpha=as.factor(method), fill=as.factor(species), col=as.factor(species))) +
  scale_fill_manual(name="Species", values=spcols, labels=sort(unique(bball$species))) + 
  scale_color_manual(name="Species", values=spcols, labels=sort(unique(bball$species))) +  
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() + xlab("") +
  ylab("Growing Degree Days") +# coord_cartesian(ylim=c(50,165)) + 
  scale_alpha_manual(name="Method", values=c(0.2, 0.7),
                     labels=c("hobo"="Hobo Logger", "ws"="Weather Station")) +
  geom_point(aes(x=species, y=fstarspp), col="black") +
  geom_point(aes(x=(species+0.1), y=fstarspp_raw), col="red") +
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)
pdf("figures/gddmethod_boxplot_prov.pdf", width=10, height=6)
gddbox
dev.off()

gddbox_sites<- ggplot(bball, aes(x=species, y=gdd, alpha=site)) + geom_boxplot(aes(alpha=as.factor(site), fill=as.factor(species), col=as.factor(species))) +
  scale_fill_manual(name="Species", values=spcols, labels=sort(unique(bball$species))) + 
  scale_color_manual(name="Species", values=spcols, labels=sort(unique(bball$species))) +  
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() + xlab("") +
  ylab("Growing Degree Days") +# coord_cartesian(ylim=c(50,165)) + 
  scale_alpha_manual(name="Site", values=c(0.2, 0.7),
                     labels=c("arb"="Arnold Arboretum", "hf"="Harvard Forest")) +
  geom_point(aes(x=species, y=fstarspp), col="black") +
  geom_point(aes(x=(species+0.1), y=fstarspp_raw), col="red") +
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)
pdf("figures/gddsite_boxplot_prov.pdf", width=10, height=6)
gddbox_sites
dev.off()

use.urban <- "prov"
bball$treatmenttype <- if(use.urban=="urban"){ifelse(bball$site=="arb", 1, 0)}else if(use.urban=="prov"){
  as.numeric(bball$prov)}

datalist.gdd <- with(bball, 
                     list(y = gdd, 
                          urban = provenance,
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
labs <- if(use.urban=="urban"){c("Arboretum", "Weather Station", "Arboretum x\nWeather Station",
                                 "Sigma Arboretum", "Sigma \nWeather Station", 
                                 "Sigma Interaction")}else if(use.urban=="prov"){
                                   c("Provenance", "Weather Station", "Provenance x\nWeather Station",
                                     "Sigma Provenance", "Sigma \nWeather Station", 
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
  lines(noncps[rownameshere[i],c("25%","75%")],rep(pos.y,2),col="darkgrey")
  points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
  for(spsi in 1:spnum){
    pos.sps.i<-which(grepl(paste0("[",spsi,"]"),rownames(noncps),fixed=TRUE))[c(2:4)]
    jitt<-(spsi/40) + 0.08
    pos.y.sps.i<-pos.y-jitt
    pos.x.sps.i<-noncps[pos.sps.i[i],"mean"]
    lines(noncps[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
          col=alpha(my.pal[spsi], alphahere))
    points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
    
  }
}
par(xpd=TRUE) # so I can plot legend outside
#legend(120, 6, sort(unique(gsub("_", " ", bball$species))), pch=my.pch[1:spnum],
#      col=alpha(my.pal[1:spnum], alphahere),
#     cex=1, bty="n", text.font=3)
dev.off()


if(FALSE){
  ws <- ggplot(bball[(bball$method=="ws"),], aes(x=gdd)) + geom_histogram(aes(fill=site)) + theme_classic() +
  scale_fill_manual(name="Site", values=cols, labels=sort(unique(bball$site))) + ggtitle("Weather Station") +
  #coord_cartesian(xlim=c(-10, 25)) + 
  xlab("GDD") + ylab("")

hl <- ggplot(bball[(bball$method=="hobo"),], aes(x=gdd)) + geom_histogram(aes(fill=site)) + theme_classic() +
  scale_fill_manual(name="Site", values=cols, labels=sort(unique(bball$site))) + ggtitle("Hobo Logger") +
  #coord_cartesian(xlim=c(-10, 25)) + 
  xlab("GDD") + ylab("")
egg::ggarrange(ws, hl)


arb <- ggplot(bball[(bball$site=="arb"),], aes(x=gdd)) + geom_histogram(aes(fill=method)) + theme_classic() +
  scale_fill_manual(name="Method", values=cols, labels=sort(unique(bball$method))) + ggtitle("Arboretum") +
  #coord_cartesian(xlim=c(-10, 25)) + 
  xlab("GDD") + ylab("")
hf <- ggplot(bball[(bball$site=="hf"),], aes(x=gdd)) + geom_histogram(aes(fill=method)) + theme_classic() +
  scale_fill_manual(name="Method", values=cols, labels=sort(unique(bball$method))) + ggtitle("Harvard Forest") +
  #coord_cartesian(xlim=c(-10, 25)) + 
  xlab("GDD") + ylab("")
egg::ggarrange(arb, hf)

brms::brm(gdd ~ provenance + type + (provenance + type|species), data=bball)
}

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

xtext <- seq(1, 2, by=1)
cols <-viridis_pal(option="viridis")(3)

ws <- ggplot(bball[(bball$method=="ws"),], aes(x=gdd)) + geom_histogram(aes(fill=site), alpha=0.3) + theme_classic() +
  scale_fill_manual(name="Site", values=cols, labels=c("Arboretum", "Harvard Forest")) + ggtitle("Weather Station") +
  coord_cartesian(xlim=c(100, 700)) + 
  geom_vline(xintercept=mean(bball$gdd[(bball$method=="ws" & bball$site=="arb")]), col=cols[[1]], linetype="dashed") +
  geom_vline(xintercept=mean(bball$gdd[(bball$method=="ws" & bball$site=="hf")]), col=cols[[2]], linetype="dashed") +
  xlab("Growing Degree Days (GDD)") + ylab("") +
  scale_y_continuous(expand = c(0, 0)) +
  #scale_x_continuous(breaks = seq(-20, 40, by=5)) +
  theme(legend.position="none")
hobo <- ggplot(bball[(bball$method=="hobo"),], aes(x=gdd)) + geom_histogram(aes(fill=site), alpha=0.3) + theme_classic() +
  scale_fill_manual(name="Site", values=cols, labels=c("Arboretum", "Harvard Forest")) + ggtitle("Hobo Logger") +
  geom_vline(xintercept=mean(bball$gdd[(bball$method=="hobo" & bball$site=="arb")]), col=cols[[1]], linetype="dashed") +
  geom_vline(xintercept=mean(bball$gdd[(bball$method=="hobo" & bball$site=="hf")]), col=cols[[2]], linetype="dashed") +
  coord_cartesian(xlim=c(100, 700)) + 
  xlab("Growing Degree Days (GDD)") + ylab("") +
  scale_y_continuous(expand = c(0, 0)) 
pdf("figures/gdd_meathods_real.pdf", width=8, height=4, onefile=FALSE)
egg::ggarrange(ws, hobo, ncol=2)
dev.off()

#bball$provgroup <- 5*round(bball$provenance/5, digits=0)
#unique(bball$provgroup) # 35, 40, 45, 55
## Maybe instead I should group by quantile: 0-25%, 25-75% and 75%+
bball$provgroup <- NA
quant0 <- quantile(bball$provenance, 0)
quant25 <- quantile(bball$provenance, 0.25)
quant75 <- quantile(bball$provenance, 0.75)
quant100 <- quantile(bball$provenance, 1)
bball$provgroup <- ifelse(bball$provenance<=quant25, "low", bball$provenance)
bball$provgroup <- ifelse(bball$provenance<=quant75 & bball$provenance>quant25, "mid", bball$provgroup)
bball$provgroup <- ifelse(bball$provenance<=quant100 & bball$provenance>quant75, "high", bball$provgroup)


provcols <- viridis_pal(option="viridis")(3)
provhobo <- ggplot(bball[(bball$method=="hobo"),], aes(x=gdd)) + geom_histogram(aes(fill=as.factor(provgroup)), alpha=0.2) + theme_classic() +
  scale_fill_manual(name="Provenance\nLatitude", values=provcols) + ggtitle("Hobo Logger") +
  coord_cartesian(xlim=c(100, 700)) + 
  geom_vline(xintercept=mean(bball$gdd[(bball$method=="hobo" & bball$provgroup=="low")]), col=provcols[[1]], linetype="dashed") +
  geom_vline(xintercept=mean(bball$gdd[(bball$method=="hobo" & bball$provgroup=="mid")]), col=provcols[[2]], linetype="dashed") +
  geom_vline(xintercept=mean(bball$gdd[(bball$method=="hobo" & bball$provgroup=="high")]), col=provcols[[3]], linetype="dashed") +
  #geom_vline(xintercept=mean(bball$gdd[(bball$method=="hobo" & bball$provgroup==55)]), col=provcols[[4]], linetype="dashed") +
  xlab("Mean Temperature (°C)") + ylab("") +
  scale_y_continuous(expand = c(0, 0)) 
provws <- ggplot(bball[(bball$method=="ws"),], aes(x=gdd)) + geom_histogram(aes(fill=as.factor(provgroup)), alpha=0.2) + theme_classic() +
  scale_fill_manual(name="Provenance\nLatitude", values=provcols) + ggtitle("Weather Station") +
  coord_cartesian(xlim=c(100, 700)) + 
  xlab("Mean Temperature (°C)") + ylab("") +
  geom_vline(xintercept=mean(bball$gdd[(bball$method=="ws" & bball$provgroup=="low")]), col=provcols[[1]], linetype="dashed") +
  geom_vline(xintercept=mean(bball$gdd[(bball$method=="ws" & bball$provgroup=="mid")]), col=provcols[[2]], linetype="dashed") +
  geom_vline(xintercept=mean(bball$gdd[(bball$method=="ws" & bball$provgroup=="high")]), col=provcols[[3]], linetype="dashed") +
  #geom_vline(xintercept=mean(bball$gdd[(bball$method=="ws" & bball$provgroup==55)]), col=provcols[[4]], linetype="dashed") +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "none")
pdf("figures/gdd_meathods_provreal.pdf", width=8, height=4, onefile=FALSE)
egg::ggarrange(provws, provhobo, ncol=2)
dev.off()


climws <- read.csv("output/clean_clim_ws.csv")
climws <- subset(climws, select=c("date", "year", "doy", "hour", "tmean", "climatetype"))
colstokeep <- colnames(climws)
climhobo <- read.csv("output/clean_clim_hobo.csv")

climhobo <- subset(climhobo, select=c(colstokeep))

clim <- rbind(climws, climhobo)
clim$method <- ifelse(clim$climatetype=="weldhill" | clim$climatetype=="harvardforest", "ws", "hobo")
clim$site <- ifelse(clim$climatetype%in%c("arb10", "arb11", "arb12", "arb13", "arb14", "arb15", 
                                          "arb2", "arb3", "arb4", "arb5", "arb7", "arb8", "arb9",  
                                          "weldhill"), "arb", "hf")

clim <- clim[(clim$year==2019),]
clim <- clim[(clim$doy<=180 & clim$doy>=44),]

ws <- ggplot(clim[(clim$method=="ws"),], aes(x=tmean)) + geom_histogram(aes(fill=site), alpha=0.3) + theme_classic() +
  scale_fill_manual(name="Site", values=cols, labels=c("Arboretum", "Harvard Forest")) + ggtitle("Weather Station") +
  coord_cartesian(xlim=c(-20, 40)) + 
  geom_vline(xintercept=mean(clim$tmean[(clim$method=="ws" & clim$site=="arb")]), col=cols[[1]], linetype="dashed") +
  geom_vline(xintercept=mean(clim$tmean[(clim$method=="ws" & clim$site=="hf")]), col=cols[[2]], linetype="dashed") +
  xlab("Mean Temperature (°C)") + ylab("") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(-20, 40, by=5)) +
  theme(legend.position="none")
hobo <- ggplot(clim[(clim$method=="hobo"),], aes(x=tmean)) + geom_histogram(aes(fill=site), alpha=0.3) + theme_classic() +
  scale_fill_manual(name="Site", values=cols, labels=c("Arboretum", "Harvard Forest")) + ggtitle("Hobo Logger") +
  geom_vline(xintercept=mean(clim$tmean[(clim$method=="hobo" & clim$site=="arb")]), col=cols[[1]], linetype="dashed") +
  geom_vline(xintercept=mean(clim$tmean[(clim$method=="hobo" & clim$site=="hf")]), col=cols[[2]], linetype="dashed") +
  coord_cartesian(xlim=c(-20, 40)) + 
  xlab("Mean Temperature (°C)") + ylab("") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(-20, 40, by=5))
pdf("figures/clim_meathods_real.pdf", width=8, height=4, onefile=FALSE)
egg::ggarrange(ws, hobo, ncol=2)
dev.off()


use.urban <- "prov"
bball$treatmenttype <- if(use.urban=="urban"){ifelse(bball$site=="arb", 1, 0)}else if(use.urban=="prov"){
  as.numeric(bball$prov)}

datalist.gdd <- with(bball, 
                     list(y = gdd, 
                          urban = provenance,
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
labs <- if(use.urban=="urban"){c("Arboretum", "Weather Station", "Arboretum x\nWeather Station",
                                 "Sigma Arboretum", "Sigma \nWeather Station", 
                                 "Sigma Interaction")}else if(use.urban=="prov"){
                                   c("Provenance", "Weather Station", "Provenance x\nWeather Station",
                                     "Sigma Provenance", "Sigma \nWeather Station", 
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
  lines(noncps[rownameshere[i],c("25%","75%")],rep(pos.y,2),col="darkgrey")
  points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
  for(spsi in 1:spnum){
    pos.sps.i<-which(grepl(paste0("[",spsi,"]"),rownames(noncps),fixed=TRUE))[c(2:4)]
    jitt<-(spsi/40) + 0.08
    pos.y.sps.i<-pos.y-jitt
    pos.x.sps.i<-noncps[pos.sps.i[i],"mean"]
    lines(noncps[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
          col=alpha(my.pal[spsi], alphahere))
    points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
    
  }
}
par(xpd=TRUE) # so I can plot legend outside
#legend(120, 6, sort(unique(gsub("_", " ", bball$species))), pch=my.pch[1:spnum],
#      col=alpha(my.pal[1:spnum], alphahere),
#     cex=1, bty="n", text.font=3)
dev.off()


bball <- fstarrawfunc(bball) ### warnings are actually okay, it is because we don't have exactly the same number of species at each site

pdf("figures/gddaccuracy_raw_real.pdf", width=6, height=4)
plot(as.numeric(as.factor(bball$type)), 
     as.numeric(bball$gdd_accuracy_raw), col=cols[as.factor(bball$method)], 
     ylab="Raw GDD accuracy", xaxt="none",xlab="")
axis(side=1, at=xtext, labels = c("Hobo Logger", "Weather Station"))
dev.off()

pdf("figures/gddaccuracy_real.pdf", width=6, height=4)
plot(as.numeric(as.factor(bball$type)), 
     as.numeric(bball$gdd_accuracy), col=cols[as.factor(bball$method)], 
     ylab="GDD accuracy", xaxt="none",xlab="")
axis(side=1, at=xtext, labels = c("Hobo Logger", "Weather Station"))
dev.off()

pdf("figures/gddaccuracy_raw_real.pdf", width=6, height=4)
plot(as.numeric(as.factor(bball$site)), 
     as.numeric(bball$gdd_accuracy_raw), col=cols[as.factor(bball$site)], 
     ylab="Raw GDD accuracy", xaxt="none",xlab="")
axis(side=1, at=xtext, labels = c("Arboretum", "Harvard Forest"))
dev.off()


spcols <-viridis_pal(option="viridis")(15)
gddbox<- ggplot(bball, aes(x=species, y=gdd, alpha=method)) + geom_boxplot(aes(alpha=as.factor(method), fill=as.factor(species), col=as.factor(species))) +
  scale_fill_manual(name="Species", values=spcols, labels=sort(unique(bball$species))) + 
  scale_color_manual(name="Species", values=spcols, labels=sort(unique(bball$species))) +  
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() + xlab("") +
  ylab("Growing Degree Days") +# coord_cartesian(ylim=c(50,165)) + 
  scale_alpha_manual(name="Method", values=c(0.2, 0.7),
                     labels=c("hobo"="Hobo Logger", "ws"="Weather Station")) +
  geom_point(aes(x=species, y=fstarspp), col="black") +
  geom_point(aes(x=(species+0.1), y=fstarspp_raw), col="red") +
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)
pdf("figures/gddmethod_boxplot_real.pdf", width=10, height=6)
gddbox
dev.off()

gddbox_sites<- ggplot(bball, aes(x=species, y=gdd, alpha=site)) + geom_boxplot(aes(alpha=as.factor(site), fill=as.factor(species), col=as.factor(species))) +
  scale_fill_manual(name="Species", values=spcols, labels=sort(unique(bball$species))) + 
  scale_color_manual(name="Species", values=spcols, labels=sort(unique(bball$species))) +  
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() + xlab("") +
  ylab("Growing Degree Days") +# coord_cartesian(ylim=c(50,165)) + 
  scale_alpha_manual(name="Site", values=c(0.2, 0.7),
                     labels=c("arb"="Arnold Arboretum", "hf"="Harvard Forest")) +
  geom_point(aes(x=species, y=fstarspp), col="black") +
  geom_point(aes(x=(species+0.1), y=fstarspp_raw), col="red") +
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)
pdf("figures/gddsite_boxplot_prov.pdf", width=10, height=6)
gddbox_sites
dev.off()
