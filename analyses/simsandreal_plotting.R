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

df <- read.csv("output/cleanmicro_gdd_2019.csv")


###### Okay, we'll first start with the first hypothesis: Weather stations are less accurate measures of the same weather than hobo loggers.
## For the bbfunc(), we need to input a lot of information.
# 1. What is our question? Are we looking at the method effect? This would be "hobo" or the urban effect which is "urban" or finally the provenance effect which is "prov"
# 2. What parameter are we intersted in? This can be "ws" or "hobo" if we select "hobo" as our question
# 3. What is the mu effect of our question? 
# 4. What is the sigma of our question?
# 5. What is the GDD threshold or fstar?
# 6. What is the sigma fstar?
# 7. Mean arboretum temperature
# 8. Sigma arboretum temperature
# 9. Microclimatatic effect mean (so what is added to the mean temperature at the arboretum)
# 10. Sigma of microclimatic effect
# 11. Mean Harvard Forest temperature
# 12. Sigma Harvard Forest temperature
# 13. Microclimatatic effect mean (so what is added to the mean temperature at the forest)
# 14. Sigma of microclimatic effect

simsdat <- bbfunc("hobo", "ws", 0, 10, 300, 50, 11, 4, 1, 0, 9, 2, -1, 0)

  bball <- simsdat[[1]]
  xtext <- seq(0.5, 1, by=0.5)
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


pdf("figures/gddsites_noisyws.pdf", width=8, height=4)
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

if(FALSE){
  ws <- ggplot(clim[(clim$method=="ws"),], aes(x=tmean)) + geom_histogram(aes(fill=site)) + theme_classic() +
    scale_fill_manual(name="Site", values=cols, labels=sort(unique(clim$site))) + ggtitle("Weather Station") +
    coord_cartesian(xlim=c(-10, 25)) + xlab("Mean Temp (C)") + ylab("")
  
  hl <- ggplot(clim[(clim$method=="hobo"),], aes(x=tmean)) + geom_histogram(aes(fill=site)) + theme_classic() +
    scale_fill_manual(name="Site", values=cols, labels=sort(unique(clim$site))) + ggtitle("Hobo Logger") +
    coord_cartesian(xlim=c(-10, 25)) + xlab("Mean Temp (C)") + ylab("")
  grid.arrange(ws, hl, ncol=2)
}


if(FALSE){
  cols <-viridis_pal(option="plasma")(3)
  ggplot(bball, aes(x=bb)) + geom_histogram(aes(fill=site)) + theme_classic() + theme(legend.position = "none") +
    scale_fill_manual(name="Site", values=cols, labels=sort(unique(bball$site))) +
    coord_cartesian(xlim=c(0, 100)) + xlab("Day of budburst (C)") + ylab("") +
    geom_text(label=paste0("Arb obs:",nrow(bball[bball$site=="arb",])), col=cols[[1]], aes(x = 80, y = 100)) +
    geom_text(label=paste0("Arb NAs:",nrow(bball[is.na(bball$site=="arb"),])), col=cols[[1]], aes(x = 79, y = 90)) +
    geom_text(label=paste0("HF obs:",nrow(bball[bball$site=="hf",])), col=cols[[2]], aes(x = 80, y = 80)) +
    geom_text(label=paste0("HF NAs:",nrow(bball[is.na(bball$site=="hf"),])), col=cols[[2]], aes(x = 79, y = 70)) 
}


  bball$methodtype <- ifelse(bball$method=="ws", "\nWeather \nStation", "\nHobo \nLogger")
  
  cols <- viridis_pal(option="plasma")(3)
  gddcomparebb <- ggplot(bball, aes(x=methodtype, y=gdd, group=as.factor(site), fill=as.factor(site))) + 
    geom_ribbon(stat='smooth', method = "lm", se=TRUE, alpha=1, 
                aes(fill = as.factor(site), group = as.factor(site))) +
    geom_line(stat='smooth', method = "lm", alpha=1, col="black") +
    theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
          legend.text.align = 0,
          legend.key = element_rect(colour = "transparent", fill = "white"),
          plot.margin = margin(0.5, 0.5, 0.5, 1, "cm")) +
    xlab("") + 
    ylab("Growing degree days to budburst") + 
    scale_fill_manual(name="Site", values=cols,
                      labels=c("Arnold Arboretum", "Harvard Forest")) + 
    coord_cartesian(expand=0, ylim=c(0,700))
pdf("figures/gdd_interaction_noisyws.pdf", width=8, height=4)
  gddcomparebb
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
    
    
    cols <- adjustcolor("indianred3", alpha.f = 0.3) 
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
    
 
##### Now, let's check out the simulations if we have a noisy hobo logger versus the weather station
## I will keep the parameters the exact same..
    
    simsdat <- bbfunc("hobo", "hobo", 0, 20, 300, 50, 11, 4, 1, 0, 9, 2, -1, 0)
    
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
    
    
    pdf("figures/gddsites_noisyhobo.pdf", width=8, height=4)
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
    
    ### One thing I noticed is that the black dot, is consistently below the mean estimate of the weather station but varies strongly from the hobo logger...
    ## That consistent number relates the mean temperature, I think... let's see if we can come up with an equation
    bball$meangdd <- ave(bball$gdd, bball$species, bball$method)
    bball$mingdd <- ave(bball$gdd, bball$species, bball$method, FUN=min)
    
    bball$mingdd_ws <- NA
    bball$mingdd_hobo <- NA
    min_ws <- vector()
    min_hobo <- vector()
    for(i in 1:length(unique(bball$species))){ #i=2
      min_ws <- bball$mingdd[bball$method=="ws" & bball$species==i]
      bball$mingdd_ws[bball$species==i] <- min_ws
      
      min_hobo <- bball$mingdd[bball$method=="hobo" & bball$species==i]
      bball$mingdd_hobo[bball$species==i] <- min_hobo
    }
    
    bball$meangdd_ws <- NA
    bball$meangdd_hobo <- NA
    mean_ws <- vector()
    mean_hobo <- vector()
    for(i in 1:length(unique(bball$species))){ #i=2
      mean_ws <- bball$meangdd[bball$method=="ws" & bball$species==i]
      bball$meangdd_ws[bball$species==i] <- mean_ws
      
      mean_hobo <- bball$meangdd[bball$method=="hobo" & bball$species==i]
      bball$meangdd_hobo[bball$species==i] <- mean_hobo
    }
  
    fstarrawfunc <- function(df){
      
      hoboaccuracy <- max(df$meangdd_ws - df$mingdd_hobo) - min(df$meangdd_ws - df$mingdd_hobo)
      wsaccuracy <- max(df$meangdd_hobo - df$mingdd_ws) - min(df$meangdd_hobo - df$mingdd_ws)
      
      methodcheck <- hoboaccuracy - wsaccuracy
      
      if(methodcheck<0){
        df$fstarspp_raw <- df$mingdd_hbbo
      } else{
        df$fstarspp_raw <- df$mingdd_ws
      }
      return(df)
    }
    
    bball <- fstarrawfunc(bball)
      
    bball$gdd_accuracy_raw <- bball$gdd - bball$fstarspp_raw
    pdf("figures/gddaccuracy_raw_noisyhobo.pdf", width=6, height=4)
    plot(as.numeric(as.factor(bball$type)), 
         as.numeric(bball$gdd_accuracy_raw), col=cols[as.factor(bball$method)], 
         ylab="Raw GDD accuracy", xaxt="none",xlab="")
    axis(side=1, at=xtext, labels = c("Hobo Logger", "Weather Station"))
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
      #geom_point(aes(x=(species+0.1), y=mingdd_ws), col="red") +
      guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)
    pdf("figures/gddmethod_noisyhobo_boxplot_nominws.pdf", width=10, height=6)
    gddbox
    dev.off()
    
    
    if(FALSE){
    bball$methodtype <- ifelse(bball$method=="ws", "\nWeather \nStation", "\nHobo \nLogger")
    
    cols <- viridis_pal(option="plasma")(3)
    gddcomparebb <- ggplot(bball, aes(x=methodtype, y=gdd, group=as.factor(site), fill=as.factor(site))) + 
      geom_ribbon(stat='smooth', method = "lm", se=TRUE, alpha=1, 
                  aes(fill = as.factor(site), group = as.factor(site))) +
      geom_line(stat='smooth', method = "lm", alpha=1, col="black") +
      theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
            legend.text.align = 0,
            legend.key = element_rect(colour = "transparent", fill = "white"),
            plot.margin = margin(0.5, 0.5, 0.5, 1, "cm")) +
      xlab("") + 
      ylab("Growing degree days to budburst") + 
      scale_fill_manual(name="Site", values=cols,
                        labels=c("Arnold Arboretum", "Harvard Forest")) + 
      coord_cartesian(expand=0, ylim=c(0,700))
    pdf("figures/gdd_interaction_noisyhobo.pdf", width=8, height=4)
    gddcomparebb
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
    
    
    cols <- adjustcolor("indianred3", alpha.f = 0.3) 
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

