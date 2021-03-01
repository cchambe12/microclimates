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
                          iter = 1000, warmup=500, chains=4)
    
    
    cols <- adjustcolor("indianred3", alpha.f = 0.3) 
    my.pal <-rep(viridis_pal(option="viridis")(9),2)
    my.pch <- rep(15:18, each=10)
    alphahere = 0.4
    
    modoutput <- summary(urbmethod_fake)$summary
    noncps <- modoutput[!grepl("_ncp", rownames(modoutput)),]
    use.urban <- use.urban()[1]
    labs <- if(use.urban=="urban"){c("Arboretum", "Weather Station", "Arboretum x\nWeather Station",
                                     "Sigma Arboretum", "Sigma \nWeather Station", 
                                     "Sigma Interaction")}else if(use.urban=="prov"){
                                       c("Provenance", "Weather Station", "Provenance x\nWeather Station",
                                         "Sigma Provenance", "Sigma \nWeather Station", 
                                         "Sigma Interaction")}
    
    modelhere <- urbmethod_fake
    spnum <- length(unique(bball$species))

pdf("figures/muplot_noisyws.pdf", width=8, height=4)
    par(xpd=FALSE)
    par(mar=c(5,10,3,10))
    plot(x=NULL,y=NULL, xlim=c(-100,100), yaxt='n', ylim=c(0,6),
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
        pos.sps.i<-which(grepl(paste0("[",spsi,"]"),rownames(noncps),fixed=TRUE))[2:4]
        jitt<-(spsi/40) + 0.08
        pos.y.sps.i<-pos.y-jitt
        pos.x.sps.i<-noncps[pos.sps.i[i],"mean"]
        lines(noncps[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
              col=alpha(my.pal[spsi], alphahere))
        points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
        
      }
    }
    par(xpd=TRUE) # so I can plot legend outside
    legend(120, 6, sort(unique(gsub("_", " ", bball$species))), pch=my.pch[1:spnum],
           col=alpha(my.pal[1:spnum], alphahere),
           cex=1, bty="n", text.font=3)
dev.off()

use.sims <- eventReactive(input$simsrunmod,{if(input$Question=="Urban Model"){"urban"}else if(input$Question=="Provenance Latitude Model"){"prov"}
})

observeEvent(input$simsrunmod, {
  output$simsmuplot <- renderPlot(height=650,{
    use.sims <- use.sims()[1]
    bball <- get.datasims()[[1]]
    bball$treatmenttype <- if(use.sims=="urban"){ifelse(bball$site=="arb", 1, 0)}else if(use.sims=="prov"){
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
    
    
    progress <- Progress$new(max=10)
    on.exit(progress$close())
    
    progress$set(message = "Running rStan Model", detail="\nThis may take a while...")
    
    urbmethod_fake = stan('~/Documents/git/microclimates/analyses/stan/urbanmethod_normal_ncp_inter.stan', data = datalist.gdd,
                          iter = 1000, warmup=500, chains=4)#, control=list(adapt_delta=0.99, max_treedepth=15)) ### 
    
    
    
    
    #})
    
    cols <- adjustcolor("indianred3", alpha.f = 0.3) 
    my.pal <-rep(viridis_pal(option="viridis")(9),2)
    my.pch <- rep(15:18, each=10)
    alphahere = 0.4
    
    modoutput <- summary(urbmethod_fake)$summary
    noncps <- modoutput[!grepl("_ncp", rownames(modoutput)),]
    use.sims <- use.sims()[1]
    labs <- if(use.sims=="urban"){c("Arboretum", "Weather Station", "Arboretum x\nWeather Station",
                                    "Sigma Arboretum", "Sigma \nWeather Station", 
                                    "Sigma Interaction")}else if(use.sims=="prov"){
                                      c("Provenance", "Weather Station", "Provenance x\nWeather Station",
                                        "Sigma Provenance", "Sigma \nWeather Station", 
                                        "Sigma Interaction")}
    
    modelhere <- urbmethod_fake
    bball <- isolate(get.datasims()[[1]])
    spnum <- length(unique(bball$species))
    par(xpd=FALSE)
    par(mar=c(5,10,3,10))
    plot(x=NULL,y=NULL, xlim=c(-100,100), yaxt='n', ylim=c(0,6),
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
        pos.sps.i<-which(grepl(paste0("[",spsi,"]"),rownames(noncps),fixed=TRUE))[2:4]
        jitt<-(spsi/40) + 0.08
        pos.y.sps.i<-pos.y-jitt
        pos.x.sps.i<-noncps[pos.sps.i[i],"mean"]
        lines(noncps[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
              col=alpha(my.pal[spsi], alphahere))
        points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
        
      }
    }
    par(xpd=TRUE) # so I can plot legend outside
    legend(120, 6, sort(unique(gsub("_", " ", bball$species))), pch=my.pch[1:spnum],
           col=alpha(my.pal[1:spnum], alphahere),
           cex=1, bty="n", text.font=3)
  })
})

use.real <- eventReactive(input$realrunmod,{if(input$type=="Urban Model"){"urban"}else if(input$type=="Provenance Latitude Model"){"prov"}
})

observeEvent(input$realrunmod, {
  output$realmuplot <- renderPlot(height=650, width=750,{
    use.real <- use.real()[1]
    bball <- get.datareal
    bball$treatmenttype <- if(use.real=="urban"){as.numeric(bball$urban)}else if(use.real=="prov"){
      as.numeric(bball$provenance)}
    
    datalist.gdd <- with(bball, 
                         list(y = gdd, 
                              urban = treatmenttype,
                              method = method,
                              sp = as.numeric(as.factor(spp)),
                              N = nrow(bball),
                              n_sp = length(unique(bball$spp))
                         )
    )
    
    
    progress <- Progress$new(max=10)
    on.exit(progress$close())
    
    progress$set(message = "Running rStan Model", 
                 detail="This may take a while...")
    
    urbmethod_fake = stan('~/Documents/git/microclimates/analyses/stan/urbanmethod_normal_ncp_inter.stan', data = datalist.gdd,
                          iter = 1000, warmup=500, chains=4)#, control=list(adapt_delta=0.99, max_treedepth=15)) ### 
    
    
    
    
    #})
    
    cols <- adjustcolor("indianred3", alpha.f = 0.3) 
    my.pal <-rep(viridis_pal(option="viridis")(9),2)
    my.pch <- rep(15:18, each=10)
    alphahere = 0.4
    
    modoutput <- summary(urbmethod_fake)$summary
    noncps <- modoutput[!grepl("_ncp", rownames(modoutput)),]
    use.real <- use.real()[1]
    labs <- if(use.real=="urban"){c("Arboretum", "Weather Station", "Arboretum x\nWeather Station",
                                    "Sigma Arboretum", "Sigma \nWeather Station", 
                                    "Sigma Interaction")}else if(use.real=="prov"){
                                      c("Provenance", "Weather Station", "Provenance x\nWeather Station",
                                        "Sigma Provenance", "Sigma \nWeather Station", 
                                        "Sigma Interaction")}
    
    modelhere <- urbmethod_fake
    bball <- get.datareal
    spnum <- length(unique(bball$spp))
    par(xpd=FALSE)
    par(mar=c(5,10,3,10))
    plot(x=NULL,y=NULL, xlim=c(-100,100), yaxt='n', ylim=c(0,6),
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
        pos.sps.i<-which(grepl(paste0("[",spsi,"]"),rownames(noncps),fixed=TRUE))[2:4]
        jitt<-(spsi/40) + 0.08
        pos.y.sps.i<-pos.y-jitt
        pos.x.sps.i<-noncps[pos.sps.i[i],"mean"]
        lines(noncps[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
              col=alpha(my.pal[spsi], alphahere))
        points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
        
      }
    }
    par(xpd=TRUE) # so I can plot legend outside
    legend(120, 6, sort(unique(gsub("_", " ", bball$spp))), pch=my.pch[1:spnum],
           col=alpha(my.pal[1:spnum], alphahere),
           cex=1, bty="n", text.font=3)
  })
})




