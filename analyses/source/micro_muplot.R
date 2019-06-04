## Started 3 April 2019 ##
## By Cat - based off code from Lizzie's OSPREE plots ##

# Runs from models_stan_plotting.R #

# with COLORS for each species #

muplotfx <- function(specieshere, modelhere, nameforfig, width, height, ylim, xlim, leg1, leg2, speciesnamehere){
  spnum <- length(unique(specieshere))
  pdf(file.path(figpath, paste("", nameforfig, figpathmore, ".pdf", sep="")),
      width = width, height = height)
  par(xpd=FALSE)
  par(mar=c(5,7,3,10))
  plot(x=NULL,y=NULL, xlim=xlim, yaxt='n', ylim=ylim,
       xlab=xlab, ylab="", main=nameforfig)
  axis(2, at=1:3, labels=rev(c("Mean \nSpring Temp", "Leafout (DOY)", "Provenance \nLatitude")), las=1)
  abline(v=0, lty=2, col="darkgrey")
  rownameshere <- c("mu_b_mst_sp", "mu_b_lo_sp", "mu_b_lat_sp")
  ppeffects <- c("mu_b_mst_sp", "mu_b_lo_sp", "mu_b_lat_sp") # or 1:4 here...
  for(i in 1:3){
    pos.y<-(3:1)[i]
    pos.x<-summary(modelhere)$summary[rownameshere[i],"mean"]
    lines(summary(modelhere)$summary[rownameshere[i],c("25%","75%")],rep(pos.y,2),col="darkgrey")
    points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
    for(spsi in 1:spnum){
      pos.sps.i<-which(grepl(paste("[",spsi,"]",sep=""),rownames(summary(modelhere)$summary),fixed=TRUE))[2:4]
      jitt<-runif(1,0.05,0.4)
      pos.y.sps.i<-pos.y-jitt
      pos.x.sps.i<-summary(modelhere)$summary[pos.sps.i[i],"mean"]
      lines(summary(modelhere)$summary[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
            col=alpha(my.pal[spsi], alphahere))
      points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
      
    }
  }
  par(xpd=TRUE) # so I can plot legend outside
  legend(leg1, leg2, sort(unique(gsub("_", " ", speciesnamehere))),
         col=alpha(my.pal[1:spnum], alphahere),
         pch=my.pch[1:spnum],
         cex=1, bty="n", text.font=3)
  dev.off()
  
}