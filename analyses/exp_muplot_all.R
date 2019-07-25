## Started 3 April 2019 ##
## By Cat - based off code from Lizzie's OSPREE plots ##

# Runs from models_stan_plotting.R #

# with COLORS for each species #

muplotfx <- function(modelhere, nameforfig, width, height, ylim, xlim, leg1, leg2){
  spnum <- unique(df$spp)
  pdf(file.path(figpath, paste("", nameforfig, figpathmore, ".pdf", sep="")),
      width = width, height = height)
  par(xpd=FALSE)
  par(mar=c(5,7,3,10))
  plot(x=NULL,y=NULL, xlim=xlim, yaxt='n', ylim=ylim,
       xlab=xlab, ylab="", main=nameforfig)
  axis(2, at=1:3, labels=rev(c("Common \nGarden", "Harvard Forest", "Provenance \nLatitude")),las=1)
  abline(v=0, lty=2, col="darkgrey")
  rownameshere <- c("sitecg", "sitehf", "provenance.lat")
  ppeffects <- c("sitecg", "sitehf", "provenance.lat") # or 1:4 here...
  for(i in 1:3){#i=1
    pos.y<-(3:1)[i]
    pos.x<-modoutput[(modoutput$term==rownameshere[i]),"estimate"]
    lines(modoutput[(modoutput$term==rownameshere[i]),c("lower","upper")],rep(pos.y,2),col="darkgrey")
    points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
    for(spsi in 1:length(spnum)){#spsi=1
      pos.sps.i<-which(grepl(paste("[",spsi,"]",sep=""),mod.ranef$parameter,fixed=TRUE))
      jitt<-runif(1,0.05,0.4)
      pos.y.sps.i<-pos.y-jitt
      pos.x.sps.i<-mod.ranef[pos.sps.i[i],"mean"]
      lines(mod.ranef[pos.sps.i[i],c("lower","upper")],rep(pos.y.sps.i,2),
            col=alpha(my.pal[spsi], alphahere))
      points(pos.x.sps.i,pos.y.sps.i,cex=0.8, col=alpha(my.pal[spsi], alphahere), pch=my.pch[spsi])
      
    }
  }
  par(xpd=TRUE) # so I can plot legend outside
  legend(leg1, leg2, sort(unique(speciesnames)), pch=my.pch[1:length(spnum)],
         col=alpha(my.pal[1:length(spnum)], alphahere),
         cex=0.70, bty="n", text.font=3)
  dev.off()
  
}
