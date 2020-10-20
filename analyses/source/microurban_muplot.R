## Started 27 Feb 2020 by Cat
# Based off Lizzie's code for BB manuscript

muplotfx <- function(modelhere, nameforfig, width, height, ylim, xlim, leg1, leg2){
spnum <- length(unique(bball.stan$spp))
pdf(file.path(figpath, paste("muplot", nameforfig, figpathmore, ".pdf", sep="")),
    width = width, height = height)
par(xpd=FALSE)
par(mar=c(5,10,3,10))
plot(x=NULL,y=NULL, xlim=xlim, yaxt='n', ylim=ylim,
     xlab="Model estimate change in growing degree days to budburst", ylab="", main=nameforfig)
axis(2, at=1:3, labels=rev(c("Arboretum", "Weather Station", "Arboretum x\nWeather Station")), las=1)
abline(v=0, lty=2, col="darkgrey")
rownameshere <- c("mu_b_urban_sp", "mu_b_method_sp", "mu_b_um_sp")
ppeffects <- c("b_urban", "b_method", "b_um")
for(i in 1:3){
  pos.y<-(3:1)[i]
  pos.x<-summary(modelhere)$summary[rownameshere[i],"mean"]
  lines(summary(modelhere)$summary[rownameshere[i],c("25%","75%")],rep(pos.y,2),col="darkgrey")
  points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
for(spsi in 1:spnum){
  pos.sps.i<-which(grepl(paste("[",spsi,"]",sep=""),rownames(summary(modelhere)$summary),fixed=TRUE))[2:4]
  jitt<-(spsi/40) + 0.08
  pos.y.sps.i<-pos.y-jitt
  pos.x.sps.i<-summary(modelhere)$summary[pos.sps.i[i],"mean"]
  lines(summary(modelhere)$summary[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
      col=alpha(my.pal[spsi], alphahere))
  points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
  
}
}
par(xpd=TRUE) # so I can plot legend outside
legend(leg1, leg2, sort(unique(gsub("_", " ", bball.stan$spp))), pch=my.pch[1:spnum],
   col=alpha(my.pal[1:spnum], alphahere),
   cex=1, bty="n", text.font=3)
dev.off()

}
