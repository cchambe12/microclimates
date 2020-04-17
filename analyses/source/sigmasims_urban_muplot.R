## Started 27 Feb 2020 by Cat
# Based off Lizzie's code for BB manuscript

muplotfx <- function(modelhere, nameforfig, width, height, ylim, xlim, leg1, leg2){
spnum <- length(unique(df$species))
pdf(file.path(figpath, paste("muplot", nameforfig, figpathmore, ".pdf", sep="")),
    width = width, height = height)
par(xpd=FALSE)
par(mar=c(5,7,3,10))
plot(x=NULL,y=NULL, xlim=xlim, yaxt='n', ylim=ylim,
     xlab="Model estimate change in growing degree days to budburst", ylab="", main=nameforfig)
axis(2, at=1:5, labels=rev(c("Intercept", "Urban", "sigma_a_sp", "sigma_b_sp", "sigma_y")), las=1)
abline(v=0, lty=2, col="darkgrey")
rownameshere <- c("mu_a_sp", "mu_b_tx_sp", "sigma_a_sp", "sigma_b_tx_sp", "sigma_y")
ppeffects <- c("mu_a_sp", "mu_b_tx_sp", "sigma_a_sp", "sigma_b_tx_sp") # or 1:4 here...
for(i in 1:5){
  pos.y<-(5:1)[i]
  pos.x<-summary(modelhere)$summary[rownameshere[i],"mean"]
  lines(summary(modelhere)$summary[rownameshere[i],c("25%","75%")],rep(pos.y,2),col="darkgrey")
  points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
for(spsi in 1:spnum){
  pos.sps.i<-which(grepl(paste("[",spsi,"]",sep=""),rownames(summary(modelhere)$summary),fixed=TRUE))[1:2]
  jitt<-(spsi/spnum) + 0.01
  pos.y.sps.i<-pos.y-jitt
  pos.x.sps.i<-summary(modelhere)$summary[pos.sps.i[i],"mean"]
  lines(summary(modelhere)$summary[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
      col=alpha(my.pal[spsi], alphahere))
  points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
  
}
}
par(xpd=TRUE) # so I can plot legend outside
legend(leg1, leg2, sort(unique(gsub("_", " ", df$species))), pch=my.pch[1:spnum],
   col=alpha(my.pal[1:spnum], alphahere),
   cex=0.75, bty="n", text.font=3)
dev.off()

}
