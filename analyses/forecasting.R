### 24 February 2021 by Cat
## Forecasting GDD accuracy with warming

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#Load Libraries
library(viridis)
library(ggplot2)
library(gridExtra)

setwd("~/Documents/git/microclimates/analyses/")

source("source/sims_warm_sourcedata.R")

#### warmfunc(fstar.min, fstar.max, warmmax, meantemp, basetemp)

gdd.warm <- warmfunc(100, 300, 10, 10, 10)
gdd.warm <- gdd.warm[[1]]

#pdf("figures/forecasting_base10.pdf", width=6, height=5)
ten <- ggplot(gdd.warm, aes(as.numeric(warming), gdd_ratio, col=fstarspp, group=fstarspp)) +
  geom_line(stat='smooth', method = "lm", alpha=1, aes(col=fstarspp)) +
  #geom_point(aes(as.numeric(warming), gdd_accuracy, col=fstarspp, group=fstarspp)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.position="none",
        plot.margin = margin(0.5, 0.5, 0.5, 1, "cm")) + 
  xlab("Amount of warming (°C)") + ylab("GDD accuracy\n(estimated/observed)") +
  scale_x_continuous(breaks = sort(c(seq(0, 10, by=1)))) +
  scale_color_viridis_c("Species GDD Threshold") +
  coord_cartesian(ylim=c(1, 1.3)) 
#dev.off()

gdd.warm <- warmfunc(100, 300, 10, 10, 5)[[1]]

#pdf("figures/forecasting_base5.pdf", width=6, height=5)
five <- ggplot(gdd.warm, aes(as.numeric(warming), gdd_ratio, col=fstarspp, group=fstarspp)) +
  geom_line(stat='smooth', method = "lm", alpha=1, aes(col=fstarspp)) +
  #geom_point(aes(as.numeric(warming), gdd_accuracy, col=fstarspp, group=fstarspp)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.key = element_rect(colour = "transparent", fill = "white"),
        plot.margin = margin(0.5, 0.5, 0.5, 1, "cm")) + 
  xlab("Amount of warming (°C)") + ylab("GDD accuracy\n(estimated/observed)") +
  scale_x_continuous(breaks = sort(c(seq(0, 10, by=1)))) +
  scale_color_viridis_c("Species GDD Threshold") +
  coord_cartesian(ylim=c(1, 1.3)) 
#dev.off()


gdd.warm <- warmfunc(100, 300, 10, 10, 0)[[1]]

#pdf("figures/forecasting_base0.pdf", width=6, height=5)
zero <- ggplot(gdd.warm, aes(as.numeric(warming), gdd_ratio, col=fstarspp, group=fstarspp)) +
  geom_line(stat='smooth', method = "lm", alpha=1, aes(col=fstarspp)) +
  #geom_point(aes(as.numeric(warming), gdd_accuracy, col=fstarspp, group=fstarspp)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.key = element_rect(colour = "transparent", fill = "white"),
        plot.margin = margin(0.5, 0.5, 0.5, 1, "cm")) + 
  xlab("Amount of warming (°C)") + ylab("GDD accuracy\n(estimated/observed)") +
  scale_x_continuous(breaks = sort(c(seq(0, 10, by=1)))) +
  scale_color_viridis_c("Species GDD Threshold") +
  coord_cartesian(ylim=c(1, 1.3))
#dev.off()

gdd.warm <- warmfunc(100, 300, 10, 10, -5)[[1]]

#pdf("figures/forecasting_base-5.pdf", width=6, height=5)
neg5 <- ggplot(gdd.warm, aes(as.numeric(warming), gdd_ratio, col=fstarspp, group=fstarspp)) +
  geom_line(stat='smooth', method = "lm", alpha=1, aes(col=fstarspp)) +
  #geom_point(aes(as.numeric(warming), gdd_accuracy, col=fstarspp, group=fstarspp)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.key = element_rect(colour = "transparent", fill = "white"),
        plot.margin = margin(0.5, 0.5, 0.5, 1, "cm")) + 
  xlab("Amount of warming (°C)") + ylab("GDD accuracy\n(estimated/observed)") +
  scale_x_continuous(breaks = sort(c(seq(0, 10, by=1)))) +
  scale_color_viridis_c("Species GDD Threshold") +
  coord_cartesian(ylim=c(1, 1.3)) 
#dev.off()

pdf("figures/forecasting.pdf", width=6, height=5)
egg::ggarrange(neg5, zero, five, ten, ncol=4)
dev.off()

################################################################################################################
################ Now there seems to be an interesting mathematical phenomena occuring ##########################
################################################################################################################
## So it appears that different base thresholds result in different fstars having almost continuous accuracy with warming
# And then there is this cross over effect with other fstar values. Let's see if we can figure out the math

# I think it is a matter of...
# meantemp - basetemp
# d = rt
# So... I think it might  be similar... of GDD is equivent to distance, time is equivalent to mean or base or diff temp?
# And then is rate just simply mean/base or base/mean??? Let's try it out.

# We want to solve for the slope of with warming... okay so is it the (sum of the meantemp + n warming)*x = GDD threshold
# x = GDD threshold/E(meantemp+n)


baseneg <- warmfunc(100, 300, 10, 10, -5)[[1]]
base0 <- warmfunc(100, 300, 10, 10, 0)[[1]]
base5 <- warmfunc(100, 300, 10, 10, 5)[[1]]
base10 <- warmfunc(100, 300, 10, 10, 10)[[1]]


meantemp <- 10
basetempneg <- -5
basetemp0 <- 0
basetemp5 <- 5
basetemp10 <- 10

if(FALSE){
# Let's now solve for our "time" value
nneg <- vector()
n0 <- vector()
n5 <- vector()
n10 <- vector()
for(i in 1:11){
  nneg[i] <- (9-(-5))+i
  ntotneg <- sum(nneg)
  
  n0[i] <- (9-0)+i
  ntot0 <- sum(n0)
  
  n5[i] <- (9-5)+i
  ntot5 <- sum(n5)
  
  n10[i] <- (9-10)+i
  ntot10 <- sum(n10)
  }

slopes <- data.frame(fstarspp = sort(unique(baseneg$fstarspp)))

slopes$xneg <- sort(unique(baseneg$fstarspp))/ntotneg

slopes$x0 <- sort(unique(base0$fstarspp))/ntot0

slopes$x5 <- sort(unique(base5$fstarspp))/ntot5

slopes$x10 <- sort(unique(base10$fstarspp))/ntot10
}

### Okay, starting to see a small trend but we want to ask about ACCURACY ratio not just slope. 
# So the way that the ratio is measured is estimate/observed
# Okay, so to estimate the slope of the ratio we want 
# which is simply y = mx + b

numfstars <- length(unique(base0$fstarspp))
slopes$amneg <- round(((unique(ave(baseneg$gdd_ratio[baseneg$warming==10], baseneg$fstarspp[baseneg$warming==10])) - 
                 unique(ave(baseneg$gdd_ratio[baseneg$warming==0], baseneg$fstarspp[baseneg$warming==0])))/
                (rep(((meantemp+10)-basetempneg), numfstars) - rep(((meantemp+0)-basetempneg),numfstars))) * 1000, digits=3)
slopes$m0 <- round(((unique(ave(base0$gdd_ratio[base0$warming==10], base0$fstarspp[base0$warming==10])) - 
                unique(ave(base0$gdd_ratio[base0$warming==0], base0$fstarspp[base0$warming==0])))/
  (rep(((meantemp+10)-basetemp0), numfstars) - rep(((meantemp+0)-basetemp0),numfstars))) * 1000, digits=2)
slopes$m5 <- round(((unique(ave(base5$gdd_ratio[base5$warming==10], base5$fstarspp[base5$warming==10])) - 
                 unique(ave(base5$gdd_ratio[base5$warming==0], base5$fstarspp[base5$warming==0])))/
                (rep(((meantemp+10)-basetemp5), numfstars) - rep(((meantemp+0)-basetemp5),numfstars))) * 1000, digits=3)
slopes$m10 <- round(((unique(ave(base10$gdd_ratio[base10$warming==10], base10$fstarspp[base10$warming==10])) - 
                 unique(ave(base10$gdd_ratio[base10$warming==0], base10$fstarspp[base10$warming==0])))/
                (rep(((meantemp+10)-basetemp10), numfstars) - rep(((meantemp+0)-basetemp10),numfstars))) * 1000, digits=3)

slopesbyfstar <- gather(slopes, base, m, -fstarspp)
lm.neg <- lm(slopesbyfstar$m[slopesbyfstar$base=="mneg"]~slopesbyfstar$fstarspp[slopesbyfstar$base=="mneg"])
lm.0 <- lm(slopesbyfstar$m[slopesbyfstar$base=="m0"]~slopesbyfstar$fstarspp[slopesbyfstar$base=="m0"])
lm.5 <- lm(slopesbyfstar$m[slopesbyfstar$base=="m5"]~slopesbyfstar$fstarspp[slopesbyfstar$base=="m5"])
lm.10 <- lm(slopesbyfstar$m[slopesbyfstar$base=="m10"]~slopesbyfstar$fstarspp[slopesbyfstar$base=="m10"])

# Now calculate it!    
a1 <- coef(lm.neg)-coef(lm.0)
inter.neg0 <- c(x=-a1[[1]]/a1[[2]], y=coef(lm.neg)[[2]]*x + coef(lm.neg)[[1]])
a2 <- coef(lm.10)-coef(lm.5)
inter.105 <- c(x=-a2[[1]]/a2[[2]], y=coef(lm.10)[[2]]*x + coef(lm.10)[[1]])


cols <-viridis_pal(option="viridis")(4)
pdf("figures/forecasting_slopes.pdf", width=6, height=5)
ggplot(slopesbyfstar, aes(x=fstarspp, y=m, col=base)) + 
  geom_smooth(method="lm", se=FALSE) + 
  scale_color_manual(name="Base Temperature", values=cols, labels=c("amneg"="-5",
                                           "m0"="0",
                                           "m5"="5",
                                           "m10"="10")) +
  theme_classic() + 
  geom_vline(xintercept = inter.neg0[[1]], linetype="dashed") +
  geom_vline(xintercept = inter.105[[1]], linetype="dashed") +
  scale_x_continuous(breaks=c(seq(100, 300, by=50), round(inter.neg0[[1]], digits=0), 
                              round(inter.105[[1]], digits=0))) +
  xlab("GDD Threshold") + ylab("Slope (x1000)")
dev.off()  



