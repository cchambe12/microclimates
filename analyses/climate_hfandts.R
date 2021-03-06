## Following code from git/treespotters/Active_Volunteers.R I wanted to look at Harvard Forest data
# Cat - 7 July 2020

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(egg)

setwd("~/Documents/git/microclimates/analyses/")

cols <- cols <- viridis_pal(option="plasma")(3)
#colz <- c("salmon3", "royalblue3")
lines <- rep(c(0:6), times=5)

### Let's add in Climate data now
#clim <- read.csv("output/clean_addinclimate.csv", header=TRUE)
climhobo <- read.csv("output/clean_clim_hobo.csv")
climws <- read.csv("output/clean_clim_ws.csv")
#clim <- clim[(clim$climatetype=="harvardforest"),]
#clim <- clim[(clim$year>2015),]
climhobo <- climhobo[(climhobo$year==2019),]
climhobo$tmean <- ave(climhobo$tmean, climhobo$date, climhobo$climatetype)
climhobo$date.time <- climhobo$temp <- climhobo$tempcalib <- climhobo$hour <- NULL
climhobo <- climhobo[!duplicated(climhobo),]

climhobo <- climhobo[!(climhobo$climatetype%in%c("weldhill", "harvardforest")) ,]
climhobo$site <- substr(climhobo$climatetype, 0, 2)
#climhobo$site <- ifelse(climhobo$site=="ar", "zar", climhobo$site)

climws <- climws[(climws$year==2019),]
climws <- climws[!duplicated(climws),]

springhobo <- climhobo[(climhobo$doy>=1 & climhobo$doy<=150),]
springws <- climws[(climws$doy>=1 & climws$doy<=150),]

springws$climatetype <- ifelse(springws$climatetype=="weldhill", "arb", springws$climatetype)

springws$site <- springws$climatetype
springws$site <- ifelse(springws$site=="harvardforest", "hf", springws$site)

colstokeep <- c("date", "year", "doy", "tmean", "site", "climatetype")

springhobo <- subset(springhobo, select=colstokeep)
springws <- subset(springws, select=colstokeep)



######## Now for some plots comparing the two methods across the two sites

springws$tmeanws <- springws$tmean
springhobo$tmeanhobo <- springhobo$tmean

springws$tmean <- springhobo$tmean <-NULL

springws$climatetype <- NULL
springhobo$site <- ifelse(springhobo$site=="ar", "arb", "hf")

springws$tmeanws <- ave(springws$tmeanws, springws$date, springws$site)
springws <- springws[!duplicated(springws),]


spring <- full_join(springhobo, springws)
spring$methoddiff <- spring$tmeanhobo - spring$tmeanws

range(spring$methoddiff, na.rm = TRUE)
mean(spring$methoddiff[spring$site=="arb"], na.rm=TRUE)
mean(spring$methoddiff[spring$site=="hf"], na.rm=TRUE)

range(spring$methoddiff[spring$site=="arb"], na.rm=TRUE)
range(spring$methoddiff[spring$site=="hf"], na.rm=TRUE)

methoddiff <- ggplot(spring, aes(x=doy, y=methoddiff, col=as.factor(site))) + #geom_point(aes(col=as.factor(year)), alpha=0.1) +
  geom_line(aes(col=as.factor(site), fill=as.factor(site), linetype=climatetype), alpha=0.5, stat="smooth", method="loess", se=FALSE, span=0.9) + 
  scale_color_manual(name = "Site", values=cols, labels = c("arb"="Urban site","hf"="Forest site")) +
  scale_fill_manual(name = "Site", values=cols, labels = c("arb"="Urban site", "hf"="Forest site")) +
  scale_linetype_manual(name = "Hobo Logger", values=lines) +
  guides(linetype=FALSE) +
  ggtitle("a)") +
  theme_classic() + xlab("Day of Year") + ylab("Hobo Logger - Weather Station \nTemperature (°C)") +
  coord_cartesian(ylim=c(-1, 4), expand=0) + scale_x_continuous(breaks = seq(min(0), max(140), by=30)) +
  scale_y_continuous(breaks=seq(min(-1), max(4), by=1)) + theme(panel.spacing = unit(c(0,0,5,5),"cm"),
                                                                 legend.text = element_text(size=7),
                                                                 legend.title = element_text(size=8),
                                                                 legend.key.size = unit(0.8,"line"),
                                                                legend.position="none")

methoddiff_line <- ggplot(spring, aes(x=doy, y=methoddiff, col=as.factor(site))) + 
  geom_line(aes(col=as.factor(site), linetype=climatetype), alpha=0.3) + 
  scale_color_manual(name = "Site", values=cols, labels = c("arb"="Urban site","hf"="Forest site"),
                     guide = guide_legend(override.aes = list(alpha = 1))) +
  scale_linetype_manual(name = "Hobo Logger", values=lines) +
  guides(linetype=FALSE) +
  scale_alpha(guide = 'none') +
  ggtitle("b)") +
  theme_classic() + xlab("Day of Year") + ylab("Hobo Logger - Weather Station \nTemperature (°C)") +
  coord_cartesian(ylim=c(-4, 7), expand=0) + scale_x_continuous(breaks = seq(min(0), max(140), by=30)) +
  scale_y_continuous(breaks=seq(min(-4), max(7), by=1)) + theme(panel.spacing = unit(c(0,0,5,5),"cm"),
                                                                legend.text = element_text(size=7),
                                                                legend.title = element_text(size=8),
                                                                legend.key.size = unit(0.8,"line"))

pdf("figures/climate_methodiff.pdf", width=8, height=4, onefile=FALSE)
ggarrange(methoddiff, methoddiff_line, ncol=2)
dev.off()

if(FALSE){
pdf("figures/clim_methoddiff.pdf", width=8, height=4, onefile=FALSE)
methoddiff
dev.off()

pdf("figures/clim_methoddiff_detailed.pdf", width=8, height=4, onefile=FALSE)
methoddiff_line
dev.off()

write.csv(spring, file="output/clim_arbhf_hobows.csv", row.names=FALSE)
}

alltemps <- ggplot(spring, aes(x=doy)) + 
  geom_line(aes(col=as.factor(site), linetype=climatetype, y=tmeanhobo), alpha=0.2, stat="smooth", method="loess", se=FALSE, span=0.9) + 
  geom_smooth(aes(y=tmeanws, col=as.factor(site)), stat="smooth", method="loess", se=FALSE, span=0.9) + 
  scale_color_manual(name = "Site", values=cols, labels = c("arb"="Urban site","hf"="Forest site")) +
  scale_linetype_manual(name = "Hobo Logger", values=lines) +
  guides(linetype=FALSE) +
  ggtitle("a)") +
  geom_hline(yintercept=0, linetype="dashed", col="black") +
  theme_classic() + xlab("Day of Year") + ylab("Mean Temperature (°C)") +
  coord_cartesian(ylim=c(-8, 18), expand=0) + scale_x_continuous(breaks = seq(min(0), max(140), by=30)) +
  scale_y_continuous(breaks=seq(min(-8), max(18), by=4)) + theme(panel.spacing = unit(c(0,0,5,5),"cm"),
                                                                legend.text = element_text(size=7),
                                                                legend.title = element_text(size=8),
                                                                legend.key.size = unit(0.8,"line"),
                                                                legend.position="none")

alltemps_line <- ggplot(spring, aes(x=doy)) + 
  geom_line(aes(col=as.factor(site), linetype=climatetype, y=tmeanhobo), alpha=0.2) +#, stat="smooth", method="loess", se=FALSE, span=0.9) + 
  geom_line(aes(y=tmeanws, col=as.factor(site))) + #, stat="smooth", method="loess", se=FALSE, span=0.9) + 
  scale_color_manual(name = "Site", values=cols, labels = c("arb"="Urban site","hf"="Forest site")) +
  scale_linetype_manual(name = "Hobo Logger", values=lines) +
  guides(linetype=FALSE) +
  ggtitle("b)") +
  geom_hline(yintercept=0, linetype="dashed", col="black") +
  theme_classic() + xlab("Day of Year") + ylab("Mean Temperature (°C)") +
  coord_cartesian(ylim=c(-20, 20), expand=0) + scale_x_continuous(breaks = seq(min(0), max(140), by=30)) +
  scale_y_continuous(breaks=seq(min(-20), max(20), by=5)) + theme(panel.spacing = unit(c(0,0,5,5),"cm"),
                                                                 legend.text = element_text(size=7),
                                                                 legend.title = element_text(size=8),
                                                                 legend.key.size = unit(0.8,"line"))


pdf("figures/climate_smooth&daily.pdf", width=8, height=4, onefile=FALSE)
ggarrange(alltemps, alltemps_line, ncol=2)
dev.off()

if(FALSE){
pdf("figures/clim_alltemps.pdf", width=8, height=4, onefile=FALSE)
alltemps
dev.off()

pdf("figures/clim_alltemps_detailed.pdf", width=8, height=4, onefile=FALSE)
alltemps_line
dev.off()

pdf("figures/clim_4panel.pdf", width=8, height=7, onefile=FALSE)
ggarrange(alltemps, alltemps_line, methoddiff, methoddiff_line, nrow=2, ncol=2)
dev.off()
}


