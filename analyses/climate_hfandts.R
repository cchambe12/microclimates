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


climatehobo <- ggplot(springhobo, aes(x=doy, y=tmean, col=as.factor(site))) + #geom_point(aes(col=as.factor(year)), alpha=0.1) +
  geom_smooth(aes(col=as.factor(site), fill=as.factor(site), linetype=climatetype), alpha=0.3, stat="smooth", method="loess", se=FALSE, span=0.9) + 
  scale_color_manual(name = "Site", values=cols, labels = c("ar"="Arboretum","hf"="Harvard Forest")) +
  scale_fill_manual(name = "Site", values=cols, labels = c("ar"="Arboretum", "hf"="Harvard Forest")) +
  scale_linetype_manual(name = "Hobo Logger", values=lines) +
  guides(linetype=FALSE) +
  theme_classic() + xlab("Day of Year") + ylab("Mean \n Temperature (°C)") +
  ggtitle("b) Hobo Logger") +
  coord_cartesian(ylim=c(-8, 18), expand=0) + scale_x_continuous(breaks = seq(min(0), max(140), by=30)) +
  scale_y_continuous(breaks=seq(min(-8), max(18), by=4)) + theme(panel.spacing = unit(c(0,0,5,5),"cm"),
                                                                 legend.text = element_text(size=7),
                                                                 legend.title = element_text(size=8),
                                                                 legend.key.size = unit(0.8,"line"))

climatews <- ggplot(springws, aes(x=doy, y=tmean, col=as.factor(climatetype))) + #geom_point(aes(col=as.factor(year)), alpha=0.1) +
  geom_smooth(aes(col=as.factor(climatetype), fill=as.factor(climatetype)), stat="smooth", method="loess", se=TRUE, span=0.9) + 
  scale_color_manual(name = "Site", values=cols, labels = c("arb"="Arboretum","harvardforest"="Harvard Forest")) +
  scale_fill_manual(name = "Site", values=cols, labels = c("arb"="Arboretum", "harvardforest"="Harvard Forest")) +
  theme_classic() + xlab("Day of Year") + ylab("Mean \n Temperature (°C)") +
  ggtitle("a) Weather Station") +
  coord_cartesian(ylim=c(-8, 18), expand=0) + scale_x_continuous(breaks = seq(min(0), max(140), by=30)) +
  scale_y_continuous(breaks=seq(min(-8), max(18), by=4)) + theme(panel.spacing = unit(c(0,0,5,5),"cm"),
                                                                 legend.text = element_text(size=7),
                                                                 legend.title = element_text(size=8),
                                                                 legend.key.size = unit(0.8,"line"), 
                                                                 legend.position="none")

climate <- ggarrange(climatews, climatehobo, ncol=2)
if(FALSE){
pdf("figures/climate_hfandts.pdf", width=8, height=4, onefile=FALSE)
climate
dev.off()
}




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
  scale_color_manual(name = "Site", values=cols, labels = c("arb"="Arboretum","hf"="Harvard Forest")) +
  scale_fill_manual(name = "Site", values=cols, labels = c("arb"="Arboretum", "hf"="Harvard Forest")) +
  scale_linetype_manual(name = "Hobo Logger", values=lines) +
  guides(linetype=FALSE) +
  ggtitle("c)") +
  theme_classic() + xlab("Day of Year") + ylab("Hobo Logger - Weather Station \nTemperature (°C)") +
  coord_cartesian(ylim=c(-1, 4), expand=0) + scale_x_continuous(breaks = seq(min(0), max(140), by=30)) +
  scale_y_continuous(breaks=seq(min(-1), max(4), by=1)) + theme(panel.spacing = unit(c(0,0,5,5),"cm"),
                                                                 legend.text = element_text(size=7),
                                                                 legend.title = element_text(size=8),
                                                                 legend.key.size = unit(0.8,"line"),
                                                                legend.position="none")

methoddiff_line <- ggplot(spring, aes(x=doy, y=methoddiff, col=as.factor(site))) + 
  geom_line(aes(col=as.factor(site), linetype=climatetype), alpha=0.3) + 
  scale_color_manual(name = "Site", values=cols, labels = c("arb"="Arboretum","hf"="Harvard Forest"),
                     guide = guide_legend(override.aes = list(alpha = 1))) +
  scale_linetype_manual(name = "Hobo Logger", values=lines) +
  guides(linetype=FALSE) +
  scale_alpha(guide = 'none') +
  ggtitle("d)") +
  theme_classic() + xlab("Day of Year") + ylab("Hobo Logger - Weather Station \nTemperature (°C)") +
  coord_cartesian(ylim=c(-4, 7), expand=0) + scale_x_continuous(breaks = seq(min(0), max(140), by=30)) +
  scale_y_continuous(breaks=seq(min(-4), max(7), by=1)) + theme(panel.spacing = unit(c(0,0,5,5),"cm"),
                                                                legend.text = element_text(size=7),
                                                                legend.title = element_text(size=8),
                                                                legend.key.size = unit(0.8,"line"))

pdf("figures/clim_methoddiff.pdf", width=8, height=4, onefile=FALSE)
methoddiff
dev.off()

pdf("figures/clim_methoddiff_detailed.pdf", width=8, height=4, onefile=FALSE)
methoddiff_line
dev.off()

write.csv(spring, file="output/clim_arbhf_hobows.csv", row.names=FALSE)


alltemps <- ggplot(spring, aes(x=doy)) + 
  geom_line(aes(col=as.factor(site), linetype=climatetype, y=tmeanhobo), alpha=0.2, stat="smooth", method="loess", se=FALSE, span=0.9) + 
  geom_smooth(aes(y=tmeanws, col=as.factor(site)), stat="smooth", method="loess", se=FALSE, span=0.9) + 
  scale_color_manual(name = "Site", values=cols, labels = c("arb"="Arboretum","hf"="Harvard Forest")) +
  scale_linetype_manual(name = "Hobo Logger", values=lines) +
  guides(linetype=FALSE) +
  ggtitle("a)") +
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
  scale_color_manual(name = "Site", values=cols, labels = c("arb"="Arboretum","hf"="Harvard Forest")) +
  scale_linetype_manual(name = "Hobo Logger", values=lines) +
  guides(linetype=FALSE) +
  ggtitle("b)") +
  theme_classic() + xlab("Day of Year") + ylab("Mean Temperature (°C)") +
  coord_cartesian(ylim=c(-20, 20), expand=0) + scale_x_continuous(breaks = seq(min(0), max(140), by=30)) +
  scale_y_continuous(breaks=seq(min(-20), max(20), by=5)) + theme(panel.spacing = unit(c(0,0,5,5),"cm"),
                                                                 legend.text = element_text(size=7),
                                                                 legend.title = element_text(size=8),
                                                                 legend.key.size = unit(0.8,"line"))

pdf("figures/clim_alltemps.pdf", width=8, height=4, onefile=FALSE)
alltemps
dev.off()

pdf("figures/clim_alltemps_detailed.pdf", width=8, height=4, onefile=FALSE)
alltemps_line
dev.off()

pdf("figures/clim_4panel.pdf", width=8, height=7, onefile=FALSE)
ggarrange(alltemps, alltemps_line, methoddiff, methoddiff_line, nrow=2, ncol=2)
dev.off()



######## No longer needed but holding for potential future use...####
if(FALSE){

climate <- ggplot(spring, aes(x=doy, y=tmean, col=as.factor(year))) + #geom_point(aes(col=as.factor(year)), alpha=0.1) +
  geom_smooth(aes(col=as.factor(year), fill=as.factor(year)), stat="smooth", method="loess", se=TRUE, span=0.9) + 
  scale_color_manual(name = "Year", values=cols, labels = c("2016", "2017", "2018", "2019", "2020")) +
  scale_fill_manual(name = "Year", values=cols, labels = c("2016", "2017", "2018", "2019", "2020")) +
  theme_classic() + xlab("Day of Year") + ylab("Mean \n Temperature (°C)") +
  coord_cartesian(ylim=c(-8, 18), expand=0) + scale_x_continuous(breaks = seq(min(0), max(140), by=30)) +
  scale_y_continuous(breaks=seq(min(-8), max(18), by=4)) + theme(panel.spacing = unit(c(0,0,5,5),"cm"),
                                                                 legend.text = element_text(size=7),
                                                                 legend.title = element_text(size=8),
                                                                 legend.key.size = unit(0.8,"line"))

clim2016 <- ggplot(spring[(spring$year==2016),], aes(x=doy, y=tmean), col=cols[1]) + #geom_point(aes(col=as.factor(year)), alpha=0.1) +
  geom_smooth(col=cols[1], fill=cols[1], stat="smooth", method="loess", se=TRUE, span=0.9) + 
  #scale_color_manual(name = "Year", values=cols, labels = c("2016", "2017", "2018", "2019", "2020")) +
  #scale_fill_manual(name = "Year", values=cols, labels = c("2016", "2017", "2018", "2019", "2020")) +
  theme_classic() + xlab("Day of Year") + ylab("Mean \n Temperature (°C)") +
  coord_cartesian(ylim=c(-8, 18), expand=0) + scale_x_continuous(breaks = seq(min(0), max(140), by=30)) +
  scale_y_continuous(breaks=seq(min(-8), max(18), by=4)) + theme(panel.spacing = unit(c(0,0,5,5),"cm"),
                                                                 legend.text = element_text(size=7),
                                                                 legend.title = element_text(size=8),
                                                                 legend.key.size = unit(0.8,"line"),
                                                                 legend.position = "none")
clim2017 <- ggplot(spring[(spring$year==2017),], aes(x=doy, y=tmean), col=cols[2]) + #geom_point(aes(col=as.factor(year)), alpha=0.1) +
  geom_smooth(col=cols[2], fill=cols[2], stat="smooth", method="loess", se=TRUE, span=0.9) + 
  #scale_color_manual(name = "Year", values=cols, labels = c("2016", "2017", "2018", "2019", "2020")) +
  #scale_fill_manual(name = "Year", values=cols, labels = c("2016", "2017", "2018", "2019", "2020")) +
  theme_classic() + xlab("Day of Year") + ylab("Mean \n Temperature (°C)") +
  coord_cartesian(ylim=c(-8, 18), expand=0) + scale_x_continuous(breaks = seq(min(0), max(140), by=30)) +
  scale_y_continuous(breaks=seq(min(-8), max(18), by=4)) + theme(panel.spacing = unit(c(0,0,5,5),"cm"),
                                                                 legend.text = element_text(size=7),
                                                                 legend.title = element_text(size=8),
                                                                 legend.key.size = unit(0.8,"line"),
                                                                 legend.position = "none")

clim2018 <- ggplot(spring[(spring$year==2018),], aes(x=doy, y=tmean), col=cols[3]) + #geom_point(aes(col=as.factor(year)), alpha=0.1) +
  geom_smooth(col=cols[3], fill=cols[3], stat="smooth", method="loess", se=TRUE, span=0.9) + 
  #scale_color_manual(name = "Year", values=cols, labels = c("2016", "2017", "2018", "2019", "2020")) +
  #scale_fill_manual(name = "Year", values=cols, labels = c("2016", "2017", "2018", "2019", "2020")) +
  theme_classic() + xlab("Day of Year") + ylab("Mean \n Temperature (°C)") +
  coord_cartesian(ylim=c(-8, 18), expand=0) + scale_x_continuous(breaks = seq(min(0), max(140), by=30)) +
  scale_y_continuous(breaks=seq(min(-8), max(18), by=4)) + theme(panel.spacing = unit(c(0,0,5,5),"cm"),
                                                                 legend.text = element_text(size=7),
                                                                 legend.title = element_text(size=8),
                                                                 legend.key.size = unit(0.8,"line"),
                                                                 legend.position = "none")

clim2019 <- ggplot(spring[(spring$year==2019),], aes(x=doy, y=tmean), col=cols[4]) + #geom_point(aes(col=as.factor(year)), alpha=0.1) +
  geom_smooth(col=cols[4], fill=cols[4], stat="smooth", method="loess", se=TRUE, span=0.9) + 
  #scale_color_manual(name = "Year", values=cols, labels = c("2016", "2017", "2018", "2019", "2020")) +
  #scale_fill_manual(name = "Year", values=cols, labels = c("2016", "2017", "2018", "2019", "2020")) +
  theme_classic() + xlab("Day of Year") + ylab("Mean \n Temperature (°C)") +
  coord_cartesian(ylim=c(-8, 18), expand=0) + scale_x_continuous(breaks = seq(min(0), max(140), by=30)) +
  scale_y_continuous(breaks=seq(min(-8), max(18), by=4)) + theme(panel.spacing = unit(c(0,0,5,5),"cm"),
                                                                 legend.text = element_text(size=7),
                                                                 legend.title = element_text(size=8),
                                                                 legend.key.size = unit(0.8,"line"),
                                                                 legend.position = "none")

clim2020 <- ggplot(spring[(spring$year==2020),], aes(x=doy, y=tmean), col=cols[5]) + #geom_point(aes(col=as.factor(year)), alpha=0.1) +
  geom_smooth(col=cols[5], fill=cols[5], stat="smooth", method="loess", se=TRUE, span=0.9) + 
  #scale_color_manual(name = "Year", values=cols, labels = c("2016", "2017", "2018", "2019", "2020")) +
  #scale_fill_manual(name = "Year", values=cols, labels = c("2016", "2017", "2018", "2019", "2020")) +
  theme_classic() + xlab("Day of Year") + ylab("Mean \n Temperature (°C)") +
  coord_cartesian(ylim=c(-8, 18), expand=0) + scale_x_continuous(breaks = seq(min(0), max(140), by=30)) +
  scale_y_continuous(breaks=seq(min(-8), max(18), by=4)) + theme(panel.spacing = unit(c(0,0,5,5),"cm"),
                                                                 legend.text = element_text(size=7),
                                                                 legend.title = element_text(size=8),
                                                                 legend.key.size = unit(0.8,"line"),
                                                                 legend.position = "none")

climall <- ggarrange(clim2016, clim2017, clim2018, clim2019, ncol=4)


# Set Working Directory
setwd("~/Documents/git/microclimates/analyses/output")
b<-read.csv("clean_budburstandleafout.csv",header=TRUE)

bhf <- b[(b$type=="Harvard Forest"),]
bhf <- bhf[(bhf$year>2015),]
bhf$speciesname <- paste(bhf$genus, bhf$species)

bhf$bbdoy<-as.numeric(bhf$budburst)
bhf$bbmean<-ave(bhf$bbdoy, bhf$year, bhf$speciesname)

bhf$lodoy<-as.numeric(bhf$leafout)
bhf$lomean<-ave(bhf$lodoy, bhf$year, bhf$speciesname)


bhf$code<-reorder(bhf$speciesname, bhf$bbmean)
bhf <- bhf[!duplicated(bhf),]
bhf.nona <- na.omit(bhf)

bhf.nona <- subset(bhf.nona, select=c(year, speciesname, bbmean, lomean, code))
bhf.nona <- gather(bhf.nona, pheno, mean, -year, -speciesname, -code)
bhf.nona$bb.yr<-ave(bhf.nona$mean, bhf.nona$year)
bhf.nona$colz <- ifelse(bhf.nona$pheno=="bbmean", "salmon3", "royalblue3")

dvr2016<-ggplot(bhf.nona[(bhf.nona$year==2016),], aes(x=code, y=mean)) + geom_point(aes(shape=pheno, color=rev(colz))) +
  geom_line(col="green4", alpha=0.3) + ylab("Day of Year") +theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
                                                                  axis.text.y = element_text(face = "italic"),
                                                                  axis.text=element_text(size=9), legend.key = element_rect(fill = "transparent"),
                                                                  legend.box.background = element_rect(),
                                                                  panel.spacing = unit(2, "lines"),
                                                                  plot.title = element_text(color="#1B9E77"),
                                                                  legend.position = "none",
                                                                  axis.title.y = element_blank()) + labs(col="Phenophase") + 
  geom_hline(aes(yintercept=bb.yr), bhf.nona[(bhf.nona$year==2016),], col="black", linetype="dashed") +
  scale_shape_manual(name="Phenophase", values=c(16, 17), labels=c("Budburst", "Leafout")) +
  scale_color_manual(name="Phenophase", values=c("salmon3", "royalblue3"), labels=c("Budburst", "Leafout")) + ggtitle("2016") +
  scale_y_continuous(breaks=seq(min(120), max(180), by=10)) + coord_flip(ylim=c(120,180)) 

dvr2017<-ggplot(bhf.nona[(bhf.nona$year==2017),], aes(x=code, y=mean)) + geom_point(aes(color=rev(colz),shape=pheno)) +
  geom_line(col="green4", alpha=0.3) + ylab("Day of Year") +theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
                                                                  axis.text.y = element_blank(),
                                                                  axis.title.y = element_blank(),
                                                                  axis.ticks.y = element_blank(),
                                                                  axis.text=element_text(size=9), legend.key = element_rect(fill = "transparent"),
                                                                  legend.box.background = element_rect(),
                                                                  plot.title = element_text(color="#D95F02"),
                                                                  legend.position="none") + labs(col="Phenophase") + 
  geom_hline(aes(yintercept=bb.yr), bhf.nona[(bhf.nona$year==2017),], col="black", linetype="dashed") +
  scale_shape_manual(name="Phenophase", values=c(16, 17), labels=c("Budburst", "Leafout")) +
  scale_color_manual(name="Phenophase", values=c("salmon3", "royalblue3"), labels=c("Budburst", "Leafout")) + ggtitle("2017")+
  scale_y_continuous(breaks=seq(min(120), max(180), by=10)) + coord_flip(ylim=c(120,180)) 

dvr2018<-ggplot(bhf.nona[(bhf.nona$year==2018),], aes(x=code, y=mean)) + geom_point(aes(color=rev(colz),shape=pheno)) +
  geom_line(col="green4", alpha=0.3) + coord_flip(ylim=c(120,180)) + ylab("Day of Year") + xlab("Species") +theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
                                                                                                                  axis.text.y = element_blank(),
                                                                                                                  axis.title.y = element_blank(),
                                                                                                                  axis.ticks.y = element_blank(),
                                                                                                                  legend.position = "none",
                                                                                                                  axis.text=element_text(size=9), legend.key = element_rect(fill = "transparent"),
                                                                                                                  legend.box.background = element_rect(),
                                                                                                                  plot.title = element_text(color="#7570B3"),
                                                                                                                  legend.text = element_text(size=7),
                                                                                                                  legend.title = element_text(size=8)) + labs(col="Phenophase") + 
  geom_hline(aes(yintercept=bb.yr), bhf.nona[(bhf.nona$year==2018),], col="black", linetype="dashed") +
  scale_shape_manual(name="Phenophase", values=c(16, 17), labels=c("Budburst", "Leafout")) +
  scale_color_manual(name="Phenophase", values=c("salmon3", "royalblue3"), labels=c("Budburst", "Leafout")) + ggtitle("2018") +
  scale_y_continuous(breaks=seq(min(120), max(180), by=10))

dvr2019<-ggplot(bhf.nona[(bhf.nona$year==2019),], aes(x=code, y=mean)) + geom_point(aes(color=rev(colz),shape=pheno)) +
  geom_line(col="green4", alpha=0.3) + coord_flip(ylim=c(120,180)) + ylab("Day of Year") + xlab("Species") +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text=element_text(size=9), legend.key = element_rect(fill = "transparent"),
        legend.box.background = element_rect(),
        plot.title = element_text(color="#E7298A"),
        legend.text = element_text(size=7),
        legend.title = element_text(size=8)) + labs(col="Phenophase") + 
  geom_hline(aes(yintercept=bb.yr), bhf.nona[(bhf.nona$year==2019),], col="black", linetype="dashed") +
  scale_shape_manual(name="Phenophase", values=c(16, 17), labels=c("Budburst", "Leafout")) +
  scale_color_manual(name="Phenophase", values=c("salmon3", "royalblue3"), labels=c("Budburst", "Leafout")) + ggtitle("2019") +
  scale_y_continuous(breaks=seq(min(120), max(180), by=10))

dvr2020<-ggplot(bhf.nona[(bhf.nona$year==2020),], aes(x=code, y=mean)) + geom_point(aes(color=rev(colz),shape=pheno)) +
  geom_line(col="green4", alpha=0.3) + ylab("Day of Year") +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), legend.key = element_rect(fill = "transparent"),
        legend.box.background = element_rect(),
        panel.spacing = unit(2, "lines"),
        plot.title = element_text(color=cols[5])) + labs(col="Phenophase") + 
  geom_hline(aes(yintercept=bb.yr), bhf.nona[(bhf.nona$year==2020),], col="black", linetype="dashed") +
  scale_shape_manual(name="Phenophase", values=c(16, 17), labels=c("Budburst", "Leafout")) +
  scale_color_manual(name="Phenophase", values=rev(colz), labels=c("Budburst", "Leafout")) + ggtitle("2020") +
  scale_y_continuous(breaks=seq(min(100), max(180), by=10)) + coord_flip(ylim=c(120,180)) 


allyrs <- ggarrange(dvr2016, dvr2017, dvr2018, dvr2019, ncol=4)




quartz()
grid.arrange(allyrs, climall, nrow=3, heights = c(3, 0.5, 1.5), layout_matrix=rbind(c(1, 1, 1, 1),
                                                                                    c(NA),
                                                                                    c(NA, 2, 2, 2, 2, 2, 2, 2, 2, NA)))
}