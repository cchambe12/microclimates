### Started 15 July 2019 - Cat
## Work on making some plots!

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load libraries
library(RColorBrewer)
library(ggplot2)

# Set working directory
setwd("~/Documents/git/microclimates/analyses")

## Load the data
gdd.stan <- read.csv("output/gdd_clean_bbanddvr.csv", header=TRUE)

gdd.stan <- gdd.stan[(gdd.stan$gdd_bb<=1000),]

gdd.stan$spp <- paste(substr(gdd.stan$genus, 0, 3), substr(gdd.stan$species,0,3), sep="")
gdd.stan$site <- NA
gdd.stan$site <- ifelse(gdd.stan$type=="Treespotters", "arb", gdd.stan$site)
gdd.stan$site <- ifelse(gdd.stan$type=="Common Garden", "cg", gdd.stan$site)
gdd.stan$site <- ifelse(gdd.stan$type=="Harvard Forest", "hf", gdd.stan$site)

gdd.stan <- subset(gdd.stan, select=c("id", "provenance.lat", "spp", "site",
                                      "gdd_bb", "gdd_dvr", "fs.count", "year", "budburst"))

gdd.stan <- gdd.stan[!is.na(gdd.stan$provenance.lat),]
gdd.stan <- gdd.stan[!is.na(gdd.stan$gdd_bb),]
gdd.stan <- gdd.stan[!is.na(gdd.stan$gdd_dvr),]
gdd.stan <- gdd.stan[!is.na(gdd.stan$spp),]

cols <- colorRampPalette(brewer.pal(8,"Dark2"))(8)

gdd.stan$gddmean <- ave(gdd.stan$gdd_bb, gdd.stan$spp, gdd.stan$site, gdd.stan$year)
gdd.stan$gddsd <- ave(gdd.stan$gdd_bb, gdd.stan$spp, gdd.stan$site, gdd.stan$year, FUN=sd)
gdd.stan$ymin <- gdd.stan$gddmean-gdd.stan$gddsd
gdd.stan$ymax <- gdd.stan$gddmean+gdd.stan$gddsd

gddplot <- na.omit(gdd.stan)

gddbar <- ggplot(gddplot, aes(x=spp, y=gddmean, fill=as.factor(year), linetype=site)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=ymin, ymax=ymax),width = 0.2, position=position_dodge(0.9)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        #legend.position = "none",
        axis.text.x = element_text(angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  xlab("") + 
  ylab("Growing Degree Days to BB") + 
  scale_fill_manual(name="Years", values=cols,
                    labels=c("2016", "2017", "2018", "2019")) +
  #geom_hline(aes(yintercept = meancont), col="black", alpha=0.3, linetype="dashed") +
  #geom_hline(aes(yintercept = meantx), col="black", alpha=1, linetype="dashed")  +
  scale_linetype_manual(name="Site", values=c(1, 2, 3), labels=c("arb", "cg", "hf")) #+
  #guides(fill=FALSE) 
  #ggtitle("A. Four weeks chilling")  + coord_cartesian(xlim=c(1, 8), ylim=c(-5,85), expand=TRUE)

quartz()
gddbar

