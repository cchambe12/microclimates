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
gdd.stan <- read.csv("output/clean_gdd_bbanddvr.csv", header=TRUE)
gdd.hobo <- read.csv("output/clean_gdd_bbanddvr_hobo.csv", header=TRUE)

gdd.hobo$gdd_bb_hobo <- gdd.hobo$gdd_bb
gdd.hobo$gdd_dvr_hobo <- gdd.hobo$gdd_dvr

gdd.hobo$gdd_bb <- NULL
gdd.hobo$gdd_dvr <- NULL

gdd.hobo$spp <- paste(substr(gdd.hobo$genus, 0, 3), substr(gdd.hobo$species, 0, 3), sep="")
gdd.stan$spp <- paste(substr(gdd.stan$genus, 0, 3), substr(gdd.stan$species, 0, 3), sep="")

commoncols <- c("id", "provenance.lat", "fs.count", "year", "budburst", "spp")
gdd.hobo <- subset(gdd.hobo, select=c(commoncols, "gdd_bb_hobo"))
gdd.stan <- subset(gdd.stan, select=c(commoncols, "gdd_bb"))

gdd <- full_join(gdd.stan, gdd.hobo)

gdd <- gdd[(gdd$year==2019),]

gdd <- gdd[(gdd$gdd_bb<=1000),]
gdd <- gdd[(gdd$gdd_bb_hobo<=1000),]

#gdd$gdd.diff <- gdd$gdd_dvr_hobo - gdd$gdd_dvr ## mean = 4.81; range=c(-1.24, 13.97); sd=3.82
#gdd$gdd.diff <- gdd$gdd_bb_hobo - gdd$gdd_bb ## mean = 4.95; range=c(3.48, 9.95); sd=1.14

cols <- colorRampPalette(brewer.pal(8,"Dark2"))(8)

gddplot <- na.omit(gdd)

gddplot <- gather(gddplot, "method", "gdd", -id, -provenance.lat, -fs.count, -year, -budburst, -spp)

gddplot$gddmean <- ave(gddplot$gdd, gddplot$spp, gddplot$method)
gddplot$gddsd <- ave(gddplot$gdd, gddplot$spp, gddplot$method, FUN=sd)
gddplot$ymin <- gddplot$gddmean-gddplot$gddsd
gddplot$ymax <- gddplot$gddmean+gddplot$gddsd

gddplot <- na.omit(gddplot)


gddbar <- ggplot(gddplot, aes(x=spp, y=gddmean, fill=method)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=ymin, ymax=ymax),width = 0.2, position=position_dodge(0.9)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        #legend.position = "none",
        axis.text.x = element_text(angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  xlab("") + 
  ylab("Growing Degree Days between Leafout and Budburst") + 
  scale_fill_manual(name="Method", values=cols,
                    labels=c("Weather Station", "Hobo Logger")) #+
  #geom_hline(aes(yintercept = meancont), col="black", alpha=0.3, linetype="dashed") +
  #geom_hline(aes(yintercept = meantx), col="black", alpha=1, linetype="dashed")  +
  #scale_linetype_manual(name="Site", values=c(1, 2, 3), labels=c("arb", "cg", "hf")) #+
  #guides(fill=FALSE) 
  #ggtitle("A. Four weeks chilling")  + coord_cartesian(xlim=c(1, 8), ylim=c(-5,85), expand=TRUE)

quartz()
gddbar


