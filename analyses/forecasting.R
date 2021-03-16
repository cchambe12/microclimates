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

gdd.warm <- warmfunc(150, 350, 10, 5, 10)
gdd.warm <- gdd.warm[[1]]

pdf("figures/forecasting_base10.pdf", width=6, height=5)
ggplot(gdd.warm, aes(as.numeric(warming), gdd_accuracy, col=fstarspp, group=fstarspp)) +
  geom_line(stat='smooth', method = "lm", alpha=1, aes(col=fstarspp)) +
  #geom_point(aes(as.numeric(warming), gdd_accuracy, col=fstarspp, group=fstarspp)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.key = element_rect(colour = "transparent", fill = "white"),
        plot.margin = margin(0.5, 0.5, 0.5, 1, "cm")) + 
  xlab("Amount of warming (°C)") + ylab("GDD accuracy\n(estimated/observed)") +
  scale_x_continuous(breaks = sort(c(seq(0, 10, by=1)))) +
  scale_color_viridis_c("Species GDD Threshold")
dev.off()

gdd.warm <- warmfunc(150, 350, 10, 5, 5)[[1]]

pdf("figures/forecasting_base5.pdf", width=6, height=5)
ggplot(gdd.warm, aes(as.numeric(warming), gdd_ratio, col=fstarspp, group=fstarspp)) +
  geom_line(stat='smooth', method = "lm", alpha=1, aes(col=fstarspp)) +
  #geom_point(aes(as.numeric(warming), gdd_accuracy, col=fstarspp, group=fstarspp)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.key = element_rect(colour = "transparent", fill = "white"),
        plot.margin = margin(0.5, 0.5, 0.5, 1, "cm")) + 
  xlab("Amount of warming (°C)") + ylab("GDD accuracy\n(estimated/observed)") +
  scale_x_continuous(breaks = sort(c(seq(0, 10, by=1)))) +
  scale_color_viridis_c("Species GDD Threshold")
dev.off()


gdd.warm <- warmfunc(150, 350, 10, 5, 0)[[1]]

pdf("figures/forecasting_base0.pdf", width=6, height=5)
ggplot(gdd.warm, aes(as.numeric(warming), gdd_ratio, col=fstarspp, group=fstarspp)) +
  geom_line(stat='smooth', method = "lm", alpha=1, aes(col=fstarspp)) +
  #geom_point(aes(as.numeric(warming), gdd_accuracy, col=fstarspp, group=fstarspp)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.key = element_rect(colour = "transparent", fill = "white"),
        plot.margin = margin(0.5, 0.5, 0.5, 1, "cm")) + 
  xlab("Amount of warming (°C)") + ylab("GDD accuracy\n(estimated/observed)") +
  scale_x_continuous(breaks = sort(c(seq(0, 10, by=1)))) +
  scale_color_viridis_c("Species GDD Threshold")
dev.off()

gdd.warm <- warmfunc(150, 350, 10, 5, -5)[[1]]

pdf("figures/forecasting_base-5.pdf", width=6, height=5)
ggplot(gdd.warm, aes(as.numeric(warming), gdd_ratio, col=fstarspp, group=fstarspp)) +
  geom_line(stat='smooth', method = "lm", alpha=1, aes(col=fstarspp)) +
  geom_point(aes(as.numeric(warming), gdd_accuracy, col=fstarspp, group=fstarspp)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.key = element_rect(colour = "transparent", fill = "white"),
        plot.margin = margin(0.5, 0.5, 0.5, 1, "cm")) + 
  xlab("Amount of warming (°C)") + ylab("GDD accuracy\n(estimated/observed)") +
  scale_x_continuous(breaks = sort(c(seq(0, 10, by=1)))) +
  scale_color_viridis_c("Species GDD Threshold")
dev.off()

