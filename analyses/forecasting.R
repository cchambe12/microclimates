### 24 February 2021 by Cat
## Forecasting GDD accuracy with warming

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#Load Libraries
library(viridis)
library(ggplot2)
library(gridExtra)

source("~/Documents/git/microclimates/analyses/source/sims_warm_sourcedata.R")

gdd.warm <- warmfunc(300, 50, 10)[[1]]

quartz()
ggplot(gdd.warm, aes(as.numeric(warming), abs(gdd_accuracy), col=fstarspp, group=fstarspp)) +
  geom_line(stat='smooth', method = "loess", alpha=1, aes(col=fstarspp)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.key = element_rect(colour = "transparent", fill = "white"),
        plot.margin = margin(0.5, 0.5, 0.5, 1, "cm")) + 
  xlab("Amount of warming (°C)") + ylab("GDD accuracy") +
  scale_x_continuous(breaks = sort(c(seq(0, 10, by=1)))) +
  scale_color_viridis_c("Species GDD Threshold")
