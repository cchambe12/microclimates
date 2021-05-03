### Interpreting the posteriors to disentangle the interaction of urban by method
# Started 3 May 2021 by Cat

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())

#### Overall model:
# GDD ~ urban + method + method*urban + (urban + method + method*urban|species) 

library(RColorBrewer)
library(viridis)
library(ggplot2)
library(gridExtra)
library(rstan)

setwd("~/Documents/git/microclimates/analyses/")

realgdd <- read.csv("output/cleanmicro_gdd_2019.csv")


cols <- viridis_pal(option="viridis")(4)

realbox <- ggplot(bball, aes(x=sitemethod, y=gdd_bb)) + geom_boxplot(aes(col=sitemethod, fill=sitemethod)) +
  scale_color_manual(name="Site and Method", values = cols, 
                     labels=c("arb0"="Arboretum - Hobo Logger",
                              "arb1"="Arboretum - Weather Station",
                              "hf0"="Harvard Forest - Hobo Logger",
                              "hf1"="Harvard Forest - Weather Station")) +
  scale_fill_manual(name="Site and Method", values = cols, 
                     labels=c("arb0"="Arboretum - Hobo Logger",
                              "arb1"="Arboretum - Weather Station",
                              "hf0"="Harvard Forest - Hobo Logger",
                              "hf1"="Harvard Forest - Weather Station")) +
  ylab("Growing Degree Days") + xlab("") +
  theme_classic() +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) 

pdf(file.path("~/Documents/git/microclimates/analyses/figures/", "gdd_comparemethodsandsites_boxplot.pdf"),
    width = 8, height = 6)
realbox
dev.off()
   
bball$methodtype <- ifelse(bball$method=="ws", "\nWeather \nStation", "\nHobo \nLogger")
  
cols <- viridis_pal(option="plasma")(3)
gddcomparebb <- ggplot(bball, aes(x=methodtype, y=gdd_bb, group=site, fill=site)) + 
    geom_ribbon(stat='smooth', method = "lm", se=TRUE, alpha=1, 
                aes(fill = site, group = site)) +
    geom_line(stat='smooth', method = "lm", alpha=1, col="black") +
    theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
          legend.text.align = 0,
          legend.key = element_rect(colour = "transparent", fill = "white"),
          plot.margin = margin(0.5, 0.5, 0.5, 1, "cm")) +
    xlab("") + 
    ylab("Growing degree days to budburst") + 
    scale_fill_manual(name="Site", values=cols,
                      labels=c("arb"="Arnold Arboretum", "hf"="Harvard Forest")) + 
    coord_cartesian(expand=0, ylim=c(0,700))
  
pdf(file.path("~/Documents/git/microclimates/analyses/figures/", "gdd_comparemethodsandsites.pdf"),
    width = 8, height = 6)
gddcomparebb
dev.off()
  


######## Posterior Predictive Checks ####
load("stan/urbmethod_real.Rdata")
modoutput <- rstan::extract(urbmethod)
yrep <- modoutput$yhat

dats <- rbind(data.frame(pred = bball$gdd_bb, var = 'y'),
              data.frame(pred = as.vector(yrep)[1:2000], var = 'yrep'))

yrep <- data.frame(yrep)

dats$sitemethod <- c(bball$sitemethod, rep("posterior", 2000))
  
preds <- ggplot(yrep, aes(x=yrep)) + geom_histogram() + theme_classic()

yrep<-as.vector(yrep[sample(nrow(yrep), 2000), ])[1:411]



um <- rstan::extract(urbmethod, pars="betas")

plot(urbmethod, show_density = TRUE, ci_level = 0.5, fill_color = "purple")

plot(urbmethod, plotfun = "hist", pars = "mu_um_sp", include = FALSE)

bayesplot::ppc_stat_grouped(realgdd$gdd_bb, yrep, group = c(realgdd$urban, realgdd$method), stat = "prop_zero")

cols <- viridis_pal(option="viridis")(4)
bball$sitemethod <- paste0(bball$site, bball$type)

ggplot(bball, aes(x=gdd_bb, y=yrep[1:411], col=as.factor(sitemethod), alpha=as.factor(sitemethod))) + geom_point() +
         theme_classic() + geom_abline(slope=1, intercept=0) + coord_cartesian(xlim=c(100, 700), ylim=c(100,700)) +
  scale_color_manual(name="Site and Method", values = c(cols[1], cols[1], cols[2], cols[2]), 
                     labels=c("10"="Arboretum - Hobo Logger",
                              "11"="Arboretum - Weather Station",
                              "00"="Harvard Forest - Hobo Logger",
                              "01"="Harvard Forest - Weather Station")) +
  scale_alpha_manual(name="Site and Method", values = c(0.3, 1, 0.3, 1), 
                     labels=c("10"="Arboretum - Hobo Logger",
                              "11"="Arboretum - Weather Station",
                              "00"="Harvard Forest - Hobo Logger",
                              "01"="Harvard Forest - Weather Station"))

gddsraw <- ggplot(bball)


