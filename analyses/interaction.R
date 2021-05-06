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
bball <- realgdd
bball$site <- ifelse(bball$urban==1, "arb", "hf")
bball$sitemethod <- paste0(bball$site, bball$method)


cols <- viridis_pal(option="viridis")(4)

realbox <- ggplot(bball, aes(x=sitemethod, y=gdd_bb)) + geom_boxplot(aes(col=sitemethod, fill=sitemethod)) +
  scale_color_manual(name="Site and Method", values = cols, 
                     labels=c("arb0"="Urban site: hobo logger",
                              "arb1"="Urban site: weather station",
                              "hf0"="Forest site: hobo logger",
                              "hf1"="Forest site: weather station")) +
  scale_fill_manual(name="Site and Method", values = cols, 
                     labels=c("arb0"="Urban site: hobo logger",
                              "arb1"="Urban site: weather station",
                              "hf0"="Forest site: hobo logger",
                              "hf1"="Forest site: weather station")) +
  ylab("Growing Degree Days") + xlab("") +
  theme_classic() +
  ggtitle("a)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) 

pdf(file.path("~/Documents/git/microclimates/analyses/figures/", "gdd_comparemethodsandsites_boxplot.pdf"),
    width = 8, height = 6)
realbox
dev.off()
   
###### Model output
load("stan/urbmethod_real.Rdata")
#modoutput <- rstan::extract(urbmethod)

urbmethod.sum <- summary(urbmethod)$summary

hfhobo <- urbmethod.sum["mu_a_sp", ]
hfws <- urbmethod.sum["mu_a_sp", ] + urbmethod.sum["mu_b_method_sp", ]
arbhobo <- urbmethod.sum["mu_a_sp", ] + urbmethod.sum["mu_b_urban_sp", ]
arbws <- urbmethod.sum["mu_a_sp", ] + urbmethod.sum["mu_b_urban_sp", ] + urbmethod.sum["mu_b_method_sp", ] + urbmethod.sum["mu_b_um_sp", ]

mod_inter <- data.frame(rbind(hfhobo, hfws, arbhobo, arbws))
mod_inter$sitemethod <- c("hfhobo", "hfws", "arbhobo", "arbws")
mod_inter$site <- c("hf", "hf", "arb", "arb")
mod_inter$method <- c("hobo", "ws", "hobo", "ws")

cols <- viridis_pal(option="plasma")(4)
modinter_plot <- ggplot(mod_inter) + geom_point(aes(x=method, y=mean, col=as.factor(site))) +
  geom_linerange(aes(ymin=`X25.`, ymax=`X75.`, x=method, col=as.factor(site)), alpha=0.3) + 
  geom_line(aes(col=as.factor(site), x=method, y=mean, group=as.factor(site))) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.key = element_rect(colour = "transparent", fill = "white"),
        plot.margin = margin(0.5, 0.5, 0.5, 1, "cm")) +
  xlab("") + 
  ggtitle("b)") +
  ylab("Model estimate of GDD to budburst") + 
  scale_color_manual(name="Site", values=cols,
                    labels=c("arb"="Urban site", "hf"="Forest site")) +
  scale_x_discrete(labels=c("hobo" ="Hobo Logger","ws"= "Weather Station")) +
  scale_y_continuous(breaks=seq(min(300), max(450), by=20)) +
  coord_cartesian(xlim=c(0.9, 2.1), expand=FALSE, ylim=c(300, 450))

pdf(file.path("~/Documents/git/microclimates/analyses/figures/", "gdd_modoutput_inter.pdf"),
    width = 8, height = 6)
modinter_plot
dev.off()

pdf(file.path("~/Documents/git/microclimates/analyses/figures/", "gdd_interaction.pdf"),
    width = 12, height = 4, onefile=FALSE)
egg::ggarrange(realbox, modinter_plot, ncol=2)
dev.off()



#########################################################################################
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
                    labels=c("arb"="Urban site", "hf"="Forest site")) + 
  coord_cartesian(expand=0, ylim=c(0,700))

pdf(file.path("~/Documents/git/microclimates/analyses/figures/", "gdd_comparemethodsandsites.pdf"),
    width = 8, height = 6)
gddcomparebb
dev.off()


