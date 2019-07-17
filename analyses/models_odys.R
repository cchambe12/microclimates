### 17 July 2019
## Libraries
require(rstan)
require(brms)

options(stringsAsFactors = FALSE)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

gdd.stan<-read.csv("/n/wolkovich_lab/Lab/Cat/clean_gdd_bbanddvr.csv", header=TRUE)

gdd.stan$spp <- paste(substr(gdd.stan$genus, 0, 3), substr(gdd.stan$species,0,3), sep="")
gdd.stan$site <- NA
gdd.stan$site <- ifelse(gdd.stan$type=="Treespotters", "arb", gdd.stan$site)
gdd.stan$site <- ifelse(gdd.stan$type=="Common Garden", "cg", gdd.stan$site)
gdd.stan$site <- ifelse(gdd.stan$type=="Harvard Forest", "hf", gdd.stan$site)

gdd.stan <- subset(gdd.stan, select=c("id", "provenance.lat", "spp", "site",
                                      "gdd_bb", "gdd_dvr", "fs.count"))

gdd.stan <- gdd.stan[!is.na(gdd.stan$provenance.lat),]
gdd.stan <- gdd.stan[!is.na(gdd.stan$gdd_bb),]
gdd.stan <- gdd.stan[!is.na(gdd.stan$gdd_dvr),]
gdd.stan <- gdd.stan[!is.na(gdd.stan$spp),]

gdd.stan <- gdd.stan[(gdd.stan$gdd_bb<1000),]
gdd.stan <- gdd.stan[(gdd.stan$gdd_dvr<1000),]

mod.gddbb <- brm(gdd_bb ~ spp*site + provenance.lat, data=gdd.stan, chains=4,
                 control=list(max_treedepth = 15,adapt_delta = 0.99))

save(mod.gddbb, file="/n/wolkovich_lab/Lab/Cat/modgddbb.Rdata")


mod.gdddvr <- brm(gdd_dvr ~ spp*site + provenance.lat, data=gdd.stan, chains=4,
                 control=list(max_treedepth = 15,adapt_delta = 0.99))

save(mod.gdddvr, file="/n/wolkovich_lab/Lab/Cat/modgdddvr.Rdata")
