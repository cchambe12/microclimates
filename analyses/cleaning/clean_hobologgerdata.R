### 15 July 2019 - Cat
## Prep for adding in hobo logger data

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load libraries
library(lubridate)

## Now for the test data for calibrating
setwd("~/Documents/git/microclimates/arb_data/calibrate/")
mycsv = dir("~/Documents/git/microclimates/arb_data/calibrate/", pattern=".csv")

n <- length(mycsv)
mylist <- vector("list", n)

for(i in 1:n) mylist[[i]] <- read.csv(mycsv[i])

# now change something in all dfs in list:
mylist <- lapply(mylist, function(x) 
  {names(x) <- c("date.time", "temp") ;
  x <-x[4:19,] ;
  x$date<-gsub("\\s* .*$", '', x$date.time) ;
  x$date<- as.Date(x$date, "%m/%d/%Y") ;
  x$date<-as.Date(gsub("001", "201", x$date)) ;
  x$year<-substr(x$date, 0, 4) ;
  x$doy<-yday(x$date) ;
  x$hour<-gsub("^.* \\s*|\\s*:.*$", '', x$date.time) ;
  x$tempdiff <- 4 - x$temp ;
  x$meandiff <- mean(x$tempdiff) ;
  return(x)})

# then open in environment separately:
for (i in seq(mylist))
  assign(paste0("arb_test", i), mylist[[i]])

setwd("~/Documents/git/microclimates/arb_data/")
mycsv = dir("~/Documents/git/microclimates/arb_data", pattern=".csv")

n <- length(mycsv)
mylist <- vector("list", n)

for(i in 1:n) mylist[[i]] <- read.csv(mycsv[i])

# now change something in all dfs in list:
mylist <- lapply(mylist, function(x) 
  {names(x) <- c("date.time", "temp") ; 
  x$date<-gsub("\\s* .*$", '', x$date.time) ;
  x$date<- as.Date(x$date, "%m/%d/%Y") ;
  x$date<-as.Date(gsub("001", "201", x$date)) ;
  x$year<-substr(x$date, 0, 4) ;
  x$doy<-yday(x$date) ;
  x$hour<-gsub("^.* \\s*|\\s*:.*$", '', x$date.time) ;
  return(x)})

# then open in environment separately:
for (i in seq(mylist))
  assign(paste0("arb", i), mylist[[i]])


for(i in c(1:nrow(arb2))) {
    arb2$tempcalib[i] <- arb2$temp[i] - unique(arb_test2$meandiff)
    arb2$climatetype <- "arb2"
}
write.csv(arb2, file="~/Documents/git/microclimates/analyses/output/arb2clim.csv", row.names=FALSE)
for(i in c(1:nrow(arb3))) {
  arb3$tempcalib[i] <- arb3$temp[i] - unique(arb_test3$meandiff)
  arb3$climatetype <- "arb3"
}
write.csv(arb3, file="~/Documents/git/microclimates/analyses/output/arb3clim.csv", row.names=FALSE)
for(i in c(1:nrow(arb4))) {
  arb4$tempcalib[i] <- arb4$temp[i] - unique(arb_test4$meandiff)
  arb4$climatetype <- "arb4"
}
write.csv(arb4, file="~/Documents/git/microclimates/analyses/output/arb4clim.csv", row.names=FALSE)
for(i in c(1:nrow(arb5))) {
  arb5$tempcalib[i] <- arb5$temp[i] - unique(arb_test5$meandiff)
  arb5$climatetype <- "arb5"
}
write.csv(arb5, file="~/Documents/git/microclimates/analyses/output/arb5clim.csv", row.names=FALSE)
for(i in c(1:nrow(arb7))) {
  arb7$tempcalib[i] <- arb7$temp[i] - unique(arb_test7$meandiff)
  arb7$climatetype <- "arb7"
}
write.csv(arb7, file="~/Documents/git/microclimates/analyses/output/arb7clim.csv", row.names=FALSE)
for(i in c(1:nrow(arb8))) {
  arb8$tempcalib[i] <- arb8$temp[i] - unique(arb_test8$meandiff)
  arb8$climatetype <- "arb8"
}
write.csv(arb8, file="~/Documents/git/microclimates/analyses/output/arb8clim.csv", row.names=FALSE)
for(i in c(1:nrow(arb9))) {
  arb9$tempcalib[i] <- arb9$temp[i] - unique(arb_test9$meandiff)
  arb9$climatetype <- "arb9"
}
write.csv(arb9, file="~/Documents/git/microclimates/analyses/output/arb9clim.csv", row.names=FALSE)
for(i in c(1:nrow(arb10))) {
  arb10$tempcalib[i] <- arb10$temp[i] - unique(arb_test10$meandiff)
  arb10$climatetype <- "arb10"
}
write.csv(arb10, file="~/Documents/git/microclimates/analyses/output/arb10clim.csv", row.names=FALSE)
for(i in c(1:nrow(arb11))) {
  arb11$tempcalib[i] <- arb11$temp[i] - unique(arb_test11$meandiff)
  arb11$climatetype <- "arb11"
}
write.csv(arb11, file="~/Documents/git/microclimates/analyses/output/arb11clim.csv", row.names=FALSE)
for(i in c(1:nrow(arb12))) {
  arb12$tempcalib[i] <- arb12$temp[i] - unique(arb_test12$meandiff)
  arb12$climatetype <- "arb12"
}
write.csv(arb12, file="~/Documents/git/microclimates/analyses/output/arb12clim.csv", row.names=FALSE)
for(i in c(1:nrow(arb13))) {
  arb13$tempcalib[i] <- arb13$temp[i] - unique(arb_test13$meandiff)
  arb13$climatetype <- "arb13"
}
write.csv(arb13, file="~/Documents/git/microclimates/analyses/output/arb13clim.csv", row.names=FALSE)
for(i in c(1:nrow(arb14))) {
  arb14$tempcalib[i] <- arb14$temp[i] - unique(arb_test14$meandiff)
  arb14$climatetype <- "arb14"
}
write.csv(arb14, file="~/Documents/git/microclimates/analyses/output/arb14clim.csv", row.names=FALSE)
for(i in c(1:nrow(arb15))) {
  arb15$tempcalib[i] <- arb15$temp[i] - unique(arb_test15$meandiff)
  arb15$climatetype <- "arb15"
}
write.csv(arb15, file="~/Documents/git/microclimates/analyses/output/arb15clim.csv", row.names=FALSE)




