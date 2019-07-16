### 15 July 2019 - Cat
## Prep for adding in hobo logger data

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load libraries
library(lubridate)
library(weathermetrics)

## Now for the test data for calibrating
setwd("~/Documents/git/microclimates/hf_data/calibrate/")
mycsv = dir("~/Documents/git/microclimates/hf_data/calibrate/", pattern=".csv")

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
x$temp <- fahrenheit.to.celsius(x$temp) ;
x$tempdiff <- 4 - x$temp ;
x$meandiff <- mean(x$tempdiff) ;
return(x)})

# then open in environment separately:
for (i in seq(mylist))
  assign(paste0("hf_test", i), mylist[[i]])

setwd("~/Documents/git/microclimates/hf_data/")
mycsv = dir("~/Documents/git/microclimates/hf_data", pattern=".csv")

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
x$temp <- fahrenheit.to.celsius(x$temp) ;
return(x)})

# then open in environment separately:
for (i in seq(mylist))
  assign(paste0("hf", i), mylist[[i]])

for(i in c(1:nrow(hf1))) {
  hf1$tempcalib[i] <- hf1$temp[i] - unique(hf_test1$meandiff)
  hf1$climatetype <- "hf1"
}
write.csv(hf1, file="~/Documents/git/microclimates/analyses/output/hfclimdata/hf001clim.csv", row.names=FALSE)

for(i in c(1:nrow(hf2))) {
  hf2$tempcalib[i] <- hf2$temp[i] - unique(hf_test2$meandiff)
  hf2$climatetype <- "hf2"
}
write.csv(hf2, file="~/Documents/git/microclimates/analyses/output/hfclimdata/hf002clim.csv", row.names=FALSE)

for(i in c(1:nrow(hf3))) {
  hf3$tempcalib[i] <- hf3$temp[i] - unique(hf_test3$meandiff)
  hf3$climatetype <- "hf3"
}
write.csv(hf3, file="~/Documents/git/microclimates/analyses/output/hfclimdata/hf003clim.csv", row.names=FALSE)

for(i in c(1:nrow(hf4))) {
  hf4$tempcalib[i] <- hf4$temp[i] - unique(hf_test4$meandiff)
  hf4$climatetype <- "hf4"
}
write.csv(hf4, file="~/Documents/git/microclimates/analyses/output/hfclimdata/hf004clim.csv", row.names=FALSE)

for(i in c(1:nrow(hf5))) {
  hf5$tempcalib[i] <- hf5$temp[i] - unique(hf_test5$meandiff)
  hf5$climatetype <- "hf5"
}
write.csv(hf5, file="~/Documents/git/microclimates/analyses/output/hfclimdata/hf005clim.csv", row.names=FALSE)

for(i in c(1:nrow(hf6))) {
  hf6$tempcalib[i] <- hf6$temp[i] - unique(hf_test6$meandiff)
  hf6$climatetype <- "hf6"
}
write.csv(hf6, file="~/Documents/git/microclimates/analyses/output/hfclimdata/hf006clim.csv", row.names=FALSE)

for(i in c(1:nrow(hf7))) {
  hf7$tempcalib[i] <- hf7$temp[i] - unique(hf_test7$meandiff)
  hf7$climatetype <- "hf7"
}
write.csv(hf7, file="~/Documents/git/microclimates/analyses/output/hfclimdata/hf007clim.csv", row.names=FALSE)

if(FALSE){
for(i in c(1:nrow(hf8))) {
  hf8$tempcalib[i] <- hf8$temp[i] - unique(hf_test8$meandiff)
  hf8$climatetype <- "hf8"
}
write.csv(hf8, file="~/Documents/git/microclimates/analyses/output/hfclimdata/hf008clim.csv", row.names=FALSE)
}

for(i in c(1:nrow(hf9))) {
  hf9$tempcalib[i] <- hf9$temp[i] - unique(hf_test9$meandiff)
  hf9$climatetype <- "hf9"
}
write.csv(hf9, file="~/Documents/git/microclimates/analyses/output/hfclimdata/hf009clim.csv", row.names=FALSE)

if(FALSE){
for(i in c(1:nrow(hf10))) {
  hf10$tempcalib[i] <- hf10$temp[i] - unique(hf_test10$meandiff)
  hf10$climatetype <- "hf10"
}
write.csv(hf10, file="~/Documents/git/microclimates/analyses/output/hfclimdata/hf010clim.csv", row.names=FALSE)
}

for(i in c(1:nrow(hf11))) {
  hf11$tempcalib[i] <- hf11$temp[i] - unique(hf_test11$meandiff)
  hf11$climatetype <- "hf11"
}
write.csv(hf11, file="~/Documents/git/microclimates/analyses/output/hfclimdata/hf011clim.csv", row.names=FALSE)

for(i in c(1:nrow(hf12))) {
  hf12$tempcalib[i] <- hf12$temp[i] - unique(hf_test12$meandiff)
  hf12$climatetype <- "hf12"
}
write.csv(hf12, file="~/Documents/git/microclimates/analyses/output/hfclimdata/hf012clim.csv", row.names=FALSE)

for(i in c(1:nrow(hf13))) {
  hf13$tempcalib[i] <- hf13$temp[i] - unique(hf_test13$meandiff)
  hf13$climatetype <- "hf13"
}
write.csv(hf13, file="~/Documents/git/microclimates/analyses/output/hfclimdata/hf013clim.csv", row.names=FALSE)

for(i in c(1:nrow(hf14))) {
  hf14$tempcalib[i] <- hf14$temp[i] - unique(hf_test14$meandiff)
  hf14$climatetype <- "hf14"
}
write.csv(hf14, file="~/Documents/git/microclimates/analyses/output/hfclimdata/hf014clim.csv", row.names=FALSE)




