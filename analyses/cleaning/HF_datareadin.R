### Load in all John O'Keefe Data
# 24 January 2019 - Cat

# Load from bb_cleanmergeall.R (including libraries)

if(is.data.frame(ok15)){
  
ok15$year <- 2015
ok15$JULIAN <- as.integer(ok15$JULIAN)
ok15$CIRCUIT <- as.integer(ok15$CIRCUIT)
ok15$BBRK <- as.integer(ok15$BBRK)
ok15$FOPN <- as.integer(ok15$FOPN)
ok15$L75 <- as.integer(ok15$L75)
ok15$L95 <- as.integer(ok15$L95)
ok16<-read.csv("output/okeefe2016.csv", header=TRUE)
ok16$year <- 2016
jok <- full_join(ok15, ok16)
ok17<-read.csv("output/okeefe2017.csv", header=TRUE)
ok17$year <- 2017
jok <- full_join(jok, ok17)
ok18<-read.csv("output/okeefe2018.csv", header=TRUE)
ok18$year <- 2018
ok18$FPST <- as.character(ok18$FPST)
ok18$LFIN <- as.character(ok18$LFIN)
jok <- full_join(jok, ok18)

} else {
  print("Error: jok not a data.frame")
}

stop("Not an error, just loading in John O'Keefe data into one file. Also, you can ignore the warning message below -- code converts a column to character, but the column is created in a different dataframe that is used and deleted in this source code and should not (that I can imagine) have any other impact.")

