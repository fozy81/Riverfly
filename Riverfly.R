#### Script to read Excel files and compare Riverfly results into single table for analysis - basically, Excel sucks

### load libraries (must be installed first)

library(gdata)  # library for importing excel files
library(ggplot2) # graph plotting library

setwd("/home/tim/Dropbox/Site Data") # set working directory - this is where the Riverfly excel files

#### Get list of excel files from directory and sub-directories

files  <- list.files(pattern ='xls', full.names = T, recursive = T) 

# get a list of file names + the 'full.names' which means
# it has the directory + file name. Recursive = T - means it goes into all sub-directories/folders that are in our working directory.
filesList <- list(files)
files <- filesList[[1]][-15] 
# make files object into a list rather than a string of characters

All <- lapply(files,function(i){ 
  read.xls(i)
})

# funtion to read excel data into R - the data is in a 'list' of 'dataframes' - 'lapply' applies read.xls function to list of files

myData2 <- All 

# quick name change because of re-writing this script! me bad.

### Start doing stuff with excel data

All <- lapply(myData2,function(i){ 
#### function to get values
  
  data2 <- i[4:15,4:(length(colnames(i)))]

   data3 <- data2[4:12,3:(length(colnames(data2)))]


  ld3 <- length(colnames(data3)) - 1
  
  dataList2 <- unlist(c(data3[,1:ld3]))
  dataF2 <- data.frame(dataList2)
 
#### get names

ld4 <- length(rownames(dataF2)) 
ld5 <- ld4 / 9
taxa <- 0 
data4 <- data.frame(data2[4:12,1])
for (i in 1:ld5){
taxa[i] <- data4
}
  taxa2 <- data.frame(taxa)
 
  taxa3 <- unlist(c(taxa2[,1:ld5]))
taxa4 <- data.frame(taxa3)

dataF2$taxa <- taxa4$taxa3

## get date

dataF2$date <- 0
dataF2$date[1:18] <- paste(data2[1,3])
dataF2$date[19:36] <- paste(data2[1,5])
dataF2$date[37:54] <- paste(data2[1,7])
dataF2$date[55:72] <- paste(data2[1,9])
dataF2$date[73:90] <- paste(data2[1,11])
dataF2$date[91:108] <- paste(data2[1,13])
dataF2$date[109:126] <- paste(data2[1,15])
dataF2$date[127:144] <- paste(data2[1,17])
dataF2$date[145:162] <- paste(data2[1,19])
dataF2$date[163:180] <- paste(data2[1,21])
dataF2$date[181:198] <- paste(data2[1,23])
dataF2$date[199:216] <- paste(data2[1,25])
dataF2$date[217:234] <- paste(data2[1,27])
  return(dataF2)
})

## Get Site name
for (i in 1:length(myData2)){
  All[[i]]$site <- myData2[[i]][1,11]
}

All2 <- lapply(All,function(i){
#### cleaning
  
  dataF2 <- i
  dataF2$dataList2 <- as.character(dataF2$dataList2)
 dataF2$taxa <- as.character(dataF2$taxa)
dataClean <- dataF2[!dataF2$dataList2 == "",] 
  for (i in 1:length(dataClean$dataList2)){
  if (dataClean$dataList2[i] == "A" | dataClean$dataList2[i] == "B" |dataClean$dataList2[i] == "C" |dataClean$dataList2[i] == "D" |dataClean$dataList2[i] == "E"){
    dataClean$taxa[i] <- paste("RECORDED LOG ", dataClean$taxa[i], sep="")
 }}
 #for (i in 1:length(dataClean$dataList2)){
   # if (dataClean$dataList2[i] == "A" | dataClean$dataList2[i] == "B" |dataClean$dataList2[i] == "C" |dataClean$dataList2[i] == "D" |dataClean$dataList2[i] == "E"){
  #dataClean$log[i] <-dataClean$dataList2[i]
 # }}
#dataClean <- dataClean[!dataClean$dataList2 == "B" & !dataClean$dataList2 == "A" & !dataClean$dataList2 == "C" & !dataClean$dataList2 == "D" & !dataClean$dataList2 == "E" & !dataClean$dataList2 == "0",] 
 # dataClean  <-  dataClean[!dataClean$dataList2 == "0",] 
#dataClean$log[dataClean$dataList2 < 10] <- "A"
#dataClean$log[dataClean$dataList2 > 9 & dataClean$dataList2 < 100] <- "B"
#dataClean$log[dataClean$dataList2 > 99 & dataClean$dataList2 < 1000] <- "C"
#dataClean$log[dataClean$dataList2 > 999 & dataClean$dataList2 < 10000] <- "D"
#dataClean$log[dataClean$dataList2 > 10000] <- "E"

dataClean$value <- dataClean$dataList2
dataClean$dataList2 <- NULL
dataClean$name <- dataClean$taxa
dataClean$taxa <- NULL

  
 # dataClean$date <- sub('([\\.])', '/', dataClean$date)
  
  
  
#  dataClean$date <- format(as.Date(dataClean$date), "%d/%m/%Y")
  
dataClean$date  <- strptime(dataClean$date, "%d/%m/%y")
rownames(dataClean) <- NULL

return(dataClean)
})


dataClean <- do.call("rbind", All2)

dataClean <- dataClean[dataClean$value != 0,]
dataClean <- dataClean[dataClean$value != " ",]
dataClean$value <- as.numeric(dataClean$value)

### plot

 qplot(dataClean$date[!dataClean$name == "Total" & !dataClean$value == "NA"],dataClean$value[!dataClean$name == "Total" & !dataClean$value == "NA"], color=dataClean$name[!dataClean$name == "Total" & !dataClean$value == "NA"])

qplot(dataClean$date[dataClean$name == "Total" & !dataClean$value == "NA"],dataClean$value[dataClean$name == "Total" & !dataClean$value == "NA"], color=dataClean$site[dataClean$name == "Total" & !dataClean$value == "NA"])

### Dunctocher A810 - no date for sample, Dalmuir.xks - weird

setwd("/home/tim/R/Riverfly") 


