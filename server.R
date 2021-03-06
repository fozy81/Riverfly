library(shiny)
library(ggplot2)
library(RCurl)
library(reshape)
library(plyr)Y
library(scales)
library(httr)

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output,session) {

# URL for google spreadsheet containing raw riverfly data
myCsv <- getURL("https://docs.google.com/spreadsheets/d/1fDvRtuqolZO7bL-rhEzyCXl7cAOXLWi9a-x56rQTgYQ/export?gid=0&format=csv")
# Download data from google spreadsheet (which has been submitted via google form):
  csv1 <- read.csv(textConnection(myCsv),check.names=FALSE)

# remove "\" character from Site name as this is a special escape character and causes issues in R code
csv1$Site <- gsub("\\\"", "", csv1$Site)
csv1$Site <- gsub(",","",csv1$Site) # commas not working for hash/url creation
# Convert 'Survey date' into a time/date data type in R i.e. R will recognise this as a date not a string of characters
  csv1$'Survey date' <- strptime(csv1$'Survey date', "%m/%d/%Y")

# Format date into day, month, year format (this is the commonly used format in UK)
  csv1$'Survey date' <- format(csv1$'Survey date', "%d/%m/%y")
# use melt function to convert from wide format to long format data - google wide/long data format for details. This puts invert groups into single column rather than multiple columns
  csv2 <- melt(csv1, id.vars=c("Site", "Survey date", "CC0","Comments","Timestamp")) # pivots table from google docs with fly names in one column

# convert date to character because ddply function doesn't like time format 
  csv2$dateClean <- as.character(csv2$'Survey date')

     # create log abundance scores/riverfly score for each fly record:
  csv2$log[csv2$value < 1] <- 0
  csv2$log[csv2$value >= 1 & csv2$value < 10] <- 1
  csv2$log[csv2$value >= 10 & csv2$value < 100] <- 2
  csv2$log[csv2$value >= 100 & csv2$value < 1000] <- 3
  csv2$log[csv2$value >= 1000 & csv2$value < 100000] <- 4


#csv2[csv2$Site == 'Antermony Loch inflow u/s Antermony Loch' & csv2$dateClean == '24/10/13', 3:8]
  # create a summary riverfly score for each sample (site + date) - this could cause a problem if more than one sample taken on same day at same site:
  dataClean <- ddply(csv2, ~ dateClean + Site + Timestamp,
                     summarize, Total=sum(log))

# convert date back to date format now it has gone through ddply function
  dataClean$date  <-  as.Date(dataClean$dateClean, "%d/%m/%y")
# rename date to something more readable
  dataClean$'Survey Date' <- dataClean$date
# create trigger level !!
 dataClean$trigger <- 3 
# rename to something more readable
  dataClean$'Default Trigger Level'   <- dataClean$trigger
# rename 'Total' riverfly score to something more readable
  dataClean$'Combined Riverfly Score' <- dataClean$Total



# Data for 'All sites' tab
# combine d and dataClean for full data with trigger & riverfly score values for new tab containing all data in one
  dataFull <- merge(dataClean,csv1,by.x = c("Site","dateClean"), by.y = c("Site", "Survey date"))
dataFull[,3] <- NULL # Timestamp used to stop duplicates being dropped but no longer needed after this point 
# create data.frame (table) only with nice readable names for displaying
  dataFull <- dataFull[, c("Site" ,  "Survey Date"  ,"Mayfly" , "Stonefly","Freshwater shrimp", "Flat bodied (Heptageniidae)", "Cased caddis", "Caseless Caddis" ,"Olives (Baetidae)", "Blue Winged Olives (Ephemerellidae)","Comments", "Combined Riverfly Score" , "Default Trigger Level")]  
# URL of google spreadsheet with site information
  myCsv2 <- getURL("https://docs.google.com/spreadsheets/d/1fDvRtuqolZO7bL-rhEzyCXl7cAOXLWi9a-x56rQTgYQ/export?gid=1&format=csv") # get site details from google doc (list of all sites - even ones without sample results)

# Data for Map section
# download google spreadsheet with site information to be used for map co-ordinates  
sites <- read.csv(textConnection(myCsv2), stringsAsFactors = F) 
# removes "" quotes from hardgate burn site - this causes issues because "" are special characters in R
sites$Full.name <- gsub("\\\"", "", sites$Full.name) 
sites$Full.name  <- gsub(",","",sites$Full.name ) # commas not working for hash/url creation
# create dataframe with site name and lat/lon
dat <- sites[,c('lat', 'long', 'Full.name')]
# rename data with better names needed for map function later
  names(dat) <- c('lat', 'lon', 'Site')
# remove sites which have no data currently upload
 dat <- dat[dat$Site %in% unique(csv1$Site),]
# convert dataframe into a JOSN array for map function later - map needs JSON format
  dat_list <- toJSONArray2(dat, json = F) # converts to JSON file format for map later
  
# for values for table - reactive depending on which site is selected  
    formulaText <- reactive({
      summaryData <- eval(parse(text=paste("dataFull[dataFull$Site == \"", input$dataset, "\", 2:12]",sep="")))
       
      summaryData <-  summaryData[ order( summaryData$'Survey Date', decreasing = TRUE),]
            return(summaryData)
      })
# values for graph plotting - reactive depending on which site is selected           
      tableText <- reactive({ 
      eval(parse(text=paste("csv2[csv2$Site == \"", input$dataset, "\", ]",sep=""))) 
           })
# text for graph heading - reactive depending on which site is selected       
        captionText <- reactive({
          eval(parse(text=paste("dataClean[dataClean$site == \"", input$dataset, "\",2]",sep="")))})
# lat/lon for adjusting where the view of the map - reactive depending on which site is selected   
    mapText <- reactive({
      eval(parse(text=paste("sites[sites$Full.name == \"", input$dataset, "\", 6:7]",sep="")))
    })
# lat/lon for adjusting the link to OpenStreetMap website to enable viewing of more detailed map - reactive depending on which site is selected   
mapText2 <- reactive({
  text <- eval(parse(text=paste("sites[sites$Full.name == \"", input$dataset, "\", 6:7]",sep="")))
text1 <- paste(as.character(text[,1]))
text2 <- paste(as.character(text[,2]))
text <- paste("<a href=\"https://www.openstreetmap.org/#map=15/", text2,"/", text1,"\">Improve the map and view in more detail here</a>",sep="")
return(text)
  })
    
# Return the formula text for printing as a caption
    output$caption <- renderText({
     input$dataset
    })
  
# Generate a summary of the dataset
  output$summary = renderDataTable({
   dataset <- na.omit(formulaText())},
   options = list(aLengthMenu = c(10, 30, 50), iDisplayLength = 10)
  )
# Generate a summary table of the all the data only works in Browser not in Rstudio browser

output$allresults <- downloadHandler(
  filename = function() { paste('Riverfly-Data',Sys.time(),'.csv', sep='') },
  content = function(file) {
     write.csv(dataFull, file, row.names = F)
  }
)

  ## map 
  output$myChart2 <- renderMap({
    map3 <- Leaflet$new()
    map3$geoJson(toGeoJSON(dat_list, lat = 'lat', lon = 'lon'),
                 onEachFeature = '#! function(feature, layer){
                 layer.bindPopup(feature.properties.Site)
  } !#',
                 pointToLayer =  "#! function(feature, latlng){
                 return L.circleMarker(latlng, {
                 radius: 4,
                 fillColor: feature.properties.Color || 'red',    
                 color: '#000',
                 weight: 1,
                 fillOpacity: 0.8,
                 })
} !#"
                
  )  
  data1 <- mapText()
  map3$setView(c(data1[,2], data1[,1]), zoom = 15)
  map3$tileLayer("http://{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png")
  map3$set(height = '250px', width = '250px')
  map3
  })
 
## map text - link to openstreetmap
output$edit <- renderText({
  data1 <- mapText2()

})

# Show "Total" riverfly score on graph   
  output$view <- renderPlot({
 dataset <- tableText() 
 dataset$trigger <- 3  
#dataset <- dataset[dataset$'Survey date' == '28/11/14'|dataset$'Survey date' == '26/11/14',]#
#
#x <- as.Date("2011-08-08") + c(0,7,21)
#y <- c(7,9,11)

#DF1 <- data.frame(x,y)
#dataset <- rbind(dataset, data.frame(x=seq(min(as.Date(dataset$dateClean, "%d/%m/%y")), max(as.Date(dataset$dateClean, "%d/%m/%y")), by="1 day"), y=0))
dataset$order <- as.Date(dataset$dateClean, "%d/%m/%y")
dataset <- dataset[with(dataset, order(order)), ]
 # using qplot to plot graph for site. ggplot function didn't worked because looked for 'dataset' in global environment not locally within function
print( qplot(data=dataset, x=as.factor(as.Date(dataset$dateClean, "%d/%m/%y")), fill=variable, weight=log, colour="value")
       + geom_bar() 
       + labs(fill = "Log Abundance per group")
       + geom_abline(aes(colour="Trigger Level"),intercept=dataset$trigger,slope=0,size=2, ) +
         theme(axis.text.x=element_text(size=8)) +
         scale_x_discrete(labels = unique(format(as.Date(dataset$dateClean,"%d/%m/%y"),format= "%d-%b-%y"))) +
      scale_colour_manual(name = 'Trigger',values=c("Trigger Level"="red","value"="grey")) + ylab("Riverfly Score") + xlab("Date"))

# better date scale/spacing!!!
# '#66c2a5','#fc8d62','#8da0cb','#e78ac3','#a6d854','#ffd92f','#e5c494','#b3b3b3'


})

# summary stats for all sites
output$stats <- renderTable({
# dataset <- csv2[csv2$Site == "Antermony Loch inflow u/s Antermony Loch",] # used for testing 
dataset <- csv2

sample <- ddply(dataset, ~ dateClean + Site, function(dataset) {
 with(dataset, data.frame( value=mean(value), log=sum(log))) 
})

sample <- sample[with(sample, order(as.Date(dateClean,  "%d/%m/%y"),decreasing = TRUE )), ] 

allsites <-  with(sample, data.frame( 'Number of sites'=length(unique(Site)), 'Total Number of Samples'=length(log), 'Average Riverfly Score'=mean(log),
                                    'Max Riverfly Score'=max(log), 'Min Riverfly Score'=min(log), 'Date of Last Sample'=dateClean[1], 
                                     check.names = FALSE))

head(allsites)

})

output$histogram <- renderPlot({
   print( hist(as.Date(csv1$'Survey date',"%d/%m/%Y"), "months",axes=F,col="light blue", freq = TRUE,format = "%b %Y", xlab="Date",ylab="Samples per Month",main="Samples collected per Month"))
          axis.Date(as.Date(csv1$'Survey date',"%d/%m/%Y"),format = "%b %Y",at=sort(as.Date(csv1$'Survey date')),side=1,tcl = F)
          axis(2)
 })



output$cumsum <- renderPlot({
  csv1$num <- 1
  print(plot(y=cumsum(csv1$num),x=sort(as.Date(csv1$'Survey date',"%d/%m/%Y")),xaxt = "n",  lwd=10,col="light blue", type = "l",xlab="Date",ylab="Total Samples",main="Accumlative Samples Collected over Time"))
  axis.Date(side = 1, sort(as.Date(csv1$'Survey date',"%d/%m/%Y")), format = "%b %Y", at=sort(as.Date(csv1$'Survey date',"%d/%m/%Y")),tcl = F)
  })

output$dupes <- renderTable({
  # dataset <- csv2[csv2$Site == "Antermony Loch inflow, u/s Antermony Loch",] # used for testing
  dataset <- csv2
  
  sample <- ddply(dataset, ~ dateClean + Site + Timestamp, function(dataset) {
    with(dataset, data.frame( value=mean(value), log=sum(log))) 
  })
  
  sample <- sample[with(sample, order(as.Date(dateClean,  "%d/%m/%y"),decreasing = TRUE )), ] 
  
  allsites <-sample[duplicated(sample[,1:2]),] 

    test <- length(unique(allsites$log))
  if  (test < 1 )   
  
    allsites <- data.frame("Glad to report no issues with duplicates")
   colnames(allsites) <- c("")

  head(allsites)
  
})

# summary stats for single site
output$siteStats <- renderTable({
 dataset <- tableText()
   
  sample <- ddply(dataset, ~ dateClean + Site, function(dataset) {
    with(dataset, data.frame( value=mean(value), log=sum(log))) 
  } )
  
  sample <- sample[with(sample, order(as.Date(dateClean,  "%d/%m/%y"),decreasing = TRUE )), ] 
   
 dataset <- csv2
  rank <- ddply(dataset, ~ Site, function(dataset) {
   with(dataset, data.frame( log=sum(log)/length(unique(dataset$dateClean))))
 })
 
 noSites <-  length(unique(rank$Site))
 rank <- rank[with(rank, order(log,decreasing = T )), ] 
 rank$rank <- rank(rank$log, ties.method = "first")
 rank$rank <- rev(rank$rank)
  allsites <-  with(sample, data.frame('Number of Samples'=length(log), 'Site Average Score'=mean(log), 
                                        'Max Riverfly Score'=max(log), 'Min Riverfly Score'=min(log), 'Date of Last Sample'=dateClean[1], 
                                        'Rank of Site by Average Riverfly Score'=paste(rank$rank[rank$Site == sample$Site[1:1]], "th out of ",noSites, " sites",sep=""),check.names = FALSE))
    head(allsites)

})


#### function for creating hash/url: 
url_fields_to_sync <- ("dataset");

firstTime <- TRUE
output$hash <- reactiveText(function() {
  newHash = paste(collapse=",",
                  Map(function(field) {
                    paste(sep="=",
                          field,
                          input[[field]])
                  },
                  url_fields_to_sync))
  # the VERY FIRST time we pass the input hash up.
  return(
    if (!firstTime) {
      newHash
    } else {
      if (is.null(input$hash)) {
        NULL
      } else {
        firstTime<<-F;
        isolate(input$hash)
      }
    }
  )
})


})
