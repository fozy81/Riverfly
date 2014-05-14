library(shiny)
library(ggplot2)
library(RCurl)
library(reshape)
library(plyr)
require(rCharts)
library(scales)



# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {

# URL for google spreadsheet containing raw riverfly data
myCsv <- getURL("https://docs.google.com/spreadsheet/pub?key=0ArVD_Gwut6UBdHZkQ2g0U0NXQ0psZUltQkpKZjVEM3c&single=true&gid=0&output=csv")
  
# Download data from google spreadsheet (which has been submitted via google form):
  csv1 <- read.csv(textConnection(myCsv),check.names=FALSE)

# remove "\" character from Site name as this is a special escape character and causes issues in R code
csv1$Site <- gsub("\\\"", "", csv1$Site)
    
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
  # create a summary riverfly score for each sample (site + date) - this could cause a problem if more than one sample taken on same day at same site:
  dataClean <- ddply(csv2, ~ dateClean + Site,
                     summarize, Total=sum(log)
  )

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
# order dataClean and d so cbind/merge works correctly
  dataClean <- dataClean[with(dataClean, order(Site, date)), ] 
  csv3 <- csv1[with(csv1, order(Site)), ] 
csv3 <- csv3[!duplicated(csv3[,2:3]),]
duplicated(csv3$date)
# Data for 'All sites' tab
# combine d and dataClean for full data with trigger & riverfly score values for new tab containing all data in one
  dataFull <- cbind(dataClean,csv3)  
# create data.frame (table) only with nice readable names for displaying
  dataFull <- dataFull[, c("Site" ,  "Survey Date"  ,"Mayfly" , "Stonefly","Freshwater shrimp", "Flat bodied (Heptageniidae)", "Cased caddis", "Caseless Caddis" ,"Olives (Baetidae)", "Blue Winged Olives (Ephemerellidae)","Comments", "Combined Riverfly Score" , "Default Trigger Level")]  
# URL of google spreadsheet with site information
  myCsv2 <- getURL("https://docs.google.com/spreadsheet/pub?key=0ArVD_Gwut6UBdHZkQ2g0U0NXQ0psZUltQkpKZjVEM3c&single=true&gid=1&output=csv") # get site details from google doc (list of all sites - even ones without sample results)

# Data for Map section
# download google spreadsheet with site information to be used for map co-ordinates  
sites <- read.csv(textConnection(myCsv2), stringsAsFactors = F) 
# removes "" quotes from hardgate burn site - this causes issues because "" are special characters in R
sites$Full.name <- gsub("\\\"", "", sites$Full.name) 
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
# Generate a summary table of the all the data
output$allresults = renderDataTable({
  dataset <- na.omit(dataFull)},
  options = list(aLengthMenu = c(150, 300, 1000), iDisplayLength = 150)
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

 # using qplot to plot graph for site. ggplot function didn't worked because looked for 'dataset' in global environment not locally within function
print( qplot(data=dataset, x=as.Date(dataset$dateClean, "%d/%m/%y"), fill=variable, weight=log, colour="value")
       + geom_bar() + labs(fill = "Log Abundance per group")
       + geom_abline(aes(colour="Trigger Level"),intercept=dataset$trigger,slope=0,size=2, ) +
         scale_x_date(breaks = date_breaks("months"),
                      labels = date_format("%b-%y")) +
      scale_colour_manual(name = 'Trigger',values=c("Trigger Level"="red","value"="grey")) + ylab("Riverfly Score") + xlab("Date"))
# better date scale/spacing!!!
})

# summary stats for sites
output$stats <- renderTable({
# dataset <- csv2[csv2$Site == "Antermony Loch inflow, u/s Antermony Loch",] # used for testing
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



})
