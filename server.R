library(shiny)
library(ggplot2)
library(RCurl)
library(reshape)
library(plyr)
#library(leaflet)
#require(rCharts)
#library(data.table)


# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  myCsv <- getURL("https://docs.google.com/spreadsheet/pub?key=0ArVD_Gwut6UBdHZkQ2g0U0NXQ0psZUltQkpKZjVEM3c&single=true&gid=0&output=csv")
  o2 <- read.csv(textConnection(myCsv))
  
  o2$Survey.date <- strptime(o2$Survey.date, "%m/%d/%Y")
  o2$Survey.date <- format(o2$Survey.date, "%d/%m/%y")
  o2$dateClean  <- o2$Survey.date
  
  o2$id <- sequence(nrow(o2))
  
  o3 <- melt(o2, id.vars=c("id","dateClean","Site", "Survey.date", "CC0","Comments","Timestamp"))
  o3$value <- as.numeric(o3$value)
  o3$dateClean2 <- as.character(o3$dateClean)
  o3$site <- as.character(o3$Site)
  o3$Site. <- NULL
  o3$dateClean <- NULL
  
  o3$log[o3$value < 1] <- 0
  o3$log[o3$value >= 1 & o3$value < 10] <- 1
  o3$log[o3$value >= 10 & o3$value < 100] <- 2
  o3$log[o3$value >= 100 & o3$value < 100000] <- 3
  
  dataClean <- ddply(o3, ~ dateClean2 + site,
                     summarize, Total=sum(log)
  )
  
  #dt.o3 <- data.table(o3, key=c("dateClean2","site"))
  
  #total <- dt.o3[,list(Total=sum(log)             
  #      ), by=key(dt.o3)]
  
  #dataClean <- data.frame(total)
  dataClean$date  <-  as.Date(dataClean$dateClean2, "%d/%m/%y")
  #dataClean$date <- format(dataClean$date,  "%d/%m/%y")


  dataClean$'Survey Date' <- dataClean$date
  o3$'Survey Date' <- o3$Survey.date
  dataClean$trigger <- 2 
  
  myCsv2 <- getURL("https://docs.google.com/spreadsheet/pub?key=0ArVD_Gwut6UBdHZkQ2g0U0NXQ0psZUltQkpKZjVEM3c&single=true&gid=1&output=csv")
  sites <- read.csv(textConnection(myCsv2))  ## to be used for map co-ordinates at some point  
  
  # Return the requested dataset
    
    formulaText <- reactive({
      summaryData <- eval(parse(text=paste("o3[o3$site == \"", input$dataset, "\"& o3$value != 0, 7:9]",sep="")))
      summaryData$date <- as.character(summaryData$dateClean2)
    #  summaryData$dateClean2 <- NULL
      return(summaryData)
      })
            
      tableText <- reactive({
        eval(parse(text=paste("dataClean[dataClean$site == \"", input$dataset, "\", 1:6]",sep="")))    
      })
        
        captionText <- reactive({
          eval(parse(text=paste("dataClean[dataClean$site == \"", input$dataset, "\",2]",sep="")))})
     
    mapText <- reactive({
      eval(parse(text=paste("sites[sites$Full.name == \"", input$dataset, "\", 4:5]",sep="")))
    })
    # Return the formula text for printing as a caption
    output$caption <- renderText({
     input$dataset
    })
    
 
    
  # Generate a summary of the dataset
  output$summary = renderDataTable({
   dataset <- na.omit(formulaText())},
   options = list(aLengthMenu = c(10, 30, 50), iDisplayLength = 10)
  #  head(dataset, n=100)
  )

    
    
      # Show "Total" riverfly score on graph
  output$view <- renderPlot({
    dataset <- tableText()

    if (length(dataset$Total) >= 2){
     print(ggplot(data=dataset, aes(x=date, y=Total, fill=Total, colour="value")) + 
             geom_bar(stat="identity") + 
             geom_text(aes(x=date, y=Total, label=dateClean2, 
             vjust=ifelse(sign(Total)>0, 2, 0)),
                       position = position_dodge(width=1)) + 
             geom_abline(data=dataClean, aes(colour="Trigger Level",intercept=trigger,slope=0,size=2)) +
            scale_colour_manual(name = 'Trigger',values=c("Trigger Level"="red","value"="grey")) +
             ylab("Riverfly Score") + xlab("Date"))
    }
    else {    # if only 1 value ggplot won't render - so add a fake 0 value to get it to work 
      if(length(dataset$Total) < 2){
      dataset <- rbind(dataset,dataset[1,])
      dataset$date[2] <- as.Date('30/12/13', "%d/%m/%y")
      dataset$Total[2] <- 0
          print(ggplot(data=dataset, aes(x=date, y=Total, fill=Total,colour="value")) + 
                  geom_bar(stat="identity") + 
              #    geom_text(aes(x=date, y=Total, label=date, 
               #   vjust=ifelse(sign(Total)>0, 2, 0)),
  #    position = position_dodge(width=1)) + 
                      geom_abline(aes(colour="Trigger Level",intercept=trigger,slope=0,size=2)) + 
                scale_colour_manual(name = 'Trigger',values=c("Trigger Level"="red","value"="grey")) +
     ylab("Riverfly Score") + xlab("Date"))
    }
}
   
  })
})
