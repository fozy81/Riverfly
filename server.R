library(shiny)
library(ggplot2)
library(RCurl)
library(reshape)
library(plyr)
#library(data.table)

myCsv <- getURL("https://docs.google.com/spreadsheet/pub?key=0ArVD_Gwut6UBdHZkQ2g0U0NXQ0psZUltQkpKZjVEM3c&output=csv")
o2 <- read.csv(textConnection(myCsv))

o2$dateClean  <- strptime(o2$Survey.date, "%d/%m/%Y")
o2$id <- sequence(nrow(o2))

o3 <- melt(o2, id.vars=c("id","dateClean","Site.", "Survey.date", "CC0","Comments","Timestamp"))
o3$value <- as.numeric(o3$value)
o3$dateClean2 <- as.character(o3$dateClean)
o3$site <- as.character(o3$Site.)
o3$Site. <- NULL
o3$dateClean <- NULL

o3$log[o3$value < 10] <- 1
o3$log[o3$value >= 10 & o3$value < 100] <- 2
o3$log[o3$value >= 100 & o3$value < 100000] <- 3

dataClean <- ddply(o3, ~ dateClean2 + site,
                   summarize, Total=sum(log)
)

#dt.o3 <- data.table(o3, key=c("dateClean2","site"))

#total <- dt.o3[,list(Total=sum(log)             
#      ), by=key(dt.o3)]

#dataClean <- data.frame(total)
dataClean$date  <- as.Date(dataClean$dateClean2, "%Y-%m-%d")
dataClean$trigger <- 2 

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  # Return the requested dataset
    
    formulaText <- reactive({
      summaryData <- eval(parse(text=paste("o3[o3$site == \"", input$dataset, "\"& o3$value != 0, 6:9]",sep="")))
      summaryData$date <- as.character(summaryData$dateClean2)
      summaryData$dateClean2 <- NULL
      return(summaryData)
      })
            
      tableText <- reactive({
        eval(parse(text=paste("dataClean[dataClean$site == \"", input$dataset, "\", 2:5]",sep="")))    
      })
        
        captionText <- reactive({
          eval(parse(text=paste("dataClean[dataClean$site == \"", input$dataset, "\",2]",sep="")))})
        
           
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
             geom_text(aes(x=date, y=Total, label=date, 
             vjust=ifelse(sign(Total)>0, 2, 0)),
                       position = position_dodge(width=1)) + 
             geom_abline(data=dataClean, aes(colour="Trigger Level",intercept=trigger,slope=0,size=2)) +
            scale_colour_manual(name = 'Trigger',values=c("Trigger Level"="red","value"="grey")) +
             ylab("Riverfly Score") + xlab("Date"))
    }
    else {
      if(length(dataset$Total) < 2){
      dataset <- rbind(dataset,dataset[1,])
      dataset$date[2] <- strptime('2013-12-30', "%Y-%m-%d")
      dataset$Total[2] <- 0
          print(ggplot(data=dataset, aes(x=date, y=Total, fill=Total,colour="value")) + 
                  geom_bar(stat="identity") + 
                  geom_text(aes(x=date, y=Total, label=date, 
                  vjust=ifelse(sign(Total)>0, 2, 0)),
      position = position_dodge(width=1)) + 
                      geom_abline(aes(colour="Trigger Level",intercept=trigger,slope=0,size=2)) + 
                scale_colour_manual(name = 'Trigger',values=c("Trigger Level"="red","value"="grey")) +
     ylab("Riverfly Score") + xlab("Date"))
    }
}
   
  })
})
