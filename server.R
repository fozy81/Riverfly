library(shiny)
library(datasets)
library(ggplot2)

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  # Return the requested dataset
    
    formulaText <- reactive({
      eval(parse(text=paste("dataClean[dataClean$site == \"", input$dataset, "\",1:4]",sep="")))})
      
      
      tableText <- reactive({
        eval(parse(text=paste("dataClean[dataClean$site == \"", input$dataset, "\" & dataClean$name == \"Total\", 1:6]",sep="")))
      
     # tableText <- reactive({
        # eval(parse(text=paste("dataClean[dataClean$site == \"", input$dataset, "\",]",sep="")))
      
      })
    
        #switch(input$dataset,
      #     "The Ford" = dataClean[dataClean$site == "The Ford",],
       #    "pressure" = pressure,
       #    "cars" = cars)
  #})
    
    # Return the formula text for printing as a caption
    output$caption <- renderText({
      formulaText()
    })
    
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    dataset <- formulaText()
    head(dataset, n=100)
  })
    
    
  # Show the first "n" observations
  output$view <- renderPlot({
    dataset2 <- tableText()

    if (length(dataset2$value) >= 2){
     print(ggplot(data=dataset2, aes(x=date, y=value, fill=value)) + 
             geom_bar(stat="identity") + 
             geom_text(aes(x=date, y=value, label=date, 
             vjust=ifelse(sign(value)>0, 2, 0)),
                       position = position_dodge(width=1)) + 
             geom_line(aes(x=date,y=trigger,colour="red")) + 
             ggtitle("Riverfly Score over Time"))
    }
    else {
      if(length(dataset2$value) < 2){
      dataset2 <- rbind(dataset2,dataset2[1,])
      dataset2$date[2] <- as.Date('2013-12-30') 
      dataset2$value[2] <- 0
          print(ggplot(data=dataset2, aes(x=date, y=value, fill=value)) + 
                  geom_bar(stat="identity") + 
                  geom_text(aes(x=date, y=value, label=date, 
                  vjust=ifelse(sign(value)>0, 2, 0)),
      position = position_dodge(width=1)) + 
                      geom_line(aes(x=date,y=trigger,colour="red")) + 
                  ggtitle("Riverfly Score over Time"))
    }
}
   
  })
})
