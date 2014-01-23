library(shiny)
library(ggplot2)

load("dataClean.RData")

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  # Return the requested dataset
    
    formulaText <- reactive({
      eval(parse(text=paste("dataClean[dataClean$site == \"", input$dataset, "\",1:4]",sep="")))})
      
      
      tableText <- reactive({
        eval(parse(text=paste("dataClean[dataClean$site == \"", input$dataset, "\" & dataClean$name == \"Total\", 1:6]",sep="")))    
      })
        
        captionText <- reactive({
          eval(parse(text=paste("dataClean[dataClean$site == \"", input$dataset, "\",2]",sep="")))})
    
    
           
    # Return the formula text for printing as a caption
    output$caption <- renderText({
     input$dataset
    })
    
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    dataset <- formulaText()
    head(dataset, n=100)
  })
    
    
  # Show the first "n" observations
  output$view <- renderPlot({
    dataset <- tableText()

    if (length(dataset$value) >= 2){
     print(ggplot(data=dataset, aes(x=date, y=value, fill=value,colour="Score")) + 
             geom_bar(stat="identity") + 
             geom_text(aes(x=date, y=value, label=date, 
             vjust=ifelse(sign(value)>0, 2, 0)),
                       position = position_dodge(width=1)) + 
             geom_abline(data=dataClean, aes(colour="Trigger Level",intercept=trigger,slope=0,size=2)) +
            scale_colour_manual(name = 'Trigger',values=c("Trigger Level"="red","value"="grey")) +
             ylab("Riverfly Score") + xlab("Date"))
    }
    else {
      if(length(dataset$value) < 2){
      dataset <- rbind(dataset,dataset[1,])
      dataset$date[2] <- as.Date('2013-12-30') 
      dataset$value[2] <- 0
          print(ggplot(data=dataset, aes(x=date, y=value, fill=value,colour="value")) + 
                  geom_bar(stat="identity") + 
                  geom_text(aes(x=date, y=value, label=date, 
                  vjust=ifelse(sign(value)>0, 2, 0)),
      position = position_dodge(width=1)) + 
                      geom_abline(aes(colour="Trigger Level",intercept=trigger,slope=0,size=2)) + 
                scale_colour_manual(name = 'Trigger',values=c("Trigger Level"="red","value"="grey")) +
     ylab("Riverfly Score") + xlab("Date"))
    }
}
   
  })
})
