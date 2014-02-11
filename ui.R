library(shiny)
library(ggplot2)
library(RCurl)
library(plyr)
#library(data.table)
library(reshape)
#library(leaflet)


# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
 
  # Application title
    headerPanel("Clyde Catchment Riverfly Data"),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  sidebarPanel(
        selectInput("dataset", "Choose a site:", 
                            sort(paste(unique(sort(dataClean$site))))), 
        helpText(a("Report issues or view the code for this site on Github", href="https://github.com/fozy81/Riverfly/issues", target="_blank"))
        
    
 ),
  
  # Show a summary of the dataset and plot
  mainPanel(
    h3(textOutput("caption")), 
  plotOutput("view"),
  dataTableOutput("summary")
  )
))


