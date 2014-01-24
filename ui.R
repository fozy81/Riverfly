library(shiny)
library(ggplot2)


load("dataClean.RData")

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
 
  # Application title
    headerPanel("Clyde Catchment Riverfly Data"),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  sidebarPanel(
        selectInput("dataset", "Choose a site:", 
                            sort(paste(unique(sort(dataClean$site))))) 
             
  ),
  
  # Show a summary of the dataset and plot
  mainPanel(
    h3(textOutput("caption")), 
  plotOutput("view"),
  dataTableOutput("summary")
  )
))
