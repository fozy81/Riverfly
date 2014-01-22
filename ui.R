library(shiny)
library(ggplot2)

load("dataClean.RData")

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Clyde River Foundation Riverfly Data"),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  sidebarPanel(
    selectInput("dataset", "Choose a dataset:", 
             #   choices2 = c("rock", "pressure", "cars")),
                choices = c(paste(unique(dataClean$site)))),
    numericInput("obs", "Number of observations to view:", 10)
  ),
  
  # Show a summary of the dataset and an HTML table with the requested
  # number of observations
  mainPanel(
  plotOutput("view"),
  verbatimTextOutput("summary")
  )
))
