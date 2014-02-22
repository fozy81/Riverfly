library(shiny)
library(ggplot2)
library(RCurl)
library(plyr)
#library(data.table)
library(reshape)

#library(leaflet)

myCsv <- getURL("https://docs.google.com/spreadsheet/pub?key=0ArVD_Gwut6UBdHZkQ2g0U0NXQ0psZUltQkpKZjVEM3c&single=true&gid=0&output=csv")
o2 <- read.csv(textConnection(myCsv),check.names=FALSE)
o2$Site <- gsub("\\\"", "", o2$Site)



# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
 
  # Application title
    headerPanel("Clyde Catchment Riverfly Data"),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  sidebarPanel(
        selectInput("dataset", "Choose a site:", 
                            sort(paste(unique(sort(o2$Site))))), 
                ### add map here
        tags$style('.leaflet {height: 250px;}'),
        tags$style('.myChart2 {width: 250px}'),
        showOutput('myChart2', 'leaflet') ,
        helpText(a("Report issues or view the code for this site on Github", href="https://github.com/fozy81/Riverfly/issues", target="_blank"))
 ),
  
  # Show a summary of the dataset and plot
  mainPanel(
    h3(textOutput("caption")), 
  plotOutput("view"),
  dataTableOutput("summary")
 
  )
))


