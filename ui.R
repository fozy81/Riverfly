library(shiny)
library(ggplot2)
library(RCurl)
library(plyr)
#library(data.table)
library(reshape)

#library(leaflet)

myCsv <- getURL("https://docs.google.com/spreadsheet/pub?key=0ArVD_Gwut6UBdHZkQ2g0U0NXQ0psZUltQkpKZjVEM3c&single=true&gid=0&output=csv")
d <- read.csv(textConnection(myCsv),check.names=FALSE)
d$Site <- gsub("\\\"", "", d$Site) # "Hardgate Burn" causes problems because of "" quotes...these have been removed from 'sites' worksheet but not from 'Form' worksheet in google docs - need to fix this properly at some point


# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
 
  # Application title
    headerPanel("Clyde Catchment Riverfly Data"),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  sidebarPanel(
        selectInput("dataset", "Choose a site:", 
                            sort(paste(unique(sort(d$Site))))), 
                ### add map here
        tags$style('.leaflet {height: 250px;}'),
        tags$style('.leaflet {layerOpts: {
       attribution: "Map data from<a href=\"http://openstreetmap.org\">OpenStreetMap</a>\n         contributors, Imagery<a href=\"http://mapbox.com\">MapBox</a>" 
       ;},'),
        showOutput('myChart2', 'leaflet') ,
        helpText(a("Report issues or view the code for this site on Github", href="https://github.com/fozy81/Riverfly/issues", target="_blank"))
      , htmlOutput(paste('edit'))
 ),
  
  # Show a summary of the dataset and plot
  mainPanel(
    h3(textOutput("caption")), 
  plotOutput("view"),
  dataTableOutput("summary")
 
  )
))


