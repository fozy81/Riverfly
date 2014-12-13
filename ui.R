library(shiny)
library(ggplot2)
library(RCurl)
library(plyr)
#library(data.table)
library(reshape)
require(rCharts)
library(scales)
library(rjson)
#require(rCharts)
#library(leaflet)

hashProxy <- function(inputoutputID) {
  div(id=inputoutputID,class=inputoutputID,tag("div",""));
}

myCsv <- getURL("https://docs.google.com/spreadsheet/pub?key=0ArVD_Gwut6UBdHZkQ2g0U0NXQ0psZUltQkpKZjVEM3c&single=true&gid=0&output=csv")
d <- read.csv(textConnection(myCsv),check.names=FALSE)
d$Site <- gsub("\\\"", "", d$Site) # "Hardgate Burn" causes problems because of "" quotes...these have been removed from 'sites' worksheet but not from 'Form' worksheet in google docs - need to fix this properly at some point
d$Site <- gsub(",","", d$Site)  # commas not working for hash/url creation

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Clyde Catchment Riverfly Data"),
  
  # Sidebar with controls to select a dataset, display map of selected site and link to OpenStreetMap 
  sidebarPanel(
        selectInput("dataset", "Choose a site:", 
                            sort(paste(unique(sort(d$Site))))),
                ### add map here
        tags$style('.leaflet {height: 250px;}'),
        tags$style('.leaflet {layerOpts: {
      attribution: "Map data from<a href=\"http://openstreetmap.org\">OpenStreetMap</a>\n         contributors, Imagery<a href=\"http://mapbox.com\">MapBox</a>" 
     ;},'),
        showOutput('myChart2', 'leaflet') ,
       hr(),
       htmlOutput(paste('edit')),
       hr(),
       helpText(a("Report issues or view the code for this site on Github", href="https://github.com/fozy81/Riverfly/issues", target="_blank")),
      hr(),
      helpText("All data is creative commons zero - Clyde Catchment Riverfly contributors")
 ),
  
  # Show a summary of the dataset and plot
  mainPanel(    includeHTML("URL.js"),  hashProxy("hash"),
   tabsetPanel(
     tabPanel("Site Results", h3(textOutput("caption")),  showOutput("view", "dimple"), h4("Site Summary"),tableOutput("siteStats"),  h4("Site Data"),dataTableOutput("summary")),
     tabPanel("Summary Results", h4("Summary"), tableOutput("stats"),plotOutput("histogram"),plotOutput("cumsum"),h4("Duplicates in data?"),tableOutput("dupes"),h4("Download All Riverfly Data"),downloadButton('allresults','Download'),hr())
  )

  )
  ))

