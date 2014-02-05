library(shiny)
library(ggplot2)
library(RCurl)
library(plyr)
#library(data.table)
library(reshape)
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
