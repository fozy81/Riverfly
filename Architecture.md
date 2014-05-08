### Web application uses this architecture:

 ![image link](https://raw.githubusercontent.com/fozy81/Riverfly/riverflyTest/CRIMP%20architecture.jpg)

### 1. Sampling site detail information is entered on Google spreadsheet by Admin 

The sample site details are arranged by discussion between the volunteer and CRIMP organiser as to the best location. The CRIMP organiser updates the spreadsheet with name and location of site (Lat/Lon).

### 2. Data is uploaded to the site via a Google form 

Volunteers enter sampling data on google form (either in the field via smart phone or later at home). The sampling site details spreadsheet populates the Google form drop down menu of locations. i.e. only pre-arranged sites can be sampled. The google form is kept private and only given to volunteers once they have completed their training. The google form has some data validation features enabled. These limit very large numbers/abundance records being entered. Date is recorded but not time of sample. This limits to only one sample per day per site - this could be a short coming if more than one sample was taken from a site in a day but this unlikely. 

### 3. Data is stored on a Google spreadsheet 

Which can be found [here](https://docs.google.com/spreadsheet/pub?key=0ArVD_Gwut6UBdHZkQ2g0U0NXQ0psZUltQkpKZjVEM3c&single=true&gid=0&output=csv). The data is automatically sent from the google form and populates the spreadsheet. The site information is held on the same spreadsheet but in a different worksheet.

### 4. Code written in R and stored on github is uploaded to Shiny server

R is a statistic computer language. It is good at displaying data and using the 'shiny' package can create reactive web pages for analysing data dynamically. The files which create the app and handle user interactions are ui.R and server.R scripts. More about shiny package can be [here](http://shiny.rstudio.com/)

### 5. The 'Shiny' server hosted by Rstudio (free beta test) downloads the google spreadsheet and ues the R code to create an interactive website

The Shiny server is hosted by Rstudio and is run as a free beta test/demo - this may incurr cost in future. The shiny server can also be setup to run in the cloud e.g. amazon web services.  The basic shiny server software is free but the cloud hosting would be an added expense.




