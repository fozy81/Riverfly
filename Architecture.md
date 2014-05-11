## Web application uses this architecture:

 ![image link](https://raw.githubusercontent.com/fozy81/Riverfly/riverflyTest/Images/CRIMP%20architecture.jpg)

### 1. Sampling site detail information is entered on Google spreadsheet by Admin 

* A suitable sample site location is arranged by discussion between the volunteer and CRIMP co-ordinator.
* The CRIMP co-ordinator updates the 'sites' worksheet in the spreadsheet with name and location of site (including Lat/Lon etc). If conversion between grid reference, eastings/northings and Lat/Lon(WGS84) is required use this [site](http://www.nearby.org.uk/coord.cgi#WGS84%20Latitude/Longitude) or a similar British Geological Survey site [here](http://www.bgs.ac.uk/data/webservices/convertForm.cfm). The spreadsheet has seven columns: River, Site, Full Name, Eastings, Northings, Lat , Lon. This all need to be filled in. 

* The rows then need to be sort A-Z by Full Name column.

* Lastly, the new site names needs to be pushed to the google form so volunteers can select them when uploading their sample form. This is done by selecting the 'formRanger' tool on the menu bar and clicking 'save settings' :

 ![image link](https://raw.githubusercontent.com/fozy81/Riverfly/riverflyTest/Images/formRanger%20-%20save%20settings.png)

### 2. Data is uploaded to the site via a Google form 

* Volunteers enter sampling data on google form either in the field via smart phone or later at home. A paper sheet form is provide if smart phone can't be used in field. The sampling site details pre-entered by the co-ordinator populates the Google form drop down menu of locations. i.e. only pre-arranged sites can be sampled. The Google form uses a plugin called 'form ranger' to achieve a link from the site details spreadsheet to google form.

* The google form is kept private and only given to volunteers once they have completed their training. The google form has some data validation features enabled. These limits very large numbers/abundance records being entered. Date is recorded but not time of sample. This limits to only one sample per day per site - this could be a short-coming if more than one sample was taken from a site in a day but this unlikely. 

* All data is submitted under a Creative Commons Zero license to enable maximum re-use and distribution

### 3. Data is stored on a Google spreadsheet 

* A version of the spreadsheet is published found [here](https://docs.google.com/spreadsheet/pub?key=0ArVD_Gwut6UBdHZkQ2g0U0NXQ0psZUltQkpKZjVEM3c&single=true&gid=0&output=csv) in CSV format. The 'live' spreadsheet is only accessible by the co-ordinator/team. 

* The data is automatically sent from the google form and populates the spreadsheet. No details of which sampler or who uploaded the form is recorded. This is to avoid any data protection/privacy issues. The co-ordinator will have an idea who is likely to uploaded the data if records need to be checked.   

* The site details information is held on the same spreadsheet but in a different worksheet.

### 4. Code written in R and stored on github is uploaded to Shiny server

R is a computer language particular focused on statistics . It is good at displaying data and using the 'shiny' package can create reactive web pages for analysing data dynamically. The files which create the app and handle user interactions are ui.R and server.R scripts. More about shiny package can be [here](http://shiny.rstudio.com/). Currently, there are two branchs of the code for the site:

* Master - this is the current working code
* riverflyTest  - this is the development branch for testing

You can access all the code [here](https://github.com/fozy81/Riverfly/tree/master) - it is published under a free and open license to encourage re-use (MIT license).

### 5. The 'Shiny' server hosted by Rstudio (free beta test) downloads the google spreadsheet and uses the R code to create an interactive website

The Shiny server is hosted by Rstudio and is run as a free beta test/demo - this may incurr cost in future. The server interacts with code written R using the 'shiny' package to create interactive web pages. The Shiny server can also be setup to run in the cloud e.g. amazon web services.  The basic Shiny server software is free but the cloud hosting would be an added expense.

## Previous data handling architecture

Before this web application and google form/spreadsheet process was created, volunteers completed an Excel spreadsheet and emailed the spreadsheet to the co-ordinator. The number of spreadsheets quickly increased and having a overview of the data became difficult. Also the volunteers could only see results from their own sites and not data from the project as a whole. The Riverfly.R script was written to read the Excel spreadsheets into a single table so that all the data was in one place before starting on creating a web application.

## Future architecture

Using google forms and spreadsheets has been an easy way to collect data and is free to use, but creating a customised solution could be useful in longer term. For example Google may change terms of use, increase advertising or even stop providing the service etc. A custom solution would also offer the possibility of a native mobile 'app' rather than a webpage. 

Storing the data in a google spreadsheet has some advantages but a database backend would be more scalable and faster. If the amount of data increases greatly, then storage on google spreadsheet is not feasible. Currently, the shiny server downloads the spreadsheet every time the webpage is refresh. This means the data is always concurrent but if the amount of data was hundreds of thousands of rows this would start to have performance issues. As it is, the is small performance cost to downloading data from google spreadsheet.

The shiny server is hosted by Rstudio and is currently free. Hosting the shiny server software independently would probably improve performance but would incurr a cost (hiring an independent server).







