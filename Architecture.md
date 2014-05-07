Web application uses this architecture:

 ![image link](https://raw.githubusercontent.com/fozy81/Riverfly/riverflyTest/CRIMP%20architecture.jpg)

* Site detail information is entered on
*Data is uploaded to the site via a Google form and then stored in a Google spreadsheet which can be found here https://docs.google.com/spreadsheet/pub?key=0ArVD_Gwut6UBdHZkQ2g0U0NXQ0psZUltQkpKZjVEM3c&single=true&gid=0&output=csv


The web app downloads the google spreadsheet and creates an interactive website. The files which create the app and handle user interactions are ui.R and server.R scripts using shiny server package from RStudio. 