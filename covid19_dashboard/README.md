In this project we aim at implementing a dashboard serving as a follow-up of the Covid-19 breakdown. The dashboard is built upon
up-to-date data drawn from <datahub.io>. These data are available using the following link https://datahub.io/core/covid-19/datapackage.json.

### Objectives in this work :

- Download the data from <dataio.hub>. The data are constituted of several data sets among which:
  "time-series-19-covid-combined_csv": Detailed time series per country 
  "countries-aggregated_csv": Aggregation (over the country) 
  "worldwide-aggregated_csv": Aggregation (over the time) 
  "reference_csv": including information about the countries

- Extract the needed information and format them into dataframes and SpatialPolygonsDataFrames
  that will serve after to create the fields of the dashboards.

- We are mainly interested in three datasets included in the whole data from <datahub.io>
  These datasets are "time-series-19-covid-combined_csv", "countries-aggregated_csv", and
  "worldwide-aggregated_csv".

- Using the package shiny, leaflet and DT to display the interactive illustrations in the dashboard.
   Specifically leaflet was used for map and DT for printing out table. The package shiny was used to 
   create interactive and dynamic web-based materials.


###  Useful R packages to install and load before being able to proceed:

- "jsonlite"      --> to download the data from <datahub.io>
- "dplyr"         --> used for data manipulation
- "tidyverse"     --> used for data manipulation
- "htmlwidgets  --> a framework for embedding JavaScript visualizations into R
- "rbokeh"      --> for interactive plotting
- "leaflet"     --> useful for mapping in R 
- "dygraphs"    --> Time series visualization
- "DT"          --> enables creating interactive tabular representations
- "xts"         --> required to convert the time series data into the right format

### Execution procedure

- Upload the files in your disk
- Open the covid19_dashboard project by double clicking on the "covid19_dahsboard.Rproj" file
- Execute the R script entitled "data_preprocessing.R" which aims to prepare the necessary data and files to create the dashboard.
- Open the "covid19_dashboard.rmd" file and hit the run button to produce the dashboard.

### The resulting dashboard is constituted of three main pages :

- The first page gives a visual insight of the global situation of the covid19 outbreak with the total number of confirmed cases
, the total number of recovered and the death rate at the last date for which data are available. Furthermore, this page includes
a world map of the cases, a representation of the time series of confirmed, recovered and deaths cases, and table summarizing the
cases in each Continent/Region.

- The second page illustrates the situation, at the last date for which data are available, in Germany by offering a visual
representation of the spatial distribution among German "Länder" in a chloropleth map and by displying the data in a table.

- The third page provides us with a dynamic follow-up of the global situation of the breakdown with an interactive and dynamic
chloropleth map evolving along the time and the selection menu to have a closer look, in the form of a table, at the situation
in a specific country.
