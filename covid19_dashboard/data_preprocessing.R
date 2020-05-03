#############         Processing of the Covid-19 Data 

#### In this project we aim at implementing a dashboard serving as a follow-up of the Covid-19 breakdown. 
#### The dashboard is built upon up-to-date data drawn from <datahub.io>. 
#### The data are available using the following link https://datahub.io/core/covid-19/datapackage.json

#### This script aims to prepare the data that will be used to create the dashboard and its different fields.

#### Here, we will focus on the treatment of the covid-2019 data that are stored, managed and
#### made available by <datahub.io>. The dataset is drawn from diverse ressources which are cited
#### on the website. With respect to the corona virus breakdown, daily updates can be expected.
#### Therefore, the script is intended to adapt to these variations in order to provide analyses
#### accounting for the evolution of the solution. This means that to maintain a an up-to-date dashboard,
#### one will need to execute this entire script at each time.
#### The actions executed in the "data_preprocessing.R" file are divided to follow the same structure as the one
#### displayed in the dashboard. Every data set that is used in the dashboard creation is first formatted and made 
#### ready to be saved in the disk. Then these data sets are uploaded within the "covid19_dashboard.rmd" to produce
#### the dashboard field.

#### Some attention will be paid to any changes on the data sets structures or on the list of countries !!!
#### If the data coming from <datahub.io> have new countries added we will need to modify the file "countries.csv"
#### to take this modification into account. We will also require to check the map variables to see whether some
#### specific arrangements would be necessary.


#####   1. Installation and Loading the required packages

### Packages installation
# List of packages to be installed
pack.list = list("jsonlite","dplyr", "tidyverse", "htmlwidgets", "rbokeh", "leaflet", "dygraphs", "xts",
                 "DT", "RColorBrewer","geojson","rgdal","classInt","ggplot2","ggmap","maptools") 

insPack   = installed.packages() # List of packages already installed in the ongoing R Session

# Test whether the reauired packages are already installed or not. If so, they are installed.
for(i in 1:length(pack.list)){
  if(pack.list %in% insPack[,"Package"]){
    cat("\nR Package : ",pack.list[[i]]," is already installed.\n")
  }
  else{
    install.packages(pack.list[[i]], repos="https://cran.rstudio.com/")
    cat("\nR Package : ",pack.list[[i]]," is now installed.\n")
  }
}

### Packages Loading
library(jsonlite)
library(dplyr)
library(htmltools)
library(htmlwidgets)
library(rbokeh)
library(leaflet)
library(dygraphs)
library(xts)
library(DT)
library(RColorBrewer)
library(geojson)
library(sp)
library(rgdal)
library(classInt)
library(ggplot2)
library(ggmap)
library(maptools)

#####   2. Download the data from datahub

### Command to download and print the data with json format
json_file <- 'https://datahub.io/core/covid-19/datapackage.json'
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

# get list of all resources:
#print(json_data$resources$name)

# print all tabular data(if exists any)
#for(i in 1:length(json_data$resources$datahub$type)){
#  if(json_data$resources$datahub$type[i]=='derived/csv'){
#    path_to_file = json_data$resources$path[i]
#    data <- read.csv(url(path_to_file))
#    print(data)
#  }
#}

### Assign the data sets of interest into variables

# Collect the file name
ts_covid19_file  = json_data$resources$path[which(json_data$resources$name 
                                                    == "time-series-19-covid-combined_csv")] 
cntries_agg_file = json_data$resources$path[which(json_data$resources$name
                                                    == "countries-aggregated_csv")]
ww_agg_file      = json_data$resources$path[which(json_data$resources$name
                                                    == "worldwide-aggregated_csv")]
ref_cntries_file = json_data$resources$path[which(json_data$resources$name
                                                    == "reference_csv")]
us_confirmed_file = json_data$resources$path[which(json_data$resources$name
                                                   == "us_confirmed_csv")]
# Assignment of the data into variables
ts_covid19       = read.csv(url(ts_covid19_file))
cntries_agg      = read.csv(url(cntries_agg_file))
ww_agg           = read.csv(url(ww_agg_file))
ref_cntries      = read.csv(url(ref_cntries_file))
us_confirmed     = read.csv(url(us_confirmed_file))

#####   3. Data Manipulation To prepare the data

countries = levels(as.factor(cntries_agg$Country))

cntries_agg = arrange(cntries_agg, Country)

#####   4. Test here the visualizations we want to present.

#################################       Page 1: Dashboard on the global situation     #########################

#####   4.1 Graph of the time series of confirmed recovered and deaths globally

# Conversion of the data to the right format
ww_agg_xts = xts(ww_agg[,c("Confirmed","Recovered","Deaths")],as.Date(ww_agg$Date))

# Saving the file to be loaded and used to create the dashboard fields
save(ww_agg_xts, file = "ww_agg_xts.Rdata")

# Test the function to have an insight of the resulting plot
#dygraph(data = ww_agg_xts, xlab="Date", ylab="Number of Cases", main="Daily evolution of the situation") %>%
#  dyOptions(stackedGraph = TRUE) %>%
#  dyRangeSelector(height = 20)

#####   4.2 Tabular analysis of the cases per continent at the last date available

# In this case we will need to add a continent column to the table "cntrie_agg". 

# To this end we will load the countries.csv file with the list of countries and their corresponding
# continent and iso-2 codes.

# Loading csv file
countries = read.csv(file = "countries.csv", header = TRUE, sep = ";")

# Formatting each colum as string
countries$Continent_Region = as.character(countries$Continent_Region)
countries$Country          = as.character(countries$Country)
countries$Code_ISO2        = as.character(countries$Code_ISO2)

# Creating the variables to Cont_Reg and Code_iso to match with the countries in cntrie_agg
Cont_Reg  = rep()
Code_iso  = rep()

# Fill in the variables Cont_Reg and Code_iso
for(i in 1:nrow(cntries_agg)){
  ind = which(as.character(countries$Country) == as.character(cntries_agg$Country[i]))
  Cont_Reg[i] = countries$Continent_Region[ind]
  Code_iso[i] = countries$Code_ISO2[ind]
}

# Adding the created vector of Cont_Reg and Code_iso to cntries_agg
cntries_agg = cbind.data.frame(cntries_agg, Cont_Reg, Code_iso)

# Saving the file that will be loaded and used to create the dashboard
save(cntries_agg, file = "cntries_agg.rdata")

# Extract last date of covid-19 cases available from the datahub.io website
date_available       = as.character(levels(cntries_agg$Date))
last_date_available  = date_available[length(date_available)]

# Extracting the data for each country on this last date
cntries_agg_lastdateav = cntries_agg[which(cntries_agg$Date == last_date_available),]

# Saving these information to be loaded and used to create the dashboard fields
save(cntries_agg_lastdateav, file = "cntries_agg_lastdateav.rdata")
save(last_date_available, file = "last_date_available.rdata")

#####   4.3 Map of the world with the number of confirmed cases per country per day

# To plot the maps and to integrate the information for each country we will need to add 
# latitute and longitude columns to the data
# This information is available 

# Function to define the radius and the color of the circle markers

define_radius    = function(X){
  rad = ifelse(X<1e2, 3,
               ifelse(X<1e3, 5,
                      ifelse(X<1e4, 10,
                             ifelse(X<1e5, 15, 
                                    ifelse(X<1e6,20,25)))))
  return(rad)
}

define_bin_cases = function(X){
  bin = ifelse(X<1e2, "c1",
               ifelse(X<1e3, "c2",
                      ifelse(X<1e4, "c3",
                             ifelse(X<1e5, "c4", 
                                    ifelse(X<1e6, "c5", "c6")))))
  return(bin)
}

Rad = apply(as.matrix(cntries_agg$Confirmed), 1, function(x) define_radius(x)) 
Bin = apply(as.matrix(cntries_agg$Confirmed), 1, function(x) define_bin_cases(x))

# Function used to return the coordinates of the countries in ref_cntries

cntry_coord_func = function(df = ref_cntries){
  C_or_P = apply(as.matrix(1:nrow(df)), 1, function(x) 
    return(ifelse(df[x,"Country_Region"] == df[x,"Combined_Key"],1,0)))
  ind    = which(C_or_P == 1)
  new_df = df[ind,c(1:4,8:10)]
  return(arrange(new_df, Country_Region))
}

cntries_coord = cntry_coord_func(df = ref_cntries)

# Save the table of countries with their coordinates
save(cntries_coord, file = "cntries_coord.rdata")

# Create the vectors of latitude and longitude to be concatenated to the cntries_agg dataframe
lat_  = rep()
long_ = rep()
for(i in 1:nrow(cntries_agg)){
  ind = which(cntries_coord$Country_Region == cntries_agg$Country[i])
  lat_[i] = cntries_coord$Lat[ind]
  long_[i] = cntries_coord$Long_[ind]
}

cntries_agg = cbind.data.frame(cntries_agg, lat_, long_, Rad, Bin)

# Save the resulting file that will be loaded and used to create the dashboard mapping field
save(cntries_agg, file = "cntries_agg.rdata")


#####   4.4 Value Box at the last date available

Confirmed_ldav = ww_agg[nrow(ww_agg),"Confirmed"] 
Recovered_ldav = ww_agg[nrow(ww_agg),"Recovered"]
Deaths_ldav     = ww_agg[nrow(ww_agg),"Deaths"]

save(Confirmed_ldav, file = "Confirmed_ldav.rdata")
save(Recovered_ldav, file = "Recovered_ldav.rdata")
save(Deaths_ldav, file = "Deaths_ldav.rdata")

#################################       Page 2: Dashboard on country-specific information     ####################

#### In this part we focus on the treatment on country-specific information and specifically on
#### USA, Germany, France and Senegal.
#### This involves the processing on the data contained in datahub.io database and webscraping to look for
#### the german, french and senegalese data.

library(geojsonio)
Bins_cases = c(0, 10^(1:7))

#####   5.1 USA 

### Have a look at the tutorial to create the usa chloropleth from leaflet page in rstudio.github.io

###   Load the map of us states

# transfrom .json file into a spatial polygons data frame
us_states = geojson_read( 
  x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json",
  what = "sp")

# Extraction of the number of cases in USA at the last date available
us_confirmed_ldav = us_confirmed  %>%
  select("Province.State","iso2","code3","Lat", "Long", "Date", "Case") %>%
  filter(Date == last_date_available)

# Summary of the total number of cases in each state
us_cases_ldav = us_confirmed_ldav %>%
  group_by(Province.State) %>%
  summarize(StatesCases = sum(Case))

# Matching the total number of cases per state in "us_cases_ldav" data with the "us_states" map
ind_match_us = which(!is.na(match(us_cases_ldav$Province.State,us_states$name)))
us.cases     = matrix(NA, ncol = 1, nrow = length(ind_match_us))

for(i in 1:nrow(us.cases)){
  ind = which(as.character(us_cases_ldav$Province.State) == us_states$name[i])
  us.cases[i,1] = us_cases_ldav$StatesCases[ind] 
}

us_states$Cases = us.cases # Create the column of the Cases and add it to the "us_states" map

# Creation of the chloropleth map

us_states$labels = paste0( "<strong> Province/State: </strong> ", us_states$name, "<br/> ",
                           "<strong> Cases: </strong> ", us_states$Cases, "<br/> ") %>%
                   lapply(htmltools::HTML) # Creation of the labels

save(us_states, file = "us_states.rdata")

Pal_us  = colorBin(palette = "YlOrRd", domain=us_states$Cases, bins=Bins_cases) # Create the palette of colors

#map_us_cases_ldav = leaflet(data = us_states) %>%
#  addTiles() %>%
#  addPolygons(fillColor = ~ Pal_us(Cases), color = "white",fillOpacity = 0.7,label = ~labels,
#    highlight = highlightOptions(color = "black", bringToFront = TRUE)) %>%
#  leaflet::addLegend(pal = Pal_us, values = ~Cases, opacity = 0.7, title = "Number of cases")

#map_us_cases_ldav

#####   5.2 Germany

###   Load the map of German regions associated with covid-19 data 
de_faellen = geojson_read(
  x = "https://opendata.arcgis.com/datasets/ef4b445a53c1406892257fe63129a8ea_0.geojson",
  what = "sp")

de_faellen$labels = paste0( "<strong> Region: </strong> ", de_faellen$LAN_ew_GEN, "<br/> ",
                            "<strong> Cases: </strong> ",  de_faellen$Fallzahl, "<br/> ",
                            "<strong> (Deaths: </strong> ",de_faellen$Death, ")<br/> ") %>%
  lapply(htmltools::HTML) # Creation of the labels

save(de_faellen, file="de_faellen.rdata") # Save the data to be used in the dashboard

Pal_de = colorBin(palette="YlOrRd", domain=de_faellen$Fallzahl, bins=Bins_cases) # Create the palette of colors

#leaflet(data = de_faellen) %>%
#  addTiles() %>%
#  setView(9.9,51.5,6) %>%
#  addPolygons(fillColor = ~ Pal_de(Fallzahl), color = "white",fillOpacity = 0.7,label = ~labels,
#              highlight = highlightOptions(color = "black", bringToFront = TRUE)) %>%
#  leaflet::addLegend(pal=Pal_de, values=~Fallzahl, opacity=0.7, title="Number of cases", position="bottomleft")

#####   5.3 France

###   Load the map of French regions

### Shapefiles from OSM to be downloaded
#download.file(url = "http://osm13.openstreetmap.fr/~cquest/openfla/export/departements-20190101-shp.zip",
#              destfile = "GeoData/departements-20190101-shp.zip")
#download.file(url = "http://osm13.openstreetmap.fr/~cquest/openfla/export/regions-20190101-shp.zip",
#              destfile = "GeoData/regions-20190101-shp.zip")

### Unzip the files
#unzip(zipfile = "GeoData/departements-20190101-shp.zip", exdir = "GeoData/departements-20190101-shp")
#unzip(zipfile = "GeoData/regions-20190101-shp.zip", exdir = "GeoData/regions-20190101-shp")

###   Load the shapefile into the R Session

france_dept = readOGR(dsn = "GeoData/departements-20190101-shp",layer = "departements-20190101",
                      stringsAsFactors = FALSE)
france_reg  = readOGR(dsn = "GeoData/regions-20190101-shp",layer = "regions-20190101",
                      stringsAsFactors = FALSE)

### 
france_dept = france_dept[france_dept@data$code_insee %in% sprintf("%02d", (1:95)[-20]), ]

### Correcting some typos
france_reg$nom[1] = "La Reunion"
france_reg$nom[3] = "Ile-de-France"
france_reg$nom[13] = "Auvergne-Rhone-Alpes"
france_reg$nom[14] = "Bourgogne-Franche-Comte"
france_reg$nom[18] = "Provence-Alpes-Cote d'Azur"

###   Download the Covid-19 data
fr_cas = read.csv(url("https://www.data.gouv.fr/fr/datasets/r/0b66ca39-1623-4d9c-83ad-5434b7f9e2a4"))

fr_last_date_av = as.character(levels(fr_cas$date)[length(levels(fr_cas$date))])

fr_cas_region = fr_cas %>%
  filter(granularite == "region") %>%
  filter(date == fr_last_date_av) %>%
  select(date,maille_code,maille_nom, deces,reanimation,hospitalises,gueris)
  
### Correcting the typos
fr_cas_region$maille_nom = as.character(fr_cas_region$maille_nom)
fr_cas_region$maille_nom[4] = "La Reunion"
fr_cas_region$maille_nom[6] = "Ile-de-France"
fr_cas_region$maille_nom[8] = "Bourgogne-Franche-Comte"
fr_cas_region$maille_nom[16] = "Auvergne-Rhone-Alpes"
fr_cas_region$maille_nom[17] = "Provence-Alpes-Cote d'Azur"

### Arranging the data by region name
fr_cas_region = arrange(fr_cas_region,maille_nom)

### Adding the cases information to the map file
Deaths       = matrix(NA, ncol = 1, nrow = nrow(fr_cas_region))
Hospitalized = matrix(NA, ncol = 1, nrow = nrow(fr_cas_region))
Recovered    = matrix(NA, ncol = 1, nrow = nrow(fr_cas_region))
Reanimation  = matrix(NA, ncol = 1, nrow = nrow(fr_cas_region))

for(i in 1:nrow(fr_cas_region)){
  ind = which(france_reg$nom[i]==fr_cas_region$maille_nom)
  Deaths[i]       = fr_cas_region$deces[ind]
  Hospitalized[i] = fr_cas_region$hospitalises[ind]
  Recovered[i]    = fr_cas_region$gueris[ind]
  Reanimation[i]  = fr_cas_region$reanimation[ind]
}

Cases = Deaths + Hospitalized + Recovered + Reanimation

france_reg$Deaths       = Deaths
france_reg$Hospitalized = Hospitalized
france_reg$Recovered    = Recovered
france_reg$Reanimation  = Reanimation
france_reg$Cases        = Cases

rm(Deaths, Recovered, Hospitalized, Reanimation, Cases)

fr_covid19_ldav = france_reg

fr_covid19_ldav$labels = paste0( "<strong> Region: </strong> ", fr_covid19_ldav$nom, "<br/> ",
                            "<strong> Cases: </strong> ",  fr_covid19_ldav$Cases, "<br/> ",
                            "<strong> (Deaths: </strong> ",fr_covid19_ldav$Deaths, ")<br/> ") %>%
  lapply(htmltools::HTML) # Creation of the labels

save(fr_covid19_ldav, file = "fr_covid19_ldav.rdata")
  
Pal_fr = colorBin(palette="YlOrRd", domain=fr_covid19_ldav$Cases, bins=Bins_cases) # Create the palette of colors

#leaflet(data = fr_covid19_ldav) %>%
#  addTiles() %>%
#  setView(9.9,51.5,6) %>%
#  addPolygons(fillColor = ~ Pal_de(Cases), color = "white",fillOpacity = 0.7,label = ~labels,
#              highlight = highlightOptions(color = "black", bringToFront = TRUE)) %>%
#  leaflet::addLegend(pal=Pal_fr, values=~Cases, opacity=0.7, title="Number of cases", position="bottomleft")


## Release the memory
rm(france_dept, france_reg)


#####   5.4 Concatenate the data used for USA, Germany and France and prepare a data.table with input selection

cntry_column = c(rep("USA",nrow(us_states@data)),
                 rep("Germany",nrow(de_faellen@data)),
                 rep("France",nrow(fr_covid19_ldav@data)))
dat.tab      = cbind.data.frame(Country         = cntry_column,
                                Province_Region = c(us_states@data$name,
                                                    de_faellen@data$LAN_ew_GEN,
                                                    fr_covid19_ldav@data$nom),
                                Cases           = c(us_states@data$Cases,
                                                    de_faellen@data$Fallzahl,
                                                    fr_covid19_ldav@data$Cases))
#DT::datatable(dat.tab)

save(dat.tab, file = "dat.tab.3.cntries.rdata")


#################################       Page 3: Dashboard on time evolution: World     ###########################

#### In this part we focus on the treatment of the timeseries data and specifically on USA.
#### This involves the processing of the data in the data frame "cntries_agg", the creation of a time slider

## Loading the required package for loading the worldmap
library(maptools)

data(wrld_simpl)
map = wrld_simpl
rm(wrld_simpl)

names(map)[names(map) == "NAME"] = "Country"

## Filter the countries or entities (like cruise boats) that do not match with the ones in the map data

covid_dat = cntries_agg %>%
  filter(Country != "Diamond Princess") %>%
  filter(Country != "Kosovo") %>%
  filter(Country != "MS Zaandam") %>%
  filter(Country != "South Sudan")

#unique(as.character(covid_dat$Country))
#i1 = which(is.element(map$Country,unique(as.character(covid_dat$Country))) == FALSE)
#i2 = which(is.element(map$Country,unique(as.character(covid_dat$Country))) == TRUE)
#cnt = map$Country[i2]
#i3 = which(is.element(unique(as.character(covid_dat$Country)),cnt) == FALSE)
#cnt_cov = unique(as.character(covid_dat$Country))[i3]

## Adapt countries names in the covid data to match the ones used in the map file

levels(covid_dat$Country)[which(levels(covid_dat$Country)=="Brunei")]  = "Brunei Darussalam"
levels(covid_dat$Country)[which(levels(covid_dat$Country)=="Cabo Verde")]  = "Cape Verde"
levels(covid_dat$Country)[which(levels(covid_dat$Country)=="Congo (Brazzaville)")]  = "Congo"
levels(covid_dat$Country)[which(levels(covid_dat$Country)=="Congo (Kinshasa)")]  = "Democratic Republic of the Congo"
levels(covid_dat$Country)[which(levels(covid_dat$Country)=="Czechia")]  = "Czech Republic"
levels(covid_dat$Country)[which(levels(covid_dat$Country)=="Eswatini")]  = "Swaziland"
levels(covid_dat$Country)[which(levels(covid_dat$Country)=="Holy See")]  = "Holy See (Vatican City)"
levels(covid_dat$Country)[which(levels(covid_dat$Country)=="Iran")]  = "Iran (Islamic Republic of)"
levels(covid_dat$Country)[which(levels(covid_dat$Country)=="Korea, South")]  = "Korea, Republic of"
levels(covid_dat$Country)[which(levels(covid_dat$Country)=="Laos")]  = "Lao People's Democratic Republic"
levels(covid_dat$Country)[which(levels(covid_dat$Country)=="Libya")]  = "Libyan Arab Jamahiriya"
levels(covid_dat$Country)[which(levels(covid_dat$Country)=="Moldova")] = "Republic of Moldova"
levels(covid_dat$Country)[which(levels(covid_dat$Country)=="North Macedonia")] = "The former Yugoslav Republic of Macedonia"
levels(covid_dat$Country)[which(levels(covid_dat$Country)=="Syria")] = "Syrian Arab Republic"
levels(covid_dat$Country)[which(levels(covid_dat$Country)=="Taiwan*")] = "Taiwan"
levels(covid_dat$Country)[which(levels(covid_dat$Country)=="Tanzania")] = "United Republic of Tanzania"
levels(covid_dat$Country)[which(levels(covid_dat$Country)=="US")] = "United States"
levels(covid_dat$Country)[which(levels(covid_dat$Country)=="Vietnam")] = "Viet Nam"
levels(covid_dat$Country)[which(levels(covid_dat$Country)=="West Bank and Gaza")] = "Palestine"

## Extract the countries which are in both data sets from the map

covid_map = subset(map, is.element(map$Country,unique(as.character(covid_dat$Country))))
rm(map)

## Save the file that are now "aligned" to be used to create the interactive dynamic map
save(covid_map, file = "covid19_world_map.rdata")
save(covid_dat, file = "covid19_world_data.rdata")


#-----------------------------------------    End of the script   ---------------------------------------------------------#
