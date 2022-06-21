# Install packages required for this exercise
install.packages("tidyverse")
install.packages("sf")
install.packages("leaflet")
install.packages("janitor")
install.packages("tmap")

# Load these packages after you've installed them so you can use them in this project
library(tidyverse)
library(sf)
library(leaflet)
library(janitor)
library(tmap)


####Data Loading####
##Load in the data for the exercise and take a look at it

#first we'll read in a geographical layer of Local Authority Districts (LADs)
LADs <- st_read("data/LAD_2021_with_SAM.shp")

#we'll look at the attributes in the data
glimpse(LADs)

#when we loaded the data the console said it was a 'simple feature collection' 
#which means it's geographic data
#let's take a look at it by plotting a quick tmap
qtm(LADs)


#now we'll read population data (mid year population estimates for 2020)
population <- read_csv("data/ONS_2020_mid_year_pop_estimates_by_LAD.csv")

#look at the data to see what attributes we have
glimpse(population)



#### Data Cleaning####
## Now we start data cleaning where we get the data ready to use

#use Janitor to clean up the weirdly formatted column headers to make them easier to use
population <- clean_names(population)
#take a look to see how the names changed
glimpse(population)


#calculate a new column with total number of 14-18 year olds in each area
population <- mutate(population, students = x14 + x15 + x16 + x17 + x18)
#take a look to see the new column you've added
glimpse(population)

#select the columns we want to use and drop the ones we don't need
population_selection <- select(population, code, students, all_ages)

#now we will combine the two layers into one ready for mapping
#we'll use a join to do this (more info on joins here: https://statisticsglobe.com/r-dplyr-join-inner-left-right-full-semi-anti)
LADs_population <- left_join(LADs, population_selection, by = c("LAD21CD" = "code"))


####Analysis####
#now we'll move onto our analysis and mapping

#we're aiming to make a choropleth map so our data needs to be a ratio or percentage - we'll do that first.
#what is a choropleth? Answer: https://datavizcatalogue.com/methods/choropleth.html

#we'll calculate the population density for 14-18 year olds
#we use the Standard Area Measurement (the official area) of each LAD to calculate this
LADs_population <- mutate(LADs_population, student_density = (students/area_ha))

#take a look at the columns we've created
glimpse(LADs_population)


####Mapping####
#now we have our variables, let's make our map

#First we have to define the colour scheme of our map
pal <- colorBin(palette = "Blues", domain = LADs_population$student_density, bins = 8, pretty = FALSE)

#We're going to make some labels. Leaflet uses HTML to display labels so we can format our labels nicely.
LADs_population$labels <- paste("<strong>", LADs_population$LAD21NM, "</strong>", "<br>", 
                                LADs_population$student_density) %>% 
                            lapply(htmltools::HTML)

#Then we make our map.
#We give it some base mapping so we know where we are using addProviderTiles
#Then we add our data using addPolygons - we also make our map look good here too
#then we add our map's legend so we know what we're looking at.

map <- leaflet(LADs_population) %>% 
  addProviderTiles(providers$Esri.WorldTopoMap) %>% 
  addPolygons(fillColor = ~pal(student_density),
              fillOpacity = 0.8,
              weight = 1, 
              color = "grey",
              highlightOptions = highlightOptions(
                  weight = 2,
                  color = "black",
                  bringToFront = TRUE),
              label = LADs_population$labels) %>% 
  addLegend("bottomright", 
            pal = pal, 
            values = ~student_density,
            title = "population density for 14-18s")

#Now let's take a look at our map.
map


#There is data for the total population of each LAD in the all_ages column of the LADs_population object.
#How about making your own map by calculating the % of each LAD aged 14-18 and mapping it?