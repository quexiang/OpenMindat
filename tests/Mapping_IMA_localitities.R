# Template for

library(httr)
library(jsonlite)
library(OpenMindat)
library(tidyverse)
library(sf)
library(mapview)
####demo1####
# starbucks <- read_csv("https://raw.githubusercontent.com/libjohn/mapping-with-R/master/data/All_Starbucks_Locations_in_the_US_-_Map.csv")
# starbucksNC <- starbucks  %>% filter(State == "NC")
# starbucksNC %>% glimpse()
# mapview(starbucksNC, xcol = "Longitude", ycol = "Latitude", crs = 4269, grid = FALSE)



######## demo1  Element localitities ########
#You should get a token from mindat.org
test_base_token = "2082edf7b8dab2b9887f3c2393e822c6"
mindat_connection(test_base_token,page_size = 1500)
df_elements <- localities_list_elems_inc(c("Dy"))
mapview(df_elements, xcol = "longitude", ycol = "latitude", crs = 4269, grid = FALSE)





# create data for world coordinates using map_data() function
world_coordinates <- map_data("world")

# read volcano_eruption data from volcano.csv
volcano_eruption <- readr::read_csv("volcano.csv")

# create world map using ggplot() function
ggplot() +
  # geom_map() function takes world coordinates as input
# to plot world map color parameter determines the
# color of borders in map fill parameter determines the
# color of fill in map size determines the thickness of
# border in map
geom_map(
  data = world, map = world,
  aes(long, lat, map_id = region),
  color = "green", fill= "lightyellow"
)+
  # geom_point function is used to plot scatter plot on top
  # of world map
  geom_point(
    data = volcano_eruption,
    aes(longitude, latitude, color = primary_volcano_type,
        size=population_within_10_km),
    alpha = 1
  ) +

  # legend.position as none removes the legend
  theme(legend.position="none")
