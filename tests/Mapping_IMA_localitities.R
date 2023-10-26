# Template for

library(httr)
library(jsonlite)
library(OpenMindat)
library(tidyverse)
library(sf)
library(mapview)


#You should get a token from mindat.org
test_base_token = "2082edf7b8dab2b9887f3c2393e822c6"
mindat_connection(test_base_token,page_size = 1500)

######## demo1 localitities contain, not contain, contain but without some elements ########
df_elements <- localities_list_elems_inc(c("As"))#"Dy","Li"
mapview(df_elements, xcol = "longitude", ycol = "latitude", crs = 4269, grid = FALSE)


####### demo2 localities within a country ##########
localities_list_country
df_country <- localities_list_country(c("China"))
mapview(df_country, xcol = "longitude", ycol = "latitude", crs = 4269, grid = FALSE)

####### demo3 localities matched the input description ##########
df_desc <- localities_list_description("volcano")
mapview(df_desc, xcol = "longitude", ycol = "latitude", crs = 4269, grid = FALSE)

####### demo4 localities description ##########
df_imalist <- minerals_ima_list_ima(1)
df_tp_locs <-df_imalist$type_localities

df_out <- data.frame()
for (elements in df_tp_locs) {
  elm_id_list <- as.list(elements)
  for (elm in elm_id_list){
    df_cur_locality <- localities_retrieve_id(elm)
    df_out <- rbind(df_out,df_cur_locality)
  }
}
df_out2 <- df_out
df_out2$longitude <- as.numeric(df_out2$longitude)
df_out2$latitude <- as.numeric(df_out2$latitude)
mapview(df_out2, xcol = "longitude", ycol = "latitude", crs = 4269, grid = FALSE)


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

