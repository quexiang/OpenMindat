# Template for
library(tidyverse)

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
