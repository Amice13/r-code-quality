#Code to plot geographical locations of sites involved in the study.
#
require(rgdal)
require(ggplot2)
library(raster)
library(ggspatial)

#shape file downloaded from 'https://gadm.org/download_country_v3.html'

shp = readOGR('gadm36_KEN_1.shp', stringsAsFactors = F)


map <- ggplot() + 
  geom_polygon(data = shp, aes(x = long, y = lat, group = group), 
               colour = "black", fill = NA, show.legend = T
                 ) +
  theme_void()


labs <- data.frame(
  long = c(39.854,34.290,36.780,34.357),
  lat = c(-3.631,0.098,-1.316,-0.140),
  names = c("SWFSC-FED", "NWFSC", "HBDUEVU", 'HDUWGU'),
  stringsAsFactors = FALSE
) 

map1 = map + geom_point(aes(x = 39.854, y = -3.631),fill = "orange", size = 5, shape = 23)
map2 = map1+geom_point(aes(x = 34.290, y = 0.098), fill = "green", size = 5,shape = 23)
map3 = map2+geom_point(aes(x = 36.780, y = -1.316), fill = "red", size = 5,shape = 23)
map4 = map3 +geom_point(aes(x = 34.357, y = -0.140), fill = "blue", size = 5,shape = 23)
map4 + 
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.8, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
