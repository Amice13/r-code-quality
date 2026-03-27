library(tidyverse)
library(leaflet)


#install.packages("parzer")
library(parzer)

#install.packages("maps")
#install.packages("mapdata")
#library(maps)       
#library(mapdata)
#library(leaflet.providers)
#remotes::install_github("rstudio/leaflet.providers")

cities <- read_csv2('./Popular_Film_data/Coordinates_final_version.csv')
#cities


cities.dd = cities %>%
  mutate(Lon = parzer::parse_lon(Longitude),
         Lat = parzer::parse_lat(Latitude))

pal_cat <- colorFactor("Spectral", domain = cities.dd$Comment)



map <- cities.dd %>%
  leaflet() %>%
  addTiles(
    urlTemplate = "https://tiles.stadiamaps.com/tiles/{variant}/{z}/{x}/{y}{r}.png?api_key={apikey}",
    attribution = paste('&copy; <a href="https://stadiamaps.com/" target="_blank">Stadia Maps</a> ' ,
                        '&copy; <a href="https://stamen.com/" target="_blank">Stamen Design</a> ' ,
                        '&copy; <a href="https://openmaptiles.org/" target="_blank">OpenMapTiles</a> ' ,
                        '&copy; <a href="https://www.openstreetmap.org/copyright" target="_blank">OpenStreetMap</a>'),
    options = tileOptions(variant='stamen_toner', apikey = 'YOUR-API-KEY')
  )  %>%
  fitBounds(lng1 = -86.1581, lat1 = 39.7684, lng2 = -87.1581, lat2 = 40.7684)%>%
  addCircles(lng = ~Lon,
             lat = ~Lat, 
             label = ~Count,
             popup = ~City,
             color  = ~pal_cat(Comment),
             opacity = 0.9,
             weight= ~Weight)%>%
  addLegend(position = "bottomleft",
            title = 'Weight of the cities',
            pal = pal_cat,
            values = ~Comment
  )

map

map%>%
  addPopups(~Lon, ~Lat, ~City,
            options = popupOptions(closeButton = FALSE)
  )


