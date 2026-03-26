rm(list = ls())

library(sf)
library(dplyr)
library(purrr)
library(ggplot2)
library(haven)

###########################################################
#Setting projection epsg for diplaying maps
readin.proj=5070 #because it works with the lat and lons provided

us_co <- st_read('FILEPATH/data/shapefiles/cb_2018_us_county_5m.shp')
us_co$geoid <- us_co$GEOID

bat.dat <- read_dta("FILEPATH/data/tomap.dta")
################
#Make data.frame spatial using sf by merging with county spatial layer by fips (geoid)

g.bat.dat <- left_join(x=us_co,
                        y=bat.dat,
                        by="geoid")
#check if it is an sf (spatial) object
class(g.bat.dat)

#build the map in layers (first=bottom)

ggplot() +
  geom_sf(data = us_co,fill="white",color=NA) +  #basemap (outlines are color, fill is area; NA is transparent)
  geom_sf(data = g.bat.dat %>% dplyr::filter(treatment==0),fill="white",color="darkgray") + #plot layer of no treatment but yes data
  geom_sf(data = g.bat.dat %>% dplyr::filter(treatment==1),aes(fill=factor(year)),color="black") +
  labs(color = "test") +
  scale_fill_grey(start = 0.0, end = 0.9) +
  theme_void() +
coord_sf(xlim = c(-103, -70), ylim = c(23, 50)) +
  labs(fill = "Outbreak year")


#ggsave("FILEPATH/batMapBW.png")
  


