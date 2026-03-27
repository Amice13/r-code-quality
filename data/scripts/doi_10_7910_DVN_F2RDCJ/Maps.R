remove(list=ls())

setwd("~/Desktop/AJPS Replication/")

install.packages(c("terra", "tidyverse", "dplyr", "sp", "raster", "sf", "geos", "tmap",
                   "tmaptools", "leaflet", "haven", "rnaturalearth", "rmapshaper", "haven", "leaflet", "rgdal", "ggplot2"))

library("terra")
library("tidyverse")
library("dplyr")
library("sp")
library("raster")
library("sf")
library("geos")
library("tmap")
library("tmaptools")
library("leaflet")
library("haven")
library("rnaturalearth")
library("rmapshaper")
library("haven")
library("leaflet")
library("rgdal")
library("ggplot2")

grayscale <- c("#E8E8E8", "#BEBEBE", "#888888", "#505050", "#101010")
bluescale <- c("#eff3ff", "#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c")
wbluescale <- c("#ffffff", "#bdd7e7", "#6baed6", "#3182bd", "#08519c")
wgrayscale <- c("#ffffff", "#cccccc", "#969696", "#636363", "#252525")

data <- read_dta("iraq_panel_final.dta")

iraq<- st_read("shapefile/iraq_districts/shapefile/iraq_districts/iraq_districts.shp")
iraq <- st_make_valid(iraq)

roads<- st_read("shapefile/ocha_roads/shapefile/ocha_roads/ocha_roads.shp")
roads <- st_make_valid(roads)

primary<- st_read("shapefile/primary_smuggling/shapefile/primary_smuggling/primary_smuggling.shp")
primary <- st_make_valid(primary)

secondary<- st_read("shapefile/secondary_smuggling/shapefile/secondary_smuggling/secondary_smuggling.shp")
secondary <- st_make_valid(secondary)

# Smuggling Routes

ratlines <- tm_shape(iraq) + tm_borders(col="grey")+
tm_shape(primary) + tm_lines(col="firebrick3", lwd = 2)+
tm_shape(secondary) + tm_lines(col="dodgerblue3", lwd = 2)+
tm_layout(" ", title.size=1, legend.position = c("left","bottom"), title.position=c(.02,.96), frame = FALSE)

ratlines
tmap_save(ratlines, "figures/ratlines.png", height = 7, width=7)

# May 2004 Fortification

data0504 <- data[ which(data$moyr==532),]

names(data0504)[names(data0504) == "district"] <- "ADM3NAME"

map <- merge(iraq, data0504, by='ADM3NAME')

fort0504 <- tm_shape(map) +
  tm_borders()+
  tm_fill("num_fort", title = "Number of Border Forts", palette=wbluescale, style="fixed",
          breaks=c(0, 1, 5, 15, 25, 38), interval.closure="left", 
          labels=c("0", "1-4","5-14", "15-24", "25-37"))+
  tm_layout("May 2004", title.size=1,
            legend.position = c("left","bottom"), title.position=c(.02,.96), frame = FALSE)

fort0504
tmap_save(fort0504, "figures/fort0504.png", height = 7, width=7)

# Nov. 2004 Fortification

data1104 <- data[ which(data$moyr==538),]

names(data1104)[names(data1104) == "district"] <- "ADM3NAME"

map <- merge(iraq, data1104, by='ADM3NAME')

fort1104 <- tm_shape(map) +
  tm_borders()+
  tm_fill("num_fort", title = "Number of Border Forts", palette=wbluescale, style="fixed",
          breaks=c(0, 1, 5, 15, 25, 38), interval.closure="left", 
          labels=c("0", "1-4","5-14", "15-24", "25-37"))+
  tm_layout("Nov. 2004", title.size=1,
            legend.position = c("left","bottom"), title.position=c(.02,.96), frame = FALSE)

fort1104
tmap_save(fort1104, "figures/fort1104.png", height = 7, width=7)

# May 2005 Fortification

data0505 <- data[ which(data$moyr==544),]

names(data0505)[names(data0505) == "district"] <- "ADM3NAME"

map <- merge(iraq, data0505, by='ADM3NAME')

fort0505 <- tm_shape(map) +
  tm_borders()+
  tm_fill("num_fort", title = "Number of Border Forts", palette=wbluescale, style="fixed",
          breaks=c(0, 1, 5, 15, 25, 38), interval.closure="left", 
          labels=c("0", "1-4","5-14", "15-24", "25-37"))+
  tm_layout("May 2005", title.size=1,
            legend.position = c("left","bottom"), title.position=c(.02,.96), frame = FALSE)

fort0505
tmap_save(fort0505, "figures/fort0505.png", height = 7, width=7)

# Nov. 2005 Fortification

data1105 <- data[ which(data$moyr==550),]

names(data1105)[names(data1105) == "district"] <- "ADM3NAME"

map <- merge(iraq, data1105, by='ADM3NAME')

fort1105 <- tm_shape(map) +
  tm_borders()+
  tm_fill("num_fort", title = "Number of Border Forts", palette=wbluescale, style="fixed",
          breaks=c(0, 1, 5, 15, 25, 38), interval.closure="left", 
          labels=c("0", "1-4","5-14", "15-24", "25-37"))+
  tm_layout("Nov. 2005", title.size=1,
            legend.position = c("left","bottom"), title.position=c(.02,.96), frame = FALSE)

fort1105
tmap_save(fort1105, "figures/fort1105.png", height = 7, width=7)

# May 2006 Fortification

data0506 <- data[ which(data$moyr==556),]

names(data0506)[names(data0506) == "district"] <- "ADM3NAME"

map <- merge(iraq, data0506, by='ADM3NAME')

fort0506 <- tm_shape(map) +
  tm_borders()+
  tm_fill("num_fort", title = "Number of Border Forts", palette=wbluescale, style="fixed",
          breaks=c(0, 1, 5, 15, 25, 38), interval.closure="left", 
          labels=c("0", "1-4","5-14", "15-24", "25-37"))+
  tm_layout("May 2006", title.size=1,
            legend.position = c("left","bottom"), title.position=c(.02,.96), frame = FALSE)

fort0506
tmap_save(fort0506, "figures/fort0506.png", height = 7, width=7)

# Nov. 2006 Fortification

data1106 <- data[ which(data$moyr==562),]

names(data1106)[names(data1106) == "district"] <- "ADM3NAME"

map <- merge(iraq, data1106, by='ADM3NAME')

fort1106 <- tm_shape(map) +
  tm_borders()+
  tm_fill("num_fort", title = "Number of Border Forts", palette=wbluescale, style="fixed",
          breaks=c(0, 1, 5, 15, 25, 38), interval.closure="left", 
          labels=c("0", "1-4","5-14", "15-24", "25-37"))+
  tm_layout("Nov. 2006", title.size=1,
            legend.position = c("left","bottom"), title.position=c(.02,.96), frame = FALSE)

fort1106
tmap_save(fort1106, "figures/fort1106.png", height = 7, width=7)

