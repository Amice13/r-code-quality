################################################################################################
#####Replication Code for Figure 1 in ##########################################################
####"Who leaves and who returns? IDPs and Returnees after the Russian Invasion of Ukraine#######
####by Konstantin Ash. Forthcoming in Comparative Migration Studies#############################
################################################################################################
####Original Maps made with tmap 3.3 with manual adjustment(see Appendix)#######################
####Changes in tmap 4.1 require adjustments for perfect replication#############################
################################################################################################


library(automap)
library(data.table)
library(raster) ##WAy of uploading rasters
library(rworldmap) ##Essentially a worldmap basemap
library(sp) #automatically downloads with rgdal
library(gstat) #useful for interpolation
library(RColorBrewer)
library(tmap)
library(latticeExtra)
library(tmaptools)
library(sf)
library(dplyr)
library(cols4all)


setwd("~/UKR_adm")
return <- st_read("UKR_adm1_2022_final2.shp")
pre2014 <- st_read("UKR_adm1_2013.shp")
arrival <- st_read("UKR_POL_2022.shp")
departure <- st_read("UKR_adm1.shp")
bg <- st_read("UKR_adm0.shp")


departure <- st_make_valid(departure)
pre2014 <- st_make_valid(pre2014)

#departure$MIGRATED[departure$MIGRATED ==0] <-NA
#arrival$MIGRATED[arrival$MIGRATED ==0] <-NA
#return$MIGRATED[return$MIGRATED ==0] <-NA

names(departure)[names(departure) == "MIGRATED"] <- "Num. Departures"
names(arrival)[names(arrival) == "MIGRATED"] <- "Num. Arrivals"
names(return)[names(return) == "MIGRATED"] <- "Num. Returnees"


####Number of Departures#####

pal <- c("white",unique(c4a("brewer.Blues", n = 8, range = c(0.2, 0.9))))
breaks <-  c(-0.1, 0.5, 10, 20, 30, 40, 50, 60, 70)
labels <- c("None", "1–10", "11–20", "21–30", "31–40", "41–50", "51–60", "61-70")


tmap_mode("plot")

tm_shape(pre2014) +
  tm_polygons(col = "grey") +
  tm_shape(departure) +
  tm_polygons(
    fill = "Num. Departures",
    col = "black",
    fill.scale = tm_scale_intervals(
      breaks = breaks,
      values = pal,
      labels = labels,
      value.na = "white",
      label.na = ""
    ),
    fill.legend = tm_legend(
      title = "Num. Departures",
      na.label = "",
      na.show = FALSE
    )
  )+
  tm_text("NAME_1", size = 0.6, remove.overlap = TRUE) +
  tm_layout(
  inner.margins = c(0.3, 0.1, 0.1, 0.3),
    legend.position = c("right", "top"),  # move legend inside
    legend.bg.color = "transparent",                   # transparent background
    legend.frame = FALSE,                   # remove box around legend
    frame = FALSE ,
  bg.color = "transparent"  # remove outer frame
  )
 
  
####Number of Arrivals#####
pal2 <- c("white", unique(c4a("brewer.Greens", n = 7, range = c(0.2, 0.9))))
breaks2 <-  c(-0.1, 0.5, 10, 20, 30, 40, 50, 60)
labels2 <- c("None", "1–10", "11–20", "21–30", "31–40", "41–50", "51–60")


tmap_mode("plot")

tm_shape(pre2014) +
  tm_polygons(col = "grey") +
  tm_shape(arrival) +
  tm_polygons(
    fill = "Num. Arrivals",
    col = "black",
    fill.scale = tm_scale_intervals(
      breaks = breaks2,
      values = pal2,
      labels = labels2,
      value.na = "white",
      label.na = ""
    ),
    fill.legend = tm_legend(
      title = "Num. Arrivals",
      na.show = FALSE,
      na.label = ""
    ) )+
  tm_text("NAME_1", size = 0.6, remove.overlap = TRUE) +
  tm_layout(
    inner.margins = c(0.3, 0.1, 0.1, 0.3),
    legend.position = c("right", "top"),  # move legend inside
    legend.bg.color = "transparent",                   # transparent background
    legend.frame = FALSE,                   # remove box around legend
    frame = FALSE ,
    bg.color = "transparent"  # remove outer frame
  )


####Number of Returnees#####
pal3 <- c("white", unique(c4a("brewer.OrRd", n = 7, range = c(0.2, 0.9))))
breaks3 <-  c(-0.1, 0.5, 5, 10, 15, 20, 25, 30)
labels3 <- c("None", "1–5", "6-10", "11-15", "16-20", "21-25", "26-30")


tmap_mode("plot")

tm_shape(pre2014) +
  tm_polygons(col = "grey") +
  tm_shape(return) +
  tm_polygons(
    fill = "Num. Returnees",
    col = "black",
    fill.scale = tm_scale_intervals(
      breaks = breaks3,
      values = pal3,
      labels = labels3,
      value.na = "white",
      label.na = ""
    ),
    fill.legend = tm_legend(
      title = "Num. Returnees",
      na.show = FALSE,
      na.label = ""
    ) )+
  tm_text("NAME_1", size = 0.6, remove.overlap = TRUE) +
  tm_layout(
    inner.margins = c(0.3, 0.1, 0.1, 0.3),
    legend.position = c("right", "top"),  # move legend inside
    legend.bg.color = "transparent",                   # transparent background
    legend.frame = FALSE,                   # remove box around legend
    frame = FALSE ,
    bg.color = "transparent"  # remove outer frame
  )






########################################################################
####Appendix: Replicable Code in rgeos and tmap 3.3 (both deprecated)###
########################################################################
library(rgeos)
###uninstall tmap if version 4.1, download and install 3.3####

return <- readOGR(dsn="UKR_adm", layer="UKR_adm1_2022_final2",GDAL1_integer64=TRUE)
pre2014 <- readOGR(dsn="UKR_adm", layer="UKR_adm1_2013",GDAL1_integer64=TRUE)
arrival <- readOGR(dsn="UKR_adm", layer="UKR_POL_2022",GDAL1_integer64=TRUE)
departure <- readOGR(dsn="UKR_adm", layer="UKR_adm1",GDAL1_integer64=TRUE)
bg <- readOGR(dsn="UKR_adm", layer="UKR_adm0",GDAL1_integer64=TRUE)

tm_polygons(col = varname , # add variable(s)
            style = "cat", # set break pattern to object LQ.cuts
            palette = colorplaette,  # set color fill to blue refreshing
            border.col = "grey40", # color the borders white
            border.alpha = 0.5, # set border transparency
            title = ltitle, # title for legend
            colorNA = "white", # color of missing data
            textNA = "Missing or not eligible to gentrify",
            labels = c("Decrease in residential housing price", 
                       "Increase in residential housing price")) +
  
  departure@data[["MIGRATED"]]
departure@data$MIGRATED[departure@data$MIGRATED ==0] <-NA
tm_shape(pre2014)+tm_fill(palette="Greys")+tm_borders(col="Dark Grey")+tm_shape(departure)+tm_fill(col = 'MIGRATED',title = "Num. Departures", palette=get_brewer_pal("Blues", n = 7, contrast = c(0.2, 0.9)),colorNA = "white",textNA = "None")+tm_text("NAME_1", remove.overlap = TRUE,size= 0.6)+tm_layout(legend.outside=TRUE,legend.outside.size = .15,frame = FALSE)+tm_borders(col="Black")
arrival@data$MIGRATED[arrival@data$MIGRATED ==0] <-NA
tm_shape(pre2014)+tm_fill(palette="Greys")+tm_borders(col="Dark Grey")+tm_shape(arrival)+tm_fill(col = 'MIGRATED',title = "Num. Arrivals", palette=get_brewer_pal("Greens", n = 7, contrast = c(0.2, 0.9)),colorNA = "white",textNA = "None")+tm_text("NAME_1", remove.overlap = TRUE,size= 0.6)+tm_layout(legend.outside=TRUE,legend.outside.size = .15,frame = FALSE)+tm_borders(col="Black")
return@data$MIGRATED[return@data$MIGRATED ==0] <-NA
tm_shape(pre2014)+tm_fill(palette="Greys")+tm_borders(col="Dark Grey")+tm_shape(return)+tm_fill(col = 'MIGRATED',title = "Num. Returnees", palette=get_brewer_pal("OrRd", n = 7, contrast = c(0.2, 0.9)),colorNA = "white",textNA = "None")+tm_text("NAME_1", remove.overlap = TRUE,size= 0.6)+tm_layout(legend.outside=TRUE,legend.outside.size = .15,frame = FALSE)+tm_borders(col="Black")



