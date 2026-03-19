library(ggplot2)
library(data.table)
library(maptools)
library(plyr)
library(reshape)
library(grid)
library(rgeos)
library(rgdal)
library(gridExtra)

data <- read.csv("terr_control.csv", sep=",", stringsAsFactors=FALSE)

## clean lat and long 
data$LatLongMGRSGridLongitude <- gsub("\\°", "", data$LatLongMGRSGridLongitude)
data$LatLongMGRSGridLongitude <- gsub("\\|.*","", data$LatLongMGRSGridLongitude)
data$LatLongMGRSGridLatitude <- gsub("\\°", "", data$LatLongMGRSGridLatitude)
data$LatLongMGRSGridLatitude <- gsub("\\|.*","", data$LatLongMGRSGridLatitude)
data$longitude <- as.numeric(data$LatLongMGRSGridLongitude)
data$latitude <- as.numeric(data$LatLongMGRSGridLatitude)

## these are the two maps we want
dates <- c("2014-01-01","2015-01-01")
data <- data.table(data)
mapdata <- data[date %in% dates &  group != "Ceasefire", list(date, group, latitude, longitude, governorate)]
SYmap<-readOGR("SYR_adm1.shp", layer="SYR_adm1")


#######################################################################
##### Figure A4. Armed group presence in Syria - community level, January 2014 and 2015.
#######################################################################
map.list <- list()
for (i in 1:length(dates)) {
   map.name <- paste("Territorial Control: ", dates[i], sep="")
   map.list[[map.name]] <-
      ggplot(data=mapdata[date==dates[i],])+
      geom_polygon(data=SYmap, aes(x=long, y=lat, group=group),
                   fill="white", color="gray") +  
      geom_point(aes(x=longitude, y=latitude, color=group, shape=group), size=.05)+
      scale_color_manual("",values=c("red", "black", "purple", "blue"))+
      scale_shape_manual("",values=c(15,16,17,18))+
      ggtitle("") +
      guides(colour = guide_legend(override.aes = list(size=3))) +
               theme(plot.title=element_text(size=12),
                     legend.title=element_text(size=7),
                     legend.text=element_text(size=10),
                     axis.line=element_blank(),
                     axis.text.x=element_blank(),
                     axis.text.y=element_blank(),
                     axis.ticks=element_blank(),
                     axis.title.x=element_blank(),
                     axis.title.y=element_blank(),
                     panel.background = element_blank(),
                     panel.border=element_blank(),
                     panel.grid.major=element_blank(),
                     panel.grid.minor=element_blank(),
                     plot.background=element_blank(),
                     legend.position="bottom")
   outputname <- paste("FigureA4_controlmap-", dates[i], ".pdf", sep="")
   ggsave(plot=map.list[[map.name]], file=outputname, width=4, height=4, dpi = 150)
}






