#########################################################################
####REPRODUCING 50KM BUFFER RESULTS USING SELF-MADE BUFFER SHAPEFILES####
#########################################################################

library(foreign)
library(readstata13)
library(list)
library(tidyverse)

library(ggthemes) #themes for plotting
library(sandwich) #robust se estimator
library(lmtest) #lm functions
library(ordinal) #ologit funcitons
library(memisc) #import stata data
library(ggmap) #make nice plots with ggplot mapping
library(ggplot2) #nice plots
library(Hmisc) #multipurpose package
library(RColorBrewer) #make nice colors
library(grid) #plotting package
library(MASS) #multipurpose package
library(gplots) #nice plotting
library(lfe) #run linear fixed effects models
library(stargazer) #output nice tables
library(lubridate) #deal with dates
library(zoo) #deal with dates
library(reshape2) #powerful reshape package
library(sp) #general spatial class package
library(gstat) #geostat package
library(spacetime) #store data in proper spatial/temporal class
library(rgeos)
library(maptools) #plot and modify shapefiles
library(rgeos) #for use with maptools for shapefiles
library(rgdal) #for reading in shapefiles
library(RSAGA) #needed for spatial downscaling
library(RCurl) #also needed for spatial downscaling
library(dplyr) #for data frame manipulation functions
library(data.table) #for data.table functions
library(foreign) #for reading in .dta files
library(parallel) #for parallel processing functions
library(foreach) #for plyr parallel
library(caTools) #functions for fast running mean and sd
library(ncdf4) #tools for netCDF packages
library(stringr) #string tools
library(kfigr) #figure referencing for markdown
library(knitr) #knitting
library(pander) #pandering
library(geosphere) #distance tools
library(Rcpp) #C++ tools
library(RcppArmadillo) #C++ tools
library(viridis) # color map
library(multiwayvcov) # multiway clustered standard errors for lm
library(gtable) # for switching facet labels in ggplot
library(matrixStats) # for rowMins function
library(readstata13) # read in newer stata files
library(ggExtra) # for marginal histograms
library(gridExtra) # for multiplots
library(sf)


setwd("C:/Users/kash8/OneDrive/Documents/GIS/Koos and Neupert-Wentz JCR Replication/kash2022_rap_forthcoming_replication")
#setwd("~/kash2022_rap_forthcoming_replication")


acled<-readOGR(dsn="Shapefiles", layer="acled_marshall_spatial_join")
ged<-readOGR(dsn="Shapefiles", layer="ged_marshall_spatial_join")
murdock<-readOGR(dsn="Shapefiles", layer="borders_tribes805")
buffer_knw<-readOGR(dsn="Shapefiles", layer="buffer50")
acled@proj4string

proj4string(buffer_knw) <- CRS("+proj=longlat +datum=WGS84 +no_def")

plot(buffer_knw)
plot(murdock)


########REPLICATION######

match_bufferPoints <- over(acled, buffer_knw) #Assigns a signle buffer to points

match_bufferPoints <- na.omit(match_bufferPoints$NAME)

match_bufferPoints2 <- over(ged, buffer_knw) #Assigns a signle buffer to points

match_bufferPoints2 <- na.omit(match_bufferPoints2$NAME)

######RECREATION OF SHAPEFILE#####

dissolve<- gUnaryUnion(buffer_knw)
p.df <- data.frame( ID=1:length(p))
dissolve <- SpatialPolygonsDataFrame(dissolve, p.df, match.ID = F) 

writeOGR(dissolve,"buffer_union.shp","Shapefiles",driver = "ESRI Shapefile")

plot(dissolve)

#####Use Crop Command in ArcGIS to subset Murdock map to only polygon areas within dissolved buffers

buffer_new<-readOGR(dsn="Shapefiles", layer="buffer_inside_only_25km")

plot(buffer_new)

match_bufferPoints3 <- over(acled, buffer_new) #Assigns a signle buffer to points

match_bufferPoint3 <- na.omit(match_bufferPoints3$NAME)

match_bufferPoints4 <- over(ged, buffer_new) #Assigns a signle buffer to points

match_bufferPoints4 <- na.omit(match_bufferPoints4$NAME)

####Counts match what KNW report in article####

######FIGURE A1: Fulani Groups and Violence in ACLED Data######

acled_data<-read.csv("acled_main.csv")
acled_data_fulani <- subset(acled_data, fulani==1)
coordinates(acled_data_fulani)<- c("longitude","latitude")

murdock_fulani<-subset(murdock, Fulani==1)


plot(murdock)
plot(murdock_fulani,col="red",add=TRUE)
plot(acled_data_fulani, col="blue",pch=10,add=TRUE)
