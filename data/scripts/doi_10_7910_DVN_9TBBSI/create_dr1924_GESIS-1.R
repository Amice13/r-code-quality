####################################################################################
### Script to create shapefiles to match 1924 GESIS data from 1924 administrative division shapefiles from Max Planck Institute
####################################################################################

library(maptools)
library(rgeos)
library(rgdal)

dr1924 <- readShapePoly("data_weimar/shapefiles/dr1924.shp",proj4string=CRS("+proj=utm +ellps=WGS84 +datum=WGS84"))

#### Create new shapefile to match GESIS data ####
ID.combine <- dr1924$ID
# combine Lippe into 1 unit
ID.combine[dr1924$LAND==10000] <- 10000
# merge Riesa (Stadt) into Grossenhain
ID.combine[ID.combine==15058] <- 15036

dr1924.combine <- unionSpatialPolygons(dr1924 ,ID.combine)
# output is SpatialPolygons object

dr1924DF <- as.data.frame(dr1924)
dr1924DF$NAME <- as.character(as.vector(dr1924DF$NAME))
dr1924DF$STATUS <- as.character(as.vector(dr1924DF$STATUS))
dr1924DF$RB <- as.character(as.vector(dr1924DF$RB))
dr1924DF$TYPE <- as.character(as.vector(dr1924DF$TYPE))
dr1924DF.combine <- dr1924DF[-which(dr1924DF$LAND==10000),]
# replace 15058 record with LIPPE:
dr1924DF.combine[dr1924DF.combine$ID==15058,] <- c(0,0,10000,"LIPPE","A",10000,"/","/")
row.names(dr1924DF.combine) <- dr1924DF.combine$ID

dr1924.combine.SPDF <- SpatialPolygonsDataFrame(dr1924.combine, dr1924DF.combine)

# save new shapefile

writeOGR(dr1924.combine.SPDF, "data_weimar/shapefiles/", "dr1924_GESIS", driver="ESRI Shapefile")