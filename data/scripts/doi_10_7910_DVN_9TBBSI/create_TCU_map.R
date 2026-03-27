### Script to collapse 1924 map to aggregated Weimar unit level (687 units)
### Generates new shapefile
################################################################################################
################################################################################################

################################
### Load crosswalk
################################

load("data_weimar/crosswalks/TCU-to-DR1924.RData")

################################
### Load shapefile
################################
library(maptools)
library(rgeos)

dr1924 <- readShapePoly("data_weimar/shapefiles/dr1924_GESIS.shp",proj4string=CRS("+proj=utm +ellps=WGS84 +datum=WGS84"))

length(dr1924) #1031 -- number of map units
nrow(mapGESISAggCW) #1076 -- number of GESIS units with valid 1924 units
# this means that there are duplicates of map unit-TCU relationships

# check if each map unit is assigned to just one aggregated unit - yes!
any(stack(by(mapGESISAggCW, mapGESISAggCW$mapID,function(x) length(unique(x[,"aggKRNR"]))>1))[,1])

# create vector of aggregated units for 1031 map units by which we will collapse the map
cw <- stack(by(mapGESISAggCW, mapGESISAggCW$mapID,function(x) unique(x[,"aggKRNR"])))
row.names(cw) <- as.character(as.vector(cw$ind))
ind.vec <- cw[as.character(as.vector(dr1924$ID)),"values"]

# create aggregated map
dr1924.agg <- unionSpatialPolygons(dr1924 ,ind.vec)
plot(dr1924.agg)
# output is SpatialPolygons object

################################
### create attribute table
################################
### load aggregated Weimar data
load("data_weimar/aggregated_data/aggWeimarData.RData")

att.table <- cbind(names(dr1924.agg),aggWeimarData[names(dr1924.agg),])
colnames(att.table)[1] <- "tcuID"
att.table$tcuID <- as.character(as.vector(att.table$tcuID))

dr1924.agg.DF <- SpatialPolygonsDataFrame(dr1924.agg, att.table)

# save new shapefile
dr1924.agg <- dr1924.agg.DF
library(rgdal)
writeOGR(dr1924.agg, "data_weimar/shapefiles/", "dr1924_TCU", driver="ESRI Shapefile")
save(dr1924.agg, file="data_weimar/shapefiles/dr1924_TCU.RData")