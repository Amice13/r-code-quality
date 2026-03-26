#Code to Map InMAP Cells to Counties
#Author: Ernani F. Choma 



#Please change the working directory path on line 22 before running:
#Line 22 -- Working directory
#Also make sure to check following paths for Inputs and Outputs (i.e., Results):
#Inputs:
#Line 32 -- ISRM Marginal Values file (not provided with model/needs to be downloaded)
#Line 59 -- USCB County Boundaries Shapefile (not provided with model/needs to be downloaded)
#Line 78 and 88 -- USCB Block Group Shapefiles (not provided with model/needs to be downloaded)
#Line 100 -- USCB Block Group Population Data file (provided with model)
#Outputs:
#Line 259 -- Where to save RData file. This is the final output that is used in the Marginal Damages Model.

#Loading Packages
library(raster)
library(rgeos)

#Please change path to work directory
setwd("~/US_OnRoad_HealthImpacts/Marginal_Damages_Model/InMAP_to_County_Population_Mapping/")


print("Loading and creating InMAP Cells spatial file")

#Loading InMAP data. This dataset is available at:
#https://doi.org/10.5281/zenodo.3590127 and was accessed on March 03, 2021
#The "Cells" variable is loading one of the files in the marginal_values.zip file (after unzipping)

# Please change directory and run following line to open the marginal values file.
Cells <- read.csv("Inputs/InMAP/marginal_values.csv")

#Creating spatial file
aux.list <- lapply(1:nrow(Cells),function(i){
  t <- Cells[i,c("Location_W","Location_E","Location_S","Location_N")]
  t <- extent(as.numeric(t))
  as(t,"SpatialPolygons")
})
InMAP.Cells <- do.call(bind,aux.list)
InMAP.Cells$Index <- 1:length(InMAP.Cells)
InMAP.Cells$Pop <- Cells$Population
InMAP.Cells$Cell.ID <- Cells$cell_ID

#The projection of the InMAP Cells can be found in the file "InMAP_shape_prj.txt"
#This file is one files in the marginal_values.zip file referenced above
proj4string(InMAP.Cells) <- CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")

#Loading County Boundaries
#This is the 2019 County TIGER/LINE shapefiles provided by the US Census Bureau
#Available at:
#https://www2.census.gov/geo/tiger/TIGER2019/COUNTY/tl_2019_us_county.zip
#Accessed 03-05-21
print("")
print("")
print("Loading US Census Bureau files")

# Please change file path in the following line to open the USCB county boundaries shapefile.
Counties.Shp <- shapefile("Inputs/USCB/County_Boundaries/tl_2019_us_county.shp")

Counties <- spTransform(Counties.Shp,proj4string(InMAP.Cells))

#Loading BlockGroup Boundaries
#These are the Block Group TIGER/LINE shapefiles provided by the US Census Bureau
#Available at (56 .zip files in total one for each state/district/territory):
#https://www2.census.gov/geo/tiger/TIGER2019/BG/
#Accessed 03-05-21
state.code <- c("01","02","04","05","06","08","09",
            "10","11","12","13","15","16","17","18","19",
            "20","21","22","23","24","25","26","27","28","29",
            "30","31","32","33","34","35","36","37","38","39",
            "40","41","42","44","45","46","47","48","49",
            "50","51","53","54","55","56",
            "60","66","69","72","78")
i<-1

# Please change file path in the following line to point to USCB Block Group boundaries shapefile.
auxfilepath <- paste0("Inputs/USCB/BlockGroup_Boundaries/tl_2019_",state.code[i],"_bg.shp")

print(paste0("Loading Block Group shapefile for state ",i," of ", length(state.code)))
US.BG.SHP <- shapefile(auxfilepath)

for (i in 2:length(state.code))
{       
  print(paste0("Loading Block Group shapefile for state ",i," of ", length(state.code)))
  
  # Please change file path in the following line to point to USCB Block Group boundaries shapefile.
  auxfilepath <- paste0("Inputs/USCB/BlockGroup_Boundaries/tl_2019_",state.code[i],"_bg.shp")
  newstate <- shapefile(auxfilepath)
  US.BG.SHP <- rbind(US.BG.SHP,newstate)
}
#Removing states and territories outside the Contiguous U.S.
US.BG.SHP <- subset(US.BG.SHP, !US.BG.SHP$STATEFP %in% c("02","15","60","66","69","72","78"))

print("")
print("")
print("Reading Census Block Groups Population Data")

# Please change file path in the following line to point to USCB population data file (provided with model).
US.BG.ACS.2019 <- read.csv("Inputs/USCB/BlockGroup_PopulationData/ACSDT5Y2019.B01003_data_with_overlays.csv", skip=1)

names(US.BG.ACS.2019) <- c("ID","NAME","POPULATION","MARGIN_OF_ERROR")
US.BG.ACS.2019 <- subset(US.BG.ACS.2019, select=c("ID","POPULATION"))
US.BG.ACS.2019$ID <- as.character(US.BG.ACS.2019$ID)
US.BG.ACS.2019$POPULATION <- as.numeric(US.BG.ACS.2019$POPULATION)
US.BG.ACS.2019$GEOID <- substr(US.BG.ACS.2019$ID,10,nchar(US.BG.ACS.2019$ID))
US.BG.ACS.2019$STATEFP <- substr(US.BG.ACS.2019$GEOID,1,2)
US.BG.ACS.2019$COUNTYFP <- substr(US.BG.ACS.2019$GEOID,3,5)
US.BG.ACS.2019$TRACTCE <- substr(US.BG.ACS.2019$GEOID,6,11)
US.BG.ACS.2019$BLKGRPCE <- substr(US.BG.ACS.2019$GEOID,12,12)
US.BG.ACS.2019 <- US.BG.ACS.2019[!US.BG.ACS.2019$STATEFP %in% c("02","15","72"),]
US.BG.ACS.2019 <- subset(US.BG.ACS.2019, select=c("GEOID","POPULATION"))

Census.BlockGroups.Shp <- merge(x=US.BG.SHP, y=US.BG.ACS.2019, by="GEOID", all=TRUE)
Census.BlockGroups <- spTransform(Census.BlockGroups.Shp,proj4string(InMAP.Cells))

#Mapping InMAP Cells to U.S. Counties
#This step might require a few hours of computing time
print("")
print("")
print("Mapping InMAP Cells to U.S. Counties")
Intersections <- matrix(rep(-1, length(Counties$GEOID)*length(InMAP.Cells$Cell.ID)), ncol=length(Counties$GEOID))
dim(Intersections)

for (j in 1:length(Counties$GEOID))
{
  print(paste0("Mapping County ",j, " of ", length(Counties$GEOID)))
  start.loop <- Sys.time()
  Single.County <- subset(Counties, GEOID==Counties$GEOID[j])
  Single.Overlay <- over(Single.County, InMAP.Cells, returnList = TRUE)
  Single.Overlay <- as.data.frame(Single.Overlay)
  
  if (length(Single.Overlay$Index)>0)
  {
    Single.Overlay$perc <- -1
    Single.Overlay$perc2 <- -1
    for (k in 1:length(Single.Overlay$Cell.ID))
    {
      Single.Cell <- subset(InMAP.Cells, Cell.ID==Single.Overlay$Cell.ID[k])
      Single.Overlay$perc[k] <- gArea(gIntersection(Single.County, Single.Cell))/gArea(Single.Cell)
      i <- Single.Overlay$Cell.ID[k]
      Intersections[i,j] <- Single.Overlay$perc[k]
    }
  }
  end.loop <- Sys.time()
  loop.time <- round(as.numeric(difftime(end.loop,start.loop,units="secs")),digits=2)
  print(paste0("Time elapsed: ",loop.time, " seconds"))
}

print("")
print("")
print("Filtering Partial County-Cell Intersections")
#Any non-intersections are assigned 0
Intersections[Intersections==-1] <- 0

#Some cells do not fully intersect with the U.S. (i.e. part on land and part on ocean)
#Therefore we allocate all the population in a cell to its intersecting portion
#Since there is no population outside the U.S. in the model
Total.Cell.Intersections <- rowSums(Intersections)
Total.Cell.Intersections[Total.Cell.Intersections==0] <- 1
Intersections <- Intersections/Total.Cell.Intersections

#In this first mapping, partial intersections were allocated by area
#That is, area of partial intersection divided by cell area
#Now we will use finer population density (Census Block Group level) 
#to allocate these partial intersections by population
#i.e. population of partial intersection divided by total cell population
#whereas total cell population is the sum of the population in all its partial intersections

#Selecting the partial intersections
#These will be mapped to Census Block groups to determine the population in these partial intersections
Partial.Intersections <- which((Intersections>0 & Intersections<1), arr.ind=TRUE)
Partial.Intersections <- as.matrix(Partial.Intersections)

Store.Population <- as.data.frame(Partial.Intersections)
Store.Population$Calc.Population <- 0

#Mapping Partial Intersections to U.S. Census Block Groups
#This step might require a few days of computing time
print("")
print("")
print("Mapping partial Cell-County Intersections to USCB Block Groups")
for (i in 1:dim(Store.Population)[1])
{
  print(paste0("Mapping Partial Intersection ",i, " of ", dim(Store.Population)[1]))
  start.loop <- Sys.time()
  Select.County <- as.numeric(Partial.Intersections[i,2])
  Single.County <- subset(Counties, GEOID==Counties$GEOID[Select.County])
  Select.Cell <- as.numeric(Partial.Intersections[i,1])
  Single.Cell <- subset(InMAP.Cells, Cell.ID==Select.Cell)
  Single.Intersection <- gIntersection(Single.County, Single.Cell)
  Single.Overlay <- over(Single.Intersection, Census.BlockGroups, returnList = TRUE)
  Single.Overlay <- as.data.frame(Single.Overlay)
  if (length(Single.Overlay$GEOID)>0)
  {
    Single.Overlay$Calc.Population <- 0
    Single.Overlay$Percent.Area <- 999
    for (k in 1:length(Single.Overlay$GEOID))
    {
      aux.area <- subset(Census.BlockGroups, GEOID==Single.Overlay$GEOID[k])
      Single.Overlay$Percent.Area[k] <- gArea(gIntersection(Single.Intersection, aux.area))/gArea(aux.area)
      Single.Overlay$Calc.Population[k] <- as.numeric(Single.Overlay$POPULATION[k]) * as.numeric(Single.Overlay$Percent.Area[k])
    }
    Store.Population$Calc.Population[i] <- sum(Single.Overlay$Calc.Population)
  }
  end.loop <- Sys.time()
  loop.time <- round(as.numeric(difftime(end.loop,start.loop,units="secs")),digits=2)
  print(paste0("Time elapsed: ",loop.time, " seconds"))
}

#Calculating total cell population
Cell.Population <- aggregate(Store.Population$Calc.Population, by=list(Store.Population$row), FUN=sum)
names(Cell.Population) <- c("row","Sum.Cell.Population")

#Allocating partial intersections by population
Store.Population <- merge(x=Store.Population, y=Cell.Population, by="row", all=TRUE)
Store.Population$Percent.Population <- Store.Population$Calc.Population/Store.Population$Sum.Cell.Population
Store.Population$Percent.Population[is.na(Store.Population$Percent.Population)] <- 0


#Creating new overlay where partial intersections are population-weighted
BlockGroup.Intersections <- Intersections

for (k in 1:length(Store.Population$row))
{
  i <- Store.Population$row[k]
  j <- Store.Population$col[k]
  if (BlockGroup.Intersections[i,j]>0 & BlockGroup.Intersections[i,j]<1)
  {
    BlockGroup.Intersections[i,j] <- Store.Population$Percent.Population[k]
  }
}

print("")
print("")
print("Calculating the percentage of each county's population contained in each InMAP cell")
#Calculating county population totals based on InMAP cell values
Calc.County.Population <- t(as.matrix(Cells$Population)) %*% as.matrix(BlockGroup.Intersections)

#Calculating the percentage of the population of each county in each cell
InMAP.Cells.by.County <- t(BlockGroup.Intersections * Cells$Population)/as.vector(Calc.County.Population)
InMAP.Cells.by.County[is.na(InMAP.Cells.by.County)] <- 0

print("")
print("")
print("Filtering contiguous U.S. and Reordering Counties")
STCOU.Data <- as.data.frame(as.numeric(as.character(Counties$GEOID)))
names(STCOU.Data) <- "STCOU"
STCOU.Data$LineNumber <- 1:length(STCOU.Data$STCOU)
STCOU.Data <- STCOU.Data[!STCOU.Data$STCOU %/% 1000 %in% c(2,15,60,66,69,72,78),]
STCOU.Order <- STCOU.Data$LineNumber[order(STCOU.Data$STCOU, decreasing=FALSE)]
InMAP.Cells.by.County <- InMAP.Cells.by.County[STCOU.Order,]

print("")
print("")
print("Saving final output file")

#Please change file path to where you would like to save the output
save(InMAP.Cells.by.County, file="Outputs/InMAPCounty_Overlay.RData")

print("")
print("")
print("Finished")