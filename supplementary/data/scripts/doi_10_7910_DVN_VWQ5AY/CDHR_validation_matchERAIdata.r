#!/usr/bin/env Rscript
library(raster)
library(ncdf4)
library(sf)
library(reticulate)

## Load disaster list and locations, save the data needed for matching meteorological data
load("GCEMDAT_Rosvold2019_v2_Heatwavelocations_2006_2018.rdata")
startdate <- heatwavelocations$startdate # the events' start dates
enddate <- heatwavelocations$enddate # the events' end dates
wkt <- heatwavelocations$geometry # spatial polygons of the region affected
rm(heatwavelocations)

## Load the netcdf data and crop
time <- seq(as.Date("2006-01-01"), by="day", length.out = 4383) # make a time string
metdata <- brick("tmax_modern.nc",varname="mx2t") # mx2t is the variable for maximum temperature at 2m in this file
extent(metdata) <- c(-180.375,180.375,-90.375,90.375) # set extent to match that of the NetCDF file's

## Now start the model of the loop step to retrieve the data:
t <- vector() # initiate an empty vector to save the event-level data from
area <- vector() # initiate an empty vector to save the location surface area for weighting
for (xx in 1:length(startdate)) {
  
  print(xx)
  place <- as(wkt[[xx]],"Spatial") # get the shapefile from the same row in the locations list and make it a spatial object
  proj4string(place) <- CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0') # set proj4string
  
  if (is.na(enddate[xx])==FALSE) { # this conditional allows a maximum within the start and end date to be found if an end date is available
    
    index1 = which(abs(time-startdate[xx]) == min(abs(time - startdate[xx]))) # find the index for the start date
    index2 = which(abs(time-enddate[xx]) == min(abs(time - enddate[xx]))) # find the index for this timestep
    tmp <- vector() # reset the temporary data vector
    
    for (yy in index1:index2) {
      
      now_metdata <- metdata[[which(abs(time-startdate[xx]) == min(abs(time - startdate[xx])))]] # pick out the frame of time for the month that we want the data from
      place <- spTransform(place,crs(now_metdata))
      myextr <- as.array(raster::extract(now_metdata, place)) # use for weight by areas
      tmp <- c(tmp, max(myextr[[1]]))
    }
    
    t[xx] <- max(tmp) # choose the maximum ERAI value within this event's time frame and location
    
  } else { # this conditional chooses the start date of the event if no end date is available
    
    place <- as(wkt[[xx]],"Spatial") # get the shapefile from the same row in the locations list and make it a spatial object
    proj4string(place) <- CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0') # set proj4string
    now_metdata <- metdata[[which(abs(time-startdate[xx]) == min(abs(time - startdate[xx])))]] # pick out the closest time step to the event
    place <- spTransform(place,crs(now_metdata))
    myextr <- as.array(raster::extract(now_metdata, place)) # use for weight by areas
    t[xx] <- max(myextr[[1]]) 
    
  }
  
  area[xx] <- st_area(place)
  print(t[xx])
  
}

## Save the data to a new object
load("GCEMDAT_Rosvold2019_v2_Heatwavelocations_2006_2018.rdata")
heatwavelocations$tmax <- t # append the meteorological data to the geocoded EMDAT data frame
heatwavelocations$area <- area # append the surface area of locations to the geocoded EMDAT data frame
save(heatwavelocations,file="GCEMDAT_Rosvold2019_v2_Heatwavelocations_2006_2018_metdata.rdata") # save a new copy of the data frame
py_save_object(heatwavelocations,"CDHR_Heatwavelocations_2006_2018_metdata.pkl",pickle="pickle") # save as a pickle to work with in Python