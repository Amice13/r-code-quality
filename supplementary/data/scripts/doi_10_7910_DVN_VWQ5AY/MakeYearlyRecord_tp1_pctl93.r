#!/usr/bin/env Rscript

### Script for counting number of meteorological events over threshold
### Written for Climate, Disasters, and Human Responses project
### Aiden Jönsson, 30 September 2019

library(raster)
library(GADMTools)
library(ncdf4)
library(ncdf4.helpers)
library(sf)
library(lubridate)

## Write a column maxima function
#colMax <- function(data) sapply(data, max, na.rm = TRUE)

## Read in GADM data of country shapes
global_precip <- st_read("GADM/gadm36_0.shp")

## Make a standardized time array to find indexes in the meteorological data
time <- seq(as.Date("2006-01-01"), by="day", length.out = 4383)

## Read in meteorological data as a raster brick
metdata <- brick("/proj/bolinc/users/x_aidjo/Disasters/tp1_modernpctl.nc",varname="pctl")

## Specify the extents of the meteorological data
extent(metdata) <- c(-180.375,180.375,-90.375,90.375)

## Start and end dates for the first year
start <- as.Date("2006-01-01")
end <- as.Date("2006-12-31")

## Make column names to save to and initialize in the data frame
years <- years <- as.character(2006:2017)
global_precip[,years] <- 0

## Counter for the column name
t = 1

## Begin looping for each year
while (end < "2018-01-01") {
  
  ## Take index numbers of start and end dates
  index1 = which(time==start)
  index2 = which(time==end)
  
  ## Choose only meteorological data for that year
  now_metdata <- metdata[[index1:index2]]
  
  ##Loop through the countries in the data frame
  for (state in 1:256) {
    
    ## Get shape of current country
    place <- as(global_precip$geometry[state],"Spatial")
    
    ## Set coordinate format to same as in meteorological data
    place <- spTransform(place,crs(now_metdata))
    
    ## Extract the country's meteorological data
    extr = as.data.frame(raster::extract(now_metdata,place))
    #extr <- colMax(extr)
    
    ## Save the amount of events that exceed the threshold for the year
    global_precip[state,years[t]] <- sum(extr>93.32)/nrow(extr)
    
  }
  
  ## Update start and end dates and counter
  start <- start %m+% years(1)
  end <- end %m+% years(1)
  t <- t+1
  
}

## Save the data frame
save(global_precip,file="CDHR2019_dailyprecip_pctl93.rda")
