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

## Read in GADM data of country shapes (the GADM global shapefile folder is needed)
## Name of data frame: ensure that it is descriptive (e.g. global_wind if windspeed)
NAMEOFDATAFRAME <- st_read("GADM/gadm36_0.shp") # read in shapes of countries

## Make a standardized time array to find indexes in the meteorological data
## The period of focus for the study is from 2006.01.01 to 2017.12.31 (4383 days)
time <- seq(as.Date("2006-01-01"), by="day", length.out = 4383)

## Read in the meteorological data as a raster brick
metfile <- 'NAME_OF_NETCDFFILE.nc'
metvar <- 'NAME_OF_VAR' # e.g. 'pctl' for percentile ranking
metdata <- brick(metfile,varname=metvar) # load meteorological data

## Specify the extents of the meteorological data
extent(metdata) <- c(-180.375,180.375,-90.375,90.375) #specify extent

## Start and end dates for the first year
start <- as.Date("2006-01-01")
end <- as.Date("2006-12-31")

## Make column names to save to and initialize in the data frame with zeros
years <- as.character(2006:2017)
NAMEOFDATAFRAME[,years] <- 0

## Counter for the column name
t = 1

## Choose a threshold for
threshold <- 83.14 # e.g. 83.14 for SPI 1.0, 93.32 for SPI 1.5, 97.72 for SPI 2.0

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
    place <- as(NAMEOFDATAFRAME$geometry[state],"Spatial")
    
    ## Set coordinate format to same as in meteorological data
    place <- spTransform(place,crs(now_metdata))
    
    ## Extract the country's meteorological data
    extr = as.data.frame(raster::extract(now_metdata,place))
    #extr <- colMax(extr) # use if you would like to calculate a maximum
    
    ## Save the amount of events that exceed the threshold for the year
    NAMEOFDATAFRAME[state,years[t]] <- sum(extr>threshold)/nrow(extr)
    
  }
  
  ## Update start and end dates and counter
  start <- start %m+% years(1)
  end <- end %m+% years(1)
  t <- t+1
  
}

## Save the data frame
outfile <- 'OUTFILE.rda' # Name the file descriptively
save(NAMEOFDATAFRAME,file=outfile)
