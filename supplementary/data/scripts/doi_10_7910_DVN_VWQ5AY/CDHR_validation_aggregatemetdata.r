#!/usr/bin/env Rscript
library(units)
library(data.table)
library(plyr)

## Load the matched meteorological data and the events list data frames
load(file="GCEMDAT_Rosvold2019_v2_Heatwavelocations_2006_2018_metdata.rdata")
load(file="GCEMDAT_Rosvold2019_v2_Heatwavelist_2006_2018.rdata")
DT <- data.table(heatwavelocations) 
DT$area <- drop_units(DT$area) # if needed, drop units for math

## Do math to aggregate the list; this takes the weighted mean of the meteorological variable. Maxima/minima can be taken otherwise
tempdt <- DT[,list(wret = weighted.mean(tmax,area)),by=id] # aggregate the locations to the event level using a weighted mean
heatwavelist$tmax <- tempdt$wret # return the weighted mean meteorological variable to the events list data frame

## Save the data to a new object
heatwavelist$area <- as_units(DT$area,value="km^2") # give the area column units again, if needed
save(heatwavelist,file="GCEMDAT_Rosvold2019_v2_Heatwavelist_2006_2018.rdata")