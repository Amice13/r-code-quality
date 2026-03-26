################################################################################
# Author: Nina Brooks
# Project name: Spatial Analysis of Brick Kilns 
# Date Created: February 3, 2020
# Purpose: Identify exposed populations and create population-specific 
#          dataframes with kiln exposure
# Input files: kiln locations, population raster files from world pop
################################################################################

################################################################################
##############################          Set Up            ######################
################################################################################

rm(list=ls())

library(raster) 
library(sf) 
library(rgdal) 
library(rgeos) 
library(tidyverse) 

# Set root folder
if (Sys.info()[["user"]] == "ninabrooks"){
    rootFolder <- "~/Dropbox/pnas-kiln/spatial-analysis/"
    lib.loc <- .libPaths()
}

# set directories
raw <- paste0(rootFolder, "data/raw/")
clean <- paste0(rootFolder, "data/clean/")
functions <- paste0(rootFolder, "scripts/functions/")
output <- paste0(rootFolder, "output/")

# load function
source(paste0(functions, "kilnPopDist.R"))

# load data --------------------------------------------------------------------
# coordinates from model
kilns <- read_csv(paste0(clean, "model_kilns.csv")) 
kilnCoords <- kilns[, c("lat", "long")]

kilnCoords <- kilns %>%
    dplyr::select(lat, long) %>%
    st_as_sf(coords = c( "long", "lat"), 
             crs = 4326) %>%
    st_transform(32646)

# total population data (units are # of people per pixel)
pop <- raster(paste0(raw, "bgd_ppp_2018.tif"))

popDF <- as.data.frame(pop, xy = T)
setDT(popDF)
popDF[, id := 1:nrow(popDF)]
popDF <- popDF[complete.cases(popDF),]
popDF <-  st_as_sf(popDF, coords = c("x", "y"),
                   crs = 4326) %>%
    st_transform(32646)
setnames(popDF, "bgd_ppp_2018", "pop")

# pregnancies
pregnancies <- raster(paste0(raw, "BGD_pregs_pp_v2_2015.tif"))

pregDF <- as.data.frame(pregnancies, xy = T)
setDT(pregDF)
pregDF[, id := 1:nrow(pregDF)]
pregDF <- pregDF[complete.cases(pregDF),]
pregDF <-  st_as_sf(pregDF, coords = c("x", "y"), 
                    crs = 4326) %>%
    st_transform(32646)
setnames(pregDF, "BGD_pregs_pp_v2_2015", "pop")

################################################################################
##############################  Apply Function to Pop Data  ####################
################################################################################

# total population
popKiln <- lapply(1:nrow(popDF), 
                  function(x) kilnPopDist(point = popDF[x,], 
                                          kilns = kilnCoords))



popKilnDF <- rbindlist(popKiln)
setDT(popKilnDF)
table(popKilnDF$kilnCount1km)
sum(popKilnDF$pop[popKilnDF$closestKiln <=1000])/sum(popKilnDF$pop)
saveRDS(popKilnDF, file = paste0(clean, "popKilnDF.Rds"))

# pregnancies
pregKiln <- lapply(1:nrow(pregDF_small_tr),
                   function(x) kilnPopDist(point = pregDF_small_tr[x,], 
                                           kilns = kilnCoords))


pregKiln <- rbindlist(pregKiln)
setDT(pregKiln)
table(pregKiln$kilnCount1km)
sum(pregKiln$pop[pregKiln$closestKiln <=1000])/sum(pregKiln$pop)
saveRDS(pregKilnDF, file = paste0(clean, "pregKilnDF.Rds"))

