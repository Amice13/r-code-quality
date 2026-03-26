# conduct PM analysis for kilns around Dhaka

##################################################################################################
##############################          Set Up            ########################################
##################################################################################################

rm(list=ls())

library(raster) # to read in raster data
library(sf) # main spatial analysis package
library(rgdal) # necessary underlying spatial package
library(rgeos) # necessary underlying spatial package
library(geosphere) # for bearing
library(sp)
library(units)
library(grid)
library(patchwork) # for combining plots
library(imputeTS) # for time series imputation
library(lubridate)
library(tidyverse)


# Set root folder
if (Sys.info()[["user"]] == "ninabrooks"){
    rootFolder <- "~/Dropbox/1Stanford/Research/Brick-Kilns/geovisual-search-spatial-analysis/spatial-analysis/"
    lib.loc <- .libPaths()
}

# set directories
scripts <- paste0(rootFolder, "scripts/")
input <- paste0(rootFolder, "input/")
spatial_input <- paste0(input, "spatial-data/")
output <- paste0(rootFolder, "output/")
BD_shp <- paste0(spatial_input, "shapefiles/")


# load function to extract ERA5 (Copernicus) data
source(paste0(scripts, "functions/extractCopernicus.R"))

# create data ------------------------------------------------------------------
# load kiln coordinates from model
kilns <- read_csv(paste0(input, 
                         "model-results/all_pos_without_shape_coords_results.csv")) %>%
    sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
    sf::st_transform(crs = 32646)  # reproject kilns

# create dataframe for the 
embassy <-  st_as_sf(data.frame("lon" = 90.42461, "lat" = 23.79637), 
                     coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(32646)

embassyLatLon <- st_coordinates(st_transform(embassy, crs = 4326))
embassyLatLon <- as.data.frame(cbind(embassyLatLon[,1], embassyLatLon[,2]))
names(embassyLatLon) <- c("lon", "lat")

kilnLatLon <- st_coordinates(st_transform(kilns, crs = 4326))
kilnLatLon <- as.data.frame(cbind(kilnLatLon[,1], kilnLatLon[,2]))
names(kilnLatLon) <- c("lon", "lat")


#  create df of daily weather, rain, temp & dewpoint around us embassy in dhaka
# list all raw data from ERA5
file_list <- list.files(paste0(spatial_input, "era5/"), 
                        pattern = "\\.grib$")
# apply extractCopernicus
weather_df <- lapply(file_list, extractCOPERNICUS) %>%
    bind_rows()

file <- file_list[[1]]
weather_df <- weather_df %>%
    mutate(day = str_remove(day, "[.]"),
           day = dmy(day))


head(weather_df)

# bearing between us embassy and each kiln and store in long format
bearings <- lapply(1:nrow(kilnLatLon), 
                   function(x) bearing(embassyLatLon, kilnLatLon[x,]))

colNames <- paste0("V", 1:nrow(kilns))
bearingsDF <- do.call(rbind.data.frame, bearings) %>%
    mutate_all(function(x) ifelse(x<0, mod(x + 360,  360), x))

colnames(bearingsDF) <- "bearing"

# define breaks for wind direction
dir_breaks8 <- c(0, 360/16, (1/16 + (1:7 / 8)) * 360, 360)
dir_breaks16 <- c(0, 360/32, (1/32 + (1:15 / 16)) * 360, 360)

# define labels for wind direction
dir_labs8 <- c(
    "N", "NE", 
    "E",  "SE", 
    "S",  "SW", 
    "W",  "NW", 
    "N"
)

dir_labs16 <- c(
    "N", "NNE", "NE", "EENE",
    "E", "ESE", "SE", "SSE",
    "S", "SSW", "SW", "WSW",
    "W", "WNW", "NW", "NNW",
    "N"
)


dhakaKilnDF <- bearingsDF %>%
    mutate(bearing8 = cut(bearing,
                          breaks = dir_breaks8,
                          labels = dir_labs8,
                          right = FALSE,
                          include.lowest = TRUE),
           bearing16 = cut(bearing,
                           breaks = dir_breaks16,
                           labels = dir_labs16,
                           right = FALSE,
                           include.lowest = TRUE),
           kiln_id = 1:n())
head(dhakaKilnDF)

# calculate distance to each kiln from embassy
distance2kiln <- st_distance(embassy, kilns)
dhakaKilnDF$distance <- unclass(t(distance2kiln))/1000
head(dhakaKilnDF)


# bearing = kiln position relative to the point, dir = direction wind is blowing from
# when wind blows from the same direction as the bearing the kiln is upwind of the point (causing pollution)
# ie. bearing = N and dir = N, then wind blows from the North --> causing pollution from kiln to hit the point

# using 45-degree sectors 
windKilnDF <- inner_join(weather_df, dhakaKilnDF, 
                         by = c("dir8" = "bearing8")) %>% # inner join matches each day to all kilns by wind dir
    arrange(day)

# summarise total obs by day = # of kilns -- UPWIND
upwindKilnsByDay <- windKilnDF %>%
    group_by(day) %>%
    tally() %>%
    rename(allUpwindKilns = n)

upwindKilnsByDay100km <- windKilnDF %>%
    filter(distance <=100) %>%
    group_by(day) %>%
    tally() %>%
    rename(upwindKilns100km = n)

upwindKilnsByDay75km <- windKilnDF %>%
    filter(distance <=75) %>%
    group_by(day) %>%
    tally() %>%
    rename(upwindKilns75km = n)

upwindKilnsByDay50km <- windKilnDF %>%
    filter(distance <=50) %>%
    group_by(day) %>%
    tally() %>%
    rename(upwindKilns50km = n)

upwindKilnsByDay40km <- windKilnDF %>%
    filter(distance <=40) %>%
    group_by(day) %>%
    tally() %>%
    rename(upwindKilns40km = n)

upwindKilnsByDay30km <- windKilnDF %>%
    filter(distance <=30) %>%
    group_by(day) %>%
    tally() %>%
    rename(upwindKilns30km = n)

upwindKilnsByDay25km <- windKilnDF %>%
    filter(distance <=25) %>%
    group_by(day) %>%
    tally() %>%
    rename(upwindKilns25km = n)

upwindKilnsByDay20km <- windKilnDF %>%
    filter(distance <=20) %>%
    group_by(day) %>%
    tally() %>%
    rename(upwindKilns20km = n)

upwindKilnsByDay15km <- windKilnDF %>%
    filter(distance <=15) %>%
    group_by(day) %>%
    tally() %>%
    rename(upwindKilns15km = n)

upwindKilnsByDay12km <- windKilnDF %>%
    filter(distance <=12) %>%
    group_by(day) %>%
    tally() %>%
    rename(upwindKilns12km = n)

upwindKilnsByDay10km <- windKilnDF %>%
    filter(distance <=10) %>%
    group_by(day) %>%
    tally() %>%
    rename(upwindKilns10km = n)


dhakaEmbassyDF <- left_join(weather_df, upwindKilnsByDay, by = "day") %>%
    left_join(upwindKilnsByDay100km, by = "day") %>%
    left_join(upwindKilnsByDay75km, by = "day") %>%
    left_join(upwindKilnsByDay50km, by = "day") %>%
    left_join(upwindKilnsByDay40km, by = "day") %>%
    left_join(upwindKilnsByDay30km, by = "day") %>%
    left_join(upwindKilnsByDay25km, by = "day") %>%
    left_join(upwindKilnsByDay20km, by = "day") %>%
    left_join(upwindKilnsByDay15km, by = "day") %>%
    left_join(upwindKilnsByDay12km, by = "day") %>%
    left_join(upwindKilnsByDay10km, by = "day") %>%
    mutate_at(vars(allUpwindKilns:upwindKilns10km), replace_na, 0) # replace missing as 0, sice there are no  upwind kilns on that day

# using 30 degree sectors 
windKilnDF30deg <- inner_join(weather_df, dhakaKilnDF, by = c("dir16" = "bearing16")) %>% # inner join matches each day to all kilns by wind dir
    arrange(day)

# summarise total obs by day = # of kilns -- UPWIND
upwindKilnsByDay30deg <- windKilnDF30deg %>%
    group_by(day) %>%
    tally() %>%
    rename(allUpwindKilns = n)

upwindKilnsByDay100km30deg <- windKilnDF30deg %>%
    filter(distance <=100) %>%
    group_by(day) %>%
    tally() %>%
    rename(upwindKilns100km = n)

upwindKilnsByDay75km30deg <- windKilnDF30deg %>%
    filter(distance <=75) %>%
    group_by(day) %>%
    tally() %>%
    rename(upwindKilns75km = n)

upwindKilnsByDay50km30deg <- windKilnDF30deg %>%
    filter(distance <=50) %>%
    group_by(day) %>%
    tally() %>%
    rename(upwindKilns50km = n)

upwindKilnsByDay40km30deg <- windKilnDF30deg %>%
    filter(distance <=40) %>%
    group_by(day) %>%
    tally() %>%
    rename(upwindKilns40km = n)

upwindKilnsByDay30km30deg <- windKilnDF30deg %>%
    filter(distance <=30) %>%
    group_by(day) %>%
    tally() %>%
    rename(upwindKilns30km = n)

upwindKilnsByDay25km30deg <- windKilnDF30deg %>%
    filter(distance <=25) %>%
    group_by(day) %>%
    tally() %>%
    rename(upwindKilns25km = n)

upwindKilnsByDay20km30deg <- windKilnDF30deg %>%
    filter(distance <=20) %>%
    group_by(day) %>%
    tally() %>%
    rename(upwindKilns20km = n)

upwindKilnsByDay15km30deg <- windKilnDF30deg %>%
    filter(distance <=15) %>%
    group_by(day) %>%
    tally() %>%
    rename(upwindKilns15km = n)

upwindKilnsByDay12km30deg <- windKilnDF30deg %>%
    filter(distance <=12) %>%
    group_by(day) %>%
    tally() %>%
    rename(upwindKilns12km = n)

upwindKilnsByDay10km30deg <- windKilnDF30deg %>%
    filter(distance <=10) %>%
    group_by(day) %>%
    tally() %>%
    rename(upwindKilns10km = n)


dhakaEmbassyDF_30deg <- left_join(weather_df, upwindKilnsByDay30deg, by = "day") %>%
    left_join(upwindKilnsByDay100km30deg, by = "day") %>%
    left_join(upwindKilnsByDay75km30deg, by = "day") %>%
    left_join(upwindKilnsByDay50km30deg, by = "day") %>%
    left_join(upwindKilnsByDay40km30deg, by = "day") %>%
    left_join(upwindKilnsByDay30km30deg, by = "day") %>%
    left_join(upwindKilnsByDay25km30deg, by = "day") %>%
    left_join(upwindKilnsByDay20km30deg, by = "day") %>%
    left_join(upwindKilnsByDay15km30deg, by = "day") %>%
    left_join(upwindKilnsByDay12km30deg, by = "day") %>%
    left_join(upwindKilnsByDay10km30deg, by = "day") %>%
    mutate_at(vars(allUpwindKilns:upwindKilns10km), replace_na, 0) # replace missing as 0, sice there are no  upwind kilns on that day


# merge in PM data
pm <- read_csv(paste0(spatial_input, "pm/openAQ_Dhaka.csv")) %>%
    filter(sourcename == "StateAir_Dhaka") %>%
    separate(col = date, sep = ",", into = c("dateUTC", "dateLocal")) %>%
    mutate(dateUTC = str_remove(dateUTC, "\\{utc="),
           dateUTC = ymd_hms(dateUTC, tz = "UTC"),
           dateLocal = str_remove(dateLocal, "local="),
           dateLocal = str_remove(dateLocal, "\\{$"),
           dateLocal = ymd_hms(dateLocal, tz = "UTC")) %>%
    separate(col = coordinates, sep = ",", into = c("latitude", "longitude")) %>%
    mutate(latitude = str_remove(latitude, "\\{latitude="),
           latitude = as.numeric(latitude),
           longitude = str_remove(longitude, "longitude="),
           longitude = str_remove(longitude, "\\}"),
           longitude = as.numeric(longitude)) %>%
    arrange(dateLocal) %>%
    mutate(day = floor_date(dateLocal, "day"),
           value = na_if(value, -999),
           value = na_if(value, -15),
           value = na_if(value, 985),
           pmImp = value, 
           pmImp = na_kalman(pmImp)) %>%
    filter(day >= mdy("1-1-2017") & day <= mdy("12-31-2019")) %>%
    group_by(day) %>%
    summarise(pm = mean(value, na.rm = T),
              pmImp = mean(pmImp, na.rm = T))  %>%
    mutate(day = as.Date(day))


# merge PM into kiln + meteorological data
dhakaEmbassyDF <- left_join(dhakaEmbassyDF, pm, by = "day") %>%
    dplyr::select(day, pm, ws, wd, dir8, dir16, temp,
                  rain, dewpoint, allUpwindKilns, starts_with("upwind")) 

dhakaEmbassyDF_30deg <- left_join(dhakaEmbassyDF_30deg, pm, by = "day") %>%
    dplyr::select(day, pm, ws, wd, dir8, dir16, temp,
                  rain, dewpoint, allUpwindKilns, starts_with("upwind")) 

dhakaEmbassyDF <- dhakaEmbassyDF %>%
    mutate(year  = year(day),
           year = factor(year),
           month = month(day, label = T),
           season = month %in% c("Nov", "Dec", "Jan", "Feb", "Mar"),
           season = factor(season,
                           levels = c(T,F),
                           labels = c("Kilns On", "Kilns Off")),
           season = relevel(season, ref = "Kilns Off"),
           temp = temp -273.15, # convert to C
           dewpoint = dewpoint -273.15, # convert to C
           rain = rain*1000, # convert to mm
           kilnBins15km = ifelse(upwindKilns15km == 0, "0",
                                 ifelse(upwindKilns15km >0 & upwindKilns15km <=10, "1-10",
                                        ifelse(upwindKilns15km >10 & upwindKilns15km<=29, "11-29", "30+"))),
           kilnBins15km = factor(kilnBins15km, ordered = TRUE),
           kilnBins20km = ifelse(upwindKilns20km == 1, "1",
                                 ifelse(upwindKilns20km >1 & upwindKilns20km <=20, "2-20",
                                        ifelse(upwindKilns20km >20 & upwindKilns20km<=60, "21-60", "61+"))),
           kilnBins20km = factor(kilnBins20km, ordered = TRUE),
           kilnBins25km = ifelse(upwindKilns25km <20, "20",
                                 ifelse(upwindKilns25km >20 & upwindKilns25km <=50, "20-50",
                                        ifelse(upwindKilns25km >50 & upwindKilns25km<=80, "51-80", "81+"))),
           kilnBins25km = factor(kilnBins25km, ordered = TRUE),
           kilnBins30km = ifelse(upwindKilns30km <25, "25",
                                 ifelse(upwindKilns30km >25 & upwindKilns30km <=80, "25-80",
                                        ifelse(upwindKilns30km >80 & upwindKilns30km<=110, "81-110", "111+"))),
           kilnBins30km = factor(kilnBins30km, ordered = TRUE))
save(dhakaEmbassyDF, file = paste0(output, "createDhakaPMData/dhakaEmbassyDF.Rdata"))

dhakaEmbassyDF_30deg <-  dhakaEmbassyDF_30deg %>%
    mutate(year  = year(day),
           year = factor(year),
           month = month(day, label = T),
           season = month %in% c("Nov", "Dec", "Jan", "Feb", "Mar"),
           season = factor(season,
                           levels = c(T,F),
                           labels = c("Kilns On", "Kilns Off")),
           season = relevel(season, ref = "Kilns Off"),
           temp = temp -273.15, # convert to C
           dewpoint = dewpoint -273.15, # convert to C
           rain = rain*1000, # convert to mm
           kilnBins15km = ifelse(upwindKilns15km == 0, "0",
                                 ifelse(upwindKilns15km >0 & upwindKilns15km <=3, "1-3",
                                        ifelse(upwindKilns15km >3 & upwindKilns15km<=15, "4-15", "16+"))),
           kilnBins15km = factor(kilnBins15km, ordered = TRUE),
           kilnBins20km = ifelse(upwindKilns20km == 0, "0",
                                 ifelse(upwindKilns20km >0 & upwindKilns20km <=15, "1-15",
                                        ifelse(upwindKilns20km >15 & upwindKilns20km<=25, "16-25", "26+"))),
           kilnBins20km = factor(kilnBins20km, ordered = TRUE),
           kilnBins25km = ifelse(upwindKilns25km <20, "<20",
                                 ifelse(upwindKilns25km >20 & upwindKilns25km <=50, "20-50",
                                        ifelse(upwindKilns25km >50 & upwindKilns25km<=80, "51-80", "81+"))),
           kilnBins25km = factor(kilnBins25km, ordered = TRUE),
           kilnBins30km = ifelse(upwindKilns30km <25, "<25",
                                 ifelse(upwindKilns30km >25 & upwindKilns30km <=80, "25-80",
                                        ifelse(upwindKilns30km >80 & upwindKilns30km<=110, "81-110", "111+"))),
           kilnBins30km = factor(kilnBins30km, ordered = TRUE))

save(dhakaEmbassyDF_30deg, file = paste0(output, "createDhakaPMData/dhakaEmbassyDF_30deg.Rdata"))


dhakaEmbassyDF %>%
    group_by(season) %>%
    summarise(pm = mean(pm, na.rm = T))

