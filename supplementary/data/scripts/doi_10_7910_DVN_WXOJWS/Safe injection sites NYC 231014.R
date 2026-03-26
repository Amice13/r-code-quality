#######################################################-
# Project: Overdose prevention sites and public safety
# Last modification: October 14, 2023
# Author code: David Mitre Becerril
# Objective: estimate the effect of safe injection sites on public safety
# Notes: 
# Treated sites addresses (Opened November 30th, 2021, OnPoint NYC)
# 104-106 E 126th St, New York, NY 10035
# 500 W 180th St, New York, NY 10033
# The safe injection sites are located in police precincts 25th and 34th
# We kept data from 2019 to 2022.
# There is almost no variation within precinct, so precinct-time fixed effects make little sense
# Crime, arrest, 911-calls, and summons data go up to 09/2022
# Police stops go up to 12/2021
# Files ending in 1: single hexagon sample
# Files ending in 2: three hexagons sample
# Violentall includes UCR violent crimes + simple assault
# High-drug arrests sample: remove 11435 id as the geocoded incidents seem to be 
# from NYPD Manhattan Central Booking.
# References:
# https://www.health.ny.gov/diseases/aids/consumers/prevention/needles_syringes/docs/sep_hours_sites.pdf
# https://github.com/Chrisjb/basemapR
#######################################################-

## Set up environment ####
library(sf)
library(data.table)
library(lubridate)
library(tidygeocoder)
library(basemapR)
library(readxl)
library(fixest)
library(estimatr)
library(augsynth) #install.packages("devtools"); devtools::install_github("ebenmichael/augsynth")
library(stargazer)
library(tidyverse)
options(scipen=999) #to avoid scientific notation when exporting latex tables
sf_use_s2(FALSE) #https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data



################################################################################-
## Data processing: Identify gps coordinates of treated/control sites ##########
################################################################################-
#### 1. Get coordinates injection sites #####-
dfgps <- read.csv("../Data/Injection site addresses.csv") %>%
  geocode(address = address, method = "census")
#fwrite(dfgps, "../Data/Injection site addresses gps.csv")
#dfgps <- fread("../Data/Injection site addresses gps.csv")
rm(dfgps); gc()





###############################################################################-
## Data processing: Create hexagonal grid (all city) ##########################
###############################################################################-
#### 1. Create a hexagonal grid #####-
#Cell size measures the length of a square, not of an hexagon
#Length: 187 meters (175 = 320/1.71); 480 feet (613 = 320/1.71*3.28)
#https://data.cityofnewyork.us/Public-Safety/Police-Precincts/78dh-3ptz
sf <- read_sf("../Data/NYC_Police_Precincts/NYC_Police_Precincts.shp") %>% 
  st_transform(32618) %>%
  rename_all(tolower)
sfgrid <- st_make_grid(sf, cellsize = 320, square = FALSE)
#plot(sfgrid); plot(sf %>% select(precinct), add = TRUE)
sf <- sf %>% st_transform(4326) 
sfgrid <- sfgrid %>% 
  st_sf() %>% 
  st_transform(4326) %>% 
  mutate(id = 1:length(sfgrid))
sfgrid1 <- st_centroid(sfgrid) %>% #ignore warning
  st_join(sf %>% select(precinct)) %>%
  st_set_geometry(NULL) %>% 
  as.data.frame()
sfgrid <- sfgrid %>%
  left_join(sfgrid1, by = "id") %>%
  filter(complete.cases(precinct) | id %in% 8275)
#st_write(sfgrid, "../Data/Grid/Hexagonal grid.shp", append = FALSE)
#sfgrid <- read_sf("../Data/Grid/Hexagonal grid.shp") 
#plot(sfgrid %>% select(precinct)); plot(sf %>% select(precinct), add = TRUE)
rm(sfgrid, sfgrid1, sf); gc()





##############################################################################-
## Data processing: Create hexagonal grid (single hexagons) ##################
##############################################################################-
#### 1. Create an hexagonal grid using #####-
#Cell size measures the length of a square, not of an hexagon
#Length: 187 meters (175 = 320/1.71); 480 feet (613 = 320/1.71*3.28)
dfgps <- fread("../Data/Injection site addresses gps.csv") %>%
  st_as_sf(coords = c("long","lat"), crs = 4326) %>%
  st_transform(32618)
for(X in 1:nrow(dfgps)){
  #Create a buffer and then the hexagonal grid
  sfcircle <- st_buffer(dfgps[X,], dist = 700)
  sftemp <- sfcircle %>%
    st_make_grid(cellsize = 320, square = FALSE, 
                 offset = c(st_bbox(dfgps[X,])["xmin"], st_bbox(dfgps[X,])["ymin"])) %>%
    st_sf() %>%
    mutate(idsite = X, 
           site = dfgps[X,]$site, 
           id = row_number())
  #Identify the direct hexagon
  sftemp <- sftemp %>%
    st_join(dfgps %>% slice(X) %>% mutate(direct = 1) %>% select(direct)) %>%
    mutate(direct = ifelse(is.na(direct), 0, direct))
  #Identify the neighboring hexagons
  vids <- st_intersects(sftemp, sftemp)
  vtreat <- sftemp %>%
    filter(direct == 1) %>%
    pull(id)
  sftemp <- sftemp %>%
    mutate(neighbor = ifelse(id %in% unlist(vids[vtreat]), 1, 0),
           neighbor = ifelse(neighbor == 1 & direct == 1, 0, neighbor))
  #Keep only the treated and neighboring hexagons and merge it with the main sf
  sftemp <- sftemp %>%
    filter(direct == 1 | neighbor == 1)
  if(X == 1){
    sfgrid <- sftemp
  } else{
    sfgrid <- sfgrid %>%
      bind_rows(sftemp)  
  }
}
#Merge The After Hours Project sites that are very close (id 16 and 17)
sftemp1 <- sfgrid %>%
  filter(neighbor == 1) %>%
  filter(idsite %in% 16 & id %in% c(13,19,23)) 
sftemp2 <- sfgrid %>%
  filter(neighbor == 1) %>%
  filter(idsite %in% 17 & id %in% c(11,14)) 
sftemp <- sftemp1 %>%
  st_intersection(sftemp2) %>%
  st_union(sfgrid %>% filter((idsite == 16 & id == 23) | idsite == 17 & id == 11) %>% select(id, idsite)) %>%
  mutate(id = 1) %>%
  group_by(id) %>%
  summarise(n = n()) %>%
  st_difference(sfgrid %>% filter(idsite == 16 & id == 13)) %>%
  st_difference(sfgrid %>% filter(idsite == 17 & id == 20)) %>%
  st_difference(sfgrid %>% filter(idsite == 16 & direct == 1)) %>%
  st_difference(sfgrid %>% filter(idsite == 17 & direct == 1)) %>%
  mutate(id = 1) %>%
  group_by(id) %>%
  summarise(n = n())
sfgrid <- sfgrid %>%
  filter(!(idsite %in% 16 & id %in% c(19,23))) %>% 
  filter(!(idsite %in% 17 & id %in% c(11,14))) %>% 
  bind_rows(sftemp) %>%
  select(idsite, site, id, direct, neighbor) %>%
  mutate(site = ifelse(is.na(idsite), "The After Hours Project, Inc.", site), 
         idsite = ifelse(is.na(idsite), 16, idsite), 
         direct = ifelse(is.na(direct), 0, direct), 
         neighbor = ifelse(is.na(neighbor), 1, neighbor)) 
#Modify hexagons from the Harlem United site that are close to the treatment
sftemp <- sfgrid %>%
  filter(idsite %in% 11 & id %in% c(19,23)) %>% 
  st_difference(sfgrid %>% filter(idsite == 1 & id == 11)) %>%
  st_difference(sfgrid %>% filter(idsite == 1 & id == 14)) %>%
  select(idsite, site, id, direct, neighbor)
sfgrid <- sfgrid %>%
  filter(!(idsite %in% 11 & id %in% c(19,23))) %>% 
  bind_rows(sftemp)
#Transform CRS
sfgrid <- sfgrid %>%
  mutate(id = row_number()) %>% 
  st_transform(4326)


#### 2. Identify police precinct using centroids of hexagons #####-
#Import data
sfpd <- read_sf("../Data/NYC_Police_Precincts/NYC_Police_Precincts.shp") %>% 
  rename_all(tolower)
sfgrid1 <- st_centroid(sfgrid) %>% #ignore warning
  st_join(sfpd %>% select(precinct)) %>%
  st_set_geometry(NULL) %>% 
  as.data.frame() %>%
  select(id, precinct)
sfgrid <- sfgrid %>%
  left_join(sfgrid1, by = "id")
#Manually input the police precinct for three sites
sfgrid <- sfgrid %>%
  mutate(precinct = ifelse(id == 12, 33, precinct), 
         precinct = ifelse(id == 14, 34, precinct),
         precinct = ifelse(id == 49, 120, precinct))
#ggplot() + geom_sf(data = sfgrid, alpha = 0.3) + geom_sf(data = dfgps, color = "black")
#st_write(sfgrid, "../Data/Grid/Hexagonal grid sites1.shp", append = FALSE)
#sfgrid <- read_sf("../Data/Grid/Hexagonal grid sites1.shp") 
rm(dfgps, sfcircle, sfgrid, sfgrid1, sftemp, sftemp1, sftemp2, sfpd, vids, vtreat, X); gc()

  
  



################################################################################-
## Data processing: Create hexagonal grid (three hexagons) #####################
################################################################################-
#### 1. Create an hexagonal grid using #####-
#Cell size measures the length of a square, not of an hexagon
#Length: 187 meters (175 = 320/1.71); 480 feet (613 = 320/1.71*3.28)
dfgps <- fread("../Data/Injection site addresses gps.csv") %>%
  st_as_sf(coords = c("long","lat"), crs = 4326) %>%
  st_transform(32618)
for(X in 1:nrow(dfgps)){
  #Create a buffer and then the hexagonal grid
  sfcircle <- st_buffer(dfgps[X,], dist = 500)
  sftemp <- sfcircle %>%
    st_make_grid(cellsize = 320, square = FALSE, 
                 offset = c(st_bbox(dfgps[X,])["xmin"], st_bbox(dfgps[X,])["ymin"])) %>%
    st_sf() %>%
    mutate(idsite = X, 
           site = dfgps[X,]$site, 
           id = row_number())
  #Identify the direct hexagon
  sftemp <- sftemp %>%
    st_join(dfgps %>% slice(X) %>% mutate(direct = 1) %>% select(direct)) %>%
    mutate(direct = ifelse(is.na(direct), 0, direct))
  #Identify the neighboring hexagons
  vids <- st_intersects(sftemp, sftemp)
  vtreat <- sftemp %>%
    filter(direct == 1) %>%
    pull(id)
  sftemp <- sftemp %>%
    mutate(neighbor = ifelse(id %in% unlist(vids[vtreat]), 1, 0),
           neighbor = ifelse(neighbor == 1 & direct == 1, 0, neighbor))
  #Keep only the direct and neighboring hexagons and merge it with the main sf
  sftemp <- sftemp %>%
    filter(direct == 1 | neighbor == 1)
  if(X == 1){
    sfgrid <- sftemp
  } else{
    sfgrid <- sfgrid %>%
      bind_rows(sftemp)  
  }
}
#Merge The After Hours Project sites that are very close (id 16 and 17)
sfgrid <- sfgrid %>%
  filter(!(idsite == 16 & id %in% c(13,16))) %>%
  filter(!(idsite == 17 & id %in% c(4,7)))
sftemp1 <- sfgrid %>%
  filter(idsite == 16) %>%
  st_difference(sfgrid %>% filter(idsite == 17 & id == 3)) %>%
  st_difference(sfgrid %>% filter(idsite == 17 & id == 6)) %>%
  st_difference(sfgrid %>% filter(idsite == 17 & id == 9)) %>%
  st_difference(sfgrid %>% filter(idsite == 17 & id == 12)) %>%
  select(idsite, site, id, direct, neighbor)
sftemp2 <- sfgrid %>%
  filter(idsite == 17) %>%
  st_difference(sfgrid %>% filter(idsite == 16 & id == 3)) %>%
  st_difference(sfgrid %>% filter(idsite == 16 & id == 6)) %>%
  st_difference(sfgrid %>% filter(idsite == 16 & id == 8)) %>%
  st_difference(sfgrid %>% filter(idsite == 16 & id == 11)) %>%
  filter(!id %in% c(3,6,9)) %>%
  select(idsite, site, id, direct, neighbor)
sftemp3 <- sfgrid %>%
  filter(idsite == 17 & id %in% c(3,6,9))
sfgrid <- sfgrid %>%
  filter(!idsite %in% 16:17) %>%
  bind_rows(sftemp1) %>%
  bind_rows(sftemp2) %>%
  bind_rows(sftemp3)
#Modify hexagons from the Harlem United site that are close to the treatment
sfgrid <- sfgrid %>%
  filter(!(idsite == 11 & id %in% c(13,16)))
sftemp1 <- sfgrid %>%
  filter(idsite == 11) %>%
  st_difference(sfgrid %>% filter(idsite == 1 & id == 1)) %>%
  st_difference(sfgrid %>% filter(idsite == 1 & id == 4)) %>%
  st_difference(sfgrid %>% filter(idsite == 1 & id == 7)) %>%
  filter(!id %in% c(8,11,14)) %>%
  select(idsite, site, id, direct, neighbor)
sftemp2 <- sfgrid %>%
  filter(idsite == 1) %>%
  st_difference(sfgrid %>% filter(idsite == 11 & id == 8)) %>%
  st_difference(sfgrid %>% filter(idsite == 11 & id == 11)) %>%
  st_difference(sfgrid %>% filter(idsite == 11 & id == 14)) %>%
  select(idsite, site, id, direct, neighbor)
sftemp3 <- sfgrid %>%
  filter(idsite == 11 & id %in% c(8,11,14))
sfgrid <- sfgrid %>%
  filter(!idsite %in% c(1,11)) %>%
  bind_rows(sftemp1) %>%
  bind_rows(sftemp2) %>%
  bind_rows(sftemp3)
#Remove some spillover hexagons that are inappropiate
sfgrid <- sfgrid %>%
  filter(!(idsite == 2 & id %in% c(13,16))) %>%
  filter(!(idsite == 7 & id %in% 16))
#Transform CRS
sfgrid <- sfgrid %>%
  mutate(id = row_number()) %>%
  st_transform(4326)


#### 2. Identify police precinct using centroids of hexagons #####-
#Import data
sfpd <- read_sf("../Data/NYC_Police_Precincts/NYC_Police_Precincts.shp") %>% 
  rename_all(tolower)
sfgrid1 <- st_centroid(sfgrid) %>% #ignore warning
  st_join(sfpd %>% select(precinct)) %>%
  st_set_geometry(NULL) %>% 
  as.data.frame() %>%
  select(id, precinct)
sfgrid <- sfgrid %>%
  left_join(sfgrid1, by = "id")
#Manually input the police precinct for three sites
sfgrid <- sfgrid %>%
  mutate(precinct = ifelse(id == 68, 120, precinct), 
         precinct = ifelse(id == 69, 120, precinct),
         precinct = ifelse(id == 129, 9, precinct))
#ggplot() + geom_sf(data = sfgrid, alpha = 0.3) + geom_sf(data = dfgps, color = "black")
#st_write(sfgrid, "../Data/Grid/Hexagonal grid sites2.shp", append = FALSE)
#sfgrid <- read_sf("../Data/Grid/Hexagonal grid sites2.shp") 
rm(dfgps, sfcircle, sfgrid, sfgrid1, sftemp, sftemp1, sftemp2, sftemp3, sfpd, vids, vtreat, X); gc()






#####################################################################################-
## Data processing: aggregate CRIME data at the month-hexagon level (all city) ######
#####################################################################################-
#### 1. Import crime data #####-
#https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Historic/qgea-i56i
#https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Current-Year-To-Date-/5uac-w243
dftemp <- fread("../Data/New York/NYPD_Complaint_Data_Current__Year_To_Date_.csv", 
                select = c("CMPLNT_NUM", "CMPLNT_FR_DT", "OFNS_DESC", "Latitude", "Longitude")) %>%
  rename_all(tolower) %>%
  mutate(cmplnt_num = as.numeric(cmplnt_num))
dfcrime <- fread("../Data/New York/NYPD_Complaint_Data_Historic.csv", 
                 select = c("CMPLNT_NUM", "CMPLNT_FR_DT", "OFNS_DESC", "Latitude", "Longitude")) %>%
  rename_all(tolower) %>%
  bind_rows(dftemp) %>%
  mutate(year = year(mdy(cmplnt_fr_dt)),
         month = month(mdy(cmplnt_fr_dt))) %>%
  filter(year %in% 2018:2022 & complete.cases(month, latitude, longitude)) 
rm(dftemp); gc()
#Extract sample crimes to inspect location
set.seed(88)
dfcrime %>%
  select(cmplnt_num, latitude, longitude, parks_nm, prem_typ_desc) %>%
  distinct(latitude, longitude, .keep_all = TRUE) %>%
  fwrite("../Data/NYPD_complaint_sample.csv")


#### 2. Match crimes to each hexagon ####-
sfgrid <- read_sf("../Data/Grid/Hexagonal grid.shp") 
dfcrime <- dfcrime %>%
  mutate(crime_murder = ifelse(ofns_desc %in% c("MURDER & NON-NEGL. MANSLAUGHTER"), 1, 0),
         crime_assault = ifelse(ofns_desc %in% c('FELONY ASSAULT'), 1, 0),
         crime_robbery = ifelse(ofns_desc %in% c("ROBBERY"), 1, 0),
         crime_theft = ifelse(ofns_desc %in% c('GRAND LARCENY'), 1, 0),
         crime_burglary = ifelse(ofns_desc %in% c('BURGLARY'), 1, 0),
         crime_mvtheft = ifelse(ofns_desc %in% c('GRAND LARCENY OF MOTOR VEHICLE'), 1, 0),
         crime_weapon = ifelse(ofns_desc %in% c('DANGEROUS WEAPONS'), 1, 0),
         crime_lowassault = ifelse(ofns_desc %in% c('ASSAULT 3 & RELATED OFFENSES'), 1, 0),
         crime_dui = ifelse(ofns_desc %in% c('INTOXICATED & IMPAIRED DRIVING', 'INTOXICATED/IMPAIRED DRIVING'), 1, 0),
         crime_drugs = ifelse(ofns_desc %in% c("DANGEROUS DRUGS"), 1, 0),
         crime_trespass = ifelse(ofns_desc %in% c("CRIMINAL TRESPASS"), 1, 0),
         crime_nonserious = ifelse(ofns_desc %in% c('INTOXICATED & IMPAIRED DRIVING', 'INTOXICATED/IMPAIRED DRIVING', 
                                                    "DANGEROUS DRUGS", "CRIMINAL TRESPASS"), 1, 0))
#Spatial merge by month to speed the process
dftemp1 <- data.frame()
for(X in 1:12){
  dftemp2 <- st_as_sf(dfcrime %>% filter(month == X), coords = c("longitude","latitude"), crs = 4326) %>%
    st_join(sfgrid %>% select(id)) %>%
    st_set_geometry(NULL) %>% 
    as.data.frame() %>%
    filter(complete.cases(id)) %>%
    group_by(id, year, month) %>%
    summarise_at(vars(crime_murder:crime_nonserious), sum) %>%
    mutate(crime_violent = crime_murder + crime_robbery + crime_assault,
           crime_property = crime_theft + crime_burglary + crime_mvtheft, 
           crime_index = crime_violent + crime_property)
  dftemp1 <- dftemp1 %>%
    bind_rows(dftemp2)
}
dfcrime <- dftemp1
rm(dftemp1, dftemp2, X); gc()  
#fwrite(dfcrime, "../Data/Crime crime month-hexagon data 2018-2022.csv")
#dfcrime <- fread("../Data/Crime crime month-hexagon data 2018-2022.csv", integer64 = "character")
#dfcrime %>% group_by(year, month) %>% summarise(crime_index = sum(crime_index)) %>% View()
rm(dfcrime, sfgrid); gc()

#stats in text
dftemp <- dfcrime %>%
  mutate(datenum = year + (month - 1) /12) %>%
  group_by(datenum, year, month) %>%
  summarise_at(vars(crime_murder:crime_index), sum) %>%
  mutate(post = ifelse(datenum >= 2021.91, "post", "pre"))
ggplot(dftemp, aes(x = "datenum", y = crime_violent)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = 2021.917, color = "red", linetype = "dashed") +
  #geom_smooth(method = "lm", formula = y ~ poly(x, 2)) +
  geom_smooth(aes(group = post), method = "lm", formula = y ~ poly(x, 2)) +
  theme_bw()
rm(dftemp, dfcrime)




############################################################################################-
## Data processing: aggregate CRIME data at the month-hexagon level (single hexagon) #######
############################################################################################-
#### 1. Import crime data #####-
#https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Historic/qgea-i56i
#https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Current-Year-To-Date-/5uac-w243
dftemp <- fread("../Data/New York/NYPD_Complaint_Data_Current__Year_To_Date_.csv", 
                select = c("CMPLNT_NUM", "CMPLNT_FR_DT", "OFNS_DESC", "Latitude", "Longitude")) %>%
  rename_all(tolower) %>%
  mutate(cmplnt_num = as.numeric(cmplnt_num))
dfcrime <- fread("../Data/New York/NYPD_Complaint_Data_Historic.csv", 
                 select = c("CMPLNT_NUM", "CMPLNT_FR_DT", "OFNS_DESC", "Latitude", "Longitude")) %>%
  rename_all(tolower) %>%
  bind_rows(dftemp) %>%
  mutate(year = year(mdy(cmplnt_fr_dt)),
         month = month(mdy(cmplnt_fr_dt))) %>%
  filter(year %in% 2018:2022 & complete.cases(month, latitude, longitude)) 
rm(dftemp); gc()


#### 2. Match crimes to each hexagon ####-
sfgrid <- read_sf("../Data/Grid/Hexagonal grid sites1.shp") 
dfcrime <- dfcrime %>%
  mutate(crime_murder = ifelse(ofns_desc %in% c("MURDER & NON-NEGL. MANSLAUGHTER"), 1, 0),
         crime_assault = ifelse(ofns_desc %in% c('FELONY ASSAULT'), 1, 0),
         crime_robbery = ifelse(ofns_desc %in% c("ROBBERY"), 1, 0),
         crime_theft = ifelse(ofns_desc %in% c('GRAND LARCENY'), 1, 0),
         crime_burglary = ifelse(ofns_desc %in% c('BURGLARY'), 1, 0),
         crime_mvtheft = ifelse(ofns_desc %in% c('GRAND LARCENY OF MOTOR VEHICLE'), 1, 0),
         crime_weapon = ifelse(ofns_desc %in% c('DANGEROUS WEAPONS'), 1, 0),
         crime_lowassault = ifelse(ofns_desc %in% c('ASSAULT 3 & RELATED OFFENSES'), 1, 0),
         crime_dui = ifelse(ofns_desc %in% c('INTOXICATED & IMPAIRED DRIVING', 'INTOXICATED/IMPAIRED DRIVING'), 1, 0),
         crime_drugs = ifelse(ofns_desc %in% c("DANGEROUS DRUGS"), 1, 0),
         crime_trespass = ifelse(ofns_desc %in% c("CRIMINAL TRESPASS"), 1, 0),
         crime_nonserious = ifelse(ofns_desc %in% c('INTOXICATED & IMPAIRED DRIVING', 'INTOXICATED/IMPAIRED DRIVING', 
                                                    "DANGEROUS DRUGS", "CRIMINAL TRESPASS"), 1, 0))
#Spatial merge by month to speed the process
dftemp1 <- data.frame()
for(X in 1:12){
  dftemp2 <- st_as_sf(dfcrime %>% filter(month == X), coords = c("longitude","latitude"), crs = 4326) %>%
    st_join(sfgrid %>% select(id)) %>%
    st_set_geometry(NULL) %>% 
    as.data.frame() %>%
    filter(complete.cases(id)) %>%
    group_by(id, year, month) %>%
    summarise_at(vars(crime_murder:crime_nonserious), sum) %>%
    mutate(crime_violent = crime_murder + crime_robbery + crime_assault,
           crime_property = crime_theft + crime_burglary + crime_mvtheft, 
           crime_index = crime_violent + crime_property)
  dftemp1 <- dftemp1 %>%
    bind_rows(dftemp2)
}
dfcrime <- dftemp1
rm(dftemp1, dftemp2, X); gc()  
#fwrite(dfcrime, "../Data/Crime crime month-hexagon1 data 2018-2022.csv")
#dfcrime <- fread("../Data/Crime crime month-hexagon1 data 2018-2022.csv", integer64 = "character")
#dfcrime %>% group_by(year, month) %>% summarise(crime_index = sum(crime_index)) %>% View()
rm(dfcrime, sfgrid); gc()





############################################################################################-
## Data processing: aggregate CRIME data at the month-hexagon level (three hexagons) #######
############################################################################################-
#### 1. Import crime data #####-
#https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Historic/qgea-i56i
#https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Current-Year-To-Date-/5uac-w243
dftemp <- fread("../Data/New York/NYPD_Complaint_Data_Current__Year_To_Date_.csv", 
                select = c("CMPLNT_NUM", "CMPLNT_FR_DT", "OFNS_DESC", "Latitude", "Longitude")) %>%
  rename_all(tolower) %>%
  mutate(cmplnt_num = as.numeric(cmplnt_num))
dfcrime <- fread("../Data/New York/NYPD_Complaint_Data_Historic.csv", 
                 select = c("CMPLNT_NUM", "CMPLNT_FR_DT", "OFNS_DESC", "Latitude", "Longitude")) %>%
  rename_all(tolower) %>%
  bind_rows(dftemp) %>%
  mutate(year = year(mdy(cmplnt_fr_dt)),
         month = month(mdy(cmplnt_fr_dt))) %>%
  filter(year %in% 2018:2022 & complete.cases(month, latitude, longitude)) 
rm(dftemp); gc()


#### 2. Match crimes to each hexagon ####-
sfgrid <- read_sf("../Data/Grid/Hexagonal grid sites2.shp") 
dfcrime <- dfcrime %>%
  mutate(crime_murder = ifelse(ofns_desc %in% c("MURDER & NON-NEGL. MANSLAUGHTER"), 1, 0),
         crime_assault = ifelse(ofns_desc %in% c('FELONY ASSAULT'), 1, 0),
         crime_robbery = ifelse(ofns_desc %in% c("ROBBERY"), 1, 0),
         crime_theft = ifelse(ofns_desc %in% c('GRAND LARCENY'), 1, 0),
         crime_burglary = ifelse(ofns_desc %in% c('BURGLARY'), 1, 0),
         crime_mvtheft = ifelse(ofns_desc %in% c('GRAND LARCENY OF MOTOR VEHICLE'), 1, 0),
         crime_weapon = ifelse(ofns_desc %in% c('DANGEROUS WEAPONS'), 1, 0),
         crime_lowassault = ifelse(ofns_desc %in% c('ASSAULT 3 & RELATED OFFENSES'), 1, 0),
         crime_dui = ifelse(ofns_desc %in% c('INTOXICATED & IMPAIRED DRIVING', 'INTOXICATED/IMPAIRED DRIVING'), 1, 0),
         crime_drugs = ifelse(ofns_desc %in% c("DANGEROUS DRUGS"), 1, 0),
         crime_trespass = ifelse(ofns_desc %in% c("CRIMINAL TRESPASS"), 1, 0),
         crime_nonserious = ifelse(ofns_desc %in% c('INTOXICATED & IMPAIRED DRIVING', 'INTOXICATED/IMPAIRED DRIVING', 
                                                    "DANGEROUS DRUGS", "CRIMINAL TRESPASS"), 1, 0))
#Spatial merge by month to speed the process
dftemp1 <- data.frame()
for(X in 1:12){
  dftemp2 <- st_as_sf(dfcrime %>% filter(month == X), coords = c("longitude","latitude"), crs = 4326) %>%
    st_join(sfgrid %>% select(id)) %>%
    st_set_geometry(NULL) %>% 
    as.data.frame() %>%
    filter(complete.cases(id)) %>%
    group_by(id, year, month) %>%
    summarise_at(vars(crime_murder:crime_nonserious), sum) %>%
    mutate(crime_violent = crime_murder + crime_robbery + crime_assault,
           crime_property = crime_theft + crime_burglary + crime_mvtheft, 
           crime_index = crime_violent + crime_property)
  dftemp1 <- dftemp1 %>%
    bind_rows(dftemp2)
}
dfcrime <- dftemp1
rm(dftemp1, dftemp2, X); gc()
#fwrite(dfcrime, "../Data/Crime crime month-hexagon2 data 2018-2022.csv")
#dfcrime <- fread("../Data/Crime crime month-hexagon2 data 2018-2022.csv", integer64 = "character")
#dfcrime %>% group_by(year, month) %>% summarise(crime_index = sum(crime_index)) %>% View()
rm(dfcrime, sfgrid); gc()





##########################################################################################-
## Data processing: aggregate ARREST data at the month-hexagon level (all city) ##########
##########################################################################################-
#### 1. Import arrest data #####-
#https://data.cityofnewyork.us/Public-Safety/NYPD-Arrests-Data-Historic-/8h9b-rp9u
#https://data.cityofnewyork.us/Public-Safety/NYPD-Arrest-Data-Year-to-Date-/uip8-fykc
dftemp <- fread("../Data/New York/NYPD_Arrest_Data__Year_to_Date_.csv", 
                select = c("ARREST_DATE", "OFNS_DESC", "Latitude", "Longitude")) %>%
  rename_all(tolower) 
dfarrest <- fread("../Data/New York/NYPD_Arrests_Data__Historic_.csv", 
                  select = c("ARREST_DATE", "OFNS_DESC", "Latitude", "Longitude")) %>%
  rename_all(tolower) %>%
  bind_rows(dftemp) %>%
  mutate(year = year(mdy(arrest_date)), 
         month = month(mdy(arrest_date))) %>%
  filter(year %in% 2018:2022 & complete.cases(month, latitude, longitude)) 
rm(dftemp); gc()


#### 2. Match arrests to each hexagon ####-
sfgrid <- read_sf("../Data/Grid/Hexagonal grid.shp") 
dfarrest <- dfarrest %>%
  mutate(arrest_murder = ifelse(ofns_desc %in% c("MURDER & NON-NEGL. MANSLAUGHTER"), 1, 0),
         arrest_assault = ifelse(ofns_desc %in% c('FELONY ASSAULT'), 1, 0),
         arrest_robbery = ifelse(ofns_desc %in% c("ROBBERY"), 1, 0),
         arrest_theft = ifelse(ofns_desc %in% c('GRAND LARCENY'), 1, 0),
         arrest_burglary = ifelse(ofns_desc %in% c('BURGLARY'), 1, 0),
         arrest_mvtheft = ifelse(ofns_desc %in% c('GRAND LARCENY OF MOTOR VEHICLE'), 1, 0),
         arrest_weapon = ifelse(ofns_desc %in% c('DANGEROUS WEAPONS'), 1, 0),
         arrest_lowassault = ifelse(ofns_desc %in% c('ASSAULT 3 & RELATED OFFENSES'), 1, 0),
         arrest_dui = ifelse(ofns_desc %in% c('INTOXICATED & IMPAIRED DRIVING', 'INTOXICATED/IMPAIRED DRIVING'), 1, 0),
         arrest_drugs = ifelse(ofns_desc %in% c("DANGEROUS DRUGS"), 1, 0),
         arrest_trespass = ifelse(ofns_desc %in% c("CRIMINAL TRESPASS"), 1, 0),
         arrest_nonserious = ifelse(ofns_desc %in% c('INTOXICATED & IMPAIRED DRIVING', 'INTOXICATED/IMPAIRED DRIVING', 
                                                     "DANGEROUS DRUGS", "CRIMINAL TRESPASS"), 1, 0))
dfarrest <- st_as_sf(dfarrest, coords = c("longitude","latitude"), crs = 4326) %>%
  st_join(sfgrid %>% select(id)) %>%
  st_set_geometry(NULL) %>% 
  as.data.frame() %>%
  filter(complete.cases(id)) %>%
  group_by(id, year, month) %>%
  summarise_at(vars(arrest_murder:arrest_nonserious), sum) %>%
  mutate(arrest_violent = arrest_murder + arrest_robbery + arrest_assault,
         arrest_property = arrest_theft + arrest_burglary + arrest_mvtheft,
         arrest_index = arrest_violent + arrest_property)
#fwrite(dfarrest, "../Data/Crime arrest month-hexagon data 2018-2022.csv")
#dfarrest <- fread("../Data/Crime arrest month-hexagon data 2018-2022.csv", integer64 = "character")
#dfarrest %>% group_by(year, month) %>% summarise(arrest_index = sum(arrest_index)) %>% View()
rm(dfarrest, sfgrid); gc()



##############################################################################################-
## Data processing: aggregate ARREST data at the month-hexagon level (single hexagon) ########
##############################################################################################-
#### 1. Import arrest data #####-
#https://data.cityofnewyork.us/Public-Safety/NYPD-Arrests-Data-Historic-/8h9b-rp9u
#https://data.cityofnewyork.us/Public-Safety/NYPD-Arrest-Data-Year-to-Date-/uip8-fykc
dftemp <- fread("../Data/New York/NYPD_Arrest_Data__Year_to_Date_.csv", 
                select = c("ARREST_DATE", "OFNS_DESC", "Latitude", "Longitude")) %>%
  rename_all(tolower) 
dfarrest <- fread("../Data/New York/NYPD_Arrests_Data__Historic_.csv", 
                  select = c("ARREST_DATE", "OFNS_DESC", "Latitude", "Longitude")) %>%
  rename_all(tolower) %>%
  bind_rows(dftemp) %>%
  mutate(year = year(mdy(arrest_date)), 
         month = month(mdy(arrest_date))) %>%
  filter(year %in% 2018:2022 & complete.cases(month, latitude, longitude)) 
rm(dftemp); gc()


#### 2. Match arrests to each hexagon ####-
sfgrid <- read_sf("../Data/Grid/Hexagonal grid sites1.shp") 
dfarrest <- dfarrest %>%
  mutate(arrest_murder = ifelse(ofns_desc %in% c("MURDER & NON-NEGL. MANSLAUGHTER"), 1, 0),
         arrest_assault = ifelse(ofns_desc %in% c('FELONY ASSAULT'), 1, 0),
         arrest_robbery = ifelse(ofns_desc %in% c("ROBBERY"), 1, 0),
         arrest_theft = ifelse(ofns_desc %in% c('GRAND LARCENY'), 1, 0),
         arrest_burglary = ifelse(ofns_desc %in% c('BURGLARY'), 1, 0),
         arrest_mvtheft = ifelse(ofns_desc %in% c('GRAND LARCENY OF MOTOR VEHICLE'), 1, 0),
         arrest_weapon = ifelse(ofns_desc %in% c('DANGEROUS WEAPONS'), 1, 0),
         arrest_lowassault = ifelse(ofns_desc %in% c('ASSAULT 3 & RELATED OFFENSES'), 1, 0),
         arrest_dui = ifelse(ofns_desc %in% c('INTOXICATED & IMPAIRED DRIVING', 'INTOXICATED/IMPAIRED DRIVING'), 1, 0),
         arrest_drugs = ifelse(ofns_desc %in% c("DANGEROUS DRUGS"), 1, 0),
         arrest_trespass = ifelse(ofns_desc %in% c("CRIMINAL TRESPASS"), 1, 0),
         arrest_nonserious = ifelse(ofns_desc %in% c('INTOXICATED & IMPAIRED DRIVING', 'INTOXICATED/IMPAIRED DRIVING', 
                                                     "DANGEROUS DRUGS", "CRIMINAL TRESPASS"), 1, 0))
dfarrest <- st_as_sf(dfarrest, coords = c("longitude","latitude"), crs = 4326) %>%
  st_join(sfgrid %>% select(id)) %>%
  st_set_geometry(NULL) %>% 
  as.data.frame() %>%
  filter(complete.cases(id)) %>%
  group_by(id, year, month) %>%
  summarise_at(vars(arrest_murder:arrest_nonserious), sum) %>%
  mutate(arrest_violent = arrest_murder + arrest_robbery + arrest_assault,
         arrest_property = arrest_theft + arrest_burglary + arrest_mvtheft,
         arrest_index = arrest_violent + arrest_property)
#fwrite(dfarrest, "../Data/Crime arrest month-hexagon1 data 2018-2022.csv")
#dfarrest <- fread("../Data/Crime arrest month-hexagon1 data 2018-2022.csv", integer64 = "character")
#dfarrest %>% group_by(year, month) %>% summarise(arrest_index = sum(arrest_index)) %>% View()
rm(dfarrest, sfgrid); gc()







############################################################################################-
## Data processing: aggregate ARREST data at the month-hexagon level (three hexagons) ######
############################################################################################-
#### 1. Import arrest data #####-
#https://data.cityofnewyork.us/Public-Safety/NYPD-Arrests-Data-Historic-/8h9b-rp9u
#https://data.cityofnewyork.us/Public-Safety/NYPD-Arrest-Data-Year-to-Date-/uip8-fykc
dftemp <- fread("../Data/New York/NYPD_Arrest_Data__Year_to_Date_.csv", 
                select = c("ARREST_DATE", "OFNS_DESC", "Latitude", "Longitude")) %>%
  rename_all(tolower) 
dfarrest <- fread("../Data/New York/NYPD_Arrests_Data__Historic_.csv", 
                  select = c("ARREST_DATE", "OFNS_DESC", "Latitude", "Longitude")) %>%
  rename_all(tolower) %>%
  bind_rows(dftemp) %>%
  mutate(year = year(mdy(arrest_date)), 
         month = month(mdy(arrest_date))) %>%
  filter(year %in% 2018:2022 & complete.cases(month, latitude, longitude)) 
rm(dftemp); gc()


#### 2. Match arrests to each hexagon ####-
sfgrid <- read_sf("../Data/Grid/Hexagonal grid sites2.shp") 
dfarrest <- dfarrest %>%
  mutate(arrest_murder = ifelse(ofns_desc %in% c("MURDER & NON-NEGL. MANSLAUGHTER"), 1, 0),
         arrest_assault = ifelse(ofns_desc %in% c('FELONY ASSAULT'), 1, 0),
         arrest_robbery = ifelse(ofns_desc %in% c("ROBBERY"), 1, 0),
         arrest_theft = ifelse(ofns_desc %in% c('GRAND LARCENY'), 1, 0),
         arrest_burglary = ifelse(ofns_desc %in% c('BURGLARY'), 1, 0),
         arrest_mvtheft = ifelse(ofns_desc %in% c('GRAND LARCENY OF MOTOR VEHICLE'), 1, 0),
         arrest_weapon = ifelse(ofns_desc %in% c('DANGEROUS WEAPONS'), 1, 0),
         arrest_lowassault = ifelse(ofns_desc %in% c('ASSAULT 3 & RELATED OFFENSES'), 1, 0),
         arrest_dui = ifelse(ofns_desc %in% c('INTOXICATED & IMPAIRED DRIVING', 'INTOXICATED/IMPAIRED DRIVING'), 1, 0),
         arrest_drugs = ifelse(ofns_desc %in% c("DANGEROUS DRUGS"), 1, 0),
         arrest_trespass = ifelse(ofns_desc %in% c("CRIMINAL TRESPASS"), 1, 0),
         arrest_nonserious = ifelse(ofns_desc %in% c('INTOXICATED & IMPAIRED DRIVING', 'INTOXICATED/IMPAIRED DRIVING', 
                                                     "DANGEROUS DRUGS", "CRIMINAL TRESPASS"), 1, 0))
dfarrest <- st_as_sf(dfarrest, coords = c("longitude","latitude"), crs = 4326) %>%
  st_join(sfgrid %>% select(id)) %>%
  st_set_geometry(NULL) %>% 
  as.data.frame() %>%
  filter(complete.cases(id)) %>%
  group_by(id, year, month) %>%
  summarise_at(vars(arrest_murder:arrest_nonserious), sum) %>%
  mutate(arrest_violent = arrest_murder + arrest_robbery + arrest_assault,
         arrest_property = arrest_theft + arrest_burglary + arrest_mvtheft,
         arrest_index = arrest_violent + arrest_property)
#fwrite(dfarrest, "../Data/Crime arrest month-hexagon2 data 2018-2022.csv")
#dfarrest <- fread("../Data/Crime arrest month-hexagon2 data 2018-2022.csv", integer64 = "character")
#dfarrest %>% group_by(year, month) %>% summarise(arrest_index = sum(arrest_index)) %>% View()
rm(dfarrest, sfgrid); gc()






#############################################################################################-
## Data processing: aggregate 911 emergency data at the month-hexagon level (all city) ######
#############################################################################################-
#### 1. Import 911 data #####-
#https://data.cityofnewyork.us/Public-Safety/NYPD-Calls-for-Service-Historic-/d6zx-ckhd
#https://data.cityofnewyork.us/Public-Safety/NYPD-Calls-for-Service-Year-to-Date-/n2zq-pubd
#Done in two steps because the dataset is too large
dftemp <- fread("../Data/New York/NYPD_Calls_for_Service__Year_to_Date_.csv", 
                select = c("INCIDENT_DATE", "TYP_DESC", "Latitude", "Longitude")) %>%
  rename_all(tolower) %>%
  mutate(call_crime = ifelse(grepl("CRIME|CRIMES|ALARM|ALARMS|DISPUTE", typ_desc), 1, 0),
         call_crime = ifelse(grepl("LARCENY|DISORDERLY|ASSAULT|BURGLARY|SPOTTER|ROBBERY", typ_desc), 1, call_crime),
         call_assault = ifelse(grepl("ASSAULT", typ_desc), 1, 0),
         call_trespass = ifelse(grepl("TRESPASS", typ_desc), 1, 0),
         call_medic = ifelse(grepl("AMBULANCE", typ_desc), 1, 0)) %>%
  filter(call_crime == 1 | call_medic == 1) %>%
  mutate(year = year(mdy(incident_date)), 
         month = month(mdy(incident_date))) %>%
  filter(year %in% 2018:2022 & complete.cases(month, latitude, longitude))
dfcalls <- fread("../Data/New York/NYPD_Calls_for_Service__Historic_.csv", 
                 select = c("INCIDENT_DATE", "TYP_DESC", "Latitude", "Longitude")) %>%
  rename_all(tolower) %>%
  mutate(call_crime = ifelse(grepl("CRIME|CRIMES|ALARM|ALARMS|DISPUTE", typ_desc), 1, 0),
         call_crime = ifelse(grepl("LARCENY|DISORDERLY|ASSAULT|BURGLARY|SPOTTER|ROBBERY", typ_desc), 1, call_crime),
         call_assault = ifelse(grepl("ASSAULT", typ_desc), 1, 0),
         call_trespass = ifelse(grepl("TRESPASS", typ_desc), 1, 0),
         call_medic = ifelse(grepl("AMBULANCE", typ_desc), 1, 0)) %>%
  filter(call_crime == 1 | call_medic == 1) %>%
  mutate(year = year(mdy(incident_date)), 
         month = month(mdy(incident_date))) %>%
  filter(year %in% 2018:2022 & complete.cases(month, latitude, longitude)) %>%
  bind_rows(dftemp)
rm(dftemp); gc()  


#### 2. Match calls for service to each hexagon ####-
#Spatial merge by month to speed the process
sfgrid <- read_sf("../Data/Grid/Hexagonal grid.shp") 
dftemp1 <- data.frame()
for(X in 1:12){
  dftemp2 <- st_as_sf(dfcalls %>% filter(month == X), coords = c("longitude","latitude"), crs = 4326) %>%
    st_join(sfgrid %>% select(id)) %>%
    st_set_geometry(NULL) %>% 
    as.data.frame() %>%
    filter(complete.cases(id)) %>%
    group_by(id, year, month) %>%
    summarise_at(vars(call_crime:call_medic), sum) %>%
    ungroup()
  dftemp1 <- dftemp1 %>%
    bind_rows(dftemp2)
}
dfcalls <- dftemp1
rm(dftemp1, dftemp2, X); gc()  
#fwrite(dfcalls, "../Data/Crime calls month-hexagon data 2018-2022.csv")
#dfcalls <- fread("../Data/Crime calls month-hexagon data 2018-2022.csv", integer64 = "character")
#dfcalls %>% group_by(year, month) %>% summarise(call_crime = sum(call_crime)) %>% View()
rm(dfcalls, sfgrid); gc()







#####################################################################################################-
## Data processing: aggregate 911 emergency data at the month-hexagon level (single hexagon) ########
#####################################################################################################-
#### 1. Import 911 data #####-
#https://data.cityofnewyork.us/Public-Safety/NYPD-Calls-for-Service-Historic-/d6zx-ckhd
#https://data.cityofnewyork.us/Public-Safety/NYPD-Calls-for-Service-Year-to-Date-/n2zq-pubd
#Done in two steps because the dataset is too large
dftemp <- fread("../Data/New York/NYPD_Calls_for_Service__Year_to_Date_.csv", 
                select = c("INCIDENT_DATE", "TYP_DESC", "Latitude", "Longitude")) %>%
  rename_all(tolower) %>%
  mutate(call_crime = ifelse(grepl("CRIME|CRIMES|ALARM|ALARMS|DISPUTE", typ_desc), 1, 0),
         call_crime = ifelse(grepl("LARCENY|DISORDERLY|ASSAULT|BURGLARY|SPOTTER|ROBBERY", typ_desc), 1, call_crime),
         call_assault = ifelse(grepl("ASSAULT", typ_desc), 1, 0),
         call_trespass = ifelse(grepl("TRESPASS", typ_desc), 1, 0),
         call_medic = ifelse(grepl("AMBULANCE", typ_desc), 1, 0)) %>%
  filter(call_crime == 1 | call_medic == 1) %>%
  mutate(year = year(mdy(incident_date)), 
         month = month(mdy(incident_date))) %>%
  filter(year %in% 2018:2022 & complete.cases(month, latitude, longitude))
dfcalls <- fread("../Data/New York/NYPD_Calls_for_Service__Historic_.csv", 
                 select = c("INCIDENT_DATE", "TYP_DESC", "Latitude", "Longitude")) %>%
  rename_all(tolower) %>%
  mutate(call_crime = ifelse(grepl("CRIME|CRIMES|ALARM|ALARMS|DISPUTE", typ_desc), 1, 0),
         call_crime = ifelse(grepl("LARCENY|DISORDERLY|ASSAULT|BURGLARY|SPOTTER|ROBBERY", typ_desc), 1, call_crime),
         call_assault = ifelse(grepl("ASSAULT", typ_desc), 1, 0),
         call_trespass = ifelse(grepl("TRESPASS", typ_desc), 1, 0),
         call_medic = ifelse(grepl("AMBULANCE", typ_desc), 1, 0)) %>%
  filter(call_crime == 1 | call_medic == 1) %>%
  mutate(year = year(mdy(incident_date)), 
         month = month(mdy(incident_date))) %>%
  filter(year %in% 2018:2022 & complete.cases(month, latitude, longitude)) %>%
  bind_rows(dftemp)
rm(dftemp); gc()  


#### 2. Match calls for service to each hexagon ####-
#Spatial merge by month to speed the process
sfgrid <- read_sf("../Data/Grid/Hexagonal grid sites1.shp") 
dftemp1 <- data.frame()
for(X in 1:12){
  dftemp2 <- st_as_sf(dfcalls %>% filter(month == X), coords = c("longitude","latitude"), crs = 4326) %>%
    st_join(sfgrid %>% select(id)) %>%
    st_set_geometry(NULL) %>% 
    as.data.frame() %>%
    filter(complete.cases(id)) %>%
    group_by(id, year, month) %>%
    summarise_at(vars(call_crime:call_medic), sum) %>%
    ungroup()
  dftemp1 <- dftemp1 %>%
    bind_rows(dftemp2)
}
dfcalls <- dftemp1
rm(dftemp1, dftemp2, X); gc()  
#fwrite(dfcalls, "../Data/Crime calls month-hexagon1 data 2018-2022.csv")
#dfcalls <- fread("../Data/Crime calls month-hexagon1 data 2018-2022.csv", integer64 = "character")
#dfcalls %>% group_by(year, month) %>% summarise(call_crime = sum(call_crime)) %>% View()
rm(dfcalls, sfgrid); gc()





###################################################################################################-
## Data processing: aggregate 911 emergency data at the month-hexagon level (three hexagons) ######
###################################################################################################-
#### 1. Import 911 data #####-
#https://data.cityofnewyork.us/Public-Safety/NYPD-Calls-for-Service-Historic-/d6zx-ckhd
#https://data.cityofnewyork.us/Public-Safety/NYPD-Calls-for-Service-Year-to-Date-/n2zq-pubd
#Done in two steps because the dataset is too large
dftemp <- fread("../Data/New York/NYPD_Calls_for_Service__Year_to_Date_.csv", 
                select = c("INCIDENT_DATE", "TYP_DESC", "Latitude", "Longitude")) %>%
  rename_all(tolower) %>%
  mutate(call_crime = ifelse(grepl("CRIME|CRIMES|ALARM|ALARMS|DISPUTE", typ_desc), 1, 0),
         call_crime = ifelse(grepl("LARCENY|DISORDERLY|ASSAULT|BURGLARY|SPOTTER|ROBBERY", typ_desc), 1, call_crime),
         call_assault = ifelse(grepl("ASSAULT", typ_desc), 1, 0),
         call_trespass = ifelse(grepl("TRESPASS", typ_desc), 1, 0),
         call_medic = ifelse(grepl("AMBULANCE", typ_desc), 1, 0)) %>%
  filter(call_crime == 1 | call_medic == 1) %>%
  mutate(year = year(mdy(incident_date)), 
         month = month(mdy(incident_date))) %>%
  filter(year %in% 2018:2022 & complete.cases(month, latitude, longitude))
dfcalls <- fread("../Data/New York/NYPD_Calls_for_Service__Historic_.csv", 
                 select = c("INCIDENT_DATE", "TYP_DESC", "Latitude", "Longitude")) %>%
  rename_all(tolower) %>%
  mutate(call_crime = ifelse(grepl("CRIME|CRIMES|ALARM|ALARMS|DISPUTE", typ_desc), 1, 0),
         call_crime = ifelse(grepl("LARCENY|DISORDERLY|ASSAULT|BURGLARY|SPOTTER|ROBBERY", typ_desc), 1, call_crime),
         call_assault = ifelse(grepl("ASSAULT", typ_desc), 1, 0),
         call_trespass = ifelse(grepl("TRESPASS", typ_desc), 1, 0),
         call_medic = ifelse(grepl("AMBULANCE", typ_desc), 1, 0)) %>%
  filter(call_crime == 1 | call_medic == 1) %>%
  mutate(year = year(mdy(incident_date)), 
         month = month(mdy(incident_date))) %>%
  filter(year %in% 2018:2022 & complete.cases(month, latitude, longitude)) %>%
  bind_rows(dftemp)
rm(dftemp); gc()  


#### 2. Match calls for service to each hexagon ####-
sfgrid <- read_sf("../Data/Grid/Hexagonal grid sites2.shp") 
dftemp1 <- data.frame()
for(X in 1:12){
  dftemp2 <- st_as_sf(dfcalls %>% filter(month == X), coords = c("longitude","latitude"), crs = 4326) %>%
    st_join(sfgrid %>% select(id)) %>%
    st_set_geometry(NULL) %>% 
    as.data.frame() %>%
    filter(complete.cases(id)) %>%
    group_by(id, year, month) %>%
    summarise_at(vars(call_crime:call_medic), sum) %>%
    ungroup()
  dftemp1 <- dftemp1 %>%
    bind_rows(dftemp2)
}
dfcalls <- dftemp1
rm(dftemp1, dftemp2, X); gc()  
#fwrite(dfcalls, "../Data/Crime calls month-hexagon2 data 2018-2022.csv")
#dfcalls <- fread("../Data/Crime calls month-hexagon2 data 2018-2022.csv", integer64 = "character")
#dfcalls %>% group_by(year, month) %>% summarise(call_crime = sum(call_crime)) %>% View()
rm(dfcalls, sfgrid); gc()





################################################################################################-
## Data processing: aggregate CRIMINAL SUMMONS data at the month-hexagon level (all city) ######
################################################################################################-
#### 1. Import summons data #####-
#https://data.cityofnewyork.us/Public-Safety/NYPD-Criminal-Court-Summons-Historic-/sv2w-rv3k
#https://data.cityofnewyork.us/Public-Safety/NYPD-Criminal-Court-Summons-Incident-Level-Data-Ye/mv4k-y93f
dftemp <- fread("../Data/New York/NYPD_Criminal_Court_Summons_Incident_Level_Data__Year_To_Date_.csv", 
                select = c("SUMMONS_DATE", "Latitude", "Longitude")) %>%
  rename_all(tolower) 
dfsummons <- fread("../Data/New York/NYPD_Criminal_Court_Summons__Historic_.csv", 
                   select = c("SUMMONS_DATE", "Latitude", "Longitude")) %>%
  rename_all(tolower) %>%
  bind_rows(dftemp) %>%
  mutate(year = year(mdy(summons_date)), 
         month = month(mdy(summons_date))) %>%
  filter(year %in% 2018:2022 & complete.cases(month, latitude, longitude)) 
rm(dftemp); gc()  


#### 2. Match summons to each hexagon ####-
sfgrid <- read_sf("../Data/Grid/Hexagonal grid.shp") 
dfsummons <- st_as_sf(dfsummons, coords = c("longitude","latitude"), crs = 4326) %>%
  st_join(sfgrid %>% select(id)) %>%
  st_set_geometry(NULL) %>% 
  as.data.frame() %>%
  filter(complete.cases(id)) %>%
  group_by(id, year, month) %>%
  summarise(summons = n()) 
#fwrite(dfsummons, "../Data/Crime summons month-hexagon data 2018-2022.csv")
#dfsummons <- fread("../Data/Crime summons month-hexagon data 2018-2022.csv", integer64 = "character")
rm(dfsummons, sfgrid); gc()





#######################################################################################################-
## Data processing: aggregate CRIMINAL SUMMONS data at the month-hexagon level (single hexagon) #######
#######################################################################################################-
#### 1. Import summons data #####-
#https://data.cityofnewyork.us/Public-Safety/NYPD-Criminal-Court-Summons-Historic-/sv2w-rv3k
#https://data.cityofnewyork.us/Public-Safety/NYPD-Criminal-Court-Summons-Incident-Level-Data-Ye/mv4k-y93f
dftemp <- fread("../Data/New York/NYPD_Criminal_Court_Summons_Incident_Level_Data__Year_To_Date_.csv", 
                select = c("SUMMONS_DATE", "Latitude", "Longitude")) %>%
  rename_all(tolower) 
dfsummons <- fread("../Data/New York/NYPD_Criminal_Court_Summons__Historic_.csv", 
                   select = c("SUMMONS_DATE", "Latitude", "Longitude")) %>%
  rename_all(tolower) %>%
  bind_rows(dftemp) %>%
  mutate(year = year(mdy(summons_date)), 
         month = month(mdy(summons_date))) %>%
  filter(year %in% 2018:2022 & complete.cases(month, latitude, longitude)) 
rm(dftemp); gc()  


#### 2. Match summons to each hexagon ####-
sfgrid <- read_sf("../Data/Grid/Hexagonal grid sites1.shp") 
dfsummons <- st_as_sf(dfsummons, coords = c("longitude","latitude"), crs = 4326) %>%
  st_join(sfgrid %>% select(id)) %>%
  st_set_geometry(NULL) %>% 
  as.data.frame() %>%
  filter(complete.cases(id)) %>%
  group_by(id, year, month) %>%
  summarise(summons = n()) 
#fwrite(dfsummons, "../Data/Crime summons month-hexagon1 data 2018-2022.csv")
#dfsummons <- fread("../Data/Crime summons month-hexagon1 data 2018-2022.csv", integer64 = "character")
rm(dfsummons, sfgrid); gc()




#######################################################################################################-
## Data processing: aggregate CRIMINAL SUMMONS data at the month-hexagon level (three hexagons) #######
#######################################################################################################-
#### 1. Import summons data #####-
#https://data.cityofnewyork.us/Public-Safety/NYPD-Criminal-Court-Summons-Historic-/sv2w-rv3k
#https://data.cityofnewyork.us/Public-Safety/NYPD-Criminal-Court-Summons-Incident-Level-Data-Ye/mv4k-y93f
dftemp <- fread("../Data/New York/NYPD_Criminal_Court_Summons_Incident_Level_Data__Year_To_Date_.csv", 
                select = c("SUMMONS_DATE", "OFFENSE_DESCRIPTION", "Latitude", "Longitude")) %>%
  rename_all(tolower) 
dfsummons <- fread("../Data/New York/NYPD_Criminal_Court_Summons__Historic_.csv", 
                   select = c("SUMMONS_DATE", "OFFENSE_DESCRIPTION", "Latitude", "Longitude")) %>%
  rename_all(tolower) %>%
  bind_rows(dftemp) %>%
  mutate(year = year(mdy(summons_date)), 
         month = month(mdy(summons_date))) %>%
  filter(year %in% 2018:2022 & complete.cases(month, latitude, longitude)) 
rm(dftemp); gc()  
dfsummons %>%
  group_by(offense_description) %>%
  summarise(n()) %>%
  View()

#### 2. Match summons to each hexagon ####-
sfgrid <- read_sf("../Data/Grid/Hexagonal grid sites2.shp") 
dfsummons <- st_as_sf(dfsummons, coords = c("longitude","latitude"), crs = 4326) %>%
  st_join(sfgrid %>% select(id)) %>%
  st_set_geometry(NULL) %>% 
  as.data.frame() %>%
  filter(complete.cases(id)) %>%
  group_by(id, year, month) %>%
  summarise(summons = n()) 
#fwrite(dfsummons, "../Data/Crime summons month-hexagon2 data 2018-2022.csv")
#dfsummons <- fread("../Data/Crime summons month-hexagon2 data 2018-2022.csv", integer64 = "character")
rm(dfsummons, sfgrid); gc()




#####################################################################################-
## Data processing: aggregate 311 calls at the month-hexagon level (all city) #######
#####################################################################################-
#Import the relevant files
vfiles <- list.files("../Data/New York/311_NYPD_data", pattern = "csv", full.names = TRUE)
dfservice <- vector(mode = "list", length = length(vfiles))
pb <- txtProgressBar(max = length(vfiles), style = 3) 
for(X in 1:length(vfiles)){
  dfservice[[X]] <- fread(vfiles[X], integer64 = "character")
  setTxtProgressBar(pb, X)
}
close(pb); rm(vfiles, pb, X); gc()
dfservice <- rbindlist(dfservice) %>%
  mutate(call311_homeless = ifelse(complaint.type %in% c("Homeless Person Assistance", "Encampment", 
                                                         "Homeless Street Condition", "Homeless Encampment"), 1, 0),
         call311_substance = ifelse(complaint.type %in% c("Drug Activity", "Drinking") |
                                      grepl("Syringe", descriptor), 1, 0),
         call311_dirty = ifelse(complaint.type %in% c("Dirty Condition", "Dirty Conditions", 
                                                      "Urinating in Public", "Rodent", "Graffiti", 
                                                      "Sanitation Condition") & !grepl("Syringe", descriptor), 1, 0),
         call311_abandoned = ifelse(complaint.type %in% c("Abandoned Vehicle"), 1, 0),
         call311_noise = ifelse(grepl("Noise", complaint.type), 1, 0))

#### 2. Match summons to each hexagon ####-
#Merge datapoints to tracts shapefile and transform it to a data frame (spatial merge by month to speed the process)
sfgrid <- read_sf("../Data/Grid/Hexagonal grid.shp") 
dftemp1 <- data.frame()
for(X in 1:12){
  dftemp2 <- st_as_sf(dfservice %>% filter(month == X), coords = c("longitude","latitude"), crs = 4326) %>%
    st_join(sfgrid %>% select(id)) %>%
    st_set_geometry(NULL) %>% 
    as.data.frame() %>%
    filter(complete.cases(id)) %>%
    group_by(id, year, month) %>%
    summarise_at(vars(call311_homeless:call311_noise), sum) 
  dftemp1 <- dftemp1 %>%
    bind_rows(dftemp2)
}
dfservice <- dftemp1
rm(dftemp1, dftemp2, X); gc()  
#fwrite(dfservice, "../Data/Crime 311calls month-hexagon data 2018-2022.csv")
#dfservice <- fread("../Data/Crime 311calls month-hexagon data 2018-2022.csv", integer64 = "character")
#dfservice %>% group_by(year, month, id) %>% summarise_at(vars(call311_homeless:call311_disorder), sum) %>% View()
rm(dfservice, sfgrid); gc()





#############################################################################################-
## Data processing: aggregate 311 calls at the month-hexagon level (single hexagon) #########
#############################################################################################-
#### Import data, keeping only the relevant observations #####-
#https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2010-to-Present/erm2-nwe9
keep <- TRUE
X <- 0
Z <- 1
vrows <- 500000
while(keep == TRUE){
  #Try to read the file by 250K lines, looking for an error for the last iteration
  dftemp <- tryCatch({
    fread("../Data/New York/311_Service_Requests_from_2010_to_Present.csv", 
          skip = X + 1, nrows = vrows, select = c(1, 2, 4, 6, 7, 39, 40),
          col.names = c("unique.key", "created.date", "agency", "complaint.type", "descriptor", "latitude", "longitude")) %>% 
      mutate(year = year(mdy_hms(created.date)), 
             month = month(mdy_hms(created.date))) %>%
      filter(year %in% 2018:2022) %>%
      filter(complete.cases(year, month, latitude, longitude)) %>%
      filter(complaint.type %in% c("Rodent", "Graffiti", "Dirty Condition", "Dirty Conditions",
                                   "Sanitation Condition", "Homeless Person Assistance") |
               agency %in% "NYPD") %>%
      filter(!complaint.type %in% c("Animal-Abuse", "Animal Abuse", "Non-Emergency Police Matter")) %>%
      select(unique.key, year, month, complaint.type, descriptor, latitude, longitude)
  }, error = function(x){
    FALSE
  }
  )
  print(paste0("Ready reading rows: ", scales::comma(X), "---", Z))
  #Test whether it imported data 
  if(class(dftemp)[1] %in% c("data.table", "data.frame")){
    #Test whether the data frame has observations
    if(nrow(dftemp) > 0){
      #Export file and modify the counters
      fwrite(dftemp, paste0("../Data/New York/311_NYPD_data/311_NYPD_data", Z, ".csv"))  
      X <- X + vrows
      Z <- Z + 1
    } else{
      #For empty data frames, skip to the next iteration and modify the counter
      X <- X + vrows
      next
    }
  } else{
    #Change to false to break the loop if it did not read any observations
    keep <- FALSE
  }
}
rm(dftemp, keep, vrows, X, Z); gc()

#Import the relevant files
vfiles <- list.files("../Data/New York/311_NYPD_data", pattern = "csv", full.names = TRUE)
dfservice <- vector(mode = "list", length = length(vfiles))
pb <- txtProgressBar(max = length(vfiles), style = 3) 
for(X in 1:length(vfiles)){
  dfservice[[X]] <- fread(vfiles[X], integer64 = "character")
  setTxtProgressBar(pb, X)
}
close(pb); rm(vfiles, pb, X); gc()
dfservice <- rbindlist(dfservice) %>%
  mutate(call311_homeless = ifelse(complaint.type %in% c("Homeless Person Assistance", "Encampment", 
                                                         "Homeless Street Condition", "Homeless Encampment"), 1, 0),
         call311_substance = ifelse(complaint.type %in% c("Drug Activity", "Drinking") |
                                      grepl("Syringe", descriptor), 1, 0),
         call311_dirty = ifelse(complaint.type %in% c("Dirty Condition", "Dirty Conditions", 
                                                      "Urinating in Public", "Rodent", "Graffiti", 
                                                      "Sanitation Condition") & !grepl("Syringe", descriptor), 1, 0),
         call311_abandoned = ifelse(complaint.type %in% c("Abandoned Vehicle"), 1, 0),
         call311_noise = ifelse(grepl("Noise", complaint.type), 1, 0))


#### 2. Match summons to each hexagon ####-
#Merge datapoints to tracts shapefile and transform it to a data frame (spatial merge by month to speed the process)
sfgrid <- read_sf("../Data/Grid/Hexagonal grid sites1.shp") 
dftemp1 <- data.frame()
for(X in 1:12){
  dftemp2 <- st_as_sf(dfservice %>% filter(month == X), coords = c("longitude","latitude"), crs = 4326) %>%
    st_join(sfgrid %>% select(id)) %>%
    st_set_geometry(NULL) %>% 
    as.data.frame() %>%
    filter(complete.cases(id)) %>%
    group_by(id, year, month) %>%
    summarise_at(vars(call311_homeless:call311_noise), sum) 
  dftemp1 <- dftemp1 %>%
    bind_rows(dftemp2)
}
dfservice <- dftemp1
rm(dftemp1, dftemp2, X); gc()  
#fwrite(dfservice, "../Data/Crime 311calls month-hexagon1 data 2018-2022.csv")
#dfservice <- fread("../Data/Crime 311calls month-hexagon1 data 2018-2022.csv", integer64 = "character")
#dfservice %>% group_by(year, month, id) %>% summarise_at(vars(call311_homeless:call311_disorder), sum) %>% View()
rm(dfservice, sfgrid); gc()





###########################################################################################-
## Data processing: aggregate 311 calls at the month-hexagon level (three hexagons) #######
###########################################################################################-
#Import the relevant files
vfiles <- list.files("../../Data/New York/311_NYPD_data", pattern = "csv", full.names = TRUE)
dfservice <- vector(mode = "list", length = length(vfiles))
pb <- txtProgressBar(max = length(vfiles), style = 3) 
for(X in 1:length(vfiles)){
  dfservice[[X]] <- fread(vfiles[X], integer64 = "character")
  setTxtProgressBar(pb, X)
}
close(pb); rm(vfiles, pb, X); gc()
dfservice <- rbindlist(dfservice) %>%
  mutate(call311_homeless = ifelse(complaint.type %in% c("Homeless Person Assistance", "Encampment", 
                                                         "Homeless Street Condition", "Homeless Encampment"), 1, 0),
         call311_substance = ifelse(complaint.type %in% c("Drug Activity", "Drinking") |
                                      grepl("Syringe", descriptor), 1, 0),
         call311_dirty = ifelse(complaint.type %in% c("Dirty Condition", "Dirty Conditions", 
                                                      "Urinating in Public", "Rodent", "Graffiti", 
                                                      "Sanitation Condition") & !grepl("Syringe", descriptor), 1, 0),
         call311_abandoned = ifelse(complaint.type %in% c("Abandoned Vehicle"), 1, 0),
         call311_noise = ifelse(grepl("Noise", complaint.type), 1, 0))


#### 2. Match summons to each hexagon ####-
#Merge datapoints to tracts shapefile and transform it to a data frame (spatial merge by month to speed the process)
sfgrid <- read_sf("../Data/Grid/Hexagonal grid sites2.shp") 
dftemp1 <- data.frame()
for(X in 1:12){
  dftemp2 <- st_as_sf(dfservice %>% filter(month == X), coords = c("longitude","latitude"), crs = 4326) %>%
    st_join(sfgrid %>% select(id)) %>%
    st_set_geometry(NULL) %>% 
    as.data.frame() %>%
    filter(complete.cases(id)) %>%
    group_by(id, year, month) %>%
    summarise_at(vars(call311_homeless:call311_noise), sum) 
  dftemp1 <- dftemp1 %>%
    bind_rows(dftemp2)
}
dfservice <- dftemp1
rm(dftemp1, dftemp2, X); gc()  
#fwrite(dfservice, "../Data/Crime 311calls month-hexagon2 data 2018-2022.csv")
#dfservice <- fread("../Data/Crime 311calls month-hexagon2 data 2018-2022.csv", integer64 = "character")
rm(dfservice, sfgrid); gc()





###########################################################################-
## Data processing: Master file month-hexagon level data (all city) #######
###########################################################################-
#### 1. Import and merge dataset #####-
dfcrime <- fread("../Data/Crime crime month-hexagon data 2018-2022.csv") %>%
  mutate(crime_violentall = crime_violent + crime_lowassault, 
         crime_allassault = crime_assault + crime_lowassault)
dfarrest <- fread("../Data/Crime arrest month-hexagon data 2018-2022.csv")
dfcalls <- fread("../Data/Crime calls month-hexagon data 2018-2022.csv")
dfsummons <- fread("../Data/Crime summons month-hexagon data 2018-2022.csv")
dfservice <- fread("../Data/Crime 311calls month-hexagon data 2018-2022.csv")
sfgrid <- read_sf("../Data/Grid/Hexagonal grid.shp") %>% 
  st_set_geometry(NULL)
df <- expand.grid(year = 2018:2022, month = 1:12, id = unique(sfgrid$id)) %>%
  left_join(sfgrid, by = "id") %>%
  left_join(dfcrime, by = c("year", "month", "id")) %>%
  left_join(dfarrest, by = c("year", "month", "id")) %>%
  left_join(dfcalls, by = c("year", "month", "id")) %>%
  left_join(dfsummons, by = c("year", "month", "id")) %>%
  left_join(dfservice, by = c("year", "month", "id")) %>%
  mutate_at(vars(crime_murder:call311_noise), ~replace_na(., 0)) %>%
  mutate(call_qol = call311_substance + call311_dirty + call311_abandoned + call311_noise + call311_homeless + call_trespass) 
#fwrite(df, "../Data/Crime month-hexagon data 2018-2022.csv")
#df <- fread("../Data/Crime month-hexagon data 2018-2022.csv", integer64 = "character")
rm(df, dfcalls, dfcrime, dfarrest, dfsummons, dfservice, sfgrid); gc()






###################################################################################-
## Data processing: Master file month-hexagon level data (single hexagon) #########
###################################################################################-
#### 1. Import and merge dataset #####-
dfcrime <- fread("../Data/Crime crime month-hexagon1 data 2018-2022.csv") %>%
  mutate(crime_violentall = crime_violent + crime_lowassault, 
         crime_allassault = crime_assault + crime_lowassault)
dfarrest <- fread("../Data/Crime arrest month-hexagon1 data 2018-2022.csv")
dfcalls <- fread("../Data/Crime calls month-hexagon1 data 2018-2022.csv")
dfsummons <- fread("../Data/Crime summons month-hexagon1 data 2018-2022.csv")
dfservice <- fread("../Data/Crime 311calls month-hexagon1 data 2018-2022.csv")
sfgrid <- read_sf("../Data/Grid/Hexagonal grid sites1.shp") %>% 
  st_set_geometry(NULL)
df <- expand.grid(year = 2018:2022, month = 1:12, id = unique(sfgrid$id)) %>%
  left_join(sfgrid, by = "id") %>%
  left_join(dfcrime, by = c("year", "month", "id")) %>%
  left_join(dfarrest, by = c("year", "month", "id")) %>%
  left_join(dfcalls, by = c("year", "month", "id")) %>%
  left_join(dfsummons, by = c("year", "month", "id")) %>%
  left_join(dfservice, by = c("year", "month", "id")) %>%
  mutate_at(vars(crime_murder:call311_noise), ~replace_na(., 0)) %>%
  mutate(call_qol = call311_substance + call311_dirty + call311_abandoned + call311_noise + call311_homeless + call_trespass) 
#fwrite(df, "../Data/Crime month-hexagon1 data 2018-2022.csv")
#df <- fread("../Data/Crime month-hexagon1 data 2018-2022.csv", integer64 = "character")
rm(df, dfarrest, dfcalls, dfcrime, dfsummons, dfservice, sfgrid); gc()








###################################################################################-
## Data processing: Master file month-hexagon level data (three hexagons) #########
###################################################################################-
#### 1. Import and merge dataset #####-
dfcrime <- fread("../Data/Crime crime month-hexagon2 data 2018-2022.csv") %>%
  mutate(crime_violentall = crime_violent + crime_lowassault, 
         crime_allassault = crime_assault + crime_lowassault)
dfarrest <- fread("../Data/Crime arrest month-hexagon2 data 2018-2022.csv")
dfcalls <- fread("../Data/Crime calls month-hexagon2 data 2018-2022.csv")
dfservice <- fread("../Data/Crime 311calls month-hexagon2 data 2018-2022.csv")
dfsummons <- fread("../Data/Crime summons month-hexagon2 data 2018-2022.csv")
sfgrid <- read_sf("../Data/Grid/Hexagonal grid sites2.shp") %>% 
  st_set_geometry(NULL)
df <- expand.grid(year = 2018:2022, month = 1:12, id = unique(sfgrid$id)) %>%
  left_join(sfgrid, by = "id") %>%
  left_join(dfcrime, by = c("year", "month", "id")) %>%
  left_join(dfarrest, by = c("year", "month", "id")) %>%
  left_join(dfcalls, by = c("year", "month", "id")) %>%
  left_join(dfsummons, by = c("year", "month", "id")) %>%
  left_join(dfservice, by = c("year", "month", "id")) %>%
  mutate_at(vars(crime_murder:call311_noise), ~replace_na(., 0)) %>%
  mutate(call_qol = call311_substance + call311_dirty + call311_abandoned + call311_noise + call311_homeless + call_trespass) 
#fwrite(df, "../Data/Crime month-hexagon2 data 2018-2022.csv")
#df <- fread("../Data/Crime month-hexagon2 data 2018-2022.csv", integer64 = "character")
rm(df, dfarrest, dfcalls, dfcrime, dfsummons, dfservice, sfgrid); gc()





###############################################################################-
## Data processing: Master file month-hexagon level data (hexagon-city) #######
###############################################################################-
#Identify precincts by crime rates (remove the 22th  precinct as it is central park)
#https://johnkeefe.net/nyc-police-precinct-and-census-data
dfpop <- fread("../Data/nyc_precinct_2020pop.csv", integer64 = "character") %>%
  rename(population = P1_001N) %>%
  select(precinct, population)
dfprecinct <- fread("../Data/Crime month-hexagon data 2018-2022.csv", integer64 = "character") %>%
  group_by(precinct) %>%
  filter(year %in% 2019:2020) %>%
  summarise(crime_index2 = sum(crime_index - crime_theft), 
            crime_index = sum(crime_index)) %>%
  left_join(dfpop, by = "precinct") %>%
  mutate(crime_index2 = crime_index2/population, 
         crime_index = crime_index/population) %>%
  filter(precinct != 22) %>%
  #arrange(desc(crime_index)) %>%
  ungroup() %>%
  mutate(rank2 = rank(-crime_index2), 
         rank = rank(-crime_index)) %>%
  select(precinct, rank, rank2) 

#Import treated hexagon data
dfhex1 <- fread("../Data/Crime month-hexagon1 data 2018-2022.csv", integer64 = "character") %>%
  filter(direct == 1 & site == "OnPoint NYC") %>%
  select(-direct, -neighbor) %>%
  mutate(hexsample = 1)
dfhex2 <- fread("../Data/Crime month-hexagon2 data 2018-2022.csv", integer64 = "character") %>%
  filter(direct == 1 & site == "OnPoint NYC") %>%
  select(-direct, -neighbor) %>%
  mutate(hexsample = 3)

#Import NYC hexagon excluding the treated precincts (25 and 34); no repeated id after rbinding the data
df <- fread("../Data/Crime month-hexagon data 2018-2022.csv", integer64 = "character") %>%
  filter(!precinct %in% c(25, 34)) %>%
  mutate(idsite = 0, 
         site = "") %>%
  bind_rows(dfhex1) %>%
  bind_rows(dfhex2) %>%
  left_join(dfprecinct, by = "precinct")
#fwrite(df, "../Data/Crime month-hexagon city data 2018-2022.csv")
#df <- fread("../Data/Crime month-hexagon city data 2018-2022.csv", integer64 = "character")
#df %>% distinct(id, precinct, idsite, site) %>% View()
rm(df, dfhex1, dfhex2, dfprecinct, dfpop); gc()








################################################################################################-
## Regression: Event-study design Poisson crime, arrest, call, summons (single hexagon) ########
################################################################################################-
#Import data and create time dummy variables for the event study and individual time trends
#Removed 2018 because it is a noisy year
#Covid indicator variable: start of lockdown 03/2020 and vaccine approval for +16 y old 08/2021
df <- fread("../Data/Crime month-hexagon1 data 2018-2022.csv", integer64 = "character") %>%
  filter(direct == 1 & year %in% 2019:2022) %>%
  mutate(treated = ifelse(site %in% "OnPoint NYC", 1, 0)) %>%
  mutate(datenum = year + (month - 1) /12, 
         rel_months = round((datenum - (2021+(12-1)/12))*12)) %>%
  mutate(treat_09 = ifelse(rel_months %in% -33:-36, 1, 0)*treated,
         treat_08 = ifelse(rel_months %in% -29:-32, 1, 0)*treated,
         treat_07 = ifelse(rel_months %in% -25:-28, 1, 0)*treated,
         treat_06 = ifelse(rel_months %in% -21:-24, 1, 0)*treated,
         treat_05 = ifelse(rel_months %in% -17:-20, 1, 0)*treated,
         treat_04 = ifelse(rel_months %in% -13:-16, 1, 0)*treated,
         treat_03 = ifelse(rel_months %in% -9:-12, 1, 0)*treated,
         treat_02 = ifelse(rel_months %in% -5:-8, 1, 0)*treated,
         treat_01 = ifelse(rel_months %in% -1:-4, 1, 0)*treated,
         treat01 = ifelse(rel_months %in% 1:4, 1, 0)*treated,
         treat02 = ifelse(rel_months %in% 5:8, 1, 0)*treated, 
         treat03 = ifelse(rel_months %in% 9:12, 1, 0)*treated) 

#Regression
dfest <- data.frame("outcome" = character(), "model" = character(), "time" = numeric(), 
                    "est" = numeric(), "se" = numeric(), "irr" = numeric(), "irr.se" = numeric(),
                    "irr.low" = numeric(), "irr.up" = numeric(), "n" = numeric(), stringsAsFactors = FALSE)
vnames <- df %>% select(crime_assault:crime_allassault, arrest_weapon, arrest_drugs, call_crime:call_qol) %>% names()
vtreat <- c(sort(paste0("treat_", str_pad(2:9, 2, "left", "0")), decreasing = TRUE), 
            paste0("treat", str_pad(1:3, 2, "left", "0")))
for(X in vnames){
  for(W in c("month_id_fe")){
    #Model
    if(W == "month_id_fe"){
      #Month-year and individual FE
      f1 <- as.formula(paste0(paste0(X, " ~ ", paste0(vtreat, collapse = " + "), "+ factor(datenum) + factor(id)")))
      lm1 <- fepois(f1, cluster = "id", data = df)  
    } else if(W == "") {
      break
    } 
    #Extract coefficient and SE
    dfest <- dfest %>%
      bind_rows(data.frame("outcome" = X, 
                           "model" = W, 
                           "time" = c(-9:-2, 0:2),
                           "est" = lm1$coefficients[grepl("treat", names(lm1$coefficients))], 
                           "se" = lm1$se[grepl("treat", names(lm1$coefficients))], 
                           "irr" = exp(lm1$coefficients[grepl("treat", names(lm1$coefficients))]), 
                           "irr.se" = (exp(lm1$coefficients)*sqrt(diag(vcov(lm1))))[grepl("treat", names(lm1$coefficients))], 
                           "irr.low" = exp(lm1$coefficients - 1.96*sqrt(diag(vcov(lm1))))[grepl("treat", names(lm1$coefficients))],
                           "irr.up" = exp(lm1$coefficients + 1.96*sqrt(diag(vcov(lm1))))[grepl("treat", names(lm1$coefficients))],
                           "n" = lm1$nobs))  
    
    print(paste0("Ready loop: ", W, "--", X)) 
  }
}
#fwrite(dfest, "../Data/Event-study Poisson public_safety1.csv")
#dfest <- fread("../Data/Event-study Poisson public_safety1.csv")
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions






##############################################################################################-
## Regression: Event-study design Poisson crime, arrest, call, summons (three hexagons) ######
##############################################################################################-
#Import data and create time dummy variables for the event study and individual time trends
#Removed 2018 because it is a noisy year
df <- fread("../Data/Crime month-hexagon2 data 2018-2022.csv", integer64 = "character") %>%
  filter(direct == 1 & year %in% 2019:2022) %>%
  mutate(treated = ifelse(site %in% "OnPoint NYC", 1, 0)) %>%
  mutate(datenum = year + (month - 1) /12, 
         rel_months = round((datenum - (2021+(12-1)/12))*12)) %>%
  mutate(treat_09 = ifelse(rel_months %in% -33:-36, 1, 0)*treated,
         treat_08 = ifelse(rel_months %in% -29:-32, 1, 0)*treated,
         treat_07 = ifelse(rel_months %in% -25:-28, 1, 0)*treated,
         treat_06 = ifelse(rel_months %in% -21:-24, 1, 0)*treated,
         treat_05 = ifelse(rel_months %in% -17:-20, 1, 0)*treated,
         treat_04 = ifelse(rel_months %in% -13:-16, 1, 0)*treated,
         treat_03 = ifelse(rel_months %in% -9:-12, 1, 0)*treated,
         treat_02 = ifelse(rel_months %in% -5:-8, 1, 0)*treated,
         treat_01 = ifelse(rel_months %in% -1:-4, 1, 0)*treated,
         treat01 = ifelse(rel_months %in% 1:4, 1, 0)*treated,
         treat02 = ifelse(rel_months %in% 5:8, 1, 0)*treated, 
         treat03 = ifelse(rel_months %in% 9:12, 1, 0)*treated) 

#Regression
dfest <- data.frame("outcome" = character(), "model" = character(), "time" = numeric(), 
                    "est" = numeric(), "se" = numeric(), "irr" = numeric(), "irr.se" = numeric(),
                    "irr.low" = numeric(), "irr.up" = numeric(), "n" = numeric(), stringsAsFactors = FALSE)
vnames <- df %>% select(crime_assault:crime_allassault, arrest_weapon, arrest_drugs, call_crime:call_qol) %>% names()
vtreat <- c(sort(paste0("treat_", str_pad(2:9, 2, "left", "0")), decreasing = TRUE), 
            paste0("treat", str_pad(1:3, 2, "left", "0")))
for(X in vnames){
  for(W in c("month_id_fe")){
    #Model
    if(W == "month_id_fe"){
      #Month-year and individual FE
      f1 <- as.formula(paste0(paste0(X, " ~ ", paste0(vtreat, collapse = " + "), "+ factor(datenum) + factor(id)")))
      lm1 <- fepois(f1, cluster = "id", data = df)  
    } else if(W == "") {
      break
    } 
    #Extract coefficient and SE
    dfest <- dfest %>%
      bind_rows(data.frame("outcome" = X, 
                           "model" = W, 
                           "time" = c(-9:-2, 0:2),
                           "est" = lm1$coefficients[grepl("treat", names(lm1$coefficients))], 
                           "se" = lm1$se[grepl("treat", names(lm1$coefficients))], 
                           "irr" = exp(lm1$coefficients[grepl("treat", names(lm1$coefficients))]), 
                           "irr.se" = (exp(lm1$coefficients)*sqrt(diag(vcov(lm1))))[grepl("treat", names(lm1$coefficients))], 
                           "irr.low" = exp(lm1$coefficients - 1.96*sqrt(diag(vcov(lm1))))[grepl("treat", names(lm1$coefficients))],
                           "irr.up" = exp(lm1$coefficients + 1.96*sqrt(diag(vcov(lm1))))[grepl("treat", names(lm1$coefficients))],
                           "n" = lm1$nobs))  
    
    print(paste0("Ready loop: ", W, "--", X)) 
  }
}
#fwrite(dfest, "../Data/Event-study Poisson public_safety2.csv")
#dfest <- fread("../Data/Event-study Poisson public_safety2.csv")
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions







#############################################################################################################-
## Regression: Event-study design Poisson crime, arrest, call, summons (single hexagon-high crime city) #####
#############################################################################################################-
#Import data and create time dummy variables for the event study and individual time trends
#Removed 2018 because it is a noisy year
df <- fread("../Data/Crime month-hexagon city data 2018-2022.csv", integer64 = "character") %>%
  filter(year %in% 2019:2022) %>%
  filter(hexsample == 1 | is.na(hexsample)) %>%
  mutate(treated = ifelse(site %in% "OnPoint NYC", 1, 0), 
         datenum = year + (month - 1) /12, 
         post = ifelse(datenum >= 2021.91, 1, 0), 
         post_treated = treated*post) %>%
  mutate(rel_months = round((datenum - (2021+(12-1)/12))*12)) %>%
  mutate(treat_09 = ifelse(rel_months %in% -33:-36, 1, 0)*treated,
         treat_08 = ifelse(rel_months %in% -29:-32, 1, 0)*treated,
         treat_07 = ifelse(rel_months %in% -25:-28, 1, 0)*treated,
         treat_06 = ifelse(rel_months %in% -21:-24, 1, 0)*treated,
         treat_05 = ifelse(rel_months %in% -17:-20, 1, 0)*treated,
         treat_04 = ifelse(rel_months %in% -13:-16, 1, 0)*treated,
         treat_03 = ifelse(rel_months %in% -9:-12, 1, 0)*treated,
         treat_02 = ifelse(rel_months %in% -5:-8, 1, 0)*treated,
         treat_01 = ifelse(rel_months %in% -1:-4, 1, 0)*treated,
         treat01 = ifelse(rel_months %in% 1:4, 1, 0)*treated,
         treat02 = ifelse(rel_months %in% 5:8, 1, 0)*treated, 
         treat03 = ifelse(rel_months %in% 9:12, 1, 0)*treated) 
#Identifying the 250 unsafest hexagons in the top 10 unsafe precincts (excluding the treated ones and Times Squares)
vids <- df %>%
  filter(post == 0) %>%
  group_by(precinct, treated, rank2, id) %>%
  summarise(crime_index2 = sum(crime_index - crime_theft)) %>%
  ungroup() %>%
  filter(!precinct %in% c(14,25,34)) %>%
  filter(rank2 %in% 1:9) %>%
  mutate(rankhex2 = min_rank(-crime_index2)) %>%
  filter(rankhex2 <= 250) %>%
  pull(id)
df <- df %>% 
  filter(id %in% vids | treated == 1)

#Regression
dfest <- data.frame("outcome" = character(), "model" = character(), "time" = numeric(), 
                    "est" = numeric(), "se" = numeric(), "irr" = numeric(), "irr.se" = numeric(),
                    "irr.low" = numeric(), "irr.up" = numeric(), "n" = numeric(), stringsAsFactors = FALSE)
vnames <- df %>% select(crime_assault:crime_allassault, arrest_weapon, arrest_drugs, call_crime:call_qol) %>% names()
vtreat <- c(sort(paste0("treat_", str_pad(2:9, 2, "left", "0")), decreasing = TRUE), 
            paste0("treat", str_pad(1:3, 2, "left", "0")))
for(X in vnames){
  for(W in c("month_id_fe")){
    #Model
    if(W == "month_id_fe"){
      #Month-year and individual FE
      f1 <- as.formula(paste0(paste0(X, " ~ ", paste0(vtreat, collapse = " + "), "+ factor(datenum) + factor(id)")))
      lm1 <- fepois(f1, cluster = "id", data = df)  
    } else if(W == "") {
      break
    } 
    #Extract coefficient and SE
    dfest <- dfest %>%
      bind_rows(data.frame("outcome" = X, 
                           "model" = W, 
                           "time" = c(-9:-2, 0:2),
                           "est" = lm1$coefficients[grepl("treat", names(lm1$coefficients))], 
                           "se" = lm1$se[grepl("treat", names(lm1$coefficients))], 
                           "irr" = exp(lm1$coefficients[grepl("treat", names(lm1$coefficients))]), 
                           "irr.se" = (exp(lm1$coefficients)*sqrt(diag(vcov(lm1))))[grepl("treat", names(lm1$coefficients))], 
                           "irr.low" = exp(lm1$coefficients - 1.96*sqrt(diag(vcov(lm1))))[grepl("treat", names(lm1$coefficients))],
                           "irr.up" = exp(lm1$coefficients + 1.96*sqrt(diag(vcov(lm1))))[grepl("treat", names(lm1$coefficients))],
                           "n" = lm1$nobs))  
    
    print(paste0("Ready loop: ", W, "--", X)) 
  }
}
#fwrite(dfest, "../Data/Event-study Poisson highcrimecity public_safety1.csv")
#dfest <- fread("../Data/Event-study Poisson highcrimecity public_safety1.csv")
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions





#############################################################################################################-
## Regression: Event-study design Poisson crime, arrest, call, summons (three hexagons-high crime city) #####
#############################################################################################################-
#Import data and create time dummy variables for the event study and individual time trends
#Removed 2018 because it is a noisy year
df <- fread("../Data/Crime month-hexagon city data 2018-2022.csv", integer64 = "character") %>%
  filter(year %in% 2019:2022) %>%
  filter(hexsample == 3 | is.na(hexsample)) %>%
  mutate(treated = ifelse(site %in% "OnPoint NYC", 1, 0), 
         datenum = year + (month - 1) /12, 
         post = ifelse(datenum >= 2021.91, 1, 0), 
         post_treated = treated*post) %>%
  mutate(rel_months = round((datenum - (2021+(12-1)/12))*12)) %>%
  mutate(treat_09 = ifelse(rel_months %in% -33:-36, 1, 0)*treated,
         treat_08 = ifelse(rel_months %in% -29:-32, 1, 0)*treated,
         treat_07 = ifelse(rel_months %in% -25:-28, 1, 0)*treated,
         treat_06 = ifelse(rel_months %in% -21:-24, 1, 0)*treated,
         treat_05 = ifelse(rel_months %in% -17:-20, 1, 0)*treated,
         treat_04 = ifelse(rel_months %in% -13:-16, 1, 0)*treated,
         treat_03 = ifelse(rel_months %in% -9:-12, 1, 0)*treated,
         treat_02 = ifelse(rel_months %in% -5:-8, 1, 0)*treated,
         treat_01 = ifelse(rel_months %in% -1:-4, 1, 0)*treated,
         treat01 = ifelse(rel_months %in% 1:4, 1, 0)*treated,
         treat02 = ifelse(rel_months %in% 5:8, 1, 0)*treated, 
         treat03 = ifelse(rel_months %in% 9:12, 1, 0)*treated) 
#Identifying the 250 unsafest hexagons in the top 10 unsafe precincts (excluding the treated ones and Times Squares)
vids <- df %>%
  filter(post == 0) %>%
  group_by(precinct, treated, rank2, id) %>%
  summarise(crime_index2 = sum(crime_index - crime_theft)) %>%
  ungroup() %>%
  filter(!precinct %in% c(14,25,34)) %>%
  filter(rank2 %in% 1:9) %>%
  mutate(rankhex2 = min_rank(-crime_index2)) %>%
  filter(rankhex2 <= 250) %>%
  pull(id)
df <- df %>% 
  filter(id %in% vids | treated == 1)

#Regression
dfest <- data.frame("outcome" = character(), "model" = character(), "time" = numeric(), 
                    "est" = numeric(), "se" = numeric(), "irr" = numeric(), "irr.se" = numeric(),
                    "irr.low" = numeric(), "irr.up" = numeric(), "n" = numeric(), stringsAsFactors = FALSE)
vnames <- df %>% select(crime_assault:crime_allassault, arrest_weapon, arrest_drugs, call_crime:call_qol) %>% names()
vtreat <- c(sort(paste0("treat_", str_pad(2:9, 2, "left", "0")), decreasing = TRUE), 
            paste0("treat", str_pad(1:3, 2, "left", "0")))
for(X in vnames){
  for(W in c("month_id_fe")){
    #Model
    if(W == "month_id_fe"){
      #Month-year and individual FE
      f1 <- as.formula(paste0(paste0(X, " ~ ", paste0(vtreat, collapse = " + "), "+ factor(datenum) + factor(id)")))
      lm1 <- fepois(f1, cluster = "id", data = df)  
    } else if(W == "") {
      break
    } 
    #Extract coefficient and SE
    dfest <- dfest %>%
      bind_rows(data.frame("outcome" = X, 
                           "model" = W, 
                           "time" = c(-9:-2, 0:2),
                           "est" = lm1$coefficients[grepl("treat", names(lm1$coefficients))], 
                           "se" = lm1$se[grepl("treat", names(lm1$coefficients))], 
                           "irr" = exp(lm1$coefficients[grepl("treat", names(lm1$coefficients))]), 
                           "irr.se" = (exp(lm1$coefficients)*sqrt(diag(vcov(lm1))))[grepl("treat", names(lm1$coefficients))], 
                           "irr.low" = exp(lm1$coefficients - 1.96*sqrt(diag(vcov(lm1))))[grepl("treat", names(lm1$coefficients))],
                           "irr.up" = exp(lm1$coefficients + 1.96*sqrt(diag(vcov(lm1))))[grepl("treat", names(lm1$coefficients))],
                           "n" = lm1$nobs))  
    
    print(paste0("Ready loop: ", W, "--", X)) 
  }
}
#fwrite(dfest, "../Data/Event-study Poisson highcrimecity public_safety2.csv")
#dfest <- fread("../Data/Event-study Poisson highcrimecity public_safety2.csv")
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions








###############################################################################################################-
## Regression: Event-study design Poisson crime, arrest, call, summons (single hexagon-high drug arrests) #####
###############################################################################################################-
#Import data and create time dummy variables for the event study and individual time trends
#Removed 2018 because it is a noisy year
df <- fread("../Data/Crime month-hexagon city data 2018-2022.csv", integer64 = "character") %>%
  filter(year %in% 2019:2022) %>%
  filter(hexsample == 1 | is.na(hexsample)) %>%
  mutate(treated = ifelse(site %in% "OnPoint NYC", 1, 0), 
         datenum = year + (month - 1) /12, 
         post = ifelse(datenum >= 2021.91, 1, 0), 
         post_treated = treated*post) %>%
  mutate(rel_months = round((datenum - (2021+(12-1)/12))*12)) %>%
  mutate(treat_09 = ifelse(rel_months %in% -33:-36, 1, 0)*treated,
         treat_08 = ifelse(rel_months %in% -29:-32, 1, 0)*treated,
         treat_07 = ifelse(rel_months %in% -25:-28, 1, 0)*treated,
         treat_06 = ifelse(rel_months %in% -21:-24, 1, 0)*treated,
         treat_05 = ifelse(rel_months %in% -17:-20, 1, 0)*treated,
         treat_04 = ifelse(rel_months %in% -13:-16, 1, 0)*treated,
         treat_03 = ifelse(rel_months %in% -9:-12, 1, 0)*treated,
         treat_02 = ifelse(rel_months %in% -5:-8, 1, 0)*treated,
         treat_01 = ifelse(rel_months %in% -1:-4, 1, 0)*treated,
         treat01 = ifelse(rel_months %in% 1:4, 1, 0)*treated,
         treat02 = ifelse(rel_months %in% 5:8, 1, 0)*treated, 
         treat03 = ifelse(rel_months %in% 9:12, 1, 0)*treated) 
#Identifying the 20 hexagons with most pre-intervention drug arrests 
#(excluding central bookin/hospital areas)
vids <- df %>%
  filter(post == 0) %>%
  filter(!id %in% c(11435, 12630)) %>%
  group_by(precinct, treated, rank2, id) %>%
  summarise(arrest_drugs = sum(arrest_drugs)) %>%
  ungroup() %>%
  filter(!precinct %in% c(14,25,34)) %>%
  mutate(rankhex2 = min_rank(-arrest_drugs)) %>%
  filter(rankhex2 <= 20) %>%
  pull(id)
df <- df %>% 
  filter(id %in% vids | treated == 1)

#Regression
dfest <- data.frame("outcome" = character(), "model" = character(), "time" = numeric(), 
                    "est" = numeric(), "se" = numeric(), "irr" = numeric(), "irr.se" = numeric(),
                    "irr.low" = numeric(), "irr.up" = numeric(), "n" = numeric(), stringsAsFactors = FALSE)
vnames <- df %>% select(crime_assault:crime_allassault, arrest_weapon, arrest_drugs, call_crime:call_qol) %>% names()
vtreat <- c(sort(paste0("treat_", str_pad(2:9, 2, "left", "0")), decreasing = TRUE), 
            paste0("treat", str_pad(1:3, 2, "left", "0")))
for(X in vnames){
  for(W in c("month_id_fe")){
    #Model
    if(W == "month_id_fe"){
      #Month-year and individual FE
      f1 <- as.formula(paste0(paste0(X, " ~ ", paste0(vtreat, collapse = " + "), "+ factor(datenum) + factor(id)")))
      lm1 <- fepois(f1, cluster = "id", data = df)  
    } else if(W == "") {
      break
    } 
    #Extract coefficient and SE
    dfest <- dfest %>%
      bind_rows(data.frame("outcome" = X, 
                           "model" = W, 
                           "time" = c(-9:-2, 0:2),
                           "est" = lm1$coefficients[grepl("treat", names(lm1$coefficients))], 
                           "se" = lm1$se[grepl("treat", names(lm1$coefficients))], 
                           "irr" = exp(lm1$coefficients[grepl("treat", names(lm1$coefficients))]), 
                           "irr.se" = (exp(lm1$coefficients)*sqrt(diag(vcov(lm1))))[grepl("treat", names(lm1$coefficients))], 
                           "irr.low" = exp(lm1$coefficients - 1.96*sqrt(diag(vcov(lm1))))[grepl("treat", names(lm1$coefficients))],
                           "irr.up" = exp(lm1$coefficients + 1.96*sqrt(diag(vcov(lm1))))[grepl("treat", names(lm1$coefficients))],
                           "n" = lm1$nobs))  
    
    print(paste0("Ready loop: ", W, "--", X)) 
  }
}
#fwrite(dfest, "../Data/Event-study Poisson highdrugarrests public_safety1.csv")
#dfest <- fread("../Data/Event-study Poisson highdrugarrests public_safety1.csv")
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions




###############################################################################################################-
## Regression: Event-study design Poisson crime, arrest, call, summons (single hexagon-high drug arrests) #####
###############################################################################################################-
#Import data and create time dummy variables for the event study and individual time trends
#Removed 2018 because it is a noisy year
df <- fread("../Data/Crime month-hexagon city data 2018-2022.csv", integer64 = "character") %>%
  filter(year %in% 2019:2022) %>%
  filter(hexsample == 3 | is.na(hexsample)) %>%
  mutate(treated = ifelse(site %in% "OnPoint NYC", 1, 0), 
         datenum = year + (month - 1) /12, 
         post = ifelse(datenum >= 2021.91, 1, 0), 
         post_treated = treated*post) %>%
  mutate(rel_months = round((datenum - (2021+(12-1)/12))*12)) %>%
  mutate(treat_09 = ifelse(rel_months %in% -33:-36, 1, 0)*treated,
         treat_08 = ifelse(rel_months %in% -29:-32, 1, 0)*treated,
         treat_07 = ifelse(rel_months %in% -25:-28, 1, 0)*treated,
         treat_06 = ifelse(rel_months %in% -21:-24, 1, 0)*treated,
         treat_05 = ifelse(rel_months %in% -17:-20, 1, 0)*treated,
         treat_04 = ifelse(rel_months %in% -13:-16, 1, 0)*treated,
         treat_03 = ifelse(rel_months %in% -9:-12, 1, 0)*treated,
         treat_02 = ifelse(rel_months %in% -5:-8, 1, 0)*treated,
         treat_01 = ifelse(rel_months %in% -1:-4, 1, 0)*treated,
         treat01 = ifelse(rel_months %in% 1:4, 1, 0)*treated,
         treat02 = ifelse(rel_months %in% 5:8, 1, 0)*treated, 
         treat03 = ifelse(rel_months %in% 9:12, 1, 0)*treated) 
#Identifying the 20 hexagons with most pre-intervention drug arrests
vids <- df %>%
  filter(post == 0) %>%
  filter(!id %in% c(11435, 12630)) %>%
  group_by(precinct, treated, rank2, id) %>%
  summarise(arrest_drugs = sum(arrest_drugs)) %>%
  ungroup() %>%
  filter(!precinct %in% c(14,25,34)) %>%
  mutate(rankhex2 = min_rank(-arrest_drugs)) %>%
  filter(rankhex2 <= 20) %>%
  pull(id)
df <- df %>% 
  filter(id %in% vids | treated == 1)

#Regression
dfest <- data.frame("outcome" = character(), "model" = character(), "time" = numeric(), 
                    "est" = numeric(), "se" = numeric(), "irr" = numeric(), "irr.se" = numeric(),
                    "irr.low" = numeric(), "irr.up" = numeric(), "n" = numeric(), stringsAsFactors = FALSE)
vnames <- df %>% select(crime_assault:crime_allassault, arrest_weapon, arrest_drugs, call_crime:call_qol) %>% names()
vtreat <- c(sort(paste0("treat_", str_pad(2:9, 2, "left", "0")), decreasing = TRUE), 
            paste0("treat", str_pad(1:3, 2, "left", "0")))
for(X in vnames){
  for(W in c("month_id_fe")){
    #Model
    if(W == "month_id_fe"){
      #Month-year and individual FE
      f1 <- as.formula(paste0(paste0(X, " ~ ", paste0(vtreat, collapse = " + "), "+ factor(datenum) + factor(id)")))
      lm1 <- fepois(f1, cluster = "id", data = df)  
    } else if(W == "") {
      break
    } 
    #Extract coefficient and SE
    dfest <- dfest %>%
      bind_rows(data.frame("outcome" = X, 
                           "model" = W, 
                           "time" = c(-9:-2, 0:2),
                           "est" = lm1$coefficients[grepl("treat", names(lm1$coefficients))], 
                           "se" = lm1$se[grepl("treat", names(lm1$coefficients))], 
                           "irr" = exp(lm1$coefficients[grepl("treat", names(lm1$coefficients))]), 
                           "irr.se" = (exp(lm1$coefficients)*sqrt(diag(vcov(lm1))))[grepl("treat", names(lm1$coefficients))], 
                           "irr.low" = exp(lm1$coefficients - 1.96*sqrt(diag(vcov(lm1))))[grepl("treat", names(lm1$coefficients))],
                           "irr.up" = exp(lm1$coefficients + 1.96*sqrt(diag(vcov(lm1))))[grepl("treat", names(lm1$coefficients))],
                           "n" = lm1$nobs))  
    
    print(paste0("Ready loop: ", W, "--", X)) 
  }
}
#fwrite(dfest, "../Data/Event-study Poisson highdrugarrests public_safety2.csv")
#dfest <- fread("../Data/Event-study Poisson highdrugarrests public_safety2.csv")
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions




#################################################################################################-
## Regression: Diff-in-diff design Poisson crime, arrest, call, summons (single hexagon) ########
#################################################################################################-
#Import data and create time dummy variables (removing 2018 because it is a noisy year)
df <- fread("../Data/Crime month-hexagon1 data 2018-2022.csv", integer64 = "character") %>%
  filter(direct == 1 & year %in% 2019:2022) %>%
  mutate(treated = ifelse(site %in% "OnPoint NYC", 1, 0), 
         datenum = year + (month - 1) /12, 
         post = ifelse(datenum >= 2021.91, 1, 0), 
         post_treated = treated*post) %>%
  mutate(rel_months = round((datenum - (2021+(12-1)/12))*12)) %>%
  mutate(treat_09 = ifelse(rel_months %in% -33:-36, 1, 0)*treated,
         treat_08 = ifelse(rel_months %in% -29:-32, 1, 0)*treated,
         treat_07 = ifelse(rel_months %in% -25:-28, 1, 0)*treated,
         treat_06 = ifelse(rel_months %in% -21:-24, 1, 0)*treated,
         treat_05 = ifelse(rel_months %in% -17:-20, 1, 0)*treated,
         treat_04 = ifelse(rel_months %in% -13:-16, 1, 0)*treated,
         treat_03 = ifelse(rel_months %in% -9:-12, 1, 0)*treated,
         treat_02 = ifelse(rel_months %in% -5:-8, 1, 0)*treated,
         treat_01 = ifelse(rel_months %in% -1:-4, 1, 0)*treated,
         treat01 = ifelse(rel_months %in% 1:4, 1, 0)*treated,
         treat02 = ifelse(rel_months %in% 5:8, 1, 0)*treated, 
         treat03 = ifelse(rel_months %in% 9:12, 1, 0)*treated) 
#Identify police boroughs, https://data.cityofnewyork.us/Public-Safety/NYPD-Sectors/eizi-ujye
dfborough <- fread("../Data/NYPD_sectors.csv", check.names = TRUE) %>%
  distinct(pct, patrol_bor) %>%
  rename(precinct = pct, 
         borough = patrol_bor)
df <- df %>%
  left_join(dfborough, by = "precinct")
#Create borough linear time trends
dftemp <- as.data.frame(with(df, i(factor(borough), datenum, keep = unique(df$borough))))
names(dftemp) <- paste0("datenum_", unique(df$borough))
df <- df %>% 
  bind_cols(dftemp) %>%
  as.data.frame()
rm(dftemp)

#Regression
dfest <- data.frame("outcome" = character(), "model" = character(), "est" = numeric(), 
                    "se" = numeric(), "irr" = numeric(), "irr.se" = numeric(),
                    "irr.low" = numeric(), "irr.up" = numeric(), "pvalue" = numeric(),
                    "n" = numeric(), stringsAsFactors = FALSE)
vnames <- df %>% select(crime_assault:crime_allassault, arrest_weapon, arrest_drugs, call_crime:call_qol) %>% names()
vtrends <- paste0("datenum_", unique(df$borough)[-2]) #exclude one id-trend to avoid perfect multicollinearity
vtreat <- c(sort(paste0("treat_", str_pad(2:3, 2, "left", "0")), decreasing = TRUE))
for(X in vnames){
  for(W in c("month_id_fe", "pretrends", "borough_time_FE")){
    #Model
    if(W == "month_id_fe"){
      #Month-year and individual FE
      f1 <- as.formula(paste0(X, " ~ post_treated | datenum + id"))
      lm1 <- fepois(f1, cluster = "id", data = df)  
    } else if(W == "borough_time_FE") {
      #Month-year and individual FE + borough, linear time trends
      f1 <- as.formula(paste0(X, " ~  post_treated + ", paste0(vtrends, collapse = " + "), " | datenum + id"))
      lm1 <- fepois(f1, cluster = "id", data = df)  
    } else if(W == "pretrends"){
      #Controlling for pre-trends
      f1 <- as.formula(paste0(X, " ~  post_treated + ", paste0(vtreat, collapse = " + "), " | datenum + id"))
      lm1 <- fepois(f1, cluster = "id", data = df)  
    }
    #Estimate mean
    vmean <- df %>% 
      filter(complete.cases(!!sym(X))) %>%
      filter(post == 0 & treated == 1) %>%
      pull(!!sym(X)) %>%
      mean(.)
    #Extract coefficient and SE
    dfest <- dfest %>%
      bind_rows(data.frame("outcome" = X, 
                           "model" = W, 
                           "est" = lm1$coefficients["post_treated"], 
                           "se" = lm1$se["post_treated"], 
                           "irr" = exp(lm1$coefficients["post_treated"]), 
                           "irr.se" = (exp(lm1$coefficients["post_treated"])*sqrt(diag(vcov(lm1)))["post_treated"]), 
                           "irr.low" = exp(lm1$coefficients["post_treated"] - 1.96*sqrt(diag(vcov(lm1)))["post_treated"]),
                           "irr.up" = exp(lm1$coefficients["post_treated"] + 1.96*sqrt(diag(vcov(lm1)))["post_treated"]),
                           "pvalue" = lm1$coeftable["post_treated","Pr(>|t|)"],
                           "n" = lm1$nobs, 
                           "mean" = vmean)) 
    print(paste0("Ready loop: ", W, "--", X)) 
  }
}
#fwrite(dfest, "../Data/Diff in diff Poisson public_safety1.csv")
#dfest <- fread("../Data/Diff in diff Poisson public_safety1.csv")
#df %>% group_by(id) %>% summarise_at(vars(crime_assault:call311_disorder), function(x) sum(x==0)/n()) %>% View()
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions





###############################################################################################-
## Regression: Diff-in-diff design Poisson crime, arrest, call, summons (three hexagons) ######
###############################################################################################-
#Import data and create time dummy variables (removing 2018 because it is a noisy year)
df <- fread("../Data/Crime month-hexagon2 data 2018-2022.csv", integer64 = "character") %>%
  filter(direct == 1 & year %in% 2019:2022) %>%
  mutate(treated = ifelse(site %in% "OnPoint NYC", 1, 0), 
         datenum = year + (month - 1) /12, 
         post = ifelse(datenum >= 2021.91, 1, 0), 
         post_treated = treated*post) %>%
  mutate(rel_months = round((datenum - (2021+(12-1)/12))*12)) %>%
  mutate(treat_09 = ifelse(rel_months %in% -33:-36, 1, 0)*treated,
         treat_08 = ifelse(rel_months %in% -29:-32, 1, 0)*treated,
         treat_07 = ifelse(rel_months %in% -25:-28, 1, 0)*treated,
         treat_06 = ifelse(rel_months %in% -21:-24, 1, 0)*treated,
         treat_05 = ifelse(rel_months %in% -17:-20, 1, 0)*treated,
         treat_04 = ifelse(rel_months %in% -13:-16, 1, 0)*treated,
         treat_03 = ifelse(rel_months %in% -9:-12, 1, 0)*treated,
         treat_02 = ifelse(rel_months %in% -5:-8, 1, 0)*treated,
         treat_01 = ifelse(rel_months %in% -1:-4, 1, 0)*treated,
         treat01 = ifelse(rel_months %in% 1:4, 1, 0)*treated,
         treat02 = ifelse(rel_months %in% 5:8, 1, 0)*treated, 
         treat03 = ifelse(rel_months %in% 9:12, 1, 0)*treated) 
#Identify police boroughs, https://data.cityofnewyork.us/Public-Safety/NYPD-Sectors/eizi-ujye
dfborough <- fread("../Data/NYPD_sectors.csv", check.names = TRUE) %>%
  distinct(pct, patrol_bor) %>%
  rename(precinct = pct, 
         borough = patrol_bor)
df <- df %>%
  left_join(dfborough, by = "precinct")
#Create borough linear time trends
dftemp <- as.data.frame(with(df, i(factor(borough), datenum, keep = unique(df$borough))))
names(dftemp) <- paste0("datenum_", unique(df$borough))
df <- df %>% 
  bind_cols(dftemp) %>%
  as.data.frame()
rm(dftemp)

#Regression
dfest <- data.frame("outcome" = character(), "model" = character(), "est" = numeric(), 
                    "se" = numeric(), "irr" = numeric(), "irr.se" = numeric(),
                    "irr.low" = numeric(), "irr.up" = numeric(), "pvalue" = numeric(),
                    "n" = numeric(), stringsAsFactors = FALSE)
vnames <- df %>% select(crime_assault:crime_allassault, arrest_weapon, arrest_drugs, call_crime:call_qol) %>% names()
vtrends <- paste0("datenum_", unique(df$borough)[-2]) #exclude one id-trend to avoid perfect multicollinearity
vtreat <- c(sort(paste0("treat_", str_pad(2:3, 2, "left", "0")), decreasing = TRUE))
for(X in vnames){
  for(W in c("month_id_fe", "pretrends", "borough_time_FE")){
    #Model
    if(W == "month_id_fe"){
      #Month-year and individual FE
      f1 <- as.formula(paste0(X, " ~ post_treated | datenum + id"))
      lm1 <- fepois(f1, cluster = "id", data = df)  
    } else if(W == "borough_time_FE") {
      #Month-year and individual FE + borough, linear time trends
      f1 <- as.formula(paste0(X, " ~  post_treated + ", paste0(vtrends, collapse = " + "), " | datenum + id"))
      lm1 <- fepois(f1, cluster = "id", data = df)  
    } else if(W == "pretrends"){
      #Controlling for pre-trends
      f1 <- as.formula(paste0(X, " ~  post_treated + ", paste0(vtreat, collapse = " + "), " | datenum + id"))
      lm1 <- fepois(f1, cluster = "id", data = df)  
    }
    #Estimate mean
    vmean <- df %>% 
      filter(complete.cases(!!sym(X))) %>%
      filter(post == 0 & treated == 1) %>%
      pull(!!sym(X)) %>%
      mean(.)
    #Extract coefficient and SE
    dfest <- dfest %>%
      bind_rows(data.frame("outcome" = X, 
                           "model" = W, 
                           "est" = lm1$coefficients["post_treated"], 
                           "se" = lm1$se["post_treated"], 
                           "irr" = exp(lm1$coefficients["post_treated"]), 
                           "irr.se" = (exp(lm1$coefficients["post_treated"])*sqrt(diag(vcov(lm1)))["post_treated"]), 
                           "irr.low" = exp(lm1$coefficients["post_treated"] - 1.96*sqrt(diag(vcov(lm1)))["post_treated"]),
                           "irr.up" = exp(lm1$coefficients["post_treated"] + 1.96*sqrt(diag(vcov(lm1)))["post_treated"]),
                           "pvalue" = lm1$coeftable["post_treated","Pr(>|t|)"],
                           "n" = lm1$nobs, 
                           "mean" = vmean)) 
    print(paste0("Ready loop: ", W, "--", X)) 
  }
}
#fwrite(dfest, "../Data/Diff in diff Poisson public_safety2.csv")
#dfest <- fread("../Data/Diff in diff Poisson public_safety2.csv")
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions


#################################################################################################-
## Regression: Diff-in-diff design NegBin crime, arrest, call, summons (single hexagon) #########
#################################################################################################-
#Import data and create time dummy variables (removing 2018 because it is a noisy year)
df <- fread("../Data/Crime month-hexagon1 data 2018-2022.csv", integer64 = "character") %>%
  filter(direct == 1 & year %in% 2019:2022) %>%
  mutate(treated = ifelse(site %in% "OnPoint NYC", 1, 0), 
         datenum = year + (month - 1) /12, 
         post = ifelse(datenum >= 2021.91, 1, 0), 
         post_treated = treated*post) %>%
  mutate(rel_months = round((datenum - (2021+(12-1)/12))*12)) %>%
  mutate(treat_09 = ifelse(rel_months %in% -33:-36, 1, 0)*treated,
         treat_08 = ifelse(rel_months %in% -29:-32, 1, 0)*treated,
         treat_07 = ifelse(rel_months %in% -25:-28, 1, 0)*treated,
         treat_06 = ifelse(rel_months %in% -21:-24, 1, 0)*treated,
         treat_05 = ifelse(rel_months %in% -17:-20, 1, 0)*treated,
         treat_04 = ifelse(rel_months %in% -13:-16, 1, 0)*treated,
         treat_03 = ifelse(rel_months %in% -9:-12, 1, 0)*treated,
         treat_02 = ifelse(rel_months %in% -5:-8, 1, 0)*treated,
         treat_01 = ifelse(rel_months %in% -1:-4, 1, 0)*treated,
         treat01 = ifelse(rel_months %in% 1:4, 1, 0)*treated,
         treat02 = ifelse(rel_months %in% 5:8, 1, 0)*treated, 
         treat03 = ifelse(rel_months %in% 9:12, 1, 0)*treated) 
#Create unit specific time trends
dftemp <- as.data.frame(with(df, i(factor(id), datenum, keep = unique(df$id))))
names(dftemp) <- paste0("datenum_", unique(df$id))
df <- df %>% 
  bind_cols(dftemp) %>%
  as.data.frame()
rm(dftemp)

#Regression
dfest <- data.frame("outcome" = character(), "model" = character(), "est" = numeric(), 
                    "se" = numeric(), "irr" = numeric(), "irr.se" = numeric(),
                    "irr.low" = numeric(), "irr.up" = numeric(), "pvalue" = numeric(),
                    "n" = numeric(), stringsAsFactors = FALSE)
vnames <- df %>% select(crime_assault:crime_allassault, arrest_weapon, arrest_drugs, call_crime:call_qol) %>% names()
vtrends <- paste0("datenum_", unique(df$id)[-2]) #exclude one id-trend to avoid perfect multicollinearity
vtreat <- c(sort(paste0("treat_", str_pad(2:3, 2, "left", "0")), decreasing = TRUE))
for(X in vnames){
  for(W in c("month_id_fe", "pretrends")){
    #Model
    if(W == "month_id_fe"){
      #Month-year and individual FE
      f1 <- as.formula(paste0(X, " ~ post_treated | datenum + id"))
      lm1 <- fenegbin(f1, cluster = "id", data = df)  
    } else if(W == "pretrends"){
      #Controlling for pre-trends
      f1 <- as.formula(paste0(X, " ~  post_treated + ", paste0(vtreat, collapse = " + "), " | datenum + id"))
      lm1 <- fenegbin(f1, cluster = "id", data = df)  
    }
    #Estimate mean
    vmean <- df %>% 
      filter(complete.cases(!!sym(X))) %>%
      filter(post == 0 & treated == 1) %>%
      pull(!!sym(X)) %>%
      mean(.)
    #Extract coefficient and SE
    dfest <- dfest %>%
      bind_rows(data.frame("outcome" = X, 
                           "model" = W, 
                           "est" = lm1$coefficients["post_treated"], 
                           "se" = lm1$se["post_treated"], 
                           "irr" = exp(lm1$coefficients["post_treated"]), 
                           "irr.se" = (exp(lm1$coefficients["post_treated"])*sqrt(diag(vcov(lm1)))["post_treated"]), 
                           "irr.low" = exp(lm1$coefficients["post_treated"] - 1.96*sqrt(diag(vcov(lm1)))["post_treated"]),
                           "irr.up" = exp(lm1$coefficients["post_treated"] + 1.96*sqrt(diag(vcov(lm1)))["post_treated"]),
                           "pvalue" = lm1$coeftable["post_treated","Pr(>|t|)"],
                           "n" = lm1$nobs, 
                           "mean" = vmean)) 
    print(paste0("Ready loop: ", W, "--", X)) 
  }
}
#fwrite(dfest, "../Data/Diff in diff NegBin public_safety1.csv")
#dfest <- fread("../Data/Diff in diff NegBin public_safety1.csv")
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions





#################################################################################################-
## Regression: Diff-in-diff design NegBin crime, arrest, call, summons (three hexagons) #########
#################################################################################################-
#Import data and create time dummy variables (removing 2018 because it is a noisy year)
df <- fread("../Data/Crime month-hexagon2 data 2018-2022.csv", integer64 = "character") %>%
  filter(direct == 1 & year %in% 2019:2022) %>%
  mutate(treated = ifelse(site %in% "OnPoint NYC", 1, 0), 
         datenum = year + (month - 1) /12, 
         post = ifelse(datenum >= 2021.91, 1, 0), 
         post_treated = treated*post) %>%
  mutate(rel_months = round((datenum - (2021+(12-1)/12))*12)) %>%
  mutate(treat_09 = ifelse(rel_months %in% -33:-36, 1, 0)*treated,
         treat_08 = ifelse(rel_months %in% -29:-32, 1, 0)*treated,
         treat_07 = ifelse(rel_months %in% -25:-28, 1, 0)*treated,
         treat_06 = ifelse(rel_months %in% -21:-24, 1, 0)*treated,
         treat_05 = ifelse(rel_months %in% -17:-20, 1, 0)*treated,
         treat_04 = ifelse(rel_months %in% -13:-16, 1, 0)*treated,
         treat_03 = ifelse(rel_months %in% -9:-12, 1, 0)*treated,
         treat_02 = ifelse(rel_months %in% -5:-8, 1, 0)*treated,
         treat_01 = ifelse(rel_months %in% -1:-4, 1, 0)*treated,
         treat01 = ifelse(rel_months %in% 1:4, 1, 0)*treated,
         treat02 = ifelse(rel_months %in% 5:8, 1, 0)*treated, 
         treat03 = ifelse(rel_months %in% 9:12, 1, 0)*treated) 
#Create unit specific time trends
dftemp <- as.data.frame(with(df, i(factor(id), datenum, keep = unique(df$id))))
names(dftemp) <- paste0("datenum_", unique(df$id))
df <- df %>% 
  bind_cols(dftemp) %>%
  as.data.frame()
rm(dftemp)

#Regression
dfest <- data.frame("outcome" = character(), "model" = character(), "est" = numeric(), 
                    "se" = numeric(), "irr" = numeric(), "irr.se" = numeric(),
                    "irr.low" = numeric(), "irr.up" = numeric(), "pvalue" = numeric(),
                    "n" = numeric(), stringsAsFactors = FALSE)
vnames <- df %>% select(crime_assault:crime_allassault, arrest_weapon, arrest_drugs, call_crime:call_qol) %>% names()
vtrends <- paste0("datenum_", unique(df$id)[-2]) #exclude one id-trend to avoid perfect multicollinearity
vtreat <- c(sort(paste0("treat_", str_pad(2:3, 2, "left", "0")), decreasing = TRUE))
for(X in vnames){
  for(W in c("month_id_fe", "pretrends")){
    #Model
    if(W == "month_id_fe"){
      #Month-year and individual FE
      f1 <- as.formula(paste0(X, " ~ post_treated | datenum + id"))
      lm1 <- fenegbin(f1, cluster = "id", data = df)  
    } else if(W == "pretrends"){
      #Controlling for pre-trends
      f1 <- as.formula(paste0(X, " ~  post_treated + ", paste0(vtreat, collapse = " + "), " | datenum + id"))
      lm1 <- fenegbin(f1, cluster = "id", data = df)  
    }
    #Estimate mean
    vmean <- df %>% 
      filter(complete.cases(!!sym(X))) %>%
      filter(post == 0 & treated == 1) %>%
      pull(!!sym(X)) %>%
      mean(.)
    #Extract coefficient and SE
    dfest <- dfest %>%
      bind_rows(data.frame("outcome" = X, 
                           "model" = W, 
                           "est" = lm1$coefficients["post_treated"], 
                           "se" = lm1$se["post_treated"], 
                           "irr" = exp(lm1$coefficients["post_treated"]), 
                           "irr.se" = (exp(lm1$coefficients["post_treated"])*sqrt(diag(vcov(lm1)))["post_treated"]), 
                           "irr.low" = exp(lm1$coefficients["post_treated"] - 1.96*sqrt(diag(vcov(lm1)))["post_treated"]),
                           "irr.up" = exp(lm1$coefficients["post_treated"] + 1.96*sqrt(diag(vcov(lm1)))["post_treated"]),
                           "pvalue" = lm1$coeftable["post_treated","Pr(>|t|)"],
                           "n" = lm1$nobs, 
                           "mean" = vmean)) 
    print(paste0("Ready loop: ", W, "--", X)) 
  }
}
#fwrite(dfest, "../Data/Diff in diff NegBin public_safety2.csv")
#dfest <- fread("../Data/Diff in diff NegBin public_safety2.csv")
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions






###############################################################################################################-
## Regression: Diff-in-diff design Poisson crime, arrest, call, summons (single hexagon-high crime city) ######
###############################################################################################################-
#Import data and create time dummy variables (removing 2018 because it is a noisy year)
df <- fread("../Data/Crime month-hexagon city data 2018-2022.csv", integer64 = "character") %>%
  filter(year %in% 2019:2022) %>%
  filter(hexsample == 1 | is.na(hexsample)) %>%
  mutate(treated = ifelse(site %in% "OnPoint NYC", 1, 0), 
         datenum = year + (month - 1) /12, 
         post = ifelse(datenum >= 2021.91, 1, 0), 
         post_treated = treated*post) %>%
  mutate(rel_months = round((datenum - (2021+(12-1)/12))*12)) %>%
  mutate(treat_09 = ifelse(rel_months %in% -33:-36, 1, 0)*treated,
         treat_08 = ifelse(rel_months %in% -29:-32, 1, 0)*treated,
         treat_07 = ifelse(rel_months %in% -25:-28, 1, 0)*treated,
         treat_06 = ifelse(rel_months %in% -21:-24, 1, 0)*treated,
         treat_05 = ifelse(rel_months %in% -17:-20, 1, 0)*treated,
         treat_04 = ifelse(rel_months %in% -13:-16, 1, 0)*treated,
         treat_03 = ifelse(rel_months %in% -9:-12, 1, 0)*treated,
         treat_02 = ifelse(rel_months %in% -5:-8, 1, 0)*treated,
         treat_01 = ifelse(rel_months %in% -1:-4, 1, 0)*treated,
         treat01 = ifelse(rel_months %in% 1:4, 1, 0)*treated,
         treat02 = ifelse(rel_months %in% 5:8, 1, 0)*treated, 
         treat03 = ifelse(rel_months %in% 9:12, 1, 0)*treated) 
#Identifying the 250 unsafest hexagons in the top 10 unsafe precincts (excluding the treated ones and Times Squares)
vids <- df %>%
  filter(post == 0) %>%
  group_by(precinct, treated, rank2, id) %>%
  summarise(crime_index2 = sum(crime_index - crime_theft)) %>%
  ungroup() %>%
  filter(!precinct %in% c(14,25,34)) %>%
  filter(rank2 %in% 1:9) %>%
  mutate(rankhex2 = min_rank(-crime_index2)) %>%
  filter(rankhex2 <= 250) %>%
  pull(id)
df <- df %>% 
  filter(id %in% vids | treated == 1)
#Create unit specific time trends
dftemp <- as.data.frame(with(df, i(factor(id), datenum, keep = unique(df$id))))
names(dftemp) <- paste0("datenum_", unique(df$id))
df <- df %>% 
  bind_cols(dftemp) %>%
  as.data.frame()
rm(dftemp)

#Regression
dfest <- data.frame("outcome" = character(), "model" = character(), "est" = numeric(), 
                    "se" = numeric(), "irr" = numeric(), "irr.se" = numeric(),
                    "irr.low" = numeric(), "irr.up" = numeric(), "pvalue" = numeric(),
                    "n" = numeric(), stringsAsFactors = FALSE)
vnames <- df %>% select(crime_assault:crime_allassault, arrest_weapon, arrest_drugs, call_crime:call_qol) %>% names()
vtrends <- paste0("datenum_", unique(df$id)[-2]) #exclude one id-trend to avoid perfect multicollinearity
vtreat <- c(sort(paste0("treat_", str_pad(2:3, 2, "left", "0")), decreasing = TRUE))
for(X in vnames){
  for(W in c("month_id_fe", "pretrends")){
    #Model
    if(W == "month_id_fe"){
      #Month-year and individual FE
      f1 <- as.formula(paste0(X, " ~ post_treated | datenum + id"))
      lm1 <- fepois(f1, cluster = "id", data = df)  
    } else if(W == "pretrends"){
      #Controlling for pre-trends
      f1 <- as.formula(paste0(X, " ~  post_treated + ", paste0(vtreat, collapse = " + "), " | datenum + id"))
      lm1 <- fepois(f1, cluster = "id", data = df)  
    }
    #Estimate mean
    vmean <- df %>% 
      filter(complete.cases(!!sym(X))) %>%
      filter(post == 0 & treated == 1) %>%
      pull(!!sym(X)) %>%
      mean(.)
    #Extract coefficient and SE
    dfest <- dfest %>%
      bind_rows(data.frame("outcome" = X, 
                           "model" = W, 
                           "est" = lm1$coefficients["post_treated"], 
                           "se" = lm1$se["post_treated"], 
                           "irr" = exp(lm1$coefficients["post_treated"]), 
                           "irr.se" = (exp(lm1$coefficients["post_treated"])*sqrt(diag(vcov(lm1)))["post_treated"]), 
                           "irr.low" = exp(lm1$coefficients["post_treated"] - 1.96*sqrt(diag(vcov(lm1)))["post_treated"]),
                           "irr.up" = exp(lm1$coefficients["post_treated"] + 1.96*sqrt(diag(vcov(lm1)))["post_treated"]),
                           "pvalue" = lm1$coeftable["post_treated","Pr(>|t|)"],
                           "n" = lm1$nobs, 
                           "mean" = vmean)) 
    print(paste0("Ready loop: ", W, "--", X)) 
  }
}
#fwrite(dfest, "../Data/Diff in diff Poisson highcrimecity public_safety1.csv")
#dfest <- fread("../Data/Diff in diff Poisson highcrimecity public_safety1.csv")
#df %>% group_by(id) %>% summarise_at(vars(crime_assault:call311_disorder), function(x) sum(x==0)/n()) %>% View()
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions






###############################################################################################################-
## Regression: Diff-in-diff design Poisson crime, arrest, call, summons (three hexagons-high crime city) ######
###############################################################################################################-
#Import data and create time dummy variables (removing 2018 because it is a noisy year)
df <- fread("../Data/Crime month-hexagon city data 2018-2022.csv", integer64 = "character") %>%
  filter(year %in% 2019:2022) %>%
  filter(hexsample == 3 | is.na(hexsample)) %>%
  mutate(treated = ifelse(site %in% "OnPoint NYC", 1, 0), 
         datenum = year + (month - 1) /12, 
         post = ifelse(datenum >= 2021.91, 1, 0), 
         post_treated = treated*post) %>%
  mutate(rel_months = round((datenum - (2021+(12-1)/12))*12)) %>%
  mutate(treat_09 = ifelse(rel_months %in% -33:-36, 1, 0)*treated,
         treat_08 = ifelse(rel_months %in% -29:-32, 1, 0)*treated,
         treat_07 = ifelse(rel_months %in% -25:-28, 1, 0)*treated,
         treat_06 = ifelse(rel_months %in% -21:-24, 1, 0)*treated,
         treat_05 = ifelse(rel_months %in% -17:-20, 1, 0)*treated,
         treat_04 = ifelse(rel_months %in% -13:-16, 1, 0)*treated,
         treat_03 = ifelse(rel_months %in% -9:-12, 1, 0)*treated,
         treat_02 = ifelse(rel_months %in% -5:-8, 1, 0)*treated,
         treat_01 = ifelse(rel_months %in% -1:-4, 1, 0)*treated,
         treat01 = ifelse(rel_months %in% 1:4, 1, 0)*treated,
         treat02 = ifelse(rel_months %in% 5:8, 1, 0)*treated, 
         treat03 = ifelse(rel_months %in% 9:12, 1, 0)*treated) 
#Identifying the 250 unsafest hexagons in the top 10 unsafe precincts (excluding the treated ones and Times Squares)
vids <- df %>%
  filter(post == 0) %>%
  group_by(precinct, treated, rank2, id) %>%
  summarise(crime_index2 = sum(crime_index - crime_theft)) %>%
  ungroup() %>%
  filter(!precinct %in% c(14,25,34)) %>%
  filter(rank2 %in% 1:9) %>%
  mutate(rankhex2 = min_rank(-crime_index2)) %>%
  filter(rankhex2 <= 250) %>%
  pull(id)
df <- df %>% 
  filter(id %in% vids | treated == 1)
#Create unit specific time trends
dftemp <- as.data.frame(with(df, i(factor(id), datenum, keep = unique(df$id))))
names(dftemp) <- paste0("datenum_", unique(df$id))
df <- df %>% 
  bind_cols(dftemp) %>%
  as.data.frame()
rm(dftemp)

#Regression
dfest <- data.frame("outcome" = character(), "model" = character(), "est" = numeric(), 
                    "se" = numeric(), "irr" = numeric(), "irr.se" = numeric(),
                    "irr.low" = numeric(), "irr.up" = numeric(), "pvalue" = numeric(),
                    "n" = numeric(), stringsAsFactors = FALSE)
vnames <- df %>% select(crime_assault:crime_allassault, arrest_weapon, arrest_drugs, call_crime:call_qol) %>% names()
vtrends <- paste0("datenum_", unique(df$id)[-2]) #exclude one id-trend to avoid perfect multicollinearity
vtreat <- c(sort(paste0("treat_", str_pad(2:3, 2, "left", "0")), decreasing = TRUE))
for(X in vnames){
  for(W in c("month_id_fe", "pretrends")){
    #Model
    if(W == "month_id_fe"){
      #Month-year and individual FE
      f1 <- as.formula(paste0(X, " ~ post_treated | datenum + id"))
      lm1 <- fepois(f1, cluster = "id", data = df)  
    } else if(W == "pretrends"){
      #Controlling for pre-trends
      f1 <- as.formula(paste0(X, " ~  post_treated + ", paste0(vtreat, collapse = " + "), " | datenum + id"))
      lm1 <- fepois(f1, cluster = "id", data = df)  
    }
    #Estimate mean
    vmean <- df %>% 
      filter(complete.cases(!!sym(X))) %>%
      filter(post == 0 & treated == 1) %>%
      pull(!!sym(X)) %>%
      mean(.)
    #Extract coefficient and SE
    dfest <- dfest %>%
      bind_rows(data.frame("outcome" = X, 
                           "model" = W, 
                           "est" = lm1$coefficients["post_treated"], 
                           "se" = lm1$se["post_treated"], 
                           "irr" = exp(lm1$coefficients["post_treated"]), 
                           "irr.se" = (exp(lm1$coefficients["post_treated"])*sqrt(diag(vcov(lm1)))["post_treated"]), 
                           "irr.low" = exp(lm1$coefficients["post_treated"] - 1.96*sqrt(diag(vcov(lm1)))["post_treated"]),
                           "irr.up" = exp(lm1$coefficients["post_treated"] + 1.96*sqrt(diag(vcov(lm1)))["post_treated"]),
                           "pvalue" = lm1$coeftable["post_treated","Pr(>|t|)"],
                           "n" = lm1$nobs, 
                           "mean" = vmean)) 
    print(paste0("Ready loop: ", W, "--", X)) 
  }
}
#fwrite(dfest, "../Data/Diff in diff Poisson highcrimecity public_safety2.csv")
#dfest <- fread("../Data/Diff in diff Poisson highcrimecity public_safety2.csv")
#df %>% group_by(id) %>% summarise_at(vars(crime_assault:call311_disorder), function(x) sum(x==0)/n()) %>% View()
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions






################################################################################################################-
## Regression: Diff-in-diff design Poisson crime, arrest, call, summons (single hexagon-high drug arrests) #####
################################################################################################################-
#Import data and create time dummy variables (removing 2018 because it is a noisy year)
df <- fread("../Data/Crime month-hexagon city data 2018-2022.csv", integer64 = "character") %>%
  filter(year %in% 2019:2022) %>%
  filter(hexsample == 1 | is.na(hexsample)) %>%
  mutate(treated = ifelse(site %in% "OnPoint NYC", 1, 0), 
         datenum = year + (month - 1) /12, 
         post = ifelse(datenum >= 2021.91, 1, 0), 
         post_treated = treated*post) %>%
  mutate(rel_months = round((datenum - (2021+(12-1)/12))*12)) %>%
  mutate(treat_09 = ifelse(rel_months %in% -33:-36, 1, 0)*treated,
         treat_08 = ifelse(rel_months %in% -29:-32, 1, 0)*treated,
         treat_07 = ifelse(rel_months %in% -25:-28, 1, 0)*treated,
         treat_06 = ifelse(rel_months %in% -21:-24, 1, 0)*treated,
         treat_05 = ifelse(rel_months %in% -17:-20, 1, 0)*treated,
         treat_04 = ifelse(rel_months %in% -13:-16, 1, 0)*treated,
         treat_03 = ifelse(rel_months %in% -9:-12, 1, 0)*treated,
         treat_02 = ifelse(rel_months %in% -5:-8, 1, 0)*treated,
         treat_01 = ifelse(rel_months %in% -1:-4, 1, 0)*treated,
         treat01 = ifelse(rel_months %in% 1:4, 1, 0)*treated,
         treat02 = ifelse(rel_months %in% 5:8, 1, 0)*treated, 
         treat03 = ifelse(rel_months %in% 9:12, 1, 0)*treated) 
#Identifying the 20 hexagons with most pre-intervention drug arrests
#(excluding central booking/hospital areas)
vids <- df %>%
  filter(post == 0) %>%
  filter(!id %in% c(11435, 12630)) %>%
  group_by(precinct, treated, rank2, id) %>%
  summarise(arrest_drugs = sum(arrest_drugs)) %>%
  ungroup() %>%
  filter(!precinct %in% c(14,25,34)) %>%
  mutate(rankhex2 = min_rank(-arrest_drugs)) %>%
  filter(rankhex2 <= 20) %>%
  pull(id)
df <- df %>% 
  filter(id %in% vids | treated == 1)
#Create unit specific time trends
dftemp <- as.data.frame(with(df, i(factor(id), datenum, keep = unique(df$id))))
names(dftemp) <- paste0("datenum_", unique(df$id))
df <- df %>% 
  bind_cols(dftemp) %>%
  as.data.frame()
rm(dftemp)

#Regression
dfest <- data.frame("outcome" = character(), "model" = character(), "est" = numeric(), 
                    "se" = numeric(), "irr" = numeric(), "irr.se" = numeric(),
                    "irr.low" = numeric(), "irr.up" = numeric(), "pvalue" = numeric(),
                    "n" = numeric(), stringsAsFactors = FALSE)
vnames <- df %>% select(crime_assault:crime_allassault, arrest_weapon, arrest_drugs, call_crime:call_qol) %>% names()
vtrends <- paste0("datenum_", unique(df$id)[-2]) #exclude one id-trend to avoid perfect multicollinearity
vtreat <- c(sort(paste0("treat_", str_pad(2:3, 2, "left", "0")), decreasing = TRUE))
for(X in vnames){
  for(W in c("month_id_fe", "pretrends")){
    #Model
    if(W == "month_id_fe"){
      #Month-year and individual FE
      f1 <- as.formula(paste0(X, " ~ post_treated | datenum + id"))
      lm1 <- fepois(f1, cluster = "id", data = df)  
    } else if(W == "pretrends"){
      #Controlling for pre-trends
      f1 <- as.formula(paste0(X, " ~  post_treated + ", paste0(vtreat, collapse = " + "), " | datenum + id"))
      lm1 <- fepois(f1, cluster = "id", data = df)  
    }
    #Estimate mean
    vmean <- df %>% 
      filter(complete.cases(!!sym(X))) %>%
      filter(post == 0 & treated == 1) %>%
      pull(!!sym(X)) %>%
      mean(.)
    #Extract coefficient and SE
    dfest <- dfest %>%
      bind_rows(data.frame("outcome" = X, 
                           "model" = W, 
                           "est" = lm1$coefficients["post_treated"], 
                           "se" = lm1$se["post_treated"], 
                           "irr" = exp(lm1$coefficients["post_treated"]), 
                           "irr.se" = (exp(lm1$coefficients["post_treated"])*sqrt(diag(vcov(lm1)))["post_treated"]), 
                           "irr.low" = exp(lm1$coefficients["post_treated"] - 1.96*sqrt(diag(vcov(lm1)))["post_treated"]),
                           "irr.up" = exp(lm1$coefficients["post_treated"] + 1.96*sqrt(diag(vcov(lm1)))["post_treated"]),
                           "pvalue" = lm1$coeftable["post_treated","Pr(>|t|)"],
                           "n" = lm1$nobs, 
                           "mean" = vmean)) 
    print(paste0("Ready loop: ", W, "--", X)) 
  }
}
#fwrite(dfest, "../Data/Diff in diff Poisson highdrugarrests public_safety1.csv")
#dfest <- fread("../Data/Diff in diff Poisson highdrugarrests public_safety1.csv")
#df %>% group_by(id) %>% summarise_at(vars(crime_assault:call311_disorder), function(x) sum(x==0)/n()) %>% View()
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions





################################################################################################################-
## Regression: Diff-in-diff design Poisson crime, arrest, call, summons (three hexagons-high drug arrests) #####
################################################################################################################-
#Import data and create time dummy variables (removing 2018 because it is a noisy year)
df <- fread("../Data/Crime month-hexagon city data 2018-2022.csv", integer64 = "character") %>%
  filter(year %in% 2019:2022) %>%
  filter(hexsample == 3 | is.na(hexsample)) %>%
  mutate(treated = ifelse(site %in% "OnPoint NYC", 1, 0), 
         datenum = year + (month - 1) /12, 
         post = ifelse(datenum >= 2021.91, 1, 0), 
         post_treated = treated*post) %>%
  mutate(rel_months = round((datenum - (2021+(12-1)/12))*12)) %>%
  mutate(treat_09 = ifelse(rel_months %in% -33:-36, 1, 0)*treated,
         treat_08 = ifelse(rel_months %in% -29:-32, 1, 0)*treated,
         treat_07 = ifelse(rel_months %in% -25:-28, 1, 0)*treated,
         treat_06 = ifelse(rel_months %in% -21:-24, 1, 0)*treated,
         treat_05 = ifelse(rel_months %in% -17:-20, 1, 0)*treated,
         treat_04 = ifelse(rel_months %in% -13:-16, 1, 0)*treated,
         treat_03 = ifelse(rel_months %in% -9:-12, 1, 0)*treated,
         treat_02 = ifelse(rel_months %in% -5:-8, 1, 0)*treated,
         treat_01 = ifelse(rel_months %in% -1:-4, 1, 0)*treated,
         treat01 = ifelse(rel_months %in% 1:4, 1, 0)*treated,
         treat02 = ifelse(rel_months %in% 5:8, 1, 0)*treated, 
         treat03 = ifelse(rel_months %in% 9:12, 1, 0)*treated) 
#Identifying the 20 hexagons with most pre-intervention drug arrests
#(excluding central booking/hospital areas)
vids <- df %>%
  filter(post == 0) %>%
  filter(!id %in% c(11435, 12630)) %>%
  group_by(precinct, treated, rank2, id) %>%
  summarise(arrest_drugs = sum(arrest_drugs)) %>%
  ungroup() %>%
  filter(!precinct %in% c(14,25,34)) %>%
  mutate(rankhex2 = min_rank(-arrest_drugs)) %>%
  filter(rankhex2 <= 20) %>%
  pull(id)
df <- df %>% 
  filter(id %in% vids | treated == 1)
#Create unit specific time trends
dftemp <- as.data.frame(with(df, i(factor(id), datenum, keep = unique(df$id))))
names(dftemp) <- paste0("datenum_", unique(df$id))
df <- df %>% 
  bind_cols(dftemp) %>%
  as.data.frame()
rm(dftemp)

#Regression
dfest <- data.frame("outcome" = character(), "model" = character(), "est" = numeric(), 
                    "se" = numeric(), "irr" = numeric(), "irr.se" = numeric(),
                    "irr.low" = numeric(), "irr.up" = numeric(), "pvalue" = numeric(),
                    "n" = numeric(), stringsAsFactors = FALSE)
vnames <- df %>% select(crime_assault:crime_allassault, arrest_weapon, arrest_drugs, call_crime:call_qol) %>% names()
vtrends <- paste0("datenum_", unique(df$id)[-2]) #exclude one id-trend to avoid perfect multicollinearity
vtreat <- c(sort(paste0("treat_", str_pad(2:3, 2, "left", "0")), decreasing = TRUE))
for(X in vnames){
  for(W in c("month_id_fe", "pretrends")){
    #Model
    if(W == "month_id_fe"){
      #Month-year and individual FE
      f1 <- as.formula(paste0(X, " ~ post_treated | datenum + id"))
      lm1 <- fepois(f1, cluster = "id", data = df)  
    } else if(W == "pretrends"){
      #Controlling for pre-trends
      f1 <- as.formula(paste0(X, " ~  post_treated + ", paste0(vtreat, collapse = " + "), " | datenum + id"))
      lm1 <- fepois(f1, cluster = "id", data = df)  
    }
    #Estimate mean
    vmean <- df %>% 
      filter(complete.cases(!!sym(X))) %>%
      filter(post == 0 & treated == 1) %>%
      pull(!!sym(X)) %>%
      mean(.)
    #Extract coefficient and SE
    dfest <- dfest %>%
      bind_rows(data.frame("outcome" = X, 
                           "model" = W, 
                           "est" = lm1$coefficients["post_treated"], 
                           "se" = lm1$se["post_treated"], 
                           "irr" = exp(lm1$coefficients["post_treated"]), 
                           "irr.se" = (exp(lm1$coefficients["post_treated"])*sqrt(diag(vcov(lm1)))["post_treated"]), 
                           "irr.low" = exp(lm1$coefficients["post_treated"] - 1.96*sqrt(diag(vcov(lm1)))["post_treated"]),
                           "irr.up" = exp(lm1$coefficients["post_treated"] + 1.96*sqrt(diag(vcov(lm1)))["post_treated"]),
                           "pvalue" = lm1$coeftable["post_treated","Pr(>|t|)"],
                           "n" = lm1$nobs, 
                           "mean" = vmean)) 
    print(paste0("Ready loop: ", W, "--", X)) 
  }
}
#fwrite(dfest, "../Data/Diff in diff Poisson highdrugarrests public_safety2.csv")
#dfest <- fread("../Data/Diff in diff Poisson highdrugarrests public_safety2.csv")
#df %>% group_by(id) %>% summarise_at(vars(crime_assault:call311_disorder), function(x) sum(x==0)/n()) %>% View()
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions




########################################################################################################-
## Regression: Synthetic control on crime, arrest, call, summons (single hexagon-high crime city) ######
########################################################################################################-
#Import data (only kept a random sample of 300 (~33%) hexagons from the top decile to reduce computational time)
df <- fread("../Data/Crime month-hexagon city data 2018-2022.csv", integer64 = "character") %>%
  filter(year %in% 2019:2022) %>%
  filter(hexsample == 1 | is.na(hexsample)) %>%
  mutate(bimonth = ifelse(month %in% 1:2, 1, 6),
         bimonth = ifelse(month %in% 3:4, 2, bimonth), 
         bimonth = ifelse(month %in% 5:6, 3, bimonth), 
         bimonth = ifelse(month %in% 7:8, 4, bimonth), 
         bimonth = ifelse(month %in% 9:10, 5, bimonth)) %>%
  group_by(year, bimonth, id, site, precinct, rank2) %>%
  summarise_at(vars(crime_assault:crime_allassault, arrest_weapon, arrest_drugs, summons, call_crime:call_qol), sum) %>%
  ungroup() %>%
  mutate(treated = ifelse(site %in% "OnPoint NYC", 1, 0), 
         datenum = round(year + (bimonth - 1) /6, 3), 
         post = ifelse(datenum >= 2021.83, 1, 0), 
         post_treated = treated*post)
#Identifying the 250 unsafest hexagons in the top 10 unsafe precincts (excluding the treated ones and Times Squares)
vids <- df %>%
  filter(post == 0) %>%
  group_by(precinct, treated, rank2, id) %>%
  summarise(crime_index2 = sum(crime_index - crime_theft)) %>%
  ungroup() %>%
  filter(!precinct %in% c(14,25,34)) %>%
  filter(rank2 %in% 1:9) %>%
  mutate(rankhex2 = min_rank(-crime_index2)) %>%
  filter(rankhex2 <= 250) %>%
  pull(id)
df <- df %>% 
  filter(id %in% vids | treated == 1)
#Confirm sample and randomness
length(unique(df$id))
sfpd <- read_sf("../Data/NYC_Police_Precincts/NYC_Police_Precincts.shp") 
sfgrid <- read_sf("../Data/Grid/Hexagonal grid.shp") %>%
  filter(id %in% vids) %>%
  mutate(sample = ifelse(id %in% vids, "yes", "no"))
ggplot() +
  geom_sf(data = sfgrid, aes(fill = sample)) +
  geom_sf(data = sfpd, fill = NA) +
  theme_classic()
rm(sfgrid, sfpd, vids); gc()
#Vectors for the loop
vnames <- df %>%
  select(crime_violentall, crime_property, arrest_weapon, arrest_drugs, summons, 
         call_crime, call_trespass, call_medic, call_qol, call311_homeless, call311_disorder) %>%
  names()
vtime <- sort(unique(df$datenum))
vsite <- c(4, 11)


#Synth method: observed and synthetic hexagon, and weights
dfest1 <- data.frame("id" = 4, "synthetic" = rep(0:1, each = length(vtime)), 
                    "datenum" = rep(vtime, times = 2))
dfest2 <- data.frame("id" = 11, "synthetic" = rep(0:1, each = length(vtime)), 
                     "datenum" = rep(vtime, times = 2))
dfweight1 <- data.frame("id" = 4, "idhex" = sort(unique(df[df$treated==0,]$id)))
dfweight2 <- data.frame("id" = 11, "idhex" = sort(unique(df[df$treated==0,]$id)))
for(X in vsite){
  #Subset data
  df1 <- df %>%
    filter(id == X | treated == 0)  
  #Run over each outcome
  for(W in vnames){
    #Run model
    f1 <- as.formula(paste0(W, " ~ treated"))
    syn <- augsynth(f1, unit = id, time = datenum, data = df1, 
                    progfunc = "None", scm = TRUE, t_int = 2021.83) 
    #Extract observed and synthetic outcomes
    dftemp1 <- data.frame("synthetic" = 0, "datenum" = vtime, 
                          "outcome" = as.vector(syn$data$synth_data$Y1plot)) %>%
      rename(!!W := outcome) %>%
      bind_rows(data.frame("synthetic" = 1, "datenum" = vtime, 
                           "outcome" = as.vector(syn$data$synth_data$Y0plot %*% syn$weights)) %>%
                  rename(!!W := outcome))
    dftemp2 <- data.frame("idhex" = as.numeric(rownames(syn$weights)), 
                          "outcome" = syn$weights) %>%
      rename(!!W := outcome)
    #Keep results
    if(X == 4){
      dfest1 <- dfest1 %>%
        left_join(dftemp1, by = c("synthetic", "datenum"))
      dfweight1 <- dfweight1 %>%
        left_join(dftemp2, by = "idhex")  
    } else {
      dfest2 <- dfest2 %>%
        left_join(dftemp1, by = c("synthetic", "datenum"))
      dfweight2 <- dfweight2 %>%
        left_join(dftemp2, by = "idhex")  
    }
  }
}
dfest <- dfest1 %>%
  bind_rows(dfest2)
dfweight <- dfweight1 %>%
  bind_rows(dfweight2)
#fwrite(dfest, "../Data/Synth gap public_safety hexagon1.csv")
#fwrite(dfweight, "../Data/Synth weights public_safety hexagon1.csv")
#dfest <- fread("../Data/Synth gap public_safety hexagon1.csv", integer64 = "character")
#dfweight <- fread("../Data/Synth weights public_safety hexagon1.csv", integer64 = "character")
rm(df1, dfest, dfest1, dfest2, dfweight, dfweight1, dfweight2, syn, f1, W, X, dftemp1, dftemp2); gc()


#Synth method: placebo
vids1 <- sort(unique(df$id))
dfest1 <- expand_grid("id" = 4, "id_treated" = vids1[!vids1 %in% 11], "datenum" = vtime) %>%
  arrange(id, id_treated, datenum)
dfest2 <- expand_grid("id" = 11, "id_treated" = vids1[!vids1 %in% 4], "datenum" = vtime) %>%
  arrange(id, id_treated, datenum)
pb <- txtProgressBar(max = length(vsite)*length(vnames)*(length(vids1)-1), style = 3) 
counter <- 1
for(X in vsite){
  #Subset data and identify the hexagons ids
  df1 <- df %>%
    filter(id == X | treated == 0)
  vids2 <- sort(unique(df1$id))
  for(W in vnames){
    dftemp1 <- data.frame()
    for(Z in vids2){
      #Make each id the treated hexagon
      df1 <- df1 %>%
        mutate(treated = ifelse(id %in% Z, 1, 0))     
      #Run model
      f1 <- as.formula(paste0(W, " ~ treated"))
      syn <- suppressMessages(augsynth(f1, unit = id, time = datenum, data = df1, 
                      progfunc = "None", scm = TRUE, t_int = 2021.83))
      #Keep results
      dftemp2 <- data.frame("id" = X, "id_treated" = Z, "datenum" = sort(unique(df$datenum))) %>%
        mutate(ygap = as.vector(syn$data$synth_data$Y1plot) - 
                 as.vector(syn$data$synth_data$Y0plot %*% syn$weights)) %>%
        rename(!!W := ygap)
      dftemp1 <- dftemp1 %>%
        bind_rows(dftemp2)
      #Keep track of progress
      setTxtProgressBar(pb, counter) 
      counter <- counter + 1
    }
    if(X == 4){
      dfest1 <- dfest1 %>%  
        left_join(dftemp1, by = c("id", "id_treated", "datenum"))
    } else{
      dfest2 <- dfest2 %>%  
        left_join(dftemp1, by = c("id", "id_treated", "datenum"))  
    }
  } 
}
dfest <- dfest1 %>%
  bind_rows(dfest2)
#fwrite(dfest, "../Data/Synth placebo public_safety hexagon1.csv")
#dfest <- fread("../Data/Synth placebo public_safety hexagon1.csv", integer64 = "character")
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions






########################################################################################################-
## Regression: Synthetic control on crime, arrest, call, summons (three hexagons-high crime city) ######
########################################################################################################-
#Import data (only kept a random sample of 300 (~33%) hexagons from the top decile to reduce computational time)
#Aggregate the three hexagons into a single group, estimating its mean, and renaming such ids.
#Had to change manually the precinct and rank of the treated to ensure they are in a single precinct/rank2
df <- fread("../Data/Crime month-hexagon city data 2018-2022.csv", integer64 = "character") %>%
  filter(year %in% 2019:2022) %>%
  filter(hexsample == 3 | is.na(hexsample)) %>%
  mutate(bimonth = ifelse(month %in% 1:2, 1, 6),
         bimonth = ifelse(month %in% 3:4, 2, bimonth), 
         bimonth = ifelse(month %in% 5:6, 3, bimonth), 
         bimonth = ifelse(month %in% 7:8, 4, bimonth), 
         bimonth = ifelse(month %in% 9:10, 5, bimonth)) %>%
  group_by(year, bimonth, id, site, idsite, precinct, rank2) %>%
  summarise_at(vars(crime_murder:call_qol), sum) %>%
  mutate(id = ifelse(idsite == 0, id, idsite), 
         precinct = ifelse(id %in% 2, 34, precinct), 
         rank2 = ifelse(id %in% 2, 33, rank2)) %>%
  group_by(year, bimonth, id, site, precinct, rank2) %>%
  summarise_at(vars(crime_assault:crime_allassault, arrest_weapon, arrest_drugs, summons, call_crime:call_qol), mean) %>%
  ungroup() %>%
  mutate(treated = ifelse(site %in% "OnPoint NYC", 1, 0), 
         datenum = round(year + (bimonth - 1) /6, 3), 
         post = ifelse(datenum >= 2021.83, 1, 0), 
         post_treated = treated*post) 
#Identifying the 250 unsafest hexagons in the top 10 unsafe precincts (excluding the treated ones and Times Squares)
vids <- df %>%
  filter(post == 0) %>%
  group_by(precinct, treated, rank2, id) %>%
  summarise(crime_index2 = sum(crime_index - crime_theft)) %>%
  ungroup() %>%
  filter(!precinct %in% c(14,25,34)) %>%
  filter(rank2 %in% 1:9) %>%
  mutate(rankhex2 = min_rank(-crime_index2)) %>%
  filter(rankhex2 <= 250) %>%
  pull(id)
df <- df %>% 
  filter(id %in% vids | treated == 1)
#Confirm sample and randomness
length(unique(df$id))
sfpd <- read_sf("../Data/NYC_Police_Precincts/NYC_Police_Precincts.shp") 
sfgrid <- read_sf("../Data/Grid/Hexagonal grid.shp") %>%
  filter(id %in% vids) %>%
  mutate(sample = ifelse(id %in% vids, "yes", "no"))
ggplot() +
  geom_sf(data = sfgrid, aes(fill = sample)) +
  geom_sf(data = sfpd, fill = NA) +
  theme_classic()
rm(sfgrid, sfpd, vids); gc()
#Vectors for the loop
vnames <- df %>%
  select(crime_violentall, crime_property, arrest_weapon, arrest_drugs, summons, call_crime, 
         call_trespass, call_medic, call_qol, call311_homeless, call311_disorder) %>%
  names()
vtime <- sort(unique(df$datenum))
vsite <- c(1, 2)


#Synth method: observed and synthetic hexagon, and weights
dfest1 <- data.frame("id" = 1, "synthetic" = rep(0:1, each = length(vtime)), 
                     "datenum" = rep(vtime, times = 2))
dfest2 <- data.frame("id" = 2, "synthetic" = rep(0:1, each = length(vtime)), 
                     "datenum" = rep(vtime, times = 2))
dfweight1 <- data.frame("id" = 1, "idhex" = sort(unique(df[df$treated==0,]$id)))
dfweight2 <- data.frame("id" = 2, "idhex" = sort(unique(df[df$treated==0,]$id)))
for(X in vsite){
  #Subset data
  df1 <- df %>%
    filter(id == X | treated == 0)  
  #Run over each outcome
  for(W in vnames){
    #Run model
    f1 <- as.formula(paste0(W, " ~ treated"))
    syn <- augsynth(f1, unit = id, time = datenum, data = df1, 
                    progfunc = "None", scm = TRUE, t_int = 2021.83) 
    #Extract observed and synthetic outcomes
    dftemp1 <- data.frame("synthetic" = 0, "datenum" = vtime, 
                          "outcome" = as.vector(syn$data$synth_data$Y1plot)) %>%
      rename(!!W := outcome) %>%
      bind_rows(data.frame("synthetic" = 1, "datenum" = vtime, 
                           "outcome" = as.vector(syn$data$synth_data$Y0plot %*% syn$weights)) %>%
                  rename(!!W := outcome))
    dftemp2 <- data.frame("idhex" = as.numeric(rownames(syn$weights)), 
                          "outcome" = syn$weights) %>%
      rename(!!W := outcome)
    #Keep results
    if(X == 1){
      dfest1 <- dfest1 %>%
        left_join(dftemp1, by = c("synthetic", "datenum"))
      dfweight1 <- dfweight1 %>%
        left_join(dftemp2, by = "idhex")  
    } else {
      dfest2 <- dfest2 %>%
        left_join(dftemp1, by = c("synthetic", "datenum"))
      dfweight2 <- dfweight2 %>%
        left_join(dftemp2, by = "idhex")  
    }
  }
}
dfest <- dfest1 %>%
  bind_rows(dfest2)
dfweight <- dfweight1 %>%
  bind_rows(dfweight2)
#fwrite(dfest, "../Data/Synth gap public_safety hexagon2.csv")
#fwrite(dfweight, "../Data/Synth weights public_safety hexagon2.csv")
#dfest <- fread("../Data/Synth gap public_safety hexagon2.csv", integer64 = "character")
#dfweight <- fread("../Data/Synth weights public_safety hexagon2.csv", integer64 = "character")
rm(df1, dfest, dfest1, dfest2, dfweight, dfweight1, dfweight2, syn, f1, W, X, dftemp1, dftemp2); gc()


#Synth method: placebo
vids1 <- sort(unique(df$id))
dfest1 <- expand_grid("id" = 1, "id_treated" = vids1[!vids1 %in% 2], "datenum" = vtime) %>%
  arrange(id, id_treated, datenum)
dfest2 <- expand_grid("id" = 2, "id_treated" = vids1[!vids1 %in% 1], "datenum" = vtime) %>%
  arrange(id, id_treated, datenum)
pb <- txtProgressBar(max = length(vsite)*length(vnames)*(length(vids1)-1), style = 3) 
counter <- 1
for(X in vsite){
  #Subset data and identify the hexagons ids
  df1 <- df %>%
    filter(id == X | treated == 0)
  vids2 <- sort(unique(df1$id))
  for(W in vnames){
    dftemp1 <- data.frame()
    for(Z in vids2){
      #Make each id the treated hexagon
      df1 <- df1 %>%
        mutate(treated = ifelse(id %in% Z, 1, 0))     
      #Run model
      f1 <- as.formula(paste0(W, " ~ treated"))
      syn <- suppressMessages(augsynth(f1, unit = id, time = datenum, data = df1, 
                                       progfunc = "None", scm = TRUE, t_int = 2021.83))
      #Keep results
      dftemp2 <- data.frame("id" = X, "id_treated" = Z, "datenum" = sort(unique(df$datenum))) %>%
        mutate(ygap = as.vector(syn$data$synth_data$Y1plot) - 
                 as.vector(syn$data$synth_data$Y0plot %*% syn$weights)) %>%
        rename(!!W := ygap)
      dftemp1 <- dftemp1 %>%
        bind_rows(dftemp2)
      #Keep track of progress
      setTxtProgressBar(pb, counter) 
      counter <- counter + 1
    }
    if(X == 1){
      dfest1 <- dfest1 %>%  
        left_join(dftemp1, by = c("id", "id_treated", "datenum"))
    } else{
      dfest2 <- dfest2 %>%  
        left_join(dftemp1, by = c("id", "id_treated", "datenum"))  
    }
  } 
}
dfest <- dfest1 %>%
  bind_rows(dfest2)
#fwrite(dfest, "../Data/Synth placebo public_safety hexagon2.csv")
#dfest <- fread("../Data/Synth placebo public_safety hexagon2.csv", integer64 = "character")
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions






#####################################################################################-
## Regression: Randomization inference diff-in-diff Poisson (single hexagon) ########
#####################################################################################-
#Import data and create time dummy variables (removing 2018 because it is a noisy year)
df <- fread("../Data/Crime month-hexagon1 data 2018-2022.csv", integer64 = "character") %>%
  filter(direct == 1 & year %in% 2019:2022) %>%
  mutate(datenum = year + (month - 1) /12, 
         post = ifelse(datenum >= 2021.91, 1, 0))

#Regression
dfest <- data.frame("outcome" = character(), "treated_idsites" = character(), "est" = numeric(), 
                    "se" = numeric(), "irr" = numeric(), "irr.se" = numeric(),
                    "irr.low" = numeric(), "irr.up" = numeric(), "pvalue" = numeric(),
                    stringsAsFactors = FALSE)
vnames <- c("arrest_weapon", "arrest_drugs", "call_crime", "call_trespass", "call_medic", "summons")
vtreat <- combn(3:19, 2, simplify = FALSE)
vtreat[[137]] <- c(1, 2)
counter <- 1
pb <- txtProgressBar(max = length(vtreat)*length(vnames), style = 3) 
for(X in vnames){
  for(W in 1:length(vtreat)){
    #Change treated idsites
    df <- df %>%
      mutate(treated = ifelse(idsite %in% vtreat[[W]], 1, 0), 
             datenum = year + (month - 1) /12, 
             post = ifelse(datenum >= 2021.91, 1, 0), 
             post_treated = treated*post)
    #Month-year and individual FE
    f1 <- as.formula(paste0(X, " ~ post_treated | datenum + id"))
    lm1 <- fepois(f1, cluster = "id", data = df, warn = FALSE, notes = FALSE)  
    #Extract coefficient and SE
    dfest <- dfest %>%
      bind_rows(data.frame("outcome" = X, 
                           "treated_idsites" = paste0(vtreat[[W]], collapse = "-"), 
                           "est" = lm1$coefficients["post_treated"], 
                           "se" = lm1$se["post_treated"], 
                           "irr" = exp(lm1$coefficients["post_treated"]), 
                           "irr.se" = (exp(lm1$coefficients["post_treated"])*sqrt(diag(vcov(lm1)))["post_treated"]), 
                           "irr.low" = exp(lm1$coefficients["post_treated"] - 1.96*sqrt(diag(vcov(lm1)))["post_treated"]),
                           "irr.up" = exp(lm1$coefficients["post_treated"] + 1.96*sqrt(diag(vcov(lm1)))["post_treated"]),
                           "pvalue" = lm1$coeftable["post_treated","Pr(>|t|)"],
                           "n" = lm1$nobs)) 
    setTxtProgressBar(pb, counter)
    counter <- counter + 1
  }
}
close(pb)
#fwrite(dfest, "../Data/Diff in diff Poisson public_safety1 randominference.csv")
#dfest <- fread("../Data/Diff in diff Poisson public_safety1 randominference.csv")
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions





#####################################################################################-
## Regression: Randomization inference diff-in-diff Poisson (three hexagons) ########
#####################################################################################-
#Import data and create time dummy variables (removing 2018 because it is a noisy year)
df <- fread("../Data/Crime month-hexagon2 data 2018-2022.csv", integer64 = "character") %>%
  filter(direct == 1 & year %in% 2019:2022) %>%
  mutate(datenum = year + (month - 1) /12, 
         post = ifelse(datenum >= 2021.91, 1, 0))

#Regression
dfest <- data.frame("outcome" = character(), "treated_idsites" = character(), "est" = numeric(), 
                    "se" = numeric(), "irr" = numeric(), "irr.se" = numeric(),
                    "irr.low" = numeric(), "irr.up" = numeric(), "pvalue" = numeric(),
                    stringsAsFactors = FALSE)
vnames <- c("arrest_weapon", "arrest_drugs", "call_crime", "call_trespass", "call_medic", "summons")
vtreat <- combn(3:19, 2, simplify = FALSE)
vtreat[[137]] <- c(1, 2)
counter <- 1
pb <- txtProgressBar(max = length(vtreat)*length(vnames), style = 3) 
for(X in vnames){
  for(W in 1:length(vtreat)){
    #Change treated idsites
    df <- df %>%
      mutate(treated = ifelse(idsite %in% vtreat[[W]], 1, 0), 
             datenum = year + (month - 1) /12, 
             post = ifelse(datenum >= 2021.91, 1, 0), 
             post_treated = treated*post)
    #Month-year and individual FE
    f1 <- as.formula(paste0(X, " ~ post_treated | datenum + id"))
    lm1 <- fepois(f1, cluster = "id", data = df, warn = FALSE, notes = FALSE)  
    #Extract coefficient and SE
    dfest <- dfest %>%
      bind_rows(data.frame("outcome" = X, 
                           "treated_idsites" = paste0(vtreat[[W]], collapse = "-"), 
                           "est" = lm1$coefficients["post_treated"], 
                           "se" = lm1$se["post_treated"], 
                           "irr" = exp(lm1$coefficients["post_treated"]), 
                           "irr.se" = (exp(lm1$coefficients["post_treated"])*sqrt(diag(vcov(lm1)))["post_treated"]), 
                           "irr.low" = exp(lm1$coefficients["post_treated"] - 1.96*sqrt(diag(vcov(lm1)))["post_treated"]),
                           "irr.up" = exp(lm1$coefficients["post_treated"] + 1.96*sqrt(diag(vcov(lm1)))["post_treated"]),
                           "pvalue" = lm1$coeftable["post_treated","Pr(>|t|)"],
                           "n" = lm1$nobs)) 
    setTxtProgressBar(pb, counter)
    counter <- counter + 1
  }
}
close(pb)
#fwrite(dfest, "../Data/Diff in diff Poisson public_safety2 randominference.csv")
#dfest <- fread("../Data/Diff in diff Poisson public_safety2 randominference.csv")
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions







###############################################################################-
## Figure: treated and control hexagonal areas (all city) #####################
###############################################################################-
#Import data
sfgrid <- read_sf("../Data/Grid/Hexagonal grid.shp") 
sfpd <- read_sf("../Data/NYC_Police_Precincts/NYC_Police_Precincts.shp") %>% 
  rename_all(tolower)
dfcrime <- fread("../Data/NYPD_complaint_sample.csv")
df <- fread("../Data/Injection site addresses gps.csv") %>%
  mutate(group = ifelse(site %in% "OnPoint NYC", "0", "1"), 
         site = ifelse(grepl("VOCAL-NY", site), "VOCAL-NY", site), 
         site = str_remove_all(site, ", Inc."), 
         site = str_replace(site, "Lower East Side", "LES")) %>%
  group_by(site) %>%
  mutate(siteid1 = cur_group_id(), 
         siteid1 = LETTERS[siteid1], 
         siteid2 = paste0(siteid1, ": ", site)) 

#### 1. Plot all sites in a map ####-
p <- ggplot() +
  geom_sf(data = sfpd, fill = NA, color = "grey60", linewidth = 0.5) +
  geom_point(aes(x = long, y = lat, shape = group, fill = group, color = factor(siteid2)), size = 4, data = df) +
  geom_point(aes(x = long, y = lat), size = 4, shape = 24, fill = "darkred", data = df %>% filter(site == "OnPoint NYC")) +
  geom_text(data = df %>% filter(site != "OnPoint NYC"), aes(x = long, y = lat, label = siteid1), nudge_x = -0.017) +
  scale_fill_manual(values = c("darkred", "darkblue"), labels = c("Intervention", "Comparison")) +
  scale_shape_manual(values = c(24, 21), labels = c("Intervention", "Comparison")) +
  scale_color_manual(values = rep("white", 12)) +
  labs(x = NULL, y = NULL, shape = NULL, fill = NULL, color = NULL) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    strip.text.x = element_text(size = 12),
    strip.background = element_blank(),
    panel.background = element_blank(), axis.line = element_blank(),
    panel.border = element_rect(colour = "grey60", fill = NA),
    legend.key = element_rect(colour = "transparent", fill = "transparent"),
    legend.text = element_text(size = 14),
    legend.position = "right")
ggsave("../Tables-Figures/MapSite.jpg", plot = p, width = 9, height = 5, dpi = 300)
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions


#### 2. Plot all sites in a map (no legends) ####-
p <- ggplot() +
  geom_sf(data = sfpd, fill = NA, color = "grey60", linewidth = 0.5) +
  geom_point(aes(x = long, y = lat, shape = group, fill = group, color = factor(siteid2)), size = 4, data = df) +
  geom_point(aes(x = long, y = lat), size = 4, shape = 24, fill = "darkred", data = df %>% filter(site == "OnPoint NYC")) +
  geom_text(data = df %>% filter(site != "OnPoint NYC"), aes(x = long, y = lat, label = siteid1), nudge_x = -0.017) +
  scale_fill_manual(values = c("darkred", "darkblue"), labels = c("Intervention", "Comparison")) +
  scale_shape_manual(values = c(24, 21), labels = c("Intervention", "Comparison")) +
  scale_color_manual(values = rep("white", 12)) +
  labs(x = NULL, y = NULL, shape = NULL, fill = NULL, color = NULL) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    strip.text.x = element_text(size = 12),
    strip.background = element_blank(),
    panel.background = element_blank(), axis.line = element_blank(),
    panel.border = element_rect(colour = "grey60", fill = NA),
    legend.key = element_rect(colour = "transparent", fill = "transparent"),
    legend.text = element_text(size = 14),
    legend.position = "none")
ggsave("../Tables-Figures/MapSite_nolegend.jpg", plot = p, width = 9, height = 5, dpi = 300)
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions


#### 3. Plot all sites in a map (plain) ####-
p <- ggplot() +
  geom_sf(data = sfpd, fill = NA, color = "grey60", linewidth = 0.5) +
  #geom_point(aes(x = long, y = lat, shape = group, fill = group, color = factor(siteid2)), size = 4, data = df) +
  #geom_point(aes(x = long, y = lat), size = 4, shape = 24, fill = "darkred", data = df %>% filter(site == "OnPoint NYC")) +
  #geom_text(data = df %>% filter(site != "OnPoint NYC"), aes(x = long, y = lat, label = siteid1), nudge_x = -0.017) +
  scale_fill_manual(values = c("darkred", "darkblue"), labels = c("Intervention", "Comparison")) +
  scale_shape_manual(values = c(24, 21), labels = c("Intervention", "Comparison")) +
  scale_color_manual(values = rep("white", 12)) +
  labs(x = NULL, y = NULL, shape = NULL, fill = NULL, color = NULL) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    strip.text.x = element_text(size = 12),
    strip.background = element_blank(),
    panel.background = element_blank(), axis.line = element_blank(),
    panel.border = element_rect(colour = "grey60", fill = NA),
    legend.key = element_rect(colour = "transparent", fill = "transparent"),
    legend.text = element_text(size = 14),
    legend.position = "none")
ggsave("../Tables-Figures/MapSite_plain.jpg", plot = p, width = 6, height = 5, dpi = 300)
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions



###############################################################################-
## Figure: treated site maps (single and three hexagons) ######################
###############################################################################-
#Import data
sfgrid1 <- read_sf("../Data/Grid/Hexagonal grid sites1.shp") %>%
  filter(direct == 1 & idsite == 1)
sfgrid2 <- read_sf("../Data/Grid/Hexagonal grid sites2.shp") %>%
  filter(direct == 1 & idsite == 1)
dfcrime <- fread("../Data/NYPD_complaint_sample.csv")
df <- fread("../Data/Injection site addresses gps.csv") %>%
  slice(1)

#Map parameters
vx1 <- 0.006
vy1 <- 0.005
vx2 <- 0.007
vy2 <- 0.007

#Plot map single hexagon
p <- ggplot() +
  base_map(st_bbox(c(xmin = df$long-vx2, xmax = df$long+vx2, 
                     ymin = df$lat-vy2, ymax = df$lat+vy2), 
                   crs = st_crs(4326)), basemap = 'google', increase_zoom = 2) +
  geom_sf(data = sfgrid1, fill = "purple", alpha = 0.4) +
  geom_point(aes(x = longitude, y = latitude), color = "darkred", size = 1, shape = 16, alpha = 0.5, data = dfcrime) +
  geom_point(aes(x = long, y = lat), color = "black", size = 9, shape = 18, data = df) +
  coord_sf(xlim = c(df$long-vx1, df$long+vx1), 
           ylim = c(df$lat-vy1, df$lat+vy1)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme(axis.text = element_blank(), axis.ticks = element_blank())
ggsave("../Tables-Figures/MapSite126st1.jpg", plot = p, width = 6, height = 6, dpi = 300)

#Plot map three hexagons
p <- ggplot() +
  base_map(st_bbox(c(xmin = df$long-vx2, xmax = df$long+vx2, 
                     ymin = df$lat-vy2, ymax = df$lat+vy2), 
                   crs = st_crs(4326)), basemap = 'google', increase_zoom = 2) +
  geom_sf(data = sfgrid2, fill = "purple", alpha = 0.4) +
  geom_point(aes(x = longitude, y = latitude), color = "darkred", size = 1, shape = 16, alpha = 0.5, data = dfcrime) +
  geom_point(aes(x = long, y = lat), color = "black", size = 9, shape = 18, data = df) +
  coord_sf(xlim = c(df$long-vx1, df$long+vx1), 
           ylim = c(df$lat-vy1, df$lat+vy1)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme(axis.text = element_blank(), axis.ticks = element_blank())
ggsave("../Tables-Figures/MapSite126st2.jpg", plot = p, width = 6, height = 6, dpi = 300)
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions
#Statistics in text
#st_area(sfgrid1) #area of the polygon






###########################################################################################-
## Figure: Event study design Poisson crime, arrest, call, summons (single hexagon) #######
###########################################################################################-
#Import data
dfest <- fread("../Data/Event-study Poisson public_safety1.csv") %>%
  filter(grepl("crime|call|summons|arrest_drugs|arrest_weapon", outcome))
dfest <- dfest %>%
  bind_rows(data.frame("outcome" = rep(unique(dfest$outcome), 2), 
                       "model" = unique(dfest$model),
                       "time" = -1, 
                       "est" = 0, 
                       "se" = 0)) %>%
  mutate(ci.low = est - 1.96*se, 
         ci.up = est + 1.96*se)
plottheme <- theme(
  axis.text.x = element_text(size = 20),
  axis.text.y = element_text(size = 20),
  axis.title.x = element_text(size = 15),
  axis.title.y = element_text(size = 15),
  strip.text.x = element_text(size = 20),
  strip.background = element_rect(fill = "grey85", color = "black"),
  panel.background = element_blank(), axis.line = element_line(colour = "black"),
  legend.key = element_rect(colour = "transparent", fill = "transparent"),
  legend.key.width = unit(2, "cm"),
  legend.text = element_text(size = 18),
  legend.title = element_text(size = 18),
  legend.position = "top",
  rect = element_rect(fill = "transparent"))
vnames <- unique(dfest$outcome)
vmodel <- unique(dfest$model)

#Plot
for(X in vnames){
  for(W in vmodel){
    #Subset data
    dftemp <- dfest %>%
      filter(outcome == X & model == W) 
    if(W == "month_id_fe"){
      W <- ""
    } else{
      W <- "trend"
    }
    #Plot
    p <- ggplot(aes(x = time, y = est, group = model), data = dftemp) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
      geom_pointrange(aes(ymin = ci.low, ymax = ci.up), color = "darkblue") +
      geom_point(color = "darkblue") +
      geom_vline(xintercept = -0.4, linetype = "dashed", color = "grey70") +
      labs(x = "Quarters relative to December 2021", y = "Poisson regression coefficient") +
      scale_x_continuous(breaks = seq(-9, 2, 2)) +
      plottheme
    ggsave(paste0("../Tables-Figures/EventStudyPoisson_", X, "1", W, ".jpg"), plot = p, width = 6, height = 5, dpi = 144) 
  }
}
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions





#########################################################################################-
## Figure: Event study design Poisson crime, arrest, call, summons (three hexagons) #####
#########################################################################################-
#Import data
dfest <- fread("../Data/Event-study Poisson public_safety2.csv") %>%
  filter(grepl("crime|call|summons|arrest_drugs|arrest_weapon", outcome))
dfest <- dfest %>%
  bind_rows(data.frame("outcome" = rep(unique(dfest$outcome), 2), 
                       "model" = unique(dfest$model),
                       "time" = -1, 
                       "est" = 0, 
                       "se" = 0)) %>%
  mutate(ci.low = est - 1.96*se, 
         ci.up = est + 1.96*se)
plottheme <- theme(
  axis.text.x = element_text(size = 20),
  axis.text.y = element_text(size = 20),
  axis.title.x = element_text(size = 15),
  axis.title.y = element_text(size = 15),
  strip.text.x = element_text(size = 20),
  strip.background = element_rect(fill = "grey85", color = "black"),
  panel.background = element_blank(), axis.line = element_line(colour = "black"),
  legend.key = element_rect(colour = "transparent", fill = "transparent"),
  legend.key.width = unit(2, "cm"),
  legend.text = element_text(size = 18),
  legend.title = element_text(size = 18),
  legend.position = "top",
  rect = element_rect(fill = "transparent"))
vnames <- unique(dfest$outcome)
vmodel <- unique(dfest$model)

#Plot
for(X in vnames){
  for(W in vmodel){
    #Subset data
    dftemp <- dfest %>%
      filter(outcome == X & model == W) 
    if(W == "month_id_fe"){
      W <- ""
    } else{
      W <- "trend"
    }
    #Plot
    p <- ggplot(aes(x = time, y = est, group = model), data = dftemp) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
      geom_pointrange(aes(ymin = ci.low, ymax = ci.up), color = "darkblue") +
      geom_point(color = "darkblue") +
      geom_vline(xintercept = -0.4, linetype = "dashed", color = "grey70") +
      labs(x = "Quarters relative to December 2021", y = "Poisson regression coefficient") +
      scale_x_continuous(breaks = seq(-9, 2, 2)) +
      plottheme
    ggsave(paste0("../Tables-Figures/EventStudyPoisson_", X, "2", W, ".jpg"), plot = p, width = 6, height = 5, dpi = 144) 
  }
}
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions




#################################################################################################-
## Figure: Event study design Poisson crime, call, summons (single hexagon-high crime city) #####
#################################################################################################-
#Import data
dfest <- fread("../Data/Event-study Poisson highcrimecity public_safety1.csv") %>%
  filter(grepl("crime|call|summons|arrest_drugs|arrest_weapon", outcome))
dfest <- dfest %>%
  bind_rows(data.frame("outcome" = rep(unique(dfest$outcome), 2), 
                       "model" = unique(dfest$model),
                       "time" = -1, 
                       "est" = 0, 
                       "se" = 0)) %>%
  mutate(ci.low = est - 1.96*se, 
         ci.up = est + 1.96*se)
plottheme <- theme(
  axis.text.x = element_text(size = 20),
  axis.text.y = element_text(size = 20),
  axis.title.x = element_text(size = 15),
  axis.title.y = element_text(size = 15),
  strip.text.x = element_text(size = 20),
  strip.background = element_rect(fill = "grey85", color = "black"),
  panel.background = element_blank(), axis.line = element_line(colour = "black"),
  legend.key = element_rect(colour = "transparent", fill = "transparent"),
  legend.key.width = unit(2, "cm"),
  legend.text = element_text(size = 18),
  legend.title = element_text(size = 18),
  legend.position = "top",
  rect = element_rect(fill = "transparent"))
vnames <- unique(dfest$outcome)
vmodel <- unique(dfest$model)

#Plot
for(X in vnames){
  for(W in vmodel){
    #Subset data
    dftemp <- dfest %>%
      filter(outcome == X & model == W) 
    if(W == "month_id_fe"){
      W <- ""
    } else{
      W <- "trend"
    }
    #Plot
    p <- ggplot(aes(x = time, y = est, group = model), data = dftemp) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
      geom_pointrange(aes(ymin = ci.low, ymax = ci.up), color = "darkblue") +
      geom_point(color = "darkblue") +
      geom_vline(xintercept = -0.4, linetype = "dashed", color = "grey70") +
      labs(x = "Quarters relative to December 2021", y = "Poisson regression coefficient") +
      scale_x_continuous(breaks = seq(-9, 2, 2)) +
      plottheme
    ggsave(paste0("../Tables-Figures/EventStudyPoisson_highcity_", X, "1", W, ".jpg"), plot = p, width = 6, height = 5, dpi = 144) 
  }
}
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions





#################################################################################################-
## Figure: Event study design Poisson crime, call, summons (three hexagons-high crime city) #####
#################################################################################################-
#Import data
dfest <- fread("../Data/Event-study Poisson highcrimecity public_safety2.csv") %>%
  filter(grepl("crime|call|summons|arrest_drugs|arrest_weapon", outcome))
dfest <- dfest %>%
  bind_rows(data.frame("outcome" = rep(unique(dfest$outcome), 2), 
                       "model" = unique(dfest$model),
                       "time" = -1, 
                       "est" = 0, 
                       "se" = 0)) %>%
  mutate(ci.low = est - 1.96*se, 
         ci.up = est + 1.96*se)
plottheme <- theme(
  axis.text.x = element_text(size = 20),
  axis.text.y = element_text(size = 20),
  axis.title.x = element_text(size = 15),
  axis.title.y = element_text(size = 15),
  strip.text.x = element_text(size = 20),
  strip.background = element_rect(fill = "grey85", color = "black"),
  panel.background = element_blank(), axis.line = element_line(colour = "black"),
  legend.key = element_rect(colour = "transparent", fill = "transparent"),
  legend.key.width = unit(2, "cm"),
  legend.text = element_text(size = 18),
  legend.title = element_text(size = 18),
  legend.position = "top",
  rect = element_rect(fill = "transparent"))
vnames <- unique(dfest$outcome)
vmodel <- unique(dfest$model)

#Plot
for(X in vnames){
  for(W in vmodel){
    #Subset data
    dftemp <- dfest %>%
      filter(outcome == X & model == W) 
    if(W == "month_id_fe"){
      W <- ""
    } else{
      W <- "trend"
    }
    #Plot
    p <- ggplot(aes(x = time, y = est, group = model), data = dftemp) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
      geom_pointrange(aes(ymin = ci.low, ymax = ci.up), color = "darkblue") +
      geom_point(color = "darkblue") +
      geom_vline(xintercept = -0.4, linetype = "dashed", color = "grey70") +
      labs(x = "Quarters relative to December 2021", y = "Poisson regression coefficient") +
      scale_x_continuous(breaks = seq(-9, 2, 2)) +
      plottheme
    ggsave(paste0("../Tables-Figures/EventStudyPoisson_highcity_", X, "2", W, ".jpg"), plot = p, width = 6, height = 5, dpi = 144) 
  }
}
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions






###################################################################################################-
## Figure: Event study design Poisson crime, call, summons (single hexagon-high drug arrests) #####
###################################################################################################-
#Import data
dfest <- fread("../Data/Event-study Poisson highdrugarrests public_safety1.csv") %>%
  filter(grepl("crime|call|summons|arrest_drugs|arrest_weapon", outcome))
dfest <- dfest %>%
  bind_rows(data.frame("outcome" = rep(unique(dfest$outcome), 2), 
                       "model" = unique(dfest$model),
                       "time" = -1, 
                       "est" = 0, 
                       "se" = 0)) %>%
  mutate(ci.low = est - 1.96*se, 
         ci.up = est + 1.96*se)
plottheme <- theme(
  axis.text.x = element_text(size = 20),
  axis.text.y = element_text(size = 20),
  axis.title.x = element_text(size = 15),
  axis.title.y = element_text(size = 15),
  strip.text.x = element_text(size = 20),
  strip.background = element_rect(fill = "grey85", color = "black"),
  panel.background = element_blank(), axis.line = element_line(colour = "black"),
  legend.key = element_rect(colour = "transparent", fill = "transparent"),
  legend.key.width = unit(2, "cm"),
  legend.text = element_text(size = 18),
  legend.title = element_text(size = 18),
  legend.position = "top",
  rect = element_rect(fill = "transparent"))
vnames <- unique(dfest$outcome)
vmodel <- unique(dfest$model)

#Plot
for(X in vnames){
  for(W in vmodel){
    #Subset data
    dftemp <- dfest %>%
      filter(outcome == X & model == W) 
    if(W == "month_id_fe"){
      W <- ""
    } else{
      W <- "trend"
    }
    #Plot
    p <- ggplot(aes(x = time, y = est, group = model), data = dftemp) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
      geom_pointrange(aes(ymin = ci.low, ymax = ci.up), color = "darkblue") +
      geom_point(color = "darkblue") +
      geom_vline(xintercept = -0.4, linetype = "dashed", color = "grey70") +
      labs(x = "Quarters relative to December 2021", y = "Poisson regression coefficient") +
      scale_x_continuous(breaks = seq(-9, 2, 2)) +
      plottheme
    ggsave(paste0("../Tables-Figures/EventStudyPoisson_highdrugs_", X, "1", W, ".jpg"), plot = p, width = 6, height = 5, dpi = 144) 
  }
}
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions




###################################################################################################-
## Figure: Event study design Poisson crime, call, summons (three hexagons-high drug arrests) #####
###################################################################################################-
#Import data
dfest <- fread("../Data/Event-study Poisson highdrugarrests public_safety2.csv") %>%
  filter(grepl("crime|call|summons|arrest_drugs|arrest_weapon", outcome))
dfest <- dfest %>%
  bind_rows(data.frame("outcome" = rep(unique(dfest$outcome), 2), 
                       "model" = unique(dfest$model),
                       "time" = -1, 
                       "est" = 0, 
                       "se" = 0)) %>%
  mutate(ci.low = est - 1.96*se, 
         ci.up = est + 1.96*se)
plottheme <- theme(
  axis.text.x = element_text(size = 20),
  axis.text.y = element_text(size = 20),
  axis.title.x = element_text(size = 15),
  axis.title.y = element_text(size = 15),
  strip.text.x = element_text(size = 20),
  strip.background = element_rect(fill = "grey85", color = "black"),
  panel.background = element_blank(), axis.line = element_line(colour = "black"),
  legend.key = element_rect(colour = "transparent", fill = "transparent"),
  legend.key.width = unit(2, "cm"),
  legend.text = element_text(size = 18),
  legend.title = element_text(size = 18),
  legend.position = "top",
  rect = element_rect(fill = "transparent"))
vnames <- unique(dfest$outcome)
vmodel <- unique(dfest$model)

#Plot
for(X in vnames){
  for(W in vmodel){
    #Subset data
    dftemp <- dfest %>%
      filter(outcome == X & model == W) 
    if(W == "month_id_fe"){
      W <- ""
    } else{
      W <- "trend"
    }
    #Plot
    p <- ggplot(aes(x = time, y = est, group = model), data = dftemp) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
      geom_pointrange(aes(ymin = ci.low, ymax = ci.up), color = "darkblue") +
      geom_point(color = "darkblue") +
      geom_vline(xintercept = -0.4, linetype = "dashed", color = "grey70") +
      labs(x = "Quarters relative to December 2021", y = "Poisson regression coefficient") +
      scale_x_continuous(breaks = seq(-9, 2, 2)) +
      plottheme
    ggsave(paste0("../Tables-Figures/EventStudyPoisson_highdrugs_", X, "2", W, ".jpg"), plot = p, width = 6, height = 5, dpi = 144) 
  }
}
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions





###########################################################################################-
## Figure: Map pre-intervention hexagon index crimes (single hexagon-high crime city) #####
###########################################################################################-
#Import data
sfpd <- read_sf("../Data/NYC_Police_Precincts/NYC_Police_Precincts.shp") 
sfgrid1 <- read_sf("../Data/Grid/Hexagonal grid sites1.shp") %>%
  filter(direct == 1 & idsite %in% 1:2) %>%
  select(id, precinct)
df <- fread("../Data/Crime month-hexagon city data 2018-2022.csv", integer64 = "character") %>%
  filter(year %in% 2019:2022) %>%
  filter(hexsample == 1 | is.na(hexsample)) %>%
  mutate(treated = ifelse(site %in% "OnPoint NYC", 1, 0)) %>%
  mutate(datenum = year + (month - 1) /12, 
         post = ifelse(datenum >= 2021.91, 1, 0))
#Pre-intervention crimes, identifying the hexagons in the top unsafe precincts
dftemp <- df %>%
  filter(post == 0) %>%
  group_by(precinct, treated, rank, rank2, id) %>%
  summarise(crime_index2 = sum(crime_index - crime_theft),
            crime_index = sum(crime_index), 
            crime_murder = sum(crime_murder)) %>%
  ungroup()
dfrank <- dftemp %>%
  filter(!precinct %in% c(25,34)) %>%
  filter(rank %in% 1:8) 
dfrank2 <- dftemp %>%
  filter(!precinct %in% c(14,25,34)) %>%
  filter(rank2 %in% 1:9) %>%
  mutate(rankhex = min_rank(-crime_index), 
         rankhex2 = min_rank(-crime_index2)) 
unique(dfrank2$precinct) #28 40 41 42 73 75 81
#Identify top
quantile(dftemp$crime_murder, probs = seq(0, 1, 0.01))
plottheme <- theme(
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  strip.text.x = element_text(size = 12),
  strip.background = element_rect(fill = "grey85", color = "black"),
  panel.background = element_blank(), axis.line = element_line(colour = "black"),
  panel.border = element_rect(colour = "black", fill = NA),
  legend.key = element_rect(colour = "transparent", fill = "transparent"),
  legend.text = element_text(size = 17),
  legend.position = c(0.2, 0.8))

#Plot: map of hexagon index crimes
vids <- dfrank %>%
  pull(id)
sfgrid <- read_sf("../Data/Grid/Hexagonal grid.shp") %>%
  filter(!precinct %in% c(25, 34)) %>%
  bind_rows(sfgrid1) %>%
  mutate(sample = ifelse(id %in% vids, 1, 0), 
         sample = ifelse(id %in% c(4, 11), 2, sample),
         sample = factor(sample)) 
p <- ggplot() +
  geom_sf(data = sfgrid, aes(fill = sample), color = "grey30", linewidth = 0.02) +
  geom_sf(data = sfpd, fill = NA, color = "grey50", linewidth = 0.5) +
  geom_sf(data = sfgrid1, fill = "red", color = "grey10", linewidth = 0.01) +
  scale_fill_manual(values = c("grey99", "grey30", "red"), labels = c("Excluded", "Comparison", "Intervention")) +
  labs(x = NULL, y = NULL, fill = NULL) +
  plottheme
ggsave("../Tables-Figures/MapHex_rankcrime_index.jpg", plot = p, width = 5, height = 5, dpi = 144) 

#Plot: map of hexagon index crimes minus theft
vids <- dfrank2 %>%
  filter(rankhex2 <= 250) %>%
  pull(id)
sfgrid <- read_sf("../Data/Grid/Hexagonal grid.shp") %>%
  filter(!precinct %in% c(25, 34)) %>%
  bind_rows(sfgrid1) %>%
  mutate(sample = ifelse(id %in% vids, 1, 0), 
         sample = ifelse(id %in% c(4, 11), 2, sample),
         sample = factor(sample)) 
p <- ggplot() +
  geom_sf(data = sfgrid, aes(fill = sample), color = "white", linewidth = 0.02) +
  geom_sf(data = sfpd, fill = NA, color = "grey60", linewidth = 0.5) +
  geom_sf(data = sfgrid1, fill = "red", color = "grey10", linewidth = 0.01) +
  scale_fill_manual(values = c("white", "grey30", "red"), labels = c("", "Comparison", "Intervention")) +
  labs(x = NULL, y = NULL, fill = NULL) +
  plottheme
ggsave("../Tables-Figures/MapHex_rankcrime_index2.jpg", plot = p, width = 5, height = 5, dpi = 144) 
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions





#############################################################################################-
## Figure: Map pre-intervention hexagon drug arrests (single hexagon-high drug arrests) #####
#############################################################################################-
#Import data
sfpd <- read_sf("../Data/NYC_Police_Precincts/NYC_Police_Precincts.shp") 
sfgrid1 <- read_sf("../Data/Grid/Hexagonal grid sites1.shp") %>%
  filter(direct == 1 & idsite %in% 1:2) %>%
  select(id, precinct)
df <- fread("../Data/Crime month-hexagon city data 2018-2022.csv", integer64 = "character") %>%
  filter(year %in% 2019:2022) %>%
  filter(hexsample == 1 | is.na(hexsample)) %>%
  mutate(treated = ifelse(site %in% "OnPoint NYC", 1, 0)) %>%
  mutate(datenum = year + (month - 1) /12, 
         post = ifelse(datenum >= 2021.91, 1, 0))
#Identifying the 20 hexagons with most drug arrests (excluding central bookin/hospital areas)
vids <- df %>%
  filter(post == 0) %>%
  filter(!precinct %in% c(14,25,34)) %>%
  filter(!id %in% c(11435, 12630)) %>%
  group_by(precinct, id) %>%
  summarise(arrest_drugs = mean(arrest_drugs)) %>%
  ungroup() %>%
  mutate(rankhex2 = min_rank(-arrest_drugs)) %>%
  filter(rankhex2 <= 20) %>%
  pull(id)
df <- df %>% 
  filter(id %in% vids | treated == 1)
plottheme <- theme(
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  strip.text.x = element_text(size = 12),
  strip.background = element_rect(fill = "grey85", color = "black"),
  panel.background = element_blank(), axis.line = element_line(colour = "black"),
  panel.border = element_rect(colour = "black", fill = NA),
  legend.key = element_rect(colour = "transparent", fill = "transparent"),
  legend.text = element_text(size = 17),
  legend.position = c(0.2, 0.8))

#Plot: map of hexagon index crimes
sfgrid <- read_sf("../Data/Grid/Hexagonal grid.shp") %>%
  filter(!precinct %in% c(25, 34)) %>%
  bind_rows(sfgrid1) %>%
  mutate(sample = ifelse(id %in% vids, 1, 0), 
         sample = ifelse(id %in% c(4, 11), 2, sample),
         sample = factor(sample)) 
p <- ggplot() +
  geom_sf(data = sfgrid, aes(fill = sample), color = "white", linewidth = 0.02) +
  geom_sf(data = sfpd, fill = NA, color = "grey80", linewidth = 0.5) +
  geom_sf(data = sfgrid1, fill = "red", color = "grey10", linewidth = 0.01) +
  geom_sf(data = sfgrid %>% filter(sample == "1"), fill = "darkgreen", color = "grey10", linewidth = 0.01) +
  scale_fill_manual(values = c("white", "darkgreen", "red"), labels = c("", "Comparison", "Intervention")) +
  labs(x = NULL, y = NULL, fill = NULL) +
  plottheme
ggsave("../Tables-Figures/MapHex_rankarrest_drugs.jpg", plot = p, width = 5, height = 5, dpi = 144) 
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions





################################################################################-
## Figure: synthetic control figures (single hexagon-high crime city) ##########
################################################################################-
#Import data
dfgap <- fread("../Data/Synth gap public_safety hexagon1.csv", integer64 = "character")
dfweight <- fread("../Data/Synth weights public_safety hexagon1.csv", integer64 = "character")
dfplacebo <- fread("../Data/Synth placebo public_safety hexagon1.csv", integer64 = "character") %>%
  mutate(post = ifelse(datenum >= 2021.83, 1, 0))
plottheme <- theme(
  axis.text.x = element_text(size = 18),
  axis.text.y = element_text(size = 18),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  strip.background = element_rect(fill = "grey85", color = "black"),
  panel.background = element_blank(), axis.line = element_line(colour = "black"),
  legend.key = element_rect(colour = "transparent", fill = "transparent"),
  legend.key.width = unit(1, "cm"),
  legend.text = element_text(size = 18),
  legend.title = element_text(size = 18),
  legend.position = c(0.35, 0.9),
  rect = element_rect(fill = "transparent"))
vsite <- c(4,11)
vnames <- dfgap %>% select(crime_violentall:call311_disorder) %>% names()

#Plot: synthetic, observed incidents
for(W in vsite){
  #Subset data
  dftemp <- dfgap %>%
    filter(id == W) %>%
    mutate(synthetic = factor(synthetic))
  for(X in vnames){
    #Plot
    p <- ggplot(dftemp, aes_string(x = "datenum", y = X)) +
      geom_line(aes(linetype = synthetic), color = "black") +
      geom_vline(xintercept = 2021.833, linetype = "dashed") +
      scale_linetype_manual(values = c("solid", "dashed"), labels = c("Safe injection site", "Synthetic")) +
      labs(x =  NULL, y = "Incidents", linetype = NULL) +
      plottheme    
    ggsave(paste0("../Tables-Figures/Synthgap", W, X, ".jpg"), plot = p, width = 6, height = 5, dpi = 144) 
  }
}
rm(dftemp, p, W, X); gc()

#Plot: placebo
for(W in vsite){
  #Subset data
  dftemp1 <- dfplacebo %>%
    filter(id == W) %>%
    mutate(treated = factor(ifelse(id_treated %in% W, "1", "0")))
  for(X in vnames){
    #Estimate the RMSPE (exclude hexagons with a RMSPE 10 times larger than the treatment)
    dfrmspe <- dftemp1 %>%
      filter(post == 0) %>%
      group_by(id_treated) %>%
      summarise(rmspe = mean( (!!sym(X) - 0)^2 )^.5) %>%
      mutate(ratio = rmspe/rmspe[id_treated==W]) %>%
      ungroup()
    dftemp2 <- dftemp1 %>%
      left_join(dfrmspe, by = "id_treated") %>%
      filter(ratio < 4)
    vrmspe = dftemp2 %>%
      filter(id_treated == W) %>%
      distinct(rmspe) %>%
      pull(rmspe) %>%
      scales::comma(accuracy = 0.1)
    vplacebo <- paste0("Placebo (n=", length(unique(dftemp2$id_treated))-1, ")")
    vcity <- paste0("Treatment (RMSPE=", vrmspe, ")")
    #Remove a few outliers to improve the visualization
    if(W == 4 & X == "crime_violent") dftemp2 <- dftemp2 %>% filter(!id_treated %in% c(16866, 14854))
    else if(W == 4 & X == "crime_property") dftemp2 <- dftemp2 %>% filter(id_treated != 16866)
    else if(W == 4 & X == "call_trespass") dftemp2 <- dftemp2 %>% filter(!id_treated %in% c(13747, 14941, 15365))
    else if(W == 4 & X == "call_qol") dftemp2 <- dftemp2 %>% filter(!id_treated %in% c(15025))
    else if(W == 4 & X == "summons") dftemp2 <- dftemp2 %>% filter(!id_treated %in% c(15363, 14854, 15365))
    else if(W == 11 & X == "crime_violentall") dftemp2 <- dftemp2 %>% filter(!id_treated %in% c(16866, 15336))
    else if(W == 11 & X == "crime_violentall") dftemp2 <- dftemp2 %>% filter(!id_treated %in% c(16866))
    #else if(W == 11 & X == "crime_weapon") dftemp2 <- dftemp2 %>% filter(!id_treated %in% c(15763, 15336, 16866))
    #else if(W == 11 & X == "crime_drugs") dftemp2 <- dftemp2 %>% filter(!id_treated %in% c(15195, 15195, 13579))
    else if(W == 11 & X == "call_trespass") dftemp2 <- dftemp2 %>% filter(!id_treated %in% c(13747, 14941, 15365))
    else if(W == 11 & X == "call_qol") dftemp2 <- dftemp2 %>% filter(!id_treated %in% c(15025, 14941))
    else if(W == 11 & X == "summons") dftemp2 <- dftemp2 %>% filter(!id_treated %in% c(15363, 14854))
    p <- ggplot(dftemp2, aes_string(x = "datenum", y = X, group = "id_treated")) +
      geom_line(aes(color = treated)) +
      geom_line(data = dftemp2 %>% filter(id_treated %in% W), size = 1.1, color = "black") +
      geom_vline(xintercept = 2021.833, linetype = "dashed")  +
      geom_hline(yintercept = 0, linetype = "dashed")  +
      scale_color_manual(values = c("grey60", "black"), labels = c(vplacebo, vcity)) +
      labs(x =  NULL, y = "Incidents", color = NULL) +
      plottheme 
    ggsave(paste0("../Tables-Figures/Synthplacebo", W, X, ".jpg"), plot = p, width = 6, height = 5, dpi = 144) 
  }
}
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions
  





################################################################################-
## Figure: synthetic control figures (three hexagons-high crime city) ##########
################################################################################-
#Import data
dfgap <- fread("../Data/Synth gap public_safety hexagon2.csv", integer64 = "character")
dfweight <- fread("../Data/Synth weights public_safety hexagon2.csv", integer64 = "character")
dfplacebo <- fread("../Data/Synth placebo public_safety hexagon2.csv", integer64 = "character") %>%
  mutate(post = ifelse(datenum >= 2021.83, 1, 0))
plottheme <- theme(
  axis.text.x = element_text(size = 18),
  axis.text.y = element_text(size = 18),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  strip.background = element_rect(fill = "grey85", color = "black"),
  panel.background = element_blank(), axis.line = element_line(colour = "black"),
  legend.key = element_rect(colour = "transparent", fill = "transparent"),
  legend.key.width = unit(1, "cm"),
  legend.text = element_text(size = 18),
  legend.title = element_text(size = 18),
  legend.position = c(0.35, 0.9),
  rect = element_rect(fill = "transparent"))
vsite <- c(1,2)
vnames <- dfgap %>% select(crime_violentall:call311_disorder) %>% names()

#Plot: synthetic, observed incidents
for(W in vsite){
  #Subset data
  dftemp <- dfgap %>%
    filter(id == W) %>%
    mutate(synthetic = factor(synthetic))
  for(X in vnames){
    #Plot
    p <- ggplot(dftemp, aes_string(x = "datenum", y = X)) +
      geom_line(aes(linetype = synthetic), color = "black") +
      geom_vline(xintercept = 2021.833, linetype = "dashed") +
      scale_linetype_manual(values = c("solid", "dashed"), labels = c("Safe injection site", "Synthetic")) +
      labs(x =  NULL, y = "Incidents", linetype = NULL) +
      plottheme    
    ggsave(paste0("../Tables-Figures/Synthgap", W, X, ".jpg"), plot = p, width = 6, height = 5, dpi = 144) 
  }
}
rm(dftemp, p, W, X); gc()

#Plot: placebo
for(W in vsite){
  #Subset data
  dftemp1 <- dfplacebo %>%
    filter(id == W) %>%
    mutate(treated = factor(ifelse(id_treated %in% W, "1", "0")))
  for(X in vnames){
    #Estimate the RMSPE (exclude hexagons with a RMSPE Z times larger than the treatment)
    vratio <- 4
    if(W %in% 1 & X %in% c("crime_property", "crime_theft", "crime_mvtheft")){
      vratio <- 10
    } else if(W %in% 1 & X %in% c("crime_burglary")){
      vratio <- 90
    } else if(W %in% 2 & X %in% c("crime_lowassault", "crime_theft", "crime_weapon")){
      vratio <- 90
    }
    dfrmspe <- dftemp1 %>%
      filter(post == 0) %>%
      group_by(id_treated) %>%
      summarise(rmspe = mean( (!!sym(X) - 0)^2 )^.5) %>%
      mutate(ratio = rmspe/rmspe[id_treated==W]) %>%
      ungroup()
    dftemp2 <- dftemp1 %>%
      left_join(dfrmspe, by = "id_treated") %>%
      filter(ratio < vratio)
    vrmspe = dftemp2 %>%
      filter(id_treated == W) %>%
      distinct(rmspe) %>%
      pull(rmspe) %>%
      scales::comma(accuracy = 0.1)
    vplacebo <- paste0("Placebo (n=", length(unique(dftemp2$id_treated))-1, ")")
    vcity <- paste0("Treatment (RMSPE=", vrmspe, ")")
    p <- ggplot(dftemp2, aes_string(x = "datenum", y = X, group = "id_treated")) +
      geom_line(aes(color = treated)) +
      geom_line(data = dftemp2 %>% filter(id_treated %in% W), size = 1.1, color = "black") +
      geom_vline(xintercept = 2021.833, linetype = "dashed")  +
      geom_hline(yintercept = 0, linetype = "dashed")  +
      scale_color_manual(values = c("grey60", "black"), labels = c(vplacebo, vcity)) +
      labs(x =  NULL, y = "Incidents", color = NULL) +
      plottheme 
    ggsave(paste0("../Tables-Figures/Synthplacebo", W, X, ".jpg"), plot = p, width = 6, height = 5, dpi = 144) 
  }
}
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions




###############################################################################-
## Figure: randomization inference diff-in-diff Poisson (single hexagon) ######
###############################################################################-
#Import data
dfest <- fread("../Data/Diff in diff Poisson public_safety1 randominference.csv", integer64 = "character") 
plottheme <- theme(
  axis.text.x = element_text(size = 20),
  axis.text.y = element_text(size = 20),
  axis.title.x = element_text(size = 15),
  axis.title.y = element_text(size = 15),
  strip.text.x = element_text(size = 20),
  strip.background = element_rect(fill = "grey85", color = "black"),
  panel.background = element_blank(), axis.line = element_line(colour = "black"),
  legend.key = element_rect(colour = "transparent", fill = "transparent"),
  legend.key.width = unit(2, "cm"),
  legend.text = element_text(size = 18),
  legend.title = element_text(size = 18),
  legend.position = "bottom",
  rect = element_rect(fill = "transparent"))  

#Plot
vnames <- unique(dfest$outcome)
for(X in vnames){
  vest <- dfest %>% 
    filter(treated_idsites == "1-2" & outcome == X) %>%
    pull(est)
  p <- ggplot(dfest %>% filter(outcome == X), aes(est)) +
    geom_histogram(bins = 100, fill = "grey80", color = "black") +
    geom_vline(xintercept = vest, color = "darkred", linetype = "dashed") +
    #coord_cartesian(xlim = c(-2.5, 2.5)) +
    labs(x = "Estimate", y = "Frequency") +
    plottheme
  ggsave(paste0("../Tables-Figures/RandomInf_", X, "1.jpg"), plot = p, width = 6, height = 5, dpi = 144) 
}
dfest %>%
  group_by(outcome) %>%
  mutate(rank = cume_dist(est)) %>%
  filter(treated_idsites == "1-2")
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions




###############################################################################-
## Figure: randomization inference diff-in-diff Poisson (three hexagons) ######
###############################################################################-
#Import data
dfest <- fread("../Data/Diff in diff Poisson public_safety2 randominference.csv", integer64 = "character") %>%
  filter(outcome != "call_trespass")
plottheme <- theme(
  axis.text.x = element_text(size = 20),
  axis.text.y = element_text(size = 20),
  axis.title.x = element_text(size = 15),
  axis.title.y = element_text(size = 15),
  strip.text.x = element_text(size = 20),
  strip.background = element_rect(fill = "grey85", color = "black"),
  panel.background = element_blank(), axis.line = element_line(colour = "black"),
  legend.key = element_rect(colour = "transparent", fill = "transparent"),
  legend.key.width = unit(2, "cm"),
  legend.text = element_text(size = 18),
  legend.title = element_text(size = 18),
  legend.position = "bottom",
  rect = element_rect(fill = "transparent"))  
dftemp <- dfest %>%
  group_by(outcome) %>%
  mutate(rank = round(cume_dist(est), 2)) %>%
  filter(treated_idsites == "1-2")

#Plot
vnames <- unique(dfest$outcome)
for(X in vnames){
  vest <- dfest %>% 
    filter(treated_idsites == "1-2" & outcome == X) %>%
    pull(est)
  vtext1 <- paste0("beta == ", round(vest, digits = 2))
  vtext2 <- paste0("p = ", dftemp[dftemp$outcome==X, "rank"])
  if(X == "arrest_weapon"){
    xlabel <- -1.05
    ylabel <- 5.5
  } else if(X == "arrest_drugs"){
    xlabel <- -1.5
    ylabel <- 7.0
  } else if(X == "call_crime"){
    xlabel <- -0.15
    ylabel <- 8.0
  } else if(X == "call_medic"){
    xlabel <- -0.85
    ylabel <- 8.0
  } else if(X == "summons"){
    xlabel <- -0.80
    ylabel <- 6.0
  } 
  p <- ggplot(dfest %>% filter(outcome == X), aes(est)) +
    geom_histogram(bins = 100, fill = "grey80", color = "black") +
    geom_vline(xintercept = vest, color = "darkred", linetype = "dashed") +
    annotate('text', label = vtext1, x = xlabel, y = ylabel, hjust = 0, vjust = 1, size = 7, parse = TRUE) +
    annotate('text', label = vtext2, x = xlabel, y = ylabel-0.50, hjust = 0, vjust = 1, size = 7) +
    labs(x = "Estimate", y = "Frequency") +
    plottheme
  ggsave(paste0("../Tables-Figures/RandomInf_", X, "2.jpg"), plot = p, width = 6, height = 5, dpi = 144) 
}
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions





###########################################################################################-
## Figure: IRR quality of life calls detailed categories (single hexagon) #################
###########################################################################################-
#Import data
voutcome <- c("call_qol", "call311_substance", "call311_dirty", "call311_abandoned", "call311_noise", 
              "call311_homeless", "call_trespass")
dfest <- fread("../Data/Diff in diff Poisson public_safety1.csv") %>%
  filter(model == "month_id_fe") %>%
  filter(outcome %in% voutcome) %>%
  arrange(factor(outcome, levels = voutcome)) %>%
  mutate(id = row_number(), 
         idtext = c("Nuisance calls", "Drug\n311 calls", "Unsanitary\nconditions\n311 calls", 
                    "Abandoned\nvehicle\n311 calls", "Noise\ncomplaint \n311 calls", 
                    "Homeless\n311 calls", "Trespass\n911 calls")) %>%
  mutate(irr = paste0(format(round((irr - 1)*100, digits = 1), nsmall = 1), "%"), 
         irr.low = paste0(format(round((irr.low - 1)*100, digits = 1), nsmall = 1), "\\%"),
         irr.up = paste0(format(round((irr.up - 1)*100, digits = 1), nsmall = 1), "\\%"), 
         irr.ci = paste0(irr.low, ",", irr.up)) %>%
  mutate(est.low = est - 1.96*se,
         est.up = est + 1.96*se) %>%
  mutate(idtext = paste0(idtext, "\n", round(est, 3), "\n(", round(se, 3), ")\n", irr, "\n", irr.ci, "\n", "[", round(mean, 1), "]"))
plottheme <- theme(
  axis.text.x = element_text(size = 13),
  axis.text.y = element_text(size = 15),
  axis.title.x = element_text(size = 15),
  axis.title.y = element_text(size = 15),
  strip.text.x = element_text(size = 20),
  strip.background = element_rect(fill = "grey85", color = "black"),
  panel.background = element_blank(), axis.line = element_line(colour = "black"),
  legend.key = element_rect(colour = "transparent", fill = "transparent"),
  legend.key.width = unit(2, "cm"),
  legend.text = element_text(size = 18),
  legend.title = element_text(size = 18),
  legend.position = "top",
  rect = element_rect(fill = "transparent"))

#Immediate vicinity
p <- ggplot(dfest, aes(x = id, y = est)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_pointrange(aes(ymin = est.low, ymax = est.up), color = "darkblue") +
  geom_point(color = "darkblue") +
  labs(y = "Poisson regression coefficient", x = NULL) +
  scale_x_continuous(breaks = 1:7, labels = dfest$idtex, limits = c(1,7.1)) +
  scale_y_continuous(breaks = -2:2, limits = c(-2.2, 2.5)) +
  plottheme
ggsave("../Tables-Figures/Poisson_calls1.jpg", plot = p, width = 11, height = 7, dpi = 144) 
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions






###########################################################################################-
## Figure: IRR quality of life calls detailed categories (three hexagons) #################
###########################################################################################-
#Import data
voutcome <- c("call_qol", "call311_substance", "call311_dirty", "call311_abandoned", "call311_noise", 
              "call311_homeless", "call_trespass")
dfest <- fread("../Data/Diff in diff Poisson public_safety2.csv") %>%
  filter(model == "month_id_fe") %>%
  filter(outcome %in% voutcome) %>%
  arrange(factor(outcome, levels = voutcome)) %>%
  mutate(id = row_number(), 
         idtext = c("Nuisance calls", "Drug\n311 calls", "Unsanitary\nconditions\n311 calls", 
                    "Abandoned\nvehicle\n311 calls", "Noise\ncomplaint \n311 calls", 
                    "Homeless\n311 calls", "Trespass\n911 calls")) %>%
  mutate(irr = paste0(format(round((irr - 1)*100, digits = 1), nsmall = 1), "%"), 
         irr.low = paste0(format(round((irr.low - 1)*100, digits = 1), nsmall = 1), "\\%"),
         irr.up = paste0(format(round((irr.up - 1)*100, digits = 1), nsmall = 1), "\\%"), 
         irr.ci = paste0(irr.low, ",", irr.up)) %>%
  mutate(est.low = est - 1.96*se,
         est.up = est + 1.96*se) %>%
  mutate(idtext = paste0(idtext, "\n", round(est, 3), "\n(", round(se, 3), ")\n", irr, "\n", irr.ci, "\n", "[", round(mean, 1), "]"))
plottheme <- theme(
  axis.text.x = element_text(size = 13),
  axis.text.y = element_text(size = 15),
  axis.title.x = element_text(size = 15),
  axis.title.y = element_text(size = 15),
  strip.text.x = element_text(size = 20),
  strip.background = element_rect(fill = "grey85", color = "black"),
  panel.background = element_blank(), axis.line = element_line(colour = "black"),
  legend.key = element_rect(colour = "transparent", fill = "transparent"),
  legend.key.width = unit(2, "cm"),
  legend.text = element_text(size = 18),
  legend.title = element_text(size = 18),
  legend.position = "top",
  rect = element_rect(fill = "transparent"))

#Neighborhood
p <- ggplot(dfest, aes(x = id, y = est)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_pointrange(aes(ymin = est.low, ymax = est.up), color = "darkblue") +
  geom_point(color = "darkblue") +
  labs(y = "Poisson regression coefficient", x = NULL) +
  scale_x_continuous(breaks = 1:7, labels = dfest$idtex, limits = c(1,7.1)) +
  scale_y_continuous(breaks = -2:2, limits = c(-2.2, 2.5)) +
  plottheme
ggsave("../Tables-Figures/Poisson_calls2.jpg", plot = p, width = 11, height = 7, dpi = 144) 
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions




############################################################################################################-
## Figure: IRR quality of life calls detailed categories (single hexagon-alternative specifications) #######
############################################################################################################-
#Import data
voutcome <- c("call_qol", "call311_substance", "call311_dirty", "call311_abandoned", 
              "call311_noise", "call311_homeless", "call_trespass")
dftemp1 <- fread("../Data/Diff in diff Poisson highcrimecity public_safety1.csv") %>%
  filter(model == "month_id_fe") %>%
  mutate(model = "High crime")
dftemp2 <- fread("../Data/Diff in diff Poisson highdrugarrests public_safety1.csv") %>%
  filter(model == "month_id_fe") %>%
  mutate(model = "High drug arrests")
dftemp3 <- fread("../Data/Diff in diff Poisson public_safety1.csv") %>%
  filter(model == "month_id_fe") %>%
  mutate(model = "Main")
dftemp4 <- fread("../Data/Diff in diff NegBin public_safety1.csv") %>%
  filter(model == "month_id_fe") %>%
  mutate(model = "NegBin")
dftemp5 <- fread("../Data/Diff in diff Poisson public_safety1.csv") %>%
  filter(model == "pretrends") %>%
  mutate(model = "Pretrends")
dfest <- dftemp1 %>%
  bind_rows(dftemp2) %>%
  bind_rows(dftemp3) %>%
  bind_rows(dftemp4) %>%
  bind_rows(dftemp5) %>%
  filter(outcome %in% voutcome) %>%
  arrange(factor(outcome, levels = voutcome)) %>%
  group_by(model) %>%
  mutate(id = row_number(), 
         idtext = c("Nuisance calls", "Drug\n311 calls", "Unsanitary\nconditions\n311 calls", 
                    "Abandoned\nvehicle\n311 calls", "Noise\ncomplaint \n311 calls", 
                    "Homeless\n311 calls", "Trespass\n911 calls")) %>%
  mutate(id = ifelse(model == "High crime", id - 0.2, id), 
         id = ifelse(model == "High drug arrests", id - 0.1, id),
         id = ifelse(model == "NegBin", id + 0.1, id), 
         id = ifelse(model == "Pretrends", id + 0.2, id)) %>%
  mutate(irr = paste0(format(round((irr - 1)*100, digits = 1), nsmall = 1), "%")) %>%
  mutate(est.low = est - 1.96*se,
         est.up = est + 1.96*se) %>%
  mutate(idtext = paste0(idtext, "\n[", round(mean, 1), "]")) 
plottheme <- theme(
  axis.text.x = element_text(size = 13),
  axis.text.y = element_text(size = 15),
  axis.title.x = element_text(size = 15),
  axis.title.y = element_text(size = 15),
  strip.text.x = element_text(size = 20),
  strip.background = element_rect(fill = "grey85", color = "black"),
  panel.background = element_blank(), axis.line = element_line(colour = "black"),
  legend.key = element_rect(colour = "transparent", fill = "transparent"),
  legend.key.width = unit(0.2, "cm"),
  legend.text = element_text(size = 16),
  legend.title = element_text(size = 16),
  legend.position = "top",
  rect = element_rect(fill = "transparent"))

#Immediate vicinity
p <- ggplot(dfest, aes(x = id, y = est, group = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_pointrange(aes(ymin = est.low, ymax = est.up, shape = model), color = "darkblue") +
  geom_point(aes(shape = model), color = "darkblue", size = 3) +
  labs(y = "Regression coefficient", x = NULL, shape = NULL) +
  scale_shape_manual(values = c(0:2,4:7)) +
  scale_x_continuous(breaks = 1:7, labels = unique(dfest$idtext), limits = c(0.7,7.3)) +
  scale_y_continuous(breaks = -2:2, limits = c(-2.3, 3.0)) +
  plottheme
ggsave("../Tables-Figures/Poisson_callsAlt1.jpg", plot = p, width = 8, height = 5, dpi = 144) 
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions





############################################################################################################-
## Figure: IRR quality of life calls detailed categories (three hexagons-alternative specifications) #######
############################################################################################################-
#Import data
voutcome <- c("call_qol", "call311_substance", "call311_dirty", "call311_abandoned", "call311_noise", 
              "call311_homeless", "call_trespass")
dftemp1 <- fread("../Data/Diff in diff Poisson highcrimecity public_safety2.csv") %>%
  filter(model == "month_id_fe") %>%
  mutate(model = "High crime")
dftemp2 <- fread("../Data/Diff in diff Poisson highdrugarrests public_safety2.csv") %>%
  filter(model == "month_id_fe") %>%
  mutate(model = "High drug arrests")
dftemp3 <- fread("../Data/Diff in diff Poisson public_safety2.csv") %>%
  filter(model == "month_id_fe") %>%
  mutate(model = "Main")
dftemp4 <- fread("../Data/Diff in diff NegBin public_safety2.csv") %>%
  filter(model == "month_id_fe") %>%
  mutate(model = "NegBin")
dftemp5 <- fread("../Data/Diff in diff Poisson public_safety2.csv") %>%
  filter(model == "pretrends") %>%
  mutate(model = "Pretrends")
dfest <- dftemp1 %>%
  bind_rows(dftemp2) %>%
  bind_rows(dftemp3) %>%
  bind_rows(dftemp4) %>%
  bind_rows(dftemp5) %>%
  filter(outcome %in% voutcome) %>%
  arrange(factor(outcome, levels = voutcome)) %>%
  group_by(model) %>%
  mutate(id = row_number(), 
         idtext = c("Nuisance calls", "Drug\n311 calls", "Unsanitary\nconditions\n311 calls", 
                    "Abandoned\nvehicle\n311 calls", "Noise\ncomplaint \n311 calls", 
                    "Homeless\n311 calls", "Trespass\n911 calls")) %>%
  mutate(id = ifelse(model == "High crime", id - 0.2, id), 
         id = ifelse(model == "High drug arrests", id - 0.1, id),
         id = ifelse(model == "NegBin", id + 0.1, id), 
         id = ifelse(model == "Pretrends", id + 0.2, id)) %>%
  mutate(irr = paste0(format(round((irr - 1)*100, digits = 1), nsmall = 1), "%")) %>%
  mutate(est.low = est - 1.96*se,
         est.up = est + 1.96*se) %>%
  mutate(idtext = paste0(idtext, "\n[", round(mean, 1), "]")) 
plottheme <- theme(
  axis.text.x = element_text(size = 13),
  axis.text.y = element_text(size = 15),
  axis.title.x = element_text(size = 15),
  axis.title.y = element_text(size = 15),
  strip.text.x = element_text(size = 20),
  strip.background = element_rect(fill = "grey85", color = "black"),
  panel.background = element_blank(), axis.line = element_line(colour = "black"),
  legend.key = element_rect(colour = "transparent", fill = "transparent"),
  legend.key.width = unit(0.2, "cm"),
  legend.text = element_text(size = 16),
  legend.title = element_text(size = 16),
  legend.position = "top",
  rect = element_rect(fill = "transparent"))

#Immediate vicinity
p <- ggplot(dfest, aes(x = id, y = est, group = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_pointrange(aes(ymin = est.low, ymax = est.up, shape = model), color = "darkblue") +
  geom_point(aes(shape = model), color = "darkblue", size = 3) +
  labs(y = "Regression coefficient", x = NULL, shape = NULL) +
  scale_shape_manual(values = c(0:2,4:7)) +
  scale_x_continuous(breaks = 1:7, labels = unique(dfest$idtext), limits = c(0.7,7.3)) +
  scale_y_continuous(breaks = -2:2, limits = c(-2.3, 3.0)) +
  plottheme
ggsave("../Tables-Figures/Poisson_callsAlt2.jpg", plot = p, width = 8, height = 5, dpi = 144) 
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions





#################################################################################-
## Figure: trends by NYC, high-crime city, and single hexagon samples ###########
#################################################################################-
#Import data and create time dummy variables for the event study and individual time trends
#Removed 2018 because it is a noisy year
df <- fread("../Data/Crime month-hexagon city data 2018-2022.csv", integer64 = "character") %>%
  filter(year %in% 2019:2022) %>%
  filter(hexsample == 3 | is.na(hexsample)) %>%
  mutate(treated = ifelse(site %in% "OnPoint NYC", 1, 0), 
         bimonth = ifelse(month %in% 1:2, 1, 6),
         bimonth = ifelse(month %in% 3:4, 2, bimonth),
         bimonth = ifelse(month %in% 5:6, 3, bimonth),
         bimonth = ifelse(month %in% 7:8, 4, bimonth),
         bimonth = ifelse(month %in% 9:10, 5, bimonth),
         datemonth = year + (month - 1) /12, 
         datebimonth = year + (bimonth - 1) /6, 
         post = ifelse(datemonth >= 2021.91, 1, 0))
vnames <- df %>% 
  select(crime_index, crime_violentall, crime_property, arrest_weapon, arrest_drugs, 
         summons, call_crime, call_medic, call_qol) %>%
  names()
#Identifying the 250 unsafest hexagons in the top 10 unsafe precincts (excluding the treated ones and Times Squares)
vids <- df %>%
  filter(post == 0) %>%
  group_by(precinct, treated, rank2, id) %>%
  summarise(crime_index2 = sum(crime_index - crime_theft)) %>%
  ungroup() %>%
  filter(!precinct %in% c(14,25,34)) %>%
  filter(rank2 %in% 1:9) %>%
  mutate(rankhex2 = min_rank(-crime_index2)) %>%
  filter(rankhex2 <= 250) %>%
  pull(id)
df <- df %>% 
  filter(id %in% vids | treated == 1) %>%
  mutate(sample = ifelse(treated == 1, "Treated", "High crime")) %>%
  group_by(year, bimonth, datebimonth, sample) %>%
  summarise_at(vars(all_of(vnames)), mean) %>% 
  group_by(sample) %>%
  mutate_at(vars(all_of(vnames)), funs(./.[datebimonth==2019]*100))
plottheme <- theme(
  axis.text.x = element_text(size = 18),
  axis.text.y = element_text(size = 18),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  strip.background = element_rect(fill = "grey85", color = "black"),
  panel.background = element_blank(), axis.line = element_line(colour = "black"),
  legend.key = element_rect(colour = "transparent", fill = "transparent"),
  legend.key.width = unit(1, "cm"),
  legend.text = element_text(size = 16),
  legend.title = element_text(size = 16),
  legend.position = "top",
  rect = element_rect(fill = "transparent"))

#Plot
for(X in vnames){
  p <- ggplot(df, aes_string(x = "datebimonth", y = X, group = "sample")) +
    geom_point(aes(shape = sample), size = 3) +
    geom_line(aes(linetype = sample)) +
    geom_vline(xintercept = 2021.91, linetype = "longdash") +
    labs(x = "Bimonthly", y = "Incidents [2019=100]", shape = NULL, linetype = NULL) +
    scale_x_continuous(limits = c(2019, 2023)) +
    plottheme
  ggsave(paste0("../Tables-Figures/Trends", X, ".jpg"), plot = p, width = 6, height = 5, dpi = 144)
}
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions





###############################################################################-
## Figure: distribution of pre-intervention outcomes (single hexagon) #########
###############################################################################-
#Import data and create time dummy variables for the event study and individual time trends
#Removed 2018 because it is a noisy year
df <- fread("../Data/Crime month-hexagon city data 2018-2022.csv", integer64 = "character") %>%
  filter(year %in% 2019:2022) %>%
  filter(hexsample == 1 | is.na(hexsample)) %>%
  mutate(treated = ifelse(site %in% "OnPoint NYC", 1, 0),
         datenum = year + (month - 1) /12) %>%
  filter(datenum < 2021.91) %>%
  group_by(id) %>%
  summarise_at(vars(crime_murder:call_qol), mean)
plottheme <- theme(
  axis.text.x = element_text(size = 18),
  axis.text.y = element_text(size = 18),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  strip.background = element_rect(fill = "grey85", color = "black"),
  panel.background = element_blank(), axis.line = element_line(colour = "black"),
  legend.key = element_rect(colour = "transparent", fill = "transparent"),
  legend.key.width = unit(1, "cm"),
  legend.text = element_text(size = 18),
  legend.title = element_text(size = 18),
  legend.position = "top",
  rect = element_rect(fill = "transparent"))

vtreat1 <- df %>% filter(id == 4) %>% pull(arrest_drugs) %>% round(., 1)
vtreat2 <- df %>% filter(id == 11) %>% pull(arrest_drugs) %>% round(., 1)
p <- ggplot(df, aes(arrest_drugs)) +
  geom_histogram(bins = 100, color = "black", fill = "grey90") +
  geom_vline(xintercept = vtreat1, linetype = "dashed", color = "darkblue") +
  geom_vline(xintercept = vtreat2, linetype = "dashed", color = "darkblue") +
  scale_x_continuous(breaks = seq(0, 40, 5)) +
  scale_y_continuous(label = scales::comma) +
  labs(x = "Mean pre-intervention drug arrests", y = "Count") +
  annotate('text', label = paste0("Washington Heights (", vtreat2, ")"), x = 2, y = 4000, size = 6, angle = 90) +
  annotate('text', label = paste0("East Harlem (", vtreat1, ")"), x = 34, y = 4000, size = 6, angle = 90) +
  plottheme
ggsave("../Tables-Figures/Histarrest_drugs.jpg", plot = p, width = 8, height = 5, dpi = 144)
quantile(df$arrest_drugs, probs = seq(0, 1, 0.001))
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions






###############################################################################-
## Figure: raw monthly count of crimes per site (single hexagon) ##############
###############################################################################-
#Import data and create time dummy variables for the event study and individual time trends
#Removed 2018 because it is a noisy year
df <- fread("../Data/Crime month-hexagon1 data 2018-2022.csv", integer64 = "character") %>%
  filter(direct == 1 & year %in% 2019:2022) %>%
  mutate(quarter = NA, 
         quarter = ifelse(month %in% 1:3, 1, quarter),
         quarter = ifelse(month %in% 4:6, 2, quarter),
         quarter = ifelse(month %in% 7:9, 3, quarter),
         quarter = ifelse(month %in% 10:12, 4, quarter),
         treated = ifelse(site %in% "OnPoint NYC", "OPCs", "SSPs"), 
         datenum = year + (quarter - 1) /4) %>%
  group_by(idsite, id, datenum, treated) %>%
  summarise_at(vars(crime_murder:call_qol), mean) %>%
  ungroup()
plottheme <- theme(
  axis.text.x = element_text(size = 18),
  axis.text.y = element_text(size = 18),
  axis.title.x = element_text(size = 18),
  axis.title.y = element_text(size = 18),
  strip.background = element_rect(fill = "grey85", color = "black"),
  panel.background = element_blank(), axis.line = element_line(colour = "black"),
  legend.key = element_rect(colour = "transparent", fill = "transparent"),
  legend.key.width = unit(2, "cm"),
  legend.text = element_text(size = 18),
  legend.title = element_text(size = 18),
  legend.position = "top",
  rect = element_rect(fill = "transparent"))

for(X in c("crime_violentall", "crime_property", "arrest_weapon", 
           "arrest_drugs", "summons", "call_crime", "call_medic", "call_qol")){
  p <- ggplot(df, aes_string(x = "datenum", y = X, group = "idsite", color = "treated")) +
    geom_point(aes(shape = treated), size = 3) +
    geom_line(size = 0.90) +
    geom_point(aes_string(x = "datenum", y = X), color = "black", size = 3,
               shape = 16, data = df %>% filter(treated == "OPCs")) +
    geom_line(aes_string(x = "datenum", y = X), size = 0.90, 
              data = df %>% filter(treated == "OPCs")) +
    geom_vline(xintercept = 2021.9, color = "black", linetype = "dashed") +
    scale_color_manual(values = c("black", "grey80")) +
    scale_shape_manual(values = c(16,17)) +
    labs(x = NULL, y = "Number of incidents", color = NULL, shape = NULL) +
    plottheme  
  ggsave(paste0("../Tables-Figures/Count_quarter", X, ".jpg"), plot = p, width = 7, height = 5, dpi = 144)
}
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions




##############################################################################-
## Table: Summary statistics #################################################
##############################################################################-
#Import data
df <- fread("../Data/Crime month-hexagon1 data 2018-2022.csv", integer64 = "character") %>%
  filter(year %in% 2019:2022) %>%
  mutate(datenum = year + (month - 1) /12, 
         treated = ifelse(site %in% "OnPoint NYC", 1, 0), 
         post = ifelse(datenum >= 2021.91, 1, 0)) %>%
  filter(direct == 1)
vnames <- df %>% 
  select(crime_index, crime_violentall, crime_murder, crime_robbery,  crime_assault,
         crime_lowassault, crime_property, crime_burglary, crime_theft, crime_mvtheft, 
         arrest_weapon, arrest_drugs, summons, call_crime, call_assault, call_trespass, 
         call_medic, call311_substance, call311_dirty, call311_abandoned, call311_noise,
         call311_homeless) %>%
  names()

#Mean and standard deviation of both groups by treatment period
dfstat <- data.frame("var" = vnames, stringsAsFactors = FALSE)
vstat <- df %>% 
  group_by(treated, post) %>%
  select(treated, post, crime_index, crime_violentall, crime_murder, crime_robbery,  
         crime_assault, crime_lowassault, crime_property, crime_burglary, crime_theft, 
         crime_mvtheft, arrest_weapon, arrest_drugs, summons, call_crime, call_assault, 
         call_trespass, call_medic, call311_substance, call311_dirty, call311_abandoned, 
         call311_noise, call311_homeless) %>%
  summarise_all(list(~mean(., na.rm = TRUE), ~sd(., na.rm = TRUE))) %>%
  ungroup()
#Extract mean and std dev from control group, pre-intervention
vmean <- vstat %>%
  select(ends_with("_mean")) %>%
  rename_all(function (x) str_replace(x, "_mean", "")) %>%
  slice(1) %>%
  unlist(., use.names = TRUE)
vsd <- vstat %>%
  select(ends_with("_sd")) %>%
  rename_all(function (x) str_replace(x, "_sd", "")) %>%
  slice(1) %>%
  unlist(., use.names = TRUE)
dfstat$meancontrolpre <- vmean
dfstat$sdcontrolpre <- vsd
#Extract mean and std dev from control group, post-intervention
vmean <- vstat %>%
  select(ends_with("_mean")) %>%
  rename_all(function (x) str_replace(x, "_mean", "")) %>%
  slice(2) %>%
  unlist(., use.names = TRUE)
vsd <- vstat %>%
  select(ends_with("_sd")) %>%
  rename_all(function (x) str_replace(x, "_sd", "")) %>%
  slice(2) %>%
  unlist(., use.names = TRUE)
dfstat$meancontrolpost <- vmean
dfstat$sdcontrolpost <- vsd
#Extract mean and std dev from treatment group, pre-intervention
vmean <- vstat %>%
  select(ends_with("_mean")) %>%
  rename_all(function (x) str_replace(x, "_mean", "")) %>%
  slice(3) %>%
  unlist(., use.names = TRUE)
vsd <- vstat %>%
  select(ends_with("_sd")) %>%
  rename_all(function (x) str_replace(x, "_sd", "")) %>%
  slice(3) %>%
  unlist(., use.names = TRUE)
dfstat$meantreatpre <- vmean
dfstat$sdtreatpre <- vsd
#Extract mean and std dev from treatment group, post-intervention
vmean <- vstat %>%
  select(ends_with("_mean")) %>%
  rename_all(function (x) str_replace(x, "_mean", "")) %>%
  slice(4) %>%
  unlist(., use.names = TRUE)
vsd <- vstat %>%
  select(ends_with("_sd")) %>%
  rename_all(function (x) str_replace(x, "_sd", "")) %>%
  slice(4) %>%
  unlist(., use.names = TRUE)
dfstat$meantreatpost <- vmean
dfstat$sdtreatpost <- vsd
dfstat <- dfstat %>%
  bind_rows(data.frame("var" = rep(NA, 4))) %>%
  slice(23, 1:10, 24, 11:13, 25, 14:17, 26, 18:22)

#Export table
dftemp <- data.frame(replicate(50, sample(20)))
lm1 <- lm(as.formula(paste0("X1 ~ ", paste0("X", 2:27, collapse = "+"), "-1")), data = dftemp)
covlabels <- c("Crimes", "offset1 Index", "offset1 Violent", "offset2 Murder", "offset2 Robbery", 
               "offset2 Aggravated assault", "offset2 Simple Assault", "offset1 Property", 
               "offset2 Burglary", "offset2 Theft", "offset2 Motor vehicle theft", 
               "Law enforcement", "offset1 Weapons arrests", "offset1 Drug arrests", 
               "offset1 Criminal summons", "911 calls", "offset1 Crime", "offset1 Assault", 
               "offset1 Trespass", "offset1 Medical", "311 calls", "offset1 Drug", 
               "offset1 Unsanitary conditions", "offset1 Abandoned vehicle", 
               "offset1 Noise complaint", "offset1 Homeless")
out <- stargazer(lm1, lm1, lm1, lm1,
                 title = "Descriptive statistics, hexagon monthly level data",
                 label = "TableDescStats",
                 column.sep.width = "4pt",
                 font.size = "small",
                 no.space = TRUE,
                 coef = with(dfstat, list(meantreatpre, meantreatpost, meancontrolpre, meancontrolpost)),
                 se = with(dfstat, list(sdtreatpre, sdtreatpost, sdcontrolpre, sdcontrolpost)),
                 digits = 1,
                 covariate.labels = covlabels,
                 report = "vcs",
                 omit.stat = c("all"),
                 dep.var.caption  = "caption placeholder",
                 dep.var.labels = c("Mean count (std. dev.)"),
                 column.labels = c("Neighborhoods with OPCs", "Neighborhoods with SSP"),
                 column.separate = c(2, 2),
                 single.row = TRUE,
                 model.numbers = FALSE,
                 notes = "placeholder",
                 notes.append = FALSE,
                 header = FALSE,
                 #type = "text")
                 type = "latex")
notes <- "\\\\multicolumn{5}{l} {\\\\parbox[t]{13.2cm}{ \\\\scriptsize Notes: 
Hexagon monthly-year level mean count and standard deviation by intervention period 
(pre: 2019M1-2021M11, post: 2021M12-2022M12)) from the hexagon surrounding the 
overdose prevention center and syringe services programs. 
Index crimes include the six UCR part I crimes (murder, robbery, aggravated assault, 
burglary, theft, and motor vehicle). Violent crimes include murder, robbery, and 
aggravated and simple assault. Property crimes include burglary, theft, and 
motor vehicle theft. Weapons possession arrests refer to criminal possession of a weapon, 
Drugs possession arrests mean sale or possession of dangerous drugs. Crime 911 calls 
include those in which there was a possible crime in-progress or one has been committed. 
Assault and trespass 911 calls explicitly mention these offenses in the call. 
Medical calls include those needing an ambulance. Drug related calls include drug and 
drinking activity and loose syringe calls. Unsanitary conditions comprise calls related 
to seeing a rodent, graffiti, dirty and unsanitary conditions, and urinating in public. 
Abandoned vehicle and noise complaints are calls handled by the New York Police Department.
Homeless calls include those related to assisting a homeless person, encampment, 
and homeless street condition. }} \\\\"
out <- gsub(pattern = "\\\\textit\\{Note.*", replacement = notes, x = out)
out <- gsub(pattern = "offset1", replacement = "\\\\hspace{2mm}", x = out)
out <- gsub(pattern = "offset2", replacement = "\\\\hspace{6mm}", x = out)
out <- gsub(pattern = "offset3", replacement = "\\\\hspace{8mm}", x = out)
out <- gsub(pattern = " & \\\\multicolumn\\{4\\}\\{c\\}\\{caption placeholder.* ", replacement = "", x = out)
out <- gsub(pattern = "\\\\cline.*", replacement = " ", x = out)
out <- gsub(pattern = "\\\\\\\\\\[\\-1\\.8ex\\]", replacement = " ", x = out)
outnew <- character(length = length(out))
outnew[1:8] <- out[1:8]
outnew[9] <- out[11]
outnew[10] <- out[12]
outnew[11] <- "& \\multicolumn{2}{c}{(n = 2)} & \\multicolumn{2}{c}{(n = 17)} \\\\"
outnew[12] <- "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}"
outnew[13] <- "& Pre & Post & Pre & Post \\\\"
outnew[14] <- "\\hline"
outnew[15:46] <- out[14:45]
outnew[43] <- ""
writeLines(outnew, file("../Tables-Figures/TableDescStats.tex"))
close(file("../Tables-Figures/TableDescStats.tex"))
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions

#stats in text
df %>% 
  mutate(calls911 = call_crime + call_medic,
         calls311 = call311_substance + call311_dirty + call311_abandoned + 
           call311_noise + call311_homeless) %>%
  summarise_at(vars(crime_index, starts_with("call"), summons), list(~sum(.), ~mean(.), ~sd(.))) %>%
  select(starts_with("crime_index"), starts_with("calls911"), starts_with("calls311"),
         starts_with("summons"))
df %>% 
  mutate(calls911 = call_crime + call_medic,
         calls311 = call311_substance + call311_dirty + call311_abandoned + 
           call311_noise + call311_homeless) %>%
  summarise_at(vars(crime_index, starts_with("call"), summons), list(~sum(.), ~mean(.), ~sd(.))) %>%
  select(starts_with("crime_index"), starts_with("calls911"), starts_with("calls311"),
         starts_with("summons"))
df %>%
  filter(post == 0 & treated == 1) %>%
  select(call_crime, call_medic, call_qol) %>%
  summarise_all(list(~round(mean(., na.rm = TRUE), 1), ~round(sd(., na.rm = TRUE), 1)))
fread("../Data/Crime month-hexagon2 data 2018-2022.csv", integer64 = "character") %>%
  filter(year %in% 2019:2022) %>%
  mutate(datenum = year + (month - 1) /12, 
         treated = ifelse(site %in% "OnPoint NYC", 1, 0), 
         post = ifelse(datenum >= 2021.91, 1, 0)) %>%
  filter(direct == 1) %>%
  filter(post == 0 & treated == 1) %>%
  select(crime_violentall, crime_property, arrest_weapon, arrest_drugs, 
         summons, call_crime, call_medic, call_qol) %>%
  summarise_all(list(mean = ~round(mean(., na.rm = TRUE), 1), 
                     sd = ~round(sd(., na.rm = TRUE), 1)))





################################################################################-
## Table: Diff-in-diff Poisson crime & calls for service #######################
################################################################################-
#Import data 
voutcome <- c("crime_violentall", "crime_property", "arrest_weapon", "arrest_drugs", 
              "summons", "call_crime", "call_medic", "call_qol")
dftemp <- fread("../Data/Diff in diff Poisson public_safety2.csv") %>%
  filter(model == "month_id_fe") %>%
  filter(outcome %in% voutcome) %>%
  mutate(sample = "three_hex")
dfest <- fread("../Data/Diff in diff Poisson public_safety1.csv") %>%
  filter(model == "month_id_fe") %>%
  filter(outcome %in% voutcome) %>%
  mutate(sample = "single_hex") %>%
  bind_rows(dftemp) %>%
  arrange(factor(outcome, levels = voutcome)) %>%
  mutate(irr = paste0(format(round((irr - 1)*100, digits = 1), nsmall = 1), "\\%"), 
         irr.low = paste0(format(round((irr.low - 1)*100, digits = 1), nsmall = 1), "\\%"),
         irr.up = paste0(format(round((irr.up - 1)*100, digits = 1), nsmall = 1), "\\%"), 
         irr.ci = paste0(irr.low, ", ", irr.up))
for(X in 1:8){
  vname <- voutcome[X]
  dftemp <- dfest %>%
    filter(outcome == vname)
  assign(paste0("dfest", X), dftemp)
}
vsize1 <- dfest %>%
  filter(sample == "single_hex") %>%
  pull(n) %>%
  scales::comma(accuracy = 1)
vsize2 <- dfest %>%
  filter(sample == "three_hex") %>%
  pull(n) %>%
  scales::comma(accuracy = 1)
vmean1 <- dfest %>%
  filter(sample == "single_hex") %>%
  pull(mean) %>%
  scales::comma(accuracy = 0.1)
vmean2 <- dfest %>%
  filter(sample == "three_hex") %>%
  pull(mean) %>%
  scales::comma(accuracy = 0.1)
virr1 <- dfest %>%
  filter(sample == "single_hex") %>%
  pull(irr)
virr2 <- dfest %>%
  filter(sample == "three_hex") %>%
  pull(irr)
virrci1 <- dfest %>%
  filter(sample == "single_hex") %>%
  pull(irr.ci)
virrci2 <- dfest %>%
  filter(sample == "three_hex") %>%
  pull(irr.ci)

#Export table
dftemp <- data.frame(replicate(40, sample(20)))
lm1 <- lm(X1~X2+X3-1, data = dftemp)
covlabels <- rep("Treat*Post", 2)
out <- stargazer(lm1, lm1, lm1, lm1, lm1, lm1, lm1, lm1, 
                 title = "Association between opening an overdose prevention center and public safety",
                 label = "TableDiD_Poissonsafety",
                 column.sep.width = "-2pt",
                 font.size = "small",
                 no.space = TRUE,
                 coef = list(dfest1$est, dfest2$est, dfest3$est, dfest4$est, dfest5$est, 
                             dfest6$est, dfest7$est, dfest8$est),
                 se = list(dfest1$se, dfest2$se, dfest3$se, dfest4$se, dfest5$se, 
                           dfest6$se, dfest7$se, dfest8$se),
                 digits = 2,
                 covariate.labels = covlabels,
                 report = "vc*s",
                 omit.stat = c("all"),
                 dep.var.caption  = "caption placeholder",
                 column.labels = c("violentall", "property", "weapon", "drugs", "summons",
                                   "crime", "medic", "qol"),
                 add.lines = list(c("Percent change", virr1),
                                  c("95\\% CI", virrci1),
                                  c("Mean", vmean1),
                                  c("Observations", vsize1), 
                                  c("Percent change", virr2),
                                  c("95\\% CI", virrci2),
                                  c("Mean", vmean2),
                                  c("Observations", vsize2)),
                 model.numbers = TRUE,
                 dep.var.labels.include = FALSE,
                 #notes = "placeholder",
                 #notes.append = FALSE,
                 header = FALSE,
                 star.cutoffs = c(0.05, 0.01, 0.001),
                 #type = "text")
                 type = "latex")
notes <- "\\\\multicolumn{9}{l} {\\\\parbox[t]{22.5cm}{ \\\\scriptsize Notes: 
Difference-in-differences Poisson regression estimates on the association of public safety and
the opening of the overdose prevention centers. The specifications include hexagon and month-year 
fixed effects. Robust standard errors clustered at the hexagon level in parentheses. 
The specification follows the equation in the Supplementary Material: Statistical Methods, where
POST is an indicator for whether a given observation occurs after December, 2021 when the 2 safe 
injection sites were opened to the public. TREAT is an indicator for whether a hexagon contains 
one of the 2 OPCs as opposed to a comparison unit. Hence, the table shows the coefficient on the 
interaction between POST and TREAT, which is the estimated difference-in-differences intervention effect.
Violent crimes include murder, robbery, and aggravated and simple assault. 
Property crimes include burglary, theft, and motor vehicle theft. Weapons refer to 
criminal possession of a weapon. Drugs mean the sale or possession of dangerous drugs. 
Crime 911 calls refer to those made to law enforcement where there was a possible crime 
in-progress or one has been committed. Medical calls include those needing an ambulance. 
Nuisance calls include 911 calls for trespass and 311 calls about homelessness 
(assisting a homeless person, encampment, and homeless street condition) and disorder 
(seeing a rodent, graffiti, dirty and unsanitary conditions, drug and drinking activity, 
urinating in public, and those 311 calls under the New York Police Department jurisdiction 
such as an abandoned vehicle and noise complaint). Panel A examines the 
immediate vicinity (a single hexagon around the site). Panel B inspects the neighborhood 
(three hexagons surrounding the site). The bottom rows exhibit the percentage change 
(incidence rate ratio - 1 = exp($\\\\beta$)-1), followed by the 95 percent
confidence interval, and the pre-intervention mean count crime on the neighborhoods with OPCs and 
the number of observations. $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001. }} \\\\"
out <- gsub(pattern = "\\\\textit\\{Note.*", replacement = notes, x = out)
out <- gsub(pattern = " & \\\\multicolumn\\{8\\}\\{c\\}\\{caption placeholder.* ", replacement = "", x = out)
out <- gsub(pattern = "\\\\cline.*", replacement = " ", x = out)
out <- gsub(pattern = "\\\\\\\\\\[\\-1\\.8ex\\]", replacement = " ", x = out)
outnew <- character(length = length(out))
outnew <- out[1:9]
outnew[10] <- "& \\multicolumn{2}{c}{Crime} & \\multicolumn{3}{c}{Law enforcement} & \\multicolumn{3}{c}{Calls for service} 
               \\\\ \\cmidrule(lr){2-3}  \\cmidrule(lr){4-6} \\cmidrule(lr){7-9} "
outnew[11] <- "& Violent & Property & \\thead{Weapons \\\\ arrests} & \\thead{Drug \\\\ arrests} 
                & \\thead{Criminal \\\\ summons} & \\thead{Crime \\\\ 911 calls} 
                & \\thead{Medical \\\\ 911 calls} & \\thead{Nuisance \\\\ calls}  \\\\"
outnew[12:13] <- out[12:13]
outnew[14] <- "\\multicolumn{8}{l}{\\footnotesize \\textit{A. Immediate vicinity}} \\\\"
outnew[15:16] <- out[c(14:15)]
outnew[17] <- "\\hline"
outnew[18:21] <- out[c(19:22)]
outnew[22] <- "\\hline"
outnew[23] <- "\\hline"
outnew[24] <- "\\multicolumn{8}{l}{\\footnotesize \\textit{B. Neighborhood}} \\\\"
outnew[25:26] <- out[c(16:17)]
outnew[27] <- "\\hline"
outnew[28:36] <- out[c(23:31)]
writeLines(outnew, file("../Tables-Figures/TableDiD_Poissonsafety.tex"))
close(file("../Tables-Figures/TableDiD_Poissonsafety.tex"))
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions




################################################################################-
## Table: Diff-in-diff Poisson crime (appendix) ################################
################################################################################-
#Import data 
voutcome <- c("crime_index", "crime_violentall", "crime_property", "crime_robbery", 
              "crime_allassault", "crime_assault", "crime_lowassault", "crime_burglary", 
              "crime_theft", "crime_mvtheft")
dftemp <- fread("../Data/Diff in diff Poisson public_safety2.csv") %>%
  filter(model == "month_id_fe") %>%
  filter(outcome %in% voutcome) %>%
  mutate(sample = "three_hex")
dfest <- fread("../Data/Diff in diff Poisson public_safety1.csv") %>%
  filter(model == "month_id_fe") %>%
  filter(outcome %in% voutcome) %>%
  mutate(sample = "single_hex") %>%
  bind_rows(dftemp) %>%
  arrange(factor(outcome, levels = voutcome)) %>%
  mutate(irr = paste0(format(round((irr - 1)*100, digits = 1), nsmall = 1), "\\%"), 
         irr.low = paste0(format(round((irr.low - 1)*100, digits = 1), nsmall = 1)),
         irr.up = paste0(format(round((irr.up - 1)*100, digits = 1), nsmall = 1)), 
         irr.ci = paste0(irr.low, ", ", irr.up))
for(X in 1:10){
  vname <- voutcome[X]
  dftemp <- dfest %>%
    filter(outcome == vname)
  assign(paste0("dfest", X), dftemp)
}
vsize1 <- dfest %>%
  filter(sample == "single_hex") %>%
  pull(n) %>%
  scales::comma(accuracy = 1)
vsize2 <- dfest %>%
  filter(sample == "three_hex") %>%
  pull(n) %>%
  scales::comma(accuracy = 1)
vmean1 <- dfest %>%
  filter(sample == "single_hex") %>%
  pull(mean) %>%
  scales::comma(accuracy = 0.1)
vmean2 <- dfest %>%
  filter(sample == "three_hex") %>%
  pull(mean) %>%
  scales::comma(accuracy = 0.1)
virr1 <- dfest %>%
  filter(sample == "single_hex") %>%
  pull(irr)
virr2 <- dfest %>%
  filter(sample == "three_hex") %>%
  pull(irr)
virrci1 <- dfest %>%
  filter(sample == "single_hex") %>%
  pull(irr.ci)
virrci2 <- dfest %>%
  filter(sample == "three_hex") %>%
  pull(irr.ci)

#Export table
dftemp <- data.frame(replicate(40, sample(20)))
lm1 <- lm(X1~X2+X3-1, data = dftemp)
covlabels <- rep("Treat*Post", 2)
out <- stargazer(lm1, lm1, lm1, lm1, lm1, lm1, lm1, lm1, lm1, lm1, 
                 title = "Association between opening an overdose prevention center and crime",
                 label = "TableDiD_Poissoncrime",
                 column.sep.width = "-2pt",
                 font.size = "small",
                 no.space = TRUE,
                 coef = list(dfest1$est, dfest2$est, dfest3$est, dfest4$est, dfest5$est, 
                             dfest6$est, dfest7$est, dfest8$est, dfest9$est, dfest10$est),
                 se = list(dfest1$se, dfest2$se, dfest3$se, dfest4$se, dfest5$se, 
                           dfest6$se, dfest7$se, dfest8$se, dfest9$se, dfest10$se),
                 digits = 2,
                 covariate.labels = covlabels,
                 report = "vc*s",
                 omit.stat = c("all"),
                 dep.var.caption  = "caption placeholder",
                 column.labels = c("index", "violent", "property", "robbery", "allassault", "assault", 
                                   "lowassault", "burglary", "theft", "mvtheft"),
                 add.lines = list(c("Percent change", virr1),
                                  c("95\\% CI (\\%)", virrci1),
                                  c("Mean", vmean1),
                                  c("Observations", vsize1), 
                                  c("Percent change", virr2),
                                  c("95\\% CI (\\%)", virrci2),
                                  c("Mean", vmean2),
                                  c("Observations", vsize2)),
                 model.numbers = TRUE,
                 dep.var.labels.include = FALSE,
                 notes = "placeholder",
                 notes.append = FALSE,
                 header = FALSE,
                 star.cutoffs = c(0.05, 0.01, 0.001),
                 #type = "text")
                 type = "latex")
notes <- "\\\\multicolumn{11}{l} {\\\\parbox[t]{21.5cm}{ \\\\scriptsize Notes: 
Difference-in-differences Poisson regression estimates on the crime effects of 
opening the overdose prevention centers. The specifications include hexagon and month-year 
fixed effects. Robust standard errors clustered at the hexagon level in parentheses. 
The specification follows the equation in the Supplementary Material: Statistical Methods, where
POST is an indicator for whether a given observation occurs after December, 2021 when the 2 safe 
injection sites were opened to the public. TREAT is an indicator for whether a hexagon contains 
one of the 2 OPCs as opposed to a comparison unit. Hence, the table shows the coefficient on the 
interaction between POST and TREAT, which is the estimated difference-in-differences intervention effect.
Index crimes include the six UCR part I crimes (murder, robbery, aggravated assault, 
burglary, theft, and motor vehicle). Violent crimes include murder, robbery, and aggravated
and simple assault. Property crimes include burglary, theft, and motor vehicle theft. 
Panel A examines the immediate vicinity (a single hexagon around the site). 
Panel B inspects the neighborhood (three hexagons surrounding the site). 
The bottom rows exhibit the percentage change (incidence rate ratio - 1 = exp($\\\\beta$)-1), 
followed by the 95 percent confidence interval, and the pre-intervention mean count crime on 
the neighborhoods with OPCs and the number of observations. 
$^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001. }} \\\\"
out <- gsub(pattern = "\\\\textit\\{Note.*", replacement = notes, x = out)
out <- gsub(pattern = " & \\\\multicolumn\\{10\\}\\{c\\}\\{caption placeholder.* ", replacement = "", x = out)
out <- gsub(pattern = "\\\\cline.*", replacement = " ", x = out)
out <- gsub(pattern = "\\\\\\\\\\[\\-1\\.8ex\\]", replacement = " ", x = out)
outnew <- character(length = length(out))
outnew <- out[1:10]
outnew[11] <- "& \\thead{Index \\\\ crime} & Violent & Property & Robbery & Assault & \\thead{Aggr \\\\ Assault} 
            & \\thead{Simple \\\\ Assault} & Burglary & Theft & \\thead{Vehicle \\\\ theft} \\\\"
outnew[12:13] <- out[12:13]
outnew[14] <- "\\multicolumn{10}{l}{\\footnotesize \\textit{A. Immediate vicinity}} \\\\"
outnew[15:16] <- out[c(14:15)]
outnew[17] <- "\\hline"
outnew[18:21] <- out[c(19:22)]
outnew[22] <- "\\hline"
outnew[23] <- "\\hline"
outnew[24] <- "\\multicolumn{10}{l}{\\footnotesize \\textit{B. Neighborhood}} \\\\"
outnew[25:26] <- out[c(16:17)]
outnew[27] <- "\\hline"
outnew[28:36] <- out[c(23:31)]
writeLines(outnew, file("../Tables-Figures/TableDiD_Poissoncrime.tex"))
close(file("../Tables-Figures/TableDiD_Poissoncrime.tex"))
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions




####################################################################################-
## Table: Diff-in-diff Poisson crime & calls for service (pre-trend controls) ######
####################################################################################-
#Import data 
voutcome <- c("crime_violentall", "crime_property", "arrest_weapon", "arrest_drugs", 
              "summons", "call_crime", "call_medic", "call_qol")
dftemp <- fread("../Data/Diff in diff Poisson public_safety2.csv") %>%
  filter(model == "pretrends") %>%
  filter(outcome %in% voutcome) %>%
  mutate(sample = "three_hex")
dfest <- fread("../Data/Diff in diff Poisson public_safety1.csv") %>%
  filter(model == "pretrends") %>%
  filter(outcome %in% voutcome) %>%
  mutate(sample = "single_hex") %>%
  bind_rows(dftemp) %>%
  arrange(factor(outcome, levels = voutcome)) %>%
  mutate(irr = paste0(format(round((irr - 1)*100, digits = 1), nsmall = 1), "\\%"), 
         irr.low = paste0(format(round((irr.low - 1)*100, digits = 1), nsmall = 1), "\\%"),
         irr.up = paste0(format(round((irr.up - 1)*100, digits = 1), nsmall = 1), "\\%"), 
         irr.ci = paste0(irr.low, ", ", irr.up))
for(X in 1:8){
  vname <- voutcome[X]
  dftemp <- dfest %>%
    filter(outcome == vname)
  assign(paste0("dfest", X), dftemp)
}
vsize1 <- dfest %>%
  filter(sample == "single_hex") %>%
  pull(n) %>%
  scales::comma(accuracy = 1)
vsize2 <- dfest %>%
  filter(sample == "three_hex") %>%
  pull(n) %>%
  scales::comma(accuracy = 1)
vmean1 <- dfest %>%
  filter(sample == "single_hex") %>%
  pull(mean) %>%
  scales::comma(accuracy = 0.1)
vmean2 <- dfest %>%
  filter(sample == "three_hex") %>%
  pull(mean) %>%
  scales::comma(accuracy = 0.1)
virr1 <- dfest %>%
  filter(sample == "single_hex") %>%
  pull(irr)
virr2 <- dfest %>%
  filter(sample == "three_hex") %>%
  pull(irr)
virrci1 <- dfest %>%
  filter(sample == "single_hex") %>%
  pull(irr.ci)
virrci2 <- dfest %>%
  filter(sample == "three_hex") %>%
  pull(irr.ci)

#Export table
dftemp <- data.frame(replicate(40, sample(20)))
lm1 <- lm(X1~X2+X3-1, data = dftemp)
covlabels <- rep("Treat*Post", 2)
out <- stargazer(lm1, lm1, lm1, lm1, lm1, lm1, lm1, lm1, 
                 title = "Association between opening an overdose prevention center and public safety, pre-trend controls",
                 label = "TableDiD_safetypretrends",
                 column.sep.width = "-2pt",
                 font.size = "small",
                 no.space = TRUE,
                 coef = list(dfest1$est, dfest2$est, dfest3$est, dfest4$est, dfest5$est, 
                             dfest6$est, dfest7$est, dfest8$est),
                 se = list(dfest1$se, dfest2$se, dfest3$se, dfest4$se, dfest5$se, 
                           dfest6$se, dfest7$se, dfest8$se),
                 digits = 2,
                 covariate.labels = covlabels,
                 report = "vc*s",
                 omit.stat = c("all"),
                 dep.var.caption  = "caption placeholder",
                 column.labels = c("violentall", "property", "weapon", "drugs", "summons",
                                   "crime", "medic", "qol"),
                 add.lines = list(c("Percent change", virr1),
                                  c("95\\% CI", virrci1),
                                  c("Mean", vmean1),
                                  c("Observations", vsize1), 
                                  c("Percent change", virr2),
                                  c("95\\% CI", virrci2),
                                  c("Mean", vmean2),
                                  c("Observations", vsize2)),
                 model.numbers = TRUE,
                 dep.var.labels.include = FALSE,
                 notes = "placeholder",
                 notes.append = FALSE,
                 header = FALSE,
                 star.cutoffs = c(0.05, 0.01, 0.001),
                 #type = "text")
                 type = "latex")
notes <- "\\\\multicolumn{9}{l} {\\\\parbox[t]{22.5cm}{ \\\\scriptsize Notes: 
Difference-in-differences Poisson regression estimates on the association of public safety and
the opening of the overdose prevention centers. The specifications include hexagon and month-year 
fixed effects and controls for two four-month bin pre-intervention indicator variables as follows:
$y_{it} = \\\\omega_i + \\\\sigma_t + \\\\sum_{\\\\tau=-2}^{-3}\\\\beta_{0\\\\tau}D_{it} + \\\\beta_1D_{it} + e_{it}$. 
Robust standard errors clustered at the hexagon level in parentheses. 
POST is an indicator for whether a given observation occurs after December, 2021 when the 2 safe 
injection sites were opened to the public. TREAT is an indicator for whether a hexagon contains 
one of the 2 OPCs as opposed to a comparison unit. Hence, the table shows the coefficient on the 
interaction between POST and TREAT, which is the estimated difference-in-differences intervention effect.
Violent crimes include murder, robbery, and aggravated and simple assault. 
Property crimes include burglary, theft, and motor vehicle theft. Weapons refer to 
criminal possession of a weapon. Drugs mean the sale or possession of dangerous drugs. 
Crime 911 calls refer to those made to law enforcement where there was a possible crime 
in-progress or one has been committed. Medical calls include those needing an ambulance. 
Nuisance calls include 911 calls for trespass and 311 calls about homelessness 
(assisting a homeless person, encampment, and homeless street condition) and disorder 
(seeing a rodent, graffiti, dirty and unsanitary conditions, drug and drinking activity, 
urinating in public, and those 311 calls under the New York Police Department jurisdiction 
such as an abandoned vehicle and noise complaint). Panel A examines the 
immediate vicinity (a single hexagon around the site). Panel B inspects the neighborhood 
(three hexagons surrounding the site). The bottom rows exhibit the percentage change 
(incidence rate ratio - 1 = exp($\\\\beta$)-1), followed by the 95 percent
confidence interval, and the pre-intervention mean count crime on the neighborhoods with OPCs and 
the number of observations. $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001. }} \\\\"
out <- gsub(pattern = "\\\\textit\\{Note.*", replacement = notes, x = out)
out <- gsub(pattern = " & \\\\multicolumn\\{8\\}\\{c\\}\\{caption placeholder.* ", replacement = "", x = out)
out <- gsub(pattern = "\\\\cline.*", replacement = " ", x = out)
out <- gsub(pattern = "\\\\\\\\\\[\\-1\\.8ex\\]", replacement = " ", x = out)
outnew <- character(length = length(out))
outnew <- out[1:9]
outnew[10] <- "& \\multicolumn{2}{c}{Crime} & \\multicolumn{3}{c}{Law enforcement} & \\multicolumn{3}{c}{Calls for service} 
               \\\\ \\cmidrule(lr){2-3}  \\cmidrule(lr){4-6} \\cmidrule(lr){7-9} "
outnew[11] <- "& Violent & Property & \\thead{Weapons \\\\ arrests} & \\thead{Drug \\\\ arrests} 
                & \\thead{Criminal \\\\ summons} & \\thead{Crime \\\\ 911 calls} 
                & \\thead{Medical \\\\ 911 calls} & \\thead{Nuisance \\\\ calls}  \\\\"
outnew[12:13] <- out[12:13]
outnew[14] <- "\\multicolumn{8}{l}{\\footnotesize \\textit{A. Immediate vicinity}} \\\\"
outnew[15:16] <- out[c(14:15)]
outnew[17] <- "\\hline"
outnew[18:21] <- out[c(19:22)]
outnew[22] <- "\\hline"
outnew[23] <- "\\hline"
outnew[24] <- "\\multicolumn{8}{l}{\\footnotesize \\textit{B. Neighborhood}} \\\\"
outnew[25:26] <- out[c(16:17)]
outnew[27] <- "\\hline"
outnew[28:36] <- out[c(23:31)]
writeLines(outnew, file("../Tables-Figures/TableDiD_safetypretrends.tex"))
close(file("../Tables-Figures/TableDiD_safetypretrends.tex"))
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions







################################################################################-
## Table: Diff-in-diff Negative Binomial crime & calls for service #############
################################################################################-
#Import data 
voutcome <- c("crime_violentall", "crime_property", "arrest_weapon", "arrest_drugs", 
              "summons", "call_crime", "call_medic", "call_qol")
dftemp <- fread("../Data/Diff in diff NegBin public_safety2.csv") %>%
  filter(model == "month_id_fe") %>%
  filter(outcome %in% voutcome) %>%
  mutate(sample = "three_hex", 
         model = "")
dfest <- fread("../Data/Diff in diff NegBin public_safety1.csv") %>%
  filter(model == "month_id_fe") %>%
  filter(outcome %in% voutcome) %>%
  mutate(sample = "single_hex") %>%
  bind_rows(dftemp) %>%
  arrange(factor(outcome, levels = voutcome)) %>%
  mutate(irr = paste0(format(round((irr - 1)*100, digits = 1), nsmall = 1), "\\%"), 
         irr.low = paste0(format(round((irr.low - 1)*100, digits = 1), nsmall = 1), "\\%"),
         irr.up = paste0(format(round((irr.up - 1)*100, digits = 1), nsmall = 1), "\\%"), 
         irr.ci = paste0(irr.low, ", ", irr.up))
for(X in 1:8){
  vname <- voutcome[X]
  dftemp <- dfest %>%
    filter(outcome == vname)
  assign(paste0("dfest", X), dftemp)
}
vsize1 <- dfest %>%
  filter(sample == "single_hex") %>%
  pull(n) %>%
  scales::comma(accuracy = 1)
vsize2 <- dfest %>%
  filter(sample == "three_hex") %>%
  pull(n) %>%
  scales::comma(accuracy = 1)
vmean1 <- dfest %>%
  filter(sample == "single_hex") %>%
  pull(mean) %>%
  scales::comma(accuracy = 0.1)
vmean2 <- dfest %>%
  filter(sample == "three_hex") %>%
  pull(mean) %>%
  scales::comma(accuracy = 0.1)
virr1 <- dfest %>%
  filter(sample == "single_hex") %>%
  pull(irr)
virr2 <- dfest %>%
  filter(sample == "three_hex") %>%
  pull(irr)
virrci1 <- dfest %>%
  filter(sample == "single_hex") %>%
  pull(irr.ci)
virrci2 <- dfest %>%
  filter(sample == "three_hex") %>%
  pull(irr.ci)

#Export table
dftemp <- data.frame(replicate(40, sample(20)))
lm1 <- lm(X1~X2+X3-1, data = dftemp)
covlabels <- rep("Treat*Post", 2)
out <- stargazer(lm1, lm1, lm1, lm1, lm1, lm1, lm1, lm1, 
                 title = "Association between opening an overdose prevention center and public safety, Negative binomial regression",
                 label = "TableDiD_NegBinsafety",
                 column.sep.width = "-2pt",
                 font.size = "small",
                 no.space = TRUE,
                 coef = list(dfest1$est, dfest2$est, dfest3$est, dfest4$est, dfest5$est, 
                             dfest6$est, dfest7$est, dfest8$est),
                 se = list(dfest1$se, dfest2$se, dfest3$se, dfest4$se, dfest5$se, 
                           dfest6$se, dfest7$se, dfest8$se),
                 digits = 2,
                 covariate.labels = covlabels,
                 report = "vc*s",
                 omit.stat = c("all"),
                 dep.var.caption  = "caption placeholder",
                 column.labels = c("violentall", "property", "weapon", "drugs", "summons",
                                   "crime", "medic", "qol"),
                 add.lines = list(c("Percent change", virr1),
                                  c("95\\% CI", virrci1),
                                  c("Mean", vmean1),
                                  c("Observations", vsize1), 
                                  c("Percent change", virr2),
                                  c("95\\% CI", virrci2),
                                  c("Mean", vmean2),
                                  c("Observations", vsize2)),
                 model.numbers = TRUE,
                 dep.var.labels.include = FALSE,
                 notes = "placeholder",
                 notes.append = FALSE,
                 header = FALSE,
                 star.cutoffs = c(0.05, 0.01, 0.001),
                 #type = "text")
                 type = "latex")
notes <- "\\\\multicolumn{9}{l} {\\\\parbox[t]{22.5cm}{ \\\\scriptsize Notes: 
Difference-in-differences Negative binomial regression estimates on the association of public safety and
the opening of the overdose prevention centers. The specifications include hexagon and month-year 
fixed effects. Robust standard errors clustered at the hexagon level in parentheses. 
The specification follows the equation in the Supplementary Material: Statistical Methods, where
POST is an indicator for whether a given observation occurs after December, 2021 when the 2 safe 
injection sites were opened to the public. TREAT is an indicator for whether a hexagon contains 
one of the 2 OPCs as opposed to a comparison unit. Hence, the table shows the coefficient on the 
interaction between POST and TREAT, which is the estimated difference-in-differences intervention effect.
Violent crimes include murder, robbery, and aggravated and simple assault. 
Property crimes include burglary, theft, and motor vehicle theft. Weapons refer to 
criminal possession of a weapon. Drugs mean the sale or possession of dangerous drugs. 
Crime 911 calls refer to those made to law enforcement where there was a possible crime 
in-progress or one has been committed. Medical calls include those needing an ambulance. 
Nuisance calls include 911 calls for trespass and 311 calls about homelessness 
(assisting a homeless person, encampment, and homeless street condition) and disorder 
(seeing a rodent, graffiti, dirty and unsanitary conditions, drug and drinking activity, 
urinating in public, and those 311 calls under the New York Police Department jurisdiction 
such as an abandoned vehicle and noise complaint). Panel A examines the 
immediate vicinity (a single hexagon around the site). Panel B inspects the neighborhood 
(three hexagons surrounding the site). The bottom rows exhibit the percentage change 
(incidence rate ratio - 1 = exp($\\\\beta$)-1), followed by the 95 percent
confidence interval, and the pre-intervention mean count crime on the neighborhoods with OPCs and 
the number of observations. $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001. }} \\\\"
out <- gsub(pattern = "\\\\textit\\{Note.*", replacement = notes, x = out)
out <- gsub(pattern = " & \\\\multicolumn\\{8\\}\\{c\\}\\{caption placeholder.* ", replacement = "", x = out)
out <- gsub(pattern = "\\\\cline.*", replacement = " ", x = out)
out <- gsub(pattern = "\\\\\\\\\\[\\-1\\.8ex\\]", replacement = " ", x = out)
outnew <- character(length = length(out))
outnew <- out[1:9]
outnew[10] <- "& \\multicolumn{2}{c}{Crime} & \\multicolumn{3}{c}{Law enforcement} & \\multicolumn{3}{c}{Calls for service} 
               \\\\ \\cmidrule(lr){2-3}  \\cmidrule(lr){4-6} \\cmidrule(lr){7-9} "
outnew[11] <- "& Violent & Property & \\thead{Weapons \\\\ arrests} & \\thead{Drug \\\\ arrests} 
                & \\thead{Criminal \\\\ summons} & \\thead{Crime \\\\ 911 calls} 
                & \\thead{Medical \\\\ 911 calls} & \\thead{Nuisance \\\\ calls}  \\\\"
outnew[12:13] <- out[12:13]
outnew[14] <- "\\multicolumn{8}{l}{\\footnotesize \\textit{A. Immediate vicinity}} \\\\"
outnew[15:16] <- out[c(14:15)]
outnew[17] <- "\\hline"
outnew[18:21] <- out[c(19:22)]
outnew[22] <- "\\hline"
outnew[23] <- "\\hline"
outnew[24] <- "\\multicolumn{8}{l}{\\footnotesize \\textit{B. Neighborhood}} \\\\"
outnew[25:26] <- out[c(16:17)]
outnew[27] <- "\\hline"
outnew[28:36] <- out[c(23:31)]
writeLines(outnew, file("../Tables-Figures/TableDiD_NegBinsafety.tex"))
close(file("../Tables-Figures/TableDiD_NegBinsafety.tex"))
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions






################################################################################-
## Table: Diff-in-diff Poisson high crime sample crime & calls for service #####
################################################################################-
#Import data 
voutcome <- c("crime_violentall", "crime_property", "arrest_weapon", "arrest_drugs", 
              "summons", "call_crime", "call_medic", "call_qol")
dftemp <- fread("../Data/Diff in diff Poisson highcrimecity public_safety2.csv") %>%
  filter(model == "month_id_fe") %>%
  filter(outcome %in% voutcome) %>%
  mutate(sample = "three_hex")
dfest <- fread("../Data/Diff in diff Poisson highcrimecity public_safety1.csv") %>%
  filter(model == "month_id_fe") %>%
  filter(outcome %in% voutcome) %>%
  mutate(sample = "single_hex") %>%
  bind_rows(dftemp) %>%
  arrange(factor(outcome, levels = voutcome)) %>%
  mutate(irr = paste0(format(round((irr - 1)*100, digits = 1), nsmall = 1), "\\%"), 
         irr.low = paste0(format(round((irr.low - 1)*100, digits = 1), nsmall = 1), "\\%"),
         irr.up = paste0(format(round((irr.up - 1)*100, digits = 1), nsmall = 1), "\\%"), 
         irr.ci = paste0(irr.low, ", ", irr.up))
for(X in 1:8){
  vname <- voutcome[X]
  dftemp <- dfest %>%
    filter(outcome == vname)
  assign(paste0("dfest", X), dftemp)
}
vsize1 <- dfest %>%
  filter(sample == "single_hex") %>%
  pull(n) %>%
  scales::comma(accuracy = 1)
vsize2 <- dfest %>%
  filter(sample == "three_hex") %>%
  pull(n) %>%
  scales::comma(accuracy = 1)
vmean1 <- dfest %>%
  filter(sample == "single_hex") %>%
  pull(mean) %>%
  scales::comma(accuracy = 0.1)
vmean2 <- dfest %>%
  filter(sample == "three_hex") %>%
  pull(mean) %>%
  scales::comma(accuracy = 0.1)
virr1 <- dfest %>%
  filter(sample == "single_hex") %>%
  pull(irr)
virr2 <- dfest %>%
  filter(sample == "three_hex") %>%
  pull(irr)
virrci1 <- dfest %>%
  filter(sample == "single_hex") %>%
  pull(irr.ci)
virrci2 <- dfest %>%
  filter(sample == "three_hex") %>%
  pull(irr.ci)

#Export table
dftemp <- data.frame(replicate(40, sample(20)))
lm1 <- lm(X1~X2+X3-1, data = dftemp)
covlabels <- rep("Treat*Post", 2)
out <- stargazer(lm1, lm1, lm1, lm1, lm1, lm1, lm1, lm1, 
                 title = "Association between opening an overdose prevention center and public safety, high crime comparison sample",
                 label = "TableDiD_highcrimecitysafety",
                 column.sep.width = "-2pt",
                 font.size = "small",
                 no.space = TRUE,
                 coef = list(dfest1$est, dfest2$est, dfest3$est, dfest4$est, dfest5$est, 
                             dfest6$est, dfest7$est, dfest8$est),
                 se = list(dfest1$se, dfest2$se, dfest3$se, dfest4$se, dfest5$se, 
                           dfest6$se, dfest7$se, dfest8$se),
                 digits = 2,
                 covariate.labels = covlabels,
                 report = "vc*s",
                 omit.stat = c("all"),
                 dep.var.caption  = "caption placeholder",
                 column.labels = c("violentall", "property", "weapon", "drugs", "summons",
                                   "crime", "medic", "qol"),
                 add.lines = list(c("Percent change", virr1),
                                  c("95\\% CI", virrci1),
                                  c("Mean", vmean1),
                                  c("Observations", vsize1), 
                                  c("Percent change", virr2),
                                  c("95\\% CI", virrci2),
                                  c("Mean", vmean2),
                                  c("Observations", vsize2)),
                 model.numbers = TRUE,
                 dep.var.labels.include = FALSE,
                 notes = "placeholder",
                 notes.append = FALSE,
                 header = FALSE,
                 star.cutoffs = c(0.05, 0.01, 0.001),
                 #type = "text")
                 type = "latex")
notes <- "\\\\multicolumn{9}{l} {\\\\parbox[t]{22.5cm}{ \\\\scriptsize Notes: 
Difference-in-differences Poisson regression estimates on the association of public safety and
the opening of the overdose prevention center. The specifications include hexagon and month-year 
fixed effects. Robust standard errors clustered at the hexagon level in parentheses. 
The specification follows the equation in the Supplementary Material: Statistical Methods, where
POST is an indicator for whether a given observation occurs after December, 2021 when the 2 safe 
injection sites were opened to the public. TREAT is an indicator for whether a hexagon contains 
one of the 2 OPCs as opposed to a comparison unit. Hence, the table shows the coefficient on the 
interaction between POST and TREAT, which is the estimated difference-in-differences intervention effect.
The comparison group uses the 250 hexagons with the most index crimes (excluding theft) 
among the seven (10\\\\%) precincts with the highest index crime rates (excluding theft).
Violent crimes include murder, robbery, and aggravated and simple assault. 
Property crimes include burglary, theft, and motor vehicle theft. Weapons refer to 
criminal possession of a weapon. Drugs mean the sale or possession of dangerous drugs. 
Crime 911 calls refer to those made to law enforcement where there was a possible crime 
in-progress or one has been committed. Medical calls include those needing an ambulance. 
Nuisance calls include 911 calls for trespass and 311 calls about homelessness 
(assisting a homeless person, encampment, and homeless street condition) and disorder 
(seeing a rodent, graffiti, dirty and unsanitary conditions, drug and drinking activity, 
urinating in public, and those 311 calls under the New York Police Department jurisdiction 
such as an abandoned vehicle and noise complaint). Panel A examines the 
immediate vicinity (a single hexagon around the site). Panel B inspects the neighborhood 
(three hexagons surrounding the site). The bottom rows exhibit the percentage change 
(incidence rate ratio - 1 = exp($\\\\beta$)-1), followed by the 95 percent
confidence interval, and the pre-intervention mean count crime on the neighborhoods with OPCs and 
the number of observations. $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001. }} \\\\"
out <- gsub(pattern = "\\\\textit\\{Note.*", replacement = notes, x = out)
out <- gsub(pattern = " & \\\\multicolumn\\{8\\}\\{c\\}\\{caption placeholder.* ", replacement = "", x = out)
out <- gsub(pattern = "\\\\cline.*", replacement = " ", x = out)
out <- gsub(pattern = "\\\\\\\\\\[\\-1\\.8ex\\]", replacement = " ", x = out)
outnew <- character(length = length(out))
outnew <- out[1:9]
outnew[10] <- "& \\multicolumn{2}{c}{Crime} & \\multicolumn{3}{c}{Law enforcement} & \\multicolumn{3}{c}{Calls for service} 
               \\\\ \\cmidrule(lr){2-3}  \\cmidrule(lr){4-6} \\cmidrule(lr){7-9} "
outnew[11] <- "& Violent & Property & \\thead{Weapons \\\\ arrests} & \\thead{Drug \\\\ arrests} 
                & \\thead{Criminal \\\\ summons} & \\thead{Crime \\\\ 911 calls} 
                & \\thead{Medical \\\\ 911 calls} & \\thead{Nuisance \\\\ calls}  \\\\"
outnew[12:13] <- out[12:13]
outnew[14] <- "\\multicolumn{8}{l}{\\footnotesize \\textit{A. Immediate vicinity}} \\\\"
outnew[15:16] <- out[c(14:15)]
outnew[17] <- "\\hline"
outnew[18:21] <- out[c(19:22)]
outnew[22] <- "\\hline"
outnew[23] <- "\\hline"
outnew[24] <- "\\multicolumn{8}{l}{\\footnotesize \\textit{B. Neighborhood}} \\\\"
outnew[25:26] <- out[c(16:17)]
outnew[27] <- "\\hline"
outnew[28:36] <- out[c(23:31)]
writeLines(outnew, file("../Tables-Figures/TableDiD_highcrimecitysafety.tex"))
close(file("../Tables-Figures/TableDiD_highcrimecitysafety.tex"))
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions





########################################################################################-
## Table: Diff-in-diff Poisson high drug arrests sample crime & calls for service ######
########################################################################################-
#Import data 
voutcome <- c("crime_violentall", "crime_property", "arrest_weapon", "arrest_drugs", 
              "summons", "call_crime", "call_medic", "call_qol")
dftemp <- fread("../Data/Diff in diff Poisson highdrugarrests public_safety2.csv") %>%
  filter(model == "month_id_fe") %>%
  filter(outcome %in% voutcome) %>%
  mutate(sample = "three_hex")
dfest <- fread("../Data/Diff in diff Poisson highdrugarrests public_safety1.csv") %>%
  filter(model == "month_id_fe") %>%
  filter(outcome %in% voutcome) %>%
  mutate(sample = "single_hex") %>%
  bind_rows(dftemp) %>%
  arrange(factor(outcome, levels = voutcome)) %>%
  mutate(irr = paste0(format(round((irr - 1)*100, digits = 1), nsmall = 1), "\\%"), 
         irr.low = paste0(format(round((irr.low - 1)*100, digits = 1), nsmall = 1), "\\%"),
         irr.up = paste0(format(round((irr.up - 1)*100, digits = 1), nsmall = 1), "\\%"), 
         irr.ci = paste0(irr.low, ", ", irr.up))
for(X in 1:8){
  vname <- voutcome[X]
  dftemp <- dfest %>%
    filter(outcome == vname)
  assign(paste0("dfest", X), dftemp)
}
vsize1 <- dfest %>%
  filter(sample == "single_hex") %>%
  pull(n) %>%
  scales::comma(accuracy = 1)
vsize2 <- dfest %>%
  filter(sample == "three_hex") %>%
  pull(n) %>%
  scales::comma(accuracy = 1)
vmean1 <- dfest %>%
  filter(sample == "single_hex") %>%
  pull(mean) %>%
  scales::comma(accuracy = 0.1)
vmean2 <- dfest %>%
  filter(sample == "three_hex") %>%
  pull(mean) %>%
  scales::comma(accuracy = 0.1)
virr1 <- dfest %>%
  filter(sample == "single_hex") %>%
  pull(irr)
virr2 <- dfest %>%
  filter(sample == "three_hex") %>%
  pull(irr)
virrci1 <- dfest %>%
  filter(sample == "single_hex") %>%
  pull(irr.ci)
virrci2 <- dfest %>%
  filter(sample == "three_hex") %>%
  pull(irr.ci)

#Export table
dftemp <- data.frame(replicate(40, sample(20)))
lm1 <- lm(X1~X2+X3-1, data = dftemp)
covlabels <- rep("Treat*Post", 2)
out <- stargazer(lm1, lm1, lm1, lm1, lm1, lm1, lm1, lm1, 
                 title = "Association between opening an overdose prevention center and public safety, high drug arrests comparison sample",
                 label = "TableDiD_highdrugarrestssafety",
                 column.sep.width = "-2pt",
                 font.size = "small",
                 no.space = TRUE,
                 coef = list(dfest1$est, dfest2$est, dfest3$est, dfest4$est, dfest5$est, 
                             dfest6$est, dfest7$est, dfest8$est),
                 se = list(dfest1$se, dfest2$se, dfest3$se, dfest4$se, dfest5$se, 
                           dfest6$se, dfest7$se, dfest8$se),
                 digits = 2,
                 covariate.labels = covlabels,
                 report = "vc*s",
                 omit.stat = c("all"),
                 dep.var.caption  = "caption placeholder",
                 column.labels = c("violentall", "property", "weapon", "drugs", "summons",
                                   "crime", "medic", "qol"),
                 add.lines = list(c("Percent change", virr1),
                                  c("95\\% CI", virrci1),
                                  c("Mean", vmean1),
                                  c("Observations", vsize1), 
                                  c("Percent change", virr2),
                                  c("95\\% CI", virrci2),
                                  c("Mean", vmean2),
                                  c("Observations", vsize2)),
                 model.numbers = TRUE,
                 dep.var.labels.include = FALSE,
                 notes = "placeholder",
                 notes.append = FALSE,
                 header = FALSE,
                 star.cutoffs = c(0.05, 0.01, 0.001),
                 #type = "text")
                 type = "latex")
notes <- "\\\\multicolumn{9}{l} {\\\\parbox[t]{22.5cm}{ \\\\scriptsize Notes: 
Difference-in-differences Poisson regression estimates on the association of public safety and
the opening of the overdose prevention center. The specifications include hexagon and month-year 
fixed effects. Robust standard errors clustered at the hexagon level in parentheses. 
The specification follows the equation in the Supplementary Material: Statistical Methods, where
POST is an indicator for whether a given observation occurs after December, 2021 when the 2 safe 
injection sites were opened to the public. TREAT is an indicator for whether a hexagon contains 
one of the 2 OPCs as opposed to a comparison unit. Hence, the table shows the coefficient on the 
interaction between POST and TREAT, which is the estimated difference-in-differences intervention effect.
The comparison group uses the 20 hexagons with the most pre-intervention drug arrests across New York City.
Violent crimes include murder, robbery, and aggravated and simple assault. 
Property crimes include burglary, theft, and motor vehicle theft. Weapons refer to 
criminal possession of a weapon. Drugs mean the sale or possession of dangerous drugs. 
Crime 911 calls refer to those made to law enforcement where there was a possible crime 
in-progress or one has been committed. Medical calls include those needing an ambulance. 
Nuisance calls include 911 calls for trespass and 311 calls about homelessness 
(assisting a homeless person, encampment, and homeless street condition) and disorder 
(seeing a rodent, graffiti, dirty and unsanitary conditions, drug and drinking activity, 
urinating in public, and those 311 calls under the New York Police Department jurisdiction 
such as an abandoned vehicle and noise complaint). Panel A examines the 
immediate vicinity (a single hexagon around the site). Panel B inspects the neighborhood 
(three hexagons surrounding the site). The bottom rows exhibit the percentage change 
(incidence rate ratio - 1 = exp($\\\\beta$)-1), followed by the 95 percent
confidence interval, and the pre-intervention mean count crime on the neighborhoods with OPCs and 
the number of observations. $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001. }} \\\\"
out <- gsub(pattern = "\\\\textit\\{Note.*", replacement = notes, x = out)
out <- gsub(pattern = " & \\\\multicolumn\\{8\\}\\{c\\}\\{caption placeholder.* ", replacement = "", x = out)
out <- gsub(pattern = "\\\\cline.*", replacement = " ", x = out)
out <- gsub(pattern = "\\\\\\\\\\[\\-1\\.8ex\\]", replacement = " ", x = out)
outnew <- character(length = length(out))
outnew <- out[1:9]
outnew[10] <- "& \\multicolumn{2}{c}{Crime} & \\multicolumn{3}{c}{Law enforcement} & \\multicolumn{3}{c}{Calls for service} 
               \\\\ \\cmidrule(lr){2-3}  \\cmidrule(lr){4-6} \\cmidrule(lr){7-9} "
outnew[11] <- "& Violent & Property & \\thead{Weapons \\\\ arrests} & \\thead{Drug \\\\ arrests} 
                & \\thead{Criminal \\\\ summons} & \\thead{Crime \\\\ 911 calls} 
                & \\thead{Medical \\\\ 911 calls} & \\thead{Nuisance \\\\ calls}  \\\\"
outnew[12:13] <- out[12:13]
outnew[14] <- "\\multicolumn{8}{l}{\\footnotesize \\textit{A. Immediate vicinity}} \\\\"
outnew[15:16] <- out[c(14:15)]
outnew[17] <- "\\hline"
outnew[18:21] <- out[c(19:22)]
outnew[22] <- "\\hline"
outnew[23] <- "\\hline"
outnew[24] <- "\\multicolumn{8}{l}{\\footnotesize \\textit{B. Neighborhood}} \\\\"
outnew[25:26] <- out[c(16:17)]
outnew[27] <- "\\hline"
outnew[28:36] <- out[c(23:31)]
writeLines(outnew, file("../Tables-Figures/TableDiD_highdrugarrestssafety.tex"))
close(file("../Tables-Figures/TableDiD_highdrugarrestssafety.tex"))
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions





####################################################################################-
## Table: Diff-in-diff Poisson crime & calls for service (borough_timeFE) ##########
####################################################################################-
#Import data 
voutcome <- c("crime_violentall", "crime_property", "arrest_weapon", "arrest_drugs", 
              "summons", "call_crime", "call_medic", "call_qol")
dftemp <- fread("../Data/Diff in diff Poisson public_safety2.csv") %>%
  filter(model == "borough_time_FE") %>%
  filter(outcome %in% voutcome) %>%
  mutate(sample = "three_hex")
dfest <- fread("../Data/Diff in diff Poisson public_safety1.csv") %>%
  filter(model == "borough_time_FE") %>%
  filter(outcome %in% voutcome) %>%
  mutate(sample = "single_hex") %>%
  bind_rows(dftemp) %>%
  arrange(factor(outcome, levels = voutcome)) %>%
  mutate(irr = paste0(format(round((irr - 1)*100, digits = 1), nsmall = 1), "\\%"), 
         irr.low = paste0(format(round((irr.low - 1)*100, digits = 1), nsmall = 1), "\\%"),
         irr.up = paste0(format(round((irr.up - 1)*100, digits = 1), nsmall = 1), "\\%"), 
         irr.ci = paste0(irr.low, ", ", irr.up))
for(X in 1:8){
  vname <- voutcome[X]
  dftemp <- dfest %>%
    filter(outcome == vname)
  assign(paste0("dfest", X), dftemp)
}
vsize1 <- dfest %>%
  filter(sample == "single_hex") %>%
  pull(n) %>%
  scales::comma(accuracy = 1)
vsize2 <- dfest %>%
  filter(sample == "three_hex") %>%
  pull(n) %>%
  scales::comma(accuracy = 1)
vmean1 <- dfest %>%
  filter(sample == "single_hex") %>%
  pull(mean) %>%
  scales::comma(accuracy = 0.1)
vmean2 <- dfest %>%
  filter(sample == "three_hex") %>%
  pull(mean) %>%
  scales::comma(accuracy = 0.1)
virr1 <- dfest %>%
  filter(sample == "single_hex") %>%
  pull(irr)
virr2 <- dfest %>%
  filter(sample == "three_hex") %>%
  pull(irr)
virrci1 <- dfest %>%
  filter(sample == "single_hex") %>%
  pull(irr.ci)
virrci2 <- dfest %>%
  filter(sample == "three_hex") %>%
  pull(irr.ci)

#Export table
dftemp <- data.frame(replicate(40, sample(20)))
lm1 <- lm(X1~X2+X3-1, data = dftemp)
covlabels <- rep("Treat*Post", 2)
out <- stargazer(lm1, lm1, lm1, lm1, lm1, lm1, lm1, lm1, 
                 title = "Association between opening an overdose prevention center and public safety, Patrol Borough linear time trends",
                 label = "TableDiD_safetyborough_timeFE",
                 column.sep.width = "-2pt",
                 font.size = "small",
                 no.space = TRUE,
                 coef = list(dfest1$est, dfest2$est, dfest3$est, dfest4$est, dfest5$est, 
                             dfest6$est, dfest7$est, dfest8$est),
                 se = list(dfest1$se, dfest2$se, dfest3$se, dfest4$se, dfest5$se, 
                           dfest6$se, dfest7$se, dfest8$se),
                 digits = 2,
                 covariate.labels = covlabels,
                 report = "vc*s",
                 omit.stat = c("all"),
                 dep.var.caption  = "caption placeholder",
                 column.labels = c("violentall", "property", "weapon", "drugs", "summons",
                                   "crime", "medic", "qol"),
                 add.lines = list(c("Percent change", virr1),
                                  c("95\\% CI", virrci1),
                                  c("Mean", vmean1),
                                  c("Observations", vsize1), 
                                  c("Percent change", virr2),
                                  c("95\\% CI", virrci2),
                                  c("Mean", vmean2),
                                  c("Observations", vsize2)),
                 model.numbers = TRUE,
                 dep.var.labels.include = FALSE,
                 #notes = "placeholder",
                 #notes.append = FALSE,
                 header = FALSE,
                 star.cutoffs = c(0.05, 0.01, 0.001),
                 #type = "text")
                 type = "latex")
notes <- "\\\\multicolumn{9}{l} {\\\\parbox[t]{22.5cm}{ \\\\scriptsize Notes: 
Difference-in-differences Poisson regression estimates on the association of public safety and
the opening of the overdose prevention centers. The specifications include hexagon and month-year 
fixed effects as well as police patrol borough linear time trends (New York City is divided into 
eight police patrol boroughs: Manhattan North, Manhattan South, Queens North, Queens South, 
Brooklyn North, Brooklyn South, Bronx and Staten Island).
Robust standard errors clustered at the hexagon level in parentheses. 
POST is an indicator for whether a given observation occurs after December, 2021 when the 2 safe 
injection sites were opened to the public. TREAT is an indicator for whether a hexagon contains 
one of the 2 OPCs as opposed to a comparison unit. Hence, the table shows the coefficient on the 
interaction between POST and TREAT, which is the estimated difference-in-differences intervention effect.
Violent crimes include murder, robbery, and aggravated and simple assault. 
Property crimes include burglary, theft, and motor vehicle theft. Weapons refer to 
criminal possession of a weapon. Drugs mean the sale or possession of dangerous drugs. 
Crime 911 calls refer to those made to law enforcement where there was a possible crime 
in-progress or one has been committed. Medical calls include those needing an ambulance. 
Nuisance calls include 911 calls for trespass and 311 calls about homelessness 
(assisting a homeless person, encampment, and homeless street condition) and disorder 
(seeing a rodent, graffiti, dirty and unsanitary conditions, drug and drinking activity, 
urinating in public, and those 311 calls under the New York Police Department jurisdiction 
such as an abandoned vehicle and noise complaint). Panel A examines the 
immediate vicinity (a single hexagon around the site). Panel B inspects the neighborhood 
(three hexagons surrounding the site). The bottom rows exhibit the percentage change 
(incidence rate ratio - 1 = exp($\\\\beta$)-1), followed by the 95 percent
confidence interval, and the pre-intervention mean count crime on the neighborhoods with OPCs and 
the number of observations. $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001. }} \\\\"
out <- gsub(pattern = "\\\\textit\\{Note.*", replacement = notes, x = out)
out <- gsub(pattern = " & \\\\multicolumn\\{8\\}\\{c\\}\\{caption placeholder.* ", replacement = "", x = out)
out <- gsub(pattern = "\\\\cline.*", replacement = " ", x = out)
out <- gsub(pattern = "\\\\\\\\\\[\\-1\\.8ex\\]", replacement = " ", x = out)
outnew <- character(length = length(out))
outnew <- out[1:9]
outnew[10] <- "& \\multicolumn{2}{c}{Crime} & \\multicolumn{3}{c}{Law enforcement} & \\multicolumn{3}{c}{Calls for service} 
               \\\\ \\cmidrule(lr){2-3}  \\cmidrule(lr){4-6} \\cmidrule(lr){7-9} "
outnew[11] <- "& Violent & Property & \\thead{Weapons \\\\ arrests} & \\thead{Drug \\\\ arrests} 
                & \\thead{Criminal \\\\ summons} & \\thead{Crime \\\\ 911 calls} 
                & \\thead{Medical \\\\ 911 calls} & \\thead{Nuisance \\\\ calls}  \\\\"
outnew[12:13] <- out[12:13]
outnew[14] <- "\\multicolumn{8}{l}{\\footnotesize \\textit{A. Immediate vicinity}} \\\\"
outnew[15:16] <- out[c(14:15)]
outnew[17] <- "\\hline"
outnew[18:21] <- out[c(19:22)]
outnew[22] <- "\\hline"
outnew[23] <- "\\hline"
outnew[24] <- "\\multicolumn{8}{l}{\\footnotesize \\textit{B. Neighborhood}} \\\\"
outnew[25:26] <- out[c(16:17)]
outnew[27] <- "\\hline"
outnew[28:36] <- out[c(23:31)]
writeLines(outnew, file("../Tables-Figures/TableDiD_safetyborough_timeFE.tex"))
close(file("../Tables-Figures/TableDiD_safetyborough_timeFE.tex"))
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions




################################################################################-
## Table: Synthetic control method crime & calls for service ###################
################################################################################-
#Import data: estimate the mean at the bimonthly-hexagon level
voutcome <- c("crime_violentall", "crime_property", "arrest_weapon", "arrest_drugs", 
              "summons", "call_crime", "call_medic", "call_qol")
dfmean <- fread("../Data/Synth gap public_safety hexagon1.csv", integer64 = "character") %>%
  filter(datenum >= 2021.83 & synthetic == 1) %>%
  group_by(id) %>%
  summarise_at(vars(any_of(voutcome)), function(x) format(round(mean(x), 1), nsmall = 1)) %>%
  pivot_longer(cols = crime_violentall:call_qol, names_to = "outcome", values_to = "mean") %>%
  pivot_wider(names_from = "id", values_from = "mean", "names_prefix" = "id") %>%
  mutate(id4 = ifelse(outcome %in% c("crime_violentall", "arrest_drugs", "call_medic"), "", id4))
dfest <- fread("../Data/Synth placebo public_safety hexagon1.csv") %>%
  mutate(post = ifelse(datenum >= 2021.83, 1, 0)) %>%
  select(id, id_treated, datenum, post, any_of(voutcome)) %>%
  mutate(crime_violentall = ifelse(id == 4, NA, crime_violentall),
         arrest_drugs = ifelse(id == 4, NA, arrest_drugs), 
         call_medic = ifelse(id == 4, NA, call_medic))
dfpre <- dfest %>%
  filter(post == 0 & id_treated %in% c(4,11)) %>%
  group_by(id_treated) %>%
  summarise_at(vars(crime_violentall:call_qol), mean) %>%
  ungroup() %>%
  mutate(post = 0)
dfpost <- dfest %>%
  filter(post == 1 & id_treated %in% c(4,11)) %>%
  group_by(id_treated) %>%
  summarise_at(vars(crime_violentall:call_qol), mean) %>%
  ungroup() %>%
  mutate(post = 1) 
dfdiff <- dfpost %>%
  bind_rows(dfpre) %>%
  group_by(id_treated) %>%
  mutate_at(vars(crime_violentall:call_qol), funs(.[post == 1] - .)) %>%
  filter(post == 0)
dfpre <- dfpre %>%
  pivot_longer(cols = crime_violentall:call_qol, names_to = "outcome", values_to = "est") %>%
  pivot_wider(names_from = "id_treated", values_from = "est", "names_prefix" = "id") %>%
  mutate_at(vars(id4, id11), function(x) format(round(x, 1), nsmall = 1)) %>%
  mutate_at(vars(id4, id11), function(x) ifelse(grepl("NA", x), "", x))
dfpost <- dfpost %>%
  pivot_longer(cols = crime_violentall:call_qol, names_to = "outcome", values_to = "est") %>%
  pivot_wider(names_from = "id_treated", values_from = "est", "names_prefix" = "id") %>%
  mutate_at(vars(id4, id11), function(x) format(round(x, 1), nsmall = 1)) %>%
  mutate_at(vars(id4, id11), function(x) ifelse(grepl("NA", x), "", x))
dfrmspe <- dfest %>%
  filter(post == 0 & id_treated %in% c(4,11)) %>%
  group_by(id) %>%
  summarise_at(vars(crime_violentall:call_qol), function(x) mean((x - 0)^2)^0.5) %>%
  mutate_at(vars(crime_violentall:call_qol), function(x) format(round(x, 1), nsmall = 1)) %>%
  pivot_longer(cols = crime_violentall:call_qol, names_to = "outcome", values_to = "rmspe") %>%
  pivot_wider(names_from = "id", values_from = "rmspe", names_prefix = "id") %>%
  mutate_at(vars(id4, id11), function(x) ifelse(grepl("NA", x), "", x))
dftemp <- dfest %>%
  filter(post == 0) %>%
  group_by(id, id_treated) %>%
  summarise_at(vars(crime_violentall:call_qol), function(x) mean((x - 0)^2)^0.5) %>%
  group_by(id) %>%
  mutate_at(vars(crime_violentall:call_qol), funs(./.[id == id_treated])) %>%
  ungroup() %>%
  pivot_longer(cols = crime_violentall:call_qol, names_to = "outcome", values_to = "rmspe")
dfpvalue1 <- dfest %>%
  filter(post == 1) %>%
  group_by(id, id_treated) %>%
  summarise_at(vars(crime_violentall:call_qol), mean) %>%
  pivot_longer(cols = crime_violentall:call_qol, names_to = "outcome", values_to = "mean") %>%
  left_join(dftemp, by = c("id", "id_treated", "outcome")) %>%
  ungroup() %>%
  mutate(mean = ifelse(rmspe > 4, NA, mean)) %>%
  group_by(id, outcome) %>%
  mutate(pvalue = ifelse(id %in% 4 & outcome %in% c("crime_property", "call_qol") |
                           id %in% 11 & outcome %in% c("crime_violentall", "arrest_weapon", "call_crime", 
                                                       "call_trespass", "call_medic", "call_qol"), 
                         1-cume_dist(mean), cume_dist(mean))) %>%
  filter(id_treated %in% c(4,11)) %>%
  select(id, outcome, pvalue) 
dfpvalue2 <- dfpvalue1 %>%
  pivot_wider(names_from = "outcome", values_from = "pvalue")
dfpvalue1 <- dfpvalue1 %>%
  pivot_wider(names_from = "id", values_from = "pvalue", "names_prefix" = "id") %>%
  mutate_at(vars(id4, id11), function(x) paste0("[", format(round(x, 2), nsmall = 2), "]")) %>%
  mutate_at(vars(id4, id11), function(x) ifelse(grepl("NA", x), "", x))

#Create latex table
dftemp <- data.frame(replicate(40, sample(20)))
lm1 <- lm(X1~X2+X3-1, data = dftemp)
covlabels <- rep("Synth DiD", 2)
out <- stargazer(lm1, lm1, lm1, lm1, lm1, lm1, lm1, lm1, 
                 title = "Association between opening an overdose prevention center and public safety, Synthetic difference-in-differences",
                 label = "TableSynthsafety",
                 column.sep.width = "-1pt",
                 font.size = "small",
                 no.space = TRUE,
                 coef = with(dfdiff, list(crime_violentall, crime_property, arrest_weapon, arrest_drugs, 
                                          summons, call_crime, call_medic)),
                 p = with(dfpvalue2, list(crime_violentall, crime_property, arrest_weapon, arrest_drugs, 
                                          summons, call_crime, call_medic)),
                 digits = 1,
                 covariate.labels = covlabels,
                 star.char = c("*", "**", "***"),
                 star.cutoffs = c(.05, .01, .001),
                 report = "vc*",
                 omit.stat = c("all"),
                 dep.var.caption  = "caption placeholder",
                 dep.var.labels.include = FALSE,
                 column.labels = c("violentall", "property", "weapon", "drugs", "summons",
                                   "crime", "medic", "qol"),
                 model.numbers = TRUE,
                 add.lines = list(c("", dfpvalue1$id4),
                                  c("", dfpvalue1$id11),
                                  c("Post-MPE", dfpost$id4),
                                  c("Pre-MPE", dfpre$id4),
                                  c("Mean", dfmean$id4),
                                  c("RMSPE", dfrmspe$id4), 
                                  c("Post-MPE", dfpost$id11),
                                  c("Pre-MPE", dfpre$id11),
                                  c("Mean", dfmean$id11),
                                  c("RMSPE", dfrmspe$id11)),
                 notes = "placeholder",
                 notes.append = FALSE,
                 header = FALSE,
                 #type = "text")
                 type = "latex")
notes <- "\\\\multicolumn{9}{l} {\\\\parbox[t]{14.7cm}{ \\\\scriptsize Notes: 
Synthetic difference-in-differences estimates of the association between opening an
overdose prevention center and public safety in the immediate vicinity (a single hexagon 
around the site), computed as the mean difference between the observed and synthetic 
estimate before and after the intervention period. Implied p-values are in brackets. 
The p-value was estimated as the rank of the intervention effect divided by the number 
of donors, excluding those donors with a pre-intervention Root Mean Square Prediction 
Error (RMSPE) four times larger than the treated unit. The donor pool uses the 250 
hexagons with the most index crimes (excluding theft) among the seven (10\\\\%) precincts 
with the highest index crime rates (excluding theft). The data was aggregated to the 
bimonthly-year-hexagon level; otherwise, the data becomes too noisy to precisely predict 
the synthetic unit. Violent crimes include murder, robbery, and aggravated and simple assault. 
Property crimes include burglary, theft, and motor vehicle theft. Weapons refer to 
criminal possession of a weapon. Drugs mean the sale or possession of dangerous drugs. 
Crime 911 calls refer to those made to law enforcement where there was a possible crime 
in-progress or one has been committed. Medical calls include those needing an ambulance. 
Nuisance calls include 911 calls for trespass and 311 calls about homelessness 
(assisting a homeless person, encampment, and homeless street condition) and disorder 
(seeing a rodent, graffiti, dirty and unsanitary conditions, drug and drinking activity, 
urinating in public, and those 311 calls under the New York Police Department jurisdiction 
such as an abandoned vehicle and noise complaint). Panel A examines the 
impact on the East Harlem overdose prevention center (126th St). Panel B inspects the effects on 
the Washington Heights overdose prevention center (180th St). The bottom rows exhibit the 
post-intervention Mean Prediction Error (MPE), calculated as the mean difference between 
the observed and synthetic estimate in the post-intervention period. The second row is the 
pre-intervention MPE. The third and fourth rows are the post-intervention synthetic estimated 
mean and the RMSPE. Outcomes not achieving a good pre-intervention fit are not shown in the table.
$^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001. }} \\\\"
out <- gsub(pattern = "\\\\textit\\{Note.*", replacement = notes, x = out)
out <- gsub(pattern = "p = (0.[0-9]+)", replacement = "[\\1]", x = out) 
out <- gsub(pattern = " & \\\\multicolumn\\{8\\}\\{c\\}\\{caption placeholder.* ", replacement = "", x = out)
out <- gsub(pattern = "\\\\cline.*", replacement = " ", x = out)
out <- gsub(pattern = "\\\\\\\\\\[\\-1\\.8ex\\]", replacement = " ", x = out)
outnew <- character(length = length(out))
outnew <- out[1:9]
outnew[10] <- "& \\multicolumn{2}{c}{Crime} & \\multicolumn{3}{c}{Law enforcement} & \\multicolumn{3}{c}{Calls for service} 
               \\\\ \\cmidrule(lr){2-3}  \\cmidrule(lr){4-6} \\cmidrule(lr){7-9} "
outnew[11] <- "& Violent & Property & \\thead{Weapons \\\\ arrests} & \\thead{Drug \\\\ arrests} 
                & \\thead{Criminal \\\\ summons} & \\thead{Crime \\\\ 911 calls} 
                & \\thead{Medical \\\\ 911 calls} & \\thead{Nuisance \\\\ calls}  \\\\"
outnew[12:13] <- out[12:13]
outnew[14] <- "\\multicolumn{8}{l}{\\footnotesize \\textit{ A. East Harlem site (126th St) }} \\\\"
outnew[15] <- out[14]
outnew[16] <- out[17]
outnew[17] <- "\\hline"
outnew[18:21] <- out[19:22]
outnew[22] <- "\\hline"
outnew[23] <- "\\hline"
outnew[24] <- "\\multicolumn{8}{l}{\\footnotesize \\textit{ B. Washington Heights site (180th St)}} \\\\"
outnew[25] <- out[15]
outnew[26] <- out[18]
outnew[27] <- "\\hline"
outnew[28:36] <- out[23:31]
writeLines(outnew, file("../Tables-Figures/TableSynthsafety.tex"))
close(file("../Tables-Figures/TableSynthsafety.tex"))
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions






######################################################################################-
## Table: Summary statistics NYC, high-crime city, and single hexagon area ###########
######################################################################################-
#Import data and create time dummy variables for the event study and individual time trends
#Removed 2018 because it is a noisy year
df <- fread("../Data/Crime month-hexagon city data 2018-2022.csv", integer64 = "character") %>%
  filter(year %in% 2019:2022) %>%
  filter(hexsample == 1 | is.na(hexsample)) %>%
  mutate(treated = ifelse(site %in% "OnPoint NYC", 1, 0), 
         datenum = year + (month - 1) /12, 
         post = ifelse(datenum >= 2021.91, 1, 0))
#Identifying the 250 unsafest hexagons in the top 10 unsafe precincts (excluding the treated ones and Times Squares)
vids1 <- df %>%
  filter(post == 0) %>%
  group_by(precinct, treated, rank2, id) %>%
  summarise(crime_index2 = sum(crime_index - crime_theft)) %>%
  ungroup() %>%
  filter(!precinct %in% c(14,25,34)) %>%
  filter(rank2 %in% 1:9) %>%
  mutate(rankhex2 = min_rank(-crime_index2)) %>%
  filter(rankhex2 <= 250) %>%
  pull(id)
#Identifying the 20 hexagons with most pre-intervention drug arrests
vids2 <- df %>%
  filter(post == 0) %>%
  group_by(precinct, treated, rank2, id) %>%
  summarise(arrest_drugs = sum(arrest_drugs)) %>%
  ungroup() %>%
  filter(!precinct %in% c(14,25,34)) %>%
  mutate(rankhex2 = min_rank(-arrest_drugs)) %>%
  filter(rankhex2 <= 20) %>%
  pull(id)
df <- df %>% 
  mutate(highcrime = ifelse(id %in% vids1, 1, 0), 
         highdrugs = ifelse(id %in% vids2, 1, 0)) %>%
  filter(post == 0)
vnames <- df %>% 
  select(crime_index, crime_violentall, crime_murder, crime_robbery,  crime_assault,
         crime_lowassault, crime_property, crime_burglary, crime_theft, crime_mvtheft, 
         arrest_weapon, arrest_drugs, summons, call_crime, call_assault, call_trespass, 
         call_medic, call311_substance, call311_dirty, call311_abandoned, call311_noise,
         call311_homeless) %>%
  names()

#Create main data frame
dfstat <- data.frame("var" = vnames, stringsAsFactors = FALSE)

#Pre-treatment mean and standard deviation of NYC sample
df %>% distinct(id) %>% nrow(.)
vstat <- df %>% 
  filter(treated == 0) %>%
  select(crime_index, crime_violentall, crime_murder, crime_robbery, crime_assault, 
         crime_lowassault, crime_property, crime_burglary, crime_theft, crime_mvtheft, 
         arrest_weapon, arrest_drugs, summons, call_crime, call_assault, call_trespass, 
         call_medic, call311_substance, call311_dirty, call311_abandoned, call311_noise, 
         call311_homeless) %>%
  summarise_all(list(~mean(., na.rm = TRUE), ~sd(., na.rm = TRUE))) %>%
  ungroup()
#Extract mean and std dev
vmean <- vstat %>%
  select(ends_with("_mean")) %>%
  rename_all(function (x) str_replace(x, "_mean", "")) %>%
  unlist(., use.names = TRUE)
vsd <- vstat %>%
  select(ends_with("_sd")) %>%
  rename_all(function (x) str_replace(x, "_sd", "")) %>%
  unlist(., use.names = TRUE)
dfstat$meannyc <- vmean
dfstat$sdnyc <- vsd

#Pre-treatment mean and standard deviation of high crime sample
vstat <- df %>% 
  filter(highcrime == 1) %>%
  select(crime_index, crime_violentall, crime_murder, crime_robbery, crime_assault, 
         crime_lowassault, crime_property, crime_burglary, crime_theft, crime_mvtheft, 
         arrest_weapon, arrest_drugs, summons, call_crime, call_assault, call_trespass, 
         call_medic, call311_substance, call311_dirty, call311_abandoned, call311_noise, 
         call311_homeless) %>%
  summarise_all(list(~mean(., na.rm = TRUE), ~sd(., na.rm = TRUE))) %>%
  ungroup()
#Extract mean and std dev
vmean <- vstat %>%
  select(ends_with("_mean")) %>%
  rename_all(function (x) str_replace(x, "_mean", "")) %>%
  unlist(., use.names = TRUE)
vsd <- vstat %>%
  select(ends_with("_sd")) %>%
  rename_all(function (x) str_replace(x, "_sd", "")) %>%
  unlist(., use.names = TRUE)
dfstat$meanhigh <- vmean
dfstat$sdhigh <- vsd

#Pre-treatment mean and standard deviation of high drug arrests sample
vstat <- df %>% 
  filter(highdrugs == 1) %>%
  select(crime_index, crime_violentall, crime_murder, crime_robbery, crime_assault, 
         crime_lowassault, crime_property, crime_burglary, crime_theft, crime_mvtheft, 
         arrest_weapon, arrest_drugs, summons, call_crime, call_assault, call_trespass, 
         call_medic, call311_substance, call311_dirty, call311_abandoned, call311_noise, 
         call311_homeless) %>%
  summarise_all(list(~mean(., na.rm = TRUE), ~sd(., na.rm = TRUE))) %>%
  ungroup()
#Extract mean and std dev
vmean <- vstat %>%
  select(ends_with("_mean")) %>%
  rename_all(function (x) str_replace(x, "_mean", "")) %>%
  unlist(., use.names = TRUE)
vsd <- vstat %>%
  select(ends_with("_sd")) %>%
  rename_all(function (x) str_replace(x, "_sd", "")) %>%
  unlist(., use.names = TRUE)
dfstat$meandrugs <- vmean
dfstat$sddrugs <- vsd

#Pre-treatment mean and standard deviation of treatment
vstat <- df %>% 
  filter(treated == 1) %>%
  select(crime_index, crime_violentall, crime_murder, crime_robbery, crime_assault, 
         crime_lowassault, crime_property, crime_burglary, crime_theft, crime_mvtheft, 
         arrest_weapon, arrest_drugs, summons, call_crime, call_assault, call_trespass, 
         call_medic, call311_substance, call311_dirty, call311_abandoned, call311_noise, 
         call311_homeless) %>%
  summarise_all(list(~mean(., na.rm = TRUE), ~sd(., na.rm = TRUE))) %>%
  ungroup()
#Extract mean and std dev
vmean <- vstat %>%
  select(ends_with("_mean")) %>%
  rename_all(function (x) str_replace(x, "_mean", "")) %>%
  unlist(., use.names = TRUE)
vsd <- vstat %>%
  select(ends_with("_sd")) %>%
  rename_all(function (x) str_replace(x, "_sd", "")) %>%
  unlist(., use.names = TRUE)
dfstat$meantreat <- vmean
dfstat$sdtreat <- vsd

#Rearrange rows
dfstat <- dfstat %>%
  bind_rows(data.frame("var" = rep(NA, 4))) %>%
  slice(23, 1:10, 24, 11:13, 25, 14:17, 26, 18:22)

#Export table
dftemp <- data.frame(replicate(50, sample(20)))
lm1 <- lm(as.formula(paste0("X1 ~ ", paste0("X", 2:27, collapse = "+"), "-1")), data = dftemp)
covlabels <- c("Crimes", "offset1 Index", "offset1 Violent", "offset2 Murder", "offset2 Robbery", 
               "offset2 Aggravated assault", "offset2 Simple Assault", "offset1 Property", 
               "offset2 Burglary", "offset2 Theft", "offset2 Motor vehicle theft", 
               "Law enforcement", "offset1 Weapons arrests", "offset1 Drug arrests", 
               "offset1 Criminal summons", "911 calls", "offset1 Crime", "offset1 Assault", 
               "offset1 Trespass", "offset1 Medical", "311 calls", "offset1 Drug", 
               "offset1 Unsanitary conditions", "offset1 Abandoned vehicle", 
               "offset1 Noise complaint", "offset1 Homeless")
out <- stargazer(lm1, lm1, lm1, lm1,
                 title = "Descriptive statistics by sample selection, hexagon monthly level data",
                 label = "TableDescStatsSamples",
                 column.sep.width = "5pt",
                 font.size = "small",
                 no.space = TRUE,
                 coef = with(dfstat, list(meannyc, meanhigh, meandrugs, meantreat)),
                 se = with(dfstat, list(sdnyc, sdhigh, sddrugs, sdtreat)),
                 digits = 1,
                 covariate.labels = covlabels,
                 report = "vcs",
                 omit.stat = c("all"),
                 dep.var.caption  = "caption placeholder",
                 dep.var.labels = c("Mean count (std. dev.)"),
                 single.row = TRUE,
                 model.numbers = TRUE,
                 notes = "placeholder",
                 notes.append = FALSE,
                 header = FALSE,
                 #type = "text")
                 type = "latex")
notes <- "\\\\multicolumn{5}{l} {\\\\parbox[t]{14.0cm}{ \\\\scriptsize Notes: 
Hexagon monthly-year level pre-intervention (2019M1-2021M11) mean count and standard 
deviation from all New York City, the high-crime sample, and the intervention hexagon. 
The New York City sample includes all areas across the city. The high-crime sample 
was built by first identifying the top seven (10\\%) precincts outside of the intervention 
areas that had the highest number of index crimes (excluding larceny) per 100,000 residents, 
then, selecting the 250 hexagons with the highest volume of crimes among these seven 
precincts. The high drug arrests sample uses the 20 hexagons with the most pre-intervention 
mean drug arrests excluding areas in the police precincts where the overdose prevention
centers are located. The intervention hexagon surrounds the overdose prevention center.
Index crimes include the six UCR part I crimes (murder, robbery, aggravated assault, 
burglary, theft, and motor vehicle). Violent crimes include murder, robbery, and 
aggravated and simple assault. Property crimes include burglary, theft, and 
motor vehicle theft. Weapons possession arrests refer to criminal possession of a weapon, 
Drug possession arrests mean sale or possession of dangerous drugs. Crime 911 calls 
include those in which there was a possible crime in-progress or one has been committed. 
Assault and trespass 911 calls explicitly mention these offenses in the call. 
Medical calls include those needing an ambulance. Drug related calls include drug and 
drinking activity and loose syringe calls. Unsanitary conditions comprise calls related 
to seeing a rodent, graffiti, dirty and unsanitary conditions, and urinating in public. 
Abandoned vehicle and noise complaints are calls handled by the New York Police Department.
Homeless calls include those related to assisting a homeless person, encampment, and 
homeless street condition. 
}} \\\\"
out <- gsub(pattern = "\\\\textit\\{Note.*", replacement = notes, x = out)
out <- gsub(pattern = "offset1", replacement = "\\\\hspace{2mm}", x = out)
out <- gsub(pattern = "offset2", replacement = "\\\\hspace{6mm}", x = out)
out <- gsub(pattern = "offset3", replacement = "\\\\hspace{8mm}", x = out)
out <- gsub(pattern = " & \\\\multicolumn\\{4\\}\\{c\\}\\{caption placeholder.* ", replacement = "", x = out)
out <- gsub(pattern = "\\\\cline.*", replacement = " ", x = out)
out <- gsub(pattern = "\\\\\\\\\\[\\-1\\.8ex\\]", replacement = " ", x = out)
out[12] <- "&\\thead{New York \\\\ City \\\\ (n = 8,752)} & \\thead{High-crime  \\\\ neighborhoods \\\\ (n = 250)} & 
            \\thead{High-drug arrests  \\\\ neighborhoods \\\\ (n = 20)} & 
            \\thead{Neighborhoods \\\\ with OPCs \\\\ (n = 2)} \\\\"
out[42] <- ""
writeLines(out, file("../Tables-Figures/TableDescStatsSamples.tex"))
close(file("../Tables-Figures/TableDescStatsSamples.tex"))
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions








######################################################################################-
## Table: Sociodemographics NYC, control and comparison groups #######################
######################################################################################-
#Import data from censusreporter.org
df <- read_xlsx("../Data/NYC_zips.xlsx") %>%
  rename_all(tolower) %>%
  rename_all(function (x) str_replace_all(x, "%| |'", "")) %>%
  mutate(group = ifelse(zip %in% c(10033, 10035), "treated", "control"), 
         other = native + islander + other + two, 
         pc_income = pc_income / 1000, 
         median_hh_income = median_hh_income/1000) %>%
  group_by(group) %>%
  summarise_at(vars(white:foreignborn), funs(weighted.mean(., pop))) %>%
  select(group:black, hispanic, asian, other, pc_income:foreignborn) %>%
  pivot_longer(cols = white:foreignborn, names_to = "variable") %>%
  pivot_wider(names_from = group, values_from = value) %>%
  bind_rows(data.frame(variable = rep(NA, 3))) %>%
  slice(11, 1:5, 12, 6:8, 13, 9:10)

#Export table
dftemp <- data.frame(replicate(50, sample(20)))
lm1 <- lm(as.formula(paste0("X1 ~ ", paste0("X", 2:14, collapse = "+"), "-1")), data = dftemp)
covlabels <- c("Race and ethnicity", "offset1 White (\\%)", "offset1 Black (\\%)", "offset1 Hispanic (\\%)",
               "offset1 Asian (\\%)", "offset1 Other (\\%)", "Income levels", "offset1 Per capita (thousand dollars)", 
               "offset1 Median household (thousand dollars)", "offset1 Below poverty line (\\%)", 
               "Other sociodemographics", "offset1 Bacherlor's degree or higher (\\%)", 
               "offset1 Foreign-born population (\\%)")
out <- stargazer(lm1, lm1, 
                 title = "Neighborhood sociodemographics by intervention status, five year estimates",
                 label = "TableDescStatsNeigh",
                 column.sep.width = "1pt",
                 font.size = "small",
                 no.space = TRUE,
                 coef = with(df, list(treated, control)),
                 digits = 1,
                 covariate.labels = covlabels,
                 report = "vc",
                 omit.stat = c("all"),
                 dep.var.caption  = "caption placeholder",
                 dep.var.labels = c("XXX"),
                 column.labels = c("Neighborhoods with OPCs", "Neighborhoods with SSP"),
                 single.row = TRUE,
                 model.numbers = FALSE,
                 notes = "placeholder",
                 notes.append = FALSE,
                 header = FALSE,
                 #type = "text")
                 type = "latex")
notes <- "\\\\multicolumn{3}{l} {\\\\parbox[t]{11.0cm}{ \\\\scriptsize Notes: 
The sociodemographics come from population weighted zipcode level data where the 
OPCs (10035 and 10033) and SSPs (10002, 10009, 10010, 10018, 10027, 10302, 10451, 
10459, 10301, 11207, 11208, 11221, 11212, 11217, 11412, 11435) are located
Estimates based on the U.S. Census Bureau (2021). American Community 
Survey 5-year estimates. Retrieved from Census Reporter. }} \\\\"
out <- gsub(pattern = "\\\\textit\\{Note.*", replacement = notes, x = out)
out <- gsub(pattern = "offset1", replacement = "\\\\hspace{2mm}", x = out)
out <- gsub(pattern = "offset2", replacement = "\\\\hspace{6mm}", x = out)
out <- gsub(pattern = "offset3", replacement = "\\\\hspace{8mm}", x = out)
out <- gsub(pattern = " & \\\\multicolumn\\{2\\}\\{c\\}\\{caption placeholder.* ", replacement = "", x = out)
out <- gsub(pattern = "\\\\cline.*", replacement = " ", x = out)
out <- gsub(pattern = "\\\\\\\\\\[\\-1\\.8ex\\]", replacement = " ", x = out)
out[11] <- ""
out[12] <- " & \\thead{Neighborhoods \\\\ with OPCs} & \\thead{Neighborhoods \\\\ with SSP} \\\\ "
out[29] <- ""
writeLines(out, file("../Tables-Figures/TableDescStatsNeigh.tex"))
close(file("../Tables-Figures/TableDescStatsNeigh.tex"))
#rm(list = setdiff(ls(), lsf.str())); gc() #remove everything except the custom functions

