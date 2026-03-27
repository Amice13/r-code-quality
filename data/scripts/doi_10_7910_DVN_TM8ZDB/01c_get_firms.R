setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../data_raw")
library(sf)
library(sp)
library(tidyverse)
library(data.table)
library(readstata13)
#library(stringmatch)


## Geocoded firms
load("firms/geocoded_firms_12_13_20.RData")
dat3 = dat3 %>% filter(!is.na(lon) & !is.na(lat)) %>%
  filter(filename == "2012") %>%
  mutate(obj = firm) %>%
  select(lon, lat, obj, state) %>%
  mutate(type = "firm")


## PAC data
pac = read_csv("firms/pacs.csv") %>% select(comnam, bf_firm_has_pac)
pac = dat3 %>% filter(obj %in% pac$comnam) %>%
  mutate(type = "pacfirm")

## geocoded walmarts
wal = foreign::read.dbf("firms/walmart_nhgis.dbf")
wal = wal %>% mutate(OPENDATE = as.Date(OPENDATE)) %>%
  filter(OPENDATE < "2013-01-01") %>% # all of them
  mutate(lon = X,
         lat = Y,
         obj = paste0("walmart_", storenum),
         type = "walmart",
         state = STRSTATE ) %>%
  select(lon, lat, obj,state, type)

## geocoded hospitals
hosp = read.csv("firms/us_hospital_locations.csv") %>%
  mutate(lon = LONGITUDE,
         lat = LATITUDE,
         obj = NAME,
         state = STATE,
         type = "hospital") %>%
  filter(STATUS == "OPEN",
         TRAUMA != "NOT AVAILABLE" | BEDS > 100) %>%
  select(lon, lat, obj, state, type)

## geocoded airports
airp = read.csv("firms/Airports.csv") %>%
  mutate(lon = `ï..X`,
         lat = Y,
         obj = Loc_Id,
         state = State_Post_Office_Code,
         type = "airport") %>%
  filter(Fac_Type == "AIRPORT",
         Fac_Use == "PU",
         Status_Code == "O",
         !is.na(Commercial_Ops)) %>%
  arrange(desc(Commercial_Ops)) %>%
  select(lon, lat, obj, state, type)


## geocoded landfills
lfill = readxl::read_xlsx("firms/landfilllmopdata.xlsx") %>%
  mutate(lon = Longitude,
         lat = Latitude,
         obj = `GHGRP ID`,
         state = State,
         type = "landfill") %>%
 #filter(`Current Landfill Status` == "Closed") %>%
  select(lon, lat, obj, state, type)

dat3 = rbind(dat3, pac, hosp, wal, airp)


firms1 = dat3
save(firms1, file = "../data_clean/all_firms.RData")
