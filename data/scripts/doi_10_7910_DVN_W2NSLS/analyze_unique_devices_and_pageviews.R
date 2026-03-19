library(plyr)
library(tidyverse)
library(jsonlite)
library(here)

setwd(here())
setwd("./wikipedia")

rel_dates <- list()
rel_dates[["zh"]] <- c("2020-01-24", "2020-03-13")

apicounts <- stream_in(file("data/agg_files/zh_unique_devices_from_api_monthly.json"))


with(
    apicounts %>%
   mutate(date = as.Date(timestamp, format="%Y%m%d")),
   data.frame(
       before = mean(devices[date >= "2019-12-01" & date < "2020-01-01"]),
       before2 = mean(devices[date >= "2019-07-01" & date < "2020-01-01"]),
       during_lockdown2 = mean(devices[date %in% as.Date(c("2020-01-01", "2020-02-01", "2020-03-01"))])
   ) %>%
   mutate(
       difference_monthly = during_lockdown2 - before, # Jan to March v december
       difference2_monthly = during_lockdown2 - before2 # Jan to March v last half
   )
)


apicounts <- stream_in(file("data/agg_files/zh_unique_devices_from_api.json"))

with(
    apicounts %>%
   mutate(date = as.Date(timestamp, format="%Y%m%d")),
   data.frame(
       before = mean(devices[date >= "2019-12-01" & date < "2020-01-01"]),
       before2 = mean(devices[date >= "2019-07-01" & date < "2020-01-01"]),
       during_lockdown = mean(devices[date >= rel_dates[["zh"]][1] & date < rel_dates[["zh"]][2]])
   ) %>%
   mutate(
       difference = during_lockdown - before,
       difference2 = during_lockdown - before2
   )
)
