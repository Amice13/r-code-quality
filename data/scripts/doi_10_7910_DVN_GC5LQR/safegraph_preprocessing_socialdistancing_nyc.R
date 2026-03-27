## Documentation:
## This data was derived from Safegraph weekly social distancing metrics for the raw weekly files from Jan 1, 2019 to the 38th week of 2020
## The raw data is no longer publicly available, however for transparency we have provided the original code we used to clean the data, 
## as well as the cleaned data 
## see: https://docs.safegraph.com/docs/social-distancing-metrics for details

### load packages
library(tidyverse)
library(tidycensus)
library(foreach)
library(doMC)

rootdir <- getwd()

setwd(paste(rootdir, "/Data/safegraph/socialdistancing2020", sep = ""))

yearfolders <- list.files()

nyc_counties <- c("36005", "36047", "36061", "36081", "36085")

registerDoMC(19)

### loop through nested year, month, day folders, aggregate variables to tract
for (k in 1:length(yearfolders)) {
    setwd(yearfolders[k])
    monthfolders <- list.files()
    for (i in 1:length(monthfolders)) {
        setwd(monthfolders[i])
        dayfolders <- list.files()
        foreach (j = 1:length(dayfolders)) %dopar% {
            setwd(dayfolders[j])
            datafile <- list.files()
            name <- substr(datafile[1], 1, 10)
            df.1 <- read_csv(datafile[1])
            df.1 <- mutate(df.1, tract = substr(origin_census_block_group, 1, 11),
                           county = substr(origin_census_block_group, 1, 5)) %>%
                    filter(county %in% nyc_counties)
            df.tract <- df.1 %>% 
                group_by(tract) %>%
                summarize("devices" = sum(device_count, na.rm = TRUE),
                        "sum_completely_home" = sum(completely_home_device_count, na.rm = TRUE),
                        "sum_part_time" = sum(part_time_work_behavior_devices, na.rm = TRUE),
                        "sum_full_time" = sum(full_time_work_behavior_devices, na.rm = TRUE),
                        "sum_delivery" = sum(delivery_behavior_devices, na.rm = TRUE),
                        "wtd_median_home_time" = weighted.mean(median_home_dwell_time, device_count),
                        "wtd_median_distance_travelled" = weighted.mean(distance_traveled_from_home, device_count)) %>%
                mutate("date" = name,
                        "perc_completely_home" = sum_completely_home/devices,
                        "perc_part_time" = sum_part_time/devices,
                        "perc_full_time" = sum_full_time/devices,
                       "perc_delivery" = sum_delivery/devices,)
            cleanname <- paste(rootdir, "/safegraph/clean_socialdist/nycdaily/", name, ".csv", sep = "")
            write_csv(df.tract, path = cleanname)
            print(paste("finished ", name))
            rm(df.tract, df.1, datafile, cleanname, name)
            setwd("..")
        }
        rm(dayfolders)
        setwd("..")
    }
    rm(monthfolders)
    setwd("..")
}

### bind all files into a single file with weekly data - variables reflect weekly means
setwd(paste(rootdir, "/Data/safegraph/clean_socialdist/nycdaily/", sep = ""))
files <- list.files()
tables <- lapply(files, read.csv, header = TRUE)
df.all <- do.call(rbind, tables)


df.week <- df.all %>%
  mutate("year" = substr(date, 1,4), 
         "week" = strftime(date, format = "%V")) %>%
  group_by(tract, year, week) %>%
  summarize(avg_devices = mean(devices, na.rm = TRUE),
            avg_completely_home = weighted.mean(perc_completely_home, devices, na.rm = TRUE),
            avg_part_time = weighted.mean(perc_part_time, devices, na.rm = TRUE),
            avg_full_time = weighted.mean(perc_full_time, devices, na.rm = TRUE),
            avg_delivery = weighted.mean(perc_delivery, devices, na.rm = TRUE),
            wtd_median_home_time = weighted.mean(wtd_median_home_time, devices, na.rm = TRUE),
            wtd_median_dist_travelled = weighted.mean(wtd_median_dist_travelled, devices, na.rm = TRUE))

write_csv(df.week, paste(rootdir, "/Data/safegraph/clean_socialdist/nycdaily/social_dist_by_week_nyc.csv", sep = ""))
