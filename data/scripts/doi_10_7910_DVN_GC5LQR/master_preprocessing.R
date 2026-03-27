### load data and libraries
library(tidyverse)
library(haven)
library(sf)

rootdir <- dirname(getwd())

#ACS
## first run ACS_race_preprocessing_tract.R
setwd(paste(rootdir, "/Data/ACS/", sep =""))

df.acs_full <- read_csv("acs2018_nyc_tract_clean_byrace.csv") %>%
       mutate(age = if_else(age=="under17", age,
                            if_else(age %in% c("age65_74", "age75up"), "age65up", "age18-64"))) %>%
        group_by(tract, race, age) %>%
        summarize(population = sum(population, na.rm = TRUE)) %>%
        mutate(strat_id = paste(tract, substr(race,1,1), substr(age, nchar(age) - 1, nchar(age)), sep = "_")) %>%
        select(strat_id, population)

df.acs <- read_csv("acs2018_nyc_tract_clean.csv") %>% 
        select(!c("...1"))

df.acs_race <- read_csv("acs2018_nyc_tract_clean_byrace.csv") %>%
        select(!c(age, pop_perc, strat_id, ...1, population)) %>%
        unique()
df.acs_race <- read_csv("acs2018_nyc_tract_clean_byrace.csv") %>%
        group_by(tract, race) %>%
        summarize(population = sum(population, na.rm = TRUE)) %>%
        merge(df.acs_race, by = c("tract", "race")) %>%
        mutate(race = if_else(race=="white", "White",
                                       if_else(race=="black", "Black/African American", 
                                               if_else(race=="asian", "Asian/Pacific Islander", 
                                                       if_else(race=="hispanic", "Hispanic/Latino", race))))) %>%
        rename(tract_id = tract,
               raceethnicity = race)

#Safegraph
## first run safegraph_preprocessing_socialdistancing_nyc.R on Yale cluster
## creating 2 sets of variables - averages for first 8 weeks of year as pre-covid baseline and averages for weeks 13-20 as measure of social distancing
setwd(paste(rootdir, "/Data/safegraph/clean_socialdist/", sep = ""))
df.sgraph <- read_csv("social_dist_by_week_nyc.csv")

df.sgraph1 <- df.sgraph %>%
        mutate(period = if_else(as.integer(week)<9, "pre",
                                if_else(as.integer(week)>12 & as.integer(week)<30, "post", "na"))) %>%
        filter(year==2020 & period!="na") %>%
        group_by(period, tract) %>%
        summarize(avg_devices = mean(avg_devices),
                  avg_completely_home = mean(avg_completely_home),
                  avg_full_time = mean(avg_full_time),
                  avg_part_time = mean(avg_part_time),
                  avg_delivery = mean(avg_delivery),
                  home_time = mean(wtd_median_home_time),
                  dist_travelled = mean(wtd_median_dist_travelled)) %>% ungroup() %>%
        pivot_wider(names_from = period, values_from = avg_devices:dist_travelled) %>%
        mutate(device_chg = avg_devices_post/avg_devices_pre - 1)

df.sgraph2 <- df.sgraph %>%
        filter(year==2020 & week %in% c(10, 20)) %>%
        group_by(week, tract) %>%
        summarize(avg_devices = mean(avg_devices)) %>% 
        ungroup() %>%
        pivot_wider(names_from = week, values_from = avg_devices) %>%
        mutate(device_chg1020 = `20`/`10` - 1)

df.sgraph <- merge(df.sgraph1, df.sgraph2[,c("tract", "device_chg1020")], by = "tract")

#DHMH
### Covid outcomes data provided by NYC DOHMH - contact: BCD_reportable_data@health.nyc.gov

# setwd(paste(rootdir, "/Data/DOHMH/", sep = ""))
# df.dhmh <- read_csv("tracthosps_stratified.csv", n_max = )
# 
# df.dhmh <- df.dhmh %>%
#         mutate(agegroup = if_else(agegroup %in% c("75+", "65-74"), "65up", 
#                                   if_else(agegroup == "0-17", agegroup, "18-64")),
#                strat_id = paste(tract_id, tolower(substr(raceethnicity, 1, 1)), substr(agegroup, nchar(agegroup)-1, nchar(agegroup)), sep = "_")) %>%
#         select(!c(age_id, race_id, minors:White, `_merge`:frac_hosps)) 
# 
# df.dhmh_race <- df.dhmh %>%
#         group_by(tract_id, raceethnicity) %>%
#         summarize(stratified_tests = sum(stratified_tests, na.rm = TRUE),
#                   stratified_personstested = sum(stratified_personstested, na.rm = TRUE),
#                   stratified_cases = sum(stratified_cases, na.rm = TRUE),
#                   stratified_hospitalized = sum(stratified_hospitalized, na.rm = TRUE),
#                   stratified_totaldeaths = sum(stratified_totaldeaths, na.rm = TRUE))
# 
# df.dhmh_str <- df.dhmh %>%
#         group_by(tract_id, agegroup, raceethnicity, strat_id) %>%
#         summarize(stratified_tests = sum(stratified_tests, na.rm = TRUE),
#                   stratified_personstested = sum(stratified_personstested, na.rm = TRUE),
#                   stratified_cases = sum(stratified_cases, na.rm = TRUE),
#                   stratified_hospitalized = sum(stratified_hospitalized, na.rm = TRUE),
#                   stratified_totaldeaths = sum(stratified_totaldeaths, na.rm = TRUE))
# 
# df.dhmh_age <- df.dhmh %>%
#         group_by(tract_id, agegroup) %>%
#         summarize(stratified_tests = sum(stratified_tests, na.rm = TRUE),
#                   stratified_personstested = sum(stratified_personstested, na.rm = TRUE),
#                   stratified_cases = sum(stratified_cases, na.rm = TRUE),
#                   stratified_hospitalized = sum(stratified_hospitalized, na.rm = TRUE),
#                   stratified_totaldeaths = sum(stratified_totaldeaths, na.rm = TRUE))
# 
# df.dhmh_agg <- unique(df.dhmh[,c("tract_id", "cumulative_tests", "cumulative_persons_tested", 
#                           "cumulative_cases", "cumulative_hospitalized", "cumulative_total_deaths")]) 

### pollution and roads
# Long term averages at tract centroids calculated from NYCCAS monitor level data
# Available from: https://www.nyc.gov/site/doh/data/data-sets/air-quality-nyc-community-air-survey.page
setwd(paste(rootdir, "/Data", sep = ""))
df.pollution <- read_csv("pollution.csv")

## First run highway_processing.R and then wind_preprocessing_rings.R
df.wind <- read_csv("downwind_vars_rings.csv")

df.wind <- df.wind %>%
        mutate(downwind_5_r = downwind_5_r/station_hrs,
               downwind_3_r = downwind_3_r/station_hrs,
               downwind_2_r = downwind_2_r/station_hrs,
               downwind_1_r = downwind_1_r/station_hrs,
               downwind_0_r = downwind_0_r/station_hrs,
               downwind_5_rr = downwind_5_rr/station_hrs,
               downwind_3_rr = downwind_3_rr/station_hrs,
               downwind_2_rr = downwind_2_rr/station_hrs,
               downwind_1_rr = downwind_1_rr/station_hrs,
               downwind_0_rr = downwind_0_rr/station_hrs,
               downwind_5_r_rush = downwind_5_r_rush/station_hrs_rush,
               downwind_3_r_rush = downwind_3_r_rush/station_hrs_rush,
               downwind_2_r_rush = downwind_2_r_rush/station_hrs_rush,
               downwind_1_r_rush = downwind_1_r_rush/station_hrs_rush,
               downwind_0_r_rush = downwind_0_r_rush/station_hrs_rush,
               downwind_5_rr_rush = downwind_5_rr_rush/station_hrs_rush,
               downwind_3_rr_rush = downwind_3_rr_rush/station_hrs_rush,
               downwind_2_rr_rush = downwind_2_rr_rush/station_hrs_rush,
               downwind_1_rr_rush = downwind_1_rr_rush/station_hrs_rush,
               downwind_0_rr_rush = downwind_0_rr_rush/station_hrs_rush,
               downwind_5_r_nrush = downwind_5_r_nrush/station_hrs_nrush,
               downwind_3_r_nrush = downwind_3_r_nrush/station_hrs_nrush,
               downwind_2_r_nrush = downwind_2_r_nrush/station_hrs_nrush,
               downwind_1_r_nrush = downwind_1_r_nrush/station_hrs_nrush,
               downwind_0_r_nrush = downwind_0_r_nrush/station_hrs_nrush,
               downwind_5_rr_nrush = downwind_5_rr_nrush/station_hrs_nrush,
               downwind_3_rr_nrush = downwind_3_rr_nrush/station_hrs_nrush,
               downwind_2_rr_nrush = downwind_2_rr_nrush/station_hrs_nrush,
               downwind_1_rr_nrush = downwind_1_rr_nrush/station_hrs_nrush,
               downwind_0_rr_nrush = downwind_0_rr_nrush/station_hrs_nrush
         )

df.winddir <- read_csv(paste(rootdir, "/Data/Weather/wind_speed1221.csv", sep = "")) %>%
        select(wspeed_idw, wspeed_closest, boro_ct201)

setwd(paste(rootdir, "/Data/Traffic and Roadways/", sep = ""))
df.roads <- read_csv("tract_roads_min_dist.csv")
        
df.roads <- df.roads %>%
        mutate(census_tract = substr(boro_ct201, 2, 7),
               county = if_else(boro_code=="1", "36061",
                                if_else(boro_code=="2", "36005", 
                                        if_else(boro_code=="3", "36047",
                                                if_else(boro_code=="4", "36081", "36085")))),
               tract_id = paste(county, census_tract, sep = "")) %>%
        select(!c(census_tract, county)) %>%
        filter(is.na(boro_code)==FALSE) %>%
        rename(near_dist = dist2closesthwy)

df.pollution <- merge(df.pollution, df.roads, by = "boro_ct201", all = TRUE)
df.pollution <- merge(df.pollution, df.wind, by = "boro_ct201", all = TRUE)
df.pollution <- merge(df.pollution, df.winddir, by = "boro_ct201", all = TRUE)

#merge

df.master_stratified <- merge(df.acs, df.acs_full, by = "tract", all = TRUE)
df.master_stratified <- merge(df.master_stratified, df.sgraph, by = "tract", all = TRUE)
df.master_stratified <- merge(df.master_stratified, df.pollution, by.x = "tract", by.y = "tract_id", all = TRUE)
#df.master_stratified <- merge(df.dhmh_str, df.master_stratified, by.x = "tract_id", by.y = "tract", all = TRUE)

# if using without covid data, run this line - if running with covid data, skip
df.master_stratified <- rename(df.master_stratified, tract_id = tract)

df.race <- merge(df.acs, df.sgraph, by = "tract", all = TRUE)
df.race <- merge(df.pollution, df.race, by.x = "tract_id", by.y = "tract", all = TRUE)
df.race <- merge(df.race, df.acs_race, by = "tract_id", all = TRUE)
#df.race <- merge(df.dhmh_race, df.race, by = c("tract_id", "raceethnicity"), all = TRUE)

df.age <- merge(df.acs, df.sgraph, by = "tract", all = TRUE)
df.age <- merge(df.pollution, df.age, by.x = "tract_id", by.y = "tract", all = TRUE)
#df.age <- merge(df.dhmh_age, df.age, by = "tract_id", all = TRUE)

#secondary master with one row for each tract (non-stratified)

df.master <- merge(df.acs, df.sgraph, by = "tract", all = TRUE)
#df.master <- merge(df.master, df.dhmh_agg, by.x = "tract", by.y = "tract_id")
df.master <- merge(df.master, df.pollution, by.x = "tract", by.y = "tract_id", all = TRUE)

### geo indicators

lower_man <- c("Battery Park City-Lower Manhattan", "SoHo-TriBeCa-Civic Center-Little Italy", "Chinatown", "Lower East Side")
hudson_yards <- c("East Village", "West Village", "Gramercy", "Stuyvesant Town-Cooper Village", "Hudson Yards-Chelsea-Flatiron-Union Square")
central_park <- c("Murray Hill-Kips Bay", "Turtle Bay-East Midtown", "Midtown-Midtown South", "Clinton")
below110 <- c("Lincoln Square", "Upper West Side", "Upper East Side-Carnegie Hill", "Lenox Hill-Roosevelt Island", "Yorkville", "East Harlem South")
exception_trcts <- c(36061019500, 36061019300, 36061031900, 36061014300)
except_out <- c(36061017402)

df.master_stratified <- mutate(df.master_stratified, lower_manhattan = if_else(ntaname %in% lower_man, 1, 0),
                    hudson_yards_and_below = if_else(ntaname %in% c(lower_man, hudson_yards), 1, 0),
                    below_central_park = if_else(ntaname %in% c(lower_man, hudson_yards, central_park), 1, 0),
                    below_110th = if_else((ntaname %in% c(lower_man, hudson_yards, central_park, below110) | tract_id %in% exception_trcts)
                                          & !(tract_id %in% except_out), 1, 0))

df.race <- mutate(df.race, lower_manhattan = if_else(ntaname %in% lower_man, 1, 0),
                               hudson_yards_and_below = if_else(ntaname %in% c(lower_man, hudson_yards), 1, 0),
                               below_central_park = if_else(ntaname %in% c(lower_man, hudson_yards, central_park), 1, 0),
                               below_110th = if_else((ntaname %in% c(lower_man, hudson_yards, central_park, below110) | tract_id %in% exception_trcts)
                                                     & !(tract_id %in% except_out), 1, 0))

df.age <- mutate(df.age, lower_manhattan = if_else(ntaname %in% lower_man, 1, 0),
                         hudson_yards_and_below = if_else(ntaname %in% c(lower_man, hudson_yards), 1, 0),
                         below_central_park = if_else(ntaname %in% c(lower_man, hudson_yards, central_park), 1, 0),
                         below_110th = if_else((ntaname %in% c(lower_man, hudson_yards, central_park, below110) | tract_id %in% exception_trcts)
                                               & !(tract_id %in% except_out), 1, 0))

df.master <- mutate(df.master, lower_manhattan = if_else(ntaname %in% lower_man, 1, 0),
                    hudson_yards_and_below = if_else(ntaname %in% c(lower_man, hudson_yards), 1, 0),
                    below_central_park = if_else(ntaname %in% c(lower_man, hudson_yards, central_park), 1, 0),
                    below_110th = if_else((ntaname %in% c(lower_man, hudson_yards, central_park, below110) | tract %in% exception_trcts)
                                          & !(tract %in% except_out), 1, 0))

### add pumas

shp.tract <- st_read(paste(rootdir, "/Data/Census Tracts shapefiles/", sep = ""), layer = "geo_export_79d01148-4a56-4331-a800-1e4f76bed5aa")
df.boros <- data.frame(boro_name = c("Manhattan", "Brooklyn", "Staten Island", "Queens", "Bronx"),
                       county = c("36061", "36047", "36085", "36081","36005"))
shp.tract <- merge(shp.tract, df.boros, by = "boro_name")
shp.tract <- mutate(shp.tract, tract = paste(county, ct2010, sep = "")) %>%
        select(tract, puma)
shp.tract$geometry <- NULL

df.master <- merge(shp.tract, df.master, by = "tract", all.y = TRUE)
df.master_stratified <- merge(shp.tract, df.master_stratified, by.x = "tract", by.y = "tract_id", all.y = TRUE)
df.race <- merge(shp.tract, df.race, by.x = "tract", by.y = "tract_id", all.y = TRUE)
df.age <- merge(shp.tract, df.age, by.x = "tract", by.y = "tract_id", all.y = TRUE)

df.master <- mutate(df.master, superpuma = if_else(puma %in% c("3802", "3803", "3804"), "380234",
                                                   if_else(puma %in% c("3805", "3806"), "38056",
                                                           if_else(puma %in% c("3807", "3808"), "38078",
                                                                   if_else(puma %in% c("3809", "3810"), '38019', as.character(puma))))))

df.master_stratified <- mutate(df.master_stratified, superpuma = if_else(puma %in% c("3802", "3803", "3804"), "380234",
                                                                         if_else(puma %in% c("3805", "3806"), "38056",
                                                                                 if_else(puma %in% c("3807", "3808"), "38078",
                                                                                         if_else(puma %in% c("3809", "3810"), '38019', as.character(puma))))))
df.race <- mutate(df.race, superpuma = if_else(puma %in% c("3802", "3803", "3804"), "380234",
                                               if_else(puma %in% c("3805", "3806"), "38056",
                                                       if_else(puma %in% c("3807", "3808"), "38078",
                                                               if_else(puma %in% c("3809", "3810"), '38019', as.character(puma))))))

df.age <- mutate(df.age, superpuma = if_else(puma %in% c("3802", "3803", "3804"), "380234",
                                             if_else(puma %in% c("3805", "3806"), "38056",
                                                     if_else(puma %in% c("3807", "3808"), "38078",
                                                             if_else(puma %in% c("3809", "3810"), '38019', as.character(puma))))))

### add zip codes

tzxwalk <- read_csv(paste(rootdir, "/Data/tract_zcta_xwalk.csv", sep = "")) %>%
        mutate(tract = str_remove_all(paste(county, tract, sep = ""), "[.]")) %>%
        arrange(desc(afact)) %>%
        group_by(tract) %>%
        slice(1) %>%
        select(tract, zcta5, cntyname)

df.master <- merge(df.master, tzxwalk, by = "tract", all.x = TRUE)
df.master_stratified <- merge(df.master_stratified, tzxwalk, by = "tract", all.x = TRUE)
df.race <- merge(df.race, tzxwalk, by = "tract", all.x = TRUE)
df.age <- merge(df.age, tzxwalk, by = "tract", all.x = TRUE)

### create rate vars
# 
# df.master <- mutate(df.master, death_rate = cumulative_total_deaths*100000/tot_population,
#                     hosp_rate = cumulative_hospitalized*100000/tot_population)
# df.race <- mutate(df.race, population = if_else(raceethnicity=="White", tot_population*white,
#                                                              if_else(raceethnicity=="Black/African American", tot_population*black,
#                                                                      if_else(raceethnicity=="Hispanic/Latino", tot_population*hispanic,
#                                                                              if_else(raceethnicity=="Asian/Pacific Islander", tot_population*asian, tot_population*other_nonhisp)))),
#                         death_rate = stratified_totaldeaths*100000/population,
#                         hosp_rate = stratified_hospitalized*100000/population)
# df.age <- mutate(df.age, population = if_else(agegroup=="0-17", tot_population*tot_under17,
#                                                             if_else(agegroup=="18-64", tot_population*(tot_age18_44+tot_age45_54+tot_age55_64),
#                                                                     if_else(agegroup=="65-74", tot_population*tot_age65_74,
#                                                                             if_else(agegroup=="75up", tot_population*asian, tot_population*tot_age75up)))),
#                         death_rate = stratified_totaldeaths*100000/population,
#                         hosp_rate = stratified_hospitalized*100000/population)

### write csvs

df.master_stratified[is.na(df.master_stratified)] <- "."
df.master[is.na(df.master)] <- "."
df.race[is.na(df.race)] <- "."
df.age[is.na(df.age)] <- "."
df.master <- filter(df.master, !is.na(puma))
df.master_stratified <- filter(df.master_stratified, !is.na(puma))
df.race <- filter(df.race, !is.na(puma))
df.age <- filter(df.age, !is.na(puma))

write_csv(df.master_stratified, paste(rootdir, "/Data/master_stratified.csv", sep = ""))
write_csv(df.master, paste(rootdir, "/Data/master.csv", sep = ""))
write_csv(df.race, paste(rootdir, "/Data/race_stratified.csv", sep = ""))
write_csv(df.age, paste(rootdir, "/Data/age_stratified.csv", sep = ""))

