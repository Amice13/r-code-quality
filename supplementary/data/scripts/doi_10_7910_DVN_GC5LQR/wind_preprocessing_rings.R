library(tidyverse)
library(haven)
library(sf)
library(parallel)
library(lubridate)

rootdir <- dirname(getwd())

df.wind <- read_stata(paste(rootdir, "/Data/Weather/Global Hourly/all_2008_2018_hourly.dta", sep = ""))
df.wind$hour <- hour(str_replace(df.wind$date, "T", " "))

df.wind <- filter(df.wind, is.na(hourly_wind_direction)==FALSE) 

df.wind$unique_id<-paste(df.wind$year,df.wind$month,df.wind$day,df.wind$hour,df.wind$station)
df.wind$duplicate<-duplicated(df.wind$unique_id)
df.wind<-filter(df.wind,duplicate==FALSE)

df.wind$rushhour <- if_else(between(df.wind$hour,10,23),1,0)

df.tract_rds <- read_csv(paste(rootdir, "/Data/NYCCAS/Downwind/tract_roads_distance_bearing.csv", sep = "")) %>%
  select(dist, bearing, boro_ct201) %>%
  mutate(dist = round(dist, -1),
         bearing = round(bearing, -1),
         bearing = if_else(bearing <= 0, 360+bearing, bearing),
         max_bearing90 = if_else(bearing+15<360, bearing+15, bearing-345),
         min_bearing90 = if_else(bearing-15>0, bearing-15, bearing+345),
         max_bearing45 = if_else(bearing+22.5<360, bearing+22.5, bearing-337.5),
         min_bearing45 = if_else(bearing-22.5>0, bearing-22.5, bearing+337.5),
         wrap45 = if_else(abs(max_bearing45 - min_bearing45) >180, 1, 0),
         wrap90 = if_else(abs(max_bearing90 - min_bearing90) >180, 1, 0))

### find nearest station for each tract
stations.shp <- df.wind %>%
  mutate(latitude = round(latitude, 1),
         longitude = round(longitude, 1)) %>%
  group_by(latitude, longitude, name) %>%
  summarize(station_hrs = n()) %>%
  filter(name!="NY CITY CENTRAL PARK, NY US") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4269) %>%
  mutate(ind = row_number()) 

shp.tract <- st_read(paste(rootdir, "/Data/Census Tracts shapefiles", sep = ""), layer = "geo_export_79d01148-4a56-4331-a800-1e4f76bed5aa")  %>%
  st_transform(crs = 4269)

shp.tract$ind <- st_nearest_feature(shp.tract, stations.shp)
shp.tract$geometry <- NULL

df.station_tract <- merge(shp.tract, stations.shp, by = "ind")  %>%
  select(boro_ct201, name, station_hrs) %>%
  rename(station = name)

df.tract_rds <- unique(merge(df.tract_rds, df.station_tract, by = "boro_ct201"))

### calculate downwind days
df.wind <- filter(df.wind, name %in% df.tract_rds$station & is.na(hourly_wind_direction)==FALSE) %>%
  select(name, hourly_wind_speed, hourly_wind_direction, year, month, day, hour, rushhour) %>%
  rename(station = name)

tracts <- df.station_tract$boro_ct201

ang_btw <- function(a, amin, amax){
  start <- if_else(amax - amin < 0, amax - amin + 360, amax - amin)
  mid <- if_else(a - amin < 0, a - amin + 360, a - amin)
  return(mid < start)
}

is_downwind <- function(a, data){
  data$dw45 <- ang_btw(a, data$min_bearing90, data$max_bearing90)
  downwind_5_r <- if_else(sum(data$dw45[data$dist<5000 & data$dist>3000])==0,0,1)
  downwind_3_r <- if_else(sum(data$dw45[data$dist<=3000 & data$dist>2000])==0,0,1)
  downwind_2_r <- if_else(sum(data$dw45[data$dist<=2000 & data$dist>1000])==0,0,1)
  downwind_1_r <- if_else(sum(data$dw45[data$dist<=1000 & data$dist>500])==0,0,1)
  downwind_0_r <- if_else(sum(data$dw45[data$dist<=500])==0,0,1)
  return(c(downwind_5_r, downwind_3_r, downwind_2_r, 
           downwind_1_r, downwind_0_r))
  gc()
}

tract_dw_vars <- function(tract, df.tract_rds, df.wind){
  v <- tryCatch({
    dft <- filter(df.tract_rds, boro_ct201==tract)
    if (nrow(dft)==0) {
      v <- rep(0, 10)
    } else {
      s <- unique(dft$station)
      dfw <- filter(df.wind, station==s)
      nms <- colnames(dfw)
      dwd <- mapply(FUN = is_downwind, as.list(dfw$hourly_wind_direction), 
                    MoreArgs = list(data = dft[,c("min_bearing90", "max_bearing90", "dist")]))
      dfw <- cbind(dfw, t(dwd))
      colnames(dfw) <- c(nms, "downwind_5_r", "downwind_3_r", "downwind_2_r",  
                         "downwind_1_r", "downwind_0_r")
      v <- c(
             sum(dfw$downwind_5_r, na.rm = TRUE),
             sum(dfw$downwind_3_r, na.rm = TRUE),
             sum(dfw$downwind_2_r, na.rm = TRUE),
             sum(dfw$downwind_1_r, na.rm = TRUE),
             sum(dfw$downwind_0_r, na.rm = TRUE),
             sum(dfw$downwind_5_r[dfw$rushhour==0], na.rm = TRUE),
             sum(dfw$downwind_3_r[dfw$rushhour==0], na.rm = TRUE),
             sum(dfw$downwind_2_r[dfw$rushhour==0], na.rm = TRUE),
             sum(dfw$downwind_1_r[dfw$rushhour==0], na.rm = TRUE),
             sum(dfw$downwind_0_r[dfw$rushhour==0], na.rm = TRUE)
             )
    }},
    error = function(cond){
      message(cond)
      message(paste(tract, " has a problem"))
      v <- rep(NA, 10)
    }, warning = function(cond){
      message(cond)
      message(paste(tract, " has a problem"))
      v <- rep(NA, 10)
    }, finally = {print(paste("finished: ", tract))}
  )
  return(v)
  gc()
}

cores <- detectCores() - 2

dw_vars <- mcmapply(tract_dw_vars, as.list(tracts), 
                    MoreArgs = list(df.tract_rds = df.tract_rds, df.wind = df.wind), 
                    mc.cores = cores, mc.preschedule = FALSE)

saveRDS(dw_vars, file = paste(rootdir, "/Data/dw_vars_rings.rds", sep = ""))

df.downwind <- data.frame(t(as.data.frame(dw_vars)))


df.downwind$X11<-df.downwind$X1-df.downwind$X6
df.downwind$X12<-df.downwind$X2-df.downwind$X7
df.downwind$X13<-df.downwind$X3-df.downwind$X8
df.downwind$X14<-df.downwind$X4-df.downwind$X9
df.downwind$X15<-df.downwind$X5-df.downwind$X10

y<-crosstab(df.wind$station, df.wind$rushhour)
df.station_tract$station_hrs_nrush[df.station_tract$station=="JFK INTERNATIONAL AIRPORT, NY US"]<-y$tab[1,1]
df.station_tract$station_hrs_rush[df.station_tract$station=="JFK INTERNATIONAL AIRPORT, NY US"]<-y$tab[1,2]
df.station_tract$station_hrs_nrush[df.station_tract$station=="LAGUARDIA AIRPORT, NY US"]<-y$tab[2,1]
df.station_tract$station_hrs_rush[df.station_tract$station=="LAGUARDIA AIRPORT, NY US"]<-y$tab[2,2]
df.station_tract$station_hrs_nrush[df.station_tract$station=="NEWARK LIBERTY INTERNATIONAL AIRPORT, NJ US"]<-y$tab[3,1]
df.station_tract$station_hrs_rush[df.station_tract$station=="NEWARK LIBERTY INTERNATIONAL AIRPORT, NJ US"]<-y$tab[3,2]
df.station_tract$station_hrs_nrush[df.station_tract$station=="TETERBORO AIRPORT, NJ US"]<-y$tab[4,1]
df.station_tract$station_hrs_rush[df.station_tract$station=="TETERBORO AIRPORT, NJ US"]<-y$tab[4,2]
df.station_tract$station_hrs<-df.station_tract$station_hrs_nrush+df.station_tract$station_hrs_rush

df.downwind <- cbind(df.station_tract, df.downwind)

saveRDS(df.downwind, file = paste(rootdir, "/Data/downwind_rings.rds", sep = ""))

write_csv(df.downwind, paste(rootdir, "/Data/downwind_vars_rings_rh30.csv", sep = ""))
