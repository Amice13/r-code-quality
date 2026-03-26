


# load gridded data
data.ls <- readRDS("data/grid_dfs.rds")

data.g25 <- data.ls[[1]]
data.g50 <- data.ls[[2]]
data.g100 <- data.ls[[3]]
data.g200 <- data.ls[[4]]
data.g400 <- data.ls[[5]]

################################################
## recode variables for analysis
data.g25$mrdk_centr_dummy <- ifelse(data.g25$mrdk_precol_centr%in%c(3,4),1,0)
data.g25$mrdk_centr_dummy[is.na(data.g25$mrdk_centr_dummy)] <- 0 
data.g25$mrdk_chiefdom_dummy <- ifelse(data.g25$mrdk_precol_centr%in%c(1,2),1,0)
data.g25$mrdk_chiefdom_dummy[is.na(data.g25$mrdk_chiefdom_dummy)] <- 0 
data.g25$mrdk_centr_missing <- ifelse(is.na(data.g25$mrdk_precol_centr),1,0)

data.g25$mrdk_agric_medium <- ifelse(data.g25$mrdk_precol_agric%in%c(5,6),1,0)
data.g25$mrdk_agric_medium[is.na(data.g25$mrdk_agric_medium)] <- 0 
data.g25$mrdk_agric_high <- ifelse(data.g25$mrdk_precol_agric%in%c(7:10),1,0)
data.g25$mrdk_agric_high[is.na(data.g25$mrdk_agric_high)] <- 0 
data.g25$mrdk_agric_missing <- ifelse(is.na(data.g25$mrdk_precol_agric),1,0)

data.g25$slaves_medium <- ifelse(data.g25$slaves_area>0 & data.g25$slaves_area<0.017,1,0)
data.g25$slaves_medium[is.na(data.g25$slaves_medium)] <- 0 
data.g25$slaves_high <- ifelse(data.g25$slaves_area>0.017,1,0)
data.g25$slaves_high[is.na(data.g25$slaves_high)] <- 0 
data.g25$slaves_missing <- ifelse(is.na(data.g25$slaves_area),1,0)

data.g50$mrdk_chiefdom_dummy <- ifelse(data.g50$mrdk_chiefdom_dummy==1 & data.g50$mrdk_centr_dummy==0,1,0)
data.g50$mrdk_centr_missing <- ifelse(data.g50$mrdk_centr_missing==1 & data.g50$mrdk_chiefdom_dummy==0 & data.g50$mrdk_centr_dummy==0,1,0)
data.g50$mrdk_agric_medium <- ifelse(data.g50$mrdk_agric_medium==1 & data.g50$mrdk_agric_high==0,1,0)
data.g50$mrdk_agric_missing <- ifelse(data.g50$mrdk_agric_missing==1 & data.g50$mrdk_agric_medium==0 & data.g50$mrdk_agric_high==0,1,0)
data.g50$slaves_medium <- ifelse(data.g50$slaves_medium==1 & data.g50$slaves_high==0,1,0)
data.g50$slaves_missing <- ifelse(data.g50$slaves_missing==1 & data.g50$slaves_medium==0 & data.g50$slaves_high==0,1,0)

data.g100$mrdk_chiefdom_dummy <- ifelse(data.g100$mrdk_chiefdom_dummy==1 & data.g100$mrdk_centr_dummy==0,1,0)
data.g100$mrdk_centr_missing <- ifelse(data.g100$mrdk_centr_missing==1 & data.g100$mrdk_chiefdom_dummy==0 & data.g100$mrdk_centr_dummy==0,1,0)
data.g100$mrdk_agric_medium <- ifelse(data.g100$mrdk_agric_medium==1 & data.g100$mrdk_agric_high==0,1,0)
data.g100$mrdk_agric_missing <- ifelse(data.g100$mrdk_agric_missing==1 & data.g100$mrdk_agric_medium==0 & data.g100$mrdk_agric_high==0,1,0)
data.g100$slaves_medium <- ifelse(data.g100$slaves_medium==1 & data.g100$slaves_high==0,1,0)
data.g100$slaves_missing <- ifelse(data.g100$slaves_missing==1 & data.g100$slaves_medium==0 & data.g100$slaves_high==0,1,0)

data.g200$mrdk_chiefdom_dummy <- ifelse(data.g200$mrdk_chiefdom_dummy==1 & data.g200$mrdk_centr_dummy==0,1,0)
data.g200$mrdk_centr_missing <- ifelse(data.g200$mrdk_centr_missing==1 & data.g200$mrdk_chiefdom_dummy==0 & data.g200$mrdk_centr_dummy==0,1,0)
data.g200$mrdk_agric_medium <- ifelse(data.g200$mrdk_agric_medium==1 & data.g200$mrdk_agric_high==0,1,0)
data.g200$mrdk_agric_missing <- ifelse(data.g200$mrdk_agric_missing==1 & data.g200$mrdk_agric_medium==0 & data.g200$mrdk_agric_high==0,1,0)
data.g200$slaves_medium <- ifelse(data.g200$slaves_medium==1 & data.g200$slaves_high==0,1,0)
data.g200$slaves_missing <- ifelse(data.g200$slaves_missing==1 & data.g200$slaves_medium==0 & data.g200$slaves_high==0,1,0)

data.g400$mrdk_chiefdom_dummy <- ifelse(data.g400$mrdk_chiefdom_dummy==1 & data.g400$mrdk_centr_dummy==0,1,0)
data.g400$mrdk_centr_missing <- ifelse(data.g400$mrdk_centr_missing==1 & data.g400$mrdk_chiefdom_dummy==0 & data.g400$mrdk_centr_dummy==0,1,0)
data.g400$mrdk_agric_medium <- ifelse(data.g400$mrdk_agric_medium==1 & data.g400$mrdk_agric_high==0,1,0)
data.g400$mrdk_agric_missing <- ifelse(data.g400$mrdk_agric_missing==1 & data.g400$mrdk_agric_medium==0 & data.g400$mrdk_agric_high==0,1,0)
data.g400$slaves_medium <- ifelse(data.g400$slaves_medium==1 & data.g400$slaves_high==0,1,0)
data.g400$slaves_missing <- ifelse(data.g400$slaves_missing==1 & data.g400$slaves_medium==0 & data.g400$slaves_high==0,1,0)


# logged distances
data.g25$rivers_log <- log(1+data.g25$dist_river_nav)
data.g25$coast_log <- log(1+data.g25$dist_coast)
data.g25$trade_log <- log(1+data.g25$dist_trade_route_1900)
data.g25$explorers_log <- log(1+data.g25$dist_expl_route)
data.g25$cities_log <- log(1+data.g25$dist_jedwab_city_1900)
data.g25$capital_log <- log(1+data.g25$dist_capital_col)

data.g50$rivers_log <- log(1+data.g50$dist_river_nav)
data.g50$coast_log <- log(1+data.g50$dist_coast)
data.g50$trade_log <- log(1+data.g50$dist_trade_route_1900)
data.g50$explorers_log <- log(1+data.g50$dist_expl_route)
data.g50$cities_log <- log(1+data.g50$dist_jedwab_city_1900)
data.g50$capital_log <- log(1+data.g50$dist_capital_col)

data.g100$rivers_log <- log(1+data.g100$dist_river_nav)
data.g100$coast_log <- log(1+data.g100$dist_coast)
data.g100$trade_log <- log(1+data.g100$dist_trade_route_1900)
data.g100$explorers_log <- log(1+data.g100$dist_expl_route)
data.g100$cities_log <- log(1+data.g100$dist_jedwab_city_1900)
data.g100$capital_log <- log(1+data.g100$dist_capital_col)

data.g200$rivers_log <- log(1+data.g200$dist_river_nav)
data.g200$coast_log <- log(1+data.g200$dist_coast)
data.g200$trade_log <- log(1+data.g200$dist_trade_route_1900)
data.g200$explorers_log <- log(1+data.g200$dist_expl_route)
data.g200$cities_log <- log(1+data.g200$dist_jedwab_city_1900)
data.g200$capital_log <- log(1+data.g200$dist_capital_col)

data.g400$rivers_log <- log(1+data.g400$dist_river_nav)
data.g400$coast_log <- log(1+data.g400$dist_coast)
data.g400$trade_log <- log(1+data.g400$dist_trade_route_1900)
data.g400$explorers_log <- log(1+data.g400$dist_expl_route)
data.g400$cities_log <- log(1+data.g400$dist_jedwab_city_1900)
data.g400$capital_log <- log(1+data.g400$dist_capital_col)

# standardize suitability score
data.g25$suit_std <- scale(data.g25$suit_low_mean9)[,1]
data.g25$cal_avg_post0_std <- scale(data.g25$cal_avg_post0)[,1]
data.g25$cal_avg_post_std <- scale(data.g25$cal_avg_post)[,1]
data.g25$agric_suit_std <- scale(data.g25$agric_suit)[,1]

data.g50$suit_std <- scale(data.g50$suit_low_mean9)[,1]
data.g50$cal_avg_post0_std <- scale(data.g50$cal_avg_post0)[,1]
data.g50$cal_avg_post_std <- scale(data.g50$cal_avg_post)[,1]
data.g50$agric_suit_std <- scale(data.g50$agric_suit)[,1]

data.g100$suit_std <- scale(data.g100$suit_low_mean9)[,1]
data.g100$cal_avg_post0_std <- scale(data.g100$cal_avg_post0)[,1]
data.g100$cal_avg_post_std <- scale(data.g100$cal_avg_post)[,1]
data.g100$agric_suit_std <- scale(data.g100$agric_suit)[,1]

data.g200$suit_std <- scale(data.g200$suit_low_mean9)[,1]
data.g200$cal_avg_post0_std <- scale(data.g200$cal_avg_post0)[,1]
data.g200$cal_avg_post_std <- scale(data.g200$cal_avg_post)[,1]
data.g200$agric_suit_std <- scale(data.g200$agric_suit)[,1]

data.g400$suit_std <- scale(data.g400$suit_low_mean9)[,1]
data.g400$cal_avg_post0_std <- scale(data.g400$cal_avg_post0)[,1]
data.g400$cal_avg_post_std <- scale(data.g400$cal_avg_post)[,1]
data.g400$agric_suit_std <- scale(data.g400$agric_suit)[,1]


##### make outcome vars
#describe(100000*data.g25$nl_2015_sum/data.g25$popc_2010AD)
data.g25$nl_2015_sqkm_log <- log(1+1000*data.g25$nl_2015_sum/data.g25$area_land_sqkm)
data.g25$nl_2015_pc_log <- log(1+100000*data.g25$nl_2015_sum/data.g25$popc_2010AD)
data.g25$nl_2015_pc_log[is.na(data.g25$nl_2015_pc_log) | is.infinite(data.g25$nl_2015_pc_log)] <- 0
data.g25$urb_dens_2015_log <- log(1+data.g25$urb_pop_2015/data.g25$area_land_sqkm)
data.g25$road_dens_2015_log <- log(1+1000*data.g25$roads_imp_1998_km/data.g25$area_land_sqkm)
data.g25$crops_sqkm_log <- log(1+2890*data.g25$hance_crops_sum/data.g25$area_land_sqkm)
data.g25$minerals_sqkm_log <- log(1+2890*data.g25$hance_minerals_sum/data.g25$area_land_sqkm)

data.g50$nl_2015_sqkm_log <- log(1+1000*data.g50$nl_2015_sum/data.g50$area_land_sqkm)
data.g50$nl_2015_pc_log <- log(1+100000*data.g50$nl_2015_sum/data.g50$popc_2010AD)
data.g50$nl_2015_pc_log[is.na(data.g50$nl_2015_pc_log) | is.infinite(data.g50$nl_2015_pc_log)] <- 0
data.g50$urb_dens_2015_log <- log(1+data.g50$urb_pop_2015/data.g50$area_land_sqkm)
data.g50$road_dens_2015_log <- log(1+1000*data.g50$roads_imp_1998_km/data.g50$area_land_sqkm)
data.g50$crops_sqkm_log <- log(1+2890*data.g50$hance_crops_sum/data.g50$area_land_sqkm)
data.g50$minerals_sqkm_log <- log(1+2890*data.g50$hance_minerals_sum/data.g50$area_land_sqkm)

data.g100$nl_2015_sqkm_log <- log(1+1000*data.g100$nl_2015_sum/data.g100$area_land_sqkm)
data.g100$nl_2015_pc_log <- log(1+100000*data.g100$nl_2015_sum/data.g100$popc_2010AD)
data.g100$nl_2015_pc_log[is.na(data.g100$nl_2015_pc_log) | is.infinite(data.g100$nl_2015_pc_log)] <- 0
data.g100$urb_dens_2015_log <- log(1+data.g100$urb_pop_2015/data.g100$area_land_sqkm)
data.g100$road_dens_2015_log <- log(1+1000*data.g100$roads_imp_1998_km/data.g100$area_land_sqkm)
data.g100$crops_sqkm_log <- log(1+2890*data.g100$hance_crops_sum/data.g100$area_land_sqkm)
data.g100$minerals_sqkm_log <- log(1+2890*data.g100$hance_minerals_sum/data.g100$area_land_sqkm)

data.g200$nl_2015_sqkm_log <- log(1+1000*data.g200$nl_2015_sum/data.g200$area_land_sqkm)
data.g200$nl_2015_pc_log <- log(1+100000*data.g200$nl_2015_sum/data.g200$popc_2010AD)
data.g200$nl_2015_pc_log[is.na(data.g200$nl_2015_pc_log) | is.infinite(data.g200$nl_2015_pc_log)] <- 0
data.g200$urb_dens_2015_log <- log(1+data.g200$urb_pop_2015/data.g200$area_land_sqkm)
data.g200$road_dens_2015_log <- log(1+1000*data.g200$roads_imp_1998_km/data.g200$area_land_sqkm)
data.g200$crops_sqkm_log <- log(1+2890*data.g200$hance_crops_sum/data.g200$area_land_sqkm)
data.g200$minerals_sqkm_log <- log(1+2890*data.g200$hance_minerals_sum/data.g200$area_land_sqkm)

data.g400$nl_2015_sqkm_log <- log(1+1000*data.g400$nl_2015_sum/data.g400$area_land_sqkm)
data.g400$nl_2015_pc_log <- log(1+100000*data.g400$nl_2015_sum/data.g400$popc_2010AD)
data.g400$nl_2015_pc_log[is.na(data.g400$nl_2015_pc_log) | is.infinite(data.g400$nl_2015_pc_log)] <- 0
data.g400$urb_dens_2015_log <- log(1+data.g400$urb_pop_2015/data.g400$area_land_sqkm)
data.g400$road_dens_2015_log <- log(1+1000*data.g400$roads_imp_1998_km/data.g400$area_land_sqkm)
data.g400$crops_sqkm_log <- log(1+2890*data.g400$hance_crops_sum/data.g400$area_land_sqkm)
data.g400$minerals_sqkm_log <- log(1+2890*data.g400$hance_minerals_sum/data.g400$area_land_sqkm)


### define balance variables
bal.vars <-c("suit_std","cal_avg_post_std","TSI_CRU_mean_1901_2017","malaria_suit_max","ruggedness","elevation_mean",
             "dist_river_nav","dist_coast","dist_trade_route_1900",
             "dist_jedwab_city_1900","dist_capital_col",
             "slaves_medium","slaves_high","mrdk_agric_medium","mrdk_agric_high",
             "mrdk_chiefdom_dummy","mrdk_centr_dummy","mrdk_centr_missing")

#out.vars <- c("fibre_cables_2019_yn","fibre_cables_live_2019_yn","hospitals_2019_yn","hospitals_2019_gov_yn")
data.g25$lon_sq <- data.g25$Longitude^2
data.g25$lon_cu <- data.g25$Longitude^3

data.g25$lat_sq <- data.g25$Latitude^2
data.g25$lat_cu <- data.g25$Latitude^3


##### standardize continuous control variables
data.g25$tsetse_std <- scale(data.g25$TSI_CRU_mean_1901_2017)[,1]
data.g25$malaria_std <- scale(data.g25$malaria_suit_max)[,1]
data.g25$ruggedness_nunn_std <- scale(data.g25$ruggedness_nunn_puga)[,1]
data.g25$ruggedness_std <- scale(data.g25$ruggedness)[,1]
data.g25$elevation_std <- scale(data.g25$elevation_mean)[,1]
data.g25$rivers_log_std <- scale(data.g25$rivers_log)[,1]
data.g25$coast_log_std <- scale(data.g25$coast_log)[,1]
data.g25$rivers_std <- scale(data.g25$dist_river_nav)[,1]
data.g25$coast_std <- scale(data.g25$dist_coast)[,1]
data.g25$hance_crops_std <- scale(data.g25$hance_crops_sum)[,1]
data.g25$cal_avg_pre_std <- scale(data.g25$cal_avg_pre)[,1]
data.g25$cal_avg_post_std <- scale(data.g25$cal_avg_post)[,1]
data.g25$cal_avg_pre0_std <- scale(data.g25$cal_avg_pre0)[,1]
data.g25$cal_avg_post0_std <- scale(data.g25$cal_avg_post0)[,1]
data.g25$explorers_log_std <- scale(data.g25$explorers_log)[,1]
data.g25$explorers_std <- scale(data.g25$dist_expl_route)[,1]
data.g25$cities_log_std <- scale(data.g25$cities_log)[,1]
data.g25$cities_std <- scale(data.g25$dist_jedwab_city_1900)[,1]
data.g25$capital_log_std <- scale(data.g25$capital_log)[,1]
data.g25$capital_std <- scale(data.g25$dist_capital_col)[,1]
data.g25$trade_log_std <- scale(data.g25$trade_log)[,1]
data.g25$trade_std <- scale(data.g25$dist_trade_route_1900)[,1]
data.g25$temp_std <- scale(data.g25$temperature_fao)[,1]
data.g25$prec_std <- scale(data.g25$precipitation_fao)[,1]
data.g25$lon_std <- scale(data.g25$Longitude)[,1]
data.g25$lat_std <- scale(data.g25$Latitude)[,1]

controls.std <- c("cal_avg_post_std","tsetse_std","malaria_std","ruggedness_std","elevation_std","rivers_log_std","coast_log_std",
                  "cities_log_std","capital_log_std","trade_log_std","slaves_medium","slaves_high","mrdk_agric_high","mrdk_agric_medium",
                  "mrdk_centr_dummy","mrdk_chiefdom_dummy","mrdk_centr_missing","Longitude","Latitude","lon_sq", "lat_sq")
controls.geo <- c("cal_avg_post_std","tsetse_std","malaria_std","ruggedness_std","elevation_std","rivers_log_std","coast_log_std","Longitude","Latitude","lon_sq", "lat_sq")

controls.std2 <- c("cal_avg_post_std","tsetse_std","malaria_std","ruggedness_std","elevation_std","rivers_std","coast_std",
                   "cities_std","capital_std","trade_std","slaves_medium","slaves_high","mrdk_agric_high","mrdk_agric_medium",
                   "mrdk_centr_dummy","mrdk_chiefdom_dummy","mrdk_centr_missing","Longitude","Latitude","lon_sq", "lat_sq")








