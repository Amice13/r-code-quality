#####################################################################################
# Create all tables for online appendix of the paper
# Soeren Henn
# 2022-08-22
#####################################################################################


rm(list=ls())

## Set working directory

library(lfe)        # to run linear fixed effects models
library(stargazer)  # to output tables
library(doBy)       # to collapse data by country with summaryBy
library(dplyr)      # to bind rows
library(xtable)     # to print table to latex

#######################################################################################
############### Table B1-2: Summary Statistics for Full Regression Sample ###############
#######################################################################################

load("MainRegressionSample.RData")

data.reg[,"urban"] <- NA
data.reg[which(data.reg[,"urbrur"]==1),"urban"] <- 1
data.reg[which(data.reg[,"urbrur"]>1),"urban"] <- 0


data.reg.afro <- subset(data.reg, select = c(distance_hq, distance_border, distance_neigh5, neighbordist, traveltime, treatment_int, urban, distance_capital.raw, distance_natborder.raw, distcoast.raw, elevation.raw, ruggedness.raw, malaria.raw, agriculture.raw, missions.raw, cities.raw, distrail.raw, area_sqkm, chief_zscore, z_influence_tradition, z_trust_tradition, z_corrupt_traditiontemp, z_contact_tradition, state_capacity))
colnames(data.reg.afro)[colnames(data.reg.afro)=="distance_neigh5"] <- "distance_neigh"

load("MainRegressionSampleDHS.RData")

data.reg[,"urban"] <- NA
data.reg[which(data.reg[,"urbrur"]==1),"urban"] <- 1
data.reg[which(data.reg[,"urbrur"]>1),"urban"] <- 0


data.reg.dhs <- subset(data.reg, select = c(distance_hq, distance_border, distance_neigh5, treatment_int, urban, distance_capital.raw, distance_natborder.raw, distcoast.raw, elevation.raw, ruggedness.raw, malaria.raw, agriculture.raw, missions.raw, cities.raw, distrail.raw, area_sqkm, state_capacity, electricity, registered, time.to.water, read.all.raw, wealth.index.raw, infant.mortality.raw, trad_med, kids.gone.raw, men.always.raw, wm.always.raw))
colnames(data.reg.dhs)[colnames(data.reg.dhs)=="distance_neigh5"] <- "distance_neigh"

data.reg <- bind_rows(data.reg.afro, data.reg.dhs)

data.reg$treatment_int <- scale(data.reg$treatment_int)
data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)
data.reg$state_capacity <- scale(data.reg$state_capacity)


stargazer(data.reg.afro, type = "latex",
          title="Summary Statistics for Afrobarometer Regression Sample",
          covariate.labels=c("Distance to Headquarter (km)", "Distance to Admin. Border (km)", "Distance to Village on Other Side (km)" , "Distance to Neighbouring HQ (km)", "Traveltime to HQ (in min)", "Treatment Intensity", "Urban", "Distance to National Capital (km)", "Distance to National Border", "Distance to Coast (km)", "Elevation", "Ruggedness", "Malaria Suitability", "Agricultural Suitability", "Distance to Christian Missions (km)", "Distance to Histroical Cities (km)", "Distance to Colonial Railroad (km)", "Admin. Unit Size (sqkm)", "Chief Z-score", "Chief Influence", "Trust in Chief", "Corrupt Chief (Inverse)", "Contact with Chief", "State Capacity Index"),
          label="sumstatAfro",
          digits=2,
          omit.summary.stat=c("p25", "p75"),
          out="table_b1_sumstatAfro.tex"
)

stargazer(data.reg.dhs, type = "latex",
          title="Summary Statistics for DHS Regression Sample",
          covariate.labels=c("Distance to Headquarter (km)", "Distance to Admin. Border (km)", "Distance to Village on Other Side (km)", "Treatment Intensity", "Urban", "Distance to National Capital (km)", "Distance to National Border (km)", "Distance to Coast (km)", "Elevation", "Ruggedness", "Malaria Suitability", "Agricultural Suitability", "Distance to Christian Missions (km)", "Distance to Histroical Cities (km)", "Distance to Colonial Railroad (km)", "Admin. Unit Size (sqkm)", "State Capacity Index", "Percentage of HH  with Electricity", "Percentage of Children Registered", "Average Time to Water (min)", "Literacy", "Wealth Index", "Infant Mortality", "Traditional Medicine", "Percentage of Kids Gone", "Percentage of Men Born in Location", "Percentage of Women Born in Location"),
          digits=2,
          label="sumstatDHS",
          omit.summary.stat=c("p25", "p75"),
          out="table_b2_sumstatDHS.tex"
)





####################################################################################
######################### Tables A16-18: Robustness of DHS #########################
####################################################################################
load("MainRegressionSampleDHS.RData")

data.reg$treatment_int <- scale(data.reg$treatment_int)
data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)
data.reg$development <- scale(data.reg$development)

################################# Main Specification ################################
model.main <- felm(development ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

###################################### OLS Only #####################################
model.ols <- felm(development ~ log_distance_hq*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

##################################### Rural Only ####################################
model.rural <- felm(development ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg, subset = urbrur==2)

################################### Drop Outliers ###################################
#model.100km <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg, subset = distance_hq<100)
model.50km <- felm(development ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg, subset = distance_hq<50)

################################## Cluster Higher ###################################
model.cluster <- felm(development ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_cluster, data.reg)

########################## Controlling for Dist to Neighbor #########################
model.distneigh <- felm(development ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum + neighbordist*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

###################################### By Admin #####################################
model.admin1 <- felm(development ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg, subset = adminlevel==1)
model.admin2 <- felm(development ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg, subset = adminlevel==2)

################################### Just Extensive ##################################
model.binary <- felm(development ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

##################################### No Controls ###################################
model.nocontrol <- felm(development ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

############################## Included in Constitution #############################
model.included <- felm(development ~ distance_border*inc_leaders_dum + dist_treat*inc_leaders_dum + treatment_int*inc_leaders_dum + distance_capital*inc_leaders_dum + distance_natborder*inc_leaders_dum + distcoast*inc_leaders_dum + elevation*inc_leaders_dum + ruggedness*inc_leaders_dum + agriculture*inc_leaders_dum + cities*inc_leaders_dum + malaria*inc_leaders_dum + missions*inc_leaders_dum + distrail*inc_leaders_dum + area_sqkm*inc_leaders_dum | fixed_effects |0| district_id, data.reg)

############################## Protected by Constitution ############################
model.protected <- felm(development ~ distance_border*protect_chiefs_dum + dist_treat*protect_chiefs_dum + treatment_int*protect_chiefs_dum + distance_capital*protect_chiefs_dum + distance_natborder*protect_chiefs_dum + distcoast*protect_chiefs_dum + elevation*protect_chiefs_dum + ruggedness*protect_chiefs_dum + agriculture*protect_chiefs_dum + cities*protect_chiefs_dum + malaria*protect_chiefs_dum + missions*protect_chiefs_dum + distrail*protect_chiefs_dum + area_sqkm*protect_chiefs_dum | fixed_effects |0| district_id, data.reg)

############################### British Colonies only ###############################
model.british <- felm(development ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

############################ Ethnic Homeland Fixed Effects ##########################
model.ethnic <- felm(development ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects + NAME |0| district_id, data.reg)

###################################### "Donout" #####################################
data.reg$distance_border.abs <- abs(data.reg$distance_border)
model.donut <- felm(development ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg, subset = distance_border.abs>1)

##################################### Non_Logged ####################################
load("RegressionSampleDHS.RData")

data.reg <- data.reg[which(data.reg$distance_hq<75),]
data.reg <- data.reg[which(data.reg$distance_neigh5<30),]
data.reg$treatment <- data.reg$treatment5
data.reg$treatment_int <- data.reg$treatment_int5
data.reg$treatment_int <- data.reg$treatment_int*(data.reg$dist_coeff*-1)
data.reg$distance_border[which(data.reg$treatment==0)] <- - data.reg$distance_border[which(data.reg$treatment==0)]
data.reg$dist_treat <- data.reg$treatment*data.reg$distance_border

data.reg$treatment_int <- scale(data.reg$treatment_int)
data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)
data.reg$development <- scale(data.reg$development)

model.nonlogged <- felm(development ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

################################## Don't drop 30km ##################################
load("RegressionSampleDHS.RData")

data.reg <- data.reg[which(data.reg$distance_hq<75),]
data.reg$treatment <- data.reg$treatment_log5
data.reg$treatment_int <- data.reg$treatment_int_log5
data.reg$treatment_int <- data.reg$treatment_int*(data.reg$dist_coeff_log*-1)
data.reg$distance_border[which(data.reg$treatment==0)] <- - data.reg$distance_border[which(data.reg$treatment==0)]
data.reg$dist_treat <- data.reg$treatment*data.reg$distance_border

data.reg$treatment_int <- scale(data.reg$treatment_int)
data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)
data.reg$development <- scale(data.reg$development)

model.dont30 <- felm(development ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

#################################### Traveltime #####################################
## load first stage effects
CoeffTT <- read.csv("DistCoeffTTDHS.csv")
CoeffTT <- CoeffTT[which(CoeffTT$country!=0),]
CoeffTT$dist_coeff_log <- CoeffTT$Coefficient
CoeffTT <- CoeffTT[,c("adminlevel", "country", "dist_coeff_log")]

## distance to neighboring village at 5km
load("NeighVillDHS.RData")
NeighVill <- data
NeighVill <- subset(NeighVill, select = -c(country, fixed_effects, district_id, distance_border, treatment5))

## merge with data
load("DataRobustDHS.RData")
data.reg <- merge(data.all, CoeffTT, by=c("adminlevel", "country"))
data.reg <- merge(data.reg, NeighVill, by=c("round", "adminlevel", "latitude", "longitude"), all.x=TRUE, all.y=FALSE)

rm(CoeffTT, NeighVill, data.all)

data.reg <- data.reg[which(data.reg$log.traveltime<10),]
## drop villages further than 30km away from nearest village on the other side (calculated for 5km)
data.reg <- data.reg[which(data.reg$distance_neigh5<30),]

## assign treatment using 5km bandwidth
data.reg$treatment <- data.reg$tt_treatment_log5
data.reg$treatment_int <- data.reg$tt_treatment_int_log5

## scale treatment by first stage coefficient
data.reg$treatment_int <- data.reg$treatment_int*(data.reg$dist_coeff_log*-1)

## inverse distance border for control group
data.reg$distance_border[which(data.reg$treatment==0)] <- - data.reg$distance_border[which(data.reg$treatment==0)]

## create interaction term
data.reg$dist_treat <- data.reg$treatment*data.reg$distance_border

data.reg$treatment_int <- scale(data.reg$treatment_int)
data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)
data.reg$development <- scale(data.reg$development)

model.traveltime <- felm(development ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

################################# Instrumented HQs ##################################
## load first stage effects
CoeffLog <- read.csv("DistCoeffLogDHS.csv")
CoeffLog <- CoeffLog[which(CoeffLog$country!=0),]
CoeffLog$dist_coeff_log <- CoeffLog$Coefficient
CoeffLog <- CoeffLog[,c("adminlevel", "country", "dist_coeff_log")]

## distance to neighboring village at 5km
load("NeighVillDHS.RData")
NeighVill <- data
NeighVill <- subset(NeighVill, select = -c(country, fixed_effects, district_id, distance_border, treatment5))

## merge with data
load("DataRobustDHS.RData")
data.reg <- merge(data.all, CoeffLog, by=c("adminlevel", "country"))
data.reg <- merge(data.reg, NeighVill, by=c("round", "adminlevel", "latitude", "longitude"), all.x=TRUE, all.y=FALSE)

rm(CoeffLog, NeighVill, data.all)

data.reg <- data.reg[which(data.reg$popdendist<75),]
## drop villages further than 30km away from nearest village on the other side (calculated for 5km)
data.reg <- data.reg[which(data.reg$distance_neigh5<30),]

## assign treatment using 5km bandwidth
data.reg$treatment <- data.reg$inst_treatment_log5
data.reg$treatment_int <- data.reg$inst_treatment_int_log5

## scale treatment by first stage coefficient
data.reg$treatment_int <- data.reg$treatment_int*(data.reg$dist_coeff_log*-1)

## inverse distance border for control group
data.reg$distance_border[which(data.reg$treatment==0)] <- - data.reg$distance_border[which(data.reg$treatment==0)]

## create interaction term
data.reg$dist_treat <- data.reg$treatment*data.reg$distance_border

data.reg$treatment_int <- scale(data.reg$treatment_int)
data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)
data.reg$development <- scale(data.reg$development)

model.instrumented <- felm(development ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

#################################### Placebo HQs ####################################
## load first stage effects
CoeffLog <- read.csv("DistCoeffLogDHS.csv")
CoeffLog <- CoeffLog[which(CoeffLog$country!=0),]
CoeffLog$dist_coeff_log <- CoeffLog$Coefficient
CoeffLog <- CoeffLog[,c("adminlevel", "country", "dist_coeff_log")]

## distance to neighboring village at 5km
load("NeighVillDHS.RData")
NeighVill <- data
NeighVill <- subset(NeighVill, select = -c(country, fixed_effects, district_id, distance_border, treatment5))

## merge with data
load("DataRobustDHS.RData")
data.reg <- merge(data.all, CoeffLog, by=c("adminlevel", "country"))
data.reg <- merge(data.reg, NeighVill, by=c("round", "adminlevel", "latitude", "longitude"), all.x=TRUE, all.y=FALSE)

rm(CoeffLog, NeighVill, data.all)

data.reg <- data.reg[which(data.reg$placebodist<75),]
## drop villages further than 30km away from nearest village on the other side (calculated for 5km)
data.reg <- data.reg[which(data.reg$distance_neigh5<30),]

## assign treatment using 5km bandwidth
data.reg$treatment <- data.reg$placebo_treatment_log5
data.reg$treatment_int <- data.reg$placebo_treatment_int_log5

## scale treatment by first stage coefficient
data.reg$treatment_int <- data.reg$treatment_int*(data.reg$dist_coeff_log*-1)

## inverse distance border for control group
data.reg$distance_border[which(data.reg$treatment==0)] <- - data.reg$distance_border[which(data.reg$treatment==0)]

## create interaction term
data.reg$dist_treat <- data.reg$treatment*data.reg$distance_border

data.reg$treatment_int <- scale(data.reg$treatment_int)
data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)
data.reg$development <- scale(data.reg$development)

model.placebo <- felm(development ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

###################################### Long/Lat #####################################
load("RegressionSampleDHS.RData")

data.reg <- data.reg[which(data.reg$distance_neigh5<30),]
data.reg <- data.reg[which(data.reg$distance_hq<75),]
data.reg$treatment_int <- data.reg$treatment_int_log5
data.reg$treatment_int <- data.reg$treatment_int*(data.reg$dist_coeff_log*-1)

data.reg$x <- data.reg$longitude
data.reg$y <- data.reg$latitude
data.reg$xy <- data.reg$x * data.reg$y
data.reg$x2 <- data.reg$x * data.reg$x
data.reg$y2 <- data.reg$y * data.reg$y

data.reg$treatment_int <- scale(data.reg$treatment_int)
data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)
data.reg$development <- scale(data.reg$development)

model.longlat <- felm(development ~ treatment_int*inst_chiefs_dum + x + y + x2 + y2 + xy + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg)

##################################### No scaling ####################################
load("RegressionSampleDHS.RData")

data.reg <- data.reg[which(data.reg$distance_neigh5<30),]
data.reg <- data.reg[which(data.reg$distance_hq<75),]
data.reg$treatment <- data.reg$treatment_log5
data.reg$treatment_int <- data.reg$treatment_int_log5
data.reg$distance_border[which(data.reg$treatment==0)] <- - data.reg$distance_border[which(data.reg$treatment==0)]
data.reg$dist_treat <- data.reg$treatment*data.reg$distance_border

data.reg$treatment_int <- scale(data.reg$treatment_int)
data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)
data.reg$development <- scale(data.reg$development)

model.noscaling <- felm(development ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

################################### Just Intensive ##################################
load("RegressionSampleDHS.RData")

data.reg <- data.reg[which(data.reg$distance_hq<75),]
data.reg <- data.reg[which(data.reg$distance_neigh5<30),]
data.reg$treatment <- data.reg$treatment_log5
data.reg$treatment_int <- data.reg$treatment_int_log5
data.reg$treatment_int <- data.reg$treatment_int*(data.reg$dist_coeff_log*-1)
data.reg$distance_border[which(data.reg$treatment==0)] <- - data.reg$distance_border[which(data.reg$treatment==0)]
data.reg$dist_treat <- data.reg$treatment_int*data.reg$distance_border

data.reg$treatment_int <- scale(data.reg$treatment_int)
data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)
data.reg$development <- scale(data.reg$development)

model.intensive <- felm(development ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)



#################################### Jittering #########################################
## DHS coordinates are scambled 
## (up to 2km for urban clusters and up to 5km for rural clusters with 1% of rural clusters scrambled up to 10km)
## https://dhsprogram.com/What-We-Do/GPS-Data-Collection.cfm
## The idea is to use the distance to the border to calculate what is the probability the respondent is in a different unit
## The assumption here is that the border goes perpendicular to the respondent
## http://mathworld.wolfram.com/CircularSegment.html
## http://mathworld.wolfram.com/Quarter-TankProblem.html

load("MainRegressionSampleDHS.RData")

data.reg$treatment_int <- scale(data.reg$treatment_int)
data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)
data.reg$development <- scale(data.reg$development)


data.reg$radius <- 5
data.reg$radius[which(data.reg$urbrur==1)] <- 2
data.reg$distance_border.abs <- abs(data.reg$distance_border)
data.reg$h <- data.reg$radius - data.reg$distance_border.abs


data.reg$jitter.weight <- 1 - ((data.reg$radius*data.reg$radius*acos((data.reg$radius-data.reg$h)/data.reg$radius)-(data.reg$radius-data.reg$h)*sqrt(2*data.reg$radius*data.reg$h-(data.reg$h*data.reg$h)))/(3.14159265359*data.reg$radius*data.reg$radius))
data.reg$jitter.weight[which(data.reg$urbrur==1 & data.reg$distance_border.abs>2)] <- 1


model.jitter <- felm(development ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, weights=data.reg$jitter.weight,data.reg)

####################################### Tables ######################################
stargazer(model.main, model.nocontrol, model.binary, model.noscaling, model.longlat, model.cluster, model.jitter, type="latex",
          title="Robustness: Different Specifications",
          keep=c("treatment_int", "inst_chiefs_dum:treatment_int", "treatment", "inst_chiefs_dum:treatment"),
          dep.var.labels = "Development Index",
          column.labels=c("Main", "No Controls", "Binary Treatment", "No Scaling", "Long/Lat", "Cluster", "Scramble"),
          covariate.labels=c("Low State Capacity Treatment", "Treatment X Institutionalized"),
          keep.stat=c("n","adj.rsq"),
          add.lines = list(c("Fixed effects?", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes","Yes"),
                           c("Cluster", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit")),
          font.size="footnotesize",
          column.sep.width="1pt",
          notes.label = "Clustered standard errors in parentheses",
          label="robustspecification_dhs",
          out="table_b3_robustspecification_dhs.tex"
)


stargazer(model.main, model.50km, model.dont30, model.nonlogged, model.traveltime, type="latex",
          title="Robustness: Different Measurement",
          keep=c("treatment_int", "inst_chiefs_dum:treatment_int"),
          dep.var.labels = "Development Index",
          column.labels=c("Main", "Drop 50km", "No Restriction", "Non-Logged", "Traveltime"),
          covariate.labels=c("Low State Capacity Treatment", "Treatment X Institutionalized"),
          keep.stat=c("n","adj.rsq"),
          add.lines = list(c("Fixed effects?", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Cluster", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit")),
          font.size="footnotesize",
          column.sep.width="1pt",
          notes.label = "Clustered standard errors in parentheses",
          label="robustoutlier_dhs",
          out="table_b4_robustoutlier_dhs.tex"
)

stargazer(model.main, model.distneigh, model.admin1, model.admin2, model.donut, model.ethnic, model.instrumented, model.placebo, type="latex",
          title="Robustness: Headquarters and Boundaries",
          keep=c("treatment_int", "inst_chiefs_dum:treatment_int", "treatment", "inst_chiefs_dum:treatment"),
          dep.var.labels = "Development Index",
          column.labels=c("Main", "Distance to Neigh HQ", "Admin 1", "Admin 2", "Donut RD", "Ethnicity FE", "Instrumented HQs", "Placebo"),
          covariate.labels=c("Low State Capacity Treatment", "Treatment X Institutionalized"),
          keep.stat=c("n","adj.rsq"),
          add.lines = list(c("Fixed effects?", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Cluster", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit")),
          font.size="footnotesize",
          column.sep.width="1pt",
          notes.label = "Clustered standard errors in parentheses",
          label="robustother_dhs",
          out="table_b5_robustother_dhs.tex"
)

#####################################################################################
############# Table B6: Robustness: Interaction with Country Variables ############# 
#####################################################################################

setwd(path)
load("InteractionRegressionSampleDHS.RData")


data.reg$treatment_int <- scale(data.reg$treatment_int)
data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)

data.reg$interactionterm <- data.reg$pop_1400
data.reg$interactionterm <- scale(data.reg$interactionterm)
model1 <- felm(development ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
data.reg$interactionterm <- data.reg$colony_gbr
data.reg$interactionterm <- scale(data.reg$interactionterm)
model2 <- felm(development ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
data.reg$interactionterm <- data.reg$legor_gbr
data.reg$interactionterm <- scale(data.reg$interactionterm)
model3 <- felm(development ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
data.reg$interactionterm <- data.reg$settler.colony
data.reg$interactionterm <- scale(data.reg$interactionterm)
model4 <- felm(development ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
data.reg$interactionterm <- data.reg$gemstones
data.reg$interactionterm <- scale(data.reg$interactionterm)
model5 <- felm(development ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
data.reg$interactionterm <- data.reg$rugged
data.reg$interactionterm <- scale(data.reg$interactionterm)
model6 <- felm(development ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
data.reg$interactionterm <- data.reg$AfricaMalaria
data.reg$interactionterm <- scale(data.reg$interactionterm)
model7 <- felm(development ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
data.reg$interactionterm <- data.reg$democracy_index
data.reg$interactionterm <- scale(data.reg$interactionterm)
model8 <- felm(development ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
data.reg$interactionterm <- data.reg$q_rule_law
data.reg$interactionterm <- scale(data.reg$interactionterm)
model9 <- felm(development ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

stargazer(model1, model2, model3, model4, model5, model6, model7, model8, model9, type="latex",
          title="Robustness: Interaction with Country Variables",
          keep=c("treatment_int", "inst_chiefs_dum:treatment_int", "inst_chiefs_dum:interactionterm:treatment_int"),
          dep.var.labels = "Development Index",
          column.labels=c("Pop. 1400", "Brit. Colony", "Brit. Legal", "Settler Colony", "Gemstones", "Ruggedness", "Malaria Suit.", "Dem. Index", "Q Rule of Law"),
          covariate.labels=c("Low Local State Capacity", "Treatment X Institutionalized", "Treatment X CountryVariable", "Treatment X Institutionalized X CountryVariable"),
          keep.stat=c("n","adj.rsq"),
          add.lines = list(c("Fixed effects?", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Cluster", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit")),
          font.size="scriptsize",
          column.sep.width="0pt",
          notes.label = "Clustered s.e. in parentheses",
          label="robustinter1_dhs",
          out="table_b6_robustinter1_dhs.tex"
)

