#####################################################################################
# Create all tables for appendix of the paper
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


######## Table A1 is created by hand from the administrative data
# which can be found on the author's website: soerenhenn.com


#######################################################################################
############### Table A2: Summary Statistics for Full Regression Sample ###############
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

stargazer(data.reg, type = "latex",
          title="Summary Statistics for Full Regression Sample",
          covariate.labels=c("Distance to Headquarter (km)", "Distance to Admin. Border (km)", "Distance to Village on Other Side (km)" , "Distance to Neighbouring HQ (km)", "Traveltime to HQ (in min)", "Treatment Intensity", "Urban", "Distance to National Capital (km)", "Distance to National Border", "Distance to Coast (km)", "Elevation", "Ruggedness", "Malaria Suitability", "Agricultural Suitability", "Distance to Christian Missions (km)", "Distance to Histroical Cities (km)", "Distance to Colonial Railroad (km)", "Admin. Unit Size (sqkm)", "Chief Z-score", "Chief Influence", "Trust in Chief", "Corrupt Chief (Inverse)", "Contact with Chief", "State Capacity Index", "Percentage of HH  with Electricity", "Percentage of Children Registered", "Average Time to Water (min)", "Literacy", "Wealth Index", "Infant Mortality", "Traditional Medicine", "Percentage of Kids Gone", "Percentage of Men Born in Location", "Percentage of Women Born in Location"),
          label="sumstat",
          digits=2,
          font.size="small",
          omit.summary.stat=c("p25", "p75"),
          out="table_a2_sumstat.tex"
)

#######################################################################################
#### Table A3:  Effect of Log Distance to HQ on Outcomes Related to State Capacity ####
#######################################################################################

load("RegressionSample.RData")

model1a <- felm(z_payment ~ log_distance_hq + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail | round + district_id |0| district_id, data.reg)
model2a <- felm(z_dev ~ log_distance_hq + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail| round + district_id |0| district_id, data.reg)
model3a <- felm(z_pg ~ log_distance_hq + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail | round + district_id |0| district_id, data.reg)
model4a <- felm(state_capacity ~ log_distance_hq + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail | round + district_id |0| district_id, data.reg)

## including DHS
load("RegressionSampleDHS.RData")

model1b <- felm(z_registered ~ log_distance_hq + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail | round + district_id |0| district_id, data.reg)
model2b <- felm(z_electricity ~ log_distance_hq + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail| round + district_id |0| district_id, data.reg)
model3b <- felm(z_time.to.water ~ log_distance_hq + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail | round + district_id |0| district_id, data.reg)
model4b <- felm(state_capacity ~ log_distance_hq + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail | round + district_id |0| district_id, data.reg)


stargazer(model1a, model2a, model3a, model4a, model1b, model2b, model3b, model4b, type="latex",
          title="Log Distance to HQ on outcomes related to state capacity",
          keep=c("log_distance_hq"),
          column.labels=c("Afronarometer", "DHS"),
          column.separate=c(4,4),
          dep.var.labels = c("Taxes paid", "Local Dev", "Public Goods", "State Capacity Index","Registered","Electricty", "Time to water source", "State Capacity Index"),
          covariate.labels=c("Log Distance to HQ"),
          keep.stat=c("n","adj.rsq"),
          add.lines = list(c("Fixed effects?", "Yes", "Yes", "Yes", "Yes","Yes", "Yes", "Yes", "Yes"),
                           c("Cluster", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit","Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit")),
          font.size="small",
          notes.label = "Clustered standard errors in parentheses",
          label="firststage",
          out="table_a3_firststage.tex")

######################################################################################
########## Table A4: Effect of State Capacity on Components of Chief Z-Score ##########
######################################################################################

load("MainRegressionSample.RData")

data.reg$treatment_int <- scale(data.reg$treatment_int)
data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)
data.reg$chief_zscore <- scale(data.reg$chief_zscore)
data.reg$z_influence_tradition <- scale(data.reg$z_influence_tradition)
data.reg$z_contact_tradition <- scale(data.reg$z_contact_tradition)
data.reg$z_trust_tradition <- scale(data.reg$z_trust_tradition)
data.reg$z_corrupt_traditiontemp <- scale(data.reg$z_corrupt_traditiontemp)
data.reg$perf_trad <- scale(data.reg$perf_trad)
data.reg$listen_trad <- scale(data.reg$listen_trad)

model2 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
model2a <- felm(z_influence_tradition ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
model2b <- felm(z_contact_tradition ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
model2c <- felm(z_trust_tradition ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
model2d <- felm(z_corrupt_traditiontemp ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
model2e <- felm(perf_trad ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
model2f <- felm(listen_trad ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

stargazer(model2, model2a, model2b, model2c, model2d, model2e, model2f, type="latex",
          title="Results for Components of Chief Z-Score",
          keep=c("treatment_int", "inst_chiefs_dum:treatment_int"),
          dep.var.labels = c("Chief Z-Score", "Influence of Chief", "Contact with Chief", "Trust in Chief", "Chief not Corrupt"),
          #column.labels=c("Not Protected", "Protected"),
          covariate.labels=c("Low State Capacity Treatment", "Treatment X Institutionalized"),
          keep.stat=c("n","adj.rsq"),
          add.lines = list(c("Fixed effects?", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes" ,"Yes"),
                           c("Cluster", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit")),
          font.size="scriptsize",
          column.sep.width="1pt",
          notes.label = "Clustered standard errors in parentheses",
          label="outcomes",
          out="table_a4_outcomes.tex"
)


######################################################################################
####### Table A5: Effect of State Capacity on Components of Development Index #######
######################################################################################
load("MainRegressionSampleDHS.RData")

data.reg$treatment_int <- scale(data.reg$treatment_int)
data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)

model.read <- felm(read.all ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
model.wealth <- felm(wealth.index ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
model.piped <- felm(z_piped.water ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

stargazer(model.read, model.wealth, model.piped, type="latex",
          title="Effect of State Presence on Components of Development Index",
          keep=c("treatment_int", "inst_chiefs_dum:treatment_int"),
          dep.var.labels = c("Literacy", "Wealth", "Piped Water"),
          covariate.labels=c("Low State Presence Treatment", "Treatment X Institutionalized"),
          keep.stat=c("n","adj.rsq"),
          add.lines = list(c("Fixed effects?", "Yes", "Yes", "Yes"),
                           c("Cluster", "Admin. Unit", "Admin. Unit", "Admin. Unit")),
          font.size="footnotesize",
          notes.label = "Clustered standard errors in parentheses",
          label="main_component",
          out="table_a5_main_component.tex")


############################################################################################################
## Table A6: Effect of Treatment on Historical and Geographical Controls using Afrobarometer and DHS Data ##
############################################################################################################

load("MainRegressionSample.RData")

data.reg[,"urban"] <- NA
data.reg[which(data.reg[,"urbrur"]==1),"urban"] <- 1
data.reg[which(data.reg[,"urbrur"]>1),"urban"] <- 0

data.reg.afro <- subset(data.reg, select = c(country, distance_hq, distance_border, distance_neigh5, neighbordist, traveltime, treatment_int, urban, distance_capital, distance_natborder, distcoast, elevation, ruggedness, malaria, agriculture, missions, cities, distrail, area_sqkm, chief_zscore, z_influence_tradition, z_trust_tradition, z_corrupt_traditiontemp, z_contact_tradition, state_capacity, inst_chiefs_dum, fixed_effects, district_id, dist_treat))
colnames(data.reg.afro)[colnames(data.reg.afro)=="distance_neigh5"] <- "distance_neigh"

load("MainRegressionSampleDHS.RData")

data.reg[,"urban"] <- NA
data.reg[which(data.reg[,"urbrur"]==1),"urban"] <- 1
data.reg[which(data.reg[,"urbrur"]>1),"urban"] <- 0

data.reg.dhs <- subset(data.reg, select = c(country, distance_hq, distance_border, distance_neigh5, treatment_int, urban, distance_capital, distance_natborder, distcoast, elevation, ruggedness, malaria, agriculture, missions, cities, distrail, area_sqkm, state_capacity, electricity, registered, time.to.water, read.all, wealth.index, infant.mortality, trad_med, kids.gone, men.always, wm.always, inst_chiefs_dum, fixed_effects, district_id, dist_treat))

data.reg <- bind_rows(data.reg.afro, data.reg.dhs)

model1 <- felm(distance_capital ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
model2 <- felm(distance_natborder ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
model3 <- felm(distcoast ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
model4 <- felm(elevation ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
model5 <- felm(ruggedness ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
model6 <- felm(agriculture ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
model7 <- felm(cities ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
model8 <- felm(malaria ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
model9 <- felm(missions ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
model10 <- felm(distrail ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
model11 <- felm(area_sqkm ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

stargazer(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, type="latex",
          title="Balance Table using Afrobarometer and DHS Data",
          keep=c("treatment_int", "inst_chiefs_dum:treatment_int"),
          dep.var.labels = c("Dist Capital", "Dist Nat Border", "Dist Coast", "Elevation", "Ruggedness", "Agriculture", "Hist Cities", "Malaria", "Missions", "Dist Rail"),
          covariate.labels=c("Low State Capacity Treatment", "Treatment X Institutionalized"),
          column.sep.width = "1pt",
          keep.stat=c("n","adj.rsq"),
          add.lines = list(c("Fixed effects?", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Cluster", "Admin Unit", "Admin Unit", "Admin Unit", "Admin Unit", "Admin Unit", "Admin Unit", "Admin Unit", "Admin Unit", "Admin Unit", "Admin Unit")),
          font.size="scriptsize",
          notes.label = "Clustered Standard errors in parentheses",
          label="balance",
          out="table_a6_balance.tex")







#####################################################################################
######### Table A7: Robustness: Different Measures of Institutional Context #########
#####################################################################################

load("MainRegressionSample.RData")

data.reg$treatment_int <- scale(data.reg$treatment_int)
data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)
data.reg$chief_zscore <- scale(data.reg$chief_zscore)

model.main <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)


model.included <- felm(chief_zscore ~ distance_border*inc_leaders_dum + dist_treat*inc_leaders_dum + treatment_int*inc_leaders_dum + distance_capital*inc_leaders_dum + distance_natborder*inc_leaders_dum + distcoast*inc_leaders_dum + elevation*inc_leaders_dum + ruggedness*inc_leaders_dum + agriculture*inc_leaders_dum + cities*inc_leaders_dum + malaria*inc_leaders_dum + missions*inc_leaders_dum + distrail*inc_leaders_dum + area_sqkm*inc_leaders_dum | fixed_effects |0| district_id, data.reg)
model.protected <- felm(chief_zscore ~ distance_border*protect_chiefs_dum + dist_treat*protect_chiefs_dum + treatment_int*protect_chiefs_dum + distance_capital*protect_chiefs_dum + distance_natborder*protect_chiefs_dum + distcoast*protect_chiefs_dum + elevation*protect_chiefs_dum + ruggedness*protect_chiefs_dum + agriculture*protect_chiefs_dum + cities*protect_chiefs_dum + malaria*protect_chiefs_dum + missions*protect_chiefs_dum + distrail*protect_chiefs_dum + area_sqkm*protect_chiefs_dum | fixed_effects |0| district_id, data.reg)
model.salary <- felm(chief_zscore ~ distance_border*salary_chiefs_dum + dist_treat*salary_chiefs_dum + treatment_int*salary_chiefs_dum + distance_capital*salary_chiefs_dum + distance_natborder*salary_chiefs_dum + distcoast*salary_chiefs_dum + elevation*salary_chiefs_dum + ruggedness*salary_chiefs_dum + agriculture*salary_chiefs_dum + cities*salary_chiefs_dum + malaria*salary_chiefs_dum + missions*salary_chiefs_dum + distrail*salary_chiefs_dum + area_sqkm*salary_chiefs_dum | fixed_effects |0| district_id, data.reg)


stargazer(model.main, model.included, model.protected, model.salary, type="latex",
          title="Robustness: Different Measures of Institutional Context",
          keep=c("treatment_int", "inst_chiefs_dum:treatment_int", "inc_leaders_dum:treatment_int", "protect_chiefs_dum:treatment_int", "salary_chiefs_dum:treatment_int"),
          dep.var.labels = "Chief Z-Score",
          covariate.labels=c("Low State Capacity Treatment", "Treatment X Institutionalized", "Treatment X Mentioned", "Treatment X Protected", "Treatment X Salary"),
          keep.stat=c("n","adj.rsq"),
          add.lines = list(c("Fixed effects?", "Yes", "Yes", "Yes", "Yes"),
                           c("Cluster", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit")),
          font.size="small",
          notes.label = "Clustered standard errors in parentheses",
          label="robustmeasure",
          out="table_a7_robustmeasure.tex"
)

######################################################################################
##################### Table A8: Effect of Treatment on Migration #####################
######################################################################################

load("MainRegressionSampleDHS.RData")

data.reg$treatment_int <- scale(data.reg$treatment_int)
data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)
data.reg$kids.gone <- scale(data.reg$kids.gone)
data.reg$men.always <- scale(data.reg$men.always)
data.reg$wm.always <- scale(data.reg$wm.always)
data.reg$migration_z <- scale(data.reg$migration_z)

model1a <- felm(kids.gone ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
model1b <- felm(men.always ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
model1c <- felm(wm.always ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
model1d <- felm(migration_z ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

stargazer(model1a, model1b, model1c, model1d, type="latex",
          title="Migration",
          keep=c("treatment_int", "inst_chiefs_dum:treatment_int"),
          dep.var.labels = c("Migration", "Migration", "Migration", "Migration"),
          column.labels = c("Children", "Men", "Women", "Z-score"),
          covariate.labels=c("Low State Capacity Treatment", "Treatment X Institutionalized"),
          keep.stat=c("n","adj.rsq"),
          add.lines = list(c("Fixed effects?", "Yes", "Yes", "Yes", "Yes"),
                           c("Cluster", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit")),
          font.size="footnotesize",
          notes.label = "Clustered standard errors in parentheses",
          label="migration",
          out="table_a8_migration.tex")










#####################################################################################
##################### Table A9-A11: Robustness of Afrobarometer #####################
#####################################################################################
load("MainRegressionSample.RData")

data.reg$treatment_int <- scale(data.reg$treatment_int)
data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)
data.reg$chief_zscore <- scale(data.reg$chief_zscore)


################################# Main Specification ################################
model.main <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

###################################### OLS Only #####################################
model.ols <- felm(chief_zscore ~ log_distance_hq*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)


##################################### Urban Only ####################################
model.urban <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg, subset = urbrur==1)


################################### Drop Outliers ###################################
model.100km <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg, subset = distance_hq<100)
model.50km <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg, subset = distance_hq<50)

################################## Cluster Higher ###################################
model.cluster <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_cluster, data.reg)


########################## Controlling for Dist to Neighbor #########################
model.distneigh <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum + neighbordist*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

###################################### By Admin #####################################
model.admin1 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg, subset = adminlevel==1)
model.admin2 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg, subset = adminlevel==2)

################################### Just Extensive ##################################
model.binary <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

################################# Absolute Distance #################################
## scale distance by first stage coefficient
data.reg$log_distance_hq_scale <- data.reg$log_distance_hq*(data.reg$dist_coeff_log*-1)
data.reg$log_distance_hq_scale <- scale(data.reg$log_distance_hq_scale)
data.reg$log_distance_hq_scale <- data.reg$log_distance_hq_scale - min(data.reg$log_distance_hq_scale, na.rm = TRUE)
data.reg$log_distance_hq_scale[which(data.reg$treatment==0)] <- 0

model.absolute <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + log_distance_hq_scale*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
model.absolute2 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + log_distance_hq*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

hist(data.reg$log_distance_hq_scale)
hist(data.reg$treatment_int)
################################# Regional Dummies ##################################
## this shouldn't make a difference because of the border fixed effects
model.regions <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum + africa_region_s*inst_chiefs_dum + africa_region_w*inst_chiefs_dum + africa_region_n*inst_chiefs_dum + africa_region_e*inst_chiefs_dum + africa_region_c*inst_chiefs_dum| fixed_effects |0| district_id, data.reg)

##################################### No Controls ###################################
model.nocontrol <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

############################### British Colonies only ###############################
model.british <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg, subset=colony_gbr==1)

############################ Ethnic Homeland Fixed Effects ##########################
model.ethnic <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects + NAME |0| district_id, data.reg)

###################################### "Donout" #####################################
data.reg$distance_border.abs <- abs(data.reg$distance_border)
model.donut <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg, subset = distance_border.abs>1)

############################## "Drop Non-DHS Countries" #############################
load("MainRegressionSample.RData")

data.reg <- data.reg[which(data.reg$country!=3),]
data.reg <- data.reg[which(data.reg$country!=9),]
data.reg <- data.reg[which(data.reg$country!=11),]
data.reg <- data.reg[which(data.reg$country!=13),]
data.reg <- data.reg[which(data.reg$country!=26),]

data.reg$treatment_int <- scale(data.reg$treatment_int)
data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)
data.reg$chief_zscore <- scale(data.reg$chief_zscore)

model.alldhs <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

##################################### Rural Only ####################################
load("RegressionSample.RData")

data.reg <- data.reg[which(data.reg$distance_hq<150),]
data.reg$treatment <- data.reg$treatment_log5
data.reg$treatment_int <- data.reg$treatment_int_log5
data.reg$treatment_int <- data.reg$treatment_int*(data.reg$dist_coeff_log*-1)
data.reg$distance_border[which(data.reg$treatment==0)] <- - data.reg$distance_border[which(data.reg$treatment==0)]
data.reg$dist_treat <- data.reg$treatment*data.reg$distance_border

data.reg$treatment_int <- scale(data.reg$treatment_int)
data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)
data.reg$chief_zscore <- scale(data.reg$chief_zscore)


model.rural <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg, subset = urbrur!=1)

##################################### Non_Logged ####################################
load("RegressionSample.RData")

data.reg <- data.reg[which(data.reg$distance_hq<150),]
data.reg <- data.reg[which(data.reg$distance_neigh5<30),]
data.reg$treatment <- data.reg$treatment5
data.reg$treatment_int <- data.reg$treatment_int5
data.reg$treatment_int <- data.reg$treatment_int*(data.reg$dist_coeff*-1)
data.reg$distance_border[which(data.reg$treatment==0)] <- - data.reg$distance_border[which(data.reg$treatment==0)]
data.reg$dist_treat <- data.reg$treatment*data.reg$distance_border

data.reg$treatment_int <- scale(data.reg$treatment_int)
data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)
data.reg$chief_zscore <- scale(data.reg$chief_zscore)

model.nonlogged <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

################################## Don't drop 30km ##################################
load("RegressionSample.RData")

data.reg <- data.reg[which(data.reg$distance_hq<150),]
data.reg$treatment <- data.reg$treatment_log5
data.reg$treatment_int <- data.reg$treatment_int_log5
data.reg$treatment_int <- data.reg$treatment_int*(data.reg$dist_coeff_log*-1)
data.reg$distance_border[which(data.reg$treatment==0)] <- - data.reg$distance_border[which(data.reg$treatment==0)]
data.reg$dist_treat <- data.reg$treatment*data.reg$distance_border

data.reg$treatment_int <- scale(data.reg$treatment_int)
data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)
data.reg$chief_zscore <- scale(data.reg$chief_zscore)

model.dont30 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

#################################### Traveltime #####################################
## load first stage effects
CoeffTT <- read.csv("DistCoeffTT.csv")
CoeffTT <- CoeffTT[which(CoeffTT$country!=0),]
CoeffTT$dist_coeff_log <- CoeffTT$Coefficient
CoeffTT <- CoeffTT[,c("adminlevel", "country", "dist_coeff_log")]

## distance to neighboring village at 5km
load("NeighVill.RData")
NeighVill <- data
NeighVill <- subset(NeighVill, select = -c(country, fixed_effects, district_id, distance_border, treatment5))

## merge with data
load("DataRobust.RData")
data.reg <- merge(data.all, CoeffTT, by=c("adminlevel", "country"))
data.reg <- merge(data.reg, NeighVill, by=c("round", "adminlevel", "latitude", "longitude"), all.x=TRUE, all.y=FALSE)

rm(CoeffTT, NeighVill, data.all)

data.reg <- data.reg[which(data.reg$log.traveltime<10),]
## drop villages further than 30km away from nearest village on the other side (calculated for 5km)
data.reg <- data.reg[which(data.reg$distance_neigh5<30),]

## drop CIV second admin
data.reg$drop <- 0
data.reg$drop[which(data.reg$adminlevel==2 & data.reg$country==8)] <- 1
data.reg <- data.reg[which(data.reg$drop==0),]

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
data.reg$chief_zscore <- scale(data.reg$chief_zscore)

model.traveltime <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

################################# Instrumented HQs ##################################
## load first stage effects
CoeffLog <- read.csv("DistCoeffLog.csv")
CoeffLog <- CoeffLog[which(CoeffLog$country!=0),]
CoeffLog$dist_coeff_log <- CoeffLog$Coefficient
CoeffLog <- CoeffLog[,c("adminlevel", "country", "dist_coeff_log")]

## distance to neighboring village at 5km
load("NeighVill.RData")
NeighVill <- data
NeighVill <- subset(NeighVill, select = -c(country, fixed_effects, district_id, distance_border, treatment5))

## merge with data
load("DataRobust.RData")
data.reg <- merge(data.all, CoeffLog, by=c("adminlevel", "country"))
data.reg <- merge(data.reg, NeighVill, by=c("round", "adminlevel", "latitude", "longitude"), all.x=TRUE, all.y=FALSE)

rm(CoeffLog, NeighVill, data.all)

data.reg <- data.reg[which(data.reg$popdendist<150),]
## drop villages further than 30km away from nearest village on the other side (calculated for 5km)
data.reg <- data.reg[which(data.reg$distance_neigh5<30),]

## drop CIV second admin
data.reg$drop <- 0
data.reg$drop[which(data.reg$adminlevel==2 & data.reg$country==8)] <- 1
data.reg <- data.reg[which(data.reg$drop==0),]

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
data.reg$chief_zscore <- scale(data.reg$chief_zscore)

model.instrumented <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

#################################### Placebo HQs ####################################
## load first stage effects
CoeffLog <- read.csv("DistCoeffLog.csv")
CoeffLog <- CoeffLog[which(CoeffLog$country!=0),]
CoeffLog$dist_coeff_log <- CoeffLog$Coefficient
CoeffLog <- CoeffLog[,c("adminlevel", "country", "dist_coeff_log")]

## distance to neighboring village at 5km
load("NeighVill.RData")
NeighVill <- data
NeighVill <- subset(NeighVill, select = -c(country, fixed_effects, district_id, distance_border, treatment5))

## merge with data
load("DataRobust.RData")
data.reg <- merge(data.all, CoeffLog, by=c("adminlevel", "country"))
data.reg <- merge(data.reg, NeighVill, by=c("round", "adminlevel", "latitude", "longitude"), all.x=TRUE, all.y=FALSE)

rm(CoeffLog, NeighVill, data.all)

data.reg <- data.reg[which(data.reg$placebodist<150),]
## drop villages further than 30km away from nearest village on the other side (calculated for 5km)
data.reg <- data.reg[which(data.reg$distance_neigh5<30),]

## drop CIV second admin
data.reg$drop <- 0
data.reg$drop[which(data.reg$adminlevel==2 & data.reg$country==8)] <- 1
data.reg <- data.reg[which(data.reg$drop==0),]

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
data.reg$chief_zscore <- scale(data.reg$chief_zscore)

model.placebo <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

###################################### Long/Lat #####################################
load("RegressionSample.RData")

data.reg <- data.reg[which(data.reg$distance_neigh5<30),]
data.reg <- data.reg[which(data.reg$distance_hq<150),]
data.reg$treatment_int <- data.reg$treatment_int_log5
data.reg$treatment_int <- data.reg$treatment_int*(data.reg$dist_coeff_log*-1)

data.reg$x <- data.reg$longitude
data.reg$y <- data.reg$latitude
data.reg$xy <- data.reg$x * data.reg$y
data.reg$x2 <- data.reg$x * data.reg$x
data.reg$y2 <- data.reg$y * data.reg$y

data.reg$treatment_int <- scale(data.reg$treatment_int)
data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)
data.reg$chief_zscore <- scale(data.reg$chief_zscore)

model.longlat <- felm(chief_zscore ~ treatment_int*inst_chiefs_dum + x + y + x2 + y2 + xy + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg)


##################################### No scaling ####################################
load("RegressionSample.RData")

data.reg <- data.reg[which(data.reg$distance_neigh5<30),]
data.reg <- data.reg[which(data.reg$distance_hq<150),]
data.reg$treatment <- data.reg$treatment_log5
data.reg$treatment_int <- data.reg$treatment_int_log5
data.reg$distance_border[which(data.reg$treatment==0)] <- - data.reg$distance_border[which(data.reg$treatment==0)]
data.reg$dist_treat <- data.reg$treatment*data.reg$distance_border

data.reg$treatment_int <- scale(data.reg$treatment_int)
data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)
data.reg$chief_zscore <- scale(data.reg$chief_zscore)

model.noscaling <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

################################### Just Intensive ##################################
load("RegressionSample.RData")

data.reg <- data.reg[which(data.reg$distance_hq<150),]
data.reg <- data.reg[which(data.reg$distance_neigh5<30),]
data.reg$treatment <- data.reg$treatment_log5
data.reg$treatment_int <- data.reg$treatment_int_log5
data.reg$treatment_int <- data.reg$treatment_int*(data.reg$dist_coeff_log*-1)
data.reg$distance_border[which(data.reg$treatment==0)] <- - data.reg$distance_border[which(data.reg$treatment==0)]
data.reg$dist_treat <- data.reg$treatment_int*data.reg$distance_border

data.reg$treatment_int <- scale(data.reg$treatment_int)
data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)
data.reg$chief_zscore <- scale(data.reg$chief_zscore)

model.intensive <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)



################################### Drop missassigned ##################################
load("MainRegressionSampleNEW.RData")

data.reg$treatment.ALT <- as.numeric((data.reg$distance_hq-data.reg$nei_dist5)>0)
table(data.reg$treatment5,data.reg$treatment.ALT)
data.reg <- data.reg[-which(data.reg$treatment.ALT==1 & data.reg$treatment5==0),]
data.reg <- data.reg[-which(data.reg$treatment.ALT==0 & data.reg$treatment5==1),]
table(data.reg$treatment5,data.reg$treatment.ALT)

data.reg$treatment_int <- scale(data.reg$treatment_int)
data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)
data.reg$chief_zscore <- scale(data.reg$chief_zscore)

model.dropmis <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
summary(model.dropmis)


####################################### Tables ######################################


## Table A9: Robustness: Different Specifications
stargazer(model.main, model.nocontrol, model.binary, model.absolute, model.noscaling, model.longlat, model.cluster, model.dropmis, type="latex",
          title="Robustness: Different Specifications",
          keep=c("treatment_int", "inst_chiefs_dum:treatment_int", "treatment", "inst_chiefs_dum:treatment", "log_distance_hq", "inst_chiefs_dum:log_distance_hq"),
          dep.var.labels = "Chief Z-Score",
          column.labels=c("Main", "No Controls", "Binary Treatment", "Absolute Distance", "No Scaling", "Long/Lat", "Cluster", "Drop Misassigned"),
          covariate.labels=c("Low State Capacity Treatment", "Treatment X Institutionalized"),
          keep.stat=c("n","adj.rsq"),
          add.lines = list(c("Fixed effects?", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Cluster", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit")),
          font.size="footnotesize",
          column.sep.width="1pt",
          notes.label = "Clustered standard errors in parentheses",
          label="robustspecification",
          out="table_a9_robustspecification.tex"
)

## Table A10: Robustness: Different Measurement
stargazer(model.main, model.100km, model.50km, model.dont30, model.nonlogged, model.traveltime, model.rural, type="latex",
          title="Robustness: Different Measurement",
          keep=c("treatment_int", "inst_chiefs_dum:treatment_int"),
          dep.var.labels = "Chief Z-Score",
          column.labels=c("Main", "Drop 100km", "Drop 50km", "No Restriction", "Non-Logged", "Traveltime", "Rural Only"),
          covariate.labels=c("Low State Capacity Treatment", "Treatment X Institutionalized"),
          keep.stat=c("n","adj.rsq"),
          add.lines = list(c("Fixed effects?", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Cluster", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit")),
          font.size="footnotesize",
          column.sep.width="1pt",
          notes.label = "Clustered standard errors in parentheses",
          label="robustoutlier",
          out="table_a10_robustoutlier.tex"
)

## Table A11: Robustness: Headquarters and Boundaries
stargazer(model.main, model.distneigh, model.admin1, model.admin2, model.donut, model.ethnic, model.instrumented, model.placebo, type="latex",
          title="Robustness: Headquarters and Boundaries",
          keep=c("treatment_int", "inst_chiefs_dum:treatment_int", "treatment", "inst_chiefs_dum:treatment"),
          dep.var.labels = "Chief Z-Score",
          column.labels=c("Main", "Distance to Neigh HQ", "Admin 1", "Admin 2", "Donut RD", "Ethnicity FE", "Instrumented HQs", "Placebo"),
          covariate.labels=c("Low State Capacity Treatment", "Treatment X Institutionalized"),
          keep.stat=c("n","adj.rsq"),
          add.lines = list(c("Fixed effects?", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Cluster", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit")),
          font.size="footnotesize",
          column.sep.width="1pt",
          notes.label = "Clustered standard errors in parentheses",
          label="robustother",
          out="table_a11_robustother.tex"
)

## Table A12: Additional Robustness
stargazer(model.main, model.british, model.alldhs, type="latex",
          title="Additional Robustness",
          keep=c("treatment_int", "inst_chiefs_dum:treatment_int"),
          dep.var.labels = "Chief Z-Score",
          column.labels=c("Main", "British Colonies", "Drop Non-DHS Countries"),
          covariate.labels=c("Low State Capacity Treatment", "Treatment X Institutionalized"),
          keep.stat=c("n","adj.rsq"),
          add.lines = list(c("Fixed effects?", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Cluster", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit")),
          font.size="footnotesize",
          column.sep.width="1pt",
          notes.label = "Clustered standard errors in parentheses",
          label="robustextra",
          out="table_a12_robustextra.tex"
)


#######################################################################################
################ Table A13: Covariate Balance - Country-Level Variables ################
#######################################################################################
load("RegressionSample.RData")

data.table <- summaryBy(inst_chiefs_dum + slave_exports + pop_1400 + logem4 + european_descent + colony_gbr + legor_gbr + settler.colony + gemstones + soil + near_coast + land_area + rugged + oil.production.2000 + q_rule_law + rgdppc_1950_m + fsi.2006 + democracy_index + year_ind + violent_ind + AfricaMalaria + hist_central_country + length_km + tax.gdp.2010 + pol_decentralization ~ country, data=data.reg, keep.names = T)

data.table0 <- data.table[which(data.table$inst_chiefs_dum==0),]
data.table1 <- data.table[which(data.table$inst_chiefs_dum==1),]

tests <- list()
tests[[1]] <- t.test(data.table0$hist_central_country, y=data.table1$hist_central_country,na.rm=TRUE)
tests[[2]] <- t.test(data.table0$year_ind, y=data.table1$year_ind,na.rm=TRUE)
tests[[3]] <- t.test(data.table0$violent_ind, y=data.table1$violent_ind,na.rm=TRUE)
tests[[4]] <- t.test(data.table0$slave_exports, y=data.table1$slave_exports,na.rm=TRUE)
tests[[5]] <- t.test(data.table0$pop_1400, y=data.table1$pop_1400,na.rm=TRUE)
tests[[6]] <- t.test(data.table0$logem4, y=data.table1$logem4,na.rm=TRUE)
tests[[7]] <- t.test(data.table0$colony_gbr, y=data.table1$colony_gbr,na.rm=TRUE)
tests[[8]] <- t.test(data.table0$legor_gbr, y=data.table1$legor_gbr,na.rm=TRUE)
tests[[9]] <- t.test(data.table0$settler.colony, y=data.table1$settler.colony,na.rm=TRUE)
tests[[10]] <- t.test(data.table0$length_km, y=data.table1$length_km,na.rm=TRUE)
tests[[11]] <- t.test(data.table0$gemstones, y=data.table1$gemstones,na.rm=TRUE)
tests[[12]] <- t.test(data.table0$soil, y=data.table1$soil,na.rm=TRUE)
tests[[13]] <- t.test(data.table0$near_coast, y=data.table1$near_coast,na.rm=TRUE)
tests[[14]] <- t.test(data.table0$land_area, y=data.table1$land_area,na.rm=TRUE)
tests[[15]] <- t.test(data.table0$rugged, y=data.table1$rugged,na.rm=TRUE)
tests[[16]] <- t.test(data.table0$oil.production.2000, y=data.table1$oil.production.2000,na.rm=TRUE)
tests[[17]] <- t.test(data.table0$AfricaMalaria, y=data.table1$AfricaMalaria,na.rm=TRUE)
tests[[18]] <- t.test(data.table0$q_rule_law, y=data.table1$q_rule_law,na.rm=TRUE)
tests[[19]] <- t.test(data.table0$rgdppc_1950_m, y=data.table1$rgdppc_1950_m,na.rm=TRUE)
tests[[20]] <- t.test(data.table0$fsi.2006, y=data.table1$fsi.2006,na.rm=TRUE)
tests[[21]] <- t.test(data.table0$tax.gdp.2010, y=data.table1$tax.gdp.2010,na.rm=TRUE)
tests[[22]] <- t.test(data.table0$democracy_index, y=data.table1$democracy_index,na.rm=TRUE)
tests[[23]] <- t.test(data.table0$pol_decentralization, y=data.table1$pol_decentralization,na.rm=TRUE)


options(scipen=999)
try <- t(sapply(tests, function(x) {
  c(x$estimate[1],
    x$estimate[2],
    p.value = x$p.value)
}))

covariates <- c("Historical Centralization",
                "Year of Indpendence",
                "Violent Independence?",
                "Slave Exports",
                "Population in 1400",
                "Log Settler Mortality",
                "British Colony",
                "British Legal Origins",
                "Settler Colony",
                "Colonial Railroads (km)",
                "Gemstones",
                "Soil Quality",
                "Average Distance to Coast",
                "Land area (1000 Ha)",
                "Ruggedness",
                "Oil Production in 2000",
                "Malaria Suitability",
                "Rule of Law",
                "GDP 1950",
                "Failed State Index 2006",
                "Taxes as % of GDP 2010",
                "Democracy Index 2017",
                "Political Decentralization")

n_not <- c(sum(!is.na(data.table0$hist_central_country)),
           sum(!is.na(data.table0$year_ind)),
           sum(!is.na(data.table0$violent_ind)),
           sum(!is.na(data.table0$slave_exports)),
           sum(!is.na(data.table0$pop_1400)),
           sum(!is.na(data.table0$logem4)),
           sum(!is.na(data.table0$colony_gbr)),
           sum(!is.na(data.table0$legor_gbr)),
           sum(!is.na(data.table0$settler.colony)),
           sum(!is.na(data.table0$length_km)),
           sum(!is.na(data.table0$gemstones)),
           sum(!is.na(data.table0$soil)),
           sum(!is.na(data.table0$near_coast)),
           sum(!is.na(data.table0$land_area)),
           sum(!is.na(data.table0$rugged)),
           sum(!is.na(data.table0$oil.production.2000)),
           sum(!is.na(data.table0$AfricaMalaria)),
           sum(!is.na(data.table0$q_rule_law)),
           sum(!is.na(data.table0$rgdppc_1950_m)),
           sum(!is.na(data.table0$fsi.2006)),
           sum(!is.na(data.table0$tax.gdp.2010)),
           sum(!is.na(data.table0$democracy_index)),
           sum(!is.na(data.table0$pol_decentralization)))

n_inst <- c(sum(!is.na(data.table1$hist_central_country)),
            sum(!is.na(data.table1$year_ind)),
            sum(!is.na(data.table1$violent_ind)),
            sum(!is.na(data.table1$slave_exports)),
            sum(!is.na(data.table1$pop_1400)),
            sum(!is.na(data.table1$logem4)),
            sum(!is.na(data.table1$colony_gbr)),
            sum(!is.na(data.table1$legor_gbr)),
            sum(!is.na(data.table1$settler.colony)),
            sum(!is.na(data.table1$length_km)),
            sum(!is.na(data.table1$gemstones)),
            sum(!is.na(data.table1$soil)),
            sum(!is.na(data.table1$near_coast)),
            sum(!is.na(data.table1$land_area)),
            sum(!is.na(data.table1$rugged)),
            sum(!is.na(data.table1$oil.production.2000)),
            sum(!is.na(data.table1$AfricaMalaria)),
            sum(!is.na(data.table1$q_rule_law)),
            sum(!is.na(data.table1$rgdppc_1950_m)),
            sum(!is.na(data.table1$fsi.2006)),
            sum(!is.na(data.table1$tax.gdp.2010)),
            sum(!is.na(data.table1$democracy_index)),
            sum(!is.na(data.table1$pol_decentralization)))
table_a8 <- cbind(covariates, n_not)

table_a8 <- cbind(covariates, n_not, try[,1], n_inst, try[,2], try[,3])

print(xtable(table_a8, type = "latex", digits=2), file = "table_a13_covbalance.tex")


######################################################################################
############ Table A14-15: Robustness: Interaction with Country Variables ############
######################################################################################
load("MainRegressionSample.RData")

data.reg$treatment_int <- scale(data.reg$treatment_int)
data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)



data.reg$interactionterm <- data.reg$hist_central_country
data.reg$interactionterm <- scale(data.reg$interactionterm)
model1 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
data.reg$interactionterm <- data.reg$year_ind
data.reg$interactionterm <- scale(data.reg$interactionterm)
model2 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
data.reg$interactionterm <- data.reg$violent_ind
data.reg$interactionterm <- scale(data.reg$interactionterm)
model3 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
data.reg$interactionterm <- data.reg$slave_exports
data.reg$interactionterm <- scale(data.reg$interactionterm)
model4 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
data.reg$interactionterm <- data.reg$logem4
data.reg$interactionterm <- scale(data.reg$interactionterm)
model5 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
data.reg$interactionterm <- data.reg$length_km
data.reg$interactionterm <- scale(data.reg$interactionterm)
model6 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
data.reg$interactionterm <- data.reg$soil
data.reg$interactionterm <- scale(data.reg$interactionterm)
model7 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
data.reg$interactionterm <- data.reg$near_coast
data.reg$interactionterm <- scale(data.reg$interactionterm)
model8 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
data.reg$interactionterm <- data.reg$land_area
data.reg$interactionterm <- scale(data.reg$interactionterm)
model9 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
data.reg$interactionterm <- data.reg$oil.production.2000
data.reg$interactionterm <- scale(data.reg$interactionterm)
model10 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
data.reg$interactionterm <- data.reg$rgdppc_1950_m
data.reg$interactionterm <- scale(data.reg$interactionterm)
model11 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
data.reg$interactionterm <- data.reg$yr_sch
data.reg$interactionterm <- scale(data.reg$interactionterm)
model12 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
data.reg$interactionterm <- data.reg$fsi.2006
data.reg$interactionterm <- scale(data.reg$interactionterm)
model13 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
data.reg$interactionterm <- data.reg$tax.gdp.2010
data.reg$interactionterm <- scale(data.reg$interactionterm)
model14 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

stargazer(model1, model2, model3, model4, model5, model6, model7, type="latex",
          title="Robustness: Interaction with Country Variables",
          keep=c("treatment_int", "inst_chiefs_dum:treatment_int", "inst_chiefs_dum:interactionterm:treatment_int"),
          dep.var.labels = "Chief Z-Score",
          column.labels=c("Hist. Central.", "Year Indep.", "Violent Indep.", "Slave Export", "Settler Mortality", "Colonial Rail", "Soil Quality"),
          covariate.labels=c("Low Local State Capacity", "Treatment X Institutionalized", "Treatment X CountryVariable", "Treatment X Institutionalized X CountryVariable"),
          keep.stat=c("n","adj.rsq"),
          add.lines = list(c("Fixed effects?", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Cluster", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit")),
          font.size="scriptsize",
          column.sep.width="0pt",
          notes.label = "Clustered s.e. in parentheses",
          label="robustinter2",
          out="table_a14_robustinter2.tex"
)

stargazer(model8, model9, model10, model11, model12, model13, model14, type="latex",
          title="Robustness: Interaction with Country Variables",
          keep=c("treatment_int", "inst_chiefs_dum:treatment_int", "inst_chiefs_dum:interactionterm:treatment_int"),
          dep.var.labels = "Chief Z-Score",
          column.labels=c("Near Coast", "Land Area", "Oil Production", "RGDP 1950", "Years Schooling","Fragile State Index", "Tax Revenue over GDP"),
          covariate.labels=c("Low Local State Capacity", "Treatment X Institutionalized", "Treatment X CountryVariable", "Treatment X Institutionalized X CountryVariable"),
          keep.stat=c("n","adj.rsq"),
          add.lines = list(c("Fixed effects?", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Cluster", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit")),
          font.size="scriptsize",
          column.sep.width="0pt",
          notes.label = "Clustered s.e. in parentheses",
          label="robustinter3",
          out="table_a15_robustinter3.tex"
)

