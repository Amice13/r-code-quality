#####################################################################################
# Create all tables for main part of the paper
# Soeren Henn
# 2022-08-15
#####################################################################################


rm(list=ls())

## Set working directory




library(lfe)        # to run linear fixed effects models
library(stargazer)  # to output tables


#####################################################################################
############### Table 1:  Treatment on State Capacity and Development ###############
#####################################################################################
## Specification: RDD specification (1)
## Dependent Variable: State Presence index from Afrobarometer and DHS data

# Afrobarometer
load("MainRegressionSample.RData")

data.reg$treatment_int <- scale(data.reg$treatment_int)
data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)
data.reg$state_capacity <- scale(data.reg$state_capacity)

model1a <- felm(state_capacity ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg)

# DHS
load("MainRegressionSampleDHS.RData")

data.reg$treatment_int <- scale(data.reg$treatment_int)
data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)
data.reg$state_capacity <- scale(data.reg$state_capacity)

model1b <- felm(state_capacity ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg)

stargazer(model1a, model1b, type="latex",
          title="Regression results on local state capacity index",
          keep=c("treatment_int"),
          dep.var.labels = c("State Capacity Index", "Development Index"),
          column.labels=c("Afrobarometer", "DHS", "DHS"),
          covariate.labels=c("Low State Capacity Treatment"),
          keep.stat=c("n","adj.rsq"),
          add.lines = list(c("Fixed effects?", "Yes", "Yes", "Yes"),
                           c("Cluster", "Admin. Unit", "Admin. Unit", "Admin. Unit")),
          font.size="small",
          notes.label = "Clustered Standard errors in parentheses",
          label="measure",
          out="table_1_measure.tex")



######################################################################################
############# Table 2: Effect of State Capacity on Perceptions of Chiefs #############
######################################################################################

########## OLS correlation
load("RegressionSample.RData")
data.reg$chief_zscore <- scale(data.reg$chief_zscore)

model.correlation <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + log_distance_hq*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | district_id |0| district_id, data.reg)
summary(model.correlation)

########## Binary Treatment Indicator, No FE or clustering
load("MainRegressionSample.RData")
data.reg$chief_zscore <- scale(data.reg$chief_zscore)

model.binary <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment*inst_chiefs_dum  | 0 |0| 0, data.reg)

########## Binary Treatment Indicator, with FE and clustering
model.binary2 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment*inst_chiefs_dum  | fixed_effects |0| district_id, data.reg)

########## Intensive Treatment Indicator
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

model.noscaling <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

########## Add covariates
model.noscaling2 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

########## Main Specification 
load("MainRegressionSample.RData")

data.reg$treatment_int <- scale(data.reg$treatment_int)
data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)
data.reg$chief_zscore <- scale(data.reg$chief_zscore)

model.main <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

stargazer(model.correlation, model.binary, model.binary2, model.noscaling, model.noscaling2, model.main, type="latex",
          title="Effect of State Presence on Chief Power",
          keep=c("treatment_int", "inst_chiefs_dum:treatment_int", "treatment", "inst_chiefs_dum:treatment", "log_distance_hq", "inst_chiefs_dum:log_distance_hq"),
          dep.var.labels = "Chief Z-Score",
          column.labels=c("OLS","Binary Treatment", "Fixed Effects", "Intensive Treatment", "Controls", "Scaling"),
          covariate.labels=c("Low State Capacity Treatment", "Treatment X Institutionalized"),
          keep.stat=c("n","adj.rsq"),
          add.lines = list(c("Fixed effects?", "Yes", "No", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Cluster","Admin. Unit", "No", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit")),
          font.size="footnotesize",
          column.sep.width="1pt",
          notes.label = "Clustered standard errors in parentheses",
          label="main_spec",
          out="table_2_main.tex"
)


##############################################################################################
## Table 3:  Effect of State Capacity on Perceptions of Chiefs by Institutional Integration ##
##############################################################################################
## Run specification (1) for the full sample and then separately for countries where
## traditional authorities are recognized and where they are not

load("MainRegressionSample.RData")

# Pooled Sample
data.reg$treatment_int <- scale(data.reg$treatment_int)
data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)
data.reg$chief_zscore <- scale(data.reg$chief_zscore)

model1 <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg)
model2 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

# Not recognized
data.reg1 <- data.reg[which(data.reg$inst_chiefs_dum==0),]
data.reg1$treatment_int <- scale(data.reg1$treatment_int)
data.reg1$treatment_int <- data.reg1$treatment_int - min(data.reg1$treatment_int, na.rm = TRUE)
data.reg1$chief_zscore <- scale(data.reg1$chief_zscore)
model3a <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg1)

# Recognized
data.reg2 <- data.reg[which(data.reg$inst_chiefs_dum==1),]
data.reg2$treatment_int <- scale(data.reg2$treatment_int)
data.reg2$treatment_int <- data.reg2$treatment_int - min(data.reg2$treatment_int, na.rm = TRUE)
data.reg2$chief_zscore <- scale(data.reg2$chief_zscore)
model3b <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg2)

stargazer(model1, model2, model3a, model3b, type="latex",
          title="Main Regression Results",
          keep=c("treatment_int", "inst_chiefs_dum:treatment_int"),
          dep.var.labels = "Chief Z-Score",
          column.labels=c("Pooled Sample", "Pooled Sample", "Not Institutionalized", "Institutionalized"),
          covariate.labels=c("Low State Capacity Treatment", "Treatment X Institutionalized"),
          keep.stat=c("n","adj.rsq"),
          add.lines = list(c("Fixed effects?", "Yes", "Yes", "Yes", "Yes"),
                           c("Cluster", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit")),
          font.size="footnotesize",
          column.sep.width="1pt",
          notes.label = "Clustered Standard errors in parentheses",
          label="main_inst",
          out="table_3_main_inst.tex"
)


######################################################################################
################## Table 4: Effect of State Capacity on Development ##################
######################################################################################
## Specifications (1) and (2)
## Dependent Variable: Development Index from DHS data

load("MainRegressionSampleDHS.RData")

data.reg$treatment_int <- scale(data.reg$treatment_int)
data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)

model.average <- felm(development ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg)
model.main <- felm(development ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)

stargazer(model.average, model.main, type="latex",
          title="Effect of State Presence on Development",
          keep=c("treatment_int", "inst_chiefs_dum:treatment_int"),
          dep.var.labels = "Development Index",
          covariate.labels=c("Low State Presence Treatment", "Treatment X Institutionalized"),
          keep.stat=c("n","adj.rsq"),
          add.lines = list(c("Fixed effects?", "Yes", "Yes"),
                           c("Cluster", "Admin. Unit", "Admin. Unit")),
          font.size="small",
          notes.label = "Standard errors in parentheses",
          label="main_development",
          out="table_4_development.tex")







#####################################################################################
############## Table 5: Robustness: Interaction with Country Variables ##############
#####################################################################################

load("MainRegressionSample.RData")

data.reg$treatment_int <- scale(data.reg$treatment_int)
data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)

data.reg$interactionterm <- data.reg$pop_1400
data.reg$interactionterm <- scale(data.reg$interactionterm)
model1 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
data.reg$interactionterm <- data.reg$colony_gbr
data.reg$interactionterm <- scale(data.reg$interactionterm)
model2 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
data.reg$interactionterm <- data.reg$legor_gbr
data.reg$interactionterm <- scale(data.reg$interactionterm)
model3 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
data.reg$interactionterm <- data.reg$settler.colony
data.reg$interactionterm <- scale(data.reg$interactionterm)
model4 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
data.reg$interactionterm <- data.reg$gemstones
data.reg$interactionterm <- scale(data.reg$interactionterm)
model5 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
data.reg$interactionterm <- data.reg$rugged
data.reg$interactionterm <- scale(data.reg$interactionterm)
model6 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
data.reg$interactionterm <- data.reg$AfricaMalaria
data.reg$interactionterm <- scale(data.reg$interactionterm)
model7 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
data.reg$interactionterm <- data.reg$democracy_index
data.reg$interactionterm <- scale(data.reg$interactionterm)
model8 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
data.reg$interactionterm <- data.reg$q_rule_law
data.reg$interactionterm <- scale(data.reg$interactionterm)
model9 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
data.reg$interactionterm <- data.reg$pol_decentralization
data.reg$interactionterm <- scale(data.reg$interactionterm)
model10 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_border*interactionterm + dist_treat*interactionterm + treatment_int*interactionterm + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)


stargazer(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, type="latex",
          title="Robustness: Interaction with Country Variables",
          keep=c("treatment_int", "inst_chiefs_dum:treatment_int", "inst_chiefs_dum:interactionterm:treatment_int"),
          dep.var.labels = "Chief Z-Score",
          column.labels=c("Pop. 1400", "Brit. Colony", "Brit. Legal", "Settler Colony", "Gemstones", "Ruggedness", "Malaria Suit.", "Dem. Index", "Q Rule of Law", "Pol. Decentralization"),
          covariate.labels=c("Low Local State Capacity", "Treatment X Institutionalized", "Treatment X CountryVariable", "Treatment X Institutionalized X CountryVariable"),
          keep.stat=c("n","adj.rsq"),
          add.lines = list(c("Fixed effects?", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Cluster", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit", "Admin. Unit")),
          font.size="scriptsize",
          column.sep.width="0pt",
          notes.label = "Clustered s.e. in parentheses",
          label="robustinter",
          out="table_5_robustinter.tex"
)



