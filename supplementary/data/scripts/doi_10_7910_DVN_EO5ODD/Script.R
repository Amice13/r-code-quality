
################################################################################
################ Leader similarity and international sanctions #################
################################################################################
######################## Gutmann / Langer / Neuenkirch #########################
################################################################################
########## Accepted for Publication at Journal of Conflict Resolution ##########
################################################################################

################################### Packages ###################################

if(!require(haven)) { install.packages("haven"); library(haven) }
if(!require(readxl)) { install.packages("readxl"); library(readxl) }
if(!require(writexl)) { install.packages("writexl"); library(writexl) }
if(!require(tidyverse)) { install.packages("tidyverse"); library(tidyverse) }
if(!require(purrr)) { install.packages("purrr"); library(purrr) }
if(!require(tidyr)) { install.packages("tidyr"); library(tidyr) }
if(!require(dplyr)) { install.packages("dplyr"); library(dplyr) }
if(!require(reshape2)) { install.packages("reshape2"); library(reshape2) }
if(!require(fixest)) { install.packages("fixest"); library(fixest) }
if(!require(bife)) { install.packages("bife"); library(bife) }
if(!require(plm)) { install.packages("plm"); library(plm) }
if(!require(glm2)) { install.packages("glm2"); library(glm2) }
if(!require(sandwich)) { install.packages("sandwich"); library(sandwich) }
if(!require(mfx)) { install.packages("mfx"); library(mfx) }
if(!require(car)) { install.packages("car"); library(car) }
if(!require(xtable)) { install.packages("xtable"); library(xtable) }
if(!require(MNP)) { install.packages("MNP"); library(MNP) }
if(!require(ordinal)) { install.packages("ordinal"); library(ordinal) }
if(!require(stats)) { install.packages("stats"); library(stats) }
if(!require(remotes)) { install.packages("remotes"); library(remotes) }

## Set Workdir to Source File Location
## setwd("...")

##################################### Data #####################################

data_all <- readRDS("data.rds")
data_exclUN <- readRDS("data_exclUN.rds")
data_exclEU <- readRDS("data_exclUNEU.rds")

coups <- readRDS("data_coups.rds")
affinity_combined <- readRDS("data_affinity.rds")

############################### Model Estimation ###############################

## Function Calculation Marginal Effects
calculate_marginal_effects <- function(model) {
  # Extract coefficients
  coefficients <- coef(model)
  
  # Predict the linear predictor (latent variable)
  linear_predictor <- predict(model, type = "link")
  
  # Calculate the density of the normal distribution at each predicted value
  density_normal <- dnorm(linear_predictor)
  
  # Calculate the average density
  average_density <- mean(density_normal)
  
  # Compute the average marginal effects for the Probit model
  average_marginal_effects_probit <- coefficients * average_density
  
  # Extract the variance-covariance matrix of the coefficients
  vcov_matrix <- vcov(model)
  
  # Calculate standard errors for the marginal effects using the Delta method
  standard_errors <- sqrt(diag(vcov_matrix)) * average_density
  
  # Compute z-values for the marginal effects
  z_values <- average_marginal_effects_probit / standard_errors
  
  # Determine significance levels
  significance <- cut(abs(z_values), breaks = c(0, 1.65, 1.96, 2.58, Inf),
                      labels = c("", "*", "**", "***"), right = FALSE)
  
  # Create a data frame with the results
  results <- data.frame(
    Variable = names(coefficients),
    Marginal_Effect = average_marginal_effects_probit,
    Standard_Error = standard_errors,
    Significance = significance
  )
  
  return(results)
}

## Function Calculation Marginal Effects after Bias Error Correction
calculate_marginal_effects_bife <- function(bias_corrected_model) {
  # Extract bias-corrected coefficients
  coefficients <- coef(bias_corrected_model)
  
  # Predict the linear predictor (latent variable) using bias-corrected model
  linear_predictor <- predict(bias_corrected_model, type = "link")
  
  # Calculate the density of the normal distribution at each predicted value
  density_normal <- dnorm(linear_predictor)
  
  # Calculate the average density
  average_density <- mean(density_normal)
  
  # Compute the average marginal effects for the Probit model
  average_marginal_effects_probit <- coefficients * average_density
  
  # Extract the variance-covariance matrix of the bias-corrected model
  vcov_matrix <- vcov(bias_corrected_model)
  
  # Calculate standard errors for the marginal effects using the Delta method
  standard_errors <- sqrt(diag(vcov_matrix)) * average_density
  
  # Compute z-values for the marginal effects
  z_values <- average_marginal_effects_probit / standard_errors
  
  # Determine significance levels
  significance <- cut(abs(z_values), breaks = c(0, 1.65, 1.96, 2.58, Inf),
                      labels = c("", "*", "**", "***"), right = FALSE)
  
  # Create a data frame with the results
  results <- data.frame(
    Variable = names(coefficients),
    Marginal_Effect = average_marginal_effects_probit,
    Standard_Error = standard_errors,
    Z_Value = z_values,
    Significance = significance
  )
  
  return(results)
}

################################################################################
################################ Narrow Models #################################
################################################################################

#################################### Table 1 ###################################

# Table 1, Column 1
probit_exclEU_sy <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                            log(lagged_pop_T) + log(lagged_rgdppc_T) +
                            lagged_KOFGI_T |
                            sender_year,
                          family = "probit",
                          cluster = "sender_year",
                          data = data_exclEU)
summary(probit_exclEU_sy)
calculate_marginal_effects(probit_exclEU_sy)

# Table 1, Column 2
probit_exclEU_both <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                              log(lagged_pop_T) + log(lagged_rgdppc_T) +
                              lagged_KOFGI_T |
                              country_pair + sender_year,
                            family = "probit",
                            cluster = "country_pair",
                            data = data_exclEU)
summary(probit_exclEU_both)
calculate_marginal_effects(probit_exclEU_both)

# Table 1, Column 3
probit_exclUN_sy <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                            log(lagged_pop_T) + log(lagged_rgdppc_T) +
                            lagged_KOFGI_T |
                            sender_year,
                          family = "probit",
                          cluster = "sender_year",
                          data = data_exclUN)
summary(probit_exclUN_sy)
calculate_marginal_effects(probit_exclUN_sy)

# Table 1, Column 4
probit_exclUN_both <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                              log(lagged_pop_T) + log(lagged_rgdppc_T) +
                              lagged_KOFGI_T |
                              country_pair + sender_year,
                            family = "probit",
                            cluster = "country_pair",
                            data = data_exclUN)
summary(probit_exclUN_both)
calculate_marginal_effects(probit_exclUN_both)

# Table 1, Column 5
probit_sy <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                     log(lagged_pop_T) + log(lagged_rgdppc_T) +
                     lagged_KOFGI_T |
                     sender_year,
                   family = "probit",
                   cluster = "sender_year",
                   data = data_all)
summary(probit_sy)
calculate_marginal_effects(probit_sy)

# Table 1, Column 6
probit_both <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                       log(lagged_pop_T) + log(lagged_rgdppc_T) +
                       lagged_KOFGI_T |
                       country_pair + sender_year,
                     family = "probit",
                     cluster = "country_pair",
                     data = data_all)
summary(probit_both)
calculate_marginal_effects(probit_both)

sy_data <- data_all[-abs(probit_sy$obs_selection$obsRemoved), ]
sy_data_exclUN <- data_exclUN[-abs(probit_exclUN_sy$obs_selection$obsRemoved), ]
sy_data_exclEU <- data_exclEU[-abs(probit_exclEU_sy$obs_selection$obsRemoved), ]

both_data <- data_all[-abs(probit_both$obs_selection$obsRemoved), ]
both_data_exclUN <- data_exclUN[-abs(probit_exclUN_both$obs_selection$obsRemoved), ]
both_data_exclEU <- data_exclEU[-abs(probit_exclEU_both$obs_selection$obsRemoved), ]

################################### Table 3 ####################################

data_exclEU <- data_exclEU %>%
  mutate(objective_split = objective) %>%
  separate(objective_split,
           into = c("objective_1", "objective_2", "objective_3", "objective_4"),
           sep = ",", extra = "drop", fill = "right")

data_exclEU$objective_1 <- dplyr::recode(data_exclEU$objective_1,
                                         "0" = "0",
                                         "democracy" = "1",
                                         "destab_regime" = "2",
                                         "end_war" = "3",
                                         "human_rights" = "4",
                                         "policy_change" = "5",
                                         "prevent_war" = "6",
                                         "territorial_conflict" = "7",
                                         "terrorism" = "8",
                                         "other" = "9")
data_exclEU$objective_1 <- as.numeric(data_exclEU$objective_1)

data_exclEU$objective_2 <- dplyr::recode(data_exclEU$objective_2,
                                         "0" = "0",
                                         "democracy" = "1",
                                         "destab_regime" = "2",
                                         "end_war" = "3",
                                         "human_rights" = "4",
                                         "policy_change" = "5",
                                         "prevent_war" = "6",
                                         "territorial_conflict" = "7",
                                         "terrorism" = "8",
                                         "other" = "9")
data_exclEU <- data_exclEU %>%
  mutate(objective_2 = ifelse(is.na(objective_2), 0, as.numeric(objective_2)))

data_exclEU$objective_3 <- dplyr::recode(data_exclEU$objective_3,
                                         "0" = "0",
                                         "democracy" = "1",
                                         "destab_regime" = "2",
                                         "end_war" = "3",
                                         "human_rights" = "4",
                                         "policy_change" = "5",
                                         "prevent_war" = "6",
                                         "territorial_conflict" = "7",
                                         "terrorism" = "8",
                                         "other" = "9")
data_exclEU <- data_exclEU %>%
  mutate(objective_3 = ifelse(is.na(objective_3), 0, as.numeric(objective_3)))

data_exclEU$objective_4 <- dplyr::recode(data_exclEU$objective_4,
                                         "0" = "0",
                                         "democracy" = "1",
                                         "destab_regime" = "2",
                                         "end_war" = "3",
                                         "human_rights" = "4",
                                         "policy_change" = "5",
                                         "prevent_war" = "6",
                                         "territorial_conflict" = "7",
                                         "terrorism" = "8",
                                         "other" = "9")
data_exclEU <- data_exclEU %>%
  mutate(objective_4 = ifelse(is.na(objective_4), 0, as.numeric(objective_4)))

data_exclEU_democ <- subset(data_exclEU, objective_1 != 1 & objective_2 != 1 & objective_3 != 1 & objective_4 != 1) # democracy
data_exclEU_destab <- subset(data_exclEU, objective_1 != 2 & objective_2 != 2 & objective_3 != 2 & objective_4 != 2) # destab_regime
data_exclEU_endwar <- subset(data_exclEU, objective_1 != 3 & objective_2 != 3 & objective_3 != 3 & objective_4 != 3) # end_war
data_exclEU_hr <- subset(data_exclEU, objective_1 != 4 & objective_2 != 4 & objective_3 != 4 & objective_4 != 4) # human_rights
data_exclEU_policychange <- subset(data_exclEU, objective_1 != 5 & objective_2 != 5 & objective_3 != 5 & objective_4 != 5) # policy_change
data_exclEU_preventwar <- subset(data_exclEU, objective_1 != 6 & objective_2 != 6 & objective_3 != 6 & objective_4 != 6) # prevent_war
data_exclEU_terrconfl <- subset(data_exclEU, objective_1 != 7 & objective_2 != 7 & objective_3 != 7 & objective_4 != 7) # territorial_conflict
data_exclEU_terror <- subset(data_exclEU, objective_1 != 8 & objective_2 != 8 & objective_3 != 8 & objective_4 != 8) # terrorism
data_exclEU_other <- subset(data_exclEU, objective_1 != 9 & objective_2 != 9 & objective_3 != 9 & objective_4 != 9) # other

# Table 3, Excl. "Democracy", narrow
probit_exclEU_both_democ <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                    log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                    lagged_KOFGI_T |
                                    country_pair + sender_year,
                                  family = "probit",
                                  cluster = "country_pair",
                                  glm.iter = 200,
                                  data = data_exclEU_democ)
summary(probit_exclEU_both_democ)
calculate_marginal_effects(probit_exclEU_both_democ)

# Table 3, Excl. "Human Rights", narrow
probit_exclEU_both_hr <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                 log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                 lagged_KOFGI_T |
                                 country_pair + sender_year,
                               family = "probit",
                               cluster = "country_pair",
                               glm.iter = 200,
                               data = data_exclEU_hr)
summary(probit_exclEU_both_hr)
calculate_marginal_effects(probit_exclEU_both_hr)

# Table 3, Excl. "Destab. Regime", narrow
probit_exclEU_both_destab <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                     log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                     lagged_KOFGI_T |
                                     country_pair + sender_year,
                                   family = "probit",
                                   cluster = "country_pair",
                                   glm.iter = 200,
                                   data = data_exclEU_destab)
summary(probit_exclEU_both_destab)
calculate_marginal_effects(probit_exclEU_both_destab)

# Table 3, Excl. "Policy Change", narrow
probit_exclEU_both_policychange <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                           log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                           lagged_KOFGI_T |
                                           country_pair + sender_year,
                                         family = "probit",
                                         cluster = "country_pair",
                                         glm.iter = 200,
                                         data = data_exclEU_policychange)
summary(probit_exclEU_both_policychange)
calculate_marginal_effects(probit_exclEU_both_policychange)

# Table 3, Excl. "Prevent War", narrow
probit_exclEU_both_preventwar <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                         log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                         lagged_KOFGI_T |
                                         country_pair + sender_year,
                                       family = "probit",
                                       cluster = "country_pair",
                                       glm.iter = 200,
                                       data = data_exclEU_preventwar)
summary(probit_exclEU_both_preventwar)
calculate_marginal_effects(probit_exclEU_both_preventwar)

# Table 3, Excl. "End War", narrow
probit_exclEU_both_endwar <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                     log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                     lagged_KOFGI_T |
                                     country_pair + sender_year,
                                   family = "probit",
                                   cluster = "country_pair",
                                   glm.iter = 200,
                                   data = data_exclEU_endwar)
summary(probit_exclEU_both_endwar)
calculate_marginal_effects(probit_exclEU_both_endwar)

# Table 3, Excl. "Territ. Conflict", narrow
probit_exclEU_both_terrconfl <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                        log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                        lagged_KOFGI_T |
                                        country_pair + sender_year,
                                      family = "probit",
                                      cluster = "country_pair",
                                      glm.iter = 200,
                                      data = data_exclEU_terrconfl)
summary(probit_exclEU_both_terrconfl)
calculate_marginal_effects(probit_exclEU_both_terrconfl)

# Table 3, Excl. "Terrorism", narrow
probit_exclEU_both_terror <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                     log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                     lagged_KOFGI_T |
                                     country_pair + sender_year,
                                   family = "probit",
                                   cluster = "country_pair",
                                   glm.iter = 200,
                                   data = data_exclEU_terror)
summary(probit_exclEU_both_terror)
calculate_marginal_effects(probit_exclEU_both_terror)

# Table 3, Excl. "Other Objectives", narrow
probit_exclEU_both_otherobj <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                    log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                    lagged_KOFGI_T |
                                    country_pair + sender_year,
                                  family = "probit",
                                  cluster = "country_pair",
                                  glm.iter = 200,
                                  data = data_exclEU_other)
summary(probit_exclEU_both_otherobj)
calculate_marginal_effects(probit_exclEU_both_otherobj)

################################### Table 4 ####################################

#### Cold War Split
data_exclEU_coldwar <- subset(data_exclEU, Year <= 1991)
data_exclEU_postcoldwar <- subset(data_exclEU, Year > 1991)

# Table 4, Excl. Post-Cold War Sanctions, narrow
probit_exclEU_both_coldwar <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                      log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                      lagged_KOFGI_T |
                                      country_pair + sender_year,
                                    glm.iter = 200,
                                    family = "probit",
                                    cluster = "country_pair",
                                    data = data_exclEU_coldwar)
summary(probit_exclEU_both_coldwar)
calculate_marginal_effects(probit_exclEU_both_coldwar)

# Table 4, Excl. Cold War Sanctions, narrow
probit_exclEU_both_postcoldwar <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                          log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                          lagged_KOFGI_T |
                                          country_pair + sender_year,
                                        glm.iter = 200,
                                        family = "probit",
                                        cluster = "country_pair",
                                        data = data_exclEU_postcoldwar)
summary(probit_exclEU_both_postcoldwar)
calculate_marginal_effects(probit_exclEU_both_postcoldwar)

data_exclEU <- data_exclEU %>%
  mutate(cold_war_dummy = ifelse(Year <= 1991, 1, 0))

#### Excluding Sanctions against Democracies
data_exclEU_excldemoc <- data_exclEU[!(data_exclEU$democracy_T_dummy == 1 & data_exclEU$sanctions == 1), ]

# Table 4, Excl. Sanctions ag. Democracies, narrow
probit_exclEU_both_excldemoc <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                        log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                        lagged_KOFGI_T |
                                        country_pair + sender_year,
                                      family = "probit",
                                      cluster = "country_pair",
                                      data = data_exclEU_excldemoc)
summary(probit_exclEU_both_excldemoc)
calculate_marginal_effects(probit_exclEU_both_excldemoc)

#### Excluding US Sanctions
## Prepare data
us_sanctions_IDs <- c(3,4,5,6,8,11,13,17,19,23,25,27,30,32,34,36,41,43,44,46,50,52,57,58,60,61,64,66,68,69,70,72,73,74,75,77,81,83,84,86,88,95,96,98,
                      100,101,111,112,115,124,125,129,130,134,137,138,139,142,144,146,148,149,150,153,165,178,181,183,189,190,192,
                      194,195,196,197,199,200,201,202,203,206,208,209,210,211,212,215,216,220,221,223,224,227,228,229,
                      234,235,236,237,238,240,243,244,251,252,253,254,255,256,258,259,261,273,275,277,278,284,285,287,
                      294,295,297,298,302,303,304,310,311,312,315,324,326,328,337,338,339,340,341,342,347,348,349,351,
                      354,355,356,363,365,367,368,370,371,373,376,378,381,383,384,387,390,394,397,398,404,416,417,421,422,
                      424,427,430,434,436,441,455,457,461,464,467,472,475,478,485,487,493,494,495,497,509,511,512,513,
                      516,519,521,525,528,529,538,539,543,544,548,554,555,556,557,559,560,566,572,574,582,587,588,593,
                      596,600,606,607,615,620,623,627,628,634,637,640,643,654,661,662,665,667,672,677,679,680,684,685,686,696,700,
                      703,704,706,707,708,709,710,711,712,713,717,720,723,724,728,730,731,736,739,752,764,765,766,770,
                      783,787,788,789,790,791,794,796,800,803,806,807,812,814,815,818,830,831,834,841,844,850,860,868,
                      873,875,876,878,879,881,887,889,893,895,904,906,913,919,920,921,927,937,941,942,948,951,956,957,
                      961,964,966,971,974,979,987,988,1005,1008,1012,1023,1030,1033,1037,1041,1042,1044,1047,1049,1050,
                      1051,1053,1054,1055,1056,1057,1058,1059,1060,1061,1062,1064,1065,1072,1073,1074,1075,1076,1080,1081,
                      1082,1083,1085,1086,1087,1088,1089,1090,1094,1095,1096,1098,1099,1100,1101,1102,1103,1104,1107,1109,
                      1110,1111,1112,1113,1114,1115,1116,1117,1118,1119,1120,1121,1122,1123,1125,1126,1127,1129,1130,1131,
                      1132,1133,1135,1136,1140,1141,1142,1143,1144,1147,1149,1150,1151,1152,1153,1154,1158,1159,1160,1162,
                      1163,1164,1165,1168,1169,1170,1171,1172,1173,1174,1176,1177,1178,1182,1183,1184,1186,1188,1191,1192,
                      1196,1198,1203,1204,1206,1207,1208,1209,1213,1214,1215,1217,1218,1221,1222,1224,1230,1234,1236,1238,
                      1242,1245,1247,1251,1255,1257,1261,1264,1265,1272,1273,1277,1278,1279,1282,1283,1284,1285,1286,1287,
                      1288,1289,1290,1294,1295,1296,1314,1315,1316,1317,1318,1319,1320,1321,1323)

contains_us_sanctions_ID <- function(us_ids) {
  any(as.integer(us_ids) %in% us_sanctions_IDs)
}

data_exclEU$contains_us_sanctions <- sapply(data_exclEU$sanctions_case_id_list, contains_us_sanctions_ID)
data_exclEU_exclUS <- data_exclEU[!data_exclEU$contains_us_sanctions, ]

# Table 4, Excl. US Sanctions, narrow
probit_exclEU_exclUS_both <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                     log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                     lagged_KOFGI_T |
                                     country_pair + sender_year,
                                   family = "probit",
                                   cluster = "country_pair",
                                   data = data_exclEU_exclUS)
summary(probit_exclEU_exclUS_both)
calculate_marginal_effects(probit_exclEU_exclUS_both)

#### Excluding Trade Sanctions
data_exclEU_excltrade <- data_exclEU[data_exclEU$trade != 1, ]

# Table 4, Excl. Trade Sanctions, narrow
probit_exclEU_both_excltrade <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                        log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                        lagged_KOFGI_T |
                                        country_pair + sender_year,
                                      family = "probit",
                                      cluster = "country_pair",
                                      data = data_exclEU_excltrade)
summary(probit_exclEU_both_excltrade)
calculate_marginal_effects(probit_exclEU_both_excltrade)

################################### Table OA1 ##################################

# Table OA1, Column 1
probit_exclEU_sy_bife <- bife(sanctions ~ weighted_lead_sim_score_nd +
                                log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                lagged_KOFGI_T |
                                sender_year,
                              model = "probit",
                              data = data_exclEU)
summary(probit_exclEU_sy_bife)

probit_exclEU_sy_bife_bias <- bias_corr(probit_exclEU_sy_bife)
probit_exclEU_sy_bife_bias_coef <- coef(probit_exclEU_sy_bife_bias)
calculate_marginal_effects_bife(probit_exclEU_sy_bife_bias)

################################### Table OA2 ##################################

# Table OA2, Column 1
lpm_exclEU_sy <- feols(sanctions ~ weighted_lead_sim_score_nd +
                         log(lagged_pop_T) + log(lagged_rgdppc_T) +
                         lagged_KOFGI_T |
                         sender_year,
                       cluster = "sender_year",
                       data = sy_data_exclEU)
summary(lpm_exclEU_sy)

# Table OA2, Column 3
lpm_exclEU_both <- feols(sanctions ~ weighted_lead_sim_score_nd +
                           log(lagged_pop_T) + log(lagged_rgdppc_T) +
                           lagged_KOFGI_T |
                           country_pair + sender_year,
                         cluster = "country_pair",
                         data = both_data_exclEU)
summary(lpm_exclEU_both)

################################### Table OA4 ##################################

# Table OA4, Column 1, Excl. Military Experience
probit_exclEU_both_milexp <- feglm(sanctions ~ weighted_lead_sim_score_milexp +
                                     log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                     lagged_KOFGI_T |
                                     country_pair + sender_year,
                                   family = "probit",
                                   cluster = "country_pair",
                                   data = data_exclEU)
summary(probit_exclEU_both_milexp)
calculate_marginal_effects(probit_exclEU_both_milexp)

# Table OA4, Column 1, Excl. Education
probit_exclEU_both_educ <- feglm(sanctions ~ weighted_lead_sim_score_educ +
                                   log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                   lagged_KOFGI_T |
                                   country_pair + sender_year,
                                 family = "probit",
                                 cluster = "country_pair",
                                 data = data_exclEU)
summary(probit_exclEU_both_educ)
calculate_marginal_effects(probit_exclEU_both_educ)

# Table OA4, Column 1, Excl. Childhood Family
probit_exclEU_both_childfamily <- feglm(sanctions ~ weighted_lead_sim_score_childfamily +
                                          log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                          lagged_KOFGI_T |
                                          country_pair + sender_year,
                                        family = "probit",
                                        cluster = "country_pair",
                                        data = data_exclEU)
summary(probit_exclEU_both_childfamily)
calculate_marginal_effects(probit_exclEU_both_childfamily)

# Table OA4, Column 1, Excl. Adult Family
probit_exclEU_both_adultfamily <- feglm(sanctions ~ weighted_lead_sim_score_adultfamily +
                                          log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                          lagged_KOFGI_T |
                                          country_pair + sender_year,
                                        family = "probit",
                                        cluster = "country_pair",
                                        data = data_exclEU)
summary(probit_exclEU_both_adultfamily)
calculate_marginal_effects(probit_exclEU_both_adultfamily)

# Table OA4, Column 1, Excl. Occupation
probit_exclEU_both_occupation <- feglm(sanctions ~ weighted_lead_sim_score_occupation +
                                         log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                         lagged_KOFGI_T |
                                         country_pair + sender_year,
                                       family = "probit",
                                       cluster = "country_pair",
                                       data = data_exclEU)
summary(probit_exclEU_both_occupation)
calculate_marginal_effects(probit_exclEU_both_occupation)

# Table OA4, Column 1, Excl. Political Experience
probit_exclEU_both_polexp <- feglm(sanctions ~ weighted_lead_sim_score_polexp +
                                     log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                     lagged_KOFGI_T |
                                     country_pair + sender_year,
                                   family = "probit",
                                   cluster = "country_pair",
                                   data = data_exclEU)
summary(probit_exclEU_both_polexp)
calculate_marginal_effects(probit_exclEU_both_polexp)

# Table OA4, Column 1, Excl. Other
probit_exclEU_both_other <- feglm(sanctions ~ weighted_lead_sim_score_other +
                                    log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                    lagged_KOFGI_T |
                                    country_pair + sender_year,
                                  family = "probit",
                                  cluster = "country_pair",
                                  data = data_exclEU)
summary(probit_exclEU_both_other)
calculate_marginal_effects(probit_exclEU_both_other)

# Table OA4, Column 1, Theory-Based
probit_exclEU_both_theory <- feglm(sanctions ~ weighted_lead_sim_score_theory +
                                     log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                     lagged_KOFGI_T |
                                     country_pair + sender_year,
                                   family = "probit",
                                   cluster = "country_pair",
                                   data = data_exclEU)
summary(probit_exclEU_both_theory)
calculate_marginal_effects(probit_exclEU_both_theory)

################################### Table OA5 ##################################

# Table OA5, Column 1
probit_exclEU_both_gower <- feglm(sanctions ~ weighted_lead_sim_score_gower +
                                    log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                    lagged_KOFGI_T |
                                    country_pair + sender_year,
                                  family = "probit",
                                  cluster = "country_pair",
                                  data = data_exclEU)
summary(probit_exclEU_both_gower)
calculate_marginal_effects(probit_exclEU_both_gower)

# Table OA5, Column 3
lpm_exclEU_both_gower <- feols(sanctions ~ weighted_lead_sim_score_gower +
                                 log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                 lagged_KOFGI_T |
                                 country_pair + sender_year,
                               cluster = "country_pair",
                               data = both_data_exclEU)
summary(lpm_exclEU_both_gower)

################################################################################
################################ Broad Models ##################################
################################################################################

##################################### Data #####################################

data_all <- merge(data_all, coups, by = c("country_T", "Year"), all.x = TRUE)
data_exclUN <- merge(data_exclUN, coups, by = c("country_T", "Year"), all.x = TRUE)
data_exclEU <- merge(data_exclEU, coups, by = c("country_T", "Year"), all.x = TRUE)

data_all <- data_all %>%
  group_by(country_T) %>%
  mutate(lagged_democracy_T_dummy = dplyr::lag(democracy_T_dummy, n = 1, default = 0))
data_all <- data_all %>%
  group_by(country_T) %>%
  mutate(lagged_Human_Rights_T = dplyr::lag(Human_Rights_T, n = 1, default = 0))
data_all <- data_all %>%
  group_by(country_T) %>%
  mutate(lagged_minor_conflict_T = dplyr::lag(minor_conflict_T, n = 1, default = 0))
data_all <- data_all %>%
  group_by(country_T) %>%
  mutate(lagged_major_conflict_T = dplyr::lag(major_conflict_T, n = 1, default = 0))

data_exclUN <- data_exclUN %>%
  group_by(country_T) %>%
  mutate(lagged_democracy_T_dummy = dplyr::lag(democracy_T_dummy, n = 1, default = 0))
data_exclUN <- data_exclUN %>%
  group_by(country_T) %>%
  mutate(lagged_Human_Rights_T = dplyr::lag(Human_Rights_T, n = 1, default = 0))
data_exclUN <- data_exclUN %>%
  group_by(country_T) %>%
  mutate(lagged_minor_conflict_T = dplyr::lag(minor_conflict_T, n = 1, default = 0))
data_exclUN <- data_exclUN %>%
  group_by(country_T) %>%
  mutate(lagged_major_conflict_T = dplyr::lag(major_conflict_T, n = 1, default = 0))

data_exclEU <- data_exclEU %>%
  group_by(country_T) %>%
  mutate(lagged_democracy_T_dummy = dplyr::lag(democracy_T_dummy, n = 1, default = 0))
data_exclEU <- data_exclEU %>%
  group_by(country_T) %>%
  mutate(lagged_Human_Rights_T = dplyr::lag(Human_Rights_T, n = 1, default = 0))
data_exclEU <- data_exclEU %>%
  group_by(country_T) %>%
  mutate(lagged_minor_conflict_T = dplyr::lag(minor_conflict_T, n = 1, default = 0))
data_exclEU <- data_exclEU %>%
  group_by(country_T) %>%
  mutate(lagged_major_conflict_T = dplyr::lag(major_conflict_T, n = 1, default = 0))

#################################### Table 2 ###################################

# Table 2, Column 1
probit_exclEU_sy_broad <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                  log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                  lagged_KOFGI_T +
                                  lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                  lagged_minor_conflict_T + lagged_major_conflict_T +
                                  lagged_successful_coups |
                                  sender_year,
                                family = "probit",
                                cluster = "sender_year",
                                data = data_exclEU)
summary(probit_exclEU_sy_broad)
calculate_marginal_effects(probit_exclEU_sy_broad)

# Table 2, Column 2
probit_exclEU_dyad_broad <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                    log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                    lagged_KOFGI_T +
                                    lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                    lagged_minor_conflict_T + lagged_major_conflict_T +
                                    lagged_successful_coups |
                                    country_pair + sender_year,
                                  family = "probit",
                                  cluster = "country_pair",
                                  data = data_exclEU)
summary(probit_exclEU_dyad_broad)
calculate_marginal_effects(probit_exclEU_dyad_broad)

# Table 2, Column 3
probit_exclUN_sy_broad <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                  log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                  lagged_KOFGI_T +
                                  lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                  lagged_minor_conflict_T + lagged_major_conflict_T +
                                  lagged_successful_coups |
                                  sender_year,
                                family = "probit",
                                cluster = "sender_year",
                                data = data_exclUN)
summary(probit_exclUN_sy_broad)
calculate_marginal_effects(probit_exclUN_sy_broad)

# Table 2, Column 4
probit_exclUN_dyad_broad <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                    log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                    lagged_KOFGI_T +
                                    lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                    lagged_minor_conflict_T + lagged_major_conflict_T +
                                    lagged_successful_coups |
                                    country_pair + sender_year,
                                  family = "probit",
                                  cluster = "country_pair",
                                  data = data_exclUN)
summary(probit_exclUN_dyad_broad)
calculate_marginal_effects(probit_exclUN_dyad_broad)

# Table 2, Column 5
probit_sy_broad <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                           log(lagged_pop_T) + log(lagged_rgdppc_T) +
                           lagged_KOFGI_T +
                           lagged_democracy_T_dummy + lagged_Human_Rights_T +
                           lagged_minor_conflict_T + lagged_major_conflict_T +
                           lagged_successful_coups |
                           sender_year,
                         family = "probit",
                         cluster = "sender_year",
                         data = data_all)
summary(probit_sy_broad)
calculate_marginal_effects(probit_sy_broad)

# Table 2, Column 6
probit_dyad_broad <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                             log(lagged_pop_T) + log(lagged_rgdppc_T) +
                             lagged_KOFGI_T +
                             lagged_democracy_T_dummy + lagged_Human_Rights_T +
                             lagged_minor_conflict_T + lagged_major_conflict_T +
                             lagged_successful_coups |
                             country_pair + sender_year,
                           family = "probit",
                           cluster = "country_pair",
                           data = data_all)
summary(probit_dyad_broad)
calculate_marginal_effects(probit_dyad_broad)

#################################### Table 3 ###################################

data_exclEU_democ <- merge(data_exclEU_democ, coups, by = c("country_T", "Year"), all.x = TRUE)
data_exclEU_democ <- data_exclEU_democ %>%
  group_by(country_T) %>%
  mutate(lagged_democracy_T_dummy = dplyr::lag(democracy_T_dummy, n = 1, default = 0))
data_exclEU_democ <- data_exclEU_democ %>%
  group_by(country_T) %>%
  mutate(lagged_Human_Rights_T = dplyr::lag(Human_Rights_T, n = 1, default = 0))
data_exclEU_democ <- data_exclEU_democ %>%
  group_by(country_T) %>%
  mutate(lagged_minor_conflict_T = dplyr::lag(minor_conflict_T, n = 1, default = 0))
data_exclEU_democ <- data_exclEU_democ %>%
  group_by(country_T) %>%
  mutate(lagged_major_conflict_T = dplyr::lag(major_conflict_T, n = 1, default = 0))

# Table 3, Excl. "Democracy", broad
probit_exclEU_dyad_broad_democ <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                          log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                          lagged_KOFGI_T +
                                          lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                          lagged_minor_conflict_T + lagged_major_conflict_T +
                                          lagged_successful_coups |
                                          country_pair + sender_year,
                                        family = "probit",
                                        cluster = "country_pair",
                                        glm.iter = 200,
                                        data = data_exclEU_democ)
summary(probit_exclEU_dyad_broad_democ)
calculate_marginal_effects(probit_exclEU_dyad_broad_democ)

data_exclEU_hr <- merge(data_exclEU_hr, coups, by = c("country_T", "Year"), all.x = TRUE)
data_exclEU_hr <- data_exclEU_hr %>%
  group_by(country_T) %>%
  mutate(lagged_democracy_T_dummy = dplyr::lag(democracy_T_dummy, n = 1, default = 0))
data_exclEU_hr <- data_exclEU_hr %>%
  group_by(country_T) %>%
  mutate(lagged_Human_Rights_T = dplyr::lag(Human_Rights_T, n = 1, default = 0))
data_exclEU_hr <- data_exclEU_hr %>%
  group_by(country_T) %>%
  mutate(lagged_minor_conflict_T = dplyr::lag(minor_conflict_T, n = 1, default = 0))
data_exclEU_hr <- data_exclEU_hr %>%
  group_by(country_T) %>%
  mutate(lagged_major_conflict_T = dplyr::lag(major_conflict_T, n = 1, default = 0))

# Table 3, Excl. "Human Rights", broad
probit_exclEU_dyad_broad_hr <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                       log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                       lagged_KOFGI_T +
                                       lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                       lagged_minor_conflict_T + lagged_major_conflict_T +
                                       lagged_successful_coups |
                                       country_pair + sender_year,
                                     family = "probit",
                                     cluster = "country_pair",
                                     glm.iter = 200,
                                     data = data_exclEU_hr)
summary(probit_exclEU_dyad_broad_hr)
calculate_marginal_effects(probit_exclEU_dyad_broad_hr)

data_exclEU_destab <- merge(data_exclEU_destab, coups, by = c("country_T", "Year"), all.x = TRUE)
data_exclEU_destab <- data_exclEU_destab %>%
  group_by(country_T) %>%
  mutate(lagged_democracy_T_dummy = dplyr::lag(democracy_T_dummy, n = 1, default = 0))
data_exclEU_destab <- data_exclEU_destab %>%
  group_by(country_T) %>%
  mutate(lagged_Human_Rights_T = dplyr::lag(Human_Rights_T, n = 1, default = 0))
data_exclEU_destab <- data_exclEU_destab %>%
  group_by(country_T) %>%
  mutate(lagged_minor_conflict_T = dplyr::lag(minor_conflict_T, n = 1, default = 0))
data_exclEU_destab <- data_exclEU_destab %>%
  group_by(country_T) %>%
  mutate(lagged_major_conflict_T = dplyr::lag(major_conflict_T, n = 1, default = 0))

# Table 3, Excl. "Destab. Regime", broad
probit_exclEU_dyad_broad_destab <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                           log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                           lagged_KOFGI_T +
                                           lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                           lagged_minor_conflict_T + lagged_major_conflict_T +
                                           lagged_successful_coups |
                                           country_pair + sender_year,
                                         family = "probit",
                                         cluster = "country_pair",
                                         glm.iter = 200,
                                         data = data_exclEU_destab)
summary(probit_exclEU_dyad_broad_destab)
calculate_marginal_effects(probit_exclEU_dyad_broad_destab)

data_exclEU_policychange <- merge(data_exclEU_policychange, coups, by = c("country_T", "Year"), all.x = TRUE)
data_exclEU_policychange <- data_exclEU_policychange %>%
  group_by(country_T) %>%
  mutate(lagged_democracy_T_dummy = dplyr::lag(democracy_T_dummy, n = 1, default = 0))
data_exclEU_policychange <- data_exclEU_policychange %>%
  group_by(country_T) %>%
  mutate(lagged_Human_Rights_T = dplyr::lag(Human_Rights_T, n = 1, default = 0))
data_exclEU_policychange <- data_exclEU_policychange %>%
  group_by(country_T) %>%
  mutate(lagged_minor_conflict_T = dplyr::lag(minor_conflict_T, n = 1, default = 0))
data_exclEU_policychange <- data_exclEU_policychange %>%
  group_by(country_T) %>%
  mutate(lagged_major_conflict_T = dplyr::lag(major_conflict_T, n = 1, default = 0))

# Table 3, Excl. "Policy Change", broad
probit_exclEU_dyad_broad_policychange <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                                 log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                                 lagged_KOFGI_T +
                                                 lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                                 lagged_minor_conflict_T + lagged_major_conflict_T +
                                                 lagged_successful_coups |
                                                 country_pair + sender_year,
                                               family = "probit",
                                               cluster = "country_pair",
                                               glm.iter = 200,
                                               data = data_exclEU_policychange)
summary(probit_exclEU_dyad_broad_policychange)
calculate_marginal_effects(probit_exclEU_dyad_broad_policychange)

data_exclEU_preventwar <- merge(data_exclEU_preventwar, coups, by = c("country_T", "Year"), all.x = TRUE)
data_exclEU_preventwar <- data_exclEU_preventwar %>%
  group_by(country_T) %>%
  mutate(lagged_democracy_T_dummy = dplyr::lag(democracy_T_dummy, n = 1, default = 0))
data_exclEU_preventwar <- data_exclEU_preventwar %>%
  group_by(country_T) %>%
  mutate(lagged_Human_Rights_T = dplyr::lag(Human_Rights_T, n = 1, default = 0))
data_exclEU_preventwar <- data_exclEU_preventwar %>%
  group_by(country_T) %>%
  mutate(lagged_minor_conflict_T = dplyr::lag(minor_conflict_T, n = 1, default = 0))
data_exclEU_preventwar <- data_exclEU_preventwar %>%
  group_by(country_T) %>%
  mutate(lagged_major_conflict_T = dplyr::lag(major_conflict_T, n = 1, default = 0))

# Table 3, Excl. "Prevent War", broad
probit_exclEU_dyad_broad_preventwar <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                               log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                               lagged_KOFGI_T +
                                               lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                               lagged_minor_conflict_T + lagged_major_conflict_T +
                                               lagged_successful_coups |
                                               country_pair + sender_year,
                                             family = "probit",
                                             cluster = "country_pair",
                                             glm.iter = 200,
                                             data = data_exclEU_preventwar)
summary(probit_exclEU_dyad_broad_preventwar)
calculate_marginal_effects(probit_exclEU_dyad_broad_preventwar)

data_exclEU_endwar <- merge(data_exclEU_endwar, coups, by = c("country_T", "Year"), all.x = TRUE)
data_exclEU_endwar <- data_exclEU_endwar %>%
  group_by(country_T) %>%
  mutate(lagged_democracy_T_dummy = dplyr::lag(democracy_T_dummy, n = 1, default = 0))
data_exclEU_endwar <- data_exclEU_endwar %>%
  group_by(country_T) %>%
  mutate(lagged_Human_Rights_T = dplyr::lag(Human_Rights_T, n = 1, default = 0))
data_exclEU_endwar <- data_exclEU_endwar %>%
  group_by(country_T) %>%
  mutate(lagged_minor_conflict_T = dplyr::lag(minor_conflict_T, n = 1, default = 0))
data_exclEU_endwar <- data_exclEU_endwar %>%
  group_by(country_T) %>%
  mutate(lagged_major_conflict_T = dplyr::lag(major_conflict_T, n = 1, default = 0))

# Table 3, Excl. "End War", broad
probit_exclEU_dyad_broad_endwar <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                           log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                           lagged_KOFGI_T +
                                           lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                           lagged_minor_conflict_T + lagged_major_conflict_T +
                                           lagged_successful_coups |
                                           country_pair + sender_year,
                                         family = "probit",
                                         cluster = "country_pair",
                                         glm.iter = 200,
                                         data = data_exclEU_endwar)
summary(probit_exclEU_dyad_broad_endwar)
calculate_marginal_effects(probit_exclEU_dyad_broad_endwar)

data_exclEU_terrconfl <- merge(data_exclEU_terrconfl, coups, by = c("country_T", "Year"), all.x = TRUE)
data_exclEU_terrconfl <- data_exclEU_terrconfl %>%
  group_by(country_T) %>%
  mutate(lagged_democracy_T_dummy = dplyr::lag(democracy_T_dummy, n = 1, default = 0))
data_exclEU_terrconfl <- data_exclEU_terrconfl %>%
  group_by(country_T) %>%
  mutate(lagged_Human_Rights_T = dplyr::lag(Human_Rights_T, n = 1, default = 0))
data_exclEU_terrconfl <- data_exclEU_terrconfl %>%
  group_by(country_T) %>%
  mutate(lagged_minor_conflict_T = dplyr::lag(minor_conflict_T, n = 1, default = 0))
data_exclEU_terrconfl <- data_exclEU_terrconfl %>%
  group_by(country_T) %>%
  mutate(lagged_major_conflict_T = dplyr::lag(major_conflict_T, n = 1, default = 0))

# Table 3, Excl. "Territorial Conflict", broad
probit_exclEU_dyad_broad_terrconfl <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                              log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                              lagged_KOFGI_T +
                                              lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                              lagged_minor_conflict_T + lagged_major_conflict_T +
                                              lagged_successful_coups |
                                              country_pair + sender_year,
                                            family = "probit",
                                            cluster = "country_pair",
                                            glm.iter = 200,
                                            data = data_exclEU_terrconfl)
summary(probit_exclEU_dyad_broad_terrconfl)
calculate_marginal_effects(probit_exclEU_dyad_broad_terrconfl)

data_exclEU_terror <- merge(data_exclEU_terror, coups, by = c("country_T", "Year"), all.x = TRUE)
data_exclEU_terror <- data_exclEU_terror %>%
  group_by(country_T) %>%
  mutate(lagged_democracy_T_dummy = dplyr::lag(democracy_T_dummy, n = 1, default = 0))
data_exclEU_terror <- data_exclEU_terror %>%
  group_by(country_T) %>%
  mutate(lagged_Human_Rights_T = dplyr::lag(Human_Rights_T, n = 1, default = 0))
data_exclEU_terror <- data_exclEU_terror %>%
  group_by(country_T) %>%
  mutate(lagged_minor_conflict_T = dplyr::lag(minor_conflict_T, n = 1, default = 0))
data_exclEU_terror <- data_exclEU_terror %>%
  group_by(country_T) %>%
  mutate(lagged_major_conflict_T = dplyr::lag(major_conflict_T, n = 1, default = 0))

# Table 3, Excl. "Terrorism", broad
probit_exclEU_dyad_broad_terror <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                           log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                           lagged_KOFGI_T +
                                           lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                           lagged_minor_conflict_T + lagged_major_conflict_T +
                                           lagged_successful_coups |
                                           country_pair + sender_year,
                                         family = "probit",
                                         cluster = "country_pair",
                                         glm.iter = 200,
                                         data = data_exclEU_terror)
summary(probit_exclEU_dyad_broad_terror)
calculate_marginal_effects(probit_exclEU_dyad_broad_terror)

data_exclEU_other <- merge(data_exclEU_other, coups, by = c("country_T", "Year"), all.x = TRUE)
data_exclEU_other <- data_exclEU_other %>%
  group_by(country_T) %>%
  mutate(lagged_democracy_T_dummy = dplyr::lag(democracy_T_dummy, n = 1, default = 0))
data_exclEU_other <- data_exclEU_other %>%
  group_by(country_T) %>%
  mutate(lagged_Human_Rights_T = dplyr::lag(Human_Rights_T, n = 1, default = 0))
data_exclEU_other <- data_exclEU_other %>%
  group_by(country_T) %>%
  mutate(lagged_minor_conflict_T = dplyr::lag(minor_conflict_T, n = 1, default = 0))
data_exclEU_other <- data_exclEU_other %>%
  group_by(country_T) %>%
  mutate(lagged_major_conflict_T = dplyr::lag(major_conflict_T, n = 1, default = 0))

# Table 3, Excl. "Other Objectives", broad
probit_exclEU_dyad_broad_otherobj <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                          log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                          lagged_KOFGI_T +
                                          lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                          lagged_minor_conflict_T + lagged_major_conflict_T +
                                          lagged_successful_coups |
                                          country_pair + sender_year,
                                        family = "probit",
                                        cluster = "country_pair",
                                        glm.iter = 200,
                                        data = data_exclEU_other)
summary(probit_exclEU_dyad_broad_otherobj)
calculate_marginal_effects(probit_exclEU_dyad_broad_otherobj)

################################### Table 4 ####################################

#### Cold War Split
data_all_coldwar <- subset(data_all, Year <= 1991)
data_all_postcoldwar <- subset(data_all, Year > 1991)

data_exclUN_coldwar <- subset(data_exclUN, Year <= 1991)
data_exclUN_postcoldwar <- subset(data_exclUN, Year > 1991)

data_exclEU_coldwar <- subset(data_exclEU, Year <= 1991)
data_exclEU_postcoldwar <- subset(data_exclEU, Year > 1991)

# Table 4, Excl. Post-Cold War Sanctions, broad
probit_exclEU_dyad_broad_coldwar <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                            log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                            lagged_KOFGI_T +
                                            lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                            lagged_minor_conflict_T + lagged_major_conflict_T +
                                            lagged_successful_coups |
                                            country_pair + sender_year,
                                          glm.iter = 200,
                                          family = "probit",
                                          cluster = "country_pair",
                                          data = data_exclEU_coldwar)
summary(probit_exclEU_dyad_broad_coldwar)
calculate_marginal_effects(probit_exclEU_dyad_broad_coldwar)

# Table 4, Excl. Cold War Sanctions, broad
probit_exclEU_dyad_broad_postcoldwar <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                                log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                                lagged_KOFGI_T +
                                                lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                                lagged_minor_conflict_T + lagged_major_conflict_T +
                                                lagged_successful_coups |
                                                country_pair + sender_year,
                                              glm.iter = 200,
                                              family = "probit",
                                              cluster = "country_pair",
                                              data = data_exclEU_postcoldwar)
summary(probit_exclEU_dyad_broad_postcoldwar)
calculate_marginal_effects(probit_exclEU_dyad_broad_postcoldwar)

#### Excluding sanctions against democracies
data_exclEU_excldemoc <- data_exclEU[!(data_exclEU$democracy_T_dummy == 1 & data_exclEU$sanctions == 1), ]

# Table 4, Excl. Sanctions ag. Democracies, broad
probit_exclEU_dyad_broad_excldemoc <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                              log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                              lagged_KOFGI_T +
                                              lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                              lagged_minor_conflict_T + lagged_major_conflict_T +
                                              lagged_successful_coups |
                                              country_pair + sender_year,
                                            family = "probit",
                                            cluster = "country_pair",
                                            data = data_exclEU_excldemoc)
summary(probit_exclEU_dyad_broad_excldemoc)
calculate_marginal_effects(probit_exclEU_dyad_broad_excldemoc)

#### Excluding US sanctions
data_exclEU_exclUS <- data_exclEU_exclUS %>%
  group_by(country_T) %>%
  mutate(lagged_democracy_T_dummy = dplyr::lag(democracy_T_dummy, n = 1, default = 0))
data_exclEU_exclUS <- data_exclEU_exclUS %>%
  group_by(country_T) %>%
  mutate(lagged_Human_Rights_T = dplyr::lag(Human_Rights_T, n = 1, default = 0))
data_exclEU_exclUS <- data_exclEU_exclUS %>%
  group_by(country_T) %>%
  mutate(lagged_minor_conflict_T = dplyr::lag(minor_conflict_T, n = 1, default = 0))
data_exclEU_exclUS <- data_exclEU_exclUS %>%
  group_by(country_T) %>%
  mutate(lagged_major_conflict_T = dplyr::lag(major_conflict_T, n = 1, default = 0))
data_exclEU_exclUS <- merge(data_exclEU_exclUS, coups, by = c("country_T", "Year"), all.x = TRUE)

# Table 4, Excl. US Sanctions, broad
probit_exclEU_exclUS_dyad_broad <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                           log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                           lagged_KOFGI_T +
                                           lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                           lagged_minor_conflict_T + lagged_major_conflict_T +
                                           lagged_successful_coups |
                                           country_pair + sender_year,
                                         family = "probit",
                                         cluster = "country_pair",
                                         data = data_exclEU_exclUS)
summary(probit_exclEU_exclUS_dyad_broad)
calculate_marginal_effects(probit_exclEU_exclUS_dyad_broad)

#### Excluding trade sanctions
data_exclEU_excltrade <- data_exclEU[data_exclEU$trade != 1, ]

# Table 4, Excl. Trade Sanctions, broad
probit_exclEU_dyad_broad_excltrade <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                              log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                              lagged_KOFGI_T +
                                              lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                              lagged_minor_conflict_T + lagged_major_conflict_T +
                                              lagged_successful_coups |
                                              country_pair + sender_year,
                                            glm.iter = 200,
                                            family = "probit",
                                            cluster = "country_pair",
                                            data = data_exclEU_excltrade)
summary(probit_exclEU_dyad_broad_excltrade)
calculate_marginal_effects(probit_exclEU_dyad_broad_excltrade)

################################### Table OA1 ##################################

# Table OA1, Column 2
probit_exclEU_sy_broad_bife <- bife(sanctions ~ weighted_lead_sim_score_nd +
                                      log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                      lagged_KOFGI_T +
                                      lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                      lagged_minor_conflict_T + lagged_major_conflict_T +
                                      lagged_successful_coups |
                                      sender_year,
                                    model = "probit",
                                    data = data_exclEU)
summary(probit_exclEU_sy_broad_bife)
probit_exclEU_sy_broad_bife_bias <- bias_corr(probit_exclEU_sy_broad_bife)
calculate_marginal_effects_bife(probit_exclEU_sy_broad_bife_bias)
summary(probit_exclEU_sy_broad_bife_bias)

################################### Table OA2 ##################################

#### Linear Probability Models
lpm_data_exclEU <- data_exclEU[-abs(probit_exclEU_sy_broad$obs_selection$obsRemoved), ]

# Table OA2, Column 2
lpm_exclEU_sy_broad <- feols(sanctions ~ weighted_lead_sim_score_nd +
                               log(lagged_pop_T) + log(lagged_rgdppc_T) +
                               lagged_KOFGI_T +
                               lagged_democracy_T_dummy + lagged_Human_Rights_T +
                               lagged_minor_conflict_T + lagged_major_conflict_T +
                               lagged_successful_coups |
                               sender_year,
                             cluster = "sender_year",
                             data = lpm_data_exclEU)
summary(lpm_exclEU_sy_broad)

mod_data_exclEU <- data_exclEU[-abs(probit_exclEU_dyad_broad$obs_selection$obsRemoved), ]

# Table OA2, Column 4
lpm_exclEU_dyad_broad <- feols(sanctions ~ weighted_lead_sim_score_nd +
                                 log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                 lagged_KOFGI_T +
                                 lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                 lagged_minor_conflict_T + lagged_major_conflict_T +
                                 lagged_successful_coups |
                                 country_pair + sender_year,
                               cluster = "country_pair",
                               data = mod_data_exclEU)
summary(lpm_exclEU_dyad_broad)

# Table OA2, Column 5
lpm_exclEU_dyad_broad_ty <- feols(sanctions ~ weighted_lead_sim_score_nd +
                                    log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                    lagged_KOFGI_T +
                                    lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                    lagged_minor_conflict_T + lagged_major_conflict_T +
                                    lagged_successful_coups +
                                    as.factor(country_T) * Year |
                                    country_pair + sender_year,
                                  cluster = "country_pair",
                                  data = mod_data_exclEU)
summary(lpm_exclEU_dyad_broad_ty)

mod_data_exclEU <- mod_data_exclEU %>%
  mutate(Year2 = Year^2) # Create a quadratic year term

# Table OA2, Column 6
lpm_exclEU_dyad_broad_tyquad <- feols(sanctions ~ weighted_lead_sim_score_nd +
                                        log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                        lagged_KOFGI_T +
                                        lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                        lagged_minor_conflict_T + lagged_major_conflict_T +
                                        lagged_successful_coups +
                                        as.factor(country_T) * Year +
                                        as.factor(country_T) * Year2 |
                                        country_pair + sender_year,
                                      cluster = "country_pair",
                                      data = mod_data_exclEU)
summary(lpm_exclEU_dyad_broad_tyquad)

sy_data <- data_all[-abs(probit_sy$obs_selection$obsRemoved), ]
sy_data_exclUN <- data_exclUN[-abs(probit_exclUN_sy$obs_selection$obsRemoved), ]
sy_data_exclEU <- data_exclEU[-abs(probit_exclEU_sy$obs_selection$obsRemoved), ]

sy_data_sanc <- sy_data[sy_data$sanctions == 1, ]
sy_data_nosanc <- sy_data[sy_data$sanctions == 0, ]

sy_data_exclEU_sanc <- sy_data_exclEU[sy_data_exclEU$sanctions == 1, ]
sy_data_exclEU_nosanc <- sy_data_exclEU[sy_data_exclEU$sanctions == 0, ]

################################### Table OA3 ##################################

data_exclEU <- data_exclEU %>%
  left_join(
    affinity_combined %>% dplyr::select(Year, country_S, country_T, friend),
    by = c("Year", "country_S", "country_T")
  )

data_exclEU$friend[is.na(data_exclEU$friend)] <- 0

# Table OA3, Column 1
probit_exclEU_dyad_broad_affinity <- feglm(sanctions ~ weighted_lead_sim_score_nd +
                                             log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                             lagged_KOFGI_T +
                                             lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                             lagged_minor_conflict_T + lagged_major_conflict_T +
                                             lagged_successful_coups + friend |
                                             country_pair + sender_year,
                                           family = "probit",
                                           cluster = "country_pair",
                                           data = data_exclEU)
summary(probit_exclEU_dyad_broad_affinity)
calculate_marginal_effects(probit_exclEU_dyad_broad_affinity)

data_exclEU <- data_exclEU %>%
  mutate(joint_democracy_new = democracy_T_dummy * democracy_S_dummy)

# Table OA3, Column 2
probit_exclEU_dyad_broad_jointdemoc <- fixest::feglm(sanctions ~ weighted_lead_sim_score_nd +
                                                       log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                                       lagged_KOFGI_T +
                                                       lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                                       lagged_minor_conflict_T + lagged_major_conflict_T +
                                                       lagged_successful_coups + joint_democracy_new |
                                                       country_pair + sender_year,
                                                     family = "probit",
                                                     cluster = "country_pair",
                                                     data = data_exclEU)
summary(probit_exclEU_dyad_broad_jointdemoc)
calculate_marginal_effects(probit_exclEU_dyad_broad_jointdemoc)

# Table OA3, Column 3
probit_exclEU_dyad_broad_rivalry <- fixest::feglm(sanctions ~ weighted_lead_sim_score_nd +
                                                    log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                                    lagged_KOFGI_T +
                                                    lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                                    lagged_minor_conflict_T + lagged_major_conflict_T +
                                                    lagged_successful_coups + rivalry_nd |
                                                    country_pair + sender_year,
                                                  family = "probit",
                                                  cluster = "country_pair",
                                                  data = data_exclEU)
summary(probit_exclEU_dyad_broad_rivalry)
calculate_marginal_effects(probit_exclEU_dyad_broad_rivalry)

# Table OA3, Column 4
probit_exclEU_dyad_broad_jointigos <- fixest::feglm(sanctions ~ weighted_lead_sim_score_nd +
                                                      log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                                      lagged_KOFGI_T +
                                                      lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                                      lagged_minor_conflict_T + lagged_major_conflict_T +
                                                      lagged_successful_coups + joint_igos |
                                                      country_pair + sender_year,
                                                    family = "probit",
                                                    cluster = "country_pair",
                                                    data = data_exclEU)
summary(probit_exclEU_dyad_broad_jointigos)
calculate_marginal_effects(probit_exclEU_dyad_broad_jointigos)

# Table OA3, Column 5
probit_exclEU_dyad_broad_aff_joindemoc_rival_jointIGOs <- fixest::feglm(
  sanctions ~ weighted_lead_sim_score_nd +
    log(lagged_pop_T) + log(lagged_rgdppc_T) +
    lagged_KOFGI_T +
    lagged_democracy_T_dummy + lagged_Human_Rights_T +
    lagged_minor_conflict_T + lagged_major_conflict_T +
    lagged_successful_coups +
    friend + joint_democracy_new + rivalry_nd + joint_igos |
    country_pair + sender_year,
  family = "probit",
  cluster = "country_pair",
  data = data_exclEU
)
summary(probit_exclEU_dyad_broad_aff_joindemoc_rival_jointIGOs)
calculate_marginal_effects(probit_exclEU_dyad_broad_aff_joindemoc_rival_jointIGOs)

################################### Table OA4 ##################################

# Table OA4, Column 2, Excl. Military Experience
probit_exclEU_dyad_broad_milexp <- feglm(sanctions ~ weighted_lead_sim_score_milexp +
                                           log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                           lagged_KOFGI_T +
                                           lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                           lagged_minor_conflict_T + lagged_major_conflict_T +
                                           lagged_successful_coups |
                                           country_pair + sender_year,
                                         family = "probit",
                                         cluster = "country_pair",
                                         data = data_exclEU)
summary(probit_exclEU_dyad_broad_milexp)
calculate_marginal_effects(probit_exclEU_dyad_broad_milexp)

# Table OA4, Column 2, Excl. Education
probit_exclEU_dyad_broad_educ <- feglm(sanctions ~ weighted_lead_sim_score_educ +
                                         log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                         lagged_KOFGI_T +
                                         lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                         lagged_minor_conflict_T + lagged_major_conflict_T +
                                         lagged_successful_coups |
                                         country_pair + sender_year,
                                       family = "probit",
                                       cluster = "country_pair",
                                       data = data_exclEU)
summary(probit_exclEU_dyad_broad_educ)
calculate_marginal_effects(probit_exclEU_dyad_broad_educ)

# Table OA4, Column 2, Excl. Childhood Family
probit_exclEU_dyad_broad_childfamily <- feglm(sanctions ~ weighted_lead_sim_score_childfamily +
                                                log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                                lagged_KOFGI_T +
                                                lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                                lagged_minor_conflict_T + lagged_major_conflict_T +
                                                lagged_successful_coups |
                                                country_pair + sender_year,
                                              family = "probit",
                                              cluster = "country_pair",
                                              data = data_exclEU)
summary(probit_exclEU_dyad_broad_childfamily)
calculate_marginal_effects(probit_exclEU_dyad_broad_childfamily)

# Table OA4, Column 2, Excl. Adult Family
probit_exclEU_dyad_broad_adultfamily <- feglm(sanctions ~ weighted_lead_sim_score_adultfamily +
                                                log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                                lagged_KOFGI_T +
                                                lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                                lagged_minor_conflict_T + lagged_major_conflict_T +
                                                lagged_successful_coups |
                                                country_pair + sender_year,
                                              family = "probit",
                                              cluster = "country_pair",
                                              data = data_exclEU)
summary(probit_exclEU_dyad_broad_adultfamily)
calculate_marginal_effects(probit_exclEU_dyad_broad_adultfamily)

# Table OA4, Column 2, Excl. Occupation
probit_exclEU_dyad_broad_occupation <- feglm(sanctions ~ weighted_lead_sim_score_occupation +
                                               log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                               lagged_KOFGI_T +
                                               lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                               lagged_minor_conflict_T + lagged_major_conflict_T +
                                               lagged_successful_coups |
                                               country_pair + sender_year,
                                             family = "probit",
                                             cluster = "country_pair",
                                             data = data_exclEU)
summary(probit_exclEU_dyad_broad_occupation)
calculate_marginal_effects(probit_exclEU_dyad_broad_occupation)

# Table OA4, Column 2, Excl. Political Experience
probit_exclEU_dyad_broad_polexp <- feglm(sanctions ~ weighted_lead_sim_score_polexp +
                                           log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                           lagged_KOFGI_T +
                                           lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                           lagged_minor_conflict_T + lagged_major_conflict_T +
                                           lagged_successful_coups |
                                           country_pair + sender_year,
                                         family = "probit",
                                         cluster = "country_pair",
                                         data = data_exclEU)
summary(probit_exclEU_dyad_broad_polexp)
calculate_marginal_effects(probit_exclEU_dyad_broad_polexp)

# Table OA4, Column 2, Excl. Other
probit_exclEU_dyad_broad_other <- feglm(sanctions ~ weighted_lead_sim_score_other +
                                          log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                          lagged_KOFGI_T +
                                          lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                          lagged_minor_conflict_T + lagged_major_conflict_T +
                                          lagged_successful_coups |
                                          country_pair + sender_year,
                                        family = "probit",
                                        cluster = "country_pair",
                                        data = data_exclEU)
summary(probit_exclEU_dyad_broad_other)
calculate_marginal_effects(probit_exclEU_dyad_broad_other)

# Table OA4, Column 2, Theory-Based
probit_exclEU_dyad_broad_theory <- feglm(sanctions ~ weighted_lead_sim_score_theory +
                                           log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                           lagged_KOFGI_T +
                                           lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                           lagged_minor_conflict_T + lagged_major_conflict_T +
                                           lagged_successful_coups |
                                           country_pair + sender_year,
                                         family = "probit",
                                         cluster = "country_pair",
                                         data = data_exclEU)
summary(probit_exclEU_dyad_broad_theory)
calculate_marginal_effects(probit_exclEU_dyad_broad_theory)

################################### Table OA5 ##################################

# Table OA5, Column 2
probit_exclEU_dyad_broad_gower <- feglm(sanctions ~ weighted_lead_sim_score_gower +
                                          log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                          lagged_KOFGI_T +
                                          lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                          lagged_minor_conflict_T + lagged_major_conflict_T +
                                          lagged_successful_coups |
                                          country_pair + sender_year,
                                        family = "probit",
                                        cluster = "country_pair",
                                        data = data_exclEU)
summary(probit_exclEU_dyad_broad_gower)
calculate_marginal_effects(probit_exclEU_dyad_broad_gower)

# Table OA5, Column 4
lpm_exclEU_dyad_broad_gower <- feols(sanctions ~ weighted_lead_sim_score_gower +
                                       log(lagged_pop_T) + log(lagged_rgdppc_T) +
                                       lagged_KOFGI_T +
                                       lagged_democracy_T_dummy + lagged_Human_Rights_T +
                                       lagged_minor_conflict_T + lagged_major_conflict_T +
                                       lagged_successful_coups |
                                       country_pair + sender_year,
                                     cluster = "country_pair",
                                     data = mod_data_exclEU)
summary(lpm_exclEU_dyad_broad_gower)
