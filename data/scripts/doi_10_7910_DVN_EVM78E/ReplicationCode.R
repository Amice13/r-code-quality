#########################################################################
# Author:	        	  CG, JU
# File name:		      ReplicationCode.R
# Institution:	    	Columbia U, Johns Hopkins	U
# File Date: 	      	July 25, 2018
# Publication title:	LPG as a Clean Cooking Fuel: Adoption, Use, and Impact in India
# File purpose:	    	Replication Code 
#########################################################################

####################################################################################################################
######################################      SETTING UP CODE                   ###################################### 
######################################      LIBRARIES, VARIABLES, SUBSETS     ###################################### 
####################################################################################################################

##################
### LIBRARIES   ##
##################

## ** If you do not have these packages, please install
install.packages(c("foreign", "Hmisc", "survey", "rgdal", "tmap", "tmaptools", "sp", "tidyverse", "ifultools", 
                   "Weighted.Desc.Stat", "gridExtra", "car", "GGally", "reshape2"))

## Load packages

library(foreign)
#rcorr
library(Hmisc)
#weights
library(survey)
#maps
library(rgdal)
library(tmap)
library(tmaptools)
library(sp)
#tidyverse
library(tidyverse)
#capitalization issues
library(ifultools)
#tables
library(Weighted.Desc.Stat)
library(gridExtra)
library(car)
#correlation plot
library(GGally)
#data wrangler
library(reshape2)

##################
### LOAD DATA   ##
##################

#### ** NOTE: SET YOUR FILE PATH ***
RawDataHH = read.table("~/Dataverse_Files/RawDataHH.tab", sep = "\t", header = TRUE)

#create variable "id" with State, District, and Village Codes
RawDataHH$id = paste(RawDataHH$m1_q8_state_code, RawDataHH$m1_q9_district_code, RawDataHH$m1_q11_village_code, sep="-")

##################
### VARIABLES   ##
##################

# INDICATOR FOR COUNTING HOUSEHOLDS
RawDataHH["Interviewed"] <- 1

# LOG-TRANSFORMATION OF MONTHLY EXPENDITURE
RawDataHH["m1_q32_month_expenditure_log"] <- log(RawDataHH$m1_q32_month_expenditure)

# CREATE AGE GROUPS
RawDataHH["AgeGroups"] <- ifelse(RawDataHH$m1_q19_age > 10 & RawDataHH$m1_q19_age < 25, "Under 25", 
                                 ifelse(RawDataHH$m1_q19_age >25 & RawDataHH$m1_q19_age <= 35, "25-35",
                                        ifelse(RawDataHH$m1_q19_age >35 & RawDataHH$m1_q19_age <= 50, "35-50",
                                               ifelse(RawDataHH$m1_q19_age >50 & RawDataHH$m1_q19_age <= 65, "50-65",
                                                      ifelse(RawDataHH$m1_q19_age >65, "Over 65",
                                                             NA)))))

# CREATE INDICATOR VARIABLES FOR COVARIATES

RawDataHH["Religion_Hindu"] <- ifelse(RawDataHH$m1_q24_religion == 1, 1,
                                      0)
RawDataHH["Religion_Muslim"] <- ifelse(RawDataHH$m1_q24_religion == 2, 1,
                                       0)
RawDataHH["Caste_ScheduledCaste"] <- ifelse(RawDataHH$m1_q25_caste == 1, 1, 
                                            0)
RawDataHH["Caste_ScheduledTribe"] <- ifelse(RawDataHH$m1_q25_caste == 2, 1, 
                                            0)
RawDataHH["Caste_OtherBackwardClass"] <- ifelse(RawDataHH$m1_q25_caste == 3, 1, 
                                                0)
RawDataHH["Caste_General"] <- ifelse(RawDataHH$m1_q25_caste == 4, 1, 
                                     0)
RawDataHH["Edu_NoFormalSchooling"] <- ifelse(RawDataHH$m1_q23_edu == 1, 1, 
                                             0)
RawDataHH["Edu_UpTo5thStandard"] <- ifelse(RawDataHH$m1_q23_edu == 2, 1, 
                                           0)
RawDataHH["Edu_MoreThan5thStandard"] <- ifelse(RawDataHH$m1_q23_edu == 3 | RawDataHH$m1_q23_edu == 4 | RawDataHH$m1_q23_edu == 5, 1, 
                                               0)
RawDataHH["Decision_MaleHouseholdHead"] <- ifelse(RawDataHH$m1_q38_decision_maker==1, 1, 
                                                  0)
RawDataHH["Decision_FemaleHouseholdHead"] <- ifelse(RawDataHH$m1_q38_decision_maker==2, 1, 
                                                    0)
RawDataHH["Decision_Both"] <- ifelse(RawDataHH$m1_q38_decision_maker==3, 1, 
                                     0)

# CREATE INDICATOR VARIABLES FOR LPG SATISFACTION AND DISSATISFCATION

RawDataHH["LPG_Unsatisfied"] <- ifelse(RawDataHH$m4_q104_lpg_satisfy == 1, 1,
                                       0)
RawDataHH["LPG_Unsatisfied_Expense"] <- ifelse(RawDataHH$m4_q104_1_lpg_unsatisfy_expense, 1,
                                               0)
RawDataHH["LPG_Unsatisfied_Avail"] <- ifelse(RawDataHH$m4_q104_2_lpg_unsatisfy_avail, 1,
                                             0)
RawDataHH["LPG_Unsatisfied_Far"] <- ifelse(RawDataHH$m4_q104_3_lpg_unsatisfy_far, 1,
                                           0)
RawDataHH["LPG_Unsatisfied_Maintain"] <- ifelse(RawDataHH$m4_q104_4_lpg_unsatisfy_maintain, 1,
                                                0)

# CREATE INDICATOR VARIABLE FOR LPG SATISFACTION

RawDataHH["LPG_Satisfied"] <- ifelse(RawDataHH$m4_q104_lpg_satisfy == 3, 1,
                                     0)

# HOUSEHOLD DOES NOT HAVE LPG 

RawDataHH["No_LPG"] <- ifelse(RawDataHH$m4_q103_lpg == 0, 1,
                              0)

# MEASUREMENTS OF LPG USE

RawDataHH["Large_kg_lpg"] <- RawDataHH$m4_q103_4_lpg_lcyl*14.2 # LARGE LPG CYLINDER IS 14.2 KG
RawDataHH["Small_kg_lpg"] <- RawDataHH$m4_q103_7_lpg_scyl*5 # SMALL LPG CYLINDER IS 5 KG
RawDataHH["kg_lpg"] <- RawDataHH$Large_kg_lpg + RawDataHH$Small_kg_lpg # TOTAL AMT OF LPG (KG) IN A YEAR
RawDataHH["kg_lpg_month"] <- RawDataHH$kg_lpg / 12 # AMT OF LPG (KG) PER MONTH
RawDataHH["kg_lpg_month_percapita"] <- RawDataHH$kg_lpg_month / (RawDataHH$m1_q27_no_adults + RawDataHH$m1_q29_no_children) # KG LPG PER CAPITA

# CREATING CODES FOR WHAT IS COOKED WITH LPG IN HOUSEHOLD
RawDataHH_lpg["LPG_cooked"] <- paste(RawDataHH_lpg$m4_q103_3_1_lpg_chapattis, RawDataHH_lpg$m4_q103_3_2_lpg_veg, RawDataHH_lpg$m4_q103_3_3_lpg_rice, RawDataHH_lpg$m4_q103_3_4_lpg_tea_snack, RawDataHH_lpg$m4_q103_3_5_lpg_milk, RawDataHH_lpg$m4_q103_3_6_lpg_water, sep = "")

# INDICATOR VARIABLES FOR ALL STOVE TYPES IN HOUSEHOLD
RawDataHH["3stone_in"] <- ifelse(RawDataHH$m4_q116_1_3stone_in > 0, 1,
                                 0)
RawDataHH["3stone_out"] <- ifelse(RawDataHH$m4_q116_1_3stone_out > 0, 1,
                                  0)
RawDataHH["3stone"] <- ifelse(RawDataHH$`3stone_in` + RawDataHH$`3stone_out` > 0, 1,
                                  0)
RawDataHH["Mud_in"] <- ifelse(RawDataHH$m4_q116_2_mud_in > 0, 1,
                                  0)
RawDataHH["Mud_out"] <- ifelse(RawDataHH$m4_q116_2_mud_out > 0, 1,
                              0)
RawDataHH["Mud"] <- ifelse(RawDataHH$Mud_in + RawDataHH$Mud_out > 0, 1,
                              0)
RawDataHH["Improved_in"] <- ifelse(RawDataHH$m4_q116_3_improved_in > 0, 1,
                              0)
RawDataHH["Improved_out"] <- ifelse(RawDataHH$m4_q116_3_improved_out > 0, 1,
                               0)
RawDataHH["Improved_port"] <- ifelse(RawDataHH$m4_q116_3_improved_port > 0, 1,
                               0)
RawDataHH["Improved"] <- ifelse(RawDataHH$Improved_in + RawDataHH$Improved_out + RawDataHH$Improved_port > 0, 1,
                           0)
RawDataHH["Kerosene"] <- ifelse(RawDataHH$m4_q116_4_kero_port > 0, 1,
                                0)
RawDataHH["LPG"] <- ifelse(RawDataHH$m4_q116_5_lpg_port > 0, 1,
                                0)
RawDataHH["Electric"] <- ifelse(RawDataHH$m4_q116_6_elec_port > 0, 1,
                           0)

# DOES HOUSEHOLD USE SOLID FUELS?
RawDataHH["Solid Fuels"] <- ifelse(RawDataHH$m4_q113_dungcake + RawDataHH$m4_q109_firewood + RawDataHH$m4_q114_agro > 0, 1,
                                   0)

# HOW MANY STOVES IN THE HOSUEHOLD
RawDataHH["NumberStoves"] <- RawDataHH$`3stone` + RawDataHH$Mud + RawDataHH$Improved + RawDataHH$Kerosene + RawDataHH$LPG + RawDataHH$Electric

# HOW DO HOUSEHOLDS GET LPG?
RawDataHH["LPG_from_distributor"] <- ifelse(RawDataHH$m4_q103_5_lpg_lcyl_dist + RawDataHH$m4_q103_8_lpg_scyl_dist > 0, 1,
                                            0)
RawDataHH["LPG_from_market"] <- ifelse(RawDataHH$m4_q103_6_lpg_lcyl_mkt + RawDataHH$m4_q103_9_lpg_scyl_mkt >0,1,
                                       0)

#DOES HOUSEHOLD USE LPG EXCLUSIVELY?
RawDataHH["Exclusive_LPG"] <- ifelse(RawDataHH$m4_q103_lpg==1 & RawDataHH$m4_q109_firewood==0 & RawDataHH$m4_q113_dungcake & RawDataHH$m4_q114_agro==0, 1,
                                     0)

##################
### SUBSETS     ##
##################

nolpg <- subset(RawDataHH, RawDataHH$No_LPG == 1) # NO LPG
RawDataHH_lpg <- RawDataHH[(RawDataHH$m4_q103_lpg) == 1,] # HAS LPG
RawDataHH_lpg_oneyear <- RawDataHH_lpg[(RawDataHH_lpg$m4_q103_1_lpg_year) == 1,] # HAS HAD LPG FOR <= 1 YR
RawDataHH_lpg_notoneyear <- RawDataHH_lpg[(RawDataHH_lpg$m4_q103_1_lpg_year) > 1,] # HAS HAD LPG FOR >1 YR
RawDataHH_lpg_main = RawDataHH_lpg[(RawDataHH_lpg$m5_q118_main_cookfuel) == 3,] # LPG IS THE MAIN FUEL FOR COOKING
RawDataHH_lpg_notmain = RawDataHH_lpg[!((RawDataHH_lpg$m5_q118_main_cookfuel) == 3),] # LPG IS NOT THE MAIN FUEL FOR COOKING
RawDataHH_fw <- RawDataHH[(RawDataHH$m5_q118_main_cookfuel) == 1,] # FIREWOOD IS MAIN FUEL FOR COOKING
RawDataHH_dung <- RawDataHH[(RawDataHH$m5_q118_main_cookfuel) == 2,] # DUNG IS MAIN FUEL FOR COOKING
RawDataHH_fw_gas <- RawDataHH_fw[(RawDataHH_fw$m4_q103_lpg) == 1,] #main fuel firewood but still use gas
RawDataHH_lpg_main_fw = RawDataHH_lpg_main[(RawDataHH_lpg_main$m4_q109_firewood) == 1,] #main fuel gas but still uses firewood
RawDataHH_lpg_exclusive = subset(RawDataHH, RawDataHH$Exclusive_LPG==1) #LPG exclusive

###########################
### SAMPLING WEIGHTS    ###
###########################

RawDataHH = RawDataHH[is.na(RawDataHH$weight) %in% F,]
sdesign = svydesign(id = ~m1_q3_hhid,
                    strata = ~m1_q11_village_code,
                    data = RawDataHH,
                    weights = ~weight,
                    nest = T)

RawDataHH_lpg = RawDataHH_lpg[is.na(RawDataHH_lpg$weight) %in% F,]
sdesign_lpg = svydesign(id = ~m1_q3_hhid,
                        strata = ~m1_q11_village_code,
                        data = RawDataHH_lpg,
                        weights = ~weight,
                        nest = T)

####################################################################################################################
######################################      DEVELOPMENT OF TABLES             ###################################### 
######################################      REQUIRED FOR FIGURES              ###################################### 
####################################################################################################################

#######################################
###  TABLES DEVELOPED HERE ARE NOT SHOWN PRESENTED AS FIGURES IN MANUSCRIPT OR SUPPORTING INFORMATION
###  BUT THEY ARE REQUIRED FOR THE PRODUCTION OF FIGURES
#######################################


############################
####### TABLE OF LPG DESCRIPTIVE STATISTICS
####### TABLE NOT SHOWN IN MANUSCRIPT, DATA SUPPORT FINDINGS
############################

lpg_descriptives <- c("Has LPG", "Years with LPG", "Cost of LPG connection [INR]", 
                      "LPG used per year [kg]", "Is satisfied with LPG", "Acquisition of tank: from authorized distributors", 
                      "Acquisition of tank: from market", "Cost: from authorized distributors [INR]", "Cost: from market [INR]", 
                      "Gas delivered to household", "One-way distance to get gas [km]", "Owns LPG and uses firewood",
                      "Cooks all dishes with LPG (1)")
lpg_descriptives_table <- data.frame(lpg_descriptives)
lpg_descriptives_table["Mean"] <- NA
lpg_descriptives_table["Min"] <- NA
lpg_descriptives_table["Max"] <- NA
lpg_descriptives_table["Observations"] <- NA
colnames(lpg_descriptives_table) <- c("Variable", "Mean", "Min", "Max", "Observations")
rownames(lpg_descriptives_table) <- lpg_descriptives

lpg_descriptives_table[1,2] <- svymean(RawDataHH$LPG, sdesign, na.rm=TRUE)
lpg_descriptives_table[2,2] <- svymean(RawDataHH$m4_q103_1_lpg_year, sdesign, na.rm=TRUE)
lpg_descriptives_table[3,2] <- svymean(RawDataHH$m4_q103_2_lpg_connect, sdesign, na.rm=TRUE)
lpg_descriptives_table[4,2] <- svymean(RawDataHH$kg_lpg, sdesign, na.rm=TRUE)
lpg_descriptives_table[5,2] <- svymean(RawDataHH$LPG_Satisfied, sdesign, na.rm=TRUE)
lpg_descriptives_table[6,2] <- svymean(RawDataHH$LPG_from_distributor, sdesign, na.rm=TRUE)
lpg_descriptives_table[7,2] <- svymean(RawDataHH$LPG_from_market, sdesign, na.rm=TRUE)
lpg_descriptives_table[8,2] <- svymean(RawDataHH$m4_q103_10_lpg_lcyl_dist_price, sdesign, na.rm=TRUE)
lpg_descriptives_table[9,2] <- svymean(RawDataHH$m4_q103_12_lpg_lcyl_mkt_price, sdesign, na.rm=TRUE)
lpg_descriptives_table[10,2] <- svymean(RawDataHH$m4_q103_14_lpg_cyl_deliver, sdesign, na.rm=TRUE)
lpg_descriptives_table[11,2] <- svymean(RawDataHH$m4_q103_15_lpg_distance, sdesign, na.rm=TRUE)
lpg_descriptives_table[12,2] <- mean(RawDataHH_lpg$m4_q109_firewood, na.rm=TRUE)
lpg_descriptives_table[13,2] <- mean(RawDataHH_lpg$LPG_cooked == "111110", na.rm=TRUE)

lpg_descriptives_table[1,5] <- length(is.na(RawDataHH$LPG))
lpg_descriptives_table[2,5] <- length(is.na(RawDataHH$m4_q103_1_lpg_year))
lpg_descriptives_table[3,5] <- length(is.na(RawDataHH$m4_q103_2_lpg_connect))
lpg_descriptives_table[4,5] <- length(is.na(RawDataHH$kg_lpg))
lpg_descriptives_table[5,5] <- length(is.na(RawDataHH$LPG_Satisfied))
lpg_descriptives_table[6,5] <- length(is.na(RawDataHH$LPG_from_distributor))
lpg_descriptives_table[7,5] <- length(is.na(RawDataHH$LPG_from_market))
lpg_descriptives_table[8,5] <- length(is.na(RawDataHH$m4_q103_10_lpg_lcyl_dist_price))
lpg_descriptives_table[9,5] <- length(is.na(RawDataHH$m4_q103_12_lpg_lcyl_mkt_price))
lpg_descriptives_table[10,5] <- length(is.na(RawDataHH$m4_q103_14_lpg_cyl_deliver))
lpg_descriptives_table[11,5] <- length(is.na(RawDataHH$m4_q103_15_lpg_distance))
lpg_descriptives_table[12,5] <- length(is.na(RawDataHH$m4_q109_firewood))
lpg_descriptives_table[13,5] <- length(is.na(RawDataHH$m4_q109_firewood))

lpg_descriptives_table[2,3] <- min(RawDataHH$m4_q103_1_lpg_year, na.rm = TRUE)
lpg_descriptives_table[2,4] <- max(RawDataHH$m4_q103_1_lpg_year, na.rm = TRUE)
lpg_descriptives_table[3,3] <- min(RawDataHH$m4_q103_2_lpg_connect, na.rm = TRUE)
lpg_descriptives_table[3,4] <- max(RawDataHH$m4_q103_2_lpg_connect, na.rm = TRUE)
lpg_descriptives_table[4,3] <- min(RawDataHH$kg_lpg, na.rm = TRUE)
lpg_descriptives_table[4,4] <- max(RawDataHH$kg_lpg, na.rm = TRUE)
lpg_descriptives_table[8,3] <- min(RawDataHH$m4_q103_10_lpg_lcyl_dist_price, na.rm = TRUE)
lpg_descriptives_table[8,4] <- max(RawDataHH$m4_q103_10_lpg_lcyl_dist_price, na.rm = TRUE)
lpg_descriptives_table[9,3] <- min(RawDataHH$m4_q103_12_lpg_lcyl_mkt_price, na.rm = TRUE)
lpg_descriptives_table[9,4] <- max(RawDataHH$m4_q103_12_lpg_lcyl_mkt_price, na.rm = TRUE)
lpg_descriptives_table[11,3] <- min(RawDataHH$m4_q103_15_lpg_distance, na.rm = TRUE)
lpg_descriptives_table[11,4] <- max(RawDataHH$m4_q103_15_lpg_distance, na.rm = TRUE)

lpg_descriptives_table <- lpg_descriptives_table[,-1]

lpg_descriptives_table

############################
####### TABLE OF LPG NON-ADOPTION
####### TABLE NOT SHOWN IN MANUSCRIPT, DATA SUPPORT FINDINGS
############################
sw.sum.nonadopters = data.frame(matrix(NA, 4, 1))

sw.sum.nonadopters[1,1] = round(w.mean(nolpg$m4_q105_1_nlpg_unavailable, nolpg[,395]), 2)
sw.sum.nonadopters[2,1] = round(w.mean(nolpg$m4_q105_2_nlpg_connect_expensive, nolpg[,395]), 2)
sw.sum.nonadopters[3,1] = round(w.mean(nolpg$m4_q105_3_nlpg_monthly_expensive, nolpg[,395]), 2)
sw.sum.nonadopters[4,1] = round(w.mean(nolpg$m4_q105_4_nlpg_dk, nolpg[,395]), 2)

rownames(sw.sum.nonadopters) = c("Unavailable", "Installation Cost", "Monthly Cost", "Lack of Information")

colnames(sw.sum.nonadopters) = c("All Non-Adopters (n=6712)")

sw.sum.nonadopters

############################
####### LPG DESCRIPTIVES BY TIME WITH STOVE > OR < 1 YEAR
####### TABLE NOT SHOWN IN MANUSCRIPT, DATA SUPPORT FINDINGS
############################

lpg_descriptives_time_vars <- c("Has LPG", "Cost of LPG connection [INR]", 
                                "LPG used per year [kg]", "Is satisfied with LPG", "Acquisition of tank: from authorized distributors", 
                                "Acquisition of tank: from market", "Cost: from authorized distributors [INR]", "Cost: from market [INR]", 
                                "Gas cylinders delivered to household", "One-way distance to get gas [km]", "Owns LPG and uses firewood",
                                "Cooks all dishes with LPG (†)")
lpg_descriptives_time <- data.frame(lpg_descriptives_time_vars)
lpg_descriptives_time["> 1 year (n=1343)"] <- NA
lpg_descriptives_time["< 1 year (n=517)"] <- NA
rownames(lpg_descriptives_time) <- lpg_descriptives_time_vars
lpg_descriptives_time <- lpg_descriptives_time[,-1]

lpg_descriptives_time[1,1] <- mean(RawDataHH_lpg_notoneyear$LPG, na.rm=TRUE)
lpg_descriptives_time[2,1] <- mean(RawDataHH_lpg_notoneyear$m4_q103_2_lpg_connect, na.rm=TRUE)
lpg_descriptives_time[3,1] <- mean(RawDataHH_lpg_notoneyear$kg_lpg, na.rm=TRUE)
lpg_descriptives_time[4,1] <- mean(RawDataHH_lpg_notoneyear$LPG_Satisfied, na.rm=TRUE)
lpg_descriptives_time[5,1] <- mean(RawDataHH_lpg_notoneyear$LPG_from_distributor, na.rm=TRUE)
lpg_descriptives_time[6,1] <- mean(RawDataHH_lpg_notoneyear$LPG_from_market, na.rm=TRUE)
lpg_descriptives_time[7,1] <- mean(RawDataHH_lpg_notoneyear$m4_q103_10_lpg_lcyl_dist_price, na.rm=TRUE)
lpg_descriptives_time[8,1] <- mean(RawDataHH_lpg_notoneyear$m4_q103_12_lpg_lcyl_mkt_price, na.rm=TRUE)
lpg_descriptives_time[9,1] <- mean(RawDataHH_lpg_notoneyear$m4_q103_14_lpg_cyl_deliver, na.rm=TRUE)
lpg_descriptives_time[10,1] <- mean(RawDataHH_lpg_notoneyear$m4_q103_15_lpg_distance, na.rm=TRUE)
lpg_descriptives_time[11,1] <- mean(RawDataHH_lpg_notoneyear$m4_q109_firewood, na.rm=TRUE)
lpg_descriptives_time[12,1] <- mean(RawDataHH_lpg_notoneyear$LPG_cooked == "111110", na.rm=TRUE)

lpg_descriptives_time[1,2] <- mean(RawDataHH_lpg_oneyear$LPG, na.rm=TRUE)
lpg_descriptives_time[2,2] <- mean(RawDataHH_lpg_oneyear$m4_q103_2_lpg_connect, na.rm=TRUE)
lpg_descriptives_time[3,2] <- mean(RawDataHH_lpg_oneyear$kg_lpg, na.rm=TRUE)
lpg_descriptives_time[4,2] <- mean(RawDataHH_lpg_oneyear$LPG_Satisfied, na.rm=TRUE)
lpg_descriptives_time[5,2] <- mean(RawDataHH_lpg_oneyear$LPG_from_distributor, na.rm=TRUE)
lpg_descriptives_time[6,2] <- mean(RawDataHH_lpg_oneyear$LPG_from_market, na.rm=TRUE)
lpg_descriptives_time[7,2] <- mean(RawDataHH_lpg_oneyear$m4_q103_10_lpg_lcyl_dist_price, na.rm=TRUE)
lpg_descriptives_time[8,2] <- mean(RawDataHH_lpg_oneyear$m4_q103_12_lpg_lcyl_mkt_price, na.rm=TRUE)
lpg_descriptives_time[9,2] <- mean(RawDataHH_lpg_oneyear$m4_q103_14_lpg_cyl_deliver, na.rm=TRUE)
lpg_descriptives_time[10,2] <- mean(RawDataHH_lpg_oneyear$m4_q103_15_lpg_distance, na.rm=TRUE)
lpg_descriptives_time[11,2] <- mean(RawDataHH_lpg_oneyear$m4_q109_firewood, na.rm=TRUE)
lpg_descriptives_time[12,2] <- mean(RawDataHH_lpg_oneyear$LPG_cooked == "111110", na.rm=TRUE)

lpg_descriptives_time <- lpg_descriptives_time[-1,]

lpg_descriptives_time


############################
####### TABLE OF LPG SATISFACTION
####### TABLE NOT SHOWN IN MANUSCRIPT, DATA SUPPORT FINDINGS
############################

lpg_satisfaction_plot <- c("Satisfied LPG Situation", "Dissatisfied LPG Situation", "Too expensive", "Poor availability",
                           "Too far to procure", "Poor maintenance services",
                           "More convenient than firewood", "Better for health than firewood")
sw.sum.lpgsatisfaction = data.frame(matrix(NA, length(lpg_satisfaction_plot),2))
colnames(sw.sum.lpgsatisfaction) = c("Primary (n=1093)", "Secondary (n=764)")
rownames(sw.sum.lpgsatisfaction) = lpg_satisfaction_plot

sw.sum.lpgsatisfaction[1,1] =  mean(RawDataHH_lpg_main$m4_q104_lpg_satisfy == 3, na.rm=TRUE)
sw.sum.lpgsatisfaction[2,1] =  mean(RawDataHH_lpg_main$m4_q104_lpg_satisfy == 1, na.rm=TRUE)
sw.sum.lpgsatisfaction[3,1] =  mean(RawDataHH_lpg_main$LPG_Unsatisfied_Expense, na.rm=TRUE)
sw.sum.lpgsatisfaction[4,1] =  mean(RawDataHH_lpg_main$LPG_Unsatisfied_Avail, na.rm=TRUE)
sw.sum.lpgsatisfaction[5,1] =  mean(RawDataHH_lpg_main$LPG_Unsatisfied_Far, na.rm=TRUE)
sw.sum.lpgsatisfaction[6,1] =  mean(RawDataHH_lpg_main$LPG_Unsatisfied_Maintain, na.rm=TRUE)
sw.sum.lpgsatisfaction[7,1] =  mean(RawDataHH_lpg_main$m5_q124_lpg_old_convenient == 1, na.rm=TRUE)
sw.sum.lpgsatisfaction[8,1] =  mean(RawDataHH_lpg_main$m5_q125_lpg_old_health == 1, na.rm=TRUE)

sw.sum.lpgsatisfaction[1,2] =  mean(RawDataHH_lpg_notmain$m4_q104_lpg_satisfy == 3, na.rm=TRUE)
sw.sum.lpgsatisfaction[2,2] =  mean(RawDataHH_lpg_notmain$m4_q104_lpg_satisfy == 1, na.rm=TRUE)
sw.sum.lpgsatisfaction[3,2] =  mean(RawDataHH_lpg_notmain$LPG_Unsatisfied_Expense, na.rm=TRUE)
sw.sum.lpgsatisfaction[4,2] =  mean(RawDataHH_lpg_notmain$LPG_Unsatisfied_Avail, na.rm=TRUE)
sw.sum.lpgsatisfaction[5,2] =  mean(RawDataHH_lpg_notmain$LPG_Unsatisfied_Far, na.rm=TRUE)
sw.sum.lpgsatisfaction[6,2] =  mean(RawDataHH_lpg_notmain$LPG_Unsatisfied_Maintain, na.rm=TRUE)
sw.sum.lpgsatisfaction[7,2] =  mean(RawDataHH_lpg_notmain$m5_q124_lpg_old_convenient == 1, na.rm=TRUE)
sw.sum.lpgsatisfaction[8,2] =  mean(RawDataHH_lpg_notmain$m5_q125_lpg_old_health == 1, na.rm=TRUE)

sw.sum.lpgsatisfaction


############################
####### OVERALL COOKING ARRANGEMENT SATISFACTION, BY PRIMARY COOKING ARRANGEMENT
####### TABLE NOT SHOWN IN MANUSCRIPT, DATA SUPPORT FINDINGS
############################

cook_satisfaction <- c("Satisfied with primary cooking arrangement", "Unsatisfied with primary cooking arrangement", 
                       "Unsatisfied: Excessive smoke", "Unsatisfied: Too expensive", "Unsatisfied: Too dangerous",
                       "Unsatisfied: Too time consuming", "Unsatisfied: Too difficult", "Has good quality of cooking", 
                       "Satisfied with primary cooking fuel availability", "Unsatisfied with primary cooking fuel availability", "Cooks less because of poor fuel availability", 
                       "Thinks cookstove impacts health", "LPG cooking is more convenient", "LPG cooking is better for health than traditional")
cook_satisfaction_table <- data.frame(matrix(NA,length(cook_satisfaction),3))
colnames(cook_satisfaction_table) <- c("Firewood (n=5364)", "Dung (n=1757)", "LPG (n=1093)")
rownames(cook_satisfaction_table) <- cook_satisfaction

cook_satisfaction_table[1,1] <- mean(RawDataHH_fw$m5_q122_cook_satisfy==3, na.rm=TRUE)
cook_satisfaction_table[2,1] <- mean(RawDataHH_fw$m5_q122_cook_satisfy==1, na.rm=TRUE)
cook_satisfaction_table[3,1] <- mean(RawDataHH_fw$m5_q121_cook_smoke, na.rm=TRUE)
cook_satisfaction_table[4,1] <- mean(RawDataHH_fw$m5_q121_cook_expensive, na.rm=TRUE)
cook_satisfaction_table[5,1] <- mean(RawDataHH_fw$m5_q121_cook_danger, na.rm=TRUE)
cook_satisfaction_table[6,1] <- mean(RawDataHH_fw$m5_q121_cook_muchtime, na.rm=TRUE)
cook_satisfaction_table[7,1] <- mean(RawDataHH_fw$m5_q121_cook_difficult, na.rm=TRUE)
cook_satisfaction_table[8,1] <- mean(RawDataHH_fw$m5_q121_cook_quality, na.rm=TRUE)
cook_satisfaction_table[9,1] <- mean(RawDataHH_fw$m5_q119_main_cookfuel_avail==3, na.rm=TRUE)
cook_satisfaction_table[10,1] <- mean(RawDataHH_fw$m5_q119_main_cookfuel_avail==1, na.rm=TRUE)
cook_satisfaction_table[11,1] <- mean(RawDataHH_fw$m5_q119_1_main_cookfuel_less, na.rm=TRUE)
cook_satisfaction_table[12,1] <- mean(RawDataHH_fw$m5_q123_cook_health, na.rm=TRUE)
cook_satisfaction_table[13,1] <- mean(RawDataHH_fw$m5_q124_lpg_old_convenient==1, na.rm=TRUE)
cook_satisfaction_table[14,1] <- mean(RawDataHH_fw$m5_q125_lpg_old_health==1, na.rm=TRUE)

cook_satisfaction_table[1,2] <- mean(RawDataHH_dung$m5_q122_cook_satisfy==3, na.rm=TRUE)
cook_satisfaction_table[2,2] <- mean(RawDataHH_dung$m5_q122_cook_satisfy==1, na.rm=TRUE)
cook_satisfaction_table[3,2] <- mean(RawDataHH_dung$m5_q121_cook_smoke, na.rm=TRUE)
cook_satisfaction_table[4,2] <- mean(RawDataHH_dung$m5_q121_cook_expensive, na.rm=TRUE)
cook_satisfaction_table[5,2] <- mean(RawDataHH_dung$m5_q121_cook_danger, na.rm=TRUE)
cook_satisfaction_table[6,2] <- mean(RawDataHH_dung$m5_q121_cook_muchtime, na.rm=TRUE)
cook_satisfaction_table[7,2] <- mean(RawDataHH_dung$m5_q121_cook_difficult, na.rm=TRUE)
cook_satisfaction_table[8,2] <- mean(RawDataHH_dung$m5_q121_cook_quality, na.rm=TRUE)
cook_satisfaction_table[9,2] <- mean(RawDataHH_dung$m5_q119_main_cookfuel_avail==3, na.rm=TRUE)
cook_satisfaction_table[10,2] <- mean(RawDataHH_dung$m5_q119_main_cookfuel_avail==1, na.rm=TRUE)
cook_satisfaction_table[11,2] <- mean(RawDataHH_dung$m5_q119_1_main_cookfuel_less, na.rm=TRUE)
cook_satisfaction_table[12,2] <- mean(RawDataHH_dung$m5_q123_cook_health, na.rm=TRUE)
cook_satisfaction_table[13,2] <- mean(RawDataHH_dung$m5_q124_lpg_old_convenient==1, na.rm=TRUE)
cook_satisfaction_table[14,2] <- mean(RawDataHH_dung$m5_q125_lpg_old_health==1, na.rm=TRUE)

cook_satisfaction_table[1,3] <- mean(RawDataHH_lpg_main$m5_q122_cook_satisfy==3, na.rm=TRUE)
cook_satisfaction_table[2,3] <- mean(RawDataHH_lpg_main$m5_q122_cook_satisfy==1, na.rm=TRUE)
cook_satisfaction_table[3,3] <- mean(RawDataHH_lpg_main$m5_q121_cook_smoke, na.rm=TRUE)
cook_satisfaction_table[4,3] <- mean(RawDataHH_lpg_main$m5_q121_cook_expensive, na.rm=TRUE)
cook_satisfaction_table[5,3] <- mean(RawDataHH_lpg_main$m5_q121_cook_danger, na.rm=TRUE)
cook_satisfaction_table[6,3] <- mean(RawDataHH_lpg_main$m5_q121_cook_muchtime, na.rm=TRUE)
cook_satisfaction_table[7,3] <- mean(RawDataHH_lpg_main$m5_q121_cook_difficult, na.rm=TRUE)
cook_satisfaction_table[8,3] <- mean(RawDataHH_lpg_main$m5_q121_cook_quality, na.rm=TRUE)
cook_satisfaction_table[9,3] <- mean(RawDataHH_lpg_main$m5_q119_main_cookfuel_avail==3, na.rm=TRUE)
cook_satisfaction_table[10,3] <- mean(RawDataHH_lpg_main$m5_q119_main_cookfuel_avail==1, na.rm=TRUE)
cook_satisfaction_table[11,3] <- mean(RawDataHH_lpg_main$m5_q119_1_main_cookfuel_less, na.rm=TRUE)
cook_satisfaction_table[12,3] <- mean(RawDataHH_lpg_main$m5_q123_cook_health, na.rm=TRUE)
cook_satisfaction_table[13,3] <- mean(RawDataHH_lpg_main$m5_q124_lpg_old_convenient==1, na.rm=TRUE)
cook_satisfaction_table[14,3] <- mean(RawDataHH_lpg_main$m5_q125_lpg_old_health==1, na.rm=TRUE)

cook_satisfaction_table


############################
####### DISHES COOKED, BY OWNING LPG STOVE ONE YEAR OR GREATER THAN ONE YEAR
####### TABLE NOT SHOWN IN MANUSCRIPT, DATA SUPPORT FINDINGS
############################

dishes <- c("Chapattis", "Vegetables", "Rice", "Tea/Snack", "Milk")
dishes_table_yrs <- data.frame(matrix(NA,length(dishes),2))
colnames(dishes_table_yrs) <- c("One year", "> One year")
rownames(dishes_table_yrs) <- dishes

dishes_table_yrs[1,1] <- mean(RawDataHH_lpg_oneyear$m4_q103_3_1_lpg_chapattis, na.rm=TRUE)
dishes_table_yrs[2,1] <- mean(RawDataHH_lpg_oneyear$m4_q103_3_2_lpg_veg, na.rm=TRUE)
dishes_table_yrs[3,1] <- mean(RawDataHH_lpg_oneyear$m4_q103_3_3_lpg_rice, na.rm=TRUE)
dishes_table_yrs[4,1] <- mean(RawDataHH_lpg_oneyear$m4_q103_3_4_lpg_tea_snack, na.rm=TRUE)
dishes_table_yrs[5,1] <- mean(RawDataHH_lpg_oneyear$m4_q103_3_5_lpg_milk, na.rm=TRUE)

dishes_table_yrs[1,2] <- mean(RawDataHH_lpg_notoneyear$m4_q103_3_1_lpg_chapattis, na.rm=TRUE)
dishes_table_yrs[2,2] <- mean(RawDataHH_lpg_notoneyear$m4_q103_3_2_lpg_veg, na.rm=TRUE)
dishes_table_yrs[3,2] <- mean(RawDataHH_lpg_notoneyear$m4_q103_3_3_lpg_rice, na.rm=TRUE)
dishes_table_yrs[4,2] <- mean(RawDataHH_lpg_notoneyear$m4_q103_3_4_lpg_tea_snack, na.rm=TRUE)
dishes_table_yrs[5,2] <- mean(RawDataHH_lpg_notoneyear$m4_q103_3_5_lpg_milk, na.rm=TRUE)

foodcooked_ <- ggplot(dishes_table, aes(x=dishes, y=dishes_table_yrs$Percent)) + 
  geom_bar(stat="identity") + scale_x_discrete(limits=dishes) + ylim(c(0,1)) +
  geom_text(aes(label=dishes_table$n), color="white", vjust=1.6) +
  ylab("Percent of households") + xlab("Dishes cooked") + ggtitle("Fraction of LPG-owning households cooking dishes with LPG (n=1851)") + 
  theme_gray() + 
  theme(axis.text.x = element_text(size=20)) 


####################################################################################################################
######################################      PRODUCTION OF FIGURES             ###################################### 
######################################      DATA ANALYSIS FOR MANUSCRIPT      ###################################### 
####################################################################################################################

####################
##### FIGURE 1
####################

fueltypes <- c("Firewood and chips", "Dung", "LPG", "Agro Residues")
fuels_used <- data.frame(fueltypes)
fuels_used["%"] <- NA
fuels_used[1,2] <- svymean(RawDataHH$m4_q109_firewood, sdesign, na.rm=TRUE)
fuels_used[2,2] <- svymean(RawDataHH$m4_q113_dungcake, sdesign, na.rm=TRUE)
fuels_used[3,2] <- svymean(RawDataHH$m4_q103_lpg, sdesign, na.rm=TRUE)
fuels_used[4,2] <- svymean(RawDataHH$m4_q114_agro, sdesign, na.rm=TRUE)

fuels_used["n"] <- NA
fuels_used[1,3] <- sum(RawDataHH$m4_q109_firewood, na.rm=TRUE)
fuels_used[2,3] <- sum(RawDataHH$m4_q113_dungcake, na.rm=TRUE)
fuels_used[3,3] <- sum(RawDataHH$m4_q103_lpg, na.rm=TRUE)
fuels_used[4,3] <- sum(RawDataHH$m4_q114_agro, na.rm=TRUE)

fuels_used_plot <- ggplot(fuels_used, aes(x=fuels_used$fueltypes,y=fuels_used$`%`)) + 
  geom_bar(stat="identity") + scale_x_discrete(limits=fueltypes) + ylim(c(0,1)) +
  geom_text(aes(label=paste0("N=", fuels_used$n)), color="white", vjust=1.6, size=6) +
  ylab("Percent of households\n") + xlab("\nCooking fuels used") +
  theme_gray() + 
  theme(plot.title = element_text(size=22),
        axis.text = element_text(size=18),
        axis.title = element_text(size=20))

fuels_used_plot # print figure


####################
##### FIGURE 2
####################

lpg_years <- ggplot(RawDataHH, aes(RawDataHH$m4_q103_1_lpg_year)) + 
  geom_histogram()  + ggtitle("Years with LPG") + 
  ylab("Number of households") + xlab("Years") + theme_gray()
lpg_connect_cost <- ggplot(RawDataHH, aes(RawDataHH$m4_q103_2_lpg_connect)) + 
  geom_histogram()  + ggtitle("Cost of initial LPG connection") + 
  ylab("Number of households") + xlab("Cost [INR]") + theme_gray()
lpg_distance <-  ggplot(RawDataHH, aes(RawDataHH$m4_q103_15_lpg_distance)) + 
  geom_histogram()  + ggtitle("One-way distance to acquire LPG") + 
  ylab("Number of households") + xlab("Distance [km]") + theme_gray()
lpg_cost <- ggplot(RawDataHH, aes(RawDataHH$m4_q103_10_lpg_lcyl_dist_price)) +
  geom_histogram() + ggtitle("Cost of large cylinder from distributor") + 
  ylab("Number of households") + xlab("Price [INR]") + theme_gray() + xlim(c(0,1000))
lpg_cost_mkt <- ggplot(RawDataHH, aes(RawDataHH$m4_q103_12_lpg_lcyl_mkt_price)) + 
  geom_histogram()  + ggtitle("Cost of large cylinder from market") + 
  ylab("Number of households") + xlab("Price [INR]") + theme_gray() + xlim(c(0,1000))
lpg_cost_scylc <- ggplot(RawDataHH, aes(RawDataHH$m4_q103_11_lpg_scyl_dist_price)) + 
  geom_histogram()  + ggtitle("Cost of small cylinder from distributor") + 
  ylab("Number of households") + xlab("Price [INR]") + theme_gray() + xlim(c(0,1000))

grid.arrange(lpg_connect_cost, lpg_distance, lpg_cost, lpg_cost_mkt, lpg_cost_scylc, lpg_years) # print figure


####################
##### FIGURE 3
####################

primarygas <- ggplot(RawDataHH_lpg_main, aes(RawDataHH_lpg_main$kg_lpg_month)) + 
  geom_histogram(bins=50) + ggtitle("(A) LPG Consumption as a Primary Fuel (N=1093)") + xlim(c(0,30)) + ylim(c(0,400)) + 
  ylab("Number of households") + xlab("LPG purchased per month [kg]") + theme_gray() +
  geom_vline(aes(xintercept = 14.2), linetype="dashed", colour="grey20") + 
  geom_vline(aes(xintercept = 7.1), linetype="dashed", colour="grey20")  + 
  theme(plot.title = element_text(size=22),
        axis.text = element_text(size=18),
        axis.title = element_text(size=20))


primarygas_density <- ggplot(RawDataHH_lpg_main, aes(RawDataHH_lpg_main$kg_lpg_month)) + 
  geom_density(aes(y=..count..), fill="black") + ggtitle("(A) LPG Consumption as a Primary Fuel (N=1093)") + xlim(c(0,30)) + ylim(c(0,400)) + 
  ylab("Number of households") + xlab("LPG purchased per month [kg]") + theme_gray() +
  geom_vline(aes(xintercept = 14.2), linetype="dashed", colour="grey20") + 
  geom_vline(aes(xintercept = 7.1), linetype="dashed", colour="grey20")  + 
  theme(plot.title = element_text(size=22),
        axis.text = element_text(size=18),
        axis.title = element_text(size=20))


secondarygas <- ggplot(RawDataHH_lpg_notmain, aes(RawDataHH_lpg_notmain$kg_lpg_month)) + 
  geom_histogram(bins=50) + ggtitle("(B) LPG Consumption as a Secondary Fuel (N=764)") + xlim(c(0,30)) + ylim(c(0,400)) + 
  ylab("Number of households") + xlab("LPG purchased per month [kg]") + theme_gray() + 
  geom_vline(aes(xintercept = 14.2), linetype="dashed", colour="grey20") + 
  geom_vline(aes(xintercept = 7.1), linetype="dashed", colour="grey20")  + 
  theme(plot.title = element_text(size=22),
        axis.text = element_text(size=18),
        axis.title = element_text(size=20))

secondarygas_density <- ggplot(RawDataHH_lpg_notmain, aes(RawDataHH_lpg_notmain$kg_lpg_month)) + 
  geom_density(aes(y=..count..), fill="black") + ggtitle("(B) LPG Consumption as a Secondary Fuel (N=764)") + xlim(c(0,30)) + ylim(c(0,400)) + 
  ylab("Number of households") + xlab("LPG purchased per month [kg]") + theme_gray() + 
  geom_vline(aes(xintercept = 14.2), linetype="dashed", colour="grey20") + 
  geom_vline(aes(xintercept = 7.1), linetype="dashed", colour="grey20")  + 
  theme(plot.title = element_text(size=22),
        axis.text = element_text(size=18),
        axis.title = element_text(size=20))

png("/Users/carlosgould/Dropbox/LPG Use Survey/Manuscript/Figures/lpgusagekg.png",
    width=700, height=700)
grid.arrange(primarygas, secondarygas) # print figure
dev.off()

grid.arrange(primarygas_density, secondarygas_density) # print figure


########################
###   FIGURE 4
########################

dishes <- c("Chapattis", "Vegetables", "Rice", "Tea/Snack", "Milk")
dishes_table <- data.frame(matrix(NA,length(dishes),2))
colnames(dishes_table) <- c("Percent", "n")
rownames(dishes_table) <- dishes

dishes_table[1,1] <- mean(RawDataHH_lpg$m4_q103_3_1_lpg_chapattis, na.rm=TRUE)
dishes_table[2,1] <- mean(RawDataHH_lpg$m4_q103_3_2_lpg_veg, na.rm=TRUE)
dishes_table[3,1] <- mean(RawDataHH_lpg$m4_q103_3_3_lpg_rice, na.rm=TRUE)
dishes_table[4,1] <- mean(RawDataHH_lpg$m4_q103_3_4_lpg_tea_snack, na.rm=TRUE)
dishes_table[5,1] <- mean(RawDataHH_lpg$m4_q103_3_5_lpg_milk, na.rm=TRUE)

dishes_table[1,2] <- sum(RawDataHH_lpg$m4_q103_3_1_lpg_chapattis, na.rm=TRUE)
dishes_table[2,2] <- sum(RawDataHH_lpg$m4_q103_3_2_lpg_veg, na.rm=TRUE)
dishes_table[3,2] <- sum(RawDataHH_lpg$m4_q103_3_3_lpg_rice, na.rm=TRUE)
dishes_table[4,2] <- sum(RawDataHH_lpg$m4_q103_3_4_lpg_tea_snack, na.rm=TRUE)
dishes_table[5,2] <- sum(RawDataHH_lpg$m4_q103_3_5_lpg_milk, na.rm=TRUE)

foodcooked_alllpgowners <- ggplot(dishes_table, aes(x=dishes, y=dishes_table$Percent)) + 
  geom_bar(stat="identity") + scale_x_discrete(limits=dishes) + ylim(c(0,1)) +
  geom_text(aes(label=paste0("N=", dishes_table$n)), color="white", vjust=1.6, size=6) +
  ylab("Fraction of households") + xlab("Dishes cooked") + 
  ggtitle("Fraction of LPG-owning households cooking dishes with LPG (n=1851)") + 
  theme_gray() + 
  theme(plot.title = element_text(size=22),
        axis.text = element_text(size=18),
        axis.title = element_text(size=20))

foodcooked_alllpgowners # print figure -- might take a moment

png("/Users/carlosgould/Dropbox/LPG Use Survey/Manuscript/Figures/foodcooked.png",
    width=900, height=700)
ggplot(dishes_table, aes(x=dishes, y=dishes_table$Percent)) + 
  geom_bar(stat="identity") + scale_x_discrete(limits=dishes) + ylim(c(0,1)) +
  geom_text(aes(label=paste0("N=", dishes_table$n)), color="white", vjust=1.6, size=6) +
  ylab("Fraction of households") + xlab("Dishes cooked") + 
  ggtitle("Fraction of LPG-owning households cooking dishes with LPG (n=1851)") + 
  theme_gray() + 
  theme(plot.title = element_text(size=22),
        axis.text = element_text(size=18),
        axis.title = element_text(size=20))
dev.off()

########################
###   FIGURE 5
########################

# DISHES COOKED WITH LPG WHEN IT IS THE PRIMARY FUEL

dishes <- c("Chapattis", "Vegetables", "Rice", "Tea/Snack", "Milk")

dishes_table_lpg <- data.frame(matrix(NA,length(dishes),2))
colnames(dishes_table_lpg) <- c("Percent", "n")
rownames(dishes_table_lpg) <- dishes

dishes_table_lpg[1,1] <- mean(RawDataHH_lpg_main$m4_q103_3_1_lpg_chapattis, na.rm=TRUE)
dishes_table_lpg[2,1] <- mean(RawDataHH_lpg_main$m4_q103_3_2_lpg_veg, na.rm=TRUE)
dishes_table_lpg[3,1] <- mean(RawDataHH_lpg_main$m4_q103_3_3_lpg_rice, na.rm=TRUE)
dishes_table_lpg[4,1] <- mean(RawDataHH_lpg_main$m4_q103_3_4_lpg_tea_snack, na.rm=TRUE)
dishes_table_lpg[5,1] <- mean(RawDataHH_lpg_main$m4_q103_3_5_lpg_milk, na.rm=TRUE)

dishes_table_lpg[1,2] <- sum(RawDataHH_lpg_main$m4_q103_3_1_lpg_chapattis, na.rm=TRUE)
dishes_table_lpg[2,2] <- sum(RawDataHH_lpg_main$m4_q103_3_2_lpg_veg, na.rm=TRUE)
dishes_table_lpg[3,2] <- sum(RawDataHH_lpg_main$m4_q103_3_3_lpg_rice, na.rm=TRUE)
dishes_table_lpg[4,2] <- sum(RawDataHH_lpg_main$m4_q103_3_4_lpg_tea_snack, na.rm=TRUE)
dishes_table_lpg[5,2] <- sum(RawDataHH_lpg_main$m4_q103_3_5_lpg_milk, na.rm=TRUE)

dishes_plot_lpg_main <- ggplot(dishes_table_lpg, aes(x=dishes, y=dishes_table_lpg$Percent)) + 
  geom_bar(stat="identity") + scale_x_discrete(limits=dishes) + ylim(c(0,1)) +
  geom_text(aes(label=paste0("N=", dishes_table_lpg$n)), color="white", vjust=1.6, size=6) +
  ylab("Fraction of households\n") + xlab("\nDishes cooked\n") + 
  ggtitle("(A) Fraction of households cooking dishes with LPG (primary) (n=1093)\n") + 
  theme_gray() + 
  theme(plot.title = element_text(size=22),
        axis.text = element_text(size=18),
        axis.title = element_text(size=20))

dishes_plot_lpg_main # print figure

# DISHES COOKED WITH LPG WHEN IT IS THE SECONDARY FUEL

dishes <- c("Chapattis", "Vegetables", "Rice", "Tea/Snack", "Milk")
dishes_table_gas_sec <- data.frame(matrix(NA,length(dishes),2))
colnames(dishes_table_gas_sec) <- c("Percent", "n")
rownames(dishes_table_gas_sec) <- dishes

dishes_table_gas_sec[1,1] <- mean(RawDataHH_lpg_notmain$m4_q103_3_1_lpg_chapattis, na.rm=TRUE)
dishes_table_gas_sec[2,1] <- mean(RawDataHH_lpg_notmain$m4_q103_3_2_lpg_veg, na.rm=TRUE)
dishes_table_gas_sec[3,1] <- mean(RawDataHH_lpg_notmain$m4_q103_3_3_lpg_rice, na.rm=TRUE)
dishes_table_gas_sec[4,1] <- mean(RawDataHH_lpg_notmain$m4_q103_3_4_lpg_tea_snack, na.rm=TRUE)
dishes_table_gas_sec[5,1] <- mean(RawDataHH_lpg_notmain$m4_q103_3_5_lpg_milk, na.rm=TRUE)

dishes_table_gas_sec[1,2] <- sum(RawDataHH_lpg_notmain$m4_q103_3_1_lpg_chapattis, na.rm=TRUE)
dishes_table_gas_sec[2,2] <- sum(RawDataHH_lpg_notmain$m4_q103_3_2_lpg_veg, na.rm=TRUE)
dishes_table_gas_sec[3,2] <- sum(RawDataHH_lpg_notmain$m4_q103_3_3_lpg_rice, na.rm=TRUE)
dishes_table_gas_sec[4,2] <- sum(RawDataHH_lpg_notmain$m4_q103_3_4_lpg_tea_snack, na.rm=TRUE)
dishes_table_gas_sec[5,2] <- sum(RawDataHH_lpg_notmain$m4_q103_3_5_lpg_milk, na.rm=TRUE)

dishes_plot_lpg_sec <- ggplot(dishes_table_gas_sec, aes(x=dishes, y=dishes_table_gas_sec$Percent)) + 
  geom_bar(stat="identity") + scale_x_discrete(limits=dishes) + ylim(c(0,1)) +
  geom_text(aes(label=paste0("N=", dishes_table_gas_sec$n)), color="white", vjust=1.6, size=6) +
  ylab("Fraction of households\n") + xlab("\nDishes cooked") + 
  ggtitle("(B) Fraction of households cooking dishes with LPG (secondary) (n=764)\n") + 
  theme_gray( ) + 
  theme(plot.title = element_text(size=22),
        axis.text = element_text(size=18),
        axis.title = element_text(size=20))

dishes_plot_lpg_sec # print figure

grid.arrange(dishes_plot_lpg_main, dishes_plot_lpg_sec, nrow=2) # combine plots for final figure


####################################################
#########  ** Variable creation for dishes cooked analyses
####################################################

#what combinations of food do people cook
RawDataHH_lpg["LPG_cooked"] <- paste(RawDataHH_lpg$m4_q103_3_1_lpg_chapattis, RawDataHH_lpg$m4_q103_3_2_lpg_veg, RawDataHH_lpg$m4_q103_3_3_lpg_rice, RawDataHH_lpg$m4_q103_3_4_lpg_tea_snack, RawDataHH_lpg$m4_q103_3_5_lpg_milk, RawDataHH_lpg$m4_q103_3_6_lpg_water, sep = "")

#recoding all combinations of food cooked
RawDataHH_lpg["LPG_cooked_description"] <- ifelse(RawDataHH_lpg$LPG_cooked == "000000", "Nothing",
                                                  ifelse(RawDataHH_lpg$LPG_cooked == "000001", "Water only",
                                                         ifelse(RawDataHH_lpg$LPG_cooked == "000010", "Milk only",
                                                                ifelse(RawDataHH_lpg$LPG_cooked == "000100", "Tea/Snack only",
                                                                       ifelse(RawDataHH_lpg$LPG_cooked == "000110", "Tea/Snack and Milk",
                                                                              ifelse(RawDataHH_lpg$LPG_cooked == "000111", "Tea/Snack, Milk, and Water",
                                                                                     ifelse(RawDataHH_lpg$LPG_cooked == "001000", "Rice only",
                                                                                            ifelse(RawDataHH_lpg$LPG_cooked == "001100", "Rice and Tea/Snack",
                                                                                                   ifelse(RawDataHH_lpg$LPG_cooked == "001110", "Rice, Tea/Snack, Milk",
                                                                                                          ifelse(RawDataHH_lpg$LPG_cooked == "001111", "Rice, Tea/Snack, Milk, Water",
                                                                                                                 ifelse(RawDataHH_lpg$LPG_cooked == "010000", "Vegetables only",
                                                                                                                        ifelse(RawDataHH_lpg$LPG_cooked == "010001", "Vegetables and Water",
                                                                                                                               ifelse(RawDataHH_lpg$LPG_cooked == "010100", "Vegetables and Rice",
                                                                                                                                      ifelse(RawDataHH_lpg$LPG_cooked == "010100", "Vegetables and Rice",
                                                                                                                                             ifelse(RawDataHH_lpg$LPG_cooked == "111111", "Chapattis, Vegetables, Rice, Tea/Snack, Milk, and Water",
                                                                                                                                                    ifelse(RawDataHH_lpg$LPG_cooked == "111110", "Chapattis, Vegetables, Rice, Tea/Snack, and Milk",
                                                                                                                                                           ifelse(RawDataHH_lpg$LPG_cooked == "111100", "Chapattis, Vegetables, Rice, and Tea/Snack",
                                                                                                                                                                  ifelse(RawDataHH_lpg$LPG_cooked == "111010", "Chapattis, Vegetables, Rice, and Milk",
                                                                                                                                                                         ifelse(RawDataHH_lpg$LPG_cooked == "111000", "Chapattis, Vegetables, and Rice",
                                                                                                                                                                                ifelse(RawDataHH_lpg$LPG_cooked == "110111", "Chapattis, Vegetables, Tea/Snack, Milk, and Water",
                                                                                                                                                                                       ifelse(RawDataHH_lpg$LPG_cooked == "110110", "Chapattis, Vegetables, Tea/Snack, and Milk",
                                                                                                                                                                                              ifelse(RawDataHH_lpg$LPG_cooked == "110100", "Chapattis, Vegetables, and Tea/Snack",
                                                                                                                                                                                                     ifelse(RawDataHH_lpg$LPG_cooked == "110000", "Chapattis and Vegetables",
                                                                                                                                                                                                            ifelse(RawDataHH_lpg$LPG_cooked == "101110", "Chapattis, Rice, Tea/Snack, and Milk",
                                                                                                                                                                                                                   ifelse(RawDataHH_lpg$LPG_cooked == "101100", "Chapattis, Rice, and Tea/Snack",
                                                                                                                                                                                                                          ifelse(RawDataHH_lpg$LPG_cooked == "111101", "Chapattis, Vegetables, Rice, Tea/Snack, and Water",
                                                                                                                                                                                                                                 ifelse(RawDataHH_lpg$LPG_cooked == "110010", "Chapattis, Vegetables, Tea/Snack, and Milk",
                                                                                                                                                                                                                                        ifelse(RawDataHH_lpg$LPG_cooked == "100111", "Chapattis, Tea/Snack, Milk, and Water",
                                                                                                                                                                                                                                               ifelse(RawDataHH_lpg$LPG_cooked == "100110", "Chapattis, Tea/Snack, and Milk",
                                                                                                                                                                                                                                                      ifelse(RawDataHH_lpg$LPG_cooked == "100100", "Chapattis and Tea/Snack",
                                                                                                                                                                                                                                                             ifelse(RawDataHH_lpg$LPG_cooked == "100000", "Chapattis",
                                                                                                                                                                                                                                                                    ifelse(RawDataHH_lpg$LPG_cooked == "011111", "Vegetables, Rice, Tea/Snack, Milk, and Water",
                                                                                                                                                                                                                                                                           ifelse(RawDataHH_lpg$LPG_cooked == "011110", "Vegetables, Rice, Tea/Snack, and Milk",
                                                                                                                                                                                                                                                                                  ifelse(RawDataHH_lpg$LPG_cooked == "011101", "Vegetables, Rice, Tea/Snack, and Water",
                                                                                                                                                                                                                                                                                         ifelse(RawDataHH_lpg$LPG_cooked == "011100", "Vegetables, Rice, and Tea/Snack",
                                                                                                                                                                                                                                                                                                ifelse(RawDataHH_lpg$LPG_cooked == "011010", "Vegetables, Rice, and Milk",
                                                                                                                                                                                                                                                                                                       ifelse(RawDataHH_lpg$LPG_cooked == "011000", "Vegetables and Rice",
                                                                                                                                                                                                                                                                                                              ifelse(RawDataHH_lpg$LPG_cooked == "010111", "Vegetables, Tea/Snack, Milk, and Water",
                                                                                                                                                                                                                                                                                                                     ifelse(RawDataHH_lpg$LPG_cooked == "010110", "Vegetables, Tea/Snack, and Milk",
                                                                                                                                                                                                                                                                                                                            ifelse(RawDataHH_lpg$LPG_cooked == "100010", "Chapattis and Milk",
                                                                                                                                                                                                                                                                                                                                   RawDataHH_lpg$LPG_cooked))))))))))))))))))))))))))))))))))))))))


############################
##############  FIGURE 6
############################

food_combos_cooked <- data.frame(table(RawDataHH_lpg$LPG_cooked_description))

foods <- c("m4_q103_3_1_lpg_chapattis", "m4_q103_3_2_lpg_veg", "m4_q103_3_3_lpg_rice", "m4_q103_3_4_lpg_tea_snack", "m4_q103_3_5_lpg_milk")
food_df <- RawDataHH_lpg[foods]
colnames(food_df) <- c("Chapattis", "Vegetables", "Rice", "Tea/Snack", "Milk")
food_matrix <- as.matrix(food_df)
food_corrs <- cor(food_matrix, use="na.or.complete")

food_corr_plot <- corrplot.mixed(food_corrs, lower="number",upper="color")
food_corr_plot

food_corr_plot_overall <- ggcorr(data=NULL, cor_matrix=food_corrs, label=T, low = "darkred", mid = "white", high = "steelblue",
       legend.size = 16, size=6, label_size = 8) 

food_corr_plot_overall # print figure


############################
##############  FIGURE 7
############################

# dishes cooked when LPG is a primary fuel

food_primary <- RawDataHH_lpg_main[foods]
colnames(food_primary) <- c("Chapattis", "Vegetables", "Rice", "Tea/Snack", "Milk")
food_primary_matrix <- as.matrix(food_primary)
food_primary_corrs <- cor(food_primary_matrix, use="na.or.complete")

# dishes cooked when LPG is a secondary fuel

food_secondary <- RawDataHH_lpg_notmain[foods]
colnames(food_secondary) <- c("Chapattis", "Vegetables", "Rice", "Tea/Snack", "Milk")
food_secondary_matrix <- as.matrix(food_secondary)
food_secondary_corrs <- cor(food_secondary_matrix, use="na.or.complete")

#food corrs by lpg primary and secondary

primary <- ggcorr(data=NULL, cor_matrix=food_primary_corrs, label=T, low = "darkred", mid = "white", high = "steelblue", 
                  legend.size = 16, size=6, label_size = 8)
secondary <- ggcorr(data=NULL, cor_matrix=food_secondary_corrs, label=T, low = "darkred", mid = "white", high = "steelblue",
                    legend.size = 16, size=6, label_size = 8) 


grid.arrange(primary, secondary, ncol=2) # print final plot (combined primary and secondary)

############################
##############  FIGURE 8
############################

####### PERCEPTIONS PLOT
cook_satisfaction_plot <- c("Satisfied Overall", "Unsatisfied Overall", 
                       "Excessive smoke", "Too expensive", "Too dangerous",
                       "Too time consuming", "Too difficult", "Good quality cooking", 
                       "Satisfied with fuel availability", "Unsatisfied with availability", "Cooks less, poor availability", 
                       "Cookstove impacts health", "LPG cooking more convenient", "LPG cooking better for health")
cook_satisfaction_table_plot <- cook_satisfaction_table
cook_satisfaction_table_plot$perception <- cook_satisfaction_plot
positive_plot <- cook_satisfaction_table_plot[c(1,8:9,13:14),]
negative_plot <- cook_satisfaction_table_plot[c(2:7,10:12),]

positive_melt <- melt(positive_plot)
negative_melt <- melt(negative_plot)

brks <- c(0,.25,.5,.75,1)
lbls <- c(0,.25,.5,.75,1)

# Plot
p_order <- c("Satisfied Overall", "Good quality cooking", "LPG cooking better for health", "LPG cooking more convenient", "Satisfied with fuel availability")
positive_perceptions <- ggplot(positive_melt, aes(x = perception, y = value, fill = variable)) +   # Fill column
  geom_bar(stat = "identity", position = "dodge", width = .6) +   # draw the bars
  scale_y_continuous(breaks = brks,   # Breaks
                     labels = lbls) + 
  ggtitle("(A) Positive Cooking Fuel Perceptions") +
  scale_x_discrete(limits=p_order) + 
  theme_gray() +
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank()) +   # Centre plot title
  scale_fill_manual(values=c("green4", "#feb24c", "dodgerblue4")) + # Color palette
  xlab("") + ylab("Fraction of households") + guides(fill=guide_legend(title="Primary Fuel")) + 
  theme(plot.title = element_text(size=22),
        axis.text = element_text(size=18),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.title = element_text(size=20), 
        legend.text = element_text(size=18),
        legend.title = element_text(size=18))


n_order <- c("Unsatisfied Overall",  "Excessive smoke", "Too expensive", "Too dangerous", "Too time consuming", "Too difficult", "Unsatisfied with availability", "Cooks less, poor availability", "Cookstove impacts health")
negative_perceptions <- ggplot(negative_melt, aes(x = perception, y = value, fill = variable)) +   # Fill column
  geom_bar(stat = "identity", position = "dodge", width = .6) +   # draw the bars
  scale_y_continuous(breaks = brks,   # Breaks
                     labels = lbls) + 
  scale_x_discrete(limits=n_order) + 
  ggtitle("(B) Negative Cooking Fuel Perceptions") +
  theme_gray() +
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank()) +   # Centre plot title
  scale_fill_manual(values=c("green4", "#feb24c", "dodgerblue4")) + # Color palette
  xlab("") + ylab("Fraction of households") + guides(fill=guide_legend(title="Primary Fuel")) + 
  theme(plot.title = element_text(size=22),
        axis.text = element_text(size=18),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.title = element_text(size=20), 
        legend.text = element_text(size=18),
        legend.title = element_text(size=18))

grid.arrange(positive_perceptions, negative_perceptions) # print final plot of perceptions of all main cooking fuel

####################
##### FIGURE 9
####################

sw.sum.lpgsatisfaction_plot <- sw.sum.lpgsatisfaction
sw.sum.lpgsatisfaction_plot$perception <- lpg_satisfaction_plot

lpg_melt <- melt(sw.sum.lpgsatisfaction_plot)

brks <- c(0,.25,.5,.75,1)
lbls <- c(0,.25,.5,.75,1)

# Plot
lpg_perceptions <- ggplot(lpg_melt, aes(x = perception, y = value, fill = variable)) +   # Fill column
  geom_bar(stat = "identity", position = "dodge", width = .6) +   # draw the bars
  scale_y_continuous(breaks = brks,   # Breaks
                     labels = lbls) + 
  scale_x_discrete(limits=lpg_satisfaction_plot) + 
  theme_gray() +  # Tufte theme from ggfortify
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank()) +   # Centre plot title
  scale_fill_manual(values=c("dodgerblue4", "dodgerblue1")) + # Color palette
  xlab("") + ylab("Fraction of households") + guides(fill=guide_legend(title="LPG Usage Group")) + 
  theme(plot.title = element_text(size=22),
        axis.text = element_text(size=18),
        axis.text.x = element_text(size=16, angle=45, hjust=1),
        axis.title = element_text(size=20), 
        legend.text = element_text(size=18),
        legend.title = element_text(size=18))

lpg_perceptions # print final plot of LPG perceptions

###############################################################################################
###################################### DATA ANALYSIS FOR SUPPORTING INFORMATION
###############################################################################################

################
## FIGURE A1
################

####  create state-level statistics

states <- data.frame(unique(RawDataHH$m1_q8_state))
states["n"] <- 0
states["lpg"] <- NA

for (i in 1:6) {
  states[i,2] <- sum(RawDataHH$Interviewed & RawDataHH$m1_q8_state == states[i,1], na.rm = TRUE) #n
  states[i,3] <- sum(RawDataHH$m4_q103_lpg & RawDataHH$m1_q8_state == states[i,1], na.rm = TRUE) #gas uptake
}

states["% lpg"] <- states$lpg / states$n

##### rename states

states["ST_NAME"] <- properCase(as.character(unique(states$unique.RawDataHH.m1_q8_state.)))
states[states=="West bengal"] <- "West Bengal"
states[states=="Uttar pradesh"] <- "Uttar Pradesh"
states[states=="Madhya pradesh"] <- "Madhya Pradesh"
states[states=="Odisha"] <- "Orissa" # Orissa was re-named Odisha in 2011 (but shapefile retains old nomenclature)

colnames(states) <- c("State", "n", "lpg", "Percent LPG Adoption", "ST_NAME")

####  create district-level statistics

districts <- data.frame(unique(RawDataHH$m1_q9_district))
districts["n"] <- 0
districts["lpg"] <- NA

for (i in 1:nrow(districts)) {
  districts[i,2] <- sum(RawDataHH$Interviewed & RawDataHH$m1_q9_district == districts[i,1]) #n
  districts[i,3] <- sum(RawDataHH$m4_q103_lpg & RawDataHH$m1_q9_district == districts[i,1]) #gas uptake
}

districts["% lpg"] <- districts$lpg / districts$n

#### rename districts

districts["District_Name"] <- properCase(as.character(unique(RawDataHH$m1_q9_district)))
districts[districts=="North twenty four parganas"] <- "North 24 Parganas"
districts[districts=="Khandwa (east nimar)"] <- "East Nimar"
districts[districts=="Paschim mednipur"] <- "Pashchim Medinipur"
districts[districts=="Bijnour"] <- "Bijnor"
districts[districts=="Bulandshahar"] <- "Bulandshahr"
districts[districts=="Siddharthnagar"] <- "Siddharth Nagar"
districts[districts=="Purwa champaran"] <- "Purba Champaran"

colnames(districts) <- c("District name", "n", "lpg", "Percent LPG Adoption", "DISTRICT")

##### shape file data

# state shapefile
india_shape <- readOGR(dsn = "~/Dataverse_Files/", layer = "INDIA") #### ** NOTE: SET YOUR FILE PATH ***

# district shapefile
districts_shape <- readOGR(dsn = "~/Dataverse_Files/", layer = "2011_Dist") #### ** NOTE: SET YOUR FILE PATH ***

# data join ACCESS to shapefile
districts_shape@data <- left_join(districts_shape@data, districts, by = "DISTRICT", all = TRUE)
india_shape@data <- left_join(india_shape@data, states, by = "ST_NAME", all = TRUE)

# remove NA districts (non ACCESS districts)
districts_shape_partial <- districts_shape[!is.na(districts_shape@data$`Percent LPG Adoption`), ]
india_shape_partial <- india_shape[!is.na(india_shape@data$`Percent LPG Adoption`),]

######## TWO MAPS TOGETHER: STATES AND DISTRICTS

country_map <- tm_shape(india_shape) + 
  tm_polygons(col = "white") +
  tm_borders(col = "black") +
  tm_shape(districts_shape_partial) +
  tm_polygons("Percent LPG Adoption", palette = "Blues") + 
  tm_borders(col = "gray50", lwd = 0.5) + 
  tm_legend(scale = 1.5, aes.color = c(borders = "black"), legend.position = c("right", "top"))

country_map     # may take several minutes

districts_map <- tm_shape(india_shape) + 
  tm_polygons(col="white") +
  tm_borders(col = "black") +
  tm_shape(india_shape_partial) + 
  tm_polygons("Percent LPG Adoption", palette = "Blues") +
  tm_borders(col = "gray50", lwd = 0.5) +
  tm_legend(scale = 1.5, aes.color = c(borders = "black"), legend.position = c("right", "top"))

districts_map     # may take several minutes

tmap_arrange(districts_map, country_map, ncol = 2) # combining maps

################
## FIGURE A2
################

villages <- unique(RawDataHH$m1_q11_village_code)
village_LPG <- as.data.frame(villages)
village_LPG$HHs <- NA
village_LPG$LPG <- NA
village_LPG$SFs <- NA

for(i in 1:714) {
  village_LPG$HHs[i] <- sum(RawDataHH$Interviewed[RawDataHH$m1_q11_village_code == village_LPG$villages[i]])
  village_LPG$LPG[i] <- sum(RawDataHH$LPG[RawDataHH$m1_q11_village_code == village_LPG$villages[i]])
  village_LPG$SFs[i] <- sum(RawDataHH$`Solid Fuels`[RawDataHH$m1_q11_village_code == village_LPG$villages[i]])
}

village <- ggplot(village_LPG, aes(village_LPG$LPG / village_LPG$HHs)) + 
  geom_histogram() + xlim(c(0,1)) + 
  ylab("Number of Villages") + xlab("Proportion of Households with LPG") + ggtitle("Distribution of LPG Uptake in Villages (N=714)") + 
  theme_gray()

districts <- unique(RawDataHH$m1_q9_district_code)
district_LPG <- as.data.frame(districts)
district_LPG$HHs <- NA
district_LPG$LPG <- NA
district_LPG$SFs <- NA

for(i in 1:51) {
  district_LPG$HHs[i] <- sum(RawDataHH$Interviewed[RawDataHH$m1_q9_district_code == district_LPG$districts[i]])
  district_LPG$LPG[i] <- sum(RawDataHH$LPG[RawDataHH$m1_q9_district_code == district_LPG$districts[i]])
  district_LPG$SFs[i] <- sum(RawDataHH$`Solid Fuels`[RawDataHH$m1_q9_district_code == district_LPG$districts[i]])
}

district <- ggplot(district_LPG, aes(district_LPG$LPG / district_LPG$HHs)) + 
  geom_histogram() + xlim(c(0,1)) + ylim(c(0,10)) + 
  ylab("Number of Districts") + xlab("Proportion of Households with LPG") + ggtitle("Distribution of LPG Uptake in Districts (N=51)") + 
  theme_gray()

district # print district individually

village # print village individually

grid.arrange(district, village) # print final figure of combined village and district uptake -- may take a bit


################
## FIGURE A3
################

stoves <- c("Three stone fire", "Mud stove", "Improved stove", "LPG stove", "Kerosene stove", "Electric stove")
stoves_used <- data.frame(stoves)
stoves_used["Percent"] <- NA
stoves_used["n"] <- NA

stoves_used[1,2] <- svymean(RawDataHH$`3stone`,sdesign, na.rm=TRUE)
stoves_used[2,2] <- svymean(RawDataHH$Mud,sdesign, na.rm=TRUE)
stoves_used[3,2] <- svymean(RawDataHH$Improved,sdesign, na.rm=TRUE)
stoves_used[4,2] <- svymean(RawDataHH$LPG,sdesign, na.rm=TRUE)
stoves_used[5,2] <- svymean(RawDataHH$Kerosene,sdesign, na.rm=TRUE)
stoves_used[6,2] <- svymean(RawDataHH$Electric,sdesign, na.rm=TRUE)

stoves_used[1,3] <- sum(RawDataHH$`3stone`, na.rm=TRUE)
stoves_used[2,3] <- sum(RawDataHH$Mud, na.rm=TRUE)
stoves_used[3,3] <- sum(RawDataHH$Improved, na.rm=TRUE)
stoves_used[4,3] <- sum(RawDataHH$LPG, na.rm=TRUE)
stoves_used[5,3] <- sum(RawDataHH$Kerosene, na.rm=TRUE)
stoves_used[6,3] <- sum(RawDataHH$Electric, na.rm=TRUE)

stoves_used_plot <- ggplot(stoves_used, aes(x=stoves_used$stoves, y=stoves_used$Percent)) + 
  geom_bar(stat="identity") + scale_x_discrete(limits=stoves) + ylim(c(0,1)) +
  geom_text(aes(label=stoves_used$n), color="white", vjust=1) +
  ylab("Percent of households") + xlab("Stoves") + ggtitle("Fraction of households with each stove type") +
  theme_gray()

stoves_used_plot # print figure

################
## FIGURE A4
################

#########   PRIMARY COOKING FUEL
# NOTE    1: firewood, 2: dung, 3: LPG

fuels <- c("Firewood and chips", "Dung", "LPG", "Other")
primary_fuels <- data.frame(fuels)
primary_fuels["%"] <- NA
primary_fuels[1,2] <- svymean(RawDataHH$m5_q118_main_cookfuel==1, sdesign, na.rm=TRUE)
primary_fuels[2,2] <- svymean(RawDataHH$m5_q118_main_cookfuel==2, sdesign, na.rm=TRUE)
primary_fuels[3,2] <- svymean(RawDataHH$m5_q118_main_cookfuel==3, sdesign, na.rm=TRUE)
primary_fuels[4,2] <- svymean(RawDataHH$m5_q118_main_cookfuel==4, sdesign, na.rm=TRUE)

primary_fuels["n"] <- NA
primary_fuels[1,3] <- sum(RawDataHH$m5_q118_main_cookfuel==1, na.rm=TRUE)
primary_fuels[2,3] <- sum(RawDataHH$m5_q118_main_cookfuel==2, na.rm=TRUE)
primary_fuels[3,3] <- sum(RawDataHH$m5_q118_main_cookfuel==3, na.rm=TRUE)
primary_fuels[4,3] <- sum(RawDataHH$m5_q118_main_cookfuel==4, na.rm=TRUE)

primary_fuels_plot <- ggplot(primary_fuels, aes(x=primary_fuels$fuels, y=primary_fuels$`%`)) + 
  geom_bar(stat="identity") + scale_x_discrete(limits=fuels) + ylim(c(0,1)) +
  geom_text(aes(label=primary_fuels$n), color="white", vjust=1.6) +
  ylab("Percent of households") + xlab("Primary cooking fuel") + ggtitle("Primary cooking fuel type") + 
  theme_gray()

primary_fuels_plot # print plot

################
#### FIGURE A5
################

dishes <- c("Chapattis", "Vegetables", "Rice", "Tea/Snack", "Milk", "Water boiling")
dishes_table_lpgoneyear <- data.frame(matrix(NA,length(dishes),2))
colnames(dishes_table_lpgoneyear) <- c("Percent", "n")
rownames(dishes_table_lpgoneyear) <- dishes

dishes_table_lpgoneyear[1,1] <- mean(RawDataHH_lpg_oneyear$m4_q103_3_1_lpg_chapattis, na.rm=TRUE)
dishes_table_lpgoneyear[2,1] <- mean(RawDataHH_lpg_oneyear$m4_q103_3_2_lpg_veg, na.rm=TRUE)
dishes_table_lpgoneyear[3,1] <- mean(RawDataHH_lpg_oneyear$m4_q103_3_3_lpg_rice, na.rm=TRUE)
dishes_table_lpgoneyear[4,1] <- mean(RawDataHH_lpg_oneyear$m4_q103_3_4_lpg_tea_snack, na.rm=TRUE)
dishes_table_lpgoneyear[5,1] <- mean(RawDataHH_lpg_oneyear$m4_q103_3_5_lpg_milk, na.rm=TRUE)
dishes_table_lpgoneyear[6,1] <- mean(RawDataHH_lpg_oneyear$m4_q103_3_6_lpg_water, na.rm=TRUE)

dishes_table_lpgoneyear[1,2] <- sum(RawDataHH_lpg_oneyear$m4_q103_3_1_lpg_chapattis, na.rm=TRUE)
dishes_table_lpgoneyear[2,2] <- sum(RawDataHH_lpg_oneyear$m4_q103_3_2_lpg_veg, na.rm=TRUE)
dishes_table_lpgoneyear[3,2] <- sum(RawDataHH_lpg_oneyear$m4_q103_3_3_lpg_rice, na.rm=TRUE)
dishes_table_lpgoneyear[4,2] <- sum(RawDataHH_lpg_oneyear$m4_q103_3_4_lpg_tea_snack, na.rm=TRUE)
dishes_table_lpgoneyear[5,2] <- sum(RawDataHH_lpg_oneyear$m4_q103_3_5_lpg_milk, na.rm=TRUE)
dishes_table_lpgoneyear[6,2] <- sum(RawDataHH_lpg_oneyear$m4_q103_3_6_lpg_water, na.rm=TRUE)

dishes_lpg_oneyear <- ggplot(dishes_table_lpgoneyear, aes(x=dishes, y=dishes_table_lpgoneyear$Percent)) + 
  geom_bar(stat="identity") + scale_x_discrete(limits=dishes) + ylim(c(0,1)) +
  geom_text(aes(label=dishes_table_lpgoneyear$n), color="white", vjust=1.6) +
  ylab("Percent of households") + xlab("Dishes cooked") + ggtitle("Fraction of households cooking dishes, LPG stove owned < 1 year (n=517)") + 
  theme_gray()

dishes_lpg_oneyear # dishes cooked with LPG when household has owned it <= 1 yr


#### LPG MORE THAN ONE YEAR

dishes <- c("Chapattis", "Vegetables", "Rice", "Tea/Snack", "Milk", "Water boiling")
dishes_table_lpgnotoneyear <- data.frame(matrix(NA,length(dishes),2))
colnames(dishes_table_lpgnotoneyear) <- c("Percent", "n")
rownames(dishes_table_lpgnotoneyear) <- dishes

dishes_table_lpgnotoneyear[1,1] <- mean(RawDataHH_lpg_notoneyear$m4_q103_3_1_lpg_chapattis, na.rm=TRUE)
dishes_table_lpgnotoneyear[2,1] <- mean(RawDataHH_lpg_notoneyear$m4_q103_3_2_lpg_veg, na.rm=TRUE)
dishes_table_lpgnotoneyear[3,1] <- mean(RawDataHH_lpg_notoneyear$m4_q103_3_3_lpg_rice, na.rm=TRUE)
dishes_table_lpgnotoneyear[4,1] <- mean(RawDataHH_lpg_notoneyear$m4_q103_3_4_lpg_tea_snack, na.rm=TRUE)
dishes_table_lpgnotoneyear[5,1] <- mean(RawDataHH_lpg_notoneyear$m4_q103_3_5_lpg_milk, na.rm=TRUE)
dishes_table_lpgnotoneyear[6,1] <- mean(RawDataHH_lpg_notoneyear$m4_q103_3_6_lpg_water, na.rm=TRUE)

dishes_table_lpgnotoneyear[1,2] <- sum(RawDataHH_lpg_notoneyear$m4_q103_3_1_lpg_chapattis, na.rm=TRUE)
dishes_table_lpgnotoneyear[2,2] <- sum(RawDataHH_lpg_notoneyear$m4_q103_3_2_lpg_veg, na.rm=TRUE)
dishes_table_lpgnotoneyear[3,2] <- sum(RawDataHH_lpg_notoneyear$m4_q103_3_3_lpg_rice, na.rm=TRUE)
dishes_table_lpgnotoneyear[4,2] <- sum(RawDataHH_lpg_notoneyear$m4_q103_3_4_lpg_tea_snack, na.rm=TRUE)
dishes_table_lpgnotoneyear[5,2] <- sum(RawDataHH_lpg_notoneyear$m4_q103_3_5_lpg_milk, na.rm=TRUE)
dishes_table_lpgnotoneyear[6,2] <- sum(RawDataHH_lpg_notoneyear$m4_q103_3_6_lpg_water, na.rm=TRUE)


dishes_lpg_notoneyear <- ggplot(dishes_table_lpgnotoneyear, aes(x=dishes, y=dishes_table_lpgnotoneyear$Percent)) + 
  geom_bar(stat="identity") + scale_x_discrete(limits=dishes) + ylim(c(0,1)) +
  geom_text(aes(label=dishes_table_lpgnotoneyear$n), color="white", vjust=1.6) +
  ylab("Percent of households") + xlab("Dishes cooked") + ggtitle("Fraction of households cooking dishes, LPG stove owned > 1 year (n=1343)") + 
  theme_gray()

dishes_lpg_notoneyear # dishes cooked with LPG when household has owned it > 1 yr

grid.arrange(dishes_lpg_notoneyear, dishes_lpg_oneyear, ncol=2) # print combined plot


####################
##### FIGURE A6
####################

lpg_distance_main <-  ggplot(RawDataHH_lpg_main, aes(RawDataHH_lpg_main$m4_q103_15_lpg_distance)) + 
  geom_histogram(bins=40)  + ggtitle("One-way distance to acquire LPG for LPG-Primary Fuel Households") + 
  ylab("Number of households") + xlab("Distance [km]") + theme_gray() + xlim(c(0,45)) +
  theme(plot.title=element_text(size=20),
        axis.text.y = element_text(size=18),
        axis.text.x = element_text(size=18),
        axis.title = element_text(size=22))

lpg_distance_sec <-  ggplot(RawDataHH_lpg_notmain, aes(RawDataHH_lpg_notmain$m4_q103_15_lpg_distance)) + 
  geom_histogram(bins=40)  + ggtitle("One-way distance to acquire LPG for LPG-Secondary Fuel Households") + 
  ylab("Number of households") + xlab("Distance [km]") + theme_gray() + xlim(c(0,45)) +
  theme(plot.title=element_text(size=20),
        axis.text.y = element_text(size=18),
        axis.text.x = element_text(size=18),
        axis.title = element_text(size=22))


grid.arrange(lpg_distance_main, lpg_distance_sec) # print combined plots
