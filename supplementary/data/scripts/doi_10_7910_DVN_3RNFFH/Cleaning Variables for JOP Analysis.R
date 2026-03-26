#*************************************************************************#
# Replication File for "Lethal Force in Black and White:                  #
# Assessing Racial Disparities in the Circumstances of Police Killings"   # 
#                    by Shea Streeter                                     #
#*************************************************************************#


#######################################
# Purpose of File: 
# Data Cleaning Replication 
#######################################

########################################
#  Input: PKAP_raw_data.csv.csv
########################################

########################################
# Output: jop_analysis_data.csv
########################################
library(dplyr)
library(splitstackshape)
library(lubridate)
library(stringr)

data<- read.csv("PKAP_raw_data.csv")


#Remove reference variables not needed for analysis

data<- data %>% select(-c(name, id, guardian_descrip,	guardian_url1,	guardian_url2,	guardian_url3,	guardian_url4,
                       other_url_1, other_url_2, other_url_3, other_url_4, other_url_5, other_url_6))
######################
#Variables Expanded: 
######################
  # deced_personal
  # deced_descrip
  # deced_occupy
  # non_compliance
  # police_tactic
  # warrant_crime
  # crime
  # behavior
  # classification	levels
  # armed 
  # indoor
  # location
  # reason_contact
  # shots_fired
  # shots_stuck
  # officer_gender
  # officer_race
  # other_url
#######################
#Variables cleaned
######################
# sucide_by_cop
# violent_rec
# shot_execution
# gender
# video_public
# domestic
# witness
# near_friend
# child_witness
# officer_present
# officer_fire
# officer_name
# officer_years
# officer_undercover
# officer_offduty
# swat
# officer_personal
# mental
# intox
# criminal_rec
# video
########################
#Variables created
########################
  #day_week
  #time



#Clean and create relevant variables


# Variables to clean ---------------------------------------------------------------------

# gender ------------------------------------------------------------------

data$gender<- ifelse(data$gender == "Male", 1,0)

###############################################################
#Recode all yes/no as 0/1
# sucide_by_cop
data$suicide_by_cop<- ifelse(data$suicide_by_cop == "Yes", 1,0)
# violent_rec
data$violent_rec<- ifelse(data$violent_rec == "Yes", 1,0)
# shot_execution
data$shot_execution<- ifelse(data$shot_execution == "Yes", 1,0)
# video_public
data$video_public<- ifelse(data$video_public == "Yes", 1,0)
#Domestic
data$domestic<- ifelse(data$domestic == "Yes", 1,0)
#near_friend
data$near_friend<- ifelse(data$near_friend == "Yes", 1,0)
#child_witness
data$child_witness<- ifelse(data$child_witness == "Yes", 1,0)


#officer_name
data$officer_name<- ifelse(data$officer_name == "Yes", 1,0)
#officer_years
data$officer_years<- ifelse(data$officer_years == "Yes", 1,0)
# officer_undercover
data$officer_undercover<- ifelse(data$officer_undercover == "Yes", 1,0)
# officer_offduty
data$officer_offduty<- ifelse(data$officer_offduty == "Yes", 1,0)
# swat
data$swat<- ifelse(data$swat == "Yes", 1,0)

# officer_personal
data$officer_personal<- ifelse(data$officer_personal == "Yes", 1,0)
# mental
data$mental<- ifelse(data$mental == "Yes", 1,0)
# intox
data$intox<- ifelse(data$intox == "Yes", 1,0)
# criminal_rec
data$criminal_rec<- ifelse(data$criminal_rec == "Yes", 1,0)

# video
data$video<- ifelse(data$video == "Yes", 1,0)

#officer_present
data$officer_present<- ifelse(data$officer_present == "Unknown", 0,1)
#officer_fire
data$officer_fire<- ifelse(data$officer_fire == "Unknown", 0,1)

#####################################################################


# day_week and hour ----------------------------------------------------------------


#month	#day	#year	<-- change to day of the week

data<- data %>% mutate(full_date_text_hr = str_c(year, month, day, time, sep="-"),
                       full_date_text = str_c(year, month, day, sep="-"),
                       full_date_time = ymd_hm(full_date_text_hr), 
                       full_date = ymd(full_date_text), 
                       day_week = wday(full_date, label = TRUE),
                       hour = hour(full_date_time))
#####
#########################
#Transform hour into categorical variable based on time of day
data$hour_transform<- data$hour

data$hour_transform<- ifelse(data$hour_transform %in% 0:5, "night", #midnight to 5 am
                             ifelse(data$hour_transform %in% 6:11, "morning", #6am through 11am
                                    ifelse(data$hour_transform %in% 12:17, "afternoon", #12pm through 5pm
                                           ifelse(data$hour_transform %in% 18:23, "evening", #6pm through 11pm
                                                  tolower(paste(data$time))))))

data$hour_transform<- ifelse(data$hour_transform == "early morning", "morning",
                             data$hour_transform)

data<- data %>% select(-c(time, hour, full_date_text_hr, full_date_text, full_date_time, 
                          full_date, year, month, day))
#Split day of week and time variables

data$day_week <- tolower(data$day_week)
data<- cSplit_e(data, split.col = "day_week", sep = ",", mode = "binary", drop = T,
                type= "character", fill = 0)

data<- data %>% rename(time = hour_transform)
data<- cSplit_e(data, split.col = "time", sep = ",", mode = "binary", drop = T,
                type= "character", fill = 0)
##############################################################################
##############################################################################
# Edit Factor Variables ---------------------------------------------------

#behavior
data<- cSplit_e(data, split.col = "behavior", sep = ",", mode = "binary", drop = T,
                      type= "character", fill = 0)

#crime
data<- cSplit_e(data, split.col = "crime", sep = ",", mode = "binary", drop = T,
                type= "character", fill = 0)
#warrant_crime
data<- cSplit_e(data, split.col = "warrant_crime", sep = ",", mode = "binary", drop = T,
                type= "character", fill = 0)
#non_compliance
data<- cSplit_e(data, split.col = "non_compliance", sep = ",", mode = "binary", drop = T,
                type= "character", fill = 0)
#police_tactic
data<- cSplit_e(data, split.col = "police_tactic", sep = ",", mode = "binary", drop = T,
                type= "character", fill = 0)
#officer_gender
data<- cSplit_e(data, split.col = "officer_gender", sep = ",", mode = "binary", drop = T,
                type= "character", fill = 0)
#officer_race
data<- cSplit_e(data, split.col = "officer_race", sep = ",", mode = "binary", drop = T,
                type= "character", fill = 0)
#deced_personal
data<- cSplit_e(data, split.col = "deced_personal", sep = ",", mode = "binary", drop = T,
                type= "character", fill = 0)
#deced_descrip

data<- cSplit_e(data, split.col = "deced_descrip", sep = ",", mode = "binary", drop = T,
                type= "character", fill = 0)
#deced_occupy
data<- cSplit_e(data, split.col = "deced_occupy", sep = ",", mode = "binary", drop = T,
                type= "character", fill = 0)

#classification	
data<- cSplit_e(data, split.col = "classification", sep = ",", mode = "binary", drop = T,
                type= "character", fill = 0)
#armed
data<- cSplit_e(data, split.col = "armed", sep = ",", mode = "binary", drop = T,
                type= "character", fill = 0)

#location
data<- cSplit_e(data, split.col = "location", sep = ",", mode = "binary", drop = T,
                type= "character", fill = 0)
# reason_contact
data<- cSplit_e(data, split.col = "reason_contact", sep = ",", mode = "binary", drop = T,
                type= "character", fill = 0)
#indoor
data<- cSplit_e(data, split.col = "indoor", sep = ",", mode = "binary", drop = T,
                type= "character", fill = 0)
# witness
data<- cSplit_e(data, split.col = "witness", sep = ",", mode = "binary", drop = T,
                type= "character", fill = 0)
# shots_fired 
data<- cSplit_e(data, split.col = "shots_fired", sep = ",", mode = "binary", drop = T,
                type= "character", fill = 0)
# shots_struck 
data<- cSplit_e(data, split.col = "shots_struck", sep = ",", mode = "binary", drop = T,
                type= "character", fill = 0)

##############################################################
##############################################################

#Many of the expanded categories now have names that are too long and complicated
# for analysis. The following script which cleans misspelled and duplicated columns
# Because doing so requires removing the header of the dataframe and converting everything to 
# characters, I find that it is best to write a csv of the current data and re-load the cleaned
# csv with the transformed data

write.csv(data, "data_to_clean.csv", row.names = F)

source("Clean Expanded Categories.R")

data<- read.csv("cleaned_data.csv")

##############################################################
##############################################################


# Combine data to make aggregated categories ------------------------------

#In this section I combine some alike variables into larger categories and remove sparse and
# uniformative variables



#Combine Some variables into larger, aggregate categories

data<- data  %>% 
  mutate(armed_other2 = pmax(armed_other, armed_disputed, armed_unknown, armed_vehicle),
         behavior_mental_crisis = pmax(behavior_erratic, behavior_mental), 
         behavior_other2 = pmax(behavior_property, behavior_other, behavior_verbal, behavior_yell), 
         crime_other2 = pmax(crime_threat, crime_other, crime_parole, crime_prostitution), 
         crime_property2 = pmax(crime_arson, crime_burglar, crime_property, crime_theft_auto, 
                                crime_theft, crime_robbery, crime_trespass), 
         crime_public_order = pmax(crime_disorder, crime_drug, crime_dui, crime_traffic, crime_weapon), 
         crime_violent = pmax(crime_assault, crime_homicide, crime_homicide_attempt, 
                              crime_kidnap, crime_sex), 
         immigrant = pmax(document_imm, undoc_imm), 
         location_other2 = pmax(location_other, location_park, location_school, location_yard), 
         non_compliance_kill_injure = pmax(non_compliance_kill, non_compliance_injure,
                                           non_compliance_injure_dog), 
         non_compliance_physical = pmax(non_compliance_fight, non_compliance_hit, non_compliance_resist), 
         non_compliance_reach_weapon = pmax(non_compliance_reach_officer, non_compliance_reach_waist, 
                                            non_compliance_reach_weapon),
         non_compliance_shoot2 = pmax(non_compliance_shoot, non_compliance_shoot_first),
         non_compliance_vehicle = pmax(non_compliance_drive, non_compliance_driveat, non_compliance_ram), 
         non_compliance_verbal = pmax(non_compliance_argue, non_compliance_yell),
         occupy_other2 = pmax(occupy_other, retired, unemployed),
         officer_no_uniform = pmax(officer_undercover, officer_offduty), 
         officer_other_race = pmax(officer_asian, officer_latino),
         reason_other2 = pmax(reason_other, reason_friend, reason_stranger),
         suicidal = pmax(suicide_by_cop, behavior_suicide), 
         substance_problem = pmax(alcoholic, drug_addict),
         tactic_detain_handcuff = pmax(tactic_detain, tactic_handcuff),
         tactic_physical = pmax(tactic_wall, tactic_ground, tactic_hit, tactic_hands, tactic_baton), 
         veteran = pmax(occupy_military, occupy_fmr_military),
         w_crime_other2= pmax(w_crime_threat, w_crime_unknown, w_crime_other, w_crime_parole),
         w_crime_property  = pmax(w_crime_burglar, w_crime_robbery, w_crime_theft, w_crime_trespass),
         w_crime_public_order = pmax(w_crime_drug, w_crime_traffic, w_crime_weapon), 
         w_crime_violent = pmax(w_crime_assault, w_crime_homicide, w_crime_homicide_attempt, w_crime_kidnap, w_crime_sex)
  )

#Delete old variables and less informative variables
data<- data %>% select(- c(  armed_other, armed_disputed, armed_unknown, armed_vehicle,
                         behavior_erratic, behavior_mental,behavior_property, 
                         behavior_other, behavior_verbal, behavior_yell,crime_threat,
                         crime_other, crime_parole,crime_arson, crime_burglar, crime_property, 
                         crime_theft_auto, crime_theft, crime_robbery, crime_trespass,
                         crime_disorder, crime_drug, crime_dui, crime_traffic, crime_weapon, 
                         crime_assault, crime_homicide, crime_homicide_attempt,crime_kidnap, crime_prostitution,
                         crime_sex,document_imm, undoc_imm,location_other, location_park, 
                         location_school, location_yard,non_compliance_kill, non_compliance_injure,
                         non_compliance_injure_dog,non_compliance_fight, non_compliance_hit, 
                         non_compliance_resist,non_compliance_reach_officer, 
                         non_compliance_reach_waist,non_compliance_reach_weapon,non_compliance_shoot, 
                         non_compliance_shoot_first,non_compliance_drive, non_compliance_driveat,
                         non_compliance_ram,non_compliance_argue, non_compliance_yell,
                         occupy_other, retired, unemployed,officer_undercover, officer_offduty, 
                         officer_asian, officer_latino,reason_other, reason_friend, reason_stranger,
                         suicide_by_cop, behavior_suicide,alcoholic, drug_addict,tactic_detain, 
                         tactic_handcuff,tactic_wall, tactic_ground, tactic_hit, tactic_hands,
                         tactic_baton,occupy_military, occupy_fmr_military,w_crime_threat,
                         w_crime_unknown, w_crime_other, w_crime_parole,w_crime_burglar,
                         w_crime_robbery, w_crime_theft, w_crime_trespass,w_crime_drug,
                         w_crime_traffic, w_crime_weapon,w_crime_assault, w_crime_homicide,
                         w_crime_homicide_attempt, w_crime_kidnap, w_crime_sex , personal_queer, 
                         indoor_unknown, outdoors,witness_no, witness_unclear, shots_fired_multiple, 
                         shots_fired_unknown, shots_struck_multiple, shots_struck_unknown))

#########################################################

write.csv(data, "jop_analysis_data.csv", row.names = F)

