#*************************************************************************#
# Replication File for "Lethal Force in Black and White:                  #
# Assessing Racial Disparities in the Circumstances of Police Killings"   # 
#                    by Shea Streeter                                     #
#*************************************************************************#

#######################################
# Purpose of File:
# Clean column names that have been 
# expanded in "Cleaning Variables for JOP Anlysis .R"
#######################################


#######################################
# Inputs: data_to_clean.csv
#         variable_codebook_jop.csv
######################################

#######################################
# Output: cleaned_data.csv
#######################################

library(dplyr)
library(stringr)

to_clean <- read.csv("data_to_clean.csv", header = F)
vars<- read.csv("variable_names.csv") #This contains raw names and analysis names


########################################################## 
#Some of the variable names were misspelled during the manual arbitration process or 
# misprocessed during cleaning
##########################################################
# Create vector of variables to be deleted

to_delete<- c("behavior_Erratic Behavior. Other disruptive behavior. Specify:",
                "crime_specify:",
                "warrant_crime_specify:",
                "non_compliance_",
                "police_tactic_rubber bullet)",
                "deced_personal_Bisexual or Transgender",
                "deced_personal_Lesbian",
                "location_jail)",
                "shots_fired_Death not by gunshot")


#Create dataframe of variables to be merged with correct variable cateogires

wrong<- c("behavior_Erratic behavior",
"behavior_Suicidal behavior or threats. (e.g. holding gun to own head) (",
"non_compliance_‹ñ\tThreaten officer with weapon",
"non_compliance_ell at or insult the officer",
"non_compliance_Other aggressive or non-compliant behavior. specify:",
"non_compliance_Pull out gun",
"non_compliance_Shoot&nbsp;first&nbsp;at officer",
"police_tactic_Other aggressive or non-compliant behavior. Specify:",
"police_tactic_Taser",
"deced_personal_Had Children",
"deced_occupy_‹ñ\tEducation history. Specify school name(s) and/or fields of study:",
"deced_occupy_None of the above",
"crime_Burglary/Breaking & Entering",
"crime_Motor vehicle theft",
"crime_Traffic law violations",
"non_compliance_Disobey or ignore police order",
"non_compliance_Display or brandish weapon",
"reason_contact_Traffic stop",
"non_compliance_Brandish or display weapon")

right<- c("behavior_erratic",
         "behavior_suicide",
         "non_compliance_threaten",
         "non_compliance_yell",
         "non_compliance_other",
         "non_compliance_display",
         "non_compliance_shoot_first",
         "tactic_other",
         "tactic_taser",
         "personal_had_children",
         "educ_history",
         "occupy_none",
         "crime_burglar",
         "crime_theft_auto",
         "crime_traffic",
         "non_compliance_disobey",
         "non_compliance_display",
         "reason_traffic_stop",
         "non_compliance_display")

to_merge<- data.frame(wrong, right)
  
#####################################################################
#Convert other variable names to strings to easily test whether they match

column_names<- as.vector(to_clean[1,])
column_names <- sapply(column_names, toString)

vars<- vars %>% mutate_all(as.character)
to_merge<- to_merge %>% mutate_all(as.character)

to_clean[1,]<- sapply(to_clean[1,], toString)

#Problems with education and threatened misspelled cases

# #######ALWAYS CHECK WHICH LOCATION THESE ARE IN##################################
 to_merge[3,1]<- str_c(to_clean$V91[1]) #fix weird threatened case
 to_merge[11,1]<- str_c(to_clean$V170[1]) #fix weird education history

#Delete extra columns
#######   ONLY RUN THIS ONCE ##############
for(i in length(to_delete):1){
  key_wrong <-  to_delete[i]
  wrong_loc<- match(key_wrong, column_names)
  if(!is.na(wrong_loc)){
    to_clean <- to_clean[,-wrong_loc]
  }
}

column_names<- as.vector(to_clean[1,])
column_names <- sapply(column_names, toString)

###################################################################
#Now change the long raw names of columns to shorter analysis names


for(i in 1:nrow(vars)){
  var_wrong <- vars$raw_name[i]
  var_right <- vars$analysis_name[i]
  
  #Get vector location with column_names
  var_loc<- match(var_wrong, column_names)
  
  # Based on this location, pick column of data and if NA or empty replace with value in wrong column
  if(!is.na(var_loc)){
      column_names[var_loc]<- var_right

  }
}

to_clean<- setNames(to_clean,column_names)


#merge extra columns and delete incorrect extra columns
#This way the values are carried over into the correct column

###############################################################


for(i in 1:nrow(to_merge)){
  key_right <- to_merge$right[i]
  key_wrong <- to_merge$wrong[i]
  
  #Get vector location with column_names
  right_loc<- match(key_right, column_names)
  wrong_loc<- match(key_wrong, column_names)
  
  # Based on this location, pick column of data and if NA or empty replace with value in wrong column
  
  to_clean[,right_loc]<- ifelse(to_clean[, right_loc] == "0" | to_clean[, right_loc] == "", 
                                paste(to_clean[, wrong_loc]), 
                                paste(to_clean[, right_loc]))
  
}


#Delete old columns that have now been merged
#Increment backwards to not cause problems because of deleted 
# THE COLUMN NAMES IN to_merge MUST BE IN THE SAME ORDER AS IN COLUMN NAMES OTHERWISE
#THE FOR LOOP WILL DELETE THE WRONG VARIABLES
#Get location of each of to_merge$wrong variables based on column_names and reorder dataframe

to_merge$loc_wrong<- match(to_merge$wrong, column_names)
to_merge$loc_right<- match(to_merge$right, column_names)
to_merge<- to_merge[order(to_merge$loc_wrong),]


column_names<- names(to_clean)


for(i in nrow(to_merge):1){
  key_wrong <- to_merge$wrong[i]
  wrong_loc<- match(key_wrong, column_names)
  if(!is.na(wrong_loc)){
    to_clean <- to_clean[,-wrong_loc]
  }
}

to_clean<- to_clean[-1,]
write.csv(to_clean, "cleaned_data.csv", row.names = F)
