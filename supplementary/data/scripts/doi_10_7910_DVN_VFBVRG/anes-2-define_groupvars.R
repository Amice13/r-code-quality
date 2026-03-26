################################################################################
# Created By: Pietryka
# Creation Date:  2017-02-13
# Purpose: This file takes raw anes data from electionstudies.org and creates
#          cleaned grouping variables for DIF analysis
# R Version: R version 3.3.3 (2017-03-06)
# Data Input: Data/Source/anes_timeseries_2012_stata12.dta
#             Data/Source/anes_timeseries_2016.dta
# Data Output: Data/Derived/2012ANES/anes-2-define_groupvars.RData
#
# Questions: mpietryka@fsu.edu
################################################################################

#=============================================
#'#   1. PREAMBLE                         ####
#=============================================


# ----------------- LOAD PACKAGES ---------------- #

library(tidyverse) # MANY CONVENIENCE FUNCTIONS
library(haven)     # READ DTA FILES
library(sjmisc)    # RECODE AND VARIABLE LABEL FUNCTIONS


# ----------------- LOAD FUNCTIONS---------------- #

# LOAD FUNCTION TO RECODE GROUPING VARIABLES
source("Functions/FUN-gen_groupvars.R")

# LOAD OBJECTS THAT IDENTIFY GROUPING VARIABLES
source("DataClean/scale_items.R")

# ------- LOAD DATA  ---------------- #

# Load the raw 2012 ANES in Stata .dta format
a12 <- read_dta("Data/Source/anes_timeseries_2012_stata12.dta")

# Load the raw 2016 ANES in Stata .dta format
a16 <- read_dta("Data/Source/anes_timeseries_2016.dta")


#=============================================
#'#  2. CODE 2012 ANES
#============================================
# DESPITE WHAT THE 2012 CODEBOOK INDICATES, 'dem_raceeth_x' HAS THE FOLLOWING
# CODES: -9. Refused, -8. Don't know, 1. White non-Hispanic,
# 2. Black non-Hispanic, 3. Hispanic, 4. Other non-Hispanic

a12_r <- a12  %>%
  gen_groupvars(
    gender_var = "gender_respondent_x",
    gender_codes = "1 = 0; 2 = 1; else = NA",
    pid_var = "pid_x",
    pid_codes = "1:3 = -1; 4 = 0; 5:7 = 1; else = NA",
    libcon_var = "libcpre_self",
    libcon_codes = "1:3 = -1; 4 = 0; 5:7 = 1; else = NA",
    turnout_var = "rvote2012_x",
    turnout_codes = "1 = 1; 2 = 0; else = NA",
    race_var = "dem_raceeth_x",
    race_codes = "1 = 0; 2 = 1; 3 = 2; 4 = 3; else = NA",
    educ_var = "dem_edugroup_x" ,
    educ_codes = "1 = 0; 2 = 1; 3 = 2; 4 = 3; 5 = 4; else = NA",
    age_var = "dem_age_r_x",
    age_codes =  "1:39 = 0; 40:59 = 1; 60:1000 = 2; else = NA",
    income_var = "inc_incgroup_pre",
    income_codes = "1:8 = 0; 9:16 = 1; 17:100 = 2; else = NA",
    votechoice_var = "presvote2012_x",
    votechoice_codes = "1 = 0; 2 = 1; else = NA",
    mode_var = "mode",
    mode_codes = "1 = 0; 2 = 1; else = NA"
  )

#=============================================
#'#  3. CODE 2016 ANES
#============================================


a16_r <- a16  %>%
  gen_groupvars(
    gender_var = "V161342",
    gender_codes = "1 = 0; 2 = 1; else = NA",
    pid_var = "V161158x",
    pid_codes = "1:3 = -1; 4 = 0; 5:7 = 1; else = NA",
    libcon_var = "V161126",
    libcon_codes = "1:3 = -1; 4 = 0; 5:7 = 1; else = NA",
    turnout_var = "V162031x",
    turnout_codes = "1 = 1; 0 = 0; else = NA",
    race_var = "V161310x",
    race_codes = "1 = 0; 2 = 1; 5 = 2; 3:4 = 3; 6 = 3; else = NA",
    educ_var = "V161270" ,
    educ_codes = "1:8 = 0; 9 = 1; 10:12 = 2; 13 = 3; 14:16 = 4; else = NA",
    age_var = "V161267",
    age_codes =  "1:39 = 0; 40:59 = 1; 60:1000 = 2; else = NA",
    income_var = "V161361x",
    income_codes = "1:8 = 0; 9:16 = 1; 17:100 = 2; else = NA",
    votechoice_var = "V162062x",
    votechoice_codes = "1 = 0; 2 = 1; else = NA",
    mode_var = "V160501",
    mode_codes = "1 = 0; 2 = 1; else = NA"
  )




#=============================================
#'#  3. SUMMARIZE GROUPING VARIABLES
#=============================================

# 2012
a12_r %>% select(one_of(group_vars))  %>%  summary()

a12_r %>% select(one_of(group_vars))  %>%  map(table, useNA = "always")

min_12 <- a12_r %>% select(one_of(group_vars))  %>%  map_df(min, na.rm = TRUE)

max_12 <- a12_r %>% select(one_of(group_vars))  %>%  map_df(max, na.rm = TRUE)


# 2016
a16_r %>% select(one_of(group_vars))  %>%  summary()

a16_r %>% select(one_of(group_vars))  %>%  map(table, useNA = "always")

min_16 <- a16_r %>% select(one_of(group_vars))  %>%  map_df(min, na.rm = TRUE)

max_16 <- a16_r %>% select(one_of(group_vars))  %>%  map_df(max, na.rm = TRUE)


# CHECK FOR ERRORS
all(min_12 == min_16)  %>% assertthat::assert_that()
all(max_12 == max_16)  %>% assertthat::assert_that()

# DATAMAID
library(dataMaid)
a12_r %>% select(one_of(group_vars))  %>% check()
a16_r %>% select(one_of(group_vars))  %>% check()

#=============================================
#'#  4. SAVE
#=============================================
save(a12_r, file = "Data/Derived/2012/anes-2-define_groupvars.RData")
save(a16_r, file = "Data/Derived/2016/anes-2-define_groupvars.RData")


#=============================================
#'#  5. Display Session Info
#=============================================
sessionInfo()

