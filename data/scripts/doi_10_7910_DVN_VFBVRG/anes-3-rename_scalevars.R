################################################################################
# Created By: Pietryka
# Creation Date:  2017-06-19
# Purpose: GIVE 2016 SCALE VARIABLES SAME NAMES AS IN 2012 ANES
# R Version: R version 3.3.3 (2017-03-06)
# Data Input: Data/Derived/2016/anes-2-define_groupvars.RData
# Data Output: Data/Derived/2016/anes-3-rename_scalevars.RData
#
# Questions: mpietryka@fsu.edu
################################################################################

#=============================================
#'#  1. PREAMBLE
#=============================================


# LOAD PACKAGES ----------------#
library(tidyverse) # MANY CONVENIENCE FUNCTIONS
library(sjmisc) # RECODE AND VARIABLE LABEL FUNCTIONS



# ------- LOAD DATA  ----------------#

# LOAD DATA CREATED IN 'DataClean/anes-2-define_groupvars.R'
# CONTAINS OBJECT: 'a16_r'
load("Data/Derived/2016/anes-2-define_groupvars.RData")


#=============================================
#'#  2. RENAME ITEMS
#=============================================


a16_rename <- a16_r  %>%

#' MOBILIZATION ----------------####

rename(
  # TALK WITH ANYONE? (MOBILPO_RMOB)
  mobilpo_rmob = V162010,
  # political meetings? (MOBILPO_RRALLY)
  mobilpo_rally = V162011,
  # WEAR A BUTTON OR PUT UP SIGN (MOBILPO_RBUTTN)
  mobilpo_sign= V162012,
  # WORK FOR PARTY (MOBILPO_RCAMPWK)
  mobilpo_otherwork= V162013,
  # GIVE $ TO PARTY INDIVIDUAL CANDIDATE (MOBILPO_CTBCAND)
  mobilpo_ctbcand= V162014,
  # GIVE $ TO PARTY (MOBILPO_CTBPTY)
  mobilpo_ctbpty= V162016,
  # GIVE $ TO OTHER GROUP (MOBILPO_CTBOTH)
  mobilpo_ctboth= V162017
)  %>%


#'##  LIMITED GOVERNMENT ----------------####
#'# ANSOLABEHERE, RODDEN, & SNYDER (APSR 2008) USE THREE BINARY ITEMS
# SO IGNORING FIVE-POINT 'govrole_regbus'
rename(
  govrole_big = V162183,
  govrole_market = V162184,
  govrole_lessmore = V162185
)  %>%



#'##  MORAL TRADITIONALISM ----------------####
rename(
  trad_adjust = V162207,
  trad_lifestyle = V162208,
  trad_tolerant = V162209,
  trad_famval = V162210
)  %>%


#'##  RACIAL RESENTMENT  ----------------####

rename(
  resent_workway = V162211,
  resent_slavery = V162212,
  resent_deserve = V162213,
  resent_try = V162214
) %>%


#'##  AUTHORITARIANISM  ----------------####

rename(
  auth_ind = V162239,
  auth_cur = V162240,
  auth_obed = V162241,
  auth_consid = V162242
) %>%



#'##  TIPI  ----------------####
rename(
  agree_critical = V162334,
  agree_sympathetic = V162339,
  emot_anxious = V162336,
  emot_calm = V162341,
  open_open = V162337,
  open_conventional = V162342,
  extra_extraverted = V162333,
  extra_reserved = V162338,
  conscien_dependable = V162335,
  conscien_disorganized = V162340
)  %>%




#'##  WORDSUM  ----------------####
rename(
  wordsum_setb = V161497,
  wordsum_setd = V161498,
  wordsum_sete = V161499,
  wordsum_setf = V161500,
  wordsum_setg = V161501,
  wordsum_seth = V161502,
  wordsum_setj = V161503,
  wordsum_setk = V161504,
  wordsum_setl = V161505,
  wordsum_seto = V161506
) %>%




#'##  EQUALITARIANSIM/ EGALITARIANISM  ----------------####

# ONLY USING FOUR FROM 2016, NOT EXTRA TWO FROM 2012
rename(
  egal_equal = V162243,
  egal_worryless = V162244,
  egal_notbigprob = V162245,
  egal_fewerprobs = V162246
)  %>%

#'##  NEGATIVE BLACK STEREOTYPES ----------------####

# ON 2016, USES HARDWORKING AND VIOLENT,
# BUT IN 2012, USES HARDWORKING AND INTELLIGENT

rename(
  stype_hwkblack = V162346,
  stype_intblack = V162350
)




#=============================================
#'#  3. SAVE
#=============================================

save(a16_rename, file = "Data/Derived/2016/anes-3-rename_scalevars.RData")



#=============================================
#'#  4. Display Session Info
#=============================================
sessionInfo()
