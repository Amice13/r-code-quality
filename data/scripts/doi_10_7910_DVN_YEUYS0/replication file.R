#=====================================================================================================================================#
#                                                                                                                                     #
# Title: "Informed or Overwhelmed? Disentangling the Effects of Cognitive Ability and Information on Public Opinion"                  #
# Author: Adam R. Panish                                                                                                              #
# Date: 1/20/2025                                                                                                                     #
#                                                                                                                                     #
#   Code for generating Figures 1-4 and Tables 2-6 & 8 in Main Text and Tables A1, A2, B1, & C1 and Figures E1 & E2 in the Appendix   #
#                                                                                                                                     #
#=====================================================================================================================================#


### Set Working Directory to Location of Replication Data

# setwd("~/Downloads")


### Load Required Packages

library(haven)
library(dplyr)
library(matrixStats)
library(mirt)
library(ggplot2)
library(ggrepel)
library(Cairo)
library(lme4)
library(lmerTest)
library(broom)
library(broom.mixed)
library(tidyr)
library(knitr)
library(ggh4x)
library(grid)
library(reshape2)
library(lavaan)
library(stringr)
library(ggpubr)
library(dagitty)
library(ggdag)


### Required Custom Functions

# Function for rescaling variables from 0-1
scale <- function(data) {
  (data - min(data, na.rm = T)) / max(data - min(data, na.rm = T), na.rm = T)
}

# Function for reversing and rescaling variables from 0-1
revscale <- function(data) {
  (max(data, na.rm = T) - data) / max(max(data, na.rm = T) - data, na.rm = T)
}

# Function for calculating average individual-level constraint
constraint <- function(data, prop) {
  # Allow respondents with more than [prop]% NA values to incur all NAs
  constraint <- ifelse(
    rowSums(is.na(data)) > ncol(data) / (100 / prop),
    sqrt(rowVars(as.matrix(data), na.rm = F)),
    sqrt(rowVars(as.matrix(data), na.rm = T))
  )
  
  # Rescale 0-1
  constraint <- revscale(constraint)
  
  return(constraint)
}

# Function for calculating average individual-level stability
avg_stblty <- function(data, prop) {
  # Allow respondents with more than [prop]% NA values to incur all NAs
  stability <- ifelse(
    rowSums(is.na(data)) > ncol(data) / (100 / prop),
    rowSums(data, na.rm = F),
    rowSums(data, na.rm = T)
  )
  
  # Average stability
  stability <- stability / rowSums(!is.na(data))
  
  # Rescale 0-1
  stability <- scale(stability)
  
  return(stability)
}





#========================================================================#
#                      Create 2008-2010 ANES Panel                       #
#========================================================================#

### Prepare ANES 2008-2009 Panel Study

# Load Data
ANES08_09_main <- read_dta("ANES 2008 Panel.dta")
ANES08_09_off_waves <- read_sav("ANES 2008 Panel_off waves.sav",
                                col_select = c("caseid", "W8QA_WORD1", "W8QB_WORD2", "W8QC_WORD3", "W8QD_WORD4", "W8QE_WORD5", "W8QF_WORD6", "W8QG_WORD7", 
                                               "W8QH_WORD8", "W8QI_WORD9", "W8QJ_WORD10", "W8QK_WORD11", "W8QL_WORD12", "W8QM_WORD13", "W8QO_WORD14"))

# Merge
ANES08_09 <- merge(ANES08_09_main, ANES08_09_off_waves, by = "caseid")

ANES08_09 <- 
  ANES08_09 %>% 
  mutate(
    
    # Respondent ID
    case_id = as.numeric(caseid),
    
    # Demographics
    age =       as.numeric(der02),
    male =      as.numeric(2 - der01),
    race =      relevel(factor(suppressWarnings(car::recode(as.numeric(der04), "1 = 'White'; 2 = 'Black'; 3 = 'Hispanic'; 4 = 'Other'"))), "White"),
    black =     ifelse(is.na(race), NA, ifelse(race == "Black", 1, 0)),
    hispanic =  ifelse(is.na(race), NA, ifelse(race == "Hispanic", 1, 0)),
    other =     ifelse(is.na(race), NA, ifelse(race == "Other", 1, 0)),
    education = relevel(factor(suppressWarnings(car::recode(as.numeric(der05), "1:3 = 'Less than bachelors degree'; 4 = 'Bachelors degree'; 5 = 'Advanced degree'")),
                               levels = c("Less than bachelors degree", "Bachelors degree", "Advanced degree")), "Less than bachelors degree"),
    bachelors = ifelse(is.na(education), NA, ifelse(education == "Bachelors degree", 1, 0)),
    post_grad = ifelse(is.na(education), NA, ifelse(education == "Advanced degree", 1, 0)),
    income =    ifelse(der06 %in% -6:-2, NA, (der06 - 1) / 18),
    
    # Verbal Ability (Wave 8)
    word1 = ifelse(W8QA_WORD1 %in% -5:-1, NA, ifelse(W8QA_WORD1 == 4, 1, 0)), 
    word2 = ifelse(W8QB_WORD2 %in% -5:-1, NA, ifelse(W8QB_WORD2 == 5, 1, 0)),
    word3 = ifelse(W8QC_WORD3 %in% -5:-1, NA, ifelse(W8QC_WORD3 == 5, 1, 0)), 
    word4 = ifelse(W8QD_WORD4 %in% -5:-1, NA, ifelse(W8QD_WORD4 == 3, 1, 0)), 
    word5 = ifelse(W8QE_WORD5 %in% -5:-1, NA, ifelse(W8QE_WORD5 == 1, 1, 0)), 
    word6 = ifelse(W8QF_WORD6 %in% -5:-1, NA, ifelse(W8QF_WORD6 == 3, 1, 0)), 
    word7 = ifelse(W8QG_WORD7 %in% -5:-1, NA, ifelse(W8QG_WORD7 == 5, 1, 0)), 
    word8 = ifelse(W8QH_WORD8 %in% -5:-1, NA, ifelse(W8QH_WORD8 == 4, 1, 0)), 
    word9 = ifelse(W8QI_WORD9 %in% -5:-1, NA, ifelse(W8QI_WORD9 == 4, 1, 0)), 
    word10 = ifelse(W8QJ_WORD10 %in% -5:-1, NA, ifelse(W8QJ_WORD10 == 1, 1, 0)), 
    word11 = ifelse(W8QK_WORD11 %in% -5:-1, NA, ifelse(W8QK_WORD11 == 1, 1, 0)), 
    word12 = ifelse(W8QL_WORD12 %in% -5:-1, NA, ifelse(W8QL_WORD12 == 4, 1, 0)), 
    word13 = ifelse(W8QM_WORD13 %in% -5:-1, NA, ifelse(W8QM_WORD13 == 3, 1, 0)), 
    word14 = ifelse(W8QO_WORD14 %in% -5:-1, NA, ifelse(W8QO_WORD14 == 2, 1, 0)), 
    wordsum_scale = (word1 + word2 + word3 + word4 + word5 + word6 + word7 + word8 + word9 + word10 + word11 + word12 + word13 + word14) / 14,
    
    # Need To Evaluate
    many_opinions = ifelse(w11ze1 %in% 1:4, (4 - w11ze1) / 3, NA),
    more_opinions = ifelse(w11ze2 == 2, 0.5,
                           ifelse(w11ze2 == 1, car::recode(as.numeric(w11ze3a), "-9:-1 = NA; 1 = 0; 2 = 0.25"),
                                  ifelse(w11ze2 == 3, car::recode(as.numeric(w11ze3b), "-9:-1 = NA; 1 = 1; 2 = 0.75"), NA))),
    nte_scale = (many_opinions + more_opinions) / 2,
    
    # Need For Cognition
    like_thinking = ifelse(w11ze4 == 3, 0.5,
                           ifelse(w11ze4 == 2, car::recode(as.numeric(w11ze5b), "-9:-1 = NA; 1 = 0; 2 = 0.25"),
                                  ifelse(w11ze4 == 1, car::recode(as.numeric(w11ze5a), "-9:-1 = NA; 1 = 1; 2 = 0.75"), NA))),
    prefer_complex = ifelse(w11ze6 %in% 1:2, w11ze6 - 1, NA),
    nfc_scale = (like_thinking + prefer_complex) / 2,
    
    # Political Knowledge (Wave 2 & Wave 9 [candidate biographies])
    pres_reelect.02 = ifelse(w2u2 %in% -6:-1, NA, ifelse(w2u2 == 2, 1, 0)),
    senate_term.02 =  ifelse(w2u3 %in% -6:-1, NA, ifelse(w2u3 == 6, 1, 0)),
    senate_state.02 = ifelse(w2u4 %in% -6:-1, NA, ifelse(w2u4 == 2, 1, 0)),
    house_term.02 =   ifelse(w2u5 %in% -6:-1, NA, ifelse(w2u5 == 2, 1, 0)),
    succession.02 =   ifelse(w2u6 %in% -6:-1, NA, ifelse(w2u6 == 3, 1, 0)),
    override.02 =     ifelse(w2u7 %in% -6:-1, NA, ifelse(w2u7 == 2, 1, 0)),
    # pres_reelect.11 = ifelse(w11wv7 %in% -6:-1, NA, ifelse(w11wv7 == 2, 1, 0)), # Wave 11 responses dropped from scale to avoid propagating missingness
    # senate_term.11 =  ifelse(w11wv8 %in% -6:-1, NA, ifelse(w11wv8 == 6, 1, 0)),
    # senate_state.11 = ifelse(w11wv9 %in% -6:-1, NA, ifelse(w11wv9 == 2, 1, 0)),
    # house_term.11 =   ifelse(w11wv10 %in% -6:-1, NA, ifelse(w11wv10 == 2, 1, 0)),
    # succession.11 =   ifelse(w11wv11 %in% -6:-1, NA, ifelse(w11wv11 == 3, 1, 0)),
    # override.11 =     ifelse(w11wv12 %in% -6:-1, NA, ifelse(w11wv12 == 2, 1, 0)),
    pol_know_scale = (pres_reelect.02 + senate_term.02 + senate_state.02 + house_term.02 + succession.02 + override.02) / 6,
    
    # Placement Knowledge
    
    # Obama Placement
    obama_ideology.06 = ifelse(w6h5 == 1, car::recode(as.numeric(w6h6), "-9:-7 = -1; -6:-1 = NA; 1 = 0; 2 = 0.25"),
                               ifelse(w6h5 == 2, car::recode(as.numeric(w6h7), "-9:-7 = -1; -6:-1 = NA; 1 = 1; 2 = 0.75"),
                                      car::recode(as.numeric(w6h8), "-9:-7 = -1; -6:-1 = NA; 1 = 0.25; 2 = 0.75; 3 = 0.5"))),
    obama_gay_mar.06 = ifelse(w6pb1 == 1, car::recode(as.numeric(w6pb2_favor), "-9:-7 = -1; -6:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"),
                              ifelse(w6pb1 == 2, car::recode(as.numeric(w6pb2_oppose), "-9:-7 = -1; -6:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                     car::recode(as.numeric(w6pb1), "-9:-7 = -1; -6:-1 = NA; 1 = 0.75; 2 = 0.25; 3 = 0.5"))),
    obama_taxes_rich.06 = ifelse(w6pb4 == 1, car::recode(as.numeric(w6pb5_favor), "-9:-7 = -1; -6:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                 ifelse(w6pb4 == 2, car::recode(as.numeric(w6pb5_oppose), "-9:-7 = -1; -6:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"),
                                        car::recode(as.numeric(w6pb4), "-9:-7 = -1; -6:-1 = NA; 1 = 0.25; 2 = 0.75; 3 = 0.5"))),
    obama_taxes_middle.06 = ifelse(w6pb7 == 1, car::recode(as.numeric(w6pb8_favor), "-9:-7 = -1; -6:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                   ifelse(w6pb7 == 2, car::recode(as.numeric(w6pb8_oppose), "-9:-7 = -1; -6:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"),
                                          car::recode(as.numeric(w6pb7), "-9:-7 = -1; -6:-1 = NA; 1 = 0.25; 2 = 0.75; 3 = 0.5"))),
    obama_pay_drugs.06 = ifelse(w6pb10 == 1, car::recode(as.numeric(w6pb11_favor), "-9:-7 = -1; -6:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                ifelse(w6pb10 == 2, car::recode(as.numeric(w6pb11_oppose), "-9:-7 = -1; -6:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"),
                                       car::recode(as.numeric(w6pb10), "-9:-7 = -1; -6:-1 = NA; 1 = 0.25; 2 = 0.75; 3 = 0.5"))),
    obama_healthcare_spend.06 = ifelse(w6pb13 == 1, car::recode(as.numeric(w6pb14_favor), "-9:-7 = -1; -6:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                       ifelse(w6pb13 == 2, car::recode(as.numeric(w6pb14_oppose), "-9:-7 = -1; -6:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"),
                                              car::recode(as.numeric(w6pb13), "-9:-7 = -1; -6:-1 = NA; 1 = 0.25; 2 = 0.75; 3 = 0.5"))),
    obama_detention.06 = ifelse(w6pb16 == 1, car::recode(as.numeric(w6pb17_favor), "-9:-7 = -1; -6:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"),
                                ifelse(w6pb16 == 2, car::recode(as.numeric(w6pb17_oppose), "-9:-7 = -1; -6:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                       car::recode(as.numeric(w6pb16), "-9:-7 = -1; -6:-1 = NA; 1 = 0.75; 2 = 0.25; 3 = 0.5"))),
    obama_surveillance.06 = ifelse(w6pb19 == 1, car::recode(as.numeric(w6pb20_favor), "-9:-7 = -1; -6:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                   ifelse(w6pb19 == 2, car::recode(as.numeric(w6pb20_oppose), "-9:-7 = -1; -6:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"),
                                          car::recode(as.numeric(w6pb19), "-9:-7 = -1; -6:-1 = NA; 1 = 0.25; 2 = 0.75; 3 = 0.5"))),
    obama_immig_work.06 = ifelse(w6pb22 == 1, car::recode(as.numeric(w6pb23_favor), "-9:-7 = -1; -6:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                 ifelse(w6pb22 == 2, car::recode(as.numeric(w6pb23_oppose), "-9:-7 = -1; -6:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"),
                                        car::recode(as.numeric(w6pb22), "-9:-7 = -1; -6:-1 = NA; 1 = 0.25; 2 = 0.75; 3 = 0.5"))),
    obama_immig_citizen.06 = ifelse(w6pb25 == 1, car::recode(as.numeric(w6pb26_favor), "-9:-7 = -1; -6:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                    ifelse(w6pb25 == 2, car::recode(as.numeric(w6pb26_oppose), "-9:-7 = -1; -6:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"),
                                           car::recode(as.numeric(w6pb25), "-9:-7 = -1; -6:-1 = NA; 1 = 0.25; 2 = 0.75; 3 = 0.5"))),
    obama_aff_action.09 = ifelse(w9qr3_a == 1, car::recode(as.numeric(w9qr3_b), "-9:-7 = -1; -6:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                 ifelse(w9qr3_a == 2, car::recode(as.numeric(w9qr3_c), "-9:-7 = -1; -6:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"),
                                        car::recode(as.numeric(w9qr3_a), "-9:-7 = -1; -6:-1 = NA; 1 = 0.25; 2 = 0.75; 3 = 0.5"))),
    obama_abort_health.10 = ifelse(w10ra1 == 1, car::recode(as.numeric(w10ra2_favor), "-9:-7 = -1; -6:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                   ifelse(w10ra1 == 2, car::recode(as.numeric(w10ra2_oppose), "-9:-7 = -1; -6:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"),
                                          car::recode(as.numeric(w10ra1), "-9:-7 = -1; -6:-1 = NA; 1 = 0.25; 2 = 0.75; 3 = 0.5"))),
    obama_abort_gender.10 = ifelse(w10ra3 == 1, car::recode(as.numeric(w10ra4_favor), "-9:-7 = -1; -6:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                   ifelse(w10ra3 == 2, car::recode(as.numeric(w10ra4_oppose), "-9:-7 = -1; -6:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"),
                                          car::recode(as.numeric(w10ra3), "-9:-7 = -1; -6:-1 = NA; 1 = 0.25; 2 = 0.75; 3 = 0.5"))),
    
    # McCain placement
    mccain_ideology.06 = ifelse(w6h9 == 1, car::recode(as.numeric(w6h10), "-9:-7 = -1; -6:-1 = NA; 1 = 0; 2 = 0.25"),
                                ifelse(w6h9 == 2, car::recode(as.numeric(w6h11), "-9:-7 = -1; -6:-1 = NA; 1 = 1; 2 = 0.75"),
                                       car::recode(as.numeric(w6h12), "-9:-7 = -1; -6:-1 = NA; 1 = 0.25; 2 = 0.75; 3 = 0.5"))),
    mccain_gay_mar.06 = ifelse(w6pj1 == 1, car::recode(as.numeric(w6pj2_favor), "-9:-7 = -1; -6:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"),
                               ifelse(w6pj1 == 2, car::recode(as.numeric(w6pj2_oppose), "-9:-7 = -1; -6:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                      car::recode(as.numeric(w6pj1), "-9:-7 = -1; -6:-1 = NA; 1 = 0.75; 2 = 0.25; 3 = 0.5"))),
    mccain_taxes_rich.06 = ifelse(w6pj4 == 1, car::recode(as.numeric(w6pj5_favor), "-9:-7 = -1; -6:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                  ifelse(w6pj4 == 2, car::recode(as.numeric(w6pj5_oppose), "-9:-7 = -1; -6:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"),
                                         car::recode(as.numeric(w6pj4), "-9:-7 = -1; -6:-1 = NA; 1 = 0.25; 2 = 0.75; 3 = 0.5"))),
    mccain_taxes_middle.06 = ifelse(w6pj7 == 1, car::recode(as.numeric(w6pj8_favor), "-9:-7 = -1; -6:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                    ifelse(w6pj7 == 2, car::recode(as.numeric(w6pj8_oppose), "-9:-7 = -1; -6:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"),
                                           car::recode(as.numeric(w6pj7), "-9:-7 = -1; -6:-1 = NA; 1 = 0.25; 2 = 0.75; 3 = 0.5"))),
    mccain_pay_drugs.06 = ifelse(w6pj10 == 1, car::recode(as.numeric(w6pj11_favor), "-9:-7 = -1; -6:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                 ifelse(w6pj10 == 2, car::recode(as.numeric(w6pj11_oppose), "-9:-7 = -1; -6:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"),
                                        car::recode(as.numeric(w6pj10), "-9:-7 = -1; -6:-1 = NA; 1 = 0.25; 2 = 0.75; 3 = 0.5"))),
    mccain_healthcare_spend.06 = ifelse(w6pj13 == 1, car::recode(as.numeric(w6pj14_favor), "-9:-7 = -1; -6:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                        ifelse(w6pj13 == 2, car::recode(as.numeric(w6pj14_oppose), "-9:-7 = -1; -6:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"),
                                               car::recode(as.numeric(w6pj13), "-9:-7 = -1; -6:-1 = NA; 1 = 0.25; 2 = 0.75; 3 = 0.5"))),
    mccain_detention.06 = ifelse(w6pj16 == 1, car::recode(as.numeric(w6pj17_favor), "-9:-7 = -1; -6:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"),
                                 ifelse(w6pj16 == 2, car::recode(as.numeric(w6pj17_oppose), "-9:-7 = -1; -6:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                        car::recode(as.numeric(w6pj16), "-9:-7 = -1; -6:-1 = NA; 1 = 0.75; 2 = 0.25; 3 = 0.5"))),
    mccain_surveillance.06 = ifelse(w6pj19 == 1, car::recode(as.numeric(w6pj20_favor), "-9:-7 = -1; -6:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                    ifelse(w6pj19 == 2, car::recode(as.numeric(w6pj20_oppose), "-9:-7 = -1; -6:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"),
                                           car::recode(as.numeric(w6pj19), "-9:-7 = -1; -6:-1 = NA; 1 = 0.25; 2 = 0.75; 3 = 0.5"))),
    mccain_immig_work.06 = ifelse(w6pj22 == 1, car::recode(as.numeric(w6pj23_favor), "-9:-7 = -1; -6:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                  ifelse(w6pj22 == 2, car::recode(as.numeric(w6pj23_oppose), "-9:-7 = -1; -6:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"),
                                         car::recode(as.numeric(w6pj22), "-9:-7 = -1; -6:-1 = NA; 1 = 0.25; 2 = 0.75; 3 = 0.5"))),
    mccain_immig_citizen.06 = ifelse(w6pj25 == 1, car::recode(as.numeric(w6pj26_favor), "-9:-7 = -1; -6:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                     ifelse(w6pj25 == 2, car::recode(as.numeric(w6pj26_oppose), "-9:-7 = -1; -6:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"),
                                            car::recode(as.numeric(w6pj25), "-9:-7 = -1; -6:-1 = NA; 1 = 0.25; 2 = 0.75; 3 = 0.5"))),
    mccain_aff_action.09 = ifelse(w9qr5_a == 1, car::recode(as.numeric(w9qr5_b), "-9:-7 = -1; -6:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                  ifelse(w9qr5_a == 2, car::recode(as.numeric(w9qr5_c), "-9:-7 = -1; -6:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"),
                                         car::recode(as.numeric(w9qr5_a), "-9:-7 = -1; -6:-1 = NA; 1 = 0.25; 2 = 0.75; 3 = 0.5"))),
    mccain_abort_health.10 = ifelse(w10ra5 == 1, car::recode(as.numeric(w10ra6_favor), "-9:-7 = -1; -6:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                    ifelse(w10ra5 == 2, car::recode(as.numeric(w10ra6_oppose), "-9:-7 = -1; -6:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"),
                                           car::recode(as.numeric(w10ra5), "-9:-7 = -1; -6:-1 = NA; 1 = 0.25; 2 = 0.75; 3 = 0.5"))),
    mccain_abort_gender.10 = ifelse(w10ra7 == 1, car::recode(as.numeric(w10ra8_favor), "-9:-7 = -1; -6:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                    ifelse(w10ra7 == 2, car::recode(as.numeric(w10ra8_oppose), "-9:-7 = -1; -6:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"),
                                           car::recode(as.numeric(w10ra7), "-9:-7 = -1; -6:-1 = NA; 1 = 0.25; 2 = 0.75; 3 = 0.5"))),
    
    # Placement Variable
    place_ideology.06 = ifelse(is.na(obama_ideology.06) | is.na(mccain_ideology.06), NA, 
                               ifelse(obama_ideology.06 == -1 | mccain_ideology.06 == -1, 0, 
                                      ifelse(obama_ideology.06 < mccain_ideology.06, 1, 0))),
    place_gay_mar.06 = ifelse(is.na(obama_gay_mar.06) | is.na(mccain_gay_mar.06), NA, 
                              ifelse(obama_gay_mar.06 == -1 | mccain_gay_mar.06 == -1, 0, 
                                     ifelse(obama_gay_mar.06 < mccain_gay_mar.06, 1, 0))),
    place_taxes_rich.06 = ifelse(is.na(obama_taxes_rich.06) | is.na(mccain_taxes_rich.06), NA, 
                                 ifelse(obama_taxes_rich.06 == -1 | mccain_taxes_rich.06 == -1, 0, 
                                        ifelse(obama_taxes_rich.06 < mccain_taxes_rich.06, 1, 0))),
    place_taxes_middle.06 = ifelse(is.na(obama_taxes_middle.06) | is.na(mccain_taxes_middle.06), NA, 
                                   ifelse(obama_taxes_middle.06 == -1 | mccain_taxes_middle.06 == -1, 0, 
                                          ifelse(obama_taxes_middle.06 < mccain_taxes_middle.06, 1, 0))),
    place_pay_drugs.06 = ifelse(is.na(obama_pay_drugs.06) | is.na(mccain_pay_drugs.06), NA, 
                                ifelse(obama_pay_drugs.06 == -1 | mccain_pay_drugs.06 == -1, 0, 
                                       ifelse(obama_pay_drugs.06 < mccain_pay_drugs.06, 1, 0))),
    place_healthcare_spend.06 = ifelse(is.na(obama_healthcare_spend.06) | is.na(mccain_healthcare_spend.06), NA, 
                                       ifelse(obama_healthcare_spend.06 == -1 | mccain_healthcare_spend.06 == -1, 0, 
                                              ifelse(obama_healthcare_spend.06 < mccain_healthcare_spend.06, 1, 0))),
    place_detention.06 = ifelse(is.na(obama_detention.06) | is.na(mccain_detention.06), NA, 
                                ifelse(obama_detention.06 == -1 | mccain_detention.06 == -1, 0, 
                                       ifelse(obama_detention.06 < mccain_detention.06, 1, 0))),
    place_surveillance.06 = ifelse(is.na(obama_surveillance.06) | is.na(mccain_surveillance.06), NA, 
                                   ifelse(obama_surveillance.06 == -1 | mccain_surveillance.06 == -1, 0, 
                                          ifelse(obama_surveillance.06 < mccain_surveillance.06, 1, 0))),
    place_immig_work.06 = ifelse(is.na(obama_immig_work.06) | is.na(mccain_immig_work.06), NA, 
                                 ifelse(obama_immig_work.06 == -1 | mccain_immig_work.06 == -1, 0, 
                                        ifelse(obama_immig_work.06 < mccain_immig_work.06, 1, 0))),
    place_immig_citizen.06 = ifelse(is.na(obama_immig_citizen.06) | is.na(mccain_immig_citizen.06), NA, 
                                    ifelse(obama_immig_citizen.06 == -1 | mccain_immig_citizen.06 == -1, 0, 
                                           ifelse(obama_immig_citizen.06 < mccain_immig_citizen.06, 1, 0))),
    place_aff_action.09 = ifelse(is.na(obama_aff_action.09) | is.na(mccain_aff_action.09), NA,
                                 ifelse(obama_aff_action.09 == -1 | mccain_aff_action.09 == -1, 0,
                                        ifelse(obama_aff_action.09 < mccain_aff_action.09, 1, 0))),
    # place_abort_health.10 = ifelse(is.na(obama_abort_health.10) | is.na(mccain_abort_health.10), NA,  # Wave 10 responses dropped from scale to avoid propagating missingness
    #                             ifelse(obama_abort_health.10 == -1 | mccain_abort_health.10 == -1, 0,
    #                                    ifelse(obama_abort_health.10 < mccain_abort_health.10, 1, 0))),
    # place_abort_gender.10 = ifelse(is.na(obama_abort_gender.10) | is.na(mccain_abort_gender.10), NA,
    #                             ifelse(obama_abort_gender.10 == -1 | mccain_abort_gender.10 == -1, 0,
    #                                    ifelse(obama_abort_gender.10 < mccain_abort_gender.10, 1, 0))),
    
    place_know_scale = (place_ideology.06 + place_gay_mar.06 + place_taxes_middle.06 + place_pay_drugs.06 + place_healthcare_spend.06 + 
                          place_detention.06 + place_surveillance.06 + place_immig_work.06 + place_immig_citizen.06 + place_aff_action.09) / 10, 
    
    # Political Interest (Nov 2007 Recruitment Interview [Political Interest] & Wave 1)
    politics_interest.00 = ifelse(rqpol %in% 1:5, (rqpol - 1) / 4, NA),
    pol_info_interest.01 = ifelse(w1k1 %in% 1:5, (5 - w1k1) / 4, NA),
    # pol_info_interest.02 = ifelse(w2g1 %in% 1:5, (5 - w2g1) / 4, NA), # Wave 2 responses dropped from scale to avoid propagating missingness
    pol_info_interest.09 = ifelse(w9h1 %in% 1:5, (5 - w9h1) / 4, NA),
    pol_info_interest.10 = ifelse(w10h1 %in% 1:5, (5 - w10h1) / 4, NA),
    pol_info_interest.11 = ifelse(w11h1 %in% 1:5, (5 - w11h1) / 4, NA),
    # pol_info_interest.19 = ifelse(w19h1 %in% 1:5, (5 - w19h1) / 4, NA), # Wave 19 responses dropped from scale to avoid propagating missingness
    pol_interest_scale =   (politics_interest.00 + 
                              pol_info_interest.01 +
                              pol_info_interest.09 +
                              pol_info_interest.10 +
                              pol_info_interest.11) / 5,
    
    # Attention to Politics in Media
    attention_tv.19 =        ifelse(w19f1 == 0, 0, ifelse(w19f5 %in% 1:5, (5 - w19f5) / 4, NA)),
    attention_radio.19 =     ifelse(w19f2 == 0, 0, ifelse(w19f6 %in% 1:5, (5 - w19f6) / 4, NA)),
    attention_internet.19 =  ifelse(w19f3 == 0, 0, ifelse(w19f7 %in% 1:5, (5 - w19f7) / 4, NA)),
    attention_newspaper.19 = ifelse(w19f4 == 0, 0, ifelse(w19f8 %in% 1:5, (5 - w19f8) / 4, NA)),
    att_pol_media_scale =    (attention_tv.19 + attention_radio.19 + attention_internet.19 + attention_newspaper.19) / 4,
    
    # Weekly Media Consumption
    tv_week.01 =         ifelse(w1h1 %in% 0:7, w1h1 / 7, NA),
    radio_week.01 =      ifelse(w1h2 %in% 0:7, w1h2 / 7, NA),
    internet_week.01 =   ifelse(w1h3 %in% 0:7, w1h3 / 7, NA),
    newspaper_week.01 =  ifelse(w1h4 %in% 0:7, w1h4 / 7, NA),
    tv_week.09 =         ifelse(w9f1 %in% 0:7, w9f1 / 7, NA),
    radio_week.09 =      ifelse(w9f2 %in% 0:7, w9f2 / 7, NA),
    internet_week.09 =   ifelse(w9f3 %in% 0:7, w9f3 / 7, NA),
    newspaper_week.09 =  ifelse(w9f4 %in% 0:7, w9f4 / 7, NA),
    tv_week.10 =        ifelse(w10f1 %in% 0:7, w10f1 / 7, NA),
    radio_week.10 =     ifelse(w10f2 %in% 0:7, w10f2 / 7, NA),
    internet_week.10 =  ifelse(w10f3 %in% 0:7, w10f3 / 7, NA),
    newspaper_week.10 = ifelse(w10f4 %in% 0:7, w10f4 / 7, NA),
    # tv_week.19 =        ifelse(w19f1 %in% 0:7, w19f1 / 7, NA), # Wave 19 responses dropped from scale to avoid propagating missingness
    # radio_week.19 =     ifelse(w19f2 %in% 0:7, w19f2 / 7, NA),
    # internet_week.19 =  ifelse(w19f3 %in% 0:7, w19f3 / 7, NA),
    # newspaper_week.19 = ifelse(w19f4 %in% 0:7, w19f4 / 7, NA),
    media_week =        (tv_week.01 + radio_week.01 + internet_week.01 + newspaper_week.01 +
                           tv_week.09 + radio_week.09 + internet_week.09 + newspaper_week.09 +
                           tv_week.10 + radio_week.10 + internet_week.10 + newspaper_week.10) / 12,
    
    # Weekly Political Discussion
    pol_discuss_week.01 =  ifelse(w1k2 %in% 0:7, w1k2 / 7, NA),
    # pol_discuss_week.02 =  ifelse(w2g2 %in% 0:7, w2g2 / 7, NA), # Wave 2 responses dropped from scale to avoid propagating missingness
    pol_discuss_week.09 =  ifelse(w9h2 %in% 0:7, w9h2 / 7, NA),
    pol_discuss_week.10 =  ifelse(w10h2 %in% 0:7, w10h2 / 7, NA),
    pol_discuss_week.11 =  ifelse(w11h2 %in% 0:7, w11h2 / 7, NA),
    # pol_discuss_week.19 =  ifelse(w19h2 %in% 0:7, w19h2 / 7, NA), # Wave 19 responses dropped from scale to avoid propagating missingness
    discuss_week =         (pol_discuss_week.01 + 
                              pol_discuss_week.09 + 
                              pol_discuss_week.10 + 
                              pol_discuss_week.11) / 4, 
    
    # Ideology
    lib_con =  ifelse(der09w1 %in% 1:7, (der09w1 - 1) / 6, NA),
    ideo_ext = abs(lib_con - 0.5) * 2,
    
    # Partisanship (Wave 1)
    party_id =     ifelse(der08w1 %in% 0:6, der08w1 / 6, NA),
    partisan_ext = abs(party_id - 0.5) * 2,
    affect_dem =   ifelse(w1e2 == 1, car::recode(as.numeric(w1e3), "-9:-7 = 0.667; -6:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"),
                          ifelse(w1e2 == 2, car::recode(as.numeric(w1e4), "-9:-7 = 0.333; -6:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                 car::recode(as.numeric(w1e2), "-9:-7 = 0.5; -6:-1 = NA; 1 = 0.75; 2 = 0.25; 3 = 0.5"))),
    affect_rep =   ifelse(w1e5 == 1, car::recode(as.numeric(w1e6), "-9:-7 = 0.667; -6:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"),
                          ifelse(w1e5 == 2, car::recode(as.numeric(w1e7), "-9:-7 = 0.333; -6:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                 car::recode(as.numeric(w1e5), "-9:-7 = 0.5; -6:-1 = NA; 1 = 0.75; 2 = 0.25; 3 = 0.5"))),
    affect_polar = abs(affect_rep - affect_dem),
    
    # Social Policy 
    gay_marr_ban.01 = ifelse(w1p1 == 3, 0.5,
                             ifelse(w1p1 == 2, car::recode(as.numeric(w1p_o_2), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                    ifelse(w1p1 == 1, car::recode(as.numeric(w1p_f_2), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    gay_marr_ban.10 = ifelse(w10p1 == 3, 0.5,
                             ifelse(w10p1 == 2, car::recode(as.numeric(w10p2_oppose), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                    ifelse(w10p1 == 1, car::recode(as.numeric(w10p2_favor), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    
    abortion.02 = ifelse(w2n2 == 3, 0.5,
                         ifelse(w2n2 == 1, car::recode(as.numeric(w2n_f_3), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                ifelse(w2n2 == 2, car::recode(as.numeric(w2n_o_3), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    abortion.10 = ifelse(w10r2 == 3, 0.5,
                         ifelse(w10r2 == 1, car::recode(as.numeric(w10r3_favor), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                ifelse(w10r2 == 2, car::recode(as.numeric(w10r3_oppose), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    
    gay_marriage.11 = ifelse(w11n13_a == 3, 0.5,
                             ifelse(w11n13_a == 1, car::recode(as.numeric(w11n13_b), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                    ifelse(w11n13_a == 2, car::recode(as.numeric(w11n13_c), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    gay_marriage.13 = ifelse(w13n13 == 3, 0.5,
                             ifelse(w13n13 == 1, car::recode(as.numeric(w13n14_favor), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                    ifelse(w13n13 == 2, car::recode(as.numeric(w13n14_oppose), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    handgun_home.11 = ifelse(w11n9_a == 3, 0.5,
                             ifelse(w11n9_a == 1, car::recode(as.numeric(w11n9_b), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                    ifelse(w11n9_a == 2, car::recode(as.numeric(w11n9_c), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    handgun_home.13 = ifelse(w13n9 == 3, 0.5,
                             ifelse(w13n9 == 1, car::recode(as.numeric(w13n10_favor), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                    ifelse(w13n9 == 2, car::recode(as.numeric(w13n10_oppose), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    conceal_carry.11 = ifelse(w11n11_a == 3, 0.5,
                              ifelse(w11n11_a == 2, car::recode(as.numeric(w11n11_c), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                     ifelse(w11n11_a == 1, car::recode(as.numeric(w11n11_b), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    conceal_carry.13 = ifelse(w13n11 == 3, 0.5,
                              ifelse(w13n11 == 2, car::recode(as.numeric(w13n12_oppose), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                     ifelse(w13n11 == 1, car::recode(as.numeric(w13n12_favor), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    
    # Economic Policy 
    wealthy_tax.01 = ifelse(w1p4 == 3, 0.5,
                            ifelse(w1p4 == 1, car::recode(as.numeric(w1p_f_5), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                   ifelse(w1p4 == 2, car::recode(as.numeric(w1p_o_5), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    wealthy_tax.10 = ifelse(w10p4 == 3, 0.5,
                            ifelse(w10p4 == 1, car::recode(as.numeric(w10p5_favor), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                   ifelse(w10p4 == 2, car::recode(as.numeric(w10p5_oppose), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    
    gov_pay_drugs.01 = ifelse(w1p10 == 3, 0.5,
                              ifelse(w1p10 == 1, car::recode(as.numeric(w1p_f_11), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                     ifelse(w1p10 == 2, car::recode(as.numeric(w1p_o_11), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    gov_pay_drugs.10 = ifelse(w10p10 == 3, 0.5,
                              ifelse(w10p10 == 1, car::recode(as.numeric(w10p11_favor), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                     ifelse(w10p10 == 2, car::recode(as.numeric(w10p11_oppose), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    
    healthcare_spend.01 = ifelse(w1p13 == 3, 0.5,
                                 ifelse(w1p13 == 1, car::recode(as.numeric(w1p_f_14), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                        ifelse(w1p13 == 2, car::recode(as.numeric(w1p_o_14), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    healthcare_spend.10 = ifelse(w10p13 == 3, 0.5,
                                 ifelse(w10p13 == 1, car::recode(as.numeric(w10p14_favor), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                        ifelse(w10p13 == 2, car::recode(as.numeric(w10p14_oppose), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    
    income_equality.02 = ifelse(w2q1 == 3 | w2q2 == 3, 0.5, # "top 20%" and "top 5%" question wordings merged
                                ifelse(w2q1 == 1 | w2q2 == 1, car::recode(as.numeric(w2q3), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                       ifelse(w2q1 == 2 | w2q2 == 2, car::recode(as.numeric(w2q4), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    income_equality.11 = ifelse(w11t1 == 3 | w11t2 == 3, 0.5, # "top 20%" and "top 5%" question wordings merged
                                ifelse(w11t1 == 1 | w11t2 == 1, car::recode(as.numeric(w11t3), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                       ifelse(w11t1 == 2 | w11t2 == 2, car::recode(as.numeric(w11t4), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    income_equality.13 = ifelse(w13t1 == 3 | w13t2 == 3, 0.5, # "top 20%" and "top 5%" question wordings merged
                                ifelse(w13t1 == 1 | w13t2 == 1, car::recode(as.numeric(w13t3), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                       ifelse(w13t1 == 2 | w13t2 == 2, car::recode(as.numeric(w13t4), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    
    gov_services.11 = ifelse(w11x1 == 3, 0.5,
                             ifelse(w11x1 == 1, car::recode(as.numeric(w11x2), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                    ifelse(w11x1 == 2, car::recode(as.numeric(w11x3), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    gov_services.13 = ifelse(w13za1 == 3, 0.5,
                             ifelse(w13za1 == 1, car::recode(as.numeric(w13za2), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                    ifelse(w13za1 == 2, car::recode(as.numeric(w13za3), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    
    gov_business.11 = ifelse(w11x7 == 3, 0.5,
                             ifelse(w11x7 == 1, car::recode(as.numeric(w11x8a), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                    ifelse(w11x7 == 2, car::recode(as.numeric(w11x8b), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    gov_business.13 = ifelse(w13za7 == 3, 0.5,
                             ifelse(w13za7 == 1, car::recode(as.numeric(w13za8_more), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                    ifelse(w13za7 == 2, car::recode(as.numeric(w13za8_less), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    
    soc_security_priv.11 = ifelse(w11n1_a == 3, 0.5,
                                  ifelse(w11n1_a == 2, car::recode(as.numeric(w11n1_c), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                         ifelse(w11n1_a == 1, car::recode(as.numeric(w11n1_b), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    soc_security_priv.13 = ifelse(w13n1 == 3, 0.5,
                                  ifelse(w13n1 == 2, car::recode(as.numeric(w13n2_oppose), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                         ifelse(w13n1 == 1, car::recode(as.numeric(w13n2_favor), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    
    soc_security_inc.11 = ifelse(w11n3_a == 3, 0.5,
                                 ifelse(w11n3_a == 1, car::recode(as.numeric(w11n3_b), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                        ifelse(w11n3_a == 2, car::recode(as.numeric(w11n3_c), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    soc_security_inc.13 = ifelse(w13n3 == 3, 0.5,
                                 ifelse(w13n3 == 1, car::recode(as.numeric(w13n4_favor), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                        ifelse(w13n3 == 2, car::recode(as.numeric(w13n4_oppose), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    
    soc_security_tax.11 = ifelse(w11n5_a == 3, 0.5,
                                 ifelse(w11n5_a == 1, car::recode(as.numeric(w11n5_b), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                        ifelse(w11n5_a == 2, car::recode(as.numeric(w11n5_c), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    soc_security_tax.13 = ifelse(w13n5 == 3, 0.5,
                                 ifelse(w13n5 == 1, car::recode(as.numeric(w13n6_favor), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                        ifelse(w13n5 == 2, car::recode(as.numeric(w13n6_oppose), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    
    soc_security_age.11 = ifelse(w11n7_a == 3, 0.5,
                                 ifelse(w11n7_a == 2, car::recode(as.numeric(w11n7_c), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                        ifelse(w11n7_a == 1, car::recode(as.numeric(w11n7_b), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    soc_security_age.13 = ifelse(w13n7 == 3, 0.5,
                                 ifelse(w13n7 == 2, car::recode(as.numeric(w13n8_oppose), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                        ifelse(w13n7 == 1, car::recode(as.numeric(w13n8_favor), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    
    # Immigration Policy 
    immig_work.01 = ifelse(w1p22 == 3, 0.5,
                           ifelse(w1p22 == 1, car::recode(as.numeric(w1p_f_23), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                  ifelse(w1p22 == 2, car::recode(as.numeric(w1p_o_23), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    immig_work.10 = ifelse(w10p22 == 3, 0.5,
                           ifelse(w10p22 == 1, car::recode(as.numeric(w10p23_favor), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                  ifelse(w10p22 == 2, car::recode(as.numeric(w10p23_oppose), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    
    immig_citizen.01 = ifelse(w1p25 == 3, 0.5,
                              ifelse(w1p25 == 1, car::recode(as.numeric(w1p_f_26), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                     ifelse(w1p25 == 2, car::recode(as.numeric(w1p_o_26), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    immig_citizen.10 = ifelse(w10p25 == 3, 0.5,
                              ifelse(w10p25 == 1, car::recode(as.numeric(w10p26_favor), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                     ifelse(w10p25 == 2, car::recode(as.numeric(w10p26_oppose), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    
    # Racial Policy
    aa_university.09 = ifelse(w9qr1 == 3, 0.5,
                              ifelse(w9qr1 == 1, car::recode(as.numeric(w9qr2a), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                     ifelse(w9qr1 == 2, car::recode(as.numeric(w9qr2b), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    aa_university.13 = ifelse(w13n15 == 3, 0.5,
                              ifelse(w13n15 == 1, car::recode(as.numeric(w13n16_favor), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                     ifelse(w13n15 == 2, car::recode(as.numeric(w13n16_oppose), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    
    aa_employment.11 = ifelse(w11n15_a == 3, 0.5,
                              ifelse(w11n15_a == 1, car::recode(as.numeric(w11n15_b), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                     ifelse(w11n15_a == 2, car::recode(as.numeric(w11n15_c), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    aa_employment.13 = ifelse(w13n17 == 3, 0.5,
                              ifelse(w13n17 == 1, car::recode(as.numeric(w13n18_favor), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                     ifelse(w13n17 == 2, car::recode(as.numeric(w13n18_oppose), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    
    gov_fair_treat.09 = ifelse(w9zb1 == 3, 0.5,
                               ifelse(w9zb1 == 1, car::recode(as.numeric(w9zb2_favor), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                      ifelse(w9zb1 == 2, car::recode(as.numeric(w9zb2_oppose), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    gov_fair_treat.11 = ifelse(w11zb1 == 3, 0.5,
                               ifelse(w11zb1 == 1, car::recode(as.numeric(w11zb2a), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                      ifelse(w11zb1 == 2, car::recode(as.numeric(w11zb2b), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    gov_fair_treat.17 = ifelse(w17x1 == 3, 0.5,
                               ifelse(w17x1 == 1, car::recode(as.numeric(w17x_f_2), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                      ifelse(w17x1 == 2, car::recode(as.numeric(w17x_o_2), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    
    # Environmental Policy
    emissions.02 = ifelse(w2p9 == 3, 0.5,
                          ifelse(w2p9 == 1, car::recode(as.numeric(w2p_f_10), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                 ifelse(w2p9 == 2, car::recode(as.numeric(w2p_o_10), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    emissions.10 = ifelse(w10s9 == 3, 0.5,
                          ifelse(w10s9 == 1, car::recode(as.numeric(w10s10a), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                 ifelse(w10s9 == 2, car::recode(as.numeric(w10s10b), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    
    fuel_econ.02 = ifelse(w2p11 == 3, 0.5,
                          ifelse(w2p11 == 1, car::recode(as.numeric(w2p_f_12), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                 ifelse(w2p11 == 2, car::recode(as.numeric(w2p_o_12), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    fuel_econ.10 = ifelse(w10s11 == 3, 0.5,
                          ifelse(w10s11 == 1, car::recode(as.numeric(w10s12a), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                 ifelse(w10s11 == 2, car::recode(as.numeric(w10s12b), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    
    gas_tax.02 = ifelse(w2p13 == 3, 0.5,
                        ifelse(w2p13 == 1, car::recode(as.numeric(w2p_f_14), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                               ifelse(w2p13 == 2, car::recode(as.numeric(w2p_o_14), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    gas_tax.10 = ifelse(w10s13 == 3, 0.5,
                        ifelse(w10s13 == 1, car::recode(as.numeric(w10s14a), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                               ifelse(w10s13 == 2, car::recode(as.numeric(w10s14b), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    
    # Civil Liberties
    suspend_hc.01 = ifelse(w1p16 == 3, 0.5,
                           ifelse(w1p16 == 2, car::recode(as.numeric(w1p_o_17), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                  ifelse(w1p16 == 1, car::recode(as.numeric(w1p_f_17), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    suspend_hc.10 = ifelse(w10p16 == 3, 0.5,
                           ifelse(w10p16 == 2, car::recode(as.numeric(w10p17_oppose), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                  ifelse(w10p16 == 1, car::recode(as.numeric(w10p17_favor), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    
    wiretap.01 = ifelse(w1p19 == 3, 0.5,
                        ifelse(w1p19 == 2, car::recode(as.numeric(w1p_o_20), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                               ifelse(w1p19 == 1, car::recode(as.numeric(w1p_f_20), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    wiretap.10 = ifelse(w10p19 == 3, 0.5,
                        ifelse(w10p19 == 2, car::recode(as.numeric(w10p20_oppose), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                               ifelse(w10p19 == 1, car::recode(as.numeric(w10p20_favor), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    
    .keep = "none"
  )


### Prepare 2010 ANES Panel Recontact Study

# Load Data
ANES10 <- read_dta("ANES 2010 Recontact.dta")

ANES10 <-
  ANES10 %>%
  mutate(
    
    # Respondent ID
    case_id = as.numeric(caseid),
    
    # Social Welfare Policy (June 2010, month 30 of panel)
    wealthy_tax.30 = ifelse(f1z1 == 3, 0.5,
                            ifelse(f1z1 == 1, car::recode(as.numeric(f1z2_favor), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                   ifelse(f1z1 == 2, car::recode(as.numeric(f1z2_oppose), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    income_equality.30 = ifelse(f1zc1 == 3, 0.5,
                                ifelse(f1zc1 == 1, car::recode(as.numeric(f1zc2), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                       ifelse(f1zc1 == 2, car::recode(as.numeric(f1zc3), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    gov_services.30 = ifelse(f1y1 == 3, 0.5,
                             ifelse(f1y1 == 1, car::recode(as.numeric(f1y2), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                    ifelse(f1y1 == 2, car::recode(as.numeric(f1y3), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    
    
    # Environmental Policy (June 2010, month 30 of panel)
    gas_tax.30 = ifelse(f1zb4 == 3, 0.5,
                        ifelse(f1zb4 == 1, car::recode(as.numeric(f1zb5_favor), "-9:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                               ifelse(f1zb4 == 2, car::recode(as.numeric(f1zb5_oppose), "-9:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    
    .keep = "none"
  )


# Merge ANES 2008-2009 Panel Study with ANES 2010 Panel Recontact Study to Create 2008-2010 Panel
ANES08_10 <- merge(ANES08_09, ANES10, by = "case_id", all = T)

ANES08_10 <-
  ANES08_10 %>%
  mutate(
    
    ## Stability
    
    # Social/Moral/Cultural Policy
    gay_marr_ban_stblty =  revscale(sqrt(rowVars(as.matrix(data.frame(gay_marr_ban.01, gay_marr_ban.10))))),
    # abortion_stblty =    revscale(sqrt(rowVars(as.matrix(data.frame(abortion.02, abortion.10))))), Excluded due to >99% missingness
    gay_marriage_stblty =  revscale(sqrt(rowVars(as.matrix(data.frame(gay_marriage.11, gay_marriage.13))))),
    handgun_home_stblty =  revscale(sqrt(rowVars(as.matrix(data.frame(handgun_home.11, handgun_home.13))))),
    conceal_carry_stblty = revscale(sqrt(rowVars(as.matrix(data.frame(conceal_carry.11, conceal_carry.13))))),
    
    # Redistribution and Social Welfare Programs
    wealthy_tax_stblty =       revscale(sqrt(rowVars(as.matrix(data.frame(wealthy_tax.01, wealthy_tax.10, wealthy_tax.30))))),
    gov_pay_drugs_stblty =     revscale(sqrt(rowVars(as.matrix(data.frame(gov_pay_drugs.01, gov_pay_drugs.10))))),
    healthcare_spend_stblty =  revscale(sqrt(rowVars(as.matrix(data.frame(healthcare_spend.01, healthcare_spend.10))))),
    income_equality_stblty =   revscale(sqrt(rowVars(as.matrix(data.frame(income_equality.02, income_equality.11, income_equality.13, income_equality.30))))),
    gov_services_stblty =      revscale(sqrt(rowVars(as.matrix(data.frame(gov_services.11, gov_services.13, gov_services.30))))),
    gov_business_stblty =      revscale(sqrt(rowVars(as.matrix(data.frame(gov_business.11, gov_business.13))))),
    soc_security_priv_stblty = revscale(sqrt(rowVars(as.matrix(data.frame(soc_security_priv.11, soc_security_priv.13))))),
    soc_security_inc_stblty =  revscale(sqrt(rowVars(as.matrix(data.frame(soc_security_inc.11, soc_security_inc.13))))),
    soc_security_tax_stblty =  revscale(sqrt(rowVars(as.matrix(data.frame(soc_security_tax.11, soc_security_tax.13))))),
    soc_security_age_stblty =  revscale(sqrt(rowVars(as.matrix(data.frame(soc_security_age.11, soc_security_age.13))))),
    
    # Immigration
    immig_work_stblty =    revscale(sqrt(rowVars(as.matrix(data.frame(immig_work.01, immig_work.10))))),
    immig_citizen_stblty = revscale(sqrt(rowVars(as.matrix(data.frame(immig_citizen.01, immig_citizen.10))))),
    
    # Racial Policy
    aa_university_stblty =  revscale(sqrt(rowVars(as.matrix(data.frame(aa_university.09, aa_university.13))))),
    aa_employment_stblty =  revscale(sqrt(rowVars(as.matrix(data.frame(aa_employment.11, aa_employment.13))))),
    gov_fair_treat_stblty = revscale(sqrt(rowVars(as.matrix(data.frame(gov_fair_treat.09, gov_fair_treat.11, gov_fair_treat.17))))),
    
    # Environmental Policy
    emissions_stblty = revscale(sqrt(rowVars(as.matrix(data.frame(emissions.02, emissions.10))))),
    fuel_econ_stblty = revscale(sqrt(rowVars(as.matrix(data.frame(fuel_econ.02, fuel_econ.10))))),
    gas_tax_stblty =   revscale(sqrt(rowVars(as.matrix(data.frame(gas_tax.02, gas_tax.10, gas_tax.30))))),
    
    # Civil Liberties
    suspend_hc_stblty = revscale(sqrt(rowVars(as.matrix(data.frame(suspend_hc.01, suspend_hc.10))))),
    wiretap_stblty =    revscale(sqrt(rowVars(as.matrix(data.frame(wiretap.01, wiretap.10)))))
    
  )

## Constraint
global08_10 <- ANES08_10[c("gay_marr_ban.01", "abortion.02", "gay_marriage.11", "wealthy_tax.01", "gov_pay_drugs.01", "healthcare_spend.01", "gov_services.11", 
                           "soc_security_inc.11", "soc_security_tax.11", "income_equality.02", "gov_business.11", "soc_security_priv.11", "immig_work.01", "immig_citizen.01",
                           "aa_university.09", "aa_employment.11", "gov_fair_treat.09", "emissions.02", "fuel_econ.02", "gas_tax.02")]
social08_10 <- ANES08_10[c("gay_marr_ban.01", "abortion.02", "gay_marriage.11", "handgun_home.11", "conceal_carry.11")]
econ08_10 <- ANES08_10[c("wealthy_tax.01", "gov_pay_drugs.01", "healthcare_spend.01", "income_equality.02", "gov_services.11", "gov_business.11", "soc_security_priv.11", 
                         "soc_security_inc.11", "soc_security_tax.11", "soc_security_age.11")]

ANES08_10$global_const <- constraint(global08_10, 10) # Allow 10% Missing Policies
ANES08_10$social_const <- constraint(social08_10, 10) # Allow 10% Missing Policies
ANES08_10$econ_const <- constraint(econ08_10, 10) # Allow 10% Missing Policies

## Stability
global_stblty08_10 <- ANES08_10 %>% dplyr::select(ends_with("stblty"))
econ_stblty08_10 <- ANES08_10[c("wealthy_tax_stblty", "gov_pay_drugs_stblty", "healthcare_spend_stblty", "income_equality_stblty", "gov_services_stblty",
                                "gov_business_stblty", "soc_security_priv_stblty", "soc_security_inc_stblty", "soc_security_tax_stblty", "soc_security_age_stblty")]
social_stblty08_10 <- ANES08_10[c("gay_marr_ban_stblty", "gay_marriage_stblty", "handgun_home_stblty", "conceal_carry_stblty")]

ANES08_10$global_stblty <- avg_stblty(global_stblty08_10, 50) # Allow 50% Missing Policies
ANES08_10$social_stblty <- avg_stblty(social_stblty08_10, 10) # Allow 10% Missing Policies
ANES08_10$econ_stblty <- avg_stblty(econ_stblty08_10, 10) # Allow 10% Missing Policies





#========================================================================#
#                      Create 2012-2013 ANES Panel                       #
#========================================================================#

### Prepare 2012 ANES Time Series

# Load Data
ANES12 <- as.data.frame(read_dta("ANES 2012.dta"))

ANES12 <-
  ANES12 %>% 
  mutate(
    
    # Respondent ID
    case_id = as.numeric(caseid),
    
    # Demographics
    age =       ifelse(dem_age_r_x %in% 17:90, as.numeric(dem_age_r_x), NA),
    male =      as.numeric(2 - gender_respondent_x),
    race =      relevel(factor(suppressWarnings(car::recode(as.numeric(dem_raceeth_x), "1 = 'White'; 2 = 'Black'; 3 = 'Asian'; 5 = 'Hispanic'; 4 = 'Other'; 6 = 'Other'; -9 = NA"))), "White"),
    black =     ifelse(is.na(race), NA, ifelse(race == "Black", 1, 0)),
    hispanic =  ifelse(is.na(race), NA, ifelse(race == "Hispanic", 1, 0)),
    asian =     ifelse(is.na(race), NA, ifelse(race == "Asian", 1, 0)),
    other =     ifelse(is.na(race), NA, ifelse(race == "Other", 1, 0)),
    education = relevel(factor(suppressWarnings(car::recode(as.numeric(dem_edu), "-9 = NA; 1:12 = 'Less than bachelors degree'; 13 = 'Bachelors degree'; 14:16 = 'Advanced degree'; 95 = NA")),
                               levels = c("Less than bachelors degree", "Bachelors degree", "Advanced degree")), "Less than bachelors degree"),
    bachelors = ifelse(is.na(education), NA, ifelse(education == "Bachelors degree", 1, 0)),
    post_grad = ifelse(is.na(education), NA, ifelse(education == "Advanced degree", 1, 0)),
    income =    ifelse(inc_incgroup_pre %in% -9:-2, NA, (inc_incgroup_pre - 1) / 27),
    
    # Verbal Ability
    word1 =  ifelse(wordsum_setb == 5, 1, 0),
    word2 =  ifelse(wordsum_setd == 3, 1, 0), 
    word3 =  ifelse(wordsum_sete == 1, 1, 0),
    word4 =  ifelse(wordsum_setf == 3, 1, 0), 
    word5 =  ifelse(wordsum_setg == 5, 1, 0), 
    word6 =  ifelse(wordsum_seth == 4, 1, 0), 
    word7 =  ifelse(wordsum_setj == 1, 1, 0), 
    word8 =  ifelse(wordsum_setk == 1, 1, 0), 
    word9 =  ifelse(wordsum_setl == 4, 1, 0), 
    word10 = ifelse(wordsum_seto == 2, 1, 0),
    wordsum_scale = (word1 + word2 + word3 + word4 + word5 + word6 + word7 + word8 + word9 + word10)  / 10,
    
    # Need To Evaluate
    many_opinions = ifelse(cog_opinions %in% 1:4, (4 - cog_opinions) / 3, NA),
    more_opinions = ifelse(cog_opin_x %in% 1:5, (cog_opin_x - 1) / 4, NA),   
    nte_scale = (many_opinions + more_opinions) / 2,
    
    # Political Knowledge
    know_deficit =    ifelse(preknow_sizedef == 1, 1, 0), 
    know_medicare =   ifelse(preknow_medicare == 1, 1, 0),
    know_senate =     ifelse(preknow_senterm == 6, 1, 0),
    know_spend =      ifelse(preknow_leastsp == 1, 1, 0),
    know_pres_times = ifelse(preknow_prestimes == 2, 1, 0),
    know_boehner =    ifelse(ofcrec_speaker_correct %in% 0:1, ofcrec_speaker_correct, NA),
    know_biden =      ifelse(ofcrec_vp_correct %in% 0:1, ofcrec_vp_correct, NA),
    know_cameron =    ifelse(ofcrec_pmuk_correct %in% 0:1, ofcrec_pmuk_correct, NA),
    know_roberts =    ifelse(ofcrec_cj_correct >= 0, ofcrec_cj_correct, NA),
    know_house_maj =  ifelse(knowl_housemaj %in% -7:-6, NA, ifelse(knowl_housemaj == 2, 1, 0)),
    know_sen_maj =    ifelse(knowl_senmaj %in% -7:-6, NA, ifelse(knowl_senmaj == 1, 1, 0)),
    know_treasury =   ifelse(cses_poliinfone %in% -7:-6, NA, ifelse(cses_poliinfone == 4, 1, 0)),
    know_unemploy =   ifelse(cses_polinftwo %in% -7:-6, NA, ifelse(cses_polinftwo == 2, 1, 0)),
    know_scnd_place = ifelse(cses_poliinfthree %in% -7:-6, NA, ifelse(cses_poliinfthree == 1, 1, 0)),
    know_un_sec =     ifelse(cses_poliinffour %in% -7:-6, NA, ifelse(cses_poliinffour == 3, 1, 0)),
    pol_know_scale = (know_deficit + know_medicare + know_senate + know_spend + know_pres_times + know_boehner + know_biden + know_cameron + 
                        know_roberts + know_house_maj + know_sen_maj + know_treasury + know_unemploy + know_scnd_place + know_un_sec) / 15,
    
    # Raw values for IRT
    know_roberts_raw = ifelse(ofcrec_cj_correct >= 0, ofcrec_cj_correct * 2, NA),
    
    # Placement Knowledge
    place_gov_jobs =   ifelse(guarpr_dpc %in% -9:-8 | guarpr_rpc %in% -9:-8, 0, ifelse(guarpr_dpc < guarpr_rpc, 1, 0)),
    place_gov_insur =  ifelse(inspre_dpc %in% -9:-8 | inspre_rpc %in% -9:-8, 0, ifelse(inspre_dpc < inspre_rpc, 1, 0)),
    place_aid_black =  ifelse(aidblack_dpc %in% -9:-8 | aidblack_rpc %in% -9:-8, 0, ifelse(aidblack_dpc < aidblack_rpc, 1, 0)),
    place_def_spend =  ifelse(defsppr_dpc %in% -9:-8 | defsppr_rpc %in% -9:-8, 0, ifelse(defsppr_dpc < defsppr_rpc, 1, 0)),
    place_abortion =   ifelse(abort_dpc4 %in% c(-7,-6,5) | abort_rpc4 %in% c(-7,-6,5), NA, ifelse(abort_dpc4 %in% -9:-8 | abort_rpc4 %in% -9:-8, 0, ifelse(abort_dpc4 > abort_rpc4, 1, 0))),
    place_enviro =     ifelse(envjob_dpc %in% -9:-8 | envjob_rpc %in% -9:-8, 0, ifelse(envjob_dpc < envjob_rpc, 1, 0)),
    place_spend_serv = ifelse(spsrvpr_ssdpc %in% -9:-8 | spsrvpr_ssrpc %in% -9:-8, 0, ifelse(spsrvpr_ssdpc > spsrvpr_ssrpc, 1, 0)),
    place_lib_con =    ifelse(libcpre_dpc %in% -9:-8 | libcpre_rpc %in% -9:-8, 0, ifelse(libcpre_dpc < libcpre_rpc, 1, 0)),
    place_know_scale = (place_gov_jobs + place_gov_insur + place_aid_black + place_def_spend + place_abortion + place_enviro + place_spend_serv + place_lib_con) / 8,
    
    # Political Interest
    politics_interest =  ifelse(paprofile_interestpolit %in% 1:4, (4 - paprofile_interestpolit) / 3, NA),
    campaign_interest =  ifelse(interest_following %in% 1:3, (3 - interest_following) / 2, NA),
    attention_politics = ifelse(interest_attention %in% 1:5, (5 - interest_attention) / 4, NA),
    pol_interest_scale = ifelse(is.na(politics_interest), # > 2,000 missing due to unit non-response.
                                (campaign_interest + attention_politics) / 2, # Allow respondents to be scored on just these two items
                                (politics_interest + campaign_interest + attention_politics) / 3),
    
    # Raw Values for IRT
    politics_interest_raw =  ifelse(paprofile_interestpolit %in% 1:4, 4 - paprofile_interestpolit, NA),
    campaign_interest_raw =  ifelse(interest_following %in% 1:3, 3 - interest_following, NA),
    attention_politics_raw = ifelse(interest_attention %in% 1:5, 5 - interest_attention, NA),
    
    # Attention to Politics in Media
    attention_tv =        ifelse(prmedia_wktvnws == 0, 0, ifelse(prmedia_attvnews %in% 1:5, (5 - prmedia_attvnews) / 4, NA)),
    attention_radio =     ifelse(prmedia_wkrdnws == 0, 0, ifelse(prmedia_atrdnews %in% 1:5, (5 - prmedia_atrdnews) / 4, NA)),
    attention_internet =  ifelse(prmedia_wkinews == 0, 0, ifelse(prmedia_atinews %in% 1:5, (5 - prmedia_atinews) / 4, NA)),
    attention_newspaper = ifelse(prmedia_wkpaprnws == 0, 0, ifelse(prmedia_atpprnews %in% 1:5, (5 - prmedia_atpprnews) / 4, NA)),
    att_pol_media_scale = (attention_tv + attention_radio + attention_internet + attention_newspaper) / 4,
    
    # Raw Values for IRT
    attention_tv_raw =        ifelse(prmedia_wktvnws == 0, 0, ifelse(prmedia_attvnews %in% 1:5, 5 - prmedia_attvnews, NA)),
    attention_radio_raw =     ifelse(prmedia_wkrdnws == 0, 0, ifelse(prmedia_atrdnews %in% 1:5, 5 - prmedia_atrdnews, NA)),
    attention_internet_raw =  ifelse(prmedia_wkinews == 0, 0, ifelse(prmedia_atinews %in% 1:5, 5 - prmedia_atinews, NA)),
    attention_newspaper_raw = ifelse(prmedia_wkpaprnws == 0, 0, ifelse(prmedia_atpprnews %in% 1:5, 5 - prmedia_atpprnews, NA)),
    
    # Weekly Media Consumption
    tv_week =        ifelse(prmedia_wktvnws %in% 0:7, prmedia_wktvnws / 7, NA),
    radio_week =     ifelse(prmedia_wkrdnws %in% 0:7, prmedia_wkrdnws / 7, NA),
    internet_week =  ifelse(prmedia_wkinews %in% 0:7, prmedia_wkinews / 7, NA),
    newspaper_week = ifelse(prmedia_wkpaprnws %in% 0:7, prmedia_wkpaprnws / 7, NA),
    media_week = (tv_week + radio_week + internet_week + newspaper_week) / 4,
    
    # Raw Values for IRT
    tv_week_raw =        ifelse(prmedia_wktvnws %in% 0:7, prmedia_wktvnws, NA),
    radio_week_raw =     ifelse(prmedia_wkrdnws %in% 0:7, prmedia_wkrdnws, NA),
    internet_week_raw =  ifelse(prmedia_wkinews %in% 0:7, prmedia_wkinews, NA),
    newspaper_week_raw = ifelse(prmedia_wkpaprnws %in% 0:7, prmedia_wkpaprnws, NA),
    
    # Political Discussion
    discuss_week = ifelse(discuss_disc == 2, 0, ifelse(discuss_discpstwk %in% 0:7, discuss_discpstwk / 7, NA)),
    
    # Raw Values for IRT
    discuss_week_raw = ifelse(discuss_disc == 2, 0, ifelse(discuss_discpstwk %in% 0:7, discuss_discpstwk, NA)),
    
    # Media Sources
    fox = ifelse(medsrc_tvprog_21 == 1 |    # The Five
                   medsrc_tvprog_22 == 1 |  # Fox Report
                   medsrc_tvprog_25 == 1 |  # Hannity
                   medsrc_tvprog_26 == 1 |  # Huckabee
                   medsrc_tvprog_36 == 1 |  # O'Reilly Factor
                   medsrc_tvprog_37 == 1 |  # On the Record with Greta Van Susteren
                   medsrc_tvprog_41 == 1 |  # Special Report with Bret Baier
                   medsrc_websites_05 == 1, # foxnews.com
                 1, 0),
    msnbc = ifelse(medsrc_websites_10 == 1 | # msnbc.msn.com
                     medsrc_tvprog_13 == 1,  # Chris Matthews Show
                   1, 0),
    cable = ifelse(is.na(fox) & is.na(msnbc), NA, ifelse(fox == 1 | msnbc == 1, 1, 0)),
    papers = ifelse(medsrc_printnews_01 == 1 |   # New York Times
                      medsrc_inetnews_01 == 1 |  # www.nytimes.com
                      medsrc_websites_11 == 1 |  # nytimes.com
                      medsrc_printnews_04 == 1 | # Washington Post
                      medsrc_inetnews_04 == 1 |  # www.washingtonpost.com
                      medsrc_websites_14 == 1 |  # washingtonpost.com
                      medsrc_printnews_03 == 1 | # Wall Street Journal
                      medsrc_inetnews_03 == 1,   # online.wsj.com
                    1, 0),
    talk_radio = ifelse(medsrc_radio_02 == 1 |   # The Dave Ramsey Show
                          medsrc_radio_05 == 1 | # Glenn Beck Program
                          medsrc_radio_06 == 1 | # The Laura Ingraham Show
                          medsrc_radio_07 == 1 | # The Mark Levin Show
                          medsrc_radio_11 == 1 | # The Rush Limbaugh Show
                          medsrc_radio_12 == 1 | # The Savage Nation (Michael Savage)
                          medsrc_radio_13 == 1,  # The Sean Hannity Show
                        1, 0),
    npr = ifelse(medsrc_radio_01 == 1 |   # All Things Considered (NPR)
                   medsrc_radio_04 == 1 | # Fresh Air (NPR)
                   medsrc_radio_08 == 1 | # Morning Edition (NPR)
                   medsrc_radio_14 == 1,  # Talk of the Nation (NPR)
                 1, 0),
    
    # Ideology
    lib_con =  ifelse(libcpre_self == -2, 0.5, ifelse(libcpre_self %in% 1:7, (libcpre_self - 1) / 6, NA)),
    ideo_ext = abs(lib_con - 0.5) * 2,
    
    # Partisanship
    party_id =     ifelse(pid_x %in% 1:7, (pid_x - 1) / 6, NA),
    partisan_ext = abs(party_id - 0.5) * 2,
    affect_dem =   ifelse(ft_dem %in% 0:100, ft_dem / 100, NA),
    affect_rep =   ifelse(ft_rep %in% 0:100, ft_rep / 100, NA),
    affect_polar = abs(affect_rep - affect_dem),
    
    # Social Policy
    abortion.12 =        ifelse(abortpre_4point %in% 1:4, (4 - abortpre_4point) / 3, NA),
    gay_job_discrim.12 = ifelse(gayrt_discstd_x %in% 1:5, (gayrt_discstd_x - 1) / 4, # Standard and revised item versions combined
                                ifelse(gayrt_discrev_x %in% 1:5, (gayrt_discrev_x - 1) / 4, NA)),
    gay_military.12 =    ifelse(gayrt_milstd_x %in% 1:5, (gayrt_milstd_x - 1) / 4, # Standard and revised item versions combined
                                ifelse(gayrt_milrev_x %in% 1:5, (gayrt_milrev_x - 1) / 4, NA)),
    gay_adoption.12 =    ifelse(gayrt_adopt %in% 1:2, gayrt_adopt - 1, NA),
    gay_marriage.12 =    ifelse(gayrt_marry %in% 1:3, (gayrt_marry - 1) / 2, NA),
    marijuana.12 =       ifelse(pot_legal == 3, 0.5, ifelse(pot_legal %in% 1:2, pot_legal - 1, NA)),
    death_penalty.12 = ifelse(penalty_favopp_x %in% 1:5, (5 - penalty_favopp_x) / 4, NA),
    gun_control.12 =   ifelse(gun_control %in% 1:3, ifelse(gun_control==3, 0.5, gun_control - 1), NA),
    
    # Social Welfare Spending
    soc_sec_spend.12 =   ifelse(fedspend_ss == 3, 0.5, ifelse(fedspend_ss %in% 1:2, fedspend_ss - 1, NA)),
    childcare_spend.12 = ifelse(fedspend_child == 3, 0.5, ifelse(fedspend_child %in% 1:2, fedspend_child - 1, NA)),
    school_spend.12 =    ifelse(fedspend_schools == 3, 0.5, ifelse(fedspend_schools %in% 1:2, fedspend_schools - 1, NA)),
    welfare_spend.12 =   ifelse(fedspend_welfare == 3, 0.5, ifelse(fedspend_welfare %in% 1:2, fedspend_welfare - 1, NA)),
    poor_spend.12 =      ifelse(fedspend_poor == 3, 0.5, ifelse(fedspend_poor %in% 1:2, fedspend_poor - 1, NA)),
    gov_services.12 =    ifelse(spsrvpr_ssself == -2, 0.5, ifelse(spsrvpr_ssself %in% 1:7, (7 - spsrvpr_ssself) / 6, NA)),
    wealthy_tax.12 =     ifelse(milln_milltax_x %in% 1:7, (milln_milltax_x - 1) / 6, NA),
    
    # Economic Policy
    income_equality.12 =  ifelse(cses_govtact %in% 1:5, (cses_govtact - 1) / 4, NA),
    guar_job_living.12 =  ifelse(guarpr_self == -2, 0.5, ifelse(guarpr_self %in% 1:7, (guarpr_self - 1) / 6, NA)),
    public_insurance.12 = ifelse(inspre_self == -2, 0.5, ifelse(inspre_self %in% 1:7, (inspre_self - 1) / 6, NA)),
    insurance_law.12 =    ifelse(health_2010hcr_x %in% 1:7, (health_2010hcr_x - 1) / 6, NA),
    bank_bailout.12 =     ifelse(tarp_favopp_x %in% 1:5, (tarp_favopp_x - 1) / 4, NA),
    reduce_deficit.12 =   ifelse(budget_deficit_x %in% 1:7, (7 - budget_deficit_x) / 6, NA),
    deficit_imp.12 =      ifelse(budget_defimp %in% 1:5, (5 - budget_defimp) / 4, NA),
    deficit_tax.12 =      ifelse(budget_rdef250k == 3, 0.5, ifelse(budget_rdef250k %in% 1:2, budget_rdef250k - 1, NA)),
    regulation.12 =       ifelse(govrole_regbus %in% 1:5, (govrole_regbus - 1) / 4, NA),
    
    # Immigration Policy
    immigration_levels.12 =  ifelse(immigpo_level %in% 1:5, (immigpo_level - 1) / 4, NA),
    illegal_policy.12 =      ifelse(immig_policy %in% 1:4, (4 - immig_policy) / 3, NA),
    illegal_citizen.12 =     ifelse(immig_citizen == 3, 0.5, ifelse(immig_citizen %in% 1:2, immig_citizen - 1, NA)),
    legal_status_checks.12 = ifelse(immig_checks == 3, 0.5, ifelse(immig_checks %in% 1:2, 2 - immig_checks, NA)),
    
    # Racial Policy
    aa_hire_promotion.12 = ifelse(aapost_hire_x %in% 1:5, (aapost_hire_x - 1) / 4, NA),
    aa_at_work.12 =        ifelse(aa_work_x %in% 1:7, (aa_work_x - 1) / 6, NA),
    aa_university.12 =     ifelse(aa_uni_x %in% 1:7, (aa_uni_x - 1) / 6, NA),
    gov_fair_treat.12 =    ifelse(fairjob_opin_x == -1, 0.5, ifelse(fairjob_opin_x %in% 1:5, (fairjob_opin_x - 1) / 4, NA)),
    gov_help_blacks.12 =   ifelse(aidblack_self == -2, 0.5, ifelse(aidblack_self %in% 1:7, (aidblack_self - 1) / 6, NA)),
    
    # Environmental Policy
    enviro_regulate.12 = ifelse(envjob_self == -2, 0.5, ifelse(envjob_self %in% 1:7, (envjob_self - 1) / 6, NA)),
    offshore_drill.12 =  ifelse(envir_drill == 3, 0.5, ifelse(envir_drill %in% 1:2, 2 - envir_drill, NA)),
    spend_enviro.12 =    ifelse(fedspend_enviro == 3, 0.5, ifelse(fedspend_enviro %in% 1:2, fedspend_enviro - 1, NA)),
    
    # Miscellaneous
    defense_spend.12 = ifelse(defsppr_self == -2, 0.5, ifelse(defsppr_self %in% 1:7, (defsppr_self - 1) / 6, NA)),
    
    .keep = "none"
  )


### Prepare ANES 2013 Internet Recontact Study 

# Load Data
ANES13 <- as.data.frame(read_dta("ANES 2013 Recontact.dta"))

ANES13 <-
  ANES13 %>% 
  mutate(
    
    # Respondent ID
    case_id = as.numeric(caseid),
    
    # Need For Cognition
    like_thinking = ifelse(C5_E1 == 3, 0.5,
                           ifelse(C5_E1 == 2, car::recode(as.numeric(C5_E2_Dislike), "-7:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                  ifelse(C5_E1 == 1, car::recode(as.numeric(C5_E2_Like), "-7:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    prefer_complex = ifelse(C5_E3 %in% 1:2, C5_E3 - 1, NA),
    nfc_scale = (like_thinking + prefer_complex) / 2,
    
    # Need For Affect
    in_touch_feelings =   ifelse(C5_ZC1 %in% 1:7, (C5_ZC1 - 1) / 6, NA),
    explore_feelings =    ifelse(C5_ZC2 %in% 1:7, (C5_ZC2 - 1) / 6, NA),
    emotions_overwhelm =  ifelse(C5_ZC3 %in% 1:7, (7 - C5_ZC3) / 6, NA),
    not_handle_emotions = ifelse(C5_ZC4 %in% 1:7, (7 - C5_ZC4) / 6, NA),
    nfa_scale = (in_touch_feelings + explore_feelings + emotions_overwhelm + not_handle_emotions) / 4,
    
    # Economic Policy
    deficit_imp.13 = ifelse(C5_X1 %in% 1:5, (5 - C5_X1) / 4, NA),
    deficit_tax.13 = ifelse(C5_X3 == 3, 0.5, ifelse(C5_X3 %in% 1:2, C5_X3 - 1, NA)), # Branch items not used to match item in 2012 Time Series
    
    # Miscellaneous
    defense_spend.13 = ifelse(C5_V1 == 3, 0.5,
                              ifelse(C5_V1 == 2, car::recode(as.numeric(C5_V2), "-7:-1 = NA; 1 = 0; 2 = 0.167; 3 = 0.333"),
                                     ifelse(C5_V1 == 1, car::recode(as.numeric(C5_V2), "-7:-1 = NA; 1 = 1; 2 = 0.833; 3 = 0.667"), NA))),
    
    # Gun Control
    gun_control.13 =      ifelse(C5_W1 %in% 1:3, ifelse(C5_W1 == 3, 0.5, C5_W1 - 1), NA),
    
    .keep = "none"
  )

### Merge 2012 ANES Time Series and 2013 ANES Internet Recontact Study to create panel
ANES12_13 <- merge(ANES12, ANES13, by = "case_id", all = T)

## Constraint
global12_13 <- ANES12_13[c("abortion.12", "gay_job_discrim.12", "gay_military.12", "gay_adoption.12", "gay_marriage.12", "marijuana.12", "guar_job_living.12", "soc_sec_spend.12", "childcare_spend.12",
                           "school_spend.12", "welfare_spend.12", "poor_spend.12", "gov_services.12", "wealthy_tax.12", "income_equality.12", "public_insurance.12", "insurance_law.12", "bank_bailout.12",
                           "reduce_deficit.12", "deficit_imp.12", "regulation.12", "immigration_levels.12", "illegal_policy.12", "illegal_citizen.12", "legal_status_checks.12", "aa_hire_promotion.12",
                           "aa_at_work.12", "aa_university.12", "gov_fair_treat.12", "gov_help_blacks.12", "enviro_regulate.12", "offshore_drill.12", "spend_enviro.12", "death_penalty.12", "gun_control.12")]
social12_13 <- ANES12_13[c("abortion.12", "gay_job_discrim.12", "gay_military.12", "gay_adoption.12", "gay_marriage.12", "marijuana.12", "death_penalty.12", "gun_control.12")]
econ12_13 <- ANES12_13[c("soc_sec_spend.12", "childcare_spend.12", "school_spend.12", "welfare_spend.12", "poor_spend.12", "gov_services.12", "wealthy_tax.12", "guar_job_living.12", "income_equality.12", 
                         "public_insurance.12", "insurance_law.12", "bank_bailout.12", "reduce_deficit.12", "deficit_imp.12", "regulation.12")]

ANES12_13$global_const <- constraint(global12_13, 10) # Allow 10% Missing Policies
ANES12_13$social_const <- constraint(social12_13, 10) # Allow 10% Missing Policies
ANES12_13$econ_const <- constraint(econ12_13, 10) # Allow 10% Missing Policies

## Stability
ANES12_13 <-
  ANES12_13 %>% 
  mutate(
    
    deficit_imp_stblty =   revscale(sqrt(rowVars(as.matrix(data.frame(deficit_imp.12, deficit_imp.13))))),    
    deficit_tax_stblty =   revscale(sqrt(rowVars(as.matrix(data.frame(deficit_tax.12, deficit_tax.13))))),
    defense_spend_stblty = revscale(sqrt(rowVars(as.matrix(data.frame(defense_spend.12, defense_spend.13))))),
    gun_control_stblty =   revscale(sqrt(rowVars(as.matrix(data.frame(gun_control.12, gun_control.13))))),
    
    # Global Stability
    global_stblty = scale(deficit_imp_stblty + deficit_tax_stblty + defense_spend_stblty + gun_control_stblty),
  )





#========================================================================#
#                      Create 2016-2020 ANES Panel                       #
#========================================================================#

### Prepare 2016 ANES Time Series

# Load Data
ANES16 <- as.data.frame(read_dta("ANES 2016.dta"))

ANES16 <-
  ANES16 %>% 
  mutate(
    
    # Respondent ID
    case_id = as.numeric(V160001_orig),
    
    # Demographics
    age =       ifelse(V161267 %in% 18:90, V161267, NA),
    male =      ifelse(V161342 %in% 1:3, ifelse(V161342 == 1, 1, 0), NA),
    race =      relevel(factor(suppressWarnings(car::recode(as.numeric(V161310x), "1 = 'White'; 2 = 'Black'; 5 = 'Hispanic'; 3 = 'Asian'; 4 = 'Other'; 6 = 'Other'; -9:-2 = NA"))), "White"),
    black =     ifelse(is.na(race), NA, ifelse(race == "Black", 1, 0)),
    hispanic =  ifelse(is.na(race), NA, ifelse(race == "Hispanic", 1, 0)),
    asian =     ifelse(is.na(race), NA, ifelse(race == "Asian", 1, 0)),
    other =     ifelse(is.na(race), NA, ifelse(race == "Other", 1, 0)),
    education = relevel(factor(suppressWarnings(car::recode(as.numeric(V161270), "-9 = NA; 1:12 = 'Less than bachelors degree'; 13 = 'Bachelors degree'; 14:16 = 'Advanced degree'; 90 = 'Less than bachelors degree'; 95 = NA")),
                               levels = c("Less than bachelors degree", "Bachelors degree", "Advanced degree")), "Less than bachelors degree"),
    bachelors = ifelse(is.na(education), NA, ifelse(education == "Bachelors degree", 1, 0)),
    post_grad = ifelse(is.na(education), NA, ifelse(education %in% c("Masters degree", "Advanced degree"), 1, 0)),
    degree.16 = relevel(factor(suppressWarnings(car::recode(as.numeric(V161270), "-9 = NA; 1:8 = 'No high school'; 9:10 = 'High school graduate'; 11:12 = 'Associate degree'; 13 = 'Bachelors degree'; 14 = 'Masters degree'; 15:16 = 'Advanced degree'; 90 = 'High school graduate'; 95 = NA")),
                               levels = c("No high school", "High school graduate", "Associate degree", "Bachelors degree", "Masters degree", "Advanced degree")), "No high school"),
    income =    ifelse(V161361x %in% -9:-5, NA, (V161361x - 1) / 27),
    
    # Verbal Ability
    word1 = ifelse(V161497==-5, NA, ifelse(V161497 == 1, 1, 0)), 
    word2 = ifelse(V161498==-5, NA, ifelse(V161498 == 1, 1, 0)), 
    word3 = ifelse(V161499==-5, NA, ifelse(V161499 == 1, 1, 0)), 
    word4 = ifelse(V161500==-5, NA, ifelse(V161500 == 1, 1, 0)), 
    word5 = ifelse(V161501==-5, NA, ifelse(V161501 == 1, 1, 0)), 
    word6 = ifelse(V161502==-5, NA, ifelse(V161502 == 1, 1, 0)), 
    word7 = ifelse(V161503==-5, NA, ifelse(V161503 == 1, 1, 0)), 
    word8 = ifelse(V161504==-5, NA, ifelse(V161504 == 1, 1, 0)), 
    word9 = ifelse(V161505==-5, NA, ifelse(V161505 == 1, 1, 0)), 
    word10 = ifelse(V161506==-5, NA, ifelse(V161506 == 1, 1, 0)),
    wordsum_scale = (word1 + word2 + word3 + word4 + word5 + word6 + word7 + word8 + word9 + word10) / 10,
    
    # Need To Evaluate
    strong_opinions = ifelse(V162248 %in% 1:5, (V162248 - 1) / 4, NA),
    # opin_everything = ifelse(V162249 %in% 1:5, (V162249 - 1) / 4, NA), # Excluded due to missingness
    import_opinion =  ifelse(V162250 %in% 1:5, (V162250 - 1) / 4, NA),
    # bother_neutral =  ifelse(V162251 %in% 1:5, (V162251 - 1) / 4, NA), # Excluded due to missingness
    # more_opinions =   ifelse(V162252 %in% 1:5, (V162252 - 1) / 4, NA), # Excluded due to missingness
    rather_strong =   ifelse(V162253 %in% 1:5, (V162253 - 1) / 4, NA),
    nte_scale =       (strong_opinions + import_opinion + rather_strong) / 3,
    
    # Political Knowledge
    know_senate =    ifelse(V161513 == -5, NA, ifelse(V161513 == 6, 1, 0)),
    know_spend =     ifelse(V161514 == -5, NA, ifelse(V161514 == 1, 1, 0)),
    know_house_maj = ifelse(V161515 == -5, NA, ifelse(V161515 == 2, 1, 0)),
    know_sen_maj =   ifelse(V161516 == -5, NA, ifelse(V161516 == 2, 1, 0)),
    know_biden =     ifelse(V162072 %in% -7:-6, NA, V162072),
    know_ryan =      ifelse(V162073b %in% -7:-6, NA, V162073b),
    know_merkel =    ifelse(V162074a %in% -7:-6, NA, V162074a),
    know_putin =     ifelse(V162075a %in% -7:-6, NA, V162075a),
    know_roberts =   ifelse(V162076a %in% -7:-6, NA, V162076a),
    know_unemploy =  ifelse(V162137 %in% -7:-6, NA, ifelse(V162137 == 2, 1, 0)),
    pol_know_scale = (know_senate + know_spend + know_house_maj + know_sen_maj + know_biden + know_ryan + know_merkel + know_putin + know_roberts + know_unemploy) / 10,
    
    # Raw Values for IRT (MIRT requires that variables are scaled in integers to recognize them as ordinal)
    know_ryan_raw =      ifelse(V162073b %in% -7:-6, NA, V162073b * 2),
    know_roberts_raw =      ifelse(V162076a %in% -7:-6, NA, V162076a * 2),
    
    # Placement Knowledge
    place_gov_jobs =   ifelse(V161190 %in% -9:-8 | V161191 %in% -9:-8, 0, ifelse(V161190 < V161191, 1, 0)),
    place_gov_insur =  ifelse(V161185 %in% -9:-8 | V161186 %in% -9:-8, 0, ifelse(V161185 < V161186, 1, 0)),
    place_aid_black =  ifelse(V161199 %in% -9:-8 | V161200 %in% -9:-8, 0, ifelse(V161199 < V161200, 1, 0)),
    place_def_spend =  ifelse(V161182 %in% -9:-8 | V161183 %in% -9:-8, 0, ifelse(V161182 < V161183, 1, 0)),
    place_abortion =   ifelse(V162181 %in% -7:-6 | V162182 %in% -7:-6, NA, 
                              ifelse(V162181 %in% -9:-8 | V162182 %in% -9:-8, 0, ifelse(V162181 > V162182, 1, 0))),
    place_enviro =     ifelse(V161202 %in% -9:-8 | V161203 %in% -9:-8, 0, ifelse(V161202 < V161203, 1, 0)),
    place_spend_serv = ifelse(V161179 %in% -9:-8 | V161180 %in% -9:-8, 0, ifelse(V161179 > V161180, 1, 0)),
    place_lib_con =    ifelse(V161128 %in% -9:-8 | V161129 %in% -9:-8, 0, ifelse(V161128 < V161129, 1, 0)),
    place_know_scale = (place_gov_jobs + place_gov_insur + place_aid_black + place_def_spend + place_abortion + place_enviro + place_spend_serv + place_lib_con) / 8,
    
    # Political Interest/Attention
    politics_interest = ifelse(V162256 %in% 1:4, (4 - V162256) / 3, NA),
    campaign_interest = (3 - V161004) / 2,
    attention_politics = (5 - V161003) / 4,
    pol_interest_scale = ifelse(is.na(politics_interest), (campaign_interest + attention_politics) / 2, (politics_interest + campaign_interest + attention_politics) / 3), 
    
    # Raw Values for IRT
    politics_interest_raw = ifelse(V162256 %in% 1:4, 4 - V162256, NA),
    campaign_interest_raw = 3 - V161004,
    attention_politics_raw = 5 - V161003,
    
    # Attention to Politics in Media
    attention_pol_news = ifelse(V161009 == -1, 0, ifelse(V161009 %in% 1:5, (5 - V161009) / 4, NA)),
    follow_politics_media = ifelse(V162257 %in% 1:4, (4 - V162257) / 3, NA),
    att_pol_media_scale = (attention_pol_news + follow_politics_media) / 2,
    
    # Raw Values for IRT
    attention_pol_news_raw = ifelse(V161009 == -1, 0, ifelse(V161009 %in% 1:5, 5 - V161009, NA)),
    follow_politics_media_raw = ifelse(V162257 %in% 1:4, 4 - V162257, NA),
    
    # Weekly Media Consumption
    media_week = ifelse(V161008 %in% 0:7, V161008 / 7, 0),
    
    # Raw Values for IRT
    media_week_raw = ifelse(V161008 %in% 0:7, V161008, 0),
    
    # Political Discussion
    discuss_week = ifelse(V162174 == 2, 0, ifelse(V162174a %in% 0:7, V162174a / 7, NA)),
    
    # Raw Values for IRT
    discuss_week_raw = ifelse(V162174 == 2, 0, ifelse(V162174a %in% 0:7, V162174a, NA)),
    
    # Media Sources 
    fox = ifelse(V161370 %in% c(-1, 0) & V161372 %in% c(-1, 0) & V161409 %in% c(-1, 0) & V161452 %in% c(-1, 0), 0,
                 ifelse(V161370 == 1 |    # Hannity
                          V161372 == 1 |  # The Kelly File
                          V161409 == 1 |  # The O'Reilly Factor
                          V161452 == 1,   # www.foxnews.com
                        1, NA)),
    msnbc = ifelse(V161365 %in% c(-1, 0) & V161386 %in% c(-1, 0) & V161393 %in% c(-1, 0), 0,
                   ifelse(V161365 == 1 |   # All In with Chris Hayes
                            V161386 == 1 | # Hardball with Chris Matthews
                            V161393 == 1,  # The Rachel Maddow Show
                          1, NA)),
    cable = ifelse(is.na(fox) & is.na(msnbc), NA, ifelse(fox == 1 | msnbc == 1, 1, 0)),
    papers = ifelse(V161451 %in% c(-1, 0) & V161469 %in% c(-1, 0) & V161482 %in% c(-1, 0) & V161454 %in% c(-1, 0) & V161472 %in% c(-1, 0) & V161485 %in% c(-1, 0) & V161471 %in% c(-1, 0) & V161484 %in% c(-1, 0), 0,
                    ifelse(V161451 == 1 |   # nytimes.com
                             V161469 == 1 | # The New York Times
                             V161482 == 1 | # www.nytimes.com
                             V161454 == 1 | # washingtonpost.com
                             V161472 == 1 | # The Washington Post
                             V161485 == 1 | # www.washingtonpost.com
                             V161471 == 1 | # The Wall Street Journal
                             V161484 == 1,  # online.wsj.com
                           1, NA)),
    talk_radio = ifelse(V161428 %in% c(-1, 0) & V161430 %in% c(-1, 0) & V161433 %in% c(-1, 0) & V161434 %in% c(-1, 0) & V161435 %in% c(-1, 0) & V161437 %in% c(-1, 0) & V161440 %in% c(-1, 0) & V161441 %in% c(-1, 0) & V161442 %in% c(-1, 0), 0,
                        ifelse(V161428 == 1 |   # Rush Limbaugh
                                 V161430 == 1 | # The Sean Hannity Show
                                 V161433 == 1 | # The Glenn Beck Program
                                 V161434 == 1 | # The Mark Levin Show
                                 V161435 == 1 | # The Savage Nation (Michael Savage)
                                 V161437 == 1 | # The Hugh Hewitt Show
                                 V161440 == 1 | # The Mike Gallagher Show
                                 V161441 == 1 | # The Bill Handel Show
                                 V161442 == 1,  # The Schnitt Show (Todd Schnitt)
                               1, 0)),
    npr = ifelse(V161429 %in% c(-1, 0) & V161431 %in% c(-1, 0) & V161432 %in% c(-1, 0) & V161436 %in% c(-1, 0), 0,
                 ifelse(V161429 == 1 |   # Morning Edition
                          V161431 == 1 | # All Things Considered 
                          V161432 == 1 | # Marketplace
                          V161436 == 1,  # Fresh Air
                        1, NA)),
    
    # Ideology
    lib_con =  ifelse(V161126 == 99, 0.5, ifelse(V161126 %in% 1:7, (V161126 - 1) / 6, NA)),
    ideo_ext = abs(lib_con - 0.5) * 2,
    
    # Partisanship
    party_id =     ifelse(V161158x %in% 1:7, (V161158x - 1) / 6, NA),
    partisan_ext = abs(party_id - 0.5) * 2,
    affect_dem =   ifelse(V161095 %in% -89:-88, 0.5, ifelse(V161095 %in% 0:100, V161095 / 100, NA)),
    affect_rep =   ifelse(V161096 %in% -89:-88, 0.5, ifelse(V161096 %in% 0:100, V161096 / 100, NA)),
    affect_polar = abs(affect_rep - affect_dem),
    
    # Social Policy
    abortion.16 =        ifelse(V161232 %in% 1:4, (4 - V161232) / 3, NA),
    gay_job_discrim.16 = ifelse(V161229x %in% 1:4, (V161229x - 1) / 3, NA), 
    gay_adoption.16 =    ifelse(V161230 %in% 1:2, V161230 - 1, NA), 
    gay_marriage.16 =    ifelse(V161231 %in% 1:3, (V161231 - 1) / 2, NA),
    deny_services.16 =   ifelse(V161227x %in% 1:6, (6 - V161227x) / 5, NA), 
    trans_bathroom.16 =  ifelse(V161228x %in% 1:6, (6 - V161228x) / 5, NA),
    marijuana.16 =       ifelse(V162179 == 3, 0.5, ifelse(V162179 %in% 1:2, V162179 - 1, NA)), # not in 2020
    gun_control.16 =     ifelse(V161187 == 3, 0.5, ifelse(V161187 %in% 1:2, V161187 - 1, NA)),
    death_penalty.16 =   ifelse(V161233x %in% 1:4, (4 - V161233x) / 3, NA),
    
    # Social Welfare Spending
    income_equality.16 = ifelse(V162276 %in% 1:5, (V162276 - 1) / 4, NA),
    guar_job_living.16 =  ifelse(V161189 == 99, 0.5, ifelse(V161189 %in% 1:7, (V161189 - 1) / 6, NA)),
    welfare_spend.16 =    ifelse(V161209 == 3, 0.5, ifelse(V161209 %in% 1:2, V161209 - 1, NA)),
    poor_spend.16 =       ifelse(V161211 == 3, 0.5, ifelse(V161211 %in% 1:2, V161211 - 1, NA)),
    school_spend.16 =     ifelse(V161206 == 3, 0.5, ifelse(V161206 %in% 1:2, V161206 - 1, NA)),
    gov_services.16 =     ifelse(V161178 == 99, 0.5, ifelse(V161178 %in% 1:7, (7 - V161178) / 6, NA)),
    soc_sec_spend.16 =    ifelse(V161205 == 3, 0.5, ifelse(V161205 %in% 1:2, V161205 - 1, NA)),
    healthcare_spend.16 = ifelse(V162193x %in% 1:7, (V162193x - 1) / 6, NA),
    wealthy_tax.16 =      ifelse(V162140 == 3, 0.5, ifelse(V162140 %in% 1:2, V162140 - 1, NA)),
    public_insurance.16 = ifelse(V161184 == 99, 0.5, ifelse(V161184 %in% 1:7, (V161184 - 1) / 6, NA)),
    insurance_law.16 =    ifelse(V161114x %in% 1:7, (V161114x - 1) / 6, NA),
    regulate_banks.16 =   ifelse(V162180x %in% 1:7, (V162180x - 1) / 6, NA), # not in 2020
    minimum_wage.16 =     ifelse(V162192 %in% 1:4, (V162192 - 1) / 3, NA), 
    parental_leave.16 =   ifelse(V161226x %in% 1:7, (V161226x - 1) / 6, NA), 
    deficit_imp.16 =      ifelse(V162139 %in% 1:5, (5 - V162139) / 4, NA),
    regulation.16 =       ifelse(V162186 %in% 1:5, (V162186 - 1) / 4, NA),
    
    # Immigration Policy
    immigration_levels.16 = ifelse(V162157 %in% 1:5, (V162157 - 1) / 4, NA),               
    illegal_immigrants.16 = ifelse(V161192 %in% 1:4, (4 - V161192) / 3, NA),
    end_birthright.16 =     ifelse(V161194x %in% 1:7, (7 - V161194x) / 6, NA),
    deport_children.16 =    ifelse(V161195x %in% 1:6, (6 - V161195x) / 5, NA), 
    build_wall.16 =         ifelse(V161196x %in% 1:7, (7 - V161196x) / 6, NA), 
    admit_refugees.16 =     ifelse(V161214x %in% 1:7, (V161214x - 1) / 6, NA), 
    
    # Racial Policy
    gov_assist_blacks.16 = ifelse(V161198 == 99, 0.5, ifelse(V161198 %in% 1:7, (V161198 - 1) / 6, NA)),
    aa_hire_promotion.16 = ifelse(V162238x %in% 1:5, (V162238x - 1) / 4, NA),
    aa_university.16 =     ifelse(V161204x %in% 1:7, (V161204x - 1) / 6, NA), # not in 2020
    
    # Environmental Policy
    enviro_regulate.16 = ifelse(V161201 == 99, 0.5, ifelse(V161201 %in% 1:7, (V161201 - 1) / 6, NA)),
    fed_rising_temp.16 = ifelse(V161225x %in% 1:7, (V161225x - 1) / 6, NA),
    spend_enviro.16 =    ifelse(V161212 == 3, 0.5, ifelse(V161212 %in% 1:2, V161212 - 1, NA)),
    fracking.16 =        ifelse(V161223 == 3, 0.5, ifelse(V161223 %in% 1:2, 2 - V161223, NA)), # not in 2020
    
    # Trade Policy
    free_trade.16 =    ifelse(V162176 %in% 1:7, (7 - V162176) / 6, NA),
    gov_outsource.16 = ifelse(V162177 == 3, 0.5, ifelse(V162177 %in% 1:2, V162177 - 1, NA)),
    limit_imports.16 = ifelse(V162152a %in% 1:2, V162152a - 1, 
                              ifelse(V162152b %in% 1:2, V162152b - 1,
                                     ifelse(V162152b == 99, 0.5, NA))),    
    
    # Miscellaneous
    defense_spend.16 = ifelse(V161181 == 99, 0.5, ifelse(V161181 %in% 1:7, (V161181 - 1) / 6, NA)),
    crime_spend.16 = ifelse(V161208 == 3, 0.5, ifelse(V161208 %in% 1:2, 2 - V161208, NA)),
    
    .keep = "none"
  )


### Prepare ANES 2020 Time Series

# Load Data
ANES20 <- as.data.frame(read_dta("ANES 2020.dta"))

ANES20 <-
  ANES20 %>% 
  mutate(
    
    # Respondent ID
    case_id = as.numeric(V160001_orig),
    
    # Social Policy
    abortion.20 =        ifelse(V201336 %in% 1:4, (4 - V201336) / 3, NA),
    gay_job_discrim.20 = ifelse(V201414x %in% 1:4, (V201414x - 1) / 3, NA), 
    gay_adoption.20 =    ifelse(V201415 %in% 1:2, V201415 - 1, NA), 
    gay_marriage.20 =    ifelse(V201416 %in% 1:3, (V201416 - 1) / 2, NA), 
    deny_services.20 =   ifelse(V201408x %in% 1:6, (6 - V201408x) / 5, NA),
    trans_bathroom.20 =  ifelse(V201411x %in% 1:6, (6 - V201411x) / 5, NA), 
    gun_control.20 =     ifelse(V202337 == 3, 0.5, ifelse(V202337 %in% 1:2, V202337 - 1, NA)),
    death_penalty.20 =   ifelse(V201345x %in% 1:4, (4 - V201345x) / 3, NA),
    
    # Social Welfare Spending
    income_equality.20 =  ifelse(V202426 %in% 1:5, (V202426 - 1) / 4, NA),
    guar_job_living.20 =  ifelse(V201255 == 99, 0.5, ifelse(V201255 %in% 1:7, (V201255 - 1) / 6, NA)),
    welfare_spend.20 =    ifelse(V201312 == 3, 0.5, ifelse(V201312 %in% 1:2, V201312 - 1, NA)),
    poor_spend.20 =       ifelse(V201318 == 3, 0.5, ifelse(V201318 %in% 1:2, V201318 - 1, NA)),
    school_spend.20 =     ifelse(V201303 == 3, 0.5, ifelse(V201303 %in% 1:2, V201303 - 1, NA)),
    parental_leave.20 =   ifelse(V201405x %in% 1:7, (V201405x - 1) / 6, NA),
    gov_services.20 =     ifelse(V201246 == 99, 0.5, ifelse(V201246 %in% 1:7, (7 - V201246) / 6, NA)),
    soc_sec_spend.20 =    ifelse(V201300 == 3, 0.5, ifelse(V201300 %in% 1:2, V201300 - 1, NA)),
    healthcare_spend.20 = ifelse(V202380x %in% 1:7, (V202380x - 1) / 6, NA),
    wealthy_tax.20 =      ifelse(V202325 == 3, 0.5, ifelse(V202325 %in% 1:2, (V202325 - 1) / 2, NA)),
    public_insurance.20 = ifelse(V201252 == 99, 0.5, ifelse(V201252 %in% 1:7, (V201252 - 1) / 6, NA)),
    insurance_law.20 =    ifelse(V202328x %in% 1:7, (V202328x - 1) / 6, NA), 
    minimum_wage.20 =     ifelse(V202377 %in% 1:4, (V202377 - 1) / 3, NA),
    regulation.20 =       ifelse(V202256 %in% 1:7, (V202256 - 1) / 6, NA),
    deficit_imp.20 =      ifelse(V202321 %in% 1:5, (5 - V202321) / 4, NA),
    
    # Immigration Policy
    immigration_levels.20 = ifelse(V202232 %in% 1:5, (V202232 - 1) / 4, NA),               
    illegal_immigrants.20 = ifelse(V201417 %in% 1:4, (4 - V201417) / 3, NA),
    end_birthright.20 =     ifelse(V201420x %in% 1:7, (7 - V201420x) / 6, NA),
    deport_children.20 =    ifelse(V201423x %in% 1:6, (6 - V201423x) / 5, NA),
    build_wall.20 =         ifelse(V201426x %in% 1:7, (7 - V201426x) / 6, NA),
    admit_refugees.20 =     ifelse(V202236x %in% 1:7, (V202236x - 1) / 6, NA),
    
    # Racial Policy
    aa_hire_promotion.20 = ifelse(V202252x %in% 1:2, (V202252x - 1) / 4, ifelse(V202252x %in% 3:4, V202252x / 4, NA)),
    gov_assist_blacks.20 = ifelse(V201258 == 99, 0.5, ifelse(V201258 %in% 1:7, (V201258 - 1) / 6, NA)),
    
    # Environmental Policy
    enviro_regulate.20 = ifelse(V201262 == 99, 0.5, ifelse(V201262 %in% 1:7, (V201262 - 1) / 6, NA)),
    fed_rising_temp.20 = ifelse(V201401 == 3, 0.5, ifelse(V201401 %in% 1:2, V201401 - 1, NA)),
    spend_enviro.20 =    ifelse(V201323x %in% 1:5, (V201323x - 1) / 4, NA),
    
    # Trade Policy
    free_trade.20 =    ifelse(V202361x %in% 1:7, (7 - V202361x) / 6, NA),
    limit_imports.20 = ifelse(V202229 %in% 1:2, V202229 - 1, NA), # Branch items not used to match item in 2016 Time Series
    
    # Miscellaneous
    defense_spend.20 = ifelse(V201249 == 99, 0.5, ifelse(V201249 %in% 1:7, (V201249 - 1) / 6, NA)),
    crime_spend.20 = ifelse(V201309 == 3, 0.5, ifelse(V201309 %in% 1:2, 2 - V201309, NA)),
    
    .keep = "none"
  )

# Merge 2016 ANES Time Series and 2020 ANES Time Series to Create 2016-2020 Panel
ANES16_20 <- merge(ANES16, ANES20, by = "case_id", all = T)

## Constraint
global16_20 <- ANES16_20[c("abortion.16", "gay_job_discrim.16", "gay_adoption.16", "gay_marriage.16", "deny_services.16", "trans_bathroom.16", "marijuana.16", "guar_job_living.16",
                           "welfare_spend.16", "poor_spend.16", "school_spend.16", "gov_services.16", "soc_sec_spend.16", "healthcare_spend.16", "wealthy_tax.16", "income_equality.16",
                           "regulate_banks.16", "minimum_wage.16", "deficit_imp.16", "regulation.16", "public_insurance.16", "insurance_law.16", "parental_leave.16",
                           "illegal_immigrants.16", "end_birthright.16", "deport_children.16", "build_wall.16", "admit_refugees.16", "immigration_levels.16", "aa_hire_promotion.16",
                           "gov_assist_blacks.16", "aa_university.16", "enviro_regulate.16", "fed_rising_temp.16", "spend_enviro.16", "fracking.16")]
social16_20 <- ANES16_20[c("abortion.16", "gay_job_discrim.16", "gay_adoption.16", "gay_marriage.16", "deny_services.16", "trans_bathroom.16")]
econ16_20 <- ANES16_20[c("welfare_spend.16", "poor_spend.16", "gov_services.16", "soc_sec_spend.16", "healthcare_spend.16", "guar_job_living.16", 
                         "wealthy_tax.16", "income_equality.16", "public_insurance.16", "insurance_law.16", "school_spend.16")]

ANES16_20$global_const <- constraint(global16_20, 10) # Allow 10% Missing Policies
ANES16_20$social_const <- constraint(social16_20, 10) # Allow 10% Missing Policies
ANES16_20$econ_const <- constraint(econ16_20, 10) # Allow 10% Missing Policies

## Stability
ANES16_20 <-
  ANES16_20 %>% 
  mutate(
    
    # Social Policy
    abortion_stblty =        revscale(sqrt(rowVars(as.matrix(data.frame(abortion.16, abortion.20))))),
    gay_job_discrim_stblty = revscale(sqrt(rowVars(as.matrix(data.frame(gay_job_discrim.16, gay_job_discrim.20))))),
    gay_adoption_stblty =    revscale(sqrt(rowVars(as.matrix(data.frame(gay_adoption.16, gay_adoption.20))))),
    gay_marriage_stblty =    revscale(sqrt(rowVars(as.matrix(data.frame(gay_marriage.16, gay_marriage.20))))),
    deny_services_stblty =   revscale(sqrt(rowVars(as.matrix(data.frame(deny_services.16, deny_services.20))))),
    trans_bathroom_stblty =  revscale(sqrt(rowVars(as.matrix(data.frame(trans_bathroom.16, trans_bathroom.20))))),
    death_penalty_stblty =   revscale(sqrt(rowVars(as.matrix(data.frame(death_penalty.16, death_penalty.20))))),
    gun_control_stblty =     revscale(sqrt(rowVars(as.matrix(data.frame(gun_control.16, gun_control.20))))),
    
    # Economic
    income_equality_stblty =  revscale(sqrt(rowVars(as.matrix(data.frame(income_equality.16, income_equality.20))))),
    guar_job_living_stblty =  revscale(sqrt(rowVars(as.matrix(data.frame(guar_job_living.16, guar_job_living.20))))),
    soc_sec_spend_stblty =    revscale(sqrt(rowVars(as.matrix(data.frame(soc_sec_spend.16, soc_sec_spend.20))))),
    school_spend_stblty =     revscale(sqrt(rowVars(as.matrix(data.frame(school_spend.16, school_spend.20))))),
    welfare_spend_stblty =    revscale(sqrt(rowVars(as.matrix(data.frame(welfare_spend.16, welfare_spend.20))))),
    poor_spend_stblty =       revscale(sqrt(rowVars(as.matrix(data.frame(poor_spend.16, poor_spend.20))))),
    gov_services_stblty =     revscale(sqrt(rowVars(as.matrix(data.frame(gov_services.16, gov_services.20))))),
    healthcare_spend_stblty = revscale(sqrt(rowVars(as.matrix(data.frame(healthcare_spend.16, healthcare_spend.20))))),
    wealthy_tax_stblty =      revscale(sqrt(rowVars(as.matrix(data.frame(wealthy_tax.16, wealthy_tax.20))))),
    public_insurance_stblty = revscale(sqrt(rowVars(as.matrix(data.frame(public_insurance.16, public_insurance.20))))),
    insurance_law_stblty =    revscale(sqrt(rowVars(as.matrix(data.frame(insurance_law.16, insurance_law.20))))),
    regulation_stblty =       revscale(sqrt(rowVars(as.matrix(data.frame(regulation.16, regulation.20))))),
    minimum_wage_stblty =     revscale(sqrt(rowVars(as.matrix(data.frame(minimum_wage.16, minimum_wage.20))))),
    parental_leave_stblty =   revscale(sqrt(rowVars(as.matrix(data.frame(parental_leave.16, parental_leave.20))))),
    deficit_imp_stblty =      revscale(sqrt(rowVars(as.matrix(data.frame(deficit_imp.16, deficit_imp.20))))),
    
    # Immigration Policy
    immigration_levels_stblty = revscale(sqrt(rowVars(as.matrix(data.frame(immigration_levels.16, immigration_levels.20))))),
    illegal_immigrants_stblty = revscale(sqrt(rowVars(as.matrix(data.frame(illegal_immigrants.16, illegal_immigrants.20))))),
    end_birthright_stblty =     revscale(sqrt(rowVars(as.matrix(data.frame(end_birthright.16, end_birthright.20))))),
    deport_children_stblty =    revscale(sqrt(rowVars(as.matrix(data.frame(deport_children.16, deport_children.20))))),
    build_wall_stblty =         revscale(sqrt(rowVars(as.matrix(data.frame(build_wall.16, build_wall.20))))),
    
    # Racial Policy
    aa_hire_promotion_stblty = revscale(sqrt(rowVars(as.matrix(data.frame(aa_hire_promotion.16, aa_hire_promotion.20))))),
    gov_assist_blacks_stblty = revscale(sqrt(rowVars(as.matrix(data.frame(gov_assist_blacks.16, gov_assist_blacks.20))))),
    
    # Environmental Policy
    enviro_regulate_stblty = revscale(sqrt(rowVars(as.matrix(data.frame(enviro_regulate.16, enviro_regulate.20))))),
    fed_rising_temp_stblty = revscale(sqrt(rowVars(as.matrix(data.frame(fed_rising_temp.16, fed_rising_temp.20))))),
    spend_enviro_stblty =    revscale(sqrt(rowVars(as.matrix(data.frame(spend_enviro.16, spend_enviro.20))))),
    
    # Trade Policy
    free_trade_stblty =    revscale(sqrt(rowVars(as.matrix(data.frame(free_trade.16, free_trade.20))))),
    limit_imports_stblty = revscale(sqrt(rowVars(as.matrix(data.frame(limit_imports.16, limit_imports.20))))),
    
    # Miscellaneous
    defense_spend_stblty = revscale(sqrt(rowVars(as.matrix(data.frame(defense_spend.16, defense_spend.20))))),
    crime_spend_stblty = revscale(sqrt(rowVars(as.matrix(data.frame(crime_spend.16, crime_spend.20)))))
  )

global_stblty16_20 <- ANES16_20 %>% dplyr::select(ends_with("stblty"))
social_stblty16_20 <- ANES16_20[c("abortion_stblty", "gay_job_discrim_stblty", "gay_adoption_stblty", "gay_marriage_stblty", "deny_services_stblty", "trans_bathroom_stblty")]
econ_stblty16_20 <- ANES16_20[c("soc_sec_spend_stblty", "welfare_spend_stblty", "poor_spend_stblty", "gov_services_stblty", "healthcare_spend_stblty", "wealthy_tax_stblty", 
                                "guar_job_living_stblty", "income_equality_stblty", "public_insurance_stblty", "insurance_law_stblty", "school_spend_stblty")]

ANES16_20$global_stblty <- avg_stblty(global_stblty16_20, 10) # Allow 10% Missing Policies
ANES16_20$social_stblty <- avg_stblty(social_stblty16_20, 10) # Allow 10% Missing Policies
ANES16_20$econ_stblty <- avg_stblty(econ_stblty16_20, 10) # Allow 10% Missing Policies




#========================================================================#
#                         Create Pooled Dataset                          #
#========================================================================#

ANES08_10$survey <- "2008-2010"
ANES12_13$survey <- "2012-2013"
ANES16_20$survey <- "2016-2020"

ANES_pooled <- bind_rows(ANES08_10, ANES12_13, ANES16_20)
ANES_pooled <- ANES_pooled[ANES_pooled$case_id != -1,]








#========================================================================#
# Figure 1: Models of Ability and Information Effects on Public Opinion  #
#========================================================================#

fig1a <- dagitty('dag {
  Information [pos="0,1"]
  Ability [pos="0,0"]
  "Information x Ability" [pos="1,1"]
  Attitudes [pos="2,1"]
  
  Information -> "Information x Ability"
  Ability -> "Information x Ability"
  "Information x Ability" -> Attitudes
  Ability -> Information
}')

coordinates(fig1a) <- list(
  x = c(Information = 0, Ability = 0, "Information x Ability" = 1, Attitudes = 2),
  y = c(Information = 1, Ability = 0, "Information x Ability" = 1, Attitudes = 1)
)

fig1b <- dagitty('dag {
  Information [pos="0,1"]
  Ability [pos="1,0"]
  Attitudes [pos="2,1"]
  
  Information -> Ability
  Ability -> Information
  Ability -> Attitudes
  Information -> Attitudes
}')

coordinates(fig1b) <- list(
  x = c(Information = 0, Ability = 1, Attitudes = 2),
  y = c(Information = 1, Ability = 0, Attitudes = 1)
)

fig1a_tidy <- as.data.frame(tidy_dagitty(fig1a)) %>%
  mutate(facet = "Current Paper")
fig1b_tidy <- as.data.frame(tidy_dagitty(fig1b)) %>%
  mutate(facet = "Delli Carpini & Keeter (1996)")

fig1.data <- bind_rows(fig1a_tidy, fig1b_tidy)

fig1 <- ggplot(fig1.data, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_point(size = 12, shape = 21, fill = "black", color = "black", stroke = 1) +
  geom_dag_text(aes(label = ifelse(name == "Information x Ability", "Information\nX Ability", name)), size = 1.5) +
  expand_limits(x = c(-0.5, 2.5), y = c(-0.25, 1.25)) +
  facet_grid(~facet, space = "free") +
  theme_bw() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "grey65"),
    strip.background = element_rect(fill = "grey65", color = "grey65"),
    strip.text = element_text(color = "white", face = "bold")
  )

# Save Figure 1
Cairo(1600, 800, file = "Figure 1.png", res = 300)

fig1

dev.off()





#========================================================================#
#   Figure 2: Scaling Information Consumption and Verbal Ability Items   #
#========================================================================#

### 2012
ivs_anes12_13 <- ANES12_13[c("word1", "word2", "word3", "word4", "word5", "word6", "word7", "word8", "word9", "word10",
                             "know_deficit", "know_medicare", "know_senate", "know_spend", "know_pres_times", "know_boehner", "know_biden", "know_cameron",
                             "know_roberts_raw", "know_house_maj", "know_sen_maj", "know_treasury", "know_unemploy", "know_scnd_place", "know_un_sec",
                             "place_gov_jobs", "place_gov_insur", "place_aid_black", "place_def_spend", "place_abortion", "place_enviro", "place_spend_serv", "place_lib_con",
                             "politics_interest_raw", "campaign_interest_raw", "attention_politics_raw",
                             "attention_tv_raw", "attention_radio_raw", "attention_internet_raw", "attention_newspaper_raw", 
                             "tv_week_raw", "radio_week_raw", "internet_week_raw", "newspaper_week_raw",
                             "discuss_week_raw")]

efa.fit.12 <- mirt(ivs_anes12_13, 2) # Extract two latent factors

### 2016
ivs_anes16_20 <- ANES16_20[c("word1", "word2", "word3", "word4", "word5", "word6", "word7", "word8", "word9", "word10",
                             "know_senate", "know_spend", "know_house_maj", "know_sen_maj", "know_biden", "know_ryan_raw", "know_merkel", "know_putin", "know_roberts_raw", "know_unemploy",
                             "place_gov_jobs", "place_gov_insur", "place_aid_black", "place_def_spend", "place_abortion", "place_enviro", "place_spend_serv", "place_lib_con",
                             "politics_interest_raw", "campaign_interest_raw", "attention_politics_raw",
                             "attention_pol_news_raw", "follow_politics_media_raw",
                             "media_week_raw",
                             "discuss_week_raw")]
ivs_anes16_20 <- ivs_anes16_20[rowSums(is.na(ivs_anes16_20)) != length(ivs_anes16_20),]

efa.fit.16 <- mirt(ivs_anes16_20, 2) # Extract two latent factors


### Assemble Figure 2

fig2.data.12 <- cbind(data.frame(item = rownames(summary(efa.fit.12, rotate = "varimax")[["rotF"]])),
                      data.frame(summary(efa.fit.12, rotate = "varimax")[["rotF"]], row.names = NULL))

fig2.data.12$var_type <- c(rep("Verbal Ability", 10),
                           rep("General Political Knowledge", 15),
                           rep("Candidate-Issue Placement", 8),
                           rep("Political Interest", 3),
                           rep("Attention Politics Media", 4),
                           rep("Media Consumption Freq.", 4),
                           "Political Discussion Freq.")

fig2.data.12$year <- rep("2012 ANES", nrow(fig2.data.12))

fig2.data.16 <- cbind(data.frame(item = rownames(summary(efa.fit.16, rotate = "varimax")[["rotF"]])),
                      data.frame(summary(efa.fit.16, rotate = "varimax")[["rotF"]], row.names = NULL))

fig2.data.16$var_type <- c(rep("Verbal Ability", 10),
                           rep("General Political Knowledge", 10),
                           rep("Candidate-Issue Placement", 8),
                           rep("Political Interest", 3),
                           rep("Attention Politics Media", 2),
                           "Media Consumption Freq.",
                           "Political Discussion Freq.")

fig2.data.16$year <- rep("2016 ANES", nrow(fig2.data.16))


fig2.data <- rbind(fig2.data.12, fig2.data.16)

fig2.data$var_type <- factor(fig2.data$var_type, ordered = T, levels = c("Political Interest",
                                                                         "Attention Politics Media",
                                                                         "Media Consumption Freq.",
                                                                         "Political Discussion Freq.",
                                                                         "Candidate-Issue Placement",
                                                                         "General Political Knowledge",
                                                                         "Verbal Ability"))

fig2.data$item_label <- ifelse(fig2.data$item == "attention_pol_news_raw", "Attention to news about national politics \non TV, radio, newspapers, or internet",
                               ifelse(fig2.data$item == "attention_tv_raw", "Attention to news about \nnational politics on TV", NA))

fig2 <-
  ggplot(fig2.data) +
  facet_grid(~year, scales = "free_x") +
  geom_point(aes(F2, F1, shape = var_type)) +
  geom_text_repel(aes(F2, F1, label = item_label),
                  hjust = 0,
                  vjust = 0,
                  xlim = c(0.3, Inf),
                  ylim = c(-Inf, .925),
                  direction = "both",
                  size = 2.75,
                  segment.size = .25) +
  scale_shape_manual(values = c(19, 17, 15, 18, 1, 3, 8)) +
  scale_color_manual(values = c("black", "grey")) +
  scale_x_continuous(name='Factor I',
                     breaks = c(-.25, .0, .25, .5, .75, 1),
                     labels = c("-0.25", "0.00", "0.25", "0.50", "0.75", "1.00")) +
  scale_y_continuous(name='Factor II',
                     breaks = c(-.25, .0, .25, .5, .75, 1),
                     labels = c("-0.25", "0.00", "0.25", "0.50", "0.75", "1.00")) +
  theme_test(base_size = 14) +
  labs(shape = NULL) +
  theme(
    axis.ticks = element_line(color = "grey65"),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "grey65"),
    strip.background.x = element_rect(fill = "grey65", color = "grey65"),
    strip.background.y = element_blank(),
    strip.text.x.top = element_text(color = "white")
    )

# Save Figure 2
Cairo(3675, 1600, file = "Figure 2.png", res = 400)

fig2

dev.off()







#========================================================================#
#     Table 2. Ability moderates relationship btwn info and attitudes    #
#========================================================================#

dvs <- c("global_const", "global_stblty")
ivs <- c("pol_interest_scale", "att_pol_media_scale", "media_week", "discuss_week", "place_know_scale", "pol_know_scale")
datasets <- list("2008-2010" = ANES08_10, "2012-2013" = ANES12_13, "2016-2020" = ANES16_20)

tab2.data <- list()

# Individual Panels
for (dataset_name in names(datasets)) {
  for (iv in ivs) {
    for (dv in dvs) {
      dataset <- datasets[[dataset_name]]
      formula <- as.formula(paste(dv, "~ age + male + race + education + income + wordsum_scale *", iv))
      # formula <- as.formula(paste(dv, "~ (age + male + race + education + income + wordsum_scale) *", iv)) # Fully Interacted Model
      model <- lm(formula, data = dataset)
      tidy_model <- tidy(model) %>%
        filter(term == paste0("wordsum_scale:", iv)) %>%
        mutate(dataset = dataset_name, dv = dv, iv = iv)
      tab2.data[[paste(dataset_name, dv, iv, sep = "_")]] <- tidy_model
    }
  }
}

# Pooled Panels
for (iv in ivs) {
  for (dv in dvs) {
    formula <- as.formula(paste(dv, "~ age + male + race + education + income + wordsum_scale *", iv, "+ (1 | survey)"))
    # formula <- as.formula(paste(dv, "~ (age + male + race + education + income + wordsum_scale) *", iv, "+ (1 | survey)")) # Fully Interacted Model
    model <- lmer(formula, data = ANES_pooled)
    tidy_model <- tidy(model, effects = "fixed") %>%
      filter(term == paste0("wordsum_scale:", iv)) %>%
      mutate(dataset = "Pooled", dv = dv, iv = iv)
    tab2.data[[paste("Pooled", dv, iv, sep = "_")]] <- tidy_model
  }
}

# Assemble Table 2
tab2 <- bind_rows(tab2.data) %>%
  select(dv, iv, dataset, estimate, std.error, p.value) %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    value = ifelse(p.value < 0.05, paste0("**", estimate, " (", std.error, ")**"), paste0(estimate, " (", std.error, ")"))
  ) %>%
  select(-estimate, -std.error, -p.value) %>%
  pivot_wider(names_from = c(dv, dataset), values_from = value) %>%
  arrange(factor(dv, levels = dvs))

tab2 <- tab2 %>%
  mutate(iv = recode(iv,
                     "pol_interest_scale" = "Political Interest",
                     "att_pol_media_scale" = "Attention Pol Media",
                     "media_week" = "News Frequency",
                     "discuss_week" = "Discussion Frequency",
                     "place_know_scale" = "Issue Placement",
                     "pol_know_scale" = "Political Knowledge"
  ))

column_order <- c(
  "iv",
  "global_const_2008-2010", "global_const_2012-2013", "global_const_2016-2020", "global_const_Pooled",
  "global_stblty_2008-2010", "global_stblty_2012-2013", "global_stblty_2016-2020", "global_stblty_Pooled"
)

tab2 <- tab2 %>%
  relocate(all_of(column_order))

tab2 <- tab2 %>%
  kable(
    col.names = c(
      "Ability x",
      "2008-2010 (Const)", "2012-2013 (Const)", "2016-2020 (Const)", "Pooled (Const)",
      "2008-2010 (Stblty)", "2012-2013 (Stblty)", "2016-2020 (Stblty)", "Pooled (Stblty)"
    ),
    caption = "Table 2. Verbal ability moderates the relationship between information consumption and attitudes",
    format = "pipe"
  )

# Print Table 2
tab2



#========================================================================#
#  Table 3. Effects of Information Proxies by Verbal Ability Percentile  #
#========================================================================#

dvs <- c("global_const", "global_stblty")
ivs <- c("pol_interest_scale", "att_pol_media_scale", "media_week", "discuss_week", "place_know_scale", "pol_know_scale")

ANES_pooled$wordsum_scale_95 <- ANES_pooled$wordsum_scale - quantile(ANES_pooled$wordsum_scale, 0.95, na.rm = T)
ANES_pooled$wordsum_scale_5 <- ANES_pooled$wordsum_scale - quantile(ANES_pooled$wordsum_scale, 0.05, na.rm = T)

tab3.data <- list()

for (dv in dvs) {
  for (iv in ivs) {
    for (percentile in c("95th %", "5th %")) {
      scale_var <- ifelse(percentile == "95th %", "wordsum_scale_95", "wordsum_scale_5")
      formula <- as.formula(paste(dv, "~ age + male + race + education + income +", scale_var, "*", iv, "+ (1 | survey)"))
      model <- lmer(formula, data = ANES_pooled)
      tidy_model <- tidy(model, effects = "fixed") %>%
        filter(term == iv) %>%
        mutate(dv = dv, iv = iv, percentile = percentile)
      tab3.data[[paste(dv, iv, percentile, sep = "_")]] <- tidy_model
    }
  }
}

# Assemble Table 3
tab3 <- bind_rows(tab3.data) %>%
  select(dv, iv, percentile, estimate, std.error, p.value) %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    value = ifelse(p.value < 0.05, paste0("**", estimate, " (", std.error, ")**"), paste0(estimate, " (", std.error, ")"))
  ) %>%
  select(-estimate, -std.error, -p.value) %>%
  pivot_wider(names_from = c(dv, percentile), values_from = value) %>%
  arrange(factor(iv, levels = ivs))

tab3 <- tab3 %>%
  mutate(iv = recode(iv,
                     "pol_interest_scale" = "Political Interest",
                     "att_pol_media_scale" = "Attention Pol Media",
                     "media_week" = "News Frequency",
                     "discuss_week" = "Discussion Frequency",
                     "place_know_scale" = "Issue Placement",
                     "pol_know_scale" = "Political Knowledge"
  ))

column_order <- c(
  "iv",
  "global_const_5th %", "global_const_95th %",
  "global_stblty_5th %", "global_stblty_95th %"
)

tab3 <- tab3 %>%
  relocate(all_of(column_order))

tab3 <- tab3 %>%
  kable(
    col.names = c(
      "Ability x",
      "5th Percentile (Constraint)", "95th Percentile (Constraint)",
      "5th Percentile (Stability)", "95th Percentile (Stability)"
    ),
    caption = "Table 3. Effects of Information Proxies by Verbal Ability Percentile (Pooled Models)",
    format = "pipe"
  )

# Print Table 3
tab3







#========================================================================#
#  Figure 3. Effects of Information Proxies by Verbal Ability Percentile #
#========================================================================#

dvs <- c("global_const", "global_stblty")
ivs <- c("pol_interest_scale", "att_pol_media_scale", "media_week", "discuss_week", "place_know_scale", "pol_know_scale")
samples <- list("2008-2010" = ANES08_10, "2012-2013" = ANES12_13, "2016-2020" = ANES16_20)

fig3.data.list <- list()

for(dv in dvs) {
  for (iv in ivs) {
    formula <- as.formula(paste(dv, "~ age + male + race + education + income + wordsum_scale *", iv, "+ (1 | survey)"))
    model <- lmer(formula, data = ANES_pooled)
    beta.hat <- summary(model)$coefficients[,1]
    cov <- model@vcov_beta
    z0 <- seq(min(ANES_pooled$wordsum_scale, na.rm = T), max(ANES_pooled$wordsum_scale, na.rm = T), length.out = 1000)
    dy.dx <- beta.hat[iv] + beta.hat[paste0("wordsum_scale:", iv)]*z0
    se.dy.dx <- sqrt(cov[iv, iv] + z0^2*cov[paste0("wordsum_scale:", iv), paste0("wordsum_scale:", iv)] + 2*z0*cov[iv, paste0("wordsum_scale:", iv)])
    margeff <- data.frame(z0, dy.dx, se.dy.dx)
    margeff$upr <- dy.dx + 1.96*se.dy.dx
    margeff$lwr <- dy.dx - 1.96*se.dy.dx
    margeff$dv <- dv
    margeff$iv <- iv
    fig3.data.list[[paste(dv, iv, sep = "_")]] <- margeff
  }
}

fig3.data <- do.call(rbind, fig3.data.list)

fig3.data$iv <- factor(fig3.data$iv, levels = c("pol_interest_scale", "att_pol_media_scale", "media_week", "discuss_week", "place_know_scale", "pol_know_scale"))
levels(fig3.data$iv) <- c("IV: Pol Interest",
                          "IV: Attention Media",
                          "IV: News Freq",
                          "IV: Discuss Freq",
                          "IV: Issue Placement",
                          "IV: Pol Knowledge")
fig3.data$dv <- factor(fig3.data$dv, levels = c("global_const", "global_stblty"))
levels(fig3.data$dv) <- c("DV: Constraint",
                          "DV: Stability")

fig3 <-
  fig3.data %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "ff", linewidth = 0.4, color = "grey65") +
  facet_nested_wrap(dv ~ iv, ncol = 6, scales = "free_y", remove_labels = "y") +
  geom_line(aes(x = z0, y = dy.dx), linewidth = 0.3) +
  geom_ribbon(aes(x = z0, ymin = lwr, ymax = upr), alpha = 0.2) +
  scale_x_continuous(name = "Verbal Ability",
                     breaks = c(0, 1),
                     labels = c("   Min.", "Max.   "),
                     limits = c(0, 1)) +
  scale_y_facet(dv == "DV: Constraint" & iv != "IV: Pol Interest",
                breaks = NULL,
                labels = NULL,
                limits = c(-0.16, .2)) +
  scale_y_facet(dv == "DV: Constraint" & iv == "IV: Pol Interest",
                breaks = c(-0.1, 0.0, 0.1, 0.2),
                labels = c("-0.1", "0.0", "0.1", "0.2"),
                limits = c(-0.16, .2)) +
  scale_y_facet(dv == "DV: Stability" & iv != "IV: Pol Interest",
                breaks = NULL,
                labels = NULL,
                limits = c(-0.115, .195)) +
  scale_y_facet(dv == "DV: Stability" & iv == "IV: Pol Interest",
                breaks = c(-0.1, 0.0, 0.1, 0.2),
                labels = c("-0.1", "0.0", "0.1", "0.2"),
                limits = c(-0.115, .195)) +
  ylab("Marginal Effect of IV") +
  theme_bw() +
  theme(
    axis.ticks = element_line(color = "grey65"),
    panel.border = element_rect(color = "grey65"),
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "grey65", color = "grey65"),
    strip.text.x.top = element_text(angle = 0, color = "white")
  )

fig3.gt <- ggplotGrob(fig3)

is_strip <- which(grepl("strip", fig3.gt$layout$name))
strip_row <- unique(fig3.gt$layout$t[is_strip])

fig3.gt$heights[strip_row] <- fig3.gt$heights[strip_row] + unit(0.2, "cm")

fig3.gt$grobs[is_strip] <- 
  lapply(
    fig3.gt$grobs[is_strip],
    function(strip) {
      gtable::gtable_add_row_space(strip, unit(0.5, "lines"))
    })

Cairo(6000, 4200, file = "Figure 3.png", res = 775)

grid.newpage(); grid.draw(fig3.gt)

dev.off()






#========================================================================#
#         Table 4. Domain-Specific Constraint and Stability Items        #
#========================================================================#

tab4.data <- 
  ANES16_20 %>%
  dplyr::select(ends_with("stblty") & !starts_with(c("global_stblty", "social_stblty", "econ_stblty")), "case_id")

tab4.data <- tab4.data[!(tab4.data$case_id == -1) & rowSums(!is.na(tab4.data)) > 1, -38]

tab4.data <- melt(tab4.data, value.name = "stability")

tab4.data <- tab4.data[!is.na(tab4.data$stability),]

tab4.data <- tab4.data %>%
  group_by(variable) %>% 
  mutate(
    mean = round(mean(stability), 2),
    sd = round(sd(stability), 2)
  )

tab4.data <- tab4.data %>% 
  mutate(
    variable =
      case_when(
        variable == "gay_adoption_stblty" ~       "Same-Sex Adoption", 
        variable == "gay_marriage_stblty" ~       "Same-Sex Marriage", 
        variable == "gay_job_discrim_stblty" ~    "Same-Sex Hiring Discrimination", 
        variable == "deny_services_stblty" ~      "Deny Services to Gays", 
        variable == "trans_bathroom_stblty" ~     "Transgender Bathroom", 
        variable == "abortion_stblty" ~           "Abortion", 
        variable == "minimum_wage_stblty" ~       "Minimum Wage", 
        variable == "school_spend_stblty" ~       "School Spending", 
        variable == "wealthy_tax_stblty" ~        "Taxes on the Rich", 
        variable == "soc_sec_spend_stblty" ~      "Social Security Spending", 
        variable == "guar_job_living_stblty" ~    "Guaranteed Jobs & Standard of Living", 
        variable == "public_insurance_stblty" ~   "Public Health Insurance", 
        variable == "insurance_law_stblty" ~      "Health Insurance Mandate", 
        variable == "poor_spend_stblty" ~         "Spending on the Poor", 
        variable == "welfare_spend_stblty" ~      "Welfare Spending", 
        variable == "income_equality_stblty" ~    "Government Equalize Incomes", 
        variable == "gov_services_stblty" ~       "Government Services Spending", 
        variable == "healthcare_spend_stblty" ~   "Healthcare Spending"
        ),
    domain = ifelse(variable %in% c("Same-Sex Adoption", "Same-Sex Marriage", "Same-Sex Hiring Discrimination", "Deny Services to Gays", "Transgender Bathroom", "Abortion"), 
                    "Social Policy", "Size of Government")
    )

tab4.data <- unique(tab4.data[!is.na(tab4.data$variable), -2])

tab4.data <- tab4.data %>% 
  group_by(domain, variable) %>% 
  summarize(
    Mean = first(mean),
    SD = first(sd),
    .groups = "drop"
  )

tab4.data






#========================================================================#
#  Figure 4. The Moderating Effect of Ability Differs by Policy Domain   #
#========================================================================#

dvs <- c("social_const", "social_stblty", "econ_const", "econ_stblty")
ivs <- c("pol_interest_scale", "att_pol_media_scale", "media_week", "discuss_week", "place_know_scale", "pol_know_scale")

fig4.data.list <- list()

for(dv in dvs) {
  for (iv in ivs) {
    formula <- as.formula(paste(dv, "~ age + male + race + education + income + wordsum_scale *", iv))
    model <- lm(formula, data = ANES16_20)
    beta.hat <- coef(model) 
    cov <- vcov(model)
    z0 <- seq(min(ANES16_20$wordsum_scale, na.rm = T), max(ANES16_20$wordsum_scale, na.rm = T), length.out = 1000)
    dy.dx <- beta.hat[iv] + beta.hat[paste0("wordsum_scale:", iv)]*z0
    se.dy.dx <- sqrt(cov[iv, iv] + z0^2*cov[paste0("wordsum_scale:", iv), paste0("wordsum_scale:", iv)] + 2*z0*cov[iv, paste0("wordsum_scale:", iv)])
    margeff <- data.frame(z0, dy.dx, se.dy.dx)
    margeff$upr <- dy.dx + 1.96*se.dy.dx
    margeff$lwr <- dy.dx - 1.96*se.dy.dx
    margeff$dv <- dv
    margeff$iv <- iv
    fig4.data.list[[paste(dv, iv, sep = "_")]] <- margeff
  }
}

fig4.data <- do.call(rbind, fig4.data.list)

fig4.data$iv <- factor(fig4.data$iv, levels = c("pol_interest_scale", "att_pol_media_scale", "media_week", "discuss_week", "place_know_scale", "pol_know_scale"))
levels(fig4.data$iv) <- c("IV: Pol Interest",
                          "IV: Attention Media",
                          "IV: News Freq",
                          "IV: Discuss Freq",
                          "IV: Issue Placement",
                          "IV: Pol Knowledge")
fig4.data$dv <- factor(fig4.data$dv, levels = c("social_const", "social_stblty", "econ_const", "econ_stblty"))
fig4.data$domain <- factor(case_when(fig4.data$dv %in% c("social_const", "social_stblty") ~ "Social Policy",
                                     fig4.data$dv %in% c("econ_const", "econ_stblty") ~ "Size of Government"),
                           levels = c("Social Policy", "Size of Government"))
levels(fig4.data$dv) <- c("DV: Constraint",
                          "DV: Stability",
                          "DV: Constraint",
                          "DV: Stability")

fig4 <-
  ggplot() +  
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4, color = "grey65") +
  geom_ribbon(data = fig4.data %>% filter(domain == "Social Policy"), 
              aes(x = z0, y = dy.dx, ymin = lwr, ymax = upr, fill = domain), alpha = 0.6) +
  geom_line(data = fig4.data %>% filter(domain == "Social Policy"), 
            aes(x = z0, y = dy.dx, linetype = domain), size = 0.3) +
  geom_ribbon(data = fig4.data %>% filter(domain == "Size of Government"), 
              aes(x = z0, y = dy.dx, ymin = lwr, ymax = upr, fill = domain), alpha = 0.6) +
  geom_line(data = fig4.data %>% filter(domain == "Size of Government"), 
            aes(x = z0, y = dy.dx, linetype = domain), size = 0.3) +
  guides(linetype = guide_legend(title = "Policy Domain"),
         fill = guide_legend(title = "Policy Domain")) +
  scale_linetype_manual(name = "Policy Domain",
                        labels = c("Social Policy", "Size of Government"),
                        values = c("solid", "ff")) +
  scale_fill_manual(name = "Policy Domain",
                    labels = c("Social Policy", "Size of Government"),
                    values = c("#F8766D", "#00BFC4")) +
  facet_nested_wrap(dv ~ iv, ncol = 6, scales = "free_y", remove_labels = "y") +
  scale_x_continuous(name = "Verbal Ability",
                     breaks = c(0, 1),
                     labels = c("   Min.", "Max.   "),
                     limits = c(0, 1)) +
  scale_y_facet(dv == "DV: Constraint" & iv != "IV: Pol Interest",
                breaks = NULL,
                labels = NULL,
                limits = c(-0.22, .235)) +
  scale_y_facet(dv == "DV: Constraint" & iv == "IV: Pol Interest",
                breaks = c(-0.2, -0.1, 0.0, 0.1, 0.2),
                labels = c("-0.2", "-0.1", "0.0", "0.1", "0.2"),
                limits = c(-0.22, .235)) +
  scale_y_facet(dv == "DV: Stability" & iv != "IV: Pol Interest",
                breaks = NULL,
                labels = NULL,
                limits = c(-0.15, .22)) +
  scale_y_facet(dv == "DV: Stability" & iv == "IV: Pol Interest",
                breaks = c(-0.1, 0.0, 0.1, 0.2),
                labels = c("-0.1", "0.0", "0.1", "0.2"),
                limits = c(-0.15, .22)) +
  ylab("Marginal Effect of IV") +
  theme_bw() +
  theme(
    axis.ticks = element_line(color = "grey65"),
    legend.position = "bottom",
    panel.border = element_rect(color = "grey65"),
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "grey65", color = "grey65"),
    strip.text.x.top = element_text(angle = 0, color = "white")
  )

fig4.gt <- ggplotGrob(fig4)

is_strip <- which(grepl("strip", fig4.gt$layout$name))
strip_row <- unique(fig4.gt$layout$t[is_strip])

fig4.gt$heights[strip_row] <- fig4.gt$heights[strip_row] + unit(0.2, "cm")

fig4.gt$grobs[is_strip] <- 
  lapply(
    fig4.gt$grobs[is_strip],
    function(strip) {
      gtable::gtable_add_row_space(strip, unit(0.5, "lines"))
    })

Cairo(6000, 4300, file = "Figure 4.png", res = 775)

grid.newpage(); grid.draw(fig4.gt)

dev.off()







#========================================================================#
#         Table 5. Domain-Specific Results in the 2016-2020 Panel        #
#========================================================================#

dvs <- c("social_const", "econ_const", "social_stblty", "econ_stblty")
ivs <- c("pol_interest_scale", "att_pol_media_scale", "media_week", "discuss_week", "place_know_scale", "pol_know_scale")

tab5.data <- list()

for(dv in dvs) {
  for (iv in ivs) {
    formula <- as.formula(paste(dv, "~ age + male + race + education + income + wordsum_scale *", iv))
    # formula <- as.formula(paste(dv, "~ (age + male + race + education + income + wordsum_scale) *", iv)) # Fully Interacted Model
    model <- lm(formula, data = ANES16_20)
    tidy_model <- tidy(model) %>%
      filter(term == paste0("wordsum_scale:", iv)) %>%
      mutate(dv = dv, iv = iv)
    tab5.data[[paste(dv, iv, sep = "_")]] <- tidy_model
  }
}

# Assemble Table 5
tab5 <- bind_rows(tab5.data) %>%
  select(dv, iv, estimate, std.error, p.value) %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    value = ifelse(p.value < 0.05, paste0("**", estimate, " (", std.error, ")**"), paste0(estimate, " (", std.error, ")"))
  ) %>%
  select(-estimate, -std.error, -p.value) %>%
  pivot_wider(names_from = dv, values_from = value) %>%
  arrange(factor(dv, levels = dvs))

tab5 <- tab5 %>%
  mutate(iv = recode(iv,
                     "pol_interest_scale" = "Political Interest",
                     "att_pol_media_scale" = "Attention Pol Media",
                     "media_week" = "News Frequency",
                     "discuss_week" = "Discussion Frequency",
                     "place_know_scale" = "Issue Placement",
                     "pol_know_scale" = "Political Knowledge"
  ))

column_order <- c(
  "iv",
  "social_const", "econ_const", "social_stblty", "econ_stblty"
)

tab5 <- tab5 %>%
  relocate(all_of(column_order))

tab5 <- tab5 %>%
  kable(
    col.names = c(
      "Ability x",
      "Social Policy (Const)", "Size of Government (Const)", 
      "Social Policy (Stblty)", "Size of Government (Stblty)"
    ),
    caption = "Table 5. Domain-Specific Results in the 2016-2020 Panel",
    format = "pipe"
  )

# Print Table 5
tab5





#========================================================================#
#   Table 6. Effects of Proxies by Ability Percentile and Policy Domain  #
#========================================================================#

ANES16_20 <- ANES16_20 %>%
  mutate(
    wordsum_scale_95 = wordsum_scale - quantile(wordsum_scale, 0.95, na.rm = T),
    wordsum_scale_5 = wordsum_scale - quantile(wordsum_scale, 0.05, na.rm = T)
  )

dvs <- c("social_const", "econ_const", "social_stblty", "econ_stblty")
ivs <- c("pol_interest_scale", "att_pol_media_scale", "media_week", "discuss_week", "place_know_scale", "pol_know_scale")

tab6.data <- list()

for (dv in dvs) {
  for (iv in ivs) {
    for (percentile in c("95th %", "5th %")) {
      scale_var <- ifelse(percentile == "95th %", "wordsum_scale_95", "wordsum_scale_5")
      formula <- as.formula(paste(dv, "~ age + male + race + education + income +", scale_var, "*", iv))
      # formula <- as.formula(paste(dv, "~ (age + male + race + education + income +", scale_var, ") *", iv)) # Fully Interacted Model
      model <- lm(formula, data = ANES16_20)
      tidy_model <- tidy(model, effects = "fixed") %>%
        filter(term == iv) %>%
        mutate(dv = dv, iv = iv, percentile = percentile)
      tab6.data[[paste(dv, iv, percentile, sep = "_")]] <- tidy_model
    }
  }
}

# Assemble Table 6
tab6 <- bind_rows(tab6.data) %>%
  select(dv, iv, percentile, estimate, std.error, p.value) %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    value = ifelse(p.value < 0.05, paste0("**", estimate, " (", std.error, ")**"), paste0(estimate, " (", std.error, ")"))
  ) %>%
  select(-estimate, -std.error, -p.value) %>%
  pivot_wider(names_from = c(dv, percentile), values_from = value) %>%
  arrange(factor(iv, levels = ivs))

tab6 <- tab6 %>%
  mutate(iv = recode(iv,
                     "pol_interest_scale" = "Political Interest",
                     "att_pol_media_scale" = "Attention Pol Media",
                     "media_week" = "News Frequency",
                     "discuss_week" = "Discussion Frequency",
                     "place_know_scale" = "Issue Placement",
                     "pol_know_scale" = "Political Knowledge"
  ))

column_order <- c(
  "iv",
  "social_const_5th %", "social_const_95th %",
  "econ_const_5th %", "econ_const_95th %",
  "social_stblty_5th %", "social_stblty_95th %",
  "econ_stblty_5th %", "econ_stblty_95th %"
)

tab6 <- tab6 %>%
  relocate(all_of(column_order))

tab6 <- tab6 %>%
  kable(
    col.names = c(
      "Ability x",
      "5th % (Social Const)", "95th % (Social Const)",
      "5th % (Econ Const)", "95th % (Econ Const)",
      "5th % (Social Stblty)", "95th % (Social Stblty)",
      "5th % (Econ Stblty)", "95th % (Econ Stblty)"
    ),
    caption = "Table 6. Marginal Effects of Information Proxies by Verbal Ability Percentile and Policy Domain in the 2016-2020 Panel",
    format = "pipe"
  )

# Print Table 6
tab6








#========================================================================#
#  Table 8. Controlling for Confounders Fails to Eliminate Interactions  #
#========================================================================#

dvs <- c("global_const", "global_stblty")
ivs <- c("pol_interest_scale", "att_pol_media_scale", "media_week", "discuss_week", "place_know_scale", "pol_know_scale")

# No Controls
nc_models <- list(
  "No Controls (2008-2010)" = function(dv, iv) lm(as.formula(paste(dv, "~ wordsum_scale *", iv)), data = ANES08_10),
  "No Controls (2012-2013)" = function(dv, iv) lm(as.formula(paste(dv, "~ wordsum_scale *", iv)), data = ANES12_13),
  "No Controls (2016-2020)" = function(dv, iv) lm(as.formula(paste(dv, "~ wordsum_scale *", iv)), data = ANES16_20)
)

# Controlling for Direct Effects
de_models <- list(
  "Direct Effects (2008-2010)" = function(dv, iv) lm(as.formula(paste(dv, "~ age + male + black + hispanic + other + bachelors + post_grad + income + party_id + lib_con + partisan_ext + ideo_ext + nte_scale + nfc_scale + wordsum_scale *", iv)), data = ANES08_10),
  "Direct Effects (2012-2013)" = function(dv, iv) lm(as.formula(paste(dv, "~ age + male + black + hispanic + asian + other + bachelors + post_grad + income + party_id + lib_con + partisan_ext + ideo_ext + papers + npr + talk_radio + fox + msnbc + nte_scale + nfa_scale + nfc_scale + wordsum_scale *", iv)), data = ANES12_13),
  "Direct Effects (2016-2020)" = function(dv, iv) lm(as.formula(paste(dv, "~ age + male + black + hispanic + asian + other + bachelors + post_grad + income + party_id + lib_con + partisan_ext + ideo_ext + papers + npr + talk_radio + fox + msnbc + nte_scale + wordsum_scale *", iv)), data = ANES16_20)
)

# Fully Interacted
fe_models <- list(
  "Fully Interacted (2008-2010)" = function(dv, iv) lm(as.formula(paste(dv, "~ (age + male + black + hispanic + other + bachelors + post_grad + income + party_id + lib_con + partisan_ext + ideo_ext + nte_scale + nfc_scale + wordsum_scale) *", iv)), data = ANES08_10),
  "Fully Interacted (2012-2013)" = function(dv, iv) lm(as.formula(paste(dv, "~ (age + male + black + hispanic + asian + other + bachelors + post_grad + income + party_id + lib_con + partisan_ext + ideo_ext + papers + npr + talk_radio + fox + msnbc + nte_scale + nfa_scale + nfc_scale + wordsum_scale) *", iv)), data = ANES12_13),
  "Fully Interacted (2016-2020)" = function(dv, iv) lm(as.formula(paste(dv, "~ (age + male + black + hispanic + asian + other + bachelors + post_grad + income + party_id + lib_con + partisan_ext + ideo_ext + papers + npr + talk_radio + fox + msnbc + nte_scale + wordsum_scale) *", iv)), data = ANES16_20)
)

mod_type <- list(nc_models, de_models, fe_models)

tab8.data <- list()

for(models in mod_type) {
  for (dv in dvs) {
    for (model_name in names(models)) {
      for (iv in ivs) {
        model <- models[[model_name]](dv, iv)
        tidy_model <- tidy(model) %>%
          filter(term == paste0("wordsum_scale:", iv)) %>%
          mutate(dv = dv, iv = iv, model_name = model_name)
        tab8.data[[paste(dv, iv, model_name, sep = "_")]] <- tidy_model
      }
    }
  }
}

# Assemble Table 8
tab8 <- bind_rows(tab8.data) %>%
  select(dv, iv, model_name, estimate, std.error, p.value) %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    value = ifelse(p.value < 0.05, paste0("**", estimate, " (", std.error, ")**"), paste0(estimate, " (", std.error, ")"))
  ) %>%
  select(-estimate, -std.error, -p.value) %>%
  pivot_wider(names_from = dv, values_from = value) %>%
  arrange(factor(dv, levels = dvs))

tab8 <- tab8 %>%
  mutate(iv = recode(iv,
                     "pol_interest_scale" = "Political Interest",
                     "att_pol_media_scale" = "Attention Pol Media",
                     "media_week" = "News Frequency",
                     "discuss_week" = "Discussion Frequency",
                     "place_know_scale" = "Issue Placement",
                     "pol_know_scale" = "Political Knowledge"
  ))

column_order <- c(
  "iv", "model_name", "global_const", "global_stblty"
)

tab8 <- tab8 %>%
  relocate(all_of(column_order))

tab8 <- tab8 %>%
  kable(
    col.names = c(
      "Ability x", "Model", "Constraint", "Stability"
    ),
    caption = "Table 8. Domain-Specific Results in the 2016-2020 Panel",
    format = "pipe"
  )

# Print Table 8
tab8







#========================================================================#
#         Table A1: Two Factor CFA of Wordsum and 16PF Reasoning         #
#========================================================================#

# Load Data
GSS87_Main <- read_dta("GSS 1987.dta", col_select = c("id", "worda", "wordb", "wordc", "wordd", "worde", "wordf", "wordg", "wordh", "wordi", "wordj"))
GSS87_Gibson <- read_sav("GSS 1987 Gibson.sav", col_select = c("ID1", "IQ1", "IQ2", "IQ3", "IQ4", "IQ5", "IQ6", "IQ7", "IQ8", "IQ9", "IQ10", "IQ11", "IQ12", "IQ13", "IQFIN", "IQTIME", "TIQ"))
colnames(GSS87_Gibson)[1] <- "id"

GSS87 <- merge(GSS87_Main, GSS87_Gibson, by = "id", all = T)
rm(GSS87_Main, GSS87_Gibson)

GSS87 <-
  GSS87 %>% 
  mutate(
    
    # Wordsum
    word1 =  suppressWarnings(as.numeric(car::recode(format_tagged_na(worda), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"))),
    word2 =  suppressWarnings(as.numeric(car::recode(format_tagged_na(wordb), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"))),
    word3 =  suppressWarnings(as.numeric(car::recode(format_tagged_na(wordc), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"))),
    word4 =  suppressWarnings(as.numeric(car::recode(format_tagged_na(wordd), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"))),
    word5 =  suppressWarnings(as.numeric(car::recode(format_tagged_na(worde), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"))),
    word6 =  suppressWarnings(as.numeric(car::recode(format_tagged_na(wordf), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"))),
    word7 =  suppressWarnings(as.numeric(car::recode(format_tagged_na(wordg), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"))),
    word8 =  suppressWarnings(as.numeric(car::recode(format_tagged_na(wordh), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"))),
    word9 =  suppressWarnings(as.numeric(car::recode(format_tagged_na(wordi), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"))),
    word10 = suppressWarnings(as.numeric(car::recode(format_tagged_na(wordj), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"))),
    wordsum_scale = (word1 + word2 + word3 + word4 + word5 + word6 + word7 + word8 + word9 + word10) / 10,
    
    # 16 FP Reasoning Ability
    reason1 = car::recode(IQ1, "1=0; 2=1; 3=0; 8=0; 9=0; NA=NA"), 
    reason2 = car::recode(IQ2, "1=0; 2=1; 3=0; 8=0; 9=0; NA=NA"), 
    reason3 = car::recode(IQ3, "1=0; 2=1; 3=0; 8=0; 9=0; NA=NA"), 
    reason4 = car::recode(IQ4, "1=0; 2=0; 3=1; 8=0; 9=0; NA=NA"), 
    reason5 = car::recode(IQ5, "1=0; 2=1; 3=0; 8=0; 9=0; NA=NA"), 
    reason6 = car::recode(IQ6, "1=0; 2=0; 3=1; 8=0; 9=0; NA=NA"), 
    reason7 = car::recode(IQ7, "1=1; 2=0; 3=0; 8=0; 9=0; NA=NA"), 
    reason8 = car::recode(IQ8, "1=0; 2=0; 3=1; 8=0; 9=0; NA=NA"), 
    reason9 = car::recode(IQ9, "1=0; 2=1; 3=0; 8=0; 9=0; NA=NA"), 
    reason10 = car::recode(IQ10, "1=1; 2=0; 3=0; 8=0; 9=0; NA=NA"), 
    reason11 = car::recode(IQ11, "1=0; 2=0; 3=1; 8=0; 9=0; NA=NA"), 
    reason12 = car::recode(IQ12, "1=1; 2=0; 3=0; 8=0; 9=0; NA=NA"), 
    reason13 = car::recode(IQ13, "1=1; 2=0; 3=0; 8=0; 9=0; NA=NA"), 
    reason_scale = (reason1 + reason2 + reason3 + reason4 + reason5 + reason6 + reason7 + reason8 + reason9 + reason10 + reason11 + reason12 + reason13) / 13,
    
    .keep = "none"
  )

# Correlation between additive scales
cor.test(GSS87$wordsum_scale, GSS87$reason_scale)

# Correlation between latent variables
gss87.cfa.mod <- '
  wordsum =~ word1 + word2 + word3 + word4 + word5 + word6 + word7 + word8 + word9 + word10
  reasoning =~ reason1 + reason2 + reason3 + reason4 + reason5 + reason6 + reason7 + reason8 + reason9 + reason10 + reason11 + reason12 + reason13
 '
gss87.cfa <- cfa(gss87.cfa.mod, data = GSS87, ordered = T, missing = "pairwise")

summary(gss87.cfa, fit.measures = T, standardized = T)

gss87.cfa.pars <- parameterEstimates(gss87.cfa, standardized = T)

gss87.cfa.pars$pvalue <- case_when(gss87.cfa.pars$pvalue >= 0.05 ~ " ",
                                   gss87.cfa.pars$pvalue < 0.05 & gss87.cfa.pars$pvalue > 0.01 ~ "*",
                                   gss87.cfa.pars$pvalue < 0.01 & gss87.cfa.pars$pvalue > 0.001 ~ "**",
                                   gss87.cfa.pars$pvalue < 0.001 ~ "***")





#========================================================================#
#       Table A2: Two Factor CFA of Wordsum and WAIS-R Similarities      #
#========================================================================#

# Load Data
GSS94 <- read_dta("GSS 1994.dta", col_select = c("worda", "wordb", "wordc", "wordd", "worde", "wordf", "wordg", "wordh", "wordi", "wordj",
                                                            "alike1", "alike2", "alike3", "alike4", "alike5", "alike6", "alike7", "alike8"))

GSS94 <-
  GSS94 %>% 
  mutate(
    
    # Wordsum [NA(n) = No Answer; NA(i) = Inapplicable]
    word1 =  car::recode(format_tagged_na(worda), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
    word2 =  car::recode(format_tagged_na(wordb), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
    word3 =  car::recode(format_tagged_na(wordc), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
    word4 =  car::recode(format_tagged_na(wordd), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
    word5 =  car::recode(format_tagged_na(worde), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
    word6 =  car::recode(format_tagged_na(wordf), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
    word7 =  car::recode(format_tagged_na(wordg), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
    word8 =  car::recode(format_tagged_na(wordh), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
    word9 =  car::recode(format_tagged_na(wordi), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
    word10 = car::recode(format_tagged_na(wordj), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
    wordsum_scale = (word1 + word2 + word3 + word4 + word5 + word6 + word7 + word8 + word9 + word10) / 10,
    
    # WAIS Similarities [NA(n) = No Answer; NA(d) = Don't Know]
    simil1 = car::recode(format_tagged_na(alike1), "'    0'=0; '    1'=1; '    2'=2; 'NA(n)'=0; 'NA(d)'=0") / 2, 
    simil2 = car::recode(format_tagged_na(alike2), "'    0'=0; '    1'=1; '    2'=2; 'NA(n)'=0; 'NA(d)'=0") / 2,
    simil3 = car::recode(format_tagged_na(alike3), "'    0'=0; '    1'=1; '    2'=2; 'NA(n)'=0; 'NA(d)'=0") / 2,
    simil4 = car::recode(format_tagged_na(alike4), "'    0'=0; '    1'=1; '    2'=2; 'NA(n)'=0; 'NA(d)'=0") / 2,
    simil5 = car::recode(format_tagged_na(alike5), "'    0'=0; '    1'=1; '    2'=2; 'NA(n)'=0; 'NA(d)'=0") / 2,
    simil6 = car::recode(format_tagged_na(alike6), "'    0'=0; '    1'=1; '    2'=2; 'NA(n)'=0; 'NA(d)'=0") / 2,
    simil7 = car::recode(format_tagged_na(alike7), "'    0'=0; '    1'=1; '    2'=2; 'NA(n)'=0; 'NA(d)'=0") / 2,
    simil8 = car::recode(format_tagged_na(alike8), "'    0'=0; '    1'=1; '    2'=2; 'NA(n)'=0; 'NA(d)'=0") / 2,
    simil_scale = (simil1 + simil2 + simil3 + simil4 + simil5 + simil6 + simil7 + simil8) / 8,
    
    .keep = "none"
  )

# Correlation between additive scales
cor.test(GSS94$wordsum_scale, GSS94$simil_scale)

# Correlation between latent variables
gss94.cfa.mod <- '
  wordsum =~ word1 + word2 + word3 + word4 + word5 + word6 + word7 + word8 + word9 + word10
  similarities =~ simil1 + simil2 + simil3 + simil4 + simil5 + simil6 + simil7 + simil8
 '
gss94.cfa <- cfa(gss94.cfa.mod, data = GSS94, ordered = T, missing = "pairwise")

summary(gss94.cfa, fit.measures = T, standardized = T)

gss94.cfa.pars <- parameterEstimates(gss94.cfa, standardized = T)

gss94.cfa.pars$pvalue <- case_when(gss94.cfa.pars$pvalue >= 0.05 ~ " ",
                                   gss94.cfa.pars$pvalue < 0.05 & gss94.cfa.pars$pvalue > 0.01 ~ "*",
                                   gss94.cfa.pars$pvalue < 0.01 & gss94.cfa.pars$pvalue > 0.001 ~ "**",
                                   gss94.cfa.pars$pvalue < 0.001 ~ "***")









#========================================================================#
#    Table B1: Bivariate DCSM of Verbal Ability and Newspaper Reading    #
#========================================================================#

# Load Data
GSS06_10 <- read_dta("GSS 06-10 Panel.dta",
                     col_select = c("news_1", "news_2", "news_3",
                                    "worda_1", "wordb_1", "wordc_1", "wordd_1", "worde_1", "wordf_1", "wordg_1", "wordh_1", "wordi_1", "wordj_1",
                                    "worda_2", "wordb_2", "wordc_2", 'wordd_2', "worde_2", "wordf_2", "wordg_2", "wordh_2", "wordi_2", "wordj_2",
                                    "worda_3", "wordb_3", 'wordc_3', "wordd_3", "worde_3", "wordf_3", "wordg_3", "wordh_3", "wordi_3", "wordj_3"))
GSS08_12 <- read_dta("GSS 08-12 Panel.dta",
                     col_select = c("news_1", "news_2", "news_3",
                                    "worda_1", "wordb_1", "wordc_1", "wordd_1", "worde_1", "wordf_1", "wordg_1", "wordh_1", "wordi_1", "wordj_1",
                                    "worda_2", "wordb_2", "wordc_2", 'wordd_2', "worde_2", "wordf_2", "wordg_2", "wordh_2", "wordi_2", "wordj_2",
                                    "worda_3", "wordb_3", 'wordc_3', "wordd_3", "worde_3", "wordf_3", "wordg_3", "wordh_3", "wordi_3", "wordj_3"))
GSS10_14 <- read_dta("GSS 10-14 Panel.dta",
                     col_select = c("news_1", "news_2", "news_3",
                                    "worda_1", "wordb_1", "wordc_1", "wordd_1", "worde_1", "wordf_1", "wordg_1", "wordh_1", "wordi_1", "wordj_1",
                                    "worda_2", "wordb_2", "wordc_2", 'wordd_2', "worde_2", "wordf_2", "wordg_2", "wordh_2", "wordi_2", "wordj_2",
                                    "worda_3", "wordb_3", 'wordc_3', "wordd_3", "worde_3", "wordf_3", "wordg_3", "wordh_3", "wordi_3", "wordj_3"))

suppressWarnings(GSS06_10 <-
                   GSS06_10 %>% 
                   mutate(
                     
                     # Newspaper Reading
                     news11 = ifelse(news_1 %in% 1:5, 5 - news_1, NA),
                     news21 = ifelse(news_2 %in% 1:5, 5 - news_2, NA),
                     news31 = ifelse(news_3 %in% 1:5, 5 - news_3, NA),
                     
                     # Verbal Ability
                     word11 = car::recode(format_tagged_na(worda_1), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word12 = car::recode(format_tagged_na(wordb_1), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word13 = car::recode(format_tagged_na(wordc_1), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word14 = car::recode(format_tagged_na(wordd_1), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word15 = car::recode(format_tagged_na(worde_1), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word16 = car::recode(format_tagged_na(wordf_1), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word17 = car::recode(format_tagged_na(wordg_1), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word18 = car::recode(format_tagged_na(wordh_1), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word19 = car::recode(format_tagged_na(wordi_1), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word110 = car::recode(format_tagged_na(wordj_1), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word21 = car::recode(format_tagged_na(worda_2), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word22 = car::recode(format_tagged_na(wordb_2), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word23 = car::recode(format_tagged_na(wordc_2), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word24 = car::recode(format_tagged_na(wordd_2), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word25 = car::recode(format_tagged_na(worde_2), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word26 = car::recode(format_tagged_na(wordf_2), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word27 = car::recode(format_tagged_na(wordg_2), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word28 = car::recode(format_tagged_na(wordh_2), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word29 = car::recode(format_tagged_na(wordi_2), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word210 = car::recode(format_tagged_na(wordj_2), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word31 = car::recode(format_tagged_na(worda_3), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word32 = car::recode(format_tagged_na(wordb_3), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word33 = car::recode(format_tagged_na(wordc_3), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word34 = car::recode(format_tagged_na(wordd_3), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word35 = car::recode(format_tagged_na(worde_3), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word36 = car::recode(format_tagged_na(wordf_3), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word37 = car::recode(format_tagged_na(wordg_3), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word38 = car::recode(format_tagged_na(wordh_3), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word39 = car::recode(format_tagged_na(wordi_3), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word310 = car::recode(format_tagged_na(wordj_3), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     
                     # Panel
                     panel08_12 = 0,
                     panel10_14 = 0,
                     sample = "2006-2010",
                     
                     .keep = "none"
                     
                   )
)

suppressWarnings(GSS08_12 <-
                   GSS08_12 %>% 
                   mutate(
                     
                     # Newspaper Reading
                     news11 = ifelse(news_1 %in% 1:5, 5 - news_1, NA),
                     news21 = ifelse(news_2 %in% 1:5, 5 - news_2, NA),
                     news31 = ifelse(news_3 %in% 1:5, 5 - news_3, NA),
                     
                     # Verbal Ability
                     word11 = car::recode(format_tagged_na(worda_1), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word12 = car::recode(format_tagged_na(wordb_1), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word13 = car::recode(format_tagged_na(wordc_1), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word14 = car::recode(format_tagged_na(wordd_1), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word15 = car::recode(format_tagged_na(worde_1), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word16 = car::recode(format_tagged_na(wordf_1), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word17 = car::recode(format_tagged_na(wordg_1), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word18 = car::recode(format_tagged_na(wordh_1), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word19 = car::recode(format_tagged_na(wordi_1), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word110 = car::recode(format_tagged_na(wordj_1), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word21 = car::recode(format_tagged_na(worda_2), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word22 = car::recode(format_tagged_na(wordb_2), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word23 = car::recode(format_tagged_na(wordc_2), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word24 = car::recode(format_tagged_na(wordd_2), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word25 = car::recode(format_tagged_na(worde_2), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word26 = car::recode(format_tagged_na(wordf_2), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word27 = car::recode(format_tagged_na(wordg_2), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word28 = car::recode(format_tagged_na(wordh_2), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word29 = car::recode(format_tagged_na(wordi_2), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word210 = car::recode(format_tagged_na(wordj_2), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word31 = car::recode(format_tagged_na(worda_3), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word32 = car::recode(format_tagged_na(wordb_3), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word33 = car::recode(format_tagged_na(wordc_3), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word34 = car::recode(format_tagged_na(wordd_3), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word35 = car::recode(format_tagged_na(worde_3), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word36 = car::recode(format_tagged_na(wordf_3), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word37 = car::recode(format_tagged_na(wordg_3), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word38 = car::recode(format_tagged_na(wordh_3), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word39 = car::recode(format_tagged_na(wordi_3), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word310 = car::recode(format_tagged_na(wordj_3), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     
                     # Panel
                     panel08_12 = 1,
                     panel10_14 = 0,
                     sample = "2008-2012",
                     
                     .keep = "none"
                     
                   )
)


suppressWarnings(GSS10_14 <-
                   GSS10_14 %>%
                   mutate(
                     
                     # Newspaper Reading
                     news11 = ifelse(news_1 %in% 1:5, 5 - news_1, NA),
                     news21 = ifelse(news_2 %in% 1:5, 5 - news_2, NA),
                     news31 = ifelse(news_3 %in% 1:5, 5 - news_3, NA),
                     
                     # Verbal Ability
                     word11 = car::recode(format_tagged_na(worda_1), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word12 = car::recode(format_tagged_na(wordb_1), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word13 = car::recode(format_tagged_na(wordc_1), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word14 = car::recode(format_tagged_na(wordd_1), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word15 = car::recode(format_tagged_na(worde_1), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word16 = car::recode(format_tagged_na(wordf_1), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word17 = car::recode(format_tagged_na(wordg_1), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word18 = car::recode(format_tagged_na(wordh_1), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word19 = car::recode(format_tagged_na(wordi_1), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word110 = car::recode(format_tagged_na(wordj_1), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word21 = car::recode(format_tagged_na(worda_2), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word22 = car::recode(format_tagged_na(wordb_2), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word23 = car::recode(format_tagged_na(wordc_2), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word24 = car::recode(format_tagged_na(wordd_2), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word25 = car::recode(format_tagged_na(worde_2), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word26 = car::recode(format_tagged_na(wordf_2), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word27 = car::recode(format_tagged_na(wordg_2), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word28 = car::recode(format_tagged_na(wordh_2), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word29 = car::recode(format_tagged_na(wordi_2), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word210 = car::recode(format_tagged_na(wordj_2), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word31 = car::recode(format_tagged_na(worda_3), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word32 = car::recode(format_tagged_na(wordb_3), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word33 = car::recode(format_tagged_na(wordc_3), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word34 = car::recode(format_tagged_na(wordd_3), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word35 = car::recode(format_tagged_na(worde_3), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word36 = car::recode(format_tagged_na(wordf_3), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word37 = car::recode(format_tagged_na(wordg_3), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word38 = car::recode(format_tagged_na(wordh_3), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word39 = car::recode(format_tagged_na(wordi_3), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     word310 = car::recode(format_tagged_na(wordj_3), "'    0'=0; '    1'=1; 'NA(n)'=0; 'NA(i)'=NA"),
                     
                     # Panel
                     panel08_12 = 0,
                     panel10_14 = 1,
                     sample = "2010-2014",
                     
                     .keep = "none"
                     
                   )
)

# Bind panels
GSS_panels <- rbind(GSS06_10, GSS08_12, GSS10_14)

## Bivariate Dual Change Score Model
gss.bdcm.mod <- '
  ### Latent Variables ###
  
  # Latent True Scores
  lword1 =~ 1*word11 + l2*word12 + l3*word13 + l4*word14 + l5*word15 + l6*word16 + l7*word17 + l8*word18 + l9*word19 + l10*word110
  lword2 =~ 1*word21 + l2*word22 + l3*word23 + l4*word24 + l5*word25 + l6*word26 + l7*word27 + l8*word28 + l9*word29 + l10*word210
  lword3 =~ 1*word31 + l2*word32 + l3*word33 + l4*word34 + l5*word35 + l6*word36 + l7*word37 + l8*word38 + l9*word39 + l10*word310
  lnews1 =~ 1*news11
  lnews2 =~ 1*news21
  lnews3 =~ 1*news31
  
  # Latent Change Scores
  dword2 =~ 1*lword2
  dword3 =~ 1*lword3
  dnews2 =~ 1*lnews2
  dnews3 =~ 1*lnews3
  
  # Growth Factor
  gword =~ 1*dword2 + 1*dword3
  gnews =~ 1*dnews2 + 1*dnews3

  ### Regression Structure ###

  # Autoregressions
  lword2 ~ 1*lword1
  lword3 ~ 1*lword2
  lnews2 ~ 1*lnews1
  lnews3 ~ 1*lnews2
  
  # Proportional Effects
  dword2 ~ beta_dyy*lword1
  dword3 ~ beta_dyy*lword2
  dnews2 ~ beta_dxx*lnews1
  dnews3 ~ beta_dxx*lnews2
  dword2 ~ beta_dyx*lnews1
  dword3 ~ beta_dyx*lnews2
  dnews2 ~ beta_dxy*lword1
  dnews3 ~ beta_dxy*lword2
  
  # Changes-to-Changes Effects
  dword3 + dnews3 ~ dword2 + dnews2
  
  ### Variance Structure ###
  
  # Observed Indicator Variances
  word11 ~~ var_y1*word11
  word21 ~~ var_y1*word21
  word31 ~~ var_y1*word31
  word12 ~~ var_y2*word12 
  word22 ~~ var_y2*word22 
  word32 ~~ var_y2*word32 
  word13 ~~ var_y3*word13 
  word23 ~~ var_y3*word23 
  word33 ~~ var_y3*word33 
  word14 ~~ var_y4*word14 
  word24 ~~ var_y4*word24 
  word34 ~~ var_y4*word34 
  word15 ~~ var_y5*word15 
  word25 ~~ var_y5*word25 
  word35 ~~ var_y5*word35 
  word16 ~~ var_y6*word16 
  word26 ~~ var_y6*word26 
  word36 ~~ var_y6*word36 
  word17 ~~ var_y7*word17 
  word27 ~~ var_y7*word27 
  word37 ~~ var_y7*word37 
  word18 ~~ var_y8*word18 
  word28 ~~ var_y8*word28 
  word38 ~~ var_y8*word38 
  word19 ~~ var_y9*word19 
  word29 ~~ var_y9*word29 
  word39 ~~ var_y9*word39 
  word110 ~~ var_y10*word110 
  word210 ~~ var_y10*word210 
  word310 ~~ var_y10*word310 
  news11 ~~ var_x1*news11
  news21 ~~ var_x1*news21
  news31 ~~ var_x1*news31
  
  # Latent True Score Variances
  lword1 ~~ lword1
  lword2 ~~ 0*lword2
  lword3 ~~ 0*lword3
  lnews1 ~~ lnews1
  lnews2 ~~ 0*lnews2
  lnews3 ~~ 0*lnews3
  
  # Latent Change Score Variances
  dword2 ~~ var_dy*dword2
  dword3 ~~ var_dy*dword3
  dnews2 ~~ var_dx*dnews2
  dnews3 ~~ var_dx*dnews3
  
  # Growth Factor Variances
  gword ~~ gword
  gnews ~~ gnews
  
  ### Mean Structure ###

  # Observed Indicator Intercepts 
  word11 + word21 + word31 ~ 0*1
  word12 + word22 + word32 ~ i2*1
  word13 + word23 + word33 ~ i3*1
  word14 + word24 + word34 ~ i4*1
  word15 + word25 + word35 ~ i5*1
  word16 + word26 + word36 ~ i6*1
  word17 + word27 + word37 ~ i7*1
  word18 + word28 + word38 ~ i8*1
  word19 + word29 + word39 ~ i9*1
  word110 + word210 + word310 ~ i10*1
  news11 + news21 + news31 ~ 0*1
  
  # Latent True Score Means 
  lword1 ~ 1
  lword2 ~ 0*1
  lword3 ~ 0*1
  lnews1 ~ 1
  lnews2 ~ 0*1
  lnews3 ~ 0*1
  
  # Latent Change Score Means
  dword2 ~ 1
  dword3 ~ 0*1
  dnews2 ~ 1
  dnews3 ~ 0*1
  
  # Growth Factor Means
  gword ~ 1
  gnews ~ 1
  
  ### Covariances ###
  
  # Residual Observed Covariances (Item Artifacts)
  word11 ~~ word21 + word31
  word21 ~~ word31
  word12 ~~ word22 + word32
  word22 ~~ word32
  word13 ~~ word23 + word33
  word23 ~~ word33
  word14 ~~ word24 + word34
  word24 ~~ word34
  word15 ~~ word25 + word35
  word25 ~~ word35
  word16 ~~ word26 + word36
  word26 ~~ word36
  word17 ~~ word27 + word37
  word27 ~~ word37
  word18 ~~ word28 + word38
  word28 ~~ word38
  word19 ~~ word29 + word39
  word29 ~~ word39
  word110 ~~ word210 + word310
  word210 ~~ word310
  
  # Residual Observed Covariances (Time Artifacts)
  news11 ~~ cov_x1y1*word11 + cov_x1y2*word12 + cov_x1y3*word13 + cov_x1y4*word14 + cov_x1y5*word15 + cov_x1y6*word16 + cov_x1y7*word17 + cov_x1y8*word18 + cov_x1y9*word19 + cov_x1y10*word110
  word11 ~~ cov_y1y2*word12 + cov_y1y3*word13 + cov_y1y4*word14 + cov_y1y5*word15 + cov_y1y6*word16 + cov_y1y7*word17 + cov_y1y8*word18 + cov_y1y9*word19 + cov_y1y10*word110
  word12 ~~ cov_y2y3*word13 + cov_y2y4*word14 + cov_y2y5*word15 + cov_y2y6*word16 + cov_y2y7*word17 + cov_y2y8*word18 + cov_y2y9*word19 + cov_y2y10*word110
  word13 ~~ cov_y3y4*word14 + cov_y3y5*word15 + cov_y3y6*word16 + cov_y3y7*word17 + cov_y3y8*word18 + cov_y3y9*word19 + cov_y3y10*word110
  word14 ~~ cov_y4y5*word15 + cov_y4y6*word16 + cov_y4y7*word17 + cov_y4y8*word18 + cov_y4y9*word19 + cov_y4y10*word110
  word15 ~~ cov_y5y6*word16 + cov_y5y7*word17 + cov_y5y8*word18 + cov_y5y9*word19 + cov_y5y10*word110
  word16 ~~ cov_y6y7*word17 + cov_y6y8*word18 + cov_y6y9*word19 + cov_y6y10*word110
  word17 ~~ cov_y7y8*word18 + cov_y7y9*word19 + cov_y7y10*word110
  word18 ~~ cov_y8y9*word19 + cov_y8y10*word110
  word19 ~~ cov_y9y10*word110
  
  news21 ~~ cov_x1y1*word21 + cov_x1y2*word22 + cov_x1y3*word23 + cov_x1y4*word24 + cov_x1y5*word25 + cov_x1y6*word26 + cov_x1y7*word27 + cov_x1y8*word28 + cov_x1y9*word29 + cov_x1y10*word210
  word21 ~~ cov_y1y2*word22 + cov_y1y3*word23 + cov_y1y4*word24 + cov_y1y5*word25 + cov_y1y6*word26 + cov_y1y7*word27 + cov_y1y8*word28 + cov_y1y9*word29 + cov_y1y10*word210
  word22 ~~ cov_y2y3*word23 + cov_y2y4*word24 + cov_y2y5*word25 + cov_y2y6*word26 + cov_y2y7*word27 + cov_y2y8*word28 + cov_y2y9*word29 + cov_y2y10*word210
  word23 ~~ cov_y3y4*word24 + cov_y3y5*word25 + cov_y3y6*word26 + cov_y3y7*word27 + cov_y3y8*word28 + cov_y3y9*word29 + cov_y3y10*word210
  word24 ~~ cov_y4y5*word25 + cov_y4y6*word26 + cov_y4y7*word27 + cov_y4y8*word28 + cov_y4y9*word29 + cov_y4y10*word210
  word25 ~~ cov_y5y6*word26 + cov_y5y7*word27 + cov_y5y8*word28 + cov_y5y9*word29 + cov_y5y10*word210
  word26 ~~ cov_y6y7*word27 + cov_y6y8*word28 + cov_y6y9*word29 + cov_y6y10*word210
  word27 ~~ cov_y7y8*word28 + cov_y7y9*word29 + cov_y7y10*word210
  word28 ~~ cov_y8y9*word29 + cov_y8y10*word210
  word29 ~~ cov_y9y10*word210
  
  news31 ~~ cov_x1y1*word31 + cov_x1y2*word32 + cov_x1y3*word33 + cov_x1y4*word34 + cov_x1y5*word35 + cov_x1y6*word36 + cov_x1y7*word37 + cov_x1y8*word38 + cov_x1y9*word39 + cov_x1y10*word310
  word31 ~~ cov_y1y2*word32 + cov_y1y3*word33 + cov_y1y4*word34 + cov_y1y5*word35 + cov_y1y6*word36 + cov_y1y7*word37 + cov_y1y8*word38 + cov_y1y9*word39 + cov_y1y10*word310
  word32 ~~ cov_y2y3*word33 + cov_y2y4*word34 + cov_y2y5*word35 + cov_y2y6*word36 + cov_y2y7*word37 + cov_y2y8*word38 + cov_y2y9*word39 + cov_y2y10*word310
  word33 ~~ cov_y3y4*word34 + cov_y3y5*word35 + cov_y3y6*word36 + cov_y3y7*word37 + cov_y3y8*word38 + cov_y3y9*word39 + cov_y3y10*word310
  word34 ~~ cov_y4y5*word35 + cov_y4y6*word36 + cov_y4y7*word37 + cov_y4y8*word38 + cov_y4y9*word39 + cov_y4y10*word310
  word35 ~~ cov_y5y6*word36 + cov_y5y7*word37 + cov_y5y8*word38 + cov_y5y9*word39 + cov_y5y10*word310
  word36 ~~ cov_y6y7*word37 + cov_y6y8*word38 + cov_y6y9*word39 + cov_y6y10*word310
  word37 ~~ cov_y7y8*word38 + cov_y7y9*word39 + cov_y7y10*word310
  word38 ~~ cov_y8y9*word39 + cov_y8y10*word310
  word39 ~~ cov_y9y10*word310
  
  # Growth Factor Covariance with T1 True Score
  gword ~~ lword1
  gnews ~~ lnews1

  # Covariances Between Growth Factors
  lword1 ~~ lnews1
  lword1 ~~ gnews
  lnews1 ~~ gword
  gnews ~~ gword 
  '

gss.bdcm.fit <- cfa(gss.bdcm.mod, data = GSS_panels, missing = "fiml", cluster = "sample")

summary(gss.bdcm.fit, fit.measures = T, standardized = T)

gss.bdcm.pars <- parameterEstimates(gss.bdcm.fit, standardized = T)

gss.bdcm.pars <- gss.bdcm.pars %>% 
  mutate(
    ci.lower.std = std.all - (ci.upper - est) / (est / std.all),
    ci.upper.std = std.all + (ci.upper - est) / (est / std.all),
    effect = case_when(((op == "~") & grepl('^dnews', rhs) & grepl('^dnews', lhs)) | 
                         ((op == "~") & grepl('^dword', rhs) & grepl('^dword', lhs)) |
                         ((op == "~") & grepl('^dnews', rhs) & grepl('^dword', lhs)) | 
                         ((op == "~") & grepl('^dword', rhs) & grepl('^dnews', lhs)) ~ "Change to Change Effects",
                       ((op == "~") & grepl('^lnews', rhs) & grepl('^dword', lhs)) | 
                         ((op == "~") & grepl('^lword', rhs) & grepl('^dnews', lhs)) |
                         ((op == "~") & grepl('^lnews', rhs) & grepl('^dnews', lhs)) | 
                         ((op == "~") & grepl('^lword', rhs) & grepl('^dword', lhs)) ~ "Proportional Effects",
                       .default = "other"),
    Year = case_when(effect == "Change to Change Effects" ~ 
                       paste0("\u394 (Time ", str_sub(rhs, start = -1), "-Time ", str_sub(lhs, start = -1), ") \u2192 ",
                              "\u394 (Time ", str_sub(rhs, start = -1), "-Time ", str_sub(lhs, start = -1), ")"),
                     effect == "Proportional Effects" ~ 
                       paste0("Time ", str_sub(rhs, start = -1), " \u2192 ", "\u394 (Time ", str_sub(rhs, start = -1), 
                              "-Time ", str_sub(lhs, start = -1), ")"),
                     .default = "other"),
    dv.iv = case_when((op == "~") & grepl('^dword|^lword', rhs) & grepl('^dnews', lhs) ~ "Verbal Ability \u2192 Newspaper Reading", 
                      (op == "~") & grepl('^dword|^lword', rhs) & grepl('^dword', lhs) ~ "Verbal Ability \u2192 Verbal Ability",
                      (op == "~") & grepl('^dnews|^lnews', rhs) & grepl('^dnews', lhs) ~ "Newspaper Reading \u2192 Newspaper Reading",
                      (op == "~") & grepl('^dnews|^lnews', rhs) & grepl('^dword', lhs) ~ "Newspaper Reading \u2192 Verbal Ability",
                      .default = "other"),
    sig = ifelse(ci.lower.std > 0 | ci.upper.std < 0, "Yes", "No")
  )

tabB1.data <- gss.bdcm.pars %>% 
  dplyr::select(effect, Year, dv.iv, std.all, pvalue, est, se) %>% 
  filter(effect != "other") %>% 
  arrange(effect, dv.iv, Year)

tabB1.data







#========================================================================#
#   Figure E1. Sample Size as a Function of Missingness Allowed in DVs   #
#========================================================================#

datasets <- list(
  list(data = ANES08_10[!is.na(ANES08_10$wordsum_scale), c("gay_marr_ban.01", "abortion.02", "gay_marriage.11", "wealthy_tax.01", "gov_pay_drugs.01", "healthcare_spend.01", "gov_services.11", 
                                                           "soc_security_inc.11", "soc_security_tax.11", "income_equality.02", "gov_business.11", "soc_security_priv.11", "immig_work.01", "immig_citizen.01",
                                                           "aa_university.09", "aa_employment.11", "gov_fair_treat.09", "emissions.02", "fuel_econ.02", "gas_tax.02")], 
       sample = "2008-2010", 
       dep_var = "Constraint"),
  list(data = ANES08_10[!is.na(ANES08_10$wordsum_scale), colnames(global_stblty08_10)], 
       sample = "2008-2010", 
       dep_var = "Stability"),
  list(data = ANES12_13[!is.na(ANES12_13$wordsum_scale), c("abortion.12", "gay_job_discrim.12", "gay_military.12", "gay_adoption.12", "gay_marriage.12", "marijuana.12", "guar_job_living.12", "soc_sec_spend.12", "childcare_spend.12",
                                                           "school_spend.12", "welfare_spend.12", "poor_spend.12", "gov_services.12", "wealthy_tax.12", "income_equality.12", "public_insurance.12", "insurance_law.12", "bank_bailout.12",
                                                           "reduce_deficit.12", "deficit_imp.12", "regulation.12", "immigration_levels.12", "illegal_policy.12", "illegal_citizen.12", "legal_status_checks.12", "aa_hire_promotion.12",
                                                           "aa_at_work.12", "aa_university.12", "gov_fair_treat.12", "gov_help_blacks.12", "enviro_regulate.12", "offshore_drill.12", "spend_enviro.12", "death_penalty.12", "gun_control.12")], 
       sample = "2012-2013", 
       dep_var = "Constraint"),
  list(data = ANES12_13[!is.na(ANES12_13$wordsum_scale), c("deficit_imp_stblty", "deficit_tax_stblty", "defense_spend_stblty", "gun_control_stblty")], 
       sample = "2012-2013", 
       dep_var = "Stability"),
  list(data = ANES16_20[!is.na(ANES16_20$wordsum_scale), c("abortion.16", "gay_job_discrim.16", "gay_adoption.16", "gay_marriage.16", "deny_services.16", "trans_bathroom.16", "marijuana.16", "guar_job_living.16",
                                                           "welfare_spend.16", "poor_spend.16", "school_spend.16", "gov_services.16", "soc_sec_spend.16", "healthcare_spend.16", "wealthy_tax.16", "income_equality.16",
                                                           "regulate_banks.16", "minimum_wage.16", "deficit_imp.16", "regulation.16", "public_insurance.16", "insurance_law.16", "parental_leave.16",
                                                           "illegal_immigrants.16", "end_birthright.16", "deport_children.16", "build_wall.16", "admit_refugees.16", "immigration_levels.16", "aa_hire_promotion.16",
                                                           "gov_assist_blacks.16", "aa_university.16", "enviro_regulate.16", "fed_rising_temp.16", "spend_enviro.16", "fracking.16")], 
       sample = "2016-2020", 
       dep_var = "Constraint"),
  list(data = ANES16_20[!is.na(ANES16_20$wordsum_scale), colnames(global_stblty16_20)], 
       sample = "2016-2020", 
       dep_var = "Stability")
)

prop_values <- seq(0, 99, by = 1)

figE1.data <- data.frame()

for (dataset in datasets) {
  data <- dataset$data
  sample <- dataset$sample
  dep_var <- dataset$dep_var
  nonNA_count <- numeric(length(prop_values))
  for (i in seq_along(prop_values)) {
    prop <- prop_values[i]
    NA_threshold <- ncol(data) / (100 / prop)
    sums <- ifelse(
      rowSums(is.na(data)) > NA_threshold,
      rowSums(data, na.rm = F),
      rowSums(data, na.rm = T)
    )
    nonNA_count[i] <- sum(!is.na(sums))
  }
  
  temp_df <- data.frame(
    prop = prop_values,
    nonNA_count = nonNA_count,
    sample = sample,
    dep_var = dep_var
  )
  
  figE1.data <- rbind(figE1.data, temp_df)
}

figE1 <-
  ggplot(figE1.data, aes(prop, nonNA_count)) +
  facet_grid(sample ~ dep_var, scales = "free_y") +
  geom_line() +
  geom_vline(data = data.frame(dep_var = c(rep("Constraint", 3), rep("Stability", 3)),
                               sample = rep(c("2008-2010", "2012-2013", "2016-2020"), 2),
                               x = c(rep(10, 3), 50, 10, 10)), aes(xintercept = x), color = "red") +
  labs(title = "Figure E1. Sample Size as a Function of Missingness Allowed in DVs",
       y = "Non-Missing Observations",
       x = "Proportion of Missing Items Allowed") +
  theme_bw() +
  theme(
    axis.ticks = element_line(color = "grey65"),
    panel.border = element_rect(color = "grey65"),
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "grey65", color = "grey65"),
    strip.text = element_text(angle = 0, color = "white")
  )


Cairo(4000, 4000, file = "Figure E1.png", res = 600)

figE1

dev.off()






#========================================================================#
#  Figure E2. Bivariate Relationships Between Constraint and Stability   #
#========================================================================#

figE2.data <- 
  ANES_pooled %>%
  dplyr::select("global_stblty", "global_const", "case_id", "survey")

figE2.data$survey <- factor(figE2.data$survey, levels = c("2008-2010", "2012-2013", "2016-2020"), ordered = T)

figE2.data$stab_items <- factor(case_when(figE2.data$survey == "2008-2010" ~ "24-Item Stability Index",
                                         figE2.data$survey == "2012-2013" ~ "4-Item Stability Index",
                                         figE2.data$survey == "2016-2020" ~ "36-Item Stability Index"), 
                               levels = c("24-Item Stability Index", 
                                          "4-Item Stability Index", 
                                          "36-Item Stability Index"), ordered = T)

figE2.data$const_items <- factor(case_when(figE2.data$survey == "2008-2010" ~ "35-Item Constraint Index",
                                          figE2.data$survey == "2012-2013" ~ "20-Item Constraint Index",
                                          figE2.data$survey == "2016-2020" ~ "36-Item Constraint Index"), 
                                levels = c("35-Item Constraint Index", 
                                           "20-Item Constraint Index", 
                                           "36-Item Constraint Index"), ordered = T)

figE2 <- 
  ggplot(figE2.data, aes(y = global_stblty, x = global_const)) +
  geom_point(size = 0.1) +
  geom_smooth(color = "#00AFBB") +
  scale_x_continuous(breaks = c(0, 0.5, 1),
                     labels = c("0.0", "0.5", "1.0"),
                     limits = c(0,1)) +
  facet_wrap( ~ survey, ncol = 3) +
  stat_cor(label.y = 0, label.x = 0.55,
           p.accuracy = 0.001, r.accuracy = 0.01, size = 6, method = "pearson", cor.coef.name = "r") +
  labs(y = "Attitude Stability",
       x = "Ideological Constraint") +
  theme_bw(base_size = 22) +
  theme(
    axis.ticks = element_line(color = "grey65"),
    panel.border = element_rect(color = "grey65"),
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "grey65", color = "grey65"),
    strip.text = element_text(angle = 0, color = "white")
  )

Cairo(6000, 2675, file = "Figure E2.png", res = 400)

figE2

dev.off()


