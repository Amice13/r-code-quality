# This R script prepares the raw data for all analysis underlying the main article and SI
# To guarantee version control, it uses groundhog (verion 3.2.0). 
library("groundhog") # Version control
pkgs <- c("Hmisc", # Weighted mean and variance
          "tidyverse", # Data management
          "furniture") # row-means for additive scales
groundhog.library(pkgs, "2024-10-15", include.suggests = TRUE)
source("1b-fun.R") # Self-written functions

# 1. Read and clean the raw data
################################
Dat <- readRDS("05FullFinalData_forClient.Rds") %>%
  arrange(Id, Wave) %>%
  group_by(Id) %>%
  mutate(
    wave = lead(Wave, n = 1, default = NA),
    wave  = case_when(
      is.na(wave) & Wave == 1 ~ 1,
      TRUE ~ wave)) %>% ungroup()

Dat <- Dat %>%
  filter(Wave == 1) %>%
  rows_patch(., Dat %>% filter(Wave == 2), by = "Id") %>%
  mutate(
    # 1.1 Pre-treatment moderators and controls
    ###########################################
    ## Reduce income inequality
    mod_incineq = mrln_to_num(moderator1a_c_1_resp) %>% mrln_std(wgt), 
    ## Immigration makes Denmark worse place to live
    mod_immibad = mrln_to_num(moderator1a_c_2_resp) %>% mrln_std(wgt), 
    ## Homosexual couples should have same rights
    mod_homo = mrln_to_num(moderator1a_c_3_resp) %>% mrln_std(wgt), 
    ## News consumption moderator (not part of pilot)
    media = mrln_to_num(media) %>% mrln_std(wgt), 
    # Age
    Alder = Alder %>% mrln_std(wgt), 
    # Education
    educ = case_when(
      str_detect(bagg2, "Grundskole") ~ "Low",
      str_detect(bagg2, "Lang videregående|Bachelor|Mellemlang") ~ "High",
      TRUE ~ "Medium") %>% as.factor() %>% fct_relevel("Low", "Medium", "High"),
    ## Persons in the Household
    hh_size = str_extract(bagg4, "\\d") %>% as.numeric() %>% mrln_std(wgt),
    ## Employment status
    empl = case_when(
      bagg5 == "Ja, fuldtid" ~ "Full time",
      bagg5 == "Ja, deltid" ~ "Part time",
      bagg5 == "Nej" ~ "Not employed"),
    # 1.2 Treatments
    ################
    ## Correction
    tr_crction = case_when(
      Q_random_group_1 == "Yes" ~ "No elicitation of beliefs",
      Q_random_group_02_1 == "Yes" ~ "No correction",
      Q_random_group_02_1 == "No" & Q_Exp4_ran_1 == "Yes" ~ "Correction",
      Q_random_group_02_1 == "No" & Q_Exp4_ran_2 == "Yes" ~ "Correction + Researcher",
      Q_random_group_02_1 == "No" & Q_Exp4_ran_3 == "Yes" ~ "Correction + Lawyer",
      Q_random_group_02_1 == "No" & Q_Exp4_ran_4 == "Yes" ~ "Correction + Affected") %>%
      fct_relevel("No elicitation of beliefs", "No correction", "Correction", 
                  "Correction + Researcher", "Correction + Lawyer", 
                  "Correction + Affected"),
    ## Framer
    tr_person = case_when(
      # Researcher
      photo_has_seen_01 == "Yes" ~ "Res. Kristina",
      photo_has_seen_02 == "Yes" ~ "Res. Martin",
      photo_has_seen_03 == "Yes" ~ "Res. Mathilde",
      photo_has_seen_04 == "Yes" ~ "Res. Jakob",
      # Lawyer
      photo_has_seen_05 == "Yes" ~ "Law. Tine",
      photo_has_seen_06 == "Yes" ~ "Law. Nikolaj",
      photo_has_seen_07 == "Yes" ~ "Law. Tarek",
      photo_has_seen_08 == "Yes" ~ "Law. Nanna",
      # Potentially affected
      photo_has_seen_09 == "Yes" ~ "Aff. Loubna",
      photo_has_seen_10 == "Yes" ~ "Aff. Mohammed",
      photo_has_seen_11 == "Yes" ~ "Aff. Yasmin",
      photo_has_seen_12 == "Yes" ~ "Aff. Hassan",
      tr_crction ==  "No elicitation of beliefs" ~  "No elicitation of beliefs",
      tr_crction == "No correction" ~ "No correction",
      tr_crction == "Correction" ~ "Correction") %>%
      as.factor() %>% fct_relevel(
        "No elicitation of beliefs",
        "No correction", "Correction",
        "Res. Kristina", "Res. Mathilde", "Res. Jakob", "Res. Martin",
        "Law. Tine", "Law. Nanna", "Law. Nikolaj", "Law. Tarek",
        "Aff. Yasmin", "Aff. Loubna", "Aff. Hassan", "Aff. Mohammed"),
    ## Which correspondence study
    tr_study = case_when(
      Q_Exp1_ran_1 == "Yes" ~ "Primary schools",
      Q_Exp1_ran_2 == "Yes" ~ "Hiring",
      Q_Exp1_ran_3 == "Yes" ~ "Politicians",
      Q_Exp1_ran_4 == "Yes" ~ "Housing") %>% fct_relevel("Hiring"),
    tr_study2 = case_when(
      Q_Exp1_ran_v2_1 == "Yes" ~ "Primary schools",
      Q_Exp1_ran_v2_2 == "Yes" ~ "Hiring",
      Q_Exp1_ran_v2_3 == "Yes" ~ "Politicians",
      Q_Exp1_ran_v2_4 == "Yes" ~ "Housing") %>% fct_relevel("Hiring"),
    ## Wave 1 or wave2?
    tr_wave = case_when(
      group_random_Partial_1 == "Yes" ~ "Direct outcome",
      group_random_Partial_1 == "No" ~ "Delayed outcome"),
    # 1.3 Guesses
    #############
    ## Guess 1
    guess1 = case_when(
      tr_study == "Primary schools" ~ Exp1_a_b1,
      tr_study == "Hiring" ~ Exp1_b_b1,
      tr_study == "Politicians" ~ Exp1_c_b1,
      tr_study == "Housing" ~ Exp1_d_b1),
    ### Guess 1 bias
    guess1_bias = case_when(
      tr_study == "Primary schools" ~ guess1 - 15,
      tr_study == "Hiring" ~ guess1 - 22,
      tr_study == "Politicians" ~ guess1 - 55,
      tr_study == "Housing" ~ guess1 - 29),
    z_guess1_bias = mrln_std(guess1_bias, wgt),
    ### Alternative: Guess 1 bias within-study z-standardized
    wsz_guess1_bias = case_when(
      tr_study == "Primary schools" ~ mrln_std(guess1 - 15, wgt),
      tr_study == "Hiring" ~ mrln_std(guess1 - 22, wgt),
      tr_study == "Politicians" ~ mrln_std(guess1 - 55, wgt),
      tr_study == "Housing" ~ mrln_std(guess1 - 29, wgt)),
    ### Alternative: Negative-censored guess 1 bias values < 0 -> 0.
    negcens_guess1_bias = case_when(
      guess1_bias < 0 ~ 0,
      TRUE ~ guess1_bias),
    ### Remember correction of guess 1?
    rembr_crction = case_when(
      tr_study == "Primary schools" ~ manipulation_test_1a_b1,
      tr_study == "Hiring" ~ manipulation_test_1b_b1,
      tr_study == "Politicians" ~ manipulation_test_1c_b1,
      tr_study == "Housing" ~ manipulation_test_1c_b1,),
    rembr_crction_bias = case_when(
      tr_study == "Primary schools" ~ rembr_crction - 15,
      tr_study == "Hiring" ~ rembr_crction - 22,
      tr_study == "Politicians" ~ rembr_crction - 55,
      tr_study == "Housing" ~ rembr_crction - 29),
    ## Guess 2
    guess2 = case_when(
      tr_study2 == "Primary schools" ~ Exp1_a_b1,
      tr_study2 == "Hiring" ~ Exp1_b_b1,
      tr_study2 == "Politicians" ~ Exp1_c_b1,
      tr_study2 == "Housing" ~ Exp1_d_b1),
    ### Guess 2 bias
    guess2_bias = case_when(
      tr_study == "Primary schools" ~ guess2 - 15,
      tr_study == "Hiring" ~ guess2 - 22,
      tr_study == "Politicians" ~ guess2 - 55,
      tr_study == "Housing" ~ guess2 - 29),
    z_guess2_bias = mrln_std(guess2_bias, wgt),
    # 1.4 Outcomes
    ##############
    ## Main outcome
    blind_appl_w1 = mrln_to_num(outcome1_c_1_resp),
    blind_appl_w2 = mrln_to_num(policy_a_e_3_resp),
    #### Recognition of discrimination as social problem.
    prob_ethn1 = mrln_to_num(problem_a_f_1_resp),
    prob_ethn2 = mrln_to_num(problem_a_f_3_resp),
    prob_ethn3 = mrln_to_num(problem_a_f_5_resp),
    conseq1 = mrln_to_num(consequences_a_b_1_resp),
    conseq2 = mrln_to_num(consequences_a_b_2_resp),
    ### Reasons for discrimination.
    reason1 = mrln_to_num(reason_a_c_1_resp),
    reason2 = mrln_to_num(reason_a_c_2_resp),
    reason3 = mrln_to_num(reason_a_c_3_resp),
    ### Support for equal treatment policies.
    eqltrt1 = mrln_to_num(policy_a_e_1_resp),
    eqltrt2 = mrln_to_num(policy_a_e_4_resp),
    eqltrt3 = mrln_to_num(policy_a_e_5_resp),
    eqltrt4 = mrln_to_num(policy_a_e_6_resp),
    ### Behavioral outcome
    donate = case_when(
      is.na(behavioral_outcome1_b3) ~ 0,
      TRUE ~ as.numeric(behavioral_outcome1_b3)),
    # 1.5 For the  R&R:
    ##################
    ## Field experiments are a convincing method
    fldexp = mrln_to_num(outcome1_a_b_1_resp),
    ## Politicians should listen to researchers
    rsrch_adv = mrln_to_num(outcome1_a_b_2_resp),
    ## Rename Danish letters
    Region = case_when(
      region == "Sjælland" ~ "Sjaelland",
      TRUE ~ region)) %>% rename(koen = køn)

# 2. Scales
###########
## Moderators don't scale!
Dat %>% dplyr::select(starts_with("mod_"), media) %>% drop_na() %>% cor()
prcomp(~ mod_incineq + mod_immibad + mod_homo, data = Dat, scale = TRUE) %>%
  summary()

## Ethnic discrimination is a significant problem in Denmark, scales well!
Dat %>% dplyr::select(starts_with("prob_ethn"), conseq1, conseq2) %>% drop_na() %>% cor()
prcomp(~ prob_ethn1 + prob_ethn2 + prob_ethn3 + conseq1 + conseq2, 
       data = Dat, scale = TRUE) %>%
  summary()
Dat <- Dat %>% mutate( # Make an additive scale
  recogni = rowmeans(prob_ethn1, prob_ethn2, prob_ethn3, conseq1, conseq2, na.rm = TRUE))

## Reasons for discrimination, don't scale well. 
## But we combine them anyway for brevity.
Dat %>% dplyr::select(reason1, reason2, reason3) %>% drop_na() %>% cor()
prcomp(~ reason2 + reason3, data = Dat, scale = TRUE) %>%
  summary()
Dat <- Dat %>% mutate( # Make an additive scale
  reason = rowmeans(reason1, (6-reason2), (6-reason3), na.rm = TRUE))

# Equal treatment policies, scales well!
Dat %>% dplyr::select(starts_with("eqltrt")) %>% drop_na() %>% cor()
prcomp(~ eqltrt1 + eqltrt2 + eqltrt3 + eqltrt4, data = Dat, scale = TRUE) %>%
  summary()
Dat <- Dat %>% mutate( # Make an additive scale
  eqltrt = rowmeans(eqltrt1, eqltrt2, eqltrt3, eqltrt4, na.rm = TRUE))

# Distinguish between those interviewed right away,
# and those interviewed at least a week later.
Dat_wave1 <- Dat %>%
  mutate( # Control-group specific z-standardization
    z0_recogni = case_when(
      wave == 1 ~ mrln_cntr_std(
        recogni, wgt, tr_crction == "No elicitation of beliefs"),
      TRUE ~ as.numeric(NA)),
    z1_recogni = case_when(
      wave == 1 ~ mrln_cntr_std(
        recogni, wgt, tr_crction == "No correction"),
      TRUE ~ as.numeric(NA)),
    z_recogni = mrln_std(recogni, gewicht = wgt),
    z0_reason = case_when(
      wave == 1 ~ mrln_cntr_std(
        reason, wgt, tr_crction == "No elicitation of beliefs"),
      TRUE ~ as.numeric(NA)),
    z1_reason = case_when(
      wave == 1 ~ mrln_cntr_std(
        reason, wgt, tr_crction == "No correction"),
      TRUE ~ as.numeric(NA)),  
    z_reason = mrln_std(reason, gewicht = wgt),
    z0_blind_appl_w1 = mrln_cntr_std( # Was asked to all in Wave 1 and wave 2!
      blind_appl_w1, wgt, tr_crction == "No elicitation of beliefs"),
    z1_blind_appl_w1 = mrln_cntr_std(
      blind_appl_w1, wgt, tr_crction == "No correction"),
    z_blind_appl_w1 = mrln_std(blind_appl_w1, gewicht = wgt),
    z0_eqltrt = case_when(
      wave == 1 ~ mrln_cntr_std(
        eqltrt, wgt, tr_crction == "No elicitation of beliefs"),
      TRUE ~ as.numeric(NA)),
    z1_eqltrt = case_when(
      wave == 1 ~ mrln_cntr_std(
        eqltrt, wgt, tr_crction == "No correction"),
      TRUE ~ as.numeric(NA)),  
    z_eqltrt = mrln_std(eqltrt, gewicht = wgt),
    z0_donate = mrln_cntr_std(
        donate, wgt, tr_crction == "No elicitation of beliefs"),
    z1_donate = mrln_cntr_std(
        donate, wgt, tr_crction == "No correction"))

# 4. Identify sub-samples
#########################
# Underperceivers
Dat_w1_uperc <- Dat_wave1 %>% filter(guess1_bias > 0)

# Wave 2: 1 to three weeks later
Dat_wave2 <- Dat %>% 
  filter(wave  == 2 | tr_crction == "No elicitation of beliefs") %>%
  mutate( ## Ethnic discrimination is a significant problem in Denmark
    recogni = rowmeans(prob_ethn1, prob_ethn2, prob_ethn3, na.rm = TRUE),
    z0_recogni = mrln_cntr_std(recogni, wgt, tr_crction == "No elicitation of beliefs"),
    z1_recogni = mrln_cntr_std(recogni, wgt, tr_crction == "No correction"),
    blind_appl_w2 = case_when( # Replace outcome with W1 for no elicitation
      tr_crction == "No elicitation of beliefs" ~ blind_appl_w1,
      TRUE ~ blind_appl_w2),
    z0_blind_appl_w2 = mrln_cntr_std( # Was asked to all in Wave 1 and wave 2!
      blind_appl_w2, wgt, tr_crction == "No elicitation of beliefs"),
    z1_blind_appl_w2 = mrln_cntr_std( # Was asked to all in Wave 1 and wave 2!
      blind_appl_w2, wgt, tr_crction == "No correction"),
    z0_eqltrt = mrln_cntr_std(eqltrt, wgt, tr_crction == "No elicitation of beliefs"),
    z1_eqltrt = mrln_cntr_std(eqltrt, wgt, tr_crction == "No correction"))

# Save data for analyses
save(Dat, Dat_w1_uperc, Dat_wave1, Dat_wave2, file = "CleanedData.RData")