##############################################
#### Data cleaning file: Garbe et al #########
##############################################

#### Create conjoint profile-level dataset ###

d$rd_rand_a_public_service_2 <- d$rd_rand_a_public_service_2_1
d$rd_rand_a_public_service_3 <- d$rd_rand_a_public_service_3_1
d$rd_rand_a_security_2 <- d$rd_rand_a_security_2_1
d$rd_rand_a_security_3 <- d$rd_rand_a_security_3_1
d$rd_rand_a_tax_reg_2 <- d$rd_rand_a_tax_reg_2_1
d$rd_rand_a_tax_reg_3 <- d$rd_rand_a_tax_reg_3_1
d$rd_rand_a_voting_2 <- d$rd_rand_a_voting_2_1
d$rd_rand_a_voting_3 <- d$rd_rand_a_voting_3_1
d$rd_rand_a_social_prot_2 <- d$rd_rand_a_social_prot_2_1
d$rd_rand_a_social_prot_3 <- d$rd_rand_a_social_prot_3_1

d$rd_rand_b_public_service_2 <- d$rd_rand_b_public_service_2_1
d$rd_rand_b_public_service_3 <- d$rd_rand_b_public_service_3_1
d$rd_rand_b_security_2 <- d$rd_rand_b_security_2_1
d$rd_rand_b_security_3 <- d$rd_rand_b_security_3_1
d$rd_rand_b_tax_reg_2 <- d$rd_rand_b_tax_reg_2_1
d$rd_rand_b_tax_reg_3 <- d$rd_rand_b_tax_reg_3_1
d$rd_rand_b_voting_2 <- d$rd_rand_b_voting_2_1
d$rd_rand_b_voting_3 <- d$rd_rand_b_voting_3_1
d$rd_rand_b_social_prot_2 <- d$rd_rand_b_social_prot_2_1
d$rd_rand_b_social_prot_3 <- d$rd_rand_b_social_prot_3_1

d$rd_policy_choice_2 <- d$rd_policy_choice_2_1
d$rd_policy_choice_3 <- d$rd_policy_choice_3_1
d$rd_policy_choice_confirm_2 <- d$rd_policy_choice_2_confirm_1
d$rd_policy_choice_confirm_3 <- d$rd_policy_choice_3_confirm_1

# per per profile outcome questions
d$digital_id_supportive_1_1 <- d$digital_id_supportive
d$likely_register_digital_id_1_1 <- d$likely_register_digital_id

# mechanisms questions, only asked for round 1
d$easy_access_govt_services_1_1 <- d$easy_access_govt_services
d$adequate_data_privacy_1_1 <- d$adequate_data_privacy
d$worry_punished_view_1_1 <- d$worry_punished_view
d$worry_vote_counted_1_1 <- d$worry_vote_counted
d$worry_personal_info_1_1 <- d$worry_personal_info

# manipulation check questions
d$connnected_voter_reg_1_1 <- d$connnected_voter_reg
d$take_biometric_photo_1_1 <- d$take_biometric_photo

attr_names <- c("public_service", "security", "tax_reg", "voting", "social_prot")
follow_ups <- c("easy_access_govt_services", "adequate_data_privacy", "worry_punished_view", 
                 "worry_vote_counted", "worry_personal_info")
manipulation <- c("connnected_voter_reg", "take_biometric_photo")

policyFun <- function(x, attr_names, follow_ups){
  policies_1 <- data.frame(matrix(NA, nrow = 3, ncol = length(attr_names) + 8 + length(follow_ups)))
  policies_2 <- data.frame(matrix(NA, nrow = 3, ncol = length(attr_names) + 8 + length(follow_ups)))
  colnames(policies_1) <- colnames(policies_2) <- c(attr_names, "policy", "round", "choice", "choice_confirm", 
                                                    follow_ups, "digital_id_supportive", "likely_register_digital_id", manipulation)
  for(i in c(1:3)){
    vars_1 <- paste("rd_rand_a", attr_names, i, sep = "_")
    vars_2 <- paste("rd_rand_b", attr_names, i, sep = "_" )
    char_1 <- ifelse(x[vars_1] == 1, 1, 0)
    char_2 <- ifelse(x[vars_2] == 1, 1, 0)
    choice_0 <- x[paste("rd_policy_choice", i, sep = "_")]
    choice_1 <- ifelse(choice_0 == 1, 1, ifelse(choice_0 == -999, -999, 0))
    choice_2 <- ifelse(choice_1 == -999, -999, 1 - choice_1)
    choice_confirm_0 <- x[paste("rd_policy_choice_confirm", i, sep = "_")]
    choice_confirm_1 <- ifelse(choice_confirm_0 == 1, 1, ifelse(choice_confirm_0 == 2, 0, ifelse(choice_confirm_0 == -999, -999, NA)))
    choice_confirm_2 <- ifelse(choice_confirm_0 == 2, 1, ifelse(choice_confirm_0 == 1, 0, ifelse(choice_confirm_0 == -999, -999, NA)))
    digital_id_supportive_1 <- x[paste("digital_id_supportive", i, 1, sep = "_")]
    digital_id_supportive_2 <- x[paste("digital_id_supportive", i, 2, sep = "_")]
    likely_register_digital_id_1 <- x[paste("likely_register_digital_id", i, 1, sep = "_")]
    likely_register_digital_id_2 <- x[paste("likely_register_digital_id", i, 2, sep = "_")]
    connected_voter_reg_1 <- x[paste("connnected_voter_reg", i, 1, sep = "_")]
    connected_voter_reg_2 <- x[paste("connnected_voter_reg", i, 2, sep = "_")]
    take_biometric_photo_1 <- x[paste("take_biometric_photo", i, 1, sep = "_")]
    take_biometric_photo_2 <- x[paste("take_biometric_photo", i, 2, sep = "_")]
    if(i==1){
      follow_up_1 <- x[paste(follow_ups, i, "1", sep = "_")]
      follow_up_2 <- x[paste(follow_ups, i, "2", sep = "_")]
    } else{
      follow_up_1 <- rep(NA,length(follow_ups))
      follow_up_2 <- rep(NA,length(follow_ups))
    }
    policies_1[i, ] <- as.character(c(char_1, 1, i, choice_1, choice_confirm_1, follow_up_1, 
                                      digital_id_supportive_1, likely_register_digital_id_1,
                                      connected_voter_reg_1, take_biometric_photo_1))
    policies_2[i, ] <- as.character(c(char_2, 2, i, choice_2, choice_confirm_2, follow_up_2,
                                      digital_id_supportive_2, likely_register_digital_id_2,
                                      connected_voter_reg_2, take_biometric_photo_2
                                      ))
  }
  stacked_policies <- bind_rows(policies_1, policies_2)
  policies <- data.frame(instanceID = rep(as.character(x["instanceID"]), 6),
                         stacked_policies,
                         public_service_order = rep(as.character(x["public_service_order"]), 6),
                         security_order = rep(as.character(x["security_order"]), 6),
                         tax_reg_order = rep(as.character(x["tax_reg_order"]), 6),
                         voting_order = rep(as.character(x["voting_order"]), 6),
                         social_prot_order = rep(as.character(x["social_prot_order"]), 6)
  )
  return(policies)
}

cjoint_vars_df <- NULL
for(n in c(1:nrow(d))){
  policies_n <- policyFun(d[n,], attr_names = attr_names, follow_ups = follow_ups)
  cjoint_vars_df <- bind_rows(cjoint_vars_df, policies_n)
}

cjoint_vars_df$policy_id <- paste(cjoint_vars_df$instanceID, cjoint_vars_df$round, cjoint_vars_df$policy, sep = "-")
cjoint_vars_df$respondent_round <- paste(cjoint_vars_df$instanceID, cjoint_vars_df$round, sep = "-")
cjoint_clean <- cjoint_vars_df[order(cjoint_vars_df$respondent_round), ]

rm(cjoint_vars_df, attr_names, follow_ups, manipulation, policyFun, policies_n)

cjoint_clean <- cjoint_clean %>% mutate_if(is.character, stringr::str_trim)

cjoint_clean$choice_final <- cjoint_clean$choice
cjoint_clean$choice_final[which(cjoint_clean$choice == -999)] <- cjoint_clean$choice_confirm[which(cjoint_clean$choice == -999)]

# merge in respondent characteristics

# remove one duplicate
d$duplicateID <- duplicated(d$instanceID)
#all.equal(d[which(d$instanceID == d$instanceID[d$duplicateID])[1], ], d[which(d$instanceID == d$instanceID[d$duplicateID])[2], ])
d <- d[!duplicated(d$instanceID), ]

cjoint_clean <- left_join(cjoint_clean,
                           d %>% dplyr::select(instanceID, enum_name, county, sub_county,
                                               sub_location, selected_age, selected_gender,
                                               mother_tongue, mother_tongue_oth,
                                               vote_for_president, vote_for_president_oth,
                                               trust_iebc, trust_president, trust_police,
                                               trust_courts, trust_kra, trust_religious_leaders,
                                               upcoming_president, presidential_party,
                                               edu_level, radio, computer, mobile_phone,
                                               source_income, heard_huduma_number,
                                               register_huduma_number, court_case,
                                               part_person_political_protest, party_represented,
                                               likely_vote, party_sued, party_lawsuit_case
                                              ),
                           by = "instanceID") 

#### Datasets ####

# Ethnic
cjoint_clean$mother_tongue_m <- cjoint_clean$mother_tongue %in% c(-999, -998) | is.na(cjoint_clean$mother_tongue)
cjoint_clean$mother_tongue[cjoint_clean$mother_tongue_m] <- NA

cjoint_clean$dominant <- ifelse(cjoint_clean$mother_tongue == 3 | cjoint_clean$mother_tongue == 11, 1, 0) # Kikuyu and Kalenjin
cjoint_clean$opposition <- ifelse(cjoint_clean$mother_tongue == 2, 1, 0) # Luo
cjoint_clean$securitized <- ifelse(cjoint_clean$mother_tongue == 5, 1, 0) # Somali

# Political
cjoint_clean$vote_for_president_m <- cjoint_clean$vote_for_president %in% c(-999, -998) | is.na(cjoint_clean$vote_for_president)
cjoint_clean$vote_for_president[cjoint_clean$vote_for_president_m] <- NA

cjoint_clean$political_opposition <- ifelse(cjoint_clean$vote_for_president == 2, 1, 0) # Raila Odinga

# De-mean conjoint attribute and group indicator variables

cjoint_clean <- cjoint_clean %>%
  mutate(public_service_dm = as.numeric(public_service) - mean(as.numeric(public_service), na.rm=T),
         security_dm = as.numeric(security) - mean(as.numeric(security), na.rm=T),
         tax_reg_dm = as.numeric(tax_reg) - mean(as.numeric(tax_reg), na.rm=T),
         voting_dm = as.numeric(voting) - mean(as.numeric(voting), na.rm=T),
         social_prot_dm = as.numeric(social_prot) - mean(as.numeric(social_prot), na.rm=T),
         dominant_dm = dominant - mean(dominant, na.rm = T),
         opposition_dm = opposition - mean(opposition, na.rm = T),
         securitized_dm = securitized - mean(securitized, na.rm = T),
         political_opposition_dm = political_opposition - mean(political_opposition, na.rm = T))

# Primary outcomes - drop DK and refuse
cjoint_clean$choice_final_m <- cjoint_clean$choice == -999 & cjoint_clean$choice_final == -999 # missingness indicator
cjoint_clean$choice_final <- as.numeric(cjoint_clean$choice_final)
cjoint_clean$choice_final[cjoint_clean$choice_final_m] <- NA

cjoint_clean$digital_id_supportive_m <- cjoint_clean$digital_id_supportive %in% c(-998, -999) # missingness indicator
cjoint_clean$digital_id_supportive <- as.numeric(cjoint_clean$digital_id_supportive)
cjoint_clean$digital_id_supportive[cjoint_clean$digital_id_supportive_m] <- NA

cjoint_clean$likely_register_digital_id_m <- cjoint_clean$likely_register_digital_id %in% c(-998, -999) # missingness indicator
cjoint_clean$likely_register_digital_id <- as.numeric(cjoint_clean$likely_register_digital_id) 
cjoint_clean$likely_register_digital_id[cjoint_clean$likely_register_digital_id_m] <- NA

# FIGURES Mechanisms 
## IEBC  "trust_iebc", 3 = Somewhat, 4 = A lot
cjoint_clean <- cjoint_clean %>% 
  mutate(trust_iebc_yes = trust_iebc == 3 | trust_iebc == 4) #%>% select(trust_iebc, trust_iebc_yes)

## 2022 upcoming elections "upcoming_president", Raila = 2
cjoint_clean <- cjoint_clean %>% 
  mutate(upcoming_raila_yes = upcoming_president==2) #%>% select(upcoming_president, upcoming_raila_yes)

## 2017 past elections
cjoint_clean <- cjoint_clean %>% mutate(past_odm_yes = presidential_party==10) 

## For robustness checks

# Nairobi dummy
cjoint_clean <- cjoint_clean %>%
  mutate(nairobi = ifelse(cjoint_clean$county == 1, 1, 0)) %>%
  mutate(nairobi_dm = nairobi - mean(nairobi, na.rm = T))

# Round dummies
cjoint_clean <- cjoint_clean %>%
  mutate(round1 = ifelse(round == 1, 1, 0),
         round2 = ifelse(round == 2, 1, 0),
         round3 = ifelse(round == 3, 1, 0)) %>%
  mutate(round1_dm = round1 - mean(round1, na.rm = T),
         round2_dm = round2 - mean(round2, na.rm = T),
         round3_dm = round3 - mean(round3, na.rm = T))

# Enumerator dummies
cjoint_clean_enum <- cjoint_clean %>%
  mutate(one_column = 1) %>%
  mutate(row = row_number()) %>%
  mutate(enum_name = sprintf("%02d", enum_name)) %>%
  pivot_wider(names_from = enum_name, names_prefix = "enum_",
              values_from = one_column, values_fill = 0)

enum_variable_list <- names(cjoint_clean_enum[,grepl("enum_", names(cjoint_clean_enum))])


# demean enumerator variables
for(i in 1:length(enum_variable_list)){
  variable <- enum_variable_list[i]
  enum_number <- substr(variable, 6, 7)
  vector <- data.frame(cjoint_clean_enum[,which(grepl(variable, names(cjoint_clean_enum)))])
  colnames(vector) <- c("enum_var")
  vector <- vector %>%
    mutate(enum_var = ifelse(enum_var == "NULL", 0, enum_var)) %>%
    mutate(enum_var = as.numeric(enum_var)) %>%
    mutate(dm_var = enum_var - mean(enum_var, na.rm = T)) %>%
    select(dm_var)
  
  cjoint_clean_enum <- cbind(cjoint_clean_enum, vector)
  
  names(cjoint_clean_enum)[names(cjoint_clean_enum) == "dm_var"] <- c(paste0("enum_dm_", enum_number))
}

cjoint_clean_enum_only <- cjoint_clean_enum %>% 
  dplyr::select(!!colnames(cjoint_clean_enum)[grep("enum_dm", colnames(cjoint_clean_enum))]
                )
cjoint_clean <- cbind(cjoint_clean, cjoint_clean_enum_only)

rm(enum_number, enum_variable_list, vector, variable, cjoint_clean_enum, cjoint_clean_enum_only)

# Additional covariates
cjoint_clean <- cjoint_clean %>%
  mutate(education = case_when(
  edu_level == 1 ~ "No formal schooling",
  edu_level	== 2 ~ "No formal schooling",
  edu_level	== 3 ~ "Some primary school",
  edu_level	== 4 ~ "Primary school completed",
  edu_level	== 5 ~ "Primary school completed",
  edu_level	== 6 ~ "Secondary school completed",
  edu_level	== 7 ~ "Post-secondary qualifications other than university",
  edu_level	== 8 ~ "At least some university",
  edu_level	== 9 ~ "At least some university",
  edu_level	== 10 ~ "At least some university",
  edu_level == -999 ~ "Refused to answer"
)) %>%
  mutate(no_formal_schooling = ifelse(education == "No formal schooling", 1, 0)) %>%
  mutate(some_primary_school = ifelse(education == "Some primary school", 1, 0)) %>%
  mutate(primary_school_completed = ifelse(education == "Primary school completed", 1, 0)) %>%
  mutate(secondary_school_completed = ifelse(education == "Secondary school completed", 1, 0)) %>%
  mutate(post_secondary_other_than_uni = ifelse(education == "Post-secondary qualifications other than university", 1, 0)) %>%
  mutate(at_least_some_university = ifelse(education == "At least some university", 1, 0)) %>%
  mutate(education_refused = ifelse(education == "Refused to answer", 1, 0)) %>%
  # create continuous education variable
  mutate(edu_level_cont = ifelse(edu_level == -999, NA, edu_level)) %>%
  # edit other control variables 
  mutate(radio_b = ifelse(radio == 1, 1, ifelse(radio == 2, 0, NA)),
         computer_b = ifelse(computer == 1, 1, ifelse(computer == 2, 0, NA)),
         mobile_phone_b = ifelse(mobile_phone == 1, 1, ifelse(mobile_phone == 2, 0, NA)),
         source_income_b = ifelse(source_income == 1, 1, ifelse(source_income == 2, 0, NA))) %>%
  # demean variables
  mutate(no_formal_schooling_dm = no_formal_schooling - mean(no_formal_schooling, na.rm = T),
         some_primary_school_dm = some_primary_school - mean(some_primary_school, na.rm = T),
         primary_school_completed_dm = primary_school_completed - mean(primary_school_completed, na.rm = T),
         secondary_school_completed_dm = secondary_school_completed - mean(secondary_school_completed, na.rm = T),
         post_secondary_other_than_uni_dm = post_secondary_other_than_uni - mean(post_secondary_other_than_uni, na.rm = T),
         at_least_some_university_dm = at_least_some_university - mean(at_least_some_university, na.rm = T),
         education_refused_dm = education_refused - mean(education_refused, na.rm = T),
         edu_level_cont_dm = edu_level_cont - mean(edu_level_cont, na.rm = T),
         radio_dm = radio_b - mean(radio_b, na.rm = T),
         computer_dm = computer_b - mean(computer_b, na.rm = T),
         mobile_phone_dm = mobile_phone_b - mean(mobile_phone_b, na.rm = T),
         source_income_dm = source_income_b - mean(source_income_b, na.rm = T)
  )

cjoint_clean <- cjoint_clean  %>% 
  mutate(hs_cat = case_when(
    edu_level == 1 | edu_level	== 2 | edu_level	== 3 | edu_level	== 4 | edu_level	== 5 ~ "Below Secondary Education",
    edu_level == 6 | edu_level	== 7 | edu_level	== 8 | edu_level	== 9 | edu_level	== 10   ~ "Secondary Education and above",
    edu_level == -999 ~ "Refused to answer"))

cjoint_clean <- cjoint_clean %>% 
  mutate(below_hs_b = ifelse(hs_cat == "Below Secondary Education", 1, 0), 
         hs_above_b = ifelse(hs_cat == "Secondary Education and above", 1, 0),
         hs_na_b = ifelse(hs_cat == "Refused to answer", 1, 0)) %>% 
  mutate(below_hs_dm = below_hs_b - mean(below_hs_b), 
         hs_above_dm = hs_above_b - mean(hs_above_b),
         hs_na_dm = hs_na_b - mean(hs_na_b))

cjoint_clean$hs_cat <-  factor(cjoint_clean$hs_cat, 
                                  levels = c("Refused to answer",
                                             "Below Secondary Education", 
                                             "Secondary Education and above"))
cjoint_clean <- cjoint_clean  %>% 
  mutate(post_hs_cat = case_when(
    edu_level == 1 | edu_level	== 2 | edu_level	== 3 | edu_level	== 4 | edu_level	== 5 | edu_level == 6 ~ "Up to Secondary School",
    edu_level	== 7 | edu_level	== 8 | edu_level	== 9 | edu_level	== 10   ~ "Post Secondary School",
    edu_level == -999 ~ "Refused to answer")) 

cjoint_clean <- cjoint_clean %>% 
  mutate(up_to_hs_b = ifelse(post_hs_cat == "Up to Secondary School", 1, 0), 
         post_hs_b = ifelse(post_hs_cat == "Post Secondary School", 1, 0),
         post_hs_na_b = ifelse(post_hs_cat == "Refused to answer", 1, 0)) %>% 
  mutate(up_to_hs_dm = up_to_hs_b - mean(up_to_hs_b), 
         post_hs_dm = post_hs_b - mean(post_hs_b),
         post_hs_na_dm = post_hs_na_b - mean(post_hs_na_b))

cjoint_clean$hs_cat <-  factor(cjoint_clean$hs_cat, 
                                  levels = c("Refused to answer",
                                             "Up to Secondary School", 
                                             "Post Secondary School"))

cjoint_clean <-  cjoint_clean %>% 
  mutate(radio_index = ifelse(radio == 1, 1, 0), 
         computer_index = ifelse(computer == 1, 1, 0),
         mobile_phone_index = ifelse(mobile_phone == 1, 1, 0)) %>% 
  mutate(dev_ownership = radio_index + mobile_phone_index + computer_index) %>% 
  mutate(dev_ownership_cat = case_when(
    dev_ownership == 0 ~ "No devices stated",
    dev_ownership == 1 ~ "Owns 1 device" , 
    dev_ownership == 2 ~ "Owns 2 devices" ,
    dev_ownership == 3 ~ "Owns 3 devices")) %>% 
  mutate(no_devices_stated = ifelse(dev_ownership == 0, 1, 0), 
         one_device = ifelse(dev_ownership == 1, 1, 0),
         two_devices = ifelse(dev_ownership == 2, 1, 0),
         three_devices = ifelse(dev_ownership == 3, 1, 0)) %>% 
  mutate(no_devices_stated_dm = no_devices_stated - mean(no_devices_stated), 
         one_device_dm = one_device - mean(one_device),
         two_devices_dm = two_devices - mean(two_devices),
         three_devices_dm = three_devices - mean(three_devices)) %>% 
  mutate(dev_ownership_dm = dev_ownership - mean(dev_ownership))

cjoint_clean$dev_ownership_cat <-  factor(cjoint_clean$dev_ownership_cat, 
                                             levels = c("No devices stated", 
                                                        "Owns 1 device",
                                                        "Owns 2 devices",
                                                        "Owns 3 devices"))

cjoint_clean <- cjoint_clean  %>% 
  mutate(source_income_cat = case_when(
    source_income == 1  ~ "Source income",
    source_income == 2  ~ "No source income",
    source_income == -998 | source_income == -999 ~ "Don't know/No Answer")) 
cjoint_clean <- cjoint_clean %>% 
  mutate(source_income_b = ifelse(source_income_cat == "Source income", 1, 0), 
         no_source_income_b = ifelse(source_income_cat == "No source income", 1, 0), 
         source_income_na_b = ifelse(source_income_cat == "Don't know/No Answer", 1, 0)) %>% 
  mutate(source_income_dm = source_income_b - mean(source_income_b), 
         no_source_income_dm = no_source_income_b - mean(no_source_income_b), 
         source_income_na_dm = source_income_na_b - mean(source_income_na_b))

cjoint_clean$source_income_cat <-  factor(cjoint_clean$source_income_cat, 
                                             levels = c("Don't know/No Answer", 
                                                        "Source income", 
                                                        "No source income"))


# variables for iebc factor & continuous
cjoint_clean <- cjoint_clean  %>% 
  mutate(trust_iebc_cat = case_when(
    trust_iebc == 1 | trust_iebc == 2 ~ "Don't trust IEBC",
    trust_iebc == 3 | trust_iebc == 4 ~ "Trust IEBC",
    trust_iebc == -998 | trust_iebc == -999 ~ "Don't know/No Answer")) %>% 
  mutate(trust_iebc_b = ifelse(trust_iebc_cat == "Trust IEBC", 1, 0), 
         no_trust_iebc_b = ifelse(trust_iebc_cat == "Don't trust IEBC", 1, 0), 
         trust_iebc_na_b = ifelse(trust_iebc_cat == "Don't know/No Answer", 1, 0)) %>% 
  mutate(trust_iebc_dm = trust_iebc_b - mean(trust_iebc_b), 
         no_trust_iebc_dm = no_trust_iebc_b - mean(no_trust_iebc_b), 
         trust_iebc_na_dm = trust_iebc_na_b - mean(trust_iebc_na_b)) %>% 
  mutate(trust_iebc_cont = ifelse(trust_iebc == -998 | trust_iebc == -999, NA, trust_iebc),
         trust_iebc_cont_dm = trust_iebc_cont - mean(trust_iebc_cont, na.rm = T))

cjoint_clean$trust_iebc_cat <-  factor(cjoint_clean$trust_iebc_cat, 
                                          levels = c("Don't know/No Answer", 
                                                     "Trust IEBC", 
                                                     "Don't trust IEBC"))
# variables for president  
cjoint_clean <- cjoint_clean  %>% 
  mutate(trust_president_cat = case_when(
    trust_president == 1 | trust_president == 2 ~ "Don't trust president",
    trust_president == 3 | trust_president == 4 ~ "Trust president",
    trust_president == -998 | trust_president == -999 ~ "Don't know/No Answer")) %>% 
  mutate(trust_president_b = ifelse(trust_president_cat == "Trust president", 1, 0), 
         no_trust_president_b = ifelse(trust_president_cat == "Don't trust president", 1, 0), 
         trust_president_na_b = ifelse(trust_president_cat == "Don't know/No Answer", 1, 0)) %>% 
  mutate(trust_president_dm = trust_president_b - mean(trust_president_b), 
         no_trust_president_dm = no_trust_president_b - mean(no_trust_president_b), 
         trust_president_na_dm = trust_president_na_b - mean(trust_president_na_b)) %>% 
  mutate(trust_president_cont = ifelse(trust_president == -998 | trust_president == -999, NA, trust_president),
         trust_president_cont_dm = trust_president_cont - mean(trust_president_cont, na.rm = T))

cjoint_clean$trust_president_cat <-  factor(cjoint_clean$trust_president_cat, 
                                               levels = c("Don't know/No Answer", 
                                                          "Trust president", 
                                                          "Don't trust president"))
##### variables for police
cjoint_clean <- cjoint_clean  %>% 
  mutate(trust_police_cat = case_when(
    trust_police == 1 | trust_police == 2 ~ "Don't trust police",
    trust_police == 3 | trust_police == 4 ~ "Trust police",
    trust_police == -998 | trust_police == -999 ~ "Don't know/No Answer")) %>% 
  mutate(trust_police_b = ifelse(trust_police_cat == "Trust police", 1, 0), 
         no_trust_police_b = ifelse(trust_police_cat == "Don't trust police", 1, 0), 
         trust_police_na_b = ifelse(trust_police_cat == "Don't know/No Answer", 1, 0)) %>% 
  mutate(trust_police_dm = trust_police_b - mean(trust_police_b), 
         no_trust_police_dm = no_trust_police_b - mean(no_trust_police_b), 
         trust_police_na_dm = trust_police_na_b - mean(trust_police_na_b)) %>% 
  mutate(trust_police_cont = ifelse(trust_police == -998 | trust_police == -999, NA, trust_police),
         trust_police_cont_dm = trust_police_cont - mean(trust_police_cont, na.rm = T))

cjoint_clean$trust_police_cat <-  factor(cjoint_clean$trust_police_cat, 
                                            levels = c("Don't know/No Answer", 
                                                       "Trust police", 
                                                       "Don't trust police"))

# variables for courts
cjoint_clean <- cjoint_clean  %>% 
  mutate(trust_courts_cat = case_when(
    trust_courts == 1 | trust_courts == 2 ~ "Don't trust courts",
    trust_courts == 3 | trust_courts == 4 ~ "Trust courts",
    trust_courts == -998 | trust_courts == -999 ~ "Don't know/No Answer")) %>% 
  mutate(trust_courts_b = ifelse(trust_courts_cat == "Trust courts", 1, 0), 
         no_trust_courts_b = ifelse(trust_courts_cat == "Don't trust courts", 1, 0), 
         trust_courts_na_b = ifelse(trust_courts_cat == "Don't know/No Answer", 1, 0)) %>% 
  mutate(trust_courts_dm = trust_courts_b - mean(trust_courts_b), 
         no_trust_courts_dm = no_trust_courts_b - mean(no_trust_courts_b), 
         trust_courts_na_dm = trust_courts_na_b - mean(trust_courts_na_b)) %>% 
  mutate(trust_courts_cont = ifelse(trust_courts == -998 | trust_courts == -999, NA, trust_courts),
         trust_courts_cont_dm = trust_courts_cont - mean(trust_courts_cont, na.rm = T))

cjoint_clean$trust_courts_cat <-  factor(cjoint_clean$trust_courts_cat, 
                                            levels = c("Don't know/No Answer", 
                                                       "Trust courts", 
                                                       "Don't trust courts"))
# variables for  kra
cjoint_clean <- cjoint_clean  %>% 
  mutate(trust_kra_cat = case_when(
    trust_kra == 1 | trust_kra == 2 ~ "Don't trust kra",
    trust_kra == 3 | trust_kra == 4 ~ "Trust kra",
    trust_kra == -998 | trust_kra == -999 ~ "Don't know/No Answer")) %>% 
  mutate(trust_kra_b = ifelse(trust_kra_cat == "Trust kra", 1, 0), 
         no_trust_kra_b = ifelse(trust_kra_cat == "Don't trust kra", 1, 0), 
         trust_kra_na_b = ifelse(trust_kra_cat == "Don't know/No Answer", 1, 0)) %>% 
  mutate(trust_kra_dm = trust_kra_b - mean(trust_kra_b), 
         no_trust_kra_dm = no_trust_kra_b - mean(no_trust_kra_b), 
         trust_kra_na_dm = trust_kra_na_b - mean(trust_kra_na_b)) %>% 
  mutate(trust_kra_cont = ifelse(trust_kra == -998 | trust_kra == -999, NA, trust_kra),
         trust_kra_cont_dm = trust_kra_cont - mean(trust_kra_cont, na.rm = T))

cjoint_clean$trust_kra_cat <-  factor(cjoint_clean$trust_kra_cat, 
                                         levels = c("Don't know/No Answer", 
                                                    "Trust kra", 
                                                    "Don't trust kra"))
# variables for religious leaders
cjoint_clean <- cjoint_clean  %>% 
  mutate(trust_religious_leaders_cat = case_when(
    trust_religious_leaders == 1 | trust_religious_leaders == 2 ~ "Don't trust religious leaders",
    trust_religious_leaders == 3 | trust_religious_leaders == 4 ~ "Trust religious leaders",
    trust_religious_leaders == -998 | trust_religious_leaders == -999 ~ "Don't know/No Answer")) %>% 
  mutate(trust_religious_leaders_b = ifelse(trust_religious_leaders_cat == "Trust religious leaders", 1, 0), 
         no_trust_religious_leaders_b = ifelse(trust_religious_leaders_cat == "Don't trust religious leaders", 1, 0), 
         trust_religious_leaders_na_b = ifelse(trust_religious_leaders_cat == "Don't know/No Answer", 1, 0)) %>% 
  mutate(trust_religious_leaders_dm = trust_religious_leaders_b - mean(trust_religious_leaders_b), 
         no_trust_religious_leaders_dm = no_trust_religious_leaders_b - mean(no_trust_religious_leaders_b), 
         trust_religious_leaders_na_dm = trust_religious_leaders_na_b - mean(trust_religious_leaders_na_b)) %>% 
  mutate(trust_religious_leaders_cont = ifelse(trust_religious_leaders == -998 | trust_religious_leaders == -999, NA, trust_religious_leaders),
         trust_religious_leaders_cont_dm = trust_religious_leaders_cont - mean(trust_religious_leaders_cont, na.rm = T))

cjoint_clean$trust_religious_leaders_cat <-  factor(cjoint_clean$trust_religious_leaders_cat, 
                                                       levels = c("Don't know/No Answer", 
                                                                  "Trust religious leaders", 
                                                                  "Don't trust religious leaders"))

## Trust index and Trust all as IV: president, IEBC, police, courts, KRA, religious leaders 
cjoint_clean <- cjoint_clean  %>% 
  mutate(trust_president_i = ifelse(trust_president == -998 | trust_president == -999, 0, trust_president), 
         trust_iebc_i = ifelse(trust_iebc == -998 | trust_iebc == -999, 0, trust_iebc),
         trust_police_i = ifelse(trust_police == -998 | trust_police == -999, 0, trust_police), 
         trust_courts_i = ifelse(trust_courts == -998 | trust_courts == -999, 0, trust_courts), 
         trust_kra_i = ifelse(trust_kra == -998 | trust_kra == -999, 0, trust_kra),
         trust_religious_leaders_i = ifelse(trust_religious_leaders == -998 | trust_religious_leaders == -999, 0, trust_religious_leaders)) %>% 
  mutate(trust_index_cont = trust_president_i + trust_police_i + trust_iebc_i + trust_courts_i + trust_kra_i,
         trust_all_index_cont = trust_president_i + trust_police_i + trust_iebc_i + trust_courts_i + trust_kra_i + trust_religious_leaders_i) %>% 
  mutate(trust_index_cat = case_when(
    trust_index_cont >= 0 & trust_index_cont <= 10 ~ "No trust",
    trust_index_cont >= 11 & trust_index_cont <= 20 ~ "Trust"),
    trust_all_index_cat = case_when(
      trust_all_index_cont >= 0 & trust_all_index_cont <= 12 ~ "No trust",
      trust_all_index_cont >= 13 & trust_all_index_cont <= 24 ~ "Trust")) %>% 
  mutate(trust_index_b = ifelse(trust_index_cat == "Trust", 1, 0), 
         no_trust_index_b = ifelse(trust_index_cat == "No trust", 1, 0), 
         trust_all_index_b = ifelse(trust_all_index_cat == "Trust", 1, 0), 
         no_trust_all_index_b = ifelse(trust_all_index_cat == "No trust", 1, 0)) %>% 
  mutate(trust_index_dm = trust_index_b - mean(trust_index_b), 
         no_trust_index_dm = no_trust_index_b - mean(no_trust_index_b), 
         trust_all_index_dm = trust_all_index_b - mean(trust_all_index_b), 
         no_trust_all_index_dm = no_trust_all_index_b - mean(no_trust_all_index_b)) %>% 
  mutate(trust_index_cont = ifelse(trust_index_cont == 0, NA, trust_index_cont),
         trust_index_cont_dm = trust_index_cont - mean(trust_index_cont, na.rm = T),
         trust_all_index_cont = ifelse(trust_all_index_cont == 0, NA, trust_all_index_cont),
         trust_all_index_cont_dm = trust_all_index_cont - mean(trust_all_index_cont, na.rm = T)) 
cjoint_clean$trust_index_cat <-  factor(cjoint_clean$trust_index_cat, 
                                           levels = c("Trust", 
                                                      "No trust"))
cjoint_clean$trust_all_index_cat <-  factor(cjoint_clean$trust_all_index_cat, 
                                               levels = c("Trust", 
                                                          "No trust"))

#### Participation in protest
cjoint_clean <- cjoint_clean  %>% 
  mutate(pol_protest = ifelse(part_person_political_protest ==1, 1, ifelse(part_person_political_protest==2, 0, NA))) %>% 
  mutate(pol_protest_cat = case_when(
    part_person_political_protest == 1  ~ "Protest",
    part_person_political_protest == 2  ~ "No protest",
    part_person_political_protest == -998 | part_person_political_protest == -999 ~ "Don't know/No Answer")) %>% 
  mutate(pol_protest_b = ifelse(pol_protest_cat == "Protest", 1, 0), 
         no_pol_protest_b = ifelse(pol_protest_cat == "No protest", 1, 0), 
         pol_protest_na_b = ifelse(pol_protest_cat == "Don't know/No Answer", 1, 0)) %>% 
  mutate(pol_protest_dm = pol_protest_b - mean(pol_protest_b), 
         pol_protest_dm1 = pol_protest - mean(pol_protest, na.rm = T),
         no_pol_protest_dm = no_pol_protest_b - mean(no_pol_protest_b), 
         pol_protest_na_dm = pol_protest_na_b - mean(pol_protest_na_b))
cjoint_clean$pol_protest_cat <-  factor(cjoint_clean$pol_protest_cat, 
                                           levels = c("Don't know/No Answer", 
                                                      "Protest", 
                                                      "No protest"))


#### HUDUMA NAMBA
cjoint_clean <- cjoint_clean %>% 
  mutate(register_huduma_namba = ifelse(register_huduma_number == 1, 1, 0),
         court_case_huduma = ifelse(court_case == 1, 1, 0),
         represented_jub_party = ifelse(party_represented == 1, 1, 0),
         vote_likely = ifelse(likely_vote == -999 |likely_vote == -998, NA, likely_vote))%>% 
  mutate(register_huduma_namba_dm = register_huduma_namba - mean(register_huduma_namba, na.rm = T),
         court_case_huduma_dm = court_case_huduma - mean(court_case_huduma, na.rm = T),
         represented_jub_party_dm = represented_jub_party - mean(represented_jub_party, na.rm = T),
         vote_likely_dm = vote_likely - mean(vote_likely, na.rm = T),
         past_odm_yes_dm = past_odm_yes - mean(past_odm_yes, na.rm = T))


## clean respondent-level dataset for summary stats, etc

d$mother_tongue_m <- d$mother_tongue %in% c(-999, -998) | is.na(d$mother_tongue)
d$mother_tongue[d$mother_tongue_m] <- NA

d$dominant <- ifelse(d$mother_tongue == 3 | d$mother_tongue == 11, 1, 0) # Kikuyu and Kalenjin
d$opposition <- ifelse(d$mother_tongue == 2, 1, 0) # Luo
d$securitized <- ifelse(d$mother_tongue == 5, 1, 0) # Somali

d$Subgroup <- ifelse(d$dominant == 1, "Dominant", 
                     ifelse(d$opposition == 1, "Opposition",
                            ifelse(d$securitized == 1, "Securitized", NA)))

d$q_55_1 <- d$q_55
d$q_56_1 <- d$q_56
d$q_57_1 <- d$q_57
d$q_58_1 <- d$q_58
d$q_59_1 <- d$q_59

d <- d %>%
  mutate(q_55 = case_when(
    randnum_order == 1 ~ q_55_1,
    randnum_order == 2 ~ q_55_2,
    randnum_order == 3 ~ q_55_3,
    randnum_order == 4 ~ q_55_4,
    randnum_order == 5 ~ q_55_5,
  )) %>%
  mutate(q_56 = case_when(
    randnum_order == 1 ~ q_56_1,
    randnum_order == 2 ~ q_56_2,
    randnum_order == 3 ~ q_56_3,
    randnum_order == 4 ~ q_56_4,
    randnum_order == 5 ~ q_56_5,
  )) %>%
  mutate(q_57 = case_when(
    randnum_order == 1 ~ q_57_1,
    randnum_order == 2 ~ q_57_2,
    randnum_order == 3 ~ q_57_3,
    randnum_order == 4 ~ q_57_4,
    randnum_order == 5 ~ q_57_5,
  )) %>%
  mutate(q_58 = case_when(
    randnum_order == 1 ~ q_58_1,
    randnum_order == 2 ~ q_58_2,
    randnum_order == 3 ~ q_58_3,
    randnum_order == 4 ~ q_58_4,
    randnum_order == 5 ~ q_58_5,
  )) %>%
  mutate(q_59 = case_when(
    randnum_order == 1 ~ q_59_1,
    randnum_order == 2 ~ q_59_2,
    randnum_order == 3 ~ q_59_3,
    randnum_order == 4 ~ q_59_4,
    randnum_order == 5 ~ q_59_5,
  ))

# opposition and voting
d <- d %>%
  mutate(upcoming_raila_yes = ifelse(upcoming_president==2, 1, 0)) %>%
  mutate(trust_iebc_yes = ifelse(trust_iebc==3 | trust_iebc == 4, 1, 0)) %>%
  mutate(past_odm_yes = ifelse(presidential_party==10,1,0)) %>%
  mutate(q_59 = ifelse(q_59 == -998 | q_59 == -999, NA, q_59))

# Nairobi sample (for descriptive statistics)
nairobi <- d %>% filter(county == 1)

## regions and ethnicity
d$region[d$county == 1] <-	"Nairobi"
d$region[d$county == 2] <-	"Kirinyaga"
d$region[d$county == 3] <-	"Murang'a"
d$region[d$county == 4] <-	"Nyandarua"
d$region[d$county == 5] <-	"Nyeri"
d$region[d$county == 6] <-	"Homa Bay"
d$region[d$county == 7] <-	"Kisumu"
d$region[d$county == 8] <-	"Migori"
d$region[d$county == 9] <-	"Siaya"
d$region[d$county == 10] <-	"Garissa"

d <- d %>%
  mutate(language = case_when(
    mother_tongue == "1" ~ "English", 
    mother_tongue == "2" ~ "Luo", 
    mother_tongue == "3" ~ "Kalenjin",
    mother_tongue == "4" ~ "Maasai",
    mother_tongue == "5" ~ "Somali",
    mother_tongue == "6" ~ "Swahili",
    mother_tongue == "7" ~ "Luhya",
    mother_tongue == "8" ~ "Kisii",
    mother_tongue == "9" ~ "Mijikenda",
    mother_tongue == "10" ~ "Pokot", 
    mother_tongue == "11" ~ "Kikuyu", 
    mother_tongue == "12" ~ "Kamba", 
    mother_tongue == "13" ~ "Meru", 
    mother_tongue == "14" ~ "Taita", 
    mother_tongue == "15" ~ "Turkana",
    mother_tongue == "16" ~ "Don't know",
    mother_tongue == "97" ~ "Other",
    mother_tongue == "-999" ~ "Refused"))

# descriptive stats summary table
d <- d %>%
  mutate(gender = case_when(
    selected_gender == 1 ~ "Male",
    selected_gender == 2 ~ "Female")) %>%
  mutate(female = ifelse(selected_gender == 2, 1, 0)) %>%
  mutate(voted_last_election = case_when(
    presidential_party == 0 ~ "No",
    presidential_party > 0 ~"Yes")) %>%
  mutate(voted_binary = ifelse(presidential_party > 0, 1, 0)) %>%
  mutate(education = case_when(
    edu_level == 1 ~ "No formal schooling",
    edu_level	== 2 ~ "No formal schooling",
    edu_level	== 3 ~ "Some primary school",
    edu_level	== 4 ~ "Primary school completed",
    edu_level	== 5 ~ "Primary school completed",
    edu_level	== 6 ~ "Secondary school completed",
    edu_level	== 7 ~ "Post-secondary qualifications other than university",
    edu_level	== 8 ~ "At least some university",
    edu_level	== 9 ~ "At least some university",
    edu_level	== 10 ~ "At least some university",
    edu_level == -999 ~ "Refused to answer")) %>%
  mutate(no_formal_schooling = ifelse(education == "No formal schooling", 1, 0)) %>%
  mutate(some_primary_school = ifelse(education == "Some primary school", 1, 0)) %>%
  mutate(primary_school_completed = ifelse(education == "Primary school completed", 1, 0)) %>%
  mutate(secondary_school_completed = ifelse(education == "Secondary school completed", 1, 0)) %>%
  mutate(post_secondary_other_than_uni = ifelse(education == "Post-secondary qualifications other than university", 1, 0)) %>%
  mutate(at_least_some_university = ifelse(education == "At least some university", 1, 0)) %>%
  mutate(education_refused = ifelse(education == "Refused to answer", 1, 0)) %>%
  mutate(tongue = case_when(
    mother_tongue	== 2 ~ "Luo",
    mother_tongue	== 3 ~	"Kalenjin",
    mother_tongue	== 5 ~ "Somali",
    mother_tongue	== 11 ~ "Kikuyu",
    #mother_tongue == 7 ~ "Luhya",
    #mother_tongue == 8 ~ "Kisii",
    #mother_tongue == 12 ~ "Kamba",
    #mother_tongue == 13 ~ "Meru/Embu",
    mother_tongue	> 0	~ "Other"))%>%
  mutate(luo = ifelse(mother_tongue == 2, 1, 0)) %>%
  mutate(kalenjin = ifelse(mother_tongue == 3, 1, 0)) %>%
  mutate(somali = ifelse(mother_tongue == 5, 1, 0)) %>%
  mutate(kikuyu = ifelse(mother_tongue == 11, 1, 0)) %>%
  mutate(language_refused = ifelse(mother_tongue == -999, 1, 0)) %>%
  mutate(other_language = ifelse(mother_tongue > 0 & mother_tongue != 2 & mother_tongue != 3 &
                                   mother_tongue != 5 & mother_tongue != 11, 1, 0)) %>%
  mutate(age = case_when(
    selected_age < 30 ~ "18-29",
    selected_age < 40 ~ "30-39",
    selected_age < 50 ~ "40-49",
    selected_age < 60 ~ "50-59",
    selected_age >=60 ~ "60+"))%>%
  mutate(religion_stat = case_when(
    religion == 1 ~ "Christian",
    religion == 2 ~ "Muslim",
    #religion == 3 ~ "Traditional/ethnic religion",
    religion == 3 ~ "Other",
    religion == 4 ~ "No religion",
    religion == 97 ~ "Other")) %>%
  mutate(christian = ifelse(religion == 1, 1, 0)) %>%
  mutate(muslim = ifelse(religion == 2, 1, 0)) %>%
  mutate(no_religion = ifelse(religion == 4, 1, 0)) %>%
  mutate(religion_refused = ifelse(religion == -999, 1, 0)) %>%
  mutate(hear_huduma_namba = ifelse(heard_huduma_number == 1, 1, 0)) %>%
  mutate(nairobi = ifelse(county == 1, 1, 0),
         kirinyaga = ifelse(county == 2, 1, 0),
         muranga = ifelse(county == 3, 1, 0),
         nyandarua = ifelse(county == 4, 1, 0),
         nyeri = ifelse(county == 5, 1, 0),
         homabay = ifelse(county == 6, 1, 0),
         kisumu = ifelse(county == 7, 1, 0),
         migori = ifelse(county == 8, 1, 0),
         siaya = ifelse(county == 9, 1, 0),
         garissa = ifelse(county == 10, 1, 0))

# General support and likelihood to register
cjoint_clean <- cjoint_clean %>%
  mutate(ethnic_group = ifelse(dominant == 1, "dominant",
                               ifelse(opposition == 1, "opposition",
                                      ifelse(securitized == 1, "securitized",
                                             ifelse(dominant == 0 & opposition == 0 & securitized == 0, "other", NA)))))

# Incorporate additional variables for balance table 
cjoint_clean <- left_join(cjoint_clean,
                           d %>% dplyr::select(instanceID, female, tongue, region, religion_stat, voted_binary, hear_huduma_namba),
                           by = "instanceID") 

cjoint_clean <- cjoint_clean %>%
  mutate(religion = ifelse(is.na(religion_stat), "Other", religion_stat)) %>%
  mutate(religion = factor(religion, levels = c("Other","Christian","Muslim","No religion"))) %>%
  mutate(education = factor(education, levels = c("No formal schooling","Some primary school","Primary school completed",
                                                  "Secondary school completed","Post-secondary qualifications other than university",
                                                  "At least some university"))) %>%
  mutate(tongue = factor(tongue, levels = c("Other", "Kalenjin","Kikuyu","Luo",
                                            "Somali")))

cjoint_clean <- cjoint_clean %>%
  mutate(other = ifelse(dominant == 0 & opposition == 0 & securitized == 0 & mother_tongue_m == FALSE, 1, 
                        ifelse(mother_tongue_m == FALSE, 0, NA))) %>%
  mutate(other_dm = other - mean(other, na.rm = T))

# For marginal means plots

## Clean up level and attribute labels
cjoint_clean <- cjoint_clean %>%
  mutate(public_service_f = factor(public_service, levels = c(1, 0), labels = c("Data sharing to improve services", "No data sharing to improve services")),
         social_prot_f = factor(social_prot, levels = c(1, 0), labels = c("eID linked to social transfers", "eID not linked to social transfers")),
         security_f = factor(security, levels = c(1, 0), labels = c("Security services access data", "Security services need consent")),
         tax_reg_f = factor(tax_reg, levels = c(1, 0), labels = c("eID linked to tax ID", "eID not linked to tax ID")),
         voting_f = factor(voting, levels = c(1, 0), labels = c("eID required to vote", "eID not required to vote")))

attr(cjoint_clean$public_service_f, "label") <- "Public Services"
attr(cjoint_clean$social_prot_f, "label") <- "Social Protection"
attr(cjoint_clean$security_f, "label") <- "Surveillance"
attr(cjoint_clean$tax_reg_f, "label") <- "Tax Registration"
attr(cjoint_clean$voting_f, "label") <- "Voting"

## create group factor variable
cjoint_clean$group <- "other"
cjoint_clean$group[which(cjoint_clean$dominant == 1)] <- "dominant"
cjoint_clean$group[which(cjoint_clean$opposition == 1)] <- "opposition"
cjoint_clean$group[which(cjoint_clean$securitized == 1)] <- "securitized"
cjoint_clean$group <- factor(cjoint_clean$group, levels = c("dominant", "opposition", "securitized", "other"))

# Secondary Outcomes
cjoint_clean_r1 <- cjoint_clean[cjoint_clean$round == 1, ]

## drop DK and refuse
cjoint_clean_r1$easy_access_govt_services_m <- cjoint_clean_r1$easy_access_govt_services %in% c(-999, -998)
cjoint_clean_r1$easy_access_govt_services[cjoint_clean_r1$easy_access_govt_services_m] <- NA

cjoint_clean_r1$adequate_data_privacy_m <- cjoint_clean_r1$adequate_data_privacy %in% c(-999, -998)
cjoint_clean_r1$adequate_data_privacy[cjoint_clean_r1$adequate_data_privacy_m] <- NA

cjoint_clean_r1$worry_punished_view_m <- cjoint_clean_r1$worry_punished_view %in% c(-999, -998)
cjoint_clean_r1$worry_punished_view[cjoint_clean_r1$worry_punished_view_m] <- NA

cjoint_clean_r1$worry_vote_counted_m <- cjoint_clean_r1$worry_vote_counted %in% c(-999, -998)
cjoint_clean_r1$worry_vote_counted[cjoint_clean_r1$worry_vote_counted_m] <- NA

cjoint_clean_r1$worry_personal_info_m <- cjoint_clean_r1$worry_personal_info %in% c(-999, -998)
cjoint_clean_r1$worry_personal_info[cjoint_clean_r1$worry_personal_info_m] <- NA

cjoint_clean_r1 <- cjoint_clean_r1 %>% 
  mutate(
    easy_access_govt_services = as.numeric(easy_access_govt_services), 
    adequate_data_privacy = as.numeric(adequate_data_privacy), 
    worry_punished_view = as.numeric(worry_punished_view), 
    worry_vote_counted = as.numeric(worry_vote_counted), 
    worry_personal_info = as.numeric(worry_personal_info))

## demean

cjoint_clean_r1 <- cjoint_clean_r1 %>%
  mutate(nairobi_dm = nairobi - mean(nairobi, na.rm = T))

### Create subset for heterogeneity analysis

cjoint_clean_sub <- cjoint_clean %>%
  dplyr::select(instanceID, public_service, security, tax_reg, voting,
                social_prot, round, choice_final, county, selected_age, 
                selected_gender, political_opposition, education, 
                edu_level_cont, radio_b, computer_b, mobile_phone_b, source_income_b,
                trust_iebc, trust_president, trust_police, trust_courts, trust_kra, trust_religious_leaders,
                dominant, opposition, securitized, register_huduma_number)
# for now - delete all instance IDs that didn't do all profiles
missing_IDs <- c(unique(cjoint_clean_sub$instanceID[is.na(cjoint_clean_sub$choice_final)]), "uuid:a611022e-3e3b-4a69-9f5b-749fc1af9a5a")
cjoint_clean_sub <- cjoint_clean_sub %>%
  #filter(!is.na(choice_final)) %>%
  filter(!instanceID %in% missing_IDs) %>%
  # filter(!is.na(dominant) & !is.na(selected_age) & !instanceID %in% missing_IDs) %>%
  mutate(public_service = factor(public_service, levels = c("0", "1"), labels = c("Do not share to improve PS", "Share to improve PS")),
         security = factor(security, levels = c("0", "1"), labels = c("Sec services need consent", "Sec services access data")),
         tax_reg = factor(tax_reg, levels = c("0", "1"), labels = c("Not linked to tax ID", "Linked to tax ID")),
         voting = factor(voting, levels = c("0", "1"), labels = c("eID not required to vote", "eID required to vote")),
         social_prot = factor(social_prot, levels = c("0", "1"), labels = c("Not linked to transfers", "Linked to transfers")),
         dominant = case_when(dominant == 1 ~ "Yes",
                              dominant == 0 ~ "No",
                              is.na(dominant) ~ "DK/Ref"),
         opposition = case_when(opposition == 1 ~ "Yes",
                                opposition == 0 ~ "No",
                                is.na(opposition) ~ "DK/Ref"),
         securitized = case_when(securitized == 1 ~ "Yes",
                                 securitized == 0 ~ "No",
                                 is.na(securitized) ~ "DK/Ref"),
         source_income = factor(source_income_b, levels = c(1, 0), labels = c("Yes", "No")),
         gender = case_when(selected_gender == 1 ~ "Male",
                            selected_gender == 2 ~ "Female",
                            is.na(selected_gender) ~ "Not provided"),
         age = case_when(selected_age %in% c(18:25) ~ "18-25",
                         selected_age %in% c(26:35) ~ "26-35",
                         selected_age %in% c(36:45) ~ "36-45",
                         selected_age %in% c(46:55) ~ "46-55",
                         selected_age %in% c(56:65) ~ "56-65",
                         selected_age > 65 ~ "65+",
                         is.na(selected_age) ~ "Not provided"
         ),
         nairobi = ifelse(county == 1, "Yes", "No"),
         political_opposition = case_when(political_opposition == 1 ~ "Yes",
                                          political_opposition == 0 ~ "No",
                                          is.na(political_opposition) ~ "DK/Ref"
         ),
         trust_iebc_alot = case_when(trust_iebc == 4 ~ "Yes",
                                     trust_iebc %in% c(1, 2, 3) ~ "No",
                                     trust_iebc %in% c(-998, -999) ~ "DK/Ref"
         ),
         trust_police_alot = case_when(trust_police == 4 ~ "Yes",
                                       trust_police %in% c(1, 2, 3) ~ "No",
                                       trust_police %in% c(-998, -999) ~ "DK/Ref"
         ),
         trust_president_alot = case_when(trust_president == 4 ~ "Yes",
                                          trust_president %in% c(1, 2, 3) ~ "No",
                                          trust_president %in% c(-998, -999) ~ "DK/Ref"
         ),
         trust_courts_alot = case_when(trust_courts == 4 ~ "Yes",
                                       trust_courts %in% c(1, 2, 3) ~ "No",
                                       trust_courts %in% c(-998, -999) ~ "DK/Ref"
         ),
         trust_kra_alot = case_when(trust_kra == 4 ~ "Yes",
                                    trust_kra %in% c(1, 2, 3) ~ "No",
                                    trust_kra %in% c(-998, -999) ~ "DK/Ref"
         ),
         trust_religious_leaders_alot = case_when(trust_religious_leaders == 4 ~ "Yes",
                                                  trust_religious_leaders %in% c(1, 2, 3) ~ "No",
                                                  trust_religious_leaders %in% c(-998, -999) ~ "DK/Ref"
         ),
         trust_iebc = case_when(trust_iebc %in% c(3, 4) ~ "High",
                                trust_iebc %in% c(1, 2) ~ "Low",
                                trust_iebc %in% c(-998, -999) ~ "DK/Ref"
         ),
         trust_police = case_when(trust_police %in% c(3, 4) ~ "High",
                                  trust_police %in% c(1, 2) ~ "Low",
                                  trust_police %in% c(-998, -999) ~ "DK/Ref"
                                  
         ),
         trust_president = case_when(trust_president %in% c(3, 4) ~ "High",
                                     trust_president %in% c(1, 2) ~ "Low",
                                     trust_president %in% c(-998, -999) ~ "DK/Ref"
         ),
         trust_courts = case_when(trust_courts %in% c(3, 4) ~ "High",
                                  trust_courts %in% c(1, 2) ~ "Low",
                                  trust_courts %in% c(-998, -999) ~ "DK/Ref"
         ),
         trust_kra = case_when(trust_kra %in% c(3, 4) ~ "High",
                               trust_kra %in% c(1, 2) ~ "Low",
                               trust_kra %in% c(-998, -999) ~ "DK/Ref"
         ),
         trust_religious_leaders = case_when(trust_religious_leaders %in% c(3, 4) ~ "High",
                                             trust_religious_leaders %in% c(1, 2) ~ "Low",
                                             trust_religious_leaders %in% c(-998, -999) ~ "DK/Ref"
         ),
         trust_iebc_alot_b = ifelse(trust_iebc_alot == "Yes", 1, 0),
         trust_president_alot_b = ifelse(trust_president_alot == "Yes", 1, 0),
         trust_police_alot_b = ifelse(trust_police_alot == "Yes", 1, 0),
         trust_courts_alot_b = ifelse(trust_courts_alot == "Yes", 1, 0),
         trust_kra_alot_b = ifelse(trust_kra_alot == "Yes", 1, 0),
         #
         trust_iebc_b = ifelse(trust_iebc == "High", 1, 0),
         trust_president_b = ifelse(trust_president == "High", 1, 0),
         trust_police_b = ifelse(trust_police == "High", 1, 0),
         trust_courts_b = ifelse(trust_courts == "High", 1, 0),
         trust_kra_b = ifelse(trust_kra == "High", 1, 0),
         #
         post_secondary = ifelse(education %in% c("At least some university", "Post-secondary qualifications other than university"
         ), "Yes", "No"),
         register_huduma_number = case_when(register_huduma_number == 1 ~ "Yes",
                                            register_huduma_number == 2 | is.na(register_huduma_number) ~ "No",
                                            register_huduma_number %in% c(-998, -999) ~ "DK/Ref"
         )) %>%
  rowwise() %>%
  mutate(asset_index_na = radio_b + computer_b + mobile_phone_b,
         asset_index = ifelse(is.na(asset_index_na), 0, asset_index_na),
         trust_gov_alot_index = trust_iebc_alot_b + trust_police_alot_b + trust_president_alot_b + trust_courts_alot_b + trust_kra_alot_b,
         trust_gov_alot_index_na = ifelse(trust_iebc_alot == "DK/Ref" | trust_president_alot == "DK/Ref" | trust_police_alot == "DK/Ref" | trust_courts_alot == "DK/Ref" | trust_kra_alot == "DK/Ref", NA, trust_gov_alot_index),
         trust_gov_index = trust_iebc_b + trust_police_b + trust_president_b + trust_courts_b + trust_kra_b,
         trust_gov_index_na = ifelse(trust_iebc == "DK/Ref" | trust_president == "DK/Ref" | trust_police == "DK/Ref" | trust_courts == "DK/Ref" | trust_kra == "DK/Ref", NA, trust_gov_index)
  ) %>%
  dplyr::select(!c(county, edu_level_cont, education, radio_b, computer_b, mobile_phone_b,
                   source_income_b,
                   trust_iebc_alot_b, trust_police_alot_b, trust_president_alot_b, trust_courts_alot_b, trust_kra_alot_b,
                   trust_iebc_b, trust_police_b, trust_president_b, trust_courts_b, trust_kra_b, trust_gov_alot_index_na, trust_gov_index_na,
                   selected_age, selected_gender, asset_index_na
                   
  ))
rm(missing_IDs)


