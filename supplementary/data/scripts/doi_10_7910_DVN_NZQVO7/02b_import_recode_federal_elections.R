####################################################################
####################################################################
## Replication Material
##
## Shaun Bowler, Gail McElroy, Stefan Müller:
## Voter Expectations of Government Formation in Coalition Systems: 
## The Importance of the Information Context.
## European Journal of Political Research

## File: 02b_import_recode_federal_elections.R

## See 000_description_replication_material.pdf for a detailed 
## overview of the required data and the outputs of this file.

####################################################################
####################################################################


## script to adjust variable coding and rename variables from each state election survey
## to a common format

## Note: this script requires the raw survey data which 
## need to be downloaded (after a free registration) from 
## the GESIS website.
## If you include these datasets into a sub-folder called 
## data_gesis, you can rerun this script and reproduce the analysis
## The output of this file (= a merged and harmonised dataset) 
## is called data_studies_merged_federal.rds, included in the Dataverse
## and used in subsequent scripts


## load required packages

library(foreign) # Read Data Stored by 'Minitab', 'S', 'SAS', 'SPSS', 'Stata', 'Systat', 'Weka', 'dBase', ..., CRAN v0.8-80
library(dplyr) # A Grammar of Data Manipulation, CRAN v1.0.2
library(tidyr) # Tidy Messy Data, CRAN v1.1.2
library(haven) # Import and Export 'SPSS', 'Stata' and 'SAS' Files, CRAN v2.3.1
library(stringr) # Simple, Consistent Wrappers for Common String Operations, CRAN v1.4.0
library(car) # Companion to Applied Regression, CRAN v3.0-9


## load raw data of federal election surveys and select/rename
## relevant variables

data_2009_fed_raw <- foreign::read.dta("data_gesis/ZA5300_v5-0-1_de.dta") %>% 
  as_factor()

data_2009_fed <- transmute(data_2009_fed_raw,
                           study_id = "ZA5300_v5-0-1_de",
                           election_id = "Federal Election 2009",
                           state = land,
                           year = 2009,
                           weight_microc = as.numeric(wei_tran),
                           weight_online = as.numeric(wei_trow),
                           gender = q1,
                           age = q1a,
                           education = d206,
                           pol_interest_fed =  q2,
                           satisf_democracy_fed = q5,
                           vote_intention_federal = q13,
                           scale_satisf_gov_federal =  q70,
                           interest_campaign = q3,
                           internet_use_political = q134,
                           vote_party_fed = q10b,
                           vote_party_fed_postal = q10bb,
                           vote_party_sure = q13,
                           importance_election_result = q4,
                           scale_cdu = q22a,
                           scale_csu = q22b,
                           scale_spd = q22c,
                           scale_fdp = q22c,
                           scale_greens = q22e,
                           scale_left = q22f,
                           
                           scale_coa_cdu_spd = q75a, 
                           scale_coa_cdu_fdp = q75b,
                           scale_coa_spd_greens = q75c,
                           scale_coa_spd_fdp = q75d,
                           scale_coa_cdu_greens = q75e,
                           scale_coa_spd_fdp_greens = q75f,
                           scale_coa_cdu_fdp_greens = q75g,
                           scale_coa_spd_left_greens = q75h,
                           
                           predict_gov = q78a,
                           predict_coa_two_options = q77,
                           
                           predict_coalition_type = "Coalitions: binary",
                           
                           lr_self = q59,
                           lr_cdu = q57a,
                           lr_csu = q57b,
                           lr_spd = q57c,
                           lr_fdp = q57d,
                           lr_left = q57f,
                           lr_greens = q57e,
                           party_id = q139a,
                           party_id_2 = q139b,
                           party_id_strength = q140,
                           job_type = d213,
                           income = d270)

nrow(data_2009_fed)

data_2009_fed <- data_2009_fed %>% 
  mutate(age = as.numeric(as.character(age)))



data_2013_fed_raw <- foreign::read.dta("data_gesis/ZA5700_v2-0-1_de.dta") %>% 
  as_factor()

data_2013_fed <- transmute(data_2013_fed_raw,
                           study_id = "ZA5700_v2-0-1_de",
                           election_id = "Federal Election 2013",
                           year = 2013,
                           state = bl,
                           weight_microc = as.numeric(w_tran),
                           weight_online = as.numeric(w_trow),
                           gender = q1,
                           year_of_birth = q2c,
                           age = 2013 - year_of_birth,
                           education = q163,
                           pol_interest_fed =  q3,
                           satisf_democracy_fed = q6,
                           vote_intention_federal = q13,
                           scale_satisf_gov_federal =  q74,
                           interest_campaign = q4,
                           internet_use_political = q115,
                           vote_party_fed = q11ba,
                           vote_party_fed_postal = q12ba,
                           vote_party_sure = q13,
                           importance_election_result = q5,
                           scale_cdu = q21a,
                           scale_csu = q21b,
                           scale_spd = q21c,
                           scale_fdp = q21c,
                           scale_greens = q21f,
                           scale_left = q21e,
                           scale_afd = q21h,
                           scale_pirates = q21g,
                           
                           scale_coa_cdu_spd = q79c, 
                           scale_coa_cdu_fdp = q79a,
                           scale_coa_spd_greens = q79b,
                           scale_coa_cdu_greens = q79d,
                           scale_coa_spd_fdp_greens = q79e,
                           scale_coa_spd_left_greens = q79f,
                           
                           predict_coa_cdu_spd = q80c, 
                           predict_coa_cdu_fdp = q80a,
                           predict_coa_spd_greens = q80b,
                           predict_coa_cdu_greens = q80d,
                           predict_coa_spd_fdp_greens = q80e,
                           predict_coa_spd_left_greens = q80f,
                           
                           predict_coalition_type = "Coalitions: continuous",
                           
                           lr_self = q62,
                           lr_cdu = q60a,
                           lr_csu = q60b,
                           lr_spd = q60c,
                           lr_fdp = q60d,
                           lr_left = q60e,
                           lr_greens = q60f,
                           lr_pirates = q60g,
                           lr_afd = q60h,
                           party_id = q119a,
                           party_id_2 = q119b,
                           party_id_strength = q120,
                           income = q215)

data_2013_fed <- data_2013_fed %>% 
  mutate(age = as.numeric(as.character(age)))

data_2017_fed_raw <- foreign::read.dta("data_gesis/ZA6800_v3-0-0.dta") %>% 
  as_factor()

data_2017_fed <- transmute(data_2017_fed_raw,
                           study_id = "ZA6800_v3-0-0",
                           election_id = "Federal Election 2017",
                           year = 2017,
                           state = bula,
                           weight_microc = as.numeric(w_ipfges),
                           gender = q1,
                           year_of_birth = q2a,
                           age = 2017 - year_of_birth,
                           education = q136,
                           pol_interest_fed =  q3,
                           satisf_democracy_fed = q6,
                           vote_intention_federal = q13,
                           scale_satisf_gov_federal =  q67,
                           interest_campaign = q4,
                           internet_use_political = q96,
                           vote_party_fed = q11ba,
                           vote_party_fed_postal = q12ba,
                           vote_party_sure = q13,
                           importance_election_result = q5,
                           scale_cdu = q20a,
                           scale_csu = q20b,
                           scale_spd = q20c,
                           scale_fdp = q21f,
                           scale_greens = q21e,
                           scale_left = q21d,
                           scale_afd = q21g,
                           
                           scale_coa_cdu_spd = q73c, 
                           scale_coa_cdu_fdp = q73a,
                           scale_coa_spd_greens = q73b,
                           scale_coa_cdu_greens = q73d,
                           scale_coa_spd_fdp_greens = q73e,
                           scale_coa_cdu_fdp_greens = q73f,
                           scale_coa_spd_left_greens = q73g,
                           
                           predict_gov_cdu = q75a, 
                           predict_gov_spd = q75b,
                           predict_gov_left = q75c,
                           predict_gov_greens = q75d,
                           predict_gov_fdp = q75e,
                           predict_gov_afd = q75f,
                           predict_gov_other = q75g,
                           
                           predict_coalition_type = "Government parties: binary",
                           
                           lr_self = q54,
                           lr_cdu = q52a,
                           lr_csu = q52b,
                           lr_spd = q52c,
                           lr_fdp = q52f,
                           lr_left = q52d,
                           lr_greens = q52e,
                           lr_afd = q52g,
                           party_id = q99a,
                           party_id_2 = q99b,
                           party_id_strength = q100,
                           income = q192)

## change age to numeric variable
data_2017_fed <- data_2017_fed %>% 
  mutate(age = as.numeric(as.character(age)))


nrow(data_2017_fed)

dta_combined_federal <- bind_rows(data_2009_fed,
                                  data_2013_fed,
                                  data_2017_fed)


dta_combined <- dta_combined_federal %>% 
  group_by(study_id) %>% 
  mutate(respondent_id = paste(election_id, "R", row_number(), sep = "_")) %>% 
  ungroup() %>% 
  dplyr::select(respondent_id, everything())

nrow(dta_combined)

## replace some characters
replace_ue <-  function(x){
  str_replace(x, 'ü', 'ue')
}

replace_ae <-  function(x){
  str_replace(x, 'ä', 'ae')
}


replace_ae2 <-  function(x){
  str_replace(x, 'mae�ig', 'maessig')
}

replace_ae3 <-  function(x){
  str_replace(x, 'm��ig', 'maessig')
}

replace_ae4 <-  function(x){
  str_replace(x, 'm�nn', 'maenn')
}


replace_ss <-  function(x){
  str_replace(x, 'ß', 'ss')
}

replace_ss2 <- function(x){
  str_replace(x, 'wei�', 'weiss')
}


replace_ueberhaupt <-  function(x){
  str_replace(x, '�berhaupt', 'ueberhaupt')
}

replace_colon <-  function(x){
  str_replace(x, ';', ',')
}

dta_combined <- mutate_if(dta_combined, is.character, as.factor)

dta_combined <- dta_combined %>% 
  mutate_all(funs(replace_ae)) %>% 
  mutate_all(funs(replace_ae2)) %>% 
  mutate_all(funs(replace_ae3)) %>% 
  mutate_all(funs(replace_ae4)) %>% 
  mutate_all(funs(replace_ue)) %>% 
  mutate_all(funs(replace_ueberhaupt)) %>% 
  mutate_all(funs(replace_ss)) %>% 
  mutate_all(funs(replace_ss2)) %>% 
  mutate_all(funs(replace_colon))


## recode some answers
adjust_na_trifft_nicht_zu <-  function(x){
  str_replace(x, "trifft nicht zu", NA_character_)
}

adjust_na_keine_angabe <-  function(x){
  str_replace(x, "keine Angabe", NA_character_)
  
}

adjust_party_neg <- function(x){
  str_replace(x, pattern = '-5 halte ueberhaupt nichts von dieser Partei', '-5')
  
}

adjust_party_pos <- function(x){
  str_replace(x, pattern = '+5 halte sehr viel von dieser Partei', '5')
  
}

adjust_satis_neg <- function(x){
  str_replace(x, pattern = '-5 vollstaendig unzufrieden', '-5')
  
}

adjust_satis_neg2 <- function(x){
  str_replace(x, pattern = '-5 voellig unzufrieden', '-5')
  
}


adjust_satis_neg3 <- function(x){
  str_replace(x, pattern = '-5 vollst�ndig unzufrieden', '-5')
  
}

adjust_satis_pos1 <- function(x){
  str_replace(x, pattern = fixed('+5 voll und ganz zufrieden'), '5')
  
}

adjust_satis_pos2 <- function(x){
  str_replace(x, pattern = fixed('+5 voellig zufrieden'), '5')
  
}


adjust_very_likely <- function(x){
  str_replace(x, 
              pattern = fixed('11 sehr wahrscheinlich'), '11')
  
}

adjust_very_unlikely <- function(x){
  str_replace(x, 
              pattern = fixed('1 sehr unwahrscheinlich'), '1')
  
}

adjust_very_desirable <- function(x){
  str_replace(x, 
              pattern = fixed('+5 sehr wuenschenswert'), '+5')
  
}

adjust_very_desirable2 <- function(x){
  str_replace(x, 
              pattern = fixed('+5 aeusserst wuenschenswert'), '+5')
  
}


adjust_very_undesirable <- function(x){
  str_replace(x, 
              pattern = fixed('-5 ueberhaupt nicht wuenschenswert'), '-5')
  
}

adjust_dk01 <- function(x){
  str_replace(x, 
              pattern = fixed('kann ich nicht einschaetzen'), "Don’t know")
  
}

adjust_dk02 <- function(x){
  str_replace(x, 
              pattern = fixed('kann ich nicht einsch�tzen'), "Don’t know")
  
}


adjust_dk03 <- function(x){
  str_replace(x, 
              pattern = fixed('weiss nicht'), "Don’t know")
  
}


adjust_dk04a <- function(x){
  str_replace(x, 
              pattern = fixed('nicht einzuschaetzen '), "Don’t know")
  
}

adjust_dk04b <- function(x){
  str_replace(x, 
              pattern = fixed('nicht einzuschaetzen'), "Don’t know")
  
}


adjust_dk05 <- function(x){
  str_replace(x, 
              pattern = fixed('wei� nicht'), "Don’t know")
  
}

adjust_very_left <- function(x){
  str_replace(x, 
              pattern = fixed('1 links'), '1')
  
}

adjust_very_right <- function(x){
  str_replace(x, 
              pattern = fixed('11 rechts'), '11')
  
}


dta_combined <- dta_combined %>% 
  mutate_all(funs(adjust_na_trifft_nicht_zu)) %>% 
  mutate_all(funs(adjust_na_keine_angabe)) %>% 
  mutate_all(funs(adjust_party_neg)) %>% 
  mutate_all(funs(adjust_satis_pos1)) %>% 
  mutate_all(funs(adjust_satis_pos2)) %>%
  mutate_all(funs(adjust_satis_neg)) %>%
  mutate_all(funs(adjust_satis_neg2)) %>%
  mutate_all(funs(adjust_satis_neg3)) %>%
  mutate_all(funs(adjust_very_likely)) %>%
  mutate_all(funs(adjust_very_unlikely)) %>%
  mutate_all(funs(adjust_very_desirable)) %>%
  mutate_all(funs(adjust_very_desirable2)) %>%
  mutate_all(funs(adjust_very_undesirable)) %>%
  mutate_all(funs(adjust_dk01)) %>%
  mutate_all(funs(adjust_dk02)) %>%
  mutate_all(funs(adjust_dk03)) %>%
  mutate_all(funs(adjust_dk04a)) %>%
  mutate_all(funs(adjust_dk04b)) %>%
  mutate_all(funs(adjust_dk05)) %>% 
  mutate_all(funs(adjust_very_left)) %>% 
  mutate_all(funs(adjust_very_right))


## recode/harmonise income categories

recode_income <- c("'unter 500 Euro'='0-500';
                   'Unter 500 Euro'='0-500';
                   '500 bis unter 750 Euro'='500-1000';
                   '500 bis unter 900 Euro'='500-1000';
                   '750 bis unter 1000 Euro'='500-1000';
                   '900 bis unter 1300 Euro'='1000-1500';
                   '1000 bis unter 1250 Euro'='1000-1500';
                   '1250 bis unter 1500 Euro'='1000-1500';
                   '1300 bis unter 1500 Euro'='1000-1500';
                   '1500 bis unter 2000 Euro'='1500-3500';
                   '2000 bis unter 2500 Euro'='1500-3500';
                   '2000 bis unter 2600 Euro'='1500-3500';
                   '2500 bis unter 3000 Euro'='1500-3500';
                   '2600 bis unter 3500 Euro'='1500-3500';
                   '3000 bis unter 4000 Euro'='3500-5000';
                   '3500 bis unter 4500 Euro'='3500-5000';
                   '4000 bis unter 5000 Euro'='3500-5000';
                   '4500 bis unter 6000 Euro'='5000-8000';
                   '5000 bis unter 7500 Euro'='5000-8000';
                   '6000 bis unter 8000 Euro'='5000-8000';
                   '7500 Euro bis unter 10000 Euro'='8000-';
                   '8000 und mehr Euro'='8000-';
                   '10000 Euro und mehr'='8000-';
                   'keine Angabe'='No information'")

dta_combined <- dta_combined %>% 
  mutate(income_stand = car::recode(income, recode_income))


recode_income_num <- c("'0-500'=1;
                       '500-1000'=2;
                       '1000-1500'=3;
                       '1500-3500'=4;
                       '3500-5000'=5;
                       '5000-8000'=6; '8000-'='7'; 'No information'=NA")

dta_combined <- dta_combined %>% 
  mutate(income_num = car::recode(income_stand, recode_income_num)) %>% 
  mutate(income_um = as.numeric(income_num))



## recode political interest

recode_interest <- c("'keine Angabe'='No information';
                     'ueberhaupt nicht'='No interest at all';
                     'weniger stark'='Not much interest';
                     'mittelmaessig'='Medium interest';
                     'ziemlich stark'='Strong interest';
                     'stark'='Strong interest';
                     'sehr stark'='Very strong interest'")

levels_interest <- c("No interest at all", "Not much interest",
                     "Medium interest",  "Strong interest",
                     "Very strong interest", "No information")

dta_combined <- dta_combined %>% 
  mutate(pol_interest_fed_stand = car::recode(pol_interest_fed, recode_interest)) %>% 
  mutate(interest_campaign_stand = car::recode(interest_campaign, recode_interest))

## recode satisfaction with democracy (federal level)

table(dta_combined$satisf_democracy_fed)

recode_satisf <- c("'keine Angabe'='No information';
                   'sehr unzufrieden'='Very dissatisfied';
                   'ziemlich unzufrieden'='Very dissatisfied';
                   'unzufrieden'='Dissatisfied';
                   'teils zufrieden, teils unzufrieden'='Undecided';
                   'teils/teils'='Undecided';
                   'zufrieden'='Satisfied';
                   'ziemlich zufrieden'='Very satisfied';
                   'sehr zufrieden'='Very satisfied'")

levels_satisf <- c("Very dissatisfied", "Dissatisfied",
                   "Undecided", "Satisfied",
                   "Very satisfied", "No information")

dta_combined <- dta_combined %>% 
  mutate(satisf_democracy_fed_stand = car::recode(satisf_democracy_fed, 
                                                  recode_satisf))

table(dta_combined$satisf_democracy_fed_stand)

dta_combined$satisf_democracy_fed_stand <- factor(dta_combined$satisf_democracy_fed_stand,
                                                  levels = levels_satisf)


## recode intention to vote (federal election)

table(dta_combined$vote_intention_federal)

recode_vote_int <- c("'vielleicht zur Wahl gehen'='Maybe/probably vote';
                     'wahrscheinlich zur Wahl gehen'='Maybe/probably vote';
                     'eher unsicher'='Maybe/probably not vote';
                     'ueberhaupt nicht sicher'='Maybe/probably not vote';
                     'sehr unsicher'='Don’t know';
                     'nicht so sicher'='Don’t know';
                     'weiss nicht'='Don’t know';
                     'Briefwahl'='Voted already (postal)';
                     'habe bereits Briefwahl gemacht'='Voted already (postal)';
                     'bestimmt nicht zur Wahl gehen'='Definitely not vote';
                     'ganz sicher'='Definitely vote';
                     'ziemlich sicher'='Rather vote';
                     'sicher'='Definitely vote';
                     'sehr sicher'='Definitely vote';
                     'keine Angabe'='No information'")

dta_combined <- dta_combined %>% 
  mutate(vote_intention_federal_stand = car::recode(vote_intention_federal,
                                                    recode_vote_int))

table(dta_combined$vote_intention_federal)

## recode partyID/partyID2

table(dta_combined$party_id)

recode_party_id <- c("'andere Partei'='Other party';
                     'keine Angabe'='No information';
                     'keine Partei'='No party';
                     'keine Partei, keiner Partei'='No party';
                     'mit mehreren Parteien/Unentschieden'='Other party';
                     'sonstige Nennung'='Other party';
                     'Sonstige'='Other party'")

dta_combined <- dta_combined %>% 
  mutate(party_id_stand = car::recode(party_id, recode_party_id)) %>% 
  mutate(party_id_2_stand = car::recode(party_id_2, recode_party_id)) 

table(dta_combined$party_id_stand)

table(dta_combined$education, useNA = "always")

## recode education into binary variable
recode_education <- "'Abitur bzw. erweiterte Oberschule mit Abschluss 12. Klasse (Hochschulreife)'='A-Levels';
'Abitur bzw. erweiterte Oberschule'='A-Levels';NA=NA;else='No A-Levels'"

dta_combined <- dta_combined %>% 
  mutate(education_stand = car::recode(education, recode_education))

## merge and recode vote type
dta_combined <- dta_combined %>% 
  mutate(vote_party_merged = ifelse(is.na(vote_party_fed),
                                    vote_party_fed_postal, NA))

dta_combined <- dta_combined %>% 
  mutate(vote_party_type = if_else(!is.na(vote_party_fed), "Entitled to vote",
                                   if_else(is.na(vote_party_fed) & !is.na(vote_party_fed_postal), 
                                           "Voted already", "Not entitled to vote")))

## recode vote choice

recode_vote_party <- c("'andere Partei'='Other party';
                       'CDU/CSU'='CDU'; 'CSU'='CDU';
                       'keine Angabe'='No information';
                       'noch nicht entschieden'='Don’t know';
                       'Sonstige'='Other party';
                       'sonstige Nennung'='Other party';
                       'Sonstige Nennung'='Other party';
                       'ungueltig waehlen'='Invalid vote';
                       'werde keine Zweitstimme abgeben'='Invalid vote';
                       'werde ungueltig waehlen'='Invalid vote'")


dta_combined <- dta_combined %>% 
  mutate(vote_party_merged = car::recode(vote_party_merged, recode_vote_party))

## recode gender

table(dta_combined$gender)

recode_gender <- c("'maennlich'='Male';
                   'weiblich'='Female'")

dta_combined <- dta_combined %>% 
  mutate(gender_stand = car::recode(gender, recode_gender))

## recode year of birth to age

dta_combined_age <- dta_combined %>% 
  select(year_of_birth, age)

dta_combined <- dta_combined %>%
  mutate(age_stand = as.numeric(age))

dta_combined <- mutate_if(dta_combined, is.character, as.factor)

## select the scale variables and left right numeric variables

dta_recode_scale <- dta_combined %>%
  dplyr::select(starts_with("scale_"), starts_with("lr"))

dta_recode_scale <- mutate_if(dta_recode_scale, is.character, as.factor)

replace_scale_dk <-  function(x){
  str_replace(x, 'Don’t know', NA_character_)
}

replace_scale_no_answer <-  function(x){
  str_replace(x, 'No information', NA_character_)
}

replace_space <-  function(x){
  str_replace(x, ' ', '')
}


dta_recode_scale <- dta_recode_scale %>% 
  mutate_all(funs(replace_scale_dk)) %>% 
  mutate_all(funs(replace_scale_no_answer)) %>% 
  mutate_all(funs(replace_space))


dta_recode_scale_num <- mutate_if(dta_recode_scale, is.character, as.numeric)
dta_recode_scale_num <- mutate_if(dta_recode_scale_num, is.factor, as.numeric)

## change the numeric variables

stopifnot(nrow(dta_combined) == nrow(dta_recode_scale_num))

dta_combined <- dta_combined %>% 
  select(-c(starts_with("lr_"), starts_with("scale_"))) %>% 
  bind_cols(dta_recode_scale_num)


## transform each version of coalition predictions to long format -----

## binary coding of coalition parties 

table(dta_combined$predict_coalition_type)

dta_predict_gov_binary <- dta_combined %>% 
  filter(predict_coalition_type == "Coalitions: binary") %>% 
  dplyr::select(election_id, year, predict_coalition_type, respondent_id, 
                predict_coa_two_options,
                starts_with("predict_gov"),
                starts_with("scale_coa_")) 

## recode the answers to standardised options
table(dta_predict_gov_binary$predict_coa_two_options)

recode_predict_coa_two_options <- c("'es wird fuer keine dieser Koalitionen eine Mehrheit geben'=NA;
                                     'Don’t know'=NA;
                                     'Mehrheit fuer Rot-Gruen (SPD und GRUENE)'='spd_greens';
                                     'Mehrheit fuer Schwarz-Gelb (CDU/CSU und FDP)'='cdu_fdp'")

dta_predict_gov_binary <- dta_predict_gov_binary %>% 
  mutate(predict_coa_two_options_stand = as.character(car::recode(predict_coa_two_options,
                                                                  recode_predict_coa_two_options)))

table(dta_predict_gov_binary$predict_gov)

recode_predict_gov <- c("'CDU/CSU und DIE LINKE'='cdu_left';
                        'Don’t know'=NA;
                        'CDU/CSU und FDP (Schwarz-Gelbe Koalition)'='cdu_fdp';
                        'CDU/CSU und GRUENE (Schwarz-Gruene Koalition)'='cdu_greens';
                        'CDU/CSU und SPD (Grosse Koalition)'='cdu_spd';
                        'CDU/CSU-Alleinregierung'='cdu';
                        'CDU/CSU, FDP und GRUENE (Jamaika-Koalition)'='cdu_fdp_greens';
                        'CDU/CSU, SPD und FDP'='cdu_spd_fdp';
                        'DIE LINKE und GRUENE'='left_greens';
                        'SPD und FDP (Sozialliberale Koalition)'='spd_fdp';
                        'SPD-Alleinregierung'='spd';
                        'SPD, DIE LINKE und GRUENE (Rot-Rot-Gruene Koalition)'='spd_left_greens';
                        'SPD, FDP und GRUENE (Ampel-Koalition)'='spd_fdp_greens'")

dta_predict_gov_binary <- dta_predict_gov_binary %>% 
  mutate(predict_gov_stand = as.character(car::recode(predict_gov, recode_predict_gov)))


dta_predict_gov_binary <- dta_predict_gov_binary %>% 
  mutate(predicted_coalition = ifelse(is.na(predict_coa_two_options_stand),
                                      predict_gov_stand, predict_coa_two_options_stand))

## cross-table of predicted coalitions
table(dta_predict_gov_binary$predicted_coalition)

## transform to long format
dta_predict_gov_binary_long <- dta_predict_gov_binary %>% 
  select(respondent_id, election_id, year, predicted_coalition, starts_with("scale_coa_")) %>% 
  gather(key = coalition_option, value = scale_wish_coalition, 
         -c(respondent_id, election_id, year,
            predicted_coalition)) %>%  
  arrange(election_id, respondent_id) 

dta_predict_gov_binary_long <- dta_predict_gov_binary_long %>% 
  mutate(coalition_option_stand = stringr::str_replace_all(coalition_option, "scale_coa_", "")) %>% 
  mutate(predicted_coalition_dummy = if_else(coalition_option_stand == predicted_coalition, TRUE, FALSE)) %>% 
  mutate(predicted_coalition_stand = predicted_coalition)


dta_predict_gov_binary_long <- dta_predict_gov_binary_long %>% 
  mutate(scale_wish_coalition_stand = scales::rescale(scale_wish_coalition, to =  c(0, 10))) 

dta_predict_gov_binary_long <- dta_predict_gov_binary_long %>% 
  group_by(respondent_id) %>% 
  mutate(wish_coalition_max = max(scale_wish_coalition_stand, na.rm = TRUE)) %>% 
  ungroup()

dta_predict_gov_binary_long <- dta_predict_gov_binary_long %>% 
  mutate(wish_coalition_dummy = as.logical(wish_coalition_max ==  scale_wish_coalition_stand, TRUE, FALSE)) %>% 
  mutate(dummy_wish_equals_predicted = ifelse(coalition_option_stand == predicted_coalition_stand & wish_coalition_dummy == TRUE, TRUE, FALSE))

## continuous coalition options
dta_predict_coa_continuous <- dta_combined %>% 
  filter(predict_coalition_type == "Coalitions: continuous") %>% 
  dplyr::select(predict_coalition_type, year, respondent_id, election_id,
                starts_with("predict_coa"), starts_with("scale_coa")) %>% 
  dplyr::select(-predict_coa_two_options)

nrow(dta_predict_coa_continuous)

replace_scale_dk <-  function(x){
  str_replace(x, 'Don’t know', NA_character_)
}

replace_scale_no_answer <-  function(x){
  str_replace(x, 'keine Angabe', NA_character_)
}

dta_predict_coa_continuous <- dta_predict_coa_continuous %>% 
  mutate_all(funs(replace_scale_dk)) %>% 
  mutate_all(funs(replace_scale_no_answer))

## transform to long format

dta_predict_coa_continuous_long <- dta_predict_coa_continuous %>% 
  gather(key = coalition_option, value = score_option_predict, 
         -c(respondent_id, election_id, year, 
            predict_coalition_type, starts_with("wish_coa"),
            starts_with("scale_coa"))) %>% 
  mutate(coalition_option = as.factor(coalition_option)) %>% 
  mutate(score_option_predict = as.numeric(score_option_predict)) %>% 
  mutate(coalition_option_stand = stringr::str_replace_all(coalition_option,
                                                           "predict_coa_", ""))

## get scores for desired coalitions
dta_predict_coa_continuous_long <- dta_predict_coa_continuous_long %>% 
  mutate(score_option_wish = as.numeric(ifelse(coalition_option_stand == 
                                                 "cdu_fdp", scale_coa_cdu_fdp,
                                               ifelse(coalition_option_stand == 
                                                        "cdu_greens", scale_coa_cdu_greens,
                                                      ifelse(coalition_option_stand == 
                                                               "cdu_spd", scale_coa_cdu_spd,
                                                             ifelse(coalition_option_stand == "spd_fdp_greens", scale_coa_spd_fdp_greens,
                                                                    ifelse(coalition_option == "spd_greens", scale_coa_spd_greens,
                                                                           ifelse(coalition_option == "spd_left_greens", scale_coa_spd_left_greens, NA))))))))

dta_predict_coa_continuous_long <- dta_predict_coa_continuous_long %>%  
  mutate(score_option_wish = as.numeric(score_option_wish)) %>% 
  mutate(score_option_wish_0_10 = scales::rescale(score_option_wish, c(0, 10))) %>% 
  mutate(score_option_predict = as.numeric(score_option_predict)) %>% 
  mutate(score_option_predict_0_10 = scales::rescale(score_option_predict, c(0, 10))) 

## check which coalition option(s) has/have the maximum value of the desired values
dta_predict_coa_continuous_long <- dta_predict_coa_continuous_long %>% 
  group_by(election_id, respondent_id) %>% 
  mutate(score_predict_max_0_10 = max(score_option_predict_0_10, na.rm = TRUE)) %>% 
  mutate(score_wish_max_0_10 = max(score_option_predict_0_10, na.rm = TRUE)) %>% 
  mutate(predicted_coalition_dummy = ifelse(score_predict_max_0_10 == score_option_predict_0_10, TRUE, FALSE)) %>% 
  mutate(wish_coalition_dummy = ifelse(score_wish_max_0_10 == score_option_wish_0_10, TRUE, FALSE))

nrow(dta_predict_coa_continuous_long)

## check how many respondents predicted how many options
dta_predict_coa_continuous_sum <- dta_predict_coa_continuous_long %>% 
  group_by(election_id, respondent_id) %>% 
  summarise(number_predicted_coalitions = sum(predicted_coalition_dummy, na.rm = TRUE))


dta_predict_coa_continuous_long_sum <- dta_predict_coa_continuous_long %>%
  group_by(election_id, respondent_id) %>% 
  mutate(number_predicted_coalitions = sum(predicted_coalition_dummy, na.rm = TRUE)) %>% 
  group_by(respondent_id) %>% 
  mutate(predicted_coalition_stand = ifelse(predicted_coalition_dummy == TRUE,
                                            coalition_option_stand, NA)) %>% 
  filter(!is.na(predicted_coalition_stand)) %>% 
  dplyr::select(respondent_id, predicted_coalition_stand, number_predicted_coalitions) %>% 
  unique()

nrow(dta_predict_coa_continuous_long_sum)

## merge these details (number of predicted coalitions per respondent) back to main dataset
dta_predict_coa_continuous_long_merged <- left_join(
  dta_predict_coa_continuous_long,
  dta_predict_coa_continuous_long_sum, 
  by = "respondent_id")



## binary government parties options

dta_predict_coa_binary <- dta_combined %>% 
  filter(predict_coalition_type == "Government parties: binary") %>% 
  dplyr::select(predict_coalition_type, year, respondent_id, election_id,
                starts_with("predict_gov"), starts_with("scale_coa_"))


dta_predict_coa_binary <- dta_predict_coa_binary %>% 
  mutate(predict_gov_cdu_party = car::recode(predict_gov_cdu, "'genannt'='cdu';'nicht genannt'=''")) %>% 
  mutate(predict_gov_spd_party = car::recode(predict_gov_spd, "'genannt'='spd';'nicht genannt'=''")) %>% 
  mutate(predict_gov_fdp_party = car::recode(predict_gov_fdp, "'genannt'='fdp';'nicht genannt'=''")) %>% 
  mutate(predict_gov_left_party = car::recode(predict_gov_left, "'genannt'='left';'nicht genannt'=''")) %>% 
  mutate(predict_gov_greens_party = car::recode(predict_gov_greens, "'genannt'='greens';'nicht genannt'=''")) %>% 
  mutate(predict_gov_afd_party = car::recode(predict_gov_afd, "'genannt'='afd';'nicht genannt'=''")) 

dta_predict_coa_binary <- dta_predict_coa_binary %>% 
  mutate(predicted_coalition = stringi::stri_join(predict_gov_cdu_party,
                                                  predict_gov_spd_party,
                                                  predict_gov_fdp_party,
                                                  predict_gov_left_party,
                                                  predict_gov_greens_party,
                                                  predict_gov_afd_party,
                                                  sep = "", 
                                                  ignore_null = FALSE)) 

table(dta_predict_coa_binary$predicted_coalition)

## recode/standardise predicted coalition
recode_predicted_coalition <- c("'cduafd'='cdu_afd'; 'cdufdp'='cdu_fdp';
                                'cdufdpafd'='cdu_fdp_afd';
                                'cdufdpgreens'='cdu_fdp_greens';'cdufdpleft'='cdu_fdp_left'; 'cdufdpleftafd'='cdu_fdp_left_afd';
                                'cdufdpleftgreens'='cdu_fdp_left_greens';
                                'cdugreens'='cdu_greens';'cdugreensafd'='cdu_greens_afd'; 'cduleft'='cdu_left'; 'cduleftgreens'='cdu_left_greens';'cduspd'='cdu_spd';
                               'cduspdafd'='cdu_spd_afd';'cduspdfdp';'cdu_spd_fdp';
                                'cduspdfdpafd'='cdu_spd_fdp_afd';
                                'cduspdfdpgreens'='cdu_spd_fdp_greens';
                                'cduspdfdpgreensafd'='cdu_spd_fdp_greens_afd';
                                'cduspdgreens'='cdu_spd_greens';
'cduspdfdpleftgreens'='cdu_spd_fdp_left_greens';
'cduspdfdpleftgreensafd'='cdu_spd_fdp_left_greens_afd';
                                'cduspdleft'='cdu_spd_left';
                               'cduspdleftafd'='cdu_spd_left_afd';
                               'cduspdleftgreens'='cdu_spd_left_greens';
                               'cduspdleftgreensafd'='cdu_spd_left_greens_afd';
                                'fdpgreens'='fdp_greens';
                                'fdpleftafd'='fdp_left_afd';
                               'leftgreens'='left_greens';
                                'spdafd'='spd_afd';
                                'spdfdp'='spd_fdp';
                                'spdfdpafd'='spd_fdp_afd';
                                'spdfdpgreens'='spd_fdp_greens';
                               'spdfdpleftgreens'='spd_fdp_left_greens';
                                'spdgreens'='spd_greens';
                                'spdleft'='spd_left';
                                'spdleftafd'='spd_left_afd';
                                'spdleftgreens'='spd_left_greens';
                                'spdleftgreensafd'='spd_left_greens_afd'")

dta_predict_coa_binary <- dta_predict_coa_binary %>% 
  mutate(predicted_coalition_stand = car::recode(predicted_coalition, 
                                                 recode_predicted_coalition))

dta_predict_coa_binary_long <- dta_predict_coa_binary %>% 
  dplyr::select(respondent_id, election_id, year, predicted_coalition_stand, starts_with("scale_coa_")) %>% 
  gather(key = coalition_option, value = scale_wish_coalition, 
         -c(respondent_id, election_id, year,
            predicted_coalition_stand)) %>%  
  arrange(election_id, respondent_id) 

dta_predict_coa_binary_long <- dta_predict_coa_binary_long %>% 
  mutate(coalition_option_stand = stringr::str_replace_all(coalition_option, "scale_coa_", "")) %>% 
  mutate(predicted_coalition_dummy = if_else(coalition_option_stand == predicted_coalition_stand, TRUE, FALSE)) %>% 
  mutate(scale_wish_coalition = as.numeric(scale_wish_coalition))


dta_predict_coa_binary_long <- dta_predict_coa_binary_long %>% 
  mutate(scale_wish_coalition_0_10 = scales::rescale(scale_wish_coalition, to =  c(0, 10))) 

dta_predict_coa_binary_long <- dta_predict_coa_binary_long %>% 
  group_by(respondent_id) %>% 
  mutate(wish_coalition_max_0_10 = max(scale_wish_coalition_0_10, na.rm = TRUE)) %>% 
  ungroup()

dta_predict_coa_binary_long <- dta_predict_coa_binary_long %>% 
  mutate(wish_coalition_dummy = as.logical(wish_coalition_max_0_10 ==  scale_wish_coalition_0_10, TRUE, FALSE)) %>% 
  mutate(dummy_wish_equals_predicted = ifelse(coalition_option_stand == predicted_coalition_stand & wish_coalition_dummy == TRUE, TRUE, FALSE))


## merge long datasets ----

dta_predicted_combination <- bind_rows(
  dta_predict_gov_binary_long,
  dta_predict_coa_continuous_long_merged, 
  dta_predict_coa_binary_long) %>% 
  dplyr::select(-c(starts_with("scale_coa_"), predict_coalition_type))


dta_merged <- left_join(dta_predicted_combination, dta_combined,
                        by = c("election_id", "year", "respondent_id"))

names(dta_merged)
nrow(dta_predicted_combination)

dta_merged <- dta_merged %>% 
  select(election_id, predict_coalition_type, 
         year, respondent_id, coalition_option_stand,
         predicted_coalition_stand, predicted_coalition_dummy,
         dummy_wish_equals_predicted,
         everything())

dta_merged <- dta_merged %>% 
  group_by(respondent_id) %>% 
  mutate(predicted_coalition_stand = ifelse(predicted_coalition_stand == "", NA, predicted_coalition_stand)) 

## adjust left-right judgments
dta_merged <- dta_merged %>%     
  group_by(election_id) %>% 
  mutate(n_respondents = n()) %>% 
  mutate(lr_avg_cdu = mean(lr_cdu, na.rm = TRUE),
         lr_avg_spd = mean(lr_spd, na.rm = TRUE),
         lr_avg_fdp = mean(lr_fdp, na.rm = TRUE),
         lr_avg_afd = mean(lr_afd, na.rm = TRUE),
         lr_avg_left = mean(lr_left, na.rm = TRUE),
         lr_avg_greens = mean(lr_greens, na.rm = TRUE)) %>% 
  ungroup()

table(dta_merged$coalition_option_stand)

dta_merged <- dta_merged %>%
  group_by(election_id) %>%
  mutate(distance_coa_respondent_2_most_extreme_parties = case_when(
    coalition_option_stand == "cdu_fdp" ~ abs(lr_cdu - lr_fdp),
    coalition_option_stand == "cdu_fdp_greens" ~ abs(lr_cdu - lr_greens),
    coalition_option_stand == "cdu_greens" ~ abs(lr_cdu - lr_greens),
    coalition_option_stand == "cdu_spd" ~ abs(lr_spd - lr_cdu),
    coalition_option_stand == "spd_fdp_greens" ~ abs(lr_greens - lr_fdp),
    coalition_option_stand == "spd_greens" ~ abs(lr_spd - lr_greens),
    coalition_option_stand == "spd_fdp" ~ abs(lr_fdp - lr_spd),
    coalition_option_stand == "spd_left" ~ abs(lr_spd - lr_left),
    coalition_option_stand == "spd_left_greens" ~ abs(lr_spd - lr_left),
    coalition_option_stand == "cdu" ~ 0,
    coalition_option_stand == "spd" ~ 0
  )) %>%
  ungroup()


dta_merged <- dta_merged %>%
  group_by(election_id) %>%
  mutate(distance_coa_avg_2_most_extreme_parties = case_when(
    coalition_option_stand == "cdu_fdp" ~ abs(lr_avg_cdu - lr_avg_fdp),
    coalition_option_stand == "cdu_fdp_greens" ~ abs(lr_avg_cdu - lr_avg_greens),
    coalition_option_stand == "cdu_greens" ~ abs(lr_avg_cdu - lr_avg_greens),
    coalition_option_stand == "cdu_spd" ~ abs(lr_avg_spd - lr_avg_cdu),
    coalition_option_stand == "spd_fdp_greens" ~ abs(lr_avg_greens - lr_avg_fdp),
    coalition_option_stand == "spd_greens" ~ abs(lr_avg_spd - lr_avg_greens),
    coalition_option_stand == "spd_fdp" ~ abs(lr_avg_fdp - lr_avg_spd),
    coalition_option_stand == "spd_left" ~ abs(lr_avg_spd - lr_avg_left),
    coalition_option_stand == "spd_left_greens" ~ abs(lr_avg_spd - lr_avg_left),
    coalition_option_stand == "cdu" ~ 0,
    coalition_option_stand == "spd" ~ 0
  )) %>%
  ungroup()


dta_merged <- dta_merged %>% 
  mutate(distance_coa_2_parties_no_missing = 
           ifelse(is.na(distance_coa_respondent_2_most_extreme_parties), 
                  distance_coa_avg_2_most_extreme_parties, 
                  distance_coa_respondent_2_most_extreme_parties)) 


## calculate average left-right position of the coalition
dta_merged <- dta_merged %>%
  group_by(election_id) %>%
  mutate(avg_lr_coa_respondent = case_when(
    coalition_option_stand == "cdu_fdp" ~ (lr_cdu + lr_fdp) / 2,
    coalition_option_stand == "cdu_fdp_greens" ~ (lr_cdu + lr_greens + lr_fdp) / 3,
    coalition_option_stand == "cdu_greens" ~ (lr_cdu + lr_greens) / 2,
    coalition_option_stand == "cdu_spd" ~ (lr_spd + lr_cdu) / 2,
    coalition_option_stand == "spd_fdp_greens" ~ (lr_greens + lr_fdp + lr_spd) / 3,
    coalition_option_stand == "spd_greens" ~ (lr_spd + lr_greens) / 2,
    coalition_option_stand == "spd_left" ~ (lr_spd + lr_left) / 2,
    coalition_option_stand == "spd_fdp" ~ (lr_fdp + lr_spd) / 2,
    coalition_option_stand == "spd_left_greens" ~ (lr_spd + lr_left + lr_greens) / 3,
    coalition_option_stand == "cdu" ~ lr_cdu,
    coalition_option_stand == "spd" ~ lr_spd
  )) %>%
  mutate(distance_lr_coa_self = abs(avg_lr_coa_respondent - lr_self)) %>%
  ungroup()


table(dta_merged$coalition_option_stand)
length(unique(dta_merged$coalition_option_stand))

## get average left-right value of coalition but use the left-right average values across 
## all respondents
dta_merged <- dta_merged %>%
  group_by(election_id) %>%
  mutate(avg_lr_coa_avg = case_when(
    coalition_option_stand == "cdu_fdp" ~ (lr_avg_cdu + lr_avg_fdp) / 2,
    coalition_option_stand == "cdu_fdp_greens" ~ (lr_avg_cdu + lr_avg_fdp + lr_avg_greens) / 3,
    coalition_option_stand == "cdu_greens" ~ (lr_avg_cdu + lr_avg_greens) / 2,
    coalition_option_stand == "cdu_spd" ~ (lr_avg_spd + lr_avg_cdu) / 2,
    coalition_option_stand == "spd_fdp_greens" ~ (lr_avg_spd + lr_avg_greens + lr_avg_fdp) / 3,
    coalition_option_stand == "spd_greens" ~ (lr_avg_spd + lr_avg_greens) / 2,
    coalition_option_stand == "spd_left" ~ (lr_avg_spd + lr_avg_left) / 2,
    coalition_option_stand == "spd_fdp" ~ (lr_avg_spd + lr_avg_fdp) / 2,
    coalition_option_stand == "spd_left_greens" ~ (lr_avg_spd + lr_avg_greens +  lr_avg_left) / 3,
    coalition_option_stand == "cdu_afd" ~ (lr_avg_cdu + lr_avg_afd) / 2, 
    coalition_option_stand == "cdu" ~ lr_avg_cdu,
    coalition_option_stand == "spd" ~ lr_avg_spd
  )) %>% 
  mutate(distance_lr_avg_coa_self = abs(avg_lr_coa_avg - lr_self)) %>%
  ungroup()

## if distance on respondent-level is missing use the average across all respondents
dta_merged <- dta_merged %>% 
  mutate(distance_lr_coa_self_no_missing = 
           ifelse(is.na(distance_lr_coa_self), 
                  distance_lr_avg_coa_self, 
                  distance_lr_coa_self)) %>% 
  mutate(lr_dummy_coa_missing = ifelse(is.na(distance_lr_coa_self), TRUE, FALSE))


## save merged and harmonised dataset
saveRDS(dta_merged, file = "data_studies_merged_federal.rds")

