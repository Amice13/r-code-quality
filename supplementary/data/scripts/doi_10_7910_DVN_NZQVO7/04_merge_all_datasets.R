####################################################################
####################################################################
## Replication Material
##
## Shaun Bowler, Gail McElroy, Stefan Müller:
## Voter Expectations of Government Formation in Coalition Systems: 
## The Importance of the Information Context.
## European Journal of Political Research

## File: 04_merge_all_datasets.R

## See 000_description_replication_material.pdf for a detailed 
## overview of the required data and the outputs of this file.

####################################################################
####################################################################


## merge all datasets for subsequent regression analyses and descriptive 
## plots


## load required packages
library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v1.3.0
library(scales) # Scale Functions for Visualization, CRAN v1.1.1
library(car) # Companion to Applied Regression, CRAN v3.0-9

dta_survey_raw_federal <- readRDS(file = "data_studies_merged_federal.rds") %>%
  mutate(year = as.character(year)) %>%
  mutate(income_num = as.numeric(income_num)) %>%
  mutate(election_type = "Federal Election") %>%
  mutate(bundesland_merge = "federal election") %>% 
  mutate(bundesland = "Federal Election") %>% 
  mutate(age = as.numeric(as.character(age))) %>% 
  mutate(age_stand = age)


dta_survey_raw_land <- readRDS(file = "data_studies_merged_land.rds") %>%
  mutate(year = as.character(year)) %>%
  mutate(bundesland_merge = str_to_lower(bundesland)) %>%
  mutate(coalition_gles = coalition_option_stand) %>%
  mutate(election_type = "State Election") 

## merge Land and federal elections
dta_survey_raw <- bind_rows(dta_survey_raw_land,
                            dta_survey_raw_federal)

table(dta_survey_raw$election_type)
summary(dta_survey_raw$age_stand,useNA = "always")

unique(dta_survey_raw$election_id)
length(unique(dta_survey_raw$election_id))

dta_survey <- dta_survey_raw %>%
  group_by(respondent_id) %>%
  mutate(number_predicted_coalitions = n()) %>%
  mutate(number_predicted_coalitions_unique = length(unique(predicted_coalition_stand))) %>%
  ungroup() %>%
  mutate(number_predicted_coalitions_unique = ifelse(is.na(predicted_coalition_stand), 0,
                                                     number_predicted_coalitions_unique)) %>%
  mutate(dummy_not_evaluated = ifelse(number_predicted_coalitions_unique == 0,
                                      TRUE, FALSE))


## check number of choice set per election
dta_survey %>%
  group_by(election_id) %>%
  summarise(maximum = length(unique(coalition_option_stand))) %>%
  ungroup() %>%
  mutate(mean = mean(maximum)) %>%
  arrange(-maximum)

raw <- dta_survey %>%
  select(respondent_id, predicted_coalition_stand,
         number_predicted_coalitions_unique,
         election_type,
         predict_coalition_type,
         dummy_not_evaluated, election_id) %>%
  unique()

summary(raw$dummy_not_evaluated)


summary(dta_survey$number_predicted_coalitions)

table(dta_survey$coalition_option_stand,
      dta_survey$predict_coalition_type)


dta_prob_combined_land <- readRDS(file = "data_coalition_predictions_land.rds") %>%
  rename(bundesland_merge = bundesland) %>%
  mutate(year = as.character(year)) %>%
  mutate(coalition_gles = coalition)

dta_prob_combined_federal <- readRDS(file = "data_coalition_predictions_federal.rds") %>%
  rename(bundesland_merge = bundesland) %>%
  mutate(year = as.character(year)) %>%
  mutate(coalition_gles = coalition)


dta_prob_combined <- bind_rows(dta_prob_combined_land,
                               dta_prob_combined_federal)


dta_metadata <- readRDS(file = "metadata_elections_land.rds")

dta_metadata_small <- dta_metadata %>%
  select(-c(party:seats, election_id)) %>%
  mutate(coalition_after_election = coalition_gles) %>%
  mutate(coalition_before_election = lag_coalition_gles) %>%
  unique() %>%
  mutate(year = as.character(year)) %>%
  rename(bundesland_merge = bundesland) %>%
  select(-coalition_gles) %>%
  mutate(coalition_after_election =
           car::recode(coalition_after_election, "'spd_greens_ssw'='spd_greens'")) %>%
  mutate(coalition_before_election =
           car::recode(coalition_before_election, "'spd_greens_ssw'='spd_greens'"))


recode_coalition <- c("'cdu'='CDU'; 'cdu_fdp'='CDU, FDP'; 'cdu_fdp_greens'='CDU, FDP, Greens';
                      'cdu_greens'='CDU, Greens'; 'cdu_spd'='CDU, SPD';
                      'cdu_spd_greens'='CDU, SPD, Greens'; 'spd_fdp_greens'='SPD, FDP, Greens'; 'spd_greens'='SPD, Greens'; 'spd_greens_ssw'='SPD, Greens, SSW'; 'spd_left'='SPD, Left'; 'spd_left_greens'='SPD, Left, Greens';'spd'='SPD';'spd_fdp'='SPD, FDP'; 'cdu_afd'='CDU, AfD'")

dta_metadata_small$coalition_after_election_factor <-  car::recode(dta_metadata_small$coalition_after_election, recode_coalition)

dta_survey <- dta_survey %>%
  mutate(coalition_gles = coalition_option_stand)


## merge metadata on elections
dta_combined <- left_join(dta_survey, dta_metadata_small,
                          by = c("bundesland_merge", "year"))

## merge the coalition probabilities
dta_combined <- left_join(dta_combined, dta_prob_combined,
                          by = c("bundesland_merge", "year", "coalition_gles"))


## load aggregated news coverage of coalitions
news_combined <- readRDS("data_news_coalitions.rds") %>% 
  rename(bundesland_merge = bundesland,
         coalition_gles = coalition_type)


dta_combined <- left_join(dta_combined, news_combined,
                          by = c("bundesland_merge", "year", "coalition_gles"))


## recode coalition after election for the federal level
dta_combined <- dta_combined %>%
  mutate(coalition_after_election_federal = ifelse(election_id == "Federal Election 2009", "cdu_fdp", "cdu_spd")) %>% 
  mutate(coalition_after_election = ifelse(election_type == "Federal Election", coalition_after_election_federal, coalition_after_election))

table(dta_combined$coalition_after_election,
      dta_combined$election_type)


dta_combined <- dta_combined %>%
  mutate(year_election = paste(year, bundesland, sep = ": ")) %>% 
  mutate(year_election = stringr::str_replace_all(year_election, "ue", "ü"))

recode_coalition_predicted <- c("'cdu'='CDU'; 'cdu_fdp'='CDU, FDP'; 'cdu_fdp_greens'='CDU, FDP, Greens';
                                'cdu_greens'='CDU, Greens'; 'cdu_spd'='CDU, SPD';
                                'cdu_spd_greens'='CDU, SPD, Greens'; 'spd_fdp_greens'='SPD, FDP, Greens'; 'spd_greens'='SPD, Greens'; 'spd_greens_ssw'='SPD, Greens, SSW'; 'spd_left'='SPD, Left'; 'spd_left_greens'='SPD, Left, Greens'; 'spd_greens_other'='SPD, Greens, Other'; 'spd_other'='SPD, Other'; 'spd_fdp'='SPD, FDP'; 'spd_fdp_left'='SPD, FDP, Left'; 'spd_greens_fw'='SPD, Greens';
                                'left_other'='Left, Other'='greens_other'='Greens, Other';
                                'left'='Left';'left_greens'='Left, Greens';'fdp_greens_other'='FDP, Greens, Other'; 'fdp_greens'='FDP, Greens';'fdp'='FDP';'cdu_spd_other'='CDU, SPD, Other';
                                'cdu_spd_left_greens'='CDU, SPD, Left, Greens'='cdu_spd_fdp_other'='CDU, SPD, FDP, Other'='cdu_spd_fdp_left'='CDU, SPD, FDP, Left'; 'cdu_spd_fdp_left_greens'='CDU, SPD, FDP, Left, Greens';'cdu_afd'='CDU, AfD'; 'cdu_fdp_left'='CDU, FDP, Left'; 'spd'='SPD';
                                'cdu_fdp_other'='CDU, FDP, Other';'cdu_fw'='CDU, FW';
                                'cdu_left_greens'='CDU, Left, Greens'; 'cdu_other'='CDU, Other';
                                'cdu_spd_fdp'='CDU, SPD, FDP'; 'cdu_spd_fdp_greens'='CDU, SPD, FDP, Greens'; 'cdu_spd_fdp_left'='CDU, SPD, FDP, Left'")

dta_combined$predicted_coalition_factor <-  car::recode(dta_combined$predicted_coalition_stand, recode_coalition_predicted)

## recode CDU to CSU for Bavarian elections
dta_combined_no_bavaria <- dta_combined %>%
  filter(bundesland_merge != "bayern")

dta_combined_bavaria <- dta_combined %>%
  filter(bundesland_merge == "bayern")

dta_combined_bavaria <- dta_combined_bavaria %>%
  mutate(predicted_coalition_factor = str_replace(predicted_coalition_factor,
                                                  "CDU", "CSU"))

dta_combined <- bind_rows(dta_combined_bavaria,
                          dta_combined_no_bavaria)

## check that number of observations did not change
stopifnot(nrow(dta_combined) == nrow(dta_survey))

table(dta_combined$coalition_after_election,
      dta_combined$election_type)

table(dta_combined$predicted_coalition_stand,
      dta_combined$election_type)

## check whether coalition prediction was correct
dta_combined <- dta_combined %>% 
  mutate(correct_prediction_coalition = 
           if_else(coalition_after_election == predicted_coalition_stand, TRUE, FALSE)) 

table(dta_combined$correct_prediction_coalition,
      dta_combined$election_type)


dta_combined <- dta_combined %>% 
  mutate(correct_prediction_coalition_num = ifelse(correct_prediction_coalition == TRUE, 1, 0)) %>% 
  mutate(predicted_coalition_num = ifelse(predicted_coalition_stand == coalition_option_stand, 1, 0)) %>% 
  mutate(coalition_after_election_factor = coalition_after_election)

dta_combined$predicted_coalition_plot <-  car::recode(dta_combined$predicted_coalition_stand, recode_coalition_predicted)

dta_combined$coalition_option_print <-  car::recode(dta_combined$coalition_option_stand, recode_coalition_predicted)

dta_combined$coalition_after_election_plot <-  car::recode(dta_combined$coalition_after_election, recode_coalition_predicted)


## get English Land names
dta_combined <- dta_combined %>% 
  mutate(bundesland_english = str_replace_all(bundesland, "ue", "ü")) %>% 
  mutate(bundesland_english = str_replace_all(bundesland_english, "Bayern", "Bavaria")) %>% 
  mutate(bundesland_english = str_replace_all(bundesland_english, "Mecklenburg-Vorpommern", "Mecklenburg-Western Pomerania")) %>% 
  mutate(bundesland_english = str_replace_all(bundesland_english, "Hessen", "Hesse")) %>% 
  mutate(bundesland_english = str_replace_all(bundesland_english, "Nordrhein-Westfalen", "North Rhine-Westphalia")) %>% 
  mutate(bundesland_english = str_replace_all(bundesland_english, "Rheinland-Pfalz", "Rhineland-Palatinate")) %>% 
  mutate(bundesland_english = str_replace_all(bundesland_english, "Niedersachsen", "Lower Saxony")) %>% 
  mutate(bundesland_english = str_replace_all(bundesland_english, "Sachsen", "Saxony")) %>% 
  mutate(bundesland_english = str_replace(bundesland_english, "Thüringen", "Thuringia")) 


## create English election_id

dta_combined <- dta_combined %>% 
  mutate(election_id_english = paste(bundesland_english, year, sep = " "))

dta_combined <- dta_combined %>% 
  mutate(predicted_equals_true = ifelse(predicted_coalition_stand == coalition_after_election, TRUE, FALSE)) 

## select only the relevant variables
dta_select_reg <- dta_combined %>% 
  dplyr::select(year, year_election,
                election_id, 
                election_id_english,
                election_type, bundesland, 
                bundesland_english,
                respondent_id,
                age_stand,
                interest_campaign_stand,
                predict_coalition_type, 
                gender_stand, 
                education_stand,
                income_stand, 
                income_num,
                party_id_stand,
                coalition_option_stand,
                ref_perc_of_all_coalitions, 
                ref_total,
                coalition_option_print,
                predicted_coalition_plot,
                coalition_after_election_plot,
                correct_prediction_coalition,
                predicted_equals_true,
                predicted_coalition_dummy, 
                wish_coalition_dummy,
                number_predicted_coalitions_unique,
                number_polls,
                lr_self,
                avg_lr_coa_avg,
                avg_lr_coa_respondent,
                distance_lr_coa_self,
                distance_lr_avg_coa_self,
                distance_lr_coa_self_no_missing,
                lr_dummy_coa_missing,
                starts_with("lr_"),
                distance_coa_2_parties_no_missing,
                distance_coa_respondent_2_most_extreme_parties,
                distance_coa_avg_2_most_extreme_parties,
                predicted_coalition_stand,
                probability_surplus,
                election_date, 
                coalition_after_election, 
                coalition_before_election, 
                enp,
                pedersen_vol, 
                change_pm_party, lag_coalition_gles, 
                coalition_gles,
                probability_mean,
                probability_sd,
                probability, 
                spd_in_coa, cdu_in_coa) 


## create some new variables

dta_select_reg <- dta_select_reg %>% 
  mutate(probability_proportion = probability / 100) %>% 
  mutate(probability_proportion_surplus = probability_surplus / 100) %>% 
  mutate(predicted_equals_true = ifelse(coalition_option_stand == coalition_after_election, TRUE, FALSE)) %>% 
  mutate(incumbent_coalition = ifelse(coalition_before_election == coalition_option_stand, TRUE, FALSE)) %>% 
  mutate(coalition_before_election = ifelse(election_type == "State Election",
                                            coalition_before_election,
                                            ifelse(election_type == "Federal Election"& year == 2017, "cdu_spd", 
                                                   ifelse(election_type == "Federal Election" & year == 2013, "cdu_fdp", "cdu_spd")))) %>% 
  mutate(incumbent_reelected = ifelse(coalition_after_election == coalition_before_election, "Incumbent gov. reelected", "Incumbent gov. not reelected")) %>% 
  mutate(election_id = as.factor(election_id),
         gender_stand = as.factor(gender_stand),
         afd_dummy_strong = as.factor(ifelse(year >= 2015, "2015-2017 (strong)", "2010-2014 (weak/before foundation)")),
         wish_coalition_dummy = as.factor(wish_coalition_dummy),
         year = as.numeric(year)) 


## code coalition that was formed after the election (add for federal elections)
dta_select_reg <- dta_select_reg %>% 
  mutate(coalition_after_election = ifelse(election_id == "Federal Election 2009", "cdu_fdp",
                                           ifelse(election_id == "Federal Election 2013", "cdu_spd",
                                                  ifelse(election_id == "Federal Election 2017", "cdu_spd", 
                                                         coalition_after_election))))



## create dummy: incument party re-elected?
dta_select_reg <- dta_select_reg %>% 
  mutate(incumbent_reelected = ifelse(election_id == "Federal Election 2009", "Incumbent gov. not reelected",
                                      ifelse(election_id == "Federal Election 2013", "Incumbent gov. not reelected",
                                             ifelse(election_id == "Federal Election 2017", "Incumbent gov. reelected", incumbent_reelected)))) %>% 
  mutate(incumbent_reelected = as.factor(incumbent_reelected)) %>% 
  mutate(federal_state_dummy = ifelse(str_detect(election_id, "Federal"), "Federal", "State"))



## get binary indicator: is government option the incumbent government
## (missing for federal elections)

## filter federal elections and code incumbent coalitions
dta_select_federal <- dta_select_reg %>% 
  filter(str_detect(election_id, "Federal")) %>% 
  mutate(incumbent_coalition = ifelse(election_id == "Federal Election 2009" & coalition_option_stand == "cdu_spd", TRUE,
                                      ifelse(election_id == "Federal Election 2013" & coalition_option_stand == "cdu_fdp", TRUE,
                                             ifelse(election_id == "Federal Election 2017" & coalition_option_stand == "cdu_spd", TRUE, FALSE))))

## check whether predicted coalition equals the true coalition
dta_select_federal <- dta_select_federal %>% 
  mutate(predicted_equals_true = ifelse(predicted_coalition_stand == coalition_after_election, TRUE, FALSE)) 

## select state elections and bind state and elections back to data frame
dta_select_land <- dta_select_reg %>% 
  filter(!str_detect(election_id, "Federal")) 

table(dta_select_land$predicted_equals_true, useNA = "always")
table(dta_select_federal$predicted_equals_true, useNA = "always")

dta_select_reg <- bind_rows(dta_select_federal, dta_select_land)

table(dta_select_reg$incumbent_coalition,
      dta_select_reg$federal_state_dummy)


## create new variables for media reports on coalitions
dta_select_reg <- dta_select_reg %>% 
  mutate(ref_total = ifelse(is.na(ref_total), 0, ref_total)) %>%  ## if coalition not mentioned at all, assign value 0
  mutate(ref_perc_of_all_coalitions = ifelse(is.na(ref_perc_of_all_coalitions), 0, ref_perc_of_all_coalitions)) %>%  ## if coalition not mentioned at all, assign value 0
  group_by(election_id) %>% 
  ungroup() 


## prepare Worscores data

## load Wordscores from Bräuninger et al (2020) book (part of Appendix)
## we use the overarching left-right scores

dat_wordscores <- read_csv("data_parties_all_wordscores.csv")

## rescale Wordscores from 0 to 10 where 0 is the manifesto with the lowest score
## in the sample and 10 is the manifesto with the highest score
dat_wordscores <- dat_wordscores %>% 
  mutate(wordscore_leftright_0_10 = scales::rescale(wordscore_leftright, 
                                                    to = c(0, 10)))

summary(dat_wordscores$wordscore_leftright_0_10)


## change dataset to wide format to have one observation per election
## and one column per party
## this is the format also used for voters' left-right evalutions parties
## and allows us to estimate the same type of variables using the Wordscores

dat_wordscores_wide <- dat_wordscores %>% 
  select(election_id, party, wordscore_leftright_0_10) %>% 
  spread(party, wordscore_leftright_0_10)

names(dat_wordscores_wide)

colnames(dat_wordscores_wide) <- paste("ws_lr", 
                                       colnames(dat_wordscores_wide), 
                                       sep = "_")

## rename election_id
dat_wordscores_wide <- dat_wordscores_wide %>% 
  rename(election_id = ws_lr_election_id)

## merge Wordscores observations with respondent-choice level dataset
dta_select_reg <- left_join(dta_select_reg, dat_wordscores_wide,
                            by = "election_id")

## use these values to estimate the absolute differences in left-right values between the parties

table(dta_select_reg$coalition_option_stand)
length(unique(dta_select_reg$coalition_option_stand))

dta_select_reg <- dta_select_reg %>%
  group_by(election_id) %>%
  mutate(distance_coa_ws_2_most_extreme_parties = case_when(
    coalition_option_stand == "cdu_fdp" ~ abs(ws_lr_cdu - ws_lr_fdp),
    coalition_option_stand == "cdu_afd" ~ abs(ws_lr_afd - ws_lr_cdu),
    coalition_option_stand == "cdu_fdp_greens" ~ abs(ws_lr_cdu - ws_lr_greens),
    coalition_option_stand == "cdu_greens" ~ abs(ws_lr_cdu - ws_lr_greens),
    coalition_option_stand == "cdu_spd" ~ abs(ws_lr_cdu - ws_lr_spd),
    coalition_option_stand == "spd_fdp_greens" ~ abs(ws_lr_fdp - ws_lr_greens),
    coalition_option_stand == "spd_greens_fw" ~ abs(ws_lr_fw - ws_lr_greens),
    coalition_option_stand == "cdu_fw" ~ abs(ws_lr_fw - ws_lr_cdu),
    coalition_option_stand == "spd_greens" ~ abs(ws_lr_spd - ws_lr_greens),
    coalition_option_stand == "spd_left" ~ abs(ws_lr_spd - ws_lr_left),
    coalition_option_stand == "spd_fdp" ~ abs(ws_lr_fdp - ws_lr_spd),
    coalition_option_stand == "spd_left_greens" ~ abs(ws_lr_spd - ws_lr_left),
    coalition_option_stand == "cdu" ~ 0,
    coalition_option_stand == "spd" ~ 0
  )) %>%
  ungroup()

hist(dta_select_reg$distance_coa_ws_2_most_extreme_parties)
hist(dta_select_reg$distance_coa_respondent_2_most_extreme_parties)

## use these values to estimate the average left-right position of all coalition parties (unweighted)
dta_select_reg <- dta_select_reg %>%
  group_by(election_id) %>%
  mutate(avg_lr_coa_ws = case_when(
    coalition_option_stand == "cdu_fdp" ~ (ws_lr_cdu + ws_lr_fdp) / 2,
    coalition_option_stand == "cdu_fdp_greens" ~ (ws_lr_cdu + ws_lr_greens + ws_lr_fdp) / 3,
    coalition_option_stand == "cdu_greens" ~ (ws_lr_cdu + ws_lr_greens) / 2,
    coalition_option_stand == "cdu_spd" ~ (ws_lr_spd + ws_lr_cdu) / 2,
    coalition_option_stand == "spd_fdp_greens" ~ (ws_lr_greens + ws_lr_fdp + ws_lr_spd) / 3,
    coalition_option_stand == "spd_greens" ~ (ws_lr_spd + ws_lr_greens) / 2,
    coalition_option_stand == "spd_left" ~ (ws_lr_spd + ws_lr_left) / 2,
    coalition_option_stand == "spd_fdp" ~ (ws_lr_spd + ws_lr_fdp) / 2,
    coalition_option_stand == "spd_greens_fw" ~ (ws_lr_greens + ws_lr_spd  + ws_lr_fw) / 3,
    coalition_option_stand == "cdu_fw" ~ (ws_lr_cdu + ws_lr_fw) / 2,
    coalition_option_stand == "spd_left_greens" ~ (ws_lr_spd + ws_lr_left + ws_lr_greens) / 3,
    coalition_option_stand == "cdu" ~ ws_lr_cdu,
    coalition_option_stand == "spd" ~ ws_lr_spd
  )) %>%
  mutate(distance_ws_lr_coa_self = abs(avg_lr_coa_ws - lr_self)) %>%
  ungroup()

table(dta_select_reg$predicted_equals_true)


## merge the number of all coalitions on the Länder level between 1990 and 2017
## based on 106 elections


dta_coalitions <- read.csv("data_coalitions_laender.csv")

## check whether any coalitions have not been coded correctly
setdiff(unique(dta_coalitions$government_parties),
        unique(dta_select_reg$coalition_option_stand))



## merge information on count of formed coalitions with main dataset
dta_select_reg <- left_join(dta_select_reg, dta_coalitions)

## create indicator that signals whether a coalition option has been formed
## at least five times on the state level

dta_select_reg <- dta_select_reg %>% 
  mutate(coalition_formed_at_least_5_times = ifelse(coalitions_formed19902017_abs > 5, 
                                                    "TRUE", "FALSE")) 

## recode NA as false
table(dta_select_reg$coalition_formed_at_least_5_times, useNA = "always")

dta_select_reg <- dta_select_reg %>% 
  mutate(coalition_formed_at_least_5_times = ifelse(is.na(coalition_formed_at_least_5_times), 
                                                    "FALSE", coalition_formed_at_least_5_times)) 

table(dta_select_reg$coalition_formed_at_least_5_times, useNA = "always")

## save dataset which will be used for all subsequent analyses
## note that respondents who did not make predictions are still 
## included in the dataset
saveRDS(dta_select_reg, "data_reg_full.rds")


