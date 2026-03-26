# Libraries ---------------------------------------------------------------
# Handle relative filepaths on everyone's machines
library(here)

# Data encryption
library(cyphr)

# Data manipulation
library(tidyverse)
library(lubridate)

# String templating
library(glue)


# Load Data ---------------------------------------------------------------
message("Loading data...")

# Vimeo dictionary
vimeo <- read.csv("data/vimeo_doctors_t2.csv")


# Helpers -----------------------------------------------------------------
# Convert strings to snake case
to_snake <- function(str) {
  
  # Drop parens
  str_clean <- str_remove_all(str, "\\(|\\)")
  
  # Remove whitespace
  str_clean <- str_replace_all(str_clean, " +", "_")
  
  # Insert underscore before camel case letters
  # see: https://stackoverflow.com/questions/39036688/using-captured-groups-in-str-replace-stri-replace-stringi-vs-stringr
  str_clean <- stringi::stri_replace_all(str_clean,
                                         regex="(?<=[a-z])([A-Z])",
                                         replacement="_$1")
  
  tolower(str_clean)
}

# Adds columns, treating NA as zero
sum_cols <- function(...) { rowSums(data.frame(list(...)), na.rm = FALSE) } #MK: used to be true and added NAs as 0

# Grab entries from a dictionary, based on matching keys in the data and the dict
# Used to add information about Vimeo videos to our dataframe
match_to_dict <- function(dict_entry, dict_key, data_key, dict = vimeo) {
  
  entries <- dict[[dict_entry]]
  matches <- match(data_key, dict[[dict_key]])
  entries[matches]
  
}

str_detect_all <- function(string, patterns) {
  detects <- map(patterns, str_detect, string = string)
  reduce(detects, `&`)
}

fct_detect_any <- function(fct, levels) {
  
  detects <- map(levels, `==`, fct)
  reduce(detects, `|`)
}

# Groups of Columns, by How They Should Be Handled ------------------------
numeric_cols <- c("progress", "duration_in_seconds", "standard_hhi",
                  "standard_education")

yn_cols <- c(glue("public_behavior_{1:6}"),
             glue("public_behavior_{1:6}_followup"),
             "own_disposable_masks",
             "own_disposable_masks_followup",
             "asymptomatic_infect",
             "asymptomatic_infect_followup")
recode_yn <- function(x) { as.integer(x == "Yes") }

freq_cols   <- c(glue("safety_behaviors_{1:4}"),
                 glue("safety_behaviors_{1:4}_followup"))
freq_levels <- c("Never", "Sometimes", "About half the time",
                 "Most of the time", "Always")

agree_cols <- c(glue("priming_video_rating_{1:2}"),
                glue("video_1_rating_{1:4}"),
                glue("video_2_rating_{1:4}"),
                glue("video_3_rating_{1:4}"))
agree_levels <- c("Strongly disagree", "Disagree", "Somewhat disagree",
                  "Neither agree nor disagree",
                  "Somewhat agree", "Agree", "Strongly agree")

case_count_cols <- c("cases_prior_bin", "cases_post_bin",
                     "cases_prior_bin_followup")

case_count_levels <- c("Between 0 and 1,000 each day",
                       "Between 1,001 and 10,000 each day",
                       "Between 10,001 and 100,000 each day",
                       "Between 100,001 and 200,000 each day")

link_cols   <- glue("link_choices_{1:5}")
recode_link <- function(x) { as.integer(x == "I would like this link") }

persons_met_levels <- c("None", "1-3", "4-10", "11-20", "21-30", "31 or more")
reusable_masks_levels <- c("No", "Yes, between 1 and 3 in the home.",
                           "Yes, more than 3 in the home.")

# Process Data ------------------------------------------------------------
message("Processing data...")
# Data without header rows, and with column names in snake case
responses <- launch%>%
  rename_all(to_snake) %>%
  filter(status == "IP Address")

# Convert columns to right types
responses_typed <- responses %>%
  # Dates:
  mutate(across(ends_with("_date"), as_datetime)) %>%
  # Decimals and Floats:
  mutate(across(starts_with("timing_"), as.numeric),
         across(starts_with("video_priming_"), as.numeric),
         across(starts_with("video1_"), as.numeric),
         across(starts_with("video2_"), as.numeric),
         across(starts_with("video3_"), as.numeric),
         across(matches("cases_(prior|post)_.+_1"), as.numeric),
         across(all_of(numeric_cols), as.numeric)) %>%
  # Integers
  mutate(across(all_of(yn_cols), recode_yn),
         across(all_of(link_cols), recode_link),
         across(matches("death_mult"), as.integer),
         across(matches("char_+."), as.integer),
         wtp_masks_1 = as.integer(wtp_masks_1),
         age = as.integer(age),
         age_followup = as.integer(age_followup))%>%
  # Factors:
  mutate(across(all_of(freq_cols), factor, levels = freq_levels),
         across(all_of(agree_cols), factor, levels = agree_levels),
         across(all_of(case_count_cols), factor, levels = case_count_levels),
         across(starts_with("own_reusable"), factor, levels = reusable_masks_levels),
         across(starts_with("persons_met"), factor, levels = persons_met_levels),
         age_bin = case_when(age < 25 ~ "17_to_24",
                             age < 45 ~ "25_to_44",
                             age < 65 ~ "45_to_64",
                             !is.na(age) ~ "65_plus"))


# Subset to responses after the launch was ordered
responses_dated <- filter(responses_typed,
                          start_date >= ymd_hms("2020-08-07 16:36:37 UTC"))

# Consolidate fields that are elicited across several questions:
responses_cons <- responses_dated %>%
  mutate(finished = as.logical(finished),
         attn_pass_color  = str_detect(tolower(attn_check_color), "puce|puc|uce|pu e|punc|pus|pice|cupe"),
         attn_pass_likely = attn_check_likely == "Somewhat likely",
         # Record missing values as failures:
         attn_pass_color  = ifelse(is.na(attn_pass_color), FALSE, attn_pass_color), 
         attn_pass_likely = ifelse(is.na(attn_pass_likely), FALSE, attn_pass_likely),
         attn_pass        = attn_pass_color | attn_pass_likely,
         cases_prior = coalesce(cases_prior_hun_1,
                                cases_prior_thou_1,
                                cases_prior_ten_thou_1,
                                cases_prior_hun_thou_1),
         cases_post = coalesce(cases_post_hun_1,
                               cases_post_thou_1,
                               cases_post_ten_thou_1,
                               cases_post_hun_thou_1),
         cases_followup = coalesce(cases_prior_hun_1_followup,
                                   cases_prior_thou_1_followup,
                                   cases_prior_ten_thou_1_followup,
                                   cases_prior_hun_thou_1_followup),
         bw_death_mult_prior = coalesce(b_death_mult_prior_1,
                                        w_death_mult_prior_1),
         b_death_mult_prior = ifelse(bw_death_prior != "Black individuals are more likely to die than White individuals",0,b_death_mult_prior_1),
         bw_death_mult_post = coalesce(b_death_mult_post_14,
                                       w_death_mult_post_8),

         b_death_mult_post = ifelse(!is.na(w_death_mult_post_8),0,b_death_mult_post_14),
         bw_death_mult_followup = coalesce(b_death_mult_prior_1_followup,
                                           w_death_mult_prior_1_followup),
         
         charity_covid = coalesce(char_cov_cov_1st_sli_1,
                                  1000 - char_cov_non_1st_sli_1),
         charity_covid = 1000 - charity_covid, #MK: sounds counterintuitive, but it's the other way around
         charity_covid_order = case_when(!is.na(char_cov_cov_1st_sli_1) ~ "covid_1st",
                                         !is.na(char_cov_non_1st_sli_1) ~ "covid_2nd"),
         charity_black = coalesce(char_blk_blk_1st_sli_1,
                                  1000 - char_blk_gen_1st_sli_1),
         charity_black = 1000 - charity_black, #MK: sounds counterintuitive, but it's the other way around
         charity_black_order = case_when(!is.na(char_blk_blk_1st_sli_1) ~ "black_1st",
                                         !is.na(char_blk_gen_1st_sli_1) ~ "black_2nd"),
         #new versions of links_total and links_any are created further
         #links_total = sum_cols(link_choices_1, link_choices_2, link_choices_3,
            #                    link_choices_4, link_choices_5),
         #links_any   = as.integer(links_total > 0), #MK: used to be greater than 1, I'm guessing it was a typo
         public_behavior_total = sum_cols(public_behavior_1, public_behavior_2,
                                          public_behavior_3, public_behavior_4,
                                          public_behavior_5, public_behavior_6),
         public_behavior_any   = as.integer(public_behavior_total > 0),
         # Kenosha indicators
         pre_kenosha = as.integer(start_date < ymd_hms("2020-08-23 00:00:01 UTC")),
         post_kenosha = as.integer(start_date > ymd_hms("2020-08-26 23:59:59 UTC")),
         # Optimism/pesimism indicators
         deaths_prior_underest = as.integer(bw_death_mult_prior < 4),
         deaths_prior_overest = as.integer(bw_death_mult_prior > 4)) %>%
  # Drop components
  select(-attn_pass_puce,
         -matches("cases_(prior|post)_.+_1"),
         -starts_with("char_"),
         -b_death_mult_prior_1, -w_death_mult_prior_1,
         -b_death_mult_post_14, -w_death_mult_post_8,
         -b_death_mult_prior_1_followup, -w_death_mult_prior_1_followup)

# Add treatment and video information
responses_trt <- responses_cons %>%
  # Videos (see match_to_dict helper function)
  mutate(video_prime_id = substr(video_priming, 32, 40),
         video_1_id     = substr(video1, 32, 40),
         video_2_id     = substr(video2, 32, 40),
         video_3_id     = substr(video3, 32, 40),
         # Speaker race:
         video_prime_race = match_to_dict(dict_entry = "speaker_race",
                                          dict_key   = "video_id",
                                          data_key   = video_prime_id),
         video_1_race     = match_to_dict(dict_entry = "speaker_race",
                                          dict_key   = "video_id",
                                          data_key   = video_1_id),
         video_2_race     = match_to_dict(dict_entry = "speaker_race",
                                          dict_key   = "video_id",
                                          data_key   = video_2_id),
         video_3_race     = match_to_dict(dict_entry = "speaker_race",
                                          dict_key   = "video_id",
                                          data_key   = video_3_id),
         # Speaker sex:
         video_prime_sex = match_to_dict(dict_entry = "speaker_gender",
                                         dict_key   = "video_id",
                                         data_key   = video_prime_id),
         video_1_sex     = match_to_dict(dict_entry = "speaker_gender",
                                         dict_key   = "video_id",
                                         data_key   = video_1_id),
         video_2_sex     = match_to_dict(dict_entry = "speaker_gender",
                                         dict_key   = "video_id",
                                         data_key   = video_2_id),
         video_3_sex     = match_to_dict(dict_entry = "speaker_gender",
                                         dict_key   = "video_id",
                                         data_key   = video_3_id),
         # Speaker name:
         video_prime_name = match_to_dict(dict_entry = "speaker_name",
                                          dict_key   = "video_id",
                                          data_key   = video_prime_id),
         video_1_name     = match_to_dict(dict_entry = "speaker_name",
                                          dict_key   = "video_id",
                                          data_key   = video_1_id),
         video_2_name     = match_to_dict(dict_entry = "speaker_name",
                                          dict_key   = "video_id",
                                          data_key   = video_2_id),
         video_3_name     = match_to_dict(dict_entry = "speaker_name",
                                          dict_key   = "video_id",
                                          data_key   = video_3_id)) %>%
  # Treatment flags
  mutate(ethnicity = case_when(ethnicity == 1 ~ "white",
                               ethnicity == 2 ~ "black",
                               !is.na(ethnicity) ~ "other",
                               TRUE ~ NA_character_),
         black_doc =  as.integer(tmt_speaker == "black"),
         concord_ama  = as.integer(ama_speaker == ethnicity),
         concord_doc  = as.integer(tmt_speaker == ethnicity),
         concord_both = as.integer(concord_ama & concord_doc),
         concord_ama_only = as.integer(concord_ama * (1-concord_doc)),
         concord_doc_only = as.integer(concord_doc * (1-concord_ama)),
         concord_any  = as.integer(concord_ama | concord_doc),
         concord_some  = as.integer((concord_ama | concord_doc) & !(concord_ama & concord_doc)),
         racism_ama   = as.integer(ama_message == "racism"),
         racism_doc   = as.integer(tmt_message == "covid_plus_racism"),
         discrepancy_doc = racism_doc, #relabeling
         racism_both  = as.integer(racism_ama & racism_doc),
         racism_any   = as.integer(racism_ama | racism_doc),
         covid_any    = as.integer(str_detect(tmt_message, "covid")),
         any_black_treatment = as.integer(racism_ama | racism_doc | black_doc),
         all_black_treatments = as.integer(racism_ama & racism_doc & black_doc),
         covid_only   = as.integer(tmt_message == "covid_only"),
         pure_placebo = as.integer(ama_message == "placebo" & tmt_message == "placebo"),
         # Interactions
         covid_any__racism_doc  = covid_any * racism_doc,
         covid_any__discrepancy_doc  = covid_any__racism_doc, #relabeling
         covid_any__racism_ama  = covid_any * racism_ama,
         covid_any__racism_any  = covid_any * racism_any,
         covid_any__black_doc = covid_any * black_doc,
         covid_any__concord_doc = covid_any * concord_doc,
         covid_any__concord_any = covid_any * concord_any,
         covid_any__racism_ama__concord_any = covid_any * racism_ama * concord_any,
         concord_any__racism_ama = concord_any * racism_ama,
         covid_any__concord_ama__racism_ama = covid_any * concord_ama * racism_ama,
         concord_ama__racism_ama = concord_ama * racism_ama,
         concord_ama__concord_doc = concord_ama * concord_doc,
         concord_ama__concord_doc__racism_ama__covid_any = concord_ama * concord_doc * racism_ama * covid_any,
         concord_ama__covid_any = concord_ama * covid_any,
         concord_doc__racism_doc = concord_doc * racism_doc,
         concord_doc__discrepancy_doc = concord_doc__racism_doc,
         racism_ama__racism_doc = racism_ama * racism_doc,
         racism_ama__discrepancy_doc = racism_ama__racism_doc,
         concord_ama__racism_doc = concord_ama * racism_doc,
         concord_ama__discrepancy_doc = concord_ama__racism_doc,
         racism_ama__concord_ama__racism_doc = racism_ama * concord_ama * racism_doc,
         racism_ama__concord_ama__discrepancy_doc = racism_ama__concord_ama__racism_doc,
         deaths_prior_underest__racism_doc = deaths_prior_underest * racism_doc,
         deaths_prior_underest__discrepancy_doc = deaths_prior_underest__racism_doc,
         deaths_prior_overest__racism_doc = deaths_prior_overest * racism_doc,
         deaths_prior_overest__discrepancy_doc = deaths_prior_overest__racism_doc,
         pre_kenosha__racism_doc = pre_kenosha * racism_doc,
         pre_kenosha__discrepancy_doc = pre_kenosha__racism_doc,
         post_kenosha__racism_doc = post_kenosha * racism_doc,
         post_kenosha__discrepancy_doc = post_kenosha__racism_doc,
         pre_kenosha__covid_any = pre_kenosha * covid_any,
         pre_kenosha__concord_ama = pre_kenosha * concord_ama,
         post_kenosha__covid_any = post_kenosha * covid_any,
         post_kenosha__concord_ama = post_kenosha * concord_ama,
         # Interactions for graphs
         covid_any_x_concord_doc = covid_any * concord_doc,
         covid_any_x_discord_doc = covid_any * (1-concord_doc),
         covid_any_x_concord_doc_x_racism_ama = covid_any * concord_doc * racism_ama,
         covid_any_x_discord_doc_x_racism_ama = covid_any * (1-concord_doc) * racism_ama,
         placebo_doc_x_racism_ama = (1-covid_any) * racism_ama,
         covid_any_x_concord_doc_x_placebo_ama = covid_any * concord_doc * (1-racism_ama),
         covid_any_x_discord_doc_x_placebo_ama = covid_any * (1-concord_doc) * (1-racism_ama),
         covid_any_x_racism_ama = covid_any * racism_ama,
         covid_any_only = covid_any * (1-racism_ama),
         racism_ama_only = (1-covid_any) * racism_ama
  )

# Rename columns
responses_rnm <- responses_trt %>% 
  rename_all(str_replace, "video1", "video_1") %>%
  rename_all(str_replace, "video2", "video_2") %>%
  rename_all(str_replace, "video3", "video_3") %>%
  rename_all(str_replace, "video", "vid") %>%
  rename_all(str_replace, "priming", "prime") %>%
  rename(ama_race = ama_speaker,
         tmt_race = tmt_speaker,
         wtp_masks = wtp_masks_1,
         safety_mask_in  = safety_behaviors_1,
         safety_mask_out = safety_behaviors_2,
         safety_hands    = safety_behaviors_3,
         safety_distance = safety_behaviors_4,
         safety_mask_in_followup  = safety_behaviors_1_followup,
         safety_mask_out_followup = safety_behaviors_2_followup,
         safety_hands_followup    = safety_behaviors_3_followup,
         safety_distance_followup = safety_behaviors_4_followup,
         public_eat              = public_behavior_1,
         public_movie            = public_behavior_2,
         public_groceries        = public_behavior_3,
         public_shopping         = public_behavior_4,
         public_transit_work     = public_behavior_5,
         public_transit_personal = public_behavior_6,
         public_eat_followup              = public_behavior_1_followup,
         public_movie_followup            = public_behavior_2_followup,
         public_groceries_followup        = public_behavior_3_followup,
         public_shopping_followup         = public_behavior_4_followup,
         public_transit_work_followup     = public_behavior_5_followup,
         public_transit_personal_followup = public_behavior_6_followup,
         link_exercise      =  link_choices_1,
         link_state_hotline =  link_choices_2,
         link_testing       =  link_choices_3,
         link_mgh_resources =  link_choices_4,
         link_mgh_app       =  link_choices_5)

#Creation of link variable for intended behavior:

responses_rnm$link_exercise  = ifelse(is.na(responses_rnm$link_exercise)*(!is.na(responses_rnm$timing_link_choice_page_submit)), 0, responses_rnm$link_exercise)
responses_rnm$link_state_hotline  = ifelse(is.na(responses_rnm$link_state_hotline)*(!is.na(responses_rnm$timing_link_choice_page_submit)), 0, responses_rnm$link_state_hotline)
responses_rnm$link_testing   = ifelse(is.na(responses_rnm$link_testing )*(!is.na(responses_rnm$timing_link_choice_page_submit)), 0, responses_rnm$link_testing )
responses_rnm$link_mgh_resources  = ifelse(is.na(responses_rnm$link_mgh_resources)*(!is.na(responses_rnm$timing_link_choice_page_submit)), 0, responses_rnm$link_mgh_resources)
responses_rnm$link_mgh_app  = ifelse(is.na(responses_rnm$link_mgh_app )*(!is.na(responses_rnm$timing_link_choice_page_submit)), 0, responses_rnm$link_mgh_app )

responses_rnm$links_total = responses_rnm$link_exercise + responses_rnm$link_state_hotline + responses_rnm$link_testing + responses_rnm$link_mgh_resources + responses_rnm$link_mgh_app 
responses_rnm$links_any   = as.integer(responses_rnm$links_total > 0)


# Further recoding and column creation
agreed <- c("Somewhat agree", "Strongly agree")
often <- c("Most of the time", "Always")
always <- c("Always")

practices <- c("Stay outdoors as much as possible and stay six feet away from other people",
               "Wash your hands before going outside and when you come home",
               "Wear a mask or facial covering")

practices_followup <- c("Stay outdoors as much as possible, and stay six feet away from other people",
                        "Wash your hands before going outside and when you come home",
                        "Wear a mask or facial covering")

symptoms <- c("Fever", "Cough", "Difficulty breathing", "New loss of taste or smell") #MK: included anosmia

responses_rcd <- responses_rnm %>%
  mutate(party_dem = standard_political_party == 1,
         party_rep = standard_political_party == 2,
         hhi_above_60k           = case_when(standard_hhi == -3105 ~ NA,
                                             # Else:
                                             TRUE ~ standard_hhi >=11),
         hhi_above_60k           = hhi_above_60k,
         hs_graduate             = standard_education >= 4,
         race_black              = ethnicity == "black",
         race_white              = ethnicity == "white",
         sex_female              = gender == "2",
         sex_male                = gender == "1",
         black_more_likely_to_die = bw_death_prior == "Black individuals are more likely to die than White individuals",
         # TODO: calculate these based on case counts in days leading up to survey (MK: done)
         cases_prior_underest      = cases_prior    < 60184,
         cases_prior_overest       = cases_prior    > 60184,
         cases_post_underest       = cases_post     < 60184,
         cases_post_overest        = cases_post     > 60184,
         cases_followup_underest   = cases_followup < 60184,
         cases_followup_overest    = cases_followup > 60184,
         vid_prime_useful          = prime_vid_rating_1 %in% agreed,
         vid_prime_trustworthy     = prime_vid_rating_2 %in% agreed,
         vid_1_useful              = vid_1_rating_1 %in% agreed,
         vid_1_trustworthy         = vid_1_rating_2 %in% agreed,
         vid_1_follow_recs         = vid_1_rating_3 %in% agreed,
         vid_1_will_share          = vid_1_rating_4 %in% agreed,
         vid_2_useful              = vid_2_rating_1 %in% agreed,
         vid_2_trustworthy         = vid_2_rating_2 %in% agreed,
         vid_2_follow_recs         = vid_2_rating_3 %in% agreed,
         vid_2_will_share          = vid_2_rating_4 %in% agreed,
         vid_3_useful              = vid_3_rating_1 %in% agreed,
         vid_3_trustworthy         = vid_3_rating_2 %in% agreed,
         vid_3_follow_recs         = vid_3_rating_3 %in% agreed,
         vid_3_will_share          = vid_3_rating_4 %in% agreed,
         vid_prime_useful          = ifelse(is.na(prime_vid_rating_1), NA, vid_prime_useful     ),
         vid_prime_trustworthy     = ifelse(is.na(prime_vid_rating_2), NA, vid_prime_trustworthy),
         vid_1_useful              = ifelse(is.na(vid_1_rating_1), NA, vid_1_useful     ),
         vid_1_trustworthy         = ifelse(is.na(vid_1_rating_2), NA, vid_1_trustworthy),
         vid_1_follow_recs         = ifelse(is.na(vid_1_rating_3), NA, vid_1_follow_recs),
         vid_1_will_share          = ifelse(is.na(vid_1_rating_4), NA, vid_1_will_share ),
         vid_2_useful              = ifelse(is.na(vid_2_rating_1), NA, vid_2_useful     ),
         vid_2_trustworthy         = ifelse(is.na(vid_2_rating_2), NA, vid_2_trustworthy),
         vid_2_follow_recs         = ifelse(is.na(vid_2_rating_3), NA, vid_2_follow_recs),
         vid_2_will_share          = ifelse(is.na(vid_2_rating_4), NA, vid_2_will_share ),
         vid_3_useful              = ifelse(is.na(vid_3_rating_1), NA, vid_3_useful     ),
         vid_3_trustworthy         = ifelse(is.na(vid_3_rating_2), NA, vid_3_trustworthy),
         vid_3_follow_recs         = ifelse(is.na(vid_3_rating_3), NA, vid_3_follow_recs),
         vid_3_will_share          = ifelse(is.na(vid_3_rating_4), NA, vid_3_will_share ),
         vid_prime_index           = (vid_prime_useful+vid_prime_trustworthy)/2,
         vid_1_index               = (vid_1_useful+vid_1_trustworthy+vid_1_follow_recs+vid_1_will_share)/4,
         vid_2_index               = (vid_2_useful+vid_2_trustworthy+vid_2_follow_recs+vid_2_will_share)/4,
         vid_3_index               = (vid_3_useful+vid_3_trustworthy+vid_3_follow_recs+vid_3_will_share)/4,
         vid_all_index              = (vid_prime_index+vid_1_index+vid_2_index+vid_3_index)/4,
         know_3_practices          = str_detect_all(how_to_prevent, practices),
         know_3_practices_followup = str_detect_all(how_to_prevent_followup, practices_followup),
         know_4_symptoms           = str_detect_all(covid_symptoms, symptoms),
         know_4_symptoms_followup  = str_detect_all(covid_symptoms_followup, symptoms),
         know_gap_index            = 1-(know_3_practices+know_4_symptoms)/2,
         know_gap_index_followup   = 1-(know_3_practices_followup+know_4_symptoms_followup)/2,
         know_six_feet = str_detect_all(how_to_prevent, "Stay outdoors as much as possible and stay six feet away from other people"),
         know_wash_hands = str_detect_all(how_to_prevent, "Wash your hands before going outside and when you come home" ),
         know_mask =  str_detect_all(how_to_prevent, "Wear a mask or facial covering" ),
         know_fever =  str_detect_all(covid_symptoms, "Fever" ),
         know_cough =  str_detect_all(covid_symptoms, "Cough" ),
         know_breathing =  str_detect_all(covid_symptoms, "Difficulty breathing"  ),
         know_taste =  str_detect_all(covid_symptoms, "New loss of taste or smell" ),
         know_asympt = asymptomatic_infect,
         know_mask_in = str_detect_all(when_to_mask, "Indoors, at all times" ),
         know_mask_out = str_detect_all(when_to_mask, "Outdoors when it is impossible to stay six feet away from people" ),
         know_gap_count =  ifelse(is.na(timing_knowledge_page_submit), NA,10 -know_mask_in - know_mask_out - know_asympt - know_six_feet - know_wash_hands -  know_mask - know_fever -  know_cough -  know_breathing - know_taste),
         any_knowledge_gap = as.integer(know_gap_count>0),
         
         #knowledge gap follow up:
         know_six_feet_followup = str_detect_all(how_to_prevent_followup, "Stay outdoors as much as possible and stay six feet away from other people"),
         know_wash_hands_followup = str_detect_all(how_to_prevent_followup, "Wash your hands before going outside and when you come home" ),
         know_mask_followup =  str_detect_all(how_to_prevent_followup, "Wear a mask or facial covering" ),
         know_fever_followup =  str_detect_all(covid_symptoms_followup, "Fever" ),
         know_cough_followup =  str_detect_all(covid_symptoms_followup, "Cough" ),
         know_breathing_followup =  str_detect_all(covid_symptoms_followup, "Difficulty breathing"  ),
         know_taste_followup =  str_detect_all(covid_symptoms_followup, "New loss of taste or smell" ),
         know_asympt_followup = asymptomatic_infect_followup,
         know_mask_in_followup = str_detect_all(when_to_mask_followup, "Indoors, at all times" ),
         know_mask_out_followup = str_detect_all(when_to_mask_followup, "Outdoors when it is impossible to stay six feet away from people" ),
         know_gap_count_followup =  ifelse(is.na(timing_knowledge_page_submit_followup), NA,10 -know_mask_in_followup - know_mask_out_followup - know_asympt_followup - know_six_feet_followup - know_wash_hands_followup -  know_mask_followup - know_fever_followup -  know_cough_followup -  know_breathing_followup - know_taste_followup),
         any_knowledge_gap_followup = as.integer(know_gap_count_followup>0),

         # Binarized safety behavior
         safety_mask_in_often   = safety_mask_in %in% often,
         safety_mask_out_often  = safety_mask_out %in% often,
         safety_hands_often     = safety_hands %in% often,
         safety_distance_often  = safety_distance %in% often,
         safety_mask_in_often_followup   = safety_mask_in_followup %in% often,
         safety_mask_out_often_followup  = safety_mask_out_followup %in% often,
         safety_hands_often_followup     = safety_hands_followup %in% often,
         safety_distance_often_followup  = safety_distance_followup %in% often,
         safety_mask_in_often   = ifelse(is.na(safety_mask_in), NA, safety_mask_in_often),
         safety_mask_out_often  = ifelse(is.na(safety_mask_out), NA, safety_mask_out_often),
         safety_hands_often     = ifelse(is.na(safety_hands), NA, safety_hands_often),
         safety_distance_often  = ifelse(is.na(safety_distance), NA, safety_distance_often),
         safety_mask_in_often_followup   = ifelse(is.na(safety_mask_in_followup), NA, safety_mask_in_often_followup),
         safety_mask_out_often_followup  = ifelse(is.na(safety_mask_out_followup), NA, safety_mask_out_often_followup),
         safety_hands_often_followup     = ifelse(is.na(safety_hands_followup), NA, safety_hands_often_followup),
         safety_distance_often_followup  = ifelse(is.na(safety_distance_followup), NA, safety_distance_often_followup),
         safety_all = safety_mask_in_often*safety_mask_out_often*safety_hands_often*safety_distance_often,
         safety_total =  sum_cols(safety_mask_in_often, safety_mask_out_often,
                                           safety_hands_often, safety_distance_often),
         safety_all_followup = safety_mask_in_often_followup*safety_mask_out_often_followup*safety_hands_often_followup*safety_distance_often_followup,
         safety_total_followup =  sum_cols(safety_mask_in_often_followup, safety_mask_out_often_followup,
                                  safety_hands_often_followup, safety_distance_often_followup),
         # Binarized safety behavior ALWAYS
         safety_mask_in_always   = safety_mask_in %in% always,
         safety_mask_out_always  = safety_mask_out %in% always,
         safety_hands_always     = safety_hands %in% always,
         safety_distance_always  = safety_distance %in% always,
         safety_mask_in_always_followup   = safety_mask_in_followup %in% always,
         safety_mask_out_always_followup  = safety_mask_out_followup %in% always,
         safety_hands_always_followup     = safety_hands_followup %in% always,
         safety_distance_always_followup  = safety_distance_followup %in% always,
         safety_mask_in_always   = ifelse(is.na(safety_mask_in), NA, safety_mask_in_always),
         safety_mask_out_always  = ifelse(is.na(safety_mask_out), NA, safety_mask_out_always),
         safety_hands_always     = ifelse(is.na(safety_hands), NA, safety_hands_always),
         safety_distance_always  = ifelse(is.na(safety_distance), NA, safety_distance_always),
         safety_mask_in_always_followup   = ifelse(is.na(safety_mask_in_followup), NA, safety_mask_in_always_followup),
         safety_mask_out_always_followup  = ifelse(is.na(safety_mask_out_followup), NA, safety_mask_out_always_followup),
         safety_hands_always_followup     = ifelse(is.na(safety_hands_followup), NA, safety_hands_always_followup),
         safety_distance_always_followup  = ifelse(is.na(safety_distance_followup), NA, safety_distance_always_followup),
         safety_all_always = safety_mask_in_always*safety_mask_out_always*safety_hands_always*safety_distance_always,
         safety_total_always =  sum_cols(safety_mask_in_always, safety_mask_out_always,
                                                  safety_hands_always, safety_distance_always),
         safety_all_always_followup = safety_mask_in_always_followup*safety_mask_out_always_followup*safety_hands_always_followup*safety_distance_always_followup,
         safety_total_always_followup =  sum_cols(safety_mask_in_always_followup, safety_mask_out_always_followup,
                                  safety_hands_always_followup, safety_distance_always_followup),
         # Changes in safety behavior
         safety_mask_in_increase  = as.integer(safety_mask_in_followup)  > as.integer(safety_mask_in),
         safety_mask_out_increase = as.integer(safety_mask_out_followup) > as.integer(safety_mask_out),
         safety_hands_increase    = as.integer(safety_hands_followup)    > as.integer(safety_hands),
         safety_distance_increase = as.integer(safety_distance_followup) > as.integer(safety_distance),
         safety_mask_in_decrease  = as.integer(safety_mask_in_followup)  < as.integer(safety_mask_in),
         safety_mask_out_decrease = as.integer(safety_mask_out_followup) < as.integer(safety_mask_out),
         safety_hands_decrease    = as.integer(safety_hands_followup)    < as.integer(safety_hands),
         safety_distance_decrease = as.integer(safety_distance_followup) < as.integer(safety_distance),
         # Changes in mask ownership
         own_disposable_masks_increase = own_disposable_masks_followup > own_disposable_masks,
         own_reusable_masks_increase   = as.integer(own_reusable_masks_followup) > as.integer(own_reusable_masks),
         own_disposable_masks_decrease = own_disposable_masks_followup < own_disposable_masks,
         own_reusable_masks_decrease   = as.integer(own_reusable_masks_followup) < as.integer(own_reusable_masks),
         # Changes in persons met
         persons_met_increase = as.integer(persons_met_followup) > as.integer(persons_met),
         persons_met_decrease = as.integer(persons_met_followup) < as.integer(persons_met),
         # Policy views
         fed_balance_agree = as.integer(fed_balance == "They managed the balance just right"),
         state_balance_agree = as.integer(state_balance == "My state decided appropriately"),
         # Time dummies
         time_quartile = ntile(start_date, 4),
         # No-followup indicator
         no_follow = ifelse(is.na(progress_followup), 1, as.integer(progress_followup != 100))) %>% 
  # coerce all logical columns to integers
  mutate(across(where(is_logical), as.integer))


# Save --------------------------------------------------------------------
message("Saving data...")
write_rds(responses_rcd, "temp/launch_1.rds")


# Done --------------------------------------------------------------------
message("Done.")
