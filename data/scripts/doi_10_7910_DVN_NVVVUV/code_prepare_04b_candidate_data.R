############################################################################
############################################################################
#### 
#### Constantine Boussalis, Travis G. Coan, Mirya R. Holman, Stefan Müller
#### Gender, Candidate Emotional Expression,
#### and Voter Reactions During Televised Debates 
####
#### American Political Science Review 
####
#### Note: this script is only required for data preparation 
### and does not need to be executed to reproduce any analyses reported
### in the paper or online appendix
############################################################################
############################################################################

# load packages
library(dplyr) # CRAN v1.0.5
library(vroom) # CRAN v1.4.0
library(readr) # CRAN v1.4.0

# set working directory 

setwd("")

# prepare voter-level file

# select one respondent (all respondents have the same number of observations)
# and keep the relevant candidate-level variables
dat_05_speaker_cv_raw <- read_csv("data_for_analysis_2005.csv") %>% 
    filter(respondent_id == "ID100") %>% 
    select(debate_name = year, hms_merge,
           cv_lastname, speaker, strength, frequency,
           starts_with("sentiment_log"),
           z_sentiment_log_lexicoder,
           z_sentiment_log_rauh, 
           z_frequency, z_frequency_l1) 

nrow(dat_05_speaker_cv_raw)

summary(dat_05_speaker_cv_raw$sentiment_log_lexicoder)


# make sure to select only unique values
dat_05_speaker_cv <- unique(dat_05_speaker_cv_raw)

nrow(dat_05_speaker_cv)

dat_09_speaker_cv_raw <- vroom("data_for_analysis_2009.csv") %>% 
    filter(respondent_id == "22110010") %>% 
    select(debate_name = year, hms_merge,
           cv_lastname, speaker, strength, frequency,
           starts_with("sentiment_log"),
           z_sentiment_log_lexicoder,
           z_sentiment_log_rauh, 
           z_frequency, z_frequency_l1) 


# select only unique values (one observation per candidate-second)
dat_09_speaker_cv <- unique(dat_09_speaker_cv_raw)


dat_13_speaker_cv_raw <- vroom("data_for_analysis_2013.csv") %>% 
    filter(respondent_id == "LD-A-01") %>% 
    select(debate_name = year, hms_merge, 
           cv_lastname, speaker, strength, frequency,
           starts_with("sentiment_log"),
           z_sentiment_log_lexicoder,
           z_sentiment_log_rauh,
           z_frequency, z_frequency_l1) 

# select only unique values (one observation per candidate-second)
dat_13_speaker_cv <- unique(dat_13_speaker_cv_raw)

nrow(dat_13_speaker_cv_raw)

table(dat_13_speaker_cv$debate_name, useNA = "always")

dat_17_speaker_cv_raw <- vroom("data_for_analysis_2017.csv") %>% 
    filter(respondent_id == "L1062X045") %>% 
    select(debate_name = year, hms_merge, 
           cv_lastname, speaker, strength, frequency,
           starts_with("sentiment_log"),
           z_sentiment_log_lexicoder,
           z_sentiment_log_rauh,
           z_frequency, z_frequency_l1) 


# select only unique values (one observation per candidate-second)
dat_17_speaker_cv <- unique(dat_17_speaker_cv_raw)

nrow(dat_17_speaker_cv_raw)


dat_17_minor_speaker_cv_raw <- vroom("data_for_analysis_2017_minor.csv") %>% 
    filter(respondent_id == "M1104D007") %>% 
    select(debate_name = year, hms_merge, 
           cv_lastname, speaker, strength, frequency,
           starts_with("sentiment_log"),
           z_sentiment_log_lexicoder,
           z_sentiment_log_rauh,
           z_frequency, z_frequency_l1) 


# select only unique values (one observation per candidate-second)
dat_17_minor_speaker_cv <- unique(dat_17_minor_speaker_cv_raw)

nrow(dat_17_minor_speaker_cv_raw)


# bind all of these files into one long dataframe
# requires transforming some variables to characters

dat_05_speaker_cv$hms_merge <- as.character(dat_05_speaker_cv$hms_merge)
dat_09_speaker_cv$hms_merge <- as.character(dat_09_speaker_cv$hms_merge)
dat_13_speaker_cv$hms_merge <- as.character(dat_13_speaker_cv$hms_merge)
dat_17_speaker_cv$hms_merge <- as.character(dat_17_speaker_cv$hms_merge)
dat_17_minor_speaker_cv$hms_merge <- as.character(dat_17_minor_speaker_cv$hms_merge)

dat_05_speaker_cv$debate_name <- as.character(dat_05_speaker_cv$debate_name)
dat_09_speaker_cv$debate_name <- as.character(dat_09_speaker_cv$debate_name)
dat_13_speaker_cv$debate_name <- as.character(dat_13_speaker_cv$debate_name)
dat_17_speaker_cv$debate_name <- as.character(dat_17_speaker_cv$debate_name)
dat_17_minor_speaker_cv$debate_name <- as.character(dat_17_minor_speaker_cv$debate_name)


dat_speaker_cv <- bind_rows(dat_05_speaker_cv,
                            dat_09_speaker_cv,
                            dat_13_speaker_cv,
                            dat_17_speaker_cv,
                            dat_17_minor_speaker_cv) %>% 
    mutate(pitch_sentiment_models = 1)


summary(dat_speaker_cv$sentiment_log_lexicoder)

table(dat_speaker_cv$debate_name,useNA = "always")

# sort ascending by debate and time
dat_speaker_cv <- dat_speaker_cv %>% 
    arrange(debate_name, hms_merge)


nrow(dat_speaker_cv)

# now load the tmp datasets that 
# are on the candidate-emotion-frame level
# keep the emotion frame data


dat_05_frames <- readRDS("data_tmp/data_full_2005.rds") %>% 
    ungroup() %>% 
    select(hms_merge, hms_frames, speaker, cv_lastname, debate_name = year, text_english,
           gender_policy_area_broad, utterance_id,
           statement_number,
           topic_broad_policy_area,
           z_emo_anger, z_emo_happiness, z_emo_neutral,
           starts_with("emo"), code_smile, value_rtr) %>% 
    mutate(z_emo_nonneutral = 1 - z_emo_neutral) %>% 
    group_by(hms_merge) %>% 
    mutate(mean_rtr = mean(value_rtr, na.rm = TRUE)) %>% 
    ungroup() %>% 
    select(-value_rtr) %>% 
    unique() 


nrow(dat_05_frames)

table(dat_05_frames$debate_name)

dat_09_frames <- readRDS("data_tmp/data_full_2009.rds") %>% 
    ungroup() %>% 
    select(hms_merge, speaker, cv_lastname, debate_name = year, text_english,
           situation_pos_neg, statement_number,utterance_id,
           gender_policy_area_broad,
           topic_broad_policy_area,
           z_emo_anger, z_emo_happiness, z_emo_neutral, value_rtr,
           starts_with("emo")) %>% 
    mutate(z_emo_nonneutral = 1 - z_emo_neutral) %>% 
    group_by(hms_merge) %>% 
    mutate(mean_rtr = mean(value_rtr, na.rm = TRUE)) %>% 
    ungroup() %>% 
    select(-value_rtr) %>% 
    unique() 

table(dat_09_frames$debate_name, useNA = "always")


dat_13_frames <- readRDS("data_tmp/data_full_2013.rds") %>% 
    ungroup() %>% 
    select(hms_merge, speaker, cv_lastname, debate_name = year, text_english,
           situation_pos_neg, statement_number,utterance_id,
           gender_policy_area_broad,
           topic_broad_policy_area,
           z_emo_anger, z_emo_happiness, z_emo_neutral,
           starts_with("emo"), value_rtr) %>% 
    mutate(z_emo_nonneutral = 1 - z_emo_neutral) %>% 
    group_by(hms_merge) %>% 
    mutate(mean_rtr = mean(value_rtr, na.rm = TRUE)) %>% 
    ungroup() %>% 
    select(-value_rtr) %>% 
    unique() 


dat_17_frames <- readRDS("data_tmp/data_full_2017.rds") %>% 
    ungroup() %>% 
    select(hms_merge, speaker, cv_lastname, debate_name = year, text_english,
           situation_pos_neg, statement_number, utterance_id,
           gender_policy_area_broad, 
           topic_broad_policy_area,
           z_emo_anger, z_emo_happiness, z_emo_neutral,
           starts_with("emo"), value_rtr) %>% 
    mutate(z_emo_nonneutral = 1 - z_emo_neutral) %>% 
    group_by(hms_merge) %>% 
    mutate(mean_rtr = mean(value_rtr, na.rm = TRUE)) %>% 
    ungroup() %>% 
    select(-value_rtr) %>% 
    unique() 


dat_17_minor_frames <- readRDS("data_tmp/data_full_2017_minor.rds") %>% 
    ungroup() %>% 
    select(hms_merge, speaker, cv_lastname, debate_name = year, text_english,
           situation_pos_neg, statement_number, utterance_id,
           gender_policy_area_broad,
           topic_broad_policy_area,
           z_emo_anger, z_emo_happiness, z_emo_neutral,
           starts_with("emo"), value_rtr) %>% 
    mutate(z_emo_nonneutral = 1 - z_emo_neutral) %>% 
    group_by(hms_merge) %>% 
    mutate(mean_rtr = mean(value_rtr, na.rm = TRUE)) %>% 
    ungroup() %>% 
    select(-value_rtr) %>% 
    unique() 


# bind second-level data
dat_frames <- bind_rows(dat_05_frames,
                        dat_09_frames,
                        dat_13_frames,
                        dat_17_frames,
                        dat_17_minor_frames)



table(dat_frames$cv_lastname)


# merge data frames
dat_merged <- left_join(dat_frames, dat_speaker_cv,
                        by = c("hms_merge", "speaker", "cv_lastname",
                               "debate_name"))






dat_merged <- dat_merged %>% 
    mutate(hms_plot = ifelse(debate_name == "2005", hms_frames, hms_merge))


dat_merged <- dat_merged %>% 
    mutate(pitch_sentiment_models = ifelse(is.na(pitch_sentiment_models), 0, 1))

dat_merged <- dat_merged %>% 
    mutate(speaker_equals_cv_lastname = ifelse(speaker == cv_lastname, 1, 0))

table(dat_merged$pitch_sentiment_models,
      dat_merged$speaker_equals_cv_lastname)

nrow(dat_merged)

summary(dat_merged$mean_rtr)

               # create lagged variables on speaker-utterance level

dat_merged <- dat_merged %>% 
    arrange() %>% 
    arrange(debate_name, hms_frames) %>% 
    group_by(debate_name, utterance_id, cv_lastname) %>% 
    mutate(l_emo_anger = lag(emo_anger),
           l_emo_happiness = lag(emo_happiness),
           l_emo_neutral = lag(emo_neutral)) %>% 
    select(debate_name, contains("hms"),
           statement_number, utterance_id,
           speaker, cv_lastname,
           emo_anger, emo_happiness, l_emo_happiness,
           emo_neutral,
           l_emo_neutral, l_emo_anger,
           everything())

# save file
write_csv(dat_merged, "data_candidate_models.csv")
