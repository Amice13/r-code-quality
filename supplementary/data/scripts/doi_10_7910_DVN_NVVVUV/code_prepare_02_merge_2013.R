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
library(dplyr)    # CRAN v1.0.5
library(tidyr)    # CRAN v1.1.3
library(car)      # CRAN v3.0-10
library(zoo)      # CRAN v1.8-8
library(haven)    # CRAN v2.3.1
library(foreign)  # CRAN v0.8-81
library(stringr)  # CRAN v1.4.0
library(quanteda) # CRAN v3.0.0

# set working directory
setwd("")


# load raw data
dat_13_survey <- read.dta("data_dontshare/ZA5709_v3-0-0.dta")

# code and select relevant variables here
dat_13_survey_select <- dat_13_survey %>% 
    select(match_id,
           age = a44,
           gender = a43,
           party_id = a54a,
           political_interest = a1,
           knowledge_q1_dummy = a18_v1,
           knowledge_q2_dummy = a21_v1,
           knowledge_q3_dummy = a22_v1,
           knowledge_q4_dummy = a24_v1) %>% 
    filter(age != "-95") # filter respondents who did not participate

table(dat_13_survey_select$political_interest)

# recode variables
recode_gender <- c("'maennlich'='Male';
                   'weiblich'='Female'")

recode_political_interest <- c("'ueberhaupt nicht'='0 no interest at all';
                               'weniger Stark'='1 not very strong';
                               'mittelmaessig'='2 medium interest';
                               'stark'='3 strong';
                               'sehr stark'='4 very strong'")

recode_party_id <- c("'keine Partei'='No party ID/No answer';
                     'PIRATEN'='Other party';
                     'andere Partei'='Other party';
                     'DIE LINKE'='Left Party';
                     'GRUENE'='Greens';
                     'keine Angabe'='No party ID/No answer'")

dat_13_survey_select <- dat_13_survey_select %>% 
    mutate(gender = car::recode(gender, recode_gender)) %>% 
    mutate(party_id = car::recode(party_id, recode_party_id)) %>% 
    mutate(political_interest = car::recode(political_interest, recode_political_interest)) 


dat_13_survey_select <- dat_13_survey_select %>% 
    mutate(knowledge_q1_dummy_num = ifelse(knowledge_q1_dummy == "richtig", 1, 0)) %>% 
    mutate(knowledge_q2_dummy_num = ifelse(knowledge_q2_dummy == "richtig", 1, 0)) %>% 
    mutate(knowledge_q3_dummy_num = ifelse(knowledge_q3_dummy == "richtig", 1, 0)) %>% 
    mutate(knowledge_q4_dummy_num = ifelse(knowledge_q4_dummy == "richtig", 1, 0)) %>% 
    mutate(political_knowledge = 
               (knowledge_q1_dummy_num +  knowledge_q2_dummy_num +
                    knowledge_q3_dummy_num + knowledge_q4_dummy_num)) 

table(dat_13_survey_select$political_knowledge)

# remove the individual knowledge variables
dat_13_survey_select <- dat_13_survey_select %>% 
    select(-starts_with("knowledge_"))

# load RTR data
dat_13_dial <- read.dta("data_dontshare/ZA5711_v1-0-0.dta")

# load content analysis
dat_13_content <- read.dta("data_dontshare/ZA5710_v2-2-0.dta") 

# load transcript of debates
dat_13_content_text <- read.csv("data_debate_transcripts/debate_2013_translated.csv")

dat_13_content_merged_all <- bind_cols(dat_13_content_text, dat_13_content)

dat_13_content_merged_all$v9 <- as.character(dat_13_content_merged_all$v9)


# exclude statements that last 0 seconds (v8 == 0)
# these are interjections and no proper statements

nrow(dat_13_content_merged_all)

dat_13_content_merged <- dat_13_content_merged_all %>% 
    filter(v8 > 0)

nrow(dat_13_content_merged)

dat_13_content_merged <- dat_13_content_merged %>% 
    ungroup() %>% 
    mutate(statement_number = paste("s", 1:n(), v10, sep = "_"))


                  # create variable that assesses whether a statement describes the 
# societal situation in a positive, negative, or neutral way

table(dat_13_content_merged$v28)

# > table(dat_13_content_merged$v28)
# 
# nicht-funktionale Aussage                      Nein 
# 87                       123 
# Ja        neutral/ambivalent 
# 164                       503 

# it is unclear what categories "Ja" (Yes) and "Nein" (No) relate to

# A manual inspection reveals (se lines below) that "Nein" means
# "positive" and "Ja" means "negative
dat_pos <- filter(dat_13_content_merged, v28 == "Nein")

head(dat_pos$text_translated, 3)
head(dat_pos$v9, 3)

dat_13_content_merged <- dat_13_content_merged %>% 
    mutate(situation_pos_neg = ifelse(v28 == "Nein", "Positive",
                                      ifelse(v28 == "Ja", "Negative",
                                             "Other/Neutral")))


# load sentiment dictionaries and perform sentiment analysis
data_dictionary_lexicoder <- readRDS("data_dictionary_lsd_de.rds")

data_dictionary_rauh <- readRDS("data_dictionary_rauh.rds")


corp_13 <- corpus(dat_13_content_merged, text_field = "v9")

dat_13_sentiment_lexicoder <- corp_13 %>% 
    tokens() %>% 
    tokens_lookup(dictionary = data_dictionary_lexicoder,
                  nested_scope = "dictionary") %>% 
    dfm() %>% 
    convert(to = "data.frame") %>% 
    mutate(sentiment_log_lexicoder = 
               log((pos + 0.5) / (neg + 0.5)))


dat_13_sentiment_rauh <- corp_13 %>% 
    tokens() %>% 
    tokens_lookup(dictionary = data_dictionary_rauh,
                  nested_scope = "dictionary") %>% 
    dfm() %>% 
    convert(to = "data.frame") %>% 
    mutate(sentiment_log_rauh = 
               log((positive + 0.5) / (negative + 0.5))) %>% 
    select(-doc_id)

# combine sentiment scores and content analysis
dat_13_content_merged <- bind_cols(dat_13_content_merged,
                                   dat_13_sentiment_lexicoder,
                                   dat_13_sentiment_rauh)


# group the dataset by start second and speaker and only keep the longer utterance
# (sometimes, the SAME speaker has TWO statements per second, but this
# would duplicate RTR values at a later stage


dat_13_content_merged <- dat_13_content_merged %>% 
    group_by(v10, v2, v3, v4) %>% # v2, v3, v4 = start, v10 = speaker
    mutate(count_second_speaker = n())

table(dat_13_content_merged$count_second_speaker)


# now select the LONGER statement (based on the number of words)
dat_13_content_merged$ntoken <- ntoken(corp_13, remove_punct = TRUE)

dat_13_content_unique <- dat_13_content_merged %>% 
    arrange(v2, v3, v4, v10, -ntoken) %>% 
    group_by(v2, v3, v4, v10) %>% 
    filter(row_number()==1) %>% 
    ungroup()

nrow(dat_13_content_unique) - nrow(dat_13_content_merged)

# transform dial data to long format (one observation per second and respondent)
dat_13_dial_long <- dat_13_dial %>% 
    gather(time_raw, value_rtr_dial, -c(study:nummer)) %>% 
    mutate(time_rtr = str_replace_all(time_raw, "t0", "")) %>% 
    mutate(value_rtr_dial = ifelse(value_rtr_dial == "neutral", 4, value_rtr_dial))

dat_13_dial_long$value_rtr_dial <- as.numeric(dat_13_dial_long$value_rtr_dial)

table(dat_13_dial_long$value_rtr_dial,useNA = "always")

# merge dial data with respondent information
dat_13_dial_long_merged <- left_join(dat_13_dial_long, dat_13_survey_select,
                                     by = "match_id")


stopifnot(nrow(dat_13_dial_long_merged) - nrow(dat_13_dial_long) == 0)

# https://stackoverflow.com/questions/44488300/change-column-variable-string-from-1-to-01/44492565#44492565
# convert number to correct format (e.g., change 1 to 01)
dat_13_content_unique$second <- sprintf("%02d", as.numeric(dat_13_content_unique$v4))
dat_13_content_unique$minute <- sprintf("%02d", as.numeric(dat_13_content_unique$v3))
dat_13_content_unique$hour <- dat_13_content_unique$v2

nrow(dat_13_content_unique)
nrow(dat_13_content_merged)

# select only relevant variables from content analysis
dat_13_content_clean <- dat_13_content_unique %>% 
    mutate(start_hms = paste0(hour, minute, second)) %>% 
    mutate(time_rtr = start_hms) %>% 
    select(start_hms, time_rtr, 
           hour, minute, second,
           statement_number,
           situation_pos_neg,
           starts_with("sentiment_log"),
           speaker = v10,
           topic = v18,
           text_german = v9,
           text_english = text_translated) # select content variables

table(dat_13_content_clean$speaker)

                  # create a dummy that distinguishes between politians and 
# moderators. 
# Arrange each second in ascending order (candidate AFTER moderator). This ensures 
# that the politician's statements is used for the subsequent seconds until a new statement 
# starts

dat_13_content_clean <- dat_13_content_clean %>% 
    mutate(candidate_dummy = ifelse(speaker %in% c("Merkel, Angela", "Steinbrueck, Peer"), 1, 0))

# arrange each hms: first moderator (=0), then candidate (=1)
# only happens rarely!


dat_13_content_clean <- dat_13_content_clean %>% 
    arrange(start_hms, candidate_dummy)

# let's check this for start_hms == 00548
# we expect: Kloeppel first, then Steinmeier

dat_content_check <- filter(dat_13_content_clean, start_hms == "00548")

dat_content_check$speaker
# correct: Kloeppel first, then Steinbrueck

# recode speakers (only use last names)
table(dat_13_content_clean$speaker)

recode_speakers <- c(
    "'Merkel, Angela'='Merkel';
    'Steinbrueck, Peer'='Steinbrueck';
    'Illner, Maybrit'='Illner';
    'Kloeppel, Peter'='Kloeppel';
    'Raab, Stephan'='Raab';
    'Will, Anne'='Will'"
)

dat_13_content_clean <- dat_13_content_clean %>% 
    mutate(speaker = car::recode(speaker, recode_speakers))


dat_13_content_clean <- dat_13_content_clean %>% 
    group_by(speaker) %>%  
    mutate(z_sentiment_log_lexicoder = (sentiment_log_lexicoder - mean(sentiment_log_lexicoder, na.rm = T)) / sd(sentiment_log_lexicoder, na.rm = T),
           z_sentiment_log_rauh = (sentiment_log_rauh - mean(sentiment_log_rauh, na.rm = T)) / sd(sentiment_log_rauh, na.rm = T)) %>% 
    ungroup()


tail(dat_13_content_clean$time_rtr)

table(dat_13_content_clean$speaker)

head(dat_13_dial_long_merged$time_rtr)
head(dat_13_content_clean$time_rtr)

# merge dial RTR data with clean content analysis
dat_13_dial_final <- left_join(dat_13_dial_long_merged,
                               dat_13_content_clean, 
                               by = "time_rtr")

dat_13_dial_final <- dat_13_dial_final %>% 
    arrange(match_id, time_raw) %>% 
    group_by(match_id, time_raw) %>% 
    mutate(group = "No treatment/control groups") %>% 
    mutate(n_times_included = 1:n()) %>% 
    rename(respondent_id = match_id)

table(dat_13_dial_final$speaker)

prop.table(table(dat_13_dial_final$n_times_included)) * 100

# some observations in there multiple times (seconds when a new statement started and the previous one ended)
# nothing that can be done about this and it is less than 1.3 percent of all seconds
nrow(dat_13_dial_final) - nrow(dat_13_dial_long_merged)

# "carry" over variables (such as text, sentiment etc)
dat_13_dial_final <- dat_13_dial_final %>% 
    ungroup() %>% 
    arrange(respondent_id, time_rtr) %>% 
    group_by(respondent_id) %>% 
    mutate(statement_number = zoo::na.locf(statement_number, na.rm = FALSE)) %>% 
    mutate(z_sentiment_log_lexicoder = zoo::na.locf(z_sentiment_log_lexicoder, na.rm = FALSE)) %>% 
    mutate(z_sentiment_log_rauh = zoo::na.locf(z_sentiment_log_rauh, na.rm = FALSE)) %>% 
    mutate(sentiment_log_rauh_carried = zoo::na.locf(sentiment_log_rauh, na.rm = FALSE)) %>% 
    mutate(sentiment_log_lexicoder_carried = zoo::na.locf(sentiment_log_lexicoder, na.rm = FALSE)) %>% 
    mutate(speaker_carried = zoo::na.locf(speaker, na.rm = FALSE)) %>% 
    mutate(topic_carried = zoo::na.locf(topic, na.rm = FALSE)) %>% 
    arrange(respondent_id, time_rtr) %>% 
    select(c(year, survey:n_times_included,
             group, respondent_id, statement_number,starts_with("z_sentiment"),
             contains("_carried"))) %>% 
    select(-c(sentiment_log_rauh, sentiment_log_lexicoder))

dat_13_dial_final <- dat_13_dial_final %>% 
    mutate(s = str_sub(time_rtr, -2, -1)) %>% 
    mutate(m = str_sub(time_rtr, -4, -3)) %>% 
    mutate(h = str_sub(time_rtr, -6, -5)) %>% 
    select(s, m, h,
           respondent_id, group, 
           age,
           gender, party_id, 
           political_knowledge,
           political_interest,
           time_rtr,
           value_rtr_dial, 
           text = text_german, text_english,
           statement_number, 
           situation_pos_neg,
           speaker_carried, 
           topic_carried,
           starts_with("z_sentiment"),
           starts_with("sentiment_log"), 
           n_times_included) %>% 
    ungroup() %>% 
    rename(speaker = speaker_carried,
           sentiment_log_rauh = sentiment_log_rauh_carried,
           sentiment_log_lexicoder = sentiment_log_lexicoder_carried,
           topic = topic_carried)




dat_13_statement_ids <- dat_13_dial_final %>% 
    select(speaker, time_rtr) %>% # keep only speaker and time_rtr
    arrange(time_rtr) %>% 
    unique() %>% # keep "unique" values -> each speaker, time_rtr second only once
    mutate(speaker_lag = lag(speaker), # lag speaker
           time_rtr_lag = lag(time_rtr)) %>% 
    mutate(change_speaker = ifelse(speaker != speaker_lag, 1, NA)) %>% # if change in speaker: 1, else NA
    mutate(row_number = 1:n()) %>% # get number of row as a unique identifier
    mutate(utterance_id_raw = ifelse(change_speaker == 1, paste(speaker, "s", row_number,
                                                                sep = "_"))) %>% # if change in speaker, create the speaker, row_number combination
    mutate(utterance_id = zoo::na.locf(utterance_id_raw, na.rm = FALSE)) %>% # fill in the NA values (= no change in speaker) with the "latest" statement ID
    select(utterance_id, time_rtr, speaker) %>% 
    group_by(utterance_id) %>% 
    mutate(seconds_utterance_id = n())



# merge segment ID information with dataset
dat_13_dial_final <- left_join(dat_13_dial_final,
                               dat_13_statement_ids, 
                               by = c("time_rtr", "speaker"))



# dat_check <- dat_13_dial_final %>% 
#     arrange(respondent_id, time_rtr) %>% 
#     filter(time_rtr >= "00548") %>% 
#     select(statement_number, time_rtr, speaker, sentiment_log_lexicoder,
#            text_english)

# save dataset
saveRDS(dat_13_dial_final, "data_tmp/data_2013.rds")
