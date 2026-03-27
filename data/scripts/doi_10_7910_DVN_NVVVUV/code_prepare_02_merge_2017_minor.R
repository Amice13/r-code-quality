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


# load survey data
dat_17_survey <- read.dta("data_dontshare/ZA6810_v1-0-0.dta")

# recode and select relevant variables from survey
dat_17_survey_select <- dat_17_survey %>% 
    select(match_id,
           age = a144,
           party_id = a126b,
           political_interest = a1,
           gender = a143,
           knowledge_q1_less = a35,
           knowledge_q2_notchange = a37a,
           knowledge_q3_more = a37b)

# recode variables
recode_gender <- c("'maennlich'='Male';
                   'weiblich'='Female'")

recode_political_interest <- c("'ueberhaupt nicht'='0 no interest at all/no answer';
                               'weniger stark'='1 not very strong';
                               'mittelmaessig'='2 medium interest';
                               'stark'='3 strong';
                               'sehr stark'='4 very strong';
                               'keine Angabe'='0 no interest at all/no answer'")

recode_party_id <- c("'keiner Partei'='No party ID/No answer';
                     'PIRATEN'='Other party';
                     'andere Partei'='Other party';
                     'DIE LINKE'='Left Party';
                     'GRUENE'='Greens';
                     'oedp'='Other party';
                     'keine Angabe'='No party ID/No answer'")

dat_17_survey_select <- dat_17_survey_select %>% 
    mutate(gender = car::recode(gender, recode_gender)) %>% 
    mutate(party_id = car::recode(party_id, recode_party_id)) %>% 
    mutate(political_interest = car::recode(political_interest, recode_political_interest)) 


table(dat_17_survey_select$knowledge_q1_less)
table(dat_17_survey_select$knowledge_q3_more)

dat_17_survey_select <- dat_17_survey_select %>% 
    mutate(knowledge_q1_dummy_num = ifelse(knowledge_q1_less == "weniger", 1, 0)) %>% 
    mutate(knowledge_q2_dummy_num = ifelse(knowledge_q2_notchange == "nicht aendern", 1, 0)) %>% 
    mutate(knowledge_q3_dummy_num = ifelse(knowledge_q3_more == "erhoehen", 1, 0)) %>% 
    mutate(political_knowledge = 
               (knowledge_q1_dummy_num +  knowledge_q2_dummy_num +
                    knowledge_q3_dummy_num)) 


dat_17_survey_select <- dat_17_survey_select %>% 
    select(-starts_with("knowledge_"))

# load RTR dial data
dat_17_dial <- read.dta("data_dontshare/ZA6830_v1-0-0.dta")

# load content analysis and transcripts
dat_17_content <- read.dta("data_dontshare/ZA6829_v1-0-0.dta") 
dat_17_content_text <- read.csv("data_debate_transcripts/debate_2017_five_parties_translated.csv")

dat_17_content_merged_all <- bind_cols(dat_17_content_text, dat_17_content)



# exclude statements that last 0 seconds (v11 == "00:00")
# these are interjections and no proper statements

nrow(dat_17_content_merged_all)

dat_17_content_merged <- dat_17_content_merged_all %>% 
    filter(v11 != "00:00")

nrow(dat_17_content_merged)

dat_17_content_merged <- dat_17_content_merged %>% 
    mutate(statement_number = paste("s", 1:n(), v13, sep = "_"))

dat_17_content_merged$v12 <- as.character(dat_17_content_merged$v12)


                  # create variable that assesses whether a statement describes the 
# societal situation in a positive, negative, or neutral way

table(dat_17_content_merged$v40)

# > table(dat_17_content_merged$v40)
# 
# nicht-funktionale Aussage/trifft nicht zu 
# 494 
# positiv 
# 19 
# negativ 
# 118 
# neutral/ambivalent 
# 51 

dat_17_content_merged <- dat_17_content_merged %>% 
    mutate(situation_pos_neg = ifelse(v40 == "positiv", "Positive",
                                      ifelse(v40 == "negativ", "Negative",
                                             "Other/Neutral")))


table(dat_17_content_merged$situation_pos_neg)


# perform sentiment analysis and load Lexicoder and Rauh's sentiment dictionary

data_dictionary_lexicdoer <- readRDS("data_dictionary_lsd_de.rds")

data_dictionary_rauh <- readRDS("data_dictionary_rauh.rds")

summary(data_dictionary_rauh)

corp_17 <- corpus(dat_17_content_merged, text_field = "v12")

dat_17_sentiment_lexicoder <- corp_17 %>% 
    tokens() %>% 
    tokens_lookup(dictionary = data_dictionary_lexicdoer,
                  nested_scope = "dictionary") %>% 
    dfm() %>% 
    quanteda::convert(to = "data.frame") %>% 
    mutate(sentiment_log_lexicoder = 
               log((pos + 0.5) / (neg + 0.5)))


dat_17_sentiment_rauh <- corp_17 %>% 
    tokens() %>% 
    tokens_lookup(dictionary = data_dictionary_rauh,
                  nested_scope = "dictionary") %>% 
    dfm() %>% 
    quanteda::convert(to = "data.frame") %>% 
    mutate(sentiment_log_rauh = 
               log((positive + 0.5) / (negative + 0.5))) %>% 
    select(-doc_id)


# combine sentiment scores and content analysis
dat_17_content_merged <- bind_cols(dat_17_content_merged,
                                   dat_17_sentiment_rauh,
                                   dat_17_sentiment_lexicoder)


# group the dataset by start second and speaker and only keep the longer utterance
# (sometimes, the SAME speaker has TWO statements per second, but this
# would duplicate RTR values at a later stage

dat_17_content_merged$start_second <- as.character(dat_17_content_merged$v9)

dat_17_content_merged <- dat_17_content_merged %>% 
    group_by(v13, start_second) %>% # v9 = start of statement, v13 = speaker
    mutate(count_second_speaker = n())


table(dat_17_content_merged$count_second_speaker)

# now select the SHORTER statement (based on the number of words)

dat_17_content_merged$ntoken <- ntoken(corp_17, remove_punct = TRUE)

dat_17_content_unique <- dat_17_content_merged %>% 
    arrange(start_second, v13, -ntoken) %>% 
    group_by(start_second, v13) %>% 
    filter(row_number()==1) %>% 
    ungroup()

# transform dial data to long format (one observation per second and respondent)
dat_17_dial_long <- dat_17_dial %>% 
    gather(time_raw, value_rtr_dial, -c(study:rtr_instruktion)) %>% 
    mutate(time_rtr = str_replace_all(time_raw, "t0", ""))

# merge dial data with respondent information
dat_17_dial_long_merged <- left_join(dat_17_dial_long, dat_17_survey_select,
                                     by = "match_id")

stopifnot(nrow(dat_17_dial_long_merged) - nrow(dat_17_dial_long) == 0)

# https://stackoverflow.com/questions/44488300/change-column-variable-string-from-1-to-01/44492565#44492565
# convert number to correct format (e.g., change 1 to 01)
dat_17_content_unique$second <- sprintf("%02d", as.numeric(dat_17_content_unique$v5))
dat_17_content_unique$minute <- sprintf("%02d", as.numeric(dat_17_content_unique$v4))
dat_17_content_unique$hour <- dat_17_content_unique$v3

dat_17_content_clean <- dat_17_content_unique %>% 
    mutate(start_hms = paste0(hour, minute, second)) %>% 
    mutate(time_rtr = start_hms) %>% 
    select(start_hms, time_rtr, 
           hour, minute, second,
           statement_number,
           sentiment_log_rauh,
           sentiment_log_lexicoder,
           situation_pos_neg,
           v3, v4, v5,
           speaker = v13,
           topic = v22,
           text_german = v12,
           text_english = text_translated) # select content variables




table(dat_17_content_clean$speaker)

recode_speakers <- c(
    "'Nitsche, Christian'='Nitsche';
    'Mickich, Sonia Seymour'='Mickich';
    'Weidel, Alice'='Weidel';
    'Oezdemir, Cem'='Ozdemir';
    'Wagenknecht, Sahra'='Wagenknecht';
    'Lindner, Christian'='Lindner';
    'Herrmann, Joachim'='Herrmann'"
)

dat_17_content_clean <- dat_17_content_clean %>% 
    mutate(speaker = car::recode(speaker, recode_speakers))



                  # create a dummy that distinguishes between politians and 
# moderators. 
# Arrange each second in ascending order (candidate AFTER moderator). This ensures 
# that the politician's statements is used for the subsequent seconds until a new statement 
# starts

dat_17_content_clean <- dat_17_content_clean %>% 
    mutate(candidate_dummy = ifelse(speaker %in% c("Ozdemir", "Lindner", 
                                                   "Herrmann", "Weidel", "Wagenknecht"),
                                    1, 0))

table(dat_17_content_clean$speaker,
      dat_17_content_clean$candidate_dummy)

# arrange each hms: first moderator (=0), then candidate (=1)
# only happens rarely!

dat_17_content_clean <- dat_17_content_clean %>% 
    arrange(start_hms, candidate_dummy)

# let's check this for start_hms == 04327

# we expect: moderator first, then Merkel

dat_content_check <- filter(dat_17_content_clean, start_hms == "04442")

dat_content_check$speaker
# correct


dat_17_content_clean <- dat_17_content_clean %>% 
    group_by(speaker) %>%  
    mutate(z_sentiment_log_lexicoder = (sentiment_log_lexicoder - mean(sentiment_log_lexicoder, na.rm = T)) / sd(sentiment_log_lexicoder, na.rm = T),
           z_sentiment_log_rauh = (sentiment_log_rauh - mean(sentiment_log_rauh, na.rm = T)) / sd(sentiment_log_rauh, na.rm = T)) %>% 
    ungroup()

table(dat_17_content_clean$speaker)

head(dat_17_dial_long_merged$time_rtr)
head(dat_17_content_clean$time_rtr)

# merge long RTR data and content analsyis
dat_17_dial_final <- left_join(dat_17_dial_long_merged,
                               dat_17_content_clean, 
                               by = "time_rtr")


dat_17_dial_final <- dat_17_dial_final %>% 
    arrange(match_id, time_rtr) %>% 
    group_by(match_id, time_rtr) %>% 
    mutate(n_times_included = 1:n())

table(dat_17_dial_final$speaker)

prop.table(table(dat_17_dial_final$n_times_included)) * 100

# some observations in there multiple times (seconds when a new statement started and the previous one ended)
nrow(dat_17_dial_final) - nrow(dat_17_dial_long_merged)

# "carry" over variables with this code (such as text, sentiment etc)
dat_17_dial_final <- dat_17_dial_final %>% 
    ungroup() %>% 
    rename(respondent_id = match_id) %>% 
    arrange(respondent_id, time_rtr) %>% 
    group_by(respondent_id) %>% 
    mutate(statement_number = zoo::na.locf(statement_number, na.rm = FALSE)) %>% 
    mutate(z_sentiment_log_lexicoder = zoo::na.locf(z_sentiment_log_lexicoder, na.rm = FALSE)) %>% 
    mutate(z_sentiment_log_rauh = zoo::na.locf(z_sentiment_log_rauh, na.rm = FALSE)) %>% 
    mutate(sentiment_log_lexicoder_carried = zoo::na.locf(sentiment_log_lexicoder, na.rm = FALSE)) %>% 
    mutate(sentiment_log_rauh_carried = zoo::na.locf(sentiment_log_rauh, na.rm = FALSE)) %>% 
    mutate(topic_carried = zoo::na.locf(topic, na.rm = FALSE)) %>% 
    mutate(speaker_carried = zoo::na.locf(speaker, na.rm = FALSE)) %>% 
    arrange(respondent_id, time_rtr) %>% 
    mutate(group = "No treatment/control groups") 

length(unique(dat_17_dial_final$respondent_id)) # 36

dat_17_dial_final_recoded <- dat_17_dial_final %>% 
    mutate(s = str_sub(time_rtr, -2, -1)) %>% 
    mutate(m = str_sub(time_rtr, -4, -3)) %>% 
    mutate(h = str_sub(time_rtr, -6, -5)) %>% 
    select(s, m, h,
           respondent_id, 
           statement_number,
           situation_pos_neg,
           group, age, 
           gender, party_id, 
           political_knowledge,
           political_interest,
           rtr_system, time_rtr,
           value_rtr_dial, speaker, topic,
           text = text_german, text_english, 
           n_times_included,
           starts_with("z_sentiment"),
           contains("_carried")) %>% 
    ungroup() %>% 
    rename(speaker_raw = speaker,
           speaker = speaker_carried,
           sentiment_log_rauh = sentiment_log_rauh_carried,
           sentiment_log_lexicoder = sentiment_log_lexicoder_carried,
           topic_raw = topic,
           topic = topic_carried)

                  # create segment IDs
dat_17_minor_statement_ids <- dat_17_dial_final_recoded %>% 
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

# merge segment IDs with final data
dat_17_dial_final_recoded <- left_join(dat_17_dial_final_recoded,
                                       dat_17_minor_statement_ids, 
                                       by = c("time_rtr", "speaker"))

saveRDS(dat_17_dial_final_recoded, "data_tmp/data_2017_minor.rds")


