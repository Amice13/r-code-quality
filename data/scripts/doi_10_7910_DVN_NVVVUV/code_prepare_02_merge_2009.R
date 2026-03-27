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
library(zoo)      # CRAN v1.8-8
library(car)      # CRAN v3.0-10
library(haven)    # CRAN v2.3.1
library(foreign)  # CRAN v0.8-81
library(stringr)  # CRAN v1.4.0
library(quanteda) # CRAN v3.0.0

# set working directory

setwd("")


# load raw data
dat_09_survey <- read.dta("data_dontshare/ZA5309_v2-0-0.dta")

## code and select relevant variables here

dat_09_survey_select <- dat_09_survey %>% 
    select(match_id = id,
           location  = Standort,
           age = A45,
           gender = A44, 
           party_id = A56_01,
           political_interest = A01,
           knowledge_q1_less = A18,
           knowledge_q2_same = A19,
           knowledge_q3_less = A22
    )

# recode variables

recode_gender <- c("'maennlich'='Male';
                   'weiblich'='Female';
                   'keine Angabe'='No answer'")

recode_political_interest <- c("'ueberhaupt nicht'='0 no interest at all/no answer';
                               'keine Angabe'='0 no interest at all/no answer';
                               'weniger stark'='1 not very strong';
                               'mittelmaessig'='2 medium interest';
                               'stark'='3 strong';
                               'sehr stark'='4 very strong'")

recode_party_id <- c("'keiner Partei'='No party ID/No answer';
                     'PIRATEN'='Other party';
                     'andere Partei'='Other party';
                     'DIE LINKE'='Left Party';
                     'gruene'='Greens';
                     'spd'='SPD';
                     'fdp'='FDP';
                     'keine Angabe'='No party ID/No answer'")

table(dat_09_survey_select$political_interest)

dat_09_survey_select <- dat_09_survey_select %>% 
    mutate(gender = car::recode(gender, recode_gender)) %>% 
    mutate(party_id = car::recode(party_id, recode_party_id)) %>% 
    mutate(political_interest = car::recode(political_interest, recode_political_interest)) 

table(dat_09_survey_select$gender)
table(dat_09_survey_select$party_id)
table(dat_09_survey_select$political_interest)


table(dat_09_survey_select$knowledge_q3_less)

dat_09_survey_select <- dat_09_survey_select %>% 
    mutate(knowledge_q1_dummy_num = ifelse(str_detect(knowledge_q1_less, "RICHTIGE ANTWORT"), 1, 0)) %>% 
    mutate(knowledge_q2_dummy_num = ifelse(str_detect(knowledge_q2_same, "RICHTIGE ANTWORT"), 1, 0)) %>% 
    mutate(knowledge_q3_dummy_num = ifelse(str_detect(knowledge_q3_less, "RICHTIGE ANTWORT"), 1, 0)) %>% 
    mutate(political_knowledge = 
               (knowledge_q1_dummy_num +  knowledge_q2_dummy_num +
                    knowledge_q3_dummy_num)) 


dat_09_survey_select <- dat_09_survey_select %>% 
    mutate(group = ifelse(location %in% c("Hohenheim",
                                          "Manneheim",
                                          "Jena"), 
                          "Video and audio",
                          ifelse(location %in% c("Landau",
                                                 "Kaiserslautern"),
                                 "Audio only", NA))) %>% 
    filter(group == "Video and audio")


table(dat_09_survey_select$location)

# In Stuttgart-Hohenheim, Jena und Landau wurde die RTR-Messung mit
# Hilfe der Drehreglertechnik durchgeführt.

# In Mannheim und Kaiserslautern wurde die RTR-Messung 
# mit der Push-Button-Technik durchge- führt.

# get only Dial respondents
dat_09_survey_select <- dat_09_survey_select %>% 
    mutate(type_rtr = ifelse(location %in% c("Hohenheim",
                                             "Landau",
                                             "Jena"), 
                             "Dial",
                             ifelse(location %in% c("Mannheim",
                                                    "Kaiserslautern"),
                                    "Push", NA))) %>% 
    filter(type_rtr == "Dial")


table(dat_09_survey_select$location,
      dat_09_survey_select$type_rtr)

# load RTR data
dat_09 <- read.dta("data_dontshare/ZA5310_v1-1-0.dta")

# load content analysis
dat_09_content <- read.dta("data_dontshare/ZA5311_v1-0-0.dta") %>% 
    select(-c(TEXT, Fortlaufende)) # remove text because it is incomplete in this file


# load file with text and English translation
dat_09_content_text <- read.csv("data_debate_transcripts/debate_2009_translated.csv")

# combine content analysis and English translation of debate
dat_09_content_merged <- bind_cols(dat_09_content_text, dat_09_content)

summary(dat_09_content_merged$DAUER)

# all statements last at least one second (in contrast to the 2017 and 2013 debates)

dat_09_content_merged <- dat_09_content_merged %>% 
    ungroup() %>% 
    mutate(statement_number = paste("s", 1:n(), SPRECHER, sep = "_"))


# perform a sentiment analysis

# load dictionaries (Lexicoder and Rauh's sentiment dictionary)

data_dictionary_lexicoder <- readRDS("data_dictionary_lsd_de.rds")

data_dictionary_rauh <- readRDS("data_dictionary_rauh.rds")

# transform text to character coding
dat_09_content_merged$TEXT <- as.character(dat_09_content_merged$TEXT)

                  # create a quanteda text corpus
corp_09 <- corpus(dat_09_content_merged, text_field = "TEXT")


dat_09_sentiment_lexicoder <- corp_09 %>% 
    tokens() %>% 
    tokens_lookup(dictionary = data_dictionary_lexicoder,
                  nested_scope = "dictionary") %>% 
    dfm() %>% 
    quanteda::convert(to = "data.frame") %>% 
    mutate(sentiment_log_lexicoder = 
               log((pos + 0.5) / (neg + 0.5)))


dat_09_sentiment_rauh <- corp_09 %>% 
    tokens() %>% 
    tokens_lookup(dictionary = data_dictionary_rauh,
                  nested_scope = "dictionary") %>% 
    dfm() %>% 
    quanteda::convert(to = "data.frame") %>% 
    mutate(sentiment_log_rauh = 
               log((positive + 0.5) / (negative + 0.5))) %>% 
    select(-doc_id)

# merge sentiment data and content analysis
dat_09_content_merged <- bind_cols(dat_09_content_merged,
                                   dat_09_sentiment_rauh,
                                   dat_09_sentiment_lexicoder)


                  # create variable that assesses whether a statement describes the 
# societal situation in a positive, negative, or neutral way
table(dat_09_content_merged$LAGE)

# > table(dat_09_content_merged$LAGE)
# 
# neutral/nicht vorhanden       negative Anmutung 
# 288                      38 
# positive Anmutung         trifft nicht zu 
# 24                     135 

dat_09_content_merged <- dat_09_content_merged %>% 
    mutate(situation_pos_neg = ifelse(LAGE == "positive Anmutung", "Positive",
                                      ifelse(LAGE == "negative Anmutung", "Negative",
                                             "Other/Neutral")))


# group the dataset by start second and speaker and only keep the longer utterance
# (sometimes, the SAME speaker has TWO statements per second, but this
# would duplicate RTR values at a later stage

# now select the LONGER statement (based on the number of words)

dat_09_content_merged$ntoken <- ntoken(corp_09, remove_punct = TRUE)

dat_09_content_unique <- dat_09_content_merged %>% 
    arrange(STARTH, STARTMIN, STARTSEC, SPRECHER, -ntoken) %>% 
    group_by(STARTH, STARTMIN, STARTSEC, SPRECHER) %>% 
    filter(row_number()==1) %>% 
    ungroup()

nrow(dat_09_content_unique) - nrow(dat_09_content_merged)


# transform push and dial data to long format (one observation per second and respondent)

dat_09_long <- dat_09 %>% 
    gather(time_raw, value_rtr, -c(study:ID)) %>% 
    mutate(time_rtr = str_replace_all(time_raw, "t0", "")) %>% 
    rename(match_id = ID) %>% 
    filter(!is.na(match_id))

table(dat_09_long$match_id)

# remove invalid IDs
dat_09_longprob <- filter(dat_09_long, is.na(value_rtr))


table(dat_09_long$value_rtr,useNA = "always")


# merge dial data with respondent information

dat_09_long_merged <- left_join(dat_09_long, dat_09_survey_select,
                                by = "match_id")

table(dat_09_long_merged$age)

stopifnot(nrow(dat_09_long_merged) - nrow(dat_09_long) == 0)

# convert number to correct format (e.g., change 1 to 01)
dat_09_content_unique$second <- sprintf("%02d", as.numeric(dat_09_content_unique$STARTSEC))
dat_09_content_unique$minute <- sprintf("%02d", as.numeric(dat_09_content_unique$STARTMIN))
dat_09_content_unique$hour <- dat_09_content_unique$STARTH

# select relevant variables from content analysis
dat_09_content_clean <- dat_09_content_unique %>% 
    mutate(start_hms = paste0(hour, minute, second)) %>% 
    mutate(SPRECHER = as.character(SPRECHER)) %>% # recode Klöppel
    mutate(SPRECHER = ifelse(str_detect(SPRECHER, "ppel"), "Klöppel", SPRECHER)) %>% 
    mutate(time_rtr = start_hms) %>% 
    select(start_hms, time_rtr, 
           hour, minute, second,
           statement_number,
           sentiment_log_lexicoder, 
           sentiment_log_rauh,
           situation_pos_neg,
           statement_number,
           speaker = SPRECHER,
           topic = THEMA,
           text_german = TEXT,
           text_english = text_translated) # select content variables


# standardize sentiment

dat_09_content_clean <- dat_09_content_clean %>% 
    group_by(speaker) %>%  
    mutate(z_sentiment_log_lexicoder = (sentiment_log_lexicoder - mean(sentiment_log_lexicoder, na.rm = T)) / sd(sentiment_log_lexicoder, na.rm = T),
           z_sentiment_log_rauh = (sentiment_log_rauh - mean(sentiment_log_rauh, na.rm = T)) / sd(sentiment_log_rauh, na.rm = T)) %>% 
    ungroup()


cor.test(dat_09_content_clean$z_sentiment_log_lexicoder,
         dat_09_content_clean$z_sentiment_log_rauh)

# merge long data with content analysis
dat_09_final <- left_join(dat_09_long_merged,
                          dat_09_content_clean, 
                          by = "time_rtr")


dat_09_final <- dat_09_final %>% 
    arrange(match_id, time_rtr) %>% 
    group_by(match_id, time_rtr) %>% 
    mutate(n_times_included = n())

table(dat_09_final$speaker)

# "carry" over variables with this code (such as text, sentiment etc)
dat_09_final_carried <- dat_09_final %>% 
    ungroup() %>% 
    arrange(match_id, time_rtr) %>% 
    group_by(match_id) %>% 
    mutate(speaker = car::recode(speaker, "'Klöppel'='Kloeppel'")) %>% 
    mutate(speaker = str_squish(speaker)) %>% 
    mutate(statement_number = zoo::na.locf(statement_number, na.rm = FALSE)) %>% 
    mutate(sentiment_log_rauh_carried = zoo::na.locf(sentiment_log_rauh, na.rm = FALSE)) %>% 
    mutate(sentiment_log_lexicoder_carried = zoo::na.locf(sentiment_log_lexicoder, na.rm = FALSE)) %>% 
    mutate(z_sentiment_log_rauh = zoo::na.locf(z_sentiment_log_rauh, na.rm = FALSE)) %>% 
    mutate(z_sentiment_log_lexicoder = zoo::na.locf(z_sentiment_log_lexicoder, na.rm = FALSE)) %>%  
    mutate(speaker_carried = zoo::na.locf(speaker, na.rm = FALSE)) %>% 
    mutate(topic_carried = zoo::na.locf(topic, na.rm = FALSE)) %>% 
    arrange(match_id, time_rtr) %>% 
    select(c(year, group, 
             survey:n_times_included,
             sentiment_log_rauh, 
             statement_number,
             z_sentiment_log_rauh,
             z_sentiment_log_lexicoder,
             sentiment_log_lexicoder, type_rtr,
             contains("_carried"))) %>% 
    rename(respondent_id = match_id) 


dat_09_final_carried_recode <- dat_09_final_carried %>% 
    mutate(s = str_sub(time_rtr, -2, -1)) %>% 
    mutate(m = str_sub(time_rtr, -4, -3)) %>% 
    mutate(h = str_sub(time_rtr, -6, -5)) %>% 
    select(s, m, h,
           respondent_id, group, age, 
           gender, party_id, 
           political_knowledge,
           political_interest,
           location,
           time_rtr,
           value_rtr, speaker, topic,
           text = text_german, text_english, 
           n_times_included,
           statement_number,
           starts_with("z_sentiment"),
           situation_pos_neg,
           contains("_carried")) %>% 
    ungroup() %>% 
    rename(speaker_raw = speaker,
           speaker = speaker_carried,
           sentiment_log_rauh = sentiment_log_rauh_carried,
           sentiment_log_lexicoder = sentiment_log_lexicoder_carried,
           topic_raw = topic,
           topic = topic_carried)

table(dat_09_final_carried_recode$location,
      dat_09_final_carried_recode$group)


# remove respondents who did not specify their gender
# and only keep respondents who watched the video of the debate

table(dat_09_final_carried_recode$gender)

dat_09_final_carried_recode_complete <- dat_09_final_carried_recode %>% 
    filter(gender != "No answer") %>% 
    filter(group != "Audio only") 


table(dat_09_final_carried_recode_complete$gender)
table(dat_09_final_carried_recode_complete$statement_number)

                  # create segment IDs
dat_09_statement_ids <- dat_09_final_carried_recode_complete %>% 
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


dat_09_final_carried_recode_complete <- left_join(dat_09_final_carried_recode_complete,
                                                  dat_09_statement_ids, 
                                                  by = c("time_rtr", "speaker"))



# save dataset
saveRDS(dat_09_final_carried_recode_complete, "data_tmp/data_2009.rds")
