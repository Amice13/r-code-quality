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


# load package
library(haven)      # CRAN v2.3.1
library(dplyr)      # CRAN v1.0.5
library(tidyr)      # CRAN v1.1.3
library(car)        # CRAN v3.0-10
library(readr)      # CRAN v1.4.0
library(sjlabelled) # CRAN v1.1.7
library(lubridate)  # CRAN v1.7.10
library(hms)        # CRAN v1.0.0
library(zoo)        # CRAN v1.8-8
library(quanteda)   # CRAN v3.0.0
library(stringr)    # CRAN v1.4.0

# set working directory

setwd("")

# The data measuring time starts at 00:01:30 with the intro, 
# which includes the presentation of the duelists and the 
# introduction of the journalists. 
# All times of the coding unit refer to this starting point 
# of the measurement.
# The coding starts at 00:02:15 when Sabine Christiansen,
# after greeting and introducing all participants, 
# begins with the introduction to her initial question.

# # save dataset as csv and import immediately (avoids problems with labels)
# dat_secs_coding <- read_sav("data_dontshare/Duell Inhaltsanalyse Komplett mit RTR.sav")
# 
# #write_csv(dat_secs_coding, "data_dontshare/data_coding_secondlevel_2005.csv")

dat_secs_coding <- read_csv("data_dontshare/data_coding_secondlevel_2005.csv")

dat_secs_rtr <- read_sav("data_dontshare/crm_data_replication_journal_of_communication.sav")

dat_survey <- read_sav("data_dontshare/Duelldaten Mainz mit Recodierungen.sav")

# reshape RTR to long format and create clean time variable
dat_rtr_long <- dat_secs_rtr %>%
    select(-c(SEC, starts_with("MZ"))) %>% 
    gather(respondent_id, value_rtr, -c(ZEIT)) %>% 
    mutate(time_rtr = str_replace_all(ZEIT, ".000000", ""))


table(dat_rtr_long$value_rtr)


# select only relevant variables from survey and recode variables

# get knowledge questions and recode manually
dat_wissen <- dat_survey %>% 
    select(wirtend1,
           eintend1,
           arbtend1)

dat_survey <- dat_survey %>% 
    mutate(knowledge_q1 = ifelse(wirtend1 == 2, 1, 0)) %>% 
    mutate(knowledge_q2 = ifelse(eintend1 == 2, 1, 0)) %>% 
    mutate(knowledge_q3 = ifelse(eintend1 == 1, 1, 0)) 

table(dat_wissen$wirtend1)

# wirtend: chr "Ist das Wirtschaftswachstum zurzeit größer
# oder geringer als vor der Wahl 1998?(1)"
# .. ..- attr(*, "names")= chr [1:4] "größer" "geringer" "gleich groß" "weiß nicht"

# https://de.statista.com/statistik/daten/studie/74644/umfrage/prognose-zur-entwicklung-des-bip-in-deutschland/
# Answer: geringer


# eintend1: Ist die Einkommenssteuer heute höher 
# oder niedriger als vor der Wahl 1998?(1)
# ..- attr(*, "labels")= Named num [1:4] 1 2 3 4
# .. ..- attr(*, "names")= chr [1:4] "höher" "nierdiger" "gleich hoch" "weiß nicht"
# Answer: niedriger
# https://de.wikipedia.org/wiki/Steuerreform_2000_in_Deutschland


# arbtend1: Haben wir im Moment mehr oder weniger Arbeitslose 
# als vor der Wahl 1998?(1)
#  ..- attr(*, "labels")= Named num [1:4] 1 2 3 4
# .. ..- attr(*, "names")= chr [1:4] "mehr" "weniger" "gleich viele" "weiß nicht"
# Answer: weniger
# http://www.sozialpolitik-aktuell.de/tl_files/sozialpolitik-aktuell/_Politikfelder/Arbeitsmarkt/Datensammlung/PDF-Dateien/abbIV31.pdf

str(dat_survey$eintend1)

dat_survey$geschl1_ch <- as.character(dat_survey$geschl1)
dat_survey$parteip1_ch <- as.character(dat_survey$parteip1)

table(dat_survey$geschl1)
dat_survey$interes1

dat_survey_select <- dat_survey %>% 
    mutate(respondent_id = paste0("ID", nummer)) %>% 
    select(respondent_id, 
           political_interest_raw = interes1,
           age = alter1, 
           gender = geschl1_ch,
           party_id = parteip1_ch,
           starts_with("knowledge_")) %>% 
    mutate(gender = car::recode(gender, "'1'='Male';'2'='Female'")) %>% 
    mutate(party_id = car::recode(party_id, "1='CDU/CSU';2='SPD';
                                  3='FDP';4='Greens';5='Left Party';
                                  6='Other party';9='No party ID/No answer'"))

summary(dat_survey_select$age)

dat_survey_select <- dat_survey_select %>% 
    mutate(political_interest = as.character(political_interest_raw)) %>% 
    mutate(political_interest = car::recode(political_interest, 
                                            "'1'='4 very strong';
                                            '2'='3 strong';
                                            '3'='2 medium interest';
                                            '4'='1 not very strong';
                                            '5'='0 no interest at all'"))

table(dat_survey_select$political_interest,useNA = "always")



dat_survey_select <- dat_survey_select %>% 
    mutate(political_knowledge = knowledge_q1 + knowledge_q2 + knowledge_q3) %>% 
    select(-starts_with("knowledge"))

table(dat_survey_select$political_knowledge)

# parteeip1: party preference

# merge survey data to RTR data
dat_rtr_long$respondent_id <- as.character(dat_rtr_long$respondent_id)
dat_survey_select$respondent_id <- as.character(dat_survey_select$respondent_id)

# remove all labels (otherwise, merging does not work)
dat_survey_select <- remove_all_labels(dat_survey_select)
dat_rtr_long <- remove_all_labels(dat_rtr_long)

dat_rtr_survey <- left_join(dat_rtr_long, dat_survey_select,
                            by = "respondent_id")


# now add the second-level coding of the debate

dat_secs_coding <- dat_secs_coding %>% 
    mutate(time_rtr = as.character(Zeit))


str(dat_secs_coding$Blickzuhörer)

# some variables need to be transformed to numeric to enable recoding
dat_secs_coding$blickzuhoerer_clean <- as.numeric(dat_secs_coding$Blickzuhörer)
dat_secs_coding$blickredner_clean <- as.numeric(dat_secs_coding$Blickredner)
dat_secs_coding$laecheln_clean <- as.numeric(dat_secs_coding$Lächeln)



# select only relevant variables
dat_secs_coding_select <- dat_secs_coding %>% 
    mutate(topic = thema) %>% # requires recoding
    mutate(speaker = car::recode(sprecher, "0='Noone';
                                1='Schroeder';
                                2='Merkel';
                                3='Christiansen';
                                4='Illner';
                                5='Kausch';
                                6='Kloeppel'")) %>% 
    mutate(code_actorsinimage = car::recode(Akteure, 
                                            "1='Only Schroeder';
                                            2='Only Merkel';
                                            3='Schroeder and Merkel';
                                            4='Schroeder (frontal) and Merkel';
                                            5='Merkel (frontal) and Schroeder';
                                            6='Only Moderator';
                                            7='Schroeder and Moderator';
                                            8='Merkel and Moderator';
                                            9='Schroeder and Merkel and Moderator'")) %>% 
    mutate(code_emotionalappeal = car::recode(emotion, 
                                              "0='No emotion';
                                        10='Negative emotional appeal';
                                        11='Appeal to fear';
                                        20='Positive emotional appeal';
                                        21='Appeal to togetherness/community'")) %>% 
    mutate(code_directionviewspeaker = car::recode(blickredner_clean,
                                                   "0='No eye contact';
                                                   1='Candidate barely looks past the spectators';
                                                   2='Directly facing the camera';
                                                   3='Facing conversation partner'")) %>% 
    mutate(code_directionviewlistener = car::recode(blickzuhoerer_clean,
                                                    "0='No eye contact';
                                                   1='Candidate barely looks past the spectators';
                                                   2='Directly facing the camera';
                                                   3='Facing conversation partner'")) %>% 
    mutate(code_smile = car::recode(laecheln_clean,
                                    "0='No smile';
                                    1='Light smile';
                                    2='Strong smile'")) %>% 
    mutate(code_vocalpitch = car::recode(Tonlage,
                                         "0='No candidate speaks';
                                         1='Calm/relaxed voice pitch';
                                         2='Energetic pitch'")) %>% 
    mutate(code_directionviewlistener = car::recode(Blickzuhörer,
                                                    "0='No eye contact';
                                                   1='Candidate barely looks past the spectators';
                                                   2='Directly facing the camera';
                                                   3='Facing conversation partner'")) %>% 
    mutate(code_listenercommentingposition = car::recode(Position,
                                                         "0='Neutral';
                                                   1='Disapproval';
                                                   2='Arrogance';
                                                   3='Approval'")) %>% 
    select(time_rtr, topic, speaker,
           starts_with("code_"))


# now add and wrangle the transcript

dat_transcript_raw <- read.csv("data_debate_transcripts/debate_2005_translated.csv",
                               na.strings = "")

# get correct hms date
dat_transcript_raw$hms <- 
    ymd_hms(paste("2005-09-04", dat_transcript_raw$time, sep = " ")) 

head(dat_transcript_raw$hms)

# From the codebook of Nagel et al (2012): 
# "Die Codierung beginnt bei 00:02:15, 
# wenn Sabine Christiansen nach Begrüßung und 
# Vorstellung aller Beteiligter mit der Einleitung
# zu ihrer Eingangsfrage beginnt."

# Schroders first statement starts at 00:02:31 in the 
# replication data, and at 00:01:03 in the transcript. 
# Thus, we need to add 
lubridate::hms("00:02:31") - lubridate::hms("00:01:03")
# 1 minute and 31 seconds = 60 + 28 = 88 seconds to the 
# hms_merge column in dat_transcript_raw

# check out how to add seconds
ymd_hms("2005-09-04 00:01:03") + lubridate::dseconds(88)

dat_transcript_raw <- dat_transcript_raw %>% 
    mutate(hms_merge_content = hms + lubridate::dseconds(88))

# check time for 6th row (first statement by Schroder)

dat_transcript_raw[6, ]


dat_secs_coding_select$hms_merge_content <- ymd_hms(paste("2005-09-04",
                                                          dat_secs_coding_select$time_rtr, 
                                                          sep = " "))


# now get a single "text column" with German and English text
# remove observations where text is NA and then construct the statement_number variable

# check whether Schröder's or Mekel's statement is longer when it starts 
# in the same second


nwords <- function(string, pseudo=F){
    ifelse( pseudo, 
            pattern <- "\\S+", 
            pattern <- "[[:alpha:]]+" 
    )
    str_count(string, pattern)
}

dat_transcript_raw <- dat_transcript_raw %>% 
    mutate(words_schroder = nwords(schroder),
           words_merkel = nwords(merkel)) %>% 
    mutate(words_merkel = ifelse(is.na(words_merkel), 0, words_merkel),
           words_schroder = ifelse(is.na(words_schroder), 0, words_schroder))

# now select the speaker whose statement is longer when 
# it starts at the same second. 

dat_transcript_raw <- dat_transcript_raw %>% 
    mutate(speaker_select = ifelse(words_merkel > words_schroder, "Merkel",
                                   ifelse(words_schroder > words_merkel, "Schroder", 
                                          ifelse(words_schroder == 0 & words_merkel == 0, NA, NA))))


equal_length <- dat_transcript_raw %>% 
    filter(words_merkel == words_schroder) %>% 
    filter(words_merkel != 0)
equal_length$words_merkel

nrow(equal_length)

# a manual inspection shows that Merkel talks before and after this statement,
# and Schröder interrupts her - therefore, we assign these 13 words to Merkel

dat_transcript_raw <- dat_transcript_raw %>% 
    mutate(speaker_select = ifelse(words_merkel == 13 & words_schroder == 13, "Merkel",
                                   speaker_select))



equal_length$time

# now get one text column
dat_transcript_raw <- dat_transcript_raw %>% 
    mutate(text = ifelse(speaker_select == "Merkel", merkel,
                         ifelse(speaker_select == "Schroder", schroder, NA))) %>% 
    mutate(text = ifelse(is.na(text), "TEXT MODERATOR", text)) %>% 
    #filter(!is.na(text)) %>% 
    mutate(statement_number = paste("s", speaker_select, 1:n(), sep = "_"))



dat_transcript_raw <- dat_transcript_raw %>% 
    mutate(text_translated = ifelse(speaker_select == "Merkel", merkel_translated,
                                    ifelse(speaker_select == "Schroder", schroder_translated, NA))) %>% 
    mutate(text_translated = ifelse(is.na(text_translated), "TEXT MODERATOR", text_translated)) 


# perform sentiment analysis and load dictionaries:
# Lexicoder and Rauh's sentiment dictionary

data_dictionary_lexicoder <- readRDS("data_dictionary_lsd_de.rds")

data_dictionary_rauh <- readRDS("data_dictionary_rauh.rds")

corp_05 <- corpus(dat_transcript_raw, text_field = "text")

dat_05_sentiment_lexicoder <- corp_05 %>% 
    tokens() %>% 
    tokens_lookup(dictionary = data_dictionary_lexicoder,
                  nested_scope = "dictionary") %>% 
    dfm() %>% 
    quanteda::convert(to = "data.frame") %>% 
    mutate(sentiment_log_lexicoder = 
               log((pos + 0.5) / (neg + 0.5))) %>% 
    select(-c(pos, neg))


dat_05_sentiment_rauh <- corp_05 %>% 
    tokens() %>% 
    tokens_lookup(dictionary = data_dictionary_rauh,
                  nested_scope = "dictionary") %>% 
    dfm() %>% 
    quanteda::convert(to = "data.frame") %>% 
    mutate(sentiment_log_rauh = 
               log((positive + 0.5) / (negative + 0.5))) %>% 
    select(-c(doc_id, positive, negative, neg_positive, neg_negative))

# merge sentiment scores with content analysis
dat_transcript_sentiment <- bind_cols(dat_transcript_raw,
                                      dat_05_sentiment_lexicoder,
                                      dat_05_sentiment_rauh)


# change sentiment from 0 to NA if text == MODERATOR

dat_transcript_sentiment <- dat_transcript_sentiment %>% 
    mutate(sentiment_log_rauh = ifelse(text == "TEXT MODERATOR", NA, 
                                       sentiment_log_rauh)) %>% 
    mutate(sentiment_log_lexicoder = ifelse(text == "TEXT MODERATOR", NA, 
                                            sentiment_log_lexicoder))


table(dat_transcript_sentiment$sentiment_log_lexicoder)

dat_secs_coding_select_text <- left_join(dat_secs_coding_select, dat_transcript_sentiment,
                                         by = "hms_merge_content")

table(dat_secs_coding_select_text$speaker_select,
      dat_secs_coding_select_text$speaker)


# standarise sentiment
dat_secs_coding_select_text <- dat_secs_coding_select_text %>% 
    group_by(speaker) %>%  
    mutate(z_sentiment_log_lexicoder = (sentiment_log_lexicoder - mean(sentiment_log_lexicoder, na.rm = T)) / sd(sentiment_log_lexicoder, na.rm = T),
           z_sentiment_log_rauh = (sentiment_log_rauh - mean(sentiment_log_rauh, na.rm = T)) / sd(sentiment_log_rauh, na.rm = T)) %>% 
    ungroup()



# remove unncessary text columns <
dat_secs_coding_select_text <- dat_secs_coding_select_text %>% 
    select(-c("merkel", "schroder", "moderators", "merkel_translated",
              "doc_id", starts_with("words_"),
              "speaker_select",
              "schroder_translated"))

dat_rtr_survey <- dat_rtr_survey %>% 
    mutate(time_rtr = as.character(time_rtr))

# join RTR data and second-level (content) analysis
dat_2005_full <- left_join(dat_rtr_survey, dat_secs_coding_select_text,
                           by = "time_rtr")



# The coding starts at 00:02:15 when Sabine Christiansen,
# after greeting and introducing all participants, 
# begins with the introduction to her initial question.

dat_2005_full_arranged <- dat_2005_full %>% 
    arrange(time_rtr) %>% 
    select(-c(ZEIT, political_interest_raw)) %>% 
    arrange(respondent_id, time_rtr) %>% 
    filter(!is.na(age)) #remove respondent with missing information on age



dat_2005_statement_ids <- dat_2005_full_arranged %>% 
    ungroup() %>% 
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


# carry over the sentiment variables


dat_2005_full_arranged_final <- left_join(dat_2005_full_arranged,
                                          dat_2005_statement_ids, 
                                          by = c("time_rtr", "speaker"))


dat_2005_full_arranged_final <- dat_2005_full_arranged_final %>%
    group_by(respondent_id) %>% 
    mutate(statement_number = zoo::na.locf(statement_number, na.rm = FALSE))


dat_2005_full_arranged_final <- dat_2005_full_arranged_final %>% 
    group_by(respondent_id, statement_number) %>% 
    mutate(sentiment_log_rauh = zoo::na.locf(sentiment_log_rauh, na.rm = FALSE)) %>% 
    mutate(sentiment_log_lexicoder = zoo::na.locf(sentiment_log_lexicoder, na.rm = FALSE)) %>% 
    mutate(z_sentiment_log_rauh = zoo::na.locf(z_sentiment_log_rauh, na.rm = FALSE)) %>% 
    mutate(z_sentiment_log_lexicoder = zoo::na.locf(z_sentiment_log_lexicoder, na.rm = FALSE))


# change sentiment to NA if speaker != Schroder or Merkel

table(dat_2005_full_arranged$speaker)

dat_2005_full_arranged <- dat_2005_full_arranged %>% 
    ungroup() %>% 
    mutate(sentiment_log_rauh = ifelse(speaker %in% c("Schroeder", "Merkel"),
                                       sentiment_log_rauh, NA)) %>% 
    mutate(sentiment_log_lexicoder = ifelse(speaker %in% c("Schroeder", "Merkel"),
                                            sentiment_log_lexicoder, NA)) %>% 
    mutate(z_sentiment_log_rauh = ifelse(speaker %in% c("Schroeder", "Merkel"),
                                         z_sentiment_log_rauh, NA)) %>% 
    mutate(z_sentiment_log_lexicoder = ifelse(speaker %in% c("Schroeder", "Merkel"),
                                              z_sentiment_log_lexicoder, NA))




table(dat_2005_full_arranged_final$time_rtr)

dat_2005_full_arranged_final <- rename(dat_2005_full_arranged_final, text_english = text_translated) %>% 
    ungroup()

# save 2005 debate
saveRDS(dat_2005_full_arranged_final, "data_tmp/data_2005.rds")

