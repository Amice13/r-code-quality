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

library(readr)   # CRAN v1.4.0
library(dplyr)   # CRAN v1.0.5
library(tidyr)   # CRAN v1.1.3
library(stringr) # CRAN v1.4.0
library(car)     # CRAN v3.0-10

# set working directory
setwd("")


# load topics (merged for all debates)

dat_topics <- read_csv("data_topics_tvdebates_combined.csv")

dat_topics <- dat_topics %>% 
    select(year, topic, topic_broad_policy_area, 
           gender_policy_area, n) %>% 
    mutate(topic_broad_policy_area = car::recode(topic_broad_policy_area,
                                                 "'foreign_policy'='Foreign policy';
                                                 'foreign_pol'='Foreign policy';
                                                 'gen_gov'='Government affairs (general)';
                                                 'enviro'='Environment'")) %>% 
    mutate(topic_broad_policy_area = str_to_title(topic_broad_policy_area)) %>% 
    mutate(gender_policy_area = str_to_title(gender_policy_area)) %>% 
    mutate(gender_policy_area_narrow = str_replace_all(gender_policy_area,  " / Feminine", "")) %>% 
    mutate(gender_policy_area_narrow = str_replace_all(gender_policy_area_narrow,  " / Masculine", "")) %>% 
    mutate(gender_policy_area_broad = str_replace_all(gender_policy_area, "Neutral / ", "")) %>% 
    select(-gender_policy_area) %>% 
    rename(code_numeric = n) 


# check topics

table(dat_topics$gender_policy_area_narrow)
table(dat_topics$gender_policy_area_broad)


## 2017 ----

dat_17_rtr_dial <- readRDS("data_tmp/data_2017.rds")

dat_17_rtr_dial <- dat_17_rtr_dial %>% 
    mutate(value_rtr = value_rtr_dial,
           type_rtr = "Dial")

# load emotion data
dat_17_frames <- read_csv("secdata_2017.csv")

# check how the time is coded
table(dat_17_frames$h)
table(dat_17_frames$m)
table(dat_17_frames$s)

# add trailing 0s to harmonize the h, m, s data
dat_17_frames$h <- sprintf("%02d", as.numeric(dat_17_frames$h))
dat_17_frames$m <- sprintf("%02d", as.numeric(dat_17_frames$m))
dat_17_frames$s <- sprintf("%02d", as.numeric(dat_17_frames$s))


dat_17_frames <- dat_17_frames %>% 
    mutate(hms_merge = paste(h, m, s, sep = ":")) %>% 
    select(-c(h, m, s))

# add second-level pitch data
dat_17_pitch <- read_csv("pitch_duell_2017.csv")

# add trailing 0s to harmonize the h, m, s data
dat_17_pitch$h <- sprintf("%02d", as.numeric(dat_17_pitch$h))
dat_17_pitch$m <- sprintf("%02d", as.numeric(dat_17_pitch$m))
dat_17_pitch$s <- sprintf("%02d", as.numeric(dat_17_pitch$s))

dat_17_pitch <- dat_17_pitch %>% 
    mutate(hms_merge = paste(h, m, s, sep = ":")) %>% 
    select(-c(h, m, s))

# merge pitch and frame data
dat_17_frames_pitch <- left_join(dat_17_frames,
                                 dat_17_pitch, 
                                 by = "hms_merge")


# adjust h, m, s in dial data
dat_17_rtr_dial$h <- sprintf("%02d", as.numeric(dat_17_rtr_dial$h))
dat_17_rtr_dial$m <- sprintf("%02d", as.numeric(dat_17_rtr_dial$m))
dat_17_rtr_dial$s <- sprintf("%02d", as.numeric(dat_17_rtr_dial$s))


dat_17_rtr_dial <- dat_17_rtr_dial %>% 
    mutate(hms_merge = paste(h, m, s, sep = ":")) %>% 
    select(-c(h, m, s))

# load dial data and pitch
dat_17_merged <- left_join(dat_17_rtr_dial, dat_17_frames_pitch,
                           by = "hms_merge")


dat_17_merged <- dat_17_merged %>% 
    mutate(speaker = str_squish(speaker)) %>% 
    mutate(cv_lastname = str_squish(cv_lastname))

# merge topics
dat_17_merged <- left_join(dat_17_merged,
                           filter(dat_topics, year == "2017"),
                           by = "topic")

table(dat_17_merged$gender_policy_area_broad, useNA = "always")
table(dat_17_merged$topic_broad_policy_area, useNA = "always")

# recode policy area
dat_17_merged <- dat_17_merged %>%
    mutate(topic_broad_policy_area = ifelse(is.na(topic_broad_policy_area), "None", topic_broad_policy_area)) %>% 
    mutate(gender_policy_area_broad = ifelse(is.na(gender_policy_area_broad), 
                                             "None", gender_policy_area_broad)) %>% 
    mutate(gender_policy_area_narrow = ifelse(is.na(gender_policy_area_narrow), 
                                              "None", gender_policy_area_narrow))


table(dat_17_merged$year, useNA = "always")

dat_17_merged$year <- "2017"

# save 2017 data
saveRDS(dat_17_merged, "data_tmp/data_full_2017.rds")


## 2017 (minor parties) ----

dat_17_minor_rtr_dial <- readRDS("data_tmp/data_2017_minor.rds")

# rename variables
dat_17_minor_rtr_dial <- dat_17_minor_rtr_dial %>% 
    mutate(value_rtr = value_rtr_dial,
           type_rtr = "Dial")


dat_17_minor_frames <- read_csv("secdata_2017_minor.csv")

# check how the time is coded
table(dat_17_minor_frames$h)
table(dat_17_minor_frames$m)
table(dat_17_minor_frames$s)

# add trailing 0s to harmonize the h, m, s data
dat_17_minor_frames$h <- sprintf("%02d", as.numeric(dat_17_minor_frames$h))
dat_17_minor_frames$m <- sprintf("%02d", as.numeric(dat_17_minor_frames$m))
dat_17_minor_frames$s <- sprintf("%02d", as.numeric(dat_17_minor_frames$s))


dat_17_minor_frames <- dat_17_minor_frames %>% 
    mutate(hms_merge = paste(h, m, s, sep = ":")) %>% 
    select(-c(h, m, s))

# add second-level pitch data
dat_17_minor_pitch <- read_csv("pitch_duell_2017_minor.csv")

# add trailing 0s to harmonize the h, m, s data
dat_17_minor_pitch$h <- sprintf("%02d", as.numeric(dat_17_minor_pitch$h))
dat_17_minor_pitch$m <- sprintf("%02d", as.numeric(dat_17_minor_pitch$m))
dat_17_minor_pitch$s <- sprintf("%02d", as.numeric(dat_17_minor_pitch$s))

dat_17_minor_pitch <- dat_17_minor_pitch %>% 
    mutate(hms_merge = paste(h, m, s, sep = ":")) %>% 
    select(-c(h, m, s))

# merge pitch and frame data
dat_17_minor_frames_pitch <- left_join(dat_17_minor_frames,
                                       dat_17_minor_pitch, 
                                       by = "hms_merge")


dat_17_minor_rtr_dial$h <- sprintf("%02d", as.numeric(dat_17_minor_rtr_dial$h))
dat_17_minor_rtr_dial$m <- sprintf("%02d", as.numeric(dat_17_minor_rtr_dial$m))
dat_17_minor_rtr_dial$s <- sprintf("%02d", as.numeric(dat_17_minor_rtr_dial$s))


dat_17_minor_rtr_dial <- dat_17_minor_rtr_dial %>% 
    mutate(hms_merge = paste(h, m, s, sep = ":")) %>% 
    select(-c(h, m, s))


dat_17_minor_merged <- left_join(dat_17_minor_rtr_dial, dat_17_minor_frames_pitch,
                                 by = "hms_merge")


dat_17_minor_merged <- dat_17_minor_merged %>% 
    mutate(speaker = str_squish(speaker)) %>% 
    mutate(cv_lastname = str_squish(cv_lastname))

# merge topics
dat_17_minor_merged <- left_join(dat_17_minor_merged,
                                 filter(dat_topics, year == "2017 (minor)"))

table(dat_17_minor_merged$speaker,
      dat_17_minor_merged$cv_lastname)

dat_17_minor_merged <- dat_17_minor_merged %>%
    mutate(topic_broad_policy_area = ifelse(is.na(topic_broad_policy_area), "None", topic_broad_policy_area)) %>% 
    mutate(gender_policy_area_broad = ifelse(is.na(gender_policy_area_broad), 
                                             "None", gender_policy_area_broad)) %>% 
    mutate(gender_policy_area_narrow = ifelse(is.na(gender_policy_area_narrow), 
                                              "None", gender_policy_area_narrow))

table(dat_17_minor_merged$gender_policy_area_broad)
table(dat_17_minor_merged$speaker,
      dat_17_minor_merged$cv_lastname)

table(dat_17_minor_merged$year, useNA = "always")

dat_17_minor_merged$year <- "2017 (minor)"


# save 2017 data
saveRDS(dat_17_minor_merged, "data_tmp/data_full_2017_minor.rds")


## 2013 ----

# load merged data
dat_13_rtr_full <- readRDS("data_tmp/data_2013.rds")

# rename some variables
dat_13_rtr_full <- dat_13_rtr_full %>% 
    mutate(value_rtr = value_rtr_dial,
           type_rtr = "Dial")


# load second-level frame data
dat_13_frames <- read_csv("secdata_2013.csv")

# check how the time is coded
table(dat_13_frames$h)
table(dat_13_frames$m)
table(dat_13_frames$s)

# add trailing 0s to harmonize the h, m, s data
dat_13_frames$h <- sprintf("%02d", as.numeric(dat_13_frames$h))
dat_13_frames$m <- sprintf("%02d", as.numeric(dat_13_frames$m))
dat_13_frames$s <- sprintf("%02d", as.numeric(dat_13_frames$s))

dat_13_frames <- dat_13_frames %>% 
    mutate(hms_merge = paste(h, m, s, sep = ":")) %>% 
    select(-c(h, m, s))


# add second-level pitch data
dat_13_pitch <- read_csv("pitch_duell_2013.csv")

# add trailing 0s to harmonize the h, m, s data
dat_13_pitch$h <- sprintf("%02d", as.numeric(dat_13_pitch$h))
dat_13_pitch$m <- sprintf("%02d", as.numeric(dat_13_pitch$m))
dat_13_pitch$s <- sprintf("%02d", as.numeric(dat_13_pitch$s))

dat_13_pitch <- dat_13_pitch %>% 
    mutate(hms_merge = paste(h, m, s, sep = ":")) %>% 
    select(-c(h, m, s))

# merge pitch and frame data
dat_13_frames_pitch <- left_join(dat_13_frames,
                                 dat_13_pitch,
                                 by = "hms_merge")


dat_13_rtr_full$h <- sprintf("%02d", as.numeric(dat_13_rtr_full$h))
dat_13_rtr_full$m <- sprintf("%02d", as.numeric(dat_13_rtr_full$m))
dat_13_rtr_full$s <- sprintf("%02d", as.numeric(dat_13_rtr_full$s))

dat_13_rtr_full <- dat_13_rtr_full %>% 
    mutate(hms_merge = paste(h, m, s, sep = ":")) %>% 
    select(-c(h, m, s))


dat_13_merged <- left_join(dat_13_rtr_full, dat_13_frames_pitch,
                           by = "hms_merge")


# use the same names for cv_lastname and speaker
dat_13_merged <- dat_13_merged %>% 
    mutate(cv_lastname = car::recode(cv_lastname, "'Steinbruck'='Steinbrueck'")) %>% 
    mutate(speaker = str_squish(speaker)) %>% 
    mutate(cv_lastname = str_squish(cv_lastname))


# merge topics
dat_13_merged <- left_join(dat_13_merged,
                           filter(dat_topics, year == "2013"))

table(dat_13_merged$gender_policy_area_broad)

dat_13_merged <- select(dat_13_merged, -code_numeric)


dat_13_merged <- dat_13_merged %>%
    mutate(topic_broad_policy_area = ifelse(is.na(topic_broad_policy_area), "None", topic_broad_policy_area)) %>% 
    mutate(gender_policy_area_broad = ifelse(is.na(gender_policy_area_broad), 
                                             "None", gender_policy_area_broad)) %>% 
    mutate(gender_policy_area_narrow = ifelse(is.na(gender_policy_area_narrow), 
                                              "None", gender_policy_area_narrow))

table(dat_13_merged$gender_policy_area_broad)

dat_13_merged <- dat_13_merged %>% 
    mutate(value_rtr = value_rtr_dial)

table(dat_13_merged$year, useNA = "always")

dat_13_merged$year <- "2013"


saveRDS(dat_13_merged, "data_tmp/data_full_2013.rds")


## 2009 ----

# load merged data from 2009 debate
dat_09_rtr_full <- readRDS("data_tmp/data_2009.rds")

dat_09_rtr_full <- dat_09_rtr_full %>% 
    mutate(value_rtr = value_rtr,
           type_rtr = "Dial")

# load second-level frame data
dat_09_frames <- read_csv("secdata_2009.csv")

# check how the time is coded
table(dat_09_frames$h)
table(dat_09_frames$m)
table(dat_09_frames$s)

# add trailing 0s to harmonize the h, m, s data
dat_09_frames$h <- sprintf("%02d", as.numeric(dat_09_frames$h))
dat_09_frames$m <- sprintf("%02d", as.numeric(dat_09_frames$m))
dat_09_frames$s <- sprintf("%02d", as.numeric(dat_09_frames$s))

dat_09_frames <- dat_09_frames %>% 
    mutate(hms_merge = paste(h, m, s, sep = ":")) %>% 
    select(-c(h, m, s))


# add second-level pitch data
dat_09_pitch <- read_csv("pitch_duell_2009.csv")

# add trailing 0s to harmonize the h, m, s data
dat_09_pitch$h <- sprintf("%02d", as.numeric(dat_09_pitch$h))
dat_09_pitch$m <- sprintf("%02d", as.numeric(dat_09_pitch$m))
dat_09_pitch$s <- sprintf("%02d", as.numeric(dat_09_pitch$s))

dat_09_pitch <- dat_09_pitch %>% 
    mutate(hms_merge = paste(h, m, s, sep = ":")) %>% 
    select(-c(h, m, s))

# merge pitch and frame data
dat_09_frames_pitch <- left_join(dat_09_frames,
                                 dat_09_pitch, 
                                 by = "hms_merge")


dat_09_rtr_full$h <- sprintf("%02d", as.numeric(dat_09_rtr_full$h))
dat_09_rtr_full$m <- sprintf("%02d", as.numeric(dat_09_rtr_full$m))
dat_09_rtr_full$s <- sprintf("%02d", as.numeric(dat_09_rtr_full$s))

dat_09_rtr_full <- dat_09_rtr_full %>% 
    mutate(hms_merge = paste(h, m, s, sep = ":")) %>% 
    select(-c(h, m, s))

# load RTR data with frame and pitch data
dat_09_merged <- left_join(dat_09_rtr_full, dat_09_frames_pitch,
                           by = "hms_merge")

dat_09_merged <- dat_09_merged %>% 
    mutate(cv_lastname = str_squish(cv_lastname))

# merge topics
dat_09_merged <- left_join(dat_09_merged,
                           filter(dat_topics, year == "2009"))

table(dat_09_merged$gender_policy_area_broad)

dat_09_merged <- select(dat_09_merged, -code_numeric)

# recode policy area
dat_09_merged <- dat_09_merged %>%
    mutate(topic_broad_policy_area = ifelse(is.na(topic_broad_policy_area), "None", topic_broad_policy_area)) %>% 
    mutate(gender_policy_area_broad = ifelse(is.na(gender_policy_area_broad), 
                                             "None", gender_policy_area_broad)) %>% 
    mutate(gender_policy_area_narrow = ifelse(is.na(gender_policy_area_narrow), 
                                              "None", gender_policy_area_narrow))



table(dat_09_merged$year, useNA = "always")

dat_09_merged$year <- "2009"


# save 2009 data
saveRDS(dat_09_merged, "data_tmp/data_full_2009.rds")


## 2005 ----

dat_05_rtr <- readRDS("data_tmp/data_2005.rds")


dat_05_rtr <- dat_05_rtr %>% 
    mutate(type_rtr = "Dial") %>% 
    mutate(speaker_carried = speaker)

# load second-level emotion data
dat_05_frames <- read_csv("secdata_2005.csv")

# remove the top 39 rows because they are from the CSPAN announcement
# and because the data won't align otherwise

dat_05_frames <- dat_05_frames[-c(1:39), ]

# check how the time is coded
table(dat_05_frames$h)
table(dat_05_frames$m)
table(dat_05_frames$s)

# add trailing 0s to harmonize the h, m, s data

dat_05_frames$h <- sprintf("%02d", as.numeric(dat_05_frames$h))
dat_05_frames$m <- sprintf("%02d", as.numeric(dat_05_frames$m))
dat_05_frames$s <- sprintf("%02d", as.numeric(dat_05_frames$s))

dat_05_frames <- dat_05_frames %>% 
    mutate(hms_frames = paste(h, m, s, sep = ":")) %>% 
    select(-c(h, m, s))


# The times are not aligned: the first time stamp in the rtr data is 00:01:31 
# because the data started earlier

# Thus, we select the unique time values in both data sets, create a number ranging from 0 to nrow-1
# and use this variable (which then in both datasets starts at 0) to merge the second-level observations


dat_05_rtr_times <- dat_05_rtr %>% 
    ungroup() %>% 
    select(time_rtr) %>% 
    unique() %>% 
    arrange(time_rtr) %>% 
    mutate(n_second_merge = 0:(n()-1))

head(dat_05_rtr_times$n_second_merge)


dat_05_rtr_new_time_code <- dat_05_rtr %>% 
    left_join(dat_05_rtr_times, by = "time_rtr")

head(dat_05_rtr_new_time_code$n_second_merge)

stopifnot(nrow(dat_05_rtr) == nrow(dat_05_rtr_new_time_code))

head(dat_05_rtr_new_time_code$n_second_merge)


# repeat the same for the second-level data

dat_05_frames_times <- dat_05_frames %>% 
    ungroup() %>% 
    select(hms_frames) %>% 
    unique() %>% 
    arrange(hms_frames) %>% 
    mutate(n_second_merge = 0:(n()-1))

head(dat_05_frames_times$n_second_merge)

dat_05_frames_new_time_code <- dat_05_frames %>% 
    left_join(dat_05_frames_times, by = "hms_frames")

stopifnot(nrow(dat_05_frames) == nrow(dat_05_frames_new_time_code))

head(dat_05_frames_new_time_code$n_second_merge)

# add second-level pitch  and text data

dat_05_pitch <- read_csv("pitch_duell_2005.csv")


dat_05_pitch$h <- sprintf("%02d", as.numeric(dat_05_pitch$h))
dat_05_pitch$m <- sprintf("%02d", as.numeric(dat_05_pitch$m))
dat_05_pitch$s <- sprintf("%02d", as.numeric(dat_05_pitch$s))

dat_05_pitch <- dat_05_pitch %>% 
    mutate(hms_frames = paste(h, m, s, sep = ":")) %>% 
    select(-c(h, m, s))

# merge pitch and frame data

dat_05_frames_pitch_new_time_code <- left_join(dat_05_frames_new_time_code,
                                               dat_05_pitch, by = "hms_frames")

# now merge the two ...new_time_code data frames

dat_05_merged <- left_join(dat_05_rtr_new_time_code, 
                           dat_05_frames_pitch_new_time_code,
                           by = "n_second_merge")


dat_05_merged <- dat_05_merged %>% 
    select(hms_frames, everything())



# rename topic_more_detail to topic in order to merge it properly
dat_topics_05 <- filter(dat_topics, year == "2005")

dat_topics_05 <- select(dat_topics_05, -topic)

dat_topics_05 <- rename(dat_topics_05, topic = code_numeric)

dat_topics_05$year <- as.character(dat_topics_05$year)

# now merge by topic code (not text)

# merge topics

dat_05_merged$topic <- as.character(dat_05_merged$topic)
dat_topics_05$topic <- as.character(dat_topics_05$topic)

dat_05_merged <- left_join(dat_05_merged,
                           dat_topics_05)

dat_05_merged <- dat_05_merged %>% 
    mutate(cv_lastname = str_replace_all(cv_lastname, 'Schroder', 'Schroeder')) %>% 
    mutate(cv_lastname = str_squish(cv_lastname))

table(dat_05_merged$cv_lastname)

table(dat_05_merged$gender_policy_area_narrow, useNA = "always")

dat_05_merged <- dat_05_merged %>%
    mutate(topic_broad_policy_area = ifelse(is.na(topic_broad_policy_area), "None", topic_broad_policy_area)) %>% 
    mutate(gender_policy_area_broad = ifelse(is.na(gender_policy_area_broad), 
                                             "None", gender_policy_area_broad)) %>% 
    mutate(gender_policy_area_narrow = ifelse(is.na(gender_policy_area_narrow), 
                                              "None", gender_policy_area_narrow))



                 # create a new variable called hms_merge which is identical to time_rtr 
# this ensures that the same variables have the same names across all debates

dat_05_merged <- dat_05_merged %>% 
    mutate(hms_merge = time_rtr) %>% 
    select(time_rtr, hms_merge, respondent_id, everything())


table(dat_05_merged$year, useNA = "always")

dat_05_merged$year <- "2005"

# save dataset
saveRDS(dat_05_merged, "data_tmp/data_full_2005.rds")


