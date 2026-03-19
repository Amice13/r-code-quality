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

library(dplyr)   # CRAN v1.0.5     
library(tidyr)   # CRAN v1.1.3
library(stringr) # CRAN v1.4.0
library(rlang)   # CRAN v0.4.11

# set working directory
setwd("")


#------------
# Functions
#------------

generate_lag <- function(data, vname, l){
    # Takes a data frame, a variable name (string), 
    # and a lag value "l" (integer), and returns 
    # the data frame with the lagged value included.
    
    vlabel = rlang::sym(paste(vname, '_l', l, sep = ''))
    vname = rlang::sym(vname)
    data_ <- data %>% 
        group_by(respondent_id) %>% 
        mutate(!!vlabel := lag(!!vname, n = l, order_by = time_rtr))
    
    return(data_)
}

generate_lags <- function(data, vname, lag_num){
    # Takes a data frame, a variable name (string), 
    # and the number of lags to calculate "lag_num" (integer), 
    # and returns the data frame with the lagged value included.
    
    data_ = data
    for (l in seq(lag_num)){
        data_ = generate_lag(data_, vname, l)
    }
    return(data_)
}


## 2005 ----

# load data
dat_2005_raw <- readRDS("data_tmp/data_full_2005.rds") %>% ungroup()


table(dat_2005_raw$year, useNA = "always")

# arrange 2005 data correctly
dat_2005_raw <- arrange(dat_2005_raw, respondent_id, time_rtr)
nrow(dat_2005_raw)

# remove text columns

dat_2005_raw <- select(dat_2005_raw, -c(text, text_english))

table(dat_2005_raw$statement_number)

# only keep unique observations
dat_2005 <- unique(dat_2005_raw)

nrow(dat_2005) - nrow(dat_2005_raw)

# Output debate time master file
master2005 <- distinct(subset(dat_2005, select = c(time_rtr, respondent_id)))

# Generate new emotion variables
dat_2005$emo_noneutral <- 1 - dat_2005$emo_neutral # non-neutral emotion
emo_neg_val_mtx <- subset(dat_2005, select = c(emo_anger, emo_contempt, emo_disgust, emo_fear, emo_sadness))
dat_2005$emo_neg_val <- rowSums(emo_neg_val_mtx)

# standardize variables (cv_lastname level: facial emotions)
dat_2005 <- dat_2005 %>%
    group_by(cv_lastname) %>%
    mutate(z_emo_noneutral = (emo_noneutral - mean(emo_noneutral)) / sd(emo_noneutral),
           z_emo_neg_val = (emo_neg_val - mean(emo_neg_val)) / sd(emo_neg_val)) %>% 
    ungroup()

nrow(dat_2005)

# standardize variables that require that the person in the image is also speaking
# (we can achieve this by grouping by speaker and cv_lastname and getting standardised values for all possible combinations)

dat_2005 <- dat_2005 %>% 
    group_by(speaker, cv_lastname) %>% 
    mutate(z_frequency = (frequency - mean(frequency)) / sd(frequency),
           mean_frequency_speaker_cv_lastname_combo = mean(frequency),
           mean_sentiment_lexicoder_speaker_cv_lastname_combo = mean(sentiment_log_lexicoder, na.rm = TRUE),
           z_strength = (strength - mean(strength)) / sd(strength),
           z_sentiment_log_lexicoder2 = (sentiment_log_lexicoder - mean(sentiment_log_lexicoder, na.rm = T)) / sd(sentiment_log_lexicoder, na.rm = T),
           z_sentiment_log_rauh2 = (sentiment_log_rauh - mean(sentiment_log_rauh, na.rm = T)) / sd(sentiment_log_rauh, na.rm = T)) %>% 
    group_by(time_rtr, speaker) %>% 
    mutate(n_secs_included = n()) %>%
    arrange(-n_secs_included, time_rtr) %>%
    select(respondent_id, time_rtr, n_secs_included, speaker, cv_lastname, frequency,
           mean_frequency_speaker_cv_lastname_combo, z_frequency,
           z_emo_noneutral,z_sentiment_log_lexicoder,
           mean_sentiment_lexicoder_speaker_cv_lastname_combo,
           z_sentiment_log_rauh, everything()) %>%
    ungroup() %>%
    arrange(respondent_id, time_rtr)


dat_2005 <- dat_2005 %>% 
    mutate(z_frequency = ifelse(speaker != cv_lastname, NA, z_frequency)) %>% 
    mutate(z_strength = ifelse(speaker != cv_lastname, NA, z_strength))


# Generate lags. Split dataframe by speaker in order to attribute second to
# one of the politicians.

# Variables to lag
vnames = c('z_emo_anger',
           'z_emo_contempt',
           'z_emo_disgust',
           'z_emo_fear',
           'z_emo_happiness',
           'z_emo_neutral',
           'z_emo_sadness',
           'z_emo_surprise',
           'z_emo_noneutral',
           'z_emo_neg_val',
           'z_frequency',
           'z_strength',
           'frequency',
           'strength',
           'z_sentiment_log_lexicoder',
           'z_sentiment_log_rauh')


# Merkel -- note duplicates formed here
merkel_2005 <- dat_2005 %>%
    filter(speaker_carried == "Merkel" & cv_lastname == "Merkel")

merkel_2005 <- merkel_2005 %>%
    distinct(time_rtr, respondent_id, .keep_all = TRUE) # drop duplicate timestamp

# Merge to full debate in order to ensure time is handeled correctly
merkel_merged <- merge(master2005, merkel_2005, 
                       by = c('respondent_id','time_rtr'), 
                       all.x = T)

# Generate lags...
merkel_ <- merkel_merged
for (vname in vnames){
    print(paste('Processing', vname))
    merkel_ = generate_lags(merkel_, vname, l = 4)
}

# Merge back to Merkel's 2005 observations to ensure a single
# candidate per second
merkel_$time <- seq.int(nrow(merkel_))
merkel_ <- merkel_ %>%
    dplyr::select(respondent_id, 
                  time_rtr, 
                  contains("anger_l"),
                  contains("contempt_l"),
                  contains("disgust_l"),
                  contains("fear_l"),
                  contains("happiness_l"),
                  contains("neutral_l"),
                  contains("sadness_l"),
                  contains("surprise_l"),
                  contains("noneutral_l"),
                  contains("_val_l"),
                  contains("frequency_l"),
                  contains("strength_l"),
                  contains("sentiment_log_lexicoder_l"),
                  contains("sentiment_log_rauh_l"))

merkel_final <- merge(merkel_2005, merkel_,
                      by=c('respondent_id', 'time_rtr'), 
                      all.x = T)

# Schroeder
schroeder_2005 <- dat_2005 %>%
    filter(speaker == "Schroeder" & cv_lastname == "Schroeder")

summary(schroeder_2005$z_frequency)

# Merge to full debate in order to ensure time is handeled correctly
schroeder_merged <-  merge(master2005, schroeder_2005, 
                           by = c('respondent_id','time_rtr'), 
                           all.x = T)

# Generate lags...
schroeder_ <- schroeder_merged
for (vname in vnames){
    print(paste('Processing', vname))
    schroeder_ = generate_lags(schroeder_, vname, l = 4)
}

# Merge back to Schroeder's 2005 observations to ensure a single
# candidate per second
schroeder_$time <- seq.int(nrow(schroeder_))
schroeder_ <- schroeder_ %>%
    dplyr::select(respondent_id, 
                  time_rtr, 
                  contains("anger_l"),
                  contains("contempt_l"),
                  contains("disgust_l"),
                  contains("fear_l"),
                  contains("happiness_l"),
                  contains("neutral_l"),
                  contains("sadness_l"),
                  contains("surprise_l"),
                  contains("noneutral_l"),
                  contains("_val_l"),
                  contains("frequency_l"),
                  contains("strength_l"),
                  contains("sentiment_log_lexicoder_l"),
                  contains("sentiment_log_rauh_l"))

schroeder_final <- merge(schroeder_2005, schroeder_, 
                         by=c('respondent_id', 'time_rtr'), 
                         all.x = T)

# Combine Merkel and Schroeder data and write to disk
duell_2005_for_analysis <- rbind(merkel_final, schroeder_final)
duell_2005_for_analysis <- duell_2005_for_analysis[order(duell_2005_for_analysis$time),]

table(duell_2005_for_analysis$hms_frames)
summary(duell_2005_for_analysis$z_frequency)
summary(duell_2005_for_analysis$z_frequency_l1)

write.csv(duell_2005_for_analysis, "data_for_analysis_2005.csv")


## 2013 ----

# Load data
dat_2013_raw <- readRDS("data_tmp/data_full_2013.rds") %>% ungroup()

dat_2013_raw <- arrange(dat_2013_raw, respondent_id, time_rtr)

# remove text columns

dat_2013_raw <- select(dat_2013_raw, -c(text, text_english))

nrow(dat_2013_raw)

dat_2013 <- unique(dat_2013_raw)

nrow(dat_2013)

# Output debate time master file
master2013 <- distinct(subset(dat_2013, select = c(time_rtr, respondent_id)))

# Generate new emotion variables
dat_2013$emo_noneutral <- 1 - dat_2013$emo_neutral # non-neutral emotion
emo_neg_val_mtx <- subset(dat_2013, select = c(emo_anger, emo_contempt, emo_disgust, emo_fear, emo_sadness))
dat_2013$emo_neg_val <- rowSums(emo_neg_val_mtx)



# (standardize variables (cv_lastname level: facial emotions)
dat_2013 <- dat_2013 %>%
    group_by(cv_lastname) %>%
    mutate(z_emo_noneutral = (emo_noneutral - mean(emo_noneutral)) / sd(emo_noneutral),
           z_emo_neg_val = (emo_neg_val - mean(emo_neg_val)) / sd(emo_neg_val)) %>% 
    ungroup()

nrow(dat_2013)

# standardize variables that require that the person in the image is also speaking
# (we can achieve this by grouping by speaker and cv_lastname and getting standardised values for all possible combinations)

dat_2013 <- dat_2013 %>% 
    group_by(speaker, cv_lastname) %>% # 
    mutate(z_frequency = (frequency - mean(frequency)) / sd(frequency),
           mean_frequency_speaker_cv_lastname_combo = mean(frequency),
           mean_sentiment_lexicoder_speaker_cv_lastname_combo = mean(sentiment_log_lexicoder, na.rm = TRUE),
           z_strength = (strength - mean(strength)) / sd(strength),
           z_sentiment_log_lexicoder2 = (sentiment_log_lexicoder - mean(sentiment_log_lexicoder, na.rm = T)) / sd(sentiment_log_lexicoder, na.rm = T),
           z_sentiment_log_rauh2 = (sentiment_log_rauh - mean(sentiment_log_rauh, na.rm = T)) / sd(sentiment_log_rauh, na.rm = T)) %>% 
    group_by(time_rtr, speaker) %>% 
    arrange(respondent_id, time_rtr)


dat_2013 <- dat_2013 %>% 
    mutate(z_frequency = ifelse(speaker != cv_lastname, NA, z_frequency)) %>% 
    mutate(z_strength = ifelse(speaker != cv_lastname, NA, z_strength)) 

summary(dat_2013$z_frequency)

cor.test(dat_2013$z_sentiment_log_lexicoder,
         dat_2013$z_sentiment_log_lexicoder2)

# Generate lags. Split dataframe by speaker in order to attribute second to
# one of the politicians.

# Variables to lag
vnames = c('z_emo_anger',
           'z_emo_contempt',
           'z_emo_disgust',
           'z_emo_fear',
           'z_emo_happiness',
           'z_emo_neutral',
           'z_emo_sadness',
           'z_emo_surprise',
           'z_emo_noneutral',
           'z_emo_neg_val',
           'z_frequency',
           'z_strength',
           'frequency',
           'strength',
           'z_sentiment_log_lexicoder',
           'z_sentiment_log_rauh')

# Merkel
merkel_2013 <- dat_2013 %>%
    filter(speaker == "Merkel" & cv_lastname == "Merkel")

# Merge to full debate in order to ensure time is handeled correctly
merkel_merged <- merge(master2013, merkel_2013, 
                       by = c('respondent_id','time_rtr'), 
                       all.x = T)

# Generate lags...
merkel_ <- merkel_merged
for (vname in vnames){
    print(paste('Processing', vname))
    merkel_ = generate_lags(merkel_, vname, l = 4)
}


# Merge back to Merkel's 2013 observations to ensure a single
# candidate per second
merkel_$time <- seq.int(nrow(merkel_))
merkel_ <- merkel_ %>%
    dplyr::select(respondent_id, 
                  time_rtr, 
                  contains("anger_l"),
                  contains("contempt_l"),
                  contains("disgust_l"),
                  contains("fear_l"),
                  contains("happiness_l"),
                  contains("neutral_l"),
                  contains("sadness_l"),
                  contains("surprise_l"),
                  contains("noneutral_l"),
                  contains("_val_l"),
                  contains("frequency_l"),
                  contains("strength_l"),
                  contains("sentiment_log_lexicoder_l"),
                  contains("sentiment_log_rauh_l"))

merkel_final <- merge(merkel_2013, merkel_, 
                      by=c('respondent_id', 'time_rtr'),
                      all.x = T)

# Steinbrueck
steinbrueck_2013 <- dat_2013 %>%
    filter(speaker == "Steinbrueck" & cv_lastname == "Steinbrueck")

# Merge to full debate in order to ensure time is handeled correctly
stein_merged <- merge(master2013, steinbrueck_2013, 
                      by = c('respondent_id','time_rtr'), 
                      all.x = T)

# Generate lags...
stein_ <- stein_merged
for (vname in vnames){
    print(paste('Processing', vname))
    stein_ = generate_lags(stein_, vname, l = 4)
}

# Merge back to Steinbrueck's 2013 observations to ensure a single
# candidate per second
stein_$time <- seq.int(nrow(stein_))
stein_ <- stein_ %>%
    dplyr::select(respondent_id, 
                  time_rtr, 
                  contains("anger_l"),
                  contains("contempt_l"),
                  contains("disgust_l"),
                  contains("fear_l"),
                  contains("happiness_l"),
                  contains("neutral_l"),
                  contains("sadness_l"),
                  contains("surprise_l"),
                  contains("noneutral_l"),
                  contains("_val_l"),
                  contains("frequency_l"),
                  contains("strength_l"),
                  contains("sentiment_log_lexicoder_l"),
                  contains("sentiment_log_rauh_l"))

stein_final <- merge(steinbrueck_2013, stein_, 
                     by=c('respondent_id', 'time_rtr'), 
                     all.x = T)

# Combine Merkel and Steinbruck data and write to disk
duell_2013_for_analysis <- rbind(merkel_final, stein_final)
duell_2013_for_analysis <- duell_2013_for_analysis[order(duell_2013_for_analysis$time),]


# remove unnecessary variables 
duell_2013_for_analysis <- select(duell_2013_for_analysis, -c(topic))

summary(duell_2013_for_analysis$z_frequency)
summary(duell_2013_for_analysis$z_frequency_l1)

cor.test(duell_2013_for_analysis$z_sentiment_log_lexicoder,
         duell_2013_for_analysis$z_sentiment_log_lexicoder2)

write.csv(duell_2013_for_analysis, "data_for_analysis_2013.csv")


## 2009 ----

# load data
dat_2009_raw <- readRDS("data_tmp/data_full_2009.rds") %>% ungroup()

dat_2009_raw <- arrange(dat_2009_raw, respondent_id, time_rtr)


# remove text columns

dat_2009_raw <- select(dat_2009_raw, -c(text, text_english))

nrow(dat_2009_raw)

dat_2009 <- unique(dat_2009_raw)

nrow(dat_2009)

# Pre-process
# Note subset to exclude audio only and remove Push participants
dat_2009 <- subset(dat_2009, group != "Audio only")

# output debate time master file
master2009 <- distinct(subset(dat_2009, select = c(time_rtr, respondent_id)))

# generate new emotion variables
dat_2009$emo_noneutral <- 1 - dat_2009$emo_neutral # non-neutral emotion
emo_neg_val_mtx <- subset(dat_2009, select = c(emo_anger, emo_contempt, emo_disgust, emo_fear, emo_sadness))
dat_2009$emo_neg_val <- rowSums(emo_neg_val_mtx)


# (standardize variables (cv_lastname level: facial emotions)
dat_2009 <- dat_2009 %>%
    group_by(cv_lastname) %>%
    mutate(z_emo_noneutral = (emo_noneutral - mean(emo_noneutral)) / sd(emo_noneutral),
           z_emo_neg_val = (emo_neg_val - mean(emo_neg_val)) / sd(emo_neg_val)) %>% 
    ungroup()

# standardize variables that require that the person in the image is also speaking
# (we can achieve this by grouping by speaker and cv_lastname and getting standardised values for all possible combinations)

dat_2009 <- dat_2009 %>% 
    group_by(speaker, cv_lastname) %>% # 
    mutate(z_frequency = (frequency - mean(frequency)) / sd(frequency),
           mean_frequency_speaker_cv_lastname_combo = mean(frequency),
           z_sentiment_log_lexicoder2 = (sentiment_log_lexicoder - mean(sentiment_log_lexicoder, na.rm = T)) / sd(sentiment_log_lexicoder, na.rm = T),
           z_sentiment_log_rauh2 = (sentiment_log_rauh - mean(sentiment_log_rauh, na.rm = T)) / sd(sentiment_log_rauh, na.rm = T),
           mean_sentiment_lexicoder_speaker_cv_lastname_combo = mean(sentiment_log_lexicoder, na.rm = TRUE),
           z_strength = (strength - mean(strength)) / sd(strength)) %>% 
    group_by(time_rtr, speaker) %>% 
    arrange(respondent_id, time_rtr)


dat_2009 <- dat_2009 %>% 
    mutate(z_frequency = ifelse(speaker != cv_lastname, NA, z_frequency)) %>% 
    mutate(z_strength = ifelse(speaker != cv_lastname, NA, z_strength))


summary(dat_2009$z_frequency)

# Generate lags. Split dataframe by speaker in order to attribute second to
# one of the politicians.

# Variables to lag
vnames <- c('z_emo_anger',
            'z_emo_contempt',
            'z_emo_disgust',
            'z_emo_fear',
            'z_emo_happiness',
            'z_emo_neutral',
            'z_emo_sadness',
            'z_emo_surprise',
            'z_emo_noneutral',
            'z_emo_neg_val',
            'z_frequency',
            'z_strength',
            'frequency',
            'strength',
            'z_sentiment_log_lexicoder',
            'z_sentiment_log_rauh')


# Merkel
merkel_2009 <- dat_2009 %>%
    filter(speaker == "Merkel" & cv_lastname == "Merkel")

summary(merkel_2009$z_frequency)

# Merge to full debate in order to ensure time is handeled correctly
merkel_merged <- merge(master2009, merkel_2009, 
                       by = c('respondent_id','time_rtr'), 
                       all.x = T)

# Generate lags...
merkel_ <- merkel_merged
for (vname in vnames){
    print(paste('Processing', vname))
    merkel_ = generate_lags(merkel_, vname, l = 4)
}

# Merge back to Merkel's 2009 observations to ensure a single
# candidate per second
merkel_$time <- seq.int(nrow(merkel_))
merkel_ <- merkel_ %>%
    dplyr::select(respondent_id, 
                  time_rtr, 
                  contains("anger_l"),
                  contains("contempt_l"),
                  contains("disgust_l"),
                  contains("fear_l"),
                  contains("happiness_l"),
                  contains("neutral_l"),
                  contains("sadness_l"),
                  contains("surprise_l"),
                  contains("noneutral_l"),
                  contains("_val_l"),
                  contains("frequency_l"),
                  contains("strength_l"),
                  contains("sentiment_log_lexicoder_l"),
                  contains("sentiment_log_rauh_l"))

merkel_final <- merge(merkel_2009, merkel_, 
                      by=c('respondent_id', 'time_rtr'), 
                      all.x = T)

# Steinmeier
steinmeier_2009 <- dat_2009 %>%
    filter(speaker == "Steinmeier" & cv_lastname == "Steinmeier")

summary(steinmeier_2009$z_frequency)

# Merge to full debate in order to ensure time is handeled correctly
steinmeier_merged <- merge(master2009, steinmeier_2009, 
                           by = c('respondent_id','time_rtr'), 
                           all.x = T)

# Generate lags...
steinmeier_ <- steinmeier_merged
for (vname in vnames){
    print(paste('Processing', vname))
    steinmeier_ = generate_lags(steinmeier_, vname, l = 4)
}


# Merge back to Steinmeier's 2009 observations to ensure a single
# candidate per second
steinmeier_$time <- seq.int(nrow(steinmeier_))
steinmeier_ <- steinmeier_ %>%
    dplyr::select(respondent_id, 
                  time_rtr, 
                  contains("anger_l"),
                  contains("contempt_l"),
                  contains("disgust_l"),
                  contains("fear_l"),
                  contains("happiness_l"),
                  contains("neutral_l"),
                  contains("sadness_l"),
                  contains("surprise_l"),
                  contains("noneutral_l"),
                  contains("_val_l"),
                  contains("frequency_l"),
                  contains("strength_l"),
                  contains("sentiment_log_lexicoder_l"),
                  contains("sentiment_log_rauh_l"))

steinmeier_final <- merge(steinmeier_2009, steinmeier_, 
                          by=c('respondent_id', 'time_rtr'), 
                          all.x = T)

# Combine Merkel and Steinmeier data and write to disk
duell_2009_for_analysis <- rbind(merkel_final, steinmeier_final)
duell_2009_for_analysis <- duell_2009_for_analysis[order(duell_2009_for_analysis$time),]

# remove unnecessary variables

duell_2009_for_analysis <- select(duell_2009_for_analysis, -c(topic, topic_raw))

cor.test(duell_2009_for_analysis$z_sentiment_log_lexicoder,
         duell_2009_for_analysis$z_sentiment_log_lexicoder2)

write.csv(duell_2009_for_analysis, "data_for_analysis_2009.csv")


## 2017 ----

# load data
dat_2017_raw <- readRDS("data_tmp/data_full_2017.rds") %>% ungroup()

dat_2017_raw <- arrange(dat_2017_raw, respondent_id, time_rtr)


# remove text columns

dat_2017_raw <- select(dat_2017_raw, -c(text, text_english))

nrow(dat_2017_raw)

dat_2017 <- unique(dat_2017_raw)

nrow(dat_2017)

# output debate time master file
master2017 <- distinct(subset(dat_2017, select = c(time_rtr, respondent_id)))

# generate new emotion variables
dat_2017$emo_noneutral <- 1 - dat_2017$emo_neutral # non-neutral emotion
emo_neg_val_mtx <- subset(dat_2017, select = c(emo_anger, emo_contempt, emo_disgust, emo_fear, emo_sadness))
dat_2017$emo_neg_val <- rowSums(emo_neg_val_mtx)

# (standardize variables (cv_lastname level: facial emotions)
dat_2017 <- dat_2017 %>%
    group_by(cv_lastname) %>%
    mutate(z_emo_noneutral = (emo_noneutral - mean(emo_noneutral)) / sd(emo_noneutral),
           z_emo_neg_val = (emo_neg_val - mean(emo_neg_val)) / sd(emo_neg_val)) %>% 
    ungroup()

# standardize variables that require that the person in the image is also speaking
# (we can achieve this by grouping by speaker and cv_lastname and getting standardised values for all possible combinations)

dat_2017 <- dat_2017 %>% 
    group_by(speaker, cv_lastname) %>% # 
    mutate(z_frequency = (frequency - mean(frequency)) / sd(frequency),
           mean_frequency_speaker_cv_lastname_combo = mean(frequency),
           mean_sentiment_lexicoder_speaker_cv_lastname_combo = mean(sentiment_log_lexicoder, na.rm = TRUE),
           z_strength = (strength - mean(strength)) / sd(strength),
           z_sentiment_log_lexicoder2 = (sentiment_log_lexicoder - mean(sentiment_log_lexicoder, na.rm = T)) / sd(sentiment_log_lexicoder, na.rm = T),
           z_sentiment_log_rauh2 = (sentiment_log_rauh - mean(sentiment_log_rauh, na.rm = T)) / sd(sentiment_log_rauh, na.rm = T)) %>% 
    group_by(time_rtr, speaker) %>% 
    arrange(respondent_id, time_rtr)


dat_2017 <- dat_2017 %>% 
    mutate(z_frequency = ifelse(speaker != cv_lastname, NA, z_frequency)) %>% 
    mutate(z_strength = ifelse(speaker != cv_lastname, NA, z_strength)) 


summary(dat_2017$z_frequency)


# Generate lags. Split dataframe by speaker in order to attribute second to
# one of the politicians.

# Variables to lag
vnames <- c('z_emo_anger',
            'z_emo_contempt',
            'z_emo_disgust',
            'z_emo_fear',
            'z_emo_happiness',
            'z_emo_neutral',
            'z_emo_sadness',
            'z_emo_surprise',
            'z_emo_noneutral',
            'z_emo_neg_val',
            'z_frequency',
            'z_strength',
            'frequency',
            'strength',
            'z_sentiment_log_lexicoder',
            'z_sentiment_log_rauh')

# Merkel
merkel_2017 <- dat_2017 %>%
    filter(speaker == "Merkel" & cv_lastname == "Merkel")

summary(merkel_2017$z_frequency)

# Merge to full debate in order to ensure time is handeled correctly
merkel_merged <- merge(master2017, merkel_2017, 
                       by = c('respondent_id','time_rtr'), 
                       all.x = T)

# Generate lags...
merkel_ <- merkel_merged
for (vname in vnames){
    print(paste('Processing', vname))
    merkel_ = generate_lags(merkel_, vname, l = 4)
}

# Merge back to Merkel's 2017 observations to ensure a single
# candidate per second
merkel_$time <- seq.int(nrow(merkel_))
merkel_ <- merkel_ %>%
    dplyr::select(respondent_id, 
                  time_rtr, 
                  contains("anger_l"),
                  contains("contempt_l"),
                  contains("disgust_l"),
                  contains("fear_l"),
                  contains("happiness_l"),
                  contains("neutral_l"),
                  contains("sadness_l"),
                  contains("surprise_l"),
                  contains("noneutral_l"),
                  contains("_val_l"),
                  contains("frequency_l"),
                  contains("strength_l"),
                  contains("sentiment_log_lexicoder_l"),
                  contains("sentiment_log_rauh_l"))

merkel_final <- merge(merkel_2017, merkel_, 
                      by=c('respondent_id', 'time_rtr'), 
                      all.x = T)

# Schulz
schulz_2017 <- dat_2017 %>%
    filter(speaker == "Schulz" & cv_lastname == "Schulz")

summary(schulz_2017$z_strength)

# Merge to full debate in order to ensure time is handeled correctly
schulz_merged <- merge(master2017, schulz_2017, 
                       by = c('respondent_id','time_rtr'), 
                       all.x = T)

# Generate lags...
schulz_ <- schulz_merged
for (vname in vnames){
    print(paste('Processing', vname))
    schulz_ = generate_lags(schulz_, vname, l = 4)
}

# Merge back to Schulz's 2017 observations to ensure a single
# candidate per second
schulz_$time <- seq.int(nrow(schulz_))
schulz_ <- schulz_ %>%
    dplyr::select(respondent_id, 
                  time_rtr, 
                  contains("anger_l"),
                  contains("contempt_l"),
                  contains("disgust_l"),
                  contains("fear_l"),
                  contains("happiness_l"),
                  contains("neutral_l"),
                  contains("sadness_l"),
                  contains("surprise_l"),
                  contains("noneutral_l"),
                  contains("_val_l"),
                  contains("frequency_l"),
                  contains("strength_l"),
                  contains("sentiment_log_lexicoder_l"),
                  contains("sentiment_log_rauh_l"))

schulz_final <- merge(schulz_2017, schulz_, 
                      by=c('respondent_id', 'time_rtr'), 
                      all.x = T)

# Combine Merkel and Steinbruck data and write to disk
duell_2017_for_analysis <- rbind(merkel_final, schulz_final)
duell_2017_for_analysis <- duell_2017_for_analysis[order(duell_2017_for_analysis$time),]


summary(duell_2017_for_analysis$z_frequency)

cor.test(duell_2017_for_analysis$z_sentiment_log_lexicoder,
         duell_2017_for_analysis$z_sentiment_log_lexicoder2)

write.csv(duell_2017_for_analysis, "data_for_analysis_2017.csv")


## 2017 (minor parties) ----

# load data
dat_2017_minor_raw <- readRDS("data_tmp/data_full_2017_minor.rds") %>% ungroup()

nrow(dat_2017_minor_raw)

# remove text columns

dat_2017_minor_raw <- select(dat_2017_minor_raw, -c(text, text_english))

dat_2017_minor <- unique(dat_2017_minor_raw)

nrow(dat_2017_minor)

dat_2017_minor <- arrange(dat_2017_minor, respondent_id, time_rtr)

# no need to subset, all respondents on dial system

# output debate time master file
master2017_minor <- distinct(subset(dat_2017_minor, select = c(time_rtr, respondent_id)))

# generate new emotion variables
dat_2017_minor$emo_noneutral <- 1 - dat_2017_minor$emo_neutral # non-neutral emotion
emo_neg_val_mtx <- subset(dat_2017_minor, select = c(emo_anger, emo_contempt, emo_disgust, emo_fear, emo_sadness))
dat_2017_minor$emo_neg_val <- rowSums(emo_neg_val_mtx)



# (standardize variables (cv_lastname level: facial emotions)
dat_2017_minor <- dat_2017_minor %>%
    group_by(cv_lastname) %>%
    mutate(z_emo_noneutral = (emo_noneutral - mean(emo_noneutral)) / sd(emo_noneutral),
           z_emo_neg_val = (emo_neg_val - mean(emo_neg_val)) / sd(emo_neg_val)) %>% 
    ungroup()

# standardize variables that require that the person in the image is also speaking
# (we can achieve this by grouping by speaker and cv_lastname and getting standardised values for all possible combinations)

dat_2017_minor <- dat_2017_minor %>% 
    group_by(speaker, cv_lastname) %>% 
    mutate(z_frequency = (frequency - mean(frequency)) / sd(frequency),
           mean_frequency_speaker_cv_lastname_combo = mean(frequency),
           mean_sentiment_lexicoder_speaker_cv_lastname_combo = mean(sentiment_log_lexicoder, na.rm = TRUE),
           z_strength = (strength - mean(strength)) / sd(strength),
           z_sentiment_log_lexicoder2 = (sentiment_log_lexicoder - mean(sentiment_log_lexicoder, na.rm = T)) / sd(sentiment_log_lexicoder, na.rm = T),
           z_sentiment_log_rauh2 = (sentiment_log_rauh - mean(sentiment_log_rauh, na.rm = T)) / sd(sentiment_log_rauh, na.rm = T)) %>% 
    group_by(time_rtr, speaker) %>% 
    arrange(respondent_id, time_rtr)


dat_2017_minor <- dat_2017_minor %>% 
    mutate(z_frequency = ifelse(speaker != cv_lastname, NA, z_frequency)) %>% 
    mutate(z_strength = ifelse(speaker != cv_lastname, NA, z_strength)) 

summary(dat_2017_minor$z_frequency)


# Generate lags. Split dataframe by speaker in order to attribute second to
# one of the politicians.

table(dat_2017_minor$speaker)

# Variables to lag
vnames <- c('z_emo_anger',
            'z_emo_contempt',
            'z_emo_disgust',
            'z_emo_fear',
            'z_emo_happiness',
            'z_emo_neutral',
            'z_emo_sadness',
            'z_emo_surprise',
            'z_emo_noneutral',
            'z_emo_neg_val',
            'z_frequency',
            'z_strength',
            'frequency',
            'strength',
            'z_sentiment_log_lexicoder',
            'z_sentiment_log_rauh')

# Herrmann

herrmann_2017 <- dat_2017_minor %>%
    filter(speaker == "Herrmann" & cv_lastname == "Herrmann")

# Merge to full debate in order to ensure time is handeled correctly
herrmann_merged <- merge(master2017_minor, herrmann_2017, 
                         by = c('respondent_id','time_rtr'), 
                         all.x = T)

# Generate lags...
herrmann_ <- herrmann_merged
for (vname in vnames){
    print(paste('Processing', vname))
    herrmann_ = generate_lags(herrmann_, vname, l = 4)
}

# Merge back to candidate's observations to ensure a single
# candidate per second
herrmann_$time <- seq.int(nrow(herrmann_))
herrmann_ <- herrmann_ %>%
    dplyr::select(respondent_id, 
                  time_rtr, 
                  contains("anger_l"),
                  contains("contempt_l"),
                  contains("disgust_l"),
                  contains("fear_l"),
                  contains("happiness_l"),
                  contains("neutral_l"),
                  contains("sadness_l"),
                  contains("surprise_l"),
                  contains("noneutral_l"),
                  contains("_val_l"),
                  contains("frequency_l"),
                  contains("strength_l"),
                  contains("sentiment_log_lexicoder_l"),
                  contains("sentiment_log_rauh_l"))

herrmann_final <- merge(herrmann_2017, herrmann_, 
                        by=c('respondent_id', 'time_rtr'), 
                        all.x = T)

# Lindner

lindner_2017 <- dat_2017_minor %>%
    filter(speaker == "Lindner" & cv_lastname == "Lindner")

# Merge to full debate in order to ensure time is handeled correctly
lindner_merged <- merge(master2017_minor, lindner_2017, 
                        by = c('respondent_id','time_rtr'), 
                        all.x = T)

# Generate lags...
lindner_ <- lindner_merged
for (vname in vnames){
    print(paste('Processing', vname))
    lindner_ = generate_lags(lindner_, vname, l = 4)
}

# Merge back to candidate's observations to ensure a single
# candidate per second
lindner_$time <- seq.int(nrow(lindner_))
lindner_ <- lindner_ %>%
    dplyr::select(respondent_id, 
                  time_rtr, 
                  contains("anger_l"),
                  contains("contempt_l"),
                  contains("disgust_l"),
                  contains("fear_l"),
                  contains("happiness_l"),
                  contains("neutral_l"),
                  contains("sadness_l"),
                  contains("surprise_l"),
                  contains("noneutral_l"),
                  contains("_val_l"),
                  contains("frequency_l"),
                  contains("strength_l"),
                  contains("sentiment_log_lexicoder_l"),
                  contains("sentiment_log_rauh_l"))

lindner_final <- merge(lindner_2017, lindner_, 
                       by=c('respondent_id', 'time_rtr'), 
                       all.x = T)

# Ozdemir

ozdemir_2017 <- dat_2017_minor %>%
    filter(speaker == "Ozdemir" & cv_lastname == "Ozdemir")

# Merge to full debate in order to ensure time is handeled correctly
ozdemir_merged <- merge(master2017_minor, ozdemir_2017, 
                        by = c('respondent_id','time_rtr'), 
                        all.x = T)

# Generate lags...
ozdemir_ <- ozdemir_merged
for (vname in vnames){
    print(paste('Processing', vname))
    ozdemir_ = generate_lags(ozdemir_, vname, l = 4)
}

# Merge back to candidate's observations to ensure a single
# candidate per second
ozdemir_$time <- seq.int(nrow(ozdemir_))
ozdemir_ <- ozdemir_ %>%
    dplyr::select(respondent_id, 
                  time_rtr, 
                  contains("anger_l"),
                  contains("contempt_l"),
                  contains("disgust_l"),
                  contains("fear_l"),
                  contains("happiness_l"),
                  contains("neutral_l"),
                  contains("sadness_l"),
                  contains("surprise_l"),
                  contains("noneutral_l"),
                  contains("_val_l"),
                  contains("frequency_l"),
                  contains("strength_l"),
                  contains("sentiment_log_lexicoder_l"),
                  contains("sentiment_log_rauh_l"))

ozdemir_final <- merge(ozdemir_2017, ozdemir_, 
                       by=c('respondent_id', 'time_rtr'), 
                       all.x = T)

# Wagenknecht

wagenknecht_2017 <- dat_2017_minor %>%
    filter(speaker == "Wagenknecht" & cv_lastname == "Wagenknecht")

# Merge to full debate in order to ensure time is handeled correctly
wagenknecht_merged <- merge(master2017_minor, wagenknecht_2017, 
                            by = c('respondent_id','time_rtr'), 
                            all.x = T)

# Generate lags...
wagenknecht_ <- wagenknecht_merged
for (vname in vnames){
    print(paste('Processing', vname))
    wagenknecht_ = generate_lags(wagenknecht_, vname, l = 4)
}

# Merge back to candidate's observations to ensure a single
# candidate per second
wagenknecht_$time <- seq.int(nrow(wagenknecht_))
wagenknecht_ <- wagenknecht_ %>%
    dplyr::select(respondent_id, 
                  time_rtr, 
                  contains("anger_l"),
                  contains("contempt_l"),
                  contains("disgust_l"),
                  contains("fear_l"),
                  contains("happiness_l"),
                  contains("neutral_l"),
                  contains("sadness_l"),
                  contains("surprise_l"),
                  contains("noneutral_l"),
                  contains("_val_l"),
                  contains("frequency_l"),
                  contains("strength_l"),
                  contains("sentiment_log_lexicoder_l"),
                  contains("sentiment_log_rauh_l"))
wagenknecht_final <- merge(wagenknecht_2017, wagenknecht_, 
                           by=c('respondent_id', 'time_rtr'), 
                           all.x = T)

# Weidel

weidel_2017 <- dat_2017_minor %>%
    filter(speaker == "Weidel" & cv_lastname == "Weidel")

# Merge to full debate in order to ensure time is handeled correctly
weidel_merged <- merge(master2017_minor, weidel_2017, 
                       by = c('respondent_id','time_rtr'), 
                       all.x = T)

# Generate lags...
weidel_ <- weidel_merged
for (vname in vnames){
    print(paste('Processing', vname))
    weidel_ = generate_lags(weidel_, vname, l = 4)
}

# Merge back to candidate's observations to ensure a single
# candidate per second
weidel_$time <- seq.int(nrow(weidel_))
weidel_ <- weidel_ %>%
    dplyr::select(respondent_id, 
                  time_rtr, 
                  contains("anger_l"),
                  contains("contempt_l"),
                  contains("disgust_l"),
                  contains("fear_l"),
                  contains("happiness_l"),
                  contains("neutral_l"),
                  contains("sadness_l"),
                  contains("surprise_l"),
                  contains("noneutral_l"),
                  contains("_val_l"),
                  contains("frequency_l"),
                  contains("strength_l"),
                  contains("sentiment_log_lexicoder_l"),
                  contains("sentiment_log_rauh_l"))

weidel_final <- merge(weidel_2017, weidel_, 
                      by=c('respondent_id', 'time_rtr'), 
                      all.x = T)

summary(weidel_final$z_sentiment_log_lexicoder)


# Combine data and write to disk
duell_2017_minor_for_analysis <- rbind(herrmann_final, lindner_final, ozdemir_final, wagenknecht_final, weidel_final)
duell_2017_minor_for_analysis <- duell_2017_minor_for_analysis[order(duell_2017_minor_for_analysis$time),]

# remove unnecessary variables

duell_2017_minor_for_analysis <- select(duell_2017_minor_for_analysis,
                                        -c(topic, topic_raw))

summary(duell_2017_minor_for_analysis$z_frequency)

cor.test(duell_2017_minor_for_analysis$z_sentiment_log_lexicoder,
         duell_2017_minor_for_analysis$z_sentiment_log_lexicoder2)

write.csv(duell_2017_minor_for_analysis, "data_for_analysis_2017_minor.csv")
