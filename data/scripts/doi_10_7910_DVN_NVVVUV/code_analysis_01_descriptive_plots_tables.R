############################################################################
############################################################################
#### 
#### Constantine Boussalis, Travis G. Coan, Mirya R. Holman, Stefan Müller
#### Gender, Candidate Emotional Expression,
#### and Voter Reactions During Televised Debates 
####
#### American Political Science Review 
####
#### code_analysis_01_descriptive_plots_tables.R
#### Note: this code reproduces descriptive plots and tables 
#### reported in the main paper and appendix. 
#### For details on all datasets, R scripts, and instructions 
#### please consult the file "0000_replication_instructions.pdf" in the 
#### Dataverse of this project.
############################################################################
############################################################################


# load required packages
library(quanteda)    # CRAN v3.0.0
library(dplyr)       # CRAN v1.0.5
library(stringr)     # CRAN v1.4.0
library(texreg)      # CRAN v1.37.5
library(vroom)       # CRAN v1.4.0
library(ggplot2)     # CRAN v3.3.3
library(GGally)      # CRAN v2.1.0
library(cowplot)     # CRAN v1.1.1
library(scales)      # CRAN v1.1.1
library(ggbeeswarm)  # CRAN v0.6.0
library(countrycode) # CRAN v1.2.0
library(maps)        # CRAN v3.3.0
library(forcats)     # CRAN v0.5.1
library(tidyr)       # CRAN v1.1.3
library(Hmisc)       # CRAN v4.4-2


# set working directory 
setwd("")

# get custom ggplot2 scheme
source("code_function_theme_base.R")

# Figure 01 ----

# Figure 01 is a flowchart with the theoretical expectations and does not 
# require any replication code


# Figure 02 ----

# load dataset with candidate-second as unit of analysis 
# (used in regression models with candidate emotions as dependent variable)
dat_candidatelevel <- read.csv("data_candidate_models.csv")


# get only observations when politicians are speaking (remove moderators)
dat_politicians <- dat_candidatelevel %>% 
    filter(speaker %in% c("Merkel",
                          "Steinbrueck",
                          "Steinmeier",
                          "Schulz",
                          "Schroeder",
                          "Herrmann",
                          "Lindner",
                          "Wagenknecht",
                          "Weidel",
                          "Ozdemir"))

# get average confidence scores for each speaker
dat_emotions_long <- dat_candidatelevel %>% 
    mutate(cv_lastname = dplyr::recode(cv_lastname, "Schroeder" = "Schröder", 
                                       "Steinbrueck" = "Steinbrück",
                                       "Ozdemir" = "Özdemir")) %>% # recode speakers
    select(cv_lastname, debate_name, emo_anger, emo_happiness) %>% 
    gather(emotion, value, - c(debate_name, cv_lastname)) # change data to "long" format


# group by debate, speaker, and emotional expression and get the average scores
dat_emotions_sum <- dat_emotions_long %>% 
    group_by(debate_name, cv_lastname, emotion) %>%
    summarise(mean_emo_sec = mean(value)) %>% 
    filter(cv_lastname %in% c("Merkel",
                              "Schröder",
                              "Steinmeier",
                              "Steinbrück",
                              "Schulz",
                              "Lindner",
                              "Özdemir",
                              "Herrmann",
                              "Weidel",
                              "Wagenknecht"))


# clean up labels
dat_emotions_sum <- dat_emotions_sum %>% 
    mutate(emotion = str_replace_all(emotion, "emo_", "")) %>% 
    mutate(emotion = str_to_title(emotion)) # transform emotion to Title Case


                     # create better labels for plot
dat_emotions_sum <- dat_emotions_sum %>% 
    mutate(year_speaker = paste(debate_name, cv_lastname, sep = ": ")) %>% 
    mutate(emotion_speaker = paste0(emotion, " (", cv_lastname, debate_name, ")"))


table(dat_emotions_sum$year_speaker)

# remove minor debate and add a dummy for Angela Merkel
dat_emotions_sum_merkeldebate <- dat_emotions_sum %>% 
    filter(debate_name != "2017 (minor)") %>% 
    mutate(merkel_dummy = ifelse(cv_lastname == "Merkel", TRUE, FALSE))

# relevel speakers
levels_speakers <- c("2005: Merkel",
                     "2009: Merkel",
                     "2013: Merkel",
                     "2017: Merkel",
                     "2005: Schröder",
                     "2009: Steinmeier",
                     "2013: Steinbrück",
                     "2017: Schulz")

dat_emotions_sum_merkeldebate$year_speaker <- factor(dat_emotions_sum_merkeldebate$year_speaker,
                                                     levels = levels_speakers)


                     # create and save Figure 2 ----
ggplot(dat_emotions_sum_merkeldebate,
       aes(x = forcats::fct_relevel(emotion), y = mean_emo_sec,
           colour = merkel_dummy,
           fill = merkel_dummy)) +
    geom_bar(stat = "identity", width = 0.1) +
    geom_point(aes(shape = merkel_dummy), size = 4) +
    scale_shape_manual(values = c(15, 16)) +
    scale_y_continuous(limits = c(0, 0.13)) +
    facet_wrap(~year_speaker, nrow = 2) +
    scale_colour_manual(values = c("darkred", "black")) +
    scale_fill_manual(values = c("darkred", "black")) +
    coord_flip() +
    labs(x = NULL, y = "Average Confidence Score") +
    theme(legend.position = "none",
          strip.text = element_text(size = 14, face = "bold"))
ggsave("fig_02.pdf",
       width = 10, height = 3)


# Descriptive figures and tables for appendix -----


## Figure A01 ----

# plot the prevalence of televised debates across the world

# televised debates by country (accessed 6 March 2021)

# https://aceproject.org/epic-en/CDTable?view=country&question=ME059
dat_debates <- read.csv("data_debates_worldwide.csv")


dat_debates <- dat_debates %>% 
    dplyr::rename(country = Country.Territory,
                  info_debate = Answers)

dat_debates_valid <- filter(dat_debates, country != "")

table(dat_debates_valid$info_debate)

# recode Malta
# https://newsbook.com.mt/en/40-years-ago-maltas-first-televised-political-debate/

# recode Australia
# https://link.springer.com/chapter/10.1057%2F9780230379602_3
# https://www.theguardian.com/australia-news/2019/apr/29/beyond-terrible-televised-leaders-debate-serves-up-a-winner-and-a-grinner
dat_debates_valid <- dat_debates_valid %>% 
    mutate(info_debate = ifelse(country == "Malta", "Yes", info_debate)) %>% 
    mutate(info_debate = ifelse(country == "Australia", "Yes", info_debate))


# recode the information on debate into two categories
dat_debates_valid <- dat_debates_valid %>% 
    mutate(debate_factor = ifelse(str_detect(info_debate, "Yes"),
                                  "Televised Debates (Presidential and/or Legislative Elections)",
                                  "No Televised Debates/No Information"))

table(dat_debates_valid$debate_factor)

# use country code package to prepare data for plot
dat_debates_valid$id <- countrycode::countrycode(dat_debates_valid$country,
                                                 origin = 'country.name.en', destination = 'iso2c')



dat_debates_valid$continent <- countrycode::countrycode(dat_debates_valid$country,
                                                        origin = 'country.name.en', destination = 'continent')


dat_debates_valid$debate_factor <- factor(dat_debates_valid$debate_factor,
                                          levels = c("Televised Debates (Presidential and/or Legislative Elections)",
                                                     "No Televised Debates/No Information"))


dat_debates_valid <- dat_debates_valid %>% 
    mutate(debate_dummy = ifelse(str_detect(debate_factor, "No"), 0, 1)) 

table(dat_debates_valid$debate_dummy)

# summarize prevalence of debates per continent

dat_continent <- dat_debates_valid %>% 
    filter(!is.na(continent)) %>% 
    group_by(continent) %>% 
    summarise(prop_debates = sum(debate_dummy) / n(),
              n_countries = n()) %>% 
    arrange(-prop_debates)

dat_continent

# prepare map
world_map <- map_data(map = "world")
world_map$region <- iso.alpha(world_map$region) # convert country name to ISO code

ggplot(dat_debates_valid, aes(map_id = id)) +
    geom_map(aes(fill = debate_factor), color = "grey50", size = 0.2,
             map = world_map) +
    expand_limits(x = world_map$long, y = world_map$lat) +
    scale_fill_manual(values = c("grey20", "grey95"), name = NULL) +
    theme_void() +
    coord_fixed() +
    theme(legend.position = "bottom",
          legend.key.width = unit(0.5, "cm"))
ggsave("fig_a01.pdf", width = 6.2, height = 4)



## Figure A02 ----

# Figure A02 is a screenshot from the annotation tool and does not 
# require any code


## Figure A06 ----

# subset the 2005 debate and validate the automated coding of happiness
# with the human coding of smiles
dat_05_validate <- dat_candidatelevel %>% 
    filter(debate_name == "2005") %>% 
    filter(speaker %in% c("Merkel", "Schroeder")) %>% 
    filter(speaker == cv_lastname)

# for 36 seconds, code_smile is NA - remove these observations
table(dat_05_validate$code_smile, useNA = "always")

dat_05_validate <- filter(dat_05_validate, !is.na(code_smile))

# nicer label for Schröder
dat_05_validate$speaker <- str_replace_all(dat_05_validate$speaker, "oe", "ö")

# relevel factor
dat_05_validate$code_smile <- factor(dat_05_validate$code_smile,
                                     levels = c("Strong smile",
                                                "Light smile",
                                                "No smile"))

# get the averages of standardized happiness for both speakers
# across the three levels for the "smile" variable
dat_05_subset_sum <- dat_05_validate %>% 
    group_by(speaker, code_smile) %>% 
    summarise(mean = mean(z_emo_happiness),
              se = sd(z_emo_happiness) / sqrt(n()),
              ci_lower_95 = mean - 1.96 * se,
              ci_upper_95 = mean + 1.96 * se,
              ci_lower_90 = mean - 1.645 * se,
              ci_upper_90 = mean + 1.645 * se)
dat_05_subset_sum


#                    # create plot and save as Figure A06
ggplot(dat_05_validate, aes(x = code_smile, y = z_emo_happiness)) +
    geom_boxplot(fill = "grey80", outlier.colour = "grey60", outlier.size = 0.7,
                 alpha = 0.3, width = 0.5) +
    facet_wrap(~speaker) +
    labs(x = "Human Coding of Facial Expression",
         y = "Automated Detection\nof Happiness")
ggsave("fig_a06.pdf",
       width = 10, height = 4)



## Figure A07 ----

# estimate correlation between the two sentiment dictionaries 
# for each debate


dat_analysis <- dat_politicians %>% 
    select(debate_name, statement_number, utterance_id,
           speaker, cv_lastname,situation_pos_neg, sentiment_log_lexicoder,
           sentiment_log_rauh, text_english) %>% 
    unique() %>% 
    filter(!cv_lastname %in% c("unknown", "na")) %>% 
    filter(!is.na(text_english)) %>% 
    filter(speaker == cv_lastname) %>% 
    filter(speaker %in% c("Herrmann", "Lindner", "Schroeder",
                          "Merkel", "Ozdemir",
                          "Schulz", "Steinbrueck",
                          "Steinmeier", "Wagenknecht",
                          "Weidel"))

table(dat_analysis$speaker)


# give better labels for names
dat_analysis <- dat_analysis %>% 
    mutate(speaker_clean = dplyr::recode(speaker, "Ozdemir" = "Özdemir",
                                         "Steinbrueck" =  "Steinbrück",
                                         "Schroeder" = "Schröder")) %>% 
    mutate(speaker_gender = ifelse(speaker %in% 
                                       c("Merkel", "Wagenknecht", "Weidel"),
                                   "Female", "Male")) %>% 
    mutate(speaker_clean = paste0(speaker_gender, ": ", speaker_clean))

table(dat_analysis$speaker_clean)

# rename variables and filter missing observations
dat_sentiment_cor <- dat_analysis %>% 
    dplyr::rename(Rauh = sentiment_log_rauh,
                  Lexicoder = sentiment_log_lexicoder) %>% 
    filter(!is.na(Rauh)) %>% 
    filter(!is.na(Lexicoder))

table(dat_analysis$speaker)

# compare sentiment scores based on Lexicoder and Rauh's dictionary
# for statements by male and female politicians

dat_sentiment_cor_sum_gender <- dat_sentiment_cor %>% 
    group_by(speaker_gender) %>% 
    summarise(cor = cor(Rauh, Lexicoder))

# scatterplot (add random noise to points to avoid overplotting)
# along with the correlation coefficient and a smoothed line
# (Figure A07) 
set.seed(235)
ggplot(data = dat_sentiment_cor, aes(x = Lexicoder, y = Rauh)) +
    geom_jitter(alpha = 0.2, width = 0.5, height = 0.5) +
    facet_grid(~speaker_gender) +
    geom_smooth() +
    geom_text(data = dat_sentiment_cor_sum_gender, 
              aes(label = paste0("r=", round(cor, 2))),
              x = -2, y = 2.5,
              size = 4.5,
              colour = "black")
ggsave("fig_a07.pdf",
       width = 9, height = 4)


## Figure A08 ----

# now create a "long" dataset 
# and get the mean and confidence intervals 
# for the three classes regarding the "societal situation"
# for each debate to assess whether a more positive depiction of the societal 
# situation corresponds to more positive textual sentiment

table(dat_analysis$speaker)

dat_sentiment_long_sum <- dat_analysis %>% 
    filter(debate_name != "2005") %>% # 2005 does not contain the situation_pos_neg variable
    select(sentiment_log_lexicoder, sentiment_log_rauh,
           debate_name, situation_pos_neg) %>% 
    gather(sent_dict, sentiment, 
           -c(debate_name, situation_pos_neg)) %>% 
    group_by(debate_name, situation_pos_neg, sent_dict) %>% 
    summarise(mean = mean(sentiment),
              n = n(),
              se = sd(sentiment) / sqrt(n()),
              ci_lower_90 = mean - 1.645 * se,
              ci_upper_90 = mean + 1.645 * se,
              ci_lower_95 = mean - 1.96 * se,
              ci_upper_95 = mean + 1.96 * se) %>% 
    mutate(sent_dict = ifelse(str_detect(sent_dict, "lexicoder"), "Lexicoder",
                              "Rauh"))


# plot the means and confidence intervals
ggplot(dat_sentiment_long_sum, 
       aes(x = situation_pos_neg, y = mean,
           colour = situation_pos_neg)) +
    geom_point(size = 3) +
    geom_linerange(aes(ymin = ci_lower_95,
                       ymax = ci_upper_95),
                   size = 0.5) +
    geom_linerange(aes(ymin = ci_lower_90,
                       ymax = ci_upper_90),
                   size = 1.3)  +   
    geom_text(aes(label = n), colour = "black", size = 4,
              nudge_x = 0.35) +
    scale_colour_manual(values = c("darkred", "grey30", "darkgreen")) +
    facet_grid(sent_dict~debate_name) +
    labs(x = "Description of the Social Situation",
         y = "Estimated Sentiment") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
ggsave("fig_a08.pdf",
       width = 9, height = 7)



## Figure A09 ---- 

# prepare data for correlation plot

# select only Merkeldebates
dat_analysis_merkeldebates <- dat_politicians %>% 
    filter(debate_name != "2017 (minor)") %>%
    mutate(merkel_dummy = ifelse(speaker == "Merkel", "Merkel", "Male Competitors")) %>% 
    filter(speaker == cv_lastname) %>%  # only keep observations where person is speaking (required for sentiment and pitch)
    filter(speaker %in% c("Merkel", "Steinbrueck", "Steinmeier", "Schulz",
                          "Schroeder"))

table(dat_analysis_merkeldebates$speaker)


# get averages of emotions in each statement

dat_all_statement_cor <- dat_analysis_merkeldebates %>% 
    mutate(emo_nonneutral = 1 - emo_neutral) %>% 
    group_by(debate_name, merkel_dummy, statement_number) %>% 
    summarise(`Non-Neutral` = mean(emo_nonneutral, na.rm = TRUE),
              Anger = mean(emo_anger, na.rm = TRUE),
              Happiness = mean(emo_happiness, na.rm = TRUE),
              Sentiment = mean(sentiment_log_lexicoder, na.rm = TRUE),
              `Frequency (Pitch)` = mean(frequency, na.rm = TRUE))


# remove missing observations
dat_statement_cor <- dat_all_statement_cor %>% 
    ungroup() %>% 
    select(-c(debate_name, statement_number)) %>% 
    na.omit()

table(dat_statement_cor$merkel_dummy)

# relevel factor
dat_statement_cor$merkel_dummy <- factor(dat_statement_cor$merkel_dummy,
                                         levels = c("Merkel", "Male Competitors"))


                     # create and save Figure A09
GGally::ggpairs(data = dat_statement_cor, columns = 2:6,
                mapping = ggplot2::aes(colour=merkel_dummy, alpha = 0.1,
                                       fill = merkel_dummy)) +
    scale_colour_manual(values = c("grey50", "darkred")) +
    scale_fill_manual(values = c("grey50", "darkred")) +
    theme(strip.text.y = element_text(size = 15, face = "bold"),
          axis.text = element_text(size = 10))
ggsave("fig_a09.pdf",
       width = 11, height = 11)



## Figure A10 ----

## get average scores and confidence intervals of relevant emotions, 
## separately for each topic

## get binary variables whether pitch is 1 or 1.5 standard deviations
## above mean
dat_candidatelevel <- dat_candidatelevel %>% 
    mutate(high_zff1 = ifelse(z_frequency > 1, 1, 0)) %>% 
    mutate(high_zff15 = ifelse(z_frequency > 1.5, 1, 0)) 


dat_raw_speaker <- dat_candidatelevel %>% 
    filter(cv_lastname %in% c("Merkel", "Schroeder",
                              "Steinmeier", "Steinbrueck",
                              "Schulz", "Lindner",
                              "Weidel", "Wagenknecht",
                              "Herrmann", "Ozdemir"))




# select only speakers participating in main debates
# and select only relevant variabes
speakers <- c("Merkel", "Schroeder",
              "Steinbrueck", "Steinmeier",
              "Schulz")


dat_select_means <- dat_candidatelevel %>% 
    select(debate_name, speaker, cv_lastname,
           topic_broad_policy_area, 
           gender_policy_area_broad,
           `Anger` = z_emo_anger,
           statement_number, debate_name,
           `Happiness` = z_emo_happiness,
           `Non-neutral Emotions` = z_emo_nonneutral,
           `Sentiment` = z_sentiment_log_lexicoder,
           `Frequency` = z_frequency)



dat_long_sentiment <- dat_select_means %>% 
    select(statement_number, debate_name, 
           gender_policy_area_broad,
           topic_broad_policy_area,speaker, cv_lastname,
           Sentiment) %>% 
    filter(!is.na(Sentiment)) %>% 
    filter(speaker %in% speakers) %>% 
    filter(speaker == cv_lastname) %>% 
    unique() %>% 
    select(-statement_number) %>% 
    gather(var, value, -c(speaker, debate_name, gender_policy_area_broad,
                          cv_lastname, topic_broad_policy_area)) 


dat_long_pitch <- dat_select_means %>% 
    filter(speaker == cv_lastname) %>% 
    select(debate_name, gender_policy_area_broad,
           topic_broad_policy_area,speaker, cv_lastname,
           speaker, 
           Frequency) %>% 
    gather(var, value, -c(speaker, debate_name, 
                          gender_policy_area_broad,
                          cv_lastname, topic_broad_policy_area)) 


dat_long_emo <- dat_select_means %>% 
    select(debate_name, 
           topic_broad_policy_area,speaker, cv_lastname,
           gender_policy_area_broad,
           speaker, Happiness, `Non-neutral Emotions`,
           Anger) %>% 
    filter(cv_lastname %in% speakers) %>% 
    gather(var, value, -c(speaker, debate_name, 
                          gender_policy_area_broad,
                          cv_lastname, 
                          topic_broad_policy_area)) 

# combine dataframes for plot
dat_select_combined <- bind_rows(
    dat_long_sentiment,
    dat_long_emo,
    dat_long_pitch
)

table(dat_select_combined$cv_lastname)

table(dat_select_combined$var)

# get averages and confidence intervals 
# for all emotions, conditional on topic and gendered nature of topic
dat_long_sum <- dat_select_combined %>% 
    filter(cv_lastname %in% speakers) %>% 
    group_by(var, gender_policy_area_broad, topic_broad_policy_area) %>% 
    summarise(mean = mean(value, na.rm = TRUE),
              se = sd(value, na.rm = TRUE) / sqrt(n()),
              ci_lower_95 = mean - 1.96 * se,
              ci_upper_95 = mean + 1.96 * se,
              ci_lower_90 = mean - 1.645 * se,
              ci_upper_90 = mean + 1.645 * se)


# arrange for plotting
dat_long_sum <- dplyr::arrange(dat_long_sum,
                               var, -mean)

# recode topic
dat_long_sum <- dat_long_sum %>% 
    mutate(topic_broad_policy_area = 
               ifelse(str_detect(topic_broad_policy_area, "Government Affairs"),
                      "Government Affairs", topic_broad_policy_area))


table(dat_long_sum$var)

# determine order of facets
dat_long_sum$var <- factor(dat_long_sum$var,
                           levels = c("Anger", 
                                      "Happiness",
                                      "Non-neutral Emotions",
                                      "Sentiment",
                                      "Frequency"))


                     # create and save Figure A10
ggplot(dat_long_sum, 
       aes(x = factor(nrow(dat_long_sum):1),
           y = mean,
           colour = gender_policy_area_broad,
           shape = gender_policy_area_broad)) +
    geom_point(size = 3) +
    geom_linerange(aes(ymin = ci_lower_95,
                       ymax = ci_upper_95),
                   size = 0.5) +
    geom_linerange(aes(ymin = ci_lower_90,
                       ymax = ci_upper_90),
                   size = 1.3)  + 
    coord_flip() +
    scale_colour_grey(start = 0.1, end = 0.7,
                      name = "Topic Classification") +
    facet_wrap(~var, nrow = 3, scales = "free") +
    scale_shape_manual(values = c(16, 1, 15, 17),
                       name = "Topic Classification") +
    scale_x_discrete(breaks = nrow(dat_long_sum):1,
                     labels = dat_long_sum$topic_broad_policy_area) +
    labs(y = "Score (standardized by speaker and debate)",
         x = NULL) +
    theme(legend.title = element_text(vjust = 0.5),
          legend.position = c(0.8, 0.15))
ggsave("fig_a10.pdf",
       width = 10, height = 12)




## Figure A11 ----

# Compare respondents from RTR samples with respondents in 
# representative election studies
# This file was originally created in code_prepare_04_harmonise_election_studies.R
# but because the data wrangling required files which can only be downloaded after a free 
# registration at GESIS, we cannot provide the raw surveys in the Dataverse
# code_prepare_04_harmonise_election_studies.R contains the DOIs of all datasets

# load data
dat_compare_respondents <- read.csv("data_compare_respondents.csv")

                     # create plot and save as Figure A11
ggplot(dat_compare_respondents, aes(x = freq, y = forcats::fct_rev(value), fill = type)) +
    geom_bar(stat = "identity", position = position_dodge(preserve = "single"),
             width = 0.5) +
    facet_wrap(~year_var, ncol = 4, scales = "free_y") +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = "Percentage", y = NULL) +
    scale_fill_manual(values = c("grey70", "black"), 
                      guide = guide_legend(reverse = TRUE)) +
    theme(legend.title = element_blank(),
          axis.text = element_text(size = 12),
          strip.text.x = element_text(size = 12, face = "bold"))
ggsave("fig_a11.pdf",
       width = 13, height = 10)


## Figure A12 ----

## get standard deviation of dial values and number of changes of dial values per minute

# load data for analysis
dat_05_for_dial <- vroom("data_for_analysis_2005.csv") %>% 
    select(respondent_id, value_rtr, utterance_id,
           hms = hms_frames, speaker) %>% 
    unique() %>% 
    mutate(year = "2005")

dat_09_for_dial <- vroom("data_for_analysis_2009.csv") %>% 
    select(respondent_id, value_rtr, utterance_id,
           hms =  hms_merge, speaker) %>% 
    unique() %>% 
    mutate(year = "2009")

dat_13_for_dial <- vroom("data_for_analysis_2013.csv") %>% 
    select(respondent_id, value_rtr, utterance_id,
           hms = hms_merge, speaker) %>% 
    unique() %>% 
    mutate(year = "2013")

dat_17_for_dial <- vroom("data_for_analysis_2017.csv") %>% 
    select(respondent_id, value_rtr, utterance_id,
           hms = time_rtr, speaker, group) %>% 
    unique() %>% 
    mutate(year = "2017")


dat_17_minor_for_dial <- vroom("data_for_analysis_2017_minor.csv") %>% 
    select(respondent_id, value_rtr, hms = time_rtr, speaker) %>% 
    unique() %>% 
    mutate(year = "2017\n(minor parties)")


# change respondent ID and hms to character 
dat_05_for_dial$respondent_id <- as.factor(dat_05_for_dial$respondent_id)
dat_09_for_dial$respondent_id <- as.factor(dat_09_for_dial$respondent_id)
dat_13_for_dial$respondent_id <- as.factor(dat_13_for_dial$respondent_id)
dat_17_for_dial$respondent_id <- as.factor(dat_17_for_dial$respondent_id)
dat_17_minor_for_dial$respondent_id <- as.factor(dat_17_minor_for_dial$respondent_id)


dat_05_for_dial$hms <- as.character(dat_05_for_dial$hms)
dat_09_for_dial$hms <- as.character(dat_09_for_dial$hms)
dat_13_for_dial$hms <- as.character(dat_13_for_dial$hms)
dat_17_for_dial$hms <- as.character(dat_17_for_dial$hms)
dat_17_minor_for_dial$hms <- as.character(dat_17_minor_for_dial$hms)

# combine data
dat_combined_for_dial <- bind_rows(
    dat_05_for_dial,
    dat_09_for_dial,
    dat_13_for_dial,
    dat_17_for_dial,
    dat_17_minor_for_dial
)


# get standard deviation of dial changes on debate-respondent level
dat_sum_sd_dial <- dat_combined_for_dial %>% 
    ungroup() %>% 
    group_by(year, respondent_id) %>% 
    summarise(sd_rtr = sd(value_rtr, na.rm = TRUE))


p_sd <- ggplot(dat_sum_sd_dial, aes(x = forcats::fct_rev(factor(year)), y = sd_rtr)) + 
    geom_boxplot(outlier.colour = "white", colour = "red") +
    ggbeeswarm::geom_quasirandom(alpha = 0.2, size = 1) +
    coord_flip() +
    labs(x = NULL,
         title = "(a) Standard Deviation of RTR Dial Values",
         y = "Standard Deviation (7-Point Scale)") +
    theme(plot.title = element_text(face = "bold", 
                                    hjust = 0.5,
                                    size = 15,
                                    margin=margin(10,0,15,0)))


# group by utterance_id to avoid that lagged change are caused by missing seconds 
# (=moderator speaking which is excluded in this dataset)
# we are looking at changes when one person is speaking
# note: results virtually the same when you exclude utterance_id from the code below
dat_sum_changes_gles <- dat_combined_for_dial %>%
    filter(year != "2005") %>% 
    arrange(year, respondent_id, hms) %>% 
    ungroup() %>% 
    group_by(year, respondent_id, utterance_id) %>% 
    mutate(value_rtr_lag = lag(value_rtr)) %>% 
    mutate(change_rtr = ifelse(value_rtr != value_rtr_lag, 1, 0))


# check the degree of dial changes
dat_change <- dat_sum_changes_gles %>% 
    mutate(diff_rtr = abs(value_rtr_lag - value_rtr)) %>%
    filter(diff_rtr != 0) 

round(prop.table(table(dat_change$diff_rtr)), 3) * 100
# in 83.3 percent of cases, the dial is changed by only one value

# different recording of RTR in 2005 -- therefore we need to code changes differently
dat_sum_changes_2005 <- dat_combined_for_dial %>%
    filter(year == "2005") %>% 
    arrange(year, respondent_id, hms) %>% 
    ungroup() %>% 
    group_by(year, respondent_id, utterance_id) %>% 
    mutate(value_rtr_lag = lag(value_rtr)) %>% 
    mutate(diff_rtr = abs(value_rtr_lag - value_rtr)) %>% # check absolute difference between two values
    mutate(change_rtr = ifelse(diff_rtr >= 1, 1, 0))

# bind both dataframes
dat_sum_changes <- bind_rows(dat_sum_changes_2005,
                             dat_sum_changes_gles)


# aggregate dial changes on the level of respondents
dat_changes_respondent <- dat_sum_changes %>% 
    group_by(year, respondent_id) %>% 
    summarise(sum_change_rtr = sum(change_rtr, na.rm = TRUE),
              n_seconds = n(),
              change_rtr_second = sum_change_rtr / n_seconds, # dial changes per second
              change_rtr_minute = change_rtr_second * 60) # dial changes per minute



# get average changes per minute in RTR value
compare_changes <- dat_changes_respondent %>% 
    group_by(year) %>% 
    summarise(change_rtr_minute_mean = mean(change_rtr_minute, na.rm = TRUE),
              change_rtr_minute_median = median(change_rtr_minute, na.rm = TRUE)) %>% 
    mutate(changes_every_x_seconds_mean = 60 / change_rtr_minute_mean,
           changes_every_x_seconds_median = 60 / change_rtr_minute_median)

compare_changes

                     # create plot of dial movements
p_changes <- ggplot(dat_changes_respondent, aes(x = forcats::fct_rev(factor(year)), 
                                                y = change_rtr_minute)) + 
    geom_boxplot(outlier.colour = "white", colour = "red") +
    ggbeeswarm::geom_quasirandom(alpha = 0.2, size = 1) +
    scale_y_continuous(breaks = c(seq(0, 25, 5))) +
    coord_flip() +
    labs(x = NULL,
         title = "(b) Movements of RTR Dial Per Minute",
         y = "Count of Movements") +
    theme(plot.title = element_text(face = "bold", 
                                    hjust = 0.5,
                                    size = 15,
                                    margin=margin(10,0,15,0)))

                     # create plot
cowplot::plot_grid(p_sd, p_changes)
ggsave("fig_a12.pdf",
       width = 10.5, height = 5)


## Figure A13 ----

# get data frame of debates involving Angela Merkel
# and create dummy variables testing whether the standardized frequency 
# is 1 or 1.5 standard deviations above the mean

dat_politicians_merkeldebates <- dat_candidatelevel %>% 
    filter(debate_name != "2017 (minor)") %>% 
    mutate(high_zff1 = ifelse(z_frequency > 1, 1, 0),
           high_zff15 = ifelse(z_frequency > 1.5, 1, 0))

table(dat_politicians_merkeldebates$cv_lastname)

# select only relevant variables 
# and transform to "long" format
dat_face <- dat_politicians_merkeldebates %>% 
    mutate(emo_nonneutral = 1 - emo_neutral) %>% 
    select(emo_anger, emo_happiness, emo_nonneutral,
           cv_lastname, speaker, hms_merge, debate_name) %>% 
    gather(emotion, value, -c(hms_merge, debate_name, speaker, cv_lastname))


# now we get the averages and bootstrapped CIs for emotions of interest


# averages and 95 percent confidence intervals for facial emotions
set.seed(35)
dat_sum_face_95 <- dat_face %>%
    filter(cv_lastname %in% c("Merkel", "Schroeder", 
                              "Steinbrueck", "Steinmeier",
                              "Schulz")) %>% 
    group_by(debate_name, cv_lastname, emotion) %>% 
    do(data.frame(rbind(Hmisc::smean.cl.boot(.$value, conf.int = .95, B = 1000))))  


# averages and 90 percent confidence intervals for facial emotions
set.seed(35)
dat_sum_face_90 <- dat_face %>%
    filter(cv_lastname %in% c("Merkel", "Schroeder", 
                              "Steinbrueck", "Steinmeier",
                              "Schulz")) %>% 
    group_by(debate_name, cv_lastname, emotion) %>% 
    do(data.frame(rbind(Hmisc::smean.cl.boot(.$value, conf.int = .90, B = 1000))))  %>% 
    rename(Lower90 = Lower, Upper90 = Upper) %>% 
    select(-Mean)


# merge both dataframes
dat_sum_face <- left_join(dat_sum_face_90, dat_sum_face_95,
                          c("debate_name", "cv_lastname", "emotion"))


# repeat for voice pitch (1 or 1.5 standard deviations above the mean)

dat_pitch <- dat_politicians_merkeldebates %>% 
    filter(speaker == cv_lastname) %>% 
    select(high_zff1, high_zff15,
           hms_merge, debate_name, speaker, cv_lastname) %>% 
    gather(emotion, value, -c(hms_merge, debate_name, speaker, cv_lastname))


# averages and 95 percent confidence intervals for pitch
set.seed(35)
dat_sum_pitch_95 <- dat_pitch %>%
    filter(speaker %in% c("Merkel", "Schroeder", 
                          "Steinbrueck", "Steinmeier",
                          "Schulz")) %>% 
    group_by(debate_name, cv_lastname, emotion) %>% 
    do(data.frame(rbind(Hmisc::smean.cl.boot(.$value, conf.int = .95, B = 1000))))  

# averages and 90 percent confidence intervals for pitch
set.seed(35)
dat_sum_pitch_90 <- dat_pitch %>%
    filter(speaker %in% c("Merkel", "Schroeder", 
                          "Steinbrueck", "Steinmeier",
                          "Schulz")) %>% 
    group_by(debate_name, cv_lastname, emotion) %>% 
    do(data.frame(rbind(Hmisc::smean.cl.boot(.$value, conf.int = .90, B = 1000)))) %>% 
    rename(Lower90 = Lower, Upper90 = Upper) %>% 
    select(-Mean)


# merge both dataframes
dat_sum_pitch <- left_join(dat_sum_pitch_90, dat_sum_pitch_95,
                           c("debate_name", "cv_lastname", "emotion"))


# next, repeat analysis for sentiment (on level of statements, not seconds!)

# 95 percent confidence intervals
set.seed(35)
dat_sentiment_95 <- dat_politicians_merkeldebates %>% 
    filter(speaker == cv_lastname) %>% 
    filter(speaker %in% c("Merkel", "Schroeder", 
                          "Steinbrueck", "Steinmeier",
                          "Schulz")) %>% 
    select(sentiment_log_lexicoder, 
           speaker,
           debate_name, cv_lastname,
           statement_number) %>% 
    unique() %>% 
    gather(emotion, value, -c(speaker, cv_lastname, 
                              statement_number,
                              debate_name)) %>% 
    group_by(debate_name, speaker, cv_lastname, emotion) %>% 
    do(data.frame(rbind(Hmisc::smean.cl.boot(.$value, conf.int = .95, B = 1000))))


# 90 percent confidence intervals
set.seed(35)
dat_sentiment_90 <- dat_politicians_merkeldebates %>% 
    filter(speaker == cv_lastname) %>% 
    filter(speaker %in% c("Merkel", "Schroeder", 
                          "Steinbrueck", "Steinmeier",
                          "Schulz")) %>% 
    select(sentiment_log_lexicoder, 
           speaker,
           debate_name, cv_lastname,
           statement_number) %>% 
    unique() %>% 
    gather(emotion, value, -c(speaker, cv_lastname, 
                              statement_number,
                              debate_name)) %>% 
    group_by(debate_name, speaker, cv_lastname, emotion) %>% 
    do(data.frame(rbind(Hmisc::smean.cl.boot(.$value, conf.int = .90, B = 1000)))) %>% 
    rename(Lower90 = Lower, Upper90 = Upper) %>% 
    select(-Mean)


# merge both dataframes
dat_sum_sentiment <- left_join(dat_sentiment_90, dat_sentiment_95,
                               c("debate_name", "cv_lastname", "emotion"))


# now merge the dataframe for facial emotions, pitch, and sentiment
dat_sum <- bind_rows(dat_sum_face, dat_sum_pitch,
                     dat_sum_sentiment)

table(dat_sum$emotion)

# recode speakers, create Merkel dummy,
# and give better names for emotion variables for plotting
dat_sum <- dat_sum %>% 
    mutate(speaker_clean = dplyr::recode(
        cv_lastname, "Schroeder" = "Schröder",
        "Steinbrueck" = "Steinbrück"
    )) %>% 
    mutate(speaker_year = paste0(debate_name, ": ", speaker_clean)) %>% 
    mutate(merkel_dummy = ifelse(speaker_clean == "Merkel", "Merkel", "Male Opponent")) %>% 
    mutate(emotion_clean = case_when(
        str_detect(emotion, "anger") ~ "Anger",
        str_detect(emotion, "happi") ~ "Happiness",
        str_detect(emotion, "neutral") ~ "Non-neutral Emotions",
        emotion ==  "high_zff1" ~ "High Voice Pitch (+1 SD)",
        emotion == "high_zff15"  ~ "High Voice Pitch (+1.5 SD)",
        str_detect(emotion, "lexic") ~ "Sentiment (Lexicoder)"
    ))


dat_sum$merkel_dummy <- relevel(factor(dat_sum$merkel_dummy),
                                ref = "Merkel")


# change factor levels for plot
levels_emotions <- c(
    "Happiness", 
    "Anger",
    "Non-neutral Emotions",
    "High Voice Pitch (+1 SD)",
    "High Voice Pitch (+1.5 SD)",
    "Sentiment (Lexicoder)"
)

dat_sum$emotion_clean <- factor(dat_sum$emotion_clean,
                                levels = levels_emotions)


                     # create a function to plot means and confidence intervals
ggemotion <- function(x, xlab = NULL, ylab) {
    
    ggplot(x, aes(x = debate_name, y = Mean,
                  colour = merkel_dummy,
                  shape = merkel_dummy)) +
        geom_point(size = 3,
                   position = position_dodge(width = 0.45)) +
        scale_colour_manual(values = c("black", "grey50")) +
        scale_shape_manual(values = c(16, 15)) +
        geom_linerange(aes(ymin = Lower,
                           ymax = Upper),
                       size = 0.5,
                       position = position_dodge(width = 0.45)) +
        geom_linerange(aes(ymin = Lower90,
                           ymax = Upper90),
                       size = 1.3,
                       position = position_dodge(width = 0.45))  +   
        facet_wrap(~emotion_clean, scales = "free") +
        labs(x = xlab, y = ylab) +
        theme(legend.position = "none")
}


p_happiness <- ggemotion(x = filter(dat_sum, emotion_clean == "Happiness"),
                         ylab = "Conf. Score") +
    scale_y_continuous(limits = c(0, 0.15))
p_happiness

p_anger <- ggemotion(x = filter(dat_sum, emotion_clean == "Anger"),
                     ylab = "Conf. Score")
p_anger

p_nonneutral <- ggemotion(x = filter(dat_sum, emotion_clean == "Non-neutral Emotions"),
                          ylab = "Conf. Score") +
    scale_y_continuous(limits = c(0, 0.2))
p_nonneutral


p_pitch1sd <- ggemotion(x = filter(dat_sum, emotion_clean == "High Voice Pitch (+1 SD)"),
                        ylab = "Seconds") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))
p_pitch1sd


p_pitch15sd <- ggemotion(x = filter(dat_sum, emotion == "high_zff15"),
                         ylab = "Seconds") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 0.1))
p_pitch15sd


p_sentiment <- ggemotion(x = filter(dat_sum, emotion_clean == "Sentiment (Lexicoder)"),
                         ylab = "Sentiment Score") +
    scale_y_continuous(limits = c(0, 1))
p_sentiment


# only add one legend
legend_add <- cowplot::get_legend(p_sentiment + theme(legend.position=c(1.6,0.5),
                                                      legend.direction = "horizontal",
                                                      legend.title = element_blank()))

# combine plots and save as Figure A13
cowplot::plot_grid(p_happiness,
                   p_anger,
                   p_nonneutral,
                   p_pitch1sd,
                   p_pitch15sd,
                   p_sentiment,
                   align = "v",
                   axis = 'l',
                   rel_heights = c(0.45, 0.45, 0.1),
                   legend_add)
ggsave("fig_a13.pdf",
       width = 10.6, height = 7)



## Table A01 ----

# apply the dictionary of masculine and feminine language
# and predict whether Merkel uses more "feminie language"

                     # create text corpus

corp <- corpus(dat_analysis, text_field = "text_english")

# get the terms from Roberts and Utych replication files
# note that the original file contained a few spelling mistakes 
# which we adjusted manually
dat_words <- read.csv("data_roberts_utych_replication.csv")

# get separate vectors with scores of words for all coders,
# only female coders, and only male coders
vector_gender_all <- dat_words$mean_a
vector_gender_female <- dat_words$mean_f
vector_gender_male <- dat_words$mean_m

names(vector_gender_all) <- dat_words$word
names(vector_gender_female) <- dat_words$word
names(vector_gender_male) <- dat_words$word

# get scores using judgment by female respondents
dfmat_speeches_weighted_female <- corp %>% 
    tokens() %>% 
    dfm() %>% 
    dfm_keep(pattern = dat_words$word) %>% 
    dfm_weight(scheme = "prop") %>%   # remove if you don't want normalized scores
    dfm_weight(weights = vector_gender_female)


# get scores using judgment by male respondents
dfmat_speeches_weighted_male <- corp %>% 
    tokens() %>% 
    dfm() %>% 
    dfm_keep(pattern = dat_words$word) %>% 
    dfm_weight(scheme = "prop") %>%   # remove if you don't want normalized scores
    dfm_weight(weights = vector_gender_male)


# get scores using judgment by all respondents
dfmat_speeches_weighted_all <- corp %>% 
    tokens() %>% 
    dfm() %>% 
    dfm_keep(pattern = dat_words$word) %>% 
    dfm_weight(scheme = "prop") %>%   # remove if you don't want normalized scores
    dfm_weight(weights = vector_gender_all)


# number of terms used at least once 
nfeat(dfmat_speeches_weighted_all)

# total number of terms
nrow(dat_words)


# get scores using row sums
dat_analysis$gender_language_all <- rowSums(dfmat_speeches_weighted_all)
dat_analysis$gender_language_female <- rowSums(dfmat_speeches_weighted_female)
dat_analysis$gender_language_male <- rowSums(dfmat_speeches_weighted_male)

# change reference categories
dat_analysis$speaker_clean <- relevel(factor(dat_analysis$speaker_clean),
                                      ref = "Female: Merkel")


dat_analysis$speaker_gender <- relevel(factor(dat_analysis$speaker_gender),
                                       ref = "Male")


dat_analysis <- dat_analysis %>% 
    mutate(merkel_dummy = ifelse(speaker == "Merkel", "Merkel", "Male Candidates"))

# run regression models

# all coders
lm_merkel_all <- lm(gender_language_all ~ merkel_dummy + debate_name, 
                    data = filter(dat_analysis, debate_name != "2017 (minor)"))

# female coders
lm_merkel_female <- lm(gender_language_female ~ merkel_dummy + debate_name, 
                       data = filter(dat_analysis, debate_name != "2017 (minor)"))

# male coders
lm_merkel_male <- lm(gender_language_male ~ merkel_dummy + debate_name, 
                     data = filter(dat_analysis, debate_name != "2017 (minor)"))


# all coders
lm_gender_all <- lm(gender_language_all ~ speaker_gender + debate_name, 
                    data = dat_analysis)

# female coders
lm_gender_female <- lm(gender_language_female ~ speaker_gender + debate_name, 
                       data = dat_analysis)

# male coders
lm_gender_male <- lm(gender_language_male ~ speaker_gender + debate_name, 
                     data = dat_analysis)



table(dat_analysis$speaker_clean)

                     # create regression table
screenreg(list(lm_merkel_all,
               lm_merkel_female,
               lm_merkel_male,
               lm_gender_all, 
               lm_gender_female,
               lm_gender_male))


# save Table A01 as a tex file
texreg(list(lm_merkel_all,
            lm_merkel_female,
            lm_merkel_male,
            lm_gender_all, lm_gender_female,
            lm_gender_male),
       custom.model.names = c(
           "(1) All coders",
           "(2) Female",
           "(3) Male",
           "(4) All coders",
           "(5) Female",
           "(6) Male"),
       omit.coef = c("debate*"), # remove coefficients for debate fixed-effects
       custom.coef.names = c(
           "Intercept",
           "Speaker: Merkel (ref.: Male)",
           "Speaker: Female (ref.: Male)"),
       file = "tab_a01.tex",
       fontsize = "scriptsize",
       label = "tab:reg_gendered_language",
       caption = "Predicting the masculinity of language using method introduced by \\citet{roberts2020linking}. 
       Models 1--3 limit the analysis to the four debates involving Angela Merkel. Models 4--6 also consider the minor party debate. All models include debate fixed effects.",
       caption.above = FALSE)

