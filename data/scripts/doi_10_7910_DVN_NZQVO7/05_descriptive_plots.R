####################################################################
####################################################################
## Replication Material
##
## Shaun Bowler, Gail McElroy, Stefan Müller:
## Voter Expectations of Government Formation in Coalition Systems: 
## The Importance of the Information Context.
## European Journal of Political Research

## File: 05_descriptive_plots.R

## See 000_description_replication_material.pdf for a detailed 
## overview of the required data and the outputs of this file.

####################################################################
####################################################################


library(dplyr) # A Grammar of Data Manipulation, CRAN v1.0.2
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics, CRAN v3.3.2
library(stringr) # Simple, Consistent Wrappers for Common String Operations, CRAN v1.4.0
library(car) # Companion to Applied Regression, CRAN v3.0-9
library(forcats) # Tools for Working with Categorical Variables (Factors), CRAN v0.5.0
library(htmlTable) # Advanced Tables for Markdown/HTML, CRAN v2.1.0
library(Hmisc) # Harrell Miscellaneous, CRAN v4.4-1
library(scales) # Scale Functions for Visualization, CRAN v1.1.1 # Scale Functions for Visualization, CRAN v1.1.1
library(srvyr) # 'dplyr'-Like Syntax for Summary Statistics of Survey Data, CRAN v0.4.0
library(scales) # Scale Functions for Visualization, CRAN v1.1.1 # Scale Functions for Visualization, CRAN v1.1.1

## load ggplot2 scheme
source("function_theme_base.R")


## load combined dataset with all relevant variables
dta_combined <- readRDS("data_reg_full.rds") 


## create table with overview of elections, number of polls, 
## and number of coalition options

## binary numeric indicator for correct coalition prediction

dta_combined <- dta_combined %>% 
  mutate(correct_prediction_coalition_num = as.numeric(correct_prediction_coalition))

dta_combined <- dta_combined %>% 
  group_by(election_id_english) %>% 
  mutate(number_choices = length(unique(coalition_option_stand)))


choice_set <- dta_combined %>% 
  unique() %>% 
  group_by(election_id) %>% 
  mutate(choices = length(unique(coalition_option_stand))) %>% 
  dplyr::select(election_id, choices) 

mean(choice_set$choices)  

sum(choice_set$choices)  


## Table 1 ----
## create table with descriptive statistics
dta_combined_table <- dta_combined %>% 
  group_by(year, bundesland_english) %>% 
  mutate(`Respondents (total)` = length(unique(respondent_id)),
         `Government options` = length(unique(coalition_option_stand))) %>% 
  select(year, bundesland_english, `Respondents (total)`, number_polls, `Government options`) %>% 
  unique() %>% 
  arrange(year, bundesland_english) %>% 
  rename(Year = year,
         Election = bundesland_english,
         Polls = number_polls) 

dta_dontknow <- dta_combined %>% 
  select(year, bundesland_english, respondent_id, predicted_coalition_stand) %>% 
  unique() %>% 
  mutate(dontknow = ifelse(is.na(predicted_coalition_stand), 1, 0)) %>% 
  group_by(year, bundesland_english) %>% 
  summarise(`Don't know/No answer` = sum(dontknow)) %>% 
  rename(Year = year,
         Election = bundesland_english)

dta_combined_table <- left_join(dta_combined_table, dta_dontknow,
                                by = c("Year", "Election"))

dta_combined_table <- dta_combined_table %>% 
  ungroup() %>% 
  mutate(`No prediction (%)` = paste0(round(100 * `Don't know/No answer` / `Respondents (total)`, 1), "%")) %>% 
  select(Year, Election, `Respondents (total)`,
         `Don't know/No answer`,
         `No prediction (%)`, `Government options`, Polls) 

dta_combined_table

sum(dta_combined_table$`Respondents (total)`)

## > sum(dta_combined_table$`Government options`)
## [1] 181

dta_combined_options <- dta_combined %>% 
  select(election_id, coalition_option_stand, probability) %>% 
  filter(!is.na(probability)) %>% 
  unique()

nrow(dta_combined_options)

## save table 01
htmlTable::htmlTable(dta_combined_table, 
                     align = "left",
                     file = "tab_01.htm",
                     rnames = FALSE)



## create dataset with only one observation (the corrected government)
dta_combined_one_obs <- dta_combined %>% 
  select(respondent_id, predicted_coalition_stand, everything()) %>% 
  filter(predicted_coalition_dummy == TRUE) 

dta_combined_one_obs <- dta_combined_one_obs %>% 
  group_by(respondent_id) %>% 
  mutate(number_predicted_coalitions_unique = length(unique(predicted_coalition_stand))) %>% 
  ungroup()


## select one observation randomly for each respondent
set.seed(123)
dta_combined_one_obs_random <- dta_combined_one_obs %>% 
  group_by(respondent_id) %>% 
  sample_n(size = 1) %>% 
  ungroup()

## select only respondents who predicted one coalition
dta_combined_one_obs_only <- dta_combined_one_obs %>% 
  filter(number_predicted_coalitions_unique == 1)


## nrow(dta_combined_one_obs_test)
## 
## stopifnot(nrow(dta_combined_one_obs_random) == length(unique(dta_combined_one_obs_test$respondent_id)))

set.seed(123)


## Figure 1 ----

## Percentage of correct coalition predictions, pooled across all Land elections

## bootstrap proportions by selecting one observation per respondent
## (to take into account take some respondents gave the same likelihood to many coalition types)
set.seed(235)
boot_predict_correct_random <- dta_combined_one_obs_random %>%
  group_by(predict_coalition_type, coalition_after_election_plot, 
           election_type, election_id_english) %>%
  do(data.frame(rbind(smean.cl.boot(.$correct_prediction_coalition_num, na.rm = TRUE)))) %>% 
  mutate(model = "One prediction per respondent selected randomly")

## allow more than one prediction
set.seed(235)
boot_predict_correct_more_than_one <- dta_combined_one_obs %>%
  group_by(predict_coalition_type, coalition_after_election_plot, election_type, election_id_english) %>%
  do(data.frame(rbind(smean.cl.boot(.$correct_prediction_coalition_num, na.rm = TRUE)))) %>% 
  mutate(model = "More than one prediction allowed")

## only select respondents who made a single prediction
set.seed(235)
boot_predict_correct_one_prediction <- dta_combined_one_obs %>%
  filter(number_predicted_coalitions_unique == 1) %>% 
  group_by(predict_coalition_type, coalition_after_election_plot, election_type, election_id_english) %>%
  do(data.frame(rbind(smean.cl.boot(.$correct_prediction_coalition_num, na.rm = TRUE)))) %>% 
  mutate(model = "Only respondents with one prediction")


## bind data frames and create variables for plot
boot_combined <- bind_rows(boot_predict_correct_random,
                           boot_predict_correct_more_than_one,
                           boot_predict_correct_one_prediction) %>% 
  mutate(election_id_coa = paste0(election_id_english, " (", 
                                  coalition_after_election_plot, ")")) %>% 
  mutate(election_id_coa = as.factor(str_replace_all(election_id_coa, "ue", "ü"))) %>% 
  mutate(election_id_coa = car::recode(election_id_coa, "'Bayern 2013 (CDU)'='Bayern 2013 (CSU)'"))


## create and save Figure 1

## Percentage of correct coalition predictions in each election

ggplot(filter(boot_combined, 
              model == "Only respondents with one prediction"),
       aes(x = reorder(election_id_coa, Mean), 
           y = Mean,
           ymax = Upper, ymin = Lower,
           colour = election_type,
           shape = election_type)) + 
  geom_point(size = 3) +
  geom_linerange(size = 1.05) +
  scale_y_continuous(limits = c(0, 1), breaks = c(seq(0, 1, 0.2)),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_colour_manual(values = c("red", "black"), name = NULL) +
  scale_shape_manual(values = c(17, 19), name = NULL) +
  coord_flip() + 
  theme(legend.position = "bottom") +
  labs(y = "Percentage of correct government predictions", 
       x = NULL) 
ggsave(file = "fig_01.pdf",
       width = 8, height = 7)
ggsave(file = "fig_01.png", 
       width = 8, height = 7)



## Figure 2 ----

## allow for more than one prediction
set.seed(235)
total_boot_predict_correct_more_than_one <- dta_combined_one_obs %>%
  group_by(election_type) %>% 
  do(data.frame(rbind(smean.cl.boot(.$correct_prediction_coalition_num, na.rm = TRUE, conf.int=.95)))) %>% 
  mutate(model = "More than one prediction allowed")

## select one prediction randomly for each respondent and check accuracy
set.seed(235)
total_boot_predict_correct_random <- dta_combined_one_obs_random %>%
  group_by(election_type) %>% 
  do(data.frame(rbind(smean.cl.boot(.$correct_prediction_coalition_num, na.rm = TRUE, conf.int=.95)))) %>% 
  mutate(model = "One prediction per respondent selected randomly")

## select only respondents who made a single prediction
set.seed(235)
total_boot_predict_correct_one_prediction <- dta_combined_one_obs %>%
  filter(number_predicted_coalitions_unique == 1) %>% 
  group_by(election_type) %>% 
  do(data.frame(rbind(smean.cl.boot(.$correct_prediction_coalition_num, na.rm = TRUE, conf.int=.95)))) %>% 
  mutate(model = "Only respondents with one prediction")

## bind data frames
total_boot_predict <- bind_rows(total_boot_predict_correct_more_than_one,
                                total_boot_predict_correct_random,
                                total_boot_predict_correct_one_prediction)

total_boot_predict$election_type <- factor(total_boot_predict$election_type, 
                                           levels = c("State Election", "Federal Election"))


## create and save Figure 2 
ggplot(total_boot_predict, aes(x = model, y = Mean, 
                               ymin = Lower, ymax = Upper,
                               colour = election_type,
                               shape = election_type)) +
  geom_point(size = 3) +
  geom_linerange(size = 1.05) +
  coord_flip() + 
  scale_shape_manual(values = c(20, 17), name = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     limits = c(0, 0.6),
                     breaks = c(seq(0, 0.6, 0.1))) +
  scale_colour_manual(values = c("black", "red"), name = NULL) +
  labs(y = "Percentage of correct predictions", x = NULL) 
ggsave("fig_02.pdf", width = 8, height = 2.5)
ggsave("fig_02.png", width = 8, height = 2.5)




## Figure 3 ----

## Proportions of the most frequently predicted governments for each election



length(dta_combined_one_obs_random$respondent_id)
nrow(dta_combined_one_obs_random)

types <- dta_combined_one_obs_random %>% 
  group_by(election_id) %>% 
  select(predict_coalition_type) %>% 
  unique() 


df_summary <- dta_combined_one_obs_only %>% 
  mutate(predicted_coalition_factor = as.factor(predicted_coalition_stand)) %>%
  mutate(coalition_after_election_factor = as.factor(coalition_after_election)) %>% 
  mutate(election_type = as.factor(election_type)) %>% 
  srvyr::as_survey_design() %>% 
  group_by(election_type, year, bundesland_english,
           coalition_after_election_factor, predicted_coalition_factor) %>% 
  summarize(prop = srvyr::survey_mean(na.rm = TRUE)) %>% 
  mutate(ci_lower = prop - 1.96 * prop_se) %>% 
  mutate(ci_upper = prop + 1.96 * prop_se) %>% 
  group_by(year) %>% 
  arrange(year, -prop) %>%
  mutate(order = row_number()) 

df_summary <- df_summary %>% 
  mutate(coalition_after_election_factor = str_replace_all(coalition_after_election_factor, "_ssw", "")) %>% 
  mutate(coalition_after_election_char = as.character(coalition_after_election_factor)) %>% 
  mutate(predicted_coalition_char = as.character(predicted_coalition_factor)) %>% 
  mutate(correct_prediction = ifelse(coalition_after_election_char == predicted_coalition_char, TRUE, FALSE)) 


recode_coalition_predicted <- c("'cdu'='CDU'; 'cdu_fdp'='CDU, FDP'; 'cdu_fdp_greens'='CDU, FDP, Greens';
                                'cdu_greens'='CDU, Greens'; 'cdu_spd'='CDU, SPD';
                                'cdu_spd_greens'='CDU, SPD, Greens'; 'spd_fdp_greens'='SPD, FDP, Greens'; 'spd_greens'='SPD, Greens'; 'spd_greens_ssw'='SPD, Greens, SSW'; 'spd_left'='SPD, Left'; 'spd_left_greens'='SPD, Left, Greens'; 'spd_greens_other'='SPD, Greens, Other'; 'spd_other'='SPD, Other'; 'spd_fdp'='SPD, FDP'; 'spd_fdp_left'='SPD, FDP, Left'; 'spd_greens_fw'='SPD, Greens';
                                'left_other'='Left, Other'='greens_other'='Greens, Other';
                                'left'='Left';'left_greens'='Left, Greens';'fdp_greens_other'='FDP, Greens, Other'; 'fdp_greens'='FDP, Greens';'fdp'='FDP';'cdu_spd_other'='CDU, SPD, Other';
                                'cdu_spd_left_greens'='CDU, SPD, Left, Greens'='cdu_spd_fdp_other'='CDU, SPD, FDP, Other'='cdu_spd_fdp_left'='CDU, SPD, FDP, Left'; 'cdu_spd_fdp_left_greens'='CDU, SPD, FDP, Left, Greens';'cdu_afd'='CDU, AfD'; 'cdu_fdp_left'='CDU, FDP, Left'; 'spd'='SPD';
                                'cdu_fdp_other'='CDU, FDP, Other';'cdu_fw'='CDU, FW';
                                'cdu_left_greens'='CDU, Left, Greens'; 'cdu_other'='CDU, Other';
                                'cdu_spd_fdp'='CDU, SPD, FDP'; 'cdu_spd_fdp_greens'='CDU, SPD, FDP, Greens'; 'cdu_spd_fdp_left'='CDU, SPD, FDP, Left'")

df_summary$predicted_coalition_plot <-  car::recode(df_summary$predicted_coalition_factor, recode_coalition_predicted)

## recode Bavarian CSU to CDU ("sister party")
df_summary_no_bavaria <- df_summary %>%
  filter(bundesland_english != "Bavaria")

df_summary_bavaria <- df_summary %>%
  filter(bundesland_english == "Bavaria")

df_summary_bavaria <- df_summary_bavaria %>%
  mutate(predicted_coalition_plot = str_replace(predicted_coalition_plot,
                                                "CDU", "CSU"))


df_summary <- bind_rows(df_summary_bavaria, df_summary_no_bavaria)

df_summary_top <- df_summary %>% 
  mutate(election_id_english = paste0(year, ": ", bundesland_english)) %>% 
  group_by(election_id_english) %>% 
  arrange(-prop) %>% 
  mutate(n = 1:n()) %>% 
  filter(n <= 3)


## create and save Figure 3
ggplot(data = df_summary_top, 
       aes(x = factor(nrow(df_summary_top):1), 
           y = prop, ymin = ci_lower,
           ymax = ci_upper, 
           colour = correct_prediction,
           shape = correct_prediction)) +
  geom_pointrange() +
  facet_wrap(~election_id_english, scales = "free_y", ncol = 2) +
  coord_flip() +
  scale_colour_manual(values = c("black", "red")) +
  scale_x_discrete(breaks = factor(nrow(df_summary_top):1),
                   labels = factor(df_summary_top$predicted_coalition_plot)) +
  labs(x = NULL, y = "Percentage of predictions") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 0.9)) +
  theme(strip.text = element_text(size = 12, face = "bold"),
        legend.position = "none")
ggsave(file = "fig_03.pdf", 
       width = 10, height = 11.5)
ggsave(file = "fig_03.png", 
       width = 10, height = 11.5)




## Figure A01 ----

## check correlations and correspondence between measures of coalition probability:
## allow surplus governments or do not include them

prob_combined_land <- readRDS("data_coalition_predictions_land.rds") %>% 
  mutate(type = "State Elections")

prob_combined_federal <- readRDS("data_coalition_predictions_federal.rds") %>% 
  mutate(type = "Federal Elections")

prob_combined <- bind_rows(prob_combined_land, prob_combined_federal)

prob_combined$type <- factor(prob_combined$type, levels = c("State Elections", "Federal Elections"))

cor.test(prob_combined$probability, prob_combined$probability_surplus)

ggplot(prob_combined, aes(x = probability, y = probability_surplus)) + 
  geom_jitter(width = 3, height = 3, size = 2) + 
  labs(x = "Probability of majority (no surplus governments)", y = "Probability of majority") + 
  facet_wrap(~type)  +
  theme(panel.border = element_rect(colour = "black", fill=NA))
ggsave("fig_a01.pdf", width = 8, height = 4)




## Figure A02 ----

## load the 50 most frequent features (after accounting for multiword expressions) 
## from the corpus of relevant sentences 
## (i.e. sentences that mention at least two parties and a term indicating cooperation)
## The csv file also contains manual English translations of the terms

tstat_freq_translated <- read_csv("data_tstat_freq_translated.csv")

## merge German (original) and English (translated) terms into one string
tstat_freq_translated <- tstat_freq_translated %>% 
  mutate(feature_both = paste0(feature, " (", feature_translated, ")"))

## create and save plot
ggplot(tstat_freq_translated, 
       aes(x = reorder(feature_both, frequency), y = frequency)) +
  geom_bar(stat = "identity", width = 0.2) + 
  geom_point(size = 3) +
  coord_flip() +
  labs(x = NULL, y = "Most frequent terms and multi-word expressions") 
ggsave("fig_a02.pdf",
       width = 8, height = 9)



## Figure A03 ----

## check for survey instrument effects (number of choices/question format) ---


## data frame with correct/incorrect coalition prediction dummy per respondent
## if respondent made several guesses, the respondent is in the dataset once per guess
dta_combined_correct <- dta_combined %>% 
  mutate(correct_prediction_coalition_num = ifelse(correct_prediction_coalition == TRUE, 1, 0)) %>% 
  mutate(predicted_coalition_num = ifelse(predicted_coalition_stand == coalition_option_stand, 1, 0)) %>% 
  mutate(coalition_after_election_factor = coalition_after_election) %>% 
  ungroup() %>% 
  select(predict_coalition_type, number_choices, election_id_english, election_type, correct_prediction_coalition_num,
         predicted_coalition_stand, respondent_id) %>% 
  unique()

nrow(dta_combined_correct) ## predictions
length(unique(dta_combined_correct$respondent_id)) ## respondents

## bootstrap means and confidence intervals

set.seed(234)
boot_choices <- dta_combined_correct %>%
  filter(predict_coalition_type == "Coalitions: continuous") %>% 
  group_by(number_choices) %>%
  do(data.frame(rbind(smean.cl.boot(.$correct_prediction_coalition_num, na.rm = TRUE, conf.int = 0.95)))) %>% 
  mutate(model = "Number of choices")

## create and save Figure A03
ggplot(boot_choices, aes(x = factor(number_choices), 
                         y = Mean,
                         ymax = Upper, ymin = Lower)) + 
  geom_bar(stat = "identity", position = position_dodge(width = 1), 
           colour = "black",
           fill = "grey90",
           width = 0.6) +
  geom_errorbar(position = position_dodge(width = .8), width = 0.4,
                size = 0.7, colour = "grey50") + 
  scale_shape_discrete(name = NULL) +
  scale_y_continuous(limits = c(0, 0.65), breaks = c(seq(0, 0.65, 0.1))) +
  labs(y = "Correct predictions", 
       x = "Number of governments to evaluate")
ggsave(file = "fig_a03.pdf", 
       width = 8, height = 4)



## Figure A08 ----

## compare Wordscores estimates to left-right positions
dta_parties <- read_csv("data_parties_all_wordscores.csv")


dta_parties_clean <- dta_parties %>% 
  separate(election_id, into = c("state", "election_year"), sep = " ", remove = FALSE) %>% 
  mutate(party_merge = str_to_lower(party)) %>% 
  select(-party) %>% 
  mutate(state = str_replace_all(state, "ue", "ü")) 

## use the full dataset, get the left-right party placements by voters
## exclude the wordscores placements and estimate correlations

dta_lr_placements <- dta_combined %>% 
  select(respondent_id, election_id, election_id_english, year,
         starts_with("lr_")) %>% 
  select(-contains(c("ws_", "avg_", "dummy", "lr_self"))) %>% 
  gather(party, lr_voter, -c(respondent_id, election_id, election_id_english,
                             year)) %>% 
  unique() ## keep every respondent only once


## filter missing evaluations (either because respondent did 
## not evaluate party or because party was not part of survey)

dta_lr_placements <- filter(dta_lr_placements, !is.na(lr_voter)) 

length(unique(dta_lr_placements$respondent_id))

dta_lr_placements_clean <- dta_lr_placements %>% 
  rename(election_year = year) %>% 
  mutate(party = str_replace_all(party, "lr_", "")) %>% 
  group_by(election_id_english, election_id, election_year, party) %>% 
  do(data.frame(rbind(Hmisc::smean.cl.boot(.$lr_voter)))) %>% 
  mutate(party_merge = str_to_lower(party)) %>% 
  mutate(party_merge = car::recode(party_merge, "'csu'='cdu'"))

names(dta_lr_placements_clean)

dta_lr_placements_clean$election_year <- as.character(dta_lr_placements_clean$election_year)
dta_parties_clean$election_year <- as.character(dta_parties_clean$election_year)


dta_joined <- left_join(dta_lr_placements_clean, 
                        dta_parties_clean)

## remove party-election observations without a Wordscores value
dta_joined <- filter(dta_joined, !is.na(wordscore_leftright))

## only keep the relevant parties 
dta_joined <- dta_joined %>% 
  filter(party %in% c("afd", "cdu", "csu", "fdp",
                      "greens", "left", "spd"))

dta_joined <- dta_joined %>% 
  mutate(election_id = str_replace_all(election_id, "ue", "ü")) %>% 
  mutate(party = car::recode(party, "'cdu'='CDU/CSU';
                               'csu'='CDU/CSU';
                               'fdp'='FDP';
                               'left'='Left';
                               'greens'='Greens';
                               'afd'='AfD';
                               'spd'='SPD'"))


dta_joined <- dta_joined %>% 
  mutate(election_id_english = str_replace_all(election_id, "Bayern", "Bavaria")) %>% 
  mutate(election_id_english = str_replace_all(election_id_english, "Mecklenburg-Vorpommern", "Mecklenburg-Western Pomerania")) %>% 
  mutate(election_id_english = str_replace_all(election_id_english, "Hessen", "Hesse")) %>% 
  mutate(election_id_english = str_replace_all(election_id_english, "Nordrhein-Westfalen", "North Rhine-Westphalia")) %>% 
  mutate(election_id_english = str_replace_all(election_id_english, "Rheinland-Pfalz", "Rhineland-Palatinate")) %>% 
  mutate(election_id_english = str_replace_all(election_id_english, "Niedersachsen", "Lower Saxony")) %>% 
  mutate(election_id_english = str_replace_all(election_id_english, "Sachsen", "Saxony")) %>% 
  mutate(election_id_english = str_replace(election_id_english, "Thüringen", "Thuringia")) 


## estimate correlations on the level of elections
cors_election <- dta_joined %>% 
  group_by(election_id_english) %>% 
  summarise(cor = round(cor(Mean, wordscore_leftright), 2))


## set colours for parties
colour_parties_subset <- c("blue", "black",
                           "orange",
                           "darkgreen",
                           "darkred",
                           "red")

## create and save Figure A08
ggplot(dta_joined, aes(x = Mean,
                       y = wordscore_leftright,
                       colour = party)) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper)) +
  geom_errorbar(aes(ymin = wordscore_leftright - 1.96 * wordscore_leftright_se,
                    ymax = wordscore_leftright + 1.96 * wordscore_leftright_se)) +
  geom_point() +
  facet_wrap(~election_id_english, ncol = 4) +
  geom_text(data = cors_election,
            size = 4.5,
            aes(label = paste("r=", round(cor, 2), sep="")), 
            x = 1.5, y = 20,
            colour = "grey50") +   
  guides(colour = guide_legend(nrow = 1)) +
  scale_colour_manual(values = colour_parties_subset) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11),
        strip.text = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits  = c(-0.5, 10.5),
                     breaks = c(seq(0, 10, 2))) +
  labs(x = "Average party placement by voters",
       y = "Wordscores estimate of party manifesto")
ggsave("fig_a08.pdf", width =  13, height = 12)


## Figure A09 ----

## compare perceived distances between the two most extreme parties 
## in a coalition based on respondents' evaluations 
## and Wordscores estimates

dta_coa_distance <- dta_combined %>% 
  filter(federal_state_dummy == "State") %>% 
  select(distance_coa_ws_2_most_extreme_parties,
         distance_coa_avg_2_most_extreme_parties,
         coalition_option_print, coalition_option_stand,
         election_id_english) %>% 
  unique()


cors_distance_all <- dta_coa_distance %>% 
  ungroup() %>% 
  summarise(cor = round(cor(distance_coa_ws_2_most_extreme_parties, 
                            distance_coa_avg_2_most_extreme_parties), 2))


cor.test(dta_combined$distance_coa_ws_2_most_extreme_parties,
         dta_combined$distance_coa_avg_2_most_extreme_parties)

ggplot(dta_coa_distance, 
       aes(x = distance_coa_avg_2_most_extreme_parties,
           y = distance_coa_ws_2_most_extreme_parties)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm") +
  geom_text(data = cors_distance_all,
            size = 6,
            aes(label = paste("r=", round(cor, 2), sep="")), 
            x = 0.2, y = 3.5,
            colour = "grey50") +
  labs(x = "Average perceived distance between potential coalition partners (respondents)",
       y = "Distance between potential\ncoalition partners (Wordscores)")
ggsave("fig_a09.pdf", width =  8, height = 4)



