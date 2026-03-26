####################################################################
####################################################################
## Replication Material
##
## Shaun Bowler, Gail McElroy, Stefan Müller:
## Voter Expectations of Government Formation in Coalition Systems: 
## The Importance of the Information Context.
## European Journal of Political Research

## File: 07c_random_forest.R

## See 000_description_replication_material.pdf for a detailed 
## overview of the required data and the outputs of this file.

####################################################################
####################################################################

## run random forest models to assess the relative importance of 
## variables predicting true/wrong coalition predictions


library(dplyr) # A Grammar of Data Manipulation, CRAN v1.0.2
library(stringr) # Simple, Consistent Wrappers for Common String Operations, CRAN v1.4.0
library(randomForest) # Breiman and Cutler's Random Forests for Classification and Regression, CRAN v4.6-14
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics, CRAN v3.3.2
library(car) # Companion to Applied Regression, CRAN v3.0-9
library(tidyr) # Tidy Messy Data, CRAN v1.1.2


## load ggplot2 scheme
source("function_theme_base.R")

## load respondent-choice level dataset
dta_select_reg <- readRDS("data_reg_full.rds") 


## recode the age categories
dta_select_reg <- dta_select_reg %>% 
  mutate(age_categorical = case_when(age_stand %in% 16:25 ~ "16-25",
                                     age_stand %in% 26:35 ~ "26-35",
                                     age_stand %in% 36:45 ~ "36-45",
                                     age_stand %in% 46:55 ~ "46-55",
                                     age_stand %in% 56:65 ~ "56-65",
                                     age_stand > 66 ~  "66 and older"))

## select the unique desired coalitions by respondent
dta_select_reg_wish_unique <- dta_select_reg %>% 
  mutate(wish_coalition_stand = ifelse(wish_coalition_dummy == TRUE, coalition_option_stand, NA)) %>% 
  select(respondent_id, wish_coalition_stand) %>% 
  filter(!is.na(wish_coalition_stand)) %>% 
  unique()

dta_select_reg <- dta_select_reg %>% 
  mutate(predicted_equals_true_factor = as.factor(predicted_equals_true)) 


## select only respondents who predicted a single government
dta_select_reg_one <- dta_select_reg %>% 
  filter(predicted_coalition_dummy == TRUE) %>% 
  filter(number_predicted_coalitions_unique == 1)

dta_select_reg_one <- dta_select_reg_one %>% 
  mutate(predicted_equals_true_numeric = as.numeric(predicted_equals_true)) 

dta_select_reg_one <- dta_select_reg_one %>% 
  ungroup() %>% 
  group_by(election_id, respondent_id) %>% 
  mutate(predicted_equals_true = ifelse(sum(predicted_equals_true_numeric) >= 1, 1, 0)) %>% 
  mutate(proportion_predicted_true = predicted_equals_true / number_predicted_coalitions_unique) %>% 
  arrange(-number_predicted_coalitions_unique, respondent_id)


## recode and relevel variables
dta_select_reg_one$interest_campaign_stand[dta_select_reg_one$interest_campaign_stand=='Don’t know'] <- NA
dta_select_reg_one$interest_campaign_stand[dta_select_reg_one$interest_campaign_stand=='No information'] <- NA

dta_select_reg_one$interest_campaign_stand_factor <- factor(dta_select_reg_one$interest_campaign_stand)

table(dta_select_reg_one$interest_campaign_stand_factor)


dta_select_reg_one$interest_campaign_stand_factor <- factor(dta_select_reg_one$interest_campaign_stand_factor,
                                                            levels = c('No interest at all',
                                                                       'Not much interest',
                                                                       'Medium interest',
                                                                       'Strong interest',
                                                                       'Very strong interest'))

## select relevant variables for random forest models

dat_random_forest <- dta_select_reg_one %>% 
  ungroup() %>% 
  select(election_id, 
         predicted_equals_true_numeric,
         incumbent_coalition,
         wish_coalition_dummy,
         pedersen_vol,
         probability_proportion_surplus,
         gender_stand,
         pedersen_vol,
         distance_lr_coa_self_no_missing,
         incumbent_coalition,
         interest_campaign_stand_factor,
         education_stand,
         number_predicted_coalitions_unique,
         predict_coalition_type,
         age_categorical,
         distance_coa_2_parties_no_missing,
         ## lr_avg_coa_self +
         income_num) %>% 
  filter(number_predicted_coalitions_unique == 1) 

length(unique(dat_random_forest$election_id))


## change some variables to factors/numeric variables
dat_random_forest$predicted_equals_true_numeric <- factor(dat_random_forest$predicted_equals_true_numeric)
dat_random_forest$age_categorical <- factor(dat_random_forest$age_categorical)

dat_random_forest$unique_number <- 1:nrow(dat_random_forest)
dat_random_forest$election_id <- factor(dat_random_forest$election_id)

dat_random_forest$incumbent_coalition <- as.factor(dat_random_forest$incumbent_coalition)


## run Random Forest Model #1

random_forest_1 <- randomForest::randomForest(
  predicted_equals_true_numeric ~
    wish_coalition_dummy + 
    probability_proportion_surplus + 
    gender_stand +
    interest_campaign_stand_factor + 
    education_stand +
    distance_lr_coa_self_no_missing +
    incumbent_coalition + 
    pedersen_vol + 
    election_id +
    age_categorical + 
    income_num,
  data = dat_random_forest,
  na.action = na.omit,
  keep.forest = FALSE,
  importance = TRUE,
  ntree = 1000
)

## tidy output of random forest model
importance_1 <- randomForest::importance(random_forest_1, type = 1)
vnames_1 <- rownames(importance_1)
i_1 <- as.vector(importance_1)
names(i_1) <- vnames_1
dat_importance_1 <- data.frame(importance = i_1[order(i_1, decreasing = TRUE)])
dat_importance_1$variable <- rownames(dat_importance_1)

dat_importance_1$type <-  "Model 1"


## run Random Forest Model #2
## (this model includes more observations because we do not use 
## desired government (which is not available in all election surveys, 
## but the distance between the respondent and the government as an 
## alternative measure)

random_forest_2 <- randomForest::randomForest(
  predicted_equals_true_numeric ~
    distance_coa_2_parties_no_missing + 
    distance_lr_coa_self_no_missing +
    probability_proportion_surplus + 
    gender_stand +
    interest_campaign_stand_factor + 
    education_stand +
    incumbent_coalition + 
    pedersen_vol + 
    election_id +
    age_categorical + 
    income_num,
  data = dat_random_forest,
  na.action = na.omit,
  keep.forest = FALSE,
  importance = TRUE,
  ntree = 1000
)


## tidy output of random forest model
importance_2 <- randomForest::importance(random_forest_2, type = 1)
vnames_2 <- rownames(importance_2)
i_2 <- as.vector(importance_2)
names(i_2) <- vnames_2
dat_importance_2 <- data.frame(importance = i_2[order(i_2, decreasing = TRUE)])
dat_importance_2$variable <- rownames(dat_importance_2)

dat_importance_2$type <-  "Model 2"


## recode variable names
recode_randomforest <- c("
                         'probability_proportion_surplus'='Coalition_Probability of Majority';
                         'election_id'='Context_Election';
                         'pedersen_vol'='Context_Pedersens Volatility Index';
                         'incumbent_coalition'='Coalition_Incumbent Government';
                         'income_num'='Individual_Income';
                         'age_categorical'='Individual_Age';
                         'interest_campaign_stand_factor'='Individual_Interest in Campaign';
                         'wish_coalition_dummy'='Coalition_Desired Government';
                         'education_stand'='Individual_Education';
                         'gender_stand'='Individual_Gender';
                         'distance_coa_2_parties_no_missing'='Coalition_Perceived Distance Between Parties';
                         'distance_lr_coa_self_no_missing'='Coalition_Distance Between Government and Respondent'")



dat_importance_merged <- bind_rows(dat_importance_1,
                                   dat_importance_2) %>% 
  mutate(variable = car::recode(variable, recode_randomforest)) %>% 
  separate(variable, into = c("level", "variable"), sep = "_") %>% 
  arrange(type, -importance) %>% 
  mutate(variable = str_replace_all(variable, "Pedersens", "Pedersen's"))

## Figure 05 ----

## create and save Figure 05
ggplot(data = dat_importance_merged, 
       aes(x = factor(nrow(dat_importance_merged):1), 
           y = importance,
           fill = level)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ type, scales = "free_y", nrow = 2) +
  coord_flip() +
  scale_fill_manual(values = c("black", "grey50", "grey80")) +
  scale_x_discrete(breaks = nrow(dat_importance_merged):1,
                   labels = dat_importance_merged$variable) +
  labs(x = NULL, y = "Variable importance (mean decrease in accuracy)") +
  theme(legend.title = element_blank())
ggsave("fig_05.pdf", width = 8, height = 6)
ggsave("fig_05.png", width = 8, height = 6)



