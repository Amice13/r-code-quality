####################################################################
####################################################################
## Replication Material
##
## Shaun Bowler, Gail McElroy, Stefan Müller:
## Voter Expectations of Government Formation in Coalition Systems: 
## The Importance of the Information Context.
## European Journal of Political Research

## File: 07a_reg_logit.R

## See 000_description_replication_material.pdf for a detailed 
## overview of the required data and the outputs of this file.

####################################################################
####################################################################


## run logistic regressions with right/wrong prediction as the 
## dependent variable

## load packages

library(dplyr) # A Grammar of Data Manipulation, CRAN v1.0.2
library(rms) # Regression Modeling Strategies, CRAN v6.0-1
library(texreg) # Conversion of R Regression Output to LaTeX or HTML Tables, CRAN v1.37.5
library(effects) # Effect Displays for Linear, Generalized Linear, and Other Models, CRAN v4.2-0
library(lme4) # Linear Mixed-Effects Models using 'Eigen' and S4, CRAN v1.1-23


## load respondent-choice level dataset
dta_combined <- readRDS("data_reg_full.rds") 

## remove respondents who did not predict a coalition (important)
## note that the prediction of "don't know" responses is included in 
## 07d_reg_dontknow.R

dta_select_reg <- dta_combined %>% 
  filter(!is.na(predicted_coalition_stand))

table(dta_select_reg$predicted_equals_true)

## change order of variables in data frame
dta_select_reg <- dta_select_reg %>% 
  select(predicted_equals_true, 
         predicted_coalition_stand, 
         everything())

## make age a factor variable
dta_select_reg <- dta_select_reg %>% 
  mutate(age_categorical = case_when(age_stand %in% 16:25 ~ "16-25",
                                     age_stand %in% 26:35 ~ "26-35",
                                     age_stand %in% 36:45 ~ "36-45",
                                     age_stand %in% 46:55 ~ "46-55",
                                     age_stand %in% 56:65 ~ "56-65",
                                     age_stand > 66 ~  "66 and older"))



dta_select_reg <- dta_select_reg %>% 
  mutate(predicted_equals_true_factor = as.factor(predicted_equals_true)) 

## get only the prediction(s) by respondents
dta_select_reg_predictions <- dta_select_reg %>% 
  filter(predicted_coalition_dummy == TRUE) 

dta_select_reg_predictions$predicted_equals_true_numeric <- as.numeric(dta_select_reg_predictions$predicted_equals_true)

dta_select_reg_predictions$incumbent_coalition <- as.factor(dta_select_reg_predictions$incumbent_coalition)

## check whether (at least one) of the predictions equals the true prediction
dta_select_reg_predictions <- dta_select_reg_predictions %>% 
  ungroup() %>% 
  group_by(election_id, respondent_id) %>% 
  mutate(predicted_equals_true = ifelse(sum(predicted_equals_true_numeric) >= 1, 1, 0)) %>% 
  mutate(proportion_predicted_true = predicted_equals_true / number_predicted_coalitions_unique) %>% 
  arrange(-number_predicted_coalitions_unique, respondent_id)


dta_select_reg_predictions <- dta_select_reg_predictions %>% 
  group_by(respondent_id) %>% 
  mutate(n = n())

## recode and relevel variables
dta_select_reg_predictions$interest_campaign_stand[dta_select_reg_predictions$interest_campaign_stand=='Don’t know'] <- NA
dta_select_reg_predictions$interest_campaign_stand[dta_select_reg_predictions$interest_campaign_stand=='No information'] <- NA

dta_select_reg_predictions$interest_campaign_stand_factor <- factor(dta_select_reg_predictions$interest_campaign_stand)

dta_select_reg_predictions$interest_campaign_stand_factor <- factor(dta_select_reg_predictions$interest_campaign_stand_factor,
                                                                    levels = c('No interest at all',
                                                                               'Not much interest',
                                                                               'Medium interest',
                                                                               'Strong interest',
                                                                               'Very strong interest'))

dta_select_reg_predictions$gender_stand <- factor(dta_select_reg_predictions$gender_stand,
                                                  levels = c("Male", "Female"))


## filter only those respondents who made one prediction
dta_one_prediction <- dta_select_reg_predictions %>% 
  filter(number_predicted_coalitions_unique == 1) %>% 
  filter(!is.na(wish_coalition_dummy)) ## only respondents/elections with valid wishful dummy variable


## run regression models
glm_individual <- glmer(predicted_equals_true_numeric ~
                          wish_coalition_dummy + 
                          interest_campaign_stand_factor + 
                          education_stand + 
                          gender_stand +
                          age_categorical + 
                          income_num + 
                          predict_coalition_type +
                          (1 | election_id),
                        data = dta_one_prediction,
                        family = binomial(link = "logit"))


glm_context <- glmer(predicted_equals_true_numeric ~
                       probability_proportion_surplus + 
                       incumbent_coalition +
                       federal_state_dummy +
                       #pedersen_vol + 
                       predict_coalition_type +
                       (1 | election_id),
                     data = dta_one_prediction,
                     family = binomial(link = "logit"))



glm_individual_context <- glmer(predicted_equals_true_numeric ~
                                  wish_coalition_dummy + 
                                  interest_campaign_stand_factor + 
                                  education_stand + 
                                  gender_stand +
                                  age_categorical + 
                                  income_num + 
                                  probability_proportion_surplus + 
                                  incumbent_coalition +
                                  federal_state_dummy +
                                  pedersen_vol + 
                                  (1 | predict_coalition_type),
                                data = dta_one_prediction,
                                family = binomial(link = "logit"))


glm_individual_context_morethanone <- glmer(predicted_equals_true_numeric ~
                                              wish_coalition_dummy + 
                                              interest_campaign_stand_factor + 
                                              education_stand + 
                                              gender_stand +
                                              age_categorical + 
                                              income_num + 
                                              probability_proportion_surplus + 
                                              incumbent_coalition +
                                              federal_state_dummy +
                                              pedersen_vol + 
                                              number_predicted_coalitions_unique +
                                              (1 | predict_coalition_type),
                                            data = dta_select_reg_predictions,
                                            family = binomial(link = "logit"))


## check names of coefficient to create better names
screenreg(list(glm_individual, 
               glm_context, 
               glm_individual_context, 
               glm_individual_context_morethanone))


coefnames_predictcorrect <- c("Desired Government",
                              "Interest in the Election: Not much (ref.: No interest at all)",
                              "Interest in the Election: Medium",
                              "Interest in the Election: Strong",
                              "Interest in the Election: Very strong",
                              "Education: No A-Levels",
                              "Gender: Female",
                              "Age: 26-35 (ref.: 18-25)",
                              "Age: 36-45",
                              "Age: 46-55",
                              "Age: 56-65",
                              "Age: 66 and older",
                              "Income",
                              "Prediction Question: Coalitions - Continuous (ref.: Coa. - Binary)",
                              "Prediction Question: Government Parties - Binary",
                              "Probability of a Majority",
                              "Incumbent Government",
                              "State Election",
                              "Pedersen's Volatility Index",
                              "Number of Predicted Coalitions by Respondent")


## create custom GOF names
gof_names <- c("AIC", "BIC", "Log likelihood",
               "Num obs.", "Num groups: Election",
               "Num groups: Coalition Prediction Question")


## Table 3 ----
htmlreg(list(glm_individual, 
             glm_context, 
             glm_individual_context, 
             glm_individual_context_morethanone),
        custom.coef.names = coefnames_predictcorrect,
        include.var = FALSE,
        single.row = FALSE,
        caption = "",
        omit.coef = "(Intercept)",
        custom.gof.names = gof_names,
        file = "tab_03.htm")


wordreg(list(glm_individual, 
             glm_context, 
             glm_individual_context, 
             glm_individual_context_morethanone),
        custom.coef.names = coefnames_predictcorrect,
        include.var = FALSE,
        single.row = FALSE,
        caption = "",
        omit.coef = "(Intercept)",
        custom.gof.names = gof_names,
        file = "tab_03.doc")
