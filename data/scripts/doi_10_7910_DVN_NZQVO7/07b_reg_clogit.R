####################################################################
####################################################################
## Replication Material
##
## Shaun Bowler, Gail McElroy, Stefan Müller:
## Voter Expectations of Government Formation in Coalition Systems: 
## The Importance of the Information Context.
## European Journal of Political Research

## File: 07b_reg_clogit.R

## See 000_description_replication_material.pdf for a detailed 
## overview of the required data and the outputs of this file.

####################################################################
####################################################################

## conditional logit models to predict the selected government option

library(stringr) # Simple, Consistent Wrappers for Common String Operations, CRAN v1.4.0
library(texreg) # Conversion of R Regression Output to LaTeX or HTML Tables, CRAN v1.37.5
library(broom) # Convert Statistical Objects into Tidy Tibbles, CRAN v0.7.2
library(survival) # Survival Analysis, CRAN v3.1-12
library(dplyr) # A Grammar of Data Manipulation, CRAN v1.0.2
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics, CRAN v3.3.2

## load ggplot2 scheme
source("function_theme_base.R")

## load respondent-choice-level dataset
dta_combined <- readRDS("data_reg_full.rds") 

table(dta_combined$coalition_option_stand)

## remove missing responses (important)
dta_select_reg <- dta_combined %>% 
  filter(!is.na(predicted_coalition_stand))


## standardise continuous variables (Gelman et al. 2020: 186-187)
## within each election

## write function for proposed standarisation
standardize_2sd <- function(var) {
  (var - mean(var, na.rm = TRUE)) / (2 * sd(var, na.rm = TRUE))
}


dta_select_reg <- dta_select_reg %>% 
  group_by(election_id) %>% 
  mutate(stand_probability_proportion_surplus = standardize_2sd(probability_proportion_surplus)) %>% 
  mutate(stand_ref_perc_of_all_coalitions = standardize_2sd(ref_perc_of_all_coalitions)) %>%   
  mutate(stand_distance_lr_coa_self_no_missing = standardize_2sd(distance_lr_coa_self_no_missing)) %>% 
  mutate(stand_distance_coa_2_parties_no_missing = standardize_2sd(distance_coa_2_parties_no_missing)) %>% 
  mutate(stand_distance_ws_lr_coa_self = standardize_2sd(distance_ws_lr_coa_self)) %>% 
  mutate(stand_distance_coa_ws_2_most_extreme_parties = standardize_2sd(distance_coa_ws_2_most_extreme_parties)) 


## create strata for respondent ID by choice for conditional logit models
dta_select_reg <- dta_select_reg %>% 
  mutate(respondent_id_per_choice = paste(respondent_id, predicted_coalition_stand))


## recode coalitions to a nicer format
## recode cdu to CSU because there was only one CDU or CSU single-party government (Bavaria 2013)

recode_coalition <- c("'cdu'='CSU'; 'cdu_fdp'='CDU, FDP'; 'cdu_fdp_greens'='CDU, FDP, Greens';
                      'cdu_greens'='CDU, Greens'; 'cdu_spd'='CDU, SPD';
                      'cdu_spd_greens'='CDU, SPD, Greens'; 'spd_fdp_greens'='SPD, FDP, Greens'; 'spd_greens'='SPD, Greens'; 'spd_greens_ssw'='SPD, Greens, SSW'; 'spd_left'='SPD, Left'; 'spd_left_greens'='SPD, Left, Greens';'spd'='SPD';'spd_fdp'='SPD, FDP'; 'cdu_afd'='CDU, AfD'")

dta_select_reg$coalition_after_election_factor <-  car::recode(dta_select_reg$coalition_after_election, recode_coalition)


## create nicer election ID variable
dta_select_reg <- dta_select_reg %>% 
  mutate(election_id_coalition = paste0(election_id_english, " (",coalition_after_election_factor, ")"))



## check which elections containt the "wishful thinking" dummy
dta_select_reg_sum_wish <- dta_select_reg %>% 
  group_by(election_id) %>% 
  mutate(wish_coalition_dummy_logical = as.logical(wish_coalition_dummy)) %>% 
  summarise(has_wish_coalition_dummy = sum(wish_coalition_dummy_logical, na.rm = TRUE)) %>% 
  mutate(has_wish_coalition_dummy_logical = ifelse(has_wish_coalition_dummy > 0, TRUE, FALSE)) %>% 
  ungroup()

## merge information on wishful thinking dummy to the main dataset
dta_select_reg <- left_join(dta_select_reg, dta_select_reg_sum_wish, by = "election_id") 


## filter State elections
dta_select_reg_land <- dta_select_reg %>% 
  filter(election_type == "State Election")

## filter federal elections
dta_select_reg_federal <- dta_select_reg %>% 
  filter(election_type == "Federal Election")

## number of observations
nrow(dta_select_reg_federal)
nrow(dta_select_reg_land)
nrow(dta_select_reg)


## get overview of predicted coalitions
table(dta_select_reg$predicted_coalition_stand)


## only filter respondents who predicted one coalition for the subset of
## state ections and federal elections
dta_select_reg_unique_land <- dta_select_reg_land %>% 
  filter(number_predicted_coalitions_unique == 1)

dta_select_reg_unique_federal <- dta_select_reg_federal %>% 
  filter(number_predicted_coalitions_unique == 1)



## prepare dataset for regression models that only include surveys 
## which include the wishful thinking dummy variable constructed 
## in previous scripts

dta_select_reg_loop_wishful <- dta_select_reg %>% 
  filter(has_wish_coalition_dummy_logical > 0) %>% 
  mutate(election_id_coalition = paste0(election_id_english, " (",coalition_after_election_factor, ")"))

table(dta_select_reg_loop_wishful$election_id_coalition)


election_char_wishful <- unique(dta_select_reg_loop_wishful$election_id_coalition)

## 18 elections contain the wishful thinking dummy variable
length(election_char_wishful)

## use standardized continuous variables

## Figure 04 -----

## run loop with conditional logit model for each election 
## where we have an indicator of the desired government
## empty data frame
df_clogit_election_wishful_stand <- data.frame()


for (i in election_char_wishful) {
  df_model <- broom::tidy(survival::clogit(predicted_coalition_dummy ~ 
                                             wish_coalition_dummy + ## binary variable - no standardisation necessary
                                             stand_probability_proportion_surplus +
                                             stand_ref_perc_of_all_coalitions +
                                             stand_distance_coa_2_parties_no_missing + 
                                             strata(respondent_id), 
                                           data = filter(dta_select_reg_loop_wishful, 
                                                         election_id_coalition == i), 
                                           method = "efron", robust = TRUE)) %>% 
    mutate(model = i)
  
  df_clogit_election_wishful_stand <- rbind(df_clogit_election_wishful_stand, df_model)
  
}


df_clogit_election_wishful_stand <- df_clogit_election_wishful_stand %>% 
  mutate(term_print = car::recode(term, "'wish_coalition_dummyTRUE'='Desired\\nGovernment';
                                    'stand_probability_proportion_surplus'='Probability\\nof a Majority';
                                    'stand_distance_coa_2_parties_no_missing'='Perceived Distance\\nBetween Parties';
                                    'stand_ref_perc_of_all_coalitions'='Coalition Signals\\nin Newspapers'")) 

df_clogit_election_wishful_stand$term_print <- factor(df_clogit_election_wishful_stand$term_print,
                                                      levels = c("Desired\nGovernment",
                                                                 "Probability\nof a Majority",
                                                                 "Perceived Distance\nBetween Parties",
                                                                 "Coalition Signals\nin Newspapers", 
                                                                 "Incumbent"))


df_clogit_election_wishful_stand <- df_clogit_election_wishful_stand %>% 
  mutate(election_type = ifelse(str_detect(model, "Federal"), "Federal", "State")) 

## create and save Figure 4
ggplot(df_clogit_election_wishful_stand, aes(x = forcats::fct_rev(model), y = estimate, #colour = term, 
                                             ymin = estimate - 1.96 * robust.se,
                                             ymax = estimate + 1.96 * robust.se)) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "red", size = 1.05) +
  geom_linerange() + 
  geom_point(size = 1.5) +
  coord_flip() +
  facet_grid(election_type~term_print, scales = "free_y", space = "free_y") +
  labs(x = NULL, y = "Standardised coefficients (and 95% CIs)") +
  theme(legend.position = "none",
        panel.border=element_rect(colour = "black",
                                  size = 0.5, fill=NA),
        strip.text.x = element_text(size = 12, face = "bold"),
        panel.background = element_blank())
ggsave(filename = "fig_04.pdf", 
       width = 11.3, height = 7)
ggsave(filename = "fig_04.png", 
       width = 11.3, height = 7)


## Figure A05 ----

## now repeat the model but use all 22 elections and use the difference 
## between the respondent and the coalition option (based on the respondent's
## left-right self-placement and her left-right placement of the parties


dta_select_reg_loop_all <- dta_select_reg 


election_char_all <- unique(dta_select_reg_loop_all$election_id_coalition)

## check which elections are not included
setdiff(election_char_all, election_char_wishful)


## empty data frame for coefficients from models with standardised coefficients

df_clogit_election_all_stand <- data.frame()
for (i in election_char_all) {
  model_df_all <- broom::tidy(survival::clogit(predicted_coalition_dummy ~ 
                                                 stand_probability_proportion_surplus +
                                                 stand_ref_perc_of_all_coalitions +
                                                 stand_distance_lr_coa_self_no_missing + 
                                                 stand_distance_coa_2_parties_no_missing + 
                                                 strata(respondent_id), 
                                               data = filter(dta_select_reg_loop_all, election_id_coalition == i), 
                                               method = "efron", robust = TRUE)) %>% 
    mutate(model = i)
  
  df_clogit_election_all_stand <- rbind(df_clogit_election_all_stand, model_df_all)
  
}

## recode coefficient names
df_clogit_election_all_stand <- df_clogit_election_all_stand %>% 
  mutate(term_print = car::recode(term, "'stand_probability_proportion_surplus'='Probability\\nof a Majority';
                                    'stand_distance_coa_2_parties_no_missing'='Perceived Distance\\nBetween Parties';
                                    'stand_distance_lr_coa_self_no_missing'='Ideological Distance\\nGov. and Respondent';
                                    'stand_ref_perc_of_all_coalitions'='Coalition Signals\\nin Newspapers'")) 

df_clogit_election_all_stand$term_print <- factor(df_clogit_election_all_stand$term_print,
                                                  levels = c("Ideological Distance\nGov. and Respondent",
                                                             "Probability\nof a Majority",
                                                             "Perceived Distance\nBetween Parties",
                                                             "Coalition Signals\nin Newspapers"))


## check whether election is a federal or state election
df_clogit_election_all_stand <- df_clogit_election_all_stand %>% 
  mutate(election_type = ifelse(str_detect(model, "Federal"), "Federal", "State")) 


## create and save Figure A05
ggplot(df_clogit_election_all_stand, aes(x = forcats::fct_rev(model), 
                                         y = estimate,  
                                         ymin = estimate - 1.96 * robust.se,
                                         ymax = estimate + 1.96 * robust.se)) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "red", size = 1.05) +
  geom_linerange() + 
  geom_point(size = 1.5) +
  coord_flip() +
  facet_grid(election_type~term_print, 
             scales = "free_y", space = "free_y") +
  labs(x = NULL, y = "Standardised coefficients (and 95% CIs)") +
  theme(legend.position = "none",
        panel.border=element_rect(colour = "black",
                                  size = 0.5, fill=NA),
        strip.text.x = element_text(size = 12, face = "bold"))
ggsave(filename = "fig_a05.pdf", 
       width = 12, 
       height = 7)



## Figure A07 ----

## repeat model but now add interaction between common coalition and probability of majority
## empty data frame for coefficients from models with standardised coefficients

df_clogit_election_int_all_stand <- data.frame()

## remove NRW 2010 from this analysis because the model does not converge

election_char_all_int <- election_char_all[!election_char_all %in% "North Rhine-Westphalia 2010 (SPD, Greens)"]

for (i in election_char_all_int) {
  model_df_all <- broom::tidy(survival::clogit(predicted_coalition_dummy ~ 
                                                 stand_probability_proportion_surplus * coalition_formed_at_least_5_times + 
                                                 ## stand_ref_perc_of_all_coalitions +
                                                 stand_distance_lr_coa_self_no_missing + 
                                                 stand_distance_coa_2_parties_no_missing + 
                                                 strata(respondent_id), 
                                               data = filter(dta_select_reg_loop_all, 
                                                             election_id_coalition == i), 
                                               method = "efron", robust = TRUE)) %>% 
    mutate(model = i)
  
  df_clogit_election_int_all_stand <- bind_rows(df_clogit_election_int_all_stand, model_df_all)
  
}

## recode coefficient names
df_clogit_election_int_all_stand <- df_clogit_election_int_all_stand %>%
  mutate(term_print = car::recode(term, "'stand_probability_proportion_surplus'='Probability\\nof a Majority';
                                    'stand_distance_coa_2_parties_no_missing'='Perceived Distance\\nBetween Parties';
                                    'stand_distance_lr_coa_self_no_missing'='Ideological Distance\\nGov. and Respondent';
                                    'stand_ref_perc_of_all_coalitions'='Coalition Signals\\nin Newspapers';
                                    'coalition_formed_at_least_5_timesTRUE'='Common Coalition'"))

df_clogit_election_int_all_stand <- df_clogit_election_int_all_stand %>% 
  mutate(term_print = str_replace_all(term_print, 'stand_probability_proportion_surplus:coalition_formed_at_least_5_timesTRUE',
                                      'Prob Majority x Common'))

df_clogit_election_int_all_stand$term_print <- factor(df_clogit_election_int_all_stand$term_print,
                                                      levels = c("Ideological Distance\nGov. and Respondent",
                                                                 "Probability\nof a Majority",
                                                                 "Perceived Distance\nBetween Parties",
                                                                 ## "Coalition Signals\nin Newspapers",
                                                                 "Common Coalition",
                                                                 "Prob Majority x Common"))


## check whether election is a federal or state election
df_clogit_election_int_all_stand <- df_clogit_election_int_all_stand %>% 
  mutate(election_type = ifelse(str_detect(model, "Federal"), "Federal", "State")) 


ggplot(df_clogit_election_int_all_stand, aes(x = forcats::fct_rev(model), 
                                             y = estimate, #colour = term, 
                                             ymin = estimate - 1.96 * robust.se,
                                             ymax = estimate + 1.96 * robust.se)) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "red", size = 1.05) +
  geom_linerange() + 
  geom_point(size = 1.5) +
  coord_flip() +
  facet_grid(election_type~term_print, scales = "free_y", space = "free_y") +
  labs(x = NULL, y = "Standardised coefficients (and 95% CIs)") +
  theme(legend.position = "none",
        panel.border=element_rect(colour = "black",
                                  size = 0.5, fill=NA),
        strip.text.x = element_text(size = 12, face = "bold"),
        panel.background = element_blank())
ggsave(filename = "fig_a07.pdf", 
       width = 15, 
       height = 7)


## Figure A10 ----

## now repeat the models for all state elections with Wordscores estimates of 
## the distance between the two most extreme coalition parties and 
## the distance between the respondent and the average left-right position of a coalition
## based on the Wordscores estimates

## select only State elections
dta_select_reg_loop_wordscores <- dta_select_reg %>%  
  filter(!election_id %in% c("Federal Election 2009",
                             "Federal Election 2013",
                             "Federal Election 2017")) 

election_char_wordscores <- unique(dta_select_reg_loop_wordscores$election_id_coalition)

## check which elections are not included
setdiff(election_char_all, election_char_wordscores)

## loop for regression models by election with standardised coefficients
df_clogit_election_wordscores_stand <- data.frame()

for (i in election_char_wordscores) {
  df_ws <- broom::tidy(survival::clogit(predicted_coalition_dummy ~ 
                                          stand_probability_proportion_surplus +
                                          stand_ref_perc_of_all_coalitions +
                                          stand_distance_ws_lr_coa_self + 
                                          stand_distance_coa_ws_2_most_extreme_parties + 
                                          strata(respondent_id), 
                                        data = filter(dta_select_reg_loop_wordscores, 
                                                      election_id_coalition == i), 
                                        method = "efron", robust = TRUE)) %>% 
    mutate(model = i)
  df_clogit_election_wordscores_stand <- rbind(df_clogit_election_wordscores_stand, df_ws)
  
}


df_clogit_election_wordscores_stand <- df_clogit_election_wordscores_stand %>% 
  mutate(term_print = car::recode(term, "'stand_probability_proportion_surplus'='Probability\\nof a Majority';
                                    'stand_distance_coa_ws_2_most_extreme_parties'='Distance\\nBetween Parties\\n(Wordscores)';
                                    'stand_distance_ws_lr_coa_self'='Ideological Distance\\nGov. and Respondent\\n(Wordscores)';
                                    'stand_ref_perc_of_all_coalitions'='Coalition Signals\\nin Newspapers'")) 

df_clogit_election_wordscores_stand$term_print <- factor(df_clogit_election_wordscores_stand$term_print,
                                                         levels = c("Ideological Distance\nGov. and Respondent\n(Wordscores)",
                                                                    "Probability\nof a Majority",
                                                                    "Distance\nBetween Parties\n(Wordscores)",
                                                                    "Coalition Signals\nin Newspapers"))


## create and save Figure A10
ggplot(df_clogit_election_wordscores_stand, aes(x = forcats::fct_rev(model), y = estimate, #colour = term, 
                                                ymin = estimate - 1.96 * robust.se,
                                                ymax = estimate + 1.96 * robust.se)) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "red", size = 1.05) +
  geom_linerange() + 
  geom_point(size = 1.5) +
  coord_flip() +
  facet_grid(~term_print, scales = "free_y", space = "free_y") +
  labs(x = NULL, y = "Standardised coefficients (and 95% CIs)") +
  theme(legend.position = "none",
        panel.border=element_rect(colour = "black",
                                  size = 0.5, fill=NA),
        strip.text.x = element_text(size = 12, face = "bold"),
        panel.background = element_blank())
ggsave(filename = "fig_a10.pdf", 
       width = 12, 
       height = 6)




## run additional model (pooled across elections) 
## for the Supporting Information

## Table A03 ----

## run various models for (pooled) state elections
## state elections only
clogit_full_state_1 <- clogit(predicted_coalition_dummy ~ 
                                wish_coalition_dummy +
                                stand_probability_proportion_surplus +
                                incumbent_coalition +
                                stand_distance_coa_2_parties_no_missing + 
                                cluster(election_id) +
                                strata(respondent_id), data = dta_select_reg_land, 
                              method = "efron", robust = TRUE)


## repeat model but use the distance variable instead of the 
## wishful thinking dummy
clogit_full_state_2 <- clogit(predicted_coalition_dummy ~ 
                                stand_probability_proportion_surplus +
                                incumbent_coalition +
                                stand_distance_coa_2_parties_no_missing +
                                stand_distance_lr_coa_self_no_missing + 
                                cluster(election_id) +
                                strata(respondent_id), 
                              data = dta_select_reg_land, 
                              method = "efron", robust = TRUE)

## add news coverage variable
clogit_full_state_3 <- clogit(predicted_coalition_dummy ~ 
                                wish_coalition_dummy +
                                stand_probability_proportion_surplus +
                                incumbent_coalition +
                                stand_distance_coa_2_parties_no_missing + 
                                stand_ref_perc_of_all_coalitions +
                                cluster(election_id) +
                                strata(respondent_id), 
                              data = dta_select_reg_land,
                              method = "efron", robust = TRUE)


## rerun model but only use respondents who made a single prediction
clogit_full_state_4 <- clogit(predicted_coalition_dummy ~ 
                                wish_coalition_dummy +
                                stand_probability_proportion_surplus +
                                incumbent_coalition +
                                stand_distance_coa_2_parties_no_missing +
                                stand_ref_perc_of_all_coalitions +
                                cluster(election_id) +
                                strata(respondent_id), 
                              data = dta_select_reg_unique_land, 
                              method = "efron", robust = TRUE)


## get overview of all regression models
screenreg(list(clogit_full_state_1,
               clogit_full_state_2,
               clogit_full_state_3,
               clogit_full_state_4))


htmlreg(list(clogit_full_state_1,
             clogit_full_state_2,
             clogit_full_state_3,
             clogit_full_state_4),
        custom.coef.names = c("Desired Government",
                              "Probability of a Majority (stand.)",
                              "Incumbent Government",
                              "Perceived Distance Between Parties (stand.)",
                              "Ideological Distance Government and Respondent (stand.)",
                              "Coalition Signals in Newspapers (stand.)"),
        caption = "",
        single.row = FALSE,
        file = "tab_a03.htm")



## Table A04 ----

## repeat models for federal elections

clogit_full_federal_1 <- clogit(predicted_coalition_dummy ~ 
                                  wish_coalition_dummy +
                                  stand_probability_proportion_surplus +
                                  incumbent_coalition +
                                  stand_distance_coa_2_parties_no_missing + 
                                  cluster(election_id) +
                                  strata(respondent_id), data = dta_select_reg_federal, 
                                method = "efron", robust = TRUE)


## repeat model but use the distance variable instead of the 
## wishful thinking dummy
clogit_full_federal_2 <- clogit(predicted_coalition_dummy ~ 
                                  stand_probability_proportion_surplus +
                                  incumbent_coalition +
                                  stand_distance_coa_2_parties_no_missing +
                                  stand_distance_lr_coa_self_no_missing + 
                                  cluster(election_id) +
                                  strata(respondent_id), 
                                data = dta_select_reg_federal, 
                                method = "efron", robust = TRUE)


## add news coverage variable
clogit_full_federal_3 <- clogit(predicted_coalition_dummy ~ 
                                  wish_coalition_dummy +
                                  stand_probability_proportion_surplus +
                                  incumbent_coalition +
                                  stand_distance_coa_2_parties_no_missing + 
                                  stand_ref_perc_of_all_coalitions +
                                  cluster(election_id) +
                                  strata(respondent_id), 
                                data = dta_select_reg_federal,
                                method = "efron", robust = TRUE)


## rerun model but only use respondents who made a single prediction
clogit_full_federal_4 <- clogit(predicted_coalition_dummy ~ 
                                  wish_coalition_dummy +
                                  stand_probability_proportion_surplus +
                                  incumbent_coalition +
                                  stand_distance_coa_2_parties_no_missing +
                                  stand_ref_perc_of_all_coalitions +
                                  cluster(election_id) +
                                  strata(respondent_id), 
                                data = dta_select_reg_unique_federal, 
                                method = "efron", robust = TRUE)


## get overview of all regression models
screenreg(list(clogit_full_federal_1,
               clogit_full_federal_2,
               clogit_full_federal_3,
               clogit_full_federal_4))


htmlreg(list(clogit_full_federal_1,
             clogit_full_federal_2,
             clogit_full_federal_3,
             clogit_full_federal_4),
        custom.coef.names = c("Desired Government",
                              "Probability of a Majority (stand.)",
                              "Incumbent Government",
                              "Perceived Distance Between Parties (stand.)",
                              "Ideological Distance Government and Respondent (stand.)",
                              "Coalition Signals in Newspapers (stand.)"),
        caption = "",
        single.row = FALSE,
        file = "tab_a04.htm")


