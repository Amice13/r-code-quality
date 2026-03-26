####################################################################
####################################################################
## Replication Material
##
## Shaun Bowler, Gail McElroy, Stefan Müller:
## Voter Expectations of Government Formation in Coalition Systems: 
## The Importance of the Information Context.
## European Journal of Political Research

## File: 07d_reg_dontknow.R

## See 000_description_replication_material.pdf for a detailed 
## overview of the required data and the outputs of this file.

####################################################################
####################################################################

## predict don't know responses for coalition prediction questions

## load packages
library(dplyr) # A Grammar of Data Manipulation, CRAN v1.0.2
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics, CRAN v3.3.2
library(lme4) # Linear Mixed-Effects Models using 'Eigen' and S4, CRAN v1.1-23
library(texreg) # Conversion of R Regression Output to LaTeX or HTML Tables, CRAN v1.37.5
library(rms) # Regression Modeling Strategies, CRAN v6.0-1

## load ggplot2 scheme
source("function_theme_base.R")

## load respondent-choice level dataset
dta_combined <- readRDS("data_reg_full.rds")

## recode variables required for the analysis
dta_select_reg <- dta_combined %>% 
    mutate(election_id = as.factor(election_id),
           gender_stand = as.factor(gender_stand),
           interest_campaign_stand = as.factor(interest_campaign_stand),
           incumbent_reelected = as.factor(incumbent_reelected),
           income_stand = as.factor(income_stand),
           education_stand = as.factor(education_stand),
           election_type = as.factor(election_type),
           afd_dummy_strong = as.factor(ifelse(year >= 2015, "2015-2017 (strong)", "2010-2014 (weak/before foundation)")),
           wish_coalition_dummy = as.factor(wish_coalition_dummy),
           year = as.numeric(year)) 

## relevel interest in campaign
dta_select_reg$interest_campaign_stand <- factor(dta_select_reg$interest_campaign_stand,
                                                 levels = c("No interest at all",
                                                            "Not much interest",
                                                            "Medium interest",
                                                            "Strong interest",
                                                            "Very strong interest"))


## create a dummy variable checking whether a respondent predicted a coalition
dat_dk <- dta_select_reg %>% 
    mutate(dontknow = ifelse(is.na(predicted_coalition_stand), 1, 0)) %>% 
    select(respondent_id, dontknow, interest_campaign_stand,
           election_id, election_id_english,  education_stand,
           year, predict_coalition_type, income_stand, election_type, 
           gender_stand, lr_self, pedersen_vol) %>% 
    unique()

length(unique(dat_dk$respondent_id))
nrow(dat_dk)


## bootstrap "don't now" proportions
boot_dontknow <- dat_dk %>%
    group_by(election_id_english, predict_coalition_type, election_type) %>%
    do(data.frame(rbind(smean.cl.boot(.$dontknow, na.rm = TRUE, conf.int = 0.95))))

## change order of factor levels
boot_dontknow$predict_coalition_type <- factor(boot_dontknow$predict_coalition_type,
                                               levels = c("Coalitions: binary", 
                                                          "Government parties: binary",
                                                          "Coalitions: continuous"))

## Figure A04 ----
## plot proportion of don't know responses per election
ggplot(boot_dontknow, aes(x = reorder(election_id_english, Mean), 
                          y = Mean, ymin = Lower, ymax = Upper,
                          shape = predict_coalition_type,
                          colour = predict_coalition_type)) + 
    geom_pointrange(size = 0.8) +
    scale_colour_manual(name = "Question type", 
                        values = c("black", "grey70", "grey40")) + 
    scale_shape_discrete(name = "Question type") +
    coord_flip() +
    scale_y_continuous(labels = scales::percent, 
                       breaks = c(seq(0, 0.30, 0.05))) + 
    labs(x = NULL, 
         y = "Percentage of 'Don't know/No answer'\nfor predicted government (and 95% CIs)") +
    theme(legend.position = c(0.6, 0.2))
ggsave("fig_a04.pdf", width = 8, height = 6)


prop.table(table(dat_dk$dontknow, useNA = "always"))

dat_dk$year <- as.factor(dat_dk$year)
dat_dk$gender_stand <- factor(dat_dk$gender_stand, levels = c("Male", "Female"))

## run regression models predicting don't know responses
reg_dk1 <- glmer(dontknow ~ 
                     predict_coalition_type + 
                     election_type + 
                     (1 | election_id),  
                 family = binomial, data = dat_dk)


reg_dk2 <- glmer(dontknow ~ 
                     predict_coalition_type + 
                     election_type +
                     interest_campaign_stand + 
                     gender_stand +
                     education_stand +
                     (1 | election_id),
                 family = binomial, data = dat_dk)


## Table A01 ----
## print regression table to check names of coefficients
screenreg(list(reg_dk1, reg_dk2))


htmlreg(list(reg_dk1, reg_dk2),
        custom.coef.names = c("Prediction Question: Coalitions - Continuous (ref.: Coa. - Binary)",
                              "Prediction Question: Government Parties - Binary",
                              "State election",
                              "Interest in the Election: Not much (ref.: No interest at all)",
                              "Interest in the Election: Medium",
                              "Interest in the Election: Strong",
                              "Interest in the Election: Very strong",
                              "Female",
                              "Education: No A-Levels"),
        caption = "",
        ci.force = FALSE,
        single.row = TRUE,
        include.var = FALSE,
        omit.coef = "(Intercept)",
        custom.gof.names = c("AIC", "BIC", "Log Likelihood", 
                             "Num obs.", "Num groups: Elections"),
        file = "tab_a01.htm")

