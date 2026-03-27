###### Survey analysis #####

## This script produces results presented in section C of the supplementary materials
## First, it produces and saves full results tables (tables SM34-38)
## Then it produces the descriptive figure presented (figure SM10-11)
## Some of these results are used to produce figures 9-10 in the main text, the code for which is in figure_8.R and figure_9.R

## Set up and data

rm(list=ls())

library(dplyr)
library(fixest)
library(tidyr)
library(broom)
library(ggplot2)

load("gha_survey.RData")

## Run models

### Urban rural gaps in attitudes

trust_gap1 <- feols(new_mean_trust ~ urbrur, data=gha_survey_gaps)
trust_gap2 <- feols(new_mean_trust ~ urbrur + age + gender + education + 
                      vote2020_inc + region, data=gha_survey_gaps)

pres_gap1 <- feols(trust_pres ~ urbrur, data=gha_survey_gaps)
pres_gap2 <- feols(trust_pres ~ urbrur + age + gender + education + 
                     vote2020_inc + region, data=gha_survey_gaps)

### Contact/sensitive discussion

baseline_regression <- function(dependent_var) {
  formula <- as.formula(paste(dependent_var, "~ use_mobile + age + gender + education + vote2020_inc + region"))
  feols(formula, data = gha_survey_analysis)
}

contact_vars <- c("contact_urban_week", "phone_politics", 
                  "phone_econ", "phone_corruption")

contact_results <- lapply(contact_vars, baseline_regression)

### Attitudes (trust/president)

trust_regression <- function(ind_var) {
  formula <- as.formula(paste("new_mean_trust ~", ind_var, 
                              "+ age + gender + education + vote2020_inc + region"))
  feols(formula, data = gha_survey_analysis)
}

contact_vars2 <- c("use_mobile", "contact_urban_week", 
                   "phone_politics", "phone_econ", "phone_corruption")

trust_results <- lapply(contact_vars2, trust_regression)

pres_regression <- function(ind_var) {
  formula <- as.formula(paste("trust_pres ~", ind_var, 
                              "+ age + gender + education + vote2020_inc + region"))
  feols(formula, data = gha_survey_analysis)
}

pres_results <- lapply(contact_vars2, pres_regression)

### Remittances (baseline)

remit_regression1 <- function(dep_var) {
  formula <- as.formula(paste(dep_var, 
                              "~ domestic_remit_bin + age + gender + 
                              education + vote2020_inc + region"))
  feols(formula, data = gha_survey_analysis)
}

contact_vars3 <- c("use_mobile", "contact_urban_week", 
                   "phone_politics", "phone_econ", "phone_corruption")

remit_results1 <- lapply(contact_vars3, remit_regression1)

remit_regression2 <- function(dep_var) {
  formula <- as.formula(paste(dep_var, 
                              "~ domestic_remit_std + age + gender + 
                              education + vote2020_inc + region"))
  feols(formula, data = gha_survey_analysis)
}

remit_results2 <- lapply(contact_vars3, remit_regression2)

### Remittances (heterogeneity)

remit_het_regression <- function(ind_var) {
  formula <- as.formula(paste("new_mean_trust ~ 
                              i(domestic_remit_levels,", ind_var, 
                              ") + domestic_remit_levels + age + 
                              gender + education + vote2020_inc + region"))
  feols(formula, data = gha_survey_analysis)
}

contact_vars3 <- c("use_mobile", "contact_urban_week", 
                   "phone_politics", "phone_econ", "phone_corruption")

remit_het_results <- lapply(contact_vars3, remit_het_regression)

### Make and save tables

setFixest_dict(c(contact_urban_week = "Urban contact (weekly)", 
                 new_mean_trust = "Mean trust", 
                 trust_pres = "Trust President",
                 domestic_remit_bin = "Any (0-1)", 
                 domestic_remit_std = "Frequency (std)",
                 domestic_remit_levels = "Remittance group",
                 domestic_remit_levelsNever = "Remittance group (Never)",
                 domestic_remit_levelsRegular = "Remittance group (Regular)",
                 phone_politics = "Politics", 
                 phone_econ = "The economy", 
                 phone_corruption = "Corruption", 
                 use_mobile = "Mobile use",
                 bono = "Bono region (more remote)", 
                 urbrurUrban = "Urban (vs rural)"))

tab_SM34 <- etable(trust_gap1, trust_gap2, 
                          pres_gap1, pres_gap2,
                          title = "Baseline urban-rural gaps",
                          keep = "Urban",
                          signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
                          extralines=list("_^Controls"=c("No", "Yes", "No", "Yes")),
                          digits.stats = 3,
                          fontsize = "tiny",
                          convergence = FALSE,
                          tex=TRUE, 
                          notes = "Notes: Mean difference in trust between urban and rural respondents in original author survey in Ghana. Covariates include age, gender, education, previous vote choice, and region FEs.")


tab_SM35 <- etable(contact_results[1], contact_results[2], 
                          contact_results[3], contact_results[4],
                          title = "Mobile use on urban contact and politically sensitive discussion",
                          keep = "Mobile use",
                          signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
                          headers = list("^:_:Type" = list("General contact" = 1, "Discussion topics" = 3)),
                          digits = 3,
                          digits.stats = 3,
                          fontsize = "tiny",
                          convergence = FALSE,
                          tex=TRUE, 
                          notes = "Notes: Marginal effect of mobile phone use on urban contact and politically sensitive discussion, using rural respondents to original author survey in Ghana. Covariates include age, gender, education, previous vote choice, and region FEs.")
                   
tab_SM36 <- etable(trust_results[1], trust_results[2], 
                          trust_results[3], trust_results[4],
                          trust_results[5],
                          pres_results[1], pres_results[2], 
                          pres_results[3], pres_results[4],
                          pres_results[5],
                          title = "Mobile use, contact and discussion on political trust",
                          keep = c("Mobile use", 
                                   "Urban",
                                   "Politics", "The economy", "Corruption"),
                          signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
                          headers = list("^:_:Type" = list("General contact" = 2, "Discussion topics" = 3)),
                          digits = 3,
                          digits.stats = 3,
                          fontsize = "tiny",
                          convergence = FALSE,
                          tex=TRUE, 
                          notes = "Notes: Marginal effect of mobile phone use, urban contact and politically sensitive discussion on political trust. Data from rural respondents to original author survey in Ghana. Covariates include age, gender, education, previous vote choice, and region FEs.")
                   
                   
tab_SM37 <- etable(remit_results1[1], remit_results2[1],
                          remit_results1[2], remit_results2[2],
                          remit_results1[3], remit_results2[3],
                          remit_results1[4], remit_results2[4],
                          remit_results1[5], remit_results2[5],
                          title = "Financial remittances and contact with relatives",
                          keep = c("Any", "Freq"),
                          signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
                          headers = list("^:_:Type" = list("General contact and phone use" = 4, 
                                                           "Discussion topics" = 6)),
                          digits = 3,
                          digits.stats = 3,
                          fontsize = "tiny",
                          convergence = FALSE,
                          tex=TRUE, 
                          notes = "Notes: Marginal effect of financial remittances on urban contact and politically sensitive discussion. Data from rural respondents to original author survey in Ghana. Covariates include age, gender, education, previous vote choice, and region FEs.")                   

tab_SM38 <- etable(remit_het_results[1], remit_het_results[2], 
                          remit_het_results[3], remit_het_results[4], 
                          remit_het_results[5],
                          keep = c("Remit"),
                          signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
                          order = c("Mobile", "Urban", "Politics", 
                                    "The", "Corruption"),  # Interaction terms first, baseline term last
                          title = "Heterogeneity by regularity of financial support",
                          headers = list("^:_:Type" = list("Phone use" = 1, 
                                                           "General contact" = 1, 
                                                           "Discussion topics" = 3)),
                          digits.stats = 3,
                          fontsize = "tiny",
                          convergence = FALSE,
                          tex = TRUE, 
                          notes = "Notes: Marginal effect of mobile phone use, urban contact and politically sensitive discussion on political trust, conditional on receipt of financial remittances from relatives. Data from rural respondents to original author survey in Ghana. Covariates include age, gender, education, previous vote choice, and region FEs.")                 
                   
#### Save tables

save(tab_SM34, tab_SM35, 
     tab_SM36, tab_SM37, 
     tab_SM38, file = "survey_tabs.RData")
                   
#### Code to reproduce Figures SM10-SM11

data_plot_use <- gha_survey_plots %>%
  select(resp_id, use_phone_call_week,
         use_phone_sms_week, 
         use_phone_socmedia_week, 
         use_phone_momo_week,
         urbrur) %>%
  pivot_longer(cols = starts_with("use_phone_"), 
               names_to = "phone_use", 
               values_to = "value") %>%
  group_by(urbrur, phone_use) %>%
  summarise(fun = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(phone_use = ifelse(phone_use == "use_phone_call_week", "\nMaking calls", 
                            ifelse(phone_use == "use_phone_sms_week", "\nSend/receive text messages", 
                                   ifelse(phone_use == "use_phone_socmedia_week", "\nUsing social media", 
                                          "\nUsing mobile money"))))


fig_SM10 <- ggplot(data=data_plot_use, aes(x = factor(phone_use,
                                          levels = c(
                                            "\nMaking calls", 
                                            "\nSend/receive text messages", 
                                            "\nUsing social media", 
                                            "\nUsing mobile money")),
                               y = fun, 
                               col = urbrur,
                               fill = urbrur, 
                               group = urbrur)) +
  geom_col(position = position_dodge(), col="black") +
  ylab("Proportion using\nat least weekly\n") +
  xlab("\nType of phone use") +
  scale_y_continuous(limits = c(0,1), 
                     breaks = c(0,.25,.5,.75,1), 
                     labels = c("0%", "25%", "50%", "75%", "100%")) +
  scale_fill_manual(values = c("darkred", "darkgrey")) +
  scale_colour_manual(values = c("darkred", "darkgrey")) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        axis.title.x = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank()) +
  theme(axis.line.x = element_line(size = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        axis.line.y = element_line(size = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        panel.spacing = unit(2, "lines"),
        axis.title = element_text(colour = "#000000"),
        axis.text = element_text(colour = "#000000"), 
        plot.title = element_text(colour = "#000000", face = "bold"))

data_plot_rel <- gha_survey_plots %>%
  select(resp_id,
         contact_community_week, contact_rural_week,
         contact_urban_week, contact_abroad_week,
         urbrur) %>%
  pivot_longer(cols = starts_with("contact_"), 
               names_to = "rel_contact", 
               values_to = "value") %>%
  group_by(urbrur, rel_contact) %>%
  summarise(fun = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(rel_contact = ifelse(rel_contact == "contact_abroad_week", "\nAbroad", 
                              ifelse(rel_contact == "contact_urban_week", "\nUrban area", 
                                     ifelse(rel_contact == "contact_rural_week", "\nRural area", "\nLocal community"))))

fig_SM11 <- ggplot(data=data_plot_rel, aes(x = factor(rel_contact,
                                          levels = c(
                                            "\nLocal community",
                                            "\nRural area",
                                            "\nUrban area",
                                            "\nAbroad")),
                               y = fun, 
                               col = urbrur,
                               fill = urbrur, 
                               group = urbrur)) +
  geom_col(position = position_dodge(), col="black") +
  ylab("Proportion calling\nat least weekly\n") +
  xlab("\nRelative's location") +
  scale_y_continuous(limits = c(0,1), 
                     breaks = c(0,.25,.5,.75,1), 
                     labels = c("0%", "25%", "50%", "75%", "100%")) +
  scale_fill_manual(values = c("darkred", "darkgrey")) +
  scale_colour_manual(values = c("darkred", "darkgrey")) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        axis.title.x = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank()) +
  theme(axis.line.x = element_line(size = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        axis.line.y = element_line(size = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        panel.spacing = unit(2, "lines"),
        axis.title = element_text(colour = "#000000"),
        axis.text = element_text(colour = "#000000"), 
        plot.title = element_text(colour = "#000000", face = "bold"))


                   
                   
                   