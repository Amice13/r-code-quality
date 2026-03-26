##### Household panel analysis ########

## This script produces household panel analysis results presented in section B of the supplementary materials
## First, it produces and saves full results tables (tables SM26-33)
## Then it produces the descriptive figure presented (figure SM9)
## Some of these results are used to produce figure 8 in the main text, the code for which is in figure_8.R

## Set up and data

rm(list=ls())

library(dplyr)
library(fixest)
library(tidyr)
library(broom)
library(ggplot2)

load("gh_hh_panel.RData")

## Function for running household FE model

mobile_twfe <- function(data, dep_vars) {
  results <- list()

  for (dep_var in dep_vars) {
    formula <- as.formula(paste(dep_var, "~ mobile | FPrimary + wave"))
    model_name <- paste0("model_", dep_var)
    results[[model_name]] <- feols(formula, data = data)
  }
  
  return(results)
}

## Save two lists with names of the outcome variables we want to use

#### Main outcomes
dep_vars <- c("urban_contact_month_ever", "urban_contact_month_prop", "urban_contact_month_count",
              "urban_contact_week_ever", "urban_contact_week_prop", "urban_contact_week_count",
              "urban_contact_day_ever", "urban_contact_day_prop", "urban_contact_day_count",
              "urban_visits_month_ever", "urban_visits_month_prop", "urban_visits_month_count",
              "urban_visits_week_ever", "urban_visits_week_prop", "urban_visits_week_count",
              "urban_visits_day_ever", "urban_visits_day_prop", "urban_visits_day_count")

#### Alternative outcomes using differnet urban definition
dep_vars_alt <- c("urban_contact_month_ever_alt", "urban_contact_month_prop_alt", 
                  "urban_contact_month_count_alt",
                  "urban_contact_week_ever_alt", "urban_contact_week_prop_alt", 
                  "urban_contact_week_count_alt",
                  "urban_contact_day_ever_alt", "urban_contact_day_prop_alt", "urban_contact_day_count_alt",
                  "urban_visits_month_ever_alt", "urban_visits_month_prop_alt", 
                  "urban_visits_month_count_alt",
                  "urban_visits_week_ever_alt", "urban_visits_week_prop_alt", "urban_visits_week_count_alt",
                  "urban_visits_day_ever_alt", "urban_visits_day_prop_alt", "urban_visits_day_count_alt")

## Run models 

#### Baseline: Mobile phone access and contact (section B.3.1)

contact_results <- mobile_twfe(data = combined_hh_rural_analysis, 
                               dep_vars = dep_vars)

#### Robustness I: urban only Accra or Kumasi (section B.4)

contact_results_alt <- mobile_twfe(data = combined_hh_rural_analysis, 
                               dep_vars = dep_vars_alt)

#### Robustness II: waves 1 and 2 only (section B.4)

contact_results_w12 <- mobile_twfe(data = combined_hh_rural1_12, 
                                   dep_vars = dep_vars)

#### Robustness III: waves 2 and 3 only (section B.4)

contact_results_w23 <- mobile_twfe(data = combined_hh_rural1_23, 
                                   dep_vars = dep_vars)

##### Make and save tables


## Set dictionary for tables

setFixest_dict(c(mobile = "Mobile", 
                 FPrimary = "Household",
                 wave = "Survey wave",
                 urban_contact_day_ever = "Ever",
                 urban_contact_day_count = "Count",
                 urban_contact_day_prop = "Portion",
                 urban_contact_week_ever = "Ever",
                 urban_contact_week_count = "Count",
                 urban_contact_week_prop = "Portion",
                 urban_contact_month_ever = "Ever",
                 urban_contact_month_count = "Count",
                 urban_contact_month_prop = "Portion",
                 urban_visits_day_ever = "Ever",
                 urban_visits_day_count = "Count",
                 urban_visits_day_prop = "Portion",
                 urban_visits_week_ever = "Ever",
                 urban_visits_week_count = "Count",
                 urban_visits_week_prop = "Portion",
                 urban_visits_month_ever = "Ever",
                 urban_visits_month_count = "Count",
                 urban_visits_month_prop = "Portion",
                 urban_contact_day_ever_alt = "Ever",
                 urban_contact_day_count_alt = "Count",
                 urban_contact_day_prop_alt = "Portion",
                 urban_contact_week_ever_alt = "Ever",
                 urban_contact_week_count_alt = "Count",
                 urban_contact_week_prop_alt = "Portion",
                 urban_contact_month_ever_alt = "Ever",
                 urban_contact_month_count_alt = "Count",
                 urban_contact_month_prop_alt = "Portion",
                 urban_visits_day_ever_alt = "Ever",
                 urban_visits_day_count_alt = "Count",
                 urban_visits_day_prop_alt = "Portion",
                 urban_visits_week_ever_alt = "Ever",
                 urban_visits_week_count_alt = "Count",
                 urban_visits_week_prop_alt = "Portion",
                 urban_visits_month_ever_alt = "Ever",
                 urban_visits_month_count_alt = "Count",
                 urban_visits_month_prop_alt = "Portion"))

## Make latex tables (SM26 to SM33)

tab_SM26 <- etable(contact_results[1], contact_results[2], contact_results[3],
       contact_results[4], contact_results[5], contact_results[6],
       contact_results[7], contact_results[8], contact_results[9],
       title = "Mobile phone access and non-physical interaction with urban relatives",
       keep = "Mobile",
       signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
       digits = 3,
       digits.stats = 2,
       fitstat = c("n", "r2"),
       headers = list("^:_:Type" = list("Monthly" = 3, "Weekly" = 3, "Daily" = 3)),
       fontsize = "tiny",
       tex=TRUE,
       label = "tab:mobile-to-urban-contact-tabs-I", 
       notes = "Notes: Coefficients represent the marginal effect of a household gaining access to a mobile device between survey waves on non-physical contact with urban relatives. Data from rural households in the Ghana Socioeconomic Panel.")

tab_SM27 <- etable(contact_results[10], contact_results[11], contact_results[12],
       contact_results[13], contact_results[14], contact_results[15],
       contact_results[16], contact_results[17], contact_results[18],
       title = "Mobile phone access and in-person visits from urban relatives",
       keep = "Mobile",
       signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
       digits = 3,
       digits.stats = 2,
       fitstat = c("n", "r2"),
       headers = list("^:_:Type" = list("Monthly" = 3, "Weekly" = 3, "Daily" = 3)),
       fontsize = "tiny",
       tex=TRUE,
       label = "tab:mobile-to-urban-contact-tabs-II", 
       notes = "Notes: Coefficients represent the marginal effect of a household gaining access to a mobile device between survey waves on in-person visits from urban relatives. Data from rural households in the Ghana Socioeconomic Panel.")

tab_SM28 <- etable(contact_results_alt[1], contact_results_alt[2], contact_results_alt[3],
       contact_results_alt[4], contact_results_alt[5], contact_results_alt[6],
       contact_results_alt[7], contact_results_alt[8], contact_results_alt[9],
       title = "Mobile phone access and digital interaction with urban relatives (Accra or Kumasi)",
       keep = "Mobile",
       signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
       digits = 3,
       digits.stats = 2,
       fitstat = c("n", "r2"),
       headers = list("^:_:Type" = list("Monthly" = 3, "Weekly" = 3, "Daily" = 3)),
       fontsize = "tiny",
       tex=TRUE, 
       notes = "Notes: Impact of household phone access on non-physical contact with urban relatives, using narrower definition of urban (if relative lives in Accra or Kumasi only).")

tab_SM29 <- etable(contact_results_alt[10], contact_results_alt[11], contact_results_alt[12],
       contact_results_alt[13], contact_results_alt[14], contact_results_alt[15],
       contact_results_alt[16], contact_results_alt[17], contact_results_alt[18],
       title = "Mobile phone access and in-person visits from urban relatives (Accra or Kumasi)",
       keep = "Mobile",
       signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
       digits = 3,
       digits.stats = 2,
       fitstat = c("n", "r2"),
       headers = list("^:_:Type" = list("Monthly" = 3, "Weekly" = 3, "Daily" = 3)),
       fontsize = "tiny",
       tex=TRUE, 
       notes = "Notes: Impact of household phone access on in-person visits from urban relatives, using narrower definition of urban (if relative lives in Accra or Kumasi only).")

tab_SM30 <- etable(contact_results_w12[1], contact_results_w12[2], contact_results_w12[3],
       contact_results_w12[4], contact_results_w12[5], contact_results_w12[6],
       contact_results_w12[7], contact_results_w12[8], contact_results_w12[9],
       title = "Mobile phone access and digital interaction with urban relatives (waves 1 and 2 only)",
       keep = "Mobile",
       signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
       digits = 3,
       digits.stats = 2,
       fitstat = c("n", "r2"),
       headers = list("^:_:Type" = list("Monthly" = 3, "Weekly" = 3, "Daily" = 3)),
       fontsize = "tiny",
       tex=TRUE,
       label = "tab:mobile-to-urban-contact-tabs-I", 
       notes = "Notes: Replicates table SM26 using only waves 1 and 2.")

tab_SM31 <- etable(contact_results_w12[10], contact_results_w12[11], contact_results_w12[12],
       contact_results_w12[13], contact_results_w12[14], contact_results_w12[15],
       contact_results_w12[16], contact_results_w12[17], contact_results_w12[18],
       title = "Mobile phone access and in-person visits from urban relatives (waves 1 and 2 only)",
       keep = "Mobile",
       signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
       digits = 3,
       digits.stats = 2,
       fitstat = c("n", "r2"),
       headers = list("^:_:Type" = list("Monthly" = 3, "Weekly" = 3, "Daily" = 3)),
       fontsize = "tiny",
       tex=TRUE,
       label = "tab:mobile-to-urban-contact-tabs-II", 
       notes = "Notes: Replicates table SM27 using only waves 1 and 2.")

tab_SM32 <- etable(contact_results_w23[1], contact_results_w23[2], contact_results_w23[3],
       contact_results_w23[4], contact_results_w23[5], contact_results_w23[6],
       contact_results_w23[7], contact_results_w23[8], contact_results_w23[9],
       title = "Mobile phone access and digital interaction with urban relatives (waves 2 and 3 only)",
       keep = "Mobile",
       signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
       digits = 3,
       digits.stats = 2,
       fitstat = c("n", "r2"),
       headers = list("^:_:Type" = list("Monthly" = 3, "Weekly" = 3, "Daily" = 3)),
       fontsize = "tiny",
       tex=TRUE,
       label = "tab:mobile-to-urban-contact-tabs-I", 
       notes = "Notes: Replicates table SM26 using only waves 2 and 3.")

tab_SM33 <- etable(contact_results_w23[10], contact_results_w23[11], contact_results_w23[12],
       contact_results_w23[13], contact_results_w23[14], contact_results_w23[15],
       contact_results_w23[16], contact_results_w23[17], contact_results_w23[18],
       title = "Mobile phone access and in-person visits from urban relatives (waves 2 and 3 only)",
       keep = "Mobile",
       signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
       digits = 3,
       digits.stats = 2,
       fitstat = c("n", "r2"),
       headers = list("^:_:Type" = list("Monthly" = 3, "Weekly" = 3, "Daily" = 3)),
       fontsize = "tiny",
       tex=TRUE,
       label = "tab:mobile-to-urban-contact-tabs-II", 
       notes = "Notes: Replicates table SM26 using only waves 2 and 3.")

#### Save tables

save(tab_SM26, tab_SM27, 
     tab_SM28, tab_SM29, 
     tab_SM30, tab_SM31,
     tab_SM32, tab_SM33, file = "hh_panel_tabs.RData")

#### Code to reproduce Figure SM9

urbrur_desc <- combined_hh_rural_analysis %>%
  group_by(wave) %>%
  mutate(wave = ifelse(wave == 1, "Wave 1",
                       ifelse(wave == 2, "Wave 2", 
                              "Wave 3")))

fig_SM9 <- ggplot(data=urbrur_desc, aes(x=hh_count_urban, group = as.factor(wave))) +
  geom_histogram(aes(fill=as.factor(wave)), position = "dodge") +
  theme_minimal() +
  theme(legend.title = element_blank(), 
        legend.position = "bottom",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c("black", "darkgrey", "darkblue")) +
  ylab("Count") +
  xlab("\nNumber of urban relatives named by household") +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), 
        axis.title.y = element_blank()) +
  theme(axis.line.x = element_line(linewidth = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        axis.line.y = element_line(linewidth = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        panel.spacing = unit(2, "lines"),
        axis.title = element_text(colour = "#000000"),
        axis.text = element_text(colour = "#000000"), 
        plot.title = element_text(colour = "#000000", face = "bold"))

fig_SM9 ## Prints result




