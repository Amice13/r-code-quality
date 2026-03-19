########### FIGURE 10 ############

### This code produces figure 10 in the main text
### This includes running relevant regression analyses and using them to create a coefficient plot
### The analyses uses original survey data provided by the author, saved in the file "gha_survey.RData"

##################

## Clear environment

rm(list=ls())

## Load packages

library(fixest)
library(dplyr)
library(ggplot2)
library(purrr)

## Load data

load("gha_survey.RData")

## Run regressions 

#### a) with mean trust as outcome

trust_regression <- function(ind_var) {
  formula <- as.formula(paste("new_mean_trust ~", ind_var, 
                              "+ age + gender + education + vote2020_inc + region"))
  feols(formula, data = gha_survey_analysis)
}

contact_vars2 <- c("use_mobile", "contact_urban_week", 
                   "phone_politics", "phone_econ", "phone_corruption")

trust_results <- lapply(contact_vars2, trust_regression)

#### b) with president trust as outcome


pres_regression <- function(ind_var) {
  formula <- as.formula(paste("trust_pres ~", ind_var, 
                              "+ age + gender + education + vote2020_inc + region"))
  feols(formula, data = gha_survey_analysis)
}

pres_results <- lapply(contact_vars2, pres_regression)

## Convert results in a dataframe and clean names etc

tidy_model_result <- function(model_result) {
  broom::tidy(model_result, conf.int = TRUE) %>%
    dplyr::mutate(dependent_var = as.character(formula(model_result))[2])
}

trust_results_plot <- 
  map_df(trust_results, tidy_model_result, .id = "model") %>%
  filter(term %in% contact_vars2) %>%
  mutate(iv = c("Mobile \nphone use ", 
                "Contact urban \n(general) ",
                "Discuss \npolitics ", 
                "Discuss \neconomy ",
                "Discuss \ncorruption "), 
         outcome = "a) Mean political trust\n") 

pres_results_plot <- 
  map_df(pres_results, tidy_model_result, .id = "model") %>%
  filter(term %in% contact_vars2) %>%
  mutate(iv = c("Mobile \nphone use ", 
                "Contact urban \n(general) ",
                "Discuss \npolitics ", 
                "Discuss \neconomy ",
                "Discuss \ncorruption "), 
         outcome = "b) Trust President\n") 

attitudes_plot_data <- rbind(trust_results_plot, pres_results_plot)

attitudes_plot_data <- rbind(attitudes_plot_data, attitudes_plot_data) %>%
  mutate(group = c(rep("95% CI", 10), rep("90% CI", 10))) %>%
  mutate(conf.low = ifelse(group == "95% CI", 
                           estimate - 1.96*std.error, 
                           estimate - 1.68*std.error),
         conf.high = ifelse(group == "95% CI", 
                            estimate + 1.96*std.error, 
                            estimate + 1.68*std.error))

## Make and print plot

attitude_plot <- ggplot(attitudes_plot_data, 
                        aes(y=factor(iv, 
                                     levels = c("Discuss \npolitics ", 
                                                "Discuss \neconomy ",
                                                "Discuss \ncorruption ",
                                                "Contact urban \n(general) ",
                                                "Mobile \nphone use ")),
                            x=estimate, col=group)) +
  facet_grid(~outcome, scales = "free_x") +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  geom_vline(xintercept=0) +
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high, height=0),
                 position = position_dodge(width=0.05)) +
  geom_point(col="grey30") +
  scale_colour_manual(values = c("darkred", "darkgrey")) +
  theme_minimal() +
  scale_x_continuous(limits = c(-0.25, 0.13), 
                     breaks = c(-0.2, -0.1, 0, 0.1),
                     labels = c("\n-20", "\n-10", "\n0", "\n10")) +
  xlab("\n Marginal effect (percent standard deviation)") +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.title.y = element_blank()) +
  theme(axis.line.x = element_line(size = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        axis.line.y = element_line(size = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        panel.spacing = unit(2, "lines"),
        axis.title = element_text(colour = "#000000"),
        axis.text = element_text(colour = "#000000"), 
        plot.title = element_text(colour = "#000000", face = "bold"),
        strip.text = element_text(size = 12))

attitude_plot

