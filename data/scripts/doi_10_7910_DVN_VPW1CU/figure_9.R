########### FIGURE 9 ############

### This code produces figure 9 in the main text
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

baseline_regression <- function(dependent_var) {
  formula <- as.formula(paste(dependent_var, "~ use_mobile + age + gender + education + vote2020_inc + region"))
  feols(formula, data = gha_survey_analysis)
}

contact_vars <- c("contact_urban_week", "phone_politics", 
                  "phone_econ", "phone_corruption")

contact_results <- lapply(contact_vars, baseline_regression)

## Convert results in a dataframe and clean names etc

tidy_model_result <- function(model_result) {
  broom::tidy(model_result, conf.int = TRUE) %>%
    dplyr::mutate(dependent_var = as.character(formula(model_result))[2])
}

contact_results_plot <- 
  map_df(contact_results, tidy_model_result, .id = "model") %>%
  filter(term == "use_mobile") %>%
  mutate(outcome = c("Contact urban \n(general) \n",
                     "Discuss \npolitics \n", 
                     "Discuss \neconomy \n",
                     "Discuss \ncorruption \n"), 
         type = ifelse(outcome == "Contact urban \n(general) \n", 
                       "a) Urban contact\n", "b) Sensitive discussion\n")) 

contact_results_plot <- rbind(contact_results_plot, contact_results_plot) %>%
  mutate(group = c(rep("95% CI", 4), rep("90% CI", 4))) %>%
  mutate(conf.low = ifelse(group == "95% CI", 
                           estimate - 1.96*std.error, 
                           estimate - 1.68*std.error),
         conf.high = ifelse(group == "95% CI", 
                            estimate + 1.96*std.error, 
                            estimate + 1.68*std.error))

### Make and print coefficient plot

contact_plot <- ggplot(contact_results_plot, 
                       aes(y=factor(outcome,
                                    levels = c("Discuss \npolitics \n", 
                                               "Discuss \neconomy \n",
                                               "Discuss \ncorruption \n",
                                               "Contact urban \n(general) \n")),
                           x=estimate, col=group)) +
  facet_grid(~factor(type,
                     levels = c("a) Urban contact\n", 
                                "b) Sensitive discussion\n")), 
             scales = "free", space="free_y") +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  geom_vline(xintercept=0) +
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high, height=0),
                 position = position_dodge(width=0.03)) +
  geom_point(col="grey30") +
  scale_colour_manual(values = c("darkred", "darkgrey")) +
  scale_x_continuous(limits = c(0, 0.335),
                     breaks = c(0, 0.1, 0.2, 0.3), 
                     labels = c("\n0", "\n10", "\n20", "\n30")) +
  theme_minimal() +
  xlab("\nMarginal effect of mobile use (percent standard deviation)") +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
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

contact_plot


