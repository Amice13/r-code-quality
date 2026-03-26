########### FIGURE 8 ############

### This code produces figure 8 in the main text
### I first run the regression models and save the results, before making the coefficient plot
### The data can be found in "gha_hh_panel.RData"

##################

## Clear environment

rm(list=ls())

## Packages

library(dplyr)
library(fixest)
library(ggplot2)
library(purrr)

## Load data
load("gh_hh_panel.RData")

## Run the regression specifications

mobile_twfe <- function(data, dep_vars) {
  results <- list()
  for (dep_var in dep_vars) {
    formula <- as.formula(paste(dep_var, "~ mobile | FPrimary + wave"))
    model_name <- paste0("model_", dep_var)
    results[[model_name]] <- feols(formula, data = data)
  }
  return(results)
}

dep_vars <- c("urban_contact_month_ever", "urban_contact_month_prop", "urban_contact_month_count",
              "urban_contact_week_ever", "urban_contact_week_prop", "urban_contact_week_count",
              "urban_contact_day_ever", "urban_contact_day_prop", "urban_contact_day_count",
              "urban_visits_month_ever", "urban_visits_month_prop", "urban_visits_month_count",
              "urban_visits_week_ever", "urban_visits_week_prop", "urban_visits_week_count",
              "urban_visits_day_ever", "urban_visits_day_prop", "urban_visits_day_count")

contact_results <- mobile_twfe(data = combined_hh_rural_analysis, 
                               dep_vars = dep_vars)


## Convert results to a dataframe and clean for plotting

tidy_model_result <- function(model_result) {
  broom::tidy(model_result, conf.int = TRUE) %>%
    dplyr::mutate(dependent_var = as.character(formula(model_result))[2])
}

contact_results_plot <- 
  map_df(contact_results, tidy_model_result, .id = "model") %>%
  mutate(outcome = c(rep("b) Non-physical contact\n", 9), rep("a) In-person visit\n", 9)),
         freq = rep(c(rep("\nMonthly", 3), 
                      rep("\nWeekly", 3), 
                      rep("\nDaily", 3)), 2),
         spec = rep(c("Ever", "Proportion", "Count"), 6),
         'Contact measure' = rep(c("At least one relative", 
                                   "Proportion of relatives", 
                                   "Number of relatives"), 6))

## Make and print coefficient plot in main text

ggplot(contact_results_plot, aes(y=freq, x=estimate, group=`Contact measure`, col=`Contact measure`)) +
  facet_wrap(~outcome, scales = "fixed") +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  scale_y_discrete(limits = c("\nMonthly", "\nWeekly", "\nDaily")) +
  geom_vline(xintercept=0) +
  scale_x_continuous(limits = c(-0.135, 0.31), breaks = c(-0.1, 0, 0.1, 0.2, 0.3), 
                     labels = c("-10", "0", "10", "20", "30")) +
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high, height=0),
                 position = position_dodge(width=0.5)) +
  geom_point(position = position_dodge(width=0.5), 
             col = "darkgrey") +
  theme_minimal() +
  coord_flip() +
  ylab("\nFrequency of contact") +
  xlab("Change in urban exposure\n(percent standard deviation)\n") +
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_point(aes(shape=`Contact measure`), position = position_dodge(width=0.5)) +
  scale_colour_manual(values = c("darkblue", "darkgrey", "black")) +
  geom_hline(yintercept=0, col="#000000") +
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


