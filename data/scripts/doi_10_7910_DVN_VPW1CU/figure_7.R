########### FIGURE 7 ############

### This code produces figure 7 in the main text
### I first show the code needed to directly create the model results, and then to create the coefficient plots
### The data comes from the file "gha_hh_panel.RData"

##################

## Clear environment

rm(list=ls())

## Packages

library(dplyr)
library(tidyr)
library(fixest)
library(ggplot2)

## Data
load("gh_hh_panel.RData")

## Clean data for plot

combined_hh_rural1_desc <- combined_hh_rural_analysis %>%
  mutate(urban_contact_week_ever = ifelse(urban_contact_week_ever > 0, 1, 0)) %>%
  group_by(wave) %>%
  summarise(
    mean_mobile = mean(mobile, na.rm = TRUE) * 100,
    urban_contact_week_ever = mean(urban_contact_week_ever, na.rm = TRUE) * 100
  ) %>%
  pivot_longer(cols = -wave, names_to = "variable", values_to = "value") %>%
  distinct(value, .keep_all = TRUE) %>%
  mutate(type = case_when(
    variable == "mean_mobile" ~ "a) Mobile access\n",
    variable == "urban_contact_week_ever" ~ "b) Non-physical urban contact\n"
  ))

manual_labels <- list(
  "a) Mobile access\n" = c("60", "65", "80", "85%"),
  "b) Non-physical urban contact\n" = c("10", "15", "20", "25%")
)


## Make and print plot

ggplot(data = combined_hh_rural1_desc, aes(x=wave, y = value)) +
  facet_wrap(~type, scales="free_y") +
  geom_point() +
  geom_line() +
  scale_x_continuous(limits = c(0.9,3.1), breaks = c(1,2,3), 
                     labels = c("\nWave 1\n(2009/10)", 
                                "\nWave 2\n(2013/14)", 
                                "\nWave 3\n(2018/19)")) +
  ylab("Percentage of households\n") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
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
