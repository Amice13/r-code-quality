### THIS SCRIPT OUTPUTS FIGURES S26, S27, S28, AND S29,
### WHICH PLOT THE DISTRIBUTIONS OF FATHERS' AND MOTHER'S AGE, EDUCTION, MOVING STATUS AND INCOME. 
rm(list = ls())

## LOAD PACKAGES
require(sandwich)
require(lmtest)
require(lfe)
require(gtools)
require(tidyverse)
require(stargazer)
require(reshape)
require(hrbrthemes)
require(scales)
hrbrthemes::import_roboto_condensed()

## LOAD DATA

load('02-data/parental-vars-2000.Rdata')
load('02-data/parental-vars-2017.Rdata')

## COMBINE
df_all <- bind_rows(df_with_parental_vars_2000_selected,df_with_parental_vars_2017_selected)

axis_title_size <- 14

source("01-code/r_utils.R")

#### PLOT AGE DISTRIBUTION OF FATHER ####
dists_plot_age_pop <- df_all %>%
  ggplot(aes()) + facet_wrap(~sample) + 
  geom_histogram(aes(x = age_pop)) + 
  theme_shom() + 
  labs(x = "Age (Father)", y = 'Count')+
  scale_y_continuous(labels=comma)
ggsave(filename = "03-output/01-plots/FigS26a.jpeg",plot = dists_plot_age_pop, width = 9, height = 5, units = 'in', dpi=600)


#### PLOT AGE DISTRIBUTION OF MOTHER ####
dists_plot_age_mom <- df_all %>%
  ggplot(aes()) + facet_wrap(~sample) + 
  geom_histogram(aes(x = age_mom)) + 
  theme_shom() + 
  labs(x = "Age (Mother)", y = 'Count')+
  scale_y_continuous(labels=comma)
ggsave(filename = "03-output/01-plots/FigS26b.jpeg", plot = dists_plot_age_mom, width = 9, height = 5, units = 'in', dpi=600)

#### PLOT MOVING STATUS DISTRIBUTION OF FATHER ####
dists_plot_mover_pop <- df_all %>%
  mutate(mover_char = ifelse(mover == 1, "Moved","Did not Move")) %>%
  ggplot(aes()) + facet_wrap(~sample) + 
  geom_bar(aes(x = mover_char)) + 
  theme_shom() + 
  labs(x = "Mover Status (Father)", y = 'Count')+
  scale_y_continuous(labels=comma)
ggsave(filename = "03-output/01-plots/FigS28a.jpeg",plot = dists_plot_mover_pop, width = 9, height = 5, units = 'in', dpi=600)



#### PLOT MOVING STATUS DISTRIBUTION OF MOTHER ####
dists_plot_mover_mom <- df_all %>%
  mutate(mover_char = ifelse(mover == 1, "Moved","Did not Move")) %>%
  ggplot(aes()) + facet_wrap(~sample) + 
  geom_bar(aes(x = mover_char)) + 
  theme_shom() + 
  labs(x = "Mover Status (Mother)", y = 'Count')+
  scale_y_continuous(labels=comma)
ggsave(filename = "03-output/01-plots/FigS28b.jpeg", plot = dists_plot_mover_mom, width = 9, height = 5, units = 'in', dpi=600)


#### PLOT MOVING STATUS DISTRIBUTION OF FATHER ####
dists_plot_wage_pop <- df_all %>%
  ggplot(aes()) + facet_wrap(~sample) + 
  geom_histogram(aes(x = incwage)) + 
  theme_shom() + 
  labs(x = "Wage Income (Father)", y = 'Count') + 
  scale_x_continuous(trans = "log", labels =comma)+
  scale_y_continuous(labels=comma)
ggsave(filename = "03-output/01-plots/FigS29a.jpeg",plot = dists_plot_wage_pop, width = 9, height = 5, units = 'in', dpi=600)


#### PLOT MOVING STATUS DISTRIBUTION OF MOTHER ####
dists_plot_wage_mom <- df_all %>%
  ggplot(aes()) + facet_wrap(~sample) + 
  geom_histogram(aes(x = incwage)) + 
  theme_shom() + 
  labs(x = "Wage Income (Mother)", y = 'Count') + 
  scale_x_continuous(trans = "log", labels = comma)+
  scale_y_continuous(labels=comma)
ggsave(filename = "03-output/01-plots/FigS29b.jpeg",plot = dists_plot_wage_mom, width = 9, height = 5, units = 'in', dpi=600)


#### PLOT EDUCATION STATUS DISTRIBUTION OF FATHER ####
dists_plot_educ_pop <- df_all %>%
  mutate(educ_char = ifelse(completed_highschool_pop == 1, "Completed High School","Did Not Complete High School")) %>%
  drop_na(educ_char) %>%
  ggplot(aes()) + facet_wrap(~sample) + 
  geom_bar(aes(x = educ_char)) + 
  theme_shom() + 
  labs(x = "Education Status (Father)", y = 'Count')+
  scale_y_continuous(labels = comma)+
  theme(axis.text.x = element_text(angle = 45, vjust = .65))
ggsave(filename = "03-output/01-plots/FigS27a.jpeg", plot = dists_plot_educ_pop, width = 9, height = 5, units = 'in', dpi=600)


#### PLOT EDUCATION STATUS DISTRIBUTION OF MOTHER ####
dists_plot_educ_mom <- df_all %>%
  mutate(educ_char = ifelse(completed_highschool_mom == 1, "Completed High School","Did Not Complete High School")) %>%
  drop_na(educ_char) %>%
  ggplot(aes()) + facet_wrap(~sample) + 
  geom_bar(aes(x = educ_char)) + 
  theme_shom() + 
  labs(x = "Education Status (Mother)",y = 'Count')+
  scale_y_continuous(labels = comma)+
  theme(axis.text.x = element_text(angle = 45, vjust = .65))
ggsave(filename = "03-output/01-plots/FigS27b.jpeg",plot = dists_plot_educ_mom, width = 9, height = 5, units = 'in', dpi=600)




