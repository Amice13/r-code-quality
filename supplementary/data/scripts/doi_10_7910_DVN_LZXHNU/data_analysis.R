###### COMPARATIVE PUBLIC ATTITUDES ABOUT DRONE STRIKES ###### ######
###### DATA ANALYSIS ###### DATA ANALYSIS ###### DATA ANALYSIS ######

###### Authors: Diletta Alparone, Jaroslaw Kantorowicz & Graig R. Klein

###### LOAD PACKAGES

#install.packages("tidyverse")
library(tidyverse)
#install.packages("tidyverse")
library(sjPlot)
#install.packages("broom.helpers")
library(broom.helpers)
#install.packages("ggsci")
library(ggsci)
#install.packages("ggpubr")
library(ggpubr)
#install.packages("effsize")
library(effsize)
#install.packages("apa")
library(apa)
#install.packages("psych")
library(psych)
#install.packages("vtable")
library(vtable)
#install.packages("stargazer")
library(stargazer)
#install.packages("ggpubr")
library(ggpubr)

###### LOAD DATA

drones_df <- read_csv("drones_full.csv")

names(drones_df)

###### RECLASSIFY VARIABLES TO FACTORS

purrr::map_df(drones_df, class)

drones_df$age_cat <- as.factor(drones_df$age_cat)

table(drones_df$age_cat)

drones_df$education_cat <- as.factor(drones_df$education_cat)

table(drones_df$education_cat)

drones_df$gender_cat <- as.factor(drones_df$gender_cat)

table(drones_df$gender_cat)

drones_df$region_cat <- as.factor(drones_df$region_cat)

table(drones_df$region_cat)

drones_df$drones_treatments <- as.factor(drones_df$drones_treatments)

table(drones_df$drones_treatments)

drones_df$wave <- as.factor(drones_df$wave)

table(drones_df$wave)

###### ANALYSIS MAIN RESULTS

results_main <- lm(formula = drones_outcome ~ drones_treatments * wave, 
                   data = drones_df,
                   weights = weight)

table_main_results <- tidy_all_effects(results_main)

table_main_results <- table_main_results %>% 
                      tidyr::separate(term, c("Treatment", "Wave"), ":")

results_combined <- lm(formula = drones_outcome ~ drones_treatments, 
                       data = drones_df)

table_results_combined <- tidy_all_effects(results_combined)

table_results_combined <- table_results_combined %>% 
                          rename(Treatment = term) %>% mutate(Wave = "Pooled")

combined_table <- bind_rows(table_main_results, table_results_combined)

combined_table$Treatment <- factor(combined_table$Treatment,
                                   levels = c("Control",
                                              "Drones",
                                              "Drones + booster",
                                              "Proportionality",
                                              "Distinction"))

combined_table$Sample <- factor(combined_table$Wave,
                                   levels = c("Italy 1",
                                              "Italy 2",
                                              "Poland",
                                              "Germany",
                                              "Pooled"))

ggplot(data = combined_table) +
  theme_sjplot2() +
  geom_pointrange(mapping = aes(x = Treatment, 
                                y = estimate,
                                ymin = conf.low,
                                ymax = conf.high,
                                group = Sample,
                                color = Sample,
                                shape = Sample),
                  position = position_dodge(width = 0.5)) +
  theme(legend.position = "bottom",
        legend.margin=margin(t=-10),
        axis.title.y = element_text(vjust = 2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.6),
        panel.grid.minor.y = element_line(size = 0.6, 
                                          linetype = "dashed")) +
  labs(x = "", y = "Support for intervetion", color = "Sample") +
  ylim(3,5) +
  scale_color_manual(values = c("black", "gray20", "gray40", "gray60", "maroon")) +
  scale_shape_manual(values = c(15, 17, 4, 23, 19)) +
  geom_bracket(xmin = 1.75, 
               xmax = 3.25, 
               y.position = 4.95,
               label = "Drones:compare~with~control", 
               type = "expression",
               tip.length = c(0.05, 0.05),
               color = "gray50") +
  geom_bracket(xmin = 3.75, 
               xmax = 5.25, 
               y.position = 4.7,
               label = "Legal~reminders:compare~with~drones", 
               type = "expression",
               tip.length = c(0.05, 0.05),
               color = "gray50")

# FIGURE 1 IN THE PAPER

ggsave("main_results.jpg",
       width = 10,
       height = 6,
       scale = 0.7,
       dpi = 500)

###### ANALYSIS MEDIATION

drones_df_med <- drones_df[drones_df$drones_treatments=="Control" |
                           drones_df$drones_treatments=="Drones" |
                           drones_df$drones_treatments=="Drones + booster",]

drones_df_med$drones_treatments <- as.character(drones_df_med$drones_treatments)

drones_df_med$drones_treatments <- as.factor(drones_df_med$drones_treatments)

results_mediation <- lm(formula = drones_outcome_med ~ drones_treatments * wave, 
                        data = drones_df_med,
                        weights = weight)

table_mediation_results <- tidy_all_effects(results_mediation)

table_mediation_results <- table_mediation_results %>% 
                           tidyr::separate(term, c("Treatment", "Wave"), ":")

results_mediation_combined <- lm(formula = drones_outcome_med ~ drones_treatments, 
                                 data = drones_df_med)

table_results_mediation_combined <- tidy_all_effects(results_mediation_combined)

table_results_mediation_combined <- table_results_mediation_combined %>% 
                                    rename(Treatment = term) %>% mutate(Wave = "Pooled")

combined_mediation_table <- bind_rows(table_mediation_results, table_results_mediation_combined)

combined_mediation_table$Treatment <- factor(combined_mediation_table$Treatment,
                                   levels = c("Control",
                                              "Drones",
                                              "Drones + booster"))

combined_mediation_table$Sample <- factor(combined_mediation_table$Wave,
                                levels = c("Italy 1",
                                           "Italy 2",
                                           "Poland",
                                           "Germany",
                                           "Pooled"))

ggplot(data = combined_mediation_table) +
  theme_sjplot2() +
  geom_pointrange(mapping = aes(x = Treatment, 
                                y = estimate,
                                ymin = conf.low,
                                ymax = conf.high,
                                group = Sample,
                                color = Sample,
                                shape = Sample),
                  position = position_dodge(width = 0.5)) +
  theme(legend.position = "bottom",
        legend.margin=margin(t=-10),
        axis.title.y = element_text(vjust = 2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.6),
        panel.grid.minor.y = element_line(size = 0.6, 
                                          linetype = "dashed")) +
  labs(x = "", y = "Casualties perception", color = "Sample") +
  ylim(2.5,5.5) +
  scale_color_manual(values = c("black", "gray20", "gray40", "gray60", "maroon")) +
  scale_shape_manual(values = c(15, 17, 4, 23, 19))

# FIGURE 2 IN THE PAPER

ggsave("mediation_results.jpg",
       width = 8,
       height = 5,
       scale = 0.7,
       dpi = 500)

#### T-TESTS AND EFFECT SIZES

options(scipen=999)

compare_means(formula = drones_outcome ~ drones_treatments, 
              data = drones_df, 
              method = "t.test")

compare_means(formula = drones_outcome ~ drones_treatments, 
              data = drones_df, 
              method = "t.test")

compare_means(formula = drones_outcome ~ drones_treatments, 
              data = drones_df[drones_df$wave=="Poland",], 
              method = "t.test")

compare_means(formula = drones_outcome ~ drones_treatments, 
              data = drones_df[drones_df$wave=="Italy 2",], 
              method = "t.test")

compare_means(formula = drones_outcome ~ drones_treatments, 
              data = drones_df[drones_df$wave=="Italy 1",], 
              method = "t.test")

compare_means(formula = drones_outcome ~ drones_treatments, 
              data = drones_df[drones_df$wave=="Germany",], 
              method = "t.test")

psych::describeBy(drones_df$drones_outcome, group = drones_df$drones_treatments)

psych::describeBy(drones_df$drones_outcome_med, group = drones_df$drones_treatments)

drones_df$drones_treatments <- as.character(drones_df$drones_treatments)

apa::cohens_d(drones_df[drones_df$drones_treatments=="Control" |
                                 drones_df$drones_treatments=="Drones",],
              dv = drones_outcome, iv = drones_treatments)

apa::cohens_d(drones_df[drones_df$drones_treatments=="Control" |
                          drones_df$drones_treatments=="Drones + booster",],
              dv = drones_outcome, iv = drones_treatments)

apa::cohens_d(drones_df[drones_df$drones_treatments=="Drones + booster" |
                          drones_df$drones_treatments=="Proportionality",],
              dv = drones_outcome, iv = drones_treatments)

apa::cohens_d(drones_df[drones_df$drones_treatments=="Drones + booster" |
                          drones_df$drones_treatments=="Distinction",],
              dv = drones_outcome, iv = drones_treatments)


compare_means(formula = drones_outcome_med ~ drones_treatments, 
              data = drones_df_med, 
              method = "t.test")

#### DESCRIPTIVE STATISTICS: QUOTA DISTRIBUTION

### Italy 1

drones_df_italy_1 <- drones_df %>% filter(wave=="Italy 1")

sumtable(drones_df_italy_1, 
         vars = c("age_cat", 
                  "gender_cat", 
                  "education_cat", 
                  "region_cat"))

### Italy 2

drones_df_italy_2 <- drones_df %>% filter(wave=="Italy 2")

sumtable(drones_df_italy_2, 
         vars = c("age_cat", 
                  "gender_cat", 
                  "education_cat", 
                  "region_cat"))

### Germany

drones_df_germany <- drones_df %>% filter(wave=="Germany")

sumtable(drones_df_germany, 
         vars = c("age_cat", 
                  "gender_cat", 
                  "education_cat", 
                  "region_cat"))

### Poland

drones_df_poland <- drones_df %>% filter(wave=="Poland")

sumtable(drones_df_poland, 
         vars = c("age_cat", 
                  "gender_cat", 
                  "education_cat", 
                  "region_cat"))


#### REGRESSION TABLES FOR THE APPENDIX

table(drones_df$wave)

# MAIN RESULTS

italy1_reg_baseline <- lm(data = drones_df[drones_df$wave=="Italy 1",],
                        formula = drones_outcome ~ drones_treatments,
                        weights = weight)

italy1_reg_control <- lm(data = drones_df[drones_df$wave=="Italy 1",],
                         formula = drones_outcome ~ drones_treatments +
                           age_cat + gender_cat + education_cat,
                         weights = weight)

italy2_reg_baseline <- lm(data = drones_df[drones_df$wave=="Italy 2",],
                          formula = drones_outcome ~ drones_treatments,
                          weights = weight)

italy2_reg_control <- lm(data = drones_df[drones_df$wave=="Italy 2",],
                         formula = drones_outcome ~ drones_treatments +
                           age_cat + gender_cat + education_cat,
                         weights = weight)

poland_reg_baseline <- lm(data = drones_df[drones_df$wave=="Poland",],
                          formula = drones_outcome ~ drones_treatments,
                          weights = weight)

poland_reg_control <- lm(data = drones_df[drones_df$wave=="Poland",],
                         formula = drones_outcome ~ drones_treatments +
                           age_cat + gender_cat + education_cat,
                         weights = weight)

germany_reg_baseline <- lm(data = drones_df[drones_df$wave=="Germany",],
                          formula = drones_outcome ~ drones_treatments,
                          weights = weight)

germany_reg_control <- lm(data = drones_df[drones_df$wave=="Germany",],
                         formula = drones_outcome ~ drones_treatments +
                           age_cat + gender_cat + education_cat,
                         weights = weight)

pooled_reg_baseline <- lm(data = drones_df,
                           formula = drones_outcome ~ drones_treatments)

pooled_reg_control <- lm(data = drones_df,
                          formula = drones_outcome ~ drones_treatments +
                            age_cat + gender_cat + education_cat)

stargazer(italy1_reg_baseline, italy1_reg_control,
          align = TRUE, 
          type = "html",
          title = "Regression Table",
          style = "qje",
          out = "Italy_1_table.doc",
          dep.var.labels = c("Support for intervention"))

stargazer(italy2_reg_baseline, italy2_reg_control,
          align = TRUE, 
          type = "html",
          title = "Regression Table",
          style = "qje",
          out = "Italy_2_table.doc",
          dep.var.labels = c("Support for intervention"))

stargazer(poland_reg_baseline, poland_reg_control,
          align = TRUE, 
          type = "html",
          title = "Regression Table",
          style = "qje",
          out = "Poland_table.doc",
          dep.var.labels = c("Support for intervention"))

stargazer(germany_reg_baseline, germany_reg_control,
          align = TRUE, 
          type = "html",
          title = "Regression Table",
          style = "qje",
          out = "Germany_table.doc",
          dep.var.labels = c("Support for intervention"))

stargazer(pooled_reg_baseline, pooled_reg_control,
          align = TRUE, 
          type = "html",
          title = "Regression Table",
          style = "qje",
          out = "Pooled_table.doc",
          dep.var.labels = c("Support for intervention"))

# MEDIATION

italy1_reg_baseline <- lm(data = drones_df_med[drones_df_med$wave=="Italy 1",],
                          formula = drones_outcome_med ~ drones_treatments,
                          weights = weight)

italy1_reg_control <- lm(data = drones_df_med[drones_df_med$wave=="Italy 1",],
                         formula = drones_outcome_med ~ drones_treatments +
                           age_cat + gender_cat + education_cat,
                         weights = weight)

italy2_reg_baseline <- lm(data = drones_df_med[drones_df_med$wave=="Italy 2",],
                          formula = drones_outcome_med ~ drones_treatments,
                          weights = weight)

italy2_reg_control <- lm(data = drones_df_med[drones_df_med$wave=="Italy 2",],
                         formula = drones_outcome_med ~ drones_treatments +
                           age_cat + gender_cat + education_cat,
                         weights = weight)

poland_reg_baseline <- lm(data = drones_df_med[drones_df_med$wave=="Poland",],
                          formula = drones_outcome_med ~ drones_treatments,
                          weights = weight)

poland_reg_control <- lm(data = drones_df_med[drones_df_med$wave=="Poland",],
                         formula = drones_outcome_med ~ drones_treatments +
                           age_cat + gender_cat + education_cat,
                         weights = weight)

germany_reg_baseline <- lm(data = drones_df_med[drones_df_med$wave=="Germany",],
                           formula = drones_outcome_med ~ drones_treatments,
                           weights = weight)

germany_reg_control <- lm(data = drones_df_med[drones_df_med$wave=="Germany",],
                          formula = drones_outcome_med ~ drones_treatments +
                            age_cat + gender_cat + education_cat,
                          weights = weight)

pooled_reg_baseline <- lm(data = drones_df_med,
                          formula = drones_outcome_med ~ drones_treatments)

pooled_reg_control <- lm(data = drones_df_med,
                         formula = drones_outcome_med ~ drones_treatments +
                           age_cat + gender_cat + education_cat)

stargazer(italy1_reg_baseline, italy1_reg_control,
          align = TRUE, 
          type = "html",
          title = "Regression Table",
          style = "qje",
          out = "Italy_1_table_med.doc",
          dep.var.labels = c("Perception of casualties"))

stargazer(italy2_reg_baseline, italy2_reg_control,
          align = TRUE, 
          type = "html",
          title = "Regression Table",
          style = "qje",
          out = "Italy_2_table_med.doc",
          dep.var.labels = c("Perception of casualties"))

stargazer(poland_reg_baseline, poland_reg_control,
          align = TRUE, 
          type = "html",
          title = "Regression Table",
          style = "qje",
          out = "Poland_table_med.doc",
          dep.var.labels = c("Perception of casualties"))

stargazer(germany_reg_baseline, germany_reg_control,
          align = TRUE, 
          type = "html",
          title = "Regression Table",
          style = "qje",
          out = "Germany_table_med.doc",
          dep.var.labels = c("Perception of casualties"))

stargazer(pooled_reg_baseline, pooled_reg_control,
          align = TRUE, 
          type = "html",
          title = "Regression Table",
          style = "qje",
          out = "Pooled_table_med.doc",
          dep.var.labels = c("Perception of casualties"))

# BALANCE TESTING

drones_df$age_cat_num <- as.numeric(drones_df$age_cat)

table(drones_df$age_cat, drones_df$age_cat_num)

drones_df$gender_cat_num <- as.numeric(drones_df$gender_cat)

table(drones_df$gender_cat, drones_df$gender_cat_num)

drones_df$education_cat_num <- as.numeric(drones_df$education_cat)

table(drones_df$education_cat, drones_df$education_cat_num)

it1_age <- compare_means(data = drones_df[drones_df$wave=="Italy 1",],
              formula = age_cat_num ~ drones_treatments,
              weights = drones_df$weight,
              method = "t.test")

write.csv(it1_age, "it1_age.csv")

it2_age <- compare_means(data = drones_df[drones_df$wave=="Italy 2",],
              formula = age_cat_num ~ drones_treatments,
              weights = drones_df$weight,
              method = "t.test")

write.csv(it2_age, "it2_age.csv")

pol_age <- compare_means(data = drones_df[drones_df$wave=="Poland",],
              formula = age_cat_num ~ drones_treatments,
              weights = drones_df$weight,
              method = "t.test")
 
write.csv(pol_age, "pol_age.csv")

ger_age <- compare_means(data = drones_df[drones_df$wave=="Germany",],
              formula = age_cat_num ~ drones_treatments,
              weights = drones_df$weight,
              method = "t.test")

write.csv(ger_age, "ger_age.csv")

pooled_age <- compare_means(data = drones_df,
              formula = age_cat_num ~ drones_treatments,
              weights = drones_df$weight,
              method = "t.test")

write.csv(pooled_age, "pooled_age.csv")

ita1_gen <- compare_means(data = drones_df[drones_df$wave=="Italy 1",],
              formula = gender_cat_num ~ drones_treatments,
              weights = drones_df$weight,
              method = "t.test")

write.csv(ita1_gen, "ita1_gen.csv")

ita2_gen <- compare_means(data = drones_df[drones_df$wave=="Italy 2",],
              formula = gender_cat_num ~ drones_treatments,
              weights = drones_df$weight,
              method = "t.test")

write.csv(ita2_gen, "it2_gen.csv")

pol_gen <- compare_means(data = drones_df[drones_df$wave=="Poland",],
              formula = gender_cat_num ~ drones_treatments,
              weights = drones_df$weight,
              method = "t.test")

write.csv(pol_gen, "pol_gen.csv")

ger_gen <- compare_means(data = drones_df[drones_df$wave=="Germany",],
              formula = gender_cat_num ~ drones_treatments,
              weights = drones_df$weight,
              method = "t.test")

write.csv(ger_gen, "ger_gen.csv")

pooled_gen <- compare_means(data = drones_df,
              formula = gender_cat_num ~ drones_treatments,
              weights = drones_df$weight,
              method = "t.test")

write.csv(pooled_gen, "pooled_gen.csv")

ita1_edu <- compare_means(data = drones_df[drones_df$wave=="Italy 1",],
              formula = education_cat_num ~ drones_treatments,
              weights = drones_df$weight,
              method = "t.test")

write.csv(ita1_edu, "ita1_edu.csv")

ita2_edu <- compare_means(data = drones_df[drones_df$wave=="Italy 2",],
              formula = education_cat_num ~ drones_treatments,
              weights = drones_df$weight,
              method = "t.test")

write.csv(ita2_edu, "ita2_edu.csv")

pol_edu <- compare_means(data = drones_df[drones_df$wave=="Poland",],
              formula = education_cat_num ~ drones_treatments,
              weights = drones_df$weight,
              method = "t.test")

write.csv(pol_edu, "pol_edu.csv")

ger_edu <- compare_means(data = drones_df[drones_df$wave=="Germany",],
              formula = education_cat_num ~ drones_treatments,
              weights = drones_df$weight,
              method = "t.test")

write.csv(ger_edu, "ger_edu.csv")

pooled_edu <- compare_means(data = drones_df,
              formula = education_cat_num ~ drones_treatments,
              weights = drones_df$weight,
              method = "t.test")

write.csv(pooled_edu, "pooled_edu.csv")

####### END ####### END ####### END ####### END ####### END ####### END #######
####### END ####### END ####### END ####### END ####### END ####### END #######
####### END ####### END ####### END ####### END ####### END ####### END #######
