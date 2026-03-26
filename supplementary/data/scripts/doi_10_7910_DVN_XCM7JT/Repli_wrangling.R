# --------------------------------------------
#
# Author: Shaka Y.J. Li
# Copyright (c) 
# Email: yl24m@fsu.edu
#
# Date: 2025-12-20
#
# Script Name: Replication code for wrangling data
#
# Script Description: 
# Replication code for the article:
# "From Killing Many to Targeting Few: Economic Growth and Selective Repression in Taiwan’s White Terror Period"
#
# Notes: 
# This is the code for wrangling data from Taiwan Transitional Justice Commission dataset
#
# --------------------------------------------


#################################
# Load data and required packages
################################
library(readxl)

################################################################################
# Data wrangling for death sentence across Chiang kai-shek and Chiang Ching-kuo
################################################################################

data <- read_excel("tjc.xlsx")

death_cases <- data |> 
  filter(f_death == "有") |> 
  select(f_death, d3_y)

death_cases$sentence_year <- death_cases$d3_y + 1911

death_cases_period <- death_cases %>%
  mutate(period = case_when(
    sentence_year >= 1949 & sentence_year <= 1970 ~ "Chiang Kai-shek",
    sentence_year > 1970 & sentence_year <= 1988 ~ "Chiang Ching-kuo",
    TRUE ~ "Other"
  )) %>%
  mutate(period = factor(period, levels = c("Chiang Kai-shek", "Chiang Ching-kuo", "Other")))

death_cases_period_all <- death_cases_period %>%
  filter(sentence_year >= 1949 & sentence_year <= 1988) %>%
  group_by(sentence_year, period) %>%
  summarise(total_charge_death = sum(d3_y, na.rm = TRUE), .groups = "drop")

write.csv(death_cases_period_all, "Data/dta_death_sentence.csv", row.names = FALSE)

a <- ggplot(death_cases_period_all, aes(x = sentence_year, y = total_charge_death, fill = period)) +
  geom_bar(stat = "identity", position = "stack") +  
  labs(x = "Year", y = "Total Death Sentences", fill = "Leader Period") + 
  theme_minimal() + 
  scale_fill_manual(values = c("Chiang Kai-shek" = "grey30", "Chiang Ching-kuo" = "grey70")) +
  theme(
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12)
  )

ggsave("figs/fig2.png", plot = a, width = 10, height = 6, dpi = 300)

###########################################################
# Data wrangling for sentence outcomes by leadership period
# (Chiang Kai-shek vs. Chiang Ching-kuo)
###########################################################

unique(data$f_dpriv)

death_subset <- data %>%
  mutate(
    prosecution   = if_else(d1_prosec == "有", 1, 0),
    f_death_bin   = if_else(f_death   == "有", 1, 0),
    f_term_bin    = if_else(f_term    == "有", 1, 0),
    f_life_bin    = if_else(f_life    == "有", 1, 0),
    sentence_year = as.numeric(d3_y) + 1911
  ) %>%
  select(id, d3_y, sentence_year,
         prosecution, f_death_bin, f_term_bin, f_life_bin)

death_subset_clean <- death_subset %>%
  filter(sentence_year >= 1949,
         sentence_year <= 1988)

sentence_stats <- death_subset_clean %>%
  group_by(sentence_year) %>%
  summarise(
    pros_count    = sum(prosecution,  na.rm = TRUE),
    death_count   = sum(f_death_bin,   na.rm = TRUE),
    term_count    = sum(f_term_bin,    na.rm = TRUE),
    life_count    = sum(f_life_bin,    na.rm = TRUE)
  ) %>%
  arrange(sentence_year)

sentence_clean_long <- sentence_stats %>%
  pivot_longer(
    cols = c(pros_count, death_count, term_count, life_count),
    names_to = "type",
    values_to = "count"
  )

sentence_clean_long <- sentence_clean_long %>%
  mutate(leader_period = case_when(
    sentence_year <= 1970 ~ "Chiang Kai-shek", 
    sentence_year > 1970 ~ "Chiang Ching-kuo"  
  ))

sentence_clean_long <- sentence_clean_long %>%
  mutate(
    type = recode(type,
                  pros_count  = "Prosecutions",
                  death_count = "Death Sentences",
                  term_count  = "Fixed-term imprisonment",
                  life_count  = "Life imprisonment")
  )

write.csv(sentence_clean_long, "Data/dta_sentence_outcome.csv", row.names = FALSE)

c <- ggplot(sentence_clean_long, 
       aes(x = sentence_year, y = count, fill = leader_period)) +
  geom_col(width = 0.8) + 
  facet_wrap(~ type, scales = "free_y") +
  labs(
    title = "",
    x = "Year",
    y = "Count",
    fill = "Leader Period"
  ) +
  scale_fill_manual(values = c(
    "Chiang Kai-shek"  = "grey30",
    "Chiang Ching-kuo" = "grey70"
  )) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 12)
  )

ggsave("figs/fig_appen_A1.png", plot = c, width = 10, height = 6, dpi = 300)
