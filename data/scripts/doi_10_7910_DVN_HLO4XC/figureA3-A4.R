# *********************************************************************
# Replication code for Systemic Discrimination Among Large U.S. Employers
# Patrick M. Kline, Evan K. Rose, Christopher R. Walters
# April, 2022

# This code produces Figures A3-A4, which plot mean contact rates by
# first and last name.

# It must be run in the top level of the replication archive directory.
# *********************************************************************

library(haven)
library(tidyverse)
library(RColorBrewer)
library(ggtext)
library(ggplot2)
library(cowplot)

# Load the data
df <- read_dta(file='data/data.dta')

# Figure A3: Callbacks by applicant first name
ols <- felm(cb ~ black + white - 1|0|0|job_id, data = df)
ols2 <- felm(cb ~ factor(firstname) - 1|0|0|job_id, data = df)
coef <- as.data.frame(summary(ols2)$coefficients)
coef <- coef %>% rownames_to_column(var='coef') %>%
  extract(coef, "firstname", "\\(firstname\\)([A-Za-z]+)", remove=TRUE)
coef <- df %>% select(race,gender,firstname) %>% distinct %>%
  right_join(coef, by='firstname') %>% arrange(race,gender,firstname) %>%
  mutate(order=row_number())
coef[,1:3]<- lapply(coef[, 1:3], as.factor)

coef <- coef %>% mutate("Race and gender"=case_when(
      race == "Black" & gender == "Female" ~ "Black, female",
      race == "Black" & gender == "Male" ~ "Black, male",
      race == "White" & gender == "Female" ~ "White, female",
      race == "White" & gender == "Male" ~ "White, male"))

ggplot(data=coef, aes(x=reorder(factor(firstname),order), y=Estimate, fill=`Race and gender`)) +
  geom_bar(stat='identity',alpha=.3) + theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(
    x = "Applicant first name",
    y = "Mean 30-day callback rate",
    fill = "Race and gender") +  
  geom_segment(aes(x = 1, y = coef(ols)['black'], xend = 38, yend = coef(ols)['black']),
               show.legend = FALSE, color="#F8766D") +  
  geom_segment(aes(x = 39, y = coef(ols)['white'], xend = 76, yend = coef(ols)['white']),
               show.legend = FALSE, color="#00BFCF")
  ggsave(paste("figures/figureA3.pdf",sep=""), 
    height=5, width=8, units="in")

# Figure A4: Callbacks by applicant last name
ols2 <- felm(cb ~ factor(lastname) - 1|0|0|job_id, data = df)
coef <- as.data.frame(summary(ols2)$coefficients)
coef <- coef %>% rownames_to_column(var='coef') %>%
  extract(coef, "lastname", "\\(lastname\\)([A-Za-z]+)", remove=TRUE)
coef <- df %>% select(race,lastname) %>% distinct %>%
  right_join(coef, by='lastname') %>% arrange(race,lastname) %>%
  mutate(order=row_number())

ggplot(data=coef, aes(x=reorder(factor(lastname),order), y=Estimate, fill=factor(race))) +
  geom_bar(stat='identity',alpha=.3) + theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(
    x = "Applicant last name",
    y = "Mean 30-day callback rate",
    fill = "Race") +  
  geom_segment(aes(x = 1, y = coef(ols)['black'], xend = 26, yend = coef(ols)['black']),
               show.legend = FALSE, color="#F8766D") +  
  geom_segment(aes(x = 27, y = coef(ols)['white'], xend = 52, yend = coef(ols)['white']),
               show.legend = FALSE, color="#00BFCF")
  ggsave(paste("figures/figureA4.pdf",sep=""), 
    height=5, width=8, units="in")
