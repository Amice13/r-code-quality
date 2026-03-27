## Hager / Hilbig - Replication code - Jan 7, 2020
## hhilbig@g.harvard.edu
##
## Figures reproduced in this file
## A4, A5
##
## Tables reproduced in this file
## A2
### ### ### ###

rm(list = ls())
library(tidyverse)

## Get the report meta data file

report_df <- read_rds('Opinion_Reports_V2.RDS')

#### Figure A4 ####

## We first need a list of all state elections 

state_elections <- read_rds('State_Elections.RDS')

## The goal is to calculate the time that has passed between a given election
##    and the release of an opinion report
##
## Therefore, we create a data frame of all possible report-election pairs

dyad_df <- expand.grid(report_df$report, state_elections$election_id,
                       stringsAsFactors = F, KEEP.OUT.ATTRS = F) %>%
  rename(report = 1, election_id = 2) %>%
  left_join(.,  report_df[, c('report', 'date')]) %>%
  left_join(., state_elections[, c('election_id', 'date_election')]) %>%
  mutate(date_report = date) %>%
  mutate(time_diff = as.numeric(date_election - date_report)) %>%
  mutate(within_120_days = ifelse(abs(time_diff) < 121, 1, 0))

## Draw the figure ##

p1 <- ggplot(data = dyad_df[dyad_df$within_120_days == 1, ], 
             aes(x = time_diff, ..density..)) + 
  geom_histogram(color = "black",
                 boundary = 0.5,
                 size = 0.4, 
                 binwidth = 14,
                 fill = "grey95") +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black',
             size = 1) + 
  theme_bw() + theme_bw() + 
  xlab('Days between report release\n and state election') +
  ylab('Density')
p1

#### Table A2 ####

## Scale three variables for regressions

report_df$salience_data_collection_end <- scale(report_df$salience_data_collection_end)
report_df$salience_data_collection_start <- scale(report_df$salience_data_collection_start)
report_df$survey_words_stem <- scale(report_df$survey_words_stem)

## Run models 

m1 <- lm(survey_time_to_rel ~ salience_data_collection_start, data = report_df)
m2 <- lm(survey_time_to_rel ~ survey_words_stem, data = report_df)
m3 <- lm(survey_time_to_rel ~ survey_days, data = report_df)
m4 <- lm(survey_time_to_rel ~ salience_data_collection_start + 
          topic_econ_policy + topic_foreign_policy+ 
           topic_culture + topic_education +topic_interior + 
           topic_environmental,
         data = report_df)
m5 <- lm(survey_time_to_rel ~ survey_words_stem + 
            topic_econ_policy + topic_foreign_policy+ 
            topic_culture + topic_education +topic_interior + 
           topic_environmental,
          data = report_df)
m6 <- lm(survey_time_to_rel ~ survey_days + 
            topic_econ_policy + topic_foreign_policy+ 
            topic_culture + topic_education +topic_interior + 
           topic_environmental,
          data = report_df)
m7 <- lm(survey_time_to_rel ~ salience_data_collection_start + 
           survey_words_stem + 
             survey_days + 
            topic_econ_policy + topic_foreign_policy+ 
            topic_culture + topic_education +topic_interior + 
           topic_environmental,
          data = report_df)

## List of covariate labels for stargazer

labels_covs <- c('Salience of topic (SD)', 'Length of report (SD)', 
                 'Length of data collection (days)',
                 'Economic policy', 'Foreign policy',
                 'Culture', 'Education', 'Interior', 
                 'Environmental policy')

## Make list of all models 

model_list <- list(m1, m2, m3, m4, m5, m6, m7)

## Outout via stargazer

library(stargazer)
stargazer(model_list, 
          covariate.labels = labels_covs, 
          keep.stat = c('n', 'rsq'), 
          dep.var.labels = 'Days between data collection and report release',
          style = 'ajps', font.size = 'small', digits = 4)

#### Figure A5 ####

## Load Bundestag sessions

session_df <- read_rds('Bundestag_Sessions.RDS')

## add id to report df

session_dyads <- expand.grid(session_id = session_df$session_id,
                             report = report_df$report, 
                             stringsAsFactors = F) %>%
  left_join(., session_df) %>%
  left_join(., report_df[, c('report', 'date')]) %>%
  rename(report_date = 4) %>%
  mutate(time_dist = session_date - report_date) %>%
  filter(between(time_dist, -62, 63))

## Plot

p1 <- ggplot(data = session_dyads, 
             aes(x = time_dist)) + 
  geom_histogram(color = "black",
                 boundary = 0,
                 size = 0.4, 
                 binwidth = 7,
                 fill = "grey95") +
  scale_y_continuous(labels=function(x)x / length(unique(session_df$session_date))) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black',
                      size = 1) + 
  theme_bw() +
  xlab('Days between report release\n and parliament session') +
  ylab('Number of reports released')
p1
