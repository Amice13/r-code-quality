## Hager / Hilbig - Replication code - Jan 7, 2020
## hhilbig@g.harvard.edu
##
## Figures reproduced in this file
## A13
### ### ### ###

rm(list= ls())

## Packages

library(tidyverse)
library(lfe)
library(lubridate)

## Load file

pb_full <- read_rds('Politbarometer.RDS')

## Select some vars

pb_full <- pb_full %>%
 dplyr::select(scale_govt, id, date_pb, matches('weight'))

## Aggregate gov't popularity scale

pb_agg <- pb_full %>%
  group_by(date_pb) %>%
  summarize(scale_agg1 = mean(scale_govt, na.rm = T),
            sd1 = sd(scale_govt, na.rm = T) / 
              sqrt(sum(!is.na(scale_govt))))

## Get the surveys 

report_df <- read_rds('Opinion_Reports_V2.RDS')

## Add week

report_df <- report_df %>%
  mutate(week_start_date = week(start_date2),
         year_start_date = year(start_date2))

## Report dates DF

dates_reports <- data.frame(date = unique(report_df$start_date2)) %>%
  filter(date> min(pb_full$date_pb))

## Aggregate number of reports started by week and year

report_dates_agg <- report_df %>%
  group_by(week_start_date, year_start_date) %>%
  summarise(no_reports = n()) %>%
  rename(week = week_start_date,
         year = year_start_date)

#### Look at start dates by month / week ####
## Week data set 

df <- expand.grid(week = 1:52, year = 2009:2013) %>%
  filter(!(week > 40 & year == 2013)) %>%
  filter(!(week < 40 & year == 2009))

## Add dates to PB data

pb_agg <- pb_agg %>%
  mutate(week = lubridate::week(date_pb),
         year = lubridate::year(date_pb))

## Merge to the week DF
## Fill down (if t is missing, fill in T-1 recursively)

df <- left_join(df, pb_agg %>% dplyr::select(-date_pb)) %>%
  fill(scale_agg1, .direction = 'down')

## Merge report collection start date to this !

df <- left_join(df, report_dates_agg)

## Fill in the NAs

df$no_reports[is.na(df$no_reports)] <- 0

## Drop first few rows (before start date of legislaturperiode)

df <- df %>% 
  filter(!is.na(scale_agg1)) %>%
  mutate(any_report_started = ifelse(no_reports > 0, 1 ,0))

##  Create lags

df <- df %>%
  mutate(pop_lag1 = lag(scale_agg1, n = 1),
         pop_lag2 = lag(scale_agg1, n = 2),
         pop_lag3 = lag(scale_agg1, n = 3))

## Regressions

m1 <- felm(any_report_started ~ scale(scale_agg1) | 0 | 0 | 0, 
           data = df)
m2 <- felm(any_report_started ~ scale(scale_agg1) | year |0|0, 
           data = df)
m3 <- felm(any_report_started ~ scale(scale_agg1) | year + week |0|0, 
           data = df)

## Models with lagged popularity

m1_lag1 <- felm(any_report_started ~ scale(pop_lag1) | 0 | 0 | 0, 
                data = df)
m1_lag2 <- felm(any_report_started ~ scale(pop_lag2) | 0 | 0 | 0, 
                data = df)
m1_lag3 <- felm(any_report_started ~ scale(pop_lag3) | 0 | 0 | 0, 
                data = df)

## Lags + FE

m3_lag1 <- felm(any_report_started ~ scale(pop_lag1) | year + week | 0 | 0, 
                data = df)
m3_lag2 <- felm(any_report_started ~ scale(pop_lag2) | year + week | 0 | 0, 
                data = df)
m3_lag3 <- felm(any_report_started ~ scale(pop_lag3) | year + week | 0 | 0, 
                data = df)

## To plot

list_models <- list(m1, m1_lag1, m1_lag2, m1_lag3, 
                    m3, m3_lag1, m3_lag2, m3_lag3)
list_models <- list_models %>%
  purrr::map(., ~broom::tidy(., conf.int = T)) %>%
  map(., ~filter(., !term == '(Intercept)')) %>%
  reduce(rbind) %>%
  mutate(lag = rep(c('t', 't-1', 't-2', 't-3'), 2),
         model = rep(c('No FE', 'Year + Week FE'), each = 4)) %>%
  mutate(lag = paste0('Satisfaction with\nthe government (', lag, ')')) %>%
  mutate(lag = factor(lag, levels = unique(lag)[4:1]))

## Plot this

pd <- position_dodge(0.4)

p2 <- ggplot(list_models, aes(x = lag, y = estimate, group = model)) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = model),
                position = pd, width = 0) +
  geom_point(aes(x = lag, y = estimate, shape = model), 
             fill = 'white', 
             position = pd) +
  scale_shape_manual(name = '', values = c(21, 22)) +
  scale_color_manual(name = '', values = c('black', 'grey40')) +
  xlab('') + ylab('') + theme_bw() + coord_flip() + 
  theme(legend.position = 'bottom')
p2
