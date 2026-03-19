## Hager / Hilbig - Replication code - Jan 7, 2020
## hhilbig@g.harvard.edu
##
## Figures reproduced in this file
## A10, A11
### ### ### ###

rm(list= ls())

## Packages

library(tidyverse)
library(pbapply)
library(reshape2)

## Load the full Politbarometer (PB) data ##

pb_full <- read_rds('Politbarometer.RDS')

## List of issues categories

cat_list <- c("soc_policy", "foreign_policy", "education", "environmental", 
              "econ_policy", "interior", "culture")

## We use two methods to calculate the salience rank
## 1st method: Mean number of mentions of each issue in PB

pb_agg <- pb_full %>%
  group_by(date_pb) %>%
  summarise_at(vars(one_of(c(cat_list, 'other'))),
               funs(mean.simple = mean(., na.rm = T)))

## 2nd Method: Discount overlapping topics by dividing by 1/sqrt(n)

pb_agg2 <- pb_full %>%
  rowwise() %>%
  mutate(rowsums = sum(soc_policy, foreign_policy, education, 
                       environmental, econ_policy, interior, culture,
                       other,
                       na.rm = T))

## Divide by square root of row sum

pb_agg2[, c(cat_list, 'other')] <- pbapply(pb_agg2[, c(cat_list, 'other')], 2, 
                                           function(x) {
                                             out = ifelse(x !=0, x / sqrt(pb_agg2$rowsums), 0)
                                             out
                                           })

## Mean by date

pb_agg2 <- pb_agg2 %>%
  group_by(date_pb) %>%
  summarise_at(vars(one_of(c(cat_list, 'other'))),
               funs(mean.adjusted = mean(., na.rm = T)))

## Merge the two methods

pb_agg <- cbind(pb_agg, pb_agg2 %>% dplyr::select(-date_pb))

## Each issue over time 

pb_agg_melt <- reshape2::melt(pb_agg, id.vars = 'date_pb', 
                              variable.name = 'issue') %>%
  mutate(issue = as.character(issue)) %>%
  separate(col = 'issue', into = c('issue', 'method'), sep = '\\.')

## Select only the simple and adjusted salience mean

pb_agg_plot <- pb_agg_melt %>%
  filter(method %in% c('simple', 'adjusted')) %>%
  filter(issue != 'culture_mean')

## Salience over the full date range

drange <- seq(min(pb_agg$date_pb),
              max(pb_agg$date_pb), by = "day")

## Make this a df, fill all values 

df <- data.frame(date = drange) %>%
  left_join(., pb_agg, by = c('date' = 'date_pb')) %>%
  dplyr::select(date, matches('adjusted')) %>%
  mutate(index = 1:n()) %>%
  dplyr::select(-matches('other'))

## Declare variable names

vars <- c("soc_policy_mean.adjusted", "foreign_policy_mean.adjusted", 
          "education_mean.adjusted", 
          "environmental_mean.adjusted", "econ_policy_mean.adjusted", 
          "interior_mean.adjusted", "culture_mean.adjusted")

## Get loess prediction for every issue

for (j in vars) {
  loess <- loess(as.formula(paste0(j, '~ index')), 
                 data = df, span = 0.3)
  loess_pred <- predict(loess, newdata = df$index)
  
  ## Add to df
  df[, paste0(j, '_loess')] <- loess_pred
}

## Convert this into ranks

rankvars <- c("soc_policy_mean.adjusted_loess", "foreign_policy_mean.adjusted_loess", 
              "education_mean.adjusted_loess", "environmental_mean.adjusted_loess", 
              "econ_policy_mean.adjusted_loess", "interior_mean.adjusted_loess", 
              "culture_mean.adjusted_loess")

## Create the rank vars

for (j in vars) df[, paste0(j, '_rank')] <- NA
rankvars <- paste0(vars, '_rank')

## Assign ranks

ranks <- apply(df[, paste0(vars, '_loess')], 
               1, function(x) 8 - rank(x)) %>%
  t()
df[, rankvars] <- ranks

## Keep only rankvars and date

df <- df %>%
  dplyr::select(matches('rank'), date)

## Change columns names

colnames(df) <- str_replace_all(colnames(df), '_mean.adjusted_rank', '')

## Get report meta data

report_df <- read_rds('Opinion_Reports_V2.RDS')

## Remove some objects

rm(list = ls()[!ls() %in% c('df', 'report_df', 'pb_agg', 'pb_agg_plot')])

## Merge ranks to Df
## First, select only the rank variables

df <- df %>%
  dplyr::select(soc_policy, econ_policy, foreign_policy,
                culture, education, interior, environmental, date)
colnames(df)[-8] <- paste0('salience_', colnames(df)[-8])

## Merge to the report DF

report_df <- report_df %>% left_join(., df, 
                                     by= c('start_date2' = 'date'))

## Select salience and topic variables

salvars <- c("salience_soc_policy", "salience_econ_policy", "salience_foreign_policy", 
             "salience_culture", "salience_education", "salience_interior", 
             "salience_environmental")
topicvars <- c("topic_soc_policy", "topic_econ_policy", 
               "topic_foreign_policy", 
               "topic_culture", "topic_education", 
               "topic_interior", "topic_environmental")


## Element wise multiplication

out <- report_df[, salvars] * report_df[, topicvars]
out[out == 0] <- 99
out <- apply(out, 1, min)

## Plot this

plot_df <- table(out[out != 99])%>%
  data.frame() %>%
  rename(rank = Var1, n = Freq)

#### Figure A10 ####

## Plot this

p2 <- ggplot(data = plot_df, aes(x = rank, y = n)) + 
  geom_bar(stat = 'identity', fill = 'grey93', 
           color = 'black', width = 0.8) +
  theme_bw() +
  xlab('Salience of Report Topic\n(1 = Most Salient)') +
  ylab('Number of Reports')
p2

## Salience over time with rug
## The goal is to merge this to the PB over time file
## We wanna make a rug plot

report_melt <- report_df %>%
  reshape2::melt(id.vars = 'start_date2',
                 measure.vars = c('topic1_th_agg',
                                  'topic2_th_agg',
                                  'topic3_th_agg')) %>%
  filter(!is.na(value)) %>%
  dplyr::select(value, start_date2) %>%
  mutate(released = 1) %>%
  rename(issue = value) %>%
  mutate(issue = paste0(issue, '_mean')) %>%
  rename(date_pb = start_date2) %>%
  filter(!issue == 'culture_mean') %>%
  filter(date_pb > min(pb_agg$date_pb))

## Merge report df to rug 

pb_agg_plot2 <- pb_agg_plot

##

pb_agg_plot$issue <- factor(pb_agg_plot$issue)
levels(pb_agg_plot$issue) <- c('Economic Policy', 
                               'Education' ,'Environmental Policy',
                               'Foreign Policy', 'Interior', 'Other',
                               'Social Policy')
pb_agg_plot$issue <- factor(pb_agg_plot$issue, 
                            levels = c('Economic Policy', 'Social Policy',
                                       'Foreign Policy', 'Education' ,
                                       'Environmental Policy',
                                       'Interior', 'Other')[1:7])
report_melt$method = 'simple'
report_melt$issue <- factor(report_melt$issue)
levels(report_melt$issue) <- c('Economic Policy', 
                               'Education' ,'Environmental Policy',
                               'Foreign Policy', 'Interior',
                               'Social Policy')
#### Figure A11 ####

p3 <- ggplot(data = pb_agg_plot %>% filter(!method == 'adjusted'), 
             aes(x = date_pb, 
                 y = value)) +
  geom_line(alpha = 0.9) + 
  theme_bw() +
  facet_wrap(~ issue) +
  geom_smooth(span = 0.3, se = F, size = 0.5, color = 'grey60') +
  geom_segment(data = report_melt %>% filter(released == 1), 
               aes(x = date_pb, y= 0, xend = date_pb, yend = 0.1,
                   group = method), color = 'grey10', size = 0.4,
               alpha = 0.8) +
  xlab('') + ylab('Salience') +  
  coord_cartesian(clip = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        legend.position = 'none')
p3
