require(ggplot2)
require(sp)
require(doBy)
require(ggthemes)
require(tidyverse)
require(lubridate)

load('executive-actions-subset.Rda')
load('executive-action-coverage.Rda')

# distribution of coverage
articles = articles_coverage_final_dates

tbl <- table(articles$lowande.uid)
tbl[order(-as.numeric(tbl))]

median(tbl) # median among those that are covered

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(tbl) # mode amond those that are covered

tbl[order(-as.numeric(tbl))][1:10] # top ten

# average time to coverage

# add new topics to both action and article datasets

actions <- action_dataset_final
articles <- articles_coverage_final_dates
rm(action_dataset_final,
   articles_coverage_final_dates)


articles_when <- left_join(articles, 
                           select(actions, lowande.uid, president),
                           by = "lowande.uid")

articles_when <- articles_when %>%
  mutate(action_year = as.integer(year(action_date))) %>% 
  mutate(beginning_admin = case_when(
    action_year %in% 2017:2018 & president == "Trump" ~ 1,
    action_year %in% 2009:2014 & president == "Obama" ~ 1,
    action_year %in% 2001:2004 & president == "Bush" ~ 1,
    action_year %in% 1993:1998 & president == "Clinton" ~ 1,
    action_year %in% 1989:1990 & president == "HWBush" ~ 1,
    TRUE ~ 0
  )) %>% 
  filter(beginning_admin == 1)

articles_when$pub_date = as.character(articles_when$pub_date)
articles_when$pub_date = as.Date(articles_when$pub_date, "%Y-%m-%d")

articles_when <- articles_when %>% 
  mutate(n_days_after = as.integer(pub_date - action_date))

actions_when <- articles_when %>% 
  group_by(lowande.uid,president) %>%
  summarise(median_year = round(median(n_days_after / 365), 2),
            median_month = round(median(n_days_after / 30), 1),
            median_days = round(median(n_days_after, 1)))

actions_when <- actions_when %>% 
  mutate(party = case_when(
    president == "Trump" ~ 'R', 
    president == "Obama" ~ 'D', 
    president == "Bush" ~ 'R', 
    president == "Clinton" ~ 'D', 
    president == "HWBush" ~ 'R'
  ))

# get medians for geom_text
president_medians <- actions_when %>% 
  group_by(president) %>% 
  summarise(pres_median_year = round(median(median_days / 365), 2),
            pres_median_month = round(median(median_days / 30), 1))

actions_when$president = factor(actions_when$president,levels = c('HWBush','Clinton','Bush','Obama','Trump'))
# -----------------------------------------------

# Figure 6.2a: Most coverage occurs at the announcement of new actions. 
violin_days_after <- actions_when %>% 
  ggplot(aes(as.factor(president), 
             median_days / 365,
             group = president,
             fill = party)) +
  geom_violin(colour='white') +
  scale_fill_manual(values=c('#7b7d7b','#7b7d7b')) +
  stat_summary(fun = median) +
  geom_text(aes(label = 1.8,x = 'Trump',y = 5.7),color = "#000000") +
  geom_text(aes(label = 2.3,x = 'Obama',y = 5.7),color = "#000000") +
  geom_text(aes(label = 0.1,x = 'HWBush',y = 5.7),color = "#000000") +
  geom_text(aes(label = 0.7,x = 'Clinton',y = 5.7),color = "#000000") +
  geom_text(aes(label = 0.8,x = 'Bush',y = 5.7),color = "#000000") +
  scale_y_continuous(breaks = 0:8, 
                     labels = 0:8) + 
  scale_x_discrete(labels = c("Bush",
                              "Clinton",
                              "W. Bush",
                              "Obama",
                              "Trump")) +
  geom_hline(yintercept = 0, colour="gray", linetype = "longdash") +
  coord_flip() + 
  labs(x = NULL,
       y = "Years After Action",
       title='') +
  theme_classic() +
  theme(text = element_text(family='Palatino'),
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(face = "bold"),
        axis.title.x = element_text(face = "italic"),
        axis.title.y = element_text(face = "italic"),
        axis.text.x=element_text(colour="black"),
        axis.text.y=element_text(colour="black"))
# -----------------------------------------------

# next, scale by proportion of time left in term

load('executive-actions-subset.Rda')
load('executive-action-coverage.Rda')


actions <- action_dataset_final
articles <- articles_coverage_final_dates
rm(action_dataset_final,
   articles_coverage_final_dates)


articles_when <- left_join(articles, 
                           select(actions, lowande.uid, president),
                           by = "lowande.uid")

articles_when$pub_date = as.character(articles_when$pub_date)
articles_when$pub_date = as.Date(articles_when$pub_date, "%Y-%m-%d")

articles_when <- articles_when %>% 
  mutate(n_days_after = as.integer(pub_date - action_date))

articles_when$n_days_after[articles_when$n_days_after<0] = 0 # have to recode to zero for formula to work 

articles_when <- articles_when %>% 
  mutate(last_day = case_when(
    president == "Trump" ~ '2021-01-20', 
    president == "Obama" ~ '2017-01-20', 
    president == "Bush" ~ '2009-01-20', 
    president == "Clinton" ~ '2001-01-20', 
    president == "HWBush" ~ '1993-01-20'
  ))

articles_when$last_day = as.Date(articles_when$last_day,"%Y-%m-%d")

articles_when$action_to_end = as.numeric(articles_when$last_day - articles_when$action_date)

articles_when$time_after_scaled = 1 - articles_when$n_days_after/articles_when$action_to_end

actions_when <- articles_when %>% 
  group_by(lowande.uid,president) %>%
  summarise(median_year = round(median(n_days_after / 365), 2),
            median_month = round(median(n_days_after / 30), 1),
            median_scaled = median(time_after_scaled))

actions_when <- actions_when %>% 
  mutate(party = case_when(
    president == "Trump" ~ 'R', 
    president == "Obama" ~ 'D', 
    president == "Bush" ~ 'R', 
    president == "Clinton" ~ 'D', 
    president == "HWBush" ~ 'R'
  ))

president_medians <- actions_when %>% 
  group_by(president) %>% 
  summarise(pres_median_scaled = round(median_scaled,2))

president_medians = summaryBy(pres_median_scaled ~ president,president_medians,FUN='median',keep.names=T,na.rm=T)

actions_when$president = factor(actions_when$president,levels = c('HWBush','Clinton','Bush','Obama','Trump'))
# -----------------------------------------------

# Figure 6.2b: Most coverage occurs at the announcement of new actions. 
violin_scaled <- actions_when %>% 
  ggplot(aes(as.factor(president), 
             median_scaled,
             group = president,
             fill = party)) +
  geom_violin(colour='white') +
  scale_fill_manual(values=c('#7b7d7b','#7b7d7b')) +
  stat_summary(fun = median) +
  scale_y_continuous(breaks = 0:8, 
                     labels = 0:8) + 
  scale_x_discrete(labels = c("Bush",
                              "Clinton",
                              "W. Bush",
                              "Obama",
                              "Trump")) +
  geom_text(aes(label = 0.96,x = 'Trump',y = .1),color = "#000000") +
  geom_text(aes(label = 0.975,x = 'Obama',y = .1),color = "#000000") +
  geom_text(aes(label = 0.99,x = 'HWBush',y = .1),color = "#000000") +
  geom_text(aes(label = 0.99,x = 'Clinton',y = .1),color = "#000000") +
  geom_text(aes(label = 0.98,x = 'Bush',y = .1),color = "#000000") +
  geom_hline(yintercept = 1, colour="gray", linetype = "longdash") +
  coord_flip() + 
  labs(x = NULL,
       y = "% Time Left in Term",
       title='') +
  theme_classic() +
  theme(text = element_text(family='Palatino'),
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(face = "bold"),
        axis.title.x = element_text(face = "italic"),
        axis.title.y = element_text(face = "italic"),
        axis.text.x=element_text(colour="black"),
        axis.text.y=element_text(colour="black"))
# -----------------------------------------------

