## Data and Materials Sharing
#==========================================================================
# 01-user-match-topics.R
# Purpose: classify user ideology and match with URLs
# Author: Blind for review
#==========================================================================

library(tidyverse)
options(scipen = 999)

# S2.1 Determining User Ideology
# Resulted in the identification of 80,130 users who met criteria (see S2.1).

# load user information based on the bayesian spatial following model (theta is the output). 
# Please refer to: Barberá, P. (2015). Birds of the same feather tweet together: 
# Bayesian ideal point estimation using Twitter data. Political analysis, 23(1), 76-91.

users_US_EN <- readRDS('01_users_us_en.rds')

# classify user ideology categories by tertiles 
vTert = quantile(users_US_EN$theta, c(0:3/3), na.rm=T) 

users_US_EN$theta_class = with(users_US_EN, cut(theta, vTert, include.lowest = T, labels = c( "conservative", "moderate", "liberal")))
table(users_US_EN$theta_class)

# all URLs shared by topic (one user can share multiple URLs) 
# this is an output of S1.2 Processing of URLs 

url_reps_all_topics <- readRDS('01_url_reps_all_topics.rds')

# merge urls by topic with users; filter out users with ideological information and unique URLs
urls_britney <- list(url_reps_all_topics %>% filter(topic == 'FreeBritney'), users_US_EN) %>% 
  reduce(left_join, by=c("author.id")) %>% filter(is.na(theta)==FALSE) %>% select(-urls) %>% distinct()
urls_biden_putin <- list(url_reps_all_topics %>% filter(topic == 'Biden-Putin'), users_US_EN) %>% 
  reduce(left_join, by=c("author.id")) %>% filter(is.na(theta)==FALSE) %>% select(-urls) %>% distinct()
urls_covid19 <- list(url_reps_all_topics %>% filter(topic == 'COVID-19'), users_US_EN) %>% 
  reduce(left_join, by=c("author.id")) %>% filter(is.na(theta)==FALSE) %>% select(-urls) %>% distinct()
urls_allstargame <- list(url_reps_all_topics %>% filter(topic == 'MLB'), users_US_EN) %>% 
  reduce(left_join, by=c("author.id")) %>% filter(is.na(theta)==FALSE) %>% select(-urls) %>% distinct()

url_reps_all_topics_merge <- plyr::rbind.fill(urls_britney, urls_biden_putin, urls_covid19, urls_allstargame)
saveRDS(url_reps_all_topics_merge, '01_url_reps_all_topics_merge.rds')

