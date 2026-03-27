## Data and Materials Sharing
#==========================================================================
# 03-classify-url-ideology.R
# Purpose: classify URL ideology by the sharers' ratio
# Author: Blind for review
#==========================================================================
library(plyr)
library(dplyr)
library(purrr)
options(scipen = 999)

# load user level URL shares
urls_merge <- readRDS('01_url_reps_all_topics_merge.rds')
urls_diversity <- readRDS('02_urls_diversity_topic_all.rds')

# match urls with the diversity and select those more than 10 times shared
# then, compute url's ideological leanings based on the sharing ratio
process_topic <- function(topic_name, urls_merge, urls_diversity) {
  url_count <- urls_merge %>%
    filter(topic == topic_name) %>%
    group_by(urls_rep) %>%
    summarise(share_n = n(), .groups = 'drop') %>%
    filter(share_n >= 10)
  
  merged_data <- list(
    urls_merge %>% filter(topic == topic_name),
    urls_diversity %>% filter(topic == topic_name),
    url_count
  ) %>% reduce(left_join, by = "urls_rep")
  
  merged_data <- merged_data %>%
    mutate(
      url_fixed = ifelse(conservative / share_n >= 0.7, 'conservative',
                         ifelse(liberal / share_n >= 0.7, 'liberal', 'moderate/mixed')),
      url_fixed50 = ifelse(conservative / share_n >= 0.5, 'conservative',
                           ifelse(liberal / share_n >= 0.5, 'liberal', 'moderate/mixed')),
      url_fixed90 = ifelse(conservative / share_n >= 0.9, 'conservative',
                           ifelse(liberal / share_n >= 0.9, 'liberal', 'moderate/mixed'))
    ) %>%
    filter(!is.na(share_n) & no_shared_partisan == 0)
  
  print(length(merged_data$id))
  
  return(merged_data)
}

topics <- c('FreeBritney', 'Biden-Putin', 'COVID-19', 'MLB')
processed_data <- map(topics, ~ process_topic(.x, urls_merge, urls_diversity))
names(processed_data) <- topics

all_topics <- plyr::rbind.fill(processed_data)
all_topics <- all_topics %>% mutate(topic = topic.x) %>% select(-topic.x, -topic.y)
saveRDS(all_topics, '03_all_topics_URL_n10.rds')

