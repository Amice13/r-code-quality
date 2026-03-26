## Data and Materials Sharing
#==========================================================================
# 02-measure-diversity.R
# Purpose: compute URLs diversity (by ideological user shares)
# Author: Blind for review
#==========================================================================
library(tidyverse)
library(openxlsx)
library(vegan)
library(readr)
options(scipen = 999)

# load user and URL share data
urls_merge <- readRDS('01_url_reps_all_topics_merge.rds')

# compute diversity (Simpson's and Shannon's) by topics
# then, normalize the diversity score between 0 to 1 (by multiply 2)
process_diversity <- function(topic_name, urls_merge) {
  topic_data <- urls_merge %>%
    filter(topic == topic_name) %>%
    group_by(urls_rep, theta_class) %>%
    count() %>%
    pivot_wider(names_from = theta_class, values_from = n, values_fill = 0)
  
  topic_data_partisan <- topic_data %>% select(-moderate)
  
  simpson <- plyr::ddply(topic_data_partisan, ~urls_rep, function(x) {
    data.frame(SIMPSON = diversity(x[-1], index = "simpson"))
  })
  
  shannon <- plyr::ddply(topic_data_partisan, ~urls_rep, function(x) {
    data.frame(SHANNON = diversity(x[-1], index = "shannon"))
  })
  
  topic_data <- list(topic_data, simpson, shannon) %>% reduce(full_join, by = "urls_rep")
  
  topic_data <- topic_data %>%
    mutate(
      no_shared_partisan = ifelse(SIMPSON == 1, 1, 0),
      SIMPSON2 = ifelse(SIMPSON == 1, NA, SIMPSON * 2),
      topic = topic_name
    ) %>%
    select(-SIMPSON)
  
  hist(topic_data$SIMPSON2)
  hist(topic_data$SHANNON)
  
  return(topic_data)
}

topics <- c('Biden-Putin', 'COVID-19', 'FreeBritney', 'MLB')
processed_topics <- map_dfr(topics, ~ process_diversity(.x, urls_merge))

saveRDS(processed_topics, '02_urls_diversity_topic_all.rds')
