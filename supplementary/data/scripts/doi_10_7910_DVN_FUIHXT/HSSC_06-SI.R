## Data and Materials Sharing
#==========================================================================
# 06-SI.R
# Purpose: replicate results of SI
# Author: Blind for review
#==========================================================================
library(tidyverse)
options(scipen = 999)

# load file
all_topics <- readRDS('03_all_topics_URL_n10.rds')

## S2.3 Thresholds of URL Ideology.

process_data <- function(data, threshold_col) {
  data %>%
    count(theta_class, !!sym(threshold_col)) %>%
    mutate(
      pct = round(prop.table(n) * 100,1),
      thresholds = threshold_col,
      url_leanings = !!sym(threshold_col)
    ) %>%
    ungroup() %>%
    select(-!!sym(threshold_col))
}


# the biden putin summit
process_data(all_topics %>% filter(topic == "Biden-Putin"), "url_fixed50")
process_data(all_topics %>% filter(topic == "Biden-Putin"), "url_fixed") # 70%
process_data(all_topics %>% filter(topic == "Biden-Putin"), "url_fixed90")

# covid19
process_data(all_topics %>% filter(topic == "COVID-19"), "url_fixed50")
process_data(all_topics %>% filter(topic == "COVID-19"), "url_fixed") # 70%
process_data(all_topics %>% filter(topic == "COVID-19"), "url_fixed90")

# MLB
process_data(all_topics %>% filter(topic == "MLB"), "url_fixed50")
process_data(all_topics %>% filter(topic == "MLB"), "url_fixed") # 70%
process_data(all_topics %>% filter(topic == "MLB"), "url_fixed90")

# britney
process_data(all_topics %>% filter(topic == "FreeBritney"), "url_fixed50")
process_data(all_topics %>% filter(topic == "FreeBritney"), "url_fixed") # 70%
process_data(all_topics %>% filter(topic == "FreeBritney"), "url_fixed90")

# S2.4 Diversity Index Validation (Shannon's Index)

# create a unique list of urls by selecting necessary variables
all_temp_uniqueURL_s <- all_topics %>% select(urls_rep, SHANNON, starts_with('url_fixed'), share_n, topic) %>% distinct()

## replication for Figure 1A

(temp_biden_putin_url_sep_s <- ggplot(all_temp_uniqueURL_s %>% filter(topic=="Biden-Putin"), aes(x=SHANNON)) +
    geom_histogram( aes(y=(after_stat(count))/sum(after_stat(count))*100), bins = 50 ) +
    labs(
      title = "Biden-Putin Summit",
      x = "Shannon's Index",
      y = "Percent") +
    theme_bw() +
    scale_fill_manual(values=c( "grey")) +
    
    theme(legend.position = 'none') +
    labs(fill='URL Leanings') 
)

(temp_covid19_url_sep_s <- ggplot(all_temp_uniqueURL_s %>% filter(topic=="COVID-19"), aes(x=SHANNON)) +
    geom_histogram( aes(y=(after_stat(count))/sum(after_stat(count))*100), bins = 50 ) +
    labs(
      title = "COVID-19",
      x = "Shannon's Index",
      y = "Percent") +
    theme_bw() +
    theme(legend.position = 'none') +
    scale_fill_manual(values=c( "grey")) +
    labs(fill='URL Leanings') 
)

(temp_britney_url_sep_s <- ggplot(all_temp_uniqueURL_s %>% filter(topic=="FreeBritney"), aes(x=SHANNON)) +
    geom_histogram( aes(y=(after_stat(count))/sum(after_stat(count))*100), bins = 50 ) +
    labs(
      title = "FreeBritney",
      x = "Shannon's Index",
      y = "Percent") +
    theme_bw() +
    theme(legend.position = 'none') +
    scale_fill_manual(values=c(  "grey")) +
    labs(fill='URL Leanings') 
)

(temp_allstargame_url_sep_s <- ggplot(all_temp_uniqueURL_s %>% filter(topic=="MLB"), aes(x=SHANNON)) +
    geom_histogram( aes(y=(after_stat(count))/sum(after_stat(count))*100), bins = 50 ) +
    labs(
      title = "MLB All-Star Game",
      x = "Shannon's Index",
      y = "Percent") +
    theme_bw() +
    theme(legend.position = 'none') +
    scale_fill_manual(values=c( "grey")) +
    labs(fill='URL Leanings') 
)


(figure1_url_sep_s <- ggarrange(temp_biden_putin_url_sep_s, 
                                      temp_covid19_url_sep_s, 
                                      temp_britney_url_sep_s, 
                                      temp_allstargame_url_sep_s,
                                      ncol = 2, nrow = 2, common.legend =TRUE, legend="bottom",
                                      font.label = list(size = 10)) +
    theme(plot.margin = margin(0,0,1,0, "cm"))) 


# S2.5 Multiple Retweets and URL Inclusions on Topics

## A user can retweet multiple times about a single topic

# Biden-Putin
multi_tweets_users <- all_topics %>% filter(topic == "Biden-Putin") %>% group_by(author.id) %>% count()
max(multi_tweets_users$n) # 122
round(mean(multi_tweets_users$n),1) # 2.1
round(sd(multi_tweets_users$n),1) # 3.0

temp <- multi_tweets_users %>% filter(n >= 2)  # more than twice
multi_tweets_users_n2 <- length(temp$n)
round(multi_tweets_users_n2/length(unique(multi_tweets_users$author.id))*100,1) # 39.6%

temp <- multi_tweets_users %>% filter(n >= 10)  # more than 10 times
multi_tweets_users_n10 <- length(temp$n)
round(multi_tweets_users_n10/length(unique(multi_tweets_users$author.id))*100,1) # 2.1%

## COVID-19
multi_tweets_users <- all_topics %>% filter(topic == "COVID-19") %>% group_by(author.id) %>% count()
max(multi_tweets_users$n) # 120
round(mean(multi_tweets_users$n),1) # 1.6
round(sd(multi_tweets_users$n),1) # 2.1

temp <- multi_tweets_users %>% filter(n >= 2)  # 
multi_tweets_users_n2 <- length(temp$n)
round(multi_tweets_users_n2/length(multi_tweets_users$author.id)*100,1) # 29.7%

temp <- multi_tweets_users %>% filter(n >= 10)  # 
multi_tweets_users_n10 <- length(temp$n)
round(multi_tweets_users_n10/length(multi_tweets_users$author.id)*100,1) # 0.5%

## FreeBritney
multi_tweets_users <- all_topics %>% filter(topic == "FreeBritney") %>% group_by(author.id) %>% count()
max(multi_tweets_users$n) # 84
round(mean(multi_tweets_users$n),1) # 1.9
round(sd(multi_tweets_users$n),1) # 2.8

temp <- multi_tweets_users %>% filter(n >= 2)  # 
multi_tweets_users_n2 <- length(temp$n)
round(multi_tweets_users_n2/length(multi_tweets_users$author.id)*100,1) # 36.0%

temp <- multi_tweets_users %>% filter(n >= 10)  # 
multi_tweets_users_n10 <- length(temp$n)
round(multi_tweets_users_n10/length(multi_tweets_users$author.id)*100,1) # 1.7%

## MLB
multi_tweets_users <- all_topics %>% filter(topic == "MLB") %>% group_by(author.id) %>% count()
max(multi_tweets_users$n) # 46
round(mean(multi_tweets_users$n),1) # 1.5
round(sd(multi_tweets_users$n),1) # 1.7

temp <- multi_tweets_users %>% filter(n >= 2)  # 
multi_tweets_users_n2 <- length(temp$n)
round(multi_tweets_users_n2/length(multi_tweets_users$author.id)*100,1) # 22.7%

temp <- multi_tweets_users %>% filter(n >= 10)  # 
multi_tweets_users_n10 <- length(temp$n)
round(multi_tweets_users_n10/length(multi_tweets_users$author.id)*100,1) # 0.7%


## Each retweet can include more than one URL

# Biden-Putin
multi_urls_tweet <-  all_topics %>% filter(topic == "Biden-Putin") %>% group_by(id) %>% count()
max(multi_urls_tweet$n) # 3
round(mean(multi_urls_tweet$n),1) # 1.1
round(sd(multi_urls_tweet$n),1) # 0.3

temp <- multi_urls_tweet %>% filter(n >= 2) 
multi_urls_tweet_n2 <- length(temp$n)
round(multi_urls_tweet_n2/length(unique(multi_urls_tweet$id))*100,1) # 6.9

## COVID-19

multi_urls_tweet <- all_topics %>% filter(topic == "COVID-19") %>% group_by(id) %>% count()
max(multi_urls_tweet$n) # 4
round(mean(multi_urls_tweet$n),1) # 1.2
round(sd(multi_urls_tweet$n),1) # 0.4

temp <- multi_urls_tweet %>% filter(n >= 2) 
multi_urls_tweet_n2 <- length(temp$n)
round(multi_urls_tweet_n2/length(unique(multi_urls_tweet$id))*100,1) # 17.5%

## FreeBritney
multi_urls_tweet <- all_topics %>% filter(topic == "FreeBritney") %>% group_by(id) %>% count()
max(multi_urls_tweet$n) # 3
round(mean(multi_urls_tweet$n),1) # 1.1
round(sd(multi_urls_tweet$n),1) # 0.3

temp <- multi_urls_tweet %>% filter(n >= 2) 
multi_urls_tweet_n2 <- length(temp$n)
round(multi_urls_tweet_n2/length(unique(multi_urls_tweet$id))*100,1) # 12.7%

## MLB
multi_urls_tweet <- all_topics %>% filter(topic == "MLB") %>% group_by(id) %>% count()
max(multi_urls_tweet$n) # 3
round(mean(multi_urls_tweet$n),1) # 1.0
round(sd(multi_urls_tweet$n),1) # 0.2

temp <- multi_urls_tweet %>% filter(n >= 2) 
multi_urls_tweet_n2 <- length(temp$n)
round(multi_urls_tweet_n2/length(unique(multi_urls_tweet$id))*100,1) # 2.4%

## S2.6 Prediction with GLM (see HSSC_05-figure2.R)

