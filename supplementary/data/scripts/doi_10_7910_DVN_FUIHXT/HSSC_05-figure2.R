## Data and Materials Sharing
#==========================================================================
# 05-figure2.R
# Purpose: replicate figure 2
# Author: Blind for review
#==========================================================================
library(tidyverse)
options(scipen = 999)
library(openxlsx)
library(ggplot2)
library(ggpubr)
require(scales)
library(sjPlot)
library(lmtest)
library(sandwich)

# load all tweets
# data contain id, created_at, text, author.id, referenced_tweets.retweeted.id, topic
tweets_topics <- readRDS('05_all_tweets_topics.rds')

# load url (share n >= 10) data
all_urls_n10 <- readRDS('03_all_topics_URL_n10.rds')

# merge url classification and retweet information (text, types)
topic_url_text <- list(all_urls_n10, tweets_topics) %>% reduce(left_join, by=c('topic', 'id', 'author.id'))

# find quoted tweets
# for MLB, the data format has changed, exclude those are just retweeted (without quotes)
topic_url_text <- topic_url_text %>% 
  mutate(rt_quote = ifelse(grepl("^RT @", text)==FALSE & grepl("Biden-Putin|COVID-19|FreeBritney", topic)==TRUE, 1, 
                           ifelse( grepl("MLB", topic)==TRUE & is.na(referenced_tweets.retweeted.id) == TRUE, 1, 0)))

# change the levels of variable within the URL ideology
topic_url_text$url_fixed <- factor(topic_url_text$url_fixed, levels = c("conservative", "liberal", "moderate/mixed"))
topic_url_text$url_fixed50 <- factor(topic_url_text$url_fixed50, levels = c("conservative", "liberal", "moderate/mixed"))
topic_url_text$url_fixed90 <- factor(topic_url_text$url_fixed90, levels = c("conservative", "liberal", "moderate/mixed"))


# URLs level — aggregated percentage of quoted comments; 
# 70% reference
url_all_quote <- topic_url_text %>% 
  mutate(cross_sharing = ifelse( theta_class == "conservative" & url_fixed == "liberal", 1,
                                 ifelse( theta_class == "liberal" & url_fixed == "conservative", 1, 0)))

# 50% reference
url_all_quote <- url_all_quote %>% 
  mutate(cross_sharing_50 = ifelse( theta_class == "conservative" & url_fixed50 == "liberal", 1,
                                    ifelse( theta_class == "liberal" & url_fixed50 == "conservative", 1, 0)))
# 90% reference
url_all_quote <- url_all_quote %>% 
  mutate(cross_sharing_90 = ifelse( theta_class == "conservative" & url_fixed90 == "liberal", 1,
                                    ifelse( theta_class == "liberal" & url_fixed90 == "conservative", 1, 0)))


# calculate the percentage of shares that included an appended comment
url_all_quote_aggregate <- url_all_quote %>% group_by(urls_rep) %>% 
  mutate(rt_quote_agg = mean(rt_quote)*100, 
         cross_share_agg = mean(cross_sharing)*100,
         cross_share_50_agg = mean(cross_sharing_50)*100,
         cross_share_90_agg = mean(cross_sharing_90)*100) %>%
  select(rt_quote_agg, topic, SIMPSON2, url_fixed, url_fixed50, url_fixed90, share_n, cross_share_agg, cross_share_50_agg, cross_share_90_agg) %>% distinct()
round(mean(url_all_quote_aggregate$rt_quote_agg),1) # 19.5
round(sd(url_all_quote_aggregate$rt_quote_agg),1) # 24
min(url_all_quote_aggregate$rt_quote_agg) # 0
max(url_all_quote_aggregate$rt_quote_agg) # 100

url_all_quote_aggregate %>% ungroup() %>% filter(topic == "Biden-Putin") %>% summarize(mean = mean(cross_share_agg))
url_all_quote_aggregate %>% ungroup() %>% filter(topic == "COVID-19") %>% summarize(mean = mean(cross_share_agg))
url_all_quote_aggregate %>% ungroup() %>% filter(topic == "FreeBritney") %>% summarize(mean = mean(cross_share_agg))
url_all_quote_aggregate %>% ungroup() %>% filter(topic == "MLB") %>% summarize(mean = mean(cross_share_agg))

# the mean percentage of cross-ideological sharing

# 70%
round(mean(url_all_quote_aggregate$cross_share_agg),1) # 1 
round(sd(url_all_quote_aggregate$cross_share_agg),1) # 3.3
min(url_all_quote_aggregate$cross_share_agg) # 0
max(url_all_quote_aggregate$cross_share_agg) # 30

# S2.3 Thresholds of URL Ideology; the results of Figure 2 of the main text
# 50%
round(mean(url_all_quote_aggregate$cross_share_50_agg),1) # 3.9
round(sd(url_all_quote_aggregate$cross_share_50_agg),1) # 7.7

# 90%
round(mean(url_all_quote_aggregate$cross_share_90_agg),1) # 0.2
round(sd(url_all_quote_aggregate$cross_share_90_agg),1) # 1

# level changes for figure
url_all_quote_aggregate$url_fixed <- factor(url_all_quote_aggregate$url_fixed, levels = c( "liberal", "conservative", "moderate/mixed"))
url_all_quote_aggregate$url_fixed50 <- factor(url_all_quote_aggregate$url_fixed50, levels = c( "liberal", "conservative", "moderate/mixed"))
url_all_quote_aggregate$url_fixed90 <- factor(url_all_quote_aggregate$url_fixed90, levels = c( "liberal", "conservative", "moderate/mixed"))


## linear regression for Figure 2A (with different thresholds)
# 50%
shares_comments_unique_50 <- lm(rt_quote_agg ~ topic + SIMPSON2 + url_fixed50 + share_n + cross_share_50_agg, url_all_quote_aggregate)
summary(shares_comments_unique_50)

# 70%
shares_comments_unique_70 <- lm(rt_quote_agg ~ topic + SIMPSON2 + url_fixed + share_n + cross_share_agg, url_all_quote_aggregate)
summary(shares_comments_unique_70)

# 90%
shares_comments_unique_90 <- lm(rt_quote_agg ~ topic + SIMPSON2 + url_fixed90 + share_n + cross_share_90_agg, url_all_quote_aggregate)
summary(shares_comments_unique_90)

# Prediction with GLM (S2.6)
url_all_quote_aggregate <- url_all_quote_aggregate %>% mutate(rt_quote_agg.bi = ifelse(rt_quote_agg==0, 0, 1))
url_all_quote_aggregate$url_fixed <- factor(url_all_quote_aggregate$url_fixed, levels = c( "liberal", "conservative", "moderate/mixed"))

shares_comments_unique_glm <- glm(rt_quote_agg.bi ~ topic + SIMPSON2 + url_fixed + share_n + cross_share_agg, family=binomial(link="logit"), url_all_quote_aggregate)
summary(shares_comments_unique_glm)
coeftest(shares_comments_unique_glm, vcov. = vcovHC(shares_comments_unique_glm, type = "HC1"))

## Figure 2A
subtitle1 <- expression(paste("Estimates *** ", italic(p), " < .001;", " ** ", italic(p), " < .01"))

(shares_comments_unique_plot <- plot_model(shares_comments_unique_70, vline.color = "gray", show.values = TRUE, value.offset = .3) +
    labs(
      title = "Predicting Percentage of URL Shared Accompanied by a Comment",
      subtitle = subtitle1,
      x = "",
      y = "Percentage of Shares with a Comment") +
    theme_bw() +
    theme(legend.position = 'bottom', plot.title = element_text(size=10), plot.subtitle = element_text(size=8)) +
    scale_x_discrete(limits = c("topicMLB", 
                                "topicFreeBritney", 
                                "topicCOVID-19", 
                                "share_n", 
                                "url_fixedconservative", "url_fixedmoderate/mixed", 
                                "SIMPSON2",
                                "cross_share_agg"),
                     labels = c("Topic: MLB\n(ref. Biden-Putin)", 
                                "Topic: FreeBritney\n(ref. Biden-Putin)",  
                                "Topic: COVID-19\n(ref. Biden-Putin)",  
                                "Number of Shares",
                                "Conservative URLs\n(ref. Liberal)", "Moderate/Mixed URLs\n(ref. Liberal)", 
                                "Simpson's Diversity",
                                "Cross-ideological\nSharing"
                     )) +
    scale_colour_manual(values = shares_comments_unique$coefficients)
)

### Preparing for response coding ###

url_all_quote <- url_all_quote %>% mutate(id_author = paste(id, author.id, sep="_"))

# 70% reference
url_all_quote <- url_all_quote %>% 
  mutate(category_70 = ifelse(theta_class=='conservative' & url_fixed=='conservative', "cons_Users, cons_URLs",
                              ifelse(theta_class=='conservative' & url_fixed=='liberal', "cons_Users, libs_URLs",
                                     ifelse(theta_class=='liberal' & url_fixed=='conservative', "libs_Users, cons_URLs",
                                            ifelse(theta_class=='liberal' & url_fixed=='liberal', "libs_Users, libs_URLs", NA)))))

# 50% reference
url_all_quote <- url_all_quote %>%
  mutate(category_50 = ifelse(theta_class=='conservative' & url_fixed50=='conservative', "cons_Users, cons_URLs",
                              ifelse(theta_class=='conservative' & url_fixed50=='liberal', "cons_Users, libs_URLs",
                                     ifelse(theta_class=='liberal' & url_fixed50=='conservative', "libs_Users, cons_URLs",
                                            ifelse(theta_class=='liberal' & url_fixed50=='liberal', "libs_Users, libs_URLs", NA)))))

# 90% reference
url_all_quote <- url_all_quote %>%
  mutate(category_90 = ifelse(theta_class=='conservative' & url_fixed90=='conservative', "cons_Users, cons_URLs",
                              ifelse(theta_class=='conservative' & url_fixed90=='liberal', "cons_Users, libs_URLs",
                                     ifelse(theta_class=='liberal' & url_fixed90=='conservative', "libs_Users, cons_URLs",
                                            ifelse(theta_class=='liberal' & url_fixed90=='liberal', "libs_Users, libs_URLs", NA)))))

quote_reactance <- url_all_quote %>% filter(rt_quote==1 & is.na(category_70)==FALSE)
url_all_quote %>% dplyr::count(rt_quote) %>% mutate(rt_perc = prop.table(n)*100)

# percentage of quoted responses
round(length(quote_reactance$id)/length(url_all_quote$id)*100, 1) # 4.7%

table(quote_reactance$topic)
table(quote_reactance$reactance)
table(quote_reactance$cross_sharing_50)
mean(quote_reactance$share_n)
## load the completed reactance coding 
reactance_coding <- readRDS('05_reactance_coding.rds')

reactance_coding <- reactance_coding %>% mutate(counter.argue = ifelse(reactance=="counter.argument", 1, 0))
reactance_coding <- reactance_coding %>% mutate(cross.sharing = ifelse(grepl("cons_Users, libs_URLs|libs_Users, cons_URLs", category_70)==TRUE, 1, 0))

# S1.4 Quoted Responses (percentage)

url_all_quote <- url_all_quote %>% mutate(reactance_coded = ifelse(rt_quote==1 & is.na(category_70)==FALSE, 1, 0))

url_all_quote  %>% filter(topic == "Biden-Putin") %>% count(reactance_coded) %>% mutate(count = round(prop.table(n)*100, 1))
url_all_quote  %>% filter(topic == "COVID-19") %>% count(reactance_coded) %>% mutate(count = round(prop.table(n)*100, 1))
url_all_quote  %>% filter(topic == "FreeBritney") %>% count(reactance_coded) %>% mutate(count = round(prop.table(n)*100, 1))
url_all_quote  %>% filter(topic == "MLB") %>% count(reactance_coded) %>% mutate(count = round(prop.table(n)*100, 1))

reactance_coding %>% filter(counter.argue==1) %>% group_by(topic) %>% dplyr::count()
table(reactance_coding$counter.argue)

# Prepare for counter argument 
# counter_argue_aggregate
counter_argue_aggregate <- reactance_coding %>% group_by(urls_rep) %>% 
  dplyr::summarise(counter.argue.agg = mean(counter.argue)*100, cross.sharing.agg = mean(cross.sharing))
counter_argue_aggregate <- list(counter_argue_aggregate, reactance_coding %>% select(urls_rep, topic, url_fixed, SIMPSON2, share_n) %>% distinct()) %>% 
  reduce(left_join, by="urls_rep")

# the mean percentage of counter argument (aggregated)
round(mean(counter_argue_aggregate$counter.argue.agg),0) # 11%
round(sd(counter_argue_aggregate$counter.argue.agg),1) # 24.4

# the mean of aggregated cross sharing
mean(counter_argue_aggregate$cross.sharing.agg)
sd(counter_argue_aggregate$cross.sharing.agg)


# Figure 2B
subtitle2 <- expression(paste("Estimates *** ", italic(p), " < .001"))

# change the level for convenience
counter_argue_aggregate$url_fixed <- factor(counter_argue_aggregate$url_fixed, levels = c("liberal", "conservative", "moderate/mixed"))
table(counter_argue_aggregate$url_fixed)

# run regression
counter_argue <- lm(counter.argue.agg ~ topic + SIMPSON2 + url_fixed + share_n + cross.sharing.agg , counter_argue_aggregate)
summary(counter_argue)

# Prediction with GLM (S2.6)
counter_argue_aggregate <- counter_argue_aggregate %>% mutate(counter.argue.agg.bi = ifelse(counter.argue.agg==0, 0, 1))
counter_argue_glm <- glm(counter.argue.agg.bi ~ topic + SIMPSON2 + url_fixed + share_n + cross.sharing.agg, family=binomial(link="logit"), counter_argue_aggregate)
summary(counter_argue_glm)
coeftest(counter_argue_glm, vcov. = vcovHC(counter_argue_glm, type = "HC1"))


# vline.color = "grey43",
(counter_argue_unique_plot <- plot_model(counter_argue, vline.color = "gray", show.values = TRUE, value.offset = .3) +
    labs(
      title = "Predicting Percentage of Comments on Ideological URL Shares\nthat Include Counterargument",
      subtitle = subtitle2,
      x = "",
      y = "Percentage of Shares with Counterargument Responses") +
    theme_bw() +
    theme(legend.position = 'bottom', plot.title = element_text(size=10), plot.subtitle = element_text(size=8)) +
    scale_x_discrete(limits = c("topicMLB", "topicFreeBritney", "topicCOVID-19", 
                                "share_n", 
                                "url_fixedconservative", "SIMPSON2",
                                "cross.sharing.agg"),
                     labels = c("Topic: MLB\n(ref. Biden-Putin)", "Topic: FreeBritney\n(ref. Biden-Putin)",  "Topic: COVID-19\n(ref. Biden-Putin)", 
                                "Number of Shares",
                                "Conservative URLs\n(ref. Liberal)", "Simpson's Diversity",
                                "Cross-ideological Sharing"
                     )) +
    scale_colour_manual(values = counter_argue$coefficients)
)


## variability only
(figure2_merge4 <- ggarrange(shares_comments_unique_plot, counter_argue_unique_plot,
                             labels = c("A", "B"),
                             ncol = 2, nrow = 1, common.legend = FALSE, legend="bottom",
                             font.label = list(size = 15)))

png("HSSC_figure2.png", units="in", width=12, height=5, res=300)
figure2_merge4
dev.off()


