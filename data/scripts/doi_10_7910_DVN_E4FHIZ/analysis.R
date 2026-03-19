# remove everything so we are working with an empty space
rm(list=ls(all=T))

# load packages
library(readr)
library(dplyr)
library(urltools)
library(stringr)
library(purrr)
library(tidyr)
library(httr)
library(textclean)
library(gsubfn)
library(anytime)
library(xtable)
library(ggplot2)
library(gridExtra)
library(Publish)
library(lubridate)
library(png)


ccdfs_both <- read_csv("Desktop/replication_data/ccdfs_both.csv")
ccdfs_directly_commented <- read_csv("Desktop/replication_data/ccdfs_directly_commented.csv")
ccdfs_same_url <- read_csv("Desktop/replication_data/ccdfs_same_url.csv")
comments_linking_factcheckers <- read_csv("Desktop/replication_data/comments_linking_factcheckers.csv")

# Table S2

ccdfs_both %>% 
  group_by(veracity) %>%
    summarize(
      N = n(),
      Mean = mean(num_commenters),
      SD = sd(num_commenters),
      Min = min(num_commenters),
      Max = max(num_commenters)
    ) %>% xtable()        

ks.test(ccdfs_both$num_commenters[ccdfs_both$veracity == "true"], ccdfs_both$num_commenters[ccdfs_both$veracity == "false"])

# Table S3

ccdfs_both %>% 
  group_by(veracity) %>%
    summarize(
      N = n(),
      Mean = mean(num_comments_in_thread),
      SD = sd(num_comments_in_thread),
      Min = min(num_comments_in_thread),
      Max = max(num_comments_in_thread)
    ) %>% xtable()        

ks.test(ccdfs_both$num_comments_in_thread[ccdfs_both$veracity == "true"], ccdfs_both$num_comments_in_thread[ccdfs_both$veracity == "false"])

# Table S4

ccdfs_both %>% 
  group_by(veracity) %>%
    summarize(
      N = n(),
      Mean = mean(max_depth_comments),
      SD = sd(max_depth_comments),
      Min = min(max_depth_comments),
      Max = max(max_depth_comments)
    ) %>% xtable()        

ks.test(ccdfs_both$max_depth_comments[ccdfs_both$veracity == "true"], ccdfs_both$max_depth_comments[ccdfs_both$veracity == "false"])

# Table S5

ccdfs_both %>% 
  group_by(veracity) %>%
    summarize(
      N = n(),
      Mean = mean(post_life_hours),
      SD = sd(post_life_hours),
      Min = min(post_life_hours),
      Max = max(post_life_hours)
    ) %>% xtable()        

ks.test(ccdfs_both$post_life_hours[ccdfs_both$veracity == "true"], ccdfs_both$post_life_hours[ccdfs_both$veracity == "false"])

# Table S6

ccdfs_directly_commented %>%
  group_by(veracity) %>%
    summarize(
      N = n(),
      Mean = mean(num_commenters),
      SD = sd(num_commenters),
      Min = min(num_commenters),
      Max = max(num_commenters)
    ) %>% xtable()

ks.test(ccdfs_directly_commented$num_commenters[ccdfs_directly_commented$veracity == "true"], ccdfs_directly_commented$num_commenters[ccdfs_directly_commented$veracity == "false"])


# Table S7

ccdfs_directly_commented %>%
  group_by(veracity) %>%
    summarize(
      N = n(),
      Mean = mean(num_comments_in_thread),
      SD = sd(num_comments_in_thread),
      Min = min(num_comments_in_thread),
      Max = max(num_comments_in_thread)
    ) %>% xtable()

ks.test(ccdfs_directly_commented$num_comments_in_thread[ccdfs_directly_commented$veracity == "true"], ccdfs_directly_commented$num_comments_in_thread[ccdfs_directly_commented$veracity == "false"])

# Table S8

ccdfs_directly_commented %>%
  group_by(veracity) %>%
    summarize(
      N = n(),
      Mean = mean(max_depth_comments),
      SD = sd(max_depth_comments),
      Min = min(max_depth_comments),
      Max = max(max_depth_comments)
    ) %>% xtable()

ks.test(ccdfs_directly_commented$max_depth_comments[ccdfs_directly_commented$veracity == "true"], ccdfs_directly_commented$max_depth_comments[ccdfs_directly_commented$veracity == "false"])

# Table S9

ccdfs_directly_commented %>%
  group_by(veracity) %>%
    summarize(
      N = n(),
      Mean = mean(post_life_hours),
      SD = sd(post_life_hours),
      Min = min(post_life_hours),
      Max = max(post_life_hours)
    ) %>% xtable()

ks.test(ccdfs_directly_commented$post_life_hours[ccdfs_directly_commented$veracity == "true"], ccdfs_directly_commented$post_life_hours[ccdfs_directly_commented$veracity == "false"])

# Table S10

ccdfs_same_url %>%
  group_by(veracity) %>%
    summarize(
      N = n(),
      Mean = mean(num_commenters),
      SD = sd(num_commenters),
      Min = min(num_commenters),
      Max = max(num_commenters)
    ) %>% xtable()

ks.test(ccdfs_same_url$num_commenters[ccdfs_same_url$veracity == "true"], ccdfs_same_url$num_commenters[ccdfs_same_url$veracity == "false"])

# Table S11

ccdfs_same_url %>%
  group_by(veracity) %>%
    summarize(
      N = n(),
      Mean = mean(num_comments_in_thread),
      SD = sd(num_comments_in_thread),
      Min = min(num_comments_in_thread),
      Max = max(num_comments_in_thread)
    ) %>% xtable()

ks.test(ccdfs_same_url$num_comments_in_thread[ccdfs_same_url$veracity == "true"], ccdfs_same_url$num_comments_in_thread[ccdfs_same_url$veracity == "false"])

# Table S12

ccdfs_same_url %>%
  group_by(veracity) %>%
    summarize(
      N = n(),
      Mean = mean(max_depth_comments),
      SD = sd(max_depth_comments),
      Min = min(max_depth_comments),
      Max = max(max_depth_comments)
    ) %>% xtable()

ks.test(ccdfs_same_url$max_depth_comments[ccdfs_same_url$veracity == "true"], ccdfs_same_url$max_depth_comments[ccdfs_same_url$veracity == "false"])


# Table S13

ccdfs_same_url %>%
  group_by(veracity) %>%
    summarize(
      N = n(),
      Mean = mean(post_life_hours),
      SD = sd(post_life_hours),
      Min = min(post_life_hours),
      Max = max(post_life_hours)
    ) %>% xtable()

ks.test(ccdfs_same_url$post_life_hours[ccdfs_same_url$veracity == "true"], ccdfs_same_url$post_life_hours[ccdfs_same_url$veracity == "false"])

# Table S14

ccdfs_directly_commented %>% 
  group_by(veracity) %>%
    summarize(
      N = n(),
      Mean = mean(score),
      SD = sd(score),
      Min = min(score),
      Max = max(score)
    ) %>% xtable()        

ks.test(ccdfs_directly_commented$score[ccdfs_both$veracity == "true"], ccdfs_directly_commented$score[ccdfs_both$veracity == "false"])

# Table S15

comments_linking_factcheckers %>% 
  group_by(veracity) %>%
    summarize(
      N = n(),
      Mean = mean(score),
      SD = sd(score),
      Min = min(score),
      Max = max(score)
    ) %>% xtable()        

ks.test(comments_linking_factcheckers$score[comments_linking_factcheckers$veracity == "true"], comments_linking_factcheckers$score[comments_linking_factcheckers$veracity == "false"])


# Test of differences in first fact-checking comment time

median(ccdfs_directly_commented$time_to_first_factcheck[ccdfs_directly_commented$veracity=="true"])
median(ccdfs_directly_commented$time_to_first_factcheck[ccdfs_directly_commented$veracity=="false"])


ks.test(ccdfs_directly_commented$time_to_first_factcheck[ccdfs_both$veracity == "true"], ccdfs_directly_commented$time_to_first_factcheck[ccdfs_both$veracity == "false"])

# Test of differences in deletion by whether receiving a fact-checking comment
t.test(ccdfs_both$is_deleted ~ ccdfs_both$directly_commented)

t.test(ccdfs_both$is_deleted[ccdfs_both$veracity=="false"] ~ ccdfs_both$directly_commented[ccdfs_both$veracity=="false"])
t.test(ccdfs_both$is_deleted[ccdfs_both$veracity=="mixed"] ~ ccdfs_both$directly_commented[ccdfs_both$veracity=="mixed"])
t.test(ccdfs_both$is_deleted[ccdfs_both$veracity=="true"] ~ ccdfs_both$directly_commented[ccdfs_both$veracity=="true"])

# Test of differences in removal by whether receiving a fact-checking comment
t.test(ccdfs_both$is_removed ~ ccdfs_both$directly_commented)

t.test(ccdfs_both$is_removed[ccdfs_both$veracity=="false"] ~ ccdfs_both$directly_commented[ccdfs_both$veracity=="false"])
t.test(ccdfs_both$is_removed[ccdfs_both$veracity=="mixed"] ~ ccdfs_both$directly_commented[ccdfs_both$veracity=="mixed"])
t.test(ccdfs_both$is_removed[ccdfs_both$veracity=="true"] ~ ccdfs_both$directly_commented[ccdfs_both$veracity=="true"])


