## Terrorism, Trust, and Identity ##
 
## Media Text Analysis
## (with thanks to Laure Bokobza for writing the script)

#install.packages('BiocManager')
#install.packages("devtools")
#devtools::install_github("vincentbagilet/mediocrethemes")

library(ggplot2)
library(mediocrethemes)

options(scipen = 99)
pacman::p_load(pdftools, dplyr, stringr, readr, purrr, tm, quanteda,
               tidytext, tidyverse, forcats, stringr, mediocrethemes, qdapRegex,
               gridExtra, ggpubr)
set_mediocre_all()

setwd("Place path to Replication Materials folder here")

# load text data
text_data_all <- readRDS("text_data_all.rds")
text_firstpage_all <- readRDS("first_page_all.rds")
text_first5pages_all <- readRDS("first_5pages_all.rds")
text_first10pages_all <- readRDS("first_10pages_all.rds")

## 
## 0. Data cleaning 
## 

# removing stopwords
data(stop_words) # load common stopwords English

custom_stopwords <- as.data.frame(c("guardian", "theguardian", "daily", "dailytrust", "november", "december", "page", "pages", "volume", "vol", "today", "yesterday", "day", "dailytrust.com")) %>% 
  rename(word = `c("guardian", "theguardian", "daily", "dailytrust", "november", "december", "page", "pages", "volume", "vol", "today", "yesterday", "day", "dailytrust.com")`) %>% 
  mutate(lexicon = "CUSTOM") # custom stopwords

stop_words <- rbind(stop_words, custom_stopwords)

# remove numbers, punctuation and stopwords
tidy_textdata_all <- text_data_all %>%
  mutate(#word = gsub(x = word, "(s?)(f|ht)tp(s?)://\\S+\\b", replacement = ""),
         word = rm_url(word),
         word = gsub(x = word, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = "")) %>% # remove URLs and punctuation and numbers
  filter(!is.na(word) & word != "") %>% 
  anti_join(stop_words) %>% 
  mutate(month = substr(date, 4, 5),
         date = as.Date(date, "%d/%m/%Y"))

tidy_textdata_firstpage <- text_firstpage_all %>%
  mutate(#word = gsub(x = word, "(s?)(f|ht)tp(s?)://\\S+\\b", replacement = ""),
         word = rm_url(word),
         word = gsub(x = word, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = "")) %>% # remove URLs and punctuation and numbers
  filter(!is.na(word) & word != "") %>% 
  anti_join(stop_words) %>% 
  mutate(month = substr(date, 6, 7))

tidy_textdata_first5pages <- text_first5pages_all %>%
  mutate(#word = gsub(x = word, "(s?)(f|ht)tp(s?)://\\S+\\b", replacement = ""),
        word = rm_url(word),
         word = gsub(x = word, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = "")) %>% # remove URLs and punctuation and numbers
  filter(!is.na(word) & word != "") %>% 
  anti_join(stop_words) %>% 
  mutate(month = substr(date, 6, 7))

tidy_textdata_first10pages <- text_first10pages_all %>%
  mutate(#word = gsub(x = word, "(s?)(f|ht)tp(s?)://\\S+\\b", replacement = ""),
    word = rm_url(word),
    word = gsub(x = word, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = "")) %>% # remove URLs and punctuation and numbers
  filter(!is.na(word) & word != "") %>% 
  anti_join(stop_words) %>% 
  mutate(month = substr(date, 6, 7))


# sanity check: words that appear the most 
plot_commonwords_all <- tidy_textdata_all %>%
  count(word, sort = TRUE) %>%
  filter(n > 3000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL,
       title = "Most common words (all issues)")
plot_commonwords_all

plot_commonwords_firstpage <- tidy_textdata_firstpage %>%
  count(word, sort = TRUE) %>%
  filter(n > 40) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL,
       title = "Most common words (first page)")
plot_commonwords_firstpage

## 
## Data wrangling 
## 

# convert to tf-idf
# for all issues, all pages
tidy_textdata_all_words <- tidy_textdata_all %>% 
  count(date, word, sort = TRUE)

tidy_textdata_all_words <- tidy_textdata_all %>% 
  group_by(date, word) %>% 
  summarise(n = n(), 
            date = date, 
            month = month) %>% 
  filter(row_number()==1) 

tidy_textdata_all_words_tot <- tidy_textdata_all_words %>% 
  group_by(date) %>% 
  summarize(total = sum(n))

tidy_textdata_all_words <- left_join(tidy_textdata_all_words, tidy_textdata_all_words_tot) %>% 
  arrange(-n)

# convert to tf-idf
# for all issues, first page only
tidy_textdata_firstpage_words <- tidy_textdata_firstpage %>% 
  count(date, word, sort = TRUE)

tidy_textdata_firstpage_words <- tidy_textdata_firstpage %>% 
  group_by(date, word) %>% 
  summarise(n = n(), 
            date = date, 
            month = month) %>% 
  filter(row_number()==1) 

tidy_textdata_firstpage_words_tot <- tidy_textdata_firstpage_words %>% 
  group_by(date) %>% 
  summarize(total = sum(n))

tidy_textdata_firstpage_words <- left_join(tidy_textdata_firstpage_words, tidy_textdata_firstpage_words_tot) %>% 
  arrange(-n)

# convert to tf-idf
# for all issues, first 5 pages 
tidy_textdata_first5pages_words <- tidy_textdata_first5pages %>% 
  count(date, word, sort = TRUE)

tidy_textdata_first5pages_words <- tidy_textdata_first5pages %>% 
  group_by(date, word) %>% 
  summarise(n = n(), 
            date = date, 
            month = month) %>% 
  filter(row_number()==1) 

tidy_textdata_first5pages_words_tot <- tidy_textdata_first5pages_words %>% 
  group_by(date) %>% 
  summarize(total = sum(n))

tidy_textdata_first5pages_words <- left_join(tidy_textdata_first5pages_words, tidy_textdata_first5pages_words_tot) %>% 
  arrange(-n)


# convert to tf-idf
# for all issues, first 10 pages 
tidy_textdata_first10pages_words <- tidy_textdata_first10pages %>% 
  count(date, word, sort = TRUE)

tidy_textdata_first10pages_words <- tidy_textdata_first10pages %>% 
  group_by(date, word) %>% 
  summarise(n = n(), 
            date = date, 
            month = month) %>% 
  filter(row_number()==1) 

tidy_textdata_first10pages_words_tot <- tidy_textdata_first10pages_words %>% 
  group_by(date) %>% 
  summarize(total = sum(n))

tidy_textdata_first10pages_words <- left_join(tidy_textdata_first10pages_words, tidy_textdata_first10pages_words_tot) %>% 
  arrange(-n)



## 
## 1. Dictionary analysis 
##

## 1. Create dictionaries 
primary_dict_general <- c("pdp", "peoples democratic party", "party", "namadi", "sambo", "president", "president", "jonathan", "primary", "election", "apc", "opposition", "buhari", "result", "winner", "mate")
primary_dict_pdp <- c("pdp", "peoples democratic party", "vice-president", "vice", "president", "party", "namadi", "sambo", "president", "goodluck", "jonathan", "primary", "election", "result", "winner", "mate", "candidate")
primary_dict_apc <- c("apc", "all progressives congress", "opposition", "party", "muhammadu", "buhari","primary", "election", "result", "winner", "mate", "candidate")
# primary_dict_hausa <- 
# primary_dict_fulani <-
terror_dict_general <- c("terrorist", "terrorism", "terror", "extremist", "bomb", "boko", "haram", "boko haram", "attack", "bombing", "bomber",
                         "civilians", "blast", "kill", "suicide", "injured", "injuries", "gunmen", "gunman", "destroy", "dead", "victim", "casualty")
terror_dict_specific <- c("terrorist", "terrorism", "terror", "extremist", "bomb", "boko", "haram", "boko haram", "attack", "bombing", "bomber", 
                          "civilians", "blast", "kill", "suicide", "injured", "injuries", "gunmen", "gunman", "petrol bomb","convoy", "destroy", "car bomb", "dead", "victim", "casualty",
                          "kantin", "kwari", "textile market", "kano", "terminus", "gajiganna", "jos", "terminus", "amhadu bello way")

# keep only words that refer to each dictionary
terror_articles_general <- tidy_textdata_all[str_detect(tidy_textdata_all$word, paste(terror_dict_general, collapse = "|")), ] %>% 
  mutate(dict = "terror (general)")
terror_articles_specific <-  tidy_textdata_all[str_detect(tidy_textdata_all$word, paste(terror_dict_specific, collapse = "|")), ] %>% 
  mutate(dict = "terror (specific)")
# old one terror_articles_test<-  tidy_textdata_all[str_detect(tidy_textdata_all$word, terror_dict_test), ] %>% 
# mutate(dict = "terror (specific)")
primary_articles_general <- tidy_textdata_all[str_detect(tidy_textdata_all$word, paste(primary_dict_general, collapse = "|")), ] %>% 
  mutate(dict = "primary (general)")
primary_articles_pdp <- tidy_textdata_all[str_detect(tidy_textdata_all$word, paste(primary_dict_pdp, collapse = "|")), ] %>% 
  mutate(dict = "primary (PDP)")
primary_articles_apc <- tidy_textdata_all[str_detect(tidy_textdata_all$word, paste(primary_dict_apc, collapse = "|")), ] %>% 
  mutate(dict = "primary (APC)")

terror_firstpage_general <- tidy_textdata_firstpage[str_detect(tidy_textdata_firstpage$word, paste(terror_dict_general, collapse = "|")), ] %>% 
  mutate(dict = "terror (general)")
terror_firstpage_specific <- tidy_textdata_firstpage[str_detect(tidy_textdata_firstpage$word, paste(terror_dict_specific, collapse = "|")),] %>% 
  mutate(dict = "terror (specific)")
primary_firstpage_general <- tidy_textdata_firstpage[str_detect(tidy_textdata_firstpage$word, paste(primary_dict_general, collapse = "|")), ] %>% 
  mutate(dict = "primary (general)")
primary_firstpage_pdp <- tidy_textdata_firstpage[str_detect(tidy_textdata_firstpage$word, paste(primary_dict_pdp, collapse = "|")), ] %>% 
  mutate(dict = "primary (PDP)")
primary_firstpage_apc <- tidy_textdata_firstpage[str_detect(tidy_textdata_firstpage$word, paste(primary_dict_apc, collapse = "|")), ] %>% 
  mutate(dict = "primary (APC)")

terror_first5pages_general <- tidy_textdata_first5pages[str_detect(tidy_textdata_first5pages$word, paste(terror_dict_general, collapse = "|")), ] %>% 
  mutate(dict = "terror (general)")
terror_first5pages_specific <- tidy_textdata_first5pages[str_detect(tidy_textdata_first5pages$word, paste(terror_dict_specific, collapse = "|")),] %>% 
  mutate(dict = "terror (specific)")
primary_first5pages_general <- tidy_textdata_first5pages[str_detect(tidy_textdata_first5pages$word, paste(primary_dict_general, collapse = "|")), ] %>% 
  mutate(dict = "primary (general)")
primary_first5pages_pdp <- tidy_textdata_first5pages[str_detect(tidy_textdata_first5pages$word, paste(primary_dict_pdp, collapse = "|")), ] %>% 
  mutate(dict = "primary (PDP)")
primary_first5pages_apc <- tidy_textdata_first5pages[str_detect(tidy_textdata_first5pages$word, paste(primary_dict_apc, collapse = "|")), ] %>% 
  mutate(dict = "primary (APC)")

terror_first10pages_general <- tidy_textdata_first10pages[str_detect(tidy_textdata_first10pages$word, paste(terror_dict_general, collapse = "|")), ] %>% 
  mutate(dict = "terror (general)")
terror_first10pages_specific <- tidy_textdata_first10pages[str_detect(tidy_textdata_first10pages$word, paste(terror_dict_specific, collapse = "|")),] %>% 
  mutate(dict = "terror (specific)")
primary_first10pages_general <- tidy_textdata_first10pages[str_detect(tidy_textdata_first10pages$word, paste(primary_dict_general, collapse = "|")), ] %>% 
  mutate(dict = "primary (general)")
primary_first10pages_pdp <- tidy_textdata_first10pages[str_detect(tidy_textdata_first10pages$word, paste(primary_dict_pdp, collapse = "|")), ] %>% 
  mutate(dict = "primary (PDP)")
primary_first10pages_apc <- tidy_textdata_first10pages[str_detect(tidy_textdata_first10pages$word, paste(primary_dict_apc, collapse = "|")), ] %>% 
  mutate(dict = "primary (APC)")

dict_articles <- rbind(terror_articles_general, terror_articles_specific, primary_articles_general, primary_articles_pdp, primary_articles_apc)
dict_firstpage <- rbind(terror_firstpage_general, terror_firstpage_specific, primary_firstpage_general, primary_firstpage_pdp, primary_firstpage_apc)
dict_first5pages <- rbind(terror_first5pages_general, terror_first5pages_specific, primary_first5pages_general, primary_first5pages_pdp, primary_first5pages_apc)
dict_first10pages <- rbind(terror_first10pages_general, terror_first10pages_specific, primary_first10pages_general, primary_first10pages_pdp, primary_first10pages_apc)

issue_nbwords <- tidy_textdata_all_words %>% 
  group_by(date) %>% 
  select(c("date","total")) %>% 
  filter(row_number()==1) # number of words by journal issue 

firstpage_nbwords <- tidy_textdata_firstpage_words %>% 
  group_by(date) %>% 
  select(c("date","total")) %>% 
  filter(row_number()==1) # number of words by first page 

first5pages_nbwords <- tidy_textdata_first5pages_words %>% 
  group_by(date) %>% 
  select(c("date","total")) %>% 
  filter(row_number()==1) # number of words by first 5 pages

first10pages_nbwords <- tidy_textdata_first10pages_words %>% 
  group_by(date) %>% 
  select(c("date","total")) %>% 
  filter(row_number()==1) # number of words by first 10 pages 

dict_articles <- merge(dict_articles, issue_nbwords, by = "date", all.x = T)
dict_firstpage <- merge(dict_firstpage,firstpage_nbwords,  by = "date", all.x = T)
dict_first5pages <-  merge(dict_first5pages,first5pages_nbwords,  by = "date", all.x = T)
dict_first10pages <-  merge(dict_first10pages,first10pages_nbwords,  by = "date", all.x = T)

# compute share of each dict by issue
share_dict <- dict_articles %>% 
  group_by(date, dict, total) %>% 
  summarise(n = n()) %>% 
  mutate(share = n/total) %>% 
  group_by(date, dict) %>% 
  summarise(mean_share = mean(share))

share_dict_newspaper <- dict_articles %>% 
  group_by(date, dict, newspaper, total) %>% 
  summarise(n = n()) %>% 
  mutate(share = n/total) %>% 
  group_by(date, dict, newspaper) %>% 
  summarise(mean_share = mean(share))

# compute share of each dict by first page
share_firstpage <- dict_firstpage %>% 
  group_by(date, dict, total) %>% 
  summarise(n = n()) %>% 
  mutate(share = n/total) %>% 
  group_by(date, dict) %>% 
  summarise(mean_share = mean(share))

share_firstpage_newspaper <- dict_firstpage %>% 
  group_by(date, dict, newspaper, total) %>% 
  summarise(n = n()) %>% 
  mutate(share = n/total) %>% 
  group_by(date, dict, newspaper) %>% 
  summarise(mean_share = mean(share))

# compute share of each dict by first 5 pages
share_first5pages <- dict_first5pages %>% 
  group_by(date, dict, total) %>% 
  summarise(n = n()) %>% 
  mutate(share = n/total) %>% 
  group_by(date, dict) %>% 
  summarise(mean_share = mean(share))

share_first5pages_newspaper <- dict_first5pages %>% 
  group_by(date, dict, newspaper, total) %>% 
  summarise(n = n()) %>% 
  mutate(share = n/total) %>% 
  group_by(date, dict, newspaper) %>% 
  summarise(mean_share = mean(share))

# compute share of each dict by first 10 pages
share_first10pages <- dict_first10pages %>% 
  group_by(date, dict, total) %>% 
  summarise(n = n()) %>% 
  mutate(share = n/total) %>% 
  group_by(date, dict) %>% 
  summarise(mean_share = mean(share))

share_first10pages_newspaper <- dict_first10pages %>% 
  group_by(date, dict, newspaper, total) %>% 
  summarise(n = n()) %>% 
  mutate(share = n/total) %>% 
  group_by(date, dict, newspaper) %>% 
  summarise(mean_share = mean(share))

## 2. Plot 
## loess smoothed ## 
smooth_all <- ggplot(share_dict, aes(date, mean_share, group = dict, color = dict)) +
  geom_point(size = 0.75) +
  geom_smooth(span = 0.25, alpha = 0.10) +
  geom_vline(xintercept = as.numeric(share_dict$date[c(71, 76)]), 
             linetype = 4, color = "black") +
  geom_vline(xintercept = as.numeric(share_dict$date[c(11, 27, 91, 131)]), 
             linetype = 4, color = "grey") +
  labs(y = "% of words about topic",
       title = "Coverage of primaries and attacks (all pages)") +
  theme_mediocre() +
  scale_mediocre_d(pal = "coty")

smooth_firstpage <- ggplot(share_firstpage, aes(date, mean_share, group = dict, color = dict)) +
  geom_point(size = 0.75) +
  geom_smooth(span = 0.25, alpha = 0.10) +
  geom_vline(xintercept = as.numeric(share_dict$date[c(71, 76)]), 
             linetype = 4, color = "black") +
  geom_vline(xintercept = as.numeric(share_dict$date[c(11, 27, 91, 131)]), 
             linetype = 4, color = "grey") +
  labs(y = "% of words about topic",
       title = "Coverage of primaries and attacks (first page only)") +
  theme_mediocre() +
  scale_mediocre_d(pal = "coty")

smooth_first5pages <- ggplot(share_first5pages, aes(date, mean_share, group = dict, color = dict)) +
  geom_point(size = 0.75) +
  geom_smooth(span = 0.25, alpha = 0.10) +
  geom_vline(xintercept = as.numeric(share_dict$date[c(71, 76)]), 
             linetype = 4, color = "black") +
  geom_vline(xintercept = as.numeric(share_dict$date[c(11, 27, 91, 131)]), 
             linetype = 4, color = "grey") +
  labs(y = "% of words about topic",
       title = "Coverage of primaries and attacks (first 5 pages)") +
  theme_mediocre() +
  scale_mediocre_d(pal = "coty")

smooth_first10pages <- ggplot(share_first10pages, aes(date, mean_share, group = dict, color = dict)) +
  geom_point(size = 0.75) +
  geom_smooth(span = 0.25, alpha = 0.10) +
  geom_vline(xintercept = as.numeric(share_dict$date[c(71, 76)]), 
             linetype = 4, color = "black") +
  geom_vline(xintercept = as.numeric(share_dict$date[c(11, 27, 91, 131)]), 
             linetype = 4, color = "grey") +
  labs(y = "% of words about topic",
       title = "Coverage of primaries and attacks (first 10 pages)") +
  theme_mediocre() +
  scale_mediocre_d(pal = "coty")


plot_smooth_all <- ggarrange(smooth_all, smooth_firstpage, smooth_first5pages,smooth_first10pages,
          ncol = 2, nrow = 2,
          common.legend = TRUE, legend = "bottom")

plot_smooth_all

ggsave(plot_smooth_all, 
       file = "SI_Output/Figure_B14_plot_smooth_all.png",
       width = 12,
       height = 10,
       dpi = 1200)

## 
## 2. Tf-idf analysis 
##

# reference: https://alexluscombe.ca/blog/exploratory-data-analysis-using-tf-idf/

# first 10 pages
tidy_textdata_first10pages_words_smallwindow <- tidy_textdata_first10pages_words %>% 
  arrange(date) %>% 
  filter(date >= as.Date("2014-12-08") & date <= as.Date("2014-12-15"))

plot_tfidf_first10pages <- tidy_textdata_first10pages_words_smallwindow %>% 
  bind_tf_idf(word, date, n) %>% #add tf-idf calculations to our df
  arrange(desc(tf_idf)) %>%  #arrange by tf-idf score
  group_by(date) %>% 
  slice_max(tf_idf, n = 5) %>%  #keep only words with highest 10 tf-idf scores for each page
  ungroup() %>% #ungroup before plotting
  mutate(date_label = as.factor(date)) %>%
  mutate(date_label = fct_reorder(date_label, date)) %>%
  mutate(word = fct_reorder(word, tf_idf)) %>%
  ggplot(aes(y = word, x = tf_idf, fill = date_label)) +
  theme_mediocre() +
  scale_mediocre_d(pal = "coty")+
  geom_col() +
  facet_wrap(~ date_label, scales = "free", ncol = 2) +
  theme(legend.position = "") +
  labs(title = "Most important words in the 10 first pages\nin the days surrounding the attacks (tf-idf metric)",
       y = "",
       x = "tf-idf")

plot_tfidf_first10pages

ggsave(plot_tfidf_first10pages, 
       file = "SI_Output/Figure_B15_plot_tfidf_first10pages.png", 
       width = 8,
       height = 12)
