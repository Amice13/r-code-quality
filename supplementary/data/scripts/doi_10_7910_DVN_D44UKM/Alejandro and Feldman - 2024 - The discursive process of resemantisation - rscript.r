library(readtext)
library(quanteda)
library(tidytext)
library(ggplot2)
library(magrittr)
library(dplyr)
library(topicmodels)
library(tidyr)
library(igraph)
library(ggraph)
library(scales)
library(widyr)
library(readr)
library(wordcloud)
library(reshape2)

male <- readtext("~/Documents/Dissertation/malecorpus/")
male_corpus <- corpus(male)
male_tokens <- tokens(male_corpus)
male_dfm <- dfm(male_tokens)
male_dtm <- as.DocumentTermMatrix(male_dfm)

male_spread <- read_csv("~/Documents/Dissertation/Dissertation.csv")
male_corpus$documents$communicationQ <- male_spread$`Communication?`
male_corpus$documents$campaignQ <- male_spread$`Campaign materials?`

male_subcorpus0 <- corpus_subset(male_corpus, male_corpus$documents$communicationQ == 0)
male_subcorpus1 <- corpus_subset(male_corpus, male_corpus$documents$communicationQ == 1)

# SUBCORPUS 0

male_subcorpus0.tokens <- tokens(male_subcorpus0,
                               remove_numbers = TRUE,
                               remove_punct = TRUE,
                               remove_symbols = TRUE,
                               remove_separators = TRUE,
                               remove_twitter = TRUE,
                               remove_hyphens = TRUE,
                               remove_url = TRUE)

male_subcorpus0.dfm <- dfm(male_subcorpus0.tokens,
                           stem = TRUE,
                           remove = c(stopwords("english"),
                                      "will", 
                                      "pdf",
                                      "aspx",
                                      "?",
                                      "??",
                                      "*[àáâéèêçîïûùú]*",
                                      "*[0-9]*",
                                      "*_*",
                                      "*.*",
                                      "*'*"))

male_subcorpus0.tidy <- tidy(male_subcorpus0.dfm)
male_subcorpus0.tidy$communication <- "zero"

# SUBCORPUS 1

male_subcorpus1.tokens <- tokens(male_subcorpus1,
                                 remove_numbers = TRUE,
                                 remove_punct = TRUE,
                                 remove_symbols = TRUE,
                                 remove_separators = TRUE,
                                 remove_twitter = TRUE,
                                 remove_hyphens = TRUE,
                                 remove_url = TRUE)

male_subcorpus1.dfm <- dfm(male_subcorpus1.tokens,
                           stem = TRUE,
                           remove = c(stopwords("english"),
                                      "will", 
                                      "pdf",
                                      "aspx",
                                      "?",
                                      "??",
                                      "*[àáâéèêçîïûùú]*",
                                      "*[0-9]*",
                                      "*_*",
                                      "*.*",
                                      "*'*"))

male_subcorpus1.tidy <- tidy(male_subcorpus1.dfm)
male_subcorpus1.tidy$communication <- "one"

# COMPARING 0 VS 1

total_tidy01 <- full_join(male_subcorpus0.tidy, male_subcorpus1.tidy)

total_frequency <- total_tidy01 %>% 
  group_by(communication) %>% 
  count(term, wt = count, sort = TRUE) %>% 
  left_join(total_tidy01 %>% 
              group_by(communication) %>% 
              summarise(total = n())) %>% 
  mutate(freq = n/total)

total_frequency <- total_frequency %>% 
  select(communication, term, freq) %>% 
  spread(communication, freq) %>% 
  arrange(zero, one)

ggplot(total_frequency, aes(zero, one)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = term), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")

total_ratios <- total_tidy01 %>%
  count(term, communication) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  spread(communication, n, fill = 0) %>%
  mutate_if(is.numeric, funs((. + 1) / sum(. + 1))) %>%
  mutate(logratio = log(zero / one)) %>%
  arrange(desc(logratio))

total_ratios %>% 
  arrange(abs(logratio))

total_ratios %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, logratio)) %>%
  ggplot(aes(term, logratio, fill = logratio < 0)) +
  geom_col() +
  coord_flip() +
  ylab("log odds ratio (zero/one)") +
  scale_fill_discrete(name = "", labels = c("zero", "one"))