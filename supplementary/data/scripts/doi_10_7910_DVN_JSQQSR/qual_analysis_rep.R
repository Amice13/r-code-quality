# R version 4.1.1 (2021-08-10) -- "Kick Things"



install.packages("stopwords", version = "2.3")
install.packages("tidytext", version = "0.3.2")
install.packages("wordcloud", version = "2.6")
install.packages("tidyverse", version = "1.3.1")

library(stopwords)
library(wordcloud)
library(tidyverse)
library(tidytext)

rm(list = ls())
# setwd("") set working directory
sink("Qual_LogFile.txt",type = "output") # Create Log File

text_dataframe <- read_csv("qualitative_analysis_file_REP.csv")

freq_dataframe_t <- text_dataframe %>% filter(treatment==1) %>%
  unnest_tokens(word, answer) %>% filter(!(word %in% stopwords("en")))
freq_dataframe_c <- text_dataframe %>% filter(treatment==0) %>%
  unnest_tokens(word, answer) %>% filter(!(word %in% stopwords("en")))
count_dataframe_t = freq_dataframe_t %>% filter(question_id==6) %>% count(word)
count_dataframe_c = freq_dataframe_c %>% filter(question_id==4|question_id==5) %>% count(word) %>%
  filter(!(word=="it’s")) %>%
  filter(!(word=="don’t")) %>%
  filter(!(word=="yes"))



pdf("wordcloud_Q6_T.pdf") 
set.seed(123)
wordcloud(words = count_dataframe_t$word, freq = count_dataframe_t$n, max.words = 50)
dev.off()

pdf("wordcloud_Q6_C.pdf") 
set.seed(123)
wordcloud(words = count_dataframe_c$word, freq = count_dataframe_c$n, max.words = 50)
dev.off()

