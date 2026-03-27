##Text Analysis

#Clear Environment
rm(list=ls())

#Load packages (install first if not already installed)
library(tidyverse)
library(tm)
library(tidytext)


setwd("YourDirectory\\replication_package_final")

text<-read.csv("Raw Data\\text_responses.csv")

###Processing

text$com<-gsub("nya", "", text$com) ## -nya is a suffix showing ownership ("sawahnya" means "his rice fields")

###Make sure words are properly spaced, not connected by punctuation
text$com<-gsub("-", "", text$com)
text$com<-gsub("/", " ", text$com)
text$com<-gsub(",", " ", text$com)
text$com<-gsub("\\(", " ", text$com)
text$com<-gsub(")", " ", text$com)
text$com<-gsub("\\+", " ", text$com)
text$com<-gsub("_", " ", text$com)
text$com<-gsub("\n", " ", text$com)
text$com<-gsub("2", " ", text$com)
text$com<-gsub("\\.", " ", text$com)

text$com<-gsub("rumah tangga", "", text$com) #"rumah tangga" means household, "rumah" means house. Former is not likely informative but latter is.

textCorpus <- Corpus(VectorSource(text$com)) 
textDTM <- DocumentTermMatrix(textCorpus)
textDTM_tidy <- tidy(textDTM)

custom_stop_words <- tibble(word = c("jumlah","ada", "adalah", "ahmad", "apa", "atau", "dan", "dari", "dengan", "ini", "itu", "jadi", "juga", "kalau", "karena", "lain", "pada", "sehingga", "slamet", "untuk", "yang", "pak", "pu", "tidak", "lebih", "sudah", "bak", "sedangkan", "amat", "sdh", "belum", "blm", "masih", "sama", "sedikit", "yg"))
##Stop words are not meaningful, for example: because, is, or, etc.

textDTM_tidy_cleaned <- textDTM_tidy %>% 
  anti_join(custom_stop_words, by = c("term" = "word")) 

counts<-textDTM_tidy_cleaned%>%
  group_by(term)%>%
  summarize(total = sum(count, na.rm = T))

counts<-counts %>% arrange(desc(total))

stargazer(counts[c(1:20),], summary = F)
