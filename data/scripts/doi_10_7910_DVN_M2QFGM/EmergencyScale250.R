#############################################################
# Project:  Supranational emergency politics?
# Task:     Develop 'emergency' scaling dictionaries from HoC
#           word embedding model
# Author:   Christian Rauh (13.04.2021)
#############################################################

# R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"

# Packages ####
library(tidyverse) # 1.3.0
library(text2vec) # 0.6
library(quanteda) # 2.1.2

`%notin%` <- Negate(`%in%`)


# HoC Word Vectors ####
# 300 dims, window of 10 context words
# Note: has been created in R < 3.5, save version 2
load("./HoCWordVectors/HoC_WordVectors_Context10_300d.Rdata")


# Seed word approach ####
# Inspired by Rheault https://www.researchgate.net/publication/311990204_Expressions_of_Anxiety_in_Political_Texts
# and Rheault et al.: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0168843

# Define  discourse seeds 
emergency <- c("crisis", "emergency", "danger", "peril", "hazard", "threat", "risk", "disaster", "uncertainty", "uncertain")
normality <- c("normality", "normal", "safety", "stability", "regularity", "routine", "calm", "usual", "certainty", "certain")

# Check whether these words have actually occurred in the HoC corpus
emergency %in% row.names(word_vectors)
normality %in% row.names(word_vectors)


# Similarity to emergency terms
target.vectors <- word_vectors[emergency, ] 
emergency.vector <- target.vectors %>% # Mean vector of emergency terms
  t %>% as.data.frame %>% 
  mutate(concept_mean = rowMeans(.)) %>% 
  select(concept_mean) %>% 
  t %>% as.matrix
emergency.sim <- sim2(word_vectors, emergency.vector, method = "cosine", norm = "l2") %>% 
  as.data.frame() %>% 
  rename(emergency.similarity = concept_mean) %>% 
  rownames_to_column(var = "term") %>% 
  arrange(desc(emergency.similarity))


# Similarity to normality terms
target.vectors <- word_vectors[normality, ] 
normality.vector <- target.vectors %>% # Mean vector of normality terms
  t %>% as.data.frame %>% 
  mutate(concept_mean = rowMeans(.)) %>% 
  select(concept_mean) %>% 
  t %>% as.matrix
normality.sim <- sim2(word_vectors, normality.vector, method = "cosine", norm = "l2") %>% 
  as.data.frame() %>% 
  rename(normality.similarity = concept_mean) %>% 
  rownames_to_column(var = "term") %>% 
  arrange(desc(normality.similarity))

rm(target.vectors)


# Scale: Term-level differences in similarity to normality and emergency

nc.scale <- merge(emergency.sim, normality.sim) %>% # merge based on terms
  mutate(diff = emergency.similarity - normality.similarity) %>% # higher values indicate emergency
  arrange(desc(diff))

hist(nc.scale$diff)

# Clean up
nc <- nc.scale %>% 
  filter(!(term %in% stopwords("english"))) %>% # There are none !? (x_max setting!?)
  filter(str_detect(term, "[a-z]")) %>% # Real words, exclude symbols etc
  filter(!(str_detect(term, "â€"))) %>% # Encoding errors
  filter(nchar(term) > 1) %>% 
  arrange(desc(diff)) # Descending order of diff (closeness to emergency seeds)

# Get most extreme words
# 250 on each side, similar to Rheault
# Distirbution is symmetrical, implies cutoff at ~ +/- .2

nc.dict <- rbind(head(nc, 250), tail(nc, 250))
mean(nc.dict$diff)
hist(nc.dict$diff)
nc.dict$nc <- (nc.dict$diff - min(nc.dict$diff)) / (max(nc.dict$diff) - min(nc.dict$diff))
hist(nc.dict$nc)
mean(nc.dict$nc)


# Export ###
# Note that minor precision differences might ensue due to rounding in differetn environments
# For your reference, I export a separate file ("_repl") so that you can inspect differences to originally used file also contained here
# write.csv2(nc.dict, "./HoCWordVectors/EmergencyScale250.csv", row.names = F)
write.csv2(nc.dict, "./HoCWordVectors/EmergencyScale250_repl.csv", row.names = F)
