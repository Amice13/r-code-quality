####################################################
# Project:  Supranational emergency politics?
# Task:     Train a GloVe word embedding model on the 
#           House of Commons corpus (ParlSpeech V2)
# Author:   Christian Rauh (04.09.2020)
####################################################

# Note: For computational power reasons, this script has been run on an 
# external/remote machine (64 cores / 256 GB RAM) - and thus outside 
# the overall project and with some peculiarities commented throughout

# R version 3.3.3 (2017-03-06) -- "Another Canoe"

# Packages ####
library(tidyverse) # 1.2.1
library(text2vec) # 0.5.1 # http://text2vec.org/glove.html


# Set WD manually - Adapt to your environment ####
# Remote machine didn't accept RStudio projects
setwd("~/Desktop/SSD Drive/rauh/WordVectors")

start <- Sys.time()


# Corpus ####
# HoC speeches from ParlSpech V2 - premanently available at https://doi.org/10.7910/DVN/L4OAKN
# Siuce remote machine features an older R version only, 
# manually stored in save version 2 rds beforehand
corp <- read_rds("./Corpora/SV2_Corp_HouseOfCommons_V2.rds") %>% 
  filter(chair == F) # Exclude organisational speeches from the chair

texts <- tolower(corp$text) %>% # Character vector, lower case
  str_replace_all("[[:punct:]]", " ") %>% # Remove punctuation
  str_replace_all("[0-9]", " ") %>%  # Remove numbers
  str_replace_all("  ", " ") # Double white spaces to one

# sample <- texts[sample(1:length(texts), 1000, replace = F)] # for testing purposes
sample <- texts

# Clean up 
rm(corp)
gc()


# Prepare vocabulary ####

# Create iterator over tokens
tokens <- space_tokenizer(sample)

# Create vocabulary (unigrams)
it <- itoken(tokens, progressbar = T)
vocab <- create_vocabulary(it)
# Consider only words appearing at least 100 times
vocab <- prune_vocabulary(vocab, term_count_min = 100L)



# Prepare term co-occurence matrix ####

# Use our filtered vocabulary
vectorizer <- vocab_vectorizer(vocab)
# use window of 10 for context words (takes some time)
tcm <- create_tcm(it, vectorizer, skip_grams_window = 10L)


# Fit GloVe model ####

# 300 dimensions
# check the maximum number of co-occurrences (GloVe paper, p. 4)
glove = GlobalVectors$new(word_vectors_size = 300, vocabulary = vocab, x_max = 10) 
wv_main <- glove$fit_transform(tcm, n_iter = 100, convergence_tol = 0.01)


# model learns two sets of word vectors - main and context
# both can be used as result, but it usually better (idea from GloVe paper) 
# to average or take a sum of main and context vector
dim(wv_main)
wv_context <- glove$components
dim(wv_context)
word_vectors = wv_main + t(wv_context)


# Export ####
save(word_vectors, file = "./PretrainedVectors/HoC/HoC_WordVectors_Context10_300d.Rdata")

end <- Sys.time()
end-start # Time difference of 36.50825 mins (on the remote machine - WZB THEIA)