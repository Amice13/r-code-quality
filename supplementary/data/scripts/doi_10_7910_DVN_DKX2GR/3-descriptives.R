##### Script Purpose ------------------------------ 
# This script simply notes the descriptive statistics of the corpus that  
#   was analyzed. It was actually generated in script 1 of this project
# Requires the wrangled_df.rds from prior script

##### Load Packages ------------------------------ 
library(tidyverse)

##### Import Data ------------------------------ 
df <- readRDS("wrangled_df.rds")


##### Study Descriptives ------------------------------ 

# Before tidying, the full data had 71,399 words before removing stop words, punctuation, and numbers
# the quanteda package has a list of 175 english stopwords
# After removing stop words, 35,523 words remaining
#    All of this is computed in the first script

# This command verifies that the tidied dataset has the same 35,523 words
# sum(df$n_tokens)

