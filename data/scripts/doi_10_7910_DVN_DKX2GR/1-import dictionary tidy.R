##### Script Purpose ------------------------------ 
# Imports all of our data
#   and then runs it through the dictionary analysis.
# Saves the end result as an R object to be referenced by future scripts
# 
# Following the quanteda quick start guide to figure this stuff out
# https://cran.r-project.org/web/packages/quanteda/vignettes/quickstart.html



##### Load Packages ------------------------------ 
library(tidyverse)
library(quanteda)
library(readtext)
library(tidytext)



##### import data ------------------------------ 
# Read our data from a folder all at once, using readtext 
df_import <- readtext(file = "data-import", text_field = "text") 

# The Death Penalty Denomination statements are dropped from analysis for this paper
df_import <- df_import %>% 
  filter(issue != "deathpenaltydenom")



##### Creating a quanteda corpus ------------------------------ 
# Each referendum will be a "document" and are combined together as a "corpus"
my_corpus <- corpus(df_import)

# Word count for each document within the corpus
# sum(ntoken(my_corpus)) 
#   71399 words total, prior to removing stopwords



##### Tokenizing texts ------------------------------ 
# Now, time to tokenize it, and move towards LIWC style analysis. 

# Command below will show you the stopword list
# stopwords(language = "english")
# length(stopwords(language = "english"))
#   quanteda uses a list of 175 english stop words


# Creating a document-feature matrix
my_dfm <- dfm(my_corpus, 
              remove = stopwords("english"), 
              remove_punct = TRUE,
              remove_numbers = TRUE)
# sum(ntoken(my_dfm)) 
# 37809 words total, after removing stopwords
# warnings in the code are related to deprecated functions



##### Grouping Word by dictionary ------------------------------ 
# load Moral Foundations Dictionary version 2.0 (not yet published, see Jeremy Frimer)
dict_mfd <- dictionary(file = "dictionary/mfd2.0.dic")

# Create a new DFM object that uses the dictionary from above
my_dfm_mfd <- dfm(my_corpus, dictionary = dict_mfd)



##### Tidy df ------------------------------ 

# Get the word counts (ntoken) for each document, after removing stopwords
df_moral <- tidy(my_dfm_mfd) %>% 
  rename(doc_id = "document") %>% 
  spread(key = term, value = count)

# Can now sum up the MFs by the new issue variable
# This code gets rid of NAs, groups by the new issue variable, and sums
df_moral[is.na(df_moral)] <- 0

# combine vice and virtue scores
df_moral <- df_moral %>%
  transmute(
    doc_id,
    care_n = care.vice + care.virtue,
    fairness_n = fairness.vice + fairness.virtue,
    loyalty_n = loyalty.vice + loyalty.virtue,
    authority_n = authority.vice + authority.virtue,
    sanctity_n = sanctity.vice + sanctity.virtue,
    moralwords_n = care_n + fairness_n + loyalty_n + authority_n + sanctity_n
  ) 

# Get the word counts (ntoken) for each document, after removing stopwords, etc
df_docvars <- tidy(my_dfm) %>% 
  rename(doc_id = "document") %>% 
  group_by(doc_id) %>% 
  summarize(n_tokens = sum(count)) %>% 
  right_join(df_import, by = "doc_id") %>% 
  select(-text)

# Join df_docvars and df_moral into the final dataset for analysis, and reorder
df <- df_docvars %>% 
  left_join(df_moral, by = "doc_id") %>% 
  select(doc_id, referendum, issue, side, n_tokens, moralwords_n, care_n:sanctity_n)

# create ratios
df <- df %>% 
  mutate(care_perc = (care_n / n_tokens) * 100, 
         fairness_perc = (fairness_n / n_tokens) * 100, 
         loyalty_perc = (loyalty_n / n_tokens) * 100, 
         authority_perc = (authority_n / n_tokens) * 100, 
         sanctity_perc = (sanctity_n / n_tokens) * 100,
         moralwords_perc = (moralwords_n / n_tokens) * 100
  )

# before writing to file, mutate the auto-generated doc_ids into something more useful
df <- df %>% 
  mutate(doc_id = paste(issue, referendum, side))


# remove unneeded dataframes
rm(df_import, df_docvars, df_moral, dict_mfd, my_corpus, my_dfm, my_dfm_mfd)

# write_csv(df, "tidy_df.csv")
saveRDS(df, "tidy_df.rds")



