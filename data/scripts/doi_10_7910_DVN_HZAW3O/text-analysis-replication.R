# title:    Replication text analysis Policy Change Acceptance paper
# authors:  Maurits Meijers & Ruth Dassonneville
# date:     September 2024


# Set working directory
setwd('~/Dropbox/PolicyChange_Acceptance/replication/analysis/text analysis-open ended')

# Load packages
library(haven)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(deeplr)
library(httr)
library(jsonlite)
library(foreign)
library(tidyverse)
library(readr)
library(tidytext)
library(textdata)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

# Read the data (Stata format)
data <- read_dta('~/Dropbox/PolicyChange_Acceptance/replication/data/data_yougov.dta')
data_uk <- subset(data, country == 2)
data_de <- read_csv('~/Dropbox/PolicyChange_Acceptance/replication/analysis/text analysis-open ended/data_de_translations.csv') 
data_es <- read_csv('~/Dropbox/PolicyChange_Acceptance/replication/analysis/text analysis-open ended/data_es_translations.csv') 
data_nl <- read_csv('~/Dropbox/PolicyChange_Acceptance/replication/analysis/text analysis-open ended/data_nl_translations.csv') 
data_pl <- read_csv('~/Dropbox/PolicyChange_Acceptance/replication/analysis/text analysis-open ended/data_pl_translations.csv') 

# Rename ru8 variable in uk dataframe so all data for text analysis have the same name 
data_uk <- data_uk %>%
  rename(ru8_new = ru8)

# Add sample variable to all country datasets
data_uk <- data_uk %>%
  mutate(countrysample = 'UK')
data_de <- data_de %>%
  mutate(countrysample = 'DE')
data_es <- data_es %>%
  mutate(countrysample = 'ES')
data_nl <- data_nl %>%
  mutate(countrysample = 'NL')
data_pl <- data_pl %>%
  mutate(countrysample = 'PL')

# standardize political interest variable

data_uk <- data_uk  %>%
  mutate(
    pol_interest = case_when(
      var10 == 1 ~ "Very interested",
      var10 == 2 ~ "Somewhat interested",
      var10 == 3 ~ "In between",
      var10 == 4 ~ "Not very interested",
      var10 == 5 ~ "Not at all interested",
      TRUE ~ NA_character_  # handle any unexpected values
    )
  )

data_de <- data_de  %>%
  mutate(
    pol_interest = case_when(
      var10 == 1 ~ "Very interested",
      var10 == 2 ~ "Somewhat interested",
      var10 == 3 ~ "In between",
      var10 == 4 ~ "Not very interested",
      var10 == 5 ~ "Not at all interested",
      TRUE ~ NA_character_  # handle any unexpected values
    )
  )

data_es <- data_es  %>%
  mutate(
    pol_interest = case_when(
      var10 == 1 ~ "Very interested",
      var10 == 2 ~ "Somewhat interested",
      var10 == 3 ~ "In between",
      var10 == 4 ~ "Not very interested",
      var10 == 5 ~ "Not at all interested",
      TRUE ~ NA_character_  # handle any unexpected values
    )
  )

data_pl <- data_pl  %>%
  mutate(
    pol_interest = case_when(
      var10 == 1 ~ "Very interested",
      var10 == 2 ~ "Somewhat interested",
      var10 == 3 ~ "In between",
      var10 == 4 ~ "Not very interested",
      var10 == 5 ~ "Not at all interested",
      TRUE ~ NA_character_  # handle any unexpected values
    )
  )

data_nl <- data_nl  %>%
  mutate(
    pol_interest = case_when(
      pol_int == 5 ~ "Very interested",
      pol_int == 4 ~ "Somewhat interested",
      pol_int == 3 ~ "In between",
      pol_int == 2 ~ "Not very interested",
      pol_int == 1 ~ "Not at all interested",
      TRUE ~ NA_character_  # handle any unexpected values
    ),
  )

# Define columns of interest
columns_of_interest <- c("ru8_new", "pol_interest", "countrysample", "ru9")


# Filter and combine data frames
combined_df <- bind_rows(
  data_uk %>% select(all_of(columns_of_interest)),
  data_de %>% select(all_of(columns_of_interest)),
  data_es %>% select(all_of(columns_of_interest)),
  data_nl %>% select(all_of(columns_of_interest)),
  data_pl %>% select(all_of(columns_of_interest))
)

# Define text corpus 
corp_id <- corpus(combined_df,text_field = "ru8_new")
dmat_id <- corp_id 

# Descriptive statistics, average number of words per answer (before pre-processing), referred to in footnote

## Full corpus
myCorpus <- corpus(combined_df$ru8_new) %>%
  tokens(remove_punct = TRUE) 
myTokens <- tokens(myCorpus)
word_counts <- sapply(myTokens, function(x) sum(!is.na(x)))
average_word_count <- mean(word_counts)
cat("Average Word Count:", round(average_word_count, 2), "words\n")

missing_responses <- sum(is.na(combined_df$ru8_new))
print(paste("Number of respondents who did not answer:", missing_responses))

## UK
myCorpus <- corpus(data_uk$ru8_new) %>%
  tokens(remove_punct = TRUE) 
myTokens <- tokens(myCorpus)
word_counts <- sapply(myTokens, function(x) sum(!is.na(x)))
average_word_count <- mean(word_counts)
cat("Average Word Count:", round(average_word_count, 2), "words\n")

## DE
myCorpus <- corpus(data_de$ru8_new) %>%
  tokens(remove_punct = TRUE) 
myTokens <- tokens(myCorpus)
word_counts <- sapply(myTokens, function(x) sum(!is.na(x)))
average_word_count <- mean(word_counts)
cat("Average Word Count:", round(average_word_count, 2), "words\n")

## ES
myCorpus <- corpus(data_es$ru8_new) %>%
  tokens(remove_punct = TRUE) 
myTokens <- tokens(myCorpus)
word_counts <- sapply(myTokens, function(x) sum(!is.na(x)))
average_word_count <- mean(word_counts)
cat("Average Word Count:", round(average_word_count, 2), "words\n")

## PL
myCorpus <- corpus(data_pl$ru8_new) %>%
  tokens(remove_punct = TRUE) 
myTokens <- tokens(myCorpus)
word_counts <- sapply(myTokens, function(x) sum(!is.na(x)))
average_word_count <- mean(word_counts)
cat("Average Word Count:", round(average_word_count, 2), "words\n")

## NL
myCorpus <- corpus(data_nl$ru8_new) %>%
  tokens(remove_punct = TRUE) 
myTokens <- tokens(myCorpus)
word_counts <- sapply(myTokens, function(x) sum(!is.na(x)))
average_word_count <- mean(word_counts)
cat("Average Word Count:", round(average_word_count, 2), "words\n")


# Pre-processing of text for frequency and keyness analysis
tokens_data <- combined_df %>%
  corpus(text_field = "ru8_new") %>%
  tokens(remove_punct = TRUE) %>%
  tokens_remove(pattern = stopwords(language = "en")) %>%
  tokens_tolower()

dfm_data <- tokens_data %>%
  dfm() %>%
  dfm_trim(min_termfreq = 3)

tstat_col <- textstat_collocations(tokens_data, min_count = 10, tolower = FALSE)
head(tstat_col, 30)

toks_comp <- tokens_compound(tokens_data, pattern = tstat_col[tstat_col$z > 3,], 
                             case_insensitive = FALSE)

## Define vector of words to drop (includes words without substantive meaning and words that appear in the question)
word_to_erase <- paste0(c("think", "parties", "party", "sometimes", "change", "changing", "policy", "positions", "policies",
                          "believe"), collapse = "|") 

## Define vector of stop words
stop_words <- c(
  "the", "a", "an", "and", "or", "it", "its",
  "is", "was", "will", "be", "have", "been", "this", "that", 
  "of", "to", "for", "in", "on", "by", "as", "at", "with", "don't", "dont", "don,t", "idk", "etc", "don",
  "none", "get", "e.g.", "it's", "know", "i.e.", "na", "dk", "also", "can", "may", "due",
  "want", "order", "make", "not sure", "xxx", "e.g"
)
stop_words_regex <- str_c(stop_words, collapse = "|")
stop_words_regex <- str_c("\\b(", stop_words_regex, ")\\b") # This makes sure we don't delete text in the middle of words

combined_df <- combined_df |> 
  mutate(
    ### Cleaning of survey questions ###
    ru9_nochange = ifelse(ru9 >= 4, 1, 0),
    ru9_change   = ifelse(ru9 < 4, 1, 0),
    ru9_category = ifelse(ru9_change == 1, "Pro change", "Contra change"),
    ru9_category_numeric = ifelse(ru9_category == "No Change", 1, 2),
    ### Handling text data ###
    ru8_new = tolower(ru8_new), # put all words in lower cap
    ru8_new = str_remove_all(ru8_new, {{word_to_erase}}),
    ru8_new = str_replace_all(ru8_new, stop_words_regex, " " ),  # Remove the custom list of stop words
    ) #|> 

combined_df$ru8_new <- as.character(combined_df$ru8_new)
AllWords <- combined_df %>%
  corpus(text_field = "ru8_new") %>%
  tokens(remove_punct = TRUE) %>%
  tokens_remove(pattern = stopwords(language = "en")) %>%
  tokens_wordstem() %>%
  tokens_tolower() %>%
  tokens_compound(pattern = tstat_col[tstat_col$z > 3,], 
                  case_insensitive = FALSE) %>%
  tokens_select(min_nchar = 3) %>%  # Use tokens_select to filter out words of 2 characters or less
  dfm() %>%
  dfm_trim(min_termfreq = 3)


# Subset processed data for country-specific analyses
uk_df <- combined_df %>%
  filter(countrysample == "UK")
de_df <- combined_df %>%
  filter(countrysample == "DE")
es_df <- combined_df %>%
  filter(countrysample == "ES")
nl_df <- combined_df %>%
  filter(countrysample == "NL")
pl_df <- combined_df %>%
  filter(countrysample == "PL")

UKWords <- uk_df %>%
  corpus(text_field = "ru8_new") %>%
  tokens(remove_punct = TRUE) %>%
  tokens_remove(pattern = stopwords(language = "en")) %>%
  tokens_wordstem() %>%
  tokens_tolower() %>%
  tokens_compound(pattern = tstat_col[tstat_col$z > 3,], 
                  case_insensitive = FALSE) %>%
  tokens_select(min_nchar = 3) %>%  # Use tokens_select to filter out words of 2 characters or less
  dfm() %>%
  dfm_trim(min_termfreq = 3)

DEWords <- de_df %>%
  corpus(text_field = "ru8_new") %>%
  tokens(remove_punct = TRUE) %>%
  tokens_remove(pattern = stopwords(language = "en")) %>%
  tokens_wordstem() %>%
  tokens_tolower() %>%
  tokens_compound(pattern = tstat_col[tstat_col$z > 3,], 
                  case_insensitive = FALSE) %>%
  tokens_select(min_nchar = 3) %>%  # Use tokens_select to filter out words of 2 characters or less
  dfm() %>%
  dfm_trim(min_termfreq = 3)

ESWords <- es_df %>%
  corpus(text_field = "ru8_new") %>%
  tokens(remove_punct = TRUE) %>%
  tokens_remove(pattern = stopwords(language = "en")) %>%
  tokens_wordstem() %>%
  tokens_tolower() %>%
  tokens_compound(pattern = tstat_col[tstat_col$z > 3,], 
                  case_insensitive = FALSE) %>%
  tokens_select(min_nchar = 3) %>%  # Use tokens_select to filter out words of 2 characters or less
  dfm() %>%
  dfm_trim(min_termfreq = 3)

NLWords <- nl_df %>%
  corpus(text_field = "ru8_new") %>%
  tokens(remove_punct = TRUE) %>%
  tokens_remove(pattern = stopwords(language = "en")) %>%
  tokens_wordstem() %>%
  tokens_tolower() %>%
  tokens_compound(pattern = tstat_col[tstat_col$z > 3,], 
                  case_insensitive = FALSE) %>%
  tokens_select(min_nchar = 3) %>%  # Use tokens_select to filter out words of 2 characters or less
  dfm() %>%
  dfm_trim(min_termfreq = 3)

PLWords <- pl_df %>%
  corpus(text_field = "ru8_new") %>%
  tokens(remove_punct = TRUE) %>%
  tokens_remove(pattern = stopwords(language = "en")) %>%
  tokens_wordstem() %>%
  tokens_tolower() %>%
  tokens_compound(pattern = tstat_col[tstat_col$z > 3,], 
                  case_insensitive = FALSE) %>%
  tokens_select(min_nchar = 3) %>%  # Use tokens_select to filter out words of 2 characters or less
  dfm() %>%
  dfm_trim(min_termfreq = 3)



# Frequency bar graph  (plot 20 most frequent words) -- Figure 5 in paper

## UK
tstat1 <- textstat_frequency(UKWords)
tstat1

library("ggplot2")
ggplot(tstat1[1:20, ], aes(x = reorder(feature, frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "navyblue") +
  coord_flip() +
  theme_bw() +
  labs(x = NULL, y = "Frequency") +
  theme(axis.text.y = element_text(size = 12),axis.text.x = element_text(size = 12),axis.title.x = element_text(size = 12))
ggsave("frequency-uk.pdf",  width=5, height=6)

## DE
tstat1 <- textstat_frequency(DEWords)
tstat1

library("ggplot2")
ggplot(tstat1[1:20, ], aes(x = reorder(feature, frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "navyblue") +
  coord_flip() +
  theme_bw() +
  labs(x = NULL, y = "Frequency") +
  theme(axis.text.y = element_text(size = 12),axis.text.x = element_text(size = 12),axis.title.x = element_text(size = 12))
ggsave("frequency-de.pdf",  width=5, height=6)

## ES
tstat1 <- textstat_frequency(ESWords)
tstat1

library("ggplot2")
ggplot(tstat1[1:20, ], aes(x = reorder(feature, frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "navyblue") +
  coord_flip() +
  theme_bw() +
  labs(x = NULL, y = "Frequency") +
  theme(axis.text.y = element_text(size = 12),axis.text.x = element_text(size = 12),axis.title.x = element_text(size = 12))
ggsave("frequency-es.pdf",  width=5, height=6)

## NL
tstat1 <- textstat_frequency(NLWords)
tstat1

library("ggplot2")
ggplot(tstat1[1:20, ], aes(x = reorder(feature, frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "navyblue") +
  coord_flip() +
  theme_bw() +
  labs(x = NULL, y = "Frequency") +
  theme(axis.text.y = element_text(size = 12),axis.text.x = element_text(size = 12),axis.title.x = element_text(size = 12))
ggsave("frequency-nl.pdf",  width=5, height=6)

## PL
tstat1 <- textstat_frequency(PLWords)
tstat1

library("ggplot2")
ggplot(tstat1[1:20, ], aes(x = reorder(feature, frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "navyblue") +
  coord_flip() +
  theme_bw() +
  labs(x = NULL, y = "Frequency") +
  theme(axis.text.y = element_text(size = 12),axis.text.x = element_text(size = 12),axis.title.x = element_text(size = 12))
ggsave("frequency-pl.pdf",  width=5, height=6)



# Keyness analysis distinguishing word use of pro/contra change groups

## Group the dfm based on the ru9_category variable
AllWords2 <- dfm_group(AllWords, groups = docvars(AllWords, "ru9_category"))
UKWords2 <- dfm_group(UKWords, groups = docvars(UKWords, "ru9_category"))
DEWords2 <- dfm_group(DEWords, groups = docvars(DEWords, "ru9_category"))
ESWords2 <- dfm_group(ESWords, groups = docvars(ESWords, "ru9_category"))
NLWords2 <- dfm_group(NLWords, groups = docvars(NLWords, "ru9_category"))
PLWords2 <- dfm_group(PLWords, groups = docvars(PLWords, "ru9_category"))

## Keyness by group, all countries - Figure 6 in paper
tstat2 <- textstat_keyness(AllWords2, target="Pro change")
pdf(file = "keyness-all-groups.pdf", width=14, height=8)
textplot_keyness(tstat2,
                 color = c("navyblue", "lightblue"),
                 labelcolor = "black",
                 labelsize = 3.5)
dev.off()

## Keyness UK - Included in Appendix K
tstat2 <- textstat_keyness(UKWords2, target="Pro change")
pdf(file = "keyness-uk-groups.pdf", width=14, height=8)
textplot_keyness(tstat2,
                 color = c("navyblue", "lightblue"),
                 labelcolor = "black",
                 labelsize = 3.5)
dev.off()

## Keyness DE - Included in Appendix K
tstat2 <- textstat_keyness(DEWords2, target="Pro change")
pdf(file = "keyness-de-groups.pdf", width=14, height=8)
textplot_keyness(tstat2,
                 color = c("navyblue", "lightblue"),
                 labelcolor = "black",
                 labelsize = 3.5)
dev.off()

## Keyness ES - Included in Appendix K
tstat2 <- textstat_keyness(ESWords2, target="Pro change")
pdf(file = "keyness-es-groups.pdf", width=14, height=8)
textplot_keyness(tstat2,
                 color = c("navyblue", "lightblue"),
                 labelcolor = "black",
                 labelsize = 3.5)
dev.off()

## Keyness PL - Included in Appendix K
tstat2 <- textstat_keyness(PLWords2, target="Pro change")
pdf(file = "keyness-pl-groups.pdf", width=14, height=8)
textplot_keyness(tstat2,
                 color = c("navyblue", "lightblue"),
                 labelcolor = "black",
                 labelsize = 3.5)
dev.off()

## Keyness NL - Included in Appendix K
tstat2 <- textstat_keyness(NLWords2, target="Pro change")
pdf(file = "keyness-nl-groups.pdf", width=14, height=8)
textplot_keyness(tstat2,
                 color = c("navyblue", "lightblue"),
                 labelcolor = "black",
                 labelsize = 3.5)
dev.off()


# Keyness analyses distinguishing high/low interest groups - reported in Appendix G

## Recode the 'pol_interest' variable
docvars_df <- docvars(AllWords)

docvars_df <- docvars_df %>%
  mutate(pol_interest_group = case_when(
    pol_interest %in% c("Somewhat interested", "Very interested") ~ "High Interest",
    pol_interest %in% c("In between", "Not very interested", "Not at all interested") ~ "Low Interest",
    TRUE ~ NA_character_  # Handle any other cases or missing values
  ))

docvars(AllWords) <- docvars_df
AllWords3 <- dfm_group(AllWords, groups = docvars(AllWords, "pol_interest_group"))

## Perform keyness analysis
tstat3 <- textstat_keyness(AllWords3, target = "High Interest")

## Plot keyness analysis results
pdf(file = "keyness-politics-interest-all.pdf", width = 14, height = 8)
textplot_keyness(tstat3,
                 color = c("darkgreen", "lightgreen"),
                 labelcolor = "black",
                 labelsize = 3.5)
dev.off()