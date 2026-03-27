
install.packages("quanteda")
install.packages("tidyverse")  # For data manipulation
install.packages("tidytext")   # For text processing
install.packages("tidylo")     # For calculating log odds



library(tidyverse)
library(tidytext)
library(tidylo)
library(quanteda)
library(SnowballC)  # Load SnowballC for stemming


britain <- read.csv("G:\\My Drive\\university\\phd\\women image\\britain.csv", stringsAsFactors = FALSE, encoding="UTF-8")
head(britain)

# Tokenize the speeches into individual words
tokenized_data <- britain %>%
  unnest_tokens(word, Text)
head(tokenized_data)
word_counts <- tokenized_data %>%
  count(Party, word, sort = TRUE)
head(word_counts)

data("stop_words")
cleaned_data_uk <- tokenized_data %>%
  anti_join(stop_words, by = "word")  # Remove common stopwords
head(cleaned_data_uk)

cleaned_data_uk <- cleaned_data_uk %>%
  mutate(Party = recode(Party,
                        "Labour" = "Labour",
                        "The Conservative Party" = "Conservatives",
                        "UKIP" = "UKIP/Brexit"))

cleaned_data_uk <- cleaned_data_uk %>%
  mutate(word = wordStem(word, language = "en"))  # Apply English-specific stemming

word_counts <- cleaned_data_uk %>%
  count(Party, word, sort = TRUE)


log_odds <- word_counts %>%
  bind_log_odds(set = Party, feature = word, n = n)
head(log_odds)


#top_words <- log_odds %>%

top_words <- log_odds %>%
  group_by(Party) %>%
  slice_max(log_odds_weighted, n = 15) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, log_odds_weighted, Party))


  ggplot(top_words, aes(word, log_odds_weighted, fill = Party)) +
    geom_col(show.legend = FALSE) +  # Show legend for parties
    facet_wrap(~ Party, scales = "free_y", nrow = 1) +  # Separate rows for each Party
    coord_flip() +  # Flip coordinates for better readability
    scale_x_reordered() +  # Reorder x-axis based on word frequency
    scale_fill_grey(start = 0.2, end = 0.8) +  # Convert colors to grayscale
    labs(x = "Word", y = NULL, title = NULL) +
    theme_minimal() + # Apply a minimal theme
    theme(axis.text.y = element_text(margin = margin(r = 15)),
          axis.title.y = element_text(margin = margin(r = 30)))
  
  