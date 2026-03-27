install.packages("quanteda")
install.packages("tidyverse")  # For data manipulation
install.packages("tidytext")   # For text processing
install.packages("tidylo")     # For calculating log odds
install.packages("tm") 
install.packages("SnowballC")

library(tidyverse)
library(tidytext)
library(tidylo)
library(quanteda)
library(SnowballC)


netherlands <- read.csv("G:\\My Drive\\university\\phd\\women image\\netherlands.csv", stringsAsFactors = FALSE, encoding="UTF-8")
head(netherlands)

netherlands <- netherlands %>%
  mutate(Text = as.character(Text))

# Tokenize the speeches into individual words
tokenized_netherlands <- netherlands %>%
  mutate(word = strsplit(Text, "\\s+")) %>%
  unnest(word)

stopwords_dutch <- stopwords("nl")
stopwords_dutch <- tibble(word = stopwords_dutch)

cleaned_netherlands <- tokenized_netherlands %>%
  anti_join(stopwords_dutch, by = "word")  # Remove Dutch stopwords

cleaned_netherlands <- cleaned_netherlands %>%
  mutate(word = wordStem(word, language = "nl"))  # Apply Dutch-specific stemming

word_counts_netherlands <- cleaned_netherlands %>%
  count(Party, word, sort = TRUE)
head(word_counts_netherlands)


log_odds_netherlands <- word_counts_netherlands %>%
  bind_log_odds(set = Party, feature = word, n = n)

top_words_netherlands <- log_odds_netherlands %>%
  group_by(Party) %>%
  top_n(15, log_odds_weighted) %>%
  ungroup()


ggplot(top_words_netherlands, aes(reorder_within(word, log_odds_weighted, Party), 
                             log_odds_weighted, fill = Party)) +
  geom_col(show.legend = FALSE) +  # Show legend for parties
  facet_wrap(~ Party, scales = "free_y", nrow = 1) +  # Separate rows for each Party
  coord_flip() +  # Flip coordinates for better readability
  scale_x_reordered() +  # Reorder x-axis based on word frequency
  scale_fill_grey(start = 0.2, end = 0.8) +  # Convert colors to grayscale
  labs(x = "Word", y = NULL, title = NULL) +
  theme_minimal() +  # Apply a minimal theme
  theme(axis.text.y = element_text(margin = margin(r = 15)),
        axis.title.y = element_text(margin = margin(r = 30)))# Add space between y-axis text and plot

