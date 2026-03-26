library(tidyverse)
library(tidytext)
library(tidylo)
library(stopwords)
library(SnowballC)  # Load SnowballC for stemming

france <- read.csv("G:\\My Drive\\university\\phd\\women image\\france.csv", stringsAsFactors = FALSE, encoding="UTF-8")
head(france)

france <- france %>%
  mutate(Text = as.character(Text))

tokenized_france <- france %>%
  mutate(word = strsplit(Text, "\\s+")) %>%
  unnest(word)


stopwords_french <- stopwords("fr")
stopwords_french <- tibble(word = stopwords_french)

cleaned_france <- tokenized_france %>%
  anti_join(stopwords_french, by = "word")  # Remove French stopwords

cleaned_france <- cleaned_france %>%
  mutate(Party = recode(Party,
                        "National Front" = "NF",
                        "The Socialists" = "Socialists",
                        "UMP" = "UMP/LR"))
cleaned_france <- cleaned_france %>%
  mutate(word = wordStem(word, language = "fr"))  # Apply French-specific stemming

word_counts_france <- cleaned_france %>%
  count(Party, word, sort = TRUE)

log_odds_france <- word_counts_france %>%
  bind_log_odds(set = Party, feature = word, n = n)

top_words_france <- log_odds_france %>%
  group_by(Party) %>%
  top_n(15, log_odds_weighted) %>%
  ungroup()

ggplot(top_words_france, aes(reorder_within(word, log_odds_weighted, Party), 
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



