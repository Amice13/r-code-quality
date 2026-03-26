library(tidyverse) # Piping, read_csv, dplyr
library(tidytext) # Basic preprocessing
library(quanteda) # quanteda is the preferred ingestion pipeline
library(seededlda) # Fit LDA -- it doesn't really matter which package we use

# Callum's channel sample -- need to manually add the four because they were
# missing ids in the CSV, but it's OK
channels_sample = read_csv("data/sample_channels_victimhood.csv") %>%
  pull(node) %>%
  c(1207633990, 1227163562, 1321650946, 1277769680)

# Read all aggregated messages and filter to the channel sample. Keep these
# as is, we're going to re-join later.
raw_messages = read_csv("data_chunks_out/messages_aggregated.csv") %>%
  filter(channel_id %in% channels_sample)

# Now, pre-process.
messages = raw_messages %>%
  select(channel_id, running_document, message) %>%
  mutate(aggregate_id = paste0(channel_id, "_", running_document)) %>%
  select(-channel_id, -running_document) %>%
  # Lowercase and remove URLs.
  mutate(message = str_replace_all(
    str_to_lower(message),
    "http\\S+\\s*", "")) %>%
  # Unnest tokens will remove punctuation
  unnest_tokens(
    output = "token",
    input = "message"
  ) %>%
  # Remove numbers and emails -- everything should be alphabetical
  filter(str_detect(token, "^[a-z]*$")) %>%
  # Default stopwords, we could probably do better
  anti_join(get_stopwords(), by=c("token" = "word")) %>%
  # Re-collapse to messages for import into quanteda, which doesn't want 
  # tokens as an input format
  group_by(aggregate_id) %>%
  summarize(message = paste0(token, collapse=" ")) %>%
  ungroup() %>%
  # Quanteda corpus
  corpus(docid_field = "aggregate_id", text_field = "message") %>%
  # Quanteda DFM,
  dfm()
# Notably not done in pre-processing: stemming/lemmatizing!


# Pre-processing documents: no rare terms, no empty documents
base_dtm = messages %>%
  # Trim rarely used words
  dfm_trim(min_termfreq = 100, min_docfreq = 50) %>%
  # Messages include at least 5 words
  dfm_subset(ntoken(.) > 5)

# Run model
out = textmodel_lda(base_dtm, k = 100, max_iter = 1500, verbose = TRUE)

saveRDS(out, "victimhood_out/fit_model.rds")

# Outputs: top words by topic, this is simple
terms(out, n = 50) %>% 
  as_tibble() %>%
  write_csv("victimhood_out/top_terms_by_topic_2.csv")

# Outputs: top documents by topic
out_docs = tibble(
  document = rownames(out$theta),
  out$theta %>% as_tibble()
) %>%
  # Get top documents by topic
  pivot_longer(cols=2:ncol(.), names_to = "topic", values_to = "prop") %>%
  mutate(topic = as.numeric(str_replace(topic, "topic", ""))) %>%
  group_by(topic) %>%
  arrange(-prop) %>%
  top_n(100) %>%
  # Clean to re-join with raw messages
  separate(col = "document", into=c("channel_id", "running_document"), sep="_") %>%
  mutate(channel_id = as.numeric(channel_id),
         running_document = as.numeric(running_document)) %>%
  arrange(topic, -prop) %>%
  # Re-join
  left_join(raw_messages)

# Output  
write_csv(out_docs, "victimhood_out/top_documents_by_topic_2.csv")



