library(tidyverse)
set.seed(19861108)

# Get the topic IDs that can be used to sample from.
#sample_docs = 
#  read_csv("victimhood_out/topic_labels_comments_CB.csv") %>%
#  filter(include == 1, !is.na(include)) %>%
#  mutate(topic_n = as.numeric(str_replace(topic_n, "topic", ""))) %>%
#  pull(topic_n)

# Topics selected by at least two of the three of us.
sample_docs = tibble(
  topic_n = c(
    5, 8, 9, 11, 13, 14, 18, 20, 22, 23, 27, 28, 29, 32, 33, 34, 
    35, 36, 39, 40, 47, 48, 49, 51, 53, 54, 56, 57, 58, 60, 61, 62, 
    64, 65, 66, 77, 88, 89, 95)
) %>%
  pull(topic_n)

out_model = readRDS("victimhood_out/initial_topic_model/fit_model.rds")

out_docs = tibble(
  document = rownames(out_model$theta),
  out_model$theta %>% as_tibble()
) %>%
  # Selecting the columns -- column 2 = topic 1 etc etc.
  select(document, sample_docs + 1) %>%
  # Add up the probability of being in a victimhood topic
  rowwise() %>%
  mutate(sum_prob = sum(c_across(topic5:topic95), na.rm = TRUE)) %>% 
  # Only take documents whose sum of victimhood is at least 50%
  filter(sum_prob > 0.50)

# First, here are the 200 messages we're pulling from the identified high-
# victimhood channels + high-victimhood topics 
set.seed(19861108)
pilot_sample = out_docs %>%
  ungroup() %>%
  slice_sample(n = 200, weight_by = out_docs$sum_prob, replace = FALSE) 

# Now, here are all of our messages (5.973 million)
raw_messages = read_csv("data_chunks_out/messages_aggregated.csv")

# Now, pre-process.
messages = raw_messages %>%
  select(channel_id, running_document, min_date, message) %>%
  mutate(aggregate_id = paste0(channel_id, "_", running_document))

selected_subset = messages %>% 
  inner_join(
    pilot_sample %>% select(document, sum_prob),
    by = c("aggregate_id" = "document")
  ) %>%
  select(-aggregate_id)

# Unselected subset
channels_sample = read_csv("data/sample_channels_victimhood.csv") %>%
  pull(node) %>%
  c(1207633990, 1227163562, 1321650946, 1277769680)

german_channels = read_csv("data_chunks_out/channels_german.csv") %>%
  pull(id)

unselected_subset = messages %>%
  # Make sure the remaining messages are not from the 28 sampled channels
  filter(!channel_id %in% channels_sample) %>%
  # Make sure the remaining messages are English
  filter(!channel_id %in% german_channels, channel_id != 1003201003) %>%
  # Make sure messages are at least 10 words
  mutate(num_words = str_count(message, "\\w+")) %>%
  filter(num_words >= 10) %>%
  select(-num_words) 

unselected_subset2 = unselected_subset %>%
  slice_sample(n = 50, replace = FALSE)

bind_rows(
  selected_subset,
  unselected_subset2
) %>% select(-aggregate_id) %>%
  write_csv("victimhood_out/pilot_main.csv")


# Second pilot batch
included_first_pilot = read_csv("victimhood_out/pilot_1/pilot_main.csv") %>%
  select(channel_id, running_document) %>%
  mutate(combined = paste0(channel_id, "_", running_document))

set.seed(247365)
pilot_sample_2 = out_docs %>%
  ungroup() %>%
  anti_join(included_first_pilot, by=c("document" = "combined")) %>%
  slice_sample(n = 75, weight_by = sum_prob, replace = FALSE) 

selected_subset_p2 = messages %>% 
  inner_join(
    pilot_sample_2 %>% select(document, sum_prob),
    by = c("aggregate_id" = "document")
  ) %>%
  select(-aggregate_id)

unselected_subset3 = unselected_subset %>%
  anti_join(included_first_pilot) %>%
  slice_sample(n = 25, replace=FALSE)

bind_rows(
  selected_subset_p2,
  unselected_subset3
) %>% select(-aggregate_id) %>%
  write_csv("victimhood_out/pilot_main_2.csv")

# Third pilot batch
included_second_pilot = read_csv("victimhood_out/pilot_2/pilot_main_2.csv") %>%
  select(channel_id, running_document) %>%
  mutate(combined = paste0(channel_id, "_", running_document))

set.seed(11081986)
pilot_sample_3 = out_docs %>%
  ungroup() %>%
  anti_join(included_first_pilot, by=c("document" = "combined")) %>%
  anti_join(included_second_pilot, by=c("document" = "combined")) %>%
  separate(document, into=c("channel_id", "running_document"), sep="_") %>%
  filter(channel_id != 1402715991) %>%
  mutate(document = paste0(channel_id, "_", running_document)) %>%
  select(document, everything()) %>%
  slice_sample(n = 75, weight_by = sum_prob, replace = FALSE) 

selected_subset_p3 = messages %>% 
  inner_join(
    pilot_sample_3 %>% select(document, sum_prob),
    by = c("aggregate_id" = "document")
  ) %>%
  select(-aggregate_id)

unselected_subset_pilot3 = unselected_subset %>%
  anti_join(included_first_pilot) %>%
  anti_join(included_second_pilot) %>%
  slice_sample(n = 25, replace=FALSE)

bind_rows(
  selected_subset_p3,
  unselected_subset_pilot3
) %>% select(-aggregate_id) %>%
  write_csv("victimhood_out/pilot_3/pilot_main_3.csv")


# Final training sample
included_third_pilot = read_csv("victimhood_out/pilot_3/pilot_main_3.csv") %>%
  select(channel_id, running_document) %>%
  mutate(combined = paste0(channel_id, "_", running_document))

# Final training sample
set.seed(11081986)
training_sample = out_docs %>%
  ungroup() %>%
  anti_join(included_first_pilot, by=c("document" = "combined")) %>%
  anti_join(included_second_pilot, by=c("document" = "combined")) %>%
  anti_join(included_third_pilot, by=c("document" = "combined")) %>%
  separate(document, into=c("channel_id", "running_document"), sep="_") %>%
  filter(channel_id != 1402715991) %>%
  mutate(document = paste0(channel_id, "_", running_document)) %>%
  select(document, everything()) %>%
  slice_sample(n = 1500, weight_by = sum_prob, replace = FALSE)

selected_subset_training = messages %>% 
  inner_join(
    training_sample %>% select(document, sum_prob),
    by = c("aggregate_id" = "document")
  ) %>%
  select(-aggregate_id)

unselected_subset_training = unselected_subset %>%
  anti_join(included_first_pilot) %>%
  anti_join(included_second_pilot) %>%
  anti_join(included_third_pilot) %>%
  slice_sample(n = 1050, replace=FALSE)

full_training_sample = bind_rows(
  selected_subset_training,
  unselected_subset_training
) %>% select(-aggregate_id) %>%
  select(channel_id, running_document, min_date, sum_prob, message) %>%
  slice_sample(prop = 1)

training_1 = full_training_sample %>% slice(1:1275)
training_2 = full_training_sample %>% slice(1276:2550)

write_csv(training_1, "victimhood_out/training/train_sample_CB.csv")
write_csv(training_2, "victimhood_out/training/train_sample_CC.csv")


  