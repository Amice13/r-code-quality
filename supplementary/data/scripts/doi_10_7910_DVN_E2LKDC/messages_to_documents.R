library(tidyverse)
library(lubridate)

# Read messages
messages = read_csv("../raw_data/messages_nov2022.csv")
file_suffix = ymd(today())

# Initial approach -- identify just time deltas to get the distribution plot
tmp_deltadist = messages %>%
  select(channel_id, message, date) %>%
  group_by(channel_id) %>%
  arrange(date) %>%
  mutate(
    time_delta = date - lag(date, 1),
    time_delta = as.numeric(time_delta),
    time_delta = ifelse(time_delta > 60 * 10, 60 * 10, time_delta)
  ) %>% ungroup()

msg_chunks = tmp_deltadist %>%
  mutate(
    ends_with_ht = str_detect(message, "#[[:alnum:]]+$"),
    ends_with_atname = str_detect(message, "@[[:alnum:]]+$"),
    ends_with_turl = str_detect(message, "t.me/[0-9A-Za-z_]+$"),
    ends_with_url = str_detect(message, "https?://[0-9A-Za-z\\-_\\./]+$"),
    new_message = 
      time_delta >= 600 |
      ends_with_ht |
      ends_with_atname |
      ends_with_turl |
      ends_with_url
  )

plot_deltadist = tmp_deltadist %>%
  ggplot(aes(x = time_delta)) + 
  geom_histogram() + 
  theme_bw() +
  xlab("delta_t between messages") +
  ylab("Density") +
  theme(axis.text.y = element_blank())

ggsave(paste0("messages_to_documents/hist_documents_", file_suffix, ".pdf"))
# Sharp elbow dropoff, so we're setting the gap to 600

# Configuration: minimum gap between messages to be a new document
min_gap = 600

# Approach 1: Just rely on dplyr groupings and do it all at once
# Group by group, arrange by date
# output = messages %>%
#   group_by(channel_id) %>%
#   arrange(date) %>%
#   # First, setting up time delta / document breaks
#   mutate(
#     time_delta = date - lag(date, 1),
#     new_chunk = case_when(
#       # If the time delta is NA, then it's the first message of a group
#       is.na(time_delta) ~ 1,
#       # If the time delta is above the threshold, then it's a new document
#       time_delta > min_gap ~ 1,
#       # If not, it's the same document
#       TRUE ~ 0),
#     # The cumulative sum of the new document flags = the document number
#     # within the messages
#     running_document = cumsum(new_chunk)
#   ) %>%
#   # Remove empty messages
#   filter(!is.na(message)) %>%
#   # Now combine all the documents
#   group_by(channel_id, running_document) %>% 
#   summarize(
#     message = paste0(message, collapse=" "),
#     # Some general information about what we combined; note
#     # that any portion of the document that was an empty attachment
#     # will get lost here
#     min_id = min(id),
#     max_id = max(id),
#     min_date = min(date),
#     max_date = max(date),
#     total_attachments = sum(file_count)
#   ) %>%
#   ungroup()

# Approach 2: Chunk by group so we can see progress a little better
# library(furrr)
# library(progressr)
# plan(multiprocess, workers = 4)
library(progress)

# ~6GB RAM to create the split
split_messages = messages %>% group_by(channel_id) %>% group_split()
rm(messages)
results = list()

pbar = progress_bar$new(total = length(split_messages))
for(ind in 1:length(split_messages)) {
  pbar$tick()
  pbar$message(
    paste0("Working on ", unique(split_messages[[ind]]$channel_id), ". ", 
           ind, "/", length(split_messages)))

  results[[ind]] = split_messages[[ind]] %>%
    group_by(channel_id) %>%
    arrange(date) %>%
    mutate(
      time_delta = date - lag(date, 1),
      ends_with_ht = str_detect(message, "#[[:alnum:]]+$"),
      ends_with_atname = str_detect(message, "@[[:alnum:]]+$"),
      ends_with_turl = str_detect(message, "t.me/[0-9A-Za-z_]+$"),
      ends_with_url = str_detect(message, "https?://[0-9A-Za-z\\-_\\./]+$"),
      new_chunk = case_when(
        is.na(time_delta) ~ 1,
        time_delta >= min_gap ~ 1,
        ends_with_ht | ends_with_atname | ends_with_turl | ends_with_url ~ 1,
        TRUE ~ 0
      ),
      running_document = cumsum(new_chunk)) %>%
    filter(!is.na(message)) %>%
    group_by(channel_id, running_document) %>%
    summarize(
      message = paste0(message, collapse=" "),
      # Some general information about what we combined; note
      # that any portion of the document that was an empty attachment
      # will get lost here
      min_id = min(id),
      max_id = max(id),
      min_date = min(date),
      max_date = max(date),
      total_attachments = sum(file_count),
      .groups = "drop_last"
    ) %>%
    ungroup()
}

# Now, merge the results
merged_results = data.table::rbindlist(results)

write_csv(merged_results, paste0("data_chunks_out/messages_aggregated_", file_suffix, ".csv"))

# set.seed(19861108)
# 
# covid_channels = read_csv("data_chunks_out/covid_communities.csv")
# german_channels = read_csv("data_chunks_out/channels_german.csv")
# 
# set.seed(19861109)
# 
# non_covid_channels = merged_results %>% 
#   select(channel_id, running_document, message, min_date, max_date) %>%
#   anti_join(covid_channels, by = c("channel_id" = "node")) %>%
#   anti_join(german_channels, by = c("channel_id" = "id")) %>%
#   anti_join(existing_messages) %>%
#   slice_sample(n = 50)
#   
# covid_channels = merged_results %>% 
#     select(channel_id, running_document, message, min_date, max_date) %>%
#     filter(channel_id %in% covid_channels$node) %>%
#     anti_join(existing_messages) %>%
#     slice_sample(n = 200)
#     
# set.seed(19861109)
# non_covid_channels_CC = non_covid_channels %>%
#   slice_sample(n = 50)
# 
# covid_channels_CC = covid_channels %>% slice_sample(n = 200)
# 
# non_covid_channels_MS = non_covid_channels %>%
#   slice_sample(n = 50)
# 
# covid_channels_MS = covid_channels %>% slice_sample(n = 200)
# 
# bind_rows(non_covid_channels_CC, covid_channels_CC) %>%
#   sample() %>%
#   mutate(masks_CC = NA, vaccination_CC = NA, lockdown_CC = NA, cam_CC = NA,
#          test_CC = NA) %>%
#   write_csv("covid_out/covid_pilot_2_CC.csv")
# 
# bind_rows(non_covid_channels_MS, covid_channels_MS) %>%
#   sample() %>%
#   mutate(masks_MS = NA, vaccination_MS = NA, lockdown_MS = NA, cam_MS = NA,
#          test_MS = NA) %>%
#   write_csv("covid_out/covid_pilot_2_MS.csv")
# 
# 
# pilot_covid_coding = bind_rows(non_covid_channels, covid_channels) %>%
#   sample()
# 
# pilot_covid_coding %>% select(channel_id, running_document, message, min_date, max_date) %>% mutate(
#   masks_AR = NA,
#   vaccination_AR = NA,
#   lockdown_AR = NA,
#   cam_AR = NA
# ) %>%
#   write_csv("covid_out/covid_pilot_AR.csv")
# 
