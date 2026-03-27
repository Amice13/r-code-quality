library(tidyverse)
library(tidycomm)

# Build combined pilot
callum_pilot = read_csv("victimhood_out/pilot_2/pilot_main_2_CC.csv") %>%
  select(channel_id, running_document, message, victimhood) %>%
  rename(victim_cc = victimhood)

constantine_pilot = read_csv("victimhood_out/pilot_2/pilot_main_2_CB.csv") %>%
  select(channel_id, running_document, victim_cb)

combined = callum_pilot %>%
  left_join(constantine_pilot) %>%
  replace_na(list(victim_cc = 0, victim_cb = 0)) %>%
  filter(channel_id != 1402715991)

# Write disagreement
combined %>% 
  filter(victim_cc != victim_cb) %>%
  write_csv("victimhood_out/pilot_2/disagreements.csv")

# Get ICR metrics
combined %>%
  select(-message) %>%
  pivot_longer(
    cols = 3:4,
    names_to = "coder_id",
    values_to = "coding"
  ) %>%
  mutate(combined_id = paste0(channel_id, "_", running_document)) %>%
  test_icr(
    combined_id,
    coder_id,
    coding,
    cohens_kappa = TRUE
  )

