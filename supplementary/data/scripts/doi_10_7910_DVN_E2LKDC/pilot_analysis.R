library(tidyverse)
library(tidycomm)

# Build combined pilot

aaron_pilot = read_csv("victimhood_out/pilot_main_AR.csv") %>%
  select(channel_id, running_document, message, is_victimization) %>%
  rename(victim_ar = is_victimization)

callum_pilot = read_csv("victimhood_out/pilot_main_CC.csv") %>%
  select(channel_id, running_document, victimhood) %>%
  rename(victim_cc = victimhood)

constantine_pilot = read_csv("victimhood_out/pilot_main_CB.csv") %>%
  select(channel_id, running_document, victimhood) %>%
  rename(victim_cb = victimhood)

combined = aaron_pilot %>%
  left_join(callum_pilot) %>%
  left_join(constantine_pilot) %>%
  replace_na(list(victim_ar = 0, victim_cc = 0, victim_cb = 0))

# combined %>% 
#  filter(victim_cc != victim_cb) %>%
#  write_csv("victimhood_out/pilot_combined.csv")

reconcile = read_csv("victimhood_out/pilot_combined.csv") %>%
  select(channel_id, running_document, agreed)

combined = combined %>%
  left_join(reconcile) %>%
  mutate(
    victim_cc = coalesce(agreed, victim_cc),
    victim_cb = coalesce(agreed, victim_cb)
  ) %>%
  select(-agreed)

# Get ICR metrics

combined %>%
  select(-message) %>%
  pivot_longer(
    cols = 3:5,
    names_to = "coder_id",
    values_to = "coding"
  ) %>%
  filter(coder_id != "victim_ar") %>%
  mutate(combined_id = paste0(channel_id, "_", running_document)) %>%
  test_icr(
    combined_id,
    coder_id,
    coding,
    cohens_kappa = TRUE
  )

