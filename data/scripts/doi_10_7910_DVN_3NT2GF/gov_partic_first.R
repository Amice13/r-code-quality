####Government Particpation First Year####

#Import the parlgov dataset#

cmp_id <- import("Data/ParlGov/parlgov.xlsx", which = "party") %>%
  select(cmp, party_id)

parl <- import("Data/ParlGov/parlgov.xlsx", which = "cabinet") %>%
  arrange(country_name, party_id, start_date) %>%
  left_join(cmp_id, by = "party_id") %>%
  filter(!is.na(cmp)) %>%
  group_by(cmp) %>%
  mutate(in_gov = ifelse(cabinet_party == 1 | prime_minister == 1, 1, 0),
         gov_lag = dplyr::lag(in_gov),
         enter = ifelse(in_gov == 1 & gov_lag == 0, 1, 
                        ifelse(in_gov == 1 & gov_lag == 1, 1, 0))) %>%
  filter(enter == 1) %>%
  dplyr::summarize(first_gov = min(start_date)) %>%
  ungroup() %>%
  separate(first_gov, into = c('year', 'month', 'day')) %>%
  select(cmp, year) %>%
  rename(first_gov = year,
         party = cmp)
