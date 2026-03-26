# We now create the party level dataset.

party_level <- expert_dataset %>%
  group_by(country, country_short, party_short, party_name_english, party_name_original,
           poppa_id, partyfacts_id) %>%
  summarise(n_experts = n(),
            across(manichean:compromise, ~ mean(.x, na.rm = TRUE))) %>%
  ungroup() %>% 
  mutate(populism =  rowMeans(select(., "manichean", "indivisible", "generalwill", "peoplecentrism", "antielitism"))) %>% 
  relocate(populism, .before = manichean) %>% 
  dplyr::left_join(., expert_dataset %>% 
                     select(country_short, poppa_id, contains("n_experts")) %>% 
                     pivot_longer(cols = manichean_n_experts:compromise_n_experts, values_to = "n_experts", names_to = "question") %>% 
                     distinct() %>% 
                     group_by(country_short, poppa_id) %>% 
                     summarise(mean_n_experts = mean(n_experts, na.rm=T)) %>% 
                     ungroup(),
                   by = c("country_short", "poppa_id")) %>% 
  relocate(mean_n_experts, .after = n_experts) %>% 
  select(country, country_short, party_short, party_name_english, party_name_original, poppa_id, everything()) %>% 
  mutate(country = forcats::fct_collapse(country, Belgium = c("Flanders", "Wallonie"))) %>%
  mutate(country = forcats::fct_relevel(country, sort),
         wave = "Wave 2 - 2023")

saveRDS(object = party_level, file = "final_data/poppa2.RDS")

rm(list = setdiff(ls(), c("run_start", "run_start_total")))

