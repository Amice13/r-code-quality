

candidate_df <-
  estimation_data %>%
  filter(A == "Wave 1") %>%
  select(V_3, F2, V_39_D, V_44, E, V_8_B, V_9, V_13_A:V_13_G) %>%
  pivot_longer(cols = V_13_A:V_13_G) %>%
  mutate(label = case_when(name == "V_13_A" ~ "Health",
                           name == "V_13_B" ~ "Education",
                           name == "V_13_C" ~ "Civil Rights",
                           name == "V_13_D" ~ "Pollution",
                           name == "V_13_E" ~ "Minority Rights",
                           name == "V_13_F" ~ "Climate Change",
                           name == "V_13_G" ~ "Economic Development")) %>%
  mutate(educ_var = case_when(V_3 %in% c("S1", "S2", "S3") ~ "University Educataion",
                              TRUE ~ "No University Education"),
         live_in_dist = case_when(as.character(F2) == as.character(V_8_B) ~ "Born in District",
                                  TRUE ~ "Born Elsewhere"),
         meeting_constituents = case_when(V_39_D == "<1 jam" ~ "Less than an hour",
                                          V_39_D == "TT/TJ" ~ NA_character_,
                                          V_39_D == "1-5 jam" ~ "1-5 hours",
                                          TRUE ~ "More than five hours"),
         money_spent = case_when(V_44 == "< 10 juta" ~ "<10m",
                                 V_44 == "10 - 50 juta" ~ "10-50m",
                                 V_44 == "TT/TJ" ~ NA_character_,
                                 TRUE ~ ">50m"),
         run_before = case_when(V_9 == "Ya" ~ "Run Before",
                                TRUE ~ "Never Ran"))

voter_df <-
  sikap_df %>%
  select(prob_importance_health, prob_importance_education, prob_importance_humanrights, 
         prob_importance_pollution, prob_importance_climatechange, prob_importance_devecon) %>%
  pivot_longer(everything()) %>%
  mutate(label = case_when(name == "prob_importance_health" ~ "Health",
                           name == "prob_importance_education" ~ "Education",
                           name == "prob_importance_humanrights" ~ "Civil Rights",
                           name == "prob_importance_pollution" ~ "Pollution",
                           name == "prob_importance_climatechange" ~ "Climate Change",
                           name == "prob_importance_devecon" ~ "Economic Development")) %>%
  group_by(label) %>%
  summarise(voter_val = mean(value == "Sangat penting", na.rm = T))



left_join(voter_df, candidate_df %>%
            filter(!is.na(meeting_constituents)) %>%
            group_by(label, meeting_constituents) %>%
            summarise(pol_val = mean(value == 4, na.rm = T)) %>% #4 = sangat penting
            pivot_wider(id_cols = label, values_from = pol_val, names_from = meeting_constituents)
) %>%
  select(label, voter_val, `Less than an hour`, `1-5 hours`, `More than five hours`) %>%
  mutate(diff_voter_a = voter_val - `Less than an hour`,
         diff_voter_b = voter_val - `1-5 hours`,
         diff_voter_c = voter_val -`More than five hours`) %>%
  mutate(label = factor(label, levels = c("Climate Change", "Pollution", "Civil Rights", "Economic Development", "Health", "Education"))) %>%
  arrange(label) %>%
  gt() %>%
  cols_label(voter_val = "Voters' Beliefs (V)",
             `Less than an hour` = "<1 hr (1)",
             `1-5 hours` = "1-5 hrs (2)",
             `More than five hours` = ">5 hrs (3)",
             diff_voter_a = "V - (1)",
             diff_voter_b = "V - (2)",
             diff_voter_c = "V - (3)",
             label = "",
             .fn = md) %>%
  fmt_percent(
    columns = -label,
    decimals = 1
  ) %>%
  tab_spanner(label = "Time Meeting Voters", 
              columns = c(3,4,5)) %>%
  tab_spanner(label = "Politicians' Second-Order Beliefs", 
              columns = c(3,4,5)) %>%
  tab_spanner(label = "Difference in Beliefs", 
              columns = c(6,7,8)) %>%
  tab_header(
    title = "Voters First-Order and Politicians' Second-Order Beliefs, by Amount of Time Meeting With Voters") %>%
  as_latex() %>%
  as.character() %>%
  cat(., file = "./_4_outputs/tables/table_a3.tex")



left_join(voter_df, candidate_df %>%
            filter(!is.na(money_spent)) %>%
            group_by(label, money_spent) %>%
            summarise(pol_val = mean(value == 4, na.rm = T)) %>%
            pivot_wider(id_cols = label, values_from = pol_val, names_from = money_spent)
) %>%
  select(label, voter_val, `<10m`, `10-50m`, `>50m`) %>%
  mutate(diff_voter_a = voter_val - `<10m`,
         diff_voter_b = voter_val - `10-50m`,
         diff_voter_c = voter_val -`>50m`) %>%
  mutate(label = factor(label, levels = c("Climate Change", "Pollution", "Civil Rights", "Economic Development", "Health", "Education"))) %>%
  arrange(label) %>%
  gt() %>%
  cols_label(voter_val = "Voters' Beliefs (V)",
             `<10m` = "<10m (1)",
             `10-50m` = "10-50m (2)",
             `>50m` = ">50m (3)",
             diff_voter_a = "V - (1)",
             diff_voter_b = "V - (2)",
             diff_voter_c = "V - (3)",
             label = "",
             .fn = md) %>%
  fmt_percent(
    columns = -label,
    decimals = 1
  ) %>%
  tab_spanner(label = "Money Spent", 
              columns = c(3,4,5)) %>%
  tab_spanner(label = "Politicians' Second-Order Beliefs", 
              columns = c(3,4,5)) %>%
  tab_spanner(label = "Difference in Beliefs", 
              columns = c(6,7,8)) %>%
  tab_header(
    title = "Voters First-Order and Politicians' Second-Order Beliefs, by Amount of Money Spent") %>%
  as_latex() %>%
  as.character() %>%
  cat(., file = "./_4_outputs/tables/table_a4.tex")


left_join(voter_df, candidate_df %>%
            filter(!is.na(E)) %>%
            filter(E != "4") %>%
            group_by(label, E) %>%
            summarise(pol_val = mean(value == 4, na.rm = T)) %>%
            pivot_wider(id_cols = label, values_from = pol_val, names_from = E)
) %>%
  mutate(diff_voter_a = voter_val - `Nomor Urut 1`,
         diff_voter_b = voter_val - `Nomor Urut 2`,
         diff_voter_c = voter_val - `Nomor Urut 3`) %>%
  mutate(label = factor(label, levels = c("Climate Change", "Pollution", "Civil Rights", "Economic Development", "Health", "Education"))) %>%
  arrange(label) %>%
  gt() %>%
  cols_label(voter_val = "Voters' Beliefs (V)",
             `Nomor Urut 1` = "(1)",
             `Nomor Urut 2` = "(2)",
             `Nomor Urut 3` = "(3)",
             diff_voter_a = "V - (1)",
             diff_voter_b = "V - (2)",
             diff_voter_c = "V - (3)",
             label = "",
             .fn = md) %>%
  fmt_percent(
    columns = -label,
    decimals = 1
  ) %>%
  tab_spanner(label = "List Position", 
              columns = c(3,4,5)) %>%
  tab_spanner(label = "Politicians' Second-Order Beliefs", 
              columns = c(3,4,5)) %>%
  tab_spanner(label = "Difference in Beliefs", 
              columns = c(6,7,8)) %>%
  tab_header(
    title = "Voters First-Order and Politicians' Second-Order Beliefs, by List Position") %>%
  as_latex() %>%
  as.character() %>%
  cat(., file = "./_4_outputs/tables/table_a5.tex")
