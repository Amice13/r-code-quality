estimation_data %>%
  filter(!is.na(treatment)) %>%
  filter(treatment_observed == 0) %>%
  filter(Y6 == "Hampir semuanya dapat dipahami dengan baik") %>%
  mutate(other_people = (Y2 == "Ada")*1,
         very_honest = (Y7 == "Sangat jujur")*1)

library(gtable)
library(gt)

#get elections data
dprd2_data <- read_csv("./_3_data/dprd2_vote_w_seats.csv")
dprd2_data %<>% 
  select(kode_dapil, party, seats_won) %>% 
  na.omit() %>% 
  distinct() %>%
  mutate(kab_code = str_sub(kode_dapil, 1, 4)) %>%
  group_by(kab_code, party) %>%
  summarise(seats_won = sum(seats_won)) %>%
  group_by(kab_code) %>%
  mutate(total_seats = sum(seats_won),
         party_prop = seats_won/total_seats) %>%
  filter(party < 19) %>%
  na.omit() %>%
  group_by(kab_code) %>%
  summarise(hh_index = sum(party_prop*party_prop))



estimation_data %>%
  mutate(rank_number = case_when(E == "Nomor Urut 1" ~ 1,
                                 E == "Nomor Urut 2" ~ 2,
                                 E == "Nomor Urut 3" ~ 3,
                                 TRUE ~ NA_real_)) %>%
  mutate(code_kab = as.character(code_kab)) %>%
  left_join(.,dprd2_data, by = c("code_kab" = "kab_code")) %>%
  mutate(misperception = case_when(clim_change_misperc < 1 ~ "Low",
                                   clim_change_misperc > 1 ~ "High")) %>%
  group_by(misperception) %>%
  summarise(val = n(),
            gender_male = mean(V_1 == "Laki-laki", na.rm = T),
            education_college = mean(V_3 %in% c("S1", "S2", "S3"), na.rm = T),
            avg_age = mean(V_2, na.rm = T),
            eth_javanese = mean(V_5 == "Islam", na.rm = T),
            prev_election = mean(V_9 == "Ya", na.rm = T),
            hh_index = mean(hh_index, na.rm = T),
            rank_number = mean(rank_number, na.rm = T)) %>%
  ungroup() %>%
  filter(!is.na(misperception)) %>%
  gt(caption = "Misperceptions, by Demographic Characteristics") %>%
  cols_label(misperception = "Misperception",
             val = "N",
             gender_male = "Male (%)",
             education_college = "College (%)",
             avg_age = "Age",
             eth_javanese = "Islam (%)",
             prev_election = "Run Before (%)",
             hh_index = "HH (avg)",
             rank_number = "List (avg)",
             .fn = md) %>%
  fmt_percent(
    columns = -c(misperception, val, avg_age, hh_index),
    decimals = 1
  ) %>%
  fmt_number(columns = c(avg_age, hh_index, rank_number), decimals = 2) %>%
  as_latex() %>%
  as.character() %>%
  cat(., file = "./_4_outputs/tables/table_a7.tex")
