
#make politician data
wave2 <- read_csv("./_3_data/politician.csv")

#recode main vars
estimation_data <-
  wave2 %>%
  mutate(treatment = case_when(GG == "Kelompok A" ~ 1,
                               GG == "Kelompok B" ~ 0,
                               TRUE ~ NA_real_)) %>%
  mutate(treatment_observed = case_when(GG2 == "Ya, pernah lihat/baca hasilnya" ~ 1,
                                        GG2 == "Tidak pernah" ~ 0,
                                        TRUE ~ NA_real_)) %>%
  mutate_at(vars(V_12_A, V_12_B, V_12_C, V_12_D, V_12_E, V_12_F, V_12_G,
                 V_13_A, V_13_B, V_13_C, V_13_D, V_13_E, V_13_F, V_13_G,
                 V_62_A, V_62_B,
                 V_61_A, V_61_B, V_61_C, V_61_D, V_61_E),
            funs(case_when(. == "Sangat penting" ~ 4,
                           . == "Penting" ~ 3,
                           . %in% c("Tidak Penting", "Tidak penting")  ~ 2,
                           . == "Sangat tidak penting" ~ 1,
                           TRUE ~ NA_real_))) %>%
  rowwise() %>%
  mutate(avg_index_v12 = sum(V_12_D, V_12_F, na.rm = T)/2,
         avg_index_v13 = sum(V_13_D, V_13_F, na.rm = T)/2)




#recode climate change misperceptions index
estimation_data <-
  estimation_data %>%
  mutate(pollution_misperc = case_when(A == "Wave 1" ~ 3.52 - V_13_D, #these raw digits are the median values in wave 1
                                       TRUE ~ NA_real_),
         clim_change_misperc = case_when(A == "Wave 1" ~ 3.39 - V_13_F, #these raw digits are the median values wave 1
                                         TRUE ~ NA_real_),
  ) %>%
  group_by(ID) %>%
  mutate(pollution_misperc = mean(pollution_misperc, na.rm = T),
         pollution_misperc = case_when(is.nan(pollution_misperc) ~ NA_real_,
                                       TRUE ~ pollution_misperc),
         clim_change_misperc = mean(clim_change_misperc, na.rm = T),
         clim_change_misperc = case_when(is.nan(clim_change_misperc) ~ NA_real_,
                                         TRUE ~ clim_change_misperc))



#make asset index vars
lhk_data <- read_csv("./_3_data/clientelism-zlhkpn.csv") %>% filter(tahun_lapor == 2022)

#recode certain district names to match across datasets and direct merge
estimation_data %<>% 
  mutate(kabupaten = case_when(str_sub(F2, 1, 5) == "KOTA " ~ F2,
                               TRUE ~ paste0("KABUPATEN ", F2))) %>%
  mutate(kabupaten = case_when(kabupaten == "KABUPATEN TOLI TOLI" ~ "KABUPATEN TOLITOLI",
                               TRUE ~ kabupaten)) %>%
  left_join(., lhk_data, by = c("kabupaten"))




#make sikap data, this is the public opinion stuff for the main analysis
sikap_df <- read_csv("./_3_data/sikap1.csv")
sikap_df_2 <- read_csv("./_3_data/sikap2.csv")
sikap_df_3 <- read_csv("./_3_data/sikap3.csv")

sikap_df <- bind_rows(sikap_df, sikap_df_2, sikap_df_3)



#this is the supplemental stuff from week 36 in the mechanisms analysis
sikap_36 <- read_csv("./_3_data/sikap36.csv")


