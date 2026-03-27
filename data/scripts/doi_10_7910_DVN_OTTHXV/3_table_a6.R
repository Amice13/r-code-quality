
estimation_data %>%
  group_by(treatment, treatment_observed) %>%
  summarise(val = n(),
            gender_male = mean(V_1 == "Laki-laki", na.rm = T),
            education_college = mean(V_3 %in% c("S1", "S2", "S3"), na.rm = T),
            avg_age = mean(V_2, na.rm = T),
            eth_javanese = mean(V_5 == "Islam", na.rm = T),
            prev_election = mean(V_9 == "Ya", na.rm = T)) %>%
  mutate(treatment = case_when(treatment == 0 ~ "Control",
                               treatment == 1 ~ "Treated"),
         treatment_observed = case_when(treatment_observed == 0 ~ "Not Obtained",
                                        treatment_observed == 1 ~ "Obtained")) %>%
  ungroup() %>%
  filter(!is.na(treatment)) %>%
  gt(caption = "SUTVA Violations, by Demographic Characteristics") %>%
  cols_label(treatment = "Treatment",
             treatment_observed = "Contaminated",
             val = "N",
             gender_male = "Male (%)",
             education_college = "College (%)",
             avg_age = "Age",
             eth_javanese = "Islam (%)",
             prev_election = "Run Before (%)",
             .fn = md) %>%
  fmt_percent(
    columns = -c(treatment, treatment_observed, val, avg_age),
    decimals = 1
  ) %>%
  as_latex() %>%
  as.character() %>%
  cat(., file = "./_4_outputs/tables/table_a6.tex")
