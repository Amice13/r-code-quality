
models_data <- 
  estimation_data %>%
  filter(!is.na(treatment)) %>%
  filter(treatment_observed == 0) %>%
  filter(Y6 == "Hampir semuanya dapat dipahami dengan baik")



balance_data <-
  models_data %>%
  mutate(gender = case_when(V_1 == "Laki-laki" ~ 1,
                            V_1 == "Perempuan" ~ 0,
                            TRUE ~ NA_real_)) %>%
  mutate(age = V_2)  %>%
  mutate(education = case_when(V_3 == "Sekolah Menengah Atas (SMA) atau sederajat" ~ 1,
                               V_3 == "Diploma (D1/D2/D3)" ~ 2,
                               V_3 == "S1" ~ 3,
                               V_3 == "S2" ~ 4, 
                               V_3 == "S3" ~ 5,
                               TRUE ~ NA_real_))  %>%
  mutate(college_diploma = case_when(V_3 == "Sekolah Menengah Atas (SMA) atau sederajat" ~ 0,
                                     V_3 == "Diploma (D1/D2/D3)" ~ 1,
                                     V_3 == "S1" ~ 1,
                                     V_3 == "S2" ~ 1, 
                                     V_3 == "S3" ~ 1,
                                     TRUE ~ NA_real_))  %>%
  mutate(political_experience = case_when(V_9 == "Tidak" ~ 0, 
                                          V_9 == "Ya" ~ 1,
                                          TRUE ~ NA_real_)) %>%
  mutate(religion_islam = case_when(V_5 == "Islam" ~ 1,
                                    TRUE ~ 0),
         ethnicity_javanese = case_when(V_6 == "Jawa" ~ 1,
                                        TRUE ~ 0),
         ethnicity_sunda = case_when(V_6 == "Sunda" ~ 1,
                                     TRUE ~ 0),
         ethnicity_malay = case_when(V_6 == "Melayu" ~ 1,
                                     TRUE ~ 0))

p1 <- lm_robust(gender ~ treatment, data = balance_data) %>% tidy() %>% pull(p.value) %>% .[2]
p2 <- lm_robust(age ~ treatment, data = balance_data) %>% tidy() %>% pull(p.value) %>% .[2]
p3 <- lm_robust(college_diploma ~ treatment, data = balance_data) %>% tidy() %>% pull(p.value) %>% .[2]
p4 <- lm_robust(political_experience ~ treatment, data = balance_data) %>% tidy() %>% pull(p.value) %>% .[2]
p5 <- lm_robust(religion_islam ~ treatment, data = balance_data) %>% tidy() %>% pull(p.value) %>% .[2]
p6 <- lm_robust(ethnicity_javanese ~ treatment, data = balance_data) %>% tidy() %>% pull(p.value) %>% .[2]
p7 <- lm_robust(ethnicity_sunda ~ treatment, data = balance_data) %>% tidy() %>% pull(p.value) %>% .[2]
p8 <- lm_robust(ethnicity_malay ~ treatment, data = balance_data) %>% tidy() %>% pull(p.value) %>% .[2]

balance_data %>%
  ungroup() %>%
  group_by(treatment) %>%
  summarise(gender = mean(gender, na.rm = T),
            age = mean(age, na.rm = T),
            college_diploma = mean(college_diploma, na.rm = T),
            political_experience = mean(political_experience, na.rm = T),
            religion_islam = mean(religion_islam, na.rm = T),
            ethnicity_javanese = mean(ethnicity_javanese, na.rm = T),
            ethnicity_sunda = mean(ethnicity_sunda, na.rm = T),
            ethnicity_malay = mean(ethnicity_malay, na.rm = T)) %>%
  t() %>%
  data.frame() %>%
  rownames_to_column() %>%
  .[-1,] %>%
  bind_cols(c(p1, p2, p3, p4, p5, p6, p7, p8)) %>%
  set_colnames(c("rowname", "control", "treatment", "p")) %>%
  data.frame() %>%
  mutate_at(vars(control, treatment, p), funs(round(., 2))) %>%
  mutate(rowname = case_when(rowname == "age" ~ "Age",
                             rowname == "college_diploma" ~ "Education: College",
                             rowname == "gender" ~ "Gender: Man",
                             rowname == "political_experience" ~ "Political Experience",
                             
                             rowname == "religion_islam" ~ "Religion: Islam",
                             
                             rowname == "ethnicity_javanese" ~ "Ethnicity: Javanese",
                             rowname == "ethnicity_sunda" ~ "Ethnicity: Sundanese",
                             rowname == "ethnicity_malay" ~ "Ethnicity: Melayu")) %>%
  gt(caption = "Balance Test and Descriptives") %>%
  gt::cols_label(rowname = "Category", 
                 control = "Control" , 
                 treatment = "Treatment",
                 p = "p-value") %>%
  #tab_header(caption = "Balance Test and Descriptives") %>%
  as_latex() %>%
  cat(., file = "./_4_outputs/tables/table_a2.tex")
