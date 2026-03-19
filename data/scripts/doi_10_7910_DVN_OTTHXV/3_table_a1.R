
summary(factor(as.character(sikap_df$demog_edu)))

sikap_df %>%
  mutate(demog_province = trimws(demog_province)) %>%
  summarise(male = mean(demog_sex == "Laki-laki", na.rm = T),
            female = mean(demog_sex == "Perempuan", na.rm = T),
            age_24 = mean(demog_age <= 24, na.rm = T),
            age_25_34 = mean(demog_age <= 34 & demog_age > 24, na.rm = T),
            age_35_44 = mean(demog_age <= 44 & demog_age > 34, na.rm = T),
            age_45_54 = mean(demog_age <= 54 & demog_age > 44, na.rm = T),
            age_55 = mean(demog_age > 54, na.rm = T),
            region_sum = mean(demog_province %in% c("Bangka-Belitung", "Bengkulu", "Jambi", "Nanggroe Aceh", "Sumatera Barat",
            "Sumatera Selatan","Sumatera Utara", "Lampung", "Kepulauan Riau", "Riau"), na.rm = T),
            region_java = mean(demog_province %in% c("DKI Jakarta", "D.I. Yogyakarta", "Banten", "Jawa Barat", "Jawa Tengah", "Jawa Timur", 'Bali'), na.rm = T),
            region_other = mean(demog_province %in% c("Gorontalo", "Kalimantan Barat", "Kalimantan Tengah", "Kalimantan Selatan", "Kalimantan Timur", "Kalimantan Utara", "Maluku", "Maluku Utara", "Nusa Tenggara Barat", 
                                                      "Nusa Tenggara Timur", "Papua", "Papua Barat", "Sulawesi Barat", "Sulawesi Tengah", "Sulawesi Tenggara", "Sulawesi Selatan", "Sulawesi Utara"), na.rm = T),
            religion_islam = mean(demog_religion == "Islam", na.rm = T),
            religion_chris = mean(demog_religion == "Kristen Protestan", na.rm = T),
            religion_other = mean(demog_religion %in% c("Buddha", "Hindu", "Katolik", "Konghucu", "Lain-lain", "Kepercayaan tradisional Indonesia", "Tidak menganut agama apapun")),
            educ_univ = mean(demog_edu %in% c("Tamat Diploma atau Sarjana", "Tamat Program Pasca-sarjana"), na.rm = T),
            educ_hs = mean(demog_edu == "Tamat SMA / Sederajat", na.rm = T),
            educ_low = mean(demog_edu %in% c("Tamat Sekolah Dasar", "Tamat Sekolah Menengah Pertama / Sederajat", "Tidak bersekolah"), na.rm = T)) %>%
  mutate_all(funs(.*100)) %>%
  rbind(c(49.8, 50.2, 17.9, 26.3, 22.4, 16.4, 17.0, 20.4, 61.1, 18.5, 87.4, 9.3, 2.8, 7.2, 25.3, 67.4)) %>%
  t() %>%
  data.frame() %>%
  rownames_to_column() %>%
  mutate_at(vars(X1), funs(round(as.numeric(.), digits = 1))) %>%
  
  mutate(rowname = case_when(rowname == "age_24" ~ "Age: 18-24",
                         rowname == "age_25_34" ~ "Age: 25-34",
                         rowname == "age_35_44" ~ "Age: 35-44",
                         rowname == "age_45_54" ~ "Age: 45-54",
                         rowname == "age_55" ~ "Age: 55+",
                         rowname == "educ_univ" ~ "Education: College",
                         rowname == "educ_hs" ~ "Education: HS",
                         rowname == "educ_low" ~ "Education: Less than HS",
                         rowname == "male" ~ "Gender: Man",
                         rowname == "female" ~ "Gender: Woman",
                         
                         rowname == "religion_islam" ~ "Religion: Islam",
                         rowname == "religion_chris" ~ "Religion: Christian",
                         rowname == "religion_other" ~ "Religion: Other",
                         rowname == "region_sum" ~ "Region: Sumatera",
                         rowname == "region_java" ~ "Region: Java",
                         rowname == "region_other" ~ "Region: Other")) %>%
  gt(caption = "Descriptive Statistics and Population Comparison") %>%
  gt::cols_label(rowname = "Category", 
                 X1 = "SIKAP (%)" , 
                 X2 = "Population (%)") %>%
  as_latex() %>%
  cat(., file = "./_4_outputs/tables/table_a1.tex")
