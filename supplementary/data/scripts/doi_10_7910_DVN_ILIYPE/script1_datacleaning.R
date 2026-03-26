#---- SCRIPT 1/2 ----
#---- DATA WRANGLING & DESCRIPTIVE STATS----


# Packages
remove(list=ls())

install.packages("pacman")
library(pacman)

p_load(here, rio, ggpubr, dplyr, jtools, estimatr, stargazer, ggdist,
       modelsummary, forcats, Hmisc, scales, stringr, cregg, haven, viridis)

df <- import(here("data", "UniversityofOxford_Results_230523_CLIENT_V3.sav"))

# ---- Reshape data ----

# # Judgements for the two profiles in each round:
# # A and B are the profiles, Q1 Q2 are the questions, final number is the task (eg Q2_B2 is Q2, profile B, round 2)
# Q1_A1
# Q1_B1 
# Q2_A1
# Q2_B1 
# ...
# Q2_B4: Q2, Profile B, round 4. 

# # The attributes follow the pattern:
# q_attr6_concept1_task1
# # attr: the attribute.
# # concept: profile A or B
# # task: task
# # Therefore:
# # concept1_task1 == ProfA
# # concept2_task1 == ProfB
# # concept1_task2 == ProfC
# # concept2_task2 == ProfD
# # concept1_task3 == ProfE
# # concept2_task3 == ProfF
# # concept1_task4 == ProfG
# # concept2_task4 == ProfH
# # So I replace these and give them the profile A-H names

names(df) <- gsub("concept1_task1", "_ProfA", names(df), fixed = T)
names(df) <- gsub("concept2_task1", "_ProfB", names(df), fixed = T)
names(df) <- gsub("concept1_task2", "_ProfC", names(df), fixed = T)
names(df) <- gsub("concept2_task2", "_ProfD", names(df), fixed = T)
names(df) <- gsub("concept1_task3", "_ProfE", names(df), fixed = T)
names(df) <- gsub("concept2_task3", "_ProfF", names(df), fixed = T)
names(df) <- gsub("concept1_task4", "_ProfG", names(df), fixed = T)
names(df) <- gsub("concept2_task4", "_ProfH", names(df), fixed = T)

# First cases
# want to only keep the Qs for profile A, round 1
as <- df %>%
  dplyr::select(-ends_with(c("_ProfB", "_ProfC", "_ProfD", "_ProfE", "_ProfF", "_ProfG", "_ProfH"))) %>%
  rename_at(vars(ends_with("_ProfA")), ~ str_remove(., "_ProfA")) %>%
  dplyr::select(-Q1_B1,-Q2_B1, -Q1_A2, -Q2_A2, -Q1_B2, -Q2_B2) %>%
  rename(Q1 = Q1_A1, Q2 = Q2_A1) %>% 
  dplyr::select(ID, Q1, Q2, q_attr1_, q_attr2_, q_attr3_, q_attr4_,
                q_attr5_, q_attr6_, q_attr7_, q_attr8_, q_attr9_, uk_grp_as_v2, 
                pastvote_ge_2019, politics_scale_profile_update, profile_gender, profile_socialgrade_cie,
                age, highest_education_gce, xprofile_gross)

as$task <- "Task1"
as$profile <- "A"

# Second cases

bs <- df %>%
  dplyr::select(-ends_with(c("_ProfA", "_ProfC", "_ProfD", "_ProfE", "_ProfF", "_ProfG", "_ProfH"))) %>%
  rename_at(vars(ends_with("_ProfB")), ~ str_remove(., "_ProfB")) %>%
  #dplyr::select(-Q1_A1, -Q2_A1, -Q1_A2, -Q1_B2, -Q2_A2, -Q2_B2) %>%
  rename(Q1 = Q1_B1, Q2 = Q2_B1) %>% # First question, Profile B, Task 1
  dplyr::select(ID, Q1, Q2, q_attr1_, q_attr2_, q_attr3_, q_attr4_,
                q_attr5_, q_attr6_, q_attr7_, q_attr8_, q_attr9_, uk_grp_as_v2, 
                pastvote_ge_2019, politics_scale_profile_update, profile_gender, profile_socialgrade_cie,
                age, highest_education_gce, xprofile_gross)

bs$task <- "Task1"
bs$profile <- "B"

# Third cases

cs <- df %>%
  dplyr::select(-ends_with(c("_ProfA", "_ProfB", "_ProfD", "_ProfE", "_ProfF", "_ProfG", "_ProfH"))) %>%
  rename_at(vars(ends_with("_ProfC")), ~ str_remove(., "_ProfC")) %>%
  #dplyr::select(-Q1_A1,-Q1_B1, -Q2_A1, Q2_B1, Q1_B2, Q2_B2) %>%
  rename(Q1 = Q1_A2, Q2 = Q2_A2) %>% 
  dplyr::select(ID, Q1, Q2, q_attr1_, q_attr2_, q_attr3_, q_attr4_,
                q_attr5_, q_attr6_, q_attr7_, q_attr8_, q_attr9_, uk_grp_as_v2, 
                pastvote_ge_2019, politics_scale_profile_update, profile_gender, profile_socialgrade_cie,
                age, highest_education_gce, xprofile_gross)

cs$task <- "Task2"
cs$profile <- "A"

# Fourth cases

ds <- df %>%
  dplyr::select(-ends_with(c("_ProfA", "_ProfB", "_ProfC", "_ProfE", "_ProfF", "_ProfG", "_ProfH"))) %>%
  rename_at(vars(ends_with("_ProfD")), ~ str_remove(., "_ProfD")) %>%
  #dplyr::select(-Q1_A1, -Q1_B1, -Q2_A1, -Q2_B1, -Q1_A2, -Q2_A2) %>%
  rename(Q1 = Q1_B2, Q2 = Q2_B2) %>% 
  dplyr::select(ID, Q1, Q2, q_attr1_, q_attr2_, q_attr3_, q_attr4_,
                q_attr5_, q_attr6_, q_attr7_, q_attr8_, q_attr9_, uk_grp_as_v2, 
                pastvote_ge_2019, politics_scale_profile_update, profile_gender, profile_socialgrade_cie,
                age, highest_education_gce, xprofile_gross)

ds$task <- "Task2"
ds$profile <- "B"

# Fifth cases (es)

es <- df %>%
  dplyr::select(-ends_with(c("_ProfA", "_ProfB", "_ProfC", "_ProfD", "_ProfF", "_ProfG", "_ProfH"))) %>%
  rename_at(vars(ends_with("_ProfE")), ~ str_remove(., "_ProfE")) %>%
  #dplyr::select(-Q1_A1, -Q1_B1, -Q2_A1, -Q2_B1, -Q1_A2, -Q2_A2) %>%
  rename(Q1 = Q1_A3, Q2 = Q2_A3) %>% 
  dplyr::select(ID, Q1, Q2, q_attr1_, q_attr2_, q_attr3_, q_attr4_,
                q_attr5_, q_attr6_, q_attr7_, q_attr8_, q_attr9_, uk_grp_as_v2, 
                pastvote_ge_2019, politics_scale_profile_update, profile_gender, profile_socialgrade_cie,
                age, highest_education_gce, xprofile_gross)

es$task <- "Task3"
es$profile <- "A"

# Sixth cases (fs)
fs <- df %>%
  dplyr::select(-ends_with(c("_ProfA", "_ProfB", "_ProfC", "_ProfD", "_ProfE", "_ProfG", "_ProfH"))) %>%
  rename_at(vars(ends_with("_ProfF")), ~ str_remove(., "_ProfF")) %>%
  # dplyr::select(-Q1_A1, -Q1_B1, -Q2_A1, -Q2_B1, -Q1_A2, -Q2_A2) %>%
  rename(Q1 = Q1_B3, Q2 = Q2_B3) %>% 
  dplyr::select(ID, Q1, Q2, q_attr1_, q_attr2_, q_attr3_, q_attr4_,
                q_attr5_, q_attr6_, q_attr7_, q_attr8_, q_attr9_, uk_grp_as_v2, 
                pastvote_ge_2019, politics_scale_profile_update, profile_gender, profile_socialgrade_cie,
                age, highest_education_gce, xprofile_gross)

fs$task <- "Task3"
fs$profile <- "B"

# seventh cases (gs)
gs <- df %>%
  dplyr::select(-ends_with(c("_ProfA", "_ProfB", "_ProfC", "_ProfD", "_ProfE", "_ProfF", "_ProfH"))) %>%
  rename_at(vars(ends_with("_ProfG")), ~ str_remove(., "_ProfG")) %>%
  # dplyr::select(-Q1_A1, -Q1_B1, -Q2_A1, -Q2_B1, -Q1_A2, -Q2_A2) %>%
  rename(Q1 = Q1_A4, Q2 = Q2_A4) %>% 
  dplyr::select(ID, Q1, Q2, q_attr1_, q_attr2_, q_attr3_, q_attr4_,
                q_attr5_, q_attr6_, q_attr7_, q_attr8_, q_attr9_, uk_grp_as_v2, 
                pastvote_ge_2019, politics_scale_profile_update, profile_gender, profile_socialgrade_cie,
                age, highest_education_gce, xprofile_gross)

gs$task <- "Task4"
gs$profile <- "A"

# Eight cases (hs, final)
hs <- df %>%
  dplyr::select(-ends_with(c("_ProfA", "_ProfB", "_ProfC", "_ProfD", "_ProfE", "_ProfF", "_ProfG"))) %>%
  rename_at(vars(ends_with("_ProfH")), ~ str_remove(., "_ProfH")) %>%
  #dplyr::select(-Q1_A1, -Q1_B1, -Q2_A1, -Q2_B1, -Q1_A2, -Q2_A2) %>%
  rename(Q1 = Q1_B4, Q2 = Q2_B4) %>% 
  dplyr::select(ID, Q1, Q2, q_attr1_, q_attr2_, q_attr3_, q_attr4_,
                q_attr5_, q_attr6_, q_attr7_, q_attr8_, q_attr9_, uk_grp_as_v2, 
                pastvote_ge_2019, politics_scale_profile_update, profile_gender, profile_socialgrade_cie,
                age, highest_education_gce, xprofile_gross)

hs$task <- "Task4"
hs$profile <- "B"

cases <- rbind(as, bs, cs, ds, es, fs, gs, hs)

cases <- cases %>% mutate(
  occupation = q_attr1_, 
  housing = q_attr2_ ,
  p_occupation = q_attr3_,
  region = q_attr4_,
  pol_office = q_attr5_,
  education = q_attr6_,
  gender = q_attr7_,
  age = q_attr8_,
  spouse_occupation = q_attr9_,
  group = uk_grp_as_v2,
  arm = case_when(group == 2 ~ "No treatment",
                  pol_office == 1 ~ "Councillor",
                  pol_office == 2 ~ "MP",
                  pol_office == 3 ~ "No experience")
)

cases_bl <- cases


## 07/11: add the filter to reduce to only the group asked this question
# This reduces the sample to 16k. 

cases <- cases %>% filter(group == 2) %>%
  mutate(occupation = recode(q_attr1_,
                             "1" = "Occ: Farm labourer",
                             "2" = "Occ: Supermarket employee",
                             "3" = "Occ: Secretary",
                             "4" = "Occ: Legal assistant",
                             "5" = "Occ: University lecturer",
                             "6" = "Occ: Paramedic",
                             "7" = "Occ: Barrister",
                             "8" = "Occ: Website designer"),
         occupation = as.factor(occupation),
         housing = recode(q_attr2_,
                          "1" = "Owns outright",
                          "2" = "Mortgage",
                          "3" = "Rents local authority",
                          "4" = "Rents private"),
         housing = as.factor(housing),
         p_occupation = recode(q_attr3_,
                               "1" = "BG: Farm labourer",
                               "2" = "BG: Supermarket employee",
                               "3" = "BG: Secretary",
                               "4" = "BG: Legal assistant",
                               "5" = "BG: University lecturer",
                               "6" = "BG: Paramedic",
                               "7" = "BG: Barrister",
                               "8" = "BG: Website designer"), 
         p_occupation = as.factor(p_occupation),
         spouse_occupation = recode(q_attr9_,
                                    "1" = "SP: Farm labourer",
                                    "2" = "SP: Supermarket employee",
                                    "3" = "SP: Secretary",
                                    "4" = "SP: Legal assistant",
                                    "5" = "SP: University lecturer",
                                    "6" = "SP: Paramedic",
                                    "7" = "SP: Barrister",
                                    "8" = "SP: Website designer"), 
         spouse_occupation = as.factor(spouse_occupation),
         region = recode(q_attr4_,
                         "1" = "Scotland",
                         "2" = "Wales",
                         "3" = "N. Ireland",
                         "4" = "London",
                         "5" = "S. East",
                         "6" = "N. East",
                         "7" = "S. West",
                         "8" = "N. West"),
         region = as.factor(region),
         pol_office = recode(q_attr5_, 
                             "1" = "Councillor",
                             "2" = "MP",
                             "3" = "None"), 
         pol_office = as.factor(pol_office),
         education = recode(q_attr6_, 
                            "1" = "University", 
                            "2" = "Oxford", 
                            "3" = "College", 
                            "4" = "Left at 16"),
         education = as.factor(education),
         gender = recode(q_attr7_, 
                         "1" = "Male",
                         "2" = "Female"),
         gender = as.factor(gender),
         age = recode(q_attr8_, 
                      "1" = "24",
                      "2" = "35",
                      "3" = "46",
                      "4" = "57",
                      "5" = "68",
                      "6" = "79"),
         age = as.factor(age),
         choice_working = if_else(Q1 == 1, 1, 0),
         choice_middle = if_else(Q1 %in% c(2:4), 1, 0),
         vote = case_when(pastvote_ge_2019 == 1 ~ 1,
                          pastvote_ge_2019 == 2 ~ 2,
                          pastvote_ge_2019 == 3 ~ 3,
                          pastvote_ge_2019 %in% c(4:7) ~ 4),
         class = case_when(profile_socialgrade_cie %in% c(1,2) ~ 3, # AB
                           profile_socialgrade_cie %in% c(3,4) ~ 2, # C
                           profile_socialgrade_cie %in% c(5,6) ~ 1), # DE
         resp_educ = case_when(highest_education_gce %in% c(1:3) ~ 1, #none, other, or GCSE
                               highest_education_gce %in% c(4:5, 8) ~ 2, # Alevel, below degree, technical
                               highest_education_gce %in% c(6) ~ 3) # degree
  )

cases$xprofile_gross[cases$xprofile_gross %in% c(16, 17)] <- NA     

cases <- cases %>%
  mutate(income = ntile(xprofile_gross, 3))

cases$education <- factor(cases$education,
                          levels = c("Left at 16", "College", "University", "Oxford"),
                          labels = c("Left at 16", "College", "University", "Oxford"))
cases$housing <- factor(cases$housing,
                        levels = c("Rents local authority", "Rents private", 
                                   "Mortgage", "Owns outright"),
                        labels = c("Rents local authority", "Rents private", 
                                   "Mortgage", "Owns outright"))
cases$occupation <- factor(cases$occupation,
                           levels = c("Occ: Farm labourer", "Occ: Supermarket employee", 
                                      "Occ: Secretary", "Occ: Legal assistant",
                                      "Occ: University lecturer", "Occ: Paramedic",
                                      "Occ: Barrister", "Occ: Website designer"),
                           labels = c("Occ: Farm labourer", "Occ: Supermarket employee", 
                                      "Occ: Secretary", "Occ: Legal assistant",
                                      "Occ: University lecturer", "Occ: Paramedic",
                                      "Occ: Barrister", "Occ: Website designer"))
cases$p_occupation <- factor(cases$p_occupation,
                             levels = c("BG: Farm labourer", "BG: Supermarket employee", 
                                        "BG: Secretary", "BG: Legal assistant",
                                        "BG: University lecturer", "BG: Paramedic",
                                        "BG: Barrister", "BG: Website designer"),
                             labels = c("BG: Farm labourer", "BG: Supermarket employee", 
                                        "BG: Secretary", "BG: Legal assistant",
                                        "BG: University lecturer", "BG: Paramedic",
                                        "BG: Barrister", "BG: Website designer"))
cases$spouse_occupation <- factor(cases$spouse_occupation,
                                  levels = c("SP: Farm labourer", "SP: Supermarket employee", 
                                             "SP: Secretary", "SP: Legal assistant",
                                             "SP: University lecturer", "SP: Paramedic",
                                             "SP: Barrister", "SP: Website designer"),
                                  labels = c("SP: Farm labourer", "SP: Supermarket employee", 
                                             "SP: Secretary", "SP: Legal assistant",
                                             "SP: University lecturer", "SP: Paramedic",
                                             "SP: Barrister", "SP: Website designer"))

cases_wns <- cases 

cases$Q2[cases$Q2 == 997] <- NA    # 2828 responses removed

labels <- c(occupation = "Occupation", housing = "Housing",
            p_occupation = "Parental Occ", region = "Birth Region",
            education = "Education", gender = "Gender",
            age = "Age", spouse_occupation = "Spouse Occ")

n_distinct(cases$ID) # 2084
n_distinct(cases_wns$ID) # 2084 
prop.table(table(cases_wns$Q2, cases_wns$task), 2)


cases_wns <- cases_wns %>% mutate(notsurebin = ifelse(Q2 == 997, 1, 0))

table(cases_wns$notsurebin) 
# 0    1 
# 9672 2828 

cases_wns %>% mutate(`Education` = case_when(resp_educ == 1 ~ "None/GCSE",
                                             resp_educ == 2 ~ "A-Level",
                                             resp_educ == 3 ~ "Degree+"),
                     `Social class` = case_when(class == 1 ~ "DE",
                                                class == 2 ~ "C",
                                                class == 3 ~ "AB"),
                     `Left-right` = if_else(politics_scale_profile_update == 8, NA, politics_scale_profile_update),
                     `Gender` = if_else(profile_gender == 1, "Male", "Female"),
                     Treatment = if_else(notsurebin == 1, "Non-response", "Response")) %>%
  select(`Education`, `Social class`, `Left-right`, `Gender`, Treatment) %>%
  datasummary_balance(~Treatment, ., stars = T, output = here("outputs_appendix/Table6.tex")) 

cases_wns %>% filter(task == "Task1") %>% mutate(`Education` = case_when(resp_educ == 1 ~ "None/GCSE",
                                                                         resp_educ == 2 ~ "A-Level",
                                                                         resp_educ == 3 ~ "Degree+"),
                                                 `Social class` = case_when(class == 1 ~ "DE",
                                                                            class == 2 ~ "C",
                                                                            class == 3 ~ "AB"),
                                                 `Left-right` = if_else(politics_scale_profile_update == 8, NA, politics_scale_profile_update),
                                                 `Gender` = if_else(profile_gender == 1, "Male", "Female"),
                                                 Treatment = if_else(notsurebin == 1, "Non-response", "Response")) %>%
  select(`Education`, `Social class`, `Left-right`, `Gender`, Treatment) %>%
  datasummary_balance(~Treatment, ., stars = T, output = "outputs_appendix/Table7.tex") 



plot(cj_freqs(cases, ~ occupation + housing + p_occupation + region
              + education + gender + age + spouse_occupation,))+
  theme(legend.position = "none")+
  scale_fill_viridis(discrete = TRUE, option = "plasma", 
                     begin = 0.15, 
                     end = 0.8, 
                     direction = 1) +
  scale_color_viridis(discrete = TRUE, option = "plasma", 
                      begin = 0.15, 
                      end = 0.8, 
                      direction = 1)+
  theme(axis.title.x = element_blank())+
  labs(title="Frequency of conjoint attribute values")
ggsave("Figure4.png", 
       path = here("outputs_appendix"),
       dpi=500, height=24, width=20, units="cm")



library(doBy)
library(modelsummary)
library(modelr)

dfX <- df %>% 
  mutate(
    vote = case_when(pastvote_ge_2019 == 1 ~ 1,
                     pastvote_ge_2019 == 2 ~ 2,
                     pastvote_ge_2019 == 3 ~ 3,
                     pastvote_ge_2019 %in% c(4:7) ~ 4),
    class = case_when(profile_socialgrade_cie %in% c(1,2) ~ 3, # AB
                      profile_socialgrade_cie %in% c(3,4) ~ 2, # C
                      profile_socialgrade_cie %in% c(5,6) ~ 1), # DE
    resp_educ = case_when(highest_education_gce %in% c(1:3) ~ 1, #none, other, or GCSE
                          highest_education_gce %in% c(4:5, 8) ~ 2, # Alevel, below degree, technical
                          highest_education_gce %in% c(6) ~ 3) # degree
  )

dfX <- dfX %>% 
  mutate(
    profile_gender = case_when(profile_gender == 1~ "Male",
                               profile_gender == 2 ~ "Female"))

dfX <- dfX %>% 
  mutate(
    class = case_when(class == 1 ~ "DE",
                      class == 2 ~ "C",
                      class == 3 ~ "AB"))

dfX <- dfX %>% 
  mutate(
    vote = case_when(vote == 1 ~ "Conservative",
                     vote == 2 ~ "Labour",
                     vote == 3 ~ "Lib Dem",
                     vote == 4 ~ "Other"))

dfX <- dfX %>% 
  mutate(resp_educ = case_when(resp_educ == 1 ~ "None to GCSE",
                               resp_educ == 2 ~ "A Level or Technical",
                               resp_educ == 3 ~ "Degree or above"))


summary.df<- subset(dfX, select = c(age, profile_gender, class, vote, 
                                    resp_educ, politics_scale_profile_update))
datasummary_skim(data=summary.df, type='categorical',
                 output = 'outputs_appendix/table2.tex')
datasummary_skim(data=summary.df,
                 output = 'outputs_appendix/table3.tex')


saveRDS(cases, file = "data/analysisdata.rds")

