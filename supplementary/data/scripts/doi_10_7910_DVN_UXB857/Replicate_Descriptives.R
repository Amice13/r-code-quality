library(gtsummary) # v1.4.1
library(jtools) #v2.1.3
library(modelsummary) #v0.7.0
library(sandwich) #v3.0-1
library(stargazer) #v5.2.2
library(scales) #v1.1.1
library(tidyverse) #v1.3.1
library(vtable) #v1.3.1

### Load data
oia <- read.csv('oia_apsr_final_replication.csv')

## Remove Rs that failed attention checks
oia %>%
  filter(informed_consent == 1 & attention_check1 == 3 & attention_check2 == 1) %>%
  filter(!is.na(attention_check1)) -> oia


####
oia %>%
  mutate_all(na_if,"") %>%
  mutate(income = case_when(
    hhi == 1 | hhi == 2 ~ "20k or less",
    hhi >= 3 & hhi <= 10 ~ "20 - 59k",
    hhi >= 11 & hhi <= 18 ~ "60 - 99k",
    hhi >= 19 ~ "100k or more",
    hhi == -3105 ~ "NA"),
    region = factor(case_when(
      region == 1 ~ "Northeast",
      region == 2 ~ "Midwest",
      region == 3 ~ "South",
      region == 4 ~ "West")),
    gender = factor(case_when(gender == 1 ~ "Male",
                              gender == 2 ~ "Women")),
    age = factor(case_when(
      age >= 18 & age <= 29 ~ "18 - 29",
      age >= 30 & age <= 41 ~ "30 - 41",
      age >= 42 & age <= 54 ~ "42 - 54",
      age >= 55 ~ "55 or more",
      TRUE ~ "other")),
    educ = factor(case_when(
      educ == 1 ~ "Less than HS", 
      educ == 2 ~ "HS",
      educ == 3 ~ "Some college",
      educ == 4 ~ "AA",
      educ == 5 ~ "BA",
      educ == 6 ~ "Postgraduate",
      TRUE ~ "other")),
    ethnicity = factor(case_when(
      ethnicity_1 == 1 ~ "White",
      ethnicity_2 == 1 ~ "Hispanic",
      ethnicity_3 == 1 ~ "Black",
      ethnicity_4 == 1 ~ "Asian",
      ethnicity_5 == 1 ~ "Native American",
      ethnicity_6 == 1 ~ "Other",
      TRUE ~ "other"))) %>%
  mutate(pid_3level = case_when(pid_7level == 1 ~ 1,
                                pid_7level == 2 ~ 1,
                                pid_7level == 3 ~ 1,
                                pid_7level == 4 ~ 2,
                                pid_7level == 5 ~ 3,
                                pid_7level == 6 ~ 3,
                                pid_7level == 7 ~ 3),
         polid_3level = case_when(pol_ideology == 1 ~ 1,
                                  pol_ideology == 2 ~ 1,
                                  pol_ideology == 3 ~ 1,
                                  pol_ideology == 4 ~ 2,
                                  pol_ideology == 5 ~ 3,
                                  pol_ideology == 6 ~ 3,
                                  pol_ideology == 7 ~ 3)) %>%
  rename(age_qual = age.1) %>%
  mutate(income = relevel(factor(income), 
                          ref = "20 - 59k"),
         age = relevel(factor(age),
                       ref = "30 - 41"),
         educ = relevel(factor(educ),
                        ref = "HS"),
         treatment_s2_gen = relevel(factor(treatment_s2_gen),
                                    ref = "Control"),
         gender = relevel(factor(gender),
                          ref = "Male"),
         ethnicity = relevel(factor(ethnicity),
                             ref = "White") ) -> oia

############### ACS Table from IPUMS

acs19 <- haven::read_dta("acs_2019.dta")

acs19 %>% 
  filter(age >= 18,
         ftotinc >= 0) %>%
  select(sex, age, region, ftotinc, educd, race, rachsing, perwt) %>%
  mutate(sex = case_when(sex == 1 ~ "Male",
                         sex == 2 ~ "Women"),
         age = case_when(age >= 18 & age <= 29 ~ "18 - 29",
                         age >= 30 & age <= 41 ~ "30 - 41",
                         age >= 42 & age <= 54 ~ "42 - 54",
                         age >= 55 ~ "55 or more",
                         TRUE ~ "other"),
         region = case_when(region >= 11 & region <= 13 ~ "Northeast",
                             region >= 21 & region <= 23 ~ "Midwest",
                             region >= 31 & region <= 34 ~ "South",
                             region >= 41 & region <= 43 ~ "West"),
         education = case_when(educd == 00 | 
                                  educd >= 003 & educd <= 061 ~ "Less than HS",
                                educd >= 062 & educd <= 064 ~ "HS",
                                educd >= 065 & educd <= 080 |
                                  educd == 090 | educd == 100 | 
                                  educd >= 110 & educd <= 113 ~ "Some college",
                                educd >= 081 & educd <= 083 ~ "AA",
                                educd == 101 ~ "BA",
                                educd >= 114 & educd <= 116 ~ "Postgraduate",
                                educd == 999 & educd == 001 ~ "NA",
                                TRUE ~ "NA"),
         income = case_when(ftotinc >= 0 & ftotinc <= 20000 ~ "20k or less",
                             ftotinc > 20000 & ftotinc <= 59999 ~ "20 - 59k",
                             ftotinc >= 60000 & ftotinc <= 99999 ~ "60 - 99k",
                             ftotinc >= 100000 & ftotinc <= 9999997 ~ "100k or more",
                             ftotinc == 9999998 | ftotinc == 9999999 ~ "NA"),
         rachsing = case_when(rachsing == 1 ~ "White",
                              rachsing == 2 ~ "Black",
                              rachsing == 3 ~ "Native American",
                              rachsing == 4 ~ "Asian",
                              rachsing == 5 ~ "Hispanic")) %>%
  mutate(region = as.factor(region),
         sex = as.factor(sex),
         age = as.factor(age),
         income = as.factor(income),
         education = as.factor(education),
         rachsing = as.factor(rachsing)) -> acs19_v2


# Select variables wanted
acs19_v2 %>%
  select(sex, age, rachsing, region, education, income) %>%
  rename(gender = sex,
         income = income,
         region = region,
         education = education,
         ethnicity = rachsing) -> acs19_v2

# Add data label
acs19_v2$data_kind <- rep("acs", length = nrow(acs19_v2))

# Select only the data you want
oia %>%
  rename(education = educ) %>%
  select(gender, age, ethnicity, region, education, income) %>%
  mutate(gender = as.factor(gender),
         age = as.factor(age),
         ethnicity = as.factor(ethnicity),
         region = as.factor(region),
         education = as.factor(education),
         income = as.factor(income)) -> demographics_t2


# add data label to demographics
demographics_t2$data_kind <- rep("sample", length = nrow(demographics_t2))



#############################################     
################ Table A2 ###################
#############################################  

#### Merge the data
table2a <- full_join(acs19_v2, demographics_t2)

# Print results
table2a %>%
  mutate(data_kind2 = case_when(
    data_kind == "acs" ~ 1,
    data_kind == "sample" ~ 2,
  )) %>% 
  sumtable(group = "data_kind2",
           summ=c('notNA(x)','mean(x)'),
           factor.counts = T)


#############################################     
################ Table A3 ###################
#############################################     

oia %>% 
  mutate(pid_3level = factor(case_when(pid_3level == 1 ~ "Democrat",
                                       pid_3level == 2 ~ "Independent",
                                       pid_3level == 3 ~ "Republican")),
         polid_3level = factor(case_when(polid_3level == 1 ~ "Liberal",
                                         polid_3level == 2 ~ "Moderate",
                                         polid_3level == 3 ~ "Conservative"))) %>%
  select(pid_3level, polid_3level) %>%
  sumtable(
    factor.counts = T,
    out = "viewer")


#############################################     
################ Table A4 ###################
#############################################     

oia %>%
  mutate(pid_3level = as.factor(pid_3level),
         polid_3level = as.factor(polid_3level)) %>%
  select(gender, income, educ, age, region, ethnicity,
         pid_3level, polid_3level,
         treatment_s1) %>%
  sumtable(
    group = "treatment_s1",
    factor.counts = T,
    out = "viewer")

#############################################     
################ Table A5 ###################
#############################################     

oia %>%
  mutate(treatment_s2_gen = factor(treatment_s2_gen,
                                   levels = c("Control",
                                              "Doc Only",
                                              "Access Undoc")),
         pid_3level = as.factor(pid_3level),
         polid_3level = as.factor(polid_3level)) %>%
  select(gender, income, educ, age, region, ethnicity, 
         pid_3level, polid_3level,
         treatment_s2_gen) %>%
  sumtable(
    out = "viewer",
    group = "treatment_s2_gen",
    factor.counts = T)



