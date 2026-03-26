#### ---- Models to predict baseline levels of support for Bay du Nord 

library(tidyverse)
library(marginaleffects)
library(modelsummary)
library(broom)
library(knitr)
library(magrittr)
library(fixest)
library(tinytable)

setwd("/Users/srowa/Dropbox/Projects/Oil and gas cap/replication")

#### Load data ####

## Analysis data
full_field_analysis <- read_csv("input/full_field_analysis.csv")


#### Baseline models: without/with demographic controls #### 

## Re-level factor reference categories
full_field_analysis <- full_field_analysis %>% 
  mutate(province_bucket = fct_relevel(province_bucket,
                                       "General population", after = 0L),
         distance_bucket = fct_relevel(distance_bucket,
                                       ">350km", after = 0L),
         education = fct_relevel(education,
                                 c("High school", "Elementary", "Diploma", "BA", "Graduate"),
                                 after = 0L),
         gender = fct_relevel(gender, "Man",  after = 0L))

## Model with no controls
bdn_no_controls <- feols(bdn_baseline ~ sw(province_bucket, distance_bucket,
                                            ffff_bucket, ff_employed,
                                            climate_concern, climate_support),
                          data = full_field_analysis)

## Model with controls
bdn_controls <- feols(bdn_baseline ~ sw(province_bucket, distance_bucket,
                                        ffff_bucket, ff_employed,
                                        climate_concern, climate_support) +
                        education + age + gender,
                      data = full_field_analysis)

## Group no controls outputs
tidy_no_controls <- NULL
for (i in 1:length(bdn_no_controls)) {
  hold <- tidy(bdn_no_controls[[i]]) %>% 
    mutate(model = i)
  tidy_no_controls <- bind_rows(tidy_no_controls, hold)
}

## Group controls outputs
tidy_controls <- NULL
for (i in 1:length(bdn_controls)) {
  hold <- tidy(bdn_controls[[i]]) %>% 
    mutate(model = i)
  tidy_controls <- bind_rows(tidy_controls, hold)
}  

# Combine outputs
bdn_tt <- bind_rows(tidy_no_controls %>% mutate(spec = "sparse"), 
                    tidy_controls %>% mutate(spec = "controls")) %>% 
  select(-statistic) %>% 
  filter(str_detect(term, c("Intercept|age|gender|education"))==F) %>% 
  select(term, beta = estimate, se = std.error, p = p.value, model, spec) %>% 
  pivot_wider(names_from = "spec", values_from = beta:p) %>% 
  mutate(beta_sparse = ifelse(p_sparse < 0.05, 
                              paste(round(beta_sparse, 3), "*", sep = ""),
                              paste(round(beta_sparse, 3))),
         beta_controls = ifelse(p_controls < 0.05, 
                                paste(round(beta_controls, 3), "*", sep = ""),
                                paste(round(beta_controls, 3)))) %>% 
  mutate_at(vars(se_sparse, se_controls), 
            ~paste("(", round(., 3), ")", sep = "")) %>% 
  select(-starts_with("p_")) %>% 
  mutate(contrast = term) %>% 
  mutate(term = case_when(str_detect(term, "province_bucket") ~ "Province --- ref: general population",
                          str_detect(term, "distance_bucket") ~ "Distance from FF extraction  --- ref: >350km",
                          str_detect(term, "ffff_bucket") ~ "Family and friends in FF  --- ref: 0/20",
                          str_detect(term, "ff_employed") ~ "Employed in FF --- ref: No",
                          term == "climate_concern" ~ "Climate concern",
                          term == "climate_support" ~ "Climate support"))  %>% 
  mutate(contrast = case_when(str_detect(contrast, "Alberta") ~ "Alberta",
                              str_detect(contrast, "Newfoundland") ~ "Newfoundland",
                              str_detect(contrast, "<100km") ~ "<100km",
                              str_detect(contrast, "<350km") ~ "<350km",
                              str_detect(contrast, "1-4") ~ "1-4",
                              str_detect(contrast, "5+") ~ "5+",
                              str_detect(contrast, "Yes") ~ "Yes",
                              str_detect(contrast, "climate") ~ "One-unit on 0-10 scale")) %>% 
  select(model, term, contrast, ends_with("sparse"), ends_with("controls")) %>% 
  tt() 
save_tt(bdn_tt, "output/bdn_baseline.docx", overwrite = F)



#### Controlling for voting intention #### 


## Model with no controls
m_vote_no_controls <- feols(bdn_baseline ~ sw(province_bucket, distance_bucket,
                                           ffff_bucket, ff_employed,
                                           climate_concern, climate_support),
                         data = full_field_analysis %>% 
                           filter(!is.na(vote_intention)))

## Model with controls
m_vote_controls <- feols(bdn_baseline ~ sw(province_bucket, distance_bucket,
                                        ffff_bucket, ff_employed,
                                        climate_concern, climate_support) +
                        education + age + gender + vote_intention,
                      data = full_field_analysis)

## Group no controls outputs
vote_no_controls <- NULL
for (i in 1:length(m_vote_no_controls)) {
  hold <- tidy(m_vote_no_controls[[i]]) %>% 
    mutate(model = i)
  vote_no_controls <- bind_rows(vote_no_controls, hold)
}

## Group controls outputs
vote_controls <- NULL
for (i in 1:length(m_vote_controls)) {
  hold <- tidy(m_vote_controls[[i]]) %>% 
    mutate(model = i)
  vote_controls <- bind_rows(vote_controls, hold)
}  

# Combine outputs
vote_tt <- bind_rows(vote_no_controls %>% mutate(spec = "sparse"), 
                    vote_controls %>% mutate(spec = "controls")) %>% 
  select(-statistic) %>% 
  filter(str_detect(term, c("Intercept|age|gender|education|vote"))==F) %>% 
  select(term, beta = estimate, se = std.error, p = p.value, model, spec) %>% 
  pivot_wider(names_from = "spec", values_from = beta:p) %>% 
  mutate(beta_sparse = ifelse(p_sparse < 0.05, 
                              paste(round(beta_sparse, 3), "*", sep = ""),
                              paste(round(beta_sparse, 3))),
         beta_controls = ifelse(p_controls < 0.05, 
                                paste(round(beta_controls, 3), "*", sep = ""),
                                paste(round(beta_controls, 3)))) %>% 
  mutate_at(vars(se_sparse, se_controls), 
            ~paste("(", round(., 3), ")", sep = "")) %>% 
  select(-starts_with("p_")) %>% 
  mutate(contrast = term) %>% 
  mutate(term = case_when(str_detect(term, "province_bucket") ~ "Province --- ref: general population",
                          str_detect(term, "distance_bucket") ~ "Distance from FF extraction  --- ref: >350km",
                          str_detect(term, "ffff_bucket") ~ "Family and friends in FF  --- ref: 0/20",
                          str_detect(term, "ff_employed") ~ "Employed in FF --- ref: No",
                          term == "climate_concern" ~ "Climate concern",
                          term == "climate_support" ~ "Climate support"))  %>% 
  mutate(contrast = case_when(str_detect(contrast, "Alberta") ~ "Alberta",
                              str_detect(contrast, "Newfoundland") ~ "Newfoundland",
                              str_detect(contrast, "<100km") ~ "<100km",
                              str_detect(contrast, "<350km") ~ "<350km",
                              str_detect(contrast, "1-4") ~ "1-4",
                              str_detect(contrast, "5+") ~ "5+",
                              str_detect(contrast, "Yes") ~ "Yes",
                              str_detect(contrast, "climate") ~ "One-unit on 0-10 scale")) %>% 
  select(model, term, contrast, ends_with("sparse"), ends_with("controls")) %>% 
  tt() 

save_tt(vote_tt, "output/bdn_baseline_vote.docx", overwrite = F)
# And paste table in text





