
# Call helper functions

source("helpers.R")

# Read in data

experiment_df <- read.csv("../data/raw/survey_experiment.csv")

# Clean variables

experiment_df <- experiment_df %>% mutate(
  
  Male = case_when(sarpanchgender == "rohit" ~ 1,
                   sarpanchgender == "bharti" ~ 0),
  
  Maratha = case_when(sarpanchcaste == "marathe" ~ 1,
                      sarpanchcaste == "kamble" ~ 0),
  
  authority = case_when(q9.2.1 == "Sarpanch" ~ 1,
                        q9.2.1 == "Someone else" ~ 0),
  
  pliability = case_when(q9.2.3 == "Resign" ~ 1,
                         q9.2.3 == "Resist" ~ 0),
  
  backlash = case_when(q9.2.4 == "Yes, it is likely" ~ 1,
                       q9.2.4 == "No, it is not likely" ~ 0),
  
  rural = case_when(q1.2 == "Village" ~ 1,
                    q1.2 == "Nagar palika in semi-urban/taluka/district center" |
                      q1.2 == "Mahanagar Palika in Jalgaon city/Dhule City/Malegaon City" ~ 0),
  
  age = q2.1,
  
  gender = case_when(q2.2 == "Female" ~ 1,
                     q2.2 == "Male" ~ 0),
  
  religion = q2.3,
  
  caste = q2.4,
  
  education = case_when(q2.5 == "None" |
                          q2.5 == "up to 5th grade" ~ "Primary school or less",
                        q2.5 == "up to 8th grade" |
                          q2.5 == "up to 10th grad" |
                          q2.5 == "up to 12th grade" ~ "Secondary school",
                        q2.5 == "post-graduates" |
                          q2.5 == "undergraduate" |
                          q2.5 == "doctorate" ~ "Undergraduate and above"),
  prior_vote = q3.2,
  
  knowledge_local_politics = case_when(q4.15 == "Not knowledgeable at all" ~ 0,
                                       q4.15 == "Extremely knowledgeable" |
                                         q4.15 == "Moderately knowledgeable" |
                                         q4.15 == "Slightly knowledgeable" |
                                         q4.15 == "Very knowledgeable" ~ 1),
  
  gender_norms = q10.1_1)


# Remove blank experimental observations

experiment_df <- experiment_df %>%
  filter(sarpanchgendercaste_combination != "NA NA")


# Restrict to relevant variables

final_experiment_df <- experiment_df %>%
  select(Male,
         Maratha,
         sarpanchgendercaste_combination,
         authority,
         pliability,
         backlash,
         rural,
         age,
         gender,
         religion,
         caste,
         education,
         prior_vote,
         knowledge_local_politics,
         gender_norms)


# Output to analysis data folder

write.csv(final_experiment_df, "../data/analysis/survey_experiment_clean.csv")

