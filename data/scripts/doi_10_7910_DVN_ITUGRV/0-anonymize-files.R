library(tidyverse)
library(readxl)

read_rds("bld/survey_data_wave_1_with_indices.rds") %>%
  select(Respondent_type,
         pid,
         starts_with("research_type"),
         year_complete_phd,
         Discipline,
         econ_subfield,
         ends_with('_approximately_when_was'),
         ends_with("how_many"),
         ends_with("about_last"),
         starts_with("publicly_posting_distribution_of_opinion_discipline"),
         starts_with("analysis_pre_registration_distribution_of_opinion_discipline"),
         ends_with("your_opinion")) %>%
  filter(Discipline == "Economics") %>%
  mutate(year_complete_phd = ifelse(year_complete_phd < 2010, 0, 3000)) %>%
  write_rds("economics/data/survey_wave_1_min_raw.rds")



read_rds('bld/wave_1_response_rate.rds') %>%
  select(Discipline, email_usca, completed_survey, `Career stage`) %>%
  filter(Discipline == "Economics") %>%
  write_rds("economics/data/response_rate_min.rds")



readxl::read_xlsx('ext/wave_1_sample_audit_raw-2.xlsx') %>%
  select(audit,
         starts_with("verified"),
         pid,
         has_posted_data_before,
         manual_subfield = `Nick Subfield`,
         Theory,
         has_registered_before,
         completed_survey) %>%
  write_rds("economics/data/audit_data_min_raw.rds")
