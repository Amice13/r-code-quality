## Cleaning
rm(list = ls())
getwd()
library(tidyverse)
df <- read_rds("data/survey_wave_1_min_raw.rds")
audit_data <- read_rds('data/audit_data_min_raw.rds')
source("code/define-levels.R")

## main survey data -----------------------------------
df <-  df %>%
  mutate(`Career stage` = factor(if_else(grepl("faculty", Respondent_type), 'Published Author', 'PhD Student'),
                                 levels = career_stage_levels),
         Discipline = factor(Discipline, levels = discipline_levels),
         `Research type` = factor(
           case_when(
             research_type_quant_experimental == TRUE ~ "Experimental",
             research_type_quant_observational == TRUE |
               research_type_quant_other == TRUE ~ "Quantitative non-experimental",
             TRUE ~ "Qualitative or Theoretical"),
           levels = research_type_levels),
         has_done_posting_study_instruments = (fct_explicit_na(posting_study_instruments_approximately_how_many, 0) != 0),
         has_done_publicly_posting = (fct_explicit_na(publicly_posting_approximately_how_many, 0) != 0),
         has_done_analysis_pre_registration = (fct_explicit_na(analysis_pre_registration_approximately_how_many, 0) != 0),
         has_done_any_practice_ever = (has_done_posting_study_instruments|has_done_publicly_posting|has_done_analysis_pre_registration),
         has_done_posting_study_instruments_last_paper = (replace_na(posting_study_instruments_think_about_last, 0) != 0),
         has_done_publicly_posting_last_paper = (replace_na(publicly_posting_think_about_last, 0) != 0),
         has_done_analysis_pre_registration_last_paper = (replace_na(analysis_pre_registration_think_about_last, 0) != 0),
         has_done_any_practice_ever_last_paper = (has_done_posting_study_instruments_last_paper|has_done_publicly_posting_last_paper|has_done_analysis_pre_registration_last_paper)
  )



## Time Trend -----------------------------------
## time trend for subfields -----------------------------------
create_time_trend_df_sub <- function(df, author_cutoff_year = 2010){
  time_trend <- df %>%
    filter(`Career stage` == 'Published Author' &
             year_complete_phd < 2010) %>%
    select_(.dots = c('ends_with("approximately_when_was")',
                      'Discipline',
                      '`Research type`', 'econ_subfield')) %>%
    mutate_at(vars(ends_with('_approximately_when_was')),
              funs(replace_na(., Inf))) %>%
    rename_all(.funs = str_remove_all, pattern = '_approximately_when_was') %>%
    rowwise() %>%
    mutate(!! combined_string_practice := min(c(publicly_posting,
                                                posting_study_instruments,
                                                analysis_pre_registration))) %>%
    ungroup %>%
    gather(key = "Practice", value = "Year", -Discipline, -`Research type`, -`econ_subfield`) %>%
    group_by(Practice, Discipline, econ_subfield, `Research type`, Year) %>%
    summarise(new_adopters = n()) %>%
    ungroup %>%
    complete(nesting(Practice), nesting(Discipline), nesting(`Research type`), nesting(econ_subfield), Year,
             fill = list(new_adopters = 0)) %>%
    group_by(Practice, Discipline, econ_subfield, `Research type`) %>%
    arrange(Year) %>%
    mutate(total_adopters = cumsum(new_adopters)) %>%
    ungroup


  time_trend <- bind_rows(
    time_trend,
    time_trend %>%
      group_by(Year, Practice, `Research type`, econ_subfield) %>%
      summarise_at(.vars = vars(contains('adopters')), 'sum') %>%
      mutate(Discipline = combined_string_discipline),
    time_trend %>%
      group_by(Year, Practice, Discipline, econ_subfield) %>%
      summarise_at(.vars = vars(contains('adopters')), 'sum') %>%
      mutate(`Research type` = combined_string_research_type),
    time_trend %>%
      group_by(Year, Practice, econ_subfield) %>%
      summarise_at(.vars = vars(contains('adopters')), 'sum') %>%
      mutate(Discipline = combined_string_discipline,
             `Research type` = combined_string_research_type)
  ) %>%
    group_by(Practice, Discipline, `Research type`, econ_subfield) %>%
    arrange(Year) %>%
    mutate(new_adopters_share = new_adopters / sum(new_adopters),
           total_adopters_share = cumsum(new_adopters_share)) %>%
    ungroup

  time_trend <- time_trend %>%
    mutate(Practice = factor(dplyr::recode(Practice,
                                           publicly_posting = "Posting data or code online",
                                           posting_study_instruments = "Posting study instruments online",
                                           analysis_pre_registration = "Pre-registering hypotheses or analyses"),
                             levels = practice_levels),
           thicker_line = (Practice == combined_string_practice),
           `Research type` = factor(`Research type`, levels = research_type_levels),
           Discipline = factor(Discipline, levels = discipline_levels),
           econ_subfield = factor(econ_subfield, levels = subfield_levels)
    ) %>%
    arrange(Discipline, econ_subfield, `Research type`, Practice, Year)
  return(time_trend)
}

### main time trend -----------------------------------
create_time_trend_df <- function(df, author_cutoff_year = 2010){ # main time trend; only Published Authors
  time_trend <- df %>%
    filter(`Career stage` == 'Published Author' &
             year_complete_phd < author_cutoff_year) %>%
    select_(.dots = c('ends_with("approximately_when_was")',
                      'Discipline',
                      '`Research type`')) %>%
    mutate_at(vars(ends_with('_approximately_when_was')),
              funs(replace_na(., Inf))) %>%
    rename_all(.funs = str_remove_all, pattern = '_approximately_when_was') %>%
    rowwise() %>%
    mutate(!! combined_string_practice := min(c(publicly_posting,
                                                posting_study_instruments,
                                                analysis_pre_registration))) %>%
    ungroup %>%
    gather(key = "Practice", value = "Year", -Discipline, -`Research type`) %>%
    group_by(Practice, Discipline, `Research type`, Year) %>%
    summarise(new_adopters = n()) %>%
    ungroup %>%
    complete(nesting(Practice), nesting(Discipline), nesting(`Research type`), Year,
             fill = list(new_adopters = 0)) %>%
    group_by(Practice, Discipline, `Research type`) %>%
    arrange(Year) %>%
    mutate(total_adopters = cumsum(new_adopters)) %>%
    ungroup

  time_trend <- bind_rows(
    time_trend,
    time_trend %>%
      group_by(Year, Practice, `Research type`) %>%
      summarise_at(.vars = vars(contains('adopters')), 'sum') %>%
      mutate(Discipline = combined_string_discipline),
    time_trend %>%
      group_by(Year, Practice, Discipline) %>%
      summarise_at(.vars = vars(contains('adopters')), 'sum') %>%
      mutate(`Research type` = combined_string_research_type),
    time_trend %>%
      group_by(Year, Practice) %>%
      summarise_at(.vars = vars(contains('adopters')), 'sum') %>%
      mutate(Discipline = combined_string_discipline,
             `Research type` = combined_string_research_type)
  ) %>%
    group_by(Practice, Discipline, `Research type`) %>%
    arrange(Year) %>%
    mutate(new_adopters_share = new_adopters / sum(new_adopters),
           total_adopters_share = cumsum(new_adopters_share)) %>%
    ungroup

  time_trend <- time_trend %>%
    mutate(Practice = factor(dplyr::recode(Practice,
                                           publicly_posting = "Posting data or code online",
                                           posting_study_instruments = "Posting study instruments online",
                                           analysis_pre_registration = "Pre-registering hypotheses or analyses"),
                             levels = practice_levels),
           thicker_line = (Practice == combined_string_practice),
           `Research type` = factor(`Research type`, levels = research_type_levels),
           Discipline = factor(Discipline, levels = discipline_levels)
    ) %>%
    arrange(Discipline, `Research type`, Practice, Year)
  return(time_trend)
}


time_trend <- create_time_trend_df(df, 2010)

### time trend for Econ overall -----------------------------------
economics_time_trend <- time_trend %>% # 92 rows
  filter(Discipline == "Economics",
         `Research type` == "All") %>%
  mutate(econ_subfield = "All")

### Time trend for econ subfields -----------------------------------
time_trend_subfield <- create_time_trend_df_sub(df, 2010) %>%
  filter(Discipline == 'Economics' &
           `Research type` == 'All') %>%
  filter(econ_subfield == 'Macro Economics' |
           econ_subfield  == 'Development Economics' |
           econ_subfield  == 'Theory' |
           econ_subfield  == 'Labor Economics') %>%
  bind_rows(economics_time_trend) %>%
  mutate(econ_subfield = case_when(
    econ_subfield == "Macro Economics" ~ "Macroeconomics",
    econ_subfield == "Theory" ~ "Economic Theory",
    TRUE ~ econ_subfield))


###  audit data -----------------------------------
audit_data <-
  audit_data %>%
  filter(audit) %>%
  rename(verified_registered_before=has_registered_before,
         verified_posted_data_before=has_posted_data_before) %>%
  mutate(verified_done_any_practice=
           (verified_registered_before|verified_posted_data_before)) %>%
  left_join(df %>%
              select(pid,
                     `Research type`,
                     has_done_analysis_pre_registration,
                     has_done_publicly_posting,
                     econ_subfield) %>%
              mutate(econ_subfield=ifelse(is.na(econ_subfield),
                                          'Others',
                                          econ_subfield),by='pid'))

audit_data <- audit_data %>%
  mutate(is_theorist=ifelse(completed_survey==TRUE,
                            econ_subfield=="Econometrics"|econ_subfield=="Theory",
                            Theory=="Theory"),
         is_macro_finance=replace_na(ifelse(completed_survey==TRUE,
                                            econ_subfield=="Macro Economics"|econ_subfield=="Finance",
                                            manual_subfield == "Finance"|manual_subfield == "Macroeconomics"), FALSE))


## Calibration Table -----------------------------------
calibration_table <- audit_data %>%
  select(completed_survey,
         verified_registered_before,
         verified_posted_data_before,
         verified_done_any_practice) %>%
  group_by(completed_survey) %>%
  summarise_all(mean) %>%
  mutate(var_name = ifelse(completed_survey,'V_R','V_N')) %>%
  select(-completed_survey) %>%
  gather('Practice','value',-var_name) %>%
  mutate(Practice=recode(Practice,verified_registered_before='Pre-registering hypotheses or analyses',
                         verified_posted_data_before='Posting data or code online',
                         verified_done_any_practice='Any')) %>%
  spread(var_name,value='value') %>%
  add_row(Practice='Posting study instruments online',V_N=NA,V_R=NA) %>%
  left_join(time_trend %>%
              filter(Year == 2017,
                     Discipline == 'Economics',
                     `Research type` == combined_string_research_type) %>%
              select(Practice, S_R = total_adopters_share))

V_N_post_intsrument = ((calibration_table%>%
                          filter(Practice=='Posting study instruments online')%>%pull(S_R))
                       *(calibration_table%>%filter(Practice=='Any')%>%pull(V_N))/
                         (calibration_table%>%filter(Practice=='Any')%>%pull(S_R)))
V_R_post_intsrument = ((calibration_table%>%
                          filter(Practice=='Posting study instruments online')%>%pull(S_R))
                       *(calibration_table%>%filter(Practice=='Any')%>%pull(V_R))/
                         (calibration_table%>%filter(Practice=='Any')%>%pull(S_R)))

calibration_table <- calibration_table %>%
  mutate(
    V_N = ifelse(Practice=='Posting study instruments online', V_N_post_intsrument, V_N),
    V_R = ifelse(Practice=='Posting study instruments online', V_R_post_intsrument, V_R),
    D_R = S_R-V_R,
    `$\\widehat{D_N}$`= V_N/V_R*D_R,
    `$\\widehat{S_N}$` = V_N + `$\\widehat{D_N}$`,
    scaling_factor = `$\\widehat{S_N}$`/ S_R,
    Practice = factor(Practice,levels = practice_levels))

## Perceived and actual opinion -----------------------------------
perceived_opinion <-
  bind_rows(
    df %>%
      group_by(`Career stage`, Discipline, has_done_publicly_posting, has_done_analysis_pre_registration) %>%
      summarise_at(vars(starts_with("publicly_posting_distribution_of_opinion_discipline"),
                        starts_with("analysis_pre_registration_distribution_of_opinion_discipline")),
                   sum, na.rm = TRUE),
    df %>%
      group_by( Discipline, has_done_publicly_posting, has_done_analysis_pre_registration) %>%
      summarise_at(vars(starts_with("publicly_posting_distribution_of_opinion_discipline"),
                        starts_with("analysis_pre_registration_distribution_of_opinion_discipline")),
                   sum, na.rm = TRUE)%>%
      mutate(`Career stage` = factor(combined_string_career_stage,levels=career_stage_levels)),
    df %>%
      group_by( `Career stage`, has_done_publicly_posting, has_done_analysis_pre_registration) %>%
      summarise_at(vars(starts_with("publicly_posting_distribution_of_opinion_discipline"),
                        starts_with("analysis_pre_registration_distribution_of_opinion_discipline")),
                   sum, na.rm = TRUE)%>%
      mutate(Discipline = factor(combined_string_discipline,levels=discipline_levels)),
    df %>%
      group_by(has_done_publicly_posting, has_done_analysis_pre_registration) %>%
      summarise_at(vars(starts_with("publicly_posting_distribution_of_opinion_discipline"),
                        starts_with("analysis_pre_registration_distribution_of_opinion_discipline")),
                   sum, na.rm = TRUE) %>%
      mutate(`Career stage` = factor(combined_string_career_stage,levels=career_stage_levels),
             Discipline = factor(combined_string_discipline,levels=discipline_levels)),
    df %>%
      group_by(`Career stage`) %>%
      summarise_at(vars(starts_with("publicly_posting_distribution_of_opinion_discipline"),
                        starts_with("analysis_pre_registration_distribution_of_opinion_discipline")),
                   sum, na.rm = TRUE) %>%
      mutate(Discipline = factor(combined_string_discipline,levels=discipline_levels))
  ) %>%
  gather(key, value = 'n', -`Career stage`, -Discipline, -has_done_publicly_posting, -has_done_analysis_pre_registration) %>%
  separate(key, c('Practice', 'Opinion level'),
           sep = '_distribution_of_opinion_discipline_') %>%
  mutate(Practice = dplyr::recode(Practice, publicly_posting = !!publicly_posting_recode_proper,
                                  analysis_pre_registration = !!analysis_pre_registration_recode_proper),
         `Opinion level` = factor(`Opinion level`, levels = opinion_levels,
                                  labels = names(opinion_levels),
                                  ordered = TRUE),
         `Has done it` = if_else(Practice == !!publicly_posting_recode_proper,
                                 has_done_publicly_posting,
                                 has_done_analysis_pre_registration)) %>%
  ungroup %>%
  select(-has_done_publicly_posting, -has_done_analysis_pre_registration) %>%
  group_by(Practice, `Career stage`, Discipline, `Opinion level`, `Has done it`) %>%
  summarise(n = sum(n)) %>%
  group_by(Practice, `Career stage`, Discipline, `Has done it`) %>%
  mutate(`Share` = n / sum(n),
         type = 'Perception') %>%
  ungroup

actual_opinion <- bind_rows(
  df %>%
    filter(!is.na(publicly_posting_what_is_your_opinion)) %>%
    group_by(`Career stage`,Discipline) %>%
    count(publicly_posting_what_is_your_opinion) %>%
    rename(`Opinion level` = publicly_posting_what_is_your_opinion) %>%
    mutate(Practice = !!publicly_posting_recode_proper),
  df %>%
    filter(!is.na(publicly_posting_what_is_your_opinion)) %>%
    group_by(Discipline) %>%
    count(publicly_posting_what_is_your_opinion) %>%
    rename(`Opinion level` = publicly_posting_what_is_your_opinion) %>%
    mutate(Practice = !!publicly_posting_recode_proper,
           `Career stage` = factor(combined_string_career_stage,levels=career_stage_levels)),
  df %>%
    filter(!is.na(analysis_pre_registration_what_is_your_opinion)) %>%
    group_by(`Career stage`, Discipline) %>% count(analysis_pre_registration_what_is_your_opinion) %>%
    rename(`Opinion level` = analysis_pre_registration_what_is_your_opinion) %>%
    mutate(Practice = !!analysis_pre_registration_recode_proper),
  df %>%
    filter(!is.na(analysis_pre_registration_what_is_your_opinion)) %>%
    group_by(Discipline) %>% count(analysis_pre_registration_what_is_your_opinion) %>%
    rename(`Opinion level` = analysis_pre_registration_what_is_your_opinion) %>%
    mutate(Practice = !!analysis_pre_registration_recode_proper,
           `Career stage` = factor(combined_string_career_stage,levels=career_stage_levels))
) %>% ungroup


actual_opinion <- bind_rows(
  actual_opinion,
  actual_opinion %>%
    group_by(Practice,`Career stage` ,`Opinion level`) %>%
    summarise(n = sum(n)) %>%
    mutate(Discipline = factor(combined_string_discipline,levels=discipline_levels))
) %>%
  group_by(Discipline, `Career stage`, Practice) %>%
  mutate(`Share` = n/sum(n),
         `Opinion level` = factor(`Opinion level`, levels = opinion_levels,
                                  label = names(opinion_levels),
                                  ordered = TRUE),
         type = 'Actual') %>%
  select(-n) %>%
  ungroup

## temp output -----------------------------------
write_rds(df, "temp/survey_wave_1_min.rds")
write_rds(audit_data, "temp/audit_data_min.rds")
write_rds(calibration_table, "temp/calibration_table.rds")
write_rds(perceived_opinion, "temp/perceived_opinion.rds")
write_rds(actual_opinion, "temp/actual_opinion.rds")
write_rds(time_trend_subfield, "temp/time_trend_subfield.rds") # for figures

## response rate (not run b/c uses email info) -----------------------------------
# target_sample <- read_csv('ext/wave_1_sample.csv', col_types = cols(.default = '?')) %>%
#   mutate(Discipline = factor(Discipline, levels=discipline_levels),
#          email_usca = str_detect(str_remove_all(email, '\\W*$'), regex_email_usca),
#          email_outside_usca = str_detect(str_remove_all(email, '\\W*$'), regex_email_outside_usca),
#          Region = factor(if_else(email_usca,
#                                  'USA and Canada',
#                                  'Outside USA and Canada'),
#                          levels = c("Outside USA and Canada","USA and Canada")))%>%
#   select('pid', 'Respondent_type', 'Discipline', 'Group_type','Region')
#
#
#
#
# usca_tld <- paste0('\\.', c('ca', 'com', 'edu', 'gov', 'org'))
# outside_usca_tld <- c(paste0('\\.', c('at', 'ar', 'au', 'be', 'br', 'cat', 'ch', 'cl', 'cm', 'cn', 'cy', 'cz', 'de', 'dk', 'es', 'eu', 'gr', 'fi', 'fr', 'hk', 'hu', 'ie', 'il', 'in', 'is', 'it', 'jp', 'kr', 'mx', 'my', 'nl', 'no', 'nz', 'pt', 'pk', 'pl', 'ru', 'sa', 'se', 'si', 'sk', 'sg', 'tr', 'tw', 'uk', 'za')),
#                       c('@ecb\\.int', '@gmx\\.net'))
# regex_email_usca <- regex(paste(paste0('(', usca_tld, '$)', collapse = '|')), ignore_case = TRUE)
# regex_email_outside_usca <- regex(paste(paste0('(', outside_usca_tld, '$)'), collapse = '|'), ignore_case = TRUE)
# target_sample <- read_csv('ext/wave_1_sample.csv', col_types = cols(.default = '?')) %>%
#   mutate(`Career stage` = if_else(wave_1_sample_strata_phd_student,
#                                   'PhD Student',
#                                   'Published Author'),
#          email_usca = str_detect(str_remove_all(email, '\\W*$'), regex_email_usca),
#          email_outside_usca = str_detect(str_remove_all(email, '\\W*$'), regex_email_outside_usca),
#          Region = if_else(email_usca,
#                           'USA & Canada',
#                           'Outside USA & Canada'))
#
# response_rate <- target_sample %>%
#   mutate(completed_survey = if_else(is.na(completed_survey), FALSE, completed_survey)) %>%
#   filter(Status_distribution_tracking != 'Email Bounced')
