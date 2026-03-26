rm(list=ls())

#Set working directory to the Replications folder



#Load Packages
library(tidyverse)
library(readxl)
library(stringr)
library(writexl)
library(haven)


#######################################

d <- read_excel('Data/Processed_Data/Docket_Information_1997_2024.xlsx')

d <- d %>%
  mutate(n=nchar(lower_ct),
         m = nchar(lower_docket)) %>%
  rowwise() %>%
  group_by(docket) %>%
  filter(n==min(n), m==min(m)) %>%
  ungroup()  %>% filter(case_name!='NA') %>% select(-c('m','n')) %>% unique() 

cda <- read_excel('Data/Processed_Data/Categorized_Docket_Actions.xlsx')


merged_sc_data <- cda %>%
  mutate(docket_number = str_replace(docket_number,'–','-')) %>%
  left_join(d, by = c('docket_number'='docket')) %>%
  mutate(lower_docket=ifelse(str_detect(lower_docket, '\\d'),lower_docket,NA),
         lower_ct=ifelse(str_detect(lower_ct,'[a-z]'),lower_ct,NA),
         lower_ct = str_split_i(lower_ct,'    ',1)) %>% select(-case_name) %>%
  rename('gov_petitioner' = 'sg_petitioner',
         'gov_respondent' = 'sg_respondent')


stata_version <- mutate(merged_sc_data, across(where(is.character), ~replace_na(.x,"")))

write_xlsx(merged_sc_data,'Data/shadow_docket_database_v2-0.xlsx')
write_csv(merged_sc_data,'Data/shadow_docket_database_v2-0.csv')
write_dta(stata_version, "Data/shadow_docket_database_v2-0.dta")





