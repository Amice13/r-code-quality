# Open packages 

source('scripts/cleaning_and_management/libraries.R') 

# Code for split violin plots and adjusted labels

source('scripts/cleaning_and_management/split_violin_and_labels.R') 

# Load data 

# Delphi survey
expanel <- readRDS("data/Delphi survey/expanel.rds") 


#######################################
#### Table 1: Delphi survey sample ####

first_wave <- expanel %>%
  ungroup() %>%
  select(id,panel,stakeholder,contains("_stake"),contains("_aca"),contains("_exp"),contains("regexp"),years_cat) %>%
  unique() %>%
  select(years_cat,stakeholder,contains("_aca"),contains("_exp"),contains("regexp")) %>%
  frq() %>%
  bind_rows() %>%
  filter(val!="No") %>% 
  select(val, frq, valid.prc)

second_wave <- expanel %>%
  ungroup() %>%
  select(id,panel,stakeholder,contains("_stake"),contains("_aca"),contains("_exp"),contains("regexp"),years_cat) %>%
  unique() %>%
  select(panel,years_cat,stakeholder,contains("_aca"),contains("_exp"),contains("regexp")) %>%
  filter(panel==1) %>%
  select(-panel) %>%
  frq() %>%
  bind_rows() %>%
  filter(val!="No") %>% 
  select(val, frq, valid.prc)

# analised sample
analised <- expanel %>%
  ungroup() %>%
  select(id,panel,stakeholder,contains("_stake"),contains("_aca"),contains("_exp"),contains("regexp"),years_cat) %>%
  unique() %>%
  select(panel,years_cat,stakeholder,contains("_aca"),contains("_exp"),contains("regexp")) %>%
  filter(panel==1, years_cat %!in% young, europe_regexp=="Europe") %>%
  select(-panel) %>%
  frq() %>%
  bind_rows() %>%
  filter(val!="No") %>% 
  select(val, frq, valid.prc)

# put them together and print in excel
list_of_datasets <- list("first" = first_wave, "second" = second_wave, "analised" = analised)

xlsx::write.xlsx(list_of_datasets,
                 file="output/tables/Table1.xlsx",
                 append=F,
                 row.names=FALSE)
