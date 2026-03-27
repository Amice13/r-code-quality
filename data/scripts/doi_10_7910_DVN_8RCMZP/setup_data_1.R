## THIS CODE WILL NOT RUN
## This code compiles a dataset at the contract level with information about contract
## duration, extent competed, and number of bidders.

rm(list = ls())
library(dplyr)
library(stats)
library(spatstat)
library(DBI)
library(data.table)
library(binsreg)
library(ggplot2)

outdir <- '../output/'

## Connect to database (USER MUST SET UP OWN DATABASE) 
con <- DBI::dbConnect(RPostgres::Postgres(),dbname='#####',
                 host='localhost',port=####,
                 user='postgres',password='####')

## Pull base dataset (master_clean) from SQL
sql.suffix <- '_20220701'
master_clean   <- tbl(con, paste0('clean',sql.suffix))

## Merge in additional variables
awarding_agency_code <- tbl(con, 'awarding_agency_code') ## Awarding agency identifier
extent_competed_code <- tbl(con, 'extent_competed_code') ## Extent competed code

master_clean <- left_join(master_clean, awarding_agency_code) 
master_clean <- left_join(master_clean, extent_competed_code)
extent_competed_code <- data.frame(extent_competed_code)

## Keep only variables of interest 
data <- master_clean %>% select(
                             contract_award_unique_key_CODE,
                             year,
                             month,
                             action_date,
                             naics2, 
                             period_of_performance_start_date,
                             period_of_performance_current_end_date,
                             naics_code,
                             awarding_agency_code,
                             action_date,
                             extent_competed_code,
                             extent_competed,
                             number_of_offers_received,
                             federal_action_obligation,
                             recipient_parent_name_CODE
                         )

data <- data %>% group_by(contract_award_unique_key_CODE, naics2) %>% mutate(
                                                                          min_date              = min(action_date, na.rm=TRUE),
                                                                          start_date            = min(period_of_performance_start_date,na.rm=TRUE),
                                                                          end_date              = max(period_of_performance_current_end_date,na.rm=TRUE),
                                                                          contract_obligations  = sum(federal_action_obligation, na.rm=TRUE))


data    <- data  %>%   mutate(duration = as.numeric(end_date - start_date) + 1)
data    <- data  %>%   mutate(durationInRange = ifelse((duration < 1 | duration > 7301), yes = 0, no = 1))

data    <- data  %>% filter(action_date == min_date) %>% select(contract_award_unique_key_CODE,
                                                                year,
                                                                month,
                                                                action_date,
                                                                min_date, 
                                                                duration,
                                                                durationInRange,
                                                                extent_competed_code,
                                                                extent_competed,
                                                                number_of_offers_received,
                                                                contract_obligations)

data    <- data  %>% filter(durationInRange == 1)
data    <- data.frame(data)
data    <- unique(data)


## Drop contracts with multiple extent_competed codes (<.0001 percent of observations).
data    <- data  %>%   group_by(contract_award_unique_key_CODE) %>% mutate(count = n()) %>% filter(count == 1) 
data    <- data  %>%   filter(durationInRange == 1 & contract_obligations > 0)

## Final cleaning 
data$min_date <- NULL 
data$count    <- NULL
data$durationInRange <- NULL
data$number_of_offers_received <- as.numeric(as.character(data$number_of_offers_received))

## Save intermediate file
save(data, file = '../../data/intermediate_file_1.RData')




                              




