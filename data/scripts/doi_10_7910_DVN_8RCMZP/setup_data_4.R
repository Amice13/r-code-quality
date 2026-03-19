## THIS FILE WILL NOT RUN WITHOUT SETTING UP SQL DATABASE 
## NOTE: This file takes the raw data and creates a panel dataset with
## three variables: year, naics code (NAICS 6), and sector contract obligations
rm(list = ls())
library(dplyr)
library(stats)
library(spatstat)
library(DBI)
library(data.table)
library(binsreg)
library(ggplot2)
library(foreign) 

## Preliminaries 
datadir <- '../../data/'

## Connect to SQL database
con <- DBI::dbConnect(RPostgres::Postgres(),dbname='contracts',
                 host='localhost',port=####,
                 user='postgres',password='####')


## Pull main dataset from SQL
sql.suffix <- '_20220701'
master_clean   <- tbl(con, paste0('clean',sql.suffix))

## Merge in recipient parent duns codes
recipient_parent_duns_CODE <- tbl(con, paste0('recipient_parent_duns_CODE', sql.suffix))
master_clean <- left_join(master_clean, recipient_parent_duns_CODE)

## Create quarter variable
master_clean   <- master_clean %>% mutate(quarter = quarter(action_date))

##########################################################################
################### Firm-Level Dataset ###################################
##########################################################################
firms <- master_clean %>% select(recipient_parent_duns, year, quarter, federal_action_obligation)

## DUNS numbers sometimes drop leading 00s. Fix this.
firms <- firms %>% mutate(digits = nchar(recipient_parent_duns))
firms <- firms %>% filter(!is.na(recipient_parent_duns))

firms <- data.frame(firms)

firms$recipient_parent_duns[firms$digits == 1] <- paste0('00000000',firms$recipient_parent_duns[firms$digits == 1])
firms$recipient_parent_duns[firms$digits == 7] <- paste0('00',firms$recipient_parent_duns[firms$digits == 7])
firms$recipient_parent_duns[firms$digits == 8] <- paste0('0',firms$recipient_parent_duns[firms$digits == 8])



firms <- firms        %>% group_by(recipient_parent_duns, year, quarter) %>% summarise(
                                                                                 firm_quarter_obligations = sum(federal_action_obligation)
                                                                             )


firms <- firms %>% filter(year > 2000) 


##########################################################################
################### Sector-Level Dataset #################################
##########################################################################
sectors <- master_clean %>% select(naics_code, year, quarter, federal_action_obligation)
sectors <- sectors      %>% group_by(naics_code, year, quarter) %>% summarise(
                                                                        sector_quarter_obligations = sum(federal_action_obligation)
                                                                        )

sectors <- data.frame(sectors)

## Keep only six-digit sectors and sectors for which lifetime contracts are more than 1000
sectors <- sectors %>% mutate(digits = nchar(naics_code)) %>% filter(digits == 6)
sectors <- sectors %>% group_by(naics_code) %>% mutate(total_contracts = sum(sector_quarter_obligations))
sectors <- sectors %>% filter(total_contracts >= 1000)

sectors$digits <- NULL
sectors$total_contracts <- NULL

## Save as stata file 
write.dta(firms, file = '../../data/firms_unbalanced.dta')
write.dta(sectors, file = '../../data/sectors_unbalanced.dta')




