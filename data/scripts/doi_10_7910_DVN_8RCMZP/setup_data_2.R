## THIS FILE WILL NOT RUN WITHOUT SETTING UP SQL DATABASE
## NOTE: This file takes the raw data and creates a panel dataset with
## three variables: year, recipient_parent_duns, and contract obligations. 
rm(list = ls())
library(dplyr)
library(stats)
library(spatstat)
library(DBI)
library(data.table)
library(binsreg)
library(ggplot2)

## Preliminaries 
datadir <- '../../data/'

## Connect to SQL database
con <- DBI::dbConnect(RPostgres::Postgres(),dbname='contracts',
                 host='localhost',port=####,
                 user='postgres',password='####')


## Pull main dataset from SQL 
sql.suffix <- '_20220701'
master_clean   <- tbl(con, paste0('clean',sql.suffix))

## Merge in contract award and recipient parent name identifiers. 
recipient_parent_duns_CODE <- tbl(con, paste0('recipient_parent_duns_CODE', sql.suffix))
recipient_parent_name_CODE <- tbl(con, paste0('recipient_parent_name_CODE', sql.suffix))

master_clean <- left_join(master_clean, recipient_parent_name_CODE)
master_clean <- left_join(master_clean, recipient_parent_duns_CODE) 

## Group by firm and year to get total obligations for each firm in each year. 
data <- master_clean  %>% group_by(recipient_parent_duns, year) %>% summarise(
                                                                        firm_obligations = sum(federal_action_obligation, na.rm=TRUE)
                                                                    )

data <- data.frame(data)



## DUNS numbers sometimes drop leading 00s. Fix this.
data <- data %>% mutate(digits = nchar(recipient_parent_duns))
data <- data %>% filter(!is.na(recipient_parent_duns))

data$recipient_parent_duns[data$digits == 1] <- paste0('00000000',data$recipient_parent_duns[data$digits == 1])
data$recipient_parent_duns[data$digits == 7] <- paste0('00',data$recipient_parent_duns[data$digits == 7])
data$recipient_parent_duns[data$digits == 8] <- paste0('0',data$recipient_parent_duns[data$digits == 8])

## Save file 
save(data, file = paste0(datadir, 'intermediate_file_2.RData'))



