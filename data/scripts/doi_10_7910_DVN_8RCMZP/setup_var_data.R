## THIS FILE WILL NOT RUN WITHOUT SETTING UP SQL DATABASE
## This file creates the dataset (monthly x naics 2 sector) used in
## the VAR.
rm(list = ls())
library(dplyr)
library(stats)
library(DBI)
library(data.table)

## ---- PRELIMINARIES ---- ##
## Set directories
datadir <- '../data/'

## --------- SQL ---------- ##
## Connect to SQL database
con <- DBI::dbConnect(RPostgres::Postgres(),dbname='contracts',
                 host='localhost',port=####,
                 user='postgres',password='####')

## Pull main dataset from SQL
sql.suffix <- '_20220701'
master_clean   <- tbl(con, paste0('clean',sql.suffix))
master_clean$naics_code <- as.character(master_clean$naics_code)

## Collapse by month, naics2 
raw <- master_clean %>% group_by(year, month, naics2) %>% summarise(raw_obligations = sum(federal_action_obligation))
raw <- raw %>% mutate(date = as.Date(paste0(year,'-',month,'-01')))
raw <- data.frame(raw)


## Save file
write.dta(raw, file = 'contract_data_var.dta')
