## THIS FILE WILL NOT RUN WITHOUT SETTING UP OWN SQL DATABASE
## NOTE: This file takes the raw data and creates a panel dataset with
## the variables: year, naics code (NAICS 6), and total contract obligations,
## defense contract obligations, and non-defense contract obligations 
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

## Group by firm and year to get total obligations for each firm in each year. 
data <- master_clean  %>% group_by(naics_code, year) %>% summarise(
                                                             sector_obligations = sum(federal_action_obligation)
                                                         )


defense <- master_clean %>% filter(awarding_agency_code == 97)
defense <- defense      %>% group_by(naics_code, year) %>% summarise(
                                                               defense_obligations = sum(federal_action_obligation))


nondef  <- master_clean %>% filter(awarding_agency_code != 97)

nondef <- nondef      %>% group_by(naics_code, year) %>% summarise(
                                                               nondef_obligations = sum(federal_action_obligation))

data      <- data.frame(data)
defense   <- data.frame(defense)
nondef    <- data.frame(nondef)


data <- left_join(data, defense)
data <- left_join(data, nondef)


save(data, file = paste0(datadir, 'intermediate_file_3.RData'))









