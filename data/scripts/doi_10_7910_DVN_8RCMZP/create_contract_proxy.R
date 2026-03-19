########################################################
## This script pulls data from the NIPAs to compare to
## the USASpending contracts data. The resulting files
## are used to create Figure 1 and Table 1. 
########################################################
rm(list=ls())
library(dplyr)
library(data.table)
library(data.table)
library(foreign)
library(xlsx)

## Preliminaries
source('getFred.R')
datadir <- "../data/"
outdir <-  "../output/"

## Pull in list of FRED Codes to download
fred.codes <- fread(file = paste0(datadir,'fred_codes.csv'), header = TRUE)
N <- c(seq(1,nrow(fred.codes)))

## Pull data from FRED 
for (n in N) {
    series.code <- fred.codes$fred_code[n]
    series.name <- fred.codes$var_name[n]
    temp.data   <- getFRED(paste(series.code), dir = paste0(datadir))
    colnames(temp.data) <- c("date",series.name)
    temp.data <- data.frame(temp.data)

    if (n == 1) {
        fred.data <- temp.data
    }

    if (n != 1) {
        fred.data <- left_join(fred.data, temp.data)
    }

}

fred.data <- fred.data %>% mutate(fyear = year(date))
fred.data$fyear[month(fred.data$date) > 8] <- fred.data$fyear[month(fred.data$date)>8] + 1
fred.data <- fred.data %>% mutate(year = year(date),
                                  quarter = quarter(date))



## Create proxy variable 
proxy <- data.frame(
    quarter             = fred.data$year + (fred.data$quarter-1)/4, 
    gce_gi              = (fred.data$gce_gi), 
    def_contract_proxy  = (fred.data$int_def + fred.data$investment_def - fred.data$ggi_def),
    non_contract_proxy  = (fred.data$int_nondef + fred.data$investment_nondef - fred.data$ggi_nondef),
    wages               = fred.data$comp
)

proxy <- proxy %>% mutate(
                       contract_proxy = def_contract_proxy + non_contract_proxy,
                       non_wage       = gce_gi - wages - (def_contract_proxy + non_contract_proxy)
                       )

proxy <- proxy %>% select(def_contract_proxy, non_contract_proxy, wages, contract_proxy, non_wage, quarter)

## Save stata file 
write.dta(proxy, file = paste0(datadir, 'contracts_for_ramey_merge.dta'))



