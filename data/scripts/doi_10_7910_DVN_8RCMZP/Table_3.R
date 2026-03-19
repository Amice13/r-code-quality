rm(list = ls())
library(dplyr)
library(stats)
library(spatstat)
library(data.table)

datadir <- '../data/'
outdir  <- '../output/'

source('wtd.stats.s')

## Load data (intermediate_file_1). 
load(file = paste0(datadir,'intermediate_file_1.RData'))
data <- data.frame(data)

## Summary Statistics
contracts_sample <- data %>% filter(number_of_offers_received != 999 & number_of_offers_received != 0)
contracts_sample <- contracts_sample %>% filter(year < 2022)

contracts_sample <- contracts_sample %>% mutate(comp_type = "")

contracts_sample$comp_type[contracts_sample$extent_competed == "COMPETED UNDER SAP" | contracts_sample$extent_competed == "COMPETITIVE DELIVERY ORDER" | contracts_sample$extent_competed == "FULL AND OPEN COMPETITION"] <- "Competed" 

contracts_sample$comp_type[contracts_sample$extent_competed == "FULL AND OPEN COMPETITION AFTER EXCLUSION OF SOURCES" | contracts_sample$extent_competed == "FOLLOW ON TO COMPETED ACTION"] <- "Partially Competed" 

contracts_sample$comp_type[contracts_sample$extent_competed == "NON-COMPETITIVE DELIVERY ORDER" | contracts_sample$extent_competed == "NOT AVAILABLE FOR COMPETITION" | contracts_sample$extent_competed == "NOT COMPETED" | contracts_sample$extent_competed == "NOT COMPETED UNDER SAP"] <- "Not Competed" 


contracts_sample <- contracts_sample %>% group_by(comp_type) %>% mutate(contract_weight = contract_obligations/sum(contract_obligations))



extent_competed  <- contracts_sample %>% group_by(comp_type) %>% summarise(
                                                                           count         = n(),
                                                                           total_value   = sum(contract_obligations, na.rm=TRUE),
                                                                           mean_offers   = round(mean(number_of_offers_received),1),
                                                                           median_offers = round(median(number_of_offers_received),1),
                                                                           wtd.mean      = round(sum(contract_weight*number_of_offers_received),1),
                                                                           wtd.median    = round(wtd.quantile(number_of_offers_received, weights = contract_obligations, probs = 0.5),1))

extent_competed <- data.frame(extent_competed)



extent_competed <- extent_competed %>% filter(extent_competed != "") %>% mutate(share_count = round(count/sum(count),2),
                                                                                share_value = round(total_value/sum(total_value),2))

## Format table
extent_competed <- extent_competed %>% mutate(order = c(1,3,2)) %>% arrange(order) 
extent_competed$comp_type[extent_competed$comp_type == "Competed"] <- "Fully Competed" 

summary_stats   <- data.frame(
    extent_competed    = extent_competed$comp_type,
    share_of_contracts = paste0(extent_competed$share_count, ' (', extent_competed$share_value, ')'),
    mean_offers        = paste0(extent_competed$mean_offers, ' (', extent_competed$wtd.mean, ')'),
    median_offers      = paste0(extent_competed$median_offers, ' (', extent_competed$wtd.median, ')')
    )

write.table(summary_stats, paste0(outdir,'competition_bidding_stats.tex'), sep = '&', eol = '\\\\ \n', row.names = FALSE,
            col.names = FALSE, quote = FALSE)



