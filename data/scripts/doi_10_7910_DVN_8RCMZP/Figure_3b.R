## This file creates Figure 3(b) using "intermediate file 2"
rm(list = ls())
library(dplyr)
library(data.table)

## Preliminaries
datadir <- '../data/'
outdir <- '../output/'

## Load data (intermediate file 2), which is contracts by firm x year
load(file = paste0(datadir, 'intermediate_file_2.RData'))
data <- data %>% filter(year > 2000 & year < 2022)

## Calculate firm share of total contracts
govt_firms <- data %>% group_by(recipient_parent_duns) %>% summarise(
                                                               firm_contracts = sum(firm_obligations, na.rm=TRUE)
                                                               )

govt_firms <- govt_firms %>% mutate(firm_share = firm_contracts/sum(firm_contracts, na.rm=TRUE))

## Load compustat crosswalk and change variable names
crosswalk <- fread(file = '../data/compustat_crosswalk.csv')
colnames(crosswalk) <- c("gvkey", "recipient_parent_duns")
crosswalk$recipient_parent_duns <- as.character(crosswalk$recipient_parent_duns)
crosswalk$gvkey <- as.character(crosswalk$gvkey)

govt_firms <- left_join(govt_firms, crosswalk)
matched    <- govt_firms %>% filter(!is.na(gvkey))


## Load Compustat Firm Sales
compustat <- fread(file = '../data/compustat_firm_sales.csv')
compustat <- compustat %>% select(fyear, gvkey, sale, conml)
names(compustat)[names(compustat)=='fyear'] <- 'year'
compustat <- compustat %>% filter(!is.na(sale))

## Merge in Total Business Sales (FRED mnemonic TOTBUSSMSA) 
total_bus <- fread(file = "../data/total_business_sales.csv")
total_bus <- total_bus %>% mutate(year = year(DATE))
compustat <- left_join(compustat, total_bus)

total_bus <- total_bus %>% filter(year > 2000 & year < 2022)

compustat$DATE <- NULL
names(compustat)[names(compustat) == 'TOTBUSSMSA'] <- 'total_sales'
compustat$total_sales <- as.numeric(compustat$total_sales)

compustat <- compustat %>% group_by(gvkey, conml) %>% summarise(
                                                          firm_sales  = sum(sale, na.rm=TRUE)
                                                      )


compustat <- compustat %>% mutate(sales_share = firm_sales/sum(as.numeric(total_bus$TOTBUSSMSA)))


## Merge compustat and matched government firms. 
compustat$gvkey <- as.character(compustat$gvkey)
compustat       <- left_join(compustat, matched)
compustat <- compustat %>% filter(!is.na(recipient_parent_duns))

compustat <- data.frame(compustat)
compustat <- compustat %>% arrange(-firm_share)

## Top Panel of Table 4
table4.firms <- head(compustat, 3) %>% select(conml, firm_share, sales_share)


## Make plot (Figure 3b) 
pdf(paste0(outdir,'compustat_govt_firm_compare.pdf'), height = 5, width = 5)
plot(log(compustat$sales_share), log(compustat$firm_share), pch = 20, col = 'royalblue',
     xlab = 'Total Business Sales Shares (Logs)', ylab = 'Federal Purchases Shares (Logs)', ylim = c(-23,0),
     xlim = c(-23,0))
abline(0,1, lty = 5)
dev.off()

