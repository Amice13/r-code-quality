rm(list=ls())
library(data.table)
library(stats)
library(foreign)
library(dplyr)

datadir <- '../data/'
outdir <- '../output/'

## Read in contracts data from 'intermediate_file_3.RData'
load(paste0(datadir, 'intermediate_file_3.RData'))
data <- data %>% filter(year < 2022)

## Only keep observations assigned to a 6-digit NAICS code (not 4 or 2)
naics6 <- data %>% mutate(digits = nchar(naics_code)) %>% filter(digits == 6)

## Collapse by naics code (total, defense, and non-defense) 
naics6 <- naics6 %>% group_by(naics_code) %>% summarise(
                                                  total_contracts  = sum(sector_obligations, na.rm=TRUE),
                                                  defense_contracts = sum(defense_obligations, na.rm=TRUE),
                                                  nondef_contracts  = sum(nondef_obligations,na.rm=TRUE)
                                                  )

## Calculate sector share
naics6 <- naics6 %>% mutate(sector_share  = total_contracts/sum(total_contracts),
                            defense_share = defense_contracts/sum(defense_contracts),
                            nondef_share  = nondef_contracts/sum(nondef_contracts))
naics6$naics_code <- as.character(naics6$naics_code)


## Load IO x NAICS crosswalk (from the BEA IO tables) and merge
xw <- fread(file = paste0(datadir, 'IOtoNAICSxw.csv'))
colnames(xw) <- c("io_code", "naics_code")
naics6 <- left_join(naics6, xw)

## Keep only those that merged at the 6-digit level
merged <- naics6 %>% filter(!is.na(io_code))

## Check to see if anything merges at the 5-digit naics 
not.merged <- naics6 %>% filter(is.na(io_code))
not.merged <- not.merged %>% mutate(naics5 = substr(naics_code, 1,5))
not.merged$io_code <- NULL 
colnames(xw) <- c('io_code', 'naics5')
not.merged <- left_join(not.merged, xw)

merged2 <- not.merged %>% filter(!is.na(io_code))

## Check to see if anything merges at the 4-digit level
not.merged         <- not.merged %>% filter(is.na(io_code))
not.merged         <- not.merged %>% mutate(naics4 = substr(naics_code,1,4))
not.merged$io_code <- NULL
not.merged$naics5  <- NULL 
colnames(xw)       <- c('io_code', 'naics4')
not.merged         <- left_join(not.merged, xw)

merged3 <- not.merged %>% filter(!is.na(io_code))

## Read in io table data and descriptions
## THESE FILES COME FROM Pasten, Schoenle, Weber (2023)
## See Appendix A.4 of that paper for details on construction 
value.added     <- fread(file = paste0(datadir, 'value_added_2002.csv'))
gdp.k           <- fread(file = paste0(datadir, '2002CkDetailed_agg_6_8_nx.csv'))
industries.long <- fread(file = paste0(datadir, '2002_IndustriesIndex_agg_6_8_nx.csv'))
industries.agg  <- fread(file = paste0(datadir, 'agg_6_8_nx.csv'))

colnames(industries.long) <- c("row","io_code","description")
industries.long           <- industries.long %>% filter(row < 416)

gdp.k           <- cbind(industries.long, gdp.k)
gdp.k           <- gdp.k %>% select(io_code, V1)
colnames(gdp.k) <- c("io_code","gdp_k")
gdp.k           <- gdp.k %>% mutate(gdp_shares = gdp_k/sum(gdp_k))

merged  <- left_join(merged, gdp.k)
merged2 <- left_join(merged2, gdp.k)
merged3 <- left_join(merged3, gdp.k)

merged <- merged %>% group_by(io_code) %>% summarise(
                                               sector_share  = sum(sector_share, na.rm=TRUE),
                                               defense_share = sum(defense_share, na.rm=TRUE),
                                               nondef_share  = sum(nondef_share, na.rm=TRUE),
                                               gdp_shares    = mean(gdp_shares, na.rm=TRUE))

merged2 <- merged2 %>% group_by(io_code) %>% summarise(
                                               sector_share  = sum(sector_share, na.rm=TRUE),
                                               defense_share = sum(defense_share, na.rm=TRUE),
                                               nondef_share  = sum(nondef_share, na.rm=TRUE),
                                               gdp_shares    = mean(gdp_shares, na.rm=TRUE))


merged3 <- merged3 %>% group_by(io_code) %>% summarise(
                                               sector_share  = sum(sector_share, na.rm=TRUE),
                                               defense_share = sum(defense_share, na.rm=TRUE),
                                               nondef_share  = sum(nondef_share, na.rm=TRUE),
                                               gdp_shares    = mean(gdp_shares, na.rm=TRUE))



merged <- rbind(merged, merged2)
merged <- rbind(merged, merged3) 


merged <- data.frame(merged)
merged <- merged %>% arrange(-sector_share)


## MAKE PLOT (Figure 3a) 
pdf(paste0(outdir,'consumption_shares_LOGS.pdf'), height = 5,  width = 5)
plot(log(merged$gdp_shares), log(merged$defense_share), pch = 8,
     col = 'dodgerblue4', 
     xlab = 'GDP Shares (Logs)', ylab = 'Federal Purchases Shares (Logs)',
     cex.lab = 1.0, ylim = c(-17,-1), xlim = c(-15,-1))
abline(0,1, col = 'black', lty = 2)
par(new = TRUE)
plot(log(merged$gdp_shares), log(merged$nondef_share), pch = 17, col = 'green3', xlab = '', ylab = '',ylim = c(-17,-1), xlim = c(-15,-1))
legend('topleft', legend = c('Federal Defense', 'Federal Non-Defense'), col = c('dodgerblue4','green3'), pch = c(8,17), bty = 'n')
dev.off()

#####################################################################
## Create table of two-digit gdp.k shares
## These are inputs to the bottom section of Table 4
industries.agg <- industries.agg %>% mutate(naics2 = substr(io_agg, 4,5))
industries.agg$io_agg <- NULL
gdpk.2digit <- left_join(gdp.k, industries.agg)
gdpk.2digit <- gdpk.2digit %>% group_by(naics2) %>% summarise(gdp_k = sum(gdp_k))
gdpk.2digit <- data.frame(gdpk.2digit)
gdpk.2digit <- gdpk.2digit %>% mutate(gdpk_share = gdp_k/sum(gdp_k))
gdpk.2digit$gdpk_share <- round(gdpk.2digit$gdpk_share,4)

data   <- data   %>% mutate(naics2 = substr(naics_code,1,2))
naics2 <- data   %>% group_by(naics2) %>% summarise(total_contracts = sum(sector_obligations))
naics2 <- naics2 %>% mutate(sector_share = total_contracts/sum(total_contracts))

table4.2digit <- left_join(naics2, gdpk.2digit)
table4.2digit <- table4.2digit %>% arrange(-sector_share)

## The entries for the 2-digit (bottom) panel of Table 4:
table4.2digit <- head(table4.2digit,3)

## The entries for the 6-digit (middle) panel of Table 4:
table4.6digit <- head(merged, 3) 

#####################################################################






