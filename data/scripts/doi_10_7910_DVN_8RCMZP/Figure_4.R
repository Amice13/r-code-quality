## This file creates Figure 4 (a and b) using "intermediate_file_3.RData"
## which is a panel of federal contracts by NAICS sector and year.
rm(list = ls())
library(dplyr)
library(data.table)
library(readstata13)

## Preliminaries
datadir <- '../data/'
outdir <- '../output/'

line.col <- 'mediumblue'
line.col.2 <- 'lightskyblue'
line.col.3  <- 'navyblue'
ybounds = c(100,300)

## Load data (intermediate file 3)
load(file = paste0(datadir, 'intermediate_file_3.RData'))
data <- data %>% filter(year < 2022) 

## Create NAICS2 code
data <- data %>% mutate(naics2 = substr(naics_code,1,2),
                        digits = nchar(naics_code))

## Load Frequency of price adjustment data. These files are aggregated from
## the confidential BLS PPI microdata.

## NAICS 2-digit sectors
theta   <- read.dta13(paste0(datadir,'freq2d.dta'))
theta6d <- fread(file = paste0('../data/FrequencyNaics6dnew.csv'))

## NAICS 6-digit sectors
colnames(theta)    <- c("naics2", "freq_id")
theta$naics2       <- as.character(theta$naics2)
colnames(theta6d)  <- c("naics_code", "freq_id") 

## NAICS 2 Contract Shares
naics2 <- data %>% group_by(naics2) %>% summarise(
                                            sector_obligations = sum(sector_obligations, na.rm=TRUE)
                                            )

naics2 <- naics2 %>% mutate(sector_share = sector_obligations/sum(sector_obligations, na.rm=TRUE))
naics2 <- naics2 %>% filter(!is.na(naics2))
naics2 <- left_join(naics2, theta)


## NAICS 6 Contract Shares
naics6 <- data    %>% filter(digits == 6) ## Keep only if coded as a 6-digit sector (sometimes entered as 2 or 4-digit) 
naics6 <- naics6  %>% group_by(naics_code) %>% summarise(
                                                   sector_obligations = sum(sector_obligations, na.rm=TRUE)
                                                   )

naics6 <- naics6  %>% mutate(sector_share = sector_obligations/sum(sector_obligations))
naics6 <- left_join(naics6, theta6d)
naics6 <- naics6 %>% filter(sector_share > 0)


## Make Plots
vec <- naics2$sector_share
inch.size <- 0.4

vec6d <- naics6$sector_share


## 2-digit version 
pdf(paste0(outdir,'calvo_graph_bls_ALL.pdf'), height = 5, width = 5)
symbols(naics2$sector_share, naics2$freq_id, circles = vec,
        inches = inch.size, fg = line.col, bg = line.col.2, lwd = 2, xlab = 'Share of Spending',
        ylab = 'Frequency of Price Changes')
dev.off()

pdf(paste0(outdir,'calvo_graph_bls_6d_ALL.pdf'), height = 5, width = 5)
symbols(naics6$sector_share, naics6$freq_id, circles = vec6d,
        inches = inch.size, fg = line.col, bg = line.col.2, lwd = 2, xlab = 'Share of Spending',
        ylab = 'Frequency of Price Changes')
dev.off()

