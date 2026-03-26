rm(list = ls())
library(dplyr)
library(stats)
library(spatstat)
library(data.table)

## Preliminaries 
datadir <- '../data/'
outdir  <- '../output/'

line.col1 <- 'black'
line.col2 <- 'lightskyblue'

## Load intermediate file, which has (recipient parent) firm contract obligations by year 
load(file = paste0(datadir,'intermediate_file_2.RData'))

## Keep sample year from 2001-2021 
data  <- data %>% filter(year < 2022)

## Keep firms only in years in which they have positive obligations with the government
data  <- data %>% filter(firm_obligations >  0)

## Collapse data to get total firm obligations and the number of years that the
## firm is in the sample (aka "firm tenure" in the data).
firms <- data %>% group_by(recipient_parent_duns) %>% summarise(
                                                          firm_obligations = sum(firm_obligations),
                                                          n_years          = n()
                                                      )

## Minor cleaning 
firms$n_years <- as.numeric(as.character(firms$n_years))
firms <- firms %>% filter(!is.na(recipient_parent_duns))

##### Generate ECDFs of firm tenure, unweighted and weighted by firm obligations 
ecdf      <- ecdf(firms$n_years)
ecdf.wtd  <- ewcdf(firms$n_years, weights = firms$firm_obligations)

## Make Plot 
pdf(paste0(outdir,'pooled_firm_tenure_ecdfs.pdf'), height = 5, width = 5)
plot(ecdf, lwd = 3, col = line.col1, do.points = FALSE,
     main = '', xlab = 'Firm Duration (Years)', ylab = 'Cumulative Share',
     lty = 1, verticals = TRUE, col.01line = 'gray1', bty = 'l')
lines(ecdf.wtd, lwd = 3, col = line.col2, col.01line = 'gray1', do.points = FALSE, verticals = TRUE)
abline(v = 365, lwd = 0.7, col = 'gray1', lty = 5)
abline(v = 365*2, lwd = 0.7, col = 'gray1', lty = 5)
legend('bottomright', legend = c("Unweighted", "Dollar-Weighted"),
       col = c(line.col1, line.col2), lwd = 3, lty = 1, bty = 'n')
dev.off()


