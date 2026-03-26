## Hager / Hilbig - Inheritance replication code - Feb 26 2019
## hhilbig@g.harvard.edu
##
## This file : Wealth and Income data summary statistics
## Tables reproduced in this file: A12
##
### ### ### ###

rm(list = ls())

## Packages

library(stargazer)

## Then, load the data

inher <- read.csv('Data_Main.csv')

#### TABLE A12 : Tax record data summary statistics ####

## Helper function to get summary stats

make_sumstats <- function(v_sum, df) {
  
  ## First, get just the desired variable
  
  v <- df[, v_sum]
  
  ## Get mean, sd, min, max, and number of missing observations
  
  m <- mean(v, na.rm = T)
  s <- sd(v, na.rm = T)
  mi <- min(v, na.rm = T)
  ma <- max(v, na.rm = T)
  nrate_miss <- sum(is.na(v)) / length(v)
  
  ## Return the summary stats, round to two digits
  
  round(c(m, s, mi, ma, nrate_miss * 100), 2)
}

## Create a list of the desired variables

sum_list <- c("suminc31_mean", "suminc31_median", "suminc31_gini",
              "inc_rental_mean", "inc_rental_median", "inc_rental_gini")

## Get summary stats for each variable

ss_table <- lapply(sum_list, make_sumstats, df = inher)

## Convert to DF

ss_table <- do.call('rbind', ss_table)
ss_table <- data.frame(ss_table)

## Rename the variables

vars_proper <- c('Mean income (Euro)', 'Median income (Euro)', 'Income GINI',
                 'Mean rental income (Euro)', 'Median rental income (Euro)',
                 'Rental income GINI')

## Add the proper variable names to the table

ss_table <- cbind(vars_proper, ss_table)

## Change the column names for stargazer output

colnames(ss_table) <- c('', 'Mean', 'SD', 'Min', 'Max', '\\% Missing')

## Ouput via stargazer - this is table A13

stargazer(ss_table, summary = F, digits = 2, rownames = F, style = 'ajps')

