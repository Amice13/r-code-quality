##########################################################################
# Table A1: Heterogeneous Treatment Effects of Campaign Limits on 
# PSC Victories by Distance of Polling Stations to an AG Branch
###########################################################################

rm(list=ls())
library(data.table)
library(rdrobust)
library(here)
library(tidyr)
library(kableExtra)
library(modelsummary)
library(dplyr)

# data
electoral <- readRDS(here("data","mayors_municipal_level.rds"))

## Aux functions (for exporting rdrobust with modelsummary)

tidy.rdrobust <- function(model, ...) {
  ret <- data.frame(
    term = 'Robust Coef.',
    estimate = model$coef[3, 1],
    std.error = model$se[3, 1],
    p.value = model$pv[3, 1]
  )
  row.names(ret) <- NULL
  ret
}

glance.rdrobust <- function(model, ...) {
  ret <- data.frame(
    #Kernel = model$kernel,
    Bandwidth = paste('R$', round(model$bws[2,2],0)),
    #Bw.Selection = model$bwselect,
    Eff.N.obs = as.character(model$N_h[[1]] + model$N_h[[2]])
  )
  ret
}


# Above/below median groups
electoral <- electoral %>%
  mutate(
    dist_avg_hi_ad = as.numeric(avg_distance_ad > median(avg_distance_ad, na.rm = TRUE)),
    dist_median_hi_ad = as.numeric(median_distance_ad > median(median_distance_ad, na.rm = TRUE))
  )


### Table 2 - Panel A

dist_avg_hi <- with(filter(electoral, dist_avg_hi_ad==1), 
                    rdrobust(psc_elected_2016, inverted_margin, all=TRUE))

dist_avg_lo <- with(filter(electoral, dist_avg_hi_ad==0), 
                    rdrobust(psc_elected_2016, inverted_margin, all=TRUE))

dist_median_hi <- with(filter(electoral, dist_median_hi_ad==1), 
                       rdrobust(psc_elected_2016, inverted_margin, all=TRUE))

dist_median_lo <- with(filter(electoral, dist_median_hi_ad==0), 
                       rdrobust(psc_elected_2016, inverted_margin, all=TRUE))


table_prb_2016 <- list('(1)' = dist_avg_hi, 
                       '(2)' = dist_median_hi, 
                       '(3)' = dist_avg_lo, 
                       '(4)' = dist_median_lo
)

row1 <- c('Distance Measure','Mean','Median','Mean','Median')
row2 <- c('Distance','High','High','Low','Low')

modelsummary(table_prb_2016, 
             statistic = c("[{std.error}]",'p.value'),
             #output = 'latex',
             stars = T,
             add_rows = data.frame(t(data.frame(row1,row2))),
             title = 'Heterogeneous Treatment Effects of Campaign Limits on PSC Victories by Distance of Polling Stations to an AG Branch (Post-Treatment, 2016)') 


### Table A1 - Panel B

dist_avg_hi <- with(filter(electoral, dist_avg_hi_ad==1), 
                    rdrobust(psc_elected_2012, inverted_margin, all=TRUE))

dist_avg_lo <- with(filter(electoral, dist_avg_hi_ad==0), 
                    rdrobust(psc_elected_2012, inverted_margin, all=TRUE))

dist_median_hi <- with(filter(electoral, dist_median_hi_ad==1), 
                       rdrobust(psc_elected_2012, inverted_margin, all=TRUE))

dist_median_lo <- with(filter(electoral, dist_median_hi_ad==0), 
                       rdrobust(psc_elected_2012, inverted_margin, all=TRUE))


table_prb_2012 <- list('(1)' = dist_avg_hi, 
                       '(2)' = dist_median_hi,  
                       '(3)' = dist_avg_lo, 
                       '(4)' = dist_median_lo
)

row1 <- c('Distance Measure','Mean','Median','Mean','Median')
row2 <- c('Distance','High','High','Low','Low')

modelsummary(table_prb_2012, 
             statistic = c("[{std.error}]",'p.value'),
             #output = 'latex',
             stars = T,
             add_rows = data.frame(t(data.frame(row1,row2))),
             title = 'Heterogeneous Treatment Effects of Campaign Limits on PSC Victories by Distance of Polling Stations to an AG Branch (Pre-Treatment, 2012)') 



