### ----------------------------------------------------------------------------
### CLIMATE CHANGE, ARIDITY, AND INTERNAL MIGRATION 
### ----------------------------------------------------------------------------

### 
### EXPLORATORY MODELS: DETERMINANTS OF MIGRATION FLOWS ------------------------
###  


rm(list=ls())

###
### PACKAGES -------------------------------------------------------------------
### 

library(tidyverse)
library(fixest)
library(ggpubr)

citation("tidyverse")
citation("fixest")
citation("ggpubr")

###
### LOAD DATA ------------------------------------------------------------------
### 

load(file="full migration data.RData")

# > dividing gdp per capita by 1000 for rescaling
d <- d %>% 
  mutate(orig_gdp_per_capita = orig_gdp_per_capita/1000,
         dest_gdp_per_capita = dest_gdp_per_capita/1000)

###
### CROSS-SECTIONAL MODELS TO EXPLORE CHARACTERISTICS OF  OUTMIGRATION REGIONS ----
### 


m1 <- feols(flow_out_rate_annual ~
              as.factor(mig_interval) + dest_no+
              as.factor(year_cat10)+
              cntry_name, 
            data = d)
summary(m1, se = "hetero")             

m2 <- feols(flow_out_rate_annual ~
              as.factor(mig_interval) + dest_no +
              log(dist) + contig+
              as.factor(year_cat10)+
              cntry_name,
            data = d) 
summary(m2, se = "hetero")             


m3 <- feols(flow_out_rate_annual ~
              as.factor(mig_interval) + dest_no +
              log(dist) + contig+
              as.factor(year_cat10)+
              as.factor(orig_landborder)+as.factor(dest_landborder)+
              cntry_name,
            data = d) 
summary(m3, se = "hetero")

m4 <- feols(flow_out_rate_annual ~
              as.factor(mig_interval) + dest_no +
              log(dist) + contig+
              as.factor(year_cat10)+
              as.factor(orig_landborder)+as.factor(dest_landborder)+
              orig_gdp_per_capita+orig_indgen_afaf+
              dest_gdp_per_capita+dest_indgen_afaf+
              cntry_name,
            data = d) 
summary(m4, se = "hetero")

m5 <- feols(flow_out_rate_annual ~
              as.factor(mig_interval) + dest_no +
              log(dist) + contig+
              as.factor(year_cat10)+
              as.factor(orig_landborder)+as.factor(dest_landborder)+
              orig_urban+
              dest_urban+
              cntry_name,
            data = d) 
summary(m5, se = "hetero")

m6 <- feols(flow_out_rate_annual ~
              as.factor(mig_interval) + dest_no +
              log(dist) + contig+
              as.factor(year_cat10)+
              as.factor(orig_landborder)+as.factor(dest_landborder)+ gini+
              cntry_name,
            data = d) 
summary(m6, se = "hetero")


models <- etable(m1, m2,m3,m4,m5, m6,
                 drop = c("year_cat10", "country_name"),
                 se="hetero",
                 digits = "r4",
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
                 coefstat = "confint",ci=0.9)
models


models <- etable(m1, m2,m3,m4,m5, m6,
                 drop = c("year_cat10", "country_name"),
                 se="hetero",
                 digits = "r4",
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models

write.csv(models, "table_extended_migration drivers worldwide.csv")
