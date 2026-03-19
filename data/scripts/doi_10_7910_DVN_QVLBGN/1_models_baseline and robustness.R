### ----------------------------------------------------------------------------
### CLIMATE CHANGE, ARIDITY, AND INTERNAL MIGRATION 
### ----------------------------------------------------------------------------

rm(list=ls())

### 
### BASELINE MODELS AND ROBUSTNESS CHECKS --------------------------------------
###  


###
### LOAD DATA AND PACKAGES -----------------------------------------------------
### 

library(tidyverse)
library(fixest)

citation("fixest")
citation("tidyverse")

load(file="full migration data.RData")

###
###  Table 1: BASELINE MODELS -------------------------------------------------
### 

m0 <-  fepois(flow_out_rate_annual ~  
                worldregion*as.factor(year_cat10)+
                log(dist)+contig+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              data=d) 
summary(m0, se = "cluster")

m1a <-  fepois(flow_out_rate_annual ~ 
                 orig_ai_mean10_stan1 +
                 dest_ai_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m1a, se = "cluster")

m2a <-  fepois(flow_out_rate_annual ~ 
                 orig_pdsi_mean10_stan1 +
                 dest_pdsi_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m2a, se = "cluster")

m3a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei03_mean10_stan1 + 
                 dest_spei03_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m3a, se = "cluster")

m4a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei12_mean10_stan1+ 
                 dest_spei12_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m4a, se = "cluster")

models <- etable(m0, m1a, m2a,m3a,m4a,
                 keep = c("orig_ai_mean10_stan1","orig_spei12_mean10_stan1",
                          "orig_spei03_mean10_stan1", "orig_pdsi_mean10_stan1",
                          "dest_ai_mean10_stan1","dest_spei12_mean10_stan1",
                          "dest_spei03_mean10_stan1", "dest_pdsi_mean10_stan1",
                          "dist", "contig", "mig_interval" ),
                 se="cluster",
                 order = c("ai", "pdsi", "spei", "dist","contig", "mig_interval", "year" ),
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models

write.csv(models, "table_baseline_drought and aridity impacts on outmigration rate2.csv")


models <- etable(m1a, m2a,m3a,m4a,
                 keep = c("orig_ai_mean10_stan1","orig_spei12_mean10_stan1",
                          "orig_spei03_mean10_stan1", "orig_pdsi_mean10_stan1",
                          "dest_ai_mean10_stan1","dest_spei12_mean10_stan1",
                          "dest_spei03_mean10_stan1", "dest_pdsi_mean10_stan1",
                          "dist", "contig", "mig_interval" ),
                 se="cluster",
                 coefstat="confint",
                 ci=0.9,
                 order = c("ai", "pdsi", "spei", "dist","contig", "mig_interval", "year" ),
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models


###
### SUPPLEMENT EXTENDED MODEL - Table S5: INTERNATIONAL MIGRATION  -------------
### 


load("international migration data.RData")

intmig <- intmig %>% 
  mutate(cntry_code = ifelse(cntry_code=="ROM", "ROU", cntry_code))

table(d$year_cat10)

d <- d %>% 
  mutate(decade = recode(year_cat10,
                         "(-Inf,1.97e+03]" = "1960s", 
                         "(1.97e+03,1.98e+03]" = "1970s",
                         "(1.98e+03,1.99e+03]" = "1980s",  
                         "(1.99e+03,2e+03]"="1990s",   
                         "(2e+03,2.01e+03]" ="2000s",
                         "(2.01e+03, Inf]" = "2010s"))




d <- d %>% left_join(intmig, by=c("alpha3"="cntry_code", "decade"="decade2"))
summary(d$mig_international)
summary(d$flow_international)

d.country <- d %>% 
  group_by(alpha3, year) %>% 
  summarize(flow_out_rate_annual = mean(flow_out_rate_annual, na.rm=T),
            flow = sum(flow_annual, na.rm=T), 
            orig_pop = sum(orig_pop, na.rm=T),
            cntry_pop = unique(pop),
            mig_international = unique(flow_international),
            worldregion = unique(worldregion),
            year_cat10 = unique(year_cat10), 
            mig_interval = unique(mig_interval))

m1 <-  feols(log(mig_international) ~ 
               log(flow)+
               as.factor(year_cat10)+
               as.factor(mig_interval),
             fixef = c("alpha3"),
             data=d.country) 
summary(m1, se = "cluster")

m2 <-  feols(log(mig_international) ~ 
               log(flow)+
               log(orig_pop)+
               as.factor(year_cat10)+
               as.factor(mig_interval),
             fixef = c("alpha3"),
             data=d.country) 
summary(m2, se = "cluster")

m3 <-  feols(log(mig_international) ~ 
               flow_out_rate_annual+
               as.factor(year_cat10)+
               as.factor(mig_interval),
             fixef = c("alpha3"),
             data=d.country) 
summary(m3, se = "cluster")

m4 <-  feols(log(mig_international) ~ 
               flow_out_rate_annual+
               log(orig_pop)+
               as.factor(year_cat10)+
               as.factor(mig_interval),
             fixef = c("alpha3"),
             data=d.country) 
summary(m4, se = "cluster")


models <- etable(m1, m2,m3,m4,
                 keep = c("flow","flow_out_rate_annual",
                          "orig_pop","mig_interval"),
                 se="cluster",
                 order = c("ai", "pdsi", "spei", "dist","contig", "mig_interval", "year" ),
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models

write.csv(models, "table_extended_international and internal migration.csv")



###
### SUPPLEMENT EXTENDED MODEL - Table S6: LAGGED MODELS ------------------------
### 


m0 <-  fepois(flow_out_rate_annual ~  
                worldregion*as.factor(year_cat10)+
                log(dist)+contig+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              data=d) 
summary(m0, se = "cluster")

m1a <-  fepois(flow_out_rate_annual ~ 
                 orig_ai_mean5_stan1+
                 orig_ai_mean6_10_stan1+
                 orig_ai_mean11_15_stan1+
                 orig_ai_mean16_20_stan1+
                 
                 dest_ai_mean5_stan1+
                 dest_ai_mean6_10_stan1+
                 dest_ai_mean11_15_stan1+
                 dest_ai_mean16_20_stan1+
                 
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m1a, se = "cluster")

m2a <-  fepois(flow_out_rate_annual ~ 
                 orig_pdsi_mean5_stan1+
                 orig_pdsi_mean6_10_stan1+
                 orig_pdsi_mean11_15_stan1+
                 orig_pdsi_mean16_20_stan1+
                 
                 dest_pdsi_mean5_stan1+
                 dest_pdsi_mean6_10_stan1+
                 dest_pdsi_mean11_15_stan1+
                 dest_pdsi_mean16_20_stan1+
                 
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m2a, se = "cluster")

m3a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei03_mean5_stan1+
                 orig_spei03_mean6_10_stan1+
                 orig_spei03_mean11_15_stan1+
                 orig_spei03_mean16_20_stan1+
                 
                 dest_spei03_mean5_stan1+
                 dest_spei03_mean6_10_stan1+
                 dest_spei03_mean11_15_stan1+
                 dest_spei03_mean16_20_stan1+
                 
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m3a, se = "cluster")

m4a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei12_mean5_stan1+
                 orig_spei12_mean6_10_stan1+
                 orig_spei12_mean11_15_stan1+
                 orig_spei12_mean16_20_stan1+
                 
                 dest_spei12_mean5_stan1+
                 dest_spei12_mean6_10_stan1+
                 dest_spei12_mean11_15_stan1+
                 dest_spei12_mean16_20_stan1+
                 
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m4a, se = "cluster")

models <- etable(m0, m1a, m2a,m3a,m4a,
                 se="cluster",
                 order = c("ai", "pdsi", "spei", "dist","contig", "mig_interval", "year" ),
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models


write.csv(models, "table_robustness_lagged models.csv")




###
### SUPPLEMENT EXTENDED MODEL - Table S7: INTERACTING BY MIG_INTERVAL ----------
### 

m0 <-  fepois(flow_out_rate_annual ~  
                worldregion*as.factor(year_cat10)+
                log(dist)+contig+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              data=d) 
summary(m0, se = "cluster")

m1a <-  fepois(flow_out_rate_annual ~ 
                 orig_ai_mean10_stan1*mig_interval +
                 dest_ai_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m1a, se = "cluster")

m2a <-  fepois(flow_out_rate_annual ~ 
                 orig_pdsi_mean10_stan1*mig_interval +
                 dest_pdsi_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m2a, se = "cluster")

m3a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei03_mean10_stan1*mig_interval + 
                 dest_spei03_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m3a, se = "cluster")

m4a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei12_mean10_stan1*mig_interval+ 
                 dest_spei12_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m4a, se = "cluster")

models <- etable(m0, m1a, m2a,m3a,m4a,
                 keep = c("orig_ai_mean10_stan1","orig_spei12_mean10_stan1",
                          "orig_spei03_mean10_stan1", "orig_pdsi_mean10_stan1",
                          "dest_ai_mean10_stan1","dest_spei12_mean10_stan1",
                          "dest_spei03_mean10_stan1", "dest_pdsi_mean10_stan1",
                          "dist", "contig", "mig_interval" ),
                 se="cluster",
                 order = c("ai", "pdsi", "spei03","spei12", "dist","contig", "mig_interval", "year" ),
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models


write.csv(models, "table_robustness_interacting with migration interval2.csv")



##
## SUPPLEMENT EXTENDED MODEL - TABLE S8: QUADRATIC TERMS IN MODEL --------------
## 

d <- d %>% 
  mutate(orig_ai_mean10_stan1_sq = orig_ai_mean10_stan1^2,
         orig_spei03_mean10_stan1_sq = orig_spei03_mean10_stan1^2,
         orig_spei12_mean10_stan1_sq = orig_spei12_mean10_stan1^2,
         orig_pdsi_mean10_stan1_sq = orig_pdsi_mean10_stan1^2)


m0 <-  fepois(flow_out_rate_annual ~  
                worldregion*as.factor(year_cat10)+
                log(dist)+contig+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              data=d) 
summary(m0, se = "cluster")

m1a <-  fepois(flow_out_rate_annual ~ 
                 orig_ai_mean10_stan1 +
                 orig_ai_mean10_stan1_sq+
                 dest_ai_mean10_stan1 +
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m1a, se = "cluster")

m2a <-  fepois(flow_out_rate_annual ~ 
                 orig_pdsi_mean10_stan1 +
                 orig_pdsi_mean10_stan1_sq +
                 dest_pdsi_mean10_stan1 +
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m2a, se = "cluster")

m3a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei03_mean10_stan1 + 
                 orig_spei03_mean10_stan1_sq +
                 dest_spei03_mean10_stan1 + 
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m3a, se = "cluster")

m4a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei12_mean10_stan1+ 
                 orig_spei12_mean10_stan1_sq+
                 dest_spei12_mean10_stan1+ 
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m4a, se = "cluster")

models <- etable(m0, m1a, m2a,m3a,m4a,
                 keep = c("orig_ai_mean10_stan1","orig_spei12_mean10_stan1",
                          "orig_spei03_mean10_stan1", "orig_pdsi_mean10_stan1",
                          "orig_ai_mean10_stan1_sq", "orig_pdsi_mean10_stan1_sq",
                          "orig_spei12_mean10_stan1_sq", "orig_pdsi_mean10_stan1_sq",
                          "dist", "contig", "mig_interval" ),
                 se="cluster",
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models

write.csv(models, "table_extended_quadratic terms in models2.csv")


##
## SUPPLEMENT ROBUSTNESS - TABLE S17: CONTROLING FOR NEIGHBORHOOD VALUES -------
## 

load("neighborhood climate values.RData")

d.neighbors <- d.neighbors %>% mutate(year = as.numeric(year))
d.neighbors <- d.neighbors %>% 
  filter(grepl("[A-Za-z]", geolevel1) == FALSE)
d.neighbors <- d.neighbors %>% 
  mutate(geolevel1 = as.numeric(geolevel1))

d.new <- d %>% 
  left_join(d.neighbors,
            by=c("orig" = "geolevel1", "year" = "year"))

d.new %>% filter(is.na(neighbors_ai_mean10_stan1)) %>% group_by(alpha3, year, orig) %>% count() %>% print(n=600)


d.new <- d.new %>% 
  mutate(neighbors_ai_mean10_stan1 = ifelse(is.na(neighbors_ai_mean10_stan1), 0,neighbors_ai_mean10_stan1),
         neighbors_pdsi_mean10_stan1 = ifelse(is.na(neighbors_pdsi_mean10_stan1), 0,neighbors_pdsi_mean10_stan1),
         neighbors_spei03_mean10_stan1 = ifelse(is.na(neighbors_spei03_mean10_stan1), 0,neighbors_spei03_mean10_stan1),
         neighbors_spei12_mean10_stan1 = ifelse(is.na(neighbors_spei12_mean10_stan1), 0,neighbors_spei12_mean10_stan1))

m0 <-  fepois(flow_out_rate_annual ~  
                worldregion*as.factor(year_cat10)+
                log(dist)+contig+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              cluster = c("alpha3", "year_cat10"),
              data=d.new) 
summary(m0, se = "cluster")

m1a <-  fepois(flow_out_rate_annual ~ 
                 orig_ai_mean10_stan1 +
                 dest_ai_mean10_stan1 +
                 
                 neighbors_ai_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               cluster = c("alpha3", "year_cat10"),
               data=d.new) 
summary(m1a, se = "cluster")

m2a <-  fepois(flow_out_rate_annual ~ 
                 orig_pdsi_mean10_stan1 +
                 dest_pdsi_mean10_stan1 +
                 
                 neighbors_pdsi_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               cluster = c("alpha3", "year_cat10"),
               data=d.new) 
summary(m2a, se = "cluster")

m3a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei03_mean10_stan1 + 
                 dest_spei03_mean10_stan1 + 
                 
                 neighbors_spei03_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               cluster = c("alpha3", "year_cat10"),
               data=d.new) 
summary(m3a, se = "cluster")

m4a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei12_mean10_stan1+ 
                 dest_spei12_mean10_stan1+
                 neighbors_spei12_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               cluster = c("alpha3", "year_cat10"),
               data=d.new) 
summary(m4a, se = "cluster")

models <- etable(m0, m1a, m2a,m3a,m4a,
                 
                 se="cluster",cluster = c("orig_dest_dyad"),
                 order = c("ai", "pdsi", "spei", "dist","contig", "mig_interval", "year" ),
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models

write.csv(models, "table_robutness_spatial models.csv")


###
### SUPPLEMENT ROBUSTNESS - TABLE S18: MULITLATERAL RESITANCE TERMS ------------
###


d <- d %>% 
  mutate(orig_decade_fe = paste(orig,decade, sep="_"),
         dest_decade_fe = paste(dest,decade, sep="_"))

d <- d %>% 
  mutate(ai_mean10_stan1_dif = orig_ai_mean10_stan1-dest_ai_mean10_stan1,
         pdsi_mean10_stan1_dif = orig_pdsi_mean10_stan1-dest_pdsi_mean10_stan1,
         spei03_mean10_stan1_dif = orig_spei03_mean10_stan1-dest_spei03_mean10_stan1,
         spei12_mean10_stan1_dif = orig_spei12_mean10_stan1-dest_spei12_mean10_stan1,
         
         ai_mean10_stan1_ratio = orig_ai_mean10_stan1-dest_ai_mean10_stan1,
         pdsi_mean10_stan1_ratio = orig_pdsi_mean10_stan1-dest_pdsi_mean10_stan1,
         spei03_mean10_stan1_ratio = orig_spei03_mean10_stan1-dest_spei03_mean10_stan1,
         spei12_mean10_stan1_ratio = orig_spei12_mean10_stan1-dest_spei12_mean10_stan1,
  )

m0 <-  fepois(flow_out_rate_annual ~  
                worldregion*as.factor(year_cat10)+
                log(dist)+contig+
                as.factor(mig_interval),
              fixef = c("orig", "dest_decade_fe"),
              data=d) 
summary(m0, se = "cluster")

m1a <-  fepois(flow_out_rate_annual ~ 
                 ai_mean10_stan1_dif+
                 as.factor(mig_interval),
               fixef = c("orig", "dest_decade_fe"),
               data=d) 
summary(m1a, se = "cluster")

m2a <-  fepois(flow_out_rate_annual ~ 
                 pdsi_mean10_stan1_dif+
                 as.factor(mig_interval),
               fixef = c("orig", "dest_decade_fe"),
               data=d) 
summary(m2a, se = "cluster")

m3a <-  fepois(flow_out_rate_annual ~ 
                 spei03_mean10_stan1_dif+
                 as.factor(mig_interval),
               fixef = c("orig", "dest_decade_fe"),
               data=d) 
summary(m3a, se = "cluster")

m4a <-  fepois(flow_out_rate_annual ~ 
                 spei12_mean10_stan1_dif+
                 as.factor(mig_interval),
               fixef = c("orig", "dest_decade_fe"),
               data=d) 
summary(m4a, se = "cluster")

models <- etable(m0, m1a, m2a,m3a,m4a,
                 se="cluster",
                 order = c("ai", "pdsi", "spei", "dist","contig", "mig_interval", "year" ),
                 cluster="orig_dest_dyad",
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models


write.csv(models, "table_robustness_multilateral resistance.csv")


###
### SUPPLEMENT ROBUSTNESS - TABLE S19: CONTROLING FOR INTERNATIONAL MIGRATION ----
### 

m0 <-  fepois(flow_out_rate_annual ~  
                worldregion*as.factor(year_cat10)+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              data=d) 
summary(m0, se = "cluster")

m1a <-  fepois(flow_out_rate_annual ~ 
                 orig_ai_mean10_stan1 +
                 dest_ai_mean10_stan1+
                 flow_international+
                 worldregion*as.factor(year_cat10)+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m1a, se = "cluster")

m2a <-  fepois(flow_out_rate_annual ~ 
                 orig_pdsi_mean10_stan1 +
                 dest_pdsi_mean10_stan1+
                 flow_international+
                 worldregion*as.factor(year_cat10)+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m2a, se = "cluster")

m3a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei03_mean10_stan1 + 
                 dest_spei03_mean10_stan1+
                 flow_international+
                 worldregion*as.factor(year_cat10)+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m3a, se = "cluster")

m4a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei12_mean10_stan1+ 
                 dest_spei12_mean10_stan1+
                 flow_international+
                 worldregion*as.factor(year_cat10)+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m4a, se = "cluster")

models <- etable(m1a, m2a,m3a,m4a,
                 keep = c("orig_ai_mean10_stan1","orig_spei12_mean10_stan1",
                          "orig_spei03_mean10_stan1", "orig_pdsi_mean10_stan1",
                          "dest_ai_mean10_stan1","dest_spei12_mean10_stan1",
                          "dest_spei03_mean10_stan1", "dest_pdsi_mean10_stan1", "flow_international",
                          "dist", "contig", "mig_interval" ),
                 se="cluster",
                 order = c("ai", "pdsi", "spei", "dist","contig", "mig_interval", "year" ),
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models

write.csv(models, "table_robustness_controling for international migration.csv")




##
## SUPPLEMENT ROBUSTNESS - TABLE S20: LEAD VALUES INSTEAD OF LAG  --------------
## 


m0 <-  fepois(flow_out_rate_annual ~  
                alpha3*as.factor(year_cat10)+
                log(dist)+contig+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              data=d) 
summary(m0, se = "cluster")

m1a <-  fepois(flow_out_rate_annual ~ 
                 orig_ai_mean10_stan1 +
                 orig_ai_lead_mean10_stan1 +
                 dest_ai_mean10_stan1 +
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m1a, se = "cluster")

m2a <-  fepois(flow_out_rate_annual ~ 
                 orig_pdsi_mean10_stan1 +
                 orig_pdsi_lead_mean10_stan1 +
                 dest_pdsi_mean10_stan1 +
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m2a, se = "cluster")

m3a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei03_mean10_stan1 + 
                 orig_spei03_lead_mean10_stan1 + 
                 dest_spei03_mean10_stan1 + 
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m3a, se = "cluster")

m4a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei12_mean10_stan1+ 
                 orig_spei12_lead_mean10_stan1+
                 dest_spei12_mean10_stan1+ 
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m4a, se = "cluster")

models <- etable(m0, m1a, m2a,m3a,m4a,
                 keep = c("orig_ai_mean10_stan1","orig_spei12_mean10_stan1",
                          "orig_spei03_mean10_stan1", "orig_pdsi_mean10_stan1",
                          "orig_ai_lead_mean10_stan1","orig_spei12_lead_mean10_stan1",
                          "orig_spei03_lead_mean10_stan1", "orig_pdsi_lead_mean10_stan1",
                          "mig_interval", "dist", "contig"),
                 se="cluster",
                 order = c("ai", "pdsi", "spei", "dist","contig", "mig_interval", "year" ),
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models

write.csv(models, "table_robustness_drought and aridity impacts controlling for lead values2.csv")


##
## SUPPLEMENT ROBUSTNESS - TABLE S21: LOG LINEAR MODELS ------------------------
## 


d <- d %>% 
  mutate(flow_nonzero = ifelse(flow_annual<1, 1,flow_annual))

m0 <-  feols(log(flow_nonzero) ~  
               worldregion*as.factor(year_cat10)+
               log(dist)+contig+
               as.factor(mig_interval),
             fixef = c("orig_dest_dyad"),
             data=d) 
summary(m0, se = "cluster")

m1a <-  feols(log(flow_nonzero) ~ 
                orig_ai_mean10_stan1 +
                dest_ai_mean10_stan1+
                log(orig_pop)+
                log(dest_pop)+
                worldregion*as.factor(year_cat10)+
                log(dist)+contig+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              data=d) 
summary(m1a, se = "cluster")

m2a <-  feols(log(flow_nonzero) ~ 
                orig_pdsi_mean10_stan1 +
                dest_pdsi_mean10_stan1+
                log(orig_pop)+
                log(dest_pop)+
                worldregion*as.factor(year_cat10)+
                log(dist)+contig+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              data=d) 
summary(m2a, se = "cluster")

m3a <-  feols(log(flow_nonzero) ~ 
                orig_spei03_mean10_stan1 + 
                dest_spei03_mean10_stan1+
                log(orig_pop)+
                log(dest_pop)+
                worldregion*as.factor(year_cat10)+
                log(dist)+contig+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              data=d) 
summary(m3a, se = "cluster")

m4a <-  feols(log(flow_nonzero) ~ 
                orig_spei12_mean10_stan1+ 
                dest_spei12_mean10_stan1+
                log(orig_pop)+
                log(dest_pop)+
                worldregion*as.factor(year_cat10)+
                log(dist)+contig+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              data=d) 
summary(m4a, se = "cluster")

models <- etable(m0, m1a, m2a,m3a,m4a,
                 keep = c("orig_ai_mean10_stan1","orig_spei12_mean10_stan1",
                          "orig_spei03_mean10_stan1", "orig_pdsi_mean10_stan1",
                          "dest_ai_mean10_stan1","dest_spei12_mean10_stan1",
                          "dest_spei03_mean10_stan1", "dest_pdsi_mean10_stan1",
                          "dist", "contig", "mig_interval" ),
                 se="cluster",
                 order = c("ai", "pdsi", "spei", "dist","contig", "mig_interval", "year" ),
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models

write.csv(models, "table_robustness_log linear model.csv")




###
### SUPPLEMENT ROBUSTNESS - TABLE S22: NET MIGRATION RATE AS OUTCOME  ----------
### 



m0 <-  feols(net_rate_annual ~  
               worldregion*as.factor(year_cat10)+
               log(dist)+contig+
               as.factor(mig_interval),
             fixef = c("orig_dest_dyad"),
             data=d) 
summary(m0, se = "cluster")

m1a <-  feols(net_rate_annual ~ 
                orig_ai_mean10_stan1 +
                dest_ai_mean10_stan1 +
                worldregion*as.factor(year_cat10)+
                log(dist)+contig+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              data=d) 
summary(m1a, se = "cluster", coefstat ="confint")

m2a <-  feols(net_rate_annual ~ 
                orig_pdsi_mean10_stan1 +
                dest_pdsi_mean10_stan1 +
                worldregion*as.factor(year_cat10)+
                log(dist)+contig+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              data=d) 
summary(m2a, se = "cluster")

m3a <-  feols(net_rate_annual ~ 
                orig_spei03_mean10_stan1 + 
                dest_spei03_mean10_stan1 + 
                worldregion*as.factor(year_cat10)+
                log(dist)+contig+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              data=d) 
summary(m3a, se = "cluster")

m4a <-  feols(net_rate_annual ~ 
                orig_spei12_mean10_stan1+ 
                dest_spei12_mean10_stan1+ 
                worldregion*as.factor(year_cat10)+
                log(dist)+contig+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              data=d) 
summary(m4a, se = "cluster")


models <- etable(m1a, m2a,m3a,m4a,
                 keep = c("orig_ai_mean10_stan1","orig_spei12_mean10_stan1",
                          "orig_spei03_mean10_stan1", "orig_pdsi_mean10_stan1",
                          "mig_interval", "dist", "contig"),
                 se="cluster",
                 ci=0.9,
                 coefstat ="confint",
                 order = c("ai", "pdsi", "spei", "dist","contig", "mig_interval", "year" ),
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models

models <- etable(m0, m1a, m2a,m3a,m4a,
                 keep = c("orig_ai_mean10_stan1","orig_spei12_mean10_stan1",
                          "orig_spei03_mean10_stan1", "orig_pdsi_mean10_stan1",
                          "mig_interval", "dist", "contig"),
                 se="cluster",
                 order = c("ai", "pdsi", "spei", "dist","contig", "mig_interval", "year" ),
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models

options(scipen=999)
write.csv(models, "table_robustness_net migration rate as outcome.csv")




###
###  SUPPLEMENT ROBUSTNESS - TABLE S23: POPULATION WEIGHTING -------------------
### 

m0 <-  fepois(flow_out_rate_annual ~  
                worldregion*as.factor(year_cat10)+
                log(dist)+contig+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              weights = log(d$orig_pop),
              data=d) 
summary(m0, se = "cluster")

m1a <-  fepois(flow_out_rate_annual ~ 
                 orig_ai_mean10_stan1 +
                 dest_ai_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               weights = log(d$orig_pop),
               data=d) 
summary(m1a, se = "cluster")

m2a <-  fepois(flow_out_rate_annual ~ 
                 orig_pdsi_mean10_stan1 +
                 dest_pdsi_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               weights = log(d$orig_pop),
               data=d) 
summary(m2a, se = "cluster")

m3a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei03_mean10_stan1 + 
                 dest_spei03_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               weights = log(d$orig_pop),
               data=d) 
summary(m3a, se = "cluster")

m4a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei12_mean10_stan1+ 
                 dest_spei12_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               weights = log(d$orig_pop),
               data=d) 
summary(m4a, se = "cluster")

models <- etable(m0, m1a, m2a,m3a,m4a,
                 keep = c("orig_ai_mean10_stan1","orig_spei12_mean10_stan1",
                          "orig_spei03_mean10_stan1", "orig_pdsi_mean10_stan1",
                          "dest_ai_mean10_stan1","dest_spei12_mean10_stan1",
                          "dest_spei03_mean10_stan1", "dest_pdsi_mean10_stan1",
                          "dist", "contig", "mig_interval" ),
                 se="cluster",
                 order = c("ai", "pdsi", "spei", "dist","contig", "mig_interval", "year" ),
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models


write.csv(models, "table_robustness_population weighting.csv")



###
### SUPPLEMENT ROBUSTNESS - TABLE S24: WEIGHTING BY AREA SIZE ------------------
### 


m0 <-  fepois(flow_out_rate_annual ~  
                worldregion*as.factor(year_cat10)+
                log(dist)+contig+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              data=d,
              weights = d$orig_area) 
summary(m0, se = "cluster")

m1a <-  fepois(flow_out_rate_annual ~ 
                 orig_ai_mean10_stan1 +
                 dest_ai_mean10_stan1 +
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d,
               weights = d$orig_area) 
summary(m1a, se = "cluster")

m2a <-  fepois(flow_out_rate_annual ~ 
                 orig_pdsi_mean10_stan1 +
                 dest_pdsi_mean10_stan1 +
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d,
               weights = d$orig_area) 
summary(m2a, se = "cluster")

m3a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei03_mean10_stan1 + 
                 dest_spei03_mean10_stan1 + 
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d,
               weights = d$orig_area) 
summary(m3a, se = "cluster")

m4a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei12_mean10_stan1+ 
                 dest_spei12_mean10_stan1+ 
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d,
               weights = d$orig_area) 
summary(m4a, se = "cluster")

models <- etable(m0, m1a, m2a,m3a,m4a,
                 keep = c("orig_ai_mean10_stan1","orig_spei12_mean10_stan1",
                          "orig_spei03_mean10_stan1", "orig_pdsi_mean10_stan1",
                          "mig_interval", "dist", "contig"),
                 se="cluster",
                 order = c("ai", "pdsi", "spei","dist","contig", "mig_interval", "year" ),
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models

write.csv(models, "table_robustness_results weighted by area size2.csv")


###
### SUPPLEMENT ROBUSTNESS - TABLE S25: WEIGHTING BY NUMBER OF DESTINATION REGIONS IN COUNTRY ----------
### 


m0 <-  fepois(flow_out_rate_annual ~  
                worldregion*as.factor(year_cat10)+
                log(dist)+contig+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              data=d,
              weights = d$dest_no) 
summary(m0, se = "cluster")

m1a <-  fepois(flow_out_rate_annual ~ 
                 orig_ai_mean10_stan1 +
                 dest_ai_mean10_stan1 +
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d,
               weights = d$dest_no) 
summary(m1a, se = "cluster")

m2a <-  fepois(flow_out_rate_annual ~ 
                 orig_pdsi_mean10_stan1 +
                 dest_pdsi_mean10_stan1 +
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d,
               weights = d$dest_no) 
summary(m2a, se = "cluster")

m3a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei03_mean10_stan1 + 
                 dest_spei03_mean10_stan1 + 
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d,
               weights = d$dest_no) 
summary(m3a, se = "cluster")

m4a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei12_mean10_stan1+ 
                 dest_spei12_mean10_stan1+ 
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d,
               weights = d$dest_no) 
summary(m4a, se = "cluster")

models <- etable(m0, m1a, m2a,m3a,m4a,
                 keep = c("orig_ai_mean10_stan1","orig_spei12_mean10_stan1",
                          "orig_spei03_mean10_stan1", "orig_pdsi_mean10_stan1",
                          "mig_interval", "dist", "contig"),
                 se="cluster",
                 order = c("ai", "pdsi", "spei","dist","contig", "mig_interval", "year" ),
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models

write.csv(models, "table_robustness_results weighted by number of destination regions2.csv")





###
### SUPPLEMENT ROBUSTNESS - TABLE S26: 5 YEARS PRIOR TO CENSUS INSTEAD OF 10 ------
### 


m0 <-  fepois(flow_out_rate_annual ~  
                worldregion*as.factor(year_cat5)+
                log(dist)+contig+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              data=d) 
summary(m0, se = "cluster")

m1a <-  fepois(flow_out_rate_annual ~ 
                 orig_ai_mean5_stan1 +
                 dest_ai_mean5_stan1 +
                 worldregion*as.factor(year_cat5)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m1a, se = "cluster")

m2a <-  fepois(flow_out_rate_annual ~ 
                 orig_pdsi_mean5_stan1 +
                 dest_pdsi_mean5_stan1 +
                 worldregion*as.factor(year_cat5)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m2a, se = "cluster")

m3a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei03_mean5_stan1 + 
                 dest_spei03_mean5_stan1 + 
                 worldregion*as.factor(year_cat5)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m3a, se = "cluster")

m4a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei12_mean5_stan1+ 
                 dest_spei12_mean5_stan1+ 
                 worldregion*as.factor(year_cat5)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m4a, se = "cluster")

models <- etable(m0, m1a, m2a,m3a,m4a,
                 keep = c("orig_ai_mean5_stan1","orig_spei12_mean5_stan1",
                          "orig_spei03_mean5_stan1", "orig_pdsi_mean5_stan1",
                          "mig_interval", "dist", "contig"),
                 se="cluster",
                 order = c("ai", "pdsi", "spei", "dist","contig", "mig_interval", "year" ),
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models

write.csv(models, "table_robustness_time interval of 5 years instead of 10 before census2.csv")



###
### SUPPLEMENT ROBUSTNESS - TABLE S27:  6 to 10 YEARS PRIOR TO CENSUS INSTEAD OF 10 -----
### 


m0 <-  fepois(flow_out_rate_annual ~  
                worldregion*as.factor(year_cat10)+
                log(dist)+contig+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              data=d) 
summary(m0, se = "cluster")

m1a <-  fepois(flow_out_rate_annual ~ 
                 orig_ai_mean6_10_stan1 +
                 dest_ai_mean6_10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m1a, se = "cluster")

m2a <-  fepois(flow_out_rate_annual ~ 
                 orig_pdsi_mean6_10_stan1 +
                 dest_pdsi_mean6_10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m2a, se = "cluster")

m3a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei03_mean6_10_stan1 + 
                 dest_spei03_mean6_10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m3a, se = "cluster")

m4a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei12_mean6_10_stan1+ 
                 dest_spei12_mean6_10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m4a, se = "cluster")

models <- etable(m0, m1a, m2a,m3a,m4a,
                 keep = c("orig_ai_mean6_10_stan1","orig_pdsi_mean6_10_stan1",
                          "orig_spei03_mean6_10_stan1", "orig_spei12_mean6_10_stan1",
                          "dest_ai_mean6_10_stan1","dest_pdsi_mean6_10_stan1",
                          "dest_spei03_mean6_10_stan1", "dest_spei12_mean6_10_stan1",
                          "dist", "contig", "mig_interval" ),
                 se="cluster",
                 order = c("ai", "pdsi", "spei", "dist","contig", "mig_interval", "year" ),
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models


write.csv(models, "table_robustness_time interval of 6-10 years instead of 10 before census2.csv")


###
### SUPPLEMENT ROBUSTNESS - TABLE S28:  20 YEARS PRIOR TO CENSUS INSTEAD OF 10 -------
### 


m0 <-  fepois(flow_out_rate_annual ~  
                worldregion*as.factor(year_cat10)+
                log(dist)+contig+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              data=d) 
summary(m0, se = "cluster")

m1a <-  fepois(flow_out_rate_annual ~ 
                 orig_ai_mean20_stan1 +
                 dest_ai_mean20_stan1 +
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m1a, se = "cluster")

m2a <-  fepois(flow_out_rate_annual ~ 
                 orig_pdsi_mean20_stan1 +
                 dest_pdsi_mean20_stan1 +
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m2a, se = "cluster")

m3a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei03_mean20_stan1 + 
                 dest_spei03_mean20_stan1 + 
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m3a, se = "cluster")

m4a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei12_mean20_stan1+ 
                 dest_spei12_mean20_stan1+ 
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m4a, se = "cluster")

models <- etable(m0, m1a, m2a,m3a,m4a,
                 keep = c("orig_ai_mean20_stan1","orig_spei12_mean20_stan1",
                          "orig_spei03_mean20_stan1", "orig_pdsi_mean20_stan1",
                          "mig_interval", "dist", "contig"),
                 se="cluster",
                 order = c("ai", "pdsi", "spei", "dist","contig", "mig_interval", "year" ),
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models

write.csv(models, "table_robustness_time interval of 20 years instead of 10 before census2.csv")


##
## SUPPLEMENT ROBUSTNESS - TABLE S29: CONTROLLING FOR LAST YEAR VALUES ---------
## 



m0 <-  fepois(flow_out_rate_annual ~  
                worldregion*as.factor(year_cat10)+
                log(dist)+contig+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              data=d) 
summary(m0, se = "cluster")

m1a <-  fepois(flow_out_rate_annual ~ 
                 orig_ai_mean10_stan1 +
                 orig_ai_m1_stan1+
                 dest_ai_mean10_stan1 +
                 dest_ai_m1_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m1a, se = "cluster")

m2a <-  fepois(flow_out_rate_annual ~ 
                 orig_pdsi_mean10_stan1 +
                 orig_pdsi_m1_stan1+
                 dest_pdsi_mean10_stan1 +
                 dest_pdsi_m1_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m2a, se = "cluster")

m3a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei03_mean10_stan1 + 
                 orig_spei03_m1_stan1+
                 dest_spei03_mean10_stan1 + 
                 dest_spei03_m1_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m3a, se = "cluster")

m4a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei12_mean10_stan1+ 
                 orig_spei12_m1_stan1+
                 dest_spei12_mean10_stan1+ 
                 dest_spei12_m1_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m4a, se = "cluster")


models <- etable(m0, m1a, m2a,m3a,m4a,
                 keep = c("orig_ai_mean10_stan1","orig_spei12_mean10_stan1",
                          "orig_spei03_mean10_stan1", "orig_pdsi_mean10_stan1",
                          "orig_ai_m1", "orig_spei12_m1", "orig_spei03_m1", "orig_pdsi_m1",
                          "dist", "contig", "mig_interval"),
                 se="cluster",
                 order = c("orig_ai", "pdsi",  "spei", "orig_ndvi", "mig_interval", "orig_no", "year" ),
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models

write.csv2(models, "table_robustness_controlling for last year values2.csv")




###
### SUPPLEMENT ROBUSTNESS - TABLE S30:  FIXED EFFECTS SEPARATE FOR ORIGIN AND DESTINATION PAIRS ------- 
### 


m0 <-  fepois(flow_out_rate_annual ~  
                worldregion*as.factor(year_cat10)+
                as.factor(mig_interval)+
                log(dist)+as.factor(contig),
              fixef = c("orig", "dest"),
              cluster="orig_dest_dyad",
              data=d) 
summary(m0, se = "cluster")

m1a <-  fepois(flow_out_rate_annual ~ 
                 orig_ai_mean10_stan1 +
                 dest_ai_mean10_stan1 +
                 worldregion*as.factor(year_cat10)+
                 as.factor(mig_interval)+
                 log(dist)+as.factor(contig),
               fixef = c("orig", "dest"),
               cluster="orig_dest_dyad",
               data=d) 
summary(m1a, se = "cluster")

m2a <-  fepois(flow_out_rate_annual ~ 
                 orig_pdsi_mean10_stan1 +
                 dest_pdsi_mean10_stan1 +
                 worldregion*as.factor(year_cat10)+
                 as.factor(mig_interval)+
                 log(dist)+as.factor(contig),
               fixef = c("orig", "dest"),
               cluster="orig_dest_dyad",
               data=d) 
summary(m2a, se = "cluster")

m3a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei03_mean10_stan1 + 
                 dest_spei03_mean10_stan1 + 
                 worldregion*as.factor(year_cat10)+
                 as.factor(mig_interval)+
                 log(dist)+as.factor(contig),
               fixef = c("orig", "dest"),
               cluster="orig_dest_dyad",
               data=d) 
summary(m3a, se = "cluster")

m4a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei12_mean10_stan1+ 
                 dest_spei12_mean10_stan1+ 
                 worldregion*as.factor(year_cat10)+
                 as.factor(mig_interval)+
                 log(dist)+as.factor(contig),
               cluster="orig_dest_dyad",
               fixef = c("orig", "dest"),
               data=d) 
summary(m4a, se = "cluster")

models <- etable(m0, m1a, m2a,m3a,m4a,
                 keep = c("orig_ai_mean10_stan1","orig_spei12_mean10_stan1",
                          "orig_spei03_mean10_stan1", "orig_pdsi_mean10_stan1",
                          "dest_ai_mean10_stan1","dest_spei12_mean10_stan1",
                          "dest_spei03_mean10_stan1", "dest_pdsi_mean10_stan1",
                          "mig_interval", "dist", "contig"),
                 se="cluster",
                 cluster="orig_dest_dyad",
                 order = c("ai", "pdsi", "spei","dist","contig", "mig_interval", "year" ),
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models

write.csv(models, "table_robustnesses_fixed effects at the origin and destination pair level2.csv")




###
### SUPPLEMENT ROBUSTNESS - TABLE S31: WITHOUT WORLD REGIONS SPECIFIC TIME TRENDS --------
### 

m0 <-  fepois(flow_out_rate_annual ~  
                as.factor(year_cat10)+
                log(dist)+contig+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              data=d) 
summary(m0, se = "cluster")

m1a <-  fepois(flow_out_rate_annual ~ 
                 orig_ai_mean10_stan1 +
                 dest_ai_mean10_stan1 +
                 as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m1a, se = "cluster")

m2a <-  fepois(flow_out_rate_annual ~ 
                 orig_pdsi_mean10_stan1 +
                 dest_pdsi_mean10_stan1 +
                 as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m2a, se = "cluster")

m3a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei03_mean10_stan1 + 
                 dest_spei03_mean10_stan1 + 
                 as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m3a, se = "cluster")

m4a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei12_mean10_stan1+ 
                 dest_spei12_mean10_stan1+ 
                 as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m4a, se = "cluster")

models <- etable(m0, m1a, m2a,m3a,m4a,
                 keep = c("orig_ai_mean10_stan1","orig_spei12_mean10_stan1",
                          "orig_spei03_mean10_stan1", "orig_pdsi_mean10_stan1",
                          "mig_interval", "dist", "contig"),
                 se="cluster",
                 order = c("ai", "pdsi", "spei", "dist","contig", "mig_interval", "year" ),
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models

write.csv(models, "table_robustness_no world region specific time trends controlled for2.csv")



##
## SUPPLEMENT ROBUSTNESS - TABLE S32: CHECKING FOR COUNTRY SPECIFIC SPECIFIC TIME TRENDS ------
## 

m0 <-  fepois(flow_out_rate_annual ~  
                alpha3*as.factor(year_cat10)+
                log(dist)+contig+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              data=d) 
summary(m0, se = "cluster")

m1a <-  fepois(flow_out_rate_annual ~ 
                 orig_ai_mean10_stan1 +
                 dest_ai_mean10_stan1 +
                 alpha3*(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m1a, se = "cluster")

m2a <-  fepois(flow_out_rate_annual ~ 
                 orig_pdsi_mean10_stan1 +
                 dest_pdsi_mean10_stan1 +
                 alpha3*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m2a, se = "cluster")

m3a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei03_mean10_stan1 + 
                 dest_spei03_mean10_stan1 + 
                 alpha3*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m3a, se = "cluster")

m4a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei12_mean10_stan1+ 
                 dest_spei12_mean10_stan1+ 
                 alpha3*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m4a, se = "cluster")

models <- etable(m0, m1a, m2a,m3a,m4a,
                 keep = c("orig_ai_mean10_stan1","orig_spei12_mean10_stan1",
                          "orig_spei03_mean10_stan1", "orig_pdsi_mean10_stan1",
                          "mig_interval", "dist", "contig"),
                 se="cluster",
                 order = c("ai", "pdsi", "spei","dist","contig", "mig_interval", "year" ),
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models

write.csv(models, "table_robustness_country specific time trends controlled for2.csv")


##
## SUPPLEMENT ROBUSTNESS - TABLE S33: CONTROLING FOR 5 YEAR TIME WINDOWS INSTEAD OF 10 YEARS -------
## 

m0 <-  fepois(flow_out_rate_annual ~  
                worldregion*as.factor(year_cat5)+
                log(dist)+contig+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              data=d) 
summary(m0, se = "cluster")

m1a <-  fepois(flow_out_rate_annual ~ 
                 orig_ai_mean10_stan1 +
                 dest_ai_mean10_stan1 +
                 worldregion*(year_cat5)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m1a, se = "cluster")

m2a <-  fepois(flow_out_rate_annual ~ 
                 orig_pdsi_mean10_stan1 +
                 dest_pdsi_mean10_stan1 +
                 worldregion*(year_cat5)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m2a, se = "cluster")

m3a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei03_mean10_stan1 + 
                 dest_spei03_mean10_stan1 + 
                 worldregion*(year_cat5)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m3a, se = "cluster")

m4a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei12_mean10_stan1+ 
                 dest_spei12_mean10_stan1+ 
                 worldregion*(year_cat5)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m4a, se = "cluster")

models <- etable(m0, m1a, m2a,m3a,m4a,
                 keep = c("orig_ai_mean10_stan1","orig_spei12_mean10_stan1",
                          "orig_spei03_mean10_stan1", "orig_pdsi_mean10_stan1",
                          "mig_interval", "dist", "contig"),
                 se="cluster",
                 order = c("ai", "pdsi", "spei", "dist","contig", "mig_interval", "year" ),
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models

write.csv(models, "table_robustness_5 year dummy time trends controlled for2.csv")



###
### SUPPLEMENT ROBUSTNESS - TABLE S34:  STANDARDIZATION AT THE COUNTRY LEVEL -------
### 


m0 <-  fepois(flow_out_rate_annual ~  
                worldregion*as.factor(year_cat10)+
                log(dist)+contig+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              data=d) 
summary(m0, se = "cluster")

m1a <-  fepois(flow_out_rate_annual ~ 
                 orig_ai_mean10_stan2 +
                 dest_ai_mean10_stan2 +
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m1a, se = "cluster")

m2a <-  fepois(flow_out_rate_annual ~ 
                 orig_pdsi_mean10_stan2 +
                 dest_pdsi_mean10_stan2 +
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m2a, se = "cluster")

m3a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei03_mean10_stan2 + 
                 dest_spei03_mean10_stan2 + 
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m3a, se = "cluster")

m4a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei12_mean10_stan2+ 
                 dest_spei12_mean10_stan2+ 
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m4a, se = "cluster")

models <- etable(m0, m1a, m2a,m3a,m4a,
                 keep = c("orig_ai_mean10_stan2","orig_spei12_mean10_stan2",
                          "orig_spei03_mean10_stan2", "orig_pdsi_mean10_stan2",
                          "mig_interval", "dist", "contig"),
                 se="cluster",
                 order = c("ai", "pdsi", "spei", "dist","contig", "mig_interval", "year" ),
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models

write.csv(models, "table_robustness_standardization at the country level2.csv")



###
### SUPPLEMENT ROBUSTNESS - TABLE S35: STANDARDIZATION AT THE GLOBAL LEVEL ------
### 


m0 <-  fepois(flow_out_rate_annual ~  
                worldregion*as.factor(year_cat10)+
                log(dist)+contig+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              data=d) 
summary(m0, se = "cluster")

m1a <-  fepois(flow_out_rate_annual ~ 
                 orig_ai_mean10_stan4 +
                 dest_ai_mean10_stan4 +
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m1a, se = "cluster")

m2a <-  fepois(flow_out_rate_annual ~ 
                 orig_pdsi_mean10_stan4 +
                 dest_pdsi_mean10_stan4 +
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m2a, se = "cluster")

m3a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei03_mean10_stan4 + 
                 dest_spei03_mean10_stan4 + 
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m3a, se = "cluster")

m4a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei12_mean10_stan4+ 
                 dest_spei12_mean10_stan4+ 
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m4a, se = "cluster")

models <- etable(m0, m1a, m2a,m3a,m4a,
                 keep = c("orig_ai_mean10_stan4","orig_spei12_mean10_stan4",
                          "orig_spei03_mean10_stan4", "orig_pdsi_mean10_stan4",
                          "mig_interval", "dist", "contig"),
                 se="cluster",
                 order = c("ai", "pdsi", "spei", "dist","contig", "mig_interval", "year" ),
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models

write.csv(models, "table_robustness_standardization at the global level2.csv")



##
## SUPPLEMENT ROBUSTNESS - TABLE S36:  UNSTANDARDIZED CLIMATE MEASURES  --------
## 

d <- d %>% 
  mutate(orig_ai_mean10=-1*orig_ai_mean10,
         orig_spei12_mean10=-1*orig_spei12_mean10,
         orig_spei03_mean10=-1*orig_spei03_mean10,
         orig_pdsi_mean10=-1*orig_pdsi_mean10)


m0 <-  fepois(flow_out_rate_annual ~  
                worldregion*as.factor(year_cat10)+
                log(dist)+contig+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              data=d) 
summary(m0, se = "cluster")

m1a <-  fepois(flow_out_rate_annual ~ 
                 orig_ai_mean10 +
                 dest_ai_mean10 +
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m1a, se = "cluster")

m2a <-  fepois(flow_out_rate_annual ~ 
                 orig_pdsi_mean10 +
                 dest_pdsi_mean10 +
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m2a, se = "cluster")

m3a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei03_mean10 + 
                 dest_spei03_mean10 + 
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m3a, se = "cluster")

m4a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei12_mean10+ 
                 dest_spei12_mean10+ 
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m4a, se = "cluster")

models <- etable(m0, m1a, m2a,m3a,m4a,
                 keep = c("orig_ai_mean10","orig_spei12_mean10",
                          "orig_spei03_mean10", "orig_pdsi_mean10",
                          "mig_interval"),
                 se="cluster",
                 order = c("ai", "pdsi", "spei","dist","contig", "mig_interval", "year" ),
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models

write.csv(models, "table_robustness_unstandardized climate measures2.csv")




##
## SUPPLEMENT ROBUSTNESS - TABLE S37:  MOST EXTREME VALUES INSTEAD OF MEAN------
## 

m0 <-  fepois(flow_out_rate_annual ~  
                worldregion*as.factor(year_cat10)+
                log(dist)+contig+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              data=d) 
summary(m0, se = "cluster")

m1a <-  fepois(flow_out_rate_annual ~ 
                 orig_ai_min10_stan1 +
                 dest_ai_min10_stan1 +
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m1a, se = "cluster")

m2a <-  fepois(flow_out_rate_annual ~ 
                 orig_pdsi_min10_stan1 +
                 dest_pdsi_min10_stan1 +
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m2a, se = "cluster")

m3a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei03_min10_stan1 + 
                 dest_spei03_min10_stan1 + 
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m3a, se = "cluster")

m4a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei12_min10_stan1+ 
                 dest_spei12_min10_stan1+ 
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m4a, se = "cluster")

models <- etable(m0, m1a, m2a,m3a,m4a,
                 keep = c("orig_ai_min10_stan1","orig_spei12_min10_stan1",
                          "orig_spei03_min10_stan1", "orig_pdsi_min10_stan1",
                          "mig_interval", "dist", "contig"),
                 se="cluster",
                 order = c("ai", "pdsi", "spei", "dist","contig", "mig_interval", "year" ),
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models

write.csv(models, "table_robustness_most extreme valules instead of average2.csv")




###
### SUPPLEMENT ROBUSTNESS - TABLE S38:  TEMPERATURE AND PRECIPITATION ANOMALY ------
### 

m0 <-  fepois(flow_out_rate_annual ~  
                worldregion*as.factor(year_cat10)+
                log(dist)+contig+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              data=d) 
summary(m0, se = "cluster")

m1a <-  fepois(flow_out_rate_annual ~ 
                 orig_pre_anomaly_mean10 +
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m1a, se = "cluster")

m2a <-  fepois(flow_out_rate_annual ~ 
                 orig_tmp_anomaly_mean10+           +
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m2a, se = "cluster")

m3a <-  fepois(flow_out_rate_annual ~ 
                 orig_pre_anomaly_mean10 + 
                 orig_tmp_anomaly_mean10+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m3a, se = "cluster")

m4a <-  fepois(flow_out_rate_annual ~ 
                 orig_pre_anomaly_mean10 + 
                 orig_tmp_anomaly_mean10+
                 dest_pre_anomaly_mean10 + 
                 dest_tmp_anomaly_mean10+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m4a, se = "cluster")

models <- etable(m0, m1a, m2a,m3a,m4a,
                 keep = c("orig_tmp_anomaly_mean10","orig_pre_anomaly_mean10",
                          "dest_tmp_anomaly_mean10","dest_pre_anomaly_mean10",
                          "dist", "contig", "mig_interval" ),
                 se="cluster",
                 order = c("ai", "pdsi", "spei", "dist","contig", "mig_interval", "year" ),
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models

write.csv(models, "table_robustness_temperature and precipitation anomalies.csv")




###
###  SUPPLEMENT ROBUSTNESS - TABLE S39 & S40: DIFFERENT CLUSTERING -------------
### 

d <- d %>% 
  mutate(country_decade = paste(alpha3, decade, sep=""),
         worldregion_decade = paste(worldregion, decade, sep=""))

m1a <-  fepois(flow_out_rate_annual ~ 
                 orig_ai_mean10_stan1 +
                 dest_ai_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               weights = log(d$orig_pop),
               data=d) 
summary(m1a, cluster=c("orig_dest_dyad", "decade"))

m2a <-  fepois(flow_out_rate_annual ~ 
                 orig_pdsi_mean10_stan1 +
                 dest_pdsi_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               weights = log(d$orig_pop),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m2a, cluster=c("orig_dest_dyad", "country_decade"))

m3a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei03_mean10_stan1 + 
                 dest_spei03_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               weights = log(d$orig_pop),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m3a, cluster=c("orig_dest_dyad", "decade"))

m4a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei12_mean10_stan1+ 
                 dest_spei12_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               weights = log(d$orig_pop),
               data=d) 


summary(m4a, cluster=c("orig_dest_dyad", "decade"))

models <- etable( m1a, m2a,m3a,m4a,
                  keep = c("orig_ai_mean10_stan1","orig_spei12_mean10_stan1",
                           "orig_spei03_mean10_stan1", "orig_pdsi_mean10_stan1",
                           "dest_ai_mean10_stan1","dest_spei12_mean10_stan1",
                           "dest_spei03_mean10_stan1", "dest_pdsi_mean10_stan1",
                           "dist", "contig", "mig_interval" ),
                  cluster=c("orig_dest_dyad", "decade"),
                  order = c("ai", "pdsi", "spei", "dist","contig", "mig_interval", "year" ),
                  signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models


write.csv(models, "table_robustness_temporal and spatial clustering separate.csv")


models <- etable( m1a, m2a,m3a,m4a,
                  keep = c("orig_ai_mean10_stan1","orig_spei12_mean10_stan1",
                           "orig_spei03_mean10_stan1", "orig_pdsi_mean10_stan1",
                           "dest_ai_mean10_stan1","dest_spei12_mean10_stan1",
                           "dest_spei03_mean10_stan1", "dest_pdsi_mean10_stan1",
                           "dist", "contig", "mig_interval" ),
                  cluster=c("orig_dest_dyad", "country_decade"),
                  order = c("ai", "pdsi", "spei", "dist","contig", "mig_interval", "year" ),
                  signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models


write.csv(models, "table_robustness_temporal and spatial clustering with country time trends.csv")








