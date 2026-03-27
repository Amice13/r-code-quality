
### ----------------------------------------------------------------------------
### CLIMATE CHANGE, ARIDITY, AND INTERNAL MIGRATION 
### ----------------------------------------------------------------------------

### 
### EXTENDED MODELS BY AGE AND SEX ---------------------------------------------
###  

rm(list=ls())

###
### LOAD PACKAGES --------------------------------------------------------------
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

d.sexage <- d.sexage %>% filter(mig_interval != 10)

#> restricting data to cases for which we have observations for all sex/age groups
d.sexage <- d.sexage %>% 
  filter(!is.na(flow_out_rate_annual_female015)&
           !is.na(flow_out_rate_annual_female1520)&
           !is.na(flow_out_rate_annual_female2025)&
           !is.na(flow_out_rate_annual_female2530)&
           !is.na(flow_out_rate_annual_female3045)&
           !is.na(flow_out_rate_annual_female4560)&
           !is.na(flow_out_rate_annual_female60)&
           !is.na(flow_out_rate_annual_male015)&
           !is.na(flow_out_rate_annual_male1520)&
           !is.na(flow_out_rate_annual_male2025)&
           !is.na(flow_out_rate_annual_male2530)&
           !is.na(flow_out_rate_annual_male3045)&
           !is.na(flow_out_rate_annual_male4560)&
           !is.na(flow_out_rate_annual_male60))

#> focus on less developed countries first
aux1 <- d.sexage %>% filter(grp_un_develop=="Less")

#> include only region-region pairs that are also covered in the main migration dataset
aux1 <- aux1 %>% filter(include==1)

##
## FEMALE, AGE, AI -------------------------------------------------------------
## 

m_female015_ai_less <-  fepois(flow_out_rate_annual_female015 ~ 
                            orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                            worldregion*as.factor(year_cat10)+
                            log(dist)+contig+as.factor(mig_interval),
                          fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                         data=aux1) 

summary(m_female015_ai_less)

m_female1520_ai_less <-  fepois(flow_out_rate_annual_female1520 ~ 
                                  orig_ai_mean10_stan1+  dest_ai_mean10_stan1+ 
                                  worldregion*as.factor(year_cat10)+
                             log(dist)+contig+as.factor(mig_interval),
                           fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                           data=aux1) 



m_female2025_ai_less <-  fepois(flow_out_rate_annual_female2025 ~ 
                                  orig_ai_mean10_stan1+
                                  dest_ai_mean10_stan1+
                             worldregion*as.factor(year_cat10)+
                             log(dist)+contig+as.factor(mig_interval),
                           fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                           data=aux1) 

m_female2530_ai_less <-  fepois(flow_out_rate_annual_female2530 ~ 
                                  orig_ai_mean10_stan1+
                                  dest_ai_mean10_stan1+
                                  worldregion*as.factor(year_cat10)+
                             log(dist)+contig+as.factor(mig_interval),
                           fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                           data=aux1) 

m_female3045_ai_less <-  fepois(flow_out_rate_annual_female3045 ~ 
                                  orig_ai_mean10_stan1+
                                  dest_ai_mean10_stan1+
                                  worldregion*as.factor(year_cat10)+
                             log(dist)+contig+as.factor(mig_interval),
                           fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                           data=aux1) 

m_female4560_ai_less <-  fepois(flow_out_rate_annual_female4560 ~ 
                                  orig_ai_mean10_stan1+
                                  dest_ai_mean10_stan1+
                                  worldregion*as.factor(year_cat10)+
                             log(dist)+contig+as.factor(mig_interval),
                           fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                           data=aux1) 

m_female60_ai_less <-  fepois(flow_out_rate_annual_female60 ~ 
                                orig_ai_mean10_stan1+
                                dest_ai_mean10_stan1+
                                worldregion*as.factor(year_cat10)+
                           log(dist)+contig+as.factor(mig_interval),
                         fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                         data=aux1) 


##
## MALE, AGE, AI -------------------------------------------------------------
## 


m_male015_ai_less <-  fepois(flow_out_rate_annual_male015 ~ 
                          orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                          worldregion*as.factor(year_cat10)+
                          log(dist)+contig+as.factor(mig_interval),
                        fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                        data=aux1) 

m_male1520_ai_less <-  fepois(flow_out_rate_annual_male1520 ~ 
                           orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                           worldregion*as.factor(year_cat10)+
                           log(dist)+contig+as.factor(mig_interval),
                         fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                         data=aux1) 

m_male2025_ai_less <-  fepois(flow_out_rate_annual_male2025 ~ 
                           orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                           worldregion*as.factor(year_cat10)+
                           log(dist)+contig+as.factor(mig_interval),
                         fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                         data=aux1) 

m_male2530_ai_less <-  fepois(flow_out_rate_annual_male2530 ~ 
                           orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                           worldregion*as.factor(year_cat10)+
                           log(dist)+contig+as.factor(mig_interval),
                         fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                         data=aux1) 

m_male3045_ai_less <-  fepois(flow_out_rate_annual_male3045 ~ 
                           orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                           worldregion*as.factor(year_cat10)+
                           log(dist)+contig+as.factor(mig_interval),
                         fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                         data=aux1) 

m_male4560_ai_less <-  fepois(flow_out_rate_annual_male4560 ~ 
                           orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                           worldregion*as.factor(year_cat10)+
                           log(dist)+contig+as.factor(mig_interval),
                         fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                         data=aux1) 

m_male60_ai_less <-  fepois(flow_out_rate_annual_male60 ~ 
                         orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                         worldregion*as.factor(year_cat10)+
                         log(dist)+contig+as.factor(mig_interval),
                       fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                       data=aux1) 



##
## FEMALE, AGE, SPEI03 -------------------------------------------------------------
## 


m_female015_spei03_less <-  fepois(flow_out_rate_annual_female015 ~ 
                                orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                worldregion*as.factor(year_cat10)+
                                log(dist)+contig+as.factor(mig_interval),
                              fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                              data=aux1) 

m_female1520_spei03_less <-  fepois(flow_out_rate_annual_female1520  ~ 
                                 orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                 worldregion*as.factor(year_cat10)+
                                 log(dist)+contig+as.factor(mig_interval),
                               fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                               data=aux1) 

m_female2025_spei03_less <-  fepois(flow_out_rate_annual_female2025 ~ 
                                 orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                 worldregion*as.factor(year_cat10)+
                                 log(dist)+contig+as.factor(mig_interval),
                               fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                               data=aux1) 

m_female2530_spei03_less <-  fepois(flow_out_rate_annual_female2530  ~ 
                                 orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                 worldregion*as.factor(year_cat10)+
                                 log(dist)+contig+as.factor(mig_interval),
                               fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                               data=aux1) 

m_female3045_spei03_less <-  fepois(flow_out_rate_annual_female3045 ~ 
                                 orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                 worldregion*as.factor(year_cat10)+
                                 log(dist)+contig+as.factor(mig_interval),
                               fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                               data=aux1) 

m_female4560_spei03_less <-  fepois(flow_out_rate_annual_female4560  ~ 
                                 orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                 worldregion*as.factor(year_cat10)+
                                 log(dist)+contig+as.factor(mig_interval),
                               fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                               data=aux1) 

m_female60_spei03_less <-  fepois(flow_out_rate_annual_female60  ~ 
                               orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                               worldregion*as.factor(year_cat10)+
                               log(dist)+contig+as.factor(mig_interval),
                             fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                             data=aux1) 


##
## MALE, AGE, SPEI03 -------------------------------------------------------------
## 


m_male015_spei03_less <-  fepois(flow_out_rate_annual_male015  ~ 
                              orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                              worldregion*as.factor(year_cat10)+
                              log(dist)+contig+as.factor(mig_interval),
                            fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                            data=aux1) 

m_male1520_spei03_less <-  fepois(flow_out_rate_annual_male1520  ~ 
                               orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                               worldregion*as.factor(year_cat10)+
                               log(dist)+contig+as.factor(mig_interval),
                             fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                             data=aux1) 

m_male2025_spei03_less <-  fepois(flow_out_rate_annual_male2025  ~ 
                               orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                               worldregion*as.factor(year_cat10)+
                               log(dist)+contig+as.factor(mig_interval),
                             fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                             data=aux1) 

m_male2530_spei03_less <-  fepois(flow_out_rate_annual_male2530  ~ 
                               orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                               worldregion*as.factor(year_cat10)+
                               log(dist)+contig+as.factor(mig_interval),
                             fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                             data=aux1) 

m_male3045_spei03_less <-  fepois(flow_out_rate_annual_male3045  ~ 
                               orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                               worldregion*as.factor(year_cat10)+
                               log(dist)+contig+as.factor(mig_interval),
                             fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                             data=aux1) 

m_male4560_spei03_less <-  fepois(flow_out_rate_annual_male4560  ~ 
                               orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                               worldregion*as.factor(year_cat10)+
                               log(dist)+contig+as.factor(mig_interval),
                             fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                             data=aux1) 

m_male60_spei03_less <-  fepois(flow_out_rate_annual_male60  ~ 
                             orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                             worldregion*as.factor(year_cat10)+
                             log(dist)+contig+as.factor(mig_interval),
                           fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                           data=aux1) 



##
## FEMALE, AGE, SPEI12 -------------------------------------------------------------
## 


m_female015_spei12_less <-  fepois(flow_out_rate_annual_female015 ~ 
                                orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                worldregion*as.factor(year_cat10)+
                                log(dist)+contig+as.factor(mig_interval),
                              fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                              data=aux1) 

m_female1520_spei12_less <-  fepois(flow_out_rate_annual_female1520 ~ 
                                 orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                 worldregion*as.factor(year_cat10)+
                                 log(dist)+contig+as.factor(mig_interval),
                               fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                               data=aux1) 



m_female2025_spei12_less <-  fepois(flow_out_rate_annual_female2025~ 
                                 orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                 worldregion*as.factor(year_cat10)+
                                 log(dist)+contig+as.factor(mig_interval),
                               fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                               data=aux1) 

m_female2530_spei12_less <-  fepois(flow_out_rate_annual_female2530 ~ 
                                 orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                 worldregion*as.factor(year_cat10)+
                                 log(dist)+contig+as.factor(mig_interval),
                               fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                               data=aux1) 

m_female3045_spei12_less <-  fepois(flow_out_rate_annual_female3045~ 
                                 orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                 worldregion*as.factor(year_cat10)+
                                 log(dist)+contig+as.factor(mig_interval),
                               fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                               data=aux1) 

m_female4560_spei12_less <-  fepois(flow_out_rate_annual_female4560~ 
                                 orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                 worldregion*as.factor(year_cat10)+
                                 log(dist)+contig+as.factor(mig_interval),
                               fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                               data=aux1) 

m_female60_spei12_less <-  fepois(flow_out_rate_annual_female60 ~ 
                               orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                               worldregion*as.factor(year_cat10)+
                               log(dist)+contig+as.factor(mig_interval),
                             fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                             data=aux1) 


##
## MALE, AGE, spei12 -------------------------------------------------------------
## 


m_male015_spei12_less <-  fepois(flow_out_rate_annual_male015 ~ 
                              orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                              worldregion*as.factor(year_cat10)+
                              log(dist)+contig+as.factor(mig_interval),
                            fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                            data=aux1) 

m_male1520_spei12_less <-  fepois(flow_out_rate_annual_male1520 ~ 
                               orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                               worldregion*as.factor(year_cat10)+
                               log(dist)+contig+as.factor(mig_interval),
                             fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                             data=aux1) 

m_male2025_spei12_less <-  fepois(flow_out_rate_annual_male2025~ 
                               orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                               worldregion*as.factor(year_cat10)+
                               log(dist)+contig+as.factor(mig_interval),
                             fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                             data=aux1) 

m_male2530_spei12_less <-  fepois(flow_out_rate_annual_male2530 ~ 
                               orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                               worldregion*as.factor(year_cat10)+
                               log(dist)+contig+as.factor(mig_interval),
                             fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                             data=aux1) 

m_male3045_spei12_less <-  fepois(flow_out_rate_annual_male3045 ~ 
                               orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                               worldregion*as.factor(year_cat10)+
                               log(dist)+contig+as.factor(mig_interval),
                             fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                             data=aux1) 

m_male4560_spei12_less <-  fepois(flow_out_rate_annual_male4560 ~ 
                               orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                               worldregion*as.factor(year_cat10)+
                               log(dist)+contig+as.factor(mig_interval),
                             fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                             data=aux1) 

m_male60_spei12_less <-  fepois(flow_out_rate_annual_male60 ~ 
                             orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                             worldregion*as.factor(year_cat10)+
                             log(dist)+contig+as.factor(mig_interval),
                           fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                           data=aux1) 

##
## FEMALE, AGE, PDSI -------------------------------------------------------------
## 


m_female015_pdsi_less <-  fepois(flow_out_rate_annual_female015 ~ 
                              orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                              worldregion*as.factor(year_cat10)+
                              log(dist)+contig+as.factor(mig_interval),
                            fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                            data=aux1) 

m_female1520_pdsi_less <-  fepois(flow_out_rate_annual_female1520~ 
                               orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                               worldregion*as.factor(year_cat10)+
                               log(dist)+contig+as.factor(mig_interval),
                             fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                             data=aux1) 



m_female2025_pdsi_less <-  fepois(flow_out_rate_annual_female2025 ~ 
                               orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                               worldregion*as.factor(year_cat10)+
                               log(dist)+contig+as.factor(mig_interval),
                             fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                             data=aux1) 

m_female2530_pdsi_less <-  fepois(flow_out_rate_annual_female2530 ~ 
                               orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                               worldregion*as.factor(year_cat10)+
                               log(dist)+contig+as.factor(mig_interval),
                             fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                             data=aux1) 

m_female3045_pdsi_less <-  fepois(flow_out_rate_annual_female3045 ~ 
                               orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                               worldregion*as.factor(year_cat10)+
                               log(dist)+contig+as.factor(mig_interval),
                             fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                             data=aux1) 

m_female4560_pdsi_less <-  fepois(flow_out_rate_annual_female4560 ~ 
                               orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                               worldregion*as.factor(year_cat10)+
                               log(dist)+contig+as.factor(mig_interval),
                             fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                             data=aux1) 

m_female60_pdsi_less <-  fepois(flow_out_rate_annual_female60 ~ 
                             orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                             worldregion*as.factor(year_cat10)+
                             log(dist)+contig+as.factor(mig_interval),
                           fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                           data=aux1) 


##
## MALE, AGE, pdsi -------------------------------------------------------------
## 


m_male015_pdsi_less <-  fepois(flow_out_rate_annual_male015 ~ 
                            orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                            worldregion*as.factor(year_cat10)+
                            log(dist)+contig+as.factor(mig_interval),
                          fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                          data=aux1) 

m_male1520_pdsi_less <-  fepois(flow_out_rate_annual_male1520 ~ 
                             orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                             worldregion*as.factor(year_cat10)+
                             log(dist)+contig+as.factor(mig_interval),
                           fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                           data=aux1) 

m_male2025_pdsi_less <-  fepois(flow_out_rate_annual_male2025 ~ 
                             orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                             worldregion*as.factor(year_cat10)+
                             log(dist)+contig+as.factor(mig_interval),
                           fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                           data=aux1) 

m_male2530_pdsi_less <-  fepois(flow_out_rate_annual_male2530 ~ 
                             orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                             worldregion*as.factor(year_cat10)+
                             log(dist)+contig+as.factor(mig_interval),
                           fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                           data=aux1) 

m_male3045_pdsi_less <-  fepois(flow_out_rate_annual_male3045 ~ 
                             orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                             worldregion*as.factor(year_cat10)+
                             log(dist)+contig+as.factor(mig_interval),
                           fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                           data=aux1) 

m_male4560_pdsi_less <-  fepois(flow_out_rate_annual_male4560 ~ 
                             orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                             worldregion*as.factor(year_cat10)+
                             log(dist)+contig+as.factor(mig_interval),
                           fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                           data=aux1) 

m_male60_pdsi_less <-  fepois(flow_out_rate_annual_male60 ~ 
                           orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                           worldregion*as.factor(year_cat10)+
                           log(dist)+contig+as.factor(mig_interval),
                         fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                         data=aux1) 


##
## MORE DEVELOPED  -------------------------------------------------------------
## 

#> focus on more developed countries 
aux2 <- d.sexage %>% filter(grp_un_develop=="More")

#> include only region-region pairs that are also covered in the main migration dataset
aux2 <- aux2 %>% filter(include==1)

##
## FEMALE, AGE, AI -------------------------------------------------------------
## 

m_female015_ai_more <-  fepois(flow_out_rate_annual_female015 ~ 
                                 orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                                 worldregion*as.factor(year_cat10)+
                                 log(dist)+contig+as.factor(mig_interval),
                               fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                               data=aux2) 

m_female1520_ai_more <-  fepois(flow_out_rate_annual_female1520 ~ 
                                  orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                                  worldregion*as.factor(year_cat10)+
                                  log(dist)+contig+as.factor(mig_interval),
                                fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                data=aux2) 



m_female2025_ai_more <-  fepois(flow_out_rate_annual_female2025 ~ 
                                  orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                                  worldregion*as.factor(year_cat10)+
                                  log(dist)+contig+as.factor(mig_interval),
                                fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                data=aux2) 

m_female2530_ai_more <-  fepois(flow_out_rate_annual_female2530 ~ 
                                  orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                                  worldregion*as.factor(year_cat10)+
                                  log(dist)+contig+as.factor(mig_interval),
                                fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                data=aux2) 

m_female3045_ai_more <-  fepois(flow_out_rate_annual_female3045 ~ 
                                  orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                                  worldregion*as.factor(year_cat10)+
                                  log(dist)+contig+as.factor(mig_interval),
                                fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                data=aux2) 

m_female4560_ai_more <-  fepois(flow_out_rate_annual_female4560 ~ 
                                  orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                                  worldregion*as.factor(year_cat10)+
                                  log(dist)+contig+as.factor(mig_interval),
                                fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                data=aux2) 

m_female60_ai_more <-  fepois(flow_out_rate_annual_female60 ~ 
                                orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                                worldregion*as.factor(year_cat10)+
                                log(dist)+contig+as.factor(mig_interval),
                              fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                              data=aux2) 


##
## MALE, AGE, AI -------------------------------------------------------------
## 


m_male015_ai_more <-  fepois(flow_out_rate_annual_male015 ~ 
                               orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                               worldregion*as.factor(year_cat10)+
                               log(dist)+contig+as.factor(mig_interval),
                             fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                             data=aux2) 

m_male1520_ai_more <-  fepois(flow_out_rate_annual_male1520 ~ 
                                orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                                worldregion*as.factor(year_cat10)+
                                log(dist)+contig+as.factor(mig_interval),
                              fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                              data=aux2) 

m_male2025_ai_more <-  fepois(flow_out_rate_annual_male2025 ~ 
                                orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                                worldregion*as.factor(year_cat10)+
                                log(dist)+contig+as.factor(mig_interval),
                              fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                              data=aux2) 

m_male2530_ai_more <-  fepois(flow_out_rate_annual_male2530 ~ 
                                orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                                worldregion*as.factor(year_cat10)+
                                log(dist)+contig+as.factor(mig_interval),
                              fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                              data=aux2) 

m_male3045_ai_more <-  fepois(flow_out_rate_annual_male3045 ~ 
                                orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                                worldregion*as.factor(year_cat10)+
                                log(dist)+contig+as.factor(mig_interval),
                              fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                              data=aux2) 

m_male4560_ai_more <-  fepois(flow_out_rate_annual_male4560 ~ 
                                orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                                worldregion*as.factor(year_cat10)+
                                log(dist)+contig+as.factor(mig_interval),
                              fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                              data=aux2) 

m_male60_ai_more <-  fepois(flow_out_rate_annual_male60 ~ 
                              orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                              worldregion*as.factor(year_cat10)+
                              log(dist)+contig+as.factor(mig_interval),
                            fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                            data=aux2) 



##
## FEMALE, AGE, SPEI03 -------------------------------------------------------------
## 


m_female015_spei03_more <-  fepois(flow_out_rate_annual_female015 ~ 
                                     orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                     worldregion*as.factor(year_cat10)+
                                     log(dist)+contig+as.factor(mig_interval),
                                   fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                   data=aux2) 

m_female1520_spei03_more <-  fepois(flow_out_rate_annual_female1520  ~ 
                                      orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                      worldregion*as.factor(year_cat10)+
                                      log(dist)+contig+as.factor(mig_interval),
                                    fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                    data=aux2) 

m_female2025_spei03_more <-  fepois(flow_out_rate_annual_female2025 ~ 
                                      orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                      worldregion*as.factor(year_cat10)+
                                      log(dist)+contig+as.factor(mig_interval),
                                    fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                    data=aux2) 

m_female2530_spei03_more <-  fepois(flow_out_rate_annual_female2530  ~ 
                                      orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                      worldregion*as.factor(year_cat10)+
                                      log(dist)+contig+as.factor(mig_interval),
                                    fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                    data=aux2) 

m_female3045_spei03_more <-  fepois(flow_out_rate_annual_female3045 ~ 
                                      orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                      worldregion*as.factor(year_cat10)+
                                      log(dist)+contig+as.factor(mig_interval),
                                    fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                    data=aux2) 

m_female4560_spei03_more <-  fepois(flow_out_rate_annual_female4560  ~ 
                                      orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                      worldregion*as.factor(year_cat10)+
                                      log(dist)+contig+as.factor(mig_interval),
                                    fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                    data=aux2) 

m_female60_spei03_more <-  fepois(flow_out_rate_annual_female60  ~ 
                                    orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                    worldregion*as.factor(year_cat10)+
                                    log(dist)+contig+as.factor(mig_interval),
                                  fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                  data=aux2) 


##
## MALE, AGE, SPEI03 -------------------------------------------------------------
## 


m_male015_spei03_more <-  fepois(flow_out_rate_annual_male015  ~ 
                                   orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                   worldregion*as.factor(year_cat10)+
                                   log(dist)+contig+as.factor(mig_interval),
                                 fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                 data=aux2) 

m_male1520_spei03_more <-  fepois(flow_out_rate_annual_male1520  ~ 
                                    orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                    worldregion*as.factor(year_cat10)+
                                    log(dist)+contig+as.factor(mig_interval),
                                  fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                  data=aux2) 

m_male2025_spei03_more <-  fepois(flow_out_rate_annual_male2025  ~ 
                                    orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                    worldregion*as.factor(year_cat10)+
                                    log(dist)+contig+as.factor(mig_interval),
                                  fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                  data=aux2) 

m_male2530_spei03_more <-  fepois(flow_out_rate_annual_male2530  ~ 
                                    orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                    worldregion*as.factor(year_cat10)+
                                    log(dist)+contig+as.factor(mig_interval),
                                  fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                  data=aux2) 

m_male3045_spei03_more <-  fepois(flow_out_rate_annual_male3045  ~ 
                                    orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                    worldregion*as.factor(year_cat10)+
                                    log(dist)+contig+as.factor(mig_interval),
                                  fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                  data=aux2) 

m_male4560_spei03_more <-  fepois(flow_out_rate_annual_male4560  ~ 
                                    orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                    worldregion*as.factor(year_cat10)+
                                    log(dist)+contig+as.factor(mig_interval),
                                  fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                  data=aux2) 

m_male60_spei03_more <-  fepois(flow_out_rate_annual_male60  ~ 
                                  orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                  worldregion*as.factor(year_cat10)+
                                  log(dist)+contig+as.factor(mig_interval),
                                fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                data=aux2) 



##
## FEMALE, AGE, SPEI12 -------------------------------------------------------------
## 


m_female015_spei12_more <-  fepois(flow_out_rate_annual_female015 ~ 
                                     orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                     worldregion*as.factor(year_cat10)+
                                     log(dist)+contig+as.factor(mig_interval),
                                   fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                   data=aux2) 

m_female1520_spei12_more <-  fepois(flow_out_rate_annual_female1520 ~ 
                                      orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                      worldregion*as.factor(year_cat10)+
                                      log(dist)+contig+as.factor(mig_interval),
                                    fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                    data=aux2) 



m_female2025_spei12_more <-  fepois(flow_out_rate_annual_female2025~ 
                                      orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                      worldregion*as.factor(year_cat10)+
                                      log(dist)+contig+as.factor(mig_interval),
                                    fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                    data=aux2) 

m_female2530_spei12_more <-  fepois(flow_out_rate_annual_female2530 ~ 
                                      orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                      worldregion*as.factor(year_cat10)+
                                      log(dist)+contig+as.factor(mig_interval),
                                    fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                    data=aux2) 

m_female3045_spei12_more <-  fepois(flow_out_rate_annual_female3045~ 
                                      orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                      worldregion*as.factor(year_cat10)+
                                      log(dist)+contig+as.factor(mig_interval),
                                    fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                    data=aux2) 

m_female4560_spei12_more <-  fepois(flow_out_rate_annual_female4560~ 
                                      orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                      worldregion*as.factor(year_cat10)+
                                      log(dist)+contig+as.factor(mig_interval),
                                    fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                    data=aux2) 

m_female60_spei12_more <-  fepois(flow_out_rate_annual_female60 ~ 
                                    orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                    worldregion*as.factor(year_cat10)+
                                    log(dist)+contig+as.factor(mig_interval),
                                  fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                  data=aux2) 


##
## MALE, AGE, spei12 -------------------------------------------------------------
## 


m_male015_spei12_more <-  fepois(flow_out_rate_annual_male015 ~ 
                                   orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                   worldregion*as.factor(year_cat10)+
                                   log(dist)+contig+as.factor(mig_interval),
                                 fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                 data=aux2) 

m_male1520_spei12_more <-  fepois(flow_out_rate_annual_male1520 ~ 
                                    orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                    worldregion*as.factor(year_cat10)+
                                    log(dist)+contig+as.factor(mig_interval),
                                  fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                  data=aux2) 

m_male2025_spei12_more <-  fepois(flow_out_rate_annual_male2025~ 
                                    orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                    worldregion*as.factor(year_cat10)+
                                    log(dist)+contig+as.factor(mig_interval),
                                  fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                  data=aux2) 

m_male2530_spei12_more <-  fepois(flow_out_rate_annual_male2530 ~ 
                                    orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                    worldregion*as.factor(year_cat10)+
                                    log(dist)+contig+as.factor(mig_interval),
                                  fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                  data=aux2) 

m_male3045_spei12_more <-  fepois(flow_out_rate_annual_male3045 ~ 
                                    orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                    worldregion*as.factor(year_cat10)+
                                    log(dist)+contig+as.factor(mig_interval),
                                  fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                  data=aux2) 

m_male4560_spei12_more <-  fepois(flow_out_rate_annual_male4560 ~ 
                                    orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                    worldregion*as.factor(year_cat10)+
                                    log(dist)+contig+as.factor(mig_interval),
                                  fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                  data=aux2) 

m_male60_spei12_more <-  fepois(flow_out_rate_annual_male60 ~ 
                                  orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                  worldregion*as.factor(year_cat10)+
                                  log(dist)+contig+as.factor(mig_interval),
                                fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                data=aux2) 

##
## FEMALE, AGE, PDSI -------------------------------------------------------------
## 


m_female015_pdsi_more <-  fepois(flow_out_rate_annual_female015 ~ 
                                   orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                                   worldregion*as.factor(year_cat10)+
                                   log(dist)+contig+as.factor(mig_interval),
                                 fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                 data=aux2) 

m_female1520_pdsi_more <-  fepois(flow_out_rate_annual_female1520~ 
                                    orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                                    worldregion*as.factor(year_cat10)+
                                    log(dist)+contig+as.factor(mig_interval),
                                  fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                  data=aux2) 



m_female2025_pdsi_more <-  fepois(flow_out_rate_annual_female2025 ~ 
                                    orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                                    worldregion*as.factor(year_cat10)+
                                    log(dist)+contig+as.factor(mig_interval),
                                  fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                  data=aux2) 

m_female2530_pdsi_more <-  fepois(flow_out_rate_annual_female2530 ~ 
                                    orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                                    worldregion*as.factor(year_cat10)+
                                    log(dist)+contig+as.factor(mig_interval),
                                  fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                  data=aux2) 

m_female3045_pdsi_more <-  fepois(flow_out_rate_annual_female3045 ~ 
                                    orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                                    worldregion*as.factor(year_cat10)+
                                    log(dist)+contig+as.factor(mig_interval),
                                  fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                  data=aux2) 

m_female4560_pdsi_more <-  fepois(flow_out_rate_annual_female4560 ~ 
                                    orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                                    worldregion*as.factor(year_cat10)+
                                    log(dist)+contig+as.factor(mig_interval),
                                  fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                  data=aux2) 

m_female60_pdsi_more <-  fepois(flow_out_rate_annual_female60 ~ 
                                  orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                                  worldregion*as.factor(year_cat10)+
                                  log(dist)+contig+as.factor(mig_interval),
                                fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                data=aux2) 


##
## MALE, AGE, pdsi -------------------------------------------------------------
## 


m_male015_pdsi_more <-  fepois(flow_out_rate_annual_male015 ~ 
                                 orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                                 worldregion*as.factor(year_cat10)+
                                 log(dist)+contig+as.factor(mig_interval),
                               fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                               data=aux2) 

m_male1520_pdsi_more <-  fepois(flow_out_rate_annual_male1520 ~ 
                                  orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                                  worldregion*as.factor(year_cat10)+
                                  log(dist)+contig+as.factor(mig_interval),
                                fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                data=aux2) 

m_male2025_pdsi_more <-  fepois(flow_out_rate_annual_male2025 ~ 
                                  orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                                  worldregion*as.factor(year_cat10)+
                                  log(dist)+contig+as.factor(mig_interval),
                                fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                data=aux2) 

m_male2530_pdsi_more <-  fepois(flow_out_rate_annual_male2530 ~ 
                                  orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                                  worldregion*as.factor(year_cat10)+
                                  log(dist)+contig+as.factor(mig_interval),
                                fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                data=aux2) 

m_male3045_pdsi_more <-  fepois(flow_out_rate_annual_male3045 ~ 
                                  orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                                  worldregion*as.factor(year_cat10)+
                                  log(dist)+contig+as.factor(mig_interval),
                                fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                data=aux2) 

m_male4560_pdsi_more <-  fepois(flow_out_rate_annual_male4560 ~ 
                                  orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                                  worldregion*as.factor(year_cat10)+
                                  log(dist)+contig+as.factor(mig_interval),
                                fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                data=aux2) 

m_male60_pdsi_more <-  fepois(flow_out_rate_annual_male60 ~ 
                                orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                                worldregion*as.factor(year_cat10)+
                                log(dist)+contig+as.factor(mig_interval),
                              fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                              data=aux2) 



##
## PREPARING COEFFICIENT PLOTS BY AGE, SEX, AND ARIDITY MEASURES ---------------
## 


##
## LESS DEVELOPED --------------------------------------------------------------
## 


## Aridity ---------------------------------------------------------------------

envindicator <- "orig_ai_mean10_stan1"

# Female

m.sum <- summary(m_female015_ai_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female015_ai_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "<15",
         sex = "female", 
         aridity = "AI",
         developed = "Less developed")

m.sum <- summary(m_female1520_ai_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female1520_ai_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "15-20",
         sex = "female", 
         aridity = "AI",
         developed = "Less developed")

m.sum <- summary(m_female2025_ai_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female2025_ai_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "21-25",
         sex = "female", 
         aridity = "AI",
         developed = "Less developed")

m.sum <- summary(m_female2530_ai_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female2530_ai_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "26-30",
         sex = "female", 
         aridity = "AI",
         developed = "Less developed")

m.sum <- summary(m_female3045_ai_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female3045_ai_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "31-45",
         sex = "female", 
         aridity = "AI",
         developed = "Less developed")

m.sum <- summary(m_female4560_ai_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female4560_ai_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "46-60",
         sex = "female", 
         aridity = "AI",
         developed = "Less developed")

m.sum <- summary(m_female60_ai_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female60_ai_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = ">60",
         sex = "female", 
         aridity = "AI",
         developed = "Less developed")



# Male

m.sum <- summary(m_male015_ai_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male015_ai_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "<15",
         sex = "male", 
         aridity = "AI",
         developed = "Less developed")

m.sum <- summary(m_male1520_ai_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male1520_ai_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "15-20",
         sex = "male", 
         aridity = "AI",
         developed = "Less developed")

m.sum <- summary(m_male2025_ai_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male2025_ai_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "21-25",
         sex = "male", 
         aridity = "AI",
         developed = "Less developed")

m.sum <- summary(m_male2530_ai_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male2530_ai_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "26-30",
         sex = "male", 
         aridity = "AI",
         developed = "Less developed")

m.sum <- summary(m_male3045_ai_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male3045_ai_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "31-45",
         sex = "male", 
         aridity = "AI",
         developed = "Less developed")

m.sum <- summary(m_male4560_ai_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male4560_ai_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "46-60",
         sex = "male", 
         aridity = "AI",
         developed = "Less developed")

m.sum <- summary(m_male60_ai_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male60_ai_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = ">60",
         sex = "male", 
         aridity = "AI",
         developed = "Less developed")



## SPEI03 ---------------------------------------------------------------------

envindicator <- "orig_spei03_mean10_stan"

# Female

m.sum <- summary(m_female015_spei03_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female015_spei03_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "<15",
         sex = "female", 
         aridity = "SPEI03",
         developed = "Less developed")

m.sum <- summary(m_female1520_spei03_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female1520_spei03_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "15-20",
         sex = "female", 
         aridity = "SPEI03",
         developed = "Less developed")

m.sum <- summary(m_female2025_spei03_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female2025_spei03_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "21-25",
         sex = "female", 
         aridity = "SPEI03",
         developed = "Less developed")

m.sum <- summary(m_female2530_spei03_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female2530_spei03_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "26-30",
         sex = "female", 
         aridity = "SPEI03",
         developed = "Less developed")

m.sum <- summary(m_female3045_spei03_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female3045_spei03_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "31-45",
         sex = "female", 
         aridity = "SPEI03",
         developed = "Less developed")

m.sum <- summary(m_female4560_spei03_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female4560_spei03_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "46-60",
         sex = "female", 
         aridity = "SPEI03",
         developed = "Less developed")

m.sum <- summary(m_female60_spei03_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female60_spei03_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = ">60",
         sex = "female", 
         aridity = "SPEI03",
         developed = "Less developed")



# Male

m.sum <- summary(m_male015_spei03_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male015_spei03_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "<15",
         sex = "male", 
         aridity = "SPEI03",
         developed = "Less developed")

m.sum <- summary(m_male1520_spei03_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male1520_spei03_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "15-20",
         sex = "male", 
         aridity = "SPEI03",
         developed = "Less developed")

m.sum <- summary(m_male2025_spei03_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male2025_spei03_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "21-25",
         sex = "male", 
         aridity = "SPEI03",
         developed = "Less developed")

m.sum <- summary(m_male2530_spei03_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male2530_spei03_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "26-30",
         sex = "male", 
         aridity = "SPEI03",
         developed = "Less developed")

m.sum <- summary(m_male3045_spei03_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male3045_spei03_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "31-45",
         sex = "male", 
         aridity = "SPEI03",
         developed = "Less developed")

m.sum <- summary(m_male4560_spei03_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male4560_spei03_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "46-60",
         sex = "male", 
         aridity = "SPEI03",
         developed = "Less developed")

m.sum <- summary(m_male60_spei03_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male60_spei03_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = ">60",
         sex = "male", 
         aridity = "SPEI03",
         developed = "Less developed")


## spei12 ---------------------------------------------------------------------

envindicator <- "orig_spei12_mean10_stan"

# Female

m.sum <- summary(m_female015_spei12_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female015_spei12_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "<15",
         sex = "female", 
         aridity = "SPEI12",
         developed = "Less developed")

m.sum <- summary(m_female1520_spei12_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female1520_spei12_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "15-20",
         sex = "female", 
         aridity = "SPEI12",
         developed = "Less developed")

m.sum <- summary(m_female2025_spei12_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female2025_spei12_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "21-25",
         sex = "female", 
         aridity = "SPEI12",
         developed = "Less developed")

m.sum <- summary(m_female2530_spei12_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female2530_spei12_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "26-30",
         sex = "female", 
         aridity = "SPEI12",
         developed = "Less developed")

m.sum <- summary(m_female3045_spei12_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female3045_spei12_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "31-45",
         sex = "female", 
         aridity = "SPEI12",
         developed = "Less developed")

m.sum <- summary(m_female4560_spei12_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female4560_spei12_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "46-60",
         sex = "female", 
         aridity = "SPEI12",
         developed = "Less developed")

m.sum <- summary(m_female60_spei12_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female60_spei12_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = ">60",
         sex = "female", 
         aridity = "SPEI12",
         developed = "Less developed")



# Male

m.sum <- summary(m_male015_spei12_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male015_spei12_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "<15",
         sex = "male", 
         aridity = "SPEI12",
         developed = "Less developed")

m.sum <- summary(m_male1520_spei12_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male1520_spei12_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "15-20",
         sex = "male", 
         aridity = "SPEI12",
         developed = "Less developed")

m.sum <- summary(m_male2025_spei12_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male2025_spei12_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "21-25",
         sex = "male", 
         aridity = "SPEI12",
         developed = "Less developed")

m.sum <- summary(m_male2530_spei12_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male2530_spei12_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "26-30",
         sex = "male", 
         aridity = "SPEI12",
         developed = "Less developed")

m.sum <- summary(m_male3045_spei12_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male3045_spei12_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "31-45",
         sex = "male", 
         aridity = "SPEI12",
         developed = "Less developed")

m.sum <- summary(m_male4560_spei12_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male4560_spei12_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "46-60",
         sex = "male", 
         aridity = "SPEI12",
         developed = "Less developed")

m.sum <- summary(m_male60_spei12_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male60_spei12_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = ">60",
         sex = "male", 
         aridity = "SPEI12",
         developed = "Less developed")


## pdsi ---------------------------------------------------------------------

envindicator <- "orig_pdsi_mean10_stan"

# Female

m.sum <- summary(m_female015_pdsi_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female015_pdsi_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "<15",
         sex = "female", 
         aridity = "PDSI",
         developed = "Less developed")

m.sum <- summary(m_female1520_pdsi_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female1520_pdsi_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "15-20",
         sex = "female", 
         aridity = "PDSI",
         developed = "Less developed")

m.sum <- summary(m_female2025_pdsi_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female2025_pdsi_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "21-25",
         sex = "female", 
         aridity = "PDSI",
         developed = "Less developed")

m.sum <- summary(m_female2530_pdsi_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female2530_pdsi_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "26-30",
         sex = "female", 
         aridity = "PDSI",
         developed = "Less developed")

m.sum <- summary(m_female3045_pdsi_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female3045_pdsi_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "31-45",
         sex = "female", 
         aridity = "PDSI",
         developed = "Less developed")

m.sum <- summary(m_female4560_pdsi_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female4560_pdsi_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "46-60",
         sex = "female", 
         aridity = "PDSI",
         developed = "Less developed")

m.sum <- summary(m_female60_pdsi_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female60_pdsi_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = ">60",
         sex = "female", 
         aridity = "PDSI",
         developed = "Less developed")



# Male

m.sum <- summary(m_male015_pdsi_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male015_pdsi_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "<15",
         sex = "male", 
         aridity = "PDSI",
         developed = "Less developed")

m.sum <- summary(m_male1520_pdsi_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male1520_pdsi_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "15-20",
         sex = "male", 
         aridity = "PDSI",
         developed = "Less developed")

m.sum <- summary(m_male2025_pdsi_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male2025_pdsi_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "21-25",
         sex = "male", 
         aridity = "PDSI",
         developed = "Less developed")

m.sum <- summary(m_male2530_pdsi_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male2530_pdsi_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "26-30",
         sex = "male", 
         aridity = "PDSI",
         developed = "Less developed")

m.sum <- summary(m_male3045_pdsi_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male3045_pdsi_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "31-45",
         sex = "male", 
         aridity = "PDSI",
         developed = "Less developed")

m.sum <- summary(m_male4560_pdsi_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male4560_pdsi_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "46-60",
         sex = "male", 
         aridity = "PDSI",
         developed = "Less developed")

m.sum <- summary(m_male60_pdsi_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male60_pdsi_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = ">60",
         sex = "male", 
         aridity = "PDSI",
         developed = "Less developed")

##
## MORE DEVELOPED --------------------------------------------------------------
## 


## Aridity ---------------------------------------------------------------------

envindicator <- "orig_ai_mean10_stan1"

# Female

m.sum <- summary(m_female015_ai_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female015_ai_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "<15",
         sex = "female", 
         aridity = "AI",
         developed = "More developed")

m.sum <- summary(m_female1520_ai_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female1520_ai_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "15-20",
         sex = "female", 
         aridity = "AI",
         developed = "More developed")

m.sum <- summary(m_female2025_ai_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female2025_ai_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "21-25",
         sex = "female", 
         aridity = "AI",
         developed = "More developed")

m.sum <- summary(m_female2530_ai_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female2530_ai_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "26-30",
         sex = "female", 
         aridity = "AI",
         developed = "More developed")

m.sum <- summary(m_female3045_ai_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female3045_ai_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "31-45",
         sex = "female", 
         aridity = "AI",
         developed = "More developed")

m.sum <- summary(m_female4560_ai_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female4560_ai_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "46-60",
         sex = "female", 
         aridity = "AI",
         developed = "More developed")

m.sum <- summary(m_female60_ai_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female60_ai_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = ">60",
         sex = "female", 
         aridity = "AI",
         developed = "More developed")



# Male

m.sum <- summary(m_male015_ai_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male015_ai_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "<15",
         sex = "male", 
         aridity = "AI",
         developed = "More developed")

m.sum <- summary(m_male1520_ai_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male1520_ai_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "15-20",
         sex = "male", 
         aridity = "AI",
         developed = "More developed")

m.sum <- summary(m_male2025_ai_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male2025_ai_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "21-25",
         sex = "male", 
         aridity = "AI",
         developed = "More developed")

m.sum <- summary(m_male2530_ai_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male2530_ai_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "26-30",
         sex = "male", 
         aridity = "AI",
         developed = "More developed")

m.sum <- summary(m_male3045_ai_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male3045_ai_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "31-45",
         sex = "male", 
         aridity = "AI",
         developed = "More developed")

m.sum <- summary(m_male4560_ai_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male4560_ai_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "46-60",
         sex = "male", 
         aridity = "AI",
         developed = "More developed")

m.sum <- summary(m_male60_ai_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male60_ai_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = ">60",
         sex = "male", 
         aridity = "AI",
         developed = "More developed")



## SPEI03 ---------------------------------------------------------------------

envindicator <- "orig_spei03_mean10_stan"

# Female

m.sum <- summary(m_female015_spei03_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female015_spei03_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "<15",
         sex = "female", 
         aridity = "SPEI03",
         developed = "More developed")

m.sum <- summary(m_female1520_spei03_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female1520_spei03_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "15-20",
         sex = "female", 
         aridity = "SPEI03",
         developed = "More developed")

m.sum <- summary(m_female2025_spei03_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female2025_spei03_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "21-25",
         sex = "female", 
         aridity = "SPEI03",
         developed = "More developed")

m.sum <- summary(m_female2530_spei03_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female2530_spei03_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "26-30",
         sex = "female", 
         aridity = "SPEI03",
         developed = "More developed")

m.sum <- summary(m_female3045_spei03_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female3045_spei03_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "31-45",
         sex = "female", 
         aridity = "SPEI03",
         developed = "More developed")

m.sum <- summary(m_female4560_spei03_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female4560_spei03_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "46-60",
         sex = "female", 
         aridity = "SPEI03",
         developed = "More developed")

m.sum <- summary(m_female60_spei03_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female60_spei03_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = ">60",
         sex = "female", 
         aridity = "SPEI03",
         developed = "More developed")



# Male

m.sum <- summary(m_male015_spei03_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male015_spei03_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "<15",
         sex = "male", 
         aridity = "SPEI03",
         developed = "More developed")

m.sum <- summary(m_male1520_spei03_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male1520_spei03_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "15-20",
         sex = "male", 
         aridity = "SPEI03",
         developed = "More developed")

m.sum <- summary(m_male2025_spei03_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male2025_spei03_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "21-25",
         sex = "male", 
         aridity = "SPEI03",
         developed = "More developed")

m.sum <- summary(m_male2530_spei03_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male2530_spei03_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "26-30",
         sex = "male", 
         aridity = "SPEI03",
         developed = "More developed")

m.sum <- summary(m_male3045_spei03_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male3045_spei03_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "31-45",
         sex = "male", 
         aridity = "SPEI03",
         developed = "More developed")

m.sum <- summary(m_male4560_spei03_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male4560_spei03_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "46-60",
         sex = "male", 
         aridity = "SPEI03",
         developed = "More developed")

m.sum <- summary(m_male60_spei03_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male60_spei03_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = ">60",
         sex = "male", 
         aridity = "SPEI03",
         developed = "More developed")


## spei12 ---------------------------------------------------------------------

envindicator <- "orig_spei12_mean10_stan"

# Female

m.sum <- summary(m_female015_spei12_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female015_spei12_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "<15",
         sex = "female", 
         aridity = "SPEI12",
         developed = "More developed")

m.sum <- summary(m_female1520_spei12_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female1520_spei12_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "15-20",
         sex = "female", 
         aridity = "SPEI12",
         developed = "More developed")

m.sum <- summary(m_female2025_spei12_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female2025_spei12_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "21-25",
         sex = "female", 
         aridity = "SPEI12",
         developed = "More developed")

m.sum <- summary(m_female2530_spei12_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female2530_spei12_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "26-30",
         sex = "female", 
         aridity = "SPEI12",
         developed = "More developed")

m.sum <- summary(m_female3045_spei12_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female3045_spei12_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "31-45",
         sex = "female", 
         aridity = "SPEI12",
         developed = "More developed")

m.sum <- summary(m_female4560_spei12_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female4560_spei12_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "46-60",
         sex = "female", 
         aridity = "SPEI12",
         developed = "More developed")

m.sum <- summary(m_female60_spei12_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female60_spei12_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = ">60",
         sex = "female", 
         aridity = "SPEI12",
         developed = "More developed")



# Male

m.sum <- summary(m_male015_spei12_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male015_spei12_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "<15",
         sex = "male", 
         aridity = "SPEI12",
         developed = "More developed")

m.sum <- summary(m_male1520_spei12_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male1520_spei12_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "15-20",
         sex = "male", 
         aridity = "SPEI12",
         developed = "More developed")

m.sum <- summary(m_male2025_spei12_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male2025_spei12_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "21-25",
         sex = "male", 
         aridity = "SPEI12",
         developed = "More developed")

m.sum <- summary(m_male2530_spei12_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male2530_spei12_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "26-30",
         sex = "male", 
         aridity = "SPEI12",
         developed = "More developed")

m.sum <- summary(m_male3045_spei12_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male3045_spei12_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "31-45",
         sex = "male", 
         aridity = "SPEI12",
         developed = "More developed")

m.sum <- summary(m_male4560_spei12_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male4560_spei12_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "46-60",
         sex = "male", 
         aridity = "SPEI12",
         developed = "More developed")

m.sum <- summary(m_male60_spei12_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male60_spei12_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = ">60",
         sex = "male", 
         aridity = "SPEI12",
         developed = "More developed")


## pdsi ---------------------------------------------------------------------

envindicator <- "orig_pdsi_mean10_stan"

# Female

m.sum <- summary(m_female015_pdsi_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female015_pdsi_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "<15",
         sex = "female", 
         aridity = "PDSI",
         developed = "More developed")

m.sum <- summary(m_female1520_pdsi_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female1520_pdsi_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "15-20",
         sex = "female", 
         aridity = "PDSI",
         developed = "More developed")

m.sum <- summary(m_female2025_pdsi_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female2025_pdsi_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "21-25",
         sex = "female", 
         aridity = "PDSI",
         developed = "More developed")

m.sum <- summary(m_female2530_pdsi_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female2530_pdsi_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "26-30",
         sex = "female", 
         aridity = "PDSI",
         developed = "More developed")

m.sum <- summary(m_female3045_pdsi_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female3045_pdsi_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "31-45",
         sex = "female", 
         aridity = "PDSI",
         developed = "More developed")

m.sum <- summary(m_female4560_pdsi_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female4560_pdsi_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "46-60",
         sex = "female", 
         aridity = "PDSI",
         developed = "More developed")

m.sum <- summary(m_female60_pdsi_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_female60_pdsi_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = ">60",
         sex = "female", 
         aridity = "PDSI",
         developed = "More developed")



# Male

m.sum <- summary(m_male015_pdsi_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male015_pdsi_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "<15",
         sex = "male", 
         aridity = "PDSI",
         developed = "More developed")

m.sum <- summary(m_male1520_pdsi_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male1520_pdsi_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "15-20",
         sex = "male", 
         aridity = "PDSI",
         developed = "More developed")

m.sum <- summary(m_male2025_pdsi_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male2025_pdsi_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "21-25",
         sex = "male", 
         aridity = "PDSI",
         developed = "More developed")

m.sum <- summary(m_male2530_pdsi_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male2530_pdsi_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "26-30",
         sex = "male", 
         aridity = "PDSI",
         developed = "More developed")

m.sum <- summary(m_male3045_pdsi_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male3045_pdsi_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "31-45",
         sex = "male", 
         aridity = "PDSI",
         developed = "More developed")

m.sum <- summary(m_male4560_pdsi_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male4560_pdsi_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = "46-60",
         sex = "male", 
         aridity = "PDSI",
         developed = "More developed")

m.sum <- summary(m_male60_pdsi_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_male60_pdsi_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         age = ">60",
         sex = "male", 
         aridity = "PDSI",
         developed = "More developed")



##
##  BIND EVERYTHING TOGETHER ---------------------------------------------------
##


m.coef <- rbind(
  
  m.coef_female015_ai_less, m.coef_female015_pdsi_less,
  m.coef_female015_spei03_less , m.coef_female015_spei12_less ,
  m.coef_female1520_ai_less  ,   m.coef_female1520_pdsi_less,
  m.coef_female1520_spei03_less, m.coef_female1520_spei12_less ,
  m.coef_female2025_ai_less  ,   m.coef_female2025_pdsi_less  ,
  m.coef_female2025_spei03_less, m.coef_female2025_spei12_less,
  m.coef_female2530_ai_less  ,   m.coef_female2530_pdsi_less  ,
  m.coef_female2530_spei03_less , m.coef_female2530_spei12_less,
  m.coef_female3045_ai_less    , m.coef_female3045_pdsi_less  ,
  m.coef_female3045_spei03_less,  m.coef_female3045_spei12_less,
  m.coef_female4560_ai_less  ,   m.coef_female4560_pdsi_less  ,
 m.coef_female4560_spei03_less , m.coef_female4560_spei12_less,
 m.coef_female60_ai_less     ,   m.coef_female60_pdsi_less  ,
 m.coef_female60_spei03_less ,  m.coef_female60_spei12_less , 
   m.coef_male015_ai_less     ,   m.coef_male015_pdsi_less   ,  
 m.coef_male015_spei03_less  ,    m.coef_male015_spei12_less  , 
 m.coef_male1520_ai_less   ,    m.coef_male1520_pdsi_less ,   
   m.coef_male1520_spei03_less  , m.coef_male1520_spei12_less  ,
 m.coef_male2025_ai_less      ,  m.coef_male2025_pdsi_less   ,
 m.coef_male2025_spei03_less  , m.coef_male2025_spei12_less  ,
   m.coef_male2530_ai_less    ,   m.coef_male2530_pdsi_less  , 
 m.coef_male2530_spei03_less ,  m.coef_male2530_spei12_less ,
 m.coef_male3045_ai_less     ,  m.coef_male3045_pdsi_less   ,
   m.coef_male3045_spei03_less ,  m.coef_male3045_spei12_less  , 
 m.coef_male4560_ai_less       , m.coef_male4560_pdsi_less  , 
 m.coef_male4560_spei03_less ,   m.coef_male4560_spei12_less  ,
   m.coef_male60_ai_less     ,    m.coef_male60_pdsi_less  ,  
   m.coef_male60_spei03_less  , m.coef_male60_spei12_less,
 
 m.coef_female015_ai_more, m.coef_female015_pdsi_more,
 m.coef_female015_spei03_more , m.coef_female015_spei12_more ,
 m.coef_female1520_ai_more  ,   m.coef_female1520_pdsi_more,
 m.coef_female1520_spei03_more, m.coef_female1520_spei12_more ,
 m.coef_female2025_ai_more  ,   m.coef_female2025_pdsi_more  ,
 m.coef_female2025_spei03_more, m.coef_female2025_spei12_more,
 m.coef_female2530_ai_more  ,   m.coef_female2530_pdsi_more  ,
 m.coef_female2530_spei03_more , m.coef_female2530_spei12_more,
 m.coef_female3045_ai_more    , m.coef_female3045_pdsi_more  ,
 m.coef_female3045_spei03_more,  m.coef_female3045_spei12_more,
 m.coef_female4560_ai_more  ,   m.coef_female4560_pdsi_more  ,
 m.coef_female4560_spei03_more , m.coef_female4560_spei12_more,
 m.coef_female60_ai_more     ,   m.coef_female60_pdsi_more  ,
 m.coef_female60_spei03_more ,  m.coef_female60_spei12_more , 
 m.coef_male015_ai_more     ,   m.coef_male015_pdsi_more   ,  
 m.coef_male015_spei03_more  ,    m.coef_male015_spei12_more  , 
 m.coef_male1520_ai_more   ,    m.coef_male1520_pdsi_more ,   
 m.coef_male1520_spei03_more  , m.coef_male1520_spei12_more  ,
 m.coef_male2025_ai_more      ,  m.coef_male2025_pdsi_more   ,
 m.coef_male2025_spei03_more  , m.coef_male2025_spei12_more  ,
 m.coef_male2530_ai_more    ,   m.coef_male2530_pdsi_more  , 
 m.coef_male2530_spei03_more ,  m.coef_male2530_spei12_more ,
 m.coef_male3045_ai_more     ,  m.coef_male3045_pdsi_more   ,
 m.coef_male3045_spei03_more ,  m.coef_male3045_spei12_more  , 
 m.coef_male4560_ai_more       , m.coef_male4560_pdsi_more  , 
 m.coef_male4560_spei03_more ,   m.coef_male4560_spei12_more  ,
 m.coef_male60_ai_more     ,    m.coef_male60_pdsi_more  ,  
 m.coef_male60_spei03_more  , m.coef_male60_spei12_more
 )
  
 m.coef <- m.coef %>% 
  mutate(order = recode(
    age, 
    "<15" = 1L,
    "15-20" = 2L,
    "21-25" = 3L,
    "26-30" = 4L,
    "31-45" = 5L,
    "46-60" = 6L,
    ">60" = 7L
    ))

 
 m.coef <- m.coef %>% 
   mutate(order2 = recode(
     aridity, 
     "AI" = 1L,
     "PDSI" = 2L,
     "SPEI03" = 3L,
     "SPEI12" = 4L,
   ))
 
 ##
 ## COEFFICIENT PLOT ------------------------------------------------------------
 ##
 
 
 g1 <- 
  m.coef %>% 
   ggplot(aes(x=coef, y=fct_reorder(age, -order), color=sex, group=sex))+ 
   geom_vline(aes(xintercept=0), color="Red")+
   geom_pointrange(
     aes(xmin = ci_low, xmax = ci_up), # , colour = climatic, shape = climatic, fill = climatic
     position = position_dodge(0.6), 
     size = 0.6,
     shape=18#, key_glyph = draw_key_point
   ) +
   geom_path(position = position_dodge(0.6), alpha=0.8,  linetype="dashed")+
   facet_grid(developed~fct_reorder(aridity,order2), space="free_x")+
   scale_color_manual(name="", values = c("#328da8", "#1aa156"))+
   scale_x_continuous(labels=scales::percent)+
   xlab("Migration impacts")+ylab("")+
   #ggtitle("(a) Effect differences by world regions")+
   theme_bw() +
   ggtitle("")+
   theme(
     strip.text = element_text(face = "bold", size=11),
     strip.background = element_rect(fill = "#daebdd"),
     legend.position = "bottom",
     axis.text=element_text(size=11)
   )+
   coord_cartesian(xlim=c(-0.12,0.25))
 
g1

m.coef.agesex <- m.coef

save(g1, m.coef.agesex, file="figure_effect differences by age group2.RData")
ggsave(plot = g1, filename="figure_effect differences by age group2.png", width=12, height = 5)




