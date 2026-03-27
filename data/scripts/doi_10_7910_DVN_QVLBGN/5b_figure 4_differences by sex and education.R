
### ----------------------------------------------------------------------------
### CLIMATE CHANGE, ARIDITY, AND INTERNAL MIGRATION 
### ----------------------------------------------------------------------------

### 
### EXTENDED MODELS BY EDUCATION AND SEX ---------------------------------------
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

##
## LOAD DATA -------------------------------------------------------------------
## 

load(file="full migration data.RData")
d.sexedu <- d.sexedu %>% filter(mig_interval != 10)

#> restricting data to cases for which we have observations for all sex/age groups
d.sexedu <- d.sexedu %>% 
  filter(!is.na(flow_out_rate_annual_femalelessprimary)&
           !is.na(flow_out_rate_annual_femaleprimary)&
           !is.na(flow_out_rate_annual_femalesecondary)&
           !is.na(flow_out_rate_annual_femaletertiary)&
           !is.na(flow_out_rate_annual_malelessprimary)&
           !is.na(flow_out_rate_annual_maleprimary)&
           !is.na(flow_out_rate_annual_malesecondary)&
           !is.na(flow_out_rate_annual_maletertiary))


#> focus on less developed countries first
aux1 <- d.sexedu %>% filter(grp_un_develop=="Less")

#> include only region-region pairs that are also covered in the main migration dataset
aux1 <- aux1 %>% filter(include==1)


##
## FEMALE, EDUCATION, AI -------------------------------------------------------------
## 


m_femalelessprimary_ai_less <-  fepois(flow_out_rate_annual_femalelessprimary ~ 
                                    orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                                    worldregion*as.factor(year_cat10)+
                                    log(dist)+contig+as.factor(mig_interval),
                                  fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                  data=aux1)

m_femaleprimary_ai_less <-  fepois(flow_out_rate_annual_femaleprimary ~ 
                                orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                                worldregion*as.factor(year_cat10)+
                                log(dist)+contig+as.factor(mig_interval),
                              fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                              data=aux1)

m_femalesecondary_ai_less <-  fepois(flow_out_rate_annual_femalesecondary~ 
                                  orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                                  worldregion*as.factor(year_cat10)+
                                  log(dist)+contig+as.factor(mig_interval),
                                fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                data=aux1)

m_femaletertiary_ai_less <-  fepois(flow_out_rate_annual_femaletertiary ~ 
                                 orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                                 worldregion*as.factor(year_cat10)+
                                 log(dist)+contig+as.factor(mig_interval),
                               fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                               data=aux1)


##
## MALE, EDUCATION, AI -------------------------------------------------------------
## 


m_malelessprimary_ai_less <-  fepois(flow_out_rate_annual_malelessprimary ~ 
                                  orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                                  worldregion*as.factor(year_cat10)+
                                  log(dist)+contig+as.factor(mig_interval),
                                fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                data=aux1)

m_maleprimary_ai_less <-  fepois(flow_out_rate_annual_maleprimary ~ 
                              orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                              worldregion*as.factor(year_cat10)+
                              log(dist)+contig+as.factor(mig_interval),
                            fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                            data=aux1)

m_malesecondary_ai_less <-  fepois(flow_out_rate_annual_malesecondary ~ 
                                orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                                worldregion*as.factor(year_cat10)+
                                log(dist)+contig+as.factor(mig_interval),
                              fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                              data=aux1)

m_maletertiary_ai_less <-  fepois(flow_out_rate_annual_maletertiary~ 
                               orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                               worldregion*as.factor(year_cat10)+
                               log(dist)+contig+as.factor(mig_interval),
                             fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                             data=aux1)


##
## FEMALE, EDUCATION, SPEI03 -------------------------------------------------------------
## 


m_femalelessprimary_spei03_less <-  fepois(flow_out_rate_annual_femalelessprimary ~ 
                                        orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                        worldregion*as.factor(year_cat10)+
                                        log(dist)+contig+as.factor(mig_interval),
                                      fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                      data=aux1)

m_femaleprimary_spei03_less <-  fepois(flow_out_rate_annual_femaleprimary ~ 
                                    orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                    worldregion*as.factor(year_cat10)+
                                    log(dist)+contig+as.factor(mig_interval),
                                  fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                  data=aux1)

m_femalesecondary_spei03_less <-  fepois(flow_out_rate_annual_femalesecondary ~ 
                                      orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                      worldregion*as.factor(year_cat10)+
                                      log(dist)+contig+as.factor(mig_interval),
                                    fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                    data=aux1)

m_femaletertiary_spei03_less <-  fepois(flow_out_rate_annual_femaletertiary ~ 
                                     orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                     worldregion*as.factor(year_cat10)+
                                     log(dist)+contig+as.factor(mig_interval),
                                   fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                   data=aux1)

##
## MALE, EDUCATION, SPEI03 -------------------------------------------------------------
## 


m_malelessprimary_spei03_less <-  fepois(flow_out_rate_annual_malelessprimary ~ 
                                      orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                      worldregion*as.factor(year_cat10)+
                                      log(dist)+contig+as.factor(mig_interval),
                                    fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                    data=aux1)

m_maleprimary_spei03_less <-  fepois(flow_out_rate_annual_maleprimary ~ 
                                  orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                  worldregion*as.factor(year_cat10)+
                                  log(dist)+contig+as.factor(mig_interval),
                                fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                data=aux1)

m_malesecondary_spei03_less <-  fepois(flow_out_rate_annual_malesecondary ~ 
                                    orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                    worldregion*as.factor(year_cat10)+
                                    log(dist)+contig+as.factor(mig_interval),
                                  fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                  data=aux1)

m_maletertiary_spei03_less <-  fepois(flow_out_rate_annual_maletertiary ~ 
                                   orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                   worldregion*as.factor(year_cat10)+
                                   log(dist)+contig+as.factor(mig_interval),
                                 fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                 data=aux1)


##
## FEMALE, EDUCATION, SPEI12 -------------------------------------------------------------
## 


m_femalelessprimary_spei12_less <-  fepois(flow_out_rate_annual_femalelessprimary ~ 
                                        orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                        worldregion*as.factor(year_cat10)+
                                        log(dist)+contig+as.factor(mig_interval),
                                      fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                      data=aux1)

m_femaleprimary_spei12_less <-  fepois(flow_out_rate_annual_femaleprimary ~ 
                                    orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                    worldregion*as.factor(year_cat10)+
                                    log(dist)+contig+as.factor(mig_interval),
                                  fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                  data=aux1)



m_femalesecondary_spei12_less <-  fepois(flow_out_rate_annual_femalesecondary~ 
                                      orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                      worldregion*as.factor(year_cat10)+
                                      log(dist)+contig+as.factor(mig_interval),
                                    fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                    data=aux1)

m_femaletertiary_spei12_less <-  fepois(flow_out_rate_annual_femaletertiary ~ 
                                     orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                     worldregion*as.factor(year_cat10)+
                                     log(dist)+contig+as.factor(mig_interval),
                                   fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                   data=aux1)


##
## MALE, EDUCATION, spei12 -------------------------------------------------------------
## 


m_malelessprimary_spei12_less <-  fepois(flow_out_rate_annual_malelessprimary ~ 
                                      orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                      worldregion*as.factor(year_cat10)+
                                      log(dist)+contig+as.factor(mig_interval),
                                    fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                    data=aux1)

m_maleprimary_spei12_less <-  fepois(flow_out_rate_annual_maleprimary ~ 
                                  orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                  worldregion*as.factor(year_cat10)+
                                  log(dist)+contig+as.factor(mig_interval),
                                fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                data=aux1)

m_malesecondary_spei12_less <-  fepois(flow_out_rate_annual_malesecondary ~ 
                                    orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                    worldregion*as.factor(year_cat10)+
                                    log(dist)+contig+as.factor(mig_interval),
                                  fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                  data=aux1)

m_maletertiary_spei12_less <-  fepois(flow_out_rate_annual_maletertiary ~ 
                                   orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                   worldregion*as.factor(year_cat10)+
                                   log(dist)+contig+as.factor(mig_interval),
                                 fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                 data=aux1)


##
## FEMALE, EDUCATION, pdsi -------------------------------------------------------------
## 


m_femalelessprimary_pdsi_less <-  fepois(flow_out_rate_annual_femalelessprimary ~ 
                                      orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                                      worldregion*as.factor(year_cat10)+
                                      log(dist)+contig+as.factor(mig_interval),
                                    fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                    data=aux1)

m_femaleprimary_pdsi_less <-  fepois(flow_out_rate_annual_femaleprimary~ 
                                  orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                                  worldregion*as.factor(year_cat10)+
                                  log(dist)+contig+as.factor(mig_interval),
                                fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                data=aux1)



m_femalesecondary_pdsi_less <-  fepois(flow_out_rate_annual_femalesecondary~ 
                                    orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                                    worldregion*as.factor(year_cat10)+
                                    log(dist)+contig+as.factor(mig_interval),
                                  fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                  data=aux1)

m_femaletertiary_pdsi_less <-  fepois(flow_out_rate_annual_femaletertiary ~ 
                                   orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                                   worldregion*as.factor(year_cat10)+
                                   log(dist)+contig+as.factor(mig_interval),
                                 fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                 data=aux1)


##
## MALE, EDUCATION, pdsi -------------------------------------------------------------
## 


m_malelessprimary_pdsi_less <-  fepois(flow_out_rate_annual_malelessprimary ~ 
                                    orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                                    worldregion*as.factor(year_cat10)+
                                    log(dist)+contig+as.factor(mig_interval),
                                  fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                  data=aux1)

m_maleprimary_pdsi_less <-  fepois(flow_out_rate_annual_maleprimary ~ 
                                orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                                worldregion*as.factor(year_cat10)+
                                log(dist)+contig+as.factor(mig_interval),
                              fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                              data=aux1)

m_malesecondary_pdsi_less <-  fepois(flow_out_rate_annual_malesecondary ~ 
                                  orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                                  worldregion*as.factor(year_cat10)+
                                  log(dist)+contig+as.factor(mig_interval),
                                fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                data=aux1)

m_maletertiary_pdsi_less <-  fepois(flow_out_rate_annual_maletertiary ~ 
                                 orig_pdsi_mean10_stan1+dest_pdsi_mean10_stan1+
                                 worldregion*as.factor(year_cat10)+
                                 log(dist)+contig+as.factor(mig_interval),
                               fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                               data=aux1)


##
## MORE DEVELOPED --------------------------------------------------------------
## 

#> focus on more developed countries 
aux2 <- d.sexedu %>% filter(grp_un_develop=="More")

#> include only region-region pairs that are also covered in the main migration dataset
aux2 <- aux2 %>% filter(include==1)


##
## FEMALE, EDUCATION, AI -------------------------------------------------------------
## 


m_femalelessprimary_ai_more <-  fepois(flow_out_rate_annual_femalelessprimary ~ 
                                         orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                                         worldregion*as.factor(year_cat10)+
                                         log(dist)+contig+as.factor(mig_interval),
                                       fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                       data=aux2)

m_femaleprimary_ai_more <-  fepois(flow_out_rate_annual_femaleprimary ~ 
                                     orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                                     worldregion*as.factor(year_cat10)+
                                     log(dist)+contig+as.factor(mig_interval),
                                   fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                   data=aux2)

m_femalesecondary_ai_more <-  fepois(flow_out_rate_annual_femalesecondary~ 
                                       orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                                       worldregion*as.factor(year_cat10)+
                                       log(dist)+contig+as.factor(mig_interval),
                                     fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                     data=aux2)

m_femaletertiary_ai_more <-  fepois(flow_out_rate_annual_femaletertiary ~ 
                                      orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                                      worldregion*as.factor(year_cat10)+
                                      log(dist)+contig+as.factor(mig_interval),
                                    fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                    data=aux2)


##
## MALE, EDUCATION, AI -------------------------------------------------------------
## 


m_malelessprimary_ai_more <-  fepois(flow_out_rate_annual_malelessprimary ~ 
                                       orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                                       worldregion*as.factor(year_cat10)+
                                       log(dist)+contig+as.factor(mig_interval),
                                     fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                     data=aux2)

m_maleprimary_ai_more <-  fepois(flow_out_rate_annual_maleprimary ~ 
                                   orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                                   worldregion*as.factor(year_cat10)+
                                   log(dist)+contig+as.factor(mig_interval),
                                 fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                 data=aux2)

m_malesecondary_ai_more <-  fepois(flow_out_rate_annual_malesecondary ~ 
                                     orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                                     worldregion*as.factor(year_cat10)+
                                     log(dist)+contig+as.factor(mig_interval),
                                   fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                   data=aux2)

m_maletertiary_ai_more <-  fepois(flow_out_rate_annual_maletertiary~ 
                                    orig_ai_mean10_stan1+dest_ai_mean10_stan1+
                                    worldregion*as.factor(year_cat10)+
                                    log(dist)+contig+as.factor(mig_interval),
                                  fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                  data=aux2)


##
## FEMALE, EDUCATION, SPEI03 -------------------------------------------------------------
## 


m_femalelessprimary_spei03_more <-  fepois(flow_out_rate_annual_femalelessprimary ~ 
                                             orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                             worldregion*as.factor(year_cat10)+
                                             log(dist)+contig+as.factor(mig_interval),
                                           fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                           data=aux2)

m_femaleprimary_spei03_more <-  fepois(flow_out_rate_annual_femaleprimary ~ 
                                         orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                         worldregion*as.factor(year_cat10)+
                                         log(dist)+contig+as.factor(mig_interval),
                                       fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                       data=aux2)

m_femalesecondary_spei03_more <-  fepois(flow_out_rate_annual_femalesecondary ~ 
                                           orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                           worldregion*as.factor(year_cat10)+
                                           log(dist)+contig+as.factor(mig_interval),
                                         fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                         data=aux2)

m_femaletertiary_spei03_more <-  fepois(flow_out_rate_annual_femaletertiary ~ 
                                          orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                          worldregion*as.factor(year_cat10)+
                                          log(dist)+contig+as.factor(mig_interval),
                                        fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                        data=aux2)

##
## MALE, EDUCATION, SPEI03 -------------------------------------------------------------
## 


m_malelessprimary_spei03_more <-  fepois(flow_out_rate_annual_malelessprimary ~ 
                                           orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                           worldregion*as.factor(year_cat10)+
                                           log(dist)+contig+as.factor(mig_interval),
                                         fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                         data=aux2)

m_maleprimary_spei03_more <-  fepois(flow_out_rate_annual_maleprimary ~ 
                                       orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                       worldregion*as.factor(year_cat10)+
                                       log(dist)+contig+as.factor(mig_interval),
                                     fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                     data=aux2)

m_malesecondary_spei03_more <-  fepois(flow_out_rate_annual_malesecondary ~ 
                                         orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                         worldregion*as.factor(year_cat10)+
                                         log(dist)+contig+as.factor(mig_interval),
                                       fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                       data=aux2)

m_maletertiary_spei03_more <-  fepois(flow_out_rate_annual_maletertiary ~ 
                                        orig_spei03_mean10_stan1+dest_spei03_mean10_stan1+
                                        worldregion*as.factor(year_cat10)+
                                        log(dist)+contig+as.factor(mig_interval),
                                      fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                      data=aux2)


##
## FEMALE, EDUCATION, SPEI12 -------------------------------------------------------------
## 


m_femalelessprimary_spei12_more <-  fepois(flow_out_rate_annual_femalelessprimary ~ 
                                             orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                             worldregion*as.factor(year_cat10)+
                                             log(dist)+contig+as.factor(mig_interval),
                                           fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                           data=aux2)

m_femaleprimary_spei12_more <-  fepois(flow_out_rate_annual_femaleprimary ~ 
                                         orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                         worldregion*as.factor(year_cat10)+
                                         log(dist)+contig+as.factor(mig_interval),
                                       fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                       data=aux2)



m_femalesecondary_spei12_more <-  fepois(flow_out_rate_annual_femalesecondary~ 
                                           orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                           worldregion*as.factor(year_cat10)+
                                           log(dist)+contig+as.factor(mig_interval),
                                         fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                         data=aux2)

m_femaletertiary_spei12_more <-  fepois(flow_out_rate_annual_femaletertiary ~ 
                                          orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                          worldregion*as.factor(year_cat10)+
                                          log(dist)+contig+as.factor(mig_interval),
                                        fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                        data=aux2)


##
## MALE, EDUCATION, spei12 -------------------------------------------------------------
## 


m_malelessprimary_spei12_more <-  fepois(flow_out_rate_annual_malelessprimary ~ 
                                           orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                           worldregion*as.factor(year_cat10)+
                                           log(dist)+contig+as.factor(mig_interval),
                                         fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                         data=aux2)

m_maleprimary_spei12_more <-  fepois(flow_out_rate_annual_maleprimary ~ 
                                       orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                       worldregion*as.factor(year_cat10)+
                                       log(dist)+contig+as.factor(mig_interval),
                                     fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                     data=aux2)

m_malesecondary_spei12_more <-  fepois(flow_out_rate_annual_malesecondary ~ 
                                         orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                         worldregion*as.factor(year_cat10)+
                                         log(dist)+contig+as.factor(mig_interval),
                                       fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                       data=aux2)

m_maletertiary_spei12_more <-  fepois(flow_out_rate_annual_maletertiary ~ 
                                        orig_spei12_mean10_stan1+dest_spei12_mean10_stan1+
                                        worldregion*as.factor(year_cat10)+
                                        log(dist)+contig+as.factor(mig_interval),
                                      fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                      data=aux2)


##
## FEMALE, EDUCATION, pdsi -------------------------------------------------------------
## 


m_femalelessprimary_pdsi_more <-  fepois(flow_out_rate_annual_femalelessprimary ~ 
                                           orig_pdsi_mean10_stan1+
                                           worldregion*as.factor(year_cat10)+
                                           log(dist)+contig+as.factor(mig_interval),
                                         fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                         data=aux2)

m_femaleprimary_pdsi_more <-  fepois(flow_out_rate_annual_femaleprimary~ 
                                       orig_pdsi_mean10_stan1+
                                       worldregion*as.factor(year_cat10)+
                                       log(dist)+contig+as.factor(mig_interval),
                                     fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                     data=aux2)



m_femalesecondary_pdsi_more <-  fepois(flow_out_rate_annual_femalesecondary~ 
                                         orig_pdsi_mean10_stan1+
                                         worldregion*as.factor(year_cat10)+
                                         log(dist)+contig+as.factor(mig_interval),
                                       fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                       data=aux2)

m_femaletertiary_pdsi_more <-  fepois(flow_out_rate_annual_femaletertiary ~ 
                                        orig_pdsi_mean10_stan1+
                                        worldregion*as.factor(year_cat10)+
                                        log(dist)+contig+as.factor(mig_interval),
                                      fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                      data=aux2)


##
## MALE, EDUCATION, pdsi -------------------------------------------------------------
## 


m_malelessprimary_pdsi_more <-  fepois(flow_out_rate_annual_malelessprimary ~ 
                                         orig_pdsi_mean10_stan1+
                                         worldregion*as.factor(year_cat10)+
                                         log(dist)+contig+as.factor(mig_interval),
                                       fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                       data=aux2)

m_maleprimary_pdsi_more <-  fepois(flow_out_rate_annual_maleprimary ~ 
                                     orig_pdsi_mean10_stan1+
                                     worldregion*as.factor(year_cat10)+
                                     log(dist)+contig+as.factor(mig_interval),
                                   fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                   data=aux2)

m_malesecondary_pdsi_more <-  fepois(flow_out_rate_annual_malesecondary ~ 
                                       orig_pdsi_mean10_stan1+
                                       worldregion*as.factor(year_cat10)+
                                       log(dist)+contig+as.factor(mig_interval),
                                     fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                     data=aux2)

m_maletertiary_pdsi_more <-  fepois(flow_out_rate_annual_maletertiary ~ 
                                      orig_pdsi_mean10_stan1+
                                      worldregion*as.factor(year_cat10)+
                                      log(dist)+contig+as.factor(mig_interval),
                                    fixef = c("orig_dest_dyad"),cluster=c("orig_dest_dyad"),
                                    data=aux2)



##
## LESS DEVELOPED --------------------------------------------------------------
## 


## Aridity ---------------------------------------------------------------------

envindicator <- "orig_ai_mean10_stan1"


# Female

m.sum <- summary(m_femalelessprimary_ai_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_femalelessprimary_ai_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "< primary",
         sex = "female", 
         aridity = "AI",
         developed = "Less developed")

m.sum <- summary(m_femaleprimary_ai_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_femaleprimary_ai_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "primary",
         sex = "female", 
         aridity = "AI",
         developed = "Less developed")

m.sum <- summary(m_femalesecondary_ai_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_femalesecondary_ai_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "secondary",
         sex = "female", 
         aridity = "AI",
         developed = "Less developed")

m.sum <- summary(m_femaletertiary_ai_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_femaletertiary_ai_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "tertiary",
         sex = "female", 
         aridity = "AI",
         developed = "Less developed")


# Male

m.sum <- summary(m_malelessprimary_ai_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_malelessprimary_ai_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "< primary",
         sex = "male", 
         aridity = "AI",
         developed = "Less developed")

m.sum <- summary(m_maleprimary_ai_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_maleprimary_ai_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "primary",
         sex = "male", 
         aridity = "AI",
         developed = "Less developed")

m.sum <- summary(m_malesecondary_ai_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_malesecondary_ai_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "secondary",
         sex = "male", 
         aridity = "AI",
         developed = "Less developed")

m.sum <- summary(m_maletertiary_ai_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_maletertiary_ai_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "tertiary",
         sex = "male", 
         aridity = "AI",
         developed = "Less developed")


## SPEI03 ---------------------------------------------------------------------

envindicator <- "orig_spei03_mean10_stan"

# Female

m.sum <- summary(m_femalelessprimary_spei03_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_femalelessprimary_spei03_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "< primary",
         sex = "female", 
         aridity = "SPEI03",
         developed = "Less developed")

m.sum <- summary(m_femaleprimary_spei03_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_femaleprimary_spei03_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "primary",
         sex = "female", 
         aridity = "SPEI03",
         developed = "Less developed")

m.sum <- summary(m_femalesecondary_spei03_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_femalesecondary_spei03_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "secondary",
         sex = "female", 
         aridity = "SPEI03",
         developed = "Less developed")

m.sum <- summary(m_femaletertiary_spei03_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_femaletertiary_spei03_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "tertiary",
         sex = "female", 
         aridity = "SPEI03",
         developed = "Less developed")


# Male

m.sum <- summary(m_malelessprimary_spei03_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_malelessprimary_spei03_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "< primary",
         sex = "male", 
         aridity = "SPEI03",
         developed = "Less developed")

m.sum <- summary(m_maleprimary_spei03_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_maleprimary_spei03_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "primary",
         sex = "male", 
         aridity = "SPEI03",
         developed = "Less developed")

m.sum <- summary(m_malesecondary_spei03_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_malesecondary_spei03_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "secondary",
         sex = "male", 
         aridity = "SPEI03",
         developed = "Less developed")

m.sum <- summary(m_maletertiary_spei03_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_maletertiary_spei03_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "tertiary",
         sex = "male", 
         aridity = "SPEI03",
         developed = "Less developed")



## spei12 ---------------------------------------------------------------------

envindicator <- "orig_spei12_mean10_stan"

# Female

m.sum <- summary(m_femalelessprimary_spei12_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_femalelessprimary_spei12_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "< primary",
         sex = "female", 
          aridity = "SPEI12",
         developed = "Less developed")

m.sum <- summary(m_femaleprimary_spei12_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_femaleprimary_spei12_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "primary",
         sex = "female", 
          aridity = "SPEI12",
         developed = "Less developed")

m.sum <- summary(m_femalesecondary_spei12_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_femalesecondary_spei12_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "secondary",
         sex = "female", 
          aridity = "SPEI12",
         developed = "Less developed")

m.sum <- summary(m_femaletertiary_spei12_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_femaletertiary_spei12_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "tertiary",
         sex = "female", 
          aridity = "SPEI12",
         developed = "Less developed")


# Male

m.sum <- summary(m_malelessprimary_spei12_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_malelessprimary_spei12_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "< primary",
         sex = "male", 
          aridity = "SPEI12",
         developed = "Less developed")

m.sum <- summary(m_maleprimary_spei12_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_maleprimary_spei12_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "primary",
         sex = "male", 
          aridity = "SPEI12",
         developed = "Less developed")

m.sum <- summary(m_malesecondary_spei12_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_malesecondary_spei12_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "secondary",
         sex = "male", 
          aridity = "SPEI12",
         developed = "Less developed")

m.sum <- summary(m_maletertiary_spei12_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_maletertiary_spei12_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "tertiary",
         sex = "male", 
          aridity = "SPEI12",
         developed = "Less developed")


## pdsi ---------------------------------------------------------------------

envindicator <- "orig_pdsi_mean10_stan"

# Female

m.sum <- summary(m_femalelessprimary_pdsi_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_femalelessprimary_pdsi_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "< primary",
         sex = "female", 
          aridity = "PDSI",
         developed = "Less developed")

m.sum <- summary(m_femaleprimary_pdsi_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_femaleprimary_pdsi_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "primary",
         sex = "female", 
          aridity = "PDSI",
         developed = "Less developed")

m.sum <- summary(m_femalesecondary_pdsi_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_femalesecondary_pdsi_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "secondary",
         sex = "female", 
          aridity = "PDSI",
         developed = "Less developed")

m.sum <- summary(m_femaletertiary_pdsi_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_femaletertiary_pdsi_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "tertiary",
         sex = "female", 
          aridity = "PDSI",
         developed = "Less developed")


# Male

m.sum <- summary(m_malelessprimary_pdsi_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_malelessprimary_pdsi_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "< primary",
         sex = "male", 
          aridity = "PDSI",
         developed = "Less developed")

m.sum <- summary(m_maleprimary_pdsi_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_maleprimary_pdsi_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "primary",
         sex = "male", 
          aridity = "PDSI",
         developed = "Less developed")

m.sum <- summary(m_malesecondary_pdsi_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_malesecondary_pdsi_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "secondary",
         sex = "male", 
          aridity = "PDSI",
         developed = "Less developed")

m.sum <- summary(m_maletertiary_pdsi_less, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_maletertiary_pdsi_less <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "tertiary",
         sex = "male", 
          aridity = "PDSI",
         developed = "Less developed")



##
## MORE DEVELOPED --------------------------------------------------------------
## 


## Aridity ---------------------------------------------------------------------

envindicator <- "orig_ai_mean10_stan1"


# Female

m.sum <- summary(m_femalelessprimary_ai_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_femalelessprimary_ai_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
        
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "< primary",
         sex = "female", 
         aridity = "AI",
         developed = "More developed")

m.sum <- summary(m_femaleprimary_ai_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_femaleprimary_ai_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "primary",
         sex = "female", 
         aridity = "AI",
         developed = "More developed")

m.sum <- summary(m_femalesecondary_ai_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_femalesecondary_ai_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "secondary",
         sex = "female", 
         aridity = "AI",
         developed = "More developed")

m.sum <- summary(m_femaletertiary_ai_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_femaletertiary_ai_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "tertiary",
         sex = "female", 
         aridity = "AI",
         developed = "More developed")


# Male

m.sum <- summary(m_malelessprimary_ai_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_malelessprimary_ai_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "< primary",
         sex = "male", 
         aridity = "AI",
         developed = "More developed")

m.sum <- summary(m_maleprimary_ai_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_maleprimary_ai_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "primary",
         sex = "male", 
         aridity = "AI",
         developed = "More developed")

m.sum <- summary(m_malesecondary_ai_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_malesecondary_ai_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "secondary",
         sex = "male", 
         aridity = "AI",
         developed = "More developed")

m.sum <- summary(m_maletertiary_ai_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_maletertiary_ai_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "tertiary",
         sex = "male", 
         aridity = "AI",
         developed = "More developed")


## SPEI03 ---------------------------------------------------------------------

envindicator <- "orig_spei03_mean10_stan"

# Female

m.sum <- summary(m_femalelessprimary_spei03_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_femalelessprimary_spei03_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "< primary",
         sex = "female", 
         aridity = "SPEI03",
         developed = "More developed")

m.sum <- summary(m_femaleprimary_spei03_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_femaleprimary_spei03_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "primary",
         sex = "female", 
         aridity = "SPEI03",
         developed = "More developed")

m.sum <- summary(m_femalesecondary_spei03_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_femalesecondary_spei03_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "secondary",
         sex = "female", 
         aridity = "SPEI03",
         developed = "More developed")

m.sum <- summary(m_femaletertiary_spei03_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_femaletertiary_spei03_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "tertiary",
         sex = "female", 
         aridity = "SPEI03",
         developed = "More developed")


# Male

m.sum <- summary(m_malelessprimary_spei03_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_malelessprimary_spei03_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "< primary",
         sex = "male", 
         aridity = "SPEI03",
         developed = "More developed")

m.sum <- summary(m_maleprimary_spei03_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_maleprimary_spei03_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "primary",
         sex = "male", 
         aridity = "SPEI03",
         developed = "More developed")

m.sum <- summary(m_malesecondary_spei03_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_malesecondary_spei03_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "secondary",
         sex = "male", 
         aridity = "SPEI03",
         developed = "More developed")

m.sum <- summary(m_maletertiary_spei03_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_maletertiary_spei03_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "tertiary",
         sex = "male", 
         aridity = "SPEI03",
         developed = "More developed")



## spei12 ---------------------------------------------------------------------

envindicator <- "orig_spei12_mean10_stan"

# Female

m.sum <- summary(m_femalelessprimary_spei12_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_femalelessprimary_spei12_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "< primary",
         sex = "female", 
         aridity = "SPEI12",
         developed = "More developed")

m.sum <- summary(m_femaleprimary_spei12_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_femaleprimary_spei12_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "primary",
         sex = "female", 
         aridity = "SPEI12",
         developed = "More developed")

m.sum <- summary(m_femalesecondary_spei12_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_femalesecondary_spei12_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "secondary",
         sex = "female", 
         aridity = "SPEI12",
         developed = "More developed")

m.sum <- summary(m_femaletertiary_spei12_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_femaletertiary_spei12_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "tertiary",
         sex = "female", 
         aridity = "SPEI12",
         developed = "More developed")


# Male

m.sum <- summary(m_malelessprimary_spei12_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_malelessprimary_spei12_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "< primary",
         sex = "male", 
         aridity = "SPEI12",
         developed = "More developed")

m.sum <- summary(m_maleprimary_spei12_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_maleprimary_spei12_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "primary",
         sex = "male", 
         aridity = "SPEI12",
         developed = "More developed")

m.sum <- summary(m_malesecondary_spei12_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_malesecondary_spei12_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "secondary",
         sex = "male", 
         aridity = "SPEI12",
         developed = "More developed")

m.sum <- summary(m_maletertiary_spei12_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_maletertiary_spei12_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "tertiary",
         sex = "male", 
         aridity = "SPEI12",
         developed = "More developed")


## pdsi ---------------------------------------------------------------------

envindicator <- "orig_pdsi_mean10_stan"

# Female

m.sum <- summary(m_femalelessprimary_pdsi_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_femalelessprimary_pdsi_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "< primary",
         sex = "female", 
         aridity = "PDSI",
         developed = "More developed")

m.sum <- summary(m_femaleprimary_pdsi_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_femaleprimary_pdsi_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "primary",
         sex = "female", 
         aridity = "PDSI",
         developed = "More developed")

m.sum <- summary(m_femalesecondary_pdsi_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_femalesecondary_pdsi_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "secondary",
         sex = "female", 
         aridity = "PDSI",
         developed = "More developed")

m.sum <- summary(m_femaletertiary_pdsi_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_femaletertiary_pdsi_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "tertiary",
         sex = "female", 
         aridity = "PDSI",
         developed = "More developed")

# Male

m.sum <- summary(m_malelessprimary_pdsi_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_malelessprimary_pdsi_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "< primary",
         sex = "male", 
         aridity = "PDSI",
         developed = "More developed")

m.sum <- summary(m_maleprimary_pdsi_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_maleprimary_pdsi_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "primary",
         sex = "male", 
         aridity = "PDSI",
         developed = "More developed")

m.sum <- summary(m_malesecondary_pdsi_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_malesecondary_pdsi_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "secondary",
         sex = "male", 
         aridity = "PDSI",
         developed = "More developed")

m.sum <- summary(m_maletertiary_pdsi_more, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "name")
m.coef <- m.coef[grep(envindicator, m.coef$name), ]
m.coef <- m.coef[1,]
m.coef_maletertiary_pdsi_more <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE , 
         edu = "tertiary",
         sex = "male", 
         aridity = "PDSI",
         developed = "More developed")


##
##  BIND EVERYTHING TOGETHER ---------------------------------------------------
##


m.coef <- rbind(
  
  m.coef_femalelessprimary_ai_less, m.coef_femalelessprimary_pdsi_less,
  m.coef_femalelessprimary_spei03_less , m.coef_femalelessprimary_spei12_less ,
  m.coef_femaleprimary_ai_less  ,   m.coef_femaleprimary_pdsi_less,
  m.coef_femaleprimary_spei03_less, m.coef_femaleprimary_spei12_less ,
  m.coef_femalesecondary_ai_less  ,   m.coef_femalesecondary_pdsi_less  ,
  m.coef_femalesecondary_spei03_less, m.coef_femalesecondary_spei12_less,
  m.coef_femaletertiary_ai_less  ,   m.coef_femaletertiary_pdsi_less  ,
  m.coef_femaletertiary_spei03_less , m.coef_femaletertiary_spei12_less,

  m.coef_malelessprimary_ai_less     ,   m.coef_malelessprimary_pdsi_less   ,  
  m.coef_malelessprimary_spei03_less  ,    m.coef_malelessprimary_spei12_less  , 
  m.coef_maleprimary_ai_less   ,    m.coef_maleprimary_pdsi_less ,   
  m.coef_maleprimary_spei03_less  , m.coef_maleprimary_spei12_less  ,
  m.coef_malesecondary_ai_less      ,  m.coef_malesecondary_pdsi_less   ,
  m.coef_malesecondary_spei03_less  , m.coef_malesecondary_spei12_less  ,
  m.coef_maletertiary_ai_less    ,   m.coef_maletertiary_pdsi_less  , 
  m.coef_maletertiary_spei03_less  ,  m.coef_maletertiary_spei12_less,

  m.coef_femalelessprimary_ai_more, m.coef_femalelessprimary_pdsi_more,
  m.coef_femalelessprimary_spei03_more , m.coef_femalelessprimary_spei12_more ,
  m.coef_femaleprimary_ai_more  ,   m.coef_femaleprimary_pdsi_more,
  m.coef_femaleprimary_spei03_more, m.coef_femaleprimary_spei12_more ,
  m.coef_femalesecondary_ai_more  ,   m.coef_femalesecondary_pdsi_more  ,
  m.coef_femalesecondary_spei03_more, m.coef_femalesecondary_spei12_more,
  m.coef_femaletertiary_ai_more  ,   m.coef_femaletertiary_pdsi_more  ,
  m.coef_femaletertiary_spei03_more , m.coef_femaletertiary_spei12_more,
  
  m.coef_malelessprimary_ai_more     ,   m.coef_malelessprimary_pdsi_more   ,  
  m.coef_malelessprimary_spei03_more  ,    m.coef_malelessprimary_spei12_more  , 
  m.coef_maleprimary_ai_more   ,    m.coef_maleprimary_pdsi_more ,   
  m.coef_maleprimary_spei03_more  , m.coef_maleprimary_spei12_more  ,
  m.coef_malesecondary_ai_more      ,  m.coef_malesecondary_pdsi_more   ,
  m.coef_malesecondary_spei03_more  , m.coef_malesecondary_spei12_more  ,
  m.coef_maletertiary_ai_more    ,   m.coef_maletertiary_pdsi_more  , 
  m.coef_maletertiary_spei03_more  ,  m.coef_maletertiary_spei12_more
)

m.coef <- m.coef %>% 
  mutate(order = recode(
    edu, 
    "< primary" = 1L,
    "primary" = 2L,
    "secondary" = 3L,
    "tertiary" = 4L
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

g2 <- 
  m.coef %>% 
  ggplot(aes(x=coef, y=fct_reorder(edu, -order), color=sex, group=sex))+ 
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
  coord_cartesian(xlim=c(-0.1,0.25))

g2

m.coef.edusex <- m.coef

save(g2, m.coef.edusex, file="figure_effect differences by education group2.RData")
ggsave(plot = g2, filename="figure_dot whisker coef plot_effect differences by education group2.png",
       width=12, height = 4.5)

