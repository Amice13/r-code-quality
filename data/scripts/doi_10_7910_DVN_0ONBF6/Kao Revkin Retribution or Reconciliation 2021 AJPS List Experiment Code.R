#List Experiment Analysis for AJPS (Kao & Revkin, forthcoming 2021)

library(list)
library(dplyr)
library(readstata13)

list_df <- read.dta13("Kao Revkin Retribution or Reconciliation AJPS 2021 Dataset.dta") %>% #Replace with appropriate file path so R can read the file on your computer

    distinct(R_ID,.keep_all=TRUE) #Counts each respondent only once

t.test(q11_1_num~listexperimentprofile,na.rm=TRUE,conf.level = 0.95, data=list_df) #Welch Two Sample t-test of difference in means between control group ("group 3") and treatment group whose list includes the sensitive item ("group 4")


