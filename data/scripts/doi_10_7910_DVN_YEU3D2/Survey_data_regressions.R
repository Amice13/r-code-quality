
rm(list=ls())
gc()
# pkgFilenames <- read.csv("pkgFilenames.csv", stringsAsFactors = FALSE)[,1]
# install.packages(pkgFilenames, repos = NULL, type = "win.binary")
##name of file were files are saved: pak_r, packare2install

setwd("w:/incoming/maya_hisachon_chiild")

library(stargazer)
library(lubridate)
library(ggplot2)
library(reshape2)
library(dplyr)
library(scales)
library(pastecs)
library(Hmisc)
library(MASS)
library(plyr)
library(foreign)
library(rio)
library(data.table)
library(Matching)
library(MatchIt)
library(rmarkdown)
library(dplyr)
library(textclean)
library(substr)
library(lubridate)
library(stargazer)
library(cobalt)
library(lmtest)
library(sandwich)


load("feb_big_seker_data_1")
load("a_feb_big_seker_1_data")
load("h_feb_big_seker_1_data")
load("j_feb_big_seker_1_data")



feb_big_seker_data <- feb_big_seker_data_1
feb_big_seker_data$trust_nii_high <- ifelse(feb_big_seker_data$trust_nii>=4,1,0)
feb_big_seker_data$trust_high  <- ifelse(feb_big_seker_data$trust_nii>=4 |feb_big_seker_data$trust_government>=4 ,1,0)
feb_big_seker_data$exist_high <- ifelse(feb_big_seker_data$program_will_exist_25y>=4,1,0)
feb_big_seker_data$exist_high_10 <- ifelse(feb_big_seker_data$program_will_exist_10y%>=4,1,0)
feb_big_seker_data$trust_gov_high <- ifelse(feb_big_seker_data$trust_government>=4,1,0)
feb_big_seker_data$cover_high <- ifelse(feb_big_seker_data$cover_expenses_easily>=4,1,0)

j_feb_big_seker_1_data$exist_high <- ifelse(j_feb_big_seker_1_data$trust_government>=4,1,0)
j_feb_big_seker_1_data$trust_nii_high <- ifelse(j_feb_big_seker_1_data$trust_nii>=4,1,0)
j_feb_big_seker_1_data$trust_high  <- ifelse(j_feb_big_seker_1_data$trust_nii>=4 |j_feb_big_seker_1_data$trust_government>=4 ,1,0)
j_feb_big_seker_1_data$exist_high <- ifelse(j_feb_big_seker_1_data$program_will_exist_25y>=4,1,0)
j_feb_big_seker_1_data$cover_high <- ifelse(j_feb_big_seker_1_data$cover_expenses_easily>=4,1,0)
j_feb_big_seker_1_data$exist_high_10 <- ifelse(j_feb_big_seker_1_data$program_will_exist_10y>=4,1,0)
j_feb_big_seker_1_data$trust_gov_high <- ifelse(j_feb_big_seker_1_data$trust_government>=4,1,0)


h_feb_big_seker_1_data$exist_high <- ifelse(h_feb_big_seker_1_data$trust_government>=4,1,0)
h_feb_big_seker_1_data$trust_nii_high <- ifelse(h_feb_big_seker_1_data$trust_nii>=4,1,0)
h_feb_big_seker_1_data$trust_high  <- ifelse(h_feb_big_seker_1_data$trust_nii>=4 |h_feb_big_seker_1_data$trust_government>=4 ,1,0)
h_feb_big_seker_1_data$exist_high <- ifelse(h_feb_big_seker_1_data$program_will_exist_25y>=4,1,0)
h_feb_big_seker_1_data$cover_high <- ifelse(h_feb_big_seker_1_data$cover_expenses_easily>=4,1,0)
h_feb_big_seker_1_data$exist_high_10 <- ifelse(h_feb_big_seker_1_data$program_will_exist_10y>=4,1,0)
h_feb_big_seker_1_data$trust_gov_high <- ifelse(h_feb_big_seker_1_data$trust_government>=4,1,0)


a_feb_big_seker_1_data$exist_high <- ifelse(a_feb_big_seker_1_data$trust_government>=4,1,0)
a_feb_big_seker_1_data$trust_nii_high <- ifelse(a_feb_big_seker_1_data$trust_nii>=4,1,0)
a_feb_big_seker_1_data$trust_high  <- ifelse(a_feb_big_seker_1_data$trust_nii>=4 |a_feb_big_seker_1_data$trust_government>=4 ,1,0)
a_feb_big_seker_1_data$exist_high <- ifelse(a_feb_big_seker_1_data$program_will_exist_25y>=4,1,0)
a_feb_big_seker_1_data$cover_high <- ifelse(a_feb_big_seker_1_data$cover_expenses_easily>=4,1,0)
a_feb_big_seker_1_data$exist_high_10 <- ifelse(a_feb_big_seker_1_data$program_will_exist_10y>=4,1,0)
a_feb_big_seker_1_data$trust_gov_high <- ifelse(a_feb_big_seker_1_data$trust_government>=4,1,0)



feb_big_seker_data$income= 1000*(feb_big_seker_data$father_wage+feb_big_seker_data$mother_wage)
a_feb_big_seker_1_data$income= 1000*(a_feb_big_seker_1_data$father_wage+a_feb_big_seker_1_data$mother_wage)
h_feb_big_seker_1_data$income= 1000*(h_feb_big_seker_1_data$father_wage+h_feb_big_seker_1_data$mother_wage)
j_feb_big_seker_1_data$income= 1000*(j_feb_big_seker_1_data$father_wage+j_feb_big_seker_1_data$mother_wage)


cor(feb_big_seker_data$financial_knowledge,feb_big_seker_data$finance_lit_basic)
feb_big_seker_data$financial_knowledge1 <-ifelse(feb_big_seker_data$financial_knowledge%in%c(4,5),1,0)
cor(feb_big_seker_data$financial_knowledge,feb_big_seker_data$mother_academic)

feb_big_seker_data$financial_knowledge1 <-ifelse(feb_big_seker_data$financial_knowledge%in%c(0,1),1,0)
feb_big_seker_data$finance_lit_basic1 <-ifelse(feb_big_seker_data$finance_lit_basic%in%c(0,1),1,0)
feb_big_seker_data$financial_knowledge2 <-ifelse(feb_big_seker_data$financial_knowledge%in%c(4,5),1,0)
feb_big_seker_data$finance_lit_basic2 <-ifelse(feb_big_seker_data$finance_lit_basic%in%c(3),1,0)
feb_big_seker_data$ex_50_nis_19 <- ifelse(feb_big_seker_data$ex_50_nis==1 & feb_big_seker_data$feb_19==1,1,0)
feb_big_seker_data$k_digital <-ifelse(feb_big_seker_data$device%in%c("DevicePhone","DeviceComputer","DeviceComputer","DeviceTV","DeviceTablet"),1,0)
feb_big_seker_data$no_digital <-ifelse(feb_big_seker_data$device%in%c("DeviceUnknown"),1,NA)
feb_big_seker_data$no_digital <-ifelse(feb_big_seker_data$k_digital==1,0,feb_big_seker_data$no_digital)
feb_big_seker_data$feb19_nodigital <-ifelse(feb_big_seker_data$no_digital==1 & feb_big_seker_data$feb_19==1,1,0)
feb_big_seker_data$nodigital_50_19 <-ifelse(feb_big_seker_data$no_digital==1 & feb_big_seker_data$feb_19==1 & feb_big_seker_data$ex_50_nis==1,1,0)
feb_big_seker_data$phone <-ifelse(feb_big_seker_data$device=="DevicePhone",1,0)
feb_big_seker_data$phone_19 <-ifelse(feb_big_seker_data$phone==1 & feb_big_seker_data$feb_19==1,1,0)
feb_big_seker_data$computer_19 <-ifelse(feb_big_seker_data$computer==1 & feb_big_seker_data$feb_19==1,1,0)
feb_big_seker_data$phone_19_50 <-ifelse(feb_big_seker_data$phone==1 & feb_big_seker_data$ex_50_nis_19==1,1,0)
feb_big_seker_data$phone_50 <-ifelse(feb_big_seker_data$phone==1 & feb_big_seker_data$ex_50_nis==1,1,0)
feb_big_seker_data$computer_19_50 <-ifelse(feb_big_seker_data$computer==1 & feb_big_seker_data$ex_50_nis_19==1,1,0)
feb_big_seker_data$choose_risk <- ifelse(feb_big_seker_data$kod_choice%in%c(120,121,130,131),1,0)
feb_big_seker_data$investment_fund_risk_19 <- ifelse(feb_big_seker_data$kod_choice%in%c(120,121,131,130) & feb_big_seker_data$feb_19==1,1,0)
feb_big_seker_data$choose_invest <- ifelse(feb_big_seker_data$kod_choice%in%c(120,121,130,131,110,111,140,141,210,211,220,230,231),1,0)
feb_big_seker_data$choose_invest_19 <- ifelse(feb_big_seker_data$choose_invest==1 & feb_big_seker_data$feb_19==1,1,0)
feb_big_seker_data$choose_bank <- ifelse(feb_big_seker_data$kod_choice%in%c(210,211,220,230,231),1,0)
feb_big_seker_data$choose_bank_19 <- ifelse(feb_big_seker_data$choose_bank==1 & feb_big_seker_data$feb_19==1,1,0)

feb_seker_digital <- feb_big_seker_data[feb_big_seker_data$k_digital==1,]
feb_seker_non_digital <- feb_big_seker_data[feb_big_seker_data$k_digital==0,]
feb_seker_feb19 <-feb_big_seker_data[feb_big_seker_data$feb_19==1,]
feb_seker_feb19_digital <-feb_seker_digital[feb_seker_digital$feb_19==1,]

#### Arab population sample
cor(a_feb_big_seker_1_data$financial_knowledge,a_feb_big_seker_1_data$finance_lit_basic)
a_feb_big_seker_1_data$financial_knowledge1 <-ifelse(a_feb_big_seker_1_data$financial_knowledge%in%c(4,5),1,0)
table(a_feb_big_seker_1_data$financial_knowledge1)
table(a_feb_big_seker_1_data$finance_lit_basic)
cor(a_feb_big_seker_1_data$financial_knowledge,a_feb_big_seker_1_data$mother_academic)

a_feb_big_seker_1_data$financial_knowledge1 <-ifelse(a_feb_big_seker_1_data$financial_knowledge%in%c(0,1),1,0)
a_feb_big_seker_1_data$finance_lit_basic1 <-ifelse(a_feb_big_seker_1_data$finance_lit_basic%in%c(0,1),1,0)
a_feb_big_seker_1_data$financial_knowledge2 <-ifelse(a_feb_big_seker_1_data$financial_knowledge%in%c(4,5),1,0)
a_feb_big_seker_1_data$finance_lit_basic2 <-ifelse(a_feb_big_seker_1_data$finance_lit_basic%in%c(3),1,0)

a_feb_big_seker_1_data$ex_50_nis_19 <- ifelse(a_feb_big_seker_1_data$ex_50_nis==1 & a_feb_big_seker_1_data$feb_19==1,1,0)

a_feb_big_seker_1_data$k_digital <-ifelse(a_feb_big_seker_1_data$device%in%c("DevicePhone","DeviceComputer","DeviceComputer","DeviceTV","DeviceTablet"),1,0)
a_feb_big_seker_1_data$no_digital <-ifelse(a_feb_big_seker_1_data$device%in%c("DeviceUnknown"),1,NA)
a_feb_big_seker_1_data$no_digital <-ifelse(a_feb_big_seker_1_data$k_digital==1,0,a_feb_big_seker_1_data$no_digital)
a_feb_big_seker_1_data$feb19_nodigital <-ifelse(a_feb_big_seker_1_data$no_digital==1 & a_feb_big_seker_1_data$feb_19==1,1,0)
a_feb_big_seker_1_data$nodigital_50_19 <-ifelse(a_feb_big_seker_1_data$no_digital==1 & a_feb_big_seker_1_data$feb_19==1 & a_feb_big_seker_1_data$ex_50_nis==1,1,0)
a_feb_big_seker_1_data$phone <-ifelse(a_feb_big_seker_1_data$device=="DevicePhone",1,0)
a_feb_big_seker_1_data$phone_19 <-ifelse(a_feb_big_seker_1_data$phone==1 & a_feb_big_seker_1_data$feb_19==1,1,0)
a_feb_big_seker_1_data$computer_19 <-ifelse(a_feb_big_seker_1_data$computer==1 & a_feb_big_seker_1_data$feb_19==1,1,0)
a_feb_big_seker_1_data$phone_19_50 <-ifelse(a_feb_big_seker_1_data$phone==1 & a_feb_big_seker_1_data$ex_50_nis_19==1,1,0)
a_feb_big_seker_1_data$phone_50 <-ifelse(a_feb_big_seker_1_data$phone==1 & a_feb_big_seker_1_data$ex_50_nis==1,1,0)
a_feb_big_seker_1_data$computer_19_50 <-ifelse(a_feb_big_seker_1_data$computer==1 & a_feb_big_seker_1_data$ex_50_nis_19==1,1,0)
a_feb_big_seker_1_data$choose_risk <- ifelse(a_feb_big_seker_1_data$kod_choice%in%c(120,121,130,131),1,0)
a_feb_big_seker_1_data$investment_fund_risk_19 <- ifelse(a_feb_big_seker_1_data$kod_choice%in%c(120,121,131,130) & a_feb_big_seker_1_data$feb_19==1,1,0)
a_feb_big_seker_1_data$choose_invest <- ifelse(a_feb_big_seker_1_data$kod_choice%in%c(120,121,130,131,110,111,140,141,210,211,220,230,231),1,0)
a_feb_big_seker_1_data$choose_invest_19 <- ifelse(a_feb_big_seker_1_data$choose_invest==1 & a_feb_big_seker_1_data$feb_19==1,1,0)
a_feb_big_seker_1_data$choose_bank <- ifelse(a_feb_big_seker_1_data$kod_choice%in%c(210,211,220,230,231),1,0)
a_feb_big_seker_1_data$choose_bank_19 <- ifelse(a_feb_big_seker_1_data$choose_bank==1 & a_feb_big_seker_1_data$feb_19==1,1,0)

a_feb_seker_digital <- a_feb_big_seker_1_data[a_feb_big_seker_1_data$k_digital==1,]
a_feb_seker_non_digital <- a_feb_big_seker_1_data[a_feb_big_seker_1_data$k_digital==0,]
a_feb_seker_feb19 <-a_feb_big_seker_1_data[a_feb_big_seker_1_data$feb_19==1,]
a_feb_seker_feb19_digital <-feb_seker_digital[feb_seker_digital$feb_19==1,]

#### Non minority population
cor(j_feb_big_seker_1_data$financial_knowledge,j_feb_big_seker_1_data$finance_lit_basic)
j_feb_big_seker_1_data$financial_knowledge1 <-ifelse(j_feb_big_seker_1_data$financial_knowledge%in%c(4,5),1,0)
cor(j_feb_big_seker_1_data$financial_knowledge,j_feb_big_seker_1_data$mother_academic)
table(j_feb_big_seker_1_data$financial_knowledge,j_feb_big_seker_1_data$finance_lit_basic)

j_feb_big_seker_1_data$financial_knowledge1 <-ifelse(j_feb_big_seker_1_data$financial_knowledge%in%c(0,1),1,0)
j_feb_big_seker_1_data$finance_lit_basic1 <-ifelse(j_feb_big_seker_1_data$finance_lit_basic%in%c(0,1),1,0)
j_feb_big_seker_1_data$financial_knowledge2 <-ifelse(j_feb_big_seker_1_data$financial_knowledge%in%c(4,5),1,0)
j_feb_big_seker_1_data$finance_lit_basic2 <-ifelse(j_feb_big_seker_1_data$finance_lit_basic%in%c(3),1,0)

j_feb_big_seker_1_data$ex_50_nis_19 <- ifelse(j_feb_big_seker_1_data$ex_50_nis==1 & j_feb_big_seker_1_data$feb_19==1,1,0)
j_feb_big_seker_1_data$k_digital <-ifelse(j_feb_big_seker_1_data$device%in%c("DevicePhone","DeviceComputer","DeviceComputer","DeviceTV","DeviceTablet"),1,0)
j_feb_big_seker_1_data$no_digital <-ifelse(j_feb_big_seker_1_data$device%in%c("DeviceUnknown"),1,NA)
j_feb_big_seker_1_data$no_digital <-ifelse(j_feb_big_seker_1_data$k_digital==1,0,j_feb_big_seker_1_data$no_digital)
j_feb_big_seker_1_data$feb19_nodigital <-ifelse(j_feb_big_seker_1_data$no_digital==1 & j_feb_big_seker_1_data$feb_19==1,1,0)
j_feb_big_seker_1_data$nodigital_50_19 <-ifelse(j_feb_big_seker_1_data$no_digital==1 & j_feb_big_seker_1_data$feb_19==1 & j_feb_big_seker_1_data$ex_50_nis==1,1,0)
j_feb_big_seker_1_data$phone <-ifelse(j_feb_big_seker_1_data$device=="DevicePhone",1,0)
j_feb_big_seker_1_data$phone_19 <-ifelse(j_feb_big_seker_1_data$phone==1 & j_feb_big_seker_1_data$feb_19==1,1,0)
j_feb_big_seker_1_data$computer_19 <-ifelse(j_feb_big_seker_1_data$computer==1 & j_feb_big_seker_1_data$feb_19==1,1,0)
j_feb_big_seker_1_data$phone_19_50 <-ifelse(j_feb_big_seker_1_data$phone==1 & j_feb_big_seker_1_data$ex_50_nis_19==1,1,0)
j_feb_big_seker_1_data$phone_50 <-ifelse(j_feb_big_seker_1_data$phone==1 & j_feb_big_seker_1_data$ex_50_nis==1,1,0)
j_feb_big_seker_1_data$computer_19_50 <-ifelse(j_feb_big_seker_1_data$computer==1 & j_feb_big_seker_1_data$ex_50_nis_19==1,1,0)
j_feb_big_seker_1_data$choose_risk <- ifelse(j_feb_big_seker_1_data$kod_choice%in%c(120,121,130,131),1,0)
j_feb_big_seker_1_data$investment_fund_risk_19 <- ifelse(j_feb_big_seker_1_data$kod_choice%in%c(120,121,131,130) & j_feb_big_seker_1_data$feb_19==1,1,0)
j_feb_big_seker_1_data$choose_invest <- ifelse(j_feb_big_seker_1_data$kod_choice%in%c(120,121,130,131,110,111,140,141,210,211,220,230,231),1,0)
j_feb_big_seker_1_data$choose_invest_19 <- ifelse(j_feb_big_seker_1_data$choose_invest==1 & j_feb_big_seker_1_data$feb_19==1,1,0)
j_feb_big_seker_1_data$choose_bank <- ifelse(j_feb_big_seker_1_data$kod_choice%in%c(210,211,220,230,231),1,0)
j_feb_big_seker_1_data$choose_bank_19 <- ifelse(j_feb_big_seker_1_data$choose_bank==1 & j_feb_big_seker_1_data$feb_19==1,1,0)

j_feb_seker_digital <- j_feb_big_seker_1_data[j_feb_big_seker_1_data$k_digital==1,]
j_feb_seker_non_digital <- j_feb_big_seker_1_data[j_feb_big_seker_1_data$k_digital==0,]
j_feb_seker_feb19 <-j_feb_big_seker_1_data[j_feb_big_seker_1_data$feb_19==1,]
j_feb_seker_feb19_digital <-feb_seker_digital[feb_seker_digital$feb_19==1,]

####
cor(h_feb_big_seker_1_data$financial_knowledge,h_feb_big_seker_1_data$finance_lit_basic)
h_feb_big_seker_1_data$financial_knowledge1 <-ifelse(h_feb_big_seker_1_data$financial_knowledge%in%c(4,5),1,0)
table(h_feb_big_seker_1_data$financial_knowledge1)
table(h_feb_big_seker_1_data$finance_lit_basic)
cor(h_feb_big_seker_1_data$financial_knowledge,h_feb_big_seker_1_data$mother_academic)

h_feb_big_seker_1_data$financial_knowledge1 <-ifelse(h_feb_big_seker_1_data$financial_knowledge%in%c(0,1),1,0)
h_feb_big_seker_1_data$finance_lit_basic1 <-ifelse(h_feb_big_seker_1_data$finance_lit_basic%in%c(0,1),1,0)
h_feb_big_seker_1_data$financial_knowledge2 <-ifelse(h_feb_big_seker_1_data$financial_knowledge%in%c(4,5),1,0)
h_feb_big_seker_1_data$finance_lit_basic2 <-ifelse(h_feb_big_seker_1_data$finance_lit_basic%in%c(3),1,0)

h_feb_big_seker_1_data$ex_50_nis_19 <- ifelse(h_feb_big_seker_1_data$ex_50_nis==1 & h_feb_big_seker_1_data$feb_19==1,1,0)


h_feb_big_seker_1_data$k_digital <-ifelse(h_feb_big_seker_1_data$device%in%c("DevicePhone","DeviceComputer","DeviceComputer","DeviceTV","DeviceTablet"),1,0)
h_feb_big_seker_1_data$no_digital <-ifelse(h_feb_big_seker_1_data$device%in%c("DeviceUnknown"),1,NA)
h_feb_big_seker_1_data$no_digital <-ifelse(h_feb_big_seker_1_data$k_digital==1,0,h_feb_big_seker_1_data$no_digital)
h_feb_big_seker_1_data$feb19_nodigital <-ifelse(h_feb_big_seker_1_data$no_digital==1 & h_feb_big_seker_1_data$feb_19==1,1,0)
h_feb_big_seker_1_data$nodigital_50_19 <-ifelse(h_feb_big_seker_1_data$no_digital==1 & h_feb_big_seker_1_data$feb_19==1 & h_feb_big_seker_1_data$ex_50_nis==1,1,0)
h_feb_big_seker_1_data$phone <-ifelse(h_feb_big_seker_1_data$device=="DevicePhone",1,0)
h_feb_big_seker_1_data$phone_19 <-ifelse(h_feb_big_seker_1_data$phone==1 & h_feb_big_seker_1_data$feb_19==1,1,0)
h_feb_big_seker_1_data$computer_19 <-ifelse(h_feb_big_seker_1_data$computer==1 & h_feb_big_seker_1_data$feb_19==1,1,0)
h_feb_big_seker_1_data$phone_19_50 <-ifelse(h_feb_big_seker_1_data$phone==1 & h_feb_big_seker_1_data$ex_50_nis_19==1,1,0)
h_feb_big_seker_1_data$phone_50 <-ifelse(h_feb_big_seker_1_data$phone==1 & h_feb_big_seker_1_data$ex_50_nis==1,1,0)
h_feb_big_seker_1_data$computer_19_50 <-ifelse(h_feb_big_seker_1_data$computer==1 & h_feb_big_seker_1_data$ex_50_nis_19==1,1,0)
h_feb_big_seker_1_data$choose_risk <- ifelse(h_feb_big_seker_1_data$kod_choice%in%c(120,121,130,131),1,0)
h_feb_big_seker_1_data$investment_fund_risk_19 <- ifelse(h_feb_big_seker_1_data$kod_choice%in%c(120,121,131,130) & h_feb_big_seker_1_data$feb_19==1,1,0)
h_feb_big_seker_1_data$choose_invest <- ifelse(h_feb_big_seker_1_data$kod_choice%in%c(120,121,130,131,110,111,140,141,210,211,220,230,231),1,0)
h_feb_big_seker_1_data$choose_invest_19 <- ifelse(h_feb_big_seker_1_data$choose_invest==1 & h_feb_big_seker_1_data$feb_19==1,1,0)
h_feb_big_seker_1_data$choose_bank <- ifelse(h_feb_big_seker_1_data$kod_choice%in%c(210,211,220,230,231),1,0)
h_feb_big_seker_1_data$choose_bank_19 <- ifelse(h_feb_big_seker_1_data$choose_bank==1 & h_feb_big_seker_1_data$feb_19==1,1,0)

h_feb_seker_digital <- h_feb_big_seker_1_data[h_feb_big_seker_1_data$k_digital==1,]
h_feb_seker_non_digital <- h_feb_big_seker_1_data[h_feb_big_seker_1_data$k_digital==0,]
h_feb_seker_feb19 <-h_feb_big_seker_1_data[h_feb_big_seker_1_data$feb_19==1,]
h_feb_seker_feb19_digital <-feb_seker_digital[feb_seker_digital$feb_19==1,]

###
feb_big_seker_data$choose_clean <- feb_big_seker_data$choose_new
feb_big_seker_data$choose_clean <- ifelse(feb_big_seker_data$kod_choice%in%c(310,410),0,feb_big_seker_data$choose_clean)
feb_big_seker_data$kosher <- ifelse(feb_big_seker_data$kod_choice%in%c(140,141),1,0)
feb_big_seker_data$choose_clean_19 <- ifelse(feb_big_seker_data$kod_choice%in%c(310,410) & feb_big_seker_data$feb_19==1,0,feb_big_seker_data$choose_clean)
feb_big_seker_data$kosher_19 <- ifelse(feb_big_seker_data$kod_choice%in%c(140,141)& feb_big_seker_data$feb_19==1,1,0)

a_feb_big_seker_1_data$choose_clean <- a_feb_big_seker_1_data$choose_new
a_feb_big_seker_1_data$choose_clean <- ifelse(a_feb_big_seker_1_data$kod_choice%in%c(310,410),0,a_feb_big_seker_1_data$choose_clean)
a_feb_big_seker_1_data$kosher <- ifelse(a_feb_big_seker_1_data$kod_choice%in%c(140,141),1,0)
a_feb_big_seker_1_data$choose_clean_19 <- ifelse(a_feb_big_seker_1_data$kod_choice%in%c(310,410) & a_feb_big_seker_1_data$feb_19==1,0,a_feb_big_seker_1_data$choose_clean)
a_feb_big_seker_1_data$kosher_19 <- ifelse(a_feb_big_seker_1_data$kod_choice%in%c(140,141)& a_feb_big_seker_1_data$feb_19==1,1,0)


h_feb_big_seker_1_data$choose_clean <- h_feb_big_seker_1_data$choose_new
h_feb_big_seker_1_data$choose_clean <- ifelse(h_feb_big_seker_1_data$kod_choice%in%c(310,410),0,h_feb_big_seker_1_data$choose_clean)
h_feb_big_seker_1_data$kosher <- ifelse(h_feb_big_seker_1_data$kod_choice%in%c(140,141),1,0)
h_feb_big_seker_1_data$choose_clean_19 <- ifelse(h_feb_big_seker_1_data$kod_choice%in%c(310,410) & h_feb_big_seker_1_data$feb_19==1,0,h_feb_big_seker_1_data$choose_clean)
h_feb_big_seker_1_data$kosher_19 <- ifelse(h_feb_big_seker_1_data$kod_choice%in%c(140,141)& h_feb_big_seker_1_data$feb_19==1,1,0)


j_feb_big_seker_1_data$choose_clean <- j_feb_big_seker_1_data$choose_new
j_feb_big_seker_1_data$choose_clean <- ifelse(j_feb_big_seker_1_data$kod_choice%in%c(310,410),0,j_feb_big_seker_1_data$choose_clean)
j_feb_big_seker_1_data$kosher <- ifelse(j_feb_big_seker_1_data$kod_choice%in%c(140,141),1,0)
j_feb_big_seker_1_data$choose_clean_19 <- ifelse(j_feb_big_seker_1_data$kod_choice%in%c(310,410) & j_feb_big_seker_1_data$feb_19==1,0,j_feb_big_seker_1_data$choose_clean)
j_feb_big_seker_1_data$kosher_19 <- ifelse(j_feb_big_seker_1_data$kod_choice%in%c(140,141)& j_feb_big_seker_1_data$feb_19==1,1,0)


####
#########################Table 5
##for robustness change trust_high to ither trust variables
##change to logit regression

prob_reg1=lm(feb_19~  sms_feb*trust_high+sms_feb*financial_knowledge1+sms_feb*finance_lit_basic1+
               parents_hardi+parents_arab+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             
             data= feb_big_seker_data,weights=weights)

summary(prob_reg1)

prob_reg2=lm(feb_19~  sms_feb*trust_high+sms_feb*financial_knowledge1+sms_feb*finance_lit_basic1+
               parents_hardi+parents_arab+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             data= a_feb_big_seker_1_data,weights=weights)

summary(prob_reg2)


prob_reg3=lm(feb_19~   sms_feb*trust_high+sms_feb*financial_knowledge1+sms_feb*finance_lit_basic1+
               parents_hardi+parents_arab+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             data= h_feb_big_seker_1_data,weights=weights)

summary(prob_reg3)

prob_reg4=lm(feb_19~  sms_feb*trust_high+sms_feb*financial_knowledge1+sms_feb*finance_lit_basic1+
               parents_hardi+parents_arab+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             data= j_feb_big_seker_1_data,weights=weights)

summary(prob_reg4)

coef1 <- vcovHC (prob_reg1)
coef2 <- vcovHC (prob_reg2)
coef3 <- vcovHC (prob_reg3)
coef4 <- vcovHC (prob_reg4)

robust_se1 <-(sqrt(diag(coef1)))
robust_se2 <-(sqrt(diag(coef2)))
robust_se3 <-(sqrt(diag(coef3)))
robust_se4 <-(sqrt(diag(coef4)))
robust_se <- c(robust_se1,robust_se2,robust_se3,robust_se4)


stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Made any choice by Febuary 19th",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb","sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS","Received SMS*Arab","Received SMS*Ultra-Orthodox", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="trustlit_choose_long.htm",
          no.space=TRUE, digits=2, se =list(robust_se))
#################
##############


########

prob_reg1=lm(ex_50_nis_19~ sms_feb*trust_high+sms_feb*financial_knowledge1+sms_feb*finance_lit_basic1+
               parents_hardi+parents_arab+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             
             data= feb_big_seker_data,weights=weights)

summary(prob_reg1)

prob_reg2=lm(ex_50_nis_19~ sms_feb*trust_high+sms_feb*financial_knowledge1+sms_feb*finance_lit_basic1+
               parents_hardi+parents_arab+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             data= a_feb_big_seker_1_data,weights=weights)

summary(prob_reg2)


prob_reg3=lm(ex_50_nis_19~  sms_feb*trust_high+sms_feb*financial_knowledge1+sms_feb*finance_lit_basic1+
               parents_hardi+parents_arab+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             data= h_feb_big_seker_1_data,weights=weights)

summary(prob_reg3)

prob_reg4=lm(ex_50_nis_19~ sms_feb*trust_high+sms_feb*financial_knowledge1+sms_feb*finance_lit_basic1+
               parents_hardi+parents_arab+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             data= j_feb_big_seker_1_data,weights=weights)

summary(prob_reg4)

coef1 <- vcovHC (prob_reg1)
coef2 <- vcovHC (prob_reg2)
coef3 <- vcovHC (prob_reg3)
coef4 <- vcovHC (prob_reg4)

robust_se1 <-(sqrt(diag(coef1)))
robust_se2 <-(sqrt(diag(coef2)))
robust_se3 <-(sqrt(diag(coef3)))
robust_se4 <-(sqrt(diag(coef4)))
robust_se <- c(robust_se1,robust_se2,robust_se3,robust_se4)


stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Choose 50 by Febuary 19th",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb","sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS","Received SMS*Arab","Received SMS*Ultra-Orthodox", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="50_trustlit_choose_long.htm", 
          no.space=TRUE, digits=2, se =list(robust_se))
#################
########Not in table but discussed
prob_reg1=lm(phone_19~ sms_feb*trust_high+sms_feb*financial_knowledge1+sms_feb*finance_lit_basic1+
               parents_hardi+parents_arab+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             
             data= feb_big_seker_data,weights=weights)

summary(prob_reg1)

prob_reg2=lm(phone_19~ sms_feb*trust_high+sms_feb*trust_high+sms_feb*financial_knowledge1+sms_feb*finance_lit_basic1+
               parents_hardi+parents_arab+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             data= a_feb_big_seker_1_data,weights=weights)

summary(prob_reg2)


prob_reg3=lm(phone_19~ sms_feb*trust_high+ sms_feb*financial_knowledge1+sms_feb*finance_lit_basic1+
               parents_hardi+parents_arab+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             data= h_feb_big_seker_1_data,weights=weights)

summary(prob_reg3)

prob_reg4=lm(phone_19~ sms_feb*trust_high+sms_feb*financial_knowledge1+sms_feb*finance_lit_basic1+
               parents_hardi+parents_arab+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             data= j_feb_big_seker_1_data,weights=weights)

summary(prob_reg4)

coef1 <- vcovHC (prob_reg1)
coef2 <- vcovHC (prob_reg2)
coef3 <- vcovHC (prob_reg3)
coef4 <- vcovHC (prob_reg4)

robust_se1 <-(sqrt(diag(coef1)))
robust_se2 <-(sqrt(diag(coef2)))
robust_se3 <-(sqrt(diag(coef3)))
robust_se4 <-(sqrt(diag(coef4)))
robust_se <- c(robust_se1,robust_se2,robust_se3,robust_se4)


stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Choose by phone by Febuary 19th",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb","sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS","Received SMS*Arab","Received SMS*Ultra-Orthodox", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="phone_trust_lit_long.htm", 
          no.space=TRUE, digits=2, se =list(robust_se))
#################
#################

prob_reg1=lm(investment_fund_risk_19~sms_feb*trust_high+ sms_feb*financial_knowledge1+sms_feb*finance_lit_basic1+
               parents_hardi+parents_arab+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             
             data= feb_big_seker_data,weights=weights)

summary(prob_reg1)

prob_reg2=lm(investment_fund_risk_19~ sms_feb*trust_high+sms_feb*financial_knowledge1+sms_feb*finance_lit_basic1+
               parents_hardi+parents_arab+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             data= a_feb_big_seker_1_data,weights=weights)

summary(prob_reg2)


prob_reg3=lm(investment_fund_risk_19~ sms_feb*trust_high+ sms_feb*financial_knowledge1+sms_feb*finance_lit_basic1+
               parents_hardi+parents_arab+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             data= h_feb_big_seker_1_data,weights=weights)

summary(prob_reg3)

prob_reg4=lm(investment_fund_risk_19~ sms_feb*trust_high+sms_feb*financial_knowledge1+sms_feb*finance_lit_basic1+
               parents_hardi+parents_arab+ 
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             data= j_feb_big_seker_1_data,weights=weights)

summary(prob_reg4)

coef1 <- vcovHC (prob_reg1)
coef2 <- vcovHC (prob_reg2)
coef3 <- vcovHC (prob_reg3)
coef4 <- vcovHC (prob_reg4)

robust_se1 <-(sqrt(diag(coef1)))
robust_se2 <-(sqrt(diag(coef2)))
robust_se3 <-(sqrt(diag(coef3)))
robust_se4 <-(sqrt(diag(coef4)))
robust_se <- c(robust_se1,robust_se2,robust_se3,robust_se4)


stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Choose risky track by Febuary 19th",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb","sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS","Received SMS*Arab","Received SMS*Ultra-Orthodox", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="risk_trustlit_long.htm", 
          no.space=TRUE, digits=2, se =list(robust_se))

################
###############


prob_reg1=lm(choose_bank_19~ sms_feb*trust_high+sms_feb*financial_knowledge1+sms_feb*finance_lit_basic1+
               parents_hardi+parents_arab+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             
             data= feb_big_seker_data,weights=weights)

summary(prob_reg1)

prob_reg2=lm(choose_bank_19~sms_feb*trust_high+ sms_feb*financial_knowledge1+sms_feb*finance_lit_basic1+
               parents_hardi+parents_arab+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             data= a_feb_big_seker_1_data,weights=weights)

summary(prob_reg2)


prob_reg3=lm(choose_bank_19~ sms_feb*trust_high+ sms_feb*financial_knowledge1+sms_feb*finance_lit_basic1+
               parents_hardi+parents_arab+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             data= h_feb_big_seker_1_data,weights=weights)

summary(prob_reg3)

prob_reg4=lm(choose_bank_19~ sms_feb*trust_high+sms_feb*financial_knowledge1+sms_feb*finance_lit_basic1+
               parents_hardi+parents_arab+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             data= j_feb_big_seker_1_data,weights=weights)

summary(prob_reg4)

coef1 <- vcovHC (prob_reg1)
coef2 <- vcovHC (prob_reg2)
coef3 <- vcovHC (prob_reg3)
coef4 <- vcovHC (prob_reg4)

robust_se1 <-(sqrt(diag(coef1)))
robust_se2 <-(sqrt(diag(coef2)))
robust_se3 <-(sqrt(diag(coef3)))
robust_se4 <-(sqrt(diag(coef4)))
robust_se <- c(robust_se1,robust_se2,robust_se3,robust_se4)


stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Choose bank track by Febuary 19th",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb","sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS","Received SMS*Arab","Received SMS*Ultra-Orthodox", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="bankseker_trustlit_long.htm", 
          no.space=TRUE, digits=2, se =list(robust_se))
###############

##robust specification - other specification of made any choice
prob_reg1=lm(choose_clean_19~ sms_feb*trust_high+sms_feb*financial_knowledge1+sms_feb*finance_lit_basic1+
               parents_hardi+parents_arab+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             
             data= feb_big_seker_data,weights=weights)

summary(prob_reg1)

prob_reg2=lm(choose_clean_19~sms_feb*trust_high+ sms_feb*financial_knowledge1+sms_feb*finance_lit_basic1+
               parents_hardi+parents_arab+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             data= a_feb_big_seker_1_data,weights=weights)

summary(prob_reg2)


prob_reg3=lm(choose_clean_19~sms_feb*trust_high+  sms_feb*financial_knowledge1+sms_feb*finance_lit_basic1+
               parents_hardi+parents_arab+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             data= h_feb_big_seker_1_data,weights=weights)

summary(prob_reg3)

prob_reg4=lm(choose_clean_19~ sms_feb*trust_high+sms_feb*financial_knowledge1+sms_feb*finance_lit_basic1+
               parents_hardi+parents_arab+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             data= j_feb_big_seker_1_data,weights=weights)

summary(prob_reg4)

coef1 <- vcovHC (prob_reg1)
coef2 <- vcovHC (prob_reg2)
coef3 <- vcovHC (prob_reg3)
coef4 <- vcovHC (prob_reg4)

robust_se1 <-(sqrt(diag(coef1)))
robust_se2 <-(sqrt(diag(coef2)))
robust_se3 <-(sqrt(diag(coef3)))
robust_se4 <-(sqrt(diag(coef4)))
robust_se <- c(robust_se1,robust_se2,robust_se3,robust_se4)


stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Choose clean track by Febuary 19th",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb","sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS","Received SMS*Arab","Received SMS*Ultra-Orthodox", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="cleaneker_trustlit_long.htm", 
          no.space=TRUE, digits=2, se =list(robust_se))


###############


prob_reg1=lm(kosher_19~ sms_feb*trust_high+sms_feb*financial_knowledge1+sms_feb*finance_lit_basic1+
               parents_hardi+parents_arab+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             
             data= feb_big_seker_data,weights=weights)

summary(prob_reg1)

prob_reg2=lm(kosher_19~ sms_feb*trust_high+sms_feb*financial_knowledge1+sms_feb*finance_lit_basic1+
               parents_hardi+parents_arab+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             data= a_feb_big_seker_1_data,weights=weights)

summary(prob_reg2)


prob_reg3=lm(kosher_19~ sms_feb*trust_high+ sms_feb*financial_knowledge1+sms_feb*finance_lit_basic1+
               parents_hardi+parents_arab+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             data= h_feb_big_seker_1_data,weights=weights)

summary(prob_reg3)

prob_reg4=lm(kosher_19~ sms_feb*trust_high+sms_feb*financial_knowledge1+sms_feb*finance_lit_basic1+
               parents_hardi+parents_arab+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             data= j_feb_big_seker_1_data,weights=weights)

summary(prob_reg4)

coef1 <- vcovHC (prob_reg1)
coef2 <- vcovHC (prob_reg2)
coef3 <- vcovHC (prob_reg3)
coef4 <- vcovHC (prob_reg4)

robust_se1 <-(sqrt(diag(coef1)))
robust_se2 <-(sqrt(diag(coef2)))
robust_se3 <-(sqrt(diag(coef3)))
robust_se4 <-(sqrt(diag(coef4)))
robust_se <- c(robust_se1,robust_se2,robust_se3,robust_se4)


stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Choose kosher track by Febuary 19th",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb","sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS","Received SMS*Arab","Received SMS*Ultra-Orthodox", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="kosherker_trustlit_long.htm", 
          no.space=TRUE, digits=2, se =list(robust_se))


########Table 6
prob_reg1=lm(feb_19~ sms_feb*cover_high +sms_feb*trust_high+sms_feb*financial_knowledge1+sms_feb*finance_lit_basic1+
               parents_hardi+parents_arab+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             
             data= feb_big_seker_data,weights=weights)

summary(prob_reg1)

prob_reg2=lm(feb_19~ sms_feb*cover_high+ sms_feb*trust_high+sms_feb*financial_knowledge1+sms_feb*finance_lit_basic1+
               parents_hardi+parents_arab+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             data= a_feb_big_seker_1_data,weights=weights)

summary(prob_reg2)


prob_reg3=lm(feb_19~ sms_feb*cover_high + sms_feb*trust_high+sms_feb*financial_knowledge1+sms_feb*finance_lit_basic1+
               parents_hardi+parents_arab+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             data= h_feb_big_seker_1_data,weights=weights)

summary(prob_reg3)

prob_reg4=lm(feb_19~sms_feb*cover_high + sms_feb*trust_high+sms_feb*financial_knowledge1+sms_feb*finance_lit_basic1+
               parents_hardi+parents_arab+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             data= j_feb_big_seker_1_data,weights=weights)

summary(prob_reg4)

coef1 <- vcovHC (prob_reg1)
coef2 <- vcovHC (prob_reg2)
coef3 <- vcovHC (prob_reg3)
coef4 <- vcovHC (prob_reg4)

robust_se1 <-(sqrt(diag(coef1)))
robust_se2 <-(sqrt(diag(coef2)))
robust_se3 <-(sqrt(diag(coef3)))
robust_se4 <-(sqrt(diag(coef4)))
robust_se <- c(robust_se1,robust_se2,robust_se3,robust_se4)


stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Made any choice by Febuary 19th",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb","sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS","Received SMS*Arab","Received SMS*Ultra-Orthodox", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="covertrustlit_choose_long.htm",
          no.space=TRUE, digits=2, se =list(robust_se))
#################
##############
###################################Adittional statistics - 
###check if savings decreased and additional stats

rm(list=ls())
gc()

##file before matching
load ("cda_seker_new_before_patition")


cda_seker_new_before_patition$same_more_savings <- ifelse(cda_seker_new_before_patition$change_deposits_child_saving%in%c(1,2),1,0)

a_cda_seker<- subset(cda_seker_new_before_patition,cda_seker_new_before_patition$parents_arab==1)
h_cda_seker<- subset(cda_seker_new_before_patition,cda_seker_new_before_patition$parents_hardi==1)
j_cda_seker<- subset(cda_seker_new_before_patition,cda_seker_new_before_patition$parents_hardi==0 &cda_seker_new_before_patition$parents_arab==0) 


###this is for all children, not for those that already made a choice

cda_seker_new_before_patition$trust_gov_high <- ifelse(cda_seker_new_before_patition$trust_government>=4,1,0)
cda_seker_new_before_patition$trust_nii_high <- ifelse(cda_seker_new_before_patition$trust_nii>=4,1,0)
cda_seker_new_before_patition$trust_high  <- ifelse(cda_seker_new_before_patition$trust_nii>=4 |cda_seker_new_before_patition$trust_government>=4 ,1,0)
cda_seker_new_before_patition$exist_high <- ifelse(cda_seker_new_before_patition$program_will_exist_25y>=4,1,0)
cda_seker_new_before_patition$cover_high <- ifelse(cda_seker_new_before_patition$cover_expenses_easily%in%c(4,5),1,0)

j_cda_seker$trust_gov_high <- ifelse(j_cda_seker$trust_government>=4,1,0)
j_cda_seker$trust_nii_high <- ifelse(j_cda_seker$trust_nii>=4,1,0)
j_cda_seker$trust_high  <- ifelse(j_cda_seker$trust_nii>=4 |j_cda_seker$trust_government>=4 ,1,0)
j_cda_seker$exist_high <- ifelse(j_cda_seker$program_will_exist_25y>=4,1,0)
j_cda_seker$cover_high <- ifelse(j_cda_seker$cover_expenses_easily>=4,1,0)

h_cda_seker$trust_gov_high <- ifelse(h_cda_seker$trust_government>=4,1,0)
h_cda_seker$trust_nii_high <- ifelse(h_cda_seker$trust_nii>=4,1,0)
h_cda_seker$trust_high  <- ifelse(h_cda_seker$trust_nii>=4 |h_cda_seker$trust_government>=4 ,1,0)
h_cda_seker$exist_high <- ifelse(h_cda_seker$program_will_exist_25y>=4,1,0)
h_cda_seker$cover_high <- ifelse(h_cda_seker$cover_expenses_easily>=4,1,0)

a_cda_seker$trust_gov_high <- ifelse(a_cda_seker$trust_government>=4,1,0)
a_cda_seker$trust_nii_high <- ifelse(a_cda_seker$trust_nii>=4,1,0)
a_cda_seker$trust_high  <- ifelse(a_cda_seker$trust_nii>=4 |a_cda_seker$trust_government>=4 ,1,0)
a_cda_seker$exist_high <- ifelse(a_cda_seker$program_will_exist_25y>=4,1,0)
a_cda_seker$cover_high <- ifelse(a_cda_seker$cover_expenses_easily>=4,1,0)

cda_seker_new_before_patition$income= (cda_seker_new_before_patition$father_wage+cda_seker_new_before_patition$mother_wage)/1000
a_cda_seker$income= (a_cda_seker$father_wage+a_cda_seker$mother_wage)/1000
h_cda_seker$income= (h_cda_seker$father_wage+h_cda_seker$mother_wage)/1000
j_cda_seker$income= (j_cda_seker$father_wage+j_cda_seker$mother_wage)/1000


cor(cda_seker_new_before_patition$financial_knowledge,cda_seker_new_before_patition$finance_lit_basic)
cda_seker_new_before_patition$financial_knowledge1 <-ifelse(cda_seker_new_before_patition$financial_knowledge%in%c(4,5),1,0)
table(cda_seker_new_before_patition$financial_knowledge1)
table(cda_seker_new_before_patition$finance_lit_basic)
cor(cda_seker_new_before_patition$financial_knowledge,cda_seker_new_before_patition$mother_academic)
table(cda_seker_new_before_patition$financial_knowledge,cda_seker_new_before_patition$finance_lit_basic)
cda_seker_new_before_patition$financial_knowledge1 <-ifelse(cda_seker_new_before_patition$financial_knowledge%in%c(0,1),1,0)
cda_seker_new_before_patition$finance_lit_basic1 <-ifelse(cda_seker_new_before_patition$finance_lit_basic%in%c(0,1),1,0)
cda_seker_new_before_patition$financial_knowledge2 <-ifelse(cda_seker_new_before_patition$financial_knowledge%in%c(4,5),1,0)
cda_seker_new_before_patition$finance_lit_basic2 <-ifelse(cda_seker_new_before_patition$finance_lit_basic%in%c(3),1,0)

table(cda_seker_new_before_patition$financial_knowledge1,cda_seker_new_before_patition$finance_lit_basic1)
cda_seker_new_before_patition$ex_50_nis_19 <- ifelse(cda_seker_new_before_patition$ex_50_nis==1 & cda_seker_new_before_patition$feb_19==1,1,0)
cda_seker_new_before_patition$k_digital <-ifelse(cda_seker_new_before_patition$device%in%c("DevicePhone","DeviceComputer","DeviceComputer","DeviceTV","DeviceTablet"),1,0)
cda_seker_new_before_patition$no_digital <-ifelse(cda_seker_new_before_patition$device%in%c("DeviceUnknown"),1,NA)
cda_seker_new_before_patition$no_digital <-ifelse(cda_seker_new_before_patition$k_digital==1,0,cda_seker_new_before_patition$no_digital)
cda_seker_new_before_patition$feb19_nodigital <-ifelse(cda_seker_new_before_patition$no_digital==1 & cda_seker_new_before_patition$feb_19==1,1,0)
cda_seker_new_before_patition$nodigital_50_19 <-ifelse(cda_seker_new_before_patition$no_digital==1 & cda_seker_new_before_patition$feb_19==1 & cda_seker_new_before_patition$ex_50_nis==1,1,0)
cda_seker_new_before_patition$phone <-ifelse(cda_seker_new_before_patition$device=="DevicePhone",1,0)
cda_seker_new_before_patition$phone_19 <-ifelse(cda_seker_new_before_patition$phone==1 & cda_seker_new_before_patition$feb_19==1,1,0)
cda_seker_new_before_patition$computer <-ifelse(cda_seker_new_before_patition$device=="DeviceComputer",1,0)
cda_seker_new_before_patition$computer_19 <-ifelse(cda_seker_new_before_patition$computer==1 & cda_seker_new_before_patition$feb_19==1,1,0)
cda_seker_new_before_patition$phone_19_50 <-ifelse(cda_seker_new_before_patition$phone==1 & cda_seker_new_before_patition$ex_50_nis_19==1,1,0)
cda_seker_new_before_patition$phone_50 <-ifelse(cda_seker_new_before_patition$phone==1 & cda_seker_new_before_patition$ex_50_nis==1,1,0)
cda_seker_new_before_patition$computer_19_50 <-ifelse(cda_seker_new_before_patition$computer==1 & cda_seker_new_before_patition$ex_50_nis_19==1,1,0)
cda_seker_new_before_patition$choose_risk <- ifelse(cda_seker_new_before_patition$kod_choice%in%c(120,121,130,131),1,0)
cda_seker_new_before_patition$investment_fund_risk_19 <- ifelse(cda_seker_new_before_patition$kod_choice%in%c(120,121,131,130) & cda_seker_new_before_patition$feb_19==1,1,0)
cda_seker_new_before_patition$choose_invest <- ifelse(cda_seker_new_before_patition$kod_choice%in%c(120,121,130,131,110,111,140,141,210,211,220,230,231),1,0)
cda_seker_new_before_patition$choose_invest_19 <- ifelse(cda_seker_new_before_patition$choose_invest==1 & cda_seker_new_before_patition$feb_19==1,1,0)
cda_seker_new_before_patition$choose_bank <- ifelse(cda_seker_new_before_patition$kod_choice%in%c(210,211,220,230,231),1,0)
cda_seker_new_before_patition$choose_bank_19 <- ifelse(cda_seker_new_before_patition$choose_bank==1 & cda_seker_new_before_patition$feb_19==1,1,0)
feb_seker_digital <- cda_seker_new_before_patition[cda_seker_new_before_patition$k_digital==1,]
feb_seker_non_digital <- cda_seker_new_before_patition[cda_seker_new_before_patition$k_digital==0,]
feb_seker_feb19 <-cda_seker_new_before_patition[cda_seker_new_before_patition$feb_19==1,]
feb_seker_feb19_digital <-feb_seker_digital[feb_seker_digital$feb_19==1,]

####
cor(a_cda_seker$financial_knowledge,a_cda_seker$finance_lit_basic)
a_cda_seker$financial_knowledge1 <-ifelse(a_cda_seker$financial_knowledge%in%c(4,5),1,0)
table(a_cda_seker$financial_knowledge1)
table(a_cda_seker$finance_lit_basic)
cor(a_cda_seker$financial_knowledge,a_cda_seker$mother_academic)
table(a_cda_seker$financial_knowledge,a_cda_seker$finance_lit_basic)
a_cda_seker$financial_knowledge1 <-ifelse(a_cda_seker$financial_knowledge%in%c(0,1),1,0)
a_cda_seker$finance_lit_basic1 <-ifelse(a_cda_seker$finance_lit_basic%in%c(0,1),1,0)
a_cda_seker$financial_knowledge2 <-ifelse(a_cda_seker$financial_knowledge%in%c(4,5),1,0)
a_cda_seker$finance_lit_basic2 <-ifelse(a_cda_seker$finance_lit_basic%in%c(3),1,0)

table(a_cda_seker$financial_knowledge1,a_cda_seker$finance_lit_basic1)
a_cda_seker$ex_50_nis_19 <- ifelse(a_cda_seker$ex_50_nis==1 & a_cda_seker$feb_19==1,1,0)
a_cda_seker$k_digital <-ifelse(a_cda_seker$device%in%c("DevicePhone","DeviceComputer","DeviceComputer","DeviceTV","DeviceTablet"),1,0)
a_cda_seker$no_digital <-ifelse(a_cda_seker$device%in%c("DeviceUnknown"),1,NA)
a_cda_seker$no_digital <-ifelse(a_cda_seker$k_digital==1,0,a_cda_seker$no_digital)
a_cda_seker$feb19_nodigital <-ifelse(a_cda_seker$no_digital==1 & a_cda_seker$feb_19==1,1,0)
a_cda_seker$nodigital_50_19 <-ifelse(a_cda_seker$no_digital==1 & a_cda_seker$feb_19==1 & a_cda_seker$ex_50_nis==1,1,0)
a_cda_seker$phone <-ifelse(a_cda_seker$device=="DevicePhone",1,0)
a_cda_seker$computer <-ifelse(a_cda_seker$device=="DeviceComputer",1,0)
a_cda_seker$phone_19 <-ifelse(a_cda_seker$phone==1 & a_cda_seker$feb_19==1,1,0)
a_cda_seker$computer_19 <-ifelse(a_cda_seker$computer==1 & a_cda_seker$feb_19==1,1,0)
a_cda_seker$phone_19_50 <-ifelse(a_cda_seker$phone==1 & a_cda_seker$ex_50_nis_19==1,1,0)
a_cda_seker$phone_50 <-ifelse(a_cda_seker$phone==1 & a_cda_seker$ex_50_nis==1,1,0)
a_cda_seker$computer_19_50 <-ifelse(a_cda_seker$computer==1 & a_cda_seker$ex_50_nis_19==1,1,0)
a_cda_seker$choose_risk <- ifelse(a_cda_seker$kod_choice%in%c(120,121,130,131),1,0)
a_cda_seker$investment_fund_risk_19 <- ifelse(a_cda_seker$kod_choice%in%c(120,121,131,130) & a_cda_seker$feb_19==1,1,0)
a_cda_seker$choose_invest <- ifelse(a_cda_seker$kod_choice%in%c(120,121,130,131,110,111,140,141,210,211,220,230,231),1,0)
a_cda_seker$choose_invest_19 <- ifelse(a_cda_seker$choose_invest==1 & a_cda_seker$feb_19==1,1,0)
a_cda_seker$choose_bank <- ifelse(a_cda_seker$kod_choice%in%c(210,211,220,230,231),1,0)
a_cda_seker$choose_bank_19 <- ifelse(a_cda_seker$choose_bank==1 & a_cda_seker$feb_19==1,1,0)

a_feb_seker_digital <- a_cda_seker[a_cda_seker$k_digital==1,]
a_feb_seker_non_digital <- a_cda_seker[a_cda_seker$k_digital==0,]
a_feb_seker_feb19 <-a_cda_seker[a_cda_seker$feb_19==1,]
a_feb_seker_feb19_digital <-feb_seker_digital[feb_seker_digital$feb_19==1,]

####
cor(j_cda_seker$financial_knowledge,j_cda_seker$finance_lit_basic)
j_cda_seker$financial_knowledge1 <-ifelse(j_cda_seker$financial_knowledge%in%c(4,5),1,0)
table(j_cda_seker$financial_knowledge1)
table(j_cda_seker$finance_lit_basic)
cor(j_cda_seker$financial_knowledge,j_cda_seker$mother_academic)
table(j_cda_seker$financial_knowledge,j_cda_seker$finance_lit_basic)
j_cda_seker$financial_knowledge1 <-ifelse(j_cda_seker$financial_knowledge%in%c(0,1),1,0)
j_cda_seker$finance_lit_basic1 <-ifelse(j_cda_seker$finance_lit_basic%in%c(0,1),1,0)
j_cda_seker$financial_knowledge2 <-ifelse(j_cda_seker$financial_knowledge%in%c(4,5),1,0)
j_cda_seker$finance_lit_basic2 <-ifelse(j_cda_seker$finance_lit_basic%in%c(3),1,0)
table(j_cda_seker$financial_knowledge1,j_cda_seker$finance_lit_basic1)
j_cda_seker$ex_50_nis_19 <- ifelse(j_cda_seker$ex_50_nis==1 & j_cda_seker$feb_19==1,1,0)
j_cda_seker$k_digital <-ifelse(j_cda_seker$device%in%c("DevicePhone","DeviceComputer","DeviceComputer","DeviceTV","DeviceTablet"),1,0)
j_cda_seker$no_digital <-ifelse(j_cda_seker$device%in%c("DeviceUnknown"),1,NA)
j_cda_seker$no_digital <-ifelse(j_cda_seker$k_digital==1,0,j_cda_seker$no_digital)
j_cda_seker$feb19_nodigital <-ifelse(j_cda_seker$no_digital==1 & j_cda_seker$feb_19==1,1,0)
j_cda_seker$nodigital_50_19 <-ifelse(j_cda_seker$no_digital==1 & j_cda_seker$feb_19==1 & j_cda_seker$ex_50_nis==1,1,0)
j_cda_seker$phone <-ifelse(j_cda_seker$device=="DevicePhone",1,0)
j_cda_seker$computer <-ifelse(j_cda_seker$device=="DeviceComputer",1,0)
j_cda_seker$phone_19 <-ifelse(j_cda_seker$phone==1 & j_cda_seker$feb_19==1,1,0)
j_cda_seker$computer_19 <-ifelse(j_cda_seker$computer==1 & j_cda_seker$feb_19==1,1,0)
j_cda_seker$phone_19_50 <-ifelse(j_cda_seker$phone==1 & j_cda_seker$ex_50_nis_19==1,1,0)
j_cda_seker$phone_50 <-ifelse(j_cda_seker$phone==1 & j_cda_seker$ex_50_nis==1,1,0)
j_cda_seker$computer_19_50 <-ifelse(j_cda_seker$computer==1 & j_cda_seker$ex_50_nis_19==1,1,0)
j_cda_seker$choose_risk <- ifelse(j_cda_seker$kod_choice%in%c(120,121,130,131),1,0)
j_cda_seker$investment_fund_risk_19 <- ifelse(j_cda_seker$kod_choice%in%c(120,121,131,130) & j_cda_seker$feb_19==1,1,0)
j_cda_seker$choose_invest <- ifelse(j_cda_seker$kod_choice%in%c(120,121,130,131,110,111,140,141,210,211,220,230,231),1,0)
j_cda_seker$choose_invest_19 <- ifelse(j_cda_seker$choose_invest==1 & j_cda_seker$feb_19==1,1,0)
j_cda_seker$choose_bank <- ifelse(j_cda_seker$kod_choice%in%c(210,211,220,230,231),1,0)
j_cda_seker$choose_bank_19 <- ifelse(j_cda_seker$choose_bank==1 & j_cda_seker$feb_19==1,1,0)
j_feb_seker_digital <- j_cda_seker[j_cda_seker$k_digital==1,]
j_feb_seker_non_digital <- j_cda_seker[j_cda_seker$k_digital==0,]
j_feb_seker_feb19 <-j_cda_seker[j_cda_seker$feb_19==1,]
j_feb_seker_feb19_digital <-feb_seker_digital[feb_seker_digital$feb_19==1,]

####
cor(h_cda_seker$financial_knowledge,h_cda_seker$finance_lit_basic)
h_cda_seker$financial_knowledge1 <-ifelse(h_cda_seker$financial_knowledge%in%c(4,5),1,0)
table(h_cda_seker$financial_knowledge1)
table(h_cda_seker$finance_lit_basic)
cor(h_cda_seker$financial_knowledge,h_cda_seker$mother_academic)
table(h_cda_seker$financial_knowledge,h_cda_seker$finance_lit_basic)
h_cda_seker$financial_knowledge1 <-ifelse(h_cda_seker$financial_knowledge%in%c(0,1),1,0)
h_cda_seker$finance_lit_basic1 <-ifelse(h_cda_seker$finance_lit_basic%in%c(0,1),1,0)
h_cda_seker$financial_knowledge2 <-ifelse(h_cda_seker$financial_knowledge%in%c(4,5),1,0)
h_cda_seker$finance_lit_basic2 <-ifelse(h_cda_seker$finance_lit_basic%in%c(3),1,0)
table(h_cda_seker$financial_knowledge1,h_cda_seker$finance_lit_basic1)
h_cda_seker$ex_50_nis_19 <- ifelse(h_cda_seker$ex_50_nis==1 & h_cda_seker$feb_19==1,1,0)
h_cda_seker$k_digital <-ifelse(h_cda_seker$device%in%c("DevicePhone","DeviceComputer","DeviceComputer","DeviceTV","DeviceTablet"),1,0)
h_cda_seker$no_digital <-ifelse(h_cda_seker$device%in%c("DeviceUnknown"),1,NA)
h_cda_seker$no_digital <-ifelse(h_cda_seker$k_digital==1,0,h_cda_seker$no_digital)
h_cda_seker$feb19_nodigital <-ifelse(h_cda_seker$no_digital==1 & h_cda_seker$feb_19==1,1,0)
h_cda_seker$nodigital_50_19 <-ifelse(h_cda_seker$no_digital==1 & h_cda_seker$feb_19==1 & h_cda_seker$ex_50_nis==1,1,0)
h_cda_seker$phone <-ifelse(h_cda_seker$device=="DevicePhone",1,0)
h_cda_seker$computer <-ifelse(h_cda_seker$device=="DeviceComputer",1,0)
h_cda_seker$phone_19 <-ifelse(h_cda_seker$phone==1 & h_cda_seker$feb_19==1,1,0)
h_cda_seker$computer_19 <-ifelse(h_cda_seker$computer==1 & h_cda_seker$feb_19==1,1,0)
h_cda_seker$phone_19_50 <-ifelse(h_cda_seker$phone==1 & h_cda_seker$ex_50_nis_19==1,1,0)
h_cda_seker$phone_50 <-ifelse(h_cda_seker$phone==1 & h_cda_seker$ex_50_nis==1,1,0)
h_cda_seker$computer_19_50 <-ifelse(h_cda_seker$computer==1 & h_cda_seker$ex_50_nis_19==1,1,0)
h_cda_seker$choose_risk <- ifelse(h_cda_seker$kod_choice%in%c(120,121,130,131),1,0)
h_cda_seker$investment_fund_risk_19 <- ifelse(h_cda_seker$kod_choice%in%c(120,121,131,130) & h_cda_seker$feb_19==1,1,0)
h_cda_seker$choose_invest <- ifelse(h_cda_seker$kod_choice%in%c(120,121,130,131,110,111,140,141,210,211,220,230,231),1,0)
h_cda_seker$choose_invest_19 <- ifelse(h_cda_seker$choose_invest==1 & h_cda_seker$feb_19==1,1,0)
h_cda_seker$choose_bank <- ifelse(h_cda_seker$kod_choice%in%c(210,211,220,230,231),1,0)
h_cda_seker$choose_bank_19 <- ifelse(h_cda_seker$choose_bank==1 & h_cda_seker$feb_19==1,1,0)
h_feb_seker_digital <- h_cda_seker[h_cda_seker$k_digital==1,]
h_feb_seker_non_digital <- h_cda_seker[h_cda_seker$k_digital==0,]
h_feb_seker_feb19 <-h_cda_seker[h_cda_seker$feb_19==1,]
h_feb_seker_feb19_digital <-feb_seker_digital[feb_seker_digital$feb_19==1,]

###
cda_seker_new_before_patition$choose_clean <- ifelse(cda_seker_new_before_patition$kod_choice%in%c(310,410) & cda_seker_new_before_patition$choose_new==1,0,cda_seker_new_before_patition$choose_new)
cda_seker_new_before_patition$kosher <- ifelse(cda_seker_new_before_patition$kod_choice%in%c(140,141),1,0)
cda_seker_new_before_patition$choose_clean_19 <- ifelse(cda_seker_new_before_patition$kod_choice%in%c(310,410) & cda_seker_new_before_patition$feb_19==1,0,cda_seker_new_before_patition$feb_19)
cda_seker_new_before_patition$kosher_19 <- ifelse(cda_seker_new_before_patition$kod_choice%in%c(140,141)& cda_seker_new_before_patition$feb_19==1,1,0)
a_cda_seker$choose_clean <- ifelse(a_cda_seker$kod_choice%in%c(310,410) & a_cda_seker$choose_new==1,0,a_cda_seker$choose_new)
a_cda_seker$kosher <- ifelse(a_cda_seker$kod_choice%in%c(140,141),1,0)
a_cda_seker$choose_clean_19 <- ifelse(a_cda_seker$kod_choice%in%c(310,410) & a_cda_seker$feb_19==1,0,a_cda_seker$feb_19)
a_cda_seker$kosher_19 <- ifelse(a_cda_seker$kod_choice%in%c(140,141)& a_cda_seker$feb_19==1,1,0)
h_cda_seker$choose_clean <- ifelse(h_cda_seker$kod_choice%in%c(310,410) & h_cda_seker$choose_new==1,0,h_cda_seker$choose_new)
h_cda_seker$kosher <- ifelse(h_cda_seker$kod_choice%in%c(140,141),1,0)
h_cda_seker$choose_clean_19 <- ifelse(h_cda_seker$kod_choice%in%c(310,410) & h_cda_seker$feb_19==1,0,h_cda_seker$feb_19)
h_cda_seker$kosher_19 <- ifelse(h_cda_seker$kod_choice%in%c(140,141)& h_cda_seker$feb_19==1,1,0)
j_cda_seker$choose_clean <- ifelse(j_cda_seker$kod_choice%in%c(310,410) & j_cda_seker$choose_new==1,0,j_cda_seker$choose_new)
j_cda_seker$kosher <- ifelse(j_cda_seker$kod_choice%in%c(140,141),1,0)
j_cda_seker$choose_clean_19 <- ifelse(j_cda_seker$kod_choice%in%c(310,410) & j_cda_seker$feb_19==1,0,j_cda_seker$feb_19)
j_cda_seker$kosher_19 <- ifelse(j_cda_seker$kod_choice%in%c(140,141)& j_cda_seker$feb_19==1,1,0)
cda_seker_new_before_patition$trust_nii_high <- ifelse(cda_seker_new_before_patition$trust_nii%in%c(4,5),1,0)
cda_seker_new_before_patition$trust_high  <- ifelse(cda_seker_new_before_patition$trust_nii%in%c(4,5) |cda_seker_new_before_patition$trust_government%in%c(4,5) ,1,0)
cda_seker_new_before_patition$exist_high_10 <- ifelse(cda_seker_new_before_patition$program_will_exist_10y%in%c(4,5),1,0)
a_cda_seker$trust_nii_high <- ifelse(a_cda_seker$trust_nii%in%c(4,5),1,0)
a_cda_seker$trust_high  <- ifelse(a_cda_seker$trust_nii%in%c(4,5) |a_cda_seker$trust_government%in%c(4,5) ,1,0)
a_cda_seker$exist_high_10 <- ifelse(a_cda_seker$program_will_exist_10y%in%c(4,5),1,0)
h_cda_seker$trust_nii_high <- ifelse(h_cda_seker$trust_nii%in%c(4,5),1,0)
h_cda_seker$trust_high  <- ifelse(h_cda_seker$trust_nii%in%c(4,5) |h_cda_seker$trust_government%in%c(4,5) ,1,0)
h_cda_seker$exist_high_10 <- ifelse(h_cda_seker$program_will_exist_10y%in%c(4,5),1,0)
j_cda_seker$trust_nii_high <- ifelse(j_cda_seker$trust_nii%in%c(4,5),1,0)
j_cda_seker$trust_high  <- ifelse(j_cda_seker$trust_nii%in%c(4,5) |j_cda_seker$trust_government%in%c(4,5) ,1,0)
j_cda_seker$exist_high_10 <- ifelse(j_cda_seker$program_will_exist_10y%in%c(4,5),1,0)



##same as recode
summary(cda_seker_new_before_patition)
summary(a_cda_seker)
summary(h_cda_seker)
summary(j_cda_seker)


##############Moving Savings - 
##For robustness checks change variables stay the same
prob_reg1=lm(decrease_child_private_saving~ choose_new+cover_high+financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+income+
               mother_academic+father_academic+parents_hardi+parents_arab+parents_married+gill_child,
             
             data= cda_seker_new_before_patition)

summary(prob_reg1)

prob_reg2=lm(decrease_child_private_saving~ choose_new+cover_high+financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+income+
               mother_academic+father_academic+parents_hardi+parents_arab+parents_married+gill_child,
             data= a_cda_seker)

summary(prob_reg2)


prob_reg3=lm(decrease_child_private_saving~ choose_new+cover_high+financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             data= h_cda_seker)

summary(prob_reg3)

prob_reg4=lm(decrease_child_private_saving~choose_new+cover_high+ financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             data= j_cda_seker)

summary(prob_reg4)


stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Explaining decrease choose",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb","sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS","Received SMS*Arab","Received SMS*Ultra-Orthodox", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="explaining_decrease choose.htm", 
          no.space=TRUE, digits=2 )

prob_reg1=lm(decrease_child_private_saving~ choose_clean+cover_high+financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+income+
               mother_academic+father_academic+parents_hardi+parents_arab+parents_married+gill_child,
             
             data= cda_seker_new_before_patition)

summary(prob_reg1)

prob_reg2=lm(decrease_child_private_saving~ choose_clean+cover_high+financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             data= a_cda_seker)

summary(prob_reg2)


prob_reg3=lm(decrease_child_private_saving~ choose_clean+cover_high+financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             data= h_cda_seker)

summary(prob_reg3)

prob_reg4=lm(decrease_child_private_saving~choose_clean+cover_high+ financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             data= j_cda_seker)

summary(prob_reg4)


stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Explaining decrease choose",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb","sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS","Received SMS*Arab","Received SMS*Ultra-Orthodox", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="explaining_decreasechoose_clean.htm", 
          no.space=TRUE, digits=2 )



prob_reg1=lm(decrease_child_private_saving~ ex_50_nis+cover_high+financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+income+
               mother_academic+father_academic+parents_hardi+parents_arab+parents_married+gill_child,
             
             data= cda_seker_new_before_patition)

summary(prob_reg1)

prob_reg2=lm(decrease_child_private_saving~ ex_50_nis+cover_high+financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             data= a_cda_seker)

summary(prob_reg2)


prob_reg3=lm(decrease_child_private_saving~ ex_50_nis+cover_high+financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             data= h_cda_seker)

summary(prob_reg3)

prob_reg4=lm(decrease_child_private_saving~ex_50_nis+cover_high+ financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+income+
               mother_academic+father_academic+parents_married+gill_child,
             data= j_cda_seker)

summary(prob_reg4)


stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Explaining decrease 50",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb","sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS","Received SMS*Arab","Received SMS*Ultra-Orthodox", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="explaining_decrease_50.htm", 
          no.space=TRUE, digits=2 )




prob_reg1=lm(decrease_child_private_saving~ ex_50_nis,
             data= cda_seker_new_before_patition)

summary(prob_reg1)

prob_reg2=lm(decrease_child_private_saving~ ex_50_nis,             data= a_cda_seker)

summary(prob_reg2)


prob_reg3=lm(decrease_child_private_saving~  ex_50_nis,              data= h_cda_seker)

summary(prob_reg3)

prob_reg4=lm(decrease_child_private_saving~ex_50_nis,
             data= j_cda_seker)

summary(prob_reg4)


stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Explaining decrease 50",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb","sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS","Received SMS*Arab","Received SMS*Ultra-Orthodox", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="explaining_decrease_50_short.htm", 
          no.space=TRUE, digits=2 )



prob_reg1=lm(decrease_child_private_saving~ choose_clean,
             data= cda_seker_new_before_patition)

summary(prob_reg1)

prob_reg2=lm(decrease_child_private_saving~ choose_clean,             data= a_cda_seker)

summary(prob_reg2)


prob_reg3=lm(decrease_child_private_saving~  choose_clean,              data= h_cda_seker)

summary(prob_reg3)

prob_reg4=lm(decrease_child_private_saving~choose_clean,
             data= j_cda_seker)

summary(prob_reg4)


stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Explaining decrease clean",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb","sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS","Received SMS*Arab","Received SMS*Ultra-Orthodox", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="explaining_decrease_clean_short.htm", 
          no.space=TRUE, digits=2 )




prob_reg1=lm(decrease_child_private_saving~ choose_new,
             data= cda_seker_new_before_patition)

summary(prob_reg1)

prob_reg2=lm(decrease_child_private_saving~ choose_new,             data= a_cda_seker)

summary(prob_reg2)


prob_reg3=lm(decrease_child_private_saving~  choose_new,              data= h_cda_seker)

summary(prob_reg3)

prob_reg4=lm(decrease_child_private_saving~choose_new,
             data= j_cda_seker)

summary(prob_reg4)


stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Explaining decrease choose",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb","sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS","Received SMS*Arab","Received SMS*Ultra-Orthodox", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="explaining_decrease_chooseshort.htm", 
          no.space=TRUE, digits=2 )

#########################
prob_reg1=lm(decrease_child_private_saving~ sms_feb*choose_new,
             data= cda_seker_new_before_patition)

summary(prob_reg1)

prob_reg2=lm(decrease_child_private_saving~ sms_feb*choose_new,             data= a_cda_seker)

summary(prob_reg2)


prob_reg3=lm(decrease_child_private_saving~  sms_feb*choose_new,              data= h_cda_seker)

summary(prob_reg3)

prob_reg4=lm(decrease_child_private_saving~sms_feb*choose_new,
             data= j_cda_seker)

summary(prob_reg4)


stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Explaining decrease SMS",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb","sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS","Received SMS*Arab","Received SMS*Ultra-Orthodox", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="explaining_decrease_SMSchooseshort.htm", 
          no.space=TRUE, digits=2 )

#####################

#########################
prob_reg1=lm(decrease_child_private_saving~ sms_feb*choose_clean,
             data= cda_seker_new_before_patition)

summary(prob_reg1)

prob_reg2=lm(decrease_child_private_saving~ sms_feb*choose_clean,             data= a_cda_seker)

summary(prob_reg2)


prob_reg3=lm(decrease_child_private_saving~  sms_feb*choose_clean,              data= h_cda_seker)

summary(prob_reg3)

prob_reg4=lm(decrease_child_private_saving~sms_feb*choose_clean,
             data= j_cda_seker)

summary(prob_reg4)


stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Explaining decrease SMSc clean",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb","sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS","Received SMS*Arab","Received SMS*Ultra-Orthodox", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="explaining_decrease_SMSchooscleaneshort.htm", 
          no.space=TRUE, digits=2 )
###############
prob_reg1=lm(decrease_child_private_saving~ sms_feb*ex_50_nis,
             data= cda_seker_new_before_patition)

summary(prob_reg1)

prob_reg2=lm(decrease_child_private_saving~ sms_feb*ex_50_nis,             data= a_cda_seker)

summary(prob_reg2)


prob_reg3=lm(decrease_child_private_saving~  sms_feb*ex_50_nis,              data= h_cda_seker)

summary(prob_reg3)

prob_reg4=lm(decrease_child_private_saving~sms_feb*ex_50_nis,
             data= j_cda_seker)

summary(prob_reg4)


stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Explaining decrease SMSc 50",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb","sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS","Received SMS*Arab","Received SMS*Ultra-Orthodox", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="explaining_decrease_SMS50.htm", 
          no.space=TRUE, digits=2 )


#########################
prob_reg1=lm(decrease_child_private_saving~ sms_feb*choose_new*parents_arab,
             data= cda_seker_new_before_patition)

summary(prob_reg1)

prob_reg2=lm(decrease_child_private_saving~ sms_feb*choose_new,             data= a_cda_seker)

summary(prob_reg2)


prob_reg3=lm(decrease_child_private_saving~  sms_feb*choose_new,              data= h_cda_seker)

summary(prob_reg3)

prob_reg4=lm(decrease_child_private_saving~sms_feb*choose_new,
             data= j_cda_seker)

summary(prob_reg4)


stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Explaining decrease SMS",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb","sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS","Received SMS*Arab","Received SMS*Ultra-Orthodox", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="explaining_dec_arab_SMSchooseshor.htm", 
          no.space=TRUE, digits=2 )



############################


prob_reg1=lm(finance_lit_basic1~ parents_arab+parents_hardi,
             
             data= cda_seker_new_before_patition)

summary(prob_reg1)

prob_reg2=lm(financial_knowledge1~ parents_arab+parents_hardi,
             
             data= cda_seker_new_before_patition)

summary(prob_reg2)


prob_reg3=lm(trust_gov_high~parents_arab+parents_hardi,
             
             data= cda_seker_new_before_patition)
summary(prob_reg3)

prob_reg4=lm(cover_high~ parents_arab+parents_hardi,
             
             data= cda_seker_new_before_patition)

summary(prob_reg4)


stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Explaining attributes",align=TRUE,
          type="html",dep.var.labels=c("Objective Lit","Subjective Lit","Trust","Cover"),
          # order=c("sms_feb","sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS","Received SMS*Arab","Received SMS*Ultra-Orthodox", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="explaining withminorities.htm", 
          no.space=TRUE, digits=2 )

################################Robustness more savings or the same instead of decrease

prob_reg1=lm(same_more_savings~ feb_19+cover_high+financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+mother_wage+father_wage+
               mother_academic+father_academic+parents_hardi+parents_arab+parents_married+gill_child,
             
             data= cda_seker_new_before_patition)

summary(prob_reg1)

prob_reg2=lm(same_more_savings~ feb_19+cover_high+financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+mother_wage+father_wage+
               mother_academic+father_academic+parents_married+gill_child,
             data= a_cda_seker)

summary(prob_reg2)


prob_reg3=lm(same_more_savings~ feb_19+cover_high+financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+mother_wage+father_wage+
               mother_academic+father_academic+parents_married+gill_child,
             data= h_cda_seker)

summary(prob_reg3)

prob_reg4=lm(same_more_savings~feb_19+cover_high+ financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+mother_wage+father_wage+
               mother_academic+father_academic+parents_married+gill_child,
             data= j_cda_seker)

summary(prob_reg4)


stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Explaining increase choose",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb","sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS","Received SMS*Arab","Received SMS*Ultra-Orthodox", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="19_beforeprat_explaining_increase_same_choose.htm", 
          no.space=TRUE, digits=2 )

prob_reg1=lm(same_more_savings~ feb_19+cover_high+financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+mother_wage+father_wage+
               mother_academic+father_academic+parents_hardi+parents_arab+parents_married+gill_child,
             
             data= cda_seker_new_before_patition)

summary(prob_reg1)

prob_reg2=lm(same_more_savings~ feb_19+cover_high+financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+mother_wage+father_wage+
               mother_academic+father_academic+parents_married+gill_child,
             data= a_cda_seker)

summary(prob_reg2)


prob_reg3=lm(same_more_savings~ feb_19+cover_high+financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+mother_wage+father_wage+
               mother_academic+father_academic+parents_married+gill_child,
             data= h_cda_seker)

summary(prob_reg3)

prob_reg4=lm(same_more_savings~feb_19+cover_high+ financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+mother_wage+father_wage+
               mother_academic+father_academic+parents_married+gill_child,
             data= j_cda_seker)

summary(prob_reg4)


stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Explaining increase choose",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb","sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS","Received SMS*Arab","Received SMS*Ultra-Orthodox", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="19_beforeprat_explaining_increase_same_clean.htm", 
          no.space=TRUE, digits=2 )



prob_reg1=lm(same_more_savings~ ex_50_nis_19+cover_high+financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+mother_wage+father_wage+
               mother_academic+father_academic+parents_hardi+parents_arab+parents_married+gill_child,
             
             data= cda_seker_new_before_patition)

summary(prob_reg1)

prob_reg2=lm(same_more_savings~ ex_50_nis_19+cover_high+financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+mother_wage+father_wage+
               mother_academic+father_academic+parents_married+gill_child,
             data= a_cda_seker)

summary(prob_reg2)


prob_reg3=lm(same_more_savings~ ex_50_nis_19+cover_high+financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+mother_wage+father_wage+
               mother_academic+father_academic+parents_married+gill_child,
             data= h_cda_seker)

summary(prob_reg3)

prob_reg4=lm(same_more_savings~ex_50_nis_19+cover_high+ financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+mother_wage+father_wage+
               mother_academic+father_academic+parents_married+gill_child,
             data= j_cda_seker)

summary(prob_reg4)


stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Explaining increase 50",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb","sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS","Received SMS*Arab","Received SMS*Ultra-Orthodox", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="19_beforeprat_explaining_increase_50.htm", 
          no.space=TRUE, digits=2 )
################
##############
##########

prob_reg1=lm(same_more_savings~ investment_fund_risk_19+cover_high+financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+mother_wage+father_wage+
               mother_academic+father_academic+parents_hardi+parents_arab+parents_married+gill_child,
             
             data= cda_seker_new_before_patition)

summary(prob_reg1)

prob_reg2=lm(same_more_savings~ investment_fund_risk_19+cover_high+financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+mother_wage+father_wage+
               mother_academic+father_academic+parents_married+gill_child,
             data= a_cda_seker)

summary(prob_reg2)


prob_reg3=lm(same_more_savings~ investment_fund_risk_19+cover_high+financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+mother_wage+father_wage+
               mother_academic+father_academic+parents_married+gill_child,
             data= h_cda_seker)

summary(prob_reg3)

prob_reg4=lm(same_more_savings~investment_fund_risk_19+cover_high+ financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+mother_wage+father_wage+
               mother_academic+father_academic+parents_married+gill_child,
             data= j_cda_seker)

summary(prob_reg4)


stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Explaining increase risk",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb","sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS","Received SMS*Arab","Received SMS*Ultra-Orthodox", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="19_beforeprat_explaining_increase_risk.htm", 
          no.space=TRUE, digits=2 )



###################


prob_reg1=lm(decrease_child_private_saving~ ex_50_nis_19,
             data= cda_seker_new_before_patition)

summary(prob_reg1)

prob_reg2=lm(decrease_child_private_saving~ ex_50_nis_19,             data= a_cda_seker)

summary(prob_reg2)


prob_reg3=lm(decrease_child_private_saving~  ex_50_nis_19,              data= h_cda_seker)

summary(prob_reg3)

prob_reg4=lm(decrease_child_private_saving~ex_50_nis_19,
             data= j_cda_seker)

summary(prob_reg4)


stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Explaining decrease 50",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb","sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS","Received SMS*Arab","Received SMS*Ultra-Orthodox", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="19_beforeprat_explaining_decrease_50_short.htm", 
          no.space=TRUE, digits=2 )



prob_reg1=lm(decrease_child_private_saving~ choose_clean_19,
             data= cda_seker_new_before_patition)

summary(prob_reg1)

prob_reg2=lm(decrease_child_private_saving~ choose_clean_19,             data= a_cda_seker)

summary(prob_reg2)


prob_reg3=lm(decrease_child_private_saving~  choose_clean_19,              data= h_cda_seker)

summary(prob_reg3)

prob_reg4=lm(decrease_child_private_saving~choose_clean_19,
             data= j_cda_seker)

summary(prob_reg4)


stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Explaining decrease clean",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb","sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS","Received SMS*Arab","Received SMS*Ultra-Orthodox", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="19_beforeprat_explaining_decrease_clean_short.htm", 
          no.space=TRUE, digits=2 )




prob_reg1=lm(decrease_child_private_saving~ feb_19,
             data= cda_seker_new_before_patition)

summary(prob_reg1)

prob_reg2=lm(decrease_child_private_saving~ feb_19,             data= a_cda_seker)

summary(prob_reg2)


prob_reg3=lm(decrease_child_private_saving~  feb_19,              data= h_cda_seker)

summary(prob_reg3)

prob_reg4=lm(decrease_child_private_saving~feb_19,
             data= j_cda_seker)

summary(prob_reg4)


stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Explaining decrease choose",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb","sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS","Received SMS*Arab","Received SMS*Ultra-Orthodox", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="19_beforeprat_explaining_decrease_chooseshort.htm", 
          no.space=TRUE, digits=2 )

################
###########


prob_reg1=lm(same_more_savings~ ex_50_nis_19,
             data= cda_seker_new_before_patition)

summary(prob_reg1)

prob_reg2=lm(same_more_savings~ ex_50_nis_19,             data= a_cda_seker)

summary(prob_reg2)


prob_reg3=lm(same_more_savings~  ex_50_nis_19,              data= h_cda_seker)

summary(prob_reg3)

prob_reg4=lm(same_more_savings~ex_50_nis_19,
             data= j_cda_seker)

summary(prob_reg4)


stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Explaining increase 50",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb","sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS","Received SMS*Arab","Received SMS*Ultra-Orthodox", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="19_beforeprat_explaining_increase_50_short.htm", 
          no.space=TRUE, digits=2 )



prob_reg1=lm(same_more_savings~ choose_clean_19,
             data= cda_seker_new_before_patition)

summary(prob_reg1)

prob_reg2=lm(same_more_savings~ choose_clean_19,             data= a_cda_seker)

summary(prob_reg2)


prob_reg3=lm(same_more_savings~  choose_clean_19,              data= h_cda_seker)

summary(prob_reg3)

prob_reg4=lm(same_more_savings~choose_clean_19,
             data= j_cda_seker)

summary(prob_reg4)


stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Explaining increase clean",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb","sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS","Received SMS*Arab","Received SMS*Ultra-Orthodox", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="19_beforeprat_explaining_increase_clean_short.htm", 
          no.space=TRUE, digits=2 )




prob_reg1=lm(same_more_savings~ feb_19,
             data= cda_seker_new_before_patition)

summary(prob_reg1)

prob_reg2=lm(same_more_savings~ feb_19,             data= a_cda_seker)

summary(prob_reg2)


prob_reg3=lm(same_more_savings~  feb_19,              data= h_cda_seker)

summary(prob_reg3)

prob_reg4=lm(same_more_savings~feb_19,
             data= j_cda_seker)

summary(prob_reg4)


stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Explaining increase choose",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb","sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS","Received SMS*Arab","Received SMS*Ultra-Orthodox", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="19_beforeprat_explaining_increase_chooseshort.htm", 
          no.space=TRUE, digits=2 )


################################

prob_reg1=lm(same_more_savings~ choose_new+cover_high+financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+mother_wage+father_wage+
               mother_academic+father_academic+parents_hardi+parents_arab+parents_married+gill_child,
             
             data= cda_seker_new_before_patition)

summary(prob_reg1)

prob_reg2=lm(same_more_savings~ choose_new+cover_high+financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+mother_wage+father_wage+
               mother_academic+father_academic+parents_married+gill_child,
             data= a_cda_seker)

summary(prob_reg2)


prob_reg3=lm(same_more_savings~ choose_new+cover_high+financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+mother_wage+father_wage+
               mother_academic+father_academic+parents_married+gill_child,
             data= h_cda_seker)

summary(prob_reg3)

prob_reg4=lm(same_more_savings~choose_new+cover_high+ financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+mother_wage+father_wage+
               mother_academic+father_academic+parents_married+gill_child,
             data= j_cda_seker)

summary(prob_reg4)


stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Explaining increase choose",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb","sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS","Received SMS*Arab","Received SMS*Ultra-Orthodox", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="beforeprat_explaining_increase_same_choose.htm", 
          no.space=TRUE, digits=2 )

prob_reg1=lm(same_more_savings~ choose_clean+cover_high+financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+mother_wage+father_wage+
               mother_academic+father_academic+parents_hardi+parents_arab+parents_married+gill_child,
             
             data= cda_seker_new_before_patition)

summary(prob_reg1)

prob_reg2=lm(same_more_savings~ choose_clean+cover_high+financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+mother_wage+father_wage+
               mother_academic+father_academic+parents_married+gill_child,
             data= a_cda_seker)

summary(prob_reg2)


prob_reg3=lm(same_more_savings~ choose_clean+cover_high+financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+mother_wage+father_wage+
               mother_academic+father_academic+parents_married+gill_child,
             data= h_cda_seker)

summary(prob_reg3)

prob_reg4=lm(same_more_savings~choose_clean+cover_high+ financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+mother_wage+father_wage+
               mother_academic+father_academic+parents_married+gill_child,
             data= j_cda_seker)

summary(prob_reg4)


stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Explaining increase choose",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb","sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS","Received SMS*Arab","Received SMS*Ultra-Orthodox", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="beforeprat_explaining_increase_same_clean.htm", 
          no.space=TRUE, digits=2 )



prob_reg1=lm(same_more_savings~ ex_50_nis+cover_high+financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+mother_wage+father_wage+
               mother_academic+father_academic+parents_hardi+parents_arab+parents_married+gill_child,
             
             data= cda_seker_new_before_patition)

summary(prob_reg1)

prob_reg2=lm(same_more_savings~ ex_50_nis+cover_high+financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+mother_wage+father_wage+
               mother_academic+father_academic+parents_married+gill_child,
             data= a_cda_seker)

summary(prob_reg2)


prob_reg3=lm(same_more_savings~ ex_50_nis+cover_high+financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+mother_wage+father_wage+
               mother_academic+father_academic+parents_married+gill_child,
             data= h_cda_seker)

summary(prob_reg3)

prob_reg4=lm(same_more_savings~ex_50_nis+cover_high+ financial_knowledge1+finance_lit_basic1+trust_gov_high+
               parents_num_children+mother_wage+father_wage+
               mother_academic+father_academic+parents_married+gill_child,
             data= j_cda_seker)

summary(prob_reg4)


stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Explaining increase 50",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb","sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS","Received SMS*Arab","Received SMS*Ultra-Orthodox", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="beforeprat_explaining_increase_50.htm", 
          no.space=TRUE, digits=2 )
################
