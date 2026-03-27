##regression RnR
rm(list=ls())
gc()

##new after RnR nearest neibour
pkgFilenames <- read.csv("pkgFilenames.csv", stringsAsFactors = FALSE)[,1]
install.packages(pkgFilenames, repos = NULL, type = "win.binary")
##pak_r
##packare2install


setwd("w:/incoming/maya_hisachon_chiild")

#library(vars)
library(stargazer)
library(lubridate)
library(ggplot2)
library(reshape2)
library(dplyr)
library(scales)
#library(forecast)
library(pastecs)
library(Hmisc)
#library(aod)
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


load("feb_nearest_main_t_data")
load("a_total_match_feb_main_data")
load("h_total_match_feb_main_data")
load("j_total_match_feb_main_data")

rm(list=ls())
gc()


##for robustness specifications uncomment
# load("feb_nearest_noreplace_t_data")
# load("a_total_match_feb_noreplace_data")
# load("h_total_match_feb_noreplace_data")
# load("j_total_match_feb_noreplace_data")
# feb_nearest_main_t_data <- feb_nearest_noreplace_t_data
# a_total_match_feb_main_data <- a_total_match_feb_noreplace_data
# h_total_match_feb_main_data <- h_total_match_feb_noreplace_data
# j_total_match_feb_main_data <- j_total_match_feb_noreplace_data

# # 
# load("feb_nearest_caliper_t_data")
# load("a_total_match_feb_caliper_data")
# load("h_total_match_feb_caliper_data")
# load("j_total_match_feb_caliper_data")
# feb_nearest_main_t_data <- feb_nearest_caliper_t_data
# a_total_match_feb_main_data <- a_total_match_feb_caliper_data
# h_total_match_feb_main_data <- h_total_match_feb_caliper_data
# j_total_match_feb_main_data <- j_total_match_feb_caliper_data

# 
# load("feb_nearest_ratio_t_data")
# load("a_total_match_feb_ratio_data")
# load("h_total_match_feb_ratio_data")
# load("j_total_match_feb_ratio_data")
# feb_nearest_main_t_data <- feb_nearest_ratio_t_data
# a_total_match_feb_main_data <- a_total_match_feb_ratio_data
# h_total_match_feb_main_data <- h_total_match_feb_ratio_data
# j_total_match_feb_main_data <- j_total_match_feb_ratio_data

# 
# load("feb_nearest_lamas_t_data")
# load("a_total_match_feb_lamas_data")
# load("h_total_match_feb_lamas_data")
# load("j_total_match_feb_lamas_data")
# feb_nearest_main_t_data <- feb_nearest_lamas_t_data
# a_total_match_feb_main_data <- a_total_match_feb_lamas_data
# h_total_match_feb_main_data <- h_total_match_feb_lamas_data
# j_total_match_feb_main_data <- j_total_match_feb_lamas_data


feb_nearest_main_t_data$choose_clean <- feb_nearest_main_t_data$choose_new
feb_nearest_main_t_data$choose_clean <- ifelse(feb_nearest_main_t_data$kod_choice%in%c(310,410),0,feb_nearest_main_t_data$choose_clean)
feb_nearest_main_t_data$kosher <- ifelse(feb_nearest_main_t_data$kod_choice%in%c(140,141),1,0)
feb_nearest_main_t_data$choose_clean_19 <- ifelse(feb_nearest_main_t_data$kod_choice%in%c(310,410) & feb_nearest_main_t_data$feb_19==1,0,feb_nearest_main_t_data$choose_clean)
feb_nearest_main_t_data$kosher_19 <- ifelse(feb_nearest_main_t_data$kod_choice%in%c(140,141)& feb_nearest_main_t_data$feb_19==1,1,0)

a_total_match_feb_main_data$choose_clean <- a_total_match_feb_main_data$choose_new
a_total_match_feb_main_data$choose_clean <- ifelse(a_total_match_feb_main_data$kod_choice%in%c(310,410),0,a_total_match_feb_main_data$choose_clean)
a_total_match_feb_main_data$kosher <- ifelse(a_total_match_feb_main_data$kod_choice%in%c(140,141),1,0)
a_total_match_feb_main_data$choose_clean_19 <- ifelse(a_total_match_feb_main_data$kod_choice%in%c(310,410) & a_total_match_feb_main_data$feb_19==1,0,a_total_match_feb_main_data$choose_clean)
a_total_match_feb_main_data$kosher_19 <- ifelse(a_total_match_feb_main_data$kod_choice%in%c(140,141)& a_total_match_feb_main_data$feb_19==1,1,0)


h_total_match_feb_main_data$choose_clean <- h_total_match_feb_main_data$choose_new
h_total_match_feb_main_data$choose_clean <- ifelse(h_total_match_feb_main_data$kod_choice%in%c(310,410),0,h_total_match_feb_main_data$choose_clean)
h_total_match_feb_main_data$kosher <- ifelse(h_total_match_feb_main_data$kod_choice%in%c(140,141),1,0)
h_total_match_feb_main_data$choose_clean_19 <- ifelse(h_total_match_feb_main_data$kod_choice%in%c(310,410) & h_total_match_feb_main_data$feb_19==1,0,h_total_match_feb_main_data$choose_clean)
h_total_match_feb_main_data$kosher_19 <- ifelse(h_total_match_feb_main_data$kod_choice%in%c(140,141)& h_total_match_feb_main_data$feb_19==1,1,0)


j_total_match_feb_main_data$choose_clean <- j_total_match_feb_main_data$choose_new
j_total_match_feb_main_data$choose_clean <- ifelse(j_total_match_feb_main_data$kod_choice%in%c(310,410),0,j_total_match_feb_main_data$choose_clean)
j_total_match_feb_main_data$kosher <- ifelse(j_total_match_feb_main_data$kod_choice%in%c(140,141),1,0)
j_total_match_feb_main_data$choose_clean_19 <- ifelse(j_total_match_feb_main_data$kod_choice%in%c(310,410) & j_total_match_feb_main_data$feb_19==1,0,j_total_match_feb_main_data$choose_clean)
j_total_match_feb_main_data$kosher_19 <- ifelse(j_total_match_feb_main_data$kod_choice%in%c(140,141)& j_total_match_feb_main_data$feb_19==1,1,0)


feb_nearest_main_t_data$income= 1000*(feb_nearest_main_t_data$father_wage+feb_nearest_main_t_data$mother_wage)

feb_nearest_main_t_data$ex_50_nis_19 <- ifelse(feb_nearest_main_t_data$ex_50_nis==1 & feb_nearest_main_t_data$feb_19==1,1,0)

feb_nearest_main_t_data$k_digital <-ifelse(feb_nearest_main_t_data$device%in%c("DevicePhone","DeviceComputer","DeviceComputer","DeviceTV","DeviceTablet"),1,0)
feb_nearest_main_t_data$no_digital <-ifelse(feb_nearest_main_t_data$device%in%c("DeviceUnknown"),1,NA)
feb_nearest_main_t_data$no_digital <-ifelse(feb_nearest_main_t_data$k_digital==1,0,feb_nearest_main_t_data$no_digital)
feb_nearest_main_t_data$feb19_nodigital <-ifelse(feb_nearest_main_t_data$no_digital==1 & feb_nearest_main_t_data$feb_19==1,1,0)
feb_nearest_main_t_data$nodigital_50_19 <-ifelse(feb_nearest_main_t_data$no_digital==1 & feb_nearest_main_t_data$feb_19==1 & feb_nearest_main_t_data$ex_50_nis==1,1,0)
feb_nearest_main_t_data$phone <-ifelse(feb_nearest_main_t_data$device=="DevicePhone",1,0)
feb_nearest_main_t_data$phone_19 <-ifelse(feb_nearest_main_t_data$phone==1 & feb_nearest_main_t_data$feb_19==1,1,0)

feb_nearest_main_t_data$no_phone_19 <-ifelse(feb_nearest_main_t_data$phone==0 & feb_nearest_main_t_data$feb_19==1,1,0)
feb_nearest_main_t_data$no_phone_choose <-ifelse(feb_nearest_main_t_data$no_phone==1 & feb_nearest_main_t_data$choose_new==1,1,0)
feb_nearest_main_t_data$computer <-ifelse(feb_nearest_main_t_data$device=="DeviceComputer",1,0)
feb_nearest_main_t_data$computer_19 <-ifelse(feb_nearest_main_t_data$computer==1 & feb_nearest_main_t_data$feb_19==1,1,0)
feb_nearest_main_t_data$phone_19_50 <-ifelse(feb_nearest_main_t_data$phone==1 & feb_nearest_main_t_data$ex_50_nis_19==1,1,0)
feb_nearest_main_t_data$computer_19_50 <-ifelse(feb_nearest_main_t_data$computer==1 & feb_nearest_main_t_data$ex_50_nis_19==1,1,0)
feb_nearest_main_t_data$choose_risk <- ifelse(feb_nearest_main_t_data$kod_choice%in%c(120,121,130,131),1,0)
feb_nearest_main_t_data$investment_fund_risk_19 <- ifelse(feb_nearest_main_t_data$kod_choice%in%c(120,121,131,130) & feb_nearest_main_t_data$feb_19==1,1,0)
feb_nearest_main_t_data$choose_invest <- ifelse(feb_nearest_main_t_data$kod_choice%in%c(120,121,130,131,110,111,140,141,210,211,220,230,231),1,0)
feb_nearest_main_t_data$choose_invest_19 <- ifelse(feb_nearest_main_t_data$choose_invest==1 & feb_nearest_main_t_data$feb_19==1,1,0)
feb_nearest_main_t_data$choose_bank <- ifelse(feb_nearest_main_t_data$kod_choice%in%c(210,211,220,230,231),1,0)
feb_nearest_main_t_data$choose_bank_19 <- ifelse(feb_nearest_main_t_data$choose_bank==1 & feb_nearest_main_t_data$feb_19==1,1,0)


feb_only_sms_nearest_digital <- feb_nearest_main_t_data[feb_nearest_main_t_data$k_digital==1,]
feb_only_sms_nearest_non_digital <- feb_nearest_main_t_data[feb_nearest_main_t_data$k_digital==0,]

feb_only_sms_nearest_feb19 <-feb_nearest_main_t_data[feb_nearest_main_t_data$feb_19==1,]
feb_only_sms_nearest_feb19_digital <-feb_only_sms_nearest_digital[feb_only_sms_nearest_digital$feb_19==1,]

#########################
j_total_match_feb_main_data$income= 1000*(j_total_match_feb_main_data$father_wage+j_total_match_feb_main_data$mother_wage)
j_total_match_feb_main_data$choose_risk <- ifelse(j_total_match_feb_main_data$kod_choice%in%c(120,121,130,131),1,0)
j_total_match_feb_main_data$investment_fund_risk_19 <- ifelse(j_total_match_feb_main_data$kod_choice%in%c(120,121,131,130) & j_total_match_feb_main_data$feb_19==1,1,0)
j_total_match_feb_main_data$choose_invest <- ifelse(j_total_match_feb_main_data$kod_choice%in%c(120,121,130,131,110,111,140,141,210,211,220,230,231),1,0)
j_total_match_feb_main_data$choose_invest_19 <- ifelse(j_total_match_feb_main_data$choose_invest==1 & j_total_match_feb_main_data$feb_19==1,1,0)
j_total_match_feb_main_data$choose_bank <- ifelse(j_total_match_feb_main_data$kod_choice%in%c(210,211,220,230,231),1,0)
j_total_match_feb_main_data$choose_bank_19 <- ifelse(j_total_match_feb_main_data$choose_bank==1 & j_total_match_feb_main_data$feb_19==1,1,0)


j_total_match_feb_main_data$ex_50_nis_19 <- ifelse( j_total_match_feb_main_data$ex_50_nis==1 &  j_total_match_feb_main_data$feb_19==1,1,0)
j_total_match_feb_main_data$k_digital <-ifelse( j_total_match_feb_main_data$device%in%c("DevicePhone","DeviceComputer","DeviceComputer","DeviceTV","DeviceTablet"),1,0)
j_total_match_feb_main_data$no_digital <-ifelse( j_total_match_feb_main_data$device%in%c("DeviceUnknown"),1,NA)
j_total_match_feb_main_data$no_digital <-ifelse( j_total_match_feb_main_data$k_digital==1,0, j_total_match_feb_main_data$no_digital)
j_total_match_feb_main_data$k_digit <-ifelse( j_total_match_feb_main_data$device%in%c("DevicePhone","DeviceComputer","DeviceComputer","DeviceTV","DeviceTablet")&  j_total_match_feb_main_data$choose_20_clean_10==1,1,0)
j_total_match_feb_main_data$feb19_nodigital <-ifelse( j_total_match_feb_main_data$no_digital==1 &  j_total_match_feb_main_data$feb_19==1,1,0)
j_total_match_feb_main_data$nodigital_50_19 <-ifelse( j_total_match_feb_main_data$no_digital==1 &  j_total_match_feb_main_data$feb_19==1 &  j_total_match_feb_main_data$ex_50_nis==1,1,0)
j_total_match_feb_main_data$phone <-ifelse( j_total_match_feb_main_data$device=="DevicePhone",1,0)
j_total_match_feb_main_data$phone_19 <-ifelse( j_total_match_feb_main_data$phone==1 &  j_total_match_feb_main_data$feb_19==1,1,0)
j_total_match_feb_main_data$no_phone <-ifelse( j_total_match_feb_main_data$device=="DevicePhone",0,1)
j_total_match_feb_main_data$no_phone_19 <-ifelse( j_total_match_feb_main_data$no_phone==1 &  j_total_match_feb_main_data$feb_19==1,1,0)
j_total_match_feb_main_data$computer <-ifelse( j_total_match_feb_main_data$device=="DeviceComputer",1,0)
j_total_match_feb_main_data$computer_19 <-ifelse( j_total_match_feb_main_data$computer==1 &  j_total_match_feb_main_data$feb_19==1,1,0)
j_total_match_feb_main_data$phone_19_50 <-ifelse( j_total_match_feb_main_data$phone==1 &  j_total_match_feb_main_data$ex_50_nis_19==1,1,0)
j_total_match_feb_main_data$phone_50 <-ifelse( j_total_match_feb_main_data$phone==1 &  j_total_match_feb_main_data$ex_50_nis==1,1,0)
j_total_match_feb_main_data$computer_19_50 <-ifelse( j_total_match_feb_main_data$computer==1 &  j_total_match_feb_main_data$ex_50_nis_19==1,1,0)

j_feb_only_sms_nearest_digital <-  j_total_match_feb_main_data[ j_total_match_feb_main_data$k_digital==1,]
j_feb_only_sms_nearest_non_digital <-  j_total_match_feb_main_data[ j_total_match_feb_main_data$k_digital==0,]

j_feb_only_sms_nearest_feb19 <- j_total_match_feb_main_data[ j_total_match_feb_main_data$feb_19==1,]
j_feb_only_sms_nearest_feb19_digital <-j_feb_only_sms_nearest_digital[j_feb_only_sms_nearest_digital$feb_19==1,]

######################
a_total_match_feb_main_data$income= 1000*(a_total_match_feb_main_data$father_wage+a_total_match_feb_main_data$mother_wage)


a_total_match_feb_main_data$choose_risk <- ifelse(a_total_match_feb_main_data$kod_choice%in%c(120,121,130,131),1,0)
a_total_match_feb_main_data$investment_fund_risk_19 <- ifelse(a_total_match_feb_main_data$kod_choice%in%c(120,121,131,130) & a_total_match_feb_main_data$feb_19==1,1,0)

a_total_match_feb_main_data$choose_invest <- ifelse(a_total_match_feb_main_data$kod_choice%in%c(120,121,130,131,110,111,140,141,210,211,220,230,231),1,0)
a_total_match_feb_main_data$choose_invest_19 <- ifelse(a_total_match_feb_main_data$choose_invest==1 & a_total_match_feb_main_data$feb_19==1,1,0)

a_total_match_feb_main_data$choose_bank <- ifelse(a_total_match_feb_main_data$kod_choice%in%c(210,211,220,230,231),1,0)
a_total_match_feb_main_data$choose_bank_19 <- ifelse(a_total_match_feb_main_data$choose_bank==1 & a_total_match_feb_main_data$feb_19==1,1,0)

a_total_match_feb_main_data$ex_50_nis_19 <- ifelse( a_total_match_feb_main_data$ex_50_nis==1 &  a_total_match_feb_main_data$feb_19==1,1,0)
a_total_match_feb_main_data$k_digital <-ifelse( a_total_match_feb_main_data$device%in%c("DevicePhone","DeviceComputer","DeviceComputer","DeviceTV","DeviceTablet"),1,0)
a_total_match_feb_main_data$no_digital <-ifelse( a_total_match_feb_main_data$device%in%c("DeviceUnknown"),1,NA)
a_total_match_feb_main_data$no_digital <-ifelse( a_total_match_feb_main_data$k_digital==1,0, a_total_match_feb_main_data$no_digital)
a_total_match_feb_main_data$feb19_nodigital <-ifelse( a_total_match_feb_main_data$no_digital==1 &  a_total_match_feb_main_data$feb_19==1,1,0)
a_total_match_feb_main_data$nodigital_50_19 <-ifelse( a_total_match_feb_main_data$no_digital==1 &  a_total_match_feb_main_data$feb_19==1 &  a_total_match_feb_main_data$ex_50_nis==1,1,0)
a_total_match_feb_main_data$phone <-ifelse( a_total_match_feb_main_data$device=="DevicePhone",1,0)
a_total_match_feb_main_data$phone_19 <-ifelse( a_total_match_feb_main_data$phone==1 &  a_total_match_feb_main_data$feb_19==1,1,0)
a_total_match_feb_main_data$no_phone <-ifelse( a_total_match_feb_main_data$device=="DevicePhone",0,1)
a_total_match_feb_main_data$no_phone_19 <-ifelse( a_total_match_feb_main_data$no_phone==1 &  a_total_match_feb_main_data$feb_19==1,1,0)
a_total_match_feb_main_data$computer <-ifelse( a_total_match_feb_main_data$device=="DeviceComputer",1,0)
a_total_match_feb_main_data$computer_19 <-ifelse( a_total_match_feb_main_data$computer==1 &  a_total_match_feb_main_data$feb_19==1,1,0)
a_total_match_feb_main_data$phone_19_50 <-ifelse( a_total_match_feb_main_data$phone==1 &  a_total_match_feb_main_data$ex_50_nis_19==1,1,0)
a_total_match_feb_main_data$phone_50 <-ifelse( a_total_match_feb_main_data$phone==1 &  a_total_match_feb_main_data$ex_50_nis==1,1,0)
a_total_match_feb_main_data$computer_19_50 <-ifelse( a_total_match_feb_main_data$computer==1 &  a_total_match_feb_main_data$ex_50_nis_19==1,1,0)
#cannot iuse digital to understand anything
a_feb_only_sms_nearest_digital <-  a_total_match_feb_main_data[ a_total_match_feb_main_data$k_digital==1,]
a_feb_only_sms_nearest_non_digital <-  a_total_match_feb_main_data[ a_total_match_feb_main_data$k_digital==0,]

a_feb_only_sms_nearest_feb19 <- a_total_match_feb_main_data[ a_total_match_feb_main_data$feb_19==1,]
a_feb_only_sms_nearest_feb19_digital <-a_feb_only_sms_nearest_digital[a_feb_only_sms_nearest_digital$feb_19==1,]

######################


h_total_match_feb_main_data$ex_50_nis_19 <- ifelse( h_total_match_feb_main_data$ex_50_nis==1 &  h_total_match_feb_main_data$feb_19==1,1,0)
h_total_match_feb_main_data$income= h_total_match_feb_main_data$father_wage+h_total_match_feb_main_data$mother_wage
h_total_match_feb_main_data$choose_risk <- ifelse(h_total_match_feb_main_data$kod_choice%in%c(120,121,130,131),1,0)
h_total_match_feb_main_data$investment_fund_risk_19 <- ifelse(h_total_match_feb_main_data$kod_choice%in%c(120,121,131,130) & h_total_match_feb_main_data$feb_19==1,1,0)

h_total_match_feb_main_data$choose_invest <- ifelse(h_total_match_feb_main_data$kod_choice%in%c(120,121,130,131,110,111,140,141,210,211,220,230,231),1,0)
h_total_match_feb_main_data$choose_invest_19 <- ifelse(h_total_match_feb_main_data$choose_invest==1 & h_total_match_feb_main_data$feb_19==1,1,0)

h_total_match_feb_main_data$choose_bank <- ifelse(h_total_match_feb_main_data$kod_choice%in%c(210,211,220,230,231),1,0)
h_total_match_feb_main_data$choose_bank_19 <- ifelse(h_total_match_feb_main_data$choose_bank==1 & h_total_match_feb_main_data$feb_19==1,1,0)


h_total_match_feb_main_data$k_digital <-ifelse( h_total_match_feb_main_data$device%in%c("DevicePhone","DeviceComputer","DeviceComputer","DeviceTV","DeviceTablet"),1,0)
h_total_match_feb_main_data$no_digital <-ifelse( h_total_match_feb_main_data$device%in%c("DeviceUnknown"),1,NA)
h_total_match_feb_main_data$no_digital <-ifelse( h_total_match_feb_main_data$k_digital==1,0, h_total_match_feb_main_data$no_digital)
h_total_match_feb_main_data$feb19_nodigital <-ifelse( h_total_match_feb_main_data$no_digital==1 &  h_total_match_feb_main_data$feb_19==1,1,0)
h_total_match_feb_main_data$nodigital_50_19 <-ifelse( h_total_match_feb_main_data$no_digital==1 &  h_total_match_feb_main_data$feb_19==1 &  h_total_match_feb_main_data$ex_50_nis==1,1,0)
h_total_match_feb_main_data$phone <-ifelse( h_total_match_feb_main_data$device=="DevicePhone",1,0)
h_total_match_feb_main_data$phone_19 <-ifelse( h_total_match_feb_main_data$phone==1 &  h_total_match_feb_main_data$feb_19==1,1,0)
h_total_match_feb_main_data$no_phone <-ifelse( h_total_match_feb_main_data$device=="DevicePhone",0,1)
h_total_match_feb_main_data$no_phone_19 <-ifelse( h_total_match_feb_main_data$no_phone==1 &  h_total_match_feb_main_data$feb_19==1,1,0)
h_total_match_feb_main_data$computer <-ifelse( h_total_match_feb_main_data$device=="DeviceComputer",1,0)
h_total_match_feb_main_data$computer_19 <-ifelse( h_total_match_feb_main_data$computer==1 &  h_total_match_feb_main_data$feb_19==1,1,0)
h_total_match_feb_main_data$phone_19_50 <-ifelse( h_total_match_feb_main_data$phone==1 &  h_total_match_feb_main_data$ex_50_nis_19==1,1,0)
h_total_match_feb_main_data$phone_50 <-ifelse( h_total_match_feb_main_data$phone==1 &  h_total_match_feb_main_data$ex_50_nis==1,1,0)
h_total_match_feb_main_data$computer_19_50 <-ifelse( h_total_match_feb_main_data$computer==1 &  h_total_match_feb_main_data$ex_50_nis_19==1,1,0)


h_feb_only_sms_nearest_digital <-  h_total_match_feb_main_data[ h_total_match_feb_main_data$k_digital==1,]
h_feb_only_sms_nearest_non_digital <-  h_total_match_feb_main_data[ h_total_match_feb_main_data$k_digital==0,]

h_feb_only_sms_nearest_feb19 <- h_total_match_feb_main_data[ h_total_match_feb_main_data$feb_19==1,]
h_feb_only_sms_nearest_feb19_digital <-h_feb_only_sms_nearest_digital[h_feb_only_sms_nearest_digital$feb_19==1,]

######################n
feb_nearest_main_t_data$feb19_digital <-ifelse(feb_nearest_main_t_data$k_digital==1 & feb_nearest_main_t_data$feb_19==1,1,0)
h_total_match_feb_main_data$feb19_digital <-ifelse(h_total_match_feb_main_data$k_digital==1 & h_total_match_feb_main_data$feb_19==1,1,0)
a_total_match_feb_main_data$feb19_digital <-ifelse(a_total_match_feb_main_data$k_digital==1 & a_total_match_feb_main_data$feb_19==1,1,0)
j_total_match_feb_main_data$feb19_digital <-ifelse(j_total_match_feb_main_data$k_digital==1 & j_total_match_feb_main_data$feb_19==1,1,0)


feb_only_sms_mahalanobis_feb19 <-feb_nearest_main_t_data[feb_nearest_main_t_data$feb_19==1,]
h_feb_only_sms_nearest_feb19 <-h_total_match_feb_main_data[h_total_match_feb_main_data$feb_19==1,]
a_feb_only_sms_nearest_feb19 <-a_total_match_feb_main_data[a_total_match_feb_main_data$feb_19==1,]
j_feb_only_sms_nearest_feb19 <-j_total_match_feb_main_data[j_total_match_feb_main_data$feb_19==1,]

##################################regressions

###Table 1
###########################
prob_reg1=lm(feb_19~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married+
               sms_feb*parents_arab+
              sms_feb*parents_hardi,
             data= feb_nearest_main_t_data,weights=weights)

summary(prob_reg1)

prob_reg2=lm(feb_19~ sms_feb+
                sms_feb*income+
                sms_feb*mother_academic+
                sms_feb*father_academic+
                sms_feb*parents_num_children+
                sms_feb*gill_child+
                 sms_feb*parents_married,
              #  sms_feb*parents_arab+ 
               # sms_feb*parents_hardi,
              data= a_total_match_feb_main_data,weights=weights)

summary(prob_reg2)


prob_reg3=lm(feb_19~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married,
             #  sms_feb*parents_arab+ 
             # sms_feb*parents_hardi,
             data= h_total_match_feb_main_data,weights=weights)

summary(prob_reg3)

prob_reg4=lm(feb_19~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married,
             #  sms_feb*parents_arab+ 
             # sms_feb*parents_hardi,
             data= j_total_match_feb_main_data,weights=weights)

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
          # order=c("sms_feb", "sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "sms_feb*income", "sms_feb*mother_academic", "sms_feb*father_academic",
          #         "sms_feb*parents_num_children", 
          #         "sms_feb*gill_child","sms_feb*parents_married",
          #         "parents_arab", "parents_hardi", "income", "mother_academic", "father_academic",
          #         "parents_num_children", "gill_child", "parents_married"),
          # 
          # covariate.labels = c("Received SMS", 
          #                      "Received SMS*Arab","Received SMS*Ultra-Orthodox",
          #                      "Received SMS*Family income",
          #                      "Received SMS*Mother academic","Received SMS*Father academic",
          #                      "Received SMS*Number of children","Received SMS*Age of child",
          #                      "Received SMS*Parents Married", 
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Number of children", "Age of child","Parents Married"),                     
          out="choose_int.htm",no.space=TRUE, digits=2 ,se =list(robust_se))


##################Table 7
###########################
prob_reg1=lm(feb_19~  sms_feb+
               sms_feb*periphery_cluster+
               sms_feb*socio_econ_cluster+
               sms_feb*rural+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married+
               sms_feb*parents_arab+
               sms_feb*parents_hardi,
               data= feb_nearest_main_t_data,weights=weights)

summary(prob_reg1)

prob_reg2=lm(feb_19~ sms_feb+
               sms_feb*rural+
               sms_feb*periphery_cluster+
               sms_feb*socio_econ_cluster+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married,
             #  sms_feb*parents_arab+ 
             # sms_feb*parents_hardi,
             data= a_total_match_feb_main_data,weights=weights)

summary(prob_reg2)


prob_reg3=lm(feb_19~ sms_feb+
               sms_feb*rural+
               sms_feb*periphery_cluster+
               sms_feb*socio_econ_cluster+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married,
             #  sms_feb*parents_arab+ 
             # sms_feb*parents_hardi,
             data= h_total_match_feb_main_data,weights=weights)

summary(prob_reg3)

prob_reg4=lm(feb_19~ sms_feb+
               sms_feb*rural+
               sms_feb*periphery_cluster+
               sms_feb*socio_econ_cluster+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married,
             #  sms_feb*parents_arab+ 
             # sms_feb*parents_hardi,
             data= j_total_match_feb_main_data,weights=weights)



coef1 <- vcovHC (prob_reg1)
coef2 <- vcovHC (prob_reg2)
coef3 <- vcovHC (prob_reg3)
coef4 <- vcovHC (prob_reg4)

robust_se1 <-(sqrt(diag(coef1)))
robust_se2 <-(sqrt(diag(coef2)))
robust_se3 <-(sqrt(diag(coef3)))
robust_se4 <-(sqrt(diag(coef4)))
robust_se <- c(robust_se1,robust_se2,robust_se3,robust_se4)



summary(prob_reg4)
stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Made any choice by Febuary 19th",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb",
          #         "sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "sms_feb*rural",
          #         "sms_feb*periphery_cluster","sms_feb*socio_econ_cluster",
          #         "sms_feb*income",
          #         "sms_feb*mother_academic","sms_feb*father_academic",
          #         "sms_feb*parents_married","sms_feb*gill_child","sms*parents_num_children",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS", 
          #                      "Received SMS*Arab","Received SMS*Ultra-Orthodox",
          #                      "Received SMS*Rural","Received SMS*Periphery index",
          #                      "Received SMS*Socio-econ index",
          #                      "Received SMS*Family income",
          #                      "Received SMS*Mother academic","Received SMS*Father academic",
          #                      "Received SMS*Parents Married","Received SMS*Age of child", 
          #                      "Received SMS*Number of children",
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="choose_lamas_int.htm",
          no.space=TRUE, digits=2, se =list(robust_se))

#####################
#Table 2

prob_reg1=lm(ex_50_nis_19~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married+
             sms_feb*parents_arab+ 
               sms_feb*parents_hardi,
             data= feb_nearest_main_t_data,weights=weights)

summary(prob_reg1)

prob_reg2=lm(ex_50_nis_19~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married,
             #  sms_feb*parents_arab+ 
             # sms_feb*parents_hardi,
             data= a_total_match_feb_main_data,weights=weights)

summary(prob_reg2)


prob_reg3=lm(ex_50_nis_19~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married,
             #  sms_feb*parents_arab+ 
             # sms_feb*parents_hardi,
             data= h_total_match_feb_main_data,weights=weights)

summary(prob_reg3)

prob_reg4=lm(ex_50_nis_19~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married,
             #  sms_feb*parents_arab+ 
             # sms_feb*parents_hardi,
             data= j_total_match_feb_main_data,weights=weights)

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

stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Choose to add funds by Febuary 19th",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb",
          #         "sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "sms_feb*income",
          #         "sms_feb*mother_academic","sms_feb*father_academic",
          #         "sms_feb*parents_married","sms_feb*gill_child","sms*parents_num_children",
          #         "parents_arab","parents_hardi",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # # covariate.labels = c("Received SMS", 
          #                      "Received SMS*Arab","Received SMS*Ultra-Orthodox",
          #                      "Received SMS*Family income",
          #                      "Received SMS*Mother academic","Received SMS*Father academic",
          #                      "Received SMS*Parents Married","Received SMS*Age of child", 
          #                      "Received SMS*Number of children",
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="50_int.htm",
          no.space=TRUE, digits=2, se =list(robust_se))

################Table 4

prob_reg1=lm(phone_19~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married+
               sms_feb*parents_arab+ 
              sms_feb*parents_hardi,
             data= feb_nearest_main_t_data,weights=weights)

summary(prob_reg1)

prob_reg2=lm(phone_19~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married,
             #  sms_feb*parents_arab+ 
             # sms_feb*parents_hardi,
             data= a_total_match_feb_main_data,weights=weights)

summary(prob_reg2)


prob_reg3=lm(phone_19~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married,
             #  sms_feb*parents_arab+ 
             # sms_feb*parents_hardi,
             data= h_total_match_feb_main_data,weights=weights)

summary(prob_reg3)

prob_reg4=lm(phone_19~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married,
             #  sms_feb*parents_arab+ 
             # sms_feb*parents_hardi,
             data= j_total_match_feb_main_data,weights=weights)

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

stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Choose by Smartphone by Febuary 19th",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb",
          #         "sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "sms_feb*income",
          #         "sms_feb*mother_academic","sms_feb*father_academic",
          #         "sms_feb*parents_married","sms_feb*gill_child","sms*parents_num_children",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS", 
          #                      "Received SMS*Arab","Received SMS*Ultra-Orthodox",
          #                      "Received SMS*Family income",
          #                      "Received SMS*Mother academic","Received SMS*Father academic",
          #                      "Received SMS*Parents Married","Received SMS*Age of child", 
          #                      "Received SMS*Number of children",
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="phone_int.htm",
          no.space=TRUE, digits=2, se =list(robust_se))


##################
#####Computer regressions discussed in text
prob_reg1=lm(computer_19~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married+
              sms_feb*parents_arab+ 
              sms_feb*parents_hardi,
             data= feb_nearest_main_t_data,weights=weights)

summary(prob_reg1)

prob_reg2=lm(computer_19~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married,
             #  sms_feb*parents_arab+ 
             # sms_feb*parents_hardi,
             data= a_total_match_feb_main_data,weights=weights)

summary(prob_reg2)


prob_reg3=lm(computer_19~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married,
             #  sms_feb*parents_arab+ 
             # sms_feb*parents_hardi,
             data= h_total_match_feb_main_data,weights=weights)

summary(prob_reg3)

prob_reg4=lm(computer_19~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married,
             #  sms_feb*parents_arab+ 
             # sms_feb*parents_hardi,
             data= j_total_match_feb_main_data,weights=weights)

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

stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Choose by Computer by Febuary 19th",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
#           order=c("sms_feb", "sms_feb*parents_arab","sms_feb*parents_hardi",
#                   "sms_feb*income", "sms_feb*mother_academic", "sms_feb*father_academic",
#                   "sms_feb*parents_num_children", 
#                   "sms_feb*gill_child","sms_feb*parents_married",
#                   "parents_arab", "parents_hardi", "income", "mother_academic", "father_academic",
#                   "parents_num_children", "gill_child", "parents_married"),
# # 
# #           covariate.labels = c("Received SMS",
#                                "Received SMS*Arab","Received SMS*Ultra-Orthodox",
#                                "Received SMS*Family income",
#                                "Received SMS*Mother academic","Received SMS*Father academic",
#                                "Received SMS*Parents Married","Received SMS*Age of child",
#                                "Received SMS*Number of children",
#                                "Arab","Ultra-Orthodox",
#                                "Family income","Mother academic", "Father academic",
#                                "Parents Married","Age of child","Number of children"),
          out="computer_int.htm",
          no.space=TRUE, digits=2, se =list(robust_se))


###3non digital choice discussed in text
prob_reg1=lm(feb19_nodigital~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married+
               sms_feb*parents_arab+ 
              sms_feb*parents_hardi,
             data= feb_nearest_main_t_data,weights=weights)

summary(prob_reg1)

prob_reg2=lm(feb19_nodigital~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married,
             #  sms_feb*parents_arab+ 
             # sms_feb*parents_hardi,
             data= a_total_match_feb_main_data,weights=weights)

summary(prob_reg2)


prob_reg3=lm(feb19_nodigital~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married,
             #  sms_feb*parents_arab+ 
             # sms_feb*parents_hardi,
             data= h_total_match_feb_main_data,weights=weights)

summary(prob_reg3)

prob_reg4=lm(feb19_nodigital~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married,
             #  sms_feb*parents_arab+ 
             # sms_feb*parents_hardi,
             data= j_total_match_feb_main_data,weights=weights)

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

stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Choose not digitaly by Febuary 19th",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb",
          #         "sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "sms_feb*income",
          #         "sms_feb*mother_academic","sms_feb*father_academic",
          #         "sms_feb*parents_married","sms_feb*gill_child","sms*parents_num_children",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS", 
          #                      "Received SMS*Arab","Received SMS*Ultra-Orthodox",
          #                      "Received SMS*Family income",
          #                      "Received SMS*Mother academic","Received SMS*Father academic",
          #                      "Received SMS*Parents Married","Received SMS*Age of child", 
          #                      "Received SMS*Number of children",
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="nodigital_int.htm",
          no.space=TRUE, digits=2, se =list(robust_se))


#####Table 3
prob_reg1=lm(investment_fund_risk_19~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married+
               sms_feb*parents_arab+ 
              sms_feb*parents_hardi,
             data= feb_nearest_main_t_data,weights=weights)

summary(prob_reg1)

prob_reg2=lm(investment_fund_risk_19~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married,
             #  sms_feb*parents_arab+ 
             # sms_feb*parents_hardi,
             data= a_total_match_feb_main_data,weights=weights)

summary(prob_reg2)


prob_reg3=lm(investment_fund_risk_19~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married,
             #  sms_feb*parents_arab+ 
             # sms_feb*parents_hardi,
             data= h_total_match_feb_main_data,weights=weights)

summary(prob_reg3)

prob_reg4=lm(investment_fund_risk_19~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married,
             #  sms_feb*parents_arab+ 
             # sms_feb*parents_hardi,
             data= j_total_match_feb_main_data,weights=weights)

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

stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Choose risky fund by Febuary 19th",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb",
          #         "sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "sms_feb*income",
          #         "sms_feb*mother_academic","sms_feb*father_academic",
          #         "sms_feb*parents_married","sms_feb*gill_child","sms*parents_num_children",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS", 
          #                      "Received SMS*Arab","Received SMS*Ultra-Orthodox",
          #                      "Received SMS*Family income",
          #                      "Received SMS*Mother academic","Received SMS*Father academic",
          #                      "Received SMS*Parents Married","Received SMS*Age of child", 
          #                      "Received SMS*Number of children",
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="risky_int.htm",
          no.space=TRUE, digits=2, se =list(robust_se))

#######


###discussed in text bank
prob_reg1=lm(choose_bank_19~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married+
              sms_feb*parents_arab+ 
             sms_feb*parents_hardi,
             data= feb_nearest_main_t_data,weights=weights)

summary(prob_reg1)

prob_reg2=lm(choose_bank_19~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married,
             #  sms_feb*parents_arab+ 
             # sms_feb*parents_hardi,
             data= a_total_match_feb_main_data,weights=weights)

summary(prob_reg2)


prob_reg3=lm(choose_bank_19~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married,
             #  sms_feb*parents_arab+ 
             # sms_feb*parents_hardi,
             data= h_total_match_feb_main_data,weights=weights)

summary(prob_reg3)

prob_reg4=lm(choose_bank_19~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married,
             #  sms_feb*parents_arab+ 
             # sms_feb*parents_hardi,
             data= j_total_match_feb_main_data,weights=weights)

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

stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Choose bank by Febuary 19th",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb",
          #         "sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "sms_feb*income",
          #         "sms_feb*mother_academic","sms_feb*father_academic",
          #         "sms_feb*parents_married","sms_feb*gill_child","sms*parents_num_children",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS", 
          #                      "Received SMS*Arab","Received SMS*Ultra-Orthodox",
          #                      "Received SMS*Family income",
          #                      "Received SMS*Mother academic","Received SMS*Father academic",
          #                      "Received SMS*Parents Married","Received SMS*Age of child", 
          #                      "Received SMS*Number of children",
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="bank_int.htm",
          no.space=TRUE, digits=2, se =list(robust_se))



##########################Robustness choose#
prob_reg1=lm(choose_clean_19~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married+
             sms_feb*parents_arab+ 
               sms_feb*parents_hardi,
             data= feb_nearest_main_t_data,weights=weights)

summary(prob_reg1)

prob_reg2=lm(choose_clean_19~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married,
             #  sms_feb*parents_arab+ 
             # sms_feb*parents_hardi,
             data= a_total_match_feb_main_data,weights=weights)

summary(prob_reg2)


prob_reg3=lm(choose_clean_19~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married,
             #  sms_feb*parents_arab+ 
             # sms_feb*parents_hardi,
             data= h_total_match_feb_main_data,weights=weights)

summary(prob_reg3)

prob_reg4=lm(choose_clean_19~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married,
             #  sms_feb*parents_arab+ 
             # sms_feb*parents_hardi,
             data= j_total_match_feb_main_data,weights=weights)

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

stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Made any active choice by Febuary 19th",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb",
          #         "sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "sms_feb*income",
          #         "sms_feb*mother_academic","sms_feb*father_academic",
          #         "sms_feb*parents_married","sms_feb*gill_child","sms*parents_num_children",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS", 
          #                      "Received SMS*Arab","Received SMS*Ultra-Orthodox",
          #                      "Received SMS*Family income",
          #                      "Received SMS*Mother academic","Received SMS*Father academic",
          #                      "Received SMS*Parents Married","Received SMS*Age of child", 
          #                      "Received SMS*Number of children",
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="choose_clean_int.htm",
          no.space=TRUE, digits=2, se =list(robust_se))


###########################choose religious discussed in text
prob_reg1=lm(kosher_19~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married+
             sms_feb*parents_arab+ 
               sms_feb*parents_hardi,
             data= feb_nearest_main_t_data,weights=weights)

summary(prob_reg1)

prob_reg2=lm(kosher_19~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married,
             #  sms_feb*parents_arab+ 
             # sms_feb*parents_hardi,
             data= a_total_match_feb_main_data,weights=weights)

summary(prob_reg2)


prob_reg3=lm(kosher_19~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married,
             #  sms_feb*parents_arab+ 
             # sms_feb*parents_hardi,
             data= h_total_match_feb_main_data,weights=weights)

summary(prob_reg3)

prob_reg4=lm(kosher_19~ sms_feb+
               sms_feb*income+
               sms_feb*mother_academic+
               sms_feb*father_academic+
               sms_feb*parents_num_children+
               sms_feb*gill_child+
               sms_feb*parents_married,
             #  sms_feb*parents_arab+ 
             # sms_feb*parents_hardi,
             data= j_total_match_feb_main_data,weights=weights)

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

stargazer(prob_reg1,prob_reg2,prob_reg3,prob_reg4, title="Choose religious investment by Febuary 19th",align=TRUE,
          type="html",dep.var.labels=c("All sample","Arab","Ultra-Orthodox","Other"),
          # order=c("sms_feb",
          #         "sms_feb*parents_arab","sms_feb*parents_hardi",
          #         "sms_feb*income",
          #         "sms_feb*mother_academic","sms_feb*father_academic",
          #         "sms_feb*parents_married","sms_feb*gill_child","sms*parents_num_children",
          #         "income",
          #         "mother_academic","father_academic",
          #         "parents_married","gill_child","parents_num_children"),
          # covariate.labels = c("Received SMS", 
          #                      "Received SMS*Arab","Received SMS*Ultra-Orthodox",
          #                      "Received SMS*Family income",
          #                      "Received SMS*Mother academic","Received SMS*Father academic",
          #                      "Received SMS*Parents Married","Received SMS*Age of child", 
          #                      "Received SMS*Number of children",
          #                      "Arab","Ultra-Orthodox",
          #                      "Family income","Mother academic", "Father academic",
          #                      "Parents Married","Age of child","Number of children"),                     
          out="choose_kosher_int.htm",
          no.space=TRUE, digits=2, se =list(robust_se))


##################
