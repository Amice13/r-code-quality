library(haven)
library(labelled)
library(lubridate)
library(questionr)
library(fixest)
library(modelsummary)
library(marginaleffects)
library(tidyverse)

#####
# DATA AVAILABILITY
# 
# This work is based on data from Understanding Society,
# produced by the University of Essex Institute of Social and Economic Research
# and supplied by the UK Data Service.
# 
# These data are Crown Copyright and have been used by permission.
#
# These data are available from the UK Data Service at the following permanent link: http://doi.org/10.5255/UKDA-Series-2000053
#
# Users seeking to replicate this work should download the indresp.dta file for waves 1-12,
# and save in the file structure as in the subsequent section.
#
#####
#CREATE PANEL
#Wave 1

w1 <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/a_indresp.dta")

w1$disabled <- case_when(w1$a_health==1 & w1$a_disdif1==1 ~ 1,
                         w1$a_health==1 & w1$a_disdif2==1 ~ 1,
                         w1$a_health==1 & w1$a_disdif3==1 ~ 1,
                         w1$a_health==1 & w1$a_disdif4==1 ~ 1,
                         w1$a_health==1 & w1$a_disdif5==1 ~ 1,
                         w1$a_health==1 & w1$a_disdif6==1 ~ 1,
                         w1$a_health==1 & w1$a_disdif7==1 ~ 1,
                         w1$a_health==1 & w1$a_disdif8==1 ~ 1,
                         w1$a_health==1 & w1$a_disdif9==1 ~ 1,
                         w1$a_health==1 & w1$a_disdif10==1 ~ 1,
                         w1$a_health==1 & w1$a_disdif11==1 ~ 1,
                         w1$a_health==1 & w1$a_disdif12==1 ~ 1,
                         w1$a_health==1 & w1$a_disdif1==0 & w1$a_disdif2==0 & w1$a_disdif3==0 & w1$a_disdif4==0 & w1$a_disdif5==0 & w1$a_disdif6==0 &
                           w1$a_disdif7==0 & w1$a_disdif8==0 & w1$a_disdif9==0 & w1$a_disdif10==0 & w1$a_disdif11==0 & w1$a_disdif12==0 ~ 0,
                         w1$a_health==2 ~ 0)

prop.table(table(w1$disabled))

w1 <- w1 %>%
  mutate(hcond = case_when(a_hcond96 == 0 ~ 1,
                           a_hcond96 == 1 ~ 0))

w1$party7 <- case_when(w1$a_vote4==1 | w1$a_vote3==1 ~ 1,
                       w1$a_vote4==2 | w1$a_vote3==2 ~ 2,
                       w1$a_vote4==3 | w1$a_vote3==3 ~ 3,
                       w1$a_vote4==4 | w1$a_vote3==4 ~ 4,
                       w1$a_vote4==5 | w1$a_vote3==5 ~ 5,
                       w1$a_vote4==6 | w1$a_vote3==6 ~ 6,
                       w1$a_vote4==7 | w1$a_vote3==7 ~ 7,
                       w1$a_vote4==8 | w1$a_vote3==8 ~ 7,
                       w1$a_vote4==9 | w1$a_vote3==9 ~ 7,
                       w1$a_vote4==10 | w1$a_vote3==10 ~ 7,
                       w1$a_vote4==11 | w1$a_vote3==11 ~ 7,
                       w1$a_vote4==97 | w1$a_vote3==97 ~ 7)

w1_red <- w1 %>% mutate(wave = 1, vote7 = NA_real_, vote8 = NA_real_) %>%
  select(pidp,wave,psu=a_psu,strata=a_strata,xw=a_indinus_xw,year=a_intdaty_dv,month=a_intdatm_dv,day=a_intdatd_dv,country=a_country,gor=a_gor_dv,
         disabled,age=a_dvage,sex=a_sex,m_inc=a_fimngrs_dv,empstat=a_jbstat,hiqual=a_hiqual_dv,ethn=a_ethn_dv,party7,vote7,
         vote1=a_vote1,vote2=a_vote2,vote3=a_vote3,vote4=a_vote4,vote8,health=a_health,disdif1=a_disdif1,disdif2=a_disdif2,disdif3=a_disdif3,
         disdif4=a_disdif4,disdif5=a_disdif5,disdif6=a_disdif6,disdif7=a_disdif7,disdif8=a_disdif8,disdif9=a_disdif9,disdif10=a_disdif10,
         disdif11=a_disdif11,disdif12=a_disdif12,vote5=a_vote5,finnow=a_finnow,finfut=a_finfut,hcond)

#Wave 2

w2 <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/b_indresp.dta")

w2$disabled <- case_when(w2$b_health==1 & w2$b_disdif1==1 ~ 1,
                         w2$b_health==1 & w2$b_disdif2==1 ~ 1,
                         w2$b_health==1 & w2$b_disdif3==1 ~ 1,
                         w2$b_health==1 & w2$b_disdif4==1 ~ 1,
                         w2$b_health==1 & w2$b_disdif5==1 ~ 1,
                         w2$b_health==1 & w2$b_disdif6==1 ~ 1,
                         w2$b_health==1 & w2$b_disdif7==1 ~ 1,
                         w2$b_health==1 & w2$b_disdif8==1 ~ 1,
                         w2$b_health==1 & w2$b_disdif9==1 ~ 1,
                         w2$b_health==1 & w2$b_disdif10==1 ~ 1,
                         w2$b_health==1 & w2$b_disdif11==1 ~ 1,
                         w2$b_health==1 & w2$b_disdif12==1 ~ 1,
                         w2$b_health==1 & w2$b_disdif1==0 & w2$b_disdif2==0 & w2$b_disdif3==0 & w2$b_disdif4==0 & w2$b_disdif5==0 & w2$b_disdif6==0 &
                           w2$b_disdif7==0 & w2$b_disdif8==0 & w2$b_disdif9==0 & w2$b_disdif10==0 & w2$b_disdif11==0 & w2$b_disdif12==0 ~ 0,
                         w2$b_health==2 ~ 0)

w2 <- w2 %>%
  mutate(hcond = case_when(b_hcondn96 == 0 ~ 1,
                           b_hcondn96 == 1 ~ 0))

w2$party7 <- case_when(w2$b_vote4==1 | w2$b_vote3==1 ~ 1,
                       w2$b_vote4==2 | w2$b_vote3==2 ~ 2,
                       w2$b_vote4==3 | w2$b_vote3==3 ~ 3,
                       w2$b_vote4==4 | w2$b_vote3==4 ~ 4,
                       w2$b_vote4==5 | w2$b_vote3==5 ~ 5,
                       w2$b_vote4==6 | w2$b_vote3==6 ~ 6,
                       w2$b_vote4==7 | w2$b_vote3==7 ~ 7,
                       w2$b_vote4==8 | w2$b_vote3==8 ~ 7,
                       w2$b_vote4==9 | w2$b_vote3==9 ~ 7,
                       w2$b_vote4==10 | w2$b_vote3==10 ~ 7,
                       w2$b_vote4==11 | w2$b_vote3==11 ~ 7,
                       w2$b_vote4==97 | w2$b_vote3==97 ~ 7)

w2$vote7 <- case_when(w2$b_vote8==1 ~ 1,
                      w2$b_vote8==2 ~ 2,
                      w2$b_vote8==3 ~ 3,
                      w2$b_vote8==4 ~ 4,
                      w2$b_vote8==5 ~ 5,
                      w2$b_vote8==6 ~ 6,
                      w2$b_vote8==7 ~ 7,
                      w2$b_vote8==8 ~ 7,
                      w2$b_vote8==9 ~ 7,
                      w2$b_vote8==10 ~ 7,
                      w2$b_vote8==11 ~ 7,
                      w2$b_vote8==97 ~ 7)

prop.table(table(w2$vote7))

w2_red <- w2 %>% mutate(wave = 2, month = (wave-1)*12+b_month) %>%
  select(pidp,wave,psu=b_psu,strata=b_strata,xw=b_indinub_xw,year=b_intdaty_dv,month=b_intdatm_dv,day=b_intdatd_dv,country = b_country,gor=b_gor_dv,
         disabled,age=b_dvage,sex=b_sex,m_inc=b_fimngrs_dv,empstat=b_jbstat,hiqual=b_hiqual_dv,ethn=b_ethn_dv,party7,vote7,
         vote1=b_vote1,vote2=b_vote2,vote3=b_vote3,vote4=b_vote4,vote8=b_vote8,health=b_health,disdif1=b_disdif1,disdif2=b_disdif2,disdif3=b_disdif3,
         disdif4=b_disdif4,disdif5=b_disdif5,disdif6=b_disdif6,disdif7=b_disdif7,disdif8=b_disdif8,disdif9=b_disdif9,disdif10=b_disdif10,
         disdif11=b_disdif11,disdif12=b_disdif12,vote5=b_vote5,finnow=b_finnow,finfut=b_finfut,hcond)

#Wave 3

w3 <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/c_indresp.dta")

w3$disabled <- case_when(w3$c_health==1 & w3$c_disdif1==1 ~ 1,
                         w3$c_health==1 & w3$c_disdif2==1 ~ 1,
                         w3$c_health==1 & w3$c_disdif3==1 ~ 1,
                         w3$c_health==1 & w3$c_disdif4==1 ~ 1,
                         w3$c_health==1 & w3$c_disdif5==1 ~ 1,
                         w3$c_health==1 & w3$c_disdif6==1 ~ 1,
                         w3$c_health==1 & w3$c_disdif7==1 ~ 1,
                         w3$c_health==1 & w3$c_disdif8==1 ~ 1,
                         w3$c_health==1 & w3$c_disdif9==1 ~ 1,
                         w3$c_health==1 & w3$c_disdif10==1 ~ 1,
                         w3$c_health==1 & w3$c_disdif11==1 ~ 1,
                         w3$c_health==1 & w3$c_disdif12==1 ~ 1,
                         w3$c_health==1 & w3$c_disdif1==0 & w3$c_disdif2==0 & w3$c_disdif3==0 & w3$c_disdif4==0 & w3$c_disdif5==0 & w3$c_disdif6==0 &
                           w3$c_disdif7==0 & w3$c_disdif8==0 & w3$c_disdif9==0 & w3$c_disdif10==0 & w3$c_disdif11==0 & w3$c_disdif12==0 ~ 0,
                         w3$c_health==2 ~ 0)

w3 <- w3 %>%
  mutate(hcond = case_when(c_hcond96 == 0 ~ 1,
                           c_hcond96 == 1 ~ 0))

w3$party7 <- case_when(w3$c_vote4==1 | w3$c_vote3==1 ~ 1,
                       w3$c_vote4==2 | w3$c_vote3==2 ~ 2,
                       w3$c_vote4==3 | w3$c_vote3==3 ~ 3,
                       w3$c_vote4==4 | w3$c_vote3==4 ~ 4,
                       w3$c_vote4==5 | w3$c_vote3==5 ~ 5,
                       w3$c_vote4==6 | w3$c_vote3==6 ~ 6,
                       w3$c_vote4==7 | w3$c_vote3==7 ~ 7,
                       w3$c_vote4==8 | w3$c_vote3==8 ~ 7,
                       w3$c_vote4==9 | w3$c_vote3==9 ~ 7,
                       w3$c_vote4==10 | w3$c_vote3==10 ~ 7,
                       w3$c_vote4==11 | w3$c_vote3==11 ~ 7,
                       w3$c_vote4==97 | w3$c_vote3==97 ~ 7)

w3$party4 <- case_when(w3$c_vote4==1 | w3$c_vote3==1 ~ 1,
                       w3$c_vote4==2 | w3$c_vote3==2 ~ 2,
                       w3$c_vote4==3 | w3$c_vote3==3 ~ 3,
                       w3$c_vote4==4 | w3$c_vote3==4 ~ 4,
                       w3$c_vote4==5 | w3$c_vote3==5 ~ 4,
                       w3$c_vote4==6 | w3$c_vote3==6 ~ 4,
                       w3$c_vote4==7 | w3$c_vote3==7 ~ 4,
                       w3$c_vote4==8 | w3$c_vote3==8 ~ 4,
                       w3$c_vote4==9 | w3$c_vote3==9 ~ 4,
                       w3$c_vote4==10 | w3$c_vote3==10 ~ 4,
                       w3$c_vote4==11 | w3$c_vote3==11 ~ 4,
                       w3$c_vote4==97 | w3$c_vote3==97 ~ 4)

w3_red <- w3 %>% mutate(wave = 3, month = (wave-1)*12+c_month, vote7 = NA_real_, vote8 = NA_real_) %>%
  select(pidp,wave,psu=c_psu,strata=c_strata,xw=c_indinub_xw,year=c_intdaty_dv,month=c_intdatm_dv,day=c_intdatd_dv,country = c_country,gor=c_gor_dv,
         disabled,age=c_dvage,sex=c_sex,m_inc=c_fimngrs_dv,empstat=c_jbstat,hiqual=c_hiqual_dv,ethn=c_ethn_dv,party7,vote7,
         vote1=c_vote1,vote2=c_vote2,vote3=c_vote3,vote4=c_vote4,vote8,health=c_health,disdif1=c_disdif1,disdif2=c_disdif2,disdif3=c_disdif3,
         disdif4=c_disdif4,disdif5=c_disdif5,disdif6=c_disdif6,disdif7=c_disdif7,disdif8=c_disdif8,disdif9=c_disdif9,disdif10=c_disdif10,
         disdif11=c_disdif11,disdif12=c_disdif12,vote5=c_vote5,finnow=c_finnow,finfut=c_finfut,hcond)

#Wave 4

w4 <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/d_indresp.dta")

w4$disabled <- case_when(w4$d_health==1 & w4$d_disdif1==1 ~ 1,
                         w4$d_health==1 & w4$d_disdif2==1 ~ 1,
                         w4$d_health==1 & w4$d_disdif3==1 ~ 1,
                         w4$d_health==1 & w4$d_disdif4==1 ~ 1,
                         w4$d_health==1 & w4$d_disdif5==1 ~ 1,
                         w4$d_health==1 & w4$d_disdif6==1 ~ 1,
                         w4$d_health==1 & w4$d_disdif7==1 ~ 1,
                         w4$d_health==1 & w4$d_disdif8==1 ~ 1,
                         w4$d_health==1 & w4$d_disdif9==1 ~ 1,
                         w4$d_health==1 & w4$d_disdif10==1 ~ 1,
                         w4$d_health==1 & w4$d_disdif11==1 ~ 1,
                         w4$d_health==1 & w4$d_disdif12==1 ~ 1,
                         w4$d_health==1 & w4$d_disdif1==0 & w4$d_disdif2==0 & w4$d_disdif3==0 & w4$d_disdif4==0 & w4$d_disdif5==0 & w4$d_disdif6==0 &
                           w4$d_disdif7==0 & w4$d_disdif8==0 & w4$d_disdif9==0 & w4$d_disdif10==0 & w4$d_disdif11==0 & w4$d_disdif12==0 ~ 0,
                         w4$d_health==2 ~ 0)

w4 <- w4 %>%
  mutate(hcond = case_when(d_hcond96 == 0 ~ 1,
                           d_hcond96 == 1 ~ 0))

w4$party7 <- case_when(w4$d_vote4==1 | w4$d_vote3==1 ~ 1,
                       w4$d_vote4==2 | w4$d_vote3==2 ~ 2,
                       w4$d_vote4==3 | w4$d_vote3==3 ~ 3,
                       w4$d_vote4==4 | w4$d_vote3==4 ~ 4,
                       w4$d_vote4==5 | w4$d_vote3==5 ~ 5,
                       w4$d_vote4==6 | w4$d_vote3==6 ~ 6,
                       w4$d_vote4==7 | w4$d_vote3==7 ~ 7,
                       w4$d_vote4==8 | w4$d_vote3==8 ~ 7,
                       w4$d_vote4==9 | w4$d_vote3==9 ~ 7,
                       w4$d_vote4==10 | w4$d_vote3==10 ~ 7,
                       w4$d_vote4==11 | w4$d_vote3==11 ~ 7,
                       w4$d_vote4==97 | w4$d_vote3==97 ~ 7)

w4$party4 <- case_when(w4$d_vote4==1 | w4$d_vote3==1 ~ 1,
                       w4$d_vote4==2 | w4$d_vote3==2 ~ 2,
                       w4$d_vote4==3 | w4$d_vote3==3 ~ 3,
                       w4$d_vote4==4 | w4$d_vote3==4 ~ 4,
                       w4$d_vote4==5 | w4$d_vote3==5 ~ 4,
                       w4$d_vote4==6 | w4$d_vote3==6 ~ 4,
                       w4$d_vote4==7 | w4$d_vote3==7 ~ 4,
                       w4$d_vote4==8 | w4$d_vote3==8 ~ 4,
                       w4$d_vote4==9 | w4$d_vote3==9 ~ 4,
                       w4$d_vote4==10 | w4$d_vote3==10 ~ 4,
                       w4$d_vote4==11 | w4$d_vote3==11 ~ 4,
                       w4$d_vote4==97 | w4$d_vote3==97 ~ 4)

w4_red <- w4 %>% mutate(wave = 4, month = (wave-1)*12+d_month, vote7 = NA_real_, vote8 = NA_real_) %>%
  select(pidp,wave,psu=d_psu,strata=d_strata,xw=d_indinub_xw,year=d_intdaty_dv,month=d_intdatm_dv,day=d_intdatd_dv,country = d_country,gor=d_gor_dv,
         disabled,age=d_dvage,sex=d_sex,m_inc=d_fimngrs_dv,empstat=d_jbstat,hiqual=d_hiqual_dv,ethn=d_ethn_dv,party7,vote7,
         vote1=d_vote1,vote2=d_vote2,vote3=d_vote3,vote4=d_vote4,vote8,health=d_health,disdif1=d_disdif1,disdif2=d_disdif2,disdif3=d_disdif3,
         disdif4=d_disdif4,disdif5=d_disdif5,disdif6=d_disdif6,disdif7=d_disdif7,disdif8=d_disdif8,disdif9=d_disdif9,disdif10=d_disdif10,
         disdif11=d_disdif11,disdif12=d_disdif12,vote5=d_vote5,finnow=d_finnow,finfut=d_finfut,hcond)

#Wave 5

w5 <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/e_indresp.dta")

w5$disabled <- case_when(w5$e_health==1 & w5$e_disdif1==1 ~ 1,
                         w5$e_health==1 & w5$e_disdif2==1 ~ 1,
                         w5$e_health==1 & w5$e_disdif3==1 ~ 1,
                         w5$e_health==1 & w5$e_disdif4==1 ~ 1,
                         w5$e_health==1 & w5$e_disdif5==1 ~ 1,
                         w5$e_health==1 & w5$e_disdif6==1 ~ 1,
                         w5$e_health==1 & w5$e_disdif7==1 ~ 1,
                         w5$e_health==1 & w5$e_disdif8==1 ~ 1,
                         w5$e_health==1 & w5$e_disdif9==1 ~ 1,
                         w5$e_health==1 & w5$e_disdif10==1 ~ 1,
                         w5$e_health==1 & w5$e_disdif11==1 ~ 1,
                         w5$e_health==1 & w5$e_disdif12==1 ~ 1,
                         w5$e_health==1 & w5$e_disdif1==0 & w5$e_disdif2==0 & w5$e_disdif3==0 & w5$e_disdif4==0 & w5$e_disdif5==0 & w5$e_disdif6==0 &
                           w5$e_disdif7==0 & w5$e_disdif8==0 & w5$e_disdif9==0 & w5$e_disdif10==0 & w5$e_disdif11==0 & w5$e_disdif12==0 ~ 0,
                         w5$e_health==2 ~ 0)

w5 <- w5 %>%
  mutate(hcond = case_when(e_hcond96 == 0 ~ 1,
                           e_hcond96 == 1 ~ 0))

w5$party7 <- case_when(w5$e_vote4==1 | w5$e_vote3==1 ~ 1,
                       w5$e_vote4==2 | w5$e_vote3==2 ~ 2,
                       w5$e_vote4==3 | w5$e_vote3==3 ~ 3,
                       w5$e_vote4==4 | w5$e_vote3==4 ~ 4,
                       w5$e_vote4==5 | w5$e_vote3==5 ~ 5,
                       w5$e_vote4==6 | w5$e_vote3==6 ~ 6,
                       w5$e_vote4==7 | w5$e_vote3==7 ~ 7,
                       w5$e_vote4==8 | w5$e_vote3==8 ~ 7,
                       w5$e_vote4==9 | w5$e_vote3==9 ~ 7,
                       w5$e_vote4==10 | w5$e_vote3==10 ~ 7,
                       w5$e_vote4==11 | w5$e_vote3==11 ~ 7,
                       w5$e_vote4==97 | w5$e_vote3==97 ~ 7)

w5$party4 <- case_when(w5$e_vote4==1 | w5$e_vote3==1 ~ 1,
                       w5$e_vote4==2 | w5$e_vote3==2 ~ 2,
                       w5$e_vote4==3 | w5$e_vote3==3 ~ 3,
                       w5$e_vote4==4 | w5$e_vote3==4 ~ 4,
                       w5$e_vote4==5 | w5$e_vote3==5 ~ 4,
                       w5$e_vote4==6 | w5$e_vote3==6 ~ 4,
                       w5$e_vote4==7 | w5$e_vote3==7 ~ 4,
                       w5$e_vote4==8 | w5$e_vote3==8 ~ 4,
                       w5$e_vote4==9 | w5$e_vote3==9 ~ 4,
                       w5$e_vote4==10 | w5$e_vote3==10 ~ 4,
                       w5$e_vote4==11 | w5$e_vote3==11 ~ 4,
                       w5$e_vote4==97 | w5$e_vote3==97 ~ 4)

w5_red <- w5 %>% mutate(wave = 5, month = (wave-1)*12+e_month, vote7 = NA_real_, vote8 = NA_real_) %>%
  select(pidp,wave,psu=e_psu,strata=e_strata,xw=e_indinub_xw,year=e_intdaty_dv,month=e_intdatm_dv,day=e_intdatd_dv,country = e_country,gor=e_gor_dv,
         disabled,age=e_dvage,sex=e_sex,m_inc=e_fimngrs_dv,empstat=e_jbstat,hiqual=e_hiqual_dv,ethn=e_ethn_dv,party7,vote7,
         vote1=e_vote1,vote2=e_vote2,vote3=e_vote3,vote4=e_vote4,vote8,health=e_health,disdif1=e_disdif1,disdif2=e_disdif2,disdif3=e_disdif3,
         disdif4=e_disdif4,disdif5=e_disdif5,disdif6=e_disdif6,disdif7=e_disdif7,disdif8=e_disdif8,disdif9=e_disdif9,disdif10=e_disdif10,
         disdif11=e_disdif11,disdif12=e_disdif12,vote5=e_vote5,finnow=e_finnow,finfut=e_finfut,hcond)

#Wave 6

w6 <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/f_indresp.dta")

w6$disabled <- case_when(w6$f_health==1 & w6$f_disdif1==1 ~ 1,
                         w6$f_health==1 & w6$f_disdif2==1 ~ 1,
                         w6$f_health==1 & w6$f_disdif3==1 ~ 1,
                         w6$f_health==1 & w6$f_disdif4==1 ~ 1,
                         w6$f_health==1 & w6$f_disdif5==1 ~ 1,
                         w6$f_health==1 & w6$f_disdif6==1 ~ 1,
                         w6$f_health==1 & w6$f_disdif7==1 ~ 1,
                         w6$f_health==1 & w6$f_disdif8==1 ~ 1,
                         w6$f_health==1 & w6$f_disdif9==1 ~ 1,
                         w6$f_health==1 & w6$f_disdif10==1 ~ 1,
                         w6$f_health==1 & w6$f_disdif11==1 ~ 1,
                         w6$f_health==1 & w6$f_disdif12==1 ~ 1,
                         w6$f_health==1 & w6$f_disdif1==0 & w6$f_disdif2==0 & w6$f_disdif3==0 & w6$f_disdif4==0 & w6$f_disdif5==0 & w6$f_disdif6==0 &
                           w6$f_disdif7==0 & w6$f_disdif8==0 & w6$f_disdif9==0 & w6$f_disdif10==0 & w6$f_disdif11==0 & w6$f_disdif12==0 ~ 0,
                         w6$f_health==2 ~ 0)

w6 <- w6 %>%
  mutate(hcond = case_when(f_hcond96 == 0 ~ 1,
                           f_hcond96 == 1 ~ 0))

w6$party7 <- case_when(w6$f_vote4==1 | w6$f_vote3==1 ~ 1,
                       w6$f_vote4==2 | w6$f_vote3==2 ~ 2,
                       w6$f_vote4==3 | w6$f_vote3==3 ~ 3,
                       w6$f_vote4==4 | w6$f_vote3==4 ~ 4,
                       w6$f_vote4==5 | w6$f_vote3==5 ~ 5,
                       w6$f_vote4==6 | w6$f_vote3==6 ~ 6,
                       w6$f_vote4==7 | w6$f_vote3==7 ~ 7,
                       w6$f_vote4==8 | w6$f_vote3==8 ~ 7,
                       w6$f_vote4==9 | w6$f_vote3==9 ~ 7,
                       w6$f_vote4==10 | w6$f_vote3==10 ~ 7,
                       w6$f_vote4==11 | w6$f_vote3==11 ~ 7,
                       w6$f_vote4==97 | w6$f_vote3==97 ~ 7)

w6$party4 <- case_when(w6$f_vote4==1 | w6$f_vote3==1 ~ 1,
                       w6$f_vote4==2 | w6$f_vote3==2 ~ 2,
                       w6$f_vote4==3 | w6$f_vote3==3 ~ 3,
                       w6$f_vote4==4 | w6$f_vote3==4 ~ 4,
                       w6$f_vote4==5 | w6$f_vote3==5 ~ 4,
                       w6$f_vote4==6 | w6$f_vote3==6 ~ 4,
                       w6$f_vote4==7 | w6$f_vote3==7 ~ 4,
                       w6$f_vote4==8 | w6$f_vote3==8 ~ 4,
                       w6$f_vote4==9 | w6$f_vote3==9 ~ 4,
                       w6$f_vote4==10 | w6$f_vote3==10 ~ 4,
                       w6$f_vote4==11 | w6$f_vote3==11 ~ 4,
                       w6$f_vote4==97 | w6$f_vote3==97 ~ 4)

w6_red <- w6 %>% mutate(wave = 6, month = (wave-1)*12+f_month, vote7 = NA_real_, vote8 = NA_real_) %>%
  select(pidp,wave,psu=f_psu,strata=f_strata,xw=f_indinui_xw,year=f_intdaty_dv,month=f_intdatm_dv,day=f_intdatd_dv,country=f_country,gor=f_gor_dv,
         disabled,age=f_dvage,sex=f_sex,m_inc=f_fimngrs_dv,empstat=f_jbstat,hiqual=f_hiqual_dv,ethn=f_ethn_dv,party7,vote7,
         vote1=f_vote1,vote2=f_vote2,vote3=f_vote3,vote4=f_vote4,vote8,health=f_health,disdif1=f_disdif1,disdif2=f_disdif2,disdif3=f_disdif3,
         disdif4=f_disdif4,disdif5=f_disdif5,disdif6=f_disdif6,disdif7=f_disdif7,disdif8=f_disdif8,disdif9=f_disdif9,disdif10=f_disdif10,
         disdif11=f_disdif11,disdif12=f_disdif12,vote5=f_vote5,finnow=f_finnow,finfut=f_finfut,hcond)

#Wave 7

w7 <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/g_indresp.dta")

w7$disabled <- case_when(w7$g_health==1 & w7$g_disdif1==1 ~ 1,
                         w7$g_health==1 & w7$g_disdif2==1 ~ 1,
                         w7$g_health==1 & w7$g_disdif3==1 ~ 1,
                         w7$g_health==1 & w7$g_disdif4==1 ~ 1,
                         w7$g_health==1 & w7$g_disdif5==1 ~ 1,
                         w7$g_health==1 & w7$g_disdif6==1 ~ 1,
                         w7$g_health==1 & w7$g_disdif7==1 ~ 1,
                         w7$g_health==1 & w7$g_disdif8==1 ~ 1,
                         w7$g_health==1 & w7$g_disdif9==1 ~ 1,
                         w7$g_health==1 & w7$g_disdif10==1 ~ 1,
                         w7$g_health==1 & w7$g_disdif11==1 ~ 1,
                         w7$g_health==1 & w7$g_disdif12==1 ~ 1,
                         w7$g_health==1 & w7$g_disdif1==0 & w7$g_disdif2==0 & w7$g_disdif3==0 & w7$g_disdif4==0 & w7$g_disdif5==0 & w7$g_disdif6==0 &
                           w7$g_disdif7==0 & w7$g_disdif8==0 & w7$g_disdif9==0 & w7$g_disdif10==0 & w7$g_disdif11==0 & w7$g_disdif12==0 ~ 0,
                         w7$g_health==2 ~ 0)

w7 <- w7 %>%
  mutate(hcond = case_when(g_hcond96 == 0 ~ 1,
                           g_hcond96 == 1 ~ 0))

w7$party7 <- case_when(w7$g_vote4==1 | w7$g_vote3==1 ~ 1,
                       w7$g_vote4==2 | w7$g_vote3==2 ~ 2,
                       w7$g_vote4==3 | w7$g_vote3==3 ~ 3,
                       w7$g_vote4==4 | w7$g_vote3==4 ~ 4,
                       w7$g_vote4==5 | w7$g_vote3==5 ~ 5,
                       w7$g_vote4==6 | w7$g_vote3==6 ~ 6,
                       w7$g_vote4==7 | w7$g_vote3==7 ~ 7,
                       w7$g_vote4==8 | w7$g_vote3==8 ~ 7,
                       w7$g_vote4==9 | w7$g_vote3==9 ~ 7,
                       w7$g_vote4==10 | w7$g_vote3==10 ~ 7,
                       w7$g_vote4==11 | w7$g_vote3==11 ~ 7,
                       w7$g_vote4==97 | w7$g_vote3==97 ~ 7)

w7$party4 <- case_when(w7$g_vote4==1 | w7$g_vote3==1 ~ 1,
                       w7$g_vote4==2 | w7$g_vote3==2 ~ 2,
                       w7$g_vote4==3 | w7$g_vote3==3 ~ 3,
                       w7$g_vote4==4 | w7$g_vote3==4 ~ 4,
                       w7$g_vote4==5 | w7$g_vote3==5 ~ 4,
                       w7$g_vote4==6 | w7$g_vote3==6 ~ 4,
                       w7$g_vote4==7 | w7$g_vote3==7 ~ 4,
                       w7$g_vote4==8 | w7$g_vote3==8 ~ 4,
                       w7$g_vote4==9 | w7$g_vote3==9 ~ 4,
                       w7$g_vote4==10 | w7$g_vote3==10 ~ 4,
                       w7$g_vote4==11 | w7$g_vote3==11 ~ 4,
                       w7$g_vote4==97 | w7$g_vote3==97 ~ 4)

w7$vote7 <- case_when(w7$g_vote8==1 ~ 1,
                      w7$g_vote8==2 ~ 2,
                      w7$g_vote8==3 ~ 3,
                      w7$g_vote8==4 ~ 4,
                      w7$g_vote8==5 ~ 5,
                      w7$g_vote8==6 ~ 6,
                      w7$g_vote8==7 ~ 7,
                      w7$g_vote8==8 ~ 7,
                      w7$g_vote8==9 ~ 7,
                      w7$g_vote8==10 ~ 7,
                      w7$g_vote8==11 ~ 7,
                      w7$g_vote8==97 ~ 7)

w7_red <- w7 %>% mutate(wave = 7, month = (wave-1)*12+g_month) %>%
  select(pidp,wave,psu=g_psu,strata=g_strata,xw=g_indinui_xw,year=g_intdaty_dv,month=g_intdatm_dv,day=g_intdatd_dv,country=g_country,gor=g_gor_dv,
         disabled,age=g_dvage,sex=g_sex,m_inc=g_fimngrs_dv,empstat=g_jbstat,hiqual=g_hiqual_dv,ethn=g_ethn_dv,party7,vote7,
         vote1=g_vote1,vote2=g_vote2,vote3=g_vote3,vote4=g_vote4,vote8=g_vote8,health=g_health,disdif1=g_disdif1,disdif2=g_disdif2,disdif3=g_disdif3,
         disdif4=g_disdif4,disdif5=g_disdif5,disdif6=g_disdif6,disdif7=g_disdif7,disdif8=g_disdif8,disdif9=g_disdif9,disdif10=g_disdif10,
         disdif11=g_disdif11,disdif12=g_disdif12,vote5=g_vote5,finnow=g_finnow,finfut=g_finfut,hcond)

#Wave 8

w8 <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/h_indresp.dta")

w8$disabled <- case_when(w8$h_health==1 & w8$h_disdif1==1 ~ 1,
                         w8$h_health==1 & w8$h_disdif2==1 ~ 1,
                         w8$h_health==1 & w8$h_disdif3==1 ~ 1,
                         w8$h_health==1 & w8$h_disdif4==1 ~ 1,
                         w8$h_health==1 & w8$h_disdif5==1 ~ 1,
                         w8$h_health==1 & w8$h_disdif6==1 ~ 1,
                         w8$h_health==1 & w8$h_disdif7==1 ~ 1,
                         w8$h_health==1 & w8$h_disdif8==1 ~ 1,
                         w8$h_health==1 & w8$h_disdif9==1 ~ 1,
                         w8$h_health==1 & w8$h_disdif10==1 ~ 1,
                         w8$h_health==1 & w8$h_disdif11==1 ~ 1,
                         w8$h_health==1 & w8$h_disdif12==1 ~ 1,
                         w8$h_health==1 & w8$h_disdif1==0 & w8$h_disdif2==0 & w8$h_disdif3==0 & w8$h_disdif4==0 & w8$h_disdif5==0 & w8$h_disdif6==0 &
                           w8$h_disdif7==0 & w8$h_disdif8==0 & w8$h_disdif9==0 & w8$h_disdif10==0 & w8$h_disdif11==0 & w8$h_disdif12==0 ~ 0,
                         w8$h_health==2 ~ 0)

w8$vote7 <- case_when(w8$h_vote8==1 ~ 1,
                      w8$h_vote8==2 ~ 2,
                      w8$h_vote8==3 ~ 3,
                      w8$h_vote8==4 ~ 4,
                      w8$h_vote8==5 ~ 5,
                      w8$h_vote8==6 ~ 6,
                      w8$h_vote8==7 ~ 7,
                      w8$h_vote8==8 ~ 7,
                      w8$h_vote8==9 ~ 7,
                      w8$h_vote8==10 ~ 7,
                      w8$h_vote8==11 ~ 7,
                      w8$h_vote8==97 ~ 7)

w8$vote4 <- case_when(w8$h_vote8==1 ~ 1,
                      w8$h_vote8==2 ~ 2,
                      w8$h_vote8==3 ~ 3,
                      w8$h_vote8==4 ~ 4,
                      w8$h_vote8==5 ~ 4,
                      w8$h_vote8==6 ~ 4,
                      w8$h_vote8==7 ~ 4,
                      w8$h_vote8==8 ~ 4,
                      w8$h_vote8==9 ~ 4,
                      w8$h_vote8==10 ~ 4,
                      w8$h_vote8==11 ~ 4,
                      w8$h_vote8==97 ~ 4)

w8_red <- w8 %>% mutate(wave = 8, month = (wave-1)*12+h_month, party7 = NA_real_,
                        vote1 = NA_real_, vote2 = NA_real_, vote3 = NA_real_, vote4 = NA_real_, vote5 = NA_real_) %>%
  select(pidp,wave,psu=h_psu,strata=h_strata,xw=h_indscui_xw,year=h_intdaty_dv,month=h_intdatm_dv,day=h_intdatd_dv,country=h_country,gor=h_gor_dv,
         disabled,age=h_dvage,sex=h_sex,m_inc=h_fimngrs_dv,empstat=h_jbstat,hiqual=h_hiqual_dv,ethn=h_ethn_dv,party7,vote7,
         vote1,vote2,vote3,vote4,vote8=h_vote8,health=h_health,disdif1=h_disdif1,disdif2=h_disdif2,disdif3=h_disdif3,
         disdif4=h_disdif4,disdif5=h_disdif5,disdif6=h_disdif6,disdif7=h_disdif7,disdif8=h_disdif8,disdif9=h_disdif9,disdif10=h_disdif10,
         disdif11=h_disdif11,disdif12=h_disdif12,finnow=h_finnow,finfut=h_finfut)

#Wave 9

w9 <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/i_indresp.dta")

w9$disabled <- case_when(w9$i_health==1 & w9$i_disdif1==1 ~ 1,
                         w9$i_health==1 & w9$i_disdif2==1 ~ 1,
                         w9$i_health==1 & w9$i_disdif3==1 ~ 1,
                         w9$i_health==1 & w9$i_disdif4==1 ~ 1,
                         w9$i_health==1 & w9$i_disdif5==1 ~ 1,
                         w9$i_health==1 & w9$i_disdif6==1 ~ 1,
                         w9$i_health==1 & w9$i_disdif7==1 ~ 1,
                         w9$i_health==1 & w9$i_disdif8==1 ~ 1,
                         w9$i_health==1 & w9$i_disdif9==1 ~ 1,
                         w9$i_health==1 & w9$i_disdif10==1 ~ 1,
                         w9$i_health==1 & w9$i_disdif11==1 ~ 1,
                         w9$i_health==1 & w9$i_disdif12==1 ~ 1,
                         w9$i_health==1 & w9$i_disdif1==0 & w9$i_disdif2==0 & w9$i_disdif3==0 & w9$i_disdif4==0 & w9$i_disdif5==0 & w9$i_disdif6==0 &
                           w9$i_disdif7==0 & w9$i_disdif8==0 & w9$i_disdif9==0 & w9$i_disdif10==0 & w9$i_disdif11==0 & w9$i_disdif12==0 ~ 0,
                         w9$i_health==2 ~ 0)

w9$party7 <- case_when(w9$i_vote4==1 | w9$i_vote3==1 ~ 1,
                       w9$i_vote4==2 | w9$i_vote3==2 ~ 2,
                       w9$i_vote4==3 | w9$i_vote3==3 ~ 3,
                       w9$i_vote4==4 | w9$i_vote3==4 ~ 4,
                       w9$i_vote4==5 | w9$i_vote3==5 ~ 5,
                       w9$i_vote4==6 | w9$i_vote3==6 ~ 6,
                       w9$i_vote4==7 | w9$i_vote3==7 ~ 7,
                       w9$i_vote4==8 | w9$i_vote3==8 ~ 7,
                       w9$i_vote4==9 | w9$i_vote3==9 ~ 7,
                       w9$i_vote4==10 | w9$i_vote3==10 ~ 7,
                       w9$i_vote4==11 | w9$i_vote3==11 ~ 7,
                       w9$i_vote4==97 | w9$i_vote3==97 ~ 7)

w9$party4 <- case_when(w9$i_vote4==1 | w9$i_vote3==1 ~ 1,
                       w9$i_vote4==2 | w9$i_vote3==2 ~ 2,
                       w9$i_vote4==3 | w9$i_vote3==3 ~ 3,
                       w9$i_vote4==4 | w9$i_vote3==4 ~ 4,
                       w9$i_vote4==5 | w9$i_vote3==5 ~ 4,
                       w9$i_vote4==6 | w9$i_vote3==6 ~ 4,
                       w9$i_vote4==7 | w9$i_vote3==7 ~ 4,
                       w9$i_vote4==8 | w9$i_vote3==8 ~ 4,
                       w9$i_vote4==9 | w9$i_vote3==9 ~ 4,
                       w9$i_vote4==10 | w9$i_vote3==10 ~ 4,
                       w9$i_vote4==11 | w9$i_vote3==11 ~ 4,
                       w9$i_vote4==97 | w9$i_vote3==97 ~ 4)

w9$vote7 <- case_when(w9$i_vote8==1 ~ 1,
                      w9$i_vote8==2 ~ 2,
                      w9$i_vote8==3 ~ 3,
                      w9$i_vote8==4 ~ 4,
                      w9$i_vote8==5 ~ 5,
                      w9$i_vote8==6 ~ 6,
                      w9$i_vote8==7 ~ 7,
                      w9$i_vote8==8 ~ 7,
                      w9$i_vote8==9 ~ 7,
                      w9$i_vote8==10 ~ 7,
                      w9$i_vote8==11 ~ 7,
                      w9$i_vote8==97 ~ 7)

w9_red <- w9 %>% mutate(wave = 9, month = (wave-1)*12+i_month) %>%
  select(pidp,wave,psu=i_psu,strata=i_strata,xw=i_indscui_xw,year=i_intdaty_dv,month=i_intdatm_dv,day=i_intdatd_dv,country=i_country,gor=i_gor_dv,
         disabled,age=i_dvage,sex=i_sex,m_inc=i_fimngrs_dv,empstat=i_jbstat,hiqual=i_hiqual_dv,ethn=i_ethn_dv,party7,vote7,
         vote1=i_vote1,vote2=i_vote2,vote3=i_vote3,vote4=i_vote4,vote8=i_vote8,health=i_health,disdif1=i_disdif1,disdif2=i_disdif2,disdif3=i_disdif3,
         disdif4=i_disdif4,disdif5=i_disdif5,disdif6=i_disdif6,disdif7=i_disdif7,disdif8=i_disdif8,disdif9=i_disdif9,disdif10=i_disdif10,
         disdif11=i_disdif11,disdif12=i_disdif12,vote5=i_vote5,finnow=i_finnow,finfut=i_finfut)

#Wave 10

w10 <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/j_indresp.dta")

w10$disabled <- case_when(w10$j_health==1 & w10$j_disdif1==1 ~ 1,
                          w10$j_health==1 & w10$j_disdif2==1 ~ 1,
                          w10$j_health==1 & w10$j_disdif3==1 ~ 1,
                          w10$j_health==1 & w10$j_disdif4==1 ~ 1,
                          w10$j_health==1 & w10$j_disdif5==1 ~ 1,
                          w10$j_health==1 & w10$j_disdif6==1 ~ 1,
                          w10$j_health==1 & w10$j_disdif7==1 ~ 1,
                          w10$j_health==1 & w10$j_disdif8==1 ~ 1,
                          w10$j_health==1 & w10$j_disdif9==1 ~ 1,
                          w10$j_health==1 & w10$j_disdif10==1 ~ 1,
                          w10$j_health==1 & w10$j_disdif11==1 ~ 1,
                          w10$j_health==1 & w10$j_disdif12==1 ~ 1,
                          w10$j_health==1 & w10$j_disdif1==0 & w10$j_disdif2==0 & w10$j_disdif3==0 & w10$j_disdif4==0 & w10$j_disdif5==0 & w10$j_disdif6==0 &
                            w10$j_disdif7==0 & w10$j_disdif8==0 & w10$j_disdif9==0 & w10$j_disdif10==0 & w10$j_disdif11==0 & w10$j_disdif12==0 ~ 0,
                          w10$j_health==2 ~ 0)

w10$party7 <- case_when(w10$j_vote4==1 | w10$j_vote3==1 ~ 1,
                        w10$j_vote4==2 | w10$j_vote3==2 ~ 2,
                        w10$j_vote4==3 | w10$j_vote3==3 ~ 3,
                        w10$j_vote4==4 | w10$j_vote3==4 ~ 4,
                        w10$j_vote4==5 | w10$j_vote3==5 ~ 5,
                        w10$j_vote4==6 | w10$j_vote3==6 ~ 6,
                        w10$j_vote4==7 | w10$j_vote3==7 ~ 7,
                        w10$j_vote4==8 | w10$j_vote3==8 ~ 7,
                        w10$j_vote4==9 | w10$j_vote3==9 ~ 7,
                        w10$j_vote4==10 | w10$j_vote3==10 ~ 7,
                        w10$j_vote4==11 | w10$j_vote3==11 ~ 7,
                        w10$j_vote4==97 | w10$j_vote3==97 ~ 7)

w10$vote7 <- case_when(w10$j_vote8==1 ~ 1,
                       w10$j_vote8==2 ~ 2,
                       w10$j_vote8==3 ~ 3,
                       w10$j_vote8==4 ~ 4,
                       w10$j_vote8==5 ~ 5,
                       w10$j_vote8==6 ~ 6,
                       w10$j_vote8==7 ~ 7,
                       w10$j_vote8==7 ~ 7,
                       w10$j_vote8==7 ~ 7,
                       w10$j_vote8==7 ~ 7,
                       w10$j_vote8==7 ~ 7,
                       w10$j_vote8==7 ~ 7)

w10_red <- w10 %>% mutate(wave = 10, month = (wave-1)*12+j_month) %>%
  select(pidp,wave,psu=j_psu,strata=j_strata,xw=j_indscui_xw,year=j_intdaty_dv,month=j_intdatm_dv,day=j_intdatd_dv,country=j_country,gor=j_gor_dv,
         disabled,age=j_dvage,sex=j_sex,m_inc=j_fimngrs_dv,empstat=j_jbstat,hiqual=j_hiqual_dv,ethn=j_ethn_dv,party7,vote7,
         vote1=j_vote1,vote2=j_vote2,vote3=j_vote3,vote4=j_vote4,vote8=j_vote8,health=j_health,disdif1=j_disdif1,disdif2=j_disdif2,disdif3=j_disdif3,
         disdif4=j_disdif4,disdif5=j_disdif5,disdif6=j_disdif6,disdif7=j_disdif7,disdif8=j_disdif8,disdif9=j_disdif9,disdif10=j_disdif10,
         disdif11=j_disdif11,disdif12=j_disdif12,vote5=j_vote5,finnow=j_finnow,finfut=j_finfut)

#Wave 11

w11 <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/k_indresp.dta")

w11$disabled <- case_when(w11$k_health==1 & w11$k_disdif1==1 ~ 1,
                          w11$k_health==1 & w11$k_disdif2==1 ~ 1,
                          w11$k_health==1 & w11$k_disdif3==1 ~ 1,
                          w11$k_health==1 & w11$k_disdif4==1 ~ 1,
                          w11$k_health==1 & w11$k_disdif5==1 ~ 1,
                          w11$k_health==1 & w11$k_disdif6==1 ~ 1,
                          w11$k_health==1 & w11$k_disdif7==1 ~ 1,
                          w11$k_health==1 & w11$k_disdif8==1 ~ 1,
                          w11$k_health==1 & w11$k_disdif9==1 ~ 1,
                          w11$k_health==1 & w11$k_disdif10==1 ~ 1,
                          w11$k_health==1 & w11$k_disdif11==1 ~ 1,
                          w11$k_health==1 & w11$k_disdif12==1 ~ 1,
                          w11$k_health==1 & w11$k_disdif1==0 & w11$k_disdif2==0 & w11$k_disdif3==0 & w11$k_disdif4==0 & w11$k_disdif5==0 & w11$k_disdif6==0 &
                            w11$k_disdif7==0 & w11$k_disdif8==0 & w11$k_disdif9==0 & w11$k_disdif10==0 & w11$k_disdif11==0 & w11$k_disdif12==0 ~ 0,
                          w11$k_health==2 ~ 0)

w11$party7 <- case_when(w11$k_vote4==1 | w11$k_vote3==1 ~ 1,
                        w11$k_vote4==2 | w11$k_vote3==2 ~ 2,
                        w11$k_vote4==3 | w11$k_vote3==3 ~ 3,
                        w11$k_vote4==4 | w11$k_vote3==4 ~ 4,
                        w11$k_vote4==5 | w11$k_vote3==5 ~ 5,
                        w11$k_vote4==6 | w11$k_vote3==6 ~ 6,
                        w11$k_vote4==7 | w11$k_vote3==7 ~ 7,
                        w11$k_vote4==8 | w11$k_vote3==8 ~ 7,
                        w11$k_vote4==9 | w11$k_vote3==9 ~ 7,
                        w11$k_vote4==10 | w11$k_vote3==10 ~ 7,
                        w11$k_vote4==11 | w11$k_vote3==11 ~ 7,
                        w11$k_vote4==97 | w11$k_vote3==97 ~ 7)

w11$vote7 <- case_when(w11$k_vote8==1 ~ 1,
                       w11$k_vote8==2 ~ 2,
                       w11$k_vote8==3 ~ 3,
                       w11$k_vote8==4 ~ 4,
                       w11$k_vote8==5 ~ 5,
                       w11$k_vote8==6 ~ 6,
                       w11$k_vote8==7 ~ 7,
                       w11$k_vote8==7 ~ 7,
                       w11$k_vote8==7 ~ 7,
                       w11$k_vote8==7 ~ 7,
                       w11$k_vote8==7 ~ 7,
                       w11$k_vote8==7 ~ 7)

w11_red <- w11 %>% mutate(wave = 11,month = (wave-1)*12+k_month) %>%
  select(pidp,wave,psu=k_psu,strata=k_strata,xw=k_indscui_xw,year=k_intdaty_dv,month=k_intdatm_dv,day=k_intdatd_dv,country=k_country,gor=k_gor_dv,
         disabled,age=k_dvage,sex=k_sex,m_inc=k_fimngrs_dv,empstat=k_jbstat,hiqual=k_hiqual_dv,ethn=k_ethn_dv,party7,vote7,
         vote1=k_vote1,vote2=k_vote2,vote3=k_vote3,vote4=k_vote4,vote8=k_vote8,health=k_health,disdif1=k_disdif1,disdif2=k_disdif2,disdif3=k_disdif3,
         disdif4=k_disdif4,disdif5=k_disdif5,disdif6=k_disdif6,disdif7=k_disdif7,disdif8=k_disdif8,disdif9=k_disdif9,disdif10=k_disdif10,
         disdif11=k_disdif11,disdif12=k_disdif12,vote5=k_vote5,finnow=k_finnow,finfut=k_finfut)

#Wave 12

w12 <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/l_indresp.dta", encoding = "latin1")

w12$disabled <- case_when(w12$l_health==1 & w12$l_disdif1==1 ~ 1,
                          w12$l_health==1 & w12$l_disdif2==1 ~ 1,
                          w12$l_health==1 & w12$l_disdif3==1 ~ 1,
                          w12$l_health==1 & w12$l_disdif4==1 ~ 1,
                          w12$l_health==1 & w12$l_disdif5==1 ~ 1,
                          w12$l_health==1 & w12$l_disdif6==1 ~ 1,
                          w12$l_health==1 & w12$l_disdif7==1 ~ 1,
                          w12$l_health==1 & w12$l_disdif8==1 ~ 1,
                          w12$l_health==1 & w12$l_disdif9==1 ~ 1,
                          w12$l_health==1 & w12$l_disdif10==1 ~ 1,
                          w12$l_health==1 & w12$l_disdif11==1 ~ 1,
                          w12$l_health==1 & w12$l_disdif12==1 ~ 1,
                          w12$l_health==1 & w12$l_disdif1==0 & w12$l_disdif2==0 & w12$l_disdif3==0 & w12$l_disdif4==0 & w12$l_disdif5==0 & w12$l_disdif6==0 &
                            w12$l_disdif7==0 & w12$l_disdif8==0 & w12$l_disdif9==0 & w12$l_disdif10==0 & w12$l_disdif11==0 & w12$l_disdif12==0 ~ 0,
                          w12$l_health==2 ~ 0)

w12$party7 <- case_when(w12$l_vote4==1 | w12$l_vote3==1 ~ 1,
                        w12$l_vote4==2 | w12$l_vote3==2 ~ 2,
                        w12$l_vote4==3 | w12$l_vote3==3 ~ 3,
                        w12$l_vote4==4 | w12$l_vote3==4 ~ 4,
                        w12$l_vote4==5 | w12$l_vote3==5 ~ 5,
                        w12$l_vote4==6 | w12$l_vote3==6 ~ 6,
                        w12$l_vote4==7 | w12$l_vote3==7 ~ 7,
                        w12$l_vote4==8 | w12$l_vote3==8 ~ 7,
                        w12$l_vote4==9 | w12$l_vote3==9 ~ 7,
                        w12$l_vote4==10 | w12$l_vote3==10 ~ 7,
                        w12$l_vote4==11 | w12$l_vote3==11 ~ 7,
                        w12$l_vote4==97 | w12$l_vote3==97 ~ 7)

w12$vote7 <- case_when(w12$l_vote8==1 ~ 1,
                       w12$l_vote8==2 ~ 2,
                       w12$l_vote8==3 ~ 3,
                       w12$l_vote8==4 ~ 4,
                       w12$l_vote8==5 ~ 5,
                       w12$l_vote8==6 ~ 6,
                       w12$l_vote8==7 ~ 7,
                       w12$l_vote8==7 ~ 7,
                       w12$l_vote8==7 ~ 7,
                       w12$l_vote8==7 ~ 7,
                       w12$l_vote8==7 ~ 7,
                       w12$l_vote8==7 ~ 7)

w12_red <- w12 %>% mutate(wave = 12, month = (wave-1)*24+l_month) %>%
  select(pidp,wave,psu=l_psu,strata=l_strata,xw=l_indscui_xw,year=l_intdaty_dv,month=l_intdatm_dv,day=l_intdatd_dv,country=l_country,gor=l_gor_dv,
         disabled,age=l_dvage,sex=l_sex,m_inc=l_fimngrs_dv,empstat=l_jbstat,hiqual=l_hiqual_dv,ethn=l_ethn_dv,party7,vote7,
         vote1=l_vote1,vote2=l_vote2,vote3=l_vote3,vote4=l_vote4,vote8=l_vote8,health=l_health,disdif1=l_disdif1,disdif2=l_disdif2,disdif3=l_disdif3,
         disdif4=l_disdif4,disdif5=l_disdif5,disdif6=l_disdif6,disdif7=l_disdif7,disdif8=l_disdif8,disdif9=l_disdif9,disdif10=l_disdif10,
         disdif11=l_disdif11,disdif12=l_disdif12,vote5=l_vote5,finnow=l_finnow,finfut=l_finfut)

#Combine to make panel

us_l <- bind_rows(w1_red,w2_red,w3_red,w4_red,w5_red,w6_red,w7_red,w8_red,w9_red,w10_red,w11_red,w12_red)

#add wave 7 longitudinal weight to long file

us_l <- left_join(us_l,select(w7,pidp,lw=g_indscus_lw))

#clean and code covars

us_l$year <- ifelse(us_l$year<0,NA,us_l$year)

us_l$month <- ifelse(us_l$month<0,NA,us_l$month)

us_l$day <- ifelse(us_l$day<0,NA,us_l$day)

us_l <- us_l %>%
  mutate(date = make_date(year, month, day))

us_l$age <- ifelse(us_l$age<0,NA,us_l$age)

us_l$sex <- ifelse(us_l$sex<0,NA,us_l$sex)

us_l$m_inc_r <- ifelse(us_l$m_inc<0,0,us_l$m_inc)

us_l$log_m_inc <- log(us_l$m_inc_r+1)

us_l$country <- factor(ifelse(us_l$country<0,NA,us_l$country))

us_l$gor <- factor(ifelse(us_l$gor<0,NA,us_l$gor))

us_l$con <- ifelse(us_l$party7==1,1,0)

us_l$lab <- ifelse(us_l$party7==2,1,0)

us_l$convslab <- case_when(us_l$party7==1 ~ 1,
                           us_l$party7==2 ~ 0)

us_l <- us_l %>%
  mutate(empstat4 = case_when(empstat==1~1,
                              empstat==2~1,
                              empstat==3~2,
                              empstat==4~3,
                              empstat==5~4,
                              empstat==6~4,
                              empstat==7~4,
                              empstat==8~4,
                              empstat==9~4,
                              empstat==10~4,
                              empstat==11~4,
                              empstat==12~4,
                              empstat==13~4,
                              empstat==97~4))

us_l <- us_l %>%
  mutate(ethmin = case_when(ethn==1~0,
                            ethn>1~1))

us_l <- us_l %>%
  mutate(degree = case_when(hiqual==1~1,
                            hiqual>1~0))

us_l <- us_l %>%
  mutate(edu3 = case_when(hiqual==1 ~ 3,
                          hiqual %in% 2:5 ~ 2,
                          hiqual==9 ~ 1))

#outcome vars

#missing coded as 0

table(us_l$vote3)

table(us_l$vote4)

table(us_l$vote3,us_l$wave)

us_l$leftright_noNA <- case_when(us_l$vote4==1 | us_l$vote3==1 ~ 1,
                                 us_l$vote4==2 | us_l$vote3==2 ~ 0,
                                 us_l$vote4==3 | us_l$vote3==3 ~ 0,
                                 us_l$vote4==4 | us_l$vote3==4 ~ 0,
                                 us_l$vote4==5 | us_l$vote3==5 ~ 0,
                                 us_l$vote4==6 | us_l$vote3==6 ~ 0,
                                 us_l$vote4==7 | us_l$vote3==7 ~ 0,
                                 us_l$vote4==8 | us_l$vote3==8 ~ 0,
                                 us_l$vote4==9 | us_l$vote3==9 ~ 0,
                                 us_l$vote4==10 | us_l$vote3==10 ~ 0,
                                 us_l$vote4==11 | us_l$vote3==11 ~ 0,
                                 us_l$vote4==12 | us_l$vote3==12 ~ 0,
                                 us_l$vote4==13 | us_l$vote3==13 ~ 0,
                                 us_l$vote4==14 | us_l$vote3==14 ~ 0,
                                 us_l$vote4==15 | us_l$vote3==15 ~ 0,
                                 us_l$vote3==95 ~ 0,
                                 us_l$vote4==97 | us_l$vote3==97 ~ 0,
                                 us_l$vote4==-2 | us_l$vote3==-2 ~ 0,
                                 us_l$vote4==-1 | us_l$vote3==-1 ~ 0)

table(us_l$leftright_noNA)

#protest

us_l$protest_noNA <- case_when(us_l$vote4==6 | us_l$vote3==6 ~ 1,
                               us_l$vote4==12 | us_l$vote3==12 ~ 1,
                               us_l$vote4==13 | us_l$vote3==13 ~ 1,
                               us_l$vote4==97 | us_l$vote3==97 ~ 1,
                               us_l$vote4==1 | us_l$vote3==1 ~ 0,
                               us_l$vote4==2 | us_l$vote3==2 ~ 0,
                               us_l$vote4==3 | us_l$vote3==3 ~ 0,
                               us_l$vote4==4 | us_l$vote3==4 ~ 0,
                               us_l$vote4==5 | us_l$vote3==5 ~ 0,
                               us_l$vote4==7 | us_l$vote3==7 ~ 0,
                               us_l$vote4==8 | us_l$vote3==8 ~ 0,
                               us_l$vote4==9 | us_l$vote3==9 ~ 0,
                               us_l$vote4==10 | us_l$vote3==10 ~ 0,
                               us_l$vote4==11 | us_l$vote3==11 ~ 0,
                               us_l$vote4==14 | us_l$vote3==14 ~ 0,
                               us_l$vote4==15 | us_l$vote3==15 ~ 0,
                               us_l$vote3==95 ~ 0,
                               us_l$vote4==-2 | us_l$vote3==-2 ~ 0,
                               us_l$vote4==-1 | us_l$vote3==-1 ~ 0)

table(us_l$protest_noNA)

#NA as protest

us_l$NA_as_protest <- case_when(us_l$vote4==6 | us_l$vote3==6 ~ 1,
                                us_l$vote4==12 | us_l$vote3==12 ~ 1,
                                us_l$vote4==13 | us_l$vote3==13 ~ 1,
                                us_l$vote4==97 | us_l$vote3==97 ~ 1,
                                us_l$vote3==95 ~ 1,
                                us_l$vote4==1 | us_l$vote3==1 ~ 0,
                                us_l$vote4==2 | us_l$vote3==2 ~ 0,
                                us_l$vote4==3 | us_l$vote3==3 ~ 0,
                                us_l$vote4==4 | us_l$vote3==4 ~ 0,
                                us_l$vote4==5 | us_l$vote3==5 ~ 0,
                                us_l$vote4==7 | us_l$vote3==7 ~ 0,
                                us_l$vote4==8 | us_l$vote3==8 ~ 0,
                                us_l$vote4==9 | us_l$vote3==9 ~ 0,
                                us_l$vote4==10 | us_l$vote3==10 ~ 0,
                                us_l$vote4==11 | us_l$vote3==11 ~ 0,
                                us_l$vote4==14 | us_l$vote3==14 ~ 0,
                                us_l$vote4==15 | us_l$vote3==15 ~ 0,
                                us_l$vote4==-2 | us_l$vote3==-2 ~ 0,
                                us_l$vote4==-1 | us_l$vote3==-1 ~ 0)

#vote_all no NA

us_l$vote_all_noNA <- case_when(us_l$vote4==1 | us_l$vote3==1 ~ 1,
                                us_l$vote4==2 | us_l$vote3==2 ~ 2,
                                us_l$vote4==3 | us_l$vote3==3 ~ 3,
                                us_l$vote4==4 | us_l$vote3==4 ~ 4,
                                us_l$vote4==5 | us_l$vote3==5 ~ 5,
                                us_l$vote4==6 | us_l$vote3==6 ~ 6,
                                us_l$vote4==7 | us_l$vote3==7 ~ 7,
                                us_l$vote4==8 | us_l$vote3==8 ~ 8,
                                us_l$vote4==9 | us_l$vote3==9 ~ 9,
                                us_l$vote4==10 | us_l$vote3==10 ~ 10,
                                us_l$vote4==11 | us_l$vote3==11 ~ 11,
                                us_l$vote4==12 | us_l$vote3==12 ~ 12,
                                us_l$vote4==13 | us_l$vote3==13 ~ 13,
                                us_l$vote4==14 | us_l$vote3==14 ~ 14,
                                us_l$vote4==15 | us_l$vote3==15 ~ 15,
                                us_l$vote4==97 | us_l$vote3==97 ~ 97,
                                us_l$vote3==95 ~ 0,
                                us_l$vote4==-2 | us_l$vote3==-2 ~ 0,
                                us_l$vote4==-1 | us_l$vote3==-1 ~ 0)

table(us_l$vote_all_noNA)

#code missing as NA

table(us_l$vote1)

us_l$vote1 <- ifelse(us_l$vote1<0,NA,us_l$vote1)

table(us_l$vote2)

us_l$vote2 <- ifelse(us_l$vote2<0,NA,us_l$vote2)

table(us_l$vote3)

us_l <- us_l %>%
  mutate(vote3 = case_when(vote3<0 ~ NA_real_,
                           vote3==95 ~ NA_real_,
                           vote3==96 ~ NA_real_,
                           TRUE ~ as.numeric(vote3)))

table(us_l$vote4)

us_l <- us_l %>%
  mutate(vote4 = case_when(vote4<0 ~ NA_real_,
                           vote4==95 ~ NA_real_,
                           vote4==96 ~ NA_real_,
                           TRUE ~ as.numeric(vote4)))

table(us_l$vote8)

us_l$vote8 <- ifelse(us_l$vote8<0,NA,us_l$vote8)

#vote all

us_l %>%
  filter(!is.na(us_l$vote4) & !is.na(us_l$vote3)) %>%
  count(wave)

us_l$vote_all <- case_when(us_l$vote4==1 | us_l$vote3==1 ~ 1,
                           us_l$vote4==2 | us_l$vote3==2 ~ 2,
                           us_l$vote4==3 | us_l$vote3==3 ~ 3,
                           us_l$vote4==4 | us_l$vote3==4 ~ 4,
                           us_l$vote4==5 | us_l$vote3==5 ~ 5,
                           us_l$vote4==6 | us_l$vote3==6 ~ 6,
                           us_l$vote4==7 | us_l$vote3==7 ~ 7,
                           us_l$vote4==8 | us_l$vote3==8 ~ 8,
                           us_l$vote4==9 | us_l$vote3==9 ~ 9,
                           us_l$vote4==10 | us_l$vote3==10 ~ 10,
                           us_l$vote4==11 | us_l$vote3==11 ~ 11,
                           us_l$vote4==12 | us_l$vote3==12 ~ 12,
                           us_l$vote4==13 | us_l$vote3==13 ~ 13,
                           us_l$vote4==14 | us_l$vote3==14 ~ 14,
                           us_l$vote4==15 | us_l$vote3==15 ~ 15,
                           us_l$vote4==97 | us_l$vote3==97 ~ 97)

vote_all_wave <- us_l %>%
  filter(country %in% 1:2 & wave < 8) %>%
  count(wave,vote_all) %>%
  spread(vote_all, n, fill = 0)

write.table(vote_all_wave, file = "vote_all_wave", sep = ",", quote = FALSE, row.names = F)

table(us_l$leftright_noNA,us_l$vote_all)

table(us_l$protest_noNA,us_l$vote_all)

#left-right outcome

table(us_l$vote3)

table(us_l$vote4)

table(us_l$vote3,us_l$wave)

us_l$leftright <- case_when(us_l$vote4==1 | us_l$vote3==1 ~ 1,
                            us_l$vote4==2 | us_l$vote3==2 ~ 0,
                            us_l$vote4==3 | us_l$vote3==3 ~ 0,
                            us_l$vote4==4 | us_l$vote3==4 ~ 0,
                            us_l$vote4==5 | us_l$vote3==5 ~ 0,
                            us_l$vote4==6 | us_l$vote3==6 ~ 0,
                            us_l$vote4==7 | us_l$vote3==7 ~ 0,
                            us_l$vote4==8 | us_l$vote3==8 ~ 0,
                            us_l$vote4==9 | us_l$vote3==9 ~ 0,
                            us_l$vote4==10 | us_l$vote3==10 ~ 0,
                            us_l$vote4==11 | us_l$vote3==11 ~ 0,
                            us_l$vote4==12 | us_l$vote3==12 ~ 0,
                            us_l$vote4==13 | us_l$vote3==13 ~ 0,
                            us_l$vote4==14 | us_l$vote3==14 ~ 0,
                            us_l$vote4==15 | us_l$vote3==15 ~ 0,
                            us_l$vote4==97 | us_l$vote3==97 ~ 0)

table(us_l$leftright)

#protest

us_l$protest <- case_when(us_l$vote4==6 | us_l$vote3==6 ~ 1,
                          us_l$vote4==12 | us_l$vote3==12 ~ 1,
                          us_l$vote4==13 | us_l$vote3==13 ~ 1,
                          us_l$vote4==97 | us_l$vote3==97 ~ 1,
                          us_l$vote4==1 | us_l$vote3==1 ~ 0,
                          us_l$vote4==2 | us_l$vote3==2 ~ 0,
                          us_l$vote4==3 | us_l$vote3==3 ~ 0,
                          us_l$vote4==4 | us_l$vote3==4 ~ 0,
                          us_l$vote4==5 | us_l$vote3==5 ~ 0,
                          us_l$vote4==7 | us_l$vote3==7 ~ 0,
                          us_l$vote4==8 | us_l$vote3==8 ~ 0,
                          us_l$vote4==9 | us_l$vote3==9 ~ 0,
                          us_l$vote4==10 | us_l$vote3==10 ~ 0,
                          us_l$vote4==11 | us_l$vote3==11 ~ 0,
                          us_l$vote4==14 | us_l$vote3==14 ~ 0,
                          us_l$vote4==15 | us_l$vote3==15 ~ 0)

table(us_l$protest)

# Check outcomes

us_l %>%
  filter(wave<8) %>%
  count(leftright,vote_all)

us_l %>%
  filter(wave<8) %>%
  count(protest,vote_all)

us_l %>%
  filter(wave<8) %>%
  count(leftright_noNA,vote_all_noNA)

us_l %>%
  filter(wave<8) %>%
  count(protest_noNA,vote_all_noNA)

#subjective finances

table(us_l$finnow,us_l$wave)

table(us_l$finfut,us_l$wave)

us_l <- us_l %>% mutate(across(starts_with("fin"), ~ ifelse(. < 0, NA, .)))

#previous health condition

table(us_l$hcond)

us_l %>%
  mutate(hcond = ifelse(wave==2,NA,hcond)) %>%
  count(hcond)

us_l %>%
  arrange(pidp,wave) %>%
  mutate(hcond = ifelse(wave==2,NA,hcond)) %>%
  group_by(pidp) %>%
  mutate(prevhcon = ifelse(1 %in% hcond,1,0)) %>%
  select(pidp,wave,hcond,prevhcon) %>%
  print(n=100)

#save full file

saveRDS(us_l, "us_l.rds")

#####
#LONGITUDINAL DATA
#Coding variables

us_l <- read_rds("us_l.rds")

#arrange dataset

us_l_red <- us_l %>%
  arrange(pidp,wave)

#lagged left-right

us_l_red <- us_l_red %>%
  group_by(pidp) %>%
  mutate(leftright_lag = lag(leftright)) %>%
  ungroup()

#lagged protest

us_l_red <- us_l_red %>%
  group_by(pidp) %>%
  mutate(protest_lag = lag(protest)) %>%
  ungroup()

#lead left-right

us_l_red <- us_l_red %>%
  group_by(pidp) %>%
  mutate(leftright_lead = lead(leftright)) %>%
  ungroup()

#lead protest

us_l_red <- us_l_red %>%
  group_by(pidp) %>%
  mutate(protest_lead = lead(protest)) %>%
  ungroup()

#fix age at first response so not co-linear with time
#and fix sex and ethnicity too

us_l_red <- us_l_red %>%
  group_by(pidp) %>%
  mutate(age_fix = age[row_number()==1],
         eth_fix = ethmin[row_number()==1],
         sex_fix = sex[row_number()==1]) %>%
  ungroup()

#check this has worked

us_l_red %>%
  group_by(pidp) %>%
  filter(length(table(age_fix))>1) %>%
  select(pidp,wave,age,age_fix)

us_l_red %>%
  group_by(pidp) %>%
  filter(length(table(ethmin))>1)

us_l_red %>%
  group_by(pidp) %>%
  filter(length(table(eth_fix))>1) %>%
  select(pidp,wave,ethmin,eth_fix)

us_l_red %>%
  group_by(pidp) %>%
  filter(length(table(sex))>1) %>%
  select(pidp,wave,sex,sex_fix)

us_l_red %>%
  group_by(pidp) %>%
  filter(length(table(sex_fix))>1) %>%
  select(pidp,wave,sex,sex_fix)

#drop under 18s

us_l_red <- us_l_red %>%
  filter(!age<18)

#restrict to E&W

us_l_red$country

us_l_red <- us_l_red %>% filter(country==1 | country==2)

#construct treatment variables

#severity

us_l_red <- us_l_red %>%
  mutate(health = ifelse(health < 0, NA_real_, health),
         across(starts_with("disdif"), ~ ifelse(.x < 0, NA_real_, .x)),
         disdif_sum = rowSums(across(starts_with("disdif")),na.rm = T),
         disab_sev = ifelse(health==2,0,ifelse(disdif_sum>1,2,disdif_sum)))

table(us_l_red$health, useNA = "always")

#type

us_l_red <- us_l_red %>%
  mutate(disab_phys = case_when(health==1 & rowSums(select(.,disdif1,disdif2,disdif3,disdif4,disdif10))>0 ~ 1,
                                !is.na(health) ~ 0),
         disab_comm = case_when(health==1 & rowSums(select(.,disdif5,disdif6,disdif7))>0 ~ 1,
                                !is.na(health) ~ 0),
         disab_ment = case_when(health==1 & rowSums(select(.,disdif8,disdif9))>0 ~ 1,
                                !is.na(health) ~ 0),
         disab_oth = case_when(health==1 & rowSums(select(.,disdif11,disdif12))>0 ~ 1,
                               !is.na(health) ~ 0))

#lagged disability

us_l_red <- us_l_red %>%
  group_by(pidp) %>%
  mutate(disab_lag = lag(disabled)) %>%
  ungroup()

#lead disability

us_l_red <- us_l_red %>%
  group_by(pidp) %>%
  mutate(disab_lead = lead(disabled)) %>%
  ungroup()

#check this has worked

us_l_red %>%
  group_by(pidp) %>%
  filter(1 %in% disabled) %>%
  select(pidp,wave,disabled,disab_lag) %>%
  print(n = 100)

#two-period disability

us_l_red <- us_l_red %>%
  group_by(pidp) %>%
  mutate(disab_two = case_when(lag(disabled) == 0 & disabled == 0 ~ 0,
                               lag(disabled) == 0 & disabled == 1 ~ 0,
                               lag(disabled) == 1 & disabled == 0 ~ 0,
                               lag(disabled) == 1 & disabled == 1 ~ 1)) %>%
  ungroup()

#check this has worked

us_l_red %>%
  group_by(pidp) %>%
  filter(1 %in% disabled) %>%
  select(pidp,wave,disabled,disab_two) %>%
  print(n = 100)

#create longitudinal variable, distinguishing transitions in and out

us_l_red <- us_l_red %>%
  group_by(pidp) %>%
  mutate(disab_long = case_when(lag(disabled) == 0 & disabled == 0 ~ 0,
                                lag(disabled) == 0 & disabled == 1 ~ 1,
                                lag(disabled) == 1 & disabled == 0 ~ 2,
                                lag(disabled) == 1 & disabled == 1 ~ 3)) %>%
  ungroup()

us_l_red %>%
  group_by(pidp) %>%
  filter(1 %in% disabled) %>%
  select(pidp,wave,disabled,disab_long) %>%
  print(n=100)

#define past, present vs not disabled

us_l_red <- us_l_red %>%
  group_by(pidp) %>%
  mutate(disabstart = ifelse(1 %in% disabled,
                             min(wave[disabled==1]),0)) %>%
  mutate(disab_past = case_when(disabled == 1 ~ 2,
                                disabled == 0 & 1 %in% disabled & wave>disabstart ~ 1,
                                disabled == 0 ~ 0)) %>%
  dplyr::select(-disabstart) %>%
  ungroup()

us_l_red %>%
  group_by(pidp) %>%
  arrange(pidp,wave) %>%
  filter(1 %in% disabled) %>%
  select(pidp,wave,disabled,disab_past) %>%
  print(n=100)

#continuously disabled

length(table(us_l_red$disabled))

us_l_red <- us_l_red %>%
  group_by(pidp) %>%
  mutate(disab_cont = case_when(disabled==1 & length(table(disabled))==1 ~ 2,
                                disabled==1 & length(table(disabled))>1 ~ 1,
                                TRUE ~ 0)) %>%
  ungroup()

us_l_red %>%
  select(pidp,wave,disabled,disab_cont) %>%
  print(n=100)

table(us_l_red$disabled,us_l_red$disab_cont)

#drop missing observations for all variables except lw and outcomes

us_l_red %>% summary()

us_l_red %>% filter(wave<8 & xw!=0) %>% select(disabled,con,convslab,protest,age,sex,ethmin,log_m_inc,empstat4,edu3) %>% summary()

us_l_red %>% filter(wave<8 & xw!=0 & !is.na(convslab))

prop.table(table(us_l_red$wave,us_l_red$convslab),margin=1)

table(is.na(us_l_red$m_inc))

table(is.na(us_l_red$m_inc_r))

table(us_l_red$vote4)

table(us_l_red$NA_as_protest,us_l_red$protest_noNA)

us_l_red <- us_l_red %>% drop_na(-lw,-party7,-vote7,-vote1,-vote2,
                                 -vote3,-vote4,-vote5,-vote8,-starts_with("disdif"),
                                 -con,-lab,-convslab,-leftright,-protest,-vote_all,
                                 -disab_lag,-disab_lead,-disab_two,-disab_long,-disab_past,
                                 -leftright_lag,-protest_lag,-leftright_lead,-protest_lead,-hcond,
                                 -leftright_noNA,-protest_noNA,-NA_as_protest,-vote_all_noNA,
                                 -finnow,-finfut)

names(us_l_red)

#save reduced files

saveRDS(us_l_red, "us_l_red.rds")

#####
#Descriptives

us_l_red <- readRDS("us_l_red.rds")

#produce some before dropping outcome vars

us_l_panel <- us_l_red %>% filter(wave<8)

table(is.na(us_l_panel$leftright))

#table 1
#construction of main outcome variable by disability

vote1_total <- us_l_panel %>%
  count(vote1, wt = xw) %>% 
  mutate(prop = n/sum(n)*100, vote1 = case_when(vote1==1~"Supports party: Yes")) %>%
  filter(vote1 == "Supports party: Yes") %>%
  rename(Variable=vote1,N=n,`%`=prop)

vote2_total <- us_l_panel %>%
  count(vote2, wt = xw) %>%
  mutate(prop = n/sum(n)*100, vote2 = case_when(vote2==1~"Closer to party: Yes")) %>%
  filter(vote2 == "Closer to party: Yes") %>% 
  rename(Variable=vote2,N=n,`%`=prop) 

vote4_total <- us_l_panel %>%
  count(is.na(vote4), wt = xw) %>%
  mutate(prop = n/sum(n)*100, `is.na(vote4)` = case_when(`is.na(vote4)`==FALSE~"Named party after first two questions")) %>%
  filter(`is.na(vote4)` == "Named party after first two questions") %>% 
  rename(Variable=`is.na(vote4)`,N=n,`%`=prop)

vote3_total <- us_l_panel %>%
  count(is.na(vote3), wt = xw) %>%
  mutate(prop = n/sum(n)*100, `is.na(vote3)` = case_when(`is.na(vote3)`==FALSE~"Named party after third question")) %>%
  filter(`is.na(vote3)` == "Named party after third question") %>%
  rename(Variable=`is.na(vote3)`,N=n,`%`=prop)

outcome_total <- us_l_panel %>%
  count(is.na(leftright), wt = xw) %>% 
  mutate(prop = n/sum(n)*100, `is.na(leftright)` = case_when(`is.na(leftright)`==FALSE~"Final variable: valid response",
                                                             `is.na(leftright)`==TRUE~"Final variable: missing")) %>%
  rename(Variable=`is.na(leftright)`,N=n,`%`=prop)

psupp_total <- bind_rows(vote1_total,vote2_total,vote4_total,vote3_total,outcome_total)

vote1_disabled <- us_l_panel %>%
  count(disabled, vote1, wt = xw) %>%
  group_by(disabled) %>% 
  mutate(prop = n/sum(n)*100, vote1 = case_when(vote1==1~"Supports party: Yes")) %>%
  filter(vote1 == "Supports party: Yes") %>% ungroup() %>%
  rename(Disabled=disabled,Variable=vote1,N=n,`%`=prop) %>% pivot_wider(names_from = "Disabled",values_from = c(N,`%`))

vote2_disabled <- us_l_panel %>%
  count(disabled, vote2, wt = xw) %>%
  group_by(disabled) %>% 
  mutate(prop = n/sum(n)*100, vote2 = case_when(vote2==1~"Closer to party: Yes")) %>%
  filter(vote2 == "Closer to party: Yes") %>% ungroup() %>%
  rename(Disabled=disabled,Variable=vote2,N=n,`%`=prop) %>% pivot_wider(names_from = "Disabled",values_from = c(N,`%`))

vote4_disabled <- us_l_panel %>%
  count(disabled, is.na(vote4), wt = xw) %>%
  group_by(disabled) %>% 
  mutate(prop = n/sum(n)*100, `is.na(vote4)` = case_when(`is.na(vote4)`==FALSE~"Named party after first two questions")) %>%
  filter(`is.na(vote4)` == "Named party after first two questions") %>% ungroup() %>%
  rename(Disabled=disabled,Variable=`is.na(vote4)`,N=n,`%`=prop) %>% pivot_wider(names_from = "Disabled",values_from = c(N,`%`))

vote3_disabled <- us_l_panel %>%
  count(disabled, is.na(vote3), wt = xw) %>%
  group_by(disabled) %>% 
  mutate(prop = n/sum(n)*100, `is.na(vote3)` = case_when(`is.na(vote3)`==FALSE~"Named party after third question")) %>%
  filter(`is.na(vote3)` == "Named party after third question") %>% ungroup() %>%
  rename(Disabled=disabled,Variable=`is.na(vote3)`,N=n,`%`=prop) %>% pivot_wider(names_from = "Disabled",values_from = c(N,`%`))

outcome_disabled <- us_l_panel %>%
  count(disabled, is.na(leftright), wt = xw) %>%
  group_by(disabled) %>% 
  mutate(prop = n/sum(n)*100, `is.na(leftright)` = case_when(`is.na(leftright)`==FALSE~"Final variable: valid response",
                                                             `is.na(leftright)`==TRUE~"Final variable: missing")) %>%
  ungroup() %>%
  rename(Disabled=disabled,Variable=`is.na(leftright)`,N=n,`%`=prop) %>% pivot_wider(names_from = "Disabled",values_from = c(N,`%`))

psupp_disabled <- bind_rows(vote1_disabled,vote2_disabled,vote4_disabled,vote3_disabled,outcome_disabled)

psupp_all <- bind_cols(psupp_total,psupp_disabled[-1]) %>%
  select(Variable,N,`%`,N_1,`%_1`,N_0,`%_0`) %>%
  mutate(across(contains("%"), round, 1)) %>%
  mutate(across(contains("N"), round, 0))

write.table(psupp_all, file = "psupp_all", sep = ",", quote = FALSE, row.names = F)

#create panel by reducing to wave 1-7 and dropping those with outcome var missing

us_l_panel <- us_l_red %>% filter(wave<8 & !is.na(vote_all))

#save panel file

saveRDS(us_l_panel, "us_l_panel.rds")

us_l_panel <- read_rds("us_l_panel.rds")

#Panel obs and N

us_l_panel %>% filter(xw!=0)

us_l_panel %>%
  summarise(obs = n(), N = length(unique(pidp)), ave_waves = obs/N)

#Disability descriptives

#For appendix
#Proportion and N disabled and heterogeneity

disab_prop <- us_l_panel %>%
  group_by(disabled) %>%
  count(disabled, wt = xw) %>% 
  mutate(prop_disab = n/sum(n)*100) %>%
  ungroup() %>%
  mutate(prop = n/sum(n)*100, Measure = "Disability") %>%
  rename(N=n,`%`=prop,`% of disabled`=prop_disab) %>%
  filter(disabled==1) %>%
  dplyr::select(Measure,N,`%`,`% of disabled`)

disab_phys_prop <- us_l_panel %>%
  group_by(disabled) %>%
  count(disab_phys, wt = xw) %>% 
  mutate(prop_disab = n/sum(n)*100) %>%
  ungroup() %>%
  mutate(prop = n/sum(n)*100, Measure = "Physical disability") %>%
  rename(N=n,`%`=prop,`% of disabled`=prop_disab) %>%
  filter(disab_phys==1) %>%
  dplyr::select(Measure,N,`%`,`% of disabled`)

disab_comm_prop <- us_l_panel %>%
  group_by(disabled) %>%
  count(disab_comm, wt = xw) %>% 
  mutate(prop_disab = n/sum(n)*100) %>%
  ungroup() %>%
  mutate(prop = n/sum(n)*100, Measure = "Communication disability") %>%
  rename(N=n,`%`=prop,`% of disabled`=prop_disab) %>%
  filter(disab_comm==1) %>%
  dplyr::select(Measure,N,`%`,`% of disabled`)

disab_ment_prop <- us_l_panel %>%
  group_by(disabled) %>%
  count(disab_ment, wt = xw) %>% 
  mutate(prop_disab = n/sum(n)*100) %>%
  ungroup() %>%
  mutate(prop = n/sum(n)*100, Measure = "Mental disability") %>%
  rename(N=n,`%`=prop,`% of disabled`=prop_disab) %>%
  filter(disab_ment==1) %>%
  dplyr::select(Measure,N,`%`,`% of disabled`)

disab_oth_prop <- us_l_panel %>%
  group_by(disabled) %>%
  count(disab_oth, wt = xw) %>% 
  mutate(prop_disab = n/sum(n)*100) %>%
  ungroup() %>%
  mutate(prop = n/sum(n)*100, Measure = "Other disability") %>%
  rename(N=n,`%`=prop,`% of disabled`=prop_disab) %>%
  filter(disab_oth==1) %>%
  dplyr::select(Measure,N,`%`,`% of disabled`)

disab_sev_prop <- us_l_panel %>%
  group_by(disabled) %>%
  count(disab_sev, wt = xw) %>% 
  mutate(prop_disab = n/sum(n)*100) %>%
  ungroup() %>%
  mutate(prop = n/sum(n)*100) %>%
  rename(N=n,`%`=prop,`% of disabled`=prop_disab) %>%
  filter(disab_sev!=0) %>%
  mutate(Measure = c("Single ADL","Multiple ADLs")) %>%
  dplyr::select(Measure,N,`%`,`% of disabled`)

disab_cont_prop <- us_l_panel %>%
  group_by(disabled) %>%
  count(disab_cont, wt = xw) %>% 
  mutate(prop_disab = n/sum(n)*100) %>%
  ungroup() %>%
  mutate(prop = n/sum(n)*100) %>%
  rename(N=n,`%`=prop,`% of disabled`=prop_disab) %>%
  filter(disab_cont!=0) %>%
  mutate(Measure = c("Temporary","Continuous")) %>%
  dplyr::select(Measure,N,`%`,`% of disabled`)

disab_het_desc <- bind_rows(disab_prop,disab_phys_prop,disab_comm_prop,disab_ment_prop,disab_oth_prop,
                            disab_sev_prop,disab_cont_prop) %>%
  mutate(across(contains("%"), round, 1)) %>%
  mutate(across(contains("N"), round, 0))

write.table(disab_het_desc, file = "disab_het_desc", sep = ",", quote = FALSE, row.names = F)

#obs and N split by disability transition

dis_trans <- us_l_panel %>%
  group_by(pidp) %>%
  mutate(dis_lag = c(disabled[row_number()==1],diff(disabled))) %>%
  slice(-1) %>%
  ungroup() %>%
  group_by(dis_lag,disabled) %>%
  summarise(obs = sum(xw), N = length(unique(pidp))) %>%
  ungroup() %>%
  mutate(prop_obs = obs/sum(obs)*100, prop_N = N/sum(N)*100) %>%
  mutate(obs = round(obs, 0)) %>%
  mutate(across(contains("prop"), round, 1)) %>%
  dplyr::select(dis_lag,disabled,obs,prop_obs,N,prop_N)

write.table(dis_trans, file = "dis_trans", sep = ",", quote = FALSE, row.names = F)

#Proportion reporting disability at any point

us_l_panel <- us_l_panel %>%
  group_by(pidp) %>%
  mutate(dis_bin = if_else(1 %in% disabled,1,0)) %>%
  ungroup() 

us_l_panel %>%
  group_by(dis_bin) %>%
  summarise(obs = n(), N = length(unique(pidp)))

17809/(33788+17809)

#disability by wave

disab_wave <- as_tibble(cbind(wtd.table(us_l_panel$wave,us_l_panel$disabled,weights=us_l_panel$xw),
                              prop.table(wtd.table(us_l_panel$wave,us_l_panel$disabled,weights=us_l_panel$xw),margin = 1)))

names(disab_wave) <- c("N (non-disabled)","N (disabled)","% (non-disabled)","% (disabled)")

disab_wave <- disab_wave %>%
  mutate(across(3:4,~ . * 100)) %>%
  mutate(Wave = 1:7) %>%
  mutate(across(contains("%"), round, 1)) %>%
  mutate(across(contains("N "), round, 0)) %>%
  select(Wave, `N (non-disabled)`,`% (non-disabled)`,`N (disabled)`,`% (disabled)`)

write.table(disab_wave, file = "disab_wave", sep = ",", quote = FALSE, row.names = F)

#Party support descriptives

#table 1
#table of party support by disability

table(us_l_panel$vote_all)

count_voteall <- us_l_panel %>% filter(!vote_all %in% c(4,11)) %>%
  count(vote_all, wt = xw) %>% 
  mutate(prop = n/sum(n)*100, vote_all = case_when(vote_all==1~"Conservatives",
                                                   vote_all==2~"Labour",
                                                   vote_all==3~"Lib Dems",
                                                   vote_all==5~"Plaid Cymru",
                                                   vote_all==6~"Greens",
                                                   vote_all==12~"UKIP",
                                                   vote_all==13~"BNP",
                                                   vote_all==97~"Other")) %>%
  rename(Party=vote_all,N=n,`Vote share`=prop)

disab_voteall <- us_l_panel %>% filter(!vote_all %in% c(4,11)) %>%
  count(disabled, vote_all, wt = xw) %>%
  group_by(disabled) %>% 
  mutate(prop = n/sum(n)*100, vote_all = case_when(vote_all==1~"Conservatives",
                                                   vote_all==2~"Labour",
                                                   vote_all==3~"Lib Dems",
                                                   vote_all==5~"Plaid Cymru",
                                                   vote_all==6~"Greens",
                                                   vote_all==12~"UKIP",
                                                   vote_all==13~"BNP",
                                                   vote_all==97~"Other")) %>% ungroup() %>%
  rename(Disabled=disabled,Party=vote_all,N=n,`Vote share`=prop) %>% pivot_wider(names_from = "Disabled",values_from = c(N,"Vote share"))

pid_all <- left_join(count_voteall,disab_voteall)

table(us_l_panel$leftright)

count_leftright <- us_l_panel %>% filter(!vote_all %in% c(4,11)) %>%
  count(leftright, wt = xw) %>% 
  mutate(prop = n/sum(n)*100, leftright = case_when(leftright==1~"Left-Right",
                                                    leftright==0~"Left")) %>%
  rename(Party=leftright,N=n,`Vote share`=prop) %>% filter(Party=="Left-Right")

disab_leftright <- us_l_panel %>% filter(!vote_all %in% c(4,11)) %>%
  count(disabled, leftright, wt = xw) %>%
  group_by(disabled) %>% 
  mutate(prop = n/sum(n)*100, leftright = case_when(leftright==1~"Left-Right",
                                                    leftright==0~"Left")) %>%
  rename(Party=leftright,N=n,`Vote share`=prop) %>% ungroup() %>% filter(Party=="Left-Right") %>%
  pivot_wider(names_from = disabled,values_from = c(N,"Vote share"))

leftright_all <- left_join(count_leftright,disab_leftright)

table(us_l_panel$leftright)

count_protest <- us_l_panel %>% filter(!vote_all %in% c(4,11)) %>%
  count(protest, wt = xw) %>% 
  mutate(prop = n/sum(n)*100, protest = case_when(protest==1~"Protest",
                                                  protest==0~"Non-protest")) %>%
  rename(Party=protest,N=n,`Vote share`=prop) %>% filter(Party=="Protest")

disab_protest <- us_l_panel %>% filter(!vote_all %in% c(4,11)) %>%
  count(disabled, protest, wt = xw) %>%
  group_by(disabled) %>% 
  mutate(prop = n/sum(n)*100, protest = case_when(protest==1~"Protest",
                                                  protest==0~"Non-protest")) %>%
  rename(Party=protest,N=n,`Vote share`=prop) %>% ungroup() %>% filter(Party=="Protest") %>%
  pivot_wider(names_from = disabled,values_from = c(N,"Vote share"))

protest_all <- left_join(count_protest,disab_protest)

outcomes_all <- rbind(pid_all,leftright_all,protest_all) %>% select(Party,N,`Vote share`,N_1,`Vote share_1`,N_0,`Vote share_0`) %>%
  mutate(across(contains("Vote"), round, 1)) %>%
  mutate(across(contains("N"), round, 0))

write.table(outcomes_all, file = "outcomes_all", sep = ",", quote = FALSE, row.names = F)

us_l_panel %>% filter(!vote_all %in% c(4,11)) %>%
  count(disabled, age_group, vote_all, wt = xw) %>%
  group_by(disabled, age_group) %>% 
  mutate(prop = n/sum(n)*100, vote_all = case_when(vote_all==1~"Conservatives",
                                                   vote_all==2~"Labour",
                                                   vote_all==3~"Lib Dems",
                                                   vote_all==5~"Plaid Cymru",
                                                   vote_all==6~"Greens",
                                                   vote_all==12~"UKIP",
                                                   vote_all==13~"BNP",
                                                   vote_all==97~"Other")) %>% ungroup() %>%
  rename(Disabled=disabled,Party=vote_all,N=n,`Vote share`=prop) %>% pivot_wider(names_from = Disabled, values_from = c(N,"Vote share")) %>%
  print(n=Inf)

#Outcomes by wave

lr_wave <- as_tibble(cbind(wtd.table(us_l_panel$wave,us_l_panel$leftright,weights=us_l_panel$xw),
                           prop.table(wtd.table(us_l_panel$wave,us_l_panel$leftright,weights=us_l_panel$xw),margin = 1)))

lr_wave <- lr_wave[c(2,4)]

names(lr_wave) <- c("N (Left-Right)","% (Left-Right)")

protest_wave <- as_tibble(cbind(wtd.table(us_l_panel$wave,us_l_panel$protest,weights=us_l_panel$xw),
                                prop.table(wtd.table(us_l_panel$wave,us_l_panel$protest,weights=us_l_panel$xw),margin = 1)))

protest_wave <- protest_wave[c(2,4)]

names(protest_wave) <- c("N (Protest)","% (Protest)")

outcomes_wave <- cbind(lr_wave,protest_wave)

outcomes_wave <- outcomes_wave %>%
  mutate(Wave = 1:7) %>%
  mutate(across(contains("%"),~.*100)) %>%
  mutate(across(contains("%"), round, 1)) %>%
  mutate(across(contains("N "), round, 0)) %>%
  relocate(Wave)

write.table(outcomes_wave, file = "outcomes_wave", sep = ",", quote = FALSE, row.names = F)

#Party support transitions

#Left-Right

us_l_panel$leftright

table(us_l_panel$leftright,us_l_panel$vote_all)

lr_trans <- us_l_panel %>%
  group_by(pidp) %>%
  mutate(lr_lag = c(leftright[row_number()==1],diff(leftright))) %>%
  slice(-1) %>%
  ungroup() %>%
  group_by(lr_lag,leftright) %>%
  summarise(obs = sum(xw), N = length(unique(pidp))) %>%
  ungroup() %>%
  mutate(prop_obs = obs/sum(obs)*100, prop_N = N/sum(N)*100,) %>%
  mutate(obs = round(obs, 0)) %>%
  mutate(across(contains("prop"), round, 1)) %>%
  select(lr_lag,leftright,obs,prop_obs,N,prop_N)

write.table(lr_trans, file = "lr_trans", sep = ",", quote = FALSE, row.names = F)

protest_trans <- us_l_panel %>%
  group_by(pidp) %>%
  mutate(protest_lag = c(protest[row_number()==1],diff(protest))) %>%
  slice(-1) %>%
  ungroup() %>%
  group_by(protest_lag,protest) %>%
  summarise(obs = sum(xw), N = length(unique(pidp))) %>%
  ungroup() %>%
  mutate(prop_obs = obs/sum(obs)*100, prop_N = N/sum(N)*100,) %>%
  mutate(obs = round(obs, 0)) %>%
  mutate(across(contains("prop"), round, 1)) %>%
  select(protest_lag,protest,obs,prop_obs,N,prop_N)

write.table(protest_trans, file = "protest_trans", sep = ",", quote = FALSE, row.names = F)

#Covariates by disability

#categorical

disability_region <- us_l_panel %>%
  count(gor, disabled, wt = xw) %>%
  group_by(disabled) %>% 
  mutate(prop = n/sum(n)*100) %>%
  pivot_wider(names_from = "disabled",values_from = c(n,prop)) %>%
  mutate(across(contains("prop"), round, 1)) %>%
  mutate(across(contains("N"), round, 0))

disability_sex <- us_l_panel %>%
  count(sex, disabled, wt = xw) %>%
  group_by(disabled) %>% 
  mutate(prop = n/sum(n)*100) %>% 
  pivot_wider(names_from = "disabled",values_from = c(n,prop)) %>%
  mutate(across(contains("prop"), round, 1)) %>%
  mutate(across(contains("n"), round, 0))

disability_empstat <- us_l_panel %>%
  count(empstat4, disabled, wt = xw) %>%
  group_by(disabled) %>% 
  mutate(prop = n/sum(n)*100) %>% 
  pivot_wider(names_from = "disabled",values_from = c(n,prop)) %>%
  mutate(across(contains("prop"), round, 1)) %>%
  mutate(across(contains("n"), round, 0))

disability_edu <- us_l_panel %>%
  count(edu3, disabled, wt = xw) %>%
  group_by(disabled) %>% 
  mutate(prop = n/sum(n)*100) %>% 
  pivot_wider(names_from = "disabled",values_from = c(n,prop)) %>%
  mutate(across(contains("prop"), round, 1)) %>%
  mutate(across(contains("n"), round, 0))

disability_eth <- us_l_panel %>%
  count(ethmin, disabled, wt = xw) %>%
  group_by(disabled) %>% 
  mutate(prop = n/sum(n)*100) %>% 
  pivot_wider(names_from = "disabled",values_from = c(n,prop)) %>%
  mutate(across(contains("prop"), round, 1)) %>%
  mutate(across(contains("n"), round, 0))

disability_cat_covars <- rbind(disability_sex[-1],disability_empstat[-1],disability_edu[-1],disability_eth[-1],disability_region[-1]) %>%
  select(n_0,prop_0,n_1,prop_1)

write.table(disability_cat_covars, file = "disability_cat_covars", sep = ",", quote = FALSE, row.names = F)

#continuous

disability_age <- us_l_panel %>%
  group_by(disabled) %>%
  summarise(mean = wtd.mean(age, weights = xw)) %>%
  mutate(across(where(is.numeric), round, 1)) %>%
  pivot_wider(names_from = "disabled",values_from = "mean")

disability_income <- us_l_panel %>%
  group_by(disabled) %>%
  summarise(mean = wtd.mean(m_inc, weights = xw)) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  pivot_wider(names_from = "disabled",values_from = "mean")

disability_cont_covars <- rbind(disability_age,disability_income)

write.table(disability_cont_covars, file = "disability_cont_covars", sep = ",", quote = FALSE, row.names = F)

prop.table(wtd.table(us_l_panel$m_inc<0, weights = us_l_panel$xw))

#####
#MAIN RESULTS
#LPM regression on pooled data

us_l_panel <- read_rds("us_l_panel.rds")

#code variables as factors

us_l_panel$disabled <- as_factor(us_l_panel$disabled)

us_l_panel$disab_sev <- as_factor(us_l_panel$disab_sev)

us_l_panel$disab_phys <- as_factor(us_l_panel$disab_phys)

us_l_panel$disab_comm <- as_factor(us_l_panel$disab_comm)

us_l_panel$disab_ment <- as_factor(us_l_panel$disab_ment)

us_l_panel$disab_oth <- as_factor(us_l_panel$disab_oth)

us_l_panel$disab_lag <- as_factor(us_l_panel$disab_lag)

us_l_panel$disab_two <- as_factor(us_l_panel$disab_two)

us_l_panel$disab_long <- as_factor(us_l_panel$disab_long)

us_l_panel$disab_cont <- as_factor(us_l_panel$disab_cont)

us_l_panel$disdif_sum <- as_factor(us_l_panel$disdif_sum)

us_l_panel$disab_past <- as_factor(us_l_panel$disab_past)

year(us_l_panel$date)

month(us_l_panel$date)

table(us_l_panel$wave,year(us_l_panel$date))

us_l_panel$year_month <- as_factor(paste(us_l_panel$year,us_l_panel$month))

us_l_panel$year <- as_factor(us_l_panel$year)

us_l_panel$month <- as_factor(us_l_panel$month)

us_l_panel$wave <- as_factor(us_l_panel$wave)

us_l_panel$sex <- as_factor(us_l_panel$sex)

us_l_panel$sex_fix <- as_factor(us_l_panel$sex_fix)

us_l_panel$eth <- as_factor(us_l_panel$ethmin)

us_l_panel$eth_fix <- as_factor(us_l_panel$eth_fix)

us_l_panel$empstat4 <- as_factor(us_l_panel$empstat4)

us_l_panel$degree <- as_factor(us_l_panel$degree)

us_l_panel$edu3 <- as_factor(us_l_panel$edu3)

us_l_panel$leftright_lag <- as_factor(us_l_panel$leftright_lag)

us_l_panel$protest_lag <- as_factor(us_l_panel$protest_lag)

us_l_panel <- us_l_panel %>%
  mutate(age_group =  cut(age, breaks = c(-Inf,25,35,45,55,65,Inf)))

table(us_l_panel$age_group)

#construct Left-Right models

lr1 <- feols(leftright~disabled|year_month, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

lr2 <- feols(leftright~disabled+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

lr3 <- feols(leftright~disabled+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

#panel estimation using two-way fixed effects

lrtwfe1 <- feols(leftright~disabled|pidp+year_month, data=us_l_panel, weights=us_l_panel$xw)

lrtwfe2 <- feols(leftright~disabled|pidp+year_month+gor, data=us_l_panel, weights=us_l_panel$xw)

lrtwfe3 <- feols(leftright~disabled+empstat4+log_m_inc+edu3|pidp+year_month+gor, data=us_l_panel, weights=us_l_panel$xw)

#table 2
#output of main left-right results

#check fixest f-test functionality

f_test <- fitstat(lr1, type = "f")

f_test <- fitstat(lrtwfe1, type = "f")

f_test[["f"]][[1]]

#create modelsummary approach to generating custom gof

modlist <- list(lr1,lr2,lr3,lrtwfe1,lrtwfe2,lrtwfe3)

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

glance_custom.fixest <- function(x, ...) {
  # check if we should include the linear hypothesis test in this specific model
  if (!isTRUE(attr(x, "FTEST"))) return(NULL)
  
  #conduct test
  ftest <- fitstat(x, type = "f")
  
  # return a 1-row tibble with each statistic in separate columns
  out <- tibble(
    "F-test" = ftest[["f"]][[1]],
    "P-value" = ftest[["f"]][[2]])
  return(out)
}

etable(lr1,lr2,lr3,lrtwfe1,lrtwfe2,lrtwfe3)

cm <- c('disabled1' = 'Disability')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test","adj.r.squared","nobs"), coef_map = cm, output = "table 2.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#full results for appendix

cm <- c('edu33' = 'Quals: Degree or higher',
        'edu32' = 'Quals: Below degree',
        'edu31' = 'Quals: None (ref)',
        'log_m_inc' = 'Monthly income (log)',
        'empstat44' = 'Economically inactive',
        'empstat43' = 'Retired',
        'empstat42' = 'Unemployed',
        'empstat41' = 'Employed (ref)',
        'eth1' = 'Non-white British',
        'sex2' = 'Female',
        'I(age^2)' = 'Age^2',
        'age' = 'Age',
        'disabled1' = 'Disabled')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test","adj.r.squared","nobs"), coef_map = rev(cm), output = "table 2 appendix.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#estimated marginal means

marginal_means(lr1, variables = "disabled")

predictions(lr1,
            by = "disabled",
            newdata = datagrid(grid_type = "balanced"))

4.6/38.5

marginal_means(lr2, variables = "disabled")

(0.218/0.312)-1

9.4/31.2

#testing mediation effect
#implemented based on the following: https://stats.stackexchange.com/questions/93540/testing-equality-of-coefficients-from-two-different-regressions 

compare_coefs <- function(model1, model2, coefficient){
  
  # extract the relevant data
  b1 <- coef(model1)[[coefficient]]     #get reg. coefs for model 1
  se1 <- se(model1)[[coefficient]]    #get std errors for model 1
  
  b2 <- coef(model2)[[coefficient]]     #get reg. coefs for model 2
  se2 <- se(model2)[[coefficient]]    #get std errors for model 2
  
  # Clogg et al. (1995) formula as cited by Ray Paternoster et al. (1998)
  b = b1 - b2
  s1 = se1^2
  s2 = se2^2
  sc = s1 + s2
  v = b / sqrt(sc)
  
  data.frame(diff=b, zdiff=v, `p-value`=format(2*pnorm(-abs(v)), scientific=FALSE))
  
}

compare_coefs(lr2,lr3,"disabled1")

#type

lrtype1 <- feols(leftright~disab_phys+disab_comm+disab_ment+disab_oth|year_month, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

lrtype2 <- feols(leftright~disab_phys+disab_comm+disab_ment+disab_oth+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

lrtype3 <- feols(leftright~disab_phys+disab_comm+disab_ment+disab_oth+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

lrtwfetype1 <- feols(leftright~disab_phys+disab_comm+disab_ment+disab_oth|pidp+year_month, data=us_l_panel, weights=us_l_panel$xw)

lrtwfetype2 <- feols(leftright~disab_phys+disab_comm+disab_ment+disab_oth|pidp+year_month+gor, data=us_l_panel, weights=us_l_panel$xw)

lrtwfetype3 <- feols(leftright~disab_phys+disab_comm+disab_ment+disab_oth+empstat4+log_m_inc+edu3|pidp+year_month+gor, data=us_l_panel, weights=us_l_panel$xw)

etable(lrtype1,lrtype2,lrtype3,lrtwfetype1,lrtwfetype2,lrtwfetype3)

#test mediation

compare_coefs(lrtype2,lrtype3,"disab_phys1")
compare_coefs(lrtype2,lrtype3,"disab_comm1")
compare_coefs(lrtype2,lrtype3,"disab_ment1")
compare_coefs(lrtype2,lrtype3,"disab_oth1")

#produce joint equality of coefficients rows

modlist <- list(lrtype1,lrtype2,lrtype3,lrtwfetype1,lrtwfetype2,lrtwfetype3)

lrtype_lh_results <- lapply(modlist, function(model) {
  linearHypothesis(model, c("disab_phys1 = disab_comm1",
                            "disab_phys1 = disab_ment1",
                            "disab_phys1 = disab_oth1"))
})

add_rows <- as_tibble(bind_rows(lrtype_lh_results)) %>%
  drop_na() %>%
  mutate(stars = case_when(`Pr(>Chisq)`<0.001 ~ "***",
                           `Pr(>Chisq)`<0.01 ~ "**",
                           `Pr(>Chisq)`<0.05 ~ "*",
                           `Pr(>Chisq)`>=0.05 ~ ""),
         estimate = paste0(round(Chisq,2),stars),
         model = 1:6,
         term = "Equality of coefficients") %>%
  select(term,model,estimate) %>%
  pivot_wider(names_from = model, values_from = estimate)

attr(add_rows, "position") <- c(9)

#produce model summary

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('disab_oth1' = 'Other disability',
        'disab_ment1' = 'Mental disability',
        'disab_comm1' = 'Communication disability',
        'disab_phys1' = 'Physical disability')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test", "adj.r.squared", "nobs"), coef_map = rev(cm), output = "table 3a.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001), add_rows = add_rows)

#z-test for equality of coefficients

hypotheses(lrtype2, "disab_phys1 = disab_comm1")
hypotheses(lrtype2, "disab_phys1 = disab_ment1")
hypotheses(lrtype2, "disab_phys1 = disab_oth1")
hypotheses(lrtype2, "disab_comm1 = disab_ment1")
hypotheses(lrtype2, "disab_comm1 = disab_oth1")
hypotheses(lrtype2, "disab_ment1 = disab_oth1")

#severity

us_l_panel %>% select(disabled, disab_sev, disab_cont) %>% summary()

lrsev1 <- feols(leftright~disab_sev|year_month, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

lrsev2 <- feols(leftright~disab_sev+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

lrsev3 <- feols(leftright~disab_sev+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

lrtwfesev1 <- feols(leftright~disab_sev|pidp+year_month, data=us_l_panel, weights=us_l_panel$xw)

lrtwfesev2 <- feols(leftright~disab_sev|pidp+year_month+gor, data=us_l_panel, weights=us_l_panel$xw)

lrtwfesev3 <- feols(leftright~disab_sev+empstat4+log_m_inc+edu3|pidp+year_month+gor, data=us_l_panel, weights=us_l_panel$xw)

etable(lrsev1,lrsev2,lrsev3,lrtwfesev1,lrtwfesev2,lrtwfesev3)

#produce joint equality of coefficients rows

modlist <- list(lrsev1,lrsev2,lrsev3,lrtwfesev1,lrtwfesev2,lrtwfesev3)

lrsev_lh_results <- lapply(modlist, function(model) {
  linearHypothesis(model, "disab_sev1 = disab_sev2")
})

add_rows <- as_tibble(bind_rows(lrsev_lh_results)) %>%
  drop_na() %>%
  mutate(stars = case_when(`Pr(>Chisq)`<0.001 ~ "***",
                           `Pr(>Chisq)`<0.01 ~ "**",
                           `Pr(>Chisq)`<0.05 ~ "*",
                           `Pr(>Chisq)`>=0.05 ~ ""),
         estimate = paste0(round(Chisq,2),stars),
         model = 1:6,
         term = "Equality of coefficients") %>%
  select(term,model,estimate) %>%
  pivot_wider(names_from = model, values_from = estimate)

attr(add_rows, "position") <- c(5)

#produce summary table

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

attr(modlist[[1]], 'FTEST')

cm <- c('disab_sev2' = 'More than 1 ADL',
        'disab_sev1' = '1 ADL')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test", "adj.r.squared", "nobs"), coef_map = rev(cm), output = "table 3b.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001), add_rows = add_rows)

#z-test for equality of coefficients

hypotheses(lrsev2, "disab_sev1 = disab_sev2")
hypotheses(lrsev3, "disab_sev1 = disab_sev2")

#duration

lrcont1 <- feols(leftright~disab_cont|year_month, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

lrcont2 <- feols(leftright~disab_cont+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

lrcont3 <- feols(leftright~disab_cont+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

etable(lrcont1,lrcont2,lrcont3)

#produce joint equality of coefficients rows

modlist <- list(lrcont1,lrcont2,lrcont3)

lrcont_lh_results <- lapply(modlist, function(model) {
  linearHypothesis(model, "disab_cont1 = disab_cont2")
})

add_rows <- as_tibble(bind_rows(lrcont_lh_results)) %>%
  drop_na() %>%
  mutate(stars = case_when(`Pr(>Chisq)`<0.001 ~ "***",
                           `Pr(>Chisq)`<0.01 ~ "**",
                           `Pr(>Chisq)`<0.05 ~ "*",
                           `Pr(>Chisq)`>=0.05 ~ ""),
         estimate = paste0(round(Chisq,2),stars),
         model = 1:3,
         term = "Equality of coefficients") %>%
  select(term,model,estimate) %>%
  pivot_wider(names_from = model, values_from = estimate)

attr(add_rows, "position") <- c(5)

#produce model summary

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

attr(modlist[[1]], 'FTEST')

cm <- c('disab_cont1' = 'Temporary disability',
        'disab_cont2' = 'Continuous disability')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test", "adj.r.squared", "nobs"), coef_map = cm, output = "table 3c.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001), add_rows = add_rows)

#z-test for equality of coefficients

hypotheses(lrcont2, "disab_cont1 = disab_cont2")
hypotheses(lrcont3, "disab_cont1 = disab_cont2")

#Protest outcome
#construct baseline models

protest1 <- feols(protest~disabled|year_month, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

protest2 <- feols(protest~disabled+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

protest3 <- feols(protest~disabled+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

protesttwfe1 <- feols(protest~disabled|pidp+year_month, data=us_l_panel, weights=us_l_panel$xw)

protesttwfe2 <- feols(protest~disabled|pidp+year_month+gor, data=us_l_panel, weights=us_l_panel$xw)

protesttwfe3 <- feols(protest~disabled+empstat4+log_m_inc+edu3|pidp+year_month+gor, data=us_l_panel, weights=us_l_panel$xw)

etable(protest1,protest2,protest3,protesttwfe1,protesttwfe2,protesttwfe3)

modlist <- list(protest1,protest2,protest3,protesttwfe1,protesttwfe2,protesttwfe3)

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('disabled1' = 'Disability')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test", "adj.r.squared", "nobs"), coef_map = cm, output = "table 4.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#full results for appendix

cm <- c('edu33' = 'Quals: Degree or higher',
        'edu32' = 'Quals: Below degree',
        'edu31' = 'Quals: None (ref)',
        'log_m_inc' = 'Monthly income (log)',
        'empstat44' = 'Economically inactive',
        'empstat43' = 'Retired',
        'empstat42' = 'Unemployed',
        'empstat41' = 'Employed (ref)',
        'eth1' = 'Non-white British',
        'sex2' = 'Female',
        'I(age^2)' = 'Age^2',
        'age' = 'Age',
        'disabled1' = 'Disabled')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test","adj.r.squared","nobs"), coef_map = rev(cm), output = "table 4 appendix.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#predicted values

#estimated marginal means

marginal_means(protest1, variables = "disabled")

predictions(protest1,
            by = "disabled",
            newdata = datagrid(grid_type = "balanced"))

(0.137/0.113)

marginal_means(protest2, variables = "disabled")

(0.134/0.0912)

marginal_means(protest3, variables = "disabled")

(0.139/0.102)

#testing mediation effect
#implemented based on the following: https://stats.stackexchange.com/questions/93540/testing-equality-of-coefficients-from-two-different-regressions 

compare_coefs(protest2,protest3,"disabled1")

#type

protesttype1 <- feols(protest~disab_phys+disab_comm+disab_ment+disab_oth|year_month, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

protesttype2 <- feols(protest~disab_phys+disab_comm+disab_ment+disab_oth+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

protesttype3 <- feols(protest~disab_phys+disab_comm+disab_ment+disab_oth+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

protesttwfetype1 <- feols(protest~disab_phys+disab_comm+disab_ment+disab_oth|pidp+year_month, data=us_l_panel, weights=us_l_panel$xw)

protesttwfetype2 <- feols(protest~disab_phys+disab_comm+disab_ment+disab_oth|pidp+year_month+gor, data=us_l_panel, weights=us_l_panel$xw)

protesttwfetype3 <- feols(protest~disab_phys+disab_comm+disab_ment+disab_oth+empstat4+log_m_inc+edu3|pidp+year_month+gor, data=us_l_panel, weights=us_l_panel$xw)

etable(protesttype1,protesttype2,protesttype3,protesttwfetype1,protesttwfetype2,protesttwfetype3)

#produce joint equality of coefficients rows

modlist <- list(protesttype1,protesttype2,protesttype3,protesttwfetype1,protesttwfetype2,protesttwfetype3)

protesttype_lh_results <- lapply(modlist, function(model) {
  linearHypothesis(model, c("disab_phys1 = disab_comm1",
                            "disab_phys1 = disab_ment1",
                            "disab_phys1 = disab_oth1"))
})

add_rows <- as_tibble(bind_rows(protesttype_lh_results)) %>%
  drop_na() %>%
  mutate(stars = case_when(`Pr(>Chisq)`<0.001 ~ "***",
                           `Pr(>Chisq)`<0.01 ~ "**",
                           `Pr(>Chisq)`<0.05 ~ "*",
                           `Pr(>Chisq)`>=0.05 ~ ""),
         estimate = paste0(round(Chisq,2),stars),
         model = 1:6,
         term = "Equality of coefficients") %>%
  select(term,model,estimate) %>%
  pivot_wider(names_from = model, values_from = estimate)

attr(add_rows, "position") <- c(7)

#produce summary table

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('disab_oth1' = 'Other disability',
        'disab_ment1' = 'Mental disability',
        'disab_comm1' = 'Communication disability',
        'disab_phys1' = 'Physical disability')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test", "adj.r.squared", "nobs"), coef_map = rev(cm), output = "table 5a.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001), add_rows = add_rows)

#estimated marginal means

marginal_means(protesttype3, variables = "disab_comm")

protesttwfetype3

(0.009254/0.137)

#severity

protestsev1 <- feols(protest~disab_sev|year_month, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

protestsev2 <- feols(protest~disab_sev+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

protestsev3 <- feols(protest~disab_sev+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

protesttwfesev1 <- feols(protest~disab_sev|pidp+year_month, data=us_l_panel, weights=us_l_panel$xw)

protesttwfesev2 <- feols(protest~disab_sev|pidp+year_month+gor, data=us_l_panel, weights=us_l_panel$xw)

protesttwfesev3 <- feols(protest~disab_sev+empstat4+log_m_inc+edu3|pidp+year_month+gor, data=us_l_panel, weights=us_l_panel$xw)

#produce joint equality of coefficients rows

modlist <- list(protestsev1,protestsev2,protestsev3,protesttwfesev1,protesttwfesev2,protesttwfesev3)

protestsev_lh_results <- lapply(modlist, function(model) {
  linearHypothesis(model, "disab_sev1 = disab_sev2")
})

add_rows <- as_tibble(bind_rows(protestsev_lh_results)) %>%
  drop_na() %>%
  mutate(stars = case_when(`Pr(>Chisq)`<0.001 ~ "***",
                           `Pr(>Chisq)`<0.01 ~ "**",
                           `Pr(>Chisq)`<0.05 ~ "*",
                           `Pr(>Chisq)`>=0.05 ~ ""),
         estimate = paste0(round(Chisq,2),stars),
         model = 1:6,
         term = "Equality of coefficients") %>%
  select(term,model,estimate) %>%
  pivot_wider(names_from = model, values_from = estimate)

attr(add_rows, "position") <- c(5)

#produce summary table

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('disab_sev2' = 'More than 1 ADL',
        'disab_sev1' = '1 ADL')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test", "adj.r.squared", "nobs"), coef_map = rev(cm), output = "table 5b.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001), add_rows = add_rows)

#estimated marginal means

marginal_means(protestsev3, variables = "disab_sev")

protesttwfesev3

(0.006842/0.105)

#duration

protestcont1 <- feols(protest~disab_cont|year_month, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

protestcont2 <- feols(protest~disab_cont+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

protestcont3 <- feols(protest~disab_cont+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

#produce joint equality of coefficients rows

modlist <- list(protestcont1,protestcont2,protestcont3)

protestcont_lh_results <- lapply(modlist, function(model) {
  linearHypothesis(model, "disab_cont1 = disab_cont2")
})

add_rows <- as_tibble(bind_rows(protestcont_lh_results)) %>%
  drop_na() %>%
  mutate(stars = case_when(`Pr(>Chisq)`<0.001 ~ "***",
                           `Pr(>Chisq)`<0.01 ~ "**",
                           `Pr(>Chisq)`<0.05 ~ "*",
                           `Pr(>Chisq)`>=0.05 ~ ""),
         estimate = paste0(round(Chisq,2),stars),
         model = 1:3,
         term = "Equality of coefficients") %>%
  select(term,model,estimate) %>%
  pivot_wider(names_from = model, values_from = estimate)

attr(add_rows, "position") <- c(5)

#produce summary table

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('disab_cont1' = 'Temporary disability',
        'disab_cont2' = 'Continuous disability')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test", "adj.r.squared", "nobs"), coef_map = cm, output = "table 5c.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001), add_rows = add_rows)

#####
#ROBUSTNESS

#WEIGHTS

#Unweighted

#Left-Right

fitstat(lr1_uw, type = "f")

lr1_uw <- feols(leftright~disabled|year_month, data=us_l_panel, cluster=us_l_panel$pidp)

lr2_uw <- feols(leftright~disabled+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel, cluster=us_l_panel$pidp)

lr3_uw <- feols(leftright~disabled+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor,
                data=us_l_panel, cluster=us_l_panel$pidp)

lr1_twfe_uw <- feols(leftright~disabled|year_month+pidp, data=us_l_panel, cluster=us_l_panel$pidp)

lr2_twfe_uw <- feols(leftright~disabled|year_month+gor+pidp, data=us_l_panel, cluster=us_l_panel$pidp)

lr3_twfe_uw <- feols(leftright~disabled+empstat4+log_m_inc+edu3|year_month+gor+pidp,
                     data=us_l_panel, cluster=us_l_panel$pidp)

etable(lr1,lr2,lr3,lrtwfe1,lrtwfe2,lrtwfe3)

etable(lr1_uw,lr2_uw,lr3_uw,lr1_twfe_uw,lr2_twfe_uw,lr3_twfe_uw)

#export

modlist <- list(lr1_uw,lr2_uw,lr3_uw,lr1_twfe_uw,lr2_twfe_uw,lr3_twfe_uw)

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('disabled1' = 'Disabled')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = rev(c("F-test","adj.r.squared","nobs")), coef_map = rev(cm), output = "Left-Right unweighted.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#Protest

fitstat(lr1_lw, type = "f")

protest1_uw <- feols(protest~disabled|year_month, data=us_l_panel, cluster=us_l_panel$pidp)

protest2_uw <- feols(protest~disabled+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel, cluster=us_l_panel$pidp)

protest3_uw <- feols(protest~disabled+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor,
                     data=us_l_panel, cluster=us_l_panel$pidp)

protest1_twfe_uw <- feols(protest~disabled|year_month+pidp, data=us_l_panel, cluster=us_l_panel$pidp)

protest2_twfe_uw <- feols(protest~disabled|year_month+gor+pidp, data=us_l_panel, cluster=us_l_panel$pidp)

protest3_twfe_uw <- feols(protest~disabled+empstat4+log_m_inc+edu3|year_month+gor+pidp,
                          data=us_l_panel, cluster=us_l_panel$pidp)

etable(protest1_uw,protest2_uw,protest3_uw,protest1_twfe_uw,protest2_twfe_uw,protest3_twfe_uw)

etable(protest1,protest2,protest3,protesttwfe1,protesttwfe2,protesttwfe3)

#export

modlist <- list(protest1_uw,protest2_uw,protest3_uw,protest1_twfe_uw,protest2_twfe_uw,protest3_twfe_uw)

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('disabled1' = 'Disabled')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test","adj.r.squared","nobs"), coef_map = rev(cm), output = "Protest unweighted.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#longitudinal weights

#Left-Right

fitstat(lr1_lw, type = "f")

lr1_lw <- feols(leftright~disabled|year_month, data=us_l_panel, weights=us_l_panel$lw, cluster=us_l_panel$pidp)

lr2_lw <- feols(leftright~disabled+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel, weights=us_l_panel$lw, cluster=us_l_panel$pidp)

lr3_lw <- feols(leftright~disabled+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor,
                data=us_l_panel, weights=us_l_panel$lw, cluster=us_l_panel$pidp)

lr1_twfe_lw <- feols(leftright~disabled|year_month+pidp, data=us_l_panel, weights=us_l_panel$lw, cluster=us_l_panel$pidp)

lr2_twfe_lw <- feols(leftright~disabled|year_month+gor+pidp, data=us_l_panel, weights=us_l_panel$lw, cluster=us_l_panel$pidp)

lr3_twfe_lw <- feols(leftright~disabled+empstat4+log_m_inc+edu3|year_month+gor+pidp,
                     data=us_l_panel, weights=us_l_panel$lw, cluster=us_l_panel$pidp)

etable(lr1,lr2,lr3,lrtwfe1,lrtwfe2,lrtwfe3)

etable(lr1_lw,lr2_lw,lr3_lw,lr1_twfe_lw,lr2_twfe_lw,lr3_twfe_lw)

#export

modlist <- list(lr1_lw,lr2_lw,lr3_lw,lr1_twfe_lw,lr2_twfe_lw,lr3_twfe_lw)

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('disabled1' = 'Disabled')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test","adj.r.squared","nobs"), coef_map = rev(cm), output = "table A10 - LR lw.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#Protest

protest1_lw <- feols(protest~disabled|year_month, data=us_l_panel, weights=us_l_panel$lw, cluster=us_l_panel$pidp)

protest2_lw <- feols(protest~disabled+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel, weights=us_l_panel$lw, cluster=us_l_panel$pidp)

protest3_lw <- feols(protest~disabled+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor,
                     data=us_l_panel, weights=us_l_panel$lw, cluster=us_l_panel$pidp)

protest1_twfe_lw <- feols(protest~disabled|year_month+pidp, data=us_l_panel, weights=us_l_panel$lw, cluster=us_l_panel$pidp)

protest2_twfe_lw <- feols(protest~disabled|year_month+gor+pidp, data=us_l_panel, weights=us_l_panel$lw, cluster=us_l_panel$pidp)

protest3_twfe_lw <- feols(protest~disabled+empstat4+log_m_inc+edu3|year_month+gor+pidp,
                          data=us_l_panel, weights=us_l_panel$lw, cluster=us_l_panel$pidp)

etable(protest1_lw,protest2_lw,protest3_lw,protest1_twfe_lw,protest2_twfe_lw,protest3_twfe_lw)

#export

modlist <- list(protest1_lw,protest2_lw,protest3_lw,protest1_twfe_lw,protest2_twfe_lw,protest3_twfe_lw)

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('disabled1' = 'Disabled')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test","adj.r.squared","nobs"), coef_map = rev(cm), output = "table A11 - protest lw.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#SAMPLE

#whole panel

us_l_red <- read_rds("us_l_red.rds")

us_l_panel_full <- us_l_red %>% filter(!is.na(vote_all))

#code variables as factors (again)

us_l_panel_full$disabled <- as_factor(us_l_panel_full$disabled)

us_l_panel_full$year_month <- as_factor(paste(us_l_panel_full$year,us_l_panel_full$month))

us_l_panel_full$sex <- as_factor(us_l_panel_full$sex)

us_l_panel_full$eth <- as_factor(us_l_panel_full$ethmin)

us_l_panel_full$empstat4 <- as_factor(us_l_panel_full$empstat4)

us_l_panel_full$edu3 <- as_factor(us_l_panel_full$edu3)

#Left-Right

lr1_full <- feols(leftright~disabled|year_month, data=us_l_panel_full, weights=us_l_panel_full$xw, cluster=us_l_panel_full$pidp)

lr2_full <- feols(leftright~disabled+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel_full, weights=us_l_panel_full$xw, cluster=us_l_panel_full$pidp)

lr3_full <- feols(leftright~disabled+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor, data=us_l_panel_full, weights=us_l_panel_full$xw, cluster=us_l_panel_full$pidp)

lrtwfe1_full <- feols(leftright~disabled|pidp+year_month, data=us_l_panel_full, weights=us_l_panel_full$xw)

lrtwfe2_full <- feols(leftright~disabled|pidp+year_month+gor, data=us_l_panel_full, weights=us_l_panel_full$xw)

lrtwfe3_full <- feols(leftright~disabled+empstat4+log_m_inc+edu3|pidp+year_month+gor, data=us_l_panel_full, weights=us_l_panel_full$xw)

modlist <- list(lr1_full,lr2_full,lr3_full,lrtwfe1_full,lrtwfe2_full,lrtwfe3_full)

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('disabled1' = 'Disabled')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test","adj.r.squared","nobs"), coef_map = rev(cm), output = "Left-Right - full panel.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#protest

protest_full1 <- feols(protest~disabled|year_month, data=us_l_panel_full, weights=us_l_panel_full$xw, cluster=us_l_panel_full$pidp)

protest_full2 <- feols(protest~disabled+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel_full, weights=us_l_panel_full$xw, cluster=us_l_panel_full$pidp)

protest_full3 <- feols(protest~disabled+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor, data=us_l_panel_full, weights=us_l_panel_full$xw, cluster=us_l_panel_full$pidp)

protest_fulltwfe1 <- feols(protest~disabled|pidp+year_month, data=us_l_panel_full, weights=us_l_panel_full$xw)

protest_fulltwfe2 <- feols(protest~disabled|pidp+year_month+gor, data=us_l_panel_full, weights=us_l_panel_full$xw)

protest_fulltwfe3 <- feols(protest~disabled+empstat4+log_m_inc+edu3|pidp+year_month+gor, data=us_l_panel_full, weights=us_l_panel_full$xw)

etable(protest_full1,protest_full2,protest_full3,protest_fulltwfe1,protest_fulltwfe2,protest_fulltwfe3)

modlist <- list(protest_full1,protest_full2,protest_full3,protest_fulltwfe1,protest_fulltwfe2,protest_fulltwfe3)

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('disabled1' = 'Disabled')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test", "adj.r.squared", "nobs"), coef_map = rev(cm), output = "Protest - full panel.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#working age sample

us_l_panel_wa <- read_rds("us_l_panel.rds")

us_l_panel_wa <- us_l_panel_wa %>% filter(age<66)

#code variables as factors (again)

us_l_panel_wa$disabled <- as_factor(us_l_panel_wa$disabled)

us_l_panel_wa$year_month <- as_factor(paste(us_l_panel_wa$year,us_l_panel_wa$month))

us_l_panel_wa$sex <- as_factor(us_l_panel_wa$sex)

us_l_panel_wa$eth <- as_factor(us_l_panel_wa$ethmin)

us_l_panel_wa$empstat4 <- as_factor(us_l_panel_wa$empstat4)

us_l_panel_wa$edu3 <- as_factor(us_l_panel_wa$edu3)

#Left-Right

lr1_wa <- feols(leftright~disabled|year_month, data=us_l_panel_wa, weights=us_l_panel_wa$xw, cluster=us_l_panel_wa$pidp)

lr2_wa <- feols(leftright~disabled+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel_wa, weights=us_l_panel_wa$xw, cluster=us_l_panel_wa$pidp)

lr3_wa <- feols(leftright~disabled+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor, data=us_l_panel_wa, weights=us_l_panel_wa$xw, cluster=us_l_panel_wa$pidp)

lrtwfe1_wa <- feols(leftright~disabled|pidp+year_month, data=us_l_panel_wa, weights=us_l_panel_wa$xw)

lrtwfe2_wa <- feols(leftright~disabled|pidp+year_month+gor, data=us_l_panel_wa, weights=us_l_panel_wa$xw)

lrtwfe3_wa <- feols(leftright~disabled+empstat4+log_m_inc+edu3|pidp+year_month+gor, data=us_l_panel_wa, weights=us_l_panel_wa$xw)

modlist <- list(lr1_wa,lr2_wa,lr3_wa,lrtwfe1_wa,lrtwfe2_wa,lrtwfe3_wa)

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('disabled1' = 'Disabled')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test","adj.r.squared","nobs"), coef_map = rev(cm), output = "Left-Right - working age.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#protest

protest_wa1 <- feols(protest~disabled|year_month, data=us_l_panel_wa, weights=us_l_panel_wa$xw, cluster=us_l_panel_wa$pidp)

protest_wa2 <- feols(protest~disabled+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel_wa, weights=us_l_panel_wa$xw, cluster=us_l_panel_wa$pidp)

protest_wa3 <- feols(protest~disabled+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor, data=us_l_panel_wa, weights=us_l_panel_wa$xw, cluster=us_l_panel_wa$pidp)

protest_watwfe1 <- feols(protest~disabled|pidp+year_month, data=us_l_panel_wa, weights=us_l_panel_wa$xw)

protest_watwfe2 <- feols(protest~disabled|pidp+year_month+gor, data=us_l_panel_wa, weights=us_l_panel_wa$xw)

protest_watwfe3 <- feols(protest~disabled+empstat4+log_m_inc+edu3|pidp+year_month+gor, data=us_l_panel_wa, weights=us_l_panel_wa$xw)

etable(protest_wa1,protest_wa2,protest_wa3,protest_watwfe1,protest_watwfe2,protest_watwfe3)

modlist <- list(protest_wa1,protest_wa2,protest_wa3,protest_watwfe1,protest_watwfe2,protest_watwfe3)

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('disabled1' = 'Disabled')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test", "adj.r.squared", "nobs"), coef_map = rev(cm), output = "Protest - working age.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#including Scotland

us_l <- read_rds("us_l.rds")

#arrange dataset

us_l_red <- us_l %>%
  arrange(pidp,wave)

#drop under 18s

us_l_red <- us_l_red %>%
  filter(!age<18)

#remove NI

table(us_l_red$country)

us_l_ews <- us_l_red %>% filter(country!=4)

#code variables as factors (again)

us_l_ews$disabled <- as_factor(us_l_ews$disabled)

us_l_ews$year_month <- as_factor(paste(us_l_ews$year,us_l_ews$month))

us_l_ews$sex <- as_factor(us_l_ews$sex)

us_l_ews$eth <- as_factor(us_l_ews$ethmin)

us_l_ews$empstat4 <- as_factor(us_l_ews$empstat4)

us_l_ews$edu3 <- as_factor(us_l_ews$edu3)

#filter to waves 1-7 and non-missing of model variables

us_l_ews <- us_l_ews %>% filter(wave<8) %>%
  drop_na(disabled,age,year_month,sex,eth,gor,empstat4,log_m_inc,edu3,vote_all)

table(us_l_ews$country,us_l_ews$vote_all)

#Left-Right

lr1_ews <- feols(leftright~disabled|year_month, data=us_l_ews, weights=us_l_ews$xw, cluster=us_l_ews$pidp)

lr2_ews <- feols(leftright~disabled+age+I(age^2)+sex+eth|year_month+gor, data=us_l_ews, weights=us_l_ews$xw, cluster=us_l_ews$pidp)

lr3_ews <- feols(leftright~disabled+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor, data=us_l_ews, weights=us_l_ews$xw, cluster=us_l_ews$pidp)

lrtwfe1_ews <- feols(leftright~disabled|pidp+year_month, data=us_l_ews, weights=us_l_ews$xw)

lrtwfe2_ews <- feols(leftright~disabled|pidp+year_month+gor, data=us_l_ews, weights=us_l_ews$xw)

lrtwfe3_ews <- feols(leftright~disabled+empstat4+log_m_inc+edu3|pidp+year_month+gor, data=us_l_ews, weights=us_l_ews$xw)

modlist <- list(lr1_ews,lr2_ews,lr3_ews,lrtwfe1_ews,lrtwfe2_ews,lrtwfe3_ews)

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('disabled1' = 'Disabled')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test","adj.r.squared","nobs"), coef_map = rev(cm), output = "Left-Right - inc Scotland.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#protest

protest_ews1 <- feols(protest~disabled|year_month, data=us_l_ews, weights=us_l_ews$xw, cluster=us_l_ews$pidp)

protest_ews2 <- feols(protest~disabled+age+I(age^2)+sex+eth|year_month+gor, data=us_l_ews, weights=us_l_ews$xw, cluster=us_l_ews$pidp)

protest_ews3 <- feols(protest~disabled+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor, data=us_l_ews, weights=us_l_ews$xw, cluster=us_l_ews$pidp)

protest_ewstwfe1 <- feols(protest~disabled|pidp+year_month, data=us_l_ews, weights=us_l_ews$xw)

protest_ewstwfe2 <- feols(protest~disabled|pidp+year_month+gor, data=us_l_ews, weights=us_l_ews$xw)

protest_ewstwfe3 <- feols(protest~disabled+empstat4+log_m_inc+edu3|pidp+year_month+gor, data=us_l_ews, weights=us_l_ews$xw)

etable(protest_ews1,protest_ews2,protest_ews3,protest_ewstwfe1,protest_ewstwfe2,protest_ewstwfe3)

modlist <- list(protest_ews1,protest_ews2,protest_ews3,protest_ewstwfe1,protest_ewstwfe2,protest_ewstwfe3)

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('disabled1' = 'Disabled')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test", "adj.r.squared", "nobs"), coef_map = rev(cm), output = "Protest - inc Scotland.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#testing Left-Right in first wave in case of anti-incumbent voting

us_l_panel_labgov <- us_l_panel %>% filter(date < "2010-05-12")

lr1w1 <- feols(leftright~disabled|year_month, data=us_l_panel_labgov, weights=us_l_panel_labgov$xw, cluster=us_l_panel_labgov$pidp)

fitstat(lr1w1, type = "f")

lr2w1 <- feols(leftright~disabled+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel_labgov, weights=us_l_panel_labgov$xw, cluster=us_l_panel_labgov$pidp)

lr3w1 <- feols(leftright~disabled+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor,
               data=us_l_panel_labgov, weights=us_l_panel_labgov$xw, cluster=us_l_panel_labgov$pidp)

table(us_l_panel_labgov$pidp)>1

table(us_l_panel_labgov$disab_long)

#ESTIMATOR

#probit model using fixest

#Left-Right

lr1_probit_fe <- fixest::feglm(leftright~disabled|year_month, data=us_l_panel, family = quasibinomial(link = "probit"),  weights=us_l_panel$xw, cluster=us_l_panel$pidp)

lr2_probit_fe <- fixest::feglm(leftright~disabled+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel, family = quasibinomial(link = "probit"), weights=us_l_panel$xw, cluster=us_l_panel$pidp)

lr3_probit_fe <- fixest::feglm(leftright~disabled+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor, data=us_l_panel, family = quasibinomial(link = "probit"), weights=us_l_panel$xw, cluster=us_l_panel$pidp)

etable(lr1_probit_fe,lr2_probit_fe,lr3_probit_fe)

cm <- c('disabled' = 'Disabled')

modlist <- list(avg_slopes(lr1_probit_fe),avg_slopes(lr2_probit_fe, variables = "disabled"),avg_slopes(lr3_probit_fe, variables = "disabled"))

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("adj.r.squared","nobs"), output = "Left-Right probit estimates.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

glance(lr1_probit_fe)

modlist <- list(lr1_probit,lr2_probit,lr3_probit)

cm <- c('disabled1' = 'Disabled')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test","adj.r.squared","nobs"), coef_map = rev(cm), output = "Left-Right probit gof.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#protest

protest1_probit <- fixest::feglm(protest~disabled|year_month, data=us_l_panel, weights=us_l_panel$xw, family = quasibinomial(link = "probit"), cluster=us_l_panel$pidp)

protest2_probit <- fixest::feglm(protest~disabled+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel, family = quasibinomial(link = "probit"), weights=us_l_panel$xw, cluster=us_l_panel$pidp)

protest3_probit <- fixest::feglm(protest~disabled+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor, data=us_l_panel, family = quasibinomial(link = "probit"), weights=us_l_panel$xw, cluster=us_l_panel$pidp)

etable(protest1_probit,protest2_probit,protest3_probit)

fitstat(protest1_probit, type = "f")

cm <- c('disabled' = 'Disabled')

modlist <- list(avg_slopes(protest1_probit),avg_slopes(protest2_probit, variables = "disabled"),avg_slopes(protest3_probit, variables = "disabled"))

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test","adj.r.squared","nobs"), coef_map = rev(cm), output = "Protest probit estimates.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

modlist <- list(protest1_probit,protest2_probit,protest3_probit)

cm <- c('disabled1' = 'Disabled')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test", "adj.r.squared", "nobs"), coef_map = rev(cm), output = "Protest probit gof.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#DISABILITY MEASURES

#lagged disability

#left-right

lr1_lag_disab <- feols(leftright~disab_lag|year_month, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

lr2_lag_disab <- feols(leftright~disab_lag+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

lr3_lag_disab <- feols(leftright~disab_lag+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

lrtwfe1_lag_disab <- feols(leftright~disab_lag|pidp+year_month, data=us_l_panel, weights=us_l_panel$xw)

lrtwfe2_lag_disab <- feols(leftright~disab_lag|pidp+year_month+gor, data=us_l_panel, weights=us_l_panel$xw)

lrtwfe3_lag_disab <- feols(leftright~disab_lag+empstat4+log_m_inc+edu3|pidp+year_month+gor, data=us_l_panel, weights=us_l_panel$xw)

#output

modlist <- list(lr1_lag_disab,lr2_lag_disab,lr3_lag_disab,lrtwfe1_lag_disab,lrtwfe2_lag_disab,lrtwfe3_lag_disab)

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('disab_lag1' = 'Disabled (t-1)')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test","adj.r.squared","nobs"), coef_map = rev(cm), output = "Lagged disability - Left-Right.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#protest

protest1_lag_disab <- feols(protest~disab_lag|year_month, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

protest2_lag_disab <- feols(protest~disab_lag+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

protest3_lag_disab <- feols(protest~disab_lag+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

protesttwfe1_lag_disab <- feols(protest~disab_lag|pidp+year_month, data=us_l_panel, weights=us_l_panel$xw)

protesttwfe2_lag_disab <- feols(protest~disab_lag|pidp+year_month+gor, data=us_l_panel, weights=us_l_panel$xw)

protesttwfe3_lag_disab <- feols(protest~disab_lag+empstat4+log_m_inc+edu3|pidp+year_month+gor, data=us_l_panel, weights=us_l_panel$xw)

#output

modlist <- list(protest1_lag_disab,protest2_lag_disab,protest3_lag_disab,protesttwfe1_lag_disab,protesttwfe2_lag_disab,protesttwfe3_lag_disab)

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('disab_lag1' = 'Disabled (t-1)')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test","adj.r.squared","nobs"), coef_map = rev(cm), output = "Lagged disability - Protest.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#two-period disability

#left-right

lr1_disab_two <- feols(leftright~disab_two|year_month, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

lr2_disab_two <- feols(leftright~disab_two+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

lr3_disab_two <- feols(leftright~disab_two+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

lrtwfe1_disab_two <- feols(leftright~disab_two|pidp+year_month, data=us_l_panel, weights=us_l_panel$xw)

lrtwfe2_disab_two <- feols(leftright~disab_two|pidp+year_month+gor, data=us_l_panel, weights=us_l_panel$xw)

lrtwfe3_disab_two <- feols(leftright~disab_two+empstat4+log_m_inc+edu3|pidp+year_month+gor, data=us_l_panel, weights=us_l_panel$xw)

#output

modlist <- list(lr1_disab_two,lr2_disab_two,lr3_disab_two,lrtwfe1_disab_two,lrtwfe2_disab_two,lrtwfe3_disab_two)

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('disab_two1' = 'Two-period disability')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test","adj.r.squared","nobs"), coef_map = rev(cm), output = "Two-period disability - Left-Right.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#protest

protest1_disab_two <- feols(protest~disab_two|year_month, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

protest2_disab_two <- feols(protest~disab_two+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

protest3_disab_two <- feols(protest~disab_two+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

protesttwfe1_disab_two <- feols(protest~disab_two|pidp+year_month, data=us_l_panel, weights=us_l_panel$xw)

protesttwfe2_disab_two <- feols(protest~disab_two|pidp+year_month+gor, data=us_l_panel, weights=us_l_panel$xw)

protesttwfe3_disab_two <- feols(protest~disab_two+empstat4+log_m_inc+edu3|pidp+year_month+gor, data=us_l_panel, weights=us_l_panel$xw)

#output

modlist <- list(protest1_disab_two,protest2_disab_two,protest3_disab_two,protesttwfe1_disab_two,protesttwfe2_disab_two,protesttwfe3_disab_two)

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('disab_two1' = 'Two-period disability')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test","adj.r.squared","nobs"), coef_map = rev(cm), output = "Two-period disability - Protest.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#longitudinal

table(us_l_panel$disab_long)

lrlong1 <- feols(leftright~disab_long|year_month, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

lrlong2 <- feols(leftright~disab_long+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

lrlong3 <- feols(leftright~disab_long+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

lrlong1_twfe <- feols(leftright~disab_long|pidp+year_month, data=us_l_panel, weights=us_l_panel$xw)

lrlong2_twfe <- feols(leftright~disab_long|pidp+year_month+gor, data=us_l_panel, weights=us_l_panel$xw)

lrlong3_twfe <- feols(leftright~disab_long+empstat4+log_m_inc+edu3|pidp+year_month+gor, data=us_l_panel, weights=us_l_panel$xw)

modlist <- list(lrlong1,lrlong2,lrlong3,lrlong1_twfe,lrlong2_twfe,lrlong3_twfe)

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('disab_long3' = 'Permanent disability',
        'disab_long2' = 'Transition out of disability',
        'disab_long1' = 'Transition into disability')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test", "adj.r.squared", "nobs"), coef_map = rev(cm), output = "LR longitudinal.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

protestlong1 <- feols(protest~disab_long|year_month, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

protestlong2 <- feols(protest~disab_long+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

protestlong3 <- feols(protest~disab_long+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

protestlong1_twfe <- feols(protest~disab_long|pidp+year_month, data=us_l_panel, weights=us_l_panel$xw)

protestlong2_twfe <- feols(protest~disab_long|pidp+year_month+gor, data=us_l_panel, weights=us_l_panel$xw)

protestlong3_twfe <- feols(protest~disab_long+empstat4+log_m_inc+edu3|pidp+year_month+gor, data=us_l_panel, weights=us_l_panel$xw)

modlist <- list(protestlong1,protestlong2,protestlong3,protestlong1_twfe,protestlong2_twfe,protestlong3_twfe)

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test", "adj.r.squared", "nobs"), coef_map = rev(cm), output = "Protest longitudinal.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#onset and exit separated

#removing those who have a disability spell prior to an onset

us_l_panel %>%
  group_by(pidp) %>%
  filter(1 %in% disabled) %>%
  dplyr::select(pidp,wave,disabled,disab_long,disab_past) %>%
  print(n=100)

us_l_panel %>%
  mutate(disab_filter = disab_long == 1 & dplyr::lag(disab_past) == 1) %>%
  group_by(pidp) %>%
  filter(1 %in% disabled) %>%
  dplyr::select(pidp,wave,disabled,disab_long,disab_past,disab_filter) %>%
  print(n=100)

us_l_panel_prior <- us_l_panel %>%
  mutate(disab_filter = disab_long == 1 & dplyr::lag(disab_past) == 1) %>%
  group_by(pidp) %>%
  filter(!T %in% disab_filter)

#original models

lr_prior1 <- feols(leftright~disabled|year_month, data=us_l_panel_prior, weights=us_l_panel_prior$xw, cluster=us_l_panel_prior$pidp)

lr_prior2 <- feols(leftright~disabled+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel_prior, weights=us_l_panel_prior$xw, cluster=us_l_panel_prior$pidp)

lr_prior3 <- feols(leftright~disabled+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor, data=us_l_panel_prior, weights=us_l_panel_prior$xw, cluster=us_l_panel_prior$pidp)

lr_prior1_twfe <- feols(leftright~disabled|pidp+year_month, data=us_l_panel_prior, weights=us_l_panel_prior$xw)

lr_prior2_twfe <- feols(leftright~disabled|pidp+year_month+gor, data=us_l_panel_prior, weights=us_l_panel_prior$xw)

lr_prior3_twfe <- feols(leftright~disabled+empstat4+log_m_inc+edu3|pidp+year_month+gor, data=us_l_panel_prior, weights=us_l_panel_prior$xw)

etable(lr_prior1,lr_prior2,lr_prior3,lr_prior1_twfe,lr_prior2_twfe,lr_prior3_twfe)

modlist <- list(lr_prior1,lr_prior2,lr_prior3,lr_prior1_twfe,lr_prior2_twfe,lr_prior3_twfe)

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('disabled1' = 'Disability')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test", "adj.r.squared", "nobs"), coef_map = rev(cm), output = "LR - new onset.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

protest_prior1 <- feols(protest~disabled|year_month, data=us_l_panel_prior, weights=us_l_panel_prior$xw, cluster=us_l_panel_prior$pidp)

protest_prior2 <- feols(protest~disabled+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel_prior, weights=us_l_panel_prior$xw, cluster=us_l_panel_prior$pidp)

protest_prior3 <- feols(protest~disabled+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor, data=us_l_panel_prior, weights=us_l_panel_prior$xw, cluster=us_l_panel_prior$pidp)

protest_prior1_twfe <- feols(protest~disabled|pidp+year_month, data=us_l_panel_prior, weights=us_l_panel_prior$xw)

protest_prior2_twfe <- feols(protest~disabled|pidp+year_month+gor, data=us_l_panel_prior, weights=us_l_panel_prior$xw)

protest_prior3_twfe <- feols(protest~disabled+empstat4+log_m_inc+edu3|pidp+year_month+gor, data=us_l_panel_prior, weights=us_l_panel_prior$xw)

modlist <- list(protest_prior1,protest_prior2,protest_prior3,protest_prior1_twfe,protest_prior2_twfe,protest_prior3_twfe)

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test", "adj.r.squared", "nobs"), coef_map = rev(cm), output = "Protest - new onset.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#OUTCOME MEASURES

#party support NA as 0

us_l_red <- read_rds("us_l_red.rds")

us_l_panel_noNA <- us_l_red %>% filter(wave<8)

#code variables as factors (again)

us_l_panel_noNA$disabled <- as_factor(us_l_panel_noNA$disabled)

us_l_panel_noNA$year_month <- as_factor(paste(us_l_panel_noNA$year,us_l_panel_noNA$month))

us_l_panel_noNA$sex <- as_factor(us_l_panel_noNA$sex)

us_l_panel_noNA$eth <- as_factor(us_l_panel_noNA$ethmin)

us_l_panel_noNA$empstat4 <- as_factor(us_l_panel_noNA$empstat4)

us_l_panel_noNA$edu3 <- as_factor(us_l_panel_noNA$edu3)

#Left-Right

lr1_noNA <- feols(leftright_noNA~disabled|year_month, data=us_l_panel_noNA, weights=us_l_panel_noNA$xw, cluster=us_l_panel_noNA$pidp)

lr2_noNA <- feols(leftright_noNA~disabled+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel_noNA, weights=us_l_panel_noNA$xw, cluster=us_l_panel_noNA$pidp)

lr3_noNA <- feols(leftright_noNA~disabled+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor, data=us_l_panel_noNA, weights=us_l_panel_noNA$xw, cluster=us_l_panel_noNA$pidp)

lrtwfe1_noNA <- feols(leftright_noNA~disabled|pidp+year_month, data=us_l_panel_noNA, weights=us_l_panel_noNA$xw)

lrtwfe2_noNA <- feols(leftright_noNA~disabled|pidp+year_month+gor, data=us_l_panel_noNA, weights=us_l_panel_noNA$xw)

lrtwfe3_noNA <- feols(leftright_noNA~disabled+empstat4+log_m_inc+edu3|pidp+year_month+gor, data=us_l_panel_noNA, weights=us_l_panel_noNA$xw)

modlist <- list(lr1_noNA,lr2_noNA,lr3_noNA,lrtwfe1_noNA,lrtwfe2_noNA,lrtwfe3_noNA)

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('disabled1' = 'Disabled')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test","adj.r.squared","nobs"), coef_map = rev(cm), output = "Left-Right no NA.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#protest

protest_noNA1 <- feols(protest_noNA~disabled|year_month, data=us_l_panel_noNA, weights=us_l_panel_noNA$xw, cluster=us_l_panel_noNA$pidp)

protest_noNA2 <- feols(protest_noNA~disabled+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel_noNA, weights=us_l_panel_noNA$xw, cluster=us_l_panel_noNA$pidp)

protest_noNA3 <- feols(protest_noNA~disabled+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor, data=us_l_panel_noNA, weights=us_l_panel_noNA$xw, cluster=us_l_panel_noNA$pidp)

protest_noNAtwfe1 <- feols(protest_noNA~disabled|pidp+year_month, data=us_l_panel_noNA, weights=us_l_panel_noNA$xw)

protest_noNAtwfe2 <- feols(protest_noNA~disabled|pidp+year_month+gor, data=us_l_panel_noNA, weights=us_l_panel_noNA$xw)

protest_noNAtwfe3 <- feols(protest_noNA~disabled+empstat4+log_m_inc+edu3|pidp+year_month+gor, data=us_l_panel_noNA, weights=us_l_panel_noNA$xw)

etable(protest_noNA1,protest_noNA2,protest_noNA3,protest_noNAtwfe1,protest_noNAtwfe2,protest_noNAtwfe3)

modlist <- list(protest_noNA1,protest_noNA2,protest_noNA3,protest_noNAtwfe1,protest_noNAtwfe2,protest_noNAtwfe3)

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('disabled1' = 'Disabled')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test", "adj.r.squared", "nobs"), coef_map = rev(cm), output = "Protest no NA.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#no vote coded as protest

us_l_panel_noNA$NA_as_protest

NA_as_protest1 <- feols(NA_as_protest~disabled|year_month, data=us_l_panel_noNA, weights=us_l_panel_noNA$xw, cluster=us_l_panel_noNA$pidp)

NA_as_protest2 <- feols(NA_as_protest~disabled+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel_noNA, weights=us_l_panel_noNA$xw, cluster=us_l_panel_noNA$pidp)

NA_as_protest3 <- feols(NA_as_protest~disabled+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor, data=us_l_panel_noNA, weights=us_l_panel_noNA$xw, cluster=us_l_panel_noNA$pidp)

NA_as_protesttwfe1 <- feols(NA_as_protest~disabled|pidp+year_month, data=us_l_panel_noNA, weights=us_l_panel_noNA$xw)

NA_as_protesttwfe2 <- feols(NA_as_protest~disabled|pidp+year_month+gor, data=us_l_panel_noNA, weights=us_l_panel_noNA$xw)

NA_as_protesttwfe3 <- feols(NA_as_protest~disabled+empstat4+log_m_inc+edu3|pidp+year_month+gor, data=us_l_panel_noNA, weights=us_l_panel_noNA$xw)

etable(NA_as_protest1,NA_as_protest2,NA_as_protest3,NA_as_protesttwfe1,NA_as_protesttwfe2,NA_as_protesttwfe3)

modlist <- list(NA_as_protest1,NA_as_protest2,NA_as_protest3,NA_as_protesttwfe1,NA_as_protesttwfe2,NA_as_protesttwfe3)

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('disabled1' = 'Disabled')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test", "adj.r.squared", "nobs"), coef_map = rev(cm), output = "Protest as NA.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#coding UKIP as right-wing

us_l_panel$leftright_UKIP <- case_when(us_l_panel$vote4==1 | us_l_panel$vote3==1 ~ 1,
                                       us_l_panel$vote4==2 | us_l_panel$vote3==2 ~ 0,
                                       us_l_panel$vote4==3 | us_l_panel$vote3==3 ~ 0,
                                       us_l_panel$vote4==4 | us_l_panel$vote3==4 ~ 0,
                                       us_l_panel$vote4==5 | us_l_panel$vote3==5 ~ 0,
                                       us_l_panel$vote4==6 | us_l_panel$vote3==6 ~ 0,
                                       us_l_panel$vote4==7 | us_l_panel$vote3==7 ~ 0,
                                       us_l_panel$vote4==8 | us_l_panel$vote3==8 ~ 0,
                                       us_l_panel$vote4==9 | us_l_panel$vote3==9 ~ 0,
                                       us_l_panel$vote4==10 | us_l_panel$vote3==10 ~ 0,
                                       us_l_panel$vote4==11 | us_l_panel$vote3==11 ~ 0,
                                       us_l_panel$vote4==12 | us_l_panel$vote3==12 ~ 1,
                                       us_l_panel$vote4==13 | us_l_panel$vote3==13 ~ 0,
                                       us_l_panel$vote4==14 | us_l_panel$vote3==14 ~ 0,
                                       us_l_panel$vote4==15 | us_l_panel$vote3==15 ~ 0,
                                       us_l_panel$vote4==97 | us_l_panel$vote3==97 ~ 0)

table(us_l_panel$leftright_UKIP,us_l_panel$leftright)

lr1_ukip <- feols(leftright_UKIP~disabled|year_month, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

lr2_ukip <- feols(leftright_UKIP~disabled+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

lr3_ukip <- feols(leftright_UKIP~disabled+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor,
                  data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

lr1_twfe_ukip <- feols(leftright_UKIP~disabled|year_month+pidp, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

lr2_twfe_ukip <- feols(leftright_UKIP~disabled|year_month+gor+pidp, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

lr3_twfe_ukip <- feols(leftright_UKIP~disabled+empstat4+log_m_inc+edu3|year_month+gor+pidp,
                       data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

#export

modlist <- list(lr1_ukip,lr2_ukip,lr3_ukip,lr1_twfe_ukip,lr2_twfe_ukip,lr3_twfe_ukip)

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('disabled1' = 'Disabled')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test","adj.r.squared","nobs"), coef_map = rev(cm), output = "LR with UKIP.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#estimating just Con vs Lab

convslab1 <- feols(convslab~disabled|year_month, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

convslab2 <- feols(convslab~disabled+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

convslab3 <- feols(convslab~disabled+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor,
                   data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

convslab1_twfe <- feols(convslab~disabled|year_month+pidp, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

convslab2_twfe <- feols(convslab~disabled|year_month+gor+pidp, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

convslab3_twfe <- feols(convslab~disabled+empstat4+log_m_inc+edu3|year_month+gor+pidp,
                        data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

#export

modlist <- list(convslab1,convslab2,convslab3,convslab1_twfe,convslab2_twfe,convslab3_twfe)

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('disabled1' = 'Disabled')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test","adj.r.squared","nobs"), coef_map = rev(cm), output = "Con vs Lab.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#testing protest including Plaid Cymru

us_l_panel$protest_PC <- case_when(us_l_panel$vote4==5 | us_l_panel$vote3==5 ~ 1,
                                   us_l_panel$vote4==6 | us_l_panel$vote3==6 ~ 1,
                                   us_l_panel$vote4==12 | us_l_panel$vote3==12 ~ 1,
                                   us_l_panel$vote4==13 | us_l_panel$vote3==13 ~ 1,
                                   us_l_panel$vote4==97 | us_l_panel$vote3==97 ~ 1,
                                   us_l_panel$vote4==1 | us_l_panel$vote3==1 ~ 0,
                                   us_l_panel$vote4==2 | us_l_panel$vote3==2 ~ 0,
                                   us_l_panel$vote4==3 | us_l_panel$vote3==3 ~ 0,
                                   us_l_panel$vote4==4 | us_l_panel$vote3==4 ~ 0,
                                   us_l_panel$vote4==7 | us_l_panel$vote3==7 ~ 0,
                                   us_l_panel$vote4==8 | us_l_panel$vote3==8 ~ 0,
                                   us_l_panel$vote4==9 | us_l_panel$vote3==9 ~ 0,
                                   us_l_panel$vote4==10 | us_l_panel$vote3==10 ~ 0,
                                   us_l_panel$vote4==11 | us_l_panel$vote3==11 ~ 0,
                                   us_l_panel$vote4==14 | us_l_panel$vote3==14 ~ 0,
                                   us_l_panel$vote4==15 | us_l_panel$vote3==15 ~ 0)

table(us_l_panel$protest_PC,us_l_panel$protest)

protest_PC1 <- feols(protest_PC~disabled|year_month, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

protest_PC2 <- feols(protest_PC~disabled+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

protest_PC3 <- feols(protest_PC~disabled+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor,
                     data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

protest_PC1_twfe <- feols(protest_PC~disabled|year_month+pidp, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

protest_PC2_twfe <- feols(protest_PC~disabled|year_month+gor+pidp, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

protest_PC3_twfe <- feols(protest_PC~disabled+empstat4+log_m_inc+edu3|year_month+gor+pidp,
                          data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

#export

modlist <- list(protest_PC1,protest_PC2,protest_PC3,protest_PC1_twfe,protest_PC2_twfe,protest_PC3_twfe)

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('edu33' = 'Quals: Degree or higher',
        'edu32' = 'Quals: Below degree',
        'edu31' = 'Quals: None (ref)',
        'log_m_inc' = 'Monthly income (log)',
        'empstat44' = 'Economically inactive',
        'empstat43' = 'Retired',
        'empstat42' = 'Unemployed',
        'empstat41' = 'Employed (ref)',
        'eth1' = 'Non-white British',
        'sex2' = 'Female',
        'I(age^2)' = 'Age^2',
        'age' = 'Age',
        'disabled1' = 'Disabled')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test","adj.r.squared","nobs"), coef_map = rev(cm), output = "Protest with Plaid.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))


#predicting outcomes while controlling for lagged outcome

#left-right

lr1_lag_outcome <- feols(leftright~disabled+leftright_lag|year_month, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

lr2_lag_outcome <- feols(leftright~disabled+leftright_lag+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

lr3_lag_outcome <- feols(leftright~disabled+leftright_lag+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

lrtwfe1_lag_outcome <- feols(leftright~disabled+leftright_lag|pidp+year_month, data=us_l_panel, weights=us_l_panel$xw)

lrtwfe2_lag_outcome <- feols(leftright~disabled+leftright_lag|pidp+year_month+gor, data=us_l_panel, weights=us_l_panel$xw)

lrtwfe3_lag_outcome <- feols(leftright~disabled+leftright_lag+empstat4+log_m_inc+edu3|pidp+year_month+gor, data=us_l_panel, weights=us_l_panel$xw)

#output

modlist <- list(lr1_lag_outcome,lr2_lag_outcome,lr3_lag_outcome,lrtwfe1_lag_outcome,lrtwfe2_lag_outcome,lrtwfe3_lag_outcome)

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('disabled1' = 'Disabled')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test","adj.r.squared","nobs"), coef_map = rev(cm), output = "Controlling for lagged outcome - Left-Right.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#protest

protest1_lag_outcome <- feols(protest~disabled+protest_lag|year_month, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

protest2_lag_outcome <- feols(protest~disabled+protest_lag+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

protest3_lag_outcome <- feols(protest~disabled+protest_lag+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

protesttwfe1_lag_outcome <- feols(protest~disabled+protest_lag|pidp+year_month, data=us_l_panel, weights=us_l_panel$xw)

protesttwfe2_lag_outcome <- feols(protest~disabled+protest_lag|pidp+year_month+gor, data=us_l_panel, weights=us_l_panel$xw)

protesttwfe3_lag_outcome <- feols(protest~disabled+protest_lag+empstat4+log_m_inc+edu3|pidp+year_month+gor, data=us_l_panel, weights=us_l_panel$xw)

#output

modlist <- list(protest1_lag_outcome,protest2_lag_outcome,protest3_lag_outcome,protesttwfe1_lag_outcome,protesttwfe2_lag_outcome,protesttwfe3_lag_outcome)

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('disabled1' = 'Disabled')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test","adj.r.squared","nobs"), coef_map = rev(cm), output = "Controlling for lagged outcome - protest.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#COVARIATES

#subjective measures of financial distress

us_l_panel_fin <- us_l_panel %>% drop_na(finnow,finfut)

#remember to recode finfut

us_l_panel_fin %>% select(starts_with("fin")) %>% summary()

us_l_panel_fin <- us_l_panel_fin %>% mutate(finfut = case_when(finfut == 3 ~ 2,
                                                               finfut == 2 ~ 3,
                                                               finfut == 1 ~ 1))

lr3_fin <- feols(leftright~disabled+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor, data=us_l_panel_fin, weights=us_l_panel_fin$xw, cluster=us_l_panel_fin$pidp)

lr_finnow <- feols(leftright~disabled+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3+finnow|year_month+gor, data=us_l_panel_fin, weights=us_l_panel_fin$xw, cluster=us_l_panel_fin$pidp)

lr_finfut <- feols(leftright~disabled+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3+finfut|year_month+gor, data=us_l_panel_fin, weights=us_l_panel_fin$xw, cluster=us_l_panel_fin$pidp)

lr_finboth <- feols(leftright~disabled+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3+finnow+finfut|year_month+gor, data=us_l_panel_fin, weights=us_l_panel_fin$xw, cluster=us_l_panel_fin$pidp)

etable(lr3_fin,lr_finnow,lr_finfut,lr_finboth)

modlist <- list(lr3_fin,lr_finnow,lr_finfut,lr_finboth)

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('edu33' = 'Quals: Degree or higher',
        'edu32' = 'Quals: Below degree',
        'edu31' = 'Quals: None (ref)',
        'log_m_inc' = 'Monthly income (log)',
        'empstat44' = 'Economically inactive',
        'empstat43' = 'Retired',
        'empstat42' = 'Unemployed',
        'empstat41' = 'Employed (ref)',
        'eth1' = 'Non-white British',
        'sex2' = 'Female',
        'I(age^2)' = 'Age^2',
        'age' = 'Age',
        'finnow' = 'Current finances',
        'finfut' = 'Future finances',
        'disabled1' = 'Disabled')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test", "adj.r.squared", "nobs"), coef_map = rev(cm), output = "LR subj finance.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#actual output

lr1_fin <- feols(leftright~disabled+finnow+finfut|year_month, data=us_l_panel_fin, weights=us_l_panel_fin$xw, cluster=us_l_panel_fin$pidp)

lr2_fin <- feols(leftright~disabled+finnow+finfut+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel_fin, weights=us_l_panel_fin$xw, cluster=us_l_panel_fin$pidp)

lr3_fin <- feols(leftright~disabled+finnow+finfut+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor, data=us_l_panel_fin, weights=us_l_panel_fin$xw, cluster=us_l_panel_fin$pidp)

lr1_fin_twfe <- feols(leftright~disabled+finnow+finfut|year_month+pidp, data=us_l_panel_fin, weights=us_l_panel_fin$xw, cluster=us_l_panel_fin$pidp)

lr2_fin_twfe <- feols(leftright~disabled+finnow+finfut|year_month+gor+pidp, data=us_l_panel_fin, weights=us_l_panel_fin$xw, cluster=us_l_panel_fin$pidp)

lr3_fin_twfe <- feols(leftright~disabled+finnow+finfut+empstat4+log_m_inc+edu3|year_month+gor+pidp,
                      data=us_l_panel_fin, weights=us_l_panel_fin$xw, cluster=us_l_panel_fin$pidp)

etable(lr1_fin,lr2_fin,lr3_fin,lr1_fin_twfe,lr2_fin_twfe,lr3_fin_twfe)

modlist <- list(lr1_fin,lr2_fin,lr3_fin,lr1_fin_twfe,lr2_fin_twfe,lr3_fin_twfe)

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('finnow' = 'Current finances',
        'finfut' = 'Future finances',
        'disabled1' = 'Disabled')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test", "adj.r.squared", "nobs"), coef_map = rev(cm), output = "LR subj finance.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#actual output

protest1_fin <- feols(protest~disabled+finnow+finfut|year_month, data=us_l_panel_fin, weights=us_l_panel_fin$xw, cluster=us_l_panel_fin$pidp)

protest2_fin <- feols(protest~disabled+finnow+finfut+age+I(age^2)+sex+eth|year_month+gor, data=us_l_panel_fin, weights=us_l_panel_fin$xw, cluster=us_l_panel_fin$pidp)

protest3_fin <- feols(protest~disabled+finnow+finfut+age+I(age^2)+sex+eth+empstat4+log_m_inc+edu3|year_month+gor, data=us_l_panel_fin, weights=us_l_panel_fin$xw, cluster=us_l_panel_fin$pidp)

protest1_fin_twfe <- feols(protest~disabled+finnow+finfut|year_month+pidp, data=us_l_panel_fin, weights=us_l_panel_fin$xw, cluster=us_l_panel_fin$pidp)

protest2_fin_twfe <- feols(protest~disabled+finnow+finfut|year_month+gor+pidp, data=us_l_panel_fin, weights=us_l_panel_fin$xw, cluster=us_l_panel_fin$pidp)

protest3_fin_twfe <- feols(protest~disabled+finnow+finfut+empstat4+log_m_inc+edu3|year_month+gor+pidp,
                           data=us_l_panel_fin, weights=us_l_panel_fin$xw, cluster=us_l_panel_fin$pidp)

etable(protest1_fin,protest2_fin,protest3_fin,protest1_fin_twfe,protest2_fin_twfe,protest3_fin_twfe)

modlist <- list(protest1_fin,protest2_fin,protest3_fin,protest1_fin_twfe,protest2_fin_twfe,protest3_fin_twfe)

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('finnow' = 'Current finances',
        'finfut' = 'Future finances',
        'disabled1' = 'Disabled')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test", "adj.r.squared", "nobs"), coef_map = rev(cm), output = "protest subj finance.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#age group transitions interaction

table(us_l_panel$age_group)

table(us_l_panel$age,us_l_panel$age_group)

#Left-Right

lr1_twfe_age <- feols(leftright~disabled*age_group|year_month+gor+pidp, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

lr2_twfe_age <- feols(leftright~disabled*age_group|year_month+gor+pidp, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

lr3_twfe_age <- feols(leftright~disabled*age_group+empstat4+log_m_inc+edu3|year_month+gor+pidp,
                      data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

etable(lr1_twfe_age,lr2_twfe_age,lr3_twfe_age)

#Protest

protest1_twfe_age <- feols(protest~disabled*age_group|year_month+gor+pidp, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

protest2_twfe_age <- feols(protest~disabled*age_group|year_month+gor+pidp, data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

protest3_twfe_age <- feols(protest~disabled*age_group+empstat4+log_m_inc+edu3|year_month+gor+pidp,
                           data=us_l_panel, weights=us_l_panel$xw, cluster=us_l_panel$pidp)

etable(protest1_twfe_age,protest2_twfe_age,protest3_twfe_age)

#export

etable(lr1_twfe_age,lr2_twfe_age,lr3_twfe_age,protest1_twfe_age,protest2_twfe_age,protest3_twfe_age)

modlist <- list(lr1_twfe_age,lr2_twfe_age,lr3_twfe_age,protest1_twfe_age,protest2_twfe_age,protest3_twfe_age)

modlist <- lapply(modlist, function(model) {
  attr(model, 'FTEST') <- TRUE
  model
})

cm <- c('disabled1' = 'Disability transition',
        'age_group(25,35]' = 'Aged 26-35',
        'age_group(35,45]' = 'Aged 36-45',
        'age_group(45,55]' = 'Aged 46-55',
        'age_group(55,65]' = 'Aged 56-65',
        'age_group(65, Inf]' = 'Aged 66+',
        'disabled1:age_group(25,35]' = 'Disability transition aged 26-35',
        'disabled1:age_group(35,45]' = 'Disability transition aged 36-45',
        'disabled1:age_group(45,55]' = 'Disability transition aged 46-55',
        'disabled1:age_group(55,65]' = 'Disability transition aged 56-65',
        'disabled1:age_group(65, Inf]' = 'Disability transition aged 66+')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("F-test","adj.r.squared","nobs"), coef_map = cm, output = "table A12 - transition x agegroup.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#####
#UNREPORTED ROBUSTNESS

library(mediation)

#Formal mediation

#Left-right

#income

summary(lr3)

lr3_inc_med1 <- lm(log_m_inc~disabled+age+I(age^2)+sex+eth+year_month+gor, data = us_l_panel, weights = us_l_panel$xw)

lr3_inc_med2 <- lm(leftright~log_m_inc+disabled+age+I(age^2)+sex+eth+year_month+gor, data = us_l_panel, weights = us_l_panel$xw)

lr_med_inc_out <- mediate(lr3_inc_med1, lr3_inc_med2, treat = "disabled", mediator = "log_m_inc", sims = 100)

summary(lr_med_inc_out)

#education as binary - degree or not LPM

us_l_panel <- us_l_panel %>%
  mutate(deg = case_when(edu3=="3"~1,
                         edu3=="2"~0,
                         edu3=="1"~0))

table(us_l_panel$deg,us_l_panel$edu3)

lr3_edu_med1 <- lm(deg~disabled+age+I(age^2)+sex+eth+year_month+gor, data = us_l_panel, weights = us_l_panel$xw)

lr3_edu_med2 <- lm(leftright~deg+disabled+age+I(age^2)+sex+eth+year_month+gor, data = us_l_panel, weights = us_l_panel$xw)

lr_med_edu_out <- mediate(lr3_edu_med1, lr3_edu_med2, treat = "disabled", mediator = "deg", sims = 100)

summary(lr_med_edu_out)

#education as ordinal - have to drop FEs

lr3_edu_med1 <- polr(edu3~disabled+age+sex+eth, data = us_l_panel, weights = us_l_panel$xw, method = "probit", Hess = T)

lr3_edu_med2 <- lm(leftright~edu3+disabled+age+sex+eth, data = us_l_panel, weights = us_l_panel$xw)

lr3_edu_med1$Hessian

lr_med_edu_out <- mediate(lr3_edu_med1, lr3_edu_med2, treat = "disabled", mediator = "edu3", sims = 100)

summary(lr_med_edu_out)

modelsummary(med_inc_out, estimate = "{estimate}{stars}", statistic = "[{conf.low}, {conf.high}]", conf_level = 0.95,
             gof_omit = 'Deviance|Log|BIC|AIC|Within', coef_omit ="month|year|gor", output = "mediation.docx")

#empstat as binary - retired or not LPM

us_l_panel <- us_l_panel %>%
  mutate(retired = case_when(empstat4=="4"~0,
                             empstat4=="3"~1,
                             empstat4=="2"~0,
                             empstat4=="1"~0))

table(us_l_panel$retired,us_l_panel$empstat4)

lr3_ret_med1 <- lm(retired~disabled+age+I(age^2)+sex+eth+year_month+gor, data = us_l_panel, weights = us_l_panel$xw)

lr3_ret_med2 <- lm(leftright~retired+disabled+age+I(age^2)+sex+eth+year_month+gor, data = us_l_panel, weights = us_l_panel$xw)

lr_med_ret_out <- mediate(lr3_ret_med1, lr3_ret_med2, treat = "disabled", mediator = "retired", sims = 100)

summary(lr_med_ret_out)

#empstat as binary - employed or not LPM

us_l_panel <- us_l_panel %>%
  mutate(employed = case_when(empstat4=="4"~0,
                              empstat4=="3"~0,
                              empstat4=="2"~0,
                              empstat4=="1"~1))

table(us_l_panel$employed,us_l_panel$empstat4)

lr3_emp_med1 <- lm(employed~disabled+age+I(age^2)+sex+eth+year_month+gor, data = us_l_panel, weights = us_l_panel$xw)

lr3_emp_med2 <- lm(leftright~employed+disabled+age+I(age^2)+sex+eth+year_month+gor, data = us_l_panel, weights = us_l_panel$xw)

lr_med_emp_out <- mediate(lr3_emp_med1, lr3_emp_med2, treat = "disabled", mediator = "employed", sims = 100)

summary(lr_med_emp_out)

#output

modlist <- list("Income"=lr_med_inc_out,
                "Degree"=lr_med_edu_out,
                "Retired"=lr_med_ret_out,
                "Employed"=lr_med_emp_out)

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "[{conf.low}, {conf.high}]", conf_level = 0.95,
             gof_omit = 'Deviance|Log|BIC|AIC|Within', coef_omit ="month|year|gor", output = "LR_mediation.docx")

#Protest

#income

summary(protest3)

protest3_inc_med1 <- lm(log_m_inc~disabled+age+I(age^2)+sex+eth+year_month+gor, data = us_l_panel, weights = us_l_panel$xw)

protest3_inc_med2 <- lm(protest~log_m_inc+disabled+age+I(age^2)+sex+eth+year_month+gor, data = us_l_panel, weights = us_l_panel$xw)

protest_med_inc_out <- mediate(protest3_inc_med1, protest3_inc_med2, treat = "disabled", mediator = "log_m_inc", sims = 100)

summary(protest_med_inc_out)

#education as binary - degree or not LPM

us_l_panel <- us_l_panel %>%
  mutate(deg = case_when(edu3=="3"~1,
                         edu3=="2"~0,
                         edu3=="1"~0))

table(us_l_panel$deg,us_l_panel$edu3)

protest3_edu_med1 <- lm(deg~disabled+age+I(age^2)+sex+eth+year_month+gor, data = us_l_panel, weights = us_l_panel$xw)

protest3_edu_med2 <- lm(protest~deg+disabled+age+I(age^2)+sex+eth+year_month+gor, data = us_l_panel, weights = us_l_panel$xw)

protest_med_edu_out <- mediate(protest3_edu_med1, protest3_edu_med2, treat = "disabled", mediator = "deg", sims = 100)

summary(protest_med_edu_out)

#empstat as binary - retired or not LPM

us_l_panel <- us_l_panel %>%
  mutate(retired = case_when(empstat4=="4"~0,
                             empstat4=="3"~1,
                             empstat4=="2"~0,
                             empstat4=="1"~0))

table(us_l_panel$retired,us_l_panel$empstat4)

protest3_ret_med1 <- lm(retired~disabled+age+I(age^2)+sex+eth+year_month+gor, data = us_l_panel, weights = us_l_panel$xw)

protest3_ret_med2 <- lm(protest~retired+disabled+age+I(age^2)+sex+eth+year_month+gor, data = us_l_panel, weights = us_l_panel$xw)

protest_med_ret_out <- mediate(protest3_ret_med1, protest3_ret_med2, treat = "disabled", mediator = "retired", sims = 100)

summary(protest_med_ret_out)

#empstat as binary - employed or not LPM

us_l_panel <- us_l_panel %>%
  mutate(employed = case_when(empstat4=="4"~0,
                              empstat4=="3"~0,
                              empstat4=="2"~0,
                              empstat4=="1"~1))

table(us_l_panel$employed,us_l_panel$empstat4)

protest3_emp_med1 <- lm(employed~disabled+age+I(age^2)+sex+eth+year_month+gor, data = us_l_panel, weights = us_l_panel$xw)

protest3_emp_med2 <- lm(protest~employed+disabled+age+I(age^2)+sex+eth+year_month+gor, data = us_l_panel, weights = us_l_panel$xw)

protest_med_emp_out <- mediate(protest3_emp_med1, protest3_emp_med2, treat = "disabled", mediator = "employed", sims = 100)

summary(protest_med_emp_out)

#output

modlist <- list("Income"=protest_med_inc_out,
                "Degree"=protest_med_edu_out,
                "Retired"=protest_med_ret_out,
                "Employed"=protest_med_emp_out)

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "[{conf.low}, {conf.high}]", conf_level = 0.95,
             gof_omit = 'Deviance|Log|BIC|AIC|Within', coef_omit ="month|year|gor", output = "protest_mediation.docx")

