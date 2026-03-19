### This file replicates the panel data analyses reported in Toshkov and Mazepus 'Does the Election Winner–Loser Gap Extend to Subjective Health and Well-Being?' (2022) in Political Studies Review
### All data files are available from the LISS website after registration: https://www.dataarchive.lissdata.nl/

### 0.  Libraries and functions ---------------------------------------------
library(haven)
library(tidyverse)
library(tableHTML)
### 1a. Load and merge LISS data sources: 2010 elections and government ---------------------------------------

### Elections on 09-06-2010, new government (VVD in; PvdA and CU out, CDA stays) on 14-10-2010

## Background variables
b09 <- read_sav('./data/2010 elections/avars_200912_EN_20p.sav')  # 12 2009
b10 <- read_sav('./data/2010 elections/avars_201012_EN_20p.sav')  # 12 2010

## Politics and values
p09 <- read_sav('./data/2010 elections/cv10c_EN_10p.sav') # Wave 3: 02-11-2009 till 30-12-2009
p10 <- read_sav('./data/2010 elections/cv11d_EN_10p.sav') # Wave 4: 01-11-2010 till 29-12-2010

## Health 
h09 <- read_sav('./data/2010 elections/ch09c_EN_11p.sav') # Wave 3: 02-11-2009 till 30-12-2009
h10 <- read_sav('./data/2010 elections/ch10d_EN_10p.sav') # Wave 4: 01-11-2010 till 29-12-2010

## Personality
r09 <- read_sav('./data/2010 elections/cp09b_10p_EN.sav') # Wave 2: 04-05-2009 till 30-06-2009
r10 <- read_sav('./data/2010 elections/cp10c_10p_EN.sav') # Wave 3: 03-05-2010 till 30-06-2010

## Merge the datasets
liss09 <- left_join (p09, h09, by='nomem_encr')
liss09 <- left_join (liss09, r09, by='nomem_encr')
liss09 <- left_join (liss09, b09, by='nomem_encr')

liss10 <- left_join (p10, h10, by='nomem_encr')
liss10 <- left_join (liss10, r10, by='nomem_encr')
#liss10 <- left_join (liss10, b10, by='nomem_encr')

liss0910<-left_join(liss09, liss10, by='nomem_encr')

## Recode variables
liss0910 <- liss0910 %>%
  filter (leeftijd > 17) %>%
  mutate (age = leeftijd,
          age_cat = lftdcat,
          age_cat3 = ifelse(age<35, 'young', ifelse(age<65, 'midage', 'old')),
          edu = dplyr::recode (as.character(oplzon), '1'='1.lower','2'='2.midlow','3'='2.midlow','4'='3.midhigh', '5'='3.midhigh', '6'='4.high', .default=NA_character_),
          edu_cat3 = ifelse(edu=='4.high', 'high_edu', ifelse(edu=='3.midhigh', 'mid_edu', 'low_edu')),
          sex = ifelse(geslacht==2, 'woman', 'man'),
          
          party.vote.intent.09 = dplyr::recode(as.character(cv10c058), '1' = 'no vote', '3' ='CDA', '4'='PvdA', '5'='VVD', '6'='SP', '7'='GL','8'='D66', '9'='CU', 
                                               '10'='SGP', '11'='TN', '12'='PVV', '13'='PvdD',.default = NA_character_),
          
          subjective.health.09 = ch09c004,
          subjective.health.dummy.09 = ifelse(subjective.health.09 > 3, 1, ifelse(subjective.health.09 <3, -1, 0)),
          
          subjective.health.change.09 = ch09c005,
          felt.happy.09 = ch09c015,
          felt.anxious.09 = ch09c011,
          felt.depressed.09 = ch09c014,
          
          happy.09 = cp09b010,
          lifesat.09 = cp09b011,
          feel.good.09 = cp09b013,
          lifesat2.09 = cp09b016,
          trust.09 = cp09b019,
          feel.connected.09 = cp09b135,
          
          party.vote.10 = dplyr::recode(as.character(cv11d169), '1' = 'VVD', '2' ='PvdA', '3'='PVV', '4'='CDA', '5'='SP', '6'='D66','7'='GL', '8'='CU', '9'='SGP', 
                                        '10'='PvdD',.default = NA_character_),
          voted.10 = cv11d053,
          party.vote.10 = ifelse (voted.10==2, 'no vote', party.vote.10),
          pol.interest = ifelse(cv11d012 == 1, 1, 0),
          
          subjective.health.10 = ch10d004,
          subjective.health.dummy.10 = ifelse(subjective.health.10 > 3, 1, ifelse(subjective.health.10 < 3, -1, 0)),
          subjective.health.change.post = ch10d005,
          felt.happy.10 = ch10d015,
          felt.anxious.10 = ch10d011,
          felt.depressed.10 = ch10d014,
          
          happy.10 = cp10c010,
          lifesat.10 = cp10c011,
          feel.good.10 = cp10c013,
          lifesat2.10 = cp10c016,
          trust.10 = cp10c019,
          feel.connected.10 = cp10c135,
          
          winner.status = ifelse (party.vote.10 == 'CDA' | party.vote.10 == 'VVD' , 'winner',  'loser'),
          cabinet.status = ifelse (party.vote.10 == 'VVD', 'winner', ifelse(party.vote.10 == 'PvdA' | party.vote.10 == 'CU', 'loser', '0_neutral')),
          election.status = ifelse (party.vote.10 == 'VVD' | party.vote.10 == 'PVV' , 'winner', ifelse(party.vote.10 == 'PvdA' | party.vote.10 == 'CDA' | party.vote.10 == 'SP', 'loser', 
                                                                             ifelse(party.vote.10 == 'no vote', 'no vote','0_neutral'))),
          
          winner.strong = ifelse (cv11d173 >8 & cabinet.status=='winner', 1, 0),
          loser.strong = ifelse ((cv11d174 > 8 | cv11d180 > 8) & cabinet.status=='loser', 1, 0),
          
          change.subjective.health = subjective.health.10 - subjective.health.09,
          change.felt.happy = felt.happy.10 - felt.happy.09,
          change.felt.anxious = felt.anxious.10 - felt.anxious.09,
          change.felt.depressed = felt.depressed.10 - felt.depressed.09,
          
          change.happy = happy.10 - happy.09,
          change.lifesat = lifesat.10 - lifesat.09,
          change.feel.good = feel.good.10 - feel.good.09,
          change.lifesat2 = lifesat2.10 - lifesat2.09,
          change.trust = trust.10 - trust.09,
          change.feel.connected = feel.connected.10 - feel.connected.09
          )

### 1b. Load and merge LISS data sources: 2012 elections and government ---------------------------------------
# Elections on 12-09-2012, new government (VVD, PvdA in; CDA and PVV out) on 5-11-2012

## Background variables
b11 <- read_sav('./data/2012 elections/avars_201112_EN_20p.sav')  # 12 2011
b12 <- read_sav('./data/2012 elections/avars_201212_EN_10p.sav')  # 12 2012

## Politics and values
p11 <- read_sav('./data/2012 elections/cv12e_EN_10p.sav') # Wave 5: 05-12-2011 till 31-01-2012
p12 <- read_sav('./data/2012 elections/cv13f_EN_10p.sav') # Wave 6: 03-12-2012 till 29-01-2013

## Health
h11 <- read_sav('./data/2012 elections/ch11e_EN_10p.sav') # Wave 5: 07-11-2011 till 26-12-2011
h12 <- read_sav('./data/2012 elections/ch12f_EN_10p.sav') # Wave 6: 05-11-2012 (!) till 31-12-2012

## Personality
r11 <- read_sav('./data/2012 elections/cp12e_10p_EN.sav') # Wave 5: 07-05-2012 till 26-06-2012
r12 <- read_sav('./data/2012 elections/cp13f_EN_10p.sav') # Wave 6: 06-05-2013 till 25-06-2013


## Merge the datasets
liss11 <- left_join (p11, h11, by='nomem_encr')
liss11 <- left_join (liss11, r11, by='nomem_encr')
liss11 <- left_join (liss11, b11, by='nomem_encr')

liss12 <- left_join (p12, h12, by='nomem_encr')
liss12 <- left_join (liss12, r12, by='nomem_encr')

liss1112<-left_join(liss11, liss12, by='nomem_encr')

## Recode variables
liss1112 <- liss1112 %>%
  filter (leeftijd > 17) %>%
  mutate (age = leeftijd,
          age_cat = lftdcat,
          age_cat3 = ifelse(age<35, 'young', ifelse(age<65, 'midage', 'old')),
          edu = dplyr::recode (as.character(oplzon), '1'='1.lower','2'='2.midlow','3'='2.midlow','4'='3.midhigh', '5'='3.midhigh', '6'='4.high', .default=NA_character_),
          edu_cat3 = ifelse(edu=='4.high', 'high_edu', ifelse(edu=='3.midhigh', 'mid_edu', 'low_edu')),
          sex = ifelse(geslacht==2, 'woman', 'man'),
          
          party.vote.intent.11 = dplyr::recode(as.character(cv12e171), '1' = 'no vote', '3' ='VVD', '4'='PvdA', '5'='PVV',
                                               '6'='CDA', '7'='SP','8'='D66', '9'='GL', 
                                               '10'='CU', '11'='SGP', '12'='PvdD',.default = NA_character_),
          
          subjective.health.11 = ch11e004,
          subjective.health.change.11 = ch11e005,
          subjective.health.dummy.11 = ifelse(subjective.health.11 > 3, 1, ifelse(subjective.health.11 < 3, -1, 0)),
          felt.happy.11 = ch11e015,
          felt.anxious.11 = ch11e011,
          felt.depressed.11 = ch11e014,
          
          happy.11 = cp12e010,
          lifesat.11 = cp12e011,
          feel.good.11 = cp12e013,
          lifesat2.11 = cp12e016,
          trust.11 = cp12e019,
          feel.connected.11 = cp12e135,
          
          party.vote.12 = dplyr::recode(as.character(cv13f207), '1' = 'VVD', '2' ='PvdA', '3'='PVV', '4'='SP', '5'='CDA', 
                                        '6'='D66','7'='CU', '8'='GL', '9'='SGP', 
                                        '10'='PvdD','11'='50+',.default = NA_character_),
          voted.12 = cv13f053,
          party.vote.12 = ifelse (voted.12==2, 'no vote', party.vote.12),
          
          subjective.health.12 = ch12f004,
          subjective.health.dummy.12 = ifelse(subjective.health.12 > 3, 1, ifelse(subjective.health.12 < 3, -1, 0)),
          subjective.health.change.post = ch12f005,
          felt.happy.12 = ch12f015,
          felt.anxious.12 = ch12f011,
          felt.depressed.12 = ch12f014,
          
          happy.12 = cp13f010,
          lifesat.12 = cp13f011,
          feel.good.12 = cp13f013,
          lifesat2.12 = cp13f016,
          trust.12 = cp13f019,
          feel.connected.12 = cp13f135,
          
          winner.status = ifelse (party.vote.12 == 'PvdA' | party.vote.12 == 'VVD' , 'winner',  'loser'),
          cabinet.status = ifelse (party.vote.12 == 'PvdA', 'winner', ifelse(party.vote.12 == 'CDA', 'loser', '0_neutral')),
          election.status = ifelse (party.vote.12 == 'VVD' | party.vote.12 == 'PvdA' , 'winner', ifelse(party.vote.12 == 'CDA' | party.vote.12 == 'PVV' | party.vote.12 == 'GL', 'loser', 
                                                                                                       ifelse(party.vote.12 == 'no vote', 'no vote','0_neutral'))),
          pol.interest = ifelse(cv13f012 == 1, 1, 0),
          
          change.subjective.health = subjective.health.12 - subjective.health.11,
          change.felt.happy = felt.happy.12 - felt.happy.11,
          change.felt.anxious = felt.anxious.12 - felt.anxious.11,
          change.felt.depressed = felt.depressed.12 - felt.depressed.11,
       
          change.happy = happy.12 - happy.11,
          change.lifesat = lifesat.12 - lifesat.11,
          change.feel.good = feel.good.12 - feel.good.11,
          change.lifesat2 = lifesat2.12 - lifesat2.11,
          change.trust = trust.12 - trust.11,
          change.feel.connected = feel.connected.12 - feel.connected.11
  )

### 1c. Load and merge LISS data sources: 2017 elections and government ---------------------------------------
# Elections on 15-03-2017, new government (VVD, PvdA out; CDA, D66 and CU in) on 26 October 2017

## Background variables
b16 <- read_sav('./data/2017 elections/avars_201612_EN_10p.sav')  # 12 2016
b17 <- read_sav('./data/2017 elections/avars_201712_EN_10p.sav')  # 12 2017

## Politics and values
p16 <- read_sav('./data/2017 elections/cv17i_EN_10p.sav') # Wave 9: 05-12-2016 till 31-01-2017
p17 <- read_sav('./data/2017 elections/cv18j_EN_10p.sav') # Wave 10: 03-17-2017 till 29-01-2013

## Health
h16 <- read_sav('./data/2017 elections/ch16i_EN_10p.sav') # Wave 9: 07-11-2016 till 27-12-2016
h17 <- read_sav('./data/2017 elections/ch17j_EN_10p.sav') # Wave 10: 06-11-2017 till 26-12-2017

## Personality
r15 <- read_sav('./data/2017 elections/cp15h_EN_10p.sav') # Wave 8: 02-11-2015 till 29-12-2015 (!)
r17 <- read_sav('./data/2017 elections/cp17i_EN_10p.sav') # Wave 9: 01-05-2017 till 25-06-2017
r18 <- read_sav('./data/2017 elections/cp18j_EN_10p.sav') # Wave 10: 07-05-2018 till 26-06-2018 (!)


## Merge the datasets
liss16 <- left_join (p16, h16, by='nomem_encr')
liss16 <- left_join (liss16, r15, by='nomem_encr')
liss16 <- left_join (liss16, b16, by='nomem_encr')

liss17 <- left_join (p17, h17, by='nomem_encr')
liss17 <- left_join (liss17, r17, by='nomem_encr')
liss17 <- left_join (liss17, r18, by='nomem_encr')

liss1617<-left_join(liss16, liss17, by='nomem_encr')

## Recode variables
liss1617 <- liss1617 %>%
  filter (leeftijd > 17) %>%
  mutate (age = leeftijd,
          age_cat = lftdcat,
          age_cat3 = ifelse(age<35, 'young', ifelse(age<65, 'midage', 'old')),
          edu = dplyr::recode (as.character(oplzon), '1'='1.lower','2'='2.midlow','3'='2.midlow','4'='3.midhigh', '5'='3.midhigh', '6'='4.high', .default=NA_character_),
          edu_cat3 = ifelse(edu=='4.high', 'high_edu', ifelse(edu=='3.midhigh', 'mid_edu', 'low_edu')),
          sex = ifelse(geslacht==2, 'woman', 'man'),
          
          party.vote.intent.16 = dplyr::recode(as.character(cv17i244), '1' = 'no vote', '2' ='VVD', '3'='PvdA', '4'='PVV',
                                               '5'='SP', '6'='CDA', '7'='D66','8'='CU', '9'='GL', 
                                               '10'='SGP', '11'='PvdD','12'='50+', '13'='DENK',.default = NA_character_),
          
          subjective.health.16 = ch16i004,
          subjective.health.dummy.16 = ifelse(subjective.health.16 > 3, 1, ifelse(subjective.health.16 < 3, -1, 0)),
          subjective.health.change.16 = ch16i005,
          felt.happy.16 = ch16i015,
          felt.anxious.16 = ch16i011,
          felt.depressed.16 = ch16i014,
          
          happy.16 = cp15h010,
          lifesat.16 = cp15h011,
          feel.good.16 = cp15h013,
          lifesat2.16 = cp15h016,
          trust.16 = cp15h019,
          feel.connected.16 = cp15h135,
          
          
          party.vote.17 = dplyr::recode(as.character(cv18j307), '1' = 'VVD', '2' ='PVV', '3'='CDA', '4'='D66', '5'='GL', 
                                        '6'='SP','7'='PvdA', '8'='CU', '9'='PvdD', 
                                        '10'='50+','11'='SGP','12'='DENK','13'='FvD',.default = NA_character_),
          voted.17 = cv18j053,
          party.vote.17 = ifelse (voted.17==2, 'no vote', party.vote.17),
          pol.interest = ifelse(cv18j012 == 1, 1, 0),
          
          
          subjective.health.17 = ch17j004,
          subjective.health.dummy.17 = ifelse(subjective.health.17 > 3, 1, ifelse(subjective.health.17 < 3, -1, 0)),
          subjective.health.change.post = ch17j005,
          felt.happy.17 = ch17j015,
          felt.anxious.17 = ch17j011,
          felt.depressed.17 = ch17j014,

          happy.17 = cp17i010,
          lifesat.17 = cp17i011,
          feel.good.17 = cp17i013,
          lifesat2.17 = cp17i016,
          trust.17 = cp17i019,
          feel.connected.17 = cp17i135,
          
          happy.18 = cp18j010,
          lifesat.18 = cp18j011,
          feel.good.18 = cp18j013,
          lifesat2.18 = cp18j016,
          trust.18 = cp18j019,
          feel.connected.18 = cp18j135,
          
          winner.status = ifelse (party.vote.17 == 'CDA' | party.vote.17 == 'VVD' | party.vote.17 == 'CU' | party.vote.17 == 'D66' , 'winner',  'loser'),
          cabinet.status = ifelse (party.vote.17 == 'VVD' | party.vote.17 == 'D66' | party.vote.17 == 'CDA' | party.vote.17 == 'CU', 'winner', ifelse(party.vote.17 == 'PvdA', 'loser', '0_neutral')),
          election.status = ifelse (party.vote.17 == 'GL' | party.vote.17 == 'D66' | party.vote.17 == 'PVV' | party.vote.17 == 'CDA' , 'winner', ifelse(party.vote.17 == 'VVD' | party.vote.17 == 'PvdA', 'loser', 
                                                                                                        ifelse(party.vote.17 == 'no vote', 'no vote','0_neutral'))),
          change.subjective.health = subjective.health.17 - subjective.health.16,
          change.felt.happy = felt.happy.17 - felt.happy.16,
          change.felt.anxious = felt.anxious.17 - felt.anxious.16,
          change.felt.depressed = felt.depressed.17 - felt.depressed.16,
         
          change.happy = happy.17 - happy.16,
          change.lifesat = lifesat.17 - lifesat.16,
          change.feel.good = feel.good.17 - feel.good.16,
          change.lifesat2 = lifesat2.17 - lifesat2.16,
          change.trust = trust.17 - trust.16,
          change.feel.connected = feel.connected.17 - feel.connected.16,
          
          change2.happy = happy.18 - happy.16,
          change2.lifesat = lifesat.18 - lifesat.16,
          change2.feel.good = feel.good.18 - feel.good.16,
          change2.lifesat2 = lifesat2.18 - lifesat2.16,
          change2.trust = trust.18 - trust.16,
          change2.feel.connected = feel.connected.18 - feel.connected.16
  )


### 1d. Combine data: stack all data into one dataset -------------------------------------------------
liss.all <- bind_rows(liss0910[, c('change.subjective.health','subjective.health.change.post','change.felt.happy','change.felt.depressed','change.felt.anxious',
                                  'change.happy','change.lifesat','change.feel.good','change.trust','change.feel.connected',
                                  'cabinet.status', 'election.status','winner.status', 'age','sex', 'pol.interest', 'nomem_encr')],
                      liss1112[, c('change.subjective.health','subjective.health.change.post','change.felt.happy','change.felt.depressed','change.felt.anxious',
                                   'change.happy','change.lifesat','change.feel.good','change.trust','change.feel.connected',
                                   'cabinet.status', 'election.status','winner.status', 'age','sex', 'pol.interest', 'nomem_encr')],
                      liss1617[, c('change.subjective.health','subjective.health.change.post','change.felt.happy','change.felt.depressed','change.felt.anxious',
                                   'change.happy','change.lifesat','change.feel.good','change.trust','change.feel.connected',
                                   'cabinet.status', 'election.status','winner.status', 'age','sex', 'pol.interest', 'nomem_encr')],
                      .id = "id")
summary(liss.all)
write.csv(liss.all, './data/liss_data_processed.csv')

### 2. Table of descriptives ---------------------------------------

vars <-c ('change.subjective.health',
          'change.happy','change.lifesat','change.feel.good','change.felt.depressed','change.felt.anxious','change.feel.connected')

### Table of descriptive
descr_table<-data.frame(matrix(0, nrow=length(vars), ncol=6))
descr_table[,1] <- vars
colnames(descr_table) <- c('Variable', 'Min','Median','Mean', 'Max', 'StDv')
liss.all<-data.frame(liss.all)
for (i in 1: length(vars)){
  descr_table[i,2]<-round(min(liss.all[, vars[i]], na.rm=TRUE),1)
  descr_table[i,3]<-round(median(liss.all[, vars[i]], na.rm=TRUE),1)
  descr_table[i,4]<-round(mean(liss.all[, vars[i]], na.rm=TRUE),1)
  descr_table[i,5]<-round(max(liss.all[, vars[i]], na.rm=TRUE),1)
  descr_table[i,6]<-round(sd(liss.all[, vars[i]], na.rm=TRUE),1)
}

descr_table_health<-tableHTML(descr_table, rownames=FALSE, border = 0.5) %>% add_theme('scientific')
write_tableHTML(descr_table_health, file='./tables/descr_table_liss_health.html')

#### 3a. Regression models I: cabinet status ---------------------------------------
rem1<-lm(change.subjective.health ~ cabinet.status + age + sex, data=liss.all)
rem1a<-lm(subjective.health.change.post ~ cabinet.status + age + sex, data=liss.all) # alternative way to measure change in health
rem2<-update(rem1, change.happy  ~ .)
rem3<-update(rem1, change.lifesat  ~ .)
rem4<-update(rem1, change.feel.good   ~ .)
rem5<-update(rem1, change.felt.depressed  ~ .)
rem6<-update(rem1, change.felt.anxious  ~ .)
rem7<-update(rem1, change.feel.connected  ~ .)

tab_model(rem1, rem2, rem3, rem4, rem5, rem6, rem7, show.ci=FALSE, show.se=TRUE, digits=2, digits.p=2, digits.re=2, file = './tables/liss_models_health.html')

# Check clustering at the respondent level
library(sandwich)
library(lmtest)
m1coeffs_cl <- coeftest(rem7, vcov = vcovCL, cluster = ~ nomem_encr)
m1coeffs_cl

### 3b. Regression models II: winner status ---------------------------------------
wrem1<-lm(change.subjective.health ~ winner.status + age + sex, data=liss.all)
wrem2<-update(wrem1, change.happy  ~ .)
wrem3<-update(wrem1, change.lifesat  ~ .)
wrem4<-update(wrem1, change.feel.good   ~ .)
wrem5<-update(wrem1, change.felt.depressed  ~ .)
wrem6<-update(wrem1, change.felt.anxious  ~ .)
wrem7<-update(wrem1, change.feel.connected  ~ .)

tab_model(wrem1, wrem2, wrem3, wrem4, wrem5, wrem6, wrem7, show.ci=FALSE, show.se=TRUE, digits=2, digits.p=2, digits.re=2, file = './tables/liss_models_health_2.html')

### 4. Differential dropout ---------------------------------------
liss0910$dropout <- ifelse (is.na(liss0910$nohouse_encr.x.y)==T, 1, 0)
liss1112$dropout <- ifelse (is.na(liss1112$cv13f_m)==T, 1, 0)
liss1617$dropout <- ifelse (is.na(liss1617$cv18j_m1)==T, 1, 0)

# no differential dropout per party
round(prop.table(table(liss0910$dropout, liss0910$party.vote.intent.09),2),2)
round(prop.table(table(liss1112$dropout, liss1112$party.vote.intent.11),2),2)
round(prop.table(table(liss1617$dropout, liss1617$party.vote.intent.16),2),2)

# but differential dropout per health condition
round(prop.table(table(liss0910$dropout, liss0910$subjective.health.09),2),2)
round(prop.table(table(liss1112$dropout, liss1112$subjective.health.11),2),2)
round(prop.table(table(liss1617$dropout, liss1617$subjective.health.16),2),2)

### 5. Exploratory analysis: party support per health condition ---------------------------------------
round(prop.table(table(liss0910$subjective.health.dummy.09, liss0910$party.vote.intent.09),2),2)
round(prop.table(table(liss1112$subjective.health.dummy.11, liss1112$party.vote.intent.11),2),2)
round(prop.table(table(liss1617$subjective.health.dummy.16, liss1617$party.vote.intent.16),2),2)
### The end ---------------------------
sessionInfo()





