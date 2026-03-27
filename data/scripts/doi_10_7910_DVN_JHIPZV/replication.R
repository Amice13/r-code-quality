#######################################################
###Replication codes for Fazekas, Mihály, Sberna, Salvatore & Vannucci, Alberto (2021) The Extra-legal Governance of Corruption. Tracing the Organization of Corruption in Public Procurement. Governance
###2010 November
#######################################################

#setting your working directory
setwd("[your folder]")

library(tidyverse)
library(stringr)
library(broom)
library(ggplot2)

library(caret)#models tuning
library(stargazer)#tables formatting

#logistic regression:
library(ROCR)
library(pROC)
library(plotROC)
source("scripts/log_reg_funs.R")

#Random Forest:
library(randomForest)

#Boosting:
library(gbm)

#In this script we are using an Italian Public Procurement data with new variables created. The process of creating new variables is below. 
IT_data <- read.csv("IT_PP_data.csv", stringsAsFactors = FALSE)


#### IT PP data - variables creation #### 
##merge geographical features
#NUTS3
#We match NUTS3 codes with local province codes with NUTS3. Source: https://www.istat.it/it/archivio/6789
Elenco_comuni_italiani <- read.csv("data/nuts_data/Elenco-comuni-italiani.csv", sep = ',', stringsAsFactors = FALSE)
Elenco_comuni_italiani <- Elenco_comuni_italiani %>% select(c(Codice.Provincia..Storico..1., NUTS3)) %>% distinct()
Elenco_comuni_italiani$Codice.Provincia..Storico..1.<-as.numeric(Elenco_comuni_italiani$Codice.Provincia..Storico..1.)
IT_data <- IT_data %>%
  left_join(
    Elenco_comuni_italiani, 
    by = c('cod_prov'='Codice.Provincia..Storico..1.')
  )
table(IT_data$NUTS3)
unique(IT_data$NUTS3)
IT_data %>% filter(!is.na(NUTS3)&nchar(NUTS3)>2) %>% count()/nrow(IT_data)
#filling missing values with the existing observations in ca_nuts column 
IT_data <- IT_data %>% mutate(NUTS3 = ifelse(is.na(NUTS3)|nchar(NUTS3)<2,ca_nuts,NUTS3)) 
IT_data$NUTS3<-toupper(IT_data$NUTS3)
table(IT_data$NUTS3)
#check
Elenco_comuni_italiani %>% select(NUTS3) %>% filter(!(NUTS3%in%IT_data$NUTS3)) # 4 codes
IT_data <- IT_data %>% mutate(NUTS3 = ifelse(str_detect(NUTS3,'ITZ') | nchar(NUTS3)<2, NA, NUTS3))
rm(Elenco_comuni_italiani)

#Population
#source: https://ec.europa.eu/eurostat/web/regions/IT_data/IT_database
pop <- read.csv("data/nuts_data/demo_r_pjangrp3_1_Data.csv",stringsAsFactors = FALSE)
pop <- pop %>% 
  filter(SEX=='Total' & TIME=='2014') %>% 
  select(c(GEO,Value))
pop <- pop %>% 
  mutate(Value=Value %>% str_remove_all(',')) %>% 
  filter(str_detect(GEO, "IT")) 
IT_data <- IT_data %>% 
  left_join(pop, by = c('NUTS3'='GEO')) %>% 
  rename(pop_nuts = Value)
sum(!is.na(IT_data$pop_nuts))/nrow(IT_data)
IT_data$pop_nuts<-as.numeric(IT_data$pop_nuts)
hist(IT_data$pop_nuts)
rm(pop)
#Log population
IT_data <- IT_data %>% mutate(lpop_nuts = log(pop_nuts))
hist(IT_data$lpop_nuts)

#Coastal region dummy 
#source: https://ec.europa.eu/eurostat/web/coastal-island-outermost-regions/background
regions <- read.csv("data/nuts_data/NUTS3_geogr_features.csv",stringsAsFactors = FALSE)
IT_data <- IT_data %>% left_join(
  regions %>% select(properties.NUTS_ID,properties.COAST_TYPE), 
  by = c('NUTS3'="properties.NUTS_ID")
)
sum(!is.na(IT_data$properties.COAST_TYPE))/nrow(IT_data)
#check
IT_data %>%
  filter(is.na(properties.COAST_TYPE)) %>%
  select(properties.COAST_TYPE, NUTS3, comunelitoraneo) %>%
  summary() 
IT_data <- IT_data %>% select(-comunelitoraneo)
IT_data <- IT_data %>% 
  rename(coast_region = properties.COAST_TYPE) %>%
  mutate(coast_region=ifelse(coast_region==2|coast_region==3,0,1)) 
table(IT_data$coast_region,useNA="ifany")
rm(regions)
#missing values to 0
IT_data<-IT_data %>% mutate(coast_region=ifelse(is.na(coast_region),0,coast_region))
table(IT_data$coast_region,useNA="ifany")

#Mountain region 
#source: https://cohesionIT_data.ec.europa.eu/Other/LOOKUP-NUTS3-2010-Mountain-Island-Sparsely-populat/dcr2-quev/IT_data
regions_m <- read.csv("data/nuts_data/LOOKUP_NUTS3_2010__-_Mountain__Island__Sparsely_populated__RUP_regions_-_EU28.csv", stringsAsFactors = FALSE)
regions_m <- regions_m %>% filter(mountain.regions==1) %>% select(NUTS3_ID_2010, mountain.regions)
table(IT_data$comunemontano)
IT_data <- IT_data %>%
  left_join(
    regions_m, #list of mountain regions
    by=c('NUTS3'="NUTS3_ID_2010")
  ) %>%
  mutate(mountain.regions=ifelse(is.na(mountain.regions),0, mountain.regions)) %>%
  rename(mountain_region=mountain.regions) %>%
  mutate(mountain_region=as.factor(mountain_region))
IT_data %>%
  select(comunemontano, mountain_region) %>%
  mutate(comunemontano=as.factor(comunemontano)) %>%
  summary()
IT_data <- IT_data  %>% select(-comunemontano)
table(IT_data$mountain_region,useNA = "ifany")
rm(regions_m)

#Area (sq.km.)
#source (2014): https://appsso.eurostat.ec.europa.eu/nui/show.do?IT_dataset=demo_r_d3area&lang=en
area <- read.csv("data/nuts_data/demo_r_d3area_1_Data.csv", stringsAsFactors = FALSE) %>% 
  select(c(GEO, Value)) %>% 
  distinct() %>% 
  filter(str_detect(GEO,"IT")) %>% 
  mutate(Value=Value %>% str_remove_all(',') %>% as.numeric())
IT_data <- IT_data %>% left_join(area, by=c('NUTS3'="GEO"))
IT_data <- IT_data %>% rename(kmq=Value)
hist(IT_data$kmq)
sum(is.na(IT_data$kmq))
IT_data <- IT_data %>% select(-cod_prov)
rm(area)
#area in thousands
hist(IT_data$kmq)
IT_data <- IT_data %>% mutate(kmq_thousands = kmq/1000)
hist(IT_data$kmq_thousands)

#share of largest supplier in the year (procuring body-level indicator) - imputation
sum(is.na(IT_data$iwmshare_y))/nrow(IT_data)
IT_data <- IT_data %>% group_by(cpv_div,year) %>%
  mutate(iwmshare_y=ifelse(is.na(iwmshare_y),median(iwmshare_y,na.rm=TRUE),iwmshare_y)) %>% 
  ungroup()
IT_data <- IT_data %>% 
  mutate(iwmshare_y=ifelse(is.na(iwmshare_y),median(iwmshare_y,na.rm=TRUE),iwmshare_y))
sum(is.na(IT_data$iwmshare_y))
hist(IT_data$iwmshare_y)

#Procedure simplified
IT_data$ca_procedure_simpl[IT_data$ca_procedure==1]<-9
IT_data$ca_procedure_simpl[IT_data$ca_procedure==2]<-1
IT_data$ca_procedure_simpl[IT_data$ca_procedure==3]<-1
IT_data$ca_procedure_simpl[IT_data$ca_procedure==4]<-1
IT_data$ca_procedure_simpl[IT_data$ca_procedure==5]<-1
IT_data$ca_procedure_simpl[IT_data$ca_procedure==6]<-1
IT_data$ca_procedure_simpl[IT_data$ca_procedure==7]<-1
IT_data$ca_procedure_simpl[IT_data$ca_procedure==8]<-9
IT_data$ca_procedure_simpl[IT_data$ca_procedure==9]<-9
IT_data$ca_procedure_simpl[IT_data$ca_procedure==10]<-9
IT_data$ca_procedure_simpl[IT_data$ca_procedure==11]<-2
IT_data$ca_procedure_simpl[IT_data$ca_procedure==12]<-3
IT_data$ca_procedure_simpl[IT_data$ca_procedure==13]<-3
IT_data$ca_procedure_simpl[IT_data$ca_procedure==14]<-3
IT_data$ca_procedure_simpl[IT_data$ca_procedure==15]<-3
IT_data$ca_procedure_simpl[IT_data$ca_procedure==16]<-3
IT_data$ca_procedure_simpl[IT_data$ca_procedure==17]<-4
IT_data$ca_procedure_simpl[IT_data$ca_procedure==18]<-4
IT_data$ca_procedure_simpl[IT_data$ca_procedure==19]<-4
IT_data$ca_procedure_simpl[IT_data$ca_procedure==20]<-9
IT_data$ca_procedure_simpl[IT_data$ca_procedure==21]<-9

IT_data$ca_procedure_simpl<-factor(IT_data$ca_procedure_simpl, levels=c(1,2,3,4,9),
                                   labels=c("Direct award",
                                            "Open procedure",
                                            "Negotiated",
                                            "Restricted",
                                            "Other/Missing"))
table(IT_data$ca_procedure_simpl)
dev.off()


#####
#In this script we are using a TED dataset with variables created. The process of creating new variables is outlined below. 
#Link to the full TED dataset: https://opentender.eu/all/download
TED_data<-read.csv("TED_data.csv",stringsAsFactors = FALSE)
#### TED data - variables creation ####
#functions for variables creation:
source("scripts/TED_data_funs.R")

##merge geographical features

#Population
nuts3 <- TED_data %>% select(buyer_nuts) %>% distinct() %>% filter(nchar(buyer_nuts)>2)
pop <- read.csv("data/nuts_data/demo_r_pjangrp3_1_Data.csv",stringsAsFactors = FALSE)
pop <- pop %>% 
  filter(SEX=='Total' & TIME=='2014') %>% 
  select(c(GEO,Value))
pop <- pop %>% mutate(Value=Value %>% str_remove_all(','))
nuts3 <- nuts3 %>% 
  left_join(pop, by = c('buyer_nuts'='GEO')) %>% 
  rename(pop_nuts = Value) 
sum(!is.na(nuts3$pop_nuts))/nrow(nuts3)
# unmatched codes are old NUTS3 codes, replacing them with NUTS 2016 
nuts3_2006 <- readxl::read_excel("data/nuts_data/2006-2010.xls",sheet = 'Correspondence NUTS-3') %>% 
  select(c('Code 2006', 'Code 2010'))
nuts3_2010 <- readxl::read_excel("data/nuts_data/NUTS 2010 - NUTS 2013.xls", sheet = 'Correspondence NUTS-3') %>% 
  select(c('Code 2010', 'Code 2013'))
nuts3 <- match_old_nuts(nuts3, nuts3_2006, nuts3_2010) %>% 
  select(c(buyer_nuts, pop_nuts)) %>% 
  distinct()
sum(!is.na(nuts3$pop_nuts))/nrow(nuts3)
rm(pop)

#Area
area <- read.csv("data/nuts_data/demo_r_d3area_1_Data.csv", stringsAsFactors = FALSE) %>% 
  select(c(GEO, Value)) %>% 
  distinct() %>% 
  mutate(Value=Value %>% str_remove_all(',') %>% as.numeric())
nuts3 <- nuts3 %>% 
  left_join(area, by=c('buyer_nuts'="GEO")) %>%
  rename(kmq = Value)
sum(!is.na(nuts3$kmq))/nrow(nuts3)
nuts3 <- match_old_nuts_area(nuts3, nuts3_2006, nuts3_2010) %>% 
  select(c(buyer_nuts, pop_nuts, kmq)) %>% distinct()
sum(!is.na(nuts3$kmq))/nrow(nuts3)
rm(area)

#Mountain region dummy
regions_m <- read.csv("data/nuts_data/LOOKUP_NUTS3_2010__-_Mountain__Island__Sparsely_populated__RUP_regions_-_EU28.csv", stringsAsFactors = FALSE)
regions_m <- regions_m %>% 
  filter(mountain.regions==1) %>% 
  select(NUTS3_ID_2010, mountain.regions)
nuts3 <- nuts3 %>% 
  left_join(regions_m, by=c('buyer_nuts'="NUTS3_ID_2010")) 
sum(is.na(nuts3$mountain.regions))/nrow(nuts3)
nuts3 <- match_old_mountains(nuts3, nuts3_2006, nuts3_2010) %>% 
  select(c(buyer_nuts, pop_nuts, kmq, mountain_region)) %>% 
  distinct() %>% 
  mutate(mountain_region=ifelse(is.na(mountain_region),0, mountain_region))
table(nuts3$mountain_region)
rm(regions_m)

#Coastal region dummy 
coast_type <- readxl::read_excel("data/nuts_data/coast_regions.xlsx")
nuts3 <- nuts3 %>% left_join(coast_type, by = c('buyer_nuts'="NUTS"))
nuts3 <- match_old_coast(nuts3, nuts3_2006, nuts3_2010) %>% 
  select(c(buyer_nuts, pop_nuts, kmq, mountain_region, coast_region)) %>% 
  mutate(coast_region=ifelse(is.na(coast_region),0,coast_region)) 
table(nuts3$coast_region)
rm(coast_type)
rm(nuts3_2006)
rm(nuts3_2010)

#merge
TED_data <- TED_data %>% left_join(nuts3, by = 'buyer_nuts')
rm(nuts3)

#We impute population and area variables since some NUTS3 codes in the data are not matched
sum(is.na(TED_data$pop_nuts))
TED_data <- TED_data %>% mutate(pop_nuts=as.numeric(pop_nuts))
TED_data <- TED_data %>% 
  group_by(buyer_country) %>% 
  mutate(pop_nuts=ifelse(is.na(pop_nuts), median(pop_nuts, na.rm=TRUE), pop_nuts)) %>% 
  ungroup()
sum(is.na(TED_data$kmq))
TED_data <- TED_data %>% 
  group_by(buyer_country) %>% 
  mutate(kmq=ifelse(is.na(kmq), median(kmq, na.rm=TRUE), kmq)) %>% 
  ungroup()

#log population
TED_data <- TED_data %>% 
  mutate(pop_nuts=as.numeric(pop_nuts)) %>% 
  mutate(lpop_nuts = ifelse(!is.na(pop_nuts),log(pop_nuts),pop_nuts))
hist(TED_data$lpop_nuts)

#area in thousands
TED_data <- TED_data %>% mutate(kmq_thousands = kmq/1000)
hist(TED_data$kmq_thousands)

## Red flags
#share of largest supplier in the year (procuring body-level indicator) - imputation
TED_data<-TED_data %>% 
  group_by(cpv_div,tender_year) %>% 
  mutate(iwmshare_y=ifelse(is.na(iwmshare_y),median(iwmshare_y,na.rm=TRUE),iwmshare_y)) %>% 
  ungroup()
sum(is.na(TED_data$iwmshare_y))/nrow(TED_data)

#Procedure simplified
table(TED_data$tender_proceduretype,useNA = "always")
TED_data$ca_procedure_simpl<-TED_data$tender_proceduretype
TED_data$ca_procedure_simpl[TED_data$tender_proceduretype=="NEGOTIATED_WITH_PUBLICATION"]<-"Negotiated"
TED_data$ca_procedure_simpl[TED_data$tender_proceduretype=="NEGOTIATED_WITHOUT_PUBLICATION"]<-"Negotiated"
TED_data$ca_procedure_simpl[TED_data$tender_proceduretype=="NEGOTIATED"]<-"Negotiated"
TED_data$ca_procedure_simpl[TED_data$tender_proceduretype=="OPEN"]<-"Open procedure"
TED_data$ca_procedure_simpl[TED_data$tender_proceduretype=="RESTRICTED"]<-"Restricted"
TED_data$ca_procedure_simpl[is.na(TED_data$tender_proceduretype)]<-"Other/Missing"
TED_data$ca_procedure_simpl[TED_data$tender_proceduretype=="INOVATION_PARTNERSHIP"]<-"Other/Missing"
TED_data$ca_procedure_simpl[TED_data$tender_proceduretype=="COMPETITIVE_DIALOG"]<-"Negotiated"
table(TED_data$ca_procedure_simpl,useNA = "always")

#### Merge variables for validity tests: Financial Secrecy Index and Share of shareholders from EU blacklisted countries ####
FSI<-read.csv("data/FSI.csv",stringsAsFactors = FALSE)
BS <- readxl::read_excel("data/blacklisted.xlsx")
#Financial Secrecy Index
TED_data<-TED_data %>% 
  left_join(
    FSI %>% filter(!is.na(ISO)) %>% select(-country),
    by=c("buyer_country"="ISO")
  )
#Share of shareholders from EU blacklisted countries
BS<-BS %>% 
  rename(BS_total_score=`on total shareholders`) %>% 
  select(c(country_code,BS_total_score)) %>% na.omit()
TED_data<-TED_data %>% 
  left_join(BS, by=c("buyer_country"="country_code"))

#### Categorical columns to factors ####
cols <- c('mafia_dummy', 'nrb_cat', 'ca_procedure_simpl', 'relprice_cat', 
          'lcontractv_cat', 'is_local_perfomance', 'consortium',
          'ca_contract_type', 'cpv_div', 'ca_date_month', 'coast_region', 
          'mountain_region', 'relica_cat')
IT_data[,cols] <- lapply(IT_data[,cols], factor)
str(IT_data[,cols])

cols <- c('nrb_cat', 'ca_procedure_simpl', 'relprice_cat', 
          'lcontractv_cat', 'is_local_perfomance', 'consortium',
          'ca_contract_type', 'cpv_div', 'ca_date_month', 'coast_region', 
          'mountain_region')
TED_data[,cols] <- lapply(TED_data[,cols] , factor)
str(TED_data[,cols])

#### Table 2 ####
options(scipen=999) #remove scientific notation

t1<-IT_data %>% 
  filter(mafia_dummy!=9) %>% 
  group_by(mafia_dummy) %>% 
  mutate(`Number of obs.`=n()) %>% 
  mutate(`Mean contract value`=mean(ca_contract_value,na.rm=TRUE)) %>% 
  mutate(`Median contract value`=median(ca_contract_value,na.rm=TRUE)) %>% 
  mutate(`Min contract value`=min(ca_contract_value,na.rm=TRUE)) %>%
  mutate(`Max contract value`=max(ca_contract_value,na.rm=TRUE)) %>%
  ungroup() %>% 
  select(c(mafia_dummy,`Number of obs.`,`Mean contract value`,`Median contract value`,
           `Min contract value`,`Max contract value`)) %>% 
  distinct() %>% 
  reshape2::melt(id.vars="mafia_dummy")

t2<-IT_data %>% 
  filter(mafia_dummy!=9) %>% 
  group_by(mafia_dummy,cpv_div) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  group_by(mafia_dummy) %>% 
  mutate(value=(n/sum(n))*100) %>% 
  top_n(3) %>% 
  ungroup() %>% 
  select(-n) %>% 
  rename("variable"="cpv_div")

rbind(t1,t2) %>% 
  mutate(value=round(value,2)) %>% 
  gt::gt()

#### 30/70 split ####
data_mf <- IT_data %>%  filter(mafia_dummy==1|mafia_dummy==0)
set.seed(123)
smp <- floor(0.7 * nrow(data_mf))
train_ind <- sample(seq_len(nrow(data_mf)), size = smp)

#### Table 3 ####
train <- data_mf[train_ind, ]
test <- data_mf[-train_ind, ]
#removing empty levels
train <- droplevels(train)
test <- droplevels(test)
train$mafia_dummy<-as.factor(as.character(train$mafia_dummy))
test$mafia_dummy<-as.factor(as.character(test$mafia_dummy))

#model 1: number of bidders / single bidding alone
train <- within(train, ca_date_month <- relevel(ca_date_month, ref = 12))
test <- within(test, ca_date_month <- relevel(ca_date_month, ref = 12))
glm.fit.1 <- glm(mafia_dummy~ 
                   # red flags:
                   nrb_cat +  
                   # controls
                   anb_nrc_y + lcontractv_cat + is_local_perfomance + consortium + ca_contract_type + cpv_div + ca_date_month + 
                   #missing values
                   adm_err + 
                   # region related:
                   lpop_nuts + kmq_thousands + coast_region + mountain_region, 
                 data=train, family = binomial)
summary(glm.fit.1)
levels(test$cpv_div) <- levels(train$cpv_div)
glm.accuracy(test, 'mafia_dummy', glm.fit.1)

#model 2: procedure type
glm.fit.2 <- glm(mafia_dummy~ 
                   # tender related:
                   ca_procedure_simpl + 
                   # controls
                   anb_nrc_y + lcontractv_cat + is_local_perfomance + consortium + ca_contract_type + cpv_div + ca_date_month + 
                   #missing values
                   adm_err + 
                   # region related:
                   lpop_nuts + kmq_thousands + coast_region + mountain_region, 
                 data=train, family = binomial)
summary(glm.fit.2)
glm.accuracy(test, 'mafia_dummy', glm.fit.2)

#model 3
train <- within(train, relprice_cat <- relevel(relprice_cat, ref = 2))
test <- within(test, relprice_cat <- relevel(relprice_cat, ref = 2))
glm.fit.3 <- glm(mafia_dummy~ 
                   # red flags:
                   relprice_cat +  
                   # controls
                   anb_nrc_y + lcontractv_cat + is_local_perfomance + consortium + ca_contract_type + cpv_div + ca_date_month + 
                   #missing values
                   adm_err + 
                   # region related:
                   lpop_nuts + kmq_thousands + coast_region + mountain_region, 
                 data=train, family = binomial)
summary(glm.fit.3)
glm.accuracy(test, 'mafia_dummy', glm.fit.3)

#model 4
glm.fit.4 <- glm(mafia_dummy~ 
                   # red flags:
                   iwmshare_y +  
                   # controls
                   anb_nrc_y + lcontractv_cat + is_local_perfomance + consortium + ca_contract_type + cpv_div + ca_date_month + 
                   # cv_miss is included in lcontractv_cat
                   #missing values
                   adm_err + 
                   # region related:
                   lpop_nuts + kmq_thousands + coast_region + mountain_region, 
                 data=train, family = binomial)
summary(glm.fit.4)
glm.accuracy(test, 'mafia_dummy', glm.fit.4)

#model 5
glm.fit.5 <- glm(mafia_dummy~ 
                   # red flags:
                   relica_cat +  
                   # controls
                   anb_nrc_y + lcontractv_cat + is_local_perfomance + consortium + ca_contract_type +  
                   cpv_div + ca_date_month + 
                   #missing values
                   adm_err + 
                   # region related:
                   lpop_nuts + kmq_thousands + coast_region + mountain_region, 
                 data=train, family = binomial)
summary(glm.fit.5)
glm.accuracy(test, 'mafia_dummy', glm.fit.5)

#model 6: all predictors together
glm.fit.6 <- glm(mafia_dummy~ 
                   # red flags:
                   nrb_cat + ca_procedure_simpl + relprice_cat + iwmshare_y + relica_cat + 
                   # controls
                   anb_nrc_y + lcontractv_cat + is_local_perfomance + consortium + ca_contract_type + cpv_div + ca_date_month + 
                   #missing values
                   adm_err + 
                   # region related:
                   lpop_nuts + kmq_thousands + coast_region + mountain_region, 
                 data=train, family = binomial)
summary(glm.fit.6)
glm.accuracy(test, 'mafia_dummy', glm.fit.6)

#formatted output
stargazer::stargazer(glm.fit.1, glm.fit.2, glm.fit.3, glm.fit.4, glm.fit.5, glm.fit.6,
                     omit        = c('cpv_div', 'ca_date_month', 'ca_contract_type2', 'is_local_perfomance', 'consortium' ),
                     omit.labels = c('CPV division', "Contract award month", "Supply type", "Local performance", "Consortium"),
                     type = 'text', out = 'lr.html')

#### Table 4: Logistic regression accuracy ####
glm.fit <- glm(mafia_dummy~ 
                 # red flags:
                 nrb_cat + ca_procedure_simpl + relprice_cat + 
                 iwmshare_y + relica_cat +
                 # controls
                 anb_nrc_y + lcontractv_cat + is_local_perfomance + 
                 consortium + ca_contract_type +cpv_div + ca_date_month + 
                 #missing values
                 adm_err + 
                 # regional variables:
                 lpop_nuts + kmq_thousands + coast_region + mountain_region, 
               data=train, family = binomial)
summary(glm.fit)
glm.accuracy(test, 'mafia_dummy', glm.fit)

#### Table 4: Random Forest accuracy ####
train <- data_mf[train_ind, ]
test <- data_mf[-train_ind, ]
#remove empty levels from factor variables before fitting the model
train <- droplevels(train)
test <- droplevels(test)
train$mafia_dummy<-as.factor(as.character(train$mafia_dummy))
test$mafia_dummy<-as.factor(as.character(test$mafia_dummy))

#vector of predictors
features <- Hmisc::Cs(nrb_cat,
                      ca_procedure_simpl,
                      relprice_cat,
                      relica_cat,
                      iwmshare_y,
                      anb_nrc_y,
                      lcontractv_cat,
                      is_local_perfomance,
                      consortium,
                      ca_contract_type,
                      cpv_div,
                      ca_date_month,
                      adm_err,
                      lpop_nuts,
                      kmq_thousands,
                      coast_region,
                      mountain_region)
#model
RF_model <- randomForest(mafia_dummy~., 
                         data = train %>% select(c(features, mafia_dummy)), 
                         mtry=6, ntree=1000)
levels(test$cpv_div) <- levels(train$cpv_div)
pred <- predict(RF_model, test)
confusionMatrix(pred, test$mafia_dummy) 

#### Table 4: Boosting accuracy ####
train <- data_mf[train_ind, ]
test <- data_mf[-train_ind, ]
#we remove empty levels from factor variables before fitting the model
train <- droplevels(train)
test <- droplevels(test)
# GBM requires the response variable to be coded as numeric 0/1.
train$mafia_dummy<-as.numeric(as.character(train$mafia_dummy))
test$mafia_dummy<-as.numeric(as.character(test$mafia_dummy))
class(train$mafia_dummy)
class(test$mafia_dummy)
#model
gbm.fit <- gbm(
  mafia_dummy~nrb_cat + ca_procedure_simpl + relprice_cat + iwmshare_y +
    relica_cat + anb_nrc_y + lcontractv_cat + is_local_perfomance + 
    consortium + ca_contract_type + cpv_div + ca_date_month + adm_err + 
    lpop_nuts + kmq_thousands + coast_region + mountain_region,
  data = train,
  distribution = "bernoulli",
  n.trees = 400,
  interaction.depth = 5,
  shrinkage = 0.1, 
  bag.fraction = .75,
  n.cores = NULL,
  verbose = FALSE
)  
preds <- predict(gbm.fit,n.trees=400, newdata=test, type='response')
preds2 <- ifelse(preds > 0.5, "1", "0")
confusionMatrix(as.factor(preds2),as.factor(test$mafia_dummy))$overall[1] 

#### Figure 2 ####
summary(gbm.fit)
dev.off()

#### Figure 3 ####
#Panel A: number of bids
plotdata<-plot.gbm(gbm.fit, i="nrb_cat", type="response", return.grid=TRUE)
barplot(plotdata$y, names.arg = plotdata$nrb_cat, ylab="EGO presence probability", xlab="bidder number categories")

#Panel B: procedure type
plotdata<-plot.gbm(gbm.fit, i="ca_procedure_simpl", type="response", return.grid=TRUE)
barplot(plotdata$y, names.arg = plotdata$ca_procedure_simpl, ylim = c(0, 0.2), ylab="EGO presence probability", xlab="procedure type")

#Panel C: relative price
plotdata<-plot.gbm(gbm.fit, i="relprice_cat", type="response", return.grid=TRUE)
barplot(plotdata$y, names.arg = plotdata$relprice_cat, ylim = c(0, 0.2), ylab="EGO presence probability", xlab="relative price categories")

#Panel D: cost overrun
plotdata<-plot.gbm(gbm.fit, i="relica_cat", type="response", return.grid=TRUE)
barplot(plotdata$y, names.arg = plotdata$relica_cat, ylim = c(0, 0.2), ylab="EGO presence probability", xlab="cost overrun categories")

#Panel E: share of supplier in buyer's spend
plotdata<-plot.gbm(gbm.fit, i="iwmshare_y", type="response", return.grid=TRUE)
plot(plotdata, type="l", ylab="EGO presence probability", ylim = c(0, 0.7), xlab="Share of supplier in buyer's annual spending")

dev.off()

#### Prediction on IT PP data using logistic regression model ####
#remove missing values
data_pred <- IT_data %>% select(c(id,features))%>% na.omit()
nrow(data_pred)/nrow(IT_data) #-2.5% 
#Adding levels of the factor variable CPV division to the model
glm.fit$xlevels[["cpv_div"]] <- union(glm.fit$xlevels[["cpv_div"]], levels(data_pred$cpv_div))
EGO_prob_glm <- predict(glm.fit,newdata=data_pred %>% select(features), type='response')
data_pred$EGO_prob_glm<-EGO_prob_glm

#### Prediction on IT PP data using Boosting model ####
EGO_prob_GBM <- predict(gbm.fit,n.trees=400, newdata=data_pred %>% select(features), type='response')
data_pred$EGO_prob_GBM<-EGO_prob_GBM

#### Prediction on IT PP data using Random Forest model ####
#In order to make predictions on the full Italian sample, we fit the same RF model without dropping empty levels.
train <- data_mf[train_ind, ] %>% select(c(features, mafia_dummy))
train$mafia_dummy <- factor(as.character(train$mafia_dummy))
RF_model <- randomForest(mafia_dummy~.,
                         data = train,
                         mtry=6, ntree=1000)
EGO_prob_RF <- predict(RF_model, data_pred, type = 'prob')
data_pred$EGO_prob_RF <- EGO_prob_RF[ ,2]

#### Table 4: Validity test using IT PP data and Transcrime mafia index ####
#preparing data
data_pred<-data_pred %>% 
  left_join(
    IT_data %>% select(c(id,comune,provincia,mafia_index)),
    by="id"
    )

#logistic regression
tmp<-data_pred %>% 
  filter(comune==1) %>%
  group_by(provincia) %>% 
  mutate(OC=mean(EGO_prob_glm,na.rm=TRUE)) %>% 
  ungroup() %>% 
  select(c(provincia,mafia_index,OC)) %>% distinct() %>% 
  na.omit() 
cor<-round(cor(tmp$OC,tmp$mafia_index),2)
star<-gtools::stars.pval(cor.test(tmp$OC,tmp$mafia_index)$p.value)[1]
tibble(cor,star)

#Random forest
tmp<-data_pred %>% 
  filter(comune==1) %>%
  group_by(provincia) %>% 
  mutate(OC=mean(EGO_prob_RF,na.rm=TRUE)) %>% 
  ungroup() %>% 
  select(c(provincia,mafia_index,OC)) %>% distinct() %>% 
  na.omit() 
cor<-round(cor(tmp$OC,tmp$mafia_index),2)
star<-gtools::stars.pval(cor.test(tmp$OC,tmp$mafia_index)$p.value)[1]
tibble(cor,star)

#Boosting
tmp<-data_pred %>% 
  filter(comune==1) %>%
  group_by(provincia) %>% 
  mutate(OC=mean(EGO_prob_GBM,na.rm=TRUE)) %>% 
  ungroup() %>% 
  select(c(provincia,mafia_index,OC)) %>% distinct() %>% 
  na.omit() 
cor<-cor(tmp$OC,tmp$mafia_index)
star<-gtools::stars.pval(cor.test(tmp$OC,tmp$mafia_index)$p.value)[1]
tibble(cor,star)

#### Contract-level prediction on TED data using logistic regression model ####
#List of features without cost overrun
features_2 <- Hmisc::Cs(nrb_cat,
                        ca_procedure_simpl,
                        relprice_cat,
                        iwmshare_y,
                        anb_nrc_y,
                        lcontractv_cat,
                        is_local_perfomance,
                        consortium,
                        ca_contract_type,
                        cpv_div,
                        ca_date_month,
                        adm_err,
                        lpop_nuts,
                        kmq_thousands,
                        coast_region,
                        mountain_region)
TED_data_pred <- TED_data %>% select(c(tender_id,features_2)) %>% distinct() 
#In order to make predictions on TED data, we fit the model without cost overrun variable (relica_cat).
train <- data_mf[train_ind, ] 
train <- droplevels(train)
train <- train %>% select(c(features_2, mafia_dummy))
train$mafia_dummy<-as.factor(as.character(train$mafia_dummy))
glm.fit <- glm(mafia_dummy~ 
                 nrb_cat + ca_procedure_simpl + relprice_cat + iwmshare_y + 
                 anb_nrc_y + lcontractv_cat + is_local_perfomance + 
                 consortium + ca_contract_type +cpv_div + ca_date_month + 
                 adm_err + lpop_nuts + kmq_thousands + coast_region + 
                 mountain_region, data=train, family = binomial)
#adding missing levels of the factor variable CPV division to the model
glm.fit$xlevels[["cpv_div"]] <- union(glm.fit$xlevels[["cpv_div"]], levels(TED_data_pred$cpv_div))
glm.fit$xlevels[["consortium"]] <- union(glm.fit$xlevels[["consortium"]], levels(TED_data_pred$consortium))
glm.fit$xlevels[["ca_contract_type"]] <- union(glm.fit$xlevels[["ca_contract_type"]], levels(TED_data_pred$ca_contract_type))
EGO_prob_glm <- predict(glm.fit,newdata=TED_data_pred %>% select(features_2), type='response')
TED_data_pred$EGO_prob_glm<-EGO_prob_glm

#### Contract-level prediction on TED data using Boosting model ####
#In order to make predictions on TED data, we fit the model without cost overrun variable (relica_cat).
train <- data_mf[train_ind, ] 
train <- droplevels(train)
train$mafia_dummy<-as.numeric(as.character(train$mafia_dummy))
class(train$mafia_dummy)
#In order to make predictions on TED data, we fit the Boosting model without cost overrun variable (relica_cat). Also, we add levels of factor variables from the TED data to the train dataset.
gbm.fit <- gbm(
  mafia_dummy~nrb_cat + ca_procedure_simpl + relprice_cat + iwmshare_y +
    anb_nrc_y + lcontractv_cat + is_local_perfomance + 
    consortium + ca_contract_type + cpv_div + ca_date_month + adm_err + 
    lpop_nuts + kmq_thousands + coast_region + mountain_region,
  data = train,
  distribution = "bernoulli",
  n.trees = 400,
  interaction.depth = 5,
  shrinkage = 0.1, 
  bag.fraction = .75,
  n.cores = NULL,
  verbose = FALSE
)  
EGO_prob_GBM <- predict(gbm.fit,n.trees=400, newdata=TED_data_pred %>% select(features_2), type='response')
TED_data_pred$EGO_prob_GBM<-EGO_prob_GBM

#### Contract-level prediction on TED data using Random Forest model ####
#In order to make predictions on TED data, we fit the model without cost overrun variable (relica_cat). Also, we add levels of factor variables from the TED data to the train dataset.
train <- data_mf[train_ind, ] 
train <- droplevels(train)
train$consortium <- factor(train$consortium, levels=levels(TED_data_pred$consortium))
train$cpv_div <- factor(train$cpv_div, levels=levels(TED_data_pred$cpv_div))
train$ca_contract_type <- factor(train$ca_contract_type, levels=levels(TED_data_pred$ca_contract_type))
train$mafia_dummy <- factor(as.character(train$mafia_dummy))
train <- train %>% select(c(features_2, mafia_dummy))
RF_model <- randomForest(mafia_dummy~., 
                         data = train, 
                         mtry=6, ntree=1000)
TED_data_pred$ca_procedure_simpl <- factor(TED_data_pred$ca_procedure_simpl, levels=levels(train$ca_procedure_simpl))
EGO_prob_RF <- predict(RF_model, TED_data_pred %>% select(features_2), type = 'prob')
TED_data_pred$EGO_prob_RF <- EGO_prob_RF[ ,2]

#### Table 4: Validity test using TED data and Financial Secrecy Index ####
#preparing data
TED_data_pred<-TED_data_pred %>% 
  left_join(
    TED_data %>% 
      select(c(tender_id,buyer_country,FSI_score,BS_total_score)) %>% 
      distinct(),
    by="tender_id"
    )

#Logistic regression
tmp<-TED_data_pred %>% 
  group_by(buyer_country) %>% 
  mutate(mean_prob=mean(EGO_prob_glm,na.rm=TRUE))%>% 
  ungroup() %>% 
  select(c(buyer_country,mean_prob,FSI_score)) %>% distinct() %>% 
  na.omit()
cor<-round(cor(tmp$mean_prob,tmp$FSI_score),2)
star<-gtools::stars.pval(cor.test(tmp$mean_prob,tmp$FSI_score)$p.value)[1]
tibble(cor,star)

#Random Forest
tmp<-TED_data_pred %>% 
  group_by(buyer_country) %>% 
  mutate(mean_prob=mean(EGO_prob_RF,na.rm=TRUE))%>% 
  ungroup() %>% 
  select(c(buyer_country,mean_prob,FSI_score)) %>% distinct() %>% 
  na.omit()
cor<-round(cor(tmp$mean_prob,tmp$FSI_score),2)
star<-gtools::stars.pval(cor.test(tmp$mean_prob,tmp$FSI_score)$p.value)[1]
tibble(cor,star)

#Boosting
tmp<-TED_data_pred %>% 
  group_by(buyer_country) %>% 
  mutate(mean_prob=mean(EGO_prob_GBM,na.rm=TRUE))%>% 
  ungroup() %>% 
  select(c(buyer_country,mean_prob,FSI_score)) %>% distinct() %>% 
  na.omit()
cor<-round(cor(tmp$mean_prob,tmp$FSI_score),2)
star<-gtools::stars.pval(cor.test(tmp$mean_prob,tmp$FSI_score)$p.value)[1]
tibble(cor,star)

#### Table 4: Validity test using TED data and share of shareholders from EU blacklisted countries ####
#logistic regression
tmp<-TED_data_pred %>% 
  group_by(buyer_country) %>% 
  mutate(mean_prob=mean(EGO_prob_glm,na.rm=TRUE))%>% 
  ungroup() %>% 
  select(c(buyer_country,mean_prob,BS_total_score)) %>% distinct() %>% 
  na.omit()
cor<-round(cor(tmp$mean_prob,tmp$BS_total_score),2)
star<-gtools::stars.pval(cor.test(tmp$mean_prob,tmp$BS_total_score)$p.value)[1]
tibble(cor,star)

#Random Forest
tmp<-TED_data_pred %>% 
  group_by(buyer_country) %>% 
  mutate(mean_prob=mean(EGO_prob_RF,na.rm=TRUE))%>% 
  ungroup() %>% 
  select(c(buyer_country,mean_prob,BS_total_score)) %>% distinct() %>% 
  na.omit()
cor<-round(cor(tmp$mean_prob,tmp$BS_total_score),2)
star<-gtools::stars.pval(cor.test(tmp$mean_prob,tmp$BS_total_score)$p.value)[1]
tibble(cor,star)

# Boosting
tmp<-TED_data_pred %>% 
  group_by(buyer_country) %>% 
  mutate(mean_prob=mean(EGO_prob_GBM,na.rm=TRUE))%>% 
  ungroup() %>% 
  select(c(buyer_country,mean_prob,BS_total_score)) %>% distinct() %>% 
  na.omit()
cor<-round(cor(tmp$mean_prob,tmp$BS_total_score),2)
star<-gtools::stars.pval(cor.test(tmp$mean_prob,tmp$BS_total_score)$p.value)[1]
tibble(cor,star)
