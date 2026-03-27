### Replication code
### Article: "How patronage delivers: Political appointments, bureaucratic accountability, and service delivery in Brazil"
### Author: Guillermo Toral (www.guillermotoral.com)
### Date: June 2022
### This file implements descriptive analyses, generating plots and tables
### R version, platform, and package versions reported at the end of the file

# Prepare the environment -------------------------------------------------

### This section of the code prepares the environment 

# Clean the environment
rm(list = ls())

# Install required packages if not previously installed
package_list <- c("tidyverse", "timelineS", "texreg", "lmtest", "sandwich", "lfe", "readstata13", "readxl", "here") 
packages_to_install <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(packages_to_install)>0){
  install.packages(packages_to_install)
}

# Load required packages
library(tidyverse)
library(timelineS)
library(texreg)
library(lmtest)
library(sandwich)
library(lfe)
library(readstata13)
library(readxl)
library(here)

# Set Working Directory to wherever this file is located.
setwd(here())
# The directory where the code folder is located must also have a "plots" and a "tables" subdirectory

# Extract statistics mentioned in the text: Municipal spending in education, healthcare, and social assistance ------------------------------------------

# Import data on municipal spending, from the National Treasury
s <- read.delim("../../datasets/downloaded/tesouro_nacional/municipal_spending_2017.csv", sep=";", fileEncoding="latin1", skip=3)

# Clean amounts so that they're numeric values
s$Valor <- as.numeric(gsub(",", ".", s$Valor))

education_spending <- s %>%
  mutate(cod_conta_short = substr(Conta,1,2),
         cod_conta_long = substr(Conta,1,6)) %>%
  filter(Coluna=="Despesas Pagas" & cod_conta_long=="12 - E") %>% # "Despesas pagas" = paid expenses, "12 - E" corresponds to education
  mutate(spending_education_2017 = as.numeric(Valor)) %>%
  dplyr::select(Cod.IBGE, spending_education_2017) 

healthcare_spending <- s %>%
  mutate(cod_conta_short = substr(Conta,1,2),
         cod_conta_long = substr(Conta,1,6)) %>%
  filter(Coluna=="Despesas Pagas" & cod_conta_long=="10 - S") %>% # "Despesas pagas" = paid expenses, "10 - S" corresponds to healthcare
  mutate(spending_healthcare_2017 = as.numeric(Valor)) %>%
  dplyr::select(Cod.IBGE, spending_healthcare_2017) 

socialassistance_spending <- s %>%
  mutate(cod_conta_short = substr(Conta,1,2),
         cod_conta_long = substr(Conta,1,6)) %>%
  filter(Coluna=="Despesas Pagas" & cod_conta_long=="08 - A") %>% # "Despesas pagas" = paid expenses, "08 - A" corresponds to social assistance
  mutate(spending_socialassistance_2017 = as.numeric(Valor)) %>%
  dplyr::select(Cod.IBGE, spending_socialassistance_2017) 

total_spending <- s %>%
  mutate(cod_conta_short = substr(Conta,1,2),
         cod_conta_long = substr(Conta,1,6)) %>%
  filter(Coluna=="Despesas Pagas" & substr(Conta,1,15)=="Despesas Exceto") %>% # "Despesas pagas" = paid expenses, "Despesas Exceto" corresponds to all expenses ("Despesas Exceto Intraorçamentárias", i.e. Spending except intra-budgetary)
  mutate(spending_total_2017 = as.numeric(Valor)) %>%
  dplyr::select(Cod.IBGE, spending_total_2017) 

# Merge all spending datasets
ms <- left_join(total_spending, education_spending)
ms <- left_join(ms, healthcare_spending)
ms <- left_join(ms, socialassistance_spending)
ms$spending_education_2017_pc <- ms$spending_education_2017/ms$spending_total_2017
ms$spending_healthcare_2017_pc <- ms$spending_healthcare_2017/ms$spending_total_2017
ms$spending_socialassistance_2017_pc <- ms$spending_socialassistance_2017/ms$spending_total_2017
ms$spending_socialareas_pc <- (ms$spending_education_2017+ms$spending_healthcare_2017+ms$spending_socialassistance_2017)/ms$spending_total_2017

# Extract mean of municipal spending in education + healthcare + social assistance
summary(ms$spending_socialareas_pc) # 60%

# Extract statistics mentioned in the text: Median municipality population --------

# Import data with municipality covariates, from Base dos dados
d <- read_csv("../../datasets/downloaded/basedosdados/basedosdados_municipality_covariates.csv")

# Select data 2017 (median is similar for other years)
p <- d %>%
  filter(year==2017)

summary(p$population) # Median population < 12,000 people

# Generate Figure 9 and extract statistics mentioned in the text: Size of the municipal government workforce ----------------------------------------

# Load data on 2016 workforce by municipality
# This dataset was built using restricted-access, individually-identified data from the Ministry of the Economy which reports data on all formal-sector workers in the country (RAIS)
# The original, restricted-access dataset can only be obatained through partnership with the Ministry of the Economy (see information at https://www.gov.br/pt-br/servicos/solicitar-acesso-aos-dados-identificados-rais-e-caged)
# Using those files and their individual unique identifiers (CPF), I generated municipality-level counts of unique municipal employees and unique formal-sector employees in 2016
# The file "formal_vs_municipal_employees_2016.csv" has those counts. 

w <- read_csv("../../datasets/analysis/other/formal_vs_municipal_employees_2016.csv")

# Generate count of municipal employees as share of formal-sector employees
w$cpf_government_share_formal <- w$total_cpf_government/w$total_cpf_formal

# Load municipal-level covariates and extract population for 2016
m <- read_csv("../../datasets/downloaded/basedosdados/basedosdados_municipality_covariates.csv")
municipality_population_2016 <- m %>%
  dplyr::filter(year==2016) %>%
  mutate(cod_ibge = as.numeric(substr(ibge,1,6))) %>%
  dplyr::select(cod_ibge, population)

w <- left_join(w, municipality_population_2016)
w$cpf_government_share_population <- w$total_cpf_government/w$population

pdf("../../plots/municipal_employees_share_of_formal_labor_force.pdf",width=9,height=4)
par(mfrow=c(1,2))
plot(density(w$cpf_government_share_population,na.rm=T),main="", col="blue",xlab="Municipal employees as \n share of the local population",lwd=2)
grid()
lines(density(w$cpf_government_share_population,na.rm=T),main="", col="blue",xlab="Municipal employees as \n share of the local population",lwd=2)
plot(density(w$cpf_government_share_formal,na.rm=T),main="",col="blue",xlab="Municipal employees as \n share of the local formal workforce",lwd=2)
grid()
lines(density(w$cpf_government_share_formal,na.rm=T),main="",col="blue",xlab="Municipal employees as \n share of the local formal workforce",lwd=2)
dev.off()

# Average share of population hired by municipal governments
summary(w$cpf_government_share_population) # 4.7%

# Average share of formal employment force hired by municipal governments
summary(w$cpf_government_share_formal) # 38.2%

# Generate Figure 10: World Management Survey plots -------------------------------------------

# Data on hospitals in Brazil -- this file was obtained through a data request to the team of the World Management Survey
# The dataset was received with hospital identifiers, but in agreement with the World Management Survey team I am including a de-identified version of the dataset in the replication package, which I produced with the code below

# d <- read.dta13("../../datasets/downloaded/bloom_et_al/wms_gtoral.dta", generate.factors=T, convert.factors=T)
# # Create indicators for whether a hospital is managed by a municipal or state government, by relying on hospital names
# d$municipal <- c()
# d$estadual <- c()
# for(i in 1:nrow(d)){
#   d$municipal[i] <- ifelse(grepl("MUNICIPAL",d$hospital_name[i]) | grepl("MUN ",d$hospital_name[i]),1,0)
#   d$estadual[i] <- ifelse(grepl("ESTADUAL",d$hospital_name[i]) | grepl("REGIONAL",d$hospital_name[i]),1,0)
# }
# # Select the columns I use for this analysis
# d <- d %>%
#   dplyr::select(cty, hospital_id, municipal, estadual, management)
# # Export
# save.dta13(d, "../../datasets/downloaded/bloom_et_al/wms_brazil_hospitals.dta")

# Import the anonymized dataset
d <- read.dta13("../../datasets/downloaded/bloom_et_al/wms_brazil_hospitals.dta")

# Split dataset for municipal (dm), state (de), and private (dp) hospitals
dm <- d[which(d$municipal==1),]
de <- d[which(d$estadual==1),]
dp <- d[which(d$municipal==0 & d$estadual==0),]

# Data on high schools -- file is from the replication package of a paper published by Bloom et al 
s <- read.dta13("../../datasets/downloaded/bloom_et_al/paper_schools.dta")

# Split dataset for schools in Brazil (sb), public (sbpub), and private (sbpri)
sb <- s[which(s$country=="Brazil"),]
sbpub <- sb[which(sb$om_pri==0),]
sbpri <- sb[which(sb$om_pri==1),]

# Split dataset for schools in the US (usa), public (usapub), and private (usapri)
usa <- s[which(s$country=="US"),]
usapub <- usa[which(sb$om_pri==0),]
usapri <- usa[which(sb$om_pri==1),]

pdf("../../plots/managementscores_BR_USA.pdf", width=9, height=3)
par(mfrow=c(1,3))
plot(density(dm$management),xlim=c(0.4,4.8),ylim=c(0,1), xlab="", main="Hospitals in Brazil", lty=1, lwd=2)
lines(density(de$management), lty=2, lwd=2)
lines(density(dp$management), lty=3, lwd=1)
legend("topright", lty=c(1,2,3), lwd=c(2,2,1), c("Municipal", "State", "Private"), bty="n")
plot(density(sbpub$management),xlim=c(0.4, 4.8), ylim=c(0,1),xlab="", main="High schools in Brazil", lty=1, lwd=2)
lines(density(sbpri$management), lty=2, lwd=1)
legend("topright", lty=c(1,2), lwd=c(2,1), c("Public", "Private"), bty="n")
plot(density(usapub$management,na.rm=T),xlim=c(0.4, 4.8), ylim=c(0,1),xlab="", main="High schools in the USA", lty=1, lwd=2)
lines(density(usapri$management,na.rm=T), lty=2, lwd=1)
legend("topright", lty=c(1,2), lwd=c(2,1), c("Public", "Private"), bty="n")
dev.off()

# Generate Table 5: Predictors of director appointment mode and school quality -----------------------------------------

# Load data built for the RDD, which has school director appointment modes and school quality scores for 2013, together with covariates
d <- read_csv("../../datasets/analysis/rdd/rdd_analysis.csv")

# Run regressions where the DV is different appointment modes
m1 <- lm(director_appointed ~ # Director covariates
           female + age_forties + age_above50 + race_white + race_black_or_brown + schooling_tertiary + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + teacher_experience_over15yr + director_experience_3to10yr + director_experience_over10yr + director_here_3to10yr + director_here_over10yr, 
         data=d)

m2 <- lm(director_appointed ~ # Director & municipality covariates
           female + age_forties + age_above50 + race_white + race_black_or_brown + schooling_tertiary + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + teacher_experience_over15yr + director_experience_3to10yr + director_experience_over10yr + director_here_3to10yr + director_here_over10yr
         + log_gdp_percapita + log_population + deaths_perthousand + mayor_first_term + electoral_concentration,
         data=d)

m3 <- lm(director_appointed ~ # Director & municipality & school covariates
           female + age_forties + age_above50 + race_white + race_black_or_brown + schooling_tertiary + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + teacher_experience_over15yr + director_experience_3to10yr + director_experience_over10yr + director_here_3to10yr + director_here_over10yr
         + log_gdp_percapita + log_population + deaths_perthousand + mayor_first_term + electoral_concentration
         + rural + log_number_workers + students_per_classroom + school_inse
         , data=d)

m4 <- lm(director_elected ~ # Director & municipality & school covariates
           female + age_forties + age_above50 + race_white + race_black_or_brown + schooling_tertiary + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + teacher_experience_over15yr + director_experience_3to10yr + director_experience_over10yr + director_here_3to10yr + director_here_over10yr
         + log_gdp_percapita + log_population + deaths_perthousand + mayor_first_term + electoral_concentration
         + rural + log_number_workers + students_per_classroom + school_inse
         , data=d)

m5 <- lm(director_civil_service ~ # Director & municipality & school covariates
           female + age_forties + age_above50 + race_white + race_black_or_brown + schooling_tertiary + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + teacher_experience_over15yr + director_experience_3to10yr + director_experience_over10yr + director_here_3to10yr + director_here_over10yr
         + log_gdp_percapita + log_population + deaths_perthousand + mayor_first_term + electoral_concentration
         + rural + log_number_workers + students_per_classroom + school_inse
         , data=d)

# Run regressions where the DV is the school's IDEB score
# For these regressions, restrict comparisons to schools with either appointed, elected, or civil service for clarity
dd <- subset(d,d$director_appointed==1 | d$director_elected==1 | d$director_civil_service==1)

i1 <- lm(ideb_cont ~ # Director covariates
           director_appointed + director_elected + female + age_forties + age_above50 + race_white + race_black_or_brown + schooling_tertiary + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + teacher_experience_over15yr + director_experience_3to10yr + director_experience_over10yr + director_here_3to10yr + director_here_over10yr
         , data=dd)

i2 <- lm(ideb_cont ~ # Director & municipality covariates
           director_appointed + director_elected + female + age_forties + age_above50 + race_white + race_black_or_brown + schooling_tertiary + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + teacher_experience_over15yr + director_experience_3to10yr + director_experience_over10yr + director_here_3to10yr + director_here_over10yr
         + log_gdp_percapita + log_population + deaths_perthousand + mayor_first_term + electoral_concentration
         , data=dd)

i3 <- lm(ideb_cont ~ # Director & municipality & school covariates
           director_appointed + director_elected + female + age_forties + age_above50 + race_white + race_black_or_brown + schooling_tertiary + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + teacher_experience_over15yr + director_experience_3to10yr + director_experience_over10yr + director_here_3to10yr + director_here_over10yr
         + log_gdp_percapita + log_population + deaths_perthousand + mayor_first_term + electoral_concentration
         + rural + log_number_workers + students_per_classroom + school_inse
         , data=dd)

i4 <- lm(ideb_cont ~ # Director & municipality & school covariates & IDEB target
           director_appointed + director_elected + female + age_forties + age_above50 + race_white + race_black_or_brown + schooling_tertiary + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + teacher_experience_over15yr + director_experience_3to10yr + director_experience_over10yr + director_here_3to10yr + director_here_over10yr
         + log_gdp_percapita + log_population + deaths_perthousand + mayor_first_term + electoral_concentration
         + rural + log_number_workers + students_per_classroom + school_inse + ideb_target
         , data=dd)

i5 <- felm(ideb_cont ~ # Director & school covariates & IDEB target & municipality fixed effects
             director_appointed + director_elected + female + age_forties + age_above50 + race_white + race_black_or_brown + schooling_tertiary + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + teacher_experience_over15yr + director_experience_3to10yr + director_experience_over10yr + director_here_3to10yr + director_here_over10yr
           # + log_gdp_per_capita + log_population + deaths_per_thousand + mayor_first_term + electoral_concentration
           + rural + log_number_workers + students_per_classroom + school_inse + ideb_target
           | cod_ibge
           , data=dd)

# Create and export regression table
texreg(list(m1,m2,m3,m4,m5,i1,i2,i3,i4,i5),
       file="../../tables/predictors_appointed_ideb.tex",table=F,
       digits=3,
       override.se = list(coeftest(m1,vcov=vcovHC(m1,type="HC1"))[,2],coeftest(m2,vcov=vcovHC(m2,type="HC1"))[,2],coeftest(m3,vcov=vcovHC(m3,type="HC1"))[,2],coeftest(m4,vcov=vcovHC(m4,type="HC1"))[,2],coeftest(m5,vcov=vcovHC(m5,type="HC1"))[,2],coeftest(i1,vcov=vcovHC(i1,type="HC1"))[,2],coeftest(i2,vcov=vcovHC(i2,type="HC1"))[,2],coeftest(i3,vcov=vcovHC(i3,type="HC1"))[,2],coeftest(i4,vcov=vcovHC(i4,type="HC1"))[,2],coeftest(i5,vcov=vcovHC(i5,type="HC1"))[,2]),
       override.pvalues = list(coeftest(m1,vcov=vcovHC(m1,type="HC1"))[,4],coeftest(m2,vcov=vcovHC(m2,type="HC1"))[,4],coeftest(m3,vcov=vcovHC(m3,type="HC1"))[,4],coeftest(m4,vcov=vcovHC(m4,type="HC1"))[,4],coeftest(m5,vcov=vcovHC(m5,type="HC1"))[,4],coeftest(i1,vcov=vcovHC(i1,type="HC1"))[,4],coeftest(i2,vcov=vcovHC(i2,type="HC1"))[,4],coeftest(i3,vcov=vcovHC(i3,type="HC1"))[,4],coeftest(i4,vcov=vcovHC(i4,type="HC1"))[,4],coeftest(i5,vcov=vcovHC(i5,type="HC1"))[,4]),
       custom.header = list("Appointed" = 1:3, "Elected"=4, "Civil service"=5, "IDEB score"=6:10),
       custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)", "(10)"),
       groups = list("Director covariates" = 2:15, "Municipality covariates" = 16:20, "School covariates" = 21:27),
       custom.coef.names = c("Intercept",
                             "Female", 
                             "Age 40-49 (vs <40)",
                             "Age 50+ (vs <40)",
                             "White (vs other race)",
                             "Black/brown (vs other race)",
                             "Tertiary degree (vs < tertiary)",
                             "Postgraduate degree (vs < tertiary)",
                             "No other jobs",
                             "6-15 years of teaching exp. (vs <6)",
                             ">15 years of teaching exp. (vs <6)",
                             "3-10 years of director exp. (vs <3)",
                             ">10 years of director exp. (vs <3)",
                             "3-10 years as director of school (vs <3)",
                             ">10 years as director of school (vs <3)",
                             "GDP per capita (logged)",
                             "Population (logged)",
                             "Number of deaths per 1,000",
                             "Mayor is in first term",
                             "Electoral concentration",
                             "Rural",
                             "Number of staff (logged)",
                             "Students per classroom (average)",
                             "School socioeconomic index",
                             "Director is appointed (vs civil service)",
                             "Director is elected (vs civil service)",
                             "IDEB target"),
       custom.gof.rows = list("Municipality fixed effects" = c(rep("No",9),"Yes")),
       custom.note = c("%stars. HC1 standard errors in brackets"),
       include.rsquared = F, include.adjrs = T, include.nobs = T, include.groups=F,
       booktabs=TRUE,use.packages=FALSE,
       stars = c(0.001, 0.01, 0.05))

# Generate Figure 12: Timeline of IDEB tests and information release -----------------------------------------------------------

d <- as.data.frame(cbind(c("2008 \n municipal \n elections",
                           "2009 \n Prova Brasil \n (tests & surveys)", 
                           "IDEB \n published", 
                           "2011 \n Prova Brasil\n (tests & surveys)",
                           "IDEB \n published", 
                           
                           "2012 \n municipal \n elections", 
                           "2013 \n Prova Brasil \n (tests & surveys)", 
                           "IDEB \n published", 
                           "2015 \n Prova Brasil\n (tests & surveys)", 
                           "IDEB \n published",
                           
                           "2016 \n municipal \n elections",
                           "2017 \n Prova Brasil \n (tests & surveys)", 
                           "IDEB \n published"),
                         
                         c("2008-10-05", "2009-10-19", "2010-07-01", "2011-11-07", "2012-08-14",
                           "2012-10-07", "2013-11-11", "2014-09-05", "2015-11-09", "2016-09-08", 
                           "2016-10-02", "2017-10-23", "2018-09-04")))

d$V2 <- as.Date(d$V2)
d <- d[order(d$V2),]

pdf("../../plots/ideb_timeline.pdf",width=9,height=6)
par(mar=c(0.2,0.2,0.2,0.2))
timelineS(d, scale = "year",
          line.width = 7,line.color="blue",
          #  scale.font=c(2,2,1,1,2,2,1,1,2,2),
          # label.position=c("down", "down", "down", "down", "down", "up",  "up",  "up",  "up",  "up"),
          # label.length=c(.5,.8,.8,.5,,.5,1,1,1,1,1),
          label.position = c(4,3,3,3,3,4,3,3,3,3,4,3,3,3,3),
          label.direction=c("up"),
          label.cex = c(rep(0.9,12)),
          label.length=c(.2, .3, .5, .3, .5, .2, .3, .5, .3, .5, .2, .3, .5, .3, .5),
          labels=d$V1, buffer.days=200,
          label.color=c(rep("blue",6)),
          point.color=c(rep("blue",6)))
dev.off()

# Generate Figure 13: Characteristics of fieldwork locations --------------

# Import data with municipality human development indicators, from Base dos Dados
h <- read_csv("../../datasets/downloaded/basedosdados/basedosdados_municipality_humandevelopment.csv")

h <- h %>%
  dplyr::filter(year==2010) %>%
  mutate(cod_ibge = as.numeric(substr(ibge,1,6)))

# Import and merge data on municipality-level school quality scores
ideb <- data.frame(read_excel("../../datasets/downloaded/inep/ideb/divulgacao_anos_iniciais_municipios_2019.xlsx",skip=9)) 

ideb <- ideb %>%
  dplyr::filter(REDE == "Municipal") %>%
  mutate(cod_ibge = as.numeric(substr(CO_MUNICIPIO,1,6)),
         ideb = as.numeric(VL_INDICADOR_REND_2015)*as.numeric(VL_NOTA_MEDIA_2015)) %>%
  dplyr::select(cod_ibge, ideb)

h <- left_join(h, ideb)

# Import and merge data on municipal election outcomes
e <- read_csv("../../datasets/analysis/other/municipal_elections_2016.csv")

h <- left_join(h, e)

# Municipality codes of interview locations, by state
ceara <- c(230440, 230300, 230280, 230763, 230240, 231050, 231320, 231290, 230800, 230470, 230725)
riodejaneiro <- c(330455, 330270, 330550, 330180, 330190, 330280, 330360, 330400)
minasgerais <- c(317040, 314700, 313360, 311050)
goias <- c(522185, 521250, 520400, 520800, 521760, 520620)
riograndedonorte <- c(240810, 240420, 241120, 240200, 240270, 240160, 240360, 240750, 240895, 241370)
paraiba <- c(250750, 251530, 251597, 251276, 250523, 250403)
saopaulo <- c(355030)
interview_locations <- c(ceara, riodejaneiro, minasgerais, goias, riograndedonorte, paraiba, saopaulo)

hh <- h[which(h$cod_ibge %in% interview_locations),]

pdf("../../plots/interview_locations.pdf", width=9, height=10)
par(mfrow=c(4,3))
plot(density(log(h$population)), main="Population logged", xlab="",lwd=3)
abline(v=log(hh$population), col="red")
lines(density(log(h$population)), main="Population logged", xlab="",lwd=3)
plot(density(h$population_share_rural, na.rm=T),lwd=3, main="Population rural, share", xlab="")
abline(v=hh$population_share_rural, col="red")
lines(density(h$population_share_rural, na.rm=T),lwd=3, main="Population rural, share", xlab="")
plot(density(h$gini),lwd=3, main="Gini coefficient",xlab="")
abline(v=hh$gini, col="red")
lines(density(h$gini,na.rm=T),lwd=3, main="Gini coefficient",xlab="")
plot(density(h$human_development_index,na.rm=T),lwd=3, main="Human development index",xlab="")
abline(v=hh$human_development_index, col="red")
lines(density(h$human_development_index),lwd=3, main="Human development index",xlab="")
plot(density(log(h$income_percapita)),lwd=3, main="Income per capita (logged BRL)",xlab="")
abline(v=log(hh$income_percapita), col="red")
lines(density(log(h$income_percapita)),lwd=3, main="Income per capita (logged BRL)",xlab="")
plot(density(h$population_share_poor),lwd=3, main="Population who live in poverty, %",xlab="")
abline(v=hh$population_share_poor, col="red")
lines(density(h$population_share_poor),lwd=3, main="Population who live in poverty, %",xlab="")
plot(density(h$child_mortality_rate),lwd=3, main="Child mortality rate \n (deaths for every 1,000 born)",xlab="")
abline(v=hh$child_mortality_rate, col="red")
lines(density(h$child_mortality_rate),lwd=3, main="Child mortality rate \n (deaths for every 1,000 born)",xlab="")
plot(density(h$ideb,na.rm=T),lwd=3, main="School quality score \n (test scores, passing rates)",xlab="")
abline(v=hh$ideb, col="red")
lines(density(h$ideb,na.rm=T),lwd=3, main="School quality score",xlab="")
plot(density(h$schooling_delay_2plusyears),lwd=3, main="Age-grade distortion rate \n (% of students lagging by 2 grades or +)",xlab="")
abline(v=hh$schooling_delay_2plusyears, col="red")
lines(density(h$schooling_delay_2plusyears),lwd=3, main="Age-grade distortion rate \n (% of students lagging by 2 grades or +)",xlab="")
plot(density(h$winner_voteshare,na.rm=T),lwd=3, main="Winner vote share \n in mayoral election",xlab="")
abline(v=hh$winner_voteshare, col="red")
lines(density(h$winner_voteshare,na.rm=T),lwd=3, main="Winner vote share \n in mayoral election",xlab="")
plot(density(h$winner_margin, na.rm=T),lwd=3, main="Margin of victory \n in mayoral election", xlab="")
abline(v=hh$winner_margin, col="red")
lines(density(h$winner_margin, na.rm=T),lwd=3, main="Margin of victory \n in mayoral election", xlab="")
plot(density(h$electoral_concentration,na.rm=T),lwd=3, main="Herfindahl index \n of electoral concentration",xlab="")
abline(v=hh$electoral_concentration, col="red")
lines(density(h$electoral_concentration,na.rm=T),lwd=3, main="Herfindahl index \n of electoral concentration",xlab="")
dev.off()

# NOTES -- R version, platform, and loaded packages -------------------------

### sessionInfo(package = NULL)

# R version 3.6.3 (2020-02-29)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS  10.16
# 
# Matrix products: default
# LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
#   [1] grid      stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] xtable_1.8-4    cjoint_2.1.0    survey_4.0      survival_3.1-11 Matrix_1.2-18   lmtest_0.9-37  
# [7] zoo_1.8-7       sandwich_2.5-1  texreg_1.37.1   codebook_0.9.2  readxl_1.3.1    here_1.0.1     
# [13] forcats_0.5.0   stringr_1.4.0   dplyr_1.0.0     purrr_0.3.3     readr_1.4.0     tidyr_1.0.2    
# [19] tibble_3.0.0    ggplot2_3.3.5   tidyverse_1.3.0
# 
# loaded via a namespace (and not attached):
#   [1] nlme_3.1-145      fs_1.4.1          lubridate_1.7.4   webshot_0.5.2     httr_1.4.1        rprojroot_2.0.2  
# [7] repr_1.1.0        tools_3.6.3       backports_1.1.5   utf8_1.1.4        R6_2.4.1          DT_0.13          
# [13] DBI_1.1.0         colorspace_1.4-1  withr_2.1.2       tidyselect_1.1.0  compiler_3.6.3    cli_2.3.0        
# [19] rvest_0.3.5       xml2_1.3.0        desc_1.2.0        labeling_0.3      scales_1.1.0      digest_0.6.25    
# [25] rmarkdown_2.6     base64enc_0.1-3   pkgconfig_2.0.3   htmltools_0.4.0   labelled_2.5.0    fastmap_1.0.1    
# [31] dbplyr_1.4.2      highr_0.8         htmlwidgets_1.5.1 rlang_1.0.0       rstudioapi_0.11   shiny_1.4.0.2    
# [37] farver_2.0.3      generics_0.0.2    jsonlite_1.6.1    crosstalk_1.1.0.1 magrittr_1.5      Rcpp_1.0.7       
# [43] munsell_0.5.0     fansi_0.4.1       lifecycle_0.2.0   stringi_1.4.6     yaml_2.2.1        rmdpartials_0.5.8
# [49] plyr_1.8.6        parallel_3.6.3    listenv_0.8.0     promises_1.1.0    crayon_1.3.4      lattice_0.20-40  
# [55] haven_2.3.1       splines_3.6.3     hms_0.5.3         knitr_1.28        pillar_1.4.3      pkgload_1.0.2    
# [61] codetools_0.2-16  reprex_0.3.0      glue_1.3.2        evaluate_0.14     mitools_2.4       modelr_0.1.6     
# [67] vctrs_0.3.1       httpuv_1.5.2      testthat_2.3.2    cellranger_1.1.0  gtable_0.3.0      future_1.17.0    
# [73] assertthat_0.2.1  xfun_0.20         mime_0.9          skimr_2.1.3       broom_0.5.5       later_1.0.0      
# [79] rsconnect_0.8.16  globals_0.12.5    ellipsis_0.3.0   