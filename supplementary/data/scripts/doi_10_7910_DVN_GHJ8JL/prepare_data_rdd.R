### Replication code
### Article: "How patronage delivers: Political appointments, bureaucratic accountability, and service delivery in Brazil"
### Author: Guillermo Toral (www.guillermotoral.com)
### Date: June 2022
### This file prepares the regression discontinuity design data
### R version, platform, and package versions reported at the end of the file

# Prepare the environment -------------------------------------------------

### This section of the code prepares the environment 

# Clean the environment
rm(list = ls())

# Install required packages if not previously installed
package_list <- c("tidyverse", "here", "readxl", "codebook", "here") 
packages_to_install <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(packages_to_install)>0){
  install.packages(packages_to_install)
}

# Load required packages
library(tidyverse)
library(here)
library(readxl)
library(codebook)
library(here)

# Have dplyr not print information when using group_by
options(dplyr.summarise.inform = FALSE)

# Set Working Directory to wherever this file is located.
setwd(here())
# The directory where the code folder is located must also have a "plots" and a "tables" subdirectory

# Load and clean treatment data (school quality scores) ------------------------------------------------

# Import data with primary school scores, omitting the first 9 rows which don't include data
ideb <- data.frame(read_excel("../../datasets/downloaded/inep/ideb/divulgacao_anos_iniciais_escolas_2019.xlsx",skip=9)) 

# Subset to municipal schools, and grab data corresponding to 2013
ideb <- ideb[which(ideb$REDE=='Municipal'),
             grep('CO_MUNICIPIO|ID_ESCOLA|VL_INDICADOR_REND_2013|VL_NOTA_MEDIA_2013|VL_OBSERVADO_2013|VL_PROJECAO_2013',names(ideb))]

# Remove NA's
ideb <- data.frame(apply(ideb,2,as.numeric))

# Rename variables 
ideb13 <- ideb %>%
  mutate(cod_escola= ID_ESCOLA,
         test_scores = VL_NOTA_MEDIA_2013, # "nota média" = average score 
         passing = VL_INDICADOR_REND_2013, # "indicador rendimento" refers to passing rates
         ideb = VL_OBSERVADO_2013, # "valor observado" refers to IDEB scores, with the rounding applied by the Ministry
         ideb_cont = VL_INDICADOR_REND_2013*VL_NOTA_MEDIA_2013, # to obtain the continuous IDEB scores, I multiply the passing rates by the average student test score
         ideb_target = VL_PROJECAO_2013, # "valor projeção" refers to the IDEB target
         ideb_gap_inep = VL_OBSERVADO_2013-VL_PROJECAO_2013, # the gap with the government's rounding: IDEB score - IDEB target
         ideb_gap = VL_INDICADOR_REND_2013*VL_NOTA_MEDIA_2013-VL_PROJECAO_2013, # the gap without rounding: passing*score - target
         ideb_gap_centered = VL_INDICADOR_REND_2013*VL_NOTA_MEDIA_2013-VL_PROJECAO_2013+0.05, # gap centered, adding 0.05 (with the rounding, that's equivalent to zero)
         treated = ifelse(VL_INDICADOR_REND_2013*VL_NOTA_MEDIA_2013-VL_PROJECAO_2013+0.05>=0,1,0)) %>% # treatment indicator, equal 1 if the continuous ideb score, plus 0.05 for the rounding, is at 0 or above (that is, if the IDEB score, with rounding, is at least as large as the IDEB target)
  dplyr::select(cod_escola, test_scores, passing, ideb, ideb_cont, ideb_target, ideb_gap_inep, ideb_gap, ideb_gap_centered, treated)

# Import data with middle school scores
ideb_ef2 <- data.frame(read_excel("../../datasets/downloaded/inep/ideb/divulgacao_anos_finais_escolas_2019.xlsx",skip=9))

# Subset to municipal schools, and grab data corresponding to 2013
ideb_ef2 <- ideb_ef2[which(ideb_ef2$REDE=='Municipal'),
                     grep('CO_MUNICIPIO|ID_ESCOLA|VL_INDICADOR_REND_2013|VL_NOTA_MEDIA_2013|VL_OBSERVADO_2013|VL_PROJECAO_2013',names(ideb_ef2))] 

# Remove NA's
ideb_ef2 <- data.frame(apply(ideb_ef2,2,as.numeric)) 

# Rename variables (see comments above)
ideb13_ef2 <- ideb_ef2 %>%
  mutate(cod_escola = ID_ESCOLA,
         test_scores = VL_NOTA_MEDIA_2013,
         passing = VL_INDICADOR_REND_2013,
         ideb = VL_OBSERVADO_2013,
         ideb_cont = VL_INDICADOR_REND_2013*VL_NOTA_MEDIA_2013,
         ideb_target = VL_PROJECAO_2013,
         ideb_gap_inep = VL_OBSERVADO_2013-VL_PROJECAO_2013,
         ideb_gap = VL_INDICADOR_REND_2013*VL_NOTA_MEDIA_2013-VL_PROJECAO_2013,
         ideb_gap_centered = VL_INDICADOR_REND_2013*VL_NOTA_MEDIA_2013-VL_PROJECAO_2013+0.05,
         treated = ifelse(VL_INDICADOR_REND_2013*VL_NOTA_MEDIA_2013-VL_PROJECAO_2013+0.05>=0,1,0)) %>%
  dplyr::select(cod_escola, test_scores, passing, ideb, ideb_cont, ideb_target, ideb_gap_inep, ideb_gap, ideb_gap_centered, treated)

# Create list of schools that have both a score and a target for middle school IDEB
ideb13_ef2 <- subset(ideb13_ef2, !is.na(ideb13_ef2$ideb_gap))

# Omit from the main dataset schools that have a school quality signal for middle school
ideb13 <- subset(ideb13, !(ideb13$cod_escola %in% ideb13_ef2$cod_escola))

# Load and clean outcome data (director turnover) -------------------------

# Load data on the government's survey of school directors corresponding to 2015
dir15 <- read_csv("../../datasets/downloaded/inep/anresc/diretor_2015.csv")

# Select municipal schools and the variable about whether the school's director has been in their post for a year or less
dir15 <- dir15 %>%
  dplyr::filter(ID_DEPENDENCIA_ADM==3) %>% 
  mutate(cod_escola = as.numeric(ID_ESCOLA),
         director_turnover = ifelse(TX_RESP_Q017=="A",1,0)) %>%
  dplyr::select(cod_escola, director_turnover)

# Load and clean director appointment mode data ---------------------------

# Load data on the government's survey of school directors corresponding to 2013
dir13 <- read_csv("../../datasets/downloaded/inep/anresc/diretor_2013.csv")

# Select municipal schools and the variables related to the director's selection mode
# Q014 refers to the survey question asking them how they were selected to lead the school
# A = civil service only, B = Election only, C = Political appointment only, D = Selective process only, E = Selective process and election, F = Selective process and political appointment, G = Other method
dir13 <- dir13 %>%
  dplyr::filter(ID_DEPENDENCIA_ADM==3) %>% # Select municipal schools
  mutate(cod_escola = as.numeric(ID_ESCOLA),
         director_appointed = ifelse(TX_RESP_Q014=="C" | TX_RESP_Q014=="F",1,0),
         director_elected = ifelse(TX_RESP_Q014=="B" | TX_RESP_Q014=="E",1,0),
         director_civil_service = ifelse(TX_RESP_Q014=="A",1,0),
         director_selected = ifelse(TX_RESP_Q014=="D",1,0)) %>%
  dplyr::select(cod_escola, director_appointed, director_elected, director_civil_service, director_selected)

# Merge treatment, outcome, and appointment data ----------------------------------------

d <- left_join(dir13, ideb13, by="cod_escola")
d <- left_join(d, dir15, by="cod_escola")

# Define interactions between forcing variable, treatment, and appointed / elected / civil service
d$fv <- d$ideb_gap_centered # Forcing variable
d$treated_fv_appointed <- d$treated*d$fv*d$director_appointed  
d$treated_appointed <- d$treated*d$director_appointed
d$fv_appointed <- d$fv*d$director_appointed
d$treated_fv_elected <- d$treated*d$fv*d$director_elected
d$treated_elected <- d$treated*d$director_elected
d$fv_elected <- d$fv*d$director_elected
d$treated_fv_civil_service <- d$treated*d$fv*d$director_civil_service
d$treated_civil_service <- d$treated*d$director_civil_service
d$fv_civil_service <- d$fv*d$director_civil_service

# Load and merge municipality identifiers --------------------------------------------

# Load school data from the 2013 school census
e <- read_delim("../../datasets/downloaded/inep/censo_escolar/escolas_2013.csv",delim="|") 
ee <- e %>%
  mutate(cod_escola = PK_COD_ENTIDADE,
         cod_municipio = FK_COD_MUNICIPIO) %>%
  dplyr::select(cod_escola, cod_municipio)

d <- left_join(d, ee)

# Define 6-digit municipality identifier, and 2-digit state identifier
d$uf <- substr(d$cod_municipio,1,2)
d$cod_ibge <- as.numeric(substr(d$cod_municipio,1,6))

# Load, clean, and merge covariates: Municipal sociodemographics ---------------------

# Load dataset with municipality-level covariates
m <- read_csv("../../datasets/downloaded/basedosdados/basedosdados_municipality_covariates.csv")

# Select data for 2013
municipality_covariates_2013 <- m %>%
  dplyr::filter(year==2013) %>%
  mutate(cod_ibge = as.numeric(substr(ibge,1,6)),
         log_population = log(population),
         log_gdp_percapita = log(gdppercapita)) %>%
  dplyr::select(cod_ibge, 
                population, 
                log_population,
                log_gdp_percapita,
                deaths,
                deaths_perthousand)

# Merge
d <- left_join(d, municipality_covariates_2013, by="cod_ibge")
  
# Load, clean, and merge covariates: Municipal-level IDEB -----------------

# Load data on municipality-level IDEB
ideb_mun <- data.frame(read_excel("../../datasets/downloaded/inep/ideb/divulgacao_anos_iniciais_municipios_2019.xlsx",skip=9)) 

# Subset to municipal schools, and grab relevant variables corresponding to 2013
ideb_mun <- ideb_mun[which(ideb_mun$REDE=='Municipal'),grep('CO_MUNICIPIO|VL_INDICADOR_REND_2013|VL_NOTA_MEDIA_2013|VL_OBSERVADO_2013|VL_PROJECAO_2013',names(ideb_mun))] # Keep only measures for municipal schools, and variables of interest

# Remove NA's
ideb_mun <- data.frame(apply(ideb_mun,2,as.numeric)) 

# Rename variables (see comments above)
ideb13_mun <- ideb_mun %>%
  mutate(cod_ibge = as.numeric(substr(CO_MUNICIPIO,1,6)),
         mun_test_scores = VL_NOTA_MEDIA_2013,
         mun_passing = VL_INDICADOR_REND_2013,
         mun_ideb = VL_OBSERVADO_2013,
         mun_ideb_target = VL_PROJECAO_2013) %>%
  dplyr::select(cod_ibge, mun_test_scores, mun_passing, mun_ideb, mun_ideb_target)

d <- left_join(d, ideb13_mun)

# Load, clean, and merge covariates: School census ------------------

# Load 2013 school-census dataset
e <- read_delim("../../datasets/downloaded/inep/censo_escolar/escolas_2013.csv",delim="|") 

# Select variables for controls
school_census <- e %>% 
  mutate(cod_escola = PK_COD_ENTIDADE,
         dependencia = ID_DEPENDENCIA_ADM, # 1 = federal, 2 = state, 3 = municipal, 4 = private
         rural = ifelse(ID_LOCALIZACAO==2,1,0), # indicator for rural school
         number_workers = NUM_FUNCIONARIOS, # number of school staff
         log_number_workers = log(number_workers), 
         in_settlement_land = ifelse(ID_LOCALIZACAO_DIFERENCIADA==1,1,0), # indicator for whether the school is in a settlement
         in_indigenous_land = ifelse(ID_LOCALIZACAO_DIFERENCIADA==2,1,0), # indicator for whether the school is in indigenous land
         in_quilombola_land = ifelse(ID_LOCALIZACAO_DIFERENCIADA==3,1,0), # indicator for whether the school is in quilombola land
         classrooms_in_use = NUM_SALAS_UTILIZADAS) %>% # number of classrooms in use
  dplyr::select(cod_escola, rural, number_workers, log_number_workers, in_settlement_land, in_indigenous_land, in_quilombola_land, classrooms_in_use)

d <- left_join(d, school_census)

# Load, clean, and merge covariates: School educational indicators -------

# Average number of students per classroom
a <- data.frame(read_excel("../../datasets/downloaded/inep/indicadores_educacionais/MEDIA ALUNOS TURMA ESCOLAS 2013.xlsx",skip=9))
a <- a[,grep("PK_COD_ENTIDADE|ATU_FUN",names(a))] # Keep only measures for municipal schools, and variable of interest
a <- data.frame(apply(a,2,as.numeric)) # Remove NA's

atu <- a %>%
  mutate(cod_escola = PK_COD_ENTIDADE, 
         students_per_classroom = ATU_FUN) %>%
  dplyr::select(cod_escola, students_per_classroom)

d <- left_join(d, atu)

# School socioeconomic index
inse <- data.frame(read_excel("../../datasets/downloaded/inep/indicadores_educacionais/Indicador_INSE_por_Escola.xlsx",skip=9))
inse <- inse[,grep("COD_ESCOLA|INSE...VALOR.ABSOLUTO",names(inse))] # Keep only measures for municipal schools, and variable of interest
inse <- data.frame(apply(inse,2,as.numeric)) # Remove NA's

school_socioeconomic_index <- inse %>%
  mutate(cod_escola = COD_ESCOLA,
         school_inse = INSE...VALOR.ABSOLUTO) %>%
  dplyr::select(cod_escola, school_inse)

d <- left_join(d, school_socioeconomic_index)

# Load, clean, and merge covariates: School director characteristics --------

director_2013 <- read_csv("../../datasets/downloaded/inep/anresc/diretor_2013.csv")
director_covariates <- director_2013 %>%
  mutate(cod_escola = as.numeric(ID_ESCOLA),
         female = ifelse(TX_RESP_Q001=="B",1,0),
         # Age indicators
         age_below24 = ifelse(TX_RESP_Q002=="A",1,0), 
         age_25a29 = ifelse(TX_RESP_Q002=="B",1,0),
         age_30a39 = ifelse(TX_RESP_Q002=="C",1,0), 
         age_40a49 = ifelse(TX_RESP_Q002=="D",1,0),
         age_50a54 = ifelse(TX_RESP_Q002=="E",1,0),
         age_above54 = ifelse(TX_RESP_Q002=="F",1,0),
         # Aggregated indicators
         age_below40 = ifelse(age_below24==1 | age_25a29==1 | age_30a39==1,1,0),
         age_forties = ifelse(age_40a49==1,1,0),
         age_above50 = ifelse(age_50a54==1 | age_above54==1,1,0),
         # Race indicators
         race_white = ifelse(TX_RESP_Q003 == "A",1,0), 
         race_black = ifelse(TX_RESP_Q003 == "B",1,0),
         race_brown = ifelse(TX_RESP_Q003 == "C",1,0), 
         race_asian = ifelse(TX_RESP_Q003 == "D",1,0),
         race_indigenous = ifelse(TX_RESP_Q003 == "E",1,0),
         race_notinformed = ifelse(TX_RESP_Q003 == "F",1,0),
         # Aggregated indicators
         race_white = race_white,
         race_black_or_brown = ifelse(race_black==1 | race_brown==1,1,0),
         race_other = ifelse(race_white==0 & race_black_or_brown==0,1,0),
         # Education indicators
         schooling_lessthanhighschool = ifelse(TX_RESP_Q004 == "A",1,0), 
         schooling_magisterio = ifelse(TX_RESP_Q004 == "B",1,0), 
         schooling_otherhighschool = ifelse(TX_RESP_Q004 == "C",1,0), 
         schooling_tertiary_pedagogy = ifelse(TX_RESP_Q004 == "D",1,0), 
         schooling_tertiary_normal = ifelse(TX_RESP_Q004 == "E",1,0), 
         schooling_tertiary_licenciatura = ifelse(TX_RESP_Q004 == "F" | TX_RESP_Q004 == "G" | TX_RESP_Q004 == "H",1,0), 
         schooling_tertiary_other = ifelse(TX_RESP_Q004 == "I",1,0),
         schooling_noposgraduate = ifelse(TX_RESP_Q008 == "A",1,0),
         schooling_atualizacao = ifelse(TX_RESP_Q008 == "B",1,0),
         schooling_especializacao = ifelse(TX_RESP_Q008 == "C",1,0),
         schooling_masters = ifelse(TX_RESP_Q008 == "D",1,0),
         schooling_doctorate = ifelse(TX_RESP_Q008 == "E",1,0),
         # Aggregated indicators
         schooling_lessthantertiary = ifelse(schooling_lessthanhighschool==1|schooling_magisterio==1|schooling_otherhighschool==1,1,0),
         schooling_tertiary = ifelse(schooling_tertiary_pedagogy==1 | schooling_tertiary_normal==1 | schooling_tertiary_licenciatura==1 | schooling_tertiary_other==1,1,0),
         schooling_postgraduate = ifelse(schooling_atualizacao==1 | schooling_especializacao==1 | schooling_especializacao==1 | schooling_masters==1 | schooling_doctorate==1,1,0),
         # Job indicators
         has_other_job_education = ifelse(TX_RESP_Q011=="A",1,0),
         has_other_job_noeducation = ifelse(TX_RESP_Q011=="B",1,0),
         works_morethan40h = ifelse(TX_RESP_Q013=="A",1,0),
         # Aggregated indicators
         exclusive_dedication = ifelse(has_other_job_education==0 & has_other_job_noeducation==0,1,0),
         # Teaching experience indicators
         teacher_experience_lessthan1yr = ifelse(TX_RESP_Q015=="B",1,0),
         teacher_experience_1to2yr = ifelse(TX_RESP_Q015=="C",1,0),
         teacher_experience_3to5yr = ifelse(TX_RESP_Q015=="D",1,0),
         teacher_experience_6to10yr = ifelse(TX_RESP_Q015=="E",1,0),
         teacher_experience_11to15yr = ifelse(TX_RESP_Q015=="F",1,0),
         teacher_experience_16to20yr = ifelse(TX_RESP_Q015=="G",1,0),
         teacher_experience_over20yr = ifelse(TX_RESP_Q015=="H",1,0),
         # Aggregate indicators
         teacher_experience_lessthan6yr = ifelse(TX_RESP_Q015=="A"|TX_RESP_Q015=="B"|TX_RESP_Q015=="C"|TX_RESP_Q015=="D",1,0),
         teacher_experience_6to15yr = ifelse(TX_RESP_Q015=="E"|TX_RESP_Q015=="F",1,0),
         teacher_experience_over15yr = ifelse(TX_RESP_Q015=="G"|TX_RESP_Q015=="H",1,0),
         # Director experience indicators
         director_experience_1to2yr = ifelse(TX_RESP_Q016=="B",1,0),
         director_experience_3to5yr = ifelse(TX_RESP_Q016=="C",1,0),
         director_experience_6to10yr = ifelse(TX_RESP_Q016=="D",1,0),
         director_experience_11to15yr = ifelse(TX_RESP_Q016=="E",1,0),
         director_experience_16to20yr = ifelse(TX_RESP_Q016=="F",1,0),
         director_experience_over20yr = ifelse(TX_RESP_Q016=="G",1,0),
         # Aggregate indicators
         director_experience_lessthan3yr = ifelse(TX_RESP_Q016=="A"|TX_RESP_Q016=="B",1,0),
         director_experience_3to10yr = ifelse(TX_RESP_Q016=="C"|TX_RESP_Q016=="D",1,0),
         director_experience_over10yr = ifelse(TX_RESP_Q016=="E"|TX_RESP_Q016=="F"|TX_RESP_Q016=="G",1,0),
         # Director in this school indicators
         director_here_lessthan1yr = ifelse(TX_RESP_Q017=="A",1,0),
         director_here_1to2yr = ifelse(TX_RESP_Q017=="B",1,0),
         director_here_3to5yr = ifelse(TX_RESP_Q017=="C",1,0),
         director_here_6to10yr = ifelse(TX_RESP_Q017=="D",1,0),
         director_here_11to15yr = ifelse(TX_RESP_Q017=="E",1,0),
         director_here_16to20yr = ifelse(TX_RESP_Q017=="F",1,0),
         director_here_morethan20yr = ifelse(TX_RESP_Q017=="G",1,0),
         # Aggregate indicators
         director_here_lessthan3yr = ifelse(TX_RESP_Q017=="A"|TX_RESP_Q017=="B",1,0),
         director_here_3to10yr = ifelse(TX_RESP_Q017=="C"|TX_RESP_Q017=="D",1,0),
         director_here_over10yr = ifelse(TX_RESP_Q017=="E"|TX_RESP_Q017=="F"|TX_RESP_Q017=="G",1,0)) %>%
  dplyr::select(-c(1:118))

d <- left_join(d, director_covariates)

# Load, clean, and merge covariates: School-level IDEB scores 2011 ---------------------

# Load IDEB data
ideb11 <- data.frame(read_excel("../../datasets/downloaded/inep/ideb/divulgacao_anos_iniciais_escolas_2019.xlsx",skip=9)) 

# Subset to municipal schools, and grab relevant variables 
ideb11 <- ideb11[which(ideb11$REDE=='Municipal'),
                 grep('ID_ESCOLA|VL_INDICADOR_REND_2011|VL_NOTA_MEDIA_2011|VL_OBSERVADO_2011|VL_PROJECAO_2011',names(ideb11))] # Keep only measures for municipal schools, and variables of interest

# Remove NA's
ideb11 <- data.frame(apply(ideb11,2,as.numeric)) 

# Rename variables of interest (see comments above)
ideb_2011 <- ideb11 %>%
  mutate(cod_escola= ID_ESCOLA,
         test_scores_2011 = VL_NOTA_MEDIA_2011,
         passing_2011 = VL_INDICADOR_REND_2011,
         ideb_2011 = VL_OBSERVADO_2011,
         ideb_target_2011 = VL_PROJECAO_2011) %>%
  dplyr::select(cod_escola, test_scores_2011, passing_2011, ideb_2011, ideb_target_2011)

# Merge
d <- left_join(d, ideb_2011)

# Load, clean, and merge covariates: Municipal politics  --------

# Load data on candidates and their performance in local elections from 2000 to 2016, from Brazil's Supreme Electoral Court (TSE) via the "electionsBR" package
load("../../datasets/downloaded/tse/tse_elections_2000_to_2016.RData") # files starting with d and v have data on candidates and candidates' performance by year. Numbers correspond to election years, from 2000 to 2016

### Extract data on candidates

# Define vector with categories of candidates to exclude -- these are candidates whose candidacy was not validated or was cancelled by the electoral justice, or who died
invalid <- c("INDEFERIDO", "INDEFERIDO POR IMPUGNAÇÃO", "FALECIDO", "CASSADO", "CASSAÇÃO DO REGISTRO", "CANCELAMENTO", "CANCELADO", "INELEGÍVEL")

# Identify mayoral candidates running for the 2012 - 2016 term
candidates_1216 <- d12 %>% 
  # Exclude candidates other than valid candidates for mayor who got elected
  dplyr::filter(DESCRICAO_CARGO=="PREFEITO" &  # Retain only candidates for mayoral office
                  !(DES_SITUACAO_CANDIDATURA %in% invalid)) %>% # And whose candidacy was not invalid
  # Keep and rename variables of interest
  dplyr::select(code_municipality_tse = SIGLA_UE, # TSE's municipality code
                code_candidate = SEQUENCIAL_CANDIDATO, # TSE's candidate code
                cpf_candidate = CPF_CANDIDATO, # Candidate's CPF (a government unique ID for individuals, similar to the social security number in the US)
                code_party = NUMERO_PARTIDO, # TSE's party code
                name_party = SIGLA_PARTIDO, # Party name
                round = NUM_TURNO, # Round of elections
                candidate_situation = DES_SITUACAO_CANDIDATURA, # situation of the candidacy
                candidate_result = DESC_SIT_TOT_TURNO,
                election_description = DESCRICAO_ELEICAO) # Description of the election
# Identify those who were elected as mayors
mayors_1216 <- candidates_1216 %>% 
  dplyr::filter(candidate_result == "ELEITO") # Keep elected ones 
# Identify municipalities where we observe multiple mayors being elected
repeated_muns <- mayors_1216[duplicated(mayors_1216$code_municipality_tse),] # 28
repeated_elections <- mayors_1216[which(mayors_1216$code_municipality_tse %in% repeated_muns$code_municipality_tse),] 
# Identify municipalities where we observe supplementary elections
supplementary_elections_1216 <- repeated_elections[which(repeated_elections$election_description!="ELEIÇÃO MUNICIPAL 2012"),]
supplementary_elections_1216$supplementary_election <- 1
# Remove observations where there are repeated elections
candidates_1216 <- candidates_1216 %>% 
  dplyr::filter(!(code_municipality_tse %in% repeated_elections$code_municipality_tse))
candidates_1216$supplementary_election <- 0
# Add the supplementary elections
candidates_1216 <- rbind(candidates_1216, supplementary_elections_1216)
# Remove one municipality for which we observe two supplementary elections -- we do not know which one is valid
candidates_1216 <- subset(candidates_1216, candidates_1216$code_municipality_tse != "05312")
# Filter the mayors file
mayors_1216 <- candidates_1216 %>% 
  dplyr::filter(candidate_result == "ELEITO") # Keep elected ones 
nrow(mayors_1216)==n_distinct(mayors_1216$code_municipality_tse) # 5567

# Identify mayoral candidates running for the 2008 - 2012 term
candidates_0812 <- d08 %>% 
  # Exclude candidates other than valid candidates for mayor who got elected
  dplyr::filter(DESCRICAO_CARGO=="PREFEITO" &  # Retain only candidates for mayoral office
                  !(DES_SITUACAO_CANDIDATURA %in% invalid)) %>%  # And whose candidacy was not invalid
  # Keep and rename variables of interest
  dplyr::select(code_municipality_tse = SIGLA_UE, # TSE's municipality code
                code_candidate = SEQUENCIAL_CANDIDATO, # TSE's candidate code
                cpf_candidate = CPF_CANDIDATO, # Candidate's CPF (a government unique ID for individuals, similar to the social security number in the US)
                code_party = NUMERO_PARTIDO, # TSE's party code
                name_party = SIGLA_PARTIDO, # Party name
                round = NUM_TURNO, # Round of elections
                candidate_situation = DES_SITUACAO_CANDIDATURA, # situation of the candidacy
                candidate_result = DESC_SIT_TOT_TURNO,
                election_description = DESCRICAO_ELEICAO) # Description of the election
# Identify those who were elected as mayors
mayors_0812 <- candidates_0812 %>% 
  dplyr::filter(candidate_result == "ELEITO") # Keep elected ones 
# Identify municipalities where we observe multiple mayors being elected
repeated_muns <- mayors_0812[duplicated(mayors_0812$code_municipality_tse),] # 28
repeated_elections <- mayors_0812[which(mayors_0812$code_municipality_tse %in% repeated_muns$code_municipality_tse),] 
# Identify municipalities where we observe supplementary elections
supplementary_elections_0812 <- repeated_elections[which(repeated_elections$election_description== "ELEIÇÕES SUPLEMENTARES 2008"),]
supplementary_elections_0812$supplementary_election <- 1
# Remove observations where there are repeated elections
candidates_0812 <- candidates_0812 %>% 
  dplyr::filter(!(code_municipality_tse %in% repeated_elections$code_municipality_tse))
candidates_0812$supplementary_election <- 0
# Add the supplementary elections
candidates_0812 <- rbind(candidates_0812, supplementary_elections_0812)
# Remove two municipalities for which we observe two supplementary elections (we do not know which one is valid)
candidates_0812 <- subset(candidates_0812, candidates_0812$code_municipality_tse != "09156" & candidates_0812$code_municipality_tse != "43494")
# Filter the candidates file to keep only elected ones
mayors_0812 <- candidates_0812 %>% 
  dplyr::filter(candidate_result == "ELEITO") # Keep elected ones 
nrow(mayors_0812)==n_distinct(mayors_0812$code_municipality_tse) # 5553

### Extract data on votes

# Votes in the 2012 election
votes_1216 <- v12 %>%
  # Exclude candidates other than approved candidates who ran for mayor, did not have their candidacies 
  dplyr::filter(DESCRICAO_CARGO == "PREFEITO" & 
                  DESC_SIT_CAND_SUPERIOR == "APTO" &
                  !(DESC_SIT_CANDIDATO %in% invalid)) %>%
  dplyr::select(code_municipality_tse = SIGLA_UE, # TSE's municipality code
                code_candidate = SQ_CANDIDATO, # TSE's candidate code
                code_party = NUMERO_PARTIDO, # TSE's party code
                round = NUM_TURNO, # Round of elections
                votes = TOTAL_VOTOS, # Number of votes
                candidate_result = DESC_SIT_CAND_TOT, # situation of the candidacy
                election_description = DESCRICAO_ELEICAO) %>%
  dplyr::mutate(votes = as.numeric(votes))
votes_1216_regular <- votes_1216 %>%
  dplyr::filter(!(code_municipality_tse %in% supplementary_elections_1216$code_municipality_tse))
votes_1216_supplementary <- votes_1216 %>%
  dplyr::filter(election_description != "ELEIÇÃO MUNICIPAL 2012")
votes_1216 <- rbind(votes_1216_regular, votes_1216_supplementary)

## Merge with identifiers for all municipalities

# Load municipality identifiers, obtained via basedosdados
municipality_identifiers <- read_csv("../../datasets/downloaded/basedosdados/basedosdados_municipality_identifiers.csv")

# Select TSE identifier
municipalities <- municipality_identifiers %>%
  mutate(cod_municipio = id_municipio,
         cod_ibge = as.numeric(substr(cod_municipio,1,6)),
         cod_tse = id_municipio_tse) %>%
  dplyr::select(cod_municipio, cod_tse)

# Generate the 5-digit version of the TSE code (with leading zeroes)
municipalities$code_municipality_tse <- str_pad(as.character(municipalities$cod_tse), 5, side = "left", pad = "0") # Add leading zeroes (TSE codes are all 5-digit)

# Merge mayor data, keeping separate datasets for each election cycle
m12 <- left_join(municipalities, mayors_1216)
m08 <- left_join(municipalities, mayors_0812)

### Measure electoral performance in 2012

m12$incumbent_mayor_ran <- NA
m12$total_votes <- NA
m12$electoral_concentration <- NA
m12$incumbent_mayor_voteshare <- NA
m12$strongest_opponent_voteshare <- NA

for(i in 1:nrow(m12)){
  # Get votes in the municipality
  votes_here <- subset(votes_1216, votes_1216$code_municipality_tse == m12$code_municipality_tse[i] & votes_1216$round==1)
  # List of candidates
  candidates_here <- subset(candidates_1216, candidates_1216$code_municipality_tse == m12$code_municipality_tse[i] & candidates_1216$round==1)
  # Check if the incumbent ran
  cpf_incumbent <- m08[which(m08$code_municipality_tse==m12$code_municipality_tse[i]),"cpf_candidate"][[1]]
  m12$incumbent_mayor_ran[i] <- ifelse(cpf_incumbent %in% candidates_here$cpf_candidate,1,0)
  votes_here_by_candidate <- subset(votes_here, votes_here$round==1) %>% 
    group_by(code_candidate) %>% # Some candidates have votes reported in multiple lines
    dplyr::summarise(votes = sum(votes))
  m12$electoral_concentration[i] <- sum((votes_here_by_candidate$votes/sum(votes_here_by_candidate$votes))^2)
  # Vote numbers in the first round
  # Sum number of votes
  m12$total_votes[i] <- sum(votes_here_by_candidate$votes)
  # Vote share of incumbent
  if(m12$incumbent_mayor_ran[i]==0){
    next
  }
  code_incumbent <- candidates_here[which(candidates_here$cpf_candidate==cpf_incumbent & candidates_here$round==1),"code_candidate"][[1]]
  m12$incumbent_mayor_voteshare[i] <- sum(votes_here_by_candidate[which(votes_here_by_candidate$code_candidate==code_incumbent),"votes"][[1]]/sum(votes_here_by_candidate$votes))
  votes_here_by_opponent <- subset(votes_here_by_candidate, votes_here_by_candidate$code_candidate != code_incumbent) %>%
    arrange(desc(votes))
  m12$strongest_opponent_voteshare[i] <- sum(votes_here_by_opponent[1,"votes"][[1]]/sum(votes_here_by_candidate$votes))
  if(m12$round[i]==2){ # If the election was decided in the second round, record electoral performance in that round instead of the first one
    # Vote tallies
    votes_here <- subset(votes_1216, votes_1216$code_municipality_tse == m12$code_municipality_tse[i] & votes_1216$round==2)
    # List of candidates
    candidates_here <- subset(candidates_1216, candidates_1216$code_municipality_tse == m12$code_municipality_tse[i] & candidates_1216$round==2)
    # Check if the incumbent made it to the second round
    m12$incumbent_mayor_ran[i] <- ifelse(cpf_incumbent %in% candidates_here$cpf_candidate,1,0)
    if(m12$incumbent_mayor_ran[i]==0){
      m12$incumbent_mayor_voteshare[i] <- NA
      m12$strongest_opponent_voteshare[i] <- NA
      next
    }
    votes_here_by_candidate <- subset(votes_here, votes_here$round==2) %>% 
      group_by(code_candidate) %>% # Some candidates have votes reported in multiple lines
      dplyr::summarise(votes = sum(votes))
    m12$incumbent_mayor_voteshare[i] <- sum(votes_here_by_candidate[which(votes_here_by_candidate$code_candidate==code_incumbent),"votes"][[1]]/sum(votes_here_by_candidate$votes))
    votes_here_by_opponent <- subset(votes_here_by_candidate, votes_here_by_candidate$code_candidate != code_incumbent) %>%
      arrange(desc(votes))
    m12$strongest_opponent_voteshare[i] <- sum(votes_here_by_opponent[1,"votes"][[1]]/sum(votes_here_by_candidate$votes))
  }
}

m12$mayor_reelected <- ifelse(m12$incumbent_mayor_voteshare>m12$strongest_opponent_voteshare,1,0)
m12$mayor_first_term <- ifelse(m12$mayor_reelected==0 | is.na(m12$mayor_reelected),1,0)

## Add data on party of the incumbent
m12$winner_party_pt <- NA
m12$winner_party_psdb <- NA
m12$winner_party_large_programmatic <- NA
m12$party_large_programmatic <- ifelse(m12$code_party==13|m12$code_party==45,1,0)

for(i in 1:nrow(m12)){
  incumbent_party <- m12[i,"name_party"][[1]]
  m12$winner_party_pt[i] <- ifelse(incumbent_party=="PT",1,0)
  m12$winner_party_psdb[i] <- ifelse(incumbent_party=="PSDB",1,0)
  m12$winner_party_large_programmatic[i] <- ifelse(incumbent_party=="PT" | incumbent_party=="PSDB",1,0)
}

municipal_elections_2012 <- m12 %>%
  mutate(cod_ibge=as.numeric(substr(cod_municipio,1,6))) %>%
  dplyr::select(cod_ibge, 
                mayor_first_term,
                electoral_concentration,
                incumbent_mayor_voteshare,
                party_large_programmatic)

d <- left_join(d, municipal_elections_2012)

# Export dataset and generate codebook ----------------------------------------------------------

# Select only variables that are used in the analysis

d <- d %>%
  dplyr::select(cod_escola,
                cod_ibge,
                uf,
                director_appointed,
                director_elected,
                director_civil_service,
                ideb_cont,
                ideb_target,
                ideb_gap_centered,
                treated,
                director_turnover,
                fv,
                treated_fv_appointed,
                treated_appointed,
                fv_appointed,
                treated_fv_elected,
                treated_elected,
                fv_elected,
                treated_fv_civil_service,
                treated_civil_service,
                fv_civil_service,
                log_population,
                log_gdp_percapita,
                deaths_perthousand,
                rural,
                number_workers,
                log_number_workers,
                in_settlement_land,
                in_indigenous_land,
                in_quilombola_land,
                students_per_classroom,
                school_inse,
                female,
                age_below40,
                age_forties,
                age_above50,
                race_white,
                race_black_or_brown,
                race_other,
                schooling_lessthantertiary,
                schooling_tertiary,
                schooling_postgraduate,
                exclusive_dedication,
                teacher_experience_lessthan6yr,
                teacher_experience_6to15yr,
                teacher_experience_over15yr, 
                director_experience_lessthan3yr,
                director_experience_3to10yr,
                director_experience_over10yr,
                director_here_lessthan3yr,
                director_here_3to10yr,
                director_here_over10yr,
                ideb_2011,
                test_scores_2011,
                passing_2011,
                mayor_first_term,
                electoral_concentration,
                party_large_programmatic)

# Export dataset
write_csv(d, "../../datasets/analysis/rdd/rdd_analysis.csv")

# Generate codebook
rdd_analysis <- read_csv("../../datasets/analysis/rdd/rdd_analysis.csv")

var_label(rdd_analysis) <- list(
  cod_escola = "School identifier (code assigned by INEP)",
  cod_ibge = "Municipality identifier (code assigned by IBGE, NA = missing)", 
  uf = "State identifier (code assigned by IBGE, NA = missing)",
  director_appointed = "Indicator for whether the school director declares in the 2013 survey being politically appointed (1 = Yes, 0 = No, NA = missing)",
  director_elected = "Indicator for whether the school director declares in the 2013 survey being elected (1 = Yes, 0 = No, NA = missing)",
  director_civil_service = "Indicator for whether the school director declares in the 2013 survey being in the civil service (1 = Yes, 0 = No, NA = missing)",
  ideb_cont = "2013 IDEB score, without the rounding applied by the Ministry (NA = missing)",
  ideb_target = "2013 IDEB target (NA = missing)",
  ideb_gap_centered = "Distance between the 2013 IDEB score and the 2013 IDEB target, centered (NA = missing)",
  treated = "Indicator for whether the school met its IDEB target (1 = Yes, 0 = No, NA = missing)",
  director_turnover = "Indicator for whether the school director declares in the 2015 survey having been in their post for less than a year (1 = Yes, 0 = No, NA = missing)",
  fv = "RD forcing variable (equal to ideb_gap_centered, NA = missing)",
  treated_fv_appointed = "Interaction (treated) x (fv) x (director_appointed) (NA = missing)",
  treated_appointed = "Interaction (treated) x (director_appointed) (NA = missing)",
  fv_appointed = "Interaction (fv) x (director_appointed) (NA = missing)",
  treated_fv_elected = "Interaction (treated) x (fv) x (director_elected) (NA = missing)",
  treated_elected = "Interaction (treated) x (director_elected) (NA = missing)",
  fv_elected = "Interaction (fv) x (director_elected) (NA = missing)",
  treated_fv_civil_service = "Interaction (treated) x (fv) x (director_civil_service) (NA = missing)",
  treated_civil_service = "Interaction (treated) x (director_civil_service) (NA = missing)",
  fv_civil_service = "Interaction (fv) x (director_civil_service) (NA = missing)",
  log_population = "Municipality population, logged (NA = missing)",
  log_gdp_percapita = "Municipality GDP per capita, logged (NA = missing)",
  deaths_perthousand = "Number of deaths in the municipality per 1,000 inhabitants (NA = missing)",
  rural = "Indicator for whether the school is in a rural area (1 = Yes, 0 = No, NA = missing)",
  number_workers = "Number of school staff (NA = missing)",
  log_number_workers = "Number of school staff, logged (NA = missing)",
  in_settlement_land = "Indicator for whether the school is in a settlement (1 = Yes, 0 = No, NA = missing)",
  in_indigenous_land = "Indicator for whether the school is in indigenous land (1 = Yes, 0 = No, NA = missing)",
  in_quilombola_land = "Indicator for whether the school is in quilombola land (1 = Yes, 0 = No, NA = missing)",
  students_per_classroom = "Number of students per classroom (NA = missing)",
  school_inse = "School socioeconomic index (higher indices denote better socioeconomic conditions) (NA = missing)",
  female = "Indicator for whether the school director declares in the 2013 survey being female (1 = Yes, 0 = No, NA = missing)",
  age_below40 = "Indicator for whether the school director declares in the 2013 survey being younger than 40 (1 = Yes, 0 = No, NA = missing)",
  age_forties = "Indicator for whether the school director declares in the 2013 survey being 40-49 years old (1 = Yes, 0 = No, NA = missing)",
  age_above50 = "Indicator for whether the school director declares in the 2013 survey being older than 50 (1 = Yes, 0 = No, NA = missing)",
  race_white = "Indicator for whether the school director declares in the 2013 survey being white (1 = Yes, 0 = No, NA = missing)",
  race_black_or_brown = "Indicator for whether the school director declares in the 2013 survey being black or brown (1 = Yes, 0 = No, NA = missing)",
  race_other = "Indicator for whether the school director declares in the 2013 survey being in another racial category (1 = Yes, 0 = No, NA = missing)",
  schooling_lessthantertiary = "Indicator for whether the school director declares in the 2013 survey having less than a tertiary education diploma (1 = Yes, 0 = No, NA = missing)",
  schooling_tertiary = "Indicator for whether the school director declares in the 2013 survey having a tertiary education diploma (1 = Yes, 0 = No, NA = missing)",
  schooling_postgraduate = "Indicator for whether the school director declares in the 2013 survey having a postgraduate education diploma (1 = Yes, 0 = No, NA = missing)",
  exclusive_dedication = "Indicator for whether the school director declares in the 2013 survey not having another job (1 = Yes, 0 = No, NA = missing)",
  teacher_experience_lessthan6yr = "Indicator for whether the school director declares in the 2013 having less than 6 years of teaching experience (1 = Yes, 0 = No, NA = missing)",
  teacher_experience_6to15yr = "Indicator for whether the school director declares in the 2013 having between 6 and 15 years of teaching experience (1 = Yes, 0 = No, NA = missing)",
  teacher_experience_over15yr = "Indicator for whether the school director declares in the 2013 having more than 15 years of teaching experience (1 = Yes, 0 = No, NA = missing)",
  director_experience_lessthan3yr = "Indicator for whether the school director declares in the 2013 having less than 3 years of experience as director (1 = Yes, 0 = No, NA = missing)",
  director_experience_3to10yr = "Indicator for whether the school director declares in the 2013 having between 3 and 10 years of experience as director (1 = Yes, 0 = No, NA = missing)",
  director_experience_over10yr = "Indicator for whether the school director declares in the 2013 having over 10 years of experience as director (1 = Yes, 0 = No, NA = missing)",
  director_here_lessthan3yr = "Indicator for whether the school director declares in the 2013 having spent less than 3 years as director of this school (1 = Yes, 0 = No, NA = missing)",
  director_here_3to10yr = "Indicator for whether the school director declares in the 2013 having spent between 3 and 10 years as director of this school (1 = Yes, 0 = No, NA = missing)",
  director_here_over10yr = "Indicator for whether the school director declares in the 2013 having spent over 10 years as director of this school (1 = Yes, 0 = No, NA = missing)",
  ideb_2011 = "IDEB score corresponding to the 2011 ANRESC (NA = missing)",
  test_scores_2011 = "Average student test scores corresponding to the 2011 ANRESC (NA = missing)",
  passing_2011 = "Passing rate corresponding to the 2011 ANRESC (NA = missing)",
  mayor_first_term = "Indicator for whether the municipality's mayor is in their first term (1 = Yes, 0 = No, NA = missing)",
  electoral_concentration = "Herfindahl index of electoral concentration in the 2012 municipal elections (NA = missing)",
  party_large_programmatic = "Indicator for whether the municipality's mayor elected in 2012 belonged to a large, programmatic party (PT or PSDB) (1 = Yes, 0 = No, NA = missing)")

codebook::label_browser_static(rdd_analysis)

# NOTES -- R version, platform, and loaded packages -------------------------

### sessionInfo(package = NULL)

# R version 3.6.3 (2020-02-29)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS  10.16
# 
# Matrix products: default
# LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib
# 
# Random number generation:
#   RNG:     Mersenne-Twister 
# Normal:  Inversion 
# Sample:  Rounding 
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
#   [1] grid      stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] electionsBR_0.3.1 xtable_1.8-4      cjoint_2.1.0      survey_4.0        survival_3.1-11   Matrix_1.2-18    
# [7] lmtest_0.9-37     zoo_1.8-7         sandwich_2.5-1    texreg_1.37.1     codebook_0.9.2    readxl_1.3.1     
# [13] here_1.0.1        forcats_0.5.0     stringr_1.4.0     dplyr_1.0.0       purrr_0.3.3       readr_1.4.0      
# [19] tidyr_1.0.2       tibble_3.0.0      ggplot2_3.3.5     tidyverse_1.3.0  
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