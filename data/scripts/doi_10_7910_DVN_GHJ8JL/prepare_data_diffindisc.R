### Replication code
### Article: "How patronage delivers: Political appointments, bureaucratic accountability, and service delivery in Brazil"
### Author: Guillermo Toral (www.guillermotoral.com)
### Date: June 2022
### This file prepares the difference-in-discontinuities analysis dataset, and a dataset of municipal-level 2016 election outcomes
### R version, platform, and package versions reported at the end of the file

# Prepare the environment -------------------------------------------------

### This section of the code prepares the environment 

# Clean the environment
rm(list = ls())

# Install required packages if not previously installed
package_list <- c("tidyverse", "readxl", "electionsBR", "here", "codebook") 
packages_to_install <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(packages_to_install)>0){
  install.packages(packages_to_install)
}

# Load required packages
library(tidyverse)
library(readxl)
library(electionsBR)
library(here)
library(codebook)

# Have dplyr not print information when using group_by
options(dplyr.summarise.inform = FALSE)

# Set Working Directory to wherever this file is located.
setwd(here())
# The directory where the code folder is located must also have a "plots" and a "tables" subdirectory

# Load and clean outcome data (school quality scores) --------------------------------------

# Import data with school quality scores for primary school
id <- data.frame(read_excel("../../datasets/downloaded/inep/ideb/divulgacao_anos_iniciais_escolas_2019.xlsx",skip=9)) # Omit the first 9 rows which don't include data

# Subset to municipal schools, and select variables of interest
ideb <- id[which(id$REDE=='Municipal'),
             grep('CO_MUNICIPIO|ID_ESCOLA|VL_INDICADOR_REND_2017|VL_INDICADOR_REND_2015|VL_INDICADOR_REND_2013|VL_INDICADOR_REND_2011|VL_INDICADOR_REND_2009|VL_NOTA_MEDIA_2017|VL_NOTA_MEDIA_2015|VL_NOTA_MEDIA_2013|VL_NOTA_MEDIA_2011|VL_NOTA_MEDIA_2009|VL_PROJECAO_2017|VL_OBSERVADO_2015',
                  names(id))] 

# Remove NA's
ideb <- data.frame(apply(ideb,2,as.numeric)) 

# Rename, generate and select variables of interest
ideb <- ideb %>%
  mutate(ibge = CO_MUNICIPIO, # municipality identifier
         cod_escola= ID_ESCOLA, # school identifier
         ideb_2017 = VL_INDICADOR_REND_2017*VL_NOTA_MEDIA_2017, # ideb score 2017, without rounding
         ideb_2015 = VL_INDICADOR_REND_2015*VL_NOTA_MEDIA_2015, # ideb score 2015, without rounding
         ideb_2015_mec = VL_OBSERVADO_2015, # ideb score 2015, with the rounding applied by the Ministry
         ideb_2013 = VL_INDICADOR_REND_2013*VL_NOTA_MEDIA_2013, # ideb score 2013, without rounding
         ideb_2011 = VL_INDICADOR_REND_2011*VL_NOTA_MEDIA_2011, # ideb score 2011, without rounding
         ideb_2009 = VL_INDICADOR_REND_2009*VL_NOTA_MEDIA_2009, # ideb score 2009, without rounding
         ideb_target_2017 = VL_PROJECAO_2017, # ideb target 2017
         ideb_delta = VL_INDICADOR_REND_2017*VL_NOTA_MEDIA_2017-VL_INDICADOR_REND_2015*VL_NOTA_MEDIA_2015) %>% # change in ideb scores from 2015 to 2017, without rounding
  dplyr::select(ibge, cod_escola, ideb_2017, ideb_2015, ideb_2015_mec, ideb_2013, ideb_2011, ideb_2009, ideb_target_2017, ideb_delta)

# Load and clean data on directors post-election (2017) --------------------------

# Load data on the government's survey of school directors corresponding to 2017
director_2017 <- read_csv("../../datasets/downloaded/inep/anresc/diretor_2017.csv")

# Clean data and select relevant variables
director17 <- director_2017 %>%
  filter(ID_DEPENDENCIA_ADM==3) %>% # Select municipal schools
  mutate(cod_escola = as.numeric(ID_ESCOLA), # school identifier
         # Indicators for director selection mode
         director_appointed = ifelse(TX_RESP_Q014=="C" | TX_RESP_Q014=="F",1,0),
         director_elected_2017 = ifelse(TX_RESP_Q014=="B" | TX_RESP_Q014=="E",1,0),
         director_civil_service_2017 = ifelse(TX_RESP_Q014=="A",1,0),
         # Indicators for director's tenure in their job
         director_here_lessthan1yr_2017 = ifelse(TX_RESP_Q017=="A",1,0),
         director_here_1to2yr_2017 = ifelse(TX_RESP_Q017=="B",1,0),
         director_here_3to5yr_2017 = ifelse(TX_RESP_Q017=="C",1,0),
         ## Mechanisms
         support_from_above_2017 = ifelse(TX_RESP_Q078=="A",1,0),
         insufficient_financial_resources_2017 = ifelse(TX_RESP_Q067=="A",0,ifelse(TX_RESP_Q067=="B",1,ifelse(TX_RESP_Q067=="C",2,ifelse(TX_RESP_Q067=="D",3,NA)))),
         insufficient_teachers_2017 = ifelse(TX_RESP_Q068=="A",0,ifelse(TX_RESP_Q068=="B",1,ifelse(TX_RESP_Q068=="C",2,ifelse(TX_RESP_Q068=="D",3,NA)))),
         teacher_turnover_2017 = ifelse(TX_RESP_Q075=="A",0,ifelse(TX_RESP_Q075=="B",1,ifelse(TX_RESP_Q075=="C",2,ifelse(TX_RESP_Q075=="D",3,NA))))) %>%
  dplyr::select(cod_escola, director_appointed, director_elected_2017, director_civil_service_2017, director_here_lessthan1yr_2017, director_here_1to2yr_2017, director_here_3to5yr_2017,
                support_from_above_2017, insufficient_financial_resources_2017, insufficient_teachers_2017, teacher_turnover_2017)

# Load and clean data on directors pre-election (2015) --------------------------

# Load data on the government's survey of school directors corresponding to 2015
director_2015 <- read_csv("../../datasets/downloaded/inep/anresc/diretor_2015.csv")

# Clean data and select relevant variables
director15 <- director_2015 %>%
  filter(ID_DEPENDENCIA_ADM==3) %>% # Select municipal schools
  mutate(cod_escola = as.numeric(ID_ESCOLA), # School identifer
         # Selection mode
         director_appointed_2015 = ifelse(TX_RESP_Q014=="C" | TX_RESP_Q014=="F",1,0),
         director_elected_2015 = ifelse(TX_RESP_Q014=="B" | TX_RESP_Q014=="E",1,0),
         director_civil_service_2015 = ifelse(TX_RESP_Q014=="A",1,0),
         # Gender
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
         director_here_lessthan3yr_2015 = ifelse(TX_RESP_Q017=="A"|TX_RESP_Q017=="B",1,0),
         director_here_3to10yr = ifelse(TX_RESP_Q017=="C"|TX_RESP_Q017=="D",1,0),
         director_here_over10yr = ifelse(TX_RESP_Q017=="E"|TX_RESP_Q017=="F"|TX_RESP_Q017=="G",1,0),
         ## Mechanisms
         support_from_above_2015 = ifelse(TX_RESP_Q078=="A",1,0),
         insufficient_financial_resources_2015 = ifelse(TX_RESP_Q067=="A",0,ifelse(TX_RESP_Q067=="B",1,ifelse(TX_RESP_Q067=="C",2,ifelse(TX_RESP_Q067=="D",3,NA)))),
         insufficient_teachers_2015 = ifelse(TX_RESP_Q068=="A",0,ifelse(TX_RESP_Q068=="B",1,ifelse(TX_RESP_Q068=="C",2,ifelse(TX_RESP_Q068=="D",3,NA)))),
         teacher_turnover_2015 = ifelse(TX_RESP_Q075=="A",0,ifelse(TX_RESP_Q075=="B",1,ifelse(TX_RESP_Q075=="C",2,ifelse(TX_RESP_Q075=="D",3,NA))))) %>%
  dplyr::select(cod_escola, director_appointed_2015, director_elected_2015, director_civil_service_2015, female, age_below40, age_forties, age_above50, race_white, race_black_or_brown, race_other, 
                schooling_lessthantertiary, schooling_tertiary, schooling_postgraduate, exclusive_dedication, teacher_experience_lessthan6yr, teacher_experience_6to15yr, teacher_experience_over15yr,
                director_experience_lessthan3yr, director_experience_3to10yr, director_experience_over10yr, director_here_lessthan3yr_2015, 
                support_from_above_2015, insufficient_financial_resources_2015, insufficient_teachers_2015, teacher_turnover_2015)

# Merge IDEB and director data, and add municipality identifiers ----------

s <- left_join(ideb, director17, by="cod_escola")
s <- left_join(s, director15, by="cod_escola")

# Merge with school's municipality identifiers, as obtained from the official school census
e <- read_delim("../../datasets/downloaded/inep/censo_escolar/escolas_2017.csv",delim="|") 
ee <- e %>%
  mutate(cod_escola = CO_ENTIDADE, # school identifier
         cod_municipio = CO_MUNICIPIO) %>% # municipality identifier
  dplyr::select(cod_escola, cod_municipio)

s <- left_join(s, ee, by="cod_escola")

# Generate 6-digit municipality code, and 2-digit state (UF, i.e. unit of the federation) code
s$uf <- substr(s$cod_municipio,1,2)
s$cod_ibge <- as.numeric(substr(s$cod_municipio,1,6))

# Generate deltas for mechanisms
s$support_from_above_delta <- s$support_from_above_2017-s$support_from_above_2015
s$insufficient_financial_resources_delta <- s$insufficient_financial_resources_2017-s$insufficient_financial_resources_2015
s$insufficient_teachers_delta <- s$insufficient_teachers_2017-s$insufficient_teachers_2015
s$teacher_turnover_delta <- s$teacher_turnover_2017-s$teacher_turnover_2015

# Load, clean, and merge data on elections ----------------------------------------

# Load data on candidates and their performance in local elections from 2000 to 2016, from Brazil's Supreme Electoral Court (TSE) via the "electionsBR" package
load("../../datasets/downloaded/tse/tse_elections_2000_to_2016.RData") # files starting with d and v have data on candidates and candidates' performance by year. Numbers correspond to election years, from 2000 to 2016

# List of categories of candidates to exclude -- these are candidates whose candidacy was not validated or was cancelled by the electoral justice, or who died
invalid <- c("INDEFERIDO", "INDEFERIDO POR IMPUGNAÇÃO", "FALECIDO", "CASSADO", "CASSAÇÃO DO REGISTRO", "CANCELAMENTO", "CANCELADO", "INELEGÍVEL")

# Identify mayoral candidates running for the 2016 - 2020 term
candidates_1620 <- d16 %>% 
  # Exclude candidates other than valid candidates for mayor who got elected
  dplyr::filter(DESCRICAO_CARGO=="PREFEITO" & # Retain only candidates for mayoral office
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
mayors_1620 <- candidates_1620 %>% 
  dplyr::filter(candidate_result == "ELEITO") # Keep elected ones 
# Identify municipalities where we observe multiple mayors being elected
repeated_muns <- mayors_1620[duplicated(mayors_1620$code_municipality_tse),] # 28
repeated_elections <- mayors_1620[which(mayors_1620$code_municipality_tse %in% repeated_muns$code_municipality_tse),] 
# Identify municipalities where we observe supplementary elections
supplementary_elections_1620 <- repeated_elections[which(repeated_elections$election_description!="Eleições Municipais 2016"),]
supplementary_elections_1620$supplementary_election <- 1
# Remove observations where there are repeated elections
candidates_1620 <- candidates_1620 %>% 
  dplyr::filter(!(code_municipality_tse %in% repeated_elections$code_municipality_tse))
candidates_1620$supplementary_election <- 0
# Add the supplementary elections
candidates_1620 <- rbind(candidates_1620, supplementary_elections_1620)
# Filter the mayors file
mayors_1620 <- candidates_1620 %>% 
  dplyr::filter(candidate_result == "ELEITO") # Keep elected ones 
nrow(mayors_1620)==n_distinct(mayors_1620$code_municipality_tse) # 5550

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

# Votes in the 2016 election
votes_1620 <- v16 %>%
  # Exclude candidates other than approved candidates who ran for mayor, did not have their candidacies voided
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
votes_1620_regular <- votes_1620 %>%
  dplyr::filter(!(code_municipality_tse %in% supplementary_elections_1620$code_municipality_tse))
votes_1620_supplementary <- votes_1620 %>%
  dplyr::filter(election_description != "ELEIÇÕES MUNICIPAIS 2016")
votes_1620 <- rbind(votes_1620_regular, votes_1620_supplementary)

### Merge with municipal identifiers

# Obtained from basedosdados
municipality_identifiers <- read_csv("../../datasets/downloaded/basedosdados/basedosdados_municipality_identifiers.csv")
municipalities <- municipality_identifiers %>%
  mutate(cod_ibge = id_municipio_6,
         cod_tse = id_municipio_tse) %>%
  dplyr::select(cod_ibge, cod_tse)

# Generate the 5-digit version of the TSE code (with leading zeroes)
municipalities$code_municipality_tse <- str_pad(as.character(municipalities$cod_tse), 5, side = "left", pad = "0") # Add leading zeroes (TSE codes are all 5-digit)

# Merge mayor data, keeping separate datasets for each election cycle
m16 <- left_join(municipalities, mayors_1620)
m12 <- left_join(municipalities, mayors_1216)

### Measure electoral peformance of the incumbent and the strongest opponent

# 2016 election 
m16$incumbent_mayor_ran <- NA
m16$total_votes <- NA
m16$electoral_concentration <- NA
m16$incumbent_mayor_voteshare <- NA
m16$strongest_opponent_voteshare <- NA

for(i in 1:nrow(m16)){
  # Get votes in the municipality
  votes_here <- subset(votes_1620, votes_1620$code_municipality_tse == m16$code_municipality_tse[i] & votes_1620$round==1)
  # List of candidates
  candidates_here <- subset(candidates_1620, candidates_1620$code_municipality_tse == m16$code_municipality_tse[i] & candidates_1620$round==1)
  # Check if the incumbent ran
  cpf_incumbent <- m12[which(m12$code_municipality_tse==m16$code_municipality_tse[i]),"cpf_candidate"][[1]]
  m16$incumbent_mayor_ran[i] <- ifelse(cpf_incumbent %in% candidates_here$cpf_candidate,1,0)
  votes_here_by_candidate <- subset(votes_here, votes_here$round==1) %>% 
    group_by(code_candidate) %>% # Some candidates have votes reported in multiple lines
    dplyr::summarise(votes = sum(votes))
  m16$electoral_concentration[i] <- sum((votes_here_by_candidate$votes/sum(votes_here_by_candidate$votes))^2)
  # Vote numbers in the first round
  # Sum number of votes
  m16$total_votes[i] <- sum(votes_here_by_candidate$votes)
  # Vote share of incumbent
  if(m16$incumbent_mayor_ran[i]==0){
    next
  }
  code_incumbent <- candidates_here[which(candidates_here$cpf_candidate==cpf_incumbent & candidates_here$round==1),"code_candidate"][[1]]
  m16$incumbent_mayor_voteshare[i] <- sum(votes_here_by_candidate[which(votes_here_by_candidate$code_candidate==code_incumbent),"votes"][[1]]/sum(votes_here_by_candidate$votes))
  votes_here_by_opponent <- subset(votes_here_by_candidate, votes_here_by_candidate$code_candidate != code_incumbent) %>%
    arrange(desc(votes))
  m16$strongest_opponent_voteshare[i] <- sum(votes_here_by_opponent[1,"votes"][[1]]/sum(votes_here_by_candidate$votes))
  if(m16$round==2){ # If the election was decided in the second round, record electoral performance in that round instead of the first one
    # Vote tallies
    votes_here <- subset(votes_1620, votes_1620$code_municipality_tse == m16$code_municipality_tse[i] & votes_1620$round==2)
    # List of candidates
    candidates_here <- subset(candidates_1620, candidates_1620$code_municipality_tse == m16$code_municipality_tse[i] & candidates_1620$round==2)
    # Check if the incumbent made it to the second round
    m16$incumbent_mayor_ran[i] <- ifelse(cpf_incumbent %in% candidates_here$cpf_candidate,1,0)
    if(m16$incumbent_mayor_ran[i]==0){
      m16$incumbent_mayor_voteshare[i] <- NA
      m16$strongest_opponent_voteshare[i] <- NA
      next
    }
    votes_here_by_candidate <- subset(votes_here, votes_here$round==2) %>% 
      group_by(code_candidate) %>% # Some candidates have votes reported in multiple lines
      dplyr::summarise(votes = sum(votes))
    m16$incumbent_mayor_voteshare[i] <- sum(votes_here_by_candidate[which(votes_here_by_candidate$code_candidate==code_incumbent),"votes"][[1]]/sum(votes_here_by_candidate$votes))
    votes_here_by_opponent <- subset(votes_here_by_candidate, votes_here_by_candidate$code_candidate != code_incumbent) %>%
      arrange(desc(votes))
    m16$strongest_opponent_voteshare[i] <- sum(votes_here_by_opponent[1,"votes"][[1]]/sum(votes_here_by_candidate$votes))
  }
}

# Remove observations where the winner was decided in a supplementary elections
# These elections take place when the election held at the regular date is deemed invalid by the electoral courts
# Unfortunately TSE does not report when these elections are held, but they are typically months or years after the official date.
# Less than 1% of municipalities in the data have supplementary elections. Leaving them in the data does not alter the results. 

m16 <- subset(m16, m16$supplementary_election==0)

municipal_elections_2016 <- m16 %>%
  mutate(challenger_margin = strongest_opponent_voteshare - incumbent_mayor_voteshare,
         mayor_lost = ifelse(challenger_margin>0,1,0)) %>%
  dplyr::select(cod_ibge,
                electoral_concentration,
                incumbent_mayor_ran,
                incumbent_mayor_voteshare,
                strongest_opponent_voteshare,
                challenger_margin,
                mayor_lost)

s <- left_join(s, municipal_elections_2016)

# Load, clean, and merge covariates: Municipal sociodemographics ---------------------

# Load data on municipal covariates, obtained from basedosdados
m <- read_csv("../../datasets/downloaded/basedosdados/basedosdados_municipality_covariates.csv")

municipality_covariates_2015 <- m %>%
  dplyr::filter(year==2015) %>%
  mutate(cod_municipio = ibge,
         log_population = log(population),
         log_gdp_percapita = log(gdppercapita)) %>%
  dplyr::select(cod_municipio, 
                log_population,
                log_gdp_percapita,
                deaths_perthousand)

s <- left_join(s, municipality_covariates_2015, by="cod_municipio")

# Load, clean, and merge covariates: School census ------------------

## Load school census data for 2015
e <- read_delim("../../datasets/downloaded/inep/censo_escolar/escolas_2015.csv",delim="|") 

school_census <- e %>% 
  mutate(cod_escola = CO_ENTIDADE,
         rural = ifelse(TP_LOCALIZACAO==2,1,0),
         number_workers = NU_FUNCIONARIOS,
         in_settlement_land = ifelse(TP_LOCALIZACAO_DIFERENCIADA==1,1,0),
         in_indigenous_land = ifelse(TP_LOCALIZACAO_DIFERENCIADA==2,1,0),
         in_quilombola_land = ifelse(TP_LOCALIZACAO_DIFERENCIADA==3,1,0)) %>%
  dplyr::select(cod_escola, rural, number_workers, in_settlement_land, in_indigenous_land, in_quilombola_land)

s <- left_join(s, school_census)

# Load, clean, and merge covariates: School educational indicators -------

## Average number of students per classroom (skip the first 9 rows of the file which do not contain data)
a <- data.frame(read_excel("../../datasets/downloaded/inep/indicadores_educacionais/ATU_ESCOLAS_2015.xlsx",skip=9))
a <- a[,grep("CO_ENTIDADE|ATU_FUN",names(a))] # Keep only measures for municipal schools, and variable of interest
a <- data.frame(apply(a,2,as.numeric)) # Remove NA's

# Rename and select variables of interest
atu <- a %>%
  mutate(cod_escola = CO_ENTIDADE, 
         students_per_classroom = ATU_FUN) %>%
  dplyr::select(cod_escola, students_per_classroom)

s <- left_join(s, atu)

## School socioeconomic index
inse <- data.frame(read_excel("../../datasets/downloaded/inep/indicadores_educacionais/INSE_2015.xlsx",skip=9))
inse <- inse[,grep("CO_ESCOLA|INSE_VALOR_ABSOLUTO",names(inse))] # Keep only measures for municipal schools, and variable of interest
inse <- data.frame(apply(inse,2,as.numeric)) # Remove NA's

# Rename and select variables of interest
school_socioeconomic_index <- inse %>%
  mutate(cod_escola = CO_ESCOLA, 
         school_inse = INSE_VALOR_ABSOLUTO) %>%
  dplyr::select(cod_escola, school_inse)

s <- left_join(s, school_socioeconomic_index)

# Load, clean, and merge covariates: Directors stability by 2019 ----------

director_2019 <- read_csv("../../datasets/downloaded/inep/anresc/diretor_2019.csv")
director19 <- director_2019 %>%
  filter(ID_DEPENDENCIA_ADM==3) %>% # Select municipal schools
  mutate(cod_escola = as.numeric(ID_ESCOLA),
         director_here_years_2019 = TX_RESP_Q006) %>% # Q0006 corresponds to a question asking the director how many years they have been in their post
  dplyr::select(cod_escola, director_here_years_2019)

s <- left_join(s, director19)

# Export municipal-level election dataset and generate codebook -----------

# Select only variables that are used in the analysis
m <- municipal_elections_2016 %>%
  mutate(winner_voteshare = ifelse(incumbent_mayor_voteshare > strongest_opponent_voteshare, incumbent_mayor_voteshare, strongest_opponent_voteshare),
         winner_margin = ifelse(incumbent_mayor_voteshare > strongest_opponent_voteshare, incumbent_mayor_voteshare - strongest_opponent_voteshare, strongest_opponent_voteshare - incumbent_mayor_voteshare)) %>%
  dplyr::select(cod_ibge,
                challenger_margin,
                winner_margin,
                winner_voteshare,
                electoral_concentration,
                mayor_lost)

# Export dataset
write_csv(m, "../../datasets/analysis/other/municipal_elections_2016.csv")

# Generate codebook
municipal_elections_2016 <- read_csv("../../datasets/analysis/other/municipal_elections_2016.csv")

var_label(municipal_elections_2016) <- list(
  cod_ibge = "Municipality identifier (code assigned by IBGE)", 
  challenger_margin = "Vote margin of the strongest challenger to the incumbent in the 2016 election (NA = missing)",
  winner_margin = "Vote margin of the winner in the 2016 election (NA = missing)",
  winner_voteshare = "Vote share of the winner in the 2016 election (NA = missing)",
  electoral_concentration = "Herfindahl index of electoral concentration in the 2016 election (NA = missing)",
  mayor_lost = "Indicator for whether the sitting mayor lost the 2016 election (NA = missing)"
)
  
codebook::label_browser_static(municipal_elections_2016)

# Export diff-in-disc analysis dataset and generate codebook -----------

# Select only variables that are used in the analysis
dd <- s %>%
  dplyr::select(# School, municipality, and state identifiers
                cod_escola,
                cod_ibge,
                uf,
                # School quality scores
                ideb_2017,
                ideb_2015,
                ideb_2015_mec,
                ideb_2013,
                ideb_2011,
                ideb_2009,
                ideb_delta,
                ideb_target_2017,
                # Director selection modes
                director_appointed,
                director_appointed_2015,
                director_elected_2015,
                director_civil_service_2015,
                # Director tenure in the job
                director_here_years_2019,
                director_here_lessthan1yr_2017,
                director_here_1to2yr_2017,
                director_here_3to5yr_2017,
                director_here_lessthan3yr_2015,
                # Director answers about problems in the school
                support_from_above_delta,
                insufficient_financial_resources_delta,
                insufficient_teachers_delta,
                teacher_turnover_delta,
                # Director covariates
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
                # School covariates
                rural,
                number_workers,
                in_settlement_land,
                in_indigenous_land,
                in_quilombola_land,
                students_per_classroom,
                school_inse,
                # Municipality election outcomes and covariates
                challenger_margin,
                mayor_lost,
                electoral_concentration,
                log_gdp_percapita,
                log_population,
                deaths_perthousand)

# Export dataset
write_csv(dd, "../../datasets/analysis/diffindisc/diffindisc_analysis.csv")

# Generate codebook
diffindisc_analysis <- read_csv("../../datasets/analysis/diffindisc/diffindisc_analysis.csv")

var_label(diffindisc_analysis) <- list(
  cod_escola = "School identifier (generated by INEP)",
  cod_ibge = "Municipality identifier (generated by IBGE) (NA = missing)",
  uf = "State identifier (generated by IBGE) (NA = missing)",
  ideb_2017 = "IDEB school quality score for 2017 (NA = missing)",
  ideb_2015 = "IDEB school quality score for 2015 (NA = missing)",
  ideb_2015_mec = "IDEB school quality score for 2015, with the rounding applied by the Ministry (NA = missing)",
  ideb_2013 = "IDEB school quality score for 2013 (NA = missing)",
  ideb_2011 = "IDEB school quality score for 2011 (NA = missing)",
  ideb_2009 = "IDEB school quality score for 2009 (NA = missing)",
  ideb_delta = "Difference in the IDEB school quality score from 2015 to 2017 (NA = missing)",
  ideb_target_2017 = "IDEB school quality target for 2017 (NA = missing)",
  director_appointed = "Indicator for whether the school director reports being appointed in the 2017 ANRESC director survey (1 = Yes, 0 = No, NA = missing)",
  director_appointed_2015 = "Indicator for whether the school director reports being appointed in the 2015 ANRESC director survey (1 = Yes, 0 = No, NA = missing)",
  director_elected_2015 = "Indicator for whether the school director reports being elected in the 2015 ANRESC director survey (1 = Yes, 0 = No, NA = missing)",
  director_civil_service_2015 = "Indicator for whether the school director reports being civil service in the 2015 ANRESC director survey (1 = Yes, 0 = No, NA = missing)",
  director_here_years_2019 = "Number of years the school director reports having been in their post in the 2019 ANRESC director survey (NA = missing)",
  director_here_lessthan1yr_2017 = "Indicator for whether the school director reports having been in their post for less than 1 year in the 2017 ANRESC director survey (1 = Yes, 0 = No, NA = missing)",
  director_here_1to2yr_2017 = "Indicator for whether the school director reports having been in their post for 1-2 years in the 2017 ANRESC director survey (1 = Yes, 0 = No, NA = missing)",
  director_here_3to5yr_2017 = "Indicator for whether the school director reports having been in their post for 3-5 years in the 2017 ANRESC director survey (1 = Yes, 0 = No, NA = missing)",
  director_here_lessthan3yr_2015 = "Indicator for whether the school director reports having been in their post for less than 3 years in the 2015 ANRESC director survey (1 = Yes, 0 = No, NA = missing)",
  support_from_above_delta = "Difference in the school director answer to the question 'Is there support from higher instances?' (relative to school management), between the ANRESC director surveys of 2015 and 2017 (original question is binary, 1 = Yes and 0 = No, NA = missing)",
  insufficient_financial_resources_delta = "Difference in the school director answer to the question 'Was the functioning of the school hindered by insufficient financial resources?', between the ANRESC director surveys of 2015 and 2017 (original question uses a 4-point scale, 0 = No, 1 = Yes, a little, 2 = Yes, moderately, 3 = Yes, a lot, NA = missing)",
  insufficient_teachers_delta = "Difference in the school director answer to the question 'Was the functioning of the school hindered by insufficient teachers for some subjects or classrooms?', between the ANRESC director surveys of 2015 and 2017 (original question uses a 4-point scale, 0 = No, 1 = Yes, a little, 2 = Yes, moderately, 3 = Yes, a lot, NA = missing)",
  teacher_turnover_delta = "Difference in the school director answer to the question 'Was the functioning of the school hindered by high levels of teacher turnover?', between the ANRESC director surveys of 2015 and 2017 (original question uses a 4-point scale, 0 = No, 1 = Yes, a little, 2 = Yes, moderately, 3 = Yes, a lot, NA = missing)",
  female = "Indicator for whether the school director reports being female in the 2015 ANRESC director survey (1 = Yes, 0 = No, NA = missing)",
  age_below40 = "Indicator for whether the school director reports being less than 40 years old in the 2015 ANRESC director survey (1 = Yes, 0 = No, NA = missing)",
  age_forties = "Indicator for whether the school director reports being 40-49 years old in the 2015 ANRESC director survey (1 = Yes, 0 = No, NA = missing)",
  age_above50 = "Indicator for whether the school director reports being above 50 years old in the 2015 ANRESC director survey (1 = Yes, 0 = No, NA = missing)",
  race_white = "Indicator for whether the school director reports being white in the 2015 ANRESC director survey (1 = Yes, 0 = No, NA = missing)",
  race_black_or_brown = "Indicator for whether the school director reports being black or brown in the 2015 ANRESC director survey (1 = Yes, 0 = No, NA = missing)",
  race_other = "Indicator for whether the school director reports being neither white, black, or brown in the 2015 ANRESC director survey (1 = Yes, 0 = No, NA = missing)",
  schooling_lessthantertiary = "Indicator for whether the school director reports having completed less than a tertiary degree in the 2015 ANRESC director survey (1 = Yes, 0 = No, NA = missing)",
  schooling_tertiary = "Indicator for whether the school director reports having completed a tertiary degree in the 2015 ANRESC director survey (1 = Yes, 0 = No, NA = missing)",
  schooling_postgraduate = "Indicator for whether the school director reports having completed a postgraduate degree in the 2015 ANRESC director survey (1 = Yes, 0 = No, NA = missing)",
  exclusive_dedication = "Indicator for whether the school director reports having no other job in the 2015 ANRESC director survey (1 = Yes, 0 = No, NA = missing)",
  teacher_experience_lessthan6yr = "Indicator for whether the school director reports having less than 6 years of teaching experience in the 2015 ANRESC director survey (1 = Yes, 0 = No, NA = missing)",
  teacher_experience_6to15yr = "Indicator for whether the school director reports having 6-15 years of teaching experience in the 2015 ANRESC director survey (1 = Yes, 0 = No, NA = missing)",
  teacher_experience_over15yr = "Indicator for whether the school director reports having over 15 years of teaching experience in the 2015 ANRESC director survey (1 = Yes, 0 = No, NA = missing)",
  director_experience_lessthan3yr = "Indicator for whether the school director reports having less than 3 years of director experience in the 2015 ANRESC director survey (1 = Yes, 0 = No, NA = missing)",
  director_experience_3to10yr = "Indicator for whether the school director reports having 3-10 years of director experience in the 2015 ANRESC director survey (1 = Yes, 0 = No, NA = missing)",
  director_experience_over10yr = "Indicator for whether the school director reports having over 10 years of director experience in the 2015 ANRESC director survey (1 = Yes, 0 = No, NA = missing)",
  rural = "Indicator for whether the school is in a rural area (1 = Yes, 0 = No, NA = missing)",
  number_workers = "Number of staff employed at the school (NA = missing)",
  in_settlement_land = "Indicator for whether the school is in a settlement land (1 = Yes, 0 = No, NA = missing)",
  in_indigenous_land = "Indicator for whether the school is in an indigenous land (1 = Yes, 0 = No, NA = missing)",
  in_quilombola_land = "Indicator for whether the school is in a quilombola land (1 = Yes, 0 = No, NA = missing)",
  students_per_classroom = "Average number of students per classroom in the school (NA = missing)",
  school_inse = "School socioeconomic index (higher indices denote better socioeconomic conditions) (NA = missing)",
  challenger_margin = "Vote margin of the strongest challenger to the incumbent in the 2016 municipal election (NA = missing)",
  mayor_lost = "Indicator for whether the sitting mayor lost the 2016 municipal election (1 = Yes, 0 = No, NA = missing)",
  electoral_concentration = "Herfindahl index of electoral concentration in the 2016 municipal election (ranges from 0 to 1, with higher values denoting more concentration of votes in less candidates) (NA = missing)",
  log_gdp_percapita = "Municipality GDP per capita in 2015, logged (NA = missing)",
  log_population = "Municipality population in 2015, logged (NA = missing)",
  deaths_perthousand = "Municipality deaths per 1,000 residents (NA = missing)")

codebook::label_browser_static(diffindisc_analysis)

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
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] electionsBR_0.3.1 readxl_1.3.1      forcats_0.5.0     stringr_1.4.0     dplyr_1.0.0       purrr_0.3.3      
# [7] readr_1.4.0       tidyr_1.0.2       tibble_3.0.0      ggplot2_3.3.5     tidyverse_1.3.0  
# 
# loaded via a namespace (and not attached):
#   [1] Rcpp_1.0.7       cellranger_1.1.0 pillar_1.4.3     compiler_3.6.3   dbplyr_1.4.2     tools_3.6.3     
# [7] jsonlite_1.6.1   lubridate_1.7.4  lifecycle_0.2.0  nlme_3.1-145     gtable_0.3.0     lattice_0.20-40 
# [13] pkgconfig_2.0.3  rlang_1.0.0      reprex_0.3.0     DBI_1.1.0        cli_2.3.0        rstudioapi_0.11 
# [19] yaml_2.2.1       haven_2.3.1      withr_2.1.2      xml2_1.3.0       httr_1.4.1       fs_1.4.1        
# [25] generics_0.0.2   vctrs_0.3.1      hms_0.5.3        grid_3.6.3       tidyselect_1.1.0 glue_1.3.2      
# [31] R6_2.4.1         modelr_0.1.6     magrittr_1.5     backports_1.1.5  scales_1.1.0     ellipsis_0.3.0  
# [37] rvest_0.3.5      assertthat_0.2.1 colorspace_1.4-1 stringi_1.4.6    munsell_0.5.0    broom_0.5.5     
# [43] crayon_1.3.4   