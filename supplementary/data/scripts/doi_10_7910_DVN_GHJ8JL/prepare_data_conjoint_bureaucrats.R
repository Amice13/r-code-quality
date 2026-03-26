### Replication code
### Article: "How patronage delivers: Political appointments, bureaucratic accountability, and service delivery in Brazil"
### Author: Guillermo Toral (www.guillermotoral.com)
### Date: June 2022
### This file prepares the bureaucrat conjoint experiment data
### R version, platform, and package versions reported at the end of the file

### Note that this file uses the original survey data, which are not included in the replication package to protect human subjects
### The author will not share the original survey files to protect subjects (which are easily identifiable). However requests for descriptive statistics can be sent to the author (contact info at www.guillermotoral.com)

# Prepare the environment -------------------------------------------------

### This section of the code prepares the environment 

# Clean the environment
rm(list = ls())

# Install required packages if not previously installed
package_list <- c("tidyverse", "here", "plyr", "codebook") 
packages_to_install <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(packages_to_install)>0){
  install.packages(packages_to_install)
}

# Load required packages
library(tidyverse)
library(here)
library(codebook)

# Set Working Directory to wherever this file is located.
setwd(here())
# The directory where the code folder is located must also have a "plots" and a "tables" subdirectory

# Load and clean original school director conjoint dataset ------------------------------------------------------------

# Load the original dataset (not included in the replication package to protect human subjects)
d <- read_csv("../../datasets/collected/survey_bureaucrats/survey_escolas_original.csv") 

# Create vectors that are used below to clean the data
answer_categories <- c("Not at all", "A little", "Quite", "A lot")
answer_categories_very <- c("Not at all", "A little", "Quite", "Very")

# Remove column headers as produced by Qualtrics 
d <- d[c(3:nrow(d)),] 

# Remove pilot responses that were not produced by target participants 
d <- subset(d, d$StartDate>="2018-11-26")

# Remove entries from subjects who did not agree to participate
d <- subset(d, d$p6=="YES, they wish to participate in the survey")

# Rename and recode variables in the original survey dataset that will be used in the analysis
escola_managers <- d %>%
  mutate(respondent_id = ResponseId,
         municipality = p2_1, 
         appointed = ifelse(p16 == "Appointment", 1, 0),
         alignment_mayor_professionals = match(p44_2, answer_categories),
         mayor_concerned = match(p44_3, answer_categories),
         secretariat_holds_us_accountable = match(p44_8, answer_categories),
         secretariat_supports_us_toimprove_results = match(p44_10, answer_categories),
         sees_politicians_as_programmatic = ifelse(alignment_mayor_professionals == 4 &
                                                   mayor_concerned == 4 &
                                                   secretariat_holds_us_accountable == 4 &
                                                   secretariat_supports_us_toimprove_results == 4, 1, 0),
         sector = "education")

# Collect data on profiles and choices in round 1

conjoint_schools_1a <- escola_managers %>%
  mutate(conjoint_round = 1,
         experience = `Rd_1_A_Experiencia como diretor`,
         education = Rd_1_A_Formacao, 
         selection = Rd_1_A_Selecao,
         political_connections = `Rd_1_A_Vinculos politicos`,
         unit_performance = `Rd_1_A_Desempenho no IDEB`,
         relationship_to_professionals = `Rd_1_A_Relacao com os professores`,
         chosen_reform = ifelse(p50_2=="Director A",1,0),
         chosen_performance = ifelse(p50_3=="Director A",1,0),
         chosen_implementation = ifelse(p50_5=="Director A",1,0),
         chosen_communication = ifelse(p50_6=="Director A",1,0)) %>%
  dplyr::select(respondent_id, municipality, appointed, sees_politicians_as_programmatic, conjoint_round:chosen_communication)

conjoint_schools_1b <- escola_managers %>%
  mutate(conjoint_round = 1,
         experience = `Rd_1_B_Experiencia como diretor`,
         education = Rd_1_B_Formacao,
         selection = Rd_1_B_Selecao,
         political_connections = `Rd_1_B_Vinculos politicos`,
         unit_performance = `Rd_1_B_Desempenho no IDEB`,
         relationship_to_professionals = `Rd_1_B_Relacao com os professores`,
         chosen_reform = ifelse(p50_2=="Director B",1,0),
         chosen_performance = ifelse(p50_3=="Director B",1,0),
         chosen_implementation = ifelse(p50_5=="Director B",1,0),
         chosen_communication = ifelse(p50_6=="Director B",1,0)) %>%
  dplyr::select(respondent_id, municipality, appointed, sees_politicians_as_programmatic, conjoint_round:chosen_communication)

# Collect data on profiles and choices in round 2

conjoint_schools_2a <- escola_managers %>%
  mutate(conjoint_round = 2,
         experience = `Rd_2_A_Experiencia como diretor`,
         education = Rd_2_A_Formacao, 
         selection = Rd_2_A_Selecao,
         political_connections = `Rd_2_A_Vinculos politicos`,
         unit_performance = `Rd_2_A_Desempenho no IDEB`,
         relationship_to_professionals = `Rd_2_A_Relacao com os professores`,
         chosen_reform = ifelse(p52_2=="Director A",1,0),
         chosen_performance = ifelse(p52_3=="Director A",1,0),
         chosen_implementation = ifelse(p52_5=="Director A",1,0),
         chosen_communication = ifelse(p52_6=="Director A",1,0)) %>%
  dplyr::select(respondent_id, municipality, appointed, sees_politicians_as_programmatic, conjoint_round:chosen_communication)

conjoint_schools_2b <- escola_managers %>%
  mutate(conjoint_round = 2,
         experience = `Rd_2_B_Experiencia como diretor`,
         education = Rd_2_B_Formacao,
         selection = Rd_2_B_Selecao,
         political_connections = `Rd_2_B_Vinculos politicos`,
         unit_performance = `Rd_2_B_Desempenho no IDEB`,
         relationship_to_professionals = `Rd_2_B_Relacao com os professores`,
         chosen_reform = ifelse(p52_2=="Director B",1,0),
         chosen_performance = ifelse(p52_3=="Director B",1,0),
         chosen_implementation = ifelse(p52_5=="Director B",1,0),
         chosen_communication = ifelse(p52_6=="Director B",1,0)) %>%
  dplyr::select(respondent_id, municipality, appointed, sees_politicians_as_programmatic, conjoint_round:chosen_communication)

# Collect data on profiles and choices in round 3

conjoint_schools_3a <- escola_managers %>%
  mutate(conjoint_round = 3,
         experience = `Rd_3_A_Experiencia como diretor`,
         education = Rd_3_A_Formacao, 
         selection = Rd_3_A_Selecao,
         political_connections = `Rd_3_A_Vinculos politicos`,
         unit_performance = `Rd_3_A_Desempenho no IDEB`,
         relationship_to_professionals = `Rd_3_A_Relacao com os professores`,
         chosen_reform = ifelse(p54_2=="Director A",1,0),
         chosen_performance = ifelse(p54_3=="Director A",1,0),
         chosen_implementation = ifelse(p54_5=="Director A",1,0),
         chosen_communication = ifelse(p54_6=="Director A",1,0)) %>%
  dplyr::select(respondent_id, municipality, appointed, sees_politicians_as_programmatic, conjoint_round:chosen_communication)

conjoint_schools_3b <- escola_managers %>%
  mutate(conjoint_round = 3,
         experience = `Rd_3_B_Experiencia como diretor`,
         education = Rd_3_B_Formacao,
         selection = Rd_3_B_Selecao,
         political_connections = `Rd_3_B_Vinculos politicos`,
         unit_performance = `Rd_3_B_Desempenho no IDEB`,
         relationship_to_professionals = `Rd_3_B_Relacao com os professores`,
         chosen_reform = ifelse(p54_2=="Director B",1,0),
         chosen_performance = ifelse(p54_3=="Director B",1,0),
         chosen_implementation = ifelse(p54_5=="Director B",1,0),
         chosen_communication = ifelse(p54_6=="Director B",1,0)) %>%
  dplyr::select(respondent_id, municipality, appointed, sees_politicians_as_programmatic, conjoint_round:chosen_communication)

# Collect data on profiles and choices in round 4

conjoint_schools_4a <- escola_managers %>%
  mutate(conjoint_round = 4,
         experience = `Rd_4_A_Experiencia como diretor`,
         education = Rd_4_A_Formacao, 
         selection = Rd_4_A_Selecao,
         political_connections = `Rd_4_A_Vinculos politicos`,
         unit_performance = `Rd_4_A_Desempenho no IDEB`,
         relationship_to_professionals = `Rd_4_A_Relacao com os professores`,
         chosen_reform = ifelse(p56_2=="Director A",1,0),
         chosen_performance = ifelse(p56_3=="Director A",1,0),
         chosen_implementation = ifelse(p56_5=="Director A",1,0),
         chosen_communication = ifelse(p56_6=="Director A",1,0)) %>%
  dplyr::select(respondent_id, municipality, appointed, sees_politicians_as_programmatic, conjoint_round:chosen_communication)

conjoint_schools_4b <- escola_managers %>%
  mutate(conjoint_round = 3,
         experience = `Rd_4_B_Experiencia como diretor`,
         education = Rd_4_B_Formacao,
         selection = Rd_4_B_Selecao,
         political_connections = `Rd_4_B_Vinculos politicos`,
         unit_performance = `Rd_4_B_Desempenho no IDEB`,
         relationship_to_professionals = `Rd_4_B_Relacao com os professores`,
         chosen_reform = ifelse(p56_2=="Director B",1,0),
         chosen_performance = ifelse(p56_3=="Director B",1,0),
         chosen_implementation = ifelse(p56_5=="Director B",1,0),
         chosen_communication = ifelse(p56_6=="Director B",1,0)) %>%
  dplyr::select(respondent_id, municipality, appointed, sees_politicians_as_programmatic, conjoint_round:chosen_communication)

# Merge data from all rounds
school_conjoints <- rbind(conjoint_schools_1a,
                          conjoint_schools_1b,
                          conjoint_schools_2a,
                          conjoint_schools_2b,
                          conjoint_schools_3a,
                          conjoint_schools_3b,
                          conjoint_schools_4a,
                          conjoint_schools_4b)

# Label profiles
library(plyr)
school_conjoints$experience <- revalue(school_conjoints$experience, c("3 anos"="3 years", "10 anos"="10 years"))
school_conjoints$education  <- revalue(school_conjoints$education, c("Mestrado"="Masters", "Licenciatura"="Bachelors"))
school_conjoints$selection  <- revalue(school_conjoints$selection, c("Concurso"="Civil service", "Indicação política"="Political appointment", "Eleição pela comunidade"="Election"))
school_conjoints$political_connections  <- revalue(school_conjoints$political_connections, c("Não tem vínculos políticos na prefeitura"="No connections", "Tem vínculos políticos na prefeitura"="Has connections"))
school_conjoints$unit_performance <- revalue(school_conjoints$unit_performance, c("Meta da escola não foi atingida"="Targets not met", "Meta da escola foi atingida"="Targets met"))
school_conjoints$relationship_to_professionals <- revalue(school_conjoints$relationship_to_professionals, c("Relação boa com os professores"="Good", "Relação fraca com os professores"="Bad", "Relação ruim com os professores"="Bad"))
detach(package:plyr)

# Load and clean original clinic manager conjoint dataset ------------------------------------------------------------

# Load the original dataset (not included in the replication package to protect human subjects)
d <- read_csv("../../datasets/collected/survey_bureaucrats/survey_ubs_original.csv") 

# Remove column headers as produced by Qualtrics 
d <- d[c(3:nrow(d)),] 

# Remove pilot responses that were not produced by target participants 
d <- subset(d, d$StartDate>="2018-11-26")

# Remove entries from subjects who did not agree to participate
d <- subset(d, d$p6=="YES, they wish to participate in the survey")

# Rename and recode variables in the original survey dataset that will be used in the analysis
ubs_managers <- d %>%
  mutate(respondent_id = ResponseId,
         municipality = p2_1, 
         appointed = ifelse(p16 == "Appointment", 1, 0),
         alignment_mayor_professionals = match(p44_2, answer_categories),
         mayor_concerned = match(p44_3, answer_categories),
         secretariat_holds_us_accountable = match(p44_8, answer_categories),
         secretariat_supports_us_toimprove_results = match(p44_10, answer_categories),
         sees_politicians_as_programmatic = ifelse(alignment_mayor_professionals == 4 &
                                                     mayor_concerned == 4 &
                                                     secretariat_holds_us_accountable == 4 &
                                                     secretariat_supports_us_toimprove_results == 4, 1, 0),
         sector = "healthcare")

# Conjoint data on profiles and choices in round 1

conjoint_clinics_1a <- ubs_managers %>%
  mutate(conjoint_round = 1,
         experience = `Rd_1_A_Experiencia como gerente`,
         education = Rd_1_A_Formacao, 
         selection = Rd_1_A_Selecao,
         political_connections = `Rd_1_A_Vinculos politicos`,
         unit_performance = `Rd_1_A_Desempenho nos indicadores`,
         relationship_to_professionals = `Rd_1_A_Relacao com os profissionais`,
         chosen_reform = ifelse(p50_2=="Manager A",1,0),
         chosen_performance = ifelse(p50_3=="Manager A",1,0),
         chosen_implementation = ifelse(p50_5=="Manager A",1,0),
         chosen_communication = ifelse(p50_6=="Manager A",1,0)) %>%
  dplyr::select(respondent_id, municipality, appointed, sees_politicians_as_programmatic, conjoint_round:chosen_communication)

conjoint_clinics_1b <- ubs_managers %>%
  mutate(conjoint_round = 1,
         experience = `Rd_1_B_Experiencia como gerente`,
         education = Rd_1_B_Formacao, 
         selection = Rd_1_B_Selecao,
         political_connections = `Rd_1_B_Vinculos politicos`,
         unit_performance = `Rd_1_B_Desempenho nos indicadores`,
         relationship_to_professionals = `Rd_1_B_Relacao com os profissionais`,
         chosen_reform = ifelse(p50_2=="Manager B",1,0),
         chosen_performance = ifelse(p50_3=="Manager B",1,0),
         chosen_implementation = ifelse(p50_5=="Manager B",1,0),
         chosen_communication = ifelse(p50_6=="Manager B",1,0)) %>%
  dplyr::select(respondent_id, municipality, appointed, sees_politicians_as_programmatic, conjoint_round:chosen_communication)

# Conjoint data on profiles and choices in round 2

conjoint_clinics_2a <- ubs_managers %>%
  mutate(conjoint_round = 2,
         experience = `Rd_2_A_Experiencia como gerente`,
         education = Rd_2_A_Formacao, 
         selection = Rd_2_A_Selecao,
         political_connections = `Rd_2_A_Vinculos politicos`,
         unit_performance = `Rd_2_A_Desempenho nos indicadores`,
         relationship_to_professionals = `Rd_2_A_Relacao com os profissionais`,
         chosen_reform = ifelse(p52_2=="Manager A",1,0),
         chosen_performance = ifelse(p52_3=="Manager A",1,0),
         chosen_implementation = ifelse(p52_5=="Manager A",1,0),
         chosen_communication = ifelse(p52_6=="Manager A",1,0)) %>%
  dplyr::select(respondent_id, municipality, appointed, sees_politicians_as_programmatic, conjoint_round:chosen_communication)

conjoint_clinics_2b <- ubs_managers %>%
  mutate(conjoint_round = 2,
         experience = `Rd_2_B_Experiencia como gerente`,
         education = Rd_2_B_Formacao, 
         selection = Rd_2_B_Selecao,
         political_connections = `Rd_2_B_Vinculos politicos`,
         unit_performance = `Rd_2_B_Desempenho nos indicadores`,
         relationship_to_professionals = `Rd_2_B_Relacao com os profissionais`,
         chosen_reform = ifelse(p52_2=="Manager B",1,0),
         chosen_performance = ifelse(p52_3=="Manager B",1,0),
         chosen_implementation = ifelse(p52_5=="Manager B",1,0),
         chosen_communication = ifelse(p52_6=="Manager B",1,0)) %>%
  dplyr::select(respondent_id, municipality, appointed, sees_politicians_as_programmatic, conjoint_round:chosen_communication)

# Conjoint data on profiles and choices in round 3

conjoint_clinics_3a <- ubs_managers %>%
  mutate(conjoint_round = 3,
         experience = `Rd_3_A_Experiencia como gerente`,
         education = Rd_3_A_Formacao, 
         selection = Rd_3_A_Selecao,
         political_connections = `Rd_3_A_Vinculos politicos`,
         unit_performance = `Rd_3_A_Desempenho nos indicadores`,
         relationship_to_professionals = `Rd_3_A_Relacao com os profissionais`,
         chosen_reform = ifelse(p54_2=="Manager A",1,0),
         chosen_performance = ifelse(p54_3=="Manager A",1,0),
         chosen_implementation = ifelse(p54_5=="Manager A",1,0),
         chosen_communication = ifelse(p54_6=="Manager A",1,0)) %>%
  dplyr::select(respondent_id, municipality, appointed, sees_politicians_as_programmatic, conjoint_round:chosen_communication)

conjoint_clinics_3b <- ubs_managers %>%
  mutate(conjoint_round = 3,
         experience = `Rd_3_B_Experiencia como gerente`,
         education = Rd_3_B_Formacao, 
         selection = Rd_3_B_Selecao,
         political_connections = `Rd_3_B_Vinculos politicos`,
         unit_performance = `Rd_3_B_Desempenho nos indicadores`,
         relationship_to_professionals = `Rd_3_B_Relacao com os profissionais`,
         chosen_reform = ifelse(p54_2=="Manager B",1,0),
         chosen_performance = ifelse(p54_3=="Manager B",1,0),
         chosen_implementation = ifelse(p54_5=="Manager B",1,0),
         chosen_communication = ifelse(p54_6=="Manager B",1,0)) %>%
  dplyr::select(respondent_id, municipality, appointed, sees_politicians_as_programmatic, conjoint_round:chosen_communication)

# Conjoint data on profiles and choices in round 3

conjoint_clinics_4a <- ubs_managers %>%
  mutate(conjoint_round = 4,
         experience = `Rd_4_A_Experiencia como gerente`,
         education = Rd_4_A_Formacao, 
         selection = Rd_4_A_Selecao,
         political_connections = `Rd_4_A_Vinculos politicos`,
         unit_performance = `Rd_4_A_Desempenho nos indicadores`,
         relationship_to_professionals = `Rd_4_A_Relacao com os profissionais`,
         chosen_reform = ifelse(p56_2=="Manager A",1,0),
         chosen_performance = ifelse(p56_3=="Manager A",1,0),
         chosen_implementation = ifelse(p56_5=="Manager A",1,0),
         chosen_communication = ifelse(p56_6=="Manager A",1,0)) %>%
  dplyr::select(respondent_id, municipality, appointed, sees_politicians_as_programmatic, conjoint_round:chosen_communication)

conjoint_clinics_4b <- ubs_managers %>%
  mutate(conjoint_round = 4,
         experience = `Rd_4_B_Experiencia como gerente`,
         education = Rd_4_B_Formacao, 
         selection = Rd_4_B_Selecao,
         political_connections = `Rd_4_B_Vinculos politicos`,
         unit_performance = `Rd_4_B_Desempenho nos indicadores`,
         relationship_to_professionals = `Rd_4_B_Relacao com os profissionais`,
         chosen_reform = ifelse(p56_2=="Manager B",1,0),
         chosen_performance = ifelse(p56_3=="Manager B",1,0),
         chosen_implementation = ifelse(p56_5=="Manager B",1,0),
         chosen_communication = ifelse(p56_6=="Manager B",1,0)) %>%
  dplyr::select(respondent_id, municipality, appointed, sees_politicians_as_programmatic, conjoint_round:chosen_communication)

# Merge data from all rounds
clinic_conjoints <- rbind(conjoint_clinics_1a,
                          conjoint_clinics_1b,
                          conjoint_clinics_2a,
                          conjoint_clinics_2b,
                          conjoint_clinics_3a,
                          conjoint_clinics_3b,
                          conjoint_clinics_4a,
                          conjoint_clinics_4b)

# Label profiles
library(plyr)
clinic_conjoints$experience <- revalue(clinic_conjoints$experience, c("3 anos"="3 years", "10 anos"="10 years"))
clinic_conjoints$education  <- revalue(clinic_conjoints$education, c("Mestrado"="Masters", "Graduação"="Bachelors"))
clinic_conjoints$selection  <- revalue(clinic_conjoints$selection, c("Concurso"="Civil service", "Indicação política"="Political appointment", "Eleição pela comunidade"="Election"))
clinic_conjoints$political_connections  <- revalue(clinic_conjoints$political_connections, c("Não tem vínculos políticos na prefeitura"="No connections", "Tem vínculos políticos na prefeitura"="Has connections"))
clinic_conjoints$unit_performance <- revalue(clinic_conjoints$unit_performance, c("Meta da UBS não foram atingidas"="Targets not met", "Metas da UBS foram atingidas"="Targets met"))
clinic_conjoints$relationship_to_professionals <- revalue(clinic_conjoints$relationship_to_professionals, c("Boa relação com os profissionais"="Good", "Fraca relação com os profissionais"="Bad"))
detach(package:plyr)

# Load and clean original social assistance center manager conjoint dataset ------------------------------------------------------------

# Load the original dataset (not included in the replication package to protect human subjects)
d <- read_csv("../../datasets/collected/survey_bureaucrats/survey_cras_original.csv") 

# Remove column headers as produced by Qualtrics 
d <- d[c(3:nrow(d)),] 

# Remove pilot responses that were not produced by target participants 
d <- subset(d, d$StartDate>="2018-11-26")

# Remove entries from subjects who did not agree to participate
d <- subset(d, d$p6=="YES, they wish to participate in the survey")

# Rename and recode variables in the original survey dataset that will be used in the analysis
cras_managers <- d %>%
  mutate(respondent_id = ResponseId,
         municipality = p2_1, 
         appointed = ifelse(p16 == "Appointment", 1, 0),
         alignment_mayor_professionals = match(p44_2, answer_categories),
         mayor_concerned = match(p44_3, answer_categories),
         secretariat_holds_us_accountable = match(p44_8, answer_categories),
         secretariat_supports_us_toimprove_results = match(p44_10, answer_categories),
         sees_politicians_as_programmatic = ifelse(alignment_mayor_professionals == 4 &
                                                     mayor_concerned == 4 &
                                                     secretariat_holds_us_accountable == 4 &
                                                     secretariat_supports_us_toimprove_results == 4, 1, 0),
         sector = "social assistance")

# Conjoint data on profiles and choices in round 1

conjoint_cras_1a <- cras_managers %>%
  mutate(conjoint_round = 1,
         experience = `Rd_1_A_Experiencia como coordenador`,
         education = Rd_1_A_Formacao, 
         selection = Rd_1_A_Selecao,
         political_connections = `Rd_1_A_Vinculos politicos`,
         unit_performance = `Rd_1_A_Desempenho nos indicadores`,
         relationship_to_professionals = `Rd_1_A_Relacao com os profissionais`,
         chosen_reform = ifelse(p50_2=="Coordinator A",1,0),
         chosen_performance = ifelse(p50_3=="Coordinator A",1,0),
         chosen_implementation = ifelse(p50_5=="Coordinator A",1,0),
         chosen_communication = ifelse(p50_6=="Coordinator A",1,0)) %>%
  dplyr::select(respondent_id, municipality, appointed, sees_politicians_as_programmatic, conjoint_round:chosen_communication)

conjoint_cras_1b <- cras_managers %>%
  mutate(conjoint_round = 1,
         experience = `Rd_1_B_Experiencia como coordenador`,
         education = Rd_1_B_Formacao, 
         selection = Rd_1_B_Selecao,
         political_connections = `Rd_1_B_Vinculos politicos`,
         unit_performance = `Rd_1_B_Desempenho nos indicadores`,
         relationship_to_professionals = `Rd_1_B_Relacao com os profissionais`,
         chosen_reform = ifelse(p50_2=="Coordinator B",1,0),
         chosen_performance = ifelse(p50_3=="Coordinator B",1,0),
         chosen_implementation = ifelse(p50_5=="Coordinator B",1,0),
         chosen_communication = ifelse(p50_6=="Coordinator B",1,0)) %>%
  dplyr::select(respondent_id, municipality, appointed, sees_politicians_as_programmatic, conjoint_round:chosen_communication)

# Conjoint data on profiles and choices in round 2

conjoint_cras_2a <- cras_managers %>%
  mutate(conjoint_round = 2,
         experience = `Rd_2_A_Experiencia como coordenador`,
         education = Rd_2_A_Formacao, 
         selection = Rd_2_A_Selecao,
         political_connections = `Rd_2_A_Vinculos politicos`,
         unit_performance = `Rd_2_A_Desempenho nos indicadores`,
         relationship_to_professionals = `Rd_2_A_Relacao com os profissionais`,
         chosen_reform = ifelse(p52_2=="Coordinator A",1,0),
         chosen_performance = ifelse(p52_3=="Coordinator A",1,0),
         chosen_implementation = ifelse(p52_5=="Coordinator A",1,0),
         chosen_communication = ifelse(p52_6=="Coordinator A",1,0)) %>%
  dplyr::select(respondent_id, municipality, appointed, sees_politicians_as_programmatic, conjoint_round:chosen_communication)

conjoint_cras_2b <- cras_managers %>%
  mutate(conjoint_round = 2,
         experience = `Rd_2_B_Experiencia como coordenador`,
         education = Rd_2_B_Formacao, 
         selection = Rd_2_B_Selecao,
         political_connections = `Rd_2_B_Vinculos politicos`,
         unit_performance = `Rd_2_B_Desempenho nos indicadores`,
         relationship_to_professionals = `Rd_2_B_Relacao com os profissionais`,
         chosen_reform = ifelse(p52_2=="Coordinator B",1,0),
         chosen_performance = ifelse(p52_3=="Coordinator B",1,0),
         chosen_implementation = ifelse(p52_5=="Coordinator B",1,0),
         chosen_communication = ifelse(p52_6=="Coordinator B",1,0)) %>%
  dplyr::select(respondent_id, municipality, appointed, sees_politicians_as_programmatic, conjoint_round:chosen_communication)

# Conjoint data on profiles and choices in round 3

conjoint_cras_3a <- cras_managers %>%
  mutate(conjoint_round = 3,
         experience = `Rd_3_A_Experiencia como coordenador`,
         education = Rd_3_A_Formacao, 
         selection = Rd_3_A_Selecao,
         political_connections = `Rd_3_A_Vinculos politicos`,
         unit_performance = `Rd_3_A_Desempenho nos indicadores`,
         relationship_to_professionals = `Rd_3_A_Relacao com os profissionais`,
         chosen_reform = ifelse(p54_2=="Coordinator A",1,0),
         chosen_performance = ifelse(p54_3=="Coordinator A",1,0),
         chosen_implementation = ifelse(p54_5=="Coordinator A",1,0),
         chosen_communication = ifelse(p54_6=="Coordinator A",1,0)) %>%
  dplyr::select(respondent_id, municipality, appointed, sees_politicians_as_programmatic, conjoint_round:chosen_communication)

conjoint_cras_3b <- cras_managers %>%
  mutate(conjoint_round = 3,
         experience = `Rd_3_B_Experiencia como coordenador`,
         education = Rd_3_B_Formacao, 
         selection = Rd_3_B_Selecao,
         political_connections = `Rd_3_B_Vinculos politicos`,
         unit_performance = `Rd_3_B_Desempenho nos indicadores`,
         relationship_to_professionals = `Rd_3_B_Relacao com os profissionais`,
         chosen_reform = ifelse(p54_2=="Coordinator B",1,0),
         chosen_performance = ifelse(p54_3=="Coordinator B",1,0),
         chosen_implementation = ifelse(p54_5=="Coordinator B",1,0),
         chosen_communication = ifelse(p54_6=="Coordinator B",1,0)) %>%
  dplyr::select(respondent_id, municipality, appointed, sees_politicians_as_programmatic, conjoint_round:chosen_communication)

# Conjoint data on profiles and choices in round 4

conjoint_cras_4a <- cras_managers %>%
  mutate(conjoint_round = 4,
         experience = `Rd_4_A_Experiencia como coordenador`,
         education = Rd_4_A_Formacao, 
         selection = Rd_4_A_Selecao,
         political_connections = `Rd_4_A_Vinculos politicos`,
         unit_performance = `Rd_4_A_Desempenho nos indicadores`,
         relationship_to_professionals = `Rd_4_A_Relacao com os profissionais`,
         chosen_reform = ifelse(p56_2=="Coordinator A",1,0),
         chosen_performance = ifelse(p56_3=="Coordinator A",1,0),
         chosen_implementation = ifelse(p56_5=="Coordinator A",1,0),
         chosen_communication = ifelse(p56_6=="Coordinator A",1,0)) %>%
  dplyr::select(respondent_id, municipality, appointed, sees_politicians_as_programmatic, conjoint_round:chosen_communication)

conjoint_cras_4b <- cras_managers %>%
  mutate(conjoint_round = 4,
         experience = `Rd_4_B_Experiencia como coordenador`,
         education = Rd_4_B_Formacao, 
         selection = Rd_4_B_Selecao,
         political_connections = `Rd_4_B_Vinculos politicos`,
         unit_performance = `Rd_4_B_Desempenho nos indicadores`,
         relationship_to_professionals = `Rd_4_B_Relacao com os profissionais`,
         chosen_reform = ifelse(p56_2=="Coordinator B",1,0),
         chosen_performance = ifelse(p56_3=="Coordinator B",1,0),
         chosen_implementation = ifelse(p56_5=="Coordinator B",1,0),
         chosen_communication = ifelse(p56_6=="Coordinator B",1,0)) %>%
  dplyr::select(respondent_id, municipality, appointed, sees_politicians_as_programmatic, conjoint_round:chosen_communication)

# Merge data from all rounds
cras_conjoints <- rbind(conjoint_cras_1a,
                          conjoint_cras_1b,
                          conjoint_cras_2a,
                          conjoint_cras_2b,
                          conjoint_cras_3a,
                          conjoint_cras_3b,
                          conjoint_cras_4a,
                          conjoint_cras_4b)

# Label profiles
library(plyr)
cras_conjoints$experience <- revalue(cras_conjoints$experience, c("3 anos"="3 years", "10 anos"="10 years"))
cras_conjoints$education  <- revalue(cras_conjoints$education, c("Mestrado"="Masters", "Graduação"="Bachelors"))
cras_conjoints$selection  <- revalue(cras_conjoints$selection, c("Concurso"="Civil service", "Indicação política"="Political appointment", "Eleição pela comunidade"="Election"))
cras_conjoints$political_connections  <- revalue(cras_conjoints$political_connections, c("Não tem vínculos políticos na prefeitura"="No connections", "Tem vínculos políticos na prefeitura"="Has connections"))
cras_conjoints$unit_performance <- revalue(cras_conjoints$unit_performance, c("Metas do CRAS não foram atingidas"="Targets not met", "Metas do CRAS foram atingidas"="Targets met"))
cras_conjoints$relationship_to_professionals <- revalue(cras_conjoints$relationship_to_professionals, c("Boa relação com os profissionais"="Good", "Fraca relação com os profissionais"="Bad"))
detach(package:plyr)

# Generate indicator for whether all respondents in the municipality are appointed --------------------------------------------------------

bureaucrats_conjoint <- rbind(school_conjoints, clinic_conjoints, cras_conjoints)

# Generate municipality-level shares of respondents who are politically appointed
bm <- bureaucrats_conjoint %>%
  group_by(municipality) %>%
  summarise(share_appointed = mean(appointed))
municipalities_all_appointed <- bm[which(bm$share_appointed==1),"municipality"][[1]]

# Create indicator 
bureaucrats_conjoint$municipality_all_appointed <- ifelse(bureaucrats_conjoint$municipality %in% municipalities_all_appointed, 1, 0)

# Remove municipality identifier, to protect human subjects
bureaucrats_conjoint <- bureaucrats_conjoint %>%
  dplyr::select(-municipality)

# Remove conjoint round id, which will not be used in analyses
bureaucrats_conjoint <- bureaucrats_conjoint %>%
  dplyr::select(-conjoint_round)

# Export bureaucrat conjoint dataset and generate its codebook ------------------------------------------------------------------

# Export dataset 

write_csv(bureaucrats_conjoint, "../../datasets/analysis/survey_bureaucrats/conjoint_bureaucrats_analysis.csv")

# Generate codebook

conjoint_bureaucrats_analysis <- read_csv("../../datasets/analysis/survey_bureaucrats/conjoint_bureaucrats_analysis.csv")

var_label(conjoint_bureaucrats_analysis) <- list(
  respondent_id = "Respondent ID (generated by Qualtrics)",
  appointed = "Indicator for whether the respondent is politically appointed (1 = Yes, 0 = No)",
  sees_politicians_as_programmatic = "Indicator for whether the respondent sees politicians as programmatic (i.e., they expressed the highest possible level of agreement with the statements 'The mayor and professionals in [respondent's sector] have the same priorities with regards to [schools / clinics / centers]', 'The mayor is concerned with improving the quality of [schools / clinics / centers]', 'The secretariat holds us accountable for the results of this [school / clinic / center]', 'The secretariat helps us improve the performance of this [school / clinic / center]' (1 = Yes, 0 = No, NA = missing)" ,
  experience = "Hypothetical manager's years of experience (3 years / 10 years)",
  education = "Hypothetical manager's education level (Bachelors / Masters)",
  selection = "Hypothetical manager's selection mode (Civil service / Election / Political appointment)",
  political_connections = "Hypothetical manager's political connections (Has connections / No connections)",
  unit_performance = "Performance of the unit [school / clinic / social assistance center] where the hypothetical manager works (Targets met / Targets not met)",
  relationship_to_professionals = "Hypothetical manager's relationship to professionals in their unit [school / clinic / social assistance center] (Good / Bad)",
  chosen_reform = "Whether this hypothetical manager is chosen for the question 'Which of these [directors/managers / coordinators] do you think would obtain a reform for the [school / clinic / social assistance center]?' (1 = Yes, 0 = No, NA = missing)",
  chosen_performance = "Whether this hypothetical manager is chosen for the question 'Which of these [directors/managers/coordinators] do you think would achieve better scores in [student learning/community healthcare/social assistance center indicators]?' (1 = Yes, 0 = No, NA = missing)",
  chosen_implementation = "Whether this hypothetical manager is chosen for the question 'Which of these [directors / managers / coordinators] do you think would have more chances of implementing changes requested by the mayor’s office?' (1 = Yes, 0 = No, NA = missing)",
  chosen_communication = "Whether this hypothetical manager is chosen for the question 'Which of these [directors / managers / coordinators] do you think would have a better communication with the Secretariat of [education / healthcare / social assistance]?' (1 = Yes, 0 = No, NA = missing)",
  municipality_all_appointed = "Indicator for whether all respondents in the municipality are politically appointed (1 = Yes, 0 = No)")

codebook::label_browser_static(conjoint_bureaucrats_analysis)

# NOTES -- R version, platform, and loaded packages -------------------------

### sessionInfo(package = NULL)

# R version 3.6.3 (2020-02-29)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS Catalina 10.15.7
# 
# Matrix products: default
# BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
# LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
#   [1] grid      stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] lubridate_1.7.4 plyr_1.8.6      cjoint_2.1.0    survey_4.0      Hmisc_4.4-0    
# [6] lattice_0.20-40 texreg_1.37.1   xtable_1.8-4    ebal_0.1-6      Matching_4.9-7 
# [11] lfe_2.8-5       Matrix_1.2-18   multcomp_1.4-13 TH.data_1.0-10  MASS_7.3-51.5  
# [16] mvtnorm_1.1-0   rdd_0.57        Formula_1.2-3   AER_1.2-9       survival_3.1-11
# [21] car_3.0-7       carData_3.0-3   lmtest_0.9-37   zoo_1.8-7       sandwich_3.0-1 
# [26] rdrobust_0.99.7 forcats_0.5.0   stringr_1.4.0   dplyr_1.0.0     purrr_0.3.3    
# [31] readr_1.3.1     tidyr_1.0.2     tibble_3.0.0    ggplot2_3.3.0   tidyverse_1.3.0
# 
# loaded via a namespace (and not attached):
#   [1] colorspace_1.4-1    ellipsis_0.3.0      rio_0.5.16          rsconnect_0.8.16   
# [5] htmlTable_1.13.3    base64enc_0.1-3     fs_1.4.1            rstudioapi_0.11    
# [9] farver_2.0.3        fansi_0.4.1         xml2_1.3.0          codetools_0.2-16   
# [13] splines_3.6.3       knitr_1.28          jsonlite_1.6.1      broom_0.5.5        
# [17] cluster_2.1.0       dbplyr_1.4.2        png_0.1-7           shiny_1.4.0.2      
# [21] compiler_3.6.3      httr_1.4.1          backports_1.1.5     assertthat_0.2.1   
# [25] fastmap_1.0.1       cli_2.0.2           later_1.0.0         acepack_1.4.1      
# [29] htmltools_0.4.0     tools_3.6.3         gtable_0.3.0        glue_1.3.2         
# [33] Rcpp_1.0.4          cellranger_1.1.0    vctrs_0.3.1         nlme_3.1-145       
# [37] xfun_0.12           openxlsx_4.1.4      rvest_0.3.5         mime_0.9           
# [41] lifecycle_0.2.0     scales_1.1.0        hms_0.5.3           promises_1.1.0     
# [45] parallel_3.6.3      RColorBrewer_1.1-2  yaml_2.2.1          curl_4.3           
# [49] gridExtra_2.3       rpart_4.1-15        latticeExtra_0.6-29 stringi_1.5.3      
# [53] checkmate_2.0.0     zip_2.0.4           rlang_0.4.6         pkgconfig_2.0.3    
# [57] htmlwidgets_1.5.1   labeling_0.3        tidyselect_1.1.0    magrittr_1.5       
# [61] R6_2.4.1            generics_0.0.2      DBI_1.1.0           pillar_1.4.3       
# [65] haven_2.3.1         foreign_0.8-76      withr_2.1.2         abind_1.4-5        
# [69] nnet_7.3-13         modelr_0.1.6        crayon_1.3.4        utf8_1.1.4         
# [73] jpeg_0.1-8.1        readxl_1.3.1        data.table_1.12.8   reprex_0.3.0       
# [77] digest_0.6.25       httpuv_1.5.2        munsell_0.5.0       mitools_2.4 