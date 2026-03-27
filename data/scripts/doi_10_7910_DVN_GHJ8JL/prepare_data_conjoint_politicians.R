### Replication code
### Article: "How patronage delivers: Political appointments, bureaucratic accountability, and service delivery in Brazil"
### Author: Guillermo Toral (www.guillermotoral.com)
### Date: June 2022
### This file prepares the politician conjoint experiment data
### R version, platform, and package versions reported at the end of the file

### Note that this file uses the original survey data, which are not included in the replication package to protect human subjects
### The author will not share the original survey files to protect subjects (which are easily identifiable). However requests for descriptives can be sent to the author (contact info at www.guillermotoral.com)

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

# Clean original survey dataset ------------------------------------------------------------

# Load the original dataset (not included in the replication package to protect human subjects)
d <- read_csv("../../datasets/collected/survey_politicians/survey_politicians_original.csv") 

# Remove column headers as produced by Qualtricds 
d <- d[c(3:nrow(d)),] 

# Remove pilot responses that were not produced by target participants 
d <- subset(d, substr(d$StartDate,1,10)>"2019-02-10")

# Clean conjoint data ------------------------------------------------------------

# Create variables identifying respondent type and the municipality where they work
d <- d %>%
  mutate(mayor = ifelse(p2=="Mayor",1,0), 
         secretary = ifelse(p2=="Secretary",1,0),
         secretariat_areas = p4, 
         secretary_education = ifelse(grepl("Education", secretariat_areas),1,0),
         secretary_healthcare = ifelse(grepl("Healthcare", secretariat_areas),1,0),
         secretary_socialassistance = ifelse(grepl("Social assistance", secretariat_areas),1,0),
         secretary_finance = ifelse(grepl("Finance", secretariat_areas),1,0),
         secretary_administration = ifelse(grepl("Administration", secretariat_areas),1,0),
         municipality = p6)

# Remove respondents who are not part of the target population
d <- d %>% 
  filter(mayor==1 | secretary_education==1 | secretary_healthcare==1 | secretary_socialassistance==1 | secretary_finance==1 | secretary_administration==1)

# Remove respondents who did not complete the survey
d <- d %>%
  dplyr::filter(Finished=="True")

# Round 1
conjoint_politicians_1a <- d %>%
  mutate(respondent_id = ResponseId,
         conjoint_round = 1,
         experience = `Rd_1_A_Experiencia como funcionario`,
         education = Rd_1_A_Formacao,
         selection = `Rd_1_A_Tipo de contrato`,
         political_connections = `Rd_1_A_Vinculos politicos`,
         union_membership = `Rd_1_A_Atividade sindical`,
         gender = `Rd_1_A_Sexo`,
         chosen_effort = ifelse(p39_1=="Worker A",1,0),
         chosen_performance = ifelse(p39_2=="Worker A",1,0),
         chosen_implementation = ifelse(p39_4=="Worker A",1,0),
         chosen_communication = ifelse(p39_5=="Worker A",1,0)) %>%
  dplyr::select(respondent_id:chosen_communication)
conjoint_politicians_1b <- d %>%
  mutate(respondent_id = ResponseId,
         conjoint_round = 1,
         experience = `Rd_1_B_Experiencia como funcionario`,
         education = Rd_1_B_Formacao,
         selection = `Rd_1_B_Tipo de contrato`,
         political_connections = `Rd_1_B_Vinculos politicos`,
         union_membership = `Rd_1_B_Atividade sindical`,
         gender = `Rd_1_B_Sexo`,
         chosen_effort = ifelse(p39_1=="Worker B",1,0),
         chosen_performance = ifelse(p39_2=="Worker B",1,0),
         chosen_implementation = ifelse(p39_4=="Worker B",1,0),
         chosen_communication = ifelse(p39_5=="Worker B",1,0)) %>%
  dplyr::select(respondent_id:chosen_communication)

# Round 2
conjoint_politicians_2a <- d %>%
  mutate(respondent_id = ResponseId,
         conjoint_round = 2,
         experience = `Rd_2_A_Experiencia como funcionario`,
         education = Rd_2_A_Formacao,
         selection = `Rd_2_A_Tipo de contrato`,
         political_connections = `Rd_2_A_Vinculos politicos`,
         union_membership = `Rd_2_A_Atividade sindical`,
         gender = `Rd_2_A_Sexo`,
         chosen_effort = ifelse(p41_1=="Worker A",1,0),
         chosen_performance = ifelse(p41_2=="Worker A",1,0),
         chosen_implementation = ifelse(p41_4=="Worker A",1,0),
         chosen_communication = ifelse(p41_5=="Worker A",1,0)) %>%
  dplyr::select(respondent_id:chosen_communication)
conjoint_politicians_2b <- d %>%
  mutate(respondent_id = ResponseId,
         conjoint_round = 2,
         experience = `Rd_2_B_Experiencia como funcionario`,
         education = Rd_2_B_Formacao,
         selection = `Rd_2_B_Tipo de contrato`,
         political_connections = `Rd_2_B_Vinculos politicos`,
         union_membership = `Rd_2_B_Atividade sindical`,
         gender = `Rd_2_B_Sexo`,
         chosen_effort = ifelse(p41_1=="Worker B",1,0),
         chosen_performance = ifelse(p41_2=="Worker B",1,0),
         chosen_implementation = ifelse(p41_4=="Worker B",1,0),
         chosen_communication = ifelse(p41_5=="Worker B",1,0),) %>%
  dplyr::select(respondent_id:chosen_communication)

# Round 3
conjoint_politicians_3a <- d %>%
  mutate(respondent_id = ResponseId,
         conjoint_round = 3,
         experience = `Rd_3_A_Experiencia como funcionario`,
         education = Rd_3_A_Formacao,
         selection = `Rd_3_A_Tipo de contrato`,
         political_connections = `Rd_3_A_Vinculos politicos`,
         union_membership = `Rd_3_A_Atividade sindical`,
         gender = `Rd_3_A_Sexo`,
         chosen_effort = ifelse(p43_1=="Worker A",1,0),
         chosen_performance = ifelse(p43_2=="Worker A",1,0),
         chosen_implementation = ifelse(p43_4=="Worker A",1,0),
         chosen_communication = ifelse(p43_5=="Worker A",1,0)) %>%
  dplyr::select(respondent_id:chosen_communication)
conjoint_politicians_3b <- d %>%
  mutate(respondent_id = ResponseId,
         conjoint_round = 3,
         experience = `Rd_3_B_Experiencia como funcionario`,
         education = Rd_3_B_Formacao,
         selection = `Rd_3_B_Tipo de contrato`,
         political_connections = `Rd_3_B_Vinculos politicos`,
         union_membership = `Rd_3_B_Atividade sindical`,
         gender = `Rd_3_B_Sexo`,
         chosen_effort = ifelse(p43_1=="Worker B",1,0),
         chosen_performance = ifelse(p43_2=="Worker B",1,0),
         chosen_implementation = ifelse(p43_4=="Worker B",1,0),
         chosen_communication = ifelse(p43_5=="Worker B",1,0)) %>%
  dplyr::select(respondent_id:chosen_communication)

# Round 4
conjoint_politicians_4a <- d %>%
  mutate(respondent_id = ResponseId,
         conjoint_round = 4,
         experience = `Rd_4_A_Experiencia como funcionario`,
         education = Rd_4_A_Formacao,
         selection = `Rd_4_A_Tipo de contrato`,
         political_connections = `Rd_4_A_Vinculos politicos`,
         union_membership = `Rd_4_A_Atividade sindical`,
         gender = `Rd_4_A_Sexo`,
         chosen_effort = ifelse(p45_1=="Worker A",1,0),
         chosen_performance = ifelse(p45_2=="Worker A",1,0),
         chosen_implementation = ifelse(p45_4=="Worker A",1,0),
         chosen_communication = ifelse(p45_5=="Worker A",1,0)) %>%
  dplyr::select(respondent_id:chosen_communication)
conjoint_politicians_4b <- d %>%
  mutate(respondent_id = ResponseId,
         conjoint_round = 4,
         experience = `Rd_4_B_Experiencia como funcionario`,
         education = `Rd_4_B_Formacao`,
         selection = `Rd_4_B_Tipo de contrato`,
         political_connections = `Rd_4_B_Vinculos politicos`,
         union_membership = `Rd_4_B_Atividade sindical`,
         gender = `Rd_4_B_Sexo`,
         chosen_effort = ifelse(p45_1=="Worker B",1,0),
         chosen_performance = ifelse(p45_2=="Worker B",1,0),
         chosen_implementation = ifelse(p45_4=="Worker B",1,0),
         chosen_communication = ifelse(p45_5=="Worker B",1,0)) %>%
  dplyr::select(respondent_id:chosen_communication)

# Export politician conjoint experiment dataset and generate its codebook ----------------------------------------------------------

# Bind together data for all rounds
conjoint_politicians <- rbind(conjoint_politicians_1a, 
                              conjoint_politicians_1b, 
                              conjoint_politicians_2a,
                              conjoint_politicians_2b,
                              conjoint_politicians_3a,
                              conjoint_politicians_3b,
                              conjoint_politicians_4a,
                              conjoint_politicians_4b)

# Define labels in English

library(plyr)
conjoint_politicians$experience <- revalue(conjoint_politicians$experience, c("3 anos"="3 years", "10 anos"="10 years"))
conjoint_politicians$education  <- revalue(conjoint_politicians$education, c("Pós-graduação"="Graduate degree", "Graduação"="Undergraduate degree"))
conjoint_politicians$selection  <- revalue(conjoint_politicians$selection, c("Contratação temporária"="Temporary contract", "Concurso público"="Civil service"))
conjoint_politicians$political_connections  <- revalue(conjoint_politicians$political_connections, c("Não tem vínculos políticos na prefeitura"="No connections with city hall", "Tem vínculos políticos na prefeitura"="Has connections with city hall"))
conjoint_politicians$union_membership <- revalue(conjoint_politicians$union_membership, c("Participa de um sindicato"="Participates in a union", "Não participa de nenhum sindicato"="Does not participate in a union"))
conjoint_politicians$gender <- revalue(conjoint_politicians$gender, c("Homem"="Man", "Mulher"="Woman"))
detach(package:plyr)

# Remove conjoint round identifier, which is not used in the analysis

conjoint_politicians <- conjoint_politicians %>%
  dplyr::select(-conjoint_round)

write_csv(conjoint_politicians, "../../datasets/analysis/survey_politicians/conjoint_politicians_analysis.csv")

# Generate codebook

conjoint_politicians_analysis <- read_csv("../../datasets/analysis/survey_politicians/conjoint_politicians_analysis.csv")

var_label(conjoint_politicians_analysis) <- list(
  respondent_id = "Respondent ID (generated by Qualtrics)",
  experience = "Hypothetical bureaucrat's years of experience (3 years / 10 years)",
  education = "Hypothetical bureaucrat's education level (Graduate degree / Undergraduate degree)",
  selection = "Hypothetical bureaucrat's selection mode (Civil service / Temporary contract)",
  political_connections = "Hypothetical bureaucrat's political connections (Has connections with city hall / No connections with city hall)",
  union_membership = "Hypothetical bureaucrat's union membership (Does not participate in a union / Participates in a union)",
  gender = "Hypothetical bureaucrat's gender (Man / Woman)",
  chosen_effort = "Whether this hypothetical bureaucrat is chosen for the question 'Which of these bureaucrats do you think would work extra hours if necessary?' (1 = Yes, 0 = No)",
  chosen_performance = "Whether this hypothetical bureaucrat is chosen for the question 'Which of these bureaucrats do you think would achieve better performance?' (1 = Yes, 0 = No)",
  chosen_implementation = "Whether this hypothetical bureaucrat is chosen for the question 'Which of these bureaucrats do you think would have more chances of implementing changes requested by the local government?' (1 = Yes, 0 = No)",
  chosen_communication = "Whether this hypothetical bureaucrat is chosen for the question 'Which of these bureaucrats do you think would have a better communication with the local government?' (1 = Yes, 0 = No)")

codebook::label_browser_static(conjoint_politicians_analysis)

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