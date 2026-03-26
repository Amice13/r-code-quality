### Replication code
### Article: "How patronage delivers: Political appointments, bureaucratic accountability, and service delivery in Brazil"
### Author: Guillermo Toral (www.guillermotoral.com)
### Date: June 2022
### This file prepares the politician survey data
### R version, platform, and package versions reported at the end of the file

### Note that this file uses the original survey data, which are not included in the replication package to protect human subjects
### The author will not share the original survey files to protect subjects (which are easily identifiable). However requests for descriptive statistics can be sent to the author (contact info at www.guillermotoral.com)

# Prepare the environment -------------------------------------------------

### This section of the code prepares the environment 

# Clean the environment
rm(list = ls())

# Install required packages if not previously installed
package_list <- c("tidyverse", "here", "readxl", "codebook") 
packages_to_install <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(packages_to_install)>0){
  install.packages(packages_to_install)
}

# Load required packages
library(tidyverse)
library(here)
library(readxl)
library(codebook)

# Set Working Directory to wherever this file is located.
setwd(here())
# The directory where the code folder is located must also have a "plots" and a "tables" subdirectory

# Clean original survey dataset ------------------------------------------------------------

# Load the original dataset (not included in the replication package to protect human subjects)
d <- read_csv("../../datasets/collected/survey_politicians/survey_politicians_original.csv") 

# Remove column headers as produced by Qualtrics 
d <- d[c(3:nrow(d)),] 

# Remove pilot responses that were not produced by target participants 
d <- subset(d, substr(d$StartDate,1,10)>"2019-02-10")

# Create vectors that are used later to clean the data
answer_categories <- c("Not at all", "A little", "Quite", "A lot")
ranking_reversed <- as.character(c(7:1))

# Rename and recode variables in the original survey dataset that will be used in the analysis
d <- d %>%
  mutate(mayor = ifelse(p2=="Mayor",1,0), 
         secretary = ifelse(p2=="Secretary",1,0),
         secretariat_areas = p4, 
         secretary_education = ifelse(grepl("Education", secretariat_areas),1,0),
         secretary_healthcare = ifelse(grepl("Healthcare", secretariat_areas),1,0),
         secretary_socialassistance = ifelse(grepl("Social assistance", secretariat_areas),1,0),
         secretary_finance = ifelse(grepl("Finance", secretariat_areas),1,0),
         secretary_administration = ifelse(grepl("Administration", secretariat_areas),1,0),
         municipality = p6, 
         politician_experience = as.numeric(p7),
         bureaucrat_experience = ifelse(p8=="No",0,1), 
         party_member = ifelse(p10=="Yes",1,0),
         female = ifelse(p12 == "Woman",1,0), 
         age = as.numeric(p14), 
         education_highschool_orless = ifelse(p13 == "Complete basic education" | p13=="Complete high school" | p13 =="Incomplete basic education" | p13 == "Incomplete high school",1,0),
         education_college_ormore = ifelse(p13=="Complete bachelors degree" | p13=="Doutorado" | p13=="Mestrado" | p13=="Masters / PhD / graduate degree",1,0),
         ranking_resp_improvement_mayor = as.numeric(p23_1), 
         meetings_managers_in_area = ifelse(secretary_education==1, as.numeric(p29_1_8),
                                          ifelse(secretary_healthcare==1, as.numeric(p29_2_8),
                                             ifelse(secretary_socialassistance==1, as.numeric(p29_3_8), NA))))

# Remove respondents who are not part of the target population
d <- d %>% 
  filter(mayor==1 | secretary_education==1 | secretary_healthcare==1 | secretary_socialassistance==1 | secretary_finance==1 | secretary_administration==1)

# Remove respondents who did not complete the survey
d <- d %>%
  dplyr::filter(Finished=="True")

# Export politician-level dataset and generate its codebook ----------------------------------------------------------

### This section selects the relevant variables, exports the analysis dataset, and generates a codebook

p <- d %>%
  dplyr::select(mayor,
         secretary,
         politician_experience,
         bureaucrat_experience,
         party_member,
         female,
         age,
         education_highschool_orless,
         education_college_ormore,
         ranking_resp_improvement_mayor,
         meetings_managers_in_area)

write_csv(p, "../../datasets/analysis/survey_politicians/survey_politicians_analysis.csv")

### Generate codebook with variable labels and values

survey_politicians_analysis <- read_csv("../../datasets/analysis/survey_politicians/survey_politicians_analysis.csv")

var_label(survey_politicians_analysis) <- list(
  mayor = "Indicator for whether the respondent is a mayor (1 = Yes, 0 = No)",
  secretary = "Indicator for whether the respondent is a secretary (1 = Yes, 0 = No)",
  politician_experience = "Experience as a politician (Number of years)",
  bureaucrat_experience = "Experience as a bureaucrat (Number of years)",
  party_member = "Indicator for whether the respondent is member of a political party (1 = Yes, 0 = No)",
  female = "Indicator for whether the respondent is female (1 = Yes, 0 = No)",
  age = "Respondent's age (Number of years)",
  education_highschool_orless = "Indicator for whether the respondent has a high school diploma or less (1 = Yes, 0 = No)",
  education_college_ormore = "Indicator for whether the respondent has a college degree or more (1 = Yes, 0 = No)",
  ranking_resp_improvement_mayor = "Ranking the respondent gives to the mayor when it comes to the responsibility to improve public services, from a list of 7 local stakeholders (1 = Mayor ranked first, 2 = Mayor ranked second, 3 = Mayor ranked third, 4 = Mayor ranked fourth, 5 = Mayor ranked fifth, 6 = Mayor ranked sixth, 7 = Mayor ranked seventh and last)",
  meetings_managers_in_area = "Meetings secretaries of education / healthcare / social assistance report with street-level managers in their area in the previous 3 months (Number of meetings; NA = not applicable or missing)")

codebook::label_browser_static(survey_politicians_analysis)

# Count municipalities and respondents per municipality; build dataset with respondents per municipality ---------------------

### This section creates and exports a municipality-level dataset with the count of respondents per municipality
### The analysis dataset excludes the municipality identifier to protect human subjects

# Count municipalities where there are respondents
n_distinct(d$municipality) # 142 municipalities

# Count respondents by municipality
dm <- d %>%
  group_by(municipality) %>%
  dplyr::summarise(respondents=n())
summary(dm$respondents) # Median 3 respondents per municipality

# Import dataset with municipality identifiers
muns <- read_excel("../../datasets/downloaded/ibge/ibge_dtb_municipio.xls")

# Select municipality identifier codes for municipalities in Rio Grande do Norte
muns_rn <- muns %>%
  mutate(municipality = Nome_Município, 
         cod_ibge = as.numeric(substr(`Código Município Completo`,1,6))) %>%
  filter(substr(cod_ibge,1,2)=="24") %>% # First 2 digits of the IBGE code identify the state, and 24 is the code for Rio Grande do Norte
  dplyr::select(municipality, cod_ibge)
dm[which(dm$municipality=="Augusto Severo"),"municipality"] <- "Campo Grande" # This municipality changed name

dm <- left_join(dm, muns_rn)

### Add municipality-level covariates

# Merge an indicator for whether the mayor was re-elected in 2016
# Load dataset on municipal elections, which is built with the code in "prepare_data_diffindisc.R"
m <- read_csv("../../datasets/analysis/other/municipal_elections_2016.csv") %>%
  dplyr::filter(substr(cod_ibge,1,2)=="24") %>% # Select only municipalities in Rio Grande do Norte
  mutate(mayor_reelected = ifelse(mayor_lost==0,1,0),
         mayor_reelected = ifelse(is.na(mayor_reelected),0,mayor_reelected)) %>% # Create indicator for re-elected mayor
  dplyr::select(cod_ibge, mayor_reelected)
dm <- left_join(m, dm) %>%
  mutate(respondents = ifelse(is.na(respondents),0,respondents)) # Assign to 0 if there are no respondents

# Merge other municipal-level covariates
covariates <- read_csv("../../datasets/downloaded/basedosdados/basedosdados_municipality_covariates.csv") %>%
  dplyr::filter(year==2016) %>%
  mutate(cod_ibge = as.numeric(substr(ibge,1,6)))
dm <- left_join(dm, covariates)

# Transform variables
dm <- dm %>%
  mutate(log_population = log(population),
         log_gdppercapita = log(gdppercapita),
         no_respondents = ifelse(respondents==0,1,0)) %>%
  dplyr::select(cod_ibge, respondents, no_respondents, log_population, log_gdppercapita, deaths_perthousand, mayor_reelected)

# Export municipality-level dataset for nonresponse analysis, and generate its codebook  ---------

write_csv(dm, "../../datasets/analysis/survey_politicians/survey_politicians_nonresponse_analysis.csv")

### Generate codebook with variable labels and values

survey_politicians_nonresponse_analysis <- read_csv("../../datasets/analysis/survey_politicians/survey_politicians_nonresponse_analysis.csv")

var_label(survey_politicians_nonresponse_analysis) <- list(
  cod_ibge = "Municipality code (IBGE identifier)",
  respondents = "Number of survey respondents",
  no_respondents = "Indicator for whether there are no respondents from this municipality (1 = Yes, 0 = No)",
  log_population = "Population (logged)",
  log_gdppercapita = "GDP per capita (logged)",
  deaths_perthousand = "Number of deaths for every 1,000 residents",
  mayor_reelected = "Indicator for whether the incumbent was reelected in 2016 (1 = Yes, 0 = No)")

codebook::label_browser_static(survey_politicians_nonresponse_analysis)

# NOTES -- R version, platform, and loaded packages -------------------------

### sessionInfo(package = NULL)
# 
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
#   [1] xtable_1.8-4    cjoint_2.1.0    survey_4.0      survival_3.1-11 Matrix_1.2-18   lmtest_0.9-37   zoo_1.8-7      
# [8] sandwich_2.5-1  texreg_1.37.1   codebook_0.9.2  readxl_1.3.1    here_1.0.1      forcats_0.5.0   stringr_1.4.0  
# [15] dplyr_1.0.0     purrr_0.3.3     readr_1.4.0     tidyr_1.0.2     tibble_3.0.0    ggplot2_3.3.5   tidyverse_1.3.0
# 
# loaded via a namespace (and not attached):
#   [1] nlme_3.1-145      fs_1.4.1          lubridate_1.7.4   webshot_0.5.2     httr_1.4.1        rprojroot_2.0.2  
# [7] repr_1.1.0        tools_3.6.3       backports_1.1.5   utf8_1.1.4        R6_2.4.1          DT_0.13          
# [13] DBI_1.1.0         colorspace_1.4-1  withr_2.1.2       tidyselect_1.1.0  compiler_3.6.3    cli_2.3.0        
# [19] rvest_0.3.5       xml2_1.3.0        labeling_0.3      scales_1.1.0      digest_0.6.25     rmarkdown_2.6    
# [25] base64enc_0.1-3   pkgconfig_2.0.3   htmltools_0.4.0   labelled_2.5.0    fastmap_1.0.1     dbplyr_1.4.2     
# [31] highr_0.8         htmlwidgets_1.5.1 rlang_1.0.0       rstudioapi_0.11   shiny_1.4.0.2     farver_2.0.3     
# [37] generics_0.0.2    jsonlite_1.6.1    crosstalk_1.1.0.1 magrittr_1.5      Rcpp_1.0.7        munsell_0.5.0    
# [43] fansi_0.4.1       lifecycle_0.2.0   stringi_1.4.6     yaml_2.2.1        rmdpartials_0.5.8 plyr_1.8.6       
# [49] parallel_3.6.3    listenv_0.8.0     promises_1.1.0    crayon_1.3.4      lattice_0.20-40   haven_2.3.1      
# [55] splines_3.6.3     hms_0.5.3         knitr_1.28        pillar_1.4.3      codetools_0.2-16  reprex_0.3.0     
# [61] glue_1.3.2        evaluate_0.14     mitools_2.4       modelr_0.1.6      vctrs_0.3.1       httpuv_1.5.2     
# [67] cellranger_1.1.0  gtable_0.3.0      future_1.17.0     assertthat_0.2.1  xfun_0.20         mime_0.9         
# [73] skimr_2.1.3       broom_0.5.5       later_1.0.0       rsconnect_0.8.16  globals_0.12.5    ellipsis_0.3.0   