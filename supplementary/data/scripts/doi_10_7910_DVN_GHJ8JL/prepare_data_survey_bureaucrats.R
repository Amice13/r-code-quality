### Replication code
### Article: "How patronage delivers: Political appointments, bureaucratic accountability, and service delivery in Brazil"
### Author: Guillermo Toral (www.guillermotoral.com)
### Date: June 2022
### This file prepares the bureaucrat survey data
### R version, platform, and package versions reported at the end of the file

### Note that this file uses the original survey data, which are not included in the replication package to protect human subjects
### The author will not share the original survey files to protect subjects (which are easily identifiable). However requests for descriptive statistics can be sent to the author (contact info at www.guillermotoral.com)

# Prepare the environment -------------------------------------------------

### This section of the code prepares the environment 

# Clean the environment
rm(list = ls())

# Install required packages if not previously installed
package_list <- c("tidyverse", "here", "codebook") 
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

# Load and clean original school director survey dataset ------------------------------------------------------------

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
  mutate(municipality = p2_1, 
         experience_manager = ifelse(p13=="Menos de 1 ano", 0, ifelse(p13=="More than 20", 21, as.numeric(p13))),
         experience_professional = ifelse(p14=="More than 40", 41, as.numeric(p14)),
         appointed = ifelse(p16 == "Appointment", 1, 0),
         elected = ifelse(p16 == "Election", 1, 0),
         civil_service = ifelse(p16 == "Public civil service exam", 1, 0),
         percent_votes_election = p22, 
         only_one_candidate_election = ifelse(p25=="Only 1", 1, 0),
         age = as.numeric(p33),
         female = ifelse(p34=="Woman", 1, 0),
         lives_in_municipality = ifelse(p30=="Yes", 1, 0), 
         education_college = ifelse(p31 == "Complete bachelors degree",1,0),
         education_morethan_college = grepl("Graduate", p31),
         education_lessthan_college = ifelse(education_college == 0 & education_morethan_college==0,1,0),
         education_highschool_orless = ifelse(p31 != "Incomplete bachelors degree" & education_college==0 & education_morethan_college==0,1,0),
         no_other_jobs = ifelse(p28=="Does not have other jobs outside this school",1,0),
         meetings_with_clients = as.numeric(p40_1_8),
         meetings_with_technicians = as.numeric(p40_2_8),
         meetings_with_professionals = as.numeric(p40_3_8),
         meetings_with_secretary = as.numeric(p40_4_8),
         meetings_with_mayor = as.numeric(p40_5_8),
         meetings_with_citycouncilor = as.numeric(p40_6_8),
         trust_mayor = match(p44_1, answer_categories),
         alignment_mayor_professionals = match(p44_2, answer_categories),
         mayor_concerned = match(p44_3, answer_categories),
         trust_secretary = match(p44_4, answer_categories),
         proximity_to_mayor = match(p46_1, answer_categories_very),
         proximity_to_secretary = match(p46_2, answer_categories_very),
         secretariat_holds_us_accountable = match(p44_8, answer_categories),
         secretariat_supports_us_toimprove_results = match(p44_10, answer_categories),
         union_member = ifelse(p68=="Yes",1,ifelse(p68=="No", 0, NA)),
         party_member = ifelse(p69=="Yes",1,ifelse(p69=="No", 0, NA)),
         worked_for_campaign = ifelse(p71=="Yes",1,ifelse(p71=="No", 0, NA)),
         political_links_influence_temp_hiring = match(p66_1, answer_categories),
         sector = "education") %>%
  dplyr::select(municipality:sector)

# Load and clean original clinic manager survey dataset ------------------------------------------------------------

# Load the original dataset (not included in the replication package to protect human subjects)
d <- read_csv("../../datasets/collected/survey_bureaucrats/survey_ubs_original.csv") 

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
ubs_managers <- d %>%
  mutate(municipality = p2_1, 
         experience_manager = ifelse(p13=="Menos de 1 ano", 0, ifelse(p13=="More than 20", 21, as.numeric(p13))),
         experience_professional = ifelse(p13=="Menos de 1 ano", 0, ifelse(p14=="More than 40", 41, as.numeric(p14))),
         appointed = ifelse(p16 == "Appointment", 1, 0),
         elected = ifelse(p16 == "Election", 1, 0),
         civil_service = ifelse(p16 == "Public civil service exam", 1, 0),
         percent_votes_election = p22, 
         only_one_candidate_election = ifelse(p25=="Only 1", 1, 0),
         age = as.numeric(p33),
         female = ifelse(p34=="Woman", 1, 0),
         lives_in_municipality = ifelse(p30=="Yes", 1, 0), 
         education_college = ifelse(p31 == "Complete bachelors degree",1,0),
         education_morethan_college = grepl("Graduate", p31),
         education_lessthan_college = ifelse(education_college == 0 & education_morethan_college==0,1,0),
         education_highschool_orless = ifelse(p31 != "Incomplete bachelors degree" & education_college==0 & education_morethan_college==0,1,0),
         no_other_jobs = ifelse(p28=="Does not have other jobs outside this school",1,0),
         meetings_with_clients = as.numeric(p40_1_8),
         meetings_with_technicians = as.numeric(p40_2_8),
         meetings_with_professionals = as.numeric(p40_3_8),
         meetings_with_secretary = as.numeric(p40_4_8),
         meetings_with_mayor = as.numeric(p40_5_8),
         meetings_with_citycouncilor = as.numeric(p40_6_8),
         trust_mayor = match(p44_1, answer_categories),
         alignment_mayor_professionals = match(p44_2, answer_categories),
         mayor_concerned = match(p44_3, answer_categories),
         trust_secretary = match(p44_4, answer_categories),
         proximity_to_mayor = match(p46_1, answer_categories_very),
         proximity_to_secretary = match(p46_2, answer_categories_very),
         secretariat_holds_us_accountable = match(p44_8, answer_categories),
         secretariat_supports_us_toimprove_results = match(p44_10, answer_categories),
         union_member = ifelse(p63=="Yes",1,ifelse(p63=="No", 0, NA)),
         party_member = ifelse(p64=="Yes",1,ifelse(p64=="No", 0, NA)),
         worked_for_campaign = ifelse(p66=="Yes",1,ifelse(p66=="No", 0, NA)),
         political_links_influence_temp_hiring = match(p61_1, answer_categories),
         sector = "healthcare") %>%
  dplyr::select(municipality:sector)

# Load and clean original social assistance center manager survey dataset ------------------------------------------------------------

# Load the original dataset (not included in the replication package to protect human subjects)
d <- read_csv("../../datasets/collected/survey_bureaucrats/survey_cras_original.csv") 

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
cras_managers <- d %>%
  mutate(municipality = p2_1, 
         experience_manager = ifelse(p13=="Less than 1 year", 0, ifelse(p13=="More than 20", 21, as.numeric(p13))),
         experience_professional = ifelse(p14=="Less than a year", 0, as.numeric(p14)),
         appointed = ifelse(p16 == "Appointment", 1, 0),
         elected = ifelse(p16 == "Election", 1, 0),
         civil_service = ifelse(p16 == "Public civil service exam", 1, 0),
         percent_votes_election = p22, 
         only_one_candidate_election = ifelse(p25=="Only 1", 1, 0),
         age = as.numeric(p33),
         female = ifelse(p34=="Woman", 1, 0),
         lives_in_municipality = ifelse(p30=="Yes", 1, 0), 
         education_college = ifelse(p31 == "Complete bachelors degree",1,0),
         education_morethan_college = grepl("Graduate", p31),
         education_lessthan_college = ifelse(education_college == 0 & education_morethan_college==0,1,0),
         education_highschool_orless = ifelse(p31 != "Incomplete bachelors degree" & education_college==0 & education_morethan_college==0,1,0),
         no_other_jobs = ifelse(p28=="Does not have other jobs outside this school",1,0),
         meetings_with_clients = as.numeric(p40_1_8),
         meetings_with_technicians = as.numeric(p40_2_8),
         meetings_with_professionals = as.numeric(p40_3_8),
         meetings_with_secretary = as.numeric(p40_4_8),
         meetings_with_mayor = as.numeric(p40_5_8),
         meetings_with_citycouncilor = as.numeric(p40_6_8),
         trust_mayor = match(p44_1, answer_categories),
         alignment_mayor_professionals = match(p44_2, answer_categories),
         mayor_concerned = match(p44_3, answer_categories),
         trust_secretary = match(p44_4, answer_categories),
         proximity_to_mayor = match(p46_1, answer_categories_very),
         proximity_to_secretary = match(p46_2, answer_categories_very),
         secretariat_holds_us_accountable = match(p44_8, answer_categories),
         secretariat_supports_us_toimprove_results = match(p44_10, answer_categories),
         union_member = ifelse(p63=="Yes",1,ifelse(p63=="No", 0, NA)),
         party_member = ifelse(p64=="Yes",1,ifelse(p64=="No", 0, NA)),
         worked_for_campaign = ifelse(p66=="Yes",1,ifelse(p66=="No", 0, NA)),
         political_links_influence_temp_hiring = match(p61_1, answer_categories),
         sector = "social assistance") %>%
  dplyr::select(municipality:sector)

# Merge, clean election vote shares, generate indicator for whether all respondents are in the same municipality, and remove municipality identifier --------------------------------------------------------

# Merge all three types of bureaucrats in a single dataset
b <- bind_rows(escola_managers, ubs_managers, cras_managers)

# Clean responses to the question on percent vote share for elected managers
### Change "," for "."
b$percent_votes_election <- gsub(",", ".", b$percent_votes_election)
### Remove percent signs
b$percent_votes_election <- gsub("%", "", b$percent_votes_election)
### Do calculation for observation where a faction is entered
b$percent_votes_election[b$percent_votes_election=="174/200"] <- 174/200*100
### Substitute "Nao lembra" (they don't remember) for an NA
b$percent_votes_election[b$percent_votes_election=="Nao lembra"] <- NA
### Substitute "57 (diferença de 3 votos)" (57, difference of 3 votes) for 57
b$percent_votes_election[b$percent_votes_election=="57 (diferença de 3 votos)"] <- 57
### Remove words
b$percent_votes_election <- gsub("[^[:digit:]., ]", "", b$percent_votes_election)
### Convert to numeric
b$percent_votes_election <- as.numeric(b$percent_votes_election)

# Generate municipality-level shares of respondents who are politically appointed
bm <- b %>%
  group_by(municipality) %>%
  summarise(share_appointed = mean(appointed))
municipalities_all_appointed <- bm[which(bm$share_appointed==1),"municipality"][[1]]

# All surveyed managers are appointed in 37% of municipalities in the sample
length(municipalities_all_appointed)/n_distinct(b$municipality)

# Create indicator 
b$municipality_all_appointed <- ifelse(b$municipality %in% municipalities_all_appointed, 1, 0)

# Remove municipality identifier, to protect human subjects

b <- b %>%
  dplyr::select(-municipality)

# Export and generate codebook --------------------------------------------

write_csv(b, "../../datasets/analysis/survey_bureaucrats/survey_bureaucrats_analysis.csv")

# Generate codebook with variable labels and values

survey_bureaucrats_analysis <- read_csv("../../datasets/analysis/survey_bureaucrats/survey_bureaucrats_analysis.csv")

var_label(survey_bureaucrats_analysis) <- list(
  experience_manager = "Years of experience as a manager (Number of years, NA = missing)",
  experience_professional = "Years of experience as a professional in the sector (Number of years, NA = missing)",
  appointed = "Indicator for whether the respondent is appointed (1 = Yes, 0 = No)",
  elected = "Indicator for whether the respondent is elected (1 = Yes, 0 = No)",
  civil_service = "Indicator for whether the respondent is selected through the civil service (1 = Yes, 0 = No)",
  percent_votes_election = "Percent of votes elected managers report in their own, latest election (% votes, NA = not applicable or missing)",
  only_one_candidate_election = "Indicator for whether elected managers report they ran unopposed (1 = Yes, 0 = No, NA = not applicable or missing)",
  age = "Respondent's age",
  female = "Indicator for whether the respondent is female (1 = Yes, 0 = No)",
  lives_in_municipality = "Indicator for whether the respondent lives in the municipality where they work (1 = Yes, 0 = No)",
  education_college = "Indicator for whether the respondent has only a college degree (1 = Yes, 0 = No)",
  education_morethan_college = "Indicator for whether the respondent has more than a college degree (1 = Yes, 0 = No)",
  education_lessthan_college = "Indicator for whether the respondent has less than a college degree (1 = Yes, 0 = No)",
  education_highschool_orless = "Indicator for whether the respondent has only a high school degree or less (1 = Yes, 0 = No)",
  no_other_jobs = "Indicator for whether the respondent only works as manager of this unit (1 = Yes, 0 = No, NA = missing)",
  meetings_with_clients = "Number of meetings the respondent reports, in the previous 3 months, with clients of their unit, e.g., parents, patients (Number of meetings, NA = missing)",
  meetings_with_technicians = "Number of meetings the respondent reports, in the previous 3 months, with technical staff in the secretariat (Number of meetings, NA = missing)",
  meetings_with_professionals = "Number of meetings the respondent reports, in the previous 3 months, with professionals of their unit, e.g., teachers, nurses, social workers (Number of meetings, NA = missing)", 
  meetings_with_secretary = "Number of meetings the respondent reports, in the previous 3 months, with the municipal secretary of their sector (Number of meetings, NA = missing)",
  meetings_with_mayor = "Number of meetings the respondent reports, in the previous 3 months, with the mayor (Number of meetings, NA = missing)",
  meetings_with_citycouncilor = "Number of meetings the respondent reports, in the previous 3 months, with the city councilor (Number of meetings, NA = missing)",
  trust_mayor = "Respondent's level of agreement with the statement 'I trust the mayor' (1 = Not at all, 2 = A little, 3 = Quite, 4 = A lot, NA = missing)",
  alignment_mayor_professionals = "Respondent's level of agreement with the statement 'The mayor and professionals in [respondent's sector] have the same priorities with regards to [schools / clinics / centers]' (1 = Not at all, 2 = A little, 3 = Quite, 4 = A lot, NA = missing)",
  mayor_concerned = "Respondent's level of agreement with the statement 'The mayor is concerned with improving the quality of [schools / clinics / centers]' (1 = Not at all, 2 = A little, 3 = Quite, 4 = A lot, NA = missing)",
  trust_secretary = "Respondent's level of agreement with the statement 'I trust the secretary' (1 = Not at all, 2 = A little, 3 = Quite, 4 = A lot, NA = missing)",
  proximity_to_mayor = "Respondent's level of agreement with the statement 'I feel close to the mayor' (1 = Not at all, 2 = A little, 3 = Quite, 4 = A lot, NA = missing)",
  proximity_to_secretary = "Respondent's level of agreement with the statement 'I feel close to the secretary' (1 = Not at all, 2 = A little, 3 = Quite, 4 = A lot, NA = missing)",
  secretariat_holds_us_accountable = "Respondent's level of agreement with the statement 'The secretariat holds us accountable for the results of this [school / clinic / center]' (1 = Not at all, 2 = A little, 3 = Quite, 4 = A lot, NA = missing)",
  secretariat_supports_us_toimprove_results = "Respondent's level of agreement with the statement 'The secretariat helps us improve the performance of this [school / clinic / center]' (1 = Not at all, 2 = A little, 3 = Quite, 4 = A lot, NA = missing)",
  union_member = "Indicator for whether the respondent belongs to a union (1 = Yes, 0 = No, NA = missing)",
  party_member = "Indicator for whether the respondent belongs to a political party (1 = Yes, 0 = No, NA = missing)",
  worked_for_campaign = "Indicator for whether the respondent has worked for an electoral campaign in the municipality (1 = Yes, 0 = No, NA = missing)",
  political_links_influence_temp_hiring = "Respondent's response to the question 'In general, how much do you think political connections influence the hiring of temporary professionals [in their sector]' (1 = Not at all, 2 = A little, 3 = Quite, 4 = A lot, NA = missing)",
  sector = "Respondent's sector (education / healthcare / social assistance)",
  municipality_all_appointed = "Indicator for whether all respondents surveyed in the municipality are politically appointed (1 = Yes, 0 = No)"
  )

codebook::label_browser_static(survey_bureaucrats_analysis)

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