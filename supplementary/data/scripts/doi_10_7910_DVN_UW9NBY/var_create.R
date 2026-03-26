# install_version("haven", version = "2.5.4");install_version("tidyverse", version = "2.0.0")
# install_version("dplyr", version = "1.1.4");install_version("rlang", version = "1.1.3")







# List all objects in the environment
all_objects <- ls()

# Specify the objects to keep
objects_to_keep <- c("lib", 'wd')

# Identify objects to remove
objects_to_remove <- setdiff(all_objects, objects_to_keep)
rm(list = objects_to_remove)



library(haven)
library(dplyr)
library(tidyverse)
library(rlang)

###### PREPARE FUNCTIONS FOR MAKING TABLES AND ESTIMATION #####
create_indices <- function(data, id_variable, treatment_variable, control_value, ...) {

  treatment_variable <- enquo(treatment_variable)
  id_variable <- enquo(id_variable)
  index_vars <- enquos(...)

  data %>%
    dplyr::select(!!id_variable, !!treatment_variable, ...) %>%
    gather(outcome, value, -!!id_variable, -!!treatment_variable) %>%
    # Standardize outcome values to be mean 0 and sd 1 in control condition
    group_by(outcome) %>%
    mutate(value = (value - mean(value[!!treatment_variable == control_value], na.rm = TRUE)) /
             sd(value[!!treatment_variable == control_value], na.rm = TRUE)) %>%
    # For each unit, check if at least one index component is non-missing
    group_by(!!id_variable) %>%
    mutate(valid_for_index = any(!is.na(value))) %>%
    # Impute missing values by outcome and treatment condition
    group_by(!!treatment_variable, outcome) %>%
    mutate(
      value = case_when(
        # If at least one non-missing component and missing value,
        # impute with mean of that variable for the same treatment condition
        valid_for_index & is.na(value) ~ mean(value, na.rm = TRUE),
        # Otherwise, leave as realized value
        TRUE ~ value
      )
    ) %>%
    # Compute index by unit
    group_by(!!id_variable, !!treatment_variable) %>%
    # Get the mean of the outcome values by unit
    summarize(index = mean(value)) %>%
    ungroup() %>%
    # Standardize index again to create second version that is mean 0 and sd 1 in control condition
    mutate(index_std = (index - mean(index[!!treatment_variable == control_value], na.rm = TRUE)) /
             sd(index[!!treatment_variable == control_value], na.rm = TRUE))
}



pct_maker = function(x, y){
  x_pct = ifelse(x==0 | y==0, NA, x/y)
  return(x_pct)
}

count_nonna = function(x){
  count = sum(!is.na(x))
  return(count)
}

mean_nona = function(x){
  count = mean(x, na.rm = T)
  return(count)
}

sd_nona = function(x){
  count = sd(x, na.rm = T)
  return(count)
}

destring = function(x){
  destringed = x %>% as.character() %>% as.numeric()
  return(destringed)
}





###################### WORKING DIRECTORY
setwd(wd)

# Get merged pamphlet level data 
dta = read.csv("pamphlets_level.csv")



######### Number of new members outcomes #########

# Check missingness on treatment status
sum(is.na(dta$Treatment_assigned))
sum(is.na(dta$Pamphlet_ID_final))

## Create treatment-control indicator for indices
dta = dta %>% mutate(control = ifelse((Treatment_assigned==5|Treatment_assigned==10), 1, 0))


#### Define outcomes
# New member
dta$newmemb = ifelse(!is.na(dta$gpname)&!is.na(dta$district_mcs)&!is.na(dta$assembly_name_mcs_clean), 
                      1, 0)

# Motivated new member - volunteering for AAP
dta$time_aap_num = dta$time_aap %>% as.character() %>% as.numeric()
dta$motivmemb = ifelse(dta$newmemb==1&dta$time_aap_num>=0&dta$time_aap_num<3, 1, 0)


# Missed call indicator
dta$missedcall = ifelse(!(is.na(dta$uniq_id)), 1, 0) 

######### Identity and Skill outcomes ########
# Code religion & caste
# Keyword for each social group
keyword.muslim = c("muslim", "ansari", "Ansari", "iraki", "islam", "khan",
                   "md", "mohmaddan", "momin", "muslim", "pathan", "sayad", "siddiqui", 
                   "sunni")
keyword.christian = c("isai", "christian")
keyword.general = c("baniya", "Bhagat", "brahmin", "choudary", "forwad cast",
                    "Goswami", "gupta", "rajput", "thakur", "general")
keyword.SC = c("betha", "sco", "SC", "sc")
keyword.ST = c("lohar", "sarna", "st")
keyword.OBC = c("bc", "gwala", "jolha", "karmakr", "koiri", "Koiri", "kori", "kuidmi", "kurmi",
                "Kurmi", "kurmit", "kushwaha", "Kushwaha", "kuswaha", "naai", "nai", "sahu",
                "teli", "yadav", "obc")
keyword.hindu = c("hindu", "sikh", "jain", "Sanatan", "kurmi", "Ved")

# Create new variables for each group
dta = dta %>% mutate(female = ifelse(gender == 0, 1, 0), 
                       hindu = ifelse(religion %in% keyword.hindu|religion_others %in% keyword.hindu|
                                        caste %in% keyword.hindu | caste_others %in% keyword.hindu,  1, 0),
                       muslim = ifelse(religion %in% keyword.muslim|religion_others %in% keyword.muslim|
                                         caste %in% keyword.muslim | caste_others %in% keyword.muslim, 1, 0),
                       christian = ifelse(religion %in% keyword.christian|religion_others %in% keyword.christian|
                                            caste %in% keyword.christian | caste_others %in% keyword.christian, 1, 0),
                       general.caste = ifelse(religion %in% keyword.general|religion_others %in% keyword.general|
                                                caste %in% keyword.general | caste_others %in% keyword.general, 1, 0),
                       SC.caste = ifelse(religion %in% keyword.SC|religion_others %in% keyword.SC|
                                           caste %in% keyword.SC | caste_others %in% keyword.SC, 1, 0),
                       ST.group = ifelse(religion %in% keyword.ST|religion_others %in% keyword.ST|
                                           caste %in% keyword.ST | caste_others %in% keyword.ST, 1, 0),
                       OBC.caste = ifelse(religion %in% keyword.OBC|religion_others %in% keyword.OBC|
                                            caste %in% keyword.OBC | caste_others %in% keyword.OBC, 1, 0), 
                       excluded.group = ifelse(muslim == 1|christian==1|SC.caste==1|ST.group==1|female==1|
                                                 OBC.caste==1, 1, 0),
                       excluded.caste = ifelse(SC.caste==1|ST.group==1|OBC.caste==1, 1, 0))

# High school keywords
hs = c("mac", "matric", "metric", "metrik", "Automobile Engineering", 
       "graduation", "ITI-vocational", "diploma", "grade 11", "grade 10", "grade 12")

# University keywords
univ = c("B TAKE", "b tec", "B. com" ,"B. Tech", "b.com", "b.ed", "B.TECH", 
         "ba","BA" ,"BA 2nd Year", "baed",  "bcom", "bms",  "bsc",
         "electronic engineer", "i com","I.S.E","IA",  "is", "IS", "isc",
         "ISC", "llb", "LLB", "M. COM","M. Ed", "ma", "MA",
         "ma. phd",  "master", "mba","MBA","MCA (Master Of Computer Application)",
         "medical", "Medical BIMS", "MJMC , MA , PGDRD","msc","MSE", "grade 12", 
         "PhD", "post graduation", "diploma")

# Create education variables
# First clean education and other educations vars
dta = dta %>% mutate(
  educ_cl = case_when(
    !is.na(literate) & literate==0 ~ 0,
    !is.na(literate) & literate==1 ~ 1, 
    education=="grade 2" ~ 2, 
    education=="grade 3" ~ 3, 
    education=="grade 4" ~ 4,
    education=="grade 5" ~ 5,
    education=="grade 6" ~ 6,
    education=="grade 7" ~ 7,
    education=="grade 8" ~ 8,
    education=="grade 9"|education_others=="grade 9"|education_others=="non 10" ~ 9,
    education=="grade 10"|education_others=="grade 10" ~ 10,
    education=="grade 11"|education_others=="non metric" ~ 11,
    education=="grade 12"|education_others=="grade 12" ~ 12,
    education=="ITI vocational" ~ 12,
    education=="graduation"|education=="graduation" ~ 15,
    education=="diploma" ~ 15,
    education=="post graduation"|education=="post graduation" ~ 17,
    education_others=="PhD" ~ 20))

# Any education and high education
dta = dta %>% 
  mutate(
    any_educ = 0,
    any_educ = case_when(
      is.na(educ_cl) ~ 0,
      educ_cl <= 1 ~ 0,
      educ_cl > 1 ~ 1),
    educ_hi = 0, 
    educ_hi = case_when(
      is.na(educ_cl) ~ 0,
      educ_cl < 12 ~ 0,
      educ_cl >= 12 ~ 1))

#  Employment variables
dta = dta %>% 
  mutate(any.employ = 0,
         any.employ = case_when(
           employed > 90 ~ 0,
           is.na(employed) ~ 0,
           employed==0 ~ 0,
           employed==1 ~ 1))


# Other skill variables
dta = dta %>% mutate(prior.vote = 0, prior.volunteer = 0, 
                       education = as.character(education),
                       employed = as.character(employed),
                       vote_2014 = as.character(vote_2014),
                       volunteer_party = as.numeric(volunteer_party))

dta = dta %>% 
  mutate(prior.vote = ifelse(dta$vote_2014==1, 1, 0), 
       prior.volunteer = ifelse(dta$volunteer_party==1, 1, 0),
       prior.vote = ifelse(is.na(prior.vote), 0, prior.vote), 
       prior.volunteer = ifelse(is.na(prior.volunteer), 0, prior.volunteer))


# Political experience indeces
polexp <- create_indices(dta, pamph_ID, control, 1, prior.vote, prior.volunteer)
colnames(polexp) = c("pamph_ID", "control", "polexp.index", "polexp.index_std")
dta <- left_join(dta, polexp)

# Lower case all variable names
colnames(dta) = tolower(colnames(dta))

  
# Replace "NA" to NA
dta = apply(dta, 2, FUN = function(x) ifelse(x=="NA", NA, x)) %>% as.data.frame()

# Replace colnames with clean labels
cols = colnames(dta)
colnames(dta) = colnames(dta) %>% tolower()
colnames(dta) = gsub("\\.", "_", colnames(dta))

# Clear key variables and create main treatment variables
dta = dta %>% ungroup() %>%
  mutate(assembly_id = assembly_id %>% as.character() %>% as.numeric(),
         female = female %>% destring(),
         female = ifelse(is.na(female), 0, female),
         excluded_group = excluded_group %>% as.character() %>% as.numeric(),
         excluded_group = ifelse(is.na(excluded_group), 0, excluded_group),
         excluded_religion = ifelse(religion == "hindu" | is.na(religion), 0, 1),
         treatment_assigned = treatment_assigned %>% as.character() %>% as.numeric(),
         pamphlet_id_final = pamphlet_id_final %>% as.character() %>% as.numeric(),
         t_base = ifelse(treatment_assigned==5|treatment_assigned==10, 1, 0),
         t_cand = ifelse(treatment_assigned==1|treatment_assigned==6, 1, 0),
         t_care = ifelse(treatment_assigned==2|treatment_assigned==7, 1, 0),
         t_ideo = ifelse(treatment_assigned==3|treatment_assigned==8, 1, 0),
         t_poli = ifelse(treatment_assigned==4|treatment_assigned==9, 1, 0),
         t_base_takeup = ifelse(pamphlet_id_final==5|pamphlet_id_final==10, 1, 0),
         t_cand_takeup = ifelse(pamphlet_id_final==1|pamphlet_id_final==6, 1, 0),
         t_care_takeup = ifelse(pamphlet_id_final==2|pamphlet_id_final==7, 1, 0),
         t_ideo_takeup = ifelse(pamphlet_id_final==3|pamphlet_id_final==8, 1, 0),
         t_poli_takeup = ifelse(pamphlet_id_final==4|pamphlet_id_final==9, 1, 0),
         excluded_caste = excluded_caste %>% as.character() %>% as.numeric(),
         excluded_religion = excluded_religion %>% as.character() %>% as.numeric()) %>%
  group_by(assembly_id, name_order) %>%
  mutate(id_vp = cur_group_id()) 

dta = dta %>% mutate(
  newmemb = newmemb %>% as.character() %>% as.numeric(),
  motivmemb = motivmemb %>% as.character() %>% as.numeric(),
  missedcall = missedcall %>% as.character() %>% as.numeric(),
  any_employ = any_employ %>% as.character() %>% as.numeric(),
  educ_hi = educ_hi  %>% as.character() %>% as.numeric(),
  polexp_index = polexp_index %>% as.character() %>% as.numeric(),
  prior_vote = prior_vote %>% as.character() %>% as.numeric(),
  prior_volunteer = prior_volunteer %>% as.character() %>% as.numeric(),
  any_educ = any_educ %>% as.character() %>% as.numeric(),
  hindu = hindu %>% as.character() %>% as.numeric(),
  muslim = destring(muslim),
  consented_21 = destring(consented_21),
  sc_caste = destring(sc_caste),
  obc_caste = destring(obc_caste),
  st_group = destring(st_group))

# add missing religious groups
dta = dta %>%
  mutate(christian = ifelse(religion=="christian", 1, 0)) %>%
  mutate(christian = ifelse(is.na(christian), 0, christian)) %>%
  mutate(other_relig = ifelse(religion!="christian" & religion!="muslim" & religion!="hindu", 1, 0)) %>%
  mutate(other_relig = ifelse(is.na(other_relig), 0, other_relig)) %>%
  mutate(obc_caste = ifelse(obc_caste==1, 1, 0)) %>%
  mutate(obc_caste = ifelse(is.na(obc_caste), 0, obc_caste)) 




# Define outcomes on volunteering
dta = dta %>%
  mutate(time_aap_full = ifelse(time_aap==1, 1, 0)) %>%
  mutate(time_aap_full = ifelse(is.na(time_aap_full), 0, time_aap_full)) %>%
  mutate(time_aap_part = ifelse(time_aap==2, 1, 0)) %>%
  mutate(time_aap_part = ifelse(is.na(time_aap_part), 0, time_aap_part))


# Define new skill index 
dta = dta %>%
  mutate(skill_index = ifelse(any_educ==1|any_employ==1|prior_volunteer==1|prior_vote==1, 1, 0)) %>%
  mutate(skill_index = ifelse(is.na(skill_index), 0, skill_index))


####### Recall survey coding #########
## Add variables from the recall survey
dta = dta %>%
  mutate(contacted_21 = destring(contacted_21)) %>%
  mutate(consented_21 = destring(consented_21)) %>%
  mutate(contacted_21 = ifelse(is.na(contacted_21), 0, contacted_21)) %>%
  mutate(consented_21 = ifelse(is.na(consented_21), 0, consented_21)) 


# Create variables for follow-up survey
dta = dta %>%
  mutate(excluded_group_21 = ifelse(consented_21==1 & excluded_group == 1, 1, 0)) %>%
  mutate(female_21 = ifelse(consented_21==1 & female == 1, 1, 0)) %>%
  mutate(excluded_caste_21 = ifelse(consented_21==1 & excluded_caste == 1, 1, 0)) %>%
  mutate(excluded_religion_21 = ifelse(consented_21==1 & excluded_religion == 1, 1, 0)) %>%
  mutate(any_employ_21 = ifelse(consented_21==1 & any_employ == 1, 1, 0)) %>%
  mutate(educ_hi_21 = ifelse(consented_21==1 & educ_hi == 1, 1, 0)) %>%
  mutate(polexp_index_21 = ifelse(consented_21==1, polexp_index, 0)) %>%
  mutate(prior_vote_21 = ifelse(consented_21==1 & prior_vote == 1, 1, 0)) %>%
  mutate(prior_volunteer_21 = ifelse(consented_21==1 & prior_volunteer == 1, 1, 0))  %>%
  mutate(skill_index_21 = ifelse(skill_index==1 & consented_21==1, 1, 0))

# create a variable for included groups
dta <- dta %>%
  mutate(included_groups = ifelse(excluded_group!=1 & missedcall==1, 1, 0))
# create a variable for unskilled groups
dta = dta %>%
  mutate(unskilled = ifelse(skill_index!=1 & missedcall==1, 1, 0))

# VP ID that reflects clustering by wards
dta = dta %>% group_by(id_vp, gp_ward_final) %>%
  mutate(id_vp2 = group_indices())

# add variable for female pamphlet treatment
dta = dta %>%
  mutate(fem_treat_assigned = case_when(
    treatment_assigned <= 5 ~ 0,
    treatment_assigned > 5 ~ 1
  ),
  fem_treat_takeup = case_when(
    pamphlet_id_final <= 5 ~ 0,
    pamphlet_id_final > 5 ~ 1
  ),
  t_fem = ifelse(fem_treat_assigned==1, 1, 0),
  t_fem_takeup = ifelse(fem_treat_takeup==1, 1, 0))

# multiply all outcomes by a 1,000 for interpretability 
dta = dta %>%
  mutate_at(vars(missedcall, newmemb, motivmemb, excluded_group, female,
                 excluded_caste, excluded_religion, consented_21, excluded_group_21,
                 skill_index_21,
                 sc_caste, st_group, obc_caste, muslim, hindu,christian,
                 other_relig, any_employ, educ_hi,
                 prior_vote, prior_volunteer, skill_index, included_groups,
                 unskilled, excluded_caste_21, excluded_religion_21, 
                 female_21, educ_hi_21, any_employ_21, prior_vote_21,
                 prior_volunteer_21, time_aap_full, time_aap_part), 
            .funs = function(x){x*1000})


# Create new variables for each treatment
dta = dta %>%
  mutate(t_base_male_takeup = ifelse(t_base_takeup==1 & t_fem_takeup==0, 1, 0)) %>%
  mutate(t_base_female_takeup = ifelse(t_base_takeup==1 & t_fem_takeup==1, 1, 0)) %>%
  mutate(t_cand_male_takeup = ifelse(t_cand_takeup==1 & t_fem_takeup==0, 1, 0)) %>%
  mutate(t_cand_female_takeup = ifelse(t_cand_takeup==1 & t_fem_takeup==1, 1, 0)) %>%
  mutate(t_care_male_takeup = ifelse(t_care_takeup==1 & t_fem_takeup==0, 1, 0)) %>%
  mutate(t_care_female_takeup = ifelse(t_care_takeup==1 & t_fem_takeup==1, 1, 0)) %>%
  mutate(t_poli_male_takeup = ifelse(t_poli_takeup==1 & t_fem_takeup==0, 1, 0)) %>%
  mutate(t_poli_female_takeup = ifelse(t_poli_takeup==1 & t_fem_takeup==1, 1, 0)) %>%
  mutate(t_ideo_male_takeup = ifelse(t_ideo_takeup==1 & t_fem_takeup==0, 1, 0)) %>%
  mutate(t_ideo_female_takeup = ifelse(t_ideo_takeup==1 & t_fem_takeup==1, 1, 0)) %>%
  mutate(t_base_male = ifelse(t_base==1 & t_fem==0, 1, 0)) %>%
  mutate(t_base_female = ifelse(t_base==1 & t_fem==1, 1, 0)) %>%
  mutate(t_cand_male = ifelse(t_cand==1 & t_fem==0, 1, 0)) %>%
  mutate(t_cand_female = ifelse(t_cand==1 & t_fem==1, 1, 0)) %>%
  mutate(t_care_male = ifelse(t_care==1 & t_fem==0, 1, 0)) %>%
  mutate(t_care_female = ifelse(t_care==1 & t_fem==1, 1, 0)) %>%
  mutate(t_poli_male = ifelse(t_poli==1 & t_fem==0, 1, 0)) %>%
  mutate(t_poli_female = ifelse(t_poli==1 & t_fem==1, 1, 0)) %>%
  mutate(t_ideo_male = ifelse(t_ideo==1 & t_fem==0, 1, 0)) %>%
  mutate(t_ideo_female = ifelse(t_ideo==1 & t_fem==1, 1, 0))

# keep analysis variables
dta = dta %>% ungroup %>%
  dplyr::select(
    treatment_assigned, fem_treat_assigned, t_base,
    id_vp2, assembly_id, missedcall, consented_21, excluded_group,
    t_fem, t_cand, t_poli, t_ideo, t_care, t_fem_takeup,
    t_base_male, t_base_female, t_base_male_takeup, t_base_takeup, t_base_female_takeup,
    t_cand_male, t_cand_female, t_cand_male_takeup, t_cand_takeup, t_cand_female_takeup,
    t_care_male, t_care_female, t_care_male_takeup, t_care_takeup, t_care_female_takeup,
    t_ideo_male, t_ideo_female, t_ideo_male_takeup, t_ideo_takeup, t_ideo_female_takeup,
    t_poli_male, t_poli_female, t_poli_male_takeup, t_poli_takeup, t_poli_female_takeup,
    female, excluded_caste, sc_caste, st_group, obc_caste,
    excluded_religion, muslim, christian, other_relig, hindu,
    skill_index, any_employ, educ_hi, prior_vote, prior_volunteer,
    excluded_group_21, skill_index_21, newmemb, motivmemb, included_groups,
    unskilled, female_21, excluded_caste_21, excluded_religion_21,
    any_employ_21, educ_hi_21, prior_vote_21, prior_volunteer_21)



####### Output final replication dataset ########
write.csv(dta, "aap_pamphlets_replication.csv", row.names = F)
