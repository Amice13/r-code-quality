# Replication file for "Knowledge of Social Rights as Political Knowledge"
 # by Rebecca Weitz-Shapiro and Matthew S. Winters, published in 
 # Political Behavior

######################
######################
## SET UP WORKSPACE ##
######################
######################

library(tidyverse)
library(survey)
library(janitor)

 # Function for Simulation-Based Tests of Differences in Coefficients
sims.diff.test <- function(model1, model2, var, n.sims=1000){
  pk.sims <- rnorm(n.sims, mean=coef(model1)[var], 
                   sd=coef(summary(model1))[var,2])
  ksr.sims <- rnorm(n.sims, mean=coef(model2)[var], 
                   sd=coef(summary(model2))[var,2])
  diff.sims <- pk.sims - ksr.sims
  print(quantile(diff.sims, probs=c(0.025,0.975)))
}

###########################
###########################
## BRAZIL DATA ANALYSIS ##
###########################
###########################

#########################
# Brazil Data: Recoding #
#########################

brazil_data_raw = read_csv("Brazil_survey_PKKSR_replication.csv")

### Get Rid of Observations Where All PK Questions are NAs

 # This code looks at the two rounds of PK questions and retains observations
  # that include at least one non-NA answer
 # We then do the same thing for the two rounds of KSR questions

brazil_data <- brazil_data_raw %>% filter_at(vars(Q5.3_7_TEXT, Q5.4, Q5.5, 
  Q5.6, Q5.7, Q5.8, Q5.9, Q6.1_10_TEXT, Q6.2, Q6.3, Q6.4, Q6.5, Q6.6, Q6.7), 
  any_vars(!is.na(.))) 

 # This command drops 115 observations (2,029 --> 1,914)

brazil_data <- brazil_data %>% filter_at(vars(Q5.19, Q5.20, Q5.21, Q5.22,
  Q5.23, Q6.10, Q6.11, Q6.12, Q6.13, Q6.14), 
  any_vars(!is.na(.)))

 # The command above drops an additional 59 observations (1,914 -> 1,855)

### Code PK and KSR Questions (Two Passes for Each Question)

brazil_data %>%
  mutate(
    pk_correct_states = Q5.3_7_TEXT == "26",
    pk_correct_judges = Q5.4 == "19", # 19 indicates R selected 11 (the correct #)
    pk_correct_term = Q5.5 == "12", # 12 indicates R selected 4 (the correct #)
    pk_correct_econ = Q5.6 == "9", # 9 indicates R selected Guedes
    pk_correct_vp = Q5.7 == "14", # 14 indicates R selected Mourao
    pk_correct_opposition = Q5.8 == "14", # 14 indicates R selected PT
    pk_correct_bolsonaro = Q5.9 == "11", # 11 indicates R selected PSL
    manipulation_check = tolower(Q5.10_89_TEXT) == "k" # k denotes passed
  ) -> brazil_data

brazil_data %>%
  mutate(
    pk_correct_states2 = Q6.1_10_TEXT == "26",
    pk_correct_judges2 = Q6.2 == "27", # 27 indicates R selected 11 (the correct #)
    pk_correct_term2 = Q6.3 == "16", # 16 indicates R selected 4 (the correct #)
    pk_correct_econ2 = Q6.4 == "13", # 13 indicates R selected Guedes
    pk_correct_vp2 = Q6.5 == "18", # 18 indicates R selected Mourao
    pk_correct_opposition2 = Q6.6 == "18", # 18 indicates R selected PT
    pk_correct_bolsonaro2 = Q6.7 == "19" # 19 indicates R selected PSL
  ) -> brazil_data

brazil_data %>%
  mutate(
    ksr_correct_welfare = Q5.19 == "6", 
    ksr_correct_edu = Q5.20 == "18",
    ksr_correct_salary = Q5.21 == "15",
    ksr_correct_pubedu = Q5.22 == "10",
    ksr_correct_pubedu_alt = Q5.22 == "10" | Q5.22 == "11",
    ksr_correct_bolsa = Q5.23 == "12"
  ) -> brazil_data

brazil_data %>%
  mutate(
    ksr_correct_welfare2 = Q6.10 == "10", 
    ksr_correct_edu2 = Q6.11 == "27",
    ksr_correct_salary2 = Q6.12 == "19",
    ksr_correct_pubedu2 = Q6.13 == "16",
    ksr_correct_pubedu2_alt = Q6.13 == "16" | Q6.13 == "18",
    ksr_correct_bolsa2 = Q6.14 == "15"
  ) -> brazil_data

### Create PK and KSR Indices (and Scaled Versions)

brazil_data %>% 
  mutate(pk1 = select(., pk_correct_states, pk_correct_states2) 
            %>% rowSums(na.rm=TRUE),
         pk2 = select(., pk_correct_judges, pk_correct_judges2)
            %>% rowSums(na.rm=TRUE),
         pk3 = select(., pk_correct_term, pk_correct_term2)
            %>% rowSums(na.rm=TRUE),
         pk4 = select(., pk_correct_econ, pk_correct_econ2)
            %>% rowSums(na.rm=TRUE),
         pk5 = select(., pk_correct_vp, pk_correct_vp2)
            %>% rowSums(na.rm=TRUE),
         pk6 = select(., pk_correct_opposition, pk_correct_opposition2)
            %>% rowSums(na.rm=TRUE),
         pk7 = select(., pk_correct_bolsonaro, pk_correct_bolsonaro2)
            %>% rowSums(na.rm=TRUE),
         ksr1 = select(., ksr_correct_welfare, ksr_correct_welfare2)
            %>% rowSums(na.rm=TRUE),
         ksr2 = select(., ksr_correct_edu, ksr_correct_edu2)
            %>% rowSums(na.rm=TRUE),
         ksr3 = select(., ksr_correct_salary, ksr_correct_salary2)
            %>% rowSums(na.rm=TRUE),
         ksr4 = select(., ksr_correct_pubedu, ksr_correct_pubedu2)
            %>% rowSums(na.rm=TRUE),
         ksr5 = select(., ksr_correct_bolsa, ksr_correct_bolsa2)
            %>% rowSums(na.rm=TRUE),
         ksr4_alt = select(., ksr_correct_pubedu_alt, ksr_correct_pubedu2_alt)
            %>% rowSums(na.rm=TRUE)) %>%
  mutate(pk_ind = pk1 + pk2 + pk3 + pk4 + pk5 + pk6 + pk7,
         ksr_ind = ksr1 + ksr2 + ksr3 + ksr4 + ksr5,
         ksr_ind_alt = ksr1 + ksr2 + ksr3 + ksr4_alt + ksr5,
         ksr_what = ksr1 + ksr2 + ksr3) %>%
  mutate(pk_ind_scaled = pk_ind/7,
         ksr_ind_scaled = ksr_ind/5,
         ksr_ind_alt_scaled = ksr_ind_alt/5,
         ksr_what_scaled = ksr_what/3) -> brazil_data

 # Create 21 permutations of five-item PK indices
temp1 <- gtools::combinations(7,5, c('pk1','pk2','pk3', 'pk4', 'pk5', 'pk6', 'pk7'))
 
for(i in 1:21){
  pk_ind_5. <- rowSums(brazil_data[,temp1[i,]])/5   # calculating sum of a five chosen PK Qs for each respondent
  brazil_data[, ncol(brazil_data)+1] <- pk_ind_5.  # creating a new variable at the end of the last column
  colnames(brazil_data)[ncol(brazil_data)] <- paste0("pk_ind_5.",i) # labeling the new column
}

### Predictors and Weighting Variables

brazil_data %>%
  mutate(edu = as.numeric(Q16.3)) %>%
  mutate(
    female = as.numeric(Q16.4 == "15"), # 1 = female
    edu = 
      0*(edu == 1110) + # No school
      1*(edu >= 1111 & edu <= 1117) + # Some primary
      2*(edu == 1118) + # Completed primary
      3*(edu >= 1119 & edu <= 1120) + # Some secondary
      4*(edu == 1121) + # Completed secondary
      5*(edu >= 1122 & edu <= 1124) + # Some university
      6*(edu == 1125) + # Completed 4-year degree
      7*(edu >= 1126), # Graduate degree
    ses = as.numeric(as.factor(Q16.1)) - 1,         # SES scale from 0 to 16
    ses_alt = as.numeric(Q16.2) - 1) %>%   # Alternate SES scale from 0 to 4: 4 = never have difficulty covering monthly expenses; 0 = always
  mutate(edu_3cat = case_when(
     edu %in% 0:3 ~ "Less than High School",
     edu %in% 4 ~ "High School",
     edu %in% 5:7 ~ "College or More"),
    gender_cat = factor(female, labels = c("male", "female")),
    region_cat = factor(Q5.11, labels=c("Norte", "Nordeste", 
      "Centro-Oeste", "Sudeste", "Sul"))) -> brazil_data                 

 # In order to weight by income, we want to turn the income scale into
  # multiples of the monthly salary R$937 in order to correspond to population
  # proportions.  Because our survey question about income and the 
  # data on the Brazilian population use different cutpoints, we end
  # up using three broad income categories.

brazil_data %>% 
  mutate(ses_weight=
      0*(ses>=0 & ses<=4) +
      1*(ses>=5 & ses<=10) +
      2*(ses>=11 & ses<=16)) -> brazil_data

### Survey Weights

# Can Only Weight if Values are Not Missing
brazil_data_dropna <- drop_na(brazil_data, c("gender_cat", "region_cat", 
  "edu_3cat", "ses_weight"))
# Put Unweighted Data into svydesign() Format
brazil_data_unweighted <- svydesign(ids=~1, data=brazil_data_dropna)

# Create Data Frames with Population Margins
pop.gen <- data.frame(gender_cat = c("male", "female"),
  Freq = nrow(brazil_data_unweighted) * c(0.4897, 0.5103))
pop.reg <- data.frame(region_cat = c("Centro-Oeste","Nordeste","Norte","Sudeste","Sul"),
  Freq = nrow(brazil_data_unweighted)* c(0.0744,0.2779,0.0841,0.4206,0.1430))
pop.edu <- data.frame(edu_3cat = c("Less than High School", "High School", "College or More"),
  Freq = nrow(brazil_data_unweighted)* c(0.526,0.269,0.205))
pop.ses <- data.frame(ses_weight = 0:2,
  Freq = nrow(brazil_data_unweighted)* c(0.509, 0.287, 0.204))

brazil_data_weighted <- rake(
  design = brazil_data_unweighted,
  sample.margins = list( ~ gender_cat,  ~ region_cat, ~ edu_3cat, ~ses_weight),
  population.margins = list(pop.gen, pop.reg, pop.edu, pop.ses)
)

 # To trim these weights, we identify the 5th and 95th percentile 
  # weights and trim to those values.

weight.min <- quantile(weights(brazil_data_weighted), probs=0.05)
weight.max <- quantile(weights(brazil_data_weighted), probs=0.95) 

brazil_data_trimmedwts <- trimWeights(brazil_data_weighted, 
  lower=weight.min, upper=weight.max, strict=TRUE)

#########################
# Brazil Data: Analysis #
#########################

### Correlation (Reported in Text)
jtools::svycor(~pk_ind + ksr_ind, design=brazil_data_trimmedwts, na.rm=T)

### Regression Equations
pk3 = pk_ind_scaled ~ edu + female + ses
ksr3 = ksr_ind_scaled ~ edu + female + ses

### Create a list of the variables for use below
brazil.vars <- c('ses', 'edu', 'female')

### Models
brazil_pk <- svyglm(update(pk3,~.), design=brazil_data_trimmedwts)
brazil_ksr <- svyglm(update(ksr3,~.), design=brazil_data_trimmedwts)

### 21 Permutations of PK Measure (Reported in Supplementary Appendix C)
five_item_dvs <- colnames(brazil_data)[match("pk_ind_5.1",names(brazil_data)):(match("pk_ind_5.21",names(brazil_data)))]

brazil_five_item_pk_out <- lapply(five_item_dvs, function(x) {
  svyglm(substitute(i ~ edu + female + ses,
         list(i = as.name(x))), design=brazil_data_trimmedwts)
})

##################
# Brazil: Output #
##################

# Summary Statistics
as.data.frame(select(brazil_data, pk_ind_scaled, ksr_ind_scaled, edu, ses, female)) %>%
stargazer::stargazer(omit.summary.stat = c("p25", "p75"), digits=2, 
  title="Brazil Data Summary Statistics (Unweighted)", out="./Plots/brazil_summarystats.html")

# Table with Brazil Analysis
stargazer::stargazer(brazil_pk, brazil_ksr,
  order=brazil.vars,
  covariate.labels=c("SES", "Education", "Female"), 
  dep.var.labels=c("Political Knowledge","Knowledge of Social Rights"),
  keep.stat = c("n", "rsq"),
  type="html", out="./Plots/brazil_weighted.html")

 # R-squared
poliscidata::fit.svyglm(brazil_pk)
poliscidata::fit.svyglm(brazil_ksr)

 # Test Differences in Coefficients
sims.diff.test(brazil_pk, brazil_ksr, "ses")
sims.diff.test(brazil_pk, brazil_ksr, "edu")
sims.diff.test(brazil_pk, brazil_ksr, "female")

# Permutations of Five-Item PK Variables Separated into Two Tables
stargazer::stargazer(brazil_five_item_pk_out[1:11], 
  order=brazil.vars,
  covariate.labels=c("SES", "Education", "Female"), 
  dep.var.labels=c("Political Knowledge"),
  column.labels = paste("(",1:11,")", sep=""),
  model.numbers = FALSE,
  keep.stat = c("n", "rsq"),
  type="html", out="./Plots/brazil_five_item_pk_permutations1.html")

stargazer::stargazer(brazil_five_item_pk_out[12:21], 
 order=brazil.vars,
 covariate.labels=c("SES", "Education", "Female"), 
 dep.var.labels=c("Political Knowledge"),
 column.labels = paste("(",12:21,")", sep=""),
 model.numbers = FALSE,
 keep.stat = c("n", "rsq"),
 type="html", out="./Plots/brazil_five_item_pk_permutations2.html")

########################
########################
## ARGENTINA ANALYSIS ##
########################
########################

############################
# Argentina Data: Recoding #
############################

arg_data <- haven::read_dta("Argentina_PKKSR_replication.dta") 

####### PREDICTOR VARIABLES #######

# Age
# Name of the variable: age

# Sex
# Name of variable: female 
# Values: 1=Yes, 0=No

# Education 
# Name of the variable: hhh_edu 
# Values: 1=Sin estudios, 2=Primaria incompleta, 3=Primaria completa, 4=Secundaria incompleta, 5=Secundaria completa, 6=Terciario incompleto, 7=Terciario completo, 
# 8=Universitario incompleto, 9=Universitario completo, 10=Postgrado

# SES
# Name of variable: nse
# Values = ABC1, C2, C3, D1, D2 - E

# Number of children under 18 years old 
# 40. Para finalizar, ¿Cuántos niños menores de 18 años en total viven en su hogar en este momento? 
# ID: v85

###############################
##### RECODE PREDICTORS #######
###############################

# Create a new variable age^2  (Age squared) from age and add it to the dataset #
age2 <- NA
age2 <- arg_data$age^2
arg_data$age2 <- age2

# Recode education (edu):

arg_data$edu <- NA
arg_data$edu <- as.numeric(car::recode(arg_data$hhh_edu,
                                       "1=0;
                                        2=0;
                                        3=1;
                                        4=2;
                                        5=3;
                                        6=4;
                                        7=5;
                                        8=4;
                                        9=5;
                                       10=5;
                                    else=NA"))

#Values:
# 0= Without formal education or incomplete primary
# 1= Complete primary
# 2= Incomplete secondary
# 3= Complete secondary
# 4= Incomplete tertiary or university
# 5= Complete tertiary or university

# Create an alternative measure of education with categories 4 and 5 collapsed (edu_alt):
arg_data$edu_alt <- NA
arg_data$edu_alt <- as.numeric(car::recode(arg_data$hhh_edu,
                                       "1=0;
                                        2=0;
                                        3=1;
                                        4=2;
                                        5=3;
                                        6=4;
                                        7=4;
                                        8=4;
                                        9=4;
                                       10=4;
                                    else=NA"))

#Values:
# 0= Without formal education or incomplete primary
# 1= Complete primary
# 2= Incomplete secondary
# 3= Complete secondary
# 4= Tertiary or university (complete or incomplete)

# Recode socioeconomic status SES (ses):
# Reverse values so that lower values denote more lived poverty.

arg_data$ses <- NA
arg_data$ses <- as.numeric(car::recode(arg_data$nse,
                                       "'ABC1'= 4;
                                       'C2' = 3;
                                       'C3' = 2;
                                       'D1' = 1;
                                       'D2 - E'= 0;
                                       else=NA"))

# Recode number of children under 18 years old (v85):

arg_data$child_under18 <-NA
arg_data$child_under18 <- as.numeric(car::recode(arg_data$v85,
                                                 "2=0;
                                                  3=1;
                                                  4=2;
                                                  5=3;
                                              else=NA"))

# Values: 
# 0= No children under 18
# 1= 1 child under 18
# 2= 2 children under 18
# 3= 3 or more children under 18

# Create a new variable for number of children under 18 years old (v85) as a dichotomous variable:

arg_data$child_under18_alt <-NA
arg_data$child_under18_alt <- as.numeric(car::recode(arg_data$v85,
                                                 "2=0;
                                                  3=1;
                                                  4=1;
                                                  5=1;
                                              else=NA"))

# Values: 
# 0= No children under 18
# 1= 1 or more children under 18

#########################
### OUTCOME VARIABLES ###
#########################

### Political Knowledge (PK) ###

#21. ¿Recuerda cuántas provincias tiene la República Argentina, contando la Ciudad de Buenos Aires?# 
#ID: v44
#22. ¿Cuántos años dura el mandato presidencial en Argentina? #
#ID: v45
#23. ¿Cuál es la institución encargada de determinar la constitucionalidad de las leyes?#
#ID: v46
#24. Actualmente, ¿cuál es el bloque mayoritario en la Cámara de Diputados?# 
#ID: v47
#25. ¿Quién es el actual Ministro de Economía? #
#ID: v48

### Knowledge of Social Rights (KSR) ### 

#26. El Estado Argentino garantiza la enseñanza primaria pública y gratuita.# 
#ID: v49
#27. De acuerdo al Calendario Nacional de Vacunación, ¿quiénes tienen derecho a recibir vacunas gratuitas? #
#ID: v50
#28. ¿Quién es el responsable de realizar el pago mensual de la Asignación Universal por Hijo (AUH)?#
#ID: v51
#29. De acuerdo a la reglamentación vigente, ¿es cierto que los beneficiarios de la Asignación
#Universal por Hijo (AUH) tienen que presentar una acreditación anual de escolarización y
#control de salud de los niños?# 
#ID: v52
#30. De acuerdo a la reglamentación, ¿cuántos miembros de un mismo grupo familiar podían
#cobrar el Ingreso Familiar de Emergencia (IFE)?#
#ID: v53

### POLITICAL KNOWLEDGE (PK) ###
# Create new variables that select the correct answers from the questionnaire (TRUE) #

arg_data %>%
  mutate(
    pk_correct_provinces = v44 == 4, # Correct answer = 24 provinces
    pk_correct_president = v45 == 1, # Correct answer = 4 years
    pk_correct_laws = v46 == 2, # Correct answer = Supreme Court
    pk_correct_deputies = v47 == 2, # Correct answer = Frente de Todos
    pk_correct_economy = v48 == 3 # Correct answer = Martin Guzman
  ) -> arg_data

arg_data$pk1 <-arg_data$pk_correct_provinces
arg_data$pk2 <-arg_data$pk_correct_president
arg_data$pk3 <-arg_data$pk_correct_laws
arg_data$pk4 <-arg_data$pk_correct_deputies
arg_data$pk5 <-arg_data$pk_correct_economy

### KNOWLEDGE OF SOCIAL RIGHTS ###

# What: ksr1, ksr2
# Who: ksr3
# How: ksr4, ksr5

# Create new variables that select the correct answers from the questionnaire (TRUE) #

arg_data %>%
  mutate(
    ksr_correct_edu = v49 == 1, # Correct answer = True
    ksr_correct_vaccines = v50 == 2, # Correct answer = Everyone 
    ksr_correct_AUH = v51 == 4, # Correct answer = ANSES
    ksr_correct_AUH_cond = v52 == 1, # Correct answer = True
    ksr_correct_IFE = v53 == 1 # Correct answer = One person
  ) -> arg_data

arg_data$ksr1 <-arg_data$ksr_correct_edu
arg_data$ksr2 <-arg_data$ksr_correct_vaccines
arg_data$ksr3 <-arg_data$ksr_correct_AUH
arg_data$ksr4 <-arg_data$ksr_correct_AUH_cond
arg_data$ksr5 <-arg_data$ksr_correct_IFE

#Drop the variables from the index that we don't use to simplify the visualization
arg_data = subset(arg_data, select = -c(pk_correct_provinces,
                                  pk_correct_president,
                                  pk_correct_laws,
                                  pk_correct_deputies,
                                  pk_correct_economy,
                                  ksr_correct_edu,
                                  ksr_correct_vaccines,
                                  ksr_correct_AUH,
                                  ksr_correct_AUH_cond,
                                  ksr_correct_IFE) )

# Create a subset that excludes the observations full of NAs for the variables of the PK and KSR Index.
# We want to keep the observations in which the respondent answer at least one question in both the PK Index the KSR Index.

argentina.na <- NA
argentina.na <- filter_at(arg_data,vars(starts_with("pk")),any_vars(!is.na(.)))
argentina.na <- filter_at(argentina.na,vars(starts_with("ksr")),any_vars(!is.na(.)))

# Create PK and KSR Indices (Scaled)

argentina.na %>%  mutate(pk_ind = 
  select(., pk1, pk2, pk3, pk4, pk5) %>% rowSums(na.rm=TRUE),
 ksr_ind = select(., ksr1, ksr2, ksr3, ksr4, ksr5) %>% rowSums(na.rm=TRUE),
 ksr_what = select(., ksr1, ksr2) %>% rowSums(na.rm=TRUE),
 ksr_how = select(., ksr4, ksr5) %>% rowSums(na.rm=TRUE),
 ksr_who = select(., ksr3) %>% rowSums(na.rm=TRUE)) -> 
 argentina.na

argentina.na %>% mutate(pk_ind_scaled = pk_ind/5,
 ksr_ind_scaled = ksr_ind/5,
 ksr_what_scaled = ksr_what/2,
 ksr_how_scaled = ksr_how/2) -> argentina.na


################################
### Argentina Data: Analysis ###
################################

### Regressions
pk1 = pk_ind_scaled ~ edu + female + age + age2 + ses + child_under18 
ksr1 = ksr_ind_scaled ~ edu + female + age + age2 + ses + child_under18

#PK (scaled index)
argentina_pk <- lm(update(pk1,~.), data= argentina.na)

#KSR (scaled index)
argentina_ksr <- lm(update(ksr1,~.), data = argentina.na)

##########################
# Argentina Data: Output #
##########################

arg.vars <- c('ses', 'edu', 'female', 'age', 'age2', 'child_under18')

# Summary Statistics 
as.data.frame(select(argentina.na, pk_ind_scaled, ksr_ind_scaled, edu, 
                     female, age, age2,ses, child_under18)) %>%
  stargazer::stargazer(omit.summary.stat = c("p25", "p75"), digits=2, 
                       title="Argentina Data Summary Statistics - PK Index (Unweighted)")

# Table with Argentina Analysis
stargazer::stargazer(argentina_pk, argentina_ksr,
  order=arg.vars,
  covariate.labels=c("SES", "Education", "Female", "Age", "Age Squared", "Minor Children"), 
  dep.var.labels=c ("Political Knowledge","Knowledge of Social Rights"),
  keep.stat = c("n", "rsq"),
  type="html", out="./Plots/arg_pkksr.html")

 # Test Differnces in Coefficients
sims.diff.test(argentina_pk, argentina_ksr, "ses")
sims.diff.test(argentina_pk, argentina_ksr, "edu")
sims.diff.test(argentina_pk, argentina_ksr, "female")
sims.diff.test(argentina_pk, argentina_ksr, "child_under18")

############################
############################
## PAKISTAN DATA ANALYSIS ##
############################
############################

pakistan_data <- readstata13::read.dta13('Pakistan_cleaned_data_December2021.dta')

### Predictor Variables

 # Recode education (dichotomous variable)
 # Note: coding Madrassa as NA (missing)
pakistan_data$educ_dich <- NA
pakistan_data$educ_dich <- as.numeric(car::recode(pakistan_data$education,
                                                 "'No education' = 0;
                                       'Primary school (grades 1-5)' = 1;
                                       'Middle school (grades 6-8)' = 1;
                                       'Matric pass (9th and 10th grade)' = 1;
                                       'Intermediate pass (11th and 12th grade)' = 1;
                                       'Undergraduate (Bsc/Bcom)' = 1;
                                       'Masters/ PhD' = 1;
                                        else=NA")) - 1

#Values: 1= Education (Includes Primary School, Middle school, Matric pass, Intermediate pass, Undergraduate level, 
# and  Masters/ PhD), 0= No education

#Female
 #Name of the variable: gender
 #Values: 1= female; 0=male

#Age: Age of the respondent 

#Age squared
# Create a new variable age^2  (Age squared) from age and add it to the dataset #
age2 <- NA
age2 <- pakistan_data$age^2
pakistan_data$age2 <- age2

#Minor children
#Name of the variable: hh_size_children
#Note that only  6% report No children under 18!
#Values= 0-18 children under 18

#Recode variable to three categories

pakistan_data$minor_children <- NA
pakistan_data$minor_children <- as.numeric(car::recode(pakistan_data$hh_size_children,
                                           "0=0;
                                            1=1;
                                            2=1;
                                            3=2;
                                            4=2;
                                            5=2;
                                            6=2;
                                            7=2;
                                            8=2;
                                            9=2;
                                            10=2;
                                            11=2;
                                            12=2;
                                            13=2;
                                            14=2;
                                            15=2;
                                            16=2;
                                            17=2;
                                            18=2;
                                            else=NA"))

# Values: 
# 0= No children under 18
# 1= 1 or 2 children under 18
# 2= 3 or more children under 18


### Outcome Variables

# Renaming
pakistan_data$pk1 <- pakistan_data$H1_correct
pakistan_data$pk2 <- pakistan_data$H2_correct
pakistan_data$pk3 <- pakistan_data$H3_correct
pakistan_data$pk4 <- pakistan_data$H4_correct
pakistan_data$pk5 <- pakistan_data$H6_correct
pakistan_data$pk6 <- pakistan_data$H7_correct
pakistan_data$pk7 <- pakistan_data$H8_correct
pakistan_data$ksr1 <- pakistan_data$H5_correct
pakistan_data$ksr2 <- pakistan_data$H9_correct
pakistan_data$ksr3 <- pakistan_data$H11_correct

# Create a subset that excludes the observations full of NAs for the variables of the PK and KSR Index
 # We want to keep the observations in which the respondent answer at least one question in both the PK Index the KSR Index
pakistan.na <- NA
pakistan.na <- filter_at(pakistan_data,vars(starts_with("pk")),any_vars(!is.na(.)))
pakistan.na <- filter_at(pakistan.na,vars(starts_with("ksr")),any_vars(!is.na(.)))

# Create PK and KSR Indices (Scaled)
 # H5_correct (ksr1) is the what question in this data
 # H9_correct (ksr2) and H11_correct (ksr3) are the two who questions

pakistan.na %>%  mutate(pk_ind = 
  select(., pk1, pk2, pk3, pk4, pk5, pk6, pk7) %>% rowSums(na.rm=TRUE),
 ksr_ind = select(., ksr1, ksr2, ksr3) %>% rowSums(na.rm=TRUE),
 ksr_what = select(., ksr1) %>% rowSums(na.rm=TRUE),
 ksr_who = select(., ksr2, ksr3) %>% rowSums(na.rm=TRUE)) -> 
 pakistan.na

pakistan.na %>% mutate(pk_ind_scaled = pk_ind/7,
 ksr_ind_scaled = ksr_ind/3,
 ksr_who_scaled = ksr_who/2) -> pakistan.na

# Create 35 permutations of three-item PK indices

temp1 <- gtools::combinations(7,3, c('pk1', 'pk2', 'pk3', 
 'pk4', 'pk5', 'pk6', 'pk7'))
 
for(i in 1:nrow(temp1)){
  pk_ind_3. <- rowSums(pakistan.na[,temp1[i,]], na.rm=TRUE)/3   # calculating sum of a five chosen PK Qs for each respondent
  pakistan.na[, ncol(pakistan.na)+1] <- pk_ind_3.  # creating a new variable at the end of the last column
  colnames(pakistan.na)[ncol(pakistan.na)] <- paste0("pk_ind_3.",i) # labeling the new column
}

###########################
# Pakistan Data: Analysis #
###########################

### Correlation
cor(pakistan.na$pk_ind_scaled, pakistan.na$ksr_ind_scaled)

pk1 = pk_ind_scaled ~ gender + age + age2 + educ_dich + minor_children 
ksr1 = ksr_ind_scaled ~ gender + age + age2 + educ_dich + minor_children 

 # Create a list of the variables for use below
pakistan.vars <- c('educ_dich', 'gender', 'age', 'age2', 'minor_childen')
pakistan.vars.bisp <- c('educ_dich', 'gender', 'age', 'age2', 'minor_childen', 'bisp')

pakistan_pk <- lm(update(pk1,~.), data=pakistan.na)
pakistan_ksr <- lm(update(ksr1,~.), data=pakistan.na)

pakistan_pk_bisp <- lm(pk_ind_scaled ~ gender + age + age2 + educ_dich + 
 minor_children + BISP_ben, data=pakistan.na)
pakistan_ksr_bisp <- lm(ksr_ind_scaled ~ gender + age + age2 + educ_dich + 
 minor_children + BISP_ben, data=pakistan.na)

  # 35 Permutations of PK Measure

three_item_dvs <- colnames(pakistan.na)[match("pk_ind_3.1",names(pakistan.na)):(match("pk_ind_3.35",names(pakistan.na)))]

pakistan_three_item_pk_out <- lapply(three_item_dvs, function(x) {
  lm(substitute(i ~ gender + age + age2 + educ_dich + minor_children,
         list(i = as.name(x))), data=pakistan.na)
})


# KSR Sub-Indices

pak_ksr_what <- lm(ksr_what ~gender + age + age2 + educ_dich + minor_children,
 data=pakistan.na)
pak_ksr_who <- lm(ksr_who_scaled ~gender + age + age2 + educ_dich + minor_children,
 data=pakistan.na)


#########################
# Pakistan Data: Output #
#########################

# Summary Statistics
as.data.frame(select(pakistan.na, pk_ind_scaled, ksr_ind_scaled, educ_dich, 
  gender, age, age2, minor_children)) %>%
stargazer::stargazer(omit.summary.stat = c("p25", "p75"), digits=2, 
  title="Pakistan Data Summary Statistics (Unweighted)", out="./Plots/pakistan_summarystats.html")

# Table with Pakistan Analysis
stargazer::stargazer(pakistan_pk, pakistan_ksr,
  order=pakistan.vars,
  covariate.labels=c("Education", "Female", "Age", "Age Squared", "Children"), 
  dep.var.labels=c("Political Knowledge","Knowledge of Social Rights"),
  keep.stat = c("n", "rsq"),
  type="html", out="./Plots/pakistan_results.html")

 # Test Differnces in Coefficients
sims.diff.test(pakistan_pk, pakistan_ksr, "educ_dich")
sims.diff.test(pakistan_pk, pakistan_ksr, "gender")
sims.diff.test(pakistan_pk, pakistan_ksr, "minor_children")

# 35 Permutations of Three-Item PK Variables Separated into Three Tables
stargazer::stargazer(pakistan_three_item_pk_out[1:12], 
  order=pakistan.vars,
  covariate.labels=c("Education", "Female", "Age", "Age Squared", "Children"), 
  dep.var.labels=c("Political Knowledge"),
  column.labels = paste("(",1:12,")", sep=""),
  model.numbers = FALSE,
  keep.stat = c("n", "rsq"),
  type="html", out="./Plots/pakistan_three_item_pk_permutations1.html")

stargazer::stargazer(pakistan_three_item_pk_out[13:24], 
  order=pakistan.vars,
  covariate.labels=c("Education", "Female", "Age", "Age Squared", "Children"), 
  dep.var.labels=c("Political Knowledge"),
  column.labels = paste("(",13:24,")", sep=""),
  model.numbers = FALSE,
  keep.stat = c("n", "rsq"),
  type="html", out="./Plots/pakistan_three_item_pk_permutations2.html")

stargazer::stargazer(pakistan_three_item_pk_out[25:35], 
  order=pakistan.vars,
  covariate.labels=c("Education", "Female", "Age", "Age Squared", "Children"), 
  dep.var.labels=c("Political Knowledge"),
  column.labels = paste("(",25:35,")", sep=""),
  model.numbers = FALSE,
  keep.stat = c("n", "rsq"),
  type="html", out="./Plots/pakistan_three_item_pk_permutations3.html")

#####################
#####################
## MALAWI ANALYSIS ##
#####################
#####################

# Not doing the permutations-based analysis in Malawi because the main
 # results are not in line with H2

malawi_data_raw <- read.csv("Malawi_PKKSR_Replication.csv")

# Identify Observations Where the R Answered None of the PK Qs 
  # There are three
malawi_data_raw %>% mutate(
  no_pk_answers = ifelse(district_num==999 & justices=="ref" & pres_term=="ref" & 
 finance_min=="ref" & vice_pres=="ref" & party_NatAss=="ref" & party_Pres=="ref" &
 zomba_mayor=="ref" & zomba_councilors=="ref", 1, 0)
) -> malawi_data

 # Identify Observations Where the R Answered None of the KSR Qs
malawi_data %>% mutate(
  no_ksr_answers = ifelse(free_health=="ref" & school_fees=="ref" & 
  gov_sctp=="ref" & sctp_child=="ref" & nat_id=="ref", 1, 
  0)) -> malawi_data

 # Get Rid of These Observations -- There Were Five
malawi_data %>% filter(no_pk_answers==0 & no_ksr_answers==0) -> malawi_data
  
# Covariates
malawi_data %>% mutate(
  agesq = age*age,
  female = gender=="female",
  edu = case_when(
    education=="no" ~ 1,
    education=="none" ~ 2,
    education=="prim_i" ~ 3,
    education=="prim_c" ~ 4,
    education=="sec_i" ~ 5,
    education=="sec_c" ~ 6,
    education=="tert_i" ~ 7,
    education=="tert_c" ~ 8,
    education=="uni_i" ~ 9,
    education=="uni_c" ~ 10,
    education=="post_i" ~ 11,
    education=="post_c" ~ 12),
  log_income = log(income_month + 1)
) -> malawi_data

malawi_data %>% mutate(ses = 0) %>% 
  mutate(ses = ifelse(ses_2=="yes", ses + 1, ses)) %>%
  mutate(ses = ifelse(ses_3=="yes", ses + 1, ses)) %>%
  mutate(ses = ifelse(ses_4=="yes", ses + 1, ses)) %>%
  mutate(ses = ifelse(ses_5=="yes", ses + 1, ses)) %>%
  mutate(ses = ifelse(ses_6=="yes", ses + 1, ses)) %>%
  mutate(ses = ifelse(ses_7=="yes", ses + 1, ses)) %>%
  mutate(ses = ifelse(ses_8=="cement", ses + 1, ses)) %>%
  mutate(ses = ifelse(ses_2=="NA" & ses_3=="NA" & ses_4=="NA" & 
   ses_5=="NA" & ses_6=="NA" & ses_7=="NA" & ses_8=="ref", NA, ses)) -> 
  malawi_data

 # Code PK Questions
malawi_data %>% mutate(
  pk_districts = district_num==28,
  pk_justices = justices=="9",
  pk_presterm = pres_term=="5",
  pk_finmin = finance_min=="mwanam",
  pk_vp = vice_pres=="chimuli",
  pk_natlassembly = party_NatAss=="dpp",
  pk_presparty = party_Pres=="dpp",
  pk_mayor = zomba_mayor=="bula",
  pk_councilors = zomba_councilors=="10"
) -> malawi_data

 # Coding KSR Questions
malawi_data %>% mutate(
  ksr_freehealth = free_health=="yes",
  ksr_freeschool = school_fees=="yes",
  ksr_sctp = gov_sctp=="dc",
  ksr_sctpcondition = sctp_child=="no",
  ksr_natlid = nat_id=="nrb") -> malawi_data

 # Create Scaled PK and KSR Indices
malawi_data %>% mutate(
  pk_index_scaled = (pk_districts + pk_justices + pk_presterm + 
   pk_finmin + pk_vp + pk_natlassembly + pk_presparty + pk_mayor +
   pk_councilors)/9,
  ksr_index_scaled = (ksr_freehealth + ksr_freeschool + ksr_sctp +
   ksr_sctpcondition + ksr_natlid)/5,
  ksr_what_scaled = (ksr_freehealth + ksr_freeschool)/2,
  ksr_who_scaled = (ksr_sctp + ksr_natlid)/2,
  ksr_nosctp = (ksr_freehealth + ksr_freeschool + ksr_natlid)/3,
  ksr_sctponly = (ksr_sctp + ksr_sctpcondition)/2
) -> malawi_data

#########################
# Malawi Data: Analysis #
#########################

malawi_pk <- lm(pk_index_scaled ~ log_income + edu + female + age + agesq,
 data=malawi_data)
malawi_ksr <- lm(ksr_index_scaled ~ log_income + edu + female + age + agesq,
 data=malawi_data)

malawi_ksr_what  <- lm(ksr_what_scaled ~ log_income + edu + female + age + agesq,
 data=malawi_data)
malawi_ksr_who  <- lm(ksr_who_scaled ~ log_income + edu + female + age + agesq,
 data=malawi_data)
malawi_ksr_how  <- lm(ksr_sctpcondition ~ log_income + edu + female + age + agesq,
 data=malawi_data)

malawi_ksr_nosctp <- lm(ksr_nosctp ~ log_income + edu + female + age + agesq,
 data=malawi_data)
malawi_ksr_sctponly <- lm(ksr_sctponly ~ log_income + edu + female + age + agesq,
 data=malawi_data)

#######################
# Malawi Data: Output #
#######################

# Malawi PK / KSR Analysis
stargazer::stargazer(malawi_pk, malawi_ksr,
  covariate.labels=c("Income", "Education", "Female", "Age", "Age Squared"), 
  dep.var.labels=c("Political Knowledge","Knowledge of Social Rights"),
  keep.stat = c("n", "rsq"),
  type="html", out="./Plots/malawi_results.html")

 # Test Differences in Coefficients
sims.diff.test(malawi_pk, malawi_ksr, "log_income")
sims.diff.test(malawi_pk, malawi_ksr, "edu")
sims.diff.test(malawi_pk, malawi_ksr, "femaleTRUE")


############################
############################
## AFROBAROMETER ANALYSIS ##
############################
############################

###########################
# Afrobarometer: Recoding #
###########################

ab <- foreign::read.spss('Afrobarometer_merged_r3_data.sav',to.data.frame=T)

### OUTCOME VARIABLES

# For the PK index, we will use the following questions from the survey:
#   1. Can you tell me the name of: Your Member of Parliament/National Assembly Representative?
#     ID: q43a2
#   2. Can you tell me the name of: The Deputy President/Vice President?
#     ID: q43c2
#   3. Which political party has the most seats in parliament/the national assembly?
#     ID: q44a2
#   4. How many times someone can legally be elected President/Prime Minister?
#     ID: q44b2
#   5. Whose responsibility it is to determine whether or not a law is constitutional?
#     ID: q44c2

# For the KSR index, we use:
#   1. Can you tell me whether the [Ghanaian/Kenyan/etc.] government has a policy to 
#      provide: Free primary education, that is, parents do not have to pay school fees?
#     ID: q69a
#   2. Can you tell me whether the [Ghanaian/Kenyan/etc.] government has a policy to 
#      provide: Free health care at public clinics, that is, no fees for visits or 
#      medicine?
#      ID: q69b

# Recode values for above questions so correct responses = 1, else = 0.
 # Questions have two different codes for right answers, so recode in two steps.
Qs1 <- c('q43a2','q43b2','q43c2')
Qs2 <- c('q44a2','q44b2','q44c2','q69a','q69b')
for(i in Qs1){
  ab[,i] <- as.numeric(car::recode(ab[,i],
   "'Correct name'=1;'Missing data'=NA;NA=NA;else=0"))-1
}
for(i in Qs2){
  ab[,i] <- as.numeric(car::recode(ab[,i],
   "'Correct answer'=1;'Missing data'=NA;NA=NA;else=0"))-1
}

# Rename these variables for easier management:

# PK Index 
ab$pk1 <- ab$q43a2  
ab$pk3 <- ab$q43c2
ab$pk4 <- ab$q44a2
ab$pk5 <- ab$q44b2
ab$pk6 <- ab$q44c2

# KSR Index
ab$ksr1 <- ab$q69a
ab$ksr2 <- ab$q69b

# Go from 25,397 to 24,324 on first and then to 24,321 on second
ab.na <- NA
ab.na <- filter_at(ab,vars(starts_with("pk")),any_vars(!is.na(.)))
ab.na <- filter_at(ab.na,vars(starts_with("ksr")),any_vars(!is.na(.)))

 # Then sum the relevant variables for each index
ab.na %>% mutate(pk_ind = select(., pk1, pk3, pk4, pk5, pk6) %>%
  rowSums(na.rm=TRUE), 
 ksr_ind = select(., ksr1, ksr2) %>% rowSums(na.rm=TRUE)) -> ab.na

ab.na %>% mutate(pk_index_scaled = pk_ind/5,
 ksr_index_scaled = ksr_ind/2) -> ab.na

 # Create a series of two-question PK scales for robustness checks

temp1 <- gtools::combinations(5,2, c('pk1','pk3', 
 'pk4', 'pk5', 'pk6'))
 
for(i in 1:nrow(temp1)){
  pk_ind_2. <- rowSums(ab.na[,temp1[i,]], na.rm=TRUE)/2   # calculating sum of a five chosen PK Qs for each respondent
  ab.na[, ncol(ab.na)+1] <- pk_ind_2.  # creating a new variable at the end of the last column
  colnames(ab.na)[ncol(ab.na)] <- paste0("pk_ind_2.",i) # labeling the new column
}

### PREDICTOR VARIABLES

#   1. Education
#   ID: q90
#   2. Income (proxied with Lived Poverty Index)
#     a. How often have you or your family gone without enough food?
#       ID: q8a
#       Values: 0=never, ..., 3=always (code -1,9, and 998 as NA)
#     b. "" clean water?
#       ID: q8b
#       Values: same
#     c. "" medicine or treatment?
#       ID: q8c
#       Values: same
#     d. "" fuel to cook food?
#       ID: q8d
#       Value: same
#     e. "" cash income?
#       ID: q8e
#       Value: same
#   3. Age
#     ID: q1
#   4. Sex
#     ID: q101
#   5. Children at Home
#     ID: q8f -- allows for "no children answer"

# Recode education variable (edu)
ab.na$edu <- as.numeric(car::recode(ab.na$q90,
                      "'No formal schooling'=0;
                      'Informal schooling only'=0;
                      'Some primary schooling'=0;
                      'Primary school completed'=1;
                      'Some secondary school/high school'=1;
                      'Secondary school completed/high school'=2;
                      'Post-secondary qualifications, not university'=3;
                      'Some university'=3;
                      'University completed'=3;
                      'Post-graduate'=3;
                      else=NA"))

# Create an SES status variable based on the quantile of a reversed LPI scale 
ab.na <- ab.na %>%
  mutate(food=car::recode(q8a,"'Never'=0;
                          'Just once or twice'=1;
                          'Several times'=2;
                          'Many times'=3;
                          'Always'=4;
                          else=NA"),
         water=car::recode(q8b,"'Never'=0;
                          'Just once or twice'=1;
                          'Several times'=2;
                          'Many times'=3;
                          'Always'=4;
                          else=NA"),
         care=car::recode(q8c,"'Never'=0;
                          'Just once or twice'=1;
                          'Several times'=2;
                          'Many times'=3;
                          'Always'=4;
                          else=NA"),
         fuel=car::recode(q8d,"'Never'=0;
                          'Just once or twice'=1;
                          'Several times'=2;
                          'Many times'=3;
                          'Always'=4;
                          else=NA"),
         cash=car::recode(q8e,"'Never'=0;
                          'Just once or twice'=1;
                          'Several times'=2;
                          'Many times'=3;
                          'Always'=4;
                          else=NA")) %>%
  mutate(LPI=5-((as.numeric(food)+   # Reverse values so that lower values
                as.numeric(water)+   # denote more lived poverty. 
                as.numeric(care)+    
                as.numeric(fuel)+
                as.numeric(cash))/5))

ab.na$ses <- NA
for(i in unique(ab.na$country)){
  quants <- with(ab.na[ab.na$country==i,],
                quantile(LPI,c(.2,.4,.6,.8),na.rm=T))
  ab.na[ab.na$country==i,]$ses[ab.na[ab.na$country==i,]$LPI<quants[1]] <- 0 
  ab.na[ab.na$country==i,]$ses[ab.na[ab.na$country==i,]$LPI>=quants[1]] <- 1
  ab.na[ab.na$country==i,]$ses[ab.na[ab.na$country==i,]$LPI>=quants[2]] <- 2
  ab.na[ab.na$country==i,]$ses[ab.na[ab.na$country==i,]$LPI>=quants[3]] <- 3
  ab.na[ab.na$country==i,]$ses[ab.na[ab.na$country==i,]$LPI>=quants[4]] <- 4
}

# Recode age variable (age)
ab.na$age <- NA
ab.na$age[ab.na$q1!="Missing"&
          ab.na$q1!="Refused"&
          ab.na$q1!="Don't know"] <-
  as.numeric(as.character(ab.na$q1[ab.na$q1!="Missing"&
                       ab.na$q1!="Refused"&
                       ab.na$q1!="Don't know"]))

# Recode gender variable -- takes categorical values of Male and Female
ab.na <- ab.na %>% 
  mutate(female=recode(q101, "Male"=0,
    "Female"=1))

# Create 0/1 children variable
ab.na <- ab.na %>%
  mutate(children=recode(q8f,"No children"=0,
   "Never"=1, "Just once or twice"=1, "Several times"=1,
   "Many times"=1, "Always"=1))

### COUNTRY-LEVEL VARIABLE

# Afrobarometer Round 3 was conducted in 2005; we want 2005 Polity scores for
 # all of the surveyed countries -- polity2 variable

 # Zimbabwe is not in the data anymore -- it is in the raw ab data
  # and has a Polity score of -4

polity <- data.frame(country=unique(ab.na$country), 
  polity=c(6, 8, 10, 8, 8, 8, 7, 6, 7, 5, 6, 4, 8, 9, -1, -1, 5))

ab.na <- inner_join(ab.na, polity)

### WEIGHTING

# Put dataset into different format to account for survey weights
 # Note: withinwt (within-country weight) used
abw <- svydesign(ids = ~1, data = ab.na, weights = ab.na$withinwt) 

###########################
# Afrobarometer: Analysis #
###########################

### Correlations
jtools::svycor(~pk_index_scaled + ksr_index_scaled, design=abw, na.rm=T)

### Specifications
spec.pk <- pk_index_scaled ~ ses + edu + female + age + I(age^2) + children
spec.ksr <- ksr_index_scaled ~ ses + edu + female + age + + I(age^2) + children

 # Create a list of the variables for use below
afro.vars <- c('ses', 'edu', 'female', 'age', 'I(age^2)', 'children')

### Pooled Models
pk_pooled_raw <- svyglm(update(spec.pk,~. + country),
                             design=abw)
ksr_pooled_raw <- svyglm(update(spec.ksr,~. + country),
                             design=abw)

pk_pooled = broom::tidy(pk_pooled_raw) %>%
  mutate(model="Full Data (FE)",outcome="Political Knowledge")
ksr_pooled = broom::tidy(ksr_pooled_raw) %>%
  mutate(model="Full Data (FE)",outcome='Knowledge of Social Rights')

### By country
cntry_out <- list()
for(i in unique(abw$variables$country[abw$variables$country!="Zimbabwe"])){
  m1 <- try(broom::tidy(svyglm(spec.pk, design=subset(abw, country==i))) %>%
    mutate(model=i,outcome='Political Knowledge'))
  m2 <- try(broom::tidy(svyglm(spec.ksr, design=subset(abw, country==i))) %>%
    mutate(model=i,outcome='Knowledge of Social Rights'))
  cntry_out[[i]] <- rbind(m1,m2)
}

### Bind output together
pk.ksr.results_df <- rbind(do.call(rbind,cntry_out),
                  pk_pooled, ksr_pooled)  %>% 
  arrange(term,estimate) %>%   # Arrange values by size of estimate
  mutate(ord=row_number())

### Subset to Polity > 6
pk_pooled_democ <- svyglm(update(spec.pk,~. + country), subset=polity>6,
                             design=abw)
ksr_pooled_democ <- svyglm(update(spec.ksr,~. + country), subset=polity>6,
                             design=abw)


### Set of 10 Two-Item PK Measures

 # Identify the list of dvs
two_item_dvs <- colnames(ab.na)[match("pk_ind_2.1",names(ab.na)):(match("pk_ind_2.1",names(ab.na))+9)]

ab_two_item_pk_out <- lapply(two_item_dvs, function(x) {
  svyglm(substitute(i ~ ses + edu + female + age + I(age^2) + children + country,
         list(i = as.name(x))), design=abw)
})


##########################
# Afrombarometer: Output #
##########################

# Summary Statistics
select(ab.na, pk_index_scaled, ksr_index_scaled, edu, ses, age, female, children) %>%
stargazer::stargazer(omit.summary.stat = c("p25", "p75"), digits=2, out="./Plots/ab_summarystats.html")

# Table with Pooled Analysis
stargazer::stargazer(pk_pooled_raw, ksr_pooled_raw, omit="country",
  order=afro.vars,
  covariate.labels=c("SES", "Education", "Female", "Age", "Age Squared", "Children at Home"), 
  dep.var.labels=c("Political Knowledge","Knowledge of Social Rights"),
  keep.stat = c("n", "rsq"),
  type="html", out="./Plots/ab_pooled.html")

 # Can't get r-squared directly from the svyglm object
poliscidata::fit.svyglm(pk_pooled_raw)
poliscidata::fit.svyglm(ksr_pooled_raw)

sims.diff.test(pk_pooled_raw, ksr_pooled_raw, "ses")
sims.diff.test(pk_pooled_raw, ksr_pooled_raw, "edu")
sims.diff.test(pk_pooled_raw, ksr_pooled_raw, "female")
sims.diff.test(pk_pooled_raw, ksr_pooled_raw, "children")

# Country-Specific Plots

 # PK
pk.ksr.results_df %>% filter(outcome=='Political Knowledge') %>% 
  filter(term %in% afro.vars,
         model!="Full Data (FE)") %>%
  mutate(term = factor(term, levels=afro.vars)) %>%
  ggplot(aes(term,estimate)) +
  geom_point() + geom_hline(yintercept=0)   +
  theme_bw() + 
  geom_errorbar(aes(ymax = estimate+1.96*std.error, 
                    ymin = estimate-1.96*std.error),width=.25) +
  facet_wrap(~model) +
  ggtitle("Marginal Effect of Covariates on Political Knowledge") + 
  scale_x_discrete(breaks=afro.vars,
                   labels=c("SES","Education","Female","Age","Age Squared","Children")) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust=.5,face="italic")) +
  labs(subtitle="Results Shown by Country",
       y="", x="") +
  coord_flip() 
ggsave('./Plots/ab_pk_bycountry.pdf',units='in',width=9,height=8)

  # KSR
pk.ksr.results_df %>% filter(outcome=="Knowledge of Social Rights") %>% 
  filter(term %in% afro.vars,
         model!="Full Data (FE)") %>%
  mutate(term = factor(term, levels=afro.vars)) %>%
  ggplot(aes(term,estimate)) +
  geom_point() + geom_hline(yintercept=0)   +
  theme_bw() + 
  geom_errorbar(aes(ymax = estimate+1.96*std.error, 
                    ymin = estimate-1.96*std.error),width=.25) +
  facet_wrap(~model) +
  ggtitle("Marginal Effect of Covariates on Knowledge of Social Rights") + 
  scale_x_discrete(breaks=afro.vars,
                   labels=c("SES","Education","Female","Age","Age Squared","Children")) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust=.5,face="italic")) +
  labs(subtitle="Results Shown by Country",
       y="", x="") +
  coord_flip() 
ggsave('./plots/ab_ksr_bycountry.pdf',units='in',width=9,height=8)

# Permutations of Two-Item PK Variables
stargazer::stargazer(ab_two_item_pk_out, omit="country",
  order=afro.vars,
  covariate.labels=c("SES", "Education", "Female", "Age", "Age Squared", "Children at Home"), 
  dep.var.labels=c("Political Knowledge"),
  keep.stat = c("n", "rsq"),
  type="html", out="./Plots/ab_two_item_pk_permutations.html")

# End of File