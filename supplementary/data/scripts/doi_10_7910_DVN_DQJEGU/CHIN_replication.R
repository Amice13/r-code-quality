###############
# Replication Code for: Understanding the Timing of Chinese Border Incursions into India
###############

argv <- getwd()
setwd(argv)

# Load libraries 
if (!require("pacman")) install.packages("pacman", repos = "http://cran.us.r-project.org")
pacman::p_load(stargazer,tidyverse,tidyr,knitr,kableExtra)


library(stargazer)
library(tidyverse)
library(tidyr)
library(knitr)
library(kableExtra)


## read in data
incursion_small <- read.csv("./data/Incursion_dataset.csv",stringsAsFactors = F)


##############
# Models 
##############

########
# Lead 1 month
########

incursion_small$t <- 1:nrow(incursion_small) 

incur_lead_1 <- incursion_small %>% 
  select(-c(date,incursion_lag2,incursion_lag3,
            incursion_lag4,incursion_lag5,incursion_lag6)) %>%
  na.omit()

# temporal 
l1_temp <-  incursion_lag1~summer+In_Const+In_Deploy+
  Ch_Const+Ch_Deploy+IN_PK_med_hos+IN_US_med_coop+
  Exec_Meet+IN_CH_num_hos+ch_jp_terr+
  tenure_months_China+tenure_months_India+
  anticipation_India+
  UCDP_IN+protest_India+
  protest_China+CH_uncertain+CH_confidence+t

temp_1 <- glm(family = binomial("logit"), l1_temp,
              data=incur_lead_1)


########
# Lead 2 months
########

incur_lead_1 <- incursion_small %>% 
  select(-c(date,incursion_lag1,incursion_lag3,
            incursion_lag4,incursion_lag5,incursion_lag6)) %>%
  na.omit()

l1_temp <-  incursion_lag2~summer+In_Const+In_Deploy+
  Ch_Const+Ch_Deploy+IN_PK_med_hos+IN_US_med_coop+
  Exec_Meet+IN_CH_num_hos+ch_jp_terr+
  tenure_months_China+tenure_months_India+
  anticipation_India+
  UCDP_IN+protest_India+
  protest_China+CH_uncertain+CH_confidence+t

temp_2 <- glm(family = binomial("logit"), l1_temp,
              data=incur_lead_1)



########
# Lead 3 months
########

incur_lead_1 <- incursion_small %>% 
  select(-c(date,incursion_lag1,incursion_lag2,
            incursion_lag4,incursion_lag5,incursion_lag6)) %>%
  na.omit()

l1_temp <-  incursion_lag3~summer+In_Const+In_Deploy+
  Ch_Const+Ch_Deploy+IN_PK_med_hos+IN_US_med_coop+
  Exec_Meet+IN_CH_num_hos+ch_jp_terr+
  tenure_months_China+tenure_months_India+
  anticipation_India+
  UCDP_IN+protest_India+
  protest_China+CH_uncertain+CH_confidence+t

temp_3 <- glm(family = binomial("logit"), l1_temp,
              data=incur_lead_1)


########
# Lead 4 months
########

incur_lead_1 <- incursion_small %>% 
  select(-c(date,incursion_lag1,incursion_lag2,
            incursion_lag3,incursion_lag5,incursion_lag6)) %>%
  na.omit()

l1_temp <-  incursion_lag4~summer+In_Const+In_Deploy+
  Ch_Const+Ch_Deploy+IN_PK_med_hos+IN_US_med_coop+
  Exec_Meet+IN_CH_num_hos+ch_jp_terr+
  tenure_months_China+tenure_months_India+
  anticipation_India+
  UCDP_IN+protest_India+
  protest_China+CH_uncertain+CH_confidence+t

temp_4 <- glm(family = binomial("logit"), l1_temp,
              data=incur_lead_1)


########
# Lead 5 months
########

incur_lead_1 <- incursion_small %>% 
  select(-c(date,incursion_lag1,incursion_lag2,
            incursion_lag3,incursion_lag4,incursion_lag6)) %>%
  na.omit()

l1_temp <-  incursion_lag5~summer+In_Const+In_Deploy+
  Ch_Const+Ch_Deploy+IN_PK_med_hos+IN_US_med_coop+
  Exec_Meet+IN_CH_num_hos+ch_jp_terr+
  tenure_months_China+tenure_months_India+
  anticipation_India+
  UCDP_IN+protest_India+
  protest_China+CH_uncertain+CH_confidence+t

temp_5 <- glm(family = binomial("logit"), l1_temp,
              data=incur_lead_1)


########
# Lead 6 months
########

incur_lead_1 <- incursion_small %>% 
  select(-c(date,incursion_lag1,incursion_lag2,
            incursion_lag3,incursion_lag4,incursion_lag5)) %>%
  na.omit()

l1_temp <-  incursion_lag6~summer+In_Const+In_Deploy+
  Ch_Const+Ch_Deploy+IN_PK_med_hos+IN_US_med_coop+
  Exec_Meet+IN_CH_num_hos+ch_jp_terr+
  tenure_months_China+tenure_months_India+
  anticipation_India+
  UCDP_IN+protest_India+
  protest_China+CH_uncertain+CH_confidence+t

temp_6 <- glm(family = binomial("logit"), l1_temp,
              data=incur_lead_1)




########
# Output Tables 
######## 
stargazer(temp_1,temp_2,temp_3,
          temp_4,temp_5,temp_6,
          omit = c( "Constant"),
          covariate.labels = c("Summer",
                               "IN Construction",
                               "IN Deployment",
                               "CH Construction",
                               "CH Deployment",
                               "IN-PK Hostility",
                               "IN-US Cooperation",
                               "Exec Meet",
                               "IN-CH Hostility",
                               "CH Territorial",
                               "CH Tenure",
                               "IN Tenure",
                               "IN Election",
                               "IN Internal Conflict",
                               "IN Protest",
                               "CH Protest",
                               "Econ Uncertainty",
                               "Econ Confidence"
                               
          ),
          #title = "Other Political Aspects",
          #notes = "Village FE is included", 
          omit.stat = c("ser", "f","aic","ll"),
          column.labels  = c("t+1","t+2","t+3","t+4","t+5","t+6"),
          dep.var.labels.include = F,
          out.header = F, header = F, 
          colnames = F,
          model.numbers = F,
          model.names =F,
          digits=2,
          no.space = TRUE,dep.var.caption = "DV: Incursion (lead by t+n months)",
          out = "./output/GLM_Full_Regression_Table_temporal.tex"
)



stargazer(temp_1,temp_2,temp_3,
          temp_4,temp_5,temp_6,
          omit = c( "Constant"),
          covariate.labels = c("Summer",
                               "IN Construction",
                               "IN Deployment",
                               "CH Construction",
                               "CH Deployment",
                               "IN-PK Hostility",
                               "IN-US Cooperation",
                               "Exec Meet",
                               "IN-CH Hostility",
                               "CH Territorial",
                               "CH Tenure",
                               "IN Tenure",
                               "IN Election",
                               "IN Internal Conflict",
                               "IN Protest",
                               "CH Protest",
                               "Econ Uncertainty",
                               "Econ Confidence"
                               
          ),
          #title = "Other Political Aspects",
          #notes = "Village FE is included", 
          omit.stat = c("ser", "f","aic","ll"),
          column.labels  = c("t+1","t+2","t+3","t+4","t+5","t+6"),
          report=('vc*p'),
          dep.var.labels.include = F,
          out.header = F, header = F, 
          colnames = F,
          model.numbers = F,
          model.names =F,
          digits=2,
          no.space = TRUE,dep.var.caption = "DV: Incursion (lead by t+n months)",
          out ="./output/GLM_Full_Regression_Table_temporal_pval.tex"
)


#######################
## Descriptive Statistics 
#######################
sum_stats <- incursion_small %>% select(-c(incursion_lag2:incursion_lag6))%>%
  na.omit %>%
  select("Incursion"=incursion_lag1,
         "IN Election"=anticipation_India,
         "Econ Confidence"=CH_confidence,
         "CH Construction"=Ch_Const,
         "CH Deployment"=Ch_Deploy,
         "CH Territorial"=ch_jp_terr,
         "Econ Uncertainty"=CH_uncertain,
         "Exec Meet"=Exec_Meet,
         "IN-CH Hostility"=IN_CH_num_hos,
         "IN Construction"=In_Const,
         "IN Deployment"=In_Deploy,
         "IN-PK Hostility"=IN_PK_med_hos,
         "IN-US Cooperation"=IN_US_med_coop,
         "CH Protest"=protest_China,
         "IN Protest"=protest_India,
         "Summer"=summer,
         "CH Tenure"=tenure_months_China,
         "IN Tenure"=tenure_months_India,
         "IN Internal Conflict"= UCDP_IN
  ) %>%
  
  # Find the mean, st. dev., min, and max for each variable 
  summarise_all(funs(mean, sd, min, max)) %>%
  
  # Move summary stats to columns
  gather(key, value, everything()) %>% 
  separate(key, into = c("variable", "stat"), sep = "_") %>%
  spread(stat, value) %>%
  
  # Set order of summary statistics 
  select(variable, mean, sd, min, max) %>%
  
  # Round all numeric variables to one decimal point
  mutate_each(funs(round(., 1)), -variable) %>%
  kable(format = 'latex', booktabs = TRUE) %>%
  cat(., file = "./output/Descriptive.tex")
