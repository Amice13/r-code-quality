# ----------------------------------------------------------------------
#  Project: Diversity in Healthcare Provision Reduces Jewish Patients' Prejudice towards Arabs
#  Last updated: "Wed Feb 17 08:17:30 2021"
#  Input data: "main_data_april19.xlsx"
#  Purpose: Replication Materials -- SI
#  Outputs: All tables and Figures from SI
#  Machine: Chagai's Macbook Pro
# ----------------------------------------------------------------------

#############################################################################
# Load relevant paackges      
#############################################################################
library("readxl")
library("tidyverse")
library("stringr")
library("xlsx")
library("estimatr")
library("gridExtra")
library("ggpubr")
library("texreg")
library("stargazer")
library("xtable")
library("lfe")
library("MASS")
library("multiwayvcov")
library("lmtest")
library("xlsx")
library("panelView")

#############################################################################
# Read in main data
#############################################################################

# read data
data <- read_csv("pnas_diversity_main.csv")
nrow(data)

# subset to Jewish patients for main analyses
data_j <- data %>% 
 filter(.,
        jewish == 1)
nrow(data_j)


#############################################################################
# Generate Descriptive Statistics Table -- S2
#############################################################################


disc_table <- dplyr::select(data_j,
                     c(age, gender_num, religiosity_num, right_left,
                       edu_num, ten_day, arab,
                       therm_arab_num,
                       sd_arab_num, trust_arabs_num,
                       peace_arabs_num,
                       z_index, arab_past,
                       cycle_violence,
                       previous_contact_arabs_num))
stargazer(as.data.frame(disc_table),
          covariate.labels = c("Age",  "Male", "Religiosity", "Ideology", "Education",
                               "10 Day Survey",
                               "Arab Doc", "Thermometer Arab",
                               "Soc Dis Arabs", "Trust Arabs",
                               "Peace Arabs", "Outcome Index", "Arab Doc (Past)",
                               "Cycle of Violence", "Previous Contact Arabs"),
          title = "Descriptive Statistics - Jewish Patients",
          label = "tab:dsc",
          style = "qje")



#############################################################################
# Generate figure describing diagnoses in clinics -- Figure S1
#############################################################################

ggplot(data_j, aes(x= fct_infreq(sickness)))+
 geom_bar(color = "dodgerblue2", fill = "dodgerblue3", size = .2)+
 labs(x = "", 
      y = "Count") +
 theme(text = element_text(size = 10, family = "Times"),
       panel.grid.major = element_blank(), 
       axis.text.x = element_text(size = 10, angle=90),
       plot.caption = element_text(size = 10, family = "Times",hjust = -.02),
       panel.grid.minor = element_blank(),
       panel.background = element_blank(), 
       axis.line = element_line(colour = "black"))



#####################################################################################
# Generate figure describing share of Jewish/Arab doctors by clinic-day -- Figure S2
#####################################################################################
# Load list of doctors by day
share_docs <- read_csv("list_docs.csv")
panelView(panelview_dum ~ share_arab, 
          data = share_docs, 
          index = c("clinic_code", "visit_date.x"), 
          # axis.lab="unit",
          #  axis.adjust = T,
          axis.lab.gap = c(10,0),
          xlab = "Date", ylab = "Clinic", 
          background = "white",
          main = "Share of Arab Doctors by Clinic-Day", gridOff = F)

#############################################################################
# Compare sample with IDI represenative sample -- Table S3
#############################################################################

# Read in IDI 2018 data
idi_survey <- read.xlsx("idi.xlsx", sheetIndex = 1, 
                        header=T)


# Clean relevant responses for idi data
idi_data <-  idi_survey %>% 
 filter(.,
        !grepl("מוסל", jewish)) %>% # Filter jewish respondents
 mutate(.,
        gender_num = case_when( # create gender indicator
         gender == "גבר" ~ 1,
         gender == "אישה" ~ 0
        ),
        religiosity = case_when(
         relig == "חילוני" ~ "Secular",
         relig == "מסורתי דתי" ~ "Traditional",
         relig == "מסורתי לא דתי"~ "Traditional",
         relig == "חרדי" ~ "Ultra-Orthodox",
         relig == "מסרב/ת להשיב" ~ "Other",
         relig == "אחר ( לרשום - לברר אם רשום כחסר דת/נוצרי)" ~ "Other",
         grepl("דתי לאומי", relig) ~ "Religious" # create religiosity indicator
        ),
        religiosity_num = case_when(
         religiosity == "Secular" ~ 0,
         religiosity == "Traditional" ~ 1,
         religiosity == "Religious" ~ 2,
         religiosity == "Ultra-Orthodox" ~ 3 # turn religiosity into numeric
        ),
        right_left = case_when(
         right_left == "ימין" ~ 1,
         right_left == "ימין מתון" ~2,
         right_left == "מרכז" ~ 3,
         right_left == "שמאל מתון" ~ 4,
         right_left == "שמאל" ~ 5 # create ideology indicator
        ),
        edu = case_when(
         edu == "אקדמית חלקית, ללא תואר" ~ "High-School",
         edu == "יסודית או פחות" ~ "Elementry",
         edu == "ישיבה על תיכונית" ~ "Yeshiva",
         grepl("סמינר למורים", edu) ~ "BA",
         grepl("אקדמי מלא", edu) ~ "BA",
         grepl("בגרות", edu)  ~  "High-School"# create education indicator
        ),
        edu_num = case_when(
         edu == "Elementry" ~ 0,
         edu == "High-School" ~ 1,
         edu == "Yeshiva" ~ 2,
         edu == "BA" ~ 3 # turn education into numeric
        ),
        age = case_when(
         Q7 == "18-24" ~ 1,
         Q7 == "25-34" ~ 2,
         Q7 == "35-44" ~ 3,
         Q7 == "45-54" ~ 4,
         Q7 == "55-64" ~ 5,
         grepl("מעלה", Q7) ~ 6), # create age indicator
        idi_dummy = 1) %>% 
 dplyr::select(.,
               c("gender_num", "religiosity_num", 
                 "right_left", "edu_num",
                 "age", "idi_dummy")) # Select relevant variables for comparison with clinic sample




# Modify education in clinic data [to allow comparison] and create an indicator for comparison
data_j_comp <- data_j %>% 
 mutate(.,
        idi_dummy = 0,
        edu_num = case_when(
         edu_num < 4 ~ edu_num,
         edu_num == 4 ~ 3
        ),
        age = case_when(
         age %in% 18:24 ~ 1,
         age %in% 25:34 ~ 2,
         age %in% 35:44 ~ 3,
         age %in% 45:54 ~ 4,
         age %in% 55:64 ~ 5,
         age  > 64 ~ 6
        )) %>% 
 dplyr::select(.,
               c("gender_num", "religiosity_num", 
                 "right_left", "edu_num", "idi_dummy", "age"))


# Combine IDI and clinic data sets together
idi_hosptial <- rbind(idi_data, data_j_comp) 


# Rename Variables and Values for table
idi_hosptial <- idi_hosptial %>% 
 rename(.,
        Age = age,
        Gender = gender_num,
        Religiosity = religiosity_num,
        Ideology = right_left,
        Education = edu_num,
        IDI = idi_dummy)

# Create seperate dataframes for each group, which are used to report N in Table S3 
clin <- idi_hosptial %>% 
 filter(.,
        IDI == 0)

id <- idi_hosptial %>% 
 filter(.,
        IDI == 1)

# Run t.test comparing both samples by 5 demographic variables

# Age
age <- t.test(Age ~ IDI, data = idi_hosptial) %>% 
 tidy(.) %>% mutate(., Variable = "Age") %>% 
 mutate(.,
        N_Clinic = sum(!is.na(clin$Age)),
        N_IDI = sum(!is.na(id$Age)))

# Gender
gend <- t.test(Gender ~ IDI, data = idi_hosptial) %>% 
 tidy(.) %>% mutate(., Variable = "Male") %>% 
 mutate(.,
        N_Clinic = sum(!is.na(clin$Gender)),
        N_IDI = sum(!is.na(id$Gender)))

# Religion
relig <- t.test(Religiosity ~ IDI, data = idi_hosptial) %>% 
 tidy(.) %>% mutate(., Variable = "Religiosity") %>% 
 mutate(.,
        N_Clinic = sum(!is.na(clin$Religiosity)),
        N_IDI = sum(!is.na(id$Religiosity)))

# Education
ed <- t.test(Education ~ IDI, data = idi_hosptial) %>% 
 tidy(.) %>% mutate(., Variable = "Education")  %>% 
 mutate(.,
        N_Clinic = sum(!is.na(clin$Education)),
        N_IDI = sum(!is.na(id$Education)))

# Ideology
id <- t.test(Ideology ~ IDI, data = idi_hosptial) %>% 
 tidy(.) %>% mutate(., Variable = "Ideology") %>% 
 mutate(.,
        N_Clinic = sum(!is.na(clin$Ideology)),
        N_IDI = sum(!is.na(id$Ideology)))

tab <- rbind(age, gend, relig, ed, id) %>% 
 rename(.,
        IDI_Mean = estimate1,
        Clinic_Mean = estimate2,
        Low = conf.low,
        High = conf.high) %>% 
 dplyr::select(.,
               Variable, N_Clinic, N_IDI, Clinic_Mean, IDI_Mean, Low, High, p.value)

names(tab)<-c("Variable", "N Clinic", "N IDI", "Clinic Mean", "IDI Mean", 
              "Low", "High", "p")
xtable(tab,
       caption = 'Comparison of IDI 2018 and Clinic Survey Samples',
                       label = 'tab:comparison')



#############################################################################
# Selection into Survey -- Table S4
#############################################################################

# Read in data that includes all patients in clinics, and notes whether a patient opted into report measures of interest
all_patients <- read_csv("all_patients.csv")


### Regress participation over identity of doctor, as well as age and gender of all patients in clinics
# Logit specification
logit1 <- glm(participate ~ arab,
              data = all_patients,
              family = "binomial") 
summary(logit1)

logit2 <- glm(participate ~ arab + age + gender, 
              data = all_patients,
              family = "binomial") 
summary(logit2)

# OLS specification
fe1 <- felm(participate ~ arab | clinic_code + visit_date | 0 |
             clinic_code,
            data = all_patients,
            keepCX = TRUE)
summary(fe1)
fe2 <- felm(participate ~ arab + age + gender | 
             clinic_code + visit_date | 0 |
             clinic_code,
            data = all_patients,
            keepCX = TRUE) 
summary(fe2)


# Report models in Table
stargazer(fe1, fe2, logit1, logit2,
          style="qje",
          title="Selection into Survey by Treatment and Covariates",
          dep.var.labels = c("Participation in Survey", "Participation in Survey",
                             "Participation in Survey", "Participation in Survey"),
          covariate.labels = c("Arab Doctor", "Age", "Gender"),
          omit.stat = c("rsq", "f", "ser", "adj.rsq", "ll", "aic"),
          ci=FALSE,
          no.space = T,
          model.names = FALSE,
          column.sep.width = "-02pt",
          omit.table.layout = "n",
          star.cutoffs = NULL,
          font.size = "small",
          label = "tab:select",
          star.char = c("", "", ""),
          add.lines = list(
           c("Model",  "OLS", "OLS", "Logit", "Logit"),
           c("Date FEs", "Yes", "Yes", "No", "No"),
           c("Clinic FEs", "Yes", "Yes", "No", "No"),
           c("Cluster",  "Clinic", "Clinic", "No", "No")))




#############################################################################
# Model non-response to survey items -- Table S5
#############################################################################

# Regress a binary variable taking a value of 1 if a respondent did not answer a sepcific survey question
# over treament (and covariates)

# Social distance
sd_attrition_1 <- felm(sd_attrition ~ arab | clinic_code + visit_date |
                        0 |clinic_code,
                       data = data_j,
                       keepCX = TRUE) 
sd_attrition_2 <- felm(sd_attrition ~ arab + religiosity_num + gender_num + 
                        edu_num + age
                       | clinic_code + visit_date | 0 |clinic_code,
                       data = data_j,
                       keepCX = TRUE) 

# Peace
peace_attrition_1 <- felm(peace_attrition ~ arab | clinic_code + visit_date |
                           0 |clinic_code,
                          data = data_j,
                          keepCX = TRUE) 
peace_attrition_2 <- felm(peace_attrition ~ arab + religiosity_num + gender_num + 
                           edu_num + age
                          | clinic_code + visit_date | 0 |clinic_code,
                          data = data_j,
                          keepCX = TRUE) 

# Trust
trust_attrition_1 <- felm(trust_attrition ~ arab | clinic_code + visit_date |
                           0 |clinic_code,
                          data = data_j,
                          keepCX = TRUE) 
trust_attrition_2 <- felm(trust_attrition ~ arab + religiosity_num + gender_num + 
                           edu_num + age
                          | clinic_code + visit_date | 0 |clinic_code,
                          data = data_j,
                          keepCX = TRUE) 


# Therm
therm_attrition_1 <- felm(therm_attrition ~ arab | clinic_code + visit_date |
                           0 |clinic_code,
                          data = data_j,
                          keepCX = TRUE) 
therm_attrition_2 <- felm(therm_attrition ~ arab + religiosity_num + gender_num + 
                           edu_num + age
                          | clinic_code + visit_date | 0 |clinic_code,
                          data = data_j,
                          keepCX = TRUE) 



stargazer(sd_attrition_1, sd_attrition_2,
          peace_attrition_1, peace_attrition_2,
          therm_attrition_1, therm_attrition_2,
          trust_attrition_1, trust_attrition_2,  style="qje",
          title="Correlation of Missing Responses with Treatment and Covariates",
          dep.var.labels = c("Social Distance", "Peace", "Thermometer", "Trust"),
          covariate.labels = c("Arab Doctor", "Religiosity", "Gender", "Education", "Age"),
          omit.stat = c("rsq", "f", "ser", "adj.rsq"),
          ci=FALSE,
          no.space = T,
          omit.table.layout = "n",
          star.cutoffs = NULL,
          font.size = "small",
          label = "tab:attrition",
          star.char = c("", "", ""),
          add.lines = list(
           c("Date FEs", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
           c("Clinic FEs", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
           c("Cluster",  "Clinic", "Clinic", "Clinic", "Clinic", "Clinic", "Clinic", "Clinic", "Clinic")))


#############################################################################
# Estimate main models with an inverse probability weight -- Table S6
#############################################################################


##### Create weighted models

### Social Distance Model

# Create Social Distance Weight
fit_sd <- glm(sd_non_attrition ~ age*arab +gender_num*arab,
              family = binomial(link = "logit"),
              data = data_j)
p_sd <- fit_sd$fitted
weights_sd <- 1/p_sd

# Run Weighted Model
sd_weight <- felm(z_sd ~ arab| 
                   clinic_code  +  visit_date | 0 |clinic_code,
                  data = data_j,
                  weights = weights_sd,
                  exactDOF=TRUE,
                  keepCX = TRUE)




### Peace Model

# Create Peace Weight
fit_peace <- glm(peace_non_attrition ~ age*arab +gender_num*arab,
                 family = binomial(link = "logit"),
                 data = data_j)
p_peace <- fit_peace$fitted
weights_peace <- 1/ p_peace

# Run Weighted Model
peace_weight <- felm(z_peace ~ arab| 
                      clinic_code  +  visit_date | 0 |clinic_code,
                     data = data_j,
                     weights = weights_peace,
                     exactDOF=TRUE,
                     keepCX = TRUE)



### Therm Model
fit_therm <- glm(therm_non_attrition ~ age*arab +gender_num*arab,
                 family = binomial(link = "logit"),
                 data = data_j)
p_therm <- fit_therm$fitted
weights_therm <- 1/ p_therm

# Run Weighted Model
therm_weight <- felm(z_therm ~ arab| 
                      clinic_code  +  visit_date | 0 |clinic_code,
                     data = data_j,
                     weights = weights_therm,
                     exactDOF=TRUE,
                     keepCX = TRUE)
summary(therm_weight)



### Trust Model
fit_trust <- glm(trust_non_attrition ~ age*arab +gender_num*arab,
                 family = binomial(link = "logit"),
                 data = data_j)
p_trust <- fit_trust$fitted
weights_trust <- 1/ p_trust

trust_weight <- felm(z_trust ~ arab| 
                      clinic_code  +  visit_date | 0 |clinic_code,
                     data = data_j,
                     weights = weights_trust,
                     exactDOF=TRUE,
                     keepCX = TRUE)
summary(trust_weight)


### Index Model
fit_index <- glm(index_non_attrition ~ age*arab +gender_num*arab,
                 family = binomial(link = "logit"),
                 data = data_j)
p_index <- fit_index$fitted
weights_index <- 1/ p_index

index_weight <- felm(z_index ~ arab| 
                      clinic_code  +  visit_date | 0 |clinic_code,
                     data = data_j,
                     weights = weights_trust,
                     exactDOF=TRUE,
                     keepCX = TRUE)
summary(index_weight)



#### Create non-weighted models for comparison
### Social Distance Model



# Social Distance
sd <- felm(z_sd ~ arab| 
                   clinic_code  +  visit_date | 0 |clinic_code,
                  data = data_j,
                  exactDOF=TRUE,
                  keepCX = TRUE)




### Peace 
peace <- felm(z_peace ~ arab| 
                      clinic_code  +  visit_date | 0 |clinic_code,
                     data = data_j,
                     exactDOF=TRUE,
                     keepCX = TRUE)



### Therm 
therm <- felm(z_therm ~ arab| 
                      clinic_code  +  visit_date | 0 |clinic_code,
                     data = data_j,
                     exactDOF=TRUE,
                     keepCX = TRUE)

### Trust 
trust <- felm(z_trust ~ arab| 
                      clinic_code  +  visit_date | 0 |clinic_code,
                     data = data_j,
                     exactDOF=TRUE,
                     keepCX = TRUE)

### Index 

index <- felm(z_index ~ arab| 
                      clinic_code  +  visit_date | 0 |clinic_code,
                     data = data_j,
                     exactDOF=TRUE,
                     keepCX = TRUE)


stargazer(sd, sd_weight,
          peace, peace_weight, 
          therm, therm_weight, 
          trust, trust_weight,
          index, index_weight,
          style="qje",
          title="Effects of Arab Doctor on Prejudice with Inverse Probability Weighting",
          dep.var.labels = c("Social Distance", "Peace", "Thermometer", "Trust", "Index"),
          covariate.labels = c("Arab Doctor"),
          omit.stat = c("rsq", "f", "ser", "adj.rsq"),
          ci=FALSE,
          no.space = T,
          omit.table.layout = "n",
          star.cutoffs = NULL,
          font.size = "small",
          label = "tab:ipw",
          star.char = c("", "", ""),
          add.lines = list(
           c("Date FEs", "Yes", "Yes", "Yes", "Yes",
             "Yes", "Yes", "Yes", "Yes","Yes","Yes"),
           c("Clinic FEs", "Yes", "Yes", "Yes", "Yes",
             "Yes", "Yes", "Yes", "Yes","Yes", "Yes"),
           c("Cluster",  "Clinic", "Clinic", "Clinic", "Clinic",
             "Clinic", "Clinic", "Clinic", "Clinic","Clinic", "Clinic"),
           c("IPW", "No", "Yes","No", "Yes","No", "Yes","No", "Yes", "No", "Yes")))




#############################################################################
# Moderating effect of doctor quality -- Table S7
#############################################################################

# Run models which control for a doctor quality score, and interact quality with treatment

# Social distance
sd_rating <- felm(z_sd ~ arab + doc_rating| 
                   clinic_code  +  visit_date | 0 | clinic_code,
                  data = data_j,
                  exactDOF=TRUE,
                  keepCX = TRUE)  

sd_rating_hte <- felm(z_sd ~ arab*doc_rating| 
                       clinic_code  +  visit_date | 0 | clinic_code,
                      data = data_j,
                      exactDOF=TRUE,
                      keepCX = TRUE)  

# Peace
peace_rating <- felm(z_peace ~ arab + doc_rating| 
                      clinic_code  +  visit_date | 0 | clinic_code,
                     data = data_j,
                     exactDOF=TRUE,
                     keepCX = TRUE) 

peace_rating_hte <- felm(z_peace ~ arab*doc_rating| 
                          clinic_code  +  visit_date | 0 | clinic_code,
                         data = data_j,
                         exactDOF=TRUE,
                         keepCX = TRUE)  




# Therm
therm_rating <- felm(z_therm ~ arab + doc_rating| 
                      clinic_code  +  visit_date | 0 | clinic_code,
                     data = data_j,
                     exactDOF=TRUE,
                     keepCX = TRUE) 

therm_rating_hte <- felm(z_therm ~ arab*doc_rating| 
                          clinic_code  +  visit_date | 0 | clinic_code,
                         data = data_j,
                         exactDOF=TRUE,
                         keepCX = TRUE)  

# Trust
trust_rating <- felm(z_trust ~ arab + doc_rating| 
                      clinic_code  +  visit_date | 0 | clinic_code,
                     data = data_j,
                     exactDOF=TRUE,
                     keepCX = TRUE)  
trust_rating_hte <- felm(z_trust ~ arab*doc_rating| 
                          clinic_code  +  visit_date | 0 | clinic_code,
                         data = data_j,
                         exactDOF=TRUE,
                         keepCX = TRUE)  

# Index
index_rating <- felm(z_index ~ arab + doc_rating| 
                      clinic_code  +  visit_date | 0 | clinic_code,
                     data = data_j,
                     exactDOF=TRUE,
                     keepCX = TRUE)  
index_rating_hte <- felm(z_index ~ arab*doc_rating| 
                          clinic_code  +  visit_date | 0 | clinic_code,
                         data = data_j,
                         exactDOF=TRUE,
                         keepCX = TRUE)  




stargazer(sd_rating, sd_rating_hte,
          peace_rating, peace_rating_hte,
          therm_rating, therm_rating_hte,
          trust_rating, trust_rating_hte,
          index_rating, index_rating_hte,  style="qje",
          title="Moderating Effects of Doctor Quality",
          dep.var.labels = c("Social Distance", "Peace", "Thermometer", "Trust", "Index"),
          covariate.labels = c("Arab Doctor", "Doctor Rating", "Arab*Rating"),
          omit.stat = c("rsq", "f", "ser", "adj.rsq"),
          ci=FALSE,
          no.space = T,
          column.sep.width = "-02pt",
          omit.table.layout = "n",
          star.cutoffs = NULL,
          font.size = "small",
          label = "tab:quality",
          star.char = c("", "", ""),
          add.lines = list(
           c("Date FEs", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes","Yes", "Yes"),
           c("Clinic FEs", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes","Yes", "Yes"),
           c("Cluster",  "Clinic", "Clinic", "Clinic", "Clinic", "Clinic", "Clinic", "Clinic", "Clinic","Clinic", "Clinic")))






#############################################################################
# Moderating effect of patient age -- Table S8
#############################################################################

# Run models which interact treatment with age, and consider 18+ respondents


# Social distance
sd_age <- felm(z_sd ~ arab*age| 
                clinic_code  +  visit_date | 0 | clinic_code,
               data = data_j,
               exactDOF=TRUE,
               keepCX = TRUE)  
sd_age2 <- data_j %>% 
 filter(., age >18) %>% 
 felm(z_sd ~ arab| 
       clinic_code  +  visit_date | 0 | clinic_code,
      data = .,
      exactDOF=TRUE,
      keepCX = TRUE)  


# Peace
peace_age <- felm(z_peace ~ arab*age| 
                   clinic_code  +  visit_date | 0 | clinic_code,
                  data = data_j,
                  exactDOF=TRUE,
                  keepCX = TRUE)  
peace_age2 <- data_j %>% 
 filter(., age >18) %>% 
 felm(z_peace ~ arab| 
       clinic_code  +  visit_date | 0 | clinic_code,
      data = .,
      exactDOF=TRUE,
      keepCX = TRUE)  

# therm
therm_age <- felm(z_therm ~ arab*age| 
                   clinic_code  +  visit_date | 0 | clinic_code,
                  data = data_j,
                  exactDOF=TRUE,
                  keepCX = TRUE)  
therm_age2 <- data_j %>% 
 filter(., age >18) %>% 
 felm(z_therm ~ arab| 
       clinic_code  +  visit_date | 0 | clinic_code,
      data = .,
      exactDOF=TRUE,
      keepCX = TRUE)  

# trust
trust_age <- felm(z_trust ~ arab*age |
                   clinic_code  +  visit_date | 0 | clinic_code,
                  data = data_j,
                  exactDOF=TRUE,
                  keepCX = TRUE)  
trust_age2 <- data_j %>% 
 filter(., age >18) %>% 
 felm(z_trust ~ arab| 
       clinic_code  +  visit_date | 0 | clinic_code,
      data = .,
      exactDOF=TRUE,
      keepCX = TRUE)  


# index
index_age <- felm(z_index ~ arab*age| 
                   clinic_code  +  visit_date | 0 | clinic_code,
                  data = data_j,
                  exactDOF=TRUE,
                  keepCX = TRUE)  
index_age2 <- data_j %>% 
 filter(., age >18) %>% 
 felm(z_index ~ arab| 
       clinic_code  +  visit_date | 0 | clinic_code,
      data = .,
      exactDOF=TRUE,
      keepCX = TRUE)  


stargazer(sd_age2, sd_age,
          peace_age2, peace_age,
          therm_age2, therm_age,
          trust_age2, trust_age,
          index_age2, index_age, style="qje",
          title="Moderating Effects of Age",
          keep = c("arab", "age", "arab:age"),
          dep.var.labels = c("Social Distance", "Peace", "Thermometer", "Trust", "Index"),
          covariate.labels = c("Arab Doctor", "Age",
                               "Arab*Age"),
          omit.stat = c("rsq", "f", "ser", "adj.rsq"),
          ci=FALSE,
          no.space = T,
          column.sep.width = "-02pt",
          omit.table.layout = "n",
          star.cutoffs = NULL,
          font.size = "small",
          label = "tab:age",
          star.char = c("", "", ""),
          add.lines = list(
           c("Sample", "18+", "Full", "18+", "Full", "18+", "Full", "18+", "Full", "18+", "Full"),
           c("Demog. Controls", "No", "No", "No", "No", "No", "No", "No", "No","No", "No"),
           c("Date FEs", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes","Yes", "Yes"),
           c("Clinic FEs", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes","Yes", "Yes"),
           c("Cluster",  "Clinic", "Clinic", "Clinic", "Clinic", "Clinic", "Clinic", "Clinic", "Clinic","Clinic", "Clinic")))


#############################################################################
# Robustness of main results to alternative model specifications -- Figure S3
#############################################################################

#### Social distance

#Base model
sd <- lm_robust(z_sd ~ arab, 
                data = data_j,
                fixed_effects = clinic_code + visit_date,
                clusters = clinic_code, se_type = "CR0")
sd_plot <- tidy(sd, conf.int = T, robust = T) %>%  mutate(., term = "Social \nDistance",
                                                          Model = "Base")
#Clinic-date fixed effect
sd_cdfe_fe <- lm_robust(z_sd ~ arab, 
                        data = data_j,
                        fixed_effects = clinic_date,
                        clusters = clinic_code, se_type = "CR0")
sd_cdfe_plot <- tidy(sd_cdfe_fe, conf.int = T, robust = T) %>%  mutate(., term = "Social \nDistance",
                                                                       Model = "Clinic-Date FE")
# Covariate controls
sd_cov <- lm_robust(z_sd ~ arab +
                     gender_num + religiosity_num +
                     edu_num + age, 
                    data = data_j,
                    fixed_effects = clinic_code + visit_date,
                    clusters = clinic_code, se_type = "CR0")
sd_cov_plot <- tidy(sd_cov, conf.int = T, robust = T) %>% 
 filter(., term == "arab") %>%  mutate(., term = "Social \nDistance",
                                       Model = "Controls")

# Clinic + doctor cluster
sd_cov_doc <- felm(z_sd ~ arab + gender_num + religiosity_num +
                    edu_num + age| 
                    clinic_code  +  visit_date | 0 |clinic_code + doc_id,
                   data = data_j,
                   exactDOF=TRUE,
                   keepCX = TRUE)

sd_cov_doc_plot <- tidy(sd_cov_doc, conf.int = T, robust = T) %>% 
 filter(., term == "arab") %>%  mutate(., term = "Social \nDistance",
                                       Model = "Clinic-Doctor \nCluster")

# No clustering
sd_cov_no_se <- felm(z_sd ~ arab + gender_num + religiosity_num +
                      edu_num + age| 
                      clinic_code  +  visit_date | 0 |0,
                     data = data_j,
                     exactDOF=TRUE,
                     keepCX = TRUE)
sd_cov_no_se$N
sd_cov_no_se_plot <- tidy(sd_cov_no_se, conf.int = T) %>% 
 filter(., term == "arab") %>%  mutate(., term = "Social \nDistance",
                                       Model = "No \nCluster")


# Wild Cluster bootsrap
data_j$visit_date_fac <- as.factor(data_j$visit_date) # Adjust date dummy to process through LM as FE
sd_lm <- lm(z_sd ~ arab + gender_num + religiosity_num +
             edu_num + age + clinic_code  +  visit_date_fac, data = data_j)
sd_wc <- cluster.boot(sd_lm, data_j$clinic_code, R = 300, boot_type = "wild")
sd_wc_out <- coeftest(sd_lm , sd_wc) %>% tidy(., conf.int = T) %>% 
 filter(., term == "arab") %>%  mutate(., term = "Social \nDistance",
                                       Model = "Wild \nCluster \nBootsrap")


### Peace

# Base model
peace <- lm_robust(z_peace ~ arab, 
                   data = data_j,
                   fixed_effects = clinic_code + visit_date,
                   clusters = clinic_code, se_type = "CR0")

peace_plot <- tidy(peace, conf.int = T, robust = T) %>%  mutate(., term = "Peace",
                                                                Model = "Base")
# Clinic-Date fixed effect
peace_cdfe_fe <- lm_robust(z_peace ~ arab, 
                           data = data_j,
                           fixed_effects = clinic_date,
                           clusters = clinic_code, se_type = "CR0")

peace_cdfe_plot <- tidy(peace_cdfe_fe, conf.int = T, robust = T) %>%  mutate(., term = "Peace",
                                                                             Model = "Clinic-Date FE")
# Covariate controls
peace_cov <- lm_robust(z_peace ~ arab +
                        gender_num + religiosity_num +
                        edu_num + age, 
                       data = data_j,
                       fixed_effects = clinic_code + visit_date,
                       clusters = clinic_code, se_type = "CR0")

peace_cov_plot <- tidy(peace_cov, conf.int = T, robust = T) %>% 
 filter(., term == "arab") %>%  mutate(., term = "Peace",
                                       Model = "Controls")

# Clinic+doctor cluster
peace_cov_doc <- felm(z_peace ~ arab + gender_num + religiosity_num +
                       edu_num + age| 
                       clinic_code  +  visit_date | 0 | clinic_code + doc_id,
                      data = data_j,
                      exactDOF=TRUE,
                      keepCX = TRUE)

peace_cov_doc_plot <- tidy(peace_cov_doc, conf.int = T, robust = T) %>% 
 filter(., term == "arab") %>%  mutate(., term = "Peace",
                                       Model = "Clinic-Doctor \nCluster")

# No cluster
peace_cov_no_se <- felm(z_peace ~ arab + gender_num + religiosity_num +
                         edu_num + age| 
                         clinic_code  +  visit_date | 0 | 0,
                        data = data_j,
                        exactDOF=TRUE,
                        keepCX = TRUE)
peace_cov_no_se_plot <- tidy(peace_cov_no_se, conf.int = T) %>% 
 filter(., term == "arab") %>%  mutate(., term = "Peace",
                                       Model = "No \nCluster")

# Wild Cluster bootsrap
peace_lm <- lm(z_peace ~ arab + gender_num + religiosity_num +
                edu_num + age + clinic_code + visit_date_fac, data = data_j)
peace_wc <- cluster.boot(peace_lm, data_j$clinic_code, R = 300, boot_type = "wild")
peace_wc_out <- coeftest(peace_lm , peace_wc) %>% tidy(., conf.int = T) %>% 
 filter(., term == "arab") %>%  mutate(., term = "Peace",
                                       Model = "Wild \nCluster \nBootsrap")


### Therm

# Base model
therm <-  lm_robust(z_therm ~ arab, 
                    data = data_j,
                    fixed_effects = clinic_code + visit_date,
                    clusters = clinic_code, se_type = "CR0")
therm_plot <- tidy(therm, conf.int = T, robust = T) %>%  mutate(., term = "Feeling \nThermometer",
                                                                Model = "Base")
# Clinic-date fixed effect
therm_cdfe_fe <- lm_robust(z_therm ~ arab, 
                           data = data_j,
                           fixed_effects = clinic_date,
                           clusters = clinic_code, se_type = "CR0")
therm_cdfe_plot <- tidy(therm_cdfe_fe, conf.int = T, robust = T) %>%  mutate(., term = "Feeling \nThermometer",
                                                                             Model = "Clinic-Date FE")

# Covariate controls
therm_cov <- lm_robust(z_therm ~ arab +
                        gender_num + religiosity_num +
                        edu_num + age, 
                       data = data_j,
                       fixed_effects = clinic_code + visit_date,
                       clusters = clinic_code, se_type = "CR0")
therm_cov_plot <- tidy(therm_cov, conf.int = T, robust = T) %>% 
 filter(., term == "arab") %>%  mutate(., term = "Feeling \nThermometer",
                                       Model = "Controls")

# Clinic+doctor cluster
therm_cov_doc <- felm(z_therm ~ arab + gender_num + religiosity_num +
                       edu_num + age| 
                       clinic_code  +  visit_date | 0 | clinic_code + doc_id,
                      data = data_j,
                      exactDOF=TRUE,
                      keepCX = TRUE)
therm_cov_doc_plot <- tidy(therm_cov_doc, conf.int = T, robust = T) %>% 
 filter(., term == "arab") %>%  mutate(., term = "Feeling \nThermometer",
                                       Model = "Clinic-Doctor \nCluster")


# No cluster 
therm_cov_no_se <- felm(z_therm ~ arab + gender_num + religiosity_num +
                         edu_num + age| 
                         clinic_code  +  visit_date | 0 | 0,
                        data = data_j,
                        exactDOF=TRUE,
                        keepCX = TRUE) 

therm_cov_no_se_plot <- tidy(therm_cov_no_se, conf.int = T) %>% 
 filter(., term == "arab") %>%  mutate(., term = "Feeling \nThermometer",
                                       Model = "No \nCluster")

# Wild Cluster bootsrap
therm_lm <- lm(z_therm ~ arab + gender_num + religiosity_num +
                edu_num + age + clinic_code + visit_date_fac, data = data_j)
therm_wc <- cluster.boot(therm_lm, data_j$clinic_code, R = 300, boot_type = "wild")
therm_wc_out <- coeftest(therm_lm , therm_wc) %>% tidy(., conf.int = T) %>% 
 filter(., term == "arab") %>%  mutate(., term = "Feeling \nThermometer",
                                       Model = "Wild \nCluster \nBootsrap")

### Trust

# Base model
trust <- lm_robust(z_trust ~ arab, 
                   data = data_j,
                   fixed_effects = clinic_code + visit_date,
                   clusters = clinic_code, se_type = "CR0")
trust_plot <- tidy(trust, conf.int = T, robust = T)%>%  mutate(., term = "Trust",
                                                               Model = "Base")

# Clinic-day FE
trust_cdfe_fe <- lm_robust(z_trust ~ arab, 
                           data = data_j,
                           fixed_effects = clinic_date,
                           clusters = clinic_code, se_type = "CR0")
trust_cdfe_plot <- tidy(trust_cdfe_fe, conf.int = T, robust = T) %>%  mutate(., term = "Trust",
                                                                             Model = "Clinic-Date FE")


# Covariate controls
trust_cov <- lm_robust(z_trust ~ arab +
                        gender_num + religiosity_num +
                        edu_num + age, 
                       data = data_j,
                       fixed_effects = clinic_code + visit_date,
                       clusters = clinic_code, se_type = "CR0")
trust_cov_plot <- tidy(trust_cov, conf.int = T, robust = T) %>% 
 filter(., term == "arab") %>%  mutate(., term = "Trust",
                                       Model = "Controls")

# Clinic+doctor cluster
trust_cov_doc <- felm(z_trust ~ arab + gender_num + religiosity_num +
                       edu_num + age| 
                       clinic_code  +  visit_date | 0 | clinic_code + doc_id,
                      data = data_j,
                      exactDOF=TRUE,
                      keepCX = TRUE)
trust_cov_doc_plot <- tidy(trust_cov_doc, conf.int = T, robust = T) %>% 
 filter(., term == "arab") %>%  mutate(., term = "Trust",
                                       Model = "Clinic-Doctor \nCluster")

# No cluster
trust_cov_no_se <- felm(z_trust ~ arab + gender_num + religiosity_num +
                         edu_num + age| 
                         clinic_code  +  visit_date | 0 | 0,
                        data = data_j,
                        exactDOF=TRUE,
                        keepCX = TRUE)

trust_cov_no_se_plot <- tidy(trust_cov_no_se, conf.int = T) %>% 
 filter(., term == "arab") %>%  mutate(., term = "Trust",
                                       Model = "No \nCluster")

# Wild cluster bootstrap
trust_lm <- lm(z_trust ~ arab + gender_num + religiosity_num +
                edu_num + age + clinic_code + visit_date_fac, data = data_j)
trust_wc <- cluster.boot(trust_lm, data_j$clinic_code, R = 300, boot_type = "wild")
trust_wc_out <- coeftest(trust_lm , trust_wc) %>% tidy(., conf.int = T, robust = T) %>% 
 filter(., term == "arab") %>%  mutate(., term = "Trust",
                                       Model = "Wild \nCluster \nBootsrap")

### Index

# Base
index <- lm_robust(z_index ~ arab, 
                   data = data_j,
                   fixed_effects = clinic_code + visit_date,
                   clusters = clinic_code, se_type = "CR0")
index_plot <- tidy(index, conf.int = T, robust = T)%>%  mutate(., term = "Index",
                                                               Model = "Base")

# Clinic-date FE
index_cdfe_fe <- lm_robust(z_index ~ arab, 
                           data = data_j,
                           fixed_effects = clinic_date,
                           clusters = clinic_code, se_type = "CR0")
index_cdfe_plot <- tidy(index_cdfe_fe, conf.int = T, robust = T) %>%  mutate(., term = "Index",
                                                                             Model = "Clinic-Date FE")

# Covariate control
index_cov <- lm_robust(z_index ~ arab +
                        gender_num + religiosity_num +
                        edu_num + age, 
                       data = data_j,
                       fixed_effects = clinic_code + visit_date,
                       clusters = clinic_code, se_type = "CR0")
index_cov_plot <- tidy(index_cov, conf.int = T, robust = T) %>% 
 filter(., term == "arab") %>%  mutate(., term = "Index",
                                       Model = "Controls")

# Clinic+doctor clusters
index_cov_doc <- felm(z_index ~ arab + gender_num + religiosity_num +
                       edu_num + age| 
                       clinic_code  +  visit_date | 0 | clinic_code + doc_id,
                      data = data_j,
                      exactDOF=TRUE,
                      keepCX = TRUE)
index_cov_doc_plot <- tidy(index_cov_doc, conf.int = T, robust = T) %>% 
 filter(., term == "arab") %>%  mutate(., term = "Index",
                                       Model = "Clinic-Doctor \nCluster")


# No cluster
index_cov_no_se <- felm(z_index ~ arab + gender_num + religiosity_num +
                         edu_num + age| 
                         clinic_code  +  visit_date | 0 | 0,
                        data = data_j,
                        exactDOF=TRUE,
                        keepCX = TRUE)
index_cov_no_se$N
index_cov_no_se_plot <- tidy(index_cov_no_se, conf.int = T) %>% 
 filter(., term == "arab") %>%  mutate(., term = "Index",
                                       Model = "No \nCluster")

# Wild cluster bootstrap
index_lm <- lm(z_index ~ arab + gender_num + religiosity_num +
                edu_num + age + clinic_code + visit_date_fac, data = data_j)
index_wc <- cluster.boot(index_lm, data_j$clinic_code, R = 300, boot_type = "wild")
index_wc_out <- coeftest(index_lm , index_wc) %>% tidy(., conf.int = T, robust = T) %>% 
 filter(., term == "arab") %>%  mutate(., term = "Index",
                                       Model = "Wild \nCluster \nBootsrap")

# bind all models
main_coef2 <- bind_rows(sd_plot, sd_cdfe_plot, sd_cov_plot, sd_cov_doc_plot, sd_cov_no_se_plot, sd_wc_out,
                        peace_plot, peace_cdfe_plot, peace_cov_plot, peace_cov_doc_plot, peace_cov_no_se_plot, peace_wc_out,
                        therm_plot, therm_cdfe_plot, therm_cov_plot, therm_cov_doc_plot, therm_cov_no_se_plot, therm_wc_out,
                        trust_plot, trust_cdfe_plot, trust_cov_plot, trust_cov_doc_plot, trust_cov_no_se_plot, trust_wc_out,
                        index_plot, index_cdfe_plot, index_cov_plot, index_cov_doc_plot, index_cov_no_se_plot, index_wc_out)


### Generate coefficient plot 
# order outcomes and models in plot
main_coef2$Model <- factor(main_coef2$Model, 
                           levels = c("Base", "Clinic-Date FE", "Controls",
                                      "Wild \nCluster \nBootsrap", "Clinic-Doctor \nCluster",
                                      "No \nCluster"))
positions_main <- c("Social \nDistance", "Peace", "Feeling \nThermometer", "Trust", "Index")
ggplot(main_coef2, aes(x = term, y = estimate)) +
 geom_hline(yintercept = 0, color = "gray50", linetype = 2, size = 0.2) +
 geom_pointrange(aes(ymin = conf.low, ymax = conf.high, color = Model, shape = Model),
                 position = position_dodge(width = 0.6),
                 fill = "white") +
 scale_y_continuous(limits = c(-0.4, 0.5)) + 
 #coord_flip()+
 scale_color_manual(values = c("firebrick2", "dodgerblue2", "gray50", "darkgoldenrod2", "chocolate","seagreen4")) +
 scale_fill_manual(values = c("firebrick3", "dodgerblue3", "gray50", "darkgoldenrod3", "chocolate","seagreen4")) +
 scale_x_discrete(limits = positions_main)+ 
 labs(x = "",
      y = "Effect Size") +
 theme(text = element_text(size = 12, family = "Times"),
       panel.grid.major = element_blank(), 
       legend.key.size = unit(2, 'lines'),
       legend.key = element_rect(colour = "transparent", fill = "white"),
       axis.text.x = element_text(size = 12),
       plot.caption = element_text(size = 12, family = "Times",hjust = -.02),
       panel.grid.minor = element_blank(),
       panel.background = element_blank(), 
       axis.line = element_line(colour = "black"))



#############################################################################
# Moderating effect of survey timing -- Table S9
#############################################################################


# Social distance
sd_time <- felm(z_sd ~ arab*ten_day| 
                 clinic_code  +  visit_date | 0 | clinic_code,
                data = data_j,
                exactDOF=TRUE,
                keepCX = TRUE)  

sd_cov_time <- felm(z_sd ~ arab*ten_day + gender_num + religiosity_num +
                     edu_num + age| 
                     clinic_code  +  visit_date | 0 | clinic_code,
                    data = data_j,
                    exactDOF=TRUE,
                    keepCX = TRUE)  

# Peace
peace_time <- felm(z_peace ~ arab*ten_day| 
                    clinic_code  +  visit_date | 0 | clinic_code,
                   data = data_j,
                   exactDOF=TRUE,
                   keepCX = TRUE)  


peace_cov_time <- felm(z_peace ~ arab*ten_day+ gender_num + religiosity_num +
                        edu_num + age| 
                        clinic_code  +  visit_date | 0 | clinic_code,
                       data = data_j,
                       exactDOF=TRUE,
                       keepCX = TRUE)  



# therm
therm_time <- felm(z_therm ~ arab*ten_day| 
                    clinic_code  +  visit_date | 0 | clinic_code,
                   data = data_j,
                   exactDOF=TRUE,
                   keepCX = TRUE)  


therm_cov_time <- felm(z_therm ~ arab*ten_day + gender_num + religiosity_num +
                        edu_num + age| 
                        clinic_code  +  visit_date | 0 | clinic_code,
                       data = data_j,
                       exactDOF=TRUE,
                       keepCX = TRUE)  

# trust
trust_time <- felm(z_trust ~ arab*ten_day| 
                    clinic_code  +  visit_date | 0 | clinic_code,
                   data = data_j,
                   exactDOF=TRUE,
                   keepCX = TRUE)  



trust_cov_time <- felm(z_trust ~ arab*ten_day + gender_num + religiosity_num +
                        edu_num + age| 
                        clinic_code  +  visit_date | 0 | clinic_code,
                       data = data_j,
                       exactDOF=TRUE,
                       keepCX = TRUE)  


# index
index_time <- felm(z_index ~ arab*ten_day| 
                    clinic_code  +  visit_date | 0 | clinic_code,
                   data = data_j,
                   exactDOF=TRUE,
                   keepCX = TRUE)  

index_cov_time <- felm(z_index ~ arab*ten_day + gender_num + religiosity_num +
                        edu_num + age| 
                        clinic_code  +  visit_date | 0 | clinic_code,
                       data = data_j,
                       exactDOF=TRUE,
                       keepCX = TRUE)  



# Output models into table
stargazer(sd_time, sd_cov_time,
          peace_time, peace_cov_time,
          therm_time, therm_cov_time,
          trust_time, trust_cov_time,
          index_time, index_cov_time,
          style="qje",
          title="Moderating Effects of Survey Timing",
          keep = c("arab", "ten_day", "arab:ten_day"),
          dep.var.labels = c("Social Distance", "Peace", "Thermometer", "Trust", "Index"),
          covariate.labels = c("Arab Doctor", "10 Day",
                               "Arab*10 Day"),
          omit.stat = c("rsq", "f", "ser", "adj.rsq"),
          ci=FALSE,
          no.space = T,
          column.sep.width = "-02pt",
          omit.table.layout = "n",
          star.cutoffs = NULL,
          font.size = "small",
          label = "tab:time",
          star.char = c("", "", ""),
          add.lines = list(
           c("Demog. Controls", "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes","No", "Yes"),
           c("Date FEs", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes","Yes", "Yes"),
           c("Clinic FEs", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes","Yes", "Yes"),
           c("Cluster",  "Clinic", "Clinic", "Clinic", "Clinic", "Clinic", "Clinic", "Clinic", "Clinic","Clinic", "Clinic")))



#############################################################################
# Moderating effect of a cycle of violence -- Table S10
#############################################################################


# Social distance
sd_viol <- felm(z_sd ~ arab*cycle_violence| 
                 clinic_code  +  visit_date | 0 | clinic_code,
                data = data_j,
                exactDOF=TRUE,
                keepCX = TRUE)  
# Peace
peace_viol <- felm(z_peace ~ arab*cycle_violence| 
                    clinic_code  +  visit_date | 0 | clinic_code,
                   data = data_j,
                   exactDOF=TRUE,
                   keepCX = TRUE)  

# therm
therm_viol <- felm(z_therm ~ arab*cycle_violence| 
                    clinic_code  +  visit_date | 0 | clinic_code,
                   data = data_j,
                   exactDOF=TRUE,
                   keepCX = TRUE)  

# trust
trust_viol <- felm(z_trust ~ arab*cycle_violence |
                    clinic_code  +  visit_date | 0 | clinic_code,
                   data = data_j,
                   exactDOF=TRUE,
                   keepCX = TRUE)  

# index
index_viol <- felm(z_index ~ arab*cycle_violence| 
                    clinic_code  +  visit_date | 0 | clinic_code,
                   data = data_j,
                   exactDOF=TRUE,
                   keepCX = TRUE)  

stargazer(sd_viol, peace_viol,
          therm_viol, trust_viol, index_viol,
          style="qje",
          title="Moderating Effects of Violent Cycle",
          keep = c("arab", "arab:cycle_violence"),
          dep.var.labels = c("Social Distance", "Peace", "Thermometer", "Trust", "Index"),
          covariate.labels = c("Arab Doctor",
                               "Arab*Violence"),
          omit.stat = c("rsq", "f", "ser", "adj.rsq"),
          ci=FALSE,
          no.space = T,
          column.sep.width = "-02pt",
          omit.table.layout = "n",
          star.cutoffs = NULL,
          font.size = "small",
          label = "tab:viol",
          star.char = c("", "", ""),
          add.lines = list(
           c("Sample", "Full",  "Full", "Full", "Full","Full"),
           c("Demog. Controls", "No", "No", "No", "No","No"),
           c("Date FEs", "Yes", "Yes", "Yes", "Yes","Yes"),
           c("Clinic FEs", "Yes", "Yes", "Yes", "Yes","Yes"),
           c("Cluster",  "Clinic", "Clinic", "Clinic", "Clinic", "Clinic")))




#############################################################################
# Moderating effect of previous contact with Arabs -- Table S11
#############################################################################


# Social distance
sd_prev <- felm(z_sd ~ arab*previous_contact_arabs_num| 
                 clinic_code  +  visit_date | 0 | clinic_code,
                data = data_j,
                exactDOF=TRUE,
                keepCX = TRUE)  

sd_cov_prev <- felm(z_sd ~ arab*previous_contact_arabs_num + gender_num + religiosity_num +
                     edu_num + age| 
                     clinic_code  +  visit_date | 0 | clinic_code,
                    data = data_j,
                    exactDOF=TRUE,
                    keepCX = TRUE)  


# Peace
peace_prev <- felm(z_peace ~ arab*previous_contact_arabs_num| 
                    clinic_code  +  visit_date | 0 | clinic_code,
                   data = data_j,
                   exactDOF=TRUE,
                   keepCX = TRUE)  


peace_cov_prev <- felm(z_peace ~ arab*previous_contact_arabs_num+ gender_num + religiosity_num +
                        edu_num + age| 
                        clinic_code  +  visit_date | 0 | clinic_code,
                       data = data_j,
                       exactDOF=TRUE,
                       keepCX = TRUE)  

# therm
therm_prev <- felm(z_therm ~ arab*previous_contact_arabs_num| 
                    clinic_code  +  visit_date | 0 | clinic_code,
                   data = data_j,
                   exactDOF=TRUE,
                   keepCX = TRUE)  

therm_cov_prev <- felm(z_therm ~ arab*previous_contact_arabs_num + gender_num + religiosity_num +
                        edu_num + age| 
                        clinic_code  +  visit_date | 0 | clinic_code,
                       data = data_j,
                       exactDOF=TRUE,
                       keepCX = TRUE)  



# trust
trust_prev <- felm(z_trust ~ arab*previous_contact_arabs_num| 
                    clinic_code  +  visit_date | 0 | clinic_code,
                   data = data_j,
                   exactDOF=TRUE,
                   keepCX = TRUE)  

trust_cov_prev <- felm(z_trust ~ arab*previous_contact_arabs_num + gender_num + religiosity_num +
                        edu_num + age| 
                        clinic_code  +  visit_date | 0 | clinic_code,
                       data = data_j,
                       exactDOF=TRUE,
                       keepCX = TRUE)  

# index
index_prev <- felm(z_index ~ arab*previous_contact_arabs_num| 
                    clinic_code  +  visit_date | 0 | clinic_code,
                   data = data_j,
                   exactDOF=TRUE,
                   keepCX = TRUE)  

index_cov_prev <- felm(z_index ~ arab*previous_contact_arabs_num + gender_num + religiosity_num +
                        edu_num + age| 
                        clinic_code  +  visit_date | 0 | clinic_code,
                       data = data_j,
                       exactDOF=TRUE,
                       keepCX = TRUE)  





stargazer(sd_prev, sd_cov_prev,
          peace_prev, peace_cov_prev,
          therm_prev, therm_cov_prev,
          trust_prev, trust_cov_prev,
          index_prev, index_cov_prev,
          style="qje",
          title="Moderating Effects of Previous Contact with Arabs",
          keep = c("arab", "previous_contact_arabs_num", "arab:previous_contact_arabs_num"),
          dep.var.labels = c("Social Distance", "Peace", "Thermometer", "Trust", "Index"),
          covariate.labels = c("Arab Doctor", "Previous Contact",
                               "Arab*Prev. Contact"),
          omit.stat = c("rsq", "f", "ser", "adj.rsq"),
          ci=FALSE,
          no.space = T,
          column.sep.width = "-02pt",
          omit.table.layout = "n",
          star.cutoffs = NULL,
          font.size = "small",
          label = "tab:prev",
          star.char = c("", "", ""),
          add.lines = list(
           c("Demog. Controls", "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes","No", "Yes"),
           c("Date FEs", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes","Yes", "Yes"),
           c("Clinic FEs", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes","Yes", "Yes"),
           c("Cluster",  "Clinic", "Clinic", "Clinic", "Clinic", "Clinic", "Clinic", "Clinic", "Clinic","Clinic", "Clinic")))




#############################################################################
# Moderating effect of gender/religiosity -- Table S12
#############################################################################



# Social distance

sd_gend <- felm(z_sd ~ arab*gender_num| 
                 clinic_code  +  visit_date | 0 | clinic_code,
                data = data_j,
                exactDOF=TRUE,
                keepCX = TRUE)  
sd_relig <- felm(z_sd ~ arab*religiosity_num| 
                  clinic_code  +  visit_date | 0 | clinic_code,
                 data = data_j,
                 exactDOF=TRUE,
                 keepCX = TRUE)  



# Peace
peace_gend <- felm(z_peace ~ arab*gender_num| 
                    clinic_code  +  visit_date | 0 | clinic_code,
                   data = data_j,
                   exactDOF=TRUE,
                   keepCX = TRUE)  
peace_relig <- felm(z_peace ~ arab*religiosity_num| 
                     clinic_code  +  visit_date | 0 | clinic_code,
                    data = data_j,
                    exactDOF=TRUE,
                    keepCX = TRUE)  

# therm
therm_gend <- felm(z_therm ~ arab*gender_num| 
                    clinic_code  +  visit_date | 0 | clinic_code,
                   data = data_j,
                   exactDOF=TRUE,
                   keepCX = TRUE)  
therm_relig <- felm(z_therm ~ arab*religiosity_num| 
                     clinic_code  +  visit_date | 0 | clinic_code,
                    data = data_j,
                    exactDOF=TRUE,
                    keepCX = TRUE)  



# trust
trust_gend <- felm(z_trust ~ arab*gender_num| 
                    clinic_code  +  visit_date | 0 | clinic_code,
                   data = data_j,
                   exactDOF=TRUE,
                   keepCX = TRUE)  
trust_relig <- felm(z_trust ~ arab*religiosity_num| 
                     clinic_code  +  visit_date | 0 | clinic_code,
                    data = data_j,
                    exactDOF=TRUE,
                    keepCX = TRUE)  



# index
index_gend <- felm(z_index ~ arab*gender_num| 
                    clinic_code  +  visit_date | 0 | clinic_code,
                   data = data_j,
                   exactDOF=TRUE,
                   keepCX = TRUE)  
index_relig <- felm(z_index ~ arab*religiosity_num| 
                     clinic_code  +  visit_date | 0 | clinic_code,
                    data = data_j,
                    exactDOF=TRUE,
                    keepCX = TRUE)  





stargazer(sd_gend, sd_relig,
          peace_gend, peace_relig,
          therm_gend, therm_relig,
          trust_gend, trust_relig,
          index_gend, index_relig,
          style="qje",
          title="Moderating Effects of Gender/Religiosity",
          keep = c("arab", "gender_num", "religiosity_num",  
                   "arab:gender_num", "arab:religiosity_num"),
          dep.var.labels = c("Social Distance", "Peace", "Thermometer", "Trust", "Index"),
          covariate.labels = c("Arab Doctor", "Gender",
                               "Arab*Gender", "Religioisity", "Arab*Relig"),
          omit.stat = c("rsq", "f", "ser", "adj.rsq"),
          ci=FALSE,
          no.space = T,
          column.sep.width = "-02pt",
          omit.table.layout = "n",
          star.cutoffs = NULL,
          font.size = "small",
          label = "tab:gend_relig",
          star.char = c("", "", ""),
          add.lines = list(
           c("Demog. Controls", "No", "No", "No", "No", "No", "No", "No", "No","No", "No"),
           c("Date FEs", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes","Yes", "Yes"),
           c("Clinic FEs", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes","Yes", "Yes"),
           c("Cluster",  "Clinic", "Clinic", "Clinic", "Clinic", "Clinic", "Clinic", "Clinic", "Clinic","Clinic", "Clinic")))






#############################################################################
# Balance including Ideology -- Figure S4
#############################################################################

# Regress arab doctor indicator over covariates 

fe_diagnostic_r <- lm_robust(arab ~ religiosity_num, 
                             data = data_j,
                             fixed_effects = clinic_code + visit_date,
                             clusters = clinic_code, se_type = "CR0") %>% 
 tidy(., conf.int = T) %>% mutate(Model = "FE",
                                  term = "Religiosity \nn=2058")


# Gender n = 2164

fe_diagnostic_g <- lm_robust(arab ~ gender_num, 
                             data = data_j,
                             fixed_effects = clinic_code + visit_date,
                             clusters = clinic_code, se_type = "CR0") %>% 
 tidy(., conf.int = T) %>% mutate(Model = "FE",
                                  term = "Gender\nn=2164")

# Education n = 1914

fe_diagnostic_e <- lm_robust(arab ~ edu_num, 
                             data = data_j,
                             fixed_effects = clinic_code + visit_date,
                             clusters = clinic_code, se_type = "CR0") %>% 
 tidy(., conf.int = T) %>% mutate(Model = "FE",
                                  term = "Education \nn=1914")




# Age n = 2164

fe_diagnostic_a <- lm_robust(arab ~ age, 
                             data = data_j,
                             fixed_effects = clinic_code + visit_date,
                             clusters = clinic_code, se_type = "CR0") %>% 
 tidy(., conf.int = T) %>% mutate(Model = "FE",
                                  term = "Age \nn=2164")


# Ideology n = 1675

fe_diagnostic_ideology <- lm_robust(arab ~ right_left, 
                                    data = data_j,
                                    fixed_effects = clinic_code + visit_date,
                                    clusters = clinic_code, se_type = "CR0") %>% 
 tidy(., conf.int = T) %>% mutate(Model = "FE",
                                  term = "Ideology \nn=1675")




# Create balance plot
bal_base2 <- rbind(fe_diagnostic_r, fe_diagnostic_g, fe_diagnostic_e, fe_diagnostic_a, fe_diagnostic_ideology)
ggplot(bal_base2, aes(x = term, y = estimate)) +
 geom_hline(yintercept = 0, color = "gray50", linetype = 2, size = 0.2) +
 geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                 position = position_dodge(width = 0.4),
                 fill = "white", color = "dodgerblue4") +
 coord_flip()+
 # scale_color_manual(values = c("firebrick2", "dodgerblue2")) +
 # scale_fill_manual(values = c("firebrick3", "dodgerblue3")) +
 labs(x = "",
      y = "Effect Size") +
 scale_y_continuous(limits = c(-0.07, 0.055)) + 
 # scale_x_discrete(limits= order_x) +
 # caption = "*Coefficients from OLS models, in which outcomes are standardized."
 theme(text = element_text(size = 12, family = "Times"),
       legend.key=element_blank(),
       panel.grid.major = element_blank(), 
       axis.text.x = element_text(size = 12),
       plot.caption = element_text(size = 12, family = "Times",hjust = -.02),
       panel.grid.minor = element_blank(),
       panel.background = element_blank(), 
       axis.line = element_line(colour = "black"))


#############################################################################
# Consider the moderating effect of ideology on main treatment -- Table S13
#############################################################################

# Social distance
sd_ideo <- felm(z_sd ~ arab*right_left| 
                 clinic_code  +  visit_date | 0 | clinic_code,
                data = data_j,
                exactDOF=TRUE,
                keepCX = TRUE)  

# Peace
peace_ideo <- felm(z_peace ~ arab*right_left| 
                    clinic_code  +  visit_date | 0 | clinic_code,
                   data = data_j,
                   exactDOF=TRUE,
                   keepCX = TRUE)  

# Therm
therm_ideo <- felm(z_therm ~ arab*right_left| 
                    clinic_code  +  visit_date | 0 | clinic_code,
                   data = data_j,
                   exactDOF=TRUE,
                   keepCX = TRUE)  


# Trust
trust_ideo <- felm(z_trust ~ arab*right_left| 
                    clinic_code  +  visit_date | 0 | clinic_code,
                   data = data_j,
                   exactDOF=TRUE,
                   keepCX = TRUE)  


# Index
index_ideo <- felm(z_index ~ arab*right_left| 
                    clinic_code  +  visit_date | 0 | clinic_code,
                   data = data_j,
                   exactDOF=TRUE,
                   keepCX = TRUE)  

stargazer(sd_ideo, peace_ideo, therm_ideo, trust_ideo, index_ideo,
          style="qje",
          title="Moderating Effects of Patient Ideology",
          dep.var.labels = c("Social Distance", "Peace", "Thermometer", "Trust", "Index"),
          covariate.labels = c("Arab Doctor", "Patient Ideology", "Arab*Ideology"),
          omit.stat = c("rsq", "f", "ser", "adj.rsq"),
          ci=FALSE,
          no.space = T,
          column.sep.width = "-02pt",
          omit.table.layout = "n",
          star.cutoffs = NULL,
          font.size = "small",
          label = "tab:ideo_mod",
          star.char = c("", "", ""),
          add.lines = list(
           c("Date FEs", "Yes", "Yes", "Yes", "Yes","Yes"),
           c("Clinic FEs", "Yes", "Yes", "Yes", "Yes","Yes"),
           c("Cluster",  "Clinic", "Clinic", "Clinic", "Clinic","Clinic")))




#########################################################################################
# Consider the effects of contact on attitudes towards foriegn workers -- Table S14
#########################################################################################

### SD
sd_fw <- felm(z_sd_worker ~ arab| 
               clinic_code  +  visit_date | 0 |clinic_code,
              data = data_j,
              exactDOF=TRUE,
              keepCX = TRUE)


sd_fw_cov <- felm(z_sd_worker ~ arab + gender_num + religiosity_num +
                   edu_num + age| 
                   clinic_code  +  visit_date | 0 |clinic_code,
                  data = data_j,
                  exactDOF=TRUE,
                  keepCX = TRUE)


# Therm

therm_fw <- felm(z_therm_worker ~ arab| 
                  clinic_code  +  visit_date | 0 |clinic_code,
                 data = data_j,
                 exactDOF=TRUE,
                 keepCX = TRUE)


therm_fw_cov <- felm(z_therm_worker ~ arab + gender_num + religiosity_num +
                      edu_num + age| 
                      clinic_code  +  visit_date | 0 |clinic_code,
                     data = data_j,
                     exactDOF=TRUE,
                     keepCX = TRUE)






stargazer(sd_fw, sd_fw_cov,
          therm_fw, therm_fw_cov,
          style="qje",
          title="Effects of Arab Doctor on Prejudice towards Foriegn Workers",
          dep.var.labels = c("Social Distance", "Thermometer"),
          covariate.labels = c("Arab Doctor", "Gender", "Religiosity",  "Education", "Age"),
          omit.stat = c("rsq", "f", "ser", "adj.rsq"),
          ci=FALSE,
          no.space = T,
          omit.table.layout = "n",
          star.cutoffs = NULL,
          font.size = "small",
          label = "tab:foriegn_work",
          star.char = c("", "", ""),
          add.lines = list(
           c("Date FEs", "Yes", "Yes", "Yes", "Yes",
             "Yes", "Yes", "Yes", "Yes"),
           c("Clinic FEs", "Yes", "Yes", "Yes", "Yes",
             "Yes", "Yes", "Yes", "Yes"),
           c("Cluster",  "Clinic", "Clinic", "Clinic", "Clinic",
             "Clinic", "Clinic", "Clinic", "Clinic")))



#####################################################################################################
# Consider the effects of contact with Jewish doctor on Arabs attitudes towards jews -- Table S15
#####################################################################################################

# Create arab subset of data
data_a <- data %>% 
 filter(.,
        jewish == 0) %>% 
 mutate(.,
        jewish_doc = ifelse(arab == 1,0,1))
# Social distance
sd_arabs <- felm(z_sd_j~ jewish_doc| 
                  clinic_code  +  visit_date | 0 | clinic_code,
                 data = data_a,
                 exactDOF=TRUE,
                 keepCX = TRUE)  
# Peace
peace_arabs <- felm(z_peace_j ~ jewish_doc| 
                     clinic_code  +  visit_date | 0 | clinic_code,
                    data = data_a,
                    exactDOF=TRUE,
                    keepCX = TRUE)  

# Therm
therm_arabs <- felm(z_therm_j ~ jewish_doc| 
                     clinic_code  +  visit_date | 0 | clinic_code,
                    data = data_a,
                    exactDOF=TRUE,
                    keepCX = TRUE)  
# Trust
trust_arabs <- felm(z_trust_j ~ jewish_doc| 
                     clinic_code  +  visit_date | 0 | clinic_code,
                    data = data_a,
                    exactDOF=TRUE,
                    keepCX = TRUE)  
# Index
index_arabs <- felm(z_index_j ~ jewish_doc| 
                     clinic_code  +  visit_date | 0 | clinic_code,
                    data = data_a,
                    exactDOF=TRUE,
                    keepCX = TRUE)  

stargazer(sd_arabs, peace_arabs, therm_arabs, trust_arabs, index_arabs,
          style="qje",
          title="Effect of Contact with a Jewish Doctor on Arab Patients' Prejudice",
          dep.var.labels = c("Social Distance", "Peace", "Thermometer", "Trust", "Index"),
          covariate.labels = c("Jewish Doctor"),
          omit.stat = c("rsq", "f", "ser", "adj.rsq"),
          ci=FALSE,
          no.space = T,
          column.sep.width = "-02pt",
          omit.table.layout = "n",
          star.cutoffs = NULL,
          font.size = "small",
          label = "tab:arab_effect",
          star.char = c("", "", ""),
          add.lines = list(
           c("Date FEs", "Yes", "Yes", "Yes", "Yes","Yes"),
           c("Clinic FEs", "Yes", "Yes", "Yes", "Yes","Yes"),
           c("Cluster",  "Clinic", "Clinic", "Clinic", "Clinic","Clinic")))




#########################################################################################
# Consider effects of previous encounters with Arab doctors amongst patients who 
# recently saw a Jewish doc -- Table S16
#########################################################################################


# Social distance
sd_previous_doc <- data_j %>% 
 filter(., arab==0) %>% 
 felm(z_sd ~ arab_past| 
       clinic_code + visit_date| 0 |clinic_code,
      data = .)



# Peace
peace_previous_doc <- data_j %>% 
 filter(., arab==0) %>% 
 felm(z_peace ~ arab_past| 
       clinic_code + visit_date| 0 |clinic_code,
      data = .)




# Therm
therm_previous_doc <- data_j %>% 
 filter(., arab==0) %>% 
 felm(z_therm ~ arab_past| 
       clinic_code + visit_date| 0 |clinic_code,
      data = .)



# Trust
trust_previous_doc <- data_j %>% 
 filter(., arab==0) %>% 
 felm(z_trust ~ arab_past| 
       clinic_code + visit_date| 0 |clinic_code,
      data = .)



# Index
index_previous_doc <- data_j %>% 
 filter(., arab==0) %>% 
 felm(z_index ~ arab_past| 
       clinic_code + visit_date| 0 |clinic_code,
      data = .)





stargazer(sd_previous_doc, peace_previous_doc, therm_previous_doc, 
          trust_previous_doc, index_previous_doc, 
          style="qje",
          title="Effects of Previous encounters with Arab Doctors (Amongst Subjects from Control Group)",
          dep.var.labels = c("Social Distance", "Peace", "Thermometer", "Trust", "Index"),
          covariate.labels = c("Arab Doctor"),
          omit.stat = c("rsq", "f", "ser", "adj.rsq"),
          ci=FALSE,
          no.space = T,
          column.sep.width = "-02pt",
          omit.table.layout = "n",
          star.cutoffs = NULL,
          font.size = "small",
          label = "tab:prev_doc_fx",
          star.char = c("", "", ""),
          add.lines = list(
           c("Date FEs",  "Yes","Yes","Yes","Yes","Yes"),
           c("Clinic FEs", "Yes","Yes","Yes","Yes","Yes"),
           c("Cluster",  "Clinic", "Clinic", "Clinic", "Clinic","Clinic")))



#####################################################################################################
# Consider effects of previous encounters with Arab doctors amongst patients who -- Figure S5
#####################################################################################################

sd_long <- felm(z_sd ~ arab + arab_past| 
                 clinic_code + visit_date| 0 |clinic_code,
                data = data_j)
sd_long_term <- tidy(sd_long, robust = T, conf.int = T) %>% 
 mutate(.,
        Outcome = "Social \nDistance")


peace_long <- felm(z_peace ~ arab + arab_past| 
                    clinic_code + visit_date| 0 |clinic_code,
                   data = data_j)
peace_long_term <- tidy(peace_long, robust = T, conf.int = T) %>% 
 mutate(.,
        Outcome = "Peace")


therm_long <- felm(z_therm ~ arab + arab_past| 
                    clinic_code + visit_date| 0 |clinic_code,
                   data = data_j)
therm_long_term <- tidy(therm_long, robust = T, conf.int = T) %>% 
 mutate(.,
        Outcome = "Feeling \nThermometer")



trust_long <- felm(z_trust ~ arab + arab_past| 
                    clinic_code + visit_date| 0 |clinic_code,
                   data = data_j)
trust_long_term <- tidy(trust_long, robust = T, conf.int = T) %>% 
 mutate(.,
        Outcome = "Trust")



index_long <- felm(z_index ~ arab + arab_past| 
                    clinic_code + visit_date| 0 |clinic_code,
                   data = data_j)
index_long_term <- tidy(index_long, robust = T, conf.int = T) %>% 
 mutate(.,
        Outcome = "Index")


long_term_plot <- rbind(sd_long_term, peace_long_term, therm_long_term,
                        trust_long_term, index_long_term) %>% 
 mutate(.,
        term = ifelse(term == "arab", "Recently Treated \nby Arab Doctor", "Previously Treated \nby Arab Doctor"))
long_term_plot$term <- factor(long_term_plot$term, 
                              levels = c("Recently Treated \nby Arab Doctor", "Previously Treated \nby Arab Doctor"))

positions_main <- c("Social \nDistance", "Peace", "Feeling \nThermometer",
                    "Trust", "Index")
ggplot(long_term_plot, aes(x = Outcome, y = estimate, color = term, fill = term, shape = term)) +
 geom_hline(yintercept = 0, color = "gray50", linetype = 2, size = 0.2) +
 geom_pointrange(aes(ymin = conf.low, ymax = conf.high), 
                 position = position_dodge(width = 0.6)) +
 scale_color_brewer(palette = "Set1")+
 scale_x_discrete(limits = positions_main)+ 
 scale_y_continuous(limits = c(-0.4, 0.5)) + 
 labs(x = "",
      y = "Effect Size",
      color = "",
      shape = "",
      fill = "") +
 theme(text = element_text(size = 12, family = "Times"),
       panel.grid.major = element_blank(), 
       legend.key = element_rect(colour = NA, fill = NA),
       axis.text.x = element_text(size = 12),
       legend.position="bottom",
       plot.caption = element_text(size = 10, family = "Times",hjust = -.02),
       panel.grid.minor = element_blank(),
       panel.background = element_blank(), 
       axis.line = element_line(colour = "black"))

