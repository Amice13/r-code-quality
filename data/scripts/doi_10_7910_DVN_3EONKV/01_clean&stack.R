########################################
#### CONJOINT ANALYSIS - CLEAN DATASET #
########################################


#This code cleans the dataset and gets it ready for the empirical analysis

#INCLUDE YOUR PATH HERE
rm(list=ls())

#warning: in some operating systems, the file needs to be opened in UTF-8

# Clean sociodemographics -------------------------------------------------
library("readxl")
library("plyr")
library("lattice")
library("car")
library("openxlsx")

#If necessary, set encoding to avoid errors
Sys.setlocale(locale = "en_US.ISO8859-1")

#Open excel file with information on respondent's sociodemographic characteristics. 
sociodata <- read_xlsx("ses_data.xlsx", sheet="data")


#Recode ethnicity - race
sociodata$ethnicity <- as.factor(sociodata$ethnicity)
sociodata$ethnicity <- recode(sociodata$ethnicity , 
                              "c('Caucasian', 'White')='1'; else='0'")

#education
sociodata$education <- as.factor(sociodata$education)
sociodata$education <- recode(sociodata$education , 
                              "c('College/A levels', 'Doctorate degree (PhD/MD/other)',
                              'Graduate degree (MA/MSc/MPhil/other)')='College or higher'; 
                              c('Undergraduate degree (BA/BSc/other)')='Undergraduate degree';
                              c('Secondary school/GCSE')='Secondary education';
                              else='Other'")

#political affiliation (we remove 'others' as we don't know what they mean)
sociodata$political_affiliation <- as.factor(sociodata$political_affiliation)
sociodata$political_affiliation <- recode(sociodata$political_affiliation ,  
                                          "c('MISSING', 'N/A', 'Other')='Other'; 'None'='Independent'")

sociodata$political_affiliation2 <- NA
sociodata$political_affiliation2[sociodata$political_affiliation=="Democrat"] <- "Democrat"
sociodata$political_affiliation2[sociodata$political_affiliation=="Independent"] <- "Independent"
sociodata$political_affiliation2[sociodata$political_affiliation=="Republican"] <- "Republican"

sociodata$political_affiliation2 <- as.factor(sociodata$political_affiliation2)

sociodata$PID <- as.factor(sociodata$political_affiliation2)


#income
sociodata$income2 <- as.factor(sociodata$income)
sociodata$income <- as.factor(sociodata$income)
sociodata$income <- recode(sociodata$income ,  
                           "c('£10,000 - £19,999', 'Less than £10,000', '£20,000 - £29,999')='Less than 30,000';
                           c('£30,000 - £39,999', '£40,000 - £49,999', '£50,000 - £59,999')='Between 30,000 and 60,000';
                           c('£60,000 - £69,999', '£70,000 - £79,999', '£80,000 - £89,999', '£90,000 - £99,999',
                              '£100,000 - £149,999', 'More than £150,000')='More than 60,000';
                           else='Other'")
sociodata$income <- as.factor(sociodata$income)

sociodata$Income_group <- NA
sociodata$Income_group[sociodata$income2=="Less than £10,000" | 
                   sociodata$income2 == "£10,000 - £19,999"] <- 1
sociodata$Income_group[sociodata$income2 == "£90,000 - £99,999" | 
              sociodata$income2 == "£100,000 - £149,999" | 
              sociodata$income2 == "More than £150,000" ] <- 0


sociodata$Income_group <- as.factor(sociodata$Income_group)
levels(sociodata$Income_group)[levels(sociodata$Income_group)==0] <- "High"
levels(sociodata$Income_group)[levels(sociodata$Income_group)==1] <- "Low"




#household income
sociodata$h_income2 <- as.factor(sociodata$h_income)
sociodata$h_income <- as.factor(sociodata$h_income)
sociodata$h_income <- recode(sociodata$h_income ,  
                             "c('£10,000 - £19,999', ' Less than £10,000', '£20,000 - £29,999')='Less than 30,000';
                             c('£30,000 - £39,999', ' £40,000 - £49,999', '£50,000 - £59,999')='Between 30,000 and 60,000';
                             c('£60,000 - £69,999', '£70,000 - £79,999', '£80,000 - £89,999', '£90,000 - £99,999',
                              '£100,000 - £149,999', 'More than £150,000')='More than 60,000';
                             else='Other'")
sociodata$h_income <- as.factor(sociodata$h_income)

#age
sociodata$age[is.na(sociodata$age)] <- 31.76

#age
sociodata$age_cat <- 0
sociodata$age_cat[sociodata$age>0 & sociodata$age<30  ] <- 1
sociodata$age_cat[sociodata$age>29 & sociodata$age<60  ] <- 2
sociodata$age_cat[sociodata$age>59 & sociodata$age<100  ] <- 3

sociodata$age_cat <- as.factor(sociodata$age_cat)
sociodata$age_cat <- recode(sociodata$age_cat,  
                             "c('1')='Less than 30';
                             c('2')='Between 30 and 60';
                             c('')='More than 60'")

#sex
sociodata$sex <- as.factor(sociodata$sex)
sociodata$sex[is.na(sociodata$sex)] <- "Male"

# Start conjoint -------------------------------------------------

#Open dataset extracted from Prolific
mydata <- read_excel("conjoint.xlsx") 


#keep only variables of interest. 
myconjoint <- mydata[c(11,19:30, 37:48, 55:66, 69, 75:86, 93:104, 106)] 
rm(mydata)


#Convert from wide to long for analysis
#create first a unique id
id <- rownames(myconjoint)
myconjoint <- cbind(id=id, myconjoint)
myconjoint$id <- as.factor(myconjoint$id)
rm(id)

#change conjoint labels to stack the dataset. 

#First table
names(myconjoint)[names(myconjoint) == 'cf1_1'] <- 'Q1A_t1'
names(myconjoint)[names(myconjoint) == 'cf1_2'] <- 'Q1B_t1'
names(myconjoint)[names(myconjoint) == 'Q20_1_TEXT'] <- 'wealthA_t1'
names(myconjoint)[names(myconjoint) == 'Q20_2_TEXT'] <- 'originA_t1'
names(myconjoint)[names(myconjoint) == 'Q20_3_TEXT'] <- 'wealthiestA_t1'
names(myconjoint)[names(myconjoint) == 'Q20_4_TEXT'] <- 'poorestA_t1'
names(myconjoint)[names(myconjoint) == 'Q20_5_TEXT'] <- 'mobilityA_t1'
names(myconjoint)[names(myconjoint) == 'Q20_6_TEXT'] <- 'wealthB_t1'
names(myconjoint)[names(myconjoint) == 'Q20_7_TEXT'] <- 'originB_t1'
names(myconjoint)[names(myconjoint) == 'Q20_8_TEXT'] <- 'wealthiestB_t1'
names(myconjoint)[names(myconjoint) == 'Q20_9_TEXT'] <- 'poorestB_t1'
names(myconjoint)[names(myconjoint) == 'Q20_10_TEXT'] <- 'mobilityB_t1'

#Second table
names(myconjoint)[names(myconjoint) == 'cf2_1'] <- 'Q1A_t2'
names(myconjoint)[names(myconjoint) == 'cf2_2'] <- 'Q1B_t2'
names(myconjoint)[names(myconjoint) == 'Q19_1_TEXT'] <- 'wealthA_t2'
names(myconjoint)[names(myconjoint) == 'Q19_2_TEXT'] <- 'originA_t2'
names(myconjoint)[names(myconjoint) == 'Q19_3_TEXT'] <- 'wealthiestA_t2'
names(myconjoint)[names(myconjoint) == 'Q19_4_TEXT'] <- 'poorestA_t2'
names(myconjoint)[names(myconjoint) == 'Q19_5_TEXT'] <- 'mobilityA_t2'
names(myconjoint)[names(myconjoint) == 'Q19_6_TEXT'] <- 'wealthB_t2'
names(myconjoint)[names(myconjoint) == 'Q19_7_TEXT'] <- 'originB_t2'
names(myconjoint)[names(myconjoint) == 'Q19_8_TEXT'] <- 'wealthiestB_t2'
names(myconjoint)[names(myconjoint) == 'Q19_9_TEXT'] <- 'poorestB_t2'
names(myconjoint)[names(myconjoint) == 'Q19_10_TEXT'] <- 'mobilityB_t2'

#Third table
names(myconjoint)[names(myconjoint) == 'cf3_1'] <- 'Q1A_t3'
names(myconjoint)[names(myconjoint) == 'cf3_2'] <- 'Q1B_t3'
names(myconjoint)[names(myconjoint) == 'Q24_1_TEXT'] <- 'wealthA_t3'
names(myconjoint)[names(myconjoint) == 'Q24_2_TEXT'] <- 'originA_t3'
names(myconjoint)[names(myconjoint) == 'Q24_3_TEXT'] <- 'wealthiestA_t3'
names(myconjoint)[names(myconjoint) == 'Q24_4_TEXT'] <- 'poorestA_t3'
names(myconjoint)[names(myconjoint) == 'Q24_5_TEXT'] <- 'mobilityA_t3'
names(myconjoint)[names(myconjoint) == 'Q24_6_TEXT'] <- 'wealthB_t3'
names(myconjoint)[names(myconjoint) == 'Q24_7_TEXT'] <- 'originB_t3'
names(myconjoint)[names(myconjoint) == 'Q24_8_TEXT'] <- 'wealthiestB_t3'
names(myconjoint)[names(myconjoint) == 'Q24_9_TEXT'] <- 'poorestB_t3'
names(myconjoint)[names(myconjoint) == 'Q24_10_TEXT'] <- 'mobilityB_t3'

#Fourth table
names(myconjoint)[names(myconjoint) == 'cf4_1'] <- 'Q1A_t4'
names(myconjoint)[names(myconjoint) == 'cf4_2'] <- 'Q1B_t4'
names(myconjoint)[names(myconjoint) == 'Q29_1_TEXT'] <- 'wealthA_t4'
names(myconjoint)[names(myconjoint) == 'Q29_2_TEXT'] <- 'originA_t4'
names(myconjoint)[names(myconjoint) == 'Q29_3_TEXT'] <- 'wealthiestA_t4'
names(myconjoint)[names(myconjoint) == 'Q29_4_TEXT'] <- 'poorestA_t4'
names(myconjoint)[names(myconjoint) == 'Q29_5_TEXT'] <- 'mobilityA_t4'
names(myconjoint)[names(myconjoint) == 'Q29_6_TEXT'] <- 'wealthB_t4'
names(myconjoint)[names(myconjoint) == 'Q29_7_TEXT'] <- 'originB_t4'
names(myconjoint)[names(myconjoint) == 'Q29_8_TEXT'] <- 'wealthiestB_t4'
names(myconjoint)[names(myconjoint) == 'Q29_9_TEXT'] <- 'poorestB_t4'
names(myconjoint)[names(myconjoint) == 'Q29_10_TEXT'] <- 'mobilityB_t4'

#Fifth table
names(myconjoint)[names(myconjoint) == 'cf5_1'] <- 'Q1A_t5'
names(myconjoint)[names(myconjoint) == 'cf5_2'] <- 'Q1B_t5'
names(myconjoint)[names(myconjoint) == 'Q33_1_TEXT'] <- 'wealthA_t5'
names(myconjoint)[names(myconjoint) == 'Q33_2_TEXT'] <- 'originA_t5'
names(myconjoint)[names(myconjoint) == 'Q33_3_TEXT'] <- 'wealthiestA_t5'
names(myconjoint)[names(myconjoint) == 'Q33_4_TEXT'] <- 'poorestA_t5'
names(myconjoint)[names(myconjoint) == 'Q33_5_TEXT'] <- 'mobilityA_t5'
names(myconjoint)[names(myconjoint) == 'Q33_6_TEXT'] <- 'wealthB_t5'
names(myconjoint)[names(myconjoint) == 'Q33_7_TEXT'] <- 'originB_t5'
names(myconjoint)[names(myconjoint) == 'Q33_8_TEXT'] <- 'wealthiestB_t5'
names(myconjoint)[names(myconjoint) == 'Q33_9_TEXT'] <- 'poorestB_t5'
names(myconjoint)[names(myconjoint) == 'Q33_10_TEXT'] <- 'mobilityB_t5'


# stack profiles
conj <- reshape(myconjoint, 
                varying = list(
                  names(myconjoint)[grep("^wealthA", names(myconjoint))],
                  names(myconjoint)[grep("^wealthB", names(myconjoint))],
                  names(myconjoint)[grep("^originA", names(myconjoint))],
                  names(myconjoint)[grep("^originB", names(myconjoint))],
                  names(myconjoint)[grep("^wealthiestA", names(myconjoint))],
                  names(myconjoint)[grep("^wealthiestB", names(myconjoint))],
                  names(myconjoint)[grep("^poorestA", names(myconjoint))],
                  names(myconjoint)[grep("^poorestB", names(myconjoint))],
                  names(myconjoint)[grep("^mobilityA", names(myconjoint))],
                  names(myconjoint)[grep("^mobilityB", names(myconjoint))],
                  names(myconjoint)[grep("^Q1A", names(myconjoint))],
                  names(myconjoint)[grep("^Q1B", names(myconjoint))]
                ),
                v.names = c("wealthA", "wealthB", 
                            "originA", "originB",
                            "wealthiestA", "wealthiestB",
                            "poorestA", "poorestB",
                            "mobilityA", "mobilityB",
                          "Q1A", "Q1B"),
                timevar = "pair",
                idvar = "ID",
                direction = "long"
)

## create conjoint profile display pair variable
conj[["id_pair"]] <- paste0(conj[["ID"]], "_", conj[["pair"]])


# stack a/b options
conj <- reshape(conj,
                varying = list(
                  c("wealthA", "wealthB"), 
                  c("originA", "originB"),
                  c("wealthiestA", "wealthiestB"),
                  c("poorestA", "poorestB"),
                  c("mobilityA", "mobilityB"),
                  c("Q1A", "Q1B")                ),
                v.names = c("wealth", "origin", "wealthiest", "poorest", 
                            "mobility", "Q1"),
                timevar = "AB",
                idvar = "id_pair",
                direction = "long"
)

conj[["AB"]] <- c("A", "B")[conj[["AB"]]]

#For the conjoint analysis, we need the conjoint variables to be factors.

#wealth
conj$wealth <- as.factor(conj$wealth)
conj$wealth <- recode(conj$wealth, 
                               "'would decrease'= 'Would decrease';
                               'would increase'= 'Would increase';
                               'would stay the same'= 'Would stay the same'")
conj$wealth <- factor(conj$wealth, 
                 levels = c("Would stay the same", "Would decrease", "Would increase"))

#origin
conj$origin <- as.factor(conj$origin)
conj$origin <- recode(conj$origin, 
                      '"effort"= "Effort";
                      "family connections"= "Family connections";
                      "luck"= "Luck";
                      "people\'s talent" = "People\'s talent"')
conj$origin <- relevel(conj$origin, "People's talent", 
                         "Effort",
                         "Family connections",
                         "Luck")

#wealthiest
conj$wealthiest <- as.factor(conj$wealthiest)
conj$wealthiest <- recode(conj$wealthiest, 
                                   "'would be poorer'= 'Would be poorer';
                                   'would be wealthier'= 'Would be wealthier';
                                   'would keep their status'= 'Would keep their status'")
conj$wealthiest <- factor(conj$wealthiest, 
                                   levels = c("Would keep their status",
                                              "Would be poorer", "Would be wealthier"))
#poorest
conj$poorest <- as.factor(conj$poorest)
conj$poorest <- recode(conj$poorest, 
          "'would be poorer'= 'Would be poorer ';
         'would be wealthier'= 'Would be wealthier ';
         'would keep their status'= 'Would keep their status '")
conj$poorest <- factor(conj$poorest, 
                 levels = c("Would keep their status ", "Would be poorer ", "Would be wealthier "))

#mobility
conj$mobility <- as.factor(conj$mobility)
conj$mobility <- recode(conj$mobility, 
                 "'Downward mobility would be likely'= 'Downward mobility would be likely';
                  'Downward social mobility'= 'Downward mobility would be likely';
                'No social mobility'= 'No social mobility';
                 'None'= 'No social mobility';
                  'Upward and Downward mobility would be likely'= 'Upward and Downward mobility would be likely';
                   'Upward and Downward social mobility'= 'Upward and Downward mobility would be likely';
                'Upward mobility would be likely'= 'Upward mobility would be likely';   
          'Upward social mobility'= 'Upward mobility would be likely'")   

conj$mobility <- relevel(conj$mobility,"No social mobility", 
                         "Upward and Downward mobility would be likely",
                          "Downward mobility would be likely",
                          "Upward mobility would be likely")

#recode ideology
conj$ideol_cat <- as.factor(conj$self_ideol)
conj$ideol_cat <- recode(conj$ideol_cat, 
                                  '1=1; 2=1;3=1;4=2;5=3;6=3;7=3')

conj$ideol_cat <- factor(conj$ideol_cat,
                                  levels = c(1,2,3),
                                  labels = c("Liberal", "Moderate", "Conservative")) 

#change name outcome
conj <- rename(conj, c("Q1" = "fair"))

#merge sociodata back in
conj <- merge(conj,sociodata,by="prolific_id")


# EXPORT DATA FOR EMPIRICAL ANALYSES
library("rio")
rio::export(conj, "conjoint-data-stacked.rds")

