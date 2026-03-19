########################################
########### APPENDIX   #################
########################################

#This code reproduces the tables and figures included in the main text. 

#INCLUDE YOUR PATH HERE
rm(list=ls())
setwd("")

#warning: in some operating systems, the file needs to be opened in UTF-8
Sys.setlocale(locale = "en_US.ISO8859-1")

#install.packages("rio")
#install.packages("dplyr")
#install.packages("openxlsx")
#install.packages("tibble")
library("rio")
library("dplyr")
library("openxlsx")
library("tibble")

conj <- rio::import("conjoint-data-stacked.rds")

# Table A1: Sample description  --------------------------------------------------------------

myvars <- c("prol_id", "attack", "oil", "ally", "trade", "years", "gov", "person", "elections",
            "religion", "militar", "ID", "sanction", "foreignaid", "ideol", "promoting_1",
            "promoting_2", "promoting2_1", "promoting2_2")
data_short <- conj[myvars]
data_short <- data_short[complete.cases(data_short),]


sociodat <- read.xlsx("socio_demo.xlsx", sheet="socio_clean")

data_full <- merge(data_short,sociodat,by.x=c("prol_id"), 
                   by.y = c("Participant.id"))

#Recode control variables
#sex
data_full$sex <- as.factor(data_full$sex)
data_full$sex [is.na(data_full$sex )] <- "Male"

#Age - no need to recode
data_full$Age[is.na(data_full$Age)] <- 31.8

data_full$age_rec[data_full$Age>0 & data_full$Age<25] <- 1
data_full$age_rec[data_full$Age>24 & data_full$Age<35] <- 2
data_full$age_rec[data_full$Age>34 & data_full$Age<55] <- 3
data_full$age_rec[data_full$Age>54] <- 4
data_full$age_rec <- as.factor(data_full$age_rec)
data_full$age_rec <- factor(data_full$age_rec,
                            levels = c(1,2,3,4),
                            labels = c("Less than 25", "25-34", "35-54", "55-77")) 

#ethnicity
data_full$ethnicity <- as.factor(data_full$ethnicity)
data_full$ethnicity_short <- 0
data_full$ethnicity_short[data_full$ethnicity=="Caucasian"] <- 1
data_full$ethnicity_short[data_full$ethnicity=="African"] <- 2
data_full$ethnicity_short[data_full$ethnicity=="Latino/Hispanic"] <- 3
data_full$ethnicity_short[data_full$ethnicity=="East Asian"] <- 4

data_full$ethnicity_short <- as.factor(data_full$ethnicity_short)
data_full$ethnicity_short <- factor(data_full$ethnicity_short,
                                    levels = c(0,1,2,3,4),
                                    labels = c("Others", "Caucasian", "African",
                                               "Latino/Hispanic",
                                               "East Asian")) 

#political affiliation
data_full <- data_full %>% 
  rename(
    political_affiliation = 'Political.Affiliation.(US)')
data_full$pol_affiliation <- 0
data_full$pol_affiliation[data_full$political_affiliation=="Democrat"] <- 1
data_full$pol_affiliation[data_full$political_affiliation=="Republican"] <- 2
data_full$pol_affiliation <- factor(data_full$pol_affiliation,
                                    levels = c(0,1,2),
                                    labels = c("Rest", "Democrat", "Republican")) 

#household income
data_full <- data_full %>% 
  rename(
    householdinc = 'Household.Income.(GBP)')
data_full$hh_income <- 0
#In case your file comes with a strange character in front of the dolar sign, it means that the system imported
#the file in a different encoding. If it is the case, run the following line and/or adapt the character accordingly.
#data_full$householdinc <- gsub("Â", "", data_full$householdinc)
data_full$hh_income[data_full$householdinc=="N/A"] <- 0
data_full$hh_income[data_full$householdinc=="Rather not say"] <- 0
data_full$hh_income[data_full$householdinc=="Less than £10,000 "] <- 1
data_full$hh_income[data_full$householdinc=="£10,000 - £15,999"] <- 1
data_full$hh_income[data_full$householdinc=="£10,000 - £19,999"] <- 1
data_full$hh_income[data_full$householdinc=="£16,000 - £19,999"] <- 1
data_full$hh_income[data_full$householdinc=="£20,000 - £29,999"] <- 1
data_full$hh_income[data_full$householdinc=="£30,000 - £39,999"] <- 1
data_full$hh_income[data_full$householdinc=="£40,000 - £49,999"] <- 2
data_full$hh_income[data_full$householdinc=="£50,000 - £59,999"] <- 2
data_full$hh_income[data_full$householdinc=="£60,000 - £69,999"] <- 2
data_full$hh_income[data_full$householdinc=="£70,000 - £79,999"] <- 2
data_full$hh_income[data_full$householdinc=="£80,000 - £89,999"] <- 3
data_full$hh_income[data_full$householdinc=="£90,000 - £99,999"] <- 3
data_full$hh_income[data_full$householdinc=="£100,000 - £149,999"] <- 3
data_full$hh_income[data_full$householdinc=="More than £150,000"] <- 3

data_full$hh_income <- factor(data_full$hh_income,
                              levels = c(0,1,2, 3),
                              labels = c("Dk/NA", "Less than 52,000$",
                                         "Between 52,000$ and 105,000$",
                                         "More than 105,000$")) 
#Education
data_full$educ  <- 0
data_full$educ[data_full$Highest.education.level=="N/A"] <- 0
data_full$educ[data_full$Highest.education.level=="No formal qualifications"] <- 0
data_full$educ[data_full$Highest.education.level=="Secondary school/GCSE"] <- 1
data_full$educ[data_full$Highest.education.level=="College/A levels"] <- 2
data_full$educ[data_full$Highest.education.level=="Undergraduate degree (BA/BSc/other)"] <- 3
data_full$educ[data_full$Highest.education.level=="Graduate degree (MA/MSc/MPhil/other)"] <- 4
data_full$educ[data_full$Highest.education.level=="Doctorate degree (PhD/MD/other)"] <- 4

data_full$educ <- factor(data_full$educ,
                         levels = c(0,1,2,3, 4),
                         labels = c("No formal qualification", "Secondary school",
                                    "College", "Undergraduate degree",
                                    "Graduate or Doctorate degree")) 

#install.packages("summarytools")
library("summarytools")
#Age
freq(data_full$age_rec)
#Ethnicity
freq(data_full$ethnicity_short)
#Household income
freq(data_full$hh_income)
#Political affiliation
freq(data_full$pol_affiliation)
#Gender
freq(data_full$sex)
#Ideology
freq(data_full$ideol)



# Table C1 --------------------------------

#install.packages("cjoint)
library("cjoint")
res_attack <-  glm(attack ~ oil + ally  + trade + years  + 
                     gov + person + elections + religion + militar, 
                   data = data_short, family = binomial(link = "probit"))
attack_coef <- summary(res_attack)
attack_coef

res_attack_controls <- glm(attack ~ oil + ally  + trade + years  + 
                             gov + person + elections + religion + militar +
                             sex + age_rec + ethnicity_short  + pol_affiliation +
                             hh_income + educ, 
                           data = data_full,family = binomial(link = "probit"))
attack_coef_c <- summary(res_attack_controls)
attack_coef_c

res_sanction <- glm(sanction ~ oil + ally  + trade + years  + 
                      gov + person + elections + religion + militar, 
                    data = data_full,
                    family = binomial(link = "probit"))
sanction_coef <- summary(res_sanction)
sanction_coef

res_sanction_controls <- glm(sanction ~ oil + ally  + trade + years  + 
                               gov + person + elections + religion + militar +
                               sex + age_rec + ethnicity_short  + pol_affiliation +
                               hh_income + educ, 
                             data = data_full,
                             family = binomial(link = "probit"))
sanction_coef_c <- summary(res_sanction_controls)
sanction_coef_c

res_foreignaid <- glm(foreignaid ~ oil + ally  + trade + years  + 
                        gov + person + elections + religion + militar, 
                      data = data_full,
                      family = binomial(link = "probit"))
foreignaid_coef <- summary(res_foreignaid)
foreignaid_coef

res_foreignaid_controls <- glm(foreignaid ~ oil + ally  + trade + years  + 
                                 gov + person + elections + religion + militar +
                                 sex + age_rec + ethnicity_short  + pol_affiliation +
                                 hh_income + educ, 
                               data = data_full,
                               family = binomial(link = "probit"))
foreignaid_coef_c <- summary(res_foreignaid_controls)
foreignaid_coef_c


# Figure C1 ---------------------------------------------------------------


data_full$weights <- 0

data_full$weights[data_full$ideol=="Conservative"] <- 0.278
data_full$weights[data_full$ideol=="Moderate"] <- 0.3449
data_full$weights[data_full$ideol=="Liberal"] <- 0.3770

#The analysis can also be run with weights for ethnicity. 
#data_full$weights_ethnicity
#data_full$weights_ethnicity[data_full$ideol=="Caucasian"] <- 0.613
#data_full$weights_ethnicity[data_full$ideol=="African"] <- 0.133
#data_full$weights_ethnicity[data_full$ideol=="Latino/Hispanic"] <- 0.178
#data_full$weights_ethnicity[data_full$ideol=="Rest"] <- 0.76


res_attack_w <- amce(attack ~ oil + ally  + trade + years  + 
                       gov + person + elections + religion + militar, 
                     data = data_full[!is.na(data_full[["attack"]]), ],
                     weights = "weights",
                     respondent.id = "ID")
res_sanction_w <- amce(sanction ~ oil + ally  + trade + years  + 
                         gov + person + elections + religion + militar, 
                       data = data_full[!is.na(data_full[["sanction"]]), ],
                       weights = "weights",
                       respondent.id = "ID")
res_foreignaid_w <- amce(foreignaid ~ oil + ally  + trade + years  + 
                           gov + person + elections + religion + militar, 
                         data = data_full[!is.na(data_full[["foreignaid"]]), ],
                         weights = "weights",
                         respondent.id = "ID")

#extract the coefficients of each model
attack_coef <- summary(res_attack_w)
attack_coef <- attack_coef[["amce"]]
attack_coef$lb <- attack_coef$Estimate - (1.96*attack_coef$`Std. Err`)
attack_coef$ub <- attack_coef$Estimate + (1.96*attack_coef$`Std. Err`)
attack_coef$Level <- as.factor(attack_coef$Level)

myvars <- c("Attribute", "Level", "Estimate", "lb", "ub")
coefs_graph <- attack_coef[myvars]
coefs_graph$outcome <- as.factor(1) 

sanction_coef <- summary(res_sanction_w)
sanction_coef <- sanction_coef[["amce"]]
sanction_coef$lb <- sanction_coef$Estimate - (1.96*sanction_coef$`Std. Err`)
sanction_coef$ub <- sanction_coef$Estimate + (1.96*sanction_coef$`Std. Err`)
sanction_coef$Level <- as.factor(sanction_coef$Level)

myvars <- c("Attribute", "Level", "Estimate", "lb", "ub")
coefs_graph3 <- sanction_coef[myvars]
coefs_graph3$outcome <- as.factor(2) 

foreign_coef <- summary(res_foreignaid_w)
foreign_coef <- foreign_coef[["amce"]]
foreign_coef$lb <- foreign_coef$Estimate - (1.96*foreign_coef$`Std. Err`)
foreign_coef$ub <- foreign_coef$Estimate + (1.96*foreign_coef$`Std. Err`)
foreign_coef$Level <- as.factor(foreign_coef$Level)

myvars <- c("Attribute", "Level", "Estimate", "lb", "ub")
coefs_graph2 <- foreign_coef[myvars]
coefs_graph2$outcome <- as.factor(3) 

coefs_final <- rbind(coefs_graph, coefs_graph2, coefs_graph3)


coefs_final$outcome <- factor(coefs_final$outcome,
                              levels = c(1,2,3),
                              labels = c("Military intervention", 
                                         "Economic sanctions", "Democracy Aid")) 


#Now we need to create different rows for the titles and the baselines:
coefs_final$Level <- paste0('   ', coefs_final$Level)

#create new rows with information to add
Level <- c("Trade relationships",
           "   (B=The country is an important trade partner of the US)",
           "International military alliance",
           "   (B=The country is a US ally)",
           "Natural resources",
           "   (B=Non-oil exporting country)",
           "Political power and policy are controlled by...",
           "   (B=A group of top regime officials (a council, party comittee or junta))",
           "The regime's leader is...",
           "   (B=A civilian who heads the regime's official party)",
           "Does the regime hold elections?",
           "   (B=No)",
           "Years the regime has been in power...",
           "   (B=4 years)",
           "The country is predominantly...",
           "   (B=Christian)",
           "The regime is militarily...",
           "   (B=Weak)")

coefs_final <- add_row(coefs_final, Level=Level)
coefs_final$outcome <- as.character(coefs_final$outcome)
coefs_final$outcome[is.na(coefs_final$outcome)] <- "Military intervention"

coefs_final <- add_row(coefs_final, Level=Level)
coefs_final$outcome[is.na(coefs_final$outcome)] <- "Economic sanctions"

coefs_final <- add_row(coefs_final, Level=Level)
coefs_final$outcome[is.na(coefs_final$outcome)] <- "Democracy Aid"

#Relevel the Levels
coefs_final$Level <- as.factor(coefs_final$Level)
coefs_final$levels_ordered <- coefs_final$Level

coefs_final$levels_ordered <- factor(coefs_final$levels_ordered,
                                     levels=c("Political power and policy are controlled by...",
                                              "   (B=A group of top regime officials (a council, party comittee or junta))",
                                              "   A single unconstrained individual (the leader)",
                                              "The regime's leader is...",
                                              "   (B=A civilian who heads the regime's official party)",
                                              "   A monarch",
                                              "   A member of the military",
                                              "Does the regime hold elections?",
                                              "   (B=No)",
                                              "   Yes but only regime candidates can run",
                                              "   Yes and some opposition parties are allowed to run",
                                              "Years the regime has been in power...",
                                              "   (B=4 years)",
                                              "   10 years",
                                              "   25 years",
                                              "The country is predominantly...",
                                              "   (B=Christian)",
                                              "   Buddhist",
                                              "   Muslim",
                                              "The regime is militarily...",
                                              "   (B=Weak)",
                                              "   Strong" ,
                                              "Natural resources",
                                              "   (B=Non-oil exporting country)",
                                              "   Oil-exporting country" ,
                                              "International military alliance",
                                              "   (B=The country is a US ally)" ,
                                              "   The country is NOT a US ally",
                                              "Trade relationships",
                                              "   (B=The country is an important trade partner of the US)",
                                              "   The country is NOT an important trade partner of the US"))

coefs_final$levels_ordered <- factor(coefs_final$levels_ordered, 
                                     levels=rev(levels(coefs_final$levels_ordered)))


coefs_final$outcome <- factor(coefs_final$outcome,
                              levels = c("Military intervention", 
                                         "Economic sanctions", "Democracy Aid"))



ggplot(coefs_final) +
  geom_point(aes(y=Estimate, x=levels_ordered, color=outcome, group=outcome), 
             position = position_dodge(width = 0.5)) +
  geom_pointrange(aes(y=Estimate, x=levels_ordered, ymin=lb,ymax=ub, color=outcome, group=outcome), 
                  position = position_dodge(width = 0.5)) +
  coord_flip() +
  theme_bw() +
  geom_hline(yintercept=0,linetype=4, colour="black") +
  facet_grid( ~ outcome, drop=T ) +
  theme(legend.position="none",
        axis.text.y = element_text(hjust = 0),
        axis.ticks.y=element_blank()) +
  xlab("") 



# Figure C2 ---------------------------------------------------------------

#US would be better off if we just stayed home (1) otherwise
data_full$stay_home <- 0
data_full$stay_home[data_full$promoting_1==1] <- 1
data_full$stay_home[data_full$promoting_1==2] <- 1
data_full$stay_home <- as.factor(data_full$stay_home)
data_full$stay_home <- factor(data_full$stay_home,
                              levels = c(0, 1),
                              labels = c("Rest", "The U.S. would be better off if we just stayed home")) 
data_full$stay_home <- relevel(data_full$stay_home, ref = "Rest")


#us should NOT use military force against other countries (1) otherwise
data_full$mil_force <- 0
data_full$mil_force[data_full$promoting_2==1] <- 1
data_full$mil_force[data_full$promoting_2==2] <- 1
data_full$mil_force <- as.factor(data_full$mil_force)
data_full$mil_force <- factor(data_full$mil_force,
                              levels = c(0, 1),
                              labels = c("Rest ", "The U.S. should NOT use military force against other countries because it only makes problems worse")) 
data_full$mil_force <- relevel(data_full$mil_force, ref = "Rest ")

#us should promote democracies to improve human rights (1) otherwise
data_full$prom_dem_hr <- 0
data_full$prom_dem_hr[data_full$promoting2_1==1] <- 1
data_full$prom_dem_hr[data_full$promoting2_1==2] <- 1
data_full$prom_dem_hr <- as.factor(data_full$prom_dem_hr)
data_full$prom_dem_hr <- factor(data_full$prom_dem_hr,
                                levels = c(0, 1),
                                labels = c("Rest  ", "The U.S. should promote democracies to improve human rights")) 
data_full$prom_dem_hr <- relevel(data_full$prom_dem_hr, ref = "Rest  ")

#us should promote democracies to have more reliable allies (1) otherwise
data_full$prom_dem_allies <- 0
data_full$prom_dem_allies[data_full$promoting2_2==1] <- 1
data_full$prom_dem_allies[data_full$promoting2_2==2] <- 1
data_full$prom_dem_allies <- as.factor(data_full$prom_dem_allies)
data_full$prom_dem_allies <- factor(data_full$prom_dem_allies,
                                    levels = c(0, 1),
                                    labels = c("Rest   ", "The U.S. should promote democracies because democracies make for more reliable allies")) 
data_full$prom_dem_allies <- relevel(data_full$prom_dem_allies, ref = "Rest   ")

#Models
res_attack_atti <- amce(attack ~ stay_home + mil_force + prom_dem_hr + prom_dem_allies +
                          sex + age_rec + ethnicity_short  + ideol +
                          hh_income + educ, 
                        data = data_full,
                        respondent.id = "ID")
res_sanction_atti <- amce(sanction ~ stay_home + mil_force + prom_dem_hr + prom_dem_allies +
                            sex + age_rec + ethnicity_short  + ideol +
                            hh_income + educ, 
                          data = data_full,
                          respondent.id = "ID")
res_foreign_atti <- amce(foreignaid ~ stay_home + mil_force + prom_dem_hr + prom_dem_allies +
                           sex + age_rec + ethnicity_short  + ideol +
                           hh_income + educ, 
                         data = data_full,
                         respondent.id = "ID")

#extract the coefficients of each model
attack_coef <- summary(res_attack_atti)
attack_coef <- attack_coef[["amce"]]
attack_coef$lb <- attack_coef$Estimate - (1.96*attack_coef$`Std. Err`)
attack_coef$ub <- attack_coef$Estimate + (1.96*attack_coef$`Std. Err`)
attack_coef$Level <- as.factor(attack_coef$Level)

myvars <- c("Attribute", "Level", "Estimate", "lb", "ub")
coefs_graph <- attack_coef[myvars]
coefs_graph$outcome <- "Military intervention"

sanction_coef <- summary(res_sanction_atti)
sanction_coef <- sanction_coef[["amce"]]
sanction_coef$lb <- sanction_coef$Estimate - (1.96*sanction_coef$`Std. Err`)
sanction_coef$ub <- sanction_coef$Estimate + (1.96*sanction_coef$`Std. Err`)
sanction_coef$Level <- as.factor(sanction_coef$Level)

myvars <- c("Attribute", "Level", "Estimate", "lb", "ub")
coefs_graph3 <- sanction_coef[myvars]
coefs_graph3$outcome <- "Economic sanctions"

foreign_coef <- summary(res_foreign_atti)
foreign_coef <- foreign_coef[["amce"]]
foreign_coef$lb <- foreign_coef$Estimate - (1.96*foreign_coef$`Std. Err`)
foreign_coef$ub <- foreign_coef$Estimate + (1.96*foreign_coef$`Std. Err`)
foreign_coef$Level <- as.factor(foreign_coef$Level)

myvars <- c("Attribute", "Level", "Estimate", "lb", "ub")
coefs_graph2 <- foreign_coef[myvars]
coefs_graph2$outcome <- "Democracy Aid"

coefs_final <- rbind(coefs_graph, coefs_graph2, coefs_graph3)

#Now we need to create different rows for the titles and the baselines:
#we add blank space in the levels
coefs_final$Level <- paste0('   ', coefs_final$Level)

#we now create new rows with information to add
Level <- c("Age",
           "   (B=Less than 25)",
           "Ethnicity",
           "   (B=Others)",
           "Household income",
           "   (B=Dk/Rather not say)",
           "Ideology",
           "   (B=Conservative)",
           "Isolationism",
           "   (B=Rest)",
           "Democracies as allies",
           "   (B=Rest)   ",
           "Promote human rights",
           "   (B=Rest)  ",
           "Gender",
           "   (B=Female)",
           "Military force",
           "   (B=Rest)     ",
           "Education",
           "   (B=No formal qualification)   ")

coefs_final <- add_row(coefs_final, Level=Level)
coefs_final$outcome <- as.character(coefs_final$outcome)
coefs_final$outcome[is.na(coefs_final$outcome)] <- "Military intervention"

coefs_final <- add_row(coefs_final, Level=Level)
coefs_final$outcome[is.na(coefs_final$outcome)] <- "Economic sanctions"

coefs_final <- add_row(coefs_final, Level=Level)
coefs_final$outcome[is.na(coefs_final$outcome)] <- "Democracy Aid"

#Relevel the Levels
coefs_final$Level <- as.factor(coefs_final$Level)
coefs_final$levels_ordered <- coefs_final$Level

coefs_final$levels_ordered <- factor(coefs_final$levels_ordered,
                                     levels=c("Isolationism",
                                              "   (B=Rest)",
                                              "   The U.S. would be better off if we just stayed home",
                                              "Military force",
                                              "   (B=Rest)     " ,
                                              "   The U.S. should NOT use military force against other countries because it only makes problems worse",
                                              "Promote human rights",
                                              "   (B=Rest)  ",
                                              "   The U.S. should promote democracies to improve human rights" ,
                                              "Democracies as allies",
                                              "   (B=Rest)   ",
                                              "   The U.S. should promote democracies because democracies make for more reliable allies" ,
                                              "Age",
                                              "   (B=Less than 25)",
                                              "   25-34",
                                              "   35-54",
                                              "   55-77",
                                              "Ethnicity",
                                              "   (B=Others)",
                                              "   Caucasian",
                                              "   African",
                                              "   Latino/Hispanic",
                                              "   East Asian",
                                              "Household income",
                                              "   (B=Dk/Rather not say)",
                                              "   Less than 52,000$",
                                              "   Between 52,000$ and 105,000$",
                                              "   More than 105,000$",
                                              "Ideology",
                                              "   (B=Conservative)",
                                              "   Moderate",
                                              "   Liberal",
                                              "Gender",
                                              "   (B=Female)",
                                              "   Male" ,
                                              "Education",
                                              "   (B=No formal qualification)   ",
                                              "   Secondary school",
                                              "   College", 
                                              "   Undergraduate degree",
                                              "   Graduate or Doctorate degree" ))

coefs_final$levels_ordered <- factor(coefs_final$levels_ordered, 
                                     levels=rev(levels(coefs_final$levels_ordered)))

coefs_final$outcome <- factor(coefs_final$outcome,
                              levels = c("Military intervention", 
                                         "Economic sanctions", "Democracy Aid"))


ggplot(coefs_final) +
  geom_point(aes(y=Estimate, x=levels_ordered, color=outcome, group=outcome), position = position_dodge(width = 0.5)) +
  geom_pointrange(aes(y=Estimate, x=levels_ordered, ymin=lb,ymax=ub, color=outcome, group=outcome), position = position_dodge(width = 0.5)) +
  coord_flip() +
  theme_bw() +
  geom_hline(yintercept=0,linetype=4, colour="black") +
  facet_grid( ~ outcome, drop=T ) +
  theme(legend.position="none",
        axis.text.y = element_text(hjust = 0),
        axis.ticks.y=element_blank()) +
  xlab("") 



# Table C2 ----------------------------------------------------------------

#install.packages("foreign")
#install.packages("systemfit")
library("foreign")
library("systemfit")

#AMCE
res_attack <- amce(attack ~ oil + ally  + trade + years  + 
                     gov + person + elections + religion + militar, 
                   data = data_full,
                   respondent.id = "ID")
attack_coef <- summary(res_attack)
attack_coef

res_sanction <- amce(sanction ~ oil + ally  + trade + years  + 
                       gov + person + elections + religion + militar, 
                     data = data_full,
                     respondent.id = "ID")
sanction_coef <- summary(res_sanction)
sanction_coef

res_foreignaid <- amce(foreignaid ~ oil + ally  + trade + years  + 
                         gov + person + elections + religion + militar, 
                       data = data_full,
                       respondent.id = "ID")
foreignaid_coef <- summary(res_foreignaid)
foreignaid_coef

#SUR
gen_attack_eq <- attack~oil + ally  + trade + years  + 
  gov + person + elections + religion + militar
gen_sanction_eq <- sanction~oil + ally  + trade + years  + 
  gov + person + elections + religion + militar
gen_foreignaid_eq <- foreignaid~oil + ally  + trade + years  + 
  gov + person + elections + religion + militar

fitsur <- systemfit(list(atreg=gen_attack_eq, 
                         sanreg=gen_sanction_eq, 
                         forreg=gen_foreignaid_eq), "SUR",
                    data=data_short)

summary(fitsur)


# Figure C3a --------------------------------------------------------------

res_attack_ideol <- amce(attack ~ ideol * (oil + ally  + trade + years  + 
                                             gov + person + elections + religion + militar), 
                         data = data_short,
                         respondent.varying = "ideol",  
                         respondent.id = "ID")
res_sanction_ideol <- amce(sanction ~ ideol * (oil + ally  + trade + years  + 
                                                 gov + person + elections + religion + militar), 
                           data = data_short,
                           respondent.varying = "ideol",  
                           respondent.id = "ID")
res_foreignaid_ideol <- amce(foreignaid ~ ideol * (oil + ally  + trade + years  + 
                                                     gov + person + elections + religion + militar), 
                             data = data_short,
                             respondent.varying = "ideol",  
                             respondent.id = "ID")

attack_coef <- summary(res_attack_ideol)
attack_coef_right <- attack_coef[["ideol1amce"]]
attack_coef_centre <- attack_coef[["ideol2amce"]]
attack_coef_left <- attack_coef[["ideol3amce"]]

attack_coef_left$ideol <- 1
attack_coef_centre$ideol <- 2
attack_coef_right$ideol <- 3

attack_coef <- rbind(attack_coef_left, attack_coef_centre, attack_coef_right)

attack_coef$lb <- attack_coef$Estimate - (1.96*attack_coef$`Std. Err`)
attack_coef$ub <- attack_coef$Estimate + (1.96*attack_coef$`Std. Err`)
attack_coef$Level <- as.factor(attack_coef$Level)

myvars <- c("Attribute", "Level", "Estimate", "lb", "ub", "ideol")
coefs_graph <- attack_coef[myvars]

#Now we need to create different rows for the titles and the baselines:
coefs_graph$Level <- paste0('   ', coefs_graph$Level)

#create new rows with information to add
Level <- c("Trade relationships",
           "   (B=The country is an important trade partner of the US)",
           "International military alliance",
           "   (B=The country is a US ally)",
           "Natural resources",
           "   (B=Non-oil exporting country)",
           "Political power and policy are controlled by...",
           "   (B=A group of top regime officials (a council, party comittee or junta))",
           "The regime's leader is...",
           "   (B=A civilian who heads the regime's official party)",
           "Does the regime hold elections?",
           "   (B=No)",
           "Years the regime has been in power...",
           "   (B=4 years)",
           "The country is predominantly...",
           "   (B=Christian)",
           "The regime is militarily...",
           "   (B=Weak)")

coefs_graph <- add_row(coefs_graph, Level=Level)
coefs_graph$ideol[is.na(coefs_graph$ideol)] <- 1
coefs_graph <- add_row(coefs_graph, Level=Level)
coefs_graph$ideol[is.na(coefs_graph$ideol)] <- 2
coefs_graph <- add_row(coefs_graph, Level=Level)
coefs_graph$ideol[is.na(coefs_graph$ideol)] <- 3

#Relevel the Levels
coefs_graph$Level <- as.factor(coefs_graph$Level)
coefs_graph$levels_ordered <- coefs_graph$Level

levels_ordered <- factor(coefs_graph$levels_ordered,
                         levels=c("Political power and policy are controlled by...",
                                  "   (B=A group of top regime officials (a council, party comittee or junta))",
                                  "   A single unconstrained individual (the leader)",
                                  "The regime's leader is...",
                                  "   (B=A civilian who heads the regime's official party)",
                                  "   A monarch",
                                  "   A member of the military",
                                  "Does the regime hold elections?",
                                  "   (B=No)",
                                  "   Yes but only regime candidates can run",
                                  "   Yes and some opposition parties are allowed to run",
                                  "Years the regime has been in power...",
                                  "   (B=4 years)",
                                  "   10 years",
                                  "   25 years",
                                  "The country is predominantly...",
                                  "   (B=Christian)",
                                  "   Buddhist",
                                  "   Muslim",
                                  "The regime is militarily...",
                                  "   (B=Weak)",
                                  "   Strong" ,
                                  "Natural resources",
                                  "   (B=Non-oil exporting country)",
                                  "   Oil-exporting country" ,
                                  "International military alliance",
                                  "   (B=The country is a US ally)" ,
                                  "   The country is NOT a US ally",
                                  "Trade relationships",
                                  "   (B=The country is an important trade partner of the US)",
                                  "   The country is NOT an important trade partner of the US"))

coefs_graph$levels_ordered <- levels_ordered
coefs_graph$levels_ordered <- factor(coefs_graph$levels_ordered, 
                                     levels=rev(levels(coefs_graph$levels_ordered)))

coefs_graph$ideol <- factor(coefs_graph$ideol,
                            levels = c(1,2,3),
                            labels = c("Liberal", "Moderate", "Conservative")) 



ggplot(coefs_graph) +
  geom_point(aes(y=Estimate, x=levels_ordered, color=ideol, group=ideol), position = position_dodge(width = 0.5)) +
  geom_pointrange(aes(y=Estimate, x=levels_ordered, ymin=lb,ymax=ub, color=ideol, group=ideol), position = position_dodge(width = 0.5)) +
  coord_flip() +
  theme_bw() +
  geom_hline(yintercept=0,linetype=4, colour="black") +
  facet_grid( ~ ideol, drop=T ) +
  theme(legend.position="none",
        axis.text.y = element_text(hjust = 0),
        axis.ticks.y=element_blank()) +
  xlab("") 

# Figure C3b --------------------------------------------------------------

sanction_coef <- summary(res_sanction_ideol)
sanction_coef_right <- sanction_coef[["ideol1amce"]]
sanction_coef_centre <- sanction_coef[["ideol2amce"]]
sanction_coef_left <- sanction_coef[["ideol3amce"]]

sanction_coef_left$ideol <- 1
sanction_coef_centre$ideol <- 2
sanction_coef_right$ideol <- 3

sanction_coef <- rbind(sanction_coef_left, sanction_coef_centre, sanction_coef_right)

sanction_coef$lb <- sanction_coef$Estimate - (1.96*sanction_coef$`Std. Err`)
sanction_coef$ub <- sanction_coef$Estimate + (1.96*sanction_coef$`Std. Err`)
sanction_coef$Level <- as.factor(sanction_coef$Level)

myvars <- c("Attribute", "Level", "Estimate", "lb", "ub", "ideol")
coefs_graph2 <- sanction_coef[myvars]

#Now we need to create different rows for the titles and the baselines:
coefs_graph2$Level <- paste0('   ', coefs_graph2$Level)

Level <- c("Trade relationships",
           "   (B=The country is an important trade partner of the US)",
           "International military alliance",
           "   (B=The country is a US ally)",
           "Natural resources",
           "   (B=Non-oil exporting country)",
           "Political power and policy are controlled by...",
           "   (B=A group of top regime officials (a council, party comittee or junta))",
           "The regime's leader is...",
           "   (B=A civilian who heads the regime's official party)",
           "Does the regime hold elections?",
           "   (B=No)",
           "Years the regime has been in power...",
           "   (B=4 years)",
           "The country is predominantly...",
           "   (B=Christian)",
           "The regime is militarily...",
           "   (B=Weak)")

coefs_graph2 <- add_row(coefs_graph2, Level=Level)
coefs_graph2$ideol[is.na(coefs_graph2$ideol)] <- 1
coefs_graph2 <- add_row(coefs_graph2, Level=Level)
coefs_graph2$ideol[is.na(coefs_graph2$ideol)] <- 2
coefs_graph2 <- add_row(coefs_graph2, Level=Level)
coefs_graph2$ideol[is.na(coefs_graph2$ideol)] <- 3

#Relevel the Levels
coefs_graph2$Level <- as.factor(coefs_graph2$Level)
coefs_graph2$levels_ordered <- coefs_graph2$Level


levels_ordered <- factor(coefs_graph2$levels_ordered,
                         levels=c("Political power and policy are controlled by...",
                                  "   (B=A group of top regime officials (a council, party comittee or junta))",
                                  "   A single unconstrained individual (the leader)",
                                  "The regime's leader is...",
                                  "   (B=A civilian who heads the regime's official party)",
                                  "   A monarch",
                                  "   A member of the military",
                                  "Does the regime hold elections?",
                                  "   (B=No)",
                                  "   Yes but only regime candidates can run",
                                  "   Yes and some opposition parties are allowed to run",
                                  "Years the regime has been in power...",
                                  "   (B=4 years)",
                                  "   10 years",
                                  "   25 years",
                                  "The country is predominantly...",
                                  "   (B=Christian)",
                                  "   Buddhist",
                                  "   Muslim",
                                  "The regime is militarily...",
                                  "   (B=Weak)",
                                  "   Strong" ,
                                  "Natural resources",
                                  "   (B=Non-oil exporting country)",
                                  "   Oil-exporting country" ,
                                  "International military alliance",
                                  "   (B=The country is a US ally)" ,
                                  "   The country is NOT a US ally",
                                  "Trade relationships",
                                  "   (B=The country is an important trade partner of the US)",
                                  "   The country is NOT an important trade partner of the US"))

coefs_graph2$levels_ordered <- levels_ordered
coefs_graph2$levels_ordered <- factor(coefs_graph2$levels_ordered, 
                                      levels=rev(levels(coefs_graph2$levels_ordered)))

coefs_graph2$ideol <- factor(coefs_graph2$ideol,
                             levels = c(1,2,3),
                             labels = c("Liberal", "Moderate", "Conservative")) 


ggplot(coefs_graph2) +
  geom_point(aes(y=Estimate, x=levels_ordered, color=ideol, group=ideol), position = position_dodge(width = 0.5)) +
  geom_pointrange(aes(y=Estimate, x=levels_ordered, ymin=lb,ymax=ub, color=ideol, group=ideol), position = position_dodge(width = 0.5)) +
  coord_flip() +
  theme_bw() +
  geom_hline(yintercept=0,linetype=4, colour="black") +
  facet_grid( ~ ideol, drop=T ) +
  theme(legend.position="none",
        axis.text.y = element_text(hjust = 0),
        axis.ticks.y=element_blank()) +
  xlab("") 


# Figure C3c --------------------------------------------------------------

foreignaid_coef <- summary(res_foreignaid_ideol)
foreignaid_coef_right <- foreignaid_coef[["ideol1amce"]]
foreignaid_coef_centre <- foreignaid_coef[["ideol2amce"]]
foreignaid_coef_left <- foreignaid_coef[["ideol3amce"]]

foreignaid_coef_left$ideol <- 1
foreignaid_coef_centre$ideol <- 2
foreignaid_coef_right$ideol <- 3

foreignaid_coef <- rbind(foreignaid_coef_left, foreignaid_coef_centre, foreignaid_coef_right)

foreignaid_coef$lb <- foreignaid_coef$Estimate - (1.96*foreignaid_coef$`Std. Err`)
foreignaid_coef$ub <- foreignaid_coef$Estimate + (1.96*foreignaid_coef$`Std. Err`)
foreignaid_coef$Level <- as.factor(foreignaid_coef$Level)

myvars <- c("Attribute", "Level", "Estimate", "lb", "ub", "ideol")
coefs_graph3 <- foreignaid_coef[myvars]

#Now we need to create different rows for the titles and the baselines:
coefs_graph3$Level <- paste0('   ', coefs_graph3$Level)

Level <- c("Trade relationships",
           "   (B=The country is an important trade partner of the US)",
           "International military alliance",
           "   (B=The country is a US ally)",
           "Natural resources",
           "   (B=Non-oil exporting country)",
           "Political power and policy are controlled by...",
           "   (B=A group of top regime officials (a council, party comittee or junta))",
           "The regime's leader is...",
           "   (B=A civilian who heads the regime's official party)",
           "Does the regime hold elections?",
           "   (B=No)",
           "Years the regime has been in power...",
           "   (B=4 years)",
           "The country is predominantly...",
           "   (B=Christian)",
           "The regime is militarily...",
           "   (B=Weak)")

coefs_graph3 <- add_row(coefs_graph3, Level=Level)
coefs_graph3$ideol[is.na(coefs_graph3$ideol)] <- 1
coefs_graph3 <- add_row(coefs_graph3, Level=Level)
coefs_graph3$ideol[is.na(coefs_graph3$ideol)] <- 2
coefs_graph3 <- add_row(coefs_graph3, Level=Level)
coefs_graph3$ideol[is.na(coefs_graph3$ideol)] <- 3


#Relevel the Levels
coefs_graph3$Level <- as.factor(coefs_graph3$Level)
coefs_graph3$levels_ordered <- coefs_graph3$Level

levels_ordered <- factor(coefs_graph2$levels_ordered,
                         levels=c("Political power and policy are controlled by...",
                                  "   (B=A group of top regime officials (a council, party comittee or junta))",
                                  "   A single unconstrained individual (the leader)",
                                  "The regime's leader is...",
                                  "   (B=A civilian who heads the regime's official party)",
                                  "   A monarch",
                                  "   A member of the military",
                                  "Does the regime hold elections?",
                                  "   (B=No)",
                                  "   Yes but only regime candidates can run",
                                  "   Yes and some opposition parties are allowed to run",
                                  "Years the regime has been in power...",
                                  "   (B=4 years)",
                                  "   10 years",
                                  "   25 years",
                                  "The country is predominantly...",
                                  "   (B=Christian)",
                                  "   Buddhist",
                                  "   Muslim",
                                  "The regime is militarily...",
                                  "   (B=Weak)",
                                  "   Strong" ,
                                  "Natural resources",
                                  "   (B=Non-oil exporting country)",
                                  "   Oil-exporting country" ,
                                  "International military alliance",
                                  "   (B=The country is a US ally)" ,
                                  "   The country is NOT a US ally",
                                  "Trade relationships",
                                  "   (B=The country is an important trade partner of the US)",
                                  "   The country is NOT an important trade partner of the US"))

coefs_graph3$levels_ordered <- levels_ordered
coefs_graph3$levels_ordered <- factor(coefs_graph3$levels_ordered, 
                                      levels=rev(levels(coefs_graph3$levels_ordered)))

coefs_graph3$ideol <- factor(coefs_graph3$ideol,
                             levels = c(1,2,3),
                             labels = c("Liberal", "Moderate", "Conservative")) 


ggplot(coefs_graph3) +
  geom_point(aes(y=Estimate, x=levels_ordered, color=ideol, group=ideol), position = position_dodge(width = 0.5)) +
  geom_pointrange(aes(y=Estimate, x=levels_ordered, ymin=lb,ymax=ub, color=ideol, group=ideol), position = position_dodge(width = 0.5)) +
  coord_flip() +
  theme_bw() +
  geom_hline(yintercept=0,linetype=4, colour="black") +
  facet_grid( ~ ideol, drop=T ) +
  theme(legend.position="none",
        axis.text.y = element_text(hjust = 0),
        axis.ticks.y=element_blank()) +
  xlab("") 


# Figure C4a --------------------------------------------------------------

res_attack_party <- amce(attack ~ pol_affiliation * (oil + ally  + trade + years  + 
                                                       gov + person + elections + religion + militar), 
                         data = data_full[!is.na(data_full[["attack"]]), ],
                         respondent.varying = "pol_affiliation",  
                         respondent.id = "ID")
res_sanction_party <- amce(sanction ~ pol_affiliation * (oil + ally  + trade + years  + 
                                                           gov + person + elections + religion + militar), 
                           data = data_full[!is.na(data_full[["sanction"]]), ],
                           respondent.varying = "pol_affiliation",  
                           respondent.id = "ID")
res_foreignaid_party <- amce(foreignaid ~ pol_affiliation * (oil + ally  + trade + years  + 
                                                               gov + person + elections + religion + militar), 
                             data = data_full[!is.na(data_full[["foreignaid"]]), ],
                             respondent.varying = "pol_affiliation",  
                             respondent.id = "ID")


#extract the coefficients of each model
attack_coef <- summary(res_attack_party)
attack_coef_rest <- attack_coef[["polaffiliation1amce"]]
attack_coef_democrat <- attack_coef[["polaffiliation2amce"]]
attack_coef_republican <- attack_coef[["polaffiliation3amce"]]

attack_coef_rest$party <- 1
attack_coef_democrat$party <- 2
attack_coef_republican$party <- 3

attack_coef <- rbind(attack_coef_rest, attack_coef_democrat, attack_coef_republican)

attack_coef$lb <- attack_coef$Estimate - (1.96*attack_coef$`Std. Err`)
attack_coef$ub <- attack_coef$Estimate + (1.96*attack_coef$`Std. Err`)
attack_coef$Level <- as.factor(attack_coef$Level)

myvars <- c("Attribute", "Level", "Estimate", "lb", "ub", "party")
coefs_graph <- attack_coef[myvars]

#Now we need to create different rows for the titles and the baselines:
coefs_graph$Level <- paste0('   ', coefs_graph$Level)

Level <- c("Trade relationships",
           "   (B=The country is an important trade partner of the US)",
           "International military alliance",
           "   (B=The country is a US ally)",
           "Natural resources",
           "   (B=Non-oil exporting country)",
           "Political power and policy are controlled by...",
           "   (B=A group of top regime officials (a council, party comittee or junta))",
           "The regime's leader is...",
           "   (B=A civilian who heads the regime's official party)",
           "Does the regime hold elections?",
           "   (B=No)",
           "Years the regime has been in power...",
           "   (B=4 years)",
           "The country is predominantly...",
           "   (B=Christian)",
           "The regime is militarily...",
           "   (B=Weak)")

coefs_graph <- add_row(coefs_graph, Level=Level)
coefs_graph$party[is.na(coefs_graph$party)] <- 1
coefs_graph <- add_row(coefs_graph, Level=Level)
coefs_graph$party[is.na(coefs_graph$party)] <- 2
coefs_graph <- add_row(coefs_graph, Level=Level)
coefs_graph$party[is.na(coefs_graph$party)] <- 3

#Relevel the Levels
coefs_graph$Level <- as.factor(coefs_graph$Level)
coefs_graph$levels_ordered <- coefs_graph$Level

levels_ordered <- factor(coefs_graph$levels_ordered,
                         levels=c("Political power and policy are controlled by...",
                                  "   (B=A group of top regime officials (a council, party comittee or junta))",
                                  "   A single unconstrained individual (the leader)",
                                  "The regime's leader is...",
                                  "   (B=A civilian who heads the regime's official party)",
                                  "   A monarch",
                                  "   A member of the military",
                                  "Does the regime hold elections?",
                                  "   (B=No)",
                                  "   Yes but only regime candidates can run",
                                  "   Yes and some opposition parties are allowed to run",
                                  "Years the regime has been in power...",
                                  "   (B=4 years)",
                                  "   10 years",
                                  "   25 years",
                                  "The country is predominantly...",
                                  "   (B=Christian)",
                                  "   Buddhist",
                                  "   Muslim",
                                  "The regime is militarily...",
                                  "   (B=Weak)",
                                  "   Strong" ,
                                  "Natural resources",
                                  "   (B=Non-oil exporting country)",
                                  "   Oil-exporting country" ,
                                  "International military alliance",
                                  "   (B=The country is a US ally)" ,
                                  "   The country is NOT a US ally",
                                  "Trade relationships",
                                  "   (B=The country is an important trade partner of the US)",
                                  "   The country is NOT an important trade partner of the US"))

coefs_graph$levels_ordered <- levels_ordered
coefs_graph$levels_ordered <- factor(coefs_graph$levels_ordered, 
                                     levels=rev(levels(coefs_graph$levels_ordered)))

coefs_graph$party <- factor(coefs_graph$party,
                            levels = c(1,2,3),
                            labels = c("Rest", "Democrat", "Republican")) 


ggplot(coefs_graph) +
  geom_point(aes(y=Estimate, x=levels_ordered, color=party, group=party), position = position_dodge(width = 0.5)) +
  geom_pointrange(aes(y=Estimate, x=levels_ordered, ymin=lb,ymax=ub, color=party, group=party), position = position_dodge(width = 0.5)) +
  coord_flip() +
  theme_bw() +
  geom_hline(yintercept=0,linetype=4, colour="black") +
  facet_grid( ~ party, drop=T ) +
  theme(legend.position="none",
        axis.text.y = element_text(hjust = 0),
        axis.ticks.y=element_blank()) +
  xlab("") 


# Figure C4b --------------------------------------------------------------


#extract the coefficients of each model
sanction_coef <- summary(res_sanction_party)
sanction_coef_rest <- sanction_coef[["polaffiliation1amce"]]
sanction_coef_democrat <- sanction_coef[["polaffiliation2amce"]]
sanction_coef_republican <- sanction_coef[["polaffiliation3amce"]]

sanction_coef_rest$party <- 1
sanction_coef_democrat$party <- 2
sanction_coef_republican$party <- 3

sanction_coef <- rbind(sanction_coef_rest, sanction_coef_democrat, sanction_coef_republican)

sanction_coef$lb <- sanction_coef$Estimate - (1.96*sanction_coef$`Std. Err`)
sanction_coef$ub <- sanction_coef$Estimate + (1.96*sanction_coef$`Std. Err`)
sanction_coef$Level <- as.factor(sanction_coef$Level)

myvars <- c("Attribute", "Level", "Estimate", "lb", "ub", "party")
coefs_graph <- sanction_coef[myvars]

#Now we need to create different rows for the titles and the baselines:
coefs_graph$Level <- paste0('   ', coefs_graph$Level)

Level <- c("Trade relationships",
           "   (B=The country is an important trade partner of the US)",
           "International military alliance",
           "   (B=The country is a US ally)",
           "Natural resources",
           "   (B=Non-oil exporting country)",
           "Political power and policy are controlled by...",
           "   (B=A group of top regime officials (a council, party comittee or junta))",
           "The regime's leader is...",
           "   (B=A civilian who heads the regime's official party)",
           "Does the regime hold elections?",
           "   (B=No)",
           "Years the regime has been in power...",
           "   (B=4 years)",
           "The country is predominantly...",
           "   (B=Christian)",
           "The regime is militarily...",
           "   (B=Weak)")


coefs_graph <- add_row(coefs_graph, Level=Level)
coefs_graph$party[is.na(coefs_graph$party)] <- 1
coefs_graph <- add_row(coefs_graph, Level=Level)
coefs_graph$party[is.na(coefs_graph$party)] <- 2
coefs_graph <- add_row(coefs_graph, Level=Level)
coefs_graph$party[is.na(coefs_graph$party)] <- 3


#Relevel the Levels
coefs_graph$Level <- as.factor(coefs_graph$Level)
coefs_graph$levels_ordered <- coefs_graph$Level

levels_ordered <- factor(coefs_graph$levels_ordered,
                         levels=c("Political power and policy are controlled by...",
                                  "   (B=A group of top regime officials (a council, party comittee or junta))",
                                  "   A single unconstrained individual (the leader)",
                                  "The regime's leader is...",
                                  "   (B=A civilian who heads the regime's official party)",
                                  "   A monarch",
                                  "   A member of the military",
                                  "Does the regime hold elections?",
                                  "   (B=No)",
                                  "   Yes but only regime candidates can run",
                                  "   Yes and some opposition parties are allowed to run",
                                  "Years the regime has been in power...",
                                  "   (B=4 years)",
                                  "   10 years",
                                  "   25 years",
                                  "The country is predominantly...",
                                  "   (B=Christian)",
                                  "   Buddhist",
                                  "   Muslim",
                                  "The regime is militarily...",
                                  "   (B=Weak)",
                                  "   Strong" ,
                                  "Natural resources",
                                  "   (B=Non-oil exporting country)",
                                  "   Oil-exporting country" ,
                                  "International military alliance",
                                  "   (B=The country is a US ally)" ,
                                  "   The country is NOT a US ally",
                                  "Trade relationships",
                                  "   (B=The country is an important trade partner of the US)",
                                  "   The country is NOT an important trade partner of the US"))

coefs_graph$levels_ordered <- levels_ordered
coefs_graph$levels_ordered <- factor(coefs_graph$levels_ordered, 
                                     levels=rev(levels(coefs_graph$levels_ordered)))

coefs_graph$party <- factor(coefs_graph$party,
                            levels = c(1,2,3),
                            labels = c("Rest", "Democrat", "Republican")) 



ggplot(coefs_graph) +
  geom_point(aes(y=Estimate, x=levels_ordered, color=party, group=party), position = position_dodge(width = 0.5)) +
  geom_pointrange(aes(y=Estimate, x=levels_ordered, ymin=lb,ymax=ub, color=party, group=party), position = position_dodge(width = 0.5)) +
  coord_flip() +
  theme_bw() +
  geom_hline(yintercept=0,linetype=4, colour="black") +
  facet_grid( ~ party, drop=T ) +
  theme(legend.position="none",
        axis.text.y = element_text(hjust = 0),
        axis.ticks.y=element_blank()) +
  xlab("") 


# Figure C4c --------------------------------------------------------------

#extract the coefficients of each model
foreign_coef <- summary(res_foreignaid_party)
foreign_coef_rest <- foreign_coef[["polaffiliation1amce"]]
foreign_coef_democrat <- foreign_coef[["polaffiliation2amce"]]
foreign_coef_republican <- foreign_coef[["polaffiliation3amce"]]

foreign_coef_rest$party <- 1
foreign_coef_democrat$party <- 2
foreign_coef_republican$party <- 3

foreign_coef <- rbind(foreign_coef_rest, foreign_coef_democrat, foreign_coef_republican)

foreign_coef$lb <- foreign_coef$Estimate - (1.96*foreign_coef$`Std. Err`)
foreign_coef$ub <- foreign_coef$Estimate + (1.96*foreign_coef$`Std. Err`)
foreign_coef$Level <- as.factor(foreign_coef$Level)

myvars <- c("Attribute", "Level", "Estimate", "lb", "ub", "party")
coefs_graph <- foreign_coef[myvars]

#Now we need to create different rows for the titles and the baselines:
coefs_graph$Level <- paste0('   ', coefs_graph$Level)

Level <- c("Trade relationships",
           "   (B=The country is an important trade partner of the US)",
           "International military alliance",
           "   (B=The country is a US ally)",
           "Natural resources",
           "   (B=Non-oil exporting country)",
           "Political power and policy are controlled by...",
           "   (B=A group of top regime officials (a council, party comittee or junta))",
           "The regime's leader is...",
           "   (B=A civilian who heads the regime's official party)",
           "Does the regime hold elections?",
           "   (B=No)",
           "Years the regime has been in power...",
           "   (B=4 years)",
           "The country is predominantly...",
           "   (B=Christian)",
           "The regime is militarily...",
           "   (B=Weak)")

coefs_graph <- add_row(coefs_graph, Level=Level)
coefs_graph$party[is.na(coefs_graph$party)] <- 1
coefs_graph <- add_row(coefs_graph, Level=Level)
coefs_graph$party[is.na(coefs_graph$party)] <- 2
coefs_graph <- add_row(coefs_graph, Level=Level)
coefs_graph$party[is.na(coefs_graph$party)] <- 3


#Relevel the Levels
coefs_graph$Level <- as.factor(coefs_graph$Level)
coefs_graph$levels_ordered <- coefs_graph$Level

levels_ordered <- factor(coefs_graph$levels_ordered,
                         levels=c("Political power and policy are controlled by...",
                                  "   (B=A group of top regime officials (a council, party comittee or junta))",
                                  "   A single unconstrained individual (the leader)",
                                  "The regime's leader is...",
                                  "   (B=A civilian who heads the regime's official party)",
                                  "   A monarch",
                                  "   A member of the military",
                                  "Does the regime hold elections?",
                                  "   (B=No)",
                                  "   Yes but only regime candidates can run",
                                  "   Yes and some opposition parties are allowed to run",
                                  "Years the regime has been in power...",
                                  "   (B=4 years)",
                                  "   10 years",
                                  "   25 years",
                                  "The country is predominantly...",
                                  "   (B=Christian)",
                                  "   Buddhist",
                                  "   Muslim",
                                  "The regime is militarily...",
                                  "   (B=Weak)",
                                  "   Strong" ,
                                  "Natural resources",
                                  "   (B=Non-oil exporting country)",
                                  "   Oil-exporting country" ,
                                  "International military alliance",
                                  "   (B=The country is a US ally)" ,
                                  "   The country is NOT a US ally",
                                  "Trade relationships",
                                  "   (B=The country is an important trade partner of the US)",
                                  "   The country is NOT an important trade partner of the US"))

coefs_graph$levels_ordered <- levels_ordered
coefs_graph$levels_ordered <- factor(coefs_graph$levels_ordered, 
                                     levels=rev(levels(coefs_graph$levels_ordered)))

coefs_graph$party <- factor(coefs_graph$party,
                            levels = c(1,2,3),
                            labels = c("Rest", "Democrat", "Republican")) 



ggplot(coefs_graph) +
  geom_point(aes(y=Estimate, x=levels_ordered, color=party, group=party), position = position_dodge(width = 0.5)) +
  geom_pointrange(aes(y=Estimate, x=levels_ordered, ymin=lb,ymax=ub, color=party, group=party), position = position_dodge(width = 0.5)) +
  coord_flip() +
  theme_bw() +
  geom_hline(yintercept=0,linetype=4, colour="black") +
  facet_grid( ~ party, drop=T ) +
  theme(legend.position="none",
        axis.text.y = element_text(hjust = 0),
        axis.ticks.y=element_blank()) +
  xlab("") 


# Figure C5 ---------------------------------------------------------------

#install.packages("asbio")
#install.packages("clusterSEs")
library("asbio")
library("clusterSEs")

res_attack <- lm(attack ~ oil + ally  + trade + years  + 
                   gov + person + elections + religion + militar, 
                 data = data_short)
attackr <- joint.ci.bonf(res_attack)

attackr$prov <- as.factor(rownames(attackr))
attackr <- subset(attackr, prov!= "(Intercept)")

attackr$Level <- c("Oil-exporting country", "The country is NOT a US ally",
                   "The country is NOT an important trade partner of the US",
                   "10 years", "25 years", "A single unconstrained individual (the leader)",
                   "A member of the military", "A monarch", "Yes and some opposition parties are allowed to run",
                   "Yes but only regime candidates can run", "Buddhist", "Muslim",
                   "Strong")

rownames(attackr) <- c()
names(attackr)[names(attackr) == '2.5 %'] <- 'lb'
names(attackr)[names(attackr) == '97.5 %'] <- 'ub'


attackr$Attribute <- c("oil", "ally", "trade", "years", "years",
                       "gov", "person", "person", "elections",
                       "elections", "religion", "religion", "militar")

myvars <- c("Attribute", "Level", "lb", "ub")
coefs_graph <- attackr[myvars]
coefs_graph$outcome <- as.factor(1) 


res_sanction <- lm(sanction ~ oil + ally  + trade + years  + 
                     gov + person + elections + religion + militar, 
                   data = data_short)
sanctionr <- joint.ci.bonf(res_sanction)

sanctionr$prov <- as.factor(rownames(sanctionr))
sanctionr <- subset(sanctionr, prov!= "(Intercept)")

sanctionr$Level <- c("Oil-exporting country", "The country is NOT a US ally",
                     "The country is NOT an important trade partner of the US",
                     "10 years", "25 years", "A single unconstrained individual (the leader)",
                     "A member of the military", "A monarch", "Yes and some opposition parties are allowed to run",
                     "Yes but only regime candidates can run", "Buddhist", "Muslim",
                     "Strong")

rownames(sanctionr) <- c()
names(sanctionr)[names(sanctionr) == '2.5 %'] <- 'lb'
names(sanctionr)[names(sanctionr) == '97.5 %'] <- 'ub'


sanctionr$Attribute <- c("oil", "ally", "trade", "years", "years",
                         "gov", "person", "person", "elections",
                         "elections", "religion", "religion", "militar")

myvars <- c("Attribute", "Level", "lb", "ub")
coefs_graph3 <- sanctionr[myvars]
coefs_graph3$outcome <- as.factor(2) 

res_foreignaid <- lm(foreignaid ~ oil + ally  + trade + years  + 
                       gov + person + elections + religion + militar, 
                     data = data_short)
foreignr <- joint.ci.bonf(res_foreignaid)
foreignr$prov <- as.factor(rownames(foreignr))
foreignr <- subset(foreignr, prov!= "(Intercept)")

foreignr$Level <- c("Oil-exporting country", "The country is NOT a US ally",
                    "The country is NOT an important trade partner of the US",
                    "10 years", "25 years", "A single unconstrained individual (the leader)",
                    "A member of the military", "A monarch", "Yes and some opposition parties are allowed to run",
                    "Yes but only regime candidates can run", "Buddhist", "Muslim",
                    "Strong")

rownames(foreignr) <- c()
names(foreignr)[names(foreignr) == '2.5 %'] <- 'lb'
names(foreignr)[names(foreignr) == '97.5 %'] <- 'ub'

foreignr$Attribute <- c("oil", "ally", "trade", "years", "years",
                        "gov", "person", "person", "elections",
                        "elections", "religion", "religion", "militar")

myvars <- c("Attribute", "Level", "lb", "ub")
coefs_graph2 <- foreignr[myvars]
coefs_graph2$outcome <- as.factor(3) 

coefs_final <- rbind(coefs_graph, coefs_graph2, coefs_graph3)

coefs_final$outcome <- factor(coefs_final$outcome,
                              levels = c(1,2,3),
                              labels = c("Military intervention", 
                                         "Economic sanctions", "Democracy Aid")) 

#Now we need to create different rows for the titles and the baselines:
coefs_final$Level <- paste0('   ', coefs_final$Level)

Level <- c("Trade relationships",
           "   (B=The country is an important trade partner of the US)",
           "International military alliance",
           "   (B=The country is a US ally)",
           "Natural resources",
           "   (B=Non-oil exporting country)",
           "Political power and policy are controlled by...",
           "   (B=A group of top regime officials (a council, party comittee or junta))",
           "The regime's leader is...",
           "   (B=A civilian who heads the regime's official party)",
           "Does the regime hold elections?",
           "   (B=No)",
           "Years the regime has been in power...",
           "   (B=4 years)",
           "The country is predominantly...",
           "   (B=Christian)",
           "The regime is militarily...",
           "   (B=Weak)")

coefs_final <- add_row(coefs_final, Level=Level)
coefs_final$outcome <- as.character(coefs_final$outcome)
coefs_final$outcome[is.na(coefs_final$outcome)] <- "Military intervention"

coefs_final <- add_row(coefs_final, Level=Level)
coefs_final$outcome[is.na(coefs_final$outcome)] <- "Economic sanctions"

coefs_final <- add_row(coefs_final, Level=Level)
coefs_final$outcome[is.na(coefs_final$outcome)] <- "Democracy Aid"


#Relevel the Levels
coefs_final$Level <- as.factor(coefs_final$Level)
coefs_final$levels_ordered <- coefs_final$Level

coefs_final$levels_ordered <- factor(coefs_final$levels_ordered,
                                     levels=c("Political power and policy are controlled by...",
                                              "   (B=A group of top regime officials (a council, party comittee or junta))",
                                              "   A single unconstrained individual (the leader)",
                                              "The regime's leader is...",
                                              "   (B=A civilian who heads the regime's official party)",
                                              "   A monarch",
                                              "   A member of the military",
                                              "Does the regime hold elections?",
                                              "   (B=No)",
                                              "   Yes but only regime candidates can run",
                                              "   Yes and some opposition parties are allowed to run",
                                              "Years the regime has been in power...",
                                              "   (B=4 years)",
                                              "   10 years",
                                              "   25 years",
                                              "The country is predominantly...",
                                              "   (B=Christian)",
                                              "   Buddhist",
                                              "   Muslim",
                                              "The regime is militarily...",
                                              "   (B=Weak)",
                                              "   Strong" ,
                                              "Natural resources",
                                              "   (B=Non-oil exporting country)",
                                              "   Oil-exporting country" ,
                                              "International military alliance",
                                              "   (B=The country is a US ally)" ,
                                              "   The country is NOT a US ally",
                                              "Trade relationships",
                                              "   (B=The country is an important trade partner of the US)",
                                              "   The country is NOT an important trade partner of the US"))

coefs_final$levels_ordered <- factor(coefs_final$levels_ordered, 
                                     levels=rev(levels(coefs_final$levels_ordered)))


coefs_final$outcome <- factor(coefs_final$outcome,
                              levels = c("Military intervention", 
                                         "Economic sanctions", "Democracy Aid"))

coefs_final$Estimate <- (coefs_final$lb + coefs_final$ub)/2


ggplot(coefs_final) +
  geom_point(aes(y=Estimate, x=levels_ordered, color=outcome, group=outcome), position = position_dodge(width = 0.5)) +
  geom_pointrange(aes(y=Estimate, x=levels_ordered, ymin=lb,ymax=ub, color=outcome, group=outcome), position = position_dodge(width = 0.5)) +
  coord_flip() +
  theme_bw() +
  geom_hline(yintercept=0,linetype=4, colour="black") +
  facet_grid( ~ outcome, drop=T ) +
  theme(legend.position="none",
        axis.text.y = element_text(hjust = 0),
        axis.ticks.y=element_blank()) +
  xlab("")


# Table C3 ----------------------------------------------------------------

myvars <- c("attack", "oil", "ally", "trade", "years", "gov", "person", "elections",
            "religion", "militar", "ID", "sanction", "foreignaid")
df <- data_short[myvars]

#Create all the possible combinations of outcomes

#A - All of them
df$outcomeA <- 0
df$outcomeA[df$attack==1 & df$sanction==1 & df$foreignaid==1 ] <- 1

#B - NONE of them
df$outcomeB <- 0
df$outcomeB[df$attack==0 & df$sanction==0 & df$foreignaid==0 ] <- 1

#C - Military intervention and Sanction
df$outcomeC <- 0
df$outcomeC[df$attack==1 & df$sanction==1 & df$foreignaid==0 ] <- 1

#D - Military intervention and Aid
df$outcomeD <- 0
df$outcomeD[df$attack==1 & df$sanction==0 & df$foreignaid==1 ] <- 1

#E - Sanction and Aid
df$outcomeE <- 0
df$outcomeE[df$attack==0 & df$sanction==1 & df$foreignaid==1 ] <- 1

#F - Sanction
df$outcomeF <- 0
df$outcomeF[df$attack==0 & df$sanction==1 & df$foreignaid==0 ] <- 1

#G - Aid
df$outcomeG <- 0
df$outcomeG[df$attack==0 & df$sanction==0 & df$foreignaid==1 ] <- 1

#H - Military intervention
df$outcomeH <- 0
df$outcomeH[df$attack==1 & df$sanction==0 & df$foreignaid==0 ] <- 1


#Stack it
df_mlogit <- reshape(df,
                     varying = list(
                       c("outcomeA", "outcomeB", "outcomeC",
                         "outcomeD","outcomeE","outcomeF",
                         "outcomeG","outcomeH") 
                     ),
                     v.names = c("outcome"),
                     timevar = "alt",
                     idvar = "id_outcome",
                     direction = "long"
)


df_mlogit[["alt"]] <- c("A", "B", "C", "D",
                        "E", "F", "G", "H")[df_mlogit[["alt"]]]


df_mlogit$alt <- recode_factor(df_mlogit$alt, "A" = "All", 
                        "B" = "None", "C"="Attack_Sanction",
                        "D"="Attack_Aid", "E"="Sanction_Aid", "F"="Sanction", "G"="Aid", "H"="Attack")


df_mlogit$alt <- factor(df_mlogit$alt, levels = c("None", "All", "Aid", "Sanction", "Attack", "Attack_Aid",
                                                      "Attack_Sanction", "Sanction_Aid"))


df_mlogit$alt <- as.factor(df_mlogit$alt)
df_mlogit$alt <- relevel(df_mlogit$alt, ref = "None")


df_wide <- df_mlogit[ which(df_mlogit$outcome==1), ]

#install.packages("nnet")
library("nnet")

model_mnm <- multinom(alt ~ oil + ally  + trade + years  + 
                        gov + person + elections + 
                        religion + militar, data=df_wide)

model_mnm.coef <- exp(coef(model_mnm))

#To export the results and easily copy the coefficients, run the following lines.
#install.packages("stargazer")
library("stargazer")
stargazer(model_mnm, type="html", out="multinom_coef.html",coef = list(model_mnm.coef))


# Figure C6 ---------------------------------------------------------------

dt_attack <- data.frame(gov="A single unconstrained individual (the leader)",
                        person="A member of the military",
                        elections="No",
                        years="25 years",
                        religion="Muslim",
                        militar="Weak",
                        oil="Oil-exporting country", 
                        ally="The country is NOT a US ally",
                        trade="The country is NOT an important trade partner of the US")


res_attack <-  glm(attack ~ oil + ally  + trade + years  + 
                     gov + person + elections + religion + militar, 
                   data = data_short, family = binomial(link = "probit"))

attack_df <- predict(res_attack, dt_attack,  type = 'response', se=T)

predf <- attack_df$fit # predicted
lower <- attack_df$fit - (1.96*attack_df$se.fit) # lower bounds
upper <- attack_df$fit + (1.96*attack_df$se.fit) # upper bounds

df <- data.frame(predf, lower, upper)
df$instrument <- c("Military intervention")


dt_sanction <- data.frame(gov="A group of top regime officials (a council, party committee or junta)",
                          person="A civilian who heads the regime's official party",
                          elections="Yes and some opposition parties are allowed to run",
                          years="4 years",
                          religion="Christian",
                          militar="Weak",
                          oil="Non-oil-exporting country", 
                          ally="The country is a US ally",
                          trade="The country is NOT an important trade partner of the US")



res_sanction <-  glm(sanction ~ oil + ally  + trade + years  + 
                       gov + person + elections + religion + militar, 
                     data = data_short, family = binomial(link = "probit"))

sanction_df <- predict(res_sanction, dt_sanction,  type = 'response', se=T)

predf <- sanction_df$fit # predicted
lower <- sanction_df$fit - (1.96*sanction_df$se.fit) # lower bounds
upper <- sanction_df$fit + (1.96*sanction_df$se.fit) # upper bounds

df2 <- data.frame(predf, lower, upper)
df2$instrument <- c("Economic sanctions")

df <- rbind(df, df2)


dt_aid <- data.frame(gov="A single unconstrained individual (the leader)",
                     person="A member of the military",
                     elections="No",
                     years="25 years",
                     religion="Muslim",
                     militar="Strong",
                     oil="Non-oil-exporting country", 
                     ally="The country is NOT a US ally",
                     trade="The country is NOT an important trade partner of the US")



res_foreignaid <- glm(foreignaid ~ oil + ally  + trade + years  + 
                        gov + person + elections + religion + militar, 
                      data = data_short, family = binomial(link = "probit"))


aid_df <- predict(res_foreignaid, dt_aid,  type = 'response', se=T)

predf <- aid_df$fit # predicted
lower <- aid_df$fit - (1.96*aid_df$se.fit) # lower bounds
upper <- aid_df$fit + (1.96*aid_df$se.fit) # upper bounds

df3 <- data.frame(predf, lower, upper)
df3$instrument <- c("Democracy Aid")

df <- rbind(df, df3)

df$instrument <- as.factor(df$instrument)

ggplot(df) +
  geom_pointrange(aes(x = instrument, y = predf, ymin = lower, 
                      ymax = upper), 
                  position = position_dodge(.3), lwd=1, stat = "identity") +
  theme_bw() +
  theme(legend.position="none",
        axis.text.y = element_text(hjust = 0),
        axis.ticks.y=element_blank()) +
  xlab("") + ylab("Predicted value") +
  theme(text = element_text(size=20))


# Figure D6 ---------------------------------------------------------------


#Recode political information
#install.packages("memisc")
library("memisc")

#john roberts
conj$item_1 <- 0
conj$item_1[(conj$ps_1==1)] <- 1


#Afghanistan
conj$item_2 <- 0
conj$item_2[(conj$ps_2_1==1)] <- 1


#Vietnam
conj$item_3 <- 0
conj$item_3[(conj$ps_2_2==1)] <- 1


#Korea
conj$item_4 <- 0
conj$item_4[(conj$ps_2_3==1)] <- 1

#Germany
conj$item_5 <- 0
conj$item_5[(conj$ps_2_4==1)] <- 1

#Peru
conj$item_6 <- 1
conj$item_6[(conj$ps_2_5==1)] <- 0

#Burma
conj$item_7 <- 1
conj$item_7[(conj$ps_2_6==1)] <- 0


#override veto
conj$item_8 <- 0
conj$item_8[(conj$ps_3==2)] <- 1


#Sum them  up
conj$info_index <- with(conj, item_1 + item_2 + item_3 + item_4 +
                          item_5 + item_6 + item_7 + item_8, na.rm = F)

hist(conj$info_index, main="", xlab = "Index of Political Sophistication")


