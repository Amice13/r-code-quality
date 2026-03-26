
#2025_04_03

load("repdf_pol_goal_exp.RData")

#### Data preparation 

#Make Gender Variable ####

# view the variable: 
table(data$Z3)

# Convert Z3 to lowercase and remove whitespace
data$Z3 <- tolower(trimws(data$Z3))

# view the variable again: 
table(data$Z3)

data$gender1 <- ifelse(data$Z3 %in% c("f"  ,"femme"  ,"donna"  , 
                                      "fem"  ,"female"  ,"feminin"  ,"femminile"  , 
                                      "frau"  ,"madame"  ,"féminin"  ,"mme"  , 
                                      "w"  ,"weiblich"  ,"woman"  ,"weibliche"  , 
                                      "famme"  ,"fe"  ,"femail"  ,"femal"  , 
                                      "femelle"  ,"femenino"  ,"femin"  ,"feminile"  , 
                                      "feminine"  ,"fémin"  ,"feminmin"  , 
                                      "femme feministe"  ,"femme noderne"  ,"femmes"  ,"femmina"   , 
                                      "fille"  ,"fr"  ,"femin"  ,"feminile"  , 
                                      "sono una donna" , "weblich" , "weinlich"  , "femme arc en ciel"), 
                       "female",
                       ifelse(data$Z3 %in% c( "'männlich" , "boy" , "h"  , 
                                              "freitext fragezeichen punkt punkt punkt männlich" , "herr"  , 
                                              "home" , "homme" , "männnlich"  , "muško", "marié", 
                                              "homme cisgenre" , "jeune homme" , "m" , "man"  , 
                                              "mand" , "maennlich" , "male" , "maenlich"  , 
                                              "manlich" , "mann" , "mannlich" , "maschi"  , 
                                              "maschile" , "mascula" , "maschio" , "masculain"  , 
                                              "masculin" , "masculin singulier" , "masculines" , "masculino"  , 
                                              "maskulin" , "masqulin" , "mec" , "men"  , 
                                              "menlich" , "menn" , "mec" , "monsieur"  , "hétérosexuel"  , "retraité",
                                              "mr" , "msculin" , "mâle alpha" , "mâle"  , 
                                              "mämlich" , "mämmlich" , "män" , "mändlich"  , 
                                              "mänlich" , "männer" , "männlch" , "männdlich"  , 
                                              "männlich oder manchmal auch helikopter wenns praktisch ist"  , 
                                              "männlich und eine option zum ankreuzen hätte gereicht"  , 
                                              "männliche" , "mänlich" , "männlich"  , 
                                              "mendlich" , "mànlich" , "mänlich" , "männlich"  ,  "mànnlich",
                                              "uomo" , "masc" , "maschilista" , "mämmlich", "män",
                                              "homme réaliste dans un monde en détresse"),
                              "male",
                              ifelse(data$Z3 %in% c("andere"  , "aucun"  , "aucune"  , "beides"  , 
                                                    "divers"  , "diverses"  , "individu"  , 
                                                    "neutral"  , "neutre"  , "nicht binär"  , 
                                                    "nichtbinär"  , "not specified"  , "queer", "x", "non" ),
                                     "non-binary (as an umbrella term)", 
                                     ifelse(data$Z3 %in% c("cisgenre"  , "ca ne vous regarde pas"  , "couple" ,    "das macht keinen sehr wissenschaftlichen eindruck"  ,
                                                           "ddd"  , "delfin"  , "etero",  "gender"  , "goht euch nüt ah"  , "hetero"  , "hétéro"  , 
                                                           "kann man ja heute frei wählen"  ,  "merci"  , "keine anig",
                                                           "italien"  , "je n'ai pas d'avis"  ,  "je sais pas",   "normal"  , "ou qu un"  ,
                                                           "pas compris" , "quel genre" , "genre" , "nein"  , "trans"  , "tuamadre", 
                                                           "sympa"    , "rose"  ,  "wei es nicht mehr"  , "waschmaschine",    "es",  "xx",                                                       
                                                           "xxx" , "z"  ,    "keine sngaben" ) , 
                                            "other", NA))))


table(data$gender1)



data$gender <-  ifelse(data$gender1 == "other", NA, data$gender)

data$gender <- as.factor(data$gender)

table(data$gender)


# Age ####
data$Birthyear <- as.numeric(data$Z2)
data$Age <- 2022-data$Birthyear 
# =  AGE AT SURVEYYEAR.

#Age categories 
data$agecat <- NA
data$agecat[data$Age <=30] <- "18-30 years"
data$agecat[data$Age <=40&data$Age>=31] <- "31-40 years"
data$agecat[data$Age <=50&data$Age>=41] <- "41-50 years"
data$agecat[data$Age <=60&data$Age>=51] <- "51-60 years"
data$agecat[data$Age <=70&data$Age>=61] <- "61-70 years"
data$agecat[data$Age <=80&data$Age>=71] <- "71-80 years"
data$agecat[data$Age>=81] <- "> 80 years"
data$agecat <- ordered(data$agecat, levels=c("18-30 years","31-40 years","41-50 years","51-60 years","61-70 years",
                                             "71-80 years","> 80 years"))

table(data$agecat)


#Education ####

data$educ <- NA
data$educ[data$Z9 == "No education completed"] <- "Secondary I"
data$educ[data$Z9 == "Compulsory school"] <- "Secondary I"
data$educ[data$Z9 == "Higher technical school (3 years full-time or 4 years part-time)"] <- "Secondary II"
data$educ[data$Z9 == "Professional apprenticeship or vocational school"] <- "Secondary II"
data$educ[data$Z9 == "Higher vocational training"] <- "Tertiary"
data$educ[data$Z9 == "School leading to baccalaureate, Diploma for teaching in primary school or preprimary school, vocational baccalaureate"] <- "Tertiary"
data$educ[data$Z9 == "University, ETH, Universities of Applied Sciences"] <- "Tertiary"

table(data$educ)

data$educ <- as.factor(data$educ)

#Income ####

data$income <- data$Z15
data$income <- ordered(data$income, levels=c("under CHF 5,000","CHF 5,001 to CHF 7,000",
                                             "CHF 7,001 to CHF 9,000","CHF 9,001 to CHF 13,000","over CHF 13,001"))
table(data$income)


data$incomehighdummy <- NA
data$incomehighdummy <- ifelse(data$income=="over CHF 13,00", "Higher", "Lower")
data$incomehighdummy[data$income=="CHF 9,001 to CHF 13,000"] <- "Higher"

data$incomehighdummy <- as.factor(data$incomehighdummy )
table(data$incomehighdummy)





#Living condition ####

data$house <- NA

data$house[data$Z4=="Tenant or sub-tenant?"] <- "Tenant"
data$house[data$Z4=="Condominium/apartment owner or co-owner?"] <-  "Own flat"
data$house[data$Z4=="House owner or co-owner?"] <-  "Own house"
data$house[data$Z4=="Cooperative member?"] <-  "Cooperative"
data$house[data$Z4=="Other"] <-  "Other"

table(data$house)

data$ownhouseflat <- NA
data$ownhouseflat <- as.factor(ifelse(data$house=="Own house", "Owning", "Other"))
data$ownhouseflat[data$house=="Own flat"] <- "Owning"

data$ownhouseflat <- as.factor(data$ownhouseflat)

table(data$ownhouseflat)






# GEOGRAPHICAL ####################################

### Make regional covariates - actual placement



data$urban <- ""
data$urban[data$X9_CATEGORIES=="Alps-Urban"] <- "Urban"
data$urban[data$X9_CATEGORIES=="Jura-Urban"] <- "Urban"
data$urban[data$X9_CATEGORIES=="Midlands-Urban"] <- "Urban"
data$urban[data$X9_CATEGORIES=="Alps-Periurban"] <- "Agglomeration"
data$urban[data$X9_CATEGORIES=="Midlands-Periurban"] <- "Agglomeration"
data$urban[data$X9_CATEGORIES=="Jura-Periurban"] <- "Agglomeration"
data$urban[data$X9_CATEGORIES=="Alps-Rural"] <- "Rural"
data$urban[data$X9_CATEGORIES=="Midlands-Rural"] <- "Rural"
data$urban[data$X9_CATEGORIES=="Jura-Rural"] <- "Rural"

# Attitudes
# Political Left-right placement ##########################################################

data$leftright <- as.numeric(data$H2.)
data$leftright[data$H2.=="0 - far left"] <- 0
data$leftright[data$H2.=="10 - far right"] <- 10
table(data$leftright)

data$leri <- "None"
data$leri[data$leftright <= 4] <- "Left"
data$leri[data$leftright == 5] <- "Center"
data$leri[data$leftright >= 6] <- "Right"
table(data$leri)
data$leri <- factor(data$leri,  levels= c("Left", "Center", "Right", "None"))




# Carfree households ####
data$nocarhousehold <- data$B1
data$nocarhousehold [data$nocarhousehold  == ""] <- NA
data$nocarhousehold[data$nocarhousehold == 'No'] <- 1
data$nocarhousehold[data$nocarhousehold == 'Yes, one'] <- 0
data$nocarhousehold[data$nocarhousehold == 'Yes, several'] <- 0
data$nocarhousehold <- as.factor(ifelse(data$nocarhousehold == 1, "No car", "Has car"))
table(data$nocarhousehold)




## Questions on priorities ####

## Questions on priorities ####

data$ccprio1 <-data$A1.1_12_1
data$ccprio1[is.na(data$ccprio1)] <- 0
data$ccprio1 <- dplyr::recode(data$ccprio1, 'First'  = 1, .default = 0)
data$ccprio1 <- as.factor(ifelse(data$ccprio1 == 1, "Yes", "No"))
table(data$ccprio1)

data$energyprio1 <-data$A1.1_9_1
data$energyprio1[is.na(data$energyprio1)] <- 0
data$energyprio1 <- dplyr::recode(data$energyprio1, 'First'  = 1, .default = 0)
data$energyprio1 <- as.factor(ifelse(data$energyprio1 == 1, "Yes", "No"))
table(data$energyprio1)






#Trust ####
data$trustscience <- data$H5_1
data$trustscience <- as.integer(data$trustscience)
#recode NAs
data[data == ""] <- NA
# rename goal1 to goal ####
#copy
data$goal <- data$goal1
# remove the old column
data <- data[, !(names(data) %in% c("goal1"))]
#policies ####
table(data$polchoicelow)
data$FC <- data$polchoicelow
table(data$FCH1)
data$FCH1 <- gsub("Min A Fin ", "Min A Fin", data$FCH1)
table(data$FCH1)


table(data$C_LM)
data$C_LM <- gsub("_c2", "", data$C_LM)
table(data$C_LM)
table(data$C_H1)
data$C_H1 <- gsub("_c", "", data$C_H1)
data$C_H1 <- gsub("_C", "", data$C_H1)
table(data$C_H1)
table(data$C_H2)
data$C_H2 <- gsub("_c", "", data$C_H2)
table(data$C_H2)
table(data$CH3)
data$CH3 <- gsub("_c", "", data$CH3)
table(data$CH3)



table(data$FC)
data$FC <- ifelse(!is.na(data$FC), data$FC, data$FCH1)
data$FC <- ifelse(!is.na(data$FC), data$FC, data$FCH2)
data$FC <- ifelse(!is.na(data$FC), data$FC, data$FCH3)
data$FC <- ifelse(!is.na(data$FC), data$FC, data$C_LM)
data$FC <- ifelse(!is.na(data$FC), data$FC, data$C_H1)
data$FC <- ifelse(!is.na(data$FC), data$FC, data$C_H2)
data$FC <- ifelse(!is.na(data$FC), data$FC, data$CH3)
table(data$FC)


#### Generate firstpolchoice 
data$firstpolchoice <- ifelse(data$FC == "Max A Fin" | data$FC == "Max B Reg", 3, 
                              ifelse(data$FC == "Mid A Fin" | data$FC == "Mid B Reg", 2, 
                                     ifelse(data$FC == "Min A Fin" | data$FC == "Min B Reg", 1, 0)))

data$firstpolchoice <- ifelse(data$firstpolchoice ==  3, "Max",
                              ifelse(data$firstpolchoice == 2, "Med", 
                                     ifelse(data$firstpolchoice ==  1, "Min", 
                                            ifelse(data$firstpolchoice ==  0, "None",   NA))))







# rename FeBa. to feba ####
#copy
data$feba <- data$FeBa.
# remove the old column
data <- data[, !(names(data) %in% c("FeBa."))]




# did the goal change after feedback, and if so, how?  ####

###

data %>% 
  summarise(total_non_na = sum(!is.na(Zielanpassung)))


table(data$Zielanpassung, data$goal, exclude = NULL)
# did the goal change later, and if so, how?  ####
data$goalchanged <-  NA

# 1 if goal was elevated (only possible if goal was not high before)
data$goalchanged[data$Zielanpassung == "100% 2040" & data$goal== "medium"] <- "1"

# 0 if goal was unchanged
data$goalchanged[data$Zielanpassung == "100% 2040" & data$goal== "high"] <- "0"
data$goalchanged[data$Zielanpassung == "100% 2050" & data$goal== "medium"] <- "0"


# -1 if goal was decreased
data$goalchanged[data$Zielanpassung == "100% 2050" & data$goal == "high"] <- "-1"
table(data$goalchanged)
data$goalchanged[data$Zielanpassung == "90% 2050" & data$goal == "high"] <- "-1"
table(data$goalchanged)
data$goalchanged[data$Zielanpassung == "90% 2050" & data$goal== "medium"] <- "-1"
table(data$goalchanged)


table(data$goalchanged, exclude = NULL)


data %>% 
  summarise(total_non_na = sum(!is.na(goalchanged)))




### Policy Choice after Feedback  ####

table(is.na(data$APH1))
#37 #ok
table(is.na(data$APH2.))
#86
table(is.na(data$APH3.))
#63
table(is.na(data$APLM))
#71

#37+86+63+71 = 257


data %>% select(c("APH1", "APH2.", "APH3.", "APLM")) %>% 
  summarise(total_non_na = sum(!is.na(APH1) | !is.na(APH2.)  | !is.na(APH3.) | !is.na(APLM)   ))
#### Variable SC "second choice"
data$SC <- NA


# MED FIN

data$SC[grepl("Finally, there are requirements in the area of renewable electricity: from 2030 there will be a solar obligation on new buildings and renovations.", data$APH1)] <- "Mid A Fin"
table(data$SC)
data$SC[grepl("Finally, there are requirements in the area of renewable electricity: from 2030 there will be a solar obligation on new buildings and renovations.", data$APH2.)] <- "Mid A Fin"
table(data$SC)
data$SC[grepl("Finally, there are requirements in the area of renewable electricity: from 2030 there will be a solar obligation on new buildings and renovations.", data$APH3.)] <- "Mid A Fin" 
table(data$SC)
data$SC[grepl("Finally, there are requirements in the area of renewable electricity: from 2030 there will be a solar obligation on new buildings and renovations.", data$APLM)] <- "Mid A Fin"
table(data$SC)

# MID REG
data$SC[grepl("Electricity suppliers will also be required to have at least 80% domestic renewable electricity in their electricity mix from 2030.", data$APH1)] <- "Mid B Reg"
table(data$SC)
data$SC[grepl("Electricity suppliers will also be required to have at least 80% domestic renewable electricity in their electricity mix from 2030.", data$APH2.)] <- "Mid B Reg" 
table(data$SC)
data$SC[grepl("Electricity suppliers will also be required to have at least 80% domestic renewable electricity in their electricity mix from 2030.", data$APH3.)] <- "Mid B Reg" 
table(data$SC)
data$SC[grepl("Electricity suppliers will also be required to have at least 80% domestic renewable electricity in their electricity mix from 2030.", data$APLM)] <- "Mid B Reg"
table(data$SC)



#MIN

data$SC[grepl("From 2040, there will be a solar obligation on new buildings and renovations.", data$APH1)] <- "Min A Fin" 
table(data$SC)
data$SC[grepl("From 2040, there will be a solar obligation on new buildings and renovations.", data$APH2.)] <- "Min A Fin"
table(data$SC)
data$SC[grepl("From 2040, there will be a solar obligation on new buildings and renovations.", data$APH3.)] <- "Min A Fin" 
table(data$SC)
data$SC[grepl("From 2040, there will be a solar obligation on new buildings and renovations.", data$APLM)] <- "Min A Fin"
table(data$SC)


data$SC[grepl("There are requirements", data$APH1)] <- "Min B Reg"
table(data$SC)
data$SC[grepl("There are requirements", data$APH2.)] <- "Min B Reg" 
table(data$SC)
data$SC[grepl("There are requirements", data$APH3.)] <- "Min B Reg" 
table(data$SC)
data$SC[grepl("There are requirements", data$APLM)] <- "Min B Reg"
table(data$SC)


data$SC[grepl("I do not support any of these", data$APH1)] <- "none"
table(data$SC)
data$SC[grepl("I do not support any of these", data$APH2.)] <-"none"
table(data$SC)
data$SC[grepl("I do not support any of these", data$APH3.)] <- "none"
table(data$SC)
data$SC[grepl("I do not support any of these", data$APLM)] <- "none"
table(data$SC)



#High

data$SC[grepl("CHF 0,12", data$APH1)] <- "Max A Fin"
table(data$SC)
data$SC[grepl("CHF 0,12.", data$APH2.)] <- "Max A Fin"
table(data$SC)
data$SC[grepl("CHF 0,12", data$APH3.)] <- "Max A Fin" 
table(data$SC)
data$SC[grepl("CHF 0,12", data$APLM)] <- "Max A Fin"
table(data$SC)


data$SC[grepl("The construction of a plant for the", data$APH1)] <- "Max B Reg"
table(data$SC)
data$SC[grepl("The construction of a plant for the", data$APH2.)] <- "Max B Reg" 
table(data$SC)
data$SC[grepl("The construction of a plant for the", data$APH3.)] <- "Max B Reg" 
table(data$SC)
data$SC[grepl("The construction of a plant for the", data$APLM)] <- "Max B Reg"


data %>% 
  summarise(total_non_na = sum(!is.na(SC)))






data$policychanged <- NA
data$policychanged[data$SC %in% c("Max B Reg", "Max A Fin")] <- "1"

data$policychanged[data$FC == "Min A Fin" & data$SC %in% c("Mid B Reg", "Mid A Fin")] <- "1"

data$policychanged[data$FC == "Min B Reg" & data$SC %in% c("Mid B Reg", "Mid A Fin")] <- "1"

data$policychanged[data$FC == data$SC] <- "0"

data$policychanged[data$FC == "Mid B Reg" & data$SC == "Mid A Fin"] <- "0"# horizontal movement

data$policychanged[data$FC == "Mid A Fin" & data$SC == "Mid B Reg"] <- "0" # horizontal movement
data$policychanged[data$FC == "Min A Fin" & data$SC == "Min B Reg"] <- "0" # horizontal movement
data$policychanged[data$FC == "Min B Reg" & data$SC == "Min A Fin"] <- "0" # horizontal movement



data$policychanged[data$FC == "Mid A Fin" & data$SC %in% c("Min B Reg", "Min A Fin")] <- "-1" # policy ambition reduced
data$policychanged[data$FC == "Mid B Reg" & data$SC %in% c("Min B Reg", "Min A Fin")] <- "-1" # policy ambition reduced
data$policychanged[data$SC == "none"] <- "-1"  # policy ambition reduced
table(data$policychanged, exclude = NULL)

data %>% 
  summarise(total_non_na = sum(!is.na(policychanged)))


#### prepare for the analysis code
data$treat <- as.factor(data$treat) # indicates control1,control2,goal,pick 
data$treat <- ifelse(data$treat == "control2", "No Goal", 
                     ifelse(data$treat == "goal", "Goal Assigned", (
                       ifelse(data$treat == "pick", "Goal Selected", "Policy+Goal"))))
data$treat <- as.factor(data$treat)   

data$treat <- forcats::fct_recode(data$treat,  "No Goal" = "No Goal", 
                                  "Policy+Goal" = "Policy+Goal", 
                                  "Goal Assigned" = "Goal Assigned",
                                  "Goal Selected" = "Goal Selected")
data$treat <- forcats::fct_relevel(data$treat, c("No Goal", "Policy+Goal", "Goal Assigned", "Goal Selected"))



data$feba <- as.factor(data$feba) # indicates respondent got feedback and decided to change goal/do not change anything/change policy mix

data$FC <- as.factor(data$FC) #non-numeric first choice: max a fin, max b reg,... none
data$SC <- as.factor(data$SC)  #non-numeric second choice: max a fin, max b reg,... none



data$goal <- as.factor(data$goal) #numeric indicator (1-3) for low (1)/medium (2)/high (3)
data$goal <- forcats::fct_recode(data$goal,  "Low Goal" = "low",  "Medium Goal" = "medium", "High Goal" = "high")

data$goal <- forcats::fct_relevel(data$goal, c("Low Goal", "Medium Goal", "High Goal"))



data$newvar <- paste(data$treat, data$goal,sep=" ")
data$newvar  <- as.factor(data$newvar)


#### prepare the last goal (lgoal)
data$lgoal <- data$goal
data$lgoal[data$Zielanpassung == '100% 2050'] <- 'Medium Goal'
data$lgoal[data$Zielanpassung == '100% 2040'] <- 'High Goal'
data$lgoal[data$Zielanpassung == '90% 2050'] <- 'Low Goal'


data$lastpolchoice <- data$firstpolchoice
data$lastpolchoice [data$SC == 'none'] <- 'None'
data$lastpolchoice [data$SC == 'Max A Fin'] <- 'Max'
data$lastpolchoice [data$SC == 'Max B Reg'] <- 'Max'
data$lastpolchoice [data$SC == 'Mid A Fin'] <- 'Med'
data$lastpolchoice [data$SC == 'Mid B Reg'] <- 'Med'
data$lastpolchoice [data$SC == 'Min A Fin'] <- 'Min'
data$lastpolchoice [data$SC == 'Min B Reg'] <- 'Min'



#############################################################################
# ####################  CREATE DUMMIES  #################### ####
################################################################################

data$FirstChoiceHighPol <- ifelse(data$firstpolchoice == "Max", 1, 0)
data$FirstChoiceNoPol <- ifelse(data$firstpolchoice == "None", 1, 0)
data$FirstChoiceHighMed <- ifelse(data$firstpolchoice == "Max", 1, 
                                  ifelse(data$firstpolchoice == "Med", 1, 0))

data$TrustInScience <- ifelse(is.na(data$trustscience) | data$trustscience < 8, "Low or NA", "High")

data$TrustInScience <- as.factor(data$TrustInScience)


#After Feedback 

data$SecChoiceIncreasePol <- ifelse(data$policychanged == 1 , 1, 0)
data$GoalDecreasedAfterFeedback <- ifelse(data$goalchanged == -1 , 1, 0)


data$NothingDoneAfterFeedback <- ifelse(data$feba == "no_change_at_all", 1, 
                           ifelse(data$feba == "goal_change" |  data$feba == "policy_change" , 0, 
                                NA)) 

data$Feedback_decison <- forcats::fct_recode(data$feba,  "Goal" = "goal_change", 
"Neither" = "no_change_at_all" , 
"Policy" = "policy_change")



data$Feedback_decison <- factor(data$Feedback_decison)


# Make Experimental Dataset, only! #### 
gs_data <- data %>% filter(treat== 'Goal Selected')
data <-  data %>% filter(treat!= 'Goal Selected')

