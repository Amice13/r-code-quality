setwd("???????")
setwd("???????")
getwd()

library(pacman)
pacman::p_load(interplot, oddsratio, rmarkdown, psych, stargazer, ggplot2, foreign, ggridges, 
               gridExtra, survival, knitr, broom, purrr, semTools, lavaan, Rmisc, car, tidyverse, dplyr, 
               survival)

cand.sub <- read.csv("TYPE_FILE_NAME_HERE", stringsAsFactors = FALSE)

names(cand.sub)

####### FOR REFERENCE - HOUSEKEEPING ON RAW DATA #########

## Occupation - Q57

cand.sub$Q57[cand.sub$Q57=="Civil Service"] <- "Political"
cand.sub$Q57[cand.sub$Q57=="Charity Sector"] <- "Charitable/'Helping'"
cand.sub$Q57[cand.sub$Q57=="Business (SMEs, Corporate, Consultancy, IT)"] <- "Brokerage"
cand.sub$Q57[cand.sub$Q57=="Armed Forces"] <- "Charitable/'Helping'"
cand.sub$Q57[cand.sub$Q57=="Administration"] <- "Administrative"
cand.sub$Q57[cand.sub$Q57=="Administrative/Secretarial"] <- "Administrative"
cand.sub$Q57[cand.sub$Q57=="Armed Forces or Emergency Services"] <- "Charitable/'Helping'"
cand.sub$Q57[cand.sub$Q57=="Charitable and similar public sector"] <- "Charitable/'Helping'"
cand.sub$Q57[cand.sub$Q57=="Church"] <- "Charitable/'Helping'"
cand.sub$Q57[cand.sub$Q57=="Education"] <- "Public Sector Professional"
cand.sub$Q57[cand.sub$Q57=="Finance"] <- "Brokerage"
cand.sub$Q57[cand.sub$Q57=="Health"] <- "Public Sector Professional"
cand.sub$Q57[cand.sub$Q57=="Higher Education"] <- "Public Sector Professional"
cand.sub$Q57[cand.sub$Q57=="Manual"] <- "Manual"
cand.sub$Q57[cand.sub$Q57=="None prior to politics"] <- "Political"
cand.sub$Q57[cand.sub$Q57=="Politics"] <- "Political"
cand.sub$Q57[cand.sub$Q57=="Public Affairs (PR, Marketing, Sales)"] <- "Brokerage"
cand.sub$Q57[cand.sub$Q57=="Religion"] <- "Charitable/'Helping'"
cand.sub$Q57[cand.sub$Q57=="Teaching"] <- "Public Sector Professional"
cand.sub$Q57[cand.sub$Q57=="Don't know"] <- NA
cand.sub$Q57[cand.sub$Q57==""] <- NA
cand.sub$Q57[cand.sub$Q57=="Emergency Services"] <- "Charitable/'Helping'"
cand.sub$Q57[cand.sub$Q57=="Finance (banking and accountancy)"] <- "Brokerage"
cand.sub$Q57[cand.sub$Q57=="Health care"] <- "Public Sector Professional"
cand.sub$Q57[cand.sub$Q57=="Law"] <- "Brokerage"
cand.sub$Q57[cand.sub$Q57=="Media and Journalism"] <- "Brokerage"
cand.sub$Q57[cand.sub$Q57=="Other"] <- "Other"
cand.sub$Q57[cand.sub$Q57=="Property/Estate Agent"] <- "Other"
cand.sub$Q57[cand.sub$Q57=="Public Relations"] <- "Brokerage"
cand.sub$Q57[cand.sub$Q57=="Skilled Manual"] <- "Manual"
cand.sub$Q57[cand.sub$Q57=="Transport"] <- "Manual"
cand.sub$Q57[cand.sub$Q57=="19"] <- NA
cand.sub$Q57[cand.sub$Q57=="20"] <- NA
table(cand.sub$Q57)

# Parisanship - Left-Right

cand.sub$Q60[cand.sub$Q60=="Alliance Party (nir)"] <- "Irish unionist parties"
cand.sub$Q60[cand.sub$Q60=="Democratic Unionist Party (nir)"] <- "Irish unionist parties"
cand.sub$Q60[cand.sub$Q60=="DUP"] <- "Irish unionist parties"
cand.sub$Q60[cand.sub$Q60=="Independent(s) (nir)"] <- "Independent"
cand.sub$Q60[cand.sub$Q60=="Independent"] <- "Independent"
cand.sub$Q60[cand.sub$Q60==""] <- NA
cand.sub$Q60[cand.sub$Q60=="Don't know"] <- NA
cand.sub$Q60[cand.sub$Q60=="Not applicable"] <- NA
cand.sub$Q60[cand.sub$Q60=="Other (nir)"] <- "Other"
cand.sub$Q60[cand.sub$Q60=="Plaid Cyrmu"] <- "Plaid Cymru"
cand.sub$Q60[cand.sub$Q60=="Refusal"] <- NA
cand.sub$Q60[cand.sub$Q60=="SNP"] <- "Scottish National Party"
cand.sub$Q60[cand.sub$Q60=="Traditional Unionist Party (nir)"] <- "Irish unionist parties"
cand.sub$Q60[cand.sub$Q60=="UKIP"] <- "UK Independence Party"
cand.sub$Q60[cand.sub$Q60=="Ulster Unionist Party"] <- "Irish unionist parties"
cand.sub$Q60[cand.sub$Q60=="Ulster Unionist Party (nir)"] <- "Irish unionist parties"
cand.sub$Q60[cand.sub$Q60=="UUP"] <- "Irish unionist parties"
cand.sub$Q60[cand.sub$Q60=="Social Democratic and Labour Party (nir)"] <- "Labour"
cand.sub$Q60[cand.sub$Q60=="Green"] <- "Green Party"
cand.sub$Q60[cand.sub$Q60=="Sinn Fein (nir)"] <- "Sinn Fein"

cand.sub$LeftRight <- ifelse(cand.sub$Q60 == "Conservative", 1,
                              ifelse(cand.sub$Q60 == "UK Independence Party", 1, 
                                     ifelse(cand.sub$Q60 == "Irish unionist parties", 1, 
                                     ifelse(cand.sub$Q60 == "Green Party", 2,
                                            ifelse(cand.sub$Q60 == "Labour", 2, 
                                                   ifelse(cand.sub$Q60 == "Liberal Democrat", 2,
                                                          ifelse(cand.sub$Q60 == "Scottish National Party", 2, NA)))))))
table(cand.sub$LeftRight)
cand.sub$LeftRight[cand.sub$LeftRight==1] <- "Centre->Right"
cand.sub$LeftRight[cand.sub$LeftRight==2] <- "Centre->Left"
cand.sub$LeftRight[cand.sub$LeftRight==""] <- NA

# Education

cand.sub$Q77[cand.sub$Q77=="(Modern) Apprenticeship, Advanced (Modern) Apprenticeship, SVQ/NVQ/Key Skills Level 1 and 2 City and Guilds Craft/Inter"] <- "Apprenticeship"
cand.sub$Q77[cand.sub$Q77=="3-4 year University/CNAA first Degree (BA, BSc., BEd., BEng. etc)"] <- "Higher Education Degree"
cand.sub$Q77[cand.sub$Q77=="5 year University/CNAA first Degree (MB, BDS, BV etc)"] <- "Higher Education Degree"
cand.sub$Q77[cand.sub$Q77=="City and Guild certificate"] <- "Apprenticeship"
cand.sub$Q77[cand.sub$Q77=="Clerical and commercial"] <- "A-Levels/Vocational Diploma"
cand.sub$Q77[cand.sub$Q77=="CSE grade 1, GCE O level, GCSE, School Certificate"] <- "None"
cand.sub$Q77[cand.sub$Q77=="CSE grades 2-5"] <- "None"
cand.sub$Q77[cand.sub$Q77=="Don't know"] <- "None"
cand.sub$Q77[cand.sub$Q77=="Edexcel/BTEC/BEC/TEC - Higher National Certificate (HNC) or equivalent"] <- "A-Levels/Vocational Diploma"
cand.sub$Q77[cand.sub$Q77=="Foundation Degree (FdA, FdSc etc)"] <- "A-Levels/Vocational Diploma"
cand.sub$Q77[cand.sub$Q77=="GCE A level or Higher Certificate"] <- "A-Levels/Vocational Diploma"
cand.sub$Q77[cand.sub$Q77=="HE Access"] <- "A-Levels/Vocational Diploma"
cand.sub$Q77[cand.sub$Q77=="Masters Degree, M.Phil, Post-Graduate Diplomas and Certificates"] <- "Postgraduate Degree"
cand.sub$Q77[cand.sub$Q77=="No formal qualifications"] <- "None"
cand.sub$Q77[cand.sub$Q77=="None of these"] <- "None"
cand.sub$Q77[cand.sub$Q77=="Nursing certificate, Teacher training, HE Diploma, Edexcel/BTEC/BEC/TEC - Higher National Diploma (HND), OCR/RSA - Highe"] <- "A-Levels/Vocational Diploma"
cand.sub$Q77[cand.sub$Q77=="Nursing qualification (eg SEN, SRN, SCM, RGN)"] <- "Higher Education Degree"
cand.sub$Q77[cand.sub$Q77=="ONC"] <- "Apprenticeship"
cand.sub$Q77[cand.sub$Q77=="Other"] <- "None"
cand.sub$Q77[cand.sub$Q77=="Other technical, professional or higher qualification"] <- "Higher Education Degree"
cand.sub$Q77[cand.sub$Q77=="Ph.D, D.Phil or equivalent"] <- "Postgraduate Degree"
cand.sub$Q77[cand.sub$Q77=="Recognised trade apprenticeship completed"] <- "Apprenticeship"
cand.sub$Q77[cand.sub$Q77=="Refusal"] <- "None"
cand.sub$Q77[cand.sub$Q77=="Scottish Higher Certificate"] <- "A-Levels/Vocational Diploma"
cand.sub$Q77[cand.sub$Q77=="Scottish Ordinary/ Lower Certificate"] <- "None"
cand.sub$Q77[cand.sub$Q77=="Teaching qualification (not degree)"] <- "Higher Education Degree"
cand.sub$Q77[cand.sub$Q77=="University diploma"] <- "A-Levels/Vocational Diploma"
cand.sub$Q77[cand.sub$Q77=="University or CNAA first degree (eg BA, B.Sc, B.Ed)"] <- "Higher Education Degree"
cand.sub$Q77[cand.sub$Q77=="University or CNAA higher degree (eg M.Sc, Ph.D)"] <- "Postgraduate Degree"
cand.sub$Q77[cand.sub$Q77=="Vocational A-level (AVCE), GCE Applied A-level, NVQ/SVQ Level 3 GNVQ/SNVQ Advanced, Edexcel/BTEC/BEC/TEC (General/Ordina"] <- "A-Levels/Vocational Diploma"
cand.sub$Q77[cand.sub$Q77=="Youth training certificate/skillseekers"] <- "A-Levels/Vocational Diploma"
cand.sub$Q77[cand.sub$Q77==""] <- NA
cand.sub$Q77[cand.sub$Q77=="City and Guild certificate – advanced"] <- "Apprenticeship"

table(cand.sub$Q77)

cand.sub$Education <- factor(cand.sub$Q77, 
                              levels = c("None", 
                                         "Apprenticeship",
                                         "A-Levels/Vocational Diploma",
                                         "Higher Education Degree",
                                         "Postgraduate Degree"), ordered = TRUE)


############ HOUSEKEEPING ON CODED FILE (i.e. BJPOLS REPLICATION DATA 1) ############

#### Start with basic values
#### Create values as factors (Q45 - Q54; Q64 - Q73)
## 1. Items created by value
cand.sub$CONF1 <- factor(cand.sub$Q45, 
                          levels = c("Not like me at all", 
                                     "Not like me",
                                     "A little like me",
                                     "Somewhat like me",
                                     "Like me",
                                     "Very much like me"), ordered = TRUE)
table(cand.sub$CONF1)
levels(cand.sub$CONF1)

cand.sub$TRAD1 <- factor(cand.sub$Q46, 
                          levels = c("Not like me at all", 
                                     "Not like me",
                                     "A little like me",
                                     "Somewhat like me",
                                     "Like me",
                                     "Very much like me"), ordered = TRUE)
table(cand.sub$TRAD1)
levels(cand.sub$TRAD1)

cand.sub$BENE1 <- factor(cand.sub$Q47, 
                          levels = c("Not like me at all", 
                                     "Not like me",
                                     "A little like me",
                                     "Somewhat like me",
                                     "Like me",
                                     "Very much like me"), ordered = TRUE)
table(cand.sub$BENE1)
levels(cand.sub$BENE1)

cand.sub$UNIV1 <- factor(cand.sub$Q48, 
                          levels = c("Not like me at all", 
                                     "Not like me",
                                     "A little like me",
                                     "Somewhat like me",
                                     "Like me",
                                     "Very much like me"), ordered = TRUE)
table(cand.sub$UNIV1)
levels(cand.sub$UNIV1)

cand.sub$SELF1 <- factor(cand.sub$Q49, 
                          levels = c("Not like me at all", 
                                     "Not like me",
                                     "A little like me",
                                     "Somewhat like me",
                                     "Like me",
                                     "Very much like me"), ordered = TRUE)
table(cand.sub$SELF1)
levels(cand.sub$SELF1)

cand.sub$STIM1 <- factor(cand.sub$Q50, 
                          levels = c("Not like me at all", 
                                     "Not like me",
                                     "A little like me",
                                     "Somewhat like me",
                                     "Like me",
                                     "Very much like me"), ordered = TRUE)
table(cand.sub$STIM1)
levels(cand.sub$STIM1)

cand.sub$HEDO1 <- factor(cand.sub$Q51, 
                          levels = c("Not like me at all", 
                                     "Not like me",
                                     "A little like me",
                                     "Somewhat like me",
                                     "Like me",
                                     "Very much like me"), ordered = TRUE)
table(cand.sub$HEDO1)
levels(cand.sub$HEDO1)

cand.sub$ACHI1 <- factor(cand.sub$Q52, 
                          levels = c("Not like me at all", 
                                     "Not like me",
                                     "A little like me",
                                     "Somewhat like me",
                                     "Like me",
                                     "Very much like me"), ordered = TRUE)
table(cand.sub$ACHI1)
levels(cand.sub$ACHI1)

cand.sub$POWE1 <- factor(cand.sub$Q53, 
                          levels = c("Not like me at all", 
                                     "Not like me",
                                     "A little like me",
                                     "Somewhat like me",
                                     "Like me",
                                     "Very much like me"), ordered = TRUE)
table(cand.sub$POWE1)
levels(cand.sub$POWE1)

cand.sub$SECU1 <- factor(cand.sub$Q54, 
                          levels = c("Not like me at all", 
                                     "Not like me",
                                     "A little like me",
                                     "Somewhat like me",
                                     "Like me",
                                     "Very much like me"), ordered = TRUE)
table(cand.sub$SECU1)
levels(cand.sub$SECU1)

cand.sub$CONF2 <- factor(cand.sub$Q64, 
                          levels = c("Not like me at all", 
                                     "Not like me",
                                     "A little like me",
                                     "Somewhat like me",
                                     "Like me",
                                     "Very much like me"), ordered = TRUE)
table(cand.sub$CONF2)
levels(cand.sub$CONF2)

cand.sub$TRAD2 <- factor(cand.sub$Q65, 
                          levels = c("Not like me at all", 
                                     "Not like me",
                                     "A little like me",
                                     "Somewhat like me",
                                     "Like me",
                                     "Very much like me"), ordered = TRUE)
table(cand.sub$TRAD2)
levels(cand.sub$TRAD2)

cand.sub$BENE2 <- factor(cand.sub$Q66, 
                          levels = c("Not like me at all", 
                                     "Not like me",
                                     "A little like me",
                                     "Somewhat like me",
                                     "Like me",
                                     "Very much like me"), ordered = TRUE)
table(cand.sub$BENE2)
levels(cand.sub$BENE2)

cand.sub$UNIV2 <- factor(cand.sub$Q67, 
                          levels = c("Not like me at all", 
                                     "Not like me",
                                     "A little like me",
                                     "Somewhat like me",
                                     "Like me",
                                     "Very much like me"), ordered = TRUE)
table(cand.sub$UNIV2)
levels(cand.sub$UNIV2)

cand.sub$SELF2 <- factor(cand.sub$Q68, 
                          levels = c("Not like me at all", 
                                     "Not like me",
                                     "A little like me",
                                     "Somewhat like me",
                                     "Like me",
                                     "Very much like me"), ordered = TRUE)
table(cand.sub$SELF2)
levels(cand.sub$SELF2)

cand.sub$STIM2 <- factor(cand.sub$Q69, 
                          levels = c("Not like me at all", 
                                     "Not like me",
                                     "A little like me",
                                     "Somewhat like me",
                                     "Like me",
                                     "Very much like me"), ordered = TRUE)
table(cand.sub$STIM2)
levels(cand.sub$STIM2)

cand.sub$HEDO2 <- factor(cand.sub$Q70, 
                          levels = c("Not like me at all", 
                                     "Not like me",
                                     "A little like me",
                                     "Somewhat like me",
                                     "Like me",
                                     "Very much like me"), ordered = TRUE)
table(cand.sub$HEDO2)
levels(cand.sub$HEDO2)

cand.sub$ACHI2 <- factor(cand.sub$Q71, 
                          levels = c("Not like me at all", 
                                     "Not like me",
                                     "A little like me",
                                     "Somewhat like me",
                                     "Like me",
                                     "Very much like me"), ordered = TRUE)
table(cand.sub$ACHI2)
levels(cand.sub$ACHI2)

cand.sub$POWE2 <- factor(cand.sub$Q72, 
                          levels = c("Not like me at all", 
                                     "Not like me",
                                     "A little like me",
                                     "Somewhat like me",
                                     "Like me",
                                     "Very much like me"), ordered = TRUE)
table(cand.sub$POWE2)
levels(cand.sub$POWE2)

cand.sub$SECU2 <- factor(cand.sub$Q73, 
                          levels = c("Not like me at all", 
                                     "Not like me",
                                     "A little like me",
                                     "Somewhat like me",
                                     "Like me",
                                     "Very much like me"), ordered = TRUE)
table(cand.sub$SECU2)
levels(cand.sub$SECU2)

## Then turn to numeric
cand.sub$CONF1 <- as.numeric(cand.sub$CONF1)
cand.sub$CONF2 <- as.numeric(cand.sub$CONF2)
cand.sub$TRAD1 <- as.numeric(cand.sub$TRAD1)
cand.sub$TRAD2 <- as.numeric(cand.sub$TRAD2)
cand.sub$BENE1 <- as.numeric(cand.sub$BENE1)
cand.sub$BENE2 <- as.numeric(cand.sub$BENE2)
cand.sub$UNIV1 <- as.numeric(cand.sub$UNIV1)
cand.sub$UNIV2 <- as.numeric(cand.sub$UNIV2)
cand.sub$SELF1 <- as.numeric(cand.sub$SELF1)
cand.sub$SELF2 <- as.numeric(cand.sub$SELF2)
cand.sub$STIM1 <- as.numeric(cand.sub$STIM1)
cand.sub$STIM2 <- as.numeric(cand.sub$STIM2)
cand.sub$HEDO1 <- as.numeric(cand.sub$HEDO1)
cand.sub$HEDO2 <- as.numeric(cand.sub$HEDO2)
cand.sub$ACHI1 <- as.numeric(cand.sub$ACHI1)
cand.sub$ACHI2 <- as.numeric(cand.sub$ACHI2)
cand.sub$POWE1 <- as.numeric(cand.sub$POWE1)
cand.sub$POWE2 <- as.numeric(cand.sub$POWE2)
cand.sub$SECU1 <- as.numeric(cand.sub$SECU1)
cand.sub$SECU2 <- as.numeric(cand.sub$SECU2)

## 2. Raw value scores calculated for 10 lower order BHV
cand.sub$CONFORMITY <- (as.numeric(cand.sub$CONF1) + as.numeric(cand.sub$CONF2))/2
cand.sub$TRADITION <- (as.numeric(cand.sub$TRAD1) + as.numeric(cand.sub$TRAD2))/2
cand.sub$BENEVOLENCE <- (as.numeric(cand.sub$BENE1) + as.numeric(cand.sub$BENE2))/2
cand.sub$UNIVERSALISM <- (as.numeric(cand.sub$UNIV1) + as.numeric(cand.sub$UNIV2))/2
cand.sub$SELFDIRECTION <- (as.numeric(cand.sub$SELF1) + as.numeric(cand.sub$SELF2))/2
cand.sub$STIMULATION <- (as.numeric(cand.sub$STIM1) + as.numeric(cand.sub$STIM2))/2
cand.sub$HEDONISM <- (as.numeric(cand.sub$HEDO1) + as.numeric(cand.sub$HEDO2))/2
cand.sub$ACHIEVEMENT <- (as.numeric(cand.sub$ACHI1) + as.numeric(cand.sub$ACHI2))/2
cand.sub$POWER <- (as.numeric(cand.sub$POWE1) + as.numeric(cand.sub$POWE2))/2
cand.sub$SECURITY <- (as.numeric(cand.sub$SECU1) + as.numeric(cand.sub$SECU2))/2

## And calculate 4 higher order values

cand.sub$Conservation <- (as.numeric(cand.sub$CONF1) + as.numeric(cand.sub$CONF2) + as.numeric(cand.sub$TRAD1) + as.numeric(cand.sub$TRAD2) +
                            as.numeric(cand.sub$SECU1) + as.numeric(cand.sub$SECU2))/6

cand.sub$Selftranscendence <- (as.numeric(cand.sub$BENE1) + as.numeric(cand.sub$BENE2) + as.numeric(cand.sub$UNIV1) + as.numeric(cand.sub$UNIV2))/4

cand.sub$Openness <- (as.numeric(cand.sub$SELF1) + as.numeric(cand.sub$SELF2) + as.numeric(cand.sub$STIM1) + as.numeric(cand.sub$STIM2) +
                        as.numeric(cand.sub$HEDO1) + as.numeric(cand.sub$HEDO2))/6

cand.sub$Selfenhancement <- (as.numeric(cand.sub$ACHI1) + as.numeric(cand.sub$ACHI2) +
                               as.numeric(cand.sub$POWE1) + as.numeric(cand.sub$POWE2))/4

## 3. Calculate centred mean scores

cand.sub$MRAT <- (as.numeric(cand.sub$CONF1) + as.numeric(cand.sub$CONF2) + as.numeric(cand.sub$TRAD1) + as.numeric(cand.sub$TRAD2) +
                    as.numeric(cand.sub$BENE1) + as.numeric(cand.sub$BENE2) + as.numeric(cand.sub$UNIV1) + as.numeric(cand.sub$UNIV2) +
                    as.numeric(cand.sub$SELF1) + as.numeric(cand.sub$SELF2) + as.numeric(cand.sub$STIM1) + as.numeric(cand.sub$STIM2) +
                    as.numeric(cand.sub$HEDO1) + as.numeric(cand.sub$HEDO2) + as.numeric(cand.sub$ACHI1) + as.numeric(cand.sub$ACHI2) +
                    as.numeric(cand.sub$POWE1) + as.numeric(cand.sub$POWE2) + as.numeric(cand.sub$SECU1) + as.numeric(cand.sub$SECU2))/20

cand.sub$CONFC <- cand.sub$CONFORMITY - cand.sub$MRAT
cand.sub$TRADC <- cand.sub$TRADITION - cand.sub$MRAT
cand.sub$BENEC <- cand.sub$BENEVOLENCE - cand.sub$MRAT
cand.sub$UNIVC <- cand.sub$UNIVERSALISM - cand.sub$MRAT
cand.sub$SELFC <- cand.sub$SELFDIRECTION - cand.sub$MRAT
cand.sub$STIMC <- cand.sub$STIMULATION - cand.sub$MRAT
cand.sub$HEDOC <- cand.sub$HEDONISM - cand.sub$MRAT
cand.sub$ACHIC <- cand.sub$ACHIEVEMENT - cand.sub$MRAT
cand.sub$POWEC <- cand.sub$POWER - cand.sub$MRAT
cand.sub$SECUC <- cand.sub$SECURITY - cand.sub$MRAT

cand.sub$CONC <- cand.sub$Conservation - cand.sub$MRAT
cand.sub$STC <- cand.sub$Selftranscendence - cand.sub$MRAT
cand.sub$OPENC <- cand.sub$Openness - cand.sub$MRAT
cand.sub$SEC <- cand.sub$Selfenhancement - cand.sub$MRAT

###### NOTE THAT DATA ON THE PUBLIC FOR EACH VARIABLE HAS BEEN ADDED FROM THE ESS #######
###### A STANDALONE SPSS DATAFILE, TAILORED TO SHOW CODED VARIABLES COPIED ACROSS TO THIS FILE, IS AVAILBALE ON DATAVERSE FOR REFERENCE ######

## Create a rescaling formula for housekeeping
range01 <- function(x, ...){(x-min(x, ...))/(max(x, ...)-min(x, ...))}

# Re-scale all centred mean values on a 0-1 scale
cand.sub$CONFORMITY1 <- range01(cand.sub$CONFC, na.rm = T)
cand.sub$TRADITION1 <- range01(cand.sub$TRADC, na.rm = T) 
cand.sub$BENEVOLENCE1 <- range01(cand.sub$BENEC, na.rm = T)  
cand.sub$UNIVERSALISM1 <- range01(cand.sub$UNIVC, na.rm = T)
cand.sub$SELFDIRECTION1 <- range01(cand.sub$SELFC, na.rm = T)
cand.sub$STIMULATION1 <- range01(cand.sub$STIMC, na.rm = T)
cand.sub$HEDONISM1 <- range01(cand.sub$HEDOC, na.rm = T)
cand.sub$ACHIEVEMENT1 <- range01(cand.sub$ACHIC, na.rm = T)
cand.sub$POWER1 <- range01(cand.sub$POWEC, na.rm = T)
cand.sub$SECURITY1 <- range01(cand.sub$SECUC, na.rm = T)

## Data cleaning for other relevant variables with mixed string and numeric entries (i.e. result from copying across public data from ESS7)
# 1. Status
cand.sub$ELECMP[cand.sub$ELECMP == 1] <- "Candidate"
cand.sub$ELECMP[cand.sub$ELECMP == 2] <- "Public"
cand.sub$ELECMP[cand.sub$ELECMP == 3] <- "Member of Parliament"
cand.sub$ELECMP[cand.sub$ELECMP == ""] <- NA
table(cand.sub$ELECMP)

## Add new dependent variables

cand.sub$pol <- ifelse(cand.sub$ELECMP != "Public", 1, 0)

cand.sub$MP <- ifelse(cand.sub$ELECMP == "Member of Parliament", 1, 0)

table(cand.sub$MP)
table(cand.sub$pol)

# Gender
cand.sub$gender[cand.sub$gender == 1] <- "Male"
cand.sub$gender[cand.sub$gender == 2] <- "Female"
cand.sub$gender[cand.sub$gender == ""] <- NA
table(cand.sub$gender)

# Age
cand.sub$Q116[cand.sub$Q116==999] <- NA
cand.sub$Q116[cand.sub$Q116==""] <- NA
cand.sub$Age1 <- range01(cand.sub$Q116, na.rm = T)

# Partisanship
cand.sub$LeftRight[cand.sub$LeftRight == 1] <- "Centre->Left"
cand.sub$LeftRight[cand.sub$LeftRight == 2] <- "Centre->Right"
cand.sub$LeftRight[cand.sub$LeftRight == ""] <- NA
table(cand.sub$LeftRight)

# Occupation
cand.sub$Q57[cand.sub$Q57 == ""] <- NA
cand.sub$Q57[cand.sub$Q57 == 1] <- "Brokerage"
cand.sub$Q57[cand.sub$Q57 == 2] <- "Public Sector Professional"
cand.sub$Q57[cand.sub$Q57 == 3] <- "Manual"
cand.sub$Q57[cand.sub$Q57 == 4] <- "Administrative"
cand.sub$Q57[cand.sub$Q57 == 5] <- "Charitable/'Helping'"
cand.sub$Q57[cand.sub$Q57 == 6] <- "Other"
cand.sub$Q57[cand.sub$Q57 == 7] <- "Other"
cand.sub$Q57[cand.sub$Q57 == 999] <- NA
table(cand.sub$Q57)

# Education
cand.sub$Education[cand.sub$Education == 999] <- NA
cand.sub$Education[cand.sub$Education == 1] <- "Postgraduate Degree"
cand.sub$Education[cand.sub$Education == 2] <- "Higher Education Degree"
cand.sub$Education[cand.sub$Education == 3] <- "A-Levels/Vocational Diploma"
cand.sub$Education[cand.sub$Education == 4] <- "Apprenticeship"
cand.sub$Education[cand.sub$Education == 5] <- "None"
cand.sub$Education[cand.sub$Education == ""] <- NA
table(cand.sub$Education)

cand.sub$Education <- factor(cand.sub$Education, 
                              levels = c("None", 
                                         "Apprenticeship",
                                         "A-Levels/Vocational Diploma",
                                         "Higher Education Degree",
                                         "Postgraduate Degree"), ordered = TRUE)
cand.sub$EDUCATION <- (as.numeric(cand.sub$Education))
table(cand.sub$EDUCATION)

# Prior experience
cand.sub$Q117[cand.sub$Q117 == 1] <- "YES"
cand.sub$Q117[cand.sub$Q117 == 0] <- "NO"
table(cand.sub$Q117)


########## ANALYSIS ##########

# 1. Check measurement invariance between MPs and unsuccessful candidates

## Create new subset of MPs and unsuccessful candidates
cand.sub2 <- subset(cand.sub, pol == 1)
cand.sub2$survey <- ifelse(cand.sub2$ELECMP == "Member of Parliament", 1,
                           ifelse(cand.sub2$ELECMP == "Candidate", 2, NA))
table(cand.sub2$survey)

MODEL <- 
  'CONF =~ CONF1 + CONF2 
TRAD =~ TRAD1 + TRAD2 
SECU =~ SECU1 + SECU2
BENE =~ BENE1 + BENE2
UNIV =~ UNIV1 + UNIV2
ACHI =~ ACHI1 + ACHI2 
POWE =~ POWE1 + POWE2
SELF =~ SELF1 + SELF2
STIM =~ STIM1 + STIM2
HEDO =~ HEDO1 + HEDO2'

Fit <- cfa(MODEL, data = cand.sub2, missing = "fiml")
summary(Fit, fit.measures = TRUE, standardized = TRUE)

Model1 <- cfa(MODEL, data = cand.sub2, missing = "fiml", group = "survey") ## Configural invariance
summary(Model1, fit.measures = TRUE, standardized = TRUE)

Model2 <- cfa(MODEL, data = cand.sub2, missing = "fiml", group = "survey",
              group.equal = c("loadings")) ## Metric invariance
summary(Model2, fit.measures = TRUE, standardized = TRUE)

anova(Model1, Model2)

Model3 <- cfa(MODEL, data = cand.sub2, missing = "fiml", group = "survey",
              group.equal = c("loadings", "intercepts")) ## Scalar invariance
summary(Model3, fit.measures = TRUE, standardized = TRUE)

anova(Model2, Model3)

Model4 <- cfa(MODEL, data = cand.sub2, missing = "fiml", group = "survey",
              group.equal = c("loadings", "intercepts", "residuals")) # Strict invariance

summary(Model4, fit.measures = TRUE, standardized = TRUE)
anova(Model3, Model4)

MODEL5 <- cfa(MODEL, data = cand.sub2, missing = "fiml", group = "survey",
              group.equal = c("loadings", "intercepts", "residuals", "lv.variances",
                              "lv.covariances")) # Structural invariance

summary(MODEL5, fit.measures = TRUE, standardized = TRUE)
anova(Model4, MODEL5)

### NOTE: metric and scalar invariance assumed on 10 factor model.

### Examine mean differences across groups
## Create a loop function to run multiple t-tests in one go
library(survival)

multi.tests <- function(fun = t.test, df, vars, group.var, ...) {
  sapply(simplify = FALSE,                                    # sapply(simplify=T) better, elements named
         vars,                                                # loop on vector of outcome variable names
         function(var) {
           formula <- as.formula(paste(var, "~", group.var))# create a formula with outcome and grouping var.
           fun(data = df, formula, ...)                     # perform test with a given fun, default t.test
         }
  )
}

## T-tests between MPs and public
table(cand.sub$ELECMP)
cand.sub3 <- subset(cand.sub, ELECMP != "Candidate")
cand.sub3$survey <- ifelse(cand.sub3$ELECMP == "Member of Parliament", 1,
                           ifelse(cand.sub3$ELECMP == "Public", 2, NA))
table(cand.sub3$survey)

res.multi.t.tests <-
  multi.tests(fun = t.test,
              df = cand.sub3,
              vars = c("CONFC", "TRADC", "BENEC", "UNIVC", "SELFC", "STIMC",
                       "HEDOC", "ACHIC", "POWEC", "SECUC"),
              group.var = "ELECMP",
              var.equal = TRUE)
res.multi.t.tests

## Extract just p-values for results 
p <- data.frame(p.value = sapply(res.multi.t.tests, getElement, name = "p.value"))

## Apply Bonferroni corrections

pairwise.t.test(cand.sub3$CONFC, 
                cand.sub3$ELECMP, 
                p.adjust = "bonferroni")

pairwise.t.test(cand.sub3$TRADC, 
                cand.sub3$ELECMP, 
                p.adjust = "bonferroni")

pairwise.t.test(cand.sub3$BENEC, 
                cand.sub3$ELECMP, 
                p.adjust = "bonferroni")

pairwise.t.test(cand.sub3$UNIVC, 
                cand.sub3$ELECMP, 
                p.adjust = "bonferroni")

pairwise.t.test(cand.sub3$SELFC, 
                cand.sub3$ELECMP, 
                p.adjust = "bonferroni")

pairwise.t.test(cand.sub3$STIMC, 
                cand.sub3$ELECMP, 
                p.adjust = "bonferroni")

pairwise.t.test(cand.sub3$HEDOC, 
                cand.sub3$ELECMP, 
                p.adjust = "bonferroni")

pairwise.t.test(cand.sub3$ACHIC, 
                cand.sub3$ELECMP, 
                p.adjust = "bonferroni")

pairwise.t.test(cand.sub3$POWEC, 
                cand.sub3$ELECMP, 
                p.adjust = "bonferroni")

pairwise.t.test(cand.sub3$SECUC, 
                cand.sub3$ELECMP, 
                p.adjust = "bonferroni")


## T-tests between matched sample of MPs and public, with Bonferroni corrections

Match <- read.spss(to.data.frame=TRUE, "TYPE FILE NAME HERE")
names(Match)
table(Match$MPPub)

res.multi.t.tests1 <-
  multi.tests(fun = t.test,
              df = Match,
              vars = c("ConformityCentred", "TraditionCentred", "BenevolenceCentred", "UniversalismCentred", 
                       "SelfDirectCentred", "StimulationCentred",
                       "HedonismCentred", "AchievementCentred", "PowerCentred", "SecurityCentred"),
              group.var = "MPPub",
              var.equal = TRUE)
res.multi.t.tests1

## Extract just p-values for results 
data.frame(p.value = sapply(res.multi.t.tests1, getElement, name = "p.value"))

pairwise.t.test(Match$ConformityCentred, 
                Match$MPPub, 
                p.adjust = "bonferroni")

pairwise.t.test(Match$TraditionCentred, 
                Match$MPPub, 
                p.adjust = "bonferroni")

pairwise.t.test(Match$BenevolenceCentred, 
                Match$MPPub, 
                p.adjust = "bonferroni")

pairwise.t.test(Match$UniversalismCentred, 
                Match$MPPub, 
                p.adjust = "bonferroni")

pairwise.t.test(Match$SelfDirectCentred, 
                Match$MPPub, 
                p.adjust = "bonferroni")

pairwise.t.test(Match$StimulationCentred, 
                Match$MPPub, 
                p.adjust = "bonferroni")

pairwise.t.test(Match$HedonismCentred, 
                Match$MPPub, 
                p.adjust = "bonferroni")

pairwise.t.test(Match$AchievementCentred, 
                Match$MPPub, 
                p.adjust = "bonferroni")

pairwise.t.test(Match$PowerCentred, 
                Match$MPPub, 
                p.adjust = "bonferroni")

pairwise.t.test(Match$SecurityCentred, 
                Match$MPPub, 
                p.adjust = "bonferroni")

## T-tests between public and candidates, with Bonferroni corrections
## To diminish chance of selection effects, we'd still expect to see diff on Power values
table(cand.sub$ELECMP)
cand.sub4 <- subset(cand.sub, ELECMP != "Member of Parliament")
cand.sub4$survey <- ifelse(cand.sub4$ELECMP == "Candidate", 1,
                           ifelse(cand.sub4$ELECMP == "Public", 2, NA))
table(cand.sub4$survey)

res.multi.t.tests2 <-
  multi.tests(fun = t.test,
              df = cand.sub4,
              vars = c("CONFC", "TRADC", "BENEC", "UNIVC", "SELFC", "STIMC",
                       "HEDOC", "ACHIC", "POWEC", "SECUC"),
              group.var = "ELECMP",
              var.equal = TRUE)
res.multi.t.tests2

## Create a dataframe for comparing lower order values

dat1 <- data.frame(
  Party = factor(c("Elected MPs","Unsuccessful Candidates","British Public (matched to MPs)","British Public ",
                   "Elected MPs","Unsuccessful Candidates","British Public (matched to MPs)","British Public ",
                   "Elected MPs","Unsuccessful Candidates","British Public (matched to MPs)","British Public ",
                   "Elected MPs","Unsuccessful Candidates","British Public (matched to MPs)","British Public ",
                   "Elected MPs","Unsuccessful Candidates","British Public (matched to MPs)","British Public ",
                   "Elected MPs","Unsuccessful Candidates","British Public (matched to MPs)","British Public ",
                   "Elected MPs","Unsuccessful Candidates","British Public (matched to MPs)","British Public ",
                   "Elected MPs","Unsuccessful Candidates","British Public (matched to MPs)","British Public ",
                   "Elected MPs","Unsuccessful Candidates","British Public (matched to MPs)","British Public ",
                   "Elected MPs","Unsuccessful Candidates","British Public (matched to MPs)","British Public "
  ), levels=c("Elected MPs","Unsuccessful Candidates","British Public (matched to MPs)","British Public ")),
  Value = factor(c("CF", "CF", "CF", "CF", "TR",
                   "TR", "TR", "TR", "BE", "BE", "BE",
                   "BE", "UN", "UN", "UN", "UN",
                   "SD", "SD", "SD", "SD", "ST",
                   "ST", "ST", "ST", "HE", "HE", "HE",
                   "HE", "AC", "AC", "AC", "AC", "PO",
                   "PO", "PO", "PO", "SE", "SE", "SE", "SE"), 
                 levels=c("CF", "TR", "BE", "UN", "SD",
                          "ST", "HE", "AC", "PO", "SE")),
  Mean = c(0.0079, -0.5527, -0.2734, -0.1931, 
           -0.9623, -1.6341, -0.0512, 0.0471,
           1.1514, 0.9550, 0.8225, 0.8521,
           0.8792, 1.1255, 0.8595, 0.6440,
           0.6415, 1.1759, 0.5548, 0.4647,
           -0.1455, 0.3658, -0.6926, -0.6422,
           -0.4920, -0.0178, -0.5108, -0.4085,
           -0.4673, -0.5759, -0.4300, -0.5577,
           -0.4425, -0.4286, -1.0764, -1.0819,
           -0.1702, -0.4131, 0.3679, 0.5531))
dat1

## Visualise differences

ggplot(data=dat1, aes(x=Value, y=Mean, group=Party, shape=Party)) + 
  geom_line(aes(linetype=Party), size=1) + 
  geom_point(size=3, fill="white") +
  scale_shape_manual(values=c(22,21,23,24))+
  xlab("Basic Values") + ylab("Centred Mean Scores")+
  guides(color = guide_legend("Sample")) +
  theme_bw() +
  theme(
    axis.title.y = element_text(vjust= 2.0),
    axis.title.x = element_text(vjust= -1.5),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18, face = "bold"))

## Extract just p-values for results 
data.frame(p.value = sapply(res.multi.t.tests2, getElement, name = "p.value"))

## Apply Bonferroni corrections

pairwise.t.test(cand.sub4$CONFC, 
                cand.sub4$ELECMP, 
                p.adjust = "bonferroni")

pairwise.t.test(cand.sub4$TRADC, 
                cand.sub4$ELECMP, 
                p.adjust = "bonferroni")

pairwise.t.test(cand.sub4$BENEC, 
                cand.sub4$ELECMP, 
                p.adjust = "bonferroni")

pairwise.t.test(cand.sub4$UNIVC, 
                cand.sub4$ELECMP, 
                p.adjust = "bonferroni")

pairwise.t.test(cand.sub4$SELFC, 
                cand.sub4$ELECMP, 
                p.adjust = "bonferroni")

pairwise.t.test(cand.sub4$STIMC, 
                cand.sub4$ELECMP, 
                p.adjust = "bonferroni")

pairwise.t.test(cand.sub4$HEDOC, 
                cand.sub4$ELECMP, 
                p.adjust = "bonferroni")

pairwise.t.test(cand.sub4$ACHIC, 
                cand.sub4$ELECMP, 
                p.adjust = "bonferroni")

pairwise.t.test(cand.sub4$POWEC, 
                cand.sub4$ELECMP, 
                p.adjust = "bonferroni")

pairwise.t.test(cand.sub4$SECUC, 
                cand.sub4$ELECMP, 
                p.adjust = "bonferroni")

## Binary logits using all politicians and then just MPs

## Create occupational dummy variables
table(cand.sub$Q57)
cand.sub$Brokerage <- ifelse(cand.sub$Q57 == "Brokerage", 1, 0)
cand.sub$Charitable <- ifelse(cand.sub$Q57 == "Charitable/'Helping'", 1, 0)
cand.sub$Manual <- ifelse(cand.sub$Q57 == "Manual" |
                            cand.sub$Q57 == "Administrative", 1, 0)
cand.sub$Public <- ifelse(cand.sub$Q57 == "Public Sector Professional", 1, 0)
table(cand.sub$Charitable)

## 1. Values only
table(cand.sub$pol)

model1 <- glm(cand.sub$pol ~ cand.sub$CONFORMITY1 + cand.sub$TRADITION1 + cand.sub$BENEVOLENCE1 +
                cand.sub$UNIVERSALISM1 + cand.sub$SELFDIRECTION1 + cand.sub$STIMULATION1 +
                cand.sub$POWER1 + cand.sub$SECURITY1, data = cand.sub, family = binomial(link = 'logit'))
summary(model1)

## Robust standard errors
install.packages("sandwich")
library(sandwich)
cov.m1 <- vcovHC(model1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(model1), "Robust SE" = std.err,
                "Pr(>|z|)" = 2 * pnorm(abs(coef(model1)/std.err), lower.tail=FALSE),
                LL = coef(model1) - 1.96 * std.err,
                UL = coef(model1) + 1.96 * std.err)
r.est

## Pseudo-R squared
library(DescTools)
PseudoR2(model1, which = "CoxSnell")

## 2. + Demographics
table(cand.sub$Age1)

model2 <- glm(cand.sub$pol ~ cand.sub$CONFORMITY1 + cand.sub$TRADITION1 + cand.sub$BENEVOLENCE1 +
                cand.sub$UNIVERSALISM1 + cand.sub$SELFDIRECTION1 + cand.sub$STIMULATION1 +
                cand.sub$POWER1 + cand.sub$SECURITY1 + cand.sub$gender + cand.sub$Age1, data = cand.sub, family = binomial(link = 'logit'))
summary(model2)

## Robust standard errors
cov.m1 <- vcovHC(model2, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est1 <- cbind(Estimate= coef(model2), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(model2)/std.err), lower.tail=FALSE),
               LL = coef(model2) - 1.96 * std.err,
               UL = coef(model2) + 1.96 * std.err)
r.est1

## Pseudo-R squared
PseudoR2(model2, which = "CoxSnell")

## 3. + Socio-economics
table(cand.sub$Age1)

model3 <- glm(cand.sub$pol ~ cand.sub$CONFORMITY1 + cand.sub$TRADITION1 + cand.sub$BENEVOLENCE1 +
                cand.sub$UNIVERSALISM1 + cand.sub$SELFDIRECTION1 + cand.sub$STIMULATION1 +
                cand.sub$POWER1 + cand.sub$SECURITY1 + cand.sub$gender + cand.sub$Age1 +
                cand.sub$EDUCATION + cand.sub$Brokerage + cand.sub$Charitable +
                cand.sub$Manual + cand.sub$Public, data = cand.sub, family = binomial(link = 'logit'))
summary(model3)

## Robust standard errors
cov.m1 <- vcovHC(model3, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est2 <- cbind(Estimate= coef(model3), "Robust SE" = std.err,
                "Pr(>|z|)" = 2 * pnorm(abs(coef(model3)/std.err), lower.tail=FALSE),
                LL = coef(model3) - 1.96 * std.err,
                UL = coef(model3) + 1.96 * std.err)
r.est2

## Pseudo-R squared
PseudoR2(model3, which = "CoxSnell")

## 4. + Political Experience
table(cand.sub$Age1)

model4 <- glm(pol ~ CONFORMITY1 + TRADITION1 + BENEVOLENCE1 +
                UNIVERSALISM1 + SELFDIRECTION1 + STIMULATION1 +
                POWER1 + SECURITY1 + gender + Age1 +
                EDUCATION + Brokerage + Charitable +
                Manual + Public + Q117, data = cand.sub, family = binomial(link = 'logit'))
summary(model4)

## Robust standard errors
cov.m1 <- vcovHC(model4, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est3 <- cbind(Estimate= coef(model4), "Robust SE" = std.err,
                "Pr(>|z|)" = 2 * pnorm(abs(coef(model4)/std.err), lower.tail=FALSE),
                LL = coef(model4) - 1.96 * std.err,
                UL = coef(model4) + 1.96 * std.err)
r.est3

## Pseudo-R squared
PseudoR2(model4, which = "CoxSnell")

## 5. MPs only
names(cand.sub)
table(cand.sub$MP)

model5 <- glm(MP ~ CONFORMITY1 + TRADITION1 + BENEVOLENCE1 +
                UNIVERSALISM1 + SELFDIRECTION1 + STIMULATION1 +
                POWER1 + SECURITY1 + gender + Age1 +
                EDUCATION + Brokerage + Charitable +
                Manual + Public + Q117, data = cand.sub, family = binomial(link = 'logit'))
summary(model5)

## Robust standard errors
cov.m1 <- vcovHC(model5, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est4 <- cbind(Estimate= coef(model5), "Robust SE" = std.err,
                "Pr(>|z|)" = 2 * pnorm(abs(coef(model5)/std.err), lower.tail=FALSE),
                LL = coef(model5) - 1.96 * std.err,
                UL = coef(model5) + 1.96 * std.err)
r.est4

## Pseudo-R squared
PseudoR2(model5, which = "CoxSnell")

## Without age included:

model6 <- glm(MP ~ CONFORMITY1 + TRADITION1 + BENEVOLENCE1 +
                UNIVERSALISM1 + SELFDIRECTION1 + STIMULATION1 +
                POWER1 + SECURITY1 + gender +
                EDUCATION + Brokerage + Charitable +
                Manual + Public + Q117, data = cand.sub, family = binomial(link = 'logit'))
summary(model6)

## Robust standard errors
cov.m1 <- vcovHC(model6, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est4 <- cbind(Estimate= coef(model6), "Robust SE" = std.err,
                "Pr(>|z|)" = 2 * pnorm(abs(coef(model6)/std.err), lower.tail=FALSE),
                LL = coef(model6) - 1.96 * std.err,
                UL = coef(model6) + 1.96 * std.err)
r.est4

## Pseudo-R squared
PseudoR2(model6, which = "CoxSnell")


## Save output as HTML - remember to change standard errors!

## As Html
stargazer(model1, model2, model3, model4, model5, column.labels = c("Values only", 
                                                            "+ Demographics",
                                                            "+ Professional Resources",
                                                            "+ Political Opportunity",
                                                            "Full model for Elected MPs"), 
          type = "text", title = "Table 4. Basic values and candidate emergence in the UK.",
          dep.var.labels=c("Candidate Emergence"),
          covariate.labels = c("Conformity","Tradition", "Benevolence", "Universalism",
                               "Self-Direction", "Stimulation", "Power",
                               "Security", "Gender", "Age", "Education", "Brokerage",
                               "Charity/'Helping'", "Manual/Administrative", "Public Sector Professional", 
                               "Prior Experience"), 
          omit.stat=c("ser","f"), no.space=TRUE, out = "Candidate_emergence.htm")

## HTML of model 6 for appendix

## As Html
stargazer(model6, column.labels = c("Full model for Elected MPs (minus controls for age)"), 
          type = "text", title = "Appendix A. Basic values and candidate emergence in the UK.",
          dep.var.labels=c("Candidate Emergence"),
          covariate.labels = c("Conformity","Tradition", "Benevolence", "Universalism",
                               "Self-Direction", "Stimulation", "Power",
                               "Security", "Gender", "Education", "Brokerage",
                               "Charity/'Helping'", "Manual/Administrative", "Public Sector Professional", 
                               "Prior Experience"), 
          omit.stat=c("ser","f"), no.space=TRUE, out = "Candidate_emergence_appendix.htm")


## Odds ratios of final models
or_glm(cand.sub, model4, CI = 0.95)
or_glm(cand.sub, model5, CI = 0.95)


## AME for logits 4 and 5
install.packages("margins")
library("margins")
M <- margins(model4)
M = summary(M) 

M1 <- margins(model5)
M2 = summary(M1) 

## Combine in one plot

M$model <- "A"
M2$model <- "B"
dfc <- rbind(M, M2)

ggplot(dfc, aes(factor, AME, group = model, shape = model, colour = model)) +
  geom_point(position=position_dodge(width=0.7), size = 2) +
  geom_errorbar(aes(x = factor, ymin = lower, ymax = upper),
                width = .7,position=position_dodge(width=0.7)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  labs(x = NULL, y = "Average Marginal Effects") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        legend.text = element_text(size=12)) +
  scale_x_discrete(limits=c("CONFORMITY1", "TRADITION1", "BENEVOLENCE1",
                            "UNIVERSALISM1", "SELFDIRECTION1", "STIMULATION1",
                            "POWER1", "SECURITY1", "genderMale", "Age1",
                            "EDUCATION", "Brokerage", "Charitable",
                            "Manual", "Public", "Q117YES"),
                   labels=c("CONFORMITY1" = "Conformity (0-1)", "TRADITION1" = "Tradition (0-1)", "BENEVOLENCE1" = "Benevolence (0-1)",
                            "UNIVERSALISM1" = "Universalism (0-1)", "SELFDIRECTION1" = "Self-Direction (0-1)",
                            "STIMULATION1" = "Stimulation (0-1)", "POWER1" = "Power (0-1)",
                            "SECURITY1" = "Security (0-1)", "genderMale"= "Gender (Male)", "Age1" = "Age (0-1)", 
                            "EDUCATION" = "Education", "Brokerage" = "Brokerage",
                            "Charitable" = "Charity/'Helping'", "Manual" = "Manual/Administrative", 
                            "Public" = "Public Sector Professional", 
                            "Q117YES" = "Political Experience")) +
  scale_color_manual(labels = c("All Candidates", "Elected MPs"), values = c("black", "grey")) +
  guides(color = guide_legend("")) +
  scale_shape_manual(labels = c("All Candidates", "Elected MPs"), values = c(19, 17)) +
  guides(shape = guide_legend(""))

## EFA on MP sample for inter-item correlations between Conformity and Benevolence
x <- cbind(cand.sub2$CONF1, cand.sub2$CONF2, cand.sub2$TRAD1, 
           cand.sub2$TRAD2, cand.sub2$SECU1, cand.sub2$SECU2, cand.sub2$BENE1, 
           cand.sub2$BENE2, cand.sub2$UNIV1, cand.sub2$UNIV2,
           cand.sub2$ACHI1, cand.sub2$ACHI2, cand.sub2$POWE1, cand.sub2$POWE2,
           cand.sub2$SELF1, cand.sub2$SELF2, cand.sub2$STIM1, cand.sub2$STIM2, 
           cand.sub2$HEDO1, cand.sub2$HEDO2)
summary(x)
cor(na.omit(x))

## 10 factor model
fac1 <- factanal(na.omit(x), factors = 10, rotation = "varimax")
fac1


MP <- subset(cand.sub2, ELECMP == "Member of Parliament")
cor.test(MP$CONF1, MP$CONF2, use = "pairwise.complete.obs", method = "pearson")
cor.test(MP$CONF1, MP$BENE1, use = "pairwise.complete.obs", method = "pearson")
cor.test(MP$CONF1, MP$BENE2, use = "pairwise.complete.obs", method = "pearson")
cor.test(MP$CONF2, MP$BENE1, use = "pairwise.complete.obs", method = "pearson")
cor.test(MP$CONF2, MP$BENE2, use = "pairwise.complete.obs", method = "pearson")

## Compare original and altered CFA models on fit statistics
## Conf1 loads onto security and conf2 onto benevolence

MODEL <- 
  'TRAD =~ TRAD1 + TRAD2 
SECU =~ SECU1 + SECU2 + CONF1
BENE =~ BENE1 + BENE2 + CONF2
UNIV =~ UNIV1 + UNIV2
ACHI =~ ACHI1 + ACHI2 
POWE =~ POWE1 + POWE2
SELF =~ SELF1 + SELF2
STIM =~ STIM1 + STIM2
HEDO =~ HEDO1 + HEDO2'

Fit <- cfa(MODEL, data = cand.sub2, missing = "fiml")
summary(Fit, fit.measures = TRUE, standardized = TRUE)
