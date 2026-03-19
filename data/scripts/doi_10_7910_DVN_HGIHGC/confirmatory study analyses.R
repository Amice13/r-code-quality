
library(dplyr)
library(effects)
library(ggplot2)
library(jtools)
library(lavaan)
library(nlme)
library(psych)
library(reghelper)
library(RSA)
library(stargazer)
library(summarytools)

setwd ("ENTER PATH")
data <- read.csv("confirmatory study data.csv", header=TRUE, stringsAsFactors = FALSE)

#### code SES variables ####

#### subjective SES

# current SES ladder
# code to see if participants selected one and only one rung of the ladder
# participants who did not select only one rung are assigned a missing value below
data$rung1selected <- 0; data$rung1selected[data$currentSESladder_12=="On"] <- 1
data$rung2selected <- 0; data$rung2selected[data$currentSESladder_9=="On"] <- 1
data$rung3selected <- 0; data$rung3selected[data$currentSESladder_8=="On"] <- 1
data$rung4selected <- 0; data$rung4selected[data$currentSESladder_7=="On"] <- 1
data$rung5selected <- 0; data$rung5selected[data$currentSESladder_6=="On"] <- 1
data$rung6selected <- 0; data$rung6selected[data$currentSESladder_5=="On"] <- 1
data$rung7selected <- 0; data$rung7selected[data$currentSESladder_4=="On"] <- 1
data$rung8selected <- 0; data$rung8selected[data$currentSESladder_3=="On"] <- 1
data$rung9selected <- 0; data$rung9selected[data$currentSESladder_2=="On"] <- 1
data$rung10selected <- 0; data$rung10selected[data$currentSESladder_1=="On"] <- 1
data$totalrungsselected <- with(data, (rowSums(cbind(rung1selected,rung2selected,rung3selected,rung4selected,rung5selected,rung6selected,rung7selected,rung8selected,rung9selected,rung10selected), na.rm=TRUE)))
table(data$totalrungsselected)
# assign SES ladder values, plus missing values to those who did not select only one rung 
data$currentSESladder <- NA; 
data$currentSESladder[data$currentSESladder_12=="On"] <- 1
data$currentSESladder[data$currentSESladder_9=="On"] <- 2
data$currentSESladder[data$currentSESladder_8=="On"] <- 3
data$currentSESladder[data$currentSESladder_7=="On"] <- 4
data$currentSESladder[data$currentSESladder_6=="On"] <- 5
data$currentSESladder[data$currentSESladder_5=="On"] <- 6
data$currentSESladder[data$currentSESladder_4=="On"] <- 7
data$currentSESladder[data$currentSESladder_3=="On"] <- 8
data$currentSESladder[data$currentSESladder_2=="On"] <- 9
data$currentSESladder[data$currentSESladder_1=="On"] <- 10
data$currentSESladder[data$totalrungsselected!=1] <- NA
table(data$currentSESladder)
describe(data$currentSESladder)

# code current subjective SES 3-item scale
data$currentSESVlad_currentSESVlad_1 <- as.numeric(data$currentSESVlad_currentSESVlad_1); data$currentSESVlad_currentSESVlad_2 <- as.numeric(data$currentSESVlad_currentSESVlad_2); data$currentSESVlad_currentSESVlad_3<- as.numeric(data$currentSESVlad_currentSESVlad_3)
data$current3item <- with(data, (rowMeans(cbind(currentSESVlad_currentSESVlad_1,currentSESVlad_currentSESVlad_2,currentSESVlad_currentSESVlad_3), na.rm=TRUE)))
with(data, psych::alpha(data.frame(currentSESVlad_currentSESVlad_1,currentSESVlad_currentSESVlad_2,currentSESVlad_currentSESVlad_3), na.rm=TRUE, check.keys=FALSE))
describe(data$current3item)

# code childhood ladder
# code to see if participants selected one and only one early.rung of the ladder
# participants who did not select only one early.rung are assigned a missing value below
data$early.rung1selected <- 0; data$early.rung1selected[data$earlySESladder_4=="On"] <- 1
data$early.rung2selected <- 0; data$early.rung2selected[data$earlySESladder_5=="On"] <- 1
data$early.rung3selected <- 0; data$early.rung3selected[data$earlySESladder_6=="On"] <- 1
data$early.rung4selected <- 0; data$early.rung4selected[data$earlySESladder_7=="On"] <- 1
data$early.rung5selected <- 0; data$early.rung5selected[data$earlySESladder_8=="On"] <- 1
data$early.rung6selected <- 0; data$early.rung6selected[data$earlySESladder_9=="On"] <- 1
data$early.rung7selected <- 0; data$early.rung7selected[data$earlySESladder_10=="On"] <- 1
data$early.rung8selected <- 0; data$early.rung8selected[data$earlySESladder_11=="On"] <- 1
data$early.rung9selected <- 0; data$early.rung9selected[data$earlySESladder_12=="On"] <- 1
data$early.rung10selected <- 0; data$early.rung10selected[data$earlySESladder_13=="On"] <- 1
data$totalearly.rungsselected <- with(data, (rowSums(cbind(early.rung1selected,early.rung2selected,early.rung3selected,early.rung4selected,early.rung5selected,early.rung6selected,early.rung7selected,early.rung8selected,early.rung9selected,early.rung10selected), na.rm=TRUE)))
table(data$totalearly.rungsselected)
# assign SES ladder values, plus missing values to those who did not select only one rung 
data$earlySESladder <- NA; 
data$earlySESladder[data$earlySESladder_4=="On"] <- 1
data$earlySESladder[data$earlySESladder_5=="On"] <- 2
data$earlySESladder[data$earlySESladder_6=="On"] <- 3
data$earlySESladder[data$earlySESladder_7=="On"] <- 4
data$earlySESladder[data$earlySESladder_8=="On"] <- 5
data$earlySESladder[data$earlySESladder_9=="On"] <- 6
data$earlySESladder[data$earlySESladder_10=="On"] <- 7
data$earlySESladder[data$earlySESladder_11=="On"] <- 8
data$earlySESladder[data$earlySESladder_12=="On"] <- 9
data$earlySESladder[data$earlySESladder_13=="On"] <- 10
data$earlySESladder[data$totalearlyrungsselected!=1] <- NA
table(data$earlySESladder)
describe(data$earlySESladder)

# code childhood subjective SES 3-item scale
data$earlySESVlad_earlySESVlad_1 <- as.numeric(data$earlySESVlad_earlySESVlad_1); data$earlySESVlad_earlySESVlad_2 <- as.numeric(data$earlySESVlad_earlySESVlad_2); data$earlySESVlad_earlySESVlad_3<- as.numeric(data$earlySESVlad_earlySESVlad_3)
data$early3item <- with(data, (rowMeans(cbind(earlySESVlad_earlySESVlad_1,earlySESVlad_earlySESVlad_2,earlySESVlad_earlySESVlad_3), na.rm=TRUE)))
with(data, psych::alpha(data.frame(earlySESVlad_earlySESVlad_1,earlySESVlad_earlySESVlad_2,earlySESVlad_earlySESVlad_3), na.rm=TRUE, check.keys=FALSE))
describe(data$early3item)

#### income

# current personal income
data$personalincome <- as.numeric(data$personalincome)
table(data$personalincome)
data$personal_income <- NA; 
data$personal_income[data$personalincome==0] <- 0
data$personal_income[data$personalincome==1] <- 2500
data$personal_income[data$personalincome==2] <- 7500
data$personal_income[data$personalincome==3] <- 12500
data$personal_income[data$personalincome==4] <- 17500
data$personal_income[data$personalincome==5] <- 22500
data$personal_income[data$personalincome==6] <- 27500
data$personal_income[data$personalincome==7] <- 32500
data$personal_income[data$personalincome==8] <- 37500
data$personal_income[data$personalincome==9] <- 42500
data$personal_income[data$personalincome==10] <- 47500
data$personal_income[data$personalincome==11] <- 55000
data$personal_income[data$personalincome==12] <- 65000
data$personal_income[data$personalincome==13] <- 75000
data$personal_income[data$personalincome==14] <- 85000
data$personal_income[data$personalincome==15] <- 95000
data$personal_income[data$personalincome==16] <- 112500
data$personal_income[data$personalincome==17] <- 137500
data$personal_income[data$personalincome==18] <- 162500
data$personal_income[data$personalincome==19] <- 187500
data$personal_income[data$personalincome==20] <- 225000
data$personal_income[data$personalincome==21] <- 275000
data$personal_income[data$personalincome==22] <- 325000
data$personal_income[data$personalincome==23] <- 375000
data$personal_income[data$personalincome==24] <- 425000
data$personal_income[data$personalincome==25] <- 475000
data$personal_income[data$personalincome==26] <- 550000
data$personal_income[data$personalincome==27] <- 650000
data$personal_income[data$personalincome==28] <- 750000
data$personal_income[data$personalincome==29] <- 850000
data$personal_income[data$personalincome==30] <- 950000
data$personal_income[data$personalincome==31] <- 1000000
describe(data$personal_income)

# current household income
data$householdincome <- as.numeric(data$householdincome)
table(data$householdincome)
data$livingsituation <- as.numeric(data$livingsituation)
table(data$livingsituation)
# step 1 = assign personal income to new household income variable (which will remain the value for those who live alone)
data$household_income <- data$personal_income
describe(data$household_income)
# step 2 = assign missing value to those who do not live alone but did not answer the household income question
# this is nobody in this data set
data$household_income[data$livingsituation==1 & data$householdincome<0] <- NA
describe(data$household_income)
# step 3 = assign selected household income values to those who were shown and answered that question
data$household_income[data$householdincome==0] <- 0
data$household_income[data$householdincome==1] <- 2500
data$household_income[data$householdincome==2] <- 7500
data$household_income[data$householdincome==3] <- 12500
data$household_income[data$householdincome==4] <- 17500
data$household_income[data$householdincome==5] <- 22500
data$household_income[data$householdincome==6] <- 27500
data$household_income[data$householdincome==7] <- 32500
data$household_income[data$householdincome==8] <- 37500
data$household_income[data$householdincome==9] <- 42500
data$household_income[data$householdincome==10] <- 47500
data$household_income[data$householdincome==11] <- 55000
data$household_income[data$householdincome==12] <- 65000
data$household_income[data$householdincome==13] <- 75000
data$household_income[data$householdincome==14] <- 85000
data$household_income[data$householdincome==15] <- 95000
data$household_income[data$householdincome==16] <- 112500
data$household_income[data$householdincome==17] <- 137500
data$household_income[data$householdincome==18] <- 162500
data$household_income[data$householdincome==19] <- 187500
data$household_income[data$householdincome==20] <- 225000
data$household_income[data$householdincome==21] <- 275000
data$household_income[data$householdincome==22] <- 325000
data$household_income[data$householdincome==23] <- 375000
data$household_income[data$householdincome==24] <- 425000
data$household_income[data$householdincome==25] <- 475000
data$household_income[data$householdincome==26] <- 550000
data$household_income[data$householdincome==27] <- 650000
data$household_income[data$householdincome==28] <- 750000
data$household_income[data$householdincome==29] <- 850000
data$household_income[data$householdincome==30] <- 950000
data$household_income[data$householdincome==31] <- 1000000
describe(data$household_income)

# note: some participants chose a higher personal income value than household income value, even though household income was defined as personal income plus income of others in the household
data$personallargerthanhousehold <- NA
data$personallargerthanhousehold[data$personalincome>data$householdincome] <- 1
data$personallargerthanhousehold[data$personalincome<=data$householdincome] <- 0
table(data$personallargerthanhousehold)

# code parental income
data$fatherincome <- as.numeric(data$fatherincome)
table(data$fatherincome)
data$fincome <- NA; 
data$fincome[data$fatherincome==0] <- 0
data$fincome[data$fatherincome==1] <- 2500
data$fincome[data$fatherincome==2] <- 7500
data$fincome[data$fatherincome==3] <- 12500
data$fincome[data$fatherincome==4] <- 17500
data$fincome[data$fatherincome==5] <- 22500
data$fincome[data$fatherincome==6] <- 27500
data$fincome[data$fatherincome==7] <- 32500
data$fincome[data$fatherincome==8] <- 37500
data$fincome[data$fatherincome==9] <- 42500
data$fincome[data$fatherincome==10] <- 47500
data$fincome[data$fatherincome==11] <- 55000
data$fincome[data$fatherincome==12] <- 65000
data$fincome[data$fatherincome==13] <- 75000
data$fincome[data$fatherincome==14] <- 85000
data$fincome[data$fatherincome==15] <- 95000
data$fincome[data$fatherincome==16] <- 112500
data$fincome[data$fatherincome==17] <- 137500
data$fincome[data$fatherincome==18] <- 162500
data$fincome[data$fatherincome==19] <- 187500
data$fincome[data$fatherincome==20] <- 225000
data$fincome[data$fatherincome==21] <- 275000
data$fincome[data$fatherincome==22] <- 325000
data$fincome[data$fatherincome==23] <- 375000
data$fincome[data$fatherincome==24] <- 425000
data$fincome[data$fatherincome==25] <- 475000
data$fincome[data$fatherincome==26] <- 550000
data$fincome[data$fatherincome==27] <- 650000
data$fincome[data$fatherincome==28] <- 750000
data$fincome[data$fatherincome==29] <- 850000
data$fincome[data$fatherincome==30] <- 950000
data$fincome[data$fatherincome==31] <- 1000000
describe(data$fincome)
data$motherincome <- as.numeric(data$motherincome)
table(data$motherincome)
data$mincome <- NA; 
data$mincome[data$motherincome==0] <- 0
data$mincome[data$motherincome==1] <- 2500
data$mincome[data$motherincome==2] <- 7500
data$mincome[data$motherincome==3] <- 12500
data$mincome[data$motherincome==4] <- 17500
data$mincome[data$motherincome==5] <- 22500
data$mincome[data$motherincome==6] <- 27500
data$mincome[data$motherincome==7] <- 32500
data$mincome[data$motherincome==8] <- 37500
data$mincome[data$motherincome==9] <- 42500
data$mincome[data$motherincome==10] <- 47500
data$mincome[data$motherincome==11] <- 55000
data$mincome[data$motherincome==12] <- 65000
data$mincome[data$motherincome==13] <- 75000
data$mincome[data$motherincome==14] <- 85000
data$mincome[data$motherincome==15] <- 95000
data$mincome[data$motherincome==16] <- 112500
data$mincome[data$motherincome==17] <- 137500
data$mincome[data$motherincome==18] <- 162500
data$mincome[data$motherincome==19] <- 187500
data$mincome[data$motherincome==20] <- 225000
data$mincome[data$motherincome==21] <- 275000
data$mincome[data$motherincome==22] <- 325000
data$mincome[data$motherincome==23] <- 375000
data$mincome[data$motherincome==24] <- 425000
data$mincome[data$motherincome==25] <- 475000
data$mincome[data$motherincome==26] <- 550000
data$mincome[data$motherincome==27] <- 650000
data$mincome[data$motherincome==28] <- 750000
data$mincome[data$motherincome==29] <- 850000
data$mincome[data$motherincome==30] <- 950000
data$mincome[data$motherincome==31] <- 1000000
describe(data$mincome)
with(data,cor.test(fincome,mincome))
data$parental_income <- with(data, (rowSums(cbind(fincome,mincome), na.rm=TRUE)))
describe(data$parental_income)

#### education
data$EduPartici <- as.numeric(data$EduPartici)
table(data$EduPartici)
table(data$EduPartici_8_TEXT) # specify what kind of professional degree if they pick that option
table(data$EduPartici_7_TEXT) # specify what kind of vocational degree if they pick that option
table(data$EduPartici_6_TEXT) # specify what kind of other degree if they pick that option
data$education <- data$EduPartici; table(data$education)
data$education[data$EduPartici==7] <- NA; table(data$education)
data$education[data$EduPartici==8] <- NA; table(data$education)
data$education[data$EduPartici_8_TEXT=="Doctorate of Veterinary Medicine"] <- 6; table(data$education)
data$education[data$EduPartici_8_TEXT=="J.D."] <- 6; table(data$education)
data$education[data$EduPartici_8_TEXT=="jd"] <- 6; table(data$education)
data$education[data$EduPartici_8_TEXT=="Jd"] <- 6; table(data$education)
data$education[data$EduPartici_8_TEXT=="JD"] <- 6; table(data$education)
data$education[data$EduPartici_8_TEXT=="law"] <- 6; table(data$education)
data$education[data$EduPartici_8_TEXT=="Law"] <- 6; table(data$education)
data$education[data$EduPartici_6_TEXT=="1.5 years of college credit. No degree, yet. "] <- 3; table(data$education)
data$education[data$EduPartici_6_TEXT=="4 years tertiary education - 1 year Liberal Arts college, 3 years college level art school, no degree."] <- 3; table(data$education)
data$education[data$EduPartici_6_TEXT=="about to receive bachelors degree"] <- 3; table(data$education)
data$education[data$EduPartici_6_TEXT=="College No credits"] <- 3; table(data$education)
data$education[data$EduPartici_6_TEXT=="some college"] <- 3; table(data$education)
data$education[data$EduPartici_6_TEXT=="Some college"] <- 3; table(data$education)
data$education[data$EduPartici_6_TEXT=="Some College"] <- 3; table(data$education)
data$education[data$EduPartici_6_TEXT=="SOME COLLEGE"] <- 3; table(data$education)
data$education[data$EduPartici_6_TEXT=="Some college credit no degree"] <- 3; table(data$education)
data$education[data$EduPartici_6_TEXT=="Some College education"] <- 3; table(data$education)
data$education[data$EduPartici_6_TEXT=="some college, no degree"] <- 3; table(data$education)
data$education[data$EduPartici_6_TEXT=="some college.  AGOOD DEAL OF PERSONAL EDUCATION ..."] <- 3; table(data$education)
data$education[data$EduPartici_6_TEXT=="still in college "] <- 3; table(data$education)
data$education[data$EduPartici_6_TEXT=="still in college for bachelor's"] <- 3; table(data$education)
data$education[data$EduPartici_6_TEXT=="Student: Undergrad"] <- 3; table(data$education)
data$education[data$EduPartici_6_TEXT=="two years college (no degree)"] <- 3; table(data$education)
table(data$education)
describe(data$education)

# code parental education
data$parentaleducation_Edu_Par_1 <- as.numeric(data$parentaleducation_Edu_Par_1); data$parentaleducation_Edu_Par_2 <- as.numeric(data$parentaleducation_Edu_Par_2)
table(data$parentaleducation_Edu_Par_1); table(data$parentaleducation_Edu_Par_2)
data$mother_education <- data$parentaleducation_Edu_Par_1
data$mother_education[data$parentaleducation_Edu_Par_1==8] <- NA
data$father_education <- data$parentaleducation_Edu_Par_2
data$father_education[data$parentaleducation_Edu_Par_2==8] <- NA
table(data$mother_education); table(data$father_education)
cor.test(data$mother_education,data$father_education)
data$parental_education <- with(data, (rowMeans(cbind(father_education,mother_education), na.rm=TRUE)))
describe(data$parental_education)

##### code entitlement ####
##### scale from Campbell, Bonacci, Shelton, Exline, & Bushman JPA 2004
data$entitlement_entitlement_1 <- as.numeric(data$entitlement_entitlement_1); data$entitlement_entitlement_2 <- as.numeric(data$entitlement_entitlement_2); data$entitlement_entitlement_3 <- as.numeric(data$entitlement_entitlement_3); data$entitlement_entitlement_4  <- as.numeric(data$entitlement_entitlement_4); data$entitlement_entitlement_5 <- as.numeric(data$entitlement_entitlement_5); data$entitlement_entitlement_6 <- as.numeric(data$entitlement_entitlement_6); data$entitlement_entitlement_7 <- as.numeric(data$entitlement_entitlement_7); data$entitlement_entitlement_8 <- as.numeric(data$entitlement_entitlement_8); data$entitlement_entitlement_9 <- as.numeric(data$entitlement_entitlement_9)
data$r.entitlement_entitlement_5 <- 8-data$entitlement_entitlement_5
data$entitlement <- with(data,(rowMeans(cbind(entitlement_entitlement_1,entitlement_entitlement_2,entitlement_entitlement_3,entitlement_entitlement_4,r.entitlement_entitlement_5,entitlement_entitlement_6,entitlement_entitlement_7,entitlement_entitlement_8,entitlement_entitlement_9), na.rm=TRUE)))
with(data, psych::alpha(data.frame(entitlement_entitlement_1,entitlement_entitlement_2,entitlement_entitlement_3,entitlement_entitlement_4,r.entitlement_entitlement_5,entitlement_entitlement_6,entitlement_entitlement_7,entitlement_entitlement_8,entitlement_entitlement_9), na.rm=T))

#### code demographic variables ####
# Gender
table(data$gender)
data$Female <- 0
data$Female[data$gender==' Female'] <- 1
data$Female[data$gender=='cis female'] <- 1
data$Female[data$gender=='f'] <- 1
data$Female[data$gender=='F'] <- 1
data$Female[data$gender=='famale '] <- 1
data$Female[data$gender=='Femail'] <- 1
data$Female[data$gender=='femal'] <- 1
data$Female[data$gender=='female'] <- 1
data$Female[data$gender=='Female'] <- 1
data$Female[data$gender=='FEMALE'] <- 1
data$Female[data$gender=='female '] <- 1
data$Female[data$gender=='Female '] <- 1
data$Female[data$gender=='Females'] <- 1
data$Female[data$gender=='fenake'] <- 1
data$Female[data$gender=='Fenale'] <- 1
data$Female[data$gender=='Lady'] <- 1
data$Female[data$gender=='Woman'] <- 1
table(data$Female)
data$Male <- 0
data$Male[data$gender=='m'] <- 1
data$Male[data$gender=='M'] <- 1
data$Male[data$gender=='Mal'] <- 1
data$Male[data$gender=='male'] <- 1
data$Male[data$gender=='Male'] <- 1
data$Male[data$gender=='MAle'] <- 1
data$Male[data$gender=='MALE'] <- 1
data$Male[data$gender=='male '] <- 1
data$Male[data$gender=='Male '] <- 1
data$Male[data$gender=='Male.'] <- 1
data$Male[data$gender=='Man'] <- 1
data$Male[data$gender=='Mlae'] <- 1
table(data$Male)
data$FemalevsMale <- NA
data$FemalevsMale[data$Female==1] <- 1
data$FemalevsMale[data$Male==1] <- 0
table(data$FemalevsMale)

# Ethnicity
table(data$RacePartic)
data$Caucasian <- 0
data$Caucasian[data$RacePartic=='Afro Caribbean American / White European American (bi-racial).'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='american white '] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='American White Person of Irish extraction'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='american, irish'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='American,Polish'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Aryan (Nordic)'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='asian american irish german'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='asian and white'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Asian Caucasian '] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Asian, White'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Biracial (black/white)'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Black and White'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='cacausian'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Cacausian'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='cauc'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='caucaisan'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='caucasean'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='caucasian'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Caucasian'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='CAUCASIAN'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Caucasian '] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Caucasian (White)'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Caucasian and Native American'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='caucasian, european-american'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Caucasian, Hispanic/Latino'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Caucasian/White'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='CAUCASIAN/WHITE'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Caucasin'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='caucasion'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Caucasion'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Euro-descended American'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='European'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='European american'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='European American'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='fwhite'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='German'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Greek'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Greek American'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Haitian/Irish/German'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Hispanic / Caucasian'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Hispanic, white'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Irish-American'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Irish. Scottish, Austrian'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Irish/White'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Italian'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Italian American'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Italian American '] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Italian American/Caucasian'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Lithuanian'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Mixed - Asian and White'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Mixed (Native & White)'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Mixed (White and Asian)'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Mixed (white/asian)'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='mixed european'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Mixed: Hispanic, Eastern European'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='multiracial, hispanic, middle eastern, white'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Native American, Caucasian'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Non-Hispanic White'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='russian and french'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='scot irish'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='scotch/irish/russian/polish'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='western european'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='white'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='White'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='WHITE'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='white '] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='White '] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='White - Caucasion'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='white - Native American'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='white (caucasian)'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='white af'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='white american'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='White American'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='White and asian'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='White and Latino'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='white and native american'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='white caucasian'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='White caucasian'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='White Caucasian'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='white european'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='White European descent'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='white hispanic'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='White Hispanic'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='White Latina '] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='White-Hispanic'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='white, Ashkenazi jewish'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='white, caucasian'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='white, hispanic'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='White, Hispanic'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='White, Italian, Greek, English'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='White, japanese'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='white, native american'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='White, Native American'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='White.'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='white/American/Irish descent'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='White/Asian'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='White/Black'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='white/caucasian'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='White/caucasian'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='White/Caucasian'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='White/Caucasian, European descent'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='White/Caucasion'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='White/Causcasian'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='white/Irish American'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='white/latino'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='White/Latino'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='White/Mexican'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Whitw'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='whte'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='Whte'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='whtie'] <- 1; table(data$Caucasian)
data$Caucasian[data$RacePartic=='WITHE'] <- 1; table(data$Caucasian)
data$African <- 0
data$African[data$RacePartic=='Afro Caribbean American / White European American (bi-racial).'] <- 1; table(data$African)
data$African[data$RacePartic=='African'] <- 1; table(data$African)
data$African[data$RacePartic=='african America'] <- 1; table(data$African)
data$African[data$RacePartic=='African american'] <- 1; table(data$African)
data$African[data$RacePartic=='African American'] <- 1; table(data$African)
data$African[data$RacePartic=='African American '] <- 1; table(data$African)
data$African[data$RacePartic=='African American and Puerto Rican'] <- 1; table(data$African)
data$African[data$RacePartic=='African-American'] <- 1; table(data$African)
data$African[data$RacePartic=='asian and african american'] <- 1; table(data$African)
data$African[data$RacePartic=='Asian, African American'] <- 1; table(data$African)
data$African[data$RacePartic=='Biracial (black/white)'] <- 1; table(data$African)
data$African[data$RacePartic=='black'] <- 1; table(data$African)
data$African[data$RacePartic=='Black'] <- 1; table(data$African)
data$African[data$RacePartic=='Black '] <- 1; table(data$African)
data$African[data$RacePartic=='Black /African-American'] <- 1; table(data$African)
data$African[data$RacePartic=='Black American'] <- 1; table(data$African)
data$African[data$RacePartic=='Black and White'] <- 1; table(data$African)
data$African[data$RacePartic=='black Hebrew'] <- 1; table(data$African)
data$African[data$RacePartic=='Haitian/Irish/German'] <- 1; table(data$African)
data$African[data$RacePartic=='negro'] <- 1; table(data$African)
data$African[data$RacePartic=='White/Black'] <- 1; table(data$African)
data$Asian <- 0
data$Asian[data$RacePartic=='asian'] <- 1; table(data$Asian)
data$Asian[data$RacePartic=='Asian'] <- 1; table(data$Asian)
data$Asian[data$RacePartic=='asian american'] <- 1; table(data$Asian)
data$Asian[data$RacePartic=='Asian American'] <- 1; table(data$Asian)
data$Asian[data$RacePartic=='asian american irish german'] <- 1; table(data$Asian)
data$Asian[data$RacePartic=='asian and african american'] <- 1; table(data$Asian)
data$Asian[data$RacePartic=='asian and white'] <- 1; table(data$Asian)
data$Asian[data$RacePartic=='Asian Caucasian '] <- 1; table(data$Asian)
data$Asian[data$RacePartic=='Asian-American'] <- 1; table(data$Asian)
data$Asian[data$RacePartic=='Asian, African American'] <- 1; table(data$Asian)
data$Asian[data$RacePartic=='Asian, White'] <- 1; table(data$Asian)
data$Asian[data$RacePartic=='chinese'] <- 1; table(data$Asian)
data$Asian[data$RacePartic=='Chinese'] <- 1; table(data$Asian)
data$Asian[data$RacePartic=='Chinese American'] <- 1; table(data$Asian)
data$Asian[data$RacePartic=='Chinese, American'] <- 1; table(data$Asian)
data$Asian[data$RacePartic=='Chinese; Malaysian'] <- 1; table(data$Asian)
data$Asian[data$RacePartic=='East Asian'] <- 1; table(data$Asian)
data$Asian[data$RacePartic=='Filipino'] <- 1; table(data$Asian)
data$Asian[data$RacePartic=='Filipino-American'] <- 1; table(data$Asian)
data$Asian[data$RacePartic=='japanese'] <- 1; table(data$Asian)
data$Asian[data$RacePartic=='Japanese'] <- 1; table(data$Asian)
data$Asian[data$RacePartic=='Japanese American'] <- 1; table(data$Asian)
data$Asian[data$RacePartic=='Mixed - Asian and White'] <- 1; table(data$Asian)
data$Asian[data$RacePartic=='Mixed (White and Asian)'] <- 1; table(data$Asian)
data$Asian[data$RacePartic=='Mixed (white/asian)'] <- 1; table(data$Asian)
data$Asian[data$RacePartic=='Mixed, Asian American'] <- 1; table(data$Asian)
data$Asian[data$RacePartic=='South Asian'] <- 1; table(data$Asian)
data$Asian[data$RacePartic=='Taiwanese'] <- 1; table(data$Asian)
data$Asian[data$RacePartic=='Taiwanese - Asian '] <- 1; table(data$Asian)
data$Asian[data$RacePartic=='Vietnamese American'] <- 1; table(data$Asian)
data$Asian[data$RacePartic=='White and asian'] <- 1; table(data$Asian)
data$Asian[data$RacePartic=='White, japanese'] <- 1; table(data$Asian)
data$Asian[data$RacePartic=='White/Asian'] <- 1; table(data$Asian)
data$Hispanic <- 0
data$Hispanic[data$RacePartic=='African American and Puerto Rican'] <- 1; table(data$Hispanic)
data$Hispanic[data$RacePartic=='Caucasian, Hispanic/Latino'] <- 1; table(data$Hispanic)
data$Hispanic[data$RacePartic=='Cuban'] <- 1; table(data$Hispanic)
data$Hispanic[data$RacePartic=='Egyptian, Argentinian '] <- 1; table(data$Hispanic)
data$Hispanic[data$RacePartic=='hispanic'] <- 1; table(data$Hispanic)
data$Hispanic[data$RacePartic=='Hispanic'] <- 1; table(data$Hispanic)
data$Hispanic[data$RacePartic=='hispanic '] <- 1; table(data$Hispanic)
data$Hispanic[data$RacePartic=='Hispanic '] <- 1; table(data$Hispanic)
data$Hispanic[data$RacePartic=='Hispanic / Caucasian'] <- 1; table(data$Hispanic)
data$Hispanic[data$RacePartic=='Hispanic, white'] <- 1; table(data$Hispanic)
data$Hispanic[data$RacePartic=='Hispanic/Latino'] <- 1; table(data$Hispanic)
data$Hispanic[data$RacePartic=='Latina'] <- 1; table(data$Hispanic)
data$Hispanic[data$RacePartic=='latino'] <- 1; table(data$Hispanic)
data$Hispanic[data$RacePartic=='Latino'] <- 1; table(data$Hispanic)
data$Hispanic[data$RacePartic=='Latino(a)'] <- 1; table(data$Hispanic)
data$Hispanic[data$RacePartic=='mexican'] <- 1; table(data$Hispanic)
data$Hispanic[data$RacePartic=='Mexican-american'] <- 1; table(data$Hispanic)
data$Hispanic[data$RacePartic=='Mixed: Hispanic, Eastern European'] <- 1; table(data$Hispanic)
data$Hispanic[data$RacePartic=='multiracial, hispanic, middle eastern, white'] <- 1; table(data$Hispanic)
data$Hispanic[data$RacePartic=='Portuguese'] <- 1; table(data$Hispanic)
data$Hispanic[data$RacePartic=='Puerto Rican'] <- 1; table(data$Hispanic)
data$Hispanic[data$RacePartic=='White and Latino'] <- 1; table(data$Hispanic)
data$Hispanic[data$RacePartic=='white hispanic'] <- 1; table(data$Hispanic)
data$Hispanic[data$RacePartic=='White Hispanic'] <- 1; table(data$Hispanic)
data$Hispanic[data$RacePartic=='White-Hispanic'] <- 1; table(data$Hispanic)
data$Hispanic[data$RacePartic=='white, hispanic'] <- 1; table(data$Hispanic)
data$Hispanic[data$RacePartic=='White, Hispanic'] <- 1; table(data$Hispanic)
data$Hispanic[data$RacePartic=='white/latino'] <- 1; table(data$Hispanic)
data$Hispanic[data$RacePartic=='White/Latino'] <- 1; table(data$Hispanic)
data$Hispanic[data$RacePartic=='White/Mexican'] <- 1; table(data$Hispanic)
data$other <- 0
data$other[data$RacePartic=='American'] <- 1; table(data$other)
data$other[data$RacePartic=='American '] <- 1; table(data$other)
data$other[data$RacePartic=='american indian'] <- 1; table(data$other)
data$other[data$RacePartic=='American Indian'] <- 1; table(data$other)
data$other[data$RacePartic=='athest '] <- 1; table(data$other)
data$other[data$RacePartic=='Bengali'] <- 1; table(data$other)
data$other[data$RacePartic=='Chistan'] <- 1; table(data$other)
data$other[data$RacePartic=='hindo '] <- 1; table(data$other)
data$other[data$RacePartic=='hindu '] <- 1; table(data$other)
data$other[data$RacePartic=='Indian'] <- 1; table(data$other)
data$other[data$RacePartic=='Indian, American'] <- 1; table(data$other)
data$other[data$RacePartic=='Indo-European '] <- 1; table(data$other)
data$other[data$RacePartic=='Jewish'] <- 1; table(data$other)
data$other[data$RacePartic=='Middle Eastern '] <- 1; table(data$other)
data$other[data$RacePartic=='Mixed'] <- 1; table(data$other)
data$other[data$RacePartic=='multi ethnic'] <- 1; table(data$other)
data$other[data$RacePartic=='Native American'] <- 1; table(data$other)
data$other[data$RacePartic=='Native American-Cherokee'] <- 1; table(data$other)
data$other[data$RacePartic=='Other'] <- 1; table(data$other)
data$other[data$RacePartic=='Pacific Islander'] <- 1; table(data$other)
data$other[data$RacePartic=='Pakistani and Arab'] <- 1; table(data$other)
data$other[data$RacePartic=='prodastant'] <- 1; table(data$other)
data$other[data$RacePartic=='Spiritual'] <- 1; table(data$other)
data$other[data$RacePartic=='Two or more races'] <- 1; table(data$other)
data$unreported <- 0
data$unreported[data$RacePartic=='I dont know'] <- 1; table(data$unreported)
data$unreported[data$RacePartic=='Nope'] <- 1; table(data$unreported)
data$unreported[data$RacePartic=='w'] <- 1; table(data$unreported)
data$unreported[data$RacePartic=='yes'] <- 1; table(data$unreported)
# need to add 1 to total--this next line does not work because of the apostrophy
# data$other[data$RacePartic=='I see more hispanics around here than anything else, but I don't make much effort to identify ethnicity or ethnicities around me. '] <- 1; table(data$other)
# also need to add the 62 who left it blank according to the table

data$age <- as.numeric(data$age) 
data$yearborn <- data$age+1910
data$ageyears <- 2018-(data$age+1910)
with(data,describe(cbind(yearborn,ageyears)))

#### descriptives stats ####

table(data$Female); table(data$Male); table(data$FemalevsMale)

describe(data$ageyears)

table(data$Caucasian); table(data$African); table(data$Asian); table(data$Hispanic); table(data$other); table(data$unreported)

# standardize and create current subjective SES composite
meancurrentSESladder <- mean(data$currentSESladder,na.rm=TRUE); sdcurrentSESladder <- sd(data$currentSESladder,na.rm=TRUE)
data$z.currentSESladder <- (data$currentSESladder-meancurrentSESladder)/sdcurrentSESladder
meancurrent3item <- mean(data$current3item,na.rm=TRUE); sdcurrent3item <- sd(data$current3item,na.rm=TRUE)
data$z.current3item <- (data$current3item-meancurrent3item)/sdcurrent3item
with(data, describe(cbind(currentSESladder,z.currentSESladder,current3item,z.current3item)))
with(data, corr.test(cbind(currentSESladder,z.currentSESladder,current3item,z.current3item)))
data$current.subjective.SES <- with(data, (rowMeans(cbind(z.currentSESladder,z.current3item), na.rm=TRUE)))
with(data, describe(cbind(currentSESladder,z.currentSESladder,current3item,z.current3item,current.subjective.SES)))

# standardize and create childhood subjective SES composite
meanearlySESladder <- mean(data$earlySESladder,na.rm=TRUE); sdearlySESladder <- sd(data$earlySESladder,na.rm=TRUE)
data$z.earlySESladder <- (data$earlySESladder-meanearlySESladder)/sdearlySESladder
meanearly3item <- mean(data$early3item,na.rm=TRUE); sdearly3item <- sd(data$early3item,na.rm=TRUE)
data$z.early3item <- (data$early3item-meanearly3item)/sdearly3item
with(data, describe(cbind(earlySESladder,z.earlySESladder,early3item,z.early3item)))
with(data, corr.test(cbind(earlySESladder,z.earlySESladder,early3item,z.early3item)))
data$early.subjective.SES <- with(data, (rowMeans(cbind(z.earlySESladder,z.early3item), na.rm=TRUE)))
with(data, describe(cbind(earlySESladder,z.earlySESladder,early3item,z.early3item,early.subjective.SES)))

with(data, psych::alpha(data.frame(currentSESVlad_currentSESVlad_1,currentSESVlad_currentSESVlad_2,currentSESVlad_currentSESVlad_3), na.rm=TRUE, check.keys=FALSE))
with(data, corr.test(cbind(currentSESladder,z.currentSESladder,current3item,z.current3item)))

cor.test(data$mother_education,data$father_education)

with(data, psych::alpha(data.frame(earlySESVlad_earlySESVlad_1,earlySESVlad_earlySESVlad_2,earlySESVlad_earlySESVlad_3), na.rm=TRUE, check.keys=FALSE))
with(data, corr.test(cbind(earlySESladder,z.earlySESladder,early3item,z.early3item)))

with(data, psych::alpha(data.frame(entitlement_entitlement_1,entitlement_entitlement_2,entitlement_entitlement_3,entitlement_entitlement_4,r.entitlement_entitlement_5,entitlement_entitlement_6,entitlement_entitlement_7,entitlement_entitlement_8,entitlement_entitlement_9), na.rm=T))

with(data, describe(cbind(personal_income,parental_income,education,parental_education,currentSESladder,current3item,earlySESladder,early3item,entitlement)))

with(data, corr.test(cbind(personal_income,parental_income,education,parental_education,currentSESladder,current.subjective.SES,earlySESladder,early.subjective.SES,entitlement)))
with(data,cor.test(parental_income,entitlement))
with(data,cor.test(parental_education,entitlement))
with(data,cor.test(currentSESladder,entitlement))
with(data,cor.test(current.subjective.SES,entitlement))
with(data,cor.test(earlySESladder,entitlement))
with(data,cor.test(early.subjective.SES,entitlement))

#### explore reliability of SES composites
data$z.personal_income <- scale(data$personal_income); data$z.education <- scale(data$education)
with(data, psych::alpha(data.frame(current.subjective.SES,z.personal_income,z.education), na.rm=TRUE, check.keys=FALSE))
data$z.parental_income <- scale(data$parental_income); data$z.parental_education <- scale(data$parental_education)
with(data, psych::alpha(data.frame(early.subjective.SES,z.parental_income,z.parental_education), na.rm=TRUE, check.keys=FALSE))

#### test interactions between early and current SES predicting entitlement ####

#### income
data$mean.parental_income <- mean(data$parental_income, na.rm=TRUE); data$mean.personal_income <- mean(data$personal_income, na.rm=TRUE)
data$sd.parental_income <- sd(data$parental_income, na.rm=TRUE); data$sd.personal_income <- sd(data$personal_income, na.rm=TRUE)
data$c.parental_income.div <- (data$parental_income-data$mean.parental_income)/10000; data$c.personal_income.div <- (data$personal_income-data$mean.personal_income)/10000
data$sd.parental_income.div <- sd(data$c.parental_income.div, na.rm=TRUE); data$sd.personal_income.div <- sd(data$c.personal_income.div, na.rm=TRUE)
with(data, describe(cbind(c.parental_income.div,parental_income,c.personal_income.div,personal_income)))
summary(entitlement.early.current.income <- lm(entitlement ~ c.personal_income.div*c.parental_income.div, data=data, na.action=na.omit))
#simple slopes
data$c.personal_income.div.high <- data$c.personal_income.div+data$sd.personal_income.div; summary(entitlement.early.highcurrent.income <- lm(entitlement ~ c.parental_income.div*c.personal_income.div.high, data=data, na.action=na.omit))
data$c.personal_income.div.low <- data$c.personal_income.div-data$sd.personal_income.div; summary(entitlement.early.lowcurrent.income <- lm(entitlement ~ c.parental_income.div*c.personal_income.div.low, data=data, na.action=na.omit))
data$c.parental_income.div.high <- data$c.parental_income.div+data$sd.parental_income.div; summary(entitlement.highearly.current.income <- lm(entitlement ~ c.parental_income.div.high*c.personal_income.div, data=data, na.action=na.omit))
data$c.parental_income.div.low <- data$c.parental_income.div-data$sd.parental_income.div; summary(entitlement.lowearly.current.income <- lm(entitlement ~ c.parental_income.div.low*c.personal_income.div, data=data, na.action=na.omit))
# Plot
incentitle <- data.frame(effect("c.personal_income.div*c.parental_income.div", entitlement.early.current.income, xlevels=list(c.parental_income.div=c(-2.123,106.19))))
ggplot(incentitle) + geom_line(aes(c.personal_income.div, fit,  color = c.parental_income.div, linetype=factor(c.parental_income.div)), size= 1.25, )+ scale_colour_gradient(low = "darkred", high = "darkblue") + theme_bw()+
  coord_cartesian(xlim=c(-10,110), ylim=c(1,7)) +
  scale_linetype_manual(values=c("dotted", "solid"))+
  theme(legend.position='none', panel.grid.major = element_blank(),
        text = element_text(size=20),
        panel.grid.minor = element_blank(),)+ xlab("Current Income")+ylab("Entitlement")
# with controls
summary(entitlement.early.current.income.controls <- lm(entitlement ~ FemalevsMale + Caucasian + ageyears + c.personal_income.div*c.parental_income.div, data=data, na.action=na.omit))

##### education
data$mean.parental_education <- mean(data$parental_education, na.rm=TRUE); data$mean.education <- mean(data$education, na.rm=TRUE)
data$sd.parental_education <- sd(data$parental_education, na.rm=TRUE); data$sd.education <- sd(data$education, na.rm=TRUE)
data$c.parental_education <- (data$parental_education-data$mean.parental_education); data$c.education <- (data$education-data$mean.education)
with(data, describe(cbind(c.parental_education,parental_education,c.education,education)))
summary(entitlement.early.current.education <- lm(entitlement ~ c.education*c.parental_education, data=data, na.action=na.omit))
#simple slopes
data$c.parental_education.high <- data$c.parental_education-data$sd.parental_education; summary(entitlement.highearly.current.education <- lm(entitlement ~ c.parental_education.high*c.education, data=data, na.action=na.omit))
data$c.parental_education.low <- data$c.parental_education+data$sd.parental_education; summary(entitlement.lowearly.current.education <- lm(entitlement ~ c.parental_education.low*c.education, data=data, na.action=na.omit))
data$c.education.high <- data$c.education-data$sd.education; summary(entitlement.early.highcurrent.education <- lm(entitlement ~ c.parental_education*c.education.high, data=data, na.action=na.omit))
data$c.education.low <- data$c.education+data$sd.education; summary(entitlement.early.lowcurrent.education <- lm(entitlement ~ c.parental_education*c.education.low, data=data, na.action=na.omit))
# Plot
educentitle <- data.frame(effect("c.education*c.parental_education", entitlement.early.current.education, xlevels=list(c.parental_education=c(-1.31,1.31))))
ggplot(educentitle) + geom_line(aes(c.education, fit,  color = c.parental_education, linetype=factor(c.parental_education)), size= 1.25, )+ scale_colour_gradient(low = "darkred", high = "darkblue") + theme_bw()+
  coord_cartesian(xlim=c(-3,4), ylim=c(1,7)) +
  scale_linetype_manual(values=c("dotted", "solid"))+
  theme(legend.position='none', panel.grid.major = element_blank(),
        text = element_text(size=20),
        panel.grid.minor = element_blank(),)+ xlab("Current Education")+ylab("Entitlement")
# with controls
summary(entitlement.early.current.education.controls <- lm(entitlement ~ FemalevsMale + Caucasian + ageyears + c.education*c.parental_education, data=data, na.action=na.omit))

##### subjective SES
data$mean.early.subjective.SES <- mean(data$early.subjective.SES, na.rm=TRUE); data$mean.current.subjective.SES <- mean(data$current.subjective.SES, na.rm=TRUE)
data$sd.early.subjective.SES <- sd(data$early.subjective.SES, na.rm=TRUE); data$sd.current.subjective.SES <- sd(data$current.subjective.SES, na.rm=TRUE)
data$c.early.subjective.SES <- (data$early.subjective.SES-data$mean.early.subjective.SES); data$c.current.subjective.SES <- (data$current.subjective.SES-data$mean.current.subjective.SES)
with(data, describe(cbind(c.early.subjective.SES,early.subjective.SES,c.current.subjective.SES,current.subjective.SES)))
summary(entitlement.early.current.subjective <- lm(entitlement ~ c.current.subjective.SES*c.early.subjective.SES, data=data, na.action=na.omit))
#simple slopes
data$c.early.subjective.SES.high <- data$c.early.subjective.SES-data$sd.early.subjective.SES; summary(entitlement.highearly.current.current.subjective.SES <- lm(entitlement ~ c.early.subjective.SES.high*c.current.subjective.SES, data=data, na.action=na.omit))
data$c.early.subjective.SES.low <- data$c.early.subjective.SES+data$sd.early.subjective.SES; summary(entitlement.lowearly.current.current.subjective.SES <- lm(entitlement ~ c.early.subjective.SES.low*c.current.subjective.SES, data=data, na.action=na.omit))
data$c.current.subjective.SES.high <- data$c.current.subjective.SES-data$sd.current.subjective.SES; summary(entitlement.early.highcurrent.current.subjective.SES <- lm(entitlement ~ c.early.subjective.SES*c.current.subjective.SES.high, data=data, na.action=na.omit))
data$c.current.subjective.SES.low <- data$c.current.subjective.SES+data$sd.current.subjective.SES; summary(entitlement.early.lowcurrent.current.subjective.SES <- lm(entitlement ~ c.early.subjective.SES*c.current.subjective.SES.low, data=data, na.action=na.omit))
# Plot
subjentitle <- data.frame(effect("c.current.subjective.SES*c.early.subjective.SES", entitlement.early.current.subjective, xlevels=list(c.early.subjective.SES=c(-.93,.93))))
ggplot(subjentitle) + geom_line(aes(c.current.subjective.SES, fit,  color = c.early.subjective.SES, linetype=factor(c.early.subjective.SES)), size= 1.25, )+ scale_colour_gradient(low = "darkred", high = "darkblue") + theme_bw()+
  coord_cartesian(xlim=c(-2.5,2.5), ylim=c(1,7)) +
  scale_linetype_manual(values=c("dotted", "solid"))+
  theme(legend.position='none', panel.grid.major = element_blank(),
        text = element_text(size=20),
        panel.grid.minor = element_blank(),)+ xlab("Current Subjective SES")+ylab("Entitlement") 
# with controls
summary(entitlement.early.current.subjective.controls <- lm(entitlement ~ FemalevsMale + Caucasian + ageyears + c.current.subjective.SES*c.early.subjective.SES, data=data, na.action=na.omit))

##### SES ladder
data$mean.earlySESladder <- mean(data$earlySESladder, na.rm=TRUE); data$mean.currentSESladder <- mean(data$currentSESladder, na.rm=TRUE)
data$sd.earlySESladder <- sd(data$earlySESladder, na.rm=TRUE); data$sd.currentSESladder <- sd(data$currentSESladder, na.rm=TRUE)
data$c.earlySESladder <- (data$earlySESladder-data$mean.earlySESladder); data$c.currentSESladder <- (data$currentSESladder-data$mean.currentSESladder)
with(data, describe(cbind(c.earlySESladder,earlySESladder,c.currentSESladder,currentSESladder)))
summary(entitlement.early.current.ladder <- lm(entitlement ~ c.currentSESladder*c.earlySESladder, data=data, na.action=na.omit))
#simple slopes
data$c.earlySESladder.high <- data$c.earlySESladder-data$sd.earlySESladder; summary(entitlement.highearly.current.currentSESladder <- lm(entitlement ~ c.earlySESladder.high*c.currentSESladder, data=data, na.action=na.omit))
data$c.earlySESladder.low <- data$c.earlySESladder+data$sd.earlySESladder; summary(entitlement.lowearly.current.currentSESladder <- lm(entitlement ~ c.earlySESladder.low*c.currentSESladder, data=data, na.action=na.omit))
data$c.currentSESladder.high <- data$c.currentSESladder-data$sd.currentSESladder; summary(entitlement.early.highcurrent.currentSESladder <- lm(entitlement ~ c.earlySESladder*c.currentSESladder.high, data=data, na.action=na.omit))
data$c.currentSESladder.low <- data$c.currentSESladder+data$sd.currentSESladder; summary(entitlement.early.lowcurrent.currentSESladder <- lm(entitlement ~ c.earlySESladder*c.currentSESladder.low, data=data, na.action=na.omit))
# Plot
subjentitle <- data.frame(effect("c.currentSESladder*c.earlySESladder", entitlement.early.current.ladder, xlevels=list(c.earlySESladder=c(-1.88,1.88))))
ggplot(subjentitle) + geom_line(aes(c.currentSESladder, fit,  color = c.earlySESladder, linetype=factor(c.earlySESladder)), size= 1.25, )+ scale_colour_gradient(low = "darkred", high = "darkblue") + theme_bw()+
  coord_cartesian(xlim=c(-5,5), ylim=c(1,7)) +
  scale_linetype_manual(values=c("dotted", "solid"))+
  theme(legend.position='none', panel.grid.major = element_blank(),
        text = element_text(size=20),
        panel.grid.minor = element_blank(),)+ xlab("Current Subjective SES")+ylab("Entitlement") 

################################################################################################################################################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################################################################################################################################################

# EXPLORATORY analysis with only participants who were born in the United States

# create data frame of participants born in the United States
table(data$stateborn_1)
data$stateborn_1[is.na(data$stateborn_1)] <- 0; table(data$stateborn_1)
data.USborn <- filter(data, stateborn_1!=1)

# re-run the main models
summary(entitlement.early.current.income <- lm(entitlement ~ c.personal_income.div*c.parental_income.div, data=data.USborn, na.action=na.omit))
summary(entitlement.early.current.education <- lm(entitlement ~ c.education*c.parental_education, data=data, na.action=na.omit))
summary(entitlement.early.current.subjective <- lm(entitlement ~ c.current.subjective.SES*c.early.subjective.SES, data=data, na.action=na.omit))

################################################################################################################################################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################################################################################################################################################

# EXPLORATORY analysis of personal income without outliers
describe(data$personal_income)
table(data$personal_income)
data$personalincomeoutlier <- NA
meanpersonalincome <- mean(data$personal_income, na.rm=TRUE); sdpersonalincome <- sd(data$personal_income, na.rm=TRUE); outlierpersonalincomecutoff <- meanpersonalincome+(3.29*sdpersonalincome)
meanpersonalincome; sdpersonalincome; outlierpersonalincomecutoff
data$personalincomeoutlier[data$personal_income>outlierpersonalincomecutoff] <- 1
data$personalincomeoutlier[data$personal_income<outlierpersonalincomecutoff] <- 0
table(data$personalincomeoutlier)
describe(data$parental_income)
table(data$parental_income)
data$parentalincomeoutlier <- NA
meanparentalincome <- mean(data$parental_income, na.rm=TRUE); sdparentalincome <- sd(data$parental_income, na.rm=TRUE); outlierparentalincomecutoff <- meanparentalincome+(3.29*sdparentalincome)
meanparentalincome; sdparentalincome; outlierparentalincomecutoff
data$parentalincomeoutlier[data$parental_income>outlierparentalincomecutoff] <- 1
data$parentalincomeoutlier[data$parental_income<outlierparentalincomecutoff] <- 0
table(data$parentalincomeoutlier)
data.noincomeinteractionoutliers <- subset(data, personalincomeoutlier==0 & parentalincomeoutlier==0)
summary(entitlement.early.current.income.nooutliers <- lm(entitlement ~ personal_income*parental_income, data=data.noincomeinteractionoutliers, na.action=na.omit))

# EXPLORATORY analysis of personal income without anomalous values that exceed household income
data$personalincomehigherthanhousehold <- 0
data$personalincomehigherthanhousehold[data$personalincome>data$householdincome] <- 1
table(data$personalincomehigherthanhousehold)
data.noanomalies <- subset(data, personalincomehigherthanhousehold==0)
summary(entitlement.early.current.income.noanomalies <- lm(entitlement ~ personal_income*parental_income, data=data.noanomalies, na.action=na.omit))

# EXPLORATORY analysis of personal income without outliers *and* without anomalous values that exceed household income
data.noincomeinteractionoutliers$personalincomehigherthanhousehold <- 0
data.noincomeinteractionoutliers$personalincomehigherthanhousehold[data.noincomeinteractionoutliers$personalincome>data.noincomeinteractionoutliers$householdincome] <- 1
table(data.noincomeinteractionoutliers$personalincomehigherthanhousehold)
data.noanomalies.noincomeinteractionoutliers <- subset(data.noincomeinteractionoutliers, personalincomehigherthanhousehold==0)
data.noanomalies.noincomeinteractionoutliers$mean.parental_income <- mean(data.noanomalies.noincomeinteractionoutliers$parental_income, na.rm=TRUE)
data.noanomalies.noincomeinteractionoutliers$mean.personal_income <- mean(data.noanomalies.noincomeinteractionoutliers$personal_income, na.rm=TRUE)
data.noanomalies.noincomeinteractionoutliers$c.parental_income.div <- (data.noanomalies.noincomeinteractionoutliers$parental_income-data.noanomalies.noincomeinteractionoutliers$mean.parental_income)/10000
data.noanomalies.noincomeinteractionoutliers$c.personal_income.div <- (data.noanomalies.noincomeinteractionoutliers$personal_income-data.noanomalies.noincomeinteractionoutliers$mean.personal_income)/10000
with(data.noanomalies.noincomeinteractionoutliers, describe(cbind(c.parental_income.div,parental_income,c.personal_income.div,personal_income)))
summary(entitlement.early.current.income.nooutliers.noanomalies <- lm(entitlement ~ c.personal_income.div*c.parental_income.div, data=data.noanomalies.noincomeinteractionoutliers, na.action=na.omit))

################################################################################################################################################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################################################################################################################################################