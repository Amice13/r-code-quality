
library(psych)
library(effects)
library(ggplot2)

setwd ("ENTER PATH")
mobility.final <- read.csv("study 2 data.csv")

### mobility analyses

#### create the composite for entitlement. from the data frame of 1,003 participants, 998 have a score for entitlement ####
mobility.final$rQ167_5 <- 8-mobility.final$Q167_5
mobility.final$entitlement <- with(mobility.final, (rowMeans(cbind(Q167_1,Q167_2,Q167_3,Q167_4,rQ167_5,Q167_6,Q167_7,Q167_8,Q167_9), na.rm=TRUE)))
describe(mobility.final$entitlement)
mobility.final$completed.entitlement <- NA
mobility.final$completed.entitlement[mobility.final$entitlement>0] <- 1
mobility.final$completed.entitlement[is.na(mobility.final$entitlement)] <- 0
table(mobility.final$completed.entitlement)

#### demographic characteristics ####

mobility.final$Female <- mobility.final$Gender
table(mobility.final$Female)
describe(mobility.final$Female)

mobility.final$participant.age <- mobility.final$Age+15
describe(mobility.final$participant.age)

table(mobility.final$Ethnicity_1)
table(mobility.final$Ethnicity_2)
table(mobility.final$Ethnicity_3)
table(mobility.final$Ethnicity_4)
table(mobility.final$Ethnicity_5)
table(mobility.final$Ethnicity_6)
table(mobility.final$Ethnicity_7)
mobility.final$Caucasian <- NA
mobility.final$Caucasian[mobility.final$Ethnicity_6==1] <- 1
mobility.final$Caucasian[is.na(mobility.final$Caucasian)] <- 0
table(mobility.final$Caucasian)
mobility.final$AmericanIndian <- NA
mobility.final$AmericanIndian[mobility.final$Ethnicity_1==1] <- 1
mobility.final$AmericanIndian[is.na(mobility.final$AmericanIndian)] <- 0
table(mobility.final$AmericanIndian)
mobility.final$Asian <- NA
mobility.final$Asian[mobility.final$Ethnicity_2==1] <- 1
mobility.final$Asian[is.na(mobility.final$Asian)] <- 0
table(mobility.final$Asian)
mobility.final$AfricanAmerican <- NA
mobility.final$AfricanAmerican[mobility.final$Ethnicity_3==1] <- 1
mobility.final$AfricanAmerican[is.na(mobility.final$AfricanAmerican)] <- 0
table(mobility.final$AfricanAmerican)
mobility.final$Latin <- NA
mobility.final$Latin[mobility.final$Ethnicity_4==1] <- 1
mobility.final$Latin[is.na(mobility.final$Latin)] <- 0
table(mobility.final$Latin)
mobility.final$NativeHawaiian <- 0
mobility.final$NativeHawaiian[mobility.final$Ethnicity_5==1] <- 1
mobility.final$NativeHawaiian[is.na(mobility.final$NativeHawaiian)] <- 0
table(mobility.final$NativeHawaiian)
mobility.final$Other <- NA
mobility.final$Other[mobility.final$Ethnicity_7==1] <- 1
mobility.final$Other[is.na(mobility.final$Other)] <- 0
table(mobility.final$Other)
mobility.final$number.ethnicities.reported <- with(mobility.final, (rowSums(cbind(Caucasian,AmericanIndian,Asian,AfricanAmerican,Latin,NativeHawaiian,Other), na.rm=TRUE)))
table(mobility.final$number.ethnicities.reported)

#### social class predictor variables ####

### subjective social class (only in this study of mobility, only one subjective scale was administered)

table(mobility.final$SC_Current)
table(mobility.final$SC_Past)
mobility.final$z.subjective.class.destinations <- (mobility.final$SC_Current-mean(mobility.final$SC_Current,na.rm=TRUE))/sd(mobility.final$SC_Current,na.rm=TRUE)
mobility.final$z.subjective.class.origins <- (mobility.final$SC_Past-mean(mobility.final$SC_Past,na.rm=TRUE))/sd(mobility.final$SC_Past,na.rm=TRUE)
with(mobility.final, describe(cbind(SC_Current,SC_Past,z.subjective.class.destinations,z.subjective.class.origins)))
with(mobility.final, cor.test(z.subjective.class.destinations,z.subjective.class.origins))

### income

table(mobility.final$Income_P)
mobility.final$income <- NA
mobility.final$income[mobility.final$Income_P==1] <- 5000
mobility.final$income[mobility.final$Income_P==2] <- 12500
mobility.final$income[mobility.final$Income_P==3] <- 17500
mobility.final$income[mobility.final$Income_P==4] <- 22500
mobility.final$income[mobility.final$Income_P==5] <- 27500
mobility.final$income[mobility.final$Income_P==6] <- 35000
mobility.final$income[mobility.final$Income_P==7] <- 45000
mobility.final$income[mobility.final$Income_P==8] <- 55000
mobility.final$income[mobility.final$Income_P==9] <- 65000
mobility.final$income[mobility.final$Income_P==10] <- 77500
mobility.final$income[mobility.final$Income_P==11] <- 92500
mobility.final$income[mobility.final$Income_P==12] <- 125000
mobility.final$income[mobility.final$Income_P==13] <- 175000
mobility.final$income[mobility.final$Income_P==14] <- 225000
mobility.final$income[mobility.final$Income_P==15] <- 275000
mobility.final$income.div <- mobility.final$income/10000
with(mobility.final, describe(cbind(income, income.div)))
# Hout adjustment -- value for top category calculated in a separate spreadsheet
mobility.final$income.Hout <- mobility.final$income
mobility.final$income.Hout[mobility.final$Income_P==15] <- 426619.8097
mobility.final$income.Hout.div <- mobility.final$income.Hout/10000
mobility.final$z.income.Hout.div <- (mobility.final$income.Hout.div-mean(mobility.final$income.Hout.div,na.rm=TRUE))/sd(mobility.final$income.Hout.div,na.rm=TRUE)
with(mobility.final, describe(cbind(income,income.div,income.Hout,income.Hout.div,z.income.Hout.div)))

table(mobility.final$Income_Par)
mobility.final$parental.income <- NA
mobility.final$parental.income[mobility.final$Income_Par==1] <- 5000
mobility.final$parental.income[mobility.final$Income_Par==2] <- 12500
mobility.final$parental.income[mobility.final$Income_Par==3] <- 17500
mobility.final$parental.income[mobility.final$Income_Par==4] <- 22500
mobility.final$parental.income[mobility.final$Income_Par==5] <- 27500
mobility.final$parental.income[mobility.final$Income_Par==6] <- 35000
mobility.final$parental.income[mobility.final$Income_Par==7] <- 45000
mobility.final$parental.income[mobility.final$Income_Par==8] <- 55000
mobility.final$parental.income[mobility.final$Income_Par==9] <- 65000
mobility.final$parental.income[mobility.final$Income_Par==10] <- 77500
mobility.final$parental.income[mobility.final$Income_Par==11] <- 92500
mobility.final$parental.income[mobility.final$Income_Par==12] <- 125000
mobility.final$parental.income[mobility.final$Income_Par==13] <- 175000
mobility.final$parental.income[mobility.final$Income_Par==14] <- 225000
mobility.final$parental.income[mobility.final$Income_Par==15] <- 275000
mobility.final$parental.income.div <- mobility.final$parental.income/10000
with(mobility.final, describe(cbind(parental.income, parental.income.div)))
# Hout adjustment -- value for top category calculated in a separate spreadsheet
mobility.final$parental.income.Hout <- mobility.final$parental.income
mobility.final$parental.income.Hout[mobility.final$Income_P==15] <- 336913.6871
mobility.final$parental.income.Hout.div <- mobility.final$parental.income.div
mobility.final$z.parental.income.Hout.div <- (mobility.final$parental.income.Hout.div-mean(mobility.final$parental.income.Hout.div,na.rm=TRUE))/sd(mobility.final$parental.income.Hout.div,na.rm=TRUE)
with(mobility.final, describe(cbind(parental.income,parental.income.div,parental.income.Hout,parental.income.Hout.div,z.parental.income.Hout.div)))

with(mobility.final, corr.test(cbind(income.Hout.div,parental.income.Hout.div,z.income.Hout.div,z.parental.income.Hout.div)))

# coding education
# note: variable Edu was recoded manually using the statements in the "Other" category into "Edu_recoded"
table(mobility.final$Edu_recoded)
mobility.final$education <- NA
mobility.final$education[mobility.final$Edu_recoded==1] <- 1
mobility.final$education[mobility.final$Edu_recoded==2] <- 2
mobility.final$education[mobility.final$Edu_recoded==3] <- 3
mobility.final$education[mobility.final$Edu_recoded==4] <- 4
mobility.final$education[mobility.final$Edu_recoded==5] <- 5
mobility.final$education[mobility.final$Edu_recoded==6] <- 6
mobility.final$education[mobility.final$Edu_recoded==7] <- 6
table(mobility.final$education)
describe(mobility.final$education)
mobility.final$z.education <- (mobility.final$education-mean(mobility.final$education,na.rm=TRUE))/sd(mobility.final$education,na.rm=TRUE)
describe(mobility.final$Edu_Par_1)
mobility.final$mom.Education_recoded <- NA
mobility.final$mom.Education_recoded[mobility.final$Edu_Par_1==1] <- 1
mobility.final$mom.Education_recoded[mobility.final$Edu_Par_1==2] <- 2
mobility.final$mom.Education_recoded[mobility.final$Edu_Par_1==3] <- 3
mobility.final$mom.Education_recoded[mobility.final$Edu_Par_1==4] <- 4
mobility.final$mom.Education_recoded[mobility.final$Edu_Par_1==5] <- 5
mobility.final$mom.Education_recoded[mobility.final$Edu_Par_1==6] <- 6
mobility.final$mom.Education_recoded[mobility.final$Edu_Par_1==7] <- 6
describe(mobility.final$Edu_Par_2)
mobility.final$dad.Education_recoded <- NA
mobility.final$dad.Education_recoded[mobility.final$Edu_Par_2==1] <- 1
mobility.final$dad.Education_recoded[mobility.final$Edu_Par_2==2] <- 2
mobility.final$dad.Education_recoded[mobility.final$Edu_Par_2==3] <- 3
mobility.final$dad.Education_recoded[mobility.final$Edu_Par_2==4] <- 4
mobility.final$dad.Education_recoded[mobility.final$Edu_Par_2==5] <- 5
mobility.final$dad.Education_recoded[mobility.final$Edu_Par_2==6] <- 6
mobility.final$dad.Education_recoded[mobility.final$Edu_Par_2==7] <- 6
mobility.final$parents.education <- with(mobility.final, (rowMeans(cbind(mom.Education_recoded,dad.Education_recoded), na.rm=TRUE)))
with(mobility.final, cor.test(mom.Education_recoded,dad.Education_recoded))
table(mobility.final$parents.education)
describe(mobility.final$parents.education)
mobility.final$z.parents.education <- (mobility.final$parents.education-mean(mobility.final$parents.education,na.rm=TRUE))/sd(mobility.final$parents.education,na.rm=TRUE)

with(mobility.final, corr.test(cbind(education,parents.education,z.education,z.parents.education)))

# reliability and descriptives for entitlement
describe(mobility.final$entitlement)
with(mobility.final, psych::alpha(data.frame(Q167_1,Q167_2,Q167_3,Q167_4,rQ167_5,Q167_6,Q167_7,Q167_8,Q167_9), na.rm=T))
mobility.final$z.entitlement <- (mobility.final$entitlement-mean(mobility.final$entitlement,na.rm=TRUE))/sd(mobility.final$entitlement,na.rm=TRUE)
mobility.final$entitlement.notitanic <- with(mobility.final, (rowMeans(cbind(Q167_1,Q167_2,Q167_4,rQ167_5,Q167_6,Q167_7,Q167_8,Q167_9), na.rm=TRUE)))
mobility.final$z.entitlement.notitanic <- (mobility.final$entitlement.notitanic-mean(mobility.final$entitlement.notitanic,na.rm=TRUE))/sd(mobility.final$entitlement.notitanic,na.rm=TRUE)
with(mobility.final, describe(cbind(entitlement,z.entitlement,entitlement.notitanic,z.entitlement.notitanic)))

with(mobility.final, corr.test (cbind(z.subjective.class.destinations,z.subjective.class.origins,z.income.Hout.div,z.parental.income.Hout.div,z.education,z.parents.education,z.entitlement)))

#### ANALYSES WITH ENTITLEMENT ####

#### SUBJECTIVE SOCIAL CLASS AND ENTITLEMENT ####
# two way interaction between composite subjective class origins, composite subjective class destinations, and entitlement
with(mobility.final, describe(cbind(SC_Current,SC_Past,z.subjective.class.destinations,z.subjective.class.origins)))
with(mobility.final, corr.test(cbind(z.subjective.class.destinations,z.subjective.class.origins,entitlement), use="complete", adjust="none"))
with(mobility.final, cor.test(z.subjective.class.destinations,z.entitlement))
with(mobility.final, cor.test(z.subjective.class.origins,z.entitlement))
with(mobility.final, corr.test(cbind(SC_Current,SC_Past,entitlement), use="complete", adjust="none"))
composite.subjective.ols.entitlement.linear <- lm(z.entitlement ~ z.subjective.class.destinations+z.subjective.class.origins, data=mobility.final, na.action=na.omit); summary(composite.subjective.ols.entitlement.linear)
composite.subjective.ols.entitlement <- lm(z.entitlement ~ z.subjective.class.destinations*z.subjective.class.origins, data=mobility.final, na.action=na.omit); summary(composite.subjective.ols.entitlement)
# simple slopes ---- Association between class destinations and entitlement among people with low and high class origins
mobility.final$hi.subjective.origins <- mobility.final$z.subjective.class.origins - 1
mobility.final$lo.subjective.origins <- mobility.final$z.subjective.class.origins + 1
subjective.entitlement.hi.origins <- lm(z.entitlement ~ z.subjective.class.destinations*hi.subjective.origins, data=mobility.final, na.action=na.omit); summary(subjective.entitlement.hi.origins)
subjective.entitlement.lo.origins <- lm(z.entitlement ~ z.subjective.class.destinations*lo.subjective.origins, data=mobility.final, na.action=na.omit); summary(subjective.entitlement.lo.origins)
# simple slopes ---- Association between class origins and entitlement among people with low and high class destinatons
mobility.final$hi.subjective.destinations <- mobility.final$z.subjective.class.destinations - 1
mobility.final$lo.subjective.destinations <- mobility.final$z.subjective.class.destinations + 1
subjective.entitlement.hi.destinations <- lm(z.entitlement ~ hi.subjective.destinations*z.subjective.class.origins, data=mobility.final, na.action=na.omit); summary(subjective.entitlement.hi.destinations)
subjective.entitlement.lo.destinations <- lm(z.entitlement ~ lo.subjective.destinations*z.subjective.class.origins, data=mobility.final, na.action=na.omit); summary(subjective.entitlement.lo.destinations)
# Plot
subjentitle<- data.frame(effect("z.subjective.class.destinations*z.subjective.class.origins", composite.subjective.ols.entitlement, xlevels=list(z.subjective.class.origins=c(-1,1))))
ggplot(subjentitle) + geom_line(aes(z.subjective.class.destinations, fit,  color = z.subjective.class.origins, linetype=factor(z.subjective.class.origins)), size= 1.25, )+ scale_colour_gradient(low = "darkred", high = "darkblue") + theme_bw()+
  coord_cartesian(xlim=c(-2.5,2.5), ylim=c(-2,2)) +
  scale_linetype_manual(values=c("dotted", "solid"))+
  theme(legend.position='none', panel.grid.major = element_blank(),
        text = element_text(size=15),
        panel.grid.minor = element_blank(),)+ xlab("Current Subjective SES")+ylab("Entitlement") 
# two way interaction with controls
composite.subjective.ols.entitlement.controls <- lm(z.entitlement ~ z.subjective.class.destinations*z.subjective.class.origins + participant.age + Female + Caucasian, data=mobility.final, na.action=na.omit); summary(composite.subjective.ols.entitlement.controls)
# simple slopes ---- Association between class destinations and entitlement among people with low and high class origins
summary(subjective.entitlement.hi.origins.controls <- lm(z.entitlement ~ z.subjective.class.destinations*hi.subjective.origins + participant.age + Female + Caucasian, data=mobility.final, na.action=na.omit))
summary(subjective.entitlement.lo.origins.controls <- lm(z.entitlement ~ z.subjective.class.destinations*lo.subjective.origins + participant.age + Female + Caucasian, data=mobility.final, na.action=na.omit))
# simple slopes ---- Association between class origins and entitlement among people with low and high class destinatons
summary(subjective.entitlement.hi.destinations.controls <- lm(z.entitlement ~ hi.subjective.destinations*z.subjective.class.origins + participant.age + Female + Caucasian, data=mobility.final, na.action=na.omit))
summary(subjective.entitlement.lo.destinations.controls <- lm(z.entitlement ~ lo.subjective.destinations*z.subjective.class.origins + participant.age + Female + Caucasian, data=mobility.final, na.action=na.omit))
# exploratory two-way interaction with controls
composite.subjective.ols.entitlement.notitanic <- lm(z.entitlement.notitanic ~ z.subjective.class.destinations*z.subjective.class.origins, data=mobility.final, na.action=na.omit); summary(composite.subjective.ols.entitlement.notitanic)
# exploratory three way interaction with gender
summary(composite.subjective.ols.entitlement.gender <- lm(z.entitlement ~ z.subjective.class.destinations*z.subjective.class.origins*Female, data=mobility.final, na.action=na.omit))
# exploratory three way interaction with ethnicity
summary(composite.subjective.ols.entitlement.ethnicity <- lm(z.entitlement ~ z.subjective.class.destinations*z.subjective.class.origins*Caucasian, data=mobility.final, na.action=na.omit))

#### INCOME WITH HOUT CORRECTION FOR HIGHEST INCOME BRACKET AND ENTITLEMENT ####
with(mobility.final, describe(cbind(income.div,income.Hout.div,z.income.Hout.div)))
table(mobility.final$income.Hout.div)
table(mobility.final$z.income.Hout.div)
with(mobility.final, describe(cbind(parental.income.Hout.div,z.parental.income.Hout.div)))
table(mobility.final$parental.income.Hout.div)
table(mobility.final$z.parental.income.Hout.div)
with(mobility.final, corr.test(cbind(income.Hout.div,parental.income.Hout.div,entitlement), use="complete", adjust="none"))
with(mobility.final, cor.test(z.income.Hout.div,z.entitlement))
with(mobility.final, cor.test(z.parental.income.Hout.div,z.entitlement))
income.Hout.entitlement.linear <- lm(z.entitlement ~ z.income.Hout.div+z.parental.income.Hout.div, data=mobility.final, na.action=na.omit); summary(income.Hout.entitlement.linear)
income.Hout.entitlement <- lm(z.entitlement ~ z.income.Hout.div*z.parental.income.Hout.div, data=mobility.final, na.action=na.omit); summary(income.Hout.entitlement)
# simple slopes
#---- Association between class destinations and entitlement among people with low and high class origins
with(mobility.final, describe(cbind(income.Hout.div,z.income.Hout.div,parental.income.Hout.div,z.parental.income.Hout.div)))
#####################################################################################################################
############### this is a problem at the low end of parental income because -1 SD is lower than 0 ###################
#####################################################################################################################
mobility.final$hi.parental.income.Hout.div <- mobility.final$z.parental.income.Hout.div - 1
mobility.final$lo.parental.income.Hout.div <- mobility.final$z.parental.income.Hout.div + .5
income.Hout.entitlement.hi.origins <- lm(z.entitlement ~ z.income.Hout.div*hi.parental.income.Hout.div, data=mobility.final, na.action=na.omit); summary(income.Hout.entitlement.hi.origins)
income.Hout.entitlement.lo.origins <- lm(z.entitlement ~ z.income.Hout.div*lo.parental.income.Hout.div, data=mobility.final, na.action=na.omit); summary(income.Hout.entitlement.lo.origins)
#---- Association between class origins and entitlement among people with low and high class destinations
with(mobility.final, describe(cbind(income.Hout.div,z.income.Hout.div,parental.income.Hout.div,z.parental.income.Hout.div)))
############################################################################################################################################
mobility.final$hi.income.Hout.div <- mobility.final$z.income.Hout.div - 1
mobility.final$lo.income.Hout.div <- mobility.final$z.income.Hout.div + .5
income.Hout.entitlement.hi.destinations <- lm(z.entitlement ~ hi.income.Hout.div*z.parental.income.Hout.div, data=mobility.final, na.action=na.omit); summary(income.Hout.entitlement.hi.destinations)
income.Hout.entitlement.lo.destinations <- lm(z.entitlement ~ lo.income.Hout.div*z.parental.income.Hout.div, data=mobility.final, na.action=na.omit); summary(income.Hout.entitlement.lo.destinations)
# Plot
incentitle<- data.frame(effect("z.income.Hout.div*z.parental.income.Hout.div", income.Hout.entitlement, xlevels=list(z.parental.income.Hout.div=c(-.5,1))))
ggplot(incentitle) + geom_line(aes(z.income.Hout.div, fit,  color = z.parental.income.Hout.div, linetype=factor(z.parental.income.Hout.div)), size= 2, )+ scale_colour_gradient(low = "darkred", high = "darkblue") + theme_bw()+
  coord_cartesian(xlim=c(-1,7), ylim=c(-2,2)) +
  scale_linetype_manual(values=c("dotted", "solid"))+
  theme(legend.position='none', panel.grid.major = element_blank(),
        text = element_text(size=20),
        panel.grid.minor = element_blank(),)+ xlab("Current Income")+ylab("Entitlement") 
# two way interaction with controls
income.Hout.entitlement.controls <- lm(z.entitlement ~ z.income.Hout.div*z.parental.income.Hout.div + participant.age + Female + Caucasian, data=mobility.final, na.action=na.omit); summary(income.Hout.entitlement.controls)
# simple slopes
summary(income.Hout.entitlement.hi.origins.controls <- lm(z.entitlement ~ z.income.Hout.div*hi.parental.income.Hout.div + participant.age + Female + Caucasian, data=mobility.final, na.action=na.omit))
summary(income.Hout.entitlement.lo.origins.controls <- lm(z.entitlement ~ z.income.Hout.div*lo.parental.income.Hout.div + participant.age + Female + Caucasian, data=mobility.final, na.action=na.omit))
summary(income.Hout.entitlement.hi.destinations.controls <- lm(z.entitlement ~ hi.income.Hout.div*z.parental.income.Hout.div + participant.age + Female + Caucasian, data=mobility.final, na.action=na.omit))
summary(income.Hout.entitlement.lo.destinations.controls <- lm(z.entitlement ~ lo.income.Hout.div*z.parental.income.Hout.div + participant.age + Female + Caucasian, data=mobility.final, na.action=na.omit))
# exploratory two-way interaction no titanic
income.Hout.entitlement.notitanic <- lm(z.entitlement.notitanic ~ z.income.Hout.div*z.parental.income.Hout.div, data=mobility.final, na.action=na.omit); summary(income.Hout.entitlement.notitanic)
# exploratory three way interaction with gender
summary(income.Hout.entitlement.gender <- lm(z.entitlement ~ z.income.Hout.div*z.parental.income.Hout.div*Female, data=mobility.final, na.action=na.omit))
# exploratory three way interaction with ethnicity
summary(income.Hout.entitlement.ethnicity <- lm(z.entitlement ~ z.income.Hout.div*z.parental.income.Hout.div*Caucasian, data=mobility.final, na.action=na.omit))

#### EDUCATION AND ENTITLEMENT ####
# two way interaction between class origins, class destinations, and entitlement
with(mobility.final, describe(cbind(education,z.education)))
table(mobility.final$education)
table(mobility.final$z.education)
with(mobility.final, describe(cbind(parents.education,z.parents.education)))
table(mobility.final$parents.education)
table(mobility.final$z.parents.education)
with(mobility.final, cor.test(z.education,z.entitlement))
with(mobility.final, cor.test(z.parents.education,z.entitlement))
with(mobility.final, corr.test(cbind(education,parents.education,entitlement), use="complete", adjust="none"))
education.ols.entitlement.linear <- lm(z.entitlement ~ z.education+z.parents.education, data=mobility.final, na.action=na.omit); summary(education.ols.entitlement.linear)
education.ols.entitlement <- lm(z.entitlement ~ z.education*z.parents.education, data=mobility.final, na.action=na.omit); summary(education.ols.entitlement)
#---- Association between class destinations and entitlement among people with low and high class origins
# simple slopes
mobility.final$hi.parents.education <- mobility.final$z.parents.education - 1
mobility.final$lo.parents.education <- mobility.final$z.parents.education + 1
education.entitlement.hi.origins <- lm(z.entitlement ~ z.education*hi.parents.education, data=mobility.final, na.action=na.omit); summary(education.entitlement.hi.origins)
education.entitlement.lo.origins <- lm(z.entitlement ~ z.education*lo.parents.education, data=mobility.final, na.action=na.omit); summary(education.entitlement.lo.origins)
#---- Association between class origins and entitlement among people with low and high class destinations
# simple slopes
mobility.final$hi.education <- mobility.final$z.education - 1
mobility.final$lo.education <- mobility.final$z.education + 1
education.entitlement.hi.destinations <- lm(z.entitlement ~ hi.education*z.parents.education, data=mobility.final, na.action=na.omit); summary(education.entitlement.hi.destinations)
education.entitlement.lo.destinations <- lm(z.entitlement ~ lo.education*z.parents.education, data=mobility.final, na.action=na.omit); summary(education.entitlement.lo.destinations)
# Plot
eduentitle<- data.frame(effect("z.education*z.parents.education", education.ols.entitlement, xlevels=list(z.parents.education=c(-1,1))))
ggplot(eduentitle) + geom_line(aes(z.education, fit,  color = z.parents.education, linetype=factor(z.parents.education)), size= 1.25, )+ scale_colour_gradient(low = "darkred", high = "darkblue") + theme_bw()+
  coord_cartesian(xlim=c(-2.5,2.5), ylim=c(-2,2)) +
  scale_linetype_manual(values=c("dotted", "solid"))+
  theme(legend.position='none', panel.grid.major = element_blank(),
        text = element_text(size=15),
        panel.grid.minor = element_blank(),)+ xlab("Current Education")+ylab("Entitlement") 
# exploratory two-way interaction with controls
education.ols.entitlement.controls <- lm(z.entitlement ~ z.education*z.parents.education + participant.age + Female + Caucasian, data=mobility.final, na.action=na.omit); summary(education.ols.entitlement.controls)
# simple slopes
summary(education.entitlement.hi.origins.controls <- lm(z.entitlement ~ z.education*hi.parents.education + participant.age + Female + Caucasian, data=mobility.final, na.action=na.omit))
summary(education.entitlement.lo.origins.controls <- lm(z.entitlement ~ z.education*lo.parents.education + participant.age + Female + Caucasian, data=mobility.final, na.action=na.omit))
# simple slopes
summary(education.entitlement.hi.destinations.controls <- lm(z.entitlement ~ hi.education*z.parents.education + participant.age + Female + Caucasian, data=mobility.final, na.action=na.omit))
summary(education.entitlement.lo.destinations.controls <- lm(z.entitlement ~ lo.education*z.parents.education + participant.age + Female + Caucasian, data=mobility.final, na.action=na.omit))
