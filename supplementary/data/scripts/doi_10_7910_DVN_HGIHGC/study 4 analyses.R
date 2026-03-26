library(psych)
library(nlme)
library(lme4)
library(effects)
library(lavaan)
library(ggplot2)

setwd ("ENTER PATH")
mobility.final <- read.csv("study 4 data.csv")

#### create score for entitlement ####
mobility.final$ent1 <-mobility.final$entitlement_4-7
mobility.final$ent2 <-mobility.final$entitlement_5-7
mobility.final$ent3 <-mobility.final$entitlement_6-7
mobility.final$ent4 <-mobility.final$entitlement_7-7
mobility.final$r.ent5 <- 8-(mobility.final$entitlement_8-7)
mobility.final$ent6 <-mobility.final$entitlement_9-7
mobility.final$ent7 <-mobility.final$entitlement_10-7
mobility.final$ent8 <-mobility.final$entitlement_11-7
mobility.final$ent9 <-mobility.final$entitlement_12-7
mobility.final$entitlement <- with(mobility.final, (rowMeans(cbind(ent1,ent2,ent3,ent4,r.ent5,ent6,ent7,ent8,ent9), na.rm=TRUE)))

#### coding demographics ####

mobility.final$participant.age <- mobility.final$Age+15
describe(mobility.final$participant.age)

### INCOME
# personal income
table(mobility.final$Income_P)
# note that income was coded weird in qualtrics, so that category 22 was actually the $5,000-$9,999 category
mobility.final$income <- NA
mobility.final$income[mobility.final$Income_P==16] <- 0
mobility.final$income[mobility.final$Income_P==1] <- 2500
mobility.final$income[mobility.final$Income_P==22] <- 7500
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
mobility.final$income[mobility.final$Income_P==17] <- 325000
mobility.final$income[mobility.final$Income_P==18] <- 375000
mobility.final$income[mobility.final$Income_P==19] <- 425000
mobility.final$income[mobility.final$Income_P==20] <- 475000
mobility.final$income[mobility.final$Income_P==21] <- 525000
mobility.final$income.div <- mobility.final$income/10000
# Hout adjustment does not make any difference here because there were no observations in the top income category
mobility.final$income.Hout <- mobility.final$income
mobility.final$income.Hout.div <- mobility.final$income.div
with(mobility.final, describe(cbind(income,income.div,income.Hout,income.Hout.div)))
mobility.final$z.income.Hout.div <- (mobility.final$income.Hout.div-mean(mobility.final$income.Hout.div,na.rm=TRUE))/sd(mobility.final$income.Hout.div,na.rm=TRUE)
with(mobility.final,describe(cbind(income.Hout.div,z.income.Hout.div)))

# parental income (uncorrected for inflation)
table(mobility.final$Income_Par)
# note that income was coded weird in qualtrics, so that category 22 was actually the $5,000-$9,999 category
mobility.final$parental.income <- NA
mobility.final$parental.income[mobility.final$Income_Par==16] <- 0
mobility.final$parental.income[mobility.final$Income_Par==1] <- 2500
mobility.final$parental.income[mobility.final$Income_Par==22] <- 7500
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
mobility.final$parental.income[mobility.final$Income_Par==17] <- 325000
mobility.final$parental.income[mobility.final$Income_Par==18] <- 375000
mobility.final$parental.income[mobility.final$Income_Par==19] <- 425000
mobility.final$parental.income[mobility.final$Income_Par==20] <- 475000
mobility.final$parental.income[mobility.final$Income_Par==21] <- 525000
mobility.final$parental.income.div <- mobility.final$parental.income/10000
with(mobility.final, describe(cbind(parental.income,parental.income.div)))
# parental income
mobility.final$parental.income.Hout <- mobility.final$parental.income
mobility.final$parental.income.Hout[mobility.final$Income_Par==21] <- 675539.659 
# this value determined with Hout formula
mobility.final$parental.income.Hout.div <- mobility.final$parental.income.Hout/10000
with(mobility.final, describe(cbind(parental.income,parental.income.div,parental.income.Hout,parental.income.Hout.div)))
mobility.final$z.parental.income.Hout.div <- (mobility.final$parental.income.Hout.div-mean(mobility.final$parental.income.Hout.div,na.rm=TRUE))/sd(mobility.final$parental.income.Hout.div,na.rm=TRUE)
with(mobility.final,describe(cbind(parental.income.Hout.div,z.parental.income.Hout.div)))

with(mobility.final, corr.test(cbind(income.Hout.div,parental.income.Hout.div,z.income.Hout.div,z.parental.income.Hout.div)))

### EDUCATION

# participant education
table(mobility.final$Edu_recode)
mobility.final$education <- NA
mobility.final$education[mobility.final$Edu_recode==1] <- 1
mobility.final$education[mobility.final$Edu_recode==2] <- 2
mobility.final$education[mobility.final$Edu_recode==3] <- 3
mobility.final$education[mobility.final$Edu_recode==4] <- 4
mobility.final$education[mobility.final$Edu_recode==5] <- 5
mobility.final$education[mobility.final$Edu_recode==6] <- 6
mobility.final$education[mobility.final$Edu_recode==7] <- 6
mobility.final$education[mobility.final$Edu_recode==8] <- NA
table(mobility.final$education)
describe(mobility.final$education)
mobility.final$z.education <- (mobility.final$education-mean(mobility.final$education,na.rm=TRUE))/sd(mobility.final$education,na.rm=TRUE)
with(mobility.final,describe(cbind(education,z.education)))

# father education
table(mobility.final$Edu_Par_1)
mobility.final$father.education <- NA
mobility.final$father.education[mobility.final$Edu_Par_1==1] <- 1
mobility.final$father.education[mobility.final$Edu_Par_1==2] <- 2
mobility.final$father.education[mobility.final$Edu_Par_1==3] <- 3
mobility.final$father.education[mobility.final$Edu_Par_1==4] <- 4
mobility.final$father.education[mobility.final$Edu_Par_1==5] <- 5
mobility.final$father.education[mobility.final$Edu_Par_1==6] <- 6
mobility.final$father.education[mobility.final$Edu_Par_1==7] <- 6
mobility.final$father.education[mobility.final$Edu_Par_1==8] <- NA
mobility.final$father.education[mobility.final$Edu_Par_1==9] <- NA
table(mobility.final$father.education)

# mother education
table(mobility.final$Edu_Par_2)
mobility.final$mother.education <- NA
mobility.final$mother.education[mobility.final$Edu_Par_2==1] <- 1
mobility.final$mother.education[mobility.final$Edu_Par_2==2] <- 2
mobility.final$mother.education[mobility.final$Edu_Par_2==3] <- 3
mobility.final$mother.education[mobility.final$Edu_Par_2==4] <- 4
mobility.final$mother.education[mobility.final$Edu_Par_2==5] <- 5
mobility.final$mother.education[mobility.final$Edu_Par_2==6] <- 6
mobility.final$mother.education[mobility.final$Edu_Par_2==7] <- 6
mobility.final$mother.education[mobility.final$Edu_Par_2==8] <- NA
mobility.final$mother.education[mobility.final$Edu_Par_2==9] <- NA
table(mobility.final$mother.education)

with(mobility.final, cor.test(father.education, mother.education))
mobility.final$parents.education <- with(mobility.final,(rowMeans(cbind(father.education,mother.education),na.rm=TRUE)))
with(mobility.final, describe(cbind(education,parents.education,father.education, mother.education)))
with(mobility.final, corr.test(cbind(education,parents.education,father.education,mother.education)))
mobility.final$z.parents.education <- (mobility.final$parents.education-mean(mobility.final$parents.education,na.rm=TRUE))/sd(mobility.final$parents.education,na.rm=TRUE)
with(mobility.final,describe(cbind(parents.education,z.parents.education)))

with(mobility.final, corr.test(cbind(education,parents.education,z.education,z.parents.education)))

### SUBJECTIVE SOCIAL CLASS
### DESTINATIONS

# Vlad social class destinations
mobility.final$vlad.class.destinations <- with(mobility.final,(rowMeans(cbind(subjclass_2,subjclass_3,subjclass_4),na.rm=TRUE)))
with(mobility.final, psych::alpha(data.frame(subjclass_2,subjclass_3,subjclass_4), na.rm=TRUE))
mobility.final$z.vlad.class.destinations <- scale(mobility.final$vlad.class.destinations)
with(mobility.final, describe(cbind(vlad.class.destinations,z.vlad.class.destinations)))

# Oveis measure
mobility.final$oveisdests <- mobility.final$socclass
mobility.final$SC_Current <- mobility.final$socclass
mobility.final$z.oveisdests <- scale(mobility.final$oveisdests)
with(mobility.final, describe(cbind(oveisdests,z.oveisdests)))

mobility.final$subjective.class.destinations <- with(mobility.final,(rowMeans(cbind(z.vlad.class.destinations,z.oveisdests),na.rm=TRUE)))
with(mobility.final, cor.test(z.vlad.class.destinations,z.oveisdests))
with(mobility.final, psych::alpha(data.frame(z.vlad.class.destinations,z.oveisdests), na.rm=TRUE))
describe(mobility.final$subjective.class.destinations)

mobility.final$z.subjective.class.destinations <- (mobility.final$subjective.class.destinations-mean(mobility.final$subjective.class.destinations,na.rm=TRUE))/sd(mobility.final$subjective.class.destinations,na.rm=TRUE)
with(mobility.final,describe(cbind(subjective.class.destinations,z.subjective.class.destinations)))

### SUBJECTIVE SOCIAL CLASS
### ORIGINS

# Vlad social class origins
mobility.final$vlad.class.origins <- with(mobility.final,(rowMeans(cbind(csubjclass_4,csubjclass_5,csubjclass_6),na.rm=TRUE)))
with(mobility.final, psych::alpha(data.frame(csubjclass_4,csubjclass_5,csubjclass_6), na.rm=TRUE))
mobility.final$z.vlad.class.origins <- scale(mobility.final$vlad.class.origins)
with(mobility.final, describe(cbind(vlad.class.origins,z.vlad.class.origins)))

# Oveis measure origins
mobility.final$oveisorigins <- mobility.final$csocclass
mobility.final$SC_Past <- mobility.final$csocclass
mobility.final$z.oveisorigins <- scale(mobility.final$oveisorigins)
with(mobility.final, describe(cbind(oveisorigins,z.oveisorigins)))

mobility.final$subjective.class.origins <- with(mobility.final,(rowMeans(cbind(z.vlad.class.origins,z.oveisorigins),na.rm=TRUE)))
with(mobility.final, cor.test(z.vlad.class.origins,z.oveisorigins))
with(mobility.final, psych::alpha(data.frame(z.vlad.class.origins,z.oveisorigins), na.rm=TRUE))
describe(mobility.final$subjective.class.origins)

mobility.final$z.subjective.class.origins <- (mobility.final$subjective.class.origins-mean(mobility.final$subjective.class.origins,na.rm=TRUE))/sd(mobility.final$subjective.class.origins,na.rm=TRUE)
with(mobility.final,describe(cbind(subjective.class.origins,z.subjective.class.origins)))

with(mobility.final, corr.test(cbind(subjective.class.origins,subjective.class.destinations,z.subjective.class.origins,z.subjective.class.destinations)))

# gender 
table(mobility.final$Gender)
mobility.final$Female <- NA
mobility.final$Female[mobility.final$Gender==1] <- 1
mobility.final$Female[mobility.final$Gender==2] <- 0
mobility.final$Female[mobility.final$Gender==3] <- 0
table(mobility.final$Female)

# ethnicity
table(mobility.final$Ethnicity)
mobility.final$Caucasian <- NA
mobility.final$Caucasian[mobility.final$Ethnicity=="1,3,4,6" | mobility.final$Ethnicity=="1,3,6" | mobility.final$Ethnicity=="1,6" | mobility.final$Ethnicity=="2,4,6" | mobility.final$Ethnicity=="2,6" | mobility.final$Ethnicity=="2,6,7" | mobility.final$Ethnicity=="3,4,6" | mobility.final$Ethnicity=="4,6" | mobility.final$Ethnicity=="6"] <- 1
mobility.final$Caucasian[is.na(mobility.final$Caucasian)] <- 0
table(mobility.final$Caucasian)
mobility.final$AmericanIndian <- NA
mobility.final$AmericanIndian[mobility.final$Ethnicity=="1" | mobility.final$Ethnicity=="1,3,4,6" | mobility.final$Ethnicity=="1,3,6" | mobility.final$Ethnicity=="1,4" | mobility.final$Ethnicity=="1,6"] <- 1
mobility.final$AmericanIndian[is.na(mobility.final$AmericanIndian)] <- 0
table(mobility.final$AmericanIndian)
mobility.final$Asian <- NA
mobility.final$Asian[mobility.final$Ethnicity=="2" | mobility.final$Ethnicity=="2,4,6" | mobility.final$Ethnicity=="2,6" | mobility.final$Ethnicity=="2,6,7"] <- 1
mobility.final$Asian[is.na(mobility.final$Asian)] <- 0
table(mobility.final$Asian)
mobility.final$AfricanAmerican <- NA
mobility.final$AfricanAmerican[mobility.final$Ethnicity=="1,3,4,6" | mobility.final$Ethnicity=="1,3,6" | mobility.final$Ethnicity=="3" | mobility.final$Ethnicity=="3,4" | mobility.final$Ethnicity=="3,4,6"] <- 1
mobility.final$AfricanAmerican[is.na(mobility.final$AfricanAmerican)] <- 0
table(mobility.final$AfricanAmerican)
mobility.final$Latin <- NA
mobility.final$Latin[mobility.final$Ethnicity=="1,3,4,6" | mobility.final$Ethnicity=="1,4" | mobility.final$Ethnicity=="2,4,6," | mobility.final$Ethnicity=="3,4" | mobility.final$Ethnicity=="3,4,6" | mobility.final$Ethnicity=="4" | mobility.final$Ethnicity=="4,6"] <- 1
mobility.final$Latin[is.na(mobility.final$Latin)] <- 0
table(mobility.final$Latin)
mobility.final$NativeHawaiian <- 0
table(mobility.final$NativeHawaiian)
mobility.final$Other <- NA
mobility.final$Other[mobility.final$Ethnicity=="2,6,7" | mobility.final$Ethnicity=="7"] <- 1
mobility.final$Other[is.na(mobility.final$Other)] <- 0
table(mobility.final$Other)
mobility.final$number.ethnicities.reported <- with(mobility.final, (rowSums(cbind(Caucasian,AmericanIndian,Asian,AfricanAmerican,Latin,NativeHawaiian,Other), na.rm=TRUE)))
table(mobility.final$number.ethnicities.reported)

#### ANALYSES WITH ENTITLEMENT ####

with(mobility.final, psych::alpha(data.frame(ent1,ent2,ent3,ent4,r.ent5,ent6,ent7,ent8,ent9), na.rm=TRUE))
mobility.final$z.entitlement <- (mobility.final$entitlement-mean(mobility.final$entitlement,na.rm=TRUE))/sd(mobility.final$entitlement,na.rm=TRUE)
mobility.final$entitlement.notitanic <- with(mobility.final, (rowMeans(cbind(ent1,ent2,ent4,r.ent5,ent6,ent7,ent8,ent9), na.rm=TRUE)))
mobility.final$z.entitlement.notitanic <- (mobility.final$entitlement.notitanic-mean(mobility.final$entitlement.notitanic,na.rm=TRUE))/sd(mobility.final$entitlement.notitanic,na.rm=TRUE)
with(mobility.final, describe(cbind(entitlement,z.entitlement,entitlement.notitanic,z.entitlement.notitanic)))

with(mobility.final, corr.test (cbind(z.subjective.class.destinations,z.subjective.class.origins,z.income.Hout.div,z.parental.income.Hout.div,z.education,z.parents.education,z.entitlement)))

#### SUBJECTIVE SOCIAL CLASS ####
# two way interaction between composite subjective class origins, composite subjective class destinations, and entitlement
# because they are aggregate of standardized scores, subjective class destinations and origins are already centered at 0
with(mobility.final, describe(cbind(subjective.class.destinations,z.subjective.class.destinations,subjective.class.origins,z.subjective.class.origins)))
with(mobility.final, cor.test(z.subjective.class.destinations,entitlement))
with(mobility.final, cor.test(z.subjective.class.origins,entitlement))
with(mobility.final, cor.test(subjective.class.destinations,subjective.class.origins))
with(mobility.final, cor.test(z.subjective.class.destinations,z.subjective.class.origins))
with(mobility.final, corr.test(cbind(subjective.class.destinations,subjective.class.origins,entitlement), use="complete", adjust="none"))
composite.subjective.ols.entitlement <- lm(z.entitlement ~ z.subjective.class.destinations*z.subjective.class.origins, data=mobility.final, na.action=na.omit); summary(composite.subjective.ols.entitlement)
# Plot
subjentitle<- data.frame(effect("z.subjective.class.destinations*z.subjective.class.origins", composite.subjective.ols.entitlement, xlevels=list(z.subjective.class.origins=c(-1,1))))
ggplot(subjentitle) + geom_line(aes(z.subjective.class.destinations, fit,  color = z.subjective.class.origins, linetype=factor(z.subjective.class.origins)), size= 1.25, )+ scale_colour_gradient(low = "darkred", high = "darkblue") + theme_bw()+
  coord_cartesian(xlim=c(-2.5,2.5), ylim=c(-2,2)) +
  scale_linetype_manual(values=c("dotted", "solid"))+
  theme(legend.position='none', panel.grid.major = element_blank(),
        text = element_text(size=15),
        panel.grid.minor = element_blank(),)+ xlab("Current Subjective SES")+ylab("Entitlement") 
# simple slopes
mobility.final$hi.subjective.origins <- mobility.final$z.subjective.class.origins - 1
mobility.final$lo.subjective.origins <- mobility.final$z.subjective.class.origins + 1
#---- Association between class destinations and entitlement among people with low and high class origins
subjective.entitlement.hi.origins <- lm(z.entitlement ~ z.subjective.class.destinations*hi.subjective.origins, data=mobility.final, na.action=na.omit); summary(subjective.entitlement.hi.origins)
subjective.entitlement.lo.origins <- lm(z.entitlement ~ z.subjective.class.destinations*lo.subjective.origins, data=mobility.final, na.action=na.omit); summary(subjective.entitlement.lo.origins)
# simple slopes
mobility.final$hi.subjective.destinations <- mobility.final$z.subjective.class.destinations - 1
mobility.final$lo.subjective.destinations <- mobility.final$z.subjective.class.destinations + 1
#---- Association between class origins and entitlement among people with low and high class destinatons
subjective.entitlement.hi.destinations <- lm(z.entitlement ~ hi.subjective.destinations*z.subjective.class.origins, data=mobility.final, na.action=na.omit); summary(subjective.entitlement.hi.destinations)
subjective.entitlement.lo.destinations <- lm(z.entitlement ~ lo.subjective.destinations*z.subjective.class.origins, data=mobility.final, na.action=na.omit); summary(subjective.entitlement.lo.destinations)
# two way interaction with controls
composite.subjective.ols.entitlement.controls <- lm(z.entitlement ~ z.subjective.class.destinations*z.subjective.class.origins + participant.age + Female + Caucasian, data=mobility.final, na.action=na.omit); summary(composite.subjective.ols.entitlement.controls)
# simple slopes ---- Association between class destinations and entitlement among people with low and high class origins
summary(subjective.entitlement.hi.origins.controls <- lm(z.entitlement ~ z.subjective.class.destinations*hi.subjective.origins + participant.age + Female + Caucasian, data=mobility.final, na.action=na.omit))
summary(subjective.entitlement.lo.origins.controls <- lm(z.entitlement ~ z.subjective.class.destinations*lo.subjective.origins + participant.age + Female + Caucasian, data=mobility.final, na.action=na.omit))
# simple slopes ---- Association between class origins and entitlement among people with low and high class destinatons
summary(subjective.entitlement.hi.destinations.controls <- lm(z.entitlement ~ hi.subjective.destinations*z.subjective.class.origins + participant.age + Female + Caucasian, data=mobility.final, na.action=na.omit))
summary(subjective.entitlement.lo.destinations.controls <- lm(z.entitlement ~ lo.subjective.destinations*z.subjective.class.origins + participant.age + Female + Caucasian, data=mobility.final, na.action=na.omit))
# exploratory analysis of two-way interaction with no Titanic item
composite.subjective.ols.entitlement.notitanic <- lm(z.entitlement.notitanic ~ z.subjective.class.destinations*z.subjective.class.origins, data=mobility.final, na.action=na.omit); summary(composite.subjective.ols.entitlement.notitanic)

#### INCOME WITH HOUT CORRECTION FOR HIGHEST INCOME BRACKET ####
with(mobility.final, describe(cbind(income.Hout.div,z.income.Hout.div)))
table(mobility.final$income.Hout.div)
table(mobility.final$z.income.Hout.div)
with(mobility.final, describe(cbind(parental.income.Hout.div,z.parental.income.Hout.div)))
table(mobility.final$parental.income.Hout.div)
table(mobility.final$z.parental.income.Hout.div)
with(mobility.final, cor.test(z.income.Hout.div,entitlement))
with(mobility.final, cor.test(z.parental.income.Hout.div,entitlement))
with(mobility.final, cor.test(z.income.Hout.div,z.parental.income.Hout.div))
with(mobility.final, corr.test(cbind(income.Hout.div,parental.income.Hout.div,entitlement), use="complete", adjust="none"))
income.Hout.entitlement <- lm(z.entitlement ~ z.income.Hout.div*z.parental.income.Hout.div, data=mobility.final, na.action=na.omit); summary(income.Hout.entitlement)
# Plot
incentitle<- data.frame(effect("z.income.Hout.div*z.parental.income.Hout.div", income.Hout.entitlement, xlevels=list(z.parental.income.Hout.div=c(-.5,1))))
ggplot(incentitle) + geom_line(aes(z.income.Hout.div, fit,  color = z.parental.income.Hout.div, linetype=factor(z.parental.income.Hout.div)), size= 2, )+ scale_colour_gradient(low = "darkred", high = "darkblue") + theme_bw()+
  coord_cartesian(xlim=c(-1,7), ylim=c(-2,2)) +
  scale_linetype_manual(values=c("dotted", "solid"))+
  theme(legend.position='none', panel.grid.major = element_blank(),
        text = element_text(size=20),
        panel.grid.minor = element_blank(),)+ xlab("Current Income")+ylab("Entitlement") 
# simple slopes
#---- Association between class destinations and entitlement among people with low and high class origins
with(mobility.final, describe(cbind(income.Hout.div,z.income.Hout.div,parental.income.Hout.div,z.parental.income.Hout.div)))
#####################################################################################################################
############### this is a problem at the low end of parental income because -1 SD is lower than 0 ##############
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
# two way interaction with controls
income.Hout.entitlement.controls <- lm(z.entitlement ~ z.income.Hout.div*z.parental.income.Hout.div + participant.age + Female + Caucasian, data=mobility.final, na.action=na.omit); summary(income.Hout.entitlement.controls)
# simple slopes ---- Association between class destinations and entitlement among people with low and high class origins
summary(income.Hout.entitlement.hi.origins.controls <- lm(z.entitlement ~ z.income.Hout.div*hi.parental.income.Hout.div + participant.age + Female + Caucasian, data=mobility.final, na.action=na.omit))
summary(income.Hout.entitlement.lo.origins.controls <- lm(z.entitlement ~ z.income.Hout.div*lo.parental.income.Hout.div + participant.age + Female + Caucasian, data=mobility.final, na.action=na.omit))
# simple slopes ---- Association between class origins and entitlement among people with low and high class destinations
summary(income.Hout.entitlement.hi.destinations.controls <- lm(z.entitlement ~ hi.income.Hout.div*z.parental.income.Hout.div + participant.age + Female + Caucasian, data=mobility.final, na.action=na.omit))
summary(income.Hout.entitlement.lo.destinations.controls <- lm(z.entitlement ~ lo.income.Hout.div*z.parental.income.Hout.div + participant.age + Female + Caucasian, data=mobility.final, na.action=na.omit))
# exploratory analysis of two-way interaction with no Titanic item
income.Hout.entitlement.notitanic <- lm(z.entitlement.notitanic ~ z.income.Hout.div*z.parental.income.Hout.div, data=mobility.final, na.action=na.omit); summary(income.Hout.entitlement.notitanic)

#### EDUCATION ####
# two way interaction between class origins, class destinations, and entitlement
with(mobility.final, describe(cbind(education,z.education)))
table(mobility.final$education)
table(mobility.final$z.education)
with(mobility.final, describe(cbind(parents.education,z.parents.education)))
table(mobility.final$parents.education)
table(mobility.final$z.parents.education)
with(mobility.final, corr.test(cbind(education,parents.education,entitlement), use="complete", adjust="none"))
with(mobility.final, cor.test(z.education,entitlement))
with(mobility.final, cor.test(z.parents.education,entitlement))
with(mobility.final, cor.test(z.education,z.parents.education))
education.ols.entitlement <- lm(z.entitlement ~ z.education*z.parents.education, data=mobility.final, na.action=na.omit); summary(education.ols.entitlement)
# Plot
eduentitle<- data.frame(effect("z.education*z.parents.education", education.ols.entitlement, xlevels=list(z.parents.education=c(-1,1))))
ggplot(eduentitle) + geom_line(aes(z.education, fit,  color = z.parents.education, linetype=factor(z.parents.education)), size= 1.25, )+ scale_colour_gradient(low = "darkred", high = "darkblue") + theme_bw()+
  coord_cartesian(xlim=c(-2.5,2.5), ylim=c(-2,2)) +
  scale_linetype_manual(values=c("dotted", "solid"))+
  theme(legend.position='none', panel.grid.major = element_blank(),
        text = element_text(size=15),
        panel.grid.minor = element_blank(),)+ xlab("Current Education")+ylab("Entitlement") 
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
# with controls
education.ols.entitlement.controls <- lm(z.entitlement ~ z.education*z.parents.education + participant.age + Female + Caucasian, data=mobility.final, na.action=na.omit); summary(education.ols.entitlement.controls)
# simple slopes ---- Association between class destinations and entitlement among people with low and high class origins
summary(education.entitlement.hi.origins.controls <- lm(z.entitlement ~ z.education*hi.parents.education + participant.age + Female + Caucasian, data=mobility.final, na.action=na.omit))
summary(education.entitlement.lo.origins.controls <- lm(z.entitlement ~ z.education*lo.parents.education + participant.age + Female + Caucasian, data=mobility.final, na.action=na.omit))
# simple slopes ---- Association between class origins and entitlement among people with low and high class destinations
summary(education.entitlement.hi.destinations.controls <- lm(z.entitlement ~ hi.education*z.parents.education + participant.age + Female + Caucasian, data=mobility.final, na.action=na.omit))
summary(education.entitlement.lo.destinations.controls <- lm(z.entitlement ~ lo.education*z.parents.education + participant.age + Female + Caucasian, data=mobility.final, na.action=na.omit))
# exploratory analysis of two-way interaction with no Titanic item
education.ols.entitlement.notitanic <- lm(z.entitlement.notitanic ~ z.education*z.parents.education, data=mobility.final, na.action=na.omit); summary(education.ols.entitlement.notitanic)

###########################################################################################################################
##################################################### end of analyses #####################################################
###########################################################################################################################
