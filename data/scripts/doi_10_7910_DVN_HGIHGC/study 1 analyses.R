library(psych)
library(nlme)
library(lme4)
library(effects)
library(lavaan)
library(ggplot2)

setwd ("ENTER PATH")
data.study1.mobility <- read.csv("study 1 data.csv")

##### calculate entitlement scores ####
# first adjust the coding
data.study1.mobility$r.entitlement_4 <- data.study1.mobility$entitlement_4-3; data.study1.mobility$r.entitlement_5 <- data.study1.mobility$entitlement_5-3
data.study1.mobility$r.entitlement_6 <- data.study1.mobility$entitlement_6-3; data.study1.mobility$r.entitlement_7 <- data.study1.mobility$entitlement_7-3
data.study1.mobility$r.entitlement_8 <- data.study1.mobility$entitlement_8-3; data.study1.mobility$r.entitlement_9 <- data.study1.mobility$entitlement_9-3
data.study1.mobility$r.entitlement_10 <- data.study1.mobility$entitlement_10-3; data.study1.mobility$r.entitlement_11 <- data.study1.mobility$entitlement_11-3
data.study1.mobility$r.entitlement_12 <- data.study1.mobility$entitlement_12-3
table(data.study1.mobility$r.entitlement_8)
data.study1.mobility$rev.r.entitlement_8 <- 8-data.study1.mobility$r.entitlement_8
table(data.study1.mobility$rev.r.entitlement_8)
data.study1.mobility$entitlement <- with(data.study1.mobility,(rowMeans(cbind(r.entitlement_4,r.entitlement_5,r.entitlement_6,r.entitlement_7,rev.r.entitlement_8,r.entitlement_9,r.entitlement_10,r.entitlement_11,r.entitlement_12),na.rm=TRUE)))
describe(data.study1.mobility$entitlement)

##### descriptive statistics ####

data.study1.mobility$participant.age <- data.study1.mobility$Age+15
describe(data.study1.mobility$participant.age)

table(data.study1.mobility$Gen)
data.study1.mobility$Gender <- NA
data.study1.mobility$Gender[data.study1.mobility$Gen=="f"] <- 1
data.study1.mobility$Gender[data.study1.mobility$Gen=="F"] <- 1
data.study1.mobility$Gender[data.study1.mobility$Gen=="female"] <- 1
data.study1.mobility$Gender[data.study1.mobility$Gen=="Female"] <- 1
data.study1.mobility$Gender[data.study1.mobility$Gen=="FEMALE"] <- 1
data.study1.mobility$Gender[data.study1.mobility$Gen=="female "] <- 1
data.study1.mobility$Gender[data.study1.mobility$Gen=="Female "] <- 1
data.study1.mobility$Gender[data.study1.mobility$Gen=="FEMENINE"] <- 1
data.study1.mobility$Gender[data.study1.mobility$Gen=="femle"] <- 1
data.study1.mobility$Gender[data.study1.mobility$Gen=="genderqueer (and thank you, by the way, for having this be a written answer)"] <- 2
data.study1.mobility$Gender[data.study1.mobility$Gen=="m"] <- 0
data.study1.mobility$Gender[data.study1.mobility$Gen=="M"] <- 0
data.study1.mobility$Gender[data.study1.mobility$Gen=="male"] <- 0
data.study1.mobility$Gender[data.study1.mobility$Gen=="Male"] <- 0
data.study1.mobility$Gender[data.study1.mobility$Gen=="MALE"] <- 0
data.study1.mobility$Gender[data.study1.mobility$Gen=="Male "] <- 0
data.study1.mobility$Gender[data.study1.mobility$Gen=="masculino"] <- 0
data.study1.mobility$Gender[data.study1.mobility$Gen=="woman"] <- 1
table(data.study1.mobility$Gender)
data.study1.mobility$Female <- NA
data.study1.mobility$Female[data.study1.mobility$Gender==1] <- 1
data.study1.mobility$Female[data.study1.mobility$Gender==0] <- 0
data.study1.mobility$Female[data.study1.mobility$Gender==2] <- 0
table(data.study1.mobility$Female)

# ethnicity codes
# American Indian/Alaska Native/First Nations Person (1)
# Hawaiian or Other Pacific Islander (2)
# Asian or Asian American (3)
# Black or African American (4)
# Hispanic or Latino/a (5)
# Non-Hispanic White (6)
# Other: (7) ____________________
table(data.study1.mobility$Eth_1)
table(data.study1.mobility$Eth_2)
table(data.study1.mobility$Eth_3)
table(data.study1.mobility$Eth_4)
table(data.study1.mobility$Eth_5)
table(data.study1.mobility$Eth_6)
table(data.study1.mobility$Eth_7)
data.study1.mobility$number.ethnicities.reported <- with(data.study1.mobility, (rowSums(cbind(Eth_1,Eth_2,Eth_3,Eth_4,Eth_5,Eth_6,Eth_7), na.rm=TRUE)))
table(data.study1.mobility$number.ethnicities.reported)
data.study1.mobility$Caucasian <- NA
data.study1.mobility$Caucasian[data.study1.mobility$Eth_1==1] <- 0
data.study1.mobility$Caucasian[data.study1.mobility$Eth_2==1] <- 0
data.study1.mobility$Caucasian[data.study1.mobility$Eth_3==1] <- 0
data.study1.mobility$Caucasian[data.study1.mobility$Eth_4==1] <- 0
data.study1.mobility$Caucasian[data.study1.mobility$Eth_5==1] <- 0
data.study1.mobility$Caucasian[data.study1.mobility$Eth_7==1] <- 0
data.study1.mobility$Caucasian[data.study1.mobility$Eth_6==1] <- 1
table(data.study1.mobility$Caucasian)

##### create social class variables ####

### INCOME

# personal income
# note that 16 meant no income. 14 and 15 were the top two categories
table(data.study1.mobility$Inc)
data.study1.mobility$income <- NA
data.study1.mobility$income[data.study1.mobility$Inc==16] <- 0
data.study1.mobility$income[data.study1.mobility$Inc==1] <- 5000
data.study1.mobility$income[data.study1.mobility$Inc==2] <- 12500
data.study1.mobility$income[data.study1.mobility$Inc==3] <- 17500
data.study1.mobility$income[data.study1.mobility$Inc==4] <- 22500
data.study1.mobility$income[data.study1.mobility$Inc==5] <- 27500
data.study1.mobility$income[data.study1.mobility$Inc==6] <- 35000
data.study1.mobility$income[data.study1.mobility$Inc==7] <- 45000
data.study1.mobility$income[data.study1.mobility$Inc==8] <- 55000
data.study1.mobility$income[data.study1.mobility$Inc==9] <- 65000
data.study1.mobility$income[data.study1.mobility$Inc==10] <- 77500
data.study1.mobility$income[data.study1.mobility$Inc==11] <- 92500
data.study1.mobility$income[data.study1.mobility$Inc==12] <- 125000
data.study1.mobility$income[data.study1.mobility$Inc==13] <- 175000
data.study1.mobility$income[data.study1.mobility$Inc==14] <- 225000
data.study1.mobility$income[data.study1.mobility$Inc==15] <- 275000
describe(data.study1.mobility$income)
data.study1.mobility$income.div <- data.study1.mobility$income/10000
# Hout value for top earners calculated with excel spreadsheet
data.study1.mobility$income.Hout <- data.study1.mobility$income
table(data.study1.mobility$Inc)
# Hout formula
250000*(1+(((log(8+5)-log(5))/(log(250000)-log(200000)))/(((log(8+5)-log(5))/(log(250000)-log(200000)))-1)))
data.study1.mobility$income.Hout[data.study1.mobility$Inc==15] <- 576171.9462
data.study1.mobility$income.Hout.div <- data.study1.mobility$income.Hout/10000
with(data.study1.mobility, describe(cbind(income,income.div,income.Hout,income.Hout.div)))
data.study1.mobility$z.income.Hout.div <- (data.study1.mobility$income.Hout.div-mean(data.study1.mobility$income.Hout.div,na.rm=TRUE))/sd(data.study1.mobility$income.Hout.div,na.rm=TRUE)
with(data.study1.mobility,describe(cbind(income.Hout.div,z.income.Hout.div)))

# parental income
table(data.study1.mobility$FamInc)
data.study1.mobility$parental.income <- NA
data.study1.mobility$parental.income[data.study1.mobility$FamInc==16] <- 0
data.study1.mobility$parental.income[data.study1.mobility$FamInc==1] <- 5000
data.study1.mobility$parental.income[data.study1.mobility$FamInc==2] <- 12500
data.study1.mobility$parental.income[data.study1.mobility$FamInc==3] <- 17500
data.study1.mobility$parental.income[data.study1.mobility$FamInc==4] <- 22500
data.study1.mobility$parental.income[data.study1.mobility$FamInc==5] <- 27500
data.study1.mobility$parental.income[data.study1.mobility$FamInc==6] <- 35000
data.study1.mobility$parental.income[data.study1.mobility$FamInc==7] <- 45000
data.study1.mobility$parental.income[data.study1.mobility$FamInc==8] <- 55000
data.study1.mobility$parental.income[data.study1.mobility$FamInc==9] <- 65000
data.study1.mobility$parental.income[data.study1.mobility$FamInc==10] <- 77500
data.study1.mobility$parental.income[data.study1.mobility$FamInc==11] <- 92500
data.study1.mobility$parental.income[data.study1.mobility$FamInc==12] <- 125000
data.study1.mobility$parental.income[data.study1.mobility$FamInc==13] <- 175000
data.study1.mobility$parental.income[data.study1.mobility$FamInc==14] <- 225000
data.study1.mobility$parental.income[data.study1.mobility$FamInc==15] <- 275000
describe(data.study1.mobility$parental.income)
data.study1.mobility$parental.income.div <- data.study1.mobility$parental.income/10000
# Hout value for top earners calculated with excel spreadsheet
data.study1.mobility$parental.income.Hout <- data.study1.mobility$parental.income
table(data.study1.mobility$FamInc)
# Hout formula
250000*(1+(((log(4+6)-log(6))/(log(250000)-log(200000)))/(((log(4+6)-log(6))/(log(250000)-log(200000)))-1)))
data.study1.mobility$parental.income.Hout[data.study1.mobility$FamInc==15] <- 693915.0652
data.study1.mobility$parental.income.Hout.div <- data.study1.mobility$parental.income.Hout/10000
with(data.study1.mobility, describe(cbind(parental.income,parental.income.div,parental.income.Hout,parental.income.Hout.div)))
data.study1.mobility$z.parental.income.Hout.div <- (data.study1.mobility$parental.income.Hout.div-mean(data.study1.mobility$parental.income.Hout.div,na.rm=TRUE))/sd(data.study1.mobility$parental.income.Hout.div,na.rm=TRUE)

with(data.study1.mobility, corr.test(cbind(income.Hout.div,parental.income.Hout.div,z.income.Hout.div,z.parental.income.Hout.div)))

### EDUCATION

# participant education
table(data.study1.mobility$Edu)
data.study1.mobility$education <- NA
data.study1.mobility$education[data.study1.mobility$Edu==1] <- 1
data.study1.mobility$education[data.study1.mobility$Edu==2] <- 2
data.study1.mobility$education[data.study1.mobility$Edu==3] <- 3
data.study1.mobility$education[data.study1.mobility$Edu==4] <- 4
data.study1.mobility$education[data.study1.mobility$Edu==5] <- 5
table(data.study1.mobility$education)
describe(data.study1.mobility$education)
data.study1.mobility$z.education <- (data.study1.mobility$education-mean(data.study1.mobility$education,na.rm=TRUE))/sd(data.study1.mobility$education,na.rm=TRUE)

# father education
table(data.study1.mobility$OSES1)
data.study1.mobility$father.education <- NA
data.study1.mobility$father.education[data.study1.mobility$OSES1==1] <- 1
data.study1.mobility$father.education[data.study1.mobility$OSES1==2] <- 2
data.study1.mobility$father.education[data.study1.mobility$OSES1==3] <- 3
data.study1.mobility$father.education[data.study1.mobility$OSES1==4] <- 4
data.study1.mobility$father.education[data.study1.mobility$OSES1==5] <- 5
data.study1.mobility$father.education[data.study1.mobility$OSES1==6] <- NA
table(data.study1.mobility$father.education)

# mother education
table(data.study1.mobility$OSES2)
data.study1.mobility$mother.education <- NA
data.study1.mobility$mother.education[data.study1.mobility$OSES2==1] <- 1
data.study1.mobility$mother.education[data.study1.mobility$OSES2==2] <- 2
data.study1.mobility$mother.education[data.study1.mobility$OSES2==3] <- 3
data.study1.mobility$mother.education[data.study1.mobility$OSES2==4] <- 4
data.study1.mobility$mother.education[data.study1.mobility$OSES2==5] <- 5
data.study1.mobility$mother.education[data.study1.mobility$OSES2==6] <- NA
table(data.study1.mobility$mother.education)

with(data.study1.mobility, cor.test(father.education, mother.education))
data.study1.mobility$parents.education <- with(data.study1.mobility,(rowMeans(cbind(father.education,mother.education),na.rm=TRUE)))
with(data.study1.mobility, describe(cbind(education,parents.education,father.education,mother.education)))
with(data.study1.mobility, corr.test(cbind(education,parents.education,father.education,mother.education)))
data.study1.mobility$z.parents.education <- (data.study1.mobility$parents.education-mean(data.study1.mobility$parents.education,na.rm=TRUE))/sd(data.study1.mobility$parents.education,na.rm=TRUE)

with(data.study1.mobility, corr.test(cbind(education,parents.education,z.education,z.parents.education)))

### SUBJECTIVE SOCIAL CLASS

### Vlad measure

# Vlad social class destinations
with(data.study1.mobility, psych::alpha(data.frame(vladdests_4,vladdests_5,vladdests_6), na.rm=TRUE))
data.study1.mobility$vlad.class.destinations <- with(data.study1.mobility,(rowMeans(cbind(vladdests_4,vladdests_5,vladdests_6),na.rm=TRUE)))
# Vlad social class origins
with(data.study1.mobility, psych::alpha(data.frame(vladorigins_4,vladorigins_5,vladorigins_6), na.rm=TRUE))
data.study1.mobility$vlad.class.origins <- with(data.study1.mobility,(rowMeans(cbind(vladorigins_4,vladorigins_5,vladorigins_6),na.rm=TRUE)))
with(data.study1.mobility, describe(cbind(vlad.class.destinations,vlad.class.origins)))
with(data.study1.mobility, cor.test(vlad.class.destinations,vlad.class.origins))

### Oveis 5-point scale
data.study1.mobility$r.SubjCurr <- 6-data.study1.mobility$SubjCurr
data.study1.mobility$r.SSES2 <- 6-data.study1.mobility$SSES2
data.study1.mobility$SC_Current <- data.study1.mobility$r.SubjCurr
data.study1.mobility$SC_Past <- data.study1.mobility$r.SSES2
with(data.study1.mobility, describe(cbind(r.SubjCurr,SC_Current,r.SSES2,SC_Past)))
with(data.study1.mobility, cor.test(SC_Current,SC_Past))

# combining two subjective social class variables
data.study1.mobility$zvlad.class.destinations <- scale(data.study1.mobility$vlad.class.destinations)
data.study1.mobility$zSC_Current <- scale(data.study1.mobility$r.SubjCurr)
with(data.study1.mobility, cor.test(zvlad.class.destinations,zSC_Current))
data.study1.mobility$subjective.class.destinations <- with(data.study1.mobility,(rowMeans(cbind(zvlad.class.destinations,zSC_Current),na.rm=TRUE)))
data.study1.mobility$z.subjective.class.destinations <- (data.study1.mobility$subjective.class.destinations-mean(data.study1.mobility$subjective.class.destinations,na.rm=TRUE))/sd(data.study1.mobility$subjective.class.destinations,na.rm=TRUE)
describe(data.study1.mobility$z.subjective.class.destinations)
data.study1.mobility$zvlad.class.origins <- scale(data.study1.mobility$vlad.class.origins)
data.study1.mobility$zSC_Past <- scale(data.study1.mobility$r.SSES2)
with(data.study1.mobility, cor.test(zvlad.class.origins,zSC_Past))
data.study1.mobility$subjective.class.origins <- with(data.study1.mobility,(rowMeans(cbind(zvlad.class.origins,zSC_Past),na.rm=TRUE)))
data.study1.mobility$z.subjective.class.origins <- (data.study1.mobility$subjective.class.origins-mean(data.study1.mobility$subjective.class.origins,na.rm=TRUE))/sd(data.study1.mobility$subjective.class.origins,na.rm=TRUE)
describe(data.study1.mobility$z.subjective.class.origins)

with(data.study1.mobility, describe(cbind(subjective.class.destinations,subjective.class.origins)))
with(data.study1.mobility, cor.test(subjective.class.destinations,subjective.class.origins))
with(data.study1.mobility, cor.test(z.subjective.class.destinations,z.subjective.class.origins))

### DESCRIPTIVES

with(data.study1.mobility, describe(cbind(subjective.class.destinations,subjective.class.origins,income.Hout.div,parental.income.Hout.div,education,parents.education)))

#### ANALYSES WITH ENTITLEMENT ####

describe(data.study1.mobility$entitlement)
with(data.study1.mobility, psych::alpha(data.frame(r.entitlement_4,r.entitlement_5,r.entitlement_6,r.entitlement_7,rev.r.entitlement_8,r.entitlement_9,r.entitlement_10,r.entitlement_11,r.entitlement_12), na.rm=TRUE))
data.study1.mobility$z.entitlement <- (data.study1.mobility$entitlement-mean(data.study1.mobility$entitlement,na.rm=TRUE))/sd(data.study1.mobility$entitlement,na.rm=TRUE)
describe(data.study1.mobility$z.entitlement)
data.study1.mobility$entitlement.notitanic <- with(data.study1.mobility,(rowMeans(cbind(r.entitlement_4,r.entitlement_5,r.entitlement_7,rev.r.entitlement_8,r.entitlement_9,r.entitlement_10,r.entitlement_11,r.entitlement_12),na.rm=TRUE)))
data.study1.mobility$z.entitlement.notitanic <- (data.study1.mobility$entitlement.notitanic-mean(data.study1.mobility$entitlement.notitanic,na.rm=TRUE))/sd(data.study1.mobility$entitlement.notitanic,na.rm=TRUE)
with(data.study1.mobility,describe(cbind(entitlement,z.entitlement,entitlement.notitanic,z.entitlement.notitanic)))
cor.test(data.study1.mobility$entitlement,data.study1.mobility$entitlement.notitanic)

#### CORRELATIONS #####

with(data.study1.mobility, describe(cbind(subjective.class.destinations,subjective.class.origins,income.Hout.div,parental.income.Hout.div,education,parents.education,entitlement)))
with(data.study1.mobility, corr.test(cbind(subjective.class.destinations,subjective.class.origins,income.Hout.div,parental.income.Hout.div,education,parents.education,entitlement)))

with(data.study1.mobility, describe(cbind(z.subjective.class.destinations,z.subjective.class.origins,z.income.Hout.div,z.parental.income.Hout.div,z.education,z.parents.education,z.entitlement)))
with(data.study1.mobility, corr.test(cbind(z.subjective.class.destinations,z.subjective.class.origins,z.income.Hout.div,z.parental.income.Hout.div,z.education,z.parents.education,z.entitlement)))

#### SUBJECTIVE SOCIAL CLASS ####
# two way interaction between composite subjective class origins, composite subjective class destinations, and entitlement
# because they are aggregate of standardized scores, subjective class destinations and origins are already centered at 0
with(data.study1.mobility, cor.test(z.subjective.class.destinations,z.entitlement))
with(data.study1.mobility, cor.test(z.subjective.class.origins,z.entitlement))
composite.subjective.ols.entitlement.linear <- lm(z.entitlement ~ z.subjective.class.destinations+z.subjective.class.origins, data=data.study1.mobility, na.action=na.omit); summary(composite.subjective.ols.entitlement.linear)
composite.subjective.ols.entitlement <- lm(z.entitlement ~ z.subjective.class.destinations*z.subjective.class.origins, data=data.study1.mobility, na.action=na.omit); summary(composite.subjective.ols.entitlement)
with(data.study1.mobility, describe(cbind(z.subjective.class.destinations,z.subjective.class.origins)))
describe(data.study1.mobility$z.subjective.class.origins)
# simple slopes ---- Association between class destinations and entitlement among people with low and high class origins
data.study1.mobility$hi.subjective.origins <- data.study1.mobility$z.subjective.class.origins - 1
data.study1.mobility$lo.subjective.origins <- data.study1.mobility$z.subjective.class.origins + 1
subjective.entitlement.hi.origins <- lm(z.entitlement ~ z.subjective.class.destinations*hi.subjective.origins, data=data.study1.mobility, na.action=na.omit); summary(subjective.entitlement.hi.origins)
subjective.entitlement.lo.origins <- lm(z.entitlement ~ z.subjective.class.destinations*lo.subjective.origins, data=data.study1.mobility, na.action=na.omit); summary(subjective.entitlement.lo.origins)
# simple slopes ---- Association between class origins and entitlement among people with low and high class destinations
data.study1.mobility$hi.subjective.destinations <- data.study1.mobility$z.subjective.class.destinations - 1
data.study1.mobility$lo.subjective.destinations <- data.study1.mobility$z.subjective.class.destinations + 1
subjective.entitlement.hi.destinations <- lm(z.entitlement ~ hi.subjective.destinations*z.subjective.class.origins, data=data.study1.mobility, na.action=na.omit); summary(subjective.entitlement.hi.destinations)
subjective.entitlement.lo.destinations <- lm(z.entitlement ~ lo.subjective.destinations*z.subjective.class.origins, data=data.study1.mobility, na.action=na.omit); summary(subjective.entitlement.lo.destinations)
# Plot
subjentitle <- data.frame(effect("z.subjective.class.destinations*z.subjective.class.origins", composite.subjective.ols.entitlement, xlevels=list(z.subjective.class.origins=c(-1,1))))
ggplot(subjentitle) + geom_line(aes(z.subjective.class.destinations, fit,  color = z.subjective.class.origins, linetype=factor(z.subjective.class.origins)), size= 1.25, )+ scale_colour_gradient(low = "darkred", high = "darkblue") + theme_bw()+
  coord_cartesian(xlim=c(-2,2), ylim=c(-2,2)) +
  scale_linetype_manual(values=c("dotted", "solid"))+
  theme(legend.position='none', panel.grid.major = element_blank(),
        text = element_text(size=15),
        panel.grid.minor = element_blank(),)+ xlab("Current Subjective SES")+ylab("Entitlement") 
# two way interaction with controls
composite.subjective.ols.entitlement.controls <- lm(z.entitlement ~ z.subjective.class.destinations*z.subjective.class.origins + participant.age + Female + Caucasian, data=data.study1.mobility, na.action=na.omit); summary(composite.subjective.ols.entitlement.controls)
# simple slopes ---- Association between class destinations and entitlement among people with low and high class origins
summary(subjective.entitlement.hi.origins.controls <- lm(z.entitlement ~ z.subjective.class.destinations*hi.subjective.origins + participant.age + Female + Caucasian, data=data.study1.mobility, na.action=na.omit))
summary(subjective.entitlement.lo.origins.controls <- lm(z.entitlement ~ z.subjective.class.destinations*lo.subjective.origins + participant.age + Female + Caucasian, data=data.study1.mobility, na.action=na.omit))
# simple slopes ---- Association between class origins and entitlement among people with low and high class destinations
summary(subjective.entitlement.hi.destinations.controls <- lm(z.entitlement ~ hi.subjective.destinations*z.subjective.class.origins + participant.age + Female + Caucasian, data=data.study1.mobility, na.action=na.omit))
summary(subjective.entitlement.lo.destinations.controls <- lm(z.entitlement ~ lo.subjective.destinations*z.subjective.class.origins + participant.age + Female + Caucasian, data=data.study1.mobility, na.action=na.omit))
# exploratory two-way interaction no Titanic
composite.subjective.ols.entitlement.notitanic <- lm(z.entitlement.notitanic ~ z.subjective.class.destinations*z.subjective.class.origins, data=data.study1.mobility, na.action=na.omit); summary(composite.subjective.ols.entitlement.notitanic)
# exploratory three-way interaction with gender
summary(composite.subjective.ols.entitlement.gender <- lm(z.entitlement ~ z.subjective.class.destinations*z.subjective.class.origins*Female, data=data.study1.mobility, na.action=na.omit))
# exploratory three-way interaction with ethnicity
summary(composite.subjective.ols.entitlement.ethnicity <- lm(z.entitlement ~ z.subjective.class.destinations*z.subjective.class.origins*Caucasian, data=data.study1.mobility, na.action=na.omit))

#### INCOME WITH HOUT CORRECTION FOR HIGHEST INCOME BRACKET ####
with(data.study1.mobility, describe(cbind(income.Hout,parental.income.Hout)))
with(data.study1.mobility, cor.test(z.income.Hout.div,z.entitlement))
with(data.study1.mobility, cor.test(z.parental.income.Hout.div,z.entitlement))
with(data.study1.mobility, describe(cbind(income.Hout.div,z.income.Hout.div)))
table(data.study1.mobility$income.Hout.div)
table(data.study1.mobility$z.income.Hout.div)
with(data.study1.mobility, describe(cbind(parental.income.Hout.div,z.parental.income.Hout.div)))
table(data.study1.mobility$parental.income.Hout.div)
table(data.study1.mobility$z.parental.income.Hout.div)
with(data.study1.mobility, corr.test(cbind(income.Hout.div,parental.income.Hout.div,entitlement), use="complete", adjust="none"))
income.Hout.entitlement.linear <- lm(z.entitlement ~ z.income.Hout.div+z.parental.income.Hout.div, data=data.study1.mobility, na.action=na.omit); summary(income.Hout.entitlement.linear)
income.Hout.entitlement <- lm(z.entitlement ~ z.income.Hout.div*z.parental.income.Hout.div, data=data.study1.mobility, na.action=na.omit); summary(income.Hout.entitlement)
# simple slopes
#---- Association between class destinations and entitlement among people with low and high class origins
with(data.study1.mobility, describe(cbind(income.Hout.div,z.income.Hout.div,parental.income.Hout.div,z.parental.income.Hout.div)))
#####################################################################################################################
############### this is a problem at the low end of parental income because -1 SD is lower than 0 ###################
#####################################################################################################################
data.study1.mobility$hi.parental.income.Hout.div <- data.study1.mobility$z.parental.income.Hout.div - 1
data.study1.mobility$lo.parental.income.Hout.div <- data.study1.mobility$z.parental.income.Hout.div + .5
income.Hout.entitlement.hi.origins <- lm(z.entitlement ~ z.income.Hout.div*hi.parental.income.Hout.div, data=data.study1.mobility, na.action=na.omit); summary(income.Hout.entitlement.hi.origins)
income.Hout.entitlement.lo.origins <- lm(z.entitlement ~ z.income.Hout.div*lo.parental.income.Hout.div, data=data.study1.mobility, na.action=na.omit); summary(income.Hout.entitlement.lo.origins)
#---- Association between class origins and entitlement among people with low and high class destinations
with(data.study1.mobility, describe(cbind(income.Hout.div,z.income.Hout.div,parental.income.Hout.div,z.parental.income.Hout.div)))
############################################################################################################################################
data.study1.mobility$hi.income.Hout.div <- data.study1.mobility$z.income.Hout.div - 1
data.study1.mobility$lo.income.Hout.div <- data.study1.mobility$z.income.Hout.div + .5
income.Hout.entitlement.hi.destinations <- lm(z.entitlement ~ hi.income.Hout.div*z.parental.income.Hout.div, data=data.study1.mobility, na.action=na.omit); summary(income.Hout.entitlement.hi.destinations)
income.Hout.entitlement.lo.destinations <- lm(z.entitlement ~ lo.income.Hout.div*z.parental.income.Hout.div, data=data.study1.mobility, na.action=na.omit); summary(income.Hout.entitlement.lo.destinations)
# Plot
incentitle <- data.frame(effect("z.income.Hout.div*z.parental.income.Hout.div", income.Hout.entitlement, xlevels=list(z.parental.income.Hout.div=c(-.5,1))))
ggplot(incentitle) + geom_line(aes(z.income.Hout.div, fit,  color = z.parental.income.Hout.div, linetype=factor(z.parental.income.Hout.div)), size= 2, )+ scale_colour_gradient(low = "darkred", high = "darkblue") + theme_bw()+
  coord_cartesian(xlim=c(-1,7), ylim=c(-2,2)) +
  scale_linetype_manual(values=c("dotted", "solid"))+
  theme(legend.position='none', panel.grid.major = element_blank(),
        text = element_text(size=20),
        panel.grid.minor = element_blank(),)+ xlab("Current Income")+ylab("Entitlement") 
with (data.study1.mobility, describe(cbind(z.subjective.class.destinations,z.subjective.class.origins,z.income.Hout.div,z.parental.income.Hout.div)))
# two way interaction with controls
income.Hout.entitlement.controls <- lm(z.entitlement ~ z.income.Hout.div*z.parental.income.Hout.div + participant.age + Female + Caucasian, data=data.study1.mobility, na.action=na.omit); summary(income.Hout.entitlement.controls)
# simple slopes
#---- Association between class destinations and entitlement among people with low and high class origins
summary(income.Hout.entitlement.hi.origins.controls <- lm(z.entitlement ~ z.income.Hout.div*hi.parental.income.Hout.div + participant.age + Female + Caucasian, data=data.study1.mobility, na.action=na.omit))
summary(income.Hout.entitlement.lo.origins.controls <- lm(z.entitlement ~ z.income.Hout.div*lo.parental.income.Hout.div + participant.age + Female + Caucasian, data=data.study1.mobility, na.action=na.omit))
#---- Association between class origins and entitlement among people with low and high class destinations
summary(income.Hout.entitlement.hi.destinations.controls <- lm(z.entitlement ~ hi.income.Hout.div*z.parental.income.Hout.div + participant.age + Female + Caucasian, data=data.study1.mobility, na.action=na.omit))
summary(income.Hout.entitlement.lo.destinations.controls <- lm(z.entitlement ~ lo.income.Hout.div*z.parental.income.Hout.div + participant.age + Female + Caucasian, data=data.study1.mobility, na.action=na.omit))
# exploratory two-way interaction no Titanic item
income.Hout.entitlement.notitanic <- lm(z.entitlement.notitanic ~ z.income.Hout.div*z.parental.income.Hout.div, data=data.study1.mobility, na.action=na.omit); summary(income.Hout.entitlement.notitanic)
# exploratory three-way interaction with gender
summary(income.Hout.entitlement.gender <- lm(z.entitlement ~ z.income.Hout.div*z.parental.income.Hout.div*Female, data=data.study1.mobility, na.action=na.omit))
# exploratory three-way interaction with ethnicity
summary(income.Hout.entitlement.ethnicity <- lm(z.entitlement ~ z.income.Hout.div*z.parental.income.Hout.div*Caucasian, data=data.study1.mobility, na.action=na.omit))

#### EDUCATION ####
# two way interaction between class origins, class destinations, and entitlement
with(data.study1.mobility, cor.test(z.education,z.entitlement))
with(data.study1.mobility, cor.test(z.parents.education,z.entitlement))
with(data.study1.mobility, describe(cbind(education,z.education)))
table(data.study1.mobility$education)
table(data.study1.mobility$z.education)
with(data.study1.mobility, describe(cbind(parents.education,z.parents.education)))
table(data.study1.mobility$parents.education)
table(data.study1.mobility$z.parents.education)
with(data.study1.mobility, corr.test(cbind(education,parents.education,entitlement), use="complete", adjust="none"))
education.ols.entitlement.linear <- lm(z.entitlement ~ z.education+z.parents.education, data=data.study1.mobility, na.action=na.omit); summary(education.ols.entitlement.linear)
education.ols.entitlement <- lm(z.entitlement ~ z.education*z.parents.education, data=data.study1.mobility, na.action=na.omit); summary(education.ols.entitlement)
with(data.study1.mobility, describe(cbind(education,z.education,parents.education,z.parents.education)))
# simple slopes ---- Association between class destinations and entitlement among people with low and high class origins
data.study1.mobility$hi.parents.education <- data.study1.mobility$z.parents.education - 1
data.study1.mobility$lo.parents.education <- data.study1.mobility$z.parents.education + 1
education.entitlement.hi.origins <- lm(z.entitlement ~ z.education*hi.parents.education, data=data.study1.mobility, na.action=na.omit); summary(education.entitlement.hi.origins)
education.entitlement.lo.origins <- lm(z.entitlement ~ z.education*lo.parents.education, data=data.study1.mobility, na.action=na.omit); summary(education.entitlement.lo.origins)
# simple slopes ---- Association between class origins and entitlement among people with low and high class destinations
data.study1.mobility$hi.education <- data.study1.mobility$z.education - 1
data.study1.mobility$lo.education <- data.study1.mobility$z.education + 1
education.entitlement.hi.destinations <- lm(z.entitlement ~ hi.education*z.parents.education, data=data.study1.mobility, na.action=na.omit); summary(education.entitlement.hi.destinations)
education.entitlement.lo.destinations <- lm(z.entitlement ~ lo.education*z.parents.education, data=data.study1.mobility, na.action=na.omit); summary(education.entitlement.lo.destinations)
# Plot
eduentitle<- data.frame(effect("z.education*z.parents.education", education.ols.entitlement, xlevels=list(z.parents.education=c(-1,1))))
ggplot(eduentitle) + geom_line(aes(z.education, fit,  color = z.parents.education, linetype=factor(z.parents.education)), size= 1.25, )+ scale_colour_gradient(low = "darkred", high = "darkblue") + theme_bw()+
  coord_cartesian(xlim=c(-2.5,2.5), ylim=c(-2,2)) +
  scale_linetype_manual(values=c("dotted", "solid"))+
  theme(legend.position='none', panel.grid.major = element_blank(),
        text = element_text(size=15),
        panel.grid.minor = element_blank(),)+ xlab("Current Education")+ylab("Entitlement") 
# two way interaction with controls
education.ols.entitlement.controls <- lm(z.entitlement ~ z.education*z.parents.education + participant.age + Female + Caucasian, data=data.study1.mobility, na.action=na.omit); summary(education.ols.entitlement.controls)
# simple slopes ---- Association between class destinations and entitlement among people with low and high class origins
summary(education.entitlement.hi.origins.controls <- lm(z.entitlement ~ z.education*hi.parents.education + participant.age + Female + Caucasian, data=data.study1.mobility, na.action=na.omit))
summary(education.entitlement.lo.origins.controls <- lm(z.entitlement ~ z.education*lo.parents.education + participant.age + Female + Caucasian, data=data.study1.mobility, na.action=na.omit))
# simple slopes ---- Association between class origins and entitlement among people with low and high class destinations
summary(education.entitlement.hi.destinations.controls <- lm(z.entitlement ~ hi.education*z.parents.education + participant.age + Female + Caucasian, data=data.study1.mobility, na.action=na.omit))
summary(education.entitlement.lo.destinations.controls <- lm(z.entitlement ~ lo.education*z.parents.education + participant.age + Female + Caucasian, data=data.study1.mobility, na.action=na.omit))
# exploratory two-way interaction no Titanic item
education.ols.entitlement.notitanic <- lm(z.entitlement.notitanic ~ z.education*z.parents.education, data=data.study1.mobility, na.action=na.omit); summary(education.ols.entitlement.notitanic)
# exploratory three way interaction with gender
summary(education.ols.entitlement.gender <- lm(z.entitlement ~ z.education*z.parents.education*Female, data=data.study1.mobility, na.action=na.omit))
# exploratory three way interaction with ethnicity
summary(education.ols.entitlement.ethnicity <- lm(z.entitlement ~ z.education*z.parents.education*Caucasian, data=data.study1.mobility, na.action=na.omit))

###########################################################################################################################
##################################################### end of analyses #####################################################
###########################################################################################################################
