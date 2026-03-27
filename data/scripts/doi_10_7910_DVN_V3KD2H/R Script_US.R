## --------------------------------------------------
## R script US data analysis
## Survey experiment on meat substitutes
## US survey data
## Created December 2020
## Last modified February 2023
## --------------------------------------------------

######################################################
## Installing Packages
######################################################
install.packages(c("gapminder", "lmtest", "readxl", "nFactors", "psych", "AICcmodavg", "foreign", "corrplot", "sandwich"),  dependencies = TRUE)
install.packages("sparsereg","expm", "estimatr")
install.packages(c("ggplot2", "ggpubr", "tidyverse", "broom.mixed", "ggstance", "jtools"))
install.packages("stargazer")

######################################################
## Loading Packages
######################################################
library(AICcmodavg)
library(lmtest)
library(readxl)
library(gapminder)
library(psych)
library(nFactors)
library(tidyverse)
library(stargazer)
library(sandwich)
library(FSA) #includes Dunn Test

######################################################
## Loading Data
######################################################
dataUS <- read_excel("dataUS_clean.xlsx")

######################################################
## Randomization Tests
######################################################
## --------------------------------------------------
## Balance Checks
## --------------------------------------------------
attach(dataUS)
m.age <- lm(age ~ Group)
summary(m.age)

m.lessthanhighschool <- glm(education == 1 ~ Group, family=binomial())
summary(m.lessthanhighschool)

m.highschool <- glm(education == 2 ~ Group, family=binomial())
summary(m.highschool)

m.college <- glm(education == 3 ~ Group, family=binomial())
summary(m.college)

m.bachelor <- glm(education == 4 ~ Group, family=binomial())
summary(m.bachelor)

m.graduate <- glm(education == 5 ~ Group, family=binomial())
summary(m.graduate)

m.income <- lm(income ~ Group)
summary(m.income)

m.govintervention <- lm(gov_intervention ~ Group)
summary(m.govintervention)

m.polspectrum <- lm(political_spectrum ~ Group)
summary(m.polspectrum)

m.polparty <- lm(polparty ~ Group)
summary(m.polparty)

m.experience <- lm(experience ~ Group)
summary(m.experience)

m.diet <- lm(diet ~ Group)
summary(m.diet)
detach(dataUS)

######################################################
## MLS regressions simple models
######################################################
## --------------------------------------------------
## DV1a: Intention to increase substitute consumption----
## --------------------------------------------------
#lm1: Y = treatments + e
lm1 <- lm(q72 ~ Treatment_ , data = dataUS)
summary(lm1)
coeftest(lm1, vcov=sandwich )
dataUS %>% group_by(Treatment_) %>% summarise_at(vars(q72), list(sd = sd))

#Test homoskedasticity assumption
bptest(lm1) #test shows that if we can reject the null that the variance of the residuals is constant, there is heteroscedasticity
#use robust standard errors
coeftest(lm1, vcov=sandwich )

#lm1a: Y = treatments + experience + e
lm1a <- lm(q72 ~ Treatment_ + experience , data = dataUS)
summary(lm1a)
bptest(lm1a) #test shows that if we can reject the null that the variance of the residuals is constant, there is heteroscedasticity
coeftest(lm1a, vcov=sandwich)
#lm1b: Y = treatments * experience + e
lm1b <- lm(q72 ~ Treatment_*experience , data = dataUS)
summary(lm1b)
bptest(lm1b) #test shows that if we can reject the null that the variance of the residuals is constant, there is heteroscedasticity
coeftest(lm1b, vcov=sandwich)

#Wald test to check model fit when adding variables
waldtest(lm1, lm1a, lm1b, vcov=sandwich)

##Determine impact of treatment assignment
aov1 <- aov(q72 ~ Treatment_, data = dataUS)
summary(aov1)
TukeyHSD(aov1) #determine if there is significant impact of the Treatment_ assignment (control, treatments) 
#Test anova assumptions
# 1. Homogeneity of variances
plot(aov1, 1)
# 2. Normality
plot(aov1, 2)
#extract the residuals
aov1_residuals <- residuals(object = aov1 )
#run Shapiro-Wilk test
shapiro.test(x = aov1_residuals)
#ANOVA assumes homogeneity of variances and normality, if assumptions are not met, Kruskal-Wallis rank sum test as alternative
kruskal1 <- kruskal.test(q72 ~ Treatment_, data = dataUS)
kruskal1
dunnTest(q72 ~ Treatment_, data = dataUS, two.sided=T)
dunnTest(q72 ~ Treatment_, data = dataUS, two.sided=T, method = "bh")

## --------------------------------------------------
## DV1b: Intention to reduce meat----
## --------------------------------------------------
#lm2: Y = treatments + e
lm2 <- lm(q74 ~ Treatment_ , data = dataUS)
summary(lm2)
coeftest(lm2, vcov=sandwich)
dataUS_q74 <- subset(dataUS, q74 !="NA") #Generate subset of data with meat eaters only
dataUS_q74 %>% group_by(Treatment_) %>% summarise_at(vars(q74), list(name = sd))

#lm2a: Y = treatments + experience + e
lm2a <- lm(q74 ~ Treatment_ + experience , data = dataUS)
summary(lm2a)
coeftest(lm2a, vcov=sandwich )
#lm2b: Y = treatments * experience + e
lm2b <- lm(q74 ~ Treatment_*experience , data = dataUS)
summary(lm2b)
coeftest(lm2b, vcov=sandwich )

##Wald test to check model fit when adding variables
waldtest(lm2, lm2a, lm2b, vcov = sandwich )

##Determine impact of treatment assignment
kruskal2 <- kruskal.test(q74 ~ Treatment_, data = dataUS)
kruskal2
dunnTest(q74 ~ Treatment_, data = dataUS, two.sided=T)
dunnTest(q74 ~ Treatment_, data = dataUS, two.sided=T, method = "bh")

## --------------------------------------------------
## DV1c: Willingness to pay for substitutes
## --------------------------------------------------
#lm3: Y = treatments + e
lm3 <- lm(q76_1 ~ Treatment_ , data = dataUS)
summary(lm3)
coeftest(lm3, vcov=sandwich )

#lm3a: Y = treatments + experience + e
lm3a <- lm(q76_1 ~ Treatment_ + experience , data = dataUS)
summary(lm3a)
coeftest(lm3a, vcov=sandwich )
#lm3b: Y = treatments * experience + e
lm3b <- lm(q76_1 ~ Treatment_*experience , data = dataUS)
summary(lm3b)
coeftest(lm3b, vcov=sandwich )

##Wald test to check model fit when adding variables
waldtest(lm3, lm3a, lm3b, vcov=sandwich)

##Determine impact of treatment assignment
kruskal3 <- kruskal.test(q76_1 ~ Treatment_, data = dataUS)
kruskal3
dunnTest(q76_1 ~ Treatment_, data = dataUS, two.sided=T)
dunnTest(q76_1 ~ Treatment_, data = dataUS, two.sided=T, method = "bh")

## --------------------------------------------------
## DV2a: Support of meat reduction policies----
## --------------------------------------------------
#lm4: Y = treatments + e
lm4 <- lm(q78 ~ Treatment_ , data = dataUS)
summary(lm4)
coeftest(lm4, vcov=sandwich )
dataUS %>% group_by(Treatment_) %>% summarise_at(vars(q78), list(name = sd))

#lm4a: Y = treatments + experience + e
lm4a <- lm(q78 ~ Treatment_ + experience , data = dataUS)
summary(lm4a)
coeftest(lm4a, vcov=sandwich )
#lm4b: Y = treatments * experience + e
lm4b <- lm(q78 ~ Treatment_*experience, data = dataUS)
summary(lm4b)
coeftest(lm4b, vcov=sandwich )

##Wald test to check model fit when adding variables
waldtest(lm4, lm4a, lm4b, vcov=sandwich)

##Determine impact of treatment assignment
kruskal4 <- kruskal.test(q78 ~ Treatment_, data = dataUS)
kruskal4
dunnTest(q78 ~ Treatment_, data = dataUS, two.sided=T)
dunnTest(q78 ~ Treatment_, data = dataUS, two.sided=T, method = "bh")

## --------------------------------------------------
## DV2b: Support of policies incentivizing substitute consumption----
## --------------------------------------------------
#lm5: Y = treatments + e
lm5 <- lm(q79 ~ Treatment_ , data = dataUS)
summary(lm5)
coeftest(lm5, vcov=sandwich )
dataUS %>% group_by(Treatment_) %>% summarise_at(vars(q79), list(name = sd))

#lm5a: Y = treatments + experience + e
lm5a <- lm(q79 ~ Treatment_ + experience , data = dataUS)
summary(lm5a)
coeftest(lm5a, vcov=sandwich )
#lm5b: Y = treatments * experience + e
lm5b <- lm(q79 ~ Treatment_*experience, data = dataUS)
summary(lm5b)
coeftest(lm5b, vcov=sandwich )

##Wald test to check model fit when adding variables
waldtest(lm5, lm5a, lm5b, vcov=sandwich)

##Determine impact of treatment assignment
kruskal5 <- kruskal.test(q79 ~ Treatment_, data = dataUS)
kruskal5
dunnTest(q79 ~ Treatment_, data = dataUS, two.sided=T)
dunnTest(q79 ~ Treatment_, data = dataUS, two.sided=T, method = "bh")

## Regression Tables DVs 1a, 1b, 2a, 2b
lm2$rse <-sqrt(diag(vcovHC(lm2, type="HC2")))
lm1$rse <-sqrt(diag(vcovHC(lm1, type="HC2")))
lm4$rse <-sqrt(diag(vcovHC(lm4, type="HC2")))
lm5$rse <-sqrt(diag(vcovHC(lm5, type="HC2")))
stargazer(lm2, lm1, lm4, lm5,
          title = "Main effects US sample (consumption and general policy support intentions)", 
          dep.var.labels = c("DV1b: Intention to reduce meat consumption", "DV1a: Intention to increase substitute consumption", "DV2a: Intention to support meat reduction policies", "DV2b: Intention to support substitute incentivizing policies"),
          covariate.labels = c( "Information Group", "Social Norms Group"),
          se=list(lm2$rse, lm1$rse, lm4$rse, lm5$rse),  
          align= TRUE, header = FALSE, type = "html", no.space = TRUE, out = "US_Main effects_DV1a+b_DV2a+b.html" )

lm2b$rse <-sqrt(diag(vcovHC(lm2b, type="HC2")))
lm1b$rse <-sqrt(diag(vcovHC(lm1b, type="HC2")))
lm4b$rse <-sqrt(diag(vcovHC(lm4b, type="HC2")))
lm5b$rse <-sqrt(diag(vcovHC(lm5b, type="HC2")))
stargazer(lm2b, lm1b, lm4b, lm5b,
          title = "Interaction effects US sample (consumption and general policy support intentions)", 
          dep.var.labels = c("DV1b: Intention to reduce meat consumption", "DV1a: Intention to increase substitute consumption", "DV2a: Intention to support meat reduction policies", "DV2b: Intention to support substitute incentivizing policies"),
          covariate.labels = c( "Information Group", "Social Norms Group" , "experience", "experience*Information Group", "experience*Social Norms"),
          se=list(lm2b$rse, lm1b$rse, lm4b$rse, lm5b$rse),  
          align= TRUE, header = FALSE, type = "html", no.space = TRUE, out = "US_Interaction effects_DV1a+b_DV2a+b.html" )

## --------------------------------------------------
## DV2c: Support of meat reduction policy instruments----
## --------------------------------------------------
##taxes
#lm6: Y = treatments + e
lm6 <- lm(q81_1 ~ Treatment_ , data = dataUS)
summary(lm6)
coeftest(lm6, vcov=sandwich )
#lm6a: Y = treatments + experience + e
lm6a <- lm(q81_1 ~ Treatment_ + experience , data = dataUS)
summary(lm6a)
coeftest(lm6a, vcov=sandwich )
#lm6b: Y = treatments * experience + e
lm6b <- lm(q81_1 ~ Treatment_*experience , data = dataUS)
summary(lm6b)
coeftest(lm6b, vcov=sandwich)
##Wald test to check model fit when adding variables
waldtest(lm6, lm6a, lm6b, vcov=sandwich)
##Determine impact of treatment assignment
kruskal6 <- kruskal.test(q81_1 ~ Treatment_, data = dataUS)
kruskal6
dunnTest(q81_1 ~ Treatment_, data = dataUS, two.sided=T)
dunnTest(q81_1 ~ Treatment_, data = dataUS, two.sided=T,method = "bh")

##two meat-free days
#lm7: Y = treatments + e
lm7 <- lm(q81_2 ~ Treatment_ , data = dataUS)
summary(lm7)
coeftest(lm7, vcov=sandwich )
#lm7a: Y = treatments + experience + e
lm7a_US <- lm(q81_2 ~ Treatment_ + experience , data = dataUS)
summary(lm7a_US)
coeftest(lm7a, vcov=sandwich )
#lm7b: Y = treatments * experience + e
lm7b <- lm(q81_2 ~ Treatment_*experience , data = dataUS)
summary(lm7b)
coeftest(lm7b, vcov=sandwich )
##Wald test to check model fit when adding variables
waldtest(lm7, lm7a, lm7b, vcov = sandwich)
#Determine impact of treatment assignment
kruskal7 <- kruskal.test(q81_2 ~ Treatment_, data = dataUS)
kruskal7
dunnTest(q81_2 ~ Treatment_, data = dataUS, two.sided=T)
dunnTest(q81_2 ~ Treatment_, data = dataUS, two.sided=T, method = "bh")

##elimination of subsidies
#lm8: Y = treatments + e
lm8 <- lm(q81_3 ~ Treatment_ , data = dataUS)
summary(lm8)
coeftest(lm8, vcov=sandwich )
#lm8a: Y = treatments + experience + e
lm8a <- lm(q81_3 ~ Treatment_ + experience , data = dataUS)
summary(lm8a)
coeftest(lm8a, vcov=sandwich )
#lm8b: Y = treatments * experience + e
lm8b <- lm(q81_3 ~ Treatment_*experience , data = dataUS)
summary(lm8b)
coeftest(lm8b, vcov=sandwich )
##Wald test to check model fit when adding variables
waldtest(lm8, lm8a, lm8b, vcov=sandwich)
##Determine impact of treatment assignment
kruskal8 <- kruskal.test(q81_3 ~ Treatment_, data = dataUS)
kruskal8
dunnTest(q81_3 ~ Treatment_, data = dataUS, two.sided=T)
dunnTest(q81_3 ~ Treatment_, data = dataUS, two.sided=T, method = "bh")

#Based on additive index

lm8_index <- lm(q81_index ~ Treatment_ , data = dataUS)
summary(lm8_index)

## Regression tables DV2c
lm6$rse <-sqrt(diag(vcovHC(lm6, type="HC2")))
lm7$rse <-sqrt(diag(vcovHC(lm7, type="HC2")))
lm8$rse <-sqrt(diag(vcovHC(lm8, type="HC2")))
stargazer(lm6, lm7, lm8,
          title = "Main effects US sample (specific meat reduction policy support intentions)", 
          dep.var.labels = c("DV2c-1: Taxes", "DV2c-2: Two meat-free days", "DV2c-3: Elimination of subsidies"),
          covariate.labels = c( "Information Group", "Social Norms Group"),
          se=list(lm6$rse, lm7$rse, lm8$rse),  
          align= TRUE, header = FALSE, type = "html", no.space = TRUE, out = "US_Main effects_DV2c.html" )

## --------------------------------------------------
## DV2d: Support of instruments supporting policies incentivizing substitutes----
## --------------------------------------------------
##taxes
#lm9: Y = treatments + e
lm9 <- lm(q83_1 ~ Treatment_ , data = dataUS)
summary(lm9)
coeftest(lm9, vcov=sandwich )
#lm9a: Y = treatments + experience + e
lm9a <- lm(q83_1 ~ Treatment_ + experience , data = dataUS)
summary(lm9a)
coeftest(lm9a, vcov=sandwich )
#lm9b: Y = treatments * experience + e
lm9b <- lm(q83_1 ~ Treatment_*experience , data = dataUS)
summary(lm9b)
coeftest(lm9b, vcov=sandwich )
##Wald test to check model fit when adding variables
waldtest(lm9, lm9a, lm9b, vcov=sandwich)
##Determine impact of treatment assignment
kruskal9 <- kruskal.test(q83_1 ~ Treatment_, data = dataUS)
kruskal9
dunnTest(q83_1 ~ Treatment_, data = dataUS, two.sided=T)
dunnTest(q83_1 ~ Treatment_, data = dataUS, two.sided=T, method = "bh")

##two substitute days
#lm10: Y = treatments + e
lm10 <- lm(q83_2 ~ Treatment_ , data = dataUS)
summary(lm10)
coeftest(lm10, vcov=sandwich )
#lm10a: Y = treatments + experience + e
lm10a <- lm(q83_2 ~ Treatment_ + experience , data = dataUS)
summary(lm10a)
coeftest(lm10a, vcov=sandwich )
#lm10b: Y = treatments * experience + e
lm10b <- lm(q83_2 ~ Treatment_*experience , data = dataUS)
summary(lm10b)
coeftest(lm10b, vcov=sandwich )
##Wald test to check model fit when adding variables
waldtest(lm10, lm10a, lm10b, vcov=sandwich)
##Determine impact of treatment assignment
kruskal10 <- kruskal.test(q83_2 ~ Treatment_, data = dataUS)
kruskal10
dunnTest(q83_2 ~ Treatment_, data = dataUS, two.sided=T)
dunnTest(q83_2 ~ Treatment_, data = dataUS, two.sided=T, method = "bh")

##introduction of subsidies
#lm11: Y = treatments + e
lm11 <- lm(q83_3 ~ Treatment_ , data = dataUS)
summary(lm11)
coeftest(lm11, vcov=sandwich )
#lm11a: Y = treatments + experience + e
lm11a <- lm(q83_3 ~ Treatment_ + experience , data = dataUS)
summary(lm11a)
coeftest(lm11a, vcov=sandwich )
#lm11b: Y = treatments * experience + e
lm11b <- lm(q83_3 ~ Treatment_*experience , data = dataUS)
summary(lm11b)
coeftest(lm11b, vcov=sandwich )
##Wald test to check model fit when adding variables
waldtest(lm11, lm11a, lm11b, vcov=sandwich)
##Determine impact of treatment assignment
kruskal11 <- kruskal.test(q83_3 ~ Treatment_, data = dataUS)
kruskal11
dunnTest(q83_3 ~ Treatment_, data = dataUS, two.sided=T)
dunnTest(q83_3 ~ Treatment_, data = dataUS, two.sided=T, method = "bh")

#Based on additive index

lm9_index <- lm(q83_index ~ Treatment_ , data = dataUS)
summary(lm9_index)

## Regression table DV2d
lm9$rse <-sqrt(diag(vcovHC(lm9, type="HC2")))
lm10$rse <-sqrt(diag(vcovHC(lm10, type="HC2")))
lm11$rse <-sqrt(diag(vcovHC(lm11, type="HC2")))
stargazer(lm9, lm10, lm11,
          title = "Main effects US sample (specific substitute incentivizing policy support intentions)", 
          dep.var.labels = c("DV2d-1: Taxes", "DV2d-2: Two substitute days", "DV2d-3: Introduction of subsidies"),
          covariate.labels = c( "Information Group", "Social Norms Group"),
          se=list(lm9$rse, lm10$rse, lm11$rse),  
          align= TRUE, header = FALSE, type = "html", no.space = TRUE, out = "US_Main effects_DV2d.html" )

## Regression table DV 2c+d Index
lm9_index$rse <-sqrt(diag(vcovHC(lm9_index, type="HC2")))
lm8_index$rse <-sqrt(diag(vcovHC(lm8_index, type="HC2")))
stargazer(lm9_index, lm8_index, 
          title = "Main effects  US sample (additive specific policy support indexes)", 
          dep.var.labels = c("DV2d_index: Meat reduction policy support index", "DV2c_index: Substitute incentivizing policy support index"),
          covariate.labels = c( "Information Group", "Social Norms Group"),
          se=list(lm9_index$rse, lm8_index$rse),  
          align= TRUE, header = FALSE, type = "html", no.space = TRUE, out = "US_Main effects_DV2d+c_indexes.html" )

## --------------------------------------------------
## DV3a: Intention to offer substitutes to guests
## --------------------------------------------------
##cooking for friends at home
#lm12: Y = treatments + e
lm12 <- lm(q85_1 ~ Treatment_ , data = dataUS)
summary(lm12)
coeftest(lm12, vcov=sandwich )
#lm12a: Y = treatments + experience + e
lm12a <- lm(q85_1 ~ Treatment_ + experience , data = dataUS)
summary(lm12a)
coeftest(lm12a, vcov=sandwich )
#lm12b: Y = treatments * experience + e
lm12b <- lm(q85_1 ~ Treatment_*experience , data = dataUS)
summary(lm12b)
coeftest(lm12b, vcov=sandwich )
##Wald test to check model fit when adding variables
waldtest(lm12, lm12a, lm12b, vcov = sandwich)
##Determine impact of treatment assignment
kruskal12 <- kruskal.test(q85_1 ~ Treatment_, data = dataUS)
kruskal12
dunnTest(q85_1 ~ Treatment_, data = dataUS, two.sided=T)

##cooking for family on a holiday
#lm13: Y = treatments + e
lm13 <- lm(q85_2 ~ Treatment_ , data = dataUS)
summary(lm13)
coeftest(lm13, vcov=sandwich )
#lm13a: Y = treatments + experience + e
lm13a <- lm(q85_2 ~ Treatment_ + experience , data = dataUS)
summary(lm13a)
coeftest(lm13a, vcov=sandwich )
#lm13b: Y = treatments * experience + e
lm13b <- lm(q85_2 ~ Treatment_*experience , data = dataUS)
summary(lm13b)
coeftest(lm13b, vcov=sandwich )
##Wald test to check model fit when adding variables
waldtest(lm13, lm13a, lm13b, vcov = sandwich)
##Determine impact of treatment assignment
kruskal13 <- kruskal.test(q85_2 ~ Treatment_, data = dataUS)
kruskal13
dunnTest(q85_2 ~ Treatment_, data = dataUS, two.sided=T)

##cooking for family on a weekday
#lm14: Y = treatments + e
lm14 <- lm(q85_3 ~ Treatment_ , data = dataUS)
summary(lm14)
coeftest(lm14, vcov=sandwich )
#lm14a: Y = treatments + experience + e
lm14a <- lm(q85_3 ~ Treatment_ + experience , data = dataUS)
summary(lm14a)
coeftest(lm14a, vcov=sandwich )
#lm14b: Y = treatments * experience + e
lm14b <- lm(q85_3 ~ Treatment_*experience , data = dataUS)
summary(lm14b)
coeftest(lm14b, vcov=sandwich )
##Wald test to check model fit when adding variables
waldtest(lm14, lm14a, lm14b, vcov = sandwich)
##Determine impact of treatment assignment
kruskal14 <- kruskal.test(q85_3 ~ Treatment_, data = dataUS)
kruskal14
dunnTest(q85_3 ~ Treatment_, data = dataUS, two.sided=T)

##hosting a party
#lm15: Y = treatments + e
lm15 <- lm(q85_4 ~ Treatment_ , data = dataUS)
summary(lm15)
coeftest(lm15, vcov=sandwich )
#lm15a: Y = treatments + experience + e
lm15a <- lm(q85_4 ~ Treatment_ + experience , data = dataUS)
summary(lm15a)
coeftest(lm15a, vcov=sandwich )
#lm15b: Y = treatments * experience + e
lm15b <- lm(q85_4 ~ Treatment_*experience , data = dataUS)
summary(lm15b)
coeftest(lm15b, vcov=sandwich )
##Wald test to check model fit when adding variables
waldtest(lm15, lm15a, lm15b, vcov = sandwich)
##Determine impact of treatment assignment
kruskal15 <- kruskal.test(q85_4 ~ Treatment_, data = dataUS)
kruskal15
dunnTest(q85_4 ~ Treatment_, data = dataUS, two.sided=T)

##organizing a business meal
#lm16: Y = treatments + e
lm16 <- lm(q85_5 ~ Treatment_ , data = dataUS)
summary(lm16)
coeftest(lm16, vcov=sandwich )
#lm16a: Y = treatments + experience + e
lm16a <- lm(q85_5 ~ Treatment_ + experience , data = dataUS)
summary(lm16a)
coeftest(lm16a, vcov=sandwich )
#lm16b: Y = treatments * experience + e
lm16b <- lm(q85_5 ~ Treatment_*experience , data = dataUS)
summary(lm16b)
coeftest(lm16b, vcov=sandwich )
##Wald test to check model fit when adding variables
waldtest(lm16, lm16a, lm16b, vcov = sandwich)
##Determine impact of treatment assignment
kruskal16 <- kruskal.test(q85_5 ~ Treatment_, data = dataUS)
kruskal16
dunnTest(q85_5 ~ Treatment_, data = dataUS, two.sided=T)

##cooking for vegetarians
#lm17: Y = treatments + e
lm17 <- lm(q85_6 ~ Treatment_ , data = dataUS)
summary(lm17)
coeftest(lm17, vcov=sandwich )
#lm17a: Y = treatments + experience + e
lm17a <- lm(q85_6 ~ Treatment_ + experience , data = dataUS)
summary(lm17a)
coeftest(lm17a, vcov=sandwich )
#lm17b: Y = treatments * experience + e
lm17b <- lm(q85_6 ~ Treatment_*experience , data = dataUS)
summary(lm17b)
coeftest(lm17b, vcov=sandwich )
##Wald test to check model fit when adding variables
waldtest(lm17, lm17a, lm17b, vcov = sandwich)
##Determine impact of treatment assignment
kruskal17 <- kruskal.test(q85_6 ~ Treatment_, data = dataUS)
kruskal17
dunnTest(q85_6 ~ Treatment_, data = dataUS, two.sided=T)

## --------------------------------------------------
## DV3b: Intention to share substitute information 
## --------------------------------------------------
#lm18: Y = treatments + e
lm18 <- lm(q105 ~ Treatment_ , data = dataUS)
summary(lm18)
coeftest(lm18, vcov=sandwich )
#lm18a: Y = treatments + experience + e
lm18a <- lm(q105 ~ Treatment_ + experience , data = dataUS)
summary(lm18a)
coeftest(lm18a, vcov=sandwich )
#lm12b: Y = treatments * experience + e
lm18b <- lm(q105 ~ Treatment_*experience , data = dataUS)
summary(lm18b)
coeftest(lm18b, vcov=sandwich )
##Wald test to check model fit when adding variables
waldtest(lm18, lm18a, lm18b, vcov = sandwich)
##Determine impact of treatment assignment
kruskal18 <- kruskal.test(q105 ~ Treatment_, data = dataUS)
kruskal18
dunnTest(q105 ~ Treatment_, data = dataUS, two.sided=T)

######################################################
## Plot US models----------
######################################################
library(jtools)
library(ggstance)
library(broom.mixed)
library(ggpubr)

US_models_plot = plot_summs(lm2,lm1, lm4,lm5, scale = TRUE,  inner_ci_level = .9,
                            coefs = c("Information\nTreatment" = "Treatment_Information", "Social Norms\nTreatment" = "Treatment_Social Norms"),
                            model.names = c("Intentions to consume\nless meat", "Intentions to consume\nmore meat substitute", "Support for meat\nreduction policy","Support for meat substitute\nincentive policy"))+ xlim(-0.3, 1) +theme(legend.position="bottom",axis.title=element_text(size=12),legend.text = element_text(size = 10), axis.text=element_text(size=12))

ggsave("US_models_plot.jpeg", US_models_plot, height = 6, width = 5*2, dpi = 300)

US_models_index_plot = plot_summs(lm2,lm1, lm9_index,lm8_index, scale = TRUE,  inner_ci_level = .9,
                                  coefs = c("Information\nTreatment" = "Treatment_Information", "Social Norms\nTreatment" = "Treatment_Social Norms"),
                                  model.names = c("Intentions to consume\nless meat", "Intentions to consume\nmore meat substitute", "Support for meat\nreduction policy","Support for meat substitute\nincentive policy"))+ xlim(-0.3, 1)+theme(legend.position="bottom", axis.title=element_text(size=12),legend.text = element_text(size = 10), axis.text=element_text(size=12))
ggsave("US_models_index_plot.jpeg", US_models_index_plot, height = 6, width = 5*2, dpi = 300)



US_models_plot_controls = plot_summs(lm2_ctrl, lm1_ctrl, lm4_ctrl, lm5_ctrl, scale = TRUE,  inner_ci_level = .9,robust = TRUE, 
                                     coefs = c("Information Treatment"="Treatment_Information", "Social Norms Treatment"="Treatment_Social Norms", "experience"="experience", "cooking experience"="cookingexp", "restaurant experience"= "restaurantexp", "government intervention" = "gov_intervention"),
                                     model.names = c("Intentions to consume\nless meat", "Intentions to consume\nmore meat substitute", "Support for meat\nreduction policy","Support for meat substitute\nincentive policy"))+ xlim(-1, 1)+theme(legend.position="bottom", axis.title=element_text(size=12),legend.text = element_text(size = 10), axis.text=element_text(size=12))

ggsave("US_models_plot_controls.jpeg", US_models_plot_controls, height = 6, width = 5*2, dpi = 300)



install.packages("interactions")
library(interactions)

US_models_plot_interact_policy_incentive = interact_plot(lm4b, pred = experience , modx = Treatment_, interval = TRUE, modx.labels = c("Control", "Information Treatment", "Social Norms Treatment"), legend.main = c("Treatment"),
                                                         int.type = "confidence", int.width = .8)+theme(legend.position="bottom", axis.title=element_text(size=12),legend.text = element_text(size = 10), axis.text=element_text(size=12))

US_models_plot_interact_policy_incentive= US_models_plot_interact_policy_incentive + 
  xlab("Meat Substitute Experience") + ylab("Support for Meat Substitute Incentive Policy")

ggsave("US_models_plot_interact_policy_incentive.jpeg", US_models_plot_interact_policy_incentive, height = 6, width = 5*2, dpi = 300)

summary(lm5b)

US_models_plot_interact_policy_reduction = interact_plot(lm5b, pred = experience , modx = Treatment_, interval = TRUE, modx.labels = c("Control Group", "Information Treatment", "Social Norms Treatment"), legend.main = c("Treatment"),
                                                         int.type = "confidence", int.width = .8)+theme(legend.position="bottom", axis.title=element_text(size=12),legend.text = element_text(size = 10), axis.text=element_text(size=12))

US_models_plot_interact_policy_reduction= US_models_plot_interact_policy_reduction + 
  xlab("Meat Substitute Experience") + ylab("Support for Meat Reduction Policy")

ggsave("US_models_plot_interact_policy_reduction.jpeg", US_models_plot_interact_policy_reduction, height = 6, width = 5*2, dpi = 300)


######################################################
## Linear models with control variables---------
######################################################
## --------------------------------------------------
## DV1a: Intention to buy substitutes-------
## --------------------------------------------------
#lm1_crtl: Y = treatments + experience + additional variables + e
lm1_ctrl <- lm(q72 ~ Treatment_ + experience + ability + cookingexp + restaurantexp + gov_intervention +esc +education +age +gender+region+income, data = dataUS)
summary(lm1_ctrl)
coeftest(lm1_ctrl, vcov=sandwich )
#lm1_crtl: Y = treatments*experience + additional variables + e
lm1a_ctrl <- lm(q72 ~ Treatment_*experience + ability + cookingexp + restaurantexp + gov_intervention +esc +education +age +gender+region+income, data = dataUS)
summary(lm1a_ctrl)
coeftest(lm1a_ctrl, vcov=sandwich )

#Wald test to check model fit when adding variables
waldtest(lm1, lm1_ctrl, vcov=sandwich)

## --------------------------------------------------
## DV1b: Intention to reduce meat-------
## --------------------------------------------------
#lm2_crtl: Y = treatments + experience + additional variables + e
lm2_ctrl <- lm(q74 ~ Treatment_ + experience + ability + cookingexp + restaurantexp + gov_intervention +esc +education +age +gender+region+income, data = dataUS)
summary(lm2_ctrl)
coeftest(lm2_ctrl, vcov=sandwich )
#lm2_crtl: Y = treatments*experience + additional variables + e
lm2a_ctrl <- lm(q74 ~ Treatment_*experience + ability + cookingexp + restaurantexp + gov_intervention +esc +education +age +gender+region+income, data = dataUS)
summary(lm2a_ctrl)
coeftest(lm2a_ctrl, vcov=sandwich )

##Wald test to check model fit when adding variables
waldtest(lm2, lm2_ctrl, vcov = sandwich )

## --------------------------------------------------
## DV1c: Willingness to pay for substitutes----------
## --------------------------------------------------
#lm3_crtl: Y = treatments + experience + additional variables + e
lm3_ctrl <- lm(q76_1 ~ Treatment_ + experience + ability + cookingexp + restaurantexp + gov_intervention +esc +education +age +gender+region+income, data = dataUS)
summary(lm3_ctrl)
coeftest(lm3_ctrl, vcov=sandwich )
#lm3_crtl: Y = treatments*experience + additional variables + e
lm3a_ctrl <- lm(q76_1 ~ Treatment_*experience + ability + cookingexp + restaurantexp + gov_intervention +esc +education +age +gender+region+income, data = dataUS)
summary(lm3a_ctrl)
coeftest(lm3a_ctrl, vcov=sandwich )

##Wald test to check model fit when adding variables
waldtest(lm3, lm3_ctrl, vcov=sandwich)

## --------------------------------------------------
## DV2a: Support of meat reduction policies---------
## --------------------------------------------------
#lm4_crtl: Y = treatments + experience + additional variables + e
lm4_ctrl <- lm(q78 ~ Treatment_ + experience + ability + cookingexp + restaurantexp + diet + gov_intervention +esc +education +age +gender+region+income, data = dataUS)
summary(lm4_ctrl)
coeftest(lm4_ctrl, vcov=sandwich )
#lm4_crtl: Y = treatments*experience + additional variables + e
lm4a_ctrl <- lm(q78 ~ Treatment_*experience + ability + cookingexp + restaurantexp + diet + gov_intervention +esc +education +age +gender+region+income, data = dataUS)
summary(lm4a_ctrl)
coeftest(lm4a_ctrl, vcov=sandwich )

##Wald test to check model fit when adding variables
waldtest(lm4, lm4_ctrl, vcov=sandwich)

## --------------------------------------------------
## DV2b: Support of policies incentivizing substitute consumption---------
## --------------------------------------------------
#lm5_crtl: Y = treatments + experience + additional variables + e
lm5_ctrl <- lm(q79 ~ Treatment_ + experience + ability + cookingexp + restaurantexp + diet + gov_intervention +esc +education +age +gender+region+income, data = dataUS)
summary(lm5_ctrl)
coeftest(lm5_ctrl, vcov=sandwich )
#lm5_crtl: Y = treatments*experience + additional variables + e
lm5a_ctrl <- lm(q79 ~ Treatment_*experience + ability + cookingexp + restaurantexp + diet + gov_intervention +esc +education +age +gender+region+income, data = dataUS)
summary(lm5a_ctrl)
coeftest(lm5a_ctrl, vcov=sandwich )

##Wald test to check model fit when adding variables
waldtest(lm5, lm5_ctrl, vcov=sandwich)

## Regression Tables DVs 1a, 1b, 2a, 2b
lm2a_ctrl$rse <-sqrt(diag(vcovHC(lm2a_ctrl, type="HC2")))
lm1a_ctrl$rse <-sqrt(diag(vcovHC(lm1a_ctrl, type="HC2")))
lm4a_ctrl$rse <-sqrt(diag(vcovHC(lm4a_ctrl, type="HC2")))
lm5a_ctrl$rse <-sqrt(diag(vcovHC(lm5a_ctrl, type="HC2")))
stargazer(lm2a_ctrl, lm1a_ctrl, lm4a_ctrl, lm5a_ctrl,
          title = "Interaction Effects with controls US sample (consumption and general policy support intentions)", 
          dep.var.labels = c("DV1b: Intention to reduce meat consumption", "DV1a: Intention to increase substitute consumption", "DV2a: Intention to support meat reduction policies", "DV2b: Intention to support substitute incentivizing policies"),
          covariate.labels = c( "Information Group", "Social Norms Group", "experience", "ability", "cooking exp", "restaurant exp", "government intervention", "esc", "education", "age", "gender", "region", "income", "experience*Information Group", "experience*Social Norms Group"),
          se=list(lm2a_ctrl$rse, lm1a_ctrl$rse, lm4a_ctrl$rse, lm5a_ctrl$rse), 
          align= TRUE, header = FALSE, type = "html", no.space = TRUE, out = "US_Robustness_Interaction_DV1a+b_DV2a+b.html" )

## --------------------------------------------------
## DV2c: Support of meat reduction policy instruments--------
## --------------------------------------------------
##taxes
#lm6_crtl: Y = treatments + experience + additional variables + e
lm6_ctrl <- lm(q81_1 ~ Treatment_ + experience + ability + cookingexp + restaurantexp + diet + gov_intervention +esc +education +age +gender+region+income, data = dataUS)
summary(lm6_ctrl)
coeftest(lm6_ctrl, vcov=sandwich )
#lm6_crtl: Y = treatments*experience + additional variables + e
lm6a_ctrl <- lm(q81_1 ~ Treatment_*experience + ability + cookingexp + restaurantexp + diet + gov_intervention +esc +education +age +gender+region+income, data = dataUS)
summary(lm6a_ctrl)
coeftest(lm6a_ctrl, vcov=sandwich )

##Wald test to check model fit when adding variables
waldtest(lm6, lm6_ctrl, vcov=sandwich)

##two meat-free days
#lm7_crtl: Y = treatments + experience + additional variables + e
lm7_ctrl <- lm(q81_2 ~ Treatment_ + experience + ability + cookingexp + restaurantexp + diet + gov_intervention +esc +education +age +gender+region+income, data = dataUS)
summary(lm7_ctrl)
coeftest(lm7_ctrl, vcov=sandwich )
#lm7_crtl: Y = treatments*experience + additional variables + e
lm7a_ctrl <- lm(q81_2 ~ Treatment_*experience + ability + cookingexp + restaurantexp + diet + gov_intervention +esc +education +age +gender+region+income, data = dataUS)
summary(lm7a_ctrl)
coeftest(lm7a_ctrl, vcov=sandwich )

##Wald test to check model fit when adding variables
waldtest(lm7, lm7_ctrl, vcov = sandwich)

##elimination of subsidies
#lm8_crtl: Y = treatments + experience + additional variables + e
lm8_ctrl <- lm(q81_3 ~ Treatment_ + experience + ability + cookingexp + restaurantexp + diet + gov_intervention +esc +education +age +gender+region+income, data = dataUS)
summary(lm8_ctrl)
coeftest(lm8_ctrl, vcov=sandwich )
#lm8_crtl: Y = treatments*experience + additional variables + e
lm8a_ctrl <- lm(q81_3 ~ Treatment_*experience + ability + cookingexp + restaurantexp + diet + gov_intervention +esc +education +age +gender+region+income, data = dataUS)
summary(lm8a_ctrl)
coeftest(lm8a_ctrl, vcov=sandwich )

##Wald test to check model fit when adding variables
waldtest(lm8, lm8_ctrl, vcov=sandwich)

## --------------------------------------------------
## DV2d: Support of instruments supporting policies incentivizing substitutes----
## --------------------------------------------------
##taxes
#lm9_crtl: Y = treatments + experience + additional variables + e
lm9_ctrl <- lm(q83_1 ~ Treatment_ + experience + ability + cookingexp + restaurantexp + diet + gov_intervention +esc +education +age +gender+region+income, data = dataUS)
summary(lm9_ctrl)
coeftest(lm9_ctrl, vcov=sandwich )
#lm9_crtl: Y = treatments*experience + additional variables + e
lm9a_ctrl <- lm(q83_1 ~ Treatment_*experience + ability + cookingexp + restaurantexp + diet + gov_intervention +esc +education +age +gender+region+income, data = dataUS)
summary(lm9a_ctrl)
coeftest(lm9a_ctrl, vcov=sandwich )

##Wald test to check model fit when adding variables
waldtest(lm9, lm9_ctrl, vcov=sandwich)

##two substitute days
#lm10_crtl: Y = treatments + experience + additional variables + e
lm10_ctrl <- lm(q83_2 ~ Treatment_ + experience + ability + cookingexp + restaurantexp + diet + gov_intervention +esc +education +age +gender+region+income, data = dataUS)
summary(lm10_ctrl)
coeftest(lm10_ctrl, vcov=sandwich )
#lm10a_crtl: Y = treatments*experience + additional variables + e
lm10a_ctrl <- lm(q83_2 ~ Treatment_*experience + ability + cookingexp + restaurantexp + diet + gov_intervention +esc +education +age +gender+region+income, data = dataUS)
summary(lm10a_ctrl)
coeftest(lm10a_ctrl, vcov=sandwich )

##Wald test to check model fit when adding variables
waldtest(lm10, lm10_ctrl, vcov=sandwich)

##introduction of subsidies
#lm11_crtl: Y = treatments + experience + additional variables + e
lm11_ctrl <- lm(q83_3 ~ Treatment_ + experience + ability + cookingexp + restaurantexp + diet + gov_intervention +esc +education +age +gender+region+income, data = dataUS)
summary(lm11_ctrl)
coeftest(lm11_ctrl, vcov=sandwich)
#lm11a_crtl: Y = treatments*experience + additional variables + e
lm11a_ctrl <- lm(q83_3 ~ Treatment_*experience + ability + cookingexp + restaurantexp + diet + gov_intervention +esc +education +age +gender+region+income, data = dataUS)
summary(lm11a_ctrl)
coeftest(lm11a_ctrl, vcov=sandwich )

##Wald test to check model fit when adding variables
waldtest(lm11, lm11_ctrl, vcov=sandwich)

## --------------------------------------------------
## DV3a: Intention to offer substitutes to guests in different situations
## --------------------------------------------------
##cooking for friends at home
#lm12_crtl: Y = treatments + experience + additional variables + e
lm12_ctrl <- lm(q85_1 ~ Treatment_ + experience + ability + cookingexp + restaurantexp + diet + socialized_family + socialized_friends + socialized_coworkers + gov_intervention +esc +education +age +gender+region+income + age, data = dataUS)
summary(lm12_ctrl)
coeftest(lm12_ctrl, vcov=sandwich)
#lm12a_crtl: Y = treatments*experience + additional variables + e
lm12a_ctrl <- lm(q85_1 ~ Treatment_*experience + ability + cookingexp + restaurantexp + diet + socialized_family + socialized_friends + socialized_coworkers + gov_intervention +esc +education +age +gender+region+income + age, data = dataUS)
summary(lm12a_ctrl)
coeftest(lm12a_ctrl, vcov=sandwich )

##Wald test to check model fit when adding variables
waldtest(lm12, lm12_ctrl, vcov = sandwich)

##cooking for family on a holiday
#lm13_crtl: Y = treatments + experience + additional variables + e
lm13_ctrl <- lm(q85_2 ~ Treatment_ + experience + ability + cookingexp + restaurantexp + diet + socialized_family + socialized_friends + socialized_coworkers +gov_intervention +esc +education +age +gender+region+income + age, data = dataUS)
summary(lm13_ctrl)
coeftest(lm13_ctrl, vcov=sandwich)
#lm13a_crtl: Y = treatments*experience + additional variables + e
lm13a_ctrl <- lm(q85_2 ~ Treatment_*experience + ability + cookingexp + restaurantexp + diet + socialized_family + socialized_friends + socialized_coworkers  + gov_intervention +esc +education +age +gender+region+income + age, data = dataUS)
summary(lm13a_ctrl)
coeftest(lm13a_ctrl, vcov=sandwich )

##Wald test to check model fit when adding variables
waldtest(lm13, lm13_ctrl, vcov = sandwich)
waldtest(lm13a, lm13a_ctrl, vcov = sandwich)

##cooking for family on a weekday
#lm14_crtl: Y = treatments + experience + additional variables + e
lm14_ctrl <- lm(q85_3 ~ Treatment_ + experience + ability + cookingexp + restaurantexp + diet + socialized_family + socialized_friends + socialized_coworkers +gov_intervention +esc +education +age +gender+region+income + age, data = dataUS)
summary(lm14_ctrl)
coeftest(lm14_ctrl, vcov=sandwich)
#lm14a_crtl: Y = treatments*experience + additional variables + e
lm14a_ctrl <- lm(q85_3 ~ Treatment_*experience + ability + cookingexp + restaurantexp + diet + socialized_family + socialized_friends + socialized_coworkers  + gov_intervention +esc +education +age +gender+region+income + age, data = dataUS)
summary(lm14a_ctrl)
coeftest(lm14a_ctrl, vcov=sandwich )

##Wald test to check model fit when adding variables
waldtest(lm14, lm14_ctrl, vcov = sandwich)

##hosting a party
#lm15_crtl: Y = treatments + experience + additional variables + e
lm15_ctrl <- lm(q85_4 ~ Treatment_ + experience + ability + cookingexp + restaurantexp + diet + socialized_family + socialized_friends + socialized_coworkers +gov_intervention +esc +education +age +gender+region+income + age, data = dataUS)
summary(lm15_ctrl)
coeftest(lm15_ctrl, vcov=sandwich)
#lm15a_crtl: Y = treatments*experience + additional variables + e
lm15a_ctrl <- lm(q85_4 ~ Treatment_*experience + ability + cookingexp + restaurantexp + diet + socialized_family + socialized_friends + socialized_coworkers  + gov_intervention +esc +education +age +gender+region+income + age, data = dataUS)
summary(lm15a_ctrl)
coeftest(lm15a_ctrl, vcov=sandwich )

##Wald test to check model fit when adding variables
waldtest(lm15, lm15_ctrl, vcov = sandwich)

##organizing a business meal
#lm16_crtl: Y = treatments + experience + additional variables + e
lm16_ctrl <- lm(q85_5 ~ Treatment_ + experience + ability + cookingexp + restaurantexp + diet + socialized_family + socialized_friends + socialized_coworkers +gov_intervention +esc +education +age +gender+region+income + age, data = dataUS)
summary(lm16_ctrl)
coeftest(lm16_ctrl, vcov=sandwich)
#lm16a_crtl: Y = treatments*experience + additional variables + e
lm16a_ctrl <- lm(q85_5 ~ Treatment_*experience + ability + cookingexp + restaurantexp + diet + socialized_family + socialized_friends + socialized_coworkers  + gov_intervention +esc +education +age +gender+region+income + age, data = dataUS)
summary(lm16a_ctrl)
coeftest(lm16a_ctrl, vcov=sandwich )

##Wald test to check model fit when adding variables
waldtest(lm16, lm16_ctrl, vcov = sandwich)
waldtest(lm16a, lm16a_ctrl, vcov = sandwich)

##cooking for vegetarians
#lm17_crtl: Y = treatments + experience + additional variables + e
lm17_ctrl <- lm(q85_6 ~ Treatment_ + experience + ability + cookingexp + restaurantexp + diet + socialized_family + socialized_friends + socialized_coworkers +gov_intervention +esc +education +age +gender+region+income + age, data = dataUS)
summary(lm17_ctrl)
coeftest(lm17_ctrl, vcov=sandwich)
#lm17a_crtl: Y = treatments*experience + additional variables + e
lm17a_ctrl <- lm(q85_6 ~ Treatment_*experience + ability + cookingexp + restaurantexp + diet + socialized_family + socialized_friends + socialized_coworkers  + gov_intervention +esc +education +age +gender+region+income + age, data = dataUS)
summary(lm17a_ctrl)
coeftest(lm17a_ctrl, vcov=sandwich )

##Wald test to check model fit when adding variables
waldtest(lm17, lm17_ctrl, vcov = sandwich)
waldtest(lm17a, lm17a_ctrl, vcov = sandwich)

## --------------------------------------------------
## DV3b: Intention to share substitute information 
## --------------------------------------------------
#lm18_crtl: Y = treatments + experience + additional variables + e
lm18_ctrl <- lm(q105 ~ Treatment_ + experience +gov_intervention +esc +education +age +gender+region+income, data = dataUS)
summary(lm18_ctrl)
coeftest(lm18_ctrl, vcov=sandwich)
#lm18a_crtl: Y = treatments*experience + additional variables + e
lm18a_ctrl <- lm(q105 ~ Treatment_*experience + gov_intervention +esc +education +age +gender+region+income, data = dataUS)
summary(lm18a_ctrl)
coeftest(lm18a_ctrl, vcov=sandwich )

##Wald test to check model fit when adding variables
waldtest(lm18, lm18_ctrl, vcov = sandwich)

######################################################
## Correlation Plots----
######################################################
detach(dataUS)
##get overview of relationships between variables 
library(corrplot)

##generate dataset only with those that answered the shopping criteria matrix
dataUS_esc <- subset(dataUS, esc !="NA")
##generate datasets with and without esc only with those that answered meat reduction question (meat eaters only)
dataUS_q74 <- subset(dataUS, q74 !="NA")
dataUS_esc_q74 <- subset(dataUS, q74 !="NA"& esc != "NA")

##create correlation plot including substitute eating intentions, experience and additional variables
dataUS_removedcolumns1 <- subset(dataUS, select = c(q72, experience, diet, cookingexp, restaurantexp, fns, socialized_family, socialized_friends, ability, availability, mfs, shoppinglist, age, education, gender, gov_intervention, political_spectrum))
dataUS_cor1 <- cor(dataUS_removedcolumns1)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(dataUS_removedcolumns1) #matrix of the p-value of the correlations
corrplot(dataUS_cor1 , method="color", type="upper",
         # Add coefficient of correlation
         addCoef.col = "black",
         #Text label color and rotation
         tl.col="black", tl.srt=45, cl.align="r",
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05, insig = "blank",
         # hide correlation coefficient on the principal diagonal
         diag=FALSE
)
##create correlation plot including substitute eating intentions, experience, esc and additional variables
dataUS_removedcolumns1a <- subset(dataUS_esc, select = c(q72, experience, diet, esc, cookingexp, restaurantexp, fns, socialized_family, socialized_friends, ability, availability, mfs, shoppinglist, age, education, gender, gov_intervention, political_spectrum))
dataUS_cor1a <- cor(dataUS_removedcolumns1a)
p.mat <- cor.mtest(dataUS_removedcolumns1a) #matrix of the p-value of the correlations
corrplot(dataUS_cor1a , method="color", type="upper",
         # Add coefficient of correlation
         addCoef.col = "black",
         #Text label color and rotation
         tl.col="black", tl.srt=45, cl.align="r",
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05, insig = "blank",
         # hide correlation coefficient on the principal diagonal
         diag=FALSE
)

##create correlation plot including meat reduction intentions, experience and additional variables
dataUS_removedcolumns2 <- subset(dataUS_q74, select = c(q74, experience, diet, cookingexp, restaurantexp, fns, socialized_family, socialized_friends, ability, availability, mfs , shoppinglist, age, education, gender, gov_intervention, political_spectrum))
dataUS_cor2 <- cor(dataUS_removedcolumns2)
p.mat <- cor.mtest(dataUS_removedcolumns2) #matrix of the p-value of the correlations
corrplot(dataUS_cor2 , method="color", type="upper",
         # Add coefficient of correlation
         addCoef.col = "black",
         #Text label color and rotation
         tl.col="black", tl.srt=45, cl.align="r",
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05, insig = "blank",
         # hide correlation coefficient on the principal diagonal
         diag=FALSE
)

##create correlation plot incl. willingness to pay for substitutes, experience and additional variables
dataUS_removedcolumns5 <- subset(dataUS, select = c(q76_1, experience, diet, cookingexp, restaurantexp, fns, socialized_family, socialized_friends, ability, availability, mfs, shoppinglist, age, education, gender, gov_intervention, political_spectrum))
dataUS_cor5 <- cor(dataUS_removedcolumns5)
p.mat <- cor.mtest(dataUS_removedcolumns5) #matrix of the p-value of the correlations
corrplot(dataUS_cor5 , method="color", type="upper",
         # Add coefficient of correlation
         addCoef.col = "black",
         #Text label color and rotation
         tl.col="black", tl.srt=45, cl.align="r",
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05, insig = "blank",
         # hide correlation coefficient on the principal diagonal
         diag=FALSE
)

##create correlation plot including meat reduction policy support intentions, experience and additional variables
dataUS_removedcolumns3 <- subset(dataUS, select = c(q78, experience, diet, cookingexp, restaurantexp, fns, socialized_family, socialized_friends, ability, availability, mfs,shoppinglist, age, education, gender, gov_intervention, political_spectrum) )
dataUS_cor3 <- cor(dataUS_removedcolumns3)
p.mat <- cor.mtest(dataUS_removedcolumns3) #matrix of the p-value of the correlations
corrplot(dataUS_cor3 , method="color", type="upper",
         # Add coefficient of correlation
         addCoef.col = "black",
         #Text label color and rotation
         tl.col="black", tl.srt=45, cl.align="r",
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05, insig = "blank",
         # hide correlation coefficient on the principal diagonal
         diag=FALSE
)


##create correlation plot including support of policies incentivizing substitutes, experience and additional variables
dataUS_removedcolumns4 <- subset(dataUS, select = c(q79, experience, diet, cookingexp, restaurantexp, fns, socialized_family, socialized_friends, ability, availability, mfs, shoppinglist, age, education, gender, gov_intervention, political_spectrum) )
dataUS_cor4 <- cor(dataUS_removedcolumns4)
p.mat <- cor.mtest(dataUS_removedcolumns4) #matrix of the p-value of the correlations
corrplot(dataUS_cor4 , method="color", type="upper",
         # Add coefficient of correlation
         addCoef.col = "black",
         #Text label color and rotation
         tl.col="black", tl.srt=45, cl.align="r",
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05, insig = "blank",
         # hide correlation coefficient on the principal diagonal
         diag=FALSE
)


##create correlation plot including intentions to cook substitutes for friends at home, experience and additional variables
dataUS_removedcolumns6 <- subset(dataUS, select = c(q85_1, experience, diet, cookingexp, restaurantexp, fns, socialized_family, socialized_friends, socialized_coworkers, socialized_other, ability, availability, mfs, shoppinglist, age, education, gender) )
dataUS_cor6 <- cor(dataUS_removedcolumns6)
p.mat <- cor.mtest(dataUS_removedcolumns6) #matrix of the p-value of the correlations
corrplot(dataUS_cor6 , method="color", type="upper",
         # Add coefficient of correlation
         addCoef.col = "black",
         #Text label color and rotation
         tl.col="black", tl.srt=45, cl.align="r",
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05, insig = "blank",
         # hide correlation coefficient on the principal diagonal
         diag=FALSE
)

##create correlation plot including intentions to cook substitutes for family on a holiday, experience and additional variables
dataUS_removedcolumns7 <- subset(dataUS, select = c(q85_2, experience, diet, cookingexp, restaurantexp, fns, socialized_family, socialized_friends, socialized_coworkers, socialized_other, ability, availability, mfs, shoppinglist, age, education, gender) )
dataUS_cor7 <- cor(dataUS_removedcolumns7)
p.mat <- cor.mtest(dataUS_removedcolumns7) #matrix of the p-value of the correlations
corrplot(dataUS_cor7 , method="color", type="upper",
         # Add coefficient of correlation
         addCoef.col = "black",
         #Text label color and rotation
         tl.col="black", tl.srt=45, cl.align="r",
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05, insig = "blank",
         # hide correlation coefficient on the principal diagonal
         diag=FALSE
)

##create correlation plot including intentions to cook substitutes for family on a weekday, experience and additional variables
dataUS_removedcolumns8 <- subset(dataUS, select = c(q85_3, experience, diet, cookingexp, restaurantexp, fns, socialized_family, socialized_friends, socialized_coworkers, socialized_other,ability, availability, mfs, shoppinglist, age, education, gender) )
dataUS_cor8 <- cor(dataUS_removedcolumns8)
p.mat <- cor.mtest(dataUS_removedcolumns8) #matrix of the p-value of the correlations
corrplot(dataUS_cor8 , method="color", type="upper",
         # Add coefficient of correlation
         addCoef.col = "black",
         #Text label color and rotation
         tl.col="black", tl.srt=45, cl.align="r",
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05, insig = "blank",
         # hide correlation coefficient on the principal diagonal
         diag=FALSE
)

##create correlation plot including intentions to offer substitutes when hosting a party, experience and additional variables
dataUS_removedcolumns9 <- subset(dataUS, select = c(q85_4, experience, diet, cookingexp, restaurantexp, fns, socialized_family, socialized_friends, socialized_coworkers, socialized_other, ability, availability, mfs, shoppinglist, age, education, gender) )
dataUS_cor9 <- cor(dataUS_removedcolumns9)
p.mat <- cor.mtest(dataUS_removedcolumns9) #matrix of the p-value of the correlations
corrplot(dataUS_cor9 , method="color", type="upper",
         # Add coefficient of correlation
         addCoef.col = "black",
         #Text label color and rotation
         tl.col="black", tl.srt=45, cl.align="r",
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05, insig = "blank",
         # hide correlation coefficient on the principal diagonal
         diag=FALSE
)

##create correlation plot intentions to offer substitutes when organizing a business meal, experience and additional variables
dataUS_removedcolumns10 <- subset(dataUS, select = c(q85_5, experience, diet, cookingexp, restaurantexp, fns, socialized_family, socialized_friends, socialized_coworkers, socialized_other, ability, availability, mfs, shoppinglist, age, education, gender) )
dataUS_cor10 <- cor(dataUS_removedcolumns10)
p.mat <- cor.mtest(dataUS_removedcolumns10) #matrix of the p-value of the correlations
corrplot(dataUS_cor10 , method="color", type="upper",
         # Add coefficient of correlation
         addCoef.col = "black",
         #Text label color and rotation
         tl.col="black", tl.srt=45, cl.align="r",
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05, insig = "blank",
         # hide correlation coefficient on the principal diagonal
         diag=FALSE
)

##create correlation plot intentions to offer substitutes when cooking for vegetarians, experience and additional variables
dataUS_removedcolumns11 <- subset(dataUS, select = c(q85_6, experience, diet, cookingexp, restaurantexp, fns, socialized_family, socialized_friends, socialized_coworkers, socialized_other, ability, availability, mfs, shoppinglist, age, education, gender) )
dataUS_cor11 <- cor(dataUS_removedcolumns11)
p.mat <- cor.mtest(dataUS_removedcolumns11) #matrix of the p-value of the correlations
corrplot(dataUS_cor11 , method="color", type="upper",
         # Add coefficient of correlation
         addCoef.col = "black",
         #Text label color and rotation
         tl.col="black", tl.srt=45, cl.align="r",
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05, insig = "blank",
         # hide correlation coefficient on the principal diagonal
         diag=FALSE
)

######################################################
## Peparing variables for LassoPlus
######################################################
normFunc <- function(x){(x-mean(x, na.rm = T))/sd(x, na.rm = T)}

##Prepare full sample for LassoPlus
dataUS$Group_f  <- plyr::mapvalues(dataUS$Group, c("1", "2", "3"), c("Control","Information", "Social Norms"))
dataUS$age_z <- normFunc(dataUS$age)
dataUS$education_z <- normFunc(dataUS$education)
dataUS$gender_z <- normFunc(dataUS$gender)
dataUS$diet_z <- normFunc(dataUS$diet)
dataUS$experience_z <- normFunc(dataUS$experience)
dataUS$cookingexp_z <- normFunc(dataUS$cookingexp)
dataUS$restaurantexp_z <- normFunc(dataUS$restaurantexp)
dataUS$fns_z <- normFunc(dataUS$fns)
dataUS$socialized_family_z <- normFunc(dataUS$socialized_family)
dataUS$socialized_friends_z <- normFunc(dataUS$socialized_friends)
dataUS$socialized_coworkers_z <- normFunc(dataUS$socialized_coworkers)
dataUS$socialized_other_z <- normFunc(dataUS$socialized_other)
dataUS$household_z <- normFunc(dataUS$household)
dataUS$ability_z <- normFunc(dataUS$ability)
dataUS$availability_z <- normFunc(dataUS$availability)
dataUS$shoppingoutlet_z <- normFunc(dataUS$shoppingoutlet)
dataUS$mfs_z <- normFunc(dataUS$mfs)
dataUS$shoppinglist_z <- normFunc(dataUS$shoppinglist)
dataUS$gov_intervention_z <- normFunc(dataUS$gov_intervention)
dataUS$political_spectrum_z <- normFunc(dataUS$political_spectrum)

##Prepare full sample only meat eaters for LassoPlus
dataUS_q74$Group_f  <- plyr::mapvalues(dataUS_q74$Group, c("1", "2", "3"), c("Control","Information", "Social Norms"))
dataUS_q74$age_z <- normFunc(dataUS_q74$age)
dataUS_q74$education_z <- normFunc(dataUS_q74$education)
dataUS_q74$gender_z <- normFunc(dataUS_q74$gender)
dataUS_q74$diet_z <- normFunc(dataUS_q74$diet)
dataUS_q74$experience_z <- normFunc(dataUS_q74$experience)
dataUS_q74$cookingexp_z <- normFunc(dataUS_q74$cookingexp)
dataUS_q74$restaurantexp_z <- normFunc(dataUS_q74$restaurantexp)
dataUS_q74$fns_z <- normFunc(dataUS_q74$fns)
dataUS_q74$socialized_family_z <- normFunc(dataUS_q74$socialized_family)
dataUS_q74$socialized_friends_z <- normFunc(dataUS_q74$socialized_friends)
dataUS_q74$socialized_coworkers_z <- normFunc(dataUS_q74$socialized_coworkers)
dataUS_q74$socialized_other_z <- normFunc(dataUS_q74$socialized_other)
dataUS_q74$household_z <- normFunc(dataUS_q74$household)
dataUS_q74$ability_z <- normFunc(dataUS_q74$ability)
dataUS_q74$availability_z <- normFunc(dataUS_q74$availability)
dataUS_q74$shoppingoutlet_z <- normFunc(dataUS_q74$shoppingoutlet)
dataUS_q74$mfs_z <- normFunc(dataUS_q74$mfs)
dataUS_q74$shoppinglist_z <- normFunc(dataUS_q74$shoppinglist)
dataUS_q74$gov_intervention_z <- normFunc(dataUS_q74$gov_intervention)
dataUS_q74$political_spectrum_z <- normFunc(dataUS_q74$political_spectrum)

##Prepare reduced urban/region sample for Lasso Plus (NA = 266 in full sample due to late question introduction)
#generate dataset only with those that answered the region/urban questions
dataUS_region <- subset(dataUS, region !="NA")
dataUS_region$Group_f  <- plyr::mapvalues(dataUS_region$Group, c("1", "2", "3"), c("Control","Information", "Social Norms"))
dataUS_region$age_z <- normFunc(dataUS_region$age)
dataUS_region$education_z <- normFunc(dataUS_region$education)
dataUS_region$gender_z <- normFunc(dataUS_region$gender)
dataUS_region$urban_z <- normFunc(dataUS_region$urban)
dataUS_region$region_z <- normFunc(dataUS_region$region)
dataUS_region$diet_z <- normFunc(dataUS_region$diet)
dataUS_region$experience_z <- normFunc(dataUS_region$experience)
dataUS_region$cookingexp_z <- normFunc(dataUS_region$cookingexp)
dataUS_region$restaurantexp_z <- normFunc(dataUS_region$restaurantexp)
dataUS_region$fns_z <- normFunc(dataUS_region$fns)
dataUS_region$socialized_family_z <- normFunc(dataUS_region$socialized_family)
dataUS_region$socialized_friends_z <- normFunc(dataUS_region$socialized_friends)
dataUS_region$socialized_coworkers_z <- normFunc(dataUS_region$socialized_coworkers)
dataUS_region$socialized_other_z <- normFunc(dataUS_region$socialized_other)
dataUS_region$household_z <- normFunc(dataUS_region$household)
dataUS_region$ability_z <- normFunc(dataUS_region$ability)
dataUS_region$availability_z <- normFunc(dataUS_region$availability)
dataUS_region$shoppingoutlet_z <- normFunc(dataUS_region$shoppingoutlet)
dataUS_region$mfs_z <- normFunc(dataUS_region$mfs)
dataUS_region$shoppinglist_z <- normFunc(dataUS_region$shoppinglist)
dataUS_region$gov_intervention_z <- normFunc(dataUS_region$gov_intervention)
dataUS_region$political_spectrum_z <- normFunc(dataUS_region$political_spectrum)

##Prepare reduced shopping matrix sample for LassoPlus (NA = 522 in full sample due to late question introduction)
dataUS_esc$Group_f  <- plyr::mapvalues(dataUS_esc$Group, c("1", "2", "3"), c("Control","Information", "Social Norms"))
dataUS_esc$age_z <- normFunc(dataUS_esc$age)
dataUS_esc$education_z <- normFunc(dataUS_esc$education)
dataUS_esc$gender_z <- normFunc(dataUS_esc$gender)
dataUS_esc$diet_z <- normFunc(dataUS_esc$diet)
dataUS_esc$experience_z <- normFunc(dataUS_esc$experience)
dataUS_esc$cookingexp_z <- normFunc(dataUS_esc$cookingexp)
dataUS_esc$restaurantexp_z <- normFunc(dataUS_esc$restaurantexp)
dataUS_esc$fns_z <- normFunc(dataUS_esc$fns)
dataUS_esc$socialized_family_z <- normFunc(dataUS_esc$socialized_family)
dataUS_esc$socialized_friends_z <- normFunc(dataUS_esc$socialized_friends)
dataUS_esc$socialized_coworkers_z <- normFunc(dataUS_esc$socialized_coworkers)
dataUS_esc$socialized_other_z <- normFunc(dataUS_esc$socialized_other)
dataUS_esc$household_z <- normFunc(dataUS_esc$household)
dataUS_esc$ability_z <- normFunc(dataUS_esc$ability)
dataUS_esc$availability_z <- normFunc(dataUS_esc$availability)
dataUS_esc$shoppingoutlet_z <- normFunc(dataUS_esc$shoppingoutlet)
dataUS_esc$mfs_z <- normFunc(dataUS_esc$mfs)
dataUS_esc$shoppinglist_z <- normFunc(dataUS_esc$shoppinglist)
dataUS_esc$gov_intervention_z <- normFunc(dataUS_esc$gov_intervention)
dataUS_esc$political_spectrum_z <- normFunc(dataUS_esc$political_spectrum)

##Prepare reduced shopping matrix sample only meateaters for LassoPlus
dataUS_esc_q74$Group_f  <- plyr::mapvalues(dataUS_esc_q74$Group, c("1", "2", "3"), c("Control","Information", "Social Norms"))
dataUS_esc_q74$age_z <- normFunc(dataUS_esc_q74$age)
dataUS_esc_q74$education_z <- normFunc(dataUS_esc_q74$education)
dataUS_esc_q74$gender_z <- normFunc(dataUS_esc_q74$gender)
dataUS_esc_q74$diet_z <- normFunc(dataUS_esc_q74$diet)
dataUS_esc_q74$experience_z <- normFunc(dataUS_esc_q74$experience)
dataUS_esc_q74$cookingexp_z <- normFunc(dataUS_esc_q74$cookingexp)
dataUS_esc_q74$restaurantexp_z <- normFunc(dataUS_esc_q74$restaurantexp)
dataUS_esc_q74$fns_z <- normFunc(dataUS_esc_q74$fns)
dataUS_esc_q74$socialized_family_z <- normFunc(dataUS_esc_q74$socialized_family)
dataUS_esc_q74$socialized_friends_z <- normFunc(dataUS_esc_q74$socialized_friends)
dataUS_esc_q74$socialized_coworkers_z <- normFunc(dataUS_esc_q74$socialized_coworkers)
dataUS_esc_q74$socialized_other_z <- normFunc(dataUS_esc_q74$socialized_other)
dataUS_esc_q74$household_z <- normFunc(dataUS_esc_q74$household)
dataUS_esc_q74$ability_z <- normFunc(dataUS_esc_q74$ability)
dataUS_esc_q74$availability_z <- normFunc(dataUS_esc_q74$availability)
dataUS_esc_q74$shoppingoutlet_z <- normFunc(dataUS_esc_q74$shoppingoutlet)
dataUS_esc_q74$mfs_z <- normFunc(dataUS_esc_q74$mfs)
dataUS_esc_q74$shoppinglist_z <- normFunc(dataUS_esc_q74$shoppinglist)
dataUS_esc_q74$gov_intervention_z <- normFunc(dataUS_esc_q74$gov_intervention)
dataUS_esc_q74$political_spectrum_z <- normFunc(dataUS_esc_q74$political_spectrum)


#####################################################
## LassoPlus models for outcomes-------
######################################################
library(sparsereg)
library(expm)
## --------------------------------------------------
## DV1a: Intention to eat substitutes
## --------------------------------------------------
set.seed(1)
lp_output1 <- sparsereg::sparsereg(y = dataUS$q72, 
                                   X = as.matrix(data.frame(age = dataUS$age_z, education = dataUS$education_z, gender = dataUS$education_z, diet = dataUS$diet_z, experience = dataUS$experience_z, cooking = dataUS$cookingexp_z, restaurant = dataUS$restaurantexp_z, neophobia = dataUS$fns_z, socializedfam = dataUS$socialized_family_z, socializedfriend = dataUS$socialized_friends_z, socializedwork = dataUS$socialized_coworkers_z, socializedother = dataUS$socialized_other_z, household = dataUS$household_z, able = dataUS$ability_z, available = dataUS$availability_z, outlet = dataUS$shoppingoutlet_z, mainshopper = dataUS$mfs_z, shoplist = dataUS$shoppinglist_z, govintervention = dataUS$gov_intervention_z, leftright = dataUS$political_spectrum_z)),
                                   treat = dataUS$Group_f,
                                   scale.type = "TTX",
                                   baseline.vec = c("Control"),
                                   gibbs=1000, burnin=1000)
sparsereg::plot.sparsereg(lp_output1, plot.one =1, main1 = "Selected Main Effects", xlabel = "Posterior Median of Standardized Effects")
sparsereg::summary.sparsereg(lp_output1, order = "magnitude")

#For sample including esc
set.seed(1)
lp_output1a <- sparsereg::sparsereg(y = dataUS_esc$q72, 
                                    X = as.matrix(data.frame(age = dataUS_esc$age_z, education = dataUS_esc$education_z, gender = dataUS_esc$education_z, diet = dataUS_esc$diet_z, experience = dataUS_esc$experience_z, cooking = dataUS_esc$cookingexp_z, restaurant = dataUS_esc$restaurantexp_z, neophobia = dataUS_esc$fns_z, socializedfam = dataUS_esc$socialized_family_z, socializedfriend = dataUS_esc$socialized_friends_z, socializedwork = dataUS_esc$socialized_coworkers_z, socializedother = dataUS_esc$socialized_other_z, household = dataUS_esc$household_z, able = dataUS_esc$ability_z, available = dataUS_esc$availability_z, outlet = dataUS_esc$shoppingoutlet_z, mainshopper = dataUS_esc$mfs_z, shoplist = dataUS_esc$shoppinglist_z, govintervention = dataUS_esc$gov_intervention_z, leftright = dataUS_esc$political_spectrum_z )),
                                    treat = dataUS_esc$Group_f,
                                    scale.type = "TTX",
                                    baseline.vec = c("Control"),
                                    gibbs=1000, burnin=1000)
sparsereg::plot.sparsereg(lp_output1a, plot.one =1, main1 = "Selected Main Effects", xlabel = "Posterior Median of Standardized Effects")
sparsereg::summary.sparsereg(lp_output1a, order = "magnitude")

#For sample including urban/rural and region
lp_output1b <- sparsereg::sparsereg(y = dataUS_region$q72, 
                                    X = as.matrix(data.frame(age = dataUS_region$age_z, education = dataUS_region$education_z, gender = dataUS_region$education_z, diet = dataUS_region$diet_z, experience = dataUS_region$experience_z, cooking = dataUS_region$cookingexp_z, restaurant = dataUS_region$restaurantexp_z, neophobia = dataUS_region$fns_z, socializedfam = dataUS_region$socialized_family_z, socializedfriend = dataUS_region$socialized_friends_z, socializedwork = dataUS_region$socialized_coworkers_z, socializedother = dataUS_region$socialized_other_z, household = dataUS_region$household_z, able = dataUS_region$ability_z, available = dataUS_region$availability_z, outlet = dataUS_region$shoppingoutlet_z, mainshopper = dataUS_region$mfs_z, shoplist = dataUS_region$shoppinglist_z, govintervention = dataUS_region$gov_intervention_z, leftright = dataUS_region$political_spectrum_z )),
                                    treat = dataUS_region$Group_f,
                                    scale.type = "TTX",
                                    baseline.vec = c("Control"),
                                    gibbs=1000, burnin=1000)
sparsereg::plot.sparsereg(lp_output1b, plot.one =1, main1 = "Selected Main Effects", xlabel = "Posterior Median of Standardized Effects")
sparsereg::summary.sparsereg(lp_output1b, order = "magnitude")

## --------------------------------------------------
## DV1b: Intention to reduce meat consumption
## --------------------------------------------------
set.seed(1)
lp_output2 <- sparsereg::sparsereg(y = dataUS_q74$q74, 
                                   X = as.matrix(data.frame(age = dataUS_q74$age_z, education = dataUS_q74$education_z, gender = dataUS_q74$education_z, diet = dataUS_q74$diet_z, experience = dataUS_q74$experience_z, cooking = dataUS_q74$cookingexp_z, restaurant = dataUS_q74$restaurantexp_z, neophobia = dataUS_q74$fns_z, socializedfam = dataUS_q74$socialized_family_z, socializedfriend = dataUS_q74$socialized_friends_z, socializedwork = dataUS_q74$socialized_coworkers_z, socializedother = dataUS_q74$socialized_other_z, household = dataUS_q74$household_z, able = dataUS_q74$ability_z, available = dataUS_q74$availability_z, outlet = dataUS_q74$shoppingoutlet_z, mainshopper = dataUS_q74$mfs_z, shoplist = dataUS_q74$shoppinglist_z, govintervention = dataUS_q74$gov_intervention_z, leftright = dataUS_q74$political_spectrum_z )),
                                   treat = dataUS_q74$Group_f,
                                   scale.type = "TTX",
                                   baseline.vec = c("Control"),
                                   gibbs=1000, burnin=1000)
sparsereg::plot.sparsereg(lp_output2, plot.one =1, main1 = "Selected Main Effects DV1a", main2 = "Selected Main Effects DV2a", xlabel = "Posterior Median of Standardized Effects")
sparsereg::summary.sparsereg(lp_output2, order= "magnitude")

#For sample including esc
set.seed(1)
lp_output2a <- sparsereg::sparsereg(y = dataUS_esc_q74$q74, 
                                    X = as.matrix(data.frame(age = dataUS_esc_q74$age_z, education = dataUS_esc_q74$education_z, gender = dataUS_esc_q74$education_z, diet = dataUS_esc_q74$diet_z, experience = dataUS_esc_q74$experience_z, cooking = dataUS_esc_q74$cookingexp_z, restaurant = dataUS_esc_q74$restaurantexp_z, neophobia = dataUS_esc_q74$fns_z, socializedfam = dataUS_esc_q74$socialized_family_z, socializedfriend = dataUS_esc_q74$socialized_friends_z, socializedwork = dataUS_esc_q74$socialized_coworkers_z, socializedother = dataUS_esc_q74$socialized_other_z, household = dataUS_esc_q74$household_z, able = dataUS_esc_q74$ability_z, available = dataUS_esc_q74$availability_z, outlet = dataUS_esc_q74$shoppingoutlet_z, mainshopper = dataUS_esc_q74$mfs_z, shoplist = dataUS_esc_q74$shoppinglist_z, govintervention = dataUS_esc_q74$gov_intervention_z, leftright = dataUS_esc_q74$political_spectrum_z )),
                                    treat = dataUS_esc_q74$Group_f,
                                    scale.type = "TTX",
                                    baseline.vec = c("Control"),
                                    gibbs=1000, burnin=1000)
sparsereg::plot.sparsereg(lp_output2a, plot.one =1, main1 = "Selected Main Effects", xlabel = "Posterior Median of Standardized Effects")
sparsereg::summary.sparsereg(lp_output2a, order= "magnitude")

## --------------------------------------------------
## DV1c: Willingness to pay for substitutes
## --------------------------------------------------
set.seed(1)
lp_output19 <- sparsereg::sparsereg(y = dataUS$q76_1, 
                                    X = as.matrix(data.frame(age = dataUS$age_z, education = dataUS$education_z, gender = dataUS$education_z, diet = dataUS$diet_z, experience = dataUS$experience_z, cooking = dataUS$cookingexp_z, restaurant = dataUS$restaurantexp_z, neophobia = dataUS$fns_z, socializedfam = dataUS$socialized_family_z, socializedfriend = dataUS$socialized_friends_z, socializedwork = dataUS$socialized_coworkers_z, socializedother = dataUS$socialized_other_z, household = dataUS$household_z, able = dataUS$ability_z, available = dataUS$availability_z, outlet = dataUS$shoppingoutlet_z, mainshopper = dataUS$mfs_z, shoplist = dataUS$shoppinglist_z, govintervention = dataUS$gov_intervention_z, leftright = dataUS$political_spectrum_z)),
                                    treat = dataUS$Group_f,
                                    scale.type = "TTX",
                                    baseline.vec = c("Control"),
                                    gibbs=1000, burnin=1000)
sparsereg::plot.sparsereg(lp_output19, plot.one =1, main1 = "Selected Main Effects", xlabel = "Posterior Median of Standardized Effects")
sparsereg::summary.sparsereg(lp_output19, order = "magnitude")

## --------------------------------------------------
## DV2a: Intention to support meat reduction policies
## --------------------------------------------------
set.seed(1)
lp_output4 <- sparsereg::sparsereg(y = dataUS$q78, 
                                   X = as.matrix(data.frame(age = dataUS$age_z, education = dataUS$education_z, gender = dataUS$education_z, diet = dataUS$diet_z, experience = dataUS$experience_z, cooking = dataUS$cookingexp_z, restaurant = dataUS$restaurantexp_z, neophobia = dataUS$fns_z, socializedfam = dataUS$socialized_family_z, socializedfriend = dataUS$socialized_friends_z, socializedwork = dataUS$socialized_coworkers_z, socializedother = dataUS$socialized_other_z, household = dataUS$household_z, able = dataUS$ability_z, available = dataUS$availability_z, outlet = dataUS$shoppingoutlet_z, mainshopper = dataUS$mfs_z, shoplist = dataUS$shoppinglist_z, govintervention = dataUS$gov_intervention_z, leftright = dataUS$political_spectrum_z )),
                                   treat = dataUS$Group_f,
                                   scale.type = "TTX",
                                   baseline.vec = c("Control"),
                                   gibbs=1000, burnin=1000)
sparsereg::plot.sparsereg(lp_output4, plot.one =1, main1 = "Selected Main Effects", xlabel = "Posterior Median of Standardized Effects")
sparsereg::summary.sparsereg(lp_output4, order= "magnitude")

#For sample including escc
set.seed(1)
lp_output4a <- sparsereg::sparsereg(y = dataUS_esc$q78, 
                                    X = as.matrix(data.frame(age = dataUS_esc$age_z, education = dataUS_esc$education_z, gender = dataUS_esc$education_z, diet = dataUS_esc$diet_z, experience = dataUS_esc$experience_z, cooking = dataUS_esc$cookingexp_z, restaurant = dataUS_esc$restaurantexp_z, neophobia = dataUS_esc$fns_z, socializedfam = dataUS_esc$socialized_family_z, socializedfriend = dataUS_esc$socialized_friends_z, socializedwork = dataUS_esc$socialized_coworkers_z, socializedother = dataUS_esc$socialized_other_z, household = dataUS_esc$household_z, able = dataUS_esc$ability_z, available = dataUS_esc$availability_z, outlet = dataUS_esc$shoppingoutlet_z, mainshopper = dataUS_esc$mfs_z, shoplist = dataUS_esc$shoppinglist_z, govintervention = dataUS_esc$gov_intervention_z, leftright = dataUS_esc$political_spectrum_z )),
                                    treat = dataUS_esc$Group_f,
                                    scale.type = "TTX",
                                    baseline.vec = c("Control"),
                                    gibbs=1000, burnin=1000)
sparsereg::plot.sparsereg(lp_output4a, plot.one =1, main1 = "Selected Main Effects", xlabel = "Posterior Median of Standardized Effects")
sparsereg::summary.sparsereg(lp_output4a, order= "magnitude")

## --------------------------------------------------
## DV2b: Intention to support policies incentivizing substitute consumption
## --------------------------------------------------
set.seed(1)
lp_output5 <- sparsereg::sparsereg(y = dataUS$q79, 
                                   X = as.matrix(data.frame(age = dataUS$age_z, education = dataUS$education_z, gender = dataUS$education_z, diet = dataUS$diet_z, experience = dataUS$experience_z, cooking = dataUS$cookingexp_z, restaurant = dataUS$restaurantexp_z, neophobia = dataUS$fns_z, socializedfam = dataUS$socialized_family_z, socializedfriend = dataUS$socialized_friends_z, socializedwork = dataUS$socialized_coworkers_z, socializedother = dataUS$socialized_other_z, household = dataUS$household_z, able = dataUS$ability_z, available = dataUS$availability_z, outlet = dataUS$shoppingoutlet_z, mainshopper = dataUS$mfs_z, shoplist = dataUS$shoppinglist_z, govintervention = dataUS$gov_intervention_z, leftright = dataUS$political_spectrum_z )),
                                   treat = dataUS$Group_f,
                                   scale.type = "TTX",
                                   baseline.vec = c("Control"),
                                   gibbs=1000, burnin=1000)
sparsereg::plot.sparsereg(lp_output5, plot.one =1, main1 = "Selected Main Effects", xlabel = "Posterior Median of Standardized Effects")
sparsereg::summary.sparsereg(lp_output5, order= "magnitude")

#For sample including esc
set.seed(1)
lp_output5a <- sparsereg::sparsereg(y = dataUS_esc$q79, 
                                    X = as.matrix(data.frame(age = dataUS_esc$age_z, education = dataUS_esc$education_z, gender = dataUS_esc$education_z, diet = dataUS_esc$diet_z, experience = dataUS_esc$experience_z, cooking = dataUS_esc$cookingexp_z, restaurant = dataUS_esc$restaurantexp_z, neophobia = dataUS_esc$fns_z, socializedfam = dataUS_esc$socialized_family_z, socializedfriend = dataUS_esc$socialized_friends_z, socializedwork = dataUS_esc$socialized_coworkers_z, socializedother = dataUS_esc$socialized_other_z, household = dataUS_esc$household_z, able = dataUS_esc$ability_z, available = dataUS_esc$availability_z, outlet = dataUS_esc$shoppingoutlet_z, mainshopper = dataUS_esc$mfs_z, shoplist = dataUS_esc$shoppinglist_z, govintervention = dataUS_esc$gov_intervention_z, leftright = dataUS_esc$political_spectrum_z )),
                                    treat = dataUS_esc$Group_f,
                                    scale.type = "TTX",
                                    baseline.vec = c("Control"),
                                    gibbs=1000, burnin=1000)
sparsereg::plot.sparsereg(lp_output5a, plot.one =1, main1 = "Selected Main Effects", xlabel = "Posterior Median of Standardized Effects")
sparsereg::summary.sparsereg(lp_output5a, order= "magnitude")

## --------------------------------------------------
## DV2c: Intention to support meat reduction policy instruments
## --------------------------------------------------
##taxes
set.seed(1)
lp_output6 <- sparsereg::sparsereg(y = dataUS$q81_1, 
                                   X = as.matrix(data.frame(age = dataUS$age_z, education = dataUS$education_z, gender = dataUS$education_z, diet = dataUS$diet_z, experience = dataUS$experience_z, cooking = dataUS$cookingexp_z, restaurant = dataUS$restaurantexp_z, neophobia = dataUS$fns_z, socializedfam = dataUS$socialized_family_z, socializedfriend = dataUS$socialized_friends_z, socializedwork = dataUS$socialized_coworkers_z, socializedother = dataUS$socialized_other_z, household = dataUS$household_z, able = dataUS$ability_z, available = dataUS$availability_z, outlet = dataUS$shoppingoutlet_z, mainshopper = dataUS$mfs_z, shoplist = dataUS$shoppinglist_z, govintervention = dataUS$gov_intervention_z, leftright = dataUS$political_spectrum_z )),
                                   treat = dataUS$Group_f,
                                   scale.type = "TTX",
                                   baseline.vec = c("Control"),
                                   gibbs=1000, burnin=1000)
sparsereg::plot.sparsereg(lp_output6, plot.one =1, main1 = "Selected Main Effects", xlabel = "Posterior Median of Standardized Effects")
sparsereg::summary.sparsereg(lp_output6, order = "magnitude")

##two meat-free days
set.seed(1)
lp_output7 <- sparsereg::sparsereg(y = dataUS$q81_2, 
                                   X = as.matrix(data.frame(age = dataUS$age_z, education = dataUS$education_z, gender = dataUS$education_z, diet = dataUS$diet_z, experience = dataUS$experience_z, cooking = dataUS$cookingexp_z, restaurant = dataUS$restaurantexp_z, neophobia = dataUS$fns_z, socializedfam = dataUS$socialized_family_z, socializedfriend = dataUS$socialized_friends_z, socializedwork = dataUS$socialized_coworkers_z, socializedother = dataUS$socialized_other_z, household = dataUS$household_z, able = dataUS$ability_z, available = dataUS$availability_z, outlet = dataUS$shoppingoutlet_z, mainshopper = dataUS$mfs_z, shoplist = dataUS$shoppinglist_z, govintervention = dataUS$gov_intervention_z, leftright = dataUS$political_spectrum_z )),
                                   treat = dataUS$Group_f,
                                   scale.type = "TTX",
                                   baseline.vec = c("Control"),
                                   gibbs=1000, burnin=1000)
sparsereg::plot.sparsereg(lp_output7, plot.one =1, main1 = "Selected Main Effects", xlabel = "Posterior Median of Standardized Effects")
sparsereg::summary.sparsereg(lp_output7, order = "magnitude")

##eliminate subsidies for producers
set.seed(1)
lp_output8 <- sparsereg::sparsereg(y = dataUS$q81_3, 
                                   X = as.matrix(data.frame(age = dataUS$age_z, education = dataUS$education_z, gender = dataUS$education_z, diet = dataUS$diet_z, experience = dataUS$experience_z, cooking = dataUS$cookingexp_z, restaurant = dataUS$restaurantexp_z, neophobia = dataUS$fns_z, socializedfam = dataUS$socialized_family_z, socializedfriend = dataUS$socialized_friends_z, socializedwork = dataUS$socialized_coworkers_z, socializedother = dataUS$socialized_other_z, household = dataUS$household_z, able = dataUS$ability_z, available = dataUS$availability_z, outlet = dataUS$shoppingoutlet_z, mainshopper = dataUS$mfs_z, shoplist = dataUS$shoppinglist_z, govintervention = dataUS$gov_intervention_z, leftright = dataUS$political_spectrum_z )),
                                   treat = dataUS$Group_f,
                                   scale.type = "TTX",
                                   baseline.vec = c("Control"),
                                   gibbs=1000, burnin=1000)
sparsereg::plot.sparsereg(lp_output8, plot.one =1, main1 = "Selected Main Effects", xlabel = "Posterior Median of Standardized Effects")
sparsereg::summary.sparsereg(lp_output8, order = "magnitude")

## --------------------------------------------------
## DV2d: Intention to support policy instruments incentivizing substitute consumption
## --------------------------------------------------
##taxes
set.seed(1)
lp_output9 <- sparsereg::sparsereg(y = dataUS$q83_1, 
                                   X = as.matrix(data.frame(age = dataUS$age_z, education = dataUS$education_z, gender = dataUS$education_z, diet = dataUS$diet_z, experience = dataUS$experience_z, cooking = dataUS$cookingexp_z, restaurant = dataUS$restaurantexp_z, neophobia = dataUS$fns_z, socializedfam = dataUS$socialized_family_z, socializedfriend = dataUS$socialized_friends_z, socializedwork = dataUS$socialized_coworkers_z, socializedother = dataUS$socialized_other_z, household = dataUS$household_z, able = dataUS$ability_z, available = dataUS$availability_z, outlet = dataUS$shoppingoutlet_z, mainshopper = dataUS$mfs_z, shoplist = dataUS$shoppinglist_z, govintervention = dataUS$gov_intervention_z, leftright = dataUS$political_spectrum_z )),
                                   treat = dataUS$Group_f,
                                   scale.type = "TTX",
                                   baseline.vec = c("Control"),
                                   gibbs=1000, burnin=1000)
sparsereg::plot.sparsereg(lp_output9, plot.one =1, main1 = "Selected Main Effects", xlabel = "Posterior Median of Standardized Effects")
sparsereg::summary.sparsereg(lp_output9, order = "magnitude")

##two substitute days
set.seed(1)
lp_output10 <- sparsereg::sparsereg(y = dataUS$q83_2, 
                                    X = as.matrix(data.frame(age = dataUS$age_z, education = dataUS$education_z, gender = dataUS$education_z, diet = dataUS$diet_z, experience = dataUS$experience_z, cooking = dataUS$cookingexp_z, restaurant = dataUS$restaurantexp_z, neophobia = dataUS$fns_z, socializedfam = dataUS$socialized_family_z, socializedfriend = dataUS$socialized_friends_z, socializedwork = dataUS$socialized_coworkers_z, socializedother = dataUS$socialized_other_z, household = dataUS$household_z, able = dataUS$ability_z, available = dataUS$availability_z, outlet = dataUS$shoppingoutlet_z, mainshopper = dataUS$mfs_z, shoplist = dataUS$shoppinglist_z, govintervention = dataUS$gov_intervention_z, leftright = dataUS$political_spectrum_z )),
                                    treat = dataUS$Group_f,
                                    scale.type = "TTX",
                                    baseline.vec = c("Control"),
                                    gibbs=1000, burnin=1000)
sparsereg::plot.sparsereg(lp_output10, plot.one =1, main1 = "Selected Main Effects", xlabel = "Posterior Median of Standardized Effects")
sparsereg::summary.sparsereg(lp_output10 , order = "magnitude")

##introduce subsidies for producers
set.seed(1)
lp_output11 <- sparsereg::sparsereg(y = dataUS$q83_3, 
                                    X = as.matrix(data.frame(age = dataUS$age_z, education = dataUS$education_z, gender = dataUS$education_z, diet = dataUS$diet_z, experience = dataUS$experience_z, cooking = dataUS$cookingexp_z, restaurant = dataUS$restaurantexp_z, neophobia = dataUS$fns_z, socializedfam = dataUS$socialized_family_z, socializedfriend = dataUS$socialized_friends_z, socializedwork = dataUS$socialized_coworkers_z, socializedother = dataUS$socialized_other_z, household = dataUS$household_z, able = dataUS$ability_z, available = dataUS$availability_z, outlet = dataUS$shoppingoutlet_z, mainshopper = dataUS$mfs_z, shoplist = dataUS$shoppinglist_z, govintervention = dataUS$gov_intervention_z, leftright = dataUS$political_spectrum_z )),
                                    treat = dataUS$Group_f,
                                    scale.type = "TTX",
                                    baseline.vec = c("Control"),
                                    gibbs=1000, burnin=1000)
sparsereg::plot.sparsereg(lp_output11, plot.one =1, main1 = "Selected Main Effects", xlabel = "Posterior Median of Standardized Effects")
sparsereg::summary.sparsereg(lp_output11, order = "magnitude")

## --------------------------------------------------
## DV3a: Intentions to offer substitutes in different situations
## --------------------------------------------------
##cooking for friends at home
set.seed(1)
lp_output12 <- sparsereg::sparsereg(y = dataUS$q85_1, 
                                    X = as.matrix(data.frame(age = dataUS$age_z, education = dataUS$education_z, gender = dataUS$education_z, diet = dataUS$diet_z, experience = dataUS$experience_z, cooking = dataUS$cookingexp_z, restaurant = dataUS$restaurantexp_z, neophobia = dataUS$fns_z, socializedfam = dataUS$socialized_family_z, socializedfriend = dataUS$socialized_friends_z, socializedwork = dataUS$socialized_coworkers_z, socializedother = dataUS$socialized_other_z, household = dataUS$household_z, able = dataUS$ability_z, available = dataUS$availability_z, outlet = dataUS$shoppingoutlet_z, mainshopper = dataUS$mfs_z, shoplist = dataUS$shoppinglist_z, govintervention = dataUS$gov_intervention_z, leftright = dataUS$political_spectrum_z )),
                                    treat = dataUS$Group_f,
                                    scale.type = "TTX",
                                    baseline.vec = c("Control"),
                                    gibbs=1000, burnin=1000)
sparsereg::plot.sparsereg(lp_output12, plot.one =1, main1 = "Selected Main Effects", xlabel = "Posterior Median of Standardized Effects")
sparsereg::summary.sparsereg(lp_output12, order = "magnitude")

##cooking for family on a holiday
set.seed(1)
lp_output13 <- sparsereg::sparsereg(y = dataUS$q85_2, 
                                    X = as.matrix(data.frame(age = dataUS$age_z, education = dataUS$education_z, gender = dataUS$education_z, diet = dataUS$diet_z, experience = dataUS$experience_z, cooking = dataUS$cookingexp_z, restaurant = dataUS$restaurantexp_z, neophobia = dataUS$fns_z, socializedfam = dataUS$socialized_family_z, socializedfriend = dataUS$socialized_friends_z, socializedwork = dataUS$socialized_coworkers_z, socializedother = dataUS$socialized_other_z, household = dataUS$household_z, able = dataUS$ability_z, available = dataUS$availability_z, outlet = dataUS$shoppingoutlet_z, mainshopper = dataUS$mfs_z, shoplist = dataUS$shoppinglist_z, govintervention = dataUS$gov_intervention_z, leftright = dataUS$political_spectrum_z )),
                                    treat = dataUS$Group_f,
                                    scale.type = "TTX",
                                    baseline.vec = c("Control"),
                                    gibbs=1000, burnin=1000)
sparsereg::plot.sparsereg(lp_output13, plot.one =1, main1 = "Selected Main Effects", xlabel = "Posterior Median of Standardized Effects")
sparsereg::summary.sparsereg(lp_output13, order = "magnitude")

##cooking for family on a weekday
set.seed(1)
lp_output14 <- sparsereg::sparsereg(y = dataUS$q85_3, 
                                    X = as.matrix(data.frame(age = dataUS$age_z, education = dataUS$education_z, gender = dataUS$education_z, diet = dataUS$diet_z, experience = dataUS$experience_z, cooking = dataUS$cookingexp_z, restaurant = dataUS$restaurantexp_z, neophobia = dataUS$fns_z, socializedfam = dataUS$socialized_family_z, socializedfriend = dataUS$socialized_friends_z, socializedwork = dataUS$socialized_coworkers_z, socializedother = dataUS$socialized_other_z, household = dataUS$household_z, able = dataUS$ability_z, available = dataUS$availability_z, outlet = dataUS$shoppingoutlet_z, mainshopper = dataUS$mfs_z, shoplist = dataUS$shoppinglist_z, govintervention = dataUS$gov_intervention_z, leftright = dataUS$political_spectrum_z )),
                                    treat = dataUS$Group_f,
                                    scale.type = "TTX",
                                    baseline.vec = c("Control"),
                                    gibbs=1000, burnin=1000)
sparsereg::plot.sparsereg(lp_output14, plot.one =1, main1 = "Selected Main Effects", xlabel = "Posterior Median of Standardized Effects")
sparsereg::summary.sparsereg(lp_output14, order = "magnitude")

##hosting a party
set.seed(1)
lp_output15 <- sparsereg::sparsereg(y = dataUS$q85_4, 
                                    X = as.matrix(data.frame(age = dataUS$age_z, education = dataUS$education_z, gender = dataUS$education_z, diet = dataUS$diet_z, experience = dataUS$experience_z, cooking = dataUS$cookingexp_z, restaurant = dataUS$restaurantexp_z, neophobia = dataUS$fns_z, socializedfam = dataUS$socialized_family_z, socializedfriend = dataUS$socialized_friends_z, socializedwork = dataUS$socialized_coworkers_z, socializedother = dataUS$socialized_other_z, household = dataUS$household_z, able = dataUS$ability_z, available = dataUS$availability_z, outlet = dataUS$shoppingoutlet_z, mainshopper = dataUS$mfs_z, shoplist = dataUS$shoppinglist_z, govintervention = dataUS$gov_intervention_z, leftright = dataUS$political_spectrum_z )),
                                    treat = dataUS$Group_f,
                                    scale.type = "TTX",
                                    baseline.vec = c("Control"),
                                    gibbs=1000, burnin=1000)
sparsereg::plot.sparsereg(lp_output15, plot.one =1, main1 = "Selected Main Effects", xlabel = "Posterior Median of Standardized Effects")
sparsereg::summary.sparsereg(lp_output15, order = "magnitude")

##organizing a business meal
set.seed(1)
lp_output16 <- sparsereg::sparsereg(y = dataUS$q85_5, 
                                    X = as.matrix(data.frame(age = dataUS$age_z, education = dataUS$education_z, gender = dataUS$education_z, diet = dataUS$diet_z, experience = dataUS$experience_z, cooking = dataUS$cookingexp_z, restaurant = dataUS$restaurantexp_z, neophobia = dataUS$fns_z, socializedfam = dataUS$socialized_family_z, socializedfriend = dataUS$socialized_friends_z, socializedwork = dataUS$socialized_coworkers_z, socializedother = dataUS$socialized_other_z, household = dataUS$household_z, able = dataUS$ability_z, available = dataUS$availability_z, outlet = dataUS$shoppingoutlet_z, mainshopper = dataUS$mfs_z, shoplist = dataUS$shoppinglist_z, govintervention = dataUS$gov_intervention_z, leftright = dataUS$political_spectrum_z )),
                                    treat = dataUS$Group_f,
                                    scale.type = "TTX",
                                    baseline.vec = c("Control"),
                                    gibbs=1000, burnin=1000)
sparsereg::plot.sparsereg(lp_output16, plot.one =1, main1 = "Selected Main Effects", xlabel = "Posterior Median of Standardized Effects")
sparsereg::summary.sparsereg(lp_output16, order = "magnitude")

##cooking for vegetarians
set.seed(1)
lp_output17 <- sparsereg::sparsereg(y = dataUS$q85_6, 
                                    X = as.matrix(data.frame(age = dataUS$age_z, education = dataUS$education_z, gender = dataUS$education_z, diet = dataUS$diet_z, experience = dataUS$experience_z, cooking = dataUS$cookingexp_z, restaurant = dataUS$restaurantexp_z, neophobia = dataUS$fns_z, socializedfam = dataUS$socialized_family_z, socializedfriend = dataUS$socialized_friends_z, socializedwork = dataUS$socialized_coworkers_z, socializedother = dataUS$socialized_other_z, household = dataUS$household_z, able = dataUS$ability_z, available = dataUS$availability_z, outlet = dataUS$shoppingoutlet_z, mainshopper = dataUS$mfs_z, shoplist = dataUS$shoppinglist_z, govintervention = dataUS$gov_intervention_z, leftright = dataUS$political_spectrum_z )),
                                    treat = dataUS$Group_f,
                                    scale.type = "TTX",
                                    baseline.vec = c("Control"),
                                    gibbs=1000, burnin=1000)
sparsereg::plot.sparsereg(lp_output17, plot.one =1, main1 = "Selected Main Effects", xlabel = "Posterior Median of Standardized Effects")
sparsereg::summary.sparsereg(lp_output17, order = "magnitude")

## --------------------------------------------------
## DV3b: Intentions to share information on substitute benefits
## --------------------------------------------------
set.seed(1)
lp_output18 <- sparsereg::sparsereg(y = dataUS$q105, 
                                    X = as.matrix(data.frame(age = dataUS$age_z, education = dataUS$education_z, gender = dataUS$education_z, diet = dataUS$diet_z, experience = dataUS$experience_z, cooking = dataUS$cookingexp_z, restaurant = dataUS$restaurantexp_z, neophobia = dataUS$fns_z, socializedfam = dataUS$socialized_family_z, socializedfriend = dataUS$socialized_friends_z, socializedwork = dataUS$socialized_coworkers_z, socializedother = dataUS$socialized_other_z, household = dataUS$household_z, able = dataUS$ability_z, available = dataUS$availability_z, outlet = dataUS$shoppingoutlet_z, mainshopper = dataUS$mfs_z, shoplist = dataUS$shoppinglist_z, govintervention = dataUS$gov_intervention_z, leftright = dataUS$political_spectrum_z )),
                                    treat = dataUS$Group_f,
                                    scale.type = "TTX",
                                    baseline.vec = c("Control"),
                                    gibbs=1000, burnin=1000)
sparsereg::plot.sparsereg(lp_output18, plot.one =1, main1 = "Selected Main Effects", xlabel = "Posterior Median of Standardized Effects")
sparsereg::summary.sparsereg(lp_output18, order = "magnitude")

######################################################
## Robustness checks
######################################################
attach(dataUS)
## --------------------------------------------------
## Manipulation check
## --------------------------------------------------
##check how perception of meat impacts/substitute benefits are impacted by treatment assignment
#health impacts
rc4 <- lm(q100_1 ~ Group, data = dataUS)
summary(rc4)
coeftest(rc4, vcov=sandwich)
#environmental impacts
rc5 <- lm(q100_2 ~ Group, data = dataUS)
summary(rc5)
coeftest(rc5, vcov=sandwich)
#animal welfare impacts
rc6 <- lm(q100_3 ~ Group, data = dataUS)
summary(rc6)
coeftest(rc6, vcov=sandwich)
#health benefits
rc7 <- lm(q101_1 ~ Group, data = dataUS)
summary(rc7)
coeftest(rc7, vcov=sandwich)
#environmental benefits
rc8 <- lm(q101_2 ~ Group, data = dataUS)
summary(rc8)
coeftest(rc8, vcov=sandwich)
#animal welfare benefits
rc9 <- lm(q101_3 ~ Group, data = dataUS)
summary(rc9)
coeftest(rc9, vcov=sandwich)

##check how attitude towards meat and substitutes is impacted by treatment assignment
rc10 <- lm(meat_attributes ~ Group, data = dataUS)
summary(rc10)
coeftest(rc10, vcov=sandwich)
rc11 <- lm(substitute_attributes ~ Group, data = dataUS)
summary(rc11)
coeftest(rc11, vcov=sandwich)


##check if credibility of information received is impacted by treatment assignment
rc12 <- lm(credibility ~ Group, data = dataUS)
summary(rc12)
table(dataUS$Group)
table(dataUS$credibility, dataUS$Group==1)/450
table(dataUS$credibility, dataUS$Group==2)/456
table(dataUS$credibility, dataUS$Group==3)/454
##Determine impact of treatment assignment
kruskalrc12 <- kruskal.test(credibility ~ Treatment_, data = dataUS)
kruskalrc12
dunnTest(credibility ~ Treatment_, data = dataUS, two.sided=T)
dunnTest(credibility ~ Treatment_, data = dataUS, two.sided=T, method = "bh")

## Regression table credibility rc10
rc12$rse <-sqrt(diag(vcovHC(rc12, type="HC2")))
stargazer(rc12,
          title = "Check impact of treatment assignment on credibility of information received in the US sample", 
          dep.var.labels = c("Credibility"), 
          covariate.labels = c( "Information Group", "Social Norms Group"),
          se=list(rc12$rse),  
          align= TRUE, header = FALSE, type = "html", no.space = TRUE, out = "US_Robustness_Credibility.html" )



