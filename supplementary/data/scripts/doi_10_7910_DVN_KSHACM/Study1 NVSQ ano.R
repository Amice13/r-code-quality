#analysis scripts for Study1

#set your own directory 
dat1 <- read.csv(file="dat1.csv",header=T) #n=966
#make sure the file is saved as a csv file

names(dat1)
dat1

#manipulation test: created fit and partnership evaluation; using ttest
t.test(dat1$peval~dat1$createdfit) #significant

#manipulation test: partnership scenairos and partnership evaluation; using anova 
#note that crandom is the variable about partnership scenario (a total of 4)
#1 - costco and  Natural conservancy; 2 - Costco and BGC; 3 - Philip Morris and BGC; 4 - Philip Morris and Natural conservancy
#make sure crandom is coded as categorical
is.factor(dat1$crandom) #no
dat1$crandom <- as.factor(dat1$crandom)

fit1 <- aov(peval ~ crandom, data=dat1)
summary(fit1) #significant 

#posthoc comparsion of means across the four conditions
install.packages("rstatix")
library(rstatix)
difference <- dat1 %>% tukey_hsd(peval ~ crandom)
difference #group 1 and group 3; group 1 and group 4; group 2 and group 3; group 2 and group 4

#the average score of peval by conditions
aggregate(dat1$peval, by=list(dat1$crandom), FUN=mean)
#M1 = 5.44, M2 = 5.60, M3 = 4.71, M4 = 4.81
aggregate(dat1$peval, by=list(dat1$crandom), FUN=sd)
#SD1 = 1.04, SD2=1.02, SD3=1.45,SD4=1.40

#calcualte scale reliability nonprofit attitude, parnership evaluation
library(dplyr)
library(psych)
#nonprofit attitude, note that items1,3,4,6,7,9 were reverse coded
names(dat1)
npaitems <- select(dat1,21,24,27,29,61,62,63,64,65,66)
alpha(npaitems) #alpha = .95

#partnership evaluation
pevalitems <- select(dat1,47,48,49,50) 
alpha(pevalitems)  #alpha = .94

#descriptives
mean(dat1$npattitude,na.rm=TRUE)
sd(dat1$npattitude,na.rm=TRUE)

mean(dat1$peval)
sd(dat1$peval)

#descriptive of DVs
mean(dat1$donate)
sd(dat1$donate)

mean(dat1$volunteer)
sd(dat1$volunteer)

#two regressions
#DV: donate
model.1 <- lm(donate ~ npattitude+createdfit+peval, data=dat1)
summary(model.1)

#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -0.64626    0.18509  -3.492 0.000502 ***
#npattitude   0.48939    0.02986  16.388  < 2e-16 ***
#createdfit   0.11210    0.06613   1.695 0.090347 .  
#peval        0.21102    0.02648   7.970 4.47e-15 ***
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#Residual standard error: 1.024 on 961 degrees of freedom
#(1 observation deleted due to missingness)
#Multiple R-squared:  0.318,	Adjusted R-squared:  0.3158 
#F-statistic: 149.3 on 3 and 961 DF,  p-value: < 2.2e-16

model.2 <- lm(volunteer ~ npattitude+createdfit+peval, data=dat1)
summary(model.2)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -0.14403    0.18623  -0.773   0.4395    
#npattitude   0.38668    0.03005  12.869  < 2e-16 ***
#  createdfit   0.15590    0.06654   2.343   0.0193 *  
#  peval        0.17185    0.02664   6.451 1.76e-10 ***
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#Residual standard error: 1.031 on 961 degrees of freedom
#(1 observation deleted due to missingness)
#Multiple R-squared:  0.229,	Adjusted R-squared:  0.2266 
#F-statistic: 95.15 on 3 and 961 DF,  p-value: < 2.2e-16

#mediation effect of partnership evaluation on how created fit influences the 2 outcome variables
library(mediation)
library(gvlma)

#Step1 Check the effect of Created Fit on Mediator Partnership Evaluation
model.01 <- lm(peval ~ createdfit, data=dat1)
summary(model.01) #significant,a effect X->M

#Step2 check the effect of partnership evaluation and created fit on donate and on volunteer
model.02 <- lm(donate ~ peval + createdfit, data=dat1)
summary(model.02) 
#p eval significant, but not created fit #c not significant

model.03 <- lm(volunteer ~ peval + createdfit, data=dat1)
summary(model.03) 
#partnership evaluation and created fit are both significant

model.00 <- lm(volunteer ~  createdfit, data=dat1)
summary(model.00)  #significant

model.000 <- lm(donate ~  createdfit, data=dat1)
summary(model.000)  #significant

#donate as DV
dMed <- mediate(model.01, model.02, treat="createdfit", mediator="peval")
summary(dMed)
plot(dMed)

#acme: average casual mediation effects (indirect effect = total effect - direct effect)
#ade: average direct effects
#total effect
#prop mediated: ratio of these estimates

#results: significant mediation effect of partnership evaluation on the relationship between created fit and donate (ACME=.06, p = .02)
#no direct effect of created fit on donate (ADE=.10, p = .19) and significant total effect (.16, p = .04)
#prop. mediated .37

##Bootstrap to verify
dMedBoot <- mediate(model.01, model.02, boot=TRUE, sims=999, treat="createdfit", mediator="peval")
summary(dMedBoot)

#again, a significant mediation effect (ACME = .06, p = .03), no direct effect (ADE = .10, p = .20). 
#with increased power, this analysis still shows a significant total effect (.16, p = .05).
#prop mediated .39

#volunteer as DV
vMed <- mediate(model.01, model.03, treat="createdfit", mediator="peval")
summary(vMed)
plot(vMed)
#results: significant effect of cause on the relationship between fit and volunteer (ACME=.05, p = .02)
#direct effect of created fit on volunteer (ADE = .14, p = .05) and significant total effect (.19, p = .02)
#prop mediated 26%

#Bootstrap
vMedBoot <- mediate(model.01, model.03, boot=TRUE, sims=999, treat="createdfit", mediator="peval")
summary(vMedBoot)
#again, a significant mediation effect (ACME=.05, p = .03)
#direct effect of created fit on volunteer (ADE = .14, p = .03) and significant total effect (.19, p = .01)
#prop mediated 26%

#note that everytime the models run, it may generate different p values; howerver whether or ot these pvalues are significant (e.g. <. 05) should be consistent 
