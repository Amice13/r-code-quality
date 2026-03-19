#analysis scripts for Study2

#set your own directory 

dat2 <- read.csv(file="dat2.csv",header=T) #n=970
names(dat2)

#descriptive of outcome variables
mean(dat2$donate) #3.17 
sd(dat2$donate) #1.29
mean(dat2$volunteer) #2.88
sd(dat2$volunteer) #1.28

#reliability check of scales, Cronbach's alpha
library(psych)

#partnership evaluation
pevalitems <- select(dat2,20,21,22,23) 
alpha(pevalitems) #.96
#descriptives
mean(dat2$peval) #5.81
sd(dat2$peval) #sd 1.37

#post attitude toward a nonprofit
attitudeitems <- select(dat2,16,17,18,19) 
alpha(attitudeitems) #.95

mean(dat2$nppostattitude) #5.67
sd(dat2$nppostattitude) #1.19

#manipulation, ttest, scenario types (one with nature conservancy; one with BGCA) and partnership evaluation 
#make sure the scenario variable is coded as a factor
is.factor(dat2$np) #yes
t.test(dat2$peval~dat2$np) #not signifincant

#manipulation, ttest, partnership type and partnership evaluation
is.factor(dat2$type) #false
dat2$type <- as.factor(dat2$type)
t.test(dat2$peval~dat2$type) #not significant

#manipulation, ttest, partnership source and partnership evaluation
is.factor(dat2$source) #false
dat2$source <- as.factor(dat2$source)
t.test(dat2$peval~dat2$source) #not significant

#manipulation, anova, partnership duration and partnership evaluation
is.factor(dat2$duration) #false
dat2$duration <- as.factor(dat2$duration)
fit0 <- aov(peval ~ source,data=dat2)
summary(fit0) #not significant

#regression models
#DV1 donate intention
model1 <- lm(donate ~ gender+source+type+duration+peval+nppostattitude, data=dat2) 
summary(model1)
#type (- and significant), partnership eval (+ and significant), and post attitude (+ and significant) 

#post-hoc analysis given the counter-intuitive finding (-) 
#use the new variable of typenew (recoded from the 2 levels to 3 levels)
#1 = transactional partnerships
#2 = environmental certification between nature conservancy and union pacific
#3 = collective impact between costco and boys and girls club of america
is.factor(dat2$typenew) #true
dat2$typenew <- as.factor(dat2$typenew)

fit1 <- aov(donate ~ typenew, data=dat2)
summary(fit1) #p = .0276, significant

#post-hoc analysis to detect which of the three conditions differ in DV1, using Tukey test
install.packages("rstatix")
library(rstatix)
pwc <- dat2 %>% tukey_hsd(donate ~ typenew)
pwc #significant difference was found between condition 1 and condition 2

#calculate group means and SDs
library(plyr)
ddply(dat2,~typenew,summarise,mean=mean(donate),sd=sd(donate))

#plot of the difference (shown in Figure 2)
plot(donate~typenew, data = dat2)

#testing if there is a need to explore moderation effect of nonprofit attitude (post)

#moderation effects between post attitude and partnership characteristics 
model1.1 <- lm(donate ~ gender+source+type+duration+peval+nppostattitude+nppostattitude*source, data=dat2) 
summary(model1.1) #no significant moderation effect

model1.2 <- lm(donate ~ gender+source+type+duration+peval+nppostattitude+nppostattitude*type, data=dat2) 
summary(model1.2) #no significant moderation effect

model1.3 <- lm(donate ~ gender+source+type+duration+peval+nppostattitude+nppostattitude*duration, data=dat2) 
summary(model1.3)  #no significant moderation effect

#DV2: volunteer intention
model2 <- lm(volunteer ~ gender+source+type+duration+nppostattitude+peval, data=dat2) 
summary(model2)
#p eval + sig
#post attitude + sig

#moderation effects between post attitude and partnership characteristics 
model2.1 <- lm(volunteer ~ gender+source+type+duration+peval+nppostattitude+nppostattitude*source, data=dat2) 
summary(model2.1) #no significant moderation effect

model2.2 <- lm(volunteer ~ gender+source+type+duration+peval+nppostattitude+nppostattitude*type, data=dat2) 
summary(model2.2) #no significant moderation effect

model3.3 <- lm(volunteer ~ gender+source+type+duration+peval+nppostattitude+nppostattitude*duration, data=dat2) 
summary(model3.3)  #no significant moderation effect