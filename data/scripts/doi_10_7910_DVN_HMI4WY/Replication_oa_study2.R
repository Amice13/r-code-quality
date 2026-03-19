# Libraries
install.packages(c("texreg","lmtest"))
library(texreg)
library(lmtest)

# Read in the data
load("Study2.RData")

##########################
#### Table OA8.1
##########################

range(cons$age)
t.test(age ~ treatment, cons)

range(cons$male)
t.test(male ~ treatment, cons)

range(cons$educ_age2,na.rm=T)
t.test(educ_age2 ~ treatment, cons)

range(cons$educ_level_numeric, na.rm=T)
t.test(educ_level_numeric ~ treatment, cons)

range(cons$educ_level_numeric2, na.rm=T)
t.test(educ_level_numeric2 ~ treatment, cons)

cons_treat <- subset(cons, treatment==1)
cons_control <- subset(cons, treatment==0)
cons_treat_table <- table(cons_treat$ethnic)[order(names(table(cons_treat$ethnic)))]
cons_control_table <- cons_treat_table
table(cons_control$ethnic)[order(names(table(cons_control$ethnic)))]
cons_control_table <- c(9,5,4,0,282)
tab <- rbind(cons_treat_table, cons_control_table)
rownames(tab) <- c("cons_treat","cons_control")
chisq.test(tab)

##########################
#### Table OA8.2
##########################

round(prop.table(tab, margin=1),2)

##########################
#### Table OA8.3
##########################

randomization_check_mod <- glm(treatment ~ age + male + educ_level_numeric2 + ethnic, data=cons, family="binomial")
texreg(randomization_check_mod,stars=c(0.01,0.05,0.1))

lrtest(randomization_check_mod)

##########################
#### Table OA8.4
##########################

range(lab$age)
t.test(age ~ treatment, lab)

range(lab$male)
t.test(male ~ treatment, lab)

range(lab$educ_age2,na.rm=T)
t.test(educ_age2 ~ treatment, lab)

range(lab$educ_level_numeric)
t.test(educ_level_numeric ~ treatment, lab)

range(lab$educ_level_numeric2)
t.test(educ_level_numeric2 ~ treatment, lab)

lab_treat <- subset(lab, treatment==1)
lab_control <- subset(lab, treatment==0)
tab <- rbind(table(lab_treat$ethnic), table(lab_control$ethnic))
rownames(tab) <- c("lab_treat","lab_control")
chisq.test(tab)

##########################
#### Table OA8.5
##########################

round(prop.table(tab, margin=1),2)

##########################
#### Table OA8.6
##########################

randomization_check_mod <- glm(treatment ~ age + male + educ_level_numeric2 + ethnic, data=lab, family="binomial")
texreg(randomization_check_mod,stars=c(0.01,0.05,0.1))

lrtest(randomization_check_mod)

##########################
#### Table OA8.7
##########################

vote_loyal_logit_lab_r <- glm(vote_loyal ~ treatment + age + age_sq + male + educ_level_numeric2 + ethnic, data=lab, family="binomial")
vote_loyal_logit_cons_r <- glm(vote_loyal ~ treatment + age + age_sq + male + educ_level_numeric2 + ethnic, data=cons, family="binomial")
vote_loyal_logit_inter_r <- glm(vote_loyal ~ treatment * lab + age + age_sq + male + educ_level_numeric2 + ethnic, data=combined, family="binomial")

texreg(list(vote_loyal_logit_cons_r, vote_loyal_logit_lab_r, vote_loyal_logit_inter_r), stars=c(0.01,0.05,0.1))
