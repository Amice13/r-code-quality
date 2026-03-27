###################
###################
### Never Again ###
#### Obs & Exp ####
###################
###################

#########
# setup #
#########

rm(list=ls())

# dependencies
library(readr); library(tidymodels); library(ggpubr); library(dplyr)
library(tidyr); library(stringr); library(texreg)
library(emmeans); library(ltm); library(jtools); library(ggplot2)
library(DirectEffects); library(sensemakr); library(tidyverse)

# set working directory to "Replication" folder

#read in data
load("Replication/holoLegacyReplication1.RData")

#########################################################
##################      MAIN TEXT     ###################
#########################################################

################
### Figure 1 ###
################

# Baseline outgroup attitudes: Full sample
par(mfcol = c(2,3))
hist(data$eritrean, na.rm=T, main="Decrease African Refugees",xlab="",
     breaks=c(1:7),ylim=c(0,1200))
hist(data$westBank, na.rm=T, main="Oppose Easing Checkpoints",xlab="",
     breaks=c(1:7),ylim=c(0,1200))
hist(data$syrian, na.rm=T, main="Oppose Syrian Refugees",xlab="",
     breaks=c(1:7),ylim=c(0,1200))
hist(data$gazaExempt, na.rm=T, main="Decrease Gazan Medical Exemptions",xlab="",
     breaks=c(1:7),ylim=c(0,1200))
hist(data$palestinian, na.rm=T, main="Oppose Equal Rights for PCI",xlab="",
     breaks=c(1:7),ylim=c(0,1200))
hist(data$iranSanction, na.rm=T, main="Increase Sanctions on Iran",xlab="",
     breaks=c(1:7),ylim=c(0,1200))

############
# Figure 2 #
############

# SeqG
mod1g <- sequential_g(eritrean.x ~ exposed + treatment + prewarLoc + prewarWhiteCollar + 
                        prewarRelig + prewarWealth + logAge + man | terrorAttack + 
                        combat + ashkenazi + eduLevel + income + religiosity + 
                        groupVictim + immigrant +  personalVictim + informed | ideology, data)
mod2g <- sequential_g(syrian.x ~ exposed + treatment + prewarLoc + prewarWhiteCollar + 
                        prewarRelig + prewarWealth + logAge + man | terrorAttack + 
                        combat + ashkenazi + eduLevel + income + religiosity + 
                        groupVictim + immigrant +  personalVictim + informed | ideology, data)
mod3g <- sequential_g(westBank.x ~ exposed + treatment + prewarLoc + prewarWhiteCollar + 
                        prewarRelig + prewarWealth + logAge + man | terrorAttack + 
                        combat + ashkenazi + eduLevel + income + religiosity + 
                        groupVictim + immigrant +  personalVictim + informed | ideology, data)
mod4g <- sequential_g(palestinian.x ~ exposed + treatment + prewarLoc + prewarWhiteCollar + 
                        prewarRelig + prewarWealth + logAge + man | terrorAttack + 
                        combat + ashkenazi + eduLevel + income + religiosity + 
                        groupVictim + immigrant + personalVictim + informed | ideology, data)
mod5g <- sequential_g(gazaExempt.x ~ exposed + treatment + prewarLoc + prewarWhiteCollar + 
                        prewarRelig + prewarWealth + logAge + man | terrorAttack + 
                        combat + ashkenazi + eduLevel + income + religiosity + 
                        groupVictim + immigrant +  personalVictim + informed | ideology, data)
mod6g <- sequential_g(iranSanction.x ~ exposed + treatment + prewarLoc + prewarWhiteCollar + 
                        prewarRelig + prewarWealth + logAge + man | terrorAttack + 
                        combat + ashkenazi + eduLevel + income + religiosity + 
                        groupVictim + immigrant +  personalVictim + informed | ideology, data)

# Seq G Effects
estimate <- c(coef(mod1g)[2],coef(mod2g)[2],coef(mod3g)[2],coef(mod4g)[2],
              coef(mod5g)[2],coef(mod6g)[2])
se <- c(0.013,0.015,0.015,0.016,0.014,0.013)
lower <- c(confint(mod1g,level=0.95)[2,1],confint(mod2g,level=0.95)[2,1],
           confint(mod3g,level=0.95)[2,1],confint(mod4g,level=0.95)[2,1],
           confint(mod5g,level=0.95)[2,1],confint(mod6g,level=0.95)[2,1])
upper <- c(confint(mod1g,level=0.95)[2,2],confint(mod2g,level=0.95)[2,2],
           confint(mod3g,level=0.95)[2,2],confint(mod4g,level=0.95)[2,2],
           confint(mod5g,level=0.95)[2,2],confint(mod6g,level=0.95)[2,2])
group <- c("Eritrean & Sudanese Refugees","Syrian Refugees","West Bank Palestinians",
           "Palestinian Citizens of Israel","Palestinians from Gaza","Iranians")
gEffects <- data.frame(estimate,se,upper,lower,group)


# OLS 
mod1 <- lm(eritrean.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
             eduLevel + income + exposed + treatment + religiosity + 
             ideology + groupVictim + personalVictim + immigrant + informed +
             as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
             prewarWealth, data)
mod2 <- lm(syrian.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
             eduLevel + income + exposed + treatment + religiosity + 
             ideology + groupVictim + personalVictim + immigrant + informed +
             as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
             prewarWealth, data)
mod3 <- lm(westBank.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
             eduLevel + income + exposed + treatment + religiosity + 
             ideology + groupVictim + personalVictim + immigrant + informed + 
             as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
             prewarWealth, data)
mod4 <- lm(palestinian.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
             eduLevel + income + exposed + treatment + religiosity + 
             ideology + groupVictim + personalVictim + immigrant + informed + 
             as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
             prewarWealth, data)
mod5 <- lm(gazaExempt.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
             eduLevel + income + exposed + treatment + religiosity + 
             ideology + groupVictim + personalVictim + immigrant + informed + 
             as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
             prewarWealth, data)
mod6 <- lm(iranSanction.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
             eduLevel + income + exposed + treatment + religiosity + 
             ideology + groupVictim + personalVictim + immigrant + informed + 
             as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
             prewarWealth, data)

# OLS Effects
estimate <- c(coef(mod1)[9],coef(mod2)[9],coef(mod3)[9],coef(mod4)[9],
              coef(mod5)[9],coef(mod6)[9])
se <- c(0.012,0.015,0.015,0.015,0.014,0.014)
lower <- c(confint(mod1,level=0.95)[9,1],confint(mod2,level=0.95)[9,1],
           confint(mod3,level=0.95)[9,1],confint(mod4,level=0.95)[9,1],
           confint(mod5,level=0.95)[9,1],confint(mod6,level=0.95)[9,1])
upper <- c(confint(mod1,level=0.95)[9,2],confint(mod2,level=0.95)[9,2],
           confint(mod3,level=0.95)[9,2],confint(mod4,level=0.95)[9,2],
           confint(mod5,level=0.95)[9,2],confint(mod6,level=0.95)[9,2])
group <- c("Eritrean & Sudanese Refugees","Syrian Refugees","West Bank Palestinians",
           "Palestinian Citizens of Israel","Palestinians from Gaza","Iranians")
effects <- data.frame(estimate,se,upper,lower,group)


# combine seqG and OLS effects into one df
seqGplot <- rbind(effects,gEffects)
seqGplot$model <- c(rep("OLS",6),rep("Sequential G",6))

level_order <- c("Iranians","Palestinians from Gaza","Palestinian Citizens of Israel",
                 "West Bank Palestinians","Syrian Refugees","Eritrean & Sudanese Refugees") 
p1 <- ggplot(effects, aes(x=factor(group,level=level_order),estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper),width = 0.2) +
  geom_hline(yintercept=0, linetype="dashed") +
  ylim(-.12,.12) +
  xlab("")  + ylab("Estimate") +
  coord_flip() +
  theme_classic()

p2 <- ggplot(gEffects, aes(x=factor(group,level=level_order),estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper),width = 0.2) +
  geom_hline(yintercept=0, linetype="dashed") +
  ylim(-.12,.12) +
  xlab("")  + ylab("Estimate") +
  coord_flip()+
  theme_classic()
p1
p2

#############
## TABLE 1 ##
#############

exp1 <- lm(eritrean.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
             eduLevel + income + exposed + treatment + religiosity + 
             ideology + groupVictim + immigrant + personalVictim + informed +
             as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
             prewarWealth + preEritrean.x, data)
exp2 <- lm(syrian.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
             eduLevel + income + exposed + treatment + religiosity + 
             ideology + groupVictim + immigrant + personalVictim + informed +
             as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
             prewarWealth + preSyrian.x, data)
exp3 <- lm(westBank.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
             eduLevel + income + exposed + treatment + religiosity + 
             ideology + groupVictim + immigrant + personalVictim + informed + 
             as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
             prewarWealth + preWestBank.x, data)
exp4 <- lm(palestinian.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
             eduLevel + income + exposed + treatment + religiosity + 
             ideology + groupVictim + immigrant + personalVictim + informed + 
             as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
             prewarWealth + prePalestinian.x, data)
exp5 <- lm(gazaExempt.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
             eduLevel + income + exposed + treatment + religiosity + 
             ideology + groupVictim + immigrant + personalVictim + informed + 
             as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
             prewarWealth + preGaza.x, data)
exp6 <- lm(iranSanction.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
             eduLevel + income + exposed + treatment + religiosity + 
             ideology + groupVictim + immigrant + personalVictim + informed +
             as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
             prewarWealth + preIran.x, data)

screenreg(list(exp1,exp2,exp3,exp4,exp5,exp6),
       caption.above = TRUE,
       caption = "Experimental Results: No Framing Effect",
       custom.model.names=c("Africans","Syrians","West Bank","PCIs",
                            "Gazan Pal.","Iranians"),
       custom.gof.rows = list("Prewar Region FEs" = c("Yes","Yes","Yes","Yes","Yes","Yes"),
                              "Pre-treatment DVs" = c("Yes","Yes","Yes","Yes","Yes","Yes")),
       include.rsquared = F,
       custom.coef.map=list("treatmentempathy"="Treatment:Empathy",
                            "treatmentthreat"="Treatment:Threat",
                            "exposed"="Surv/Desc","groupVictim"="Group Victimhood",
                            "personalVictim"="Self Victimhood",
                            "terrorAttack"="Terror Attack","combat"="Combat",
                            "ashkenazi"="Ashkenazi","logAge"="log(Age)",
                            "man"="Man","eduLevel"="Education",
                            "income"="Income",
                            "religiosity"="Religiosity","ideology"="Ideology",
                            "immigrant"="Immigrant","informed"="Pol. Interest",
                            "prewarWhiteCollar"="White Collar (pre)",
                            "prewarRelig"="Religiosity (pre)",
                            "prewarWealth"="Wealth (pre)",
                            "(Intercept)"="Constant"),
       include.adjrs = F,
       digits = 2)

##############
# Time Spent #
##############
# (described at bottom of p.14)

# total duration of survey (minutes)
mean(data$duration, na.rm=T)/60

# threat: time spent on treatment
mean(c(data$`Q205_Page Submit`,data$`Q244_Page Submit`,data$`Q249_Page Submit`,
       data$`Q463_Page Submit`,data$`Q254_Page Submit`,data$`Q259_Page Submit`),
     na.rm=T)

# empathy: time spent on treatment
mean(c(data$`Q201_Page Submit`,data$`Q223_Page Submit`,data$`Q228_Page Submit`,
       data$`Q458_Page Submit`,data$`Q233_Page Submit`,data$`Q238_Page Submit`),
     na.rm=T)

# control: time spent on treatment
mean(c(data$`Q209_Page Submit`,data$`Q213_Page Submit`,data$`Q265_Page Submit`,
       data$`Q270_Page Submit`,data$`Q468_Page Submit`,data$`Q275_Page Submit`,
       data$`Q280_Page Submit`,data$`Q286_Page Submit`,data$`Q291_Page Submit`,
       data$`Q473_Page Submit`,data$`Q296_Page Submit`,data$`Q301_Page Submit`),
     na.rm=T)

###############
# NEVER AGAIN #
###############

# Average exclusionary takeaway
# (described at top of p.15)
mean(data$lessonScale,na.rm=T)

# Correlation between Holocaust takeaway and ideology
# (described in footnote 20)
cor(data$lessonScale,data$ideology, use="complete.obs")

##############
# VICTIMHOOD #
##############

# Scale creation
# (described on p.16)
  
  # Jewish victimhood
  cronbach.alpha(data[complete.cases(data[,c("Q15_1","Q15_2","Q15_3",
                                             "Q15_4","Q15_5","Q15_6")]),
                      c("Q15_1","Q15_2","Q15_3",
                        "Q15_4","Q15_5","Q15_6")], CI=T)

  # Israeli victimhood
  cronbach.alpha(data[complete.cases(data[,c("Q16_1","Q16_2","Q16_3",
                                           "Q16_4","Q16_5","Q16_6")]),
                    c("Q16_1","Q16_2","Q16_3",
                      "Q16_4","Q16_5","Q16_6")], CI=T)
  
# Cronbach's alpha for Israeli victimhood and Jewish Victimhood
# (described in footnote 21)
cronbach.alpha(data[complete.cases(data[,c("israelVictim","jewishVictim")]),c("israelVictim","jewishVictim")], CI=T)

# Factor loadings of victimhood measures
# (described in footnote 21)
fitData <- data[!is.na(data$israelVictim) & !is.na(data$jewishVictim) & !is.na(data$personalVictim),]
fit <- factanal(fitData[,c("israelVictim","jewishVictim","personalVictim")], 1, rotation="promax")
fit

############
# Figure 3 #
############

data %>% 
  mutate(exposure = ifelse(exposed==1,"Descendants & Survivors","Non-descendants")) %>%
  dplyr::select(israelVictim.x,jewishVictim.x,exposure) %>% 
  pivot_longer(cols = c(israelVictim.x,jewishVictim.x), names_to = "Victimhood") %>% 
  ggplot(aes(x = Victimhood, y = value, fill = exposure)) +
  stat_summary(geom = "bar", position = "dodge") +
  stat_summary(width = 0.2, 
               geom = "errorbar", 
               position = position_dodge(0.95), 
               fun.data = mean_cl_normal) +
  scale_fill_discrete(name="Exposure",
                      breaks=c(1, 2),
                      labels=c("Exposed", "Not exposed"))+
  xlab("Perceived Victimhood Type")+ylab("Perception of Victimhood (normalized)") +
  labs(fill="Exposure") +
  ylim(0,1) +
  scale_x_discrete(labels=c("israelVictim.x" = "Israeli", "jewishVictim.x" = "Jewish")) +
  scale_fill_brewer(palette="Greens") + theme_minimal() 

############
# Figure 4 #
############

plot_summs(list(mod1,mod2,mod3,mod4,mod5,mod6), plot.distributions = FALSE, 
           point.shape=TRUE, model.names=(c("Eritrean & Sudanese Refugees", 
                                            "Syrian Refugees", 
                                            "West Bank Palestinians", 
                                            "Palestinian Citizens of Israel", 
                                            "Palestinians from Gaza", 
                                            "Iranians")), 
           coefs=c("Group Victimhood"="groupVictim"), legend.title="Model Outgroup", 
           colors=c(rep("black",6)), scale = T)


#########################################################
##################      APPENDIX      ###################
#########################################################

###########
##  A.3  ##
###########

# Correlation between age and ideology
cor(data$age[!is.na(data$age) & !is.na(data$ideology)], 
    data$ideology[!is.na(data$age) & !is.na(data$ideology)])
summary(aov(data$ideology[!is.na(data$ageCat) & !is.na(data$ideology)] ~ 
            data$ageCat[!is.na(data$ageCat) & !is.na(data$ideology)]))

# relationship between ethnicity and descendant status
nrow(data[data$exposed==1 & data$ashkenazi==1,])
nrow(data[data$exposed==0 & data$ashkenazi==1,])
nrow(data[data$exposed==1 & data$mizrachi==1,])
nrow(data[data$exposed==0 & data$mizrachi==1,])
nrow(data[data$exposed==1 & data$mixedEthnic==1,])
nrow(data[data$exposed==0 & data$mixedEthnic==1,])
nrow(data[data$exposed==1 & data$mixedEthnic!=1 & data$ashkenazi!=1 & data$mizrachi!=1,])
nrow(data[data$exposed==0 & data$mixedEthnic!=1 & data$ashkenazi!=1 & data$mizrachi!=1,])

###########
##  A.4  ##
###########

#Function source: Replication Data for: Increasing Precision Without Altering 
#Treatment Effects: Repeated Measures Designs in Survey Experiments (version 1.0), 
#by Clifford, Scott; Sheagley, Geoffrey; Piston, Spencer

#function to calculate power
power_function<-function(alpha=.05, effect.size=.2, p = .5, N, r_pre=0) {
  cval <- qnorm(1 - alpha/2) #assuming two-tailed test
  if (r_pre==0)  {
    the.power<-pnorm(-cval+effect.size*sqrt(p*(1-p)*N))+
      pnorm(-cval - effect.size*sqrt(p*(1-p)*N))
  }
  if (r_pre!=0) {
    the.power <- pnorm(-cval + effect.size*sqrt(p*(1-p)*N/(1-r_pre^2))) + 
      pnorm(-cval - effect.size*sqrt(p*(1-p)*N/(1-r_pre^2)))
  }  
  return(list(power=the.power, samplesize=N, pretreat=r_pre))
}

#parameters
effect=.2 #cohen's d
min_n<-50 
max_n<-1000

#run Loop
sample<-(seq(from=min_n, to = max_n, by=1))
datalist = list()  
counta=0
countb=0

#allowing for variation in the correlation between pre and post treatment
#measures of the DV
r_pre_input<-c(.6, .75, .9)
length_r_pre_input<-length(r_pre_input)

for(h in 1:length(r_pre_input)){
  mat1<-matrix(data=NA, nrow=length(sample), ncol=3)
  
  for (i in sample) {
    counta<-counta+1
    power_calc<-power_function(N = i,effect.size=effect, r_pre=r_pre_input[h]) 
    
    mat1[counta,1] <- i 
    mat1[counta,2] <- h
    mat1[counta,3] <- power_calc$power
  }  
  counta<-0 
  final<-as_tibble(mat1)  
  final<-final %>% rename(N=V1,r_pre=V2, Power=V3)
  countb<-countb+1
  datalist[[countb]]<-assign(paste0("df",h), final)
}

df_final<-bind_rows(datalist)

##give the data some labels
df_final$r_pre<-factor(df_final$r_pre, 
                       levels=c(1,2,3),
                       labels=c("r =.6","r =.75","r =.9"))
summary(df_final)

plot1 <-ggplot(df_final, aes(x=N, y=Power, group=r_pre)) +
  geom_segment(x=375, xend=375, y=0, yend=1, linetype=2) +
  geom_segment(x=775, xend=775, y=0, yend=.98, linetype=2) +
  geom_segment(x=1000, xend=1000, y=0, yend=1, linetype=2) +
  geom_line(aes(color=r_pre), lwd=2.5) +
  ylab("")+xlab("")+labs(title="Power by Pre-Post Measure Correlation", size=18)+
  scale_y_continuous(breaks=seq(0, 1, .10))+
  theme_bw()+
  theme(plot.title = element_text(size=15))+
  theme(legend.title = element_blank())+
  theme(legend.position="bottom")+
  theme(legend.text=element_text(size=13))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=11))

library(pwr)
#Statistical Power Test for Linear Regression
#Model with 23 covariates [pre-war, post-war, exposure category, and treatment dummies]
#where u and v are the numerator and denominator degrees of freedom. We use f2 as the effect size measure.
#u = number of variables in the model + the intercept - 1
#v = number of error degrees of freedom, which is n - u
#f2 of .02 is "small" according to Cohen
pwr.f2.test(u = 23, v = , f2 = 0.02, sig.level = .05, power = .8)

#Statistical Power Test for T-Tests (different sample size in each group) 
#where n is the sample size, d is the effect size, and type indicates a two-sample t-test, one-sample t-test or paired t-test. We use d as the effect size measure.
#d of .2 is "small according to Cohen
#pwr.t.test(n = , d = .2, sig.level = .05, power = .8, type = c("two.sample"))

pwr.r.test(r = 0.2, power = 0.8,sig.level = 0.05)

######interaction
library(Superpower)
design_result <- ANOVA_design(design = "3b*3b", n = 666,
                              mu = c(4.1, 4.8, 4.9, 4.7, 5.1, 4.2, 5, 5.5, 4.1), sd = .2,
                              labelnames = c("condition", "control", "inclusive",
                                             "exclusive", "status", "descendant", "nondescendant", "survivor"))
# Figure A.2
exact_result <- ANOVA_exact(design_result, alpha_level = 0.5)

#############
# TABLE D.1 #
#############

ex1 <- lm(eritrean.x ~ treatment + preEritrean.x, data)
ex2 <- lm(syrian.x ~ treatment + preSyrian.x, data)
ex3 <- lm(westBank.x ~ treatment + preWestBank.x, data)
ex4 <- lm(palestinian.x ~ treatment + prePalestinian.x, data)
ex5 <- lm(gazaExempt.x ~ treatment + preGaza.x, data)
ex6 <- lm(iranSanction.x ~ treatment + preIran.x, data)
screenreg(list(ex1,ex2,ex3,ex4,ex5,ex6),
       custom.model.names=c("Africans","Syrians","West Bank","PCIs",
                            "Gazan Pal.","Iranians"),
       caption.above = TRUE,
       caption = "Experimental Results: Treatment and Pre-Post Outcomes Only",
       include.rsquared = F,
       custom.coef.map=list("treatmentempathy"="Treatment:Empathy",
                            "treatmentthreat"="Treatment:Threat",
                            "preEritrean.x"="Pre-treatment (Africans)",
                            "preSyrian.x"="Pre-treatment (Syrians)",
                            "preWestBank.x"="Pre-treatment (West Bank)",
                            "prePalestinian.x"="Pre-treatment (PCIs)",
                            "preGaza.x"="Pre-treatment (Gazan Pal.)",
                            "preIran.x"="Pre-treatment (Iranians)",
                            "(Intercept)"="Constant"),
       include.adjrs = T,
       digits = 2)

#############
# TABLE D.2 #
#############

screenreg(list(exp1,exp2,exp3,exp4,exp5,exp6),
          caption.above = TRUE,
          caption = "Experimental Results: No Framing Effect",
          custom.model.names=c("Africans","Syrians","West Bank","PCIs",
                               "Gazan Pal.","Iranians"),
          custom.gof.rows = list("Prewar Region FEs" = c("Yes","Yes","Yes","Yes","Yes","Yes"),
                                 "Pre-treatment DVs" = c("Yes","Yes","Yes","Yes","Yes","Yes")),
          include.rsquared = F,
          custom.coef.map=list("treatmentempathy"="Treatment:Empathy",
                               "treatmentthreat"="Treatment:Threat",
                               "exposed"="Surv/Desc","groupVictim"="Group Victimhood",
                               "personalVictim"="Self Victimhood",
                               "terrorAttack"="Terror Attack","combat"="Combat",
                               "ashkenazi"="Ashkenazi","logAge"="log(Age)",
                               "man"="Man","eduLevel"="Education",
                               "income"="Income",
                               "religiosity"="Religiosity","ideology"="Ideology",
                               "immigrant"="Immigrant","informed"="Pol. Interest",
                               "prewarWhiteCollar"="White Collar (pre)",
                               "prewarRelig"="Religiosity (pre)",
                               "prewarWealth"="Wealth (pre)",
                               "(Intercept)"="Constant"),
          include.adjrs = F,
          digits = 2)

#############
# TABLE D.3 #
#############

int1 <- lm(eritrean.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
             eduLevel + income + exposed*treatment + religiosity + 
             ideology + groupVictim + immigrant + personalVictim + informed +
             as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
             prewarWealth + preEritrean.x, data)
int2 <- lm(syrian.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
             eduLevel + income + exposed*treatment + religiosity + 
             ideology + groupVictim + immigrant + personalVictim + informed +
             as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
             prewarWealth + preSyrian.x, data)
int3 <- lm(westBank.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
             eduLevel + income + exposed*treatment + religiosity + 
             ideology + groupVictim + immigrant + personalVictim + informed + 
             as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
             prewarWealth + preWestBank.x, data)
int4 <- lm(palestinian.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
             eduLevel + income + exposed*treatment + religiosity + 
             ideology + groupVictim + immigrant + personalVictim + informed + 
             as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
             prewarWealth + prePalestinian.x, data)
int5 <- lm(gazaExempt.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
             eduLevel + income + exposed*treatment + religiosity + 
             ideology + groupVictim + immigrant + personalVictim + informed + 
             as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
             prewarWealth + preGaza.x, data)
int6 <- lm(iranSanction.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
             eduLevel + income + exposed*treatment + religiosity + 
             ideology + groupVictim + immigrant + personalVictim + informed +
             as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
             prewarWealth + preIran.x, data)

# check to make sure interaction appears in table
screenreg(list(int1,int2,int3,int4,int5,int6),
       caption.above = TRUE,
       caption = "Attitude Malleability: Treatment*Surv/Desc",
       custom.model.names=c("Africans","Syrians","West Bank","PCIs",
                            "Gazan Pal.","Iranians"),
       custom.gof.rows = list("Prewar Region FEs" = c("Yes","Yes","Yes","Yes","Yes","Yes"),
                              "Pre-treatment DVs" = c("Yes","Yes","Yes","Yes","Yes","Yes")),
       include.rsquared = F,
       custom.coef.map=list("treatmentempathy"="Treatment:Empathy",
                            "treatmentthreat"="Treatment:Threat",
                            "exposed:treatmentempathy"="Treatment:Empathy*Surv/Desc",
                            "exposed:treatmentthreat"="Treatment:Threat*Surv/Desc",
                            "exposed"="Surv/Desc","groupVictim"="Group Victimhood",
                            "personalVictim"="Self Victimhood",
                            "terrorAttack"="Terror Attack","combat"="Combat",
                            "ashkenazi"="Ashkenazi","logAge"="log(Age)",
                            "man"="Man","eduLevel"="Education",
                            "income"="Income",
                            "religiosity"="Religiosity","ideology"="Ideology",
                            "immigrant"="Immigrant","informed"="Pol. Interest",
                            "prewarWhiteCollar"="White Collar (pre)",
                            "prewarRelig"="Religiosity (pre)",
                            "prewarWealth"="Wealth (pre)",
                            "(Intercept)"="Constant"),
       include.adjrs = F,
       digits = 2)

#############
# TABLE E.1 #
#############

# [Table E.1 made manually. Summaries below]
summary(mod1g); summary(mod2g); summary(mod3g)
summary(mod4g); summary(mod5g); summary(mod6g)

#############
# TABLE E.2 #
#############

screenreg(list(mod1,mod2,mod3,mod4,mod5,mod6),
          custom.model.names=c("Africans","Syrians","West Bank","PCIs",
                               "Gazan Pal.","Iranians"),
          caption.above = TRUE,
          caption = "OLS Analysis for Figures 2, 4",
          custom.gof.rows = list("Prewar Region FEs" = c("Yes","Yes","Yes","Yes","Yes","Yes")),
          include.rsquared = F,
          custom.coef.map=list("treatmentempathy"="Treatment: Empathy",
                               "treatmentthreat"="Treatment: Threat",
                               "exposed"="Surv/Descendant",
                               "groupVictim"="Group Victimhood",
                               "personalVictim"="Self Victimhood",
                               "terrorAttack"="Terror Attack",
                               "combat"="Combat",
                               "ashkenazi"="Ashkenazi",
                               "logAge"="log(Age)",
                               "man"="Man",
                               "eduLevel"="Education",
                               "income"="Income",
                               "religiosity"="Religiosity",
                               "ideology"="Ideology",
                               "immigrant"="Immigrant",
                               "informed"="Pol. Interest",
                               "prewarWhiteCollar"="White Collar (prewar)",
                               "prewarRelig"="Religiosity (prewar)",
                               "prewarWealth"="Wealth (prewar)",
                               "(Intercept)"="Constant"),
          include.adjrs = T,
          digits = 2)

##############
# FIGURE F.1 #
##############

par(mfcol = c(2,3))
hist(data.g$eritrean, na.rm=T, main="Decrease African Refugees",xlab="",
     breaks=c(1:7),ylim=c(0,450))
hist(data.g$westBank, na.rm=T, main="Oppose Easing Checkpoints",xlab="",
     breaks=c(1:7),ylim=c(0,450))
hist(data.g$syrian, na.rm=T, main="Oppose Syrian Refugees",xlab="",
     breaks=c(1:7),ylim=c(0,450))
hist(data.g$gazaExempt, na.rm=T, main="Decrease Gazan Medical Exemptions",xlab="",
     breaks=c(1:7),ylim=c(0,450))
hist(data.g$palestinian, na.rm=T, main="Oppose Equal Rights for PCI",xlab="",
     breaks=c(1:7),ylim=c(0,450))
hist(data.g$iranSanction, na.rm=T, main="Increase Sanctions on Iran",xlab="",
     breaks=c(1:7),ylim=c(0,450))

#############
# TABLE F.1 #
#############

mod1e <- lm(eritrean.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + exposed + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed +
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data.g)
mod2e <- lm(syrian.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + exposed + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data.g)
mod3e <- lm(westBank.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + exposed + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data.g)
mod4e <- lm(palestinian.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + exposed + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data.g)
mod5e <- lm(gazaExempt.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + exposed + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data.g)
mod6e <- lm(iranSanction.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + exposed + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data.g)

screenreg(list(mod1e,mod2e,mod3e,mod4e,mod5e,mod6e),
       custom.model.names=c("Africans","Syrians","West Bank","PCIs",
                            "Gazan Pal.","Iranians"),
       caption.above = TRUE,
       caption = "OLS Analysis: Control Group Only",
       custom.gof.rows = list("Prewar Region FEs" = c("Yes","Yes","Yes","Yes","Yes","Yes")),
       include.rsquared = F,
       custom.coef.map=list("exposed"="Surv/Descendant",
                            "groupVictim"="Group Victimhood",
                            "personalVictim"="Self Victimhood",
                            "immigrant"="Immigrant",
                            "terrorAttack"="Terror Attack",
                            "combat"="Combat",
                            "ashkenazi"="Ashkenazi",
                            "logAge"="log(Age)",
                            "man"="Man",
                            "eduLevel"="Education",
                            "income"="Income",
                            "religiosity"="Religiosity",
                            "ideology"="Ideology",
                            "informed"="Pol. Interest",
                            "prewarWhiteCollar"="White Collar (prewar)",
                            "prewarRelig"="Religiosity (prewar)",
                            "prewarWealth"="Wealth (prewar)",
                            "(Intercept)"="Constant"),
       include.adjrs = T,
       digits = 2)

#############
# TABLE F.2 #
#############

mod1s <- lm(africanScale ~ treatment + terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + exposed + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed +
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data)
mod2s <- lm(syrianScale ~ treatment +  terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + exposed + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data)
mod3s <- lm(westbankScale ~ treatment +  terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + exposed + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed +
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data)
mod4s <- lm(palScale ~ treatment + terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + exposed + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data)
mod5s <- lm(gazaScale ~ treatment + terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + exposed + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data)
mod6s <- lm(iranScale ~ treatment + terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + exposed + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data)
mod7s <- lm(iran.x ~ treatment + terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + exposed + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data)

screenreg(list(mod1s,mod2s,mod3s,mod4s,mod5s,mod6s,mod7s),
       custom.model.names=c("Africans","Syrians","West Bank","PCIs",
                            "Gazan Pal.","Iranians", "Iranians (alt)"),
       caption.above = TRUE,
       caption = "Alternative Outcome: Scaled Dependent Variables",
       custom.gof.rows = list("Prewar Region FEs" = c("Yes","Yes","Yes","Yes","Yes","Yes","Yes")),
       include.rsquared = F,
       custom.coef.map=list("treatmentempathy"="Treatment: Empathy",
                            "treatmentthreat"="Treatment: Threat",
                            "exposed"="Surv/Descendant",
                            "groupVictim"="Group Victimhood",
                            "personalVictim"="Self Victimhood",
                            "immigrant"="Immigrant",
                            "terrorAttack"="Terror Attack",
                            "combat"="Combat",
                            "ashkenazi"="Ashkenazi",
                            "logAge"="log(Age)",
                            "man"="Man",
                            "eduLevel"="Education",
                            "income"="Income",
                            "religiosity"="Religiosity",
                            "ideology"="Ideology",
                            "informed"="Pol. Interest",
                            "prewarWhiteCollar"="White Collar (prewar)",
                            "prewarRelig"="Religiosity (prewar)",
                            "prewarWealth"="Wealth (prewar)",
                            "(Intercept)"="Constant"),
       include.adjrs = T,
       digits = 2)

##############
# FIGURE F.2 #
##############

sens1 <- cdesens(mod1g, var = "exposed") 
sens2 <- cdesens(mod2g, var = "exposed") 
sens3 <- cdesens(mod3g, var = "exposed") 
sens4 <- cdesens(mod4g, var = "exposed") 
sens5 <- cdesens(mod5g, var = "exposed") 
sens6 <- cdesens(mod6g, var = "exposed") 

plot(sens1)
plot(sens2)
plot(sens3)
plot(sens4)
plot(sens5)
plot(sens6)

#############
# TABLE F.3 #
#############

mod1a <- lm(eritrean.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + descendant_count + treatment + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed +
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data)
mod2a <- lm(syrian.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + descendant_count + treatment + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data)
mod3a <- lm(westBank.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + descendant_count + treatment + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data)
mod4a <- lm(palestinian.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + descendant_count + treatment + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data)
mod5a <- lm(gazaExempt.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + descendant_count + treatment + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data)
mod6a <- lm(iranSanction.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + descendant_count + treatment + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data)

screenreg(list(mod1a,mod2a,mod3a,mod4a,mod5a,mod6a),
       custom.model.names=c("Africans","Syrians","West Bank","PCIs",
                            "Gazan Pal.","Iranians"),
       caption.above = TRUE,
       caption = "Alternative Predictor: Count of Survivor Relatives",
       custom.gof.rows = list("Prewar Region FEs" = c("Yes","Yes","Yes","Yes","Yes","Yes")),
       include.rsquared = F,
       custom.coef.map=list("treatmentempathy"="Treatment: Empathy",
                            "treatmentthreat"="Treatment: Threat",
                            "descendant_count"="N. Survivors",
                            "groupVictim"="Group Victimhood",
                            "personalVictim"="Self Victimhood",
                            "immigrant"="Immigrant",
                            "terrorAttack"="Terror Attack",
                            "combat"="Combat",
                            "ashkenazi"="Ashkenazi",
                            "logAge"="log(Age)",
                            "man"="Man",
                            "eduLevel"="Education",
                            "income"="Income",
                            "religiosity"="Religiosity",
                            "ideology"="Ideology",
                            "informed"="Pol. Interest",
                            "prewarWhiteCollar"="White Collar (prewar)",
                            "prewarRelig"="Religiosity (prewar)",
                            "prewarWealth"="Wealth (prewar)",
                            "(Intercept)"="Constant"),
       include.adjrs = T,
       digits = 2)

#############
# TABLE F.4 #
#############

mod1f <- lm(eritrean.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + descendant_2G + treatment + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed +
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data[data$exposed==1,])
mod2f <- lm(syrian.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + descendant_2G + treatment + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data[data$exposed==1,])
mod3f <- lm(westBank.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + descendant_2G + treatment + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data[data$exposed==1,])
mod4f <- lm(palestinian.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + descendant_2G + treatment + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data[data$exposed==1,])
mod5f <- lm(gazaExempt.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + descendant_2G + treatment + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data[data$exposed==1,])
mod6f <- lm(iranSanction.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + descendant_2G + treatment + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data[data$exposed==1,])

screenreg(list(mod1f,mod2f,mod3f,mod4f,mod5f,mod6f),
       custom.model.names=c("Africans","Syrians","West Bank","PCIs",
                            "Gazan Pal.","Iranians"),
       caption.above = TRUE,
       caption = "Alternative Predictor: Parent is a Survivor",
       custom.gof.rows = list("Prewar Region FEs" = c("Yes","Yes","Yes","Yes","Yes","Yes")),
       include.rsquared = F,
       custom.coef.map=list("treatmentempathy"="Treatment: Empathy",
                            "treatmentthreat"="Treatment: Threat",
                            "descendant_2G"="Survivor Parent",
                            "groupVictim"="Group Victimhood",
                            "personalVictim"="Self Victimhood",
                            "immigrant"="Immigrant",
                            "terrorAttack"="Terror Attack",
                            "combat"="Combat",
                            "ashkenazi"="Ashkenazi",
                            "logAge"="log(Age)",
                            "man"="Man",
                            "eduLevel"="Education",
                            "income"="Income",
                            "religiosity"="Religiosity",
                            "ideology"="Ideology",
                            "informed"="Pol. Interest",
                            "prewarWhiteCollar"="White Collar (prewar)",
                            "prewarRelig"="Religiosity (prewar)",
                            "prewarWealth"="Wealth (prewar)",
                            "(Intercept)"="Constant"),
       include.adjrs = T,
       digits = 2)

#############
# TABLE F.5 #
#############

data$descendant_women[is.na(data$descendant_women)] <- 0

mod1h <- lm(eritrean.x ~ descendant_women + terrorAttack + combat + ashkenazi + 
              logAge + man + treatment +
              eduLevel + income + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed +
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data)
mod2h <- lm(syrian.x ~ descendant_women + terrorAttack + combat + ashkenazi + 
              logAge + man + treatment +
              eduLevel + income + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data)
mod3h <- lm(westBank.x ~ descendant_women + terrorAttack + combat + ashkenazi + 
              logAge + man + treatment +
              eduLevel + income + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data)
mod4h <- lm(palestinian.x ~ descendant_women + terrorAttack + combat + ashkenazi + 
              logAge + man + treatment +
              eduLevel + income + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data)
mod5h <- lm(gazaExempt.x ~ descendant_women + terrorAttack + combat + ashkenazi + 
              logAge + man + treatment +
              eduLevel + income + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data)
mod6h <- lm(iranSanction.x ~ descendant_women + terrorAttack + combat + ashkenazi + 
              logAge + man + treatment +
              eduLevel + income + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data)

screenreg(list(mod1h,mod2h,mod3h,mod4h,mod5h,mod6h),
       custom.model.names=c("Africans","Syrians","West Bank","PCIs",
                            "Gazan Pal.","Iranians"),
       caption.above = TRUE,
       caption = "Alternative Predictor: Count of Women Survivors",
       custom.gof.rows = list("Prewar Region FEs" = c("Yes","Yes","Yes","Yes","Yes","Yes")),
       include.rsquared = F,
       custom.coef.map=list("treatmentempathy"="Treatment: Empathy",
                            "treatmentthreat"="Treatment: Threat",
                            "descendant_women"="Women Survivors",
                            "groupVictim"="Group Victimhood",
                            "personalVictim"="Self Victimhood",
                            "immigrant"="Immigrant",
                            "terrorAttack"="Terror Attack",
                            "combat"="Combat",
                            "ashkenazi"="Ashkenazi",
                            "logAge"="log(Age)",
                            "man"="Man",
                            "eduLevel"="Education",
                            "income"="Income",
                            "religiosity"="Religiosity",
                            "ideology"="Ideology",
                            "informed"="Pol. Interest",
                            "prewarWhiteCollar"="White Collar (prewar)",
                            "prewarRelig"="Religiosity (prewar)",
                            "prewarWealth"="Wealth (prewar)",
                            "(Intercept)"="Constant"),
       include.adjrs = T,
       digits = 2)

#############
# TABLE F.6 #
#############

mod1i <- lm(eritrean.x ~ talk + terrorAttack + combat + ashkenazi + 
              logAge + man + treatment +
              eduLevel + income + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed +
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data)
mod2i <- lm(syrian.x ~ talk + terrorAttack + combat + ashkenazi + 
              logAge + man + treatment +
              eduLevel + income + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed +
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data)
mod3i <- lm(westBank.x ~ talk + terrorAttack + combat + ashkenazi + 
              logAge + man + treatment +
              eduLevel + income + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data)
mod4i <- lm(palestinian.x ~ talk + terrorAttack + combat + ashkenazi + 
              logAge + man + treatment +
              eduLevel + income + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data)
mod5i <- lm(gazaExempt.x ~ talk + terrorAttack + combat + ashkenazi + 
              logAge + man + treatment +
              eduLevel + income + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data)
mod6i <- lm(iranSanction.x ~ talk + terrorAttack + combat + ashkenazi + 
              logAge + man + treatment +
              eduLevel + income + religiosity + 
              ideology + groupVictim + personalVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data)

screenreg(list(mod1i,mod2i,mod3i,mod4i,mod5i,mod6i),
       custom.model.names=c("Africans","Syrians","West Bank","PCIs",
                            "Gazan Pal.","Iranians"),
       caption.above = TRUE,
       caption = "Alternative Predictor: Discussions of Holocaust",
       custom.gof.rows = list("Prewar Region FEs" = c("Yes","Yes","Yes","Yes","Yes","Yes")),
       include.rsquared = F,
       custom.coef.map=list("treatmentempathy"="Treatment: Empathy",
                            "treatmentthreat"="Treatment: Threat",
                            "talk"="Discussion",
                            "groupVictim"="Group Victimhood",
                            "personalVictim"="Self Victimhood",
                            "immigrant"="Immigrant",
                            "terrorAttack"="Terror Attack",
                            "combat"="Combat",
                            "ashkenazi"="Ashkenazi",
                            "logAge"="log(Age)",
                            "man"="Man",
                            "eduLevel"="Education",
                            "income"="Income",
                            "religiosity"="Religiosity",
                            "ideology"="Ideology",
                            "informed"="Pol. Interest",
                            "prewarWhiteCollar"="White Collar (prewar)",
                            "prewarRelig"="Religiosity (prewar)",
                            "prewarWealth"="Wealth (prewar)",
                            "(Intercept)"="Constant"),
       include.adjrs = T,
       digits = 2)

#############
# TABLE F.7 #
#############

victModel <- lm(groupVictim ~ personalVictim + terrorAttack + combat + ashkenazi + 
                  logAge + man + eduLevel + income + exposed + religiosity + 
                  ideology + immigrant + as.factor(area) + informed + serveIDF, 
                data)
screenreg(victModel,
       caption.above = TRUE,
       caption = "Predicting Group Victimhood",
       include.rsquared = F,
       custom.coef.map=list("exposed"="Surv/Descendant",
                            "terrorAttack"="Terror Attack",
                            "combat"="Combat",
                            "serveIDF"="Served IDF",
                            "ashkenazi"="Ashkenazi",
                            "logAge"="log(Age)",
                            "man"="Man",
                            "eduLevel"="Education",
                            "income"="Income",
                            "religiosity"="Religiosity",
                            "ideology"="Ideology",
                            "informed"="Pol. Interest",
                            "immigrant"="Immigrant",
                            "personalVictim"="Self Victimhood",
                            "as.factor(area)Jerusalem area"="Jerusalem",
                            "as.factor(area)Judea and Samaria area"="Judea and Samaria",
                            "as.factor(area)Sharon area"="Sharon",
                            "as.factor(area)Southern area"="Southern",
                            "as.factor(area)Tel Aviv area"="Tel Aviv",
                            "(Intercept)"="Constant"),
       include.adjrs = T,
       digits = 2)

##############
# FIGURE F.3 #
##############

sensm1 <- sensemakr(model = exp1, 
                    treatment = "groupVictim",
                    benchmark_covariates = c("religiosity","ideology","eduLevel",
                                             "immigrant","personalVictim","informed"),
                    kd = 1:2)
sensm2 <- sensemakr(model = exp2, 
                    treatment = "groupVictim",
                    benchmark_covariates = c("religiosity","ideology","eduLevel",
                                             "immigrant","personalVictim","informed"),
                    kd = 1:2)
sensm3 <- sensemakr(model = exp3, 
                    treatment = "groupVictim",
                    benchmark_covariates = c("religiosity","ideology","eduLevel",
                                             "immigrant","personalVictim","informed"),
                    kd = 1:2)
sensm4 <- sensemakr(model = exp4, 
                    treatment = "groupVictim",
                    benchmark_covariates = c("religiosity","ideology","eduLevel",
                                             "immigrant","personalVictim","informed"),
                    kd = 1:2)
sensm5 <- sensemakr(model = exp5, 
                    treatment = "groupVictim",
                    benchmark_covariates = c("religiosity","ideology","eduLevel",
                                             "immigrant","personalVictim","informed"),
                    kd = 1:2)
sensm6 <- sensemakr(model = exp6, 
                    treatment = "groupVictim",
                    benchmark_covariates = c("religiosity","ideology","eduLevel",
                                             "immigrant","personalVictim","informed"),
                    kd = 1:2)
plot(sensm1, sensitivity.of = "t-value",label.text=TRUE,cex.label.text=0.5)
plot(sensm2, sensitivity.of = "t-value",label.text=TRUE,cex.label.text=0.5)
plot(sensm3, sensitivity.of = "t-value",label.text=TRUE,cex.label.text=0.5)
plot(sensm4, sensitivity.of = "t-value",label.text=TRUE,cex.label.text=0.5)
plot(sensm5, sensitivity.of = "t-value",label.text=TRUE,cex.label.text=0.5)
plot(sensm6, sensitivity.of = "t-value",label.text=TRUE,cex.label.text=0.5)

##############
# FIGURE F.4 #
##############

mod1k <- lm(eritrean.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + exposed + religiosity + immigrant + informed +
              ideology + personalVictim.x + jewishVictim.x + israelVictim.x +
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data.g)
mod2k <- lm(syrian.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + exposed + religiosity + immigrant + informed + 
              ideology + personalVictim.x + jewishVictim.x + israelVictim.x + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data.g)
mod3k <- lm(westBank.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + exposed + religiosity + immigrant + informed +
              ideology + personalVictim.x + jewishVictim.x + israelVictim.x + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data.g)
mod4k <- lm(palestinian.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + exposed + religiosity + immigrant + informed +
              ideology + personalVictim.x + jewishVictim.x + israelVictim.x + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data.g)
mod5k <- lm(gazaExempt.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + exposed + religiosity + immigrant + informed +
              ideology + personalVictim.x + jewishVictim.x + israelVictim.x + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data.g)
mod6k <- lm(iranSanction.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + exposed + religiosity + immigrant + informed +
              ideology + personalVictim.x + jewishVictim.x + israelVictim.x + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data.g)
plot_summs(mod1k,mod2k,mod3k,mod4k,mod5k,mod6k, plot.distributions = FALSE, 
           point.shape=TRUE, model.names=(c("Eritrean & Sudanese Refugees", 
                                            "Syrian Refugees", 
                                            "West Bank Palestinians", 
                                            "Palestinian Citizens of Israel", 
                                            "Palestinians from Gaza", "Iranians")), 
           coefs=c("Personal Victimhood"="personalVictim.x",
                   "Jewish Victimhood"="jewishVictim.x",
                   "Israeli Victimhood"="israelVictim.x"), 
           legend.title="Model Outgroup", colors=c(rep("black",6)), scale = T)

#############
# TABLE F.8 #
#############

data$lowPersonalVictim <- ifelse(!is.na(data$personalVictim) & data$personalVictim < 4.181,1,0)

mod1v <- lm(eritrean.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + exposed + treatment + religiosity + 
              ideology + groupVictim + immigrant + informed +
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data[data$lowPersonalVictim==1,])
mod2v <- lm(syrian.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + exposed + treatment + religiosity + 
              ideology + groupVictim + immigrant + informed +
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data[data$lowPersonalVictim==1,])
mod3v <- lm(westBank.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + exposed + treatment + religiosity + 
              ideology + groupVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data[data$lowPersonalVictim==1,])
mod4v <- lm(palestinian.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + exposed + treatment + religiosity + 
              ideology + groupVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data[data$lowPersonalVictim==1,])
mod5v <- lm(gazaExempt.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + exposed + treatment + religiosity + 
              ideology + groupVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data[data$lowPersonalVictim==1,])
mod6v <- lm(iranSanction.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
              eduLevel + income + exposed + treatment + religiosity + 
              ideology + groupVictim + immigrant + informed + 
              as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
              prewarWealth, data[data$lowPersonalVictim==1,])

screenreg(list(mod1v,mod2v,mod3v,mod4v,mod5v,mod6v),
       custom.model.names=c("Africans","Syrians","West Bank","PCIs",
                            "Gazan Pal.","Iranians"),
       caption.above = TRUE,
       caption = "Subgroup Analysis: Respondents Perceiving Low Personal Victimhood",
       custom.gof.rows = list("Prewar Region FEs" = c("Yes","Yes","Yes","Yes","Yes","Yes")),
       include.rsquared = F,
       custom.coef.map=list("treatmentempathy"="Treatment: Empathy",
                            "treatmentthreat"="Treatment: Threat",
                            "exposed"="Surv/Descendant",
                            "groupVictim"="Group Victimhood",
                            "terrorAttack"="Terror Attack",
                            "combat"="Combat",
                            "ashkenazi"="Ashkenazi",
                            "logAge"="log(Age)",
                            "man"="Man",
                            "eduLevel"="Education",
                            "income"="Income",
                            "religiosity"="Religiosity",
                            "ideology"="Ideology",
                            "immigrant"="Immigrant",
                            "informed"="Pol. Interest",
                            "prewarWhiteCollar"="White Collar (prewar)",
                            "prewarRelig"="Religiosity (prewar)",
                            "prewarWealth"="Wealth (prewar)",
                            "(Intercept)"="Constant"),
       include.adjrs = T,
       digits = 2)

#############
# TABLE F.9 #
#############

data$hawkDove <- ifelse(data$ideology %in% c(1:4),"Dove",
                        ifelse(data$ideology %in% c(5:7),"Hawk",NA))

in1 <- lm(eritrean.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
             eduLevel + income + exposed + treatment + religiosity + 
             groupVictim*ideology + immigrant + informed + personalVictim +
             as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
             prewarWealth, data)
in2 <- lm(syrian.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
             eduLevel + income + exposed + treatment + religiosity + 
             groupVictim*ideology + immigrant + informed + personalVictim +
             as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
             prewarWealth, data)
in3 <- lm(westBank.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
             eduLevel + income + exposed + treatment + religiosity + 
             groupVictim*ideology + immigrant + informed + personalVictim +
             as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
             prewarWealth, data)
in4 <- lm(palestinian.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
             eduLevel + income + exposed + treatment + religiosity + 
             groupVictim*ideology + immigrant + informed + personalVictim +
             as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
             prewarWealth, data)
in5 <- lm(gazaExempt.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
             eduLevel + income + exposed + treatment + religiosity + 
             groupVictim*ideology + immigrant + informed + personalVictim +
             as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
             prewarWealth, data)
in6 <- lm(iranSanction.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
             eduLevel + income + exposed + treatment + religiosity + 
             groupVictim*ideology + immigrant + informed + personalVictim +
             as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
             prewarWealth, data)
screenreg(list(in1,in2,in3,in4,in5,in6),
       custom.model.names=c("Africans","Syrians","West Bank","PCIs",
                            "Gazan Pal.","Iranians"),
       caption.above = TRUE,
       caption = "Interaction Between Ideology and Group Victimhood",
       custom.gof.rows = list("Prewar Region FEs" = c("Yes","Yes","Yes","Yes","Yes","Yes")),
       include.rsquared = F,
       custom.coef.map=list("treatmentempathy"="Treatment: Empathy",
                            "treatmentthreat"="Treatment: Threat",
                            "groupVictim:ideology"="Group Victimhood*Ideology",
                            "exposed"="Surv/Descendant",
                            "groupVictim"="Group Victimhood",
                            "terrorAttack"="Terror Attack",
                            "combat"="Combat",
                            "ashkenazi"="Ashkenazi",
                            "logAge"="log(Age)",
                            "man"="Man",
                            "eduLevel"="Education",
                            "income"="Income",
                            "religiosity"="Religiosity",
                            "ideology"="Ideology",
                            "immigrant"="Immigrant",
                            "informed"="Pol. Interest",
                            "prewarWhiteCollar"="White Collar (prewar)",
                            "prewarRelig"="Religiosity (prewar)",
                            "prewarWealth"="Wealth (prewar)",
                            "(Intercept)"="Constant"),
       include.adjrs = T,
       digits = 2)

##############
# TABLE F.10 #
##############

inter1 <- lm(eritrean.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
             eduLevel + income + exposed*ideology + treatment + religiosity + 
             groupVictim + immigrant + informed + personalVictim +
             as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
             prewarWealth, data)
inter2 <- lm(syrian.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
             eduLevel + income + exposed*ideology + treatment + religiosity + 
             groupVictim + immigrant + informed + personalVictim +
             as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
             prewarWealth, data)
inter3 <- lm(westBank.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
             eduLevel + income + exposed*ideology + treatment + religiosity + 
             groupVictim + immigrant + informed + personalVictim +
             as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
             prewarWealth, data)
inter4 <- lm(palestinian.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
             eduLevel + income + exposed*ideology + treatment + religiosity + 
             groupVictim + immigrant + informed + personalVictim +
             as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
             prewarWealth, data)
inter5 <- lm(gazaExempt.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
             eduLevel + income + exposed*ideology + treatment + religiosity + 
             groupVictim + immigrant + informed + personalVictim +
             as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
             prewarWealth, data)
inter6 <- lm(iranSanction.x ~ terrorAttack + combat + ashkenazi + logAge + man + 
             eduLevel + income + exposed*ideology + treatment + religiosity + 
             groupVictim + immigrant + informed + personalVictim +
             as.factor(prewarLoc) + prewarWhiteCollar + prewarRelig +
             prewarWealth, data)
screenreg(list(inter1,inter2,inter3,inter4,inter5,inter6),
       custom.model.names=c("Africans","Syrians","West Bank","PCIs",
                            "Gazan Pal.","Iranians"),
       caption.above = TRUE,
       caption = "Exploring Interaction Between Ideology and Group Victimhood",
       custom.gof.rows = list("Prewar Region FEs" = c("Yes","Yes","Yes","Yes","Yes","Yes")),
       include.rsquared = F,
       custom.coef.map=list("treatmentempathy"="Treatment: Empathy",
                            "treatmentthreat"="Treatment: Threat",
                            "exposed:ideology"="Surv/Descendant*Ideology",
                            "exposed"="Surv/Descendant",
                            "groupVictim"="Group Victimhood",
                            "terrorAttack"="Terror Attack",
                            "combat"="Combat",
                            "ashkenazi"="Ashkenazi",
                            "logAge"="log(Age)",
                            "man"="Man",
                            "eduLevel"="Education",
                            "income"="Income",
                            "religiosity"="Religiosity",
                            "ideology"="Ideology",
                            "immigrant"="Immigrant",
                            "informed"="Pol. Interest",
                            "prewarWhiteCollar"="White Collar (prewar)",
                            "prewarRelig"="Religiosity (prewar)",
                            "prewarWealth"="Wealth (prewar)",
                            "(Intercept)"="Constant"),
       include.adjrs = T,
       digits = 2)
