#American Diasporas, Homeland Human Rights Conduct, 
#and the Onset of Human Rights-Based Economic Sanctions

#Submission ID: 223175048.R2

#Jerry Urtuzuastigui

#For the code and materials needed to build the datasets
#used in this article, email gurtuzua@iu.edu.

#Set Working Directory
setwd("/Users/jerryurtz89/IIComplete")

#Load packages
library(interplot)
library(sandwich)
library(lmtest) 
library(plm)
library(AER)
library(stargazer)

#Datasets Used in Main Analysis (EUSANCT)
load("EsanctII.Rdata")
load("LsanctII.Rdata")

#Extended Sanctions Datasets used in Sensitivity Analysis (EUSANCT/TIES)
load("ELPMTIESII.Rdata")
load("LLPMTIESII.Rdata")
load("EGMMTIESII.Rdata")
load("LGMMTIESII.Rdata")

#Table 1
#############################
###Descriptive Statistics####
#############################
#In Latex (Executive)
newdata5 <- data.frame(newdata5)
stargazer(newdata5, omit.summary.stat = c("p25", "p75"))

#In Latex (Legislative)
newdata6 <- data.frame(newdata6)
stargazer(newdata6, omit.summary.stat = c("p25", "p75"))

#Table 2
##########################
###Executive Sanctions####
##########################

#Baseline Model 
lpm1<- lm(exec3 ~ lag.d_mean*lag.theta_mean2+cluster(ccode2)+
            factor(ccode2)+spell+spell2+spell3, data=newdata5)
coeftest(lpm1, vcov = vcovHC(lpm1, type = "HC1"))
lpm1.1<-coeftest(lpm1, vcov = vcovHC(lpm1, type = "HC1"))


#Full Model
lpm2<- lm(exec3 ~ lag.exec3+lag.d_mean*lag.theta_mean2
          +lag.atopally+lag.polity+lag.relativepower2+e+lag.ElectionYear
          +lag.approval+lag.humanrightsevents+cluster(ccode2)+factor(ccode2)
          +spell+spell2+spell3, data=newdata5)
coeftest(lpm2, vcov = vcovHC(lpm2, type = "HC1"))
lpm2.1<-coeftest(lpm2, vcov = vcovHC(lpm2, type = "HC1"))

############################
###Legislative Sanctions####
############################

#Baseline Model
lpm3<- lm(legis3 ~ lag.d_mean*lag.theta_mean2+cluster(ccode2)+
            factor(ccode2)+spell+spell2+spell3, data=newdata6)
coeftest(lpm3, vcov = vcovHC(lpm3, type = "HC1"))
lpm3.1<-coeftest(lpm3, vcov = vcovHC(lpm3, type = "HC1"))

#Full Model
lpm4<- lm(legis3 ~ lag.legis3+lag.d_mean*lag.theta_mean2
          +lag.atopally+lag.polity+lag.relativepower2+e+lag.ElectionYear
          +lag.approval+lag.humanrightsevents+cluster(ccode2)+factor(ccode2)
          ++spell+spell2+spell3, data=newdata6)
coeftest(lpm4, vcov = vcovHC(lpm4, type = "HC1"))
lpm4.1<- coeftest(lpm4, vcov = vcovHC(lpm4, type = "HC1"))

#Main Table
stargazer(lpm1.1, lpm2.1, lpm3.1, lpm4.1)

##################################
######## GMMs Models 5 - 8########
##################################
#The instruments used are valid and are chosen to 
#produce robust and fully specified models.

newdata7 <- newdata5[newdata5$year > 1989, ]  
newdata7 <- newdata7[newdata7$year < 2010, ]

newdata8 <- newdata6[newdata6$year > 1989, ] 
newdata8 <- newdata8[newdata8$year < 2010, ]

#Table 3
####################
###Executive GMMs###
####################

##Keep only variables of interest
full.dyad2 <- newdata7[,c("exec3","lag.exec3", "lag.d_mean", "lag.theta_mean2", 
                          "lag.atopally",  "lag.polity", "lag.relativepower2", "e",  
                          "ccode2","year", "lag.humanrightsevents","lag.r", "lag.v2x_api",
                          "e2", "lag.logdiaspora2", "logdiaspora")]

#Convert data to pgmm formal
pgmm.full.dyad2 <- pdata.frame(full.dyad2, index=c("ccode2", "year"))

#set seed
set.seed(50)

#Baseline
gmm.full.dyad1 <- pgmm(exec3 ~  lag.exec3+lag.d_mean*lag.theta_mean2| 
                         lag(exec3 , 2:10), 
                       data = pgmm.full.dyad2, 
                       effect = "twoway",
) 
summary(gmm.full.dyad1)

#Full
gmm.full.dyad2 <- pgmm(exec3 ~  lag.exec3+lag.d_mean*lag.theta_mean2
                       +lag.atopally+lag.polity+lag.relativepower2
                       +e+lag.humanrightsevents| 
                         lag(exec3 , 2:10), 
                       data = pgmm.full.dyad2, 
                       effect = "twoway") 
summary(gmm.full.dyad2, robust = T)

######################
###Legislative GMMs###
######################

##Keep only variables of interest
full.dyad <- newdata8[,c("legis3","lag.legis3", "lag.d_mean", "lag.theta_mean2", 
                         "lag.atopally",  "lag.polity", "lag.relativepower2", "e",  
                         "ccode2","year", "lag.humanrightsevents", "lag.r", "lag.v2x_api",
                         "e2", "lag.logdiaspora2", "logdiaspora")]

#Convert data to pgmm formal
pgmm.full.dyad <- pdata.frame(full.dyad, index=c("ccode2", "year"))

#set seed
set.seed(50)

#Baseline
gmm.full.dyad3 <- pgmm(legis3 ~  lag.legis3+lag.d_mean*lag.theta_mean2| 
                         lag(legis3 , 2:10), 
                       data = pgmm.full.dyad, 
                       effect = "twoway") 
summary(gmm.full.dyad3, robust = T)

#Full
gmm.full.dyad4 <- pgmm(legis3 ~  lag.legis3+lag.d_mean*lag.theta_mean2
                       +lag.atopally+lag.polity+lag.relativepower2
                       +e+lag.humanrightsevents| 
                         lag(legis3 , 2:10), 
                       data = pgmm.full.dyad, 
                       effect = "twoway") 
summary(gmm.full.dyad4, robust = T)

#GMM Table
stargazer(gmm.full.dyad1, gmm.full.dyad2, gmm.full.dyad3, gmm.full.dyad4)

######################################
####Interaction Plot (Full Models)####
######################################

#Figure 1. Executive
felm2.2<- lm(exec3 ~ lag.exec3+lag.d_mean*lag.theta_mean2
             +lag.atopally+lag.polity+lag.relativepower2+e+lag.ElectionYear
             +lag.approval+lag.humanrightsevents+factor(ccode2)
             +spell+spell2+spell3, data=newdata5)

interplot(m = felm2.2, var1 = "lag.theta_mean2", var2 = "lag.d_mean")+
  # Add labels for X and Y axes
  xlab("(log) Diaspora Size/Mean") +
  ylab("Estimated Coefficient for Target HR") +
  # Change the background
  theme_bw() +
  theme(plot.title = element_text(face="bold")) +
  # Add a horizontal line at y = 0
  #ylim(0,1) + ###Here’s the y definition. You can also use xlim(0,1) for x values
  geom_hline(yintercept = 0, linetype = "dashed")

#Figure 2 Legislative
felm4.2<- lm(legis3 ~ lag.legis3+lag.d_mean*lag.theta_mean2
             +lag.atopally+lag.polity+lag.relativepower2+e+lag.ElectionYear
             +lag.approval+lag.humanrightsevents+factor(ccode2)
             +spell+spell2+spell3, data=newdata6)

interplot(m = felm4.2, var1 = "lag.theta_mean2", var2 = "lag.d_mean")+
  # Add labels for X and Y axes
  xlab("(log) Diaspora Size/Mean") +
  ylab("Estimated Coefficient for Target HR") +
  # Change the background
  theme_bw() +
  theme(plot.title = element_text(face="bold")) +
  # Add a horizontal line at y = 0
  #ylim(0,1) + ###Here’s the y definition. You can also use xlim(0,1) for x values
  geom_hline(yintercept = 0, linetype = "dashed")

##############################
####Other Robustness Tests####
##############################

#Initial Value Carried Forward (TABLES 1 and 2)
#Executive LPM
lpmR1E<- lm(exec3 ~ lag.exec3+lag.logdiaspora2*lag.theta_mean2
            +lag.atopally+lag.polity+lag.relativepower2+e+lag.ElectionYear
            +lag.approval+lag.humanrightsevents+cluster(ccode2)+factor(ccode2)
            +spell+spell2+spell3, data=newdata5)
lpmR1E<-coeftest(lpmR1E, vcov = vcovHC(lpmR1E, type = "HC1"))

#Executive GMM
gmm.full.dyad5 <- pgmm(exec3 ~  lag.exec3+lag.logdiaspora2*lag.theta_mean2
                       +lag.atopally+lag.polity+lag.relativepower2
                       +e+lag.humanrightsevents| 
                         lag(exec3 , 2:10), 
                       data = pgmm.full.dyad2, 
                       effect = "twoway") 
summary(gmm.full.dyad5)

#Legislative LPM
lpmR1L<- lm(legis3 ~ lag.legis3+lag.logdiaspora2*lag.theta_mean2
            +lag.atopally+lag.polity+lag.relativepower2+e+lag.ElectionYear
            +lag.approval+lag.humanrightsevents+cluster(ccode2)+factor(ccode2)
            ++spell+spell2+spell3, data=newdata6)
lpmR1L<-coeftest(lpmR1L, vcov = vcovHC(lpmR1L, type = "HC1"))

#Legislative GMM
#Full
gmm.full.dyad6 <- pgmm(legis3 ~  lag.legis3+lag.logdiaspora2*lag.theta_mean2
                       +lag.atopally+lag.polity+lag.relativepower2
                       +e+lag.humanrightsevents| 
                         lag(legis3 , 2:10), 
                       data = pgmm.full.dyad, 
                       effect = "twoway") 
summary(gmm.full.dyad6, robust = T)

#Unlagged (TABLES 3 and 4)
#Executive LPM
lpmR2E<- lm(exec3 ~ lag.exec3+logdiaspora*lag.theta_mean2
            +lag.atopally+lag.polity+lag.relativepower2+e+lag.ElectionYear
            +lag.approval+lag.humanrightsevents+cluster(ccode2)+factor(ccode2)
            +spell+spell2+spell3, data=newdata5)
lpmR2E<-coeftest(lpmR2E, vcov = vcovHC(lpmR2E, type = "HC1"))

#Executive GMM
gmm.full.dyad7 <- pgmm(exec3 ~  lag.exec3+logdiaspora*lag.theta_mean2
                       +lag.atopally+lag.polity+lag.relativepower2
                       +e+lag.humanrightsevents| 
                         lag(exec3 , 2:10), 
                       data = pgmm.full.dyad2, 
                       effect = "twoway") 
summary(gmm.full.dyad7)

#Legislative LPM
lpmR2L<- lm(legis3 ~ lag.legis3+logdiaspora*lag.theta_mean2
            +lag.atopally+lag.polity+lag.relativepower2+e+lag.ElectionYear
            +lag.approval+lag.humanrightsevents+cluster(ccode2)+factor(ccode2)
            ++spell+spell2+spell3, data=newdata6)
lpmR2L<-coeftest(lpmR2L, vcov = vcovHC(lpmR2L, type = "HC1"))


#Legislative GMM
gmm.full.dyad8 <- pgmm(legis3 ~  lag.legis3+logdiaspora*lag.theta_mean2
                       +lag.atopally+lag.polity+lag.relativepower2
                       +e+lag.humanrightsevents| 
                         lag(legis3 , 2:10), 
                       data = pgmm.full.dyad, 
                       effect = "twoway") 
summary(gmm.full.dyad8, robust = T)

#Extended Sanctions Dataset (TIES/EUSANCT) (TABLES 5 and 6)
#Executive LPM
lpmR6E<- lm(exec ~ lag.exec+lag.d_mean*lag.theta_mean2
            +lag.atopally+lag.polity+lag.relativepower2+e+lag.ElectionYear
            +lag.approval+cluster(ccode2)+factor(ccode2)
            +spell+spell2+spell3, data=newdata10)
coeftest(lpmR6E, vcov = vcovHC(lpmR6E, type = "HC1"))
lpmR6E<-coeftest(lpmR6E, vcov = vcovHC(lpmR6E, type = "HC1"))

#Executive GMM
gmm.full.dyad15 <- pgmm(exec ~  lag.exec+lag.d_mean*lag.theta_mean2
                        +lag.atopally+lag.polity+lag.relativepower2
                        +e| 
                          lag(exec, 2:20), 
                        data = pgmm.full.dyad15, 
                        effect = "twoway") 
summary(gmm.full.dyad15, robust = T)

#Legislative LPM
lpmR6L<- lm(legis ~ lag.legis+lag.d_mean*lag.theta_mean2
            +lag.atopally+lag.polity+lag.relativepower2+e+lag.ElectionYear
            +lag.approval+cluster(ccode2)+factor(ccode2)
            ++spell+spell2+spell3, data=newdata11)
lpmR6L<- coeftest(lpmR6L, vcov = vcovHC(lpmR6L, type = "HC1"))

#Legislative GMM
gmm.full.dyad16 <- pgmm(legis ~  lag.legis+lag.d_mean*lag.theta_mean2
                        +lag.atopally+lag.polity+lag.relativepower2
                        +e| 
                          lag(legis , 2:6), 
                        data = pgmm.full.dyad16, 
                        effect = "twoway") 
summary(gmm.full.dyad16, robust = T)

#V-Dem (TABLES 7 and 8)
#Executive LPM
lpmR3E<- lm(exec3 ~ lag.exec3+lag.d_mean*lag.theta_mean2
            +lag.atopally+lag.v2x_api+lag.relativepower2+e+lag.ElectionYear
            +lag.approval+lag.humanrightsevents+cluster(ccode2)+factor(ccode2)
            +spell+spell2+spell3, data=newdata5)
lpmR3E<-coeftest(lpmR3E, vcov = vcovHC(lpmR3E, type = "HC1"))

#Executive GMM
gmm.full.dyad9 <- pgmm(exec3 ~  lag.exec3+lag.d_mean*lag.theta_mean2
                       +lag.atopally+lag.v2x_api+lag.relativepower2
                       +e+lag.humanrightsevents| 
                         lag(exec3 , 2:11), 
                       data = pgmm.full.dyad2, 
                       effect = "twoway") 
summary(gmm.full.dyad9)

#Legislative LPM
lpmR3L<- lm(legis3 ~ lag.legis3+lag.d_mean*lag.theta_mean2
            +lag.atopally+lag.v2x_api+lag.relativepower2+e+lag.ElectionYear
            +lag.approval+lag.humanrightsevents+cluster(ccode2)+factor(ccode2)
            ++spell+spell2+spell3, data=newdata6)
lpmR3L<-coeftest(lpmR3L, vcov = vcovHC(lpmR3L, type = "HC1"))

#Legislative GMM
gmm.full.dyad10 <- pgmm(legis3 ~  lag.legis3+lag.d_mean*lag.theta_mean2
                        +lag.atopally+lag.v2x_api+lag.relativepower2
                        +e+lag.humanrightsevents| 
                          lag(legis3 , 2:11), 
                        data = pgmm.full.dyad, 
                        effect = "twoway") 
summary(gmm.full.dyad10, robust = T)

#Alternative Relative Power (TABLES 9 and 10)
#Executive LPM
lpmR4E<- lm(exec3 ~ lag.exec3+lag.d_mean*lag.theta_mean2
            +lag.atopally+lag.polity+lag.r+e+lag.ElectionYear
            +lag.approval+lag.humanrightsevents+cluster(ccode2)+factor(ccode2)
            +spell+spell2+spell3, data=newdata5)
lpmR4E<-coeftest(lpmR4E, vcov = vcovHC(lpmR4E, type = "HC1"))

#Executive GMM
gmm.full.dyad11 <- pgmm(exec3 ~  lag.exec3+lag.d_mean*lag.theta_mean2
                        +lag.atopally+lag.polity+lag.r
                        +e+lag.humanrightsevents| 
                          lag(exec3 , 2:10), 
                        data = pgmm.full.dyad2, 
                        effect = "twoway") 
summary(gmm.full.dyad11)

#Legislative LPM
lpmR4L<- lm(legis3 ~ lag.legis3+lag.d_mean*lag.theta_mean2
            +lag.atopally+lag.polity+lag.r+e+lag.ElectionYear
            +lag.approval+lag.humanrightsevents+cluster(ccode2)+factor(ccode2)
            +spell+spell2+spell3, data=newdata6)
lpmR4L<-coeftest(lpmR4L, vcov = vcovHC(lpmR4L, type = "HC1"))

#Legislative GMM
gmm.full.dyad12 <- pgmm(legis3 ~  lag.legis3+lag.d_mean*lag.theta_mean2
                        +lag.atopally+lag.polity+lag.r
                        +e+lag.humanrightsevents| 
                          lag(legis3 , 2:10), 
                        data = pgmm.full.dyad, 
                        effect = "twoway") 
summary(gmm.full.dyad12, robust = T)

#Trade Dependence (TABLES 11 and 12)
#Executive LPM
lpmR5E<- lm(exec3 ~ lag.exec3+lag.d_mean*lag.theta_mean2
            +lag.atopally+lag.polity+lag.relativepower2+e2+lag.ElectionYear
            +lag.approval+lag.humanrightsevents+cluster(ccode2)+factor(ccode2)
            +spell+spell2+spell3, data=newdata5)
lpmR5E<-coeftest(lpmR5E, vcov = vcovHC(lpmR5E, type = "HC1"))

#Executive GMM
gmm.full.dyad13 <- pgmm(exec3 ~  lag.exec3+lag.d_mean*lag.theta_mean2
                        +lag.atopally+lag.polity+lag.relativepower2
                        +e2+lag.humanrightsevents| 
                          lag(exec3 , 2:10), 
                        data = pgmm.full.dyad2, 
                        effect = "twoway") 
summary(gmm.full.dyad13)

#Legislative
lpmR5L<- lm(legis3 ~ lag.legis3+lag.d_mean*lag.theta_mean2
            +lag.atopally+lag.polity+lag.relativepower2+e2+lag.ElectionYear
            +lag.approval+lag.humanrightsevents+cluster(ccode2)+factor(ccode2)
            ++spell+spell2+spell3, data=newdata6)
lpmR5L<-coeftest(lpmR5L, vcov = vcovHC(lpmR5L, type = "HC1"))

#Legislative GMM
gmm.full.dyad14 <- pgmm(legis3 ~  lag.legis3+lag.d_mean*lag.theta_mean2
                        +lag.atopally+lag.polity+lag.relativepower2
                        +e2+lag.humanrightsevents| 
                          lag(legis3 , 2:10), 
                        data = pgmm.full.dyad, 
                        effect = "twoway") 
summary(gmm.full.dyad14, robust = T)

#################################
####Tables in Online Appendix####
#################################

# Tables 1 and 2 Robustness (LPM and GMM)
#Initial Value CF
stargazer(lpmR1E, lpmR1L)
stargazer(gmm.full.dyad5, gmm.full.dyad6)

#Tables 3 and 4 Unlagged
stargazer(lpmR2E, lpmR2L)
stargazer(gmm.full.dyad7, gmm.full.dyad8)

#Tables 5 and 6 Extended Sanctions Dataset (TIES/EUSANCT)
stargazer(lpmR6E, lpmR6L)
stargazer(gmm.full.dyad15, gmm.full.dyad16)

#Tables 7 and 8 V-Dem
stargazer(lpmR3E, lpmR3L)
stargazer(gmm.full.dyad9, gmm.full.dyad10)

#Tables 9 and 10 Alt. Rel. Power
stargazer(lpmR4E, lpmR4L)
stargazer(gmm.full.dyad11, gmm.full.dyad12)

#Tables 11 and 12 Trade Dependence
stargazer(lpmR5E, lpmR5L)
stargazer(gmm.full.dyad13, gmm.full.dyad14)