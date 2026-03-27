####################################################################
## author:    Michael L. Wicki
## contact:   michael.wicki@istp.ethz.ch, ETH Zurich
## file name: analysisW2.R
## Context:   Analysis Neuhausen Single Authored
## started:   2019-02-25
## summary:   Analysis of the Diff-in-Diff effect over the three waves
####################################################################


rm(list = ls())
getwd()
setwd("\\\\d.ethz.ch\\groups\\baug\\irl\\spur_group\\02_People\\Wicki Michael\\Autonomous_Driving\\analysis_paper")
set.seed(42)

################################ load packages ################################ 
library(dplyr)
library(Hmisc)
require(memisc)
library(corrplot)
library(PerformanceAnalytics)
library(ggplot2)
require(scales)
library(reshape2)
library(dotwhisker)
library(broom)
library(car)
library(knitr)
library(ggthemes)
library(tidyr)
library(plotrix)
library(psych)
library(cowplot)
library(readxl)

################################ load dataset ################################ 
load("DiffInDiff_replication.RData")
df$ageGr <- ifelse(df$age <=34, "18-34", ifelse(
  df$age > 34 & df$age <= 44, "35-44", ifelse(
    df$age > 44 & df$age <= 54, "45-54", ifelse(
      df$age > 54 & df$age <= 64, "55-64", ifelse(
        df$age > 64, "65+", NA)))))
table(df$munic, df$ageGr, df$male)
df$income <- as.factor(ifelse(df$inc1 == "inc1Weiss nicht", "NA", ifelse(
  is.na(df$inc1), "income unknown", as.character(df$inc1))))

weights <- read_excel("Z:/02_People/Wicki Michael/Autonomous_Driving/analysis_paper/weights.xlsx")

df$weight <- ifelse(df$munic=="Neuhausen am Rheinfall" & df$ageGr == "18-34" & df$male == 0, weights[1,]$Weight, ifelse(
  df$munic=="Stein am Rhein" & df$ageGr == "18-34" & df$male == 0, weights[2,]$Weight, ifelse(
    df$munic=="Thayngen" & df$ageGr == "18-34" & df$male == 0, weights[3,]$Weight, ifelse(
      df$munic=="Neuhausen am Rheinfall" & df$ageGr == "18-34" & df$male == 1, weights[4,]$Weight, ifelse(
        df$munic=="Stein am Rhein" & df$ageGr == "18-34" & df$male == 1, weights[5,]$Weight, ifelse(
          df$munic=="Thayngen" & df$ageGr == "18-34" & df$male == 1, weights[6,]$Weight, ifelse(
            df$munic=="Neuhausen am Rheinfall" & df$ageGr == "35-44" & df$male == 0, weights[7,]$Weight, ifelse(
              df$munic=="Stein am Rhein" & df$ageGr == "35-44" & df$male == 0, weights[8,]$Weight, ifelse(
                df$munic=="Thayngen" & df$ageGr == "35-44" & df$male == 0, weights[9,]$Weight, ifelse(
                  df$munic=="Neuhausen am Rheinfall" & df$ageGr == "35-44" & df$male == 1, weights[10,]$Weight, ifelse(
                    df$munic=="Stein am Rhein" & df$ageGr == "35-44" & df$male == 1, weights[11,]$Weight, ifelse(
                      df$munic=="Thayngen" & df$ageGr == "35-44" & df$male == 1, weights[12,]$Weight, ifelse(
                        df$munic=="Neuhausen am Rheinfall" & df$ageGr == "45-54" & df$male == 0, weights[13,]$Weight, ifelse(
                          df$munic=="Stein am Rhein" & df$ageGr == "45-54" & df$male == 0, weights[14,]$Weight, ifelse(
                            df$munic=="Thayngen" & df$ageGr == "45-54" & df$male == 0, weights[15,]$Weight, ifelse(
                              df$munic=="Neuhausen am Rheinfall" & df$ageGr == "45-54" & df$male == 1, weights[16,]$Weight, ifelse(
                                df$munic=="Stein am Rhein" & df$ageGr == "45-54" & df$male == 1, weights[17,]$Weight, ifelse(
                                  df$munic=="Thayngen" & df$ageGr == "45-54" & df$male == 1, weights[18,]$Weight, ifelse(
                                    df$munic=="Neuhausen am Rheinfall" & df$ageGr == "55-64" & df$male == 0, weights[19,]$Weight, ifelse(
                                      df$munic=="Stein am Rhein" & df$ageGr == "55-64" & df$male == 0, weights[20,]$Weight, ifelse(
                                        df$munic=="Thayngen" & df$ageGr == "55-64" & df$male == 0, weights[21,]$Weight, ifelse(
                                          df$munic=="Neuhausen am Rheinfall" & df$ageGr == "55-64" & df$male == 1, weights[22,]$Weight, ifelse(
                                            df$munic=="Stein am Rhein" & df$ageGr == "55-64" & df$male == 1, weights[23,]$Weight, ifelse(
                                              df$munic=="Thayngen" & df$ageGr == "55-64" & df$male == 1, weights[24,]$Weight, ifelse(
                                                df$munic=="Neuhausen am Rheinfall" & df$ageGr == "65+" & df$male == 0, weights[25,]$Weight, ifelse(
                                                  df$munic=="Stein am Rhein" & df$ageGr == "65+" & df$male == 0, weights[26,]$Weight, ifelse(
                                                    df$munic=="Thayngen" & df$ageGr == "65+" & df$male == 0, weights[27,]$Weight, ifelse(
                                                      df$munic=="Neuhausen am Rheinfall" & df$ageGr == "65+" & df$male == 1, weights[28,]$Weight, ifelse(
                                                        df$munic=="Stein am Rhein" & df$ageGr == "65+" & df$male == 1, weights[29,]$Weight, ifelse(
                                                          df$munic=="Thayngen" & df$ageGr == "65+" & df$male == 1, weights[30,]$Weight, NA
))))))))))))))))))))))))))))))


###### assign weights ########


##### Prediction #####
##### Before test ######
m1 <- lm(chgStVO ~ avcSysSc + avcSysRl + avcDrCtr + avcJoblos + avcAcci + avcPriv + avcHack + avCntct + risk + age + male + educ1 + educ2 + educ0 + dummyNeu.y + inc1, data = df, na.action=na.exclude, weights=weight)
m2 <- lm(avNoCrn ~ avcSysSc + avcSysRl + avcDrCtr + avcJoblos + avcAcci + avcPriv + avcHack + avCntct + risk + age + male + educ1 + educ2 + educ0 + dummyNeu.y + inc1, data = df, na.action=na.exclude, weights=weight)
m3 <- lm(avTests ~ avcSysSc + avcSysRl + avcDrCtr + avcJoblos + avcAcci + avcPriv + avcHack + avCntct + risk + age + male + educ1 + educ2 + educ0 + dummyNeu.y + inc1, data = df, na.action=na.exclude, weights=weight)

summary(m1)
summary(m2)
summary(m3)

library(stargazer)
table_beforetest <- stargazer(m1, m2, m3,
                      type="html", 
                      single.row = TRUE,
                      title="Table A: Predictors of self-driving vehicle attitudes before test (linear regression)", 
                      align=TRUE,
                      star.char = c("+", "*", "**", "***"),
                      star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
                      digits = 3,
                      dep.var.labels = c("Legal adaptation support", "Confidence in self-driving vehicles", "Self-driving vehicle test support"),
                      order=c(1,2,3,4,5,6,7,8,9,10,11,12,15,13,14,16,17),
                      covariate.labels = c("System safety concern", "System reliability concern", "Driver control loss concern", 
                                           "Job loss concern", "Liability concern",
                                           "Privacy concern", "Software misuse concern", "Self-driving vehicle contact", 
                                           "Willingness to take risks", "Age", "Male dummy",
                                           "Minimum education", "Secondary education", "Tertiary education",
                                           "Test municipality", "Income unknown", "Below CHF 2000", "CHF 2000-4000", "CHF 4001-6000", "CHF 6001-8000", 
                                           "CHF 8001-10000", "CHF 10001-12000", "CHF 12001-14000", "CHF 14001-16000", "More than CHF 16000",
                                           "Intercept"),
                      object.names = FALSE,
                      notes.append = FALSE, notes.align = "l",
                      notes = "Table entries are ordinary least square coefficient estimates with estimated standard errors. Observations are weighted based on the true distribution of age, gender, and municipality. ***p < 0.001, **p < 0.01, *p < 0.05, +p < 0.1",
                      out='tabs_weight/table_beforetest.html')

m2plot1 <- dwplot(list(m1, m2, m3), show_intercept = FALSE) %>% 
  relabel_predictors(c(avcSysSc = "System safety concern",                       
                       avcSysRl = "System reliability concern", 
                       avcDrCtr = "Driver control loss concern", 
                       avcJoblos = "Job loss concern", 
                       avcJoblos = "Job loss concern", 
                       avcAcci = "Liability concern",
                       avcPriv = "Privacy concern",
                       avcHack = "Software misuse concern",
                       avCntct = "Self-driving vehicle contact", 
                       risk = "Willingness to take risks", 
                       age = "Age", 
                       male = "Male dummy",
                       educ0 = "Minimum education",
                       educ1 = "Secondary education",
                       educ2 = "Tertiary education",
                       dummyNeu.y = "Test municipality")) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("Predictors of self-driving vehicle attitudes wave 1") +
  theme(plot.title = element_text(face="bold"),
        legend.justification = c(0, 0), 
        legend.background = element_rect(colour="grey80")) +
  scale_colour_manual(name="Dependent Variable",
                      breaks=c("Model 1", "Model 2", "Model 3"),
                      labels=c("Legal adaptation support", "Confidence in self-driving vehicles", "Self-driving vehicle test support"),
                      values=c("#e66101","#5e3c99","#b2abd2")) +
  theme(legend.title=element_text(size=12, family="Sans"), axis.text=element_text(size=12, family="Sans"), axis.title=element_text(size=16, family="Sans"), 
        axis.text.x = element_text(size=12, family="Sans"), panel.grid.minor = element_blank(),
        legend.text = element_text(size=12, family="Sans"), plot.title = element_text(family="Sans"), legend.position = "bottom",
        legend.direction = "horizontal") +
  guides(col=guide_legend(nrow=3,byrow=TRUE))
m2plot1

ggsave(filename = "./plots_weight/predictionBefore.png", m2plot1, width = 8, height = 8)


##### Difference wave 1 - wave 2 ######
df$DchgStV0 <- df$w2chgStVO - df$chgStVO
df$DavCncrn <- df$w2avCncrn - df$avNoCrn
df$DavTests <- df$w2avTests - df$avTests
df$DavcPriv <- df$w2avcPriv - df$avcPriv
df$DavcHack <- df$w2avcHack - df$avcHack
df$DavcAcci <- df$w2avcAcci - df$avcAcci
df$DavcSysSc <- df$w2avcSysSc - df$avcSysSc
df$DavcSysRl <- df$w2avcSysRl - df$avcSysRl
df$DavcDrCtr <- df$w2avcDrCtr - df$avcDrCtr
df$DavcJoblos <- df$w2avcJoblos - df$avcJoblos
df$DavCntct <- df$w2avCntct - df$avCntct

m4 <- lm(DchgStV0 ~ DavcSysSc + DavcSysRl + DavcDrCtr + DavcJoblos + DavcAcci + DavcPriv + DavcHack + DavCntct + risk + age+ male + educ1 + educ2 + educ0 + dummyNeu.y + inc1, data = df, na.action=na.exclude, weights=weight)
m5 <- lm(DavCncrn ~ DavcSysSc + DavcSysRl + DavcDrCtr + DavcJoblos + DavcAcci + DavcPriv + DavcHack + DavCntct + risk + age+ male + educ1 + educ2 + educ0 + dummyNeu.y + inc1, data = df, na.action=na.exclude, weights=weight)
m6 <- lm(DavTests ~ DavcSysSc + DavcSysRl + DavcDrCtr + DavcJoblos + DavcAcci + DavcPriv + DavcHack + DavCntct + risk + age+ male + educ1 + educ2 + educ0 + dummyNeu.y + inc1, data = df, na.action=na.exclude, weights=weight)

summary(m4)
summary(m5)
summary(m6)

table_difference <- stargazer(m4, m5, m6,
                             type="html", 
                             single.row = TRUE,
                             title="Table A: Predictors of self-driving vehicle attitude change from wave 1 to wave 2 (linear regression)", 
                             align=TRUE,
                             star.char = c("+", "*", "**", "***"),
                             star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
                             digits = 3,
                             dep.var.labels = c("Legal adaptation support", "Confidence in self-driving vehicles", "Self-driving vehicle test support"),
                             order=c(1,2,3,4,5,6,7,8,9,10,11,12,15,13,14,16,17),
                             covariate.labels = c("System safety concern", "System reliability concern", "Driver control loss concern", 
                                                  "Job loss concern", "Liability concern",
                                                  "Privacy concern", "Software misuse concern", "Self-driving vehicle contact", 
                                                  "Willingness to take risks", "Age", "Male dummy",
                                                  "Minimum education", "Secondary education", "Tertiary education",
                                                  "Test municipality", "Income unknown", "Below CHF 2000", "CHF 2000-4000", "CHF 4001-6000", "CHF 6001-8000", 
                                                  "CHF 8001-10000", "CHF 10001-12000", "CHF 12001-14000", "CHF 14001-16000", "More than CHF 16000",
                                                  "Intercept"),
                             object.names = FALSE,
                             notes.append = FALSE, notes.align = "l",
                             notes = "Table entries are ordinary least square coefficient estimates with estimated standard errors. Observations are weighted based on the true distribution of age, gender, and municipality. ***p < 0.001, **p < 0.01, *p < 0.05, +p < 0.1",
                             out='tabs_weight/table_differencew1w2.html')

m2plot3 <- dwplot(list(m4, m5, m6), show_intercept = FALSE) %>% 
  relabel_predictors(c(DavcSysSc = "System safety concern",                       
                       DavcSysRl = "System reliability concern", 
                       DavcDrCtr = "Driver control loss concern", 
                       DavcJoblos = "Job loss concern", 
                       DavcAcci = "Liability concern",
                       DavcPriv = "Privacy concern",
                       DavcHack = "Software misuse concern",
                       DavCntct = "Self-driving vehicle contact", 
                       risk = "Willingness to take risks", 
                       age = "Age", 
                       male = "Male dummy",
                       educ0 = "Minimum education",
                       educ1 = "Secondary education",
                       educ2 = "Tertiary education",
                       dummyNeu.y = "Test municipality")) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("Self-driving vehicle Support Prediction Difference (wave 2 - wave 1)") +
  theme(plot.title = element_text(face="bold"),
        legend.justification = c(0, 0), 
        legend.background = element_rect(colour="grey80"),
        legend.position = "bottom",
        axis.text.x = element_text(size=12, family="Sans"),
        legend.text = element_text(size=12)) +
  scale_colour_manual(name="Dependent Variable",
                      breaks=c("Model 1", "Model 2", "Model 3"),
                      labels=c("Legal adaptation support", "Confidence in self-driving vehicles", "Self-driving vehicle test support"),
                      values=c("#e66101","#5e3c99","#b2abd2"))

ggsave(filename = "./plots_weight/predictionDifferencew1w2.png", m2plot3, width = 10, height = 10)


##### Difference wave 1 - wave 3 ######
df$DchgStV0 <- df$w3chgStVO - df$chgStVO
df$DavCncrn <- df$w3avCncrn - df$avNoCrn
df$DavTests <- df$w3avTests - df$avTests
df$DavcPriv <- df$w3avcPriv - df$avcPriv
df$DavcHack <- df$w3avcHack - df$avcHack
df$DavcAcci <- df$w3avcAcci - df$avcAcci
df$DavcSysSc <- df$w3avcSysSc - df$avcSysSc
df$DavcSysRl <- df$w3avcSysRl - df$avcSysRl
df$DavcDrCtr <- df$w3avcDrCtr - df$avcDrCtr
df$DavcJoblos <- df$w3avcJoblos - df$avcJoblos
df$DavCntct <- df$w3avCntct - df$avCntct

m7 <- lm(DchgStV0 ~ DavcSysSc + DavcSysRl + DavcDrCtr + DavcJoblos + DavcAcci + DavcPriv + DavcHack + DavCntct + risk + age+ male + educ1 + educ2 + educ0 + dummyNeu.y + inc1, data = df, na.action=na.exclude, weights=weight)
m8 <- lm(DavCncrn ~ DavcSysSc + DavcSysRl + DavcDrCtr + DavcJoblos + DavcAcci + DavcPriv + DavcHack + DavCntct + risk + age+ male + educ1 + educ2 + educ0 + dummyNeu.y + inc1, data = df, na.action=na.exclude, weights=weight)
m9 <- lm(DavTests ~ DavcSysSc + DavcSysRl + DavcDrCtr + DavcJoblos + DavcAcci + DavcPriv + DavcHack + DavCntct + risk + age+ male + educ1 + educ2 + educ0 + dummyNeu.y + inc1, data = df, na.action=na.exclude, weights=weight)

summary(m7)
summary(m8)
summary(m9)

table_difference <- stargazer(m7, m8, m9,
                              type="html", 
                              single.row = TRUE,
                              title="Table A: Predictors of self-driving vehicle attitude change from wave 1 to wave 3 (linear regression)", 
                              align=TRUE,
                              star.char = c("+", "*", "**", "***"),
                              star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
                              digits = 3,
                              dep.var.labels = c("Legal adaptation support", "Confidence in self-driving vehicles", "Self-driving vehicle test support"),
                              order=c(1,2,3,4,5,6,7,8,9,10,11,12,15,13,14,16,17),
                              covariate.labels = c("System safety concern", "System reliability concern", "Driver control loss concern", 
                                                   "Job loss concern", "Liability concern",
                                                   "Privacy concern", "Software misuse concern", "Self-driving vehicle contact", 
                                                   "Willingness to take risks", "Age", "Male dummy",
                                                   "Minimum education", "Secondary education", "Tertiary education",
                                                   "Test municipality", "Income unknown", "Below CHF 2000", "CHF 2000-4000", "CHF 4001-6000", "CHF 6001-8000", 
                                                   "CHF 8001-10000", "CHF 10001-12000", "CHF 12001-14000", "CHF 14001-16000", "More than CHF 16000",
                                                   "Intercept"),
                              object.names = FALSE,
                              notes.append = FALSE, notes.align = "l",
                              notes = "Table entries are ordinary least square coefficient estimates with estimated standard errors. Observations are weighted based on the true distribution of age, gender, and municipality. ***p < 0.001, **p < 0.01, *p < 0.05, +p < 0.1",
                              out='tabs_weight/table_differencew1w3.html')

m2plot4 <- dwplot(list(m7, m8, m9), show_intercept = FALSE) %>% 
  relabel_predictors(c(DavcSysSc = "System safety concern",                       
                       DavcSysRl = "System reliability concern", 
                       DavcDrCtr = "Driver control loss concern", 
                       DavcJoblos = "Job loss concern", 
                       DavcAcci = "Liability concern",
                       DavcPriv = "Privacy concern",
                       DavcHack = "Software misuse concern",
                       DavCntct = "Self-driving vehicle contact", 
                       risk = "Willingness to take risks", 
                       age = "Age", 
                       male = "Male dummy",
                       educ0 = "Minimum education",
                       educ1 = "Secondary education",
                       educ2 = "Tertiary education",
                       dummyNeu.y = "Test municipality")) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("Self-driving vehicle Support Prediction Difference (wave 3 - wave 1)") +
  theme(plot.title = element_text(face="bold"),
        legend.justification = c(0, 0), 
        legend.background = element_rect(colour="grey80"),
        legend.position = "bottom",
        axis.text.x = element_text(size=12, family="Sans")) +
  scale_colour_manual(name="Dependent Variable",
                      breaks=c("Model 1", "Model 2", "Model 3"),
                      labels=c("Legal adaptation support", "Confidence in self-driving vehicles", "Self-driving vehicle test support"),
                      values=c("#e66101","#5e3c99","#b2abd2"))

ggsave(filename = "./plots_weight/predictionDifferencew1w3.png", m2plot3, width = 10, height = 10)

#joint plot
legend <- get_legend(m2plot3)
left <- m2plot1 + theme(legend.position="none") +
  ggtitle("Before test (wave 1)") + xlab("")

middle <- m2plot3 + theme(axis.text.y = element_blank(), legend.position="none") +
  ggtitle("Change from wave 1 to wave 2") + xlab("Coefficient Estimates")
  
right <- m2plot4 + theme(axis.text.y = element_blank(), legend.position = "none")+
  ggtitle("Change from wave 1 to wave 3") + xlab("")

fig_joint <- ggdraw() +
  draw_plot(left, x = 0.0025, y = 0.093, width = 0.45, height = 0.907) +
  draw_plot(middle, x = 0.45, y = 0.1, width = 0.275, height = 0.9) +
  draw_plot(right, x = 0.725, y = 0.1, width = 0.275, height = 0.9) +
    draw_plot(legend, x = 0.2, y = 0.02, width = 1, height = 0.1)
fig_joint

ggsave(filename = "./plots_weight/fig_joint.png", fig_joint, width = 12, height = 6)


##### Prediction Interactions #####
##### Before test ######
mm1 <- lm(chgStVO ~ (avcSysSc + avcSysRl + avcDrCtr + avcJoblos + avcAcci + avcPriv + avcHack + avCntct) * dummyNeu.y, data = df, na.action=na.exclude, weights=weight)
mm2 <- lm(avNoCrn ~ (avcSysSc + avcSysRl + avcDrCtr + avcJoblos + avcAcci + avcPriv + avcHack + avCntct) * dummyNeu.y, data = df, na.action=na.exclude, weights=weight)
mm3 <- lm(avTests ~ (avcSysSc + avcSysRl + avcDrCtr + avcJoblos + avcAcci + avcPriv + avcHack + avCntct) * dummyNeu.y, data = df, na.action=na.exclude, weights=weight)

summary(mm1)
summary(mm2)
summary(mm3)

library(stargazer)
table_beforetest_interact <- stargazer(mm1, mm2, mm3,
                              type="html", 
                              single.row = TRUE,
                              title="Table A: Predictors of self-driving vehicle attitudes before test (linear regression)", 
                              align=TRUE,
                              star.char = c("+", "*", "**", "***"),
                              star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
                              digits = 3,
                              dep.var.labels = c("Legal adaptation support", "Confidence in self-driving vehicles", "Self-driving vehicle test support"),
                              order=c(1,2,3,4,5,6,7,8,9,10,11,12,15,13,14,16,17),
                              covariate.labels = c("System safety concern", "System reliability concern", "Driver control loss concern", 
                                                   "Job loss concern", "Liability concern",
                                                   "Privacy concern", "Software misuse concern", "Self-driving vehicle contact", 
                                                   "Willingness to take risks", "Age", "Male dummy",
                                                   "Minimum education", "Secondary education", "Tertiary education",
                                                   "Test municipality", "Income unknown", "Below CHF 2000", "CHF 2000-4000", "CHF 4001-6000", "CHF 6001-8000", 
                                                   "CHF 8001-10000", "CHF 10001-12000", "CHF 12001-14000", "CHF 14001-16000", "More than CHF 16000",
                                                   "Intercept"),
                              object.names = FALSE,
                              notes.append = FALSE, notes.align = "l",
                              notes = "Table entries are ordinary least square coefficient estimates with estimated standard errors. Observations are weighted based on the true distribution of age, gender, and municipality. ***p < 0.001, **p < 0.01, *p < 0.05, +p < 0.1",
                              out='tabs_weight/table_beforetest_interact.html')


##### Difference wave 1 - wave 2 ######
df$DchgStV0 <- df$w2chgStVO - df$chgStVO
df$DavCncrn <- df$w2avCncrn - df$avNoCrn
df$DavTests <- df$w2avTests - df$avTests
df$DavcPriv <- df$w2avcPriv - df$avcPriv
df$DavcHack <- df$w2avcHack - df$avcHack
df$DavcAcci <- df$w2avcAcci - df$avcAcci
df$DavcSysSc <- df$w2avcSysSc - df$avcSysSc
df$DavcSysRl <- df$w2avcSysRl - df$avcSysRl
df$DavcDrCtr <- df$w2avcDrCtr - df$avcDrCtr
df$DavcJoblos <- df$w2avcJoblos - df$avcJoblos
df$DavCntct <- df$w2avCntct - df$avCntct

mm4 <- lm(DchgStV0 ~ (DavcSysSc + DavcSysRl + DavcDrCtr + DavcJoblos + DavcAcci + DavcPriv + DavcHack + DavCntct) * dummyNeu.y, data = df, na.action=na.exclude, weights=weight)
mm5 <- lm(DavCncrn ~ (DavcSysSc + DavcSysRl + DavcDrCtr + DavcJoblos + DavcAcci + DavcPriv + DavcHack + DavCntct) * dummyNeu.y, data = df, na.action=na.exclude, weights=weight)
mm6 <- lm(DavTests ~ (DavcSysSc + DavcSysRl + DavcDrCtr + DavcJoblos + DavcAcci + DavcPriv + DavcHack + DavCntct) * dummyNeu.y, data = df, na.action=na.exclude, weights=weight)

summary(mm4)
summary(mm5)
summary(mm6)

table_difference_interact <- stargazer(mm4, mm5, mm6,
                              type="html", 
                              single.row = TRUE,
                              title="Table A: Predictors of self-driving vehicle attitude change from wave 1 to wave 2 (linear regression)", 
                              align=TRUE,
                              star.char = c("+", "*", "**", "***"),
                              star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
                              digits = 3,
                              dep.var.labels = c("Legal adaptation support", "Confidence in self-driving vehicles", "Self-driving vehicle test support"),
                              order=c(1,2,3,4,5,6,7,8,9,10,11,12,15,13,14,16,17),
                              covariate.labels = c("System safety concern", "System reliability concern", "Driver control loss concern", 
                                                   "Job loss concern", "Liability concern",
                                                   "Privacy concern", "Software misuse concern", "Self-driving vehicle contact",
                                                   "Test municipality", "System safety concern*Test municipality", "System reliability concern*Test municipality", "Driver control loss concern*Test municipality", 
                                                   "Job loss concern*Test municipality", "Liability concern*Test municipality",
                                                   "Privacy concern*Test municipality", "Software misuse concern*Test municipality", "Self-driving vehicle contact*Test municipality"),
                              object.names = FALSE,
                              notes.append = FALSE, notes.align = "l",
                              notes = "Table entries are ordinary least square coefficient estimates with estimated standard errors. Observations are weighted based on the true distribution of age, gender, and municipality. ***p < 0.001, **p < 0.01, *p < 0.05, +p < 0.1",
                              out='tabs_weight/table_differencew1w2_interact.html')


##### Difference wave 1 - wave 3 ######
df$DchgStV0 <- df$w3chgStVO - df$chgStVO
df$DavCncrn <- df$w3avCncrn - df$avNoCrn
df$DavTests <- df$w3avTests - df$avTests
df$DavcPriv <- df$w3avcPriv - df$avcPriv
df$DavcHack <- df$w3avcHack - df$avcHack
df$DavcAcci <- df$w3avcAcci - df$avcAcci
df$DavcSysSc <- df$w3avcSysSc - df$avcSysSc
df$DavcSysRl <- df$w3avcSysRl - df$avcSysRl
df$DavcDrCtr <- df$w3avcDrCtr - df$avcDrCtr
df$DavcJoblos <- df$w3avcJoblos - df$avcJoblos
df$DavCntct <- df$w3avCntct - df$avCntct

mm7 <- lm(DchgStV0 ~ (DavcSysSc + DavcSysRl + DavcDrCtr + DavcJoblos + DavcAcci + DavcPriv + DavcHack + DavCntct) * dummyNeu.y, data = df, na.action=na.exclude, weights=weight)
mm8 <- lm(DavCncrn ~ (DavcSysSc + DavcSysRl + DavcDrCtr + DavcJoblos + DavcAcci + DavcPriv + DavcHack + DavCntct) * dummyNeu.y, data = df, na.action=na.exclude, weights=weight)
mm9 <- lm(DavTests ~ (DavcSysSc + DavcSysRl + DavcDrCtr + DavcJoblos + DavcAcci + DavcPriv + DavcHack + DavCntct) * dummyNeu.y, data = df, na.action=na.exclude, weights=weight)

summary(mm7)
summary(mm8)
summary(mm9)

table_difference_interact <- stargazer(mm7, mm8, mm9,
                              type="html", 
                              single.row = TRUE,
                              title="Table A: Predictors of self-driving vehicle attitude change from wave 1 to wave 3 (linear regression)", 
                              align=TRUE,
                              star.char = c("+", "*", "**", "***"),
                              star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
                              digits = 3,
                              dep.var.labels = c("Legal adaptation support", "Confidence in self-driving vehicles", "Self-driving vehicle test support"),
                              order=c(1,2,3,4,5,6,7,8,9,10,11,12,15,13,14,16,17),
                              covariate.labels = c("System safety concern", "System reliability concern", "Driver control loss concern", 
                                                   "Job loss concern", "Liability concern",
                                                   "Privacy concern", "Software misuse concern", "Self-driving vehicle contact",
                                                   "Test municipality", "System safety concern*Test municipality", "System reliability concern*Test municipality", "Driver control loss concern*Test municipality", 
                                                   "Job loss concern*Test municipality", "Liability concern*Test municipality",
                                                   "Privacy concern*Test municipality", "Software misuse concern*Test municipality", "Self-driving vehicle contact*Test municipality"),
                              object.names = FALSE,
                              notes.append = FALSE, notes.align = "l",
                              notes = "Table entries are ordinary least square coefficient estimates with estimated standard errors. Observations are weighted based on the true distribution of age, gender, and municipality. ***p < 0.001, **p < 0.01, *p < 0.05, +p < 0.1",
                              out='tabs_weight/table_differencew1w3_interact.html')

