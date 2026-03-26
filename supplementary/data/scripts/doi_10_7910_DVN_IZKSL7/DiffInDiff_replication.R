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
library(olsrr)

################################ load dataset ################################ 
load("DiffInDiff_replication.RData")
df$income <- as.factor(ifelse(df$inc1 == "inc1Weiss nicht", "NA", ifelse(
  is.na(df$inc1), "income unknown", as.character(df$inc1))))


##### Correlation Plot ######
corr_data1 <- subset(df, select=c("avcPriv","avcHack","avcAcci","avcSysSc","avcSysRl","avcDrPl","avcDrCtr","avcJoblos"))
corr1 <- chart.Correlation(corr_data1, histogram=TRUE, pch=30)
corr1
ggsave(filename = "plots/corr_interactions.png", corr1, width = 9.5, height = 8, dpi = 400)

#### avTests ####
data2 <- df[,c("avTests", "w2avTests", "w3avTests", "dummyNeu.y")]

means2<-aggregate(data2,mean, by=list(data2$dummyNeu.y), na.rm=TRUE)
means2<-means2[,2:length(means2)]
means2.long<-melt(means2,id.vars="dummyNeu.y")

means2.long$vchr <- as.character(means2.long$variable)
means2.long$wave <- ifelse(startsWith(means2.long$vchr, "w2"), "Wave 2", ifelse(
  startsWith(means2.long$vchr, "w3"), "Wave 3","Wave 1"))

means2.long$variable <- factor(means2.long$variable,
                               levels = c("avTests", "w2avTests", "w3avTests"),
                               labels = c("Wave 1", "Wave 2", "Wave 3"))

means2.long$se <- ifelse(means2.long$dummyNeu.y == 0 & means2.long$wave == "Wave 1", std.error(data2[data2$dummyNeu.y==0,]$avTests,na.rm), ifelse(
  means2.long$dummyNeu.y == 0 & means2.long$wave == "Wave 2", std.error(data2[data2$dummyNeu.y==0,]$w2avTests,na.rm), ifelse(
    means2.long$dummyNeu.y == 0 & means2.long$wave == "Wave 3", std.error(data2[data2$dummyNeu.y==0,]$w3avTests,na.rm), ifelse(
      means2.long$dummyNeu.y == 1 & means2.long$wave == "Wave 1", std.error(data2[data2$dummyNeu.y==1,]$avTests,na.rm), ifelse(
        means2.long$dummyNeu.y == 1 & means2.long$wave == "Wave 2", std.error(data2[data2$dummyNeu.y==1,]$w2avTests,na.rm), ifelse(
          means2.long$dummyNeu.y == 1 & means2.long$wave == "Wave 3", std.error(data2[data2$dummyNeu.y==1,]$w3avTests,na.rm), NA))))))


means2.long$divide <- ifelse(means2.long$dummyNeu.y == 1, "Test municipality", ifelse(
  means2.long$dummyNeu.y == 0, "Control municipalities", NA))

avTests <- ggplot(means2.long, aes(x=variable,
                                   col=divide, 
                                   group=divide, linetype=divide)) + 
  geom_point(aes(y = value), position=position_dodge(0.1), size=1.5) +
  geom_line(aes(y = value), position=position_dodge(0.1), size=1.5) +
  geom_errorbar(aes(ymin=value-1.96*se, ymax=value+1.96*se), width = 0.08, position=position_dodge(0.1), size=1.5) +
  theme_bw()+ 
  theme(axis.text=element_text(size=12, family="Sans"), axis.title=element_text(size=16, family="Sans"), 
        axis.text.x = element_text(angle=0, hjust=0.5, family="Sans"), panel.grid.minor = element_blank(),
        legend.text = element_text(size=12, family="Sans"), plot.title = element_text(family="Sans"), legend.position = "bottom") +
  coord_cartesian(ylim = c(1.6, 4.4), xlim = c(1.45,2.55)) +
  scale_y_discrete(limits=c(1, 2, 3, 4, 5)) +
  scale_colour_manual("", values = c("Test municipality" = "#5e3c99", "Control municipalities"= "#e66101")) +
  scale_linetype_manual("", values = c("dashed", "solid")) +
  labs(title = "Test run support", y = "\nAverage", x="") +
  theme(panel.grid.major.y = element_line(size = 0.6, linetype = 'solid', colour = "grey80"), text = element_text(size=14))
avTests

png("plots/testruns.png")
print(avTests)
dev.off()

#### general concern ####
data2 <- df[,c("avCncrn", "w2avCncrn", "w3avCncrn", "dummyNeu.y")]

means2<-aggregate(data2,mean, by=list(data2$dummyNeu.y), na.rm=TRUE)
means2<-means2[,2:length(means2)]
means2.long<-melt(means2,id.vars="dummyNeu.y")

means2.long$vchr <- as.character(means2.long$variable)
means2.long$wave <- ifelse(startsWith(means2.long$vchr, "w2"), "Wave 2", ifelse(
  startsWith(means2.long$vchr, "w3"), "Wave 3","Wave 1"))

means2.long$variable <- factor(means2.long$variable,
                               levels = c("avCncrn", "w2avCncrn", "w3avCncrn"),
                               labels = c("Wave 1", "Wave 2", "Wave 3"))

means2.long$se <- ifelse(means2.long$dummyNeu.y == 0 & means2.long$wave == "Wave 1", std.error(data2[data2$dummyNeu.y==0,]$avCncrn,na.rm), ifelse(
  means2.long$dummyNeu.y == 0 & means2.long$wave == "Wave 2", std.error(data2[data2$dummyNeu.y==0,]$w2avCncrn,na.rm), ifelse(
    means2.long$dummyNeu.y == 0 & means2.long$wave == "Wave 3", std.error(data2[data2$dummyNeu.y==0,]$w3avCncrn,na.rm), ifelse(
      means2.long$dummyNeu.y == 1 & means2.long$wave == "Wave 1", std.error(data2[data2$dummyNeu.y==1,]$avCncrn,na.rm), ifelse(
        means2.long$dummyNeu.y == 1 & means2.long$wave == "Wave 2", std.error(data2[data2$dummyNeu.y==1,]$w2avCncrn,na.rm), ifelse(
          means2.long$dummyNeu.y == 1 & means2.long$wave == "Wave 3", std.error(data2[data2$dummyNeu.y==1,]$w3avCncrn,na.rm), NA))))))


means2.long$divide <- ifelse(means2.long$dummyNeu.y == 1, "Test municipality", ifelse(
  means2.long$dummyNeu.y == 0, "Control municipalities", NA))

generalconcern <- ggplot(means2.long, aes(x=variable,
                                   col=divide, 
                                   group=divide, linetype=divide)) + 
  geom_point(aes(y = value), position=position_dodge(0.1), size=1.5) +
  geom_line(aes(y = value), position=position_dodge(0.1), size=1.5) +
  geom_errorbar(aes(ymin=value-1.96*se, ymax=value+1.96*se), width = 0.08, position=position_dodge(0.1), size=1.5) +
  theme_bw()+ 
  theme(axis.text=element_text(size=12, family="Sans"), axis.title=element_text(size=16, family="Sans"), 
        axis.text.x = element_text(angle=0, hjust=0.5, family="Sans"), panel.grid.minor = element_blank(),
        legend.text = element_text(size=12, family="Sans"), plot.title = element_text(family="Sans"), legend.position = "bottom") +
  coord_cartesian(ylim = c(1.6, 4.4), xlim = c(1.45,2.55)) +
  scale_y_discrete(limits=c(1, 2, 3, 4, 5)) +
  scale_colour_manual("", values = c("Test municipality" = "#5e3c99", "Control municipalities"= "#e66101")) +
  scale_linetype_manual("", values = c("dashed", "solid")) +
  labs(title = "Confidence in self-driving vehicles", y = "\nAverage (1-5)", x="") +
  theme(panel.grid.major.y = element_line(size = 0.6, linetype = 'solid', colour = "grey80"), text = element_text(size=14))
generalconcern

png("plots/generalconcern.png")
print(generalconcern)
dev.off()



#### policychange ####
data2 <- df[,c("chgStVO", "w2chgStVO", "w3chgStVO", "dummyNeu.y")]

means2<-aggregate(data2,mean, by=list(data2$dummyNeu.y), na.rm=TRUE)
means2<-means2[,2:length(means2)]
means2.long<-melt(means2,id.vars="dummyNeu.y")

means2.long$vchr <- as.character(means2.long$variable)
means2.long$wave <- ifelse(startsWith(means2.long$vchr, "w2"), "Wave 2", ifelse(
  startsWith(means2.long$vchr, "w3"), "Wave 3","Wave 1"))

means2.long$variable <- factor(means2.long$variable,
                               levels = c("chgStVO", "w2chgStVO", "w3chgStVO"),
                               labels = c("Wave 1", "Wave 2", "Wave 3"))

means2.long$se <- ifelse(means2.long$dummyNeu.y == 0 & means2.long$wave == "Wave 1", std.error(data2[data2$dummyNeu.y==0,]$chgStVO,na.rm), ifelse(
  means2.long$dummyNeu.y == 0 & means2.long$wave == "Wave 2", std.error(data2[data2$dummyNeu.y==0,]$w2chgStVO,na.rm), ifelse(
    means2.long$dummyNeu.y == 0 & means2.long$wave == "Wave 3", std.error(data2[data2$dummyNeu.y==0,]$w3chgStVO,na.rm), ifelse(
      means2.long$dummyNeu.y == 1 & means2.long$wave == "Wave 1", std.error(data2[data2$dummyNeu.y==1,]$chgStVO,na.rm), ifelse(
        means2.long$dummyNeu.y == 1 & means2.long$wave == "Wave 2", std.error(data2[data2$dummyNeu.y==1,]$w2chgStVO,na.rm), ifelse(
          means2.long$dummyNeu.y == 1 & means2.long$wave == "Wave 3", std.error(data2[data2$dummyNeu.y==1,]$w3chgStVO,na.rm), NA))))))


means2.long$divide <- ifelse(means2.long$dummyNeu.y == 1, "Test municipality", ifelse(
  means2.long$dummyNeu.y == 0, "Control municipalities", NA))

policychange <- ggplot(means2.long, aes(x=variable,
                                          col=divide, 
                                          group=divide, linetype=divide)) + 
  geom_point(aes(y = value), position=position_dodge(0.1), size=1.5) +
  geom_line(aes(y = value), position=position_dodge(0.1), size=1.5) +
  geom_errorbar(aes(ymin=value-1.96*se, ymax=value+1.96*se), width = 0.08, position=position_dodge(0.1), size=1.5) +
  theme_bw()+ 
  theme(axis.text=element_text(size=12, family="Sans"), axis.title=element_text(size=16, family="Sans"), 
        axis.text.x = element_text(angle=0, hjust=0.5, family="Sans"), panel.grid.minor = element_blank(),
        legend.text = element_text(size=12, family="Sans"), plot.title = element_text(family="Sans"), legend.position = "bottom") +
  coord_cartesian(ylim = c(1.6, 4.4), xlim = c(1.45,2.55)) +
  scale_y_discrete(limits=c(1, 2, 3, 4, 5)) +
  scale_colour_manual(" ", values = c("Test municipality" = "#5e3c99", "Control municipalities"= "#e66101")) +
  scale_linetype_manual(" ", values = c("Test municipality" = "dashed", "Control municipalities"= "solid")) +
  labs(title = "Change road traffic regulation", y = "\nAverage", x="") +
  theme(panel.grid.major.y = element_line(size = 0.6, linetype = 'solid', colour = "grey80"), text = element_text(size=14))
policychange

png("plots/policychange.png")
print(policychange)
dev.off()



#### joint plot DVs ####
policychange <- policychange + theme(legend.text = element_text(size=17, family="Sans"))
legend3 <- get_legend(policychange)
generalconcern <- generalconcern + theme(axis.text=element_text(size=17, family="Sans"), axis.title=element_text(size=17, family="Sans"), 
                                         axis.text.x = element_text(angle=0, hjust=0.5, family="Sans"), panel.grid.minor = element_blank(),
                                         plot.title = element_text(size=17, family="Sans"), legend.position = "none") + 
                                          coord_cartesian(ylim = c(3.4, 3.5), xlim = c(1.45,2.55)) +
  scale_y_discrete(limits=c(3, 4))
policychange <- policychange + theme(axis.text=element_text(size=17, family="Sans"), axis.title=element_text(size=17, family="Sans"), 
                                       axis.text.x = element_text(angle=0, hjust=0.5, family="Sans"), panel.grid.minor = element_blank(),
                                       plot.title = element_text(size=17, family="Sans"), legend.position = "none") + ylab("")+ 
  coord_cartesian(ylim = c(3.4, 3.5), xlim = c(1.45,2.55)) +
  scale_y_discrete(limits=c(3, 4))
avTests <- avTests + theme(axis.text=element_text(size=17, family="Sans"), axis.title=element_text(size=17, family="Sans"), 
                           axis.text.x = element_text(angle=0, hjust=0.5, family="Sans"), panel.grid.minor = element_blank(),
                           plot.title = element_text(size=17, family="Sans"), legend.position = "none") + ylab("")+ 
  coord_cartesian(ylim = c(3.4, 3.5), xlim = c(1.45,2.55)) +
  scale_y_discrete(limits=c(3, 4))

jointDV <- ggdraw() +
  draw_plot(generalconcern, x = 0, y = 0.05, width = 0.34, height = 0.95) +
  draw_plot(policychange, x = 0.345, y = 0.05, width = 0.32, height = 0.95) +
  draw_plot(avTests, x = 0.67, y = 0.05, width = 0.32, height = 0.95) +
  draw_plot(legend3, x = 0, y = 0, width = 1, height = 0.05)
jointDV

ggsave(filename = "./plots/jointDV2.png", jointDV, width = 14, height = 8)


#### Concerncs ######
data0 <- df[,c("avcPriv", "avcHack", "avcAcci", "avcSysSc", "avcSysRl", "avcDrPl", "avcDrCtr", "avcJoblos",
               "w2avcPriv", "w2avcHack", "w2avcAcci", "w2avcSysSc", "w2avcSysRl", "w2avcDrPl", "w2avcDrCtr", "w2avcJoblos", 
               "w3avcPriv", "w3avcHack", "w3avcAcci", "w3avcSysSc", "w3avcSysRl", "w3avcDrPl", "w3avcDrCtr", "w3avcJoblos","dummyNeu.y")]
sem3 <- apply(data0[,c("avcPriv", "avcHack", "avcAcci", "avcSysSc", "avcSysRl", "avcDrPl", "avcDrCtr", "avcJoblos",
                       "w2avcPriv", "w2avcHack", "w2avcAcci", "w2avcSysSc", "w2avcSysRl", "w2avcDrPl", "w2avcDrCtr", "w2avcJoblos",
                       "w3avcPriv", "w3avcHack", "w3avcAcci", "w3avcSysSc", "w3avcSysRl", "w3avcDrPl", "w3avcDrCtr", "w3avcJoblos")],
              2, function(x) sd(x, na.rm=TRUE)/sqrt(length(x)))

means<-aggregate(data0,mean, by=list(data0$dummyNeu.y), na.rm=TRUE)
means<-means[,2:length(means)]
means.long<-melt(means,id.vars="dummyNeu.y")

means.long$vchr <- as.character(means.long$variable)
means.long$wave <- ifelse(startsWith(means2.long$vchr, "w2"), "Wave 2", ifelse(
  startsWith(means2.long$vchr, "w3"), "Wave 3","Wave 1"))

means.long$variable <- factor(means.long$variable,
                              levels = c("avcPriv","avcHack","avcAcci","avcSysSc","avcSysRl","avcDrPl","avcDrCtr","avcJoblos",
                                         "w2avcPriv", "w2avcHack", "w2avcAcci", "w2avcSysSc", "w2avcSysRl", "w2avcDrPl", "w2avcDrCtr", "w2avcJoblos",
                                         "w3avcPriv", "w3avcHack", "w3avcAcci", "w3avcSysSc", "w3avcSysRl", "w3avcDrPl", "w3avcDrCtr", "w3avcJoblos"),
                              labels = c("Privacy", "Software misuse", "Liability", "System safety", 
                                         "System reliability", "Loss of driving enjoyment", "Loss of driving control", "Job loss",
                                         "Privacy", "Software misuse", "Liability", "System safety", 
                                         "System reliability", "Loss of driving enjoyment", "Loss of driving control", "Job loss",
                                         "Privacy", "Software misuse", "Liability", "System safety", 
                                         "System reliability", "Loss of driving enjoyment", "Loss of driving control", "Job loss"))

se <- function(variable) sqrt(var(variable)/length(variable))

means.long$se <- sem3

means.long$divide <- ifelse(means.long$wave=="Wave 1" & means.long$dummyNeu.y == 1, "Test municipality wave 1", ifelse(
  means.long$wave=="Wave 2" & means.long$dummyNeu.y == 1, "Test municipality wave 2", ifelse(
    means.long$wave=="Wave 3" & means.long$dummyNeu.y == 1, "Test municipality wave 3", ifelse(
      means.long$wave=="Wave 1" & means.long$dummyNeu.y == 0, "Control municipalities wave 1", ifelse(
        means.long$wave=="Wave 2" & means.long$dummyNeu.y == 0, "Control municipalities wave 2", ifelse(
          means.long$wave=="Wave 3" & means.long$dummyNeu.y == 0, "Control municipalities wave 3", NA))))))

means.long$group <- as.factor(as.numeric(means.long$variable) * (10 - means.long$dummyNeu.y))

means.long$Neu.fac <- as.factor(means.long$dummyNeu.y)

cncrnmun <- ggplot(data=subset(means.long, !is.na(variable)),aes(x=reorder(variable, value),y=value,colour=divide, shape=divide, linetype = divide)) +
  geom_point(stat="identity", position=position_dodge(0.7), size=1.5)+
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=0.6, position=position_dodge(0.7)) +
  theme_bw()+ 
  theme(axis.text=element_text(size=12, family="Sans"), axis.title=element_text(size=16, family="Sans"), 
        axis.text.x = element_text(angle=0, hjust=0, family="Sans"), panel.grid.minor = element_blank(),
        plot.title = element_text(family="Sans"), legend.text = element_text(family="Sans"),
        panel.grid.major.y = element_line(size = 0.6, linetype = 'solid', colour = "grey80"), text = element_text(size=14), 
        legend.position = c(0.3,-0.14)) +
  scale_x_discrete(labels=c("avcPriv" = "Privacy", "avcHack"="Software misuse",
                            "avcAcci"="Liability","avcSysSc"="System safety","avcSysRl"="System reliability",
                            "avcDrPl"="Loss of driving enjoyment","avcDrCtr"="Loss of driving control","avcJoblos"="Job loss")) +
  coord_cartesian(ylim = c(2.6, 3.4)) +
  scale_y_discrete(limits=c(2, 3, 4)) +
  scale_colour_manual("", values = c("Test municipality wave 1" = "#3c2662", "Test municipality wave 2" = "#5e3c99", "Test municipality wave 3" = "#8461c1", 
                                     "Control municipalities wave 1" = "#9a4101", "Control municipalities wave 2" = "#e66101", "Control municipalities wave 3" = "#fe8a35")) +
  scale_shape_manual("", values = c("Test municipality wave 1" = 19, "Test municipality wave 2" = 15, "Test municipality wave 3" = 17, "Control municipalities wave 1" = 1, "Control municipalities wave 2" = 0, "Control municipalities wave 3" = 2)) +
  scale_linetype_manual("", values = c("Test municipality wave 1" = "solid", "Test municipality wave 2" = "solid", "Test municipality wave 3" = "solid", 
                                     "Control municipalities wave 1" = "dashed", "Control municipalities wave 2" = "dashed", "Control municipalities wave 3" = "dashed")) +
  labs(title = "Concerns regarding self-driving vehicles\n1=do not agree at all, 5=fully agree", y = "Average\n\n\n\n", x="") +
  guides(colour=guide_legend(nrow=2,byrow=TRUE)) +
  coord_flip()
cncrnmun

ggsave(filename = "./plots/concernsummarymun.png", cncrnmun, width = 8, height = 9)


##### Prediction #####
##### Before test ######
m1 <- lm(chgStVO ~ avcSysSc + avcSysRl + avcDrCtr + avcJoblos + avcAcci + avcPriv + avcHack + avCntct + risk + age + male + educ1 + educ2 + educ0 + dummyNeu.y + inc1, data = df, na.action=na.exclude)
m2 <- lm(avNoCrn ~ avcSysSc + avcSysRl + avcDrCtr + avcJoblos + avcAcci + avcPriv + avcHack + avCntct + risk + age + male + educ1 + educ2 + educ0 + dummyNeu.y + inc1, data = df, na.action=na.exclude)
m3 <- lm(avTests ~ avcSysSc + avcSysRl + avcDrCtr + avcJoblos + avcAcci + avcPriv + avcHack + avCntct + risk + age + male + educ1 + educ2 + educ0 + dummyNeu.y + inc1, data = df, na.action=na.exclude)

summary(m1)
ols_vif_tol(m1)
ols_eigen_cindex(m1)
par(mfrow=c(2,2))
plot(m1)

summary(m2)
ols_vif_tol(m2)
ols_eigen_cindex(m2)
plot(m2)

summary(m3)
ols_vif_tol(m3)
ols_eigen_cindex(m3)
plot(m3)


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
                      notes = "Table entries are ordinary least square coefficient estimates with estimated standard errors. ***p < 0.001, **p < 0.01, *p < 0.05, +p < 0.1",
                      out='tabs/table_beforetest.html')

m1_df <-
  broom::tidy(m1) %>% filter(term != c("age", "male")) %>% 
  filter(!grepl('inc1*', term)) %>% 
  filter(!grepl('educ*', term)) %>% 
  relabel_predictors(c(avcSysSc = "System safety concern",                       
                       avcSysRl = "System reliability concern", 
                       avcDrCtr = "Driver control loss concern", 
                       avcJoblos = "Job loss concern", 
                       avcAcci = "Liability concern",
                       avcPriv = "Privacy concern",
                       avcHack = "Software misuse concern",
                       avCntct = "Self-driving vehicle contact", 
                       risk = "Willingness to take risks",
                       dummyNeu.y = "Test municipality")) %>%
  mutate(model = "Model 1")

m2_df <-
  broom::tidy(m2) %>% filter(term != c("age", "male")) %>% 
  filter(!grepl('inc1*', term)) %>% 
  filter(!grepl('educ*', term)) %>% 
  relabel_predictors(c(avcSysSc = "System safety concern",                       
                       avcSysRl = "System reliability concern", 
                       avcDrCtr = "Driver control loss concern", 
                       avcJoblos = "Job loss concern", 
                       avcAcci = "Liability concern",
                       avcPriv = "Privacy concern",
                       avcHack = "Software misuse concern",
                       avCntct = "Self-driving vehicle contact", 
                       risk = "Willingness to take risks",
                       dummyNeu.y = "Test municipality")) %>% 
  mutate(model = "Model 2")

m3_df <-
  broom::tidy(m3) %>% filter(term != c("age", "male")) %>% 
  filter(!grepl('inc1*', term)) %>% 
  filter(!grepl('educ*', term)) %>% 
  relabel_predictors(c(avcSysSc = "System safety concern",                       
                       avcSysRl = "System reliability concern", 
                       avcDrCtr = "Driver control loss concern", 
                       avcJoblos = "Job loss concern", 
                       avcAcci = "Liability concern",
                       avcPriv = "Privacy concern",
                       avcHack = "Software misuse concern",
                       avCntct = "Self-driving vehicle contact", 
                       risk = "Willingness to take risks",
                       dummyNeu.y = "Test municipality")) %>%
  mutate(model = "Model 3")

models123 <- rbind(m1_df, m2_df, m3_df)

m2plot1 <- dwplot(models123, dot_args = list(aes(shape = model)),
                  whisker_args = list(aes(linetype = model))) +
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
                      values=c("#cc79a7","#0072b2","#009e73")) +
  scale_shape_discrete(name="Dependent Variable",
                       breaks=c("Model 1", "Model 2", "Model 3"),
                       labels=c("Legal adaptation support", "Confidence in self-driving vehicles", "Self-driving vehicle test support")) +
  guides(
    shape = guide_legend("Dependent Variable"), 
    colour = guide_legend("Dependent Variable"))

ggsave(filename = "./plots/predictionBefore.png", m2plot1, width = 8, height = 8)


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

m4 <- lm(DchgStV0 ~ DavcSysSc + DavcSysRl + DavcDrCtr + DavcJoblos + DavcAcci + DavcPriv + DavcHack + DavCntct + risk + age+ male + educ1 + educ2 + educ0 + dummyNeu.y + inc1, data = df, na.action=na.exclude)
m5 <- lm(DavCncrn ~ DavcSysSc + DavcSysRl + DavcDrCtr + DavcJoblos + DavcAcci + DavcPriv + DavcHack + DavCntct + risk + age+ male + educ1 + educ2 + educ0 + dummyNeu.y + inc1, data = df, na.action=na.exclude)
m6 <- lm(DavTests ~ DavcSysSc + DavcSysRl + DavcDrCtr + DavcJoblos + DavcAcci + DavcPriv + DavcHack + DavCntct + risk + age+ male + educ1 + educ2 + educ0 + dummyNeu.y + inc1, data = df, na.action=na.exclude)

summary(m4)
ols_vif_tol(m4)
ols_eigen_cindex(m4)
plot(m4)

summary(m5)
ols_vif_tol(m5)
ols_eigen_cindex(m5)
plot(m5)

summary(m6)
ols_vif_tol(m6)
ols_eigen_cindex(m6)
plot(m5)


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
                             notes = "Table entries are ordinary least square coefficient estimates with estimated standard errors. ***p < 0.001, **p < 0.01, *p < 0.05, +p < 0.1",
                             out='tabs/table_differencew1w2.html')

m4_df <-
  broom::tidy(m4) %>% filter(term != c("age", "male")) %>% 
  filter(!grepl('inc1*', term)) %>% 
  filter(!grepl('educ*', term)) %>% 
  relabel_predictors(c(DavcSysSc = "System safety concern",                       
                       DavcSysRl = "System reliability concern", 
                       DavcDrCtr = "Driver control loss concern", 
                       DavcJoblos = "Job loss concern", 
                       DavcAcci = "Liability concern",
                       DavcPriv = "Privacy concern",
                       DavcHack = "Software misuse concern",
                       DavCntct = "Self-driving vehicle contact", 
                       risk = "Willingness to take risks",
                       dummyNeu.y = "Test municipality")) %>%
  mutate(model = "Model 1")

m5_df <-
  broom::tidy(m5) %>% filter(term != c("age", "male")) %>% 
  filter(!grepl('inc1*', term)) %>% 
  filter(!grepl('educ*', term)) %>% 
  relabel_predictors(c(DavcSysSc = "System safety concern",                       
                       DavcSysRl = "System reliability concern", 
                       DavcDrCtr = "Driver control loss concern", 
                       DavcJoblos = "Job loss concern", 
                       DavcAcci = "Liability concern",
                       DavcPriv = "Privacy concern",
                       DavcHack = "Software misuse concern",
                       DavCntct = "Self-driving vehicle contact", 
                       risk = "Willingness to take risks",
                       dummyNeu.y = "Test municipality")) %>% 
  mutate(model = "Model 2")

m6_df <-
  broom::tidy(m6) %>% filter(term != c("age", "male")) %>% 
  filter(!grepl('inc1*', term)) %>% 
  filter(!grepl('educ*', term)) %>% 
  relabel_predictors(c(DavcSysSc = "System safety concern",                       
                       DavcSysRl = "System reliability concern", 
                       DavcDrCtr = "Driver control loss concern", 
                       DavcJoblos = "Job loss concern", 
                       DavcAcci = "Liability concern",
                       DavcPriv = "Privacy concern",
                       DavcHack = "Software misuse concern",
                       DavCntct = "Self-driving vehicle contact", 
                       risk = "Willingness to take risks",
                       dummyNeu.y = "Test municipality")) %>%
  mutate(model = "Model 3")

models456 <- rbind(m4_df, m5_df, m6_df)

m2plot3 <- dwplot(models456, dot_args = list(aes(shape = model)),
                  whisker_args = list(aes(linetype = model))) +
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
                      values=c("#cc79a7","#0072b2","#009e73")) +
  scale_shape_discrete(name="Dependent Variable",
                       breaks=c("Model 1", "Model 2", "Model 3"),
                       labels=c("Legal adaptation support", "Confidence in self-driving vehicles", "Self-driving vehicle test support")) +
  guides(
    shape = guide_legend("Dependent Variable"), 
    colour = guide_legend("Dependent Variable"))

ggsave(filename = "./plots/predictionDifferencew1w2.png", m2plot3, width = 10, height = 10)


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

m7 <- lm(DchgStV0 ~ DavcSysSc + DavcSysRl + DavcDrCtr + DavcJoblos + DavcAcci + DavcPriv + DavcHack + DavCntct + risk + age+ male + educ1 + educ2 + educ0 + dummyNeu.y + inc1, data = df, na.action=na.exclude)
m8 <- lm(DavCncrn ~ DavcSysSc + DavcSysRl + DavcDrCtr + DavcJoblos + DavcAcci + DavcPriv + DavcHack + DavCntct + risk + age+ male + educ1 + educ2 + educ0 + dummyNeu.y + inc1, data = df, na.action=na.exclude)
m9 <- lm(DavTests ~ DavcSysSc + DavcSysRl + DavcDrCtr + DavcJoblos + DavcAcci + DavcPriv + DavcHack + DavCntct + risk + age+ male + educ1 + educ2 + educ0 + dummyNeu.y + inc1, data = df, na.action=na.exclude)

summary(m7)
ols_vif_tol(m7)
ols_eigen_cindex(m7)
par(mfrow=c(2,2))
plot(m7)

summary(m8)
ols_vif_tol(m8)
ols_eigen_cindex(m8)
plot(m8)

summary(m9)
ols_vif_tol(m9)
ols_eigen_cindex(m9)
plot(m9)


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
                              notes = "Table entries are ordinary least square coefficient estimates with estimated standard errors. ***p < 0.001, **p < 0.01, *p < 0.05, +p < 0.1",
                              out='tabs/table_differencew1w3.html')

m7_df <-
  broom::tidy(m7) %>% filter(term != c("age", "male")) %>% 
  filter(!grepl('inc1*', term)) %>% 
  filter(!grepl('educ*', term)) %>% 
  relabel_predictors(c(DavcSysSc = "System safety concern",                       
                       DavcSysRl = "System reliability concern", 
                       DavcDrCtr = "Driver control loss concern", 
                       DavcJoblos = "Job loss concern", 
                       DavcAcci = "Liability concern",
                       DavcPriv = "Privacy concern",
                       DavcHack = "Software misuse concern",
                       DavCntct = "Self-driving vehicle contact", 
                       risk = "Willingness to take risks",
                       dummyNeu.y = "Test municipality")) %>%
  mutate(model = "Model 1")

m8_df <-
  broom::tidy(m8) %>% filter(term != c("age", "male")) %>% 
  filter(!grepl('inc1*', term)) %>% 
  filter(!grepl('educ*', term)) %>% 
  relabel_predictors(c(DavcSysSc = "System safety concern",                       
                       DavcSysRl = "System reliability concern", 
                       DavcDrCtr = "Driver control loss concern", 
                       DavcJoblos = "Job loss concern", 
                       DavcAcci = "Liability concern",
                       DavcPriv = "Privacy concern",
                       DavcHack = "Software misuse concern",
                       DavCntct = "Self-driving vehicle contact", 
                       risk = "Willingness to take risks",
                       dummyNeu.y = "Test municipality")) %>% 
  mutate(model = "Model 2")

m9_df <-
  broom::tidy(m9) %>% filter(term != c("age", "male")) %>% 
  filter(!grepl('inc1*', term)) %>% 
  filter(!grepl('educ*', term)) %>% 
  relabel_predictors(c(DavcSysSc = "System safety concern",                       
                       DavcSysRl = "System reliability concern", 
                       DavcDrCtr = "Driver control loss concern", 
                       DavcJoblos = "Job loss concern", 
                       DavcAcci = "Liability concern",
                       DavcPriv = "Privacy concern",
                       DavcHack = "Software misuse concern",
                       DavCntct = "Self-driving vehicle contact", 
                       risk = "Willingness to take risks",
                       dummyNeu.y = "Test municipality")) %>%
  mutate(model = "Model 3")

models789 <- rbind(m7_df, m8_df, m9_df)

m2plot4 <- dwplot(models789, dot_args = list(aes(shape = model)),
                  whisker_args = list(aes(linetype = model))) +
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
                      values=c("#cc79a7","#0072b2","#009e73")) +
  scale_shape_discrete(name="Dependent Variable",
                       breaks=c("Model 1", "Model 2", "Model 3"),
                       labels=c("Legal adaptation support", "Confidence in self-driving vehicles", "Self-driving vehicle test support")) +
  guides(
    shape = guide_legend("Dependent Variable"), 
    colour = guide_legend("Dependent Variable"))

ggsave(filename = "./plots/predictionDifferencew1w3.png", m2plot4, width = 10, height = 10)

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

ggsave(filename = "./plots/fig_joint.png", fig_joint, width = 12, height = 6)


##### Prediction Interactions #####
##### Before test ######
mm1 <- lm(chgStVO ~ (avcSysSc + avcSysRl + avcDrCtr + avcJoblos + avcAcci + avcPriv + avcHack + avCntct) * dummyNeu.y, data = df, na.action=na.exclude)
mm2 <- lm(avNoCrn ~ (avcSysSc + avcSysRl + avcDrCtr + avcJoblos + avcAcci + avcPriv + avcHack + avCntct) * dummyNeu.y, data = df, na.action=na.exclude)
mm3 <- lm(avTests ~ (avcSysSc + avcSysRl + avcDrCtr + avcJoblos + avcAcci + avcPriv + avcHack + avCntct) * dummyNeu.y, data = df, na.action=na.exclude)

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
                                                   "Test municipality", "System safety concern*Test municipality", "System reliability concern*Test municipality", "Driver control loss concern*Test municipality", 
                                                   "Job loss concern*Test municipality", "Liability concern*Test municipality",
                                                   "Privacy concern*Test municipality", "Software misuse concern*Test municipality", "Self-driving vehicle contact*Test municipality"),
                              object.names = FALSE,
                              notes.append = FALSE, notes.align = "l",
                              notes = "Table entries are ordinary least square coefficient estimates with estimated standard errors. ***p < 0.001, **p < 0.01, *p < 0.05, +p < 0.1",
                              out='tabs/table_beforetest_interact.html')


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

mm4 <- lm(DchgStV0 ~ (DavcSysSc + DavcSysRl + DavcDrCtr + DavcJoblos + DavcAcci + DavcPriv + DavcHack + DavCntct) * dummyNeu.y, data = df, na.action=na.exclude)
mm5 <- lm(DavCncrn ~ (DavcSysSc + DavcSysRl + DavcDrCtr + DavcJoblos + DavcAcci + DavcPriv + DavcHack + DavCntct) * dummyNeu.y, data = df, na.action=na.exclude)
mm6 <- lm(DavTests ~ (DavcSysSc + DavcSysRl + DavcDrCtr + DavcJoblos + DavcAcci + DavcPriv + DavcHack + DavCntct) * dummyNeu.y, data = df, na.action=na.exclude)

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
                              notes = "Table entries are ordinary least square coefficient estimates with estimated standard errors. ***p < 0.001, **p < 0.01, *p < 0.05, +p < 0.1",
                              out='tabs/table_differencew1w2_interact.html')


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

mm7 <- lm(DchgStV0 ~ (DavcSysSc + DavcSysRl + DavcDrCtr + DavcJoblos + DavcAcci + DavcPriv + DavcHack + DavCntct) * dummyNeu.y, data = df, na.action=na.exclude)
mm8 <- lm(DavCncrn ~ (DavcSysSc + DavcSysRl + DavcDrCtr + DavcJoblos + DavcAcci + DavcPriv + DavcHack + DavCntct) * dummyNeu.y, data = df, na.action=na.exclude)
mm9 <- lm(DavTests ~ (DavcSysSc + DavcSysRl + DavcDrCtr + DavcJoblos + DavcAcci + DavcPriv + DavcHack + DavCntct) * dummyNeu.y, data = df, na.action=na.exclude)

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
                              notes = "Table entries are ordinary least square coefficient estimates with estimated standard errors. ***p < 0.001, **p < 0.01, *p < 0.05, +p < 0.1",
                              out='tabs/table_differencew1w3_interact.html')

