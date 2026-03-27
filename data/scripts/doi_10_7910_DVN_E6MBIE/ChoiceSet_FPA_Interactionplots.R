##  All options are on the table? Manuscript  ##
##  Rotem Dvir  ##
##  December 2020  ##
## R version 4.0.2 (June 2020) ##

# Set Working Directory
setwd("~/Dropbox/TAMU/Diss./Theory/Choice_set//RnR/Dataverse")

# Packages
library(foreign)
library(ggplot2)
library(gplots)
library(MASS)
library(Hmisc)
library(ggthemes)
library(devtools)
library(dplyr)
library(ggpubr)

# Set Randomizer
set.seed(2020)

# Upload master data file
MyData <- read.csv("ChoiceSetData_June2019.csv", header=TRUE, sep = ",", strip.white = T, na.strings = "")

## Create experimental conditions for reduced sample (no baseline conditions)
MyData$cas <- NA
MyData$cas[MyData$casualties==1] <- 0
MyData$cas[MyData$casualties==2] <- 1

MyData$oth <- NA
MyData$oth[MyData$other==1] <- 0
MyData$oth[MyData$other==2] <- 1

# Set experimental conditions (IV's) as factorial

MyData$horizon<-as.factor(MyData$horizon)
MyData$casualties<-as.factor(MyData$casualties)
MyData$other<-as.factor(MyData$other)

MyData$cas<-as.factor(MyData$cas)
MyData$oth<-as.factor(MyData$oth)

# Setting the contrasts for the sum of squares
options(contrasts=c("contr.helmert","contr.poly"))


##############################################################################
### Manuscript figure 5: Interaction plots for probit models (reduced sample)
##############################################################################

# Probit interaction models: The probability of accepting policy 1 & 2 into the choice-set
## Models replicate analysis in main text (using Stata .do file)

summary(m.red1 <- glm(P1 ~ horizon + cas + oth + horizon * oth +
                        Gender + Age + party + Edu_cat + FP_Know, data = MyData, family=binomial(link="probit")))

summary(m.red2 <- glm(P2 ~ horizon + cas + oth + horizon * oth +
                        Gender + Age + party + Edu_cat + FP_Know, data = MyData, family=binomial(link="probit")))

## Interaction plot P1
library(FSA)

## Run model and compute marginal means
inter.data1 = Summarize(P1 ~ horizon * oth, data = MyData)
  inter.data1$se = inter.data1$sd / sqrt(inter.data1$n)
  inter.data1$se = signif(inter.data1$se, digits=3)
  pd = position_dodge(.2)

## Plot using error bars
inter.plot1 <- ggplot(inter.data1, aes(x = oth, y = mean, color = horizon)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=.2, size=1.2, position=position_dodge(0.2)) +
  geom_point(size=2, position=position_dodge(0.2)) +
  geom_hline(yintercept = 0.5, linetype="dashed", color = "red") +
  xlab("Reciprocal Outcomes") + ylab("Probability of accepting P1") +
  ggtitle("Accept policy 1 (80% chances of success)")

## Final plot for P1
inter.plot1 <- inter.plot1 + theme_pubr() + labs(color = "Time Horizon") + 
  theme(plot.title = element_text(size=16, face="bold.italic")) +
  scale_color_manual(breaks=c("0", "1"),
                      labels=c("ST", "LT"),
                     values = c("red", "blue")) +
  scale_x_discrete(labels = c("Positive", "Negative"))

## Interaction plot P2

## Run model and compute marginal means
inter.data2 = Summarize(P2 ~ horizon * oth, data = MyData)
inter.data2$se = inter.data2$sd / sqrt(inter.data2$n)
inter.data2$se = signif(inter.data2$se, digits=3)

## Plot using error bars
inter.plot2 <- ggplot(inter.data2, aes(x = oth, y = mean, color = horizon)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=.2, size=1.2, position=position_dodge(0.2)) +
  geom_point(size=2, position=position_dodge(0.2)) +
  geom_hline(yintercept = 0.5, linetype="dashed", color = "red") +
  ylim(0.3, 1) +
  xlab("Reciprocal Outcomes") + ylab("Probability of accepting P2") +
  ggtitle("Accept policy 2 (75% chances of success)")

## Final plot for P2
inter.plot2 <- inter.plot2 + theme_pubr() + labs(color = "Time Horizon") + 
  theme(plot.title = element_text(size=16, face="bold.italic")) +
  scale_color_manual(breaks=c("0", "1"),
                     labels=c("ST", "LT"),
                     values = c("red", "blue")) +
  scale_x_discrete(labels = c("Negative", "Positive")) 
 

# Combine plots for both policy variables (figure 5 in main text)
ggarrange(inter.plot1, inter.plot2, common.legend = T, legend = "bottom",
          nrow = 1, ncol = 2)

