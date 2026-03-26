######################################

### Replication Script for Citizen Involvement in Public Policy ###

######################################

### Load relevant packages ###

library(foreign)
library(ggplot2)
library(ggsci)
library(ggpubr)
library(tidyr) 
library(reshape2)
library(gridExtra)
library(plyr) 
library(ggridges)
library(readxl)
library(ggrepel)
library(psych)
library(WRS) # For recent versions of R, the installation of this package might require you to first install RTools
# as well as the packages "pkgbuild" and "remotes" (to install dependencies such as "akima")

### Figure 2 ###

# Load the data
data_figure2 <- read_excel("Figure2_Data.xlsx", sheet = "Data")

# Variables
data_figure2$city <- as.factor(data_figure2$city)
data_figure2$neighborhood <- as.factor(data_figure2$neighborhood)
data_figure2$turnout <- as.numeric(data_figure2$turnout)
data_figure2$electorate <- as.numeric(data_figure2$electorate)
data_figure2$money <- as.numeric(data_figure2$money)

data_figure2$turnout.per <- data_figure2$turnout/data_figure2$electorate
data_figure2$money.rel <- data_figure2$money/data_figure2$electorate

# Descriptive data
describe(data_figure2)
describe(data_figure2$turnout.per*100)

# Correlations reported in text
corr.test(data_figure2$turnout.per, data_figure2$money)
corr.test(data_figure2$turnout.per, data_figure2$money.rel)

# Figure 2
figure.2a <- ggplot(data_figure2, aes(x=money/1000, y=turnout.per*100, label=neighborhood)) +
  geom_point(aes(shape=city), size=4) +
  geom_label_repel(aes(label = neighborhood),
                   box.padding   = 0.35, 
                   point.padding = 0.5, label.size=NA, fill = NA, size=5,  family="serif")   +
  scale_y_continuous(limits=c(0,37), breaks=seq(0,35, by = 5), expand=c(0,0)) +
  scale_x_continuous(limits=c(0,420), breaks=seq(50,400, by = 50), expand=c(0,0)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        panel.border = element_blank(),  axis.ticks= element_blank(),
        text=element_text(size=18,  family="serif", face="bold"))+
  labs(x="Budget in euros (x1000)", y="Voter turnout (%)") +
  scale_shape_manual(values = c(0, 15, 2, 17, 18, 5)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank())
figure.2a

figure.2b <- ggplot(data_figure2, aes(x=money.rel, y=turnout.per*100, label=neighborhood)) +
  geom_point(aes(shape=city), size=4) +
  geom_label_repel(aes(label = neighborhood),
                   box.padding   = 0.35, 
                   point.padding = 0.5, label.size=NA, fill = NA, size=5,  family="serif")   +
  scale_y_continuous(limits=c(0,37), breaks=seq(0,35, by = 5), expand=c(0,0)) +
  scale_x_continuous(limits=c(0,22), breaks=seq(5,20, by = 5), expand=c(0,0)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        panel.border = element_blank(),  axis.ticks= element_blank(),
        text=element_text(size=18,  family="serif", face="bold"))+
  labs(x="Euros per member of electorate", y="") +
  scale_shape_manual(values = c(0, 15, 2, 17, 18, 5)) +
  theme(legend.position = "top") 
figure.2b

figure2 <- ggarrange(figure.2a,figure.2b, common.legend=TRUE)
figure2

### Benoordenhout Experiment ###

ben_data <- read_excel("Benoordenhout_Data.xlsx", sheet = "Data")
names(ben_data)
nrow(ben_data)

## Attention check (SI, section 2.1-2.2)

ben_data$correct <- ifelse((ben_data$attention.check == "300k" & ben_data$money == "300k" |
                              ben_data$attention.check == "30k" & ben_data$money == "30k"|
                              ben_data$attention.check == "3k" & ben_data$money == "3k"), 1,
                          ifelse(ben_data$attention.check == "NA", "NA", 0))
ben_data$correct <- as.factor(ben_data$correct)

summary(ben_data$correct) # 158 correct, 67 incorrect

## Manipulation check (SI, section 2.1)
table(ben_data$manipulation.check)
ben_data$manipulation.check <- as.numeric(ben_data$manipulation.check)
ben_data$money <- as.factor(ben_data$money)

pass.data <- ben_data[which(ben_data$correct ==1),]

by(ben_data$manipulation.check, ben_data$money, FUN=describe)

# Figure S1
figure.checka <- ggplot(ben_data, aes(x=money, y=manipulation.check)) + 
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", position=position_dodge(width=0.9), width=0.5, size=2) +
  stat_summary(fun=mean, geom="point", size = 5) +
  scale_x_discrete(limits=c("3k", "30k", "300k"), labels=c("3,000", "30,000", "300,000")) +
  scale_y_continuous(limits=c(1,7),breaks=seq(1,7, by = 1)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        panel.border = element_blank(),  axis.ticks= element_blank(),
        text=element_text(size=22,  family="serif", face="bold"), axis.title.y = element_blank())+
  labs(x="", y="") 
figure.checka

figure.checkb <- ggplot(pass.data, aes(x=money, y=manipulation.check)) + 
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", position=position_dodge(width=0.9), width=0.5, size=2) +
  stat_summary(fun=mean, geom="point", size = 5) +
  scale_x_discrete(limits=c("3k", "30k", "300k"), labels=c("3,000", "30,000", "300,000")) +
  scale_y_continuous(limits=c(1,7),breaks=seq(1,7, by = 1)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        panel.border = element_blank(),  axis.ticks= element_blank(),
        text=element_text(size=22,  family="serif", face="bold"), axis.title.y = element_blank())+
  labs(x="", y="") 
figure.checkb

figure.checkab <- ggarrange(figure.checka, figure.checkb)
figure.checkab

# Statistical tests for manipulation check

contrast1 <- c(-2,1,1) # Setting contrasts
contrast2 <- c(0,-1,1)
ben_data$money.contrast <- ben_data$money
table(ben_data$money)
contrasts(ben_data$money.contrast) <- cbind(contrast1, contrast2)

table(ben_data$money.contrast)
aov.check <- aov(manipulation.check ~ money.contrast, data = ben_data)
summary(aov.check)
summary.lm(aov.check)

names(ben_data)
robust.check.r <- unstack(ben_data[,c(1,16)], manipulation.check ~ money)
robust.model.checkr <- t1way(robust.check.r)
robust.model.checkr 

pass.data$money.contrast <- pass.data$money
table(pass.data$money)
contrasts(pass.data$money.contrast) <- cbind(contrast1, contrast2)

aov.checkb <- aov(manipulation.check ~ money.contrast, data = pass.data)
summary(aov.checkb)
summary.lm(aov.checkb)

names(pass.data)
robust.check.r2 <- unstack(pass.data[,c(1,16)], manipulation.check ~ money)
robust.model.checkr2 <- t1way(robust.check.r2)
robust.model.checkr2 

## Figure 1 (Top panel)

# Reordering the treatment variable ("money") to have 3,000 as the reference category (also for OLS models below)

ben_data$money.reordered <- factor(ben_data$money, levels = c("3k", "30k", "300k"))
table(ben_data$money)
table(ben_data$money.reordered) # Recoding worked correctly

names(ben_data)

figure.dens1 <- ggplot(ben_data, aes(x=argue, y=money, fill=money.reordered)) + 
  geom_density_ridges(alpha=.8, quantile_lines=TRUE,
                      quantile_fun=function(x,...)mean(x),scale = 1) +
  scale_y_discrete(labels=(c("", "", "")), expand=c(0,0)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        panel.border = element_blank(),  axis.ticks= element_blank(),
        text=element_text(size=12,  family="serif", face="bold"), axis.title.y = element_blank())+
  labs(x="", y="") +
  scale_x_continuous(limits=c(0,8), breaks=seq(1,7, by = 1)) +
  scale_fill_manual(values=c("grey90", "grey70", "grey30"),name="", labels = c("3,000","30,000", "300,000")) +
  theme(legend.position = "bottom") +
  ggtitle("Post an argument") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.text = element_text(size = 14))
figure.dens1

figure.dens2 <- ggplot(ben_data, aes(x=vote, y=money, fill=money.reordered)) + 
  geom_density_ridges(alpha=.8, quantile_lines=TRUE,
                      quantile_fun=function(x,...)mean(x),scale = 1) +
  scale_y_discrete(labels=(c("", "", "")), expand=c(0,0)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        panel.border = element_blank(),  axis.ticks= element_blank(),
        text=element_text(size=12,  family="serif", face="bold"), axis.title.y = element_blank())+
  labs(x="", y="") +
  scale_x_continuous(limits=c(0,8), breaks=seq(1,7, by = 1)) +
  scale_fill_manual(values=c("grey90", "grey70", "grey30"),name="", labels = c("3,000","30,000", "300,000")) +
  theme(legend.position = "bottom") +
  ggtitle("Vote") +
  theme(plot.title = element_text(hjust = 0.5)) 
figure.dens2

figure.dens3 <- ggplot(ben_data, aes(x=submit, y=money, fill=money.reordered)) + 
  geom_density_ridges(alpha=.8, quantile_lines=TRUE,
                      quantile_fun=function(x,...)mean(x),scale = 1) +
  scale_y_discrete(labels=(c("", "", "")), expand=c(0,0)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        panel.border = element_blank(),  axis.ticks= element_blank(),
        text=element_text(size=12,  family="serif", face="bold"), axis.title.y = element_blank())+
  labs(x="", y="") +
  scale_x_continuous(limits=c(0,8), breaks=seq(1,7, by = 1)) +
  scale_fill_manual(values=c("grey90", "grey70", "grey30"),name="", labels = c("3,000","30,000", "300,000")) +
  theme(legend.position = "bottom") +
  ggtitle("Submit a proposal") +
  theme(plot.title = element_text(hjust = 0.5)) 
figure.dens3

figure.dens4 <- ggplot(ben_data, aes(x=like, y=money, fill=money.reordered)) + 
  geom_density_ridges(alpha=.8, quantile_lines=TRUE,
                      quantile_fun=function(x,...)mean(x),scale = 1) +
  scale_y_discrete(labels=(c("", "", "")), expand=c(0,0)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        panel.border = element_blank(),  axis.ticks= element_blank(),
        text=element_text(size=12,  family="serif", face="bold"), axis.title.y = element_blank())+
  labs(x="", y="") +
  scale_x_continuous(limits=c(0,8), breaks=seq(1,7, by = 1)) +
  scale_fill_manual(values=c("grey90", "grey70", "grey30"),name="", labels = c("3,000","30,000", "300,000")) +
  theme(legend.position = "bottom") +
  ggtitle("Like a proposal") +
  theme(plot.title = element_text(hjust = 0.5)) 
figure.dens4

figure.1.top <- ggarrange(figure.dens1, figure.dens2, figure.dens3, figure.dens4, common.legend = TRUE, legend = "right")
annotate_figure(figure.1.top,
                top = text_grob("Benoordenhout", face = "bold", size = 18, family="serif"))

## Figure S2

figure.dens1b <- ggplot(pass.data, aes(x=argue, y=money, fill=money)) + 
  geom_density_ridges(alpha=.8, quantile_lines=TRUE,
                      quantile_fun=function(x,...)mean(x),scale = 1) +
  scale_y_discrete(labels=(c("", "", "")), expand=c(0,0)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        panel.border = element_blank(),  axis.ticks= element_blank(),
        text=element_text(size=18,  family="serif", face="bold"), axis.title.y = element_blank())+
  labs(x="", y="") +
  scale_x_continuous(limits=c(0,8), breaks=seq(1,7, by = 1)) +
  scale_fill_npg(name="", labels = c("300,000", "30,000", "3,000")) +
  theme(legend.position = "bottom") +
  ggtitle("Post an argument") +
  theme(plot.title = element_text(hjust = 0.5)) 
figure.dens1b

figure.dens2b <- ggplot(pass.data, aes(x=vote, y=money, fill=money)) + 
  geom_density_ridges(alpha=.8, quantile_lines=TRUE,
                      quantile_fun=function(x,...)mean(x),scale = 1) +
  scale_y_discrete(labels=(c("", "", "")), expand=c(0,0)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        panel.border = element_blank(),  axis.ticks= element_blank(),
        text=element_text(size=18,  family="serif", face="bold"), axis.title.y = element_blank())+
  labs(x="", y="") +
  scale_x_continuous(limits=c(0,8), breaks=seq(1,7, by = 1)) +
  scale_fill_npg(name="", labels = c("300,000", "30,000", "3,000")) +
  theme(legend.position = "bottom") +
  ggtitle("Vote") +
  theme(plot.title = element_text(hjust = 0.5)) 
figure.dens2b

figure.dens3b <- ggplot(pass.data, aes(x=submit, y=money, fill=money)) + 
  geom_density_ridges(alpha=.8, quantile_lines=TRUE,
                      quantile_fun=function(x,...)mean(x),scale = 1) +
  scale_y_discrete(labels=(c("", "", "")), expand=c(0,0)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        panel.border = element_blank(),  axis.ticks= element_blank(),
        text=element_text(size=18,  family="serif", face="bold"), axis.title.y = element_blank())+
  labs(x="", y="") +
  scale_x_continuous(limits=c(0,8), breaks=seq(1,7, by = 1)) +
  scale_fill_npg(name="", labels = c("300,000", "30,000", "3,000")) +
  theme(legend.position = "bottom") +
  ggtitle("Submit a proposal") +
  theme(plot.title = element_text(hjust = 0.5)) 
figure.dens3b

figure.dens4b <- ggplot(pass.data, aes(x=like, y=money, fill=money)) + 
  geom_density_ridges(alpha=.8, quantile_lines=TRUE,
                      quantile_fun=function(x,...)mean(x),scale = 1) +
  scale_y_discrete(labels=(c("", "", "")), expand=c(0,0)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        panel.border = element_blank(),  axis.ticks= element_blank(),
        text=element_text(size=18,  family="serif", face="bold"), axis.title.y = element_blank())+
  labs(x="", y="") +
  scale_x_continuous(limits=c(0,8), breaks=seq(1,7, by = 1)) +
  scale_fill_npg(name="", labels = c("300,000", "30,000", "3,000")) +
  theme(legend.position = "bottom") +
  ggtitle("Like a proposal") +
  theme(plot.title = element_text(hjust = 0.5)) 
figure.dens4b

figure.s2 <- ggarrange(figure.dens1b, figure.dens2b, figure.dens3b, figure.dens4b, 
                          common.legend = TRUE, legend = "right")
figure.s2

## ANOVAs (Table S4) and their assumption checks (Table S2); descriptive statistics for Benoordenhout sample

contrast1 <- c(1,1,-2) # contrasts with 3k as ref.
contrast2 <- c(-1,1,0) # 30 vs 300k
ben_data$money.contrast <- ben_data$money
contrasts(ben_data$money.contrast) <- cbind(contrast1, contrast2)
ben_data$money.contrast

library(psych)
library(car)

describe(ben_data$argue)
describeBy(ben_data$argue, ben_data$money)

describe(ben_data$vote)
describeBy(ben_data$vote, ben_data$money)

describe(ben_data$submit)
describeBy(ben_data$submit, ben_data$money)

describe(ben_data$like)
describeBy(ben_data$like, ben_data$money)

describeBy(pass.data$argue, pass.data$money)
describeBy(pass.data$vote, pass.data$money)
describeBy(pass.data$submit, pass.data$money)
describeBy(pass.data$like, pass.data$money)

aov.argue <- aov(argue ~ money.contrast, data = ben_data)
summary(aov.argue)
summary.lm(aov.argue)
leveneTest(ben_data$argue, ben_data$money.contrast)
plot(aov.argue)
describe(ben_data[which(ben_data$money != "3k"),]$argue)

names(ben_data)
robust.argue <- unstack(ben_data[,c(18,16)], argue ~ money)
robust.model.a <- t1way(robust.argue)
robust.model.a 

aov.argue2 <- aov(argue ~ money.contrast, data = pass.data)
summary(aov.argue2)
summary.lm(aov.argue2)
leveneTest(pass.data$argue, pass.data$money.contrast)
plot(aov.argue2)
describeBy(pass.data$argue, pass.data$money)
describe(pass.data[which(pass.data$money != "3k"),]$argue)

names(pass.data)
robust.argue2 <- unstack(pass.data[,c(18,16)], argue ~ money)
robust.model.a2 <- t1way(robust.argue2)
robust.model.a2 

aov.vote <- aov(vote ~ money.contrast, data = ben_data)
summary(aov.vote)
summary.lm(aov.vote)
leveneTest(ben_data$vote, ben_data$money.contrast)
plot(aov.vote)

names(ben_data)
robust.vote <- unstack(ben_data[,c(20,16)], vote ~ money)
robust.model.v <- t1way(robust.vote)
robust.model.v 

aov.vote2 <- aov(vote ~ money.contrast, data = pass.data)
summary(aov.vote2)
summary.lm(aov.vote2)
leveneTest(pass.data$vote, pass.data$money.contrast)
plot(aov.vote2)

names(pass.data)
robust.vote2 <- unstack(pass.data[,c(20,16)], vote ~ money)
robust.model.v2 <- t1way(robust.vote2)
robust.model.v2 

aov.submit <- aov(submit ~ money.contrast, data = ben_data)
summary(aov.submit)
summary.lm(aov.submit)
leveneTest(ben_data$submit, ben_data$money.contrast)
plot(aov.submit)

names(ben_data)
robust.submit <- unstack(ben_data[,c(17,16)], submit ~ money)
robust.model.s <- t1way(robust.submit)
robust.model.s 

aov.submit2 <- aov(submit ~ money.contrast, data = pass.data)
summary(aov.submit2)
summary.lm(aov.submit2)
leveneTest(pass.data$submit, pass.data$money.contrast)
plot(aov.submit2)

names(pass.data)
robust.submit2 <- unstack(pass.data[,c(17,16)], submit ~ money)
robust.model.s2 <- t1way(robust.submit2)
robust.model.s2 

aov.like <- aov(like ~ money.contrast, data = ben_data)
summary(aov.like)
summary.lm(aov.like)
leveneTest(ben_data$like, ben_data$money.contrast)
plot(aov.like)

names(ben_data)
robust.like <- unstack(ben_data[,c(19, 16)], like ~ money)
robust.model.l <- t1way(robust.like)
robust.model.l 

aov.like2 <- aov(like ~ money.contrast, data = pass.data)
summary(aov.like2)
summary.lm(aov.submit2)
leveneTest(pass.data$like, pass.data$money.contrast)
plot(aov.like2)

names(pass.data)
robust.like2 <- unstack(pass.data[,c(19,16)], like ~ money)
robust.model.l2 <- t1way(robust.like2)
robust.model.l2 

## Descriptive statistics for control variables (Table S3)

# Setting DKs to missing data for prior ePB involvement variables
ben_data$prior.argue2 <- ifelse(ben_data$prior.argue == 1, 1,
                                ifelse(ben_data$prior.argue == 0, 0, NA))
ben_data$prior.vote2 <- ifelse(ben_data$prior.vote == 1, 1,
                               ifelse(ben_data$prior.vote == 0, 0, NA))
ben_data$prior.submit2 <- ifelse(ben_data$prior.submit == 1, 1,
                                 ifelse(ben_data$prior.submit == 0, 0, NA))
ben_data$prior.like2 <- ifelse(ben_data$prior.like == 1, 1,
                               ifelse(ben_data$prior.like == 0, 0, NA))
names(ben_data)

ben_data[,c(3,12,14,22,23)] <- sapply(ben_data[,c(3,12,14,22,23)],as.numeric) 

describe(ben_data[,c(3,12,14,22,23,27:30)])


## OLS models 

# Preparing for bootstrapped regression coefficients

library(boot)
set.seed(12345) # to make analyses reproducible

bootReg <- function (formula, data, indices)
{
  d <- data [indices,]
  fit <- lm(formula, data = d)
  return(coef(fit))
} 

# OLS models full sample (Tables S5-6)

ols.argue <- lm(argue ~ money.reordered + satisfaction.general + satisfaction.results + gender2 + education2 + age + prior.argue2, 
                 data=ben_data, na.action=na.omit)
summary(ols.argue)
plot(ols.argue)
nobs(ols.argue)

boot.argue <- boot(statistic = bootReg, formula = argue ~ money.reordered + satisfaction.general + satisfaction.results + gender2 + 
                   education2 + age + prior.argue2, data = ben_data, R = 2000)
boot.ci(boot.argue, type = "bca", index = 2) # 30k
boot.ci(boot.argue, type = "bca", index = 3) # 300k

ols.vote <- lm(vote ~ money.reordered + satisfaction.general + satisfaction.results + gender2 + education2 + age + prior.vote2, 
                data=ben_data, na.action=na.omit)
summary(ols.vote)
plot(ols.vote)
nobs(ols.vote)

boot.vote <- boot(statistic = bootReg, formula = vote ~ money.reordered + satisfaction.general + satisfaction.results + gender2 + 
                     education2 + age + prior.vote2, data = ben_data, R = 2000)
boot.ci(boot.vote, type = "bca", index = 2) # 30k
boot.ci(boot.vote, type = "bca", index = 3) # 300k

ols.submit <- lm(submit ~ money.reordered + satisfaction.general + satisfaction.results + gender2 + education2 + age + prior.submit2, 
               data=ben_data, na.action=na.omit)
summary(ols.submit)
plot(ols.submit)
nobs(ols.submit)

boot.submit <- boot(statistic = bootReg, formula = submit ~ money.reordered + satisfaction.general + satisfaction.results + gender2 + 
                      education2 + age + prior.submit2, data = ben_data, R = 2000)
boot.ci(boot.submit, type = "bca", index = 2) # 30k
boot.ci(boot.submit, type = "bca", index = 3) # 300k

ols.like <- lm(like ~ money.reordered + satisfaction.general + satisfaction.results + gender2 + education2 + age + prior.like2, 
                 data=ben_data, na.action=na.omit)
summary(ols.like)
plot(ols.like)
nobs(ols.like)

boot.like <- boot(statistic = bootReg, formula = like ~ money.reordered + satisfaction.general + satisfaction.results + gender2 + 
                    education2 + age + prior.like2, data = ben_data, R = 2000)
boot.ci(boot.like, type = "bca", index = 2) # 30k
boot.ci(boot.like, type = "bca", index = 3) # 300k

# OLS models for subsample that passed the attention check (Table S7)

pass.data2 <- ben_data[which(ben_data$correct == 1),] 

ols.argue.p <- lm(argue ~ money.reordered + satisfaction.general + satisfaction.results + gender2 + education2 + age + prior.argue2, 
                data=pass.data2, na.action=na.omit)
summary(ols.argue.p)
plot(ols.argue.p)
nobs(ols.argue.p)

ols.vote.p <- lm(vote ~ money.reordered + satisfaction.general + satisfaction.results + gender2 + education2 + age + prior.vote2, 
               data=pass.data2, na.action=na.omit)
summary(ols.vote.p)
plot(ols.vote.p)
nobs(ols.vote.p)

ols.submit.p <- lm(submit ~ money.reordered + satisfaction.general + satisfaction.results + gender2 + education2 + age + prior.submit2, 
                 data=pass.data2, na.action=na.omit)
summary(ols.submit.p)
plot(ols.submit.p)
nobs(ols.submit.p)

ols.like.p <- lm(like ~ money.reordered + satisfaction.general + satisfaction.results + gender2 + education2 + age + prior.like2, 
               data=pass.data2, na.action=na.omit)
summary(ols.like.p)
plot(ols.like.p)
nobs(ols.like.p)

## Exploratory Analyses for Benoordenhout sample (SI, section 2.3)

# Descriptive statistics for future participation in ePB
ben_data$fut.argue2 <- ifelse(ben_data$fut.argue == 1, 1,
                                ifelse(ben_data$fut.argue == 0 | ben_data$fut.argue == 99, 0, NA))
ben_data$fut.vote2 <- ifelse(ben_data$fut.vote == 1, 1,
                               ifelse(ben_data$fut.vote == 0 | ben_data$fut.vote == 99, 0, NA))
ben_data$fut.submit2 <- ifelse(ben_data$fut.submit == 1, 1,
                                 ifelse(ben_data$fut.submit == 0 | ben_data$fut.submit == 99, 0, NA))
ben_data$fut.like2 <- ifelse(ben_data$fut.like == 1, 1,
                               ifelse(ben_data$fut.like == 0 | ben_data$fut.like == 99, 0, NA))
names(ben_data)

ben_data[,c(31:34)] <- sapply(ben_data[,c(31:34)],as.numeric) 

describe(ben_data[,c(31:34)])

### Dutch population sample

data_nl <- read_excel("NL_Data.xlsx", sheet = "Data")

## Descriptive statistics (text and Tables S3-4) and sample-population comparison
data_nl$education2 <- as.numeric(data_nl$education2)
data_nl$age <- as.numeric(data_nl$age)

describe(data_nl)
describeBy(data_nl$argue, data_nl$money)
describeBy(data_nl$vote, data_nl$money)
describeBy(data_nl$like, data_nl$money)
describeBy(data_nl$submit, data_nl$money)

# Education: 0.57 - (3084+1826)/14521 = 0.23 # data from (last consulted July 28, 2021) https://opendata.cbs.nl/statline/#/CBS/nl/dataset/82275NED/table?fromstatweb
# Male: 0.47 - 0.493491511 = -0.02 # data from (last consulted July 28, 2021) https://opendata.cbs.nl/statline/#/CBS/nl/dataset/37325/table?ts=1627498313355 
# Age: 48.93 - 49.55981813 = -0.62982 # ibid. ('105 and over' all counted as 105)

## Figure 1 (Bottom panel)

figure.nl1 <- ggplot(data_nl, aes(x=argue, y=money, fill=money)) + 
  geom_density_ridges(alpha=.8, quantile_lines=TRUE,
                      quantile_fun=function(x,...)mean(x),scale = 1) +
  scale_y_discrete(limits=c("e_300","e_30", "e_3"),labels=(c("", "", "")), expand=c(0,0)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        panel.border = element_blank(),  axis.ticks= element_blank(),
        text=element_text(size=12,  family="serif", face="bold"), axis.title.y = element_blank())+
  labs(x="", y="") +
  scale_x_continuous(limits=c(0,8), breaks=seq(1,7, by = 1)) +
  scale_fill_manual(values=c("grey90", "grey70", "grey30"),name="", labels = c("3,000", "30,000", "300,000")) +
  theme(legend.position = "bottom") +
  ggtitle("Post an argument") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.text = element_text(size = 14))
figure.nl1

figure.nl2 <- ggplot(data_nl, aes(x=vote, y=money, fill=money)) + 
  geom_density_ridges(alpha=.8, quantile_lines=TRUE,
                      quantile_fun=function(x,...)mean(x),scale = 1) +
  scale_y_discrete(limits=c("e_300","e_30", "e_3"),labels=(c("", "", "")), expand=c(0,0)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        panel.border = element_blank(),  axis.ticks= element_blank(),
        text=element_text(size=12,  family="serif", face="bold"), axis.title.y = element_blank())+
  labs(x="", y="") +
  scale_x_continuous(limits=c(0,8), breaks=seq(1,7, by = 1)) +
  scale_fill_manual(values=c("grey90", "grey70", "grey30"),name="", labels = c("3,000", "30,000", "300,000")) +
  theme(legend.position = "bottom") +
  ggtitle("Vote") +
  theme(plot.title = element_text(hjust = 0.5)) 
figure.nl2

figure.nl3 <- ggplot(data_nl, aes(x=submit, y=money, fill=money)) + 
  geom_density_ridges(alpha=.8, quantile_lines=TRUE,
                      quantile_fun=function(x,...)mean(x),scale = 1) +
  scale_y_discrete(limits=c("e_300","e_30", "e_3"),labels=(c("", "", "")), expand=c(0,0)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        panel.border = element_blank(),  axis.ticks= element_blank(),
        text=element_text(size=12,  family="serif", face="bold"), axis.title.y = element_blank())+
  labs(x="", y="") +
  scale_x_continuous(limits=c(0,8), breaks=seq(1,7, by = 1)) +
  scale_fill_manual(values=c("grey90", "grey70", "grey30"),name="", labels = c("3,000", "30,000", "300,000")) +
  theme(legend.position = "bottom") +
  ggtitle("Submit a proposal") +
  theme(plot.title = element_text(hjust = 0.5)) 
figure.nl3

figure.nl4 <- ggplot(data_nl, aes(x=like, y=money, fill=money)) + 
  geom_density_ridges(alpha=.8, quantile_lines=TRUE,
                      quantile_fun=function(x,...)mean(x),scale = 1) +
  scale_y_discrete(limits=c("e_300","e_30", "e_3"),labels=(c("", "", "")), expand=c(0,0)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        panel.border = element_blank(),  axis.ticks= element_blank(),
        text=element_text(size=12,  family="serif", face="bold"), axis.title.y = element_blank())+
  labs(x="", y="") +
  scale_x_continuous(limits=c(0,8), breaks=seq(1,7, by = 1)) +
  scale_fill_manual(values=c("grey90", "grey70", "grey30"),name="", labels = c("3,000", "30,000", "300,000")) +
  theme(legend.position = "bottom") +
  ggtitle("Like a proposal") +
  theme(plot.title = element_text(hjust = 0.5)) 
figure.nl4

figure.1.bottom <- ggarrange(figure.nl1, figure.nl2, figure.nl3, figure.nl4, common.legend = TRUE, legend = "right")
annotate_figure(figure.1.bottom,
                top = text_grob("Netherlands", face = "bold", size = 18, family="serif"))

## ANOVAs (Table S4) and OLS models (Tables S8-9) for population sample, per dependent variable

# Argue

aov.argue.nl <- aov(argue ~ money, data = data_nl)
summary(aov.argue.nl)
summary.lm(aov.argue.nl)
leveneTest(data_nl$argue, data_nl$money)
plot(aov.argue.nl)

names(data_nl)
robust.argue.nl <- unstack(data_nl[,c(8,6)], argue ~ money)
t1way(robust.argue.nl) # based on 20% trimmed means

lm.argue.nl <- lm(argue ~ money + education2 + gender2 + age, data=data_nl, na.action=na.omit)
summary(lm.argue.nl)
nobs(lm.argue.nl)
boot.argue.nl <- boot(statistic = bootReg, formula = argue ~ money + education2 + gender2 + age, data = data_nl, R = 2000)
boot.ci(boot.argue.nl, type = "bca", index = 2) # 30k
boot.ci(boot.argue.nl, type = "bca", index = 3) # 300k

# Vote

aov.vote.nl <- aov(vote ~ money, data = data_nl)
summary(aov.vote.nl)
summary.lm(aov.vote.nl)
leveneTest(data_nl$vote, data_nl$money)
plot(aov.vote.nl)

names(data_nl)
robust.vote.nl <- unstack(data_nl[,c(10,6)], vote ~ money)
t1way(robust.vote.nl) 

lm.vote.nl <- lm(vote ~ money + education2 + gender2 + age, data=data_nl, na.action=na.omit)
summary(lm.vote.nl)
nobs(lm.vote.nl)
boot.vote.nl <- boot(statistic = bootReg, formula = vote ~ money + education2 + gender2 +age, data = data_nl, R = 2000)
boot.ci(boot.vote.nl, type = "bca", index = 2) # 30k
boot.ci(boot.vote.nl, type = "bca", index = 3) # 300k

# Submit

aov.submit.nl <- aov(submit ~ money, data = data_nl)
summary(aov.submit.nl)
summary.lm(aov.submit.nl)
leveneTest(data_nl$submit, data_nl$money)
plot(aov.submit.nl)

names(data_nl)
robust.submit.nl <- unstack(data_nl[,c(7,6)], submit ~ money)
t1way(robust.submit.nl) 

lm.submit.nl <- lm(submit ~ money + education2 + gender2 + age, data=data_nl, na.action=na.omit)
summary(lm.submit.nl)
nobs(lm.submit.nl)
boot.submit <- boot(statistic = bootReg, formula = submit ~ money + education2 + gender2 + age, data = data_nl, R = 2000)
boot.ci(boot.submit, type = "bca", index = 2) # 30k
boot.ci(boot.submit, type = "bca", index = 3) # 300k

# Like

aov.like.nl <- aov(like ~ money, data = data_nl)
summary(aov.like.nl)
summary.lm(aov.like.nl)
leveneTest(data_nl$like, data_nl$money)
plot(aov.like.nl)

names(data_nl)
robust.like.nl <- unstack(data_nl[,c(9,6)], like ~ money)
t1way(robust.like.nl) 

lm.like.nl <- lm(like ~ money + education2 + gender2 + age, data=data_nl, na.action=na.omit)
summary(lm.like.nl)
nobs(lm.like.nl)
plot(lm.like.nl)
boot.like <- boot(statistic = bootReg, formula = like ~ money + education2 + gender2 + age, data = data_nl, R = 2000)
boot.ci(boot.like, type = "bca", index = 2) # 30k
boot.ci(boot.like, type = "bca", index = 3) # 300k

### Exploratory Analyses (SI, section 2.3)

## ANOVAs (Table S10) for population sample by type of locality

names(data_nl)
table(data_nl$place_residence)
table(data_nl$urban_rural)

data_nl$city <- ifelse(data_nl$urban_rural == "Een dorp", 0,
                       ifelse(data_nl$urban_rural == "De voorsteden of buitenwijken van een grote stad" |
                              data_nl$urban_rural == "Een grote stad" |
                              data_nl$urban_rural == "Een kleine stad", 1, NA))
table(data_nl$city) # recoding is correct
data_nl$city <- as.numeric(data_nl$city)
summary(data_nl$city)

city.data <- data_nl[which(data_nl$city == 1),]
village.data <- data_nl[which(data_nl$city == 0),]

describeBy(city.data$submit, city.data$money)
describeBy(city.data$like, city.data$money)
describeBy(city.data$argue, city.data$money)
describeBy(city.data$vote, city.data$money)

describeBy(village.data$submit, village.data$money)
describeBy(village.data$like, village.data$money)
describeBy(village.data$argue, village.data$money)
describeBy(village.data$vote, village.data$money)

# Argue
aov.argue.city <- aov(argue ~ money, data = city.data)
summary(aov.argue.city)
summary.lm(aov.argue.city)
leveneTest(city.data$argue, city.data$money)
plot(aov.argue.city)

names(city.data)
robust.argue.city <- unstack(city.data[,c(8,6)], argue ~ money)
t1way(robust.argue.city) # based on 20% trimmed means

aov.argue.v <- aov(argue ~ money, data = village.data)
summary(aov.argue.v)
summary.lm(aov.argue.v)
leveneTest(village.data$argue, village.data$money)
plot(aov.argue.v)

names(village.data)
robust.argue.v <- unstack(village.data[,c(8,6)], argue ~ money)
t1way(robust.argue.v) # based on 20% trimmed means

# Vote

aov.vote.city <- aov(vote ~ money, data = city.data)
summary(aov.vote.city)
summary.lm(aov.vote.city)
leveneTest(city.data$vote, city.data$money)
plot(aov.vote.city)

names(city.data)
robust.vote.city <- unstack(city.data[,c(10,6)], vote ~ money)
t1way(robust.vote.city) # based on 20% trimmed means

aov.vote.v <- aov(vote ~ money, data = village.data)
summary(aov.vote.v)
summary.lm(aov.vote.v)
leveneTest(village.data$vote, village.data$money)
plot(aov.vote.v)

names(village.data)
robust.vote.v <- unstack(village.data[,c(10,6)], vote ~ money)
t1way(robust.vote.v) # based on 20% trimmed means

# Submit

aov.submit.city <- aov(submit ~ money, data = city.data)
summary(aov.submit.city)
summary.lm(aov.submit.city)
leveneTest(city.data$vote, city.data$money)
plot(aov.submit.city)

names(city.data)
robust.submit.city <- unstack(city.data[,c(7,6)], submit ~ money)
t1way(robust.submit.city) # based on 20% trimmed means

aov.submit.v <- aov(submit ~ money, data = village.data)
summary(aov.submit.v)
summary.lm(aov.submit.v)
leveneTest(village.data$submit, village.data$money)
plot(aov.submit.v)

names(village.data)
robust.submit.v <- unstack(village.data[,c(7,6)], submit ~ money)
t1way(robust.submit.v) # based on 20% trimmed means

# Like

aov.like.city <- aov(like ~ money, data = city.data)
summary(aov.like.city)
summary.lm(aov.like.city)
leveneTest(city.data$like, city.data$money)
plot(aov.like.city)

names(city.data)
robust.like.city <- unstack(city.data[,c(9,6)], like ~ money)
t1way(robust.like.city) # based on 20% trimmed means

aov.like.v <- aov(like ~ money, data = village.data)
summary(aov.like.v)
summary.lm(aov.like.v)
leveneTest(village.data$like, village.data$money)
plot(aov.like.v)

names(village.data)
robust.like.v <- unstack(village.data[,c(9,6)], like ~ money)
t1way(robust.like.v) # based on 20% trimmed means

## ANOVAs (Table S11) for population sample by province with ePBs or not

table(data_nl$place_residence)

data_nl$epb <- ifelse(data_nl$place_residence == "Zuid-Holland"|
                      data_nl$place_residence == "Noord-Holland"|
                      data_nl$place_residence == "Utrecht", 1,
                       ifelse(data_nl$place_residence == "NA", NA, 0))
table(data_nl$epb) # recoding is correct
data_nl$epb <- as.numeric(data_nl$epb)
summary(data_nl$epb)

epb1.data <- data_nl[which(data_nl$epb == 1),]
epb0.data <- data_nl[which(data_nl$epb == 0),]

describeBy(epb1.data$submit, epb1.data$money)
describeBy(epb1.data$like, epb1.data$money)
describeBy(epb1.data$argue, epb1.data$money)
describeBy(epb1.data$vote, epb1.data$money)

describeBy(epb0.data$submit, epb0.data$money)
describeBy(epb0.data$like, epb0.data$money)
describeBy(epb0.data$argue, epb0.data$money)
describeBy(epb0.data$vote, epb0.data$money)

# Argue
aov.argue.pb1 <- aov(argue ~ money, data = epb1.data)
summary(aov.argue.pb1)
summary.lm(aov.argue.pb1)
leveneTest(epb1.data$argue, epb1.data$money)
plot(aov.argue.pb1)

names(epb1.data)
robust.argue.pb1 <- unstack(epb1.data[,c(8,6)], argue ~ money)
t1way(robust.argue.pb1) # based on 20% trimmed means

aov.argue.pb0 <- aov(argue ~ money, data = epb0.data)
summary(aov.argue.pb0)
summary.lm(aov.argue.pb0)
leveneTest(epb0.data$argue, epb0.data$money)
plot(aov.argue.pb0)

names(epb0.data)
robust.argue.pb0 <- unstack(epb0.data[,c(8,6)], argue ~ money)
t1way(robust.argue.pb0) # based on 20% trimmed means

# Vote

aov.vote.pb1 <- aov(vote ~ money, data = epb1.data)
summary(aov.vote.pb1)
summary.lm(aov.vote.pb1)
leveneTest(epb1.data$vote, epb1.data$money)
plot(aov.vote.pb1)

names(epb1.data)
robust.vote.pb1 <- unstack(epb1.data[,c(10,6)], vote ~ money)
t1way(robust.vote.pb1) # based on 20% trimmed means

aov.vote.pb0 <- aov(vote ~ money, data = epb0.data)
summary(aov.vote.pb0)
summary.lm(aov.vote.pb0)
leveneTest(epb0.data$vote, epb0.data$money)
plot(aov.vote.pb0)

names(epb0.data)
robust.vote.pb0 <- unstack(epb0.data[,c(10,6)], vote ~ money)
t1way(robust.vote.pb0) # based on 20% trimmed means

# Submit

aov.submit.pb1 <- aov(submit ~ money, data = epb1.data)
summary(aov.submit.pb1)
summary.lm(aov.submit.pb1)
leveneTest(epb1.data$vote, epb1.data$money)
plot(aov.submit.pb1)

names(epb1.data)
robust.submit.pb1 <- unstack(epb1.data[,c(7,6)], submit ~ money)
t1way(robust.submit.pb1) # based on 20% trimmed means

aov.submit.pb0 <- aov(submit ~ money, data = epb0.data)
summary(aov.submit.pb0)
summary.lm(aov.submit.pb0)
leveneTest(epb0.data$submit, epb0.data$money)
plot(aov.submit.pb0)

names(epb0.data)
robust.submit.pb0 <- unstack(epb0.data[,c(7,6)], submit ~ money)
t1way(robust.submit.pb0) # based on 20% trimmed means

# Like

aov.like.pb1 <- aov(like ~ money, data = epb1.data)
summary(aov.like.pb1)
summary.lm(aov.like.pb1)
leveneTest(epb1.data$like, epb1.data$money)
plot(aov.like.pb1)

names(epb1.data)
robust.like.pb1 <- unstack(epb1.data[,c(9,6)], like ~ money)
t1way(robust.like.pb1) # based on 20% trimmed means

aov.like.pb0 <- aov(like ~ money, data = epb0.data)
summary(aov.like.pb0)
summary.lm(aov.like.pb0)
leveneTest(epb0.data$like, epb0.data$money)
plot(aov.like.pb0)

names(epb0.data)
robust.like.pb0 <- unstack(epb0.data[,c(9,6)], like ~ money)
t1way(robust.like.pb0) # based on 20% trimmed means

################################################################################
