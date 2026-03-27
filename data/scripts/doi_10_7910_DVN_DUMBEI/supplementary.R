#######################################
### tables and figures in appendices ##
#######################################

library(ggplot2)
library(dplyr)
library(tidyr)
library(xtable)
library(tidyverse)
library(plyr)
library('scales')
library(lme4)
library(lmerTest)
library(texreg)
library(interactions)
library(gridExtra)
library(lemon)

mean.na <- function(x) {  mean(x, na.rm = T)}
sd.na <- function(x) {  sd(x, na.rm = T)}
min.na <- function(x) {  min(x, na.rm = T)}
max.na <- function(x) {  max(x, na.rm = T)}
n.na <- function(x) {  length(x[!is.na(x)])}


########  preparing the data  ##########

# read survey data

survey <- read.csv("survey2019.csv")
survey2 <- read.csv("survey2020.csv")

survey <- survey[,-1]
survey2 <- survey2[,-1]

# read battleground state status

battleground3 <- read.csv("battleground3.csv")
battleground3 <- battleground3[,-1]

# read partisan segregation index

state_isolation_index <- read.csv("state_isolation.csv")
state_isolation_index <- state_isolation_index[,-1]


#########################################################
######### Table S6. battleground state status ###########
###############   in Appendix D        ##################
#########################################################

print(battleground3)

#########################################################
######### Figure S1. Partisan segregation     ###########
###############   in Appendix E        ##################
#########################################################

ggplot(state_isolation_index) +
  geom_point(aes(x = b_tc, y = reorder(state, b_tc), colour = "For Republicans"), size = 1) + 
  geom_point(aes(x = b_ct, y = reorder(state, b_tc), colour = "For Democrats") ,size = 1) + 
  scale_x_continuous(breaks = seq(0,1,0.2),
                              labels = c("0\n(Not Segregated)",
                                         "0.2", "0.4", "0.6", "0.8", "1\n(Segregated)")) +
  xlab("Indices of Partisan Segregation") +
  ylab("States") +
  theme_bw() +
  scale_color_manual(name = "Legend",
                     values = c("For Republicans" = "red",
                                "For Democrats" = "blue")
  ) 

#########################################################
######### Figure S2. Relationship between    ###########  
### battleground state status and partisan segregation ##
################## in Appendix F ########################
#########################################################

# change state abbrev
battleground3$state_po <- gsub("ME1|ME2", "ME", battleground3$state_po)
battleground3$state_po <- gsub("NE1|NE2|NE3", "NE", battleground3$state_po)

# merge two measures 
state_isolation_index <- state_isolation_index %>% select(state_po, b_ct, b_tc)
battleground3 <- battleground3 %>% select(state_po, swing3)
state_variables <- left_join(battleground3, state_isolation_index, by = "state_po")

state_variables %>%
  pivot_longer(b_ct:b_tc, names_to = "partisan", values_to = "index") %>%
  ggplot(aes(x = factor(swing3), y = index, color = partisan)) +
  geom_boxplot() +
  scale_color_manual(name = "Partisan leaning",
                     values = c("b_tc" = "red",  "b_ct" = "blue"),
                     labels = c("Republican", "Democratic")) +
  ylab("Partisan Segregation") +
  xlab("Battelground State Status") +
  theme_bw() +
  scale_x_discrete(labels = c("Solid/leaning Democratic", "Battleground", "Solid/leaning Republican"))

#########################################################
######### Appendix G. Descriptive statistics  ###########
###############   of key variables     ##################
#########################################################

#############################################
#########    Table S7.      #################
############ Frequency table of  ############
#### battleground state status (2020) #######
#############################################

table(survey2$swing3)

#############################################
#########    Table S8.      #################
############ Frequency table of  ############
#### battleground state status (2019) #######
#############################################

table(survey$swing3)

#############################################
#########    Table S9     ###################
# Key variable descriptive statistics (2020)#
#############################################

survey2$outgroup <- ifelse(survey2$pid7 < 4, survey2$b_ct, 
                           ifelse(survey2$pid7 >4 & survey2$pid7 <= 7, survey2$b_tc, NA))

survey2$dem <- ifelse(survey2$pid7 < 4, 1, 
                      ifelse(survey2$pid7 >4 & survey$pid7 <= 7, 0, NA))

descriptive2 <- sapply(survey2 %>% filter(wave == 1) %>%
                        select(outgroup, ideo7, feel, evid, tip, age, gender, educ),
                      each(n.na, mean.na, sd.na, min.na, max.na))

descriptive2 <- t(descriptive2)    
descriptive2 <- rbind(t(sapply(survey %>% select(auc),
                              each(n.na, mean.na, sd.na, min.na, max.na))),
                     descriptive2)

colnames(descriptive2) <- c("N", "M","SD","Min","Max")
rownames(descriptive2) <- c("Sensitivity", "Partisan Segregation", "Political Ideology", 
                           "Faith in Intuition", "Need for Evidence", "Truth is Political",
                           "Age", "Gender", "Education")

print(descriptive2)

#############################################
#########    Table S10     #################
# Key variable descriptive statistics (2019)#
#############################################

survey$outgroup <- ifelse(survey$pid7 < 4, survey$b_ct, 
                           ifelse(survey$pid7 >4 & survey$pid7 <= 7, survey$b_tc, NA))

survey$dem <- ifelse(survey$pid7 < 4, 1, 
                      ifelse(survey$pid7 >4 & survey$pid7 <= 7, 0, NA))

descriptive <- sapply(survey %>% filter(wave == 1) %>%
                        select(outgroup, ideo7, feel, evid, tip, age, gender, educ),
                      each(n.na, mean.na, sd.na, min.na, max.na))

descriptive <- t(descriptive)    
descriptive <- rbind(t(sapply(survey %>% select(auc),
                              each(n.na, mean.na, sd.na, min.na, max.na))),
                     descriptive)

colnames(descriptive) <- c("N", "M","SD","Min","Max")
rownames(descriptive) <- c("Sensitivity", "Partisan Segregation", "Political Ideology", 
                           "Faith in Intuition", "Need for Evidence", "Truth is Political",
                           "Age", "Gender", "Education")

print(descriptive)

#########################################################
#########    Appendix H. Full models          ###########
#########################################################

# turn wave into a factor
survey2$wave.f <- factor(survey2$wave)
survey$wave.f <- factor(survey$wave)

#########################################
#########    Table S11     ##############
#########################################

m1 <- lmer(auc ~ swing3 + wave.f + ideo7 + feel + evid + tip + 
              age + educ + gender + (1| caseid), survey2,
            REML = F)
summary(m1)

m2 <- lmer(auc ~ swing3 * ideo7 + wave.f + feel + evid + tip + 
              age + educ + gender + (1| caseid), survey2,
            REML = F)
summary(m2)

#########################################
#########    Table S12     ##############
#########################################

m3 <- lmer (auc ~ swing3 + wave.f + ideo7 + feel + evid + tip + 
              age + educ + gender + (1| caseid), survey,
            REML = F)
summary(m3)

m4 <- lmer (auc ~ swing3 * ideo7 + wave.f + feel + 
              evid + tip + age + educ + gender + (1| caseid), survey,
            REML = F)
summary(m4)

#########################################
#########    Table S13     ##############
#########################################

m5 <- lmer (auc ~ outgroup + dem + wave.f + ideo7 + feel + evid + tip + 
              age + educ + gender + (1| caseid), 
            survey2,
            REML = F)
summary(m5)

m6 <- lmer (auc ~ outgroup * ideo7 + dem + wave.f + ideo7 + feel + evid + tip + 
              age + educ + gender + (1| caseid), 
            survey2,
            REML = F)
summary(m6)

#########################################
#########    Table S14     ##############
#########################################

m7 <- lmer (auc ~ outgroup + dem + wave.f+ ideo7 + feel + evid + tip + 
              age + educ + gender + (1| caseid), 
            survey,
            REML = F)
summary(m7)

m8 <- lmer (auc ~ outgroup * ideo7 + dem + wave.f + ideo7 + feel + evid + tip + 
              age + educ + gender + (1| caseid), 
            survey,
            REML = F)
summary(m8)

#########################################################
#########    Figure S3 in Appendix I          ###########
######## Probing the moderated relationship #############
#########################################################

# create some dummy variables
survey2$swing_dem <- NA
survey2$swing_dem[survey2$swing3 == 1] <- 1
survey2$swing_dem[survey2$swing3 == 0] <- 0

survey2$rep_dem <- NA
survey2$rep_dem[survey2$swing3 == 2] <- 1
survey2$rep_dem[survey2$swing3 == 0] <- 0

m_swing_dem <- lmer (auc ~ swing_dem * ideo7 + factor(wave) + feel + evid + tip + 
              age + educ + gender + (1| caseid), survey2,
            REML = F)

p1.observed <- sim_slopes(m_swing_dem, pred = swing_dem, modx = ideo7, 
  jnplot = TRUE, data = survey2)[[5]] +
  labs(tag = "B", title = NULL, x = "Political Ideology", 
    y = "Marginal effect of\nliving in Battleground States") +
  scale_x_continuous(breaks = seq(0,8)) +
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  theme(strip.text.y = element_blank(),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold"),
        axis.text.x = element_text(angle = 30, vjust = 0.9, hjust=1))

m_rep_dem <- lmer (auc ~ rep_dem * ideo7 + factor(wave) + feel + evid + tip + 
              age + educ + gender + (1| caseid), survey2,
            REML = F)

p2.observed <- sim_slopes(m_rep_dem, pred = rep_dem, modx = ideo7, 
  jnplot = TRUE, data = survey2)[[5]] +
  labs(tag = "A", title = NULL, x = "Political Ideology", 
    y = "Marginal effect of\nliving in Republican-leaning States") +
  scale_x_continuous(breaks = seq(0,8)) +
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  theme(strip.text.y = element_blank(),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold"),
        axis.text.x = element_text(angle = 30, vjust = 0.9, hjust=1))

grid_arrange_shared_legend(p2.observed, p1.observed,
                    nrow = 1, ncol = 2, position = "bottom")  


#########################################################
#########    Appendix H. Study comparison     ###########
#########################################################

survey2020 <- survey2 %>% select(auc, outgroup, dem, swing3, ideo7, feel, evid, tip, 
                                        age, educ, gender, wave, caseid)

survey2019 <- survey %>% select(auc, outgroup, dem, swing3, ideo7, feel, evid, tip, 
                                age, educ, gender, wave, caseid)

# add a study indicator: 0 corresponds to 2019 data and 1 corresponds to 2020 data

survey2020$study <- 1
survey2019$study <- 0

# pool both data
pooled <- rbind(survey2020, survey2019)

# add an unique indicator of waves 
pooled$study.wave <- max(pooled$study) * (pooled$wave -1 ) + pooled$study

m0 <- lmer (auc ~ study * swing3 + 
              ideo7 + feel + evid + tip + 
              age + educ + gender + 
              (1|study:caseid) +  
              (1 |study:study.wave),
            pooled)

m1 <- lmer (auc ~ study + swing3 + study*ideo7 +
              ideo7 + feel + evid + tip + 
              age + educ + gender + 
              (1|study:caseid) +  
              (1 |study:study.wave),
            pooled)

m2 <- lmer (auc ~ study * swing3 * ideo7 + 
              ideo7 + feel + evid + tip + 
              age + educ + gender + 
              (1|study:caseid) +  
              (1 |study:study.wave), 
            pooled)

m3 <- lmer (auc ~ study * outgroup + dem +
              ideo7 + feel + evid + tip + 
              age + educ + gender + 
              (1|study:caseid) +  
              (1 |study:study.wave),
            pooled)

m4 <- lmer (auc ~ study + outgroup + dem + study*ideo7 +
              ideo7 + feel + evid + tip + 
              age + educ + gender + 
              (1|study:caseid) +  
              (1 |study:study.wave),
            pooled)

m5 <- lmer (auc ~ study + outgroup + dem + study * outgroup* ideo7 + 
              ideo7 + feel + evid + tip + 
              age + educ + gender + 
              (1|study:caseid) +  
              (1 |study:study.wave), 
            pooled)
#########################################
#########    Table S15     ##############
#########################################

screenreg(list(m0,m1,m2))


#########################################
#########    Table S16     ##############
#########################################

screenreg(list(m3,m4,m5))
