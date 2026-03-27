##################################
#  Replicating results  reported #
#        in the main text        #
##################################
library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)
library(texreg)
library(ggplot2)
library(ggpubr)
library(ggeffects)
library(lemon)
library(data.table)

options(scipen = 999)
select <- dplyr::select

########  preparing the data  ##########

# read both survey data

survey <- fread("survey2019.csv")
survey2 <- fread("survey2020.csv")

survey <- survey[,-1]
survey2 <- survey2[,-1]

# turn battleground state status into a factor
survey$swing_3 <- factor(survey$swing3,
                               levels = c(1,0,2),
                               labels = c("Battleground","Solid/Leaning Democratic", 
                                          "Solid/Leaning Republican"))

survey2$swing_3 <- factor(survey2$swing3,
                         levels = c(1,0,2),
                         labels = c("Battleground","Solid/Leaning Democratic", 
                                    "Solid/Leaning Republican"))

# create the partisan segregation index
survey$outgroup <- ifelse(survey$pid7 < 4, survey$b_ct, 
                          ifelse(survey$pid7 > 4 & survey$pid7 <= 7, survey$b_tc, NA))

survey2$outgroup <- ifelse(survey2$pid7 < 4, survey2$b_ct, 
                           ifelse(survey2$pid7 >4 & survey2$pid7 <= 7, survey2$b_tc, NA))


# create a dichotomous variable for party ID, 1 as Democrats and 0 as Republicans
survey$dem <- ifelse(survey$pid7 < 4, 1, 
                     ifelse(survey$pid7 >4 & survey$pid7 <= 7, 0, NA))

survey2$dem <- ifelse(survey2$pid7 < 4, 1, 
                      ifelse(survey2$pid7 >4 & survey2$pid7 <= 7, 0, NA))

# turn wave into a factor
survey$wave.f <- factor(survey$wave)
survey2$wave.f <- factor(survey2$wave)

########  modeling  ##########

m1 <- lmer(auc ~ swing_3 + wave.f + ideo7 + feel + evid + tip + 
              age + educ + gender + (1| caseid), survey2,
            REML = F)

m2 <- lmer(auc ~ swing_3 * ideo7 + wave.f + feel + evid + tip + 
              age + educ + gender + (1| caseid), survey2,
            REML = F)

m3 <- lmer (auc ~ swing_3 + wave.f + ideo7 + feel + evid + tip + 
              age + educ + gender + (1| caseid), survey,
            REML = F)

m4 <- lmer (auc ~ swing_3 * ideo7 + wave.f + feel + 
              evid + tip + age + educ + gender + (1| caseid), survey,
            REML = F)

m5 <- lmer (auc ~ outgroup + dem + wave.f + ideo7 + feel + evid + tip + 
              age + educ + gender + (1| caseid), 
            survey2,
            REML = F)

m6 <- lmer (auc ~ outgroup * ideo7 + dem + wave.f + ideo7 + feel + evid + tip + 
              age + educ + gender + (1| caseid), 
            survey2,
            REML = F)

m7 <- lmer (auc ~ outgroup + dem + wave.f+ ideo7 + feel + evid + tip + 
              age + educ + gender + (1| caseid), 
            survey,
            REML = F)

m8 <- lmer (auc ~ outgroup * ideo7 + dem + wave.f + ideo7 + feel + evid + tip + 
              age + educ + gender + (1| caseid), 
            survey,
            REML = F)

########  Table 1   ##########
# relationship between sensitivity and battleground state status
# coefficients for wave indicators are omitted in Table 1

screenreg(list(m1, m2, m3, m4), omit.coef = "wave")

########  Table 2  ##########
# relationship between sensitivity and partisan segregation
# coefficients for wave indicators are omitted in Table 2

screenreg(list(m5,m6,m7,m8), omit.coef = "wave")


########  Figure 1  ##########
# predicted sensitivity at varying values of political ideology 
# and battleground state status

# recorder battleground state status for the plot

survey2$swing_v2 <- factor(survey2$swing3,
                         levels = c(0,1,2),
                         labels = c("Solid/Leaning Democratic", "Battleground",
                                    "Solid/Leaning Republican"))

m2.2 <- lmer(auc ~ swing_v2 * ideo7 + wave.f + feel + evid + tip + 
              age + educ + gender + (1| caseid), survey2,
            REML = F)

auc_swing3_ideo <- ggpredict(m2.2, terms = c("ideo7","swing_v2", "gender [2]"))

plot(auc_swing3_ideo) +
  ylab("Predicted Sensitivity") +
  xlab("Political Ideology (Individual)") +
  ggtitle("") +
  scale_x_continuous(breaks = 1:7, labels = c("Very\nLiberal", "Liberal", "Lean\nLiberal", "Moderate", 
                                              "Lean\nConservative", "Conservative", "Very\nConservative")) + 
  scale_color_manual(values = c("#165C7D", "darkorange", "#B2182B"), 
                     labels = c("Solid/Leaning\nDemocratic", "Battleground",
                                "Solid/Leaning\nRepublican")) +
  scale_fill_manual(values = c("#165C7D", "darkorange","#B2182B"), 
                    labels = c("Solid/Leaning\nDemocratic", "Battleground",
                               "Solid/Leaning\nRepublican")) +
  theme_classic() +
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  theme(strip.text.y = element_blank(),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold"),
        axis.text.x = element_text(angle = 30, vjust = 0.9, hjust=1))

########  Figure 2  ##########
# predicted sensitivity by partisan segregation

auc_seg <- ggpredict(m5, terms = c("outgroup", "gender [2]", "dem [1]"))

plot(auc_seg) +
  ylab("Predicted Sensitivity") +
  xlab("Partisan Segregation") +
  ggtitle("") +
  scale_x_continuous(breaks = c(0,0.25, 0.50, 0.75, 1), limits = c(0,1),
                     labels = c("Not\nSegregated", "0.25", "0.50", "0.75",
                                "Segregated")) +
  theme_classic() +
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  theme(strip.text.y = element_blank(),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold"))
