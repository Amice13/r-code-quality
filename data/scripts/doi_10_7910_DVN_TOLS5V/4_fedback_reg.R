# --------------------------------------------------------------------- # 
# Federalism and Democratic Backsliding from a Comparative Perspective 
# Kaufman, Kelemen, Kolcak 
# Appendix 4: Decentralization and Backsliding
# --------------------------------------------------------------------- # 

rm(list = ls())

## set working directory, e.g. 
#setwd("~/Dropbox/fedbacksliding replication")

## Packages
library(tidyverse)
library(stargazer)
library(gghighlight)
library(gridExtra)
library(stringr)
library(reshape2)
library(ggthemes)
library(pander)
library(cowplot)
library(dplyr)
library(foreign)
library(ggeffects)
scale.01 <- function(x){(x-min(x, na.rm =T))/(max(x, na.rm =T)-min(x, na.rm =T))}


## Load dataset for the analysis 
dems_grand <- read.csv('dems_grand.csv')

## Download RAI (1950 - 2018, N = 98)
rai_grand <- tibble(readxl::read_excel("RAI_country-april-2021.xlsx"))

## Compute moving averages for RAI 
rai_grand <- rai_grand %>%
  dplyr::arrange(country_name) %>% 
  dplyr::group_by(country_name) %>% 
  dplyr::mutate(NRAI_05yr_right = zoo::rollmean(n_RAI, k = 5, fill = NA, align = c("right"))) %>% 
  dplyr::mutate(SelfRAI_05yr_right = zoo::rollmean(n_selfrule, k = 5, fill = NA, align = c("right"))) %>% 
  dplyr::ungroup()

## select necessary variables
rai_grand_sub <- rai_grand %>% dplyr::select(country_name, cowcode, year, n_RAI, NRAI_05yr_right, SelfRAI_05yr_right)

## Merge Grand Dems and RAI 
rai_grand_sub <- rai_grand_sub[rai_grand_sub$year >= 1974,]
rai_grand_sub$COWcode <- rai_grand_sub$cowcode

## subset dems grand to necessary variables 
dems_grand_new <- dems_grand %>% dplyr::select(country_name, COWcode, year, fedvsuni, ld, ld_sig2, backslider) 

merged1_study1 <- merge(rai_grand_sub, dems_grand_new, by = c("COWcode", "year"), all.x = T, all.y = F)

## create an indicator for fed vs. backsliding 
merged1_study1$fedvsuni <- as.factor(merged1_study1$fedvsuni)

## scale the moving averages from 0 to 1 
merged1_study1$NRAI_05yr_right_s <- scale.01(merged1_study1$NRAI_05yr_right)
merged1_study1$SelfRAI_05yr_right_s <- scale.01(merged1_study1$SelfRAI_05yr_right)

merged1_study1_2006 <- merged1_study1[merged1_study1$year >= 2006,]

##########
## Fig A1
logit1_rai <- glm(ld_sig2 ~ NRAI_05yr_right_s + fedvsuni, data = merged1_study1,
                 family = binomial(link="logit"))

summary(logit1_rai)

logit1_rai_p <- ggpredict(logit1_rai, terms = list(NRAI_05yr_right_s = seq(0, 1, 0.2)))

ggplot(logit1_rai_p, aes(x, predicted)) + 
  geom_line(linetype = "longdash", color = "blue") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "skyblue") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(limits = c(0, 0.15), breaks = seq(0, 0.15, 0.03)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)) +
  theme(panel.background = element_rect(fill = 'white', color = 'black')) + 
  labs(title = "", 
       caption = "",  
       x = "Moving Average RAI", y = "Predicted Probability of Backsliding") + 
  guides(color = guide_legend(nrow=2)) 

##########
## Fig A2
logit2_rai <- glm(ld_sig2 ~ SelfRAI_05yr_right_s + fedvsuni, data = merged1_study1,
                  family = binomial(link="logit"))

summary(logit2_rai)

logit2_rai_p <- ggpredict(logit2_rai, terms = list(SelfRAI_05yr_right_s = seq(0, 1, 0.2)))

ggplot(logit2_rai_p, aes(x, predicted)) + 
  geom_line(linetype = "longdash", color = "blue") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "skyblue") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(limits = c(0, 0.15), breaks = seq(0, 0.15, 0.03)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)) +
  theme(panel.background = element_rect(fill = 'white', color = 'black')) + 
  labs(title = "", 
       caption = "",  
       x = "Moving Average Self-Rule RAI", y = "Predicted Probability of Backsliding") + 
  guides(color = guide_legend(nrow=2)) 


##########
## Fig A3 
logit3_rai <- glm(ld_sig2 ~ NRAI_05yr_right_s + fedvsuni, data = merged1_study1_2006,
                  family = binomial(link="logit"))

summary(logit3_rai)

logit3_rai_p <- ggpredict(logit3_rai, terms = list(NRAI_05yr_right_s = seq(0, 1, 0.2)))

ggplot(logit3_rai_p, aes(x, predicted)) + 
  geom_line(linetype = "longdash", color = "blue") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "skyblue") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, 0.05)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)) +
  theme(panel.background = element_rect(fill = 'white', color = 'black')) + 
  labs(title = "", 
       caption = "",  
       x = "Moving Average RAI", y = "Predicted Probability of Backsliding") + 
  guides(color = guide_legend(nrow=2)) 


##########
## Fig A4
logit4_rai <- glm(ld_sig2 ~ SelfRAI_05yr_right_s + fedvsuni, data = merged1_study1_2006,
                  family = binomial(link="logit"))

summary(logit4_rai)

logit4_rai_p <- ggpredict(logit4_rai, terms = list(SelfRAI_05yr_right_s = seq(0, 1, 0.2)))

ggplot(logit4_rai_p, aes(x, predicted)) + 
  geom_line(linetype = "longdash", color = "blue") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "skyblue") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, 0.05)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)) +
  theme(panel.background = element_rect(fill = 'white', color = 'black')) + 
  labs(title = "", 
       caption = "",  
       x = "Moving Average Self-Rule RAI", y = "Predicted Probability of Backsliding") + 
  guides(color = guide_legend(nrow=2)) 

##########
## Fig A5
logit5_rai <- glm(backslider ~  NRAI_05yr_right_s + fedvsuni, data = merged1_study1,
                  family = binomial(link="logit"))

summary(logit5_rai)

logit5_rai_p <- ggpredict(logit5_rai, terms = list(NRAI_05yr_right_s = seq(0, 1, 0.2)))

ggplot(logit5_rai_p, aes(x, predicted)) + 
  geom_line(linetype = "longdash", color = "blue") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "skyblue") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(limits = c(0, 0.4), breaks = seq(0, 0.4, 0.1)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)) +
  theme(panel.background = element_rect(fill = 'white', color = 'black')) + 
  labs(title = "", 
       caption = "",  
       x = "Moving Average RAI", y = "Predicted Probability of Backsliding") + 
  guides(color = guide_legend(nrow=2)) 

##########
## Fig A6
logit6_rai <- glm(backslider ~ SelfRAI_05yr_right_s + fedvsuni, data = merged1_study1,
                  family = binomial(link="logit"))


summary(logit6_rai)

logit6_rai_p <- ggpredict(logit6_rai, terms = list(SelfRAI_05yr_right_s = seq(0, 1, 0.2)))

ggplot(logit6_rai_p, aes(x, predicted)) + 
  geom_line(linetype = "longdash", color = "blue") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "skyblue") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(limits = c(0, 0.4), breaks = seq(0, 0.4, 0.1)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)) +
  theme(panel.background = element_rect(fill = 'white', color = 'black')) + 
  labs(title = "", 
       caption = "",  
       x = "Moving Average Self-Rule RAI", y = "Predicted Probability of Backsliding") + 
  guides(color = guide_legend(nrow=2)) 

##########
## Fig A7
logit7_rai <- glm(backslider ~ NRAI_05yr_right_s + fedvsuni, data = merged1_study1_2006,
                  family = binomial(link="logit"))

summary(logit7_rai)

logit7_rai_p <- ggpredict(logit7_rai, terms = list(NRAI_05yr_right_s = seq(0, 1, 0.2)))

ggplot(logit7_rai_p, aes(x, predicted)) + 
  geom_line(linetype = "longdash", color = "blue") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "skyblue") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(limits = c(0, 0.4), breaks = seq(0, 0.4, 0.1)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)) +
  theme(panel.background = element_rect(fill = 'white', color = 'black')) + 
  labs(title = "", 
       caption = "",  
       x = "Moving Average RAI", y = "Predicted Probability of Backsliding") + 
  guides(color = guide_legend(nrow=2)) 


##########
## Fig A8
logit8_rai <- glm(backslider ~ SelfRAI_05yr_right_s + fedvsuni, data = merged1_study1_2006,
                  family = binomial(link="logit"))

summary(logit8_rai)

logit8_rai_p <- ggpredict(logit8_rai, terms = list(SelfRAI_05yr_right_s = seq(0, 1, 0.2)))

ggplot(logit8_rai_p, aes(x, predicted)) + 
  geom_line(linetype = "longdash", color = "blue") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "skyblue") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, 0.1)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)) +
  theme(panel.background = element_rect(fill = 'white', color = 'black')) + 
  labs(title = "", 
       caption = "",  
       x = "Moving Average Self-Rule RAI", y = "Predicted Probability of Backsliding") + 
  guides(color = guide_legend(nrow=2)) 

