
################################################################# 
# Vladimir Chlouba
# The Ohio State University, Department of Political Science
# chlouba.1@osu.edu
#################################################################
# Liberation Wars as Critical Junctures: Colonial Heritage and
# the Persistence of Inequality (Figures)
#################################################################

# loading the required R packages
library(foreign)
library(ggplot2)
library(stargazer)
library(MASS)
library(caret)
library(ordinal)
library(QuantPsyc)
library(reshape2)
library(plyr)
library(boot)
library(texreg)
library(arm)
library(plyr)
library(nnet)
library(Amelia)
library(effects)
library(Hmisc)
library(gridExtra)
library(MatchIt)
library(rgenoud)
library(Zelig)
library(olsrr)
library(interplot)
library(ggthemes)
library(faraway)
library(lfe)
library(dotwhisker)

rm(list = ls())

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~

# loading the dataset
data <- read.csv("inequality_data.csv")

# subseting for non-Latin American countries
data2 <- subset(data, data$omit == 0) 

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~

###############################################################################
# Figure 1: Inequality, Settlers at Independence, and Liberation Wars
###############################################################################

ggplot(data, aes(x= sett_ind, y= gini_avg, color=interaction(war, war_ind), shape = interaction(war, war_ind), label=country))+
  geom_point(size = 2.5) +  
  geom_text(aes(label=ifelse(sett_ind>0,as.character(country),'')),hjust=-0.1,vjust=-0.1, size=4.9) +
  xlab("Settlers at independence (%)") + ylab("Average Gini (2006-2015)") + theme_bw() +
  theme(text = element_text(size=20), legend.position = "none") 

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~

###############################################################################
# Figure 2: Settler Colonialism and Inequality at Independence (OLS Estimates)
###############################################################################

# creaing lists of variables
iv_list <- c('sett_ind')

control_list_basic <- c('frac+latitude+soil+landlock+log(slave_exports+1)+log(pop_1400+1)+year_ind')

dv_list <- c('gini_ind')

# creating formulas
fmla1 <- as.formula(paste(paste(dv_list[1],"~"),
                          paste(iv_list[1]),
                          paste("| ht_colonial + continent | 0 | region")))

fmla2 <- as.formula(paste(paste(dv_list[1],"~"),
                          paste(iv_list[1], "+"),
                          paste(control_list_basic),
                          paste("| ht_colonial + continent | 0 | region")))

# estimate coefficients
mod.1 <- felm(fmla1, data=data2)
mod.2 <- felm(fmla2, data=data2)

# visualization (whisker plot)
term <- c('DV: Gini at independence', 'DV: Gini at independence')

estimate <- c(mod.1$coefficients[1], mod.2$coefficients[1])

std.error <- c(mod.1$se[1], mod.2$se[1])

model <- c('Model 1', 'Model 2')

whisker.data <- as.data.frame(cbind(term, estimate, std.error, model))

whisker.data$term <- as.character(whisker.data$term)
whisker.data$estimate <- as.numeric(whisker.data$estimate)
whisker.data$std.error <- as.numeric(std.error)

dwplot(whisker.data, dot_args = (list(size = 5, pch = c(19, 4))), 
       whisker_args =  list(lwd = 1)) +
  labs(x = "Settler Population Coefficient", 
       y = "") +
  scale_color_manual(name = "Models", values = c("Model 1" = "gray0", "Model 2" = "gray"), 
                     labels = c("Model 1" = "FEs only", "Model 2" = "FEs + controls")) +
  theme_clean(base_size = 14 ) +
  geom_vline(xintercept = 0, colour = "red", linetype = 5)

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~

###############################################################################
# Figure 3: When Does Independence-era Gini Persist?
###############################################################################

# estimating model
model <- lm(gini_avg ~ gini_ind*wef_pr + neurope+log(gdp_pc)+log(pop)+frac+polity+latitude+minerals+
               sharing_unit+quality_score+ht_colonial+continent, data = data2)

# interaction plot
require(ggthemes)
int1 <- interplot(model, 'gini_ind', 'wef_pr', hist = T) +
  xlab('Property Rights')+
  ylab('Gini at Independence')+
  ggtitle('Coefficient of Independence Gini on Gini (2006-2015) by Prop. Rights')+
  theme_bw()

int1

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
