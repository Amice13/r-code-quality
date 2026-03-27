# Jaspa PhD - Series 2 - Experiment 1 (Tactile) 

#--------------------#
# Clear Environment  #
#--------------------#
rm(list = ls())
cat("\014")

# Required packages
library(openxlsx)
library(nlme)
library(Rmisc) 
library(ggplot2)
library(gridExtra)
library(lmerTest)
library(data.table)
library(Hmisc)
library(scales)
library(ggplot2)
library(ggeffects)
library(ggthemes)
library(sjPlot)
require(mclogit)
library(nnet)
library(memisc)
library(stats)
require(emmeans)

#-----------------------------#
# Load Experiment 1 Data File #
#-----------------------------#
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# Read the .csv file into a dataframe
data2use <- read.csv("E1_data2use.csv")

#----------------------#
# Mc Logit Multinomial #
#----------------------#

# Multinomial 
data2use <- subset(data2use, bad_eeg == 0)
data2use$Perception2 <- as.factor(data2use$Perception2)
data2use$Perception2 <- relevel(data2use$Perception2, ref = "1")
(mc.GLMM_Cz_N1P2_sub_diff_Multinomial_wSames<- mclogit::mblogit(formula = Perception2 ~ Cz_N1P2_diff, 
                                                                random = ~ 1|subject_id, catCov = c("free"),
                                                                estimator=c("REML"),data = data2use)) #"ML", 
summary(mc.GLMM_Cz_N1P2_sub_diff_Multinomial_wSames)
getSummary(mc.GLMM_Cz_N1P2_sub_diff_Multinomial_wSames,
           alpha=.05,
           rearrange=NULL)
mtable(mc.GLMM_Cz_N1P2_sub_diff_Multinomial_wSames)

# Get EMMEANS for each level of the DV
Perception2_emmeans <- emmeans(mc.GLMM_Cz_N1P2_sub_diff_Multinomial_wSames, specs = ~Cz_N1P2_diff, response.level = "ShockAlone_more_intense")
summary(Perception2_emmeans)

#------------------------------------#
# Plotting with ggPlot and ggPredict #
#------------------------------------#
# Plot 
z <- ggemmeans(mc.GLMM_Cz_N1P2_sub_diff_Multinomial_wSames, terms = "Cz_N1P2_diff") # %>% plot() + ggthemes::theme_tufte()
plot(z) + theme_classic() + xlab("N1 P2 amplitude at Cz (MicroV)") + ylab("Probability of perception (%)")
# Table 
get_model_data(mc.GLMM_Cz_N1P2_sub_diff_Multinomial_wSames, type = c("pred"), terms = "Cz_N1P2_diff [all]")

#-----#
# END #
#-----#
