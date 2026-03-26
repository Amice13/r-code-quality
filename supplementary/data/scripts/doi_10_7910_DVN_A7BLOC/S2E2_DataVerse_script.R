# Jaspa PhD - Series 2 - Experiment 2 (Acoustic) 

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
require(mclogit)
library(nnet)
library(memisc)
library(ggplot2)
library(ggeffects)
library(ggthemes)
library(sjPlot)
require(gamlj)
library(BayesFactor)

#-----------------------------#
# Load Experiment 2 Data File #
#-----------------------------#
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# Read the .csv file into a dataframe
data2use <- read.csv("E2_data2use.csv")

#----------------------#
# Mc Logit Multinomial #
#----------------------#
data2use <- subset(data2use, bad_eeg == 0)
data2use$Perception2 <- as.factor(data2use$Perception2)
data2use$block <- as.factor(data2use$block)
data2use$Perception2 <- relevel(data2use$Perception2, ref = "1")
(GLMM_Cz_N1P2_sub_diff_Multinomial_wSames<- mclogit::mblogit(formula = Perception2 ~ Cz_N1P2_diff + block, 
                                                             random = ~ 1|subject_id,catCov = c("free"),
                                                             estimator=c("REML"),data = data2use)) #, "REML"
summary(GLMM_Cz_N1P2_sub_diff_Multinomial_wSames)
getSummary(GLMM_Cz_N1P2_sub_diff_Multinomial_wSames,
           alpha=.05,
           rearrange=NULL)
mtable(GLMM_Cz_N1P2_sub_diff_Multinomial_wSames)

#------------------------------------#
# Plotting with ggPlot and ggPredict #
#------------------------------------#
z <- ggemmeans(GLMM_Cz_N1P2_sub_diff_Multinomial_wSames, terms = "Cz_N1P2_diff") # %>% plot() + ggthemes::theme_tufte()
plot(z) + theme_classic() + xlab("N1 P2 amplitude at Cz (MicroV)") + ylab("Probability of perception (%)")


#-------------------------------------------------------------#
# LMM to Assess whether N1/P2 is affected by Attentional Load #
#-------------------------------------------------------------#
# Note: "block" = attentional load condition. 
data2use <- subset(data2use,bad_eeg == 0)
data2use$block <-  as.factor(data2use$block)
data2use$Perception2 <-  as.factor(data2use$Perception2)
Cz_N1P2_DV_model<- gamljMixed(data= data2use, formula = Cz_N1P2_diff ~ block * Perception2 + (1|subject_id), 
                              plotError = "se", plotRandomEffects = F,
                              eDesc = T)

Cz_N1P2_DV_model
plot(Cz_N1P2_DV_model, formula = ~ Perception2)
plot(Cz_N1P2_DV_model, formula = ~ block)
data2plot <- (Cz_N1P2_DV_model ~ Condition:Perception3)

#### Following up on the no effect block. This answers the questions of whether cognitive load affects N1/P2 magnitude.NOTE: BF01 = 1/BF10. 
# Note: BF10 is the Bayes factor giving the evidence for H1 over H0. P(D|H1). P(D|H0). Whereas BF01 is evidence for H0 over H1.
bfFull = generalTestBF(Cz_N1P2_diff ~ block * Perception2,
                       data = data2use, whichRandom = "subject_id", whichModels = "bottom")
bfFull # Produces BF10 
likelyhood_of_null_block_effect<- 1/bfFull[1] #Converts to BF1
likelyhood_of_null_block_effect # Produces BF1 

#-----#
# END #
#-----#