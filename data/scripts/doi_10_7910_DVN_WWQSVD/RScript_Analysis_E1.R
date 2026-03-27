############################################################
# Definition of variable names in data sets
# ItemID = the unique item identifier
# SpkrID = the unique speaker identification number 
# Word = German-like word
# ExpCond = Word Learning Condition (No Orthography vs. Orthography)
# UnderlyingVoicing = Underlying voicing of the word (voiced vs. voiceless)
# MorphCond = Morphological condition (plural vs. singular)
# Short = experiment condition (crossing the three factors of interest)
# CoderID = the unique coder identification number
# Acc = Accuracy (1 = correct, 0 = incorrect)
# Voiceless = Voicing judgment by coder (1 = voiceless, 0=voiced)
############################################################

############################################################
#Load data file and examine structure
############################################################
E1_data<-read.csv("data_E1.csv")
str(E1_data)
############################################################

############################################################
# Load libraries for analysis
############################################################
library(lme4)
library(multcomp)
library(car)
############################################################

############################################################
# Hand-code contrast coding for the factors:
# 'UnderlyingVoicing, 'MorphCond', and 'ExpCond'
############################################################
E1_data$UnderlyingVoicing.contrast <- ifelse(E1_data$UnderlyingVoicing == "voiced", -0.5, 0.5)
E1_data$MorphCond.contrast <- ifelse(E1_data$MorphCond == "plural", -0.5, 0.5)
E1_data$ExpCond.contrast <- ifelse(E1_data$ExpCond == "No Orthography", -0.5, 0.5)
############################################################

############################################################
### Mixed effects model omnibus analysis examining the effects of 
### Underlying Voicing, Morphological Condition, and Word Learning Condition
###  on Voiceless productions (binary outcome) with full random effects 
### structure by SpkrID, Word, CoderID
############################################################
m.voiceless <- glmer(data = E1_data, 
                     formula = Voiceless ~ UnderlyingVoicing.contrast*MorphCond.contrast*ExpCond.contrast +
                       (1 + UnderlyingVoicing.contrast + MorphCond.contrast|SpkrID) + 
                       (1 + ExpCond.contrast + MorphCond.contrast|Word)+ 
                       (1 + UnderlyingVoicing.contrast + MorphCond.contrast + ExpCond.contrast|CoderID),
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa"))
summary(m.voiceless)

############################################################
# Setting up the contrasts for multiple comparisons
############################################################
contrasts <- rbind(
  #Simple effects of Morphological Condition
  "Devoicing of NoOrth_voiced" = c(-1, 1, 0, 0, 0, 0, 0, 0), 
  "Devoicing of Orth_voiced" = c(0, 0, 0, 0, -1, 1, 0, 0),
  "Devoicing of NoOrth_voiceless" = c(0, 0, -1, 1, 0, 0, 0, 0),
  "Devoicing of Orth_voiceless" = c(0, 0, 0, 0, 0, 0, -1, 1),
  
  #Simple effects of Word Learning Condition
  "Group diff in Voiced plural" = c(-1, 0, 0, 0, 1, 0, 0, 0),
  "Group diff in Voiced singular" = c(0, -1, 0, 0, 0, 1, 0, 0),
  "Group diff in Voiceless plural" = c(0, 0, -1, 0, 0, 0, 1, 0),
  "Group diff in Voiceless singular" = c(0, 0, 0, -1, 0, 0, 0, 1))

############################################################
# Conduct multiple comparisons
############################################################
m.voiceless.short<-glmer(data = E1_data,
                         formula = Voiceless ~ Short +
                           (1 + UnderlyingVoicing.contrast + MorphCond.contrast|SpkrID) +
                           (1 + ExpCond.contrast + MorphCond.contrast|Word)+
                           (1 + UnderlyingVoicing.contrast + MorphCond.contrast + ExpCond.contrast|CoderID),
                         family = binomial,
                         control = glmerControl(optimizer = "bobyqa"))
m.voiceless.contrasts <- glht(model = m.voiceless.short, 
                              linfct = mcp(Short=contrasts), test = adjusted("bonferroni"))
summary(m.voiceless.contrasts)
############################################################


############################################################
### Mixed effects model analysis examining the effects of Underlying Voicing, 
### and Morphological Condition on Voiceless productions (binary outcome) 
###  for the No Orthography group only with full random effects structure
###  by SpkrID, Word, CoderID
############################################################
m.voiceless.noorth <- glmer(data = E1_data[E1_data$ExpCond == "No Orthography",], 
                            formula = Voiceless ~ UnderlyingVoicing.contrast*MorphCond.contrast +
                              (1 + UnderlyingVoicing.contrast + MorphCond.contrast|SpkrID) + 
                              (1 + MorphCond.contrast|Word)+ 
                              (1 + UnderlyingVoicing.contrast + MorphCond.contrast|CoderID),
                            family = binomial,
                            control = glmerControl(optimizer = "bobyqa"))
summary(m.voiceless.noorth)

############################################################
### Mixed effects model analysis examining the effects of Underlying Voicing, 
### and Morphological Condition on Voiceless productions (binary outcome) 
###  for the Orthography group only with full random effects structure
###  by SpkrID, Word, CoderID
############################################################
m.voiceless.orth <- glmer(data = E1_data[E1_data$ExpCond == "Orthography",], 
                          formula = Voiceless ~ UnderlyingVoicing.contrast*MorphCond.contrast +
                            (1 + UnderlyingVoicing.contrast + MorphCond.contrast|SpkrID) + 
                            (1 + MorphCond.contrast|Word)+ 
                            (1 + UnderlyingVoicing.contrast + MorphCond.contrast|CoderID),
                          family = binomial,
                          control = glmerControl(optimizer = "bobyqa"))
summary(m.voiceless.orth)
