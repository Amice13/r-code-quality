# Testing the assumption of "No Design Effects"

# clear workspace
rm(list=ls(all=TRUE))

# load required packages
library(rio)
library(tidyverse)
library(texreg)

# get data 
df <- rio::import("data/main-data.csv")

#create binary variables of the direct question, as used in main analysis with dk dropped, 
# and one which 1 = DK, 0 = any answer

df <- df %>% 
  mutate(
    ConfidenceDirectDK = ifelse(ConfidenceDirect %in% 1:2, 0, 
                                ifelse(ConfidenceDirect == 3, 1, ConfidenceDirect)),
    SystemDirectDK = ifelse(SystemDirect %in% 1:2, 0, 
                            ifelse(SystemDirect == 3, 1, SystemDirect)),
    CorruptionDirectDK = ifelse(CorruptionDirect %in% 1:2, 0, 
                                ifelse(CorruptionDirect == 3, 1, CorruptionDirect)),
    CensorDirectDK = ifelse(CensorDirect %in% 1:2, 0,
                            ifelse(CensorDirect == 3, 1, CorruptionDirect)))

#regress DK on direct question on group to see priming effects given that we asked direct question to all

confidence.dk.test <- glm(ConfidenceDirectDK ~ Group, family = binomial(link = logit), data = df)      
system.dk.test <- glm(SystemDirectDK ~ Group, family = binomial(link=logit), data = df)      
corruption.dk.test <- glm(CorruptionDirectDK ~ Group, family = binomial(link=logit), data = df)  
censor.dk.test <- glm(CensorDirectDK ~ Group, family = binomial(link=logit), data = df)      


#Export table

texreg(list(confidence.dk.test,system.dk.test,corruption.dk.test,censor.dk.test), 
       file = "output/dk-priming.tex", custom.model.names = c("Confidence", "System", "Corruption", "Censor"), 
       table = FALSE)

  

# in the main analysis we recode Direct Questions to binary (DK coded as NA)

df$ConfidenceDirect[df$ConfidenceDirect == 3] <- NA
df$ConfidenceDirect[df$ConfidenceDirect == 2] <- 0

df$SystemDirect[df$SystemDirect == 3] <- NA
df$SystemDirect[df$SystemDirect == 2] <- 0

df$CorruptionDirect[df$CorruptionDirect == 3] <- NA
df$CorruptionDirect[df$CorruptionDirect == 2] <- 0

df$CensorDirect[df$CensorDirect == 3] <- NA
df$CensorDirect[df$CensorDirect == 2] <- 0



#regress direct question on group to see if priming effects given that we asked direct question to all

confidence.prime.test <- glm(ConfidenceDirect ~ Group, family = binomial(link = logit), data = df)      
system.prime.test <- glm(SystemDirect ~ Group, family = binomial(link=logit), data = df)      
corruption.prime.test <- glm(CorruptionDirect ~ Group, family = binomial(link=logit), data = df)  
censor.prime.test <- glm(CensorDirect ~ Group, family = binomial(link=logit), data = df)      


#Export table

texreg(list(confidence.prime.test,system.prime.test,corruption.prime.test,censor.prime.test), 
       file = "output/priming.tex", custom.model.names = c("Confidence", "System", "Corruption", "Censor"), 
       table = FALSE)





