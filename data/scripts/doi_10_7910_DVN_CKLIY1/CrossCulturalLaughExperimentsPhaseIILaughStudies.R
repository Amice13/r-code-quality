# R CODE 
# Cross Cultural Laugh Experiments / Phase II Laugh Studies / Fake Laugh Data R
# Greg Bryant

#setwd("C:/Users/Greg/Desktop/Cross Cultural Laugh Experiments/Phase II Laugh Studies/Fake Laugh Data/R") # Greg home

q1<-read.csv("PerceptionData.csv", header = TRUE)

q1$Condition <- as.factor(q1$Condition)
q1$Sex <- as.factor(q1$Sex)
q1$LangCode <- as.factor(q1$LangCode)
q1$EngFluency <- as.factor(q1$EngFluency)
q1$MMx <- as.factor(q1$MM)
q1$MM_Eng <- as.factor(q1$MM_Eng)
q1$Education <- as.factor(q1$Education)
q1$GenderSeg <- as.factor(q1$GenderSeg)
q1$CommScale <- as.factor(q1$CommScale)
q1$EconMode <- as.factor(q1$EconMode)

q1$Hit <- as.factor(q1$Accuracy)

# MODELS FOR MAIN ANALYSIS OF PERCEPTION OF LAUGHTER ACROSS SOCIETIES

m0 <- glmer(Hit ~ (1|UniqSub) + (1|Trial), data=q1, family = binomial)
m1 <- glmer(Hit ~ (1|UniqSub) + (1|Trial) + Condition, data=q1, family=binomial)
m2 <- glmer(Hit ~ (1|UniqSub) + (1|Trial) + Condition + (1|Country), data=q1, family=binomial)
m3 <- glmer(Hit ~ (1|UniqSub) + (1|Trial) + Condition + Sex, data=q1, family=binomial)
m4 <- glmer(Hit ~ (1|UniqSub) + (1|Trial) + Condition + (1|Country:Condition), data=q1, family=binomial)
m5 <- glmer(Hit ~ (1|UniqSub) + (1|Trial) + Condition + Sex + (1|Country), data=q1, family=binomial)
m6 <- glmer(Hit ~ (1|UniqSub) + (1|Trial) + Condition + Sex + (1|Country:Condition), data=q1, family=binomial)
m7 <- glmer(Hit ~ (1|UniqSub) + (1|Trial) + Condition + Sex + Condition:Sex + (1|Country), data=q1, family=binomial)

# DEMOGRAPHIC ANALYSIS

m1 <- glmer(Response ~ (1|UniqSub) + (1|Trial) + Condition + (1|Country:Condition), data=q1, family=binomial)
m2 <- glmer(Response ~ (1|UniqSub) + (1|Trial) + Condition + (1|Country:Condition) + LangCode, data=q1, family=binomial)
m3 <- glmer(Response ~ (1|UniqSub) + (1|Trial) + Condition + (1|Country:Condition) + EngFluency, data=q1, family=binomial)
m4 <- glmer(Response ~ (1|UniqSub) + (1|Trial) + Condition + (1|Country:Condition) + MM, data=q1, family=binomial)
m5 <- glmer(Response ~ (1|UniqSub) + (1|Trial) + Condition + (1|Country:Condition) + MM_Eng, data=q1, family=binomial)
m6 <- glmer(Response ~ (1|UniqSub) + (1|Trial) + Condition + (1|Country:Condition) + Education, data=q1, family=binomial)
m7 <- glmer(Response ~ (1|UniqSub) + (1|Trial) + Condition + (1|Country:Condition) + CommScale, data=q1, family=binomial)
m8 <- glmer(Response ~ (1|UniqSub) + (1|Trial) + Condition + (1|Country:Condition) + EconMode, data=q1, family=binomial)
