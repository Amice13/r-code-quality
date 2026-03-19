# Replication Code for "The Restraint Effect of Alliances on Military Responses During Crises" ---- 
# Written by Won-June Hwang, Korea National Defense University(hwj900126@naver.com)

library(dplyr); library(tidyr); library(Hmisc); library(stargazer); library(psych); library(descr)
library(lmtest); library(sandwich)                       # Robust Standard Errors
library(pglm)                                            # Panel Control
library(sampleSelection)                                 # Heckman two stage model
library(ggplot2); library(ggeffects); library(gridExtra) # Plotting Predicted Values
library(effects, quietly = TRUE); library(marginaleffects)
library(DescTools)                                       #PseudoR2 

# Data Read and Summary ----
data<-read.csv("Replication Data1(Dyad_Year).csv", header=T)
data2<-read.csv("Replication Data2(Actor_Level).csv", header=T)

summary(data)
data$crisis <- as.factor(data$crisis)
data$crismg_d<- as.factor(data$crismg_d)          #dependent variable: military response during crisis
data$a_major_ally <- as.factor(data$a_major_ally) #independent variable: alliance with a major power
data$a_defense <- as.factor(data$a_defense)       #independent variable: State A with defense pacts
data$polity_d<-as.factor(data$polity_d)           #control variable: joint democracy
data$contig_d<-as.factor(data$contig_d)           #control variable: contiguity
data$ab_ally<-as.factor(data$ab_ally)             #control variable: security cooperation
data$int_crisis<-as.factor(data$int_crisis)       #control variable: internal crisis
data$triggr_d<-as.factor(data$triggr_d)           #control variable: military trigger

summary(data2)
data2$crismg_d<- as.factor(data2$crismg_d)
data2$a_major_ally <- as.factor(data2$a_major_ally)
data2$a_defense <- as.factor(data2$a_defense)
data2$polity_d<-as.factor(data2$polity_d)
data2$contig_d<-as.factor(data2$contig_d)
data2$ab_ally<-as.factor(data2$ab_ally)
data2$int_crisis<-as.factor(data2$int_crisis)
data2$econdt_d <-as.factor(data2$econdt_d)
data2$gvinst_d <-as.factor(data2$gvinst_d)
data2$triggr_d<-as.factor(data2$triggr_d)

# Table 1. Descriptive Statistics ----

## Data1. Politically Relevant Dyad-Year
nrow(data); nrow(data[data$crisis==1,])
summary(data[data$crisis==1,]$a_major_ally);nrow(data[data$a_major_ally==1 & data$crisis==1,])/nrow(data[data$crisis==1,])*100; 100-nrow(data[data$a_major_ally==1 & data$crisis==1,])/nrow(data[data$crisis==1,])*100
summary(data$a_major_ally);nrow(data[data$a_major_ally==1,])/nrow(data)*100; 100-nrow(data[data$a_major_ally==1,])/nrow(data)*100
summary(data[data$crisis==1,]$a_defense);nrow(data[data$a_defense==1 & data$crisis==1,])/nrow(data[data$crisis==1,])*100; 100-nrow(data[data$a_defense==1 & data$crisis==1,])/nrow(data[data$crisis==1,])*100
summary(data$a_defense);nrow(data[data$a_defense==1,])/nrow(data)*100; 100-nrow(data[data$a_defense==1,])/nrow(data)*100
summary(data[data$crisis==1,]$int_crisis);nrow(data[data$int_crisis==1 & data$crisis==1,])/nrow(data[data$crisis==1,])*100; 100-nrow(data[data$int_crisis==1 & data$crisis==1,])/nrow(data[data$crisis==1,])*100; 
summary(data[data$crisis==1,]$triggr_d);nrow(data[data$triggr_d==1 & data$crisis==1,])/nrow(data[data$crisis==1,])*100; 100-nrow(data[data$triggr_d==1 & data$crisis==1,])/nrow(data[data$crisis==1,])*100
summary(data$polity_d);nrow(data[data$polity_d==1,])/nrow(data)*100; 100-nrow(data[data$polity_d==1,])/nrow(data)*100
summary(data$contig_d);nrow(data[data$contig_d==1,])/nrow(data)*100; 100-nrow(data[data$contig_d==1,])/nrow(data)*100
summary(data$ab_ally);nrow(data[data$ab_ally==1,])/nrow(data)*100; 100-nrow(data[data$ab_ally==1,])/nrow(data)*100
describe(data$cinc_a_ratio)
summary(data$crisis);nrow(data[data$crisis==1,])/nrow(data)*100; 100-nrow(data[data$crisis==1,])/nrow(data)*100
summary(data[data$crisis==1,]$crismg_d);nrow(data[data$crismg_d==1,])/nrow(data[data$crisis==1,])*100; 100-nrow(data[data$crismg_d==1,])/nrow(data[data$crisis==1,])*100

## Data2. ICB-Actor Level Data
nrow(data2)
summary(data2$a_major_ally); nrow(data2[data2$a_major_ally==1,])/nrow(data2)*100; 100-nrow(data2[data2$a_major_ally==1,])/nrow(data2)*100
summary(data2$a_defense); nrow(data2[data2$a_defense==1,])/nrow(data2)*100; 100-nrow(data2[data2$a_defense==1,])/nrow(data2)*100
summary(data2$int_crisis); nrow(data2[data2$int_crisis==1,])/nrow(data2)*100; 100-nrow(data2[data2$int_crisis==1,])/nrow(data2)*100
summary(data2$triggr_d); nrow(data2[data2$triggr_d==1,])/nrow(data2)*100; 100-nrow(data2[data2$triggr_d==1,])/nrow(data2)*100
summary(data2$polity_d); nrow(data2[data2$polity_d==1,])/nrow(data2)*100; 100-nrow(data2[data2$polity_d==1,])/nrow(data2)*100
summary(data2$contig_d); nrow(data2[data2$contig_d==1,])/nrow(data2)*100; 100-nrow(data2[data2$contig_d==1,])/nrow(data2)*100
summary(data2$ab_ally); nrow(data2[data2$ab_ally==1,])/nrow(data2)*100; 100-nrow(data2[data2$ab_ally==1,])/nrow(data2)*100
describe(data2$cinc_a_ratio)

# Table 2. Correlations of Data 2 ----
data_sub2<-data.frame(data2$a_major_ally, data2$a_defense, data2$polity_d, data2$cinc_a_ratio, data2$contig_d, data2$int_crisis, data2$triggr_d)
cor_result2<-rcorr(as.matrix(data_sub2), type="spearman")
cor_result2_2<-data.frame(cor_result2[1])
round(cor_result2_2, 3)

# Table 3. Probit Models ----

## Model 1
model1<-glm(crismg_d ~ a_defense + polity_d + cinc_a_ratio + contig_d + int_crisis + triggr_d, data = data2, family = binomial(link = "probit"))
model1_rb<-coeftest(model1, vcov=vcovHC(model1, type="HC0"))
round(PseudoR2(model1, which='all'), 3)

## Model 2
model2<-glm(crismg_d ~ a_major_ally + polity_d + cinc_a_ratio + contig_d + int_crisis  + triggr_d, data = data2, family = binomial(link = "probit"))
model2_rb<-coeftest(model2, vcov=vcovHC(model2, type="HC0"))
round(PseudoR2(model2, which='all'), 3)

## Model 3
model3<-glm(crismg_d ~ a_defense + a_major_ally + polity_d + cinc_a_ratio + contig_d + int_crisis  + triggr_d, data = data2, family = binomial(link = "probit"))
model3_rb<-coeftest(model3, vcov=vcovHC(model3, type="HC0"))
round(PseudoR2(model3, which='all'), 3)
round(confint(model3, level=0.95), 3)[3,]
round(confint(model3, level=0.90), 3)[3,]

## Model 4
model4<-glm(crismg_d ~ a_major_ally + polity_d + cinc_a_ratio + contig_d + int_crisis  + triggr_d, data = data2[data2$a_defense==1,], family = binomial(link = "probit"))
model4_rb<-coeftest(model4, vcov=vcovHC(model4, type="HC0"))
round(PseudoR2(model4, which='all'), 3)
round(confint(model4, level=0.95), 3)[2,]
round(confint(model4, level=0.90), 3)[2,]

stargazer(model1, model2, model3, model4, type = "text", digits = 3)
stargazer(model1_rb, model2_rb, model3_rb,model4_rb, type = "text", digits = 3)

# Table 4. HSMs ----
pdata<-pdata.frame(data, index=c("directed_dyad", "year"))


## Model 5-1(Selction): PPM-FE
model5_1<-pglm(crisis ~ a_defense  + polity_d + cinc_a_ratio + contig_d + ab_ally, data=pdata, effect="twoways", family=binomial(link="probit"), model="random", index=c("directed_dyad", "year"))
summary(model5_1)
round(coef(model5_1), 3)
x5<-model.matrix(~ a_defense + polity_d + cinc_a_ratio + contig_d + ab_ally, data = pdata)
lp5<-x5 %*% coef(model5_1)[-7]
data$IMR5<-as.numeric(dnorm(lp5)/pnorm(lp5))

## Model 5-2(Outcome): Probit Model with HCSE
model5_2<-glm(crismg_d ~ a_defense + a_major_ally + polity_d + cinc_a_ratio + contig_d + int_crisis + triggr_d + IMR5, data=data[data$crisis==1,], family=binomial(link="probit"))
model5_2_rb<-coeftest(model5_2, vcov=vcovHC(model5_2, type="HC0"))
round(PseudoR2(model5_2, which='all'), 3)
round(confint(model5_2, level=0.95), 3)[3,]
round(confint(model5_2, level=0.90), 3)[3,]
summary(model5_2)

## Model 6-2(Outcome): Probit Model with HCSE
model6_2<-glm(crismg_d ~ a_major_ally + polity_d + cinc_a_ratio + contig_d + int_crisis + triggr_d + IMR5, data=data[data$crisis==1 & data$a_defense==1,], family=binomial(link="probit"))
model6_2_rb<-coeftest(model6_2, vcov=vcovHC(model6_2, type="HC0"))
round(PseudoR2(model6_2, which='all'), 3)
round(confint(model6_2, level=0.95), 3)[2,]
round(confint(model6_2, level=0.90), 3)[2,]

stargazer(model5_2, model6_2, type = "text", digits = 3)
stargazer(model5_2_rb, model6_2_rb, type = "text", digits = 3)

# Table 6. HSMs: Temporal Separation ----
data_sub1<-data[data$year<=1945,]
data_sub2<-data[data$year>=1946 & data$year<=1990,]
data_sub3<-data[data$year>=1991,]
pdata1<-pdata.frame(data_sub1, index=c("directed_dyad", "year"))
pdata2<-pdata.frame(data_sub2, index=c("directed_dyad", "year"))
pdata3<-pdata.frame(data_sub3, index=c("directed_dyad", "year"))

## Model 7-1(Selection): PPM-RE
model7_1<-pglm(crisis ~ a_defense  + polity_d + cinc_a_ratio + contig_d + ab_ally, data=pdata1, effect="twoways", family=binomial(link="probit"), model="random", index=c("directed_dyad", "year"))
summary(model7_1)
round(coef(model7_1), 3)
x7<-model.matrix(~ a_defense + polity_d + cinc_a_ratio + contig_d + ab_ally, data = pdata1)
lp7<-x7 %*% coef(model7_1)[-7]
data_sub1$IMR7<-as.numeric(dnorm(lp7)/pnorm(lp7))

## Model 7-2(Outcome): Probit Model with HCSE
model7_2<-glm(crismg_d ~ a_major_ally + polity_d + cinc_a_ratio + contig_d + int_crisis + triggr_d + IMR7, data=data_sub1[data_sub1$crisis==1 & data_sub1$a_defense==1,], family=binomial(link="probit"))
summary(model7_2)
model7_2_rb<-coeftest(model7_2, vcov=vcovHC(model7_2, type="HC0"))
round(PseudoR2(model7_2, which='all'), 3)

## Model 8-1(Selection): PPM-RE
model8_1<-pglm(crisis ~ a_defense  + polity_d + cinc_a_ratio + contig_d + ab_ally, data=pdata2, effect="twoways", family=binomial(link="probit"), model="random", index=c("directed_dyad", "year"))
summary(model8_1)
round(coef(model8_1), 3)
x8<-model.matrix(~ a_defense + polity_d + cinc_a_ratio + contig_d + ab_ally, data = pdata2)
lp8<-x8 %*% coef(model8_1)[-7]
data_sub2$IMR8<-as.numeric(dnorm(lp8)/pnorm(lp8))

## Model 8-2(Outcome): Probit Model with HCSE
model8_2<-glm(crismg_d ~ a_major_ally + polity_d + cinc_a_ratio + contig_d + int_crisis + triggr_d + IMR8, data=data_sub2[data_sub2$crisis==1 & data_sub2$a_defense==1,], family=binomial(link="probit"))
model8_2_rb<-coeftest(model8_2, vcov=vcovHC(model8_2, type="HC0"))
round(PseudoR2(model8_2, which='all'), 3)
round(confint(model8_2, level=0.95), 3)[2,]
round(confint(model8_2, level=0.90), 3)[2,]

## Model 9-1(Selection): PPM-RE
model9_1<-pglm(crisis ~ a_defense  + polity_d + cinc_a_ratio + contig_d + ab_ally, data=pdata3, effect="twoways", family=binomial(link="probit"), model="random", index=c("directed_dyad", "year"))
summary(model9_1)
round(coef(model9_1), 3)
x9<-model.matrix(~ a_defense + polity_d + cinc_a_ratio + contig_d + ab_ally, data = pdata3)
lp9<-x9 %*% coef(model9_1)[-7]
data_sub3$IMR9<-as.numeric(dnorm(lp9)/pnorm(lp9))

## Model 9-2(Outcome): Probit Model with HCSE
model9_2<-glm(crismg_d ~ a_major_ally + polity_d + cinc_a_ratio + contig_d + int_crisis + triggr_d + IMR9, data=data_sub3[data_sub3$crisis==1 & data_sub3$a_defense==1,], family=binomial(link="probit"))
model9_2_rb<-coeftest(model9_2, vcov=vcovHC(model9_2, type="HC0"))
round(PseudoR2(model9_2, which='all'), 3)

stargazer(model7_2, model8_2, model9_2, type = "text", digits = 3)
stargazer(model7_2_rb, model8_2_rb, model9_2_rb, type = "text", digits = 3)

# Figure 1. Predicted Probabilities ----

data$dp_mp<-ifelse(data$a_defense == 0, 0, ifelse(data$a_major_ally==1, 2, 1))
data2$dp_mp<-ifelse(data2$a_defense == 0, 0, ifelse(data2$a_major_ally==1, 2, 1))
data$dp_mp<-factor(data$dp_mp, order=TRUE)
data2$dp_mp<-factor(data2$dp_mp, ordered=TRUE)
modelA1<-glm(crismg_d ~ dp_mp + polity_d + cinc_a_ratio + contig_d + int_crisis  + triggr_d, data = data2, family = binomial(link = "probit"))
modelA2<-glm(crismg_d ~ dp_mp + polity_d + cinc_a_ratio + contig_d + int_crisis + triggr_d + IMR5, data=data[data$crisis==1,], family=binomial(link="probit"))
round(PseudoR2(modelA1, which='all'), 3)

stargazer(modelA1, modelA2, type = "text", digits = 3)

c6<-ggpredict(modelA1, terms = "dp_mp") %>% 
  plot(ci_style = "ribbon", dot_alpha = 0.5, dot_size = 3, line_size = 0.8, connect_lines=TRUE)+
  ggtitle("Model A1")+
  xlab("DP-MP")+
  ylab("Military Response Probability") + 
  theme(text=element_text(size=12))
c7<-ggpredict(modelA2, terms = "dp_mp") %>% 
  plot(ci_style = "ribbon", dot_alpha = 0.5, dot_size = 3, line_size = 0.8, connect_lines=TRUE)+
  ggtitle("Model A2(Outcome)")+
  xlab("DP-MP")+
  ylab("Military Response Probability") + 
  theme(text=element_text(size=12))
grid.arrange(c6, c7, ncol = 2)

# Table 5. Predicted Probabilities ----
round(c6$data$predicted, 3); round(c6$data$conf.low, 3); round(c6$data$conf.high, 3)
round((c6$data$predicted[2]-c6$data$predicted[1])/c6$data$predicted[1]*100, 1)
round((c6$data$predicted[3]-c6$data$predicted[2])/c6$data$predicted[2]*100, 1)

round(c7$data$predicted, 3); round(c7$data$conf.low, 3); round(c7$data$conf.high, 3)
round((c7$data$predicted[2]-c7$data$predicted[1])/c7$data$predicted[1]*100, 1)
round((c7$data$predicted[3]-c7$data$predicted[2])/c7$data$predicted[2]*100, 1)


# Replication End for "The Restraint Effect of Alliances on Military Responses During Crises" ----