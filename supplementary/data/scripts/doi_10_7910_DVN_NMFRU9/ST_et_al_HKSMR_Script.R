#############################################################
#############################################################
#############################################################

#### Code to preprocess, and MLM data analysis - Climate Truth Discernment Score secondary dataset analysis
 

#### Packages####
library("Cairo")
library(faux)
library(DHARMa)
library(glmmTMB)
library(visreg)
library(r2glmm)
library(MetBrewer)
library(raincloudplots)
library(Rmisc)
library(emmeans)
library(MSwM)
library(plyr)
library(cocor)
library(sandwich)
library(QuantPsyc)
library(Hmisc)
library(mlogit)
library(psych)
library(gmodels)
library(tidyverse)
library (DescTools)
library(car)
library (pscl)
library (stats)
library (MASS)
library(effects)
library(sjPlot)
library(sjmisc)
library(ggplot2)
library(MuMIn)
library(lavaan)
library(TOSTER)
library(yhat)
library(lme4)
library(lmerTest)
library(lavaanPlot)
library(rethinking)
library(ggdist)
library(cowplot)
set_theme(  theme(panel.background = element_rect(fill = "white", colour = "grey50")))
devtools::source_url("https://RAW.githubusercontent.com/yjunechoe/geom_paired_raincloud/master/geom_paired_raincloud.R")

tdiagnostic <- function(merMod) {
  var.d <- crossprod(getME(merMod,"Lambdat"))
  Zt <- getME(merMod,"Zt")
  vr <- sigma(merMod)^2
  var.b <- vr*(t(Zt) %*% var.d %*% Zt)
  sI <- vr * Diagonal(nrow(merMod@frame))
  var.y <- var.b + sI
  Li <- t(chol(var.y))
  tres <- as.vector(solve(Li) %*% residuals(merMod))
  tfit <- as.vector(solve(Li) %*% fitted(merMod))
  data.frame(tres,tfit)
}
#### Clean slate. ####
rm(list = ls())
#### --------------------- Data analysis section ----------------------- #####

#Preregistered dataset

Showdown<-read.csv("C:\\...\\Showdown.csv", header=TRUE)
Showdown <- Showdown[Showdown$Condition==-1 | Showdown$Condition == 0,]

n <- as.integer(length(Showdown$F_Delay_2_R))

for (q in 1:n) {
  #exploratory MIST - Signal Detection Theory Signal Detection Theory (Batailler et al, 2022 https://doi.org/10.1177/1745691620986135)
  Showdown$MIST_hitrate[q] <- (Showdown$T_Support[q] + Showdown$T_Delay[q] + 1) / 12
  Showdown$MIST_falsealarm[q] <- (sum(Showdown$F_Support_1[q], Showdown$F_Support_2[q], Showdown$F_Support_3[q], Showdown$F_Support_4[q], Showdown$F_Support_5[q]) + sum(Showdown$F_Delay_1[q], Showdown$F_Delay_2[q], Showdown$F_Delay_3[q], Showdown$F_Delay_4[q], Showdown$F_Delay_5[q]) + 1 ) / 12
  Showdown$MIST_dprime[q] <- qnorm(Showdown$MIST_hitrate[q]) - qnorm(Showdown$MIST_falsealarm[q]);

  Showdown$MIST_D_hitrate[q] <- (Showdown$T_Delay[q] + 0.5) / 6
  Showdown$MIST_D_falsealarm[q] <- sum(Showdown$F_Delay_1[q], Showdown$F_Delay_2[q], Showdown$F_Delay_3[q], Showdown$F_Delay_4[q], Showdown$F_Delay_5[q] + 0.5) / 6
  Showdown$MIST_D_dprime[q] <- qnorm(Showdown$MIST_D_hitrate[q]) - qnorm(Showdown$MIST_D_falsealarm[q]);

  Showdown$MIST_S_hitrate[q] <- (Showdown$T_Support[q] + 0.5) / 6
  Showdown$MIST_S_falsealarm[q] <- sum(Showdown$F_Support_1[q], Showdown$F_Support_2[q], Showdown$F_Support_3[q], Showdown$F_Support_4[q], Showdown$F_Support_5[q] + 0.5) / 6
  Showdown$MIST_S_dprime[q] <- qnorm(Showdown$MIST_S_hitrate[q]) - qnorm(Showdown$MIST_S_falsealarm[q]);
}

n <- length(Showdown$ui)
p<-n-1;
w<-n*20; #n of observations
MIST_Long_MLM<-matrix(1, w);
MIST_Long_MLM<-data.frame(MIST_Long_MLM);

names(MIST_Long_MLM)[names(MIST_Long_MLM) == "MIST_Long_MLM"] <- "Participant"

for (i in 0:p) { 
  m<-i+1;
  o<-i*20;
  
  #Participant section of the loop
  MIST_Long_MLM$Participant[(1+o):(20+o)]<-m;
  MIST_Long_MLM$Politics[(1+o):(20+o)]<-Showdown$Pol_ideo[m];
  MIST_Long_MLM$Country[(1+o):(20+o)]<-Showdown$Country[m];
  MIST_Long_MLM$Age[(1+o):(20+o)]<-Showdown$Age[m];
  MIST_Long_MLM$Gender[(1+o):(20+o)]<-Showdown$Gender[m];
  
  MIST_Long_MLM$Condition[(1+o):(20+o)]<-Showdown$Condition[m];

  MIST_Long_MLM$MIST[1+o] <- Showdown$T_Support_1[m];
  MIST_Long_MLM$MIST[2+o] <- Showdown$T_Support_2[m];
  MIST_Long_MLM$MIST[3+o] <- Showdown$T_Support_3[m];
  MIST_Long_MLM$MIST[4+o] <- Showdown$T_Support_4[m];
  MIST_Long_MLM$MIST[5+o] <- Showdown$T_Support_5[m];
  MIST_Long_MLM$MIST[6+o] <- Showdown$T_Delay_1[m];
  MIST_Long_MLM$MIST[7+o] <- Showdown$T_Delay_2[m];
  MIST_Long_MLM$MIST[8+o] <- Showdown$T_Delay_3[m];
  MIST_Long_MLM$MIST[9+o] <- Showdown$T_Delay_4[m];
  MIST_Long_MLM$MIST[10+o] <- Showdown$T_Delay_5[m];
  MIST_Long_MLM$MIST[11+o] <- Showdown$F_Support_1[m];
  MIST_Long_MLM$MIST[12+o] <- Showdown$F_Support_2[m];
  MIST_Long_MLM$MIST[13+o] <- Showdown$F_Support_3[m];
  MIST_Long_MLM$MIST[14+o] <- Showdown$F_Support_4[m];
  MIST_Long_MLM$MIST[15+o] <- Showdown$F_Support_5[m];
  MIST_Long_MLM$MIST[16+o] <- Showdown$F_Delay_1[m];
  MIST_Long_MLM$MIST[17+o] <- Showdown$F_Delay_2[m];
  MIST_Long_MLM$MIST[18+o] <- Showdown$F_Delay_3[m];
  MIST_Long_MLM$MIST[19+o] <- Showdown$F_Delay_4[m];
  MIST_Long_MLM$MIST[20+o] <- Showdown$F_Delay_5[m];
  
  MIST_Long_MLM$MIST_TF[(1+o):(10+o)] <- "True";
  MIST_Long_MLM$MIST_TF[(11+o):(20+o)] <- "False";
  
  MIST_Long_MLM$Type[1+o] <- 1; #disinformation type counter
  MIST_Long_MLM$Type[2+o] <- 2;
  MIST_Long_MLM$Type[3+o] <- 3;
  MIST_Long_MLM$Type[4+o] <- 4;
  MIST_Long_MLM$Type[5+o] <- 5;
  MIST_Long_MLM$Type[6+o] <- 6;
  MIST_Long_MLM$Type[7+o] <- 7;
  MIST_Long_MLM$Type[8+o] <- 8;
  MIST_Long_MLM$Type[9+o] <- 9;
  MIST_Long_MLM$Type[10+o] <- 10;
  MIST_Long_MLM$Type[11+o] <- 11;
  MIST_Long_MLM$Type[12+o] <- 12;
  MIST_Long_MLM$Type[13+o] <- 13;
  MIST_Long_MLM$Type[14+o] <- 14;
  MIST_Long_MLM$Type[15+o] <- 15;
  MIST_Long_MLM$Type[16+o] <- 16;
  MIST_Long_MLM$Type[17+o] <- 17;
  MIST_Long_MLM$Type[18+o] <- 18;
  MIST_Long_MLM$Type[19+o] <- 19;
  MIST_Long_MLM$Type[20+o] <- 20;
  
}

MIST_Long_MLM$Grouping <- as.factor(ifelse(MIST_Long_MLM$Type<6 | (MIST_Long_MLM$Type>10 & MIST_Long_MLM$Type<16), "Support", "Delay"));

MIST_Long_MLM$Condition<-as.factor(MIST_Long_MLM$Condition);
MIST_Long_MLM$Gender<-as.factor(MIST_Long_MLM$Gender);

#Climate Truth Discernment Score breakdown across combinations of factors in the Climate Truth Discernment Task
n <- length(Showdown$ui)
p <- n-1;
w <- n*4; #n of observations

#Creating Showdown matrices for column-row transition
MIST_MLM <- matrix(1, w);
MIST_MLM <- data.frame(MIST_MLM);
names(MIST_MLM)[names(MIST_MLM) == "MIST_MLM"]  <-  "ui"

for (i in 0:p) { 
  m <- i+1;
  o <- i*4;
  MIST_MLM$ui[(1+o):(4+o)] <- Showdown$ui[m];
  MIST_MLM$Participant[(1+o):(4+o)] <- m;
  MIST_MLM$Oneline[1+o] <- 1;
  MIST_MLM$Oneline[(2+o):(4+o)] <- 0;
  MIST_MLM$Politics[(1+o):(4+o)] <- Showdown$Pol_ideo[m];
  MIST_MLM$Pol_Identity[(1+o):(4+o)]<-ifelse(Showdown$Pol_ideo[m]<3, "1. Far-left",
                                             ifelse(Showdown$Pol_ideo[m]>8, "5. Far-right",
                                                    ifelse((Showdown$Pol_ideo[m]>6 & Showdown$Pol_ideo[m]<9), "4. Right",
                                                           ifelse((Showdown$Pol_ideo[m]>2 & Showdown$Pol_ideo[m]<5), "2. Left", "3. Center"
                                                           ))));
  MIST_MLM$Education[(1+o):(4+o)] <- Showdown$Education[m];
  MIST_MLM$Age[(1+o):(4+o)] <- Showdown$Age[m];
  MIST_MLM$Gender[(1+o):(4+o)] <- Showdown$Gender[m];
  MIST_MLM$Country[(1+o):(4+o)] <- Showdown$Country[m];
  
  #Condition assignment
  MIST_MLM$Condition[(1+o):(4+o)] <- Showdown$Condition[m];

  MIST_MLM$MIST[1+o] <- Showdown$T_Support[m];
  MIST_MLM$MIST[2+o] <- Showdown$T_Delay[m];
  MIST_MLM$MIST[3+o] <- Showdown$F_Support[m];
  MIST_MLM$MIST[4+o] <- Showdown$F_Delay[m];
  
  MIST_MLM$MIST_R[1+o] <- Showdown$T_Support[m];
  MIST_MLM$MIST_R[2+o] <- Showdown$T_Delay[m];
  MIST_MLM$MIST_R[3+o] <- Showdown$F_Support_R[m];
  MIST_MLM$MIST_R[4+o] <- Showdown$F_Delay_R[m];
  
  MIST_MLM$MIST_SDT[1+o] <- Showdown$MIST_S_hitrate[m];
  MIST_MLM$MIST_SDT[2+o] <- Showdown$MIST_D_hitrate[m];
  MIST_MLM$MIST_SDT[3+o] <- Showdown$MIST_S_falsealarm[m];
  MIST_MLM$MIST_SDT[4+o] <- Showdown$MIST_D_falsealarm[m];
  
  MIST_MLM$MIST_TF[(1+o):(2+o)] <- "True";
  MIST_MLM$MIST_TF[(3+o):(4+o)] <- "False";
  
  MIST_MLM$MIST_HRFA[(1+o):(2+o)] <- "Hit rate";
  MIST_MLM$MIST_HRFA[(3+o):(4+o)] <- "False alarm";
  
  MIST_MLM$MIST_Type[1+o] <- "T - Support";
  MIST_MLM$MIST_Type[3+o] <- "T - Delay";
  MIST_MLM$MIST_Type[2+o] <- "F - Support";
  MIST_MLM$MIST_Type[4+o] <- "F - Delay";
  
  MIST_MLM$Grouping[1+o] <- "Support";
  MIST_MLM$Grouping[2+o] <- "Delay";
  MIST_MLM$Grouping[3+o] <- "Support";
  MIST_MLM$Grouping[4+o] <- "Delay";
}

MIST_MLM$MIST_Prob <- MIST_MLM$MIST/5
MIST_MLM$Gender <- as.factor(MIST_MLM$Gender)
MIST_MLM$Condition <- as.factor(MIST_MLM$Condition)
MIST_MLM$MIST_TF <- as.factor(MIST_MLM$MIST_TF)
MIST_MLM$MIST_Type <- as.factor(MIST_MLM$MIST_Type)
MIST_MLM$Grouping <- as.factor(MIST_MLM$Grouping)
MIST_MLM$MIST_HRFA <- as.factor(MIST_MLM$MIST_HRFA)
MIST_MLM$Pol_Identity <- as.factor(MIST_MLM$Pol_Identity)

#Creating Showdown matrices for column-row transition - Signal Detection Theory (Batailler et al, 2022 https://doi.org/10.1177/1745691620986135)


MIST_SDT <- matrix(1, w);
MIST_SDT <- data.frame(MIST_SDT);
names(MIST_SDT)[names(MIST_SDT) == "MIST_SDT"]  <-  "ui"

for (i in 0:p) { 
  m <- i+1;
  o <- i*2;
  MIST_SDT$ui[(1+o):(2+o)] <- Showdown$ui[m];
  MIST_SDT$Participant[(1+o):(2+o)] <- m;
  MIST_SDT$Oneline[1+o] <- 1;
  MIST_SDT$Oneline[(2+o):(2+o)] <- 0;
  MIST_SDT$Politics[(1+o):(2+o)] <- Showdown$Pol_ideo[m];
  MIST_SDT$Education[(1+o):(2+o)] <- Showdown$Education[m];
  MIST_SDT$Age[(1+o):(2+o)] <- Showdown$Age[m];
  MIST_SDT$Gender[(1+o):(2+o)] <- Showdown$Gender[m];
  MIST_SDT$CRT[(1+o):(2+o)] <- Showdown$CRT[m];
  MIST_SDT$Country[(1+o):(2+o)] <- Showdown$Country[m];

  #Condition assignment
  MIST_SDT$Condition[(1+o):(2+o)] <- Showdown$Condition[m];

  MIST_SDT$dprime[1+o] <- Showdown$MIST_S_dprime[m];
  MIST_SDT$dprime[2+o] <- Showdown$MIST_D_dprime[m];
  
  MIST_SDT$falsealarm[1+o] <- Showdown$MIST_S_falsealarm[m];
  MIST_SDT$falsealarm[2+o] <- Showdown$MIST_D_falsealarm[m];
  
  MIST_SDT$hitrate[1+o] <- Showdown$MIST_S_hitrate[m];
  MIST_SDT$hitrate[2+o] <- Showdown$MIST_D_hitrate[m];
  
  
  MIST_SDT$Grouping[1+o] <- "Support";
  MIST_SDT$Grouping[2+o] <- "Delay";
}

MIST_SDT$Condition <- as.factor(MIST_SDT$Condition)
MIST_SDT$Grouping <- as.factor(MIST_SDT$Grouping)
MIST_SDT$Gender <- as.factor(MIST_SDT$Gender)
MIST_SDT$Country <- as.factor(MIST_SDT$Country)

#### Figures -Climate Truth Discernment Score -- item-level breakdown ####

MIST_Long<-summarySE(MIST_Long_MLM[MIST_Long_MLM$Condition== -1,], measurevar="MIST", groupvars=c("Type", "Grouping"))

jpeg(file="C:\\Users\\spampatt\\Desktop\\Article_Submissions\\Showdown_data\\Images\\HMR_MIST_Items.jpeg",width= 180,height=105, units = "mm", pointsize= 15, res=300)
MIST_item_Plot<-ggplot(MIST_Long_MLM[MIST_Long_MLM$Condition== -1,], aes(x = as.factor(Type), y = MIST, fill = Grouping)) +
  geom_errorbar(data = MIST_Long, aes(x = as.factor(Type), y = MIST, group = Grouping, colour = Grouping, ymin = MIST-ci, ymax = MIST+ci), size=0.5, width = .05) +
  stat_summary(fun=mean, geom="point", size=0.5, position =position_dodge()) +
  theme_bw() +
  theme(text=element_text(size = 15, face="bold")) +
  labs(color = "Grouping") +
  scale_color_manual(values=c("black", "seagreen", "black","seagreen")) +
  theme(panel.border = element_blank(), axis.line = element_line()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Climate statement identification number", y = "Probability of being reported true") + 
  geom_hline(yintercept = 0.50) +
  geom_vline(xintercept = 10.50, linetype = "dotdash") +
  labs(title="Probability of each statement being considered true") + 
  theme(plot.title = element_text(size=12, face="bold", family = "sans"), plot.subtitle = element_text(size=12, face="bold", family = "sans"), axis.text.x = element_text(size=8, angle = 0, vjust = 0.5, hjust=0.5, family = "sans"), axis.title = element_text(size=10)) +
  scale_y_continuous(limits = c (-0.1, 1.1), breaks = c(0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1)) +
  theme(legend.title = element_text(size=10)) + theme(legend.text = element_text(size=10)) +  theme(legend.key.size = unit(0.75, 'cm'))
MIST_item_Plot
dev.off()

#### Figures - Climate Truth Discernment score -- Political ideology ####

MIST_score_delay <- ggplot(MIST_MLM[MIST_MLM$Condition==-1 & MIST_MLM$Grouping=="Delay",], aes(x = Politics, y = MIST_Prob, color = MIST_TF)) +
  geom_jitter( alpha=0.15) +
  geom_smooth(aes(x = Politics, y = MIST_Prob, color = MIST_TF), method = "lm") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(color = "Truth") +
  scale_fill_manual(values= c("#e34234", "#248721")) +
  scale_color_manual(values= c("#e34234", "#248721")) +
  theme(panel.border = element_blank(), axis.line = element_line()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Conservative political ideology", y = "Probability of reporting a statement as true") +
  labs(title="Political ideology on Delay discernment - Control") + 
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) + 
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) + 
  theme(plot.title = element_text(size = 8,  family = "sans"), axis.text.y = element_text(size = 6, angle = 0, vjust = 0.5, hjust=0.5, family = "sans"), axis.text.x = element_text(size = 6, angle = 0, vjust = 0.5, hjust=0.5,family = "sans"), axis.title = element_text(size = 7))
MIST_score_delay

MIST_score_Support <- ggplot(MIST_MLM[MIST_MLM$Condition==-1 & MIST_MLM$Grouping=="Support",], aes(x = Politics, y = MIST_Prob, color = MIST_TF)) +
  geom_jitter( alpha=0.15) +
  geom_smooth(aes(x = Politics, y = MIST_Prob, color = MIST_TF), method = "lm") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.2), legend.text = element_text(size=5, family = "sans"), legend.title = element_blank(), legend.key.size = unit(0.3, "cm")) +
  labs(color = "Truth") +
  scale_fill_manual(values= c("#e34234", "#248721")) +
  scale_color_manual(values= c("#e34234", "#248721")) +
  theme(panel.border = element_blank(), axis.line = element_line()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Conservative political ideology", y = "Probability of reporting a statement as true") +
  labs(title="Political ideology on Support discernment - Control") + 
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) + 
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) + 
  theme(plot.title = element_text(size = 8,  family = "sans"), axis.text.y = element_text(size = 6, angle = 0, vjust = 0.5, hjust=0.5, family = "sans"), axis.text.x = element_text(size = 6, angle = 0, vjust = 0.5, hjust=0.5,family = "sans"), axis.title = element_text(size = 7))
MIST_score_Support

cow_DVs_control <- plot_grid(MIST_score_delay, MIST_score_Support, ncol=2, align = "v", axis="1")
cow_DVs_control
jpeg(file="C:\\Users\\spampatt\\Desktop\\Article_Submissions\\Showdown_data\\Images\\HMR_MIST_Score_Pure.jpeg",width= 180,height=105, units = "mm", pointsize= 15, res=300)
cow_DVs_control
dev.off()


MIST_score_delay <- ggplot(MIST_MLM[MIST_MLM$Condition==0 & MIST_MLM$Grouping=="Delay",], aes(x = Politics, y = MIST_Prob, color = MIST_TF)) +
  geom_jitter( alpha=0.15) +
  geom_smooth(aes(x = Politics, y = MIST_Prob, color = MIST_TF), method = "lm") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(color = "Truth") +
  scale_fill_manual(values= c("#e34234", "#248721")) +
  scale_color_manual(values= c("#e34234", "#248721")) +
  theme(panel.border = element_blank(), axis.line = element_line()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Conservative political ideology", y = "Probability of reporting a statement as true") +
  labs(title="Political ideology on Delay discernment - Disinformation") + 
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) + 
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) + 
  theme(plot.title = element_text(size = 8,  family = "sans"), axis.text.y = element_text(size = 6, angle = 0, vjust = 0.5, hjust=0.5, family = "sans"), axis.text.x = element_text(size = 6, angle = 0, vjust = 0.5, hjust=0.5,family = "sans"), axis.title = element_text(size = 7))
MIST_score_delay

MIST_score_Support <- ggplot(MIST_MLM[MIST_MLM$Condition==0 & MIST_MLM$Grouping=="Support",], aes(x = Politics, y = MIST_Prob, color = MIST_TF)) +
  geom_jitter( alpha=0.15) +
  geom_smooth(aes(x = Politics, y = MIST_Prob, color = MIST_TF), method = "lm") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.2), legend.text = element_text(size=5, family = "sans"), legend.title = element_blank(), legend.key.size = unit(0.3, "cm")) +
  labs(color = "Truth") +
  scale_fill_manual(values= c("#e34234", "#248721")) +
  scale_color_manual(values= c("#e34234", "#248721")) +
  theme(panel.border = element_blank(), axis.line = element_line()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Conservative political ideology", y = "Probability of reporting a statement as true") +
  labs(title="Political ideology on Support discernment - Disinformation") + 
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) + 
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) + 
  theme(plot.title = element_text(size = 8,  family = "sans"), axis.text.y = element_text(size = 6, angle = 0, vjust = 0.5, hjust=0.5, family = "sans"), axis.text.x = element_text(size = 6, angle = 0, vjust = 0.5, hjust=0.5,family = "sans"), axis.title = element_text(size = 7))
MIST_score_Support

cow_DVs_disinfo <- plot_grid(MIST_score_delay, MIST_score_Support, ncol=2, align = "v", axis="1")
cow_DVs_disinfo
jpeg(file="C:\\Users\\spampatt\\Desktop\\Article_Submissions\\Showdown_data\\Images\\HMR_MIST_Score_Disinformation.jpeg",width= 180,height=105, units = "mm", pointsize= 15, res=300)
cow_DVs_disinfo
dev.off()

#### Figures - MIST score (SIMULATED) -- Motivated Reasoning Simulation ####
wrapped_title <- str_wrap("Probability of reporting a statement as true", width = 30)

set.seed(56)

# Support simulation
MIST_sim_TS <- rnorm_multi(n = 1000, mu = c(5.63, 0.5), sd = c(2.65, 0.3), varnames = c("Politics", "MIST_sim"), r = - 0.5, empirical = T)

MIST_sim_TS$MIST_TF <- "True"
MIST_sim_TS$Grouping <- "Support"

MIST_sim_FS <- rnorm_multi(n = 1000, mu = c(4.00, 0.5), sd = c(2.65, 0.3), varnames = c("Politics", "MIST_sim"), r = - 0.5, empirical = T)

MIST_sim_FS$MIST_TF <- "False"
MIST_sim_FS$Grouping <- "Support"

# Delay simulation
MIST_sim_TD <- rnorm_multi(n = 1000, mu = c(5.63, 0.5), sd = c(2.65, 0.3), varnames = c("Politics", "MIST_sim"), r = 0.5, empirical = T)

MIST_sim_TD$MIST_TF <- "True"
MIST_sim_TD$Grouping <- "Delay"

MIST_sim_FD <- rnorm_multi(n = 1000, mu = c(4.00, 0.5), sd = c(2.65, 0.3), varnames = c("Politics", "MIST_sim"), r = 0.5, empirical = T)

MIST_sim_FD$MIST_TF <- "False"
MIST_sim_FD$Grouping <- "Delay"


MIST_sim <- rbind(MIST_sim_FS, MIST_sim_TS, MIST_sim_FD, MIST_sim_TD)

MIST_sim_support <- ggplot(MIST_sim[MIST_sim$Grouping=="Support",], aes(x = Politics, y = MIST_sim, color = MIST_TF)) +
  geom_smooth(aes(x = Politics, y = MIST_sim, color = MIST_TF), method = "lm", se = F) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(color = "Truth") +
  scale_fill_manual(values= c("#e34234", "#248721")) +
  scale_color_manual(values= c("#e34234", "#248721")) +
  theme(panel.border = element_blank(), axis.line = element_line()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ylab(NULL) + xlab(NULL) +
  labs(title=" Support discernment") + 
  scale_y_continuous(limits = c(-0.1, 1.1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(limits = c(0.9, 10.1), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) + 
  theme(plot.title = element_text(size = 7,  family = "sans"), axis.text.y = element_text(size = 4, angle = 0, vjust = 0.5, hjust=0.5, family = "sans"), axis.text.x = element_text(size = 4, angle = 0, vjust = 0.5, hjust=0.5,family = "sans"), axis.title = element_text(size = 6))
MIST_sim_support


MIST_sim_delay <- ggplot(MIST_sim[MIST_sim$Grouping=="Delay",], aes(x = Politics, y = MIST_sim, color = MIST_TF)) +
  geom_smooth(aes(x = Politics, y = MIST_sim, color = MIST_TF), method = "lm", se = F) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(color = "Truth") +
  scale_fill_manual(values= c("#e34234", "#248721")) +
  scale_color_manual(values= c("#e34234", "#248721")) +
  theme(panel.border = element_blank(), axis.line = element_line()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = wrapped_title) + xlab(NULL) +
  labs(title=" Delay discernment") + 
  scale_y_continuous(limits = c(-0.1, 1.1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(limits = c(0.9, 10.1), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) + 
  theme(plot.title = element_text(size = 7,  family = "sans"), axis.text.y = element_text(size = 4, angle = 0, vjust = 0.5, hjust=0.5, family = "sans"), axis.text.x = element_text(size = 4, angle = 0, vjust = 0.5, hjust=0.5,family = "sans"), axis.title = element_text(size = 6))
MIST_sim_delay


cow_DVs <- plot_grid(MIST_sim_delay, MIST_sim_support, ncol=2, align = "v", axis="1") 

title <- ggdraw() + 
  draw_label(
    "a: Motivated Reasoning",
    size = 8, fontfamily = "sans", fontface = "bold",
    # x = 0,
    hjust = 0.5
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

cow_DVs_MR <- plot_grid(
  title, cow_DVs,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.2, 1)
)
cow_DVs_MR

jpeg(file="C:\\Users\\spampatt\\Desktop\\Article_Submissions\\Showdown_data\\Images\\HMR_MIST_Sim_MR.jpeg",width= 180,height=105, units = "mm", pointsize= 15, res=300)
cow_DVs_MR
dev.off()

#### Figures - MIST score (SIMULATED) -- Expressive Responding Simulation ####

set.seed(56)

# Support simulation
MIST_sim_TS <- rnorm_multi(n = 1000, mu = c(5.63, 0.5), sd = c(2.65, 0.3), varnames = c("Politics", "MIST_sim"), r = - 0.5, empirical = T)

MIST_sim_TS$MIST_TF <- "True"
MIST_sim_TS$Grouping <- "Support"

MIST_sim_FS <- rnorm_multi(n = 1000, mu = c(4.00, 0.5), sd = c(2.65, 0.3), varnames = c("Politics", "MIST_sim"), r = - 0.1, empirical = T)

MIST_sim_FS$MIST_TF <- "False"
MIST_sim_FS$Grouping <- "Support"

# Delay simulation
MIST_sim_TD <- rnorm_multi(n = 1000, mu = c(5.63, 0.5), sd = c(2.65, 0.3), varnames = c("Politics", "MIST_sim"), r = 0.1, empirical = T)

MIST_sim_TD$MIST_TF <- "True"
MIST_sim_TD$Grouping <- "Delay"

MIST_sim_FD <- rnorm_multi(n = 1000, mu = c(4.00, 0.5), sd = c(2.65, 0.3), varnames = c("Politics", "MIST_sim"), r = 0.5, empirical = T)

MIST_sim_FD$MIST_TF <- "False"
MIST_sim_FD$Grouping <- "Delay"


MIST_sim <- rbind(MIST_sim_FS, MIST_sim_TS, MIST_sim_FD, MIST_sim_TD)

MIST_sim_support <- ggplot(MIST_sim[MIST_sim$Grouping=="Support",], aes(x = Politics, y = MIST_sim, color = MIST_TF)) +
  geom_smooth(aes(x = Politics, y = MIST_sim, color = MIST_TF), method = "lm", se = F) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(color = "Truth") +
  scale_fill_manual(values= c("#e34234", "#248721")) +
  scale_color_manual(values= c("#e34234", "#248721")) +
  theme(panel.border = element_blank(), axis.line = element_line()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ylab(NULL) + xlab(NULL) +
  labs(title=" Support of climate action") + 
  scale_y_continuous(limits = c(-0.1, 1.1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(limits = c(0.9, 10.1), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) + 
  theme(plot.title = element_text(size = 7,  family = "sans"), axis.text.y = element_text(size = 4, angle = 0, vjust = 0.5, hjust=0.5, family = "sans"), axis.text.x = element_text(size = 4, angle = 0, vjust = 0.5, hjust=0.5,family = "sans"), axis.title = element_text(size = 6))
MIST_sim_support


MIST_sim_delay <- ggplot(MIST_sim[MIST_sim$Grouping=="Delay",], aes(x = Politics, y = MIST_sim, color = MIST_TF)) +
  geom_smooth(aes(x = Politics, y = MIST_sim, color = MIST_TF), method = "lm", se = F) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(color = "Truth") +
  scale_fill_manual(values= c("#e34234", "#248721")) +
  scale_color_manual(values= c("#e34234", "#248721")) +
  theme(panel.border = element_blank(), axis.line = element_line()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ylab(NULL) + xlab(NULL) +
  labs(title=" Delay of climate action") + 
  scale_y_continuous(limits = c(-0.1, 1.1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(limits = c(0.9, 10.1), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) + 
  theme(plot.title = element_text(size = 7,  family = "sans"), axis.text.y = element_text(size = 4, angle = 0, vjust = 0.5, hjust=0.5, family = "sans"), axis.text.x = element_text(size = 4, angle = 0, vjust = 0.5, hjust=0.5,family = "sans"), axis.title = element_text(size = 6))
MIST_sim_delay


cow_DVs <- plot_grid(MIST_sim_delay, MIST_sim_support, ncol=2, align = "v", axis="1") 

title <- ggdraw() + 
  draw_label(
    "b: Expressive Responding",
    size = 8, fontfamily = "sans", fontface = "bold",
    # x = 0,
    hjust = 0.5
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

cow_DVs_ER <- plot_grid(
  title, cow_DVs,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.2, 1)
)
cow_DVs_ER

jpeg(file="C:\\Users\\spampatt\\Desktop\\Article_Submissions\\Showdown_data\\Images\\HMR_MIST_Sim_ER.jpeg",width= 180,height=105, units = "mm", pointsize= 15, res=300)
cow_DVs_ER
dev.off()

#### Figures - MIST score (SIMULATED) -- Partisanship Bias Simulation ####

set.seed(56)

# Support simulation
MIST_sim_TS <- rnorm_multi(n = 1000, mu = c(5.63, 0.5), sd = c(2.65, 0.3), varnames = c("Politics", "MIST_sim"), r = - 0.1, empirical = T)

MIST_sim_TS$MIST_TF <- "True"
MIST_sim_TS$Grouping <- "Support"

MIST_sim_FS <- rnorm_multi(n = 1000, mu = c(4.00, 0.5), sd = c(2.65, 0.3), varnames = c("Politics", "MIST_sim"), r = - 0.1, empirical = T)

MIST_sim_FS$MIST_TF <- "False"
MIST_sim_FS$Grouping <- "Support"

# Delay simulation
MIST_sim_TD <- rnorm_multi(n = 1000, mu = c(5.63, 0.5), sd = c(2.65, 0.3), varnames = c("Politics", "MIST_sim"), r = 0.1, empirical = T)

MIST_sim_TD$MIST_TF <- "True"
MIST_sim_TD$Grouping <- "Delay"

MIST_sim_FD <- rnorm_multi(n = 1000, mu = c(4.00, 0.5), sd = c(2.65, 0.3), varnames = c("Politics", "MIST_sim"), r = 0.5, empirical = T)

MIST_sim_FD$MIST_TF <- "False"
MIST_sim_FD$Grouping <- "Delay"


MIST_sim <- rbind(MIST_sim_FS, MIST_sim_TS, MIST_sim_FD, MIST_sim_TD)

MIST_sim_support <- ggplot(MIST_sim[MIST_sim$Grouping=="Support",], aes(x = Politics, y = MIST_sim, color = MIST_TF)) +
  geom_smooth(aes(x = Politics, y = MIST_sim, color = MIST_TF), method = "lm", se = F) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(color = "Truth") +
  scale_fill_manual(values= c("#e34234", "#248721")) +
  scale_color_manual(values= c("#e34234", "#248721")) +
  theme(panel.border = element_blank(), axis.line = element_line()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ylab(NULL) + xlab("Conservative political ideology") +
  labs(title=NULL) + 
  scale_y_continuous(limits = c(-0.1, 1.1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(limits = c(0.9, 10.1), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) + 
  theme(plot.title = element_text(size = 7,  family = "sans"), axis.text.y = element_text(size = 4, angle = 0, vjust = 0.5, hjust=0.5, family = "sans"), axis.text.x = element_text(size = 4, angle = 0, vjust = 0.5, hjust=0.5,family = "sans"), axis.title = element_text(size = 6))
MIST_sim_support


MIST_sim_delay <- ggplot(MIST_sim[MIST_sim$Grouping=="Delay",], aes(x = Politics, y = MIST_sim, color = MIST_TF)) +
  geom_smooth(aes(x = Politics, y = MIST_sim, color = MIST_TF), method = "lm", se = F) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(color = "Truth") +
  scale_fill_manual(values= c("#e34234", "#248721")) +
  scale_color_manual(values= c("#e34234", "#248721")) +
  theme(panel.border = element_blank(), axis.line = element_line()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Conservative political ideology", y = wrapped_title) +
  labs(title=NULL) + 
  scale_y_continuous(limits = c(-0.1, 1.1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(limits = c(0.9, 10.1), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) + 
  theme(plot.title = element_text(size = 7,  family = "sans"), axis.text.y = element_text(size = 4, angle = 0, vjust = 0.5, hjust=0.5, family = "sans"), axis.text.x = element_text(size = 4, angle = 0, vjust = 0.5, hjust=0.5,family = "sans"), axis.title = element_text(size = 6))
MIST_sim_delay


cow_DVs <- plot_grid(MIST_sim_delay, MIST_sim_support, ncol=2, align = "v", axis="1") 

title <- ggdraw() + 
  draw_label(
    " c: Partisanship Bias",
    size = 8, fontfamily = "sans", fontface = "bold",
    # x = 0,
    hjust = 0.5
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

cow_DVs_PB <- plot_grid(
  title, cow_DVs,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.2, 1)
)
cow_DVs_PB

jpeg(file="C:\\Users\\spampatt\\Desktop\\Article_Submissions\\Showdown_data\\Images\\HMR_MIST_Sim_PB.jpeg",width= 180,height=105, units = "mm", pointsize= 15, res=300)
cow_DVs_PB
dev.off()

#### Figures - MIST score (SIMULATED) -- Bias of the Right Simulation ####

set.seed(56)

# Support simulation
MIST_sim_TS <- rnorm_multi(n = 1000, mu = c(5.63, 0.5), sd = c(2.65, 0.3), varnames = c("Politics", "MIST_sim"), r = - 0.1, empirical = T)

MIST_sim_TS$MIST_TF <- "True"
MIST_sim_TS$Grouping <- "Support"

MIST_sim_FS <- rnorm_multi(n = 1000, mu = c(4.00, 0.5), sd = c(2.65, 0.3), varnames = c("Politics", "MIST_sim"), r =  0.5, empirical = T)

MIST_sim_FS$MIST_TF <- "False"
MIST_sim_FS$Grouping <- "Support"

# Delay simulation
MIST_sim_TD <- rnorm_multi(n = 1000, mu = c(5.63, 0.5), sd = c(2.65, 0.3), varnames = c("Politics", "MIST_sim"), r = 0.1, empirical = T)

MIST_sim_TD$MIST_TF <- "True"
MIST_sim_TD$Grouping <- "Delay"

MIST_sim_FD <- rnorm_multi(n = 1000, mu = c(4.00, 0.5), sd = c(2.65, 0.3), varnames = c("Politics", "MIST_sim"), r = 0.5, empirical = T)

MIST_sim_FD$MIST_TF <- "False"
MIST_sim_FD$Grouping <- "Delay"


MIST_sim <- rbind(MIST_sim_FS, MIST_sim_TS, MIST_sim_FD, MIST_sim_TD)

MIST_sim_support <- ggplot(MIST_sim[MIST_sim$Grouping=="Support",], aes(x = Politics, y = MIST_sim, color = MIST_TF)) +
  geom_smooth(aes(x = Politics, y = MIST_sim, color = MIST_TF), method = "lm", se = F) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(color = "Truth") +
  scale_fill_manual(values= c("#e34234", "#248721")) +
  scale_color_manual(values= c("#e34234", "#248721")) +
  theme(panel.border = element_blank(), axis.line = element_line()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Conservative political ideology") + ylab(NULL) +
  labs(title=NULL) + 
  scale_y_continuous(limits = c(-0.1, 1.1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(limits = c(0.9, 10.1), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) + 
  theme(plot.title = element_text(size = 7,  family = "sans"), axis.text.y = element_text(size = 4, angle = 0, vjust = 0.5, hjust=0.5, family = "sans"), axis.text.x = element_text(size = 4, angle = 0, vjust = 0.5, hjust=0.5,family = "sans"), axis.title = element_text(size = 6))
MIST_sim_support


MIST_sim_delay <- ggplot(MIST_sim[MIST_sim$Grouping=="Delay",], aes(x = Politics, y = MIST_sim, color = MIST_TF)) +
  geom_smooth(aes(x = Politics, y = MIST_sim, color = MIST_TF), method = "lm", se = F) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(color = "Truth") +
  scale_fill_manual(values= c("#e34234", "#248721")) +
  scale_color_manual(values= c("#e34234", "#248721")) +
  theme(panel.border = element_blank(), axis.line = element_line()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Conservative political ideology") + ylab(NULL) +
  labs(title=NULL) + 
  scale_y_continuous(limits = c(-0.1, 1.1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(limits = c(0.9, 10.1), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) + 
  theme(plot.title = element_text(size = 7,  family = "sans"), axis.text.y = element_text(size = 4, angle = 0, vjust = 0.5, hjust=0.5, family = "sans"), axis.text.x = element_text(size = 4, angle = 0, vjust = 0.5, hjust=0.5,family = "sans"), axis.title = element_text(size = 6))
MIST_sim_delay


cow_DVs_CR <- plot_grid(MIST_sim_delay, MIST_sim_support, ncol=2, align = "v", axis="1") 

title <- ggdraw() + 
  draw_label(
    "d: Bias of the Right",
    size = 8, fontfamily = "sans", fontface = "bold",
    # x = 0,
    hjust = 0.5
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

cow_DVs_CR <- plot_grid(
  title, cow_DVs_CR,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.2, 1)
)
cow_DVs_CR

jpeg(file="C:\\Users\\spampatt\\Desktop\\Article_Submissions\\Showdown_data\\Images\\HMR_MIST_Sim_CR.jpeg",width= 180,height=105, units = "mm", pointsize= 15, res=300)
cow_DVs_CR
dev.off()

#### Figures - MIST score (SIMULATED) -- Lazy Reasoning Bias Simulation ####

set.seed(56)

# Support simulation
MIST_sim_TS <- rnorm_multi(n = 1000, mu = c(5.63, 0.5), sd = c(2.65, 0.3), varnames = c("Politics", "MIST_sim"), r = 0.1, empirical = T)

MIST_sim_TS$MIST_TF <- "True"
MIST_sim_TS$Grouping <- "Support"

MIST_sim_FS <- rnorm_multi(n = 1000, mu = c(4.00, 0.5), sd = c(2.65, 0.3), varnames = c("Politics", "MIST_sim"), r = 0.1, empirical = T)

MIST_sim_FS$MIST_TF <- "False"
MIST_sim_FS$Grouping <- "Support"

# Delay simulation
MIST_sim_TD <- rnorm_multi(n = 1000, mu = c(5.63, 0.5), sd = c(2.65, 0.3), varnames = c("Politics", "MIST_sim"), r = 0.1, empirical = T)

MIST_sim_TD$MIST_TF <- "True"
MIST_sim_TD$Grouping <- "Delay"

MIST_sim_FD <- rnorm_multi(n = 1000, mu = c(4.00, 0.5), sd = c(2.65, 0.3), varnames = c("Politics", "MIST_sim"), r = 0., empirical = T)

MIST_sim_FD$MIST_TF <- "False"
MIST_sim_FD$Grouping <- "Delay"


MIST_sim <- rbind(MIST_sim_FS, MIST_sim_TS, MIST_sim_FD, MIST_sim_TD)

MIST_sim_support <- ggplot(MIST_sim[MIST_sim$Grouping=="Support",], aes(x = Politics, y = MIST_sim, color = MIST_TF)) +
  geom_smooth(aes(x = Politics, y = MIST_sim, color = MIST_TF), method = "lm", se = F) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(color = "Truth") +
  scale_fill_manual(values= c("#e34234", "#248721")) +
  scale_color_manual(values= c("#e34234", "#248721")) +
  theme(panel.border = element_blank(), axis.line = element_line()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Conservative political ideology") + ylab(NULL) +
  labs(title=NULL) + 
  scale_y_continuous(limits = c(-0.1, 1.1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(limits = c(0.9, 10.1), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) + 
  theme(plot.title = element_text(size = 7,  family = "sans"), axis.text.y = element_text(size = 4, angle = 0, vjust = 0.5, hjust=0.5, family = "sans"), axis.text.x = element_text(size = 4, angle = 0, vjust = 0.5, hjust=0.5,family = "sans"), axis.title = element_text(size = 6))
MIST_sim_support

MIST_sim_delay <- ggplot(MIST_sim[MIST_sim$Grouping=="Delay",], aes(x = Politics, y = MIST_sim, color = MIST_TF)) +
  geom_smooth(aes(x = Politics, y = MIST_sim, color = MIST_TF), method = "lm", se = F) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(color = "Truth") +
  scale_fill_manual(values= c("#e34234", "#248721")) +
  scale_color_manual(values= c("#e34234", "#248721")) +
  theme(panel.border = element_blank(), axis.line = element_line()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Conservative political ideology", y = wrapped_title) +
  labs(title=NULL) + 
  scale_y_continuous(limits = c(-0.1, 1.1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(limits = c(0.9, 10.1), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) + 
  theme(plot.title = element_text(size = 7,  family = "sans"), axis.text.y = element_text(size = 4, angle = 0, vjust = 0.5, hjust=0.5, family = "sans"), axis.text.x = element_text(size = 4, angle = 0, vjust = 0.5, hjust=0.5,family = "sans"), axis.title = element_text(size = 6))
MIST_sim_delay


cow_DVs <- plot_grid(MIST_sim_delay, MIST_sim_support, ncol=2, align = "v", axis="1") 

title <- ggdraw() + 
  draw_label(
    " e: Lazy Reasoning",
    size = 8, fontfamily = "sans", fontface = "bold",
    # x = 0,
    hjust = 0.5
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

cow_DVs_PR <- plot_grid(
  title, cow_DVs,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.2, 1)
)
cow_DVs_PR

jpeg(file="C:\\Users\\spampatt\\Desktop\\Article_Submissions\\Showdown_data\\Images\\HMR_MIST_Sim_PR.jpeg", width= 180,height=105, units = "mm", pointsize= 15, res=300)
cow_DVs_PR
dev.off()

#### Figures - MIST score (SIMULATED) -- Grouped Simulation ####

cow_DVs_sim <- plot_grid(cow_DVs_MR, cow_DVs_ER, cow_DVs_PB, cow_DVs_CR, cow_DVs_PR, ncol=2, align = "v", axis="1") 
title <- ggdraw() + 
  draw_label(
    "Simulated data for the predictions made by each hypothesis",
    size = 8, fontfamily = "sans", fontface = "bold",
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

cow_DVs_sim <- plot_grid(
  title, cow_DVs_sim,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

jpeg(file="C:\\Users\\spampatt\\Desktop\\Article_Submissions\\Showdown_data\\Images\\HMR_MIST_Sim_Tot.jpeg",width= 180,height=105, units = "mm", pointsize= 15, res=300)
cow_DVs_sim
dev.off()

#### MLM analysis of Climate Truth Discernment Score - Delay and Support decomposition ####

# full comparison - UNDERPOWERED
TF_MLM<-lmer(MIST ~ (1|Participant) + (1|Country) + 
               Age + Gender + Politics +
               Condition*MIST_TF*Grouping
             , data = MIST_MLM, REML = F)  

TF_MLM<-lmer(MIST ~ (1|Participant) + (1|Country) +
               Age + Gender +
               Politics*MIST_TF*Grouping*Condition
             , data = MIST_MLM, REML = F)


# Within-condition tests

TF_MLM<-lmer(MIST ~ (1|Participant) + (1|Country) +
               Age + Gender + 
               Politics*MIST_TF*Grouping
             , data = MIST_MLM[MIST_MLM$Condition==-1,], REML = F) # Pure control condition only

TF_MLM<-lmer(MIST ~ (1|Participant) + (1|Country) +
               Age + Gender + 
               Politics*MIST_TF*Grouping
             , data = MIST_MLM[MIST_MLM$Condition==0,], REML = F) # Disinformation condition only


#Between-condition comparison
TF_MLM<-lmer(MIST ~ (1|Country) +
               Age + Gender + 
               Politics*Condition
             , data = MIST_MLM[MIST_MLM$MIST_TF=="True" & MIST_MLM$Grouping=="Support",], REML = F)

TF_MLM<-lmer(MIST ~ (1|Country) +
               Age + Gender + 
               Politics*Condition
             , data = MIST_MLM[MIST_MLM$MIST_TF=="False" & MIST_MLM$Grouping=="Delay",], REML = F)

# Summary stats
summary(TF_MLM)
AIC(TF_MLM)
confint(TF_MLM, method="Wald");
joint_tests(TF_MLM,by=c("Condition"))
joint_tests(TF_MLM,by=c("Grouping", "MIST_TF"))
joint_tests(TF_MLM,by=c( "MIST_TF"))
r.squaredGLMM(TF_MLM)
r2beta(TF_MLM , method = "sgv")
MIST_pseudo_d<-fixed_effect/sqrt(random_sds_1^2+random_sds_2^2) #for all random effects' sds

#                   planned contrasts

anova(TF_MLM, type=2)
difflsmeans(TF_MLM)

#Equivalence test for correlation betweenClimate Truth Discernment Score and political ideology, within statement dimensions, control condition
z_cor_test(MIST_MLM[MIST_MLM$MIST_TF=="False" & MIST_MLM$Grouping=="Delay" & MIST_MLM$Condition==-1,]$MIST, MIST_MLM[MIST_MLM$MIST_TF=="False" & MIST_MLM$Grouping=="Delay" & MIST_MLM$Condition==-1,]$Politics, alternative = "equivalence", method = "pearson", null = 0.1)
z_cor_test(MIST_MLM[MIST_MLM$MIST_TF=="True" & MIST_MLM$Grouping=="Delay" & MIST_MLM$Condition==-1,]$MIST, MIST_MLM[MIST_MLM$MIST_TF=="False" & MIST_MLM$Grouping=="Delay" & MIST_MLM$Condition==-1,]$Politics, alternative = "equivalence", method = "pearson", null = 0.1)
z_cor_test(MIST_MLM[MIST_MLM$MIST_TF=="False" & MIST_MLM$Grouping=="Support" & MIST_MLM$Condition==-1,]$MIST, MIST_MLM[MIST_MLM$MIST_TF=="False" & MIST_MLM$Grouping=="Support" & MIST_MLM$Condition==-1,]$Politics, alternative = "equivalence", method = "pearson", null = 0.1)
z_cor_test(MIST_MLM[MIST_MLM$MIST_TF=="True" & MIST_MLM$Grouping=="Support" & MIST_MLM$Condition==-1,]$MIST, MIST_MLM[MIST_MLM$MIST_TF=="True" & MIST_MLM$Grouping=="Support" & MIST_MLM$Condition==-1,]$Politics, alternative = "equivalence", method = "pearson", null = 0.1)

#Equivalence test for correlation betweenClimate Truth Discernment Score and political ideology, within statement dimensions, disinformation condition
z_cor_test(MIST_MLM[MIST_MLM$MIST_TF=="False" & MIST_MLM$Grouping=="Delay" & MIST_MLM$Condition==0,]$MIST, MIST_MLM[MIST_MLM$MIST_TF=="False" & MIST_MLM$Grouping=="Delay" & MIST_MLM$Condition==0,]$Politics, alternative = "two.sided", method = "pearson", null = 0)
z_cor_test(MIST_MLM[MIST_MLM$MIST_TF=="True" & MIST_MLM$Grouping=="Delay" & MIST_MLM$Condition==0,]$MIST, MIST_MLM[MIST_MLM$MIST_TF=="False" & MIST_MLM$Grouping=="Delay" & MIST_MLM$Condition==0,]$Politics, alternative = "equivalence", method = "pearson", null = 0.1)
z_cor_test(MIST_MLM[MIST_MLM$MIST_TF=="False" & MIST_MLM$Grouping=="Support" & MIST_MLM$Condition==0,]$MIST, MIST_MLM[MIST_MLM$MIST_TF=="False" & MIST_MLM$Grouping=="Support" & MIST_MLM$Condition==0,]$Politics, alternative = "equivalence", method = "pearson", null = 0.1)
z_cor_test(MIST_MLM[MIST_MLM$MIST_TF=="True" & MIST_MLM$Grouping=="Support" & MIST_MLM$Condition==0,]$MIST, MIST_MLM[MIST_MLM$MIST_TF=="True" & MIST_MLM$Grouping=="Support" & MIST_MLM$Condition==0,]$Politics, alternative = "two.sided", method = "pearson", null = 0)


# equivalence test for two correlations
r_1 <- cor.test(MIST_MLM[MIST_MLM$MIST_TF=="False" & MIST_MLM$Grouping=="Delay" & MIST_MLM$Condition==-1,]$MIST, MIST_MLM[MIST_MLM$MIST_TF=="False" & MIST_MLM$Grouping=="Delay" & MIST_MLM$Condition==-1,]$Politics)
df_1 <- as.data.frame(r_1$parameter)
r_2 <- cor.test(MIST_MLM[MIST_MLM$MIST_TF=="False" & MIST_MLM$Grouping=="Delay" & MIST_MLM$Condition==0,]$MIST, MIST_MLM[MIST_MLM$MIST_TF=="False" & MIST_MLM$Grouping=="Delay" & MIST_MLM$Condition==0,]$Politics)
df_2 <- as.data.frame(r_2$parameter)

# equivalence test for two correlations
compare_cor(r1 <- r_1[["estimate"]],
            df1 = df_1[1,1],
            r2 <- r_2[["estimate"]],
            df2 = df_2[1,1],
            null = .1,
            method = "f", # Fisher
            alternative = "e") # Equivalence

#### Supplementary Figures ---- MIST SDT -- Political ideology ####

MIST_dprime_pure <- ggplot(MIST_SDT[MIST_SDT$Condition==-1,], aes(x = Politics, y = dprime, color = Grouping)) +
  geom_jitter( alpha=0.15) +
  geom_smooth(aes(x = Politics, y = dprime, color = Grouping), method = "lm") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.2), legend.text = element_text(size=5, family = "sans"), legend.title = element_blank(), legend.key.size = unit(0.3, "cm")) +
  labs(color = "Type") +
  scale_color_manual(values=c("black", "seagreen", "black","seagreen")) +
  theme(panel.border = element_blank(), axis.line = element_line()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Political ideology", y = "Discernment ability (dprime - SDT)") +
  labs(title="a. Political ideology on discernment ability - Control") + 
  scale_y_continuous(breaks = seq(-3.1, 3, 0.2)) + 
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) + 
  theme(plot.title = element_text(size = 8,  family = "sans"), axis.text.y = element_text(size = 6, angle = 0, vjust = 0.5, hjust=0.5, family = "sans"), axis.text.x = element_text(size = 6, angle = 0, vjust = 0.5, hjust=0.5,family = "sans"), axis.title = element_text(size = 7))
MIST_dprime_pure

MIST_dprime_disinfo <- ggplot(MIST_SDT[MIST_SDT$Condition==0,], aes(x = Politics, y = dprime, color = Grouping)) +
  geom_jitter( alpha=0.15) +
  geom_smooth(aes(x = Politics, y = dprime, color = Grouping), method = "lm") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(color = "Type") +
  scale_color_manual(values=c("black", "seagreen", "black","seagreen")) +
  theme(panel.border = element_blank(), axis.line = element_line()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Political ideology", y = "Discernment ability (dprime - SDT)") +
  labs(title="b. Political ideology on discernment ability - Disinformation") + 
  scale_y_continuous(breaks = seq(-3.1, 3, 0.2)) + 
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) + 
  theme(plot.title = element_text(size = 8,  family = "sans"), axis.text.y = element_text(size = 6, angle = 0, vjust = 0.5, hjust=0.5, family = "sans"), axis.text.x = element_text(size = 6, angle = 0, vjust = 0.5, hjust=0.5,family = "sans"), axis.title = element_text(size = 7))
MIST_dprime_disinfo

cow_DVs <- plot_grid(MIST_dprime_pure, MIST_dprime_disinfo, ncol=2, align = "v", axis="1")
cow_DVs
jpeg(file="C:\\Users\\spampatt\\Desktop\\Article_Submissions\\Showdown_data\\Images\\HMR_MIST_dprime.jpeg",width= 180,height=105, units = "mm", pointsize= 15, res=300)
cow_DVs
dev.off()

#### Supplementary --- MLM analysis of MIST through Signal Detection Theory ####

MIST_dprime_RE<-lmer(dprime ~ (1|Participant) + (1|Country) + (1|Politics) +    
                       Age + Gender + CRT + Politics
                     + Condition*Grouping
                     , data = MIST_SDT, REML = TRUE)  
AIC(MIST_dprime_RE)
summary(MIST_dprime_RE)
ranef(MIST_dprime_RE)
confint(MIST_dprime_RE, method="Wald");
plot_model(MIST_dprime_RE, type="re") #to plot random effects


#Ideology MIST_dprime model --- Supplementary Analysis

MIST_dprime_MLM<-lmer(dprime ~ (1|Participant) + (1|Country) + (1|Politics) + 
                        Age + Gender +
                        Politics*Grouping
                      , data = MIST_SDT[MIST_SDT$Condition==-1, ], REML = FALSE) # Pure control condition  

MIST_dprime_MLM<-lmer(dprime ~ (1|Participant) + (1|Country) + (1|Politics) + 
                        Age + Gender +
                        Politics*Grouping
                      , data = MIST_SDT[MIST_SDT$Condition==0, ], REML = FALSE) # Disinformation condition  

#                   model summary

summary(MIST_dprime_MLM)
AIC(MIST_dprime_MLM)
confint(MIST_dprime_MLM, method="Wald");
joint_tests(MIST_dprime_MLM,by=c("Grouping"))

#Equivalence test for correlation between dprime and political ideology
z_cor_test(MIST_SDT[MIST_SDT$Grouping=="Delay" & MIST_SDT$Condition==-1,]$dprime, MIST_SDT[MIST_SDT$Grouping=="Delay" & MIST_SDT$Condition==-1,]$Politics, alternative = "two.sided", method = "pearson", null = 0.1)
z_cor_test(MIST_SDT[MIST_SDT$Grouping=="Support" & MIST_SDT$Condition==-1,]$dprime, MIST_SDT[MIST_SDT$Grouping=="Support" & MIST_SDT$Condition==-1,]$Politics, alternative = "equivalence", method = "pearson", null = 0.1)
z_cor_test(MIST_SDT[MIST_SDT$Grouping=="Delay" & MIST_SDT$Condition==0,]$dprime, MIST_SDT[MIST_SDT$Grouping=="Delay" & MIST_SDT$Condition==0,]$Politics, alternative = "two.sided", method = "pearson", null = 0.1)
z_cor_test(MIST_SDT[MIST_SDT$Grouping=="Support" & MIST_SDT$Condition==0,]$dprime, MIST_SDT[MIST_SDT$Grouping=="Support" & MIST_SDT$Condition==0,]$Politics, alternative = "two.sided", method = "pearson", null = 0.1)

# equivalence test for two correlation
r_1 <- cor.test(MIST_SDT[MIST_SDT$Grouping=="Delay" & MIST_SDT$Condition==0,]$dprime, MIST_SDT[MIST_SDT$Grouping=="Delay" & MIST_SDT$Condition==0,]$Politics)
df_1 <- as.data.frame(r_1$parameter)
r_2 <- cor.test(MIST_SDT[MIST_SDT$Grouping=="Support" & MIST_SDT$Condition==0,]$dprime, MIST_SDT[MIST_SDT$Grouping=="Support" & MIST_SDT$Condition==0,]$Politics)
df_2 <- as.data.frame(r_2$parameter)

# equivalence test for two correlations
compare_cor(r1 <- r_1[["estimate"]],
            df1 = df_1[1,1],
            r2 <- r_2[["estimate"]],
            df2 = df_2[1,1],
            null = .1,
            method = "f", # Fisher
            alternative = "e") # Equivalence

#### Supplementary E ---- MLM analysis of Climate Truth Discernment Score w/o statement 18 (with preprocessing) ####

# Preprocessing
S18<-read.csv("C:\\...\\Showdown.csv", header=TRUE)
S18 <- S18[S18$Condition==-1 | S18$Condition == 0,]
S18 <- S18 %>% select(-F_Delay_3_R, -F_Delay_3) # Removing Statement 18

n <- as.integer(length(S18$F_Delay_2_R))

for (q in 1:n) {
  S18$MIST_score[q] <- sum(S18$T_Support_1[q], S18$T_Support_2[q], S18$T_Support_3[q], S18$T_Support_4[q], S18$T_Support_5[q],
                                S18$T_Delay_1[q], S18$T_Delay_2[q], S18$T_Delay_3[q], S18$T_Delay_4[q], S18$T_Delay_5[q],
                                S18$F_Support_1_R[q], S18$F_Support_2_R[q], S18$F_Support_3_R[q], S18$F_Support_4_R[q], S18$F_Support_5_R[q],
                                S18$F_Delay_1_R[q], S18$F_Delay_2_R[q], S18$F_Delay_4_R[q], S18$F_Delay_5_R[q])
  S18$T_Support[q] <- sum(S18$T_Support_1[q], S18$T_Support_2[q], S18$T_Support_3[q], S18$T_Support_4[q], S18$T_Support_5[q])
  S18$T_Delay[q] <- sum(S18$T_Delay_1[q], S18$T_Delay_2[q], S18$T_Delay_3[q], S18$T_Delay_4[q], S18$T_Delay_5[q])
  S18$F_Support[q] <- sum(S18$F_Support_1[q], S18$F_Support_2[q], S18$F_Support_3[q], S18$F_Support_4[q], S18$F_Support_5[q])
  S18$F_Delay[q] <- sum(S18$F_Delay_1[q], S18$F_Delay_2[q], S18$F_Delay_4[q], S18$F_Delay_5[q])
  S18$F_Support_R[q] <- sum(S18$F_Support_1_R[q], S18$F_Support_2_R[q], S18$F_Support_3_R[q], S18$F_Support_4_R[q], S18$F_Support_5_R[q])
  S18$F_Delay_R[q] <- sum(S18$F_Delay_1_R[q], S18$F_Delay_2_R[q], S18$F_Delay_4_R[q], S18$F_Delay_5_R[q])
}

S18$MIST_T_score <- S18$T_Support + S18$T_Delay
S18$MIST_F_score <- S18$F_Support + S18$F_Delay


n <- as.integer(length(S18$F_Delay_2_R))

for (q in 1:n) {
  #exploratory MIST - Signal Detection Theory Signal Detection Theory (Batailler et al, 2022 https://doi.org/10.1177/1745691620986135)
  S18$MIST_hitrate[q] <- (S18$T_Support[q] + S18$T_Delay[q] + 1) / 12
  S18$MIST_falsealarm[q] <- (sum(S18$F_Support_1[q], S18$F_Support_2[q], S18$F_Support_3[q], S18$F_Support_4[q], S18$F_Support_5[q]) + sum(S18$F_Delay_1[q], S18$F_Delay_2[q], S18$F_Delay_4[q], S18$F_Delay_5[q]) + 1 ) / 11
  S18$MIST_dprime[q] <- qnorm(S18$MIST_hitrate[q]) - qnorm(S18$MIST_falsealarm[q]);
  
  S18$MIST_D_hitrate[q] <- (S18$T_Delay[q] + 0.5) / 6
  S18$MIST_D_falsealarm[q] <- sum(S18$F_Delay_1[q], S18$F_Delay_2[q], S18$F_Delay_4[q], S18$F_Delay_5[q] + 0.5) / 5
  S18$MIST_D_dprime[q] <- qnorm(S18$MIST_D_hitrate[q]) - qnorm(S18$MIST_D_falsealarm[q]);
  
  S18$MIST_S_hitrate[q] <- (S18$T_Support[q] + 0.5) / 6
  S18$MIST_S_falsealarm[q] <- sum(S18$F_Support_1[q], S18$F_Support_2[q], S18$F_Support_3[q], S18$F_Support_4[q], S18$F_Support_5[q] + 0.5) / 6
  S18$MIST_S_dprime[q] <- qnorm(S18$MIST_S_hitrate[q]) - qnorm(S18$MIST_S_falsealarm[q]);
}

#Climate Truth Discernment Score breakdown across combinations of factors in the Climate Truth Discernment Task
n <- length(S18$ui)
p <- n-1;
w <- n*4; #n of observations

#Creating S18 matrices for column-row transition
S18_MLM <- matrix(1, w);
S18_MLM <- data.frame(S18_MLM);
names(S18_MLM)[names(S18_MLM) == "S18_MLM"]  <-  "ui"

for (i in 0:p) { 
  m <- i+1;
  o <- i*4;
  S18_MLM$ui[(1+o):(4+o)] <- S18$ui[m];
  S18_MLM$Participant[(1+o):(4+o)] <- m;
  S18_MLM$Oneline[1+o] <- 1;
  S18_MLM$Oneline[(2+o):(4+o)] <- 0;
  S18_MLM$Politics[(1+o):(4+o)] <- S18$Pol_ideo[m];
  S18_MLM$Gender[(1+o):(4+o)] <- S18$Gender[m];
  S18_MLM$Age[(1+o):(4+o)] <- S18$Age[m];
  S18_MLM$Country[(1+o):(4+o)] <- S18$Country[m];
  
  #Condition assignment
  S18_MLM$Condition[(1+o):(4+o)] <- S18$Condition[m];

  S18_MLM$MIST[1+o] <- S18$T_Support[m];
  S18_MLM$MIST[2+o] <- S18$T_Delay[m];
  S18_MLM$MIST[3+o] <- S18$F_Support[m];
  S18_MLM$MIST[4+o] <- S18$F_Delay[m];
  
  S18_MLM$MIST_R[1+o] <- S18$T_Support[m];
  S18_MLM$MIST_R[2+o] <- S18$T_Delay[m];
  S18_MLM$MIST_R[3+o] <- S18$F_Support_R[m];
  S18_MLM$MIST_R[4+o] <- S18$F_Delay_R[m];
  
  S18_MLM$S18_SDT[1+o] <- S18$MIST_S_hitrate[m];
  S18_MLM$S18_SDT[2+o] <- S18$MIST_D_hitrate[m];
  S18_MLM$S18_SDT[3+o] <- S18$MIST_S_falsealarm[m];
  S18_MLM$S18_SDT[4+o] <- S18$MIST_D_falsealarm[m];
  
  S18_MLM$MIST_TF[(1+o):(2+o)] <- "True";
  S18_MLM$MIST_TF[(3+o):(4+o)] <- "False";
  
  S18_MLM$MIST_HRFA[(1+o):(2+o)] <- "Hit rate";
  S18_MLM$MIST_HRFA[(3+o):(4+o)] <- "False alarm";
  
  S18_MLM$MIST_Type[1+o] <- "T - Support";
  S18_MLM$MIST_Type[3+o] <- "T - Delay";
  S18_MLM$MIST_Type[2+o] <- "F - Support";
  S18_MLM$MIST_Type[4+o] <- "F - Delay";
  
  S18_MLM$Grouping[1+o] <- "Support";
  S18_MLM$Grouping[2+o] <- "Delay";
  S18_MLM$Grouping[3+o] <- "Support";
  S18_MLM$Grouping[4+o] <- "Delay";
}

S18_MLM$MIST_Prob <- S18_MLM$MIST/5
S18_MLM$Gender <- as.factor(S18_MLM$Gender)
S18_MLM$Condition <- as.factor(S18_MLM$Condition)
S18_MLM$MIST_TF <- as.factor(S18_MLM$MIST_TF)
S18_MLM$MIST_Type <- as.factor(S18_MLM$MIST_Type)
S18_MLM$Grouping <- as.factor(S18_MLM$Grouping)
S18_MLM$MIST_HRFA <- as.factor(S18_MLM$MIST_HRFA)

#Creating S18 matrices for column-row transition - Signal Detection Theory (Batailler et al, 2022 https://doi.org/10.1177/1745691620986135)


S18_SDT <- matrix(1, w);
S18_SDT <- data.frame(S18_SDT);
names(S18_SDT)[names(S18_SDT) == "S18_SDT"]  <-  "ui"

for (i in 0:p) { 
  m <- i+1;
  o <- i*2;
  S18_SDT$ui[(1+o):(2+o)] <- S18$ui[m];
  S18_SDT$Participant[(1+o):(2+o)] <- m;
  S18_SDT$Oneline[1+o] <- 1;
  S18_SDT$Oneline[(2+o):(2+o)] <- 0;
  S18_SDT$Politics[(1+o):(2+o)] <- S18$Pol_ideo[m];
  S18_SDT$Age[(1+o):(2+o)] <- S18$Age[m];
  S18_SDT$Gender[(1+o):(2+o)] <- S18$Gender[m];
  S18_SDT$CRT[(1+o):(2+o)] <- S18$CRT[m];
  S18_SDT$Country[(1+o):(2+o)] <- S18$Country[m];
  
  #Condition assignment
  S18_SDT$Condition[(1+o):(2+o)] <- S18$Condition[m];

  S18_SDT$dprime[1+o] <- S18$MIST_S_dprime[m];
  S18_SDT$dprime[2+o] <- S18$MIST_D_dprime[m];
  
  S18_SDT$falsealarm[1+o] <- S18$MIST_S_falsealarm[m];
  S18_SDT$falsealarm[2+o] <- S18$MIST_D_falsealarm[m];
  
  S18_SDT$hitrate[1+o] <- S18$MIST_S_hitrate[m];
  S18_SDT$hitrate[2+o] <- S18$MIST_D_hitrate[m];
  
  
  S18_SDT$Grouping[1+o] <- "Support";
  S18_SDT$Grouping[2+o] <- "Delay";
}

S18_SDT$Condition <- as.factor(S18_SDT$Condition)
S18_SDT$Grouping <- as.factor(S18_SDT$Grouping)
S18_SDT$Gender <- as.factor(S18_SDT$Gender)
S18_SDT$Country <- as.factor(S18_SDT$Country)

# Within-condition tests

TF_MLM<-lmer(MIST ~ (1|Participant) + (1|Country) +
               Age + Gender + 
               Politics*MIST_TF*Grouping
             , data = S18_MLM[S18_MLM$Condition==-1,], REML = F) # Pure control condition only

TF_MLM<-lmer(MIST ~ (1|Participant) + (1|Country) +
               Age + Gender + 
               Politics*MIST_TF*Grouping
             , data = S18_MLM[S18_MLM$Condition==0,], REML = F) # Disinformation condition only

# Summary stats
summary(TF_MLM)
AIC(TF_MLM)
confint(TF_MLM, method="Wald");
joint_tests(TF_MLM,by=c("Condition"))
joint_tests(TF_MLM,by=c("Grouping", "MIST_TF"))
joint_tests(TF_MLM,by=c( "MIST_TF"))
r.squaredGLMM(TF_MLM)
r2beta(TF_MLM , method = "sgv")
MIST_pseudo_d<-fixed_effect/sqrt(random_sds_1^2+random_sds_2^2) #for all random effects' sds

#                   planned contrasts

anova(TF_MLM, type=2)
difflsmeans(TF_MLM)

#Equivalence test for correlation between Climate Truth Discernment Score and political ideology, within statement dimensions, control condition
z_cor_test(S18_MLM[S18_MLM$MIST_TF=="False" & S18_MLM$Grouping=="Delay" & S18_MLM$Condition==-1,]$MIST, S18_MLM[S18_MLM$MIST_TF=="False" & S18_MLM$Grouping=="Delay" & S18_MLM$Condition==-1,]$Politics, alternative = "equivalence", method = "pearson", null = 0.1)
z_cor_test(S18_MLM[S18_MLM$MIST_TF=="True" & S18_MLM$Grouping=="Delay" & S18_MLM$Condition==-1,]$MIST, S18_MLM[S18_MLM$MIST_TF=="False" & S18_MLM$Grouping=="Delay" & S18_MLM$Condition==-1,]$Politics, alternative = "equivalence", method = "pearson", null = 0.1)
z_cor_test(S18_MLM[S18_MLM$MIST_TF=="False" & S18_MLM$Grouping=="Support" & S18_MLM$Condition==-1,]$MIST, S18_MLM[S18_MLM$MIST_TF=="False" & S18_MLM$Grouping=="Support" & S18_MLM$Condition==-1,]$Politics, alternative = "equivalence", method = "pearson", null = 0.1)
z_cor_test(S18_MLM[S18_MLM$MIST_TF=="True" & S18_MLM$Grouping=="Support" & S18_MLM$Condition==-1,]$MIST, S18_MLM[S18_MLM$MIST_TF=="True" & S18_MLM$Grouping=="Support" & S18_MLM$Condition==-1,]$Politics, alternative = "equivalence", method = "pearson", null = 0.1)

#Equivalence test for correlation between Climate Truth Discernment Score and political ideology, within statement dimensions, disinformation condition
z_cor_test(S18_MLM[S18_MLM$MIST_TF=="False" & S18_MLM$Grouping=="Delay" & S18_MLM$Condition==0,]$MIST, S18_MLM[S18_MLM$MIST_TF=="False" & S18_MLM$Grouping=="Delay" & S18_MLM$Condition==0,]$Politics, alternative = "two.sided", method = "pearson", null = 0.0)
z_cor_test(S18_MLM[S18_MLM$MIST_TF=="True" & S18_MLM$Grouping=="Delay" & S18_MLM$Condition==0,]$MIST, S18_MLM[S18_MLM$MIST_TF=="False" & S18_MLM$Grouping=="Delay" & S18_MLM$Condition==0,]$Politics, alternative = "equivalence", method = "pearson", null = 0.1)
z_cor_test(S18_MLM[S18_MLM$MIST_TF=="False" & S18_MLM$Grouping=="Support" & S18_MLM$Condition==0,]$MIST, S18_MLM[S18_MLM$MIST_TF=="False" & S18_MLM$Grouping=="Support" & S18_MLM$Condition==0,]$Politics, alternative = "equivalence", method = "pearson", null = 0.1)
z_cor_test(S18_MLM[S18_MLM$MIST_TF=="True" & S18_MLM$Grouping=="Support" & S18_MLM$Condition==0,]$MIST, S18_MLM[S18_MLM$MIST_TF=="True" & S18_MLM$Grouping=="Support" & S18_MLM$Condition==0,]$Politics, alternative = "two.sided", method = "pearson", null = 0)


# equivalence test for two correlations
r_1 <- cor.test(S18_MLM[S18_MLM$MIST_TF=="False" & S18_MLM$Grouping=="Delay" & S18_MLM$Condition==-1,]$MIST, S18_MLM[S18_MLM$MIST_TF=="False" & S18_MLM$Grouping=="Delay" & S18_MLM$Condition==-1,]$Politics)
df_1 <- as.data.frame(r_1$parameter)
r_2 <- cor.test(S18_MLM[S18_MLM$MIST_TF=="False" & S18_MLM$Grouping=="Delay" & S18_MLM$Condition==0,]$MIST, S18_MLM[S18_MLM$MIST_TF=="False" & S18_MLM$Grouping=="Delay" & S18_MLM$Condition==0,]$Politics)
df_2 <- as.data.frame(r_2$parameter)

# equivalence test for two correlations
compare_cor(r1 <- r_1[["estimate"]],
            df1 = df_1[1,1],
            r2 <- r_2[["estimate"]],
            df2 = df_2[1,1],
            null = .1,
            method = "f", # Fisher
            alternative = "e") # Equivalence


#### Supplementary E --- MLM analysis of MIST through Signal Detection Theory w/o statement 18 ####

MIST_dprime_RE<-lmer(dprime ~ (1|Participant) + (1|Country) + (1|Politics) +    
                       Age + Gender + CRT + Politics
                     + Condition*Grouping
                     , data = S18_SDT, REML = TRUE)  
AIC(MIST_dprime_RE)
summary(MIST_dprime_RE)
ranef(MIST_dprime_RE)
confint(MIST_dprime_RE, method="Wald");
plot_model(MIST_dprime_RE, type="re") #to plot random effects


#Ideology MIST_dprime model --- Supplementary Analysis

MIST_dprime_MLM<-lmer(dprime ~ (1|Participant) + (1|Country) + (1|Politics) + 
                        Age + Gender +
                        Politics*Grouping
                      , data = S18_SDT[S18_SDT$Condition==-1, ], REML = FALSE) # Pure control condition  

MIST_dprime_MLM<-lmer(dprime ~ (1|Participant) + (1|Country) + (1|Politics) + 
                        Age + Gender +
                        Politics*Grouping
                      , data = S18_SDT[S18_SDT$Condition==0, ], REML = FALSE) # Disinformation condition  

#                   model summary

summary(MIST_dprime_MLM)
AIC(MIST_dprime_MLM)
confint(MIST_dprime_MLM, method="Wald");
joint_tests(MIST_dprime_MLM,by=c("Grouping"))

#                   planned contrasts

anova(MIST_dprime_MLM, type=2)
difflsmeans(MIST_dprime_MLM)

#Equivalence test for correlation between dprime and political ideology
z_cor_test(S18_SDT[S18_SDT$Grouping=="Delay" & S18_SDT$Condition==-1,]$dprime, S18_SDT[S18_SDT$Grouping=="Delay" & S18_SDT$Condition==-1,]$Politics, alternative = "two.sided", method = "pearson", null = 0)
z_cor_test(S18_SDT[S18_SDT$Grouping=="Support" & S18_SDT$Condition==-1,]$dprime, S18_SDT[S18_SDT$Grouping=="Support" & S18_SDT$Condition==-1,]$Politics, alternative = "equivalence", method = "pearson", null = 0.1)
z_cor_test(S18_SDT[S18_SDT$Grouping=="Delay" & S18_SDT$Condition==0,]$dprime, S18_SDT[S18_SDT$Grouping=="Delay" & S18_SDT$Condition==0,]$Politics, alternative = "two.sided", method = "pearson", null = 0.1)
z_cor_test(S18_SDT[S18_SDT$Grouping=="Support" & S18_SDT$Condition==0,]$dprime, S18_SDT[S18_SDT$Grouping=="Support" & S18_SDT$Condition==0,]$Politics, alternative = "two.sided", method = "pearson", null = 0.1)

# equivalence test for two correlation
r_1 <- cor.test(S18_SDT[S18_SDT$Grouping=="Delay" & S18_SDT$Condition==0,]$dprime, S18_SDT[S18_SDT$Grouping=="Delay" & S18_SDT$Condition==0,]$Politics)
df_1 <- as.data.frame(r_1$parameter)
r_2 <- cor.test(S18_SDT[S18_SDT$Grouping=="Support" & S18_SDT$Condition==0,]$dprime, S18_SDT[S18_SDT$Grouping=="Support" & S18_SDT$Condition==0,]$Politics)
df_2 <- as.data.frame(r_2$parameter)

# equivalence test for two correlations
compare_cor(r1 <- r_1[["estimate"]],
            df1 = df_1[1,1],
            r2 <- r_2[["estimate"]],
            df2 = df_2[1,1],
            null = .1,
            method = "f", # Fisher
            alternative = "e") # Equivalence

