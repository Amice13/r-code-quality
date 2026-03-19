#########################################################################
##  Replication Code HPRV "Do the Bretton Woods Institutions Promote Economic Transparency?"
##  Code to make descriptive tables and figures in the paper
##  Contact: Xun Pang, xpang@pku.edu.cn
################################################################

sink("~/Desktop/Replication_ Materials/log/log_description.txt")

rm(list=ls())
library(lme4)
library(arm)
library(ggplot2)
library(texreg)
library(tidyverse)
library(stargazer)  # for making latex table of descriptive statistics
library(reshape2)
library(ggstance)
setwd("~/Desktop/Replication_ Materials/")
source('plot_functions.R', chdir = TRUE)
Trans <- read.csv("NewDataImp.csv")

################################################################
### 1. Table 1: "Possible Selection Problem: Association of lagged transparency and BWIs Loans
################################################################
## simple association: Just collect the coefficients and standard errors
SP <- matrix(NA, nrow=12, ncol=4)
llm <- summary(lm(imf_total_commit_imf_ln~transparency_hrv_lag, data=Trans))
SP[1,1] <- coef(llm)[[2]]
SP[2,1] <- coef(llm)[[4]]
llm <- summary(lm(imf_total_disburse_imf_ln~transparency_hrv_lag, data=Trans))
SP[1,2] <- coef(llm)[[2]]
SP[2,2] <- coef(llm)[[4]]
llm <- summary(lm(wb_total_commit_wb_ln~transparency_hrv_lag, data=Trans))
SP[1,3] <- coef(llm)[[2]]
SP[2,3] <- coef(llm)[[4]]
llm <- summary(lm(wb_total_disburse_wb_ln~transparency_hrv_lag, data=Trans))
SP[1,4] <- coef(llm)[[2]]
SP[2,4] <- coef(llm)[[4]]
llm <- summary(glm(imf_total_commit_imf_dummy~transparency_hrv_lag, family = binomial(link = "probit"), data=Trans))
SP[3,1] <- coef(llm)[[2]]
SP[4,1] <- coef(llm)[[4]]
llm <- summary(glm(imf_total_disburse_imf_dummy~transparency_hrv_lag, family = binomial(link = "probit"), data=Trans))
SP[3,2] <- coef(llm)[[2]]
SP[4,2] <- coef(llm)[[4]]
llm <- summary(glm(wb_total_commit_wb_dummy~transparency_hrv_lag, family = binomial(link = "probit"), data=Trans))
SP[3,3] <- coef(llm)[[2]]
SP[4,3] <- coef(llm)[[4]]
llm <- summary(glm(wb_total_disburse_wb_dummy~transparency_hrv_lag, family = binomial(link = "probit"), data=Trans))
SP[3,4] <- coef(llm)[[2]]
SP[4,4] <- coef(llm)[[4]]

## Democracies
llm <- summary(lm(imf_total_commit_imf_ln~transparency_hrv_lag, data=subset(Trans, democracy_dd_bd_lag==1 )))
SP[5,1] <- coef(llm)[[2]]
SP[6,1] <- coef(llm)[[4]]
llm <- summary(lm(imf_total_disburse_imf_ln~transparency_hrv_lag, data=subset(Trans, democracy_dd_bd_lag==1 )))
SP[5,2] <- coef(llm)[[2]]
SP[6,2] <- coef(llm)[[4]]
llm <- summary(lm(wb_total_commit_wb_ln~transparency_hrv_lag, data=subset(Trans, democracy_dd_bd_lag==1 )))
SP[5,3] <- coef(llm)[[2]]
SP[6,3] <- coef(llm)[[4]]
llm <- summary(lm(wb_total_disburse_wb_ln~transparency_hrv_lag,data=subset(Trans, democracy_dd_bd_lag==1 )))
SP[5,4] <- coef(llm)[[2]]
SP[6,4] <- coef(llm)[[4]]
llm <- summary(glm(imf_total_commit_imf_dummy_lag~transparency_hrv_lag, family = binomial(link = "probit"), data=subset(Trans, democracy_dd_bd_lag==1 )))
SP[7,1] <- coef(llm)[[2]]
SP[8,1] <- coef(llm)[[4]]
llm <- summary(glm(imf_total_disburse_imf_dummy~transparency_hrv_lag, family = binomial(link = "probit"), data=subset(Trans, democracy_dd_bd_lag==1 )))
SP[7,2] <- coef(llm)[[2]]
SP[8,2] <- coef(llm)[[4]]
llm <- summary(glm(wb_total_commit_wb_dummy~transparency_hrv_lag, family = binomial(link = "probit"), data=subset(Trans, democracy_dd_bd_lag==1 )))
SP[7,3] <- coef(llm)[[2]]
SP[8,3] <- coef(llm)[[4]]
llm <- summary(glm(wb_total_disburse_wb_dummy~transparency_hrv_lag, family = binomial(link = "probit"), data=subset(Trans, democracy_dd_bd_lag==1 )))
SP[7,4] <- coef(llm)[[2]]
SP[8,4] <- coef(llm)[[4]]
## Autocracies
llm <- summary(lm(imf_total_commit_imf_ln~transparency_hrv_lag, data=subset(Trans, democracy_dd_bd_lag==0 )))
SP[9,1] <- coef(llm)[[2]]
SP[10,1] <- coef(llm)[[4]]
llm <- summary(lm(imf_total_disburse_imf_ln~transparency_hrv_lag, data=subset(Trans, democracy_dd_bd_lag==0 )))
SP[9,2] <- coef(llm)[[2]]
SP[10,2] <- coef(llm)[[4]]
llm <- summary(lm(wb_total_commit_wb_ln~transparency_hrv_lag, data=subset(Trans, democracy_dd_bd_lag==0 )))
SP[9,3] <- coef(llm)[[2]]
SP[10,3] <- coef(llm)[[4]]
llm <- summary(lm(wb_total_disburse_wb_ln~transparency_hrv_lag,data=subset(Trans, democracy_dd_bd_lag==0 )))
SP[9,4] <- coef(llm)[[2]]
SP[10,4] <- coef(llm)[[4]]
llm <-summary(glm(imf_total_commit_imf_dummy~transparency_hrv_lag, family = binomial(link = "probit"), data=subset(Trans, democracy_dd_bd_lag==0 )))
SP[11,1] <- coef(llm)[[2]]
SP[12,1] <- coef(llm)[[4]]
llm <-summary(glm(imf_total_disburse_imf_dummy~transparency_hrv_lag, family = binomial(link = "probit"), data=subset(Trans, democracy_dd_bd_lag==0 )))
SP[11,2] <- coef(llm)[[2]]
SP[12,2] <- coef(llm)[[4]]
llm <-summary(glm(wb_total_commit_wb_dummy~transparency_hrv_lag, family = binomial(link = "probit"), data=subset(Trans, democracy_dd_bd_lag==0 )))
SP[11,3] <- coef(llm)[[2]]
SP[12,3] <- coef(llm)[[4]]
llm <-summary(glm(wb_total_disburse_wb_dummy~transparency_hrv_lag, family = binomial(link = "probit"), data=subset(Trans, democracy_dd_bd_lag==0)))
SP[11,4] <- coef(llm)[[2]]
SP[12,4] <- coef(llm)[[4]]
colnames(SP) <-  c("IMF Commitment", "IMF Disbursement", "WB Commitment", "WB Disbursement")

model <- c("pooled", "-", "-", "-","democracy","-", "-", "-", "autocracy","-", "-", "-")

loan <- rep(c("size", "-", "dummy", "-"), 3)
res <- rep(c("coef.", "se"), 6)

data.frame(model, loan, res, round(SP,3))

################################################################
## 3. Figure 1: distribution of transparency

## Figure 1: (a) Transparency
## how different transparancy is for different regime
transparency <- Trans$transparency_hrv

p <- ggplot(Trans, aes(x=transparency_hrv))+
    geom_histogram(alpha = 0.5, position = 'identity',  bins=50, fill="darkblue")+
    ylim(0, 650)+
    theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),  
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18)
          ) +
    labs(x = "Transparency", y="Count")

   ggsave(paste("disGraphs/transDistr.pdf", sep=""), p, 
          width = 8, height = 6, device="pdf")

## Figure 1: (b) Regime-specific Transparency
## how different transparancy is for different regime
Ntransd <- data.frame(subset(Trans, democracy_dd_bd==1))
Ntransa <- data.frame(subset(Trans, democracy_dd_bd==0))
transparency <- Ntransd$transparency_hrv
carrots <- data.frame(transparency)
transparency <- Ntransa$transparency_hrv
 cucumbers <- data.frame(transparency)
carrots$regime <- 'democracy'
cucumbers$regime <- 'non-democracy'
Lengths <- rbind(carrots, cucumbers)
 p <-  ggplot(Lengths, aes(transparency, fill = regime)) + 
      geom_histogram(alpha = 0.5, position = 'identity', bins=50)+
       ylim(0, 650)+
    theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),  
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.text=element_text(size=18),
        legend.title = element_blank()) +
     labs(x = "Transparency", y="Count")

   ggsave(paste("disGraphs/transDistrRegime.pdf", sep=""), p, 
          width = 9, height = 6, device="pdf")


################################################################
## Figure 2: "Distributions of Years that Countries Receive Loans"
## 2. Plot how many years of a country receiving loans
expl3 <- c( "imf_total_commit_imf_dummy_lag" ,"imf_total_disburse_imf_dummy_lag", "wb_total_commit_wb_dummy_lag", "wb_total_disburse_wb_dummy_lag")
expl4 <-c("imfc", "imfd", "wbc", "wbd")
for(i in 1:4){
 Years <- tapply(Trans[,expl3[i]],Trans$ccode,  sum)
 df <- data.frame(Years)
p <- ggplot(df, aes(x=Years)) + 
  geom_histogram(color="darkblue", fill="lightblue") 
   ggsave(paste("disGraphs/yrs",expl4[i],".pdf", sep=""), p, 
          width = 8, height = 6, device="pdf")
}


################################################################
##3. Figure 3: Loans and Regime Type
treat <- c("imf_total_commit_imf_lag_ln" , "imf_total_disburse_imf_lag_ln", "wb_total_commit_wb_lag_ln", "wb_total_disburse_wb_lag_ln")
treat2 <- c("imfC" , "imfD", "wbC", "wbD")
unit <- c("SDR", "USD")
a <- 25
b <- 20
## How different the amount of aid is given to different regimes
for(i in 1:4){
    if (i <3){
        u <- unit[1]
    }else{
        u <- unit[2]
        }
   aid <- Ntransd[,treat[i]][Ntransd[,treat[i]]>0]
   carrots <- data.frame(aid)
   aid <-  Ntransa[,treat[i]][Ntransa[,treat[i]]>0]
   cucumbers <-data.frame(aid)
   carrots$regime <- 'democracy'
   cucumbers$regime <- 'non-democracy'
    Lengths <- rbind(carrots, cucumbers)
    if (i ==1 |i==3){
p <-   ggplot(Lengths, aes(aid, fill = regime)) + 
    geom_histogram(alpha = 0.5, position = 'identity', bins=50) +
    scale_fill_manual(values=c("#69b3a2", "#404080")) +
         theme(axis.text.x = element_text(size = a),
        axis.text.y = element_text(size = a),  
        axis.title.x = element_text(size = b),
        axis.title.y = element_text(size = b),
        legend.text=element_text(size=b),
        legend.title = element_blank(), legend.position = "none")+
    labs(x = paste("Loan ( ", u, ", log)", sep=""), y="Count")
   ggsave(paste("disGraphs/",treat2[i], ".pdf", sep=""), p, 
          width = 8, height = 6, device="pdf")
    }else{
    p <-   ggplot(Lengths, aes(aid, fill = regime)) + 
    geom_histogram(alpha = 0.5, position = 'identity', bins=50) +
    scale_fill_manual(values=c("#69b3a2", "#404080")) +
         theme(axis.text.x = element_text(size = a),
        axis.text.y = element_text(size = a),  
        axis.title.x = element_text(size = b),
        axis.title.y = element_text(size = b),
        legend.text=element_text(size=b),
        legend.title = element_blank())+
     labs(x = paste("Loan ( ", u, ", log)", sep=""), y="Count")
    ggsave(paste("disGraphs/",treat2[i], ".pdf", sep=""), p, 
           width = 10, height = 6, device="pdf")
    }
}

################################################################
##4.  Figure S3: Loans and Transparency
## The first column: Both Regimes
for (i in 1:4){
Ntransd <- data.frame(subset(Trans, Trans[,expl3[i]]==1))
Ntransa <- data.frame(subset(Trans, Trans[,expl3[i]]==0))
transparency <- Ntransd$transparency_hrv
carrots <- data.frame(transparency)
transparency <- Ntransa$transparency_hrv 
 cucumbers <- data.frame(transparency)
carrots$country <- 'recipient'
cucumbers$country <- 'non-recipient'
Lengths <- rbind(carrots, cucumbers)
  p <- ggplot(Lengths, aes(transparency, fill = country)) + 
  geom_histogram(alpha = 0.5, position = 'identity', bins=50)

    ggsave(paste("disGraphs/LT",expl4[i], ".pdf", sep=""), p, 
           width = 10, height = 6, device="pdf")
}

## The second column: Democracies
for (i in 1:4){
Ntransd <- data.frame(subset(Trans,  Trans[,expl3[i]]==1&democracy_dd_bd==1))
Ntransa <- data.frame(subset(Trans,  Trans[,expl3[i]]==0&democracy_dd_bd==1))
transparency <- Ntransd$transparency_hrv
carrots <- data.frame(transparency)
transparency <- Ntransa$transparency_hrv 
 cucumbers <- data.frame(transparency)
carrots$country <- 'recipient'
cucumbers$country <- 'non-recipient'
Lengths <- rbind(carrots, cucumbers)
  p <- ggplot(Lengths, aes(transparency, fill = country)) + 
      geom_histogram(alpha = 0.5, position = 'identity', bins=50)
    ggsave(paste("disGraphs/LTdem",expl4[i], ".pdf", sep=""), p, 
           width = 10, height = 6, device="pdf")
}
## The third column: Autocracies
for (i in 1:4){
Ntransd <- data.frame(subset(Trans,  Trans[,expl3[i]]==1&democracy_dd_bd==0))
Ntransa <- data.frame(subset(Trans,  Trans[,expl3[i]]==0&democracy_dd_bd==0))
transparency <- Ntransd$transparency_hrv
carrots <- data.frame(transparency)
transparency <- Ntransa$transparency_hrv 
 cucumbers <- data.frame(transparency)
carrots$country <- 'recipient'
cucumbers$country <- 'non-recipient'
Lengths <- rbind(carrots, cucumbers)
  p <- ggplot(Lengths, aes(transparency, fill = country)) + 
      geom_histogram(alpha = 0.5, position = 'identity', bins=50)
    ggsave(paste("disGraphs/LTaut",expl4[i], ".pdf", sep=""), p, 
           width = 10, height = 6, device="pdf")
}
