###############################################################################################################################
##  Replication Code HPRV "Do the Bretton Woods Institutions Promote Economic Transparency?"
##  Method: multilevel analysis
##  Outcome Loan Dummy
##  Contact: Xun Pang, xpang@pku.edu.cn
################################################################

sink("~/Desktop/Replication_ Materials/log/log_multilevelDummy.txt")
################## multilvel analysis begins here (outcome: loan Dummy) #######################

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

## Treatment as dummy
expl <- c( "imf_total_commit_imf_dummy_lag" ,"imf_total_disburse_imf_dummy_lag", "wb_total_commit_wb_dummy_lag", "wb_total_disburse_wb_dummy_lag")

## The treatment names in graphs
expl2 <- c("IMF Commitment", "IMF Disbursement", "WB Commitment", "WB Disbursement")
dep <- c("transparency_hrv")


## M1: Multilevel Analysis with regime-fixed effect (pooling) 
res2 <- matrix(NA, ncol=3, nrow=1)
res2 <- data.frame(res2)
names(res2) <- c("mean", "ci_l", "ci_u")
for(i in 1:length(expl)){
    Cov1  <- c(expl[i],  "transparency_hrv_lag", "democracy_dd_bd_lag",  "trade_wdi_lag", "wtomem_mem_lag", "unsc_dsv_lag", "pop_wdi_lag_ln", "gdppc_wdi_lag_ln", "gdp_wdi_lag_ln","bitstodate_bit_lag", "region", "actotal", "distance")
    ## a self defined function to run multilevel models only with fixed effects 
          M1 <- MLMs_Inter_fit2(dvs = dep,
               ivs =Cov1 ,
               data = Trans )
res2 <- rbind(res2, get.coef(M1, 2))
save(M1, file=paste("DummyM1/",expl[i], ".RData", sep=""))
}

## Get rid of the first row of NA's
res2 <- res2[-1,]
res2$id <- rep("M1", 4)
res2$model <- expl2


## Table A1
load("DummyM1/imf_total_commit_imf_dummy_lag.RData")
IMF_Commit  <- M1
load("DummyM1/imf_total_disburse_imf_dummy_lag.RData")
IMF_Disburse <- M1
load("DummyM1/wb_total_commit_wb_dummy_lag.RData")
WTO_Commit <-M1
load("DummyM1/wb_total_disburse_wb_dummy_lag.RData")
WTO_Disburse <- M1
 texreg(list(IMF_Commit,IMF_Disburse,WTO_Commit,WTO_Disburse ), digits=3)

### M2: Multilevel analysis with regime-varying coefficient
res <- matrix(NA, ncol=4, nrow=1)
res <- data.frame(res)
names(res) <- c("mean", "ci_l", "ci_u", "id" )
for(i in 1:length(expl)){
    Cov1  <- c(expl[i],  "transparency_hrv_lag",  "trade_wdi_lag", "wtomem_mem_lag", "unsc_dsv_lag", "pop_wdi_lag_ln", "gdppc_wdi_lag_ln", "gdp_wdi_lag_ln","bitstodate_bit_lag", "region", "actotal", "distance")
    ## a self defined function to run multilevel models with regime-specific coefficient
   M1 <- MLMs_fit(dvs = dep,
               ivs =Cov1 ,
               data = Trans )
    res <- rbind(res, get.democracy(M1))
    #res <- rbind(res, get.democracy(M1, i=1))
save(M1, file=paste("DummyM2/",expl[i], ".RData", sep=""))
}
res <- res[-1,]
res$model <- rep(expl2, each=2)

## Table A3
load("DummyM2/imf_total_commit_imf_dummy_lag.RData")
IMF_Commit  <- M1
load("DummyM2/imf_total_disburse_imf_dummy_lag.RData")
IMF_Disburse <- M1
load("DummyM2/wb_total_commit_wb_dummy_lag.RData")
WTO_Commit <-M1
load("DummyM2/wb_total_disburse_wb_dummy_lag.RData")
WTO_Disburse <- M1
texreg(list(IMF_Commit,IMF_Disburse,WTO_Commit,WTO_Disburse ), digits=3)
## save the table for table-making in excel (change the variable names)


## M3: Run an interactive model---Regime times Loans
res4 <- matrix(NA, ncol=4, nrow=1)
res4 <- data.frame(res4)
names(res4) <- c("mean", "ci_l", "ci_u", "id")
for(i in 1:length(expl)){
    ind <- paste("democracy_dd_bd_lag*", expl[i])
    Cov1  <-  c(ind,  "transparency_hrv_lag",  "trade_wdi_lag", "wtomem_mem_lag", "unsc_dsv_lag", "pop_wdi_lag_ln",  "gdppc_wdi_lag_ln","gdp_wdi_lag_ln","bitstodate_bit_lag", "region", "actotal", "distance")
          M1 <- MLMs_Inter_fit2(dvs = dep,
               ivs =Cov1 ,
               data = Trans)
                res4 <- rbind(res4, get.democracy.interact(M1))
save(M1, file=paste("DummyM3/",expl[i], ".RData", sep=""))
}
res4 <- res4[-1,]
res4$model <- rep(expl2, each=2)
res3 <- rbind(res, res2, res4)

## Table A5
load("DummyM3/imf_total_commit_imf_dummy_lag.RData")
IMF_Commit  <- M1
load("DummyM3/imf_total_disburse_imf_dummy_lag.RData")
IMF_Disburse <- M1
load("DummyM3/wb_total_commit_wb_dummy_lag.RData")
WTO_Commit <-M1
load("DummyM3/wb_total_disburse_wb_dummy_lag.RData")
WTO_Disburse <- M1
texreg(list(IMF_Commit,IMF_Disburse,WTO_Commit,WTO_Disburse ), digits=3)


## Figure 6 (b)
CIplot1 <- ggplot(res3, aes(colour = factor(id)))
CIplot1 <- CIplot1 + geom_vline(xintercept = 0, colour = gray(1/2), lty = 2)
CIplot1 <- CIplot1 + geom_linerangeh(aes(y = factor(model),
                                         xmin = ci_l, xmax = ci_u), lwd =1, position = position_dodgev(height = 1/2))+
    scale_color_manual(values=c('black',  'purple','gray','orange', 'green'))

CIplot1 <- CIplot1 + geom_pointrangeh(aes(y = factor(model)
                                          , x = mean, xmin = ci_l, xmax = ci_u), lwd =.45,  position = position_dodgev(height = 1/2),
                                      shape = 21, fill = "WHITE")
CIplot1 <- CIplot1 + theme_bw()
#CIplot1 <- CIplot1 + theme(legend.position="bottom")
CIplot1 <- CIplot1 + labs(colour = " ")
CIplot1 <- CIplot1+ xlab(expression(beta)) + ylab("")
#CIplot1 <- CIplot1 +theme(axis.text.y = element_text( hjust = 1, size=a))
CIplot1
ggsave("regGraph/coefallDummy.pdf", CIplot1, 
       width = 5, height = 4, device="pdf")


###################################
## Robustness Checking Begins Here
###################################

## 1.Table S2:  (GDPpc*Loans) only for Democracies to check heterogenous effects on richer and poorer democracies
for(i in 1:length(expl)){
    ind <- paste("gdppc_wdi_lag_ln*", expl[i])
    Cov1  <-  c(ind,  "transparency_hrv_lag",  "trade_wdi_lag", "wtomem_mem_lag", "unsc_dsv_lag", "pop_wdi_lag_ln", "gdp_wdi_lag_ln","bitstodate_bit_lag", "region", "actotal", "distance")
          M1 <- MLMs_Inter_fit2(dvs = dep,
               ivs =Cov1 ,
               data = subset(Trans, democracy_dd_bd_lag==1 ))
save(M1, file=paste("RobustDummy/",expl[i], ".RData", sep=""))
}

## Latex tables
load("RobustDummy/imf_total_commit_imf_dummy_lag.RData")
IMF_Commit  <- M1
load("RobustDummy/imf_total_disburse_imf_dummy_lag.RData")
IMF_Disburse <- M1
load("RobustDummy/wb_total_commit_wb_dummy_lag.RData")
WTO_Commit <-M1
load("RobustDummy/wb_total_disburse_wb_dummy_lag.RData")
WTO_Disburse <- M1
texreg(list(IMF_Commit,IMF_Disburse,WTO_Commit,WTO_Disburse ), digits=3)


## 2. Table S4: Robust Check with (UNSC*Loans)---whether being UNSC member moderates the effect
res5 <- matrix(NA, ncol=4, nrow=1)
res5 <- data.frame(res5)
names(res5) <- c("mean", "ci_l", "ci_u", "id")
for(i in 1:length(expl)){
    ind <- paste("unsc_dsv_lag*", expl[i])
    Cov1  <-  c(ind,  "transparency_hrv_lag",  "trade_wdi_lag", "wtomem_mem_lag", "pop_wdi_lag_ln","gdppc_wdi_lag_ln", "gdp_wdi_lag_ln","bitstodate_bit_lag", "region", "actotal", "distance")
          M1 <- MLMs_Inter_fit2(dvs = dep,
               ivs =Cov1 ,
               data = Trans)
save(M1, file=paste("RobustDummy/",expl[i], ".RData", sep=""))
}

## Latex tables
load("RobustDummy/imf_total_commit_imf_dummy_lag.RData")
IMF_Commit  <- M1
load("RobustDummy/imf_total_disburse_imf_dummy_lag.RData")
IMF_Disburse <- M1
load("RobustDummy/wb_total_commit_wb_dummy_lag.RData")
WTO_Commit <-M1
load("RobustDummy/wb_total_disburse_wb_dummy_lag.RData")
WTO_Disburse <- M1
texreg(list(IMF_Commit,IMF_Disburse,WTO_Commit,WTO_Disburse ), digits=3)



## 3 Table S6:  a linear model with no random effects

for(i in 1:length(expl)){
    Cov1  <- c(expl[i],  "transparency_hrv_lag",  "trade_wdi_lag", "wtomem_mem_lag", "unsc_dsv_lag", "pop_wdi_lag_ln", "gdppc_wdi_lag_ln", "gdp_wdi_lag_ln","bitstodate_bit_lag", "region", "actotal", "distance")
          M1 <- MLs_fit(dvs = dep,
               ivs =Cov1 ,
               data = Trans)
save(M1, file=paste("RobustDummy/",expl[i], ".RData", sep=""))
}
 
## Latex tables
load("RobustDummy/imf_total_commit_imf_dummy_lag.RData")
IMF_Commit  <- M1
load("RobustDummy/imf_total_disburse_imf_dummy_lag.RData")
IMF_Disburse <- M1
load("RobustDummy/wb_total_commit_wb_dummy_lag.RData")
WTO_Commit <-M1
load("RobustDummy/wb_total_disburse_wb_dummy_lag.RData")
WTO_Disburse <- M1
texreg(list(IMF_Commit,IMF_Disburse,WTO_Commit,WTO_Disburse ), digits=3)
