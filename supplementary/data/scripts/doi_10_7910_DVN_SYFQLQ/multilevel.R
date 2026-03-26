#########################################################################
##  Replication Code HPRV "Do the Bretton Woods Institutions Promote Economic Transparency?"
##  Method: multilevel analysis
##  Outcome Loan size 
##  Contact: Xun Pang, xpang@pku.edu.cn
################################################################


sink("~/Desktop/Replication_ Materials/log/log_multilevelSize.txt")
################## multilvel analysis begins here (outcome: loan size) #######################

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

## the key treatment variable names in the dataset
expl <-  c(  "imf_total_commit_imf_lag_ln" , "imf_total_disburse_imf_lag_ln", "wb_total_commit_wb_lag_ln", "wb_total_disburse_wb_lag_ln")

## to rename the treatment variables using the expl2
expl2 <- c("IMF Commitment", "IMF Disbursement", "WB Commitment", "WB Disbursement")

## the name of dependent variable
dep <- c("transparency_hrv")


###### Run Model1, Model 2, and Model 3 as in Figure 6 (a)
###### The loops in the following code are to execute the models with four different treatment variables


##  Model 1: the multilevel model with fixed coefficient of regime type 
res2 <- matrix(NA, ncol=3, nrow=1)
res2 <- data.frame(res2)
names(res2) <- c("mean", "ci_l", "ci_u")
for(i in 1:length(expl)){
    Cov1  <- c(expl[i],  "transparency_hrv_lag", "democracy_dd_bd_lag", "trade_wdi_lag", "wtomem_mem_lag", "unsc_dsv_lag", "pop_wdi_lag_ln", "gdppc_wdi_lag_ln", "gdp_wdi_lag_ln","bitstodate_bit_lag", "region", "actotal", "distance")
    ## a self defined function to run multilevel models only with fixed effects 
          M1 <- MLMs_Inter_fit2(dvs = dep,
               ivs =Cov1 ,
               data = Trans )
res2 <- rbind(res2, get.coef(M1, 2))
save(M1, file=paste("SizeM1/",expl[i], ".RData", sep=""))
}
## Putting the estimates and CI's together and Get rid of the first row of NA's

res2 <- res2[-1,]
res2$id <- rep("M1", 4)
res2$model <- expl2


#### Table 2 ############
## MLM without regime-varying coefficients
load("SizeM1/imf_total_commit_imf_lag_ln.RData")
IMF_Commit  <- M1
load("SizeM1/imf_total_disburse_imf_lag_ln.RData")
IMF_Disburse <- M1
load("SizeM1/wb_total_commit_wb_lag_ln.RData")
WTO_Commit <-M1
load("SizeM1/wb_total_disburse_wb_lag_ln.RData")
WTO_Disburse <- M1
texreg(list(IMF_Commit,IMF_Disburse,WTO_Commit,WTO_Disburse ), digits=3)

### Model 2: the mltilevel model with regime-varying coefficient
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

## Save the regression results in the folder named as MultiReg    
save(M1, file=paste("SizeM2/",expl[i], ".RData", sep=""))
}

## warning messages returned, and it means that our model did fit, but it generated that warning because the random effects are very small.https://stackoverflow.com/questions/60028673/lme4-error-boundary-singular-fit-see-issingular

res <- res[-1,]
res$model <- rep(expl2, each=2)

## load model estimates to make regression table
## MLM with regime-varying coefficients
load("SizeM2/imf_total_commit_imf_lag_ln.RData")
IMF_Commit  <- M1
load("SizeM2/imf_total_disburse_imf_lag_ln.RData")
IMF_Disburse <- M1
load("SizeM2/wb_total_commit_wb_lag_ln.RData")
WTO_Commit <-M1
load("SizeM2/wb_total_disburse_wb_lag_ln.RData")
WTO_Disburse <- M1

#### Table A2 ############
## MLM without regime-varying coefficients
texreg(list(IMF_Commit,IMF_Disburse,WTO_Commit,WTO_Disburse ), digits=3)

## Model 3: Multilevel Model with interaction term of Regime type and loan size 
res4 <- matrix(NA, ncol=4, nrow=1)
res4 <- data.frame(res4)
names(res4) <- c("mean", "ci_l", "ci_u", "id")
for(i in 1:length(expl)){
    ind <- paste("democracy_dd_bd_lag*", expl[i])
    Cov1  <-  c(ind,  "transparency_hrv_lag",  "trade_wdi_lag", "wtomem_mem_lag", "unsc_dsv_lag", "pop_wdi_lag_ln", "gdppc_wdi_lag_ln", "gdp_wdi_lag_ln","bitstodate_bit_lag", "region", "actotal", "distance")
          M1 <- MLMs_Inter_fit2(dvs = dep,
               ivs =Cov1 ,
               data = Trans)
        res4 <- rbind(res4, get.democracy.interact(M1))
save(M1, file=paste("SizeM3/",expl[i], ".RData", sep=""))
}

## warning messages returned, and it means that our model did fit, but it generated that warning because the random effects are very small.

res4 <- res4[-1,]
res4$model <- rep(expl2, each=2)

## Latex tables
load("SizeM3/imf_total_commit_imf_lag_ln.RData")
IMF_Commit4  <- M1
load("SizeM3/imf_total_disburse_imf_lag_ln.RData")
IMF_Disburse4 <- M1
load("SizeM3/wb_total_commit_wb_lag_ln.RData")
WTO_Commit4 <-M1
load("SizeM3/wb_total_disburse_wb_lag_ln.RData")
WTO_Disburse4 <- M1

#### Table A4 ############
texreg(list(IMF_Commit4,IMF_Disburse4,WTO_Commit4,WTO_Disburse4 ), digits=3)


#### Figure 6 (a) ###########

## Put the coefficients together

res3 <- rbind(res, res2, res4)

## Making the graphs of coefficients of treatments including the interaction model

CIplot1 <- ggplot(res3, aes(colour = factor(id))) 

CIplot1 <- CIplot1 + geom_vline(xintercept = 0, colour = gray(1/2), lty = 2)
CIplot1 <- CIplot1 + geom_linerangeh(aes(y = factor(model),
                                         xmin = ci_l, xmax = ci_u), lwd =1, position = position_dodgev(height = 1/2))+
    scale_color_manual(values=c('black',  'purple','gray','orange', 'green'))

CIplot1 <- CIplot1 + geom_pointrangeh(aes(y = factor(model)
                                        , x = mean, xmin = ci_l, xmax = ci_u), lwd =.45,  position = position_dodgev(height = 1/2), shape = 21, fill = "WHITE")

CIplot1 <- CIplot1 + theme_bw()
CIplot1
ggsave("regGraph/coefall.pdf", CIplot1, 
       width = 5, height = 4, device="pdf")



###################################
## Robustness Checking Begins Here
###################################

## 1. Table S7: Loan Per capita as treatment

## Treatment as yearly amount
expl3 <- c("imf_commit_pc_lag_ln", "imf_disburse_pc_lag_ln", "wb_commit_pc_lag_ln", "wb_disburse_pc_lag_ln")

## The treatment names in graphs
expl4 <- c("IMF Commitment pc", "IMF Disbursement pc", "WB Commitment pc", "WB Disbursement pc")

## the name of dependent variable
dep <- c("transparency_hrv")

## Run the Best Model: Model 2 
res2 <- matrix(NA, ncol=3, nrow=1)
res2 <- data.frame(res2)
names(res2) <- c("mean", "ci_l", "ci_u")
for(i in 1:length(expl3)){
    Cov1  <- c(expl3[i],  "transparency_hrv_lag", "democracy_dd_bd_lag", "trade_wdi_lag", "wtomem_mem_lag", "unsc_dsv_lag",  "gdppc_wdi_lag_ln", "gdp_wdi_lag_ln","bitstodate_bit_lag", "region", "actotal", "distance")
    ## a self defined function to run multilevel models only with fixed effects 
          M1 <- MLMs_Inter_fit2(dvs = dep,
               ivs =Cov1 ,
               data = Trans )
res2 <- rbind(res2, get.coef(M1, 2))
save(M1, file=paste("RobustSize/",expl3[i], ".RData", sep=""))
}

## Latex tables
load("RobustSize/imf_commit_pc_lag_ln.RData")
IMF_Commit4  <- M1
load("RobustSize/imf_disburse_pc_lag_ln.RData")
IMF_Disburse4 <- M1
load("RobustSize/wb_commit_pc_lag_ln.RData")
WTO_Commit4 <-M1
load("RobustSize/wb_disburse_pc_lag_ln.RData")
WTO_Disburse4 <- M1

## Make Table S7
texreg(list(IMF_Commit4,IMF_Disburse4,WTO_Commit4,WTO_Disburse4 ), digits=3)



## 2.Table S1:  (GDPpc*Loans) only for Democracies to check heterogenous effects on richer and poorer democracies
res5 <- matrix(NA, ncol=4, nrow=1)
res5 <- data.frame(res5)
names(res5) <- c("mean", "ci_l", "ci_u", "id")
for(i in 1:length(expl)){
    ind <- paste("gdppc_wdi_lag_ln*", expl[i])
    Cov1  <-  c(ind,  "transparency_hrv_lag",  "trade_wdi_lag", "wtomem_mem_lag", "unsc_dsv_lag", "pop_wdi_lag_ln", "gdp_wdi_lag_ln","bitstodate_bit_lag", "region", "actotal", "distance")
          M1 <- MLMs_Inter_fit2(dvs = dep,
               ivs =Cov1 ,
               data = subset(Trans, democracy_dd_bd_lag==1 ))
save(M1, file=paste("RobustSize/",expl[i], ".RData", sep=""))
}


## Latex tables
load("RobustSize/imf_total_commit_imf_lag_ln.RData")
IMF_Commit3  <- M1
load("RobustSize/imf_total_disburse_imf_lag_ln.RData")
IMF_Disburse3 <- M1
load("RobustSize/wb_total_commit_wb_lag_ln.RData")
WTO_Commit3 <-M1
load("RobustSize/wb_total_disburse_wb_lag_ln.RData")
WTO_Disburse3 <- M1
texreg(list(IMF_Commit3,IMF_Disburse3,WTO_Commit3,WTO_Disburse3 ), digits=3)


## 3. Table S3: Robust Check with (UNSC*Loans)---whether being UNSC member moderates the effect
res5 <- matrix(NA, ncol=4, nrow=1)
res5 <- data.frame(res5)
names(res5) <- c("mean", "ci_l", "ci_u", "id")
for(i in 1:length(expl)){
    ind <- paste("unsc_dsv_lag*", expl[i])
    Cov1  <-  c(ind,  "transparency_hrv_lag",  "trade_wdi_lag", "wtomem_mem_lag", "pop_wdi_lag_ln","gdppc_wdi_lag_ln", "gdp_wdi_lag_ln","bitstodate_bit_lag", "region", "actotal", "distance") 
          M1 <- MLMs_Inter_fit2(dvs = dep,
               ivs =Cov1 ,
               data = Trans)
save(M1, file=paste("RobustSize/",expl[i], ".RData", sep=""))
}

## Latex tables
load("RobustSize/imf_total_commit_imf_lag_ln.RData")
IMF_Commit3  <- M1
load("RobustSize/imf_total_disburse_imf_lag_ln.RData")
IMF_Disburse3 <- M1
load("RobustSize/wb_total_commit_wb_lag_ln.RData")
WTO_Commit3 <-M1
load("RobustSize/wb_total_disburse_wb_lag_ln.RData")
WTO_Disburse3 <- M1
texreg(list(IMF_Commit3,IMF_Disburse3,WTO_Commit3,WTO_Disburse3 ), digits=3)


## 4 Table S5:  a linear model with no random effects

for(i in 1:length(expl)){
    Cov1  <- c(expl[i],  "transparency_hrv_lag",  "trade_wdi_lag", "wtomem_mem_lag", "unsc_dsv_lag", "pop_wdi_lag_ln", "gdppc_wdi_lag_ln", "gdp_wdi_lag_ln","bitstodate_bit_lag", "region", "actotal", "distance") 
          M1 <- MLs_fit(dvs = dep,
               ivs =Cov1 ,
               data = Trans)
save(M1, file=paste("RobustSize/",expl[i], ".RData", sep=""))
}
 
## Latex tables
load("RobustSize/imf_total_commit_imf_lag_ln.RData")
IMF_Commit3  <- M1
load("RobustSize/imf_total_disburse_imf_lag_ln.RData")
IMF_Disburse3 <- M1
load("RobustSize/wb_total_commit_wb_lag_ln.RData")
WTO_Commit3 <-M1
load("RobustSize/wb_total_disburse_wb_lag_ln.RData")
WTO_Disburse3 <- M1
texreg(list(IMF_Commit3,IMF_Disburse3,WTO_Commit3,WTO_Disburse3 ), digits=3)


sink()
