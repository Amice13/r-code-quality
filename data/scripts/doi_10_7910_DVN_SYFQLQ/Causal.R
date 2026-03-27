#########################################################################
##  Replication Code HPRV "Do the Bretton Woods Institutions Promote Economic Transparency?"
##  Method: PanelMatch
##  Outcome Loan Dummy 
##  Contact: Xun Pang, xpang@pku.edu.cn
################################################################


sink("~/Desktop/Replication_ Materials/log/log_causalInference.txt")

#######################################
rm(list=ls())
library(PanelMatch)   ## package version 2.0
library(ggplot2)
setwd("~/Desktop/Replication_ Materials/")
## Read Data: the data are cleaned for causal analysis
Trans <- read.csv("DataforCausal.csv")

## The treatment: two treatment states
treatment <- c( "imf_total_commit_imf_dummy" ,"imf_total_disburse_imf_dummy",  "wb_total_commit_wb_dummy", "wb_total_disburse_wb_dummy")
treatname <- c( "imfcommit","imfdisburse",  "wbcommit","wbdisburse")

## the variable names of the control variables
Cov <-  c( "transparency_hrv_lag","democracy_dd_bd_lag",   "trade_wdi_lag", "wtomem_mem_lag", "unsc_dsv_lag", "pop_wdi_lag_ln", "gdppc_wdi_lag_ln", "actotal", "distance")


#########################################################
## Figure S1 and Figure S2: the treatment assignment plots
########################################################
vn  <- names(Trans)
for (i in 1:length(treatment)){
    xx <- Trans[,which(vn==treatment[i])]
    ## How many countries that have ever received aid?
    dd <- tapply(xx, Trans$ccode, function(x)sum(x,na.rm = TRUE))
    length(dd)
    cc <- tapply(xx, Trans$ccode, function(x)length(which(!is.na(x))))
    treatunit <- length(which(dd>0))
    ## Return the counting
    ## treatment plot
    gg <- DisplayTreatment(unit.id = "ccode",
                 time.id = "year", legend.position = "bottom",
                 xlab = "year", ylab = "Country Code",
                 treatment = treatment[i], data = Trans)
   ## Save the plot
    ggsave(paste("causalGraph/",treatname[i], "TreatPlot.pdf", sep=""), gg, 
       width = 8, height = 6, device="pdf")

    ## Partitioning by Regime
    TransD <- subset(Trans, democracy_dd_bd==1)
    TransA <- subset(Trans, democracy_dd_bd==0)
    xx <- TransD[,which(vn==treatment[i])]  
    ## How many countries that have ever received aid?
    dd <- tapply(xx, TransD$ccode, function(x)sum(x,na.rm = TRUE))
    length(dd)
    cc <- tapply(xx, TransD$ccode, function(x)length(which(!is.na(x))))
    treatunit <- length(which(dd>0))
    
    ## How many countries that always received aid   
    xx <- TransA[,which(vn==treatment[i])]
    ## How many countries that have ever received aid?
    dd <- tapply(xx, TransA$ccode, function(x)sum(x,na.rm = TRUE))
    length(dd)
    cc <- tapply(xx, TransA$ccode, function(x)length(which(!is.na(x))))
    treatunit <- length(which(dd>0))
    
## How many countries that always received aid     
  ggD <- DisplayTreatment(unit.id = "ccode",
                 time.id = "year", legend.position = "bottom",
                 xlab = "year", ylab = "Country Code",
                 treatment = treatment[i], data = TransD)

   ggsave(paste("causalGraph/",treatname[i], "TreatPlotDem.pdf", sep=""), ggD, 
       width = 8, height = 6, device="pdf")
    
   ggA <- DisplayTreatment(unit.id = "ccode",
                 time.id = "year", legend.position = "bottom",
                 xlab = "year", ylab = "Country Code",
                 treatment = treatment[i], data = TransA)

   ggsave(paste("causalGraph/",treatname[i], "TreatPlotAnt.pdf", sep=""), ggA, 
       width = 8, height = 6, device="pdf")  
}

### Analyses with 3 lags and 8 future periods 
L <- 3
F <- 8   ## Change the value of F to be  8 or 4
for (i in 1:length(treatment)){
fm  <- as.formula(paste(" ", paste(Cov, collapse=" + "), sep=" ~ "))
dep <- c("transparency_hrv")

## Causal Inference with no moderator
PM.results <- PanelMatch(lag = L, time.id = "year", unit.id = "ccode", 
                         treatment = treatment[i], refinement.method = "CBPS.match", #Covariate Balancing Propensity 
                         data =Trans, match.missing = TRUE, 
                         covs.formula =fm, 
                         size.match = 5, qoi = "att", outcome.var = dep,
                         lead = 0:F,  forbid.treatment.reversal = FALSE, 
                         use.diagonal.variance.matrix = TRUE)
## Calculate the size of the matched set
mm <- data.frame(summary(PM.results$att)[[1]])
length(unique(mm[mm[,3]>0,1]))
mm <- data.frame(summary(PM.results$att)[[1]])
cat(treatment[i], "ATT has matched countries #=", length(unique(mm[mm[,3]>0,1])), ", matched obs.#= ", length(mm[,3]>0)," .\n")

PE.results <- PanelEstimate(sets = PM.results, data =Trans)

##########################
## Figure 4 and Figure 5
##########################

## Plot the estimated ATTs with 95% CIs
## The 1st column of Figure 4 and Figure 5
pdf(paste("causalGraph/",treatname[i],L,F, "ATT.pdf", sep="")) 
# 2. Create a plot
plot(PE.results, main=" ", cex.axis=1.5, cex.lab=1.5)
dev.off()

## Estimate democracy-autocracy ATTs: Democracy as moderator
PE.results1 <- PanelEstimate(sets = PM.results, data =Trans, moderator = "democracy_dd_bd") 
## Plot the estimated ATTs on democracies with 95% CIs
## The 2nd column of Figure 4 and Figure 5
pdf(paste("causalGraph/Dem",treatname[i],L,F, "ATT.pdf", sep="")) 
plot(PE.results1[[1]], main=" ", cex.axis=1.5, cex.lab=1.5)
dev.off()
## Plot the estimated ATTs on autoracies with 95% CIs
## The 3rd column of Figure 4 and Figure 5
pdf(paste("causalGraph/Aut",treatname[i],L,F, "ATT.pdf", sep="")) 
# 2. Create a plot
plot(PE.results1[[2]], main=" ", cex.axis=1.5, cex.lab=1.5)
dev.off()

##########################
## Figures S4, S5, S6, S7
##########################
## Estimate regime specific ATTs: regime with six categories as moderator
## Regime as moderator
PE.results2 <- PanelEstimate(sets = PM.results, data =Trans, moderator = "regime_dd_bd_lag") # Use did estimator
pdf(paste("causalGraph/Presidential",treatname[i],L,F, "ATT.pdf", sep="")) 
# 2. Create a plot
plot(PE.results2[[1]], main=" ", cex.axis=1.5, cex.lab=1.5)
dev.off()

pdf(paste("causalGraph/Parliamentary",treatname[i],L,F, "ATT.pdf", sep="")) 
# 2. Create a plot
plot(PE.results2[[2]], main=" ", cex.axis=1.5, cex.lab=1.5)
dev.off()

pdf(paste("causalGraph/Civilian",treatname[i],L,F, "ATT.pdf", sep="")) 
# 2. Create a plot
plot(PE.results2[[3]], main=" ", cex.axis=1.5, cex.lab=1.5)
dev.off()

pdf(paste("causalGraph/Military",treatname[i],L,F, "ATT.pdf", sep="")) 
# 2. Create a plot
plot(PE.results2[[4]], main=" ", cex.axis=1.5, cex.lab=1.5)
dev.off()

pdf(paste("causalGraph/Mixed",treatname[i],L,F, "ATT.pdf", sep="")) 
# 2. Create a plot
plot(PE.results2[[5]], main=" ", cex.axis=1.5, cex.lab=1.5)
dev.off()

pdf(paste("causalGraph/Royal",treatname[i],L,F, "ATT.pdf", sep="")) 
# 2. Create a plot
plot(PE.results2[[6]], main=" ", cex.axis=1.5, cex.lab=1.5)
dev.off()

##############################
## Figures S8, S9, S10, and S11                                     
##########################
# Median Level Transparency as Moderator
PE.results3 <- PanelEstimate(sets = PM.results, data =Trans, moderator = "med") # Use did estimator
# 2. Create a plot
pdf(paste("causalGraph/Above",treatname[i],L,F, "ATT.pdf", sep="")) 
plot(PE.results3[[1]], main=" ", cex.axis=1.5, cex.lab=1.5)
dev.off()

pdf(paste("causalGraph/Below",treatname[i],L,F, "ATT.pdf", sep="")) 
# 2. Create a plot
plot(PE.results3[[2]], main=" ", cex.axis=1.5, cex.lab=1.5)
dev.off()

## Median Level Transparency and Democracy as Moderator
PE.results4 <- PanelEstimate(sets = PM.results, data =Trans, moderator = "md") # Use did estimator
# 2. Create a plot
pdf(paste("causalGraph/AbDemo",treatname[i],L,F, "ATT.pdf", sep="")) 
plot(PE.results4[[1]], main=" ", cex.axis=1.5, cex.lab=1.5)
dev.off()

pdf(paste("causalGraph/BelAut",treatname[i],L,F, "ATT.pdf", sep="")) 
# 2. Create a plot
plot(PE.results4[[2]], main=" ", cex.axis=1.5, cex.lab=1.5)
dev.off()

pdf(paste("causalGraph/BelDemo",treatname[i],L,F, "ATT.pdf", sep="")) 
# 2. Create a plot
plot(PE.results4[[3]], main=" ", cex.axis=1.5, cex.lab=1.5)
dev.off()

pdf(paste("causalGraph/AbAut",treatname[i],L,F, "ATT.pdf", sep="")) 
# 2. Create a plot
plot(PE.results4[[4]], main=" ", cex.axis=1.5, cex.lab=1.5)
dev.off()
}

##############################
## Figures S12, S13, S14, and S15                                  
##########################
Cov2 <-  c( "transparency_hrv_lag","democracy_dd_bd_lag",   "trade_wdi_lag", "wtomem_mem_lag", "pop_wdi_lag_ln", "gdppc_wdi_lag_ln", "actotal", "distance")

for (i in 1:length(treatment)){
fm  <- as.formula(paste(" ", paste(Cov2, collapse=" + "), sep=" ~ "))
dep <- c("transparency_hrv")

## Do the matching and refinement
PM.results <- PanelMatch(lag = L, time.id = "year", unit.id = "ccode", 
                         treatment = treatment[i], refinement.method = "CBPS.match", #Covariate Balancing Propensity 
                         data =Trans, match.missing = TRUE, 
                         covs.formula =fm, 
                         size.match = 5, qoi = "att", outcome.var = dep,
                         lead = 0:F,  forbid.treatment.reversal = FALSE, 
                         use.diagonal.variance.matrix = TRUE)
mm <- data.frame(summary(PM.results$att)[[1]])
length(unique(mm[mm[,3]>0,1]))
mm <- data.frame(summary(PM.results$att)[[1]])
cat(treatment[i], "ATT has matched countries #=", length(unique(mm[mm[,3]>0,1])), ", matched obs.#= ", length(mm[,3]>0)," .\n")
## UNSC as moderator
PE.results5 <- PanelEstimate(sets = PM.results, data =Trans, moderator = "unsc_dsv_lag") # Use did estimator
pdf(paste("causalGraph/unscN",treatname[i],L,F, "ATT.pdf", sep="")) 
# 2. Create a plot
plot(PE.results5[[1]], main=" ", cex.axis=1.5, cex.lab=1.5)
dev.off()

pdf(paste("causalGraph/unsc",treatname[i],L,F, "ATT.pdf", sep="")) 
# 2. Create a plot
plot(PE.results5[[2]], main=" ", cex.axis=1.5, cex.lab=1.5)
dev.off()

## Regime as moderator
PE.results6 <- PanelEstimate(sets = PM.results, data =Trans, moderator = "unscd") # Use did estimator
pdf(paste("causalGraph/unscNDem",treatname[i],L,F, "ATT.pdf", sep="")) 
# 2. Create a plot
plot(PE.results6[[1]], main=" ", cex.axis=1.5, cex.lab=1.5)
dev.off()

pdf(paste("causalGraph/unscDem",treatname[i],L,F, "ATT.pdf", sep="")) 
# 2. Create a plot
plot(PE.results6[[2]], main=" ", cex.axis=1.5, cex.lab=1.5)
dev.off()

pdf(paste("causalGraph/unscNAut",treatname[i],L,F, "ATT.pdf", sep="")) 
# 2. Create a plot
plot(PE.results6[[3]], main=" ", cex.axis=1.5, cex.lab=1.5)
dev.off()

pdf(paste("causalGraph/unscAut",treatname[i],L,F, "ATT.pdf", sep="")) 
# 2. Create a plot
plot(PE.results6[[4]], main=" ", cex.axis=1.5, cex.lab=1.5)
dev.off()
} 

### Analyses with 3 lags and 4 future periods 
L <- 3
F <- 4   ## Change the value of F to be  8 or 4
for (i in 1:length(treatment)){
fm  <- as.formula(paste(" ", paste(Cov, collapse=" + "), sep=" ~ "))
dep <- c("transparency_hrv")

## Causal Inference with no moderator
PM.results <- PanelMatch(lag = L, time.id = "year", unit.id = "ccode", 
                         treatment = treatment[i], refinement.method = "CBPS.match", #Covariate Balancing Propensity 
                         data =Trans, match.missing = TRUE, 
                         covs.formula =fm, 
                         size.match = 5, qoi = "att", outcome.var = dep,
                         lead = 0:F,  forbid.treatment.reversal = FALSE, 
                         use.diagonal.variance.matrix = TRUE)
## Calculate the size of the matched set
mm <- data.frame(summary(PM.results$att)[[1]])
length(unique(mm[mm[,3]>0,1]))
mm <- data.frame(summary(PM.results$att)[[1]])
cat(treatment[i], "ATT has matched countries #=", length(unique(mm[mm[,3]>0,1])), ", matched obs.#= ", length(mm[,3]>0)," .\n")

PE.results <- PanelEstimate(sets = PM.results, data =Trans)

##########################
## Figure A1 and Figure A2
##########################

## Plot the estimated ATTs with 95% CIs
## The 1st column of Figure A1 and Figure A2
pdf(paste("causalGraph/",treatname[i],L,F, "ATT.pdf", sep="")) 
# 2. Create a plot
plot(PE.results, main=" ", cex.axis=1.5, cex.lab=1.5)
dev.off()

## Estimate democracy-autocracy ATTs: Democracy as moderator
PE.results1 <- PanelEstimate(sets = PM.results, data =Trans, moderator = "democracy_dd_bd") 
## Plot the estimated ATTs on democracies with 95% CIs
## The 2nd column of Figure A1 and Figure A2
pdf(paste("causalGraph/Dem",treatname[i],L,F, "ATT.pdf", sep="")) 
plot(PE.results1[[1]], main=" ", cex.axis=1.5, cex.lab=1.5)
dev.off()
## Plot the estimated ATTs on autoracies with 95% CIs
## The 3rd column of Figure A1 and Figure A2
pdf(paste("causalGraph/Aut",treatname[i],L,F, "ATT.pdf", sep="")) 
# 2. Create a plot
plot(PE.results1[[2]], main=" ", cex.axis=1.5, cex.lab=1.5)
dev.off()
}



### Analyses with 5 lags and 8 future periods 
L <- 5
F <- 8   ## Change the value of F to be  8 or 4
for (i in 1:length(treatment)){
fm  <- as.formula(paste(" ", paste(Cov, collapse=" + "), sep=" ~ "))
dep <- c("transparency_hrv")

## Causal Inference with no moderator
PM.results <- PanelMatch(lag = L, time.id = "year", unit.id = "ccode", 
                         treatment = treatment[i], refinement.method = "CBPS.match", #Covariate Balancing Propensity 
                         data =Trans, match.missing = TRUE, 
                         covs.formula =fm, 
                         size.match = 5, qoi = "att", outcome.var = dep,
                         lead = 0:F,  forbid.treatment.reversal = FALSE, 
                         use.diagonal.variance.matrix = TRUE)
## Calculate the size of the matched set
mm <- data.frame(summary(PM.results$att)[[1]])
length(unique(mm[mm[,3]>0,1]))
mm <- data.frame(summary(PM.results$att)[[1]])
cat(treatment[i], "ATT has matched countries #=", length(unique(mm[mm[,3]>0,1])), ", matched obs.#= ", length(mm[,3]>0)," .\n")

PE.results <- PanelEstimate(sets = PM.results, data =Trans)

##########################
## Figure A3 and Figure A4
##########################

## Plot the estimated ATTs with 95% CIs
## The 1st column of Figure A3 and Figure A4
pdf(paste("causalGraph/",treatname[i],L,F, "ATT.pdf", sep="")) 
# 2. Create a plot
plot(PE.results, main=" ", cex.axis=1.5, cex.lab=1.5)
dev.off()

## Estimate democracy-autocracy ATTs: Democracy as moderator
PE.results1 <- PanelEstimate(sets = PM.results, data =Trans, moderator = "democracy_dd_bd") 
## Plot the estimated ATTs on democracies with 95% CIs
## The 2nd column of Figure A3 and Figure A4
pdf(paste("causalGraph/Dem",treatname[i],L,F, "ATT.pdf", sep="")) 
plot(PE.results1[[1]], main=" ", cex.axis=1.5, cex.lab=1.5)
dev.off()
## Plot the estimated ATTs on autoracies with 95% CIs
## The 3rd column of Figure A3 and Figure A4
pdf(paste("causalGraph/Aut",treatname[i],L,F, "ATT.pdf", sep="")) 
# 2. Create a plot
plot(PE.results1[[2]], main=" ", cex.axis=1.5, cex.lab=1.5)
dev.off()
}


sink()
