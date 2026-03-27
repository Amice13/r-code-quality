############################################################################################################################################
## Replication Data for:                                                                                                                   #
## Wawro, Gregory, and Ira Katznelson. Time Counts: Quantitative Analysis for Historical Social Science. Princeton University Press, 2022. #
############################################################################################################################################
# Run Figure 4-9, 4-10.R first

rm(list=ls()) 
library(arm)
library(VGAM)

fldr        <- '../Output/bayesx'
a           <- read.table(paste0(fldr, "/bayesx.estim_f_district_spatial_sample.raw"), header=T)[,-1]
b.union     <- read.table(paste0(fldr, "/bayesx.estim_unionpop_f_district_spatial_sample.raw"), header=T)[,-1]
b.aapct     <- read.table(paste0(fldr, "/bayesx.estim_aapct_f_district_spatial_sample.raw"), header=T)[,-1]
b.urbanpct  <- read.table(paste0(fldr, "/bayesx.estim_urbanpct_f_district_spatial_sample.raw"), header=T)[,-1]
b.fe        <- read.table(paste0(fldr, "/bayesx.estim_FixedEffects1_sample.raw"), header=T)
b.dem       <- b.fe$b_2
b.laborcomm <- b.fe$b_3

a.names     <- c("73rd,Deep South","73rd,Border South","73rd,Non-South","74th,Deep South","74th,Border South","74th,Non-South","75th,Deep South",
                 "75th,Border South","75th,Non-South","76th,Deep South","76th,Border South","76th,Non-South","77th,Deep South","77th,Border South",
                 "77th,Non-South","78th,Deep South","78th,Border South","78th,Non-South","79th,Deep South","79th,Border South","79th,Non-South",
                 "80th,Deep South","80th,Border South","80th,Non-South")
a.names.alt <- t(matrix(a.names,3,8))

par(mfrow=c(1,2))
rnames   <- matrix(a.names.alt,1,24)
names.ds <- rnames[,1]
texfile  <- "../Tables/Table 4-2.tex"
roundval <- 2
lb <- .05
ub <- .95

cat("\\begin{table}\\centering \\small \\ssp \n ",file=texfile,append=FALSE)
cat("\\begin{threeparttable}\n ",file=texfile,append=TRUE)
cat("\\caption{Results from analysis of labor roll call votes in the Senate using Markov field random priors (73rd--80th Congresses)} \n \\label{tab:rc_probits_mrf} \n",file=texfile,append=TRUE)
cat("\\begin{tabular}{ld{-1}d{-1}d{-1}d{-1}} \n \\toprule \n \\midrule \n",file=texfile,append=TRUE)
cat("& \\multicolumn{1}{c}{Region-Period Effect} & \\multicolumn{1}{c}{Union} & \\multicolumn{1}{c}{AA\\%} & \\multicolumn{1}{c}{Urban \\%} \\\\ \n",file=texfile,append=TRUE)


## Compute median and credible interval and see if latter include 0
## Loop through each region and each coefficient
ests      <- cbind(apply(a,2,median),apply(b.union,2,median),apply(b.aapct,2,median),apply(b.urbanpct,2,median))
a.sig     <- ifelse(apply(a,2,quantile,(probs=c(lb,ub)))[1,] <= 0 & apply(a,2,quantile,(probs=c(lb,ub)))[2,] >= 0,"","^{*}")
b.un.sig  <- ifelse(apply(b.union,2,quantile,(probs=c(lb,ub)))[1,] <= 0 & apply(b.union,2,quantile,(probs=c(lb,ub)))[2,] >= 0,"","^{*}")
b.aa.sig  <- ifelse(apply(b.aapct,2,quantile,(probs=c(lb,ub)))[1,] <= 0 & apply(b.aapct,2,quantile,(probs=c(lb,ub)))[2,] >= 0,"","^{*}")
b.urb.sig <- ifelse(apply(b.urbanpct,2,quantile,(probs=c(lb,ub)))[1,] <= 0 & apply(b.urbanpct,2,quantile,(probs=c(lb,ub)))[2,] >= 0,"","^{*}")
sigmat    <- cbind(a.sig,b.un.sig,b.aa.sig,b.urb.sig)

for (t in 1:length(rnames)){
  if (t == 9 | t == 17) cat("\n \\midrule \n",file=texfile,append=TRUE)
  cat(rnames[t],file=texfile,append=TRUE)
  for (k in 1:ncol(ests)){
    cat (paste(" & ", round(ests[t,k],roundval),sigmat[t,k]),file=texfile,append=TRUE)
  }
  cat (" \\\\\n ",file=texfile,append=TRUE)
}

cat("\n \\midrule \\midrule \n",file=texfile,append=TRUE)
cat(paste("Democrat &", round(median(b.dem),roundval),ifelse(quantile(b.dem,(probs=c(lb,ub)))[1] <= 0 & apply(a,2,quantile,(probs=c(lb,ub)))[2] >= 0,"","^{*}")," \\\\ \n"),file=texfile,append=TRUE)
cat(paste("Labor Committe &", round(median(b.laborcomm),roundval),ifelse(quantile(b.dem,(probs=c(lb,ub)))[1] <= 0 & apply(a,2,quantile,(probs=c(lb,ub)))[2] >= 0,"","^{*}")," \\\\ \n"),file=texfile,append=TRUE)
cat("\\bottomrule \n \\end{tabular}\n \\begin{tablenotes} \n \\item \n {\\em Notes}: \\ssp $^{*} p \\leq .1$. Estimation using MCMC implemented in {\tt BayesX}, 100,000 iterations (first 10,000 discarded). \\textit{Union} indicates union density, \\textit{AA} indicates percent African-American, \\textit{Urban} indicates percent urbanization, and \\textit{Labor Committee} and \\textit{Democrat} refer to dummy variables indicating membership on the labor committee and in Democratic party, respectively. \n \\end{tablenotes} \n \\end{threeparttable} \n \\hspace{\\fill} \n \\end{table}\n \n \n",file=texfile,append=TRUE)

