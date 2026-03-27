##########################################################################
########## This code implements the simulations in Section 9.2 ##########
##########################################################################

#### The data set is from "https://economics.mit.edu/faculty/angrist/data1/data/angrist," ####

library(foreign)

### read the R functions
source("Function_RerandCov.R")

##########################################################################
############## read and preprocess the data preprocessing ################
##########################################################################
#### data preprocessing ####
# load the data
alldata <- read.dta("OKgradesUpdate_Feb5_2010 anonymized.dta")
# alldata <- read.dta("/Users/lixinran/Dropbox/Own/Projects/rerandom_covadj/Real data/opportunity knocks replication files/OKgradesUpdate_Feb5_2010 anonymized.dta")
alldata$s_mothercolabove = alldata$s_mothercolldegree + alldata$s_mothergraddegree
alldata$s_fathercolabove = alldata$s_fathercolldegree + alldata$s_fathergraddegree
# consider only second year students
alldata = alldata[alldata$s_first_year==0,]
rownames(alldata) = NULL
# get covariates and observed outcomes 
covariatename = c("s_male",
                  "s_group_quart",
                  "s_hsgrade3", 
                  "s_mtongue_english",  
                  "s_test1correct", "s_test2correct", 
                  "s_mothercolabove", 
                  "s_fathercolabove", 
                  "s_gpapreviousyear"
)
treatmentname = "T"
outcomename = "avggradefall2008" 
unit_na = as.vector( which( rowSums( is.na(alldata[, covariatename]) ) + is.na(alldata[, outcomename]) > 0 ) )
data = alldata[-unit_na, c(outcomename, treatmentname, covariatename)]


##########################################################################
########### fit model and impute missing potential outcomes ##############
##########################################################################
strata_index = names( table( alldata[, "s_group_quart"] ) )
data$Y1 = NA
data$Y0 = NA
set.seed(1)
for(k in 1:length(strata_index)){
  units = which(data$s_group_quart == strata_index[k])
  datasub = data[units,]
  lmsub = lm(avggradefall2008 ~ T + s_hsgrade3 + s_mtongue_english + s_test1correct + s_test2correct + s_mothercolabove + s_fathercolabove+ s_gpapreviousyear, data = datasub)
  resi_sd = sqrt(sum(lmsub$residuals^2)/lmsub$df.residual)
  datasub1 = datasub
  datasub1$T = 1
  datasub0 = datasub
  datasub0$T = 0
  data$Y1[units] = predict(lmsub, datasub1) + resi_sd * rnorm(length(units))
  data$Y0[units] = predict(lmsub, datasub0) + resi_sd * rnorm(length(units))
}
data$Y1[data$T == 1] = data$avggradefall2008[data$T == 1]
data$Y0[data$T == 0] = data$avggradefall2008[data$T == 0]
mean(data$Y1) - mean(data$Y0)
Y1 = data$Y1
Y0 = data$Y0
tau_true = mean(Y1-Y0)





##########################################################################
#### Rerandomize with two cases of covariates in design and analysis #####
##########################################################################

### numbers of treated and control units
n1 = sum(data$T)
n0 = sum(1-data$T)
n = nrow(data)
r1 = n1/n
r0 = n0/n
iter.max = 10^5

### complete randomization 
Z_cre = assign_CRE(n, m=n1, nperm=iter.max)

### case1: ReM with high school grade in design
case1 = list()
case1$X = as.matrix( data[,c("s_male", "s_hsgrade3")] )
case1$W = as.matrix( data[, c("s_mtongue_english",  
                              "s_test1correct", "s_test2correct", 
                              "s_mothercolabove", 
                              "s_fathercolabove", 
                              "s_gpapreviousyear")] )
case1$Z_rem = assign_ReM(n, m=n1, case1$X, pa=0.005, nperm=iter.max)

###case2: ReM with previous year gpa in design
case2 = list()
case2$X = as.matrix( data[,c("s_male", "s_gpapreviousyear")] )
case2$W = as.matrix( data[, c("s_mtongue_english",  
                              "s_test1correct", "s_test2correct", 
                              "s_mothercolabove", 
                              "s_fathercolabove", 
                              "s_hsgrade3")] )
case2$Z_rem = assign_ReM(n, m=n1, case2$X, pa=0.005, nperm=iter.max)


##########################################################################
##### Estimators with first case of covariates in design and analysis ####
##########################################################################

### difference in means under CRE with its asymptotic approximation 
tau_diff_cre = rep(NA, iter.max)
for(i in 1:iter.max){
  Y = Z_cre[i,] * Y1 + (1-Z_cre[i,]) * Y0
  tau_diff_cre[i] =  diff_est(Z_cre[i,], Y)
}

Vtt = var(Y1)/r1 + var(Y0)/r0 - var(Y1-Y0)

### adjusted estimator in case 1 with its asymptotic approximation 
case1$tau_diff_rem = rep(NA, iter.max)
case1$tau_adj_rem = rep(NA, iter.max)
case1$tau_adj_cre = rep(NA, iter.max)
for(i in 1:iter.max){
  Y = Z_cre[i,] * Y1 + (1-Z_cre[i,]) * Y0
  case1$tau_adj_cre[i] = reg_adj_est(Z_cre[i,], Y, case1$W)
}
for(i in 1:iter.max){
  Y = case1$Z_rem[i,] * Y1 + (1 - case1$Z_rem[i,]) * Y0
  case1$tau_diff_rem[i] = diff_est(case1$Z_rem[i,], Y)
  case1$tau_adj_rem[i] = reg_adj_est(case1$Z_rem[i,], Y, case1$W)
}

case1$R2tx = cal_R2(Y1, Y0, case1$X, r1, r0)
case1$Vttadj = var(lm(Y1~case1$W)$residuals)/r1 + var(lm(Y0~case1$W)$residuals)/r0 - var(lm(Y1~case1$W)$residuals - lm(Y0~case1$W)$residuals)
case1$R2txadj = cal_R2(lm(Y1~case1$W)$residuals, lm(Y0~case1$W)$residuals, case1$X, r1, r0)

### coefficients of epsiolon in the asymptotic distribution 
sqrt( Vtt * (1-case1$R2tx) ) # ReM diff
sqrt(Vtt) # CRE diff
sqrt( case1$Vttadj * (1 - case1$R2txadj) ) # ReM adj
sqrt( case1$Vttadj ) # CRE adj

### R2 for the covariates in design and analysis
case1$R2tx
cal_R2(Y1, Y0, case1$W, r1, r0)

### histogram
hist(sqrt(n)*(case1$tau_diff_rem - tau_true), freq = FALSE, ylim=c(0,0.045), breaks = 30, xlab = NA, main = NULL, xlim=c(-50,50), col="grey", border = FALSE)
title(xlab = expression(paste(sqrt(n), "(", hat(tau)-tau, ")", " or ", sqrt(n), "{", hat(tau), "(", hat(beta)[1], ",", hat(beta)[0], ")",-tau, "}")), line=3.6, cex.lab=2)
lines( density( sqrt(Vtt)*sample_std_rerand(10^7, R2=case1$R2tx, K=2, pa = 0.005) ), lty=2)
hist(sqrt(n)*(case1$tau_adj_rem - tau_true), freq = FALSE, breaks = 30, add = TRUE)
lines( density( sqrt(case1$Vttadj)*sample_std_rerand(10^7, R2=case1$R2txadj, K=2, pa = 0.005) ), lty=1)
legend("topright", legend = c("adjusted", "unadjusted", "adjusted", "unadjusted"), pch=c(0,15,NA,NA), pt.cex=3, col=c("black", "grey","black","black"),lty = c(0, 0, 1, 2), cex=2)


##########################################################################
#### Estimators with second case of covariates in design and analysis ####
##########################################################################

### adjusted estimator in case 2 with its asymptotic approximation 
case2$tau_diff_rem = rep(NA, iter.max)
case2$tau_adj_rem = rep(NA, iter.max)
case2$tau_adj_cre = rep(NA, iter.max)
for(i in 1:iter.max){
  Y = Z_cre[i,] * Y1 + (1-Z_cre[i,]) * Y0
  case2$tau_adj_cre[i] = reg_adj_est(Z_cre[i,], Y, case2$W)
}
for(i in 1:iter.max){
  Y = case2$Z_rem[i,] * Y1 + (1 - case2$Z_rem[i,]) * Y0
  case2$tau_diff_rem[i] = diff_est(case2$Z_rem[i,], Y)
  case2$tau_adj_rem[i] = reg_adj_est(case2$Z_rem[i,], Y, case2$W)
}

case2$R2tx = cal_R2(Y1, Y0, case2$X, r1, r0)
case2$Vttadj = var(lm(Y1~case2$W)$residuals)/r1 + var(lm(Y0~case2$W)$residuals)/r0 - var(lm(Y1~case2$W)$residuals - lm(Y0~case2$W)$residuals)
case2$R2txadj = cal_R2(lm(Y1~case2$W)$residuals, lm(Y0~case2$W)$residuals, case2$X, r1, r0)

### coefficients of epsiolon in the asymptotic distribution 
sqrt( Vtt * (1-case2$R2tx) ) # ReM diff
sqrt(Vtt) # CRE diff
sqrt( case2$Vttadj * (1 - case2$R2txadj) ) # ReM adj
sqrt( case2$Vttadj ) # CRE adj

### R2 for the covariates in design and analysis
case2$R2tx
cal_R2(Y1, Y0, case2$W, r1, r0)

### histogram
hist(sqrt(n)*(case2$tau_diff_rem - tau_true), freq = FALSE, ylim=c(0,0.045), breaks = 30, xlab = NA, main = NULL, xlim=c(-50,50), col="grey", border = FALSE)
title(xlab = expression(paste(sqrt(n), "(", hat(tau)-tau, ")", " or ", sqrt(n), "{", hat(tau), "(", hat(beta)[1], ",", hat(beta)[0], ")",-tau, "}")), line=3.6, cex.lab=2)
lines( density( sqrt(Vtt)*sample_std_rerand(10^7, R2=case2$R2tx, K=2, pa = 0.005) ), lty=2)
hist(sqrt(n)*(case2$tau_adj_rem - tau_true), freq = FALSE, breaks = 30, add = TRUE)
lines( density( sqrt(case2$Vttadj)*sample_std_rerand(10^7, R2=case2$R2txadj, K=2, pa = 0.005) ), lty=1)
legend("topright", legend = c("adjusted", "unadjusted", "adjusted", "unadjusted"), pch=c(0,15,NA,NA), pt.cex=3, col=c("black", "grey","black","black"),lty = c(0, 0, 1, 2), cex=2)







##########################################################################
##### Inference with first case of covariates in design and analysis #####
##########################################################################

### estimated standard deviations
case1$sd_est_diff_rem = rep(NA, iter.max)
case1$sd_est_adj_rem = rep(NA, iter.max)
case1$sd_est_diff_cre = rep(NA, iter.max)
case1$sd_est_adj_cre = rep(NA, iter.max)

for(i in 1:iter.max){
  Y = Z_cre[i,] * Y1 + (1-Z_cre[i,]) * Y0
  case1$sd_est_diff_cre[i] = sqrt( var_diff_est(Z_cre[i,], Y, case1$W) )
  case1$sd_est_adj_cre[i] = sqrt( var_adj_est(Z_cre[i,], Y, case1$W) )
  print(i)
}

for(i in 1:iter.max){
  Y = case1$Z_rem[i,] * Y1 + (1-case1$Z_rem[i,]) * Y0
  case1$sd_est_diff_rem[i] = sqrt( var_diff_est(case1$Z_rem[i,], Y, case1$W) )
  case1$sd_est_adj_rem[i] = sqrt(var_adj_est(case1$Z_rem[i,], Y, case1$W))
  print(i)
}

mean(case1$sd_est_diff_rem)
mean(case1$sd_est_diff_cre)
mean(case1$sd_est_adj_rem)
mean(case1$sd_est_adj_cre)

### coverage probabilities
mean( (case1$tau_diff_rem - qnorm(0.975) * case1$sd_est_diff_rem/sqrt(n)) * (case1$tau_diff_rem + qnorm(0.975) * case1$sd_est_diff_rem/sqrt(n)) < 0 )

mean( (tau_diff_cre - qnorm(0.975) * case1$sd_est_diff_cre/sqrt(n)) * (tau_diff_cre + qnorm(0.975) * case1$sd_est_diff_cre/sqrt(n)) < 0 )

mean( (case1$tau_adj_rem - qnorm(0.975) * case1$sd_est_adj_rem/sqrt(n)) * (case1$tau_adj_rem + qnorm(0.975) * case1$sd_est_adj_rem/sqrt(n)) < 0 )

mean( (case1$tau_adj_cre - qnorm(0.975) * case1$sd_est_adj_cre/sqrt(n)) * (case1$tau_adj_cre + qnorm(0.975) * case1$sd_est_adj_cre/sqrt(n)) < 0 )


##########################################################################
#### Inference with second case of covariates in design and analysis #####
##########################################################################

### estimated standard deviations
case2$sd_est_diff_rem = rep(NA, iter.max)
case2$sd_est_adj_rem = rep(NA, iter.max)
case2$sd_est_diff_cre = rep(NA, iter.max)
case2$sd_est_adj_cre = rep(NA, iter.max)

for(i in 1:iter.max){
  Y = Z_cre[i,] * Y1 + (1-Z_cre[i,]) * Y0
  case2$sd_est_diff_cre[i] = sqrt( var_diff_est(Z_cre[i,], Y, case2$W) )
  case2$sd_est_adj_cre[i] = sqrt( var_adj_est(Z_cre[i,], Y, case2$W) )
  print(i)
}

for(i in 1:iter.max){
  Y = case2$Z_rem[i,] * Y1 + (1-case2$Z_rem[i,]) * Y0
  case2$sd_est_diff_rem[i] = sqrt( var_diff_est(case2$Z_rem[i,], Y, case2$W) )
  case2$sd_est_adj_rem[i] = sqrt(var_adj_est(case2$Z_rem[i,], Y, case2$W))
  print(i)
}

mean(case2$sd_est_diff_rem)
mean(case2$sd_est_diff_cre)
mean(case2$sd_est_adj_rem)
mean(case2$sd_est_adj_cre)

### coverage probabilities
mean( (case2$tau_diff_rem - qnorm(0.975) * case2$sd_est_diff_rem/sqrt(n)) * (case2$tau_diff_rem + qnorm(0.975) * case2$sd_est_diff_rem/sqrt(n)) < 0 )

mean( (tau_diff_cre - qnorm(0.975) * case2$sd_est_diff_cre/sqrt(n)) * (tau_diff_cre + qnorm(0.975) * case2$sd_est_diff_cre/sqrt(n)) < 0 )

mean( (case2$tau_adj_rem - qnorm(0.975) * case2$sd_est_adj_rem/sqrt(n)) * (case2$tau_adj_rem + qnorm(0.975) * case2$sd_est_adj_rem/sqrt(n)) < 0 )

mean( (case2$tau_adj_cre - qnorm(0.975) * case2$sd_est_adj_cre/sqrt(n)) * (case2$tau_adj_cre + qnorm(0.975) * case2$sd_est_adj_cre/sqrt(n)) < 0 )