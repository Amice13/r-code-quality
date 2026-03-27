library(caret)
library(glmnet)
library(caret)
library(ROCR)
library(haven)
library(tidyverse)
library(MASS)
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
setwd("")
#DWAD
CTX <- read.csv("Njunge_CTX_15092020.csv", header = T, sep = ",")
cl <- CTX[,c(1,	458,	2, 3,	35,	15,	455,	449,	192,	227:414, 440,	441,	456,	193:220)] #1, 452, 487:674, 701, 716......717, 2, 106, 54, 715, 713, 452, 487:674, 701, 716, 458:486, 1, 717, 2, 106, 54, 715, 713, 716, 701, 450:480, 487:674
str(cl)
cl=data.frame(cl)
cl <- cl[!is.na(CTX$DWAD),]
cl <- cl[,-1] #remove subjid
site <- data.frame(model.matrix( ~ cl$site1 - 1))
cld <- cbind(cl[,c(1:4,6)], site)                
#cld[,3:4] <- lapply(cld[,3:4], function(x) as.numeric(x)-1)
str(cld)
cl <- cbind(cld, cl[,-c(1:6)])
cl[is.na(cl)]=0
x <- data.matrix(cl[,-1])
y <- data.matrix(cl[,1])
p.fac = rep(1, 227)
p.fac[c(1:8)] = 0
glmnet_case <- cv.glmnet(x,y, family = "gaussian" , alpha = 0.5, penalty.factor = p.fac, type.measure = "mse", nfolds=10, standardize = TRUE)
ridge_case <- glmnet(x,y, family="gaussian", alpha = 0.5, penalty.factor = p.fac, lambda= glmnet_case$lambda.min, standardize = TRUE)
plot(glmnet_case)
glmnet_case$lambda.min
glmnet_case$lambda.1se
coeficients <- coef(glmnet_case, s=glmnet_case$lambda.min)
myCoefs <- coef(ridge_case, s="lambda.min");
myCoefs[which(myCoefs != 0 ) ] #coefficients: intercept included
myCoefs@Dimnames[[1]][which(myCoefs != 0 ) ] #feature names: intercept included
myResults <- data.frame(features = myCoefs@Dimnames[[1]][ which(myCoefs != 0 ) ], coefs = myCoefs[which(myCoefs != 0 ) ])  #intercept included
write.csv(myResults,file="Njunge_EN_DWAD.csv")
#View(myResults)
#predict(ridge_case, newx=x, s="lambda.min")
train.pred = (predict(ridge_case, newx=x, glmnet_case$lambda.min))
train.cor = as.data.frame(cbind(train.pred,cl[,1]) )
t.cor = cor.test(train.cor[,1], train.cor[,2], method="pearson")
t.cor
plot(train.pred,cl[,1])

###bootstrapping

vboot.elnet <- function(fit, x, y, s, penalty.factor = p.fac, nfolds = 10, B = 1000, cv_replicates = 10, lambda = TRUE, n_cores = max(1, parallel::detectCores() - 1)){
  # lambda to be chosen in CV
  what.lambda <- "lambda.min"
  if(lambda)  what.lambda <- "lambda.1se"
  
  orig_predict <- glmnet::predict.glmnet(fit, newx = x, s=s, type = "response")
  orig_r2 <- 1 - (var(orig_predict - y) / var(y))
  #orig_RMSE <- sqrt(mean((orig_predict - y)^2))
  
  # Create index to bootstrap
  bootstrap <- function(x, y, alpha = fit$call$alpha, nfolds = nfolds, B = B){
    index <- sample(1:nrow(x), replace = TRUE)
    xboot <- x[index, ]
    yboot <- y[index]
    
    #penalty.factor
    p.fac = rep(1, 227)
    p.fac[c(1:8)] = 0
    
    # Fit the model using bootstrap dataset
    cv.glmnet_b <- pbapply::pbreplicate(cv_replicates, as.numeric(glmnet::cv.glmnet(xboot, yboot, alpha = fit$call$alpha, family = "gaussian", nfolds = nfolds)[what.lambda]))
    l <- median(cv.glmnet_b)
    boot_fit <- glmnet::glmnet(xboot, yboot, penalty.factor = p.fac, alpha = fit$call$alpha, family = "gaussian")
    boot_predict <- glmnet::predict.glmnet(boot_fit, newx = xboot, s = l, type = "response")
    Cb_boot <- 1 - (var(boot_predict - yboot) / var(yboot))
    
    #selected variables
    var_sel <- tryCatch(names(coef(boot_fit, s = l)[coef(boot_fit, s = l)[,1] != 0,])[-1], error = function(e) NA)
    
    # fit bootstrap model to the original dataset
    bootorig_predict <-  glmnet::predict.glmnet(boot_fit, newx = x, s = l, type = "response")
    Cb_orig <- 1 - (var(bootorig_predict - y) / var(y))
    
    
    return(list(Cb_boot = Cb_boot, Cb_orig = Cb_orig, var_sel = var_sel))
  }
  if( n_cores > 1){
    cl <- parallel::makeCluster(n_cores)
    parallel::clusterExport(cl, varlist = c("B", "x", "y", "fit", "nfolds", "bootstrap"), envir = environment())
    CBOOT <- parallel::parSapply(cl, 1:B, function(i) bootstrap(x, y, alpha = fit$call$alpha, nfolds = nfolds))
    parallel::stopCluster(cl)
    closeAllConnections()
  }
  else{
    CBOOT <- suppressWarnings(pbapply::pbreplicate(B, bootstrap(x, y, alpha = fit$call$alpha, nfolds = nfolds)))
  }
  # Optimist
  Optimisms <- as.numeric(stats::na.omit(unlist(CBOOT[1, ])) - stats::na.omit(unlist(CBOOT[2, ])))
  eff_B <- length( stats::na.omit(unlist(CBOOT[2, ])))
  O <- B^-1 * sum(unlist(CBOOT[1,]) - unlist(CBOOT[2, ]))
  # Adjusted Optimist
  Oadj <- as.numeric(orig_r2 - O)
  
  #output
  output <- list(round(data.frame(Original_R2 = as.numeric(orig_r2), Optimism = O, Validated_R2 = Oadj),3), varImportance = CBOOT[3,],
                 Effective_Bootstraps = eff_B, Optimisms = Optimisms)
  class(output) <- "bootVal"
  return(output)
  
}


booty = vboot.elnet(ridge_case, x, y, s = glmnet_case$lambda.min, nfolds = 10, B = 1000,
                    cv_replicates = 10, n_cores = max(1, parallel::detectCores() - 1))
booty[1]

features <- unlist(booty$varImportance)
feat.table = as.data.frame(table(features))
feat.table$inclusion = feat.table$Freq/1000
feat.table
write.csv(feat.table, "Njunge_bootstrap_features_DWAD.csv")



#DMAD
CTX <- read.csv("Njunge_CTX_15092020.csv", header = T, sep = ",")
cl <- CTX[,c(1,	457,	2,	35,	15,	455,	453,	192,	227:414, 441,	456,	193:220)] #, 458:486, , 487:674, , 452, 487:674, 701, 716, (proteomics 1, 717, 2, 106, 54, 715, 713, 452, 487:674, 701, 716)
str(cl)
cl=data.frame(cl)
cl <- cl[!is.na(CTX$DMAD),]
cl <- cl[,-1] #remove subjid
site <- data.frame(model.matrix( ~ cl$site1 - 1))
cld <- cbind(cl[,c(1:4,6)], site)                
str(cld)
cl <- cbind(cld, cl[,-c(1:6)])
cl[is.na(cl)]=0
x <- data.matrix(cl[,-1])
y <- data.matrix(cl[,1])
p.fac = rep(1, 227)
p.fac[c(1:8)] = 0
glmnet_case <- cv.glmnet(x,y, family = "gaussian" , alpha = 0.5, penalty.factor = p.fac, type.measure = "mse", nfolds=10, standardize = TRUE)
ridge_case <- glmnet(x,y, family="gaussian", alpha = 0.5, penalty.factor = p.fac, lambda= glmnet_case$lambda.min, standardize = TRUE)
plot(glmnet_case)
glmnet_case$lambda.min
glmnet_case$lambda.1se
coeficients <- coef(glmnet_case, s=glmnet_case$lambda.min)
myCoefs <- coef(ridge_case, s="lambda.min");
myCoefs[which(myCoefs != 0 ) ] #coefficients: intercept included
myCoefs@Dimnames[[1]][which(myCoefs != 0 ) ] #feature names: intercept included
myResults <- data.frame(features = myCoefs@Dimnames[[1]][ which(myCoefs != 0 ) ], coefs = myCoefs[which(myCoefs != 0 ) ])  #intercept included
write.csv(myResults,file="Njunge_EN_DMAD.csv")
View(myResults)
#predict(ridge_case, newx=x, s="lambda.min")
train.pred = (predict(ridge_case, newx=x, glmnet_case$lambda.min))
train.cor = as.data.frame(cbind(train.pred,cl[,1]) )
t.cor = cor.test(train.cor[,1], train.cor[,2], method="pearson")
t.cor
plot(train.pred,cl[,1])
###bootstrapping
vboot.elnet <- function(fit, x, y, s, penalty.factor = p.fac, nfolds = 10, B = 1000, cv_replicates = 10, lambda = TRUE, n_cores = max(1, parallel::detectCores() - 1)){
  # lambda to be chosen in CV
  what.lambda <- "lambda.min"
  if(lambda)  what.lambda <- "lambda.1se"
  orig_predict <- glmnet::predict.glmnet(fit, newx = x, s=s, type = "response")
  orig_r2 <- 1 - (var(orig_predict - y) / var(y))
   # Create index to bootstrap
  bootstrap <- function(x, y, alpha = fit$call$alpha, nfolds = nfolds, B = B){
    index <- sample(1:nrow(x), replace = TRUE)
    xboot <- x[index, ]
    yboot <- y[index]
    #penalty.factor
    p.fac = rep(1, 227)
    p.fac[c(1:8)] = 0
    # Fit the model using bootstrap dataset
    cv.glmnet_b <- pbapply::pbreplicate(cv_replicates, as.numeric(glmnet::cv.glmnet(xboot, yboot, alpha = fit$call$alpha, family = "gaussian", nfolds = nfolds)[what.lambda]))
    l <- median(cv.glmnet_b)
    boot_fit <- glmnet::glmnet(xboot, yboot, penalty.factor = p.fac, alpha = fit$call$alpha, family = "gaussian")
    boot_predict <- glmnet::predict.glmnet(boot_fit, newx = xboot, s = l, type = "response")
    Cb_boot <- 1 - (var(boot_predict - yboot) / var(yboot))
    #selected variables
    var_sel <- tryCatch(names(coef(boot_fit, s = l)[coef(boot_fit, s = l)[,1] != 0,])[-1], error = function(e) NA)
    # fit bootstrap model to the original dataset
    bootorig_predict <-  glmnet::predict.glmnet(boot_fit, newx = x, s = l, type = "response")
    Cb_orig <- 1 - (var(bootorig_predict - y) / var(y))
    return(list(Cb_boot = Cb_boot, Cb_orig = Cb_orig, var_sel = var_sel))
  }
  if( n_cores > 1){
    cl <- parallel::makeCluster(n_cores)
    parallel::clusterExport(cl, varlist = c("B", "x", "y", "fit", "nfolds", "bootstrap"), envir = environment())
    CBOOT <- parallel::parSapply(cl, 1:B, function(i) bootstrap(x, y, alpha = fit$call$alpha, nfolds = nfolds))
    parallel::stopCluster(cl)
    closeAllConnections()
  }
  else{
    CBOOT <- suppressWarnings(pbapply::pbreplicate(B, bootstrap(x, y, alpha = fit$call$alpha, nfolds = nfolds)))
  }
  # Optimist
  Optimisms <- as.numeric(stats::na.omit(unlist(CBOOT[1, ])) - stats::na.omit(unlist(CBOOT[2, ])))
  eff_B <- length( stats::na.omit(unlist(CBOOT[2, ])))
  O <- B^-1 * sum(unlist(CBOOT[1,]) - unlist(CBOOT[2, ]))
  # Adjusted Optimist
  Oadj <- as.numeric(orig_r2 - O)
  #output
  output <- list(round(data.frame(Original_R2 = as.numeric(orig_r2), Optimism = O, Validated_R2 = Oadj),3), varImportance = CBOOT[3,],
                 Effective_Bootstraps = eff_B, Optimisms = Optimisms)
  class(output) <- "bootVal"
  return(output)
}
booty = vboot.elnet(ridge_case, x, y, s = glmnet_case$lambda.min, nfolds = 10, B = 1000,
                    cv_replicates = 10, n_cores = max(1, parallel::detectCores() - 1))
booty[1]

features <- unlist(booty$varImportance)
feat.table = as.data.frame(table(features))
feat.table$inclusion = feat.table$Freq/1000
feat.table
#write.csv(feat.table, "Njunge_bootstrap_features_DMAD.csv")

