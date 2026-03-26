### follow coefficients_table.R result
### use the model to generate table 3: calculate marginal effect at median
library(mvtnorm)
NSIMS <- 10000

set.seed(02138)

### model 1: bayesglm model result
covariates1<-model.matrix(model1)
simulated.beta1 <- rmvnorm(NSIMS, mean = coef(model1), sigma = vcov(model1))

median.covariates1 <- apply(covariates1, MARGIN = 2, median)
median.covariates1 <- as.matrix(median.covariates1)
simulated.margin.median1<-matrix(nrow=10000,ncol=402)
for (j in 1:10000){
  simulated.margin.median1[j,] <- dnorm(t(median.covariates1) %*% simulated.beta1[j,]) %*% simulated.beta1[j,]
}


margin.median1 <-colMeans(simulated.margin.median1)
margin.std1 <- sapply(as.data.frame(simulated.margin.median1),function(x)sd(x))
margin.model1 <- cbind(margin.median1,margin.std1)
colnames(margin.model1) <- c("coefficient","std. err.")
rownames(margin.model1) <- colnames(simulated.beta1)

options("scipen"=100, "digits"=3)
print(margin.model1[2:9,],digits=2)

### model 2: glm model result
covariates2<-model.matrix(model2)
simulated.beta2 <- rmvnorm(NSIMS, mean = coef(model2), sigma = vcov(model2))

median.covariates2 <- apply(covariates2, MARGIN = 2, median)
median.covariates2 <- as.matrix(median.covariates2)
simulated.margin.median2<-matrix(nrow=10000,ncol=402)
for (j in 1:10000){
  simulated.margin.median2[j,] <- dnorm(t(median.covariates2) %*% simulated.beta2[j,]) %*% simulated.beta2[j,]
}


margin.median2 <-colMeans(simulated.margin.median2)
margin.std2 <- sapply(as.data.frame(simulated.margin.median2),function(x)sd(x))
margin.model2 <- cbind(margin.median2,margin.std2)
colnames(margin.model2) <- c("coefficient","std. err.")
rownames(margin.model2) <- colnames(simulated.beta2)

options("scipen"=100, "digits"=3)
print(margin.model2[2:9,],digits=2)

### mimic result from probitmfx
### the probitmfx doesn't produce an option for setting values at mean
### Hence, we tried to mimic the probitmfx result by writing a function
be2 <- as.matrix(coef(model2))
margin.model2.probitmfx.coef <- as.vector(dnorm(t(median.covariates2) %*% be2) %*% as.vector(be2))

fxb2 <- dnorm(t(median.covariates2) %*% be2)
gr2 <- as.numeric(fxb2)*(diag(length(coef(model2))) - as.numeric(t(median.covariates2) %*% be2) *(be2 %*% t(median.covariates2)))
margin.model2.probitmfx.std <- as.vector(sqrt(diag(gr2 %*%  vcov(model2) %*% t(gr2))))      

margin.model2.probitmfx <-  cbind(margin.model2.probitmfx.coef,margin.model2.probitmfx.std)
rownames(margin.model2.probitmfx) <- colnames(simulated.beta2)
colnames(margin.model2.probitmfx) <- c("coefficient","std. err.")

options("scipen"=100, "digits"=3)
print(margin.model2.probitmfx[2:9,],digits=2)

### model 3: glm coefficients using STATA obeservations
covariates3<-model.matrix(model3)
simulated.beta3 <- rmvnorm(NSIMS, mean = coef(model3), sigma = vcov(model3))

median.covariates3 <- apply(covariates3, MARGIN = 2, median)
median.covariates3 <- as.matrix(median.covariates3)
simulated.margin.median3<-matrix(nrow=10000,ncol=225)
for (j in 1:10000){
  simulated.margin.median3[j,] <- dnorm(t(median.covariates3) %*% simulated.beta3[j,]) %*% simulated.beta3[j,]
}


margin.median3 <-colMeans(simulated.margin.median3)
margin.std3 <- sapply(as.data.frame(simulated.margin.median3),function(x)sd(x))
margin.model3 <- cbind(margin.median3,margin.std3)
colnames(margin.model3) <- c("coefficient","std. err.")
rownames(margin.model3) <- colnames(simulated.beta3)

options("scipen"=100, "digits"=4)
print(margin.model3[2:9,],digits=2)

covariates3<-model.matrix(model3)
simulated.beta3 <- rmvnorm(NSIMS, mean = coef(model3), sigma = vcov(model3))

median.covariates3 <- apply(covariates3, MARGIN = 2, median)
median.covariates3 <- as.matrix(median.covariates3)
simulated.margin.median3<-matrix(nrow=10000,ncol=225)
for (j in 1:10000){
  simulated.margin.median3[j,] <- dnorm(t(median.covariates3) %*% simulated.beta3[j,]) %*% simulated.beta3[j,]
}


margin.median3 <-colMeans(simulated.margin.median3)
margin.std3 <- sapply(as.data.frame(simulated.margin.median3),function(x)sd(x))
margin.model3 <- cbind(margin.median3,margin.std3)
colnames(margin.model3) <- c("coefficient","std. err.")
rownames(margin.model3) <- colnames(simulated.beta3)

options("scipen"=100, "digits"=4)
print(margin.model3[2:9,],digits=2)

### mimic result from probitmfx
### the probitmfx doesn't produce an option for setting values at mean
### Hence, we tried to mimic the probitmfx result by writing a function
be3 <- as.matrix(coef(model3))
margin.model3.probitmfx.coef <- as.vector(dnorm(t(median.covariates3) %*% be3) %*% as.vector(be3))

fxb3 <- dnorm(t(median.covariates3) %*% be3)
gr3 <- as.numeric(fxb3)*(diag(length(coef(model3))) - as.numeric(t(median.covariates3) %*% be3) *(be3 %*% t(median.covariates3)))
margin.model3.probitmfx.std <- as.vector(sqrt(diag(gr3 %*%  vcov(model3) %*% t(gr3))))      

margin.model3.probitmfx <-  cbind(margin.model3.probitmfx.coef,margin.model3.probitmfx.std)
rownames(margin.model3.probitmfx) <- colnames(simulated.beta3)
colnames(margin.model3.probitmfx) <- c("coefficient","std. err.")

options("scipen"=100, "digits"=3)
print(margin.model3.probitmfx[2:9,],digits=2)


### produce latex table output
margin.model1.output <- data.frame(margin.model1[2:9,])
margin.model1.output <- round(margin.model1.output, 4)
output1 <- matrix(as.vector(rbind(as.character(margin.model1.output$coefficient),paste("(",as.vector(margin.model1.output$std..err.),")", sep=""))), nrow=16)
output1  <- data.frame(output1)
rownames(output1)[c(1,3,5,7,9,11,13,15)] <- c("Distance_lag", "log(firm asset)_lag","ROS_lag", "Firm importance_lag",
                                              "Fully state-owned_lag","GDP per capita_lag","State sector share_lag","unemployment rate_lag")

margin.model2.output <- data.frame(margin.model2[2:9,])
margin.model2.output <- round(margin.model2.output, 4)
output2 <- matrix(as.vector(rbind(as.character(margin.model2.output$coefficient),paste("(",as.vector(margin.model2.output$std..err.),")", sep=""))), nrow=16)
output2  <- data.frame(output2)
rownames(output2)[c(1,3,5,7,9,11,13,15)] <- c("Distance_lag", "log(firm asset)_lag","ROS_lag", "Firm importance_lag",
                                              "Fully state-owned_lag","GDP per capita_lag","State sector share_lag","unemployment rate_lag")

margin.model2.probitmfx.output <- data.frame(margin.model2.probitmfx[2:9,])
margin.model2.probitmfx.output <- round(margin.model2.probitmfx.output, 4)
output2.probitmfx <- matrix(as.vector(rbind(as.character(margin.model2.probitmfx.output$coefficient),paste("(",as.vector(margin.model2.probitmfx.output$std..err.),")", sep=""))), nrow=16)
output2.probitmfx  <- data.frame(output2.probitmfx)
rownames(output2.probitmfx)[c(1,3,5,7,9,11,13,15)] <- c("Distance_lag", "log(firm asset)_lag","ROS_lag", "Firm importance_lag",
                                                        "Fully state-owned_lag","GDP per capita_lag","State sector share_lag","unemployment rate_lag")


margin.model3.output <- data.frame(margin.model3[2:9,])
margin.model3.output <- round(margin.model3.output, 4)
output3 <- matrix(as.vector(rbind(as.character(margin.model3.output$coefficient),paste("(",as.vector(margin.model3.output$std..err.),")", sep=""))), nrow=16)
output3  <- data.frame(output3)
rownames(output3)[c(1,3,5,7,9,11,13,15)] <- c("Distance_lag", "log(firm asset)_lag","ROS_lag", "Firm importance_lag",
                                              "Fully state-owned_lag","GDP per capita_lag","State sector share_lag","unemployment rate_lag")

margin.model3.probitmfx.output <- data.frame(margin.model3.probitmfx[2:9,])
margin.model3.probitmfx.output <- round(margin.model3.probitmfx.output, 4)
output3.probitmfx <- matrix(as.vector(rbind(as.character(margin.model3.probitmfx.output$coefficient),paste("(",as.vector(margin.model3.probitmfx.output$std..err.),")", sep=""))), nrow=16)
output3.probitmfx  <- data.frame(output3.probitmfx)
rownames(output3.probitmfx)[c(1,3,5,7,9,11,13,15)] <- c("Distance_lag", "log(firm asset)_lag","ROS_lag", "Firm importance_lag",
                                                        "Fully state-owned_lag","GDP per capita_lag","State sector share_lag","unemployment rate_lag")

### the first column, model 2 is used to reserve a column for the STATA output. 
### we manually fixed the coefficients value for STATA output in paper.
output.latex <- cbind(output3.probitmfx,output1,output2,output2.probitmfx,output3,output3.probitmfx)

library(xtable)
print(xtable(output.latex),type="latex")

