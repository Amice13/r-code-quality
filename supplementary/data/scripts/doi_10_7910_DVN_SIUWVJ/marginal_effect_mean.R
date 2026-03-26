### follow coefficients_table.R result
### use the model to generate table 2: calculate marginal effect at mean
library(mvtnorm)
NSIMS <- 10000

set.seed(02138)

### model 1: bayesglm model result
covariates1<-model.matrix(model1)
simulated.beta1 <- rmvnorm(NSIMS, mean = coef(model1), sigma = vcov(model1))

mean.covariates1 <- apply(covariates1, MARGIN = 2, mean)
mean.covariates1 <- as.matrix(mean.covariates1)
simulated.margin.mean1<-matrix(nrow=10000,ncol=402)
for (j in 1:10000){
  simulated.margin.mean1[j,] <- dnorm(t(mean.covariates1) %*% simulated.beta1[j,]) %*% simulated.beta1[j,]
}


margin.mean1 <-colMeans(simulated.margin.mean1)
margin.std1 <- sapply(as.data.frame(simulated.margin.mean1),function(x)sd(x))
margin.model1 <- cbind(margin.mean1,margin.std1)
colnames(margin.model1) <- c("coefficient","std. err.")
rownames(margin.model1) <- colnames(simulated.beta1)

options("scipen"=100, "digits"=3)
print(margin.model1[2:9,],digits=2)

### model 2: glm model result
covariates2<-model.matrix(model2)
simulated.beta2 <- rmvnorm(NSIMS, mean = coef(model2), sigma = vcov(model2))

mean.covariates2 <- apply(covariates2, MARGIN = 2, mean)
mean.covariates2 <- as.matrix(mean.covariates2)
simulated.margin.mean2<-matrix(nrow=10000,ncol=402)
for (j in 1:10000){
  simulated.margin.mean2[j,] <- dnorm(t(mean.covariates2) %*% simulated.beta2[j,]) %*% simulated.beta2[j,]
}


margin.mean2 <-colMeans(simulated.margin.mean2)
margin.std2 <- sapply(as.data.frame(simulated.margin.mean2),function(x)sd(x))
margin.model2 <- cbind(margin.mean2,margin.std2)
colnames(margin.model2) <- c("coefficient","std. err.")
rownames(margin.model2) <- colnames(simulated.beta2)

options("scipen"=100, "digits"=3)
print(margin.model2[2:9,],digits=2)

### mimic result from probitmfx for glm
### probitmfx is slow, we look at their source code and write a functon produce same results.
be2 <- as.matrix(coef(model2))
margin.model2.probitmfx.coef <- as.vector(dnorm(t(mean.covariates2) %*% be2) %*% as.vector(be2))

fxb2 <- dnorm(t(mean.covariates2) %*% be2)
gr2 <- as.numeric(fxb2)*(diag(length(coef(model2))) - as.numeric(t(mean.covariates2) %*% be2) *(be2 %*% t(mean.covariates2)))
margin.model2.probitmfx.std <- as.vector(sqrt(diag(gr2 %*%  vcov(model2) %*% t(gr2))))      

margin.model2.probitmfx <-  cbind(margin.model2.probitmfx.coef,margin.model2.probitmfx.std)
rownames(margin.model2.probitmfx) <- colnames(simulated.beta2)
colnames(margin.model2.probitmfx) <- c("coefficient","std. err.")

options("scipen"=100, "digits"=3)
print(margin.model2.probitmfx[2:9,],digits=2)

### model 3: glm coefficients using STATA obeservations
covariates3<-model.matrix(model3)
simulated.beta3 <- rmvnorm(NSIMS, mean = coef(model3), sigma = vcov(model3))

mean.covariates3 <- apply(covariates3, MARGIN = 2, mean)
mean.covariates3 <- as.matrix(mean.covariates3)
simulated.margin.mean3<-matrix(nrow=10000,ncol=225)
for (j in 1:10000){
  simulated.margin.mean3[j,] <- dnorm(t(mean.covariates3) %*% simulated.beta3[j,]) %*% simulated.beta3[j,]
}


margin.mean3 <-colMeans(simulated.margin.mean3)
margin.std3 <- sapply(as.data.frame(simulated.margin.mean3),function(x)sd(x))
margin.model3 <- cbind(margin.mean3,margin.std3)
colnames(margin.model3) <- c("coefficient","std. err.")
rownames(margin.model3) <- colnames(simulated.beta3)

options("scipen"=100, "digits"=4)
print(margin.model3[2:9,],digits=2)

covariates3<-model.matrix(model3)
simulated.beta3 <- rmvnorm(NSIMS, mean = coef(model3), sigma = vcov(model3))

mean.covariates3 <- apply(covariates3, MARGIN = 2, mean)
mean.covariates3 <- as.matrix(mean.covariates3)
simulated.margin.mean3<-matrix(nrow=10000,ncol=225)
for (j in 1:10000){
  simulated.margin.mean3[j,] <- dnorm(t(mean.covariates3) %*% simulated.beta3[j,]) %*% simulated.beta3[j,]
}


margin.mean3 <-colMeans(simulated.margin.mean3)
margin.std3 <- sapply(as.data.frame(simulated.margin.mean3),function(x)sd(x))
margin.model3 <- cbind(margin.mean3,margin.std3)
colnames(margin.model3) <- c("coefficient","std. err.")
rownames(margin.model3) <- colnames(simulated.beta3)

options("scipen"=100, "digits"=4)
print(margin.model3[2:9,],digits=2)

### mimic result from probitmfx for glm with Stata observations
### probitmfx is slow, we look at their source code and write a functon produce same results.
be3 <- as.matrix(coef(model3))
margin.model3.probitmfx.coef <- as.vector(dnorm(t(mean.covariates3) %*% be3) %*% as.vector(be3))

fxb3 <- dnorm(t(mean.covariates3) %*% be3)
gr3 <- as.numeric(fxb3)*(diag(length(coef(model3))) - as.numeric(t(mean.covariates3) %*% be3) *(be3 %*% t(mean.covariates3)))
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
output.latex <- cbind(output3,output1,output2,output2.probitmfx,output3,output3.probitmfx)

library(xtable)
print(xtable(output.latex),type="latex")
