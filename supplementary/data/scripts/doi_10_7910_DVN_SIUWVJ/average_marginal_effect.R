### follow coefficients_table.R result
### use the model to generate table 4: calculate average marginal effect
library(mvtnorm)
NSIMS <- 1000

set.seed(02138)

### model 1: bayesglm model result
covariates1<-model.matrix(model1)
simulated.beta1 <- rmvnorm(NSIMS, mean = coef(model1), sigma = vcov(model1))

### from now, we only calculate the AME for the eight variables of interest to increase computing speed;
### also, because our dataset is really large, we only do 1000 simulations from beta distribution
margin.model1 <- matrix(ncol= 2,nrow = 8)
for (j in 1:8){
  covariates1.AME <- data.frame(covariates1)
  covariates1.AME[,j+1] <- mean(covariates1.AME[,j+1])
  covariates1.AME <- as.matrix(covariates1.AME)
  simulated.margin.AME<-rep(NA,1000)
  for (i in 1:1000){
    simulated.margin.AME[i]<-mean(dnorm(covariates1.AME %*% simulated.beta1[i,]) %*% simulated.beta1[i,j+1])
  }
  margin.model1[j,1]<-mean(simulated.margin.AME)
  margin.model1[j,2]<-sd(simulated.margin.AME)
}
rownames(margin.model1) <- colnames(simulated.beta1)[2:9]
colnames(margin.model1) <- c("coefficients", "std. error")
print(margin.model1)


covariates2<-model.matrix(model2)
simulated.beta2 <- rmvnorm(NSIMS, mean = coef(model2), sigma = vcov(model2))

### model 2: glm model result
margin.model2 <- matrix(ncol= 2,nrow = 8)
for (j in 1:8){
  covariates2.AME <- data.frame(covariates2)
  covariates2.AME[,j+1] <- mean(covariates2.AME[,j+1])
  covariates2.AME <- as.matrix(covariates2.AME)
  simulated.margin.AME<-rep(NA,1000)
  for (i in 1:1000){
    simulated.margin.AME[i]<-mean(dnorm(covariates2.AME %*% simulated.beta2[i,]) %*% simulated.beta2[i,j+1])
  }
  margin.model2[j,1]<-mean(simulated.margin.AME)
  margin.model2[j,2]<-sd(simulated.margin.AME)
}
rownames(margin.model2) <- colnames(simulated.beta2)[2:9]
colnames(margin.model2) <- c("coefficients", "std. error")
print(margin.model2)

### we didn't do probitmfx for this specification because 
### if we run it directly, reports "Error: vector memory exhausted (limit reached?)".
### margin.model2.probitmfx <- probitmfx(formula = fdece ~lndis+lnasset+ROS+importance+Dfsoe+prov_gdpper+
###                            prov_SOE+unemployment+govdummy1+yeardummy+ind2dummy, data=dat, atmean = FALSE)


### model 3: glm coefficients using STATA obeservations
covariates3<-model.matrix(model3)
simulated.beta3 <- rmvnorm(NSIMS, mean = coef(model3), sigma = vcov(model3))
margin.model3 <- matrix(ncol= 2,nrow = 8)
for (j in 1:8){
  covariates3.AME <- data.frame(covariates3)
  covariates3.AME[,j+1] <- mean(covariates3.AME[,j+1])
  covariates3.AME <- as.matrix(covariates3.AME)
  simulated.margin.AME<-rep(NA,1000)
  for (i in 1:1000){
    simulated.margin.AME[i]<-mean(dnorm(covariates3.AME %*% simulated.beta3[i,]) %*% simulated.beta3[i,j+1])
  }
  margin.model3[j,1]<-mean(simulated.margin.AME)
  margin.model3[j,2]<-sd(simulated.margin.AME)
}
rownames(margin.model3) <- colnames(simulated.beta3)[2:9]
colnames(margin.model3) <- c("coefficients", "std. error")
print(margin.model3)

### produce latex table output
margin.model1.output <- data.frame(margin.model1[1:8,])
margin.model1.output <- round(margin.model1.output, 4)
output1 <- matrix(as.vector(rbind(as.character(margin.model1.output$coefficients),paste("(",as.vector(margin.model1.output$std..error),")", sep=""))), nrow=16)
output1  <- data.frame(output1)
rownames(output1)[c(1,3,5,7,9,11,13,15)] <- c("Distance_lag", "log(firm asset)_lag","ROS_lag", "Firm importance_lag",
                                              "Fully state-owned_lag","GDP per capita_lag","State sector share_lag","unemployment rate_lag")

margin.model2.output <- data.frame(margin.model2[1:8,])
margin.model2.output <- round(margin.model2.output, 4)
output2 <- matrix(as.vector(rbind(as.character(margin.model2.output$coefficients),paste("(",as.vector(margin.model2.output$std..error),")", sep=""))), nrow=16)
output2  <- data.frame(output2)
rownames(output2)[c(1,3,5,7,9,11,13,15)] <- c("Distance_lag", "log(firm asset)_lag","ROS_lag", "Firm importance_lag",
                                              "Fully state-owned_lag","GDP per capita_lag","State sector share_lag","unemployment rate_lag")

margin.model3.output <- data.frame(margin.model3[1:8,])
margin.model3.output <- round(margin.model3.output, 4)
output3 <- matrix(as.vector(rbind(as.character(margin.model3.output$coefficients),paste("(",as.vector(margin.model3.output$std..error),")", sep=""))), nrow=16)
output3  <- data.frame(output3)
rownames(output3)[c(1,3,5,7,9,11,13,15)] <- c("Distance_lag", "log(firm asset)_lag","ROS_lag", "Firm importance_lag",
                                              "Fully state-owned_lag","GDP per capita_lag","State sector share_lag","unemployment rate_lag")

### the first column, model 2 is used to reserve a column for the STATA output. 
### we manually fixed the coefficients value for STATA output in paper.
output.latex <- cbind(output3,output1,output2,output3)

library(xtable)
print(xtable(output.latex),type="latex")



