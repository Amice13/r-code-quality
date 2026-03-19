
options(warn=-1)
library(MASS)
library(numDeriv)
###################
## AK1 Estimator
###################
AK1logLik <- function(para, mydata, z) {
  n = nrow(mydata);
  tauhat <- para[1];
  betap <- para[2];
  Coeff <- as.matrix(para[3:length(para)])
  if(ncol(mydata)>3){
    err <- mydata[,2] - as.matrix(mydata[,c(4:ncol(mydata))])%*%Coeff;
    esthat <- as.matrix(mydata[,c(4:ncol(mydata))])%*%Coeff;
  }else{
    err <- mydata[,2] - Coeff;
    esthat <- Coeff;
  }
  
  se <- mydata[,3];  
  t <- mydata[,2]/mydata[,3]
  
  cutoffs <- c(-1.96, 1.96)
  phat <- (abs(t)<1.96)*betap + (abs(t)>=1.96)*1
  
  meanbeta=rep(0,n);
  for (i in 1:n) {
    prob_mid <- (pnorm((cutoffs[2]*se[i]-esthat[i])/sqrt((tauhat) +se[i]^2))
                 -pnorm((cutoffs[1]*se[i]-esthat[i])/sqrt((tauhat) +se[i]^2)));
    prob_ex <- 1-prob_mid;
    meanbeta[i] <- betap*prob_mid + 1*prob_ex;
  }
  
  fX = dnorm(err, 0, sqrt(se^2 + tauhat));
  L <- (phat/meanbeta)*fX;
  logL <- log(L);
  LLH <- -sum(log(L));
  if(tauhat<0 | betap<0){ LLH <- 10^10; }
  if(z=="est"){return(LLH);}else{return(logL);}
}
AK1logLik_LLH <- function(para) {
  return(AK1logLik(para, AKdata, "est"))
}

AK1.est <-function(AKdata) {
  subAKdata <- AKdata[, c(2, 4:ncol(AKdata))]
  #InitialValue<-as.numeric(summary(lm(effect~.-1, data=subAKdata, weights=1/AKdata$se^2))$coefficients[,1])
  InitialValue<-as.numeric(mean(AKdata$effect))
  t2 <- mean(AKdata$se^2)/4
  Result <- tryCatch(
    {
      nlminb(AK1logLik_LLH, start=c(t2,0,InitialValue),lower=c(0, 0.01, rep(-Inf,length(InitialValue))),upper=c(Inf, Inf, rep(Inf,length(InitialValue))), hessian=TRUE, control=list(iter.max=1000, abs.tol=10^(-20), eval.max=1000))      },
      error=function(cond){
          return(list(Error = 'error',
                      convergence = 1,
                      par = c(rep(NA, ncol(AKdata)-1)),
                      EstResults=as.data.frame(matrix(NaN, nrow=5, ncol=5))))
    }
  )

    AKEstimationResults <- as.data.frame(matrix(NA, nrow=10, ncol=4))
    colnames(AKEstimationResults) <- c("method","term","variable","value")
    AKEstimationResults$method<-"AK1"
    AKEstimationResults$term<-c(rep("b0",6),"rho1","rho1","tau2","tau2")
    AKEstimationResults$variable <- c("estimate", "std.error", "statistic", "p.value","conf.low", "conf.high","estimate","std.error","estimate","std.error")

  if(Result$convergence==1){

   AKEstimationResults$value <- NA
    return(AKEstimationResults)
  }else{
    EstCoefficients <- t(as.matrix(Result$par))
    colnames(EstCoefficients) <-  c("tau2", "Betap", colnames(AKdata)[4:ncol(AKdata)])
    StdErrors <- diag(sqrt(ginv(hessian(AK1logLik_LLH, Result$par), tol=10^(-30))))
    PValue <- dt(EstCoefficients/StdErrors, df=nrow(AKdata)-length(EstCoefficients))
    EstResults <- rbind(EstCoefficients, StdErrors, PValue)
    rownames(EstResults) <- c("Coefficients", "Std.Err", "p-value")
    crit <- qt(0.975,df=(nrow(AKdata)-length(EstCoefficients)))    


   AKEstimationResults$value <- c(EstResults[1,3], EstResults[2,3], (EstResults[1,3]/EstResults[2,3]), EstResults[3,3], (EstResults[1,3]-crit*EstResults[2,3]), (EstResults[1,3]+crit*EstResults[2,3]), EstResults[1,2], EstResults[2,2],EstResults[1,1], EstResults[2,1])
    return(AKEstimationResults)
  }
}
###################
###################
###################





###################
## AK2 Estimator
###################
AK2logLik <- function(para, mydata, z) {
  n = nrow(mydata);
  tauhat <- para[1];
  beta1 <- para[2];
  beta2 <- para[3];
  beta3 <- para[4];
  
  Coeff <- as.matrix(para[5:length(para)])
  if(ncol(mydata)>3){
    err <- mydata[,2] - as.matrix(mydata[,c(4:ncol(mydata))])%*%Coeff;
    esthat <- as.matrix(mydata[,c(4:ncol(mydata))])%*%Coeff;
  }else{
    err <- mydata[,2] - Coeff;
    esthat <- Coeff;
  }
  
  se <- mydata[,3];  
  t <- mydata[,2]/mydata[,3]
  
  cutoffs <- c(-1.96, 0, 1.96);
  phat <- (t<=-1.96)*beta1 + (t>-1.96 & t<=0)*beta2 + (0< t &t<1.96)*beta3 + (t>=1.96)*1  
  
  meanbeta=rep(0,n);
  for (i in 1:n) {
    prob_vlow <- pnorm((cutoffs[1]*se[i]-esthat[i])/sqrt((tauhat) +se[i]^2))
    prob_low <- (pnorm((cutoffs[2]*se[i]-esthat[i])/sqrt((tauhat) +se[i]^2))
                 -pnorm((cutoffs[1]*se[i]-esthat[i])/sqrt((tauhat) +se[i]^2)));
    prob_upper<- (pnorm((cutoffs[3]*se[i]-esthat[i])/sqrt((tauhat) +se[i]^2))
                  -pnorm((cutoffs[2]*se[i]-esthat[i])/sqrt((tauhat) +se[i]^2)));
    prob_vupper<- 1-prob_vlow-prob_low-prob_upper;
    meanbeta[i] <- beta1*prob_vlow + beta2*prob_low + beta3*prob_upper + 1*prob_vupper;
  }
  
  fX = dnorm(err, 0, sqrt(se^2 + tauhat));
  L <- (phat/meanbeta)*fX;
  logL <- log(L);
  LLH <- -sum(log(L));
  if(tauhat<0 | beta1<0 | beta2<0 | beta3<0 ){ LLH <- 10^10; }
  if(z=="est"){return(LLH);}else{return(logL);}
}
AK2logLik_LLH <- function(para) {
  return(AK2logLik(para, AKdata, "est"))
}


AK2.est <-function(AKdata) {
  subAKdata <- AKdata[, c(2, 4:ncol(AKdata))]
  #InitialValue<-as.numeric(summary(lm(effect~.-1, data=subAKdata, weights=1/AKdata$se^2))$coefficients[,1])
  InitialValue<-as.numeric(mean(AKdata$effect))
  t2 <- mean(AKdata$se^2)/4
  Result <- tryCatch(
    {
      nlminb(AK2logLik_LLH, start=c(t2,0,0,0,InitialValue), lower=c(0, 0.01, 0.01, 0.01, rep(-Inf,length(InitialValue))),upper=c(Inf, Inf, Inf, Inf, rep(Inf,length(InitialValue))), hessian=TRUE, control=list(iter.max=1000, abs.tol=10^(-20), eval.max=1000)); },
      error=function(cond){
        return(list(Error = 'error',
                    convergence = 1,
                    par = c(rep(NA, ncol(AKdata)+1)),
                    EstResults=as.data.frame(matrix(NaN, nrow=5, ncol=5))))
      }
  )

    AKEstimationResults <- as.data.frame(matrix(NA, nrow=14, ncol=4))
    colnames(AKEstimationResults) <- c("method","term","variable","value")
    AKEstimationResults$method<-"AK2"
    AKEstimationResults$term<-c(rep("b0",6),"rho1","rho1","rho2","rho2","rho3","rho3","tau2","tau2")
    AKEstimationResults$variable <- c("estimate", "std.error", "statistic", "p.value","conf.low", "conf.high","estimate","std.error","estimate","std.error","estimate","std.error","estimate","std.error")

err<- tryCatch(
    {
     diag(sqrt(ginv(hessian(AK2logLik_LLH, Result$par), tol=10^(-30)))); },
     error=function(cond){
        return("error")
})

  if(err=="error" | Result$convergence==1){
    AKEstimationResults$value <- NA
    return(AKEstimationResults)
  }else{
    EstCoefficients <- t(as.matrix(Result$par))
    colnames(EstCoefficients) <-  c("tau2", "Beta1", "Beta2", "Beta3", colnames(AKdata)[4:ncol(AKdata)])
    StdErrors <- diag(sqrt(ginv(hessian(AK2logLik_LLH, Result$par), tol=10^(-30))))
    PValue <- dt(EstCoefficients/StdErrors, df=nrow(AKdata)-length(EstCoefficients))
    
    if(sum(is.finite(PValue))==length(EstCoefficients)){
      EstResults <- rbind(EstCoefficients, StdErrors, PValue)
      rownames(EstResults) <- c("Coefficients", "Std.Err", "p-value")
      crit <- qt(0.975,df=(nrow(AKdata)-length(EstCoefficients)))    
   AKEstimationResults$value <- c(EstResults[1,5], EstResults[2,5], (EstResults[1,5]/EstResults[2,5]), EstResults[3,5], (EstResults[1,5]-crit*EstResults[2,5]), (EstResults[1,5]+crit*EstResults[2,5]), EstResults[1,2], EstResults[2,2], EstResults[1,3], EstResults[2,3], EstResults[1,4], EstResults[2,4],EstResults[1,1], EstResults[2,1])
      return(AKEstimationResults)
    }else{
      AKEstimationResults$value <- NA
      return(AKEstimationResults)
    }
  }
}
###################
###################
###################





###################
## Clustered Standard Errors
###################
ClusteredSE_AK <-function(AK1Est, my.data, type) {
  est <- AK1Est$nlmOutput$par;
  HessMatrix <- AK1Est$hessian;
  g <- matrix(0, nrow=nrow(my.data), ncol=length(est))
  for(i in 1:length(est)){
    grad1 <- rep(0, length(est));
    grad2 <- rep(0, length(est));
    stepsize <- 10^(-6)
    grad1[i] <- -stepsize
    grad2[i] <- stepsize
    if(type=='AK1'){
      g[,i] <- (AK1logLik(est+grad2, my.data, "gra")-AK1logLik(est+grad1, my.data, "gra"))/(2*stepsize)
    }else{
      g[,i] <- (AK2logLik(est+grad2, my.data, "gra")-AK2logLik(est+grad1, my.data, "gra"))/(2*stepsize)
    }
  }
  
  cluster_index<-my.data[,1]
  I<-order(cluster_index);
  cluster_index<-sort(cluster_index);
  g = g[I,]
  g = g - matrix(rep(apply(g,2,mean),length(I)),nrow=length(I),byrow=TRUE);
  gsum = apply(g,2,cumsum);
  index_diff <- cluster_index[-1] != cluster_index[-length(cluster_index)];
  index_diff <- c(index_diff,1);
  gsum = gsum[index_diff==1,]
  gsum=rbind(gsum[1,], diff(gsum));
  Sigma=1/(dim(g)[1]-1)*(t(gsum)%*%gsum);
  RobuClusteredStdErr <- tryCatch(
    {
      diag(sqrt(nrow(my.data)*ginv(HessMatrix, tol=10^(-30))%*%Sigma%*%ginv(HessMatrix, tol=10^(-30))))
    },
    error=function(cond){ return('error'); }
  )
  return (RobuClusteredStdErr)
}
###################
###################
###################


