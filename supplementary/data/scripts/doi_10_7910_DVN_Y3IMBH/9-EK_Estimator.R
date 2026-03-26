###################################
## Primary Study Data Generation 
###################################
EK.est <- function(d, v, tb=1.96, ts=1.96) {
  se <- sqrt(v)
  m <- length(v)

  ## Estimating FPP
  FP1 <- summary(lm(d~se, weight=1/(v)))
  if(FP1$coefficient[1,4]>0.05){
   alpha1 <- as.numeric(FP1$coefficient[1,1])
   Q <- sum((as.numeric(FP1$residuals)/se)^2)
  }else{
   FP2 <- summary(lm(d~v, weight=1/(v)))
   alpha1 <- as.numeric(FP2$coefficient[1,1])
   Q <- sum((as.numeric(FP2$residuals)/se)^2)
  }

  ## Computing a (critical value) and g(.)
  wi <- 1/(v)
  sig_eta2 <- max(0, m*((Q/(m-2))-1)/sum(wi))
  a <- (((alpha1^2)-tb*tb*sig_eta2)/((tb+ts)*alpha1))*(alpha1>(tb*sqrt(sig_eta2)))
  g <- (se-a)*(se>=a)

  ## EK regression
  reg <- lm(d~g, weight=1/(v))
  EKreg <- summary(reg)$coefficient[1,]
  EKConf <- confint(reg)[1,]

  ## Combining Results
  EKEstimationResults <- as.data.frame(matrix(NA, nrow=7, ncol=4))
  colnames(EKEstimationResults) <- c("method","term","variable","value")
  EKEstimationResults$method<-"EK"
  EKEstimationResults$term<-"b0"
  EKEstimationResults$variable <- c("estimate", "std.error", "statistic", "p.value","conf.low", "conf.high","DataSize")
  EKEstimationResults$value <- c(as.numeric(EKreg), as.numeric(EKConf), m)

  return(EKEstimationResults)
}
