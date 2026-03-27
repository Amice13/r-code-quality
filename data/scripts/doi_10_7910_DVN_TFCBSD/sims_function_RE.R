sims <- function(){
  # Generate a set of correlated variables
  mu=rep(0,4)
  rho <- 0.25
  ts  <- 1:4
  H <- abs(outer(ts,ts,"-"))
  V <- rho^H
  V[2,2] <- 4

  # Define elements of unobserved heterogeneity
  X <- mvrnorm(N, mu, 3*V)

  z1 <- X[,1]
  z2 <- X[,2]^2
  z3 <- X[,1]*X[,3]

  z1 <- rep(z1,T)
  z2 <- rep(z2,T)
  z3 <- rep(z3,T)

  # Define covariates correlated with the unobserved elements of unobserved heterogeneity
  x1 <- rep(X[,1],T)+rnorm(N*T,0,0.50)
  x2 <- rep(X[,2],T)+rnorm(N*T,0,0.25)
  x3 <- rep(X[,3],T)+rnorm(N*T,0,0.25)
  x4 <- rep(X[,4],T)+rnorm(N*T,0,0.50)


  # Set identifications
  id <- rep(seq(1:N),T)
  time <- rep(1:T,each=N)

  data <- data.frame(x1,x2,x3,x4,id,time,z1,z2,z3)

  data$cre <- z1+z2+z3

  # Generate outcome variable
  z=-4+1*data$x1+1.5*data$x2+0.5*data$x3+1*data$x4+data$cre
  pr=1/(1+exp(-z))
  data$y=sapply(pr,function(i) rbinom(1,1,i))
  data$pr <- pr
  agr <- aggregate(data[,c("x1","x2","x3","x4")],list(id=data$id),mean)
  colnames(agr) <- c("id","V1","V2","V3","V4")
  data=merge(data,agr,by="id")

  d2 <- dataprep(y~x1+x2+x3+x4,data=data,degree=2,id="id")
  data2 <- data.frame(data,d2$xx)

  sum(table(aggregate(y~id, data = data, mean)$y)[c(1,4)])/(N)*100
  # Generate set of penalty parameters for model selection
  # lambda.cand=exp(seq(0,6,by=0.35))

  # PFCRE model selection
  CV <- pfcrems(y~x1+x2+x3+x4,data=data,id="id",family=binomial(link="logit"),max.steps=2000,lambda.cand=lambda.cand, degree=2)


  plot(log(lambda.cand),CV$cv, pch = 20)
  log(lambda.cand)[which(CV$cv == min(CV$cv))]

  # Alternative models Estimators
  # Oracle estimator
  oracle <- lme4::glmer(y~x1+x2+x3+x4+pol1+pol6+pol12+(1|id),data=data2,family=binomial,control = lme4::glmerControl(calc.derivs = FALSE, optimizer = "nloptwrap"))
  # Mundlak's correlated random effects
  cre <- lme4::glmer(y~x1+x2+x3+x4+V1+V2+V3+V4+(1|id),data=data,family=binomial,control = lme4::glmerControl(calc.derivs = FALSE, optimizer = "nloptwrap"))
  # Conditional Maximum Likelihood
  cmle=clogit(y~x1+x2+x3+x4+strata(id),data=data)
  # Fixed effects (dummy variables)
  fe <- feglm(y~x1+x2+x3+x4|id+time, data=data,family=binomial(link="logit"),control = feglmControl(rho.tol=1e-15))
  # Analytical bias corrected Fixed effects
  bcfe <- biasCorr(fe)
  # Basic logit model
  logit <- glm(y~x1+x2+x3+x4,data=data,family=binomial(link="logit"))


  # Estimate Average Partial Effects (or Average Marginal Effects)
  # PFCRE
  d2 <- dataprep(y~x1+x2+x3+x4,data=data,degree=2,id="id")
  data2 <- data.frame(data,d2$xx)
  eq <-  as.formula(paste(CV$Model$formula,"+",paste(names(CV$Model$gamma),collapse="+"),"+(1|id)"))
  pflme <- lme4::glmer(eq,data=data2,family=binomial(link="logit"),control = lme4::glmerControl(calc.derivs = FALSE, optimizer = "nloptwrap"))
  pfAPE <- summary(margins(pflme,variables="x2"))[2]
  # Oracle
  oracleAPE <- summary(margins(oracle,variables="x2"))[2]
  # CRE
  creAPE <- summary(margins(cre,variables="x2"))[2]
  # FE
  feAPE <- getAPEs(fe)$delta[2]
  # BCFE
  bcfeAPE <- getAPEs(bcfe)$delta[2]
  # Logit
  logitAPE <- summary(margins(logit,variables="x2"))[2]
  # Linear Probability Model
  lpm <- felm(y~x1+x2+x3+x4 | id, data=data)
  lpmAPE <- lpm$coefficients[2]

  # Predicted Probabilities, absolute error
  # PFCRE
  pfPrE <- mean(abs(predict(pflme,type="response")-data$pr))
  # Oracle
  oraclePrE <- mean(abs(predict(oracle,type="response")-data$pr))
  # CRE
  crePrE <- mean(abs(predict(cre,type="response")-data$pr))
  # FE
  heterolist <- as.numeric(fe$nms.fe$id)
  fePrE <- mean(abs(predict(fe,type="response")-data$pr[data$id %in% heterolist]))
  # BCFE
  bcfePrE <- mean(abs(predict(bcfe,type="response")-data$pr[data$id %in% heterolist]))
  # Logit
  logitPrE <- mean(abs(logit$fitted.values-data$pr))
  # Linear Probability Model
  lpmPrE <- mean(abs(lpm$fitted.values-data$pr))


  # Hausman Test
  # Test - Statistic
  HT=t(cmle$coefficients-CV$Model$beta[2:5])%*%solve(vcov(cmle)-CV$Model$vcov[2:5,2:5])%*%(cmle$coefficients-CV$Model$beta[2:5])
  # P-value
  pval=pchisq(HT,4,lower.tail=F)


  coefs <- as.numeric(c(CV$Model$beta[3],oracle@beta[3],cre@beta[3],cmle$coefficients[2],fe$coefficients[2],bcfe$coefficients[2],logit$coefficients[3]))
  apes <- as.numeric(c(pfAPE,oracleAPE,creAPE,feAPE,bcfeAPE,logitAPE,lpmAPE))
  prE <- as.numeric(c(pfPrE, oraclePrE, crePrE, fePrE, bcfePrE, logitPrE,lpmPrE))
  Htest <- as.numeric(c(HT,pval))

  names(coefs) <- c("PFCRE","Oracle","CRE","CMLE","FE","BCFE","Logit")
  names(apes) <- c("PFCRE","Oracle","CRE","FE","BCFE","Logit","LPM")
  names(prE) <- c("PFCRE","Oracle","CRE","FE","BCFE","Logit","LPM")
  names(Htest) <- c("TestStat","pvalue")
  results <- list(coefs = coefs, apes = apes, prE = prE, Htest = Htest)
  return(results)
}

