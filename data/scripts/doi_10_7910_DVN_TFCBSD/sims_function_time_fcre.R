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
  z=0.5+1*data$x1+1.5*data$x2+0.5*data$x3+1*data$x4+data$cre
  pr=1/(1+exp(-z))
  data$y=sapply(pr,function(i) rbinom(1,1,i))
  data$pr <- pr
  agr <- aggregate(data[,c("x1","x2","x3","x4")],list(id=data$id),mean)
  colnames(agr) <- c("id","V1","V2","V3","V4")
  data=merge(data,agr,by="id")
  
  d2 <- dataprep(y~x1+x2+x3+x4,data=data,degree=2,id="id")
  data2 <- data.frame(data,d2$xx)
  # Generate set of penalty parameters for model selection
  # lambda.cand=exp(seq(0,6,by=0.35))
  
  # PFCRE model selection
  btime <- Sys.time()
  CV <- pfcrems(y~x1+x2+x3+x4,data=data,id="id",family=binomial(link="logit"),max.steps=2000,lambda.cand=lambda.cand, degree=2)
  pfcre.time <- difftime(Sys.time(),btime, units = "secs") 
  
  # Alternative models Estimators
  # Oracle estimator
  btime <- Sys.time()
  oracle <- lme4::glmer(y~x1+x2+x3+x4+pol1+pol6+pol12+(1|id),data=data2,family=binomial,control = lme4::glmerControl(calc.derivs = FALSE))
  oracle.time <- difftime(Sys.time(),btime, units = "secs") 
  
  # Mundlak's correlated random effects
  btime <- Sys.time()
  cre <- lme4::glmer(y~x1+x2+x3+x4+V1+V2+V3+V4+(1|id),data=data,family=binomial, control = lme4::glmerControl(calc.derivs = FALSE))
  cre.time <- difftime(Sys.time(),btime, units = "secs") 
  
  # Conditional Maximum Likelihood
  btime <- Sys.time()
  cmle=clogit(y~x1+x2+x3+x4+strata(id),data=data)
  cmle.time <- difftime(Sys.time(),btime, units = "secs") 
  
  # F-CRE (Flexible CRE, no penalization, using same specification as PF-CRE)
  btime <- Sys.time()
  fcre <- lme4::glmer(y~x1+x2+x3+x4+pol1+pol2+pol3+pol4+pol5+pol6+pol7+pol8+pol9+pol10+pol11+pol12+pol13+pol14+(1|id), 
                      data = data2, family=binomial, control = lme4::glmerControl(calc.derivs = FALSE))
  fcre.time <- difftime(Sys.time(),btime, units = "secs") 
  
  
  
  coefs <- as.numeric(c(CV$Model$beta[3],oracle@beta[3],cre@beta[3],cmle$coefficients[2],fcre@beta[3]))
  times <- as.numeric(c(pfcre.time,oracle.time,cre.time,cmle.time,fcre.time))
    
    
  names(coefs) <- c("PFCRE","Oracle","CRE","CMLE","FCRE")
  names(times) <- c("PFCRE","Oracle","CRE","FE","FCRE")
  
  results <- list(coefs = coefs, times = times)
  return(results)
}

