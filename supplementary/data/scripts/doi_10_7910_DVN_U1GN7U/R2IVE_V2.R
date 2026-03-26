#install.packages("gcdnet")
library('gcdnet')
source('besttuning.R')

R2IVE<-function(y,D,Z,intercept=FALSE,IV.intercept=FALSE,
                lambda11=NULL,lambada12=0,lambda21=NULL,lambada22=0,
                criterion=c("CV","BIC","EBIC"),
                nfolds=10,
                tau,type=1
                ){
  
  p<-ncol(Z)
  n<-nrow(Z)
  
  ####Step 1####
  #criterion = 'BIC';nfolds = 10
  fit.d1<-best.tuning(Z,D,criterion = criterion,nfolds = nfolds,lambda2 = 1,cons = 1)
  w1<-rep(0,p)
  w1<-ifelse(fit.d1$beta==0,(.Machine$double.eps*2)^(-1),abs(fit.d1$beta)^(-1))
  
  #lambda11=NULL;lambada12=0;lambda21=NULL;lambada22=0
  fit.d2<-best.tuning(Z,D,criterion = criterion,nfolds = nfolds,lambda=lambda11,lambda2 = lambada12,cons = 1,pf=w1)
  ind_r<-which(fit.d2$beta!=0)
  
  #refit
  if(IV.intercept=='FALSE'){
    fit.d3<-lm(D~Z[,ind_r]-1)
  }else{
    fit.d3<-lm(D~Z[,ind_r])
  }
  
  gammahat<-matrix(fit.d3$coefficients)
  Dhat<-fit.d3$fitted.values
  
  ####Step 2####
  
  #initial estimator for alpha
  
  ##estimate Gamma
  fit.y1<-best.tuning(Z,y,criterion = criterion,nfolds = nfolds,lambda2 = 1,cons = 1)
  w2<-rep(0,p)
  w2<-ifelse(fit.y1$beta==0,(.Machine$double.eps*2)^(-1),abs(fit.y1$beta)^(-1))
  fit.y2<-best.tuning(Z,y,criterion = criterion,nfolds = nfolds,lambda2 = 0,cons = 1,pf=w2)
  ind<-which(fit.y2$beta!=0)
  if(all(ind_r %in% ind)){
    if(IV.intercept=='FALSE'){
      fit.y3<-lm(y~Z[,ind]-1)
    }else{
      fit.y3<-lm(y~Z[,ind])
    }
  }else{
    if(IV.intercept=='FALSE'){
      fit.y3<-lm(y~Z[,union(ind_r,ind)]-1)
    }else{
      fit.y3<-lm(y~Z[,union(ind_r,ind)])
    }
  }  
  Gammahat<-matrix(fit.y3$coefficients)
  
  ##median estiamtor 
  i_beta<-median(Gammahat[1:length(gammahat)]/gammahat)
  
  ##initial estimator for alpha
  y_tilde<-y-D*i_beta
  fit.y_t1<-best.tuning(Z,y_tilde,criterion = criterion,nfolds = nfolds,lambda2 = 1,cons = 1)
  w3<-rep(0,p)
  w3<-ifelse(fit.y_t1$beta==0,(.Machine$double.eps*2)^(-1),abs(fit.y_t1$beta)^(-1))
  
  ##estimate alpha and select controls
  if(type==1){
    fit.y_t2<-best.tuning(Z,y_tilde,criterion = criterion,nfolds = nfolds,lambda=lambda21,lambda2 = lambada22,cons = 1,pf=w3)
  }else{
    Z_tilde= Z - Dhat %*% t(Dhat) %*% Z / sum( Dhat^2)
    fit.y_t2<-best.tuning(Z_tilde,y,criterion = criterion,nfolds = nfolds,lambda=lambda21,lambda2 = lambada22,cons = 0.2,pf=w3)
  }
  fit.y_t2$best.lambda
  ind_c<-which(fit.y_t2$beta!=0)
  
  ####Step 3####
  
  Dnew<-cbind(Dhat,Z[,ind_c])
  if(intercept=='FALSE'){
    fit1<-summary(lm(y~Dnew-1))
    coef = fit1$coefficient[1,1]
    ste= fit1$coefficient[1,2]
  }else{
    fit1<-summary(lm(y~Dnew))
    coef = fit1$coefficient[c(1,2),1]
    ste = fit1$coefficient[c(1,2),2]
  }
  
  upper<-coef+qnorm(1/2+tau/2)*ste
  lower<-coef-qnorm(1/2+tau/2)*ste
  
  object <- list(coef= coef, ste=ste,
                 whichrelevant=ind_r, whichcontrol=ind_c,Dhat=Dhat,
                 upper=upper,lower=lower)
}

 