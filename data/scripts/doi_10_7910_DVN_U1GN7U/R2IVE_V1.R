library('gcdnet')
library('naivereg')
source('besttuning.R')
source('NAIVEreg.R')
source('tsls.R')

R2IVE03<-function(y,D,Z,intercept=FALSE,IV.intercept=FALSE,
                criterion=c("CV","BIC","EBIC"),
                nfolds=10,lambda=NULL,lambda2=1,tau){
  
  p<-ncol(Z)
  n<-nrow(Z)
  
  ####Step 1####
  
  fit.d<-NAIVEreg(y,D,Z,criterion="BIC",intercept = IV.intercept)
  Dhat<-fit.d$xhat 
  ind_r<-fit.d$ind
  
  ####Step 2####
  Z_tilde= Z - Dhat %*% t(Dhat) %*% Z / sum( Dhat^2) 
  
  fit.y_1<-best.tuning(Z_tilde,y,criterion = criterion,nfolds = nfolds,lambda2 = 1,cons = 1)
  w<-ifelse(fit.y_1$beta==0,(.Machine$double.eps*2)^(-1),abs(fit.y_1$beta)^(-1))
  
  fit.y_2<-best.tuning(Z_tilde,y,criterion = criterion,nfolds = nfolds,lambda2 = lambda2,cons = 0.2,pf=w3)
  ind_c<-which(fit.y_2$beta!=0)
  
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
 