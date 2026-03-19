library(gcdnet)

best.tuning<-function(X,y,lambda=NULL,lambda2=0,
                      criterion=c("CV","BIC","EBIC"),
                      nfolds=10,cons=1,pf=pf){
  
  n<-length(y)
  p<-ncol(X)
  
  if(missing(pf)){
    pf=rep(1,ncol(X))
  }
  
  if(missing(criterion)&is.null(lambda)){
      criterion='BIC'
  }
  
  if(!is.null(lambda)){
    best.lambda=lambda
    fit= gcdnet(X,y,method = "ls",lambda=best.lambda,lambda2=lambda2,pf=pf)
    beta<-as.matrix(fit$beta)
  }

  if(!missing(criterion)){
    if(criterion=='CV'){
      fit=cv.gcdnet(X,y,method = "ls",lambda2=lambda2,pred.loss="loss",nfolds=nfolds,pf=pf)
      #best.lambda<-fit$lambda.1se
      best.lambda<-fit$lambda.min
      beta<-as.matrix(fit$gcdnet.fit$beta[,which(fit$lambda==best.lambda)])
    }else{
      fit=gcdnet(X,y,nlambda=100,method = "ls",lambda2=lambda2,pf=pf)
      pr1<-predict(fit,X, s = NULL,type="link")
      RSS<-rep(0,ncol(fit$beta))
      for(i in 1:ncol(pr1)){
        RSS[i]<-mean((pr1[,i]-y)^2)
      }
      #plot(RSS)
      if(criterion=='BIC'){
        BIC = n*log(RSS)+cons*fit$df*log(n)
        #plot(BIC)
        best.lambda<-fit$lambda[which.min(BIC)]
      }else{
        EBIC = log(RSS)+cons*fit$df*(log(n)+0.2*log(ncol(Z)))/n
        #plot(EBIC)
        best.lambda<-fit$lambda[which.min(EBIC)]
      }
      beta<-as.matrix(fit$beta[,which(fit$lambda==best.lambda)])
    }
    
  }
    
 
  
  object <- list(beta= beta, best.lambda=best.lambda,criterion=criterion)
}

 