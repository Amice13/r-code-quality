Perform_Lasso_integration<-function(data,Y,n_folds,N_seq,penality){
  Results=c()
  C0=c()
  for (out in seq(1,N_seq)){
    #print(out)
    cv_Y0<-sample(rep(1:n_folds, length.out = length(which(Y==0))))
    cv_Y1<-sample(rep(1:n_folds, length.out = length(which(Y==1))))
    
    for (fold in seq(1:n_folds)){
      #print(fold)
      #Test Data
      test_sub_cv_Y0=which(Y==0)[which(cv_Y0==fold)]
      test_sub_cv_Y1=which(Y==1)[which(cv_Y1==fold)]
      
      
      Data_te_0=data[,c(test_sub_cv_Y0,test_sub_cv_Y1)]
      Y_te_0=Y[c(test_sub_cv_Y0,test_sub_cv_Y1)]
      
      #Training Data
      training_sub_cv_Y0=which(!cv_Y0==fold)
      training_sub_cv_Y1=which(!cv_Y1==fold)+length(which(Y==0))
      
      Data_tr_0=data[,c(training_sub_cv_Y0,training_sub_cv_Y1)]
      Y_tr_0=Y[c(training_sub_cv_Y0,training_sub_cv_Y1)]
      
      req_feat_D0<-c()
      X_glm=t(Data_tr_0)
      nfolds=3
      Y_glm=Y_tr_0
      #rownames(X)=Y
      count_D0=0
      while(is_empty(req_feat_D0)){
        #print('in the loop D0')
        cvfit1<-cv.glmnet(X_glm,Y_glm,family="binomial",type.measure="mse",nfolds=nfolds,penality.factor=penality)
        s1=cvfit1$lambda.min
        C=coef(cvfit1,s=s1)
        req_feat_D0<-names(which(C[-1,1]!=0))
        #print(req_feat_D0)
        if(count_D0>50){
          break
        } else {
          count_D0=count_D0+1
        }
      }
      C0=cbind(C0,coef(cvfit1,s=s1))
      tmps<-compute_metric(req_feat_D0,Data_tr_0,Y_tr_0,Data_te_0,Y_te_0)
      Results=rbind(Results,tmps)
    }
  }
  return(list(colMeans(Results),C0))
}
