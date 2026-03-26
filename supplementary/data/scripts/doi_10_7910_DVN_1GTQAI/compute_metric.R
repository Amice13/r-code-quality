compute_metric<-function(req_feat_D0,Data_tr_0,Y_tr_0,Data_te_0,Y_te_0){
  ACC=0
  AUC=0
  ACC_umap=0
  AUC_umap=0
  
  #print(req_feat_D0)
  Data_RF_tr_0=t(Data_tr_0)
  Data_RF_tr_0<-as.matrix(Data_RF_tr_0[,req_feat_D0])
  
  Data_RF_te_0=t(Data_te_0)
  Data_RF_te_0<-as.matrix(Data_RF_te_0[,req_feat_D0])
  
  #print('fitting RF')
  myrf <- randomForest(Data_RF_tr_0, Y_tr_0)
  Y_pred<-round(predict(myrf, Data_RF_te_0))
  print(cbind(Y_pred,Y_te_0))
  ACC<-mean(Y_pred==Y_te_0)
  #print(ACC)
  roc_obj <- roc(Y_te_0, Y_pred, quiet = TRUE)
  AUC<-roc_obj$auc
 
  ACC_umap=0
  AUC_umap=0
  return(c(ACC,AUC,ACC_umap,AUC_umap))
}
