path="C:/Users/Cnpq 43517320189/odrive/OneDrive For Business/UFU/Pesquisa/Pesquisas em andamento/Altman-2019/FD-2021/"
fd=NULL;time=NULL;na=NULL;smote=NULL
for (fd in 5) {
  for (time in 1:3) {
    for (na in 1) {
      for (smote in 0) {
        setwd(paste0(path,"Tests1/FD",fd,"/t-",time,"/NA",na,"/SMOTE",smote))
        
        if(!require("readxl")){install.packages("readxl")}
        library(readxl)
        train <- as.data.frame(read_excel(paste0(path,"samples1/train",fd,time,na,smote,".xlsx")))
        test <- as.data.frame(read_excel(paste0(path,"samples1/test",fd,time,na,".xlsx")))
        
        if(!require("caret")){install.packages("caret")}
        library(caret)#create models with cross-validation
        #validação cruzada
        trctrl <- trainControl(method = "cv", number = 5, allowParallel=TRUE)
        
        if(!require("doParallel")){install.packages("doParallel")}
        library(doParallel)
        #start parallel processing
        ncl<-detectCores()
        ncl
        cl <- parallel::makeCluster(ncl, setup_strategy = "sequential")
        registerDoParallel(cl)
        set.seed(123)
        ########RandomForest-----------------------------------
        ptm <- proc.time()
        train$FD<-as.factor(train$FD)
        model.rf <- train(FD ~., data = train, 
                          method = "rf", family=binomial,
                          trControl=trctrl)
        imp.rf<-varImp(model.rf, scale = TRUE)
        rownames(imp.rf[["importance"]])
        importance.rf <- data.frame(rownames(imp.rf$importance), imp.rf$importance$Overall)
        names(importance.rf)<-c('Platform', 'Importance')
        
        # Plot the data with ggplot.
        if(!require("dplyr")){install.packages("dplyr")}
        library(dplyr)
        if(!require("forcats")){install.packages("forcats")}
        library(forcats)
        
        pdf(paste0("varimp-rf",fd,time,na,smote,".pdf"))
        print(
          importance.rf %>%
            mutate(Platform = fct_reorder(Platform, Importance)) %>%
            ggplot(aes(x=Platform, y=Importance)) +
            geom_bar(stat = 'identity',colour = "gray", fill = "gray") + coord_flip()
        )
        dev.off()
        
        pred.rf<- predict(model.rf, newdata = test[,-c(1:3)], type = c("prob"))
        cm.rf=confusionMatrix(as.factor(ifelse(pred.rf[,2]>=0.5,1,0)), as.factor(test$FD))
        proc.time() - ptm
        
        # Plot ROC curve
        if(!require("ROCR")){install.packages("ROCR")}
        library(ROCR)#generate ROC plot
        pred.rf1 <-predict(model.rf, test[,-c(1:3)], type = "prob")[,2] #probabilidad de classe=yes 
        pred.rf2 <- prediction(predictions = pred.rf1, labels = test$FD)
        predict.rocr.rf <- prediction(pred.rf1, test$FD) # valor real da classe
        perf.rf <- performance(predict.rocr.rf,"tpr", "fpr")
        pdf(paste0("ROC-rf",fd,time,na,smote,".pdf"))
        plot(perf.rf, main = "ROC curve Random Forest",
             col = "blue", lwd = 2)
        abline(a = 0, b = 1, lwd = 2, lty = 2)
        dev.off() 
        
        #AUC
        perf.auc.rf <- performance(predict.rocr.rf,"auc")
        unlist(perf.auc.rf@y.values)
        
        #BRIER SCORE
        if(!require("verification")){install.packages("verification")}
        library(verification)#for verify()
        a.rf <- verify(as.numeric(test$FD), pred.rf1)
        summary(a.rf)
        
        #KS
        ks.rf = max (attr (perf.rf, 'y.values') [[1]] - attr (perf.rf, 'x.values') [[1]])
        ks.rf
        
        ########Logistic Regression-----------------------------------
        ptm <- proc.time()
        model.lr <- train(FD ~., data = train, 
                          method = "glm", family=binomial)
        imp.lr<-varImp(model.lr, scale = TRUE)
        rownames(imp.lr[["importance"]])
        importance.lr <- data.frame(rownames(imp.lr$importance), imp.lr$importance$Overall)
        names(importance.lr)<-c('Platform', 'Importance')
        
        # Plot the data with ggplot.
        if(!require("dplyr")){install.packages("dplyr")}
        library(dplyr)
        if(!require("forcats")){install.packages("forcats")}
        library(forcats)
        pdf(paste0("varimp-lr",fd,time,na,smote,".pdf"))
        print(
          importance.lr %>%
            mutate(Platform = fct_reorder(Platform, Importance)) %>%
            ggplot(aes(x=Platform, y=Importance)) +
            geom_bar(stat = 'identity',colour = "gray", fill = "gray") + coord_flip()
        )
        dev.off()
        
        
        pred.lr<- predict(model.lr, newdata = test[,-c(1:3)], type = c("prob"))
        cm.lr=confusionMatrix(as.factor(ifelse(pred.lr[,2]>=0.5,1,0)), as.factor(test$FD))
        proc.time() - ptm
        
        # Plot ROC curve
        if(!require("ROCR")){install.packages("ROCR")}
        library(ROCR)#generate ROC plot
        pred.lr1 <-predict(model.lr, test[,-c(1:3)], type = "prob")[,2] #probabilidad de classe=yes 
        pred.lr2 <- prediction(predictions = pred.lr1, labels = test$FD)
        predict.rocr.lr <- prediction(pred.lr1, test$FD) # valor real da classe
        perf.lr <- performance(predict.rocr.lr,"tpr", "fpr")
        pdf(paste0("ROC-lr",fd,time,na,smote,".pdf"))
        plot(perf.lr, main = "ROC curve Logistic Regression",
             col = "red", lwd = 2)
        abline(a = 0, b = 1, lwd = 2, lty = 2)
        dev.off() 
        
        #AUC
        perf.auc.lr <- performance(predict.rocr.lr,"auc")
        unlist(perf.auc.lr@y.values)
        
        #BRIER SCORE
        if(!require("verification")){install.packages("verification")}
        library(verification)#for verify()
        a.lr <- verify(as.numeric(test$FD), pred.lr1)
        summary(a.lr)
        
        #KS
        ks.lr = max (attr (perf.lr, 'y.values') [[1]] - attr (perf.lr, 'x.values') [[1]])
        ks.lr
        
        #save models
        save(model.rf, file=paste0("model-rf",fd,time,na,smote,".rda"))
        save(model.lr, file=paste0("model-lr",fd,time,na,smote,".rda"))
        
        #All ROCs in the same plot
        pdf(paste0("ROC",fd,time,na,smote,".pdf"))
        plot(perf.rf,  main = "ROC curve benchmarking", col = "blue", lwd = 2)
        plot(perf.lr, add = TRUE, col = "red", lwd = 2)
        abline(a = 0, b = 1, lwd = 2, lty = 2)
        legend("bottomright", legend=c("LR", "RF"), col=c("red", "blue"), lty=1:1, cex=2)
        dev.off()
        
        #ConfusionMatrices
        cm.rf1 <- table(true = test$FD, pred = ifelse(pred.rf[,2]>=0.5,1,0)) 
        cm.lr1 <- table(true = test$FD, pred = ifelse(pred.lr[,2]>=0.5,1,0)) 
        TP <-c(cm.rf1[1,1],cm.lr1[1,1]); TN <-c(cm.rf1[2,2],cm.lr1[2,2])
        FN <-c(cm.rf1[1,2],cm.lr1[1,2]); FP <-c(cm.rf1[2,1],cm.lr1[2,1])
        TError_I <- c( round(100-TP[1]*100/(TP[1]+FN[1]),2), round(100-TP[2]*100/(TP[2]+FN[2]),2))
        TError_II <- c( round(100-TN[1]*100/(TN[1]+FP[1]),2),round(100-TN[2]*100/(TN[2]+FP[2]),2))
        results=data.frame(ModelxMeasure = c("RF","LR"), TP, TN, FP, FN, TError_I,TError_II,
                           ACC= c(cm.rf$overall[["Accuracy"]],cm.lr$overall[["Accuracy"]]),
                           SENS=c(cm.rf$byClass[["Sensitivity"]], cm.lr$byClass[["Sensitivity"]]),
                           SPEC=c(cm.rf$byClass[["Specificity"]], cm.lr$byClass[["Specificity"]]),
                           BS = c(a.rf$bs,a.lr$bs),
                           AUC = c(perf.auc.rf@y.values[[1]],perf.auc.lr@y.values[[1]]),
                           KS=c(ks.rf,ks.lr))
        testresults=data.frame(ifelse(pred.rf[,2]>=0.5,1,0),ifelse(pred.lr[,2]>=0.5,1,0),
                               round(pred.rf1,2),round(pred.lr1,2))
        colnames(testresults)=c("RF","LR", "RF-prob","LR-prob") #please, confirm these names!
        testfull=cbind(test,testresults)
        
        #Save data and other things
        if(!require("WriteXLS")){install.packages("WriteXLS")}
        library(WriteXLS)
        WriteXLS("train", paste0("trainmatrix",fd,time,na,smote,".xlsx"))# record training data
        WriteXLS("testfull", paste0("testresults",fd,time,na,smote,".xlsx")) # record testing data
        WriteXLS("results", paste0("Performance",fd,time,na,smote,".xlsx")) # Write the perf measures to a new file. 
        
      }
    }
  }
}
  
rm(list = ls())
.rs.restartR()
