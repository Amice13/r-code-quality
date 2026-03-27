a<-read.table(file.choose(),header=T, fill=TRUE)#NA for missing values
dim(a)
a2<-a[,-29]
head(a2)
library(caret)
library(e1071)
library(class)
library(rsample)
library(randomForest)
library(klaR)
library(pls)
library(C50)
library(ggplot2)
set.seed(406)
training<-a2[trainIndex,]
testing<-a2[-trainIndex,]
ctrl <- trainControl(method = "repeatedcv", repeats=5)
trainIndex<-createDataPartition(a2$method, p=.70, list=FALSE)

#SVM
set.seed(406) 
vmFit <- train(x = training[,-1],
                  y = training$method,
                  method = "svmRadial",
                  metric = "Kappa",
                  preProc = c("center", "scale"),
                  tuneLength = 10,
                  trControl = ctrl)
do2<-predict(vmFit,testing[,-1])
confusionMatrix(do2,testing$method)

#KNN
set.seed(406) 
knFit <- train(x = training[,-1],
                                y = training$method,
                                method = "knn",
                                metric = "Kappa",
                                preProc = c("center", "scale"),
                                tuneLength = 10,
                                trControl = ctrl)
do4<-predict(knFit,testing[,-1])
confusionMatrix(do4,testing$method)

#Random Forest
set.seed(406) 
rfFit <- train(x = training[,-1], 
                                 y = training$method,
                                 method = "rf",
                                 metric = "Kappa",
                                 preProc = c("center", "scale"),
                                 tuneLength = 10,
                                 trControl = ctrl)
do5<-predict(rfFit,testing[,-1])
confusionMatrix(do5,testing$method)

#MDA
set.seed(406)
mdaFit<- train(x = training[,-1],
                                y = training$method,
                                method = "mda",
                                metric = "Kappa",
                                preProc = c("center", "scale"),
                                tuneLength = 10,
                                trControl = ctrl)
do6<-predict(mdaFit,testing[,-1])
confusionMatrix(do6,testing$method)

#Naive Bayes
set.seed(406)
nbFit<- train(x = training[,-1], 
                y = training$method,
                method = "nb",
                metric = "Kappa",
                preProc = c("center", "scale"),
                tuneLength = 10,
                trControl = ctrl)
do7<-predict(nbFit,testing[,-1])
confusionMatrix(do7,testing$method)


#PLS
set.seed(406)
plsFit<- train(x = training[,-1], 
                                 y = training$method,
                                 method = "pls",
                                 metric = "Kappa",
                                 preProc = c("center", "scale"),
                                 tuneLength = 10,
                                 trControl = ctrl)
do9<-predict(plsFit,testing[,-1])
confusionMatrix(do9,testing$method)

#C5.0                                          
tree<- train(x = training[,-1], 
              y = training$method,
              method = "C5.0",
              metric = "Kappa",
              preProc = c("center", "scale"),
              tuneLength = 10,
              trControl = ctrl)
do9<-predict(tree,testing[,-1])
confusionMatrix(do9,testing$method)

