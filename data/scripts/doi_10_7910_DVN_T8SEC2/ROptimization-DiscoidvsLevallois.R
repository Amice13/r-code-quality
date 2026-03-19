#Optimization

#SVM

set.seed(406)
SVMgrid <- expand.grid(C = 2^(seq(-6,6)),degree=c(1,2,3), scale=c(0.1,0.2,0.3))

dtm4<-train(method~., data = training,
                          method="svmPoly",
                          trcontrol=ctrl,
                          tuneGrid=SVMgrid,
                          preProcess = c("center","scale"))
dtm4
dtm4$bestTune
dtm4$finalModel
predict4<-predict(dtm4,testing)
confusionMatrix(predict4,testing$method)


#KNN


knn.cross <- tune.knn(x = training[,-1],
                     y = training$method,
                     k = 1:20,
                     trControl= ctrl,
                     cross=10)
summary(knn.cross)
plot(knn.cross)
knn.tuned<- train(x = training[,-1],
                                      y = training$method,
                                      method = "knn",
                                      metric = "Kappa",
                                      preProc = c("center", "scale"),
                                      tuneLength = 20,
                                      trControl = ctrl)
do4tuned<-predict(knn.tuned,testing[,-1])
confusionMatrix(do4tuned,testing$method)


#NB                   

nbtuned<- train(x = training[,-1], 
                                    y = training$method,
                                    method = "nb",
                                    metric = "Kappa",
                                    preProc = c("center", "scale"),
                                    tuneLength = 20)

do7tuned<-predict(nbtuned,testing[,-1])
confusionMatrix(do7tuned,testing$method)

                                       

#RANDOM FOREST

rftuned<- tuneRF(x = training[-1],
                       y = training$method,
                       ntreeTry = 500, # n trees
                       mtryStart = 10,#n of variables
                       stepFactor = 1.5,#increases n of variables by this factor at each step
                       improve  = .01,
                       trace = FALSE )

rftuned #for optimal "mtry"
RFtunedmodel <- randomForest(
                  x = training[-1],
                  y = training$method,
                  ntreeTry  = 500, # n trees
                  mtryStart = 7,#n of variables
                  stepFactor = 1.5,#increases n of variables by this factor at each step
                  improve  = .01,
                  trace = FALSE )

do10tuned<-predict(RFtunedmodel,testing[,-1])
confusionMatrix(do10tuned,testing$method)
varImpPlot(rftunedmodel)


#MDA

mdatuned<- train(x = training[,-1],
                 y = training$method,
                 method = "mda",
                  metric = "Kappa",
                  preProc = c("center", "scale"),
                  tuneLength = 10,
                  tuneGrid=expand.grid(.subclasses=1:13),
                  trControl = ctrl)

do6tuned<-predict(mdatuned,testing[,-1])
confusionMatrix(do6tuned,testing$method)
                                  

#PLS
       
plstuned<- train(x = training[,-1],
                  y = training$method,
                  method = "pls",
                  metric = "Kappa",
                  preProc = c("center", "scale"),
                  tuneLength = 10,
                  tuneGrid=expand.grid(.ncomp=1:10),
                  trControl = ctrl)
                  
do9tuned<-predict(plstuned,testing[,-1])
confusionMatrix(do9tuned,testing$method)


#C5.0

grid <- expand.grid( .winnow = c(TRUE,FALSE), .trials=c(1,5,10,15,20), .model="tree" )
tunedtree<- train(x = training[,-1], 
                                              y = training$method,
                                              method = "C5.0",
                                              metric = "Kappa",
                                              preProc = c("center", "scale"),
                                              tuneLength = 10,
                                              trControl = ctrl,
                                              tuneGrid=grid)
do9tuned<-predict(tunedtree,testing[,-1])
confusionMatrix(do9tuned,testing$method)



#Without "Technology classification" variable



library(dplyr)
a2 <- select(a, -technology_classification)
head(a2)
a3<-a2[,-28]
set.seed(406)
trainIndex1<-createDataPartition(a3$method, p=.70, list=FALSE)
training1<-a3[trainIndex,]
testing1<-a3[-trainIndex,]


#SVM

set.seed(406)
SVMgrid <- expand.grid (C = 2^(seq(-6,6)),degree=c(1,2,3), scale=c(0.1,0.2,0.3))
dtm4.1<-train(method~., data = training1,
                          method="svmPoly",
                          trcontrol=ctrl,
                          tuneGrid=SVMgrid,
                          preProcess = c("center","scale"))
 
 
dtm4.1
dtm4.1$bestTune
dtm4.1$finalModel
predict4.1<-predict(dtm4.1,testing1)
confusionMatrix(predict4.1,testing1$method)

#KNN

knn.cross.1 <- tune.knn(x = training1[,-1],
                             y = training1$method,
                             k = 1:20,
                             trControl= ctrl,
                             cross=10)
summary(knn.cross.1)
plot(knn.cross.1)
knn.tuned.1<- train(x = training1[,-1],
                      y = training1$method,
                      method = "knn",
                      metric = "Kappa",
                      preProc = c("center", "scale"),
                      tuneLength = 8,
                      trControl = ctrl)

doknn.1tuned<-predict(knn.tuned.1,testing1[,-1])
confusionMatrix(doknn.1tuned,testing1$method)


#NB                                    

nbtuned.1<- train(x = training1[,-1], 
                                       y = training1$method,
                                       method = "nb",
                                       metric = "Kappa",
                                       preProc = c("center", "scale"),
                                       tuneLength = 8)
do7.1tuned<-predict(nbtuned.1,testing1[,-1])
confusionMatrix(do7.1tuned,testing1$method)


#RANDOM FOREST

set.seed(406)
RFtunedmodel.1 <- tuneRF(
                 x = training1[-1],
                 y = training1$method,
                 ntreeTry = 500, # n trees
                 mtryStart = 7,#n of variables
                 stepFactor = 1.5,#increases n of variables by this factor at each step
                 improve = .01,
                 trace = FALSE )
 

RFtunedmodel.1

RFtunedmodel.1 <- randomForest(
                         x = training1[-1],
                         y = training1$method,
                         ntreeTry = 500, # n trees
                         mtryStart  = 10,#n of variables
                         stepFactor = 1.5,#increases n of variables by this factor at each step
                         improve = .01,
                         trace = FALSE )

do10.1tuned<-predict(RFtunedmodel.1,testing1[,-1])
confusionMatrix(do10.1tuned,testing1$method)


#MDA       

mdatuned.1<- train(x = training1[,-1],
                   y = training1$method,
                   method = "mda",
                   metric = "Kappa",
                   preProc = c("center", "scale"),
                   tuneLength = 10,
                   tuneGrid=expand.grid(.subclasses=1:13),
                   trControl = ctrl)
 
do6.1tuned<-predict(mdatuned.1,testing1[,-1])
confusionMatrix(do6.1tuned,testing1$method)

#PLS

plstuned.1<- train(x = training1[,-1],
                     y = training1$method,
                     method = "pls",
                     metric = "Kappa",
                     preProc = c("center", "scale"),
                     tuneLength = 10,
                     tuneGrid=expand.grid(.ncomp=1:10),
                     trControl = ctrl) 
do9.1tuned<-predict(plstuned.1,testing1[,-1])
confusionMatrix(do9.1tuned,testing1$method)

#C5.0        

grid <- expand.grid( .winnow = c(TRUE,FALSE), .trials=c(1,5,10,15,20), .model="tree" )
tunedtree.1<- train(x = training1[,-1], 
                         y = training1$method,
                         method = "C5.0",
                         metric = "Kappa",
                         preProc = c("center", "scale"),
                         tuneLength = 10,
                         trControl = ctrl,
                         tuneGrid=grid)
do9.1tuned<-predict(tunedtree.1,testing1[,-1])
confusionMatrix(do9.1tuned,testing1$method)



#FINAL MODEL REPRESENTATION. We selected RF because it is one of the most accurate models, with a clear advantage above others since it needs only 10 out of 27 variables (without "Technology Classification", discarded because its subjectivity) 

varImpPlot(RFtunedmodel.1)
plot(RFtunedmodel.1)
legend("bottomright", legend=c("OOBE","Discoid","Centripetal recurrent Levallois"),
        col=c("black", "green", "red"), lty=1:3, cex=0.6)


