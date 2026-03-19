##### Modelling for Bankruptcy Prediction ######
## by Barboza, Kimura, and Altman.

set.seed(5) #get reproducible random numbers for providing similar results.

# Loading all Packages used here.
library(ada)
library(e1071)
library(rpart)
library(mboost)
library(gbm)
library(randomForest)
library(MASS)
library(ipred)
library(aod)
library(nnet)
library(caret)
library(ggplot2)
library(ROCR)

#Reading data. Important! Must be .csv file. First row is the variable names. This file must show data that it will use to train and validate the models. Filing Date should be enumerate as it follows. 
#All rows without data will be excluded!
dataset <- read.table(file.choose(),sep=",",header=TRUE) 

#Date	 Value		  Date	  Value	  Date	  Value		Date	  Value		Date	  Value
#1/1/86	 29951		1/1/87	30316		1/1/88	30681		1/1/89	31047		1/1/90	31412
#1/1/91	 31777		1/1/92	32142		1/1/93	32508		1/1/94	32873		1/1/95	33238
#1/1/96	 33603		1/1/97	33969		1/1/98	34334		1/1/99	34699		1/1/00	35064
#1/1/01	 35430		1/1/02	35795		1/1/03	36160		1/1/04	36525		1/1/05	36891
#1/1/06	 37256		1/1/07	37621		1/1/08	37986		1/1/09	38352		1/1/10	38717
#1/1/11	 39082		1/1/12	39447		1/1/13	39813		1/1/14	40178

head(dataset) #to visualize the beginning of the file content.
colnames(dataset)[c(2,15,17)]=c("Y","BK","FD") #please, confirm these names!
dataset=subset(dataset, select=c("Y","X1","X2","X3","X4","X5","BK","GA","GS","GE","OM","CPB","NBK","FD")) #CR excluded
#Why am I need Date? Because I need FD to identify the Year that occurs the bankruptcy
dataset <- dataset[complete.cases(dataset),] #just remains data with no missing values
dataset <- dataset[!duplicated(dataset),] #nrow(test) is good to confirm 
dataset <- dataset[!dataset$X1==0,] #remove observations with X1=0
dataset <- dataset[!dataset$X4==0,]
dataset <- dataset[!dataset$X5==0,]
#first dataset = data from 1985 bankruptcy companies and 1 Year before the event.
#selecting data from FIRST DATE to LAST DATE for training
trainBK <- dataset[which(dataset$FD>20000 & dataset$FD<37256),] #select BK data from BEGINNING DATE to ENDIND DATE. This command excludes BK firm data as well, except 1Y prior to failure.
nrow(trainBK)
trainNBK <- dataset[which(dataset$Y>=min(trainBK$Y) & dataset$Y<=max(trainBK$Y) & dataset$NBK==0),]
trainNBKrandom <- trainNBK[sample(1:nrow(trainNBK), sum(trainBK$BK==-1)),]  #random portion sample
trainselect <- rbind(trainBK, trainNBKrandom) #append all data (BK and NBK) in the same dataframe.
trainselect$BK=replace(trainselect$BK, trainselect$NBK==0, 1) #replace 0 in BK to 1 in case of NBK firms.
train=subset(trainselect, select=c("X1","X2","X3","X4","X5", "GA", "GE", "GS","OM", "CPB","BK")) #CR excluded

testBK <- dataset[which(dataset$FD>37255 & dataset$FD<42000),] #select BK data from 2008/01 until now.
testNBK <- dataset[which(dataset$Y>=min(testBK$Y) & dataset$Y<=max(testBK$Y) & dataset$NBK==0),]
testNBKrandom <- testNBK[sample(1:nrow(testNBK), 99*sum(testBK$BK==-1)),]  #random portion sample
testselect <- rbind(testBK, testNBKrandom) #append all data (BK and NBK) in the same dataframe.
testselect$BK=replace(testselect$BK, testselect$NBK==0, 1) #replace 0 in BK to 1 in case of NBK firms.
test=subset(testselect, select=c("X1","X2","X3","X4","X5", "GA", "GE", "GS","OM", "CPB","BK"))

index <- 1:nrow(train) #preparing the dataset: counting the number of variables.
N=ncol(train) #preparing the dataset: counting number of observations.

#The models compilation begin right here.
##### SVM-LINEAR #####
# looking for best parameters C and gamma. Large number (500 observations or more) of data can delay too much time.
tuned <- tune.svm(BK~., data = train, gamma = 10^(-2:0), cost = 10^(-1:2))
tbper=tuned$best.performance
tbpar=tuned$best.parameter

#The SVM model by linear Kernel and real number outputs. Probability option give an option about PD as a result.
model.svm.linear = svm(BK~., data = train, kernel="linear", cost=tbpar$cost, gamma=tbpar$gamma, probability=TRUE)

# Using the option "as.factor" the confusion matrix can be produced.
model.svm.linear.factor = svm(as.factor(BK)~., data = train, kernel="linear", cost=tbpar$cost, gamma=tbpar$gamma, probability=TRUE)

#Command below shows the class evaluated by model for training data. Later, PD measures in the first column. 
prediction.svm.linear.train.class <- predict(model.svm.linear.factor, train[,-N])
prediction.svm.linear.train.probabilities <- predict(model.svm.linear.factor, train[,-N], probability=TRUE) 

#Confusion Matrix is given below.
confusion.matrix.svm.linear.train.class <- table(true = train[,N], pred = prediction.svm.linear.train.class)  


##### SVM-RADIAL BASIS FUNCTION #####
#SVM model with Radial Basis Function kernel. The other commands are analogous. 
model.svm.rbf = svm(BK~., data = train, kernel="radial", cost=tbpar$cost, gamma=tbpar$gamma, scale=T, probability=TRUE)
model.svm.rbf.factor = svm(as.factor(BK)~., data = train, kernel="radial", cost=tbpar$cost, gamma=tbpar$gamma, scale=T, probability=TRUE)
prediction.svm.rbf.train.class <- predict(model.svm.rbf.factor, train[,-N])
prediction.svm.rbf.train.probabilities <- predict(model.svm.rbf.factor, train[,-N], probability=TRUE) 
confusion.matrix.svm.rbf.train.class <- table(true = train[,N], pred = prediction.svm.rbf.train.class) 

##### BOOSTING #####
control <- rpart.control(cp = -1, maxdepth = 14, xval = 0)
# cp = -1 forces the tree to split until the depth of the tree achieves the maxdepth setting # xval is the number of cross-validations
model.boosting <- ada(BK~., data = train, test.x = train[,-N], test.y = train[,N], type = "gentle", control = control, iter = 70)
prediction.boosting.train <- predict(model.boosting, train[,-N])
prediction.boosting.train.probabilities <- predict(model.boosting, train[,-N], type="prob")
confusion.matrix.boosting.train.class <- table(true = train[,N], pred = prediction.boosting.train) 

##### BAGGING #####
#Bagging model for classification and PD outputs. The coob option requests the out-of-bag estimate of the misclassification error.
model.bagging = bagging(BK ~ ., data = train, probability=TRUE, coob=TRUE)
model.bagging.factor = bagging(as.factor(BK) ~ ., data = train, probability=TRUE, coob=TRUE) 
prediction.bagging.train.class <- predict(model.bagging.factor, train)
prediction.bagging.train.probabilities <- predict(model.bagging.factor, train[,-N], type="prob")
confusion.matrix.bagging.train.class <- table(true = train[,N], pred = prediction.bagging.train.class)

##### RANDOM FOREST #####
model.random.forest = randomForest(BK ~., data=train, importance=TRUE, proximity=TRUE, probability=TRUE)
model.random.forest.class = randomForest(as.factor(BK) ~., data=train, importance=TRUE, proximity=TRUE)
prediction.random.forest.train.class <- predict(model.random.forest.class, train[,-N]) 
prediction.random.forest.train.probabilities <- predict(model.random.forest.class, train[,-N], type="prob") 
confusion.matrix.random.forest.train.class <- table(true = train[,N], pred = prediction.random.forest.train.class) 

##### NEURAL NETWORK #####
model.neural.network <- train(BK ~ ., train, method='nnet', trControl=trainControl(method='cv'), trace=F)
prediction.neural.network.train <- predict(model.neural.network, train[,-N]) #PD as output.
prediction.neural.network.train.class <- round(prediction.neural.network.train)
confusion.matrix.neural.network.train.class <- table(true = train[,N], pred = prediction.neural.network.train.class)

##### LOGIT #####
train01=train
train01$BK=replace(train01$BK,train01$BK==-1,0)
model.logistic.regression <- glm(BK ~., data=train01, family=binomial(link = "logit"))
prediction.logistic.regression.train <- predict(model.logistic.regression, train01, type="response")
prediction.logistic.regression.train.class <- round(prediction.logistic.regression.train)
confusion.matrix.logistic.regression.train.class <- table(true = train[,N], pred = prediction.logistic.regression.train.class)

##### LDA #####
model.linear.discriminant.analysis=lda(BK ~ . , data=train)
prediction.linear.discriminant.analysis.train.class <- predict(model.linear.discriminant.analysis,newdata=train[,-N])$class
prediction.linear.discriminant.analysis.train.probabilities <- predict(model.linear.discriminant.analysis, newdata=train[,-N])$posterior
confusion.matrix.linear.discriminant.analysis.train.class <- table(true = train[,N], pred = prediction.linear.discriminant.analysis.train.class)

##### Testing the models that were constructed by training step. ##### 

##### Linear SVM Test #####
prediction.svm.linear.test.class <- predict(model.svm.linear.factor, test[,-N])
confusion.matrix.svm.linear.test.class <- table(true = test[,N], pred = prediction.svm.linear.test.class)
prediction.svm.linear.test <- predict(model.svm.linear, test[,-N])
prediction.svm.linear.test.probabilities <- predict(model.svm.linear.factor, test[,-N], probability=TRUE) 

##### SVM with RBF kernel Test #####
prediction.svm.rbf.test.class <- predict(model.svm.rbf.factor, test[,-N])
confusion.matrix.svm.rbf.test.class <- table(true = test[,N], pred = prediction.svm.rbf.test.class)
prediction.svm.rbf.test <- predict(model.svm.rbf, test[,-N])
prediction.svm.rbf.test.probabilities <- predict(model.svm.rbf.factor, test[,-N], probability=TRUE) 

##### Boosting Test #####
prediction.boosting.test <- predict(model.boosting, test[,-N], type="both")
confusion.matrix.boosting.test.class <- table(true = test[,N], pred = prediction.boosting.test$class)
prediction.boosting.test <- prediction.boosting.test$probs[,2]
prediction.boosting.test.probabilities <- predict(model.boosting, test[,-N], type="prob")

##### Bagging Test #####
prediction.bagging.test.class <- predict(model.bagging.factor, test[,-N])
confusion.matrix.bagging.test.class <- table(true = test[,N], pred = prediction.bagging.test.class)
prediction.bagging.test <- predict(model.bagging, test[,-N])
prediction.bagging.test.probabilities <- predict(model.bagging.factor, test[,-N], type="prob")

##### Random Forest Test #####
prediction.random.forest.test.class <- predict(model.random.forest.class, test[,-N]) 
confusion.matrix.random.forest.test.class <- table(true = test[,N], pred = prediction.random.forest.test.class) 
prediction.random.forest.test <- predict(model.random.forest, test[,-N])
prediction.random.forest.test.probabilities <- predict(model.random.forest.class, test[,-N], type='prob')

##### Neural Network Test #####
prediction.neural.network.test <- predict(model.neural.network, test[,-N]) 
prediction.neural.network.test.class <- round(prediction.neural.network.test)
confusion.matrix.neural.network.test.class <- table(true = test[,N], pred = prediction.neural.network.test.class)
prediction.neural.network.test.probabilites <- predict(model.neural.network, test[,-N])  

##### LOGIT Test #####
test01=test
test01$BK=replace(test01$BK,test01$BK==-1,0)
prediction.logistic.regression.test <- predict(model.logistic.regression, test01, type="response")
prediction.logistic.regression.test.class <- round(prediction.logistic.regression.test)
confusion.matrix.logistic.regression.test.class <- table(true = test[,N], pred = prediction.logistic.regression.test.class)
prediction.logistic.regression.test.probabilites <- prediction.logistic.regression.test

##### LDA Test #####
prediction.linear.discriminant.analysis.test.class <- predict(model.linear.discriminant.analysis,test[,-N])$class
confusion.matrix.linear.discriminant.analysis.test.class <- table(true = test[,N], pred = prediction.linear.discriminant.analysis.test.class)
prediction.linear.discriminant.analysis.test <- predict(model.linear.discriminant.analysis,newdata=test[,-N])$x
prediction.linear.discriminant.analysis.test.probabilities <- predict(model.linear.discriminant.analysis, newdata=test[,-N])$posterior #it shows the MAP - maximum a posteriori probability for each testing observation


#Visualizing the results through ROC plot and AUC values.
##### ROC CURVE #####
svms = prediction(prediction.svm.linear.test, test$BK)
svmrs = prediction(prediction.svm.rbf.test, test$BK)
bsts = prediction(prediction.boosting.test, test$BK)
bags = prediction(prediction.bagging.test, test$BK)
rfs = prediction(prediction.random.forest.test, test$BK)
nns <- prediction(prediction.neural.network.test, test$BK)
lrs <- prediction(prediction.logistic.regression.test, test$BK)
ldas <- prediction(prediction.linear.discriminant.analysis.test, test$BK)
perf.svm = performance(svms, "tpr", "fpr")
perf.svmr = performance(svmrs, "tpr", "fpr")
perf.bst = performance(bsts, "tpr", "fpr")
perf.bag = performance(bags, "tpr", "fpr")
perf.rf = performance(rfs, "tpr", "fpr")
perf.nn = performance(nns, "tpr", "fpr")
perf.lr = performance(lrs, "tpr", "fpr")
perf.lda = performance(ldas, "tpr", "fpr")

pdf("ROC.pdf") 
plot(perf.svm, col = 2, lty = 2)
abline(0,1, lty = 8, col = "grey")
plot(perf.svmr, add = TRUE, col = 2)
plot(perf.bst, add = TRUE, col = 3)
plot(perf.bag, add = TRUE, col = 4)
plot(perf.rf, add = TRUE, col = 5)
plot(perf.nn, add = TRUE, col = 6) 
plot(perf.lr, add = TRUE, col = 7) 
plot(perf.lda, add = TRUE, col = 7, lty = 2) 
legend(0.7, 0.36, c("SVMLin", "SVM-RBF", "Boosting", "Bagging", "Random Forest", "NeuralNet", "Logit", "LDA"), col = c(2,2,3,4,5,6,7,7), lty = c(2,1,1,1,1,1,1,2), merge = TRUE, bg = "gray99")
dev.off()

#####Other plots - The authors suggest to save other printing plot before running them. #####
#dev.new()
#plot(model.svm.rbf.factor, test, X1 ~ GS, xlim=range(-1:10), ylim=range(-10:1))
#library(rgl)
#plot3d(train[,c(3,5,10)], col=train$BK+2)
pdf("boostingvarplot.pdf")
varplot(model.boosting) # shows the relationship between descriptors and the response. Variable importance measure is based on model improvement.
dev.off()
pdf("RFvarplot.pdf")
varImpPlot(model.random.forest) 
dev.off()
pdf("RFerror.pdf")
plot(model.random.forest) #error
dev.off()
pdf("LRplots.pdf")
plot(model.logistic.regression) #many plots about LR
#LR coeffs can be viewed by printing model
dev.off()
pdf("LDAplot.pdf")
plot(model.linear.discriminant.analysis,test) #It's a barplot
dev.off()

#LDA coeffs can be viewed by printing model
#getTree(model.random.forest)

##### Results: AUC Evaluation, classification data, and errors #####
aucsvm=performance(svms, 'auc' )
aucsvmr=performance(svmrs, 'auc' )
aucbst=performance(bsts, 'auc' )
aucbag=performance(bags, 'auc' )
aucrf=performance(rfs, 'auc' )
aucnn=performance(nns, 'auc' )
auclr=performance(lrs, 'auc' )
auclda=performance(ldas, 'auc' )

Models=c("SVMLin", "SVM-RBF", "Boosting", "Bagging", "Random Forest","Neural Networks", "Logit","LDA")

TP <-c(confusion.matrix.svm.linear.test.class[1,1], confusion.matrix.svm.rbf.test.class[1,1], confusion.matrix.boosting.test.class[1,1],confusion.matrix.bagging.test.class[1,1], confusion.matrix.random.forest.test.class[1,1],confusion.matrix.neural.network.test.class[1,1], confusion.matrix.logistic.regression.test.class[1,1],confusion.matrix.linear.discriminant.analysis.test.class[1,1])


TN <-c(confusion.matrix.svm.linear.test.class[2,2], confusion.matrix.svm.rbf.test.class[2,2], confusion.matrix.boosting.test.class[2,2],confusion.matrix.bagging.test.class[2,2], confusion.matrix.random.forest.test.class[2,2],confusion.matrix.neural.network.test.class[2,2], confusion.matrix.logistic.regression.test.class[2,2],confusion.matrix.linear.discriminant.analysis.test.class[2,2])

FN <-c(confusion.matrix.svm.linear.test.class[1,2], confusion.matrix.svm.rbf.test.class[1,2], confusion.matrix.boosting.test.class[1,2],confusion.matrix.bagging.test.class[1,2], confusion.matrix.random.forest.test.class[1,2],confusion.matrix.neural.network.test.class[1,2], confusion.matrix.logistic.regression.test.class[1,2],confusion.matrix.linear.discriminant.analysis.test.class[1,2])

FP <-c(confusion.matrix.svm.linear.test.class[2,1], confusion.matrix.svm.rbf.test.class[2,1], confusion.matrix.boosting.test.class[2,1],confusion.matrix.bagging.test.class[2,1], confusion.matrix.random.forest.test.class[2,1],confusion.matrix.neural.network.test.class[2,1], confusion.matrix.logistic.regression.test.class[2,1],confusion.matrix.linear.discriminant.analysis.test.class[2,1])


TError_I <- c( round(100-TP[1]*100/(TP[1]+FN[1]),2), round(100-TP[2]*100/(TP[2]+FN[2]),2), round(100-TP[3]*100/(TP[3]+FN[3]),2),round(100-TP[4]*100/(TP[4]+FN[4]),2),round(100-TP[5]*100/(TP[5]+FN[5]),2), round(100-TP[6]*100/(TP[6]+FN[6]),2),round(100-TP[7]*100/(TP[7]+FN[7]),2),round(100-TP[8]*100/(TP[8]+FN[8]),2))

TError_II <- c( round(100-TN[1]*100/(TN[1]+FP[1]),2),round(100-TN[2]*100/(TN[2]+FP[2]),2),round(100-TN[3]*100/(TN[3]+FP[3]),2),round(100-TN[4]*100/(TN[4]+FP[4]),2),round(100-TN[5]*100/(TN[5]+FP[5]),2), round(100-TN[6]*100/(TN[6]+FP[6]),2),round(100-TN[7]*100/(TN[7]+FP[7]),2),round(100-TN[8]*100/(TN[8]+FP[8]),2))

ACC <- c( round((TP[1]+TN[1])*100/(TP[1]+FP[1]+TN[1]+FN[1]),2),round((TP[2]+TN[2])*100/(TP[2]+FP[2]+TN[2]+FN[2]),2), round((TP[3]+TN[3])*100/(TP[3]+FP[3]+TN[3]+FN[3]),2),round((TP[4]+TN[4])*100/(TP[4]+FP[4]+TN[4]+FN[4]),2), round((TP[5]+TN[5])*100/(TP[5]+FP[5]+TN[5]+FN[5]),2),round((TP[6]+TN[6])*100/(TP[6]+FP[6]+TN[6]+FN[6]),2), round((TP[7]+TN[7])*100/(TP[7]+FP[7]+TN[7]+FN[7]),2),round((TP[8]+TN[8])*100/(TP[8]+FP[8]+TN[8]+FN[8]),2))

AUC=c(round(aucsvm@y.values[[1]]*100,2),round(aucsvmr@y.values[[1]]*100,2), round(aucbst@y.values[[1]]*100,2), round(aucbag@y.values[[1]]*100,2),round(aucrf@y.values[[1]]*100,2), round(aucnn@y.values[[1]]*100,2),round(auclr@y.values[[1]]*100,2),round(auclda@y.values[[1]]*100,2))
##### The contents of training confusion matrices could be seen for next lines #####
confusion.matrix.svm.linear.train.class
confusion.matrix.svm.rbf.train.class
confusion.matrix.boosting.train.class
confusion.matrix.bagging.train.class
confusion.matrix.random.forest.train.class
confusion.matrix.neural.network.train.class
confusion.matrix.logistic.regression.train.class 
confusion.matrix.linear.discriminant.analysis.train.class

##### Printing Confusion matrices for all models after tests #####
confusion.matrix.svm.linear.test.class
confusion.matrix.svm.rbf.test.class
confusion.matrix.boosting.test.class
confusion.matrix.bagging.test.class
confusion.matrix.random.forest.test.class
confusion.matrix.neural.network.test.class
confusion.matrix.logistic.regression.test.class
confusion.matrix.linear.discriminant.analysis.test.class

##### Recording data #####
Resultstest=data.frame(Models, TP, TN, FP, FN, TError_I,TError_II, AUC, ACC)
write.csv(Resultstest, file = "Results-test.csv") # Write the data to a new file. There are several options available; see the help (use ?write.csv). Important! Don't forget to know the file location. Before to run this command, go to MISC >> CHANGE THE WORKING DIRECTORY to confirm which folder you are working.

trainresults=data.frame(train$BK,prediction.svm.linear.train.class,prediction.svm.rbf.train.class, round(prediction.boosting.train.probabilities[,2],0), prediction.bagging.train.class, prediction.random.forest.train.class, prediction.neural.network.train.class, prediction.logistic.regression.train.class, prediction.linear.discriminant.analysis.train.class)
colnames(trainresults)[c(2,3,4,5,6,7,8,9)]=c("SVM.Lin","SVM.RBF","Boost","Bagg","Rand.F","NNet","Logit","D.An.") #please, confirm these names!
trainresults=replace(trainresults, trainresults==0, -1) #replace 0 in BK to 1 in case of NBK firms.

testresults=data.frame(test$BK,prediction.svm.linear.test.class,prediction.svm.rbf.test.class, round(prediction.boosting.test.probabilities[,2],0), prediction.bagging.test.class, prediction.random.forest.test.class, prediction.neural.network.test.class, prediction.logistic.regression.test.class, prediction.linear.discriminant.analysis.test.class)
colnames(testresults)[c(2,3,4,5,6,7,8,9)]=c("SVM.Lin","SVM.RBF","Boost","Bagg","Rand.F","NNet","Logit","D.An.") #please, confirm these names!
testresults=replace(testresults, testresults==0, -1) #replace 0 for 1 in any case.

trainPD=data.frame(train$BK,round(attr(prediction.svm.linear.train.probabilities, "probabilities")[,1],4)*100, round(attr(prediction.svm.rbf.train.probabilities, "probabilities")[,1],4)*100, round(prediction.boosting.train.probabilities[,1],4)*100, round(prediction.bagging.train.probabilities[,1],4)*100, round(prediction.random.forest.train.probabilities[,1],4)*100, round(1-prediction.neural.network.train,4)*100, round(1-prediction.logistic.regression.train,4)*100, round(prediction.linear.discriminant.analysis.train.probabilities[,1],4)*100)
colnames(trainPD)[c(2,3,4,5,6,7,8,9)]=c("SVM.Lin","SVM.RBF","Boost","Bagg","Rand.F","NNet","Logit","D.An.") #please, confirm these names!

testPD=data.frame(test$BK,round(attr(prediction.svm.linear.test.probabilities, "probabilities")[,1],4)*100, round(attr(prediction.svm.rbf.test.probabilities, "probabilities")[,1],4)*100, round(prediction.boosting.test.probabilities[,1],4)*100, round(prediction.bagging.test.probabilities[,1],4)*100, round(prediction.random.forest.test.probabilities[,1],4)*100, round(1-prediction.neural.network.test,4)*100, round(1-prediction.logistic.regression.test,4)*100, round(prediction.linear.discriminant.analysis.test.probabilities[,1],4)*100)
colnames(testPD)[c(2,3,4,5,6,7,8,9)]=c("SVM.Lin","SVM.RBF","Boost","Bagg","Rand.F","NNet","Logit","D.An.") #please, confirm these names!

write.csv(train, file = "trainmatrix-test.csv") # record training data
write.csv(test, file = "testmatrix-test.csv") # record testing data
write.csv(trainresults, file = "trainresults-test.csv") # record training results
write.csv(testresults, file = "testresults-test.csv") # record testing results

##### Printing Probabilities of the tests - The authors suggests run those command lines only if you are interesting to see those ones, because the matrices are huge. #####
write.csv(trainPD, file = "trainPDresults-test.csv") # record PD of training data
write.csv(testPD, file = "testPDresults-test.csv") # record PD of testing data

##### Printing Models Parameters #####
print(model.svm.linear)
print(model.svm.rbf)
print(model.boosting)
print(model.bagging)
print(model.random.forest)
print(model.neural.network)
print(model.logistic.regression)
print(model.linear.discriminant.analysis)
#after running, print the R Console