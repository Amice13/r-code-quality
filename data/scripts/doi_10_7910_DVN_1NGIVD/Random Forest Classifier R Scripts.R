library(sp)
library(rgdal)
library(raster)
library(reshape)
library(grid)
library(gridExtra)
library(RStoolbox) 
library(caret)
library(rasterVis)
library(corrplot)
library(doParallel)
library(rpart.plot)
library(NeuralNetTools)
library(readxl)
library(e1071)  # Library required for SVM

#======================================================================================
# 1.0 Load xcel file into R for Training and Validation
#======================================================================================

TRAINING <- as.data.frame (read_excel("C:/Users/GISBOSS/Google Drive/R Codes/training.xlsx")) #Load excel file as dataframe

head(TRAINING)

TRAINING$Class <- as.factor(TRAINING$Class) # The Classification variable field converted to factors

str(TRAINING)
summary(TRAINING)

#======================================================================================
# 2.0 Split field data set into Training and Validation 
#======================================================================================

set.seed(27)
DSplit<-createDataPartition(TRAINING$Class,p=0.3,list = FALSE)

TrainingData<-TRAINING[-DSplit,]

ValidationData<-TRAINING[DSplit,]

nrow(TrainingData)
nrow(ValidationData)

summary(TrainingData)
summary(ValidationData)

str(TrainingData)


#======================================================================================
# 3.0 Set-up model tuning parameters
#======================================================================================

set.seed(123)
fitControl<- trainControl(method = "repeatedcv", number = 5, repeats = 5)



#======================================================================================
# 4.0 train RF classifier
#======================================================================================

# Use the train function from caret library
# Define "class ~ .," which denotes a formulae for using all predictor variable from the training data (i.e. data = TrainingData)


RF_model <- train(Class~., data = TrainingData,method= "rf", trControl=fitControl,prox=TRUE,fitBest=FALSE,returnData=TRUE)
print(RF_model)
plot(RF_model)
RF_model$finalModel

RF_VarImp<-varImp(RF_model,compete = FALSE)#Plot ANN variable importance

# plot(RF_VarImp)


#======================================================================================
# 5.0 Accuracy Assessment of RF Classifier using validation data to generate confuxion matrix/Error Matrix
#======================================================================================

Predict_RF<- predict(RF_model,ValidationData) #Accuracy Assessment of Classification with tuned svm model

confusionMatrix(Predict_RF,ValidationData$Class)

MatTabRF <- table(Predict_RF, ValidationData$Class)
MatTabRF

#Overall Accuracy
OA_RF<- sum(diag(MatTabRF))/sum(MatTabRF)
OA_RF<- OA_RF * 100
OA_RF

#User Accuracy
UA_RF <- (diag(MatTabRF)/rowSums(MatTabRF))*100
UA_RF

#Producer Accuracy
PA_RF <- (diag(MatTabRF)/colSums(MatTabRF))*100
PA_RF







