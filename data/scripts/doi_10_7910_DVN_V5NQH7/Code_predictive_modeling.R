#### Predictive Modeling in Marketing: Ensemble Methods for Response Modeling ####
#### Gabriela Alves Werb, Martin Schmidberger ####
#### Please cite as 

### Alves Werb, G., & Schmidberger, M. (2021). 
### Predictive Modeling in Marketing: Ensemble Methods for Response Modeling. 
### Die Unternehmung, 75(3), 376-396. doi:10.5771/0042-059X-2021-3-376


#### Set a Working Directory ####

folder<-choose.dir()
setwd(folder)

# Alternatively:
# where in "..." you insert the path to the directory where you saved your copy of the data set and want your files to be saved. 
# setwd("C:\\...") 

#### Load Packages ####


# if some of these are not installed yet, install them using install.packages("name of the library")

library(Hmisc) # descriptive statistics
library(sjlabelled) # label variables
library(corrplot) # for correlation plots
library(rpart) # for decision trees
library(rpart.plot) # pretty tree plots
library(randomForest) # for random forest
library(Matrix) # make sparse matrices for xgboost
library(xgboost) # for extreme gradient boosting
library(igraph) # plot deepness for xgboost
library(Ckmeans.1d.dp) # required for variable importance clustering in xgboost
library(data.table) # use an enhanced version of data frames
library(dplyr) # data handling
library(ggplot2) # data visualization
library(ggpubr)  # data visualization
library(scales) # define axis scales
library(mlr) # for hyperparameter tuning
library(parallelMap) # enable parallel computing
library(parallel) # enable parallel computing
library(caret) # generate confusion matrices
library(checkmate) # use for assertions in the prauc measure
# library(pROC) # ROC curves - load later, otherwise it generates conflicts with the mlr auc function
library(iml) # Interpret the models
library(mmpf) # Generate ICE data
library(tibble) # data manipulation
library(lime) # LIME explanations

#### Load Dataset ####

load("xsell.RData")

#### Preparing the Data ####

# Convert character variables to factors
xsell[sapply(xsell, is.character)] <- lapply(xsell[sapply(xsell, is.character)], as.factor) 

# Create a function to identify binary variables
is.binary<-function(var){return(length(unique(var))==2)}

# Convert binary variables to factors
xsell[sapply(xsell, is.binary)] <- lapply(xsell[sapply(xsell, is.binary)], as.factor) 

# Convert dataset to data.table format
xsell<-data.table(xsell)

# Convert segment variables to factors
xsell <- xsell[,grep("ext_",colnames(xsell), value=TRUE):=lapply(.SD, as.factor),.SDcols=patterns("ext_")]

#### Splitting the Data Set in Train and Test ####

# Set a seed for the random number generator to make results reproducible
set.seed(1234) 

# Randomly select 80% of the observations to be in the training data
train_id <- sample(1:nrow(xsell), size = floor(0.8 * nrow(xsell)), replace=FALSE)

# Split the dataset in training and test data (80/20)
train<-xsell[train_id,]
test<-xsell[-train_id,]

# Check whether the dependent variable has the same distribution in both data sets
Hmisc::describe(train$cross_buy)
Hmisc::describe(test$cross_buy)

### Undersample the Non-Cross-Buyers in the Training Data to Get a 50/50 Distribution ####
set.seed(1234)
train_under <- xsell %>% dplyr::filter(cross_buy==0) %>% dplyr::slice_sample(n=10000) %>% 
  rbind(xsell[cross_buy==1]) %>% data.table() # create the train data set, 80% of the original data

Hmisc::describe(train_under)

### Understanding the Data ####

head(xsell) # See the first six rows of the data set
str(xsell) # Overview of all variables in the data set 
summary(xsell) # Quick descriptive statistics 
Hmisc::describe(xsell) # More detailed descriptive statistics (incl. missings)

# Create a correlation plot with the numeric features
corrplot::corrplot(cor(xsell[,sapply(xsell,is.numeric),with=FALSE], # Get only numeric variables in the dataset
                       use="pairwise.complete.obs")) 

# Share of NAs
100*sapply(xsell, function(x) sum(is.na(x)))/dim(xsell)[1]

### Building the Models ####

# Common configuration for all methods

# Define 10-fold cross-validation
resamp<-mlr::makeResampleDesc("CV", iters = 10)

# Define a random search over the grids of max. 100 models
control_param<-mlr::makeTuneControlRandom(maxit=50)


# Define measure for area under the PR-Curve
prauc_fun <- function(truth, prob, positive, na_value = NaN, ...) {
  # Function adapted from mlr3measures
  # Source: https://rdrr.io/github/mlr-org/mlr3measures/src/R/binary_prauc.R
  assert_binary = function(truth, response = NULL, prob = NULL, positive = NULL, na_value = NULL) {
    assertFactor(truth, min.len = 1L, n.levels = 2L, any.missing = FALSE)
    
    if (!is.null(response)) {
      checkmate::assertFactor(response, len = length(truth), any.missing = FALSE)
      checkmate::assertSetEqual(levels(truth), levels(response), ordered = TRUE)
    }
    
    if (!is.null(prob)) {
      checkmate::assertNumeric(prob, len = length(truth), lower = 0, upper = 1, any.missing = FALSE)
    }
    
    if (!is.null(positive)) {
      checkmate::assertString(positive)
      checkmate::assertChoice(positive, levels(truth))
    }
    
    checkmate::assertNumber(na_value, na.ok = TRUE, null.ok = TRUE)
  }
  
  assert_binary(truth, prob = prob, positive = positive, na_value = na_value)
  
  i = which(truth == positive)
  n_pos = length(i)
  n_neg = length(truth) - n_pos
  
  if (n_pos == 0L || n_neg == 0L)
    return(na_value)
  
  truth <- ifelse(truth == positive, 1, 0)
  PRROC::pr.curve(
    scores.class0 = prob,
    weights.class0 = truth
  )[[2]]
}

prauc <- makeMeasure(id="prauc", name="Area under the Precision-Recall Curve", 
                     properties="classif", worst=0, best=1, 
                     minimize=FALSE, 
                     fun = function(task,model,pred,feats,extra.args){
                       prauc_fun(truth=getPredictionTruth(pred),
                                 prob=getPredictionProbabilities(pred),
                                 positive=pred$task.desc$positive)})

#### Tuning RPART Tree ####

# Create a classification task (one for train, other for test)
train_task_rpart<- mlr::makeClassifTask(data=as.data.frame(train_under), # train dataset
                                        target="cross_buy", # dependent variable
                                        positive="1") # level we want to predict (cross-buy yes)

test_task_rpart<- mlr::makeClassifTask(data=as.data.frame(test), # test dataset
                                       target="cross_buy", # dependent variable
                                       positive="1") # level we want to predict (cross-buy yes)

# Define a rpart learner 
rpart_learner<-mlr::makeLearner("classif.rpart", predict.type = "prob")

# Define the grid of hyperparameters
rpart_param <- ParamHelpers::makeParamSet(
  makeNumericParam("cp",lower=0.0005, upper=0.015), # cp
  makeIntegerParam("minsplit", lower=10,upper=50), # minsplit
  makeIntegerParam("maxdepth", lower=5, upper=30)) # maxdepth

# Inspect some possible combinations (parameters are drawn randomly from the defined intervals)
ParamHelpers::generateDesign(6, rpart_param)

# Start tuning 
set.seed(1234)
rpart_tune <- mlr::tuneParams(task=train_task_rpart, # train task
                              resampling=resamp, # settings for 10-fold CV
                              measures=list(prauc,tpr,f1, acc), # perf. measures
                              control=control_param, # limit num. of tuning combinations
                              learner=rpart_learner, # rpart learner ojbect
                              par.set=rpart_param, # hyperparameter space
                              show.info = TRUE) # show progress

# See the results
results_rpart<-mlr::generateHyperParsEffectData(rpart_tune, # tuning results
                                                partial.dep = TRUE) # partial dependence of the results on each hyperparameter

# Visualize how accuracy changes with the iterations
ggpubr::annotate_figure(
  ggpubr::ggarrange( # Plot for accuracy
    ggplot2::ggplot(results_rpart$data, aes(x=cp, y= tpr.test.mean, color=maxdepth)) + 
      geom_point() + ylab("Mean Recall in the Test Fold") + # Plot for Recall
      xlab("Complexity Parameter (CP)") + labs(color="Max. Tree Depth") +
      theme(axis.title = element_text(color="#666666", face="bold", size=14),
            axis.text = element_text(color="#666666", face="bold", size=11))+
      ylim(0.63,0.69), 
    ggplot2::ggplot(results_rpart$data, aes(x=cp, y= prauc.test.mean, color=maxdepth)) + geom_point() +
      ylab("Mean AUC-PR in the Test Fold") + xlab("Complexity Parameter (CP)") + 
      labs(color="Max. Tree Depth") +
      theme(legend.text=element_text(size=11), legend.text.align = 0.5,
            legend.title=element_text(color="#666666", face="bold",size=11),
            axis.title = element_text(color="#666666", face="bold", size=14),
            axis.text = element_text(color="#666666", face="bold", size=11))+
      ylim(0.63,0.69),
    ggplot2::ggplot(results_rpart$data, aes(x=cp, y= f1.test.mean, color=maxdepth)) + geom_point() +
      ylab("Mean F1-Score in the Test Fold") + xlab("Complexity Parameter (CP)") + 
      labs(color="Max. Tree Depth") +
      theme(legend.text=element_text(size=11), legend.text.align = 0.5,
            legend.title=element_text(color="#666666", face="bold",size=11),
            axis.title = element_text(color="#666666", face="bold", size=14),
            axis.text = element_text(color="#666666", face="bold", size=11))+
      ylim(0.63,0.69),
    ncol=3),
  top=ggpubr::text_grob("Tuning Results for RPART (Random Search"))

# See the best model 
rpart_tune # according to AUC-PR

# Train model with tuned hyperparameters (using AUC-PR) 
rpart_opt_prauc <- mlr::train(learner = mlr::setHyperPars(rpart_learner,
                                                          par.vals = rpart_tune$x),
                              task = train_task_rpart)

# Predictions on the test dataset with tuned hyperparameters (using AUC-PR) 
rpart_opt_prauc_predict<- predict(rpart_opt_prauc, # best tuned model
                                  test_task_rpart) # test data

# See performance values (AUC-PR, Recall and F1-score) in the test data
mlr::performance(rpart_opt_prauc_predict, measures = list(prauc, tpr, f1, acc))


### Tuning Random Forest ####

# Impute missing values using random forest proximities 
set.seed(1234)
train_under_imputed<-randomForest::rfImpute(cross_buy~.,data=train_under,iter=5,ntree=500)
test_imputed<-randomForest::rfImpute(cross_buy~.,data=test,iter=5,ntree=500)


# Create a classification task (one for train, other for test)
train_task_rf<- mlr::makeClassifTask(data=as.data.frame(train_under_imputed), # train dataset
                                     target="cross_buy", # dependent variable
                                     positive="1") # level we want to predict (cross sell yes)

test_task_rf<- mlr::makeClassifTask(data=as.data.frame(test_imputed), # test dataset
                                    target="cross_buy", # dependent variable
                                    positive="1") # level we want to predict (cross sell yes)

# Define a random forest learner
rf_learner<-mlr::makeLearner("classif.randomForest", predict.type = "prob")

# Define the grid of hyperparameters
rf_param <- ParamHelpers::makeParamSet(
  makeIntegerParam("mtry",lower=6, upper=30), # mtry
  makeIntegerParam("ntree", lower=100,upper=1000), # ntree
  makeIntegerParam("nodesize", lower=5, upper=30), # nodesize
  makeIntegerParam("maxnodes", lower=5, upper=500)) # maxnodes

# Inspect some possible combinations (random draws from the defined intervals)
ParamHelpers::generateDesign(6, rf_param)

# Set parameters for parallel computing
parallelStartSocket(cpus = detectCores()-1)
parallelExport("prauc_fun","prauc")
parallelLibrary("checkmate")
parallelLibrary("PRROC")

# Start tuning 
set.seed(1234)
rf_tune <- mlr::tuneParams(task=train_task_rf, # random forest train task
                           resampling=resamp,# settings for 10-fold CV
                           measures=list(prauc,tpr,f1, acc), # performance measures
                           control=control_param, # limit num. of tuning combinations
                           learner=rf_learner, # random forest learner object
                           par.set=rf_param, # hyperparameter space
                           show.info = TRUE) # show progress

# Stop parallel computing (relsease the clusters)
parallelStop()

# See the best model
rf_tune

# See the results
results_rf<-mlr::generateHyperParsEffectData(rf_tune, # tuning results
                                             partial.dep = TRUE) # partial dependence of the results on each hyperparameter

#### Figure 2: Mean Area Under the Precision-Recall Curve (AUC-PR) in the Test Folds for Selected Hyperparameter Combinations of the Random Forest #####

# Set theme in grayscale
ggplot2::theme_set(theme_bw())

ggpubr::annotate_figure(
  ggpubr::ggarrange( # Plot for accuracy
    ggplot2::ggplot(results_rf$data, aes(x=maxnodes, y= prauc.test.mean, color=ntree)) + 
      geom_point() + ylab("Mean AUC-PR in the Test Fold") + xlab("Max. Number of Terminal Nodes") + 
      scale_color_gradient2(low="grey90", high="black") +
      labs(color="Number of Trees") +
      theme(legend.position="none", # leave only second legend
            axis.title = element_text(color="#666666", face="bold", size=16),
            axis.text = element_text(color="#666666", face="bold", size=14))+
      ylim(0.72,0.76),
    ggplot2::ggplot(results_rf$data, aes(x=mtry, y= prauc.test.mean, color=ntree)) + geom_point() +
      ylab("Mean AUC-PR in the Test Fold") + xlab("Variables Randomly Selected for a Split") +
      scale_color_gradient2(low="grey90", high="black") +
      labs(color="Number of Trees") +
      theme(legend.text=element_text(size=15), legend.text.align = 0.5,
            legend.title=element_text(color="#666666", face="bold",size=15),
            axis.title = element_text(color="#666666", face="bold", size=16),
            axis.text = element_text(color="#666666", face="bold", size=14))+
      ylim(0.72,0.76),
    ncol=2, common.legend = TRUE, legend="bottom"),
  top=ggpubr::text_grob("Tuning Results for Random Forest (Random Search)",color="#666666", face="bold",size=16))

ggsave(file="tuning_rf.jpeg", width= 30, height=20, units="cm", dpi=400)

# See the best model 
rf_tune # according to AUC-PR
best_rf_parms <- data.table(results_rf$data)[prauc.test.mean==max(prauc.test.mean),] 
best_rf_parms 

# Train model with tuned hyperparameters (using AUC-PR) 
rf_learner$par.vals<-list(importance=TRUE) # include importance measure to generate var. importance later

set.seed(1234) # ensure reproducibility
rf_opt_prauc <- mlr::train(learner = mlr::setHyperPars(rf_learner, # random forest learner
                                                       par.vals = rf_tune$x), # tuned hyperparameters
                           task = train_task_rf) # rf train classification task

# Predictions on the test data set with tuned hyperparameters (using AUC-PR) 
rf_opt_prauc_predict<- predict(rf_opt_prauc, # best tuned model
                               test_task_rf) # test data

# See performance values (AUC-PR, Recall and F1-score) in the test data
mlr::performance(rf_opt_prauc_predict, measures = list(prauc, tpr, f1, acc))

### Does another threshold (instead of 0.5) deliver better results?

# Resample a model trained with the optimal pararameters
set.seed(1234)
rf_best<-mlr::resample(mlr::makeLearner("classif.randomForest",predict.type = "prob",
                                        par.vals = list(mtry=best_rf_parms$mtry, ntree=best_rf_parms$ntree, 
                                                        nodesize=best_rf_parms$nodesize, maxnodes=best_rf_parms$maxnodes)), 
                       train_task_rf, 
                       resamp,
                       measures = list(prauc, tpr,f1, acc), 
                       show.info = TRUE)

# Generate threshold analysis
rf_thresholds_prauc<-mlr::generateThreshVsPerfData(rf_best, measures = list(prauc, tpr, f1, acc))

#### Plot the result 
# mlr::plotThreshVsPerf(rf_thresholds_prauc) + ylab("Mean AUC-PR in the Test Dataset") +
#   xlab("Predicted Probability Threshold to Classify\na Customer as a Cross-Buyer") +
#   theme(axis.title = element_text(color="#666666", face="bold", size=14),
#         axis.text = element_text(color="#666666", face="bold", size=11),
#         strip.text = element_text(size = 11))+ylim(0,0.8)

# Extract optimal threshold 
rf_threshold<-data.table(rf_thresholds_prauc$data)[f1==max(f1)][threshold==max(threshold)]$threshold 

# See if the performance changes under this optimal threshold 
mlr::performance(predict(rf_opt_prauc, # best tuned model
                         test_task_rf, # test prediction task
                         threshold=rf_threshold), # new threshold
                 measures = list(prauc, tpr, f1, acc)) # performance measures

### Tuning Random Forest with Rough Imputation ####

# Impute missing values using median values and most frequent categories 

train_under_imputed_rough<-randomForest::na.roughfix(train_under)
test_imputed_rough<-randomForest::na.roughfix(test)

# Create a classification task (one for train, other for test)
train_task_rf_rough<- mlr::makeClassifTask(data=as.data.frame(train_under_imputed_rough), # train dataset
                                           target="cross_buy", # dependent variable
                                           positive="1") # level we want to predict (cross sell yes)

test_task_rf_rough<- mlr::makeClassifTask(data=as.data.frame(test_imputed_rough), # test dataset
                                          target="cross_buy", # dependent variable
                                          positive="1") # level we want to predict (cross sell yes)

# Define a random forest learner
rf_learner<-mlr::makeLearner("classif.randomForest", predict.type = "prob")

# Define the grid of hyperparameters
rf_param <- ParamHelpers::makeParamSet(
  makeIntegerParam("mtry",lower=6, upper=30), # mtry
  makeIntegerParam("ntree", lower=100,upper=1000), # ntree
  makeIntegerParam("nodesize", lower=5, upper=30), # nodesize
  makeIntegerParam("maxnodes", lower=5, upper=500)) # maxnodes

# Inspect some possible combinations (random draws from the defined intervals)
ParamHelpers::generateDesign(6, rf_param)

# Set parameters for parallel computing
parallelStartSocket(cpus = detectCores()-1)
parallelExport("prauc_fun","prauc")
parallelLibrary("checkmate")
parallelLibrary("PRROC")

# Start tuning 
set.seed(1234)
rf_tune_rough <- mlr::tuneParams(task=train_task_rf_rough, # random forest train task
                                 resampling=resamp,# settings for 10-fold CV
                                 measures=list(prauc,tpr,f1, acc), # performance measures
                                 control=control_param, # limit num. of tuning combinations
                                 learner=rf_learner, # random forest learner object
                                 par.set=rf_param, # hyperparameter space
                                 show.info = TRUE) # show progress

# Stop parallel computing (relsease the clusters)
parallelStop()

# See the best model
rf_tune_rough

# See the results
results_rf_rough<-mlr::generateHyperParsEffectData(rf_tune_rough, # tuning results
                                                   partial.dep = TRUE) # partial dependence of the results on each hyperparameter

# See the best model 
rf_tune_rough # according to AUC-PR
best_rf_parms_rough <- data.table(results_rf_rough$data)[prauc.test.mean==max(prauc.test.mean),] 

# Train model with tuned hyperparameters (using accuracy) 
rf_learner$par.vals<-list(importance=TRUE) # include importance measure to generate var. importance later

set.seed(1234) # ensure reproducibility
rf_opt_prauc_rough <- mlr::train(learner = mlr::setHyperPars(rf_learner, # random forest learner
                                                             par.vals = rf_tune_rough$x), # tuned hyperparameters
                                 task = train_task_rf_rough) # rf train classification task

# Predictions on the test data set with tuned hyperparameters (using AUC-PR) 
rf_opt_prauc_predict_rough<- predict(rf_opt_prauc_rough, # best tuned model
                                     test_task_rf_rough) # test data

# See performance values (AUC-PR, Recall and F1-score) in the test data
mlr::performance(rf_opt_prauc_predict_rough, measures = list(prauc, tpr, f1, acc))

#### Extreme Gradient Boosting #### 

# Create a classification task (one for train, other for test)
train_task_xgb<- mlr::makeClassifTask(data=as.data.frame(train_under), # train data set
                                      target="cross_buy", # dependent variable
                                      positive="1") # level we want to predict (cross sell yes)

test_task_xgb<- mlr::makeClassifTask(data=as.data.frame(test), # test data set
                                     target="cross_buy", # dependent variable
                                     positive="1") # level we want to predict (cross sell yes)

# Do again one-hot encoding for xgb
train_task_xgb<- mlr::createDummyFeatures(obj=train_task_xgb) 
test_task_xgb<- mlr::createDummyFeatures(obj=test_task_xgb) 

# Define an extreme gradient boosting
xgb_learner<-mlr::makeLearner("classif.xgboost", 
                              predict.type = "prob",
                              eval_metric="error",
                              objective="binary:logistic",
                              booster="gbtree",
                              missing=NA,
                              verbose=2,
                              early_stopping_rounds=10)

# Define the grid of hyperparameters
xgb_param <- ParamHelpers::makeParamSet(makeNumericParam("lambda",lower=0.1, upper=1), # lambda
                                        makeNumericParam("eta", lower=0.05,upper=0.3), # eta
                                        makeIntegerParam("max_depth", lower=1, upper=6), # max. depth
                                        makeIntegerParam("nrounds", lower=100, upper=1000),# num. of trees
                                        makeNumericParam("min_child_weight", lower=1, upper=10)) # min. child weight


# Inspect some possible combinations (random draws from the defined intervals)
ParamHelpers::generateDesign(6, xgb_param)

# Set parameters for parallel computing
parallelStartSocket(cpus = detectCores()-1)
parallelExport("prauc_fun","prauc")
parallelLibrary("checkmate")
parallelLibrary("PRROC")

# Start tuning 
set.seed(1234)
xgb_tune <- mlr::tuneParams(task=train_task_xgb, # xgb learning task
                            resampling=resamp, # settings for 10-fold CV
                            measures=list(prauc, tpr, f1), # performance measures
                            control=control_param, # limit num. of tuning combinations
                            learner=xgb_learner,  # rpart learner ojbect
                            par.set=xgb_param, # hyperparameter space
                            show.info = TRUE) # show progress

# Stop parallel computing (release the clusters)
parallelStop()

# See the best model
xgb_tune

# See the results
results_xgb<-mlr::generateHyperParsEffectData(xgb_tune, # tuning results
                                              partial.dep = TRUE) # partial dependence of the results on each hyperparameter

# Visualize how accuracy changes with the iterations
# ggpubr::annotate_figure(
#   ggpubr::ggarrange( # Plot for accuracy
#     ggplot2::ggplot(results_xgb$data, aes(x=max_depth, y= acc.test.mean, color=eta)) +
#       geom_point() + ylab("Mean Accuracy in the Test Fold") + # Plot for accuracy
#       xlab("Max. Tree Depth") + labs(color="Learning Rate") +
#       theme(legend.text=element_text(size=11), legend.text.align = 0.5,
#             legend.title=element_text(color="#666666", face="bold",size=11),
#             axis.title = element_text(color="#666666", face="bold", size=14),
#             axis.text = element_text(color="#666666", face="bold", size=11)),
#     ggplot2::ggplot(results_xgb$data, aes(x=max_depth, y= acc.test.mean, color=lambda)) + geom_point() +
#       ylab("Mean Accuracy in the Test Fold") +
#       xlab("Max. Tree Depth") + labs(color="L1-Shrinkage") +
#       theme(legend.text=element_text(size=11), legend.text.align = 0.5,
#             legend.title=element_text(color="#666666", face="bold",size=11),
#             axis.title = element_text(color="#666666", face="bold", size=14),
#             axis.text = element_text(color="#666666", face="bold", size=11)),
#     ncol=2),  top=text_grob("Tuning Results for Extreme Gradient Boosting (Random Search)"))

# See the best model 
xgb_tune # according to AUC-PR

# Train model with tuned hyperparameters (using AUC-PR) 
set.seed(1234) # ensure reproducibility
xgb_opt_prauc <- mlr::train(learner = mlr::setHyperPars(xgb_learner,
                                                        par.vals = xgb_tune$x),
                            task = train_task_xgb)

# Predictions on the test data set with tuned hyperparameters (using AUC-PR) 
xgb_opt_prauc_predict<- predict(xgb_opt_prauc, # best tuned model
                                test_task_xgb) # test data

# See performance values (AUC-PR, Recall and F1-score) in the test data
mlr::performance(xgb_opt_prauc_predict, measures = list(prauc, tpr, f1))

#### Logistic Regression ####

### Using the data set imputed with random forest proximities

# Create a classification task (one for train, other for test)
train_task_log_rfimp<- mlr::makeClassifTask(data=as.data.frame(train_under_imputed), # train data set
                                            target="cross_buy", # dependent variable
                                            positive="1") # level we want to predict (cross sell yes)

test_task_log_rfimp<- mlr::makeClassifTask(data=as.data.frame(test_imputed), # test data set
                                           target="cross_buy", # dependent variable
                                           positive="1") # level we want to predict (cross sell yes)

# Define a logistic regression learner
log_learner<-mlr::makeLearner("classif.logreg", predict.type = "prob")

# Fit model 
log_rfimp <- mlr::train(learner = log_learner, # logistic regression learner
                        task = train_task_log_rfimp) # logistic regression classification task

getLearnerModel(log_rfimp)

# Cross-validation to estimate error of the logistic regression
set.seed(1234)
log_rfimp_cv<-mlr::resample(log_learner, 
                            train_task_log_rfimp, 
                            resamp,
                            measures = list(prauc, tpr,f1, acc), 
                            show.info = TRUE)

# See the cross-validated error for the logistic regression
log_rfimp_cv$aggr


# Predictions on the test data set 
log_rfimp_predict<- predict(log_rfimp, # model
                            test_task_log_rfimp) # test data

# See performance values (AUC-PR, Recall, F1-score, and Accuracy) in the test data
mlr::performance(log_rfimp_predict, measures = list(prauc, tpr, f1, acc))

### Using the data set imputed with median values and most frequent categories

# Create a classification task (one for train, other for test)
train_task_log_rough<- mlr::makeClassifTask(data=as.data.frame(train_under_imputed_rough), # train data set
                                            target="cross_buy", # dependent variable
                                            positive="1") # level we want to predict (cross sell yes)

test_task_log_rough<- mlr::makeClassifTask(data=as.data.frame(test_imputed_rough), # test data set
                                           target="cross_buy", # dependent variable
                                           positive="1") # level we want to predict (cross sell yes)

# Define a logistic regression learner
log_learner<-mlr::makeLearner("classif.logreg", predict.type = "prob")

# Fit model 
log_rough <- mlr::train(learner = log_learner, # logistic regression learner
                        task = train_task_log_rough) # logistic regression classification task

getLearnerModel(log_rough)

# Cross-validation to estimate error of the logistic regression
set.seed(1234)
log_rough_cv<-mlr::resample(log_learner, 
                            train_task_log_rough, 
                            resamp,
                            measures = list(prauc, tpr,f1, acc), 
                            show.info = TRUE)

# See the cross-validated error for the logistic regression
log_rough_cv$aggr


# Predictions on the test data set 
log_rough_predict<- predict(log_rough, # model
                            test_task_log_rough) # test data

# See performance values (AUC-PR, Recall, F1-score, and Accuracy) in the test data
mlr::performance(log_rough_predict, measures = list(prauc, tpr, f1, acc))


#### Evaluating the Performance of the Models ####

#### In 10-Fold CV 
cv_metrics<-round(rbind(
  # Regression test data rough imputation
  data.table(t(log_rough_cv$aggr)),
  
  # Regression test data rfimpute imputation
  data.table(t(log_rfimp_cv$aggr)),
  
  # RPART CV
  unique(data.table(results_rpart$data)[prauc.test.mean==max(prauc.test.mean), # get results with the highest AUC-PR in CV
                                        .(prauc.test.mean,tpr.test.mean,f1.test.mean, acc.test.mean)]), # get the measures
  
  # Random Forest CV with rough imputation
  data.table(results_rf_rough$data)[prauc.test.mean==max(prauc.test.mean)&ntree==best_rf_parms_rough$ntree, # get results with the highest accuracy in CV
                                    .(prauc.test.mean,tpr.test.mean,f1.test.mean, acc.test.mean)], # get the measures
  
  # Random Forest CV with rfimpute imputation
  data.table(results_rf$data)[prauc.test.mean==max(prauc.test.mean)&ntree==best_rf_parms$ntree, # get results with the highest accuracy in CV
                              .(prauc.test.mean,tpr.test.mean,f1.test.mean, acc.test.mean)], # get the measures
  
  # Gradient Boosting CV
  data.table(results_xgb$data)[prauc.test.mean==max(prauc.test.mean), # get results with the highest accuracy in CV
                               .(prauc.test.mean,tpr.test.mean,f1.test.mean, acc.test.mean)])*100,1)
colnames(cv_metrics)<-c("AUC-PR","Recall","F1 Score", "Accuracy")

### Table 6: Comparison of the Average Performance Measures of the Best Performing Models in the Test Folds ####

table_6<-round(rbind(
  
  # Regression test data rfimpute imputation
  data.table(t(log_rfimp_cv$aggr)),
  
  # RPART CV
  unique(data.table(results_rpart$data)[prauc.test.mean==max(prauc.test.mean), # get results with the highest AUC-PR in CV
                                        .(prauc.test.mean,tpr.test.mean,f1.test.mean, acc.test.mean)]), # get the measures
  
  # Random Forest CV with rfimpute imputation
  data.table(results_rf$data)[prauc.test.mean==max(prauc.test.mean)&ntree==best_rf_parms$ntree, # get results with the highest accuracy in CV
                              .(prauc.test.mean,tpr.test.mean,f1.test.mean, acc.test.mean)], # get the measures
  
  # Gradient Boosting CV
  data.table(results_xgb$data)[prauc.test.mean==max(prauc.test.mean), # get results with the highest accuracy in CV
                               .(prauc.test.mean,tpr.test.mean,f1.test.mean, acc.test.mean)])*100,1)
colnames(table_6)<-c("AUC-PR","Recall","F1 Score", "Accuracy")

#### In the test data
test_metrics<-round(rbind(
  # Regression test data rough imputation
  mlr::performance(log_rough_predict, measures = list(prauc, tpr, f1, acc)),
  
  # Regression test data rfimpute imputation
  mlr::performance(log_rfimp_predict, measures = list(prauc, tpr, f1, acc)),
  
  # RPART test data
  performance(rpart_opt_prauc_predict, measures = list(prauc, tpr, f1, acc)),
  
  # RF test data rough imputation
  mlr::performance(rf_opt_prauc_predict_rough, measures = list(prauc, tpr, f1, acc)),
  
  # RF test data rfimpute imputation
  performance(rf_opt_prauc_predict, measures = list(prauc, tpr, f1, acc)),
  
  # Gradient Boosting test data
  performance(xgb_opt_prauc_predict, measures = list(prauc, tpr, f1, acc)))*100,1) # get the measures

colnames(test_metrics)<-c("AUC-PR","Recall","F1 Score", "Accuracy")

### Table 7: Comparison of the Performance of the Best Performing Models in the Test Data Set ####

table_7<-round(rbind(
  
  # Regression test data rfimpute imputation
  mlr::performance(log_rfimp_predict, measures = list(prauc, tpr, f1, acc)),
  
  # RPART test data
  performance(rpart_opt_prauc_predict, measures = list(prauc, tpr, f1, acc)),
  
  # RF test data rfimpute imputation
  performance(rf_opt_prauc_predict, measures = list(prauc, tpr, f1, acc)),
  
  # Gradient Boosting test data
  performance(xgb_opt_prauc_predict, measures = list(prauc, tpr, f1, acc)))*100,1) # get the measures

colnames(table_7)<-c("AUC-PR","Recall","F1 Score", "Accuracy")

### Aggregate all measures for tuned models

compute_metrics<-cbind(model=c("reg_CV_rough","reg_CV_rfimp","rpart_CV",#"rf_CV_rough",
                               "rf_CV_rfimp","xgb_CV",
                               "reg_test_rough","reg_test_rfimp","rpart_test",#"rf_test_rough",
                               "rf_test_rfimp","xgb_test"),
                       rbind(cv_metrics,test_metrics))

rm(cv_metrics,test_metrics)


### Understanding Model Predictions ####


#### Variable importance for the tuned Random Forest Model ####

# Permutation importance for tuned random forest
# Calculate importance locally for each observation
set.seed(1234) # ensure reproducibility
rf_permut_imp<-mlr::generateFeatureImportanceData(train_task_rf, # train random forest task
                                                  method="permutation.importance", # permutation imp.
                                                  learner=mlr::setHyperPars(rf_learner, # learner
                                                                            par.vals = rf_tune$x),# hyperparameters
                                                  interaction=FALSE, # no joint permutation 
                                                  local=TRUE, # importance per obs.
                                                  show.info = TRUE) # show progress

# Calculate overall importance (no local breakdown)
set.seed(1234) # ensure reproducibility
rf_permut_imp_no_local<-mlr::generateFeatureImportanceData(train_task_rf, # train rf task
                                                           method="permutation.importance", # permutation imp.
                                                           learner=mlr::setHyperPars(rf_learner, # learner
                                                                                     par.vals = rf_tune$x),# hyperparameters
                                                           interaction=FALSE, # no joint permutation 
                                                           local=FALSE, # importance per obs.
                                                           show.info = TRUE) # show progress

# Compare both results
rbind(colMeans(rf_permut_imp$res),rf_permut_imp_no_local$res)

# Create factor with the contributions
imp_rf_tuned<-dplyr::tibble(vars=names(rf_permut_imp_no_local$res),imp=t(rf_permut_imp_no_local$res[1,]))
imp_rf_tuned$vars<-factor(imp_rf_tuned$vars, levels=imp_rf_tuned$vars[order(imp_rf_tuned$imp, decreasing=TRUE)])

# Now normalize as % of total contributions of all variables
imp_rf_tuned$imp_pct<-round(imp_rf_tuned$imp/sum(imp_rf_tuned$imp),2)

# Decreasing order of importance
imp_rf_tuned<-imp_rf_tuned[order(imp_rf_tuned$imp, decreasing=TRUE),]

# Get top 10 variables
imp_rf_tuned<-imp_rf_tuned[1:10,]

# Make pretty labels

imp_rf_tuned$labels<-c("Age","Last Account","Desktop Logins",
                       "Occupation","Mobile Logins","Inflows",
                       "Living Duration","Customer Tenure",
                       "City Size","Purchase Power")

# Figure 3: Raw and Percentage Permutation Variable Importance Measures for the Tuned Random Forest Model #####

ggpubr::annotate_figure(
  ggpubr::ggarrange(
    # Raw importance values
    ggplot2::ggplot(data=imp_rf_tuned, aes(x=vars, weight=imp, fill=vars)) +
      geom_bar() + ylab("Raw Permutation Importance") +
      scale_fill_grey(name = "Variables", 
                          labels = as.character(imp_rf_tuned$labels)) +
      theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
            axis.title.x=element_blank(),
            legend.text=element_text(size=14), 
            legend.title=element_text(color="#666666", face="bold",size=16),
            axis.title = element_text(color="#666666", face="bold", size=16),
            axis.text.y = element_text(color="#666666",face="bold",size=14)),
    # As a % of the total importance of all variables
    ggplot2::ggplot(data=imp_rf_tuned, aes(x=vars, weight=imp_pct, fill=vars)) +
      geom_bar() + ylab("Permutation Importance\n(% of Total Importance)") +
      scale_fill_grey(name = "Variables", 
                          labels = as.character(imp_rf_tuned$labels)) +
      scale_y_continuous(labels = scales::percent_format(accuracy=1), limits = c(0,0.2)) +
      theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
            axis.title.x=element_blank(),
            legend.text=element_text(size=14), 
            legend.title=element_text(color="#666666", face="bold",size=16),
            axis.title = element_text(color="#666666", face="bold", size=16),
            axis.text.y = element_text(color="#666666",face="bold",size=14)),
    ncol=2, common.legend = TRUE, legend="right"),  
  top=text_grob("Variable Importance for the Tuned Random Forest Model",color="#666666", face="bold",size=18))

#### Partial Dependence and ICE Plots ####

# Make a predictor object (base for all plots)
predictor_rf<-iml::Predictor$new(model=rf_opt_prauc, # trained model 
                                 data=mlr::getTaskData(train_task_rf), # random forest task (data)
                                 class = "1") # class to predict (here: cross_buy = 1)

### Effect of the most important variable: age

# Generate the data for the plot
effect_age<-iml::FeatureEffect$new(predictor=predictor_rf, # predictor object
                                   feature="age", # variable of interest
                                   method="pdp+ice") # method (here both PDP and ICE)

# Make the plot
plot(effect_age) + ylab("Effect on Predicted Cross-Buy Probability") + 
  xlab("Age") + theme(axis.title = element_text(color="#666666", face="bold", size=14))


### Effect of the second most important variable: last_acc_opening_days

# Generate the data for the plot
effect_last_acc<-iml::FeatureEffect$new(predictor=predictor_rf, # predictor object
                                        feature="last_acc_opening_days", # variable of interest
                                        method="pdp+ice") # method (here both PDP and ICE)


# Make the plot
plot(effect_last_acc) + ylab("Effect on Predicted Cross-Buy Probability") + 
  xlab("Days Since Opening the Last Account") + theme(axis.title = element_text(color="#666666", face="bold", size=14))

### Interaction Effect of the two most important variables: age and last_acc_opening_days

# Generate the data for the plot
effect_age_last_acc<-iml::FeatureEffect$new(predictor_rf, # predictor object
                                            feature=c("age","last_acc_opening_days"), # variable of interest
                                            method="pdp") # method (ICE only possible for one variable)

# Make the plot
plot(effect_age_last_acc) + ylab("Days Since Opening the Last Account") + 
  xlab("Age") + scale_fill_continuous(name = "Predicted Cross-Buy Probability")


### Figure 4: Effect of Age and Days Since Opening the Last Account on the Predicted Cross-Buy Probability ####
ggpubr::ggarrange(
  # Age
  plot(effect_age) + ylab("Predicted Cross-Buy Prob.") + 
    xlab("Age") + theme(axis.title = element_text(color="#666666", face="bold", size=14),
                        axis.text = element_text(size=14)),
  # Last Account
  plot(effect_last_acc) + ylab("Predicted Cross-Buy Probability") + 
    xlab("Days Since Opening the Last Account") + 
    theme(axis.title = element_text(color="#666666", face="bold", size=14),
          axis.text = element_text(size=14)),
  ncol=2)

### Figure 5: Average Interaction Effect between Age and Days since Opening the Last Account #####

plot(effect_age_last_acc) + ylab("Days Since Opening the Last Account") + 
  xlab("Age") + scale_fill_continuous(name = "Predicted Cross-Buy Probability") +
  theme(legend.title=element_text(size=14), legend.title.align = 0.5,
        legend.position = "bottom", legend.text=element_text(size=14),
        axis.title = element_text(color="#666666", face="bold", size=14),
        axis.text = element_text(size=14))

### Figures B1 to B8: Effect of the 3rd up to 10th most important variable (Appendix) ####

### Desktop Logins
# Generate the data for the plot
effect_desktop<-iml::FeatureEffect$new(predictor=predictor_rf, # predictor object
                                       feature="logins_desktop", # variable of interest
                                       method="pdp+ice") # method (here both PDP and ICE)
# Make the plot
plot(effect_desktop) + ylab("Effect on Predicted Cross-Buy Probability") + 
  xlab("Number of Desktop Logins") + theme(axis.title = element_text(color="#666666", face="bold", size=14),
                                           axis.text = element_text(size=14))

### Occupation
# Generate the data for the plot
effect_occ<-iml::FeatureEffect$new(predictor=predictor_rf, # predictor object
                                   feature="occupation", # variable of interest
                                   method="pdp+ice") # method (here both PDP and ICE)
# Make the plot
plot(effect_occ) + ylab("Effect on Predicted Cross-Buy Probability") + 
  xlab("Occupation") + theme(axis.title = element_text(color="#666666", face="bold", size=14),
                             axis.text = element_text(size=14)) + coord_flip()

### Mobile Logins
# Generate the data for the plot
effect_mobile<-iml::FeatureEffect$new(predictor=predictor_rf, # predictor object
                                      feature="logins_mobile", # variable of interest
                                      method="pdp+ice") # method (here both PDP and ICE)
# Make the plot
plot(effect_mobile) + ylab("Effect on Predicted Cross-Buy Probability") + 
  xlab("Number of Mobile Logins") + theme(axis.title = element_text(color="#666666", face="bold", size=14),
                                          axis.text = element_text(size=14))

### Inflows
# Generate the data for the plot
effect_inflows<-iml::FeatureEffect$new(predictor=predictor_rf, # predictor object
                                       feature="inflows", # variable of interest
                                       method="pdp+ice") # method (here both PDP and ICE)
# Make the plot
plot(effect_inflows) + ylab("Effect on Predicted Cross-Buy Probability") + 
  xlab("Inflows (€)") + theme(axis.title = element_text(color="#666666", face="bold", size=14),
                              axis.text = element_text(size=14))

### Living Duration
# Generate the data for the plot
effect_living_duration<-iml::FeatureEffect$new(predictor=predictor_rf, # predictor object
                                               feature="ext_living_duration", # variable of interest
                                               method="pdp+ice") # method (here both PDP and ICE)
# Make the plot
plot(effect_living_duration) + ylab("Effect on Predicted Cross-Buy Probability") + 
  xlab("Living Duration") + theme(axis.title = element_text(color="#666666", face="bold", size=14),
                                  axis.text = element_text(size=14))

### Customer Tenure
# Generate the data for the plot
effect_tenure<-iml::FeatureEffect$new(predictor=predictor_rf, # predictor object
                                      feature="customer_tenure_months", # variable of interest
                                      method="pdp+ice") # method (here both PDP and ICE)
# Make the plot
plot(effect_tenure) + ylab("Effect on Predicted Cross-Buy Probability") + 
  xlab("Customer Tenure (Months)") + theme(axis.title = element_text(color="#666666", face="bold", size=14),
                                           axis.text = element_text(size=14))

### City Size
# Generate the data for the plot
effect_city_size<-iml::FeatureEffect$new(predictor=predictor_rf, # predictor object
                                         feature="ext_city_size", # variable of interest
                                         method="pdp+ice") # method (here both PDP and ICE)
# Make the plot
plot(effect_city_size) + ylab("Effect on Predicted Cross-Buy Probability") + 
  xlab("City Size") + theme(axis.title = element_text(color="#666666", face="bold", size=14),
                            axis.text = element_text(size=14))

### Purchase Power
# Generate the data for the plot
effect_ppower<-iml::FeatureEffect$new(predictor=predictor_rf, # predictor object
                                      feature="ext_purchase_power", # variable of interest
                                      method="pdp+ice") # method (here both PDP and ICE)
# Make the plot
plot(effect_ppower) + ylab("Effect on Predicted Cross-Buy Probability") + 
  xlab("Purchase Power") + theme(axis.title = element_text(color="#666666", face="bold", size=14),
                                 axis.text = element_text(size=14))

### LIME - Local Model Representation ######

# Prepare data to generate sample the observations to run through LIME
sample_obs<-data.table(predictor_rf$predict(mlr::getTaskData(train_task_rf)))
colnames(sample_obs)<-"pred_prob"
sample_obs<-cbind(sample_obs,predictor_rf$data$X)
sample_obs<-sample_obs[,':='(obs_ind=1:.N, truth=train_under$cross_buy),]
sample_obs$pred_class<-ifelse(sample_obs$pred_prob>=0.5,"1","0")
sample_obs$correct<-sample_obs$pred_class==sample_obs$truth

# Get 4 candidate observations each: randomly select correct and incorrect predicted customers
# Assign row names so that we can later know which observations from the original test data set we're analyzing

# 4 random Cross-Buyers correctly predicted
set.seed(1234)
random_correct<-sample_obs[correct==TRUE&truth=="1",.SD[sample(.N, min(4,.N))]][,-c("pred_prob","truth","correct","pred_class")]
random_correct<-column_to_rownames(random_correct,var="obs_ind")

# 4 random Cross-Buyers incorrectly predicted
set.seed(1234)
random_incorrect<-sample_obs[correct==FALSE&truth=="1",.SD[sample(.N, min(4,.N))]][,-c("pred_prob","truth","correct","pred_class")]
random_incorrect<-column_to_rownames(random_incorrect,var="obs_ind")

# Get two observations: 11739 (correct) and 12421 (incorrect)
to_explain<-rownames_to_column(predictor_rf$data$X, "obs_ind")[c(11739,12421),]
to_explain<-column_to_rownames(to_explain,var="obs_ind")

# Create an explainer object with the lime function
explainer <- lime(predictor_rf$data$X, # train data (only explanatory variables)
                  rf_opt_prauc, # model to be explained
                  bin_continuous = TRUE, # bin continuous variables?
                  n_bins = 5, # number of bins for continuous variables
                  n_permutations = 200) # number of permutations

# Explain the observations
set.seed(1234)
explanation<-explain(to_explain, # observations to explain
                     # get only columns with the indep. variables
                     dist_fun = "euclidean", # use euclidean function
                     kernel_width = 0.5, # kernel width
                     explainer, # explainer object
                     n_labels = 1, # explain only one probability
                     n_features = 4) # num. of indep. vars

#### Figure 6: Explanation of the Predictions for Two Customers using Local Interpretable Model-Agnostic Explanations ####

plot_features(explanation, ncol=1) + # modified version to allow different inputs for ncol
  theme(axis.title.y=element_text(color="#666666", face="bold", size=16),
        axis.text.y=element_text(color="#666666", face="bold", size=11),
        axis.text.x=element_text(color="#666666", face="bold", size=13),
        legend.text=element_text(size=12),
        axis.title.x=element_blank(),
        strip.text.x = element_text(color="#666666",size = 11)) + 
  scale_fill_manual(values = c('black', 'grey90'), drop = FALSE) 

#### Repeat the analysis with all the cross-buyers in the test data set
# See who was classified correctly and why

# Make a predictor object for the test data 
predictor_rf_test<-iml::Predictor$new(model=rf_opt_prauc, # trained model 
                                      data=mlr::getTaskData(test_task_rf), # random forest task (data)
                                      class = "1") # class to predict (here: cross_buy = 1)

# Tag the observations in the test data set
to_explain_all<-rownames_to_column(predictor_rf_test$data$X, "case")

# Get the actual class for each observation in the test data set
actual_labels<-rownames_to_column(data.table(cross_buy=test[,cross_buy]), "case")

# Merge the test data set with the actual response
to_explain_all<-left_join(to_explain_all,actual_labels,by="case")

# Get only the cross-buyers
to_explain_pos<-to_explain_all[cross_buy==1]

# Tag the observation number
to_explain_pos<-column_to_rownames(to_explain_pos,var="case") %>% data.table()

# Remove the actual cross-buy status
to_explain_pos_nolab<-to_explain_pos[,-"cross_buy"]

parallelStartSocket(cpus = detectCores()-1, reproducible = TRUE)
clusterSetRNGStream(iseed = 1234)
set.seed(1234)

explanation_pos<-explain(to_explain_pos_nolab, # observations to explain
                         # get only columns with the indep. variables
                         dist_fun = "euclidean", # use euclidean function
                         kernel_width = 0.5, # kernel width
                         explainer, # explainer object
                         n_labels = 1, # explain only one probability
                         n_features = 4) # num. of indep. vars
parallelStop()

explanation_pos<-data.table(explanation_pos)

# Get the correct vs. incorrect predictions
explanation_pos_correct<-explanation_pos[label==1]
explanation_pos_incorrect<-explanation_pos[label==0]

### Figure 7: Explanation of the Predictions for Cross-Buyers Correctly Identified by the Model in the Test Data ####

# Plot a heatmap with the ten most relevant explanations for correctly predicted cross-buyers
# Only display those with absolute weight higher than 0.1 

top_10<-explanation_pos_correct[abs(feature_weight)>0.1,.N,by=feature_desc][order(-N)][1:10,feature_desc]

plot_explanations(explanation_pos_correct[abs(feature_weight)>0.1&feature_desc%in%top_10]) + 
  theme(axis.text.x = element_blank(), # remove the observation numbers
        axis.text.y = element_text(color="#666666", face="bold", size=11),
        axis.title.y = element_text(color="#666666", face="bold", size=14),
        axis.title.x = element_text(color="#666666", face="bold", size=14),
        legend.title = element_text(color="#666666", face="bold",size=14),
        legend.text=element_text(size=12),
        legend.text.align = 1,
        strip.text.x =element_blank()) + 
  scale_fill_gradient('Feature\nweight', low = 'grey20', high = 'grey90')

# Plot a heatmap with the explanations incorrectly predicted cross-buyers
plot_explanations(explanation_pos_incorrect)

