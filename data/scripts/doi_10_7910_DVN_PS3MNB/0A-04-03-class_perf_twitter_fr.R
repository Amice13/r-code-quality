#!/usr/bin/Rscript
##########################################################################################
# Social Media and Political Agenda Setting (Classification Evaluation Tweets FR)
##########################################################################################
# Description:
##########################################################################################
# This Script contains the code of the Final Classification model 
# using the simple regex search to classify a text as political 
# or not with a detailed word list.
#########################################################################################
# Content
#########################################################################################
# 1) Dependencies
# 2) Objects & References
## 2.1) Load w2v Model
## 2.2) Load sub-models of Ensemble Model
## 2.3) Load Ensemble sub-functions
## 2.4) Ensemble Functions
# 3) Binary Classification with list of words
# 4) Classification Automation Function
# 5) Automated Classification of Texts
## 5.1) Load Texts from SMD
## 5.2) Classify
## 5.3) Write Classification to Elastic...
#########################################################################################
# Dependencies
#########################################################################################
rm(list=ls())

# Libraries
suppressMessages(library(rjson))
suppressMessages(library(jsonlite))
suppressMessages(library(readr))
suppressMessages(library(data.table))
suppressMessages(library(dplyr))
suppressMessages(library(parallel))
suppressMessages(library(doParallel))
suppressMessages(library(iterators))
suppressMessages(library(caret))
suppressMessages(library(h2o))
suppressMessages(library(Hmisc))
suppressMessages(library(tm))
suppressMessages(library(stargazer))
suppressMessages(library(spacyr))
suppressMessages(library(pbmcapply))
suppressMessages(library(compiler))
suppressMessages(library(stringr))
suppressMessages(library(mda))
#########################################################################################
# 2) Objects & References
#########################################################################################
# Directories:
args = commandArgs()

scriptName = args[substr(args,1,7) == '--file=']

if (length(scriptName) == 0) {
  scriptName <- rstudioapi::getSourceEditorContext()$path
} else {
  scriptName <- substr(scriptName, 8, nchar(scriptName))
}

pathName = substr(
  scriptName, 
  1, 
  nchar(scriptName) - nchar(strsplit(scriptName, '.*[/|\\]')[[1]][2])
)

setwd(pathName)
setwd("../")
parent_path <- getwd()

#Initialize h2o:
h2oServer <- h2o.init(ip="localhost", port=54321, max_mem_size="5g", nthreads=2)
#########################################################################################
## 2.1) Load w2v Model as MOJO
#########################################################################################
# Load w2v Model:
model_path <- paste0(parent_path,"/h2o_twitter_clssification/W2V_Twitter_Model_400_FR")
w2v.model <- h2o.loadModel(model_path)

# Load Class Vectors to h2o:
classvector_aka_x <- read_csv(paste0(parent_path,"/h2o_twitter_clssification/W2V_Twitter_Model_r00_FR_ClassVectors.csv"))
classvector_aka_x <- as.h2o(classvector_aka_x, "classvector_aka_x")
#########################################################################################
# 3.2) Predict Functions and other helper-functions:
#########################################################################################
stopw <- tm::stopwords(kind = "fr")
STOP_WORDS <- c(stopw)
rm(stopw)

tokenize <- function(sentences, stop.words = STOP_WORDS) {
  tokenized <- h2o.tokenize(sentences, "\\\\W+")
  
  # convert to lower case
  tokenized.lower <- h2o.tolower(tokenized)
  # remove short words (less than 2 characters)
  tokenized.lengths <- h2o.nchar(tokenized.lower)
  tokenized.filtered <- tokenized.lower[is.na(tokenized.lengths) || tokenized.lengths >= 2,]
  # remove words that contain numbers
  tokenized.words <- tokenized.filtered[h2o.grep("[0-9]", tokenized.filtered, invert = TRUE, output.logical = TRUE),]
  
  # remove stop words
  tokenized.words[is.na(tokenized.words) || (! tokenized.words %in% STOP_WORDS),]
}

# Function to get n Elements with max values from Vector:
which.max2 <- function(x, top = 2, values = FALSE)
{
  if (values)
    rev(sort(x))[1:top]
  else
    order(x, decreasing = TRUE)[1:top]
}
#########################################################################################
# 3.3) Load Training Data used for all models
#########################################################################################
# Load the Data:
data <- read_csv(paste0(parent_path,"/h2o_twitter_clssification/train_test_data_twitter_400_fr.csv"))

data <- as.h2o(data, "data")
data$selects_class <- as.factor(data$selects_class)
# Split the Data:
data.split <- h2o.splitFrame(data, ratios = 0.8, seed = 1234)

test <- data.split[2]
test <- test[[1]]
test <- as.data.frame(test)
#########################################################################################
# 3.4) Load useful Classification Models 
#########################################################################################
model_path <- paste0(parent_path,"/h2o_twitter_clssification/dl_1st_best_fr")
dl.model1 <- h2o.loadModel(model_path)

model_path <- paste0(parent_path,"/h2o_twitter_clssification/dl_2nd_best_fr")
dl.model2 <- h2o.loadModel(model_path)

#########################################################################################
# 4) Classifictation
#########################################################################################
#########################################################################################
## 4.1) Ensemble Classification 
#########################################################################################
#Ensemble Function Detailed:
getPredictionProbabilities <- function(models, data){
  sapply(models, function(m){
    p <- h2o.predict(m, data)
    as.matrix(p[, setdiff(colnames(p), "predict") ])
  }, simplify = "array")
}

#Ensemble Function (Just Result):
predictTeam <- function(models, data){
  probabilities <- getPredictionProbabilities(models, data)
  top2 <- apply(
    probabilities, 1,
    function(m) which.max2(apply(m, 1, sum))
  )
  top2 <- as.data.frame(t(top2))
  top2[,2] <- ifelse(top2[,1]== 14, top2[,2], NA)
  
  top2v <- apply(
    probabilities, 1,
    function(m) which.max2(apply(m, 1, sum), values = T)
  )
  top2v <- as.data.frame(t(top2v))
  
  top2[,2] <- ifelse(is.na(top2[,2]) == T, ifelse(top2v[,2] < 0.25, NA, top2[,2]), ifelse(top2v[,2] < 0.1, NA, top2[,2]))
  
  top2 <- cbind(top2,top2v)
  return(top2)
}

# Vector of all Models to consider:
models <- c(dl.model1,dl.model2)
#Validate Ensemble with Validation Data!
result <- getPredictionProbabilities(models = models, data = data)
result_avg <- predictTeam(models = models, data = data)

#########################################################################################
## 4.2) Evaluate Ensemble
#########################################################################################
# Counting Correctness
predictByModel <- apply(result, 1,
                        function(sample) apply(sample, 2, which.max)
)

data_r <- read_csv(paste0(parent_path,"/h2o_twitter_clssification/train_test_data_twitter_400_fr.csv"))

data_r <- data_r %>% mutate(classid = selects_class)

correctAnswersTest <- data_r$classid 
rm(data_r)



#Overall Accuracy of Ensembles models: 
eachModelsCorrectness <- apply(predictByModel, 1,
                               function(modelPredictions) modelPredictions == correctAnswersTest
)

cmc <- apply(eachModelsCorrectness, 1, sum)

cumsumtab <- cumsum(table(cmc))
(length(cmc) - cumsumtab[4]) / length(cmc)

#Confusion Matrix:
CM = confusion(result_avg$V1, correctAnswersTest)

#Overall Accuracy of Ensemble: 
ModelCorrectnessAcc <- sum(diag(CM))/(sum(CM))
ModelCorrectnessAcc
CM
#Per-Class Precision, Recall and F1
n = sum(CM) # number of instances
nc = nrow(CM) # number of classes
diag = diag(CM) # number of correctly classified instances per class 
rowsums = apply(CM, 1, sum) # number of instances per class
colsums = apply(CM, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

classnames  <- c("Elections","Environment_Energy","GenderIssues_Discrimination",
                 "EU_Europe","Finances_Taxes","PublicHealth","Immigration_Asylum",
                 "Not_Classified","Poll","SocialSecurity_WelfareState")

evaldf <- data.frame(precision, recall, f1, classnames) %>% arrange(desc(f1))

setwd(paste0(parent_path,"/tables"))
write.csv(CM, "ConfusionMartrixEnsemble_tweets_fr.csv", row.names = classnames)
write_csv(evaldf, "PerClassEnsemblePerformance_tweets_fr.csv")

#One vs ALL:
n <- sum(CM)
nc <- nrow(CM)
oneVsall <- lapply(1 : nc, function(i){
  v = c(CM[i,i],
        rowsums[i] - CM[i,i],
        colsums[i] - CM[i,i],
        n-rowsums[i] - colsums[i] + CM[i,i]);
  return(matrix(v, nrow = 2, byrow = T))
})

acc_per_class <- sapply(1:length(oneVsall), function(a){
  acc = (oneVsall[[a]][1,1] + oneVsall[[a]][2,2]) / (oneVsall[[a]][2,1] + oneVsall[[a]][1,2] + oneVsall[[a]][1,1] + oneVsall[[a]][2,2]);
  return(acc)
})

evaldf <- data.frame(precision, recall, f1, acc_per_class, classnames) %>% arrange(desc(f1))
colnames(evaldf) <- c("precision", "recall", "f1", "one_vs_all_accuracy", "classnames")
write_csv(evaldf, "PerClassEnsemblePerformance_tweets_fr.csv")

# Averaged Accuracy:
ss <- matrix(0, nrow = 2, ncol = 2)
for(ll in 1:nc){ss = ss + oneVsall[[ll]]}
averaged_accuracy <- sum(diag(ss)/sum(ss))
averaged_accuracy
#########################################################################################
h2o::h2o.shutdown(prompt = F)

