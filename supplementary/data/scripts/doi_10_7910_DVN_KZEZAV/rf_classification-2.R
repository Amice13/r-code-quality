######################################################
# Script: Classification /w Random Forest model
# Manuscript: From One to Many: Identifying Issues in CJEU Jurisprudence

# Author: Philipp Schroeder, LMU Munich, and Johan Lindholm, Umea University

###--- INSTRUCTIONS ---###
# This script classifies paragraph classes using a Random Forest model and 
# reproduces results presented in Table 5 and Figure 3 of the main manuscript, as well as Figure 11 in the manuscript's appendix

library(readtext)
library(quanteda)
library(randomForest)
library(caret)
library(e1071)
library(caTools)
library(doParallel)

rm(list = ls())

path <- "Your file directory"
path <- "C:/Users/phili/Dropbox/CJEU dataset/Papers/Issue splitting/Replication material/"

# read text files
para_qstart <- readtext(paste0(path, "qstart/*.txt"), cache = FALSE)
para_qstop <- readtext(paste0(path, "qstop/*.txt"), cache = FALSE)
para_noanswer <- readtext(paste0(path, "noanswer/*.txt"), cache = FALSE)
para_residual <- readtext(paste0(path, "residual/*.txt"), cache = FALSE)

# label classes
para_qstart$para_class <- "question_start"
para_qstop$para_class <- "question_stop"
para_noanswer$para_class <- "question_noanswer"
para_residual$para_class <- "residual"

# create corpus object
crps <- corpus(rbind(para_qstart,para_qstop,para_noanswer,para_residual))
crps[1]
crps$para_class[1]

# create document-feature matrix
tks <- crps %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE) %>%
  tokens_tolower %>%
  #tokens_select(pattern = stopwords("en"), selection = "remove") %>%
  tokens_wordstem %>%
  tokens_ngrams(3)

sparse <- tks %>% 
  dfm %>%
  dfm_trim(min_docfreq = 10)
sparse <- as.data.frame(as.matrix(sparse))
colnames(sparse) <- make.names(colnames(sparse))
sparse$para_class <- as.factor(crps$para_class)
dim(sparse)

# split into training and test sets
set.seed(123)
split <- sample.split(sparse$para_class, SplitRatio = 0.5)
xTrain <- subset(sparse, split)
dim(xTrain)
xTest <- subset(sparse, !split)
dim(xTest)
table(xTrain$para_class)
table(xTest$para_class)
str(xTrain$para_class)

# grid search for mtry random forest
control <- trainControl(method="repeatedcv", number=5, repeats=5, search="grid")
set.seed(123)
tunegrid <- expand.grid(.mtry=seq(90,110,5))
#ptm <- proc.time()
# !NB: depending on your machine specifications this code will take a long time to run
# to save time and reproduce exact results presented in the manuscript load object below instead
#rf_gridsearch <- train(para_class ~ ., data = xTrain, method="rf", ntree = 25, do.trace = TRUE, metric = "Accuracy", tuneGrid = tunegrid, trControl = control)
#proc.time() - ptm

load(paste0(path,"rf_gridsearch.RData"))
print(rf_gridsearch)

setEPS()
postscript("rf_gridsearch.eps", width = 10, height = 6) # figure 11 in manuscript appendix
plot(rf_gridsearch)
dev.off()


# fit random forest model
set.seed(123)
ptm <- proc.time()
rf_model <- randomForest(para_class ~ ., data = xTrain, mtry = 95, ntree = 25, nodesize = 5, do.trace = TRUE)
proc.time() - ptm

#save(rf_model, file = "rf_model.RData")
load(paste0(path, "rf_model.RData")) # to get exact replication of results presented in manuscript

# predict paragraph classes
yPredict <- predict(rf_model, newdata = xTest)

tab <- table(xTest$para_class, yPredict)
# check where misclassification occurs
tab

# performance metrics
pre <- function(tab, class){
  prec <- tab[class,class]/sum(tab[,class])
  return(prec)
}
re <- function(tab, class){
  rec <- tab[class,class]/sum(tab[class,])
  return(rec)
}

precision <- rep(NA, 4)
names(precision) <- c("question_start","question_stop","question_noanswer","residual")
recall <- rep(NA, 4)
names(recall) <- c("question_start","question_stop","question_noanswer","residual")

precision[1] <- pre(tab, "question_start")
precision[2] <- pre(tab, "question_stop")
precision[3] <- pre(tab, "question_noanswer")
precision[4] <- pre(tab, "residual")

recall[1] <- re(tab, "question_start")
recall[2] <- re(tab, "question_stop")
recall[3] <- re(tab, "question_noanswer")
recall[4] <- re(tab, "residual")

round(precision, digits = 2)
round(recall, digits = 2)
f <- 2*((precision*recall)/(precision+recall))
round(f, digits = 2)

out <- rbind(round(precision, digits = 2), round(recall, digits = 2), round(f, digits = 2))
rownames(out) <- c("precision","recall","f1")
out <- as.data.frame(out)
out # results presented in Table 5

# variable importance

setEPS()
postscript("rf_varimp.eps", width = 10, height = 6) # figure 3 in main manuscript
varImpPlot(rf_model, main = "Feature Importance", pch = 20)
dev.off()
