######################################################
# Script: Classification /w Naive Bayes Classifier
# Manuscript: From One to Many: Identifying Issues in CJEU Jurisprudence

# Author: Philipp Schroeder, LMU Munich, and Johan Lindholm, Umea University

###--- INSTRUCTIONS ---###
# This script classifies paragraph classes using a Naive Bayes classifier and
# reproduces results presented in Table 5 of the main manuscript

library(readtext)
library(quanteda)
library(quanteda.textmodels)
library(caTools)
library(caret)

rm(list = ls())

path <- "Your file directory"
path <- "C:/Users/phili/Dropbox/CJEU dataset/Papers/Issue Splitting/Replication material/"

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
dim(sparse)

# split into training and test sets
set.seed(123)
split <- sample.split(sparse$para_class, SplitRatio = 0.5)
xTrain <- dfm_subset(sparse, split)
dim(xTrain)
xTest <- dfm_subset(sparse, !split)
dim(xTest)
xTrain$para_class <- as.factor(xTrain$para_class)
xTest$para_class <- as.factor(xTest$para_class)
table(xTrain$para_class)
table(xTest$para_class)

# fit Naive Bayes classifier
nb_model <- textmodel_nb(xTrain, xTrain$para_class)
yPredict <- predict(nb_model, newdata = xTest)

# predict paragraph classes
yPredict <- as.data.frame(yPredict)
names(yPredict) <- "predicted"
yPredict$actual <- xTest$para_class

tab <- table(yPredict$actual, yPredict$predicted)
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
