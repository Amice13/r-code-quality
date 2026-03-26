######################################################
# Script: Classification /w CNN Network
# Manuscript: From One to Many: Identifying Issues in CJEU Jurisprudence

# Author: Philipp Schroeder, LMU Munich, and Johan Lindholm, Umea University

###--- INSTRUCTIONS ---###
# This script classifies paragraph classes using a Convolutional Neural Network and 
# reproduces results presented in Figure 12 in the manuscript's appendix

# !NB Execution of the script requires Tensorflow, Windows users also require a working version of Anaconda
# For advice on the installation of Tensorflow head to https://tensorflow.rstudio.com/installation/

library(tidyverse) 
library(tidytext)
library(quanteda)
library(readtext)
library(tensorflow)
library(keras)
library(ggplot2)
library(ggpubr)

rm(list = ls())

path <- "Your file directory"
path <- "C:/Users/phili/Dropbox/CJEU dataset/Papers/Issue Splitting/Replication material/"

# read text files
para_qstart <- readtext(paste0(path, "qstart/*.txt"), cache = FALSE)
para_qstop <- readtext(paste0(path, "qstop/*.txt"), cache = FALSE)
para_noanswer <- readtext(paste0(path, "noanswer/*.txt"), cache = FALSE)
para_residual <- readtext(paste0(path, "residual/*.txt"), cache = FALSE)

# label classes for convenience of checking correct one-hot encoding
para_qstart$class <- "question_start"
para_qstop$class <- "question_stop"
para_noanswer$class <- "question_noanswer"
para_residual$class <- "residual"

# split to training and test sets
set.seed(123)
train <- rbind(para_qstart[sample(1:1804, 902, replace = FALSE),],
               para_qstop[sample(1:1804, 902, replace = FALSE),],
               para_noanswer[sample(1:189, 95, replace = FALSE),],
               para_residual[sample(1:10000, 5000, replace = FALSE),])
test <- rbind(para_qstart,para_qstop,para_noanswer,para_residual)
test <- test[!test$doc_id %in% train$doc_id,]


# one-hot encode class
train$classInt[train$class == "question_start"] <- 0
train$classInt[train$class == "question_stop"] <- 1
train$classInt[train$class == "question_noanswer"] <- 2
train$classInt[train$class == "residual"] <- 3
train$classInt <- as.integer(train$classInt)
str(train$classInt)
my_y_train <- to_categorical(train$classInt)

test$classInt[test$class == "question_start"] <- 0
test$classInt[test$class == "question_stop"] <- 1
test$classInt[test$class == "question_noanswer"] <- 2
test$classInt[test$class == "residual"] <- 3
test$classInt <- as.integer(test$classInt)
str(test$classInt)
my_y_test <- to_categorical(test$classInt)

# max number of words in paragraphs in training set
max(sapply(strsplit(train$text[train$para_class == "question_start"], " "), length))
max(sapply(strsplit(train$text[train$para_class == "question_stop"], " "), length))
max(sapply(strsplit(train$text[train$para_class == "question_noanswer"], " "), length))
max(sapply(strsplit(train$text[train$para_class == "residual"], " "), length))

# Transform features to sequences
max_features <- 10000 # Maximum number of words to consider as features
maxlen <- 1099 # cut paragraphs after n features and pad shorter paragraphs with 0s

train_tokenizer <- text_tokenizer(num_words = max_features, lower = TRUE, filters='!"#$%&()*+,-./:;<=>?@[\\]^_`{|}~\t\n') %>% 
  fit_text_tokenizer(train$text)
train_sequences <- texts_to_sequences(train_tokenizer, train$text)
train_index <- train_tokenizer$word_index
train_index

my_x_train <- pad_sequences(train_sequences, maxlen = maxlen)  # left-padding
# check examples
train[1,"text"]
train_sequences[1]
my_x_train[1,]
my_y_train[1,]

train[1000,"text"]
train_sequences[1000]
my_x_train[1000,]
my_y_train[1000,]

test_tokenizer <- text_tokenizer(num_words = max_features, lower = TRUE, filters='!"#$%&()*+,-./:;<=>?@[\\]^_`{|}~\t\n') %>% 
  fit_text_tokenizer(test$text)
test_sequences <- texts_to_sequences(test_tokenizer, test$text)
test_index <- test_tokenizer$word_index
my_x_test <- pad_sequences(test_sequences, maxlen = maxlen) # left-padding
# check examples
test[1,"text"]
test_sequences[1]
my_x_test[1,]
my_y_test[1,]

test[1000,"text"]
test_sequences[1000]
my_x_test[1000,]
my_y_test[1000,]

# fit the Convolutional Neural Network
embedding_size = 100

model <- keras_model_sequential()

model %>%
  layer_embedding(input_dim = max_features, output_dim = embedding_size, input_length = maxlen) %>%
  layer_conv_1d(filters = 64, kernel_size = 8, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_flatten() %>%
  layer_dense(units = 64, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_dense(units = 4, activation = "softmax")

model %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

print(model)

#!NB: Your results are likely to differ slightly from results presented in the manuscript given the stochastic nature of the algorithm fitting the network
# To replicate the exact results load history object below
#on.exit(keras::backend()$clear_session())
#history <- model %>% fit(
#  my_x_train, my_y_train,
#  batch_size = 64,
#  epochs = 10,
#  validation_data = list(my_x_test, my_y_test)
#)
#save(history, file = "cnn_history.RData")
load(paste0(path, "cnn_history.RData"))


setEPS()
postscript("cnn_history.eps", width = 10, height = 8) # Figure 12 in manuscript appendix
par(mfrow = c(2,1))
plot("", xlim = c(1,10), ylim = c(0,1.8), type = "b", pch = 1,
     xlab = "Epochs", ylab = "Loss", main = "Model loss")
abline(h = seq(0,1.8,0.5), lty = 2, col = "grey")
lines(1:10, history$metrics$loss, ylim = c(0.05,0.5), type = "b", pch = 1)
lines(1:10, history$metrics$val_loss, ylim = c(0.05,0.5), type = "b", pch = 20)
legend("topleft", inset=.02,
       c("Training loss","Validation loss"), pch = c(1,20), horiz=FALSE, cex=0.8, bg = "white")

plot("", xlim = c(1,10), ylim = c(0.60,1), type = "b", pch = 1,
     xlab = "Epochs", ylab = "Accuracy", main = "Model accuracy")
abline(h = seq(0.60,1,0.1), lty = 2, col = "grey")
lines(1:10, history$metrics$accuracy, ylim = c(0.85,1), type = "b", pch = 1)
lines(1:10, history$metrics$val_accuracy, ylim = c(0.85,1), type = "b", pch = 20)
#legend("topleft", inset=.02,
#       c("Training accuracy","Validation accuracy"), pch = c(1,20), horiz=FALSE, cex=0.8, bg = "white")
dev.off()


# evaluate predictions for individual paragraphs
yPredict <- model %>% predict(my_x_test)
yPredict <- as.data.frame(yPredict)
names(yPredict) <- c("question_start", "question_stop", "question_noanswer", "residual")
selectClass <- function(vector){
  out <- names(which.max(vector))
  return(out)
}
yPredict$predicted <- apply(yPredict, 1, selectClass)
yPredict$actual <- test$class

tab <- table(yPredict$actual, yPredict$predicted)

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

precision
recall

f1 <- 2*((precision*recall)/(precision+recall))
f1
