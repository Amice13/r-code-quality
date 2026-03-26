######################################################
# Script: Classification /w Neural Network
# Manuscript: From One to Many: Identifying Issues in CJEU Jurisprudence

# Author: Philipp Schroeder, LMU Munich, and Johan Lindholm, Umea University

###--- INSTRUCTIONS ---###
# This script classifies paragraph classes using a feedforward Neural Network and 
# reproduces results presented in Table 5 of the main manuscript, as well as Figures 9 and 10 in the manuscript's appendix

# !NB Execution of the script requires Tensorflow, Windows users also require a working version of Anaconda
# For advice on the installation of Tensorflow head to https://tensorflow.rstudio.com/installation/

library(quanteda)
library(readtext)
library(tensorflow)
library(keras)
library(tfruns)
library(ggplot2)
library(ggpubr)
library(caTools)
library(dplyr)

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

# one-hot encode class
sparse$classInt[sparse$para_class == "question_start"] <- 0
sparse$classInt[sparse$para_class == "question_stop"] <- 1
sparse$classInt[sparse$para_class == "question_noanswer"] <- 2
sparse$classInt[sparse$para_class == "residual"] <- 3
sparse$classInt <- as.integer(sparse$classInt)
str(sparse$classInt)

# split into training and test sets
set.seed(123)
split <- sample.split(sparse$para_class, SplitRatio = 0.5)
xTrain <- dfm_subset(sparse, split)
dim(xTrain)
xTest <- dfm_subset(sparse, !split)
dim(xTest)
table(xTrain$para_class)
table(xTrain$classInt)
table(xTest$para_class)
table(xTest$classInt)

# one hot encode
yTrain <- to_categorical(xTrain$classInt, num_classes = 4)
yTest <- to_categorical(xTest$classInt, num_classes = 4)

# tune model parameters !NB: this may take a while on your machine
#source("Your file directory/nn_tuning_run.R")

# to save time and reproduce results from the main manuscript skip line 86 and run lines 89 and 90 instead
load(file = paste0(path, "nn_tuning_runs.RData"))
best_run <- ls_runs(order = metric_val_accuracy, decreasing= TRUE, runs_dir = paste0(path, "_tuning"))[1,]

# specify the model with optimal parameters from tuning runs
model <- keras_model_sequential() %>% 
  layer_dense(units = best_run$flag_neurons1, activation = "relu", input_shape = dim(xTrain)[2],
              kernel_regularizer = regularizer_l2(l = best_run$flag_l2)) %>% 
  layer_dropout(rate = best_run$flag_dropout1) %>%
  layer_dense(units = best_run$flag_neurons2, activation = "relu", 
              kernel_regularizer = regularizer_l2(l = best_run$flag_l2)) %>%
  layer_dropout(rate = best_run$flag_dropout2) %>%
  layer_dense(units = 4, activation = "softmax")

opt <- optimizer_rmsprop(lr = best_run$flag_lr)

model %>% compile(
  optimizer = opt,
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

print(model)

# plot history of fit
#!NB: Your results are likely to differ slightly from results presented in the manuscript given the stochastic nature of the algorithm fitting the network
# To get exactly the same results as presented in the manuscript load history object below

#on.exit(keras::backend()$clear_session())
#history <- model %>% fit(xTrain, yTrain, epochs = 10, batch_size = 64,
#              validation_data = list(xTest, yTest))
#save(history, file = "nn_history.RData")
load(paste0(path, "nn_history.RData"))

setEPS()
postscript("network_history.eps", width = 10, height = 8) # Figure 9 in manuscript appendix
par(mfrow = c(2,1))
plot("", xlim = c(1,10), ylim = c(0.05,0.5), type = "b", pch = 1,
     xlab = "Epochs", ylab = "Loss", main = "Model loss")
abline(h = seq(0.1,0.5,0.1), lty = 2, col = "grey")
lines(1:10, history$metrics$loss, ylim = c(0.05,0.5), type = "b", pch = 1)
lines(1:10, history$metrics$val_loss, ylim = c(0.05,0.5), type = "b", pch = 20)
legend("topright", inset=.02,
       c("Training loss","Validation loss"), pch = c(1,20), horiz=FALSE, cex=0.8, bg = "white")
plot("", xlim = c(1,10), ylim = c(0.85,1), type = "b", pch = 1,
     xlab = "Epochs", ylab = "Accuracy", main = "Model accuracy")
abline(h = seq(0.85,1,0.05), lty = 2, col = "grey")
lines(1:10, history$metrics$accuracy, ylim = c(0.85,1), type = "b", pch = 1)
lines(1:10, history$metrics$val_accuracy, ylim = c(0.85,1), type = "b", pch = 20)
legend("bottomright", inset=.02,
       c("Training accuracy","Validation accuracy"), pch = c(1,20), horiz=FALSE, cex=0.8, bg = "white")
dev.off()


# Fit the model to get predictions for paragraph classes
#!NB: Your results are likely to differ slightly from results presented in the manuscript given the stochastic nature of the algorithm fitting the network
# To get exactly the same predictions as presented in the manuscript load model below
#model %>% fit(xTrain, yTrain, epochs = 10, batch_size = 64,
#              validation_data = list(xTest, yTest))
model <- load_model_hdf5(paste0(path, "neural_network.h5"))



# make predictions for set held out for validation from training set
yPredict <- model %>% predict(xTest)

# evaluate predictions for individual paragraphs
yPredict <- as.data.frame(yPredict)
names(yPredict) <- c("question_start", "question_stop", "question_noanswer", "residual")
selectClass <- function(vector){
  out <- names(which.max(vector))
  return(out)
}
yPredict$predicted <- apply(yPredict, 1, selectClass)
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

# if you care to save the model file
#model %>% save_model_hdf5(paste0("neural_network_",Sys.Date(),".h5"), overwrite = TRUE)
#save(crpsTrain, file = paste0("corpus_",Sys.Date(),".RData"))
#save(xTrain, file = paste0("dfm_",Sys.Date(),".RData"))


# fit 50 replications to get a sense of variation across different runs
#!NB: Your results are likely to differ slightly from results presented in the manuscript given the stochastic nature of the algorithm fitting the network
nn_iterations <- function(){
  model %>% fit(xTrain, yTrain, epochs = 10, batch_size = 64,
                validation_data = list(xTest, yTest))
  # make predictions for set held out for validation from training set
  yPredict <- model %>% predict(xTest)
  
  # evaluate predictions for individual paragraphs
  yPredict <- as.data.frame(yPredict)
  names(yPredict) <- c("question_start", "question_stop", "question_noanswer", "residual")
  selectClass <- function(vector){
    out <- names(which.max(vector))
    return(out)
  }
  yPredict$predicted <- apply(yPredict, 1, selectClass)
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
  #save(model, file = "nn_model.RData")
  return(out)
}

#performance <- replicate(50, nn_iterations(), simplify = FALSE)
#save(performance, file = "nn_iterations.RData")
load(file = paste0(path, "nn_iterations.RData")) # to get exact results presented in manuscript
metrics <- do.call("rbind", performance)
str(metrics)
rownames(metrics)

summary(metrics[grepl("precision",rownames(metrics)), "question_start"])
summary(metrics[grepl("precision",rownames(metrics)), "question_stop"])
summary(metrics[grepl("precision",rownames(metrics)), "question_noanswer"])
summary(metrics[grepl("precision",rownames(metrics)), "residual"])

# precision
mean(metrics[grepl("precision",rownames(metrics)), "question_start"])
mean(metrics[grepl("precision",rownames(metrics)), "question_stop"])
mean(metrics[grepl("precision",rownames(metrics)), "question_noanswer"])
mean(metrics[grepl("precision",rownames(metrics)), "residual"])

# recall
mean(metrics[grepl("recall",rownames(metrics)), "question_start"])
mean(metrics[grepl("recall",rownames(metrics)), "question_stop"])
mean(metrics[grepl("recall",rownames(metrics)), "question_noanswer"])
mean(metrics[grepl("recall",rownames(metrics)), "residual"])

# f1
mean(metrics[grepl("f1",rownames(metrics)), "question_start"])
mean(metrics[grepl("f1",rownames(metrics)), "question_stop"])
mean(metrics[grepl("f1",rownames(metrics)), "question_noanswer"])
mean(metrics[grepl("f1",rownames(metrics)), "residual"])

# standard deviations of estimates
sd(metrics[grepl("precision",rownames(metrics)), "question_start"])
sd(metrics[grepl("recall",rownames(metrics)), "question_start"])
sd(metrics[grepl("f1",rownames(metrics)), "question_start"])

sd(metrics[grepl("precision",rownames(metrics)), "question_stop"])
sd(metrics[grepl("recall",rownames(metrics)), "question_stop"])
sd(metrics[grepl("f1",rownames(metrics)), "question_stop"])

sd(metrics[grepl("precision",rownames(metrics)), "question_noanswer"])
sd(metrics[grepl("recall",rownames(metrics)), "question_noanswer"])
sd(metrics[grepl("f1",rownames(metrics)), "question_noanswer"])

sd(metrics[grepl("precision",rownames(metrics)), "residual"])
sd(metrics[grepl("recall",rownames(metrics)), "residual"])
sd(metrics[grepl("f1",rownames(metrics)), "residual"])

setEPS()
postscript("iterations_performance.eps", width = 10, height = 8) # Figure 10 in manuscript appendix
par(mfrow = c(2,3))
plot(density(metrics[grepl("precision",rownames(metrics)),"question_start"]), xlim = c(0.80,1), main = "Precision for class question_start")
polygon(density(metrics[grepl("precision",rownames(metrics)),"question_start"]), col = alpha("lightgrey", 0.35), border = NA)
plot(density(metrics[grepl("precision",rownames(metrics)),"question_stop"]), xlim = c(0.80,1), main = "Precision for class question_stop")
polygon(density(metrics[grepl("precision",rownames(metrics)),"question_stop"]), col = alpha("lightgrey", 0.35), border = NA)
plot(density(metrics[grepl("precision",rownames(metrics)),"question_noanswer"]), xlim = c(0.80,1), main = "Precision for class question_noanswer")
polygon(density(metrics[grepl("precision",rownames(metrics)),"question_noanswer"]), col = alpha("lightgrey", 0.35), border = NA)

plot(density(metrics[grepl("recall",rownames(metrics)),"question_start"]), xlim = c(0.80,1), main = "Recall for class question_start")
polygon(density(metrics[grepl("recall",rownames(metrics)),"question_start"]), col = alpha("lightgrey", 0.35), border = NA)
plot(density(metrics[grepl("recall",rownames(metrics)),"question_stop"]), xlim = c(0.80,1), main = "Recall for class question_stop")
polygon(density(metrics[grepl("recall",rownames(metrics)),"question_stop"]), col = alpha("lightgrey", 0.35), border = NA)
plot(density(metrics[grepl("recall",rownames(metrics)),"question_noanswer"]), xlim = c(0.80,1), main = "Recall for class question_noanswer")
polygon(density(metrics[grepl("recall",rownames(metrics)),"question_noanswer"]), col = alpha("lightgrey", 0.35), border = NA)
dev.off()
