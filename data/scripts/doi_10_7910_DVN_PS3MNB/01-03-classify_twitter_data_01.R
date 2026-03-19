#!/usr/bin/Rscript
##################################################################################################
# Social Media and Political Agenda Setting (Classification Tweets DE)
##################################################################################################
# Description:
# Classification of tweets with the classifiers trained to classify tweets in 
# German.
##################################################################################################
# Content
##################################################################################################
# 1) Dependencies
# 2) Objects & References
## 2.1) Load Ensemble Subfunctions
## 2.2) Ensemble Functions
# 3) Binary Classification with Wordlist
# 4) Classification Automation Function
## 4.1) Load w2v Model as Generic Model
## 4.2) Load Submodels of Ensemble Model as Generic Model Files for Docker 
## 4.3) Classification Automation Function
# 5) Automated Classification of Texts
## 5.1) Load Texts from SMD
## 5.2) Classify
## 5.3) Write Classification to Elastic...
##################################################################################################
# Dependencies
##################################################################################################
rm(list=ls())

# Libraries:
library(rjson)
library(jsonlite)
library(readr)
library(data.table)
library(dplyr)
library(parallel)
library(doParallel)
library(iterators)
library(caret)
library(h2o)
library(Hmisc)
library(tm)
library(spacyr)
library(pbmcapply)
library(compiler)
library(stringr)
##################################################################################################
# 2) Objects & References
##################################################################################################
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
parent_path <- getwd()

setwd("../h2o_twitter_classification")
##################################################################################################
## 2.1) Load Ensemble Subfunctions
##################################################################################################
# Stopwords
stopw <- tm::stopwords(kind = "de")
STOP_WORDS <- c(stopw)
rm(stopw)

# Tokenizer Function:
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

# Special which.max Function:
which.max2 <- function(x, top = 2, values = FALSE)
{
  if (values)
    rev(sort(x))[1:top]
  else
    order(x, decreasing = TRUE)[1:top]
}

# Text Cleaning Function for German: 
artprepfun <- function(string){
  tmp <- stringi::stri_trans_general(string, "Latin-ASCII")
  tmp <- stringi::stri_replace_all_regex(tmp, pattern = ";|\\n", replacement = " ")
  tmp <- stringi::stri_replace_all_regex(tmp, pattern = "\\»|\\«|[[:digit:]]+|\\<|\\>", replacement = "")
  tmp <- stringi::stri_replace_all_regex(tmp, pattern = "/[\\s\\t\\n]{2,}/", replacement = " " )
  return(tmp)
}

##################################################################################################
## 2.2) Ensemble Functions
##################################################################################################
#Ensemble Function for Predicted Probabilites:
getPredictionProbabilities_txt <- function(models, tx_string, w2v.model = w2v.model){
  words <- tokenize(as.character(as.h2o(tx_string)))
  smd_tx_vec <- h2o.transform(w2v.model, words, aggregate_method = "AVERAGE")
  sapply(models, function(m){
    
    p <- h2o.predict(m, smd_tx_vec)
    as.matrix(p[, setdiff(colnames(p), "predict") ])
  }, simplify = "array")
}

#Ensemble Function for Class (needs getPredictionProbabilities_txt()):
predictTeam_txt <- function(models, data, w2v.model = w2v.model){
  probabilities <- getPredictionProbabilities_txt(models, tx_string = data, w2v.model = w2v.model)
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
  
  # Return second class if ensemble is not completely sure about one class 
  # (some text might be part of more classes especially texts belonging to the class political system)
  top2[,2] <- ifelse(is.na(top2[,2]) == T, ifelse(top2v[,2] < 0.25, "NA", top2[,2]), ifelse(top2v[,2] < 0.1, "NA", top2[,2]))
  
  top2 <- cbind(top2,top2v)
  return(top2)
}

#Classification Function (returns one or two classes depending on ensembles grade of certainty:
getclass <- function(str){
  #Start H2O Classifier and load the models:
  setwd("../h2o_twitter_clssification")
  #Initialize h2o:
  h2oServer <- h2o.init(ip="localhost", port=54321, max_mem_size="12g", nthreads=4)
  #Disable Prgress Bar in H2O:
  h2o.no_progress()
  
  # Load w2v Model:
  model_path <- "W2V_Twitter_Model_200_DE"
  w2v.model <- h2o.loadModel(model_path)
  
  model_path <- "dl_1st_best_de"
  dl.model1 <- h2o.loadModel(model_path)
  
  model_path <- "dl_2nd_best_de"
  dl.model2 <- h2o.loadModel(model_path)
  
  model_path <- "gbm_1st_best_de"
  gbm.model1 <- h2o.loadModel(model_path)

  
  # ModelsList:
  models <- c(dl.model1,dl.model2,gbm.model1)
  
  # Politics Subject
  tmp <- predictTeam_txt(models = models, data = str, w2v.model = w2v.model)
  tmp <- tmp[,1:2]
  tmp <- tmp %>% mutate(V1 = case_when(V1 == 1 ~"Elections",
                                       V1 == 2 ~"Environment_Energy",
                                       V1 == 3 ~"GenderIssues_Discrimination",
                                       V1 == 4 ~"EU_Europe",
                                       V1 == 5 ~"Finances_Taxes",
                                       V1 == 6 ~"PublicHealth",
                                       V1 == 7 ~"Immigration_Asylum",
                                       V1 == 8 ~"Not_Classified",
                                       V1 == 9 ~"Poll",
                                       V1 == 10 ~"SocialSecurity_WelfareState"),
                        V2 = case_when(V2 == 1 ~"Elections",
                                       V2 == 2 ~"Environment_Energy",
                                       V2 == 3 ~"GenderIssues_Discrimination",
                                       V2 == 4 ~"EU_Europe",
                                       V2 == 5 ~"Finances_Taxes",
                                       V2 == 6 ~"PublicHealth",
                                       V2 == 7 ~"Immigration_Asylum",
                                       V2 == 8 ~"Not_Classified",
                                       V2 == 9 ~"Poll",
                                       V2 == 10 ~"SocialSecurity_WelfareState"))
  
  # Close H2O: 
  h2o.shutdown(prompt = F)
  
  tmp <- as.character(ifelse(is.na(tmp$V2) == T, paste0(tmp$V1), paste(tmp$V1, tmp$V2, sep =",")))
  return(tmp)
}

##################################################################################################
# 3) Binary Classifier 
##################################################################################################
#Not Needed Here ....

##################################################################################################
# 4) Classification Automation Process: (no Function less RAM)
##################################################################################################
# See main classification function with name "classfun"
##################################################################################################
## 4.1) Load w2v Model as Generic Model
##################################################################################################
# See main classification function with name "classfun"
##################################################################################################
## 4.2) Load Submodels of Ensemble Model as Generic Model Files for Docker 
##################################################################################################
# See main classification function with name "classfun"
##################################################################################################
## 4.3) Classification Automation Function
##################################################################################################
classfun <- function(data = data, corenum = 4){
  datatxt <- data$text
  datatxt <- mclapply(datatxt, FUN = artprepfun, mc.cores = 4, mc.cleanup = TRUE)
  datatxt <- unlist(datatxt)
  
  data$datatxt <- datatxt
  #Split Data:
  data_to_class <- data
  rm(data)
  #Classifiy texts identified as texts regarding swiss politics:
  strings <- as.character(data_to_class$datatxt)
  
  # Ensemble Classifier:
  output <- getclass(strings)
  
  data_to_class$selectsclass <- output
  
  output <- data_to_class %>% select(-datatxt)
  return(output)
}
##################################################################################################
# 5) Automated Classification of Texts
##################################################################################################
##################################################################################################
## 5.1) Load Texts from SMD
##################################################################################################
df <- readRDS("../../data/Tweets_2018_2019_curated_sentiment.rds")
df <- df %>% filter(la == "de") %>% rename("text" = "Text")

setwd(paste0(parent_path))
getwd()
##################################################################################################
## 5.2) Classify
##################################################################################################
if(nrow(df) > 90000){
  nr <- nrow(df)
  n <- 90000
  dflist <- split(df, rep(1:ceiling(nr/n), each=n, length.out=nr))
  rm(df)
  gc()
  out <- NULL
  for(i in 1:length(dflist)){
    df <- dflist[[i]]
    out_tmp <- classfun(data = df, corenum = 4)
    out <- rbind(out, out_tmp)
    setwd(paste0(parent_path))
    rm(out_tmp, df)
    cat(i, "th module of 90000 rows run and classified!\n")
  }
} else {
  df <- df
  out <- classfun(data = df,  corenum = 4)
  setwd(paste0(parent_path))
}

##################################################################################################
## 5.3) Write Classification from Here to System 
##################################################################################################

write_rds(out, "../data/Twitter_CLASSIFIED_DE.RDS")
##################################################################################################