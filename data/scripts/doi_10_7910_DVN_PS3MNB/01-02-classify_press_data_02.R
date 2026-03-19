#!/usr/bin/Rscript
##########################################################################################
# Social Media and Political Agenda Setting (Classification Press Releases FR)
##########################################################################################
# Description:
# Classification of press releases with the classifiers trained to classify articles in 
# French.
##########################################################################################
# Content
##########################################################################################
# 1) Dependencies
# 2) Set Parent Directory
# 3) Objects & References
## 3.1) Load w2v Model
## 3.2) Load Submodels of Ensemble Model
## 3.3) Load Ensemble Subfunctions
## 3.4) Ensemble Functions
# 4) Binary Classification with Wordlist
# 5) Classification Automation Function
# 6) Automated Classification of Texts
## 7.1) Load Texts from SMD
## 8.2) Classify
## 9.3) Write Classification to Elastic...
##########################################################################################
# Dependencies
##########################################################################################
rm(list=ls())

#Libraries
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
suppressMessages(library(pbmcapply))
suppressMessages(library(compiler))
suppressMessages(library(stringr))
##########################################################################################
# 2) Set Parent Directory
##########################################################################################
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

#Directories
maindir <- "../data"
importdir <- "../data"
exportdir <- "../data"
##########################################################################################
# 3) Objects & References
##########################################################################################
#Initialize h2o:
h2oServer <- h2o.init(ip="localhost", port=54321, max_mem_size="16g", nthreads=4)
#Disable Prgress Bar in H2O:
h2o.no_progress()
#h2o.getTimezone() ===>>> UTC
#h2o.listTimezones() # We can see all H2O timezones
h2o.setTimezone("US/Central")
##########################################################################################
## 3.1) Load w2v Model as MOJO
##########################################################################################
# Load w2v Model:
model_path <- "../h2o_smd_classification_p3/W2V_2019_SMD_Model_600_FR"
w2v.model <- h2o.loadModel(model_path)
##########################################################################################
## 3.2) Load Submodels of Ensemble Model as MOJO Zip Files
##########################################################################################
model_path <- "../h2o_smd_classification_p3/dl_01_fr"
dl.model1 <- h2o.loadModel(model_path)

model_path <- "../h2o_smd_classification_p3/dl_02_fr"
dl.model2 <- h2o.loadModel(model_path)

model_path <- "../h2o_smd_classification_p3/gbm_01_fr"
gbm.model1 <- h2o.loadModel(model_path)

# ModelsList:
models <- c(dl.model1,dl.model2,gbm.model1)
##########################################################################################
## 3.3) Load Ensemble Subfunctions
##########################################################################################
# Stopwords
stopw <- tm::stopwords(kind = "fr")
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

#Load_WordLists:
entitylist <- readRDS("../h2o_smd_classification_p3/named_entities_swiss_politics_fr.RDS")
politwords <- readRDS("../h2o_smd_classification_p3/political_words_swiss_politics_fr.RDS")
non_pol_ws <- readRDS("../h2o_smd_classification_p3/non_political_words_swiss_politics_fr.RDS")
##########################################################################################
## 3.4) Ensemble Functions
##########################################################################################
#Ensemble Function for Predicted Probabilites:
getPredictionProbabilities_txt <- function(models, tx_string, w2v = w2v.model){
  words <- tokenize(as.character(as.h2o(tx_string)))
  smd_tx_vec <- h2o.transform(w2v, words, aggregate_method = "AVERAGE")
  sapply(models, function(m){
    
    p <- h2o.predict(m, smd_tx_vec)
    as.matrix(p[, setdiff(colnames(p), "predict") ])
  }, simplify = "array")
}

#Ensemble Function for Class (needs getPredictionProbabilities_txt()):
predictTeam_txt <- function(models, data, w2v = w2v.model){
  probabilities <- getPredictionProbabilities_txt(models, tx_string = data, w2v = w2v.model)
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
  # Politics Subject
  tmp <- predictTeam_txt(models = models, data = str, w2v = w2v.model)
  tmp <- tmp[,1:2]
  tmp <- tmp %>% mutate(V1 = case_when(V1 == 1 ~"Agriculture",
                                       V1 == 2 ~"EU_Europa",
                                       V1 == 3 ~"Economy",
                                       V1 == 4 ~"Education_Culture",
                                       V1 == 5 ~"Environment_Energy",
                                       V1 == 6 ~"Finances_Taxes",
                                       V1 == 7 ~"GenderIssues_Discrimination",
                                       V1 == 8 ~"Immigration_Asylum",
                                       V1 == 9 ~"InternationalRelations",
                                       V1 == 10 ~"LabourMarket",
                                       V1 == 11 ~"Law_Order",
                                       V1 == 12 ~"Other_Problems",
                                       V1 == 13 ~"Other_unclassified_Political_Texts",
                                       V1 == 14 ~"PoliticalSystem",
                                       V1 == 15 ~"PublicHealth",
                                       V1 == 16 ~"PublicServices_Infrastructure",
                                       V1 == 17 ~"Regions_NationalCohesion",
                                       V1 == 18 ~"SocialSecurity_WelfareState"),
                        V2 = case_when(V2 == 1 ~"Agriculture",
                                       V2 == 2 ~"EU_Europa",
                                       V2 == 3 ~"Economy",
                                       V2 == 4 ~"Education_Culture",
                                       V2 == 5 ~"Environment_Energy",
                                       V2 == 6 ~"Finances_Taxes",
                                       V2 == 7 ~"GenderIssues_Discrimination",
                                       V2 == 8 ~"Immigration_Asylum",
                                       V2 == 9 ~"InternationalRelations",
                                       V2 == 10 ~"LabourMarket",
                                       V2 == 11 ~"Law_Order",
                                       V2 == 12 ~"Other_Problems",
                                       V2 == 13 ~"Other_unclassified_Political_Texts",
                                       V2 == 14 ~"PoliticalSystem",
                                       V2 == 15 ~"PublicHealth",
                                       V2 == 16 ~"PublicServices_Infrastructure",
                                       V2 == 17 ~"Regions_NationalCohesion",
                                       V2 == 18 ~"SocialSecurity_WelfareState"))
  
  tmp <- as.character(ifelse(is.na(tmp$V2) == T, paste0(tmp$V1), paste(tmp$V1, tmp$V2, sep =",")))
  return(tmp)
}
##########################################################################################
# 4) Binary Classifier (Swiss Politics or other)
##########################################################################################
# Rule Based Classification (Swiss Politcis vs Other)
binclass <- function(datatxt = datatxt, entityl = entitylist, politw = politwords, non_pol_w = non_pol_ws){
  # Count Political Words in Text:
  count_pol <- stringi::stri_count_regex(tolower(datatxt), politw)
  # Count Non Political words in Text:
  count_non_pol <- stringi::stri_count_regex(tolower(datatxt), non_pol_ws)
  
  h <- stringi::stri_detect_regex(tolower(datatxt), tolower(entityl))
  #length(grep(entityl, objecttxt, ignore.case = TRUE, perl = T)
  if(h == TRUE){
    TRUE
  } else {
    if((count_pol-2) >= count_non_pol) {
      TRUE
    } else {
      TRUE
    }
  }
}

#Byte Compile for speed:
binclass <- cmpfun(binclass)
##########################################################################################
# 5) Classification Automation Function
##########################################################################################
classfun <- function(data = data, wordlist_dir = wordlist_dir ,models = models, w2v = w2v.model, 
                     corenum = 3, entityl = entitylist, politw = politwords, non_pol_w = non_pol_ws){
  datatxt <- data$text
  datatxt <- pbmclapply(datatxt, FUN = artprepfun, mc.cores = 2, mc.cleanup = TRUE)
  datatxt <- unlist(datatxt)
  
  # Siwss Politics or not:
  output <- pbmclapply(datatxt, FUN = binclass, mc.cores = corenum, mc.cleanup = TRUE)
  output <- unlist(output)
  
  #Add Boolean to data
  data$swipol <- output
  data$datatxt <- datatxt
  #Split Data:
  data_no_class <- data %>% filter(swipol == F) %>% select(-swipol) %>% mutate(selectsclass = "NotPolitical")
  data_to_class <- data %>% filter(swipol == T) %>% select(-swipol) 
  
  #Classifiy texts identified as texts regarding swiss politics:
  strings <- as.character(data_to_class$datatxt)
  
  #Makes strings smaller if needed java cannot hanlde mor than 65000 chars:
  strings <- substring(strings, first = 1, last = 65000)
  
  output <- getclass(strings)
  
  data_to_class$selectsclass <- output
  
  output <- rbind(data_no_class, data_to_class) %>% select(-datatxt)
  return(output)
}
##########################################################################################
# 6) Automated Classification of Texts
##########################################################################################
##########################################################################################
## 6.1) Load Texts from SMD
##########################################################################################
setwd(paste0(importdir))
df <- readRDS("pressreleases_to_classify.RDS")
df <- df %>% filter(la == "fr") %>% rename("text" = "Text")
#################################################################
## 6.2) Classify
#################################################################
if(nrow(df) > 90000){
  nr <- nrow(df)
  n <- 90000
  dflist <- split(df, rep(1:ceiling(nr/n), each=n, length.out=nr))
  rm(df)
  gc()
  out <- NULL
  for(i in 1:length(dflist)){
    df <- dflist[[i]]
    out_tmp <- classfun(data = df, wordlist_dir = wordlist_dir, corenum = 2)
    out <- rbind(out, out_tmp)
    rm(out_tmp, df)
    cat(i, "th module of 90000 rows run and classified!\n")
  }
} else {
  df <- df
  out <- classfun(data = df, wordlist_dir = wordlist_dir, corenum = 2)
}
##########################################################################################
## 6.3) Write Classification to Output Folder
##########################################################################################
h2o.shutdown(prompt = F)
setwd(parent_path)
setwd(paste0(exportdir))
write_rds(out, "PRESS_CLASSIFIED_FR.RDS")
