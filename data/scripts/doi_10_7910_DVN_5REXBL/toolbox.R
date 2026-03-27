
if(!require("lubridate")){install.packages("lubridate")}
if(!require("vegan")){install.packages("vegan")}
if(!require("wordcloud")){install.packages("wordcloud")}
if(!require("quanteda")){install.packages("quanteda")}
if(!require("ggplot2")){install.packages("ggplot2")}
if(!require("stargazer")){install.packages("stargazer")}
if(!require("dplyr")){install.packages("dplyr")}
if(!require("quanteda.textmodels")){install.packages("quanteda.textmodels")}
if(!require("devtools")){install.packages("devtools")}
library(devtools)
if(!require("Statamarkdown")){install_github("hemken/Statamarkdown")}

#install.packages("tidyverse")

library(lubridate)
library(vegan)
library(wordcloud)
library(quanteda)
library(ggplot2)
library(stargazer)
library(dplyr)
library(quanteda.textmodels)

rescale_nominate<-function(x){
  x<-log(x)*20
}

rescale_error<-function(x){
  x<-x*20
}

normalize<-function(x){(x-min(x))/(max(x)-min(x))}


source("tools/plotbox.R")


force_conformance <- function(x, features, force = TRUE) {
  if (!is.dfm(x))
    stop("x must be a dfm")
  if (force) {
    n <- length(featnames(x)) - length(intersect(featnames(x), features))
    if (n)
      warning(n, " feature", if (n == 1) "" else "s",
              " in newdata not used in prediction.",
              call. = FALSE, noBreaks. = TRUE)
    return(dfm_match(x, features))
  } else {
    if (!identical(featnames(x), features))
      stop("newdata's feature set is not conformant to model terms.")
    return(x)
  }
}

pr_mod<-function(object,data,force=T,se=T){
  
  sw <- coef(object)
  data <- force_conformance(data, names(sw), force)
  
  # This is different from computing term weights on only the scorable words.
  # It take rowSums() only to generates named vector.
  raw <- rowSums(dfm_weight(data, "prop", force = TRUE) %*% sw)
  fit <- raw
  
  raw_se <- rep(NA, length(raw))
  if(se==T){
    fwv <- dfm_weight(data, "prop", force = TRUE)
    for (i in seq_along(raw_se))
      raw_se[i] <- sqrt(sum(as.numeric(fwv[i,]) * (raw[i] - sw) ^ 2)) / sqrt(rowSums(data)[[i]])
  }
  
  return(data.frame(score=raw,error=raw_se))
  
}






predict_strap<-function(t2,dfg_ts){
  ps<-matrix(NA,nrow=nrow(docvars(dfg_ts)),ncol=ncol(t2))
  for(i in 1:ncol(t2)){
    scores<-t2[,i]
    # create a wordscore style object
    names(scores)<-rownames(t2)
    m2<-list()
    m2$wordscores<-scores
    class(m2)<-c("textmodel_wordscores","textmodel","list")
    #save(m2,file="models_new/mod_cat.rdata")
    
    ps[,i]<-predict(m2,dfg_ts)
  }
  return(ps)
  
}           



# Helpful functions for text cleaning
stripURL2 = function(x) {
  gsub("pic\\.[^[:space:]]+|www[^[:space:]]+|htt[^[:space:]]+|//t\\.[^[:space:]]+", " ", x)
 
  }

kill_html<-function(x){
  x<-gsub('(<[^>]*>)','',x)
}

kill_html2<-function(x){
  x<-gsub('&.*;','',x)
}

ger_num<-function(x){
  x<-gsub('[0-9],[0-9]','',x)
}


top_words<-function(x,col=1,n=100){
  x<-v1$CA$v
  x<-x[,1:5]
  x<-as.data.frame(x)
  x<-x[order(x[,col]),] 
  x1<-x[1:(n/2),]
  x<-x[order(x[,col],decreasing = T),] 
  x2<-x[1:(n/2),]
  x<-rbind(x1,x2)
  return(x)
}
