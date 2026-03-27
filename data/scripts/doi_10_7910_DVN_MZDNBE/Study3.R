library(lme4)
library(texreg)
library(effectsize)
library(stm)
library(quanteda)
library(tm)
library(geometry)
library(igraph)
library(Rtsne)
library(rsvd)
library(Amelia)
library(car)
#library(tidyverse)
library(stringi)
library(data.table)
library(splitstackshape)

setwd("/Users/amandaclayton/Dropbox/All Male Panels/2 - Male Ambition/Replication Files")


modeldata<-read.csv('Study3.csv')

#Are men more likely to list a founding father than women? 

modeldata$female<-recode(modeldata$gender, " 'Woman' = 1; else =0 ")

##Values for Table 4
t.test(Washington ~ female, data=modeldata ) #men more likely to list Washington (48% v. 73%)
t.test(Hamilton ~ female, data=modeldata ) #men more likely to list Hamilton (46% v. 28%)
t.test(Jefferson ~ female, data=modeldata ) #men more likely to list Jefferson (64% v. 33%)
t.test(Douglass ~ female, data=modeldata ) #similarly likely to list Douglass (64% v. 58%)
t.test(Anthony ~ female, data=modeldata ) #women more likely to list Anthony (73% v. 32%)
t.test(Adams ~ female, data=modeldata ) #women more likely to list Adams (38% v. 9%)

modeldata$boring<-modeldata$Read + modeldata$Rittenhouse + modeldata$Walker + modeldata$Wolcott

t.test(boring ~ female, data=modeldata ) 


#process STM data 

#iffeel ifwords ffwords ffwords

modeldata$ffwords <- gsub("[\r\n]", " ", modeldata$ffwords)
modeldata$fffeel <- gsub("[\r\n]", " ", modeldata$fffeel)
modeldata$ifwords <- gsub("[\r\n]", " ", modeldata$ifwords)
modeldata$iffeel <- gsub("[\r\n]", " ", modeldata$iffeel)

##combine "five words + thoughts" 

modeldata$ff_comb<-paste(modeldata$fffeel, modeldata$ffwords)
modeldata$if_comb<-paste(modeldata$iffeel, modeldata$ifwords)

data_ff<-modeldata[,c(77, 79)]

processed <- textProcessor(data_ff$ff_comb, metadata = data_ff, customstopwords = 
                             c("i", "me", "my" , "myself", "we", "our", "ours", 
      "ourselves", "you" ,       "your"     ,  "yours"  ,    "yourself" , 
      "yourselves", "he", "him" ,"his", "himself" ,"she", "her", "hers",     
      "herself" ,   "it",      "its"     ,   "itself"    ,
      "they" ,      "them"  ,     "their"  ,    "theirs"  ,  
      "themselves" ,"what"   ,    "which" ,     "who"       ,
      "whom"     ,  "this"    ,   "that"   ,    "these"     ,
      "those"   ,   "am"       ,  "is"     ,    "are"       ,
      "was"  ,      "were"      , "be" ,        "been"      ,
      "being"  ,    "have"  ,     "has"  ,      "had"       ,
      "having" ,    "do"    ,     "does" ,      "did"       ,
      "doing"   ,   "would" ,     "should"   ,  "could"     ,
      "ought" ,     "i'm"    ,    "you're" ,    "he's"    ,  
      "she's"   ,   "it's"    ,   "we're"    ,  "they're"   ,
      "i've" ,   "you've" ,    "we've"    ,  "they've"   ,
      "i'd"   ,     "you'd"  , "he'd"   ,    "she'd"     ,
      "we'd"  ,     "they'd"   ,  "i'll"    ,   "you'll" ,    
      "he'll"  ,    "she'll"    , "we'll"  ,    "they'll"  ,  
      "isn't"   ,   "aren't" ,    "wasn't"  ,   "weren't"  , 
      "hasn't" ,    "haven't" ,   "hadn't" ,    "doesn't"  , 
      "didn't"   ,  
      "shan't"  ,   "shouldn't" , "can't"    ,  "cannot"   , 
      "couldn't" ,  "mustn't",    "let's"     , "that's" ,
      "who's"   ,   "what's"   ,  "here's"  ,   "there's"  , 
      "when's" ,    "where's",    "why's"  ,    "how's"     ,
      "a"      ,    "an"   ,      "the"    ,    "and"  ,     
      "but"    ,    "if"    ,     "or"     ,    "because",   
      "as"     ,    "until"  ,    "while"  ,    "of"   ,     
      "at"     ,    "by"     ,    "for"     ,   "with"  ,    
      "about"  ,    "against" ,   "between"  ,  "into",      
      "through" ,   "during" ,    "before" ,    "after",     
      "above"    ,  "below"   ,   "to"     ,    "from"  ,    
      "up"     ,    "down"   ,    "in"     ,    "out"    ,   
      "on"      ,  "off"    ,    "over"     ,  "under"    , 
      "again"  ,    "further"  ,  "then"    ,   "once"    ,  
      "here"    ,   "there", "other"  ,"some"   ,    "such" ,
      "nor"    ,    "not"     ,   "only"  ,  "own"   ,     "same"   ,  
      "so"    ,     "than"  ,"too"    ,    "very", "like"), removepunctuation=TRUE, stem=TRUE)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta


STM.covariates <- stm(documents = out$documents, vocab = out$vocab,
                K = 6, prevalence = ~meta$female ,
                max.em.its = 100, data = out$meta,
               init.type = "Spectral")


labelTopics(STM.covariates, topics=NULL, n = 5, frexweight = 0.75)

plot(STM.covariates, type = "summary", xlim = c(0, 1), n=5)

plot(STM.covariates, type = "summary", xlim = c(0, 1), n=5, topic.names=c("Laws:", "History:", "Pride:", "Principles:", "People:", "Independence:"))

meta$ff_comb <-as.character(meta$ff_comb)

#Representative responses for "pride" topic

thoughts3 <- findThoughts(STM.covariates, texts = meta$ff_comb, n = 10,
                          topics = 3)$docs[[1]] #

thoughts3

prep <- estimateEffect(1:6~ female,  STM.covariates, meta = out$meta, uncertainty = "Global")


##Figure 6
par(mfrow=c(1,2))


plot(STM.covariates, type = "summary", xlim = c(0, 1), n=5, topic.names=c("Laws:", "History:", "Pride:", "Principles:", "People:", "Independence:"))

plot(prep, covariate = "female",
     model = STM.covariates, method = "difference", cov.value1 = "1",
     cov.value2 = "0",
     xlab = "Male ... Female",
     main = "Marginal Effect of Gender", xlim = c(-0.1, 0.1) ,
     labeltype = "custom", custom.labels = c("Laws:", "History:", "Pride:", "Principles:", "People:", "Independence:"))


##SI Image for responsese to SBA image (A13 and A14)


data_if<-modeldata[,c(77, 80)]

processed <- textProcessor(data_if $if_comb, metadata = data_if, customstopwords = 
                             c("i", "me", "my" , "myself", "we", "our", "ours", 
      "ourselves", "you" ,       "your"     ,  "yours"  ,    "yourself" , 
      "yourselves", "he", "him" ,"his", "himself" ,"she", "her", "hers",     
      "herself" ,   "it",      "its"     ,   "itself"    ,
      "they" ,      "them"  ,     "their"  ,    "theirs"  ,  
      "themselves" ,"what"   ,    "which" ,     "who"       ,
      "whom"     ,  "this"    ,   "that"   ,    "these"     ,
      "those"   ,   "am"       ,  "is"     ,    "are"       ,
      "was"  ,      "were"      , "be" ,        "been"      ,
      "being"  ,    "have"  ,     "has"  ,      "had"       ,
      "having" ,    "do"    ,     "does" ,      "did"       ,
      "doing"   ,   "would" ,     "should"   ,  "could"     ,
      "ought" ,     "i'm"    ,    "you're" ,    "he's"    ,  
      "she's"   ,   "it's"    ,   "we're"    ,  "they're"   ,
      "i've" ,   "you've" ,    "we've"    ,  "they've"   ,
      "i'd"   ,     "you'd"  , "he'd"   ,    "she'd"     ,
      "we'd"  ,     "they'd"   ,  "i'll"    ,   "you'll" ,    
      "he'll"  ,    "she'll"    , "we'll"  ,    "they'll"  ,  
      "isn't"   ,   "aren't" ,    "wasn't"  ,   "weren't"  , 
      "hasn't" ,    "haven't" ,   "hadn't" ,    "doesn't"  , 
      "didn't"   ,  
      "shan't"  ,   "shouldn't" , "can't"    ,  "cannot"   , 
      "couldn't" ,  "mustn't",    "let's"     , "that's" ,
      "who's"   ,   "what's"   ,  "here's"  ,   "there's"  , 
      "when's" ,    "where's",    "why's"  ,    "how's"     ,
      "a"      ,    "an"   ,      "the"    ,    "and"  ,     
      "but"    ,    "if"    ,     "or"     ,    "because",   
      "as"     ,    "until"  ,    "while"  ,    "of"   ,     
      "at"     ,    "by"     ,    "for"     ,   "with"  ,    
      "about"  ,    "against" ,   "between"  ,  "into",      
      "through" ,   "during" ,    "before" ,    "after",     
      "above"    ,  "below"   ,   "to"     ,    "from"  ,    
      "up"     ,    "down"   ,    "in"     ,    "out"    ,   
      "on"      ,  "off"    ,    "over"     ,  "under"    , 
      "again"  ,    "further"  ,  "then"    ,   "once"    ,  
      "here"    ,   "there", "other"  ,"some"   ,    "such" ,
      "nor"    ,    "not"     ,   "only"  ,  "own"   ,     "same"   ,  
      "so"    ,     "than"  ,"too"    ,    "very", "like"), removepunctuation=TRUE, stem=TRUE)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta


STM.covariates <- stm(documents = out$documents, vocab = out$vocab,
                K = 5, prevalence = ~meta$female ,
                max.em.its = 100, data = out$meta,
               init.type = "Spectral")


labelTopics(STM.covariates, topics=NULL, n = 5, frexweight = 0.75)

plot(STM.covariates, type = "summary", xlim = c(0, 1), n=5)

## Figure A13
plot(STM.covariates, type = "summary", xlim = c(0, 1), n=5, topic.names=c("Pride:", "Misc.:", "Movement:", "Voting:", "Rights:"))

meta$if_comb <-as.character(meta$if_comb)

#Representative responses for each topic 
thoughts1 <- findThoughts(STM.covariates, texts = meta$if_comb, n = 10,
                          topics = 1)$docs[[1]] #
thoughts2 <- findThoughts(STM.covariates, texts = meta$if_comb, n = 10,
                          topics = 2)$docs[[1]] 
thoughts3 <- findThoughts(STM.covariates, texts = meta$if_comb, n = 10,
                          topics = 3)$docs[[1]] #
thoughts4 <- findThoughts(STM.covariates, texts = meta$if_comb, n = 5,
                          topics = 4)$docs[[1]] #
thoughts5 <- findThoughts(STM.covariates, texts = meta$if_comb, n = 5,
                          topics = 5)$docs[[1]]  
thoughts6 <- findThoughts(STM.covariates, texts = meta$if_comb, n = 10,
                          topics = 6)$docs[[1]]  

prep <- estimateEffect(1:5~ female,  STM.covariates, meta = out$meta, uncertainty = "Global")


plot(prep, covariate = "female",
     model = STM.covariates, method = "difference", cov.value1 = "1",
     cov.value2 = "0",
     xlab = "Male ... Female",
     main = "Marginal Effect of Gender", xlim = c(-0.1, 0.1) ,
     labeltype = "custom", custom.labels = c("Pride:", "Misc:", "Movement:", "Voting:", "Rights:"))






