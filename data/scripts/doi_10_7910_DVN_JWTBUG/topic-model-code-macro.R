library(pacman)

# This loads and installs the packages you need at once
pacman::p_load(RTextTools, quanteda,
               tm,SnowballC,foreign,plyr,twitteR,
               slam,foreign,wordcloud,LiblineaR,e1071, topicmodels,readr,
               stargazer,ggplot2)

# Load all the budget statements into R

macro = "~/Dropbox/BudgetsTextAnalysis-PMRC2017/CleanData/Macro/"
setwd(macro)

statements = list.files()

# We want to create the variables: countyname,year,statementtype and read in the documents

countyname<-c()
year<-c()
statementtype<-c()
documents<-c()

for(i in 1:length(statements)){
  temp<-unlist(strsplit(statements[i], "[.]"))
  countyname<-c(countyname,temp[1])
  year<-c(year,temp[2])
  statementtype<-c(statementtype,"micro")
  documents<-c(documents,read_file(paste(micro,statements[i],sep="")))
}

# Use regular expressions to clean up some elements of the documents
cleandocs<-c()

for(i in 1:length(documents)){
    cleandocs[i]<-gsub("\xfc\xbe\x8d\x96\x90\xbc"," ",documents[i])
    cleandocs[i]<-gsub("\r\n"," ",cleandocs[i])
    cleandocs[i]<-gsub("\xfc\xbe\x8c\xa3\xa4\xbc"," ",cleandocs[i])
}


# Now we clean and preprocess the statements and prepare them for analysis

library(topicmodels)

# Preprocess the text 

text_cleaner<-function(corpus){
  tempcorpus = lapply(corpus,toString)
  for(i in 1:length(tempcorpus)){
    tempcorpus[[i]]<-iconv(tempcorpus[[i]], "ASCII", "UTF-8", sub="")
  }
  tempcorpus = lapply(tempcorpus, tolower)
  tempcorpus<-Corpus(VectorSource(tempcorpus))
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  
  #Removing all the special charecters and words
  tempcorpus <- tm_map(tempcorpus, toSpace, "/")
  tempcorpus <- tm_map(tempcorpus, toSpace, "@")
  tempcorpus <- tm_map(tempcorpus, toSpace, "\\|")
  tempcorpus <- tm_map(tempcorpus, toSpace, "#")
  tempcorpus <- tm_map(tempcorpus, toSpace, "http")
  tempcorpus <- tm_map(tempcorpus, toSpace, "https")
  tempcorpus <- tm_map(tempcorpus, toSpace, ".com")
  tempcorpus <- tm_map(tempcorpus, toSpace, "$")
  tempcorpus <- tm_map(tempcorpus, removeNumbers)
  # Remove english common stopwords
  tempcorpus <- tm_map(tempcorpus, removeWords, stopwords("english"))
  # Remove punctuation
  tempcorpus <- tm_map(tempcorpus, removePunctuation)
  
  # Eliminate extra white spaces
  tempcorpus <- tm_map(tempcorpus, stripWhitespace)
  # Stem the document
   tempcorpus <- tm_map(tempcorpus, PlainTextDocument)
   tempcorpus <- tm_map(tempcorpus,  stemDocument, "english")
  
  # Remove uninformative high-frequency words
   #tempcorpus <- tm_map(tempcorpus, toSpace, "budget")
   tempcorpus <- tm_map(tempcorpus, toSpace, "million")
   tempcorpus <- tm_map(tempcorpus, toSpace, "counti")
   #tempcorpus <- tm_map(tempcorpus, toSpace, "fund")
   tempcorpus <- tm_map(tempcorpus, toSpace, "also")
   tempcorpus <- tm_map(tempcorpus, toSpace, "will")
   tempcorpus <- tm_map(tempcorpus, toSpace, "board")
   tempcorpus <- tm_map(tempcorpus, toSpace, "last")
   #tempcorpus <- tm_map(tempcorpus, toSpace, "year")
  return(tempcorpus)
}

cleancorpus<-text_cleaner(cleandocs)

# Transform into a document-term matrix
dtm <- DocumentTermMatrix(cleancorpus,
                          control = list(removePunctuation = TRUE,
                                         stopwords=TRUE,
                                         weighting=weightTf))


# Only use words that are present 10 or more times (remove very rare words)

ten_words<-findFreqTerms(dtm,lowfreq = 10)

dtm10<-DocumentTermMatrix(cleancorpus,
                          control = list(removePunctuation = TRUE,
                                         stopwords=TRUE,
                                         weighting=weightTf,
                                         dictionary=ten_words
                          ))


# Run an LDA model with 5 topics looks optimal
ap_lda5 <- LDA(dtm10, k = 5, control = list(seed = 41616), method="VEM")
macrotable<-terms(ap_lda5, k=10)
stargazer(macrotable)

# What is the best model from a perplexity perspective?

model.perplexity<-c()
for(i in 2:20){
  ap_lda <- LDA(dtm10, k = i, control = list(seed = 1234))
  model.perplexity<-c(model.perplexity,perplexity(ap_lda))
}


setwd("~/Dropbox/BudgetsTextAnalysis-PMRC2017/Draft/figs")

png("perplexity-macro.png")
plot(2:20,
     model.perplexity,
     xlab="Number of Topics",
     ylab = "Perplexity",
     main = "Macro Budget Statements")
lines(2:20,model.perplexity)
dev.off()


# Marginal perplexity

marginal.perplexity<-c()
for(i in 2:length(model.perplexity)){
  marginal.perplexity<-c(marginal.perplexity,(model.perplexity[i]-model.perplexity[i-1])/model.perplexity[i-1])
}

index<-1:length(marginal.perplexity)

png("marginal-perplex-macro.png")
plot(index,marginal.perplexity, xlab="Number of Topics", ylab = "Marginal Perplexity (% Change)",
     main = "Macro Budget Statements")
lo <- loess(marginal.perplexity~index)
lines(predict(lo), col='red', lwd=2)
lines(index,marginal.perplexity, col='grey', lwd=1)
dev.off()


# Run an LDA model with 5 topics looks optimal
ap_lda5 <- LDA(dtm, k = 5, control = list(seed = 41616), method="VEM")
terms(ap_lda5, k=5)

### Estimate topic proportions using posterior inference

posterior_inference <- posterior(ap_lda5)
posterior_topic_dist<-posterior_inference$topics # This is the distribution of topics for each document