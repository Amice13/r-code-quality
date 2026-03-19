
install.packages("stylo")
install.packages("tm")
install.packages("NLP")


setwd("C:/...")


library(stylo)
library(tm)
library(NLP)


raw.corpus <- load.corpus(files = "all", corpus.dir = "corpus", 
                          encoding = "UTF-8")


tokenized.corpus <- txt.to.words.ext(raw.corpus,
                                     preserve.case = FALSE)


mul.doc.stem<-function(input){
  x<-input
  for (i in 1:length(x)) {
    temp <- stemDocument(unlist(as.vector(x[i]), use.names = FALSE), language = "english")
    x[i]<-as.data.frame.character(temp)
  }
  output<-x
  return(output)
}

word.stems<-mul.doc.stem(tokenized.corpus)
word.stems


corpus.char.3.grams <- txt.to.features(word.stems, ngram.size = 3,
                                       features = "w")


frequent.features <- make.frequency.list(corpus.char.3.grams, head = 3000)
frequent.features

freqs.absolute <- make.table.of.frequencies(corpus.char.3.grams, features = frequent.features, relative=FALSE)
freqs.absolute

freqs.relative <- make.table.of.frequencies(corpus.char.3.grams, features = frequent.features, relative=TRUE)
freqs.relative


stylo(frequencies = freqs.relative, gui = TRUE)


