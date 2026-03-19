
#Replication script for Lupia, Soroka and Beatty, Science Advances, 2020

#The script that follows replicates all published results except the STM model in Figure 3. Those results vary slightly due to randomization, but the scripts are available upon request.

library(tm)
library(wordcloud)
library(stargazer)
library(effects)
library(quanteda)
library(stm)
library(stringr)

load("Lupia.science.meta.Rdata") #meta data, stored by discussion in Congressional Record
load("Lupia.science.kwic.Rdata") #kwic extractions from fulltext data

###
#Figure 1
pdf("figure1.pdf",width=10,height=7)
barplot(table(cr.meta$section,cr.meta$ycongress),las=1,legend.text=c("Extensions of Remarks","House","Senate"))
title(ylab="Discussions in CR mentioning NSF")
dev.off()

###
#Table 1
addstop <- c("nsf","research","science","national","also"," u ", "will")
myCorpus <- corpus(K,docid_field = "docname",text_field = "sentence") 
myDfm <- dfm(myCorpus, tolower = TRUE, stem = FALSE ,remove = c(stopwords("english"),addstop),remove_punct = TRUE, remove_numbers=TRUE)
words <- as.data.frame(topfeatures(myDfm,20))
words$word <- rownames(words)
topwords <- words$word[1:20]
topwords
counts <- words[1:20,1]
counts
topwords <- rbind(topwords,counts)
topwords
write.csv(t(topwords),file="table1.csv",quote=F)

###
#Supplmentary Table 1
addstop <- c("nsf","research","science","national","also"," u ", "will")
order <- c(1:20)
topwords <- as.data.frame(order)
for (i in 104:115) {
  myCorpus <- corpus(K[K$congress==i,],docid_field = "docname",text_field = "sentence") 
  myDfm <- dfm(myCorpus, tolower = TRUE, stem = FALSE ,remove = c(stopwords("english"),addstop),remove_punct = TRUE, remove_numbers=TRUE)
  words <- as.data.frame(topfeatures(myDfm,20))
  words$word <- rownames(words)
  topwords <- cbind(topwords,words$word)
}
colnames(topwords) <- c("order","1995","1997","1999","2001","2003","2005","2007","2009","2011","2013","2015","2017")
topwords
write.csv(t(topwords),file="tableA1.csv",quote=F)

###
#Table 2
myCorpus <- corpus(K[K$year<2007,],docid_field = "docname",text_field = "sentence") 
myDfm <- dfm(myCorpus, tolower = TRUE, stem = FALSE ,remove = c(stopwords("english"),addstop),remove_punct = TRUE, remove_numbers=TRUE)
words <- as.data.frame(topfeatures(myDfm,2000))
words$word <- rownames(words)
topwordsS <- words
colnames(topwordsS) <- c("start.count","word")
myCorpus <- corpus(K[K$year>2006,],docid_field = "docname",text_field = "sentence") 
myDfm <- dfm(myCorpus, tolower = TRUE, stem = FALSE ,remove = c(stopwords("english"),addstop),remove_punct = TRUE, remove_numbers=TRUE)
words <- as.data.frame(topfeatures(myDfm,2000))
words$word <- rownames(words)
topwordsE <- words
colnames(topwordsE) <- c("end.count","word")
topwords <- merge(topwordsS,topwordsE,by.x="word",by.y="word",all=T)
topwords$start.count[is.na(topwords$start.count)] <- 0
topwords$end.count[is.na(topwords$end.count)] <- 0
start.n <- sum(K$wordcount[K$year<2007]) 
end.n <- sum(K$wordcount[K$year>2006]) 
topwords$diff <- (topwords$start.count / start.n) - (topwords$end.count / end.n)
topwords <- topwords[order(topwords$diff, decreasing=T),]
topwords.start <- topwords$word[1:20] 
topwords.start
topwords <- topwords[order(topwords$diff, decreasing=F),]
topwords.end <- topwords$word[1:20] 
topwords.end
words <- rbind(topwords.start,topwords.end)
words
write.csv(words,file="table2.csv",quote=F)

###
#Table 3
addstop <- c("nsf","research","science","national","also"," u ")
myCorpus <- corpus(K[K$partyF=="2.D",],docid_field = "docname",text_field = "sentence") 
myDfm <- dfm(myCorpus, tolower = TRUE, stem = FALSE ,remove = c(stopwords("english"),addstop),remove_punct = TRUE, remove_numbers=TRUE)
words <- as.data.frame(topfeatures(myDfm,20))
words$word <- rownames(words)
topwordsD <- words$word[1:20]
topwordsD
addstop <- c("nsf","research","science","national","also"," u ")
myCorpus <- corpus(K[K$partyF=="1.R",],docid_field = "docname",text_field = "sentence") 
myDfm <- dfm(myCorpus, tolower = TRUE, stem = FALSE ,remove = c(stopwords("english"),addstop),remove_punct = TRUE, remove_numbers=TRUE)
words <- as.data.frame(topfeatures(myDfm,20))
words$word <- rownames(words)
topwordsR <- words$word[1:20]
topwordsR
words <- rbind(topwordsD,topwordsR)
words
write.csv(words,file="table3.csv",quote=F)

###
#Appendix Table 2
order <- c(1:20)
topwordsD <- as.data.frame(order)
for (i in 104:115) {
  myCorpus <- corpus(K[K$congress==i & K$partyF=="2.D",],docid_field = "docname",text_field = "sentence") 
  myDfm <- dfm(myCorpus, tolower = TRUE, stem = FALSE ,remove = c(stopwords("english"),addstop),remove_punct = TRUE, remove_numbers=TRUE)
  words <- as.data.frame(topfeatures(myDfm,20))
  words$word <- rownames(words)
  topwordsD <- cbind(topwordsD,words$word)
}
colnames(topwordsD) <- c("order","1995","1997","1999","2001","2003","2005","2007","2009","2011","2013","2015","2017")
topwordsD
write.csv(t(topwordsD),file="tableA2.csv",quote=F)

###
#Appendix Table 3
order <- c(1:20)
topwordsR <- as.data.frame(order)
for (i in 104:115) {
  myCorpus <- corpus(K[K$congress==i & K$partyF=="1.R",],docid_field = "docname",text_field = "sentence") 
  myDfm <- dfm(myCorpus, tolower = TRUE, stem = FALSE ,remove = c(stopwords("english"),addstop),remove_punct = TRUE, remove_numbers=TRUE)
  words <- as.data.frame(topfeatures(myDfm,20))
  words$word <- rownames(words)
  topwordsR <- cbind(topwordsR,words$word)
}
colnames(topwordsR) <- c("order","1995","1997","1999","2001","2003","2005","2007","2009","2011","2013","2015","2017")
topwordsR
write.csv(t(topwordsR),file="tableA3.csv",quote=F)

###
#Table 4 Democrats
myCorpus <- corpus(K[K$year<2007 & K$partyF=="2.D",],docid_field = "docname",text_field = "sentence") 
myDfm <- dfm(myCorpus, tolower = TRUE, stem = FALSE ,remove = c(stopwords("english"),addstop),remove_punct = TRUE, remove_numbers=TRUE)
words <- as.data.frame(topfeatures(myDfm,2000))
words$word <- rownames(words)
topwordsS <- words
colnames(topwordsS) <- c("start.count","word")
myCorpus <- corpus(K[K$year>2006 & K$partyF=="2.D",],docid_field = "docname",text_field = "sentence") 
myDfm <- dfm(myCorpus, tolower = TRUE, stem = FALSE ,remove = c(stopwords("english"),addstop),remove_punct = TRUE, remove_numbers=TRUE)
words <- as.data.frame(topfeatures(myDfm,2000))
words$word <- rownames(words)
topwordsE <- words
colnames(topwordsE) <- c("end.count","word")
topwords <- merge(topwordsS,topwordsE,by.x="word",by.y="word",all=T)
topwords$start.count[is.na(topwords$start.count)] <- 0
topwords$end.count[is.na(topwords$end.count)] <- 0
start.n <- sum(K$wordcount[K$year<2007]) 
end.n <- sum(K$wordcount[K$year>2006]) 
topwords$diff <- (topwords$start.count / start.n) - (topwords$end.count / end.n)
topwords <- topwords[order(topwords$diff, decreasing=T),]
topwords.dem.start <- topwords$word[1:20] 
topwords.dem.start
topwords <- topwords[order(topwords$diff, decreasing=F),]
topwords.dem.end <- topwords$word[1:20] 
topwords.dem.end
words <- rbind(topwords.dem.start,topwords.dem.end)
write.csv(words,file="table4d.csv",quote=F)

###
#Table 4 Repubicans
myCorpus <- corpus(K[K$year<2007 & K$partyF=="1.R",],docid_field = "docname",text_field = "sentence") 
myDfm <- dfm(myCorpus, tolower = TRUE, stem = FALSE ,remove = c(stopwords("english"),addstop),remove_punct = TRUE, remove_numbers=TRUE)
words <- as.data.frame(topfeatures(myDfm,2000))
words$word <- rownames(words)
topwordsS <- words
colnames(topwordsS) <- c("start.count","word")
myCorpus <- corpus(K[K$year>2006 & K$partyF=="1.R",],docid_field = "docname",text_field = "sentence") 
myDfm <- dfm(myCorpus, tolower = TRUE, stem = FALSE ,remove = c(stopwords("english"),addstop),remove_punct = TRUE, remove_numbers=TRUE)
words <- as.data.frame(topfeatures(myDfm,2000))
words$word <- rownames(words)
topwordsE <- words
colnames(topwordsE) <- c("end.count","word")
topwords <- merge(topwordsS,topwordsE,by.x="word",by.y="word",all=T)
topwords$start.count[is.na(topwords$start.count)] <- 0
topwords$end.count[is.na(topwords$end.count)] <- 0
start.n <- sum(K$wordcount[K$year<2007]) 
end.n <- sum(K$wordcount[K$year>2006]) 
topwords$diff <- (topwords$start.count / start.n) - (topwords$end.count / end.n)
topwords <- topwords[order(topwords$diff, decreasing=T),]
topwords.rep.start <- topwords$word[1:20] 
topwords.rep.start
topwords <- topwords[order(topwords$diff, decreasing=F),]
topwords.rep.end <- topwords$word[1:20] 
topwords.rep.end
words <- rbind(topwords.rep.start,topwords.rep.end)
write.csv(words,file="table4r.csv",quote=F)

#Table 5
myCorpus <- corpus(K[K$partyF=="2.D",],docid_field = "docname",text_field = "sentence") 
myDfm <- dfm(myCorpus, tolower = TRUE, stem = FALSE ,remove = c(stopwords("english"),addstop),remove_punct = TRUE, remove_numbers=TRUE)
words <- as.data.frame(topfeatures(myDfm,2000))
words$word <- rownames(words)
topwordsD <- words
colnames(topwordsD) <- c("dem.count","word")
myCorpus <- corpus(K[K$partyF=="1.R",],docid_field = "docname",text_field = "sentence") 
myDfm <- dfm(myCorpus, tolower = TRUE, stem = FALSE ,remove = c(stopwords("english"),addstop),remove_punct = TRUE, remove_numbers=TRUE)
words <- as.data.frame(topfeatures(myDfm,2000))
words$word <- rownames(words)
topwordsR <- words
colnames(topwordsR) <- c("rep.count","word")
topwords <- merge(topwordsD,topwordsR,by.x="word",by.y="word",all=T)
topwords$dem.count[is.na(topwords$dem.count)] <- 0
topwords$rep.count[is.na(topwords$rep.count)] <- 0
dem.n <- sum(K$wordcount[K$partyF=="2.D"]) 
rep.n <- sum(K$wordcount[K$partyF=="1.R"]) 
topwords$diff <- (topwords$dem.count / dem.n) - (topwords$rep.count / rep.n)
topwords$dem.prop <- topwords$dem.count / dem.n
topwords$rep.prop <- topwords$rep.count / rep.n
topwords <- topwords[order(topwords$diff, decreasing=T),]
topwords.dem <- topwords$word[1:20] 
topwords.dem
topwords <- topwords[order(topwords$diff, decreasing=F),]
topwords.rep <- topwords$word[1:20] 
topwords.rep
words <- rbind(topwords.dem,topwords.rep)
write.csv(words,file="table5.csv",quote=F)

###
#Figure 2
myCorpus <- corpus(K[K$partyF=="2.D",],docid_field = "docname",text_field = "sentence") 
myDfm <- dfm(myCorpus, tolower = TRUE, stem = FALSE ,remove = c(stopwords("english"),addstop),remove_punct = TRUE, remove_numbers=TRUE)
words <- as.data.frame(topfeatures(myDfm,2000))
words$word <- rownames(words)
topwordsD <- words
colnames(topwordsD) <- c("dem.count","word")
dem.n <- sum(K$wordcount[K$partyF=="2.D"]) 
topwordsD$dem.prop <- topwordsD$dem.count / dem.n 
myCorpus <- corpus(K[K$partyF=="1.R",],docid_field = "docname",text_field = "sentence") 
myDfm <- dfm(myCorpus, tolower = TRUE, stem = FALSE ,remove = c(stopwords("english"),addstop),remove_punct = TRUE, remove_numbers=TRUE)
words <- as.data.frame(topfeatures(myDfm,2000))
words$word <- rownames(words)
topwordsR <- words
colnames(topwordsR) <- c("rep.count","word")
rep.n <- sum(K$wordcount[K$partyF=="1.R"]) 
topwordsR$rep.prop <- topwordsR$rep.count / rep.n 
topwords <- merge(topwordsD,topwordsR,by.x="word",by.y="word",all=T)
topwords$dem.count[is.na(topwords$dem.count)] <- 0
topwords$rep.count[is.na(topwords$rep.count)] <- 0
topwords$dem.prop[is.na(topwords$dem.prop)] <- 0
topwords$rep.prop[is.na(topwords$rep.prop)] <- 0
topwords <- topwords[topwords$dem.count>0,]
topwords <- topwords[topwords$rep.count>0,]
topwords$diff <- (topwords$dem.count / dem.n) - (topwords$rep.count / rep.n)
topwords$both <- topwords$dem.prop + topwords$rep.prop
topwords$dem.prop.l <- log(topwords$dem.prop)
topwords$rep.prop.l <- log(topwords$rep.prop)
topwords <- topwords[order(topwords$both,decreasing = T),]
pdf("figure2.pdf", width = 10, height = 10)
plot(topwords$dem.prop.l[topwords$cutoff==1],topwords$rep.prop.l[topwords$cutoff==1],col="gray",axes=F,ann=F,ylim=c(-8.6,-4.5),xlim=c(-8.6,-4.5))
abline(a=0,b=1,col="gray",lty=2)
axis(1,col="gray",at=c(-8.5,-7.5,-6.5,-5.5,-4.5))
axis(2,las=1,col="gray",at=c(-8.5,-7.5,-6.5,-5.5,-4.5))
title(ylab="Logged Proportion of Republican Words")
title(xlab="Logged Proportion of Democratic Words")
points(topwords$dem.prop.l,topwords$rep.prop.l,col="gray")
topwords <- topwords[order(topwords$diff,decreasing = T),] #dem at the top
topwords$graphic <- NA
topwords$graphic[1:20] <- 1
topwords$graphic[topwords$word=="department"] <- 0
topwords$graphic[topwords$word=="development"] <- 0
topwords$graphic[topwords$word=="students"] <- 0
topwords$graphic[topwords$word=="institutions"] <- 0
text(topwords$dem.prop.l[topwords$graphic==1],topwords$rep.prop.l[topwords$graphic==1],topwords$word[topwords$graphic==1],col="dodgerblue3",pos=4,cex=.8)
text(topwords$dem.prop.l[topwords$graphic==0],topwords$rep.prop.l[topwords$graphic==0],topwords$word[topwords$graphic==0],col="dodgerblue3",pos=2,cex=.8)
topwords <- topwords[order(topwords$diff,decreasing = F),] #rep at the top
points(topwords$dem.prop.l[1:20],topwords$rep.prop.l[1:20],col="red")
topwords$graphic <- NA
topwords$graphic[1:20] <- 1
topwords$graphic[topwords$word=="projects"] <- 0
topwords$graphic[topwords$word=="administration"] <- 0
topwords$graphic[topwords$word=="amended"] <- NA
topwords$graphic[topwords$word=="commission"] <- NA
text(topwords$dem.prop.l[topwords$graphic==1],topwords$rep.prop.l[topwords$graphic==1],topwords$word[topwords$graphic==1],col="firebrick3",pos=4,cex=.8)
text(topwords$dem.prop.l[topwords$graphic==0],topwords$rep.prop.l[topwords$graphic==0],topwords$word[topwords$graphic==0],col="firebrick3",pos=2,cex=.8)
dev.off()

###
#Figure 4
K$negative <- NA 
K$neg_negative <- NA
K$positive <- NA
K$neg_positive <- NA
n <- 1000 
K$batch <- ceiling(as.numeric(rownames(K))*(1/n)) 
print(paste0("Number of Batches: ",max(K$batch))) 
colnames(K)
for (i in unique(K$batch)) { 
  docs <- K[K$batch==i,c("batch","id","sentence")]
  print(paste0("Running Batch: ",i))
  lsd <- dfm(tokens_lookup(tokens(docs$sentence), dictionary = data_dictionary_LSD2015, exclusive = TRUE))
  lsd <- convert(lsd, to="data.frame") 
  if(grepl("neg_negative",c(paste(colnames(lsd),collapse=" ")))==F) lsd$neg_negative<-0
  if(grepl("neg_positive",c(paste(colnames(lsd),collapse=" ")))==F) lsd$neg_positive<-0
  K$negative[K$batch==i] <- lsd$negative
  K$neg_negative[K$batch==i] <- lsd$neg_negative
  K$positive[K$batch==i] <- lsd$positive
  K$neg_positive[K$batch==i] <- lsd$neg_positive
  print(paste0("Completed Batch: ",i))
  rm(lsd)
}
colnames(K)
K$wordcount <- ntoken(char_tolower(K$sentence), remove_punct = TRUE) 
K$lsd <- log ( (K$positive-K$neg_positive+.5) / (K$negative-K$neg_negative + .5) )
model1 <- lm(lsd ~ congressF * partyF ,data=K)
stargazer(model1,type="text")
eff1 <- effect("congressF * partyF", model1)
eff1r <- cbind(eff1$x,fit=eff1$fit,lower=eff1$lower,higher=eff1$upper)
eff1r <- eff1r[order(eff1r$partyF,eff1r$congressF),]
rownames(eff1r) <- NULL
eff1r$congress <- as.numeric(eff1r$congressF)+103
poly <- c(eff1r$congress[eff1r$partyF=="2.D"],rev(eff1r$congress[eff1r$partyF=="2.D"]))
gon <- c(eff1r$lower[eff1r$partyF=="2.D"],rev(eff1r$higher[eff1r$partyF=="2.D"]))
polyg0 <- as.data.frame(cbind(poly,gon))
poly <- c(eff1r$congress[eff1r$partyF=="1.R"],rev(eff1r$congress[eff1r$partyF=="1.R"]))
gon <- c(eff1r$lower[eff1r$partyF=="1.R"],rev(eff1r$higher[eff1r$partyF=="1.R"]))
polyg1 <- as.data.frame(cbind(poly,gon))
pdf("figure4.pdf", width = 6, height = 5)
plot(eff1r$congress[eff1r$partyF=="2.D"],eff1r$fit[eff1r$partyF=="2.D"], type="n",axes=F,ann=F, ylim=c(0,1.2),xlim=c(104,115))
polygon(polyg0$poly,polyg0$gon,border=NA,col="dodgerblue3",density=35,angle=90)
polygon(polyg1$poly,polyg1$gon,border=NA,col="firebrick3",density=36,angle=90)
lines(eff1r$congress[eff1r$partyF=="2.D"],eff1r$fit[eff1r$partyF=="2.D"], lwd=2, lty=1, col="dodgerblue3")
lines(eff1r$congress[eff1r$partyF=="1.R"],eff1r$fit[eff1r$partyF=="1.R"], lwd=2, lty=1, col="firebrick3")
axis(side=2,las=1, cex.axis=.75, col="gray")
mtext(side=2,"Sentiment of NSF Mentions",cex=.75,line=3)
axis(side=1,cex.axis=.7,col="gray",at=c(104:115),labels=c("1995","1997","1999","2001","2003","2005","2007","2009","2011","2013","2015","2017"))
dev.off()
