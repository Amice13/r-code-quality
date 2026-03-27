##################################################################################
## R commands to replicate figures in F.Genovese, 2015, "Politics Ex Cathedra:  ##
## Religious Authority and the Pope in Modern IR" (Federica Genovese, 27/09/15) ##
##################################################################################

#Sys.setenv(NOAWT= "true") # Needed for clean up of texts

library(foreign)
library(tm)
library(wordcloud)
library(SnowballC)
library(austin)
library(lda)
library(reshape)
library(reshape2)
library(modeltools)
library(ggplot2)
library(topicmodels)
library(grid)
library(Hmisc)
library(rJava)
#library(openNLP)
library(base)
library(plyr)
library(dplyr)
install.packages("Rtools")
install.packages("stm") 
if(!require(devtools)) install.packages("devtools")
install.packages("evaluate") 
library(devtools)
install_github("bstewart/stm",dependencies=TRUE)
install.packages("RCurl") #program works even if this line creates an error
install.packages("matrixStats") # , lib="O:/R/R-3.1.1/library")
install.packages("R.methodsS3") #, lib="O:/R/R-3.1.1/library")
library(R.methodsS3)
library(stm)
getwd() #Fix based on your directory 

#############################
# Data input and management #
#############################

setwd("/Users/genovesefederica/Dropbox/cc_encycl/replication/encyclicals_it")
cname <- file.path(".", "encyclicals_it_re")
cname
dir(cname)
#convert text list  to corpus
docs <- Corpus(DirSource(cname))
 class(docs)
 class(docs[[1]])
summary(docs)
inspect(docs[1])
encycl <- rownames(summary(docs))
encycl
getTransformations()

# replace "/" used sometimes to separate alternative words
for (j in seq(docs))
{
docs[[j]] <- gsub("/", " ", docs[[j]])
docs[[j]] <- gsub("@", " ", docs[[j]])
}

# Preparation of documents (conversion to lower case, removal of punctuation, yada yada)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, PlainTextDocument)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs2 <- tm_map(docs, stemDocument)
docs <- tm_map(docs2, removeWords, c(stopwords('italian'),"che", "ma", "per", "anch", "ogni", "modo", "piu", "perch", "cos","alcuni", "sempr", "quando", "cose", "part", "senza", "esser","aver","cio","puo","possono","molto","tutt","fatto", "solo","tale","stess","pid","qual","gesd","deg","deve","infatti","dell","essa","stesso","ag","ci","gia","delluomo","eg","cf","pu","d"))
inspect(docs[1])
#create a document term matrix object for input into the LDA model
dtm <- DocumentTermMatrix(docs)
dtm

#Keep some important elements of this matrix for post-estimation routines
words<-dtm[[6]]
document_index<-dtm[[4]]

# Lines 82-89 export the texts to be integrated in the dataset "encycl_stm" used to run the Structural Topic Models analysis (see below).

# setwd("/Users/genovesefederica/Dropbox/cc_encycl/replication") 
# for (i in 1:length(docs)) {
# dataframe <-data.frame(text=unlist(sapply(docs[i], `[`, "content")), 
#    stringsAsFactors=F)
#  write.table(dataframe, paste("doc",i,".txt"))    
# }

#################################
# Summary stats of raw documents#
#################################

rows <- rowSums(as.matrix(dtm))
rows
smean.sd(rows)
smedian.hilow(rows, conf.int=.05)

# Check out the row frequencies
m <- as.matrix(dtm)
v <- sort(colSums(m), decreasing=TRUE)
head(v, 70)
words <- names(v)
d <- data.frame(word=words, freq=v)
#wordcloud(d$word, d$freq, min.freq=500)
#wordcloud(d$word, d$freq, min.freq=500, random.order=FALSE, rot.per=0)
#df <- d[d$freq >= 500, ] 

rownames(m) <- encycl
newnames <- c("1959 ad petri ","1959 sacerdotii ","1959 grata recordatio ","1959 princeps ","1961 mater ","1961 aeterna dei ","1962 paenitentiam ","1963 pacem ","1964 ecclesiam ","1965 mense maio ","1965 mysterium ","1966 christi matri ","1967 populorum progressio ","1967 sacerdotalis caelibatus ","1968 humanae vitae ","1979 redemptor hominis ","1980 dives in misericordia ","1981 laborem exercens ","1985 slavorum apostoli ","1986 dominum et vivificantem ","1987 redemptoris mater ","1987 sollicitudo rei socialis ","1990 redemptoris missio ","1991 centesimus annus ","1993 veritatis splendor ","1995 evangelium vitae ","1995 ut unum sint ","1998 fides et ratio ","2003 eccl de euch ","2005 deus caritas est ","2007 spe salvi ","2009 caritas in veritate ","2013 enciclica lumen fidei it ","2015 laudato si ")
rownames(m) <- newnames

m2 <- t(m)
new_m <- wfm(m2)
dim(new_m)
docs(new_m)

#######################
# Define wfm functions#
#######################

wfm.to.lda <- function(wfm){
    m <- as.worddoc(wfm)
    v <- words(m)

    d <- list()
    for (i in 1:length(docs(m))){
        nzero <- which(m[,i]>0)
        d[[i]] <- t(matrix(as.integer(c(nzero-1, m[nzero, i])), ncol=2))
    }
    t <- docs(m)
    corpus <- list(vocab=v, documents=d, titles=t)
	return(corpus)
}

wfm.to.bmr <- function (y, wfm, filename, line.sep='\n') {
    x <- as.worddoc(wfm)
    
    lines <- c()
    marg <- wordmargin(wfm)
    for (i in 1:ncol(x)) {
        nonz.vals <- c()
        if (marg == 1){ 
        	## docs are columns
        	nonz <- which(x[, i] != 0)
        	if (length(nonz)>0)
        		nonz.vals <- as.numeric(x[nonz, i])
        } else { 
        	## docs are rows
        	nonz <- which(x[i, ] != 0)
        	if (length(nonz)>0)
        		nonz.vals <- as.numeric(x[i, nonz])
        }
        
       if (length(nonz.vals)>0){
        	## construct a line
        	## 0 if we don't know the class (ignored in training set)
	        yy <- ifelse(!is.null(y), y[i], 0)
	        
    	    ldat <- paste(nonz, ":", nonz.vals, collapse = " ", sep = "")
        	lines <- c(lines, paste(yy, ldat))
        }
    }
    writeLines(lines, filename, sep=line.sep)
}

##############################
# Topic number identification#
##############################

# How many topics? One can investigate the α values of the models fitted with the topic models package expectation maximization method (VEM) and the models fitted with VEM and α fixed.

# k<-3
# SEED <- 123
# lda_train <- list(VEM = LDA(m, k = k, control = list(seed = SEED)),  VEM_fixed = LDA(m, k = k, control = list(estimate.alpha = FALSE, seed = SEED)), Gibbs = LDA(m, k = k, method = "Gibbs", control = list(seed = SEED, burnin = 1000, thin = 100, iter = 1000)), CTM = CTM(m, k = k, control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3))))
# sapply(lda_train[1:2], slot, "alpha")
# If α is estimated its value is much smaller than the default. This indicates that the Dirichlet distribution has more mass at the corners and documents consist only of a few topics (fewer than 5).

# I calculate the following Model Fit Statistics: perplexity scores and log-likelihood

# Re-set the working directory
setwd("/Users/genovesefederica/Dropbox/cc_encycl/replication/encyclicals_model fit output") 
# Note: this takes a while! Skip to line 208 to plot estimated metrics.

for(i in 1:20){	
	#start with null perplexity and loglike objects
	perplex<-NULL
	loglike<-NULL
	
	#Generate range of topic-numbers to be evaluated
	topics <-  c(2, 3, 4, 5, 6)
	#set the random number seed for this iteration
	Seed<-i

	for (k in topics) {
	lda <- LDA(m, control=list(seed = Seed), k = k, compute.log.likelihood = TRUE)
	perplex<-rbind(perplex,perplexity<-perplexity(lda, m))
	loglike<-rbind(loglike,logLik(lda))
	}

	forplot<-cbind(topics,perplex,loglike)
	#save combined results for subsequent plotting
	forplot<-as.data.frame(forplot)
	write.csv(forplot,paste("modelfit",i,".csv"))
} #end loop

# Plot perplexity measure
combineperplex<-NULL
for(i in 1:20){
  forplot<-read.csv(paste("modelfit",i,".csv"))
  plot(forplot[,3], type="o", col="lightgrey",xlab="",ylab="",axes=FALSE,ylim=c(3000,5000),main="")
  par(new=T) 
  combineperplex<-cbind(combineperplex,forplot[,3])
}
plot(rowMeans(combineperplex), type="o", col="red",lwd=2.5,xlab="Topics",ylab="Perplexity",axes=FALSE,ylim=c(3000,5000),main="")
axis(1, at=1:5, lab=c("2", "3",  "4",  "5", "6"),las=01,cex.axis=.85)
axis(2, at=c(3000,4000,5000), lab=c("3000",    "4000","5000"), las = 0,cex.axis=.85)
dev.copy2pdf(device = quartz, file = "/Users/genovesefederica/Dropbox/cc_encycl/replication/figures/Rplot_modelfitperplexity.pdf",width = 6, height = 4)

# Plot loglikelihood measure
combinell<-NULL
for(i in 1:20){
  forplot<-read.csv(paste("modelfit",i,".csv"))
  plot(forplot[,4], type="o", col="lightgrey",xlab="",ylab="",axes=FALSE,ylim=c(-2700000,-2500000),main="")
  par(new=T) 
  combinell<-cbind(combinell,forplot[,4])
}
plot(rowMeans(combinell), type="o", col="red",lwd=2.5,xlab="Topics",ylab="Log-Likelihood",axes=FALSE,ylim=c(-2700000,-2500000),main="")
axis(1, at=1:5, lab=c("2", "3",  "4",  "5", "6"),las=01,cex.axis=.85)
axis(2, at=c(-2700000, -2600000, -2500000), lab=c( "-2700000","-2600000", "-2500000"), las = 0,cex.axis=.85)
dev.copy2pdf(device = quartz, file = "/Users/genovesefederica/Dropbox/cc_encycl/replication/figures/Rplot_modelfitloglik.pdf",width = 6, height = 4)

###########
# Analysis#
###########

# LDA with 2 topics
m.all <- wfm(m, word.margin=2)
m.lda <- wfm.to.lda(m.all)

# 50 Gibbs sampling iterations with alpha and eta parameters equal 1.
mod <- lda.collapsed.gibbs.sampler(m.lda$documents, 2, m.lda$vocab, 50, 1, 1, compute.log.likelihood=TRUE)
    
# Show the topic model results
top.topic.words(mod$topics, 10, by.score=TRUE)
top.words <- top.topic.words(mod$topics, 10, by.score=TRUE)
top.words 

# Compute LDA with set seed:
# lda <- LDA(m, control=list(seed=123, alpha = 0.1), k = 2, compute.log.likelihood = TRUE)

# Compute topics with  multiple seeds (different initializations):
	for(i in 1:50){	
	Seed<-i
	lda <- LDA(m, control=list(seed=Seed, alpha = 0.1), k = 2, compute.log.likelihood = TRUE)
	summary(lda)
	term <- terms(lda, 5)
	term <- apply(term, MARGIN = 2, paste, collapse = ", ")
}
topics <- topics(lda, 1)
topics <- data.frame(topics)
topics
topics <- data.frame(Enc=encycl[34:length(topics)], topics)


# Plot results
p <-qplot(Enc,  data = topics,  fill = term [topics], position ="dodge", geom="bar",xlab=" ", ylab=" ")

p + scale_x_discrete(breaks=c(1,10,20)) + theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")) +  theme(legend.position="bottom") + theme(axis.text.y=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank()) + coord_flip()  +
annotate("text", x = 1.03,  y = .51, label = "Laudato Si 2015", cex=3.5,  hjust = 0) +
annotate("text", x = 2.03,  y = .01, label = "Lumen Fidei 2013", cex=3.5,   hjust = 0) +  
annotate("text", x = 3.03,  y = .51, label = "Caritas In Veritate 2009", cex=3.5,   hjust = 0) +
annotate("text", x = 4.03,  y = .01, label = "Spe Salvi 2007", cex=3.5,   hjust = 0) +
annotate("text", x = 5.03,  y = .01, label = "Deus Caritas Est 2005", cex=3.5,   hjust = 0) +
annotate("text", x = 6.03,  y = .01, label = "Eccl Ed Euch 2003", cex=3.5,   hjust = 0) + 
annotate("text", x = 7.03,  y = .01, label = "Fides Et Ratio 1998", cex=3.5,   hjust = 0) +
annotate("text", x = 8.03,  y = .01, label = "Ut Unum Sint 1995", cex=3.5,   hjust = 0) + 
annotate("text", x = 9.03,  y = .51, label = "Evangelium Vitae 1995", cex=3.5,   hjust = 0) + 
annotate("text", x = 10.03,  y = .01, label = "Veritatis Splendor 1993", cex=3.5,   hjust = 0) +
annotate("text", x = 11.03,  y = .51, label = "Centesimus Annus 1991", cex=3.5,   hjust = 0) +
annotate("text", x = 12.03,  y = .01, label = "Redemptoris Missio 1990", cex=3.5,   hjust = 0) +
annotate("text", x = 13.03,  y = .51, label = "Sollicitudo Rei Socialis 1987", cex=3.5,   hjust = 0) +
annotate("text", x = 14.03,  y = .01, label = "Redemptoris Mater 1987", cex=3.5,   hjust = 0) + 
annotate("text", x = 15.03,  y = .01, label = "Dominum Et Vivific 1986", cex=3.5,   hjust = 0) +
annotate("text", x = 16.03,  y = .01, label = "Slavorum Apostoli 1985", cex=3.5,   hjust = 0) +
annotate("text", x = 17.03,  y = .51, label = "Laborem Exercens 1981", cex=3.5,   hjust = 0) +
annotate("text", x = 18.03,  y = .01, label = "Dives Misericordia 1980", cex=3.5,   hjust = 0) + 
annotate("text", x = 19.03,  y = .01, label = "Redemptor Hominis 1979", cex=3.5,   hjust = 0) +
annotate("text", x = 20.03,  y = .51, label = "Humanae Vitae 1968", cex=3.5,   hjust = 0) + 
annotate("text", x = 21.03,  y = .01, label = "Sacerdotalis Caelib 1967", cex=3.5,   hjust = 0) + 
 annotate("text", x = 22.03,  y = .51, label = "Populorum Progressio 1967", cex=3.5,   hjust = 0) + 
 annotate("text", x = 23.03,  y = .01, label = "Christi Matri 1966", cex=3.5,   hjust = 0) + 
 annotate("text", x = 24.03,  y = .01, label = "Mysterium 1965", cex=3.5,   hjust = 0) + 
annotate("text", x = 25.03,  y = .01, label = "Mense Maio 1965", cex=3.5,   hjust = 0) + 
annotate("text", x = 26.03,  y = .01, label = "Ecclesiam 1964", cex=3.5,   hjust = 0) + 
annotate("text", x = 27.03,  y = .51, label = "Pacem 1963", cex=3.5,   hjust = 0) +
annotate("text", x = 28.03,  y = .01, label = "Paenitentiam 1962", cex=3.5,   hjust = 0) +
annotate("text", x = 29.03,  y = .01, label = "Aeterna Dei 1961", cex=3.5,   hjust = 0) +
annotate("text", x = 30.03,  y = .51, label = "Mater 1961", cex=3.5,   hjust = 0) +
annotate("text", x = 31.03,  y = .51, label = "Princeps 1959", cex=3.5,   hjust = 0) +
annotate("text", x = 32.03,  y = .01, label = "Grata Recordatio 1959", cex=3.5,   hjust = 0) +
annotate("text", x = 33.03,  y = .01, label = "Sacerdotii 1959", cex=3.5,   hjust = 0) +
annotate("text", x = 34.03,  y = .01, label = "Ad Petri 1959", cex=3.5,   hjust = 0) +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +scale_fill_hue(c=45, l=80) + scale_fill_manual(values=c("#FFFF66", "#66CC99")) + ggtitle("Post 1958 Encyclicals\nDistribution of Topics") + geom_segment(aes(x = 33, y = -.05, xend = 1.5, yend = -.05), arrow = arrow(length = unit(0.5, "cm"))) + annotate("text", x = 17,  y = -.08, label = "years", cex=3.6, angle=90)

dev.copy2pdf(device = quartz, file = "/Users/genovesefederica/Dropbox/cc_encycl/replication/figures/Rplot_topicmodels.pdf",width = 7, height = 8)


#############
# Validation#
#############

# Semantic predictions

data.valid <- read.csv("/Users/genovesefederica/Dropbox/cc_encycl/replication/encycl_semanticvalid.csv", header=T)

head(data.valid)
output.valid1<- lm(data.valid[,5]~data.valid[,9]) #qualitative coding
output.valid2<- lm(data.valid[,5]~data.valid[,10]) # Catholic social teaching
mean.diff.valid1<- output.valid1$coef[2]
mean.diff.valid2<- output.valid2$coef[2]
mean.diff.valid1
mean.diff.valid2
se.valid1<- sqrt(diag(vcov(output.valid1)))[2]
se.valid2<- sqrt(diag(vcov(output.valid2)))[2]
se.valid1
se.valid2

##and the confidence intervals (parametrically)
eighty.perc<- 1.28
ninety5.perc<- 1.96

##now creating the figure for Cluster Quality
plot(c(0.2,2.3)~c(0.2,2.3), pch='', xlim=c(-0.2, 0.6),xlab='', ylab='', main='', frame.plot=F, axes=F)
title(xlab='(LDA method) - (Human association)', col='black')

abline(v=0, lty=2, col='black')
axis(1, seq(-0.2, 0.6, by=0.2),  seq(-0.2, 0.6, by=0.2), col='black')
draw.arrow<- function(se, mean,sd, num, weight){
	arrows(sd*se + mean, num, mean - sd*se, num, len=0, lwd=weight, col='black')
	}
points(mean.diff.valid1, 2, pch=18, col='black', cex = 2)
draw.arrow(se.valid1, mean.diff.valid1, eighty.perc, 2, 6)
draw.arrow(se.valid1, mean.diff.valid1, ninety5.perc, 2, 1)
text(mean.diff.valid1+ 0.003, 2.27, 'Qualitative', col='black')
text(mean.diff.valid1+ 0.006, 2.15, 'grouping', col='black')
points(mean.diff.valid2, 1, pch=18, col='black', cex = 2)
draw.arrow(se.valid2, mean.diff.valid2, eighty.perc, 1, 6)
draw.arrow(se.valid2, mean.diff.valid2, ninety5.perc, 1, 1)
text(mean.diff.valid2 + -0.010, 1.25, 'Social teachings', col='black')
text(mean.diff.valid2 + -0.001, 1.125, 'grouping', col='black')

dev.copy2pdf(device = quartz, file = "/Users/genovesefederica/Dropbox/cc_encycl/replication/figures/Rplot_validation.pdf",width = 6.5, height = 4.5)

###########################
# Wordfish robustness test#
###########################

# m2 <- t(m)
# new_m <- wfm(m2)
dim(new_m)
docs(new_m)

# 27 -> (very spiritual)
# 34 ->  laudato si
wf <- wordfish(new_m, dir=c(27,34), verbose=TRUE)
summary(wf)
plot(wf, cex=.6, xlab="Latent dimension", main="Wordfish Distribution of Post 1958 Encyclicals")
 # Express the functions that will prepare the word matrix for LDA (wc = word count, cs = charachter count, cs.nospace = character count without spaces)
 dev.copy2pdf(device = quartz, file = "/Users/genovesefederica/Dropbox/cc_encycl/replication/figures/Rplot_wf.pdf",width = 5.5, height = 7)


###############################
# Descriptives (frequencies)  #
# based on the classification # 
# of texts suggested by the   # 
# topic model analysis        #
###############################

# Note: produce the following graphs one by one!

###############################
# All Encyclicas

tdm0 <- TermDocumentMatrix(docs, control = list(wordLengths = c(1, Inf)))
idx <- which(dimnames(tdm0)$Terms == "r")
(freq.terms <- findFreqTerms(tdm0, lowfreq=420))
term.freq <- rowSums(as.matrix(tdm0))
term.freq <- subset(term.freq, term.freq >=420)
df0 <- data.frame(term = names(term.freq), freq = term.freq)
ggplot(df0, aes(x=term, y=freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") +coord_flip() + theme(axis.text.y = element_text(colour="grey20",size=8))
###############################

# Less Political Encyclicas
cname <- file.path(".", "encyclicals_it_leastpolitical")
cname
dir(cname)
docs <- Corpus(DirSource(cname))
 class(docs)
 class(docs[[1]])
summary(docs)
inspect(docs[1])
encycl <- rownames(summary(docs))
encycl
getTransformations()
# replace "/" used sometimes to separate alternative words
for (j in seq(docs))
{
docs[[j]] <- gsub("/", " ", docs[[j]])
docs[[j]] <- gsub("@", " ", docs[[j]])
}
# Preparation of documents (conversion to lower case, removal of punctuation, yada yada)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, PlainTextDocument)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs2 <- tm_map(docs, stemDocument)
docs <- tm_map(docs2, removeWords, c(stopwords('italian'),"che", "ma", "per", "anch", "ogni", "modo", "piu", "perch", "cos","alcuni", "sempr", "quando", "cose", "part", "senza", "esser","aver","cio","puo","possono","molto","tutt","fatto", "solo","tale","stess","pid","qual","gesd","deg","deve","infatti","dell","essa","stesso","ag","ci","gia","delluomo","eg","cf","pu","d","gv","san"))
#docs <- docs
inspect(docs[1])
dtm <- DocumentTermMatrix(docs)
dtm

# Check out the raw frequencies
m <- as.matrix(dtm)
v <- sort(colSums(m), decreasing=TRUE)
head(v, 14)
words <- names(v)
d <- data.frame(word=words, freq=v)

tdm1 <- TermDocumentMatrix(docs, control = list(wordLengths = c(1, Inf)))
idx <- which(dimnames(tdm1)$Terms == "r")
(freq.terms <- findFreqTerms(tdm1, lowfreq=300))
term.freq <- rowSums(as.matrix(tdm1))
term.freq <- subset(term.freq, term.freq >=300)
df1 <- data.frame(term = names(term.freq), freq = term.freq)
ggplot(df1, aes(x=term, y=freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") +coord_flip() + theme(axis.text.y = element_text(colour="grey20",size=8))
###############################

# More Political Encyclicas
cname <- file.path(".", "encyclicals_it_mostpolitical")
cname
dir(cname)
docs <- Corpus(DirSource(cname))
 class(docs)
 class(docs[[1]])
summary(docs)
inspect(docs[1])
encycl <- rownames(summary(docs))
encycl
getTransformations()
# replace "/" used sometimes to separate alternative words
for (j in seq(docs))
{
docs[[j]] <- gsub("/", " ", docs[[j]])
docs[[j]] <- gsub("@", " ", docs[[j]])
}
# Preparation of documents (conversion to lower case, removal of punctuation, yada yada)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, PlainTextDocument)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs2 <- tm_map(docs, stemDocument)
docs <- tm_map(docs2, removeWords, c(stopwords('italian'),"che", "ma", "per", "anch", "ogni", "modo", "piu", "perch", "cos","alcuni", "sempr", "quando", "cose", "part", "senza", "esser","aver","cio","puo","possono","molto","tutt","fatto", "solo","tale","stess","pid","qual","gesd","deg","deve","infatti","dell","essa","stesso","ag","ci","gia","delluomo","eg","cf","pu","d","gv","san"))
#docs <- docs
inspect(docs[1])
dtm <- DocumentTermMatrix(docs)
dtm

# Check out the raw frequencies
m <- as.matrix(dtm)
v <- sort(colSums(m), decreasing=TRUE)
head(v, 14)
words <- names(v)
d <- data.frame(word=words, freq=v)

tdm2 <- TermDocumentMatrix(docs, control = list(wordLengths = c(1, Inf)))
idx <- which(dimnames(tdm2)$Terms == "r")
(freq.terms <- findFreqTerms(tdm2, lowfreq=200))
term.freq <- rowSums(as.matrix(tdm2))
term.freq <- subset(term.freq, term.freq >=200)
df2 <- data.frame(term = names(term.freq), freq = term.freq)
ggplot(df2, aes(x=term, y=freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") +coord_flip() + theme(axis.text.y = element_text(colour="grey20",size=8))
###############################

# Combine frequency results: Multiplot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }
 if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

p0 <- ggplot(df1, aes(x=term, y=freq)) + geom_bar(stat = "identity", fill="black", colour="black") + xlab("Terms in All Encyclicals Combined") + ylab("Count\nNote: plot illustrates >400 word count") +coord_flip() + theme(axis.text.y = element_text(colour="grey20",size=8))  + theme(axis.text.x = element_text(size=6)) 
p0 

p1 <- ggplot(df1, aes(x=term, y=freq)) + geom_bar(stat = "identity", fill="#999999", colour="black") + xlab(" ") + ylab("Count") +coord_flip() + theme(axis.text.y = element_text(colour="grey20",size=10))  + theme(axis.text.x = element_text(size=8)) + ggtitle("Less Political Encyclicals") 
#\nNote: plot illustrates >300 word count
p1 

p2 <- ggplot(df2, aes(x=term, y=freq)) + geom_bar(stat = "identity", fill="#FFCCCC", colour="black") + xlab(" ") + ylab("Count") +coord_flip() + theme(axis.text.y = element_text(colour="grey20",size=10))  + theme(axis.text.x = element_text(size=8)) + ggtitle("More Political Encyclicals")  # Texts in this sample are 1963 Pacem, 1967 Populorum Progressio, 1981 Laborem Exercens, 1987 Sollicitudo Rei Socialis,  1991 Centesimus Annua,  1998 Fides Et Ratio,   and 2015 Laudato Si
#\nNote: plot illustrates >200 word count
p2

multiplot(p1,p2, cols=2)
dev.copy2pdf(device = quartz, file = "/Users/genovesefederica/Dropbox/cc_encycl/replication/figures/Rplot_topwordfreqall.pdf",width = 7.5, height = 8)


##########################
# Structural Topic Models#
##########################

rm(list=ls(all=TRUE))

setwd("/Users/genovesefederica/Dropbox/cc_encycl")

rawdata <- read.csv("encycl_stm.csv", header=T) 
data <- na.omit(rawdata) 
processed <- textProcessor(documents=data$enctext, metadata=data, language="english", verbose=TRUE)

#structure and index for usage in the stm model. Verify no-missingness.
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
#output will have object meta, documents, and vocab
docs <- out$documents
vocab <- out$vocab
meta <-out$meta

# Selection of model
	encyclicalFitselect <- selectModel(out$documents,
                         out$vocab,
                         K=3,
                         max.em.its=25, 
                         data=out$meta,
                         seed=123, runs=10)
            
plotModels(encyclicalFitselect)     
            
# Estimation for 3-topic graph (no covariates)          
for(i in 1:50){	
	Seed<-i	
	encyclicalFit <- stm(out$documents,
                         out$vocab,
                         K=3, #K=4
                         max.em.its=25, 
                         data=out$meta,
                         seed=Seed) #, emtol=1)
 }  
                        
# Exploring topics
labelTopics(encyclicalFit, n=15)

arrowLine <- function(x0,y0,x1,y1,nArrow=1,...)
{
  lines(c(x0,x1),c(y0,y1),...)
  Ax=seq(x0,x1,length=nArrow+1)
    Ay=seq(y0,y1,length=nArrow+1)
  for (i in 1:nArrow)
  {
    arrows(Ax[i],Ay[i],Ax[i+1],Ay[i+1],...)
  }
}

# Plot topic quality
topicQuality(encyclicalFit, out$documents)
arrowLine(-14.4,9.99225,-14.0,9.9926)
text(-13.8, 9.99275, 'Topic 1 top words:', col='red', cex=.9)
text(-13.8, 9.9927, 'verita, lavoro,', col='red', cex=.9)
text(-13.8, 9.99265, 'bene, proprio...', col='red', cex=.9)

arrowLine(-8.0,9.993355,-8.6,9.99293)
text(-9.15, 9.9929, 'Topic 3 top words:', col='chartreuse4', cex=.9)
text(-9.15, 9.99285, 'dio, chiesa,', col='chartreuse4', cex=.9)
text(-9.15, 9.9928, 'cristo, vita...', col='chartreuse4', cex=.9)

arrowLine(-7.2,9.99345,-7.45,9.9928)
text(-7.55, 9.99275, 'Topic 2 top words:', col='blue', cex=.9)
text(-7.55, 9.9927, 'fede, spirito,', col='blue', cex=.9)
text(-7.55, 9.99265, 'sviluppo, concilio...', col='blue', cex=.9)

dev.copy2pdf(device = quartz, file = "/Users/genovesefederica/Dropbox/cc_encycl/replication/figures/Rplot_stm_exco.pdf",width = 9, height = 5.5)


# Topic words with their corpus frequency & covariates

for(i in 1:50){	
	Seed<-i	
	encyclicalFit <- stm(out$documents,
                         out$vocab,
                         K=3, #K=4
                         prevalence =~ meta$crisis+meta$socialteaching, 
                         max.em.its=25, 
                         data=out$meta,
                         seed=Seed) #, emtol=1)
 }  
   
# Plot topic quality
#topicQuality(encyclicalFit, out$documents)
#arrowLine(-14.4,9.9905,-14.0,9.9912)
#text(-13.8, 9.9915, 'Topic 1 top words:', col='red', cex=.9)
#text(-13.8, 9.9914, 'verita, lavoro,', col='red', cex=.9)
#text(-13.8, 9.9913, 'bene, proprio...', col='red', cex=.9)

#arrowLine(-8.0,9.993,-8.6,9.99265)
#text(-9.15, 9.9926, 'Topic 3 top words:', col='chartreuse4', cex=.9)
#text(-9.15, 9.9925, 'dio, chiesa,', col='chartreuse4', cex=.9)
#text(-9.15, 9.9924, 'cristo, vita...', col='chartreuse4', cex=.9)

#arrowLine(-7.2,9.9934,-7.6,9.9925)
#text(-7.7, 9.9924, 'Topic 2 top words:', col='blue', cex=.9)
#text(-7.7, 9.9923, 'fede, spirito,', col='blue', cex=.9)
#text(-7.7, 9.9922, 'sviluppo, concilio...', col='blue', cex=.9)
 
# Perspectives depicts differences between two topics: 
# plot.STM(encyclicalFit, type="perspectives", topics=c(1,2), labeltype="frex", n=80)
# plot.STM(encyclicalFit, type="perspectives", topics=c(1,3), labeltype="frex", n=80)

# model topic prevalence over two binary variables 

prep.twobin <- estimateEffect(1:3 ~ crisis+socialteaching, 
                       encyclicalFit,
                       meta=meta, 
                       uncertainty = "Global",  nsims=150)

plot.estimateEffect(prep.twobin, 
                    covariate = "crisis", 
                    topics = c(1:3),
                    model= encyclicalFit, 
                    # printlegend=FALSE,
                    method="difference", #mean difference in topic proportions for two different values of the covariate
                    cov.value1=0, cov.value2=1,  ci.level=.8, xlim=c(-0.15,0.1),
                    xlab="Topic Prevalence Contrast:  No Crisis (0) vs Crisis (1) ", cex=.6
                    )
dev.copy2pdf(device = quartz, file = "/Users/genovesefederica/Dropbox/cc_encycl/replication/figures/Rplot_topicprev_crisis.pdf",width = 7, height = 8)



######################
# Laudato Si analysis#
######################

setwd("/Users/genovesefederica/Dropbox/cc_encycl/replication")

cname <- file.path(".", "laudato_si")
cname
dir(cname)
docs <- Corpus(DirSource(cname))
 class(docs)
 class(docs[[1]])
summary(docs)
inspect(docs[1])
encycl <- rownames(summary(docs))
encycl
getTransformations()

# replace "/" used sometimes to separate alternative words
for (j in seq(docs))
{
docs[[j]] <- gsub("/", " ", docs[[j]])
docs[[j]] <- gsub("@", " ", docs[[j]])
}

# Preparation of documents (conversion to lower case, removal of punctuation, yada yada)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, PlainTextDocument)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs2 <- tm_map(docs, stemDocument)
docs <- tm_map(docs2, removeWords, c(stopwords('italian'),"che", "ma", "per", "anch", "ogni", "modo", "piu", "perch", "cos","alcuni", "sempr", "quando", "cose", "part", "senza", "esser","aver","cio","puo","possono","molto","tutt","fatto", "solo","tale","stesso"))
#docs <- docs
inspect(docs[1])
dtm <- DocumentTermMatrix(docs)
dtm

# Check out the raw frequencies
m <- as.matrix(dtm)
v <- sort(colSums(m), decreasing=TRUE)
head(v, 14)
words <- names(v)
d <- data.frame(word=words, freq=v)
wordcloud(d$word, d$freq, min.freq=40)
wordcloud(d$word, d$freq, min.freq=40, random.order=FALSE, rot.per=0)
df <- d[d$freq >= 50, ] 
df
df= as.data.frame(cbind(Overall.Cond= 1:16, Freq= c(199,157,144,135,134,123,87,81,76,76,76,72,67,65,63,55)))
df
df.freq= as.vector(rep(df$Overall.Cond, df$Freq))

x <- barplot(table(df.freq), xaxt="n",main="Top Word Frequencies in 2015 Laudato Si Encyclical",col="forestgreen",ylab="Word Frequency")
labs <- paste(names(table(df.freq)), c("Uman","Dio","Mondo","Vita","Ambient","Altr","Social","Sviluppo","Creat","Natura","Terra","Degr","Realta","Polit","Cura","Comun"))
text(cex=1, x=x+.5, y=-4.5, labs, xpd=TRUE, srt=45, pos=2)
dev.copy2pdf(device = quartz, file = "/Users/genovesefederica/Dropbox/cc_encycl/replication/figures/Rplot_topwordfreq2015.pdf",width = 6, height = 6)


############

# Prepare for LDA with 2 topics
m.all <- wfm(m, word.margin=2)
m.lda <- wfm.to.lda(m.all)
set.seed(20)
mod <- lda.collapsed.gibbs.sampler(m.lda$documents, 2, 
    m.lda$vocab, 50, 1, 1, compute.log.likelihood=TRUE)

# Show the topic model results
top.topic.words(mod$topics, 10, by.score=TRUE)
top.words <- top.topic.words(mod$topics, 10, by.score=TRUE)
top.words 

topicwords1 <- c("dio", "mondo","ambient","creat","sviluppo","natura","terra","realta","cura","comun")
topicwords2 <- c("uman", "vita","altr","social","degr","polit","amor","paesi","umanita","relazion")

freqtopicwords1 <- c(157,144,134,123,81,76,76,67,63,55)
freqtopicwords2 <- c(199,135,123,87,72,65,48,47,47,45)

par(mfrow=c(1,2))

#plot.new()
#text(x=0.5, y=0.5, "Topic 1", ps=330)
wordcloud(topicwords1, freqtopicwords1, random.order=FALSE, rot.per=0, colors="green4")
#text(x=2.5, y=0.5, "Topic 2", ps=330)
wordcloud(topicwords2, freqtopicwords2, random.order=FALSE, rot.per=0, colors="turquoise4")
mtext("Word Clouds of Topics in 2015 Laudato Si Encyclical: Key Words", side=3, outer=TRUE, line=-3,cex = 1.15)
mtext("Topic 1 (left) and Topic 2 (right)", side=1, outer=TRUE, line=-3)
dev.copy2pdf(device = quartz, file = "/Users/genovesefederica/Dropbox/cc_encycl/replication/figures/Rplot_wordclouds.pdf",width = 7, height = 6)

