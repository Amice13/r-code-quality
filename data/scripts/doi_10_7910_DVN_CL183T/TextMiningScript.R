######################
# Clementine 29/03/17
######################

# from: https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html


######################
# Uncomment if needed
######################
 # Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")
 # install.packages(Needed, dependencies=TRUE)
setwd("~/Documents/clementine/CarinaPolicy")
cname <- "/Users/clementineCottineau/Documents/clementine/CarinaPolicy/textAnalysis2"
stat.comp<-  function( x,y){
  K <-length(unique(y))
  n <-length(x)
  m <-mean(x)
  TSS <-sum((x-m)^2)
  nk<-table(y)
  mk<-tapply(x,y,mean)
  BSS <-sum(nk* (mk-m)^2)
  result<-c(mk,100.0*BSS/TSS)
  names(result) <-c( paste("G",1:K),"% epl.")
  return(result)
}

dir(cname)
 
library(tm)
library(cluster)
library(SnowballC)
library(ggplot2)


docs <- Corpus(DirSource(cname))   

summary(docs)   
docs
#inspect(docs[2])
#docs = tm_map(docs, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
  })
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, "`")
docs <- tm_map(docs, toSpace, "‘")
docs <- tm_map(docs, toSpace, "_")
docs <- tm_map(docs, removePunctuation)

docs <- tm_map(docs, removeWords, c("section", "chapter", "page", "report"))   
docs <- tm_map(docs, tolower)   
docs <- tm_map(docs, removePunctuation)   

#writeLines(as.character(docs[2]))

docs <- tm_map(docs, content_transformer(gsub), pattern = "advanced manufacturing|advanced manufacture", replacement = "advancedManufacturing")
docs <- tm_map(docs, content_transformer(gsub), pattern = "creative and digital", replacement = "creativeDigital")
docs <- tm_map(docs, content_transformer(gsub), pattern = "northern powerhouse", replacement = "northernPowerhouse")
docs <- tm_map(docs, content_transformer(gsub), pattern = "health and life", replacement = "healthlife")
docs <- tm_map(docs, content_transformer(gsub), pattern = "low carbon", replacement = "lowCarbon")
docs <- tm_map(docs, content_transformer(gsub), pattern = "financial and professional", replacement = "financialProfessional")
docs <- tm_map(docs, content_transformer(gsub), pattern = "job market", replacement = "jobMarket")

docs <- tm_map(docs, removeNumbers)   
docs <- tm_map(docs, removeWords, stopwords("english"))   
docs <- tm_map(docs, stemDocument)   
docs <- tm_map(docs, stripWhitespace)   
#docs <- tm_map(docs, PlainTextDocument)   

dtm <- DocumentTermMatrix(docs)   
dtm 
dtm.m = as.matrix(dtm)
tdm <- TermDocumentMatrix(docs)   
tdm  
tdm.m = as.matrix(tdm)
n = as.data.frame(rownames(tdm))
colnames(n) = c("n")
n$l = nchar(as.character(n$n))
head(n[order(-n$l),], 15)
tdm.df = as.data.frame(tdm.m)
write.csv(as.data.frame(tdm.df),"frequencyUseTermsByDoc.csv")

my.df <- as.data.frame(tdm.df)
my.df.scale <- scale(my.df)
write.csv(as.data.frame(my.df.scale),"scaledFrequencyUseTermsByDoc.csv")
d <- dist(my.df.scale,method="euclidean")
fit <- hclust(d, method="ward")
plot(fit)
numberOfGroups = 4
group_terms = cutree(fit, k=numberOfGroups)
groups = data.frame("ID" = rownames(my.df.scale), "group" = group_terms)
write.csv(groups, paste0(numberOfGroups, "_groups_cah_terms.csv"))
relative.df = sweep(my.df,2,colSums(my.df),`/`)
leg = sapply(relative.df, stat.comp,y=group_terms)
write.csv(leg, "termsGroupDesc.csv")


dtm.df = as.data.frame(dtm.m)
my.df <- as.data.frame(dtm.df)
my.df.scale <- scale(my.df)
d <- dist(my.df.scale,method="euclidean")
fit <- hclust(d, method="ward")
plot(fit)
head(my.df)
numberOfGroups = 3
group_terms = cutree(fit, k=numberOfGroups)
groups = data.frame("ID" = rownames(my.df.scale), "group" = group_terms)
write.csv(groups, paste0(numberOfGroups, "_groups_cah_docs.csv"))
          
  
my.df <- as.data.frame(tdm.df)
my.df.scale <- scale(my.df)

findAssocs(tdm, 'growth', 0.9)
findAssocs(as.TermDocumentMatrix(relative.df), "growth", 0.9)
freq <- colSums(as.matrix(dtm))   
length(freq)   
ord <- order(freq)   
m <- as.matrix(dtm)   
dim(m) 
#head(m)




dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   
findAssocs(dtms, 'product', 0.9)


freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
wf <- data.frame(word=names(freq), freq=freq)   
write.csv(wf, "frequencyUseTerms.csv")   

listWords = c("product", "skill","hous","transport","advancedmanufactur",
              "ineq","divers","special","sector","invest","infrastructur","connect",
              "creativedigit","environment","financialprofession","healthlif",
              "lowcarbon","spatial","labour","job","devolut","northernpowerhous","innov",
              "growth","citi","region", "busi", "area", "employ")
library(igraph)
par(mfrow = c(2,2), mar = c(2,2,3,2))
for (word in listWords){
as = findAssocs(dtms, word, 0.9)
if (length(as[1][[1]]) > 0){
graph = data.frame()
k=0
for (i in seq(length(as))){
  a = as[i]
  n = names(a)
  l = length(as[i][[1]])
  if (l > 0){
    for ( j in seq(l)){
      k = k+1
      graph[k,1] = n
      graph[k,2] = names(as[i][[1]][j])
      graph[k,3] = as[i][[1]][j]
    }
  }
}
graph
g <- graph.data.frame(graph, directed = T)
plot(g, vertex.label.dist = 1)
title(word)
}}




wf[wf$word %in% listWords,]
sum(wf$freq)
head(wf)

p <- ggplot(subset(wf, freq>50), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   


findAssocs(dtm, c("product" , "ineq"), corlimit=0.9) # specifying a correlation limit of 0.98  

# library(wordcloud)   
# set.seed(142)   
# wordcloud(names(freq), freq, min.freq=25)   
# wordcloud(names(freq), freq, max.words=100)   



# 
# # Clustering
# 
# dtmss <- removeSparseTerms(dtm, 0.9 ) # This makes a matrix that is only 15% empty space, maximum.   
# inspect(dtmss)   
# d <- dist(t(dtmss), method="euclidian")   
# fit <- hclust(d=d, method="ward")   
# fit   
# 
# #plot(fit, hang=-1)   
# 
# plot.new()
# plot(fit, hang=-1)
# groups <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using   
# rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around the 5 clusters   
