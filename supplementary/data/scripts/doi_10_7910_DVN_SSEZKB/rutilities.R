require(xtable)
require(ggplot2)
require(knitr)
require(reshape2)
require(FactoMineR)
require(qgraph)
require(rmarkdown)
require(plyr)
require(dplyr)
require(HH)
require(plotrix)

#functions
f.noblanks <- function(data) {
  data[as.character(data)==''] <- NA
  return(data)
}

f.nona <- function(data) {
  return(data[!is.na(data),])
}

f.percent.debug <- function (x) {
  return(c(100*x/sum(x),sum(x)))
}

f.percent <- function(data) {
  data2<-t(apply(data,1,f.percent.debug))
#  colnames(data2)[ncol(data2)]<-'n'
  return(data2)
}

f.percent.col <- function (data,sum=NULL) {
  if(is.null(sum)) sum<-sum(data)
  return(100*data/sum)
}

f.word.wrap <- function(string,length) {
  return(lapply(strwrap(string,length,simplify=F),paste,collapse="\n"))
}
#input functions
f.read.tsv <- function(filename,headers=TRUE) {
  return (read.table(filename,sep="\t",allowEscapes = TRUE,encoding='UTF-8',quote="\"",header=headers,stringsAsFactors=FALSE))
}

f.read.csv <- function(filename,headers=TRUE) {
  return (read.table(filename,sep=",",allowEscapes = TRUE,encoding='UTF-8',quote="\"",header=headers,stringsAsFactors=FALSE))
}

f.temp<-function (x,trans) { 
  temp2<-trans[trans$label==x,2] 
  return(ifelse(is.na(temp2),x,temp2))
}

f.trans <- function(factor,trans) {
  if(class(factor)!="factor") return(factor) 
  else {
    return(factor(sapply(factor,function (x) return(trans[as.character(x),2]))))
#    temp<-levels(factor)
#    tempt<-data.frame(sapply(temp,f.temp,trans))
#    return(mapvalues(factor,as.character(tempt[2,]),as.character(tempt[1,])))
    }
  }

f.input <- function() {
  dataf<-NULL
  for (rown in 1:nrow(batches)) {
    datanew <- read.table(paste(batches[rown,'filename'],".tsv",sep=""), encoding="UTF-8",sep="\t",quote="\"",header=FALSE) 
    datanew$batch = factor(paste(batches[rown,'language'],"-",batches[rown,'desc'],sep=""))
    if(exists("dataf")) dataf <- rbind(dataf,datanew) else dataf <- datanew
  }
  return(dataf)
}