scale<-c('not at all'=0,'not much'=1,'somewhat'=2,'quite a bit'=3,'very'=4)
roles2 <- read.table("roles.tsv", fileEncoding="UTF-8",sep="\t",header=T) # questions

questtype<-c(rep(1,times=4),rep(2,times=6),1,3,3,4,4,5)  # Legitimacy indicator key
liknamesc <- read.table("liknamesc.tsv", encoding="UTF-8",sep="\t") # questions as row
liknamesc$type <- c(replicate(8,0),as.list(questtype),0,as.list(questtype),0,0,0,0)
colnames(liknamesc) <- c('Bosnian','English','Brief','Kratko','Type')
legscalesB<-c("Instrumentalno","Normativno","Tradicijonalno","Racionalno/zakonski","Pojedinačno") # Legitimacy source types in Bosnian
legscalesE<-c("Inst.","Norm.","Trad.","R/Legal","Ind.")  # Legitimacy source types in English
cor_labele <- c('Correlation Indicated to L-score (no paid)','Correlation Indicated to L-score (no staff)','Correlation Indicated to L-score (all)','Correlation Indicated to Conceptual','Correlation Conceptual to L-score','Type','Source')
filenames <- c('Org1','Org2','Org3','Org4')

#functions
f.splitroles <- function(data) {    
  for(i in 1:length(roles2[,"Bosnian"])) {
    data <- cbind(data,grepl(roles2[i,"Bosnian"],data[,"V4"]))
    names(data)[ncol(data)] <- paste("R",i)
  }
  return(data)
}

f.indexmeans <- function(data) {
  data$II  <- rowMeans(data[,9:12],na.rm=TRUE)
  data$IN <- rowMeans(data[,13:19],na.rm=TRUE)
  data$IT <- rowMeans(data[,20:21],na.rm=TRUE)
  data$IR <- rowMeans(data[,22:23],na.rm=TRUE)
  data$IP <- data[,24]
  data$CI  <- rowMeans(data[,26:29],na.rm=TRUE)
  data$CN <- rowMeans(data[,30:36],na.rm=TRUE)
  data$CT <- rowMeans(data[,37:38],na.rm=TRUE)
  data$CR <- rowMeans(data[,39:40],na.rm=TRUE)
  data$CP <- data[,41]
  
  return(data)
}

f.lik.nroles <- function(data) {
  result <- data.frame(row.names=unique(data$org))
  for (org in unique(data$org))  {
    for(i in 1:nrow(roles2))  {
      result[org,paste("R",i)] <- sum(data[data$org==org,paste("R",i)]==TRUE)
    }
    result[org,"total"] <- nrow(data[data$org==org,])
  }
  result <- data.frame(addmargins(as.matrix(result),margin=1))
  return(result)
}

f.lik.legmeans <- function(data) {  # build table with legitimacy score means, SE for each org
  result <- data.frame(unique(data$org))
  names(result) <- 'org'
  
  for (org in unique(data$org))  {
    result[result$org==org,"lmean"] <- mean(data[data$"R 4"==FALSE & data$org==org,"lscore"],na.rm=TRUE)
    result[result$org==org,"lmeanfull"] <- mean(data[data$org==org,"lscore"],na.rm=TRUE)
    result[result$org==org,"lmeanse"] <- std.error(data[data$org==org,"lscore"])
    
  }
  result <- rbind(result,data.frame(org="All",
                                    lmean= mean(data[data$"R 4"==FALSE,"lscore"],na.rm=TRUE),
                                    lmeanfull = mean(data[,"lscore"],na.rm=TRUE),
                                    lmeanse = std.error(data[,"lscore"])))
  return(result)
  }

f.lik.legsources <- function(data,select.lang) { # for each org, means for each potential source
  
  result2 <- data.frame(Organization=character(),Mean=numeric(),"Std. Error"=numeric())
  for (org in unique(data$org)) {
    result <- data.frame(Organization=character(),Mean=numeric(),"Std. Error"=numeric())
    for (column in 9:24) {
        result <- rbind(result,data.frame(Organization=org,
                                          Mean=5-mean(data[data$org==org,column],na.rm=TRUE),
                                          "Std. Error"=std.error(data[data$org==org,column])))
    }
    result$Indexkey <- as.integer(liknamesc[9:24,"Type"])
    if(select.lang == "English") col<-"Brief" else col<-"Kratko"
    result$Source <- reorder(liknamesc[9:24,col],as.numeric(liknamesc[9:24,"Type"]))
    result2 <- rbind(result2,result)
  }
  return(result2)
}

f.lik.legsources2 <- function(data) { # for each org, means for each potential source

  result <- melt(apply(lik5[,26:41],2,function(x) {5-mean(x,na.rm=TRUE)}))
  colnames(result) <- 'All'
  result$SE <- melt(apply(lik5[,26:41],2,function(x) {std.error(x)}))
  for (org in unique(data$org)) {
    result[,org] <- melt(apply(lik5[data$org==org,26:41],2,function(x) {5-mean(x,na.rm=TRUE)}))
  }
  result$Rank <- as.integer(17-rank(result[,1]))
  result[,"Correlation rank"] <- as.integer(17-rank(correlations[,2]))
  result$Type <- legscalesE[as.integer(liknamesc[26:41,"Type"])]
  result$Source <- reorder(liknamesc[26:41,"Brief"],result[,1])
  return(result)
}

f.lik.legsources3 <- function(data) { # for each org, means for each potential source
  
  result <- melt(apply(lik5[,9:24],2,function(x) {5-mean(x,na.rm=TRUE)}))
  colnames(result) <- 'All'
  result$SE <- melt(apply(lik5[,9:24],2,function(x) {std.error(x)}))
  for (org in unique(data$org)) {
    result[,org] <- melt(apply(lik5[data$org==org,9:24],2,function(x) {5-mean(x,na.rm=TRUE)}))
    result[((result[,org]<(result$All-result$SE*2)) | result[,org]>(result$All+result$SE*2)),paste(org,"*")] <- '*'
  }
  result$Type <- legscalesE[as.integer(liknamesc[9:24,"Type"])]
  result$Source <- reorder(liknamesc[9:24,"Brief"],result[,1])
  return(result)
}

f.lik.correlations <- function(data) { # correlations for each potential source
  result <- data.frame(C_lscore1=numeric(),C_lscore2=numeric())
  
  for (column in 9:24)  {
    result <- rbind(result,data.frame(C_lscore1=cor(data[data$"R 4"==FALSE & data$"R 5"==FALSE,"lscore"],5-data[data$"R 4"==FALSE & data$"R 5"==FALSE,column],use="pairwise.complete.obs",method="spearman"),
                                      C_lscore1=cor(data[data$"R 4"==FALSE,"lscore"],5-data[data$"R 4"==FALSE,column],use="pairwise.complete.obs",method="spearman")))
  }
  result$Indexkey <- legscalesE[as.integer(liknamesc[9:24,"Type"])]
  result$Source <- reorder(liknamesc[9:24,"Brief"],as.numeric(result[,2]))
  colnames(result) <- cor_labele[c(1:2,6:7)]
  return(result)
}

f.lik.correlations2 <- function(data,select.lang) { # correlations w/SE
  result <- data.frame(C_lscore2=numeric(),lower=numeric(),upper=numeric(),n=integer())
  
  for (column in 9:24)  {
    good <- data$"R 4"==FALSE & !is.na(data$lscore) & !is.na(data[,column])
    x1 <- data[good,"lscore"]
    x2 <- 5-data[good,column]
    stderr = 1.0 / sqrt(sum(good) - 3)
    delta = 1.96 * stderr
    r=cor(x1,x2,use="pairwise.complete.obs",method="spearman")
    lower = tanh(atanh(r) - delta)
    upper = tanh(atanh(r) + delta)
    result <- rbind(result,data.frame(C_lscore2=r,
                            lower=lower,
                            upper=upper,
                            n=sum(good)))
    
                                      
  }
  result$Indexkey <- as.integer(liknamesc[9:24,"Type"])
  if(select.lang=="English") col<-"Brief" else col<-"Kratko"
  result$Source <- reorder(liknamesc[9:24,col],as.numeric(result[,1]))
  return(result)
}

#preprocess
f.lik.trans <- function(input, levels1, levels2) {
#  levels1<-enc2utf8(levels1) # Encoding(levels1)<-'UTF-8'
  tempf<-factor(input,ordered=T,levels=levels1)
  return(mapvalues(tempf,levels1,levels2))
}

f.lik.input <- function(filenames) {
  
  for (filename in filenames) {
    liknew <- read.table(paste(filename,".tsv",sep=""), fileEncoding="UTF-8",sep="\t")
    liknew$organization = factor(filename)
    if(exists("lik")) lik <- rbind(lik,liknew) else lik <- liknew
  }

  lik[,2]<-f.lik.trans(lik[,2],c('Do 18','19-30','31-45','46-60','61 i vise'),
                       c('Up to 18','19-30','31-45','46-60','61 and above'))

  lik[,3]<-f.lik.trans(lik[,3],c('Zenski','Muski',''),c('Female','Male',''))
  lik[,5]<-f.lik.trans(lik[,5],c('Manje od 6 mjeseci','6-12 mjeseci','1-3 godine','3-5 godina','5-15 godina','vise od 15 godina'),
                       c('Less than 6 months','6-12 months','1-3 years','3-5 years','5-15 years','Over 15 years'))
  lik[,6]<-f.lik.trans(lik[,6],c('nimalo','ne puno','nesto','dosta','jako'),
                       c('not at all','not much','somewhat','quite a bit','very well'))
#  lik[,7]<-factor(as.character(lik[,7]))
  lik[,7]<-f.lik.trans(lik[,7],c('nikada','od jedan to tri puta u zadnjih sest mjeseci','barem jednom mjesecno','barem jednom sednicno','vecinu dana'),
                       c('never','1-3 times in last 6 months','at least once a month','at least once a week','most days'))
  lik[,8]<-f.lik.trans(lik[,8],c('nimalo','ne puno','nesto','dosta','jako'),
                       c('not at all','not much','somewhat','quite a bit','very'))
  return(lik)
}

f.lik.pre <- function(filenames) {
  
  lik <- f.lik.input(filenames)
  lik[as.Date(lik$"V1","%m/%d/%Y")<as.Date("8/1/2014","%m/%d/%Y"),13]<-NA # error in question 13 coding, addressed 8/1/2014
  lik3 <- f.noblanks(lik) # replace blank data with NA
  lik4 <- f.splitroles(lik3) # split roles into binaries
  lik5 <- f.indexmeans(lik4) # build indexes for each legitimacy type
  lik5$lscore=apply(lik5,1,function(x) scale[x["V8"]]) # replace text with numeric legitimacy scores
  return(lik5)
}

f.lik.pca <- function(data) {
  temp <- PCA(data)
  data.pca <- prcomp(na.omit(data), cor=TRUE)
  summary(data.pca)
  return(data.pca)
}

#reports
f.lik.o.nroles <- function(data,select.org) {
  
  nroles <- f.lik.nroles(data)
  if(select.lang=="English") total<-"Total" else total<-"Ukupno"
  colnames(nroles)<-c(as.character(roles2[,select.lang]),total)

  if(select.org=="")
    print(xtable(nroles,digits=0),type="html")
  else
    print(xtable(nroles[c(select.org,"Sum"),],digits=0),type="html")
    
}

f.lik.o.legmeans <- function(data,select.org) {
  if(select.org=="")
    print(xtable(data,digits=2),type="html")
  else
    print(xtable(data[c(select.org,"All"),],digits=2),type="html")
  
}

f.lik.o.indicators <- function(data) {
  ggplot(data, aes(x=Source, y=Mean, group=Organization,fill=Organization)) +
    geom_errorbar(width=.1, aes(ymin=Mean-Std..Error, ymax=Mean+Std..Error)) +
    geom_point(shape=21, size=3, fill="White",color=Organization) + coord_flip() +
    ylim(1,5)
}

f.lik.o.sourcesbar <- function(sourcedata,select.org,select.lang) {
  legsources <- f.lik.legsources(sourcedata,select.lang)
  if(select.org=="")
      data<-legsources
  else
      data<-legsources[legsources$Organization==select.org,]
  plot <- ggplot(data=data, aes(x=Source, y=Mean, fill=Organization)) + 
    geom_bar(stat="identity", position=position_dodge()) + 
    ylab(liknamesc[43,select.lang]) +
    xlab(liknamesc[44,select.lang]) +
    theme(axis.text.x = element_text(face="bold", size=20),
          axis.text.y = element_text(face="bold", size=15),
          axis.title.x = element_text(face="bold", size=20),
          axis.title.y = element_text(face="bold", size=20)) +
    coord_flip()
  if(!select.org=="")
      plot <- plot + geom_errorbar(width=.1, aes(ymin=Mean-1.96*Std..Error, ymax=Mean+1.96*Std..Error))
  return(plot)    

}

f.lik.o.correlations <- function(correlations) {  
  print(xtable(correlations[order(-correlations[,2]),]),type="html",include.rownames=FALSE)
}

f.lik.o.correlations2 <- function(data,select.lang) {
  correlations2 <- f.lik.correlations2(data,select.lang)
                                       
  ggplot(correlations2, aes(x=Source, y=C_lscore2, group=1)) +
    geom_errorbar(width=.1, aes(ymin=lower, ymax=upper)) +
    theme(axis.text.x = element_text(face="bold", size=20),
          axis.text.y = element_text(face="bold", size=15),
          axis.title.x = element_text(face="bold", size=20),
          axis.title.y = element_text(face="bold", size=20)) +
    geom_point(shape=21, size=3, fill="White") + coord_flip() +
    ylab(liknamesc[45,select.lang]) +
    xlab(liknamesc[44,select.lang]) +
    ylim(-1,1)
}

f.lik.o.legsources2 <- function(legsources2) {  
  print(xtable(legsources2[order(-legsources2[,1]),]),type="html",include.rownames=FALSE)
}

f.lik.o.legsources3 <- function(legsources3) {  
  print(xtable(legsources3[order(-legsources3[,1]),]),type="html",include.rownames=FALSE)
}

f.lik.o.chars <- function(data) {
  for(col in c(2,3,5,6,7,8)) {
    cat(paste("<H2>1.",col,". ",liknamesc[col,paste(select.lang)],"</H2>",sep=""))
    barplot(table(data[,col]),cex.names=1,names.arg=gsub(" ","\n",levels(data[,col])),col="blue")
    }
}

f.lik.o.genbar <- function(data,select.org) {
  png(paste("./images/",select.org,"-bar1.png",sep=""),height=1000,width=1000)
  print({f.lik.o.sourcesbar(data,select.org,select.lang)})
  dev.off()
}

f.lik.o.genbar2 <- function(data,select.lang) {
  png(paste("./images/-bar2-",select.lang,".png",sep=""),height=1000,width=1000)
  print({f.lik.o.correlations2(data,select.lang)})
  dev.off()
}

f.lik.o.rebuild <- function() {

  png("./images/qgraph1.png",height=1000,width=1000)
  print({
    qgraph(lik.con.cm,
           layout="spring",
           maximum=1,
           color=as.integer(liknamesc[c(9:24),"Type"])+2,
           labels=gsub(" ","\n",liknamesc[9:24,"Brief"]),
           label.cex=2,
           quiet=TRUE)
  })
  dev.off()

  png("./images/qgraph2.png",height=1000,width=1000)
  print({
    qgraph(lik.ind.cm,
           layout="spring",
           maximum=1,
           color=as.integer(liknamesc[c(8,9:24),"Type"])+2,
           labels=gsub(" ","\n",liknamesc[8:24,"Brief"]),
           label.cex=2,
           quiet=TRUE)
  })
  dev.off()
}

f.lik.o.comments <- function(data,select.lang) {
  print(xtable(matrix(na.omit(data[,25]))),type="html")
}

# commands
source('rutilities.R',encoding='UTF-8')

lik5 <- f.lik.pre(filenames)
lik5$lindex <- rowSums((5-lik5[,9:24])*(5-lik5[,26:41]),na.rm=TRUE) # build composite index
legmeans <- f.lik.legmeans(lik5)
correlations <- f.lik.correlations(lik5)
legsources2 <- f.lik.legsources2(lik5)
legsources3 <- f.lik.legsources3(lik5)
lik6 <- lik5[lik5$"R 4"==FALSE,]

#build models
lik.con <- lik6[,c(63,26:41)]
lik.ind <- cbind(lscore=lik6$lscore,lik6[,9:24])
lik.com <- cbind(lscore=lik6$lscore,(5-lik6[,9:24])*(5-lik6[,26:41]))

lik.con.lm <- lm(lscore ~ V26 + V27 + V28 + V29 + V30 + V31 + V32 + V33 + V34 + V35 + V36 + V37 + V38 + V39 + V40 + V41,data=lik.con)
lik.ind.lm <- lm(lscore ~ V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24,data=lik.ind)
lik.com.lm <- lm(lscore ~ V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24,data=lik.com)
# nice but brief column names
colnames(lik.con)<- liknamesc[c(8,26:41),"Brief"]
colnames(lik.ind)<- liknamesc[c(8,9:24),"Brief"]
colnames(lik.com)<- liknamesc[c(8,9:24),"Brief"]

#build correlation matrices
lik.ind.cm=cor(na.omit(lik.ind),method = "spearman")
lik.con.cm=cor(na.omit(lik.con[,-1]),method = "spearman")

f.lik.o.rebuild() # rebuild graphs
select.lang <- 'English'
f.lik.o.genbar(lik5,"")
f.lik.o.genbar2(lik5,select.lang)

render(input   = "summary.Rmd",output_file  = "summary.html",output_format="html_document",,encoding='UTF-8',quiet=TRUE)


