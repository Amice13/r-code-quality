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
require(plotfix)

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

#preprocess

f.preprocess <- function(dataf,questions) {
  scale<-c(1,2,3,4,5,NA)
  for (i in 1:nrow(questions)) {
    if (questions[i,'type']=='likert' | questions[i,'type']=='multiple3') {
      opts<-factors[factors$factor_type==questions[i,'likert_type'] & factors$lang=='bos',]
      names(scale)<-opts[3:8]
      dataf<-cbind(dataf,apply(dataf,1,function(x) scale[x[i]])) # replace text with numeric likert score
      newcol<-paste("r",as.character(i),sep="")
      names(dataf)[ncol(dataf)] <- newcol
    } else if (questions[i,'type']=='numeric' | questions[i,'type']=='likertn' | questions[i,'type']=='likertn2') {
      newcol<-paste("r",as.character(i),sep="")
      dataf<-cbind(dataf,as.numeric(as.character(dataf[,i])))
      names(dataf)[ncol(dataf)] <- newcol
    } else if (questions[i,'type']=='likertn3' | questions[i,'type']=='multiplen') {
      newcol<-paste("r",as.character(i),sep="")
      dataf<-cbind(dataf,as.numeric(substr(as.character(dataf[,i]),1,1)))
      names(dataf)[ncol(dataf)] <- newcol
    } else if (questions[i,'type']=='factor') {
      dataf[,i] <- factor(f.noblanks(as.character(dataf[,i]))) # replace blank data with NA
    }
  }
  
  return(dataf)  
}
f.buildoptions <- function(dataf,questions,filename) {
  
  allstring<-data.frame(label=character())
  for (i in as.numeric(rownames(questions[questions$type=="factor",]))) {
    allstring<-rbind(allstring,data.frame(label=unique(dataf[,i])))
  }
  for (i in as.numeric(rownames(questions[questions$type=='multiplefactor',]))) {
    allstring<-rbind(allstring,data.frame(label=unique(unlist(strsplit(as.character(dataf[,i]),split=", ")))))
  }
  alldataf=data.frame(label=unique(allstring[,1]),trans="")
  filedf<-f.read.tsv(filename)
  newfile<-merge(filedf,alldataf,by.x=1,by.y=1,all.x=TRUE,all.y=TRUE,)
  write.table(newfile[,1:2],filename,sep="\t",fileEncoding='UTF-8',row.names = FALSE)
}

f.mult_answers <- function(dataf,var,division) {
  allopts<-as.character(dataf[,var])
  options <- unique(unlist(strsplit(allopts,split=", ")))
  if (division!="") {
    result<-data.frame(levels(dataf[,division]))
    for (option in options)  {
      tempdf<-ddply(dataf,division,function (x) { return(length(grep(option,x[,var]))) })
      tempdf2<-tempdf[!is.na(tempdf[,division]),]
      result<-cbind(result,tempdf2[2])
    }
    counts<-ddply(dataf,division,nrow)
    counts<-counts[!is.na(counts[,division]),]
    result<-cbind(result[1],data.frame(apply(result[,-1],2,function (x) { return(100*x/counts[,-1])})))
    result<-cbind(result,counts[,-1])
    colnames(result)<-c('Answer',options,'n')
  } else {
    result<-data.frame(Answer="All")
    for (option in options)  {
      result=cbind(result,Frequency=100*length(grep(option,allopts))/nrow(dataf))
    }
    result<-cbind(result,nrow(dataf))
    colnames(result)[-1]<-c(options,'n')
  }
  # rownames(result)<-result[,1]
  # result<-result[,-1]
  return(result)
}

f.output <- function(dataf,questions,division,type,labels,trans,cols=NULL,coln=NULL) {
  emptyf<-data.frame(row.names=1,c('1','2','3','4','5'),rep(0,5))
  for (i in 2:nrow(questions)) {
    #   print(paste("<h2>",questions[i,'name'],"</h2><br />"))
    fieldname<-paste("r",as.character(i),sep="")
    if (paste("V",as.character(i),sep="") != division) {
      title<-as.character(lapply(strwrap(questions[i,"name"],70,simplify=F),paste,collapse="\n"))
      if (questions[i,'type']=='likert' | questions[i,'type']=='likertn' | questions[i,'type']=='likertn2') {
        if(division!="") {
          gbar2<-tapply(dataf[,fieldname],list(dataf[,fieldname],dataf[,division]),length)
          #         gbar2 <- cbind(c(1:nrow(gbar2)),gbar2)
          gbar3<-data.frame(merge(emptyf,gbar2,by.x=0,by.y=0,all.x=TRUE)[-1])
          gbar4<-as.data.frame(apply(gbar3,1,function (x) { return(ifelse(is.na(x),0,x))}))
          #         gbar4<-gbar4[-1,]
        } else {
          gbar2<-ddply(dataf,fieldname,nrow)
          gbar2<-gbar2[!is.na(gbar2[1]),]
          #          gbar2<-data.frame(tapply(dataf[,fieldname],list(dataf[,fieldname]),length))
          colnames(gbar2)[2]<-labels
          rownames(gbar2)<-gbar2[,1]
          gbar3<-data.frame(merge(emptyf,gbar2,by.x=0,by.y=0,all.x=TRUE)[-2],row.names = 1)
          gbar4<-t(data.frame(apply(gbar3,2,function (x) { return(ifelse(is.na(x),0,x))})))
        }
        if (questions[i,'type']=='likertn2') {
          gbar4<-t(gbar4)
          gbar4[,1]=1:5
          evsfieldname<-questions[i,'other']
          tempdf<-data.frame(table(evs[,evsfieldname]))
          tempdf<-cbind(tempdf,table(evs[evs$v303>=1978,evsfieldname]))[-3]
          if (i==45) tempdf[,1]<-c(4,3,1) else tempdf[,1]<-c(5,4,2,1)
          if (i %in% c(53,59)) cat(paste("<H2>",questions[i,'names'],"</H2>",sep=""))
          
          gbar5<-merge(gbar4,tempdf,by.x=1,by.y=1,all.x=TRUE,all.y=TRUE)
          gbar6<-as.data.frame(apply(gbar5,2,function (x) { return(as.numeric(ifelse(is.na(x),0,x)))}))
          colnames(gbar6)[c(ncol(gbar6)-1,ncol(gbar6))]<-c("EVS All","EVS Youth")
          gbar4<-t(gbar6)
        }
        colnames(gbar4)<-as.character(lapply(strwrap(
          factors[factors$factor_type==questions[i,'likert_type'] & factors$lang=="eng",3:7],
          10,simplify=F),paste,collapse="\n"))
        if (type==1) {
          print(likert(gbar4[-1,],
                       main=title,
                       as.percent=TRUE,
                       resize.width=0.5,
                       #              levelsname=factors[factors$factor_type==questions[i,'likert_type'] & factors$lang=="eng",3:7],
                       sub="Likert Scale"))
        } else {
          cat(paste('<H2>',title,'</H2>'))
          #           print(xtable(f.percent(gbar4)),type="html")
          print(xtable(data.frame(f.percent(gbar4)[-1,])),type="html")
        }
      }
      else if (questions[i,'type']=='factor' | questions[i,'type']=='special1') {
        fieldname<-paste("V",as.character(i),sep="")
        if (questions[i,'type'] == 'special1') {
          fieldname<-'r3'
        }
        #       dataf[,fieldname]<-f.trans(dataf[,fieldname],trans)
        if (division != "") {
          tempdf<-data.frame(table(dataf[,c(division,fieldname)]))
          colnames(tempdf)=c("Var1","Answer","Freq")
          tempdf2<-dcast(tempdf,Var1 ~ Answer)
          tempdf3<-cbind(data.frame(tempdf2[,1]),f.percent(tempdf2[,-1]))
          if(type==1) {
            tempdf4<-melt(tempdf3[,-ncol(tempdf3)])
            labels.wrap  <- f.word.wrap(levels(tempdf4[,"variable"]),10)
            colnames(tempdf4)<-c('Var1','Answer','Percentage')
            print(ggplot(tempdf4,aes(x=Answer,y=Percentage), width=0.85) + 
                    geom_bar(stat="identity",aes(fill=Var1),color="black",position="dodge") + ylab("Percentage") + xlab("") +
                    scale_x_discrete(labels=labels.wrap) +
                    theme(legend.position = "bottom") +
                    #    ,legend.text = element_text(size=10)) +, legend.direction="horizontal"
                    #                      guide = guide_legend(direction = "horizontal", title.position = "top",
                    #                                       label.position="bottom", label.hjust = 0.5, label.vjust = 0.5,
                    #                                       label.theme = element_text(angle = 90))) +
                    ggtitle(title))
          } else {
            cat(paste("<H2>",title,"</H2>"))
            rownames(tempdf2)<-tempdf2[,1]
            tempdf3<-tempdf2[,-1]
            tempdf4<-f.percent(tempdf3)
            colnames(tempdf4)[ncol(tempdf4)]<-'n'
            print(xtable(tempdf4),type="html")          
            cat("Values indicated are percentages.<br />")
          }
        }
        else {
          tempdf<-data.frame(table(dataf[,fieldname]))
          #         tempdf$Var1 <- reorder(tempdf$Var1,tempdf$Freq)
          labels.wrap  <- f.word.wrap(levels(tempdf$Var1),20)
          if(type==1) {
            print(ggplot(tempdf[order(tempdf$Freq),],aes(x=Var1,y=Freq),width=0.85) + 
                    geom_bar(stat="identity",fill="#DD8888") + ylab("Frequency") + xlab("") +
                    scale_x_discrete(labels=labels.wrap)+
                    coord_flip() +
                    ggtitle(title))
          } else {
            cat(paste("<H2>",title,"</H2>"))
            tempdf<-cbind(tempdf,f.percent.col(tempdf[,2]))
            colnames(tempdf)[ncol(tempdf)]<-"%"
            print(xtable(tempdf[order(tempdf[,3]),],digits=1),type="html")          
          }
        }
      }
      else if (questions[i,'type']=='multiplefactor') {
        fieldname<-paste("V",as.character(i),sep="")
        manswers <- f.mult_answers(dataf,fieldname,division)
        labels.wrap  <- f.word.wrap(colnames(manswers)[c(-1,-ncol(manswers))],30)
        if(division!="") {
          if(type==1) {
            tempdf<-melt(manswers[-ncol(manswers)])
            print(ggplot(tempdf,aes(x=variable,y=value), width=0.85) + 
                    geom_bar(stat="identity",aes(fill=Answer),show.legend=TRUE,position="dodge") + xlab("") + ylab("% of respondents") +
                    coord_flip() +
                    scale_x_discrete(labels=labels.wrap)+
                    ggtitle(title))
          } else {
            cat(paste("<H2>",title,"</H2>"))
            rownames(manswers)<-manswers[,1]
            manswers<-manswers[,-1]
            #           colnames(manswers)<-c(labels.wrap,"n")
            print(xtable(manswers,digits=1),type="html")                    
          }
        } else {
          if(type==1) {
            tempdf<-melt(manswers[-ncol(manswers)])
            tempdf<-tempdf[,-1]
            print(ggplot(tempdf,aes(x=variable,y=value), width=0.85) + 
                    geom_bar(stat="identity",fill="#DD8888",show.legend=TRUE) + xlab("") + ylab("% of respondents") +
                    coord_flip() +
                    scale_x_discrete(labels=labels.wrap)+
                    ggtitle(title))
          } else {
            cat(paste("<H2>",title,"</H2>"))
            tempdf<-t(manswers[,-1])
            colnames(tempdf)[ncol(tempdf)]<-"%"
            print(xtable(tempdf,digits=1),type="html")                    
          }
        } 
      }
      else if (questions[i,'type']=='numeric') {
        fieldname<-paste("r",as.character(i),sep="")
        if(division=="") {
          if(type==1) {
            print(ggplot(data.frame(Var1=na.omit(dataf[,fieldname])),aes(x=Var1), width=0.85) + 
                    geom_histogram(fill="#DD8888",bins=10,color="black") + ylab("Frequency") + xlab("") +
                    ggtitle(title))
          } else {
            tempdf<-data.frame(f.percent.col(table(dataf[,fieldname])))
            #           colnames(tempdf[ncol(tempdf)])<-"%"
            cat(paste('<H2>',title,'</H2>'))
            print(xtable(tempdf,digits=1),type="html")
          }
        } else {
          tempdf <- data.frame(Var1=dataf[,fieldname],Var2=dataf[,division])
          if (type==1) {
            print(ggplot(subset(tempdf,complete.cases(tempdf)),aes(x=Var1), width=0.85) + 
                    geom_histogram(fill="#DD8888",bins=10,color="black") + ylab("Frequency") + xlab("") +
                    facet_wrap(~Var2) +
                    ggtitle(title))
          } else {
            cat(paste('<H2>',title,'</H2>'))
            print(xtable(table(tempdf),digits=1),type="html")
          }
        }
      }
      else if (questions[i,'type']=='text') {
        fieldname<-paste("V",as.character(i),sep="")
        cat(paste("<H2>",title,"</H2>",sep=""))
        if(division=="") {
          ttable<-dataf[dataf[,fieldname]!='',c(cols,fieldname)]
          colnames(ttable)<-c(coln,'Answer')
          print(xtable(ttable),type="html")
        } else {
          ttable<-dataf[dataf[,fieldname]!='',c(division,cols,fieldname)]
          colnames(ttable)<-c('Break',coln,'Answer')
          for (divis in levels(dataf[,division])) {
            if(nrow(subset(ttable,ttable$Break==divis)) > 0) {
              cat(paste("<H3>",divis,"</H3>",sep=""))
              print(xtable(ttable[ttable[1]==divis,-1]),type="html")
            }
          }
        }
      }
      else if (questions[i,'type']=='multiple2') {
        cat(paste("<H2>",questions[i,'names'],"</H2>",sep=""))
        tempdf<-data.frame(Category=stud.quest[13:26,1],
                           "Belong %"=apply(stud.data2[,c(13:26)],2,function (x) { return (100*sum(ifelse(na.omit(x)=="Da",1,0))/length(x)) }),
                           "EVS bel.%"=apply(evs[evs$v303>=1978,c("v10","v11","v12","v13","v14","v16","v17","v18","v19","v20","v21","v22","v23","v24")],2,function (x) { return (100*sum(ifelse(x==1,1,0))/length(x)) }),
                           "Volunteer %"=apply(stud.data2[,c(27:40)],2,function (x) { return (100*sum(ifelse(na.omit(x)=="Da",1,0))/length(x)) }),
                           "EVS Vol.%"=apply(evs[evs$v303>=1978,c("v28","v29","v30","v31","v32","v34","v35","v36","v37","v38","v39","v40","v41","v42")],2,function (x) { return (100*sum(ifelse(x==1,1,0))/length(x)) }))
        print(xtable(tempdf[order(tempdf[,2]),],digits=1),type="html")
      }
      else if (questions[i,'type']=='multiple3') {
        if (i==41) cat(paste("<H2>",questions[i,'names'],"</H2>",sep=""))
        cat(paste("<H3>",questions[i,'name'],"</H3>",sep=""))
        evsfieldname<-questions[i,'other']
        evs_youth<-table(evs[evs$v303>=1978,evsfieldname])
        evs_all<-table(evs[,evsfieldname])
        alumni<-table(stud.data2[,fieldname])
        tempdf<-data.frame(rbind("EVS Youth"=evs_youth,"EVS All"=evs_all,"Alumni"=alumni[c(3,2,1)]))
        tempdf<-f.percent(tempdf)
        colnames(tempdf)<-c(factors[factors$factor_type=="often" & factors$lang=="eng",5:3],'n')
        print(xtable(tempdf,digits=1),type="html")
        cat("Values indicated are percentages")
      }
    } # if
  }
}
