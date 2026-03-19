

plot_ts<-function(me_ts,var,time="ttp",var2=NULL,ci=2,pcols=c("blue","red")){
  if(time!="ttp"){  
    me_ts$ttp<-me_ts$week
  }
  me_ts$tvar<-me_ts[,var] 
  me_ts$party<-as.factor(me_ts$party) 
  

  ttp<-me_ts$ttp
  
  par(mar = c(2, 2, 2, 6)) # Set the margin on all sides to 2
  par(xpd=TRUE)
  
  
  plot(me_ts$ttp,me_ts$tvar,col=pcols[me_ts$party],pch=c(1,19)[as.factor(me_ts$winner)],ylab="Ideology",xlab="Weeks to Primary",bty="n",axes = T,xlim = c(min(me_ts$ttp)-10, max(me_ts$ttp)+10))
  #  lines(me_ts$ttp[me_ts$party=="R" & me_ts$winner==T],me_ts$tvar[me_ts$party=="R" & me_ts$winner==T],col="red")
  #  lines(me_ts$ttp[me_ts$party=="D" & me_ts$winner==T],me_ts$tvar[me_ts$party=="D" & me_ts$winner==T],col="blue")
  #  lines(me_ts$ttp[me_ts$party=="R" & me_ts$winner==F],me_ts$tvar[me_ts$party=="R" & me_ts$winner==F],col="red",lty=2)
  #  lines(me_ts$ttp[me_ts$party=="D" & me_ts$winner==F],me_ts$tvar[me_ts$party=="D" & me_ts$winner==F],col="blue",lty=2)
  if(time=="week"){
    
    pdates<-as.POSIXct(unique(docvars(dfc,"primary_date")))
    for(i in 1:length(pdates)){
      abline(v=pdates[i],col="black")
    }
    
    abline(v=as.POSIXct("2020-11-03"),col="red")
  }
  
  
  if(!is.null(var2)){
    me_ts$theta_se<-me_ts[,var2] 
    
    for(i in 1:nrow(me)){
      segments(x0=me_ts$ttp[i],y0=me_ts$tvar[i]-ci*me_ts$theta_se[i],x1=me_ts$ttp[i],y1=me_ts$tvar[i]+ci*me_ts$theta_se[i],col=pcols[me_ts$party[i]])
    }
    
  } 

  pos1<-c(max(ttp)+5,.7*max(me_ts$tvar))
  pos2<-c(+5,.7*min(me_ts$tvar))

    
  abline(v=0)
  legend(
    x=max(as.numeric(ttp))+5,y=.8*max(me_ts$tvar),
    legend=levels(as.factor(me_ts$party)),
    col=pcols,
    pch=19,
    bty="n")
  legend(
    x=max(as.numeric(ttp)+5),y=.7*min(me_ts$tvar),
    legend=levels(me_ts$winner),
    col=pcols[1],
    pch=c(1,19),
    bty="n")
}



# pch is unused
# how to plot CI's nicer?
# what types of lines?


plot_rc<-function(m6,var,var2=NULL,ci=2,vip=NULL,var3="nominate_dim1",lab1="Twitter Text",lab2="NOMINATE",pcols=c("red","blue"),sz=.9,borders=T){
  m6$theta_scores<-m6[,var] 
  m6$dim1<-m6[,var3]
  if(cor(m6$dim1,m6$theta_scores)<0){m6$theta_scores<-m6$theta_scores*-1}
  D<-m6[m6$party=="D",]
  R<-m6[m6$party=="R",]
  par(xpd=TRUE)
  
  if(borders==T){
  plot(m6$dim1,as.numeric(m6$theta_scores),col=pcols[m6$party],xlab=lab2,ylab=lab1,pch=19)
  }else{plot(m6$dim1,as.numeric(m6$theta_scores),col=pcols[m6$party],xlab=lab2,ylab=lab1,pch=19,bty="n")}
  
    
  if(!is.null(var2)){
    m6$theta_se<-m6[,var2] 
    
    for(i in 1:nrow(me)){
      segments(x0=m6$dim1[i],y0=m6$theta_scores[i]-ci*m6$theta_se[i],x1=m6$dim1[i],y1=m6$theta_scores[i]+ci*m6$theta_se[i],col=pcols[m6$party[i]])
    }
    
  } 
  
  
  legend(
    x="bottomright",
    legend=levels(m6$party),
    col=pcols,
    pch=19
  )
  
  
  pos1<-2/3*max(m6$theta_scores)
  pos2<-1/3*max(m6$theta_scores)
  pos0<-min(m6$theta_scores)+1/2*(max(m6$theta_scores)-min(m6$theta_scores))
  pos1<-min(m6$theta_scores)+2/3*(max(m6$theta_scores)-min(m6$theta_scores))
  pos2<-min(m6$theta_scores)+1/3*(max(m6$theta_scores)-min(m6$theta_scores))
  
  pos0y<-min(m6$dim1)+1/2*(max(m6$dim1)-min(m6$dim1))
  pos1y<-min(m6$dim1)+2/3*(max(m6$dim1)-min(m6$dim1))
  pos2y<-min(m6$dim1)+1/3*(max(m6$dim1)-min(m6$dim1))
  
  
  
  #lines(lowess(m6$dim1,m6$theta_scores),col="blue")
  lines(lowess(m6$dim1[m6$party=="R"],m6$theta_scores[m6$party=="R"]),col=pcols[1])
  lines(lowess(m6$dim1[m6$party=="D"],m6$theta_scores[m6$party=="D"]),col=pcols[2])
  text(pos0y,pos0,labels=paste0("Total=",round(cor(m6$dim1,m6$theta_scores),2)),col="black")
  text(pos2y,pos1,labels=paste0("D=",round(cor(D$dim1,D$theta_scores),2)),col="red")
  text(pos1y,pos2,labels=paste0("R=",round(cor(R$dim1,R$theta_scores),2)),col="blue")
  pcols2<-c(pcols[2],pcols[1])  
 
  
    if(!is.null(vip)){
       name<-strsplit(m6$bioname,split = ",")
  m6$lastname<-sapply(name,"[[",1)
  pl<-m6[m6$bioname%in%vip,]
      plr<-pl[pl$party=="R",]
      pld<-pl[pl$party=="D",]
         textplot(pld$dim,pld$theta_scores,words = pld$lastname,new = F,show.lines = T,cex=sz,col="red")
      textplot(plr$dim,plr$theta_scores,words = plr$lastname,new = F,show.lines = T,cex=sz,col="blue")
    }

 
  
  
}




# Coefficient Plot function
mult_plot<-function(allModelFrame,level_order=level_order,legend=F,sz=17,l=1){
  zp1 <- ggplot(allModelFrame, aes(colour = Model))
  zp1 <- zp1 + scale_color_manual(values=colscheme)
  zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2*l)
  zp1 <- zp1 + geom_linerange(aes(x = factor(Variable,levels=level_order), ymin = Coefficient - SE*interval1,
                                  ymax = Coefficient + SE*interval1),
                              lwd = 1*l, position = position_dodge(width = 1/2))
  zp1 <- zp1 + geom_pointrange(aes(x = factor(Variable,levels=level_order), y = Coefficient, ymin = Coefficient - SE*interval2,
                                   ymax = Coefficient + SE*interval2),
                               lwd = 1/2*l, position = position_dodge(width = 1/2),
                               shape = 19, fill = "WHITE")
  zp1 <- zp1 + theme_minimal()
  zp1 <- zp1 + coord_flip() +   theme(axis.text = element_text(size=sz,family='sans')
                                      ,axis.title = element_blank()
                                      ,axis.line.y = element_blank()
                                      ,axis.ticks = element_blank()
                                      ,legend.title = element_text(size=sz)
                                      ,legend.text = element_text(size=sz*0.75)
                                      ,panel.grid.major.y = element_blank()
                                      ,panel.grid.minor.y = element_blank()
                                      ,legend.position='bottom')
  if(legend==F){zp1<-zp1+theme(legend.position='none')}
  return(zp1)
}

# hard colors for text
# soft colors for dots

plot_ws<-function(m6,model,var="theta_nb",var2="nominate_dim1",terms,n=100,pcols=c("blue","red"),at=T,span=1.5,sz=.7,dfm=NULL,limit=100,borders=T){
  
  par(xpd=TRUE)
  
  
  R<-m6[m6$party=="R",]
  D<-m6[m6$party=="D",]
  
  words<-as.data.frame(t(model$param))
  
  if(!is.null(dfm)){
  col<-colSums(dfm)
  
  words<-words[col>limit,]
  }
  
  words$logs<-log(words$D/words$R)
  words<-words[order(words$logs),]
  rownames(words)
  
  terms<-words$logs
  names(terms)<-rownames(words)
  
  if(at==F){
    
    terms<-terms[!grepl("#",names(terms))]
  }
  
  
  ta<-terms[1:n]
  tt<-terms[length(terms):(length(terms)-(n-1))]
  ta<-ta*-1
  tt<-tt*-1
  
#  c1<-(-0.03*seq(x[1],x[2],length.out = 40)^2)
 # c2<-(-0.03*seq(x[1],x[2],length.out = 40)^2)
  
  #c3<-(-0.03*seq(x[1],x[2],length.out = 40)^2)
  # c4<-(-0.03*seq(x[1],x[2],length.out = 40)^2)
  
 # ta<-normalize(rt)
  
  #tt<-normalize(dt)
  

  
  m6$dim1<-m6[,var2]
  m6$theta_scores<-m6[,var]
  
  x<-c(min(m6$dim1)-span,max(m6$dim1)+span)
  y<-c(min(m6$theta_scores)-span,max(m6$theta_scores)+span)

  
  if(borders==T){  
  plot(m6$dim1,as.numeric(m6$theta_scores),col=pcols[as.factor(m6$party)],xlab="NOMINATE 1",ylab="Twitter Position",ylim=c(x[1],x[2]),xlim=c(y[1],y[2]),pch=19)
  }else{
  plot(m6$dim1,as.numeric(m6$theta_scores),col=pcols[as.factor(m6$party)],xlab="NOMINATE 1",ylab="Twitter Position",ylim=c(x[1],x[2]),xlim=c(y[1],y[2]),pch=19,bty="n")
  }
    lines(lowess(m6$dim1[m6$party=="R"],m6$theta_scores[m6$party=="R"]),col=pcols[1])
  lines(lowess(m6$dim1[m6$party=="D"],m6$theta_scores[m6$party=="D"]),col=pcols[2])
  textplot(seq(x[1]+1/3*span,x[2]-1/3*span,length.out = length(tt)),tt,words = names(tt),new = F,show.lines = F,cex=sz,col="blue")
  textplot(seq(x[2]-1/3*span,x[1]+1/3*span,length.out = length(ta)),ta,words = names(ta),new = F,show.lines = F,cex=sz,col="red")
  
}



