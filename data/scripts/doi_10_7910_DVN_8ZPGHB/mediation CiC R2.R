rm( list = ls() )
library(qte)
library(xtable)
library(haven)
library(AER)

#############
# FUNCTIONS #
#############

medestDiD<-function(y,d,m,t,x=NULL, noalways=FALSE){
  if (is.null(x)==1){
  p10=mean(m[d==0]); p11=mean(m[d==1]); p01=1-p11; p00=1-p10; comp=p11-p10
  comp0=mean(y[t==1 & d==0 & m==0])-p01/comp*(mean(y[t==0 & d==1 & m==0])-mean(y[t==0 & d==0 & m==0]))
  if (noalways!=1) comp1=mean(y[t==1 & d==1 & m==1])-p10/comp*(mean(y[t==0 & d==0 & m==1])-mean(y[t==0 & d==1 & m==1]))
  if (noalways==1) comp1=mean(y[t==1 & d==1 & m==1])
  totalc= comp1-comp0
  neta10=mean(y[t==1 & d==1 & m==0]); neta00=mean(y[t==0 & d==1 & m==0]);
  comp00=(mean(y[t==0 & d==0]*(1-m[t==0 & d==0]))-mean(y[t==0 & d==1]*(1-m[t==0 & d==1])))/comp
  comp10=neta10-neta00+comp00
  cdir0=comp10-comp0; cindir1=comp1-comp10
  ntdir0=mean(y[t==1 & d==1 & m==0])-mean(y[t==0 & d==1 & m==0])-(mean(y[t==1 & d==0 & m==0])-mean(y[t==0 & d==0 & m==0]))
  if (noalways!=1){
    comp01=mean(y[t==1 & d==0 & m==1])-mean(y[t==0 & d==0 & m==1])+comp00
    cdir1=comp1-comp01; cindir0=comp01-comp0
    atdir1=mean(y[t==1 & d==1 & m==1])-mean(y[t==0 & d==1 & m==1])-(mean(y[t==1 & d==0 & m==1])-mean(y[t==0 & d==0 & m==1]))
    results=c(ntdir0, atdir1, totalc, cdir1, cdir0, cindir1, cindir0 )}
  if (noalways==1) results=c(ntdir0, totalc, cdir0, cindir1 )
  }
  if (is.null(x)==0){
    if (noalways!=1){
      temp=glm(m~x,family=binomial(probit),subset = (d==0))
      p10=predict(temp,newdata=data.frame(x), type="response")
    }
    if (noalways==1) p10=rep(0,length(d))
    temp=glm(m~x,family=binomial(probit),subset = (d==1))
    p11=predict(temp,newdata=data.frame(x), type="response")
    p01=1-p11; p00=1-p10; comp=p11-p10
    temp=lm(y~x,subset=(t==1 & d==0 & m==0))
    yt1d0m0=predict(temp,newdata=data.frame(x))
    temp=lm(y~x,subset=(t==0 & d==1 & m==0))
    yt0d1m0=predict(temp,newdata=data.frame(x))
    temp=lm(y~x,subset=(t==0 & d==0 & m==0))
    yt0d0m0=predict(temp,newdata=data.frame(x))
    temp=lm(y~x,subset=(t==1 & d==1 & m==1))
    yt1d1m1=predict(temp,newdata=data.frame(x))
    temp=lm(y~x,subset=(t==0 & d==1 & m==1))
    yt0d1m1=predict(temp,newdata=data.frame(x))
    if (noalways!=1) temp=lm(y~x,subset=(t==0 & d==0 & m==1))
    yt0d0m1=predict(temp,newdata=data.frame(x))
    temp=lm(y~x,subset=(t==1 & d==1 & m==0))
    yt1d1m0=predict(temp,newdata=data.frame(x))
    if (noalways!=1) temp=lm(y~x,subset=(t==1 & d==0 & m==1))
    yt1d0m1=predict(temp,newdata=data.frame(x))
    ymt0d0=(y*(1-t)*(1-d))*(1-m*(1-t)*(1-d))
    ymt0d1=(y*(1-t)*(d))*(1-m*(1-t)*(d))
    temp=lm(ymt0d0~x, subset=(t==0 & d==0 ))
    ymt0d0=predict(temp,newdata=data.frame(x))
    temp=lm(ymt0d1~x, subset=(t==0 & d==1 ))
    ymt0d1=predict(temp,newdata=data.frame(x))

    comp0=yt1d0m0-p01/comp*(yt0d1m0-yt0d0m0)
    if (noalways!=1) comp1=yt1d1m1-p10/comp*yt0d0m1-yt0d1m1
    if (noalways==1) comp1=yt1d1m1
    totalc= mean(comp1-comp0)
    neta10=mean(yt1d1m0); neta00=mean(yt0d1m0);
    comp00=mean((ymt0d0-ymt0d1)/comp)
    comp10=neta10-neta00+comp00
    cdir0=comp10-mean(comp0); cindir1=mean(comp1)-comp10
    ntdir0=mean(yt1d1m0-yt0d1m0-(yt1d0m0-yt0d0m0))
    if (noalways!=1){
      comp01=mean(yt1d0m1-yt0d0m1)+comp00
      cdir1=comp1-comp01; cindir0=comp01-comp0
      atdir1=mean(yt1d1m1-yt0d1m1-(yt1d0m1-yt0d0m1))
      results=c(ntdir0, atdir1, totalc, cdir1, cdir0, cindir1, cindir0 )}
    if (noalways==1) results=c(ntdir0, totalc, cdir0, cindir1 )
  }
  results
}

mediationDiD=function(y,d,m,t,x=NULL,boot=1999, cluster=NULL, noalways=FALSE){
  temp=medestDiD(y=y,d=d,m=m,t=t,x=x,noalways=noalways)
  temp2=medbootDiD(y=y,d=d,m=m,t=t,x=x,boot=boot, cluster=cluster,noalways=noalways)
  se=apply(temp2, 2, sd)
  temp3=2*pnorm(-abs(temp/se))
  results=rbind(temp, se, temp3)
  if (noalways!=1) colnames(results)=c("nt.dir","at.dir", "c.tot", "c.dir.1", "c.dir.0",  "c.indir.1", "c.indir.0")
  if (noalways==1) colnames(results)=c("nt.dir", "c.tot", "c.dir.0",  "c.indir.1")
  rownames(results)=c("est","se","pval")
  results
}

medbootDiD<-function(y,d,m,t,x=NULL,boot=1999, cluster=NULL,noalways=FALSE){
  if (is.null(cluster)){
    obs<-length(y)
    bsamples=c( )
    for(i in 1:boot){
      sboot<-sample(1:obs,obs,TRUE)
      yb=y[sboot]; db=d[sboot]; mb<-m[sboot]; tb<-t[sboot]
      if ( (is.null(x)==0)  ){
        if (is.null(ncol(x))) xb<-x[sboot]
        if (is.null(ncol(x))==0) xb<-x[sboot,]
      }
      if ( (is.null(x)==1) ) xb=NULL
      est=c(medestDiD(y=yb,d=db,m=mb,t=tb,x=xb, noalways=noalways))
      bsamples<-rbind(bsamples, est)
    }
  }
  if (is.null(cluster)==0){
    temp<-sort(cluster); clusters<-min(cluster)
    for (i in 1:length(temp)){
      if (temp[i]>max(clusters)) clusters=c(clusters,temp[i])
    }
    key=cluster; bsamples=c(); temp=c()
    obs<-length(clusters)
    while(length(temp)<boot){
      sboot<-sample(clusters,obs,TRUE)
      db<-c(); yb<-c(); xb<-c() ; mb=c(); tb=c()
      for (k in 1:length(sboot)) {
        db<-c(db,d[key==sboot[k]]); yb<-c(yb,y[key==sboot[k]]); mb<-c(mb,m[key==sboot[k]]); tb<-c(tb,t[key==sboot[k]])
        if ( (is.null(x)==0) ){
          if (is.null(ncol(x))) xb<-c(xb,x[key==sboot[k]])
          if (is.null(ncol(x))==0) xb=rbind(xb,x[key==sboot[k],])
        }
      }
      if ( (is.null(x)==1)  ) xb=NULL
      est=c(medestDiD(y=yb,d=db,m=mb,t=tb, x=xb, noalways=noalways))
      bsamples<-rbind(bsamples, est)
      temp<-c(temp,1)
    }
  }
  bna=apply(bsamples, 1, sum)
  bsamples=bsamples[is.na(bna)==0,]
  if (sum(is.na(bna))>0) cat("Warning: ",sum(is.na(bna)>0)," bootstrap sample(s) dropped due to NA's")
  bsamples
}

mediationCiC=function(y,d,m,t,x=NULL,xlabels=NULL,boot=1999, cluster=NULL,noalways=FALSE, norand=FALSE, probs=seq(0.05, 0.95,0.05)){
  temp=medestCiC(y=y,d=d,m=m,t=t,x=x,xlabels=xlabels,noalways=noalways,  norand=norand, probs=probs)
  temp2=medbootCiC(y=y,d=d,m=m,t=t,x=x,xlabels=xlabels,boot=boot, cluster=cluster,noalways=noalways, norand=norand, probs=probs)
  se=apply(temp2, 2, sd)
  temp3=2*pnorm(-abs(temp/se))
  results=rbind(temp, se, temp3)
  if (norand==1) {
    if (noalways!=1) colnames(results)=c("ntdf.dir.1","ntco.dir.0", "atdf.dir.0", "atco.dir.1")
    if (noalways==1) colnames(results)=c("ntdf.dir.1","ntco.dir.0")
  }
  if (norand!=1){
  if (noalways!=1) colnames(results)=c("nt.dir","at.dir", "c.tot", "c.dir.1", "c.dir.0",  "c.indir.1", "c.indir.0", "n.est","a.est","c.est")
  if (noalways==1) colnames(results)=c("nt.dir", "c.tot", "c.dir.0",  "c.indir.1", "n.est", "c.est")
  }
  rownames(results)=c("est","se","pval")
  results
}

medestCiC=function(y,d,m,t,x=NULL,xlabels=NULL,noalways=FALSE, norand=FALSE, probs=NULL){

  if (norand!=1){
  if ( (is.null(x)==0) & (is.null(xlabels)==0) ){
    one_d=1-d
    if (noalways!=1){
      temp=glm(m~x,family=binomial(probit),subset = (d==0))
      p10=mean(predict(temp,newdata=data.frame(x), type="response"))
    }
    if (noalways==1) p10=0
    temp=glm(m~x,family=binomial(probit),subset = (d==1))
    p11=mean(predict(temp,newdata=data.frame(x), type="response"))
    p00=1-p10;p01=1-p11; c.est=(p11-p10); a.est=p10; n.est=p01;
    data=data.frame(y,m,d,one_d,t,x); colnames(data)=c("y","m", "d","one_d", "t",xlabels)
    data.m0=data[data$m==0,]
    data.m1=data[data$m==1,]

    # CiC for never takers
    nt.dir.temp=CiC(y~d, t=1, tmin1=0, tname="t", data=data.m0, x = xlabels, se=FALSE, probs=probs)
    nt.dir=nt.dir.temp$ate
    # CiC for always takers
    if (noalways!=1){
      at.dir.temp=CiC(y~one_d, t=1, tmin1=0, tname="t", data=data.m1, x = xlabels, se=FALSE, probs=probs)
      at.dir=((-1)*at.dir.temp$ate)
    }
    # CiC for effects on compliers - these calculations are only correct if D is randomly assigned
    c.dir.0.temp=CiC(y~one_d, t=1, tmin1=0, tname="t", data=data.m0, x = xlabels, se=FALSE, probs=probs)
    c.dir.0=((-1)*c.dir.0.temp$ate)*p00/(p11-p10)- nt.dir*p01/(p11-p10)
    if (noalways!=1){
      c.dir.1.temp=CiC(y~d, t=1, tmin1=0, tname="t", data=data.m1, x = xlabels, se=FALSE, probs=probs)
      c.dir.1=(c.dir.1.temp$ate)*p11/(p11-p10)- at.dir*p10/(p11-p10)
      c.tot=(mean(y[d==1 & t==1])-mean(y[d==0 & t==1])-p01*nt.dir-p10*at.dir)/(p11-p10)
      c.indir.0=c.tot-c.dir.1
    }
    if (noalways==1)  c.tot=(mean(y[d==1 & t==1])-mean(y[d==0 & t==1])-p01*nt.dir)/(p11-p10)
    c.indir.1=c.tot-c.dir.0
  }
  if ( (is.null(x)!=0) | (is.null(xlabels)!=0) ){
    one_d=1-d
    p10=mean(m[d==0 ]);p11=mean(m[d==1]);p00=1-p10;p01=1-p11; c.est=(p11-p10); a.est=p10; n.est=p01;
    data=data.frame(y,m,d,one_d,t); colnames(data)=c("y","m", "d","one_d", "t")
    data.m0=data[data$m==0,]
    data.m1=data[data$m==1,]
    # CiC for never takers
    nt.dir.temp=CiC(y~d, t=1, tmin1=0, tname="t", data=data.m0, se=FALSE, probs=probs)
    nt.dir=nt.dir.temp$ate
    # CiC for always takers
    if (noalways!=1){
      at.dir.temp=CiC(y~one_d, t=1, tmin1=0, tname="t", data=data.m1,  se=FALSE, probs=probs)
      at.dir=((-1)*at.dir.temp$ate)
    }
    # CiC for effects on compliers
    c.dir.0.temp=CiC(y~one_d, t=1, tmin1=0, tname="t", data=data.m0,  se=FALSE, probs=probs)
    c.dir.0=((-1)*c.dir.0.temp$ate)*p00/(p11-p10)- nt.dir*p01/(p11-p10)
    if (noalways!=1){
      c.dir.1.temp=CiC(y~d, t=1, tmin1=0, tname="t", data=data.m1,  se=FALSE, probs=probs)
      c.dir.1=(c.dir.1.temp$ate)*p11/(p11-p10)- at.dir*p10/(p11-p10)
      c.tot=(mean(y[d==1 & t==1])-mean(y[d==0 & t==1])-p01*nt.dir-p10*at.dir)/(p11-p10)
      c.indir.0=c.tot-c.dir.1
    }
    if (noalways==1) c.tot=(mean(y[d==1 & t==1])-mean(y[d==0 & t==1])-p01*nt.dir)/(p11-p10)
    c.indir.1=c.tot-c.dir.0
  }
  if (noalways!=1) results=cbind(nt.dir,at.dir, c.tot, c.dir.1, c.dir.0, c.indir.1, c.indir.0, n.est,a.est,c.est)
  if (noalways==1) results=cbind(nt.dir, c.tot, c.dir.0, c.indir.1, n.est, c.est)
  }
  # effects when D is not randomized and monotonicity of M in D does not hold 
  if (norand==1){
    if ( (is.null(x)==0) & (is.null(xlabels)==0) ){
      one_d=1-d
      data=data.frame(y,m,d,one_d,t,x); colnames(data)=c("y","m", "d","one_d", "t",xlabels)
      data.m0=data[data$m==0,]
      data.m1=data[data$m==1,]
      ntdf.dir.temp=CiC(y~d, t=1, tmin1=0, tname="t", data=data.m0, x = xlabels, se=FALSE, probs=probs)
      ntdf.dir.1=ntdf.dir.temp$ate
      if (noalways!=1){
      atdf.dir.temp=CiC(y~one_d, t=1, tmin1=0, tname="t", data=data.m1, x = xlabels, se=FALSE, probs=probs)
      atdf.dir.0=((-1)*atdf.dir.temp$ate)
      atco.dir.temp=CiC(y~d, t=1, tmin1=0, tname="t", data=data.m1, x = xlabels, se=FALSE, probs=probs)
      atco.dir.1=atco.dir.temp$ate
      }
      ntco.dir.temp=CiC(y~one_d, t=1, tmin1=0, tname="t", data=data.m0, x = xlabels, se=FALSE, probs=probs)
      ntco.dir.0=((-1)*ntco.dir.temp$ate)
    }
    if ( (is.null(x)!=0) | (is.null(xlabels)!=0) ){
      one_d=1-d
      data=data.frame(y,m,d,one_d,t); colnames(data)=c("y","m", "d","one_d", "t")
      data.m0=data[data$m==0,]
      data.m1=data[data$m==1,]
      ntdf.dir.temp=CiC(y~d, t=1, tmin1=0, tname="t", data=data.m0,  se=FALSE, probs=probs)
      ntdf.dir.1=ntdf.dir.temp$ate
      if (noalways!=1){
      atdf.dir.temp=CiC(y~one_d, t=1, tmin1=0, tname="t", data=data.m1,  se=FALSE, probs=probs)
      atdf.dir.0=((-1)*atdf.dir.temp$ate)
      atco.dir.temp=CiC(y~d, t=1, tmin1=0, tname="t", data=data.m1,  se=FALSE, probs=probs)
      atco.dir.1=atco.dir.temp$ate
      }
      ntco.dir.temp=CiC(y~one_d, t=1, tmin1=0, tname="t", data=data.m0,  se=FALSE, probs=probs)
      ntco.dir.0=((-1)*ntco.dir.temp$ate)
    }
    if (noalways!=1) results=cbind(ntdf.dir.1,ntco.dir.0, atdf.dir.0, atco.dir.1)
    if (noalways==1)results=cbind(ntdf.dir.1,ntco.dir.0)
  }
  results
}

medbootCiC<-function(y,d,m,t,x,xlabels, boot=1999, cluster=NULL, noalways=FALSE, norand=FALSE, probs=NULL){
  if (is.null(cluster)){
    obs<-length(y)
    bsamples=c( )
    for(i in 1:boot){
      sboot<-sample(1:obs,obs,TRUE)
      yb=y[sboot]; db=d[sboot]; mb<-m[sboot]; tb<-t[sboot]
      if ( (is.null(x)==0) & (is.null(xlabels)==0) ){
        if (is.null(ncol(x))) xb<-x[sboot]
        if (is.null(ncol(x))==0) xb<-x[sboot,]
      }
      if ( (is.null(x)==1) | (is.null(xlabels)==1) ) xb=NULL
      est=c(medestCiC(y=yb,d=db,m=mb,t=tb,x=xb,xlabels=xlabels, noalways=noalways, norand=norand, probs=probs))
      bsamples<-rbind(bsamples, est)
    }
  }
  if (is.null(cluster)==0){
    temp<-sort(cluster); clusters<-min(cluster)
    for (i in 1:length(temp)){
      if (temp[i]>max(clusters)) clusters=c(clusters,temp[i])
    }
    key=cluster; bsamples=c(); temp=c()
    obs<-length(clusters)
    while(length(temp)<boot){
      sboot<-sample(clusters,obs,TRUE)
      db<-c(); yb<-c(); xb<-c() ; mb=c(); tb=c()
      for (k in 1:length(sboot)) {
        db<-c(db,d[key==sboot[k]]); yb<-c(yb,y[key==sboot[k]]); mb<-c(mb,m[key==sboot[k]]); tb<-c(tb,t[key==sboot[k]])
        if ( (is.null(x)==0) & (is.null(xlabels)==0) ){
          if (is.null(ncol(x))) xb<-c(xb,x[key==sboot[k]])
          if (is.null(ncol(x))==0) xb=rbind(xb,x[key==sboot[k],])
        }
      }
      if ( (is.null(x)==1) | (is.null(xlabels)==1) ) xb=NULL
      est=c(medestCiC(y=yb,d=db,m=mb,t=tb,x=xb,xlabels=xlabels,noalways=noalways, norand=norand, probs=probs))
      bsamples<-rbind(bsamples, est)
      temp<-c(temp,1)
    }
  }
  bna=apply(bsamples, 1, sum)
  bsamples=bsamples[is.na(bna)==0,]
  if (sum(is.na(bna))>0) cat("Warning: ",sum(is.na(bna)>0)," bootstrap sample(s) dropped due to NA's")
  bsamples
}

################
# APPLICATIONS #
################

# JOBS II APPLICATION  
#load data
data=read_dta("C:\\...\\ReplicationFiles_HSS\\JobsII\\JobsII_w12.dta")
attach(data)
# define variables
yy=depress
dd=treatment
mm=mediator
xx=sex
tt=time>0
# select sample
sel=1*(is.na(yy)==0 & is.na(dd)==0 & is.na(mm)==0 & (time==-1 | time==2) )
sum(sel)
y=yy[sel==1];d=dd[sel==1]; m=mm[sel==1];t=tt[sel==1]; id=pid[sel==1]; x=xx[sel==1]
# run CiC
results1=mediationCiC(y=y,d=d,m=m,t=t,x=x,xlabels=c("sex"),boot=1999, cluster=id, noalways=TRUE)
# run DiD
results2=mediationDiD(y=y,d=d,m=m,t=t, x=x, boot=1999, cluster=id, noalways=TRUE)
# run IV
iv=ivreg(y[t==1] ~ m[t==1] +x[t==1] | d[t==1] +x[t==1])
#results based on Changes-in-Changes
xtable(results1, 2)
#results based on Difference-in-Differences
xtable(results2, 2)
# results based on IV (LATE)
summary(iv, vcov=vcovHC)

# SAKE APPLICATION 
#load data
data=read_dta("C:\\...\\ReplicationFiles_HSS\\SAKE\\SAKE2004_2006-2007.dta")
attach(data)
# define variables
yy=grossincome
dd=highfertility
dd[age>=40 & age<=45]=NA
mm=mediator
xx=age
tt=year>2004
# select sample
sel=1*(is.na(yy)==0 & is.na(dd)==0 & is.na(mm)==0 & ( year==2004 |    year==2007) )
sum(sel)
y=yy[sel==1];d=dd[sel==1]; m=mm[sel==1];t=tt[sel==1];  x=xx[sel==1]; id=PID[sel==1];
# run CiC with gross income as outcome
results1=mediationCiC(y=y,d=d,m=m,t=t,boot=1999,   noalways = TRUE, norand=TRUE, cluster=id)
# redefine outcome
yy=netincome
sel=1*(is.na(yy)==0 & is.na(dd)==0 & is.na(mm)==0 & ( year==2004 |    year==2007) )
sum(sel)
y=yy[sel==1];d=dd[sel==1]; m=mm[sel==1];t=tt[sel==1];  x=xx[sel==1]; id=PID[sel==1];
#run CiC with net income as outcome
results2=mediationCiC(y=y,d=d,m=m,t=t,boot=1999,   noalways = TRUE, norand=TRUE, cluster=id)
#results with gross income as outcome
xtable(results1, 2)
# results with net income as outcome
xtable(results2, 2)



