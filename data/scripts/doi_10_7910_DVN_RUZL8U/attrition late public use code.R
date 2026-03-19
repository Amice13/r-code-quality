# clear R working space and load required R packages
rm(list=ls())
library(np)
library(xtable)


###############
## Functions ##
###############


latenonresp=function(y,d,r,z1,z2, bw1=NULL, bw2=NULL, bw3=NULL, bw4=NULL, bw5=NULL, bw6=NULL, bw7=NULL, bw8=NULL, bw9=NULL, bw10=NULL, bw11=NULL, bw12=NULL, ruleofthumb=1,  wgtfct=2, rtype="ll", numresprob=100){
  
  Pz1=mean(z1); yphi=r*(z1-Pz1); yphi2=(z1-Pz1); Pco=mean(d[z1==1])-mean(d[z1==0]); n=length(r)
  
  if (is.null(bw1)) {
    if (ruleofthumb!=1) bw1<-npregbw(ydat=yphi[d==1], xdat=z2[d==1], regtype=rtype, ckertype="gaussian", bwmethod="cv.ls")$bw
    if (ruleofthumb==1) bw1<-npudensbw(dat=z2[d==1], ckertype="gaussian", bwmethod="normal-reference")$bw
  }
  phi1a<-(npreg(bws=bw1, tydat=yphi[d==1], txdat=z2[d==1], exdat=z2, regtype=rtype, ckertype="gaussian")$mean)
  phi1apar<-cbind(1,z2)%*%coef(lm(yphi[d==1]~z2[d==1]))
  
  if (is.null(bw2)) {
    if (ruleofthumb!=1) bw2<-npregbw(ydat=yphi2[d==1], xdat=z2[d==1], regtype=rtype, ckertype="gaussian", bwmethod="cv.ls")$bw
    if (ruleofthumb==1) bw2<-bw1
  }
  phi1b<-(npreg(bws=bw2, tydat=yphi2[d==1], txdat=z2[d==1], exdat=z2, regtype=rtype, ckertype="gaussian")$mean)
  phi1bpar<-cbind(1,z2)%*%coef(lm(yphi2[d==1]~z2[d==1]))
  phi1=phi1a/phi1b; phi1par=phi1apar/phi1bpar
  
  if (is.null(bw3)) {
    if (ruleofthumb!=1) bw3<-npregbw(ydat=yphi[d==0], xdat=z2[d==0], regtype=rtype, ckertype="gaussian", bwmethod="cv.ls")$bw
    if (ruleofthumb==1) bw3<-npudensbw(dat=z2[d==0], ckertype="gaussian", bwmethod="normal-reference")$bw
  }
  phi0a<-(npreg(bws=bw3, tydat=yphi[d==0], txdat=z2[d==0], exdat=z2, regtype=rtype, ckertype="gaussian")$mean)
  phi0apar<-cbind(1,z2)%*%coef(lm(yphi[d==0]~z2[d==0]))
  
  if (is.null(bw4)) {
    if (ruleofthumb!=1) bw4<-npregbw(ydat=yphi2[d==0], xdat=z2[d==0], regtype=rtype, ckertype="gaussian", bwmethod="cv.ls")$bw
    if (ruleofthumb==1) bw4<-bw3
  }
  phi0b<-(npreg(bws=bw4, tydat=yphi2[d==0], txdat=z2[d==0], exdat=z2, regtype=rtype, ckertype="gaussian")$mean)
  phi0bpar<-cbind(1,z2)%*%coef(lm(yphi2[d==0]~z2[d==0]))
  
  phi0=phi0a/phi0b; phi0par=phi0apar/phi0bpar
  
  yrd=y*r*d*(z1-Pz1);yr1_d=y*r*(1-d)*(z1-Pz1)
  minselprob=max(min(phi1),min(phi0),0.01); maxselprob=min(max(phi1),max(phi0),1)
  
  if (is.null(bw5)) {
    if (ruleofthumb!=1) bw5<-npregbw(ydat=yrd, xdat=phi1, regtype=rtype, ckertype="gaussian", bwmethod="cv.ls")$bw
    if (ruleofthumb==1) bw5<-npudensbw(dat=phi1, ckertype="gaussian", bwmethod="normal-reference")$bw
  }
  predval=seq(from=minselprob,to=maxselprob,length.out=numresprob)
  eyrd<-(npreg(bws=bw5, tydat=yrd, txdat=phi1, exdat=predval, regtype=rtype, ckertype="gaussian")$mean)
  eyrdpar<-cbind(1,predval)%*%lm(yrd~phi1par)$coef
  
  if (is.null(bw6)) {
    if (ruleofthumb!=1) bw6<-npregbw(ydat=yr1_d, xdat=phi0, regtype=rtype, ckertype="gaussian", bwmethod="cv.ls")$bw
    if (ruleofthumb==1) bw6<-npudensbw(dat=phi0, ckertype="gaussian", bwmethod="normal-reference")$bw
  }
  eyr1_d<-(npreg(bws=bw6, tydat=yr1_d, txdat=phi0, exdat=predval, regtype=rtype, ckertype="gaussian")$mean)
  eyr1_dpar<-cbind(1,predval)%*%lm(yr1_d~phi0par)$coef
  
  late=(eyrd+eyr1_d)*1/(Pz1*(1-Pz1)*predval*Pco)
  latepar=(eyrdpar+eyr1_dpar)*1/(Pz1*(1-Pz1)*predval*Pco)
  
  
  if (is.null(bw7)) {
    if (ruleofthumb!=1) bw7<-npudensbw(dat=phi1[d==1], ckertype="gaussian", bwmethod="cv.ls")
    if (ruleofthumb==1) bw7<-npudensbw(dat=phi1[d==1],ckertype="gaussian", bwmethod="normal-reference")
  }
  
  
  if (is.null(bw8)) {
    if (ruleofthumb!=1) bw8<-npudensbw(dat=phi1par[d==1], ckertype="gaussian", bwmethod="cv.ls")
    if (ruleofthumb==1) bw8<-npudensbw(dat=phi1par[d==1], ckertype="gaussian", bwmethod="normal-reference")
  }
  
  if (is.null(bw9)) {
    if (ruleofthumb!=1) bw9<-npudensbw(dat=phi0[d==0], ckertype="gaussian", bwmethod="cv.ls")
    if (ruleofthumb==1) bw9<-npudensbw(dat=phi0[d==0], ckertype="gaussian", bwmethod="normal-reference")
  }
  
  if (is.null(bw10)) {
    if (ruleofthumb!=1) bw10<-npudensbw(dat=phi0par[d==0], ckertype="gaussian", bwmethod="cv.ls")
    if (ruleofthumb==1) bw10<-npudensbw(dat=phi0par[d==0], ckertype="gaussian", bwmethod="normal-reference")
  }
  
  grid=c(predval); lgrid=length(grid)
  fphi1para=npudens(bws=bw6, tdat=phi1par[d==1], edat=grid)$dens
  fphi0para=npudens(bws=bw8, tdat=phi0par[d==0], edat=grid)$dens
  fphi1a=npudens(bws=bw5, tdat=phi1[d==1], edat=grid)$dens
  fphi0a=npudens(bws=bw7, tdat=phi0[d==0], edat=grid)$dens
  
  
  if (wgtfct==1){
    y1sq=(y^2)*r*d*((z1/Pz1^2)+(1-z1)/((1-Pz1)^2))
    y0sq=(y^2)*r*(1-d)*((z1/Pz1^2)+(1-z1)/((1-Pz1)^2))
    if (is.null(bw11)) {
      if (ruleofthumb!=1) bw11=npregbw(ydat=y1sq, xdat=phi1, regtype=rtype, ckertype="gaussian", bwmethod="cv.ls")$bw
      if (ruleofthumb==1) bw11=npudensbw(dat=phi1, ckertype="gaussian", bwmethod="normal-reference")$bw
    }
    if (is.null(bw12)) {
      if (ruleofthumb!=1) bw12=npregbw(ydat=y0sq, xdat=phi0, regtype=rtype, ckertype="gaussian", bwmethod="cv.ls")$bw
      if (ruleofthumb==1) bw12=npudensbw(dat=phi0, ckertype="gaussian", bwmethod="normal-reference")$bw
    }
    lambda1a=npreg(bws=bw11, tydat=y1sq, txdat=phi1, exdat=grid, regtype=rtype, ckertype="gaussian")$mean
    lambda0a=npreg(bws=bw12, tydat=y0sq, txdat=phi0, exdat=grid, regtype=rtype, ckertype="gaussian")$mean
    
    ggg=lambda1a/fphi1a+lambda0a/fphi0a;ggg[ggg<0]=0.1
    wgta=grid/sqrt(ggg)
    ind=is.na(wgta)
    wgta=wgta[ind==0]
    cx=sum(wgta)
    latetemp=sum(late[ind==0]*(wgta/(cx)))
    
    temp=lm(y1sq~phi1); temp2=lm(y0sq~phi0);
    
    lambda0para=(cbind(1,grid)%*%coef(temp2))
    lambda1para=(cbind(1,grid)%*%coef(temp))
    wgtpara=grid/sqrt(lambda1para/fphi1para+lambda0para/fphi0para)
    cxpar=sum(wgtpara)
    latepartemp=sum(latepar*(wgtpara/(cxpar)))
  }
  
  if (wgtfct!=1 & wgtfct!=3){
    wgta=grid/sqrt(1/fphi1a+1/fphi0a)
    cx=sum(wgta)
    latetemp=sum(late*(wgta/(cx)))
    
    wgtpara=grid/sqrt(1/fphi1para+1/fphi0para)
    cxpar=sum(wgtpara)
    latepartemp=sum(latepar*(wgtpara/(cxpar)))
    
  }
  
  if (wgtfct==3){
    latetemp=median(late[is.na(late)==0])
    latepartemp=median(latepar[is.na(latepar)==0])
  }
  
  
  
  itt=sum(y[r==1]*z1[r==1]/Pz1)/sum(z1[r==1]/Pz1)-sum(y[r==1]*(1-z1[r==1])/(1-Pz1))/sum((1-z1[r==1])/(1-Pz1));
  Pco=sum(d[r==1]*z1[r==1]/Pz1)/sum(z1[r==1]/Pz1)-sum(d[r==1]*(1-z1[r==1])/(1-Pz1))/sum((1-z1[r==1])/(1-Pz1)); n=length(r)
  latenaive=itt/Pco
  
  list(late=latetemp, latepar=latepartemp, latenaive=latenaive, latenaivepar=latenaive, phi1=phi1, phi1par=phi1par, phi0=phi0, phi0par=phi0par, bw1=bw1, bw2=bw2, bw3=bw3, bw4=bw4, bw5=bw5, bw6=bw6)
}



latenonrespxx=function(y,d,r,z1,z2, x=NULL, xpar=NULL, bres1=NULL, bres0=NULL, bwyz1=NULL, bwdz1=NULL, bwyz0=NULL, bwdz0=NULL, bwps=NULL, bwcox1=NULL, bwcox2=NULL, bw1=NULL, bw2=NULL, bw3=NULL, bw4=NULL, bw5=NULL, bw6=NULL, bw7=NULL, bw8=NULL, bw9=NULL, bw10=NULL, bw11=NULL, bw12=NULL, ruleofthumb=1, wgtfct=2, rtype="ll", numresprob=100, estlate=TRUE){
  if (ncol(data.frame(x))>1 | is.null(ncol(x))==0) {xd1=x[d==1,]; xd0=x[d==0,]; xz11=x[z1==1,]; xz10=x[z1==0,]; xz11r1=x[z1==1 & r==1,]; xz10r1=x[z1==0 & r==1,]}
  if ( (ncol(data.frame(x))==1) & is.null(ncol(x))  ) {xd1=x[d==1]; xd0=x[d==0]; xz11=x[z1==1]; xz10=x[z1==0]; xz11r1=x[z1==1 & r==1]; xz10r1=x[z1==0 & r==1] }
  
  if (is.null(bres1)){
    if (ruleofthumb!=1) bres1=npregbw(ydat=factor(r[z1==1 ]), xdat=xz11, regtype="lc", ckertype="gaussian", bwmethod="cv.ls")$bw
    if (ruleofthumb==1) bres1=npudensbw(dat=xz11, ckertype="gaussian", bwmethod="normal-reference")$bw
  }
  pres1=npreg(bws=bres1, tydat=factor(r[z1==1 ]), txdat=xz11, exdat=x, regtype="lc", ckertype="gaussian")$mean-1
  prespara1=pnorm(cbind(1,xpar)%*%coef(glm(formula=r[z1==1 ]~cbind(xpar)[z1==1,],family=binomial(probit))))
  if (is.null(bres0)){
    if (ruleofthumb!=1) bres0=npregbw(ydat=factor(r[z1==0 ]), xdat=xz11, regtype="lc", ckertype="gaussian", bwmethod="cv.ls")$bw
    if (ruleofthumb==1) bres0=npudensbw(dat=xz10, ckertype="gaussian", bwmethod="normal-reference")$bw
  }
  pres0=npreg(bws=bres0, tydat=factor(r[z1==0 ]), txdat=xz10, exdat=x, regtype="lc", ckertype="gaussian")$mean-1
  prespara0=pnorm(cbind(1,xpar)%*%coef(glm(formula=r[z1==0 ]~cbind(xpar)[z1==0,],family=binomial(probit))))
  pres=z1*pres1+(1-z1)*pres0; prespara=z1*prespara1+(1-z1)*prespara0
  
  if (is.null(bwyz1)){
    if (ruleofthumb!=1) bwyz1<-npregbw(ydat=y[z1==1 & r==1], xdat=xz11r1, regtype=rtype, ckertype="gaussian", bwmethod="cv.ls")$bw
    if (ruleofthumb==1) bwyz1=npudensbw(dat=xz11r1, ckertype="gaussian", bwmethod="normal-reference")$bw
  }
  eyz1=npreg(bws=bwyz1, tydat=y[z1==1 & r==1], txdat=xz11r1, exdat=x, regtype=rtype, ckertype="gaussian")$mean
  eyz1par<-cbind(1,xpar)%*%coef(lm(y[z1==1 & r==1]~cbind(xpar)[z1==1 & r==1,]))
  
  if (is.null(bwyz0)){
    if (ruleofthumb!=1) bwyz0<-npregbw(ydat=y[z1==0 & r==1], xdat=xz10r1, regtype=rtype, ckertype="gaussian", bwmethod="cv.ls")$bw
    if (ruleofthumb==1) bwyz0=npudensbw(dat=xz10r1, ckertype="gaussian", bwmethod="normal-reference")$bw
  }
  eyz0=npreg(bws=bwyz0, tydat=y[z1==0 & r==1], txdat=xz10r1, exdat=x, regtype=rtype, ckertype="gaussian")$mean
  eyz0par<-cbind(1,xpar)%*%coef(lm(y[z1==0 & r==1]~cbind(xpar)[z1==0 & r==1,]))
  
  if (is.null(bwdz1)){
    if (ruleofthumb!=1) bwdz1<-npregbw(ydat=factor(d[z1==1 ]), xdat=xz11, regtype="lc", ckertype="gaussian", bwmethod="cv.ls")$bw
    if (ruleofthumb==1) bwdz1=npudensbw(dat=xz11, ckertype="gaussian", bwmethod="normal-reference")$bw
  }
  edz1=npreg(bws=bwdz1, tydat=factor(d[z1==1 ]), txdat=xz11, exdat=x, regtype="lc", ckertype="gaussian")$mean-1
  edz1par<-pnorm(cbind(1,xpar)%*%coef(glm(d[z1==1 ]~cbind(xpar)[z1==1 ,], family = binomial(probit))))
  
  if (is.null(bwyz0)){
    if (ruleofthumb!=1) bwyz0<-npregbw(ydat=y[z1==0 ], xdat=xz10, regtype=rtype, ckertype="gaussian", bwmethod="cv.ls")$bw
    if (ruleofthumb==1) bwyz0=npudensbw(dat=xz10, ckertype="gaussian", bwmethod="normal-reference")$bw
  }
  edz0=npreg(bws=bwyz0, tydat=factor(d[z1==0 ]), txdat=xz10, exdat=x, regtype="lc", ckertype="gaussian")$mean-1
  edz0par<-pnorm(cbind(1,xpar)%*%coef(glm(d[z1==0 ]~cbind(xpar)[z1==0 ,], family = binomial(probit))))
  
  regd1=data.frame(z2[d==1],xd1); regd0=data.frame(z2[d==0],xd0); reg=data.frame(z2,x); n=length(d)
  if (is.null(bwps)) {
    if (ruleofthumb!=1) bwps<-npregbw(ydat=factor(z1), xdat=x, regtype="lc", ckertype="gaussian", bwmethod="cv.ls")$bw
    if (ruleofthumb==1) bwps<-npudensbw(dat=x, ckertype="gaussian", bwmethod="normal-reference")$bw
  }
  Pz1=npreg(bws=bwps, tydat=factor(z1), txdat=x, regtype="lc", ckertype="gaussian")$mean-1
  Pz1para=fitted(glm(z1~xpar, family=binomial(probit)))
  dz1=d*z1
  if (is.null(bwcox1)) {
    if (ruleofthumb!=1) bwcox1<-npregbw(ydat=factor(dz1), xdat=x, regtype="lc", ckertype="gaussian", bwmethod="cv.ls")$bw
    if (ruleofthumb==1) bwcox1<-npudensbw(dat=x, ckertype="gaussian", bwmethod="normal-reference")$bw
  }
  edz1=npreg(bws=bwcox1, tydat=factor(dz1), txdat=x, regtype="lc", ckertype="gaussian")$mean-1
  edz1para=fitted(glm(dz1~xpar, family=binomial(probit)))
  if (is.null(bwcox2)) {
    if (ruleofthumb!=1) bwcox2<-npregbw(ydat=d, xdat=x, regtype="lc", ckertype="gaussian", bwmethod="cv.ls")$bw
    if (ruleofthumb==1) bwcox2<-npudensbw(dat=x, ckertype="gaussian", bwmethod="normal-reference")$bw
  }
  ed=npreg(bws=bwcox2, tydat=factor(d), txdat=x, regtype="lc", ckertype="gaussian")$mean-1
  edpara=fitted(glm(d~xpar, family=binomial(probit)))
  
  yphi=r*(z1-Pz1); yphi2=(z1-Pz1);
  yphipara=r*(z1-Pz1para); yphi2para=(z1-Pz1para);
  Pco=sum(d*z1/Pz1)/sum(z1/Pz1)-sum(d*(1-z1)/(1-Pz1))/sum((1-z1)/(1-Pz1));
  Pcopara=sum(d*z1/Pz1para)/sum(z1/Pz1para)-sum(d*(1-z1)/(1-Pz1para))/sum((1-z1)/(1-Pz1para));
  
  if (is.null(bw1)) {
    if (ruleofthumb!=1) bw1<-npregbw(ydat=yphi[d==1], xdat=regd1, regtype=rtype, ckertype="gaussian", bwmethod="cv.ls")$bw
    if (ruleofthumb==1) bw1<-npudensbw(dat=regd1, ckertype="gaussian", bwmethod="normal-reference")$bw
  }
  phi1a<-(npreg(bws=bw1, tydat=yphi[d==1], txdat=regd1, exdat=reg, regtype=rtype, ckertype="gaussian")$mean)
  phi1apar<-cbind(1,z2,xpar)%*%coef(lm(yphipara[d==1]~z2[d==1]+cbind(xpar)[d==1,]))
  
  if (is.null(bw2)) {
    if (ruleofthumb!=1) bw2<-npregbw(ydat=yphi2[d==1], xdat=regd1, regtype=rtype, ckertype="gaussian", bwmethod="cv.ls")$bw
    if (ruleofthumb==1) bw2<-bw1
  }
  phi1b<-(npreg(bws=bw2, tydat=yphi2[d==1], txdat=regd1, exdat=reg, regtype=rtype, ckertype="gaussian")$mean)
  phi1bpar<-cbind(1,z2,xpar)%*%coef(lm(yphi2para[d==1]~z2[d==1]+cbind(xpar)[d==1,]))
  phi1=phi1a/phi1b; phi1par=phi1apar/phi1bpar
  
  if (is.null(bw3)) {
    if (ruleofthumb!=1) bw3<-npregbw(ydat=yphi[d==0], xdat=regd0, regtype=rtype, ckertype="gaussian", bwmethod="cv.ls")$bw
    if (ruleofthumb==1) bw3<-npudensbw(dat=regd0, ckertype="gaussian", bwmethod="normal-reference")$bw
  }
  phi0a<-(npreg(bws=bw3, tydat=yphi[d==0], txdat=regd0, exdat=reg, regtype=rtype, ckertype="gaussian")$mean)
  phi0apar<-cbind(1,z2,xpar)%*%coef(lm(yphipara[d==0]~z2[d==0]+cbind(xpar)[d==0,]))
  
  if (is.null(bw4)) {
    if (ruleofthumb!=1) bw4<-npregbw(ydat=yphi2[d==0], xdat=regd0, regtype=rtype, ckertype="gaussian", bwmethod="cv.ls")$bw
    if (ruleofthumb==1) bw4<-bw3
  }
  phi0b<-(npreg(bws=bw4, tydat=yphi2[d==0], txdat=regd0, exdat=reg, regtype=rtype, ckertype="gaussian")$mean)
  phi0bpar<-cbind(1,z2,xpar)%*%coef(lm(yphi2para[d==0]~z2[d==0]+cbind(xpar)[d==0,]))
  
  phi0=phi0a/phi0b; phi0par=phi0apar/phi0bpar
  
  yrd=y*r*d*(z1-Pz1);yr1_d=y*r*(1-d)*(z1-Pz1)
  yrdpar=y*r*d*(z1-Pz1para);yr1_dpar=y*r*(1-d)*(z1-Pz1para)
  minselprob=max(min(phi1[is.na(phi1)==0]),min(phi0[is.na(phi0)==0]),0.01); maxselprob=min(max(phi1[is.na(phi1)==0]),max(phi0[is.na(phi0)==0]),1)
  regphi1=data.frame(phi1,x); regphi0=data.frame(phi0,x); varx=x
  
  if (is.null(bw5)) {
    if (ruleofthumb!=1) bw5<-npregbw(ydat=yrd, xdat=regphi1, regtype=rtype, ckertype="gaussian", bwmethod="cv.ls")$bw
    if (ruleofthumb==1) bw5<-npudensbw(dat=regphi1, ckertype="gaussian", bwmethod="normal-reference")$bw
  }
  predval=seq(from=minselprob,to=maxselprob,length.out=numresprob)
  
  eyrd=c(); eyrdpar=c(); coef1=lm(yrdpar~phi1par+xpar)$coef
  for (i in 1:numresprob){
    eyrd<-cbind(eyrd,npreg(bws=bw5, tydat=yrd, txdat=regphi1, exdat=data.frame(rep(predval[i],n),varx), regtype=rtype, ckertype="gaussian")$mean)
    eyrdpar<-cbind(eyrdpar,cbind(1,predval[i],xpar)%*%coef1)
  }
  
  if (is.null(bw6)) {
    if (ruleofthumb!=1) bw6<-npregbw(ydat=yr1_d, xdat=regphi0, regtype=rtype, ckertype="gaussian", bwmethod="cv.ls")$bw
    if (ruleofthumb==1) bw6<-npudensbw(dat=regphi0, ckertype="gaussian", bwmethod="normal-reference")$bw
  }
  
  eyr1_d=c(); eyr1_dpar=c(); coef2=lm(yr1_dpar~phi0par+xpar)$coef
  for (i in 1:numresprob){
    eyr1_d<-cbind(eyr1_d, npreg(bws=bw6, tydat=yr1_d, txdat=regphi0, exdat=data.frame(rep(predval[i],n),varx), regtype=rtype, ckertype="gaussian")$mean)
    eyr1_dpar<-cbind(eyr1_dpar, cbind(1,predval[i],xpar)%*%coef2)
  }
  late=c(); latepar=c()
  for (i in 1:numresprob){
    if (estlate!=0){
      late=cbind(late, ((eyrd[,i]+eyr1_d[,i])/(Pz1*(1-Pz1)*predval[i]*Pco)))
      latepar=cbind(latepar,((eyrdpar[,i]+eyr1_dpar[,i])/(Pz1para*(1-Pz1para)*predval[i]*Pcopara)))
    }
    if (estlate==0){
      # respective functions for the ATE
      late=cbind(late, ((eyrd[,i]+eyr1_d[,i])/( (edz1-ed*Pz1)*predval[i])))
      latepar=cbind(latepar,((eyrdpar[,i]+eyr1_dpar[,i])/( (edz1para-edpara*Pz1para)*predval[i])))
    }
  }
  
  
  if (is.null(bw7)) {
    if (ruleofthumb!=1) bw7<-npcdensbw(ydat=phi1[d==1], xdat=xd1, ckertype="gaussian", bwmethod="cv.ls")
    if (ruleofthumb==1) bw7<-npcdensbw(ydat=phi1[d==1], xdat=xd1, ckertype="gaussian", bwmethod="normal-reference")
  }
  
  
  if (is.null(bw8)) {
    if (ruleofthumb!=1) bw8<-npcdensbw(ydat=phi1par[d==1], xdat=xd1, ckertype="gaussian", bwmethod="cv.ls")
    if (ruleofthumb==1) bw8<-npcdensbw(ydat=phi1par[d==1], xdat=xd1, ckertype="gaussian", bwmethod="normal-reference")
  }
  
  if (is.null(bw9)) {
    if (ruleofthumb!=1) bw9<-npcdensbw(ydat=phi0[d==0], xdat=xd0, ckertype="gaussian", bwmethod="cv.ls")
    if (ruleofthumb==1) bw9<-npcdensbw(ydat=phi0[d==0], xdat=xd0, ckertype="gaussian", bwmethod="normal-reference")
  }
  
  if (is.null(bw10)) {
    if (ruleofthumb!=1) bw10<-npcdensbw(ydat=phi0par[d==0], xdat=xd0, ckertype="gaussian", bwmethod="cv.ls")
    if (ruleofthumb==1) bw10<-npcdensbw(ydat=phi0par[d==0], xdat=xd0, ckertype="gaussian", bwmethod="normal-reference")
  }
  
  grid=c(predval); lgrid=length(grid)
  fphi1para=c(); fphi0para=c(); fphi1a=c(); fphi0a=c();
  for (j in 1:lgrid){
    fphi1para=rbind(fphi1para,npcdens(bws=bw8, tydat=phi1par[d==1], txdat=xd1, eydat=rep(grid[j],n), exdat=x)$condens)
    fphi0para=rbind(fphi0para,npcdens(bws=bw10, tydat=phi0par[d==0], txdat=xd0, eydat=rep(grid[j],n), exdat=x)$condens)
    fphi1a=rbind(fphi1a,npcdens(bws=bw7, tydat=phi1[d==1], txdat=xd1, eydat=rep(grid[j],n), exdat=x)$condens)
    fphi0a=rbind(fphi0a,npcdens(bws=bw9, tydat=phi0[d==0], txdat=xd0, eydat=rep(grid[j],n), exdat=x)$condens)
  }
  
  if (wgtfct!=1 & wgtfct!=3){
    wgta=grid/sqrt(1/fphi1a+1/fphi0a)
    cx=colSums(wgta)
    latetemp=mean(colSums(t(late)*(wgta/(rep(1,length(grid))%*%t(cx)))))
    
    wgtpara=grid/sqrt(1/fphi1para+1/fphi0para)
    cxpar=colSums(wgtpara)
    latepartemp=mean(colSums(t(latepar)*(wgtpara/(rep(1,length(grid))%*%t(cxpar)))))
  }
  
  if (wgtfct==1){
    y1sq=(y^2)*r*d*((z1/Pz1^2)+(1-z1)/((1-Pz1)^2))
    y0sq=(y^2)*r*(1-d)*((z1/Pz1^2)+(1-z1)/((1-Pz1)^2))
    if (is.null(bw11)) {
      if (ruleofthumb!=1) bw11=npregbw(ydat=y1sq, xdat=data.frame(phi1,x), regtype=rtype, ckertype="gaussian", bwmethod="cv.ls")$bw
      if (ruleofthumb==1) bw11=npudensbw(dat=data.frame(phi1,x), ckertype="gaussian", bwmethod="normal-reference")$bw
    }
    if (is.null(bw12)) {
      if (ruleofthumb!=1) bw12=npregbw(ydat=y0sq, xdat=data.frame(phi0,x), regtype=rtype, ckertype="gaussian", bwmethod="cv.ls")$bw
      if (ruleofthumb==1) bw12=npudensbw(dat=data.frame(phi0,x), ckertype="gaussian", bwmethod="normal-reference")$bw
    }
    lambda1a=c(); lambda0a=c();
    for (j in 1:lgrid){
      lambda1a=rbind(lambda1a,npreg(bws=bw11, tydat=y1sq, txdat=data.frame(phi1,x), exdat=data.frame(rep(grid[j],n),x), regtype=rtype, ckertype="gaussian")$mean)
      lambda0a=rbind(lambda0a,npreg(bws=bw12, tydat=y0sq, txdat=data.frame(phi0,x), exdat=data.frame(rep(grid[j],n),x), regtype=rtype, ckertype="gaussian")$mean)
    }
    ggg=lambda1a/fphi1a+lambda0a/fphi0a;ggg[ggg<0]=0.1
    wgta=grid/sqrt(ggg)
    ind=rowSums(is.na(wgta))
    wgta=wgta[ind==0,]
    cx=colSums(wgta)
    latetemp=mean(colSums(t(late[,ind==0])*(wgta/(rep(1,length(grid[ind==0]))%*%t(cx)))))
    
    y1sqpar=(y^2)*r*d*((z1/Pz1para^2)+(1-z1)/((1-Pz1para)^2))
    y0sqpar=(y^2)*r*(1-d)*((z1/Pz1para^2)+(1-z1)/((1-Pz1para)^2))
    temp=lm(y1sqpar~cbind(phi1par,xpar)); temp2=lm(y0sqpar~cbind(phi0par,xpar));
    lambda0para=c(); lambda1para=c()
    for (j in 1:lgrid){
      lambda0para=rbind(lambda0para, t(cbind(1,rep(grid[j],n),xpar)%*%coef(temp2)))
      lambda1para=rbind(lambda1para, t(cbind(1,rep(grid[j],n),xpar)%*%coef(temp)))
    }
    wgtpara=grid/sqrt(lambda1para/fphi1para+lambda0para/fphi0para)
    cxpar=colSums(wgtpara)
    latepartemp=mean(colSums(t(latepar)*(wgtpara/(rep(1,length(grid))%*%t(cxpar)))))
  }
  
  if (wgtfct==3){
    latetemp=c(); latepartemp=c()
    for (j in 1:n){
      latetemp=c(latetemp, median(late[j,is.na(late[j,])==0]))
      latepartemp=c(latepartemp, median(latepar[j,is.na(latepar[j,])==0]))
    }
    latetemp=mean(latetemp); latepartemp=mean(latepartemp)
  }
  
  catenaive=(eyz1-eyz0)/(edz1-edz0)
  catenaivepar=(eyz1par-eyz0par)/(edz1par-edz0par)
  
  if (estlate!=0){
    itt=sum(r*y *z1 /(pres*Pz1) )/sum(r*z1 /(pres*Pz1) )-sum(r*y *(1-z1 )/(pres*(1-Pz1) ))/sum(r*(1-z1 )/(pres*(1-Pz1) ));
    ittpara=sum(r*y *z1 / (prespara*Pz1para) )/sum(r*z1 / (prespara*Pz1para) )-sum(r*y *(1-z1 )/ (prespara*(1-Pz1para )))/sum(r*(1-z1 )/(prespara*(1-Pz1para) ));
    Pco=sum(d *z1 /Pz1 )/sum(z1 /Pz1 )-sum(d*(1-z1 )/(1-Pz1 ))/sum((1-z1 )/(1-Pz1 )); n=length(r)
    Pcopara=sum(d *z1 /Pz1para )/sum(z1 /Pz1para )-sum(d *(1-z1 )/(1-Pz1para ))/sum((1-z1 )/(1-Pz1para ));
    latenaive=itt/Pco; latenaivepar=ittpara/Pcopara;
  }
  if (estlate==0){
    latenaive=mean(catenaive*r/pres)
    latenaivepar=mean(catenaivepar*r/prespara)
    latenaive=mean(catenaive)
    latenaivepar=mean(catenaivepar)
  }
  list(late=latetemp, latepar=latepartemp, latenaive=latenaive, latenaivepar=latenaivepar, phi1=phi1, phi1par=phi1par, phi0=phi0, phi0par=phi0par, bres1=bres1, bres0=bres0,  bwyz1=bwyz1, bwdz1=bwdz1, bwyz0=bwyz0, bwdz0=bwdz0, bwps=bwps, bwcox1=bwcox1, bwcox2=bwcox2, bw1=bw1, bw2=bw2, bw3=bw3, bw4=bw4, bw5=bw5, bw6=bw6)
}


latenonrespxxfct<-function(y,d,r,z1,z2, x=NULL, xpar=NULL, bres1=NULL, bres0=NULL,  bwyz1=NULL, bwdz1=NULL, bwyz0=NULL, bwdz0=NULL, bwps=NULL, bwcox1=NULL, bwcox2=NULL, bw1=NULL, bw2=NULL, bw3=NULL, bw4=NULL, bw5=NULL, bw6=NULL, bw7=NULL, bw8=NULL, bw9=NULL, bw10=NULL, bw11=NULL, bw12=NULL, ruleofthumb=1, wgtfct=2, rtype="ll", numresprob=100, estlate=TRUE){
  if ((is.null(x)==0) & (is.null(xpar)==0)) out=latenonrespxx(y=y,d=d,r=r,z1=z1,z2=z2, x=x, xpar=xpar, bres1=bres1, bres0=bres0,  bwyz1=bwyz1, bwdz1=bwdz1, bwyz0=bwyz0, bwdz0=bwdz0, bwps=bwps, bwcox1=bwcox1, bwcox2=bwcox2, bw1=bw1, bw2=bw2, bw3=bw3, bw4=bw4, bw5=bw5, bw6=bw6,  bw7=bw7, bw8=bw8, bw9=bw9, bw10=bw10, bw11=bw11, bw12=bw12, ruleofthumb=ruleofthumb, wgtfct=wgtfct, rtype=rtype, numresprob=numresprob, estlate=estlate)
  if (is.null(x) | is.null(xpar)) out=latenonresp(y=y,d=d,r=r,z1=z1,z2=z2, bw1=bw1, bw2=bw2, bw3=bw3, bw4=bw4, bw5=bw5, bw6=bw6,   bw7=bw7, bw8=bw8, bw9=bw9, bw10=bw10, bw11=bw11, bw12=bw12, ruleofthumb=ruleofthumb, wgtfct=wgtfct, rtype=rtype, numresprob=numresprob)
  results=c(out$late, out$latepar, out$latenaive, out$latenaivepar, out$late-out$latenaive, out$latepar-out$latenaivepar)
  list(results=results, bres1=out$bres1, bres0=out$bres0,  bwyz1=out$bwyz1, bwdz1=out$bwdz1, bwyz0=out$bwyz0, bwdz0=out$bwdz0, bwps=out$bwps, bwcox1=out$bwcox1, bwcox2=out$bwcox2, bw1=out$bw1, bw2=out$bw2, bw3=out$bw3, bw4=out$bw4, bw5=out$bw5, bw6=out$bw6, phi1=out$phi1, phi1par=out$phi1par, phi0=out$phi0, phi0par=out$phi0par)
}

bootstrap.late<-function(y,d,r,z1,z2, x=NULL, xpar=NULL, bres1=NULL, bres0=NULL,  bwyz1=NULL, bwdz1=NULL, bwyz0=NULL, bwdz0=NULL, bwps=NULL, bwcox1=NULL, bwcox2=NULL, bw1=NULL, bw2=NULL, bw3=NULL, bw4=NULL, bw5=NULL, bw6=NULL, bw7=NULL, bw8=NULL, bw9=NULL, bw10=NULL, bw11=NULL, bw12=NULL, ruleofthumb=1, wgtfct=2, rtype="ll", numresprob=100, boot=1999, estlate=TRUE){
  mc=c(); temp=c(); j=1
  while(j<=boot){
    sboot<-sample(1:length(d),length(d),TRUE)
    z1b<-z1[sboot]; z2b<-z2[sboot]; db<-d[sboot]; yb=y[sboot]; rb=r[sboot]
    if ((is.null(x)==0) & (is.null(xpar)==0)) {
      if (length(x)==length(d)) xb<-x[sboot]
      if (length(x)!=length(d)) xb<-x[sboot,]
      if (length(xpar)==length(d)) xparb<-xpar[sboot]
      if (length(xpar)!=length(d)) xparb<-xpar[sboot,]
      temp<-latenonrespxxfct(y=yb,d=db,r=rb,z1=z1b,z2=z2b, x=xb, xpar=xparb, bres1=bres1, bres0=bres0,  bwyz1=bwyz1, bwdz1=bwdz1, bwyz0=bwyz0, bwdz0=bwdz0, bwps=bwps, bwcox1=bwcox1, bwcox2=bwcox2, bw1=bw1, bw2=bw2, bw3=bw3, bw4=bw4, bw5=bw5, bw6=bw6,  bw7=bw7, bw8=bw8, bw9=bw9, bw10=bw10, bw11=bw11, bw12=bw12, ruleofthumb=ruleofthumb, wgtfct=wgtfct, rtype=rtype, numresprob=numresprob, estlate=estlate)$results
    }
    if (is.null(x) | is.null(x)) {
      temp<-latenonrespxxfct(y=yb,d=db,r=rb,z1=z1b,z2=z2b, x=NULL, xpar=NULL, bwps=NULL, bwcox1=NULL, bwcox2=NULL, bw1=bw1, bw2=bw2, bw3=bw3, bw4=bw4, bw5=bw5, bw6=bw6, bw7=bw7, bw8=bw8, bw9=bw9, bw10=bw10, bw11=bw11, bw12=bw12, ruleofthumb=ruleofthumb, wgtfct=wgtfct, rtype=rtype, numresprob=numresprob)$results
    }
    if (is.na(sum(temp))==0){
      mc<-rbind(mc,temp)
      j=j+1
    }
  }
  mc
}

latenr<-function(y,d,r,z1,z2, x=NULL, xpar=NULL,  ruleofthumb=1, wgtfct=2, rtype="ll", numresprob=100, boot=1999, estlate=TRUE){
  out=latenonrespxxfct(y,d,r,z1,z2, x=x, xpar=xpar, bres1=NULL, bres0=NULL,  bwyz1=NULL, bwdz1=NULL, bwyz0=NULL, bwdz0=NULL, bwps=NULL, bwcox1=NULL, bwcox2=NULL, bw1=NULL, bw2=NULL, bw3=NULL, bw4=NULL, bw5=NULL, bw6=NULL, bw7=NULL, bw8=NULL, bw9=NULL, bw10=NULL, bw11=NULL, bw12=NULL, ruleofthumb=ruleofthumb, wgtfct=wgtfct, rtype=rtype, numresprob=numresprob, estlate=estlate)
  if ((is.null(x)==0) & (is.null(xpar)==0)) results2=bootstrap.late(y,d,r,z1,z2, x=x, xpar=xpar, bres1=out$bres1, bres0=out$bres0,  bwyz1=out$bwyz1, bwdz1=out$bwdz1, bwyz0=out$bwyz0, bwdz0=out$bwdz0, bwps=out$bwps, bwcox1=out$bwcox1, bwcox2=out$bwcox2, bw1=out$bw1, bw2=out$bw2, bw3=out$bw3, bw4=out$bw4, bw5=out$bw5, bw6=out$bw6, bw7=out$bw7, bw8=out$bw8, bw9=out$bw9, bw10=out$bw10, bw11=out$bw11, bw12=out$bw12, ruleofthumb=ruleofthumb, wgtfct=wgtfct, rtype=rtype, numresprob=numresprob, boot=boot, estlate=estlate)
  if (is.null(x) | is.null(xpar)) results2=bootstrap.late(y,d,r,z1,z2, x=NULL, xpar=NULL, bres1=NULL, bres0=NULL,  bwyz1=NULL, bwdz1=NULL, bwyz0=NULL, bwdz0=NULL, bwps=NULL, bwcox1=NULL, bwcox2=NULL, bw1=out$bw1, bw2=out$bw2, bw3=out$bw3, bw4=out$bw4, bw5=out$bw5, bw6=out$bw6, bw7=out$bw7, bw8=out$bw8, bw9=out$bw9, bw10=out$bw10, bw11=out$bw11, bw12=out$bw12, ruleofthumb=ruleofthumb, wgtfct=wgtfct, rtype=rtype, numresprob=numresprob, boot=boot, estlate=estlate)
  results=out$results
  output<-matrix(,4,length(results))
  output[1,]<-results
  for (i in 1:length(results)){
    output[2,i]<-sd(results2[,i])
    output[3,i]<-2*pnorm(-abs(output[1,i]/output[2,i]))
    output[4,i]<-c(2*(min( mean(( results2[,i]-output[1,i])<=output[1,i]) , mean((results2[,i]-output[1,i])>output[1,i]) ) ))
  }
  list(output=output)
}



#################
## Application ##
#################

#load data - set path
load("C:\\expdata.RData")
attach(expdata)

# define covariates for non- and semiparametric estimators
x=data.frame(factor(swiss),factor(female)) #nonparametric procedure requires binary covariates to be factors
xpar=cbind(swiss,female)                

# estimate the effects:  
# ruleofthumb=1 implies that rule of thumb is used for bandwidth selection in kernel estimations (otherwise cross-validation)
# wgtfct can be 1 or 2 (see the paper), or 3 for median
# rtype specifies type of kernel estimation - "ll" is local linear regression, "lc" is local constant regression
# numresprob provides the number of equidistant grid points in psi (conditional  distribution of unobservable V in response equation)
# boot is the number of bootstrap replications
# estlate=FALSE for ATE estimation (otherwise LATE estimation)
results=latenr(y,d,r,z1,z2, x, xpar, ruleofthumb=1, wgtfct=1, rtype="lc", numresprob=100, boot=1999, estlate=TRUE)
estimates=results$output
colnames(estimates)=c("nonpara (L)ATE", "semipara (L)ATE", "nonpara MAR", "semipara MAR", "nonpara (L)ATE-MAR", "semipara (L)ATE-MAR")

# non- and semiparametric estimates
xtable(estimates, digits=2)

