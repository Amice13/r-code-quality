###############################################
## Primary Study Data (logOR) Generation     ##
## Primary Study Data (logOR) Generation     ##
#################################################################
PrimaryStudy_LogOR <- function(TreatmentEffect, sigH, bias, primaryObs){
  if(bias==0){
    P <- 0.1 + TreatmentEffect + rnorm(1, 0, sigH);
  }else{
    P <- 0.1 + TreatmentEffect;
  }
  nT<-primaryObs;
  mT<-sum(rbinom(nT, size=1, prob=P));
  nC<-primaryObs;
  mC<-sum(rbinom(nC, size=1, prob=0.1)); 
  
  correction<-(mT*mC==0)*10^(-5);
  LogOR<-log(((mT+correction)/(nT-mT+correction))/((mC+correction)/(nC-mC+correction)));
  LogORse<-sqrt( (1/(mT+correction)) + (1/(nT-mT+correction)) + (1/(mC+correction)) + (1/(nC-mC+correction)) )
  LogORci<-LogOR+LogORse*qt(c(0.025, 0.975), df=10^100)
  LogORt<-LogOR/LogORse
  return(c(TreatmentEffect,sigH, round(log(((.1+TreatmentEffect)/(.9-TreatmentEffect) )/(.1/.9)),2), LogOR, LogORse, LogORci, (LogORci[1]>0), primaryObs))
}
###################################
## Meta Analysis Data Generation ##
## Meta Analysis Data Generation ##
#################################################################
MetaStudy_LogOR <- function(effect, sigH, m, bias, PrimaryN){
  MetaStudyData<-matrix(0,nrow=m, ncol=10)
  colnames(MetaStudyData)<-c('StudyID','effect','sigH','TrueLogOR','EstimatedLogOR','StdErrLogOR','LowerBound','UpperBound','Pos_SigEffect', 'PrimaryStudyOBS')
  biasedStudy<-sample(c(ceiling(m*bias), floor(m*bias)),1)
  for(i in 1:m){
    primaryObs<-PrimaryN[i];
    if(runif(1, 0, 1)<bias){
      PrimaryStudy<-PrimaryStudy_LogOR(effect, sigH, bias, primaryObs)
      while(PrimaryStudy[8]==0){PrimaryStudy<-PrimaryStudy_LogOR(effect, sigH, bias, primaryObs)}
      MetaStudyData[i,]<-c(i,PrimaryStudy)
    }else{
      PrimaryStudy<-PrimaryStudy_LogOR(effect, sigH, bias, primaryObs)
      MetaStudyData[i,]<-c(i,PrimaryStudy)
    }}
  return(MetaStudyData)
}
#################################################################
#################################################################



###############################################
## Primary Study Data (Cohen's d) Generation ##
## Primary Study Data (Cohen's d) Generation ##
#################################################################
PrimaryStudy_Cohen_d <- function(TreatmentEffect, sigH, primaryObs){
  obsControl<-primaryObs;
  y_cj<-rnorm(obsControl, 300, 86.603) + rnorm(obsControl, 0, 50)
  obsTreatment<-primaryObs;
  Te<- rnorm(obsTreatment, 300, 86.603) + rnorm(obsTreatment, 0, 50) + (TreatmentEffect + rnorm(1, 0, sigH))
  
  dfn1<-(length(Te)-1); dfn2<-(length(y_cj)-1);
  s1<-sum((Te-mean(Te))^2)/dfn1; s2<-sum((y_cj-mean(y_cj))^2)/dfn2;
  sp<-sqrt((dfn1*s1 + dfn2*s2)/(dfn1+dfn2))
  estimatedCohen_d<-(mean(Te)-mean(y_cj))/sp
  stdErr_Cohen_d<-sqrt(((dfn1+dfn2+2)/((dfn1+1)*(dfn1+1)))+estimatedCohen_d^2/(2*(dfn1+dfn2+2)))
  cohen_ci<-estimatedCohen_d+qt(c(0.025,.975),(dfn1+dfn2))*stdErr_Cohen_d
  cohen_t<-estimatedCohen_d/stdErr_Cohen_d;
  return(c(TreatmentEffect,sigH,TreatmentEffect/100,estimatedCohen_d,stdErr_Cohen_d,cohen_ci,(cohen_t>1.96),primaryObs))
}
###################################
## Meta Analysis Data Generation ##
## Meta Analysis Data Generation ##
#################################################################
MetaStudy_Cohen_d <- function(effect, sigH, m, bias, PrimaryN){
  MetaStudyData<-matrix(0,nrow=m, ncol=10)
  colnames(MetaStudyData)<-c('StudyID','effect','sigH','TrueCohend','EstimatedCohend','StdErrCohend','LowerBound','UpperBound','Pos_SigEffect', 'PrimaryStudyOBS')
  if(bias==0.75){
    for(i in 1:m){
      primaryObs<-PrimaryN[i];
      if(runif(1, 0, 1)<bias){
        PrimaryStudy<-PrimaryStudy_Cohen_d(effect, sigH, primaryObs)
        while(PrimaryStudy[8]==0){PrimaryStudy<-PrimaryStudy_Cohen_d(effect, sigH, primaryObs)}
        MetaStudyData[i,]<-c(i,PrimaryStudy)
      }else{
        PrimaryStudy<-PrimaryStudy_Cohen_d(effect, sigH, primaryObs)
        while(PrimaryStudy[4]<=0){PrimaryStudy<-PrimaryStudy_Cohen_d(effect, sigH, primaryObs)}
        MetaStudyData[i,]<-c(i,PrimaryStudy)
      }}}else{
        for(i in 1:m){
          primaryObs<-PrimaryN[i]
          if(runif(1, 0, 1)<bias){
            PrimaryStudy<-PrimaryStudy_Cohen_d(effect, sigH, primaryObs)
            while(PrimaryStudy[8]==0){PrimaryStudy<-PrimaryStudy_Cohen_d(effect, sigH, primaryObs)}
            MetaStudyData[i,]<-c(i,PrimaryStudy)
          }else{
            PrimaryStudy<-PrimaryStudy_Cohen_d(effect, sigH, primaryObs)
            MetaStudyData[i,]<-c(i,PrimaryStudy)
          }}}
  return(MetaStudyData)
}
#################################################################
#################################################################


