##############################
## Primary Study Data (Effect) Generation 
#######################################################
PrimaryStudy <- function(StudyID, al, ali, lambdai0, type){
  if(type=='PRE'){
    sigr<-sqrt(0.25);
    m<-10;
    lambdai<-0.5+30*lambdai0;
  }else if(type=='RE'){
    sigr<-0;
    m<-1;
    lambdai<-0.5+30*lambdai0;
  }else if(type=='FE'){
    sigr<-0;
    m<-1;
    lambdai<-0.2+30*lambdai0;
  }
  obs<-100;
  PrimaryData<-as.data.frame(matrix(ncol=19, nrow=m))
  colnames(PrimaryData)<-c('StdID','EstID','al','ali','alir','lambdai','lambdair','effect','se','lowerBound','upperBound','Sig','PosSig','NegSig','nSig','nPosSig','nNegSig','nPos','obs')
  for(i in 1:m){
    alir<-rnorm(1, mean = ali, sd = sigr);
    lambdair<- lambdai + runif(1, 0, 1)*(sigr>0)
    x <- rnorm(obs, mean = 0, sd = 1)
    y <- 1 + alir*x + lambdair*rnorm(obs, mean = 0, sd = 1)
    eff_se_pval<-as.numeric(summary(lm(y~x))$coefficients[2,c(1,2,4)])
    t<-abs(as.numeric(summary(lm(y~x))$coefficients[2,3]))
    ci<-as.numeric(confint(lm(y~x), level=0.95)[2,1:2])
    PrimaryData[i,1:14]<-c(StudyID, i, al,ali, alir, lambdai, lambdair, eff_se_pval[1:2], ci, (t>=2), (ci[1]>0), (ci[2]<0))
  }
  PrimaryData$nSig<-sum(PrimaryData$Sig)/m;
  PrimaryData$nPosSig<-sum(PrimaryData$PosSig)/m;
  PrimaryData$nNegSig<-sum(PrimaryData$NegSig)/m;
  PrimaryData$nPos<-sum(PrimaryData$effect>0)/m;
  PrimaryData$obs <- obs
  return(PrimaryData)
}
#######################################################


##############################
## Meta Analysis Data Generation
#######################################################
CollectingData<- function(type, alpha){
  if(type=='PRE'){
    StudyN<-100;
    sigi<-2
  }else if(type=='RE'){
    StudyN<-1000;
    sigi<-1
  }else if(type=='FE'){
    StudyN<-1000;
    sigi<-0;
  }
  
  for(i in 1:StudyN){
    ali<-rnorm(1, mean = alpha, sd = sigi)
    if(i==1){
      MetaStudyData<-PrimaryStudy(i, alpha, ali, runif(1,0,1), type)
    }else{
      MetaStudyData<-rbind(MetaStudyData, PrimaryStudy(i, alpha, ali, runif(1,0,1), type))
    }
  }
  return(MetaStudyData)
}
#######################################################


##############################
## Creating Publication Bias
#######################################################
MetaStudy <- function(type, alpha){
  MetaData<-as.data.frame(CollectingData(type, alpha))
  return(MetaData)
}
#######################################################


##############################
# Clustered Standard Errors; ARRSM
#######################################################
clusteredSE_ARRSM <-function(regOLS, Study){
  M <- length(unique(Study)) 
  N <- length(Study) 
  K <- regOLS$rank
  dfc <- (M/(M-1)) * ((N-1)/(N-K))
  u<-apply(estfun(regOLS),2,function(x) tapply(x, Study,sum))
  vcovCL<-dfc*sandwich(regOLS, meat=crossprod(u)/N)
  ci <-coef(regOLS) + sqrt(diag(vcovCL)) %o% qt(c(0.025,0.975),summary(regOLS)$df[2])
  return (list("co"=coeftest(regOLS, vcovCL), "ci"=ci))
}
#######################################################


##############################
## Creating Publication Bias
#######################################################
ARBias <- function(MetaData, type, bias){
  if(type=='PRE'){
    rnd <- matrix(1, nrow=100, ncol=1)
    for(rndi in 1:10){rnd[c((1+10*(rndi-1)):(10*rndi))] <- runif(1,0,1);}
    MetaData<-as.data.frame(cbind(MetaData, rnd=rnd))
  }else{
    MetaData<-as.data.frame(cbind(MetaData, rnd=runif(nrow(MetaData),0,1)))
  }
  if(bias!='none'){
    if(type=='PRE'){
      if(bias=='Sig'){
        MetaData<-subset(MetaData, ((MetaData$nSig>=0.7)| (MetaData$rnd<0.1)));
      }else{
        MetaData<-subset(MetaData, ((MetaData$nPos>=0.7)| (MetaData$rnd<0.1)));
      }
    }else{
      if(bias=='Sig'){
        MetaData<-subset(MetaData, ((MetaData$Sig==1)   | (MetaData$rnd<0.1)));
      }else{
        MetaData<-subset(MetaData, ((MetaData$effect>0) | (MetaData$rnd<0.1)));
      }
    }
  }
  return(MetaData)
}
#######################################################
