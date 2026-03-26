
# October 4, 2018


rm(list = ls())
###################################
## Simulation Environment Setup  ##
#################################################################
alpha_List<-c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0);
Bias_List<-c('Sig','Sign');
Sim_Environ<-c('FE','RE','PRE')
SimN<-1000;
DEPS<<-FALSE;  #Different number of estimates per study, TRUE or FALSE


OUT_TableII   <- TRUE    # TRUE or FALSE
OUT_TableIII  <- FALSE   # TRUE or FALSE
OUT_TableIV   <- FALSE   # TRUE or FALSE
OUT_TableVI   <- FALSE   # TRUE or FALSE
#################################################################
#
#
#
#
#
#################################################################
# Import Packages
#################################################################
library(sqldf) # This pakcage is for SQL
library(gridExtra)
library(grid)
library(lmtest)
library(sandwich)
library(metafor)
#################################################################
#
#
#
#
#
###############################################
## Primary Study Data (Effect) Generation    ##
#################################################################
PrimaryStudy <- function(StudyID, al, ali, lambdai0, type){
  if(type=='PRE'){
    sigr<-sqrt(0.25);
    if(DEPS){
      m<-ceiling(rlnorm(1, log(5.5), log(3.5)  ));
    }else{
      m<-10;
    }
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
  PrimaryData<-as.data.frame(matrix(ncol=18, nrow=m))
  colnames(PrimaryData)<-c('StdID','EstID','al','ali','alir','lambdai','lambdair','effect','se','lowerBound','upperBound','Sig','PosSig','NegSig','nSig','nPosSig','nNegSig','nPos')
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
  return(PrimaryData)
}
###################################
## Meta Analysis Data Generation ##
#################################################################
MetaStudy <- function(type, alpha){
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
###################################
## RE Table II                   ##
#################################################################
RandomEffectTableII <- function(MetaData){
  size<-nrow(MetaData);
  effectFE<-as.matrix(MetaData$effect)
  wFE<-1/MetaData$se^2
  xFE<-as.matrix(rep(1, length(effectFE)))
  dfFE<-length(effectFE)-ncol(xFE)
  
  u_FE<-sum(effectFE*wFE)/sum(wFE)
  residFE<-effectFE-u_FE
  QFE<-sum(residFE*residFE*wFE)-dfFE
  I2<-QFE/(QFE+dfFE)
  CFE<-sum(wFE)-(sum(wFE*wFE)/sum(wFE))
  tau2RE <- ifelse((QFE/CFE)<0,0,QFE/CFE)
  effect<-MetaData$effect;
  se<-MetaData$se;
  t<-effect/se;
  return(c(as.numeric(quantile(effect, c(0.5,0,.05,.95,1))), as.numeric(quantile(t, c(0.5,0,.05,.95,1))), mean(MetaData$Sig==1), I2, size, tau2RE))
}
#################################################################
#################################################################
#################################################################
#
#
# 
#
#
#################################################################
# Clustered Standard Errors
#################################################################
clusteredSE <-function(regOLS, Study){
  M <- length(unique(Study)) 
  N <- length(Study) 
  K <- regOLS$rank
  dfc <- (M/(M-1)) * ((N-1)/(N-K))
  u<-apply(estfun(regOLS),2,function(x) tapply(x, Study,sum))
  vcovCL<-dfc*sandwich(regOLS, meat=crossprod(u)/N)
  ci <-coef(regOLS) + sqrt(diag(vcovCL)) %o% qt(c(0.025,0.975),summary(regOLS)$df[2])
  return (list("co"=coeftest(regOLS, vcovCL), "ci"=ci))
}
#################################################################
#
#
# 
#
#
###################################
## Creating Table II             ##
## Creating Table II             ##
#################################################################
if(OUT_TableII){
type<-'RE'; alpha<-1;
OutputFull<-as.data.frame(matrix(0, nrow=SimN, ncol=14))
colnames(OutputFull) <- c('EffectMed','EffectMin','Effect5','Effect95','EffectMax','TMed','TMin','T5','T95','TMax','Sig','I2','Size','Tau2')
OutputSig<-OutputFull; OutputPos<-OutputFull; 
for(i in 1:SimN){
  MetaData<-as.data.frame(MetaStudy(type, alpha))
  MetaData<-as.data.frame(cbind(MetaData, rnd=runif(nrow(MetaData),0,1)))
  OutputFull[i,]<-RandomEffectTableII(MetaData)
  OutputSig[i,]<-RandomEffectTableII(subset(MetaData, ((MetaData$Sig==1)   | (MetaData$rnd<0.1))))
  OutputPos[i,]<-RandomEffectTableII(subset(MetaData, ((MetaData$effect>0) | (MetaData$rnd<0.1))))
  cat("Table II - ",100*i/SimN, "(%)\n")
}

TableII<-as.data.frame(matrix(NA, nrow=15, ncol=7))
colnames(TableII) <- c('Variable','Median','Minimum','P5','P95','Maximum','Frac')
TableII[,1]<-c('Pre_Pub','Estimated_Effect','T_Statistics','Percent_Sig','I2','Sig','Estimated_Effect','T_Statistics','Percent_Sig','I2','Negative','Estimated_Effect','T_Statistics','Percent_Sig','I2')
TableII[2,2:7] <- c(mean(OutputFull$EffectMed), mean(OutputFull$EffectMin), mean(OutputFull$Effect5), mean(OutputFull$Effect95), mean(OutputFull$EffectMax), mean(OutputFull$Size/1000))
TableII[3,2:7] <- c(mean(OutputFull$TMed), mean(OutputFull$TMin), mean(OutputFull$T5), mean(OutputFull$T95), mean(OutputFull$TMax),mean(OutputFull$Size/1000))
TableII[4,2:7] <- as.numeric(c(quantile(OutputFull$Sig, c(0.5,0,.05,.95,1)),mean(OutputFull$Size/1000)))
TableII[5,2:7] <- as.numeric(c(quantile(OutputFull$I2, c(0.5,0,.05,.95,1)),mean(OutputFull$Size/1000)))

TableII[7,2:7] <- c(mean(OutputSig$EffectMed), mean(OutputSig$EffectMin), mean(OutputSig$Effect5), mean(OutputSig$Effect95), mean(OutputSig$EffectMax), mean(OutputSig$Size/1000))
TableII[8,2:7] <- c(mean(OutputSig$TMed), mean(OutputSig$TMin), mean(OutputSig$T5), mean(OutputSig$T95), mean(OutputSig$TMax),mean(OutputSig$Size/1000))
TableII[9,2:7] <- as.numeric(c(quantile(OutputSig$Sig, c(0.5,0,.05,.95,1)),mean(OutputSig$Size/1000)))
TableII[10,2:7] <- as.numeric(c(quantile(OutputSig$I2, c(0.5,0,.05,.95,1)),mean(OutputSig$Size/1000)))

TableII[12,2:7] <- c(mean(OutputPos$EffectMed), mean(OutputPos$EffectMin), mean(OutputPos$Effect5), mean(OutputPos$Effect95), mean(OutputPos$EffectMax), mean(OutputPos$Size/1000))
TableII[13,2:7] <- c(mean(OutputPos$TMed), mean(OutputPos$TMin), mean(OutputPos$T5), mean(OutputPos$T95), mean(OutputPos$TMax),mean(OutputPos$Size/1000))
TableII[14,2:7] <- as.numeric(c(quantile(OutputPos$Sig, c(0.5,0,.05,.95,1)),mean(OutputPos$Size/1000)))
TableII[15,2:7] <- as.numeric(c(quantile(OutputPos$I2, c(0.5,0,.05,.95,1)),mean(OutputPos$Size/1000)))

x11(); grid.table(TableII)
}
#################################################################
#################################################################
#
#
#
#
#
###################################
## Creating Table III            ##
#################################################################
if(OUT_TableIII){
type<-'PRE'; alpha<-1;
OutputFull<-as.data.frame(matrix(0, nrow=SimN, ncol=14))
colnames(OutputFull) <- c('EffectMed','EffectMin','Effect5','Effect95','EffectMax','TMed','TMin','T5','T95','TMax','Sig','I2','Size','Tau2')
OutputSig<-OutputFull; OutputPos<-OutputFull; 
for(i in 1:SimN){
  MetaData<-as.data.frame(MetaStudy(type, alpha))
  OutputFull[i,]<-RandomEffectTableII(MetaData)
  OutputSig[i,]<-RandomEffectTableII(subset(MetaData, (MetaData$nSig>=0.7)))
  OutputPos[i,]<-RandomEffectTableII(subset(MetaData, (MetaData$nPos>=0.7)))
  cat("Table III - ",100*i/SimN, "(%)\n")
}

TableIII<-as.data.frame(matrix(NA, nrow=15, ncol=7))
colnames(TableIII) <- c('Variable','Median','Minimum','P5','P95','Maximum','Frac')
TableIII[,1]<-c('No Bias','Estimated_Effect','T_Statistics','Percent_Sig','I2','Sig','Estimated_Effect','T_Statistics','Percent_Sig','I2','Negative','Estimated_Effect','T_Statistics','Percent_Sig','I2')
TableIII[2,2:7] <- c(mean(OutputFull$EffectMed), mean(OutputFull$EffectMin), mean(OutputFull$Effect5), mean(OutputFull$Effect95), mean(OutputFull$EffectMax), mean(OutputFull$Size/1000))
TableIII[3,2:7] <- c(mean(OutputFull$TMed), mean(OutputFull$TMin), mean(OutputFull$T5), mean(OutputFull$T95), mean(OutputFull$TMax),mean(OutputFull$Size/1000))
TableIII[4,2:7] <- as.numeric(c(quantile(OutputFull$Sig, c(0.5,0,.05,.95,1)),mean(OutputFull$Size/1000)))
TableIII[5,2:7] <- as.numeric(c(quantile(OutputFull$I2, c(0.5,0,.05,.95,1)),mean(OutputFull$Size/1000)))

TableIII[7,2:7] <- c(mean(OutputSig$EffectMed), mean(OutputSig$EffectMin), mean(OutputSig$Effect5), mean(OutputSig$Effect95), mean(OutputSig$EffectMax), mean(OutputSig$Size/1000))
TableIII[8,2:7] <- c(mean(OutputSig$TMed), mean(OutputSig$TMin), mean(OutputSig$T5), mean(OutputSig$T95), mean(OutputSig$TMax),mean(OutputSig$Size/1000))
TableIII[9,2:7] <- as.numeric(c(quantile(OutputSig$Sig, c(0.5,0,.05,.95,1)),mean(OutputSig$Size/1000)))
TableIII[10,2:7] <- as.numeric(c(quantile(OutputSig$I2, c(0.5,0,.05,.95,1)),mean(OutputSig$Size/1000)))

TableIII[12,2:7] <- c(mean(OutputPos$EffectMed), mean(OutputPos$EffectMin), mean(OutputPos$Effect5), mean(OutputPos$Effect95), mean(OutputPos$EffectMax), mean(OutputPos$Size/1000))
TableIII[13,2:7] <- c(mean(OutputPos$TMed), mean(OutputPos$TMin), mean(OutputPos$T5), mean(OutputPos$T95), mean(OutputPos$TMax),mean(OutputPos$Size/1000))
TableIII[14,2:7] <- as.numeric(c(quantile(OutputPos$Sig, c(0.5,0,.05,.95,1)),mean(OutputPos$Size/1000)))
TableIII[15,2:7] <- as.numeric(c(quantile(OutputPos$I2, c(0.5,0,.05,.95,1)),mean(OutputPos$Size/1000)))

x11(); grid.table(TableIII)
}
#################################################################
#################################################################
#
#
#
#
#
#################################################
## Main Simulation For Table IV Begins Here    ##
#################################################################
if(OUT_TableIV){
nn<-length(Sim_Environ)*length(Bias_List)*length(alpha_List)*SimN
TableIV_Raw<-as.data.frame(matrix(NA, nrow=nn, ncol=6))
colnames(TableIV_Raw)<-c('Bias','Type','alpha','Published','FAT','PET')
cnt<-0;
for(type in Sim_Environ){
  for(bias in Bias_List){
    for(alpha in alpha_List){
      for(i in 1:SimN){
        cnt<-cnt+1;
        MetaData<-as.data.frame(MetaStudy(type, alpha))
        MetaData<-as.data.frame(cbind(MetaData, rnd=runif(nrow(MetaData),0,1)))
        if(type=='PRE'){
          if(bias=='Sig'){
            MetaData<-subset(MetaData, (MetaData$nSig>=0.7));
          }else{
            MetaData<-subset(MetaData, (MetaData$nPos>=0.7));
          }
        }else{
          if(bias=='Sig'){
            MetaData<-subset(MetaData, ((MetaData$Sig==1)   | (MetaData$rnd<0.1)));
          }else{
            MetaData<-subset(MetaData, ((MetaData$effect>0) | (MetaData$rnd<0.1)));
          }
        }
        t<-MetaData$effect/MetaData$se
        invSE<-1/MetaData$se
        if(type=='PRE'){
          reg<-coeftest(lm(t~1+invSE ,data=MetaData), vcov = vcovHC, cluster = MetaData$StdID, type = "HC0")
          AlSizeFatPet <- c(alpha, nrow(MetaData), (reg[1,4]<0.05), (reg[2,4]<0.05));
        }else{
          reg<-coeftest(lm(t~1+invSE), vcov = vcovHC(lm(t~1+invSE), "HC0"));
          AlSizeFatPet <- c(alpha, nrow(MetaData), (reg[1,4]<0.05), (reg[2,4]<0.05));
        }
        TableIV_Raw[cnt,]<-c(bias,type,AlSizeFatPet)
        cat("Table IV - ",100*cnt/nn, "(%)\n")
      }
    }
  }
}
TableIV_Raw$alpha<-as.numeric(TableIV_Raw$alpha);
TableIV_Raw$Published<-as.numeric(TableIV_Raw$Published)/10;
TableIV_Raw$FAT<-as.numeric(TableIV_Raw$FAT);
TableIV_Raw$PET<-as.numeric(TableIV_Raw$PET);
TableIV<-sqldf('select Bias, Type, alpha, avg(Published) as Published, avg(FAT) as FAT, avg(PET) as PET from TableIV_Raw group by Type,Bias,alpha')

x11(); grid.table(TableIV)
}
#################################################################
#################################################################
#
#
#
#
#
#################################################
## Main Simulation For Table V Begins Here     ##
#################################################################
if(OUT_TableVI){
  Sim_Environ<-c('PRE')
  nn<-length(Sim_Environ)*length(Bias_List)*length(alpha_List)*SimN
  TableVI_Raw<-as.data.frame(matrix(NA, nrow=nn, ncol=12))
  colnames(TableVI_Raw)<-c('Bias','Type','alpha','FPP_Bhat','FPP_Bias','FPP_Cov','WLSFE_Bhat','WLSFE_Bias','WLSFE_Cov','WLSRE_Bhat','WLSRE_Bias','WLSRE_Cov')
  cnt<-0;
  for(type in Sim_Environ){
    for(bias in Bias_List){
      for(alpha in alpha_List){
        for(i in 1:SimN){
          cnt<-cnt+1;
          MetaData<-as.data.frame(MetaStudy(type, alpha))
          MetaData<-cbind(MetaData, se2=MetaData$se*MetaData$se, invSE=1/MetaData$se, t=MetaData$effect/MetaData$se)
          if(bias=='Sig'){
            MetaData<-subset(MetaData, (MetaData$nSig>=0.7));
          }else{
            MetaData<-subset(MetaData, (MetaData$nPos>=0.7));
          }
          ###################
          ## PET-PEESE     ##
          ## PET-PEESE     ##
          ##########################################################################################
          reg<-coeftest(lm(t~1+invSE ,data=MetaData), vcov = vcovHC, cluster = MetaData$StdID, type = "HC0")[1:2, c(1,3,4)]
          regci<-confint(lm(t~1+invSE ,data=MetaData), vcov = vcovHC, cluster = MetaData$StdID, type = "HC0")[2,1:2]
          if(reg[2,3]<0.05){
            reg<-coeftest(lm(t~se+invSE-1,data=MetaData), vcov = vcovHC, cluster = MetaData$StdID, type = "HC0")[1:2, c(1,3,4)]
            regci<-confint(lm(t~se+invSE-1 ,data=MetaData), vcov = vcovHC, cluster = MetaData$StdID, type = "HC0")[2,1:2]
          }
          FPP<-as.numeric(c(reg[2,1],reg[2,1]-alpha,(alpha>regci[1])*(alpha<regci[2])))
          ##########################################################################################
          
          ###################
          ## WLS - FE      ##
          ## WLS - FE      ##
          ##########################################################################################
          reg<-robust(rma(effect~1+se, data=MetaData,vi=se*se, method="FE"), cluster=MetaData$StdID, type = "HC0")
          WLSFE<-as.numeric(c(reg$b[1],reg$b[1]-alpha,(alpha>reg$ci.lb[1])*(alpha<reg$ci.ub[1])))
          ##########################################################################################
          
          
          ###################
          ## WLS - RE      ##
          ## WLS - RE      ##
          ##########################################################################################
          tau2<-rma(effect~1+se, data=MetaData, vi=se*se, method="DL")$tau2
          re_weight<-1/(MetaData$se^2 + tau2)
          regRE<-coeftest(lm(effect~1+se, weights=re_weight, data=MetaData), vcov = vcovHC, cluster=MetaData$StdID, type = "HC0")
          regREci<-confint(lm(effect~1+se, weights=re_weight, data=MetaData), vcov = vcovHC, cluster = MetaData$StdID, type = "HC0")[1,1:2]
          WLSRE<-as.numeric(c(regRE[1,1],regRE[1,1]-alpha,(alpha>regREci[1])*(alpha<regREci[2])))
          ##########################################################################################
          TableVI_Raw[cnt,]<-c(bias,type,alpha, FPP, WLSFE, WLSRE)
          cat("Table VI - ",100*cnt/nn, "(%)\n")
        }
      }
    }
  }
  TableVI_Raw$FPP_Bhat<-as.numeric(TableVI_Raw$FPP_Bhat)
  TableVI_Raw$FPP_Bias<-as.numeric(TableVI_Raw$FPP_Bias)
  TableVI_Raw$FPP_Cov<-as.numeric(TableVI_Raw$FPP_Cov)
  
  TableVI_Raw$WLSFE_Bhat<-as.numeric(TableVI_Raw$WLSFE_Bhat)
  TableVI_Raw$WLSFE_Bias<-as.numeric(TableVI_Raw$WLSFE_Bias)
  TableVI_Raw$WLSFE_Cov<-as.numeric(TableVI_Raw$WLSFE_Cov)
  
  TableVI_Raw$WLSRE_Bhat<-as.numeric(TableVI_Raw$WLSRE_Bhat)
  TableVI_Raw$WLSRE_Bias<-as.numeric(TableVI_Raw$WLSRE_Bias)
  TableVI_Raw$WLSRE_Cov<-as.numeric(TableVI_Raw$WLSRE_Cov)
  
  TableVI<-sqldf('select Bias, Type, alpha, avg(FPP_Bhat) as FPP_Bhat, avg(FPP_Bias*FPP_Bias) as FPP_MSE, avg(FPP_Cov) as FPP_TypeI, avg(WLSFE_Bhat) as WLSFE_Bhat, avg(WLSFE_Bias*WLSFE_Bias) as WLSFE_MSE, avg(WLSFE_Cov) as WLSFE_TypeI, avg(WLSRE_Bhat) as WLSRE_Bhat, avg(WLSRE_Bias*WLSRE_Bias) as WLSRE_MSE, avg(WLSRE_Cov) as WLSRE_TypeI from TableVI_Raw group by Type,Bias,alpha')
  x11(); grid.table(TableVI)
}
#################################################################
















