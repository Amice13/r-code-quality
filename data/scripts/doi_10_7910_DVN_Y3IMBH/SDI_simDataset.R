#######################
##  Simulation Set up Cohen's d
######################################
simN <- SimulationLength
simulationType <- "Cohend"
effectSize_List<-c(0, 50);
sigH_List<-c(0, 6.25, 12.5, 25, 50);
PubBias_List<-c(0, 0.5, 0.75);
MetaStudyN_List<- c(5,10,20,40,80);
param1 <- as.data.frame(expand.grid(effectSize=effectSize_List, sigH=sigH_List, PubBias=PubBias_List, m=MetaStudyN_List, SimType=simulationType))

simulationType <- "LogOdds"
effectSize_List<-c(0.00, 0.03, 0.06);
sigH_List<-c(0.006);
PubBias_List<-c(0.0, 0.5);
MetaStudyN_List<- c(5,10,20,40,80);
param2 <- as.data.frame(expand.grid(effectSize=effectSize_List, sigH=sigH_List, PubBias=PubBias_List, m=MetaStudyN_List, SimType=simulationType))

paramONE <- rbind(param1,param2)


simulationType <- "Cohend"
effectSize_List<-c(0, 50);
sigH_List<-c(0, 6.25, 12.5, 25, 50);
PubBias_List<-c(0, 0.5, 0.75);
MetaStudyN_List<- c(100,200,400,800);
param3 <- as.data.frame(expand.grid(effectSize=effectSize_List, sigH=sigH_List, PubBias=PubBias_List, m=MetaStudyN_List, SimType=simulationType))

simulationType <- "LogOdds"
effectSize_List<-c(0.00, 0.03, 0.06);
sigH_List<-c(0.006);
PubBias_List<-c(0.0, 0.5);
MetaStudyN_List<- c(100,200,400,800);
param4 <- as.data.frame(expand.grid(effectSize=effectSize_List, sigH=sigH_List, PubBias=PubBias_List, m=MetaStudyN_List, SimType=simulationType))
paramTWO <- rbind(param3,param4)
param <- rbind(paramONE,paramTWO)
param$Condition <- c(1:nrow(param))
######################################


#######################
##  Creating Simulation Datasets
######################################
for (j in 1:nrow(param)) {
  set.seed(3)
  print(paste0(Sys.time(), ", Simulation Environment: computing condition ", param[j,6], "/", max(param[,6])))
  res <- data.frame()
  for (i in 1:simN) {
    print(paste0(Sys.time(), ", computing condition (",param[j,6]," / ", max(param[,6]) , ") - ", i, "/", simN))
    if(param[j,5]=="Cohend"){
      res0<-as.data.frame(MetaStudy_Cohen_d(param[j,1], param[j,2], param[j,4], param[j,3], rep(c(32,64,125,250,500),1000)))
      res0 <- cbind.data.frame(Environment=param[j,5], replication=i,Condition=param[j,6], res0[,c(1,4,3,5,6,10)],param[j,3],param[j,4])
      colnames(res0) <- c("Environ", "RepID","Condition","StdID","TrueEffect","sigH","EstEffect","SE","obs","PubBias","m")
    }else{
      res0<-as.data.frame(MetaStudy_LogOR(param[j,1], param[j,2], param[j,4], param[j,3], rep(c(50,100,100,250,500),1000)))
      res0 <- cbind.data.frame(Environment=param[j,5], replication=i,Condition=param[j,6], res0[,c(1,4,3,5,6,10)],param[j,3],param[j,4])
      colnames(res0) <- c("Environ", "RepID","Condition","StdID","TrueEffect","sigH","EstEffect","SE","obs","PubBias","m")
    }
    res <- rbind(res, res0)
  }
  save(res, file=paste0("SDI/simData/simData_", param[j,6], ".RData"), compress="gzip")
}
#######################

