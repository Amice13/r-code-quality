#######################
##  Simulation Set up
######################################
simN <- SimulationLength
sigH_List=c(1); 
sigH_List <- c(0,0.125,0.25,0.5,1.0,2.0,4.0);			
MetaStudyN_List=c(5, 10, 20, 40, 80);                                    
effectSize_List=c(0, 1);                       			    
PubBias_List=c(0, 25, 50, 75);  # Bias = 0-100
paramONE <- as.data.frame(expand.grid(effectSize=effectSize_List, sigH=sigH_List, PubBias=PubBias_List, m=MetaStudyN_List))

sigH_List=c(1); 
sigH_List <- c(0,0.125,0.25,0.5,1.0,2.0,4.0);			
MetaStudyN_List=c(100, 200, 400, 800);                                    
effectSize_List=c(0, 1);                       			    
PubBias_List=c(0, 25, 50, 75);  # Bias = 0-100
paramTWO <- as.data.frame(expand.grid(effectSize=effectSize_List, sigH=sigH_List, PubBias=PubBias_List, m=MetaStudyN_List))

param <- rbind(paramONE, paramTWO)

param$Condition <- c(1:nrow(param))
######################################



#######################
##  Creating Simulation Datasets
######################################
for (j in beginN:endN) {
  set.seed(3)
  print(paste0(Sys.time(), ", Simulation Environment: computing condition ", param[j,5], "/", max(param[,5])))
  res <- data.frame()
  for (i in 1:simN) {
    print(paste0(Sys.time(), ", computing condition (",param[j,5]," / ", max(param[,5]) , ") - ", i, "/", simN))
    res0 <- as.data.frame(MetaStudy(param[j,1], param[j,2], param[j,4], param[j,3], rep(c(62,125,250,500,1000),1000)));
    res0 <- cbind.data.frame(Environment="BR", replication=i,Condition=param[j,5], TrueEffect=param[j,1], sigH=param[j,2], pubBias=param[j,3], m=param[j,4], res0[,c(1:3,6)])
    colnames(res0) <- c("Environ", "RepID","Condition","TrueEffect","sigH","PubBias","m","StdID","EstEffect","SE","obs")
    res <- rbind(res, res0)
  }
  save(res, file=paste0("BR/simData/simData_", param[j,5], ".RData"), compress="gzip")
}
#######################

