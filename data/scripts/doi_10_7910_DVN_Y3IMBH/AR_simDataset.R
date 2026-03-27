#######################
##  Simulation Set up
#######################
Sim_Environ<-c('RE','PRE','FE') # 'PRE'
effectSize_List=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0);
simN <- SimulationLength

param <- as.data.frame(matrix(NA, nrow=(length(Sim_Environ)*length(effectSize_List)), ncol=3))
colnames(param) <-c("Environ", "Effect","Condition")
param$Environ <- sort(rep(Sim_Environ, length(effectSize_List)))
param$Effect <- rep(effectSize_List, length(Sim_Environ))
param$Condition <- c(1:nrow(param))
######################################



#######################
##  Creating Simulation Datasets
######################################
for (j in 1:nrow(param)) {
  set.seed(3)
  print(paste0(Sys.time(), ", Simulation Environment: computing condition ", param[j,3], "/", max(param[,3])))
  res <- data.frame()
  for (i in 1:simN) {
    print(paste0(Sys.time(), ", computing condition (",param[j,1]," , ", param[j,2] , ") - ", i, "/", simN))
    res0 <- as.data.frame(MetaStudy(param[j,1], param[j,2]))
    res0 <- cbind.data.frame(Environment=param[j,1], replication=i, Condition=param[j,3], res0)
    res <- rbind(res, res0)
  }
  #colnames(res) <- c("Environ", "RepID","Condition","StdID","EstID","TrueEffect","EstEffect","SE","obs")
  save(res, file=paste0("AR/simData/simData_", param[j,3], ".RData"), compress="gzip")
}
#######################

