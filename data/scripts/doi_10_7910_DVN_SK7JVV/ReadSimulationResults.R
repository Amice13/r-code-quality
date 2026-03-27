#Abbreviations of the environments##################################################################################
#M0405 => NICS (average)
#T2008 => NIAS
#K2009 => FRERC (average)
#F0612 => WARC (average)
Environment<-c("M0405","T2008","K2009","F0612")
Nvector<-c(110,108,110,110)#number of lines evaluated at each environment


#Read and save all simulation results##############################################################################
ReadSimResults<-function(Env,SimNo,Nr,SimType,Lambda,N=110,S=1001:11000,B.CL.start=12){
  
  if(SimType=="linear"){FolderName<-paste(SimType,Lambda,sep="")}else{FolderName<-SimType}
  
  #read samples
  Pi.sample<-as.matrix(read.table(paste("./SimulationAnalysis/mta05_",Env,"_",FolderName,"_Nr",Nr,"_Sim",SimNo,"_se_Pi1.txt",sep="")))
  Basis<-as.matrix(read.table(paste("./SimulationAnalysis/mta05_",Env,"_",FolderName,"_Nr",Nr,"_Sim",SimNo,"_se_Basis1.txt",sep="")))
  Lambda.sample<-Pi.sample%*%Basis
  U.sample<-as.matrix(read.table(paste("./SimulationAnalysis/mta05_",Env,"_",FolderName,"_Nr",Nr,"_Sim",SimNo,"_se_U.txt",sep="")))
  B.sample<-as.matrix(read.table(paste("./SimulationAnalysis/mta05_",Env,"_",FolderName,"_Nr",Nr,"_Sim",SimNo,"_se_B.txt",sep="")))
  WAIC<-as.numeric(scan(paste("./SimulationAnalysis/mta05_",Env,"_",FolderName,"_Nr",Nr,"_Sim",SimNo,"_se_Criterion.txt",sep=""),skip=1,nlines=1,what="character",quiet=T)[3])
  G.sample<-as.matrix(read.table(paste("./SimulationAnalysis/mta05_",Env,"_",FolderName,"_Nr",Nr,"_Sim",SimNo,"_se_G.txt",sep="")))
  R.sample<-as.matrix(read.table(paste("./SimulationAnalysis/mta05_",Env,"_",FolderName,"_Nr",Nr,"_Sim",SimNo,"_se_R.txt",sep="")))
  
  #read samples (rm)
  U.sample.lm<-as.matrix(read.table(paste("./SimulationAnalysis/mta05_",Env,"_",FolderName,"_Nr",Nr,"_Sim",SimNo,"_lm_U.txt",sep="")))
  B.sample.lm<-as.matrix(read.table(paste("./SimulationAnalysis/mta05_",Env,"_",FolderName,"_Nr",Nr,"_Sim",SimNo,"_lm_B.txt",sep="")))
  WAIC.lm<-as.numeric(scan(paste("./SimulationAnalysis/mta05_",Env,"_",FolderName,"_Nr",Nr,"_Sim",SimNo,"_lm_Criterion.txt",sep=""),skip=1,nlines=1,what="character",quiet=T)[3])
  
  Result<-list(Pi.sample=Pi.sample,Lambda.sample=Lambda.sample,U.sample=U.sample,B.sample=B.sample,WAIC=WAIC,G.sample=G.sample,R.sample=R.sample,
               U.sample.lm=U.sample.lm,B.sample.lm=B.sample.lm,WAIC.lm=WAIC.lm)
  Result
}


for(d in Environment[2:4]){
  
  SimResult<-as.list(numeric(20))
  for(sim in 1:20){
    cat(d,sim,"\n")
    SimResult[[sim]]<-ReadSimResults(d,sim,1,"nonlinear",0)
  }
  save.image(paste("SimulationResults_nonlinear_",d,".RData",sep=""))
  
  SimResult<-as.list(numeric(20))
  for(sim in 1:20){
    cat(d,sim,"\n")
    SimResult[[sim]]<-ReadSimResults(d,sim,1,"linear",0)
  }  
  save.image(paste("SimulationResults_linear0_",d,".RData",sep=""))

  SimResult<-as.list(numeric(20))    
  for(sim in 1:20){
    cat(d,sim,"\n")
    SimResult[[sim]]<-ReadSimResults(d,sim,1,"linear",1.2)
  }
  save.image(paste("SimulationResults_linear1.2_",d,".RData",sep=""))  
}


