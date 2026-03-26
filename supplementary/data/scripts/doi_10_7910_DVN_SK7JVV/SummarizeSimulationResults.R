#Abbreviations of the environments##################################################################################
#M0405 => NICS (average)
#T2008 => NIAS
#K2009 => FRERC (average)
#F0612 => WARC (average)
Environment<-c("M0405","T2008","K2009","F0612")
Nvector<-c(110,108,110,110)#number of lines evaluated at each environment

#summarize the results (for Table 2)################################################################################
SummarizeSimResults<-function(Env,SimNo,Nr,SimType,Lambda,N=110,S=1001:11000,B.CL.start=12){
  
  if(SimType=="linear"){FolderName<-paste(SimType,Lambda,sep="")}else{FolderName<-SimType}
  if(SimType=="nonlinear"){
    Lambda<-scan(paste("./SimulationAnalysis/",Env,"_nonlinear_Nr",Nr,"_Sim",SimNo,"_Lambda.txt",sep=""),quiet=T)
    U<-apply(as.matrix(read.table(paste("./RealDataAnalysis/mta05_",Env,"_DHCL_se_U.txt",sep="")))[S,],2,mean)
    B<-apply(as.matrix(read.table(paste("./RealDataAnalysis/mta05_",Env,"_DHCL_se_B.txt",sep="")))[S,],2,mean)
    G<-apply(as.matrix(read.table(paste("./RealDataAnalysis/mta05_",Env,"_DHCL_se_G.txt",sep="")))[S,],2,mean)
    R<-apply(as.matrix(read.table(paste("./RealDataAnalysis/mta05_",Env,"_DHCL_se_R.txt",sep="")))[S,],2,mean)
  }
  if(SimType=="linear"){
    U<-apply(as.matrix(read.table(paste("./RealDataAnalysis/mta05_",Env,"_DHCL_lm_U.txt",sep="")))[S,],2,mean)
    B<-apply(as.matrix(read.table(paste("./RealDataAnalysis/mta05_",Env,"_DHCL_lm_B.txt",sep="")))[S,],2,mean)
    G<-apply(as.matrix(read.table(paste("./RealDataAnalysis/mta05_",Env,"_DHCL_lm_G.txt",sep="")))[S,],2,mean)
    R<-apply(as.matrix(read.table(paste("./RealDataAnalysis/mta05_",Env,"_DHCL_lm_R.txt",sep="")))[S,],2,mean) 
  }
  
  #read samples
  Pi.sample<-as.matrix(read.table(paste("./SimulationAnalysis/mta05_",Env,"_",FolderName,"_Nr",Nr,"_Sim",SimNo,"_se_Pi1.txt",sep="")))
  Basis<-as.matrix(read.table(paste("./SimulationAnalysis/mta05_",Env,"_",FolderName,"_Nr",Nr,"_Sim",SimNo,"_se_Basis1.txt",sep="")))
  Lambda.sample<-Pi.sample%*%Basis
  U.sample<-as.matrix(read.table(paste("./SimulationAnalysis/mta05_",Env,"_",FolderName,"_Nr",Nr,"_Sim",SimNo,"_se_U.txt",sep="")))
  U.CL.mean<-apply(U.sample[S,(N+1):(2*N)],2,mean)
  B.sample<-as.matrix(read.table(paste("./SimulationAnalysis/mta05_",Env,"_",FolderName,"_Nr",Nr,"_Sim",SimNo,"_se_B.txt",sep="")))
  B.CL.mean<-apply(B.sample[S,B.CL.start:ncol(B.sample)],2,mean)
  WAIC<-as.numeric(scan(paste("./SimulationAnalysis/mta05_",Env,"_",FolderName,"_Nr",Nr,"_Sim",SimNo,"_se_Criterion.txt",sep=""),skip=1,nlines=1,what="character",quiet=T)[3])
  G.sample<-as.matrix(read.table(paste("./SimulationAnalysis/mta05_",Env,"_",FolderName,"_Nr",Nr,"_Sim",SimNo,"_se_G.txt",sep="")))
  R.sample<-as.matrix(read.table(paste("./SimulationAnalysis/mta05_",Env,"_",FolderName,"_Nr",Nr,"_Sim",SimNo,"_se_R.txt",sep="")))
  
  #read samples (rm)
  U.sample.lm<-as.matrix(read.table(paste("./SimulationAnalysis/mta05_",Env,"_",FolderName,"_Nr",Nr,"_Sim",SimNo,"_lm_U.txt",sep="")))
  U.CL.mean.lm<-apply(U.sample.lm[S,(N+1):(2*N)],2,mean)
  B.sample.lm<-as.matrix(read.table(paste("./SimulationAnalysis/mta05_",Env,"_",FolderName,"_Nr",Nr,"_Sim",SimNo,"_lm_B.txt",sep="")))
  B.CL.mean.lm<-apply(B.sample.lm[S,B.CL.start:ncol(B.sample.lm)],2,mean)
  WAIC.lm<-as.numeric(scan(paste("./SimulationAnalysis/mta05_",Env,"_",FolderName,"_Nr",Nr,"_Sim",SimNo,"_lm_Criterion.txt",sep=""),skip=1,nlines=1,what="character",quiet=T)[3])
  
  #Summary
  Result<-c(sum((Lambda-apply(Lambda.sample[S,],2,mean))^2), #Sum of squared errors of Lambda
            cor(U[(N+1):(2*N)],U.CL.mean),
            cor(U[(N+1):(2*N)],U.CL.mean.lm),
            sum((U[(N+1):(2*N)]-U.CL.mean)^2),
            sum((U[(N+1):(2*N)]-U.CL.mean.lm)^2),
            cor(B[B.CL.start:length(B)],B.CL.mean),
            cor(B[B.CL.start:length(B)],B.CL.mean.lm),
            sum((B[B.CL.start:length(B)]-B.CL.mean)^2),
            sum((B[B.CL.start:length(B)]-B.CL.mean.lm)^2),  
            WAIC-WAIC.lm,
            as.vector(cor(cbind(G.sample[,2],R.sample[,2],Pi.sample))[-c(1,2),c(1,2)]))
  if(SimType=="nonlinear") Result<-c(cor(Lambda,apply(Lambda.sample[S,],2,mean)),Result)
  Result
}

SummarySimNonlinear<-NULL
for(d in Environment){
  for(sim in 1:20){
    cat(d,sim,"\n")
    SummarySimNonlinear<-rbind(SummarySimNonlinear,SummarizeSimResults(d,sim,1,"nonlinear",0))
  }
}
SummarySimNonlinear<-data.frame(Sim=paste(rep(Environment[1:6],each=20),"Sim",rep(1:20,6),sep=""),SummarySimNonlinear)
colnames(SummarySimNonlinear)<-c("Sim","CorLambda","SELambda","CorUse","CorUlm","SEUse","SEUlm","CorBse","CorBlm","SEBse","SEBlm","deltaWAIC",
                                 paste("CorWithG",0:7,sep=""),paste("CorWithR",0:7,sep=""))
write.table(SummarySimNonlinear,"SummarySimNonlinear.txt",quote=F,row.names=F)

SummarySimLinear0<-NULL
for(d in Environment){
  for(sim in 1:20){
    cat(d,sim,"\n")
    SummarySimLinear0<-rbind(SummarySimLinear0,SummarizeSimResults(d,sim,1,"linear",0))
  }
}
SummarySimLinear0<-data.frame(Sim=paste(rep(Environment[1:6],each=20),"Sim",rep(1:20,6),sep=""),SummarySimLinear0)
colnames(SummarySimLinear0)<-c("Sim","SELambda","CorUse","CorUlm","SEUse","SEUlm","CorBse","CorBlm","SEBse","SEBlm","deltaWAIC",
                               paste("CorWithG",0:7,sep=""),paste("CorWithR",0:7,sep=""))
write.table(SummarySimLinear0,"SummarySimLinear0.txt",quote=F,row.names=F)

SummarySimLinear1.2<-NULL
for(d in Environment){
  for(sim in 1:20){
    cat(d,sim,"\n")
    SummarySimLinear1.2<-rbind(SummarySimLinear1.2,SummarizeSimResults(d,sim,1,"linear",1.2))
  }
}
SummarySimLinear1.2<-data.frame(Sim=paste(rep(Environment[1:6],each=20),"Sim",rep(1:20,6),sep=""),SummarySimLinear1.2)
colnames(SummarySimLinear1.2)<-c("Sim","SELambda","CorUse","CorUlm","SEUse","SEUlm","CorBse","CorBlm","SEBse","SEBlm","deltaWAIC",
                                 paste("CorWithG",0:7,sep=""),paste("CorWithR",0:7,sep=""))
write.table(SummarySimLinear1.2,"SummarySimLinear1.2.txt",quote=F,row.names=F)


#Plot for the figures of the paper (for Fig 4)####################################################################
Superimpose<-function(Env,SimNovec,Nr,SimType,Lambda,N=110,S=1001:11000){
  
  if(SimType=="nonlinear") FolderName<-SimType else FolderName<-paste(SimType,Lambda,sep="")
  
  Lambda.mean<-Y<-matrix(0,nr=length(SimNovec),nc=N*Nr)
  
  for(i in 1:length(SimNovec)){
    SimNo<-SimNovec[i]
    Y[i,]<-scan(paste("./SimulationAnalysis/",Env,"_",FolderName,"_Nr",Nr,"_Sim",SimNo,"_Y.txt",sep=""),quiet=T)[1:(N*Nr)]
    Yorder<-order(Y[i,])
    Y[i,]<-Y[i,Yorder]
    #read samples
    Pi.sample<-as.matrix(read.table(paste("./SimulationAnalysis/mta05_",Env,"_",FolderName,"_Nr",Nr,"_Sim",SimNo,"_se_Pi1.txt",sep="")))
    Basis<-as.matrix(read.table(paste("./SimulationAnalysis/mta05_",Env,"_",FolderName,"_Nr",Nr,"_Sim",SimNo,"_se_Basis1.txt",sep="")))
    Lambda.sample<-Pi.sample%*%Basis
    Lambda.mean[i,]<-apply(Lambda.sample[S,],2,mean)[Yorder]
  }
  
  if(SimType=="nonlinear"){
    Lambda<-apply(as.matrix(read.table(paste("./RealDataAnalysis/mta05_",Env,"_DHCL_se_Lambda1.txt",sep="")))[S,],2,mean)
    Yreal<-scan(paste("./RealDataAnalysis/",Env,"_DHCL.txt",sep=""),quiet=T)
    Yreal<-Yreal[1:(length(Yreal)/2)]
    Yreal.order<-order(Yreal)
    Yreal<-Yreal[Yreal.order]
    Lambda<-Lambda[Yreal.order]
  }
  
  #plot
  Ylim<-c(-1.8,1.8)
  if(SimType=="nonlinear"){
    Xlim<-range(as.vector(Y),Yreal)
    
    tiff(paste("./SimulationAnalysis/",Env,"_",FolderName,"_Nr",Nr,"_Sim",min(SimNovec),"-",max(SimNovec),".tif",sep=""),unit="cm",width=4,height=4,res=600)
    par(mar=c(1.2,1.1,0.5,0.1))
    par(mgp=c(0.4,0.3,0))
    i<-1
    plot(Y[i,],Lambda.mean[i,],type="l",xlab="",ylab="",main="",col="gray60",ylim=Ylim,xlim=Xlim,
         cex.axis=0.6,tcl=-0.2)
    for(i in 2:length(SimNovec)){
      points(Y[i,],Lambda.mean[i,],type="l",col="gray60")
    }
    abline(h=0,lty=3)    
    points(Yreal,Lambda,type="l",col=2,lwd=1.5)
    dev.off() 
  }
  if(SimType=="linear"){
    Xlim<-range(as.vector(Y))
    
    tiff(paste("./SimulationAnalysis/",Env,"_",FolderName,"_Nr",Nr,"_Sim",min(SimNovec),"-",max(SimNovec),".tif",sep=""),unit="cm",width=4,height=4,res=600)
    par(mar=c(1.2,1.1,0.5,0.1))
    par(mgp=c(0.4,0.3,0))
    i<-1
    plot(Y[i,],Lambda.mean[i,],type="l",xlab="",ylab="",main="",col="gray60",ylim=Ylim,xlim=Xlim,
         cex.axis=0.6,tcl=-0.2)
    for(i in 2:length(SimNovec)){
      points(Y[i,],Lambda.mean[i,],type="l",col="gray60")
    }
    abline(h=0,lty=3)      
    abline(h=Lambda,col=2,lwd=1.5)
    dev.off()
  }
}
for(d in Environment){
  Superimpose(d,1:20,1,"nonlinear",0)
  Superimpose(d,1:20,1,"linear",0)
  Superimpose(d,1:20,1,"linear",1.2)
}
