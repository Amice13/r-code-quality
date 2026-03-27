#Abbreviations of the environments##################################################################################
#M0405 => NICS (average)
#T2008 => NIAS
#K2009 => FRERC (average)
#F0612 => WARC (average)
Environment<-c("M0405","T2008","K2009","F0612")
Nvector<-c(110,108,110,110)#number of lines evaluated at each environment

#Simulate datasets##################################################################################################
#Create nonlinear influence datasets
SimNonLinear<-function(Env,Nsim,Nr,N=110,iAfile="iA_110xBi3102",Nsp=8,Ord=4,S=1001:11000){
  
  deBoorCox2<-function(x,i,n,knotvec){
    #x: data point
    #i: ith spline (1<=i<=Nsp)
    #n: Order. n = 4 when cubic.
    #knotvec: a vector including knot points 
    #cat(x,i,n,"\n")
    ti <-knotvec[i]
    ti1<-knotvec[i+1]
    if(n==1){
      if(ti<=x&x<ti1){return(1)}else{return(0)}
    }else{
      if(knotvec[i+n-1]!=ti) temp1<-(x-ti)/(knotvec[i+n-1]-ti)*Recall(x,i,n-1,knotvec) else temp1<-0
      if(knotvec[i+n]!=knotvec[i+1]) temp2<-(knotvec[i+n]-x)/(knotvec[i+n]-knotvec[i+1])*Recall(x,i+1,n-1,knotvec) else temp2<-0
      return(temp1+temp2)
    }
  }
  
  TrueY<-scan(paste("./RealDataAnalysis/",Env,"_DHCL.txt",sep=""),quiet=T)
  Range<-range(TrueY[1:(length(TrueY)/2)])
  Knot<-numeric(Nsp+Ord)
  a<-(Range[2]-Range[1])/(Nsp-Ord)
  for(i in 1:Ord){Knot[i]<-Range[1]-0.5*a; Knot[Nsp+Ord-i+1]<-Range[2]+0.5*a}
  for(i in (Ord+1):Nsp)Knot[i]<-Knot[i-1]+a
  
  AnalysisName<-paste("./RealDataAnalysis/mta05_",Env,"_DHCL_se",sep="")
  R<-matrix(apply(as.matrix(read.table(paste(AnalysisName,"_R.txt",sep="")))[S,],2,mean),nc=2)
  U<-apply(as.matrix(read.table(paste(AnalysisName,"_U.txt",sep="")))[S,],2,mean)
  B<-apply(as.matrix(read.table(paste(AnalysisName,"_B.txt",sep="")))[S,],2,mean)
  Pi<-apply(as.matrix(read.table(paste(AnalysisName,"_Pi1.txt",sep="")))[S,],2,mean)
  Xfile<-paste("MajorGenes_N",N,"_Nr",Nr,"_10_13",sep="")
  X<-as.matrix(read.table(paste("./RealDataAnalysis/",Xfile,".txt",sep="")))
  v<-strsplit(Xfile,"_")[[1]]
  Ncov<-as.numeric(v[c(length(v)-1,length(v))]); rm(v)
  
  #Simulate
  library(MASS)
  for(sim in 1:Nsim){
    repeat{
      Er<-mvrnorm(1,rep(0,2*N*Nr),R%x%diag(N*Nr))
      UB.DH<-U[1:N]+X[,1:Ncov[1],drop=F]%*%matrix(B[1:Ncov[1]],nc=1)
      UB.CL<-U[(N+1):(2*N)]+X[,(Ncov[1]+1):(Ncov[1]+Ncov[2]),drop=F]%*%matrix(B[(Ncov[1]+1):(Ncov[1]+Ncov[2])])
      Y<-c(rep(UB.DH,each=Nr), rep(UB.CL,each=Nr))+Er
      if(Nr==1&min(Y[1:N])>=Range[1]&max(Y[1:N])<=Range[2]) break
      if(Nr>1&min(Y[1:(N*Nr)])>=min(Knot)&max(Y[1:(N*Nr)])<=max(Knot)) break
    }
    Basis<-matrix(0,nr=Nsp,nc=N*Nr)
    for(n in 1:(N*Nr)){
      for(i in 1:Nsp){
        Basis[i,n]<-deBoorCox2(Y[n],i,Ord,Knot)
      }
    }
    Lambda<-matrix(Pi,nr=1)%*%Basis
    Y[(N*Nr+1):(2*N*Nr)]<-Y[(N*Nr+1):(2*N*Nr)]+Y[1:(N*Nr)]*Lambda
    
    #output
    Output<-paste("./SimulationAnalysis/",Env,"_nonlinear_Nr",Nr,"_Sim",sim,sep="")
    write(Y,paste(Output,"_Y.txt",sep=""),nc=1)
    write(t(Basis),paste(Output,"_Basis.txt",sep=""),nc=N)
    write(Lambda,paste(Output,"_Lambda.txt",sep=""),nc=1)
    write(cov(cbind(Y[1:(N*Nr)],Y[(N*Nr+1):(2*N*Nr)]))/2,paste(Output,"_Halfcov.txt",sep=""),nc=2)
    
    Parafile<-paste(Output,"_se.txt",sep="")
    write(paste("AnalysisName",paste(Output,"_se",sep="")),Parafile)
    write(paste("Observationfile",paste(Output,"_Y.txt",sep="")),Parafile,append=T) 
    write(paste("Relationshipfile",paste(iAfile,".txt",sep="")),Parafile,append=T)
    write(paste("Vafile",paste(Output,"_Halfcov.txt",sep="")),Parafile,append=T)
    write(paste("Vefile",paste(Output,"_Halfcov.txt",sep="")),Parafile,append=T)    
    write(paste("ResidualCorfile 0"),Parafile,append=T)
    write(paste("Xfile ",Xfile,".txt",sep=""),Parafile,append=T)
    if(Nr==1) {
    	write(paste("Zfile 0"),Parafile,append=T)
    }else {
    	write(paste("Zfile ",paste("Z",N,"_",Nr,".txt",sep="")),Parafile,append=T)
    }
    write(paste("Leaveoneout 0"),Parafile,append=T)    
    write(paste("Missingvalue 99999"),Parafile,append=T)    
    write(paste("K 2"),Parafile,append=T)
    write(paste("P",N),Parafile,append=T)    
    write(paste("N",N*Nr),Parafile,append=T)    
    write(paste("va 4"),Parafile,append=T)
    write(paste("ve 4"),Parafile,append=T)
    write(paste("Nu 0 0"),Parafile,append=T)      
    write(paste("S2 0 0"),Parafile,append=T)
    write(paste("Burnin 100000"),Parafile,append=T)      
    write(paste("Ite 1000000"),Parafile,append=T)    
    write(paste("Thi 100"),Parafile,append=T)      
    write(paste("Ft",Ncov[1],Ncov[2]),Parafile,append=T)
    write(paste("SplinePrior 2"),Parafile,append=T)      
    write(paste("AddIntercept 0"),Parafile,append=T)
    write(paste("Structurefile Model3.txt"),Parafile,append=T)      
    write(paste("NcpandOrder",Nsp,Ord,-2,0),Parafile,append=T)
    
    Parafile<-paste(Output,"_lm.txt",sep="")
    write(paste("AnalysisName",paste(Output,"_lm",sep="")),Parafile)
    write(paste("Observationfile",paste(Output,"_Y.txt",sep="")),Parafile,append=T) 
    write(paste("Relationshipfile",paste(iAfile,".txt",sep="")),Parafile,append=T)
    write(paste("Vafile",paste(Output,"_Halfcov.txt",sep="")),Parafile,append=T)
    write(paste("Vefile",paste(Output,"_Halfcov.txt",sep="")),Parafile,append=T)    
    write(paste("ResidualCorfile 0"),Parafile,append=T)
    write(paste("Xfile ",Xfile,".txt",sep=""),Parafile,append=T)
    if(Nr==1) {
    	write(paste("Zfile 0"),Parafile,append=T)
    }else {
    	write(paste("Zfile ",paste("Z",N,"_",Nr,".txt",sep="")),Parafile,append=T)
    }
    write(paste("Leaveoneout 0"),Parafile,append=T)    
    write(paste("Missingvalue 99999"),Parafile,append=T)    
    write(paste("K 2"),Parafile,append=T)
    write(paste("P",N),Parafile,append=T)    
    write(paste("N",N*Nr),Parafile,append=T)    
    write(paste("va 4"),Parafile,append=T)
    write(paste("ve 4"),Parafile,append=T)
    write(paste("Nu 0 0"),Parafile,append=T)      
    write(paste("S2 0 0"),Parafile,append=T)
    write(paste("Burnin 100000"),Parafile,append=T)      
    write(paste("Ite 1000000"),Parafile,append=T)    
    write(paste("Thi 100"),Parafile,append=T)      
    write(paste("Ft",Ncov[1],Ncov[2]),Parafile,append=T)
    write(paste("SplinePrior 2"),Parafile,append=T)      
    write(paste("AddIntercept 0"),Parafile,append=T)
    write(paste("Structurefile 0"),Parafile,append=T)         
  }#sim
}
for(d in Environment){SimNonLinear(d,20,1)}

#Create linear influence datasets
SimLinear<-function(Env,Nsim,Nr,Lambda,N=110,iAfile="iA_110xBi3102",Nsp=8,Ord=4,S=1001:11000){
  
  AnalysisName<-paste("./RealDataAnalysis/mta05_",Env,"_DHCL_lm",sep="")
  R<-matrix(apply(as.matrix(read.table(paste(AnalysisName,"_R.txt",sep="")))[S,],2,mean),nc=2)
  U<-apply(as.matrix(read.table(paste(AnalysisName,"_U.txt",sep="")))[S,],2,mean)
  B<-apply(as.matrix(read.table(paste(AnalysisName,"_B.txt",sep="")))[S,],2,mean)
  Xfile<-paste("MajorGenes_N",N,"_Nr",Nr,"_10_13",sep="")
  X<-as.matrix(read.table(paste("./RealDataAnalysis/",Xfile,".txt",sep="")))
  v<-strsplit(Xfile,"_")[[1]]
  Ncov<-as.numeric(v[c(length(v)-1,length(v))]); rm(v)

  #Simulate
  library(MASS)
  for(sim in 1:Nsim){
    
    Er<-mvrnorm(1,rep(0,2*N*Nr),R%x%diag(N*Nr))
    UB.DH<-U[1:N]+X[,1:Ncov[1],drop=F]%*%matrix(B[1:Ncov[1]],nc=1)
    UB.CL<-U[(N+1):(2*N)]+X[,(Ncov[1]+1):(Ncov[1]+Ncov[2]),drop=F]%*%matrix(B[(Ncov[1]+1):(Ncov[1]+Ncov[2])])
    Y<-c(rep(UB.DH,each=Nr), rep(UB.CL,each=Nr))+Er
    Y[(N*Nr+1):(2*N*Nr)]<-Y[(Nr*N+1):(2*N*Nr)]+Y[1:(N*Nr)]*Lambda
    
    #output
    Output<-paste("./SimulationAnalysis/",Env,"_linear",round(Lambda,1),"_Nr",Nr,"_Sim",sim,sep="")
    write(Y,paste(Output,"_Y.txt",sep=""),nc=1)
    write(cov(cbind(Y[1:(N*Nr)],Y[(N*Nr+1):(2*N*Nr)]))/2,paste(Output,"_Halfcov.txt",sep=""),nc=2)
    
    Parafile<-paste(Output,"_se.txt",sep="")
    write(paste("AnalysisName",paste(Output,"_se",sep="")),Parafile)
    write(paste("Observationfile",paste(Output,"_Y.txt",sep="")),Parafile,append=T) 
    write(paste("Relationshipfile",paste(iAfile,".txt",sep="")),Parafile,append=T)
    write(paste("Vafile",paste(Output,"_Halfcov.txt",sep="")),Parafile,append=T)
    write(paste("Vefile",paste(Output,"_Halfcov.txt",sep="")),Parafile,append=T)    
    write(paste("ResidualCorfile 0"),Parafile,append=T)
    write(paste("Xfile ",Xfile,".txt",sep=""),Parafile,append=T)
    if(Nr==1) {
      write(paste("Zfile 0"),Parafile,append=T)
    }else {
      write(paste("Zfile ",paste("Z",N,"_",Nr,".txt",sep="")),Parafile,append=T)
    }
    write(paste("Leaveoneout 0"),Parafile,append=T)    
    write(paste("Missingvalue 99999"),Parafile,append=T)    
    write(paste("K 2"),Parafile,append=T)
    write(paste("P",N),Parafile,append=T)     
    write(paste("N",N*Nr),Parafile,append=T)    
    write(paste("va 4"),Parafile,append=T)
    write(paste("ve 4"),Parafile,append=T)
    write(paste("Nu 0 0"),Parafile,append=T)      
    write(paste("S2 0 0"),Parafile,append=T)
    write(paste("Burnin 100000"),Parafile,append=T)      
    write(paste("Ite 1000000"),Parafile,append=T)    
    write(paste("Thi 100"),Parafile,append=T)      
    write(paste("Ft",Ncov[1],Ncov[2]),Parafile,append=T)
    write(paste("SplinePrior 2"),Parafile,append=T)      
    write(paste("AddIntercept 0"),Parafile,append=T)
    write(paste("Structurefile Model3.txt"),Parafile,append=T)      
    write(paste("NcpandOrder",Nsp,Ord,-2,0),Parafile,append=T)
    
    Parafile<-paste(Output,"_lm.txt",sep="")
    write(paste("AnalysisName",paste(Output,"_lm",sep="")),Parafile)
    write(paste("Observationfile",paste(Output,"_Y.txt",sep="")),Parafile,append=T) 
    write(paste("Relationshipfile",paste(iAfile,".txt",sep="")),Parafile,append=T)
    write(paste("Vafile",paste(Output,"_Halfcov.txt",sep="")),Parafile,append=T)
    write(paste("Vefile",paste(Output,"_Halfcov.txt",sep="")),Parafile,append=T)    
    write(paste("ResidualCorfile 0"),Parafile,append=T)
    write(paste("Xfile ",Xfile,".txt",sep=""),Parafile,append=T)
    if(Nr==1) {
      write(paste("Zfile 0"),Parafile,append=T)
    }else {
      write(paste("Zfile ",paste("Z",N,"_",Nr,".txt",sep="")),Parafile,append=T)
    }
    write(paste("Leaveoneout 0"),Parafile,append=T)    
    write(paste("Missingvalue 99999"),Parafile,append=T)    
    write(paste("K 2"),Parafile,append=T)
    write(paste("P",N),Parafile,append=T)     
    write(paste("N",N*Nr),Parafile,append=T)   
    write(paste("va 4"),Parafile,append=T)
    write(paste("ve 4"),Parafile,append=T)
    write(paste("Nu 0 0"),Parafile,append=T)      
    write(paste("S2 0 0"),Parafile,append=T)
    write(paste("Burnin 100000"),Parafile,append=T)      
    write(paste("Ite 1000000"),Parafile,append=T)    
    write(paste("Thi 100"),Parafile,append=T)      
    write(paste("Ft",Ncov[1],Ncov[2]),Parafile,append=T)
    write(paste("SplinePrior 2"),Parafile,append=T)      
    write(paste("AddIntercept 0"),Parafile,append=T)
    write(paste("Structurefile 0"),Parafile,append=T)         
  }#sim
}
for(d in Environment){SimLinear(d,20,1,0)}
for(d in Environment){SimLinear(d,20,1,1.2)}

####################################################################################################################
#Simulated datasets were analyzed using the mta05 program.##########################################################
####################################################################################################################




