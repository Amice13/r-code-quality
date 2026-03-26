#Abbreviations of the environments##################################################################################
#M0405 => NICS (average)
#T2008 => NIAS
#K2009 => FRERC (average)
#F0612 => WARC (average)
#M2004,M2005 => NICS in 2004 and 2005
#KE2009, KL2009 => two replicates at FRERC
#F2006-F2012 => WARC in 2006 to 2012

#We used M0405, T2008, K2009, and F0612
Environment<-c("M0405","T2008","K2009","F0612")

#Input files########################################################################################################
#M0405_DHCL.txt, T2008_DHCL.txt, K2009_DHCL.txt, F0612_DHCL.txt, M2004_DHCL.txt, M2005_DHCL.txt, 
#KE2009_DHCL.txt, KL2009_DHCL.txt, F2006-F2012_DHCL.txt
#These files contain the phenotypic values of DH and CL at each environment.
#The first half values are the DH and the last half values are the CL values
#The order of the cultivars are the same as that in Supplementary Material
#The 59th and 73th cultivars were not evaluated at NIAS

#iA_110x3102.txt
#This file contains the inverse of the genetic relationship matrix between the 110 cultivars.
#The markers used in Onogi et al. 2015 and the function A.mat in the rrBLUP package were used for calculation
#The order of the cultivars are the same as that in Supplementary Material

#IdentityMatrix.txt
#This contains the 2 x 2 identity matrix which was used when the non-informative prior was used for the residual covariance of NSE

#MajorGenes_N108_Nr1_10_13.txt/MajorGenes_N110_Nr1_10_13.txt
#These files contain the intercepts and the major gene genotypes copied from the Supplementary Material
#The first 10 columns were for DH and the last 13 columns were for CL.

#Z108.txt
#This file contains the design matrix that relates the relationship matrix with the phenotypic values. This was only used for the NIAS data.

#Model3.txt
#This file specifies the position of the recursive effects between the traits and the modeling schemes of the recursive effects.
#The first and second columns (rows) indicate DH and CL, respectively. The number (3) in position [2, 1] means that DH influences CL.
#3 means that B-spline is used. When 1 is entered, a linear effect is estimated. See the source code for 2.

#MajorGeneNames.txt
#This contains the names of the major gene polymorphims.

#(Co)variance between traits (Table2)################################################################################
for(d in Environment){  
  DH<-scan(paste(d,"_DHCL.txt",sep=""))
  CL<-DH[(length(DH)/2+1):length(DH)]
  DH<-DH[1:(length(DH)/2)]
  write(cov(cbind(DH,CL))/2,paste(d,"_DHCL_Halfcov.txt",sep=""),nc=2) #to define the prior distributions of the genetic and residual covariances
  Result<-round(cov(cbind(DH,CL)),1)
  Result[1,2]<-round(cor(DH,CL),2)
  write(d,"PhenotypicCovariance.txt",nc=2,append=T)
  write(Result,"PhenotypicCovariance.txt",nc=2,append=T)
  Test<-cor.test(DH,CL,"two.sided")
  write(paste("p-value:",Test[[3]]),"PhenotypicCovariance.txt",append=T)
  write("","PhenotypicCovariance.txt",append=T)
}
rm(DH,CL,Result,Test)

#correlation between years/replicates (Tables S1 and S2)############################################################
M0405.cor<-round(cor(cbind(scan("M2004_DHCL.txt")[1:110],
                scan("M2005_DHCL.txt")[1:110])),2)
M0405.cor[lower.tri(M0405.cor)]<-round(cor(cbind(scan("M2004_DHCL.txt")[111:220],
                scan("M2005_DHCL.txt")[111:220])),2)[lower.tri(M0405.cor)]

K2009.cor<-round(cor(cbind(scan("KE2009_DHCL.txt")[1:110],
                scan("KL2009_DHCL.txt")[1:110])),2)
K2009.cor[lower.tri(K2009.cor)]<-round(cor(cbind(scan("KE2009_DHCL.txt")[111:220],
                scan("KL2009_DHCL.txt")[111:220])),2)[lower.tri(K2009.cor)]

F0612.cor<-round(cor(cbind(scan("F2006_DHCL.txt")[1:110],
                scan("F2007_DHCL.txt")[1:110],
                scan("F2008_DHCL.txt")[1:110],
                scan("F2009_DHCL.txt")[1:110],
                c(scan("F2010_DHCL.txt")[1:5],NA,scan("F2010_DHCL.txt")[6:109]),
                scan("F2011_DHCL.txt")[1:110],
                scan("F2012_DHCL.txt")[1:110]),use="pairwise.complete.obs"),2)

F0612.cor[lower.tri(F0612.cor)]<-round(cor(cbind(scan("F2006_DHCL.txt")[111:220],
                scan("F2007_DHCL.txt")[111:220],
                scan("F2008_DHCL.txt")[111:220],
                scan("F2009_DHCL.txt")[111:220],
                c(scan("F2010_DHCL.txt")[110:114],NA,scan("F2010_DHCL.txt")[115:218]),
                scan("F2011_DHCL.txt")[111:220],
                scan("F2012_DHCL.txt")[111:220]),use="pairwise.complete.obs"),2)[lower.tri(F0612.cor)]


#function to create the parameter files of mta05####################################################################
#se and lm indicate NSE and OLM, respectively
CreateParaFile.mta05<-function(TraitName,model=c("se","lm"),Env,PriorGFile,PriorRFile,Xfile,Zfile,N,Nr){

  K<-length(TraitName)
  if(K==1) Dataset<-paste(Env,TraitName,sep="_")
  if(K==2) Dataset<-paste(Env,paste(TraitName,collapse=""),sep="_")
  v<-strsplit(Xfile,"_")[[1]]
  if(K==1) Ncov<-as.numeric(strsplit(v[length(v)],".t")[[1]][1]) else Ncov<-as.numeric(c(v[length(v)-1],strsplit(v[length(v)],".t")[[1]][1]))

  Parafile<-paste(Dataset,"_",model,".txt",sep="")
  write(paste("AnalysisName",paste(Dataset,model,sep="_")),Parafile)
  write(paste("Observationfile ",Dataset,".txt",sep=""),Parafile,append=T) 
  write(paste("Relationshipfile iA_110xBi3102.txt"),Parafile,append=T)
  write(paste("Vafile",PriorGFile),Parafile,append=T)
  write(paste("Vefile",PriorRFile),Parafile,append=T)    
  write(paste("ResidualCorfile 0"),Parafile,append=T)
  write(paste("Xfile",Xfile),Parafile,append=T)
  write(paste("Zfile",Zfile),Parafile,append=T)
  write(paste("Leaveoneout 0"),Parafile,append=T)    
  write(paste("Missingvalue 99999"),Parafile,append=T)    
  write(paste("K",K),Parafile,append=T)
  write(paste("P 110"),Parafile,append=T)    
  write(paste("N",N*Nr),Parafile,append=T)   
  write(paste("va 4"),Parafile,append=T)
  write(paste("ve 4"),Parafile,append=T)
  if(K==1){
    write(paste("Nu -2"),Parafile,append=T)      
    write(paste("S2 0"),Parafile,append=T)     
  }
  if(K==2){
    write(paste("Nu 0 0"),Parafile,append=T)      
    write(paste("S2 0 0"),Parafile,append=T)    
  }
  write(paste("Burnin 100000"),Parafile,append=T)      
  write(paste("Ite 1000000"),Parafile,append=T)    
  write(paste("Thi 100"),Parafile,append=T)
  if(K==1) write(paste("Ft",Ncov[1]),Parafile,append=T)
  if(K==2) write(paste("Ft",Ncov[1],Ncov[2]),Parafile,append=T)
  write(paste("SplinePrior 2"),Parafile,append=T)      
  write(paste("AddIntercept 0"),Parafile,append=T)
  if(model=="se"){
    write(paste("Structurefile Model3.txt"),Parafile,append=T)      
    write(paste("NcpandOrder 8 4 -2 0"),Parafile,append=T)
  }
  if(model=="lm"){
    write(paste("Structurefile 0"),Parafile,append=T)      
  }
}

#Create the parameter files of mta05###############################################################################
for(env in Environment){
  if(env=="T2008"){N<-108;Zfile<-"Z108.txt"} else {N<-110;Zfile<-"0"}
  #Parameter file for NSE
  CreateParaFile.mta05(c("DH","CL"),"se",env,paste(env,"_DHCL_Halfcov.txt",sep=""),paste(env,"_DHCL_Halfcov.txt",sep=""),
                       paste("MajorGenes_N",N,"_Nr1_10_13.txt",sep=""),Zfile,N,1)
  #Parameter file for OLM
  CreateParaFile.mta05(c("DH","CL"),"lm",env,paste(env,"_DHCL_Halfcov.txt",sep=""),paste(env,"_DHCL_Halfcov.txt",sep=""),
                       paste("MajorGenes_N",N,"_Nr1_10_13.txt",sep=""),Zfile,N,1)    
}

#Files with "se" are the parameter files of NSE, and files with "lm" are the parameter files of OLM

####################################################################################################################
#Real data analyses were conducted using the mta05 program.#########################################################
#The program requires the following input files (e.g. in the case of M0405)
#M0405_DHCL.txt
#iA_110xBi3102.txt
#M0405_DHCL_Halfcov.txt
#MajorGenes_N110_Nr1_10_13.txt
#Model3.txt
#For the NIAS analysis, Z108.txt is also required.

####################################################################################################################

####################################################################################################################
#Real data analyses with the non-informative prior of the residual (co)variances were also conducted using mta05####
#The parameter files were manually prepared.
#The file names contain "nir"

####################################################################################################################


#Plot the results###################################################################################################
PlotResults<-function(Env,Program,Nir="ir",Nr=1,S=1001:11000,B.CL.start=12){
  
  if(Nir=="nir"){se<-"se_nir"}else{se<-"se"}
  
  if(Env=="T2008"){
  	Earlyheading.I <- c(1:6,75)
  	Earlyheading.L <- c(76,103)  	
  }else{
  	Earlyheading.I <- c(1:6,77)
  	Earlyheading.L <- c(78,105)  		
  }
  Earlyheading<-c(1:6,77,78,105)  
  Earlyheading.names<-c("Kirara397","Hoshinoyume","Yukihikari","Hayamasari","Hatsushizuku","Yukara","Eiko","Akage","Bozu")
  Majorgenes<-scan("MajorGeneNames.txt",what="character")
  
  Y<-scan(paste(Env,"_DHCL.txt",sep=""))
  N<-length(Y)/2
 
  #read samples (NSE)
  Pi.sample<-as.matrix(read.table(paste(Program,"_",Env,"_DHCL_",se,"_Pi1.txt",sep="")))
  Basis<-as.matrix(read.table(paste(Program,"_",Env,"_DHCL_se_Basis1.txt",sep="")))
  Lambda.sample<-Pi.sample%*%Basis
  G.sample<-as.matrix(read.table(paste(Program,"_",Env,"_DHCL_",se,"_G.txt",sep="")))
  R.sample<-as.matrix(read.table(paste(Program,"_",Env,"_DHCL_",se,"_R.txt",sep="")))
  U.sample<-as.matrix(read.table(paste(Program,"_",Env,"_DHCL_",se,"_U.txt",sep="")))
  U.DH.mean<-apply(U.sample[S,1:(ncol(U.sample)/2)],2,mean)
  U.DH.sd<-apply(U.sample[S,1:(ncol(U.sample)/2)],2,sd)  
  U.CL.mean<-apply(U.sample[S,(ncol(U.sample)/2+1):ncol(U.sample)],2,mean)
  U.CL.sd<-apply(U.sample[S,(ncol(U.sample)/2+1):ncol(U.sample)],2,sd)
  B.sample<-as.matrix(read.table(paste(Program,"_",Env,"_DHCL_",se,"_B.txt",sep="")))
  B.DH.mean<-apply(B.sample[S,2:(B.CL.start-2)],2,mean)
  B.DH.sd<-apply(B.sample[S,2:(B.CL.start-2)],2,sd)
  B.DH.sig<-apply(B.sample[S,2:(B.CL.start-2)],2,quantile,probs=0.975)*apply(B.sample[S,2:(B.CL.start-2)],2,quantile,probs=0.025)
  B.DH.sig[B.DH.sig>0]<-1
  B.DH.sig[B.DH.sig<=0]<-0
  B.CL.mean<-apply(B.sample[S,B.CL.start:ncol(B.sample)],2,mean)
  B.CL.sd<-apply(B.sample[S,B.CL.start:ncol(B.sample)],2,sd)
  B.CL.sig<-apply(B.sample[S,B.CL.start:ncol(B.sample)],2,quantile,probs=0.975)*apply(B.sample[S,B.CL.start:ncol(B.sample)],2,quantile,probs=0.025)
  B.CL.sig[B.CL.sig>0]<-1
  B.CL.sig[B.CL.sig<=0]<-0
  WAIC<-as.numeric(scan(paste(Program,"_",Env,"_DHCL_",se,"_Criterion.txt",sep=""),skip=1,nlines=1,what="character",quiet=T)[3])
  
  #read samples (OLM)
  G.sample.rm<-as.matrix(read.table(paste(Program,"_",Env,"_DHCL_lm_G.txt",sep="")))
  R.sample.rm<-as.matrix(read.table(paste(Program,"_",Env,"_DHCL_lm_R.txt",sep="")))
  U.sample.rm<-as.matrix(read.table(paste(Program,"_",Env,"_DHCL_lm_U.txt",sep="")))
  U.DH.mean.rm<-apply(U.sample.rm[S,1:(ncol(U.sample)/2)],2,mean)
  U.DH.sd.rm<-apply(U.sample.rm[S,1:(ncol(U.sample)/2)],2,sd)  
  U.CL.mean.rm<-apply(U.sample.rm[S,(ncol(U.sample)/2+1):ncol(U.sample)],2,mean)
  U.CL.sd.rm<-apply(U.sample.rm[S,(ncol(U.sample)/2+1):ncol(U.sample)],2,sd)
  B.sample.rm<-as.matrix(read.table(paste(Program,"_",Env,"_DHCL_lm_B.txt",sep="")))
  B.DH.mean.rm<-apply(B.sample.rm[S,2:(B.CL.start-2)],2,mean)
  B.DH.sd.rm<-apply(B.sample.rm[S,2:(B.CL.start-2)],2,sd)
  B.DH.sig.rm<-apply(B.sample.rm[S,2:(B.CL.start-2)],2,quantile,probs=0.975)*apply(B.sample.rm[S,2:(B.CL.start-2)],2,quantile,probs=0.025)
  B.DH.sig.rm[B.DH.sig.rm>0]<-1
  B.DH.sig.rm[B.DH.sig.rm<=0]<-0  
  B.CL.mean.rm<-apply(B.sample.rm[S,B.CL.start:ncol(B.sample.rm)],2,mean)
  B.CL.sd.rm<-apply(B.sample.rm[S,B.CL.start:ncol(B.sample.rm)],2,sd)
  B.CL.sig.rm<-apply(B.sample.rm[S,B.CL.start:ncol(B.sample.rm)],2,quantile,probs=0.975)*apply(B.sample.rm[S,B.CL.start:ncol(B.sample.rm)],2,quantile,probs=0.025)
  B.CL.sig.rm[B.CL.sig.rm>0]<-1
  B.CL.sig.rm[B.CL.sig.rm<=0]<-0
  WAIC.rm<-as.numeric(scan(paste(Program,"_",Env,"_DHCL_lm_Criterion.txt",sep=""),skip=1,nlines=1,what="character",quiet=T)[3])
  
  #plot
  Yorder<-order(Y[1:(N*Nr)])
  if(Nir=="nir") 
    tiff(paste("./Figures/",Env,".nir.tif",sep=""),unit="cm",width=17.4,height=24,res=200) 
  else
    tiff(paste("./Figures/",Env,".tif",sep=""),unit="cm",width=17.4,height=24,res=200)
  
  par(cex=0.5)
  par(mfrow=c(4,4))
  par(mar=c(6,2.2,2,0.5))

  Pos<-matrix(1:(N*Nr),nr=Nr)
  Xlim<-range(Y[1:(N*Nr)])
  Ylim<-range(Y[(1+N*Nr):(2*N*Nr)])
  plot(Y[as.vector(Pos[,-c(Earlyheading.I,Earlyheading.L)])],Y[(N*Nr)+as.vector(Pos[,-c(Earlyheading.I,Earlyheading.L)])],xlab="DH",ylab="CL",main="Phenotypic values",ylim=Ylim,xlim=Xlim)
  points(Y[as.vector(Pos[,Earlyheading.I])],Y[(N*Nr)+as.vector(Pos[,Earlyheading.I])],pch=2)
  points(Y[as.vector(Pos[,Earlyheading.L])],Y[(N*Nr)+as.vector(Pos[,Earlyheading.L])],pch=19)
     
  Lambda.mean<-apply(Lambda.sample[S,],2,mean)
  Lambda.0.975<-apply(Lambda.sample[S,],2,quantile,probs=0.975)
  Lambda.0.025<-apply(Lambda.sample[S,],2,quantile,probs=0.025)
  Ylim<-range(c(Lambda.0.975,Lambda.mean,Lambda.0.025,0))
  plot(Y[1:(N*Nr)][Yorder],Lambda.mean[Yorder],type="l",xlab="DH",ylab="Lambda",main="Lambda",col=2,ylim=Ylim)
  points(Y[1:(N*Nr)][Yorder],Lambda.0.975[Yorder],type="l",lty=2,col=2)
  points(Y[1:(N*Nr)][Yorder],Lambda.0.025[Yorder],type="l",lty=2,col=2)
  abline(h=0,lty=3)

  Ylim<-range(c(U.DH.mean[Earlyheading]+U.DH.sd[Earlyheading],U.DH.mean.rm[Earlyheading]+U.DH.sd.rm[Earlyheading],
                U.DH.mean[Earlyheading]-U.DH.sd[Earlyheading],U.DH.mean.rm[Earlyheading]-U.DH.sd.rm[Earlyheading]))
  v<-barplot(rbind(U.DH.mean[Earlyheading],U.DH.mean.rm[Earlyheading]),beside=T,col=c(2,8),  
          xlab="",ylab="cm",main="Polygenic (DH)",border=c(2,1),
          names.arg=Earlyheading.names,las=2,ylim=Ylim,cex.names=0.9)
              
  Ylim<-range(c(U.CL.mean[Earlyheading]+U.CL.sd[Earlyheading],U.CL.mean.rm[Earlyheading]+U.CL.sd.rm[Earlyheading],
                U.CL.mean[Earlyheading]-U.CL.sd[Earlyheading],U.CL.mean.rm[Earlyheading]-U.CL.sd.rm[Earlyheading]))
  v<-barplot(rbind(U.CL.mean[Earlyheading],U.CL.mean.rm[Earlyheading]),beside=T,col=c(2,8),  
          xlab="",ylab="cm",main="Polygenic (CL)",border=c(2,1),
          names.arg=Earlyheading.names,las=2,ylim=Ylim,cex.names=0.9)

  Ylim<-range(c(B.DH.mean+B.DH.sd,B.DH.mean.rm+B.DH.sd.rm,
                B.DH.mean-B.DH.sd,B.DH.mean.rm-B.DH.sd.rm))
  v<-barplot(rbind(B.DH.mean,B.DH.mean.rm),xlab="",ylab="cm",main="Gene effects (DH)",
             col=as.vector(rbind(B.DH.sig*2,B.DH.sig.rm*8)),beside=T,
             border=c(2,1),names.arg=Majorgenes[-c(1:3)],las=2,ylim=Ylim,cex.names=0.9)
  arrows(as.vector(v),
         as.vector(rbind(B.DH.mean,B.DH.mean.rm)),
         as.vector(v),
         as.vector(rbind(B.DH.mean,B.DH.mean.rm))+as.vector(rbind(B.DH.sd,B.DH.sd.rm)),
         angle=90,length=0.015)
  arrows(as.vector(v),
         as.vector(rbind(B.DH.mean,B.DH.mean.rm)),
         as.vector(v),
         as.vector(rbind(B.DH.mean,B.DH.mean.rm))-as.vector(rbind(B.DH.sd,B.DH.sd.rm)),
         angle=90,length=0.015)

  Ylim<-range(c(B.CL.mean+B.CL.sd,B.CL.mean.rm+B.CL.sd.rm,
                B.CL.mean-B.CL.sd,B.CL.mean.rm-B.CL.sd.rm))
  v<-barplot(rbind(B.CL.mean,B.CL.mean.rm),xlab="",ylab="cm",main="Gene effects (CL)",
          col=as.vector(rbind(B.CL.sig*2,B.CL.sig.rm*8)),beside=T,
          border=c(2,1),names.arg=Majorgenes,las=2,ylim=Ylim,cex.names=0.9)
  arrows(as.vector(v),
         as.vector(rbind(B.CL.mean,B.CL.mean.rm)),
         as.vector(v),
         as.vector(rbind(B.CL.mean,B.CL.mean.rm))+as.vector(rbind(B.CL.sd,B.CL.sd.rm)),
         angle=90,length=0.015)
  arrows(as.vector(v),
         as.vector(rbind(B.CL.mean,B.CL.mean.rm)),
         as.vector(v),
         as.vector(rbind(B.CL.mean,B.CL.mean.rm))-as.vector(rbind(B.CL.sd,B.CL.sd.rm)),
         angle=90,length=0.015) 

  w<-density(G.sample.rm[S,1]);v<-density(G.sample[S,1])
  Ylim<-c(0,max(c(max(v[[2]]),max(w[[2]]))))
  plot(v,xlab="",ylab="Density",main="Genetic variance (DH)",ylim=Ylim,col=2)
  points(w,type="l")
  
  w<-density(G.sample.rm[S,2]);v<-density(G.sample[S,2])
  Ylim<-c(0,max(c(max(v[[2]]),max(w[[2]]))))
  plot(v,xlab="",ylab="Density",main="Genetic covariance",ylim=Ylim,col=2)
  points(w,type="l")
  
  w<-density(G.sample.rm[S,4]);v<-density(G.sample[S,4])
  Ylim<-c(0,max(c(max(v[[2]]),max(w[[2]]))))
  plot(v,xlab="",ylab="Density",main="Genetic variance (CL)",ylim=Ylim,col=2)
  points(w,type="l")  
  
  w<-density(R.sample.rm[S,1]);v<-density(R.sample[S,1])
  Ylim<-c(0,max(c(max(v[[2]]),max(w[[2]]))))
  plot(v,xlab="",ylab="Density",main="Residual variance (DH)",ylim=Ylim,col=2)
  points(w,type="l")
  
  w<-density(R.sample.rm[S,2]);v<-density(R.sample[S,2])
  Ylim<-c(0,max(c(max(v[[2]]),max(w[[2]]))))
  plot(v,xlab="",ylab="Density",main="Residual covariance",ylim=Ylim,col=2)
  points(w,type="l")
  
  w<-density(R.sample.rm[S,4]);v<-density(R.sample[S,4])
  Ylim<-c(0,max(c(max(v[[2]]),max(w[[2]]))))
  plot(v,xlab="",ylab="Density",main="Residual variance (CL)",ylim=Ylim,col=2)
  points(w,type="l")
  
  image(abs(cor(cbind(G.sample[,2],R.sample[,2],Pi.sample))),col=paste("gray",seq(0,100,by=10),sep=""),main="Cor. of mcmc samples")
  
  plot(c(1,10),c(1,10),col="gray100",axes=F,xlab="",ylab="")
  legend(1,10,legend=c(paste("WAIC (SEM):",round(WAIC,1)),paste("WAIC (RM)   :",round(WAIC.rm,1))),cex=1)
  dev.off()
}
for(d in Environment){PlotResults(d,"mta05")}
#results when non-informative prior was used for the residual covariance
for(d in Environment){PlotResults(d,"mta05","nir")}

#Plot for the figures in the paper (Figs.1 and 3)####################################################################
CreateFigs<-function(Env,Program,Nr=1,S=1001:11000,B.CL.start=12){
  
  if(Env=="T2008"){
    Earlyheading.I <- c(1:6,75)
    Earlyheading.L <- c(76,103)  	
  }else{
    Earlyheading.I <- c(1:6,77)
    Earlyheading.L <- c(78,105)  		
  }
  Earlyheading<-c(1:6,77,78,105)  
  Earlyheading.names<-c("Kirara397","Hoshinoyume","Yukihikari","Hayamasari","Hatsushizuku","Yukara","Eiko","Akage","Bozu")
  Majorgenes<-scan("MajorGeneNames.txt",what="character")
  
  Y<-scan(paste(Env,"_DHCL.txt",sep=""))
  N<-length(Y)/2
  
  #read samples (NSE)
  Pi.sample<-as.matrix(read.table(paste(Program,"_",Env,"_DHCL_se_Pi1.txt",sep="")))
  Basis<-as.matrix(read.table(paste(Program,"_",Env,"_DHCL_se_Basis1.txt",sep="")))
  Lambda.sample<-Pi.sample%*%Basis
  U.sample<-as.matrix(read.table(paste(Program,"_",Env,"_DHCL_se_U.txt",sep="")))
  U.DH.mean<-apply(U.sample[S,1:(ncol(U.sample)/2)],2,mean)
  U.DH.sd<-apply(U.sample[S,1:(ncol(U.sample)/2)],2,sd)  
  U.CL.mean<-apply(U.sample[S,(ncol(U.sample)/2+1):ncol(U.sample)],2,mean)
  U.CL.sd<-apply(U.sample[S,(ncol(U.sample)/2+1):ncol(U.sample)],2,sd)
  B.sample<-as.matrix(read.table(paste(Program,"_",Env,"_DHCL_se_B.txt",sep="")))
  B.DH.mean<-apply(B.sample[S,2:(B.CL.start-2)],2,mean)
  B.DH.sd<-apply(B.sample[S,2:(B.CL.start-2)],2,sd)
  B.DH.sig<-apply(B.sample[S,2:(B.CL.start-2)],2,quantile,probs=0.975)*apply(B.sample[S,2:(B.CL.start-2)],2,quantile,probs=0.025)
  B.DH.sig[B.DH.sig>0]<-1
  B.DH.sig[B.DH.sig<=0]<-0
  B.CL.mean<-apply(B.sample[S,B.CL.start:ncol(B.sample)],2,mean)
  B.CL.sd<-apply(B.sample[S,B.CL.start:ncol(B.sample)],2,sd)
  B.CL.sig<-apply(B.sample[S,B.CL.start:ncol(B.sample)],2,quantile,probs=0.975)*apply(B.sample[S,B.CL.start:ncol(B.sample)],2,quantile,probs=0.025)
  B.CL.sig[B.CL.sig>0]<-1
  B.CL.sig[B.CL.sig<=0]<-0
  
  #read samples (OLM)
  U.sample.rm<-as.matrix(read.table(paste(Program,"_",Env,"_DHCL_lm_U.txt",sep="")))
  U.DH.mean.rm<-apply(U.sample.rm[S,1:(ncol(U.sample)/2)],2,mean)
  U.DH.sd.rm<-apply(U.sample.rm[S,1:(ncol(U.sample)/2)],2,sd)  
  U.CL.mean.rm<-apply(U.sample.rm[S,(ncol(U.sample)/2+1):ncol(U.sample)],2,mean)
  U.CL.sd.rm<-apply(U.sample.rm[S,(ncol(U.sample)/2+1):ncol(U.sample)],2,sd)
  B.sample.rm<-as.matrix(read.table(paste(Program,"_",Env,"_DHCL_lm_B.txt",sep="")))
  B.DH.mean.rm<-apply(B.sample.rm[S,2:(B.CL.start-2)],2,mean)
  B.DH.sd.rm<-apply(B.sample.rm[S,2:(B.CL.start-2)],2,sd)
  B.DH.sig.rm<-apply(B.sample.rm[S,2:(B.CL.start-2)],2,quantile,probs=0.975)*apply(B.sample.rm[S,2:(B.CL.start-2)],2,quantile,probs=0.025)
  B.DH.sig.rm[B.DH.sig.rm>0]<-1
  B.DH.sig.rm[B.DH.sig.rm<=0]<-0  
  B.CL.mean.rm<-apply(B.sample.rm[S,B.CL.start:ncol(B.sample.rm)],2,mean)
  B.CL.sd.rm<-apply(B.sample.rm[S,B.CL.start:ncol(B.sample.rm)],2,sd)
  B.CL.sig.rm<-apply(B.sample.rm[S,B.CL.start:ncol(B.sample.rm)],2,quantile,probs=0.975)*apply(B.sample.rm[S,B.CL.start:ncol(B.sample.rm)],2,quantile,probs=0.025)
  B.CL.sig.rm[B.CL.sig.rm>0]<-1
  B.CL.sig.rm[B.CL.sig.rm<=0]<-0
  
  #plot
  Yorder<-order(Y[1:(N*Nr)])
  #Trajectory of lambda and phenotypic values
  tiff(paste("./Figures/",Env,".trajectory.tif",sep=""),unit="cm",width=8,height=4,res=600)
  layout(matrix(c(1,2),nr=1))
  par(mar=c(1.2,1.1,0.5,0.1))
  par(mgp=c(0.4,0.3,0))
  
  Pos<-matrix(1:(N*Nr),nr=Nr)
  Xlim<-range(Y[1:(N*Nr)])
  Ylim<-range(Y[(1+N*Nr):(2*N*Nr)])
  plot(Y[as.vector(Pos[,-c(Earlyheading.I,Earlyheading.L)])],Y[(N*Nr)+as.vector(Pos[,-c(Earlyheading.I,Earlyheading.L)])],
       xlab="",ylab="",main="",ylim=Ylim,xlim=Xlim,cex=0.7,cex.axis=0.6,tcl=-0.2)
  points(Y[as.vector(Pos[,Earlyheading.I])],Y[(N*Nr)+as.vector(Pos[,Earlyheading.I])],pch=2,cex=0.7)
  points(Y[as.vector(Pos[,Earlyheading.L])],Y[(N*Nr)+as.vector(Pos[,Earlyheading.L])],pch=19,cex=0.7)
  
  Lambda.mean<-apply(Lambda.sample[S,],2,mean)
  Lambda.0.975<-apply(Lambda.sample[S,],2,quantile,probs=0.975)
  Lambda.0.025<-apply(Lambda.sample[S,],2,quantile,probs=0.025)
  #Ylim<-range(c(Lambda.0.975,Lambda.mean,Lambda.0.025,0))
  Ylim<-c(-2.2,2.5)
  plot(Y[1:(N*Nr)][Yorder],Lambda.mean[Yorder],type="l",xlab="",ylab="",main="",col=2,ylim=Ylim,lwd=1.5,cex.axis=0.6,tcl=-0.2)
  points(Y[1:(N*Nr)][Yorder],Lambda.0.975[Yorder],type="l",lty=2,col=2,lwd=1.5)
  points(Y[1:(N*Nr)][Yorder],Lambda.0.025[Yorder],type="l",lty=2,col=2,lwd=1.5)
  abline(h=0,lty=3)
  dev.off()
  
  #Polygenic effects
  tiff(paste("./Figures/",Env,".poly.Hokkaido.tif",sep=""),unit="cm",width=8,height=6,res=600)
  layout(matrix(c(rep(1,10),rep(2,10)),nr=1))
  par(mar=c(5,2,0.5,0.2))  
  par(mgp=c(0.5,0.7,0))
  
  Ylim<-c(min(c(U.DH.mean[Earlyheading],U.DH.mean.rm[Earlyheading],
                U.DH.mean[Earlyheading],U.DH.mean.rm[Earlyheading])),0)
  v<-barplot(rbind(U.DH.mean[Earlyheading],U.DH.mean.rm[Earlyheading]),beside=T,col=c(2,8),  
             xlab="",ylab="",main="",border=c(2,1),
             names.arg=Earlyheading.names,las=2,ylim=Ylim,cex.names=0.8)
  
  Ylim<-range(c(U.CL.mean[Earlyheading],U.CL.mean.rm[Earlyheading],
                U.CL.mean[Earlyheading],U.CL.mean.rm[Earlyheading]))
  v<-barplot(rbind(U.CL.mean[Earlyheading],U.CL.mean.rm[Earlyheading]),beside=T,col=c(2,8),  
             xlab="",ylab="",main="",border=c(2,1),
             names.arg=Earlyheading.names,las=2,ylim=Ylim,cex.names=0.8)
  dev.off()
  
  
  #Major gene effects
  tiff(paste("./Figures/",Env,".majorgene.tif",sep=""),unit="cm",width=8,height=6,res=600)
  layout(matrix(c(rep(1,8),rep(2,12)),nr=1))
  par(mar=c(5,2,0.5,0.2))  
  par(mgp=c(0.5,0.7,0))
  
  Ylim<-range(c(B.DH.mean+B.DH.sd,B.DH.mean.rm+B.DH.sd.rm,
                B.DH.mean-B.DH.sd,B.DH.mean.rm-B.DH.sd.rm))
  v<-barplot(rbind(B.DH.mean,B.DH.mean.rm),xlab="",ylab="",main="",
             col=as.vector(rbind(B.DH.sig*2,B.DH.sig.rm*8)),beside=T,
             border=c(2,1),names.arg=Majorgenes[-c(1:3)],las=2,ylim=Ylim,cex.names=0.8)
  arrows(as.vector(v),
         as.vector(rbind(B.DH.mean,B.DH.mean.rm)),
         as.vector(v),
         as.vector(rbind(B.DH.mean,B.DH.mean.rm))+as.vector(rbind(B.DH.sd,B.DH.sd.rm)),
         angle=90,length=0.015)
  arrows(as.vector(v),
         as.vector(rbind(B.DH.mean,B.DH.mean.rm)),
         as.vector(v),
         as.vector(rbind(B.DH.mean,B.DH.mean.rm))-as.vector(rbind(B.DH.sd,B.DH.sd.rm)),
         angle=90,length=0.015)
  
  Ylim<-range(c(B.CL.mean+B.CL.sd,B.CL.mean.rm+B.CL.sd.rm,
                B.CL.mean-B.CL.sd,B.CL.mean.rm-B.CL.sd.rm))
  v<-barplot(rbind(B.CL.mean,B.CL.mean.rm),xlab="",ylab="",main="",
             col=as.vector(rbind(B.CL.sig*2,B.CL.sig.rm*8)),beside=T,
             border=c(2,1),names.arg=Majorgenes,las=2,ylim=Ylim,cex.names=0.8)
  arrows(as.vector(v),
         as.vector(rbind(B.CL.mean,B.CL.mean.rm)),
         as.vector(v),
         as.vector(rbind(B.CL.mean,B.CL.mean.rm))+as.vector(rbind(B.CL.sd,B.CL.sd.rm)),
         angle=90,length=0.015)
  arrows(as.vector(v),
         as.vector(rbind(B.CL.mean,B.CL.mean.rm)),
         as.vector(v),
         as.vector(rbind(B.CL.mean,B.CL.mean.rm))-as.vector(rbind(B.CL.sd,B.CL.sd.rm)),
         angle=90,length=0.015) 
  
  dev.off()
}
for(d in Environment){CreateFigs(d,"mta05")}

#summarize the results (Table 3)#############################################################################################
SummarizeResults<-function(Env,Program,Nr=1,S=1001:11000,B.CL.start=12){
  
  #read samples
  Pi.sample<-as.matrix(read.table(paste(Program,"_",Env,"_DHCL_se_Pi1.txt",sep="")))
  WAIC<-as.numeric(scan(paste(Program,"_",Env,"_DHCL_se_Criterion.txt",sep=""),skip=1,nlines=1,what="character",quiet=T)[3])
  G.sample<-as.matrix(read.table(paste(Program,"_",Env,"_DHCL_se_G.txt",sep="")))
  R.sample<-as.matrix(read.table(paste(Program,"_",Env,"_DHCL_se_R.txt",sep="")))
  
  #read samples (rm)
  G.sample.rm<-as.matrix(read.table(paste(Program,"_",Env,"_DHCL_lm_G.txt",sep="")))
  R.sample.rm<-as.matrix(read.table(paste(Program,"_",Env,"_DHCL_lm_R.txt",sep="")))
  WAIC.rm<-as.numeric(scan(paste(Program,"_",Env,"_DHCL_lm_Criterion.txt",sep=""),skip=1,nlines=1,what="character",quiet=T)[3])
  
  #Summary
  c(mean(G.sample[S,1]),sd(G.sample[S,1]),
    mean(G.sample[S,2]),sd(G.sample[S,2]),
    mean(G.sample[S,4]),sd(G.sample[S,4]),
    mean(R.sample[S,1]),sd(R.sample[S,1]),
    mean(R.sample[S,2]),sd(R.sample[S,2]),
    mean(R.sample[S,4]),sd(R.sample[S,4]),
    mean(G.sample.rm[S,1]),sd(G.sample.rm[S,1]),
    mean(G.sample.rm[S,2]),sd(G.sample.rm[S,2]),
    mean(G.sample.rm[S,4]),sd(G.sample.rm[S,4]),
    mean(R.sample.rm[S,1]),sd(R.sample.rm[S,1]),
    mean(R.sample.rm[S,2]),sd(R.sample.rm[S,2]),
    mean(R.sample.rm[S,4]),sd(R.sample.rm[S,4]),    
    WAIC-WAIC.rm,
    as.vector(cor(cbind(G.sample[,2],R.sample[,2],Pi.sample))[-c(1,2),c(1,2)]))
}
SummaryReal<-NULL
for(d in Environment){
  SummaryReal<-rbind(SummaryReal,SummarizeResults(d,"mta05"))
}
SummaryReal<-data.frame(Env=Environment,SummaryReal)
colnames(SummaryReal)<-c("Env",paste(rep(rep(c("varG.DH","covG","varG.CL","varR.DH","covR","varR.CL"),each=2),2),rep(c("mean","sd"),12),rep(c("se","lm"),each=12),sep="."),
                          "deltaWAIC",paste("CorWithG",0:7,sep=""),paste("CorWithR",0:7,sep=""))
write.table(SummaryReal,"SummaryReal.txt",quote=F)

#Summarize the results when the non-informative prior was used for the residual covariance
SummarizeResults.nir<-function(Env,Program,Nr=1,S=1001:11000,B.CL.start=12){
  
  #read samples
  Pi.sample<-as.matrix(read.table(paste(Program,"_",Env,"_DHCL_se_nir_Pi1.txt",sep="")))
  WAIC<-as.numeric(scan(paste(Program,"_",Env,"_DHCL_se_nir_Criterion.txt",sep=""),skip=1,nlines=1,what="character",quiet=T)[3])
  G.sample<-as.matrix(read.table(paste(Program,"_",Env,"_DHCL_se_nir_G.txt",sep="")))
  R.sample<-as.matrix(read.table(paste(Program,"_",Env,"_DHCL_se_nir_R.txt",sep="")))

  #Summary
  c(mean(G.sample[S,1]),sd(G.sample[S,1]),
    mean(G.sample[S,2]),sd(G.sample[S,2]),
    mean(G.sample[S,4]),sd(G.sample[S,4]),
    mean(R.sample[S,1]),sd(R.sample[S,1]),
    mean(R.sample[S,2]),sd(R.sample[S,2]),
    mean(R.sample[S,4]),sd(R.sample[S,4]),
    WAIC,
    as.vector(cor(cbind(G.sample[,2],R.sample[,2],Pi.sample))[-c(1,2),c(1,2)]))
}
SummaryReal.nir<-NULL
for(d in Environment[1:4]){
  SummaryReal.nir<-rbind(SummaryReal.nir,SummarizeResults.nir(d,"mta05"))
}
SummaryReal.nir<-data.frame(Env=Environment[1:4],SummaryReal.nir)
colnames(SummaryReal.nir)<-c("Env",paste(rep(c("varG.DH","covG","varG.CL","varR.DH","covR","varR.CL"),each=2),rep(c("mean","sd"),6),sep="."),
                         "WAIC",paste("CorWithG",0:7,sep=""),paste("CorWithR",0:7,sep=""))
write.table(SummaryReal.nir,"SummaryReal.nir.txt",quote=F)


#Prediction when modifying the heading date gene genotypes (in the Discussion)#########################################
#Kirara397 at FRERC
#Ghd7_SNP is assumed to be modified
MajorGenes<-as.matrix(read.table("MajorGenes_N110_Nr1_10_13.txt"))[1,]
MajorGeneEffects<-apply(as.matrix(read.table("mta05_K2009_DHCL_se_B.txt"))[1001:11000,],2,mean)
PolygenicEffects<-apply(as.matrix(read.table("mta05_K2009_DHCL_se_U.txt"))[1001:11000,],2,mean)[c(1,111)]
ModifiedGeno<-MajorGenes
ModifiedGeno[c(9,22)]<-0 #Ghd7_SNP
#DH after modification
DH.modified<-sum(MajorGeneEffects[1:10]*ModifiedGeno[1:10])+PolygenicEffects[1]#1.577009 
#Fitted DH value
DH.expected<-sum(MajorGeneEffects[1:10]*MajorGenes[1:10])+PolygenicEffects[1]#-34.10604
#Calculate the nonlinear influence of DH
Weights<-apply(as.matrix(read.table("mta05_K2009_DHCL_se_Pi1.txt"))[1001:11000,],2,mean)
deBoorCox2<-function(x,i,n,knotvec){
  #Calculate the basis fucntion
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
K2009Y<-scan("K2009_DHCL.txt")
Range<-range(K2009Y[1:110])
Nsp<-8;Ord<-4
Knot<-numeric(Nsp+Ord)
a<-(Range[2]-Range[1])/(Nsp-Ord)
for(i in 1:Ord){Knot[i]<-Range[1]-0.5*a; Knot[Nsp+Ord-i+1]<-Range[2]+0.5*a}
for(i in (Ord+1):Nsp)Knot[i]<-Knot[i-1]+a
BasisFunction<-numeric(Nsp)
for(i in 1:Nsp)BasisFunction[i]<-deBoorCox2(DH.modified,i,Ord,Knot)
#CL after modification
CL.modified<-sum(BasisFunction*Weights)*DH.modified+sum(MajorGeneEffects[11:23]*ModifiedGeno[11:23])+PolygenicEffects[2]#9.867265
#Observed DH and CL
K2009Y[c(1,111)]#-33.49091 -25.00000
#Influence of DH at the observed DH point
sum(as.matrix(read.table("mta05_K2009_DHCL_se_Basis1.txt"))[,1]*Weights)#0.602339

#Results of OLM
MajorGeneEffects.OLM<-apply(as.matrix(read.table("mta05_K2009_DHCL_lm_B.txt"))[1001:11000,],2,mean)
PolygenicEffects.OLM<-apply(as.matrix(read.table("mta05_K2009_DHCL_lm_U.txt"))[1001:11000,],2,mean)[c(1,111)]
CL.modified.OLM<-sum(MajorGeneEffects.OLM[11:23]*ModifiedGeno[11:23])+PolygenicEffects.OLM[2]#-1.163651









