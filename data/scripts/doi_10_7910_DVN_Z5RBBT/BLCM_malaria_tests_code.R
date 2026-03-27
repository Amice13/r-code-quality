#BAYESIAN LCM: EVALUATION OF RAPID DIAGNOSTIC TEST (RDT), MICROSCOPY (LM) & PCR FOR P. FALCIPARUM

rm(list=ls())

setwd("") # Specify working directory

if(!R.Version()$arch=="i386"){message("Change R platform to 32 bit");q()}

#Need to install OPENBugs version 3.2.2 first
#browseURL("https://www.dropbox.com/sh/o3pnkhehxopmd2a/AABykGy4ar98tB44UMlOi6YFa?dl=0")

if(!"BRugs" %in% row.names(installed.packages())){install.packages("BRugs")}; require("BRugs")

#################################################################################################################################

#Data cleaning and merging

Ngere.all <- read.csv(file="Ngerenya_tests_data.csv",header=T)

str(Ngere.all)

#########################################################################################################################

#BLCM 3 TESTS MODEL - WITH COVARIATES
#############################################################################

malar.3tests <- function(){ #model 1
  
  #Set priors for tests
  #Priors - let data speak for self (uninformative priors)
  #Se,Sp[1] (rdt in 1st level of covariate)
  #Se,Sp[2] (microscopy in 1st level of covariate) 
  #se,sp[3] (pcr in 1st level of covariate)
  #Se,Sp[4] (rdt in 2nd level of covariate)
  #Se,Sp[5] (microscopy in 2nd level of covariate)
  #se,sp[6] (pcr in 2nd level of covariate)
  
  for(i in 1:6){ #Test 1=rdt; Test 2=microscopy; Test 3=pcr 
    
    se[i] ~ dbeta(1,1)
    sp[i] ~ dbeta(1,1)
    
  }
  
  for(i in 1:1){ 
    
    p[i] ~ dbeta(1,1) #1st covariate level
    
    pop[i,1:8] ~ dmulti(par[i,1:8],n[i])
    par[i,1] <- se[1]*se[2]*se[3]*p[i] + (1-sp[1])*(1-sp[2])*(1-sp[3])*(1-p[i])
    par[i,2] <- se[1]*se[2]*(1-se[3])*p[i] + (1-sp[1])*(1-sp[2])*sp[3]*(1-p[i])
    par[i,3] <- se[1]*(1-se[2])*se[3]*p[i] + (1-sp[1])*sp[2]*(1-sp[3])*(1-p[i])
    par[i,4] <- (1-se[1])*se[2]*se[3]*p[i] + sp[1]*(1-sp[2])*(1-sp[3])*(1-p[i])
    par[i,5] <- se[1]*(1-se[2])*(1-se[3])*p[i] + (1-sp[1])*sp[2]*sp[3]*(1-p[i])
    par[i,6] <- (1-se[1])*se[2]*(1-se[3])*p[i] + sp[1]*(1-sp[2])*sp[3]*(1-p[i])
    par[i,7] <- (1-se[1])*(1-se[2])*se[3]*p[i] + sp[1]*sp[2]*(1-sp[3])*(1-p[i])
    par[i,8] <- (1-se[1])*(1-se[2])*(1-se[3])*p[i] + sp[1]*sp[2]*sp[3]*(1-p[i])
    
    n[i] <- sum(pop[i,1:8])
    
  }
  
  for(i in 2:2){ 
    
    p[i] ~ dbeta(1,1) #2nd covariate level
    
    pop[i,1:8] ~ dmulti(par[i,1:8],n[i])
    par[i,1] <- se[4]*se[5]*se[6]*p[i] + (1-sp[4])*(1-sp[5])*(1-sp[6])*(1-p[i])
    par[i,2] <- se[4]*se[5]*(1-se[6])*p[i] + (1-sp[4])*(1-sp[5])*sp[6]*(1-p[i])
    par[i,3] <- se[4]*(1-se[5])*se[6]*p[i] + (1-sp[4])*sp[5]*(1-sp[6])*(1-p[i])
    par[i,4] <- (1-se[4])*se[5]*se[6]*p[i] + sp[4]*(1-sp[5])*(1-sp[6])*(1-p[i])
    par[i,5] <- se[4]*(1-se[5])*(1-se[6])*p[i] + (1-sp[4])*sp[5]*sp[6]*(1-p[i])
    par[i,6] <- (1-se[4])*se[5]*(1-se[6])*p[i] + sp[4]*(1-sp[5])*sp[6]*(1-p[i])
    par[i,7] <- (1-se[4])*(1-se[5])*se[6]*p[i] + sp[4]*sp[5]*(1-sp[6])*(1-p[i])
    par[i,8] <- (1-se[4])*(1-se[5])*(1-se[6])*p[i] + sp[4]*sp[5]*sp[6]*(1-p[i])
    
    n[i] <- sum(pop[i,1:8])
    
  }
  
  for(i in 1:3){
    
    ppv[i] <- p[1]*se[i]/(p[1]*se[i] + (1-p[1])*(1-sp[i]))
    
    npv[i] <- (1-p[1])*sp[i]/(p[1]*(1-se[i]) + (1-p[1])*sp[i])
    
  }
  
  for(i in 4:6){
    
    ppv[i] <- p[2]*se[i]/(p[2]*se[i] + (1-p[2])*(1-sp[i]))
    
    npv[i] <- (1-p[2])*sp[i]/(p[2]*(1-se[i]) + (1-p[2])*sp[i])
    
  }
  
  p.cov.se[1] <- step(se[1] - se[4]) #Testing for differences betwn test Se & Sp by covariate using Bayesian P value
  p.cov.se[2] <- step(se[2] - se[5])
  p.cov.se[3] <- step(se[3] - se[6])
  p.cov.sp[1] <- step(sp[1] - sp[4])
  p.cov.sp[2] <- step(sp[2] - sp[5])
  p.cov.sp[3] <- step(sp[3] - sp[6])
  
  
}

#############################################################

#BLCM 3 TESTS MODEL - WITHOUT COVARIATES
#################################################################

model.simp <- function(){
  
  #priors
  for(i in 1:3){ #Test 1=rdt; Test 2=microscopy; Test 3=pcr  
    
    se[i] ~ dbeta(1,1)
    sp[i] ~ dbeta(1,1)
    
  }
  
  p ~ dbeta(1,1) #1st covariate level
  
  pop[1,1:8] ~ dmulti(par[1,1:8],n)
  par[1,1] <- se[1]*se[2]*se[3]*p + (1-sp[1])*(1-sp[2])*(1-sp[3])*(1-p)
  par[1,2] <- se[1]*se[2]*(1-se[3])*p + (1-sp[1])*(1-sp[2])*sp[3]*(1-p)
  par[1,3] <- se[1]*(1-se[2])*se[3]*p + (1-sp[1])*sp[2]*(1-sp[3])*(1-p)
  par[1,4] <- (1-se[1])*se[2]*se[3]*p + sp[1]*(1-sp[2])*(1-sp[3])*(1-p)
  par[1,5] <- se[1]*(1-se[2])*(1-se[3])*p + (1-sp[1])*sp[2]*sp[3]*(1-p)
  par[1,6] <- (1-se[1])*se[2]*(1-se[3])*p + sp[1]*(1-sp[2])*sp[3]*(1-p)
  par[1,7] <- (1-se[1])*(1-se[2])*se[3]*p + sp[1]*sp[2]*(1-sp[3])*(1-p)
  par[1,8] <- (1-se[1])*(1-se[2])*(1-se[3])*p + sp[1]*sp[2]*sp[3]*(1-p)
  
  n <- sum(pop[1,1:8])
  
  
  for(i in 1:3){
    
    ppv[i] <- p*se[i]/(p*se[i] + (1-p)*(1-sp[i]))
    npv[i] <- (1-p)*sp[i]/(p*(1-se[i]) + (1-p)*sp[i])
    
  }
  
  #Testing for differences in Ses and Sps
  
  p.se[1] <- step(se[1] - se[2])
  p.se[2] <- step(se[1] - se[3])
  p.se[3] <- step(se[2] - se[3])
  
  p.sp[1] <- step(sp[1] - sp[2])
  p.sp[2] <- step(sp[1] - sp[3])
  p.sp[3] <- step(sp[2] - sp[3])
  
  #Serial interpretation of RDT & microscopy
  serial.se <- se[1]*se[2]
  serial.sp <- 1 - ((1-sp[1])*(1-sp[2]))
  
  serial.npv <- (1-p)*serial.sp/(p*(1-serial.se) + (1-p)*serial.sp)
  serial.ppv <- p*serial.se/(p*serial.se + (1-p)*(1-serial.sp))
  
  
}

##################################################################################################################################

#Data 1 - with covariates
#################################

covars <- c("age.cat","sex","season"); cov.malar.res <- mat.dat <- list(); dic.vals <- c()

for(cov in 1:length(covars)){
  
  print(paste("Currently running model for:",covars[cov],sep=" "))
  
  cov.mat <- matrix(NA,nrow=2,ncol=8)
  
  for (m in 1:2){
    
    cov.mat[m,1] <- nrow(Ngere.all[Ngere.all$rdt_result=="positive" & Ngere.all$pf_mcl.cat=="positive" & Ngere.all$pcr_result.cat=="positive" & Ngere.all[,covars[cov]]==levels(Ngere.all[,covars[cov]])[m],])
    cov.mat[m,2] <- nrow(Ngere.all[Ngere.all$rdt_result=="positive" & Ngere.all$pf_mcl.cat=="positive" & Ngere.all$pcr_result.cat=="negative" & Ngere.all[,covars[cov]]==levels(Ngere.all[,covars[cov]])[m],])
    cov.mat[m,3] <- nrow(Ngere.all[Ngere.all$rdt_result=="positive" & Ngere.all$pf_mcl.cat=="negative" & Ngere.all$pcr_result.cat=="positive" & Ngere.all[,covars[cov]]==levels(Ngere.all[,covars[cov]])[m],])
    cov.mat[m,4] <- nrow(Ngere.all[Ngere.all$rdt_result=="negative" & Ngere.all$pf_mcl.cat=="positive" & Ngere.all$pcr_result.cat=="positive" & Ngere.all[,covars[cov]]==levels(Ngere.all[,covars[cov]])[m],])
    cov.mat[m,5] <- nrow(Ngere.all[Ngere.all$rdt_result=="positive" & Ngere.all$pf_mcl.cat=="negative" & Ngere.all$pcr_result.cat=="negative" & Ngere.all[,covars[cov]]==levels(Ngere.all[,covars[cov]])[m],])
    cov.mat[m,6] <- nrow(Ngere.all[Ngere.all$rdt_result=="negative" & Ngere.all$pf_mcl.cat=="positive" & Ngere.all$pcr_result.cat=="negative" & Ngere.all[,covars[cov]]==levels(Ngere.all[,covars[cov]])[m],])
    cov.mat[m,7] <- nrow(Ngere.all[Ngere.all$rdt_result=="negative" & Ngere.all$pf_mcl.cat=="negative" & Ngere.all$pcr_result.cat=="positive" & Ngere.all[,covars[cov]]==levels(Ngere.all[,covars[cov]])[m],])
    cov.mat[m,8] <- nrow(Ngere.all[Ngere.all$rdt_result=="negative" & Ngere.all$pf_mcl.cat=="negative" & Ngere.all$pcr_result.cat=="negative" & Ngere.all[,covars[cov]]==levels(Ngere.all[,covars[cov]])[m],])
    
    
  }
  
  cov.mat.list <- list(cov.mat); names(cov.mat.list) <- "pop"; mat.dat[[cov]] <- cov.mat
  
  ####################################################################################################################################################################################
  #Convert all to BUGS format
  
  #write model to a file
  writeModel(malar.3tests,'malar.3tests.txt')
  
  #Bugs data
  bugsData(cov.mat.list,fileName='malar.dat.txt')
  
  #make 2 initial values chains 
  bugsInits(inits=list(list(se=rep(0.80,times=6),sp=rep(0.80,times=6),p=rep(0.15,times=2))),numChains=1,'CID.Init1.txt')
  bugsInits(inits=list(list(se=rep(0.90,times=6),sp=rep(0.90,times=6),p=rep(0.20,times=2))),numChains=1,'CID.Init2.txt')
  
  #now check, load data, compile etc.
  modelCheck('malar.3tests.txt') #check model file.
  modelData('malar.dat.txt') #read data file
  modelCompile(numChains=2) #compile model with 2 chains
  modelInits('CID.Init1.txt',1) #read init data file
  modelInits('CID.Init2.txt',2) #read init data file
  #modelGenInits() #generate the missing initial values
  
  modelUpdate(20000) #burn in
  
  samplesSet(c('se','sp','p','p.cov.se','p.cov.sp','ppv','npv')) #parameters to monitor
  
  modelUpdate(50000) #more iterations 
  
  #SOME DIAGNOSTICS FIRST
  #Check convergence (Trace plots) - should ideally check all
  #samplesHistory('se',mfrow=c(1,1)) # plot the chain,
  #samplesHistory('sp',mfrow=c(1,1)) # plot the chain,
  #samplesHistory('p',mfrow=c(1,1))
  
  #Plot the Gelman-Rubin diagnostic statistics - ratio should be close to 1
  #samplesBgr('se',mfrow=c(1,1))  
  #samplesBgr('sp',mfrow=c(1,1))
  #samplesBgr('p',mfrow=c(1,1))
  
  #Density plots
  #samplesDensity('se',mfrow=c(1,1)) 
  #samplesDensity('sp',mfrow=c(1,1))
  #samplesDensity('p',mfrow=c(1,1))
  
  if(cov==1){cov.malar.res[[cov]] <- samplesStats('*')} # age.cat results
  if(cov==2){cov.malar.res[[cov]] <- samplesStats('*')} # sex results
  if(cov==3){cov.malar.res[[cov]] <- samplesStats('*')} # season results
  
  dicSet(); modelUpdate(50000) #monitor DIC and do further model updates
  
  dic.vals[cov] <- dicStats()[1,3] #show DIC value
  
}

cov.malar.res
dic.vals

#################################################################################################################################################################
#Data 2 - without covariates
#########################################

cov.mat <- matrix(NA,nrow=1,ncol=8)

cov.mat[1,1] <- nrow(Ngere.all[Ngere.all$rdt_result=="positive" & Ngere.all$pf_mcl.cat=="positive" & Ngere.all$pcr_result.cat=="positive",])
cov.mat[1,2] <- nrow(Ngere.all[Ngere.all$rdt_result=="positive" & Ngere.all$pf_mcl.cat=="positive" & Ngere.all$pcr_result.cat=="negative",])
cov.mat[1,3] <- nrow(Ngere.all[Ngere.all$rdt_result=="positive" & Ngere.all$pf_mcl.cat=="negative" & Ngere.all$pcr_result.cat=="positive",])
cov.mat[1,4] <- nrow(Ngere.all[Ngere.all$rdt_result=="negative" & Ngere.all$pf_mcl.cat=="positive" & Ngere.all$pcr_result.cat=="positive",])
cov.mat[1,5] <- nrow(Ngere.all[Ngere.all$rdt_result=="positive" & Ngere.all$pf_mcl.cat=="negative" & Ngere.all$pcr_result.cat=="negative",])
cov.mat[1,6] <- nrow(Ngere.all[Ngere.all$rdt_result=="negative" & Ngere.all$pf_mcl.cat=="positive" & Ngere.all$pcr_result.cat=="negative",])
cov.mat[1,7] <- nrow(Ngere.all[Ngere.all$rdt_result=="negative" & Ngere.all$pf_mcl.cat=="negative" & Ngere.all$pcr_result.cat=="positive",])
cov.mat[1,8] <- nrow(Ngere.all[Ngere.all$rdt_result=="negative" & Ngere.all$pf_mcl.cat=="negative" & Ngere.all$pcr_result.cat=="negative",])


cov.mat.list <- list(cov.mat); names(cov.mat.list) <- "pop"

####################################################################################################################################################################################
#Convert all to BUGS format

#write model to a file
writeModel(model.simp,'model.simp.txt')

#Bugs data
bugsData(cov.mat.list,fileName='malar.dat.txt')

#make 2 initial values chains 
bugsInits(inits=list(list(se=rep(0.80,times=3),sp=rep(0.80,times=3),p=0.15)),numChains=1,'CID.Init1.txt')
bugsInits(inits=list(list(se=rep(0.90,times=3),sp=rep(0.90,times=3),p=0.20)),numChains=1,'CID.Init2.txt')

#now check, load data, compile etc.
modelCheck('model.simp.txt') #check model file.
modelData('malar.dat.txt') #read data file
modelCompile(numChains=2) #compile model with 2 chains
modelInits('CID.Init1.txt',1) #read init data file
modelInits('CID.Init2.txt',2) #read init data file
#modelGenInits() #generate the missing initial values

modelUpdate(20000) #burn in

samplesSet(c('se','sp','p','ppv','npv','p.se','p.sp','serial.se','serial.sp','serial.npv','serial.ppv')) #parameters to monitor

modelUpdate(50000) #more iterations 

#SOME DIAGNOSTICS FIRST
#Check convergence (Trace plots) - should ideally check all
#samplesHistory('se',mfrow=c(1,1)) # plot the chain,
#samplesHistory('sp',mfrow=c(1,1)) # plot the chain,
#samplesHistory('p',mfrow=c(1,1))

#Plot the Gelman-Rubin diagnostic statistics - ratio should be close to 1
#samplesBgr('se',mfrow=c(1,1))  
#samplesBgr('sp',mfrow=c(1,1))
#samplesBgr('p',mfrow=c(1,1))

#Density plots
#samplesDensity('se',mfrow=c(1,1)) 
#samplesDensity('sp',mfrow=c(1,1))
#samplesDensity('p',mfrow=c(1,1))

(cov.malar.res.simp <- samplesStats('*')) # age.cat results

dicSet(); modelUpdate(50000) #monitor DIC and do further model updates

(dic.simp <- dicStats()[1,3]) #show DIC value

#########################################################################################################################################################
#END
