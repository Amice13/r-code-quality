# -----------------------------------------------------------------------
#  Author: Aleks Ksiazkiewicz (adapted from Kees-Jan Kan)
#    Date: 08 16 2019 
#
# Saturated model, Cholesky ACE, and AE models, 
# to estimate genetic and environmental sources of variance 
# --- trivariate
# -----------------------------------------------------------------------

# NOTE: This file requires the following:
##  R version 3.0.3
##  OpenMx version 1.4-3060
##  the psych package

# Australian data should first be cleaned using "Australia data cleaning.do"
# Create a directory inside the working directory called "results" to store output files

## Clear working space
rm(list=ls(all=TRUE))

# Load OpenMx
require(OpenMx)
require(psych)

# Set working directory in lines 26 and 65
setwd("")

# Load helper functions

source("genepihelperfunctions.R")

# -----------------------------------------------------------------------
# PREPARE DATA

Data <- read.table ('AU_cleaned_pairs.csv', header=T, sep=',', na.strings=".")

describe(Data)

# b5op -> crel -> wp
# crel -> b5op -> wp
# b5op -> wp -> crel
# wp -> b5op -> crel

varnames1 <- rbind("combined_b5_op_",
                   "combined_rel_factor_",
                   "combined_b5_op_",
                   "combined_wp_social_")

varnames2 <- rbind("combined_rel_factor_",
                   "combined_b5_op_",
                   "combined_wp_social_",
                   "combined_b5_op_")

varnames3 <- rbind("combined_wp_social_",
                   "combined_wp_social_",
                   "combined_rel_factor_",
                   "combined_rel_factor_")

savenames <- rbind("AU 3var_nonresid cb5op crelfactor cwpsocial.RData",
                   "AU 3var_nonresid crelfactor cb5op cwpsocial.RData",
                   "AU 3var_nonresid cb5op cwpsocial crelfactor.RData",
                   "AU 3var_nonresid cwpsocial cb5op crelfactor.RData")

for(i in 1:4) {
setwd("")
  
# Select variables for analysis -- put them in the Vars vector
Vars     <- c(varnames1[i,1],varnames2[i,1],varnames3[i,1])
nv  		<- length(Vars)	 					# number of variables
ntv		  <- nv*2							      # number of variables * number of twins
selVars	<- paste(Vars,c(rep(1,nv),rep(2,nv)),sep="")	

# Select data for analysis
mzData	<- subset(Data, mz==1, selVars)
dzData	<- subset(Data, mz==2, selVars)

# Store and Print Descriptive Statistics
# -----------------------------------------------------------------------
summary(mzData)
summary(dzData)
(mzMeans 	<- colMeans(mzData,na.rm=TRUE))
(dzMeans 	<- colMeans(dzData,na.rm=TRUE))
(mzCov   	<- cov(mzData,use="complete"))
(dzCov  	<- cov(dzData,use="complete"))
(mzSignSD	<- (sqrt(abs(mzCov))*(mzCov>0))-(sqrt(abs(mzCov))*(mzCov<0)))
(dzSignSD	<- (sqrt(abs(dzCov))*(dzCov>0))-(sqrt(abs(dzCov))*(dzCov<0)))
(mzCor 	  <- cor(mzData,use="complete"))
(dzCor 	  <- cor(dzData,use="complete"))
mzMeansSV <- mzMeans+.1
dzMeansSV <- dzMeans+.1
mzCovSV <- mzCov+.005
dzCovSV <- dzCov+.001

# Raw data in OpenMx format
dataMZ 	  <- mxData(observed = mzData, type = "raw" )
dataDZ 	  <- mxData(observed = dzData, type = "raw" )

# Saturated model with RawData and matrices input

# Labeling
meanLabsMZ <- paste("mMZ",1:(ntv),sep="")
meanLabsDZ <- paste("mDZ",1:(ntv),sep="")

coLabsMZ <- paste("MZ",rev(ntv+1-sequence(1:ntv)),rep(1:ntv,ntv:1),sep="")
coLabsDZ <- paste("DZ",rev(ntv+1-sequence(1:ntv)),rep(1:ntv,ntv:1),sep="")

## Modeling

# Means
MeansMZ     <- mxMatrix(name="ExpMeanMZ", type="Full", nrow=1, ncol=nv*2, 
                        free=TRUE, values=mzMeansSV, labels=meanLabsMZ)
MeansDZ     <- mxMatrix(name="ExpMeanDZ", type="Full", nrow=1, ncol=nv*2, 
                        free=TRUE, values=dzMeansSV, labels=meanLabsDZ)

# Covariances
CovsMZ      <- mxMatrix(name="ExpCovMZ", type="Symm", nrow=nv*2, ncol=nv*2, 
                        free=TRUE, values=mzCovSV, labels=coLabsMZ)
CovsDZ      <- mxMatrix(name="ExpCovDZ", type="Symm", nrow=nv*2, ncol=nv*2, 
                        free=TRUE, values=dzCovSV, labels=coLabsDZ)

# To get standardized results
Imat        <- mxMatrix(name= "I", type="Iden", nrow = nv*2, ncol = nv*2)
iSDmz       <- mxAlgebra(name="iSDmz", expression=solve(sqrt(I*ExpCovMZ)))
iSDdz       <- mxAlgebra(name="iSDdz", expression=solve(sqrt(I*ExpCovDZ)))
CorsMZ      <- mxAlgebra(name="ExpCorMZ", expression=iSDmz%*%ExpCovMZ%*%iSDmz)
CorsDZ      <- mxAlgebra(name="ExpCorDZ", expression=iSDdz%*%ExpCovDZ%*%iSDdz)

# Objectives
ObjectiveMZ <- mxFIMLObjective(covariance="ExpCovMZ", means="ExpMeanMZ", 
                               dimnames=selVars)
ObjectiveDZ <- mxFIMLObjective(covariance="ExpCovDZ", means="ExpMeanDZ", 
                               dimnames=selVars)

# MZ and DZ models
MZmodel <- mxModel("MZ", dataMZ, MeansMZ, CovsMZ, Imat, iSDmz, CorsMZ, 
                   ObjectiveMZ)
DZmodel <- mxModel("DZ", dataDZ, MeansDZ, CovsDZ, Imat, iSDdz, CorsDZ, 
                   ObjectiveDZ)

# General objective
SatObjective<-mxAlgebra(MZ.objective + DZ.objective, name="min2ll")
SatAlgebraObjective<-mxAlgebraObjective("min2ll")     

# Saturated model
SatModel<-mxModel("Saturated Model",MZmodel, DZmodel, 
                  SatObjective, SatAlgebraObjective)

## Fit the saturated model
SatModelFit <- mxRun(SatModel, intervals=FALSE)
summary(SatModelFit)

## Pretty output
SatCovMatrices <- c("MZ.ExpCovMZ", "DZ.ExpCovDZ")
SatCovLabels <- c("MZ.ExpCovMZ", "DZ.ExpCovDZ")

SatCorMatrices <- c("MZ.ExpCorMZ", "DZ.ExpCorDZ")
SatCorLabels <- c("MZ.ExpCorMZ", "DZ.ExpCorDZ")

# formatOutputMatrices(SatModelFit,SatCovMatrices,SatCovLabels,Vars,4)
# formatOutputMatrices(SatModelFit,SatCorMatrices,SatCorLabels,Vars,4)







########### ACE model

## Labeling
aLabs <- paste("a",rev(nv+1-sequence(1:nv)),rep(1:nv,nv:1),sep="")
cLabs <- paste("c",rev(nv+1-sequence(1:nv)),rep(1:nv,nv:1),sep="")
eLabs <- paste("e",rev(nv+1-sequence(1:nv)),rep(1:nv,nv:1),sep="")
mLabs <- paste("mean",1:nv,sep="")

# Matrices a, c, and e to store a, c, and e path coefficients
pathA <- mxMatrix(name = "a", type = "Lower", nrow = nv, ncol = nv, 
                  labels = aLabs)
pathC <- mxMatrix(name = "c", type = "Lower", nrow = nv, ncol = nv, 
                  labels = cLabs)
pathE <- mxMatrix(name = "e", type = "Lower", nrow = nv, ncol = nv, 
                  labels = eLabs)

# Matrices generated to hold A, C, and E computed variance components
covA <- mxAlgebra(name = "A", expression = a %*% t(a))
covC <- mxAlgebra(name = "C", expression = c %*% t(c))
covE <- mxAlgebra(name = "E", expression = e %*% t(e))

# Algebra to compute total variances and standard deviations (diagonal only)
covPh <- mxAlgebra(name = "V", expression = A+C+E)
matI  <- mxMatrix(name= "I", type="Iden", nrow = nv, ncol = nv)
invSD <- mxAlgebra(name ="iSD", expression = solve(sqrt(I*V)))
corPh <- mxAlgebra(name ="rPh", expression = iSD%*%V%*%iSD)

# Algebra for expected mean and variance/covariance matrices in MZ & DZ twins
# Mean structure, algebra M to store expected means
grandMean <- mxMatrix(name="M", type="Full", nrow=1, ncol=nv, 
                      labels=paste("mean",1:nv,sep=""))
expMean <- mxAlgebra(name="expMean", expression=cbind(M,M))

# Algebra for expected variance/covariance matrix in MZ
expCovMZ <- mxAlgebra(name = "expCovMZ", 
                      expression = rbind (cbind(A+C+E, A+C),
                                          cbind(A+C,   A+C+E)))

# Algebra for expected variance/covariance matrix in DZ
expCovDZ <- mxAlgebra(name = "expCovDZ", 
                      expression = rbind (cbind(A+C+E,     0.5%x%A+C),
                                          cbind(0.5%x%A+C, A+C+E))) 

# Objectives for MZ and DZ groups
MZObjective <- mxFIMLObjective(covariance="expCovMZ", means="expMean", 
                               dimnames=selVars)
DZObjective <- mxFIMLObjective(covariance="expCovDZ", means="expMean", 
                               dimnames=selVars)

# Combine groups
pars      <- list(pathA, pathC, pathE, 
                  covA, covC, covE, covPh,
                  matI, invSD, corPh, 
                  grandMean, expMean)

# MZ and DZ models
MZmodel <- mxModel(name = "MZmodel", pars, dataMZ, expCovMZ, MZObjective)
DZmodel <- mxModel(name = "DZmodel", pars, dataDZ, expCovDZ, DZObjective)

# Objective  
min2sumll <- mxAlgebra( expression = MZmodel.objective + DZmodel.objective, 
                        name="min2sumll" )
objective <- mxAlgebraObjective("min2sumll")




# Confidence interval object - trivariate

cimultiply <- mxMatrix(name = "multiply", type = "Lower", nrow = 3, ncol = 3, 
                       values = c(100,100,100,100,100,100), free=FALSE)

# standardized a, c, and e estimates
ciAlgebraaiSD <- mxAlgebra(name = "aiSD", expression = (iSD%*%a))
ciAlgebraciSD <- mxAlgebra(name = "ciSD", expression = (iSD%*%c))
ciAlgebraeiSD <- mxAlgebra(name = "eiSD", expression = (iSD%*%e))

# % of variance accounted for by each a, c, and e path
ciAlgebraa <- mxAlgebra(name = "a100", expression = (iSD%*%a)*(iSD%*%a)*multiply)
ciAlgebrac <- mxAlgebra(name = "c100", expression = (iSD%*%c)*(iSD%*%c)*multiply)
ciAlgebrae <- mxAlgebra(name = "e100", expression = (iSD%*%e)*(iSD%*%e)*multiply)
ciAlgebrasum <- mxAlgebra(name = "sum100", expression = ((iSD%*%a)*(iSD%*%a)*multiply)+((iSD%*%c)*(iSD%*%c)*multiply)+((iSD%*%e)*(iSD%*%e)*multiply))

# % of variance accounted for by all A, C, and E paths for variables 1, 2, and 3
ciAlgebraA1 <- mxAlgebra(name = "A1", expression = 100*(A[1,1]/V[1,1]))
ciAlgebraC1 <- mxAlgebra(name = "C1", expression = 100*(C[1,1]/V[1,1]))
ciAlgebraE1 <- mxAlgebra(name = "E1", expression = 100*(E[1,1]/V[1,1]))

ciAlgebraA2 <- mxAlgebra(name = "A2", expression = 100*(A[2,2]/V[2,2]))
ciAlgebraC2 <- mxAlgebra(name = "C2", expression = 100*(C[2,2]/V[2,2]))
ciAlgebraE2 <- mxAlgebra(name = "E2", expression = 100*(E[2,2]/V[2,2]))

ciAlgebraA3 <- mxAlgebra(name = "A3", expression = 100*(A[3,3]/V[3,3]))
ciAlgebraC3 <- mxAlgebra(name = "C3", expression = 100*(C[3,3]/V[3,3]))
ciAlgebraE3 <- mxAlgebra(name = "E3", expression = 100*(E[3,3]/V[3,3]))

# % of covariance between variables 1 and 2 accounted for by A, C, and E
ciAlgebraA21 <- mxAlgebra(name = "rpA21", expression = 100*(a100[2,1]/sum100[2,1]))
ciAlgebraC21 <- mxAlgebra(name = "rpC21", expression = 100*(c100[2,1]/sum100[2,1]))
ciAlgebraE21 <- mxAlgebra(name = "rpE21", expression = 100*(e100[2,1]/sum100[2,1]))

# % of covariance between variables 1 and 3 accounted for by A, C, and E
ciAlgebraA31 <- mxAlgebra(name = "rpA31", expression = 100*(a100[3,1]/sum100[3,1]))
ciAlgebraC31 <- mxAlgebra(name = "rpC31", expression = 100*(c100[3,1]/sum100[3,1]))
ciAlgebraE31 <- mxAlgebra(name = "rpE31", expression = 100*(e100[3,1]/sum100[3,1]))

# % of covariance between variables 2 and 3 accounted for by A, C, and E
#   partialing out covariance with variable 1
ciAlgebraA32 <- mxAlgebra(name = "rpA32", expression = 100*(a100[3,2]/sum100[3,2]))
ciAlgebraC32 <- mxAlgebra(name = "rpC32", expression = 100*(c100[3,2]/sum100[3,2]))
ciAlgebraE32 <- mxAlgebra(name = "rpE32", expression = 100*(e100[3,2]/sum100[3,2]))

# designate which variables should have confidence intervals calculated
CholACEci <- mxCI(c("aiSD","ciSD","eiSD",
                    "a100","c100","e100",
                    "rPh"), interval=0.95, type="both")
# for a more extensive set of confidence intervals, use the below
# CholACEci <- mxCI(c("a","c","e",
#                     "aiSD","ciSD","eiSD",
#                     "a100","c100","e100",
#                     "A1","C1","E1",
#                     "A2","C2","E2",
#                     "A3","C3","E3",
#                     "rpA","rpC","rpE",
#                     "rpA2","rpC2","rpE2",
#                     "rpA31","rpC31","rpE31",
#                     "rpA32","rpC32","rpE32",
#                     "rPh"), interval=0.95, type="both")


# include confidence interval objects in model
CholACEModel <- mxModel(name = "Chol", pars, MZmodel, DZmodel, min2sumll, 
                      ciAlgebraaiSD, ciAlgebraciSD, ciAlgebraeiSD, 
                       cimultiply,
                       ciAlgebraa, ciAlgebrac, ciAlgebrae,ciAlgebrasum,
                       ciAlgebraA1, ciAlgebraC1, ciAlgebraE1,
                       ciAlgebraA2, ciAlgebraC2, ciAlgebraE2,
                       ciAlgebraA3, ciAlgebraC3, ciAlgebraE3,
                       ciAlgebraA21, ciAlgebraC21, ciAlgebraE21,
                       ciAlgebraA31, ciAlgebraC31, ciAlgebraE31,
                       ciAlgebraA32, ciAlgebraC32, ciAlgebraE32,
                       CholACEci,
                       objective)





# Free and fix parameters, provide starting values, and fit the model
# Select one of these sets of starting values to get it to converge

# Starting Values (Here I use the descriptive statistics of mz1)
# Starting Values (Here I use the descriptive statistics of mz2)
# Starting Values (Here I use the descriptive statistics of dz1)
# Starting Values (Here I use the descriptive statistics of dz2)

ACEgreen <- 1
AEgreen <- 1
CEgreen <- 1
Egreen <- 1
sv <- 1

cholSVoptions <- rbind(mzSignSD[1:nv,1:nv][lower.tri(mzSignSD[1:nv,1:nv],TRUE)],
                       mzSignSD[nv+1:nv,nv+1:nv][lower.tri(mzSignSD[nv+1:nv,nv+1:nv],TRUE)],
                       dzSignSD[1:nv,1:nv][lower.tri(dzSignSD[1:nv,1:nv],TRUE)],
                       dzSignSD[nv+1:nv,nv+1:nv][lower.tri(dzSignSD[nv+1:nv,nv+1:nv],TRUE)],
                       mzSignSD[1:nv,1:nv][lower.tri(mzSignSD[1:nv,1:nv],TRUE)]+c(.1,.1,.1),
                       mzSignSD[nv+1:nv,nv+1:nv][lower.tri(mzSignSD[nv+1:nv,nv+1:nv],TRUE)]+c(.1,.1,.1),
                       dzSignSD[1:nv,1:nv][lower.tri(dzSignSD[1:nv,1:nv],TRUE)]+c(.1,.1,.1),
                       dzSignSD[nv+1:nv,nv+1:nv][lower.tri(dzSignSD[nv+1:nv,nv+1:nv],TRUE)]+c(.1,.1,.1))

meanSVoptions <- rbind(mzMeans[1:nv],
                       mzMeans[nv+1:nv],
                       dzMeans[1:nv],
                       dzMeans[nv+1:nv],
                       mzMeans[1:nv],
                       mzMeans[nv+1:nv],
                       dzMeans[1:nv],
                       dzMeans[nv+1:nv])

while(ACEgreen != 0 || AEgreen != 0 || CEgreen!= 0 || Egreen!=0) {
  
if(ACEgreen!=0) {
## Fitting without confidence intervals to speed up estimation
# include option intervals=TRUE to estimate confidence intervals; see below

  cholSVACE <- cholSVoptions[sv,1]
  meanSVACE <- meanSVoptions[sv,1]
  
  CholACEModel <- omxSetParameters(CholACEModel, 
                                   c(aLabs,cLabs,eLabs,mLabs), 
                                   free=TRUE, 
                                   values = c(cholSVACE*1/2,
                                              cholSVACE*1/2,
                                              cholSVACE*1/2,
                                              meanSVACE))
  
  CholACEFit <- mxRun(CholACEModel)
  
  ACEgreen <- CholACEFit@output$status[[1]]
  
}

## Test submodels

if(AEgreen!=0) {
  
  cholSVAE <- cholSVoptions[sv,1]
  meanSVAE <- meanSVoptions[sv,1]
  
  CholAEModel <- omxSetParameters(CholACEModel, 
                                   c(aLabs,cLabs,eLabs,mLabs), 
                                   free=TRUE, 
                                   values = c(cholSVAE*1/2,
                                              cholSVAE*1/2,
                                              cholSVAE*1/2,
                                              meanSVAE))
    
  CholAEModel <- omxSetParameters(CholAEModel, cLabs, free=FALSE, values = 0)
  CholAEFit <- mxRun(CholAEModel)
  
  AEgreen <- CholAEFit@output$status[[1]]
    
}

if(CEgreen!=0) {
  
  cholSVCE <- cholSVoptions[sv,1]
  meanSVCE <- meanSVoptions[sv,1]
  
  CholCEModel <- omxSetParameters(CholACEModel, 
                                  c(aLabs,cLabs,eLabs,mLabs), 
                                  free=TRUE, 
                                  values = c(cholSVCE*1/2,
                                             cholSVCE*1/2,
                                             cholSVCE*1/2,
                                             meanSVCE))
  
  CholCEModel <- omxSetParameters(CholCEModel, aLabs, free=FALSE, values = 0)
  CholCEFit <- mxRun(CholCEModel)
  
  CEgreen <- CholCEFit@output$status[[1]]
    
}

if(Egreen!=0) {
  
  cholSVE <- cholSVoptions[sv,1]
  meanSVE <- meanSVoptions[sv,1]
  
  CholEModel <- omxSetParameters(CholACEModel, 
                                  c(aLabs,cLabs,eLabs,mLabs), 
                                  free=TRUE, 
                                  values = c(cholSVE*1/2,
                                             cholSVE*1/2,
                                             cholSVE*1/2,
                                             meanSVE))
  
  CholEModel <- omxSetParameters(CholEModel, c(cLabs,aLabs), free=FALSE, values = 0)
  CholEFit <- mxRun(CholEModel)

  Egreen <- CholEFit@output$status[[1]]
  
}

sv <- sv + 1

}

print(Vars)
green <- rbind(ACEgreen,AEgreen,CEgreen,Egreen,sv)
print(green)



# Check model fit to decide which models to focus on -- only retain models with p greater than 0.05
# Vars
# tableFitStatistics(SatModelFit,c(CholACEFit,CholAEFit,CholCEFit,CholEFit))
# tableFitStatistics(CholACEFit,c(CholAEFit,CholCEFit,CholEFit))
# summary(CholACEFit)
# summary(CholAEFit)
# summary(CholCEFit)







# Estimate confidence intervals for total A, C, and E, and extract correlations
corA <- mxAlgebra(name ="rA", 
                  expression = solve(sqrt(I*A)) %&% A)
corC <- mxAlgebra(name ="rC", 
                  expression = solve(sqrt(I*C)) %&% C)
corE <- mxAlgebra(name ="rE", 
                  expression = solve(sqrt(I*E)) %&% E)



CholACEci <- mxCI(c("rA","rC","rE"), interval=0.95, type="both")
CholAEci <- mxCI(c("rA","rE"), interval=0.95, type="both")
CholCEci <- mxCI(c("rC","rE"), interval=0.95, type="both")

CholACEciModel <- mxModel(CholACEModel,
                         corA, corC, corE,
                         CholACEci)

CholAEciModel <- mxModel(CholAEModel,
                         corA, corE,
                         CholAEci)

CholCEciModel <- mxModel(CholCEModel,
                          corC, corE,
                          CholCEci)


## Fitting - comment out any models that you don't want to run (e.g., due to poor fit, as determined above)
CholACEciFit <- mxRun(CholACEciModel, intervals=TRUE)

CholAEciFit <- mxRun(CholAEciModel, intervals=TRUE)

#CholCEciFit <- mxRun(CholCEciModel, intervals=TRUE)

# interpretations of results:
### a100[2,1] is the percent of variance in variable 2 from latent component 1  
### A2[1,1] is the total A variance for all "a" paths that lead to variable 2
### rpA2[1,1] is the percent of the covariance between variable 1 and variable 2 that is from shared A components
#         - multivariate only
### rA[1,2] is the genetic correlation between variable 1 and variable 2
#         - multivariate only




# save workspace
Vars
setwd("./results")
save.image(file=savenames[i])

}


# can check results for each model after loading the output with the commands below
# Vars
# tableFitStatistics(SatModelFit,c(CholACEFit,CholAEFit,CholCEFit,CholEFit))
# tableFitStatistics(CholACEFit,c(CholAEFit,CholCEFit,CholEFit))
# summary(SatModelFit)
# summary(CholACEciFit)
# summary(CholCEciFit)
# summary(CholAEciFit)

