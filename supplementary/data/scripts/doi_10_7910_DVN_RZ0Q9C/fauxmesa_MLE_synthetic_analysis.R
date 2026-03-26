# Code to repoduce the MLE analysis table for the Faux Mesa High Simulations
#
# The inputs here are the ground truth model in fms_groundtruth_model.RData
# and the replicated models in fms_synthetic_data_MLE.RData.
#
# Last modified 8/18/21 by CTB

#Load required libraries
library(xtable)

#Load the ground truth and fitted models
load("fms_groundtruth_model.RData")
load("fms_synthetic_data_MLE.RData")

#Compute the bias, true SE, and confidence coverage
allmco<-t(sapply(fmsMLEs,function(z){rowMeans(sapply(z,"[[","coef"))}))
bias<-sweep(allmco,2,coef(fmsfit),"-")
allse<-t(sapply(fmsMLEs,function(z){apply(sapply(z,"[[","coef"),1,sd)}))
allc95<-t(sapply(fmsMLEs,function(x){
  md<-sweep(sapply(x,"[[","coef"),1,coef(fmsfit),"-") #Mean diff
  se<-sapply(x,function(w){diag(w$cov)^0.5})          #Estimated se
  z<-md/se                                            #Nominal Z score
  rowMeans(abs(z)<1.96)                               #Coverage of 95% CI
}))

#Create table
tab<-cbind(formatC(bias,digits=4,format="f"), matrix(paste0("(",format(allse,digits=3,flag="0",format="f",drop0trailing),")"),ncol=3), formatC(allc95,digits=3,drop0trailing=F,flag="0",format="f"))[,c(1,4,7,2,5,8,3,6,9)]
xtable(tab,digits=c(0,4,3,3,4,3,3,4,3,3))
