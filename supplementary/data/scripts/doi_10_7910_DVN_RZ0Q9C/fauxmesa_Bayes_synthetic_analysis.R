# Code to repoduce the Bayes analysis tables for the Faux Mesa High Simulations
#
# The inputs here are the ground truth model in fms_groundtruth_model.RData
# and the replicated models in fms_synthetic_data_MLE.RData.
#
# Last modified 8/19/21 by CTB

#Load required libraries
library(xtable)

#Load the MAP data
load("fms_synthetic_data_Bayes.RData")

#Load the ground truth model
load("fms_groundtruth_model.RData")


#Build the mean parameter table
allmco<-t(sapply(fmsBayes,function(z){rowMeans(sapply(z,"[[","coef"))}))
tab<-cbind(formatC(delta,digits=4,format="f"), formatC(allmco,digits=4,format="f"))
rownames(tab)<-NULL
xtable(tab)
cat(apply(tab,1,function(z){paste(paste(z,collapse=" & "),"\n")}))

#Find the frequentist properties of the MAP estimators
bias<-sweep(allmco,2,coef(fmsfit),"-")
allse<-t(sapply(fmsBayes,function(z){apply(sapply(z,"[[","coef"),1,sd)}))
allc95<-t(sapply(fmsBayes,function(x){
  md<-sweep(sapply(x,"[[","coef"),1,coef(fmsfit),"-") #Mean diff
  se<-sapply(x,function(w){diag(w$cov)^0.5})          #Estimated se
  z<-md/se                                            #Nominal Z score
  rowMeans(abs(z)<1.96)                               #Coverage of 95% PI
}))

#Build the frequentist property table
tab<-cbind(formatC(delta,digits=4,format="f"), formatC(delta/(1-delta),digits=3,format="f"), formatC(bias,digits=4,format="f"), formatC(allc95,digits=3,format="f"))[,c(1,2,3,6,4,7,5,8)]
xtable(tab)
cat(apply(tab,1,function(z){paste(paste(z,collapse=" & ")," \\\\ \n")}))

#Script to produce the MAP estimate figure (coefficient movement as a
#function of delta)
dat<-cbind(delta,allmco)
pdf("map_meanest_bydelta.pdf",7,4)
par(mar=c(5,5,2,2))
plot(0,0,type="n",xlim=c(0,1),ylim=range(dat[,-1]), xlab=expression(delta), ylab
=expression(hat(theta)^"MAP"),font.axis=2,font.lab=2,cex.axis=1.25,cex.lab=1.25)
abline(v=c(0,1),lty=3)
for(i in 2:4)
  points(dat[,1],dat[,i],type="b",lwd=3,col=i)
legend(0.1,-2,col=2:4,lwd=3,pch=1,legend=c("edges","nodematch(Gender)","GWESP(0.
25)"),bty="n")
dev.off()
