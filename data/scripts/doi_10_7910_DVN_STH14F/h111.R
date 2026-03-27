#############################################
## Load libraries included in the environment
require("RcppTN")
require("RcppArmadillo")
require("Ternary")
require("RColorBrewer")
require("pscl")
require("wnominate")
#############################################

################################################################################
## Install and load custom library
install.packages("../code/compareK", repos = NULL, lib="../data", type="source")
library("compareK", lib="../data")
################################################################################

###################################
## Setup names of vote groups
typenames = c("Macroeconomics", "Civil Rights", "Health", 
              "Agriculture", "Labor and Employment", "Education", "Environment", 
              "Energy", "Transportation", "Law and Crime", "Housing", 
              "Banking and Finance", "Defense", "Space, Science and Communications", 
              "International Affairs", "Government Operations", "Public Lands")

###################################################################################
## Run MCMC algorithm for the 111th and generate graphs
set.seed(987654)
chouse=111
###################################################################################

#####################################
## Load data for one particular house
y         = as.matrix(read.table(paste("../data/H",chouse,"votesPAP.txt", sep="")))
missing   = as.matrix(read.table(paste("../data/H",chouse,"missingPAP.txt", sep="")))
group     = scan(paste("../data/H",chouse,"codesPAP.txt", sep=""))
demlead   = scan(paste("../data/H",chouse,"demleadPAP.txt", sep=""))
replead   = scan(paste("../data/H",chouse,"repleadPAP.txt", sep=""))
KK        = length(unique(group))
#####################################
  
#####################################
## Run the sampler for that house
nsamples   = 47000
burn       = 7000
thin       = 3
printevery = 500
print(date())
obj        = compareidealK1D(votes=y, missing=missing, group=group, demleader=demlead, repleader=replead, nsamples = nsamples, burn = burn, thin = thin, printevery=printevery, samplezeta=TRUE, startconfig=0, varalpprop = 0.07)
print(date())
#####################################

###########################################
## Compute the average bridging probability
## This is used in Figures 1 and 3
nunique        = function(x) length(unique(x))
numidealpoints = apply(obj$beta, c(1,3), nunique)
BP             = apply(numidealpoints==1, 1, mean)
print(paste("*** ASP =",mean(BP)))        
###########################################
  
####################################################################################
## Compute differences with respect to the legislator's "majority" ideal point
## This is used for Figure 2, as well as for some of the detailed pictures in Figure 
print("Computing difbetap")
zeroprob = function(x) mean(x==0) 
difbetap = obj$beta
rr = dim(obj$beta)[3]
II = dim(obj$beta)[1]
for(ii in 1:II){
  for(ss in 1:rr){
    ojc = as.numeric(factor(obj$beta[ii,,ss], labels=seq(1,nunique(obj$beta[ii,,ss]))))
    ind = which.max(tapply(table(group), ojc, sum))
    difbetap[ii,,ss] = difbetap[ii,,ss] - obj$beta[ii,which(ojc==ind)[1],ss]
  }
  if(ii/100==floor(ii/100)) print(ii)
}
print("*** PatialASP =")        
partialbridgeprob = apply(difbetap, c(1,2), zeroprob)
print(apply(partialbridgeprob, 2, mean))
####################################################################################
  
  
#####################################
## Gerating graphs for the 111th House
#####################################
legparty  = scan(paste("../data/H",chouse,"legpartyPAP.txt", sep=""))
legnames  = read.table(paste("../data/H",chouse,"legnamesPAP.txt", sep=""), quote="\"", comment.char="")
condnames = paste(legnames[,1],legnames[,2],legnames[,3])

###################################################################################
## Reproducing Figure 7_1 to 7_17 in the manuscript (Triangle plots)
## Figure in paper was slighty edited for legibility after the fact
colorscale = c("blue","red","green")
pchscale = c(16,15,17)
    
threecaseprob = function(x){
  z = c(mean(x==0), mean(x>0), mean(x<0))
  return(z)
}      
summariesdiffbeta = apply(difbetap, c(1,2), threecaseprob)
    
for(kk in 1:KK){
  triangle = matrix(c(1,0,0,0.5,
                      0,0.5,0.5,
                      0.5,0),nrow=3,ncol=3, byrow=T)
  square1 =  matrix(c(0.5,0.5,0,
                      0.5,0.25,0.25,
                      0,0.5,0.5,
                      0,1,0), nrow=4,ncol=3, byrow=T)
  square2 =  matrix(c(0.5,0,0.5,
                      0.5,0.25,0.25,
                      0,0.5,0.5,
                      0,0,1), nrow=4,ncol=3, byrow=T)
      
  pointstoplot = t(summariesdiffbeta[,,kk])
  colorstoplot = colorscale[legparty]
  pchtoplot    = pchscale[legparty]
  ind          = summariesdiffbeta[1,,kk]<0.5
      
  pdf(file=paste("../results/Figure7_",kk,".pdf",sep=""))
  par(mar=c(0,0,2,0))
  TernaryPlot(atip="No move",btip="Rightwards move",ctip="Leftward move",main=typenames[kk])
  TernaryPolygon(triangle, col=grey(0.7), border=grey(0.7))
  TernaryPolygon(square1, col=grey(0.5), border=grey(0.5))  
  TernaryPolygon(square2, col=grey(0.9), border=grey(0.9))  
  TernaryPoints(pointstoplot, pch=pchtoplot, cex=1.5, col=colorstoplot)
  if(sum(ind)>0){
    TernaryText(pointstoplot[ind,], condnames[ind], cex=0.9, pos=3, font=2, col=colorstoplot[ind])
  }
  dev.off()
}
###################################################################################
    
    
###################################################################################
## Reproducing Figure 8 in the manuscript (Comparison of median ranks)
obj.joint = compareidealK1D(votes=y, missing=missing, group=group, demleader=demlead, repleader=replead, nsamples = nsamples, burn = burn, thin = thin, printevery=printevery, samplezeta=FALSE, startconfig=0, varalpprop = 0.07)
obj.ind   = compareidealK1D(votes=y, missing=missing, group=group, demleader=demlead, repleader=replead, nsamples = nsamples, burn = burn, thin = thin, printevery=printevery, samplezeta=FALSE, startconfig=1, varalpprop = 0.07)

betaranks = obj$beta
for(kk in 1:KK){
  for(ss in 1:dim(obj$beta)[3]){
    betaranks[,kk,ss] = rank(obj$beta[,kk,ss])
  }
  print(kk)
}
betaranksummary.ourmodel = apply(betaranks, c(1,2), quantile, c(0.025,0.50,0.975))

betaranks = obj.ind$beta
for(kk in 1:KK){
  for(ss in 1:dim(obj.ind$beta)[3]){
    betaranks[,kk,ss] = rank(obj.ind$beta[,kk,ss]*sign(obj.ind$beta[replead,kk,ss]))
  }
  print(kk)
}
betaranksummary.ind = apply(betaranks, c(1,2), quantile, c(0.025,0.50,0.975))

betaranks = obj.joint$beta
for(kk in 1:KK){
  for(ss in 1:dim(obj.joint$beta)[3]){
    betaranks[,kk,ss] = rank(obj.joint$beta[,kk,ss])
  }
  print(kk)
}
betaranksummary.joint = apply(betaranks, c(1,2), quantile, c(0.025,0.50,0.975))
    
kk = 16   ##Government operations
ind = summariesdiffbeta[1,,kk]<0.5

pdf(file="../results/Figure8a.pdf")
par(mar=c(4,4,2,2)+0.1)
plot(betaranksummary.joint[2,,kk], betaranksummary.ourmodel[2,,kk], xlab="Median rank under IDEAL on all votes", ylab="Median rank under our model", las=1, pch=c(1,2)[as.numeric(legparty)], col=colorscale[as.numeric(legparty)])
points(betaranksummary.joint[2,ind,kk], betaranksummary.ourmodel[2,ind,kk], pch=c(19,17)[as.numeric(legparty[ind])], col=colorscale[as.numeric(legparty[ind])])
text(betaranksummary.joint[2,ind,kk], betaranksummary.ourmodel[2,ind,kk], condnames[ind], pos=3, col=colorscale[as.numeric(legparty[ind])])
title(main=paste("House",chouse,"  ",typenames[kk]))
dev.off()

pdf(file="../results/Figure8b.pdf")
par(mar=c(4,4,2,2)+0.1)
plot(betaranksummary.joint[2,,kk],betaranksummary.ind[2,,kk], xlab="Median rank under IDEAL on all votes", ylab="Median rank under IDEAL on topic votes only", las=1, pch=c(1,2)[as.numeric(legparty)], col=colorscale[as.numeric(legparty)])
points(betaranksummary.joint[2,ind,kk], betaranksummary.ind[2,ind,kk], pch=c(19,17)[as.numeric(legparty[ind])], col=colorscale[as.numeric(legparty[ind])])
text(betaranksummary.joint[2,ind,kk], betaranksummary.ind[2,ind,kk], condnames[ind], pos=3, col=colorscale[as.numeric(legparty[ind])])
title(main=paste("House",chouse,"  ",typenames[kk]))
dev.off()
###################################################################################

###################################################################################
## Reproducing Figure 4 in the manuscript (Average ranks with movers highlighted)
## Figure in paper was slightly edited for legibility to depict only a few selected legislators
namestoplot <- c("TOWNS (D NY-10)", "MORAN (D VA-8)", 
                 "STARK (D CA-13)", "RAHALL (D WV-3)", "EDWARDS (D TX-17)", 
                 "DRIEHAUS (D OH-1)", "GIFFORDS (D AZ-8)", "ROS-LEHTINE (R FL-18)", 
                 "YOUNG (R AK-1)", "HELLER (R NV-2)", "ROHRABACHER (R CA-46)", 
                 "DUNCAN (R TN-2)", "MCCLINTOCK (R CA-4)", "PAUL (R TX-14)")
ind0 = (BP < 0.5) 
ind  = (BP < 0.5) & (condnames %in% namestoplot)  
II = dim(obj.joint$beta)[1]
ord = order(betaranksummary.joint[2,,1], decreasing=F)
pdf(file="../results/Figure4.pdf")
par(mar=c(4,12,1,1)+0.1)
plot(seq(1,II), seq(1,II), type="n", axes=F,xlab="",ylab="")
segments((betaranksummary.joint[1,ord,1]), seq(1,II), (betaranksummary.joint[3,ord,1]), seq(1,II), col="grey")
segments((betaranksummary.joint[1,ord,1])[ind0[ord]], seq(1,II)[ind0[ord]], (betaranksummary.joint[3,ord,1])[ind0[ord]], seq(1,II)[ind0[ord]], col=colorscale[(legparty[ord])[ind0[ord]]])
axis(1)
axis(2, at=(seq(1,II)[ind[ord]]), labels=rep("", sum(ind)))
mtext(at=(seq(1,II)[ind[ord]]), text=((condnames[ord])[ind[ord]]), side=2, line=1, las=2, col=colorscale[(legparty[ord])[ind[ord]]])
mtext("Legislator", side=2, line=11)
box()
dev.off()
###################################################################################

#############################################################
## Reproducing Figure 5 in the manuscript (ABP for bottom 50)
nn = 50
ord = order(BP,decreasing=F)[1:nn]
pdf(file="../results/Figure5.pdf")
par(mar=c(13,4,1,1)+0.1)
plot(BP[ord[1:nn]], axes=F, xlab="", ylab=expression(BF[i]),ylim=c(0,0.08), pch=c(19,17)[as.numeric(legparty[ord[1:nn]])], col=colorscale[as.numeric(legparty[ord[1:nn]])])
mtext(at=seq(1,nn), text=condnames[ord[1:nn]], side=1, line=1, las=2, col=colorscale[as.numeric(legparty[ord[1:nn]])])
mtext("Legislator", side=1, line=11)
axis(2, las=2)
box()
dev.off()
#############################################################

##########################################################
## Reproducing Figure 6 in the manuscript (ABP for top 50)
nn = 50
ord = order(BP,decreasing=T)[1:nn]
pdf(file="../results/Figure6.pdf")
par(mar=c(13,4,1,1)+0.1)
plot(BP[ord[1:nn]], axes=F, xlab="", ylab=expression(BF[i]),ylim=c(0.95,1), pch=c(19,17)[as.numeric(legparty[ord[1:nn]])], col=colorscale[as.numeric(legparty[ord[1:nn]])])
mtext(at=seq(1,nn), text=condnames[ord[1:nn]], side=1, line=1, las=2, col=colorscale[as.numeric(legparty[ord[1:nn]])])
mtext("Legislator", side=1, line=11)
axis(2, las=2)
box()
dev.off()
##########################################################

#########################################################################################################
## Computing the percentage of the variability explained by 1st component of WNOMINATE
houdsedata     = readKH(paste("https://voteview.com/static/data/out/votes/H",chouse,"_votes.ord",sep=""))
chousewnom1    = wnominate(houdsedata,polarity=c(2,5))
firstprincomp1 = chousewnom1$eigenvalues[1]/sum(chousewnom1$eigenvalues)
#########################################################################################################

#########################################################################################################
## Close session
print(date())
save.image("../results/results.Rdata")
#########################################################################################################

#############################################################################################
## Output results for Figures 1, 2 and 3
print("******************************************************************")
print("******************************************************************")
print(paste("*** ASP (Used in Figure 1 and Figure 3) = ",mean(BP)))        
print(paste("*** Percentage of variability in 1st component of WNOMINATE (used in Figure 3) = ",firstprincomp1))
print("*** PatialASP (Used in Figure 2a to 2d) =")        
print(apply(partialbridgeprob, 2, mean))
print("******************************************************************")
print("******************************************************************")
#############################################################################################

