# Code to repoduce the lysozyme analysis
#
# The input here is the data in lysozyme_nets.Rdata.  Models
# are saved in lysozyme_analysis.Rdata, though some results are produced inline.
#
# Last modified 10/24/21 by CTB

#Load packages
library(ergm)   #Paper results used ergm 4.1.2
library(sna)
library(MASS)
library(xtable)


#Data frame with some basic amino acid properties.  Polarity, mass, and
#van der Waals volume from Wikipedia.  Hydropathy is from the scale of Kyte
#and Doolittle (1982).
aa.prop<-data.frame(
  ThreeLetterName=c("Ala","Arg","Asn","Asp","Cys","Glu","Gln","Gly","His","Ile",
    "Leu","Lys","Met","Phe","Pro","Ser","Thr","Trp","Tyr","Val","Sec","Pyl"),
  OneLetterName=c("A","R","N","D","C","E","Q","G","H","I","L","K","M","F","P",
    "S","T","W","Y","V","U","O"),
  Polar=c(FALSE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,FALSE,TRUE,FALSE,FALSE,TRUE,
    FALSE,FALSE, FALSE,TRUE,TRUE,FALSE,TRUE,FALSE,NA,NA),
  HydropathyKD=c(1.8,-4.5,-3.5,-3.5,2.5,-3.5,-3.5,-0.4,-3.2,4.5,3.8,-3.9,
    1.9,2.8,-1.6,-0.8,0.7,-0.9,-1.3,4.2,NA,NA),
  Mass=c(89.09404,174.20274,132.11904,133.10384,121.15404,147.13074,
    146.14594,75.06714,155.15634,131.17464,131.17464,146.18934,149.20784,
    165.19184,115.13194,105.09344,119.12034,204.22844,181.19124,117.14784,
    NA,NA)-18.01524,
  Volume=c(67,148,96,91,86,109,114,48,118,124,124,135,124,135,90,73,93,
    163,141,105,NA,NA),
  Charge=c(0,1,0,-1,0,-1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,NA,NA),
  Surface=c(115,225,160,150,135,190,180,75,195,175,170,200,185,210,145,115, 
    140,255,230,155, NA,NA),
  Aromatic=c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,
    FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,FALSE),
  stringsAsFactors=FALSE
)
rownames(aa.prop)<-c("A","R","N","D","C","E","Q","G","H","I","L","K","M",
  "F","P","S","T","W","Y","V","U","O")


#Define the model formula
fstr<-"edges+edgecov(logBBDist)+edgecov(ChargeMatch)+edgecov(NPolNPol) + edgecov(PolPol) + nodecov(\"Aromatic\") + edgecov(PiStack) + nodecov(\"Surface\") + nodecov(\"meanCAD\") + edgecov(logMeanDist) + edgecov(logMeanDistSurf)+ edgecov(logMeanDistAro) + gwesp(0.8, fixed=TRUE) + kstar(2) + triangle"


#Load the data
load("lysozyme_nets.Rdata")
n<-network.size(nets[[1]])  #Lysozyme network size
m<-length(nets)             #Number of networks

#Construct additional covariates
covarFromSeq<-function(s){  #Function to make covariates from aa seq
  i<-match(s,rownames(aa.prop))
  #Charge and charge matching
  chg<-aa.prop$Charge[i]
  ChargeMatch<- (chg==-1)%o%(chg==1) + (chg==1)%o%(chg==-1) - (chg==-1)%o%(chg==-1) - (chg==1)%o%(chg==1) 
  #Sequence distance
  nv<-length(s)
  bb<-matrix(0,nv,nv)
  bb[cbind(1:(nv-1),2:nv)]<-1
  bb[cbind(2:nv,1:(nv-1))]<-1
  bbDist<-geodist(bb)$gdist
  logBBDist<-log(bbDist)
  diag(logBBDist)<-0           #Keeps various things from blowing up
  #Polar/nonpolar mixing
  pol<-aa.prop$Polar[i]
  PolPol<-pol%o%pol
  NPolNPol<-(1-pol)%o%(1-pol)
  #Aromatics
  aro<-aa.prop$Aromatic[i]
  pistack<-aro%o%aro
  #Surface
  surf<-aa.prop$Surface[i]
  #Return the results
  list(Charge=chg, ChargeMatch=ChargeMatch, bbDist=bbDist, logBBDist=logBBDist, Polar=pol, PolPol=PolPol, NPolNPol=NPolNPol, Aromatic=aro, PiStack=pistack, Surface=surf)
}
lys.prop<-covarFromSeq(nets[[1]]%v%"AA")
ChargeMatch<-lys.prop$ChargeMatch    #Charge matching
bbDist<-lys.prop$bbDist              #Backbone distance matrix
logBBDist<-lys.prop$logBBDist        #Log BB distance
PolPol<-lys.prop$PolPol              #Polar -> Polar
NPolNPol<-lys.prop$NPolNPol          #Non-polar -> non-polar
PiStack<-lys.prop$PiStack            #Interactions among aromatics
for(i in 1:m){                       #Add required covars to data
  nets[[i]]%v%"Aromatic"<-lys.prop$Aromatic
  nets[[i]]%v%"Surface"<-lys.prop$Surface
}
co<-lapply(nets,function(z){         #Extract/center C-alpha coordinates
  coord<-cbind(z%v%"CA.x",z%v%"CA.y",z%v%"CA.z")
  sweep(coord,2,colMeans(coord),"-")
})
meanCAD<-rep(0,n)                    #Mean CA distance from center
for(i in 1:m)
  meanCAD<-meanCAD+rowSums(co[[i]]^2)^0.5
meanCAD<-meanCAD/m
for(i in 1:m)
  nets[[i]]%v%"meanCAD"<-meanCAD
logMeanDist<-matrix(0,n,n)           #Log mean CA distances
for(i in 1:m)
  logMeanDist<-logMeanDist+as.matrix(dist(co[[i]]))
logMeanDist<-log(logMeanDist/m)
diag(logMeanDist)<-0
#Interaction effect matrices
logMeanDistSurf<-logMeanDist*outer(lys.prop$Surface,lys.prop$Surface,"+")
logMeanDistAro<-logMeanDist*outer(lys.prop$Aromatic,lys.prop$Aromatic,"+")


#Obtain the observed stats
observed.stats <- summary(as.formula(paste("nets",fstr,sep="~")))
observed.mean.stats <- apply(observed.stats, 2, mean)


#Prior specification: 
#
#To get the prior mean degree, we start with the predicted fraction of
#exposed residues (a priori).  For folded monomeric proteins, Miller et al.
#(J. Mol Bio, 1987) report the empirical model:
#
# A_f = 6.3 M^0.73
#
#(where A_f is the SASA in A^2 and M is the molecular mass in Da).  For
#unfolded proteins, they report the model:
#
# A_u = 1.48 M + 21
#
#Now, let us assume that the maximum mean degree of a residue is the contact
#number of a packed sphere, i.e. 12.  If all "surface area" of each residue
#was covered by other residues, then this would be the approximate mean 
#degree; however, in practice, some fraction of surface area is covered by
#solvent instead.  We approximate the fraction of contacts "lost" to solvent
#as A_f/A_u, and thus the mean degree as 12 (1 - A_f/A_u).  For lysozyme
#(mass 14.3kDa), we then have
#  A_f apx 6803.554 A^2
#  A_u apx 21185 A^2
#  Fraction contacts "lost" apx 0.32
#  Mean degree apx 12 (1-A_f/A_u) = 8.15
#
#This is actually very close to the observed value in our sample (8.32),
#though obtained on a priori grounds.
prior.mdeg<-12*(1-(6.3*14300^0.73)/(1.48*14300+21))

#We set the prior wieght to be fairly small: we would want even a single
#graph observation here to largely outweigh our prior, so we set n_0=0.1
#(one tenth of a single graph observation).  This results in a delta
#of about 0.0015
n0<-0.1
delta<-n0/(n0+m)

#To generate the prior mean stats, we now generate draws from a conditional
#Bernoulli graph with (1) an expected degree matching the prior, and (2)
#the backbone ties fixed in place.  We can do this using an exact sampling
#scheme, being careful to adjust the mean degree accordingly.
tp<-matrix((n*prior.mdeg/2-n+1)/(choose(n,2)-n+1),n,n) #Tie probability matrix
tp[cbind(1:(n-1),2:n)]<-1
tp[cbind(2:n,1:(n-1))]<-1
diag(tp)<-0
set.seed(1331)
prior.networks<-lapply(1:1000,function(i){
  g<-rgraph(n,tp=tp,mode="graph")
  net<-network(g,directed=FALSE)
  net%v%"AA"<-nets[[1]]%v%"AA"
  net%v%"AA.TLA"<-nets[[1]]%v%"AA.TLA"
  net%v%"Charge"<-nets[[1]]%v%"Charge"
  net%v%"Hydrophobicity"<-nets[[1]]%v%"Hydrophobicity"
  net%v%"Mass"<-nets[[1]]%v%"Mass"
  net%v%"NAtoms"<-nets[[1]]%v%"NAtoms"
  net%v%"Polar"<-nets[[1]]%v%"Polar"
  net%v%"resid"<-nets[[1]]%v%"resid"
  net%v%"Volume"<-nets[[1]]%v%"Volume"
  net%v%"Aromatic"<-lys.prop$Aromatic
  net%v%"Surface"<-lys.prop$Surface
  net%v%"meanCAD"<-meanCAD
  net
})
class(prior.networks)<-"network.list"

#We now obtain the prior stats
prior.stats <- summary(as.formula(paste("prior.networks",fstr,sep="~")))
prior.mean.stats <- apply(prior.stats, 2, mean)


#Now, fit the model by MAP
set.seed(1331)
conjugate.lysozyme.fit <- ergm( as.formula(paste("nets[[1]]",fstr,sep="~")), target.stats = delta*prior.mean.stats + (1-delta)*observed.mean.stats, constraints=~fixedas(present=network(bbDist==1, directed=FALSE)), eval.loglik=FALSE)
postmode<-coef(conjugate.lysozyme.fit)
postcov<-conjugate.lysozyme.fit$covar/(m+n0)
postsd<-diag(postcov)^0.5
conjugate.lysozyme.fit$post.mode<-postmode
conjugate.lysozyme.fit$post.cov<-postcov
conjugate.lysozyme.fit$post.sd<-postsd
conjugate.lysozyme.fit$m<-m
conjugate.lysozyme.fit$n0<-n0
conjugate.lysozyme.fit$delta<-delta
conjugate.lysozyme.fit$prior.mean<-prior.mean.stats
conjugate.lysozyme.fit$obs.mean.stats<-observed.mean.stats

#Display coef table
xtable(cbind(postmode,postsd,postmode-1.96*postsd,postmode+1.96*postsd),digits=c(1,3,4,3,3))

#Simulate draws from the approximate posterior predictive
set.seed(1331)
theta<-mvrnorm(n = 1000, mu=postmode, Sigma=postcov)
ppred<-apply(theta,1,function(q){
  simulate(as.formula(paste("nets[[1]]",fstr,sep="~")), coef=q, constraints=~fixedas(present=network(bbDist==1,directed=FALSE)), control=control.simulate.formula(MCMC.burnin=2e5), nsim=1)
})
class(ppred)<-"network.list"


#Perform traditional model adequacy tests
deg.pp<-summary(ppred~degree(0:18))
deg.obs<-summary(nets~degree(0:18))
esp.pp<-summary(ppred~esp(0:12))
esp.obs<-summary(nets~esp(0:12))
tc.pp<-summary(ppred~triadcensus(levels=0:3))
tc.obs<-summary(nets~triadcensus(levels=0:3))
gdd<-function(x,dmax=network.size(x)-1){  #Geodesic distance distribution
  gd<-geodist(x)$gdist
  gdt<-tabulate(gd[upper.tri(gd)],dmax)
  gdt
}
gd.pp<-t(sapply(ppred,gdd))[,1:10]
gd.obs<-t(sapply(nets,gdd))[,1:10]
MAPlot<-function(sim,obs,mode=c("poly","boxes"), xaxis.lab=1:NCOL(sim), cex.axis=1.25, ...){ #Model adequacy plot
  #Get quantiles for simulation, and observed means
  mobs<-colMeans(obs)
  qsim<-apply(sim,2,quantile,c(0.025,0.25,0.5,0.75,0.975))
  msim<-colMeans(sim)
  #Plot the simulation intervals
  nx<-length(msim)
  if(match.arg(mode)=="poly"){
    plot(1,1,type="n",xlim=c(1,nx),ylim=range(c(as.vector(qsim),mobs)), axes=FALSE, font.lab=2, ...)
    axis(1,at=1:nx,labels=xaxis.lab,font.axis=2,cex.axis=cex.axis)
    axis(2,font.axis=2,cex.axis=cex.axis)
    polygon(c(1:nx,nx:1),c(qsim[1,],rev(qsim[5,])),col=rgb(0.95,0.95,0.95), border=rgb(0.4,0.4,0.4))
    polygon(c(1:nx,nx:1),c(qsim[2,],rev(qsim[4,])),col=rgb(0.9,0.9,0.9), border=rgb(0.2,0.2,0.2))
    lines(1:nx,msim,lwd=2)
    points(1:nx,mobs,pch=19,col=2)
    legend("topright",fill=c(rgb(0.95,0.95,0.95),rgb(0.90,0.90,0.90),NA,NA), pch=c(NA,NA,NA,19), lty=c(NA,NA,1,NA), lwd=c(NA,NA,2,NA), border=c(rgb(0.4,0.4,0.4), rgb(0.2,0.2,0.2), NA, NA, NA), col=c(NA,NA,1,2), legend=c("95% PPI", "50% PPI", "PP Mean", "Obs"), x.intersp=c(1,1,1.7,1.7), bty="n")
  }else{
    plot(1,1,type="n",xlim=c(1-0.5,nx+0.5),ylim=range(c(as.vector(qsim),mobs)), axes=FALSE, font.lab=2, ...)
    axis(1,at=1:nx,labels=xaxis.lab,font.axis=2,cex.axis=cex.axis)
    axis(2,font.axis=2,cex.axis=cex.axis)
    segments(1:nx,qsim[1,],1:nx,qsim[5,])
    segments((1:nx)-0.15,qsim[1,],(1:nx)+0.15,qsim[1,])
    segments((1:nx)-0.15,qsim[5,],(1:nx)+0.15,qsim[5,])
    rect((1:nx)-0.25,qsim[2,],(1:nx)+0.25,qsim[4,],col=rgb(0.9,0.9,0.9))
    points(1:nx,msim,pch=19,cex=2)
    points(1:nx,mobs,pch=19,col=2)
    legend("topright",fill=c(NA,rgb(0.90,0.90,0.90),NA,NA), pch=c(NA,NA,19,19), lty=c(1,NA,NA,NA), lwd=c(1,NA,NA,NA), border=c(NA, 1, NA, NA, NA), col=c(1,NA,1,2), legend=c("95% PPI", "50% PPI", "PP Mean", "Obs"), x.intersp=c(1.7,1,1.7,1.7), bty="n")
  }
}
pdf("lysozyme_conj_ppred_checks.pdf",8,8)
par(mfrow=c(2,2))
MAPlot(deg.pp,deg.obs,xaxis.lab=0:18,xlab="Degree",ylab="Number of Vertices", cex.lab=1.25,main="Degree")
MAPlot(esp.pp,esp.obs,xaxis.lab=0:12,xlab="ESP",ylab="Number of Edges", cex.lab=1.25,main="Edgewise Shared Partners")
MAPlot(tc.pp,tc.obs,xaxis.lab=0:3,,mode="boxes",xlab="Triad Class",ylab="Number of Triads", log="y", cex.lab=1.25,main="Triad Census")
MAPlot(gd.pp,gd.obs,xaxis.lab=1:10,xlab="Geodesic Distance",ylab="Number of Dyads", cex.lab=1.25,main="Geodisic Distance")
dev.off()

#Compare posterior predictives for structural metrics to the observed
#distributions
#Mean eccentricity
M_eccen <- function(x)
{
  M_eccentricity <- apply(geodist(x)$gdist, 1, sum)/(n-1)
  return(M_eccentricity)
}

pp.sdmecc<-sapply(ppred,function(z){sd(M_eccen(z))})
pp.sddeg<-sapply(ppred,function(z){sd(degree(z,gmode="graph"))})
pp.sdcore<-sapply(ppred,function(z){sd(kcores(z,mode="graph"))})
pp.trans<-gtrans(ppred)
obs.sdmecc<-sapply(nets,function(z){sd(M_eccen(z))})
obs.sddeg<-sapply(nets,function(z){sd(degree(z,gmode="graph"))})
obs.sdcore<-sapply(nets,function(z){sd(kcores(z,mode="graph"))})
obs.trans<-gtrans(nets)

pdf("lysozyme_conj_fourGLI.pdf",7,7)
par(mfrow=c(2,2))
boxplot(list(obs.trans,pp.trans),main="Transitivity",font.lab=2,font.axis=2,cex.lab=1.25,cex.axis=1.25,names=c("Observed","PostPred"),ylab="Transitivity")
boxplot(list(obs.sdmecc,pp.sdmecc),main="SD(M-Eccentricity)",font.lab=2,font.axis=2,cex.lab=1.25,cex.axis=1.25,names=c("Observed","PostPred"),ylab="SD(M-Eccentricity)")
boxplot(list(obs.sddeg,pp.sddeg),main="SD(Degree)",font.lab=2,font.axis=2,cex.lab=1.25,cex.axis=1.25,names=c("Observed","PostPred"),ylab="SD(Degree)")
boxplot(list(obs.sdcore,pp.sdcore),main="SD(CoreNumber)",font.lab=2,font.axis=2,cex.lab=1.25,cex.axis=1.25,names=c("Observed","PostPred"),ylab="SD(CoreNumber)")
dev.off()

#Save nontrivial/helpful objects for later reuse
save(nets, aa.prop, conjugate.lysozyme.fit, bbDist, ChargeMatch,logBBDist, logMeanDist, logMeanDistAro, logMeanDistSurf, lys.prop, ppred, prior.mean.stats, prior.networks, prior.stats, observed.stats, observed.mean.stats, PiStack, n0, meanCAD, n, m, PolPol, NPolNPol, prior.mdeg, theta, delta, fstr, file="lysozyme_analysis.Rdata")
