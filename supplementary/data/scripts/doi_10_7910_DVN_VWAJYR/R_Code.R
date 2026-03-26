
rm(list=ls())
setwd("C:/Users/Jason/Dropbox/DIADEMA Analyses/Résultats_provisoires_+_draft/Manuscript4(bota-niche)/R_analyses/2021")


#######################
## Download packages ##
#######################

library(vegan)      ; library(ade4)          ; library(adespatial) 
library(ggplot2)    ; library(forecast)      ; library(spacodiR)
library(FactoMineR) ; library(factoextra)    ; library(usdm)
library(relaimpo)   ; library(spacodiR)      ; library(stringr)
library(phytools)   ; library(brranching)    ; library(spdep)
library(phyloseq)   ; library(BiodiversityR) ; library(labdsv)
library(devtools)   ; library(V.PhyloMaker)  ; library(phylotate)
library(ape)        ; library(rethinking)    ; library(FD) ; library(varhandle)

#citation("ape") ## reference for package "ape"

#############################################
#********************************************
## Choose datasets to analyse
#********************************************
#############################################

## Choose region to analyze:
#---------------------------
#region = "FG"
#region = "PE"
region = "Both" 

## Choose if removing or not the SF plots:
#-----------------------------------------
#remove.sf = 1 # yes
remove.sf = 0 # no

## Choose the min of Nr of individuals per species:
#--------------------------------------------------
Nsp <- 3 

## Choose taxonomic level to analyze:
#------------------------------------
tax = "species"
#tax = "genus"
#tax = "family"

## Choose if weighting trees occurrences by their basal area or not:
#-------------------------------------------------------------------
#basal.area = 0 # no
basal.area = 1 # yes

## Choose the min circumference for trees:
#-----------------------------------------
CAP.threshold = 30
CAP.threshold/pi


#############################################
#********************************************
## Taxonomic abundance data  
#********************************************
#############################################
list00 <- as.data.frame(read.table("Tree_Inventory_Data.txt",h=T))
head(list00);dim(list00)

## Filter out small individuals:
list0 <- list00[list00$CAP>CAP.threshold,]
head(list0) ; dim(list0)

## Retain species with at least Nsp individuals:
namesp1 <- names(which(table(list0$species)>=Nsp))
list0   <- list0[list0$species%in%namesp1,]
head(list0) ; dim(list0)

## Select region to analyse:
#---------------------------
if(region=="FG")  { list <- list0[list0$Country%in%"FG",] }
if(region=="PE")  { list <- list0[list0$Country%in%"PE",] }
if(region=="Both"){ list <- list0 }

#head(list);dim(list)
#str(list)
#plot(density(as.numeric(log(list$CAP+1)),na.rm=TRUE))

#------------------------
## Species abundance data
#------------------------
if(tax=="species"){
nrspec <- length(table(list$species)) ; nrspec
namesp <- names (table(list$species)) ; namesp
nrplot <- length(table(list$Plot)) ; nrplot
namepl <- names (table(list$Plot)) ; namepl
TS <- as.data.frame(matrix(0,ncol=nrspec,nrow=nrplot))
colnames(TS) <- namesp ; rownames(TS) <- namepl
ST <- TS
for(i in 1:nrplot){
    sub <- list[list$Plot%in%namepl[i],]
    for(j in 1:nrspec){
       if(namesp[j]%in%sub$species){
          sub2    <- sub[sub$species%in%namesp[j],]
          TS[i,j] <- nrow(sub2)
          ST[i,j] <- log(sum(pi*((sub2$CAP/pi)/2)^2))
       }
    }
}
TS <- TS[ rowSums(TS)>=Nsp,] ; TS <- TS[,colSums(TS)>=Nsp,]
TS <- TS[ rowSums(TS)>=Nsp,] ; TS <- TS[,colSums(TS)>=Nsp,] 
}

if(basal.area==1){
  TS <- ST
  TS <- TS[ rowSums(TS)>0,] ; TS <- TS[,colSums(TS)>0,]
  TS <- TS[ rowSums(TS)>0,] ; TS <- TS[,colSums(TS)>0,] 
}

#----------------------
## Genus abundance data
#----------------------
if(tax=="genus"){
nrgen   <- length(table(list$genus)) ; nrgen
namegen <- names (table(list$genus)) ; namegen
TS <- as.data.frame(matrix(0,ncol=nrgen,nrow=nrplot))
colnames(TS) <- namegen ; rownames(TS) <- namepl
for(i in 1:nrplot){
    sub <- list[list$Plot%in%namepl[i],]
    for(j in 1:nrgen){
       if(namegen[j]%in%sub$genus){
          TS[i,j] <- nrow(sub[sub$genus%in%namegen[j],])
       }
    }
}
TS <- TS[ rowSums(TS)>=Nsp,] ; TS <- TS[,colSums(TS)>=Nsp,]
TS <- TS[ rowSums(TS)>=Nsp,] ; TS <- TS[,colSums(TS)>=Nsp,] 
}

#-----------------------
## Family abundance data
#-----------------------
if(tax=="family"){
nrfam   <- length(table(list$family)) ; nrfam
namefam <- names (table(list$family)) ; namefam
TS <- as.data.frame(matrix(0,ncol=nrfam,nrow=nrplot))
colnames(TS) <- namefam ; rownames(TS) <- namepl
for(i in 1:nrplot){
    sub <- list[list$Plot%in%namepl[i],]
    for(j in 1:nrfam){
       if(namefam[j]%in%sub$family){
          TS[i,j] <- nrow(sub[sub$family%in%namefam[j],])
       }
    }
}
TS <- TS[ rowSums(TS)>=Nsp,] ; TS <- TS[,colSums(TS)>=Nsp,]
TS <- TS[ rowSums(TS)>=Nsp,] ; TS <- TS[,colSums(TS)>=Nsp,] 
}
#head(TS[,1:10]);dim(TS)

#################################################################
#****************************************************************
## Soil variables | Plot Coordinates | Habitats | Sites | Regions
#****************************************************************
#################################################################
E <- as.data.frame(read.table("Soil_Data.txt",h=T,row.names=1)) 
head(E);dim(E)

rownames(E)%in%rownames(TS)

C <- E[,1:2] ## plot spatial coordinates
S <- as.matrix(E$Site);rownames(S)<-rownames(C) ## Nominal variable identifying sites
H <- as.matrix(E$Hab);rownames(H)<-rownames(C) ## Nominal variable identifying habitats

E <- E[,-c(1:4)]
head(E);dim(E)

## Normalising soil variables:
#-----------------------------
E.untransf <- E ## to store untransformed env. data
lambda     <- as.data.frame(matrix(ncol=ncol(E),nrow=1)) ; colnames(lambda) <- colnames(E)

for(i in 1:ncol(E)){
  lambda[1,i]  <- BoxCox.lambda(E[,i]+abs(min(E[,i],na.rm=TRUE))+0.00000000000000000000001, 
                                method = "loglik", lower = -2,  upper = 2)
  E[,i] <- BoxCox(E[,i], lambda=lambda[1,i])
}
## Verification that the back normalisation is able to retrieve the original values:
E.untransf[1:10,1] ## untransformed data
E[1:10,1] ## normalised data
INV <- InvBoxCox(E[,1], lambda[1,1], biasadj = FALSE, fvar = NULL)
INV[1:10]  ## should be = to untransformed data in traits2

head(E);dim(E)

## To visualise data distribution:
#---------------------------------
par(mfrow=c(5,5),mex=0.3)
for(j in 1:ncol(E)){hist(E[,j],col="blue",main = colnames(E)[j])}
par(mfrow=c(1,1))

## Standardising variables (z-score transformation):
#---------------------------------------------------
sd.E <- c() ; for(i in 1:ncol(E)){ sd.E <- c(sd.E,sd(E[,i],na.rm=TRUE)) }
means.E <- colMeans(E,na.rm=TRUE)
E <- decostand(E,"standardize",na.rm=TRUE)
dim(E)

INV1 <- as.numeric((E[,1]*sd.E[1])+means.E[1]) ; INV1
INV  <- InvBoxCox(INV1, lambda[1,1], biasadj = FALSE, fvar = NULL) ; INV

INV[1:10] ; E.untransf[1:10,1]

#############################################
#********************************************
## Harmonize lines among data files  
#********************************************
#############################################

TS         <- TS[sort(rownames(TS)),]
TS         <- TS[rownames(TS)%in%rownames(E),]
E          <- E [rownames(E) %in%rownames(TS),]
E.untransf <- E.untransf[rownames(E.untransf) %in%rownames(TS),]
C          <- C [rownames(C)%in%rownames(E),]
H          <- as.matrix(H[rownames(H)%in%rownames(E),])
S          <- as.matrix(S[rownames(S)%in%rownames(E),])

## Hellinger transformation of species abundances:
#-------------------------------------------------
TS <- TS[,rev(order(colSums(TS)))]
TS <- TS[,colSums(TS)>0]
TS.hel <- decostand(TS,"hellinger")

## Check if lines correspond to the same plots among data files:
#---------------------------------------------------------------
rownames(E) ; dim(E)
rownames(TS); dim(TS)
rownames(C) ; dim(C)
rownames(H) ; dim(H)
#rownames(R) ; dim(R)

table(H)

#############################################
#********************************************
## Trait Data
#********************************************
#############################################
TD0 <- as.data.frame(read.table("Trait_Data.txt",h=T)) 
head(TD0);dim(TD0)

head(TD0);dim(TD0)
head(TS);dim(TS)

if(tax=="species") { TT0 <- TD0[TD0$Species%in%colnames(TS),] }
if(tax=="genus")   { TT0 <- TD0[TD0$Genus%in%colnames(TS),]   }
if(tax=="family")  { TT0 <- TD0[TD0$Family%in%colnames(TS),]  }

head(TT0);dim(TT0)

## Harmonize species names in TT and TS:
#---------------------------------------
if(tax=="species"){
TT0 <- TT0[TT0$Species%in%colnames(TS),]
TS <- TS[,which(colnames(TS)%in%TT0$Species),]
TT0 <- TT0[which(TT0$Species%in%colnames(TS)),]
}

TT <- as.data.frame(TT0)
min(colSums(TS))
rownames(TT) <- TT$Species

TS <- TS[,sort(colnames(TS))]
TT <- TT[sort(rownames(TT)),]
head(TT);dim(TT)
head(TS[,1:10]);dim(TS)

taxo <- as.data.frame(matrix(ncol=6,nrow=nrow(TT)))
colnames(taxo) <- c("Species","Genus","Family","order","subclass2","subclass1")
taxo[,1] <- TT[,1] ; taxo[,2] <- TT[,2] ; taxo[,3] <- TT[,3]
taxo[,4] <- TT[,4] ; taxo[,5] <- TT[,5] ; taxo[,6] <- TT[,6]
head(taxo);dim(taxo)

TT <- TT[-c(1:6)]
head(TT) ; dim(TT)
rownames(TT)

## Normalizing traits:
#---------------------
TT2 <- TT
lambda2 <- as.data.frame(matrix(ncol=ncol(TT),nrow=1)) ; colnames(lambda2) <- colnames(TT)
for(i in 1:ncol(TT)){
  lambda2[1,i] <- BoxCox.lambda(TT2[,i]+abs(min(TT2[,i],na.rm=TRUE))+0.00000000000000000000001, 
                                method = "loglik", lower = -2,  upper = 2)
  TT2[,i]     <- BoxCox(TT2[,i], lambda=lambda2[1,i])
}
par(mfrow=c(4,8),mex=0.3)
for(j in 1:ncol(TT2)){hist(TT2[,j],col="red",main = colnames(TT2)[j])}
par(mfrow=c(1,1))

## Standardising variables:
#--------------------------
TT2 <- decostand(TT2,"standardize",na.rm=TRUE)
head(TT2);dim(TT2)
dim(TS);dim(TT)


#############
#************
## Summary ##
#************
#############
head(E)        ; dim(E)
head(TS[,1:5]) ; dim(TS)
head(C)        ; dim(C)
head(H)        ; dim(H)
#head(R)        ; dim(R)
head(TT2)      ; dim(TT2)

rownames(E) ;dim(E)
rownames(TS);dim(TS)
rownames(C) ;dim(C)
rownames(H) ;dim(H)

min(colSums(TS))

##############################################################################
#*****************************************************************************
#####                Matrices for Niche breath analyses                  #####
#*****************************************************************************
##############################################################################

# Env data:
#----------
#E99 <- E.untransf
E99 <- E
colnames(E99)[2] <- "P"
head(E99);dim(E99)

## Choose variables:
sel <- c(1:ncol(E99)) ; colnames(E99)[sel]

pca.soil1 <- as.numeric(rda(E99[,sel])$CA$u[,1])
pca.soil2 <- as.numeric(rda(E99[,sel])$CA$u[,2])

E99 <- cbind(E.untransf,pca.soil1,pca.soil2)
colnames(E99) <- c(colnames(E),"PC1","PC2")
head(E99);dim(E99)

# Species:
#---------
TS99 <- TS[,colSums(TS)>0] 
head(TS99[,1:10]) ; dim(TS99)
# TS99 <- decostand(TS,"hel")
min(colSums(TS99))
TS99 <- ceiling(log(TS99+1))
dim(TS);dim(TS99)
head(TS[,1:10]) ; head(TS99[,1:10])


######################################################
#####         RDA with forward selection         #####
######################################################
## RDA to check with environmental variable is the most determinant on composition
## (Just indicative, not used in the manuscript anymore)

colnames(E)
sel<-c(1:ncol(E))
E4 <- E[,sel] ; colnames(E4)

rda <- rda(TS99,E4) ; rda
fwd <- forward.sel(TS99,E4,alpha=0.05) ; fwd
E2  <- E4[,fwd[,2]] ; head(E2) ; dim(E2)
rda <- rda(TS99,E2) ; rda


#############################################################################
## Matrices O, P and B containing niche optimum, position and breadth values,
## for GRAPHS or Bayesian models without species effect!:
#############################################################################

#use.traits = 0 ## if not using traits in the niche breadth analyses
use.traits = 1 ## if using traits in the niche breadth analyses

for(i in 1:1){

## Matrix T44 of presence-absence of species:
# - - - - - - - - - - - - - - - - - - - - - -
TS44 <- TS99 ; TS44[TS44>0] <- 1

## Select species present in at least 3 plots:
# - - - - - - - - - - - - - - - - - - - - - - 
L   <- which(as.numeric(colSums(TS44))>=3) 
TS05 <- TS99[,L]
# vector to store the Nr of plots in which each species is present:
Nr_plots <- as.data.frame(as.matrix(colSums(TS44[,L])))

## Select species present in at least 2 sites:
# - - - - - - - - - - - - - - - - - - - - - - 
keep <- c()
Nr_sites <- Nr_plots ; Nr_sites[,] <- 0
for(i in 1:ncol(TS05)){
  if(length(table(S[which(TS05[,i]>0),]))>=2){keep<-c(keep,i)}
  # vector to store the Nr of sites in which each species is present:
  Nr_sites[i,] <- length(table(S[which(TS05[,i]>0),]))
}
keep
TS5 <- TS05[,keep]
dim(TS5)

## Ensure species in TS5 match with species in TT2,
## and create TT3 with the same species as in TS5:
# - - - - - - - - - - - - - - - - - - - - - - - - - 
if(use.traits == 1){
  
colnames(TS5)%in%rownames(TT2)
TT3 <- TT2[rownames(TT2)%in%colnames(TS5),] ; dim(TT3)
rownames(TT3)%in%colnames(TS5)
colnames(TS5)%in%rownames(TT3)
dim(TS5);dim(TT3)
colnames(TS5)[53] ; rownames(TT3)[53]

## Vector of colors for traits:
# - - - - - - - - - - - - - - -
colnames(TT3)
q.SLA <- quantile(TT3$SLA,     probs=c(0.15,0.25,0.35,0.45,0.55,0.65,0.75,0.85)) ; q.SLA ; col.sla.opb <- c() ; cex.sla <- c()
q.WSG <- quantile(TT3$WSG, probs=c(0.15,0.25,0.35,0.45,0.55,0.65,0.75,0.85)) ; q.WSG ; col.wsg.opb <- c() ; cex.wsg <- c()
q.SRL <- quantile(TT3$Root_SRL,  probs=c(0.15,0.25,0.35,0.45,0.55,0.65,0.75,0.85)) ; q.SRL ; col.srl.opb <- c() ; cex.srl <- c()

u=1;i=1
for(u in 1:3){  
  if(u==1){TT4 <- TT3$SLA      ; qq <- q.SLA ; col.trait <- c() ; cex.trait <- c()}
  if(u==2){TT4 <- TT3$WSG      ; qq <- q.WSG ; col.trait <- c() ; cex.trait <- c()}
  if(u==3){TT4 <- TT3$Root_SRL ; qq <- q.SRL ; col.trait <- c() ; cex.trait <- c()}
for(i in 1:nrow(TT3)){
  if(TT4[i] <  qq[1])                  { col.trait[i] <- col.alpha("red",0.6)    ; cex.trait[i] <- 3 }
  if(TT4[i] >= qq[1] & TT4[i] < qq[2]) { col.trait[i] <- col.alpha("red",0.5)    ; cex.trait[i] <- 2 }
  if(TT4[i] >= qq[2] & TT4[i] < qq[3]) { col.trait[i] <- col.alpha("red",0.4)    ; cex.trait[i] <- 1.5}
  if(TT4[i] >= qq[3] & TT4[i] < qq[4]) { col.trait[i] <- col.alpha("red",0.3)    ; cex.trait[i] <- 1 }
  if(TT4[i] >= qq[4] & TT4[i] < qq[5]) { col.trait[i] <- col.alpha("purple",0.3) ; cex.trait[i] <- 0.5}
  if(TT4[i] >= qq[5] & TT4[i] < qq[6]) { col.trait[i] <- col.alpha("blue",0.3)   ; cex.trait[i] <- 1 }
  if(TT4[i] >= qq[6] & TT4[i] < qq[7]) { col.trait[i] <- col.alpha("blue",0.4)   ; cex.trait[i] <- 1.5}
  if(TT4[i] >= qq[7] & TT4[i] < qq[8]) { col.trait[i] <- col.alpha("blue",0.5)   ; cex.trait[i] <- 2 }
  if(TT4[i] >= qq[8])                  { col.trait[i] <- col.alpha("blue",0.6)   ; cex.trait[i] <- 3 }
}
  if(u==1){col.sla.opb <- col.trait ; cex.sla.opb <- cex.trait}
  if(u==2){col.wsg.opb <- col.trait ; cex.wsg.opb <- cex.trait}
  if(u==3){col.srl.opb <- col.trait ; cex.srl.opb <- cex.trait}
}

} ## end "if(use.traits == 1){"

## Indval calculation:
# - - - - - - - - - - -
H99 <- H
dim(H99)
H99 <- as.matrix(H99[rownames(H99)%in%rownames(TS5),])
dim(H99)

#A <- as.data.frame(matrix(nrow=nrow(M2),ncol=6))
#colnames(A) <- c("Abund","CLOUD","SF","TF","WS","p.indval")
if(remove.sf == 0){
  A <- as.data.frame(matrix(nrow=ncol(TS5),ncol=5))
  colnames(A) <- c("Abund","SF","TF","WS","p.indval")
  rownames(A) <- c(colnames(TS5))
  head(A);dim(A)
  d1=2;d2=4
}

if(remove.sf == 1){
  A <- as.data.frame(matrix(nrow=ncol(TS5),ncol=4))
  colnames(A) <- c("Abund","TF","WS","p.indval")
  rownames(A) <- c(colnames(TS5))
  head(A);dim(A)
  d1=2;d2=3
}

par(mfrow=c(1,1))
#d1=2;d2=5
plot(0:10,col="white",xlim=c(0,10))
for(i in 1:nrow(A)){ ## long loop (several hours)
  ## Relative abundance
  A[i,1] <- round(sum(TS5[,i]),3)
  ## Fill Indval values in WS, TF and SF if significant:
  groups <- as.factor(H99)
  if(sum(TS5[,i])>0){
    indval.out <- indval(TS5[,i], groups, numitr=5000)
    w1 <- which(colnames(A[,d1:d2])%in%names(indval.out$relfrq))
    A[i,c(d1:d2)[w1]] <- round(indval.out$indval,3)[(d1-1):(d2-1)]
    A[i,(d2+1)]       <- round(indval.out$pval,3)
  }
  points((i/nrow(A))*10,5,pch=15,col=rainbow(nrow(A))[i],cex=1)
}
A
head(A);dim(A)

if(remove.sf==0){
  indval.OPB <- c()
  for(k in 1:nrow(A)){
    if(A[k,5]<=0.05){ 
      w <- which.max(A[k,d1:d2])
      if(w==1){indval.OPB[k]="SF"}
      if(w==2){indval.OPB[k]="TF"}
      if(w==3){indval.OPB[k]="WS"}
    }
    if(A[k,5]>0.05){indval.OPB[k]="G"}
  }
  indval.OPB
}

if(remove.sf==1){
  indval.OPB <- c()
  for(k in 1:nrow(A)){
    if(A[k,4]<=0.05){ 
      w <- which.max(A[k,d1:d2])
      if(w==1){indval.OPB[k]="TF"}
      if(w==2){indval.OPB[k]="WS"}
    }
    if(A[k,4]>0.05){indval.OPB[k]="G"}
  }
  indval.OPB
}

Indval <- as.matrix(indval.OPB) ; rownames(Indval) <- rownames(A)
head(Indval) ; dim(Indval)

## Vector of colors for indval:
# - - - - - - - - - - - - - - -
col.indval.opb <- c()
for(i in 1:length(indval.OPB)){
  if(indval.OPB[i]=="G") {col.indval.opb[i] <- col.alpha("grey50",0.3)   }
  if(indval.OPB[i]=="SF"){col.indval.opb[i] <- col.alpha("blue",0.7)   }
  if(indval.OPB[i]=="TF"){col.indval.opb[i] <- col.alpha("green3",0.7) }
  if(indval.OPB[i]=="WS"){col.indval.opb[i] <- col.alpha("orange3",0.7)}
}
col.indval.opb
plot(1:length(indval.OPB),rep(100,length(indval.OPB)),col=col.indval.opb,cex=1,pch=16)


## Create matrices O (Niche Optimum), P (Niche Position), B (Niche Breadth)
## and Z (R2 quantifying the effect of each soil variable):
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
O <- as.data.frame(matrix(ncol=ncol(E99),nrow=ncol(TS5)))
colnames(O) <- colnames(E99)
rownames(O) <- colnames(TS5)
head(O);dim(O)
B <- O
P <- O
Z <- O
ab <- c()

for(s in 1:nrow(O)){      ## for each species...
  wp <- which(TS5[,s]>0) ## in which plot species "s" is present?
  ab[s] <- sum(TS5[,s])
  for(v in 1:ncol(E99)){  ## for each soil variable...
    rda1 <- rda(TS5[,s],E99[,v])
    Z[s,v] <- RsquareAdj(rda1)$adj.r.squared
    #prda <- 1
    #if(RsquareAdj(rda1)$adj.r.squared>0.001){ prda <- anova.cca(rda1)$Pr[1] }
    #if(prda<=0.05){ Z[s,v] <- RsquareAdj(rda1)$adj.r.squared }
    #if(prda> 0.05){ Z[s,v] <- 0 }
    vec    <- rep(E99[wp,v],TS5[wp,s])
    #vec   <- rep(E99[wp,v],1)
    #Q      <- quantile(vec,probs=c(0,20,50,80,100)/100)
    Q      <- quantile(vec)
    O[s,v] <- median(vec)
    P[s,v] <- median(vec) - median(E99[,v])
    B[s,v] <- abs(Q[4]-Q[2])    
  }
}
head(O);dim(O) ## Niche Optimum  for each species and each environmental variable
head(P);dim(P) ## Niche Position for each species and each environmental variable
head(B);dim(B) ## Niche Breadth  for each species and each environmental variable
head(Z);dim(Z) ## Soil effect  for each species and each environmental variable

O.untransformed <- O
P.untransformed <- P
B.untransformed <- B
Z.untransformed <- Z

## NORMALISATION OF B, P and O   
# - - - - - - - - - - - - - - -
## Normalise B:
B0 <- B
lambda.B <- as.data.frame(matrix(ncol=ncol(B),nrow=1)) ; colnames(lambda.B) <- colnames(B)
for(i in 1:ncol(B)){
  lambda.B[1,i] <- BoxCox.lambda(B[,i]+abs(min(B[,i],na.rm=TRUE))+0.0000000001, 
                               method = "loglik", lower = -1,  upper = 3)
  B[,i]       <- BoxCox(B[,i], lambda=lambda.B[1,i])
}
## Verification that the back normalisation is able to retrieve the original values:
B0[1:10,1] ## untransformed data
B [1:10,1] ## normalised data
INV <- InvBoxCox(B[,1], lambda.B[1,1], biasadj = FALSE, fvar = NULL)
INV[1:10]  ## should be = to untransformed data in traits2

## Normalise O:
O0 <- O
head(O0)
lambda.O <- as.data.frame(matrix(ncol=ncol(O),nrow=1)) ; colnames(lambda.O) <- colnames(O)
for(i in 1:ncol(O)){
  lambda.O[1,i] <- BoxCox.lambda(O[,i]+abs(min(O[,i],na.rm=TRUE))+0.0000000001, 
                                method = "loglik", lower = -1,  upper = 3)
  O[,i]       <- BoxCox(O[,i], lambda=lambda.O[1,i])
}
## Verification that the back normalisation is able to retrieve the original values:
O0[1:10,1] ## untransformed data
O[1:10,1] ## normalised data
INV <- InvBoxCox(O[,1], lambda.O[1,1], biasadj = FALSE, fvar = NULL)
INV[1:10]  ## should be = to untransformed data in traits2

## Normalise P:
P0 <- P
head(P0)
lambda.P <- as.data.frame(matrix(ncol=ncol(P),nrow=1)) ; colnames(lambda.P) <- colnames(P)
for(i in 1:ncol(P)){
  lambda.P[1,i] <- BoxCox.lambda(P[,i]+abs(min(P[,i],na.rm=TRUE))+0.0000000001, 
                                method = "loglik", lower = -1,  upper = 3)
  P[,i]       <- BoxCox(P[,i], lambda=lambda.P[1,i])
}
## Verification that the back normalisation is able to retrieve the original values:
P0[1:10,1] ## untransformed data
P[1:10,1] ## normalised data
INV <- InvBoxCox(P[,1], lambda.P[1,1], biasadj = FALSE, fvar = NULL)
INV[1:10]  ## should be = to untransformed data in traits2

## Normalise Z:
Z0 <- Z
head(Z0)
lambda.Z <- as.data.frame(matrix(ncol=ncol(Z),nrow=1)) ; colnames(lambda.Z) <- colnames(Z)
for(i in 1:ncol(Z)){
  lambda.Z[1,i] <- BoxCox.lambda(Z[,i]+abs(min(Z[,i],na.rm=TRUE))+0.0000000001, 
                                 method = "loglik", lower = -1,  upper = 3)
  Z[,i]       <- BoxCox(Z[,i], lambda=lambda.P[1,i])
}
## Verification that the back normalisation is able to retrieve the original values:
Z0[1:10,1] ## untransformed data
Z[1:10,1] ## normalised data
INV <- InvBoxCox(Z[,1], lambda.Z[1,1], biasadj = FALSE, fvar = NULL)
INV[1:10]  ## should be = to untransformed data in traits2

## Standardisation of B, P and O:
# - - - - - - - - - - - - - - - - 
## Store mean and sd values for back standardisation after imputation:
## (to perform before standardisation!!):
means.B <- colMeans(B,na.rm=TRUE) ; sd.B <- c()
for(s in 1:ncol(B)){sd.B <- c(sd.B,sd(B[,s],na.rm=TRUE))} ; sd.B
B <- decostand(B,"standardize",na.rm=TRUE)
par(mfrow=c(5,5),mex=0.3);for(i in 1:ncol(B)){hist(B[,i],col="red", main=colnames(B)[i])};par(mfrow=c(1,1))

means.O <- colMeans(O,na.rm=TRUE) ; sd.O <- c()
for(s in 1:ncol(O)){sd.O <- c(sd.O,sd(O[,s],na.rm=TRUE))} ; sd.O
O <- decostand(O,"standardize",na.rm=TRUE)
par(mfrow=c(5,5),mex=0.3);for(i in 1:ncol(O)){hist(O[,i],col="red", main=colnames(O)[i])};par(mfrow=c(1,1))

means.P <- colMeans(P,na.rm=TRUE) ; sd.P <- c()
for(s in 1:ncol(P)){sd.P <- c(sd.P,sd(P[,s],na.rm=TRUE))} ; sd.P
P <- decostand(P,"standardize",na.rm=TRUE)
par(mfrow=c(5,5),mex=0.3);for(i in 1:ncol(P)){hist(P[,i],col="red", main=colnames(P)[i])};par(mfrow=c(1,1))

means.Z <- colMeans(Z,na.rm=TRUE) ; sd.Z <- c()
for(s in 1:ncol(Z)){sd.Z <- c(sd.Z,sd(Z[,s],na.rm=TRUE))} ; sd.Z
Z <- decostand(Z,"standardize",na.rm=TRUE)
par(mfrow=c(5,5),mex=0.3);for(i in 1:ncol(Z)){hist(Z[,i],col="red", main=colnames(Z)[i])};par(mfrow=c(1,1))


} ## end "for(i in 1:1){"


head(O);dim(O) ## Niche Optimum  for each species and each environmental variable = "Niche Position" used in the paper
head(P);dim(P) ## Niche Position for each species and each environmental variable ## sensus stricto (not used anymore as results were highly redundant)
head(B);dim(B) ## Niche Breadth  for each species and each environmental variable
head(Z);dim(Z) ## Soil effect    for each species and each environmental variable
ab             ## Total abundance of each species

Nr.plots <- as.data.frame(Nr_plots[which(rownames(Nr_plots)%in%rownames(O)),1]) 
rownames(Nr.plots) <- rownames(O) ; Nr.plots ; nrow(Nr.plots)

Nr.sites <- as.data.frame(Nr_sites[which(rownames(Nr_sites)%in%rownames(O)),1]) 
rownames(Nr.sites) <- rownames(O) ; Nr.sites ; nrow(Nr.sites)

rownames(Nr.sites)[23];rownames(Nr.plots)[23];rownames(O)[23];rownames(Nr.plots)[23];rownames(Nr.sites)[23]

#---------------------------------------------------------------------------------------------------------
## Create matrices of residuals of NB and NP to correct for region, Nr sites and species abundance effects
#---------------------------------------------------------------------------------------------------------

pe <- which(substr(rownames(E),1,2)%in%"PE") ; pe
fg <- c(1:nrow(E))[-pe] ; fg
R <- H ; R[pe] <- "PE" ; R[fg] <- "FG"
reg <- c()
i=1
colnames(TS5)[23];rownames(B)[23]
for(i in 1:nrow(B)){  ## for each species
  w <- which(TS5[,i]>0) ;  tab1 <- table(R[w,])[table(R[w,])>0]
  if(length(tab1)==2){reg <- c(reg,"both")}
  if(length(tab1)==1){reg <- c(reg,names(tab1))}
}
reg
Reg <- to.dummy(reg, "R.") ; head(Reg) ; dim(Reg)

geo1 <- as.data.frame(cbind(as.numeric(ab),as.numeric(Nr.plots[,1]),as.numeric(Nr.sites[,1]),Reg))
colnames(geo1) <- c("ab","Nr.plots","Nr.sites","Both","FG","PE")

#geo1 <- as.data.frame(cbind(as.numeric(ab),as.numeric(Nr.plots[,1]),as.numeric(Nr.sites[,1])))
#colnames(geo1) <- c("ab","Nr.plots","Nr.sites")

head(geo1) ; dim(geo1)
vif <- vifstep(geo1,th=5) ; vif
geo2 <- geo1[,-which(colnames(geo1)%in%vif@excluded)]
head(geo2) ; dim(geo2)

B.resid <- B ; O.resid <- O
i=1
for(i in 1:ncol(B)){
  B.resid[,i] <- as.numeric(residuals(rda(B[,i],cbind(geo2))))
  O.resid[,i] <- as.numeric(residuals(rda(O[,i],cbind(geo2))))
}


## Matrix of delta NB-NP values:
delta <- B.resid-O.resid
head(delta) ; dim(delta) ## negative/positive value could mean that there is more abiot. filtering/comp. excl. ?
range(delta)

#----------
## Summary
#----------
head(E)         ; dim(E)
head(C)         ; dim(C)

head(TT3)       ; dim(TT3)
head(B)         ; dim(B)
head(O)         ; dim(O)
head(B.resid)   ; dim(B.resid)
head(O.resid)   ; dim(O.resid)
head(TS5[,1:5]) ; dim(TS5)

colnames(TS5)[41] ; rownames(B.resid)[41] ; rownames(TT3) [41] ; rownames(delta)[41]

#write.table(cbind(TT3,indval.OPB,geo1),"TT3.xls")

## Calculate the number of species within each edaphic habitat:
#--------------------------------------------------------------
TS.pa <- TS5 ; TS.pa[TS.pa>0] = 1
Nsp.hab <- c()

for(k in 1:3){
  if(k==1){wh <- which(H%in%"SF")}
  if(k==2){wh <- which(H%in%"TF")}
  if(k==3){wh <- which(H%in%"WS")}
  TS100       <- TS.pa[wh,]
  Nsp.hab[k]  <- length(which(colSums(TS100)>0))
}

Nsp.hab

23/147  ## Nr of SF specialists / Nr of species on SF habitat
26/192  ## Nr of TF specialists / Nr of species on TF habitat
37/131  ## Nr of WS specialists / Nr of species on WS habitat


## Mean of traits within each habitat:
#-------------------------------------
S2B.pe <- as.data.frame(t(TT)[,1:6]) ; colnames(S2B.pe) <- c("SF","TF","WS","SD.SF","SD.TF","SD.WS") ; S2B.pe[,] <- 0
head(S2B.pe) ; dim(S2B.pe)
S2B.fg <- S2B.pe
i=1 ; j=1 ; k=1

rownames(TS5) ; rownames(H)

for(e in 1:2){

for(i in 1:3){ ## for each habitat
  
  if(e==1){
    if(i==1){w <- which(H[,1]%in%"SF" & R[,1]%in%"FG") ; sd1 = 4}
    if(i==2){w <- which(H[,1]%in%"TF" & R[,1]%in%"FG") ; sd1 = 5}
    if(i==3){w <- which(H[,1]%in%"WS" & R[,1]%in%"FG") ; sd1 = 6}
  }
  if(e==2){
    if(i==1){w <- which(H[,1]%in%"SF" & R[,1]%in%"PE") ; sd1 = 4}
    if(i==2){w <- which(H[,1]%in%"TF" & R[,1]%in%"PE") ; sd1 = 5}
    if(i==3){w <- which(H[,1]%in%"WS" & R[,1]%in%"PE") ; sd1 = 6}
  }
  
  #TS9 <- ceiling(log(TS[w,]+1)) ; dim(TS9)
  TS9 <- ceiling(log(TS5[w,]+1)) ; dim(TS9)
  
  for(k in 1:14){     ## for each trait...
  vec <- c()
     
  for(j in 1:ncol(TS9)){ ## for each species
     w2  <- which(rownames(TT)%in%colnames(TS9)[j])
     vec <- c(vec,rep(as.numeric(TT[w2,k]),sum(TS9[,j])))
  }
  
  if(e==1){
    S2B.fg[k,i]   <- mean(vec)
    S2B.fg[k,sd1] <- sd(vec)
  }
  
  if(e==2){
    S2B.pe[k,i]   <- mean(vec)
    S2B.pe[k,sd1] <- sd(vec)
  }

  }
  
}

}

S2B.fg
S2B.pe

#---------------------------
## Boxplots of E ~ habitats:
#---------------------------
dim(H)
dim(E.untransf)

sd(E.untransf$TN[which(H[,1]%in%"SF")])
sd(E.untransf$AP[which(H[,1]%in%"SF")])
sd(E.untransf$Ca[which(H[,1]%in%"SF")])
sd(E.untransf$Mg[which(H[,1]%in%"SF")])
sd(E.untransf$K [which(H[,1]%in%"SF")])

par(mfrow=c(3,2),mex=0.5)
i=1

table(H[,1])

for(i in 1:5){

if(i==1) { E9 <- log(E.untransf$TN+1) ; titl = "Soil N" }
if(i==2) { E9 <- log(E.untransf$AP+1) ; titl = "Soil P" }
if(i==3) { E9 <- log(E.untransf$Ca+1) ; titl = "Soil Ca" }
if(i==4) { E9 <- log(E.untransf$Mg+1) ; titl = "Soil Mg" }
if(i==5) { E9 <- log(E.untransf$K+1)  ; titl = "Soil K" }

habs   <- as.character(H[,1]) ; habs

q1     <- quantile(E9,probs=c(0.01,0.99))
w1.low <- which(E9<q1[1]) ; w1.up <- which(E9>q1[2])
rem    <- c(w1.low,w1.up) 
if(length(rem)>0){E9 <- E9[-rem] ; habs <- habs[-rem] } 
  
boxplot(E9~habs,col=c("blue","green","orange"),main=titl,cex.main=2,ylab="",xlab="")

TukeyHSD(aov(E9~habs))

}

par(mfrow=c(1,1))



#############################################################################
##       DATA ANALYSIS       ##
#############################################################################

## Select traits:
#---------------
colnames(TT3)
sel <- c(1:ncol(TT3))
TT8 <- cbind(TT3[,sel])
head(TT8);dim(TT8)
rownames(TT8)[43];rownames(B)[43]

pca.traits <- rda(TT8) 
pca.traits$CA$eig/sum(pca.traits$CA$eig)*100
RsquareAdj(pca.traits)

TT8.PC <- as.data.frame(rda(TT8)$CA$u[,1:2]) ; head(TT8.PC) ; dim(TT8.PC)
round(cor(TT8,TT8.PC),2)

TT9    <- cbind(TT8,TT8.PC) ; head(TT9) ; dim(TT9)
TT9    <- TT8 ; head(TT9) ; dim(TT9)

vif <- vifstep(TT9,th=5) ; vif
TT99 <- TT9
#TT99 <- TT9[,-which(colnames(TT9)%in%vif@excluded)]
head(TT99) ; dim(TT99)

## Model taxonomic information using dummy variables:
#----------------------------------------------------
spec <- as.character(colnames(TS5)) ; spec <- str_replace(spec, "_", " ")
gens <- c() ; fams <- c()
for(i in 1:length(spec)){
  w    <- which(list$species%in%colnames(TS5)[i])[1]
  gens <- c(gens,as.character(list$genus[w]))
  fams <- c(fams,as.character(list$family[w]))
}
## X.fam:
library(varhandle)
fam.dummy <- to.dummy(fams, "F.") ; head(fam.dummy) ; dim(fam.dummy)
pca.fam   <- rda(fam.dummy)
eigs      <- pca.fam$CA$eig/sum(pca.fam$CA$eig)*100
L         <- length(which(cumsum(eigs)<90)) ; L
X.fam     <- pca.fam$CA$u[,1:L] ; dim(X.fam)
colnames(X.fam) <- paste0("PCF.",c(1:ncol(X.fam)))
head(X.fam)
## X.gen:
gen.dummy <- to.dummy(gens, "G.") ; head(gen.dummy) ; dim(gen.dummy)
pca.gen   <- rda(gen.dummy)
eigs      <- pca.gen$CA$eig/sum(pca.gen$CA$eig)*100 ; eigs
L         <- length(which(cumsum(eigs)<90)) ; L
X.gen     <- pca.gen$CA$u[,1:L] ; dim(X.gen)
colnames(X.gen) <- paste0("PCG.",c(1:ncol(X.gen)))
head(X.gen)
# Phylo:
phylo.0 <- cbind(X.fam,X.gen) ; dim(phylo.0)
vif     <- vifstep(phylo.0) ; vif
w1      <- which(colnames(phylo.0)%in%vif@excluded) ; w1
phylo   <- phylo.0[,-w1]
head(phylo);dim(phylo)


#----------------
## Plot NB vs NP:
#----------------
## If using neutral colours:
col1 <- rep(col.alpha("black",0.5),nrow(B.resid))
cex1 <- rep(2.2,nrow(B.resid))

## If coloring points according to indval:
col1 <- col.indval.opb

## If using symbols according to indval:
symb <- c()
i=1
for(i in 1:length(indval.OPB)){
  if(indval.OPB[i]=="G") {symb[i]<-1}
  if(indval.OPB[i]=="WS"){symb[i]<-15}
  if(indval.OPB[i]=="TF"){symb[i]<-16}
  if(indval.OPB[i]=="SF"){symb[i]<-17}
}
symb

## If coloring points according to a trait:
# - - - - - - - - - - - - - - - - - - - - -

## for soil N, Ca and P content

for(k in 1:3){

if(k==1){ trait1 <- TT99$SLA    }
if(k==2){ trait1 <- TT99$Leaf_N }
if(k==3){ trait1 <- TT99$WSG    }
#trait1 <- TT99$PC1
#trait1 <- TT99$PC2

q1  <- quantile(trait1, probs=c(0.15,0.25,0.35,0.45,0.55,0.65,0.75,0.85)) ; q1 

TT4 <- trait1 ; qq <- q1 ; col.trait <- c() ; cex.trait <- c()
for(i in 1:nrow(TT3)){
    if(TT4[i] <  qq[1])                  { col.trait[i] <- col.alpha("red",0.6)    ; cex.trait[i] <- 3 }
    if(TT4[i] >= qq[1] & TT4[i] < qq[2]) { col.trait[i] <- col.alpha("red",0.5)    ; cex.trait[i] <- 2 }
    if(TT4[i] >= qq[2] & TT4[i] < qq[3]) { col.trait[i] <- col.alpha("red",0.4)    ; cex.trait[i] <- 1.5}
    if(TT4[i] >= qq[3] & TT4[i] < qq[4]) { col.trait[i] <- col.alpha("red",0.3)    ; cex.trait[i] <- 1 }
    if(TT4[i] >= qq[4] & TT4[i] < qq[5]) { col.trait[i] <- col.alpha("purple",0.3) ; cex.trait[i] <- 0.5}
    if(TT4[i] >= qq[5] & TT4[i] < qq[6]) { col.trait[i] <- col.alpha("blue",0.3)   ; cex.trait[i] <- 1 }
    if(TT4[i] >= qq[6] & TT4[i] < qq[7]) { col.trait[i] <- col.alpha("blue",0.4)   ; cex.trait[i] <- 1.5}
    if(TT4[i] >= qq[7] & TT4[i] < qq[8]) { col.trait[i] <- col.alpha("blue",0.5)   ; cex.trait[i] <- 2 }
    if(TT4[i] >= qq[8])                  { col.trait[i] <- col.alpha("blue",0.6)   ; cex.trait[i] <- 3 }
}

if(k==1){col1 <- col.trait ; cex1 <- cex.trait}
if(k==2){col2 <- col.trait ; cex2 <- cex.trait}
if(k==3){col3 <- col.trait ; cex3 <- cex.trait}


}

plot(trait1,pch=16,col=col.trait,cex=cex.trait)

## for soil K content
col4 <- rep("grey30",nrow(TT9)) ; cex4 <- rep(2,nrow(TT9)) 


## Plot NB ~NP:
#--------------
#par(mfrow=c(4,2),mex=0.3)

par(mfrow=c(1,1),mex=1)

sel=c(1:1) ; sel ## Soil N
sel=c(3:3) ; sel ## Soil Ca
sel=c(5:5) ; sel ## Soil P
sel=c(7:7) ; sel ## Soil K

quantile(E.untransf$AP,probs=c(0.025,0.975))

for(i in sel){

if(i==1){ Y.vp   <- B.resid$TN  ; X.vp <- O.resid$TN ; titl <- "Soil N"  }
if(i==3){ Y.vp   <- B.resid$Ca  ; X.vp <- O.resid$Ca ; titl <- "Soil Ca" }
if(i==5){ Y.vp   <- B.resid$AP  ; X.vp <- O.resid$AP ; titl <- "Soil P"  }
if(i==7){ Y.vp   <- B.resid$K   ; X.vp <- O.resid$K  ; titl <- "Soil K"  }

## if(i==3){ Y.vp   <- B.resid$Mg  ; X.vp <- O.resid$Mg ; titl <- "Soil Mg" }

if(i %in% c(1,3,5,7)){  
  
V <- as.data.frame(cbind(Y.vp,X.vp,TT9,phylo))
colnames(V)[1:2] <- c("B","O")
head(V);dim(V)

if(i==1) { col9 <- col1 ; cex9 <- cex1 ; symb9 <- symb ; j=1 ; colnames(B)[j]}
if(i==3) { col9 <- col2 ; cex9 <- cex2 ; symb9 <- symb ; j=3 ; colnames(B)[j]}
if(i==5) { col9 <- col3 ; cex9 <- cex3 ; symb9 <- symb ; j=2 ; colnames(B)[j]}
if(i==7) { col9 <- col4 ; cex9 <- cex4 ; symb9 <- symb ; j=5 ; colnames(B)[j]}

q1     <- quantile(V$B,probs=c(0.025,0.975))
w1.low <- which(V$B<q1[1]) ; w1.up <- which(V$B>q1[2])
rem    <- c(w1.low,w1.up) 
if(length(rem)>0){V <- V[-rem,] ; col9 <- col9[-rem] ; cex9 <- cex9[-rem] ; symb9 <- symb9[-rem] }

q1     <- quantile(V$O,probs=c(0.025,0.975))
w1.low <- which(V$O<q1[1]) ; w1.up <- which(V$O>q1[2])
rem <- c(w1.low,w1.up) 
if(length(rem)>0){V <- V[-rem,] ; col9 <- col9[-rem] ; cex9 <- cex9[-rem] ; symb9 <- symb9[-rem] }

head(V) ; dim(V)  
#plot(V$B)
#plot(V$O)
#plot(V$B~V$O);cor(V$B,V$O)

#head(E.untransf)
#quantile(E.untransf$TN,probs=c(0.025,0.975))
#j=1;colnames(B)[j]
#InvBoxCox(at*sd.O[j]+means.O[j], lambda.O[1,j], biasadj = FALSE, fvar = NULL)+abs(min(E.untransf[,j],na.rm=TRUE))

V$O2 <- V$O^2
  bm1 <- quap(
    alist(
      B ~ dnorm(mu, sigma),
      mu ~ a + b1*O + b2*O^2,
      a   ~ dnorm(0,1),b1~dlnorm(0,1),b2~dnorm (0,1),sigma ~ dunif (0,50)
    ), data = V )
precis(bm1,prob=0.95)

weight.seq <- seq( from=min(V$O) , to=max(V$O) , length.out=20 ) 
pred_dat <- list( O=weight.seq , O2=weight.seq^2 )
  
mu <- link( bm1 , data=pred_dat ) ; mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 ) ; sim.height <- sim( bm1 , data=pred_dat )
height.PI <- apply( sim.height , 2 , PI , prob=0.90 )

#data3 <- as.data.frame(cbind(B2[,j],U2[,j])); colnames(data3) <- c("ME","IQS")
#nlsfit <- nlsfit(data3, model=2)

plot(B ~ O, V, col=col9,xlab="",ylab="", pch = symb9, cex=cex9, cex.axis=0.0001)
lines( weight.seq , mu.mean ) ; shade( mu.PI , weight.seq ) ; shade( height.PI , weight.seq )

at     <- c(-3,-2,-1,0,1,2,3)

INV1 <- as.numeric((at*sd.O[j])+means.O[j]) ; INV1
INVX  <- InvBoxCox(INV1, lambda.O[1,j], biasadj = FALSE, fvar = NULL) ; INVX
axis( side=1 , at=at , labels=round(INVX,2), lty = 1, lwd = 1, cex.axis=1.7)
#labels <- InvBoxCox(at*sd.O[j]+means.O[j], lambda.O[1,j], biasadj = FALSE, fvar = NULL)

INV2 <- as.numeric((at*sd.B[j])+means.B[j]) ; INV2
INVY  <- InvBoxCox(INV2, lambda.B[1,j], biasadj = FALSE, fvar = NULL) ; INVY
axis( side=2 , at=at , labels=round(INVY,2), lty = 1, lwd = 1 , cex.axis=1.7)
#labels <- InvBoxCox(at*sd.O[j]+means.O[j], lambda.O[1,j], biasadj = FALSE, fvar = NULL)

#par(mfrow=c(1,1))
#boxplot(E.untransf$Ca)

}

if(i%in%c(2,4,6,8)){ plot(1,1,col="white") }

}

RsquareAdj(rda(V$B,V$O))

## VP on the side of the NB-NP graph:
#------------------------------------
Y.vp <- B.resid$TN  ; X.vp <- O.resid$TN ; titl <- "Soil N"  
Y.vp <- B.resid$Ca  ; X.vp <- O.resid$Ca ; titl <- "Soil Ca" 
Y.vp <- B.resid$AP  ; X.vp <- O.resid$AP ; titl <- "Soil P"  
Y.vp <- B.resid$K   ; X.vp <- O.resid$K  ; titl <- "Soil K"  

V <- as.data.frame(cbind(Y.vp,X.vp,TT99,phylo))
colnames(V)[1:2] <- c("B","O")
head(V);dim(V)

colnames(V)
traits88 <- V[,c(3:9,11:16)] ; head(traits88) ; dim(traits88)

anova.cca(rda(V$B,traits88))
fwd.TT <- forward.sel(V$B,traits88,alpha=0.05) ; fwd.TT
TT.vp  <- traits88[,fwd.TT[,2]] 
head(TT.vp) ; dim(TT.vp)

coco <- rep("grey60",ncol(traits88))
coco[fwd.TT[,2]] <- "black"
coco
cors <- cor(V$B,traits88) ; cors
barplot(as.vector(cors),col=coco,
        names.arg=colnames(traits88),cex.names=1.5)

# if using PCA axes for traits
TT.vp <- TT9$PC1
TT.vp <- TT9$PC2

vp <- varpart(V$B, V$O, TT.vp) ; vp ; plot(vp)
cor(V$B,TT.vp)
cor(V$O,TT.vp)


## Test of the phylogenetic signal:
#----------------------------------
colnames(V)
phylo88 <- V[,17:(ncol(V)-1)] ; head(phylo88) ; dim(phylo88)

## effect on NB:
RsquareAdj(rda(V$B,phylo88))
anova.cca(rda(V$B,phylo88))
fwd.phy  <- forward.sel(V$B,phylo88) ; fwd.phy
phylo.vp <- phylo88[,fwd.phy[,2]]
head(phylo.vp) ; dim(phylo.vp)

## effect on NP:
RsquareAdj(rda(V$O,phylo88))
anova.cca(rda(V$O,phylo88))
fwd.phy  <- forward.sel(V$O,phylo88) ; fwd.phy
phylo.vp <- phylo88[,fwd.phy[,2]]
head(phylo.vp) ; dim(phylo.vp)

## VP with phylo effect
vp <- varpart(V$B, V$O, phylo.vp) ; vp ; plot(vp)

vp <- varpart(V$B, V$O, TT.vp, phylo.vp) ; vp ; plot(vp)


#------------------------------------------------------------------
## APPENDIX S6: Boxplots comparing NB and NP among guilds:
#------------------------------------------------------------------

## Comparing NB and NP among specialists and generalists:
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
par(mfrow=c(5,2),mex=0.4)
i=5

B13 = B        ; O13 = O
B13 = B.resid  ; O13 = O.resid

for(i in 1:5){

if(i==1) { Y.vp <- B13$TN  ; X.vp <- O13$TN ; titl = "Soil N"}
if(i==2) { Y.vp <- B13$AP  ; X.vp <- O13$AP ; titl = "Soil P"}
if(i==3) { Y.vp <- B13$Ca  ; X.vp <- O13$Ca ; titl = "Soil Ca"}
if(i==4) { Y.vp <- B13$Mg  ; X.vp <- O13$Mg ; titl = "Soil Mg"}
if(i==5) { Y.vp <- B13$K   ; X.vp <- O13$K  ; titl = "Soil K"}

#colnames(B)
#j=c(8,10:13)[i] ; colnames(B)[j]

head(V);dim(V)

V           <- as.data.frame(cbind(Y.vp,X.vp))
colnames(V) <- c("B","O")
guilds      <- indval.OPB ; guilds
#Tr0         <- TT9$Leaf_N

q1     <- quantile(V$B,probs=c(0.025,0.975))
w1.low <- which(V$B<q1[1]) ; w1.up <- which(V$B>q1[2])
rem    <- c(w1.low,w1.up) 
if(length(rem)>0){V <- V[-rem,] ; guilds <- guilds[-rem] } # Tr0 <- Tr0[-rem]}

q1     <- quantile(V$O,probs=c(0.025,0.975))
w1.low <- which(V$O<q1[1]) ; w1.up <- which(V$O>q1[2])
rem <- c(w1.low,w1.up) 
if(length(rem)>0){V <- V[-rem,] ; guilds <- guilds[-rem] } # Tr0 <- Tr0[-rem]}

boxplot(V$B~guilds,col=c("grey","blue","green","orange"),main=paste0("NB : ",titl),ylab="",xlab="")
boxplot(V$O~guilds,col=c("grey","blue","green","orange"),main=paste0("NP : ",titl),ylab="",xlab="")

TukeyHSD(aov(V$B~guilds))
TukeyHSD(aov(V$O~guilds))

}

par(mfrow=c(1,1))


## Comparing traits among specialists and generalists:
# - - - - - - - - - - - - - - - - - - - - - - - - - - -
guilds <- indval.OPB ; guilds
head(TT9);dim(TT9);length(guilds)

par(mfrow=c(5,3),mex=0.4)
i=1

for(i in 1:14){
  
  guilds      <- indval.OPB ; guilds
  T10         <- TT9[,i]
  titl        <- colnames(TT9)[i] ; titl
  
  q1     <- quantile(T10,probs=c(0.025,0.975))
  w1.low <- which(T10<q1[1]) ; w1.up <- which(T10>q1[2])
  rem    <- c(w1.low,w1.up) 
  if(length(rem)>0){ T11 <- T10[-rem] ; guilds <- guilds[-rem] }

  boxplot(T11~guilds,col=c("grey","blue","green","orange"),main=titl,ylab="",xlab="")

}

par(mfrow=c(1,1))


## Tukey tests:
i=3
guilds      <- indval.OPB ; guilds
T10         <- TT9[,i]
titl        <- colnames(TT9)[i] ; titl

q1     <- quantile(T10,probs=c(0.025,0.975))
w1.low <- which(T10<q1[1]) ; w1.up <- which(T10>q1[2])
rem    <- c(w1.low,w1.up) 
if(length(rem)>0){ T11 <- T10[-rem] ; guilds <- guilds[-rem] }

TukeyHSD(aov(T11~guilds))


#----------------------------------------------------------------------------
## Respond to Reviewer 1's main comment Nr 2 regarding the NB of generalists:
#----------------------------------------------------------------------------
guilds      <- indval.OPB ; guilds

T10 <- ab                       ## if plotting graph to compare abundance/species among guilds
T10 <- as.numeric(Nr.plots[,1]) ## if plotting graph to compare Nr plots/species among guilds

T11 <- T10

q1     <- quantile(T10,probs=c(0.025,0.975))
w1.low <- which(T10<q1[1]) ; w1.up <- which(T10>q1[2])
rem    <- c(w1.low,w1.up) 
if(length(rem)>0){ T11 <- T10[-rem] ; guilds <- guilds[-rem] }

boxplot(T11~guilds,col=c("grey","blue","green","orange"),ylab="",xlab="")

TukeyHSD(aov(T11~guilds))
wilcox.test(T11[which(indval.OPB%in%"G")],T11[which(indval.OPB%in%"SF")])

mean(T10[which(indval.OPB%in%"G")])  ; length(which(indval.OPB%in%"G"))
mean(T10[which(indval.OPB%in%"SF")]) ; length(which(indval.OPB%in%"SF"))
mean(T10[which(indval.OPB%in%"TF")]) ; length(which(indval.OPB%in%"TF"))
mean(T10[which(indval.OPB%in%"WS")]) ; length(which(indval.OPB%in%"WS"))



#-----------------------------
## PCAs with varimax rotation:
#-----------------------------
library(pracma)

colnames(TT8)
Z <- TT8

colnames(B.resid)
Z <- B.resid[,c(6,9:11,8)]
Z <- O.resid[,c(6,9:11,8)]
colnames(Z)[c(1,5)] <- c("N","P")

colnames(E)
Z <- E[,c(6,8:11)]
Z <- E[,c(4:11)]

head(Z);dim(Z)
cor(Z)


protest(rda(B.resid[,c(6,9:11,8)]),rda(O.resid[,c(6,9:11,8)]))


a=1;b=2 ## To plot axes 1 and 2
#a=3;b=4 ## To plot axes 3 and 4

par(mfrow=c(1,1),mex=0.6)
pca.env = rda(Z, scale=T)
pca.env$CA$eig/sum(pca.env$CA$eig)*100
loading = vegan::scores(pca.env, choices=c(a:b))$species    #choices determines which pc to be taken
rloading = varimax(loading)$loadings
iloading = t(pinv(rloading));iloading
scores = scale(Z) %*% iloading
r = range(c(rloading, scores))
plot(scores, xlim = c(-2,2), ylim= c(-1.8,2), xlab= "PC1 ", ylab= "PC2 ",col="white")
abline(h=0,v=0,lty=2,col="grey60")
arrows(0,0, rloading[,1]*1, rloading[,2]*1, length=0.1, col="black")
text(rloading[,1], rloading[,2], labels = colnames(Z), cex=1, pos=3, col="black")

mean  (c(1,2,1,2,2,2,2))
median(c(1,2,1,2,2,2,2))

cor(pca.env$CA$u[,1:2],Z)

EIG <- pca.env$CA$eig/sum(pca.env$CA$eig)*100
barplot(EIG[1:8],col=c("black","black",rep("grey",6)))

## colour species/plots according to q1:
q1 <- B.resid$TN
q1 <- B.resid$AP
q1 <- B.resid$Ca
q1 <- B.resid$Mg
q1 <- B.resid$K
q1 <- TT9$L_N

qq  <- quantile(q1,  probs=c(0.15,0.25,0.35,0.45,0.55,0.65,0.75,0.85)) ; qq 
TT4 <- q1 ; col.trait <- c() 
cex1 <- c(2.5,2,1.5,1,0.5,1,1.5,2,2.5) ## if coloring with NB values
for(i in 1:nrow(Z)){
  if(TT4[i] <  qq[1])                  { col.trait[i] <- col.alpha("red",0.6)    ; cex.trait[i] <- cex1[1] }
  if(TT4[i] >= qq[1] & TT4[i] < qq[2]) { col.trait[i] <- col.alpha("red",0.5)    ; cex.trait[i] <- cex1[2] }
  if(TT4[i] >= qq[2] & TT4[i] < qq[3]) { col.trait[i] <- col.alpha("red",0.4)    ; cex.trait[i] <- cex1[3] }
  if(TT4[i] >= qq[3] & TT4[i] < qq[4]) { col.trait[i] <- col.alpha("red",0.3)    ; cex.trait[i] <- cex1[4] }
  if(TT4[i] >= qq[4] & TT4[i] < qq[5]) { col.trait[i] <- col.alpha("purple",0.3) ; cex.trait[i] <- cex1[5] }
  if(TT4[i] >= qq[5] & TT4[i] < qq[6]) { col.trait[i] <- col.alpha("blue",0.3)   ; cex.trait[i] <- cex1[6] }
  if(TT4[i] >= qq[6] & TT4[i] < qq[7]) { col.trait[i] <- col.alpha("blue",0.4)   ; cex.trait[i] <- cex1[7] }
  if(TT4[i] >= qq[7] & TT4[i] < qq[8]) { col.trait[i] <- col.alpha("blue",0.5)   ; cex.trait[i] <- cex1[8] }
  if(TT4[i] >= qq[8])                  { col.trait[i] <- col.alpha("blue",0.6)   ; cex.trait[i] <- cex1[9] }
}
col9 <- col.trait ; cex9 <- cex.trait
symb <- 16

## If using symbols according to indval:
symb <- c()
i=1
for(i in 1:length(indval.OPB)){
  if(indval.OPB[i]=="G") {symb[i]<-1}
  if(indval.OPB[i]=="WS"){symb[i]<-15}
  if(indval.OPB[i]=="TF"){symb[i]<-16}
  if(indval.OPB[i]=="SF"){symb[i]<-17}
}
symb


## plot PCA with colours
plot(scores, xlim = c(-2,3), ylim= c(-3.2,2.5), xlab= "PC1 ", ylab= "PC2 ",col="white")
arrows(0,0, rloading[,1]*1, rloading[,2]*1, length=0.1, col="black")
text(rloading[,1], rloading[,2], labels = colnames(Z), cex=1.3, pos=3, col="black")
abline(h=0,v=0,lty=2,col="grey60")
points(scores*1.5,col=col9,cex=cex.trait,pch=symb)

cor(scores,q1)

## PCA with ggplot:
#------------------

## With ggplot (2):
SH  <- cbind(R,H) ## matrix with region + habitat
pca <- rda(Z)

G <- as.data.frame(cbind(as.data.frame(pca$CA$u[,1:2]),SH))
G[,1] <- as.numeric(G[,1]) ; G[,2] <- as.numeric(G[,2])
colnames(G) <- c("PC1","PC2","Region","Habitat")
G[,3] <- as.character(G[,3])
G[,4] <- as.character(G[,4])
G <- as.data.frame(G)
head(G) 
str(G)

my_points_colors <- c("#004573","#ff9137") ## check on i want hue
## https://medialab.github.io/iwanthue/
gray <- gray.colors(50, start = 0.9, end = 0.3, gamma = 2.2, alpha = NULL)

plot(1,1,pch=17,cex=5)

p <-ggplot(data = G,
           aes(x = PC1,
               y = PC2,
               color = Region,
               shape = Habitat)) +
  geom_point(size = 5.5) +
  scale_color_manual(values = my_points_colors) +
  scale_shape_manual(values=c(8, 1, 17))+
  theme(axis.title = element_text(size=14),
        legend.position="bottom")

p + stat_ellipse(type = "norm", linetype = 1, aes(color=Region, group=Region))



