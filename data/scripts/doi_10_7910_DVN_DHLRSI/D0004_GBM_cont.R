# --------------------------
# GBM rerun -- subject info
# --------------------------

setwd("/pine/scr/m/e/meichen/hovestdta/data")
library(JGL)
library(HDtest)
library(equSA)
library(pheatmap)
library(network)
library(ggnet)
library(ggplot2)
library(reshape2)
library(umap)
library(huge)
 
load("B0004V2_GBM_data.rda")


source("../FUNCTIONS.R")


nrep = 20
mask.rate = 0.15

if ("simugroup" == "AC"){
  implist.simugroup <- JGNsc.cont2(y = matlist[[1]], min.cell = 20, warm = 50, iter = 50, dropThreshold = 0.65)
  observed.match <- matlist[[1]][rownames(implist.simugroup$y.impute),colnames(implist.simugroup$y.impute)]
} else if ("simugroup" == "MES"){
  implist.simugroup <- JGNsc.cont2(y = matlist[[2]], min.cell = 20, warm = 50, iter = 50, dropThreshold = 0.65)
  observed.match <- matlist[[2]][rownames(implist.simugroup$y.impute),colnames(implist.simugroup$y.impute)]
} else if ("simugroup" == "NPC"){
  implist.simugroup <- JGNsc.cont2(y = matlist[[3]], min.cell = 20, warm = 50, iter = 50, dropThreshold = 0.65)
  observed.match <- matlist[[3]][rownames(implist.simugroup$y.impute),colnames(implist.simugroup$y.impute)]
} else if ("simugroup" == "OPC"){
  implist.simugroup <- JGNsc.cont2(y = matlist[[4]], min.cell = 20, warm = 50, iter = 50, dropThreshold = 0.65)
  observed.match <- matlist[[4]][rownames(implist.simugroup$y.impute),colnames(implist.simugroup$y.impute)]
}

nr = nrow(implist.simugroup$y.impute)
nc = ncol(implist.simugroup$y.impute)
imp.sum <-0
mask.sum <-0
x = t(implist.simugroup$y.impute)
for(kk in 1:nrep){
  rmat <- matrix(data = runif(nr*nc) > mask.rate,nrow = nr, ncol = nc) # keep 1-mask.rate,
  mask <- t((observed.match!=0) | rmat)
  x.mask = x*mask
  x.mask.mcimpute = mcImpute.R(x.mask, preprocess = T) #samples by genes input
  current.imp <- t(x.mask.mcimpute)
  x = t(current.imp)
  imp.sum <- imp.sum + current.imp
}
imp.mean <- imp.sum / nrep
rownames(imp.mean) <- rownames(observed.match)
colnames(imp.mean) <- colnames(observed.match)
if (is.null(rownames(observed.match))){
  rownames(imp.mean) <- paste("gene",1:ncol(x), sep = "")
}
if (is.null(colnames(observed.match))){
  colnames(imp.mean) <- paste("sample",1:nrow(x), sep = "")
}
imp.simugroup <- imp.mean

# mes 1:30; other: 2:30
# -------------------
save.image("D0004_GBM_simugroup.rda")
