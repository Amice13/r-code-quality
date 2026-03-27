# --------------------------
# MB rerun -- subject info
# for both OTX2 and MYC
# --------------------------
setwd("..")
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
# load("H99_1_MBdata_otx2.rda")
load("B0003_data_v3.rda")
source("../FUNCTIONS.R")
  
nrep = 20
mask.rate = 0.15

if ("simugroup"=="G3"){
  implist.G3 <- JGNsc.cont2(y = matlist[[1]], min.cell = 20, warm = 50, iter = 50)
  
  observed.match <- matlist[[1]][rownames(implist.G3$y.impute),colnames(implist.G3$y.impute)]
  nr = nrow(implist.G3$y.impute)
  nc = ncol(implist.G3$y.impute)
  imp.sum <-0
  mask.sum <-0
  x = t(implist.G3$y.impute)
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
  imp.G3 <- imp.mean
  
} else if ("simugroup"=="G4"){
  implist.G4 <- JGNsc.cont2(y = matlist[[3]], min.cell = 20, warm = 50, iter = 50)
  observed.match <- matlist[[3]][rownames(implist.G4$y.impute),colnames(implist.G4$y.impute)]
  nr = nrow(implist.G4$y.impute)
  nc = ncol(implist.G4$y.impute)
  imp.sum <-0
  mask.sum <-0
  x = t(implist.G4$y.impute)
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
  imp.G4 <- imp.mean
} else if ("simugroup"=="Inter"){
  implist.Int <- JGNsc.cont2(y = matlist[[2]], min.cell = 20, warm = 50, iter = 50)
  observed.match <- matlist[[2]][rownames(implist.Int$y.impute),colnames(implist.Int$y.impute)]
  nr = nrow(implist.Int$y.impute)
  nc = ncol(implist.Int$y.impute)
  imp.sum <-0
  mask.sum <-0
  x = t(implist.Int$y.impute)
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
  imp.Int <- imp.mean
}

# -------------------
save.image("D0003_MB_simugroup.rda")

