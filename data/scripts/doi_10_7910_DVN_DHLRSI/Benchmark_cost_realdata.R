# test computational cost for
# 1. mcimpute
# 2. bayesian step

# -----------------------
# Benchmark simulation -- computational cost
# -----------------------
setwd("/pine/scr/m/e/meichen/hovestdta/data")
source("/pine/scr/m/e/meichen/hovestdta/scripts/FUNCTIONS.R")  
outpath ="/pine/scr/m/e/meichen/hovestdta/data/JGNSC_COST_real"
Rcpp::sourceCpp('/pine/scr/m/e/meichen/hovestdta/scripts/FUNCTIONS_CPP_JGNSC.cpp')


# setwd("C:/Users/meichen/OneDrive - University of North Carolina at Chapel Hill/994/medulloblastoma/data")
# source("C:/Users/meichen/OneDrive - University of North Carolina at Chapel Hill/994/medulloblastoma/scripts/FUNCTIONS.R")
# Rcpp::sourceCpp('C:/Users/meichen/OneDrive - University of North Carolina at Chapel Hill/994/medulloblastoma/scripts/FUNCTIONS_CPP_JGNSC.cpp')

# --------------------------
# MB rerun -- subject info
# for both OTX2 and MYC
# --------------------------
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

nrep = 10
mask.rate = 0.15

t1 = proc.time()
if ("simugroup"=="G3"){
  tcont1 = proc.time()
  implist.G3 <- JGNsc_cont_cpp(y = matlist[[1]], minCell= 20, warm = 50, iter = 50)
  tcont2 = proc.time()
  tcont2 - tcont1
  observed.match <- matlist[[1]][rownames(matlist[[1]])[implist.G3$keep.gene[,1]>20],]
  nr = nrow(implist.G3$y.impute)
  nc = ncol(implist.G3$y.impute)
  imp.sum <-0
  mask.sum <-0
  x = t(implist.G3$y.impute)
  tmc1 = proc.time()
  for(kk in 1:nrep){
    rmat <- matrix(data = runif(nr*nc) > mask.rate,nrow = nr, ncol = nc) # keep 1-mask.rate,
    mask <- t((observed.match!=0) | rmat)
    x.mask = x*mask
    x.mask.mcimpute = mcImpute_cpp(x.mask, preprocess = T) #samples by genes input
    current.imp <- t(x.mask.mcimpute$data)
    x = t(current.imp)
    imp.sum <- imp.sum + current.imp
  }
  tmc2 = proc.time()
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
  tcont1 = proc.time()
  implist.G4 <- JGNsc_cont_cpp(y = matlist[[3]], minCell= 20, warm = 50, iter = 50)
  tcont2 = proc.time()
  tcont2 - tcont1
  observed.match <- matlist[[3]][rownames(matlist[[3]])[implist.G4$keep.gene[,1]>20],]
  nr = nrow(implist.G4$y.impute)
  nc = ncol(implist.G4$y.impute)
  imp.sum <-0
  mask.sum <-0
  x = t(implist.G4$y.impute)
  tmc1 = proc.time()
  for(kk in 1:nrep){
    rmat <- matrix(data = runif(nr*nc) > mask.rate,nrow = nr, ncol = nc) # keep 1-mask.rate,
    mask <- t((observed.match!=0) | rmat)
    x.mask = x*mask
    x.mask.mcimpute = mcImpute_cpp(x.mask, preprocess = T) #samples by genes input
    current.imp <- t(x.mask.mcimpute$data)
    x = t(current.imp)
    imp.sum <- imp.sum + current.imp
  }
  tmc2 = proc.time()
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
  tcont1 = proc.time()
  implist.Int <- JGNsc_cont_cpp(y = matlist[[2]], minCell= 20, warm = 50, iter = 50)
  tcont2 = proc.time()
  tcont2 - tcont1
  observed.match <- matlist[[2]][rownames(matlist[[2]])[implist.Int$keep.gene[,1]>20],]
  nr = nrow(implist.Int$y.impute)
  nc = ncol(implist.Int$y.impute)
  imp.sum <-0
  mask.sum <-0
  x = t(implist.Int$y.impute)
  tmc1 = proc.time()
  for(kk in 1:nrep){
    rmat <- matrix(data = runif(nr*nc) > mask.rate,nrow = nr, ncol = nc) # keep 1-mask.rate,
    mask <- t((observed.match!=0) | rmat)
    x.mask = x*mask
    x.mask.mcimpute = mcImpute_cpp(x.mask, preprocess = T) #samples by genes input
    current.imp <- t(x.mask.mcimpute$data)
    x = t(current.imp)
    imp.sum <- imp.sum + current.imp
  }
  tmc2 = proc.time()
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


t2 = proc.time()
tdiff.imp = t2-t1

tdiff.cont = tcont2 - tcont1
tdiff.mcimp = tmc2 - tmc1

# -------------------
write.table(c(tdiff.cont[3],tdiff.mcimp[3],tdiff.imp[3]),"benchmark_cost_realdata_MB_simugroup.txt")
save.image("benchmark_realdata_D0003cpp_MB_simugroup.rda")
