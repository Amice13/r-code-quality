# -----------------------
# Benchmark simulation
# -----------------------
setwd("..")
source("../FUNCTIONS.R")  
outpath =".."
Rcpp::sourceCpp('../FUNCTIONS_CPP_JGNSC.cpp')

library(pheatmap)
library(umap)
library(huge)
library(JGL)
library(PRROC)
library(Rcpp)
library(RcppArmadillo)
 
set.seed(simnum)
# nsample = 200
# nivec.list.diff <- list(nivec= c(rep(2,10), rep(20,4)),nivec2 = rep(20,5))
# diffblk = list( 1:10,1)
# sigma.list.1 <- generateSigmaList(nivec.list = nivec.list.diff, structure = "Diff S, Identical W", diffblk = diffblk)
if ("scenario" == "DD"){
  nivec.list.diff <- list(nivec= c(10,20,30,40),nivec2 = rep(20,5))
  diffblk = list( 1:4,1:5) 
  sigma.list.1 <- generateSigmaList(nivec.list = nivec.list.diff, structure = "Diff S, Diff W", diffblk = diffblk)
} else if ("scenario" =="DI20"){
  nivec.list.diff <- list(nivec= c(rep(2,10), rep(20,4)),nivec2 = rep(20,5))
  diffblk = list( 1:10,1) 
  sigma.list.1 <- generateSigmaList(nivec.list = nivec.list.diff, structure = "Diff S, Identical W", diffblk = diffblk)
} else if ("scenario" =="DI50"){
  nivec.list.diff <- list(nivec= c(5,10,15,20, rep(25,2)),nivec2 = rep(25,4))
  diffblk = list( 1:4,1:2) 
  sigma.list.1 <- generateSigmaList(nivec.list = nivec.list.diff, structure = "Diff S, Identical W", diffblk = diffblk)
} else if ("scenario" =="ID"){
  nivec.list <- list(nivec= rep(20,5),nivec2 = rep(20,5))
  sigma.list.1 <- generateSigmaList(nivec.list = nivec.list, structure = "Identical S, Diff W", diffblk = NULL)
}


count1 <- CountMap5(sigma = sigma.list.1[[1]], ngene = 100, n = nsample)
count2 <- CountMap5(sigma = sigma.list.1[[2]], ngene = 100, n = nsample)
# count1$count[1:5,1:5]

t1 = proc.time()
count.imp1 <- JGNsc_cont_cpp(y = t(count1$count), minCell = 1, iter = 5000, warm = 2000, stepsize = 0.5, dropThreshold = 0.75, a1 = 3)
t2 = proc.time()
t2 - t1
count.imp2 <- JGNsc_cont_cpp(y = t(count2$count), minCell = 1, iter = 5000, warm = 2000, stepsize = 0.5, dropThreshold = 0.75, a1 = 3)

countlist <- list(count1, count2)
implist <- list(count.imp1, count.imp2)

observed.list <- lapply(list(countlist[[1]]$count,
                             countlist[[2]]$count),t)

# MCIMP
# data: samples by genes 
TEST = mcImpute_cpp(countlist[[1]]$count)
mcimp.list <- lapply(list(countlist[[1]]$count, countlist[[2]]$count), mcImpute_cpp, preprocess = T)

dim(mcimp.list[[1]]$data)
mcimp.list <- lapply(mcimp.list, function(x){
  x0 = x$data
  rownames(x0) <- rownames(countlist[[1]]$count)
  colnames(x0) <- colnames(countlist[[1]]$count)
  return(x0)
})

# mcimp.list[[1]][1:3,1:3]

# iter imp
# impiter <- JGNsc_iterimp(observed.list = observed.list, imputedList = implist)
# theta.star.npn <- lapply(impiter, huge.npn) 
# theta.star.npn[[1]][1:5,1:5]
theta.star.npn = RunJGNsc(observed.list = observed.list, warm = 2000, iter = 5000)
# theta.star.npn$theta.star.npn[[1]][1:5,1:5]

# ------------------------
# Gaussian Transformation
# ------------------------

observedGauList <- list()
JGNscGauList <- list()
NodropCountGauList <- list()
mcimpCountGauList <- list()
for (k in 1:length(countlist)){
  observedGauList[[k]] <- huge.npn(countlist[[k]]$count)
  JGNscGauList[[k]] <- huge.npn(t(implist[[k]]$y.impute))
  NodropCountGauList[[k]] <- huge.npn(countlist[[k]]$count.nodrop)
  mcimpCountGauList[[k]] <- huge.npn(mcimp.list[[k]])
}


# -------------------------------
# jgl results
# N BY P 

getPartcorrViaTPSelect <- function(inputgau, l1.vec, l2.vec){
  if (is.null(l1.vec)){
    l1.vec <- seq(1,30,by=2)/100
  }
  if (is.null(l2.vec)){
    l2.vec <- seq(1,20,by=2)/100
  }
  tuningparam <- NULL
  for (lam1 in l1.vec){
    for(lam2 in l2.vec){
      cat("tuning parameter:", lam1,", ", lam2,"\n")
      # tps <- AIC_select(mat.list.t.gau = input.npn, lam1 = lam1, lam2 = lam2)
      tps <- tuning_select(inputgau, lam1 = lam1, lam2 = lam2)
      tuningparam <- rbind(tuningparam, tps[[1]])
    }
  }
  colnames(tuningparam) <- c("lam1", "lam2", "aic", "bic", "ebic", "ebic1", "ebic2") 
  JGL.aic <- JGL(inputgau, lambda1 = tuningparam[which.min(tuningparam[,3]),1],
                 lambda2 = tuningparam[which.min(tuningparam[,3]),2], return.whole.theta = T)
  partcorr.aic <- lapply(JGL.aic$theta, prec2partialcorr)
  JGL.bic <- JGL(inputgau, lambda1 = tuningparam[which.min(tuningparam[,4]),1],
                 lambda2 = tuningparam[which.min(tuningparam[,4]),2], return.whole.theta = T)
  partcorr.bic <- lapply(JGL.bic$theta, prec2partialcorr)
  JGL.ebic <- JGL(inputgau, lambda1 = tuningparam[which.min(tuningparam[,5]),1],
                  lambda2 = tuningparam[which.min(tuningparam[,5]),2], return.whole.theta = T)
  partcorr.ebic <- lapply(JGL.ebic$theta, prec2partialcorr)
  res.partcor <- list(partcorr.aic = partcorr.aic,
                      partcorr.bic = partcorr.bic,
                      partcorr.ebic = partcorr.ebic,
                      tuningparam = tuningparam)
  return(res.partcor)
}

plist.aic <- list()
plist.bic <- list()
plist.ebic <- list()
plist.stars <- list()
stars.table <- NULL
tp.table <- NULL

methods.list <- list(NodropCountGauList,   
                     theta.star.npn$theta.star.npn,
                     JGNscGauList,
                     mcimpCountGauList, 
                     observedGauList)
methods <- c("NoDropout",
             "JGNsc + iter", 
             "JGNsc", 
             "McImpute",
             "Observed")
methods.order <- methods


for (mm in 1:length(methods.list)){
  tp.temp <- getPartcorrViaTPSelect(methods.list[[mm]], l1.vec = seq(1,35, by=2)/100, l2.vec = c(1,3,5,7,9,11,13,15)/100)
  stars.temp <- stars.select(methods.list[[mm]], lambvec = seq(1,35, by=2)/100)
  # stars.observed$stars.l1$opt.lambda.correction
  # stars.observed$stars.l2$opt.lambda.correction
  # partcorr.stars <- stars.observed$partcorr
  plist.aic[[mm]] <- tp.temp$partcorr.aic
  plist.bic[[mm]] <- tp.temp$partcorr.bic
  plist.ebic[[mm]] <- tp.temp$partcorr.ebic
  plist.stars[[mm]] <- stars.temp$partcorr
  tp.table <- rbind(tp.table, cbind.data.frame(tp.temp$tuningparam, methods[mm]))
  stars.table <- rbind(stars.table, c(stars.temp$stars.l1$opt.lambda.correction, 
                                      stars.temp$stars.l2$opt.lambda.correction, methods[mm]))
}


# -------------------------------
# JGL to partcorr
partcorr.true <- lapply(list(countlist[[1]]$precision, countlist[[2]]$precision), prec2partialcorr)
# true adjacency matrix
partcorr.true.trunc <- lapply(partcorr.true, trunc_precision)
trueadj.list <- lapply(partcorr.true.trunc, function(x){
  y = abs(sign(trunc_precision(x))) 
  return(y)
}) 

eval.table.aic <- eval_partcorrList(plist = plist.aic, partcorr.true.trunc = partcorr.true.trunc, 
                                    methods = methods, methods.order = methods.order)
eval.table.bic <- eval_partcorrList(plist = plist.bic, partcorr.true.trunc = partcorr.true.trunc, 
                                    methods = methods, methods.order = methods.order)
eval.table.ebic <- eval_partcorrList(plist = plist.ebic, partcorr.true.trunc = partcorr.true.trunc, 
                                     methods = methods, methods.order = methods.order)
eval.table.stars <- eval_partcorrList(plist = plist.stars, partcorr.true.trunc = partcorr.true.trunc, 
                                      methods = methods, methods.order = methods.order)

tpnames <- c("AIC","BIC","EBIC","STARS")
tpeval <- list(eval.table.aic,
               eval.table.bic,
               eval.table.ebic,
               eval.table.stars)

for (tt in 1:length(tpnames)){
  TP = tpnames[tt]
  evaltable = tpeval[[tt]]
  write.table(evaltable$sse, paste(outpath,"/D0002_",TP,"_SSE_simnum_nsample_scenario.txt", sep = ""), quote = F, sep = "\t", row.names = F, col.names = F)
  write.table(evaltable$pearson, paste(outpath,"/D0002_",TP,"_pcor_simnum_nsample_scenario.txt", sep = ""), quote = F, sep = "\t", row.names = F, col.names = F)
  write.table(evaltable$auc, paste(outpath,"/D0002_",TP,"_auc_simnum_nsample_scenario.txt", sep = ""), quote = F, sep = "\t", row.names = F, col.names = F)
  write.table(evaltable$prc, paste(outpath,"/D0002_",TP,"_auprc_simnum_nsample_scenario.txt", sep = ""), quote = F, sep = "\t", row.names = F, col.names = F)
}

if (("scenario" == "DI20") & (simnum == 1) & (nsample == 500)){
  save.image("/pine/scr/m/e/meichen/hovestdta/data/D0002_Simulation_cpp.rda")
}
 