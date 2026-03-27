# -----------------------
# Benchmark simulation
# shared link detection rate benchmark
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
countlist <- list(count1, count2)
# genes by cells
observed.list <- lapply(list(countlist[[1]]$count,
                             countlist[[2]]$count),t)


count.imp1 <- JGNsc.cont2(y = t(count1$count), dropThreshold = 0.75, min.cell = 1, warm = 200, iter = 500)
count.imp2 <- JGNsc.cont2(y = t(count2$count), dropThreshold = 0.75, min.cell = 1, warm = 200, iter = 500) 

countlist <- list(count1, count2)
implist <- list(count.imp1, count.imp2)
impiter <- JGNsc_iterimp(observed.list = observed.list, imputedList = implist)
theta.star.npn <- lapply(impiter, huge.npn) 


# ~~~~~~~~~~~~~~~~~~~
library(GENIE3)
# The algorithm outputs a matrix containing the weights of the putative regulatory links, 
# with higher weights corresponding to more likely regulatory links. 
# weightMat[i,j] is the weight of the link directed from the i-th gene to j-th gene.

res.GENIE3 <- lapply(observed.list, GENIE3) 

# ~~~~~~~~~~~~~~~~~~~
# JGNsc
# res.JGNsc <- RunJGNsc(observed.list = observed.list, min.cell = 1, runNetwork = T,
#                       l1.vec = seq(5,15, by=3)/100, l2.vec = seq(5,12, by=2)/100, a1=2, warm = 2000)
# res.JGNsc$aic.table


l1.vec <- seq(5,15,by=3)/100
l2.vec <- seq(5,12,by=2)/100
jgnsc.aic <- NULL
for (lam1 in l1.vec){
  for(lam2 in l2.vec){
    cat("tuning parameter:", lam1,", ", lam2,"\n")
    tps <- AIC_select(mat.list.t.gau = theta.star.npn, lam1 = lam1, lam2 = lam2)
    jgnsc.aic <- rbind(jgnsc.aic, tps)
  }
}
JGL.res <- JGL(theta.star.npn, lambda1 = jgnsc.aic[which.min(jgnsc.aic[,3]),1],
               lambda2 = jgnsc.aic[which.min(jgnsc.aic[,3]),2], return.whole.theta = T)
# JGL.res <- JGL(theta.star.npn, lambda1 = 0.1,
#                lambda2 = 0.05, return.whole.theta = T)
partcorr <- lapply(JGL.res$theta, prec2partialcorr)


# -------------------------------
# JGL to partcorr
partcorr.true <- lapply(list(countlist[[1]]$precision, countlist[[2]]$precision), prec2partialcorr)
# true adjacency matrix


ROCtable = NULL
tvec = c(1:1000 / 20000, 6:100/100)

for (threshtmp in tvec){
  partcorr.true.trunc <- lapply(partcorr.true, trunc_precision, threshold = threshtmp)
  trueadj.list <- lapply(partcorr.true.trunc, function(x){
    y = abs(sign(trunc_precision(x))) 
    return(y)
  }) 
  jgnsc.partcorr = lapply(partcorr, trunc_precision, threshold = threshtmp)
  res.GENIE3 = lapply(res.GENIE3, trunc_precision, threshold = threshtmp)
  
  
  methods = methods.order = c("JGNsc","GENIE3")
  
  # save.image("Benchmark_genie3.rda")
  # load("Benchmark_genie3.rda")
  # dim(res.JGNsc$theta.star.npn[[1]]) # genes with mostly zero expression are filtered
  # res.JGNsc$theta.star.npn[[1]][1:5,1:4]
  
  # make sure the order of genes are the same
  for(cond in 1:length(partcorr.true.trunc)){
    cat("condition", cond, "\n")
    if(nrow(jgnsc.partcorr[[cond]]) != nrow(partcorr.true.trunc[[cond]])){
      cgenes = intersect(rownames(jgnsc.partcorr[[cond]]), rownames(partcorr.true.trunc[[cond]]))
      partcorr.true.trunc[[cond]] = partcorr.true.trunc[[cond]][cgenes, cgenes]
      jgnsc.partcorr[[cond]] = jgnsc.partcorr[[cond]][cgenes, cgenes]
    } else if(!all(rownames(jgnsc.partcorr[[cond]]) == rownames(partcorr.true.trunc[[cond]]))){
      jgnsc.partcorr[[cond]] = jgnsc.partcorr[[cond]][rownames(partcorr.true.trunc[[cond]]),]
      jgnsc.partcorr[[cond]] = jgnsc.partcorr[[cond]][,colnames(partcorr.true.trunc[[cond]])]
    } 
    if(nrow(res.GENIE3[[cond]]) != nrow(partcorr.true.trunc[[cond]])){
      res.GENIE3[[cond]] = res.GENIE3[[cond]][rownames(partcorr.true.trunc[[cond]]),]
      res.GENIE3[[cond]] = res.GENIE3[[cond]][,colnames(partcorr.true.trunc[[cond]])]
    } else if(!all(rownames(res.GENIE3[[cond]]) == rownames(partcorr.true.trunc[[cond]]))){
      res.GENIE3[[cond]] = res.GENIE3[[cond]][rownames(partcorr.true.trunc[[cond]]),]
      res.GENIE3[[cond]] = res.GENIE3[[cond]][,colnames(partcorr.true.trunc[[cond]])]
    } 
  }
  
  # shared link detection rate:
  # sign matrix
  
  getnshare = function(estlist){
    smat = lapply(estlist , sign)
    sall = smat[[1]] + smat[[2]]
    nshare = abs(sall) == 2
    return(nshare)
  }
  
  nshare.true = getnshare(partcorr.true.trunc)
  nshare.jgnsc = getnshare(jgnsc.partcorr)
  nshare.genie3 = getnshare(res.GENIE3)
  
  sldr1 = (nshare.true + nshare.jgnsc) == 2
  sldr2 = (nshare.true + nshare.genie3) == 2
  
  sldr = c(sum(sldr1) / sum(nshare.true), sum(sldr2) / sum(nshare.true), nsample, simnum, "scenario")
  
  # false discovery rate = FP / (FP+TP)
  fdr_num1 = nshare.jgnsc - sldr1 
  sum(fdr_num1) / sum(nshare.jgnsc)
  
  fdr_num2 = nshare.genie3 - sldr2 
  sum(fdr_num2) / sum(nshare.genie3)
  
  tnr1 = sum(((1-nshare.jgnsc) + (1- nshare.true)) ==2) / sum((1- nshare.true))
  tnr2 = sum(((1-nshare.genie3) + (1- nshare.true)) ==2) / sum((1- nshare.true))
  
  
  TPR1 = sum(sldr1) / sum(nshare.true)
  TPR2 = sum(sldr2) / sum(nshare.true)
  FPR1 = 1 - tnr1
  FPR2 = 1 - tnr2
  
  ROCtable = rbind(ROCtable,
                   c(TPR1, FPR1, TPR2, FPR2, threshtmp))
}

dt1 = data.frame(ROCtable[,c(1:2,5)])
dt1$method = "JGNsc"
dt2 = data.frame(ROCtable[,3:5])
dt2$method = "GENIE3"

dt = rbind(dt1, dt2)
dt$simu = simnum
dt$nn = nsample
dt$s = "scenario"

# ggplot(dt, aes(x = X2, y = X1, color = method)) + geom_line()


# write.table(sldr, paste(outpath,"/Benchmark_sharedlinkdetectionrate_GENIE3_simnum_nsample_scenario.txt", sep = ""), quote = F, sep = "\t", row.names = F, col.names = T)
write.table(dt, paste(outpath,"/Benchmark_sharedlinkdetectionrate_GENIE3_simnum_nsample_scenario.txt", sep = ""), quote = F, sep = "\t", row.names = F, col.names = T)
