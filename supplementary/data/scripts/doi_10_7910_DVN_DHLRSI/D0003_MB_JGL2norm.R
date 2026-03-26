
setwd("..")
source("../FUNCTIONS.R")  
outpath =".."
Rcpp::sourceCpp('../FUNCTIONS_CPP_JGNSC.cpp')

library(pheatmap)
library(umap)
library(huge)
library(JGL)
library(PRROC)
 
impGauListAll <- readRDS("D0003norm_MB_JGL_Gaulist.rds")

t1 <- proc.time()
tps <- AIC_select(mat.list.t.gau = impGauListAll, lam1 = lam1replace/100, lam2 = lam2replace/1000, returnJGL = T)
t2 <- proc.time()
t2 -t1
aic1 <- tps[[1]]
jgl1 <- tps[[2]]


# saveRDS(jgl1, "H99_1_MB_OTX2_JGL_example1005.rds")
write.table(aic1, paste(outpath,"D0003norm_Lambda_", lam1replace,lam2replace,".txt", sep = ""), sep = "\t", quote = F)
saveRDS(jgl1, paste(outpath,"D0003norm_Lambda_", lam1replace,lam2replace,".rds", sep = ""))