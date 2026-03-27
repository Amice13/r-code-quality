rm(list = ls())

library(GEOquery)
getGEO(filename = "GSE29111_series_matrix.txt.gz",getGPL = F)->gse1
getGEO(filename = "GSE60993_series_matrix.txt.gz",getGPL = F)->gse2
getGEO(filename = "GSE61144_series_matrix.txt.gz",getGPL = F)->gse3

library(vioplot)
exprs(gse1)->exp1
vioplot(exp1)
exprs(gse2)->exp2
vioplot(exp2)
exprs(gse3)->exp3
vioplot(exp3)

pData(gse1)->pd1
pData(gse2)->pd2
pData(gse3)->pd3

library(tidyverse)
library(stringr)

identical(colnames(exp1),rownames(pd1))

gpl_number1 <- gse1@annotation;gpl_number1
gpl_number2 <- gse2@annotation;gpl_number2
gpl_number3 <- gse3@annotation;gpl_number3

save(exp1,exp2,exp3,pd1,pd2,pd3,group1,group2,group3,
     gpl_number1,gpl_number2,gpl_number3,
     file = "step1output.Rdata")
