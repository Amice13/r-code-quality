rm(list = ls())

library(GEOquery)
getGEO(filename = "GSE59867_series_matrix.txt.gz",getGPL = F)->gse1
getGEO(filename = "GSE62646_series_matrix.txt.gz",getGPL = F)->gse2
library(vioplot)
exprs(gse1)->exp1
vioplot(exp1)
exprs(gse2)->exp2
vioplot(exp2)
pData(gse1)->pd1
pData(gse2)->pd2
library(tidyverse)
library(stringr)
identical(colnames(exp1),rownames(pd1))
#pd1$title
group1 <- ifelse(str_detect(pd1$title, "sampling 1"), "Admission",
                ifelse(str_detect(pd1$title, "sampling 2"), "Discharge",
                       ifelse(str_detect(pd1$title, "sampling 4"), "6_Month", "other")))
#identical(colnames(exp2),rownames(pd2))
#pd2$title
group2 <- ifelse(str_detect(pd2$title, "admission"), "Admission",
                 ifelse(str_detect(pd2$title, "discharge"), "Discharge",
                        ifelse(str_detect(pd2$title, "6 months"), "6_Month", "other")))

table(c(group1,group2))
#6_Month Admission Discharge     other 
#111       139       129       155 


gpl_number1 <- gse1@annotation;gpl_number1
gpl_number2 <- gse2@annotation;gpl_number2


save(exp1,exp2,pd1,pd2,group1,group2,
     gpl_number1,gpl_number2,
     file = "step1output.Rdata")
