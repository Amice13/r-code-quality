library(ggplot2)
library(gplots)
library(plyr)
library(pheatmap)
library(RColorBrewer)

#Panel c repeat PCA of embryos -----------------------------------------------------

#PCA analysis used repeat expression value (cpm) for each repeat family in embryo replicates at different stages.
rm(list=ls())
#analyses used some processed TE counts files, which are stored in 'processed' directory. 
setwd("processed/")

#MC cross
#zygote
zygote_1.rawCounts <- read.table(file="zygote-1.rep.count.bed", sep="\t",col.names=c("count_1","family", "class"))[,c(1,2)]
zygote_2.rawCounts <- read.table(file="zygote-2.rep.count.bed", sep="\t",col.names=c("count_2","family", "class"))[,c(1,2)]
zygote_5.rawCounts <- read.table(file="zygote-5.rep.count.bed", sep="\t",col.names=c("count_5","family", "class"))[,c(1,2)]
zygote_6.rawCounts <- read.table(file="zygote-6.rep.count.bed", sep="\t",col.names=c("count_6","family", "class"))[,c(1,2)]
#remove duplicated row
zygote_1.rawCounts <- zygote_1.rawCounts[!duplicated(zygote_1.rawCounts$family), ]
zygote_2.rawCounts <- zygote_2.rawCounts[!duplicated(zygote_2.rawCounts$family), ]
zygote_5.rawCounts <- zygote_5.rawCounts[!duplicated(zygote_5.rawCounts$family), ]
zygote_6.rawCounts <- zygote_6.rawCounts[!duplicated(zygote_6.rawCounts$family), ]
#merge
zygote_merge1 <- merge(zygote_1.rawCounts, zygote_2.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
zygote_merge2 <- merge(zygote_merge1, zygote_5.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
zygote_merge3 <- merge(zygote_merge2, zygote_6.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)

colnames(zygote_merge3) <- c("family", paste("zygote", c(1:4), sep="-"))
rm(list=setdiff(ls(), "zygote_merge3"))

#early2cell
early2C_1.rawCounts <- read.table(file="early2cell-1.rep.count.bed", sep="\t",col.names=c("count_1","family", "class"))[,c(1,2)]
early2C_2.rawCounts <- read.table(file="early2cell-2.rep.count.bed", sep="\t",col.names=c("count_2","family", "class"))[,c(1,2)]
early2C_3.rawCounts <- read.table(file="early2cell-3.rep.count.bed", sep="\t",col.names=c("count_3","family", "class"))[,c(1,2)]
early2C_4.rawCounts <- read.table(file="early2cell-4.rep.count.bed", sep="\t",col.names=c("count_4","family", "class"))[,c(1,2)]
early2C_5.rawCounts <- read.table(file="early2cell-5.rep.count.bed", sep="\t",col.names=c("count_5","family", "class"))[,c(1,2)]
early2C_6.rawCounts <- read.table(file="early2cell-6.rep.count.bed", sep="\t",col.names=c("count_6","family", "class"))[,c(1,2)]
#remove duplicated row
early2C_1.rawCounts <- early2C_1.rawCounts[!duplicated(early2C_1.rawCounts$family), ]
early2C_2.rawCounts <- early2C_2.rawCounts[!duplicated(early2C_2.rawCounts$family), ]
early2C_3.rawCounts <- early2C_3.rawCounts[!duplicated(early2C_3.rawCounts$family), ]
early2C_4.rawCounts <- early2C_4.rawCounts[!duplicated(early2C_4.rawCounts$family), ]
early2C_5.rawCounts <- early2C_5.rawCounts[!duplicated(early2C_5.rawCounts$family), ]
early2C_6.rawCounts <- early2C_6.rawCounts[!duplicated(early2C_6.rawCounts$family), ]
#merge
early2C_merge1 <- merge(early2C_1.rawCounts, early2C_2.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
early2C_merge2 <- merge(early2C_merge1, early2C_3.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
early2C_merge3 <- merge(early2C_merge2, early2C_4.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
early2C_merge4 <- merge(early2C_merge3, early2C_5.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
early2C_merge5 <- merge(early2C_merge4, early2C_6.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)

colnames(early2C_merge5) <- c("family", paste("early2C", c(1:6), sep="-"))
rm(list=setdiff(ls(), c("zygote_merge3","early2C_merge5")))

#late2cell
late2C_2.rawCounts <- read.table(file="late2cell-2.rep.count.bed", sep="\t",col.names=c("count_2","family", "class"))[,c(1,2)]
late2C_3.rawCounts <- read.table(file="late2cell-3.rep.count.bed", sep="\t",col.names=c("count_3","family", "class"))[,c(1,2)]
late2C_4.rawCounts <- read.table(file="late2cell-4.rep.count.bed", sep="\t",col.names=c("count_4","family", "class"))[,c(1,2)]
late2C_5.rawCounts <- read.table(file="late2cell-5.rep.count.bed", sep="\t",col.names=c("count_5","family", "class"))[,c(1,2)]
late2C_6.rawCounts <- read.table(file="late2cell-6.rep.count.bed", sep="\t",col.names=c("count_6","family", "class"))[,c(1,2)]
late2C_7.rawCounts <- read.table(file="late2cell-7.rep.count.bed", sep="\t",col.names=c("count_7","family", "class"))[,c(1,2)]
late2C_8.rawCounts <- read.table(file="late2cell-8.rep.count.bed", sep="\t",col.names=c("count_8","family", "class"))[,c(1,2)]
late2C_9.rawCounts <- read.table(file="late2cell-9.rep.count.bed", sep="\t",col.names=c("count_9","family", "class"))[,c(1,2)]
late2C_10.rawCounts <- read.table(file="late2cell-10.rep.count.bed", sep="\t",col.names=c("count_10","family", "class"))[,c(1,2)]
late2C_11.rawCounts <- read.table(file="late2cell-11.rep.count.bed", sep="\t",col.names=c("count_11","family", "class"))[,c(1,2)]
#remove duplicated row
late2C_2.rawCounts <- late2C_2.rawCounts[!duplicated(late2C_2.rawCounts$family), ]
late2C_3.rawCounts <- late2C_3.rawCounts[!duplicated(late2C_3.rawCounts$family), ]
late2C_4.rawCounts <- late2C_4.rawCounts[!duplicated(late2C_4.rawCounts$family), ]
late2C_5.rawCounts <- late2C_5.rawCounts[!duplicated(late2C_5.rawCounts$family), ]
late2C_6.rawCounts <- late2C_6.rawCounts[!duplicated(late2C_6.rawCounts$family), ]
late2C_7.rawCounts <- late2C_7.rawCounts[!duplicated(late2C_7.rawCounts$family), ]
late2C_8.rawCounts <- late2C_8.rawCounts[!duplicated(late2C_8.rawCounts$family), ]
late2C_9.rawCounts <- late2C_9.rawCounts[!duplicated(late2C_9.rawCounts$family), ]
late2C_10.rawCounts <- late2C_10.rawCounts[!duplicated(late2C_10.rawCounts$family), ]
late2C_11.rawCounts <- late2C_11.rawCounts[!duplicated(late2C_11.rawCounts$family), ]
#merge
late2C_merge1 <- merge(late2C_2.rawCounts, late2C_3.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
late2C_merge2 <- merge(late2C_merge1, late2C_4.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
late2C_merge3 <- merge(late2C_merge2, late2C_5.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
late2C_merge4 <- merge(late2C_merge3, late2C_6.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
late2C_merge5 <- merge(late2C_merge4, late2C_7.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
late2C_merge6 <- merge(late2C_merge5, late2C_8.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
late2C_merge7 <- merge(late2C_merge6, late2C_9.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
late2C_merge8 <- merge(late2C_merge7, late2C_10.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
late2C_merge9 <- merge(late2C_merge8, late2C_11.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)

colnames(late2C_merge9) <- c("family", paste("late2C", c(1:10), sep="-"))
rm(list=setdiff(ls(), c("zygote_merge3","early2C_merge5", "late2C_merge9")))

#4cell
N4C_1.rawCounts <- read.table(file="4cell-1.rep.count.bed", sep="\t",col.names=c("count_1","family", "class"))[,c(1,2)]
N4C_2.rawCounts <- read.table(file="4cell-2.rep.count.bed", sep="\t",col.names=c("count_2","family", "class"))[,c(1,2)]
N4C_3.rawCounts <- read.table(file="4cell-3.rep.count.bed", sep="\t",col.names=c("count_3","family", "class"))[,c(1,2)]
N4C_4.rawCounts <- read.table(file="4cell-4.rep.count.bed", sep="\t",col.names=c("count_4","family", "class"))[,c(1,2)]
N4C_5.rawCounts <- read.table(file="4cell-5.rep.count.bed", sep="\t",col.names=c("count_5","family", "class"))[,c(1,2)]
N4C_7.rawCounts <- read.table(file="4cell-7.rep.count.bed", sep="\t",col.names=c("count_7","family", "class"))[,c(1,2)]
N4C_8.rawCounts <- read.table(file="4cell-8.rep.count.bed", sep="\t",col.names=c("count_8","family", "class"))[,c(1,2)]
N4C_9.rawCounts <- read.table(file="4cell-9.rep.count.bed", sep="\t",col.names=c("count_9","family", "class"))[,c(1,2)]
N4C_10.rawCounts <- read.table(file="4cell-10.rep.count.bed", sep="\t",col.names=c("count_10","family", "class"))[,c(1,2)]
N4C_11.rawCounts <- read.table(file="4cell-11.rep.count.bed", sep="\t",col.names=c("count_11","family", "class"))[,c(1,2)]
N4C_12.rawCounts <- read.table(file="4cell-12.rep.count.bed", sep="\t",col.names=c("count_12","family", "class"))[,c(1,2)]
N4C_13.rawCounts <- read.table(file="4cell-13.rep.count.bed", sep="\t",col.names=c("count_13","family", "class"))[,c(1,2)]
N4C_14.rawCounts <- read.table(file="4cell-14.rep.count.bed", sep="\t",col.names=c("count_14","family", "class"))[,c(1,2)]
N4C_15.rawCounts <- read.table(file="4cell-15.rep.count.bed", sep="\t",col.names=c("count_15","family", "class"))[,c(1,2)]
N4C_16.rawCounts <- read.table(file="4cell-16.rep.count.bed", sep="\t",col.names=c("count_16","family", "class"))[,c(1,2)]
N4C_17.rawCounts <- read.table(file="4cell-17.rep.count.bed", sep="\t",col.names=c("count_17","family", "class"))[,c(1,2)]
N4C_18.rawCounts <- read.table(file="4cell-18.rep.count.bed", sep="\t",col.names=c("count_18","family", "class"))[,c(1,2)]
#remove duplicated row
N4C_1.rawCounts <- N4C_1.rawCounts[!duplicated(N4C_1.rawCounts$family), ]
N4C_2.rawCounts <- N4C_2.rawCounts[!duplicated(N4C_2.rawCounts$family), ]
N4C_3.rawCounts <- N4C_3.rawCounts[!duplicated(N4C_3.rawCounts$family), ]
N4C_4.rawCounts <- N4C_4.rawCounts[!duplicated(N4C_4.rawCounts$family), ]
N4C_5.rawCounts <- N4C_5.rawCounts[!duplicated(N4C_5.rawCounts$family), ]
N4C_7.rawCounts <- N4C_7.rawCounts[!duplicated(N4C_7.rawCounts$family), ]
N4C_8.rawCounts <- N4C_8.rawCounts[!duplicated(N4C_8.rawCounts$family), ]
N4C_9.rawCounts <- N4C_9.rawCounts[!duplicated(N4C_9.rawCounts$family), ]
N4C_10.rawCounts <- N4C_10.rawCounts[!duplicated(N4C_10.rawCounts$family), ]
N4C_11.rawCounts <- N4C_11.rawCounts[!duplicated(N4C_11.rawCounts$family), ]
N4C_12.rawCounts <- N4C_12.rawCounts[!duplicated(N4C_12.rawCounts$family), ]
N4C_13.rawCounts <- N4C_13.rawCounts[!duplicated(N4C_13.rawCounts$family), ]
N4C_14.rawCounts <- N4C_14.rawCounts[!duplicated(N4C_14.rawCounts$family), ]
N4C_15.rawCounts <- N4C_15.rawCounts[!duplicated(N4C_15.rawCounts$family), ]
N4C_16.rawCounts <- N4C_16.rawCounts[!duplicated(N4C_16.rawCounts$family), ]
N4C_17.rawCounts <- N4C_17.rawCounts[!duplicated(N4C_17.rawCounts$family), ]
N4C_18.rawCounts <- N4C_18.rawCounts[!duplicated(N4C_18.rawCounts$family), ]
#merge
N4C_merge1 <- merge(N4C_1.rawCounts, N4C_2.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N4C_merge2 <- merge(N4C_merge1, N4C_3.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N4C_merge3 <- merge(N4C_merge2, N4C_4.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N4C_merge4 <- merge(N4C_merge3, N4C_5.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N4C_merge5 <- merge(N4C_merge4, N4C_7.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N4C_merge6 <- merge(N4C_merge5, N4C_8.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N4C_merge7 <- merge(N4C_merge6, N4C_9.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N4C_merge8 <- merge(N4C_merge7, N4C_10.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N4C_merge9 <- merge(N4C_merge8, N4C_11.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N4C_merge10 <- merge(N4C_merge9, N4C_12.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N4C_merge11 <- merge(N4C_merge10, N4C_13.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N4C_merge12 <- merge(N4C_merge11, N4C_14.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N4C_merge13 <- merge(N4C_merge12, N4C_15.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N4C_merge14 <- merge(N4C_merge13, N4C_16.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N4C_merge15 <- merge(N4C_merge14, N4C_17.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N4C_merge16 <- merge(N4C_merge15, N4C_18.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)

colnames(N4C_merge16) <- c("family", paste("N4C", c(1:17), sep="-"))
rm(list=setdiff(ls(), c("zygote_merge3","early2C_merge5","late2C_merge9","N4C_merge16")))

#8cell
N8C_6.rawCounts <- read.table(file="8cell-6.rep.count.bed", sep="\t",col.names=c("count_6","family", "class"))[,c(1,2)]
N8C_7.rawCounts <- read.table(file="8cell-7.rep.count.bed", sep="\t",col.names=c("count_7","family", "class"))[,c(1,2)]
N8C_8.rawCounts <- read.table(file="8cell-8.rep.count.bed", sep="\t",col.names=c("count_8","family", "class"))[,c(1,2)]
N8C_9.rawCounts <- read.table(file="8cell-9.rep.count.bed", sep="\t",col.names=c("count_9","family", "class"))[,c(1,2)]
N8C_10.rawCounts <- read.table(file="8cell-10.rep.count.bed", sep="\t",col.names=c("count_10","family", "class"))[,c(1,2)]
N8C_11.rawCounts <- read.table(file="8cell-11.rep.count.bed", sep="\t",col.names=c("count_11","family", "class"))[,c(1,2)]
N8C_12.rawCounts <- read.table(file="8cell-12.rep.count.bed", sep="\t",col.names=c("count_12","family", "class"))[,c(1,2)]
N8C_13.rawCounts <- read.table(file="8cell-13.rep.count.bed", sep="\t",col.names=c("count_13","family", "class"))[,c(1,2)]
N8C_14.rawCounts <- read.table(file="8cell-14.rep.count.bed", sep="\t",col.names=c("count_14","family", "class"))[,c(1,2)]
N8C_15.rawCounts <- read.table(file="8cell-15.rep.count.bed", sep="\t",col.names=c("count_15","family", "class"))[,c(1,2)]
N8C_16.rawCounts <- read.table(file="8cell-16.rep.count.bed", sep="\t",col.names=c("count_16","family", "class"))[,c(1,2)]
N8C_17.rawCounts <- read.table(file="8cell-17.rep.count.bed", sep="\t",col.names=c("count_17","family", "class"))[,c(1,2)]
N8C_18.rawCounts <- read.table(file="8cell-18.rep.count.bed", sep="\t",col.names=c("count_18","family", "class"))[,c(1,2)]
N8C_19.rawCounts <- read.table(file="8cell-19.rep.count.bed", sep="\t",col.names=c("count_19","family", "class"))[,c(1,2)]
N8C_20.rawCounts <- read.table(file="8cell-20.rep.count.bed", sep="\t",col.names=c("count_20","family", "class"))[,c(1,2)]
#remove duplicated row
N8C_6.rawCounts <- N8C_6.rawCounts[!duplicated(N8C_6.rawCounts$family), ]
N8C_7.rawCounts <- N8C_7.rawCounts[!duplicated(N8C_7.rawCounts$family), ]
N8C_8.rawCounts <- N8C_8.rawCounts[!duplicated(N8C_8.rawCounts$family), ]
N8C_9.rawCounts <- N8C_9.rawCounts[!duplicated(N8C_9.rawCounts$family), ]
N8C_10.rawCounts <- N8C_10.rawCounts[!duplicated(N8C_10.rawCounts$family), ]
N8C_11.rawCounts <- N8C_11.rawCounts[!duplicated(N8C_11.rawCounts$family), ]
N8C_12.rawCounts <- N8C_12.rawCounts[!duplicated(N8C_12.rawCounts$family), ]
N8C_13.rawCounts <- N8C_13.rawCounts[!duplicated(N8C_13.rawCounts$family), ]
N8C_14.rawCounts <- N8C_14.rawCounts[!duplicated(N8C_14.rawCounts$family), ]
N8C_15.rawCounts <- N8C_15.rawCounts[!duplicated(N8C_15.rawCounts$family), ]
N8C_16.rawCounts <- N8C_16.rawCounts[!duplicated(N8C_16.rawCounts$family), ]
N8C_17.rawCounts <- N8C_17.rawCounts[!duplicated(N8C_17.rawCounts$family), ]
N8C_18.rawCounts <- N8C_18.rawCounts[!duplicated(N8C_18.rawCounts$family), ]
N8C_19.rawCounts <- N8C_19.rawCounts[!duplicated(N8C_19.rawCounts$family), ]
N8C_20.rawCounts <- N8C_20.rawCounts[!duplicated(N8C_20.rawCounts$family), ]
#merge
N8C_merge5 <- merge(N8C_6.rawCounts, N8C_7.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N8C_merge6 <- merge(N8C_merge5, N8C_8.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N8C_merge7 <- merge(N8C_merge6, N8C_9.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N8C_merge8 <- merge(N8C_merge7, N8C_10.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N8C_merge9 <- merge(N8C_merge8, N8C_11.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N8C_merge10 <- merge(N8C_merge9, N8C_12.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N8C_merge11 <- merge(N8C_merge10, N8C_13.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N8C_merge12 <- merge(N8C_merge11, N8C_14.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N8C_merge13 <- merge(N8C_merge12, N8C_15.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N8C_merge14 <- merge(N8C_merge13, N8C_16.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N8C_merge15 <- merge(N8C_merge14, N8C_17.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N8C_merge16 <- merge(N8C_merge15, N8C_18.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N8C_merge17 <- merge(N8C_merge16, N8C_19.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N8C_merge18 <- merge(N8C_merge17, N8C_20.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)

colnames(N8C_merge18) <- c("family", paste("N8C", c(1:15), sep="-"))
rm(list=setdiff(ls(), c("zygote_merge3","early2C_merge5","late2C_merge9","N4C_merge16","N8C_merge18")))

#16cell
N16C_1.rawCounts <- read.table(file="16cell-1.rep.count.bed", sep="\t",col.names=c("count_1","family", "class"))[,c(1,2)]
N16C_2.rawCounts <- read.table(file="16cell-2.rep.count.bed", sep="\t",col.names=c("count_2","family", "class"))[,c(1,2)]
N16C_3.rawCounts <- read.table(file="16cell-3.rep.count.bed", sep="\t",col.names=c("count_3","family", "class"))[,c(1,2)]
N16C_4.rawCounts <- read.table(file="16cell-4.rep.count.bed", sep="\t",col.names=c("count_4","family", "class"))[,c(1,2)]
N16C_5.rawCounts <- read.table(file="16cell-5.rep.count.bed", sep="\t",col.names=c("count_5","family", "class"))[,c(1,2)]
N16C_6.rawCounts <- read.table(file="16cell-6.rep.count.bed", sep="\t",col.names=c("count_6","family", "class"))[,c(1,2)]
N16C_7.rawCounts <- read.table(file="16cell-7.rep.count.bed", sep="\t",col.names=c("count_7","family", "class"))[,c(1,2)]
N16C_8.rawCounts <- read.table(file="16cell-8.rep.count.bed", sep="\t",col.names=c("count_8","family", "class"))[,c(1,2)]
#remove duplicated row
N16C_1.rawCounts <- N16C_1.rawCounts[!duplicated(N16C_1.rawCounts$family), ]
N16C_2.rawCounts <- N16C_2.rawCounts[!duplicated(N16C_2.rawCounts$family), ]
N16C_3.rawCounts <- N16C_3.rawCounts[!duplicated(N16C_3.rawCounts$family), ]
N16C_4.rawCounts <- N16C_4.rawCounts[!duplicated(N16C_4.rawCounts$family), ]
N16C_5.rawCounts <- N16C_5.rawCounts[!duplicated(N16C_5.rawCounts$family), ]
N16C_6.rawCounts <- N16C_6.rawCounts[!duplicated(N16C_6.rawCounts$family), ]
N16C_7.rawCounts <- N16C_7.rawCounts[!duplicated(N16C_7.rawCounts$family), ]
N16C_8.rawCounts <- N16C_8.rawCounts[!duplicated(N16C_8.rawCounts$family), ]
#merge
N16C_merge1 <- merge(N16C_1.rawCounts, N16C_2.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N16C_merge2 <- merge(N16C_merge1, N16C_3.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N16C_merge3 <- merge(N16C_merge2, N16C_4.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N16C_merge4 <- merge(N16C_merge3, N16C_5.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N16C_merge5 <- merge(N16C_merge4, N16C_6.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N16C_merge6 <- merge(N16C_merge5, N16C_7.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N16C_merge7 <- merge(N16C_merge6, N16C_8.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)

colnames(N16C_merge7) <- c("family", paste("N16C", c(1:8), sep="-"))
rm(list=setdiff(ls(), c("zygote_merge3","early2C_merge5","late2C_merge9","N4C_merge16","N8C_merge18","N16C_merge7")))

#earlyBlast
earlyBlast_1.rawCounts <- read.table(file="earlyBlast-1.rep.count.bed", sep="\t",col.names=c("count_1","family", "class"))[,c(1,2)]
earlyBlast_2.rawCounts <- read.table(file="earlyBlast-2.rep.count.bed", sep="\t",col.names=c("count_2","family", "class"))[,c(1,2)]
earlyBlast_3.rawCounts <- read.table(file="earlyBlast-3.rep.count.bed", sep="\t",col.names=c("count_3","family", "class"))[,c(1,2)]
earlyBlast_4.rawCounts <- read.table(file="earlyBlast-4.rep.count.bed", sep="\t",col.names=c("count_4","family", "class"))[,c(1,2)]
earlyBlast_5.rawCounts <- read.table(file="earlyBlast-5.rep.count.bed", sep="\t",col.names=c("count_5","family", "class"))[,c(1,2)]
earlyBlast_6.rawCounts <- read.table(file="earlyBlast-6.rep.count.bed", sep="\t",col.names=c("count_6","family", "class"))[,c(1,2)]
earlyBlast_7.rawCounts <- read.table(file="earlyBlast-7.rep.count.bed", sep="\t",col.names=c("count_7","family", "class"))[,c(1,2)]
earlyBlast_8.rawCounts <- read.table(file="earlyBlast-8.rep.count.bed", sep="\t",col.names=c("count_8","family", "class"))[,c(1,2)]
earlyBlast_9.rawCounts <- read.table(file="earlyBlast-9.rep.count.bed", sep="\t",col.names=c("count_9","family", "class"))[,c(1,2)]
earlyBlast_10.rawCounts <- read.table(file="earlyBlast-10.rep.count.bed", sep="\t",col.names=c("count_10","family", "class"))[,c(1,2)]
earlyBlast_11.rawCounts <- read.table(file="earlyBlast-11.rep.count.bed", sep="\t",col.names=c("count_11","family", "class"))[,c(1,2)]
#remove duplicated row
earlyBlast_1.rawCounts <- earlyBlast_1.rawCounts[!duplicated(earlyBlast_1.rawCounts$family), ]
earlyBlast_2.rawCounts <- earlyBlast_2.rawCounts[!duplicated(earlyBlast_2.rawCounts$family), ]
earlyBlast_3.rawCounts <- earlyBlast_3.rawCounts[!duplicated(earlyBlast_3.rawCounts$family), ]
earlyBlast_4.rawCounts <- earlyBlast_4.rawCounts[!duplicated(earlyBlast_4.rawCounts$family), ]
earlyBlast_5.rawCounts <- earlyBlast_5.rawCounts[!duplicated(earlyBlast_5.rawCounts$family), ]
earlyBlast_6.rawCounts <- earlyBlast_6.rawCounts[!duplicated(earlyBlast_6.rawCounts$family), ]
earlyBlast_7.rawCounts <- earlyBlast_7.rawCounts[!duplicated(earlyBlast_7.rawCounts$family), ]
earlyBlast_8.rawCounts <- earlyBlast_8.rawCounts[!duplicated(earlyBlast_8.rawCounts$family), ]
earlyBlast_9.rawCounts <- earlyBlast_9.rawCounts[!duplicated(earlyBlast_9.rawCounts$family), ]
earlyBlast_10.rawCounts <- earlyBlast_10.rawCounts[!duplicated(earlyBlast_10.rawCounts$family), ]
earlyBlast_11.rawCounts <- earlyBlast_11.rawCounts[!duplicated(earlyBlast_11.rawCounts$family), ]
#merge
earlyBlast_merge1 <- merge(earlyBlast_1.rawCounts, earlyBlast_2.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
earlyBlast_merge2 <- merge(earlyBlast_merge1, earlyBlast_3.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
earlyBlast_merge3 <- merge(earlyBlast_merge2, earlyBlast_4.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
earlyBlast_merge4 <- merge(earlyBlast_merge3, earlyBlast_5.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
earlyBlast_merge5 <- merge(earlyBlast_merge4, earlyBlast_6.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
earlyBlast_merge6 <- merge(earlyBlast_merge5, earlyBlast_7.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
earlyBlast_merge7 <- merge(earlyBlast_merge6, earlyBlast_8.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
earlyBlast_merge8 <- merge(earlyBlast_merge7, earlyBlast_9.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
earlyBlast_merge9 <- merge(earlyBlast_merge8, earlyBlast_10.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
earlyBlast_merge10 <- merge(earlyBlast_merge9, earlyBlast_11.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)

colnames(earlyBlast_merge10) <- c("family", paste("earlyBlast", c(1:11), sep="-"))
rm(list=setdiff(ls(), c("zygote_merge3","early2C_merge5","late2C_merge9","N4C_merge16","N8C_merge18","N16C_merge7","earlyBlast_merge10")))

#merge all
zygote_early2C_merge <- merge(zygote_merge3, early2C_merge5, by.x="family", by.y="family", all.x=T, all.y=T)
zygote_late2C_merge <- merge(zygote_early2C_merge, late2C_merge9, by.x="family", by.y="family", all.x=T, all.y=T)
zygote_4C_merge <- merge(zygote_late2C_merge, N4C_merge16, by.x="family", by.y="family", all.x=T, all.y=T)
zygote_8C_merge <- merge(zygote_4C_merge, N8C_merge18, by.x="family", by.y="family", all.x=T, all.y=T)
zygote_16C_merge <- merge(zygote_8C_merge, N16C_merge7, by.x="family", by.y="family", all.x=T, all.y=T)
zygote_eB_merge <- merge(zygote_16C_merge, earlyBlast_merge10, by.x="family", by.y="family", all.x=T, all.y=T)
zygote_eB_merge[is.na(zygote_eB_merge)] <- 0
MC.rawCounts <- zygote_eB_merge[ ,-1]
rownames(MC.rawCounts) <- zygote_eB_merge$family

rm(list=setdiff(ls(), "MC.rawCounts"))


#total counts (genes and repeats, non-rRNA)
repeat_reads_ratio <- read.table(file="MC_repeat_reads_ratio_200hits.txt", sep="\t", header=T)
total.read.counts <- repeat_reads_ratio$non.rRNA_reads
MC.cpm <- data.frame(MC.rawCounts[1] * 1000000 / total.read.counts[1])
colnames(MC.cpm) <- colnames(MC.rawCounts)[1]
for (i in 2:ncol(MC.rawCounts)){
  MC.cpm[ ,i] <- MC.rawCounts[i] * 1000000 / total.read.counts[i]
  colnames(MC.cpm)[i] <- colnames(MC.rawCounts)[i]
}




#CM cross
#late2cell
late2C_1.rawCounts <- read.table(file="Reci.late2cell-1.rep.count.bed", sep="\t",col.names=c("count_1","family", "class"))[,c(1,2)]
late2C_2.rawCounts <- read.table(file="Reci.late2cell-2.rep.count.bed", sep="\t",col.names=c("count_2","family", "class"))[,c(1,2)]
late2C_3.rawCounts <- read.table(file="Reci.late2cell-3.rep.count.bed", sep="\t",col.names=c("count_3","family", "class"))[,c(1,2)]
late2C_4.rawCounts <- read.table(file="Reci.late2cell-4.rep.count.bed", sep="\t",col.names=c("count_4","family", "class"))[,c(1,2)]
late2C_5.rawCounts <- read.table(file="Reci.late2cell-5.rep.count.bed", sep="\t",col.names=c("count_5","family", "class"))[,c(1,2)]
late2C_6.rawCounts <- read.table(file="Reci.late2cell-6.rep.count.bed", sep="\t",col.names=c("count_6","family", "class"))[,c(1,2)]
late2C_7.rawCounts <- read.table(file="Reci.late2cell-7.rep.count.bed", sep="\t",col.names=c("count_7","family", "class"))[,c(1,2)]
late2C_8.rawCounts <- read.table(file="Reci.late2cell-8.rep.count.bed", sep="\t",col.names=c("count_8","family", "class"))[,c(1,2)]
late2C_9.rawCounts <- read.table(file="Reci.late2cell-9.rep.count.bed", sep="\t",col.names=c("count_9","family", "class"))[,c(1,2)]
late2C_10.rawCounts <- read.table(file="Reci.late2cell-10.rep.count.bed", sep="\t",col.names=c("count_10","family", "class"))[,c(1,2)]
#remove duplicated row
late2C_1.rawCounts <- late2C_1.rawCounts[!duplicated(late2C_1.rawCounts$family), ]
late2C_2.rawCounts <- late2C_2.rawCounts[!duplicated(late2C_2.rawCounts$family), ]
late2C_3.rawCounts <- late2C_3.rawCounts[!duplicated(late2C_3.rawCounts$family), ]
late2C_4.rawCounts <- late2C_4.rawCounts[!duplicated(late2C_4.rawCounts$family), ]
late2C_5.rawCounts <- late2C_5.rawCounts[!duplicated(late2C_5.rawCounts$family), ]
late2C_6.rawCounts <- late2C_6.rawCounts[!duplicated(late2C_6.rawCounts$family), ]
late2C_7.rawCounts <- late2C_7.rawCounts[!duplicated(late2C_7.rawCounts$family), ]
late2C_8.rawCounts <- late2C_8.rawCounts[!duplicated(late2C_8.rawCounts$family), ]
late2C_9.rawCounts <- late2C_9.rawCounts[!duplicated(late2C_9.rawCounts$family), ]
late2C_10.rawCounts <- late2C_10.rawCounts[!duplicated(late2C_10.rawCounts$family), ]
#merge
late2C_merge1 <- merge(late2C_1.rawCounts, late2C_2.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
late2C_merge2 <- merge(late2C_merge1, late2C_3.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
late2C_merge3 <- merge(late2C_merge2, late2C_4.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
late2C_merge4 <- merge(late2C_merge3, late2C_5.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
late2C_merge5 <- merge(late2C_merge4, late2C_6.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
late2C_merge6 <- merge(late2C_merge5, late2C_7.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
late2C_merge7 <- merge(late2C_merge6, late2C_8.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
late2C_merge8 <- merge(late2C_merge7, late2C_9.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
late2C_merge9 <- merge(late2C_merge8, late2C_10.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)

colnames(late2C_merge9) <- c("family", paste("late2C", c(1:10), sep="-"))
rm(list=setdiff(ls(), c("MC.cpm", "late2C_merge9")))

#4cell
N4C_1.rawCounts <- read.table(file="Reci.4cell-1.rep.count.bed", sep="\t",col.names=c("count_1","family", "class"))[,c(1,2)]
N4C_2.rawCounts <- read.table(file="Reci.4cell-2.rep.count.bed", sep="\t",col.names=c("count_2","family", "class"))[,c(1,2)]
N4C_3.rawCounts <- read.table(file="Reci.4cell-3.rep.count.bed", sep="\t",col.names=c("count_3","family", "class"))[,c(1,2)]
N4C_4.rawCounts <- read.table(file="Reci.4cell-4.rep.count.bed", sep="\t",col.names=c("count_4","family", "class"))[,c(1,2)]
N4C_5.rawCounts <- read.table(file="Reci.4cell-5.rep.count.bed", sep="\t",col.names=c("count_5","family", "class"))[,c(1,2)]
N4C_6.rawCounts <- read.table(file="Reci.4cell-6.rep.count.bed", sep="\t",col.names=c("count_6","family", "class"))[,c(1,2)]
N4C_7.rawCounts <- read.table(file="Reci.4cell-7.rep.count.bed", sep="\t",col.names=c("count_7","family", "class"))[,c(1,2)]
N4C_9.rawCounts <- read.table(file="Reci.4cell-9.rep.count.bed", sep="\t",col.names=c("count_9","family", "class"))[,c(1,2)]
#remove duplicated row
N4C_1.rawCounts <- N4C_1.rawCounts[!duplicated(N4C_1.rawCounts$family), ]
N4C_2.rawCounts <- N4C_2.rawCounts[!duplicated(N4C_2.rawCounts$family), ]
N4C_3.rawCounts <- N4C_3.rawCounts[!duplicated(N4C_3.rawCounts$family), ]
N4C_4.rawCounts <- N4C_4.rawCounts[!duplicated(N4C_4.rawCounts$family), ]
N4C_5.rawCounts <- N4C_5.rawCounts[!duplicated(N4C_5.rawCounts$family), ]
N4C_6.rawCounts <- N4C_6.rawCounts[!duplicated(N4C_6.rawCounts$family), ]
N4C_7.rawCounts <- N4C_7.rawCounts[!duplicated(N4C_7.rawCounts$family), ]
N4C_9.rawCounts <- N4C_9.rawCounts[!duplicated(N4C_9.rawCounts$family), ]
#merge
N4C_merge1 <- merge(N4C_1.rawCounts, N4C_2.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N4C_merge2 <- merge(N4C_merge1, N4C_3.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N4C_merge3 <- merge(N4C_merge2, N4C_4.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N4C_merge4 <- merge(N4C_merge3, N4C_5.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N4C_merge5 <- merge(N4C_merge4, N4C_6.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N4C_merge6 <- merge(N4C_merge5, N4C_7.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N4C_merge7 <- merge(N4C_merge6, N4C_9.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)

colnames(N4C_merge7) <- c("family", paste("N4C", c(1:8), sep="-"))
rm(list=setdiff(ls(), c("MC.cpm", "late2C_merge9","N4C_merge7")))

#8cell
N8C_1.rawCounts <- read.table(file="Reci.8cell-1.rep.count.bed", sep="\t",col.names=c("count_1","family", "class"))[,c(1,2)]
N8C_2.rawCounts <- read.table(file="Reci.8cell-2.rep.count.bed", sep="\t",col.names=c("count_2","family", "class"))[,c(1,2)]
N8C_3.rawCounts <- read.table(file="Reci.8cell-3.rep.count.bed", sep="\t",col.names=c("count_3","family", "class"))[,c(1,2)]
N8C_4.rawCounts <- read.table(file="Reci.8cell-4.rep.count.bed", sep="\t",col.names=c("count_4","family", "class"))[,c(1,2)]
N8C_5.rawCounts <- read.table(file="Reci.8cell-5.rep.count.bed", sep="\t",col.names=c("count_5","family", "class"))[,c(1,2)]
N8C_6.rawCounts <- read.table(file="Reci.8cell-6.rep.count.bed", sep="\t",col.names=c("count_6","family", "class"))[,c(1,2)]
N8C_7.rawCounts <- read.table(file="Reci.8cell-7.rep.count.bed", sep="\t",col.names=c("count_7","family", "class"))[,c(1,2)]
N8C_8.rawCounts <- read.table(file="Reci.8cell-8.rep.count.bed", sep="\t",col.names=c("count_8","family", "class"))[,c(1,2)]
N8C_9.rawCounts <- read.table(file="Reci.8cell-9.rep.count.bed", sep="\t",col.names=c("count_9","family", "class"))[,c(1,2)]
N8C_10.rawCounts <- read.table(file="Reci.8cell-10.rep.count.bed", sep="\t",col.names=c("count_10","family", "class"))[,c(1,2)]
N8C_11.rawCounts <- read.table(file="Reci.8cell-11.rep.count.bed", sep="\t",col.names=c("count_11","family", "class"))[,c(1,2)]
#remove duplicated row
N8C_1.rawCounts <- N8C_1.rawCounts[!duplicated(N8C_1.rawCounts$family), ]
N8C_2.rawCounts <- N8C_2.rawCounts[!duplicated(N8C_2.rawCounts$family), ]
N8C_3.rawCounts <- N8C_3.rawCounts[!duplicated(N8C_3.rawCounts$family), ]
N8C_4.rawCounts <- N8C_4.rawCounts[!duplicated(N8C_4.rawCounts$family), ]
N8C_5.rawCounts <- N8C_5.rawCounts[!duplicated(N8C_5.rawCounts$family), ]
N8C_6.rawCounts <- N8C_6.rawCounts[!duplicated(N8C_6.rawCounts$family), ]
N8C_7.rawCounts <- N8C_7.rawCounts[!duplicated(N8C_7.rawCounts$family), ]
N8C_8.rawCounts <- N8C_8.rawCounts[!duplicated(N8C_8.rawCounts$family), ]
N8C_9.rawCounts <- N8C_9.rawCounts[!duplicated(N8C_9.rawCounts$family), ]
N8C_10.rawCounts <- N8C_10.rawCounts[!duplicated(N8C_10.rawCounts$family), ]
N8C_11.rawCounts <- N8C_11.rawCounts[!duplicated(N8C_11.rawCounts$family), ]
#merge
N8C_merge5 <- merge(N8C_1.rawCounts, N8C_2.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N8C_merge6 <- merge(N8C_merge5, N8C_3.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N8C_merge7 <- merge(N8C_merge6, N8C_4.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N8C_merge8 <- merge(N8C_merge7, N8C_5.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N8C_merge9 <- merge(N8C_merge8, N8C_6.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N8C_merge10 <- merge(N8C_merge9, N8C_7.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N8C_merge11 <- merge(N8C_merge10, N8C_8.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N8C_merge12 <- merge(N8C_merge11, N8C_9.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N8C_merge13 <- merge(N8C_merge12, N8C_10.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N8C_merge14 <- merge(N8C_merge13, N8C_11.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)

colnames(N8C_merge14) <- c("family", paste("N8C", c(1:11), sep="-"))
rm(list=setdiff(ls(), c("MC.cpm", "late2C_merge9","N4C_merge7","N8C_merge14")))

#16cell
N16C_1.rawCounts <- read.table(file="Reci.16cell-1.rep.count.bed", sep="\t",col.names=c("count_1","family", "class"))[,c(1,2)]
N16C_2.rawCounts <- read.table(file="Reci.16cell-2.rep.count.bed", sep="\t",col.names=c("count_2","family", "class"))[,c(1,2)]
N16C_3.rawCounts <- read.table(file="Reci.16cell-3.rep.count.bed", sep="\t",col.names=c("count_3","family", "class"))[,c(1,2)]
N16C_4.rawCounts <- read.table(file="Reci.16cell-4.rep.count.bed", sep="\t",col.names=c("count_4","family", "class"))[,c(1,2)]
N16C_5.rawCounts <- read.table(file="Reci.16cell-5.rep.count.bed", sep="\t",col.names=c("count_5","family", "class"))[,c(1,2)]
N16C_6.rawCounts <- read.table(file="Reci.16cell-6.rep.count.bed", sep="\t",col.names=c("count_6","family", "class"))[,c(1,2)]
N16C_7.rawCounts <- read.table(file="Reci.16cell-7.rep.count.bed", sep="\t",col.names=c("count_7","family", "class"))[,c(1,2)]
N16C_8.rawCounts <- read.table(file="Reci.16cell-8.rep.count.bed", sep="\t",col.names=c("count_8","family", "class"))[,c(1,2)]
N16C_10.rawCounts <- read.table(file="Reci.16cell-10.rep.count.bed", sep="\t",col.names=c("count_10","family", "class"))[,c(1,2)]
N16C_11.rawCounts <- read.table(file="Reci.16cell-11.rep.count.bed", sep="\t",col.names=c("count_11","family", "class"))[,c(1,2)]
N16C_12.rawCounts <- read.table(file="Reci.16cell-12.rep.count.bed", sep="\t",col.names=c("count_12","family", "class"))[,c(1,2)]
N16C_13.rawCounts <- read.table(file="Reci.16cell-13.rep.count.bed", sep="\t",col.names=c("count_13","family", "class"))[,c(1,2)]
N16C_14.rawCounts <- read.table(file="Reci.16cell-14.rep.count.bed", sep="\t",col.names=c("count_14","family", "class"))[,c(1,2)]
N16C_15.rawCounts <- read.table(file="Reci.16cell-15.rep.count.bed", sep="\t",col.names=c("count_15","family", "class"))[,c(1,2)]
#remove duplicated row
N16C_1.rawCounts <- N16C_1.rawCounts[!duplicated(N16C_1.rawCounts$family), ]
N16C_2.rawCounts <- N16C_2.rawCounts[!duplicated(N16C_2.rawCounts$family), ]
N16C_3.rawCounts <- N16C_3.rawCounts[!duplicated(N16C_3.rawCounts$family), ]
N16C_4.rawCounts <- N16C_4.rawCounts[!duplicated(N16C_4.rawCounts$family), ]
N16C_5.rawCounts <- N16C_5.rawCounts[!duplicated(N16C_5.rawCounts$family), ]
N16C_6.rawCounts <- N16C_6.rawCounts[!duplicated(N16C_6.rawCounts$family), ]
N16C_7.rawCounts <- N16C_7.rawCounts[!duplicated(N16C_7.rawCounts$family), ]
N16C_8.rawCounts <- N16C_8.rawCounts[!duplicated(N16C_8.rawCounts$family), ]
N16C_10.rawCounts <- N16C_10.rawCounts[!duplicated(N16C_10.rawCounts$family), ]
N16C_11.rawCounts <- N16C_11.rawCounts[!duplicated(N16C_11.rawCounts$family), ]
N16C_12.rawCounts <- N16C_12.rawCounts[!duplicated(N16C_12.rawCounts$family), ]
N16C_13.rawCounts <- N16C_13.rawCounts[!duplicated(N16C_13.rawCounts$family), ]
N16C_14.rawCounts <- N16C_14.rawCounts[!duplicated(N16C_14.rawCounts$family), ]
N16C_15.rawCounts <- N16C_15.rawCounts[!duplicated(N16C_15.rawCounts$family), ]
#merge
N16C_merge1 <- merge(N16C_1.rawCounts, N16C_2.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N16C_merge2 <- merge(N16C_merge1, N16C_3.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N16C_merge3 <- merge(N16C_merge2, N16C_4.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N16C_merge4 <- merge(N16C_merge3, N16C_5.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N16C_merge5 <- merge(N16C_merge4, N16C_6.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N16C_merge6 <- merge(N16C_merge5, N16C_7.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N16C_merge7 <- merge(N16C_merge6, N16C_8.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N16C_merge8 <- merge(N16C_merge7, N16C_10.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N16C_merge9 <- merge(N16C_merge8, N16C_11.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N16C_merge10 <- merge(N16C_merge9, N16C_12.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N16C_merge11 <- merge(N16C_merge10, N16C_13.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N16C_merge12 <- merge(N16C_merge11, N16C_14.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
N16C_merge13 <- merge(N16C_merge12, N16C_15.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)

colnames(N16C_merge13) <- c("family", paste("N16C", c(1:14), sep="-"))
rm(list=setdiff(ls(), c("MC.cpm", "late2C_merge9","N4C_merge7","N8C_merge14","N16C_merge13")))

#earlyBlast
earlyBlast_1.rawCounts <- read.table(file="Reci.earlyBlast-1.rep.count.bed", sep="\t",col.names=c("count_1","family", "class"))[,c(1,2)]
earlyBlast_2.rawCounts <- read.table(file="Reci.earlyBlast-2.rep.count.bed", sep="\t",col.names=c("count_2","family", "class"))[,c(1,2)]
earlyBlast_3.rawCounts <- read.table(file="Reci.earlyBlast-3.rep.count.bed", sep="\t",col.names=c("count_3","family", "class"))[,c(1,2)]
earlyBlast_4.rawCounts <- read.table(file="Reci.earlyBlast-4.rep.count.bed", sep="\t",col.names=c("count_4","family", "class"))[,c(1,2)]
earlyBlast_5.rawCounts <- read.table(file="Reci.earlyBlast-5.rep.count.bed", sep="\t",col.names=c("count_5","family", "class"))[,c(1,2)]
earlyBlast_6.rawCounts <- read.table(file="Reci.earlyBlast-6.rep.count.bed", sep="\t",col.names=c("count_6","family", "class"))[,c(1,2)]
earlyBlast_7.rawCounts <- read.table(file="Reci.earlyBlast-7.rep.count.bed", sep="\t",col.names=c("count_7","family", "class"))[,c(1,2)]
earlyBlast_9.rawCounts <- read.table(file="Reci.earlyBlast-9.rep.count.bed", sep="\t",col.names=c("count_9","family", "class"))[,c(1,2)]
earlyBlast_11.rawCounts <- read.table(file="Reci.earlyBlast-11.rep.count.bed", sep="\t",col.names=c("count_11","family", "class"))[,c(1,2)]
earlyBlast_12.rawCounts <- read.table(file="Reci.earlyBlast-12.rep.count.bed", sep="\t",col.names=c("count_12","family", "class"))[,c(1,2)]
earlyBlast_13.rawCounts <- read.table(file="Reci.earlyBlast-13.rep.count.bed", sep="\t",col.names=c("count_13","family", "class"))[,c(1,2)]
earlyBlast_14.rawCounts <- read.table(file="Reci.earlyBlast-14.rep.count.bed", sep="\t",col.names=c("count_14","family", "class"))[,c(1,2)]
earlyBlast_15.rawCounts <- read.table(file="Reci.earlyBlast-15.rep.count.bed", sep="\t",col.names=c("count_15","family", "class"))[,c(1,2)]
#remove duplicated row
earlyBlast_1.rawCounts <- earlyBlast_1.rawCounts[!duplicated(earlyBlast_1.rawCounts$family), ]
earlyBlast_2.rawCounts <- earlyBlast_2.rawCounts[!duplicated(earlyBlast_2.rawCounts$family), ]
earlyBlast_3.rawCounts <- earlyBlast_3.rawCounts[!duplicated(earlyBlast_3.rawCounts$family), ]
earlyBlast_4.rawCounts <- earlyBlast_4.rawCounts[!duplicated(earlyBlast_4.rawCounts$family), ]
earlyBlast_5.rawCounts <- earlyBlast_5.rawCounts[!duplicated(earlyBlast_5.rawCounts$family), ]
earlyBlast_6.rawCounts <- earlyBlast_6.rawCounts[!duplicated(earlyBlast_6.rawCounts$family), ]
earlyBlast_7.rawCounts <- earlyBlast_7.rawCounts[!duplicated(earlyBlast_7.rawCounts$family), ]
earlyBlast_9.rawCounts <- earlyBlast_9.rawCounts[!duplicated(earlyBlast_9.rawCounts$family), ]
earlyBlast_11.rawCounts <- earlyBlast_11.rawCounts[!duplicated(earlyBlast_11.rawCounts$family), ]
earlyBlast_12.rawCounts <- earlyBlast_12.rawCounts[!duplicated(earlyBlast_12.rawCounts$family), ]
earlyBlast_13.rawCounts <- earlyBlast_13.rawCounts[!duplicated(earlyBlast_13.rawCounts$family), ]
earlyBlast_14.rawCounts <- earlyBlast_14.rawCounts[!duplicated(earlyBlast_14.rawCounts$family), ]
earlyBlast_15.rawCounts <- earlyBlast_15.rawCounts[!duplicated(earlyBlast_15.rawCounts$family), ]
#merge
earlyBlast_merge1 <- merge(earlyBlast_1.rawCounts, earlyBlast_2.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
earlyBlast_merge2 <- merge(earlyBlast_merge1, earlyBlast_3.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
earlyBlast_merge3 <- merge(earlyBlast_merge2, earlyBlast_4.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
earlyBlast_merge4 <- merge(earlyBlast_merge3, earlyBlast_5.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
earlyBlast_merge5 <- merge(earlyBlast_merge4, earlyBlast_6.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
earlyBlast_merge6 <- merge(earlyBlast_merge5, earlyBlast_7.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
earlyBlast_merge7 <- merge(earlyBlast_merge6, earlyBlast_9.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
earlyBlast_merge8 <- merge(earlyBlast_merge7, earlyBlast_11.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
earlyBlast_merge9 <- merge(earlyBlast_merge8, earlyBlast_12.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
earlyBlast_merge10 <- merge(earlyBlast_merge9, earlyBlast_13.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
earlyBlast_merge11 <- merge(earlyBlast_merge10, earlyBlast_14.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)
earlyBlast_merge12 <- merge(earlyBlast_merge11, earlyBlast_15.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)

colnames(earlyBlast_merge12) <- c("family", paste("earlyBlast", c(1:13), sep="-"))
rm(list=setdiff(ls(), c("MC.cpm", "late2C_merge9","N4C_merge7","N8C_merge14","N16C_merge13","earlyBlast_merge12")))

#merge all

late2C_4C_merge <- merge(late2C_merge9, N4C_merge7, by.x="family", by.y="family", all.x=T, all.y=T)
late2C_8C_merge <- merge(late2C_4C_merge, N8C_merge14, by.x="family", by.y="family", all.x=T, all.y=T)
late2C_16C_merge <- merge(late2C_8C_merge, N16C_merge13, by.x="family", by.y="family", all.x=T, all.y=T)
late2C_eB_merge <- merge(late2C_16C_merge, earlyBlast_merge12, by.x="family", by.y="family", all.x=T, all.y=T)
late2C_eB_merge[is.na(late2C_eB_merge)] <- 0
CM.rawCounts <- late2C_eB_merge[ ,-1]
rownames(CM.rawCounts) <- late2C_eB_merge$family

rm(list=setdiff(ls(), c("MC.cpm", "CM.rawCounts")))


#total counts (genes and repeats, non-rRNA)
repeat_reads_ratio <- read.table(file="CM_repeat_reads_ratio_200hits.txt", sep="\t", header=T)
total.read.counts <- repeat_reads_ratio$non.rRNA_reads
CM.cpm <- data.frame(CM.rawCounts[1] * 1000000 / total.read.counts[1])
colnames(CM.cpm) <- colnames(CM.rawCounts)[1]
for (i in 2:ncol(CM.rawCounts)){
  CM.cpm[ ,i] <- CM.rawCounts[i] * 1000000 / total.read.counts[i]
  colnames(CM.cpm)[i] <- colnames(CM.rawCounts)[i]
}

rm(list=setdiff(ls(), c("MC.cpm", "CM.cpm")))


#PCA and clustering plot below
#rename samples according to stages
MC.colnames <- c(paste("zygote", c(1:4), sep="-"),
                 paste("early2C", c(1:6), sep="-"),
                 paste("late2C", c(1:10), sep="-"),
                 paste("4C", c(1:17), sep="-"),
                 paste("8C", c(1:15), sep="-"),
                 paste("16C", c(1:8), sep="-"),
                 paste("earlyBlast.", c(1:11), sep="-"))

CM.colnames <- c(paste("late2C", c(11:20), sep="-"),
                 paste("4C", c(18:25), sep="-"),
                 paste("8C", c(16:26), sep="-"),
                 paste("16C", c(9:22), sep="-"),
                 paste("earlyBlast.", c(12:24), sep="-"))

MC.dat <- data.frame(family=rownames(MC.cpm), MC.cpm)
CM.dat <- data.frame(family=rownames(CM.cpm), CM.cpm)
#only use the repeats expressed in both two reciprocal crosses.
comp.cpm <- merge(MC.dat, CM.dat, by.x="family", by.y="family")
DATA <- comp.cpm[ ,-1]
rownames(DATA) <- comp.cpm$family
colnames(DATA) <- c(MC.colnames, CM.colnames)
Cross <- c(rep("MC",71), rep("CM",56))

# load ggplot2 and start to calculate PCA

library(ggplot2)
nS<-ncol(DATA) #number of samples
nG<-nrow(DATA) #number of genes

sample.names<-colnames(DATA)
stage<-unlist(lapply(strsplit(sample.names,"-"),function(x) x[1]))
embryo<-unlist(lapply(strsplit(sample.names,"\\."),function(x) x[1]))


stages<-unique(stage)
# have 11 different stages, define 5 colors for those.
coldef.stage<-c("dark grey","red","green","blue","orange","purple","brown")
col.stage<-mat.or.vec(1,nS)
for (i in 1:length(stages)){
  idx<-which(stage==stages[i])
  col.stage[idx]<-coldef.stage[i]
}

# transform the gene counts to log10 base
log.DATA <- log10(as.matrix(DATA)+1)
DATA.pca <- prcomp(t(log.DATA)) 
# Use PC1 and PC2 to make data table for visualization
PC.table <- data.frame(DATA.pca$x[ ,c(1,2)])
# find the variations from summery() function, just FYI
summary(DATA.pca)

# Add color code for each sample into PC.table
PC.table$Stage <- stage
PC.table$Stage <- factor(PC.table$Stage, levels = stages)
PC.table$Cross <- Cross

PC.table$Cross <- factor(PC.table$Cross, levels=c("MC","CM"))

# PCA Plot
ggplot(PC.table) + 
  geom_point(aes(x=PC1, y=PC2, color= Stage, shape=Cross), size=2.0) + 
  theme_bw() +
  scale_colour_manual(values = coldef.stage) +
  theme(axis.text=element_text(size=rel(1.0), face="bold")) +
  theme(legend.position="right") +
  theme(legend.title=element_blank()) + 
  theme(legend.key = element_blank()) +
  theme(legend.text=element_text(size = rel(1), face="bold")) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1)) +
  xlab("PC1") +
  ylab("PC2") +
  theme(axis.title.y = element_text(size = rel(1.3), face="bold")) +
  theme(axis.title.x = element_text(size = rel(1.3), face="bold"))




#Panel d. Total repeat reads percentage in MC cross embryos.------------------------------------------
rm(list=ls())
dat <- read.table(file="processed/MC_repeat_reads_ratio_200hits.txt", sep="\t", header=T)
#Add stage
dat$stage <- unlist(lapply(strsplit(dat$sample,"-"),function(x) x[1]))
dat$stage <- factor(dat$stage, levels=c(unique(dat$stage)))

#Boxplot showing repeat reads fraction
ggplot(dat, aes(x=stage, y=percent,fill="dodgerblue2")) +
  geom_boxplot(alpha=0.6, outlier.shape=NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.7),
             size=1.7, pch=21) +
  scale_fill_manual(values="dodgerblue2") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        axis.title.x=element_blank(),
        #legend.text=element_text(size=12),
        legend.title=element_blank(), 
        legend.key = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_line(color="black"),
        axis.line.x = element_line(color="black"),
        legend.text=element_text(size = rel(1)))
  

#Panel e. Percent of reads from 4 types of repeat types (MC) -----------------------------------------------------
LINE.dat <- data.frame(stage=dat$stage, 
                       percent=dat$LINE*100/(dat$LINE+dat$SINE+dat$LTR+dat$DNA),
                       type="LINE")
SINE.dat <- data.frame(stage=dat$stage, 
                       percent=dat$SINE*100/(dat$LINE+dat$SINE+dat$LTR+dat$DNA),
                       type="SINE")
LTR.dat <- data.frame(stage=dat$stage, 
                       percent=dat$LTR*100/(dat$LINE+dat$SINE+dat$LTR+dat$DNA),
                       type="LTR")
DNA.dat <- data.frame(stage=dat$stage, 
                      percent=dat$DNA*100/(dat$LINE+dat$SINE+dat$LTR+dat$DNA),
                      type="DNA")


LINE.mean <- 0
SINE.mean <- 0
LTR.mean <- 0
DNA.mean <- 0
Start <- 1
for (i in 1:length(table(dat$stage))) {
  End <- Start + table(dat$stage)[i] -1
  LINE.mean[i] <- mean(LINE.dat$percent[Start:End])
  SINE.mean[i] <- mean(SINE.dat$percent[Start:End])
  LTR.mean[i] <- mean(LTR.dat$percent[Start:End])
  DNA.mean[i] <- mean(DNA.dat$percent[Start:End])
  Start=End+1
}

LINE.mean.dat <- data.frame(stage=unique(dat$stage),mean_percent=LINE.mean,type="LINE")
SINE.mean.dat <- data.frame(stage=unique(dat$stage),mean_percent=SINE.mean,type="SINE")
LTR.mean.dat <- data.frame(stage=unique(dat$stage),mean_percent=LTR.mean,type="LTR")
DNA.mean.dat <- data.frame(stage=unique(dat$stage),mean_percent=DNA.mean,type="DNA")



all.repeats.dat <- rbind(LINE.mean.dat, SINE.mean.dat, LTR.mean.dat, DNA.mean.dat)


#Proportional stacked bar graph showing proportion of 4 types of repeats
ggplot(all.repeats.dat, aes(x=stage, y=mean_percent, fill=type)) +
  geom_col(color="black", position="fill") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        axis.title.x=element_blank(),
        #legend.text=element_text(size=12),
        legend.title=element_blank(), 
        legend.key = element_blank(),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_line(color="black"),
        axis.line.x = element_line(color="black"),
        legend.text=element_text(size = rel(1)))



#Panel f. Percent change of SINE, LINE, LTR, DNA in MC cross embryos. --------------------------------------------------------
rm(list=ls())

dat <- read.table(file="processed/MC_repeat_reads_ratio_200hits.txt", sep="\t", header=T)
#Add stage
dat$stage <- unlist(lapply(strsplit(dat$sample,"-"),function(x) x[1]))
dat$stage <- factor(dat$stage, levels=c(unique(dat$stage)))
# SINE percentage
dat$SINE_percet <- dat$SINE * 100 / dat$non.rRNA_reads
dat$LINE_percet <- dat$LINE * 100 / dat$non.rRNA_reads
dat$LTR_percet <- dat$LTR * 100 / dat$non.rRNA_reads
dat$DNA_percet <- dat$DNA * 100 / dat$non.rRNA_reads

SINE.dat <- data.frame(percent=dat$SINE_percet, type="SINE", stage=dat$stage)
LINE.dat <- data.frame(percent=dat$LINE_percet, type="LINE", stage=dat$stage)
LTR.dat <- data.frame(percent=dat$LTR_percet, type="LTR", stage=dat$stage)
DNA.dat <- data.frame(percent=dat$DNA_percet, type="DNA", stage=dat$stage)


SINE.zygote <- data.frame(Mean=mean(SINE.dat[which(SINE.dat$stage=="zygote"),1]), SE=std.error(SINE.dat[which(SINE.dat$stage=="zygote"),1]), stage="zygote", repeats="SINE")
SINE.early2C <- data.frame(Mean=mean(SINE.dat[which(SINE.dat$stage=="early2cell"),1]), SE=std.error(SINE.dat[which(SINE.dat$stage=="early2cell"),1]), stage="early2C", repeats="SINE")
SINE.late2C <- data.frame(Mean=mean(SINE.dat[which(SINE.dat$stage=="late2cell"),1]), SE=std.error(SINE.dat[which(SINE.dat$stage=="late2cell"),1]), stage="late2C", repeats="SINE")
SINE.4C <- data.frame(Mean=mean(SINE.dat[which(SINE.dat$stage=="4cell"),1]), SE=std.error(SINE.dat[which(SINE.dat$stage=="4cell"),1]), stage="4C", repeats="SINE")
SINE.8C <- data.frame(Mean=mean(SINE.dat[which(SINE.dat$stage=="8cell"),1]), SE=std.error(SINE.dat[which(SINE.dat$stage=="8cell"),1]), stage="8C", repeats="SINE")
SINE.16C <- data.frame(Mean=mean(SINE.dat[which(SINE.dat$stage=="16cell"),1]), SE=std.error(SINE.dat[which(SINE.dat$stage=="16cell"),1]), stage="16C", repeats="SINE")
SINE.earlyB <- data.frame(Mean=mean(SINE.dat[which(SINE.dat$stage=="earlyBlast"),1]), SE=std.error(SINE.dat[which(SINE.dat$stage=="earlyBlast"),1]), stage="earlyB", repeats="SINE")

LTR.zygote <- data.frame(Mean=mean(LTR.dat[which(LTR.dat$stage=="zygote"),1]), SE=std.error(SINE.dat[which(LTR.dat$stage=="zygote"),1]), stage="zygote", repeats="LTR")
LTR.early2C <- data.frame(Mean=mean(LTR.dat[which(LTR.dat$stage=="early2cell"),1]), SE=std.error(SINE.dat[which(LTR.dat$stage=="early2cell"),1]), stage="early2C", repeats="LTR")
LTR.late2C <- data.frame(Mean=mean(LTR.dat[which(LTR.dat$stage=="late2cell"),1]), SE=std.error(SINE.dat[which(LTR.dat$stage=="late2cell"),1]), stage="late2C", repeats="LTR")
LTR.4C <- data.frame(Mean=mean(LTR.dat[which(LTR.dat$stage=="4cell"),1]), SE=std.error(SINE.dat[which(LTR.dat$stage=="4cell"),1]), stage="4C", repeats="LTR")
LTR.8C <- data.frame(Mean=mean(LTR.dat[which(LTR.dat$stage=="8cell"),1]), SE=std.error(SINE.dat[which(LTR.dat$stage=="8cell"),1]), stage="8C", repeats="LTR")
LTR.16C <- data.frame(Mean=mean(LTR.dat[which(LTR.dat$stage=="16cell"),1]), SE=std.error(SINE.dat[which(LTR.dat$stage=="16cell"),1]), stage="16C", repeats="LTR")
LTR.earlyB <- data.frame(Mean=mean(LTR.dat[which(LTR.dat$stage=="earlyBlast"),1]), SE=std.error(SINE.dat[which(LTR.dat$stage=="earlyBlast"),1]), stage="earlyB", repeats="LTR")

LINE.zygote <- data.frame(Mean=mean(LINE.dat[which(LINE.dat$stage=="zygote"),1]), SE=std.error(LINE.dat[which(LINE.dat$stage=="zygote"),1]), stage="zygote", repeats="LINE")
LINE.early2C <- data.frame(Mean=mean(LINE.dat[which(LINE.dat$stage=="early2cell"),1]), SE=std.error(LINE.dat[which(LINE.dat$stage=="early2cell"),1]), stage="early2C", repeats="LINE")
LINE.late2C <- data.frame(Mean=mean(LINE.dat[which(LINE.dat$stage=="late2cell"),1]), SE=std.error(LINE.dat[which(LINE.dat$stage=="late2cell"),1]), stage="late2C", repeats="LINE")
LINE.4C <- data.frame(Mean=mean(LINE.dat[which(LINE.dat$stage=="4cell"),1]), SE=std.error(LINE.dat[which(LINE.dat$stage=="4cell"),1]), stage="4C", repeats="LINE")
LINE.8C <- data.frame(Mean=mean(LINE.dat[which(LINE.dat$stage=="8cell"),1]), SE=std.error(LINE.dat[which(LINE.dat$stage=="8cell"),1]), stage="8C", repeats="LINE")
LINE.16C <- data.frame(Mean=mean(LINE.dat[which(LINE.dat$stage=="16cell"),1]), SE=std.error(LINE.dat[which(LINE.dat$stage=="16cell"),1]), stage="16C", repeats="LINE")
LINE.earlyB <- data.frame(Mean=mean(LINE.dat[which(LINE.dat$stage=="earlyBlast"),1]), SE=std.error(LINE.dat[which(LINE.dat$stage=="earlyBlast"),1]), stage="earlyB", repeats="LINE")

DNA.zygote <- data.frame(Mean=mean(DNA.dat[which(DNA.dat$stage=="zygote"),1]), SE=std.error(DNA.dat[which(DNA.dat$stage=="zygote"),1]), stage="zygote", repeats="DNA")
DNA.early2C <- data.frame(Mean=mean(DNA.dat[which(DNA.dat$stage=="early2cell"),1]), SE=std.error(DNA.dat[which(DNA.dat$stage=="early2cell"),1]), stage="early2C", repeats="DNA")
DNA.late2C <- data.frame(Mean=mean(DNA.dat[which(DNA.dat$stage=="late2cell"),1]), SE=std.error(DNA.dat[which(DNA.dat$stage=="late2cell"),1]), stage="late2C", repeats="DNA")
DNA.4C <- data.frame(Mean=mean(DNA.dat[which(DNA.dat$stage=="4cell"),1]), SE=std.error(DNA.dat[which(DNA.dat$stage=="4cell"),1]), stage="4C", repeats="DNA")
DNA.8C <- data.frame(Mean=mean(DNA.dat[which(DNA.dat$stage=="8cell"),1]), SE=std.error(DNA.dat[which(DNA.dat$stage=="8cell"),1]), stage="8C", repeats="DNA")
DNA.16C <- data.frame(Mean=mean(DNA.dat[which(DNA.dat$stage=="16cell"),1]), SE=std.error(DNA.dat[which(DNA.dat$stage=="16cell"),1]), stage="16C", repeats="DNA")
DNA.earlyB <- data.frame(Mean=mean(DNA.dat[which(DNA.dat$stage=="earlyBlast"),1]), SE=std.error(DNA.dat[which(DNA.dat$stage=="earlyBlast"),1]), stage="earlyB", repeats="DNA")

combined.dat <- rbind(SINE.zygote, SINE.early2C, SINE.late2C, SINE.4C, SINE.8C, SINE.16C, SINE.earlyB,
                      LTR.zygote, LTR.early2C, LTR.late2C, LTR.4C, LTR.8C, LTR.16C, LTR.earlyB,
                      LINE.zygote, LINE.early2C, LINE.late2C, LINE.4C, LINE.8C, LINE.16C, LINE.earlyB,
                      DNA.zygote, DNA.early2C, DNA.late2C, DNA.4C, DNA.8C, DNA.16C, DNA.earlyB)

combined.dat$stage <- factor(combined.dat$stage, levels=c("zygote","early2C","late2C","4C","8C","16C","earlyB"))
combined.dat$repeats <- factor(combined.dat$repeats, levels=c("LTR","SINE","DNA","LINE"))

group.colors <- c(SINE = "#9999FF", LTR = "#66CC33", DNA ="red", LINE = "dodgerblue2")



#Boxplot showing SINEs and LTR reads fraction
ggplot(combined.dat, aes(x=stage, y=Mean,group=repeats)) +
  geom_line(aes(color=repeats), size=1.2) +
  geom_point(aes(color=repeats), size=2) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE), width = 0.2, size=0.5) +
  scale_color_manual(values=group.colors) +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        axis.title.x=element_blank(),
        #legend.text=element_text(size=12),
        legend.title=element_blank(), 
        legend.key = element_blank(),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_line(color="black"),
        axis.line.x = element_line(color="black"),
        legend.text=element_text(size = rel(1)))



#t test between SINE and LTR at late2C
SINE.late2C <- SINE.dat[which(SINE.dat$stage=="late2cell"), ]
LTR.late2C <- LTR.dat[which(LTR.dat$stage=="late2cell"), ]
#paired t-test
t.test(SINE.late2C$percent,LTR.late2C$percent,paired=TRUE)

#t test between SINE and LTR at 4C
SINE.4C <- SINE.dat[which(SINE.dat$stage=="4cell"), ]
LTR.4C <- LTR.dat[which(LTR.dat$stage=="4cell"), ]
#paired t-test
t.test(SINE.4C$percent,LTR.4C$percent,paired=TRUE)




#Panel g (left). overall allelic repeat count change from 4C to earlyB, X vs auto (average) ------------------------------------------
rm(list=ls())
library(plotrix)

#gene read counts files from MC embryos at 4C, 8C, 16C and early blastocyst stage can be downloaded from GSE168455 at GEO, including
#"4C_MC.All_rep.total.geneCount.txt"
#"8C_MC.All_rep.total.geneCount.txt"
#"16C_MC.All_rep.total.geneCount.txt"
#earlyB_MC.All_rep.total.geneCount.txt"

#------N4cell Rawdata-----------------
N4cell.comp <- read.table(file="4C_MC.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)
colnames(N4cell.comp) <- c("Chr","4.Comp1","4.Comp2","4.Comp3","4.Comp4","4.Comp5","4.Comp6","4.Comp7","4.Comp8","4.Comp9","4.Comp10","4.Comp11","4.Comp12","4.Comp13","4.Comp14","4.Comp15","4.Comp16","4.Comp17")
F.N4cell.allelic <- c(4,6,11,14,15,16)
F.N4cell.comp.Auto <- N4cell.comp[which(N4cell.comp$Chr != "chrX" & N4cell.comp$Chr != "chrY" ), c(5,7,12,15,16,17)]
total.F.N4.auto <- colSums(F.N4cell.comp.Auto)

#------N8cell Rawdata-----------------
N8cell.comp <- read.table(file="8C_MC.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)
colnames(N8cell.comp) <- c("Chr","8.Comp1","8.Comp2","8.Comp3","8.Comp4","8.Comp5","8.Comp6","8.Comp7","8.Comp8","8.Comp9","8.Comp10","8.Comp11","8.Comp12","8.Comp13","8.Comp14","8.Comp15")
F.N8cell.allelic <- c(1,5,7,10,14,15)
F.N8cell.comp.Auto <- N8cell.comp[which(N8cell.comp$Chr != "chrX" & N8cell.comp$Chr != "chrY" ), c(2,6,8,11,15,16)]
total.F.N8.auto <- colSums(F.N8cell.comp.Auto)

#------N16cell Rawdata-----------------
N16cell.comp <- read.table(file="16C_MC.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)
colnames(N16cell.comp) <- c("Chr","16.Comp1","16.Comp2","16.Comp3","16.Comp4","16.Comp5","16.Comp6","16.Comp7","16.Comp8")
F.N16cell.allelic <- c(2,3,5,6,7)
F.N16cell.comp.Auto <- N16cell.comp[which(N16cell.comp$Chr != "chrX" & N16cell.comp$Chr != "chrY" ), c(3,4,6,7,8)]
total.F.N16.auto <- colSums(F.N16cell.comp.Auto)
#------earlyBlast Rawdata-----------------
earlyBlast.comp <- read.table(file="earlyB_MC.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)
colnames(earlyBlast.comp) <- c("Chr","eB.Comp1","eB.Comp2","eB.Comp3","eB.Comp4","eB.Comp5","eB.Comp6","eB.Comp7","eB.Comp8","eB.Comp9","eB.Comp10","eB.Comp11")
F.earlyBlast.allelic <- c(1,4,5,8,10)
F.eB.comp.Auto <- earlyBlast.comp[which(earlyBlast.comp$Chr != "chrX" & earlyBlast.comp$Chr != "chrY" ), c(2,5,6,9,11)]
total.F.eB.auto <- colSums(F.eB.comp.Auto)

#---------------------------------------------------------------------------
#do the same to each individual replicates
#read the cas and ref data
#4C 4 7 12 15 16 17
#chrX
N4cell_4.chrX.total <- sum(read.table(file="processed/Fig.1g/4cell-4.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-4.REF.sum.chrX", sep="\t")[ ,1])
N4cell_7.chrX.total <- sum(read.table(file="processed/Fig.1g/4cell-7.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-7.REF.sum.chrX", sep="\t")[ ,1])
N4cell_12.chrX.total <- sum(read.table(file="processed/Fig.1g/4cell-12.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-12.REF.sum.chrX", sep="\t")[ ,1])
N4cell_15.chrX.total <- sum(read.table(file="processed/Fig.1g/4cell-15.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-15.REF.sum.chrX", sep="\t")[ ,1])
N4cell_16.chrX.total <- sum(read.table(file="processed/Fig.1g/4cell-16.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-16.REF.sum.chrX", sep="\t")[ ,1])
N4cell_17.chrX.total <- sum(read.table(file="processed/Fig.1g/4cell-17.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-17.REF.sum.chrX", sep="\t")[ ,1])
#auto
N4cell_4.auto.total <- sum(read.table(file="processed/Fig.1g/4cell-4.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-4.REF.sum.auto", sep="\t")[ ,1])
N4cell_7.auto.total <- sum(read.table(file="processed/Fig.1g/4cell-7.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-7.REF.sum.auto", sep="\t")[ ,1])
N4cell_12.auto.total <- sum(read.table(file="processed/Fig.1g/4cell-12.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-12.REF.sum.auto", sep="\t")[ ,1])
N4cell_15.auto.total <- sum(read.table(file="processed/Fig.1g/4cell-15.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-15.REF.sum.auto", sep="\t")[ ,1])
N4cell_16.auto.total <- sum(read.table(file="processed/Fig.1g/4cell-16.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-16.REF.sum.auto", sep="\t")[ ,1])
N4cell_17.auto.total <- sum(read.table(file="processed/Fig.1g/4cell-17.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-17.REF.sum.auto", sep="\t")[ ,1])

N4cell.chrX.total.normalized <- data.frame(data=c(N4cell_4.chrX.total*1000000/total.F.N4.auto[1], N4cell_7.chrX.total*1000000/total.F.N4.auto[2], N4cell_12.chrX.total*1000000/total.F.N4.auto[3], N4cell_15.chrX.total*1000000/total.F.N4.auto[4], N4cell_16.chrX.total*1000000/total.F.N4.auto[5], N4cell_17.chrX.total*1000000/total.F.N4.auto[6]), stage="4C", chr="ChrX")
N4cell.auto.total.normalized <- data.frame(data=c(N4cell_4.auto.total*1000000/total.F.N4.auto[1], N4cell_7.auto.total*1000000/total.F.N4.auto[2], N4cell_12.auto.total*1000000/total.F.N4.auto[3], N4cell_15.auto.total*1000000/total.F.N4.auto[4], N4cell_16.auto.total*1000000/total.F.N4.auto[5], N4cell_17.auto.total*1000000/total.F.N4.auto[6]), stage="4C",chr="Auto")


#8C 6 10 12 15 19 20
#chrX
N8cell_6.chrX.total <- sum(read.table(file="processed/Fig.1g/8cell-6.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/8cell-6.REF.sum.chrX", sep="\t")[ ,1])
N8cell_10.chrX.total <- sum(read.table(file="processed/Fig.1g/8cell-10.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/8cell-10.REF.sum.chrX", sep="\t")[ ,1])
N8cell_12.chrX.total <- sum(read.table(file="processed/Fig.1g/8cell-12.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/8cell-12.REF.sum.chrX", sep="\t")[ ,1])
N8cell_15.chrX.total <- sum(read.table(file="processed/Fig.1g/8cell-15.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/8cell-15.REF.sum.chrX", sep="\t")[ ,1])
N8cell_19.chrX.total <- sum(read.table(file="processed/Fig.1g/8cell-19.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/8cell-19.REF.sum.chrX", sep="\t")[ ,1])
N8cell_20.chrX.total <- sum(read.table(file="processed/Fig.1g/8cell-20.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/8cell-20.REF.sum.chrX", sep="\t")[ ,1])
#auto
N8cell_6.auto.total <- sum(read.table(file="processed/Fig.1g/8cell-6.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/8cell-6.REF.sum.auto", sep="\t")[ ,1])
N8cell_10.auto.total <- sum(read.table(file="processed/Fig.1g/8cell-10.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/8cell-10.REF.sum.auto", sep="\t")[ ,1])
N8cell_12.auto.total <- sum(read.table(file="processed/Fig.1g/8cell-12.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/8cell-12.REF.sum.auto", sep="\t")[ ,1])
N8cell_15.auto.total <- sum(read.table(file="processed/Fig.1g/8cell-15.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/8cell-15.REF.sum.auto", sep="\t")[ ,1])
N8cell_19.auto.total <- sum(read.table(file="processed/Fig.1g/8cell-19.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/8cell-19.REF.sum.auto", sep="\t")[ ,1])
N8cell_20.auto.total <- sum(read.table(file="processed/Fig.1g/8cell-20.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/8cell-20.REF.sum.auto", sep="\t")[ ,1])

N8cell.chrX.total.normalized <- data.frame(data=c(N8cell_6.chrX.total*1000000/total.F.N8.auto[1], N8cell_10.chrX.total*1000000/total.F.N8.auto[2], N8cell_12.chrX.total*1000000/total.F.N8.auto[3], N8cell_15.chrX.total*1000000/total.F.N8.auto[4], N8cell_19.chrX.total*1000000/total.F.N8.auto[5], N8cell_20.chrX.total*1000000/total.F.N8.auto[6]), stage="8C",chr="ChrX")
N8cell.auto.total.normalized <- data.frame(data=c(N8cell_6.auto.total*1000000/total.F.N8.auto[1], N8cell_10.auto.total*1000000/total.F.N8.auto[2], N8cell_12.auto.total*1000000/total.F.N8.auto[3], N8cell_15.auto.total*1000000/total.F.N8.auto[4], N8cell_19.auto.total*1000000/total.F.N8.auto[5], N8cell_20.auto.total*1000000/total.F.N8.auto[6]), stage="8C", chr="Auto")



#16C 2 3 5 6 7
#chrX
N16cell_2.chrX.total <- sum(read.table(file="processed/Fig.1g/16cell-2.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/16cell-2.REF.sum.chrX", sep="\t")[ ,1])
N16cell_3.chrX.total <- sum(read.table(file="processed/Fig.1g/16cell-3.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/16cell-3.REF.sum.chrX", sep="\t")[ ,1])
N16cell_5.chrX.total <- sum(read.table(file="processed/Fig.1g/16cell-5.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/16cell-5.REF.sum.chrX", sep="\t")[ ,1])
N16cell_6.chrX.total <- sum(read.table(file="processed/Fig.1g/16cell-6.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/16cell-6.REF.sum.chrX", sep="\t")[ ,1])
N16cell_7.chrX.total <- sum(read.table(file="processed/Fig.1g/16cell-7.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/16cell-7.REF.sum.chrX", sep="\t")[ ,1])
#auto
N16cell_2.auto.total <- sum(read.table(file="processed/Fig.1g/16cell-2.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/16cell-2.REF.sum.auto", sep="\t")[ ,1])
N16cell_3.auto.total <- sum(read.table(file="processed/Fig.1g/16cell-3.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/16cell-3.REF.sum.auto", sep="\t")[ ,1])
N16cell_5.auto.total <- sum(read.table(file="processed/Fig.1g/16cell-5.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/16cell-5.REF.sum.auto", sep="\t")[ ,1])
N16cell_6.auto.total <- sum(read.table(file="processed/Fig.1g/16cell-6.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/16cell-6.REF.sum.auto", sep="\t")[ ,1])
N16cell_7.auto.total <- sum(read.table(file="processed/Fig.1g/16cell-7.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/16cell-7.REF.sum.auto", sep="\t")[ ,1])

N16cell.chrX.total.normalized <- data.frame(data=c(N16cell_2.chrX.total*1000000/total.F.N16.auto[1], N16cell_3.chrX.total*1000000/total.F.N16.auto[2], N16cell_5.chrX.total*1000000/total.F.N16.auto[3], N16cell_6.chrX.total*1000000/total.F.N16.auto[4], N16cell_7.chrX.total*1000000/total.F.N16.auto[5]), stage="16C", chr="ChrX")
N16cell.auto.total.normalized <- data.frame(data=c(N16cell_2.auto.total*1000000/total.F.N16.auto[1], N16cell_3.auto.total*1000000/total.F.N16.auto[2], N16cell_5.auto.total*1000000/total.F.N16.auto[3], N16cell_6.auto.total*1000000/total.F.N16.auto[4], N16cell_7.auto.total*1000000/total.F.N16.auto[5]), stage="16C", chr="Auto")


#eB 1 4 5 8 10
#chrX
eB_1.chrX.total <- sum(read.table(file="processed/Fig.1g/earlyBlast-1.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/earlyBlast-1.REF.sum.chrX", sep="\t")[ ,1])
eB_4.chrX.total <- sum(read.table(file="processed/Fig.1g/earlyBlast-4.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/earlyBlast-4.REF.sum.chrX", sep="\t")[ ,1])
eB_5.chrX.total <- sum(read.table(file="processed/Fig.1g/earlyBlast-5.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/earlyBlast-5.REF.sum.chrX", sep="\t")[ ,1])
eB_8.chrX.total <- sum(read.table(file="processed/Fig.1g/earlyBlast-8.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/earlyBlast-8.REF.sum.chrX", sep="\t")[ ,1])
eB_10.chrX.total <- sum(read.table(file="processed/Fig.1g/earlyBlast-10.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/earlyBlast-10.REF.sum.chrX", sep="\t")[ ,1])
#auto
eB_1.auto.total <- sum(read.table(file="processed/Fig.1g/earlyBlast-1.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/earlyBlast-1.REF.sum.auto", sep="\t")[ ,1])
eB_4.auto.total <- sum(read.table(file="processed/Fig.1g/earlyBlast-4.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/earlyBlast-4.REF.sum.auto", sep="\t")[ ,1])
eB_5.auto.total <- sum(read.table(file="processed/Fig.1g/earlyBlast-5.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/earlyBlast-5.REF.sum.auto", sep="\t")[ ,1])
eB_8.auto.total <- sum(read.table(file="processed/Fig.1g/earlyBlast-8.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/earlyBlast-8.REF.sum.auto", sep="\t")[ ,1])
eB_10.auto.total <- sum(read.table(file="processed/Fig.1g/earlyBlast-10.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/earlyBlast-10.REF.sum.auto", sep="\t")[ ,1])

eB.chrX.total.normalized <- data.frame(data=c(eB_1.chrX.total*1000000/total.F.eB.auto[1], eB_4.chrX.total*1000000/total.F.eB.auto[2], eB_5.chrX.total*1000000/total.F.eB.auto[3], eB_8.chrX.total*1000000/total.F.eB.auto[4], eB_10.chrX.total*1000000/total.F.eB.auto[5]), stage="earlyB.", chr="ChrX")
eB.auto.total.normalized <- data.frame(data=c(eB_1.auto.total*1000000/total.F.eB.auto[1], eB_4.auto.total*1000000/total.F.eB.auto[2], eB_5.auto.total*1000000/total.F.eB.auto[3], eB_8.auto.total*1000000/total.F.eB.auto[4], eB_10.auto.total*1000000/total.F.eB.auto[5]), stage="earlyB.", chr="Auto")


chrX.total.normalized <- rbind(N4cell.chrX.total.normalized, N8cell.chrX.total.normalized, N16cell.chrX.total.normalized, eB.chrX.total.normalized)
chrX.total.normalized$data <- chrX.total.normalized$data / mean(N4cell.chrX.total.normalized$data)
auto.total.normalized <- rbind(N4cell.auto.total.normalized, N8cell.auto.total.normalized, N16cell.auto.total.normalized, eB.auto.total.normalized)
auto.total.normalized$data <- auto.total.normalized$data / mean(N4cell.auto.total.normalized$data)

rm(list=setdiff(ls(), c("chrX.total.normalized", "auto.total.normalized")))

chrX.4C.data <- data.frame(Mean=mean(chrX.total.normalized$data[chrX.total.normalized$stage=="4C"]), SE=std.error(chrX.total.normalized$data[chrX.total.normalized$stage=="4C"]), stage="4C", chr="ChrX")
chrX.8C.data <- data.frame(Mean=mean(chrX.total.normalized$data[chrX.total.normalized$stage=="8C"]), SE=std.error(chrX.total.normalized$data[chrX.total.normalized$stage=="8C"]), stage="8C", chr="ChrX")
chrX.16C.data <- data.frame(Mean=mean(chrX.total.normalized$data[chrX.total.normalized$stage=="16C"]), SE=std.error(chrX.total.normalized$data[chrX.total.normalized$stage=="16C"]), stage="16C", chr="ChrX")
chrX.eB.data <- data.frame(Mean=mean(chrX.total.normalized$data[chrX.total.normalized$stage=="earlyB."]), SE=std.error(chrX.total.normalized$data[chrX.total.normalized$stage=="earlyB."]), stage="earlyB.", chr="ChrX")

auto.4C.data <- data.frame(Mean=mean(auto.total.normalized$data[auto.total.normalized$stage=="4C"]), SE=std.error(auto.total.normalized$data[auto.total.normalized$stage=="4C"]), stage="4C", chr="Auto")
auto.8C.data <- data.frame(Mean=mean(auto.total.normalized$data[auto.total.normalized$stage=="8C"]), SE=std.error(auto.total.normalized$data[auto.total.normalized$stage=="8C"]), stage="8C", chr="Auto")
auto.16C.data <- data.frame(Mean=mean(auto.total.normalized$data[auto.total.normalized$stage=="16C"]), SE=std.error(auto.total.normalized$data[auto.total.normalized$stage=="16C"]), stage="16C", chr="Auto")
auto.eB.data <- data.frame(Mean=mean(auto.total.normalized$data[auto.total.normalized$stage=="earlyB."]), SE=std.error(auto.total.normalized$data[auto.total.normalized$stage=="earlyB."]), stage="earlyB.", chr="Auto")

total.data <- rbind(chrX.4C.data, chrX.8C.data, chrX.16C.data, chrX.eB.data,
                    auto.4C.data, auto.8C.data, auto.16C.data, auto.eB.data)
total.data$stage <- factor(total.data$stage, levels=c("4C","8C","16C","earlyB."))

dot.data <- rbind(chrX.total.normalized, auto.total.normalized)

library(ggplot2)
theme_pattern <- theme(axis.text=element_text(size=12),
                       axis.title=element_blank(),
                       legend.text=element_text(size=12),
                       legend.title=element_blank(), 
                       axis.ticks.x = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       legend.position="top",
                       #axis.line = element_line(color = 'black'),
                       axis.line.y = element_line(color="black"),
                       axis.line.x = element_line(color="black"),
                       plot.margin=unit(c(0.2,1,0,1),"cm"))

ggplot(total.data, aes(x=stage, y=Mean, fill=chr))+
  geom_jitter(data=dot.data, aes(x=stage, y=data, color=chr),
              position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8)) +
  geom_bar(stat='identity', position=position_dodge(), alpha=0.5, color="black",width=0.7) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE), position = position_dodge(0.7), width = 0.3, size=0.6)+
  scale_colour_manual(values = c("#CC0000","#0066cc")) +
  scale_fill_manual(values = c("#FF6666","#3399FF"))+
  scale_y_continuous(limits = c(0,1.1),breaks = c(0,0.2,0.4,0.6,0.8,1.0))+
  theme_bw() +
  theme_pattern 



#calculate P-value using pairted t-test

#at 8C
total.normalized.8C.X <- chrX.total.normalized[which(chrX.total.normalized$stage == "8C"), 1]
total.normalized.8C.A <- auto.total.normalized[which(auto.total.normalized$stage == "8C"), 1]
#auto vs chrX
t.test(total.normalized.8C.X, total.normalized.8C.A, paired=T)

#at 16C
total.normalized.16C.X <- chrX.total.normalized[which(chrX.total.normalized$stage == "16C"), 1]
total.normalized.16C.A <- auto.total.normalized[which(auto.total.normalized$stage == "16C"), 1]
#auto vs chrX
t.test(total.normalized.16C.X, total.normalized.16C.A, paired=T)

#at earlyB.
total.normalized.eB.X <- chrX.total.normalized[which(chrX.total.normalized$stage == "earlyB."), 1]
total.normalized.eB.A <- auto.total.normalized[which(auto.total.normalized$stage == "earlyB."), 1]
#auto vs chrX
t.test(total.normalized.eB.X, total.normalized.eB.A, paired=T)




#Panel g (right). overall allelic repeat count change from 4C to earlyB, X vs auto (average) in male

rm(list=ls())

#------N4cell Rawdata-----------------
N4cell.comp <- read.table(file="4C_MC.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)
colnames(N4cell.comp) <- c("Chr","4.Comp1","4.Comp2","4.Comp3","4.Comp4","4.Comp5","4.Comp6","4.Comp7","4.Comp8","4.Comp9","4.Comp10","4.Comp11","4.Comp12","4.Comp13","4.Comp14","4.Comp15","4.Comp16","4.Comp17")
M.N4cell.allelic <- c(1,2,3,5,7,8,9,10,12,13,17)
M.N4cell.comp.Auto <- N4cell.comp[which(N4cell.comp$Chr != "chrX" & N4cell.comp$Chr != "chrY" ), M.N4cell.allelic+1]
total.M.N4.auto <- colSums(M.N4cell.comp.Auto)

#------N8cell Rawdata-----------------
N8cell.comp <- read.table(file="8C_MC.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)
colnames(N8cell.comp) <- c("Chr","8.Comp1","8.Comp2","8.Comp3","8.Comp4","8.Comp5","8.Comp6","8.Comp7","8.Comp8","8.Comp9","8.Comp10","8.Comp11","8.Comp12","8.Comp13","8.Comp14","8.Comp15")
M.N8cell.allelic <- c(2,3,4,6,8,9,11,12,13)
M.N8cell.comp.Auto <- N8cell.comp[which(N8cell.comp$Chr != "chrX" & N8cell.comp$Chr != "chrY" ), M.N8cell.allelic+1]
total.M.N8.auto <- colSums(M.N8cell.comp.Auto)

#------N16cell Rawdata-----------------
N16cell.comp <- read.table(file="16C_MC.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)
colnames(N16cell.comp) <- c("Chr","16.Comp1","16.Comp2","16.Comp3","16.Comp4","16.Comp5","16.Comp6","16.Comp7","16.Comp8")
M.N16cell.allelic <- c(1,4,8)
M.N16cell.comp.Auto <- N16cell.comp[which(N16cell.comp$Chr != "chrX" & N16cell.comp$Chr != "chrY" ), M.N16cell.allelic+1]
total.M.N16.auto <- colSums(M.N16cell.comp.Auto)
#------earlyBlast Rawdata-----------------
earlyBlast.comp <- read.table(file="earlyB_MC.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)
colnames(earlyBlast.comp) <- c("Chr","eB.Comp1","eB.Comp2","eB.Comp3","eB.Comp4","eB.Comp5","eB.Comp6","eB.Comp7","eB.Comp8","eB.Comp9","eB.Comp10","eB.Comp11")
M.earlyBlast.allelic <- c(2,3,6,7,9,11)
M.eB.comp.Auto <- earlyBlast.comp[which(earlyBlast.comp$Chr != "chrX" & earlyBlast.comp$Chr != "chrY" ), M.earlyBlast.allelic+1]
total.M.eB.auto <- colSums(M.eB.comp.Auto)

#---------------------------------------------------------------------------
#do the same to each individual replicates
#read the cas and ref data
#4C 1 2 3 5 8 9 10 11 13 14 18
#chrX
N4cell_1.chrX.total <- sum(read.table(file="processed/Fig.1g/4cell-1.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-1.REF.sum.chrX", sep="\t")[ ,1])
N4cell_2.chrX.total <- sum(read.table(file="processed/Fig.1g/4cell-2.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-2.REF.sum.chrX", sep="\t")[ ,1])
N4cell_3.chrX.total <- sum(read.table(file="processed/Fig.1g/4cell-3.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-3.REF.sum.chrX", sep="\t")[ ,1])
N4cell_5.chrX.total <- sum(read.table(file="processed/Fig.1g/4cell-5.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-5.REF.sum.chrX", sep="\t")[ ,1])
N4cell_8.chrX.total <- sum(read.table(file="processed/Fig.1g/4cell-8.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-8.REF.sum.chrX", sep="\t")[ ,1])
N4cell_9.chrX.total <- sum(read.table(file="processed/Fig.1g/4cell-9.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-9.REF.sum.chrX", sep="\t")[ ,1])
N4cell_10.chrX.total <- sum(read.table(file="processed/Fig.1g/4cell-10.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-10.REF.sum.chrX", sep="\t")[ ,1])
N4cell_11.chrX.total <- sum(read.table(file="processed/Fig.1g/4cell-11.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-11.REF.sum.chrX", sep="\t")[ ,1])
N4cell_13.chrX.total <- sum(read.table(file="processed/Fig.1g/4cell-13.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-13.REF.sum.chrX", sep="\t")[ ,1])
N4cell_14.chrX.total <- sum(read.table(file="processed/Fig.1g/4cell-14.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-14.REF.sum.chrX", sep="\t")[ ,1])
N4cell_18.chrX.total <- sum(read.table(file="processed/Fig.1g/4cell-18.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-18.REF.sum.chrX", sep="\t")[ ,1])
#auto
N4cell_1.auto.total <- sum(read.table(file="processed/Fig.1g/4cell-1.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-1.REF.sum.auto", sep="\t")[ ,1])
N4cell_2.auto.total <- sum(read.table(file="processed/Fig.1g/4cell-2.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-2.REF.sum.auto", sep="\t")[ ,1])
N4cell_3.auto.total <- sum(read.table(file="processed/Fig.1g/4cell-3.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-3.REF.sum.auto", sep="\t")[ ,1])
N4cell_5.auto.total <- sum(read.table(file="processed/Fig.1g/4cell-5.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-5.REF.sum.auto", sep="\t")[ ,1])
N4cell_8.auto.total <- sum(read.table(file="processed/Fig.1g/4cell-8.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-8.REF.sum.auto", sep="\t")[ ,1])
N4cell_9.auto.total <- sum(read.table(file="processed/Fig.1g/4cell-9.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-9.REF.sum.auto", sep="\t")[ ,1])
N4cell_10.auto.total <- sum(read.table(file="processed/Fig.1g/4cell-10.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-10.REF.sum.auto", sep="\t")[ ,1])
N4cell_11.auto.total <- sum(read.table(file="processed/Fig.1g/4cell-11.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-11.REF.sum.auto", sep="\t")[ ,1])
N4cell_13.auto.total <- sum(read.table(file="processed/Fig.1g/4cell-13.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-13.REF.sum.auto", sep="\t")[ ,1])
N4cell_14.auto.total <- sum(read.table(file="processed/Fig.1g/4cell-14.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-14.REF.sum.auto", sep="\t")[ ,1])
N4cell_18.auto.total <- sum(read.table(file="processed/Fig.1g/4cell-18.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/4cell-18.REF.sum.auto", sep="\t")[ ,1])

N4cell.chrX.total.normalized <- data.frame(data=c(N4cell_1.chrX.total*1000000/total.M.N4.auto[1], 
                                                  N4cell_2.chrX.total*1000000/total.M.N4.auto[2], 
                                                  N4cell_3.chrX.total*1000000/total.M.N4.auto[3], 
                                                  N4cell_5.chrX.total*1000000/total.M.N4.auto[4], 
                                                  N4cell_8.chrX.total*1000000/total.M.N4.auto[5], 
                                                  N4cell_9.chrX.total*1000000/total.M.N4.auto[6],
                                                  N4cell_10.chrX.total*1000000/total.M.N4.auto[7],
                                                  N4cell_11.chrX.total*1000000/total.M.N4.auto[8],
                                                  N4cell_13.chrX.total*1000000/total.M.N4.auto[9],
                                                  N4cell_14.chrX.total*1000000/total.M.N4.auto[10],
                                                  N4cell_18.chrX.total*1000000/total.M.N4.auto[11]), 
                                           stage="4C", 
                                           chr="chrX")


N4cell.auto.total.normalized <- data.frame(data=c(N4cell_1.auto.total*1000000/total.M.N4.auto[1], 
                                                  N4cell_2.auto.total*1000000/total.M.N4.auto[2], 
                                                  N4cell_3.auto.total*1000000/total.M.N4.auto[3], 
                                                  N4cell_5.auto.total*1000000/total.M.N4.auto[4], 
                                                  N4cell_8.auto.total*1000000/total.M.N4.auto[5], 
                                                  N4cell_9.auto.total*1000000/total.M.N4.auto[6],
                                                  N4cell_10.auto.total*1000000/total.M.N4.auto[7],
                                                  N4cell_11.auto.total*1000000/total.M.N4.auto[8],
                                                  N4cell_13.auto.total*1000000/total.M.N4.auto[9],
                                                  N4cell_14.auto.total*1000000/total.M.N4.auto[10],
                                                  N4cell_18.auto.total*1000000/total.M.N4.auto[11]), 
                                           stage="4C", 
                                           chr="auto")


#8C 7 8 9 11 13 14 16 17 18
#chrX
N8cell_7.chrX.total <- sum(read.table(file="processed/Fig.1g/8cell-7.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/8cell-7.REF.sum.chrX", sep="\t")[ ,1])
N8cell_8.chrX.total <- sum(read.table(file="processed/Fig.1g/8cell-8.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/8cell-8.REF.sum.chrX", sep="\t")[ ,1])
N8cell_9.chrX.total <- sum(read.table(file="processed/Fig.1g/8cell-9.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/8cell-9.REF.sum.chrX", sep="\t")[ ,1])
N8cell_11.chrX.total <- sum(read.table(file="processed/Fig.1g/8cell-11.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/8cell-11.REF.sum.chrX", sep="\t")[ ,1])
N8cell_13.chrX.total <- sum(read.table(file="processed/Fig.1g/8cell-13.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/8cell-13.REF.sum.chrX", sep="\t")[ ,1])
N8cell_14.chrX.total <- sum(read.table(file="processed/Fig.1g/8cell-14.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/8cell-14.REF.sum.chrX", sep="\t")[ ,1])
N8cell_16.chrX.total <- sum(read.table(file="processed/Fig.1g/8cell-16.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/8cell-16.REF.sum.chrX", sep="\t")[ ,1])
N8cell_17.chrX.total <- sum(read.table(file="processed/Fig.1g/8cell-17.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/8cell-17.REF.sum.chrX", sep="\t")[ ,1])
N8cell_18.chrX.total <- sum(read.table(file="processed/Fig.1g/8cell-18.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/8cell-18.REF.sum.chrX", sep="\t")[ ,1])
#auto
N8cell_7.auto.total <- sum(read.table(file="processed/Fig.1g/8cell-7.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/8cell-7.REF.sum.auto", sep="\t")[ ,1])
N8cell_8.auto.total <- sum(read.table(file="processed/Fig.1g/8cell-8.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/8cell-8.REF.sum.auto", sep="\t")[ ,1])
N8cell_9.auto.total <- sum(read.table(file="processed/Fig.1g/8cell-9.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/8cell-9.REF.sum.auto", sep="\t")[ ,1])
N8cell_11.auto.total <- sum(read.table(file="processed/Fig.1g/8cell-11.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/8cell-11.REF.sum.auto", sep="\t")[ ,1])
N8cell_13.auto.total <- sum(read.table(file="processed/Fig.1g/8cell-13.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/8cell-13.REF.sum.auto", sep="\t")[ ,1])
N8cell_14.auto.total <- sum(read.table(file="processed/Fig.1g/8cell-14.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/8cell-14.REF.sum.auto", sep="\t")[ ,1])
N8cell_16.auto.total <- sum(read.table(file="processed/Fig.1g/8cell-16.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/8cell-16.REF.sum.auto", sep="\t")[ ,1])
N8cell_17.auto.total <- sum(read.table(file="processed/Fig.1g/8cell-17.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/8cell-17.REF.sum.auto", sep="\t")[ ,1])
N8cell_18.auto.total <- sum(read.table(file="processed/Fig.1g/8cell-18.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/8cell-18.REF.sum.auto", sep="\t")[ ,1])
N8cell.chrX.total.normalized <- data.frame(data=c(N8cell_7.chrX.total*1000000/total.M.N8.auto[1], 
                                                  N8cell_8.chrX.total*1000000/total.M.N8.auto[2], 
                                                  N8cell_9.chrX.total*1000000/total.M.N8.auto[3], 
                                                  N8cell_11.chrX.total*1000000/total.M.N8.auto[4], 
                                                  N8cell_13.chrX.total*1000000/total.M.N8.auto[5], 
                                                  N8cell_14.chrX.total*1000000/total.M.N8.auto[6],
                                                  N8cell_16.chrX.total*1000000/total.M.N8.auto[7], 
                                                  N8cell_17.chrX.total*1000000/total.M.N8.auto[8],
                                                  N8cell_18.chrX.total*1000000/total.M.N8.auto[9]), 
                                           stage="8C",chr="chrX")

N8cell.auto.total.normalized <- data.frame(data=c(N8cell_7.auto.total*1000000/total.M.N8.auto[1], 
                                                  N8cell_8.auto.total*1000000/total.M.N8.auto[2], 
                                                  N8cell_9.auto.total*1000000/total.M.N8.auto[3], 
                                                  N8cell_11.auto.total*1000000/total.M.N8.auto[4], 
                                                  N8cell_13.auto.total*1000000/total.M.N8.auto[5], 
                                                  N8cell_14.auto.total*1000000/total.M.N8.auto[6],
                                                  N8cell_16.auto.total*1000000/total.M.N8.auto[7], 
                                                  N8cell_17.auto.total*1000000/total.M.N8.auto[8],
                                                  N8cell_18.auto.total*1000000/total.M.N8.auto[9]), 
                                           stage="8C",chr="auto")


#16C 1 4 8
#chrX
N16cell_1.chrX.total <- sum(read.table(file="processed/Fig.1g/16cell-1.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/16cell-1.REF.sum.chrX", sep="\t")[ ,1])
N16cell_4.chrX.total <- sum(read.table(file="processed/Fig.1g/16cell-4.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/16cell-4.REF.sum.chrX", sep="\t")[ ,1])
N16cell_8.chrX.total <- sum(read.table(file="processed/Fig.1g/16cell-8.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/16cell-8.REF.sum.chrX", sep="\t")[ ,1])
#auto
N16cell_1.auto.total <- sum(read.table(file="processed/Fig.1g/16cell-1.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/16cell-1.REF.sum.auto", sep="\t")[ ,1])
N16cell_4.auto.total <- sum(read.table(file="processed/Fig.1g/16cell-4.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/16cell-4.REF.sum.auto", sep="\t")[ ,1])
N16cell_8.auto.total <- sum(read.table(file="processed/Fig.1g/16cell-8.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/16cell-8.REF.sum.auto", sep="\t")[ ,1])

N16cell.chrX.total.normalized <- data.frame(data=c(N16cell_1.chrX.total*1000000/total.M.N16.auto[1], 
                                                   N16cell_4.chrX.total*1000000/total.M.N16.auto[2], 
                                                   N16cell_8.chrX.total*1000000/total.M.N16.auto[3]), 
                                            stage="16C", chr="chrX")
N16cell.auto.total.normalized <- data.frame(data=c(N16cell_1.auto.total*1000000/total.M.N16.auto[1], 
                                                   N16cell_4.auto.total*1000000/total.M.N16.auto[2], 
                                                   N16cell_8.auto.total*1000000/total.M.N16.auto[3]), 
                                            stage="16C", chr="auto")



#eB 1 4 5 8 10
#chrX
eB_2.chrX.total <- sum(read.table(file="processed/Fig.1g/earlyBlast-2.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/earlyBlast-2.REF.sum.chrX", sep="\t")[ ,1])
eB_3.chrX.total <- sum(read.table(file="processed/Fig.1g/earlyBlast-3.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/earlyBlast-3.REF.sum.chrX", sep="\t")[ ,1])
eB_6.chrX.total <- sum(read.table(file="processed/Fig.1g/earlyBlast-6.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/earlyBlast-6.REF.sum.chrX", sep="\t")[ ,1])
eB_7.chrX.total <- sum(read.table(file="processed/Fig.1g/earlyBlast-7.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/earlyBlast-7.REF.sum.chrX", sep="\t")[ ,1])
eB_9.chrX.total <- sum(read.table(file="processed/Fig.1g/earlyBlast-9.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/earlyBlast-9.REF.sum.chrX", sep="\t")[ ,1])
eB_11.chrX.total <- sum(read.table(file="processed/Fig.1g/earlyBlast-11.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/earlyBlast-11.REF.sum.chrX", sep="\t")[ ,1])
#auto
eB_2.auto.total <- sum(read.table(file="processed/Fig.1g/earlyBlast-2.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/earlyBlast-2.REF.sum.auto", sep="\t")[ ,1])
eB_3.auto.total <- sum(read.table(file="processed/Fig.1g/earlyBlast-3.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/earlyBlast-3.REF.sum.auto", sep="\t")[ ,1])
eB_6.auto.total <- sum(read.table(file="processed/Fig.1g/earlyBlast-6.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/earlyBlast-6.REF.sum.auto", sep="\t")[ ,1])
eB_7.auto.total <- sum(read.table(file="processed/Fig.1g/earlyBlast-7.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/earlyBlast-7.REF.sum.auto", sep="\t")[ ,1])
eB_9.auto.total <- sum(read.table(file="processed/Fig.1g/earlyBlast-9.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/earlyBlast-9.REF.sum.auto", sep="\t")[ ,1])
eB_11.auto.total <- sum(read.table(file="processed/Fig.1g/earlyBlast-11.CAST.sum.auto", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig.1g/earlyBlast-11.REF.sum.auto", sep="\t")[ ,1])

eB.chrX.total.normalized <- data.frame(data=c(eB_2.chrX.total*1000000/total.M.eB.auto[1], 
                                              eB_3.chrX.total*1000000/total.M.eB.auto[2], 
                                              eB_6.chrX.total*1000000/total.M.eB.auto[3], 
                                              eB_7.chrX.total*1000000/total.M.eB.auto[4], 
                                              eB_9.chrX.total*1000000/total.M.eB.auto[5],
                                              eB_11.chrX.total*1000000/total.M.eB.auto[6]), 
                                       stage="earlyB.", chr="chrX")

eB.auto.total.normalized <- data.frame(data=c(eB_2.auto.total*1000000/total.M.eB.auto[1], 
                                              eB_3.auto.total*1000000/total.M.eB.auto[2], 
                                              eB_6.auto.total*1000000/total.M.eB.auto[3], 
                                              eB_7.auto.total*1000000/total.M.eB.auto[4], 
                                              eB_9.auto.total*1000000/total.M.eB.auto[5],
                                              eB_11.auto.total*1000000/total.M.eB.auto[6]), 
                                       stage="earlyB.", chr="auto")

chrX.total.normalized <- rbind(N4cell.chrX.total.normalized, N8cell.chrX.total.normalized, N16cell.chrX.total.normalized, eB.chrX.total.normalized)
chrX.total.normalized$data <- chrX.total.normalized$data / mean(N4cell.chrX.total.normalized$data)
auto.total.normalized <- rbind(N4cell.auto.total.normalized, N8cell.auto.total.normalized, N16cell.auto.total.normalized, eB.auto.total.normalized)
auto.total.normalized$data <- auto.total.normalized$data / mean(N4cell.auto.total.normalized$data)

rm(list=setdiff(ls(), c("chrX.total.normalized", "auto.total.normalized")))

chrX.4C.data <- data.frame(Mean=mean(chrX.total.normalized$data[chrX.total.normalized$stage=="4C"]), SE=std.error(chrX.total.normalized$data[chrX.total.normalized$stage=="4C"]), stage="4C", chr="chrX")
chrX.8C.data <- data.frame(Mean=mean(chrX.total.normalized$data[chrX.total.normalized$stage=="8C"]), SE=std.error(chrX.total.normalized$data[chrX.total.normalized$stage=="8C"]), stage="8C", chr="chrX")
chrX.16C.data <- data.frame(Mean=mean(chrX.total.normalized$data[chrX.total.normalized$stage=="16C"]), SE=std.error(chrX.total.normalized$data[chrX.total.normalized$stage=="16C"]), stage="16C", chr="chrX")
chrX.eB.data <- data.frame(Mean=mean(chrX.total.normalized$data[chrX.total.normalized$stage=="earlyB."]), SE=std.error(chrX.total.normalized$data[chrX.total.normalized$stage=="earlyB."]), stage="earlyB.", chr="chrX")

auto.4C.data <- data.frame(Mean=mean(auto.total.normalized$data[auto.total.normalized$stage=="4C"]), SE=std.error(auto.total.normalized$data[auto.total.normalized$stage=="4C"]), stage="4C", chr="auto")
auto.8C.data <- data.frame(Mean=mean(auto.total.normalized$data[auto.total.normalized$stage=="8C"]), SE=std.error(auto.total.normalized$data[auto.total.normalized$stage=="8C"]), stage="8C", chr="auto")
auto.16C.data <- data.frame(Mean=mean(auto.total.normalized$data[auto.total.normalized$stage=="16C"]), SE=std.error(auto.total.normalized$data[auto.total.normalized$stage=="16C"]), stage="16C", chr="auto")
auto.eB.data <- data.frame(Mean=mean(auto.total.normalized$data[auto.total.normalized$stage=="earlyB."]), SE=std.error(auto.total.normalized$data[auto.total.normalized$stage=="earlyB."]), stage="earlyB.", chr="auto")

total.data <- rbind(chrX.4C.data, chrX.8C.data, chrX.16C.data, chrX.eB.data,
                    auto.4C.data, auto.8C.data, auto.16C.data, auto.eB.data)
total.data$stage <- factor(total.data$stage, levels=c("4C","8C","16C","earlyB."))

dot.data <- rbind(chrX.total.normalized, auto.total.normalized)

library(ggplot2)
theme_pattern <- theme(axis.text=element_text(size=12),
                       axis.title=element_blank(),
                       legend.text=element_text(size=12),
                       legend.title=element_blank(), 
                       axis.ticks.x = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       legend.position="top",
                       #axis.line = element_line(color = 'black'),
                       axis.line.y = element_line(color="black"),
                       axis.line.x = element_line(color="black"),
                       plot.margin=unit(c(0.2,1,0,1),"cm"))

ggplot(total.data, aes(x=stage, y=Mean, fill=chr))+
  geom_jitter(data=dot.data, aes(x=stage, y=data, color=chr),
              position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8)) +
  geom_bar(stat='identity', position=position_dodge(), alpha=0.5, color="black",width=0.7) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE), position = position_dodge(0.7), width = 0.3, size=0.6)+
  scale_colour_manual(values = c("#CC0000","#0066cc")) +
  scale_fill_manual(values = c("#FF6666","#3399FF"))+
  scale_y_continuous(limits = c(0,1.3),breaks = c(0,0.2,0.4,0.6,0.8,1.0,1.2))+
  theme_bw() +
  theme_pattern 


#calculate P-value using pairted t-test

#at 8C
total.normalized.8C.X <- chrX.total.normalized[which(chrX.total.normalized$stage == "8C"), 1]
total.normalized.8C.A <- auto.total.normalized[which(auto.total.normalized$stage == "8C"), 1]
#auto vs chrX
t.test(total.normalized.8C.X, total.normalized.8C.A, paired=T)

#at 16C
total.normalized.16C.X <- chrX.total.normalized[which(chrX.total.normalized$stage == "16C"), 1]
total.normalized.16C.A <- auto.total.normalized[which(auto.total.normalized$stage == "16C"), 1]
#auto vs chrX
t.test(total.normalized.16C.X, total.normalized.16C.A, paired=T)

#at earlyB.
total.normalized.eB.X <- chrX.total.normalized[which(chrX.total.normalized$stage == "earlyB."), 1]
total.normalized.eB.A <- auto.total.normalized[which(auto.total.normalized$stage == "earlyB."), 1]
#auto vs chrX
t.test(total.normalized.eB.X, total.normalized.eB.A, paired=T)





#Panel h. Total repeats reads, Skew analysis for chr1, chr13 and chrX during XCI.-----------------------------------------------------

rm(list=ls())
source("MC.overall repeat analysis.R")
rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average",
                        "zygote.skew","zygote.chrX","zygote.combined.Data","zygote.mean.skew","zygote.XvsAuto.skew",
                        "early2cell.skew","early2cell.chrX","early2cell.combined.Data","early2cell.mean.skew","early2cell.XvsAuto.skew",
                        "late2cell.skew","late2cell.chrX","late2cell.combined.Data","late2cell.mean.skew","late2cell.XvsAuto.skew",
                        "N4cell.skew","N4cell.chrX","N4cell.combined.Data","N4cell.mean.skew","N4cell.XvsAuto.skew",
                        "N8cell.skew","N8cell.chrX","N8cell.combined.Data","N8cell.mean.skew","N8cell.XvsAuto.skew",
                        "N16cell.skew","N16cell.chrX","N16cell.combined.Data","N16cell.mean.skew","N16cell.XvsAuto.skew",
                        "earlyBlast.skew","earlyBlast.chrX","earlyBlast.combined.Data","earlyBlast.mean.skew","earlyBlast.XvsAuto.skew")))


chr1_13_X.dat <- rbind(zygote.skew, early2cell.skew, late2cell.skew, N4cell.skew, N8cell.skew, N16cell.skew, earlyBlast.skew)
chr1_13_X.dat$stage <- factor(chr1_13_X.dat$stage, levels=c("ZG", "early2C", "late2C","4C","8C","16C","eB"))

XvsAuto.dat <- rbind(zygote.XvsAuto.skew, early2cell.XvsAuto.skew, late2cell.XvsAuto.skew, N4cell.XvsAuto.skew, N8cell.XvsAuto.skew, N16cell.XvsAuto.skew, earlyBlast.XvsAuto.skew)
XvsAuto.dat$stage <- factor(XvsAuto.dat$stage, levels=c("ZG", "early2C", "late2C","4C","8C","16C","eB"))

chr1_13_X.dat$chr <- factor(chr1_13_X.dat$chr, levels=c("chr1", "chr13", "chrX"))

# plot chr1, 13 and chrX
ggplot(chr1_13_X.dat, aes(x=stage, y=skewing, fill=chr)) +
  geom_boxplot(outlier.shape=NA, alpha=.5, lwd=0.4) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
             aes(fill=chr), size=0.7, pch=21,stroke = 0.4) +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_blank(),
        legend.text=element_text(size=12),
        legend.title=element_blank(), 
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position="top",
        #axis.line = element_line(color = 'black'),
        axis.line.y = element_line(color="black"),
        axis.line.x = element_line(color="black"))


#8C p-value, chrX vs chr1
N8C.Xvs1.dat <- N8cell.skew[which(N8cell.skew$chr != "chr13"), ]
wilcox.test(skewing ~ chr, data=N8C.Xvs1.dat)$p.value
#8C p-value, chrX vs chr13
N8C.Xvs13.dat <- N8cell.skew[which(N8cell.skew$chr != "chr1"), ]
wilcox.test(skewing ~ chr, data=N8C.Xvs13.dat)$p.value


#16C p-value, chrX vs chr1
N16C.Xvs1.dat <- N16cell.skew[which(N16cell.skew$chr != "chr13"), ]
wilcox.test(skewing ~ chr, data=N16C.Xvs1.dat)$p.value
#16C p-value, chrX vs chr13
N16C.Xvs13.dat <- N16cell.skew[which(N16cell.skew$chr != "chr1"), ]
wilcox.test(skewing ~ chr, data=N16C.Xvs13.dat)$p.value

#eB p-value, chrX vs chr13
eB.Xvs13.dat <- earlyBlast.skew[which(earlyBlast.skew$chr != "chr1"), ]
wilcox.test(skewing ~ chr, data=eB.Xvs13.dat)$p.value
#eB p-value, chrX vs chr1
eB.Xvs1.dat <- earlyBlast.skew[which(earlyBlast.skew$chr != "chr13"), ]
wilcox.test(skewing ~ chr, data=eB.Xvs1.dat)$p.value



#Panel i. Total repeats reads, XCI Skew analysis for MC, CM and KO embryos. -----------------------------------------------------

rm(list=ls())
source("MC.overall repeat analysis.R")
rm(list=setdiff(ls(), c("late2cell.chrX","N4cell.chrX","N8cell.chrX","N16cell.chrX","earlyBlast.chrX")))

MC.late2cell <- data.frame(skew=rowMeans(late2cell.chrX, na.rm=T), genotype="MC", stage="late2C")
MC.4cell <- data.frame(skew=rowMeans(N4cell.chrX, na.rm=T), genotype="MC", stage="4C")
MC.8cell <- data.frame(skew=rowMeans(N8cell.chrX, na.rm=T), genotype="MC", stage="8C")
MC.16cell <- data.frame(skew=rowMeans(N16cell.chrX, na.rm=T), genotype="MC", stage="16C")
MC.earlyBlast <- data.frame(skew=rowMeans(earlyBlast.chrX, na.rm=T), genotype="MC", stage="earlyBlast")

MC.X_skew <- rbind(MC.late2cell, MC.4cell, MC.8cell, MC.earlyBlast)
write.table(MC.X_skew, file="MC.X_skew.tmp", sep="\t", quote=F)

source("CM.overall repeat analysis.R")
rm(list=setdiff(ls(), c("late2cell.chrX","N4cell.chrX","N8cell.chrX","N16cell.chrX","earlyBlast.chrX",
                        "KO_late2cell.chrX","KO_4cell.chrX","KO_8cell.chrX","KO_earlyBlast.chrX")))

CM.late2cell <- data.frame(skew=rowMeans(late2cell.chrX, na.rm=T), genotype="CM", stage="late2C")
CM.4cell <- data.frame(skew=rowMeans(N4cell.chrX, na.rm=T), genotype="CM", stage="4C")
CM.8cell <- data.frame(skew=rowMeans(N8cell.chrX, na.rm=T), genotype="CM", stage="8C")
CM.16cell <- data.frame(skew=rowMeans(N16cell.chrX, na.rm=T), genotype="CM", stage="16C")
CM.earlyBlast <- data.frame(skew=rowMeans(earlyBlast.chrX, na.rm=T), genotype="CM", stage="earlyBlast")

CM.X_skew <- rbind(CM.late2cell, CM.4cell, CM.8cell, CM.earlyBlast)
#For paternal fraction, the skew in MC cross should be converted to 1-CAST_ratio
CM.X_skew$skew <- 1-CM.X_skew$skew


KO_late2cell <- data.frame(skew=rowMeans(KO_late2cell.chrX, na.rm=T), genotype="KO", stage="late2C")
KO_4cell <- data.frame(skew=rowMeans(KO_4cell.chrX, na.rm=T), genotype="KO", stage="4C")
KO_8cell <- data.frame(skew=rowMeans(KO_8cell.chrX, na.rm=T), genotype="KO", stage="8C")
KO_earlyBlast <- data.frame(skew=rowMeans(KO_earlyBlast.chrX, na.rm=T), genotype="KO", stage="earlyBlast")

KO.X_skew <- rbind(KO_late2cell, KO_4cell, KO_8cell, KO_earlyBlast)
#For paternal fraction, the skew in MC cross should be converted to 1-CAST_ratio
KO.X_skew$skew <- 1-KO.X_skew$skew

MC.X_skew <- read.table(file="MC.X_skew.tmp", sep="\t")
file.remove("MC.X_skew.tmp")

X.skew <- rbind(CM.X_skew, MC.X_skew, KO.X_skew)

X.skew$stage <- factor(X.skew$stage, levels=c("late2C","4C","8C","earlyBlast"))
X.skew$genotype <- factor(X.skew$genotype, levels=c("MC","CM","KO"))


ggplot(X.skew, aes(x=stage, y=skew, fill=genotype)) +
  geom_boxplot(outlier.shape=NA, alpha=.7, lwd=0.4) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
             aes(fill=genotype), size=0.7, pch=21,stroke = 0.4) +
  scale_fill_manual(values=c("lightgoldenrod2", "orange2","lightskyblue")) +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_blank(),
        legend.text=element_text(size=12),
        legend.title=element_blank(), 
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position="top",
        #axis.line = element_line(color = 'black'),
        axis.line.y = element_line(color="black"),
        axis.line.x = element_line(color="black"))


#p-value 
#at earlyBlast stage, between CM and KO.
dat <- rbind(KO_earlyBlast, CM.earlyBlast)
wilcox.test(skew ~ genotype, data=dat)$p.value
#at 8C stage, between CM and KO.
dat <- rbind(KO_8cell, CM.8cell)
wilcox.test(skew ~ genotype, data=dat)$p.value

#at earlyBlast stage, between MC and KO.
MC.earlyBlast <- MC.X_skew[which(MC.X_skew$stage=="earlyBlast"), ]
dat <- rbind(KO_earlyBlast, MC.earlyBlast)
wilcox.test(skew ~ genotype, data=dat)$p.value
#at 8C stage, between MC and KO.
MC.8cell <- MC.X_skew[which(MC.X_skew$stage=="8C"), ]
dat <- rbind(KO_8cell, MC.8cell)
wilcox.test(skew ~ genotype, data=dat)$p.value




