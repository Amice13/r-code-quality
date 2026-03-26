#Panel.c Clustering--------------------------

rm(list=ls())
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




#Clustering

# calculate pairwise correlations for all samples and cluster based on that

C<-mat.or.vec(nS,nS)
for (i in 1:nS) {
  for (j in 1:nS){
    if (i==j){ C[i,j]<-NA }
    else {  C[i,j] = cor(log2(DATA[,i]+1),log2(DATA[,j]+1),method="pearson") }
  }
}

#add cross info to sample name
SampleName.with.cross <- c(paste("MC.zygote", c(1:4), sep="-"),
                           paste("MC.early2C", c(1:6), sep="-"),
                           paste("MC.late2C", c(1:10), sep="-"),
                           paste("MC.4C", c(1:17), sep="-"),
                           paste("MC.8C", c(1:15), sep="-"),
                           paste("MC.16C", c(1:8), sep="-"),
                           paste("MC.earlyBlast.", c(1:11), sep="-"),
                           paste("CM.late2C", c(1:10), sep="-"),
                           paste("CM.4C", c(1:8), sep="-"),
                           paste("CM.8C", c(1:11), sep="-"),
                           paste("CM.16C", c(1:14), sep="-"),
                           paste("CM.earlyBlast.", c(1:13), sep="-"))

colnames(C)<-SampleName.with.cross
rownames(C)<-SampleName.with.cross
#write.table(C,file="pairwise_pearson_correlations.txt")



# and do clustering based on the correlations
dist.corr<-as.dist(1-C) 
hcl.corr<-hclust(dist.corr,method="ward.D2")
#add color for cross information
coldef.cross <- c(rep("lightskyblue",71), rep("salmon",56))

# plot a heatmap:

library(gplots)
#Use a smaller cex within legend (e.g. cex=0.75) to adjust the size of the printed text.  
#This controls the size of the legend.  
#Also, you can play with xjust and yjust for finer control of how the legend box is justified at the specified position. 
par(xpd=T)
pdf("Pairwise Pearson correlation.pdf", width = 15, height=15)
heatmap.2(C,
          RowSideColors=coldef.cross,
          ColSideColors=col.stage,
          Colv=as.dendrogram(hcl.corr),
          Rowv=as.dendrogram(hcl.corr),
          scale="none",
          trace="none",
          main="Correlation, Ward",
          margins=c(10,6))

legend("bottomleft",
       stages,
       fill=coldef.stage,cex=1,
       bty='n',

)
legend("topright",
       unique(Cross),
       fill=c("lightskyblue","salmon"),cex=1,
       bty='n',
)
dev.off() 




#Panel d PCA by developmental stage and sex for each cross.------------------------
#use the data from Panel c.

#MC cross
DATA <- MC.dat[ ,-1]
colnames(DATA) <- c(MC.colnames)
Sex <- c("NA","NA","NA","NA", #zygote
         "F","M","M","F","F","M", #early2C
         "M","F","F","F","M","F","M","F","M","M", #late2C
         "M","M","M","F","M","F","M","M","M","M","F","M","M","F","F","F","M", #4C
         "F","M","M","M","F","M","F","M","M","F","M","M","M","F","F", #8C
         "M","F","F","M","F","F","F","M", #16C
         "F","M","M","F","F","M","M","F","M","F","M") #eB

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
PC.table$Sex <- Sex

PC.table$Sex <- factor(PC.table$Sex, levels=c("F","M","NA"))

# PCA Plot
ggplot(PC.table) + 
  geom_point(aes(x=PC1, y=PC2, color= Stage, shape=Sex), size=2.0) + 
  theme_bw() +
  scale_colour_manual(values = coldef.stage) +
  theme(axis.text=element_text(size=rel(1.0), face="bold")) +
  theme(legend.position="right") +
  theme(legend.title=element_blank()) + 
  theme(legend.key = element_blank()) +
  theme(legend.text=element_text(size = rel(1), face="bold")) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1)) +
  #xlab("PC1 (28% var.)") +
  #ylab("PC2 (17% var.)") +
  theme(axis.title.y = element_text(size = rel(1.3), face="bold")) +
  theme(axis.title.x = element_text(size = rel(1.3), face="bold"))


#CM cross

#MC cross
DATA <- CM.dat[ ,-1]
colnames(DATA) <- c(CM.colnames)
Sex <- c("F","F","F","M","F","M","M","F","M","M", #late2C
         "F","F","F","F","F","M","M","M", #4C
         "F","F","M","M","F","F","F","F","F","F","F", #8C
         "F","F","M","M","M","F","M","M","F","M","M","F","F","F", #16C
         "M","M","F","M","M","F","M","M","F","M","F","M","F") #eB

# load ggplot2 and start to calculate PCA

library(ggplot2)
nS<-ncol(DATA) #number of samples
nG<-nrow(DATA) #number of genes

sample.names<-colnames(DATA)
stage<-unlist(lapply(strsplit(sample.names,"-"),function(x) x[1]))
embryo<-unlist(lapply(strsplit(sample.names,"\\."),function(x) x[1]))


stages<-unique(stage)
# have 11 different stages, define 5 colors for those.
coldef.stage<-c("green","blue","orange","purple","brown")
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
PC.table$Sex <- Sex

PC.table$Sex <- factor(PC.table$Sex, levels=c("F","M"))

# PCA Plot
ggplot(PC.table) + 
  geom_point(aes(x=PC1, y=PC2, color= Stage, shape=Sex), size=2.0) + 
  theme_bw() +
  scale_colour_manual(values = coldef.stage) +
  theme(axis.text=element_text(size=rel(1.0), face="bold")) +
  theme(legend.position="right") +
  theme(legend.title=element_blank()) + 
  theme(legend.key = element_blank()) +
  theme(legend.text=element_text(size = rel(1), face="bold")) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1)) +
  #xlab("PC1 (28% var.)") +
  #ylab("PC2 (17% var.)") +
  theme(axis.title.y = element_text(size = rel(1.3), face="bold")) +
  theme(axis.title.x = element_text(size = rel(1.3), face="bold"))


#Panel e. PCA for differentiating ES cells. ------------------------------------------------
#PCA analysis used repeat expression value (cpm) for each repeat family in ESC replicates at different Days of differentiation.
rm(list=ls())
setwd("processed/")

#Day0
Day0_1.rawCounts <- read.table(file="Day0-1.rep.count.bed", sep="\t",col.names=c("count_1","family", "class"))[,c(1,2)]
Day0_2.rawCounts <- read.table(file="Day0-2.rep.count.bed", sep="\t",col.names=c("count_2","family", "class"))[,c(1,2)]
#remove duplicated row
Day0_1.rawCounts <- Day0_1.rawCounts[!duplicated(Day0_1.rawCounts$family), ]
Day0_2.rawCounts <- Day0_2.rawCounts[!duplicated(Day0_2.rawCounts$family), ]
#merge
Day0_merge <- merge(Day0_1.rawCounts, Day0_2.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)

colnames(Day0_merge) <- c("family", paste("Day0", c(1:2), sep="-"))
rm(list=setdiff(ls(), "Day0_merge"))

#Day1
Day1_1.rawCounts <- read.table(file="Day1-1.rep.count.bed", sep="\t",col.names=c("count_1","family", "class"))[,c(1,2)]
Day1_2.rawCounts <- read.table(file="Day1-2.rep.count.bed", sep="\t",col.names=c("count_2","family", "class"))[,c(1,2)]
#remove duplicated row
Day1_1.rawCounts <- Day1_1.rawCounts[!duplicated(Day1_1.rawCounts$family), ]
Day1_2.rawCounts <- Day1_2.rawCounts[!duplicated(Day1_2.rawCounts$family), ]
#merge
Day1_merge <- merge(Day1_1.rawCounts, Day1_2.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)

colnames(Day1_merge) <- c("family", paste("Day1", c(1:2), sep="-"))
rm(list=setdiff(ls(), c("Day0_merge","Day1_merge")))

#Day2
Day2_1.rawCounts <- read.table(file="Day2-1.rep.count.bed", sep="\t",col.names=c("count_1","family", "class"))[,c(1,2)]
Day2_2.rawCounts <- read.table(file="Day2-2.rep.count.bed", sep="\t",col.names=c("count_2","family", "class"))[,c(1,2)]
#remove duplicated row
Day2_1.rawCounts <- Day2_1.rawCounts[!duplicated(Day2_1.rawCounts$family), ]
Day2_2.rawCounts <- Day2_2.rawCounts[!duplicated(Day2_2.rawCounts$family), ]
#merge
Day2_merge <- merge(Day2_1.rawCounts, Day2_2.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)

colnames(Day2_merge) <- c("family", paste("Day2", c(1:2), sep="-"))
rm(list=setdiff(ls(), c("Day0_merge","Day1_merge","Day2_merge")))

#Day3
Day3_1.rawCounts <- read.table(file="Day3-1.rep.count.bed", sep="\t",col.names=c("count_1","family", "class"))[,c(1,2)]
Day3_2.rawCounts <- read.table(file="Day3-2.rep.count.bed", sep="\t",col.names=c("count_2","family", "class"))[,c(1,2)]
#remove duplicated row
Day3_1.rawCounts <- Day3_1.rawCounts[!duplicated(Day3_1.rawCounts$family), ]
Day3_2.rawCounts <- Day3_2.rawCounts[!duplicated(Day3_2.rawCounts$family), ]
#merge
Day3_merge <- merge(Day3_1.rawCounts, Day3_2.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)

colnames(Day3_merge) <- c("family", paste("Day3", c(1:2), sep="-"))
rm(list=setdiff(ls(), c("Day0_merge","Day1_merge","Day2_merge","Day3_merge")))

#Day4
Day4_1.rawCounts <- read.table(file="Day4-1.rep.count.bed", sep="\t",col.names=c("count_1","family", "class"))[,c(1,2)]
Day4_2.rawCounts <- read.table(file="Day4-2.rep.count.bed", sep="\t",col.names=c("count_2","family", "class"))[,c(1,2)]
#remove duplicated row
Day4_1.rawCounts <- Day4_1.rawCounts[!duplicated(Day4_1.rawCounts$family), ]
Day4_2.rawCounts <- Day4_2.rawCounts[!duplicated(Day4_2.rawCounts$family), ]
#merge
Day4_merge <- merge(Day4_1.rawCounts, Day4_2.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)

colnames(Day4_merge) <- c("family", paste("Day4", c(1:2), sep="-"))
rm(list=setdiff(ls(), c("Day0_merge","Day1_merge","Day2_merge","Day3_merge","Day4_merge")))

#Day6
Day6_1.rawCounts <- read.table(file="Day6-1.rep.count.bed", sep="\t",col.names=c("count_1","family", "class"))[,c(1,2)]
Day6_2.rawCounts <- read.table(file="Day6-2.rep.count.bed", sep="\t",col.names=c("count_2","family", "class"))[,c(1,2)]
#remove duplicated row
Day6_1.rawCounts <- Day6_1.rawCounts[!duplicated(Day6_1.rawCounts$family), ]
Day6_2.rawCounts <- Day6_2.rawCounts[!duplicated(Day6_2.rawCounts$family), ]
#merge
Day6_merge <- merge(Day6_1.rawCounts, Day6_2.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)

colnames(Day6_merge) <- c("family", paste("Day6", c(1:2), sep="-"))
rm(list=setdiff(ls(), c("Day0_merge","Day1_merge","Day2_merge","Day3_merge","Day4_merge",
                        "Day6_merge")))

#Day8
Day8_1.rawCounts <- read.table(file="Day8-1.rep.count.bed", sep="\t",col.names=c("count_1","family", "class"))[,c(1,2)]
Day8_2.rawCounts <- read.table(file="Day8-2.rep.count.bed", sep="\t",col.names=c("count_2","family", "class"))[,c(1,2)]
#remove duplicated row
Day8_1.rawCounts <- Day8_1.rawCounts[!duplicated(Day8_1.rawCounts$family), ]
Day8_2.rawCounts <- Day8_2.rawCounts[!duplicated(Day8_2.rawCounts$family), ]
#merge
Day8_merge <- merge(Day8_1.rawCounts, Day8_2.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)

colnames(Day8_merge) <- c("family", paste("Day8", c(1:2), sep="-"))
rm(list=setdiff(ls(), c("Day0_merge","Day1_merge","Day2_merge","Day3_merge","Day4_merge",
                        "Day6_merge","Day8_merge")))

#Day10
Day10_1.rawCounts <- read.table(file="Day10-1.rep.count.bed", sep="\t",col.names=c("count_1","family", "class"))[,c(1,2)]
Day10_2.rawCounts <- read.table(file="Day10-2.rep.count.bed", sep="\t",col.names=c("count_2","family", "class"))[,c(1,2)]
#remove duplicated row
Day10_1.rawCounts <- Day10_1.rawCounts[!duplicated(Day10_1.rawCounts$family), ]
Day10_2.rawCounts <- Day10_2.rawCounts[!duplicated(Day10_2.rawCounts$family), ]
#merge
Day10_merge <- merge(Day10_1.rawCounts, Day10_2.rawCounts, by.x="family", by.y="family", all.x=T, all.y=T)

colnames(Day10_merge) <- c("family", paste("Day10", c(1:2), sep="-"))
rm(list=setdiff(ls(), c("Day0_merge","Day1_merge","Day2_merge","Day3_merge","Day4_merge",
                        "Day6_merge","Day8_merge","Day10_merge")))

Day0_1_merge <- merge(Day0_merge, Day1_merge, by.x="family", by.y="family", all.x=T, all.y=T)
Day0_2_merge <- merge(Day0_1_merge, Day2_merge, by.x="family", by.y="family", all.x=T, all.y=T)
Day0_3_merge <- merge(Day0_2_merge, Day3_merge, by.x="family", by.y="family", all.x=T, all.y=T)
Day0_4_merge <- merge(Day0_3_merge, Day4_merge, by.x="family", by.y="family", all.x=T, all.y=T)
Day0_6_merge <- merge(Day0_4_merge, Day6_merge, by.x="family", by.y="family", all.x=T, all.y=T)
Day0_8_merge <- merge(Day0_6_merge, Day8_merge, by.x="family", by.y="family", all.x=T, all.y=T)
Day0_10_merge <- merge(Day0_8_merge, Day10_merge, by.x="family", by.y="family", all.x=T, all.y=T)

Day0_10_merge[is.na(Day0_10_merge)] <- 0
ESC.rawCounts <- Day0_10_merge[ ,-1]
rownames(ESC.rawCounts) <- Day0_10_merge$family

rm(list=setdiff(ls(), "ESC.rawCounts"))


#total counts (genes and repeats, non-rRNA)
repeat_reads_ratio <- read.table(file="ES_repeat_reads_ratio_200hits.txt", sep="\t", header=T)
total.read.counts <- repeat_reads_ratio$non.rRNA_reads
ESC.cpm <- data.frame(ESC.rawCounts[1] * 1000000 / total.read.counts[1])
colnames(ESC.cpm) <- colnames(ESC.rawCounts)[1]
for (i in 2:ncol(ESC.rawCounts)){
  ESC.cpm[ ,i] <- ESC.rawCounts[i] * 1000000 / total.read.counts[i]
  colnames(ESC.cpm)[i] <- colnames(ESC.rawCounts)[i]
}



#PCA and clustering plot below
#rename samples according to stages
ESC.colnames <- c(paste("Day0", c(1:2), sep="-"),
                  paste("Day1", c(1:2), sep="-"),
                  paste("Day2", c(1:2), sep="-"),
                  paste("Day3", c(1:2), sep="-"),
                  paste("Day4", c(1:2), sep="-"),
                  paste("Day6", c(1:2), sep="-"),
                  paste("Day8", c(1:2), sep="-"),
                  paste("Day10", c(1:2), sep="-"))


ESC.dat <- data.frame(family=rownames(ESC.cpm), ESC.cpm)

#only use the repeats expressed in both two reciprocal crosses.
DATA <- ESC.cpm
rownames(DATA) <- rownames(ESC.cpm)
colnames(DATA) <- colnames(ESC.cpm)


# load ggplot2 and start to calculate PCA

library(ggplot2)
nS<-ncol(DATA) #number of samples
nG<-nrow(DATA) #number of genes

sample.names<-colnames(DATA)
stage<-unlist(lapply(strsplit(sample.names,"-"),function(x) x[1]))
embryo<-unlist(lapply(strsplit(sample.names,"\\."),function(x) x[1]))


stages<-unique(stage)
# have 11 different stages, define 5 colors for those.
coldef.stage<-c("dark grey","red","green","blue","orange","purple","brown","black")
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

# PCA Plot
ggplot(PC.table) + 
  geom_point(aes(x=PC1, y=PC2, color= Stage), size=2.0) + 
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
