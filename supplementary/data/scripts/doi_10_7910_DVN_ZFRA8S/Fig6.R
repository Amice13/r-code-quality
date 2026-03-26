# In each female, directly show all the normalized female chrX and female auto counts in each stage, for comparison.
#gene read counts files from CM embryos at late2C, 4C, 8C, 16C and early blastocyst stage can be downloaded from GSE168455 at GEO, including
#"late2C_MC.All_rep.total.geneCount.txt"
#"4C_MC.All_rep.total.geneCount.txt"
#"8C_MC.All_rep.total.geneCount.txt"
#"16C_MC.All_rep.total.geneCount.txt"
#earlyB_MC.All_rep.total.geneCount.txt"

rm(list=ls())
#------late2cell Rawdata-----------------
late2cell.comp <- read.table(file="late2C_MC.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)
colnames(late2cell.comp) <- c("Chr","l2.Comp2","l2.Comp3","l2.Comp4","l2.Comp5","l2.Comp6","l2.Comp7","l2.Comp8","l2.Comp9","l2.Comp10","l2.Comp11")
Female <- c(2,3,4,6,8)
F.late2cell.comp.Auto <- late2cell.comp[which(late2cell.comp$Chr != "chrX" & late2cell.comp$Chr != "chrY" ), c(3,4,5,7,9)]
M.late2cell.comp.Auto <- late2cell.comp[which(late2cell.comp$Chr != "chrX" & late2cell.comp$Chr != "chrY" ), c(2,6,8,10,11)]
total.F.L2.auto <- colSums(F.late2cell.comp.Auto)
total.M.L2.auto <- colSums(M.late2cell.comp.Auto)

#------N4cell Rawdata-----------------
N4cell.comp <- read.table(file="4C_MC.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)
colnames(N4cell.comp) <- c("Chr","4.Comp1","4.Comp2","4.Comp3","4.Comp4",
                           "4.Comp5","4.Comp7","4.Comp8","4.Comp9","4.Comp10",
                           "4.Comp11","4.Comp12","4.Comp13","4.Comp14",
                           "4.Comp15","4.Comp16","4.Comp17","4.Comp18")
Female <- c(4,6,11,14,15,16)
F.N4cell.comp.Auto <- N4cell.comp[which(N4cell.comp$Chr != "chrX" & N4cell.comp$Chr != "chrY" ), c(5,7,12,15,16,17)]
M.N4cell.comp.Auto <- N4cell.comp[which(N4cell.comp$Chr != "chrX" & N4cell.comp$Chr != "chrY" ), c(2,3,4,6,8,9,10,11,13,14,18)]
total.F.N4.auto <- colSums(F.N4cell.comp.Auto)
total.M.N4.auto <- colSums(M.N4cell.comp.Auto)

#------N8cell Rawdata-----------------
N8cell.comp <- read.table(file="8C_MC.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)
colnames(N8cell.comp) <- c("Chr","8.Comp6","8.Comp7","8.Comp8","8.Comp9",
                           "8.Comp10","8.Comp11","8.Comp12","8.Comp13",
                           "8.Comp14","8.Comp15","8.Comp16","8.Comp17","8.Comp18","8.Comp19","8.Comp20")
Female <- c(1,5,7,10,14,15)
F.N8cell.comp.Auto <- N8cell.comp[which(N8cell.comp$Chr != "chrX" & N8cell.comp$Chr != "chrY" ), c(2,6,8,11,15,16)]
M.N8cell.comp.Auto <- N8cell.comp[which(N8cell.comp$Chr != "chrX" & N8cell.comp$Chr != "chrY" ), c(3,4,5,7,9,10,12,13,14)]
total.F.N8.auto <- colSums(F.N8cell.comp.Auto)
total.M.N8.auto <- colSums(M.N8cell.comp.Auto)

#------N16cell Rawdata-----------------
N16cell.comp <- read.table(file="16C_MC.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)
colnames(N16cell.comp) <- c("Chr","16.Comp1","16.Comp2","16.Comp3","16.Comp4","16.Comp5","16.Comp6","16.Comp7","16.Comp8")
Female <- c(2,3,5,6,7)
F.N16cell.comp.Auto <- N16cell.comp[which(N16cell.comp$Chr != "chrX" & N16cell.comp$Chr != "chrY" ), c(3,4,6,7,8)]
M.N16cell.comp.Auto <- N16cell.comp[which(N16cell.comp$Chr != "chrX" & N16cell.comp$Chr != "chrY" ), c(2,5,9)]
total.F.N16.auto <- colSums(F.N16cell.comp.Auto)
total.M.N16.auto <- colSums(M.N16cell.comp.Auto)
#------earlyBlast Rawdata-----------------
earlyBlast.comp <- read.table(file="earlyB_MC.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)
colnames(earlyBlast.comp) <- c("Chr","eB.Comp1","eB.Comp2","eB.Comp3","eB.Comp4","eB.Comp5",
                               "eB.Comp6","eB.Comp7","eB.Comp8","eB.Comp9","eB.Comp10","eB.Comp11")
Female <- c(1,4,5,8,10)
F.eB.comp.Auto <- earlyBlast.comp[which(earlyBlast.comp$Chr != "chrX" & earlyBlast.comp$Chr != "chrY" ), c(2,5,6,9,11)]
M.eB.comp.Auto <- earlyBlast.comp[which(earlyBlast.comp$Chr != "chrX" & earlyBlast.comp$Chr != "chrY" ), c(3,4,7,8,10,12)]
total.F.eB.auto <- colSums(F.eB.comp.Auto)
total.M.eB.auto <- colSums(M.eB.comp.Auto)


#Panel a. female------------------------------------------------------

#late2C
#chrX
late2cell_3.chrX <- sum(read.table(file="processed/Fig6/late2cell-3.REF.sum.chrX", sep="\t")[ ,1])
late2cell_4.chrX <- sum(read.table(file="processed/Fig6/late2cell-4.REF.sum.chrX", sep="\t")[ ,1])
late2cell_5.chrX <- sum(read.table(file="processed/Fig6/late2cell-5.REF.sum.chrX", sep="\t")[ ,1])
late2cell_7.chrX <- sum(read.table(file="processed/Fig6/late2cell-7.REF.sum.chrX", sep="\t")[ ,1])
late2cell_9.chrX <- sum(read.table(file="processed/Fig6/late2cell-9.REF.sum.chrX", sep="\t")[ ,1])
#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
late2cell_3.auto <- sum(read.table(file="processed/Fig6/late2cell-3.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/late2cell-3.CAST.sum.auto", sep="\t")[ ,1])/19
late2cell_4.auto <- sum(read.table(file="processed/Fig6/late2cell-4.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/late2cell-4.CAST.sum.auto", sep="\t")[ ,1])/19
late2cell_5.auto <- sum(read.table(file="processed/Fig6/late2cell-5.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/late2cell-5.CAST.sum.auto", sep="\t")[ ,1])/19
late2cell_7.auto <- sum(read.table(file="processed/Fig6/late2cell-7.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/late2cell-7.CAST.sum.auto", sep="\t")[ ,1])/19
late2cell_9.auto <- sum(read.table(file="processed/Fig6/late2cell-9.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/late2cell-9.CAST.sum.auto", sep="\t")[ ,1])/19

late2cell.F.chrX.normalized <- data.frame(data=c(late2cell_3.chrX*1000000/total.F.L2.auto[1], late2cell_4.chrX*1000000/total.F.L2.auto[2], late2cell_5.chrX*1000000/total.F.L2.auto[3], late2cell_7.chrX*1000000/total.F.L2.auto[4], late2cell_9.chrX*1000000/total.F.L2.auto[5]), stage="late2C", chr="ChrX", Gender="Female")
late2cell.F.auto.normalized <- data.frame(data=c(late2cell_3.auto*1000000/total.F.L2.auto[1], late2cell_4.auto*1000000/total.F.L2.auto[2], late2cell_5.auto*1000000/total.F.L2.auto[3], late2cell_7.auto*1000000/total.F.L2.auto[4], late2cell_9.auto*1000000/total.F.L2.auto[5]), stage="late2C",chr="Auto", Gender="Female")

#female
#4C
#chrX
N4cell_4.chrX <- sum(read.table(file="processed/Fig6/4cell-4.REF.sum.chrX", sep="\t")[ ,1])
N4cell_7.chrX <- sum(read.table(file="processed/Fig6/4cell-7.REF.sum.chrX", sep="\t")[ ,1])
N4cell_12.chrX <- sum(read.table(file="processed/Fig6/4cell-12.REF.sum.chrX", sep="\t")[ ,1])
N4cell_15.chrX <- sum(read.table(file="processed/Fig6/4cell-15.REF.sum.chrX", sep="\t")[ ,1])
N4cell_16.chrX <- sum(read.table(file="processed/Fig6/4cell-16.REF.sum.chrX", sep="\t")[ ,1])
N4cell_17.chrX <- sum(read.table(file="processed/Fig6/4cell-17.REF.sum.chrX", sep="\t")[ ,1])
#auto
N4cell_4.auto <- sum(read.table(file="processed/Fig6/4cell-4.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/4cell-4.CAST.sum.auto", sep="\t")[ ,1])/19
N4cell_7.auto <- sum(read.table(file="processed/Fig6/4cell-7.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/4cell-7.CAST.sum.auto", sep="\t")[ ,1])/19
N4cell_12.auto <- sum(read.table(file="processed/Fig6/4cell-12.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/4cell-12.CAST.sum.auto", sep="\t")[ ,1])/19
N4cell_15.auto <- sum(read.table(file="processed/Fig6/4cell-15.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/4cell-15.CAST.sum.auto", sep="\t")[ ,1])/19
N4cell_16.auto <- sum(read.table(file="processed/Fig6/4cell-16.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/4cell-16.CAST.sum.auto", sep="\t")[ ,1])/19
N4cell_17.auto <- sum(read.table(file="processed/Fig6/4cell-17.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/4cell-17.CAST.sum.auto", sep="\t")[ ,1])/19

N4cell.F.chrX.normalized <- data.frame(data=c(N4cell_4.chrX*1000000/total.F.N4.auto[1], N4cell_7.chrX*1000000/total.F.N4.auto[2], N4cell_12.chrX*1000000/total.F.N4.auto[3], N4cell_15.chrX*1000000/total.F.N4.auto[4], N4cell_16.chrX*1000000/total.F.N4.auto[5], N4cell_17.chrX*1000000/total.F.N4.auto[6]), stage="4C", chr="ChrX", Gender="Female")
N4cell.F.auto.normalized <- data.frame(data=c(N4cell_4.auto*1000000/total.F.N4.auto[1], N4cell_7.auto*1000000/total.F.N4.auto[2], N4cell_12.auto*1000000/total.F.N4.auto[3], N4cell_15.auto*1000000/total.F.N4.auto[4], N4cell_16.auto*1000000/total.F.N4.auto[5], N4cell_17.auto*1000000/total.F.N4.auto[6]), stage="4C",chr="Auto", Gender="Female")

#8C
#Female
#chrX
N8cell_6.chrX <- sum(read.table(file="processed/Fig6/8cell-6.REF.sum.chrX", sep="\t")[ ,1])
N8cell_10.chrX <- sum(read.table(file="processed/Fig6/8cell-10.REF.sum.chrX", sep="\t")[ ,1])
N8cell_12.chrX <- sum(read.table(file="processed/Fig6/8cell-12.REF.sum.chrX", sep="\t")[ ,1])
N8cell_15.chrX <- sum(read.table(file="processed/Fig6/8cell-15.REF.sum.chrX", sep="\t")[ ,1])
N8cell_19.chrX <- sum(read.table(file="processed/Fig6/8cell-19.REF.sum.chrX", sep="\t")[ ,1])
N8cell_20.chrX <- sum(read.table(file="processed/Fig6/8cell-20.REF.sum.chrX", sep="\t")[ ,1])
#auto
N8cell_6.auto <- sum(read.table(file="processed/Fig6/8cell-6.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/8cell-6.CAST.sum.auto", sep="\t")[ ,1])/19
N8cell_10.auto <- sum(read.table(file="processed/Fig6/8cell-10.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/8cell-10.CAST.sum.auto", sep="\t")[ ,1])/19
N8cell_12.auto <- sum(read.table(file="processed/Fig6/8cell-12.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/8cell-12.CAST.sum.auto", sep="\t")[ ,1])/19
N8cell_15.auto <- sum(read.table(file="processed/Fig6/8cell-15.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/8cell-15.CAST.sum.auto", sep="\t")[ ,1])/19
N8cell_19.auto <- sum(read.table(file="processed/Fig6/8cell-19.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/8cell-19.CAST.sum.auto", sep="\t")[ ,1])/19
N8cell_20.auto <- sum(read.table(file="processed/Fig6/8cell-20.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/8cell-20.CAST.sum.auto", sep="\t")[ ,1])/19

N8cell.F.chrX.normalized <- data.frame(data=c(N8cell_6.chrX*1000000/total.F.N8.auto[1], N8cell_10.chrX*1000000/total.F.N8.auto[2], N8cell_12.chrX*1000000/total.F.N8.auto[3], N8cell_15.chrX*1000000/total.F.N8.auto[4], N8cell_19.chrX*1000000/total.F.N8.auto[5], N8cell_20.chrX*1000000/total.F.N8.auto[6]), stage="8C",chr="ChrX", Gender="Female")
N8cell.F.auto.normalized <- data.frame(data=c(N8cell_6.auto*1000000/total.F.N8.auto[1], N8cell_10.auto*1000000/total.F.N8.auto[2], N8cell_12.auto*1000000/total.F.N8.auto[3], N8cell_15.auto*1000000/total.F.N8.auto[4], N8cell_19.auto*1000000/total.F.N8.auto[5], N8cell_20.auto*1000000/total.F.N8.auto[6]), stage="8C", chr="Auto", Gender="Female")

#16C
#Female
#chrX
N16cell_2.chrX <- sum(read.table(file="processed/Fig6/16cell-2.REF.sum.chrX", sep="\t")[ ,1])
N16cell_3.chrX <- sum(read.table(file="processed/Fig6/16cell-3.REF.sum.chrX", sep="\t")[ ,1])
N16cell_5.chrX <- sum(read.table(file="processed/Fig6/16cell-5.REF.sum.chrX", sep="\t")[ ,1])
N16cell_6.chrX <- sum(read.table(file="processed/Fig6/16cell-6.REF.sum.chrX", sep="\t")[ ,1])
N16cell_7.chrX <- sum(read.table(file="processed/Fig6/16cell-7.REF.sum.chrX", sep="\t")[ ,1])
#Auto
N16cell_2.auto <- sum(read.table(file="processed/Fig6/16cell-2.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/16cell-2.CAST.sum.auto", sep="\t")[ ,1])/19
N16cell_3.auto <- sum(read.table(file="processed/Fig6/16cell-3.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/16cell-3.CAST.sum.auto", sep="\t")[ ,1])/19
N16cell_5.auto <- sum(read.table(file="processed/Fig6/16cell-5.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/16cell-5.CAST.sum.auto", sep="\t")[ ,1])/19
N16cell_6.auto <- sum(read.table(file="processed/Fig6/16cell-6.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/16cell-6.CAST.sum.auto", sep="\t")[ ,1])/19
N16cell_7.auto <- sum(read.table(file="processed/Fig6/16cell-7.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/16cell-7.CAST.sum.auto", sep="\t")[ ,1])/19

N16cell.F.chrX.normalized <- data.frame(data=c(N16cell_2.chrX*1000000/total.F.N16.auto[1], N16cell_3.chrX*1000000/total.F.N16.auto[2], N16cell_5.chrX*1000000/total.F.N16.auto[3], N16cell_6.chrX*1000000/total.F.N16.auto[4], N16cell_7.chrX*1000000/total.F.N16.auto[5]), stage="16C", chr="ChrX", Gender="Female")
N16cell.F.auto.normalized <- data.frame(data=c(N16cell_2.auto*1000000/total.F.N16.auto[1], N16cell_3.auto*1000000/total.F.N16.auto[2], N16cell_5.auto*1000000/total.F.N16.auto[3], N16cell_6.auto*1000000/total.F.N16.auto[4], N16cell_7.auto*1000000/total.F.N16.auto[5]), stage="16C", chr="Auto", Gender="Female")

#eB
#Female
#chrX
eB_1.chrX <- sum(read.table(file="processed/Fig6/earlyBlast-1.REF.sum.chrX", sep="\t")[ ,1])
eB_4.chrX <- sum(read.table(file="processed/Fig6/earlyBlast-4.REF.sum.chrX", sep="\t")[ ,1])
eB_5.chrX <- sum(read.table(file="processed/Fig6/earlyBlast-5.REF.sum.chrX", sep="\t")[ ,1])
eB_8.chrX <- sum(read.table(file="processed/Fig6/earlyBlast-8.REF.sum.chrX", sep="\t")[ ,1])
eB_10.chrX <- sum(read.table(file="processed/Fig6/earlyBlast-10.REF.sum.chrX", sep="\t")[ ,1])
#Auto
eB_1.auto <- sum(read.table(file="processed/Fig6/earlyBlast-1.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/earlyBlast-1.CAST.sum.auto", sep="\t")[ ,1])/19
eB_4.auto <- sum(read.table(file="processed/Fig6/earlyBlast-4.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/earlyBlast-4.CAST.sum.auto", sep="\t")[ ,1])/19
eB_5.auto <- sum(read.table(file="processed/Fig6/earlyBlast-5.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/earlyBlast-5.CAST.sum.auto", sep="\t")[ ,1])/19
eB_8.auto <- sum(read.table(file="processed/Fig6/earlyBlast-8.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/earlyBlast-8.CAST.sum.auto", sep="\t")[ ,1])/19
eB_10.auto <- sum(read.table(file="processed/Fig6/earlyBlast-10.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/earlyBlast-10.CAST.sum.auto", sep="\t")[ ,1])/19

eB.F.chrX.normalized <- data.frame(data=c(eB_1.chrX*1000000/total.F.eB.auto[1], eB_4.chrX*1000000/total.F.eB.auto[2], eB_5.chrX*1000000/total.F.eB.auto[3], eB_8.chrX*1000000/total.F.eB.auto[4], eB_10.chrX*1000000/total.F.eB.auto[5]), stage="earlyB.", chr="ChrX", Gender="Female")
eB.F.auto.normalized <- data.frame(data=c(eB_1.auto*1000000/total.F.eB.auto[1], eB_4.auto*1000000/total.F.eB.auto[2], eB_5.auto*1000000/total.F.eB.auto[3], eB_8.auto*1000000/total.F.eB.auto[4], eB_10.auto*1000000/total.F.eB.auto[5]), stage="earlyB.", chr="Auto", Gender="Female")
#sum up

F.Mat.chrX.dat <- rbind(late2cell.F.chrX.normalized, N4cell.F.chrX.normalized, N8cell.F.chrX.normalized, N16cell.F.chrX.normalized, eB.F.chrX.normalized)
F.Mat.auto.dat <- rbind(late2cell.F.auto.normalized, N4cell.F.auto.normalized, N8cell.F.auto.normalized, N16cell.F.auto.normalized, eB.F.auto.normalized)
dat <- rbind(F.Mat.chrX.dat, F.Mat.auto.dat)

F.Mat.chrX.dat$data <- F.Mat.chrX.dat$data / median(late2cell.F.chrX.normalized$data)
F.Mat.auto.dat$data <- F.Mat.auto.dat$data / median(late2cell.F.auto.normalized$data)
F.Mat.dat <- rbind(F.Mat.chrX.dat, F.Mat.auto.dat)

F.Mat.dat$stage <- factor(F.Mat.dat$stage, level=c("late2C","4C","8C","16C","earlyB."))


ggplot(F.Mat.dat, aes(x=stage, y=data, fill=chr)) +
  geom_boxplot(outlier.shape=NA,alpha =0.9) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
             aes(fill=chr), size=1.3, pch=21,stroke = 0.4) +
  scale_fill_manual(values=c("#FF3300","#3399FF")) +
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



#p-value, t-test
#4C p=0.3772
N4C.X <- F.Mat.dat[which(F.Mat.dat$stage=="4C"& F.Mat.dat$chr=="ChrX"), 1]
N4C.Auto <- F.Mat.dat[which(F.Mat.dat$stage=="4C"& F.Mat.dat$chr=="Auto"), 1]
t.test(x=N4C.X, y=N4C.Auto, paired=T)
#8C, p=0.6892
N8C.X <- F.Mat.dat[which(F.Mat.dat$stage=="8C"& F.Mat.dat$chr=="ChrX"), 1]
N8C.Auto <- F.Mat.dat[which(F.Mat.dat$stage=="8C"& F.Mat.dat$chr=="Auto"), 1]
t.test(x=N8C.X, y=N8C.Auto, paired=T)
#16C p=0.11
N16C.X <- F.Mat.dat[which(F.Mat.dat$stage=="16C"& F.Mat.dat$chr=="ChrX"), 1]
N16C.Auto <- F.Mat.dat[which(F.Mat.dat$stage=="16C"& F.Mat.dat$chr=="Auto"), 1]
t.test(x=N16C.X, y=N16C.Auto)
#earlyB p=0.363
eB.X <- F.Mat.dat[which(F.Mat.dat$stage=="earlyB."& F.Mat.dat$chr=="ChrX"), 1]
eB.Auto <- F.Mat.dat[which(F.Mat.dat$stage=="earlyB."& F.Mat.dat$chr=="Auto"), 1]
t.test(x=eB.X, y=eB.Auto,paired=T)



#Panel b. Male-----------------------------------------------
#Becasue there is only one X in the male, so all chrX read would be counted in male.It does not matter which allele it is predicted from.
#late2C
#chrX
late2cell_2.chrX <- sum(read.table(file="processed/Fig6/late2cell-2.REF.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/late2cell-2.CAST.sum.chrX", sep="\t")[ ,1])
late2cell_6.chrX <- sum(read.table(file="processed/Fig6/late2cell-6.REF.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/late2cell-6.CAST.sum.chrX", sep="\t")[ ,1])
late2cell_8.chrX <- sum(read.table(file="processed/Fig6/late2cell-8.REF.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/late2cell-8.CAST.sum.chrX", sep="\t")[ ,1])
late2cell_10.chrX <- sum(read.table(file="processed/Fig6/late2cell-10.REF.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/late2cell-10.CAST.sum.chrX", sep="\t")[ ,1])
late2cell_11.chrX <- sum(read.table(file="processed/Fig6/late2cell-11.REF.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/late2cell-11.CAST.sum.chrX", sep="\t")[ ,1])
#auto (probably should divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
late2cell_2.auto <- sum(read.table(file="processed/Fig6/late2cell-2.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/late2cell-2.CAST.sum.auto", sep="\t")[ ,1])/19
late2cell_6.auto <- sum(read.table(file="processed/Fig6/late2cell-6.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/late2cell-6.CAST.sum.auto", sep="\t")[ ,1])/19
late2cell_8.auto <- sum(read.table(file="processed/Fig6/late2cell-8.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/late2cell-8.CAST.sum.auto", sep="\t")[ ,1])/19
late2cell_10.auto <- sum(read.table(file="processed/Fig6/late2cell-10.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/late2cell-10.CAST.sum.auto", sep="\t")[ ,1])/19
late2cell_11.auto <- sum(read.table(file="processed/Fig6/late2cell-11.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/late2cell-11.CAST.sum.auto", sep="\t")[ ,1])/19

late2cell.M.chrX.normalized <- data.frame(data=c(late2cell_2.chrX*1000000/total.M.L2.auto[1], late2cell_6.chrX*1000000/total.M.L2.auto[2], late2cell_8.chrX*1000000/total.M.L2.auto[3], late2cell_10.chrX*1000000/total.M.L2.auto[4], late2cell_11.chrX*1000000/total.M.L2.auto[5]), stage="late2C", chr="ChrX", Gender="Male")
late2cell.M.auto.normalized <- data.frame(data=c(late2cell_2.auto*1000000/total.M.L2.auto[1], late2cell_6.auto*1000000/total.M.L2.auto[2], late2cell_8.auto*1000000/total.M.L2.auto[3], late2cell_10.auto*1000000/total.M.L2.auto[4], late2cell_11.auto*1000000/total.M.L2.auto[5]), stage="late2C",chr="Auto", Gender="Male")

#4C
#chrX
N4cell_1.chrX <- sum(read.table(file="processed/Fig6/4cell-1.REF.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/4cell-1.CAST.sum.chrX", sep="\t")[ ,1])
N4cell_2.chrX <- sum(read.table(file="processed/Fig6/4cell-2.REF.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/4cell-2.CAST.sum.chrX", sep="\t")[ ,1])
N4cell_3.chrX <- sum(read.table(file="processed/Fig6/4cell-3.REF.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/4cell-3.CAST.sum.chrX", sep="\t")[ ,1])
N4cell_5.chrX <- sum(read.table(file="processed/Fig6/4cell-5.REF.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/4cell-5.CAST.sum.chrX", sep="\t")[ ,1])
N4cell_8.chrX <- sum(read.table(file="processed/Fig6/4cell-8.REF.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/4cell-8.CAST.sum.chrX", sep="\t")[ ,1])
N4cell_9.chrX <- sum(read.table(file="processed/Fig6/4cell-9.REF.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/4cell-9.CAST.sum.chrX", sep="\t")[ ,1])
N4cell_10.chrX <- sum(read.table(file="processed/Fig6/4cell-10.REF.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/4cell-10.CAST.sum.chrX", sep="\t")[ ,1])
N4cell_11.chrX <- sum(read.table(file="processed/Fig6/4cell-11.REF.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/4cell-11.CAST.sum.chrX", sep="\t")[ ,1])
N4cell_13.chrX <- sum(read.table(file="processed/Fig6/4cell-13.REF.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/4cell-13.CAST.sum.chrX", sep="\t")[ ,1])
N4cell_14.chrX <- sum(read.table(file="processed/Fig6/4cell-14.REF.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/4cell-14.CAST.sum.chrX", sep="\t")[ ,1])
N4cell_18.chrX <- sum(read.table(file="processed/Fig6/4cell-18.REF.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/4cell-18.CAST.sum.chrX", sep="\t")[ ,1])

#auto
N4cell_1.auto <- sum(read.table(file="processed/Fig6/4cell-1.CAST.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/4cell-1.REF.sum.auto", sep="\t")[ ,1])/19
N4cell_2.auto <- sum(read.table(file="processed/Fig6/4cell-2.CAST.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/4cell-2.REF.sum.auto", sep="\t")[ ,1])/19
N4cell_3.auto <- sum(read.table(file="processed/Fig6/4cell-3.CAST.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/4cell-3.REF.sum.auto", sep="\t")[ ,1])/19
N4cell_5.auto <- sum(read.table(file="processed/Fig6/4cell-5.CAST.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/4cell-5.REF.sum.auto", sep="\t")[ ,1])/19
N4cell_8.auto <- sum(read.table(file="processed/Fig6/4cell-8.CAST.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/4cell-8.REF.sum.auto", sep="\t")[ ,1])/19
N4cell_9.auto <- sum(read.table(file="processed/Fig6/4cell-9.CAST.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/4cell-9.REF.sum.auto", sep="\t")[ ,1])/19
N4cell_10.auto <- sum(read.table(file="processed/Fig6/4cell-10.CAST.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/4cell-10.REF.sum.auto", sep="\t")[ ,1])/19
N4cell_11.auto <- sum(read.table(file="processed/Fig6/4cell-11.CAST.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/4cell-11.REF.sum.auto", sep="\t")[ ,1])/19
N4cell_13.auto <- sum(read.table(file="processed/Fig6/4cell-13.CAST.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/4cell-13.REF.sum.auto", sep="\t")[ ,1])/19
N4cell_14.auto <- sum(read.table(file="processed/Fig6/4cell-14.CAST.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/4cell-14.REF.sum.auto", sep="\t")[ ,1])/19
N4cell_18.auto <- sum(read.table(file="processed/Fig6/4cell-18.CAST.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/4cell-18.REF.sum.auto", sep="\t")[ ,1])/19



N4cell.M.chrX.normalized <- data.frame(data=c(N4cell_1.chrX*1000000/total.M.N4.auto[1], N4cell_2.chrX*1000000/total.M.N4.auto[2], N4cell_3.chrX*1000000/total.M.N4.auto[3], 
                                              N4cell_5.chrX*1000000/total.M.N4.auto[4], N4cell_8.chrX*1000000/total.M.N4.auto[5], N4cell_9.chrX*1000000/total.M.N4.auto[6], 
                                              N4cell_10.chrX*1000000/total.M.N4.auto[7], N4cell_11.chrX*1000000/total.M.N4.auto[8], N4cell_13.chrX*1000000/total.M.N4.auto[9],
                                              N4cell_14.chrX*1000000/total.M.N4.auto[10], N4cell_18.chrX*1000000/total.M.N4.auto[11]),
                                       stage="4C", chr="ChrX", Gender="Male")
N4cell.M.auto.normalized <- data.frame(data=c(N4cell_1.auto*1000000/total.M.N4.auto[1], N4cell_2.auto*1000000/total.M.N4.auto[2], N4cell_3.auto*1000000/total.M.N4.auto[3], 
                                              N4cell_5.auto*1000000/total.M.N4.auto[4], N4cell_8.auto*1000000/total.M.N4.auto[5], N4cell_9.auto*1000000/total.M.N4.auto[6], 
                                              N4cell_10.auto*1000000/total.M.N4.auto[7], N4cell_11.auto*1000000/total.M.N4.auto[8], N4cell_13.auto*1000000/total.M.N4.auto[9],
                                              N4cell_14.auto*1000000/total.M.N4.auto[10], N4cell_18.auto*1000000/total.M.N4.auto[11]),
                                      stage="4C", chr="Auto", Gender="Male")

#8C
#chrX
N8cell_7.chrX <- sum(read.table(file="processed/Fig6/8cell-7.REF.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/8cell-7.CAST.sum.chrX", sep="\t")[ ,1])
N8cell_8.chrX <- sum(read.table(file="processed/Fig6/8cell-8.REF.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/8cell-8.CAST.sum.chrX", sep="\t")[ ,1])
N8cell_9.chrX <- sum(read.table(file="processed/Fig6/8cell-9.REF.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/8cell-9.CAST.sum.chrX", sep="\t")[ ,1])
N8cell_11.chrX <- sum(read.table(file="processed/Fig6/8cell-11.REF.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/8cell-11.CAST.sum.chrX", sep="\t")[ ,1])
N8cell_13.chrX <- sum(read.table(file="processed/Fig6/8cell-13.REF.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/8cell-13.CAST.sum.chrX", sep="\t")[ ,1])
N8cell_14.chrX <- sum(read.table(file="processed/Fig6/8cell-14.REF.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/8cell-14.CAST.sum.chrX", sep="\t")[ ,1])
N8cell_16.chrX <- sum(read.table(file="processed/Fig6/8cell-16.REF.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/8cell-16.CAST.sum.chrX", sep="\t")[ ,1])
N8cell_17.chrX <- sum(read.table(file="processed/Fig6/8cell-17.REF.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/8cell-17.CAST.sum.chrX", sep="\t")[ ,1])
N8cell_18.chrX <- sum(read.table(file="processed/Fig6/8cell-18.REF.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/8cell-18.CAST.sum.chrX", sep="\t")[ ,1])
#auto
N8cell_7.auto <- sum(read.table(file="processed/Fig6/8cell-7.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/8cell-7.CAST.sum.auto", sep="\t")[ ,1])/19
N8cell_8.auto <- sum(read.table(file="processed/Fig6/8cell-8.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/8cell-8.CAST.sum.auto", sep="\t")[ ,1])/19
N8cell_9.auto <- sum(read.table(file="processed/Fig6/8cell-9.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/8cell-9.CAST.sum.auto", sep="\t")[ ,1])/19
N8cell_11.auto <- sum(read.table(file="processed/Fig6/8cell-11.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/8cell-11.CAST.sum.auto", sep="\t")[ ,1])/19
N8cell_13.auto <- sum(read.table(file="processed/Fig6/8cell-13.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/8cell-13.CAST.sum.auto", sep="\t")[ ,1])/19
N8cell_14.auto <- sum(read.table(file="processed/Fig6/8cell-14.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/8cell-14.CAST.sum.auto", sep="\t")[ ,1])/19
N8cell_16.auto <- sum(read.table(file="processed/Fig6/8cell-16.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/8cell-16.CAST.sum.auto", sep="\t")[ ,1])/19
N8cell_17.auto <- sum(read.table(file="processed/Fig6/8cell-17.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/8cell-17.CAST.sum.auto", sep="\t")[ ,1])/19
N8cell_18.auto <- sum(read.table(file="processed/Fig6/8cell-18.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/8cell-18.CAST.sum.auto", sep="\t")[ ,1])/19


N8cell.M.chrX.normalized <- data.frame(data=c(N8cell_7.chrX*1000000/total.M.N8.auto[1], N8cell_8.chrX*1000000/total.M.N8.auto[2], N8cell_9.chrX*1000000/total.M.N8.auto[3], 
                                              N8cell_11.chrX*1000000/total.M.N8.auto[4], N8cell_13.chrX*1000000/total.M.N8.auto[5], N8cell_14.chrX*1000000/total.M.N8.auto[6], 
                                              N8cell_16.chrX*1000000/total.M.N8.auto[7], N8cell_17.chrX*1000000/total.M.N8.auto[8], N8cell_18.chrX*1000000/total.M.N8.auto[9]), 
                                       stage="8C",chr="ChrX", Gender="Male")

N8cell.M.auto.normalized <- data.frame(data=c(N8cell_7.auto*1000000/total.M.N8.auto[1], N8cell_8.auto*1000000/total.M.N8.auto[2], N8cell_9.auto*1000000/total.M.N8.auto[3], 
                                              N8cell_11.auto*1000000/total.M.N8.auto[4], N8cell_13.auto*1000000/total.M.N8.auto[5], N8cell_14.auto*1000000/total.M.N8.auto[6], 
                                              N8cell_16.auto*1000000/total.M.N8.auto[7], N8cell_17.auto*1000000/total.M.N8.auto[8], N8cell_18.auto*1000000/total.M.N8.auto[9]), 
                                       stage="8C",chr="Auto", Gender="Male")

#16C
#chrX
N16cell_1.chrX <- sum(read.table(file="processed/Fig6/16cell-1.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/16cell-1.REF.sum.chrX", sep="\t")[ ,1])
N16cell_4.chrX <- sum(read.table(file="processed/Fig6/16cell-4.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/16cell-4.REF.sum.chrX", sep="\t")[ ,1])
N16cell_8.chrX <- sum(read.table(file="processed/Fig6/16cell-8.CAST.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/16cell-8.REF.sum.chrX", sep="\t")[ ,1])

#Auto
N16cell_1.auto <- sum(read.table(file="processed/Fig6/16cell-2.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/16cell-2.CAST.sum.auto", sep="\t")[ ,1])/19
N16cell_4.auto <- sum(read.table(file="processed/Fig6/16cell-3.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/16cell-3.CAST.sum.auto", sep="\t")[ ,1])/19
N16cell_8.auto <- sum(read.table(file="processed/Fig6/16cell-5.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/16cell-5.CAST.sum.auto", sep="\t")[ ,1])/19

N16cell.M.chrX.normalized <- data.frame(data=c(N16cell_1.chrX*1000000/total.M.N16.auto[1], N16cell_4.chrX*1000000/total.M.N16.auto[2], N16cell_8.chrX*1000000/total.M.N16.auto[3]), 
                                        stage="16C", chr="ChrX", Gender="Male")
N16cell.M.auto.normalized <- data.frame(data=c(N16cell_1.auto*1000000/total.M.N16.auto[1], N16cell_4.auto*1000000/total.M.N16.auto[2], N16cell_8.auto*1000000/total.M.N16.auto[3]), 
                                        stage="16C", chr="Auto", Gender="Male")

#eB
#chrX
eB_2.chrX <- sum(read.table(file="processed/Fig6/earlyBlast-2.REF.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/earlyBlast-2.CAST.sum.chrX", sep="\t")[ ,1])
eB_3.chrX <- sum(read.table(file="processed/Fig6/earlyBlast-3.REF.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/earlyBlast-3.CAST.sum.chrX", sep="\t")[ ,1])
eB_6.chrX <- sum(read.table(file="processed/Fig6/earlyBlast-6.REF.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/earlyBlast-6.CAST.sum.chrX", sep="\t")[ ,1])
eB_7.chrX <- sum(read.table(file="processed/Fig6/earlyBlast-7.REF.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/earlyBlast-7.CAST.sum.chrX", sep="\t")[ ,1])
eB_9.chrX <- sum(read.table(file="processed/Fig6/earlyBlast-9.REF.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/earlyBlast-9.CAST.sum.chrX", sep="\t")[ ,1])
eB_11.chrX <- sum(read.table(file="processed/Fig6/earlyBlast-11.REF.sum.chrX", sep="\t")[ ,1]) + sum(read.table(file="processed/Fig6/earlyBlast-11.CAST.sum.chrX", sep="\t")[ ,1])
#Auto
eB_2.auto <- sum(read.table(file="processed/Fig6/earlyBlast-2.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/earlyBlast-2.CAST.sum.auto", sep="\t")[ ,1])/19
eB_3.auto <- sum(read.table(file="processed/Fig6/earlyBlast-3.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/earlyBlast-3.CAST.sum.auto", sep="\t")[ ,1])/19
eB_6.auto <- sum(read.table(file="processed/Fig6/earlyBlast-6.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/earlyBlast-6.CAST.sum.auto", sep="\t")[ ,1])/19
eB_7.auto <- sum(read.table(file="processed/Fig6/earlyBlast-7.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/earlyBlast-7.CAST.sum.auto", sep="\t")[ ,1])/19
eB_9.auto <- sum(read.table(file="processed/Fig6/earlyBlast-9.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/earlyBlast-9.CAST.sum.auto", sep="\t")[ ,1])/19
eB_11.auto <- sum(read.table(file="processed/Fig6/earlyBlast-11.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/earlyBlast-11.CAST.sum.auto", sep="\t")[ ,1])/19

eB.M.chrX.normalized <- data.frame(data=c(eB_2.chrX*1000000/total.M.eB.auto[1], eB_3.chrX*1000000/total.M.eB.auto[2], eB_6.chrX*1000000/total.M.eB.auto[3], 
                                          eB_7.chrX*1000000/total.M.eB.auto[4], eB_9.chrX*1000000/total.M.eB.auto[5], eB_11.chrX*1000000/total.M.eB.auto[6]), 
                                   stage="earlyB.", chr="ChrX", Gender="Male")

eB.M.auto.normalized <- data.frame(data=c(eB_2.auto*1000000/total.M.eB.auto[1], eB_3.auto*1000000/total.M.eB.auto[2], eB_6.auto*1000000/total.M.eB.auto[3], 
                                          eB_7.auto*1000000/total.M.eB.auto[4], eB_9.auto*1000000/total.M.eB.auto[5], eB_11.auto*1000000/total.M.eB.auto[6]), 
                                   stage="earlyB.", chr="Auto", Gender="Male")
#sum up

M.Mat.chrX.dat <- rbind(late2cell.M.chrX.normalized, N4cell.M.chrX.normalized, N8cell.M.chrX.normalized, N16cell.M.chrX.normalized, eB.M.chrX.normalized)
M.Mat.auto.dat <- rbind(late2cell.M.auto.normalized, N4cell.M.auto.normalized, N8cell.M.auto.normalized, N16cell.M.auto.normalized, eB.M.auto.normalized)
#dat <- rbind(M.Mat.chrX.dat, M.Mat.auto.dat)

M.Mat.chrX.dat$data <- M.Mat.chrX.dat$data / median(late2cell.M.chrX.normalized$data)
M.Mat.auto.dat$data <- M.Mat.auto.dat$data / median(late2cell.M.auto.normalized$data)
M.Mat.dat <- rbind(M.Mat.chrX.dat, M.Mat.auto.dat)

M.Mat.dat$stage <- factor(M.Mat.dat$stage, level=c("late2C","4C","8C","16C","earlyB."))


ggplot(M.Mat.dat, aes(x=stage, y=data, fill=chr)) +
  geom_boxplot(outlier.shape=NA,alpha =0.9) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
             aes(fill=chr), size=1.3, pch=21,stroke = 0.4) +
  scale_fill_manual(values=c("#FF3300","#3399FF")) +
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



#p-value, t-test
#4C p=0.8201
N4C.X <- M.Mat.dat[which(M.Mat.dat$stage=="4C"& M.Mat.dat$chr=="ChrX"), 1]
N4C.Auto <- M.Mat.dat[which(M.Mat.dat$stage=="4C"& M.Mat.dat$chr=="Auto"), 1]
t.test(x=N4C.X, y=N4C.Auto, paired=T)
#8C, p=0.002771
N8C.X <- M.Mat.dat[which(M.Mat.dat$stage=="8C"& M.Mat.dat$chr=="ChrX"), 1]
N8C.Auto <- M.Mat.dat[which(M.Mat.dat$stage=="8C"& M.Mat.dat$chr=="Auto"), 1]
t.test(x=N8C.X, y=N8C.Auto, paired=T)
#16C p=0.3791
N16C.X <- M.Mat.dat[which(M.Mat.dat$stage=="16C"& M.Mat.dat$chr=="ChrX"), 1]
N16C.Auto <- M.Mat.dat[which(M.Mat.dat$stage=="16C"& M.Mat.dat$chr=="Auto"), 1]
t.test(x=N16C.X, y=N16C.Auto,paired=T)
#earlyB p=0.4196
eB.X <- M.Mat.dat[which(M.Mat.dat$stage=="earlyB."& M.Mat.dat$chr=="ChrX"), 1]
eB.Auto <- M.Mat.dat[which(M.Mat.dat$stage=="earlyB."& M.Mat.dat$chr=="Auto"), 1]
t.test(x=eB.X, y=eB.Auto,paired=T)



#Panel c. in EScells----------------------------------

rm(list=ls())
#------late2cell Rawdata-----------------
EScell.comp <- read.table(file="processed/EScell_geneCount.txt", sep="\t", header=T, row.names=1)[ ,c(1,38:53)]
colnames(EScell.comp) <- c("Chr","Day0.Comp1","Day0.Comp2","Day1.Comp1","Day1.Comp2",
                                    "Day2.Comp1","Day2.Comp2","Day3.Comp1","Day3.Comp2",
                                    "Day4.Comp1","Day4.Comp2","Day6.Comp1","Day6.Comp2",
                                    "Day8.Comp1","Day8.Comp2","Day10.Comp1","Day10.Comp2")
EScell.comp.Auto <- EScell.comp[which(EScell.comp$Chr != "chrX" & EScell.comp$Chr != "chrY" ), -1]
total.ES.auto <- colSums(EScell.comp.Auto)

#Day0
#chrX
Day0_1.chrX <- sum(read.table(file="processed/Fig6/Day0-1.CAST.sum.chrX", sep="\t")[ ,1])
Day0_2.chrX <- sum(read.table(file="processed/Fig6/Day0-2.CAST.sum.chrX", sep="\t")[ ,1])
#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
Day0_1.auto <- sum(read.table(file="processed/Fig6/Day0-1.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/Day0-1.CAST.sum.auto", sep="\t")[ ,1])/19
Day0_2.auto <- sum(read.table(file="processed/Fig6/Day0-2.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/Day0-2.CAST.sum.auto", sep="\t")[ ,1])/19

Day0.chrX.normalized <- data.frame(data=c(Day0_1.chrX*1000000/total.ES.auto[1], Day0_2.chrX*1000000/total.ES.auto[2]), stage="Day0", chr="ChrX")
Day0.auto.normalized <- data.frame(data=c(Day0_1.auto*1000000/total.ES.auto[1], Day0_2.auto*1000000/total.ES.auto[2]), stage="Day0",chr="Auto")

#Day1
#chrX
Day1_1.chrX <- sum(read.table(file="processed/Fig6/Day1-1.CAST.sum.chrX", sep="\t")[ ,1])
Day1_2.chrX <- sum(read.table(file="processed/Fig6/Day1-2.CAST.sum.chrX", sep="\t")[ ,1])
#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
Day1_1.auto <- sum(read.table(file="processed/Fig6/Day1-1.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/Day1-1.CAST.sum.auto", sep="\t")[ ,1])/19
Day1_2.auto <- sum(read.table(file="processed/Fig6/Day1-2.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/Day1-2.CAST.sum.auto", sep="\t")[ ,1])/19

Day1.chrX.normalized <- data.frame(data=c(Day1_1.chrX*1000000/total.ES.auto[3], Day1_2.chrX*1000000/total.ES.auto[4]), stage="Day1", chr="ChrX")
Day1.auto.normalized <- data.frame(data=c(Day1_1.auto*1000000/total.ES.auto[3], Day1_2.auto*1000000/total.ES.auto[4]), stage="Day1",chr="Auto")

#Day2
#chrX
Day2_1.chrX <- sum(read.table(file="processed/Fig6/Day2-1.CAST.sum.chrX", sep="\t")[ ,1])
Day2_2.chrX <- sum(read.table(file="processed/Fig6/Day2-2.CAST.sum.chrX", sep="\t")[ ,1])
#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
Day2_1.auto <- sum(read.table(file="processed/Fig6/Day2-1.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/Day2-1.CAST.sum.auto", sep="\t")[ ,1])/19
Day2_2.auto <- sum(read.table(file="processed/Fig6/Day2-2.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/Day2-2.CAST.sum.auto", sep="\t")[ ,1])/19

Day2.chrX.normalized <- data.frame(data=c(Day2_1.chrX*1000000/total.ES.auto[5], Day2_2.chrX*1000000/total.ES.auto[6]), stage="Day2", chr="ChrX")
Day2.auto.normalized <- data.frame(data=c(Day2_1.auto*1000000/total.ES.auto[5], Day2_2.auto*1000000/total.ES.auto[6]), stage="Day2",chr="Auto")

#Day3
#chrX
Day3_1.chrX <- sum(read.table(file="processed/Fig6/Day3-1.CAST.sum.chrX", sep="\t")[ ,1])
Day3_2.chrX <- sum(read.table(file="processed/Fig6/Day3-2.CAST.sum.chrX", sep="\t")[ ,1])
#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
Day3_1.auto <- sum(read.table(file="processed/Fig6/Day3-1.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/Day3-1.CAST.sum.auto", sep="\t")[ ,1])/19
Day3_2.auto <- sum(read.table(file="processed/Fig6/Day3-2.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/Day3-2.CAST.sum.auto", sep="\t")[ ,1])/19

Day3.chrX.normalized <- data.frame(data=c(Day3_1.chrX*1000000/total.ES.auto[7], Day3_2.chrX*1000000/total.ES.auto[8]), stage="Day3", chr="ChrX")
Day3.auto.normalized <- data.frame(data=c(Day3_1.auto*1000000/total.ES.auto[7], Day3_2.auto*1000000/total.ES.auto[8]), stage="Day3",chr="Auto")

#Day4
#chrX
Day4_1.chrX <- sum(read.table(file="processed/Fig6/Day4-1.CAST.sum.chrX", sep="\t")[ ,1])
Day4_2.chrX <- sum(read.table(file="processed/Fig6/Day4-2.CAST.sum.chrX", sep="\t")[ ,1])
#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
Day4_1.auto <- sum(read.table(file="processed/Fig6/Day4-1.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/Day4-1.CAST.sum.auto", sep="\t")[ ,1])/19
Day4_2.auto <- sum(read.table(file="processed/Fig6/Day4-2.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/Day4-2.CAST.sum.auto", sep="\t")[ ,1])/19

Day4.chrX.normalized <- data.frame(data=c(Day4_1.chrX*1000000/total.ES.auto[9], Day4_2.chrX*1000000/total.ES.auto[10]), stage="Day4", chr="ChrX")
Day4.auto.normalized <- data.frame(data=c(Day4_1.auto*1000000/total.ES.auto[9], Day4_2.auto*1000000/total.ES.auto[10]), stage="Day4",chr="Auto")

#Day6
#chrX
Day6_1.chrX <- sum(read.table(file="processed/Fig6/Day6-1.CAST.sum.chrX", sep="\t")[ ,1])
Day6_2.chrX <- sum(read.table(file="processed/Fig6/Day6-2.CAST.sum.chrX", sep="\t")[ ,1])
#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
Day6_1.auto <- sum(read.table(file="processed/Fig6/Day6-1.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/Day6-1.CAST.sum.auto", sep="\t")[ ,1])/19
Day6_2.auto <- sum(read.table(file="processed/Fig6/Day6-2.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/Day6-2.CAST.sum.auto", sep="\t")[ ,1])/19

Day6.chrX.normalized <- data.frame(data=c(Day6_1.chrX*1000000/total.ES.auto[11], Day6_2.chrX*1000000/total.ES.auto[12]), stage="Day6", chr="ChrX")
Day6.auto.normalized <- data.frame(data=c(Day6_1.auto*1000000/total.ES.auto[11], Day6_2.auto*1000000/total.ES.auto[12]), stage="Day6",chr="Auto")

#Day8
#chrX
Day8_1.chrX <- sum(read.table(file="processed/Fig6/Day8-1.CAST.sum.chrX", sep="\t")[ ,1])
Day8_2.chrX <- sum(read.table(file="processed/Fig6/Day8-2.CAST.sum.chrX", sep="\t")[ ,1])
#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
Day8_1.auto <- sum(read.table(file="processed/Fig6/Day8-1.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/Day8-1.CAST.sum.auto", sep="\t")[ ,1])/19
Day8_2.auto <- sum(read.table(file="processed/Fig6/Day8-2.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/Day8-2.CAST.sum.auto", sep="\t")[ ,1])/19

Day8.chrX.normalized <- data.frame(data=c(Day8_1.chrX*1000000/total.ES.auto[13], Day8_2.chrX*1000000/total.ES.auto[14]), stage="Day8", chr="ChrX")
Day8.auto.normalized <- data.frame(data=c(Day8_1.auto*1000000/total.ES.auto[13], Day8_2.auto*1000000/total.ES.auto[14]), stage="Day8",chr="Auto")

#Day10
#chrX
Day10_1.chrX <- sum(read.table(file="processed/Fig6/Day10-1.CAST.sum.chrX", sep="\t")[ ,1])
Day10_2.chrX <- sum(read.table(file="processed/Fig6/Day10-2.CAST.sum.chrX", sep="\t")[ ,1])
#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
Day10_1.auto <- sum(read.table(file="processed/Fig6/Day10-1.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/Day10-1.CAST.sum.auto", sep="\t")[ ,1])/19
Day10_2.auto <- sum(read.table(file="processed/Fig6/Day10-2.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/Day10-2.CAST.sum.auto", sep="\t")[ ,1])/19

Day10.chrX.normalized <- data.frame(data=c(Day10_1.chrX*1000000/total.ES.auto[15], Day10_2.chrX*1000000/total.ES.auto[16]), stage="Day10", chr="ChrX")
Day10.auto.normalized <- data.frame(data=c(Day10_1.auto*1000000/total.ES.auto[15], Day10_2.auto*1000000/total.ES.auto[16]), stage="Day10",chr="Auto")


#sum up

ES.Mat.chrX.dat <- rbind(Day0.chrX.normalized, Day1.chrX.normalized, Day2.chrX.normalized, Day3.chrX.normalized, Day4.chrX.normalized, Day6.chrX.normalized, Day8.chrX.normalized, Day10.chrX.normalized)
ES.Mat.auto.dat <- rbind(Day0.auto.normalized, Day1.auto.normalized, Day2.auto.normalized, Day3.auto.normalized, Day4.auto.normalized, Day6.auto.normalized, Day8.auto.normalized, Day10.auto.normalized)
#dat <- rbind(F.Mat.chrX.dat, F.Mat.auto.dat)

ES.Mat.chrX.dat$data <- ES.Mat.chrX.dat$data / median(Day0.chrX.normalized$data)
ES.Mat.auto.dat$data <- ES.Mat.auto.dat$data / median(Day0.auto.normalized$data)
ES.Mat.dat <- rbind(ES.Mat.chrX.dat, ES.Mat.auto.dat)

ES.Mat.dat$stage <- factor(ES.Mat.dat$stage, level=c("Day0","Day1","Day2","Day3","Day4","Day6","Day8","Day10"))

ggplot(ES.Mat.dat, aes(x=stage, y=data, fill=chr)) +
  geom_boxplot(outlier.shape=NA,alpha =0.9) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
             aes(fill=chr), size=1.3, pch=21,stroke = 0.4) +
  scale_fill_manual(values=c("#FF3300","#3399FF")) +
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


#p-value, t-test
#Day10 p=0.1568
Day10.X <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day10"& ES.Mat.dat$chr=="ChrX"), 1]
Day10.Auto <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day10"& ES.Mat.dat$chr=="Auto"), 1]
t.test(x=Day10.X, y=Day10.Auto, paired=T)
#Day8, p=0.1815
Day8.X <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day8"& ES.Mat.dat$chr=="ChrX"), 1]
Day8.Auto <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day8"& ES.Mat.dat$chr=="Auto"), 1]
t.test(x=Day8.X, y=Day8.Auto, paired=T)
#Day6 p=0.1263
Day6.X <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day6"& ES.Mat.dat$chr=="ChrX"), 1]
Day6.Auto <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day6"& ES.Mat.dat$chr=="Auto"), 1]
t.test(x=Day6.X, y=Day6.Auto, paired=T)

#----------------------------------------------------------------------------------------------------------------------------------------------------------






#Panel d-f
# In females, check Xa upregulation for each TE type during imprinted XCI.
rm(list=ls())

#------late2cell Rawdata-----------------
late2cell.comp <- read.table(file="late2C_MC.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)
colnames(late2cell.comp) <- c("Chr","l2.Comp2","l2.Comp3","l2.Comp4","l2.Comp5","l2.Comp6","l2.Comp7","l2.Comp8","l2.Comp9","l2.Comp10","l2.Comp11")
Female <- c(2,3,4,6,8)
F.late2cell.comp.Auto <- late2cell.comp[which(late2cell.comp$Chr != "chrX" & late2cell.comp$Chr != "chrY" ), c(3,4,5,7,9)]
M.late2cell.comp.Auto <- late2cell.comp[which(late2cell.comp$Chr != "chrX" & late2cell.comp$Chr != "chrY" ), c(2,6,8,10,11)]
total.F.L2.auto <- colSums(F.late2cell.comp.Auto)
total.M.L2.auto <- colSums(M.late2cell.comp.Auto)

#------N4cell Rawdata-----------------
N4cell.comp <- read.table(file="4C_MC.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)
colnames(N4cell.comp) <- c("Chr","4.Comp1","4.Comp2","4.Comp3","4.Comp4",
                           "4.Comp5","4.Comp7","4.Comp8","4.Comp9","4.Comp10",
                           "4.Comp11","4.Comp12","4.Comp13","4.Comp14",
                           "4.Comp15","4.Comp16","4.Comp17","4.Comp18")
Female <- c(4,6,11,14,15,16)
F.N4cell.comp.Auto <- N4cell.comp[which(N4cell.comp$Chr != "chrX" & N4cell.comp$Chr != "chrY" ), c(5,7,12,15,16,17)]
M.N4cell.comp.Auto <- N4cell.comp[which(N4cell.comp$Chr != "chrX" & N4cell.comp$Chr != "chrY" ), c(2,3,4,6,8,9,10,11,13,14,18)]
total.F.N4.auto <- colSums(F.N4cell.comp.Auto)
total.M.N4.auto <- colSums(M.N4cell.comp.Auto)

#------N8cell Rawdata-----------------
N8cell.comp <- read.table(file="8C_MC.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)
colnames(N8cell.comp) <- c("Chr","8.Comp6","8.Comp7","8.Comp8","8.Comp9",
                           "8.Comp10","8.Comp11","8.Comp12","8.Comp13",
                           "8.Comp14","8.Comp15","8.Comp16","8.Comp17","8.Comp18","8.Comp19","8.Comp20")
Female <- c(1,5,7,10,14,15)
F.N8cell.comp.Auto <- N8cell.comp[which(N8cell.comp$Chr != "chrX" & N8cell.comp$Chr != "chrY" ), c(2,6,8,11,15,16)]
M.N8cell.comp.Auto <- N8cell.comp[which(N8cell.comp$Chr != "chrX" & N8cell.comp$Chr != "chrY" ), c(3,4,5,7,9,10,12,13,14)]
total.F.N8.auto <- colSums(F.N8cell.comp.Auto)
total.M.N8.auto <- colSums(M.N8cell.comp.Auto)

#------N16cell Rawdata-----------------
N16cell.comp <- read.table(file="16C_MC.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)
colnames(N16cell.comp) <- c("Chr","16.Comp1","16.Comp2","16.Comp3","16.Comp4","16.Comp5","16.Comp6","16.Comp7","16.Comp8")
Female <- c(2,3,5,6,7)
F.N16cell.comp.Auto <- N16cell.comp[which(N16cell.comp$Chr != "chrX" & N16cell.comp$Chr != "chrY" ), c(3,4,6,7,8)]
M.N16cell.comp.Auto <- N16cell.comp[which(N16cell.comp$Chr != "chrX" & N16cell.comp$Chr != "chrY" ), c(2,5,9)]
total.F.N16.auto <- colSums(F.N16cell.comp.Auto)
total.M.N16.auto <- colSums(M.N16cell.comp.Auto)
#------earlyBlast Rawdata-----------------
earlyBlast.comp <- read.table(file="earlyB_MC.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)
colnames(earlyBlast.comp) <- c("Chr","eB.Comp1","eB.Comp2","eB.Comp3","eB.Comp4","eB.Comp5",
                               "eB.Comp6","eB.Comp7","eB.Comp8","eB.Comp9","eB.Comp10","eB.Comp11")
Female <- c(1,4,5,8,10)
F.eB.comp.Auto <- earlyBlast.comp[which(earlyBlast.comp$Chr != "chrX" & earlyBlast.comp$Chr != "chrY" ), c(2,5,6,9,11)]
M.eB.comp.Auto <- earlyBlast.comp[which(earlyBlast.comp$Chr != "chrX" & earlyBlast.comp$Chr != "chrY" ), c(3,4,7,8,10,12)]
total.F.eB.auto <- colSums(F.eB.comp.Auto)
total.M.eB.auto <- colSums(M.eB.comp.Auto)

rm(list=setdiff(ls(), c("total.F.L2.auto","total.M.L2.auto","total.F.N4.auto","total.M.N4.auto",
                        "total.F.N8.auto","total.M.N8.auto","total.F.N16.auto","total.M.N16.auto",
                        "total.F.eB.auto","total.M.eB.auto")))




#Female

#---------------------------
#---------D.  SINE -----------
#---------------------------

#late2C
late2cell_3.REF.chrX <- read.table(file="processed/Fig6/late2cell-3.REF.sum.chrX", sep="\t")
late2cell_4.REF.chrX <- read.table(file="processed/Fig6/late2cell-4.REF.sum.chrX", sep="\t")
late2cell_5.REF.chrX <- read.table(file="processed/Fig6/late2cell-5.REF.sum.chrX", sep="\t")
late2cell_7.REF.chrX <- read.table(file="processed/Fig6/late2cell-7.REF.sum.chrX", sep="\t")
late2cell_9.REF.chrX <- read.table(file="processed/Fig6/late2cell-9.REF.sum.chrX", sep="\t")

late2cell_3.REF.auto <- read.table(file="processed/Fig6/late2cell-3.REF.sum.auto", sep="\t")
late2cell_4.REF.auto <- read.table(file="processed/Fig6/late2cell-4.REF.sum.auto", sep="\t")
late2cell_5.REF.auto <- read.table(file="processed/Fig6/late2cell-5.REF.sum.auto", sep="\t")
late2cell_7.REF.auto <- read.table(file="processed/Fig6/late2cell-7.REF.sum.auto", sep="\t")
late2cell_9.REF.auto <- read.table(file="processed/Fig6/late2cell-9.REF.sum.auto", sep="\t")

late2cell_3.CAST.auto <- read.table(file="processed/Fig6/late2cell-3.CAST.sum.auto", sep="\t")
late2cell_4.CAST.auto <- read.table(file="processed/Fig6/late2cell-4.CAST.sum.auto", sep="\t")
late2cell_5.CAST.auto <- read.table(file="processed/Fig6/late2cell-5.CAST.sum.auto", sep="\t")
late2cell_7.CAST.auto <- read.table(file="processed/Fig6/late2cell-7.CAST.sum.auto", sep="\t")
late2cell_9.CAST.auto <- read.table(file="processed/Fig6/late2cell-9.CAST.sum.auto", sep="\t")

#chrX
late2cell_3.chrX <- sum(late2cell_3.REF.chrX[grep("SINE/", late2cell_3.REF.chrX$V7) ,1])
late2cell_4.chrX <- sum(late2cell_4.REF.chrX[grep("SINE/", late2cell_4.REF.chrX$V7) ,1])
late2cell_5.chrX <- sum(late2cell_5.REF.chrX[grep("SINE/", late2cell_5.REF.chrX$V7) ,1])
late2cell_7.chrX <- sum(late2cell_7.REF.chrX[grep("SINE/", late2cell_7.REF.chrX$V7) ,1])
late2cell_9.chrX <- sum(late2cell_9.REF.chrX[grep("SINE/", late2cell_9.REF.chrX$V7) ,1])

#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
late2cell_3.auto <- sum(late2cell_3.REF.auto[grep("SINE/", late2cell_3.REF.auto$V7) ,1])/19 + sum(late2cell_3.CAST.auto[grep("SINE/", late2cell_3.CAST.auto$V7) ,1])/19
late2cell_4.auto <- sum(late2cell_4.REF.auto[grep("SINE/", late2cell_4.REF.auto$V7) ,1])/19 + sum(late2cell_4.CAST.auto[grep("SINE/", late2cell_4.CAST.auto$V7) ,1])/19
late2cell_5.auto <- sum(late2cell_5.REF.auto[grep("SINE/", late2cell_5.REF.auto$V7) ,1])/19 + sum(late2cell_5.CAST.auto[grep("SINE/", late2cell_5.CAST.auto$V7) ,1])/19
late2cell_7.auto <- sum(late2cell_7.REF.auto[grep("SINE/", late2cell_7.REF.auto$V7) ,1])/19 + sum(late2cell_7.CAST.auto[grep("SINE/", late2cell_7.CAST.auto$V7) ,1])/19
late2cell_9.auto <- sum(late2cell_9.REF.auto[grep("SINE/", late2cell_9.REF.auto$V7) ,1])/19 + sum(late2cell_9.CAST.auto[grep("SINE/", late2cell_9.CAST.auto$V7) ,1])/19

late2cell.F.chrX.normalized <- data.frame(data=c(late2cell_3.chrX*1000000/total.F.L2.auto[1], late2cell_4.chrX*1000000/total.F.L2.auto[2], late2cell_5.chrX*1000000/total.F.L2.auto[3], late2cell_7.chrX*1000000/total.F.L2.auto[4], late2cell_9.chrX*1000000/total.F.L2.auto[5]), stage="late2C", chr="ChrX", Gender="Female")
late2cell.F.auto.normalized <- data.frame(data=c(late2cell_3.auto*1000000/total.F.L2.auto[1], late2cell_4.auto*1000000/total.F.L2.auto[2], late2cell_5.auto*1000000/total.F.L2.auto[3], late2cell_7.auto*1000000/total.F.L2.auto[4], late2cell_9.auto*1000000/total.F.L2.auto[5]), stage="late2C",chr="Auto", Gender="Female")







#4C
N4cell_4.REF.chrX <- read.table(file="processed/Fig6/4cell-4.REF.sum.chrX", sep="\t")
N4cell_7.REF.chrX <- read.table(file="processed/Fig6/4cell-7.REF.sum.chrX", sep="\t")
N4cell_12.REF.chrX <- read.table(file="processed/Fig6/4cell-12.REF.sum.chrX", sep="\t")
N4cell_15.REF.chrX <- read.table(file="processed/Fig6/4cell-15.REF.sum.chrX", sep="\t")
N4cell_16.REF.chrX <- read.table(file="processed/Fig6/4cell-16.REF.sum.chrX", sep="\t")
N4cell_17.REF.chrX <- read.table(file="processed/Fig6/4cell-17.REF.sum.chrX", sep="\t")

N4cell_4.REF.auto <- read.table(file="processed/Fig6/4cell-4.REF.sum.auto", sep="\t")
N4cell_7.REF.auto <- read.table(file="processed/Fig6/4cell-7.REF.sum.auto", sep="\t")
N4cell_12.REF.auto <- read.table(file="processed/Fig6/4cell-12.REF.sum.auto", sep="\t")
N4cell_15.REF.auto <- read.table(file="processed/Fig6/4cell-15.REF.sum.auto", sep="\t")
N4cell_16.REF.auto <- read.table(file="processed/Fig6/4cell-16.REF.sum.auto", sep="\t")
N4cell_17.REF.auto <- read.table(file="processed/Fig6/4cell-17.REF.sum.auto", sep="\t")

N4cell_4.CAST.auto <- read.table(file="processed/Fig6/4cell-4.CAST.sum.auto", sep="\t")
N4cell_7.CAST.auto <- read.table(file="processed/Fig6/4cell-7.CAST.sum.auto", sep="\t")
N4cell_12.CAST.auto <- read.table(file="processed/Fig6/4cell-12.CAST.sum.auto", sep="\t")
N4cell_15.CAST.auto <- read.table(file="processed/Fig6/4cell-15.CAST.sum.auto", sep="\t")
N4cell_16.CAST.auto <- read.table(file="processed/Fig6/4cell-16.CAST.sum.auto", sep="\t")
N4cell_17.CAST.auto <- read.table(file="processed/Fig6/4cell-17.CAST.sum.auto", sep="\t")

#chrX
N4cell_4.chrX <- sum(N4cell_4.REF.chrX[grep("SINE/", N4cell_4.REF.chrX$V7) ,1])
N4cell_7.chrX <- sum(N4cell_7.REF.chrX[grep("SINE/", N4cell_7.REF.chrX$V7) ,1])
N4cell_12.chrX <- sum(N4cell_12.REF.chrX[grep("SINE/", N4cell_12.REF.chrX$V7) ,1])
N4cell_15.chrX <- sum(N4cell_15.REF.chrX[grep("SINE/", N4cell_15.REF.chrX$V7) ,1])
N4cell_16.chrX <- sum(N4cell_16.REF.chrX[grep("SINE/", N4cell_16.REF.chrX$V7) ,1])
N4cell_17.chrX <- sum(N4cell_17.REF.chrX[grep("SINE/", N4cell_17.REF.chrX$V7) ,1])

#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
N4cell_4.auto <- sum(N4cell_4.REF.auto[grep("SINE/", N4cell_4.REF.auto$V7) ,1])/19 + sum(N4cell_4.CAST.auto[grep("SINE/", N4cell_4.CAST.auto$V7) ,1])/19
N4cell_7.auto <- sum(N4cell_7.REF.auto[grep("SINE/", N4cell_7.REF.auto$V7) ,1])/19 + sum(N4cell_7.CAST.auto[grep("SINE/", N4cell_7.CAST.auto$V7) ,1])/19
N4cell_12.auto <- sum(N4cell_12.REF.auto[grep("SINE/", N4cell_12.REF.auto$V7) ,1])/19 + sum(N4cell_12.CAST.auto[grep("SINE/", N4cell_12.CAST.auto$V7) ,1])/19
N4cell_15.auto <- sum(N4cell_15.REF.auto[grep("SINE/", N4cell_15.REF.auto$V7) ,1])/19 + sum(N4cell_15.CAST.auto[grep("SINE/", N4cell_15.CAST.auto$V7) ,1])/19
N4cell_16.auto <- sum(N4cell_16.REF.auto[grep("SINE/", N4cell_16.REF.auto$V7) ,1])/19 + sum(N4cell_16.CAST.auto[grep("SINE/", N4cell_16.CAST.auto$V7) ,1])/19
N4cell_17.auto <- sum(N4cell_17.REF.auto[grep("SINE/", N4cell_17.REF.auto$V7) ,1])/19 + sum(N4cell_17.CAST.auto[grep("SINE/", N4cell_17.CAST.auto$V7) ,1])/19


N4cell.F.chrX.normalized <- data.frame(data=c(N4cell_4.chrX*1000000/total.F.N4.auto[1], N4cell_7.chrX*1000000/total.F.N4.auto[2], N4cell_12.chrX*1000000/total.F.N4.auto[3], N4cell_15.chrX*1000000/total.F.N4.auto[4], N4cell_16.chrX*1000000/total.F.N4.auto[5], N4cell_17.chrX*1000000/total.F.N4.auto[6]), stage="4C", chr="ChrX", Gender="Female")
N4cell.F.auto.normalized <- data.frame(data=c(N4cell_4.auto*1000000/total.F.N4.auto[1], N4cell_7.auto*1000000/total.F.N4.auto[2], N4cell_12.auto*1000000/total.F.N4.auto[3], N4cell_15.auto*1000000/total.F.N4.auto[4], N4cell_16.auto*1000000/total.F.N4.auto[5], N4cell_17.auto*1000000/total.F.N4.auto[6]), stage="4C",chr="Auto", Gender="Female")

#8C
#Female
#chrX
N8cell_6.REF.chrX <- read.table(file="processed/Fig6/8cell-6.REF.sum.chrX", sep="\t")
N8cell_10.REF.chrX <- read.table(file="processed/Fig6/8cell-10.REF.sum.chrX", sep="\t")
N8cell_12.REF.chrX <- read.table(file="processed/Fig6/8cell-12.REF.sum.chrX", sep="\t")
N8cell_15.REF.chrX <- read.table(file="processed/Fig6/8cell-15.REF.sum.chrX", sep="\t")
N8cell_19.REF.chrX <- read.table(file="processed/Fig6/8cell-19.REF.sum.chrX", sep="\t")
N8cell_20.REF.chrX <- read.table(file="processed/Fig6/8cell-20.REF.sum.chrX", sep="\t")
#auto
N8cell_6.REF.auto <- read.table(file="processed/Fig6/8cell-6.REF.sum.auto", sep="\t")
N8cell_10.REF.auto <- read.table(file="processed/Fig6/8cell-10.REF.sum.auto", sep="\t")
N8cell_12.REF.auto <- read.table(file="processed/Fig6/8cell-12.REF.sum.auto", sep="\t")
N8cell_15.REF.auto <- read.table(file="processed/Fig6/8cell-15.REF.sum.auto", sep="\t")
N8cell_19.REF.auto <- read.table(file="processed/Fig6/8cell-19.REF.sum.auto", sep="\t")
N8cell_20.REF.auto <- read.table(file="processed/Fig6/8cell-20.REF.sum.auto", sep="\t")

N8cell_6.CAST.auto <- read.table(file="processed/Fig6/8cell-6.CAST.sum.auto", sep="\t")
N8cell_10.CAST.auto <- read.table(file="processed/Fig6/8cell-10.CAST.sum.auto", sep="\t")
N8cell_12.CAST.auto <- read.table(file="processed/Fig6/8cell-12.CAST.sum.auto", sep="\t")
N8cell_15.CAST.auto <- read.table(file="processed/Fig6/8cell-15.CAST.sum.auto", sep="\t")
N8cell_19.CAST.auto <- read.table(file="processed/Fig6/8cell-19.CAST.sum.auto", sep="\t")
N8cell_20.CAST.auto <- read.table(file="processed/Fig6/8cell-20.CAST.sum.auto", sep="\t")

#chrX
N8cell_6.chrX <- sum(N8cell_6.REF.chrX[grep("SINE/", N8cell_6.REF.chrX$V7) ,1])
N8cell_10.chrX <- sum(N8cell_10.REF.chrX[grep("SINE/", N8cell_10.REF.chrX$V7) ,1])
N8cell_12.chrX <- sum(N8cell_12.REF.chrX[grep("SINE/", N8cell_12.REF.chrX$V7) ,1])
N8cell_15.chrX <- sum(N8cell_15.REF.chrX[grep("SINE/", N8cell_15.REF.chrX$V7) ,1])
N8cell_19.chrX <- sum(N8cell_19.REF.chrX[grep("SINE/", N8cell_19.REF.chrX$V7) ,1])
N8cell_20.chrX <- sum(N8cell_20.REF.chrX[grep("SINE/", N8cell_20.REF.chrX$V7) ,1])

#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
N8cell_6.auto <- sum(N8cell_6.REF.auto[grep("SINE/", N8cell_6.REF.auto$V7) ,1])/19 + sum(N8cell_6.CAST.auto[grep("SINE/", N8cell_6.CAST.auto$V7) ,1])/19
N8cell_10.auto <- sum(N8cell_10.REF.auto[grep("SINE/", N8cell_10.REF.auto$V7) ,1])/19 + sum(N8cell_10.CAST.auto[grep("SINE/", N8cell_10.CAST.auto$V7) ,1])/19
N8cell_12.auto <- sum(N8cell_12.REF.auto[grep("SINE/", N8cell_12.REF.auto$V7) ,1])/19 + sum(N8cell_12.CAST.auto[grep("SINE/", N8cell_12.CAST.auto$V7) ,1])/19
N8cell_15.auto <- sum(N8cell_15.REF.auto[grep("SINE/", N8cell_15.REF.auto$V7) ,1])/19 + sum(N8cell_15.CAST.auto[grep("SINE/", N8cell_15.CAST.auto$V7) ,1])/19
N8cell_19.auto <- sum(N8cell_19.REF.auto[grep("SINE/", N8cell_19.REF.auto$V7) ,1])/19 + sum(N8cell_19.CAST.auto[grep("SINE/", N8cell_19.CAST.auto$V7) ,1])/19
N8cell_20.auto <- sum(N8cell_20.REF.auto[grep("SINE/", N8cell_20.REF.auto$V7) ,1])/19 + sum(N8cell_20.CAST.auto[grep("SINE/", N8cell_20.CAST.auto$V7) ,1])/19


N8cell.F.chrX.normalized <- data.frame(data=c(N8cell_6.chrX*1000000/total.F.N8.auto[1], N8cell_10.chrX*1000000/total.F.N8.auto[2], N8cell_12.chrX*1000000/total.F.N8.auto[3], N8cell_15.chrX*1000000/total.F.N8.auto[4], N8cell_19.chrX*1000000/total.F.N8.auto[5], N8cell_20.chrX*1000000/total.F.N8.auto[6]), stage="8C",chr="ChrX", Gender="Female")
N8cell.F.auto.normalized <- data.frame(data=c(N8cell_6.auto*1000000/total.F.N8.auto[1], N8cell_10.auto*1000000/total.F.N8.auto[2], N8cell_12.auto*1000000/total.F.N8.auto[3], N8cell_15.auto*1000000/total.F.N8.auto[4], N8cell_19.auto*1000000/total.F.N8.auto[5], N8cell_20.auto*1000000/total.F.N8.auto[6]), stage="8C", chr="Auto", Gender="Female")


#16C
#Female
#chrX
N16cell_2.REF.chrX <- read.table(file="processed/Fig6/16cell-2.REF.sum.chrX", sep="\t")
N16cell_3.REF.chrX <- read.table(file="processed/Fig6/16cell-3.REF.sum.chrX", sep="\t")
N16cell_5.REF.chrX <- read.table(file="processed/Fig6/16cell-5.REF.sum.chrX", sep="\t")
N16cell_6.REF.chrX <- read.table(file="processed/Fig6/16cell-6.REF.sum.chrX", sep="\t")
N16cell_7.REF.chrX <- read.table(file="processed/Fig6/16cell-7.REF.sum.chrX", sep="\t")
#Auto
N16cell_2.REF.auto <- read.table(file="processed/Fig6/16cell-2.REF.sum.auto", sep="\t")
N16cell_3.REF.auto <- read.table(file="processed/Fig6/16cell-3.REF.sum.auto", sep="\t")
N16cell_5.REF.auto <- read.table(file="processed/Fig6/16cell-5.REF.sum.auto", sep="\t")
N16cell_6.REF.auto <- read.table(file="processed/Fig6/16cell-6.REF.sum.auto", sep="\t")
N16cell_7.REF.auto <- read.table(file="processed/Fig6/16cell-7.REF.sum.auto", sep="\t")

N16cell_2.CAST.auto <- read.table(file="processed/Fig6/16cell-2.CAST.sum.auto", sep="\t")
N16cell_3.CAST.auto <- read.table(file="processed/Fig6/16cell-3.CAST.sum.auto", sep="\t")
N16cell_5.CAST.auto <- read.table(file="processed/Fig6/16cell-5.CAST.sum.auto", sep="\t")
N16cell_6.CAST.auto <- read.table(file="processed/Fig6/16cell-6.CAST.sum.auto", sep="\t")
N16cell_7.CAST.auto <- read.table(file="processed/Fig6/16cell-7.CAST.sum.auto", sep="\t")


#chrX
N16cell_2.chrX <- sum(N16cell_2.REF.chrX[grep("SINE/", N16cell_2.REF.chrX$V7) ,1])
N16cell_3.chrX <- sum(N16cell_3.REF.chrX[grep("SINE/", N16cell_3.REF.chrX$V7) ,1])
N16cell_5.chrX <- sum(N16cell_5.REF.chrX[grep("SINE/", N16cell_5.REF.chrX$V7) ,1])
N16cell_6.chrX <- sum(N16cell_6.REF.chrX[grep("SINE/", N16cell_6.REF.chrX$V7) ,1])
N16cell_7.chrX <- sum(N16cell_7.REF.chrX[grep("SINE/", N16cell_7.REF.chrX$V7) ,1])

#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
N16cell_2.auto <- sum(N16cell_2.REF.auto[grep("SINE/", N16cell_2.REF.auto$V7) ,1])/19 + sum(N16cell_2.CAST.auto[grep("SINE/", N16cell_2.CAST.auto$V7) ,1])/19
N16cell_3.auto <- sum(N16cell_3.REF.auto[grep("SINE/", N16cell_3.REF.auto$V7) ,1])/19 + sum(N16cell_3.CAST.auto[grep("SINE/", N16cell_3.CAST.auto$V7) ,1])/19
N16cell_5.auto <- sum(N16cell_5.REF.auto[grep("SINE/", N16cell_5.REF.auto$V7) ,1])/19 + sum(N16cell_5.CAST.auto[grep("SINE/", N16cell_5.CAST.auto$V7) ,1])/19
N16cell_6.auto <- sum(N16cell_6.REF.auto[grep("SINE/", N16cell_6.REF.auto$V7) ,1])/19 + sum(N16cell_6.CAST.auto[grep("SINE/", N16cell_6.CAST.auto$V7) ,1])/19
N16cell_7.auto <- sum(N16cell_7.REF.auto[grep("SINE/", N16cell_7.REF.auto$V7) ,1])/19 + sum(N16cell_7.CAST.auto[grep("SINE/", N16cell_7.CAST.auto$V7) ,1])/19

N16cell.F.chrX.normalized <- data.frame(data=c(N16cell_2.chrX*1000000/total.F.N16.auto[1], N16cell_3.chrX*1000000/total.F.N16.auto[2], N16cell_5.chrX*1000000/total.F.N16.auto[3], N16cell_6.chrX*1000000/total.F.N16.auto[4], N16cell_7.chrX*1000000/total.F.N16.auto[5]), stage="16C", chr="ChrX", Gender="Female")
N16cell.F.auto.normalized <- data.frame(data=c(N16cell_2.auto*1000000/total.F.N16.auto[1], N16cell_3.auto*1000000/total.F.N16.auto[2], N16cell_5.auto*1000000/total.F.N16.auto[3], N16cell_6.auto*1000000/total.F.N16.auto[4], N16cell_7.auto*1000000/total.F.N16.auto[5]), stage="16C", chr="Auto", Gender="Female")




#eB
#Female
#chrX
eB_1.REF.chrX <- read.table(file="processed/Fig6/earlyBlast-1.REF.sum.chrX", sep="\t")
eB_4.REF.chrX <- read.table(file="processed/Fig6/earlyBlast-4.REF.sum.chrX", sep="\t")
eB_5.REF.chrX <- read.table(file="processed/Fig6/earlyBlast-5.REF.sum.chrX", sep="\t")
eB_8.REF.chrX <- read.table(file="processed/Fig6/earlyBlast-8.REF.sum.chrX", sep="\t")
eB_10.REF.chrX <- read.table(file="processed/Fig6/earlyBlast-10.REF.sum.chrX", sep="\t")
#Auto
eB_1.REF.auto <- read.table(file="processed/Fig6/earlyBlast-1.REF.sum.auto", sep="\t")
eB_4.REF.auto <- read.table(file="processed/Fig6/earlyBlast-4.REF.sum.auto", sep="\t") 
eB_5.REF.auto <- read.table(file="processed/Fig6/earlyBlast-5.REF.sum.auto", sep="\t") 
eB_8.REF.auto <- read.table(file="processed/Fig6/earlyBlast-8.REF.sum.auto", sep="\t") 
eB_10.REF.auto <- read.table(file="processed/Fig6/earlyBlast-10.REF.sum.auto", sep="\t")

eB_1.CAST.auto <- read.table(file="processed/Fig6/earlyBlast-1.CAST.sum.auto", sep="\t")
eB_4.CAST.auto <- read.table(file="processed/Fig6/earlyBlast-4.CAST.sum.auto", sep="\t")
eB_5.CAST.auto <- read.table(file="processed/Fig6/earlyBlast-5.CAST.sum.auto", sep="\t")
eB_8.CAST.auto <- read.table(file="processed/Fig6/earlyBlast-8.CAST.sum.auto", sep="\t")
eB_10.CAST.auto <- read.table(file="processed/Fig6/earlyBlast-10.CAST.sum.auto", sep="\t")

#chrX
eB_1.chrX <- sum(eB_1.REF.chrX[grep("SINE/", eB_1.REF.chrX$V7) ,1])
eB_4.chrX <- sum(eB_4.REF.chrX[grep("SINE/", eB_4.REF.chrX$V7) ,1])
eB_5.chrX <- sum(eB_5.REF.chrX[grep("SINE/", eB_5.REF.chrX$V7) ,1])
eB_8.chrX <- sum(eB_8.REF.chrX[grep("SINE/", eB_8.REF.chrX$V7) ,1])
eB_10.chrX <- sum(eB_10.REF.chrX[grep("SINE/", eB_10.REF.chrX$V7) ,1])

#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
eB_1.auto <- sum(eB_1.REF.auto[grep("SINE/", eB_1.REF.auto$V7) ,1])/19 + sum(eB_1.CAST.auto[grep("SINE/", eB_1.CAST.auto$V7) ,1])/19
eB_4.auto <- sum(eB_4.REF.auto[grep("SINE/", eB_4.REF.auto$V7) ,1])/19 + sum(eB_4.CAST.auto[grep("SINE/", eB_4.CAST.auto$V7) ,1])/19
eB_5.auto <- sum(eB_5.REF.auto[grep("SINE/", eB_5.REF.auto$V7) ,1])/19 + sum(eB_5.CAST.auto[grep("SINE/", eB_5.CAST.auto$V7) ,1])/19
eB_8.auto <- sum(eB_8.REF.auto[grep("SINE/", eB_8.REF.auto$V7) ,1])/19 + sum(eB_8.CAST.auto[grep("SINE/", eB_8.CAST.auto$V7) ,1])/19
eB_10.auto <- sum(eB_10.REF.auto[grep("SINE/", eB_10.REF.auto$V7) ,1])/19 + sum(eB_10.CAST.auto[grep("SINE/", eB_10.CAST.auto$V7) ,1])/19


eB.F.chrX.normalized <- data.frame(data=c(eB_1.chrX*1000000/total.F.eB.auto[1], eB_4.chrX*1000000/total.F.eB.auto[2], eB_5.chrX*1000000/total.F.eB.auto[3], eB_8.chrX*1000000/total.F.eB.auto[4], eB_10.chrX*1000000/total.F.eB.auto[5]), stage="earlyB.", chr="ChrX", Gender="Female")
eB.F.auto.normalized <- data.frame(data=c(eB_1.auto*1000000/total.F.eB.auto[1], eB_4.auto*1000000/total.F.eB.auto[2], eB_5.auto*1000000/total.F.eB.auto[3], eB_8.auto*1000000/total.F.eB.auto[4], eB_10.auto*1000000/total.F.eB.auto[5]), stage="earlyB.", chr="Auto", Gender="Female")

#sum up

F.Mat.chrX.dat <- rbind(late2cell.F.chrX.normalized, N4cell.F.chrX.normalized, N8cell.F.chrX.normalized, N16cell.F.chrX.normalized, eB.F.chrX.normalized)
F.Mat.auto.dat <- rbind(late2cell.F.auto.normalized, N4cell.F.auto.normalized, N8cell.F.auto.normalized, N16cell.F.auto.normalized, eB.F.auto.normalized)
dat <- rbind(F.Mat.chrX.dat, F.Mat.auto.dat)

F.Mat.chrX.dat$data <- F.Mat.chrX.dat$data / median(late2cell.F.chrX.normalized$data)
F.Mat.auto.dat$data <- F.Mat.auto.dat$data / median(late2cell.F.auto.normalized$data)
F.Mat.dat <- rbind(F.Mat.chrX.dat, F.Mat.auto.dat)

F.Mat.dat$stage <- factor(F.Mat.dat$stage, level=c("late2C","4C","8C","16C","earlyB."))


ggplot(F.Mat.dat, aes(x=stage, y=data, fill=chr)) +
  geom_boxplot(outlier.shape=NA,alpha =0.9) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
             aes(fill=chr), size=1.3, pch=21,stroke = 0.4) +
  scale_fill_manual(values=c("#FF3300","burlywood1")) +
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



#p-value, t-test
#4C
N4C.X <- F.Mat.dat[which(F.Mat.dat$stage=="4C"& F.Mat.dat$chr=="ChrX"), 1]
N4C.Auto <- F.Mat.dat[which(F.Mat.dat$stage=="4C"& F.Mat.dat$chr=="Auto"), 1]
t.test(x=N4C.X, y=N4C.Auto, paired=T)
#8C
N8C.X <- F.Mat.dat[which(F.Mat.dat$stage=="8C"& F.Mat.dat$chr=="ChrX"), 1]
N8C.Auto <- F.Mat.dat[which(F.Mat.dat$stage=="8C"& F.Mat.dat$chr=="Auto"), 1]
t.test(x=N8C.X, y=N8C.Auto, paired=T)
#16C
N16C.X <- F.Mat.dat[which(F.Mat.dat$stage=="16C"& F.Mat.dat$chr=="ChrX"), 1]
N16C.Auto <- F.Mat.dat[which(F.Mat.dat$stage=="16C"& F.Mat.dat$chr=="Auto"), 1]
t.test(x=N16C.X, y=N16C.Auto)
#earlyB
eB.X <- F.Mat.dat[which(F.Mat.dat$stage=="earlyB."& F.Mat.dat$chr=="ChrX"), 1]
eB.Auto <- F.Mat.dat[which(F.Mat.dat$stage=="earlyB."& F.Mat.dat$chr=="Auto"), 1]
t.test(x=eB.X, y=eB.Auto,paired=T)


#---------------------------
#---------e.  LTRs. ----------
#---------------------------


#late2C
#chrX
late2cell_3.chrX <- sum(late2cell_3.REF.chrX[grep("LTR/", late2cell_3.REF.chrX$V7) ,1])
late2cell_4.chrX <- sum(late2cell_4.REF.chrX[grep("LTR/", late2cell_4.REF.chrX$V7) ,1])
late2cell_5.chrX <- sum(late2cell_5.REF.chrX[grep("LTR/", late2cell_5.REF.chrX$V7) ,1])
late2cell_7.chrX <- sum(late2cell_7.REF.chrX[grep("LTR/", late2cell_7.REF.chrX$V7) ,1])
late2cell_9.chrX <- sum(late2cell_9.REF.chrX[grep("LTR/", late2cell_9.REF.chrX$V7) ,1])

#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
late2cell_3.auto <- sum(late2cell_3.REF.auto[grep("LTR/", late2cell_3.REF.auto$V7) ,1])/19 + sum(late2cell_3.CAST.auto[grep("LTR/", late2cell_3.CAST.auto$V7) ,1])/19
late2cell_4.auto <- sum(late2cell_4.REF.auto[grep("LTR/", late2cell_4.REF.auto$V7) ,1])/19 + sum(late2cell_4.CAST.auto[grep("LTR/", late2cell_4.CAST.auto$V7) ,1])/19
late2cell_5.auto <- sum(late2cell_5.REF.auto[grep("LTR/", late2cell_5.REF.auto$V7) ,1])/19 + sum(late2cell_5.CAST.auto[grep("LTR/", late2cell_5.CAST.auto$V7) ,1])/19
late2cell_7.auto <- sum(late2cell_7.REF.auto[grep("LTR/", late2cell_7.REF.auto$V7) ,1])/19 + sum(late2cell_7.CAST.auto[grep("LTR/", late2cell_7.CAST.auto$V7) ,1])/19
late2cell_9.auto <- sum(late2cell_9.REF.auto[grep("LTR/", late2cell_9.REF.auto$V7) ,1])/19 + sum(late2cell_9.CAST.auto[grep("LTR/", late2cell_9.CAST.auto$V7) ,1])/19

late2cell.F.chrX.normalized <- data.frame(data=c(late2cell_3.chrX*1000000/total.F.L2.auto[1], late2cell_4.chrX*1000000/total.F.L2.auto[2], late2cell_5.chrX*1000000/total.F.L2.auto[3], late2cell_7.chrX*1000000/total.F.L2.auto[4], late2cell_9.chrX*1000000/total.F.L2.auto[5]), stage="late2C", chr="ChrX", Gender="Female")
late2cell.F.auto.normalized <- data.frame(data=c(late2cell_3.auto*1000000/total.F.L2.auto[1], late2cell_4.auto*1000000/total.F.L2.auto[2], late2cell_5.auto*1000000/total.F.L2.auto[3], late2cell_7.auto*1000000/total.F.L2.auto[4], late2cell_9.auto*1000000/total.F.L2.auto[5]), stage="late2C",chr="Auto", Gender="Female")



#4C
#chrX
N4cell_4.chrX <- sum(N4cell_4.REF.chrX[grep("LTR/", N4cell_4.REF.chrX$V7) ,1])
N4cell_7.chrX <- sum(N4cell_7.REF.chrX[grep("LTR/", N4cell_7.REF.chrX$V7) ,1])
N4cell_12.chrX <- sum(N4cell_12.REF.chrX[grep("LTR/", N4cell_12.REF.chrX$V7) ,1])
N4cell_15.chrX <- sum(N4cell_15.REF.chrX[grep("LTR/", N4cell_15.REF.chrX$V7) ,1])
N4cell_16.chrX <- sum(N4cell_16.REF.chrX[grep("LTR/", N4cell_16.REF.chrX$V7) ,1])
N4cell_17.chrX <- sum(N4cell_17.REF.chrX[grep("LTR/", N4cell_17.REF.chrX$V7) ,1])

#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
N4cell_4.auto <- sum(N4cell_4.REF.auto[grep("LTR/", N4cell_4.REF.auto$V7) ,1])/19 + sum(N4cell_4.CAST.auto[grep("LTR/", N4cell_4.CAST.auto$V7) ,1])/19
N4cell_7.auto <- sum(N4cell_7.REF.auto[grep("LTR/", N4cell_7.REF.auto$V7) ,1])/19 + sum(N4cell_7.CAST.auto[grep("LTR/", N4cell_7.CAST.auto$V7) ,1])/19
N4cell_12.auto <- sum(N4cell_12.REF.auto[grep("LTR/", N4cell_12.REF.auto$V7) ,1])/19 + sum(N4cell_12.CAST.auto[grep("LTR/", N4cell_12.CAST.auto$V7) ,1])/19
N4cell_15.auto <- sum(N4cell_15.REF.auto[grep("LTR/", N4cell_15.REF.auto$V7) ,1])/19 + sum(N4cell_15.CAST.auto[grep("LTR/", N4cell_15.CAST.auto$V7) ,1])/19
N4cell_16.auto <- sum(N4cell_16.REF.auto[grep("LTR/", N4cell_16.REF.auto$V7) ,1])/19 + sum(N4cell_16.CAST.auto[grep("LTR/", N4cell_16.CAST.auto$V7) ,1])/19
N4cell_17.auto <- sum(N4cell_17.REF.auto[grep("LTR/", N4cell_17.REF.auto$V7) ,1])/19 + sum(N4cell_17.CAST.auto[grep("LTR/", N4cell_17.CAST.auto$V7) ,1])/19


N4cell.F.chrX.normalized <- data.frame(data=c(N4cell_4.chrX*1000000/total.F.N4.auto[1], N4cell_7.chrX*1000000/total.F.N4.auto[2], N4cell_12.chrX*1000000/total.F.N4.auto[3], N4cell_15.chrX*1000000/total.F.N4.auto[4], N4cell_16.chrX*1000000/total.F.N4.auto[5], N4cell_17.chrX*1000000/total.F.N4.auto[6]), stage="4C", chr="ChrX", Gender="Female")
N4cell.F.auto.normalized <- data.frame(data=c(N4cell_4.auto*1000000/total.F.N4.auto[1], N4cell_7.auto*1000000/total.F.N4.auto[2], N4cell_12.auto*1000000/total.F.N4.auto[3], N4cell_15.auto*1000000/total.F.N4.auto[4], N4cell_16.auto*1000000/total.F.N4.auto[5], N4cell_17.auto*1000000/total.F.N4.auto[6]), stage="4C",chr="Auto", Gender="Female")

#8C
#chrX
N8cell_6.chrX <- sum(N8cell_6.REF.chrX[grep("LTR/", N8cell_6.REF.chrX$V7) ,1])
N8cell_10.chrX <- sum(N8cell_10.REF.chrX[grep("LTR/", N8cell_10.REF.chrX$V7) ,1])
N8cell_12.chrX <- sum(N8cell_12.REF.chrX[grep("LTR/", N8cell_12.REF.chrX$V7) ,1])
N8cell_15.chrX <- sum(N8cell_15.REF.chrX[grep("LTR/", N8cell_15.REF.chrX$V7) ,1])
N8cell_19.chrX <- sum(N8cell_19.REF.chrX[grep("LTR/", N8cell_19.REF.chrX$V7) ,1])
N8cell_20.chrX <- sum(N8cell_20.REF.chrX[grep("LTR/", N8cell_20.REF.chrX$V7) ,1])

#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
N8cell_6.auto <- sum(N8cell_6.REF.auto[grep("LTR/", N8cell_6.REF.auto$V7) ,1])/19 + sum(N8cell_6.CAST.auto[grep("LTR/", N8cell_6.CAST.auto$V7) ,1])/19
N8cell_10.auto <- sum(N8cell_10.REF.auto[grep("LTR/", N8cell_10.REF.auto$V7) ,1])/19 + sum(N8cell_10.CAST.auto[grep("LTR/", N8cell_10.CAST.auto$V7) ,1])/19
N8cell_12.auto <- sum(N8cell_12.REF.auto[grep("LTR/", N8cell_12.REF.auto$V7) ,1])/19 + sum(N8cell_12.CAST.auto[grep("LTR/", N8cell_12.CAST.auto$V7) ,1])/19
N8cell_15.auto <- sum(N8cell_15.REF.auto[grep("LTR/", N8cell_15.REF.auto$V7) ,1])/19 + sum(N8cell_15.CAST.auto[grep("LTR/", N8cell_15.CAST.auto$V7) ,1])/19
N8cell_19.auto <- sum(N8cell_19.REF.auto[grep("LTR/", N8cell_19.REF.auto$V7) ,1])/19 + sum(N8cell_19.CAST.auto[grep("LTR/", N8cell_19.CAST.auto$V7) ,1])/19
N8cell_20.auto <- sum(N8cell_20.REF.auto[grep("LTR/", N8cell_20.REF.auto$V7) ,1])/19 + sum(N8cell_20.CAST.auto[grep("LTR/", N8cell_20.CAST.auto$V7) ,1])/19


N8cell.F.chrX.normalized <- data.frame(data=c(N8cell_6.chrX*1000000/total.F.N8.auto[1], N8cell_10.chrX*1000000/total.F.N8.auto[2], N8cell_12.chrX*1000000/total.F.N8.auto[3], N8cell_15.chrX*1000000/total.F.N8.auto[4], N8cell_19.chrX*1000000/total.F.N8.auto[5], N8cell_20.chrX*1000000/total.F.N8.auto[6]), stage="8C",chr="ChrX", Gender="Female")
N8cell.F.auto.normalized <- data.frame(data=c(N8cell_6.auto*1000000/total.F.N8.auto[1], N8cell_10.auto*1000000/total.F.N8.auto[2], N8cell_12.auto*1000000/total.F.N8.auto[3], N8cell_15.auto*1000000/total.F.N8.auto[4], N8cell_19.auto*1000000/total.F.N8.auto[5], N8cell_20.auto*1000000/total.F.N8.auto[6]), stage="8C", chr="Auto", Gender="Female")


#16C
#chrX
N16cell_2.chrX <- sum(N16cell_2.REF.chrX[grep("LTR/", N16cell_2.REF.chrX$V7) ,1])
N16cell_3.chrX <- sum(N16cell_3.REF.chrX[grep("LTR/", N16cell_3.REF.chrX$V7) ,1])
N16cell_5.chrX <- sum(N16cell_5.REF.chrX[grep("LTR/", N16cell_5.REF.chrX$V7) ,1])
N16cell_6.chrX <- sum(N16cell_6.REF.chrX[grep("LTR/", N16cell_6.REF.chrX$V7) ,1])
N16cell_7.chrX <- sum(N16cell_7.REF.chrX[grep("LTR/", N16cell_7.REF.chrX$V7) ,1])

#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
N16cell_2.auto <- sum(N16cell_2.REF.auto[grep("LTR/", N16cell_2.REF.auto$V7) ,1])/19 + sum(N16cell_2.CAST.auto[grep("LTR/", N16cell_2.CAST.auto$V7) ,1])/19
N16cell_3.auto <- sum(N16cell_3.REF.auto[grep("LTR/", N16cell_3.REF.auto$V7) ,1])/19 + sum(N16cell_3.CAST.auto[grep("LTR/", N16cell_3.CAST.auto$V7) ,1])/19
N16cell_5.auto <- sum(N16cell_5.REF.auto[grep("LTR/", N16cell_5.REF.auto$V7) ,1])/19 + sum(N16cell_5.CAST.auto[grep("LTR/", N16cell_5.CAST.auto$V7) ,1])/19
N16cell_6.auto <- sum(N16cell_6.REF.auto[grep("LTR/", N16cell_6.REF.auto$V7) ,1])/19 + sum(N16cell_6.CAST.auto[grep("LTR/", N16cell_6.CAST.auto$V7) ,1])/19
N16cell_7.auto <- sum(N16cell_7.REF.auto[grep("LTR/", N16cell_7.REF.auto$V7) ,1])/19 + sum(N16cell_7.CAST.auto[grep("LTR/", N16cell_7.CAST.auto$V7) ,1])/19

N16cell.F.chrX.normalized <- data.frame(data=c(N16cell_2.chrX*1000000/total.F.N16.auto[1], N16cell_3.chrX*1000000/total.F.N16.auto[2], N16cell_5.chrX*1000000/total.F.N16.auto[3], N16cell_6.chrX*1000000/total.F.N16.auto[4], N16cell_7.chrX*1000000/total.F.N16.auto[5]), stage="16C", chr="ChrX", Gender="Female")
N16cell.F.auto.normalized <- data.frame(data=c(N16cell_2.auto*1000000/total.F.N16.auto[1], N16cell_3.auto*1000000/total.F.N16.auto[2], N16cell_5.auto*1000000/total.F.N16.auto[3], N16cell_6.auto*1000000/total.F.N16.auto[4], N16cell_7.auto*1000000/total.F.N16.auto[5]), stage="16C", chr="Auto", Gender="Female")

#eB
#chrX
eB_1.chrX <- sum(eB_1.REF.chrX[grep("LTR/", eB_1.REF.chrX$V7) ,1])
eB_4.chrX <- sum(eB_4.REF.chrX[grep("LTR/", eB_4.REF.chrX$V7) ,1])
eB_5.chrX <- sum(eB_5.REF.chrX[grep("LTR/", eB_5.REF.chrX$V7) ,1])
eB_8.chrX <- sum(eB_8.REF.chrX[grep("LTR/", eB_8.REF.chrX$V7) ,1])
eB_10.chrX <- sum(eB_10.REF.chrX[grep("LTR/", eB_10.REF.chrX$V7) ,1])

#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
eB_1.auto <- sum(eB_1.REF.auto[grep("LTR/", eB_1.REF.auto$V7) ,1])/19 + sum(eB_1.CAST.auto[grep("LTR/", eB_1.CAST.auto$V7) ,1])/19
eB_4.auto <- sum(eB_4.REF.auto[grep("LTR/", eB_4.REF.auto$V7) ,1])/19 + sum(eB_4.CAST.auto[grep("LTR/", eB_4.CAST.auto$V7) ,1])/19
eB_5.auto <- sum(eB_5.REF.auto[grep("LTR/", eB_5.REF.auto$V7) ,1])/19 + sum(eB_5.CAST.auto[grep("LTR/", eB_5.CAST.auto$V7) ,1])/19
eB_8.auto <- sum(eB_8.REF.auto[grep("LTR/", eB_8.REF.auto$V7) ,1])/19 + sum(eB_8.CAST.auto[grep("LTR/", eB_8.CAST.auto$V7) ,1])/19
eB_10.auto <- sum(eB_10.REF.auto[grep("LTR/", eB_10.REF.auto$V7) ,1])/19 + sum(eB_10.CAST.auto[grep("LTR/", eB_10.CAST.auto$V7) ,1])/19


eB.F.chrX.normalized <- data.frame(data=c(eB_1.chrX*1000000/total.F.eB.auto[1], eB_4.chrX*1000000/total.F.eB.auto[2], eB_5.chrX*1000000/total.F.eB.auto[3], eB_8.chrX*1000000/total.F.eB.auto[4], eB_10.chrX*1000000/total.F.eB.auto[5]), stage="earlyB.", chr="ChrX", Gender="Female")
eB.F.auto.normalized <- data.frame(data=c(eB_1.auto*1000000/total.F.eB.auto[1], eB_4.auto*1000000/total.F.eB.auto[2], eB_5.auto*1000000/total.F.eB.auto[3], eB_8.auto*1000000/total.F.eB.auto[4], eB_10.auto*1000000/total.F.eB.auto[5]), stage="earlyB.", chr="Auto", Gender="Female")

#sum up

F.Mat.chrX.dat <- rbind(late2cell.F.chrX.normalized, N4cell.F.chrX.normalized, N8cell.F.chrX.normalized, N16cell.F.chrX.normalized, eB.F.chrX.normalized)
F.Mat.auto.dat <- rbind(late2cell.F.auto.normalized, N4cell.F.auto.normalized, N8cell.F.auto.normalized, N16cell.F.auto.normalized, eB.F.auto.normalized)
dat <- rbind(F.Mat.chrX.dat, F.Mat.auto.dat)

F.Mat.chrX.dat$data <- F.Mat.chrX.dat$data / median(late2cell.F.chrX.normalized$data)
F.Mat.auto.dat$data <- F.Mat.auto.dat$data / median(late2cell.F.auto.normalized$data)
F.Mat.dat <- rbind(F.Mat.chrX.dat, F.Mat.auto.dat)

F.Mat.dat$stage <- factor(F.Mat.dat$stage, level=c("late2C","4C","8C","16C","earlyB."))



ggplot(F.Mat.dat, aes(x=stage, y=data, fill=chr)) +
  geom_boxplot(outlier.shape=NA,alpha =0.9) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
             aes(fill=chr), size=1.3, pch=21,stroke = 0.4) +
  scale_fill_manual(values=c("#FF3300","darkgrey")) +
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



#p-value, t-test
#4C p=0.5694
N4C.X <- F.Mat.dat[which(F.Mat.dat$stage=="4C"& F.Mat.dat$chr=="ChrX"), 1]
N4C.Auto <- F.Mat.dat[which(F.Mat.dat$stage=="4C"& F.Mat.dat$chr=="Auto"), 1]
t.test(x=N4C.X, y=N4C.Auto, paired=T)
#8C, p=0.0006073
N8C.X <- F.Mat.dat[which(F.Mat.dat$stage=="8C"& F.Mat.dat$chr=="ChrX"), 1]
N8C.Auto <- F.Mat.dat[which(F.Mat.dat$stage=="8C"& F.Mat.dat$chr=="Auto"), 1]
t.test(x=N8C.X, y=N8C.Auto, paired=T)
#16C p=0.0001498
N16C.X <- F.Mat.dat[which(F.Mat.dat$stage=="16C"& F.Mat.dat$chr=="ChrX"), 1]
N16C.Auto <- F.Mat.dat[which(F.Mat.dat$stage=="16C"& F.Mat.dat$chr=="Auto"), 1]
t.test(x=N16C.X, y=N16C.Auto)
#earlyB p=0.02327
eB.X <- F.Mat.dat[which(F.Mat.dat$stage=="earlyB."& F.Mat.dat$chr=="ChrX"), 1]
eB.Auto <- F.Mat.dat[which(F.Mat.dat$stage=="earlyB."& F.Mat.dat$chr=="Auto"), 1]
t.test(x=eB.X, y=eB.Auto,paired=T)


#---------------------------
#---------f.  LINEs. ----------
#---------------------------

#late2C
#chrX
late2cell_3.chrX <- sum(late2cell_3.REF.chrX[grep("LINE/", late2cell_3.REF.chrX$V7) ,1])
late2cell_4.chrX <- sum(late2cell_4.REF.chrX[grep("LINE/", late2cell_4.REF.chrX$V7) ,1])
late2cell_5.chrX <- sum(late2cell_5.REF.chrX[grep("LINE/", late2cell_5.REF.chrX$V7) ,1])
late2cell_7.chrX <- sum(late2cell_7.REF.chrX[grep("LINE/", late2cell_7.REF.chrX$V7) ,1])
late2cell_9.chrX <- sum(late2cell_9.REF.chrX[grep("LINE/", late2cell_9.REF.chrX$V7) ,1])

#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
late2cell_3.auto <- sum(late2cell_3.REF.auto[grep("LINE/", late2cell_3.REF.auto$V7) ,1])/19 + sum(late2cell_3.CAST.auto[grep("LINE/", late2cell_3.CAST.auto$V7) ,1])/19
late2cell_4.auto <- sum(late2cell_4.REF.auto[grep("LINE/", late2cell_4.REF.auto$V7) ,1])/19 + sum(late2cell_4.CAST.auto[grep("LINE/", late2cell_4.CAST.auto$V7) ,1])/19
late2cell_5.auto <- sum(late2cell_5.REF.auto[grep("LINE/", late2cell_5.REF.auto$V7) ,1])/19 + sum(late2cell_5.CAST.auto[grep("LINE/", late2cell_5.CAST.auto$V7) ,1])/19
late2cell_7.auto <- sum(late2cell_7.REF.auto[grep("LINE/", late2cell_7.REF.auto$V7) ,1])/19 + sum(late2cell_7.CAST.auto[grep("LINE/", late2cell_7.CAST.auto$V7) ,1])/19
late2cell_9.auto <- sum(late2cell_9.REF.auto[grep("LINE/", late2cell_9.REF.auto$V7) ,1])/19 + sum(late2cell_9.CAST.auto[grep("LINE/", late2cell_9.CAST.auto$V7) ,1])/19

late2cell.F.chrX.normalized <- data.frame(data=c(late2cell_3.chrX*1000000/total.F.L2.auto[1], late2cell_4.chrX*1000000/total.F.L2.auto[2], late2cell_5.chrX*1000000/total.F.L2.auto[3], late2cell_7.chrX*1000000/total.F.L2.auto[4], late2cell_9.chrX*1000000/total.F.L2.auto[5]), stage="late2C", chr="ChrX", Gender="Female")
late2cell.F.auto.normalized <- data.frame(data=c(late2cell_3.auto*1000000/total.F.L2.auto[1], late2cell_4.auto*1000000/total.F.L2.auto[2], late2cell_5.auto*1000000/total.F.L2.auto[3], late2cell_7.auto*1000000/total.F.L2.auto[4], late2cell_9.auto*1000000/total.F.L2.auto[5]), stage="late2C",chr="Auto", Gender="Female")



#4C
#chrX
N4cell_4.chrX <- sum(N4cell_4.REF.chrX[grep("LINE/", N4cell_4.REF.chrX$V7) ,1])
N4cell_7.chrX <- sum(N4cell_7.REF.chrX[grep("LINE/", N4cell_7.REF.chrX$V7) ,1])
N4cell_12.chrX <- sum(N4cell_12.REF.chrX[grep("LINE/", N4cell_12.REF.chrX$V7) ,1])
N4cell_15.chrX <- sum(N4cell_15.REF.chrX[grep("LINE/", N4cell_15.REF.chrX$V7) ,1])
N4cell_16.chrX <- sum(N4cell_16.REF.chrX[grep("LINE/", N4cell_16.REF.chrX$V7) ,1])
N4cell_17.chrX <- sum(N4cell_17.REF.chrX[grep("LINE/", N4cell_17.REF.chrX$V7) ,1])

#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
N4cell_4.auto <- sum(N4cell_4.REF.auto[grep("LINE/", N4cell_4.REF.auto$V7) ,1])/19 + sum(N4cell_4.CAST.auto[grep("LINE/", N4cell_4.CAST.auto$V7) ,1])/19
N4cell_7.auto <- sum(N4cell_7.REF.auto[grep("LINE/", N4cell_7.REF.auto$V7) ,1])/19 + sum(N4cell_7.CAST.auto[grep("LINE/", N4cell_7.CAST.auto$V7) ,1])/19
N4cell_12.auto <- sum(N4cell_12.REF.auto[grep("LINE/", N4cell_12.REF.auto$V7) ,1])/19 + sum(N4cell_12.CAST.auto[grep("LINE/", N4cell_12.CAST.auto$V7) ,1])/19
N4cell_15.auto <- sum(N4cell_15.REF.auto[grep("LINE/", N4cell_15.REF.auto$V7) ,1])/19 + sum(N4cell_15.CAST.auto[grep("LINE/", N4cell_15.CAST.auto$V7) ,1])/19
N4cell_16.auto <- sum(N4cell_16.REF.auto[grep("LINE/", N4cell_16.REF.auto$V7) ,1])/19 + sum(N4cell_16.CAST.auto[grep("LINE/", N4cell_16.CAST.auto$V7) ,1])/19
N4cell_17.auto <- sum(N4cell_17.REF.auto[grep("LINE/", N4cell_17.REF.auto$V7) ,1])/19 + sum(N4cell_17.CAST.auto[grep("LINE/", N4cell_17.CAST.auto$V7) ,1])/19


N4cell.F.chrX.normalized <- data.frame(data=c(N4cell_4.chrX*1000000/total.F.N4.auto[1], N4cell_7.chrX*1000000/total.F.N4.auto[2], N4cell_12.chrX*1000000/total.F.N4.auto[3], N4cell_15.chrX*1000000/total.F.N4.auto[4], N4cell_16.chrX*1000000/total.F.N4.auto[5], N4cell_17.chrX*1000000/total.F.N4.auto[6]), stage="4C", chr="ChrX", Gender="Female")
N4cell.F.auto.normalized <- data.frame(data=c(N4cell_4.auto*1000000/total.F.N4.auto[1], N4cell_7.auto*1000000/total.F.N4.auto[2], N4cell_12.auto*1000000/total.F.N4.auto[3], N4cell_15.auto*1000000/total.F.N4.auto[4], N4cell_16.auto*1000000/total.F.N4.auto[5], N4cell_17.auto*1000000/total.F.N4.auto[6]), stage="4C",chr="Auto", Gender="Female")

#8C
#chrX
N8cell_6.chrX <- sum(N8cell_6.REF.chrX[grep("LINE/", N8cell_6.REF.chrX$V7) ,1])
N8cell_10.chrX <- sum(N8cell_10.REF.chrX[grep("LINE/", N8cell_10.REF.chrX$V7) ,1])
N8cell_12.chrX <- sum(N8cell_12.REF.chrX[grep("LINE/", N8cell_12.REF.chrX$V7) ,1])
N8cell_15.chrX <- sum(N8cell_15.REF.chrX[grep("LINE/", N8cell_15.REF.chrX$V7) ,1])
N8cell_19.chrX <- sum(N8cell_19.REF.chrX[grep("LINE/", N8cell_19.REF.chrX$V7) ,1])
N8cell_20.chrX <- sum(N8cell_20.REF.chrX[grep("LINE/", N8cell_20.REF.chrX$V7) ,1])

#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
N8cell_6.auto <- sum(N8cell_6.REF.auto[grep("LINE/", N8cell_6.REF.auto$V7) ,1])/19 + sum(N8cell_6.CAST.auto[grep("LINE/", N8cell_6.CAST.auto$V7) ,1])/19
N8cell_10.auto <- sum(N8cell_10.REF.auto[grep("LINE/", N8cell_10.REF.auto$V7) ,1])/19 + sum(N8cell_10.CAST.auto[grep("LINE/", N8cell_10.CAST.auto$V7) ,1])/19
N8cell_12.auto <- sum(N8cell_12.REF.auto[grep("LINE/", N8cell_12.REF.auto$V7) ,1])/19 + sum(N8cell_12.CAST.auto[grep("LINE/", N8cell_12.CAST.auto$V7) ,1])/19
N8cell_15.auto <- sum(N8cell_15.REF.auto[grep("LINE/", N8cell_15.REF.auto$V7) ,1])/19 + sum(N8cell_15.CAST.auto[grep("LINE/", N8cell_15.CAST.auto$V7) ,1])/19
N8cell_19.auto <- sum(N8cell_19.REF.auto[grep("LINE/", N8cell_19.REF.auto$V7) ,1])/19 + sum(N8cell_19.CAST.auto[grep("LINE/", N8cell_19.CAST.auto$V7) ,1])/19
N8cell_20.auto <- sum(N8cell_20.REF.auto[grep("LINE/", N8cell_20.REF.auto$V7) ,1])/19 + sum(N8cell_20.CAST.auto[grep("LINE/", N8cell_20.CAST.auto$V7) ,1])/19


N8cell.F.chrX.normalized <- data.frame(data=c(N8cell_6.chrX*1000000/total.F.N8.auto[1], N8cell_10.chrX*1000000/total.F.N8.auto[2], N8cell_12.chrX*1000000/total.F.N8.auto[3], N8cell_15.chrX*1000000/total.F.N8.auto[4], N8cell_19.chrX*1000000/total.F.N8.auto[5], N8cell_20.chrX*1000000/total.F.N8.auto[6]), stage="8C",chr="ChrX", Gender="Female")
N8cell.F.auto.normalized <- data.frame(data=c(N8cell_6.auto*1000000/total.F.N8.auto[1], N8cell_10.auto*1000000/total.F.N8.auto[2], N8cell_12.auto*1000000/total.F.N8.auto[3], N8cell_15.auto*1000000/total.F.N8.auto[4], N8cell_19.auto*1000000/total.F.N8.auto[5], N8cell_20.auto*1000000/total.F.N8.auto[6]), stage="8C", chr="Auto", Gender="Female")


#16C
#chrX
N16cell_2.chrX <- sum(N16cell_2.REF.chrX[grep("LINE/", N16cell_2.REF.chrX$V7) ,1])
N16cell_3.chrX <- sum(N16cell_3.REF.chrX[grep("LINE/", N16cell_3.REF.chrX$V7) ,1])
N16cell_5.chrX <- sum(N16cell_5.REF.chrX[grep("LINE/", N16cell_5.REF.chrX$V7) ,1])
N16cell_6.chrX <- sum(N16cell_6.REF.chrX[grep("LINE/", N16cell_6.REF.chrX$V7) ,1])
N16cell_7.chrX <- sum(N16cell_7.REF.chrX[grep("LINE/", N16cell_7.REF.chrX$V7) ,1])

#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
N16cell_2.auto <- sum(N16cell_2.REF.auto[grep("LINE/", N16cell_2.REF.auto$V7) ,1])/19 + sum(N16cell_2.CAST.auto[grep("LINE/", N16cell_2.CAST.auto$V7) ,1])/19
N16cell_3.auto <- sum(N16cell_3.REF.auto[grep("LINE/", N16cell_3.REF.auto$V7) ,1])/19 + sum(N16cell_3.CAST.auto[grep("LINE/", N16cell_3.CAST.auto$V7) ,1])/19
N16cell_5.auto <- sum(N16cell_5.REF.auto[grep("LINE/", N16cell_5.REF.auto$V7) ,1])/19 + sum(N16cell_5.CAST.auto[grep("LINE/", N16cell_5.CAST.auto$V7) ,1])/19
N16cell_6.auto <- sum(N16cell_6.REF.auto[grep("LINE/", N16cell_6.REF.auto$V7) ,1])/19 + sum(N16cell_6.CAST.auto[grep("LINE/", N16cell_6.CAST.auto$V7) ,1])/19
N16cell_7.auto <- sum(N16cell_7.REF.auto[grep("LINE/", N16cell_7.REF.auto$V7) ,1])/19 + sum(N16cell_7.CAST.auto[grep("LINE/", N16cell_7.CAST.auto$V7) ,1])/19

N16cell.F.chrX.normalized <- data.frame(data=c(N16cell_2.chrX*1000000/total.F.N16.auto[1], N16cell_3.chrX*1000000/total.F.N16.auto[2], N16cell_5.chrX*1000000/total.F.N16.auto[3], N16cell_6.chrX*1000000/total.F.N16.auto[4], N16cell_7.chrX*1000000/total.F.N16.auto[5]), stage="16C", chr="ChrX", Gender="Female")
N16cell.F.auto.normalized <- data.frame(data=c(N16cell_2.auto*1000000/total.F.N16.auto[1], N16cell_3.auto*1000000/total.F.N16.auto[2], N16cell_5.auto*1000000/total.F.N16.auto[3], N16cell_6.auto*1000000/total.F.N16.auto[4], N16cell_7.auto*1000000/total.F.N16.auto[5]), stage="16C", chr="Auto", Gender="Female")



#eB
#chrX
eB_1.chrX <- sum(eB_1.REF.chrX[grep("LINE/", eB_1.REF.chrX$V7) ,1])
eB_4.chrX <- sum(eB_4.REF.chrX[grep("LINE/", eB_4.REF.chrX$V7) ,1])
eB_5.chrX <- sum(eB_5.REF.chrX[grep("LINE/", eB_5.REF.chrX$V7) ,1])
eB_8.chrX <- sum(eB_8.REF.chrX[grep("LINE/", eB_8.REF.chrX$V7) ,1])
eB_10.chrX <- sum(eB_10.REF.chrX[grep("LINE/", eB_10.REF.chrX$V7) ,1])

#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
eB_1.auto <- sum(eB_1.REF.auto[grep("LINE/", eB_1.REF.auto$V7) ,1])/19 + sum(eB_1.CAST.auto[grep("LINE/", eB_1.CAST.auto$V7) ,1])/19
eB_4.auto <- sum(eB_4.REF.auto[grep("LINE/", eB_4.REF.auto$V7) ,1])/19 + sum(eB_4.CAST.auto[grep("LINE/", eB_4.CAST.auto$V7) ,1])/19
eB_5.auto <- sum(eB_5.REF.auto[grep("LINE/", eB_5.REF.auto$V7) ,1])/19 + sum(eB_5.CAST.auto[grep("LINE/", eB_5.CAST.auto$V7) ,1])/19
eB_8.auto <- sum(eB_8.REF.auto[grep("LINE/", eB_8.REF.auto$V7) ,1])/19 + sum(eB_8.CAST.auto[grep("LINE/", eB_8.CAST.auto$V7) ,1])/19
eB_10.auto <- sum(eB_10.REF.auto[grep("LINE/", eB_10.REF.auto$V7) ,1])/19 + sum(eB_10.CAST.auto[grep("LINE/", eB_10.CAST.auto$V7) ,1])/19


eB.F.chrX.normalized <- data.frame(data=c(eB_1.chrX*1000000/total.F.eB.auto[1], eB_4.chrX*1000000/total.F.eB.auto[2], eB_5.chrX*1000000/total.F.eB.auto[3], eB_8.chrX*1000000/total.F.eB.auto[4], eB_10.chrX*1000000/total.F.eB.auto[5]), stage="earlyB.", chr="ChrX", Gender="Female")
eB.F.auto.normalized <- data.frame(data=c(eB_1.auto*1000000/total.F.eB.auto[1], eB_4.auto*1000000/total.F.eB.auto[2], eB_5.auto*1000000/total.F.eB.auto[3], eB_8.auto*1000000/total.F.eB.auto[4], eB_10.auto*1000000/total.F.eB.auto[5]), stage="earlyB.", chr="Auto", Gender="Female")

#sum up

F.Mat.chrX.dat <- rbind(late2cell.F.chrX.normalized, N4cell.F.chrX.normalized, N8cell.F.chrX.normalized, N16cell.F.chrX.normalized, eB.F.chrX.normalized)
F.Mat.auto.dat <- rbind(late2cell.F.auto.normalized, N4cell.F.auto.normalized, N8cell.F.auto.normalized, N16cell.F.auto.normalized, eB.F.auto.normalized)
dat <- rbind(F.Mat.chrX.dat, F.Mat.auto.dat)

F.Mat.chrX.dat$data <- F.Mat.chrX.dat$data / median(late2cell.F.chrX.normalized$data)
F.Mat.auto.dat$data <- F.Mat.auto.dat$data / median(late2cell.F.auto.normalized$data)
F.Mat.dat <- rbind(F.Mat.chrX.dat, F.Mat.auto.dat)

F.Mat.dat$stage <- factor(F.Mat.dat$stage, level=c("late2C","4C","8C","16C","earlyB."))

ggplot(F.Mat.dat, aes(x=stage, y=data, fill=chr)) +
  geom_boxplot(outlier.shape=NA,alpha =0.9) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
             aes(fill=chr), size=1.3, pch=21,stroke = 0.4) +
  scale_fill_manual(values=c("#FF3300","#B0E2FF")) +
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



#p-value, t-test
#4C
N4C.X <- F.Mat.dat[which(F.Mat.dat$stage=="4C"& F.Mat.dat$chr=="ChrX"), 1]
N4C.Auto <- F.Mat.dat[which(F.Mat.dat$stage=="4C"& F.Mat.dat$chr=="Auto"), 1]
t.test(x=N4C.X, y=N4C.Auto, paired=T)
#8C
N8C.X <- F.Mat.dat[which(F.Mat.dat$stage=="8C"& F.Mat.dat$chr=="ChrX"), 1]
N8C.Auto <- F.Mat.dat[which(F.Mat.dat$stage=="8C"& F.Mat.dat$chr=="Auto"), 1]
t.test(x=N8C.X, y=N8C.Auto, paired=T)
#16C
N16C.X <- F.Mat.dat[which(F.Mat.dat$stage=="16C"& F.Mat.dat$chr=="ChrX"), 1]
N16C.Auto <- F.Mat.dat[which(F.Mat.dat$stage=="16C"& F.Mat.dat$chr=="Auto"), 1]
t.test(x=N16C.X, y=N16C.Auto)
#earlyB
eB.X <- F.Mat.dat[which(F.Mat.dat$stage=="earlyB."& F.Mat.dat$chr=="ChrX"), 1]
eB.Auto <- F.Mat.dat[which(F.Mat.dat$stage=="earlyB."& F.Mat.dat$chr=="Auto"), 1]
t.test(x=eB.X, y=eB.Auto,paired=T)




#Panel g. Xa upregulation of SINEs in ES differentiating cells (random XCI). ------------


rm(list=ls())
#------late2cell Rawdata-----------------
EScell.comp <- read.table(file="processed/EScell_geneCount.txt", sep="\t", header=T, row.names=1)[ ,c(1,38:53)]
colnames(EScell.comp) <- c("Chr","Day0.Comp1","Day0.Comp2","Day1.Comp1","Day1.Comp2",
                           "Day2.Comp1","Day2.Comp2","Day3.Comp1","Day3.Comp2",
                           "Day4.Comp1","Day4.Comp2","Day6.Comp1","Day6.Comp2",
                           "Day8.Comp1","Day8.Comp2","Day10.Comp1","Day10.Comp2")
EScell.comp.Auto <- EScell.comp[which(EScell.comp$Chr != "chrX" & EScell.comp$Chr != "chrY" ), -1]
total.ES.auto <- colSums(EScell.comp.Auto)

rm(list=setdiff(ls(), "total.ES.auto"))




#Day0
#chrX
Day0_1.CAST.chrX <- read.table(file="processed/Fig6/Day0-1.CAST.sum.chrX", sep="\t")
Day0_2.CAST.chrX <- read.table(file="processed/Fig6/Day0-2.CAST.sum.chrX", sep="\t")
#auto
Day0_1.REF.auto <- read.table(file="processed/Fig6/Day0-1.REF.sum.auto", sep="\t")
Day0_2.REF.auto <- read.table(file="processed/Fig6/Day0-2.REF.sum.auto", sep="\t")

Day0_1.CAST.auto <- read.table(file="processed/Fig6/Day0-1.CAST.sum.auto", sep="\t")
Day0_2.CAST.auto <- read.table(file="processed/Fig6/Day0-2.CAST.sum.auto", sep="\t")

#chrX SINE
Day0_1.chrX <- sum(Day0_1.CAST.chrX[grep("SINE/", Day0_1.CAST.chrX$V7) ,1])
Day0_2.chrX <- sum(Day0_2.CAST.chrX[grep("SINE/", Day0_2.CAST.chrX$V7) ,1])
#auto SINE
Day0_1.auto <- sum(Day0_1.REF.auto[grep("SINE/", Day0_1.REF.auto$V7) ,1])/38 + sum(Day0_1.CAST.auto[grep("SINE/", Day0_1.CAST.auto$V7) ,1])/38
Day0_2.auto <- sum(Day0_2.REF.auto[grep("SINE/", Day0_2.REF.auto$V7) ,1])/38 + sum(Day0_2.CAST.auto[grep("SINE/", Day0_2.CAST.auto$V7) ,1])/38


Day0.chrX.normalized <- data.frame(data=c(Day0_1.chrX*1000000/total.ES.auto[1], Day0_2.chrX*1000000/total.ES.auto[2]), stage="Day0", chr="ChrX")
Day0.auto.normalized <- data.frame(data=c(Day0_1.auto*1000000/total.ES.auto[1], Day0_2.auto*1000000/total.ES.auto[2]), stage="Day0",chr="Auto")

#Day1
#chrX
Day1_1.CAST.chrX <- read.table(file="processed/Fig6/Day1-1.CAST.sum.chrX", sep="\t")
Day1_2.CAST.chrX <- read.table(file="processed/Fig6/Day1-2.CAST.sum.chrX", sep="\t")
#auto
Day1_1.REF.auto <- read.table(file="processed/Fig6/Day1-1.REF.sum.auto", sep="\t")
Day1_2.REF.auto <- read.table(file="processed/Fig6/Day1-2.REF.sum.auto", sep="\t")

Day1_1.CAST.auto <- read.table(file="processed/Fig6/Day1-1.CAST.sum.auto", sep="\t")
Day1_2.CAST.auto <- read.table(file="processed/Fig6/Day1-2.CAST.sum.auto", sep="\t")

#chrX SINE
Day1_1.chrX <- sum(Day1_1.CAST.chrX[grep("SINE/", Day1_1.CAST.chrX$V7) ,1])
Day1_2.chrX <- sum(Day1_2.CAST.chrX[grep("SINE/", Day1_2.CAST.chrX$V7) ,1])
#auto SINE
Day1_1.auto <- sum(Day1_1.REF.auto[grep("SINE/", Day1_1.REF.auto$V7) ,1])/38 + sum(Day1_1.CAST.auto[grep("SINE/", Day1_1.CAST.auto$V7) ,1])/38
Day1_2.auto <- sum(Day1_2.REF.auto[grep("SINE/", Day1_2.REF.auto$V7) ,1])/38 + sum(Day1_2.CAST.auto[grep("SINE/", Day1_2.CAST.auto$V7) ,1])/38

Day1.chrX.normalized <- data.frame(data=c(Day1_1.chrX*1000000/total.ES.auto[3], Day1_2.chrX*1000000/total.ES.auto[4]), stage="Day1", chr="ChrX")
Day1.auto.normalized <- data.frame(data=c(Day1_1.auto*1000000/total.ES.auto[3], Day1_2.auto*1000000/total.ES.auto[4]), stage="Day1",chr="Auto")

#Day2
#chrX
Day2_1.CAST.chrX <- read.table(file="processed/Fig6/Day2-1.CAST.sum.chrX", sep="\t")
Day2_2.CAST.chrX <- read.table(file="processed/Fig6/Day2-2.CAST.sum.chrX", sep="\t")
#auto
Day2_1.REF.auto <- read.table(file="processed/Fig6/Day2-1.REF.sum.auto", sep="\t")
Day2_2.REF.auto <- read.table(file="processed/Fig6/Day2-2.REF.sum.auto", sep="\t")

Day2_1.CAST.auto <- read.table(file="processed/Fig6/Day2-1.CAST.sum.auto", sep="\t")
Day2_2.CAST.auto <- read.table(file="processed/Fig6/Day2-2.CAST.sum.auto", sep="\t")

#chrX SINE
Day2_1.chrX <- sum(Day2_1.CAST.chrX[grep("SINE/", Day2_1.CAST.chrX$V7) ,1])
Day2_2.chrX <- sum(Day2_2.CAST.chrX[grep("SINE/", Day2_2.CAST.chrX$V7) ,1])
#auto SINE
Day2_1.auto <- sum(Day2_1.REF.auto[grep("SINE/", Day2_1.REF.auto$V7) ,1])/38 + sum(Day2_1.CAST.auto[grep("SINE/", Day2_1.CAST.auto$V7) ,1])/38
Day2_2.auto <- sum(Day2_2.REF.auto[grep("SINE/", Day2_2.REF.auto$V7) ,1])/38 + sum(Day2_2.CAST.auto[grep("SINE/", Day2_2.CAST.auto$V7) ,1])/38

Day2.chrX.normalized <- data.frame(data=c(Day2_1.chrX*1000000/total.ES.auto[5], Day2_2.chrX*1000000/total.ES.auto[6]), stage="Day2", chr="ChrX")
Day2.auto.normalized <- data.frame(data=c(Day2_1.auto*1000000/total.ES.auto[5], Day2_2.auto*1000000/total.ES.auto[6]), stage="Day2",chr="Auto")

#Day3
#chrX
Day3_1.CAST.chrX <- read.table(file="processed/Fig6/Day3-1.CAST.sum.chrX", sep="\t")
Day3_2.CAST.chrX <- read.table(file="processed/Fig6/Day3-2.CAST.sum.chrX", sep="\t")
#auto
Day3_1.REF.auto <- read.table(file="processed/Fig6/Day3-1.REF.sum.auto", sep="\t")
Day3_2.REF.auto <- read.table(file="processed/Fig6/Day3-2.REF.sum.auto", sep="\t")

Day3_1.CAST.auto <- read.table(file="processed/Fig6/Day3-1.CAST.sum.auto", sep="\t")
Day3_2.CAST.auto <- read.table(file="processed/Fig6/Day3-2.CAST.sum.auto", sep="\t")

#chrX SINE
Day3_1.chrX <- sum(Day3_1.CAST.chrX[grep("SINE/", Day3_1.CAST.chrX$V7) ,1])
Day3_2.chrX <- sum(Day3_2.CAST.chrX[grep("SINE/", Day3_2.CAST.chrX$V7) ,1])
#auto SINE
Day3_1.auto <- sum(Day3_1.REF.auto[grep("SINE/", Day3_1.REF.auto$V7) ,1])/38 + sum(Day3_1.CAST.auto[grep("SINE/", Day3_1.CAST.auto$V7) ,1])/38
Day3_2.auto <- sum(Day3_2.REF.auto[grep("SINE/", Day3_2.REF.auto$V7) ,1])/38 + sum(Day3_2.CAST.auto[grep("SINE/", Day3_2.CAST.auto$V7) ,1])/38


Day3.chrX.normalized <- data.frame(data=c(Day3_1.chrX*1000000/total.ES.auto[7], Day3_2.chrX*1000000/total.ES.auto[8]), stage="Day3", chr="ChrX")
Day3.auto.normalized <- data.frame(data=c(Day3_1.auto*1000000/total.ES.auto[7], Day3_2.auto*1000000/total.ES.auto[8]), stage="Day3",chr="Auto")

#Day4
#chrX
Day4_1.CAST.chrX <- read.table(file="processed/Fig6/Day4-1.CAST.sum.chrX", sep="\t")
Day4_2.CAST.chrX <- read.table(file="processed/Fig6/Day4-2.CAST.sum.chrX", sep="\t")
#auto
Day4_1.REF.auto <- read.table(file="processed/Fig6/Day4-1.REF.sum.auto", sep="\t")
Day4_2.REF.auto <- read.table(file="processed/Fig6/Day4-2.REF.sum.auto", sep="\t")

Day4_1.CAST.auto <- read.table(file="processed/Fig6/Day4-1.CAST.sum.auto", sep="\t")
Day4_2.CAST.auto <- read.table(file="processed/Fig6/Day4-2.CAST.sum.auto", sep="\t")

#chrX SINE
Day4_1.chrX <- sum(Day4_1.CAST.chrX[grep("SINE/", Day4_1.CAST.chrX$V7) ,1])
Day4_2.chrX <- sum(Day4_2.CAST.chrX[grep("SINE/", Day4_2.CAST.chrX$V7) ,1])
#auto SINE
Day4_1.auto <- sum(Day4_1.REF.auto[grep("SINE/", Day4_1.REF.auto$V7) ,1])/38 + sum(Day4_1.CAST.auto[grep("SINE/", Day4_1.CAST.auto$V7) ,1])/38
Day4_2.auto <- sum(Day4_2.REF.auto[grep("SINE/", Day4_2.REF.auto$V7) ,1])/38 + sum(Day4_2.CAST.auto[grep("SINE/", Day4_2.CAST.auto$V7) ,1])/38

Day4.chrX.normalized <- data.frame(data=c(Day4_1.chrX*1000000/total.ES.auto[9], Day4_2.chrX*1000000/total.ES.auto[10]), stage="Day4", chr="ChrX")
Day4.auto.normalized <- data.frame(data=c(Day4_1.auto*1000000/total.ES.auto[9], Day4_2.auto*1000000/total.ES.auto[10]), stage="Day4",chr="Auto")


#Day6
#chrX
Day6_1.CAST.chrX <- read.table(file="processed/Fig6/Day6-1.CAST.sum.chrX", sep="\t")
Day6_2.CAST.chrX <- read.table(file="processed/Fig6/Day6-2.CAST.sum.chrX", sep="\t")
#auto
Day6_1.REF.auto <- read.table(file="processed/Fig6/Day6-1.REF.sum.auto", sep="\t")
Day6_2.REF.auto <- read.table(file="processed/Fig6/Day6-2.REF.sum.auto", sep="\t")

Day6_1.CAST.auto <- read.table(file="processed/Fig6/Day6-1.CAST.sum.auto", sep="\t")
Day6_2.CAST.auto <- read.table(file="processed/Fig6/Day6-2.CAST.sum.auto", sep="\t")

#chrX SINE
Day6_1.chrX <- sum(Day6_1.CAST.chrX[grep("SINE/", Day6_1.CAST.chrX$V7) ,1])
Day6_2.chrX <- sum(Day6_2.CAST.chrX[grep("SINE/", Day6_2.CAST.chrX$V7) ,1])
#auto SINE
Day6_1.auto <- sum(Day6_1.REF.auto[grep("SINE/", Day6_1.REF.auto$V7) ,1])/38 + sum(Day6_1.CAST.auto[grep("SINE/", Day6_1.CAST.auto$V7) ,1])/38
Day6_2.auto <- sum(Day6_2.REF.auto[grep("SINE/", Day6_2.REF.auto$V7) ,1])/38 + sum(Day6_2.CAST.auto[grep("SINE/", Day6_2.CAST.auto$V7) ,1])/38

Day6.chrX.normalized <- data.frame(data=c(Day6_1.chrX*1000000/total.ES.auto[11], Day6_2.chrX*1000000/total.ES.auto[12]), stage="Day6", chr="ChrX")
Day6.auto.normalized <- data.frame(data=c(Day6_1.auto*1000000/total.ES.auto[11], Day6_2.auto*1000000/total.ES.auto[12]), stage="Day6",chr="Auto")

#Day8
#chrX
Day8_1.CAST.chrX <- read.table(file="processed/Fig6/Day8-1.CAST.sum.chrX", sep="\t")
Day8_2.CAST.chrX <- read.table(file="processed/Fig6/Day8-2.CAST.sum.chrX", sep="\t")
#auto
Day8_1.REF.auto <- read.table(file="processed/Fig6/Day8-1.REF.sum.auto", sep="\t")
Day8_2.REF.auto <- read.table(file="processed/Fig6/Day8-2.REF.sum.auto", sep="\t")

Day8_1.CAST.auto <- read.table(file="processed/Fig6/Day8-1.CAST.sum.auto", sep="\t")
Day8_2.CAST.auto <- read.table(file="processed/Fig6/Day8-2.CAST.sum.auto", sep="\t")

#chrX SINE
Day8_1.chrX <- sum(Day8_1.CAST.chrX[grep("SINE/", Day8_1.CAST.chrX$V7) ,1])
Day8_2.chrX <- sum(Day8_2.CAST.chrX[grep("SINE/", Day8_2.CAST.chrX$V7) ,1])
#auto SINE
Day8_1.auto <- sum(Day8_1.REF.auto[grep("SINE/", Day8_1.REF.auto$V7) ,1])/38 + sum(Day8_1.CAST.auto[grep("SINE/", Day8_1.CAST.auto$V7) ,1])/38
Day8_2.auto <- sum(Day8_2.REF.auto[grep("SINE/", Day8_2.REF.auto$V7) ,1])/38 + sum(Day8_2.CAST.auto[grep("SINE/", Day8_2.CAST.auto$V7) ,1])/38


Day8.chrX.normalized <- data.frame(data=c(Day8_1.chrX*1000000/total.ES.auto[13], Day8_2.chrX*1000000/total.ES.auto[14]), stage="Day8", chr="ChrX")
Day8.auto.normalized <- data.frame(data=c(Day8_1.auto*1000000/total.ES.auto[13], Day8_2.auto*1000000/total.ES.auto[14]), stage="Day8",chr="Auto")

#Day10
#chrX
Day10_1.CAST.chrX <- read.table(file="processed/Fig6/Day10-1.CAST.sum.chrX", sep="\t")
Day10_2.CAST.chrX <- read.table(file="processed/Fig6/Day10-2.CAST.sum.chrX", sep="\t")
#auto
Day10_1.REF.auto <- read.table(file="processed/Fig6/Day10-1.REF.sum.auto", sep="\t")
Day10_2.REF.auto <- read.table(file="processed/Fig6/Day10-2.REF.sum.auto", sep="\t")

Day10_1.CAST.auto <- read.table(file="processed/Fig6/Day10-1.CAST.sum.auto", sep="\t")
Day10_2.CAST.auto <- read.table(file="processed/Fig6/Day10-2.CAST.sum.auto", sep="\t")

#chrX SINE
Day10_1.chrX <- sum(Day10_1.CAST.chrX[grep("SINE/", Day10_1.CAST.chrX$V7) ,1])
Day10_2.chrX <- sum(Day10_2.CAST.chrX[grep("SINE/", Day10_2.CAST.chrX$V7) ,1])
#auto SINE
Day10_1.auto <- sum(Day10_1.REF.auto[grep("SINE/", Day10_1.REF.auto$V7) ,1])/38 + sum(Day10_1.CAST.auto[grep("SINE/", Day10_1.CAST.auto$V7) ,1])/38
Day10_2.auto <- sum(Day10_2.REF.auto[grep("SINE/", Day10_2.REF.auto$V7) ,1])/38 + sum(Day10_2.CAST.auto[grep("SINE/", Day10_2.CAST.auto$V7) ,1])/38

Day10.chrX.normalized <- data.frame(data=c(Day10_1.chrX*1000000/total.ES.auto[15], Day10_2.chrX*1000000/total.ES.auto[16]), stage="Day10", chr="ChrX")
Day10.auto.normalized <- data.frame(data=c(Day10_1.auto*1000000/total.ES.auto[15], Day10_2.auto*1000000/total.ES.auto[16]), stage="Day10",chr="Auto")


#sum up

ES.Mat.chrX.dat <- rbind(Day0.chrX.normalized, Day1.chrX.normalized, Day2.chrX.normalized, Day3.chrX.normalized, Day4.chrX.normalized, Day6.chrX.normalized, Day8.chrX.normalized, Day10.chrX.normalized)
ES.Mat.auto.dat <- rbind(Day0.auto.normalized, Day1.auto.normalized, Day2.auto.normalized, Day3.auto.normalized, Day4.auto.normalized, Day6.auto.normalized, Day8.auto.normalized, Day10.auto.normalized)

ES.Mat.chrX.dat$data <- ES.Mat.chrX.dat$data / median(Day0.chrX.normalized$data)
ES.Mat.auto.dat$data <- ES.Mat.auto.dat$data / median(Day0.auto.normalized$data)
ES.Mat.dat <- rbind(ES.Mat.chrX.dat, ES.Mat.auto.dat)

ES.Mat.dat$stage <- factor(ES.Mat.dat$stage, level=c("Day0","Day1","Day2","Day3","Day4","Day6","Day8","Day10"))


ggplot(ES.Mat.dat, aes(x=stage, y=data, fill=chr)) +
  geom_boxplot(outlier.shape=NA,alpha =0.9) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
             aes(fill=chr), size=1.3, pch=21,stroke = 0.4) +
  scale_fill_manual(values=c("#FF3300","burlywood1")) +
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


#p-value, t-test
#Day10
Day10.X <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day10"& ES.Mat.dat$chr=="ChrX"), 1]
Day10.Auto <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day10"& ES.Mat.dat$chr=="Auto"), 1]
t.test(x=Day10.X, y=Day10.Auto, paired=T)
#Day8
Day8.X <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day8"& ES.Mat.dat$chr=="ChrX"), 1]
Day8.Auto <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day8"& ES.Mat.dat$chr=="Auto"), 1]
t.test(x=Day8.X, y=Day8.Auto, paired=T)
#Day6
Day6.X <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day6"& ES.Mat.dat$chr=="ChrX"), 1]
Day6.Auto <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day6"& ES.Mat.dat$chr=="Auto"), 1]
t.test(x=Day6.X, y=Day6.Auto, paired=T)




#Panel h. Xa upregulation of LTRs in ES differentiating cells (random XCI).


#chrX LTR
Day0_1.chrX <- sum(Day0_1.CAST.chrX[grep("LTR/", Day0_1.CAST.chrX$V7) ,1])
Day0_2.chrX <- sum(Day0_2.CAST.chrX[grep("LTR/", Day0_2.CAST.chrX$V7) ,1])
#auto LTR
Day0_1.auto <- sum(Day0_1.REF.auto[grep("LTR/", Day0_1.REF.auto$V7) ,1])/38 + sum(Day0_1.CAST.auto[grep("LTR/", Day0_1.CAST.auto$V7) ,1])/38
Day0_2.auto <- sum(Day0_2.REF.auto[grep("LTR/", Day0_2.REF.auto$V7) ,1])/38 + sum(Day0_2.CAST.auto[grep("LTR/", Day0_2.CAST.auto$V7) ,1])/38

Day0.chrX.normalized <- data.frame(data=c(Day0_1.chrX*1000000/total.ES.auto[1], Day0_2.chrX*1000000/total.ES.auto[2]), stage="Day0", chr="ChrX")
Day0.auto.normalized <- data.frame(data=c(Day0_1.auto*1000000/total.ES.auto[1], Day0_2.auto*1000000/total.ES.auto[2]), stage="Day0",chr="Auto")

#Day1
#chrX LTR
Day1_1.chrX <- sum(Day1_1.CAST.chrX[grep("LTR/", Day1_1.CAST.chrX$V7) ,1])
Day1_2.chrX <- sum(Day1_2.CAST.chrX[grep("LTR/", Day1_2.CAST.chrX$V7) ,1])
#auto LTR
Day1_1.auto <- sum(Day1_1.REF.auto[grep("LTR/", Day1_1.REF.auto$V7) ,1])/38 + sum(Day1_1.CAST.auto[grep("LTR/", Day1_1.CAST.auto$V7) ,1])/38
Day1_2.auto <- sum(Day1_2.REF.auto[grep("LTR/", Day1_2.REF.auto$V7) ,1])/38 + sum(Day1_2.CAST.auto[grep("LTR/", Day1_2.CAST.auto$V7) ,1])/38

Day1.chrX.normalized <- data.frame(data=c(Day1_1.chrX*1000000/total.ES.auto[3], Day1_2.chrX*1000000/total.ES.auto[4]), stage="Day1", chr="ChrX")
Day1.auto.normalized <- data.frame(data=c(Day1_1.auto*1000000/total.ES.auto[3], Day1_2.auto*1000000/total.ES.auto[4]), stage="Day1",chr="Auto")

#Day2
#chrX LTR
Day2_1.chrX <- sum(Day2_1.CAST.chrX[grep("LTR/", Day2_1.CAST.chrX$V7) ,1])
Day2_2.chrX <- sum(Day2_2.CAST.chrX[grep("LTR/", Day2_2.CAST.chrX$V7) ,1])
#auto LTR
Day2_1.auto <- sum(Day2_1.REF.auto[grep("LTR/", Day2_1.REF.auto$V7) ,1])/38 + sum(Day2_1.CAST.auto[grep("LTR/", Day2_1.CAST.auto$V7) ,1])/38
Day2_2.auto <- sum(Day2_2.REF.auto[grep("LTR/", Day2_2.REF.auto$V7) ,1])/38 + sum(Day2_2.CAST.auto[grep("LTR/", Day2_2.CAST.auto$V7) ,1])/38

Day2.chrX.normalized <- data.frame(data=c(Day2_1.chrX*1000000/total.ES.auto[5], Day2_2.chrX*1000000/total.ES.auto[6]), stage="Day2", chr="ChrX")
Day2.auto.normalized <- data.frame(data=c(Day2_1.auto*1000000/total.ES.auto[5], Day2_2.auto*1000000/total.ES.auto[6]), stage="Day2",chr="Auto")

#Day3
#chrX LTR
Day3_1.chrX <- sum(Day3_1.CAST.chrX[grep("LTR/", Day3_1.CAST.chrX$V7) ,1])
Day3_2.chrX <- sum(Day3_2.CAST.chrX[grep("LTR/", Day3_2.CAST.chrX$V7) ,1])
#auto LTR
Day3_1.auto <- sum(Day3_1.REF.auto[grep("LTR/", Day3_1.REF.auto$V7) ,1])/38 + sum(Day3_1.CAST.auto[grep("LTR/", Day3_1.CAST.auto$V7) ,1])/38
Day3_2.auto <- sum(Day3_2.REF.auto[grep("LTR/", Day3_2.REF.auto$V7) ,1])/38 + sum(Day3_2.CAST.auto[grep("LTR/", Day3_2.CAST.auto$V7) ,1])/38


Day3.chrX.normalized <- data.frame(data=c(Day3_1.chrX*1000000/total.ES.auto[7], Day3_2.chrX*1000000/total.ES.auto[8]), stage="Day3", chr="ChrX")
Day3.auto.normalized <- data.frame(data=c(Day3_1.auto*1000000/total.ES.auto[7], Day3_2.auto*1000000/total.ES.auto[8]), stage="Day3",chr="Auto")

#Day4
#chrX LTR
Day4_1.chrX <- sum(Day4_1.CAST.chrX[grep("LTR/", Day4_1.CAST.chrX$V7) ,1])
Day4_2.chrX <- sum(Day4_2.CAST.chrX[grep("LTR/", Day4_2.CAST.chrX$V7) ,1])
#auto LTR
Day4_1.auto <- sum(Day4_1.REF.auto[grep("LTR/", Day4_1.REF.auto$V7) ,1])/38 + sum(Day4_1.CAST.auto[grep("LTR/", Day4_1.CAST.auto$V7) ,1])/38
Day4_2.auto <- sum(Day4_2.REF.auto[grep("LTR/", Day4_2.REF.auto$V7) ,1])/38 + sum(Day4_2.CAST.auto[grep("LTR/", Day4_2.CAST.auto$V7) ,1])/38

Day4.chrX.normalized <- data.frame(data=c(Day4_1.chrX*1000000/total.ES.auto[9], Day4_2.chrX*1000000/total.ES.auto[10]), stage="Day4", chr="ChrX")
Day4.auto.normalized <- data.frame(data=c(Day4_1.auto*1000000/total.ES.auto[9], Day4_2.auto*1000000/total.ES.auto[10]), stage="Day4",chr="Auto")


#Day6
#chrX LTR
Day6_1.chrX <- sum(Day6_1.CAST.chrX[grep("LTR/", Day6_1.CAST.chrX$V7) ,1])
Day6_2.chrX <- sum(Day6_2.CAST.chrX[grep("LTR/", Day6_2.CAST.chrX$V7) ,1])
#auto LTR
Day6_1.auto <- sum(Day6_1.REF.auto[grep("LTR/", Day6_1.REF.auto$V7) ,1])/38 + sum(Day6_1.CAST.auto[grep("LTR/", Day6_1.CAST.auto$V7) ,1])/38
Day6_2.auto <- sum(Day6_2.REF.auto[grep("LTR/", Day6_2.REF.auto$V7) ,1])/38 + sum(Day6_2.CAST.auto[grep("LTR/", Day6_2.CAST.auto$V7) ,1])/38

Day6.chrX.normalized <- data.frame(data=c(Day6_1.chrX*1000000/total.ES.auto[11], Day6_2.chrX*1000000/total.ES.auto[12]), stage="Day6", chr="ChrX")
Day6.auto.normalized <- data.frame(data=c(Day6_1.auto*1000000/total.ES.auto[11], Day6_2.auto*1000000/total.ES.auto[12]), stage="Day6",chr="Auto")

#Day8
#chrX LTR
Day8_1.chrX <- sum(Day8_1.CAST.chrX[grep("LTR/", Day8_1.CAST.chrX$V7) ,1])
Day8_2.chrX <- sum(Day8_2.CAST.chrX[grep("LTR/", Day8_2.CAST.chrX$V7) ,1])
#auto LTR
Day8_1.auto <- sum(Day8_1.REF.auto[grep("LTR/", Day8_1.REF.auto$V7) ,1])/38 + sum(Day8_1.CAST.auto[grep("LTR/", Day8_1.CAST.auto$V7) ,1])/38
Day8_2.auto <- sum(Day8_2.REF.auto[grep("LTR/", Day8_2.REF.auto$V7) ,1])/38 + sum(Day8_2.CAST.auto[grep("LTR/", Day8_2.CAST.auto$V7) ,1])/38

Day8.chrX.normalized <- data.frame(data=c(Day8_1.chrX*1000000/total.ES.auto[13], Day8_2.chrX*1000000/total.ES.auto[14]), stage="Day8", chr="ChrX")
Day8.auto.normalized <- data.frame(data=c(Day8_1.auto*1000000/total.ES.auto[13], Day8_2.auto*1000000/total.ES.auto[14]), stage="Day8",chr="Auto")

#Day10
#chrX LTR
Day10_1.chrX <- sum(Day10_1.CAST.chrX[grep("LTR/", Day10_1.CAST.chrX$V7) ,1])
Day10_2.chrX <- sum(Day10_2.CAST.chrX[grep("LTR/", Day10_2.CAST.chrX$V7) ,1])
#auto LTR
Day10_1.auto <- sum(Day10_1.REF.auto[grep("LTR/", Day10_1.REF.auto$V7) ,1])/38 + sum(Day10_1.CAST.auto[grep("LTR/", Day10_1.CAST.auto$V7) ,1])/38
Day10_2.auto <- sum(Day10_2.REF.auto[grep("LTR/", Day10_2.REF.auto$V7) ,1])/38 + sum(Day10_2.CAST.auto[grep("LTR/", Day10_2.CAST.auto$V7) ,1])/38

Day10.chrX.normalized <- data.frame(data=c(Day10_1.chrX*1000000/total.ES.auto[15], Day10_2.chrX*1000000/total.ES.auto[16]), stage="Day10", chr="ChrX")
Day10.auto.normalized <- data.frame(data=c(Day10_1.auto*1000000/total.ES.auto[15], Day10_2.auto*1000000/total.ES.auto[16]), stage="Day10",chr="Auto")


#sum up

ES.Mat.chrX.dat <- rbind(Day0.chrX.normalized, Day1.chrX.normalized, Day2.chrX.normalized, Day3.chrX.normalized, Day4.chrX.normalized, Day6.chrX.normalized, Day8.chrX.normalized, Day10.chrX.normalized)
ES.Mat.auto.dat <- rbind(Day0.auto.normalized, Day1.auto.normalized, Day2.auto.normalized, Day3.auto.normalized, Day4.auto.normalized, Day6.auto.normalized, Day8.auto.normalized, Day10.auto.normalized)
#dat <- rbind(F.Mat.chrX.dat, F.Mat.auto.dat)

ES.Mat.chrX.dat$data <- ES.Mat.chrX.dat$data / median(Day0.chrX.normalized$data)
ES.Mat.auto.dat$data <- ES.Mat.auto.dat$data / median(Day0.auto.normalized$data)
ES.Mat.dat <- rbind(ES.Mat.chrX.dat, ES.Mat.auto.dat)

ES.Mat.dat$stage <- factor(ES.Mat.dat$stage, level=c("Day0","Day1","Day2","Day3","Day4","Day6","Day8","Day10"))


ggplot(ES.Mat.dat, aes(x=stage, y=data, fill=chr)) +
  geom_boxplot(outlier.shape=NA,alpha =0.9) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
             aes(fill=chr), size=1.3, pch=21,stroke = 0.4) +
  scale_fill_manual(values=c("#FF3300","darkgray")) +
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



#p-value, t-test
#Day10
Day10.X <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day10"& ES.Mat.dat$chr=="ChrX"), 1]
Day10.Auto <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day10"& ES.Mat.dat$chr=="Auto"), 1]
t.test(x=Day10.X, y=Day10.Auto, paired=T)
#Day8
Day8.X <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day8"& ES.Mat.dat$chr=="ChrX"), 1]
Day8.Auto <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day8"& ES.Mat.dat$chr=="Auto"), 1]
t.test(x=Day8.X, y=Day8.Auto, paired=T)
#Day2
Day2.X <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day2"& ES.Mat.dat$chr=="ChrX"), 1]
Day2.Auto <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day2"& ES.Mat.dat$chr=="Auto"), 1]
t.test(x=Day2.X, y=Day2.Auto, paired=T)
#Day1
Day1.X <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day1"& ES.Mat.dat$chr=="ChrX"), 1]
Day1.Auto <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day1"& ES.Mat.dat$chr=="Auto"), 1]
t.test(x=Day6.X, y=Day1.Auto, paired=T)



#Panel i, Xa upregulation of LINEs in ES cells. 

#Day0
#chrX LINE
Day0_1.chrX <- sum(Day0_1.CAST.chrX[grep("LINE/", Day0_1.CAST.chrX$V7) ,1])
Day0_2.chrX <- sum(Day0_2.CAST.chrX[grep("LINE/", Day0_2.CAST.chrX$V7) ,1])
#auto LINE
Day0_1.auto <- sum(Day0_1.REF.auto[grep("LINE/", Day0_1.REF.auto$V7) ,1])/38 + sum(Day0_1.CAST.auto[grep("LINE/", Day0_1.CAST.auto$V7) ,1])/38
Day0_2.auto <- sum(Day0_2.REF.auto[grep("LINE/", Day0_2.REF.auto$V7) ,1])/38 + sum(Day0_2.CAST.auto[grep("LINE/", Day0_2.CAST.auto$V7) ,1])/38

Day0.chrX.normalized <- data.frame(data=c(Day0_1.chrX*1000000/total.ES.auto[1], Day0_2.chrX*1000000/total.ES.auto[2]), stage="Day0", chr="ChrX")
Day0.auto.normalized <- data.frame(data=c(Day0_1.auto*1000000/total.ES.auto[1], Day0_2.auto*1000000/total.ES.auto[2]), stage="Day0",chr="Auto")

#Day1
#chrX LINE
Day1_1.chrX <- sum(Day1_1.CAST.chrX[grep("LINE/", Day1_1.CAST.chrX$V7) ,1])
Day1_2.chrX <- sum(Day1_2.CAST.chrX[grep("LINE/", Day1_2.CAST.chrX$V7) ,1])
#auto LINE
Day1_1.auto <- sum(Day1_1.REF.auto[grep("LINE/", Day1_1.REF.auto$V7) ,1])/38 + sum(Day1_1.CAST.auto[grep("LINE/", Day1_1.CAST.auto$V7) ,1])/38
Day1_2.auto <- sum(Day1_2.REF.auto[grep("LINE/", Day1_2.REF.auto$V7) ,1])/38 + sum(Day1_2.CAST.auto[grep("LINE/", Day1_2.CAST.auto$V7) ,1])/38

Day1.chrX.normalized <- data.frame(data=c(Day1_1.chrX*1000000/total.ES.auto[3], Day1_2.chrX*1000000/total.ES.auto[4]), stage="Day1", chr="ChrX")
Day1.auto.normalized <- data.frame(data=c(Day1_1.auto*1000000/total.ES.auto[3], Day1_2.auto*1000000/total.ES.auto[4]), stage="Day1",chr="Auto")

#Day2
#chrX LINE
Day2_1.chrX <- sum(Day2_1.CAST.chrX[grep("LINE/", Day2_1.CAST.chrX$V7) ,1])
Day2_2.chrX <- sum(Day2_2.CAST.chrX[grep("LINE/", Day2_2.CAST.chrX$V7) ,1])
#auto LINE
Day2_1.auto <- sum(Day2_1.REF.auto[grep("LINE/", Day2_1.REF.auto$V7) ,1])/38 + sum(Day2_1.CAST.auto[grep("LINE/", Day2_1.CAST.auto$V7) ,1])/38
Day2_2.auto <- sum(Day2_2.REF.auto[grep("LINE/", Day2_2.REF.auto$V7) ,1])/38 + sum(Day2_2.CAST.auto[grep("LINE/", Day2_2.CAST.auto$V7) ,1])/38

Day2.chrX.normalized <- data.frame(data=c(Day2_1.chrX*1000000/total.ES.auto[5], Day2_2.chrX*1000000/total.ES.auto[6]), stage="Day2", chr="ChrX")
Day2.auto.normalized <- data.frame(data=c(Day2_1.auto*1000000/total.ES.auto[5], Day2_2.auto*1000000/total.ES.auto[6]), stage="Day2",chr="Auto")

#Day3
#chrX LINE
Day3_1.chrX <- sum(Day3_1.CAST.chrX[grep("LINE/", Day3_1.CAST.chrX$V7) ,1])
Day3_2.chrX <- sum(Day3_2.CAST.chrX[grep("LINE/", Day3_2.CAST.chrX$V7) ,1])
#auto LINE
Day3_1.auto <- sum(Day3_1.REF.auto[grep("LINE/", Day3_1.REF.auto$V7) ,1])/38 + sum(Day3_1.CAST.auto[grep("LINE/", Day3_1.CAST.auto$V7) ,1])/38
Day3_2.auto <- sum(Day3_2.REF.auto[grep("LINE/", Day3_2.REF.auto$V7) ,1])/38 + sum(Day3_2.CAST.auto[grep("LINE/", Day3_2.CAST.auto$V7) ,1])/38

Day3.chrX.normalized <- data.frame(data=c(Day3_1.chrX*1000000/total.ES.auto[7], Day3_2.chrX*1000000/total.ES.auto[8]), stage="Day3", chr="ChrX")
Day3.auto.normalized <- data.frame(data=c(Day3_1.auto*1000000/total.ES.auto[7], Day3_2.auto*1000000/total.ES.auto[8]), stage="Day3",chr="Auto")

#Day4
#chrX LINE
Day4_1.chrX <- sum(Day4_1.CAST.chrX[grep("LINE/", Day4_1.CAST.chrX$V7) ,1])
Day4_2.chrX <- sum(Day4_2.CAST.chrX[grep("LINE/", Day4_2.CAST.chrX$V7) ,1])
#auto LINE
Day4_1.auto <- sum(Day4_1.REF.auto[grep("LINE/", Day4_1.REF.auto$V7) ,1])/38 + sum(Day4_1.CAST.auto[grep("LINE/", Day4_1.CAST.auto$V7) ,1])/38
Day4_2.auto <- sum(Day4_2.REF.auto[grep("LINE/", Day4_2.REF.auto$V7) ,1])/38 + sum(Day4_2.CAST.auto[grep("LINE/", Day4_2.CAST.auto$V7) ,1])/38

Day4.chrX.normalized <- data.frame(data=c(Day4_1.chrX*1000000/total.ES.auto[9], Day4_2.chrX*1000000/total.ES.auto[10]), stage="Day4", chr="ChrX")
Day4.auto.normalized <- data.frame(data=c(Day4_1.auto*1000000/total.ES.auto[9], Day4_2.auto*1000000/total.ES.auto[10]), stage="Day4",chr="Auto")


#Day6
#chrX LINE
Day6_1.chrX <- sum(Day6_1.CAST.chrX[grep("LINE/", Day6_1.CAST.chrX$V7) ,1])
Day6_2.chrX <- sum(Day6_2.CAST.chrX[grep("LINE/", Day6_2.CAST.chrX$V7) ,1])
#auto LINE
Day6_1.auto <- sum(Day6_1.REF.auto[grep("LINE/", Day6_1.REF.auto$V7) ,1])/38 + sum(Day6_1.CAST.auto[grep("LINE/", Day6_1.CAST.auto$V7) ,1])/38
Day6_2.auto <- sum(Day6_2.REF.auto[grep("LINE/", Day6_2.REF.auto$V7) ,1])/38 + sum(Day6_2.CAST.auto[grep("LINE/", Day6_2.CAST.auto$V7) ,1])/38

Day6.chrX.normalized <- data.frame(data=c(Day6_1.chrX*1000000/total.ES.auto[11], Day6_2.chrX*1000000/total.ES.auto[12]), stage="Day6", chr="ChrX")
Day6.auto.normalized <- data.frame(data=c(Day6_1.auto*1000000/total.ES.auto[11], Day6_2.auto*1000000/total.ES.auto[12]), stage="Day6",chr="Auto")

#Day8
#chrX LINE
Day8_1.chrX <- sum(Day8_1.CAST.chrX[grep("LINE/", Day8_1.CAST.chrX$V7) ,1])
Day8_2.chrX <- sum(Day8_2.CAST.chrX[grep("LINE/", Day8_2.CAST.chrX$V7) ,1])
#auto LINE
Day8_1.auto <- sum(Day8_1.REF.auto[grep("LINE/", Day8_1.REF.auto$V7) ,1])/38 + sum(Day8_1.CAST.auto[grep("LINE/", Day8_1.CAST.auto$V7) ,1])/38
Day8_2.auto <- sum(Day8_2.REF.auto[grep("LINE/", Day8_2.REF.auto$V7) ,1])/38 + sum(Day8_2.CAST.auto[grep("LINE/", Day8_2.CAST.auto$V7) ,1])/38

Day8.chrX.normalized <- data.frame(data=c(Day8_1.chrX*1000000/total.ES.auto[13], Day8_2.chrX*1000000/total.ES.auto[14]), stage="Day8", chr="ChrX")
Day8.auto.normalized <- data.frame(data=c(Day8_1.auto*1000000/total.ES.auto[13], Day8_2.auto*1000000/total.ES.auto[14]), stage="Day8",chr="Auto")

#Day10
#chrX LINE
Day10_1.chrX <- sum(Day10_1.CAST.chrX[grep("LINE/", Day10_1.CAST.chrX$V7) ,1])
Day10_2.chrX <- sum(Day10_2.CAST.chrX[grep("LINE/", Day10_2.CAST.chrX$V7) ,1])
#auto LINE
Day10_1.auto <- sum(Day10_1.REF.auto[grep("LINE/", Day10_1.REF.auto$V7) ,1])/38 + sum(Day10_1.CAST.auto[grep("LINE/", Day10_1.CAST.auto$V7) ,1])/38
Day10_2.auto <- sum(Day10_2.REF.auto[grep("LINE/", Day10_2.REF.auto$V7) ,1])/38 + sum(Day10_2.CAST.auto[grep("LINE/", Day10_2.CAST.auto$V7) ,1])/38

Day10.chrX.normalized <- data.frame(data=c(Day10_1.chrX*1000000/total.ES.auto[15], Day10_2.chrX*1000000/total.ES.auto[16]), stage="Day10", chr="ChrX")
Day10.auto.normalized <- data.frame(data=c(Day10_1.auto*1000000/total.ES.auto[15], Day10_2.auto*1000000/total.ES.auto[16]), stage="Day10",chr="Auto")


#sum up

ES.Mat.chrX.dat <- rbind(Day0.chrX.normalized, Day1.chrX.normalized, Day2.chrX.normalized, Day3.chrX.normalized, Day4.chrX.normalized, Day6.chrX.normalized, Day8.chrX.normalized, Day10.chrX.normalized)
ES.Mat.auto.dat <- rbind(Day0.auto.normalized, Day1.auto.normalized, Day2.auto.normalized, Day3.auto.normalized, Day4.auto.normalized, Day6.auto.normalized, Day8.auto.normalized, Day10.auto.normalized)
#dat <- rbind(F.Mat.chrX.dat, F.Mat.auto.dat)

ES.Mat.chrX.dat$data <- ES.Mat.chrX.dat$data / median(Day0.chrX.normalized$data)
ES.Mat.auto.dat$data <- ES.Mat.auto.dat$data / median(Day0.auto.normalized$data)
ES.Mat.dat <- rbind(ES.Mat.chrX.dat, ES.Mat.auto.dat)

ES.Mat.dat$stage <- factor(ES.Mat.dat$stage, level=c("Day0","Day1","Day2","Day3","Day4","Day6","Day8","Day10"))



ggplot(ES.Mat.dat, aes(x=stage, y=data, fill=chr)) +
  geom_boxplot(outlier.shape=NA,alpha =0.9) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
             aes(fill=chr), size=1.3, pch=21,stroke = 0.4) +
  scale_fill_manual(values=c("#FF3300","#B0E2FF")) +
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




#p-value, t-test
#Day10 
Day10.X <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day10"& ES.Mat.dat$chr=="ChrX"), 1]
Day10.Auto <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day10"& ES.Mat.dat$chr=="Auto"), 1]
t.test(x=Day10.X, y=Day10.Auto, paired=T)
#Day8
Day8.X <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day8"& ES.Mat.dat$chr=="ChrX"), 1]
Day8.Auto <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day8"& ES.Mat.dat$chr=="Auto"), 1]
t.test(x=Day8.X, y=Day8.Auto, paired=T)
#Day6
Day6.X <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day6"& ES.Mat.dat$chr=="ChrX"), 1]
Day6.Auto <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day6"& ES.Mat.dat$chr=="Auto"), 1]
t.test(x=Day6.X, y=Day6.Auto, paired=T)
#Day2
Day2.X <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day2"& ES.Mat.dat$chr=="ChrX"), 1]
Day2.Auto <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day2"& ES.Mat.dat$chr=="Auto"), 1]
t.test(x=Day2.X, y=Day2.Auto, paired=T)
#Day1
Day1.X <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day1"& ES.Mat.dat$chr=="ChrX"), 1]
Day1.Auto <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day1"& ES.Mat.dat$chr=="Auto"), 1]
t.test(x=Day1.X, y=Day2.Auto, paired=T)

