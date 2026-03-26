#Figure S6
#Panel a.  
#Note, In this comparison, I directly Show all the normalized female chrX and FEMALE auto counts in each stage, for comparison.
# Because the Paternal gene expression is still very little at late2cell stage, which is used as the standard for normalization, I cannot combine all the cas and mus RNA together and then divided by 19 or 38.
# If I did in this way, it would introduce another factor that specifically increase the autosomal expression during preimaplantation development, because paternal autosomes will increase their expression after late2cell.
#and this would void the validity of this method. 
#This method is valid only when both autosomes and chrX gene elements are regulated in the same way or under the same mechanism apart from Xm hyperactivation.
#Therefore, I can only compare female X with female autosome. female autosomes still represent autosomes. 
# In the repeat analysis, both mus and cas RNA are combine because repeats are already biallelic at late2C stage, so there is no other factor upregulating autosomal repeats in the later stage.

#gene read counts files from MC embryos at late2C, 4C, 8C, 16C and early blastocyst stage can be downloaded from GSE168455 at GEO, including
#"late2C_MC.All_rep.total.geneCount.txt"
#"4C_MC.All_rep.total.geneCount.txt"
#"8C_MC.All_rep.total.geneCount.txt"
#"16C_MC.All_rep.total.geneCount.txt"
#earlyB_MC.All_rep.total.geneCount.txt"


rm(list=ls())
#------late2cell Rawdata-----------------
late2cell.comp <- read.table(file="late2C_MC.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)
colnames(late2cell.comp) <- c("Chr","l2.Comp2","l2.Comp3","l2.Comp4","l2.Comp5","l2.Comp6","l2.Comp7","l2.Comp8","l2.Comp9","l2.Comp10","l2.Comp11")
F.late2cell.comp.Auto <- late2cell.comp[which(late2cell.comp$Chr != "chrX" & late2cell.comp$Chr != "chrY" ), c(3,4,5,7,9)]
M.late2cell.comp.Auto <- late2cell.comp[which(late2cell.comp$Chr != "chrX" & late2cell.comp$Chr != "chrY" ), c(2,6,8,10,11)]
total.F.L2.auto <- colSums(F.late2cell.comp.Auto)
total.M.L2.auto <- colSums(M.late2cell.comp.Auto)

late2cell.allelic <- read.table(file="late2C_MC.All_rep.allelic.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
colnames(late2cell.allelic) <- c("L2.Mus2","L2.Cas2","L2.Mus3","L2.Cas3","L2.Mus4","L2.Cas4","L2.Mus5","L2.Cas5","L2.Mus6","L2.Cas6","L2.Mus7","L2.Cas7","L2.Mus8","L2.Cas8","L2.Mus9","L2.Cas9","L2.Mus10","L2.Cas10","L2.Mus11","L2.Cas11")
late2cell.mus <- c(1,3,5,7,9,11,13,15,17,19)
F.late2cell.allelic <- c(2,3,4,6,8)
late2cell.cas <- c(2,4,6,8,10,12,14,16,18,20)

#------N4cell Rawdata-----------------
N4cell.comp <- read.table(file="4C_MC.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)
colnames(N4cell.comp) <- c("Chr","4.Comp1","4.Comp2","4.Comp3","4.Comp4",
                           "4.Comp5","4.Comp7","4.Comp8","4.Comp9","4.Comp10",
                           "4.Comp11","4.Comp12","4.Comp13","4.Comp14",
                           "4.Comp15","4.Comp16","4.Comp17","4.Comp18")
F.N4cell.comp.Auto <- N4cell.comp[which(N4cell.comp$Chr != "chrX" & N4cell.comp$Chr != "chrY" ), c(5,7,12,15,16,17)]
M.N4cell.comp.Auto <- N4cell.comp[which(N4cell.comp$Chr != "chrX" & N4cell.comp$Chr != "chrY" ), c(2,3,4,6,8,9,10,11,13,14,18)]
total.F.N4.auto <- colSums(F.N4cell.comp.Auto)
total.M.N4.auto <- colSums(M.N4cell.comp.Auto)

N4cell.allelic <- read.table(file="4C_MC.All_rep.allelic.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
colnames(N4cell.allelic) <- c("4.Mus1","4.Cas1","4.Mus2","4.Cas2","4.Mus3","4.Cas3","4.Mus4","4.Cas4","4.Mus5","4.Cas5","4.Mus7","4.Cas7",
                              "4.Mus8","4.Cas8","4.Mus9","4.Cas9","4.Mus10","4.Cas10","4.Mus11","4.Cas11","4.Mus12","4.Cas12","4.Mus13",
                              "4.Cas13","4.Mus14","4.Cas14","4.Mus15","4.Cas15","4.Mus16","4.Cas16","4.Mus17","4.Cas17","4.Mus18","4.Cas18")
F.N4cell.allelic <- c(4,6,11,14,15,16)
N4cell.mus <- c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33)
N4cell.cas <- c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34)

#------N8cell Rawdata-----------------
N8cell.comp <- read.table(file="8C_MC.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)
colnames(N8cell.comp) <- c("Chr","8.Comp6","8.Comp7","8.Comp8","8.Comp9",
                           "8.Comp10","8.Comp11","8.Comp12","8.Comp13",
                           "8.Comp14","8.Comp15","8.Comp16","8.Comp17","8.Comp18","8.Comp19","8.Comp20")
F.N8cell.comp.Auto <- N8cell.comp[which(N8cell.comp$Chr != "chrX" & N8cell.comp$Chr != "chrY" ), c(2,6,8,11,15,16)]
M.N8cell.comp.Auto <- N8cell.comp[which(N8cell.comp$Chr != "chrX" & N8cell.comp$Chr != "chrY" ), c(3,4,5,7,9,10,12,13,14)]
total.F.N8.auto <- colSums(F.N8cell.comp.Auto)
total.M.N8.auto <- colSums(M.N8cell.comp.Auto)

N8cell.allelic <- read.table(file="8C_MC.All_rep.allelic.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
colnames(N8cell.allelic) <- c("8.Mus6","8.Cas6","8.Mus7","8.Cas7","8.Mus8","8.Cas8","8.Mus9","8.Cas9",
                              "8.Mus10","8.Cas10","8.Mus11","8.Cas11","8.Mus12","8.Cas12","8.Mus13","8.Cas13",
                              "8.Mus14","8.Cas14","8.Mus15","8.Cas15","8.Mus16","8.Cas16","8.Mus17","8.Cas17",
                              "8.Mus18","8.Cas18","8.Mus19","8.Cas19","8.Mus20","8.Cas20")
F.N8cell.allelic <- c(1,5,7,10,14,15)
N8cell.mus <- c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)
N8cell.cas <- c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30)

#------N16cell Rawdata-----------------
N16cell.comp <- read.table(file="16C_MC.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)
colnames(N16cell.comp) <- c("Chr","16.Comp1","16.Comp2","16.Comp3","16.Comp4","16.Comp5","16.Comp6","16.Comp7","16.Comp8")
F.N16cell.comp.Auto <- N16cell.comp[which(N16cell.comp$Chr != "chrX" & N16cell.comp$Chr != "chrY" ), c(3,4,6,7,8)]
M.N16cell.comp.Auto <- N16cell.comp[which(N16cell.comp$Chr != "chrX" & N16cell.comp$Chr != "chrY" ), c(2,5,9)]
total.F.N16.auto <- colSums(F.N16cell.comp.Auto)
total.M.N16.auto <- colSums(M.N16cell.comp.Auto)

N16cell.allelic <- read.table(file="16C_MC.All_rep.allelic.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
colnames(N16cell.allelic) <- c("16.Mus1","16.Cas1","16.Mus2","16.Cas2","16.Mus3","16.Cas3","16.Mus4","16.Cas4",
                               "16.Mus5","16.Cas5","16.Mus6","16.Cas6","16.Mus7","16.Cas7","16.Mus8","16.Cas8")
F.N16cell.allelic <- c(2,3,5,6,7)
N16cell.mus <- c(1,3,5,7,9,11,13,15)
N16cell.cas <- c(2,4,6,8,10,12,14,16)

#------earlyBlast Rawdata-----------------
earlyBlast.comp <- read.table(file="earlyB_MC.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)
colnames(earlyBlast.comp) <- c("Chr","eB.Comp1","eB.Comp2","eB.Comp3","eB.Comp4","eB.Comp5",
                               "eB.Comp6","eB.Comp7","eB.Comp8","eB.Comp9","eB.Comp10","eB.Comp11")
F.eB.comp.Auto <- earlyBlast.comp[which(earlyBlast.comp$Chr != "chrX" & earlyBlast.comp$Chr != "chrY" ), c(2,5,6,9,11)]
M.eB.comp.Auto <- earlyBlast.comp[which(earlyBlast.comp$Chr != "chrX" & earlyBlast.comp$Chr != "chrY" ), c(3,4,7,8,10,12)]
total.F.eB.auto <- colSums(F.eB.comp.Auto)
total.M.eB.auto <- colSums(M.eB.comp.Auto)

earlyBlast.allelic <- read.table(file="earlyB_MC.All_rep.allelic.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
colnames(earlyBlast.allelic) <- c("eB.Mus1","eB.Cas1","eB.Mus2","eB.Cas2","eB.Mus3","eB.Cas3","eB.Mus4","eB.Cas4",
                                  "eB.Mus5","eB.Cas5","eB.Mus6","eB.Cas6","eB.Mus7","eB.Cas7","eB.Mus8","eB.Cas8",
                                  "eB.Mus9","eB.Cas9","eB.Mus10","eB.Cas10","eB.Mus11","eB.Cas11")
F.earlyBlast.allelic <- c(1,4,5,8,10)
earlyBlast.mus <- c(1,3,5,7,9,11,13,15,17,19,21)
earlyBlast.cas <- c(2,4,6,8,10,12,14,16,18,20,22)



F.mus.All <- cbind(late2cell.allelic[ ,late2cell.mus][ ,F.late2cell.allelic], 
                   N4cell.allelic[ ,N4cell.mus][ ,F.N4cell.allelic], 
                   N8cell.allelic[ ,N8cell.mus][ ,F.N8cell.allelic], 
                   N16cell.allelic[ ,N16cell.mus][ ,F.N16cell.allelic], 
                   earlyBlast.allelic[ ,earlyBlast.mus][ ,F.earlyBlast.allelic])

F.cas.All <- cbind(late2cell.allelic[ ,late2cell.cas][ ,F.late2cell.allelic], 
                   N4cell.allelic[ ,N4cell.cas][ ,F.N4cell.allelic], 
                   N8cell.allelic[ ,N8cell.cas][ ,F.N8cell.allelic], 
                   N16cell.allelic[ ,N16cell.cas][ ,F.N16cell.allelic], 
                   earlyBlast.allelic[ ,earlyBlast.cas][ ,F.earlyBlast.allelic])

F.Allelic.All <- F.mus.All + F.cas.All

auto.gene <- rownames(earlyBlast.comp[which(earlyBlast.comp$Chr != "chrX" & earlyBlast.comp$Chr != "chrY" ), ])
chrX.gene <- rownames(earlyBlast.comp[which(earlyBlast.comp$Chr == "chrX"), ])

F.mus.All.chrX <- F.mus.All[chrX.gene, ]
F.mus.All.auto <- F.mus.All[auto.gene, ]

F.cas.All.chrX <- F.cas.All[chrX.gene, ]
F.cas.All.auto <- F.cas.All[auto.gene, ]

F.Allelic.All.chrX <- F.Allelic.All[chrX.gene, ]
F.Allelic.All.auto <- F.Allelic.All[auto.gene, ]

rm(list=setdiff(ls(), c("F.mus.All.chrX",
                        "F.mus.All.auto",
                        "F.cas.All.chrX",
                        "F.cas.All.auto",
                        "F.Allelic.All.chrX",
                        "F.Allelic.All.auto",
                        "total.F.eB.auto",
                        "total.F.N16.auto",
                        "total.F.N8.auto",
                        "total.F.N4.auto",
                        "total.F.L2.auto")))

#late2C
#female
#chrX
late2cell_3.chrX <- sum(F.mus.All.chrX[ ,1])
late2cell_4.chrX <- sum(F.mus.All.chrX[ ,2])
late2cell_5.chrX <- sum(F.mus.All.chrX[ ,3])
late2cell_7.chrX <- sum(F.mus.All.chrX[ ,4])
late2cell_9.chrX <- sum(F.mus.All.chrX[ ,5])
#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
late2cell_3.auto <- sum(F.mus.All.auto[ ,1])/19
late2cell_4.auto <- sum(F.mus.All.auto[ ,2])/19
late2cell_5.auto <- sum(F.mus.All.auto[ ,3])/19
late2cell_7.auto <- sum(F.mus.All.auto[ ,4])/19
late2cell_9.auto <- sum(F.mus.All.auto[ ,5])/19

late2cell.F.chrX.normalized <- data.frame(data=c(late2cell_3.chrX*1000000/total.F.L2.auto[1], late2cell_4.chrX*1000000/total.F.L2.auto[2], late2cell_5.chrX*1000000/total.F.L2.auto[3], late2cell_7.chrX*1000000/total.F.L2.auto[4], late2cell_9.chrX*1000000/total.F.L2.auto[5]), stage="late2C", chr="ChrX", Gender="Female")
late2cell.F.auto.normalized <- data.frame(data=c(late2cell_3.auto*1000000/total.F.L2.auto[1], late2cell_4.auto*1000000/total.F.L2.auto[2], late2cell_5.auto*1000000/total.F.L2.auto[3], late2cell_7.auto*1000000/total.F.L2.auto[4], late2cell_9.auto*1000000/total.F.L2.auto[5]), stage="late2C",chr="Auto", Gender="Female")

#female
#4C
#chrX
N4cell_4.chrX <- sum(F.mus.All.chrX[ ,6])
N4cell_7.chrX <- sum(F.mus.All.chrX[ ,7])
N4cell_12.chrX <- sum(F.mus.All.chrX[ ,8])
N4cell_15.chrX <- sum(F.mus.All.chrX[ ,9])
N4cell_16.chrX <- sum(F.mus.All.chrX[ ,10])
N4cell_17.chrX <- sum(F.mus.All.chrX[ ,11])
#auto
N4cell_4.auto <- sum(F.mus.All.auto[ ,6])/19
N4cell_7.auto <- sum(F.mus.All.auto[ ,7])/19
N4cell_12.auto <- sum(F.mus.All.auto[ ,8])/19
N4cell_15.auto <- sum(F.mus.All.auto[ ,9])/19
N4cell_16.auto <- sum(F.mus.All.auto[ ,10])/19
N4cell_17.auto <- sum(F.mus.All.auto[ ,11])/19

N4cell.F.chrX.normalized <- data.frame(data=c(N4cell_4.chrX*1000000/total.F.N4.auto[1], N4cell_7.chrX*1000000/total.F.N4.auto[2], N4cell_12.chrX*1000000/total.F.N4.auto[3], N4cell_15.chrX*1000000/total.F.N4.auto[4], N4cell_16.chrX*1000000/total.F.N4.auto[5], N4cell_17.chrX*1000000/total.F.N4.auto[6]), stage="4C", chr="ChrX", Gender="Female")
N4cell.F.auto.normalized <- data.frame(data=c(N4cell_4.auto*1000000/total.F.N4.auto[1], N4cell_7.auto*1000000/total.F.N4.auto[2], N4cell_12.auto*1000000/total.F.N4.auto[3], N4cell_15.auto*1000000/total.F.N4.auto[4], N4cell_16.auto*1000000/total.F.N4.auto[5], N4cell_17.auto*1000000/total.F.N4.auto[6]), stage="4C",chr="Auto", Gender="Female")

#8C
#Female
#chrX
N8cell_6.chrX <- sum(F.mus.All.chrX[ ,12])
N8cell_10.chrX <- sum(F.mus.All.chrX[ ,13])
N8cell_12.chrX <- sum(F.mus.All.chrX[ ,14])
N8cell_15.chrX <- sum(F.mus.All.chrX[ ,15])
N8cell_19.chrX <- sum(F.mus.All.chrX[ ,16])
N8cell_20.chrX <- sum(F.mus.All.chrX[ ,17])
#auto
N8cell_6.auto <- sum(F.mus.All.auto[ ,12])/19
N8cell_10.auto <- sum(F.mus.All.auto[ ,13])/19
N8cell_12.auto <- sum(F.mus.All.auto[ ,14])/19
N8cell_15.auto <- sum(F.mus.All.auto[ ,15])/19
N8cell_19.auto <- sum(F.mus.All.auto[ ,16])/19
N8cell_20.auto <- sum(F.mus.All.auto[ ,17])/19

N8cell.F.chrX.normalized <- data.frame(data=c(N8cell_6.chrX*1000000/total.F.N8.auto[1], N8cell_10.chrX*1000000/total.F.N8.auto[2], N8cell_12.chrX*1000000/total.F.N8.auto[3], N8cell_15.chrX*1000000/total.F.N8.auto[4], N8cell_19.chrX*1000000/total.F.N8.auto[5], N8cell_20.chrX*1000000/total.F.N8.auto[6]), stage="8C",chr="ChrX", Gender="Female")
N8cell.F.auto.normalized <- data.frame(data=c(N8cell_6.auto*1000000/total.F.N8.auto[1], N8cell_10.auto*1000000/total.F.N8.auto[2], N8cell_12.auto*1000000/total.F.N8.auto[3], N8cell_15.auto*1000000/total.F.N8.auto[4], N8cell_19.auto*1000000/total.F.N8.auto[5], N8cell_20.auto*1000000/total.F.N8.auto[6]), stage="8C", chr="Auto", Gender="Female")



#16C
#Female
#chrX
N16cell_2.chrX <- sum(F.mus.All.chrX[ ,18])
N16cell_3.chrX <- sum(F.mus.All.chrX[ ,19])
N16cell_5.chrX <- sum(F.mus.All.chrX[ ,20])
N16cell_6.chrX <- sum(F.mus.All.chrX[ ,21])
N16cell_7.chrX <- sum(F.mus.All.chrX[ ,22])
#Auto
N16cell_2.auto <- sum(F.mus.All.auto[ ,18])/19
N16cell_3.auto <- sum(F.mus.All.auto[ ,19])/19
N16cell_5.auto <- sum(F.mus.All.auto[ ,20])/19
N16cell_6.auto <- sum(F.mus.All.auto[ ,21])/19
N16cell_7.auto <- sum(F.mus.All.auto[ ,22])/19

N16cell.F.chrX.normalized <- data.frame(data=c(N16cell_2.chrX*1000000/total.F.N16.auto[1], N16cell_3.chrX*1000000/total.F.N16.auto[2], N16cell_5.chrX*1000000/total.F.N16.auto[3], N16cell_6.chrX*1000000/total.F.N16.auto[4], N16cell_7.chrX*1000000/total.F.N16.auto[5]), stage="16C", chr="ChrX", Gender="Female")
N16cell.F.auto.normalized <- data.frame(data=c(N16cell_2.auto*1000000/total.F.N16.auto[1], N16cell_3.auto*1000000/total.F.N16.auto[2], N16cell_5.auto*1000000/total.F.N16.auto[3], N16cell_6.auto*1000000/total.F.N16.auto[4], N16cell_7.auto*1000000/total.F.N16.auto[5]), stage="16C", chr="Auto", Gender="Female")


#eB
#Female
#chrX
eB_1.chrX <- sum(F.mus.All.chrX[ ,23])
eB_4.chrX <- sum(F.mus.All.chrX[ ,24])
eB_5.chrX <- sum(F.mus.All.chrX[ ,25])
eB_8.chrX <- sum(F.mus.All.chrX[ ,26])
eB_10.chrX <- sum(F.mus.All.chrX[ ,27])
#Auto
eB_1.auto <- sum(F.mus.All.auto[ ,23])/19
eB_4.auto <- sum(F.mus.All.auto[ ,24])/19
eB_5.auto <- sum(F.mus.All.auto[ ,25])/19
eB_8.auto <- sum(F.mus.All.auto[ ,26])/19
eB_10.auto <- sum(F.mus.All.auto[ ,27])/19

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
#4C p=0.2507
N4C.X <- F.Mat.dat[which(F.Mat.dat$stage=="4C"& F.Mat.dat$chr=="ChrX"), 1]
N4C.Auto <- F.Mat.dat[which(F.Mat.dat$stage=="4C"& F.Mat.dat$chr=="Auto"), 1]
t.test(x=N4C.X, y=N4C.Auto, paired = TRUE, alternative = "two.sided")
#8C, p=0.01905
N8C.X <- F.Mat.dat[which(F.Mat.dat$stage=="8C"& F.Mat.dat$chr=="ChrX"), 1]
N8C.Auto <- F.Mat.dat[which(F.Mat.dat$stage=="8C"& F.Mat.dat$chr=="Auto"), 1]
t.test(x=N8C.X, y=N8C.Auto, paired = TRUE, alternative = "two.sided")
#16C p=0.01712
N16C.X <- F.Mat.dat[which(F.Mat.dat$stage=="16C"& F.Mat.dat$chr=="ChrX"), 1]
N16C.Auto <- F.Mat.dat[which(F.Mat.dat$stage=="16C"& F.Mat.dat$chr=="Auto"), 1]
t.test(x=N16C.X, y=N16C.Auto, paired = TRUE, alternative = "two.sided")
#earlyB p=0.01694
eB.X <- F.Mat.dat[which(F.Mat.dat$stage=="earlyB."& F.Mat.dat$chr=="ChrX"), 1]
eB.Auto <- F.Mat.dat[which(F.Mat.dat$stage=="earlyB."& F.Mat.dat$chr=="Auto"), 1]
t.test(x=eB.X, y=eB.Auto, paired = TRUE, alternative = "two.sided")


#Panel d.check chr1 as a positive control. "auto2" are the combined autosomal reads except chr1. This is analysis of TE
#female
#late2C
#chr1
late2cell_3.chr1 <- sum(read.table(file="processed/Fig6/late2cell-3.REF.sum.chr1", sep="\t")[ ,1])
late2cell_4.chr1 <- sum(read.table(file="processed/Fig6/late2cell-4.REF.sum.chr1", sep="\t")[ ,1])
late2cell_5.chr1 <- sum(read.table(file="processed/Fig6/late2cell-5.REF.sum.chr1", sep="\t")[ ,1])
late2cell_7.chr1 <- sum(read.table(file="processed/Fig6/late2cell-7.REF.sum.chr1", sep="\t")[ ,1])
late2cell_9.chr1 <- sum(read.table(file="processed/Fig6/late2cell-9.REF.sum.chr1", sep="\t")[ ,1])
#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
late2cell_3.auto <- sum(read.table(file="processed/Fig6/late2cell-3.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/late2cell-3.CAST.sum.auto", sep="\t")[ ,1])/19
late2cell_4.auto <- sum(read.table(file="processed/Fig6/late2cell-4.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/late2cell-4.CAST.sum.auto", sep="\t")[ ,1])/19
late2cell_5.auto <- sum(read.table(file="processed/Fig6/late2cell-5.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/late2cell-5.CAST.sum.auto", sep="\t")[ ,1])/19
late2cell_7.auto <- sum(read.table(file="processed/Fig6/late2cell-7.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/late2cell-7.CAST.sum.auto", sep="\t")[ ,1])/19
late2cell_9.auto <- sum(read.table(file="processed/Fig6/late2cell-9.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/late2cell-9.CAST.sum.auto", sep="\t")[ ,1])/19

late2cell.F.chr1.normalized <- data.frame(data=c(late2cell_3.chr1*1000000/total.F.L2.auto[1], late2cell_4.chr1*1000000/total.F.L2.auto[2], late2cell_5.chr1*1000000/total.F.L2.auto[3], late2cell_7.chr1*1000000/total.F.L2.auto[4], late2cell_9.chr1*1000000/total.F.L2.auto[5]), stage="late2C", chr="chr1", Gender="Female")
late2cell.F.auto.normalized <- data.frame(data=c(late2cell_3.auto*1000000/total.F.L2.auto[1], late2cell_4.auto*1000000/total.F.L2.auto[2], late2cell_5.auto*1000000/total.F.L2.auto[3], late2cell_7.auto*1000000/total.F.L2.auto[4], late2cell_9.auto*1000000/total.F.L2.auto[5]), stage="late2C",chr="Auto", Gender="Female")

#female
#4C
#chr1
N4cell_4.chr1 <- sum(read.table(file="processed/Fig6/4cell-4.REF.sum.chr1", sep="\t")[ ,1])
N4cell_7.chr1 <- sum(read.table(file="processed/Fig6/4cell-7.REF.sum.chr1", sep="\t")[ ,1])
N4cell_12.chr1 <- sum(read.table(file="processed/Fig6/4cell-12.REF.sum.chr1", sep="\t")[ ,1])
N4cell_15.chr1 <- sum(read.table(file="processed/Fig6/4cell-15.REF.sum.chr1", sep="\t")[ ,1])
N4cell_16.chr1 <- sum(read.table(file="processed/Fig6/4cell-16.REF.sum.chr1", sep="\t")[ ,1])
N4cell_17.chr1 <- sum(read.table(file="processed/Fig6/4cell-17.REF.sum.chr1", sep="\t")[ ,1])
#auto
N4cell_4.auto <- sum(read.table(file="processed/Fig6/4cell-4.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/4cell-4.CAST.sum.auto", sep="\t")[ ,1])/19
N4cell_7.auto <- sum(read.table(file="processed/Fig6/4cell-7.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/4cell-7.CAST.sum.auto", sep="\t")[ ,1])/19
N4cell_12.auto <- sum(read.table(file="processed/Fig6/4cell-12.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/4cell-12.CAST.sum.auto", sep="\t")[ ,1])/19
N4cell_15.auto <- sum(read.table(file="processed/Fig6/4cell-15.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/4cell-15.CAST.sum.auto", sep="\t")[ ,1])/19
N4cell_16.auto <- sum(read.table(file="processed/Fig6/4cell-16.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/4cell-16.CAST.sum.auto", sep="\t")[ ,1])/19
N4cell_17.auto <- sum(read.table(file="processed/Fig6/4cell-17.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/4cell-17.CAST.sum.auto", sep="\t")[ ,1])/19

N4cell.F.chr1.normalized <- data.frame(data=c(N4cell_4.chr1*1000000/total.F.N4.auto[1], N4cell_7.chr1*1000000/total.F.N4.auto[2], N4cell_12.chr1*1000000/total.F.N4.auto[3], N4cell_15.chr1*1000000/total.F.N4.auto[4], N4cell_16.chr1*1000000/total.F.N4.auto[5], N4cell_17.chr1*1000000/total.F.N4.auto[6]), stage="4C", chr="chr1", Gender="Female")
N4cell.F.auto.normalized <- data.frame(data=c(N4cell_4.auto*1000000/total.F.N4.auto[1], N4cell_7.auto*1000000/total.F.N4.auto[2], N4cell_12.auto*1000000/total.F.N4.auto[3], N4cell_15.auto*1000000/total.F.N4.auto[4], N4cell_16.auto*1000000/total.F.N4.auto[5], N4cell_17.auto*1000000/total.F.N4.auto[6]), stage="4C",chr="Auto", Gender="Female")

#8C
#Female
#chr1
N8cell_6.chr1 <- sum(read.table(file="processed/Fig6/8cell-6.REF.sum.chr1", sep="\t")[ ,1])
N8cell_10.chr1 <- sum(read.table(file="processed/Fig6/8cell-10.REF.sum.chr1", sep="\t")[ ,1])
N8cell_12.chr1 <- sum(read.table(file="processed/Fig6/8cell-12.REF.sum.chr1", sep="\t")[ ,1])
N8cell_15.chr1 <- sum(read.table(file="processed/Fig6/8cell-15.REF.sum.chr1", sep="\t")[ ,1])
N8cell_19.chr1 <- sum(read.table(file="processed/Fig6/8cell-19.REF.sum.chr1", sep="\t")[ ,1])
N8cell_20.chr1 <- sum(read.table(file="processed/Fig6/8cell-20.REF.sum.chr1", sep="\t")[ ,1])
#auto
N8cell_6.auto <- sum(read.table(file="processed/Fig6/8cell-6.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/8cell-6.CAST.sum.auto", sep="\t")[ ,1])/19
N8cell_10.auto <- sum(read.table(file="processed/Fig6/8cell-10.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/8cell-10.CAST.sum.auto", sep="\t")[ ,1])/19
N8cell_12.auto <- sum(read.table(file="processed/Fig6/8cell-12.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/8cell-12.CAST.sum.auto", sep="\t")[ ,1])/19
N8cell_15.auto <- sum(read.table(file="processed/Fig6/8cell-15.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/8cell-15.CAST.sum.auto", sep="\t")[ ,1])/19
N8cell_19.auto <- sum(read.table(file="processed/Fig6/8cell-19.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/8cell-19.CAST.sum.auto", sep="\t")[ ,1])/19
N8cell_20.auto <- sum(read.table(file="processed/Fig6/8cell-20.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/8cell-20.CAST.sum.auto", sep="\t")[ ,1])/19

N8cell.F.chr1.normalized <- data.frame(data=c(N8cell_6.chr1*1000000/total.F.N8.auto[1], N8cell_10.chr1*1000000/total.F.N8.auto[2], N8cell_12.chr1*1000000/total.F.N8.auto[3], N8cell_15.chr1*1000000/total.F.N8.auto[4], N8cell_19.chr1*1000000/total.F.N8.auto[5], N8cell_20.chr1*1000000/total.F.N8.auto[6]), stage="8C",chr="chr1", Gender="Female")
N8cell.F.auto.normalized <- data.frame(data=c(N8cell_6.auto*1000000/total.F.N8.auto[1], N8cell_10.auto*1000000/total.F.N8.auto[2], N8cell_12.auto*1000000/total.F.N8.auto[3], N8cell_15.auto*1000000/total.F.N8.auto[4], N8cell_19.auto*1000000/total.F.N8.auto[5], N8cell_20.auto*1000000/total.F.N8.auto[6]), stage="8C", chr="Auto", Gender="Female")

#16C
#Female
#chr1
N16cell_2.chr1 <- sum(read.table(file="processed/Fig6/16cell-2.REF.sum.chr1", sep="\t")[ ,1])
N16cell_3.chr1 <- sum(read.table(file="processed/Fig6/16cell-3.REF.sum.chr1", sep="\t")[ ,1])
N16cell_5.chr1 <- sum(read.table(file="processed/Fig6/16cell-5.REF.sum.chr1", sep="\t")[ ,1])
N16cell_6.chr1 <- sum(read.table(file="processed/Fig6/16cell-6.REF.sum.chr1", sep="\t")[ ,1])
N16cell_7.chr1 <- sum(read.table(file="processed/Fig6/16cell-7.REF.sum.chr1", sep="\t")[ ,1])
#Auto
N16cell_2.auto <- sum(read.table(file="processed/Fig6/16cell-2.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/16cell-2.CAST.sum.auto", sep="\t")[ ,1])/19
N16cell_3.auto <- sum(read.table(file="processed/Fig6/16cell-3.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/16cell-3.CAST.sum.auto", sep="\t")[ ,1])/19
N16cell_5.auto <- sum(read.table(file="processed/Fig6/16cell-5.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/16cell-5.CAST.sum.auto", sep="\t")[ ,1])/19
N16cell_6.auto <- sum(read.table(file="processed/Fig6/16cell-6.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/16cell-6.CAST.sum.auto", sep="\t")[ ,1])/19
N16cell_7.auto <- sum(read.table(file="processed/Fig6/16cell-7.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/16cell-7.CAST.sum.auto", sep="\t")[ ,1])/19

N16cell.F.chr1.normalized <- data.frame(data=c(N16cell_2.chr1*1000000/total.F.N16.auto[1], N16cell_3.chr1*1000000/total.F.N16.auto[2], N16cell_5.chr1*1000000/total.F.N16.auto[3], N16cell_6.chr1*1000000/total.F.N16.auto[4], N16cell_7.chr1*1000000/total.F.N16.auto[5]), stage="16C", chr="chr1", Gender="Female")
N16cell.F.auto.normalized <- data.frame(data=c(N16cell_2.auto*1000000/total.F.N16.auto[1], N16cell_3.auto*1000000/total.F.N16.auto[2], N16cell_5.auto*1000000/total.F.N16.auto[3], N16cell_6.auto*1000000/total.F.N16.auto[4], N16cell_7.auto*1000000/total.F.N16.auto[5]), stage="16C", chr="Auto", Gender="Female")

#eB
#Female
#chr1
eB_1.chr1 <- sum(read.table(file="processed/Fig6/earlyBlast-1.REF.sum.chr1", sep="\t")[ ,1])
eB_4.chr1 <- sum(read.table(file="processed/Fig6/earlyBlast-4.REF.sum.chr1", sep="\t")[ ,1])
eB_5.chr1 <- sum(read.table(file="processed/Fig6/earlyBlast-5.REF.sum.chr1", sep="\t")[ ,1])
eB_8.chr1 <- sum(read.table(file="processed/Fig6/earlyBlast-8.REF.sum.chr1", sep="\t")[ ,1])
eB_10.chr1 <- sum(read.table(file="processed/Fig6/earlyBlast-10.REF.sum.chr1", sep="\t")[ ,1])
#Auto
eB_1.auto <- sum(read.table(file="processed/Fig6/earlyBlast-1.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/earlyBlast-1.CAST.sum.auto", sep="\t")[ ,1])/19
eB_4.auto <- sum(read.table(file="processed/Fig6/earlyBlast-4.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/earlyBlast-4.CAST.sum.auto", sep="\t")[ ,1])/19
eB_5.auto <- sum(read.table(file="processed/Fig6/earlyBlast-5.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/earlyBlast-5.CAST.sum.auto", sep="\t")[ ,1])/19
eB_8.auto <- sum(read.table(file="processed/Fig6/earlyBlast-8.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/earlyBlast-8.CAST.sum.auto", sep="\t")[ ,1])/19
eB_10.auto <- sum(read.table(file="processed/Fig6/earlyBlast-10.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/earlyBlast-10.CAST.sum.auto", sep="\t")[ ,1])/19

eB.F.chr1.normalized <- data.frame(data=c(eB_1.chr1*1000000/total.F.eB.auto[1], eB_4.chr1*1000000/total.F.eB.auto[2], eB_5.chr1*1000000/total.F.eB.auto[3], eB_8.chr1*1000000/total.F.eB.auto[4], eB_10.chr1*1000000/total.F.eB.auto[5]), stage="earlyB.", chr="chr1", Gender="Female")
eB.F.auto.normalized <- data.frame(data=c(eB_1.auto*1000000/total.F.eB.auto[1], eB_4.auto*1000000/total.F.eB.auto[2], eB_5.auto*1000000/total.F.eB.auto[3], eB_8.auto*1000000/total.F.eB.auto[4], eB_10.auto*1000000/total.F.eB.auto[5]), stage="earlyB.", chr="Auto", Gender="Female")
#sum up

F.Mat.chr1.dat <- rbind(late2cell.F.chr1.normalized, N4cell.F.chr1.normalized, N8cell.F.chr1.normalized, N16cell.F.chr1.normalized, eB.F.chr1.normalized)
F.Mat.auto.dat <- rbind(late2cell.F.auto.normalized, N4cell.F.auto.normalized, N8cell.F.auto.normalized, N16cell.F.auto.normalized, eB.F.auto.normalized)
dat <- rbind(F.Mat.chr1.dat, F.Mat.auto.dat)

F.Mat.chr1.dat$data <- F.Mat.chr1.dat$data / median(late2cell.F.chr1.normalized$data)
F.Mat.auto.dat$data <- F.Mat.auto.dat$data / median(late2cell.F.auto.normalized$data)
F.Mat.dat <- rbind(F.Mat.chr1.dat, F.Mat.auto.dat)

F.Mat.dat$stage <- factor(F.Mat.dat$stage, level=c("late2C","4C","8C","16C","earlyB."))


ggplot(F.Mat.dat, aes(x=stage, y=data, fill=chr)) +
  geom_boxplot(outlier.shape=NA,alpha =0.9) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
             aes(fill=chr), size=1.3, pch=21,stroke = 0.4) +
  scale_fill_manual(values=c("#FF3300","lightgoldenrod")) +
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
#4C p=0.6265
N4C.1 <- F.Mat.dat[which(F.Mat.dat$stage=="4C"& F.Mat.dat$chr=="chr1"), 1]
N4C.Auto <- F.Mat.dat[which(F.Mat.dat$stage=="4C"& F.Mat.dat$chr=="Auto"), 1]
t.test(x=N4C.1, y=N4C.Auto, paired=T)
#8C, p=0.2016
N8C.1 <- F.Mat.dat[which(F.Mat.dat$stage=="8C"& F.Mat.dat$chr=="chr1"), 1]
N8C.Auto <- F.Mat.dat[which(F.Mat.dat$stage=="8C"& F.Mat.dat$chr=="Auto"), 1]
t.test(x=N8C.1, y=N8C.Auto, paired=T)
#16C, p=0.9354
N16C.1 <- F.Mat.dat[which(F.Mat.dat$stage=="16C"& F.Mat.dat$chr=="chr1"), 1]
N16C.Auto <- F.Mat.dat[which(F.Mat.dat$stage=="16C"& F.Mat.dat$chr=="Auto"), 1]
t.test(x=N16C.1, y=N16C.Auto)
#earlyB p=0.03367，but chr1 is decreased, not upregulated.
eB.1 <- F.Mat.dat[which(F.Mat.dat$stage=="earlyB."& F.Mat.dat$chr=="chr1"), 1]
eB.Auto <- F.Mat.dat[which(F.Mat.dat$stage=="earlyB."& F.Mat.dat$chr=="Auto"), 1]
t.test(x=eB.1, y=eB.Auto,paired=T)




#Panel b. Gene analysis in EScells

rm(list=ls())
#------late2cell Rawdata-----------------
EScell.comp <- read.table(file="processed/EScell_geneCount.txt", sep="\t", header=T, row.names=1)[ ,c(1,38:53)]
colnames(EScell.comp) <- c("Chr","Day0.Comp1","Day0.Comp2","Day1.Comp1","Day1.Comp2",
                           "Day2.Comp1","Day2.Comp2","Day3.Comp1","Day3.Comp2",
                           "Day4.Comp1","Day4.Comp2","Day6.Comp1","Day6.Comp2",
                           "Day8.Comp1","Day8.Comp2","Day10.Comp1","Day10.Comp2")
EScell.comp.Auto <- EScell.comp[which(EScell.comp$Chr != "chrX" & EScell.comp$Chr != "chrY" ), -1]
total.ES.auto <- colSums(EScell.comp.Auto)

EScell.allelic <- read.table(file="processed/EScell_geneCount.txt", sep="\t", header=T, row.names=1)[ ,c(1,6:37)]
colnames(EScell.allelic) <- c("Chr","Day0.Mus1","Day0.Cas1","Day0.Mus2","Day0.Cas2",
                              "Day1.Mus1","Day1.Cas1","Day1.Mus2","Day1.Cas2",
                              "Day2.Mus1","Day2.Cas1","Day2.Mus2","Day2.Cas2",
                              "Day3.Mus1","Day3.Cas1","Day3.Mus2","Day3.Cas2",
                              "Day4.Mus1","Day4.Cas1","Day4.Mus2","Day4.Cas2",
                              "Day6.Mus1","Day6.Cas1","Day6.Mus2","Day6.Cas2",
                              "Day8.Mus1","Day8.Cas1","Day8.Mus2","Day8.Cas2",
                              "Day10.Mus1","Day10.Cas1","Day10.Mus2","Day10.Cas2")

auto.gene <- rownames(EScell.comp[which(EScell.comp$Chr != "chrX" & EScell.comp$Chr != "chrY" ), ])
chrX.gene <- rownames(EScell.comp[which(EScell.comp$Chr == "chrX"), ])

F.mus.All <- EScell.allelic[ ,c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32)]
F.cas.All <- EScell.allelic[ ,c(3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33)]

F.mus.chrX <- colSums(F.mus.All[chrX.gene, ])
F.cas.chrX <- colSums(F.cas.All[chrX.gene, ])
F.mus.auto <- colSums(F.mus.All[auto.gene, ])
F.cas.auto <- colSums(F.cas.All[auto.gene, ])

#Day0
Day0_1.chrX <- F.cas.chrX[1]
Day0_2.chrX <- F.cas.chrX[2]
Day0_1.auto <- F.mus.auto[1]/19 + F.cas.auto[1]/19
Day0_2.auto <- F.mus.auto[2]/19 + F.cas.auto[2]/19

Day0.chrX.normalized <- data.frame(data=c(Day0_1.chrX*1000000/total.ES.auto[1], Day0_2.chrX*1000000/total.ES.auto[2]), stage="Day0", chr="ChrX", Gender="Female")
Day0.auto.normalized <- data.frame(data=c(Day0_1.auto*1000000/total.ES.auto[1], Day0_2.auto*1000000/total.ES.auto[2]), stage="Day0",chr="Auto", Gender="Female")

#Day1
Day1_1.chrX <- F.cas.chrX[3]
Day1_2.chrX <- F.cas.chrX[4]
Day1_1.auto <- F.mus.auto[3]/19 + F.cas.auto[3]/19
Day1_2.auto <- F.mus.auto[4]/19 + F.cas.auto[4]/19

Day1.chrX.normalized <- data.frame(data=c(Day1_1.chrX*1000000/total.ES.auto[3], Day1_2.chrX*1000000/total.ES.auto[4]), stage="Day1", chr="ChrX", Gender="Female")
Day1.auto.normalized <- data.frame(data=c(Day1_1.auto*1000000/total.ES.auto[3], Day1_2.auto*1000000/total.ES.auto[4]), stage="Day1",chr="Auto", Gender="Female")

#Day2
Day2_1.chrX <- F.cas.chrX[5]
Day2_2.chrX <- F.cas.chrX[6]
Day2_1.auto <- F.mus.auto[5]/19 + F.cas.auto[5]/19
Day2_2.auto <- F.mus.auto[6]/19 + F.cas.auto[6]/19

Day2.chrX.normalized <- data.frame(data=c(Day2_1.chrX*1000000/total.ES.auto[5], Day2_2.chrX*1000000/total.ES.auto[6]), stage="Day2", chr="ChrX", Gender="Female")
Day2.auto.normalized <- data.frame(data=c(Day2_1.auto*1000000/total.ES.auto[5], Day2_2.auto*1000000/total.ES.auto[6]), stage="Day2",chr="Auto", Gender="Female")

#Day3
Day3_1.chrX <- F.cas.chrX[7]
Day3_2.chrX <- F.cas.chrX[8]
Day3_1.auto <- F.mus.auto[7]/19 + F.cas.auto[7]/19
Day3_2.auto <- F.mus.auto[8]/19 + F.cas.auto[8]/19

Day3.chrX.normalized <- data.frame(data=c(Day3_1.chrX*1000000/total.ES.auto[7], Day3_2.chrX*1000000/total.ES.auto[8]), stage="Day3", chr="ChrX", Gender="Female")
Day3.auto.normalized <- data.frame(data=c(Day3_1.auto*1000000/total.ES.auto[7], Day3_2.auto*1000000/total.ES.auto[8]), stage="Day3",chr="Auto", Gender="Female")

#Day4
Day4_1.chrX <- F.cas.chrX[9]
Day4_2.chrX <- F.cas.chrX[10]
Day4_1.auto <- F.mus.auto[9]/19 + F.cas.auto[9]/19
Day4_2.auto <- F.mus.auto[10]/19 + F.cas.auto[10]/19

Day4.chrX.normalized <- data.frame(data=c(Day4_1.chrX*1000000/total.ES.auto[9], Day4_2.chrX*1000000/total.ES.auto[10]), stage="Day4", chr="ChrX", Gender="Female")
Day4.auto.normalized <- data.frame(data=c(Day4_1.auto*1000000/total.ES.auto[9], Day4_2.auto*1000000/total.ES.auto[10]), stage="Day4",chr="Auto", Gender="Female")

#Day6
Day6_1.chrX <- F.cas.chrX[11]
Day6_2.chrX <- F.cas.chrX[12]
Day6_1.auto <- F.mus.auto[11]/19 + F.cas.auto[11]/19
Day6_2.auto <- F.mus.auto[12]/19 + F.cas.auto[12]/19

Day6.chrX.normalized <- data.frame(data=c(Day6_1.chrX*1000000/total.ES.auto[11], Day6_2.chrX*1000000/total.ES.auto[12]), stage="Day6", chr="ChrX", Gender="Female")
Day6.auto.normalized <- data.frame(data=c(Day6_1.auto*1000000/total.ES.auto[11], Day6_2.auto*1000000/total.ES.auto[12]), stage="Day6",chr="Auto", Gender="Female")

#Day8
Day8_1.chrX <- F.cas.chrX[13]
Day8_2.chrX <- F.cas.chrX[14]
Day8_1.auto <- F.mus.auto[13]/19 + F.cas.auto[13]/19
Day8_2.auto <- F.mus.auto[14]/19 + F.cas.auto[14]/19

Day8.chrX.normalized <- data.frame(data=c(Day8_1.chrX*1000000/total.ES.auto[13], Day8_2.chrX*1000000/total.ES.auto[14]), stage="Day8", chr="ChrX", Gender="Female")
Day8.auto.normalized <- data.frame(data=c(Day8_1.auto*1000000/total.ES.auto[13], Day8_2.auto*1000000/total.ES.auto[14]), stage="Day8",chr="Auto", Gender="Female")

#Day10
Day10_1.chrX <- F.cas.chrX[15]
Day10_2.chrX <- F.cas.chrX[16]
Day10_1.auto <- F.mus.auto[15]/19 + F.cas.auto[15]/19
Day10_2.auto <- F.mus.auto[16]/19 + F.cas.auto[16]/19

Day10.chrX.normalized <- data.frame(data=c(Day10_1.chrX*1000000/total.ES.auto[15], Day10_2.chrX*1000000/total.ES.auto[16]), stage="Day10", chr="ChrX", Gender="Female")
Day10.auto.normalized <- data.frame(data=c(Day10_1.auto*1000000/total.ES.auto[15], Day10_2.auto*1000000/total.ES.auto[16]), stage="Day10",chr="Auto", Gender="Female")



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
#Day1 p=0.0003082
Day1.X <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day1"& ES.Mat.dat$chr=="ChrX"), 1]
Day1.Auto <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day1"& ES.Mat.dat$chr=="Auto"), 1]
t.test(x=Day1.X, y=Day1.Auto, paired=T)

#Day2 p=0.06329
Day2.X <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day2"& ES.Mat.dat$chr=="ChrX"), 1]
Day2.Auto <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day2"& ES.Mat.dat$chr=="Auto"), 1]
t.test(x=Day2.X, y=Day2.Auto, paired=T)

#Day3 p=0.004394
Day3.X <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day3"& ES.Mat.dat$chr=="ChrX"), 1]
Day3.Auto <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day3"& ES.Mat.dat$chr=="Auto"), 1]
t.test(x=Day3.X, y=Day3.Auto, paired=T)

#Day4 p=0.02875
Day4.X <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day4"& ES.Mat.dat$chr=="ChrX"), 1]
Day4.Auto <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day4"& ES.Mat.dat$chr=="Auto"), 1]
t.test(x=Day4.X, y=Day4.Auto, paired=T)

#Day6 p=0.01375
Day6.X <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day6"& ES.Mat.dat$chr=="ChrX"), 1]
Day6.Auto <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day6"& ES.Mat.dat$chr=="Auto"), 1]
t.test(x=Day6.X, y=Day6.Auto, paired=T)

#Day8 p=0.0001284
Day8.X <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day8"& ES.Mat.dat$chr=="ChrX"), 1]
Day8.Auto <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day8"& ES.Mat.dat$chr=="Auto"), 1]
t.test(x=Day8.X, y=Day8.Auto, paired=T)

#Day10 p=0.005453
Day10.X <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day10"& ES.Mat.dat$chr=="ChrX"), 1]
Day10.Auto <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day10"& ES.Mat.dat$chr=="Auto"), 1]
t.test(x=Day10.X, y=Day10.Auto, paired=T)







#Panel e. test repeats in chr13 in EScell as a negative control

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
#chr13
Day0_1.chr13 <- sum(read.table(file="processed/Fig6/Day0-1.CAST.sum.chr13", sep="\t")[ ,1])
Day0_2.chr13 <- sum(read.table(file="processed/Fig6/Day0-2.CAST.sum.chr13", sep="\t")[ ,1])
#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
Day0_1.auto <- sum(read.table(file="processed/Fig6/Day0-1.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/Day0-1.CAST.sum.auto", sep="\t")[ ,1])/19
Day0_2.auto <- sum(read.table(file="processed/Fig6/Day0-2.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/Day0-2.CAST.sum.auto", sep="\t")[ ,1])/19

Day0.chr13.normalized <- data.frame(data=c(Day0_1.chr13*1000000/total.ES.auto[1], Day0_2.chr13*1000000/total.ES.auto[2]), stage="Day0", chr="chr13")
Day0.auto.normalized <- data.frame(data=c(Day0_1.auto*1000000/total.ES.auto[1], Day0_2.auto*1000000/total.ES.auto[2]), stage="Day0",chr="Auto")

#Day1
#chr13
Day1_1.chr13 <- sum(read.table(file="processed/Fig6/Day1-1.CAST.sum.chr13", sep="\t")[ ,1])
Day1_2.chr13 <- sum(read.table(file="processed/Fig6/Day1-2.CAST.sum.chr13", sep="\t")[ ,1])
#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
Day1_1.auto <- sum(read.table(file="processed/Fig6/Day1-1.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/Day1-1.CAST.sum.auto", sep="\t")[ ,1])/19
Day1_2.auto <- sum(read.table(file="processed/Fig6/Day1-2.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/Day1-2.CAST.sum.auto", sep="\t")[ ,1])/19

Day1.chr13.normalized <- data.frame(data=c(Day1_1.chr13*1000000/total.ES.auto[3], Day1_2.chr13*1000000/total.ES.auto[4]), stage="Day1", chr="chr13")
Day1.auto.normalized <- data.frame(data=c(Day1_1.auto*1000000/total.ES.auto[3], Day1_2.auto*1000000/total.ES.auto[4]), stage="Day1",chr="Auto")

#Day2
#chr13
Day2_1.chr13 <- sum(read.table(file="processed/Fig6/Day2-1.CAST.sum.chr13", sep="\t")[ ,1])
Day2_2.chr13 <- sum(read.table(file="processed/Fig6/Day2-2.CAST.sum.chr13", sep="\t")[ ,1])
#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
Day2_1.auto <- sum(read.table(file="processed/Fig6/Day2-1.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/Day2-1.CAST.sum.auto", sep="\t")[ ,1])/19
Day2_2.auto <- sum(read.table(file="processed/Fig6/Day2-2.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/Day2-2.CAST.sum.auto", sep="\t")[ ,1])/19

Day2.chr13.normalized <- data.frame(data=c(Day2_1.chr13*1000000/total.ES.auto[5], Day2_2.chr13*1000000/total.ES.auto[6]), stage="Day2", chr="chr13")
Day2.auto.normalized <- data.frame(data=c(Day2_1.auto*1000000/total.ES.auto[5], Day2_2.auto*1000000/total.ES.auto[6]), stage="Day2",chr="Auto")

#Day3
#chr13
Day3_1.chr13 <- sum(read.table(file="processed/Fig6/Day3-1.CAST.sum.chr13", sep="\t")[ ,1])
Day3_2.chr13 <- sum(read.table(file="processed/Fig6/Day3-2.CAST.sum.chr13", sep="\t")[ ,1])
#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
Day3_1.auto <- sum(read.table(file="processed/Fig6/Day3-1.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/Day3-1.CAST.sum.auto", sep="\t")[ ,1])/19
Day3_2.auto <- sum(read.table(file="processed/Fig6/Day3-2.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/Day3-2.CAST.sum.auto", sep="\t")[ ,1])/19

Day3.chr13.normalized <- data.frame(data=c(Day3_1.chr13*1000000/total.ES.auto[7], Day3_2.chr13*1000000/total.ES.auto[8]), stage="Day3", chr="chr13")
Day3.auto.normalized <- data.frame(data=c(Day3_1.auto*1000000/total.ES.auto[7], Day3_2.auto*1000000/total.ES.auto[8]), stage="Day3",chr="Auto")

#Day4
#chr13
Day4_1.chr13 <- sum(read.table(file="processed/Fig6/Day4-1.CAST.sum.chr13", sep="\t")[ ,1])
Day4_2.chr13 <- sum(read.table(file="processed/Fig6/Day4-2.CAST.sum.chr13", sep="\t")[ ,1])
#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
Day4_1.auto <- sum(read.table(file="processed/Fig6/Day4-1.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/Day4-1.CAST.sum.auto", sep="\t")[ ,1])/19
Day4_2.auto <- sum(read.table(file="processed/Fig6/Day4-2.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/Day4-2.CAST.sum.auto", sep="\t")[ ,1])/19

Day4.chr13.normalized <- data.frame(data=c(Day4_1.chr13*1000000/total.ES.auto[9], Day4_2.chr13*1000000/total.ES.auto[10]), stage="Day4", chr="chr13")
Day4.auto.normalized <- data.frame(data=c(Day4_1.auto*1000000/total.ES.auto[9], Day4_2.auto*1000000/total.ES.auto[10]), stage="Day4",chr="Auto")

#Day6
#chr13
Day6_1.chr13 <- sum(read.table(file="processed/Fig6/Day6-1.CAST.sum.chr13", sep="\t")[ ,1])
Day6_2.chr13 <- sum(read.table(file="processed/Fig6/Day6-2.CAST.sum.chr13", sep="\t")[ ,1])
#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
Day6_1.auto <- sum(read.table(file="processed/Fig6/Day6-1.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/Day6-1.CAST.sum.auto", sep="\t")[ ,1])/19
Day6_2.auto <- sum(read.table(file="processed/Fig6/Day6-2.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/Day6-2.CAST.sum.auto", sep="\t")[ ,1])/19

Day6.chr13.normalized <- data.frame(data=c(Day6_1.chr13*1000000/total.ES.auto[11], Day6_2.chr13*1000000/total.ES.auto[12]), stage="Day6", chr="chr13")
Day6.auto.normalized <- data.frame(data=c(Day6_1.auto*1000000/total.ES.auto[11], Day6_2.auto*1000000/total.ES.auto[12]), stage="Day6",chr="Auto")

#Day8
#chr13
Day8_1.chr13 <- sum(read.table(file="processed/Fig6/Day8-1.CAST.sum.chr13", sep="\t")[ ,1])
Day8_2.chr13 <- sum(read.table(file="processed/Fig6/Day8-2.CAST.sum.chr13", sep="\t")[ ,1])
#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
Day8_1.auto <- sum(read.table(file="processed/Fig6/Day8-1.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/Day8-1.CAST.sum.auto", sep="\t")[ ,1])/19
Day8_2.auto <- sum(read.table(file="processed/Fig6/Day8-2.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/Day8-2.CAST.sum.auto", sep="\t")[ ,1])/19

Day8.chr13.normalized <- data.frame(data=c(Day8_1.chr13*1000000/total.ES.auto[13], Day8_2.chr13*1000000/total.ES.auto[14]), stage="Day8", chr="chr13")
Day8.auto.normalized <- data.frame(data=c(Day8_1.auto*1000000/total.ES.auto[13], Day8_2.auto*1000000/total.ES.auto[14]), stage="Day8",chr="Auto")

#Day10
#chr13
Day10_1.chr13 <- sum(read.table(file="processed/Fig6/Day10-1.CAST.sum.chr13", sep="\t")[ ,1])
Day10_2.chr13 <- sum(read.table(file="processed/Fig6/Day10-2.CAST.sum.chr13", sep="\t")[ ,1])
#auto (probably shoudl divide by 38, because I am comparing Xm with a single autosome, but the result would be the same, because it is just a ratio and eventually will be normalized.)
Day10_1.auto <- sum(read.table(file="processed/Fig6/Day10-1.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/Day10-1.CAST.sum.auto", sep="\t")[ ,1])/19
Day10_2.auto <- sum(read.table(file="processed/Fig6/Day10-2.REF.sum.auto", sep="\t")[ ,1])/19 + sum(read.table(file="processed/Fig6/Day10-2.CAST.sum.auto", sep="\t")[ ,1])/19

Day10.chr13.normalized <- data.frame(data=c(Day10_1.chr13*1000000/total.ES.auto[15], Day10_2.chr13*1000000/total.ES.auto[16]), stage="Day10", chr="chr13")
Day10.auto.normalized <- data.frame(data=c(Day10_1.auto*1000000/total.ES.auto[15], Day10_2.auto*1000000/total.ES.auto[16]), stage="Day10",chr="Auto")


#sum up

ES.Mat.chr13.dat <- rbind(Day0.chr13.normalized, Day1.chr13.normalized, Day2.chr13.normalized, Day3.chr13.normalized, Day4.chr13.normalized, Day6.chr13.normalized, Day8.chr13.normalized, Day10.chr13.normalized)
ES.Mat.auto.dat <- rbind(Day0.auto.normalized, Day1.auto.normalized, Day2.auto.normalized, Day3.auto.normalized, Day4.auto.normalized, Day6.auto.normalized, Day8.auto.normalized, Day10.auto.normalized)
#dat <- rbind(F.Mat.chr13.dat, F.Mat.auto.dat)

ES.Mat.chr13.dat$data <- ES.Mat.chr13.dat$data / median(Day0.chr13.normalized$data)
ES.Mat.auto.dat$data <- ES.Mat.auto.dat$data / median(Day0.auto.normalized$data)
ES.Mat.dat <- rbind(ES.Mat.chr13.dat, ES.Mat.auto.dat)

ES.Mat.dat$stage <- factor(ES.Mat.dat$stage, level=c("Day0","Day1","Day2","Day3","Day4","Day6","Day8","Day10"))



ggplot(ES.Mat.dat, aes(x=stage, y=data, fill=chr)) +
  geom_boxplot(outlier.shape=NA,alpha =0.9) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
             aes(fill=chr), size=1.3, pch=21,stroke = 0.4) +
  scale_fill_manual(values=c("#FF3300","chartreuse3")) +
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
#Day1 p=0.06711
Day1.13 <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day1"& ES.Mat.dat$chr=="chr13"), 1]
Day1.Auto <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day1"& ES.Mat.dat$chr=="Auto"), 1]
t.test(x=Day1.13, y=Day1.Auto, paired=T)

#Day2 p=0.04536
Day2.13 <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day2"& ES.Mat.dat$chr=="chr13"), 1]
Day2.Auto <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day2"& ES.Mat.dat$chr=="Auto"), 1]
t.test(x=Day2.13, y=Day2.Auto, paired=T)

#Day3 p=0.1706
Day3.13 <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day3"& ES.Mat.dat$chr=="chr13"), 1]
Day3.Auto <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day3"& ES.Mat.dat$chr=="Auto"), 1]
t.test(x=Day3.13, y=Day3.Auto, paired=T)

#Day4 p=0.1448
Day4.13 <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day4"& ES.Mat.dat$chr=="chr13"), 1]
Day4.Auto <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day4"& ES.Mat.dat$chr=="Auto"), 1]
t.test(x=Day4.13, y=Day4.Auto, paired=T)

#Day6 p=0.08287
Day6.13 <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day6"& ES.Mat.dat$chr=="chr13"), 1]
Day6.Auto <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day6"& ES.Mat.dat$chr=="Auto"), 1]
t.test(x=Day6.13, y=Day6.Auto, paired=T)

#Day8 p=0.3238
Day8.13 <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day8"& ES.Mat.dat$chr=="chr13"), 1]
Day8.Auto <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day8"& ES.Mat.dat$chr=="Auto"), 1]
t.test(x=Day8.13, y=Day8.Auto, paired=T)

#Day10 p=0.1398
Day10.13 <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day10"& ES.Mat.dat$chr=="chr13"), 1]
Day10.Auto <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day10"& ES.Mat.dat$chr=="Auto"), 1]
t.test(x=Day10.13, y=Day10.Auto, paired=T)










#Panel c. Test chr13 as a negative control in EScells, only looked at Day8 and Day10 to check Xa hyperactivation

rm(list=ls())
#------late2cell Rawdata-----------------
EScell.comp <- read.table(file="processed/EScell_geneCount.txt", sep="\t", header=T, row.names=1)[ ,c(1,38:53)]
colnames(EScell.comp) <- c("Chr","Day0.Comp1","Day0.Comp2","Day1.Comp1","Day1.Comp2",
                           "Day2.Comp1","Day2.Comp2","Day3.Comp1","Day3.Comp2",
                           "Day4.Comp1","Day4.Comp2","Day6.Comp1","Day6.Comp2",
                           "Day8.Comp1","Day8.Comp2","Day10.Comp1","Day10.Comp2")
EScell.comp.Auto <- EScell.comp[which(EScell.comp$Chr != "chrX" & EScell.comp$Chr != "chrY" ), -1]
total.ES.auto <- colSums(EScell.comp.Auto)

EScell.allelic <- read.table(file="processed/EScell_geneCount.txt", sep="\t", header=T, row.names=1)[ ,c(1,6:37)]
colnames(EScell.allelic) <- c("Chr","Day0.Mus1","Day0.Cas1","Day0.Mus2","Day0.Cas2",
                              "Day1.Mus1","Day1.Cas1","Day1.Mus2","Day1.Cas2",
                              "Day2.Mus1","Day2.Cas1","Day2.Mus2","Day2.Cas2",
                              "Day3.Mus1","Day3.Cas1","Day3.Mus2","Day3.Cas2",
                              "Day4.Mus1","Day4.Cas1","Day4.Mus2","Day4.Cas2",
                              "Day6.Mus1","Day6.Cas1","Day6.Mus2","Day6.Cas2",
                              "Day8.Mus1","Day8.Cas1","Day8.Mus2","Day8.Cas2",
                              "Day10.Mus1","Day10.Cas1","Day10.Mus2","Day10.Cas2")

auto.gene <- rownames(EScell.comp[which(EScell.comp$Chr != "chrX" & EScell.comp$Chr != "chrY" ), ])
chr13.gene <- rownames(EScell.comp[which(EScell.comp$Chr == "chr13"), ])

F.mus.All <- EScell.allelic[ ,c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32)]
F.cas.All <- EScell.allelic[ ,c(3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33)]

F.mus.chr13 <- colSums(F.mus.All[chr13.gene, ])
F.cas.chr13 <- colSums(F.cas.All[chr13.gene, ])
F.mus.auto <- colSums(F.mus.All[auto.gene, ])
F.cas.auto <- colSums(F.cas.All[auto.gene, ])

#Day0
Day0_1.chr13 <- F.cas.chr13[1]
Day0_2.chr13 <- F.cas.chr13[2]
Day0_1.auto <- F.mus.auto[1]/19 + F.cas.auto[1]/19
Day0_2.auto <- F.mus.auto[2]/19 + F.cas.auto[2]/19

Day0.chr13.normalized <- data.frame(data=c(Day0_1.chr13*1000000/total.ES.auto[1], Day0_2.chr13*1000000/total.ES.auto[2]), stage="Day0", chr="chr13", Gender="Female")
Day0.auto.normalized <- data.frame(data=c(Day0_1.auto*1000000/total.ES.auto[1], Day0_2.auto*1000000/total.ES.auto[2]), stage="Day0",chr="Auto", Gender="Female")

#Day1
Day1_1.chr13 <- F.cas.chr13[3]
Day1_2.chr13 <- F.cas.chr13[4]
Day1_1.auto <- F.mus.auto[3]/19 + F.cas.auto[3]/19
Day1_2.auto <- F.mus.auto[4]/19 + F.cas.auto[4]/19

Day1.chr13.normalized <- data.frame(data=c(Day1_1.chr13*1000000/total.ES.auto[3], Day1_2.chr13*1000000/total.ES.auto[4]), stage="Day1", chr="chr13", Gender="Female")
Day1.auto.normalized <- data.frame(data=c(Day1_1.auto*1000000/total.ES.auto[3], Day1_2.auto*1000000/total.ES.auto[4]), stage="Day1",chr="Auto", Gender="Female")

#Day2
Day2_1.chr13 <- F.cas.chr13[5]
Day2_2.chr13 <- F.cas.chr13[6]
Day2_1.auto <- F.mus.auto[5]/19 + F.cas.auto[5]/19
Day2_2.auto <- F.mus.auto[6]/19 + F.cas.auto[6]/19

Day2.chr13.normalized <- data.frame(data=c(Day2_1.chr13*1000000/total.ES.auto[5], Day2_2.chr13*1000000/total.ES.auto[6]), stage="Day2", chr="chr13", Gender="Female")
Day2.auto.normalized <- data.frame(data=c(Day2_1.auto*1000000/total.ES.auto[5], Day2_2.auto*1000000/total.ES.auto[6]), stage="Day2",chr="Auto", Gender="Female")

#Day3
Day3_1.chr13 <- F.cas.chr13[7]
Day3_2.chr13 <- F.cas.chr13[8]
Day3_1.auto <- F.mus.auto[7]/19 + F.cas.auto[7]/19
Day3_2.auto <- F.mus.auto[8]/19 + F.cas.auto[8]/19

Day3.chr13.normalized <- data.frame(data=c(Day3_1.chr13*1000000/total.ES.auto[7], Day3_2.chr13*1000000/total.ES.auto[8]), stage="Day3", chr="chr13", Gender="Female")
Day3.auto.normalized <- data.frame(data=c(Day3_1.auto*1000000/total.ES.auto[7], Day3_2.auto*1000000/total.ES.auto[8]), stage="Day3",chr="Auto", Gender="Female")

#Day4
Day4_1.chr13 <- F.cas.chr13[9]
Day4_2.chr13 <- F.cas.chr13[10]
Day4_1.auto <- F.mus.auto[9]/19 + F.cas.auto[9]/19
Day4_2.auto <- F.mus.auto[10]/19 + F.cas.auto[10]/19

Day4.chr13.normalized <- data.frame(data=c(Day4_1.chr13*1000000/total.ES.auto[9], Day4_2.chr13*1000000/total.ES.auto[10]), stage="Day4", chr="chr13", Gender="Female")
Day4.auto.normalized <- data.frame(data=c(Day4_1.auto*1000000/total.ES.auto[9], Day4_2.auto*1000000/total.ES.auto[10]), stage="Day4",chr="Auto", Gender="Female")

#Day6
Day6_1.chr13 <- F.cas.chr13[11]
Day6_2.chr13 <- F.cas.chr13[12]
Day6_1.auto <- F.mus.auto[11]/19 + F.cas.auto[11]/19
Day6_2.auto <- F.mus.auto[12]/19 + F.cas.auto[12]/19

Day6.chr13.normalized <- data.frame(data=c(Day6_1.chr13*1000000/total.ES.auto[11], Day6_2.chr13*1000000/total.ES.auto[12]), stage="Day6", chr="chr13", Gender="Female")
Day6.auto.normalized <- data.frame(data=c(Day6_1.auto*1000000/total.ES.auto[11], Day6_2.auto*1000000/total.ES.auto[12]), stage="Day6",chr="Auto", Gender="Female")

#Day8
Day8_1.chr13 <- F.cas.chr13[13]
Day8_2.chr13 <- F.cas.chr13[14]
Day8_1.auto <- F.mus.auto[13]/19 + F.cas.auto[13]/19
Day8_2.auto <- F.mus.auto[14]/19 + F.cas.auto[14]/19

Day8.chr13.normalized <- data.frame(data=c(Day8_1.chr13*1000000/total.ES.auto[13], Day8_2.chr13*1000000/total.ES.auto[14]), stage="Day8", chr="chr13", Gender="Female")
Day8.auto.normalized <- data.frame(data=c(Day8_1.auto*1000000/total.ES.auto[13], Day8_2.auto*1000000/total.ES.auto[14]), stage="Day8",chr="Auto", Gender="Female")

#Day10
Day10_1.chr13 <- F.cas.chr13[15]
Day10_2.chr13 <- F.cas.chr13[16]
Day10_1.auto <- F.mus.auto[15]/19 + F.cas.auto[15]/19
Day10_2.auto <- F.mus.auto[16]/19 + F.cas.auto[16]/19

Day10.chr13.normalized <- data.frame(data=c(Day10_1.chr13*1000000/total.ES.auto[15], Day10_2.chr13*1000000/total.ES.auto[16]), stage="Day10", chr="chr13", Gender="Female")
Day10.auto.normalized <- data.frame(data=c(Day10_1.auto*1000000/total.ES.auto[15], Day10_2.auto*1000000/total.ES.auto[16]), stage="Day10",chr="Auto", Gender="Female")



#sum up: Day8 and Day10

ES.Mat.chr13.dat <- rbind(Day8.chr13.normalized, Day10.chr13.normalized)
ES.Mat.auto.dat <- rbind(Day8.auto.normalized, Day10.auto.normalized)
#dat <- rbind(F.Mat.chr13.dat, F.Mat.auto.dat)

ES.Mat.chr13.dat$data <- ES.Mat.chr13.dat$data / median(Day0.chr13.normalized$data)
ES.Mat.auto.dat$data <- ES.Mat.auto.dat$data / median(Day0.auto.normalized$data)
ES.Mat.dat <- rbind(ES.Mat.chr13.dat, ES.Mat.auto.dat)

ES.Mat.dat$stage <- factor(ES.Mat.dat$stage, level=c("Day8","Day10"))


ggplot(ES.Mat.dat, aes(x=stage, y=data)) +
  geom_boxplot(fill="#3399FF", alpha =0.5)


ggplot(ES.Mat.dat, aes(x=stage, y=data, fill=chr)) +
  geom_boxplot(outlier.shape=NA,alpha =0.9) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
             aes(fill=chr), size=1.5, pch=21,stroke = 0.4) +
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

#p-value, paired t-test
#Day8 p-value = 0.1781
Day8.13 <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day8"& ES.Mat.dat$chr=="chr13"), 1]
Day8.Auto <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day8"& ES.Mat.dat$chr=="Auto"), 1]
t.test(x=Day8.13, y=Day8.Auto, paired=T)

#Day10 p=0.3245
Day10.13 <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day10"& ES.Mat.dat$chr=="chr13"), 1]
Day10.Auto <- ES.Mat.dat[which(ES.Mat.dat$stage=="Day10"& ES.Mat.dat$chr=="Auto"), 1]
t.test(x=Day10.13, y=Day10.Auto, paired=T)

