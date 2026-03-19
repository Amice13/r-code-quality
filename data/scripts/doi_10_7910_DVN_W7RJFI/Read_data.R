
##Assuming all data files are stored in "Data" directory
rm(list=ls())
#--Get geneInfo-----------------
GeneInfo.plus <- read.table(file="Attached_doc/GeneInfo.plus.txt", sep="\t", header=F, row.names=1)
colnames(GeneInfo.plus)=c("Chr","locus", "Strand","Length")
GeneInfo.minus <- read.table(file="Attached_doc/GeneInfo.minus.txt", sep="\t", header=F, row.names=1)
colnames(GeneInfo.minus)=c("Chr","locus", "Strand","Length")
GeneInfo <- rbind(GeneInfo.plus,GeneInfo.minus)


#MC cross
#------zygote Rawdata-----------------
zygote.allelic.RawCounts <- read.table(file="Data/zygote_MC.All_rep.allelic.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
zygote.comp.RawCounts <- read.table(file="Data/zygote_MC.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
zygote.RawCounts <- cbind(zygote.allelic.RawCounts, zygote.comp.RawCounts)
colnames(zygote.RawCounts) <- c("z.Mus1","z.Cas1","z.Mus2","z.Cas2","z.Mus3","z.Cas3",
                              "z.Mus4","z.Cas4","z.Comp1","z.Comp2","z.Comp3","z.Comp4")
zygote.mus <- c(1,3,5,7)
zygote.cas <- c(2,4,6,8)
zygote.comp <- c(9:12)

#------early2cell Rawdata-----------------
early2cell.allelic.RawCounts <- read.table(file="Data/early2C_MC.All_rep.allelic.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
early2cell.comp.RawCounts <- read.table(file="Data/early2C_MC.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
early2cell.RawCounts <- cbind(early2cell.allelic.RawCounts, early2cell.comp.RawCounts)
colnames(early2cell.RawCounts) <- c("E2.Mus1","E2.Cas1","E2.Mus2","E2.Cas2","E2.Mus3","E2.Cas3",
                                  "E2.Mus4","E2.Cas4","E2.Mus5","E2.Cas5","E2.Mus6","E2.Cas6",
                                  "E2.Comp1","E2.Comp2","E2.Comp3","E2.Comp4","E2.Comp5","E2.Comp6")
F.early2cell <- c(1,4,5)
M.early2cell <- c(2,3,6)
early2cell.mus <- c(1,3,5,7,9,11)
early2cell.cas <- c(2,4,6,8,10,12)
early2cell.comp <- c(13:18)

#------late2cell Rawdata-----------------
MC.late2cell.allelic.RawCounts <- read.table(file="Data/late2C_MC.All_rep.allelic.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
MC.late2cell.comp.RawCounts <- read.table(file="Data/late2C_MC.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
MC.late2cell.RawCounts <- cbind(MC.late2cell.allelic.RawCounts, MC.late2cell.comp.RawCounts)
colnames(MC.late2cell.RawCounts) <- c("L2.Mus1","L2.Cas1","L2.Mus2","L2.Cas2","L2.Mus3","L2.Cas3","L2.Mus4","L2.Cas4",
                                 "L2.Mus5","L2.Cas5","L2.Mus6","L2.Cas6","L2.Mus7","L2.Cas7",
                                 "L2.Mus8","L2.Cas8","L2.Mus9","L2.Cas9","L2.Mus10","L2.Cas10",
                                 "L2.Comp1","L2.Comp2","L2.Comp3","L2.Comp4","L2.Comp5",
                                 "L2.Comp6","L2.Comp7","L2.Comp8","L2.Comp9","L2.Comp10")
F.late2cell <- c(2,3,4,6,8)
M.late2cell <- c(1,5,7,9,10)
late2cell.mus <- c(1,3,5,7,9,11,13,15,17,19)
late2cell.cas <- c(2,4,6,8,10,12,14,16,18,20)
late2cell.comp <- c(21:30)

#------4cell Rawdata-----------------
MC.4cell.allelic.RawCounts <- read.table(file="Data/4C_MC.All_rep.allelic.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
MC.4cell.comp.RawCounts <- read.table(file="Data/4C_MC.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
MC.4cell.RawCounts <- cbind(MC.4cell.allelic.RawCounts, MC.4cell.comp.RawCounts)
colnames(MC.4cell.RawCounts) <- c("4.Mus1","4.Cas1","4.Mus2","4.Cas2","4.Mus3","4.Cas3","4.Mus4","4.Cas4",
                                  "4.Mus5","4.Cas5","4.Mus6","4.Cas6","4.Mus7","4.Cas7","4.Mus8","4.Cas8","4.Mus9","4.Cas9",
                                  "4.Mus10","4.Cas10","4.Mus11","4.Cas11","4.Mus12","4.Cas12","4.Mus13",
                                  "4.Cas13","4.Mus14","4.Cas14","4.Mus15","4.Cas15","4.Mus16","4.Cas16",
                                  "4.Mus17","4.Cas17","4.Comp1","4.Comp2","4.Comp3","4.Comp4","4.Comp5",
                                  "4.Comp6","4.Comp7","4.Comp8","4.Comp9","4.Comp10","4.Comp11","4.Comp12","4.Comp13",
                                  "4.Comp14","4.Comp15","4.Comp16","4.Comp17")
F.4cell <- c(4,6,11,14,15,16)
M.4cell <- c(1,2,3,5,7,8,9,10,12,13,17)
N4cell.mus <- c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33)
N4cell.cas <- c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34)
N4cell.comp <- c(35:51)

#------8cell Rawdata-----------------
MC.8cell.allelic.RawCounts <- read.table(file="Data/8C_MC.All_rep.allelic.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
MC.8cell.comp.RawCounts <- read.table(file="Data/8C_MC.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
MC.8cell.RawCounts <- cbind(MC.8cell.allelic.RawCounts, MC.8cell.comp.RawCounts)
colnames(MC.8cell.RawCounts) <- c("8.Mus1","8.Cas1","8.Mus2","8.Cas2","8.Mus3","8.Cas3","8.Mus4","8.Cas4",
                                  "8.Mus5","8.Cas5","8.Mus6","8.Cas6","8.Mus7","8.Cas7","8.Mus8","8.Cas8","8.Mus9","8.Cas9",
                                  "8.Mus10","8.Cas10","8.Mus11","8.Cas11","8.Mus12","8.Cas12","8.Mus13",
                                  "8.Cas13","8.Mus14","8.Cas14","8.Mus15","8.Cas15","8.Comp1","8.Comp2","8.Comp3","8.Comp4",
                                  "8.Comp5","8.Comp6","8.Comp7","8.Comp8","8.Comp9","8.Comp10","8.Comp11",
                                  "8.Comp12","8.Comp13","8.Comp14","8.Comp15")
F.8cell <- c(1,5,7,10,14,15)
M.8cell <- c(2,3,4,6,8,9,11,12,13)
N8cell.mus <- c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)
N8cell.cas <- c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30)
N8cell.comp <- c(31:45)

#------16cell Rawdata-----------------
MC.16cell.allelic.RawCounts <- read.table(file="Data/16C_MC.All_rep.allelic.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
MC.16cell.comp.RawCounts <- read.table(file="Data/16C_MC.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
MC.16cell.RawCounts <- cbind(MC.16cell.allelic.RawCounts, MC.16cell.comp.RawCounts)
colnames(MC.16cell.RawCounts) <- c("16.Mus1","16.Cas1","16.Mus2","16.Cas2","16.Mus3","16.Cas3","16.Mus4",
                                   "16.Cas4","16.Mus5","16.Cas5","16.Mus6","16.Cas6","16.Mus7","16.Cas7",
                                   "16.Mus8","16.Cas8","16.Comp1","16.Comp2","16.Comp3","16.Comp4",
                                   "16.Comp5","16.Comp6","16.Comp7","16.Comp8")
F.16cell <- c(2,3,5,6,7)
M.16cell <- c(1,4,8)
N16cell.mus <- c(1,3,5,7,9,11,13,15)
N16cell.cas <- c(2,4,6,8,10,12,14,16)
N16cell.comp <- c(17:24)

#------earlyBlast Rawdata-----------------
MC.earlyBlast.allelic.RawCounts <- read.table(file="Data/earlyB_MC.All_rep.allelic.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
MC.earlyBlast.comp.RawCounts <- read.table(file="Data/earlyB_MC.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
MC.earlyBlast.RawCounts <- cbind(MC.earlyBlast.allelic.RawCounts, MC.earlyBlast.comp.RawCounts)
colnames(MC.earlyBlast.RawCounts) <- c("eB.Mus1","eB.Cas1","eB.Mus2","eB.Cas2","eB.Mus3","eB.Cas3","eB.Mus4",
                                       "eB.Cas4","eB.Mus5","eB.Cas5","eB.Mus6","eB.Cas6","eB.Mus7","eB.Cas7",
                                       "eB.Mus8","eB.Cas8","eB.Mus9","eB.Cas9","eB.Mus10","eB.Cas10",
                                       "eB.Mus11","eB.Cas11","eB.Comp1","eB.Comp2","eB.Comp3","eB.Comp4",
                                       "eB.Comp5","eB.Comp6","eB.Comp7","eB.Comp8","eB.Comp9","eB.Comp10",
                                       "eB.Comp11")
F.earlyBlast <- c(1,4,5,8,10)
M.earlyBlast <- c(2,3,6,7,9,11)
earlyBlast.mus <- c(1,3,5,7,9,11,13,15,17,19,21)
earlyBlast.cas <- c(2,4,6,8,10,12,14,16,18,20,22)
earlyBlast.comp <- c(23:33)



MC.comp.All <- cbind(zygote.RawCounts[ ,zygote.comp], early2cell.RawCounts[ ,early2cell.comp],
                     MC.late2cell.RawCounts[ ,late2cell.comp], MC.4cell.RawCounts[ ,N4cell.comp], 
                     MC.8cell.RawCounts[ ,N8cell.comp], MC.16cell.RawCounts[ ,N16cell.comp], 
                     MC.earlyBlast.RawCounts[ ,earlyBlast.comp])

MC.F.comp.All <- cbind(zygote.RawCounts[ ,zygote.comp],
                       early2cell.RawCounts[ ,early2cell.comp][ ,F.early2cell],
                       MC.late2cell.RawCounts[ ,late2cell.comp][ ,F.late2cell],
                       MC.4cell.RawCounts[ ,N4cell.comp][ ,F.4cell],
                       MC.8cell.RawCounts[ ,N8cell.comp][ ,F.8cell],
                       MC.16cell.RawCounts[ ,N16cell.comp][ ,F.16cell],
                       MC.earlyBlast.RawCounts[ ,earlyBlast.comp][ ,F.earlyBlast])
MC.M.comp.All <- cbind(zygote.RawCounts[ ,zygote.comp],
                       early2cell.RawCounts[ ,early2cell.comp][ ,M.early2cell],
                       MC.late2cell.RawCounts[ ,late2cell.comp][ ,M.late2cell],
                       MC.4cell.RawCounts[ ,N4cell.comp][ ,M.4cell],
                       MC.8cell.RawCounts[ ,N8cell.comp][ ,M.8cell],
                       MC.16cell.RawCounts[ ,N16cell.comp][ ,M.16cell],
                       MC.earlyBlast.RawCounts[ ,earlyBlast.comp][ ,M.earlyBlast])
MC.F.mus.All <- cbind(zygote.RawCounts[ ,zygote.mus],
                      early2cell.RawCounts[ ,early2cell.mus][ ,F.early2cell],
                      MC.late2cell.RawCounts[ ,late2cell.mus][ ,F.late2cell],
                      MC.4cell.RawCounts[ ,N4cell.mus][ ,F.4cell],
                      MC.8cell.RawCounts[ ,N8cell.mus][ ,F.8cell],
                      MC.16cell.RawCounts[ ,N16cell.mus][ ,F.16cell],
                      MC.earlyBlast.RawCounts[ ,earlyBlast.mus][ ,F.earlyBlast])
MC.F.cas.All <- cbind(zygote.RawCounts[ ,zygote.cas],
                      early2cell.RawCounts[ ,early2cell.cas][ ,F.early2cell],
                      MC.late2cell.RawCounts[ ,late2cell.cas][ ,F.late2cell],
                      MC.4cell.RawCounts[ ,N4cell.cas][ ,F.4cell],
                      MC.8cell.RawCounts[ ,N8cell.cas][ ,F.8cell],
                      MC.16cell.RawCounts[ ,N16cell.cas][ ,F.16cell],
                      MC.earlyBlast.RawCounts[ ,earlyBlast.cas][ ,F.earlyBlast])
MC.M.mus.All <- cbind(zygote.RawCounts[ ,zygote.mus],
                      early2cell.RawCounts[ ,early2cell.mus][ ,M.early2cell],
                      MC.late2cell.RawCounts[ ,late2cell.mus][ ,M.late2cell],
                      MC.4cell.RawCounts[ ,N4cell.mus][ ,M.4cell],
                      MC.8cell.RawCounts[ ,N8cell.mus][ ,M.8cell],
                      MC.16cell.RawCounts[ ,N16cell.mus][ ,M.16cell],
                      MC.earlyBlast.RawCounts[ ,earlyBlast.mus][ ,M.earlyBlast])
MC.M.cas.All <- cbind(zygote.RawCounts[ ,zygote.cas],
                      early2cell.RawCounts[ ,early2cell.cas][ ,M.early2cell],
                      MC.late2cell.RawCounts[ ,late2cell.cas][ ,M.late2cell],
                      MC.4cell.RawCounts[ ,N4cell.cas][ ,M.4cell],
                      MC.8cell.RawCounts[ ,N8cell.cas][ ,M.8cell],
                      MC.16cell.RawCounts[ ,N16cell.cas][ ,M.16cell],
                      MC.earlyBlast.RawCounts[ ,earlyBlast.cas][ ,M.earlyBlast])


MC.F.Allelic.All <- MC.F.mus.All + MC.F.cas.All
MC.M.Allelic.All <- MC.M.mus.All + MC.M.cas.All


#calculate RPKM
LibSize <- colSums(MC.comp.All, na.rm = T) / 1000000  #Per million 
GeneLength <- GeneInfo[match(rownames(MC.comp.All), rownames(GeneInfo)),"Length"] / 1000
MC.comp.RPKM <- MC.comp.All[ ,1, drop=F] / (LibSize[1] * GeneLength)
for (i in 2:ncol(MC.comp.All)){
  MC.comp.RPKM[,i] <- MC.comp.All[ ,i] / (LibSize[i] * GeneLength)
  colnames(MC.comp.RPKM)[i] <- colnames(MC.comp.All)[i]
}

MC.F.comp.RPKM <- MC.comp.RPKM[ ,colnames(MC.F.comp.All)]
MC.M.comp.RPKM <- MC.comp.RPKM[ ,colnames(MC.M.comp.All)]


rm(list=setdiff(ls(), c("GeneInfo","MC.M.Allelic.All","MC.F.Allelic.All","MC.M.mus.All","MC.F.mus.All",
                        "MC.M.cas.All","MC.F.cas.All","MC.comp.All","MC.F.comp.All", "MC.M.comp.All",
                        "MC.comp.RPKM","MC.F.comp.RPKM", "MC.M.comp.RPKM")))



#Reciprocal CM crosses
#------late2cell Rawdata-----------------
CM.late2cell.allelic.RawCounts <- read.table(file="Data/late2C_CM.All_rep.allelic.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
CM.late2cell.comp.RawCounts <- read.table(file="Data/late2C_CM.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
CM.late2cell.RawCounts <- cbind(CM.late2cell.allelic.RawCounts, CM.late2cell.comp.RawCounts)
colnames(CM.late2cell.RawCounts) <- c("L2.Mus1","L2.Cas1","L2.Mus2","L2.Cas2","L2.Mus3","L2.Cas3","L2.Mus4",
                                      "L2.Cas4","L2.Mus5","L2.Cas5","L2.Mus6","L2.Cas6","L2.Mus7","L2.Cas7",
                                      "L2.Mus8","L2.Cas8","L2.Mus9","L2.Cas9","L2.Mus10","L2.Cas10","L2.Comp1",
                                      "L2.Comp2","L2.Comp3","L2.Comp4","L2.Comp5","L2.Comp6","L2.Comp7","L2.Comp8",
                                      "L2.Comp9","L2.Comp10")
F.late2cell <- c(1,2,3,5,8)
M.late2cell <- c(4,6,7,9,10)
late2cell.mus <- c(1,3,5,7,9,11,13,15,17,19)
late2cell.cas <- c(2,4,6,8,10,12,14,16,18,20)
late2cell.comp <- c(21:30)
#------N4cell Rawdata-----------------
CM.4cell.allelic.RawCounts <- read.table(file="Data/4C_CM.All_rep.allelic.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
CM.4cell.comp.RawCounts <- read.table(file="Data/4C_CM.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
CM.4cell.RawCounts <- cbind(CM.4cell.allelic.RawCounts, CM.4cell.comp.RawCounts)
colnames(CM.4cell.RawCounts) <- c("4.Mus1","4.Cas1","4.Mus2","4.Cas2","4.Mus3","4.Cas3","4.Mus4",
                                  "4.Cas4","4.Mus5","4.Cas5","4.Mus6","4.Cas6","4.Mus7","4.Cas7",
                                  "4.Mus8","4.Cas8","4.Comp1","4.Comp2","4.Comp3","4.Comp4","4.Comp5",
                                  "4.Comp6","4.Comp7","4.Comp8")
F.4cell <- c(1,2,3,4,5)
M.4cell <- c(6,7,8)
N4cell.mus <- c(1,3,5,7,9,11,13,15)
N4cell.cas <- c(2,4,6,8,10,12,14,16)
N4cell.comp <- c(17:24)
#------N8cell Rawdata-----------------
CM.8cell.allelic.RawCounts <- read.table(file="Data/8C_CM.All_rep.allelic.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
CM.8cell.comp.RawCounts <- read.table(file="Data/8C_CM.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
CM.8cell.RawCounts <- cbind(CM.8cell.allelic.RawCounts, CM.8cell.comp.RawCounts)
colnames(CM.8cell.RawCounts) <- c("8.Mus1","8.Cas1","8.Mus2","8.Cas2","8.Mus3","8.Cas3","8.Mus4",
                                  "8.Cas4","8.Mus5","8.Cas5","8.Mus6","8.Cas6","8.Mus7","8.Cas7",
                                  "8.Mus8","8.Cas8","8.Mus9","8.Cas9","8.Mus10","8.Cas10","8.Mus11",
                                  "8.Cas11","8.Comp1","8.Comp2","8.Comp3","8.Comp4","8.Comp5","8.Comp6",
                                  "8.Comp7","8.Comp8","8.Comp9","8.Comp10","8.Comp11")
F.8cell <- c(1,2,5,6,7,8,9,10,11)
M.8cell <- c(3,4)
N8cell.mus <- c(1,3,5,7,9,11,13,15,17,19,21)
N8cell.cas <- c(2,4,6,8,10,12,14,16,18,20,22)
N8cell.comp <- c(23:33)
#------N16cell Rawdata-----------------
CM.16cell.allelic.RawCounts <- read.table(file="Data/16C_CM.All_rep.allelic.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
CM.16cell.comp.RawCounts <- read.table(file="Data/16C_CM.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
CM.16cell.RawCounts <- cbind(CM.16cell.allelic.RawCounts, CM.16cell.comp.RawCounts)
colnames(CM.16cell.RawCounts) <- c("16.Mus1","16.Cas1","16.Mus2","16.Cas2","16.Mus3","16.Cas3","16.Mus4",
                                   "16.Cas4","16.Mus5","16.Cas5","16.Mus6","16.Cas6","16.Mus7","16.Cas7",
                                   "16.Mus8","16.Cas8","16.Mus9","16.Cas9","16.Mus10","16.Cas10","16.Mus11","16.Cas11","16.Mus12",
                                   "16.Cas12","16.Mus13","16.Cas13","16.Mus14","16.Cas14",
                                   "16.Comp1","16.Comp2","16.Comp3","16.Comp4","16.Comp5","16.Comp6",
                                   "16.Comp7","16.Comp8","16.Comp9","16.Comp10","16.Comp11","16.Comp12","16.Comp13",
                                   "16.Comp14")
F.16cell <- c(1,2,6,9,12,13,14)
M.16cell <- c(3,4,5,7,8,10,11)
N16cell.mus <- c(1,3,5,7,9,11,13,15,17,19,21,23,25,27)
N16cell.cas <- c(2,4,6,8,10,12,14,16,18,20,22,24,26,28)
N16cell.comp <- c(29:42)
#------earlyBlast Rawdata-----------------
CM.earlyBlast.allelic.RawCounts <- read.table(file="Data/earlyB_CM.All_rep.allelic.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
CM.earlyBlast.comp.RawCounts <- read.table(file="Data/earlyB_CM.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
CM.earlyBlast.RawCounts <- cbind(CM.earlyBlast.allelic.RawCounts, CM.earlyBlast.comp.RawCounts)
colnames(CM.earlyBlast.RawCounts) <- c("eB.Mus1","eB.Cas1","eB.Mus2","eB.Cas2","eB.Mus3","eB.Cas3","eB.Mus4",
                                       "eB.Cas4","eB.Mus5","eB.Cas5","eB.Mus6","eB.Cas6","eB.Mus7","eB.Cas7",
                                       "eB.Mus8","eB.Cas8","eB.Mus9","eB.Cas9","eB.Mus10","eB.Cas10","eB.Mus11","eB.Cas11","eB.Mus12","eB.Cas12",
                                       "eB.Mus13","eB.Cas13",
                                       "eB.Comp1","eB.Comp2","eB.Comp3","eB.Comp4","eB.Comp5","eB.Comp6",
                                       "eB.Comp7","eB.Comp8","eB.Comp9","eB.Comp10","eB.Comp11","eB.Comp12","eB.Comp13")
F.earlyBlast <- c(3,6,9,11,13)
M.earlyBlast <- c(1,2,4,5,7,8,10,12)
earlyBlast.mus <- c(1,3,5,7,9,11,13,15,17,19,21,23,25)
earlyBlast.cas <- c(2,4,6,8,10,12,14,16,18,20,22,24,26)
earlyBlast.comp <- c(27:39)


CM.comp.All <- cbind(CM.late2cell.RawCounts[ ,late2cell.comp], 
                     CM.4cell.RawCounts[ ,N4cell.comp], 
                     CM.8cell.RawCounts[ ,N8cell.comp], 
                     CM.16cell.RawCounts[ ,N16cell.comp], 
                     CM.earlyBlast.RawCounts[ ,earlyBlast.comp])

CM.F.comp.All <- cbind(CM.late2cell.RawCounts[ ,late2cell.comp][ ,F.late2cell],
                       CM.4cell.RawCounts[ ,N4cell.comp][ ,F.4cell],
                       CM.8cell.RawCounts[ ,N8cell.comp][ ,F.8cell],
                       CM.16cell.RawCounts[ ,N16cell.comp][ ,F.16cell],
                       CM.earlyBlast.RawCounts[ ,earlyBlast.comp][ ,F.earlyBlast])
CM.M.comp.All <- cbind(CM.late2cell.RawCounts[ ,late2cell.comp][ ,M.late2cell],
                       CM.4cell.RawCounts[ ,N4cell.comp][ ,M.4cell],
                       CM.8cell.RawCounts[ ,N8cell.comp][ ,M.8cell],
                       CM.16cell.RawCounts[ ,N16cell.comp][ ,M.16cell],
                       CM.earlyBlast.RawCounts[ ,earlyBlast.comp][ ,M.earlyBlast])

CM.F.mus.All <- cbind(CM.late2cell.RawCounts[ ,late2cell.mus][ ,F.late2cell],
                      CM.4cell.RawCounts[ ,N4cell.mus][ ,F.4cell],
                      CM.8cell.RawCounts[ ,N8cell.mus][ ,F.8cell],
                      CM.16cell.RawCounts[ ,N16cell.mus][ ,F.16cell],
                      CM.earlyBlast.RawCounts[ ,earlyBlast.mus][ ,F.earlyBlast])

CM.F.cas.All <- cbind(CM.late2cell.RawCounts[ ,late2cell.cas][ ,F.late2cell],
                      CM.4cell.RawCounts[ ,N4cell.cas][ ,F.4cell],
                      CM.8cell.RawCounts[ ,N8cell.cas][ ,F.8cell],
                      CM.16cell.RawCounts[ ,N16cell.cas][ ,F.16cell],
                      CM.earlyBlast.RawCounts[ ,earlyBlast.cas][ ,F.earlyBlast])

CM.M.mus.All <- cbind(CM.late2cell.RawCounts[ ,late2cell.mus][ ,M.late2cell],
                      CM.4cell.RawCounts[ ,N4cell.mus][ ,M.4cell],
                      CM.8cell.RawCounts[ ,N8cell.mus][ ,M.8cell],
                      CM.16cell.RawCounts[ ,N16cell.mus][ ,M.16cell],
                      CM.earlyBlast.RawCounts[ ,earlyBlast.mus][ ,M.earlyBlast])


CM.M.cas.All <- cbind(CM.late2cell.RawCounts[ ,late2cell.cas][ ,M.late2cell],
                      CM.4cell.RawCounts[ ,N4cell.cas][ ,M.4cell],
                      CM.8cell.RawCounts[ ,N8cell.cas][ ,M.8cell],
                      CM.16cell.RawCounts[ ,N16cell.cas][ ,M.16cell],
                      CM.earlyBlast.RawCounts[ ,earlyBlast.cas][ ,M.earlyBlast])

CM.F.Allelic.All <- CM.F.mus.All + CM.F.cas.All
CM.M.Allelic.All <- CM.M.mus.All + CM.M.cas.All

#calculate RPKM
LibSize <- colSums(CM.comp.All, na.rm = T) / 1000000  #Per million 
GeneLength <- GeneInfo[match(rownames(CM.comp.All), rownames(GeneInfo)),"Length"] / 1000
CM.comp.RPKM <- CM.comp.All[ ,1, drop=F] / (LibSize[1] * GeneLength)
for (i in 2:ncol(CM.comp.All)){
  CM.comp.RPKM[,i] <- CM.comp.All[ ,i] / (LibSize[i] * GeneLength)
  colnames(CM.comp.RPKM)[i] <- colnames(CM.comp.All)[i]
}

CM.F.comp.RPKM <- CM.comp.RPKM[ ,colnames(CM.F.comp.All)]
CM.M.comp.RPKM <- CM.comp.RPKM[ ,colnames(CM.M.comp.All)]



rm(list=setdiff(ls(), c("GeneInfo",
                        
                        "MC.M.Allelic.All","MC.F.Allelic.All","MC.M.mus.All","MC.F.mus.All",
                        "MC.M.cas.All","MC.F.cas.All","MC.comp.All","MC.F.comp.All", "MC.M.comp.All",
                        "MC.comp.RPKM","MC.F.comp.RPKM", "MC.M.comp.RPKM",
                        
                        "CM.M.Allelic.All","CM.F.Allelic.All","CM.M.mus.All","CM.F.mus.All",
                        "CM.M.cas.All","CM.F.cas.All","CM.comp.All","CM.F.comp.All", "CM.M.comp.All",
                        "CM.comp.RPKM","CM.F.comp.RPKM", "CM.M.comp.RPKM")))



#Xist KO embryos
#------late2cell Rawdata-----------------
KO.late2cell.allelic.RawCounts <- read.table(file="Data/late2C_KO.All_rep.allelic.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
KO.late2cell.comp.RawCounts <- read.table(file="Data/late2C_KO.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
KO.late2cell.RawCounts <- cbind(KO.late2cell.allelic.RawCounts, KO.late2cell.comp.RawCounts)
colnames(KO.late2cell.RawCounts) <- c("L2.Mus1","L2.Cas1","L2.Mus2","L2.Cas2",
                                    "L2.Mus3","L2.Cas3","L2.Mus4","L2.Cas4",
                                    "L2.Mus5","L2.Cas5","L2.Mus6","L2.Cas6",
                                    "L2.Mus7","L2.Cas7","L2.Mus8","L2.Cas8",
                                    "L2.Mus9","L2.Cas9","L2.Mus10","L2.Cas10",
                                    "L2.Mus11","L2.Cas11","L2.Mus12","L2.Cas12",
                                    "L2.Mus13","L2.Cas13","L2.Mus14","L2.Cas14",
                                    "L2.Mus15","L2.Cas15","L2.Mus16","L2.Cas16",
                                    "L2.Mus17","L2.Cas17","L2.Mus18","L2.Cas18",
                                    "L2.Mus19","L2.Cas19","L2.Mus20","L2.Cas20",
                                    "L2.Mus21","L2.Cas21","L2.Mus22","L2.Cas22",
                                    "L2.Mus23","L2.Cas23","L2.Comp1","L2.Comp2",
                                    "L2.Comp3","L2.Comp4","L2.Comp5","L2.Comp6",
                                    "L2.Comp7","L2.Comp8","L2.Comp9","L2.Comp10",
                                    "L2.Comp11","L2.Comp12","L2.Comp13","L2.Comp14",
                                    "L2.Comp15","L2.Comp16","L2.Comp17","L2.Comp18",
                                    "L2.Comp19","L2.Comp20","L2.Comp21","L2.Comp22","L2.Comp23")
F.late2cell <- c(7,13,17,18,19,22)
M.late2cell <- c(1:6,8:12,14,15,16,20,21,23)
late2cell.mus <- c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45)
late2cell.cas <- c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46)
late2cell.comp <- c(47:69)
#------4cell Rawdata-----------------
KO.4cell.allelic.RawCounts <- read.table(file="Data/4C_KO.All_rep.allelic.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
KO.4cell.comp.RawCounts <- read.table(file="Data/4C_KO.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
KO.4cell.RawCounts <- cbind(KO.4cell.allelic.RawCounts, KO.4cell.comp.RawCounts)
colnames(KO.4cell.RawCounts) <- c("4.Mus1","4.Cas1","4.Mus2","4.Cas2",
                                 "4.Mus3","4.Cas3","4.Mus4","4.Cas4",
                                 "4.Mus5","4.Cas5","4.Mus6","4.Cas6",
                                 "4.Mus7","4.Cas7","4.Mus8","4.Cas8",
                                 "4.Mus9","4.Cas9","4.Mus10","4.Cas10",
                                 "4.Mus11","4.Cas11","4.Mus12","4.Cas12",
                                 "4.Mus13","4.Cas13","4.Comp1","4.Comp2",
                                 "4.Comp3","4.Comp4","4.Comp5","4.Comp6",
                                 "4.Comp7","4.Comp8","4.Comp9","4.Comp10",
                                 "4.Comp11","4.Comp12","4.Comp13")
F.4cell <- c(4,7,8,9,11,13)
M.4cell <- c(1,2,3,5,6,10,12)
N4cell.mus <- c(1,3,5,7,9,11,13,15,17,19,21,23,25)
N4cell.cas <- c(2,4,6,8,10,12,14,16,18,20,22,24,26)
N4cell.comp <- c(27:39)
#------8cell Rawdata -----------------
KO.8cell.allelic.RawCounts <- read.table(file="Data/8C_KO.All_rep.allelic.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
KO.8cell.comp.RawCounts <- read.table(file="Data/8C_KO.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
KO.8cell.RawCounts <- cbind(KO.8cell.allelic.RawCounts, KO.8cell.comp.RawCounts)
colnames(KO.8cell.RawCounts) <- c("8.Mus1","8.Cas1",
                                 "8.Mus2","8.Cas2","8.Mus3","8.Cas3",
                                 "8.Mus4","8.Cas4","8.Mus5","8.Cas5",
                                 "8.Mus6","8.Cas6","8.Mus7","8.Cas7",
                                 "8.Mus8","8.Cas8","8.Mus9","8.Cas9",
                                 "8.Mus10","8.Cas10","8.Mus11","8.Cas11",
                                 "8.Mus12","8.Cas12","8.Mus13","8.Cas13",
                                 "8.Comp1","8.Comp2",
                                 "8.Comp3","8.Comp4","8.Comp5","8.Comp6",
                                 "8.Comp7","8.Comp8","8.Comp9","8.Comp10",
                                 "8.Comp11","8.Comp12","8.Comp13")
F.8cell <- c(1,5,7,10,11,13)
M.8cell <- c(2,3,4,6,8,9,12)
N8cell.mus <- c(1,3,5,7,9,11,13,15,17,19,21,23,25)
N8cell.cas <- c(2,4,6,8,10,12,14,16,18,20,22,24,26)
N8cell.comp <- c(27:39)
#------earlyBlast Rawdata-----------------
KO.earlyBlast.allelic.RawCounts <- read.table(file="Data/earlyB_KO.All_rep.allelic.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
KO.earlyBlast.comp.RawCounts <- read.table(file="Data/earlyB_KO.All_rep.total.geneCount.txt", sep="\t", header=T, row.names=1)[ ,-1]
KO.earlyBlast.RawCounts <- cbind(KO.earlyBlast.allelic.RawCounts, KO.earlyBlast.comp.RawCounts)
colnames(KO.earlyBlast.RawCounts) <- c("eB.Mus1","eB.Cas1","eB.Mus2","eB.Cas2",
                                     "eB.Mus3","eB.Cas3","eB.Mus4","eB.Cas4",
                                     "eB.Mus5","eB.Cas5","eB.Mus6","eB.Cas6",
                                     "eB.Mus7","eB.Cas7","eB.Mus8","eB.Cas8",
                                     "eB.Mus9","eB.Cas9","eB.Mus10","eB.Cas10",
                                     "eB.Mus11","eB.Cas11","eB.Mus12","eB.Cas12",
                                     "eB.Comp1","eB.Comp2","eB.Comp3","eB.Comp4",
                                     "eB.Comp5","eB.Comp6","eB.Comp7","eB.Comp8",
                                     "eB.Comp9","eB.Comp10","eB.Comp11","eB.Comp12")
F.earlyBlast <- c(2,3,4,9:12)
M.earlyBlast <- c(1,5,6,7,8)
earlyBlast.mus <- c(1,3,5,7,9,11,13,15,17,19,21,23)
earlyBlast.cas <- c(2,4,6,8,10,12,14,16,18,20,22,24)
earlyBlast.comp <- c(25:36)



KO.comp.All <- cbind(KO.late2cell.RawCounts[ ,late2cell.comp], 
                     KO.4cell.RawCounts[ ,N4cell.comp], 
                     KO.8cell.RawCounts[ ,N8cell.comp], 
                     KO.earlyBlast.RawCounts[ ,earlyBlast.comp])

KO.F.comp.All <- cbind(KO.late2cell.RawCounts[ ,late2cell.comp][ ,F.late2cell],
                       KO.4cell.RawCounts[ ,N4cell.comp][ ,F.4cell],
                       KO.8cell.RawCounts[ ,N8cell.comp][ ,F.8cell],
                       KO.earlyBlast.RawCounts[ ,earlyBlast.comp][ ,F.earlyBlast])

KO.M.comp.All <- cbind(KO.late2cell.RawCounts[ ,late2cell.comp][ ,M.late2cell], 
                       KO.4cell.RawCounts[ ,N4cell.comp][ ,M.4cell],
                       KO.8cell.RawCounts[ ,N8cell.comp][ ,M.8cell],
                       KO.earlyBlast.RawCounts[ ,earlyBlast.comp][ ,M.earlyBlast])

KO.F.mus.All <- cbind(KO.late2cell.RawCounts[ ,late2cell.mus][ ,F.late2cell], 
                      KO.4cell.RawCounts[ ,N4cell.mus][ ,F.4cell],
                      KO.8cell.RawCounts[ ,N8cell.mus][ ,F.8cell],
                      KO.earlyBlast.RawCounts[ ,earlyBlast.mus][ ,F.earlyBlast])

KO.F.cas.All <- cbind(KO.late2cell.RawCounts[ ,late2cell.cas][ ,F.late2cell], 
                      KO.4cell.RawCounts[ ,N4cell.cas][ ,F.4cell], 
                      KO.8cell.RawCounts[ ,N8cell.cas][ ,F.8cell], 
                      KO.earlyBlast.RawCounts[ ,earlyBlast.cas][ ,F.earlyBlast])

KO.M.mus.All <- cbind(KO.late2cell.RawCounts[ ,late2cell.mus][ ,M.late2cell], 
                      KO.4cell.RawCounts[ ,N4cell.mus][ ,M.4cell], 
                      KO.8cell.RawCounts[ ,N8cell.mus][ ,M.8cell], 
                      KO.earlyBlast.RawCounts[ ,earlyBlast.mus][ ,M.earlyBlast])

KO.M.cas.All <- cbind(KO.late2cell.RawCounts[ ,late2cell.cas][ ,M.late2cell], 
                      KO.4cell.RawCounts[ ,N4cell.cas][ ,M.4cell],
                      KO.8cell.RawCounts[ ,N8cell.cas][ ,M.8cell],
                      KO.earlyBlast.RawCounts[ ,earlyBlast.cas][ ,M.earlyBlast])


KO.F.Allelic.All <- KO.F.mus.All + KO.F.cas.All
KO.M.Allelic.All <- KO.M.mus.All + KO.M.cas.All

#calculate RPKM
LibSize <- colSums(KO.comp.All, na.rm = T) / 1000000  #Per million 
GeneLength <- GeneInfo[match(rownames(KO.comp.All), rownames(GeneInfo)),"Length"] / 1000
KO.comp.RPKM <- KO.comp.All[ ,1, drop=F] / (LibSize[1] * GeneLength)
for (i in 2:ncol(KO.comp.All)){
  KO.comp.RPKM[,i] <- KO.comp.All[ ,i] / (LibSize[i] * GeneLength)
  colnames(KO.comp.RPKM)[i] <- colnames(KO.comp.All)[i]
}

KO.F.comp.RPKM <- KO.comp.RPKM[ ,colnames(KO.F.comp.All)]
KO.M.comp.RPKM <- KO.comp.RPKM[ ,colnames(KO.M.comp.All)]

rm(list=setdiff(ls(), c("GeneInfo",
                        
                        "CM.M.Allelic.All","CM.F.Allelic.All","CM.M.mus.All","CM.F.mus.All",
                        "CM.M.cas.All","CM.F.cas.All","CM.comp.All","CM.F.comp.All", "CM.M.comp.All",
                        "CM.comp.RPKM","CM.F.comp.RPKM", "CM.M.comp.RPKM",
                        
                        "MC.M.Allelic.All","MC.F.Allelic.All","MC.M.mus.All","MC.F.mus.All",
                        "MC.M.cas.All","MC.F.cas.All","MC.comp.All","MC.F.comp.All", "MC.M.comp.All",
                        "MC.comp.RPKM","MC.F.comp.RPKM", "MC.M.comp.RPKM",
                        
                        "KO.M.Allelic.All","KO.F.Allelic.All","KO.M.mus.All","KO.F.mus.All",
                        "KO.M.cas.All","KO.F.cas.All","KO.comp.All","KO.F.comp.All", "KO.M.comp.All",
                        "KO.comp.RPKM","KO.F.comp.RPKM", "KO.M.comp.RPKM")))
