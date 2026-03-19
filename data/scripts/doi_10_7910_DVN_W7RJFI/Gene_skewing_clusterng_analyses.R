
#This is used to generate the skewing matrix for clustering analysis.
source("Read_data.R")
rm(list=setdiff(ls(), c("GeneInfo",
                        "CM.F.Allelic.All","CM.F.mus.All","CM.F.cas.All",
                        "MC.F.Allelic.All","MC.F.mus.All","MC.F.cas.All",
                        "KO.F.Allelic.All","KO.F.mus.All","KO.F.cas.All")))

chrXGenes <- rownames(GeneInfo[which(GeneInfo$Chr == "chrX"), ])

#skew is calculated as paternal ratio in total allelic reads.

#MC cross 
MC.skew.All <- MC.F.cas.All / MC.F.Allelic.All
MC.skew.All.l2C <- MC.skew.All[ ,c(8:12)]
MC.skew.All.N4C <- MC.skew.All[ ,c(13:18)]
MC.skew.All.N8C <- MC.skew.All[ ,c(19:24)]
MC.skew.All.N16C <- MC.skew.All[ ,c(25:29)]
MC.skew.All.eB <- MC.skew.All[ ,c(30:34)]


MC.Allelic.All.l2C <- MC.F.Allelic.All[ ,c(8:12)]
MC.Allelic.All.N4C <- MC.F.Allelic.All[ ,c(13:18)]
MC.Allelic.All.N8C <- MC.F.Allelic.All[ ,c(19:24)]
MC.Allelic.All.N16C <- MC.F.Allelic.All[ ,c(25:29)]
MC.Allelic.All.eB <- MC.F.Allelic.All[ ,c(30:34)]

#l2C
#Filtering out genes with total allelic reads <=6
chrX.skew.l2C <- MC.skew.All.l2C[intersect(rownames(MC.skew.All.l2C), chrXGenes), ]
chrX.Allelic.All.l2C <- MC.Allelic.All.l2C[intersect(rownames(MC.Allelic.All.l2C), chrXGenes), ]
loc <- which(chrX.Allelic.All.l2C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chrX.skew.l2C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 2 "non-NaN"s
chrX.rows <- rowSums(chrX.skew.l2C !="NaN") >= 1
#convert the elements in chrX.skew.l2C into numeric from character
chrX.GeneaNme <- rownames(chrX.skew.l2C)
chrX.skew.l2C <- sapply(chrX.skew.l2C, as.numeric )
All.chrX.skew.Mean.l2C <- data.frame(skew=rowMeans(chrX.skew.l2C, na.rm=T), gene=chrX.GeneaNme, chr="chrX", row=chrX.rows)

#4C
#Filtering out genes with low allelic reads
chrX.skew.N4C <- MC.skew.All.N4C[intersect(rownames(MC.skew.All.N4C), chrXGenes), ]
chrX.Allelic.All.N4C <- MC.Allelic.All.N4C[intersect(rownames(MC.Allelic.All.N4C), chrXGenes), ]
loc <- which(chrX.Allelic.All.N4C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chrX.skew.N4C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 2 "non-NaN"s
chrX.rows <- rowSums(chrX.skew.N4C !="NaN") >= 1
#convert the elements in chrX.skew.N4C into numeric from character
chrX.GeneaNme <- rownames(chrX.skew.N4C)
chrX.skew.N4C <- sapply(chrX.skew.N4C, as.numeric )
All.chrX.skew.Mean.N4C <- data.frame(skew=rowMeans(chrX.skew.N4C, na.rm=T), gene=chrX.GeneaNme, chr="chrX", row=chrX.rows)

#8C
#Filtering out genes with low allelic reads
chrX.skew.N8C <- MC.skew.All.N8C[intersect(rownames(MC.skew.All.N8C), chrXGenes), ]
chrX.Allelic.All.N8C <- MC.Allelic.All.N8C[intersect(rownames(MC.Allelic.All.N8C), chrXGenes), ]
loc <- which(chrX.Allelic.All.N8C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chrX.skew.N8C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 2 "non-NaN"s
chrX.rows <- rowSums(chrX.skew.N8C !="NaN") >= 1
#convert the elements in chrX.skew.N8C into numeric from character
chrX.GeneaNme <- rownames(chrX.skew.N8C)
chrX.skew.N8C <- sapply(chrX.skew.N8C, as.numeric )
All.chrX.skew.Mean.N8C <- data.frame(skew=rowMeans(chrX.skew.N8C, na.rm=T), gene=chrX.GeneaNme, chr="chrX", row=chrX.rows)

#16C
#Filtering out genes with low allelic reads
chrX.skew.N16C <- MC.skew.All.N16C[intersect(rownames(MC.skew.All.N16C), chrXGenes), ]
chrX.Allelic.All.N16C <- MC.Allelic.All.N16C[intersect(rownames(MC.Allelic.All.N16C), chrXGenes), ]
loc <- which(chrX.Allelic.All.N16C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chrX.skew.N16C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect genes having at least 2 "non-NaN"s
chrX.rows <- rowSums(chrX.skew.N16C !="NaN") >= 1
#convert the elements in chrX.skew.N16C into numeric from character
chrX.GeneaNme <- rownames(chrX.skew.N16C)
chrX.skew.N16C <- sapply(chrX.skew.N16C, as.numeric )
All.chrX.skew.Mean.N16C <- data.frame(skew=rowMeans(chrX.skew.N16C, na.rm=T), gene=chrX.GeneaNme, chr="chrX", row=chrX.rows)

#eB
#Filtering out genes with low allelic reads
chrX.skew.eB <- MC.skew.All.eB[intersect(rownames(MC.skew.All.eB), chrXGenes), ]
chrX.Allelic.All.eB <- MC.Allelic.All.eB[intersect(rownames(MC.Allelic.All.eB), chrXGenes), ]
loc <- which(chrX.Allelic.All.eB <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chrX.skew.eB[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect genes having at least 2 "non-NaN"s
chrX.rows <- rowSums(chrX.skew.eB !="NaN") >= 1
#convert the elements in chrX.skew.eB into numeric from character
chrX.GeneaNme <- rownames(chrX.skew.eB)
chrX.skew.eB <- sapply(chrX.skew.eB, as.numeric )
All.chrX.skew.Mean.eB <- data.frame(skew=rowMeans(chrX.skew.eB, na.rm=T), gene=chrX.GeneaNme, chr="chrX", row=chrX.rows)

#combine skew data across "5" stages###################################################################################
MC.chrX.skew.Total <- data.frame(l2C=All.chrX.skew.Mean.l2C$skew, N4C=All.chrX.skew.Mean.N4C$skew, N8C=All.chrX.skew.Mean.N8C$skew, N16C=All.chrX.skew.Mean.N16C$skew, eB=All.chrX.skew.Mean.eB$skew, locus=GeneInfo[rownames(All.chrX.skew.Mean.eB), "locus"], row.names = rownames(All.chrX.skew.Mean.eB))
MC.chrX.row.Total <- data.frame(l2C=All.chrX.skew.Mean.l2C$row, N4C=All.chrX.skew.Mean.N4C$row, N8C=All.chrX.skew.Mean.N8C$row, N16C=All.chrX.skew.Mean.N16C$row, eB=All.chrX.skew.Mean.eB$row)
#clear up intermediate variables
rm(list=setdiff(ls(), c("GeneInfo","chrXGenes",
                        "KO.F.Allelic.All","KO.F.mus.All","KO.F.cas.All",
                        "CM.F.Allelic.All","CM.F.mus.All","CM.F.cas.All",
                        "MC.chrX.skew.Total","MC.chrX.row.Total")))



#CM cross 
CM.skew.All <- CM.F.mus.All / CM.F.Allelic.All
CM.skew.All.l2C <- CM.skew.All[ ,c(1:5)]
CM.skew.All.N4C <- CM.skew.All[ ,c(6:10)]
CM.skew.All.N8C <- CM.skew.All[ ,c(11:19)]
CM.skew.All.N16C <- CM.skew.All[ ,c(20:26)]
CM.skew.All.eB <- CM.skew.All[ ,c(27:31)]

CM.Allelic.All.l2C <- CM.F.Allelic.All[ ,c(1:5)]
CM.Allelic.All.N4C <- CM.F.Allelic.All[ ,c(6:10)]
CM.Allelic.All.N8C <- CM.F.Allelic.All[ ,c(11:19)]
CM.Allelic.All.N16C <- CM.F.Allelic.All[ ,c(20:26)]
CM.Allelic.All.eB <- CM.F.Allelic.All[ ,c(27:31)]

chrXGenes <- rownames(GeneInfo[which(GeneInfo$Chr == "chrX"), ])

#l2C
#Filtering out genes with low allelic reads <6
chrX.skew.l2C <- CM.skew.All.l2C[intersect(rownames(CM.skew.All.l2C), chrXGenes), ]
chrX.Allelic.All.l2C <- CM.Allelic.All.l2C[intersect(rownames(CM.Allelic.All.l2C), chrXGenes), ]
loc <- which(chrX.Allelic.All.l2C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chrX.skew.l2C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 1 "non-NaN"s
chrX.rows <- rowSums(chrX.skew.l2C !="NaN") >= 1
#convert the elements in chrX.skew.l2C into numeric from character
chrX.GeneaNme <- rownames(chrX.skew.l2C)
chrX.skew.l2C <- sapply(chrX.skew.l2C, as.numeric )
All.chrX.skew.Mean.l2C <- data.frame(skew=rowMeans(chrX.skew.l2C, na.rm=T), gene=chrX.GeneaNme, chr="chrX", row=chrX.rows)

#4C
#Filtering out genes with low allelic reads
chrX.skew.N4C <- CM.skew.All.N4C[intersect(rownames(CM.skew.All.N4C), chrXGenes), ]
chrX.Allelic.All.N4C <- CM.Allelic.All.N4C[intersect(rownames(CM.Allelic.All.N4C), chrXGenes), ]
loc <- which(chrX.Allelic.All.N4C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chrX.skew.N4C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 2 "non-NaN"s
chrX.rows <- rowSums(chrX.skew.N4C !="NaN") >= 1
#convert the elements in chrX.skew.N4C into numeric from character
chrX.GeneaNme <- rownames(chrX.skew.N4C)
chrX.skew.N4C <- sapply(chrX.skew.N4C, as.numeric )
All.chrX.skew.Mean.N4C <- data.frame(skew=rowMeans(chrX.skew.N4C, na.rm=T), gene=chrX.GeneaNme, chr="chrX", row=chrX.rows)

#8C
#Filtering out genes with low allelic reads
chrX.skew.N8C <- CM.skew.All.N8C[intersect(rownames(CM.skew.All.N8C), chrXGenes), ]
chrX.Allelic.All.N8C <- CM.Allelic.All.N8C[intersect(rownames(CM.Allelic.All.N8C), chrXGenes), ]
loc <- which(chrX.Allelic.All.N8C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chrX.skew.N8C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 2 "non-NaN"s
chrX.rows <- rowSums(chrX.skew.N8C !="NaN") >= 1
#convert the elements in chrX.skew.N8C into numeric from character
chrX.GeneaNme <- rownames(chrX.skew.N8C)
chrX.skew.N8C <- sapply(chrX.skew.N8C, as.numeric )
All.chrX.skew.Mean.N8C <- data.frame(skew=rowMeans(chrX.skew.N8C, na.rm=T), gene=chrX.GeneaNme, chr="chrX", row=chrX.rows)

#16C
#Filtering out genes with low allelic reads
chrX.skew.N16C <- CM.skew.All.N16C[intersect(rownames(CM.skew.All.N16C), chrXGenes), ]
chrX.Allelic.All.N16C <- CM.Allelic.All.N16C[intersect(rownames(CM.Allelic.All.N16C), chrXGenes), ]
loc <- which(chrX.Allelic.All.N16C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chrX.skew.N16C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect genes having at least 2 "non-NaN"s
chrX.rows <- rowSums(chrX.skew.N16C !="NaN") >= 1
#convert the elements in chrX.skew.N16C into numeric from character
chrX.GeneaNme <- rownames(chrX.skew.N16C)
chrX.skew.N16C <- sapply(chrX.skew.N16C, as.numeric )
All.chrX.skew.Mean.N16C <- data.frame(skew=rowMeans(chrX.skew.N16C, na.rm=T), gene=chrX.GeneaNme, chr="chrX", row=chrX.rows)

#eB
#Filtering out genes with low allelic reads
chrX.skew.eB <- CM.skew.All.eB[intersect(rownames(CM.skew.All.eB), chrXGenes), ]
chrX.Allelic.All.eB <- CM.Allelic.All.eB[intersect(rownames(CM.Allelic.All.eB), chrXGenes), ]
loc <- which(chrX.Allelic.All.eB <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chrX.skew.eB[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect genes having at least 2 "non-NaN"s
chrX.rows <- rowSums(chrX.skew.eB !="NaN") >= 1
#convert the elements in chrX.skew.eB into numeric from character
chrX.GeneaNme <- rownames(chrX.skew.eB)
chrX.skew.eB <- sapply(chrX.skew.eB, as.numeric )
All.chrX.skew.Mean.eB <- data.frame(skew=rowMeans(chrX.skew.eB, na.rm=T), gene=chrX.GeneaNme, chr="chrX", row=chrX.rows)

#combine skew data across "5" stages#########
CM.chrX.skew.Total <- data.frame(l2C=All.chrX.skew.Mean.l2C$skew, N4C=All.chrX.skew.Mean.N4C$skew, N8C=All.chrX.skew.Mean.N8C$skew, N16C=All.chrX.skew.Mean.N16C$skew, eB=All.chrX.skew.Mean.eB$skew, locus=GeneInfo[rownames(All.chrX.skew.Mean.eB), "locus"], row.names = rownames(All.chrX.skew.Mean.eB))
CM.chrX.row.Total <- data.frame(l2C=All.chrX.skew.Mean.l2C$row, N4C=All.chrX.skew.Mean.N4C$row, N8C=All.chrX.skew.Mean.N8C$row, N16C=All.chrX.skew.Mean.N16C$row, eB=All.chrX.skew.Mean.eB$row)
#clear up intermediate variables
rm(list=setdiff(ls(), c("GeneInfo","chrXGenes",
                        "KO.F.Allelic.All","KO.F.mus.All","KO.F.cas.All",
                        "CM.chrX.skew.Total","CM.chrX.row.Total",
                        "MC.chrX.skew.Total","MC.chrX.row.Total")))



#KO cross 
KO.skew.All <- KO.F.mus.All / KO.F.Allelic.All
KO.skew.All.l2C <- KO.skew.All[ ,c(1:6)]
KO.skew.All.N4C <- KO.skew.All[ ,c(7:12)]
KO.skew.All.N8C <- KO.skew.All[ ,c(13:18)]
KO.skew.All.eB <- KO.skew.All[ ,c(19:25)]

KO.Allelic.All.l2C <- KO.F.Allelic.All[ ,c(1:6)]
KO.Allelic.All.N4C <- KO.F.Allelic.All[ ,c(7:12)]
KO.Allelic.All.N8C <- KO.F.Allelic.All[ ,c(13:18)]
KO.Allelic.All.eB <- KO.F.Allelic.All[ ,c(19:25)]

#l2C
#Filtering out genes with low allelic reads <6
chrX.skew.l2C <- KO.skew.All.l2C[intersect(rownames(KO.skew.All.l2C), chrXGenes), ]
chrX.Allelic.All.l2C <- KO.Allelic.All.l2C[intersect(rownames(KO.Allelic.All.l2C), chrXGenes), ]
loc <- which(chrX.Allelic.All.l2C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chrX.skew.l2C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 2 "non-NaN"s
chrX.rows <- rowSums(chrX.skew.l2C !="NaN") >= 1
#convert the elements in chrX.skew.l2C into numeric from character
chrX.GeneaNme <- rownames(chrX.skew.l2C)
chrX.skew.l2C <- sapply(chrX.skew.l2C, as.numeric )
All.chrX.skew.Mean.l2C <- data.frame(skew=rowMeans(chrX.skew.l2C, na.rm=T), gene=chrX.GeneaNme, chr="chrX", row=chrX.rows)

#4C
#Filtering out genes with low allelic reads
chrX.skew.N4C <- KO.skew.All.N4C[intersect(rownames(KO.skew.All.N4C), chrXGenes), ]
chrX.Allelic.All.N4C <- KO.Allelic.All.N4C[intersect(rownames(KO.Allelic.All.N4C), chrXGenes), ]
loc <- which(chrX.Allelic.All.N4C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chrX.skew.N4C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 2 "non-NaN"s
chrX.rows <- rowSums(chrX.skew.N4C !="NaN") >= 1
#convert the elements in chrX.skew.N4C into numeric from character
chrX.GeneaNme <- rownames(chrX.skew.N4C)
chrX.skew.N4C <- sapply(chrX.skew.N4C, as.numeric )
All.chrX.skew.Mean.N4C <- data.frame(skew=rowMeans(chrX.skew.N4C, na.rm=T), gene=chrX.GeneaNme, chr="chrX", row=chrX.rows)

#8C
#Filtering out genes with low allelic reads
chrX.skew.N8C <- KO.skew.All.N8C[intersect(rownames(KO.skew.All.N8C), chrXGenes), ]
chrX.Allelic.All.N8C <- KO.Allelic.All.N8C[intersect(rownames(KO.Allelic.All.N8C), chrXGenes), ]
loc <- which(chrX.Allelic.All.N8C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chrX.skew.N8C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 2 "non-NaN"s
chrX.rows <- rowSums(chrX.skew.N8C !="NaN") >= 1
#convert the elements in chrX.skew.N8C into numeric from character
chrX.GeneaNme <- rownames(chrX.skew.N8C)
chrX.skew.N8C <- sapply(chrX.skew.N8C, as.numeric )
All.chrX.skew.Mean.N8C <- data.frame(skew=rowMeans(chrX.skew.N8C, na.rm=T), gene=chrX.GeneaNme, chr="chrX", row=chrX.rows)

#eB
#Filtering out genes with low allelic reads
chrX.skew.eB <- KO.skew.All.eB[intersect(rownames(KO.skew.All.eB), chrXGenes), ]
chrX.Allelic.All.eB <- KO.Allelic.All.eB[intersect(rownames(KO.Allelic.All.eB), chrXGenes), ]
loc <- which(chrX.Allelic.All.eB <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chrX.skew.eB[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect genes having at least 2 "non-NaN"s
chrX.rows <- rowSums(chrX.skew.eB !="NaN") >= 1
#convert the elements in chrX.skew.eB into numeric from character
chrX.GeneaNme <- rownames(chrX.skew.eB)
chrX.skew.eB <- sapply(chrX.skew.eB, as.numeric )
All.chrX.skew.Mean.eB <- data.frame(skew=rowMeans(chrX.skew.eB, na.rm=T), gene=chrX.GeneaNme, chr="chrX", row=chrX.rows)

#combine skew data across "4" stages#########
KO.chrX.skew.Total <- data.frame(l2C=All.chrX.skew.Mean.l2C$skew, N4C=All.chrX.skew.Mean.N4C$skew, N8C=All.chrX.skew.Mean.N8C$skew, eB=All.chrX.skew.Mean.eB$skew, locus=GeneInfo[rownames(All.chrX.skew.Mean.eB), "locus"], row.names = rownames(All.chrX.skew.Mean.eB))
KO.chrX.row.Total <- data.frame(l2C=All.chrX.skew.Mean.l2C$row, N4C=All.chrX.skew.Mean.N4C$row, N8C=All.chrX.skew.Mean.N8C$row, eB=All.chrX.skew.Mean.eB$row)
#clear up intermediate variables
rm(list=setdiff(ls(), c("GeneInfo","chrXGenes",
                        "CM.chrX.skew.Total","CM.chrX.row.Total",
                        "MC.chrX.skew.Total","MC.chrX.row.Total",
                        "KO.chrX.skew.Total","KO.chrX.row.Total")))



#1. chrX skewing matrix including all stages in different crosses. 
CM.chrX.skew.AllStages <- CM.chrX.skew.Total
MC.chrX.skew.AllStages <- MC.chrX.skew.Total 
KO.chrX.skew.AllStages <- KO.chrX.skew.Total 
#2. select genes that meet expression cutoff thresholds from 4 stages required for clustering (4-cell, 8-cell, 16-cell and early Blastocyst).

CM.chrX.row.4Stages <- CM.chrX.row.Total[ ,c(2:5)]
MC.chrX.row.4Stages <- MC.chrX.row.Total[ ,c(2:5)]
CM.chrX.skew.4Stages <- CM.chrX.skew.AllStages[which(rowSums(CM.chrX.row.4Stages) == 4), c(1:5)]
MC.chrX.skew.4Stages <- MC.chrX.skew.AllStages[which(rowSums(MC.chrX.row.4Stages) == 4), c(1:5)]

rm(list=setdiff(ls(), c("GeneInfo",
                        "CM.chrX.skew.AllStages",
                        "MC.chrX.skew.AllStages",
                        "KO.chrX.skew.AllStages",
                        "CM.chrX.skew.4Stages",
                        "MC.chrX.skew.4Stages")))

