
#Gene skewing analysis for all gene density plots (average from embryo replicates that contain more than 6 allelic reads)
#Gene skewing is calculated as the paternal allelic reads fraction in total allelic reads in each embryo.
#Abbr.: ZG, zygote; e2C, early2C; l2C, late2C; N4C, 4C; N8C, 8C; N16C, 16C; eB, early Blastocyst.
###########################################################
source("Read_data.R")
rm(list=setdiff(ls(), c("GeneInfo",
                        "CM.F.Allelic.All","CM.F.mus.All","CM.F.cas.All","CM.F.comp.RPKM", "CM.M.comp.RPKM",
                        "MC.F.Allelic.All","MC.F.mus.All","MC.F.cas.All","MC.F.comp.RPKM","MC.M.comp.RPKM",
                        "KO.F.Allelic.All","KO.F.mus.All","KO.F.cas.All","KO.F.comp.RPKM","KO.M.comp.RPKM")))

########part 1###########
# get autosomal gene and chrX gene
autoGenes <- rownames(GeneInfo[which(GeneInfo$Chr != "chrX" & GeneInfo$Chr != "chrY"), ])
chrXGenes <- rownames(GeneInfo[which(GeneInfo$Chr == "chrX"), ])
chr13Genes <- rownames(GeneInfo[which(GeneInfo$Chr == "chr13"), ])

#MC cross 
MC.skew.All <- MC.F.cas.All / MC.F.Allelic.All
MC.skew.All.ZG <- MC.skew.All[ ,c(1:4)]
MC.skew.All.e2C <- MC.skew.All[ ,c(5:7)]
MC.skew.All.l2C <- MC.skew.All[ ,c(8:12)]
MC.skew.All.N4C <- MC.skew.All[ ,c(13:18)]
MC.skew.All.N8C <- MC.skew.All[ ,c(19:24)]
MC.skew.All.N16C <- MC.skew.All[ ,c(25:29)]
MC.skew.All.eB <- MC.skew.All[ ,c(30:34)]


MC.Allelic.All.ZG <- MC.F.Allelic.All[ ,c(1:4)]
MC.Allelic.All.e2C <- MC.F.Allelic.All[ ,c(5:7)]
MC.Allelic.All.l2C <- MC.F.Allelic.All[ ,c(8:12)]
MC.Allelic.All.N4C <- MC.F.Allelic.All[ ,c(13:18)]
MC.Allelic.All.N8C <- MC.F.Allelic.All[ ,c(19:24)]
MC.Allelic.All.N16C <- MC.F.Allelic.All[ ,c(25:29)]
MC.Allelic.All.eB <- MC.F.Allelic.All[ ,c(30:34)]


#Zygote
#Filtering out genes with few allelic reads
chrX.skew.ZG <- MC.skew.All.ZG[intersect(rownames(MC.skew.All.ZG), chrXGenes), ]
chrX.Allelic.All.ZG <- MC.Allelic.All.ZG[intersect(rownames(MC.Allelic.All.ZG), chrXGenes), ]
loc <- which(chrX.Allelic.All.ZG <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chrX.skew.ZG[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 2 "non-NaN"s
chrX.rows <- rowSums(chrX.skew.ZG !="NaN") > 1

chr13.skew.ZG <- MC.skew.All.ZG[intersect(rownames(MC.skew.All.ZG), chr13Genes), ]
chr13.Allelic.All.ZG <- MC.Allelic.All.ZG[intersect(rownames(MC.Allelic.All.ZG), chr13Genes), ]
loc <- which(chr13.Allelic.All.ZG <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chr13.skew.ZG[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 2 "non-NaN"s
chr13.rows <- rowSums(chr13.skew.ZG !="NaN") > 1

#convert the elements in chrX.skew.ZG and chr13.skew.ZG into numeric from character
chrX.GeneaNme <- rownames(chrX.skew.ZG)
chrX.skew.ZG <- sapply(chrX.skew.ZG, as.numeric )
chr13.GeneName <- rownames(chr13.skew.ZG)
chr13.skew.ZG <- sapply(chr13.skew.ZG, as.numeric )

All.chrX.skew.Mean.ZG <- data.frame(skew=rowMeans(chrX.skew.ZG, na.rm=T), gene=chrX.GeneaNme, chr="chrX", row=chrX.rows)
MC.chrX.skew.Mean.ZG <- All.chrX.skew.Mean.ZG[which(All.chrX.skew.Mean.ZG$row == "TRUE"), ]
All.chr13.skew.Mean.ZG <- data.frame(skew=rowMeans(chr13.skew.ZG, na.rm=T), gene=chr13.GeneName, chr="chr13",row=chr13.rows)
MC.chr13.skew.Mean.ZG <- All.chr13.skew.Mean.ZG[which(All.chr13.skew.Mean.ZG$row == "TRUE"), ]


#e2C
#Filtering out genes with few allelic reads
chrX.skew.e2C <- MC.skew.All.e2C[intersect(rownames(MC.skew.All.e2C), chrXGenes), ]
chrX.Allelic.All.e2C <- MC.Allelic.All.e2C[intersect(rownames(MC.Allelic.All.e2C), chrXGenes), ]
loc <- which(chrX.Allelic.All.e2C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chrX.skew.e2C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 2 "non-NaN"s
chrX.rows <- rowSums(chrX.skew.e2C !="NaN") > 1

chr13.skew.e2C <- MC.skew.All.e2C[intersect(rownames(MC.skew.All.e2C), chr13Genes), ]
chr13.Allelic.All.e2C <- MC.Allelic.All.e2C[intersect(rownames(MC.Allelic.All.e2C), chr13Genes), ]
loc <- which(chr13.Allelic.All.e2C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chr13.skew.e2C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 2 "non-NaN"s
chr13.rows <- rowSums(chr13.skew.e2C !="NaN") > 1

#convert the elements in chrX.skew.e2C and chr13.skew.e2C into numeric from character
chrX.GeneaNme <- rownames(chrX.skew.e2C)
chrX.skew.e2C <- sapply(chrX.skew.e2C, as.numeric )
chr13.GeneName <- rownames(chr13.skew.e2C)
chr13.skew.e2C <- sapply(chr13.skew.e2C, as.numeric )

All.chrX.skew.Mean.e2C <- data.frame(skew=rowMeans(chrX.skew.e2C, na.rm=T), gene=chrX.GeneaNme, chr="chrX", row=chrX.rows)
MC.chrX.skew.Mean.e2C <- All.chrX.skew.Mean.e2C[which(All.chrX.skew.Mean.e2C$row == "TRUE"), ]
All.chr13.skew.Mean.e2C <- data.frame(skew=rowMeans(chr13.skew.e2C, na.rm=T), gene=chr13.GeneName, chr="chr13",row=chr13.rows)
MC.chr13.skew.Mean.e2C <- All.chr13.skew.Mean.e2C[which(All.chr13.skew.Mean.e2C$row == "TRUE"), ]


#l2C
#Filtering out genes with few allelic reads <6
chrX.skew.l2C <- MC.skew.All.l2C[intersect(rownames(MC.skew.All.l2C), chrXGenes), ]
chrX.Allelic.All.l2C <- MC.Allelic.All.l2C[intersect(rownames(MC.Allelic.All.l2C), chrXGenes), ]
loc <- which(chrX.Allelic.All.l2C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chrX.skew.l2C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 2 "non-NaN"s
chrX.rows <- rowSums(chrX.skew.l2C !="NaN") > 1

chr13.skew.l2C <- MC.skew.All.l2C[intersect(rownames(MC.skew.All.l2C), chr13Genes), ]
chr13.Allelic.All.l2C <- MC.Allelic.All.l2C[intersect(rownames(MC.Allelic.All.l2C), chr13Genes), ]
loc <- which(chr13.Allelic.All.l2C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chr13.skew.l2C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 2 "non-NaN"s
chr13.rows <- rowSums(chr13.skew.l2C !="NaN") > 1

#convert the elements in chrX.skew.l2C and chr13.skew.l2C into numeric from character
chrX.GeneaNme <- rownames(chrX.skew.l2C)
chrX.skew.l2C <- sapply(chrX.skew.l2C, as.numeric )
chr13.GeneaNme <- rownames(chr13.skew.l2C)
chr13.skew.l2C <- sapply(chr13.skew.l2C, as.numeric )

All.chrX.skew.Mean.l2C <- data.frame(skew=rowMeans(chrX.skew.l2C, na.rm=T), gene=chrX.GeneaNme, chr="chrX", row=chrX.rows)
MC.chrX.skew.Mean.l2C <- All.chrX.skew.Mean.l2C[which(All.chrX.skew.Mean.l2C$row == "TRUE"), ]
All.chr13.skew.Mean.l2C <- data.frame(skew=rowMeans(chr13.skew.l2C, na.rm=T), gene=chr13.GeneaNme, chr="chr13", row=chr13.rows)
MC.chr13.skew.Mean.l2C <- All.chr13.skew.Mean.l2C[which(All.chr13.skew.Mean.l2C$row == "TRUE"), ]


#4C
#Filtering out genes with few allelic reads
chrX.skew.N4C <- MC.skew.All.N4C[intersect(rownames(MC.skew.All.N4C), chrXGenes), ]
chrX.Allelic.All.N4C <- MC.Allelic.All.N4C[intersect(rownames(MC.Allelic.All.N4C), chrXGenes), ]
loc <- which(chrX.Allelic.All.N4C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chrX.skew.N4C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 2 "non-NaN"s
chrX.rows <- rowSums(chrX.skew.N4C !="NaN") > 1

chr13.skew.N4C <- MC.skew.All.N4C[intersect(rownames(MC.skew.All.N4C), chr13Genes), ]
chr13.Allelic.All.N4C <- MC.Allelic.All.N4C[intersect(rownames(MC.Allelic.All.N4C), chr13Genes), ]
loc <- which(chr13.Allelic.All.N4C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chr13.skew.N4C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 2 "non-NaN"s
chr13.rows <- rowSums(chr13.skew.N4C !="NaN") > 1

#convert the elements in chrX.skew.N4C and chr13.skew.N4C into numeric from character
chrX.GeneaNme <- rownames(chrX.skew.N4C)
chrX.skew.N4C <- sapply(chrX.skew.N4C, as.numeric )
chr13.GeneaNme <- rownames(chr13.skew.N4C)
chr13.skew.N4C <- sapply(chr13.skew.N4C, as.numeric )

All.chrX.skew.Mean.N4C <- data.frame(skew=rowMeans(chrX.skew.N4C, na.rm=T), gene=chrX.GeneaNme, chr="chrX", row=chrX.rows)
MC.chrX.skew.Mean.N4C <- All.chrX.skew.Mean.N4C[which(All.chrX.skew.Mean.N4C$row == "TRUE"), ]
All.chr13.skew.Mean.N4C <- data.frame(skew=rowMeans(chr13.skew.N4C, na.rm=T), gene=chr13.GeneaNme, chr="chr13", row=chr13.rows)
MC.chr13.skew.Mean.N4C <- All.chr13.skew.Mean.N4C[which(All.chr13.skew.Mean.N4C$row == "TRUE"), ]


#8C
#Filtering out genes with few allelic reads
chrX.skew.N8C <- MC.skew.All.N8C[intersect(rownames(MC.skew.All.N8C), chrXGenes), ]
chrX.Allelic.All.N8C <- MC.Allelic.All.N8C[intersect(rownames(MC.Allelic.All.N8C), chrXGenes), ]
loc <- which(chrX.Allelic.All.N8C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chrX.skew.N8C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 2 "non-NaN"s
chrX.rows <- rowSums(chrX.skew.N8C !="NaN") > 1

chr13.skew.N8C <- MC.skew.All.N8C[intersect(rownames(MC.skew.All.N8C), chr13Genes), ]
chr13.Allelic.All.N8C <- MC.Allelic.All.N8C[intersect(rownames(MC.Allelic.All.N8C), chr13Genes), ]
loc <- which(chr13.Allelic.All.N8C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chr13.skew.N8C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 2 "non-NaN"s
chr13.rows <- rowSums(chr13.skew.N8C !="NaN") > 1

#convert the elements in chrX.skew.N8C and chr13.skew.N8C into numeric from character
chrX.GeneaNme <- rownames(chrX.skew.N8C)
chrX.skew.N8C <- sapply(chrX.skew.N8C, as.numeric )
chr13.GeneaNme <- rownames(chr13.skew.N8C)
chr13.skew.N8C <- sapply(chr13.skew.N8C, as.numeric )

All.chrX.skew.Mean.N8C <- data.frame(skew=rowMeans(chrX.skew.N8C, na.rm=T), gene=chrX.GeneaNme, chr="chrX", row=chrX.rows)
MC.chrX.skew.Mean.N8C <- All.chrX.skew.Mean.N8C[which(All.chrX.skew.Mean.N8C$row == "TRUE"), ]
All.chr13.skew.Mean.N8C <- data.frame(skew=rowMeans(chr13.skew.N8C, na.rm=T), gene=chr13.GeneaNme, chr="chr13", row=chr13.rows)
MC.chr13.skew.Mean.N8C <- All.chr13.skew.Mean.N8C[which(All.chr13.skew.Mean.N8C$row == "TRUE"), ]


#16C
#Filtering out genes with few allelic reads
chrX.skew.N16C <- MC.skew.All.N16C[intersect(rownames(MC.skew.All.N16C), chrXGenes), ]
chrX.Allelic.All.N16C <- MC.Allelic.All.N16C[intersect(rownames(MC.Allelic.All.N16C), chrXGenes), ]
loc <- which(chrX.Allelic.All.N16C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chrX.skew.N16C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect genes having at least 2 "non-NaN"s
chrX.rows <- rowSums(chrX.skew.N16C !="NaN") > 1

chr13.skew.N16C <- MC.skew.All.N16C[intersect(rownames(MC.skew.All.N16C), chr13Genes), ]
chr13.Allelic.All.N16C <- MC.Allelic.All.N16C[intersect(rownames(MC.Allelic.All.N16C), chr13Genes), ]
loc <- which(chr13.Allelic.All.N16C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chr13.skew.N16C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect genes having at least 2 "non-NaN"s
chr13.rows <- rowSums(chr13.skew.N16C !="NaN") > 1

#convert the elements in chrX.skew.N16C and chr13.skew.N16C into numeric from character
chrX.GeneaNme <- rownames(chrX.skew.N16C)
chrX.skew.N16C <- sapply(chrX.skew.N16C, as.numeric )
chr13.GeneaNme <- rownames(chr13.skew.N16C)
chr13.skew.N16C <- sapply(chr13.skew.N16C, as.numeric )

All.chrX.skew.Mean.N16C <- data.frame(skew=rowMeans(chrX.skew.N16C, na.rm=T), gene=chrX.GeneaNme, chr="chrX", row=chrX.rows)
MC.chrX.skew.Mean.N16C <- All.chrX.skew.Mean.N16C[which(All.chrX.skew.Mean.N16C$row == "TRUE"), ]
All.chr13.skew.Mean.N16C <- data.frame(skew=rowMeans(chr13.skew.N16C, na.rm=T), gene=chr13.GeneaNme, chr="chr13", row=chr13.rows)
MC.chr13.skew.Mean.N16C <- All.chr13.skew.Mean.N16C[which(All.chr13.skew.Mean.N16C$row == "TRUE"), ]


#eB
#Filtering out genes with few allelic reads
chrX.skew.eB <- MC.skew.All.eB[intersect(rownames(MC.skew.All.eB), chrXGenes), ]
chrX.Allelic.All.eB <- MC.Allelic.All.eB[intersect(rownames(MC.Allelic.All.eB), chrXGenes), ]
loc <- which(chrX.Allelic.All.eB <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chrX.skew.eB[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect genes having at least 2 "non-NaN"s
chrX.rows <- rowSums(chrX.skew.eB !="NaN") > 1

chr13.skew.eB <- MC.skew.All.eB[intersect(rownames(MC.skew.All.eB), chr13Genes), ]
chr13.Allelic.All.eB <- MC.Allelic.All.eB[intersect(rownames(MC.Allelic.All.eB), chr13Genes), ]
loc <- which(chr13.Allelic.All.eB <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chr13.skew.eB[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect genes having at least 2 "non-NaN"s
chr13.rows <- rowSums(chr13.skew.eB !="NaN") > 1

#convert the elements in chrX.skew.eB and chr13.skew.eB into numeric from character
chrX.GeneaNme <- rownames(chrX.skew.eB)
chrX.skew.eB <- sapply(chrX.skew.eB, as.numeric )
chr13.GeneaNme <- rownames(chr13.skew.eB)
chr13.skew.eB <- sapply(chr13.skew.eB, as.numeric )

All.chrX.skew.Mean.eB <- data.frame(skew=rowMeans(chrX.skew.eB, na.rm=T), gene=chrX.GeneaNme, chr="chrX", row=chrX.rows)
MC.chrX.skew.Mean.eB <- All.chrX.skew.Mean.eB[which(All.chrX.skew.Mean.eB$row == "TRUE"), ]
All.chr13.skew.Mean.eB <- data.frame(skew=rowMeans(chr13.skew.eB, na.rm=T), gene=chr13.GeneaNme, chr="chr13", row=chr13.rows)
MC.chr13.skew.Mean.eB <- All.chr13.skew.Mean.eB[which(All.chr13.skew.Mean.eB$row == "TRUE"), ]


rm(list=setdiff(ls(), c("GeneInfo",
                        "CM.F.Allelic.All","CM.F.mus.All","CM.F.cas.All",
                        "MC.F.Allelic.All","MC.F.mus.All","MC.F.cas.All",
                        "KO.F.Allelic.All","KO.F.mus.All","KO.F.cas.All",
                        "MC.chrX.skew.Mean.ZG","MC.chr13.skew.Mean.ZG","MC.chrX.skew.Mean.e2C","MC.chr13.skew.Mean.e2C",
                        "MC.chrX.skew.Mean.l2C","MC.chr13.skew.Mean.l2C","MC.chrX.skew.Mean.N4C","MC.chr13.skew.Mean.N4C",
                        "MC.chrX.skew.Mean.N8C","MC.chr13.skew.Mean.N8C","MC.chrX.skew.Mean.N16C","MC.chr13.skew.Mean.N16C",
                        "MC.chrX.skew.Mean.eB","MC.chr13.skew.Mean.eB")))



#KO embryos

# get autosomal gene and chrX gene
autoGenes <- rownames(GeneInfo[which(GeneInfo$Chr != "chrX" & GeneInfo$Chr != "chrY"), ])
chrXGenes <- rownames(GeneInfo[which(GeneInfo$Chr == "chrX"), ])
chr13Genes <- rownames(GeneInfo[which(GeneInfo$Chr == "chr13"), ])


KO.skew.All <- KO.F.mus.All / KO.F.Allelic.All
KO.skew.All.l2C <- KO.skew.All[ ,c(1:6)]
KO.skew.All.N4C <- KO.skew.All[ ,c(7:12)]
KO.skew.All.N8C <- KO.skew.All[ ,c(13:18)]
KO.skew.All.eB <- KO.skew.All[ ,c(19:25)]

KO.Allelic.All.l2C <- KO.F.Allelic.All[ ,c(1:6)]
KO.Allelic.All.N4C <- KO.F.Allelic.All[ ,c(7:12)]
KO.Allelic.All.N8C <- KO.F.Allelic.All[ ,c(13:18)]
KO.Allelic.All.eB <- KO.F.Allelic.All[ ,c(19:25)]


#late2cell
#Filtering out genes with allelic reads <=6
chrX.skew.l2C <- KO.skew.All.l2C[chrXGenes, ]
chrX.Allelic.All.l2C <- KO.Allelic.All.l2C[chrXGenes, ]
loc <- which(chrX.Allelic.All.l2C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chrX.skew.l2C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 2 "non-NaN"s
chrX.rows <- rowSums(chrX.skew.l2C !="NaN") > 1

chr13.skew.l2C <- KO.skew.All.l2C[chr13Genes, ]
chr13.Allelic.All.l2C <- KO.Allelic.All.l2C[chr13Genes, ]
loc <- which(chr13.Allelic.All.l2C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chr13.skew.l2C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 2 "non-NaN"s
chr13.rows <- rowSums(chr13.skew.l2C !="NaN") > 1

#convert the elements in chrX.skew.l2C and chr13.skew.l2C into numeric from character
chrX.GeneaNme <- rownames(chrX.skew.l2C)
chrX.skew.l2C <- sapply(chrX.skew.l2C, as.numeric )
chr13.GeneaNme <- rownames(chr13.skew.l2C)
chr13.skew.l2C <- sapply(chr13.skew.l2C, as.numeric )

All.chrX.skew.Mean.l2C <- data.frame(skew=rowMeans(chrX.skew.l2C, na.rm=T), gene=chrX.GeneaNme, chr="chrX", row=chrX.rows)
KO.chrX.skew.Mean.l2C <- All.chrX.skew.Mean.l2C[which(All.chrX.skew.Mean.l2C$row == "TRUE"), ]
All.chr13.skew.Mean.l2C <- data.frame(skew=rowMeans(chr13.skew.l2C, na.rm=T), gene=chr13.GeneaNme, chr="chr13", row=chr13.rows)
KO.chr13.skew.Mean.l2C <- All.chr13.skew.Mean.l2C[which(All.chr13.skew.Mean.l2C$row == "TRUE"), ]


#4cell
#Filtering out genes with allelic reads <6
chrX.skew.N4C <- KO.skew.All.N4C[chrXGenes, ]
chrX.Allelic.All.N4C <- KO.Allelic.All.N4C[chrXGenes, ]
loc <- which(chrX.Allelic.All.N4C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chrX.skew.N4C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 2 "non-NaN"s
chrX.rows <- rowSums(chrX.skew.N4C !="NaN") > 1

chr13.skew.N4C <- KO.skew.All.N4C[chr13Genes, ]
chr13.Allelic.All.N4C <- KO.Allelic.All.N4C[chr13Genes, ]
loc <- which(chr13.Allelic.All.N4C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chr13.skew.N4C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 2 "non-NaN"s
chr13.rows <- rowSums(chr13.skew.N4C !="NaN") > 1

#convert the elements in chrX.skew.N4C and chr13.skew.N4C into numeric from character
chrX.GeneaNme <- rownames(chrX.skew.N4C)
chrX.skew.N4C <- sapply(chrX.skew.N4C, as.numeric )
chr13.GeneaNme <- rownames(chr13.skew.N4C)
chr13.skew.N4C <- sapply(chr13.skew.N4C, as.numeric )

All.chrX.skew.Mean.N4C <- data.frame(skew=rowMeans(chrX.skew.N4C, na.rm=T), gene=chrX.GeneaNme, chr="chrX", row=chrX.rows)
KO.chrX.skew.Mean.N4C <- All.chrX.skew.Mean.N4C[which(All.chrX.skew.Mean.N4C$row == "TRUE"), ]
All.chr13.skew.Mean.N4C <- data.frame(skew=rowMeans(chr13.skew.N4C, na.rm=T), gene=chr13.GeneaNme, chr="chr13", row=chr13.rows)
KO.chr13.skew.Mean.N4C <- All.chr13.skew.Mean.N4C[which(All.chr13.skew.Mean.N4C$row == "TRUE"), ]


#8cell
#Filtering out genes with allelic reads <6
chrX.skew.N8C <- KO.skew.All.N8C[chrXGenes, ]
chrX.Allelic.All.N8C <- KO.Allelic.All.N8C[chrXGenes, ]
loc <- which(chrX.Allelic.All.N8C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chrX.skew.N8C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 2 "non-NaN"s
chrX.rows <- rowSums(chrX.skew.N8C !="NaN") > 1

chr13.skew.N8C <- KO.skew.All.N8C[chr13Genes, ]
chr13.Allelic.All.N8C <- KO.Allelic.All.N8C[chr13Genes, ]
loc <- which(chr13.Allelic.All.N8C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chr13.skew.N8C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 2 "non-NaN"s
chr13.rows <- rowSums(chr13.skew.N8C !="NaN") > 1

#convert the elements in chrX.skew.N8C and chr13.skew.N8C into numeric from character
chrX.GeneaNme <- rownames(chrX.skew.N8C)
chrX.skew.N8C <- sapply(chrX.skew.N8C, as.numeric )
chr13.GeneaNme <- rownames(chr13.skew.N8C)
chr13.skew.N8C <- sapply(chr13.skew.N8C, as.numeric )

All.chrX.skew.Mean.N8C <- data.frame(skew=rowMeans(chrX.skew.N8C, na.rm=T), gene=chrX.GeneaNme, chr="chrX", row=chrX.rows)
KO.chrX.skew.Mean.N8C <- All.chrX.skew.Mean.N8C[which(All.chrX.skew.Mean.N8C$row == "TRUE"), ]
All.chr13.skew.Mean.N8C <- data.frame(skew=rowMeans(chr13.skew.N8C, na.rm=T), gene=chr13.GeneaNme, chr="chr13", row=chr13.rows)
KO.chr13.skew.Mean.N8C <- All.chr13.skew.Mean.N8C[which(All.chr13.skew.Mean.N8C$row == "TRUE"), ]


#eB
#Filtering out genes with allelic reads <6
chrX.skew.eB <- KO.skew.All.eB[chrXGenes, ]
chrX.Allelic.All.eB <- KO.Allelic.All.eB[chrXGenes, ]
loc <- which(chrX.Allelic.All.eB <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chrX.skew.eB[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 2 "non-NaN"s
chrX.rows <- rowSums(chrX.skew.eB !="NaN") > 1

chr13.skew.eB <- KO.skew.All.eB[chr13Genes, ]
chr13.Allelic.All.eB <- KO.Allelic.All.eB[chr13Genes, ]
loc <- which(chr13.Allelic.All.eB <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chr13.skew.eB[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 2 "non-NaN"s
chr13.rows <- rowSums(chr13.skew.eB !="NaN") > 1

#convert the elements in chrX.skew.eB and chr13.skew.eB into numeric from character
chrX.GeneaNme <- rownames(chrX.skew.eB)
chrX.skew.eB <- sapply(chrX.skew.eB, as.numeric )
chr13.GeneaNme <- rownames(chr13.skew.eB)
chr13.skew.eB <- sapply(chr13.skew.eB, as.numeric )

All.chrX.skew.Mean.eB <- data.frame(skew=rowMeans(chrX.skew.eB, na.rm=T), gene=chrX.GeneaNme, chr="chrX", row=chrX.rows)
KO.chrX.skew.Mean.eB <- All.chrX.skew.Mean.eB[which(All.chrX.skew.Mean.eB$row == "TRUE"), ]
All.chr13.skew.Mean.eB <- data.frame(skew=rowMeans(chr13.skew.eB, na.rm=T), gene=chr13.GeneaNme, chr="chr13", row=chr13.rows)
KO.chr13.skew.Mean.eB <- All.chr13.skew.Mean.eB[which(All.chr13.skew.Mean.eB$row == "TRUE"), ]



rm(list=setdiff(ls(), c("GeneInfo",
                        "CM.F.Allelic.All","CM.F.mus.All","CM.F.cas.All",
                        "MC.F.Allelic.All","MC.F.mus.All","MC.F.cas.All",
                        "KO.F.Allelic.All","KO.F.mus.All","KO.F.cas.All",
                        "MC.chrX.skew.Mean.ZG","MC.chr13.skew.Mean.ZG","MC.chrX.skew.Mean.e2C","MC.chr13.skew.Mean.e2C",
                        "MC.chrX.skew.Mean.l2C","MC.chr13.skew.Mean.l2C","MC.chrX.skew.Mean.N4C","MC.chr13.skew.Mean.N4C",
                        "MC.chrX.skew.Mean.N8C","MC.chr13.skew.Mean.N8C","MC.chrX.skew.Mean.N16C","MC.chr13.skew.Mean.N16C",
                        "MC.chrX.skew.Mean.eB","MC.chr13.skew.Mean.eB",
                        "KO.chrX.skew.Mean.l2C","KO.chr13.skew.Mean.l2C","KO.chrX.skew.Mean.N4C","KO.chr13.skew.Mean.N4C",
                        "KO.chrX.skew.Mean.N8C", "KO.chr13.skew.Mean.N8C", "KO.chrX.skew.Mean.eB","KO.chr13.skew.Mean.eB")))



#CM cross

# get autosomal gene and chrX gene
autoGenes <- rownames(GeneInfo[which(GeneInfo$Chr != "chrX" & GeneInfo$Chr != "chrY"), ])
chrXGenes <- rownames(GeneInfo[which(GeneInfo$Chr == "chrX"), ])
chr13Genes <- rownames(GeneInfo[which(GeneInfo$Chr == "chr13"), ])


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


#l2C
#Filtering out genes with low allelic reads <6
chrX.skew.l2C <- CM.skew.All.l2C[intersect(rownames(CM.skew.All.l2C), chrXGenes), ]
chrX.Allelic.All.l2C <- CM.Allelic.All.l2C[intersect(rownames(CM.Allelic.All.l2C), chrXGenes), ]
loc <- which(chrX.Allelic.All.l2C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chrX.skew.l2C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 2 "non-NaN"s
chrX.rows <- rowSums(chrX.skew.l2C !="NaN") > 1

chr13.skew.l2C <- CM.skew.All.l2C[intersect(rownames(CM.skew.All.l2C), chr13Genes), ]
chr13.Allelic.All.l2C <- CM.Allelic.All.l2C[intersect(rownames(CM.Allelic.All.l2C), chr13Genes), ]
loc <- which(chr13.Allelic.All.l2C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chr13.skew.l2C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 2 "non-NaN"s
chr13.rows <- rowSums(chr13.skew.l2C !="NaN") > 1

#convert the elements in chrX.skew.l2C and chr13.skew.l2C into numeric from character
chrX.GeneaNme <- rownames(chrX.skew.l2C)
chrX.skew.l2C <- sapply(chrX.skew.l2C, as.numeric )
chr13.GeneaNme <- rownames(chr13.skew.l2C)
chr13.skew.l2C <- sapply(chr13.skew.l2C, as.numeric )

All.chrX.skew.Mean.l2C <- data.frame(skew=rowMeans(chrX.skew.l2C, na.rm=T), gene=chrX.GeneaNme, chr="chrX", row=chrX.rows)
CM.chrX.skew.Mean.l2C <- All.chrX.skew.Mean.l2C[which(All.chrX.skew.Mean.l2C$row == "TRUE"), ]
All.chr13.skew.Mean.l2C <- data.frame(skew=rowMeans(chr13.skew.l2C, na.rm=T), gene=chr13.GeneaNme, chr="chr13", row=chr13.rows)
CM.chr13.skew.Mean.l2C <- All.chr13.skew.Mean.l2C[which(All.chr13.skew.Mean.l2C$row == "TRUE"), ]


#4C
#Filtering out genes with low allelic reads
chrX.skew.N4C <- CM.skew.All.N4C[intersect(rownames(CM.skew.All.N4C), chrXGenes), ]
chrX.Allelic.All.N4C <- CM.Allelic.All.N4C[intersect(rownames(CM.Allelic.All.N4C), chrXGenes), ]
loc <- which(chrX.Allelic.All.N4C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chrX.skew.N4C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 2 "non-NaN"s
chrX.rows <- rowSums(chrX.skew.N4C !="NaN") > 1

chr13.skew.N4C <- CM.skew.All.N4C[intersect(rownames(CM.skew.All.N4C), chr13Genes), ]
chr13.Allelic.All.N4C <- CM.Allelic.All.N4C[intersect(rownames(CM.Allelic.All.N4C), chr13Genes), ]
loc <- which(chr13.Allelic.All.N4C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chr13.skew.N4C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 2 "non-NaN"s
chr13.rows <- rowSums(chr13.skew.N4C !="NaN") > 1

#convert the elements in chrX.skew.N4C and chr13.skew.N4C into numeric from character
chrX.GeneaNme <- rownames(chrX.skew.N4C)
chrX.skew.N4C <- sapply(chrX.skew.N4C, as.numeric )
chr13.GeneaNme <- rownames(chr13.skew.N4C)
chr13.skew.N4C <- sapply(chr13.skew.N4C, as.numeric )

All.chrX.skew.Mean.N4C <- data.frame(skew=rowMeans(chrX.skew.N4C, na.rm=T), gene=chrX.GeneaNme, chr="chrX", row=chrX.rows)
CM.chrX.skew.Mean.N4C <- All.chrX.skew.Mean.N4C[which(All.chrX.skew.Mean.N4C$row == "TRUE"), ]
All.chr13.skew.Mean.N4C <- data.frame(skew=rowMeans(chr13.skew.N4C, na.rm=T), gene=chr13.GeneaNme, chr="chr13", row=chr13.rows)
CM.chr13.skew.Mean.N4C <- All.chr13.skew.Mean.N4C[which(All.chr13.skew.Mean.N4C$row == "TRUE"), ]


#8C
#Filtering out genes with low allelic reads
chrX.skew.N8C <- CM.skew.All.N8C[intersect(rownames(CM.skew.All.N8C), chrXGenes), ]
chrX.Allelic.All.N8C <- CM.Allelic.All.N8C[intersect(rownames(CM.Allelic.All.N8C), chrXGenes), ]
loc <- which(chrX.Allelic.All.N8C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chrX.skew.N8C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 2 "non-NaN"s
chrX.rows <- rowSums(chrX.skew.N8C !="NaN") > 1

chr13.skew.N8C <- CM.skew.All.N8C[intersect(rownames(CM.skew.All.N8C), chr13Genes), ]
chr13.Allelic.All.N8C <- CM.Allelic.All.N8C[intersect(rownames(CM.Allelic.All.N8C), chr13Genes), ]
loc <- which(chr13.Allelic.All.N8C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chr13.skew.N8C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect gene rows having at least 2 "non-NaN"s
chr13.rows <- rowSums(chr13.skew.N8C !="NaN") > 1

#convert the elements in chrX.skew.N8C and chr13.skew.N8C into numeric from character
chrX.GeneaNme <- rownames(chrX.skew.N8C)
chrX.skew.N8C <- sapply(chrX.skew.N8C, as.numeric )
chr13.GeneaNme <- rownames(chr13.skew.N8C)
chr13.skew.N8C <- sapply(chr13.skew.N8C, as.numeric )

All.chrX.skew.Mean.N8C <- data.frame(skew=rowMeans(chrX.skew.N8C, na.rm=T), gene=chrX.GeneaNme, chr="chrX", row=chrX.rows)
CM.chrX.skew.Mean.N8C <- All.chrX.skew.Mean.N8C[which(All.chrX.skew.Mean.N8C$row == "TRUE"), ]
All.chr13.skew.Mean.N8C <- data.frame(skew=rowMeans(chr13.skew.N8C, na.rm=T), gene=chr13.GeneaNme, chr="chr13", row=chr13.rows)
CM.chr13.skew.Mean.N8C <- All.chr13.skew.Mean.N8C[which(All.chr13.skew.Mean.N8C$row == "TRUE"), ]


#16C
#Filtering out genes with low allelic reads
chrX.skew.N16C <- CM.skew.All.N16C[intersect(rownames(CM.skew.All.N16C), chrXGenes), ]
chrX.Allelic.All.N16C <- CM.Allelic.All.N16C[intersect(rownames(CM.Allelic.All.N16C), chrXGenes), ]
loc <- which(chrX.Allelic.All.N16C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chrX.skew.N16C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect genes having at least 2 "non-NaN"s
chrX.rows <- rowSums(chrX.skew.N16C !="NaN") > 1

chr13.skew.N16C <- CM.skew.All.N16C[intersect(rownames(CM.skew.All.N16C), chr13Genes), ]
chr13.Allelic.All.N16C <- CM.Allelic.All.N16C[intersect(rownames(CM.Allelic.All.N16C), chr13Genes), ]
loc <- which(chr13.Allelic.All.N16C <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chr13.skew.N16C[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect genes having at least 2 "non-NaN"s
chr13.rows <- rowSums(chr13.skew.N16C !="NaN") > 1

#convert the elements in chrX.skew.N16C and chr13.skew.N16C into numeric from character
chrX.GeneaNme <- rownames(chrX.skew.N16C)
chrX.skew.N16C <- sapply(chrX.skew.N16C, as.numeric )
chr13.GeneaNme <- rownames(chr13.skew.N16C)
chr13.skew.N16C <- sapply(chr13.skew.N16C, as.numeric )

All.chrX.skew.Mean.N16C <- data.frame(skew=rowMeans(chrX.skew.N16C, na.rm=T), gene=chrX.GeneaNme, chr="chrX", row=chrX.rows)
CM.chrX.skew.Mean.N16C <- All.chrX.skew.Mean.N16C[which(All.chrX.skew.Mean.N16C$row == "TRUE"), ]
All.chr13.skew.Mean.N16C <- data.frame(skew=rowMeans(chr13.skew.N16C, na.rm=T), gene=chr13.GeneaNme, chr="chr13", row=chr13.rows)
CM.chr13.skew.Mean.N16C <- All.chr13.skew.Mean.N16C[which(All.chr13.skew.Mean.N16C$row == "TRUE"), ]


#eB
#Filtering out genes with low allelic reads
chrX.skew.eB <- CM.skew.All.eB[intersect(rownames(CM.skew.All.eB), chrXGenes), ]
chrX.Allelic.All.eB <- CM.Allelic.All.eB[intersect(rownames(CM.Allelic.All.eB), chrXGenes), ]
loc <- which(chrX.Allelic.All.eB <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chrX.skew.eB[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect genes having at least 2 "non-NaN"s
chrX.rows <- rowSums(chrX.skew.eB !="NaN") > 1

chr13.skew.eB <- CM.skew.All.eB[intersect(rownames(CM.skew.All.eB), chr13Genes), ]
chr13.Allelic.All.eB <- CM.Allelic.All.eB[intersect(rownames(CM.Allelic.All.eB), chr13Genes), ]
loc <- which(chr13.Allelic.All.eB <= 6, arr.ind=T)
for (i in 1:nrow(loc)) {
  chr13.skew.eB[loc[i,"row"], loc[i,"col"]] <- "NaN"
}
#only collect genes having at least 2 "non-NaN"s
chr13.rows <- rowSums(chr13.skew.eB !="NaN") > 1

#convert the elements in chrX.skew.eB and chr13.skew.eB into numeric from character
chrX.GeneaNme <- rownames(chrX.skew.eB)
chrX.skew.eB <- sapply(chrX.skew.eB, as.numeric )
chr13.GeneaNme <- rownames(chr13.skew.eB)
chr13.skew.eB <- sapply(chr13.skew.eB, as.numeric )

All.chrX.skew.Mean.eB <- data.frame(skew=rowMeans(chrX.skew.eB, na.rm=T), gene=chrX.GeneaNme, chr="chrX", row=chrX.rows)
CM.chrX.skew.Mean.eB <- All.chrX.skew.Mean.eB[which(All.chrX.skew.Mean.eB$row == "TRUE"), ]
All.chr13.skew.Mean.eB <- data.frame(skew=rowMeans(chr13.skew.eB, na.rm=T), gene=chr13.GeneaNme, chr="chr13", row=chr13.rows)
CM.chr13.skew.Mean.eB <- All.chr13.skew.Mean.eB[which(All.chr13.skew.Mean.eB$row == "TRUE"), ]


rm(list=setdiff(ls(), c("GeneInfo",
                        "CM.F.Allelic.All","CM.F.mus.All","CM.F.cas.All",
                        "MC.F.Allelic.All","MC.F.mus.All","MC.F.cas.All",
                        "KO.F.Allelic.All","KO.F.mus.All","KO.F.cas.All",
                        "MC.chrX.skew.Mean.ZG","MC.chr13.skew.Mean.ZG","MC.chrX.skew.Mean.e2C","MC.chr13.skew.Mean.e2C",
                        "MC.chrX.skew.Mean.l2C","MC.chr13.skew.Mean.l2C","MC.chrX.skew.Mean.N4C","MC.chr13.skew.Mean.N4C",
                        "MC.chrX.skew.Mean.N8C","MC.chr13.skew.Mean.N8C","MC.chrX.skew.Mean.N16C","MC.chr13.skew.Mean.N16C",
                        "MC.chrX.skew.Mean.eB","MC.chr13.skew.Mean.eB",
                        "KO.chrX.skew.Mean.l2C","KO.chr13.skew.Mean.l2C","KO.chrX.skew.Mean.N4C","KO.chr13.skew.Mean.N4C",
                        "KO.chrX.skew.Mean.N8C", "KO.chr13.skew.Mean.N8C", "KO.chrX.skew.Mean.eB","KO.chr13.skew.Mean.eB",
                        "CM.chrX.skew.Mean.l2C","CM.chr13.skew.Mean.l2C","CM.chrX.skew.Mean.N4C","CM.chr13.skew.Mean.N4C",
                        "CM.chrX.skew.Mean.N8C","CM.chr13.skew.Mean.N8C","CM.chrX.skew.Mean.N16C","CM.chr13.skew.Mean.N16C",
                        "CM.chrX.skew.Mean.eB","CM.chr13.skew.Mean.eB")))

