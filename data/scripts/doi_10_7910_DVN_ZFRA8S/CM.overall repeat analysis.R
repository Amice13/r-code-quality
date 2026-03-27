
source("Functions.R")
#Preload and process the original stat files for each stage
repeat.type.family <- read.table(file="processed/repeat_class_family", sep="\t")
#Sort the repeats based on their classes
repeat.type.family <- repeat.type.family[order(repeat.type.family[ ,2]), ]
#some TEs have duplicated names, although they belong to different repeat types. 3 TEs were removed.
repeat.type.family <- repeat.type.family[!duplicated(repeat.type.family[[1]]), ]
rownames(repeat.type.family) <- repeat.type.family$V1



############ imprinted XCI CM cross ##########################


#late2cell 

#run 'parseTbl.totalMatrix.sh rep_CM_late2cell_R_output.txt' to generate the 3 input files.

#"CM.X_skew" is saved for use in the downstream script.
rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average","CM.X_skew")))
All.Data <- read.table(file="processed/CM_late2cell.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="processed/CM_late2cell.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="processed/CM_late2cell.repColNames.txt", sep="\t")
rownames(All.Data) <- All.rowname[ ,1]
colnames(All.Data) <- All.colname[ ,1]
All.Data <- t(All.Data)
matched.line <- match(rownames(repeat.type.family), rownames(All.Data))
All.Data <- All.Data[matched.line[!is.na(matched.line)], ]

combined.Col <- seq(from=6, to=120, by=6)
late2cell.combined.Data <- All.Data[ ,combined.Col]
All.Data <- All.Data[ ,setdiff(seq(1:ncol(All.Data)), combined.Col)]

#pull out repeat counts from chrX
late2cell.chrX <- All.Data[ ,c(96:100)]
colnames(late2cell.chrX) <- c("late2cell_1","late2cell_2","late2cell_3","late2cell_4","late2cell_5")
#filter out repeats expressed in less than 5 autosomes
All.Data <- All.Data[which(rowSums(!is.nan(All.Data[ ,c(1:95)])) >=5 | rowSums(!is.nan(All.Data[ ,c(96:100)])) >=1), ]
#Average the skewing for auto, chrX and chr1.
late2cell.mean.skew <- Chr_Average(All.Data, 5)
late2cell.skew <- data.frame(X.chr_Average(All.Data, 5), stage="late2C")
late2cell.XvsAuto.skew <- data.frame(Auto.X_Average(All.Data, 5), stage="late2C")




#For 4cell stage
#clean up unsed variables
rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average","CM.X_skew",
                        "late2cell.skew","late2cell.chrX","late2cell.combined.Data","late2cell.mean.skew","late2cell.XvsAuto.skew")))

#run 'parseTbl.totalMatrix.sh rep_CM_4cell_R_output.txt' to generate the 3 input files.

All.Data <- read.table(file="processed/CM_4cell.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="processed/CM_4cell.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="processed/CM_4cell.repColNames.txt", sep="\t")
rownames(All.Data) <- All.rowname[ ,1]
colnames(All.Data) <- All.colname[ ,1]
All.Data <- t(All.Data)
matched.line <- match(rownames(repeat.type.family), rownames(All.Data))
All.Data <- All.Data[matched.line[!is.na(matched.line)], ]

combined.Col <- seq(from=6, to=120, by=6)
N4cell.combined.Data <- All.Data[ ,combined.Col]
All.Data <- All.Data[ ,setdiff(seq(1:ncol(All.Data)), combined.Col)]

#pull out repeat counts from chrX
N4cell.chrX <- All.Data[ ,c(96:100)]
colnames(N4cell.chrX) <- c("N4cell_1","N4cell_2","N4cell_3","N4cell_4","N4cell_5")
#filter out repeats expressed in less than 5 autosomes
All.Data <- All.Data[which(rowSums(!is.nan(All.Data[ ,c(1:95)])) >=5 | rowSums(!is.nan(All.Data[ ,c(96:100)])) >=1), ]
#Average the skewing for auto, chrX and chr1.
N4cell.mean.skew <- Chr_Average(All.Data, 5)
N4cell.skew <- data.frame(X.chr_Average(All.Data, 5), stage="4C")
N4cell.XvsAuto.skew <- data.frame(Auto.X_Average(All.Data, 5), stage="4C")





#For 8cell stage
#clean up unsed variables
rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average","CM.X_skew",
                        "late2cell.skew","late2cell.chrX","late2cell.combined.Data","late2cell.mean.skew","late2cell.XvsAuto.skew",
                        "N4cell.skew","N4cell.chrX","N4cell.combined.Data","N4cell.mean.skew","N4cell.XvsAuto.skew")))

#run 'parseTbl.totalMatrix.sh rep_CM_8cell_R_output.txt' to generate the 3 input files.

All.Data <- read.table(file="processed/CM_8cell.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="processed/CM_8cell.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="processed/CM_8cell.repColNames.txt", sep="\t")
rownames(All.Data) <- All.rowname[ ,1]
colnames(All.Data) <- All.colname[ ,1]
All.Data <- t(All.Data)
matched.line <- match(rownames(repeat.type.family), rownames(All.Data))
All.Data <- All.Data[matched.line[!is.na(matched.line)], ]

combined.Col <- seq(from=10, to=200, by=10)
N8cell.combined.Data <- All.Data[ ,combined.Col]
All.Data <- All.Data[ ,setdiff(seq(1:ncol(All.Data)), combined.Col)]

#pull out repeat counts from chrX
N8cell.chrX <- All.Data[ ,c(172:180)]
colnames(N8cell.chrX) <- c("N8cell_1","N8cell_2","N8cell_3","N8cell_4","N8cell_5","N8cell_6","N8cell_7","N8cell_8","N8cell_9")
#filter out repeats expressed in less than 5 autosomes
All.Data <- All.Data[which(rowSums(!is.nan(All.Data[ ,c(1:171)])) >=5 | rowSums(!is.nan(All.Data[ ,c(172:180)])) >=1), ]
#Average the skewing for auto, chrX and chr1.
N8cell.mean.skew <- Chr_Average(All.Data, 9)
N8cell.skew <- data.frame(X.chr_Average(All.Data, 9), stage="8C")
N8cell.XvsAuto.skew <- data.frame(Auto.X_Average(All.Data, 9), stage="8C")




#For 16cell stage
#clean up unsed variables
rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average","CM.X_skew",
                        "late2cell.skew","late2cell.chrX","late2cell.combined.Data","late2cell.mean.skew","late2cell.XvsAuto.skew",
                        "N4cell.skew","N4cell.chrX","N4cell.combined.Data","N4cell.mean.skew","N4cell.XvsAuto.skew",
                        "N8cell.skew","N8cell.chrX","N8cell.combined.Data","N8cell.mean.skew","N8cell.XvsAuto.skew")))

#run 'parseTbl.totalMatrix.sh rep_CM_16cell_R_output.txt' to generate the 3 input files.

All.Data <- read.table(file="processed/CM_16cell.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="processed/CM_16cell.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="processed/CM_16cell.repColNames.txt", sep="\t")
rownames(All.Data) <- All.rowname[ ,1]
colnames(All.Data) <- All.colname[ ,1]
All.Data <- t(All.Data)
matched.line <- match(rownames(repeat.type.family), rownames(All.Data))
All.Data <- All.Data[matched.line[!is.na(matched.line)], ]

combined.Col <- seq(from=8, to=160, by=8)
N16cell.combined.Data <- All.Data[ ,combined.Col]
All.Data <- All.Data[ ,setdiff(seq(1:ncol(All.Data)), combined.Col)]

#pull out repeat counts from chrX
N16cell.chrX <- All.Data[ ,c(134:140)]
colnames(N16cell.chrX) <- c("N16cell_1","N16cell_2","N16cell_3","N16cell_4","N16cell_5","N16cell_6","N16cell_7")
#filter out repeats expressed in less than 5 autosomes
All.Data <- All.Data[which(rowSums(!is.nan(All.Data[ ,c(1:133)])) >=5 | rowSums(!is.nan(All.Data[ ,c(134:140)])) >=1), ]
#Average the skewing for auto, chrX and chr1.
N16cell.mean.skew <- Chr_Average(All.Data, 7)
N16cell.skew <- data.frame(X.chr_Average(All.Data, 7), stage="16C")
N16cell.XvsAuto.skew <- data.frame(Auto.X_Average(All.Data, 7), stage="16C")




#For earlyBlast stage
#clean up unsed variables
rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average","CM.X_skew",
                        "late2cell.skew","late2cell.chrX","late2cell.combined.Data","late2cell.mean.skew","late2cell.XvsAuto.skew",
                        "N4cell.skew","N4cell.chrX","N4cell.combined.Data","N4cell.mean.skew","N4cell.XvsAuto.skew",
                        "N8cell.skew","N8cell.chrX","N8cell.combined.Data","N8cell.mean.skew","N8cell.XvsAuto.skew",
                        "N16cell.skew","N16cell.chrX","N16cell.combined.Data","N16cell.mean.skew","N16cell.XvsAuto.skew")))

#run 'parseTbl.totalMatrix.sh rep_CM_earlyBlast_R_output.txt' to generate the 3 input files.

All.Data <- read.table(file="processed/CM_earlyBlast.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="processed/CM_earlyBlast.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="processed/CM_earlyBlast.repColNames.txt", sep="\t")
rownames(All.Data) <- All.rowname[ ,1]
colnames(All.Data) <- All.colname[ ,1]
All.Data <- t(All.Data)
matched.line <- match(rownames(repeat.type.family), rownames(All.Data))
All.Data <- All.Data[matched.line[!is.na(matched.line)], ]

combined.Col <- seq(from=6, to=120, by=6)
earlyBlast.combined.Data <- All.Data[ ,combined.Col]
All.Data <- All.Data[ ,setdiff(seq(1:ncol(All.Data)), combined.Col)]

#pull out repeat counts from chrX
earlyBlast.chrX <- All.Data[ ,c(96:100)]
colnames(earlyBlast.chrX) <- c("earlyBlast_1","earlyBlast_2","earlyBlast_3","earlyBlast_4","earlyBlast_5")
#filter out repeats expressed in less than 5 autosomes
All.Data <- All.Data[which(rowSums(!is.nan(All.Data[ ,c(1:95)])) >=5 | rowSums(!is.nan(All.Data[ ,c(96:100)])) >=1), ]
#Average the skewing for auto, chrX and chr1.
earlyBlast.mean.skew <- Chr_Average(All.Data, 5)
earlyBlast.skew <- data.frame(X.chr_Average(All.Data, 5), stage="eB")
earlyBlast.XvsAuto.skew <- data.frame(Auto.X_Average(All.Data, 5), stage="eB")




rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average","CM.X_skew",
                        "late2cell.skew","late2cell.chrX","late2cell.combined.Data","late2cell.mean.skew","late2cell.XvsAuto.skew",
                        "N4cell.skew","N4cell.chrX","N4cell.combined.Data","N4cell.mean.skew","N4cell.XvsAuto.skew",
                        "N8cell.skew","N8cell.chrX","N8cell.combined.Data","N8cell.mean.skew","N8cell.XvsAuto.skew",
                        "N16cell.skew","N16cell.chrX","N16cell.combined.Data","N16cell.mean.skew","N16cell.XvsAuto.skew",
                        "earlyBlast.skew","earlyBlast.chrX","earlyBlast.combined.Data","earlyBlast.mean.skew","earlyBlast.XvsAuto.skew")))




####################### imprinted XCI Xist-KO cross ##############################


#KO_late2cell

#run 'parseTbl.totalMatrix.sh rep_KO_late2cell_MUS.CAST_R_output.txt' to generate the 3 input files.

#clean up unused variables
rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average","CM.X_skew",
                        "late2cell.skew","late2cell.chrX","late2cell.combined.Data","late2cell.mean.skew","late2cell.XvsAuto.skew",
                        "N4cell.skew","N4cell.chrX","N4cell.combined.Data","N4cell.mean.skew","N4cell.XvsAuto.skew",
                        "N8cell.skew","N8cell.chrX","N8cell.combined.Data","N8cell.mean.skew","N8cell.XvsAuto.skew",
                        "N16cell.skew","N16cell.chrX","N16cell.combined.Data","N16cell.mean.skew","N16cell.XvsAuto.skew",
                        "earlyBlast.skew","earlyBlast.chrX","earlyBlast.combined.Data","earlyBlast.mean.skew","earlyBlast.XvsAuto.skew")))

All.Data <- read.table(file="processed/KO_late2cell_MUS.CAST.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="processed/KO_late2cell_MUS.CAST.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="processed/KO_late2cell_MUS.CAST.repColNames.txt", sep="\t")
rownames(All.Data) <- All.rowname[ ,1]
colnames(All.Data) <- All.colname[ ,1]
All.Data <- t(All.Data)
matched.line <- match(rownames(repeat.type.family), rownames(All.Data))
All.Data <- All.Data[matched.line[!is.na(matched.line)], ]

#combined.Col <- seq(from=7, to=140, by=7)
#KO_late2cell.combined.Data <- All.Data[ ,combined.Col]
#All.Data <- All.Data[ ,setdiff(seq(1:ncol(All.Data)), combined.Col)]

#pull out repeat counts from chrX
KO_late2cell.chrX <- All.Data[ ,c(115:120)]
colnames(KO_late2cell.chrX) <- c("late2cell_1","late2cell_2","late2cell_3","late2cell_4","late2cell_5","late2cell_6")
#filter out repeats expressed in less than 5 autosomes
All.Data <- All.Data[which(rowSums(!is.nan(All.Data[ ,c(1:114)])) >=5 | rowSums(!is.nan(All.Data[ ,c(115:120)])) >=1), ]
#Average the skewing for auto, chrX and chr1.
KO_late2cell.mean.skew <- Chr_Average(All.Data, 6)
KO_late2cell.skew <- data.frame(X.chr_Average(All.Data, 6), stage="late2C")
KO_late2cell.XvsAuto.skew <- data.frame(Auto.X_Average(All.Data, 6), stage="late2C")



#KO_4cell
#clean up unsed variables
rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average","CM.X_skew",
                        "late2cell.skew","late2cell.chrX","late2cell.combined.Data","late2cell.mean.skew","late2cell.XvsAuto.skew",
                        "N4cell.skew","N4cell.chrX","N4cell.combined.Data","N4cell.mean.skew","N4cell.XvsAuto.skew",
                        "N8cell.skew","N8cell.chrX","N8cell.combined.Data","N8cell.mean.skew","N8cell.XvsAuto.skew",
                        "N16cell.skew","N16cell.chrX","N16cell.combined.Data","N16cell.mean.skew","N16cell.XvsAuto.skew",
                        "earlyBlast.skew","earlyBlast.chrX","earlyBlast.combined.Data","earlyBlast.mean.skew","earlyBlast.XvsAuto.skew",
                        "KO_late2cell.skew","KO_late2cell.chrX","KO_late2cell.combined.Data","KO_late2cell.mean.skew","KO_late2cell.XvsAuto.skew")))

#run 'parseTbl.totalMatrix.sh rep_KO_4cell_MUS.CAST_R_output.txt' to generate the 3 input files.

All.Data <- read.table(file="processed/KO_4cell_MUS.CAST.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="processed/KO_4cell_MUS.CAST.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="processed/KO_4cell_MUS.CAST.repColNames.txt", sep="\t")
rownames(All.Data) <- All.rowname[ ,1]
colnames(All.Data) <- All.colname[ ,1]
All.Data <- t(All.Data)
matched.line <- match(rownames(repeat.type.family), rownames(All.Data))
All.Data <- All.Data[matched.line[!is.na(matched.line)], ]

#combined.Col <- seq(from=7, to=140, by=7)
#KO_4cell.combined.Data <- All.Data[ ,combined.Col]
#All.Data <- All.Data[ ,setdiff(seq(1:ncol(All.Data)), combined.Col)]

#pull out repeat counts from chrX
KO_4cell.chrX <- All.Data[ ,c(115:120)]
colnames(KO_4cell.chrX) <- c("N4cell_1","N4cell_2","N4cell_3","N4cell_4","N4cell_5","N4cell_6")
#filter out repeats expressed in less than 5 autosomes
All.Data <- All.Data[which(rowSums(!is.nan(All.Data[ ,c(1:114)])) >=5 | rowSums(!is.nan(All.Data[ ,c(115:120)])) >=1), ]
#Average the skewing for auto, chrX and chr1.
KO_4cell.mean.skew <- Chr_Average(All.Data, 6)
KO_4cell.skew <- data.frame(X.chr_Average(All.Data, 6), stage="4C")
KO_4cell.XvsAuto.skew <- data.frame(Auto.X_Average(All.Data, 6), stage="4C")


#KO_8cell
#clean up unsed variables
rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average","CM.X_skew",
                        "late2cell.skew","late2cell.chrX","late2cell.combined.Data","late2cell.mean.skew","late2cell.XvsAuto.skew",
                        "N4cell.skew","N4cell.chrX","N4cell.combined.Data","N4cell.mean.skew","N4cell.XvsAuto.skew",
                        "N8cell.skew","N8cell.chrX","N8cell.combined.Data","N8cell.mean.skew","N8cell.XvsAuto.skew",
                        "N16cell.skew","N16cell.chrX","N16cell.combined.Data","N16cell.mean.skew","N16cell.XvsAuto.skew",
                        "earlyBlast.skew","earlyBlast.chrX","earlyBlast.combined.Data","earlyBlast.mean.skew","earlyBlast.XvsAuto.skew",
                        "KO_late2cell.skew","KO_late2cell.chrX","KO_late2cell.combined.Data","KO_late2cell.mean.skew","KO_late2cell.XvsAuto.skew",
                        "KO_4cell.skew","KO_4cell.chrX","KO_4cell.combined.Data","KO_4cell.mean.skew","KO_4cell.XvsAuto.skew")))

#run 'parseTbl.totalMatrix.sh rep_KO_8cell_MUS.CAST_R_output.txt' to generate the 3 input files.

All.Data <- read.table(file="processed/KO_8cell_MUS.CAST.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="processed/KO_8cell_MUS.CAST.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="processed/KO_8cell_MUS.CAST.repColNames.txt", sep="\t")
rownames(All.Data) <- All.rowname[ ,1]
colnames(All.Data) <- All.colname[ ,1]
All.Data <- t(All.Data)
matched.line <- match(rownames(repeat.type.family), rownames(All.Data))
All.Data <- All.Data[matched.line[!is.na(matched.line)], ]

#combined.Col <- seq(from=7, to=140, by=7)
#KO_8cell.combined.Data <- All.Data[ ,combined.Col]
#All.Data <- All.Data[ ,setdiff(seq(1:ncol(All.Data)), combined.Col)]

#pull out repeat counts from chrX
KO_8cell.chrX <- All.Data[ ,c(115:120)]
colnames(KO_8cell.chrX) <- c("N8cell_1","N8cell_2","N8cell_3","N8cell_4","N8cell_5","N8cell_6")
#filter out repeats expressed in less than 5 autosomes
All.Data <- All.Data[which(rowSums(!is.nan(All.Data[ ,c(1:114)])) >=5 | rowSums(!is.nan(All.Data[ ,c(115:120)])) >=1), ]
#Average the skewing for auto, chrX and chr1.
KO_8cell.mean.skew <- Chr_Average(All.Data, 6)
KO_8cell.skew <- data.frame(X.chr_Average(All.Data, 6), stage="8C")
KO_8cell.XvsAuto.skew <- data.frame(Auto.X_Average(All.Data, 6), stage="8C")





#earlyBlast
#clean up unsed variables
rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average","CM.X_skew",
                        "late2cell.skew","late2cell.chrX","late2cell.combined.Data","late2cell.mean.skew","late2cell.XvsAuto.skew",
                        "N4cell.skew","N4cell.chrX","N4cell.combined.Data","N4cell.mean.skew","N4cell.XvsAuto.skew",
                        "N8cell.skew","N8cell.chrX","N8cell.combined.Data","N8cell.mean.skew","N8cell.XvsAuto.skew",
                        "N16cell.skew","N16cell.chrX","N16cell.combined.Data","N16cell.mean.skew","N16cell.XvsAuto.skew",
                        "earlyBlast.skew","earlyBlast.chrX","earlyBlast.combined.Data","earlyBlast.mean.skew","earlyBlast.XvsAuto.skew",
                        "KO_late2cell.skew","KO_late2cell.chrX","KO_late2cell.combined.Data","KO_late2cell.mean.skew","KO_late2cell.XvsAuto.skew",
                        "KO_4cell.skew","KO_4cell.chrX","KO_4cell.combined.Data","KO_4cell.mean.skew","KO_4cell.XvsAuto.skew",
                        "KO_8cell.skew","KO_8cell.chrX","KO_8cell.combined.Data","KO_8cell.mean.skew","KO_8cell.XvsAuto.skew")))

#run 'parseTbl.totalMatrix.sh rep_KO_earlyBlast_MUS.CAST_R_output.txt' to generate the 3 input files.

All.Data <- read.table(file="processed/KO_earlyBlast_MUS.CAST.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="processed/KO_earlyBlast_MUS.CAST.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="processed/KO_earlyBlast_MUS.CAST.repColNames.txt", sep="\t")
rownames(All.Data) <- All.rowname[ ,1]
colnames(All.Data) <- All.colname[ ,1]
All.Data <- t(All.Data)
matched.line <- match(rownames(repeat.type.family), rownames(All.Data))
All.Data <- All.Data[matched.line[!is.na(matched.line)], ]

#combined.Col <- seq(from=8, to=160, by=8)
#KO_earlyBlast.combined.Data <- All.Data[ ,combined.Col]
#All.Data <- All.Data[ ,setdiff(seq(1:ncol(All.Data)), combined.Col)]

#pull out repeat counts from chrX
KO_earlyBlast.chrX <- All.Data[ ,c(134:140)]
colnames(KO_earlyBlast.chrX) <- c("earlyBlast_1","earlyBlast_2","earlyBlast_3","earlyBlast_4","earlyBlast_5","earlyBlast_6","earlyBlast_7")
#filter out repeats expressed in less than 5 autosomes
All.Data <- All.Data[which(rowSums(!is.nan(All.Data[ ,c(1:133)])) >=5 | rowSums(!is.nan(All.Data[ ,c(134:140)])) >=1), ]
#Average the skewing for auto, chrX and chr1.
KO_earlyBlast.mean.skew <- Chr_Average(All.Data, 7)
KO_earlyBlast.skew <- data.frame(X.chr_Average(All.Data, 7), stage="eB")
KO_earlyBlast.XvsAuto.skew <- data.frame(Auto.X_Average(All.Data, 7), stage="eB")




rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average","CM.X_skew",
                        "late2cell.skew","late2cell.chrX","late2cell.combined.Data","late2cell.mean.skew","late2cell.XvsAuto.skew",
                        "N4cell.skew","N4cell.chrX","N4cell.combined.Data","N4cell.mean.skew","N4cell.XvsAuto.skew",
                        "N8cell.skew","N8cell.chrX","N8cell.combined.Data","N8cell.mean.skew","N8cell.XvsAuto.skew",
                        "N16cell.skew","N16cell.chrX","N16cell.combined.Data","N16cell.mean.skew","N16cell.XvsAuto.skew",
                        "earlyBlast.skew","earlyBlast.chrX","earlyBlast.combined.Data","earlyBlast.mean.skew","earlyBlast.XvsAuto.skew",
                        "KO_late2cell.skew","KO_late2cell.chrX","KO_late2cell.combined.Data","KO_late2cell.mean.skew","KO_late2cell.XvsAuto.skew",
                        "KO_4cell.skew","KO_4cell.chrX","KO_4cell.combined.Data","KO_4cell.mean.skew","KO_4cell.XvsAuto.skew",
                        "KO_8cell.skew","KO_8cell.chrX","KO_8cell.combined.Data","KO_8cell.mean.skew","KO_8cell.XvsAuto.skew",
                        "KO_earlyBlast.skew","KO_earlyBlast.chrX","KO_earlyBlast.combined.Data","KO_earlyBlast.mean.skew","KO_earlyBlast.XvsAuto.skew")))
