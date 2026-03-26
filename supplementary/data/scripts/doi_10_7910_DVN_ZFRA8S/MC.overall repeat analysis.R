
source("Functions.R")
#Preload and process the original stat files for each stage
repeat.type.family <- read.table(file="processed/repeat_class_family", sep="\t")
#Sort the repeats based on their classes
repeat.type.family <- repeat.type.family[order(repeat.type.family[ ,2]), ]
#some TEs have duplicated names, although they belong to different repeat types. 3 TEs were removed.
repeat.type.family <- repeat.type.family[!duplicated(repeat.type.family[[1]]), ]
rownames(repeat.type.family) <- repeat.type.family$V1


############ imprinted XCI ##########################



#For zygote stage
#clean up unsed variables
rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "Auto.X_Average","X.chr_Average")))

#run 'parseTbl.totalMatrix.sh rep_zygote_R_output.txt' to generate the 3 input files.

All.Data <- read.table(file="processed/MC.zygote.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="processed/MC.zygote.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="processed/MC.zygote.repColNames.txt", sep="\t")
rownames(All.Data) <- All.rowname[ ,1]
colnames(All.Data) <- All.colname[ ,1]
All.Data <- t(All.Data)
matched.line <- match(rownames(repeat.type.family), rownames(All.Data))
All.Data <- All.Data[matched.line[!is.na(matched.line)], ]

combined.Col <- seq(from=5, to=100, by=5)
zygote.combined.Data <- All.Data[ ,combined.Col]
All.Data <- All.Data[ ,setdiff(seq(1:ncol(All.Data)), combined.Col)]

#pull out repeat counts from chrX
zygote.chrX <- All.Data[ ,c(77:80)]
colnames(zygote.chrX) <- c("zygote_1", "zygote_2","zygote_3","zygote_4")
#filter out repeats expressed less than 5 times in autosomes or less than 1 in chrX
All.Data <- All.Data[which(rowSums(!is.nan(All.Data[ ,c(1:76)])) >=5 | rowSums(!is.nan(All.Data[ ,c(77:80)])) >=1), ]
#Average the skewing for auto, chrX and chr1.
zygote.mean.skew <- Chr_Average(All.Data, 4)
zygote.skew <- data.frame(X.chr_Average(All.Data, 4), stage="ZG")
zygote.XvsAuto.skew <- data.frame(Auto.X_Average(All.Data, 4), stage="ZG")




#For early2cell stage
rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average",
                        "zygote.skew","zygote.chrX","zygote.combined.Data","zygote.mean.skew","zygote.XvsAuto.skew")))

#run 'parseTbl.totalMatrix.sh rep_early2cell_R_output.txt' to generate the 3 input files.

All.Data <- read.table(file="processed/MC.early2cell.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="processed/MC.early2cell.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="processed/MC.early2cell.repColNames.txt", sep="\t")
rownames(All.Data) <- All.rowname[ ,1]
colnames(All.Data) <- All.colname[ ,1]
All.Data <- t(All.Data)
matched.line <- match(rownames(repeat.type.family), rownames(All.Data))
All.Data <- All.Data[matched.line[!is.na(matched.line)], ]

combined.Col <- seq(from=4, to=80, by=4)
early2cell.combined.Data <- All.Data[ ,combined.Col]
All.Data <- All.Data[ ,setdiff(seq(1:ncol(All.Data)), combined.Col)]

#pull out repeat counts from chrX
early2cell.chrX <- All.Data[ ,c(58:60)]
colnames(early2cell.chrX) <- c("early2cell_1","early2cell_2","early2cell_3")
#filter out repeats expressed in less than 5 samples in autosomes
All.Data <- All.Data[which(rowSums(!is.nan(All.Data[ ,c(1:57)])) >=5 | rowSums(!is.nan(All.Data[ ,c(58:60)])) >=1), ]
#Average the skewing for auto, chrX and chr1.
early2cell.mean.skew <- Chr_Average(All.Data, 3)
early2cell.skew <- data.frame(X.chr_Average(All.Data, 3), stage="early2C")
early2cell.XvsAuto.skew <- data.frame(Auto.X_Average(All.Data, 3), stage="early2C")




#late2cell 

#run 'parseTbl.totalMatrix.sh rep_late2cell_R_output.txt' to generate the 3 input files.

rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average",
                        "zygote.skew","zygote.chrX","zygote.combined.Data","zygote.mean.skew","zygote.XvsAuto.skew",
                        "early2cell.skew","early2cell.chrX","early2cell.combined.Data","early2cell.mean.skew","early2cell.XvsAuto.skew")))
All.Data <- read.table(file="processed/MC.late2cell.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="processed/MC.late2cell.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="processed/MC.late2cell.repColNames.txt", sep="\t")
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
#filter out repeats expressed in less than 5 samples in autosomes
All.Data <- All.Data[which(rowSums(!is.nan(All.Data[ ,c(1:95)])) >=5 | rowSums(!is.nan(All.Data[ ,c(96:100)])) >=1), ]
#Average the skewing for auto, chrX and chr1.
late2cell.mean.skew <- Chr_Average(All.Data, 5)
late2cell.skew <- data.frame(X.chr_Average(All.Data, 5), stage="late2C")
late2cell.XvsAuto.skew <- data.frame(Auto.X_Average(All.Data, 5), stage="late2C")




#For 4cell stage
#clean up unsed variables
rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average",
                        "zygote.skew","zygote.chrX","zygote.combined.Data","zygote.mean.skew","zygote.XvsAuto.skew",
                        "early2cell.skew","early2cell.chrX","early2cell.combined.Data","early2cell.mean.skew","early2cell.XvsAuto.skew",
                        "late2cell.skew","late2cell.chrX","late2cell.combined.Data","late2cell.mean.skew","late2cell.XvsAuto.skew")))

#run 'parseTbl.totalMatrix.sh rep_4cell_R_output.txt' to generate the 3 input files.

All.Data <- read.table(file="processed/MC.4cell.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="processed/MC.4cell.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="processed/MC.4cell.repColNames.txt", sep="\t")
rownames(All.Data) <- All.rowname[ ,1]
colnames(All.Data) <- All.colname[ ,1]
All.Data <- t(All.Data)
matched.line <- match(rownames(repeat.type.family), rownames(All.Data))
All.Data <- All.Data[matched.line[!is.na(matched.line)], ]

combined.Col <- seq(from=7, to=140, by=7)
N4cell.combined.Data <- All.Data[ ,combined.Col]
All.Data <- All.Data[ ,setdiff(seq(1:ncol(All.Data)), combined.Col)]

#pull out repeat counts from chrX
N4cell.chrX <- All.Data[ ,c(115:120)]
colnames(N4cell.chrX) <- c("N4cell_1","N4cell_2","N4cell_3","N4cell_4","N4cell_5","N4cell_6")
#filter out repeats expressed in less than 5 samples in autosomes
All.Data <- All.Data[which(rowSums(!is.nan(All.Data[ ,c(1:114)])) >=5 | rowSums(!is.nan(All.Data[ ,c(115:120)])) >=1), ]
#Average the skewing for auto, chrX and chr1.
N4cell.mean.skew <- Chr_Average(All.Data, 6)
N4cell.skew <- data.frame(X.chr_Average(All.Data, 6), stage="4C")
N4cell.XvsAuto.skew <- data.frame(Auto.X_Average(All.Data, 6), stage="4C")





#For 8cell stage
#clean up unsed variables
rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average",
                        "zygote.skew","zygote.chrX","zygote.combined.Data","zygote.mean.skew","zygote.XvsAuto.skew",
                        "early2cell.skew","early2cell.chrX","early2cell.combined.Data","early2cell.mean.skew","early2cell.XvsAuto.skew",
                        "late2cell.skew","late2cell.chrX","late2cell.combined.Data","late2cell.mean.skew","late2cell.XvsAuto.skew",
                        "N4cell.skew","N4cell.chrX","N4cell.combined.Data","N4cell.mean.skew","N4cell.XvsAuto.skew")))

#run 'parseTbl.totalMatrix.sh rep_8cell_R_output.txt' to generate the 3 input files.

All.Data <- read.table(file="processed/MC.8cell.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="processed/MC.8cell.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="processed/MC.8cell.repColNames.txt", sep="\t")
rownames(All.Data) <- All.rowname[ ,1]
colnames(All.Data) <- All.colname[ ,1]
All.Data <- t(All.Data)
matched.line <- match(rownames(repeat.type.family), rownames(All.Data))
All.Data <- All.Data[matched.line[!is.na(matched.line)], ]

combined.Col <- seq(from=7, to=140, by=7)
N8cell.combined.Data <- All.Data[ ,combined.Col]
All.Data <- All.Data[ ,setdiff(seq(1:ncol(All.Data)), combined.Col)]

#pull out repeat counts from chrX
N8cell.chrX <- All.Data[ ,c(115:120)]
colnames(N8cell.chrX) <- c("N8cell_1","N8cell_2","N8cell_3","N8cell_4","N8cell_5","N8cell_6")
#filter out repeats expressed in less than 5 samples in autosomes
All.Data <- All.Data[which(rowSums(!is.nan(All.Data[ ,c(1:114)])) >=5 | rowSums(!is.nan(All.Data[ ,c(115:120)])) >=1), ]
#Average the skewing for auto, chrX and chr1.
N8cell.mean.skew <- Chr_Average(All.Data, 6)
N8cell.skew <- data.frame(X.chr_Average(All.Data, 6), stage="8C")
N8cell.XvsAuto.skew <- data.frame(Auto.X_Average(All.Data, 6), stage="8C")




#For 16cell stage
#clean up unsed variables
rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average",
                        "zygote.skew","zygote.chrX","zygote.combined.Data","zygote.mean.skew","zygote.XvsAuto.skew",
                        "early2cell.skew","early2cell.chrX","early2cell.combined.Data","early2cell.mean.skew","early2cell.XvsAuto.skew",
                        "late2cell.skew","late2cell.chrX","late2cell.combined.Data","late2cell.mean.skew","late2cell.XvsAuto.skew",
                        "N4cell.skew","N4cell.chrX","N4cell.combined.Data","N4cell.mean.skew","N4cell.XvsAuto.skew",
                        "N8cell.skew","N8cell.chrX","N8cell.combined.Data","N8cell.mean.skew","N8cell.XvsAuto.skew")))

#run 'parseTbl.totalMatrix.sh rep_16cell_R_output.txt' to generate the 3 input files.

All.Data <- read.table(file="processed/MC.16cell.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="processed/MC.16cell.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="processed/MC.16cell.repColNames.txt", sep="\t")
rownames(All.Data) <- All.rowname[ ,1]
colnames(All.Data) <- All.colname[ ,1]
All.Data <- t(All.Data)
matched.line <- match(rownames(repeat.type.family), rownames(All.Data))
All.Data <- All.Data[matched.line[!is.na(matched.line)], ]

combined.Col <- seq(from=6, to=120, by=6)
N16cell.combined.Data <- All.Data[ ,combined.Col]
All.Data <- All.Data[ ,setdiff(seq(1:ncol(All.Data)), combined.Col)]

#pull out repeat counts from chrX
N16cell.chrX <- All.Data[ ,c(96:100)]
colnames(N16cell.chrX) <- c("N16cell_1","N16cell_2","N16cell_3","N16cell_4","N16cell_5")
#filter out repeats expressed in less than 5 samples in autosomes
All.Data <- All.Data[which(rowSums(!is.nan(All.Data[ ,c(1:95)])) >=5 | rowSums(!is.nan(All.Data[ ,c(96:100)])) >=1), ]
#Average the skewing for auto, chrX and chr1.
N16cell.mean.skew <- Chr_Average(All.Data, 5)
N16cell.skew <- data.frame(X.chr_Average(All.Data, 5), stage="16C")
N16cell.XvsAuto.skew <- data.frame(Auto.X_Average(All.Data, 5), stage="16C")




#For earlyBlast stage
#clean up unsed variables
rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average",
                        "zygote.skew","zygote.chrX","zygote.combined.Data","zygote.mean.skew","zygote.XvsAuto.skew",
                        "early2cell.skew","early2cell.chrX","early2cell.combined.Data","early2cell.mean.skew","early2cell.XvsAuto.skew",
                        "late2cell.skew","late2cell.chrX","late2cell.combined.Data","late2cell.mean.skew","late2cell.XvsAuto.skew",
                        "N4cell.skew","N4cell.chrX","N4cell.combined.Data","N4cell.mean.skew","N4cell.XvsAuto.skew",
                        "N8cell.skew","N8cell.chrX","N8cell.combined.Data","N8cell.mean.skew","N8cell.XvsAuto.skew",
                        "N16cell.skew","N16cell.chrX","N16cell.combined.Data","N16cell.mean.skew","N16cell.XvsAuto.skew")))

#run 'parseTbl.totalMatrix.sh rep_earlyBlast_R_output.txt' to generate the 3 input files.

All.Data <- read.table(file="processed/MC.earlyBlast.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="processed/MC.earlyBlast.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="processed/MC.earlyBlast.repColNames.txt", sep="\t")
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
#filter out repeats expressed in less than 5 samples in autosomes
All.Data <- All.Data[which(rowSums(!is.nan(All.Data[ ,c(1:95)])) >=5 | rowSums(!is.nan(All.Data[ ,c(96:100)])) >=1), ]
#Average the skewing for auto, chrX and chr1.
earlyBlast.mean.skew <- Chr_Average(All.Data, 5)
earlyBlast.skew <- data.frame(X.chr_Average(All.Data, 5), stage="eB")
earlyBlast.XvsAuto.skew <- data.frame(Auto.X_Average(All.Data, 5), stage="eB")




rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average",
                        "zygote.skew","zygote.chrX","zygote.combined.Data","zygote.mean.skew","zygote.XvsAuto.skew",
                        "early2cell.skew","early2cell.chrX","early2cell.combined.Data","early2cell.mean.skew","early2cell.XvsAuto.skew",
                        "late2cell.skew","late2cell.chrX","late2cell.combined.Data","late2cell.mean.skew","late2cell.XvsAuto.skew",
                        "N4cell.skew","N4cell.chrX","N4cell.combined.Data","N4cell.mean.skew","N4cell.XvsAuto.skew",
                        "N8cell.skew","N8cell.chrX","N8cell.combined.Data","N8cell.mean.skew","N8cell.XvsAuto.skew",
                        "N16cell.skew","N16cell.chrX","N16cell.combined.Data","N16cell.mean.skew","N16cell.XvsAuto.skew",
                        "earlyBlast.skew","earlyBlast.chrX","earlyBlast.combined.Data","earlyBlast.mean.skew","earlyBlast.XvsAuto.skew")))




####################### random XCI ##############################


#Day0
#clean up unused variables
rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average",
                        "zygote.skew","zygote.chrX","zygote.combined.Data","zygote.mean.skew","zygote.XvsAuto.skew",
                        "early2cell.skew","early2cell.chrX","early2cell.combined.Data","early2cell.mean.skew","early2cell.XvsAuto.skew",
                        "late2cell.skew","late2cell.chrX","late2cell.combined.Data","late2cell.mean.skew","late2cell.XvsAuto.skew",
                        "N4cell.skew","N4cell.chrX","N4cell.combined.Data","N4cell.mean.skew","N4cell.XvsAuto.skew",
                        "N8cell.skew","N8cell.chrX","N8cell.combined.Data","N8cell.mean.skew","N8cell.XvsAuto.skew",
                        "N16cell.skew","N16cell.chrX","N16cell.combined.Data","N16cell.mean.skew","N16cell.XvsAuto.skew",
                        "earlyBlast.skew","earlyBlast.chrX","earlyBlast.combined.Data","earlyBlast.mean.skew","earlyBlast.XvsAuto.skew")))

#run 'parseTbl.totalMatrix.sh rep_day0_R_output.txt' to generate the 3 input files.

All.Data <- read.table(file="processed/Day0.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="processed/Day0.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="processed/Day0.repColNames.txt", sep="\t")
rownames(All.Data) <- All.rowname[ ,1]
colnames(All.Data) <- All.colname[ ,1]
All.Data <- t(All.Data)
matched.line <- match(rownames(repeat.type.family), rownames(All.Data))
All.Data <- All.Data[matched.line[!is.na(matched.line)], ]

combined.Col <- seq(from=3, to=60, by=3)
Day0.combined.Data <- All.Data[ ,combined.Col]
All.Data <- All.Data[ ,setdiff(seq(1:ncol(All.Data)), combined.Col)]

#pull out repeat counts from chrX
Day0.chrX <- All.Data[ ,c(39:40)]
colnames(Day0.chrX) <- c("Day0_1", "Day0_2")
#filter out repeats expressed less than 2 times in autosomes or less than 1 in chrX
All.Data <- All.Data[which(rowSums(!is.nan(All.Data[ ,c(1:38)])) >=2 | rowSums(!is.nan(All.Data[ ,c(39:40)])) >=1), ]
#Average the skewing for auto, chrX and chr1.
Day0.mean.skew <- Chr_Average(All.Data, 2)
Day0.skew <- data.frame(X.chr_Average(All.Data, 2), stage="Day0")
Day0.XvsAuto.skew <- data.frame(Auto.X_Average(All.Data, 2), stage="Day0")


#Day1
#clean up unsed variables
rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average",
                        "zygote.skew","zygote.chrX","zygote.combined.Data","zygote.mean.skew","zygote.XvsAuto.skew",
                        "early2cell.skew","early2cell.chrX","early2cell.combined.Data","early2cell.mean.skew","early2cell.XvsAuto.skew",
                        "late2cell.skew","late2cell.chrX","late2cell.combined.Data","late2cell.mean.skew","late2cell.XvsAuto.skew",
                        "N4cell.skew","N4cell.chrX","N4cell.combined.Data","N4cell.mean.skew","N4cell.XvsAuto.skew",
                        "N8cell.skew","N8cell.chrX","N8cell.combined.Data","N8cell.mean.skew","N8cell.XvsAuto.skew",
                        "N16cell.skew","N16cell.chrX","N16cell.combined.Data","N16cell.mean.skew","N16cell.XvsAuto.skew",
                        "earlyBlast.skew","earlyBlast.chrX","earlyBlast.combined.Data","earlyBlast.mean.skew","earlyBlast.XvsAuto.skew",
                        "Day0.skew","Day0.chrX","Day0.combined.Data","Day0.mean.skew","Day0.XvsAuto.skew")))

#run 'parseTbl.totalMatrix.sh rep_day1_R_output.txt' to generate the 3 input files.

All.Data <- read.table(file="processed/Day1.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="processed/Day1.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="processed/Day1.repColNames.txt", sep="\t")
rownames(All.Data) <- All.rowname[ ,1]
colnames(All.Data) <- All.colname[ ,1]
All.Data <- t(All.Data)
matched.line <- match(rownames(repeat.type.family), rownames(All.Data))
All.Data <- All.Data[matched.line[!is.na(matched.line)], ]

combined.Col <- seq(from=3, to=60, by=3)
Day1.combined.Data <- All.Data[ ,combined.Col]
All.Data <- All.Data[ ,setdiff(seq(1:ncol(All.Data)), combined.Col)]

#pull out repeat counts from chrX
Day1.chrX <- All.Data[ ,c(39:40)]
colnames(Day1.chrX) <- c("Day1_1", "Day1_2")
#filter out repeats expressed less than 2 times in autosomes or less than 1 in chrX
All.Data <- All.Data[which(rowSums(!is.nan(All.Data[ ,c(1:38)])) >=2 | rowSums(!is.nan(All.Data[ ,c(39:40)])) >=1), ]
#Average the skewing for auto, chrX and chr1.
Day1.mean.skew <- Chr_Average(All.Data, 2)
Day1.skew <- data.frame(X.chr_Average(All.Data, 2), stage="Day1")
Day1.XvsAuto.skew <- data.frame(Auto.X_Average(All.Data, 2), stage="Day1")


#Day2
#clean up unsed variables
rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average",
                        "zygote.skew","zygote.chrX","zygote.combined.Data","zygote.mean.skew","zygote.XvsAuto.skew",
                        "early2cell.skew","early2cell.chrX","early2cell.combined.Data","early2cell.mean.skew","early2cell.XvsAuto.skew",
                        "late2cell.skew","late2cell.chrX","late2cell.combined.Data","late2cell.mean.skew","late2cell.XvsAuto.skew",
                        "N4cell.skew","N4cell.chrX","N4cell.combined.Data","N4cell.mean.skew","N4cell.XvsAuto.skew",
                        "N8cell.skew","N8cell.chrX","N8cell.combined.Data","N8cell.mean.skew","N8cell.XvsAuto.skew",
                        "N16cell.skew","N16cell.chrX","N16cell.combined.Data","N16cell.mean.skew","N16cell.XvsAuto.skew",
                        "earlyBlast.skew","earlyBlast.chrX","earlyBlast.combined.Data","earlyBlast.mean.skew","earlyBlast.XvsAuto.skew",
                        "Day0.skew","Day0.chrX","Day0.combined.Data","Day0.mean.skew","Day0.XvsAuto.skew",
                        "Day1.skew","Day1.chrX","Day1.combined.Data","Day1.mean.skew","Day1.XvsAuto.skew")))

#run 'parseTbl.totalMatrix.sh rep_day2_R_output.txt' to generate the 3 input files.

All.Data <- read.table(file="processed/Day2.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="processed/Day2.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="processed/Day2.repColNames.txt", sep="\t")
rownames(All.Data) <- All.rowname[ ,1]
colnames(All.Data) <- All.colname[ ,1]
All.Data <- t(All.Data)
matched.line <- match(rownames(repeat.type.family), rownames(All.Data))
All.Data <- All.Data[matched.line[!is.na(matched.line)], ]

combined.Col <- seq(from=3, to=60, by=3)
Day2.combined.Data <- All.Data[ ,combined.Col]
All.Data <- All.Data[ ,setdiff(seq(1:ncol(All.Data)), combined.Col)]

#pull out repeat counts from chrX
Day2.chrX <- All.Data[ ,c(39:40)]
colnames(Day2.chrX) <- c("Day2_1", "Day2_2")
#filter out repeats expressed less than 2 times in autosomes or less than 1 in chrX
All.Data <- All.Data[which(rowSums(!is.nan(All.Data[ ,c(1:38)])) >=2 | rowSums(!is.nan(All.Data[ ,c(39:40)])) >=1), ]
#Average the skewing for auto, chrX and chr1.
Day2.mean.skew <- Chr_Average(All.Data, 2)
Day2.skew <- data.frame(X.chr_Average(All.Data, 2), stage="Day2")
Day2.XvsAuto.skew <- data.frame(Auto.X_Average(All.Data, 2), stage="Day2")




#Day3
#clean up unsed variables
rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average",
                        "zygote.skew","zygote.chrX","zygote.combined.Data","zygote.mean.skew","zygote.XvsAuto.skew",
                        "early2cell.skew","early2cell.chrX","early2cell.combined.Data","early2cell.mean.skew","early2cell.XvsAuto.skew",
                        "late2cell.skew","late2cell.chrX","late2cell.combined.Data","late2cell.mean.skew","late2cell.XvsAuto.skew",
                        "N4cell.skew","N4cell.chrX","N4cell.combined.Data","N4cell.mean.skew","N4cell.XvsAuto.skew",
                        "N8cell.skew","N8cell.chrX","N8cell.combined.Data","N8cell.mean.skew","N8cell.XvsAuto.skew",
                        "N16cell.skew","N16cell.chrX","N16cell.combined.Data","N16cell.mean.skew","N16cell.XvsAuto.skew",
                        "earlyBlast.skew","earlyBlast.chrX","earlyBlast.combined.Data","earlyBlast.mean.skew","earlyBlast.XvsAuto.skew",
                        "Day0.skew","Day0.chrX","Day0.combined.Data","Day0.mean.skew","Day0.XvsAuto.skew",
                        "Day1.skew","Day1.chrX","Day1.combined.Data","Day1.mean.skew","Day1.XvsAuto.skew",
                        "Day2.skew","Day2.chrX","Day2.combined.Data","Day2.mean.skew","Day2.XvsAuto.skew")))

#run 'parseTbl.totalMatrix.sh rep_day3_R_output.txt' to generate the 3 input files.

All.Data <- read.table(file="processed/Day3.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="processed/Day3.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="processed/Day3.repColNames.txt", sep="\t")
rownames(All.Data) <- All.rowname[ ,1]
colnames(All.Data) <- All.colname[ ,1]
All.Data <- t(All.Data)
matched.line <- match(rownames(repeat.type.family), rownames(All.Data))
All.Data <- All.Data[matched.line[!is.na(matched.line)], ]

combined.Col <- seq(from=3, to=60, by=3)
Day3.combined.Data <- All.Data[ ,combined.Col]
All.Data <- All.Data[ ,setdiff(seq(1:ncol(All.Data)), combined.Col)]

#pull out repeat counts from chrX
Day3.chrX <- All.Data[ ,c(39:40)]
colnames(Day3.chrX) <- c("Day3_1", "Day3_2")
#filter out repeats expressed less than 2 times in autosomes or less than 1 in chrX
All.Data <- All.Data[which(rowSums(!is.nan(All.Data[ ,c(1:38)])) >=2 | rowSums(!is.nan(All.Data[ ,c(39:40)])) >=1), ]
#Average the skewing for auto, chrX and chr1.
Day3.mean.skew <- Chr_Average(All.Data, 2)
Day3.skew <- data.frame(X.chr_Average(All.Data, 2), stage="Day3")
Day3.XvsAuto.skew <- data.frame(Auto.X_Average(All.Data, 2), stage="Day3")




#Day4
#clean up unsed variables
rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average",
                        "zygote.skew","zygote.chrX","zygote.combined.Data","zygote.mean.skew","zygote.XvsAuto.skew",
                        "early2cell.skew","early2cell.chrX","early2cell.combined.Data","early2cell.mean.skew","early2cell.XvsAuto.skew",
                        "late2cell.skew","late2cell.chrX","late2cell.combined.Data","late2cell.mean.skew","late2cell.XvsAuto.skew",
                        "N4cell.skew","N4cell.chrX","N4cell.combined.Data","N4cell.mean.skew","N4cell.XvsAuto.skew",
                        "N8cell.skew","N8cell.chrX","N8cell.combined.Data","N8cell.mean.skew","N8cell.XvsAuto.skew",
                        "N16cell.skew","N16cell.chrX","N16cell.combined.Data","N16cell.mean.skew","N16cell.XvsAuto.skew",
                        "earlyBlast.skew","earlyBlast.chrX","earlyBlast.combined.Data","earlyBlast.mean.skew","earlyBlast.XvsAuto.skew",
                        "Day0.skew","Day0.chrX","Day0.combined.Data","Day0.mean.skew","Day0.XvsAuto.skew",
                        "Day1.skew","Day1.chrX","Day1.combined.Data","Day1.mean.skew","Day1.XvsAuto.skew",
                        "Day2.skew","Day2.chrX","Day2.combined.Data","Day2.mean.skew","Day2.XvsAuto.skew",
                        "Day3.skew","Day3.chrX","Day3.combined.Data","Day3.mean.skew","Day3.XvsAuto.skew")))

#run 'parseTbl.totalMatrix.sh rep_day4_R_output.txt' to generate the 3 input files.

All.Data <- read.table(file="processed/Day4.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="processed/Day4.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="processed/Day4.repColNames.txt", sep="\t")
rownames(All.Data) <- All.rowname[ ,1]
colnames(All.Data) <- All.colname[ ,1]
All.Data <- t(All.Data)
matched.line <- match(rownames(repeat.type.family), rownames(All.Data))
All.Data <- All.Data[matched.line[!is.na(matched.line)], ]

combined.Col <- seq(from=3, to=60, by=3)
Day4.combined.Data <- All.Data[ ,combined.Col]
All.Data <- All.Data[ ,setdiff(seq(1:ncol(All.Data)), combined.Col)]

#pull out repeat counts from chrX
Day4.chrX <- All.Data[ ,c(39:40)]
colnames(Day4.chrX) <- c("Day4_1", "Day4_2")
#filter out repeats expressed less than 2 times in autosomes or less than 1 in chrX
All.Data <- All.Data[which(rowSums(!is.nan(All.Data[ ,c(1:38)])) >=2 | rowSums(!is.nan(All.Data[ ,c(39:40)])) >=1), ]
#Average the skewing for auto, chrX and chr1.
Day4.mean.skew <- Chr_Average(All.Data, 2)
Day4.skew <- data.frame(X.chr_Average(All.Data, 2), stage="Day4")
Day4.XvsAuto.skew <- data.frame(Auto.X_Average(All.Data, 2), stage="Day4")




#Day6
#clean up unsed variables
rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average",
                        "zygote.skew","zygote.chrX","zygote.combined.Data","zygote.mean.skew","zygote.XvsAuto.skew",
                        "early2cell.skew","early2cell.chrX","early2cell.combined.Data","early2cell.mean.skew","early2cell.XvsAuto.skew",
                        "late2cell.skew","late2cell.chrX","late2cell.combined.Data","late2cell.mean.skew","late2cell.XvsAuto.skew",
                        "N4cell.skew","N4cell.chrX","N4cell.combined.Data","N4cell.mean.skew","N4cell.XvsAuto.skew",
                        "N8cell.skew","N8cell.chrX","N8cell.combined.Data","N8cell.mean.skew","N8cell.XvsAuto.skew",
                        "N16cell.skew","N16cell.chrX","N16cell.combined.Data","N16cell.mean.skew","N16cell.XvsAuto.skew",
                        "earlyBlast.skew","earlyBlast.chrX","earlyBlast.combined.Data","earlyBlast.mean.skew","earlyBlast.XvsAuto.skew",
                        "Day0.skew","Day0.chrX","Day0.combined.Data","Day0.mean.skew","Day0.XvsAuto.skew",
                        "Day1.skew","Day1.chrX","Day1.combined.Data","Day1.mean.skew","Day1.XvsAuto.skew",
                        "Day2.skew","Day2.chrX","Day2.combined.Data","Day2.mean.skew","Day2.XvsAuto.skew",
                        "Day3.skew","Day3.chrX","Day3.combined.Data","Day3.mean.skew","Day3.XvsAuto.skew",
                        "Day4.skew","Day4.chrX","Day4.combined.Data","Day4.mean.skew","Day4.XvsAuto.skew")))

#run 'parseTbl.totalMatrix.sh rep_day6_R_output.txt' to generate the 3 input files.

All.Data <- read.table(file="processed/Day6.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="processed/Day6.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="processed/Day6.repColNames.txt", sep="\t")
rownames(All.Data) <- All.rowname[ ,1]
colnames(All.Data) <- All.colname[ ,1]
All.Data <- t(All.Data)
matched.line <- match(rownames(repeat.type.family), rownames(All.Data))
All.Data <- All.Data[matched.line[!is.na(matched.line)], ]

combined.Col <- seq(from=3, to=60, by=3)
Day6.combined.Data <- All.Data[ ,combined.Col]
All.Data <- All.Data[ ,setdiff(seq(1:ncol(All.Data)), combined.Col)]

#pull out repeat counts from chrX
Day6.chrX <- All.Data[ ,c(39:40)]
colnames(Day6.chrX) <- c("Day6_1", "Day6_2")
#filter out repeats expressed less than 2 times in autosomes or less than 1 in chrX
All.Data <- All.Data[which(rowSums(!is.nan(All.Data[ ,c(1:38)])) >=2 | rowSums(!is.nan(All.Data[ ,c(39:40)])) >=1), ]
#Average the skewing for auto, chrX and chr1.
Day6.mean.skew <- Chr_Average(All.Data, 2)
Day6.skew <- data.frame(X.chr_Average(All.Data, 2), stage="Day6")
Day6.XvsAuto.skew <- data.frame(Auto.X_Average(All.Data, 2), stage="Day6")




#Day8
#clean up unsed variables
rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average",
                        "zygote.skew","zygote.chrX","zygote.combined.Data","zygote.mean.skew","zygote.XvsAuto.skew",
                        "early2cell.skew","early2cell.chrX","early2cell.combined.Data","early2cell.mean.skew","early2cell.XvsAuto.skew",
                        "late2cell.skew","late2cell.chrX","late2cell.combined.Data","late2cell.mean.skew","late2cell.XvsAuto.skew",
                        "N4cell.skew","N4cell.chrX","N4cell.combined.Data","N4cell.mean.skew","N4cell.XvsAuto.skew",
                        "N8cell.skew","N8cell.chrX","N8cell.combined.Data","N8cell.mean.skew","N8cell.XvsAuto.skew",
                        "N16cell.skew","N16cell.chrX","N16cell.combined.Data","N16cell.mean.skew","N16cell.XvsAuto.skew",
                        "earlyBlast.skew","earlyBlast.chrX","earlyBlast.combined.Data","earlyBlast.mean.skew","earlyBlast.XvsAuto.skew",
                        "Day0.skew","Day0.chrX","Day0.combined.Data","Day0.mean.skew","Day0.XvsAuto.skew",
                        "Day1.skew","Day1.chrX","Day1.combined.Data","Day1.mean.skew","Day1.XvsAuto.skew",
                        "Day2.skew","Day2.chrX","Day2.combined.Data","Day2.mean.skew","Day2.XvsAuto.skew",
                        "Day3.skew","Day3.chrX","Day3.combined.Data","Day3.mean.skew","Day3.XvsAuto.skew",
                        "Day4.skew","Day4.chrX","Day4.combined.Data","Day4.mean.skew","Day4.XvsAuto.skew",
                        "Day6.skew","Day6.chrX","Day6.combined.Data","Day6.mean.skew","Day6.XvsAuto.skew")))

#run 'parseTbl.totalMatrix.sh rep_day8_R_output.txt' to generate the 3 input files.

All.Data <- read.table(file="processed/Day8.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="processed/Day8.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="processed/Day8.repColNames.txt", sep="\t")
rownames(All.Data) <- All.rowname[ ,1]
colnames(All.Data) <- All.colname[ ,1]
All.Data <- t(All.Data)
matched.line <- match(rownames(repeat.type.family), rownames(All.Data))
All.Data <- All.Data[matched.line[!is.na(matched.line)], ]

combined.Col <- seq(from=3, to=60, by=3)
Day8.combined.Data <- All.Data[ ,combined.Col]
All.Data <- All.Data[ ,setdiff(seq(1:ncol(All.Data)), combined.Col)]

#pull out repeat counts from chrX
Day8.chrX <- All.Data[ ,c(39:40)]
colnames(Day8.chrX) <- c("Day8_1", "Day8_2")
#filter out repeats expressed less than 2 times in autosomes or less than 1 in chrX
All.Data <- All.Data[which(rowSums(!is.nan(All.Data[ ,c(1:38)])) >=2 | rowSums(!is.nan(All.Data[ ,c(39:40)])) >=1), ]
#Average the skewing for auto, chrX and chr1.
Day8.mean.skew <- Chr_Average(All.Data, 2)
Day8.skew <- data.frame(X.chr_Average(All.Data, 2), stage="Day8")
Day8.XvsAuto.skew <- data.frame(Auto.X_Average(All.Data, 2), stage="Day8")




#Day10
#clean up unsed variables
rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average",
                        "zygote.skew","zygote.chrX","zygote.combined.Data","zygote.mean.skew","zygote.XvsAuto.skew",
                        "early2cell.skew","early2cell.chrX","early2cell.combined.Data","early2cell.mean.skew","early2cell.XvsAuto.skew",
                        "late2cell.skew","late2cell.chrX","late2cell.combined.Data","late2cell.mean.skew","late2cell.XvsAuto.skew",
                        "N4cell.skew","N4cell.chrX","N4cell.combined.Data","N4cell.mean.skew","N4cell.XvsAuto.skew",
                        "N8cell.skew","N8cell.chrX","N8cell.combined.Data","N8cell.mean.skew","N8cell.XvsAuto.skew",
                        "N16cell.skew","N16cell.chrX","N16cell.combined.Data","N16cell.mean.skew","N16cell.XvsAuto.skew",
                        "earlyBlast.skew","earlyBlast.chrX","earlyBlast.combined.Data","earlyBlast.mean.skew","earlyBlast.XvsAuto.skew",
                        "Day0.skew","Day0.chrX","Day0.combined.Data","Day0.mean.skew","Day0.XvsAuto.skew",
                        "Day1.skew","Day1.chrX","Day1.combined.Data","Day1.mean.skew","Day1.XvsAuto.skew",
                        "Day2.skew","Day2.chrX","Day2.combined.Data","Day2.mean.skew","Day2.XvsAuto.skew",
                        "Day3.skew","Day3.chrX","Day3.combined.Data","Day3.mean.skew","Day3.XvsAuto.skew",
                        "Day4.skew","Day4.chrX","Day4.combined.Data","Day4.mean.skew","Day4.XvsAuto.skew",
                        "Day6.skew","Day6.chrX","Day6.combined.Data","Day6.mean.skew","Day6.XvsAuto.skew",
                        "Day8.skew","Day8.chrX","Day8.combined.Data","Day8.mean.skew","Day8.XvsAuto.skew")))

#run 'parseTbl.totalMatrix.sh rep_day10_R_output.txt' to generate the 3 input files.

All.Data <- read.table(file="processed/Day10.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="processed/Day10.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="processed/Day10.repColNames.txt", sep="\t")
rownames(All.Data) <- All.rowname[ ,1]
colnames(All.Data) <- All.colname[ ,1]
All.Data <- t(All.Data)
matched.line <- match(rownames(repeat.type.family), rownames(All.Data))
All.Data <- All.Data[matched.line[!is.na(matched.line)], ]

combined.Col <- seq(from=3, to=60, by=3)
Day10.combined.Data <- All.Data[ ,combined.Col]
All.Data <- All.Data[ ,setdiff(seq(1:ncol(All.Data)), combined.Col)]

#pull out repeat counts from chrX
Day10.chrX <- All.Data[ ,c(39:40)]
colnames(Day10.chrX) <- c("Day10_1", "Day10_2")
#filter out repeats expressed less than 2 times in autosomes or less than 1 in chrX
All.Data <- All.Data[which(rowSums(!is.nan(All.Data[ ,c(1:38)])) >=2 | rowSums(!is.nan(All.Data[ ,c(39:40)])) >=1), ]
#Average the skewing for auto, chrX and chr1.
Day10.mean.skew <- Chr_Average(All.Data, 2)
Day10.skew <- data.frame(X.chr_Average(All.Data, 2), stage="Day10")
Day10.XvsAuto.skew <- data.frame(Auto.X_Average(All.Data, 2), stage="Day10")


rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average",
                        "zygote.skew","zygote.chrX","zygote.combined.Data","zygote.mean.skew","zygote.XvsAuto.skew",
                        "early2cell.skew","early2cell.chrX","early2cell.combined.Data","early2cell.mean.skew","early2cell.XvsAuto.skew",
                        "late2cell.skew","late2cell.chrX","late2cell.combined.Data","late2cell.mean.skew","late2cell.XvsAuto.skew",
                        "N4cell.skew","N4cell.chrX","N4cell.combined.Data","N4cell.mean.skew","N4cell.XvsAuto.skew",
                        "N8cell.skew","N8cell.chrX","N8cell.combined.Data","N8cell.mean.skew","N8cell.XvsAuto.skew",
                        "N16cell.skew","N16cell.chrX","N16cell.combined.Data","N16cell.mean.skew","N16cell.XvsAuto.skew",
                        "earlyBlast.skew","earlyBlast.chrX","earlyBlast.combined.Data","earlyBlast.mean.skew","earlyBlast.XvsAuto.skew",
                        "Day0.skew","Day0.chrX","Day0.combined.Data","Day0.mean.skew","Day0.XvsAuto.skew",
                        "Day1.skew","Day1.chrX","Day1.combined.Data","Day1.mean.skew","Day1.XvsAuto.skew",
                        "Day2.skew","Day2.chrX","Day2.combined.Data","Day2.mean.skew","Day2.XvsAuto.skew",
                        "Day3.skew","Day3.chrX","Day3.combined.Data","Day3.mean.skew","Day3.XvsAuto.skew",
                        "Day4.skew","Day4.chrX","Day4.combined.Data","Day4.mean.skew","Day4.XvsAuto.skew",
                        "Day6.skew","Day6.chrX","Day6.combined.Data","Day6.mean.skew","Day6.XvsAuto.skew",
                        "Day8.skew","Day8.chrX","Day8.combined.Data","Day8.mean.skew","Day8.XvsAuto.skew",
                        "Day10.skew","Day10.chrX","Day10.combined.Data","Day10.mean.skew","Day10.XvsAuto.skew")))
