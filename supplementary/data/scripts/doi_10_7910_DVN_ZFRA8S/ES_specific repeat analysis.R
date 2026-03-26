#load package
#library(gplots)
library(plyr)
library(pheatmap)
library(RColorBrewer)
library(ggplot2)
rm(list=ls())
source("MC.overall repeat analysis.R")
rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average",
                        "Day0.skew","Day0.chrX","Day0.combined.Data","Day0.mean.skew","Day0.XvsAuto.skew",
                        "Day1.skew","Day1.chrX","Day1.combined.Data","Day1.mean.skew","Day1.XvsAuto.skew",
                        "Day2.skew","Day2.chrX","Day2.combined.Data","Day2.mean.skew","Day2.XvsAuto.skew",
                        "Day3.skew","Day3.chrX","Day3.combined.Data","Day3.mean.skew","Day3.XvsAuto.skew",
                        "Day4.skew","Day4.chrX","Day4.combined.Data","Day4.mean.skew","Day4.XvsAuto.skew",
                        "Day6.skew","Day6.chrX","Day6.combined.Data","Day6.mean.skew","Day6.XvsAuto.skew",
                        "Day8.skew","Day8.chrX","Day8.combined.Data","Day8.mean.skew","Day8.XvsAuto.skew",
                        "Day10.skew","Day10.chrX","Day10.combined.Data","Day10.mean.skew","Day10.XvsAuto.skew")))

LINE.rep <- repeat.type.family[which(repeat.type.family[ ,2] == "LINE"), ]
SINE.rep <- repeat.type.family[which(repeat.type.family[ ,2] == "SINE"), ]
LTR.rep <- repeat.type.family[which(repeat.type.family[ ,2] == "LTR"), ]
DNA.rep <- repeat.type.family[which(repeat.type.family[ ,2] == "DNA"), ]
#LINE-1 age
#LINE1.age <- read.table(file="processed/LINE-1_age.txt",sep="\t")


#Day0
#LINEs
LINE.matched.line <- match(rownames(LINE.rep), rownames(Day0.mean.skew))
Day0.LINE.Data <- Day0.mean.skew[LINE.matched.line[!is.na(LINE.matched.line)], ]
#cLINEs (by combing all replicates)
cLINE.matched.line <- match(rownames(LINE.rep), rownames(Day0.combined.Data))
Day0.cLINE.Data <- Day0.combined.Data[cLINE.matched.line[!is.na(cLINE.matched.line)], ]

#SINEs
SINE.matched.line <- match(rownames(SINE.rep), rownames(Day0.mean.skew))
Day0.SINE.Data <- Day0.mean.skew[SINE.matched.line[!is.na(SINE.matched.line)], ]
#LTRs
LTR.matched.line <- match(rownames(LTR.rep), rownames(Day0.mean.skew))
Day0.LTR.Data <- Day0.mean.skew[LTR.matched.line[!is.na(LTR.matched.line)], ]
#DNAs
DNA.matched.line <- match(rownames(DNA.rep), rownames(Day0.mean.skew))
Day0.DNA.Data <- Day0.mean.skew[DNA.matched.line[!is.na(DNA.matched.line)], ]

#Day1
#LINEs
LINE.matched.line <- match(rownames(LINE.rep), rownames(Day1.mean.skew))
Day1.LINE.Data <- Day1.mean.skew[LINE.matched.line[!is.na(LINE.matched.line)], ]
#cLINEs (by combing all replicates)
cLINE.matched.line <- match(rownames(LINE.rep), rownames(Day1.combined.Data))
Day1.cLINE.Data <- Day1.combined.Data[cLINE.matched.line[!is.na(cLINE.matched.line)], ]

#SINEs
SINE.matched.line <- match(rownames(SINE.rep), rownames(Day1.mean.skew))
Day1.SINE.Data <- Day1.mean.skew[SINE.matched.line[!is.na(SINE.matched.line)], ]
#LTRs
LTR.matched.line <- match(rownames(LTR.rep), rownames(Day1.mean.skew))
Day1.LTR.Data <- Day1.mean.skew[LTR.matched.line[!is.na(LTR.matched.line)], ]
#DNAs
DNA.matched.line <- match(rownames(DNA.rep), rownames(Day1.mean.skew))
Day1.DNA.Data <- Day1.mean.skew[DNA.matched.line[!is.na(DNA.matched.line)], ]

#Day2
#LINEs
LINE.matched.line <- match(rownames(LINE.rep), rownames(Day2.mean.skew))
Day2.LINE.Data <- Day2.mean.skew[LINE.matched.line[!is.na(LINE.matched.line)], ]
#cLINEs (by combing all replicates)
cLINE.matched.line <- match(rownames(LINE.rep), rownames(Day2.combined.Data))
Day2.cLINE.Data <- Day2.combined.Data[cLINE.matched.line[!is.na(cLINE.matched.line)], ]

#SINEs
SINE.matched.line <- match(rownames(SINE.rep), rownames(Day2.mean.skew))
Day2.SINE.Data <- Day2.mean.skew[SINE.matched.line[!is.na(SINE.matched.line)], ]
#LTRs
LTR.matched.line <- match(rownames(LTR.rep), rownames(Day2.mean.skew))
Day2.LTR.Data <- Day2.mean.skew[LTR.matched.line[!is.na(LTR.matched.line)], ]
#DNAs
DNA.matched.line <- match(rownames(DNA.rep), rownames(Day2.mean.skew))
Day2.DNA.Data <- Day2.mean.skew[DNA.matched.line[!is.na(DNA.matched.line)], ]

#Day3
#LINEs
LINE.matched.line <- match(rownames(LINE.rep), rownames(Day3.mean.skew))
Day3.LINE.Data <- Day3.mean.skew[LINE.matched.line[!is.na(LINE.matched.line)], ]
#cLINEs (by combing all replicates)
cLINE.matched.line <- match(rownames(LINE.rep), rownames(Day3.combined.Data))
Day3.cLINE.Data <- Day3.combined.Data[cLINE.matched.line[!is.na(cLINE.matched.line)], ]

#SINEs
SINE.matched.line <- match(rownames(SINE.rep), rownames(Day3.mean.skew))
Day3.SINE.Data <- Day3.mean.skew[SINE.matched.line[!is.na(SINE.matched.line)], ]
#LTRs
LTR.matched.line <- match(rownames(LTR.rep), rownames(Day3.mean.skew))
Day3.LTR.Data <- Day3.mean.skew[LTR.matched.line[!is.na(LTR.matched.line)], ]
#DNAs
DNA.matched.line <- match(rownames(DNA.rep), rownames(Day3.mean.skew))
Day3.DNA.Data <- Day3.mean.skew[DNA.matched.line[!is.na(DNA.matched.line)], ]

#Day4
#LINEs
LINE.matched.line <- match(rownames(LINE.rep), rownames(Day4.mean.skew))
Day4.LINE.Data <- Day4.mean.skew[LINE.matched.line[!is.na(LINE.matched.line)], ]
#cLINEs (by combing all replicates)
cLINE.matched.line <- match(rownames(LINE.rep), rownames(Day4.combined.Data))
Day4.cLINE.Data <- Day4.combined.Data[cLINE.matched.line[!is.na(cLINE.matched.line)], ]

#SINEs
SINE.matched.line <- match(rownames(SINE.rep), rownames(Day4.mean.skew))
Day4.SINE.Data <- Day4.mean.skew[SINE.matched.line[!is.na(SINE.matched.line)], ]
#LTRs
LTR.matched.line <- match(rownames(LTR.rep), rownames(Day4.mean.skew))
Day4.LTR.Data <- Day4.mean.skew[LTR.matched.line[!is.na(LTR.matched.line)], ]
#DNAs
DNA.matched.line <- match(rownames(DNA.rep), rownames(Day4.mean.skew))
Day4.DNA.Data <- Day4.mean.skew[DNA.matched.line[!is.na(DNA.matched.line)], ]

#Day6
#LINEs
LINE.matched.line <- match(rownames(LINE.rep), rownames(Day6.mean.skew))
Day6.LINE.Data <- Day6.mean.skew[LINE.matched.line[!is.na(LINE.matched.line)], ]
#cLINEs (by combing all replicates)
cLINE.matched.line <- match(rownames(LINE.rep), rownames(Day6.combined.Data))
Day6.cLINE.Data <- Day6.combined.Data[cLINE.matched.line[!is.na(cLINE.matched.line)], ]

#SINEs
SINE.matched.line <- match(rownames(SINE.rep), rownames(Day6.mean.skew))
Day6.SINE.Data <- Day6.mean.skew[SINE.matched.line[!is.na(SINE.matched.line)], ]
#LTRs
LTR.matched.line <- match(rownames(LTR.rep), rownames(Day6.mean.skew))
Day6.LTR.Data <- Day6.mean.skew[LTR.matched.line[!is.na(LTR.matched.line)], ]
#DNAs
DNA.matched.line <- match(rownames(DNA.rep), rownames(Day6.mean.skew))
Day6.DNA.Data <- Day6.mean.skew[DNA.matched.line[!is.na(DNA.matched.line)], ]


#Day8
#LINEs
LINE.matched.line <- match(rownames(LINE.rep), rownames(Day8.mean.skew))
Day8.LINE.Data <- Day8.mean.skew[LINE.matched.line[!is.na(LINE.matched.line)], ]
#cLINEs (by combing all replicates)
cLINE.matched.line <- match(rownames(LINE.rep), rownames(Day8.combined.Data))
Day8.cLINE.Data <- Day8.combined.Data[cLINE.matched.line[!is.na(cLINE.matched.line)], ]

#SINEs
SINE.matched.line <- match(rownames(SINE.rep), rownames(Day8.mean.skew))
Day8.SINE.Data <- Day8.mean.skew[SINE.matched.line[!is.na(SINE.matched.line)], ]
#LTRs
LTR.matched.line <- match(rownames(LTR.rep), rownames(Day8.mean.skew))
Day8.LTR.Data <- Day8.mean.skew[LTR.matched.line[!is.na(LTR.matched.line)], ]
#DNAs
DNA.matched.line <- match(rownames(DNA.rep), rownames(Day8.mean.skew))
Day8.DNA.Data <- Day8.mean.skew[DNA.matched.line[!is.na(DNA.matched.line)], ]

#Day10
#LINEs
LINE.matched.line <- match(rownames(LINE.rep), rownames(Day10.mean.skew))
Day10.LINE.Data <- Day10.mean.skew[LINE.matched.line[!is.na(LINE.matched.line)], ]
#cLINEs (by combing all replicates)
cLINE.matched.line <- match(rownames(LINE.rep), rownames(Day10.combined.Data))
Day10.cLINE.Data <- Day10.combined.Data[cLINE.matched.line[!is.na(cLINE.matched.line)], ]

#SINEs
SINE.matched.line <- match(rownames(SINE.rep), rownames(Day10.mean.skew))
Day10.SINE.Data <- Day10.mean.skew[SINE.matched.line[!is.na(SINE.matched.line)], ]
#LTRs
LTR.matched.line <- match(rownames(LTR.rep), rownames(Day10.mean.skew))
Day10.LTR.Data <- Day10.mean.skew[LTR.matched.line[!is.na(LTR.matched.line)], ]
#DNAs
DNA.matched.line <- match(rownames(DNA.rep), rownames(Day10.mean.skew))
Day10.DNA.Data <- Day10.mean.skew[DNA.matched.line[!is.na(DNA.matched.line)], ]






#1.LINEs

#Day0
chrX.Day0.LINE <- Day0.LINE.Data[ ,20]
#LINE.Day0 <- data.frame(skew=chrX.Day0.LINE, class="LINE", stage="Day0", age=LINE1.age$Age[match(rownames(Day0.LINE.Data), rownames(LINE1.age))], subfamily=rownames(Day0.LINE.Data), row.names=rownames(Day0.LINE.Data))
LINE.Day0 <- data.frame(skew=chrX.Day0.LINE, class="LINE", stage="Day0",row.names=rownames(Day0.LINE.Data))
LINE.Day0 <- LINE.Day0[complete.cases(LINE.Day0), ]
#Day1
chrX.Day1.LINE <- Day1.LINE.Data[ ,20]
#LINE.Day1 <- data.frame(skew=chrX.Day1.LINE, class="LINE",stage="Day1", age=LINE1.age$Age[match(rownames(Day1.LINE.Data), rownames(LINE1.age))], subfamily=rownames(Day1.LINE.Data), row.names=rownames(Day1.LINE.Data))
LINE.Day1 <- data.frame(skew=chrX.Day1.LINE, class="LINE",stage="Day1", row.names=rownames(Day1.LINE.Data))
LINE.Day1 <- LINE.Day1[complete.cases(LINE.Day1), ]
#Day2
chrX.Day2.LINE <- Day2.LINE.Data[ ,20]
#LINE.Day2 <- data.frame(skew=chrX.Day2.LINE, class="LINE",stage="Day2", age=LINE1.age$Age[match(rownames(Day2.LINE.Data), rownames(LINE1.age))], subfamily=rownames(Day2.LINE.Data), row.names=rownames(Day2.LINE.Data))
LINE.Day2 <- data.frame(skew=chrX.Day2.LINE, class="LINE",stage="Day2", row.names=rownames(Day2.LINE.Data))
LINE.Day2 <- LINE.Day2[complete.cases(LINE.Day2), ]
#Day3
chrX.Day3.LINE <- Day3.LINE.Data[ ,20]
#LINE.Day3 <- data.frame(skew=chrX.Day3.LINE, class="LINE",stage="Day3", age=LINE1.age$Age[match(rownames(Day3.LINE.Data), rownames(LINE1.age))], subfamily=rownames(Day3.LINE.Data), row.names=rownames(Day3.LINE.Data))
LINE.Day3 <- data.frame(skew=chrX.Day3.LINE, class="LINE",stage="Day3", row.names=rownames(Day3.LINE.Data))
LINE.Day3 <- LINE.Day3[complete.cases(LINE.Day3), ]
#Day4
chrX.Day4.LINE <- Day4.LINE.Data[ ,20]
#LINE.Day4 <- data.frame(skew=chrX.Day4.LINE, class="LINE",stage="Day4", age=LINE1.age$Age[match(rownames(Day4.LINE.Data), rownames(LINE1.age))], subfamily=rownames(Day4.LINE.Data), row.names=rownames(Day4.LINE.Data))
LINE.Day4 <- data.frame(skew=chrX.Day4.LINE, class="LINE",stage="Day4", row.names=rownames(Day4.LINE.Data))
LINE.Day4 <- LINE.Day4[complete.cases(LINE.Day4), ]
#Day6
chrX.Day6.LINE <- Day6.LINE.Data[ ,20]
#LINE.Day6 <- data.frame(skew=chrX.Day6.LINE, class="LINE",stage="Day6", age=LINE1.age$Age[match(rownames(Day6.LINE.Data), rownames(LINE1.age))], subfamily=rownames(Day6.LINE.Data), row.names=rownames(Day6.LINE.Data))
LINE.Day6 <- data.frame(skew=chrX.Day6.LINE, class="LINE",stage="Day6", row.names=rownames(Day6.LINE.Data))
LINE.Day6 <- LINE.Day6[complete.cases(LINE.Day6), ]
#Day8
chrX.Day8.LINE <- Day8.LINE.Data[ ,20]
#LINE.Day8 <- data.frame(skew=chrX.Day8.LINE, class="LINE",stage="Day8", age=LINE1.age$Age[match(rownames(Day8.LINE.Data), rownames(LINE1.age))], subfamily=rownames(Day8.LINE.Data), row.names=rownames(Day8.LINE.Data))
LINE.Day8 <- data.frame(skew=chrX.Day8.LINE, class="LINE",stage="Day8", row.names=rownames(Day8.LINE.Data))
LINE.Day8 <- LINE.Day8[complete.cases(LINE.Day8), ]
#Day10
chrX.Day10.LINE <- Day10.LINE.Data[ ,20]
#LINE.Day10 <- data.frame(skew=chrX.Day10.LINE, class="LINE",stage="Day10", age=LINE1.age$Age[match(rownames(Day10.LINE.Data), rownames(LINE1.age))], subfamily=rownames(Day10.LINE.Data), row.names=rownames(Day10.LINE.Data))
LINE.Day10 <- data.frame(skew=chrX.Day10.LINE, class="LINE",stage="Day10", row.names=rownames(Day10.LINE.Data))
LINE.Day10 <- LINE.Day10[complete.cases(LINE.Day10), ]



#2. SINEs

#Day0
chrX.Day0.SINE <- Day0.SINE.Data[ ,20]
SINE.Day0 <- data.frame(skew=chrX.Day0.SINE, class="SINE", stage="Day0", row.names=rownames(Day0.SINE.Data))
#SINE.Day0 <- SINE.Day0[complete.cases(SINE.Day0), ]
#Day1
chrX.Day1.SINE <- Day1.SINE.Data[ ,20]
SINE.Day1 <- data.frame(skew=chrX.Day1.SINE, class="SINE", stage="Day1", row.names=rownames(Day1.SINE.Data))
#Day2
chrX.Day2.SINE <- Day2.SINE.Data[ ,20]
SINE.Day2 <- data.frame(skew=chrX.Day2.SINE, class="SINE", stage="Day2", row.names=rownames(Day2.SINE.Data))
#Day3
chrX.Day3.SINE <- Day3.SINE.Data[ ,20]
SINE.Day3 <- data.frame(skew=chrX.Day3.SINE, class="SINE", stage="Day3", row.names=rownames(Day3.SINE.Data))
#Day4
chrX.Day4.SINE <- Day4.SINE.Data[ ,20]
SINE.Day4 <- data.frame(skew=chrX.Day4.SINE, class="SINE", stage="Day4", row.names=rownames(Day4.SINE.Data))
#Day6
chrX.Day6.SINE <- Day6.SINE.Data[ ,20]
SINE.Day6 <- data.frame(skew=chrX.Day6.SINE, class="SINE", stage="Day6", row.names=rownames(Day6.SINE.Data))
#Day8
chrX.Day8.SINE <- Day8.SINE.Data[ ,20]
SINE.Day8 <- data.frame(skew=chrX.Day8.SINE, class="SINE", stage="Day8", row.names=rownames(Day8.SINE.Data))
#Day10
chrX.Day10.SINE <- Day10.SINE.Data[ ,20]
SINE.Day10 <- data.frame(skew=chrX.Day10.SINE, class="SINE", stage="Day10", row.names=rownames(Day10.SINE.Data))


#3. LTR

#Day0
chrX.Day0.LTR <- Day0.LTR.Data[ ,20]
LTR.Day0 <- data.frame(skew=chrX.Day0.LTR, class="LTR", stage="Day0", row.names=rownames(Day0.LTR.Data))
#LTR.Day0 <- LTR.Day0[complete.cases(LTR.Day0), ]
#Day1
chrX.Day1.LTR <- Day1.LTR.Data[ ,20]
LTR.Day1 <- data.frame(skew=chrX.Day1.LTR, class="LTR", stage="Day1", row.names=rownames(Day1.LTR.Data))
#Day2
chrX.Day2.LTR <- Day2.LTR.Data[ ,20]
LTR.Day2 <- data.frame(skew=chrX.Day2.LTR, class="LTR", stage="Day2", row.names=rownames(Day2.LTR.Data))
#Day3
chrX.Day3.LTR <- Day3.LTR.Data[ ,20]
LTR.Day3 <- data.frame(skew=chrX.Day3.LTR, class="LTR", stage="Day3", row.names=rownames(Day3.LTR.Data))
#Day4
chrX.Day4.LTR <- Day4.LTR.Data[ ,20]
LTR.Day4 <- data.frame(skew=chrX.Day4.LTR, class="LTR", stage="Day4", row.names=rownames(Day4.LTR.Data))
#Day6
chrX.Day6.LTR <- Day6.LTR.Data[ ,20]
LTR.Day6 <- data.frame(skew=chrX.Day6.LTR, class="LTR", stage="Day6", row.names=rownames(Day6.LTR.Data))
#Day8
chrX.Day8.LTR <- Day8.LTR.Data[ ,20]
LTR.Day8 <- data.frame(skew=chrX.Day8.LTR, class="LTR", stage="Day8", row.names=rownames(Day8.LTR.Data))
#Day10
chrX.Day10.LTR <- Day10.LTR.Data[ ,20]
LTR.Day10 <- data.frame(skew=chrX.Day10.LTR, class="LTR", stage="Day10", row.names=rownames(Day10.LTR.Data))


#4.cLINEs (LINEs from all combined replicates)

#Day0
chrX.Day0.cLINE <- Day0.cLINE.Data[ ,20]
#cLINE.Day0 <- data.frame(skew=chrX.Day0.cLINE, class="cLINE", stage="Day0", age=LINE1.age$Age[match(rownames(Day0.cLINE.Data), rownames(LINE1.age))], subfamily=rownames(Day0.cLINE.Data), row.names=rownames(Day0.cLINE.Data))
cLINE.Day0 <- data.frame(skew=chrX.Day0.cLINE, class="cLINE", stage="Day0", row.names=rownames(Day0.cLINE.Data))
cLINE.Day0 <- cLINE.Day0[complete.cases(cLINE.Day0), ]
#Day1
chrX.Day1.cLINE <- Day1.cLINE.Data[ ,20]
#cLINE.Day1 <- data.frame(skew=chrX.Day1.cLINE, class="cLINE", stage="Day1", age=LINE1.age$Age[match(rownames(Day1.cLINE.Data), rownames(LINE1.age))], subfamily=rownames(Day1.cLINE.Data), row.names=rownames(Day1.cLINE.Data))
cLINE.Day1 <- data.frame(skew=chrX.Day1.cLINE, class="cLINE", stage="Day1", row.names=rownames(Day1.cLINE.Data))
cLINE.Day1 <- cLINE.Day1[complete.cases(cLINE.Day1), ]
#Day2
chrX.Day2.cLINE <- Day2.cLINE.Data[ ,20]
#cLINE.Day2 <- data.frame(skew=chrX.Day2.cLINE, class="cLINE", stage="Day2", age=LINE1.age$Age[match(rownames(Day2.cLINE.Data), rownames(LINE1.age))], subfamily=rownames(Day2.cLINE.Data), row.names=rownames(Day2.cLINE.Data))
cLINE.Day2 <- data.frame(skew=chrX.Day2.cLINE, class="cLINE", stage="Day2", row.names=rownames(Day2.cLINE.Data))
cLINE.Day2 <- cLINE.Day2[complete.cases(cLINE.Day2), ]
#Day3
chrX.Day3.cLINE <- Day3.cLINE.Data[ ,20]
#cLINE.Day3 <- data.frame(skew=chrX.Day3.cLINE, class="cLINE", stage="Day3", age=LINE1.age$Age[match(rownames(Day3.cLINE.Data), rownames(LINE1.age))], subfamily=rownames(Day3.cLINE.Data), row.names=rownames(Day3.cLINE.Data))
cLINE.Day3 <- data.frame(skew=chrX.Day3.cLINE, class="cLINE", stage="Day3", row.names=rownames(Day3.cLINE.Data))
cLINE.Day3 <- cLINE.Day3[complete.cases(cLINE.Day3), ]
#Day4
chrX.Day4.cLINE <- Day4.cLINE.Data[ ,20]
#cLINE.Day4 <- data.frame(skew=chrX.Day4.cLINE, class="cLINE", stage="Day4", age=LINE1.age$Age[match(rownames(Day4.cLINE.Data), rownames(LINE1.age))], subfamily=rownames(Day4.cLINE.Data), row.names=rownames(Day4.cLINE.Data))
cLINE.Day4 <- data.frame(skew=chrX.Day4.cLINE, class="cLINE", stage="Day4", row.names=rownames(Day4.cLINE.Data))
cLINE.Day4 <- cLINE.Day4[complete.cases(cLINE.Day4), ]
#Day6
chrX.Day6.cLINE <- Day6.cLINE.Data[ ,20]
#cLINE.Day6 <- data.frame(skew=chrX.Day6.cLINE, class="cLINE", stage="Day6", age=LINE1.age$Age[match(rownames(Day6.cLINE.Data), rownames(LINE1.age))], subfamily=rownames(Day6.cLINE.Data), row.names=rownames(Day6.cLINE.Data))
cLINE.Day6 <- data.frame(skew=chrX.Day6.cLINE, class="cLINE", stage="Day6", row.names=rownames(Day6.cLINE.Data))
cLINE.Day6 <- cLINE.Day6[complete.cases(cLINE.Day6), ]
#Day8
chrX.Day8.cLINE <- Day8.cLINE.Data[ ,20]
#cLINE.Day8 <- data.frame(skew=chrX.Day8.cLINE, class="cLINE", stage="Day8", age=LINE1.age$Age[match(rownames(Day8.cLINE.Data), rownames(LINE1.age))], subfamily=rownames(Day8.cLINE.Data), row.names=rownames(Day8.cLINE.Data))
cLINE.Day8 <- data.frame(skew=chrX.Day8.cLINE, class="cLINE", stage="Day8", row.names=rownames(Day8.cLINE.Data))
cLINE.Day8 <- cLINE.Day8[complete.cases(cLINE.Day8), ]
#Day10
chrX.Day10.cLINE <- Day10.cLINE.Data[ ,20]
#cLINE.Day10 <- data.frame(skew=chrX.Day10.cLINE, class="cLINE", stage="Day10", age=LINE1.age$Age[match(rownames(Day10.cLINE.Data), rownames(LINE1.age))], subfamily=rownames(Day10.cLINE.Data), row.names=rownames(Day10.cLINE.Data))
cLINE.Day10 <- data.frame(skew=chrX.Day10.cLINE, class="cLINE", stage="Day10", row.names=rownames(Day10.cLINE.Data))
cLINE.Day10 <- cLINE.Day10[complete.cases(cLINE.Day10), ]




