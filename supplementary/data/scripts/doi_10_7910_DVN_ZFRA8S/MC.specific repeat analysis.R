#load package

library(plyr)
library(pheatmap)
library(RColorBrewer)
library(ggplot2)
rm(list=ls())
source("MC.overall repeat analysis.R")
LINE.rep <- repeat.type.family[which(repeat.type.family[ ,2] == "LINE"), ]
SINE.rep <- repeat.type.family[which(repeat.type.family[ ,2] == "SINE"), ]
LTR.rep <- repeat.type.family[which(repeat.type.family[ ,2] == "LTR"), ]
DNA.rep <- repeat.type.family[which(repeat.type.family[ ,2] == "DNA"), ]
#LINE-1 age
#LINE1.age <- read.table(file="processed/LINE-1_age.txt",sep="\t")


#For zygote stage
#LINEs (mean value)
LINE.matched.line <- match(rownames(LINE.rep), rownames(zygote.mean.skew))
zygote.LINE.Data <- zygote.mean.skew[LINE.matched.line[!is.na(LINE.matched.line)], ]
#cLINEs (by combing all replicates, 99)
cLINE.matched.line <- match(rownames(LINE.rep), rownames(zygote.combined.Data))
zygote.cLINE.Data <- zygote.combined.Data[cLINE.matched.line[!is.na(cLINE.matched.line)], ]

#SINEs
SINE.matched.line <- match(rownames(SINE.rep), rownames(zygote.mean.skew))
zygote.SINE.Data <- zygote.mean.skew[SINE.matched.line[!is.na(SINE.matched.line)], ]
#LTRs
LTR.matched.line <- match(rownames(LTR.rep), rownames(zygote.mean.skew))
zygote.LTR.Data <- zygote.mean.skew[LTR.matched.line[!is.na(LTR.matched.line)], ]
#DNAs
DNA.matched.line <- match(rownames(DNA.rep), rownames(zygote.mean.skew))
zygote.DNA.Data <- zygote.mean.skew[DNA.matched.line[!is.na(DNA.matched.line)], ]

#For early2cell stage
#LINEs
LINE.matched.line <- match(rownames(LINE.rep), rownames(early2cell.mean.skew))
early2cell.LINE.Data <- early2cell.mean.skew[LINE.matched.line[!is.na(LINE.matched.line)], ]
#cLINEs (by combing all replicates)
cLINE.matched.line <- match(rownames(LINE.rep), rownames(early2cell.combined.Data))
early2cell.cLINE.Data <- early2cell.combined.Data[cLINE.matched.line[!is.na(cLINE.matched.line)], ]

#SINEs
SINE.matched.line <- match(rownames(SINE.rep), rownames(early2cell.mean.skew))
early2cell.SINE.Data <- early2cell.mean.skew[SINE.matched.line[!is.na(SINE.matched.line)], ]
#LTRs
LTR.matched.line <- match(rownames(LTR.rep), rownames(early2cell.mean.skew))
early2cell.LTR.Data <- early2cell.mean.skew[LTR.matched.line[!is.na(LTR.matched.line)], ]
#DNAs
DNA.matched.line <- match(rownames(DNA.rep), rownames(early2cell.mean.skew))
early2cell.DNA.Data <- early2cell.mean.skew[DNA.matched.line[!is.na(DNA.matched.line)], ]

#For late2cell stage
#LINEs
LINE.matched.line <- match(rownames(LINE.rep), rownames(late2cell.mean.skew))
late2cell.LINE.Data <- late2cell.mean.skew[LINE.matched.line[!is.na(LINE.matched.line)], ]
#cLINEs (by combing all replicates)
cLINE.matched.line <- match(rownames(LINE.rep), rownames(late2cell.combined.Data))
late2cell.cLINE.Data <- late2cell.combined.Data[cLINE.matched.line[!is.na(cLINE.matched.line)], ]

#SINEs
SINE.matched.line <- match(rownames(SINE.rep), rownames(late2cell.mean.skew))
late2cell.SINE.Data <- late2cell.mean.skew[SINE.matched.line[!is.na(SINE.matched.line)], ]
#LTRs
LTR.matched.line <- match(rownames(LTR.rep), rownames(late2cell.mean.skew))
late2cell.LTR.Data <- late2cell.mean.skew[LTR.matched.line[!is.na(LTR.matched.line)], ]
#DNAs
DNA.matched.line <- match(rownames(DNA.rep), rownames(late2cell.mean.skew))
late2cell.DNA.Data <- late2cell.mean.skew[DNA.matched.line[!is.na(DNA.matched.line)], ]

#For 4cell stage
#LINEs
LINE.matched.line <- match(rownames(LINE.rep), rownames(N4cell.mean.skew))
N4cell.LINE.Data <- N4cell.mean.skew[LINE.matched.line[!is.na(LINE.matched.line)], ]
#cLINEs (by combing all replicates)
cLINE.matched.line <- match(rownames(LINE.rep), rownames(N4cell.combined.Data))
N4cell.cLINE.Data <- N4cell.combined.Data[cLINE.matched.line[!is.na(cLINE.matched.line)], ]

#SINEs
SINE.matched.line <- match(rownames(SINE.rep), rownames(N4cell.mean.skew))
N4cell.SINE.Data <- N4cell.mean.skew[SINE.matched.line[!is.na(SINE.matched.line)], ]
#LTRs
LTR.matched.line <- match(rownames(LTR.rep), rownames(N4cell.mean.skew))
N4cell.LTR.Data <- N4cell.mean.skew[LTR.matched.line[!is.na(LTR.matched.line)], ]
#DNAs
DNA.matched.line <- match(rownames(DNA.rep), rownames(N4cell.mean.skew))
N4cell.DNA.Data <- N4cell.mean.skew[DNA.matched.line[!is.na(DNA.matched.line)], ]

#For 8cell stage
#LINEs
LINE.matched.line <- match(rownames(LINE.rep), rownames(N8cell.mean.skew))
N8cell.LINE.Data <- N8cell.mean.skew[LINE.matched.line[!is.na(LINE.matched.line)], ]
#cLINEs (by combing all replicates)
cLINE.matched.line <- match(rownames(LINE.rep), rownames(N8cell.combined.Data))
N8cell.cLINE.Data <- N8cell.combined.Data[cLINE.matched.line[!is.na(cLINE.matched.line)], ]

#SINEs
SINE.matched.line <- match(rownames(SINE.rep), rownames(N8cell.mean.skew))
N8cell.SINE.Data <- N8cell.mean.skew[SINE.matched.line[!is.na(SINE.matched.line)], ]
#LTRs
LTR.matched.line <- match(rownames(LTR.rep), rownames(N8cell.mean.skew))
N8cell.LTR.Data <- N8cell.mean.skew[LTR.matched.line[!is.na(LTR.matched.line)], ]
#DNAs
DNA.matched.line <- match(rownames(DNA.rep), rownames(N8cell.mean.skew))
N8cell.DNA.Data <- N8cell.mean.skew[DNA.matched.line[!is.na(DNA.matched.line)], ]

#For 16cell stage
#LINEs
LINE.matched.line <- match(rownames(LINE.rep), rownames(N16cell.mean.skew))
N16cell.LINE.Data <- N16cell.mean.skew[LINE.matched.line[!is.na(LINE.matched.line)], ]
#cLINEs (by combing all replicates)
cLINE.matched.line <- match(rownames(LINE.rep), rownames(N16cell.combined.Data))
N16cell.cLINE.Data <- N16cell.combined.Data[cLINE.matched.line[!is.na(cLINE.matched.line)], ]

#SINEs
SINE.matched.line <- match(rownames(SINE.rep), rownames(N16cell.mean.skew))
N16cell.SINE.Data <- N16cell.mean.skew[SINE.matched.line[!is.na(SINE.matched.line)], ]
#LTRs
LTR.matched.line <- match(rownames(LTR.rep), rownames(N16cell.mean.skew))
N16cell.LTR.Data <- N16cell.mean.skew[LTR.matched.line[!is.na(LTR.matched.line)], ]
#DNAs
DNA.matched.line <- match(rownames(DNA.rep), rownames(N16cell.mean.skew))
N16cell.DNA.Data <- N16cell.mean.skew[DNA.matched.line[!is.na(DNA.matched.line)], ]

#For earlyBlast stage
#LINEs
LINE.matched.line <- match(rownames(LINE.rep), rownames(earlyBlast.mean.skew))
earlyBlast.LINE.Data <- earlyBlast.mean.skew[LINE.matched.line[!is.na(LINE.matched.line)], ]
#cLINEs (by combing all replicates)
cLINE.matched.line <- match(rownames(LINE.rep), rownames(earlyBlast.combined.Data))
earlyBlast.cLINE.Data <- earlyBlast.combined.Data[cLINE.matched.line[!is.na(cLINE.matched.line)], ]

#SINEs
SINE.matched.line <- match(rownames(SINE.rep), rownames(earlyBlast.mean.skew))
earlyBlast.SINE.Data <- earlyBlast.mean.skew[SINE.matched.line[!is.na(SINE.matched.line)], ]
#LTRs
LTR.matched.line <- match(rownames(LTR.rep), rownames(earlyBlast.mean.skew))
earlyBlast.LTR.Data <- earlyBlast.mean.skew[LTR.matched.line[!is.na(LTR.matched.line)], ]
#DNAs
DNA.matched.line <- match(rownames(DNA.rep), rownames(earlyBlast.mean.skew))
earlyBlast.DNA.Data <- earlyBlast.mean.skew[DNA.matched.line[!is.na(DNA.matched.line)], ]


#1.LINEs

#e2C
chrX.early2cell.LINE <- early2cell.LINE.Data[ ,20]
#LINE.e2C <- data.frame(skew=chrX.early2cell.LINE, class="LINE", stage="early2C", age=LINE1.age$Age[match(rownames(early2cell.LINE.Data), rownames(LINE1.age))], subfamily=rownames(early2cell.LINE.Data), row.names=rownames(early2cell.LINE.Data))
LINE.e2C <- data.frame(skew=chrX.early2cell.LINE, class="LINE", stage="early2C",row.names=rownames(early2cell.LINE.Data))
LINE.e2C <- LINE.e2C[complete.cases(LINE.e2C), ]
#l2C
chrX.late2cell.LINE <- late2cell.LINE.Data[ ,20]
#LINE.l2C <- data.frame(skew=chrX.late2cell.LINE, class="LINE",stage="late2C", age=LINE1.age$Age[match(rownames(late2cell.LINE.Data), rownames(LINE1.age))], subfamily=rownames(late2cell.LINE.Data), row.names=rownames(late2cell.LINE.Data))
LINE.l2C <- data.frame(skew=chrX.late2cell.LINE, class="LINE",stage="late2C", row.names=rownames(late2cell.LINE.Data))
LINE.l2C <- LINE.l2C[complete.cases(LINE.l2C), ]
#4C
chrX.N4cell.LINE <- N4cell.LINE.Data[ ,20]
#LINE.N4C <- data.frame(skew=chrX.N4cell.LINE, class="LINE",stage="4C", age=LINE1.age$Age[match(rownames(N4cell.LINE.Data), rownames(LINE1.age))],subfamily=rownames(N4cell.LINE.Data), row.names=rownames(N4cell.LINE.Data))
LINE.N4C <- data.frame(skew=chrX.N4cell.LINE, class="LINE",stage="4C", row.names=rownames(N4cell.LINE.Data))
LINE.N4C <- LINE.N4C[complete.cases(LINE.N4C), ]
#8C
chrX.N8cell.LINE <- N8cell.LINE.Data[ ,20]
#LINE.N8C <- data.frame(skew=chrX.N8cell.LINE, class="LINE", stage="8C", age=LINE1.age$Age[match(rownames(N8cell.LINE.Data), rownames(LINE1.age))],subfamily=rownames(N8cell.LINE.Data), row.names=rownames(N8cell.LINE.Data))
LINE.N8C <- data.frame(skew=chrX.N8cell.LINE, class="LINE", stage="8C", row.names=rownames(N8cell.LINE.Data))
LINE.N8C <- LINE.N8C[complete.cases(LINE.N8C), ]
#16C
chrX.N16cell.LINE <- N16cell.LINE.Data[ ,20]
#LINE.N16C <- data.frame(skew=chrX.N16cell.LINE, class="LINE", stage="16C", age=LINE1.age$Age[match(rownames(N16cell.LINE.Data), rownames(LINE1.age))],subfamily=rownames(N16cell.LINE.Data), row.names=rownames(N16cell.LINE.Data))
LINE.N16C <- data.frame(skew=chrX.N16cell.LINE, class="LINE", stage="16C", row.names=rownames(N16cell.LINE.Data))
LINE.N16C <- LINE.N16C[complete.cases(LINE.N16C), ]
#eB
chrX.earlyBlast.LINE <- earlyBlast.LINE.Data[ ,20]
#LINE.eB <- data.frame(skew=chrX.earlyBlast.LINE, class="LINE", stage="earlyB.", age=LINE1.age$Age[match(rownames(earlyBlast.LINE.Data), rownames(LINE1.age))],subfamily=rownames(earlyBlast.LINE.Data), row.names=rownames(earlyBlast.LINE.Data))
LINE.eB <- data.frame(skew=chrX.earlyBlast.LINE, class="LINE", stage="earlyB.", row.names=rownames(earlyBlast.LINE.Data))
LINE.eB <- LINE.eB[complete.cases(LINE.eB), ]

#2. SINEs

#e2C
chrX.early2cell.SINE <- early2cell.SINE.Data[ ,20]
SINE.e2C <- data.frame(skew=chrX.early2cell.SINE, class="SINE", stage="early2C", row.names=rownames(early2cell.SINE.Data))
#SINE.e2C <- SINE.e2C[complete.cases(SINE.e2C), ]
#l2C
chrX.late2cell.SINE <- late2cell.SINE.Data[ ,20]
SINE.l2C <- data.frame(skew=chrX.late2cell.SINE, class="SINE",stage="late2C", row.names=rownames(late2cell.SINE.Data))
#4C
chrX.N4cell.SINE <- N4cell.SINE.Data[ ,20]
SINE.N4C <- data.frame(skew=chrX.N4cell.SINE, class="SINE",stage="4C", row.names=rownames(N4cell.SINE.Data))
#8C
chrX.N8cell.SINE <- N8cell.SINE.Data[ ,20]
SINE.N8C <- data.frame(skew=chrX.N8cell.SINE, class="SINE", stage="8C", row.names=rownames(N8cell.SINE.Data))
#16C
chrX.N16cell.SINE <- N16cell.SINE.Data[ ,20]
SINE.N16C <- data.frame(skew=chrX.N16cell.SINE, class="SINE", stage="16C", row.names=rownames(N16cell.SINE.Data))
#eB
chrX.earlyBlast.SINE <- earlyBlast.SINE.Data[ ,20]
SINE.eB <- data.frame(skew=chrX.earlyBlast.SINE, class="SINE", stage="earlyB.", row.names=rownames(earlyBlast.SINE.Data))

#3. LTR

#e2C
chrX.early2cell.LTR <- early2cell.LTR.Data[ ,20]
LTR.e2C <- data.frame(skew=chrX.early2cell.LTR, class="LTR", stage="early2C", row.names=rownames(early2cell.LTR.Data))
#LTR.e2C <- LTR.e2C[complete.cases(LTR.e2C), ]
#l2C
chrX.late2cell.LTR <- late2cell.LTR.Data[ ,20]
LTR.l2C <- data.frame(skew=chrX.late2cell.LTR, class="LTR",stage="late2C", row.names=rownames(late2cell.LTR.Data))
#4C
chrX.N4cell.LTR <- N4cell.LTR.Data[ ,20]
LTR.N4C <- data.frame(skew=chrX.N4cell.LTR, class="LTR",stage="4C", row.names=rownames(N4cell.LTR.Data))
#8C
chrX.N8cell.LTR <- N8cell.LTR.Data[ ,20]
LTR.N8C <- data.frame(skew=chrX.N8cell.LTR, class="LTR", stage="8C", row.names=rownames(N8cell.LTR.Data))
#16C
chrX.N16cell.LTR <- N16cell.LTR.Data[ ,20]
LTR.N16C <- data.frame(skew=chrX.N16cell.LTR, class="LTR", stage="16C", row.names=rownames(N16cell.LTR.Data))
#eB
chrX.earlyBlast.LTR <- earlyBlast.LTR.Data[ ,20]
LTR.eB <- data.frame(skew=chrX.earlyBlast.LTR, class="LTR", stage="earlyB.", row.names=rownames(earlyBlast.LTR.Data))



#4.cLINEs (LINEs from all combined replicates)

#e2C
chrX.early2cell.cLINE <- early2cell.cLINE.Data[ ,20]
#cLINE.e2C <- data.frame(skew=chrX.early2cell.cLINE, class="cLINE", stage="early2C", age=LINE1.age$Age[match(rownames(early2cell.cLINE.Data), rownames(LINE1.age))], subfamily=rownames(early2cell.cLINE.Data), row.names=rownames(early2cell.cLINE.Data))
cLINE.e2C <- data.frame(skew=chrX.early2cell.cLINE, class="cLINE", stage="early2C", row.names=rownames(early2cell.cLINE.Data))
cLINE.e2C <- cLINE.e2C[complete.cases(cLINE.e2C), ]
#l2C
chrX.late2cell.cLINE <- late2cell.cLINE.Data[ ,20]
#cLINE.l2C <- data.frame(skew=chrX.late2cell.cLINE, class="cLINE",stage="late2C", age=LINE1.age$Age[match(rownames(late2cell.cLINE.Data), rownames(LINE1.age))], subfamily=rownames(late2cell.cLINE.Data), row.names=rownames(late2cell.cLINE.Data))
cLINE.l2C <- data.frame(skew=chrX.late2cell.cLINE, class="cLINE",stage="late2C", row.names=rownames(late2cell.cLINE.Data))
cLINE.l2C <- cLINE.l2C[complete.cases(cLINE.l2C), ]
#4C
chrX.N4cell.cLINE <- N4cell.cLINE.Data[ ,20]
#cLINE.N4C <- data.frame(skew=chrX.N4cell.cLINE, class="cLINE",stage="4C", age=LINE1.age$Age[match(rownames(N4cell.cLINE.Data), rownames(LINE1.age))], subfamily=rownames(N4cell.cLINE.Data), row.names=rownames(N4cell.cLINE.Data))
cLINE.N4C <- data.frame(skew=chrX.N4cell.cLINE, class="cLINE",stage="4C", row.names=rownames(N4cell.cLINE.Data))
cLINE.N4C <- cLINE.N4C[complete.cases(cLINE.N4C), ]
#8C
chrX.N8cell.cLINE <- N8cell.cLINE.Data[ ,20]
#cLINE.N8C <- data.frame(skew=chrX.N8cell.cLINE, class="cLINE", stage="8C", age=LINE1.age$Age[match(rownames(N8cell.cLINE.Data), rownames(LINE1.age))], subfamily=rownames(N8cell.cLINE.Data), row.names=rownames(N8cell.cLINE.Data))
cLINE.N8C <- data.frame(skew=chrX.N8cell.cLINE, class="cLINE", stage="8C", row.names=rownames(N8cell.cLINE.Data))
cLINE.N8C <- cLINE.N8C[complete.cases(cLINE.N8C), ]
#16C
chrX.N16cell.cLINE <- N16cell.cLINE.Data[ ,20]
#cLINE.N16C <- data.frame(skew=chrX.N16cell.cLINE, class="cLINE", stage="16C", age=LINE1.age$Age[match(rownames(N16cell.cLINE.Data), rownames(LINE1.age))], subfamily=rownames(N16cell.cLINE.Data), row.names=rownames(N16cell.cLINE.Data))
cLINE.N16C <- data.frame(skew=chrX.N16cell.cLINE, class="cLINE", stage="16C", row.names=rownames(N16cell.cLINE.Data))
cLINE.N16C <- cLINE.N16C[complete.cases(cLINE.N16C), ]
#eB
chrX.earlyBlast.cLINE <- earlyBlast.cLINE.Data[ ,20]
#cLINE.eB <- data.frame(skew=chrX.earlyBlast.cLINE, class="cLINE", stage="earlyB.", age=LINE1.age$Age[match(rownames(earlyBlast.cLINE.Data), rownames(LINE1.age))], subfamily=rownames(earlyBlast.cLINE.Data), row.names=rownames(earlyBlast.cLINE.Data))
cLINE.eB <- data.frame(skew=chrX.earlyBlast.cLINE, class="cLINE", stage="earlyB.", row.names=rownames(earlyBlast.cLINE.Data))
cLINE.eB <- cLINE.eB[complete.cases(cLINE.eB), ]






