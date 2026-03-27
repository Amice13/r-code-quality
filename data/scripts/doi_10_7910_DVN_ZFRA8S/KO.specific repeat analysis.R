#load package
#library(gplots)
library(plyr)
library(pheatmap)
library(RColorBrewer)
library(ggplot2)
rm(list=ls())
source("CM.overall repeat analysis.R")
rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average","CM.X_skew",
                        "KO_late2cell.mean.skew","KO_4cell.mean.skew","KO_8cell.mean.skew","KO_earlyBlast.mean.skew",
                        "KO_late2cell.combined.Data","KO_4cell.combined.Data","KO_8cell.combined.Data","KO_earlyBlast.combined.Data")))

LINE.rep <- repeat.type.family[which(repeat.type.family[ ,2] == "LINE"), ]
SINE.rep <- repeat.type.family[which(repeat.type.family[ ,2] == "SINE"), ]
LTR.rep <- repeat.type.family[which(repeat.type.family[ ,2] == "LTR"), ]
DNA.rep <- repeat.type.family[which(repeat.type.family[ ,2] == "DNA"), ]
#LINE-1 age
#LINE1.age <- read.table(file="processed/LINE-1_age.txt",sep="\t")



#For late2cell stage
#LINEs
LINE.matched.line <- match(rownames(LINE.rep), rownames(KO_late2cell.mean.skew))
late2cell.LINE.Data <- KO_late2cell.mean.skew[LINE.matched.line[!is.na(LINE.matched.line)], ]
#cLINEs (by combing all replicates)
##cLINE.matched.line <- match(rownames(LINE.rep), rownames(KO_late2cell.combined.Data))
##late2cell.cLINE.Data <- KO_late2cell.combined.Data[cLINE.matched.line[!is.na(cLINE.matched.line)], ]

#SINEs
SINE.matched.line <- match(rownames(SINE.rep), rownames(KO_late2cell.mean.skew))
late2cell.SINE.Data <- KO_late2cell.mean.skew[SINE.matched.line[!is.na(SINE.matched.line)], ]
#LTRs
LTR.matched.line <- match(rownames(LTR.rep), rownames(KO_late2cell.mean.skew))
late2cell.LTR.Data <- KO_late2cell.mean.skew[LTR.matched.line[!is.na(LTR.matched.line)], ]
#DNAs
DNA.matched.line <- match(rownames(DNA.rep), rownames(KO_late2cell.mean.skew))
late2cell.DNA.Data <- KO_late2cell.mean.skew[DNA.matched.line[!is.na(DNA.matched.line)], ]

#For 4cell stage
#LINEs
LINE.matched.line <- match(rownames(LINE.rep), rownames(KO_4cell.mean.skew))
N4cell.LINE.Data <- KO_4cell.mean.skew[LINE.matched.line[!is.na(LINE.matched.line)], ]
#cLINEs (by combing all replicates)
##cLINE.matched.line <- match(rownames(LINE.rep), rownames(KO_4cell.combined.Data))
##N4cell.cLINE.Data <- KO_4cell.combined.Data[cLINE.matched.line[!is.na(cLINE.matched.line)], ]

#SINEs
SINE.matched.line <- match(rownames(SINE.rep), rownames(KO_4cell.mean.skew))
N4cell.SINE.Data <- KO_4cell.mean.skew[SINE.matched.line[!is.na(SINE.matched.line)], ]
#LTRs
LTR.matched.line <- match(rownames(LTR.rep), rownames(KO_4cell.mean.skew))
N4cell.LTR.Data <- KO_4cell.mean.skew[LTR.matched.line[!is.na(LTR.matched.line)], ]
#DNAs
DNA.matched.line <- match(rownames(DNA.rep), rownames(KO_4cell.mean.skew))
N4cell.DNA.Data <- KO_4cell.mean.skew[DNA.matched.line[!is.na(DNA.matched.line)], ]

#For 8cell stage
#LINEs
LINE.matched.line <- match(rownames(LINE.rep), rownames(KO_8cell.mean.skew))
N8cell.LINE.Data <- KO_8cell.mean.skew[LINE.matched.line[!is.na(LINE.matched.line)], ]
#cLINEs (by combing all replicates)
##cLINE.matched.line <- match(rownames(LINE.rep), rownames(KO_8cell.combined.Data))
##N8cell.cLINE.Data <- KO_8cell.combined.Data[cLINE.matched.line[!is.na(cLINE.matched.line)], ]

#SINEs
SINE.matched.line <- match(rownames(SINE.rep), rownames(KO_8cell.mean.skew))
N8cell.SINE.Data <- KO_8cell.mean.skew[SINE.matched.line[!is.na(SINE.matched.line)], ]
#LTRs
LTR.matched.line <- match(rownames(LTR.rep), rownames(KO_8cell.mean.skew))
N8cell.LTR.Data <- KO_8cell.mean.skew[LTR.matched.line[!is.na(LTR.matched.line)], ]
#DNAs
DNA.matched.line <- match(rownames(DNA.rep), rownames(KO_8cell.mean.skew))
N8cell.DNA.Data <- KO_8cell.mean.skew[DNA.matched.line[!is.na(DNA.matched.line)], ]

#For earlyBlast stage
#LINEs
LINE.matched.line <- match(rownames(LINE.rep), rownames(KO_earlyBlast.mean.skew))
earlyBlast.LINE.Data <- KO_earlyBlast.mean.skew[LINE.matched.line[!is.na(LINE.matched.line)], ]
#cLINEs (by combing all replicates)
##cLINE.matched.line <- match(rownames(LINE.rep), rownames(KO_earlyBlast.combined.Data))
##earlyBlast.cLINE.Data <- KO_earlyBlast.combined.Data[cLINE.matched.line[!is.na(cLINE.matched.line)], ]

#SINEs
SINE.matched.line <- match(rownames(SINE.rep), rownames(KO_earlyBlast.mean.skew))
earlyBlast.SINE.Data <- KO_earlyBlast.mean.skew[SINE.matched.line[!is.na(SINE.matched.line)], ]
#LTRs
LTR.matched.line <- match(rownames(LTR.rep), rownames(KO_earlyBlast.mean.skew))
earlyBlast.LTR.Data <- KO_earlyBlast.mean.skew[LTR.matched.line[!is.na(LTR.matched.line)], ]
#DNAs
DNA.matched.line <- match(rownames(DNA.rep), rownames(KO_earlyBlast.mean.skew))
earlyBlast.DNA.Data <- KO_earlyBlast.mean.skew[DNA.matched.line[!is.na(DNA.matched.line)], ]


#1.LINEs

#l2C
chrX.late2cell.LINE <- late2cell.LINE.Data[ ,20]
#LINE.l2C <- data.frame(skew=chrX.late2cell.LINE, class="LINE",stage="late2C", age=LINE1.age$Age[match(rownames(late2cell.LINE.Data), rownames(LINE1.age))], subfamily=rownames(late2cell.LINE.Data), row.names=rownames(late2cell.LINE.Data))
LINE.l2C <- data.frame(skew=chrX.late2cell.LINE, class="LINE",stage="late2C", row.names=rownames(late2cell.LINE.Data))
LINE.KO_l2C <- LINE.l2C[complete.cases(LINE.l2C), ]
#4C
chrX.N4cell.LINE <- N4cell.LINE.Data[ ,20]
#LINE.N4C <- data.frame(skew=chrX.N4cell.LINE, class="LINE",stage="4C", age=LINE1.age$Age[match(rownames(N4cell.LINE.Data), rownames(LINE1.age))],subfamily=rownames(N4cell.LINE.Data), row.names=rownames(N4cell.LINE.Data))
LINE.N4C <- data.frame(skew=chrX.N4cell.LINE, class="LINE",stage="4C", row.names=rownames(N4cell.LINE.Data))
LINE.KO_4C <- LINE.N4C[complete.cases(LINE.N4C), ]
#8C
chrX.N8cell.LINE <- N8cell.LINE.Data[ ,20]
#LINE.N8C <- data.frame(skew=chrX.N8cell.LINE, class="LINE", stage="8C", age=LINE1.age$Age[match(rownames(N8cell.LINE.Data), rownames(LINE1.age))],subfamily=rownames(N8cell.LINE.Data), row.names=rownames(N8cell.LINE.Data))
LINE.N8C <- data.frame(skew=chrX.N8cell.LINE, class="LINE", stage="8C", row.names=rownames(N8cell.LINE.Data))
LINE.KO_8C <- LINE.N8C[complete.cases(LINE.N8C), ]
#eB
chrX.earlyBlast.LINE <- earlyBlast.LINE.Data[ ,20]
#LINE.eB <- data.frame(skew=chrX.earlyBlast.LINE, class="LINE", stage="earlyB.", age=LINE1.age$Age[match(rownames(earlyBlast.LINE.Data), rownames(LINE1.age))],subfamily=rownames(earlyBlast.LINE.Data), row.names=rownames(earlyBlast.LINE.Data))
LINE.eB <- data.frame(skew=chrX.earlyBlast.LINE, class="LINE", stage="earlyB.", row.names=rownames(earlyBlast.LINE.Data))
LINE.KO_eB <- LINE.eB[complete.cases(LINE.eB), ]

#2. SINEs

#l2C
chrX.late2cell.SINE <- late2cell.SINE.Data[ ,20]
SINE.KO_l2C <- data.frame(skew=chrX.late2cell.SINE, class="SINE",stage="late2C", row.names=rownames(late2cell.SINE.Data))
#4C
chrX.N4cell.SINE <- N4cell.SINE.Data[ ,20]
SINE.KO_4C <- data.frame(skew=chrX.N4cell.SINE, class="SINE",stage="4C", row.names=rownames(N4cell.SINE.Data))
#8C
chrX.N8cell.SINE <- N8cell.SINE.Data[ ,20]
SINE.KO_8C <- data.frame(skew=chrX.N8cell.SINE, class="SINE", stage="8C", row.names=rownames(N8cell.SINE.Data))
#eB
chrX.earlyBlast.SINE <- earlyBlast.SINE.Data[ ,20]
SINE.KO_eB <- data.frame(skew=chrX.earlyBlast.SINE, class="SINE", stage="earlyB.", row.names=rownames(earlyBlast.SINE.Data))

#3. LTR

#l2C
chrX.late2cell.LTR <- late2cell.LTR.Data[ ,20]
LTR.KO_l2C <- data.frame(skew=chrX.late2cell.LTR, class="LTR",stage="late2C", row.names=rownames(late2cell.LTR.Data))
#4C
chrX.N4cell.LTR <- N4cell.LTR.Data[ ,20]
LTR.KO_4C <- data.frame(skew=chrX.N4cell.LTR, class="LTR",stage="4C", row.names=rownames(N4cell.LTR.Data))
#8C
chrX.N8cell.LTR <- N8cell.LTR.Data[ ,20]
LTR.KO_8C <- data.frame(skew=chrX.N8cell.LTR, class="LTR", stage="8C", row.names=rownames(N8cell.LTR.Data))
#eB
chrX.earlyBlast.LTR <- earlyBlast.LTR.Data[ ,20]
LTR.KO_eB <- data.frame(skew=chrX.earlyBlast.LTR, class="LTR", stage="earlyB.", row.names=rownames(earlyBlast.LTR.Data))



#4.cLINEs (LINEs from all combined replicates)

#l2C
##chrX.late2cell.cLINE <- late2cell.cLINE.Data[ ,20]
#cLINE.l2C <- data.frame(skew=chrX.late2cell.cLINE, class="cLINE",stage="late2C", age=LINE1.age$Age[match(rownames(late2cell.cLINE.Data), rownames(LINE1.age))], subfamily=rownames(late2cell.cLINE.Data), row.names=rownames(late2cell.cLINE.Data))
##cLINE.l2C <- data.frame(skew=chrX.late2cell.cLINE, class="cLINE",stage="late2C", row.names=rownames(late2cell.cLINE.Data))
##cLINE.KO_l2C <- cLINE.l2C[complete.cases(cLINE.l2C), ]
#4C
##chrX.N4cell.cLINE <- N4cell.cLINE.Data[ ,20]
#cLINE.N4C <- data.frame(skew=chrX.N4cell.cLINE, class="cLINE",stage="4C", age=LINE1.age$Age[match(rownames(N4cell.cLINE.Data), rownames(LINE1.age))], subfamily=rownames(N4cell.cLINE.Data), row.names=rownames(N4cell.cLINE.Data))
##cLINE.N4C <- data.frame(skew=chrX.N4cell.cLINE, class="cLINE",stage="4C", row.names=rownames(N4cell.cLINE.Data))
##cLINE.KO_4C <- cLINE.N4C[complete.cases(cLINE.N4C), ]
#8C
##chrX.N8cell.cLINE <- N8cell.cLINE.Data[ ,20]
#cLINE.N8C <- data.frame(skew=chrX.N8cell.cLINE, class="cLINE", stage="8C", age=LINE1.age$Age[match(rownames(N8cell.cLINE.Data), rownames(LINE1.age))], subfamily=rownames(N8cell.cLINE.Data), row.names=rownames(N8cell.cLINE.Data))
##cLINE.N8C <- data.frame(skew=chrX.N8cell.cLINE, class="cLINE", stage="8C", row.names=rownames(N8cell.cLINE.Data))
##cLINE.KO_8C <- cLINE.N8C[complete.cases(cLINE.N8C), ]
#eB
##chrX.earlyBlast.cLINE <- earlyBlast.cLINE.Data[ ,20]
#cLINE.eB <- data.frame(skew=chrX.earlyBlast.cLINE, class="cLINE", stage="earlyB.", age=LINE1.age$Age[match(rownames(earlyBlast.cLINE.Data), rownames(LINE1.age))], subfamily=rownames(earlyBlast.cLINE.Data), row.names=rownames(earlyBlast.cLINE.Data))
##cLINE.eB <- data.frame(skew=chrX.earlyBlast.cLINE, class="cLINE", stage="earlyB.", row.names=rownames(earlyBlast.cLINE.Data))
##cLINE.KO_eB <- cLINE.eB[complete.cases(cLINE.eB), ]






