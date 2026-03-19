rm(list=ls())
##### For Repeats analysis
#define functions
#function1
Chr_Average <- function(x, y){
  chr10.skew <- rowMeans(x[ ,c(1:y)], na.rm=T)
  chr11.skew <- rowMeans(x[ ,c((y+1):(y*2))], na.rm=T)
  chr12.skew <- rowMeans(x[ ,c((y*2+1):(y*3))], na.rm=T)
  chr13.skew <- rowMeans(x[ ,c((y*3+1):(y*4))], na.rm=T)
  chr14.skew <- rowMeans(x[ ,c((y*4+1):(y*5))], na.rm=T)
  chr15.skew <- rowMeans(x[ ,c((y*5+1):(y*6))], na.rm=T)
  chr16.skew <- rowMeans(x[ ,c((y*6+1):(y*7))], na.rm=T)
  chr17.skew <- rowMeans(x[ ,c((y*7+1):(y*8))], na.rm=T)
  chr18.skew <- rowMeans(x[ ,c((y*8+1):(y*9))], na.rm=T)
  chr19.skew <- rowMeans(x[ ,c((y*9+1):(y*10))], na.rm=T)
  chr1.skew <- rowMeans(x[ ,c((y*10+1):(y*11))], na.rm=T)
  chr2.skew <- rowMeans(x[ ,c((y*11+1):(y*12))], na.rm=T)
  chr3.skew <- rowMeans(x[ ,c((y*12+1):(y*13))], na.rm=T)
  chr4.skew <- rowMeans(x[ ,c((y*13+1):(y*14))], na.rm=T)
  chr5.skew <- rowMeans(x[ ,c((y*14+1):(y*15))], na.rm=T)
  chr6.skew <- rowMeans(x[ ,c((y*15+1):(y*16))], na.rm=T)
  chr7.skew <- rowMeans(x[ ,c((y*16+1):(y*17))], na.rm=T)
  chr8.skew <- rowMeans(x[ ,c((y*17+1):(y*18))], na.rm=T)
  chr9.skew <- rowMeans(x[ ,c((y*18+1):(y*19))], na.rm=T)
  chrX.skew <- rowMeans(x[ ,c((y*19+1):(y*20))], na.rm=T)
  
  Combined <- cbind(chr1.skew,chr2.skew,chr3.skew,chr4.skew,chr5.skew,chr6.skew,chr7.skew,chr8.skew,chr9.skew,chr10.skew,chr11.skew,chr12.skew,chr13.skew,chr14.skew,chr15.skew,chr16.skew,chr17.skew,chr18.skew,chr19.skew,chrX.skew)
  
  return(Combined)
}

#function2
Auto.X_Average <- function(x, y){
  chr10.skew <- rowMeans(x[ ,c(1:y)], na.rm=T)
  chr11.skew <- rowMeans(x[ ,c((y+1):(y*2))], na.rm=T)
  chr12.skew <- rowMeans(x[ ,c((y*2+1):(y*3))], na.rm=T)
  chr13.skew <- rowMeans(x[ ,c((y*3+1):(y*4))], na.rm=T)
  chr14.skew <- rowMeans(x[ ,c((y*4+1):(y*5))], na.rm=T)
  chr15.skew <- rowMeans(x[ ,c((y*5+1):(y*6))], na.rm=T)
  chr16.skew <- rowMeans(x[ ,c((y*6+1):(y*7))], na.rm=T)
  chr17.skew <- rowMeans(x[ ,c((y*7+1):(y*8))], na.rm=T)
  chr18.skew <- rowMeans(x[ ,c((y*8+1):(y*9))], na.rm=T)
  chr19.skew <- rowMeans(x[ ,c((y*9+1):(y*10))], na.rm=T)
  chr1.skew <- rowMeans(x[ ,c((y*10+1):(y*11))], na.rm=T)
  chr2.skew <- rowMeans(x[ ,c((y*11+1):(y*12))], na.rm=T)
  chr3.skew <- rowMeans(x[ ,c((y*12+1):(y*13))], na.rm=T)
  chr4.skew <- rowMeans(x[ ,c((y*13+1):(y*14))], na.rm=T)
  chr5.skew <- rowMeans(x[ ,c((y*14+1):(y*15))], na.rm=T)
  chr6.skew <- rowMeans(x[ ,c((y*15+1):(y*16))], na.rm=T)
  chr7.skew <- rowMeans(x[ ,c((y*16+1):(y*17))], na.rm=T)
  chr8.skew <- rowMeans(x[ ,c((y*17+1):(y*18))], na.rm=T)
  chr9.skew <- rowMeans(x[ ,c((y*18+1):(y*19))], na.rm=T)
  chrX.skew <- rowMeans(x[ ,c((y*19+1):(y*20))], na.rm=T)
  
  auto.combined <- data.frame(chr="auto", skewing=c(chr1.skew,chr2.skew,chr3.skew,chr4.skew,chr5.skew,chr6.skew,chr7.skew,chr8.skew,chr9.skew,chr10.skew,chr11.skew,chr12.skew,chr13.skew,chr14.skew,chr15.skew,chr16.skew,chr17.skew,chr18.skew,chr19.skew))
  chrX.combined <- data.frame(chr="chrX", skewing=chrX.skew)
  auto.chrX.combined <- rbind(auto.combined, chrX.combined)
  
  return(auto.chrX.combined)
  
}

#function3
X.chr_Average <- function(x, y){
  chr10.skew <- rowMeans(x[ ,c(1:y)], na.rm=T)
  chr11.skew <- rowMeans(x[ ,c((y+1):(y*2))], na.rm=T)
  chr12.skew <- rowMeans(x[ ,c((y*2+1):(y*3))], na.rm=T)
  chr13.skew <- rowMeans(x[ ,c((y*3+1):(y*4))], na.rm=T)
  chr14.skew <- rowMeans(x[ ,c((y*4+1):(y*5))], na.rm=T)
  chr15.skew <- rowMeans(x[ ,c((y*5+1):(y*6))], na.rm=T)
  chr16.skew <- rowMeans(x[ ,c((y*6+1):(y*7))], na.rm=T)
  chr17.skew <- rowMeans(x[ ,c((y*7+1):(y*8))], na.rm=T)
  chr18.skew <- rowMeans(x[ ,c((y*8+1):(y*9))], na.rm=T)
  chr19.skew <- rowMeans(x[ ,c((y*9+1):(y*10))], na.rm=T)
  chr1.skew <- rowMeans(x[ ,c((y*10+1):(y*11))], na.rm=T)
  chr2.skew <- rowMeans(x[ ,c((y*11+1):(y*12))], na.rm=T)
  chr3.skew <- rowMeans(x[ ,c((y*12+1):(y*13))], na.rm=T)
  chr4.skew <- rowMeans(x[ ,c((y*13+1):(y*14))], na.rm=T)
  chr5.skew <- rowMeans(x[ ,c((y*14+1):(y*15))], na.rm=T)
  chr6.skew <- rowMeans(x[ ,c((y*15+1):(y*16))], na.rm=T)
  chr7.skew <- rowMeans(x[ ,c((y*16+1):(y*17))], na.rm=T)
  chr8.skew <- rowMeans(x[ ,c((y*17+1):(y*18))], na.rm=T)
  chr9.skew <- rowMeans(x[ ,c((y*18+1):(y*19))], na.rm=T)
  chrX.skew <- rowMeans(x[ ,c((y*19+1):(y*20))], na.rm=T)
  
  auto.skew <- c(chr1.skew,chr2.skew,chr3.skew,chr4.skew,chr5.skew,chr6.skew,chr7.skew,chr8.skew,chr9.skew,chr10.skew,chr11.skew,chr12.skew,chr13.skew,chr14.skew,chr15.skew,chr16.skew,chr17.skew,chr18.skew,chr19.skew)
  auto.combined <- data.frame(chr="auto", skewing=auto.skew[!is.na(auto.skew)])
  chrX <- data.frame(chr="chrX", skewing=chrX.skew[!is.na(chrX.skew)])
  chr1 <- data.frame(chr="chr1", skewing=chr1.skew[!is.na(chr1.skew)])
  chr13 <- data.frame(chr="chr13", skewing=chr13.skew[!is.na(chr13.skew)])
  
  auto.chrX.combined <- rbind(chrX, chr1, chr13)
  
  return(auto.chrX.combined)
  
}

#function4
X.chr_Total <- function(x, y){
  
  chr10.skew <- x[ ,1]
  for(i in 2:y) {
    chr10.i.skew <- x[ ,i]
    chr10.skew <- c(chr10.skew, chr10.i.skew)
  }
  
  chr11.skew <- x[ ,y+1]
  for(i in y+2:y*2) {
    chr11.i.skew <- x[ ,i]
    chr11.skew <- c(chr11.skew, chr11.i.skew)
  }
  
  chr12.skew <- x[ ,y*2+1]  
  for(i in y*2+2:y*3) {
    chr12.i.skew <- x[ ,i]
    chr12.skew <- c(chr12.skew, chr12.i.skew)
  }
  
  chr13.skew <- x[ ,y*3+1]  
  for(i in y*3+2:y*4) {
    chr13.i.skew <- x[ ,i]
    chr13.skew <- c(chr13.skew, chr13.i.skew)
  }  
  
  chr14.skew <- x[ ,y*4+1]  
  for(i in y*4+2:y*5) {
    chr14.i.skew <- x[ ,i]
    chr14.skew <- c(chr14.skew, chr14.i.skew)
  }    
  
  chr15.skew <- x[ ,y*5+1]  
  for(i in y*5+2:y*6) {
    chr15.i.skew <- x[ ,i]
    chr15.skew <- c(chr15.skew, chr15.i.skew)
  }  
  
  chr16.skew <- x[ ,y*6+1]  
  for(i in y*6+2:y*7) {
    chr16.i.skew <- x[ ,i]
    chr16.skew <- c(chr16.skew, chr16.i.skew)
  }  
  
  chr17.skew <- x[ ,y*7+1]  
  for(i in y*7+2:y*8) {
    chr17.i.skew <- x[ ,i]
    chr17.skew <- c(chr17.skew, chr17.i.skew)
  }  
  
  chr18.skew <- x[ ,y*8+1]  
  for(i in y*8+2:y*9) {
    chr18.i.skew <- x[ ,i]
    chr18.skew <- c(chr18.skew, chr18.i.skew)
  }  
  
  chr19.skew <- x[ ,y*9+1]  
  for(i in y*9+2:y*10) {
    chr19.i.skew <- x[ ,i]
    chr19.skew <- c(chr19.skew, chr19.i.skew)
  }  
  
  chr1.skew <- x[ ,y*10+1]  
  for(i in (y*10+2):(y*11)) {
    chr1.i.skew <- x[ ,i]
    chr1.skew <- c(chr1.skew, chr1.i.skew)
  }  
  
  chr2.skew <- x[ ,y*11+1]  
  for(i in (y*11+2):(y*12)) {
    chr2.i.skew <- x[ ,i]
    chr2.skew <- c(chr2.skew, chr2.i.skew)
  }  
  
  chr3.skew <- x[ ,y*12+1]  
  for(i in (y*12+2):(y*13)) {
    chr3.i.skew <- x[ ,i]
    chr3.skew <- c(chr3.skew, chr3.i.skew)
  } 
  
  chr4.skew <- x[ ,y*13+1]  
  for(i in (y*13+2):(y*14)) {
    chr4.i.skew <- x[ ,i]
    chr4.skew <- c(chr4.skew, chr4.i.skew)
  } 
  
  chr5.skew <- x[ ,y*14+1] 
  for(i in (y*14+2):(y*15)) {
    chr5.i.skew <- x[ ,i]
    chr5.skew <- c(chr5.skew, chr5.i.skew)
  } 
  
  chr6.skew <- x[ ,y*15+1] 
  for(i in (y*15+2):(y*16)) {
    chr6.i.skew <- x[ ,i]
    chr6.skew <- c(chr6.skew, chr6.i.skew)
  } 
  
  chr7.skew <- x[ ,y*16+1]  
  for(i in (y*16+2):(y*17)) {
    chr7.i.skew <- x[ ,i]
    chr7.skew <- c(chr7.skew, chr7.i.skew)
  } 
  
  chr8.skew <- x[ ,y*17+1]
  for(i in (y*17+2):(y*18)) {
    chr8.i.skew <- x[ ,i]
    chr8.skew <- c(chr8.skew, chr8.i.skew)
  }
  
  chr9.skew <- x[ ,y*18+1]  
  for(i in (y*18+2):(y*19)) {
    chr9.i.skew <- x[ ,i]
    chr9.skew <- c(chr9.skew, chr9.i.skew)
  } 
  
  
  chrX.skew <- x[ ,y*19+1] 
  for(i in (y*19+2):(y*20)) {
    chrX.i.skew <- x[ ,i]
    chrX.skew <- c(chrX.skew, chrX.i.skew)
  } 
  
  auto.skew <- c(chr1.skew,chr2.skew,chr3.skew,chr4.skew,chr5.skew,chr6.skew,chr7.skew,chr8.skew,chr9.skew,chr10.skew,chr11.skew,chr12.skew,chr13.skew,chr14.skew,chr15.skew,chr16.skew,chr17.skew,chr18.skew,chr19.skew)
  auto.combined <- data.frame(chr="auto", skewing=auto.skew[!is.na(auto.skew)])  
  chrX <- data.frame(chr="chrX", skewing=chrX.skew[!is.na(chrX.skew)])
  chr1 <- data.frame(chr="chr1", skewing=chr1.skew[!is.na(chr1.skew)])
  chr13 <- data.frame(chr="chr13", skewing=chr13.skew[!is.na(chr13.skew)])
  
  auto.chrX.combined <- rbind(chrX, chr1, chr13)
  return(auto.chrX.combined)
  
}

#load package
library(gplots)
library(plyr)
library(pheatmap)
library(RColorBrewer)
library(ggplot2)

#Preload and process the original stat files for each stage
repeat.type.family <- read.table(file="Attached_doc/repeat_class_family", sep="\t")
#Sort the repeats based on their classes
repeat.type.family <- repeat.type.family[order(repeat.type.family[ ,2]), ]
#Row 307, 308 and 313 were found duplicated names, although they belong to different repeat types.
#Use command << duplicated(repeat.type.family.sorted[[1]]) >> to look for duplicated lines.
repeat.type.family <- repeat.type.family[-c(307, 308, 313), ]
rownames(repeat.type.family) <- repeat.type.family$V1


########## Figure 6f. skew comprison of genes and LINEs between WT and KO embryo at 8C and eB ###############
#bottom panel - for LINEs. Gene analysis is in 'Figure 6-part1.R' 

#wt 8cell stage
#run parseTbl.totalMatrix.sh rep_late2cell_R_output.txt
All.Data <- read.table(file="rep_data/8cell.allExperiments.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="rep_data/8cell.allExperiments.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="rep_data/8cell.allExperiments.repColNames.txt", sep="\t")
rownames(All.Data) <- All.rowname[ ,1]
colnames(All.Data) <- All.colname[ ,1]
All.Data <- t(All.Data)
matched.line <- match(rownames(repeat.type.family), rownames(All.Data))
All.Data <- All.Data[matched.line[!is.na(matched.line)], ]
#--------------------------------
#pull out repeat counts from chrX
N8cell.chrX <- All.Data[ ,c(115:120)]
colnames(N8cell.chrX) <- c("N8cell_1","N8cell_2","N8cell_3","N8cell_4","N8cell_5","N8cell_6")
#---------------------------------------------------------------------
#filter out repeats expressed in less than 5 samples in autosomes or at least 1 sample on chrX
All.Data <- All.Data[which(rowSums(!is.nan(All.Data[ ,c(1:114)])) >=5 | rowSums(!is.nan(All.Data[ ,c(115:120)])) >=1), ]
#Average the skewing for auto, chrX and chr1.
N8cell.skew <- data.frame(X.chr_Average(All.Data, 6), stage="8C",sample="wt")
#Need only X-linked repeats
N8cell.skew <- N8cell.skew[which(N8cell.skew$chr == "chrX"), ]
#for future X-linked LINE use
LINE.rep <- repeat.type.family[which(repeat.type.family[ ,2] == "LINE"), ]
LINE.matched.line <- match(rownames(LINE.rep), rownames(All.Data))
N8cell.LINE.Data <- All.Data[LINE.matched.line[!is.na(LINE.matched.line)], ]
N8cell.LINE.skew <- data.frame(X.chr_Total(N8cell.LINE.Data, 6), stage="8C", sample="WT")
N8cell.LINE.skew <- N8cell.LINE.skew[which(N8cell.LINE.skew$chr == "chrX"), ]


#For wt earlyBlast stage
#clean up unsed variables
rm(list=setdiff(ls(), c("X.chr_Total","repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average",
                        "N8cell.skew","N8cell.LINE.skew")))
#run parseTbl.totalMatrix.sh rep_late2cell_R_output.txt
All.Data <- read.table(file="rep_data/earlyBlast.allExperiments.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="rep_data/earlyBlast.allExperiments.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="rep_data/earlyBlast.allExperiments.repColNames.txt", sep="\t")
rownames(All.Data) <- All.rowname[ ,1]
colnames(All.Data) <- All.colname[ ,1]
All.Data <- t(All.Data)
matched.line <- match(rownames(repeat.type.family), rownames(All.Data))
All.Data <- All.Data[matched.line[!is.na(matched.line)], ]
#--------------------------------
#pull out repeat counts from chrX
earlyBlast.chrX <- All.Data[ ,c(96:100)]
colnames(earlyBlast.chrX) <- c("earlyBlast_1","earlyBlast_2","earlyBlast_3","earlyBlast_4","earlyBlast_5")
#------------------------------------------------------------------------
#filter out repeats expressed in less than 5 samples in autosomes
All.Data <- All.Data[which(rowSums(!is.nan(All.Data[ ,c(1:95)])) >=5 | rowSums(!is.nan(All.Data[ ,c(96:100)])) >=1), ]
#Average the skewing for auto, chrX and chr1.
earlyBlast.skew <- data.frame(X.chr_Average(All.Data, 5), stage="eB", sample="wt")
#Need only X-linked repeats
earlyBlast.skew <- earlyBlast.skew[which(earlyBlast.skew$chr == "chrX"), ]
#for future LINE use
LINE.rep <- repeat.type.family[which(repeat.type.family[ ,2] == "LINE"), ]
LINE.matched.line <- match(rownames(LINE.rep), rownames(All.Data))
earlyBlast.LINE.Data <- All.Data[LINE.matched.line[!is.na(LINE.matched.line)], ]
earlyBlast.LINE.skew <- data.frame(X.chr_Total(earlyBlast.LINE.Data, 5), stage="eB", sample="WT")
earlyBlast.LINE.skew <- earlyBlast.LINE.skew[which(earlyBlast.LINE.skew$chr == "chrX"), ]


#For KO 8cell stage
#clean up unsed variables
rm(list=setdiff(ls(), c("X.chr_Total","repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average",
                        "N8cell.skew","earlyBlast.skew",
                        "N8cell.LINE.skew","earlyBlast.LINE.skew")))
#run parseTbl.totalMatrix.sh rep_KO-eB_R_output.txt
All.Data <- read.table(file="rep_data/KO_8cell.allExperiments.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="rep_data/KO_8cell.allExperiments.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="rep_data/KO_8cell.allExperiments.repColNames.txt", sep="\t")
rownames(All.Data) <- All.rowname[ ,1]
colnames(All.Data) <- All.colname[ ,1]
All.Data <- t(All.Data)
matched.line <- match(rownames(repeat.type.family), rownames(All.Data))
All.Data <- All.Data[matched.line[!is.na(matched.line)], ]
#filter out repeats expressed in less than 5 samples in autosomes or less than 1 in chrX
All.Data <- All.Data[which(rowSums(!is.nan(All.Data[ ,c(1:114)])) >=5 | rowSums(!is.nan(All.Data[ ,c(115:120)])) >=1), ]
#Average the skewing for auto, chrX and chr1.
KO_8cell.skew <- data.frame(X.chr_Average(All.Data, 6), stage="8C", sample="XistKO")
#Need only X-linked repeats
KO_8cell.skew <- KO_8cell.skew[which(KO_8cell.skew$chr == "chrX"), ]
#for future LINE use
LINE.rep <- repeat.type.family[which(repeat.type.family[ ,2] == "LINE"), ]
LINE.matched.line <- match(rownames(LINE.rep), rownames(All.Data))
KO_8cell.LINE.Data <- All.Data[LINE.matched.line[!is.na(LINE.matched.line)], ]
KO_8cell.LINE.skew <- data.frame(X.chr_Total(KO_8cell.LINE.Data, 6), stage="8C", sample="XistKO")
KO_8cell.LINE.skew <- KO_8cell.LINE.skew[which(KO_8cell.LINE.skew$chr == "chrX"), ]


#For KO_earlyBlast
#clean up unsed variables
rm(list=setdiff(ls(), c("X.chr_Total","repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average",
                        "N8cell.skew","earlyBlast.skew",
                        "KO_8cell.skew","N8cell.LINE.skew",
                        "earlyBlast.LINE.skew", "KO_8cell.LINE.skew")))
#run parseTbl.totalMatrix.sh rep_KO-eB_R_output.txt
All.Data <- read.table(file="rep_data/KO_eB.allExperiments.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="rep_data/KO_eB.allExperiments.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="rep_data/KO_eB.allExperiments.repColNames.txt", sep="\t")
rownames(All.Data) <- All.rowname[ ,1]
colnames(All.Data) <- All.colname[ ,1]
All.Data <- t(All.Data)
#All.Data <- All.Data[-374, ]
matched.line <- match(rownames(repeat.type.family), rownames(All.Data))
All.Data <- All.Data[matched.line[!is.na(matched.line)], ]
#filter out repeats expressed in less than 5 samples in autosomes or less than 1 in chrX
All.Data <- All.Data[which(rowSums(!is.nan(All.Data[ ,c(1:133)])) >=5 | rowSums(!is.nan(All.Data[ ,c(134:140)])) >=1), ]
#Average the skewing for chrX
KO_eB.skew <- data.frame(X.chr_Average(All.Data, 7), stage="eB", sample="XistKO")
#Need only X-linked repeats
KO_eB.skew <- KO_eB.skew[which(KO_eB.skew$chr == "chrX"), ]
#for future LINE use
LINE.rep <- repeat.type.family[which(repeat.type.family[ ,2] == "LINE"), ]
LINE.matched.line <- match(rownames(LINE.rep), rownames(All.Data))
KO_eB.LINE.Data <- All.Data[LINE.matched.line[!is.na(LINE.matched.line)], ]
KO_eB.LINE.skew <- data.frame(X.chr_Total(KO_eB.LINE.Data, 7), stage="eB", sample="XistKO")
KO_eB.LINE.skew <- KO_eB.LINE.skew[which(KO_eB.LINE.skew$chr == "chrX"), ]


#For LINEs, skewing values is the average of all replicates.

#8C_KO
LINE.rep <- repeat.type.family[which(repeat.type.family[ ,2] == "LINE"), ]
LINE.matched.line <- match(rownames(LINE.rep), rownames(KO_8cell.skew))
KO_8C.LINE.Data <- KO_8cell.skew[LINE.matched.line[!is.na(LINE.matched.line)], ]
#8C_wt
LINE.rep <- repeat.type.family[which(repeat.type.family[ ,2] == "LINE"), ]
LINE.matched.line <- match(rownames(LINE.rep), rownames(N8cell.skew))
N8cell.LINE.Data <- N8cell.skew[LINE.matched.line[!is.na(LINE.matched.line)], ]
#eB_KO
LINE.rep <- repeat.type.family[which(repeat.type.family[ ,2] == "LINE"), ]
LINE.matched.line <- match(rownames(LINE.rep), rownames(KO_eB.skew))
KO_eB.LINE.Data <- KO_eB.skew[LINE.matched.line[!is.na(LINE.matched.line)], ]
#eB_wt
LINE.rep <- repeat.type.family[which(repeat.type.family[ ,2] == "LINE"), ]
LINE.matched.line <- match(rownames(LINE.rep), rownames(earlyBlast.skew))
earlyBlast.LINE.Data <- earlyBlast.skew[LINE.matched.line[!is.na(LINE.matched.line)], ]

LINE.dat <- rbind(KO_8C.LINE.Data,N8cell.LINE.Data,KO_eB.LINE.Data,earlyBlast.LINE.Data)

#p-value at 8C, p=0.231
N8.LINE.dat <-  rbind(KO_8C.LINE.Data,N8cell.LINE.Data)
wilcox.test(skewing ~ sample, data=N8.LINE.dat)$p.value


#p-value at eB, p=0.0193
eB.LINE.dat <-  rbind(KO_eB.LINE.Data,earlyBlast.LINE.Data)
wilcox.test(skewing ~ sample, data=eB.LINE.dat)$p.value
LINE.dat$sample <- factor(LINE.dat$sample, levels=c("XistKO","wt"))


ggplot(LINE.dat, aes(x=stage, y=skewing, fill=sample)) + 
  geom_boxplot(alpha=0.6) + 
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 1), 
             aes(fill=sample), size=1.2, pch=21) +
  theme_bw() +
  scale_fill_manual(values=c("coral3", "cornflowerblue")) +
  theme(axis.text=element_text(size=12),
        axis.title.x=element_blank(),
        legend.text=element_text(size=12),
        legend.title=element_blank(),
        legend.key = element_blank(),
        #legend.text=element_text(size = rel(1)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.line.y = element_line(color="black"),
        axis.line.x = element_line(color="black"),
        legend.background = element_rect(fill=alpha('NA', 0.2)))






#---Figure 5g,h-------------------------------

rm(list=ls())
##### For Repeats analysis
#load package
library(gplots)
library(plyr)
library(pheatmap)
library(RColorBrewer)
library(ggplot2)
setwd("~/Dropbox (Partners HealthCare)/Pub/Data/combined.rep.analyses/")
#setwd("d:/Dropbox (Partners HealthCare)/R_analyses/Data/Raw/")
#Preload and process the original stat files for each stage
repeat.type.family <- read.table(file="repeat_class_family", sep="\t")
#Sort the repeats based on their classes
repeat.type.family <- repeat.type.family[order(repeat.type.family[ ,2]), ]
#Row 307, 308 and 313 were found duplicated names, although they belong to different repeat types.
#Use command << duplicated(repeat.type.family.sorted[[1]]) >> to look for duplicated lines.
repeat.type.family <- repeat.type.family[-c(307, 308, 313), ]
rownames(repeat.type.family) <- repeat.type.family$V1

age.Eutheria <- data.frame(LINE=c("HAL1","HAL1b","L1M2","L1M3","L1M4","L1MA4","L1M5","L1MA4A","L1M4","L1MA5","L1MA6","L1MA7","L1MA8","L1MA9","L1MB2","L1MB4","L1MB7","L1MB8","L1MC1","L1MC3","L1MC4","L1MC5","L1MD","L1MD1","L1MD2","L1MD3","L1MDa","L1ME1","L1ME2","L1ME3","L1ME3A","L1MEc","L1MEd","L1ME4b"), 
                           age=c(3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3),
                           origin=rep("Eutheria", 34))
age.Muridae <- data.frame(LINE=c("L1_Mur1","L1_Mur3","Lx2A","Lx2B","Lx3B","Lx3C","Lx3_Mus","L1_Mur2","L1_Rod","Lx","Lx2","Lx2B2","Lx3_Mus","Lx3A"),
                          age=c(4,4,4,4,4,4,4,4,4,4,4,4,4,4),
                          origin=rep("Muridae", 14))
age.M.mus <- data.frame(LINE=c("Lx3A","Lx4A","Lx5","Lx5c","Lx6","Lx7","Lx8","Lx8b","Lx9","Lx10","Lx4B","Lx5b"),
                        age=c(5,5,5,5,5,5,5,5,5,5,5,5),
                        origin=rep("M.mus", 12))
age.Theria <- c("HAL1M8",2,"Theria")
age.Mammalia <- c("L1MEg2", 1,"Mammalia")

origin <- rbind(age.Mammalia,age.Theria,age.Eutheria,age.Muridae,age.M.mus)

#For KO_late2cell
#clean up unsed variables
rm(list=setdiff(ls(), c("repeat.type.family", "origin")))
#run parseTbl.totalMatrix.sh rep_KO-late2ell_R_output.txt
All.Data <- read.table(file="KO_late2cell.allExperiments.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="KO_late2cell.allExperiments.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="KO_late2cell.allExperiments.repColNames.txt", sep="\t")
rownames(All.Data) <- All.rowname[ ,1]
colnames(All.Data) <- All.colname[ ,1]
All.Data <- t(All.Data)
matched.line <- match(rownames(repeat.type.family), rownames(All.Data))
All.Data <- All.Data[matched.line[!is.na(matched.line)], ]
#for future LINE use
LINE.rep <- repeat.type.family[which(repeat.type.family[ ,2] == "LINE"), ]
LINE.matched.line <- match(rownames(LINE.rep), rownames(All.Data))
KO_late2cell.LINE.Data <- All.Data[LINE.matched.line[!is.na(LINE.matched.line)], ]
KO_late2cell.LINE.chrX <- KO_late2cell.LINE.Data[ ,"chrX_99"]
KO_late2cell.LINE.chrX.mean <- data.frame(skew=KO_late2cell.LINE.chrX, LINE=rownames(KO_late2cell.LINE.Data))
KO_late2cell.LINE.chrX.mean <- KO_late2cell.LINE.chrX.mean[complete.cases(KO_late2cell.LINE.chrX.mean), ]
KO_late2cell.LINE.chrX.mean[ ,c(3,4)] <- origin[match(KO_late2cell.LINE.chrX.mean$LINE, origin$LINE), c(2,3)]
KO_late2cell.LINE.chrX.mean <- KO_late2cell.LINE.chrX.mean[complete.cases(KO_late2cell.LINE.chrX.mean), ]
rm(list=setdiff(ls(), c("repeat.type.family","origin", "KO_late2cell.LINE.chrX.mean")))



#For KO 8cell stage
All.Data <- read.table(file="KO_8cell.allExperiments.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="KO_8cell.allExperiments.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="KO_8cell.allExperiments.repColNames.txt", sep="\t")
rownames(All.Data) <- All.rowname[ ,1]
colnames(All.Data) <- All.colname[ ,1]
All.Data <- t(All.Data)
matched.line <- match(rownames(repeat.type.family), rownames(All.Data))
All.Data <- All.Data[matched.line[!is.na(matched.line)], ]
#for future LINE use
LINE.rep <- repeat.type.family[which(repeat.type.family[ ,2] == "LINE"), ]
LINE.matched.line <- match(rownames(LINE.rep), rownames(All.Data))
KO_8cell.LINE.Data <- All.Data[LINE.matched.line[!is.na(LINE.matched.line)], ]
KO_8cell.LINE.chrX <- KO_8cell.LINE.Data[ ,"chrX_0"]
KO_8cell.LINE.chrX.mean <- data.frame(skew=KO_8cell.LINE.chrX, LINE=rownames(KO_8cell.LINE.Data))
KO_8cell.LINE.chrX.mean <- KO_8cell.LINE.chrX.mean[complete.cases(KO_8cell.LINE.chrX.mean), ]
KO_8cell.LINE.chrX.mean[ ,c(3,4)] <- origin[match(KO_8cell.LINE.chrX.mean$LINE, origin$LINE), c(2,3)]
KO_8cell.LINE.chrX.mean <- KO_8cell.LINE.chrX.mean[complete.cases(KO_8cell.LINE.chrX.mean), ]
rm(list=setdiff(ls(), c("repeat.type.family","origin","KO_late2cell.LINE.chrX.mean","KO_8cell.LINE.chrX.mean")))



#For KO_earlyBlast
All.Data <- read.table(file="KO_eB.allExperiments.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="KO_eB.allExperiments.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="KO_eB.allExperiments.repColNames.txt", sep="\t")
rownames(All.Data) <- All.rowname[ ,1]
colnames(All.Data) <- All.colname[ ,1]
All.Data <- t(All.Data)
#All.Data <- All.Data[-374, ]
matched.line <- match(rownames(repeat.type.family), rownames(All.Data))
All.Data <- All.Data[matched.line[!is.na(matched.line)], ]
#for future LINE use
LINE.rep <- repeat.type.family[which(repeat.type.family[ ,2] == "LINE"), ]
LINE.matched.line <- match(rownames(LINE.rep), rownames(All.Data))
KO_eB.LINE.Data <- All.Data[LINE.matched.line[!is.na(LINE.matched.line)], ]
KO_eB.LINE.chrX <- KO_eB.LINE.Data[ ,"chrX_0"]
KO_eB.LINE.chrX.mean <- data.frame(skew=KO_eB.LINE.chrX, LINE=rownames(KO_eB.LINE.Data))
KO_eB.LINE.chrX.mean <- KO_eB.LINE.chrX.mean[complete.cases(KO_eB.LINE.chrX.mean), ]
KO_eB.LINE.chrX.mean[ ,c(3,4)] <- origin[match(KO_eB.LINE.chrX.mean$LINE, origin$LINE), c(2,3)]
KO_eB.LINE.chrX.mean <- KO_eB.LINE.chrX.mean[complete.cases(KO_eB.LINE.chrX.mean), ]
rm(list=setdiff(ls(), c("repeat.type.family","origin","KO_late2cell.LINE.chrX.mean","KO_4cell.LINE.chrX.mean","KO_8cell.LINE.chrX.mean","KO_eB.LINE.chrX.mean")))



#For late2cell
#clean up unsed variables
All.Data <- read.table(file="late2cell.allExperiments.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="late2cell.allExperiments.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="late2cell.allExperiments.repColNames.txt", sep="\t")
rownames(All.Data) <- All.rowname[ ,1]
colnames(All.Data) <- All.colname[ ,1]
All.Data <- t(All.Data)
matched.line <- match(rownames(repeat.type.family), rownames(All.Data))
All.Data <- All.Data[matched.line[!is.na(matched.line)], ]
#for future LINE use
LINE.rep <- repeat.type.family[which(repeat.type.family[ ,2] == "LINE"), ]
LINE.matched.line <- match(rownames(LINE.rep), rownames(All.Data))
late2cell.LINE.Data <- All.Data[LINE.matched.line[!is.na(LINE.matched.line)], ]
late2cell.LINE.chrX <- late2cell.LINE.Data[ ,"chrX_99"]
late2cell.LINE.chrX.mean <- data.frame(skew=late2cell.LINE.chrX, LINE=rownames(late2cell.LINE.Data))
late2cell.LINE.chrX.mean <- late2cell.LINE.chrX.mean[complete.cases(late2cell.LINE.chrX.mean), ]
late2cell.LINE.chrX.mean[ ,c(3,4)] <- origin[match(late2cell.LINE.chrX.mean$LINE, origin$LINE), c(2,3)]
late2cell.LINE.chrX.mean <- late2cell.LINE.chrX.mean[complete.cases(late2cell.LINE.chrX.mean), ]
rm(list=setdiff(ls(), c("repeat.type.family","origin",
                        "KO_late2cell.LINE.chrX.mean","KO_4cell.LINE.chrX.mean","KO_8cell.LINE.chrX.mean","KO_eB.LINE.chrX.mean",
                        "late2cell.LINE.chrX.mean")))



#For 8cell
All.Data <- read.table(file="8cell.allExperiments.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="8cell.allExperiments.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="8cell.allExperiments.repColNames.txt", sep="\t")
rownames(All.Data) <- All.rowname[ ,1]
colnames(All.Data) <- All.colname[ ,1]
All.Data <- t(All.Data)
matched.line <- match(rownames(repeat.type.family), rownames(All.Data))
All.Data <- All.Data[matched.line[!is.na(matched.line)], ]
#for future LINE use
LINE.rep <- repeat.type.family[which(repeat.type.family[ ,2] == "LINE"), ]
LINE.matched.line <- match(rownames(LINE.rep), rownames(All.Data))
N8cell.LINE.Data <- All.Data[LINE.matched.line[!is.na(LINE.matched.line)], ]
N8cell.LINE.chrX <- N8cell.LINE.Data[ ,"chrX_99"]
N8cell.LINE.chrX.mean <- data.frame(skew=N8cell.LINE.chrX, LINE=rownames(N8cell.LINE.Data))
N8cell.LINE.chrX.mean <- N8cell.LINE.chrX.mean[complete.cases(N8cell.LINE.chrX.mean), ]
N8cell.LINE.chrX.mean[ ,c(3,4)] <- origin[match(N8cell.LINE.chrX.mean$LINE, origin$LINE), c(2,3)]
N8cell.LINE.chrX.mean <- N8cell.LINE.chrX.mean[complete.cases(N8cell.LINE.chrX.mean), ]
rm(list=setdiff(ls(), c("repeat.type.family","origin",
                        "KO_late2cell.LINE.chrX.mean","KO_4cell.LINE.chrX.mean","KO_8cell.LINE.chrX.mean","KO_eB.LINE.chrX.mean",
                        "late2cell.LINE.chrX.mean","N4cell.LINE.chrX.mean","N8cell.LINE.chrX.mean")))



#For eB
All.Data <- read.table(file="earlyBlast.allExperiments.chrRepMatrix.txt", sep="\t")
All.rowname <- read.table(file="earlyBlast.allExperiments.chrRowNames.txt", sep="\t")
All.colname <- read.table(file="earlyBlast.allExperiments.repColNames.txt", sep="\t")
rownames(All.Data) <- All.rowname[ ,1]
colnames(All.Data) <- All.colname[ ,1]
All.Data <- t(All.Data)
matched.line <- match(rownames(repeat.type.family), rownames(All.Data))
All.Data <- All.Data[matched.line[!is.na(matched.line)], ]
#for future LINE use
LINE.rep <- repeat.type.family[which(repeat.type.family[ ,2] == "LINE"), ]
LINE.matched.line <- match(rownames(LINE.rep), rownames(All.Data))
eB.LINE.Data <- All.Data[LINE.matched.line[!is.na(LINE.matched.line)], ]
eB.LINE.chrX <- eB.LINE.Data[ ,"chrX_99"]
eB.LINE.chrX.mean <- data.frame(skew=eB.LINE.chrX, LINE=rownames(eB.LINE.Data))
eB.LINE.chrX.mean <- eB.LINE.chrX.mean[complete.cases(eB.LINE.chrX.mean), ]
eB.LINE.chrX.mean[ ,c(3,4)] <- origin[match(eB.LINE.chrX.mean$LINE, origin$LINE), c(2,3)]
eB.LINE.chrX.mean <- eB.LINE.chrX.mean[complete.cases(eB.LINE.chrX.mean), ]
rm(list=setdiff(ls(), c("repeat.type.family","origin",
                        "KO_late2cell.LINE.chrX.mean","KO_4cell.LINE.chrX.mean","KO_8cell.LINE.chrX.mean","KO_eB.LINE.chrX.mean",
                        "late2cell.LINE.chrX.mean","N4cell.LINE.chrX.mean","N8cell.LINE.chrX.mean","N16cell.LINE.chrX.mean","eB.LINE.chrX.mean")))


#verify
theme_pattern <- theme(axis.text=element_text(size=12),
                       axis.title=element_blank(),
                       legend.text=element_text(size=12),
                       legend.title=element_blank(), 
                       #axis.ticks.x = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       legend.position="top",
                       #axis.line = element_line(color = 'black'),
                       axis.line.y = element_line(color="black"),
                       axis.line.x = element_line(color="black"),
                       plot.margin=unit(c(0.2,0.5,0,0.5),"cm"))



#figure 6j
#check Eutheria LINEs
#KO
Eutheria.KO_late2cell <- data.frame(KO_late2cell.LINE.chrX.mean[which(KO_late2cell.LINE.chrX.mean$origin=="Eutheria"), c(1,2,4)], type="KO",stage="late2C")
Eutheria.KO_8cell <- data.frame(KO_8cell.LINE.chrX.mean[which(KO_8cell.LINE.chrX.mean$origin=="Eutheria"), c(1,2,4)], type="KO",stage="8C")
Eutheria.KO_eB <- data.frame(KO_eB.LINE.chrX.mean[which(KO_eB.LINE.chrX.mean$origin=="Eutheria"), c(1,2,4)], type="KO",stage="eB")
#WT
Eutheria.late2cell <- data.frame(late2cell.LINE.chrX.mean[which(late2cell.LINE.chrX.mean$origin=="Eutheria"), c(1,2,4)], type="WT",stage="late2C")
Eutheria.8cell <- data.frame(N8cell.LINE.chrX.mean[which(N8cell.LINE.chrX.mean$origin=="Eutheria"), c(1,2,4)], type="WT",stage="8C")
Eutheria.eB <- data.frame(eB.LINE.chrX.mean[which(eB.LINE.chrX.mean$origin=="Eutheria"), c(1,2,4)], type="WT",stage="eB")
#combine and plot
Eutheria.combine.late2cell <-rbind(Eutheria.KO_late2cell,Eutheria.late2cell)
Eu.L2C <- ggplot(Eutheria.combine.late2cell, aes(x=skew, fill=type)) + 
  geom_density(alpha=.5) +
  theme_bw() +
  theme_pattern
wilcox.test(skew ~ type, data=Eutheria.combine.late2cell)$p.value #p=0.2948995

Eutheria.combine.8cell <-rbind(Eutheria.KO_8cell,Eutheria.8cell)
Eu.8C <- ggplot(Eutheria.combine.8cell, aes(x=skew, fill=type)) + 
  geom_density(alpha=.5) +
  theme_bw() +
  theme_pattern
wilcox.test(skew ~ type, data=Eutheria.combine.8cell)$p.value #p=0.8371287

Eutheria.combine.eB <-rbind(Eutheria.KO_eB,Eutheria.eB)
Eu.eB <-ggplot(Eutheria.combine.eB, aes(x=skew, fill=type)) + 
  geom_density(alpha=.5) +
  theme_bw() +
  scale_x_continuous(limits = c(0,1),breaks = c(0,0.25,0.5,0.75,1)) +
  theme_pattern
wilcox.test(skew ~ type, data=Eutheria.combine.eB)$p.value #p=0.015

library(gridExtra)
grid.arrange(Eu.L2C,Eu.8C,Eu.eB, nrow=1, ncol=3)


#Figure 6i
#check M.mus LINEs
#KO
M.mus.KO_late2cell <- data.frame(KO_late2cell.LINE.chrX.mean[which(KO_late2cell.LINE.chrX.mean$origin=="M.mus"), c(1,2,4)], type="KO",stage="late2C")
M.mus.KO_8cell <- data.frame(KO_8cell.LINE.chrX.mean[which(KO_8cell.LINE.chrX.mean$origin=="M.mus"), c(1,2,4)], type="KO",stage="8C")
M.mus.KO_eB <- data.frame(KO_eB.LINE.chrX.mean[which(KO_eB.LINE.chrX.mean$origin=="M.mus"), c(1,2,4)], type="KO",stage="eB")
#WT
M.mus.late2cell <- data.frame(late2cell.LINE.chrX.mean[which(late2cell.LINE.chrX.mean$origin=="M.mus"), c(1,2,4)], type="WT",stage="late2C")
M.mus.8cell <- data.frame(N8cell.LINE.chrX.mean[which(N8cell.LINE.chrX.mean$origin=="M.mus"), c(1,2,4)], type="WT",stage="8C")
M.mus.earlyBlast <- data.frame(eB.LINE.chrX.mean[which(eB.LINE.chrX.mean$origin=="M.mus"), c(1,2,4)], type="WT",stage="eB")


#check young and old LINEs in WT
Eutheria_M.mus.late2cell <- rbind(Eutheria.late2cell, M.mus.late2cell)
Eu_M.late2C.WT <- ggplot(Eutheria_M.mus.late2cell, aes(x=skew, fill=origin)) + 
  geom_density(alpha=.5) +
  theme_bw() +
  theme_pattern
wilcox.test(skew ~ origin, data=Eutheria_M.mus.late2cell)$p.value #p=0.0755

Eutheria_M.mus.8cell <- rbind(Eutheria.8cell, M.mus.8cell)
Eu_M.8C.WT <- ggplot(Eutheria_M.mus.8cell, aes(x=skew, fill=origin)) + 
  geom_density(alpha=.5) +
  theme_bw() +
  scale_x_continuous(limits = c(0,1),breaks = c(0,0.25,0.5,0.75,1)) +
  theme_pattern
wilcox.test(skew ~ origin, data=Eutheria_M.mus.8cell)$p.value #p=0.4470699

Eutheria_M.mus.eB <- rbind(Eutheria.eB, M.mus.earlyBlast)
Eu_M.eB.WT <- ggplot(Eutheria_M.mus.eB, aes(x=skew, fill=origin)) + 
  geom_density(alpha=.5) +
  theme_bw() +
  xlim(0,1) +
  theme_pattern
wilcox.test(skew ~ origin, data=Eutheria_M.mus.eB)$p.value #p=0.264

grid.arrange(Eu_M.late2C.WT,Eu_M.8C.WT,Eu_M.eB.WT, nrow=1, ncol=3)
