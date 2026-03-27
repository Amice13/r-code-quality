#Fig.5
#Panel a. Total repeats reads, Skew analysis for chr1, chr13 and chrX during XCI.-------------------------

rm(list=ls())
library(ggplot2)
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


chr1_13_X.dat <- rbind(Day0.skew, Day1.skew, Day2.skew, Day3.skew, Day4.skew, Day6.skew, Day8.skew, Day10.skew)
chr1_13_X.dat$skewing <- 1 - chr1_13_X.dat$skewing
chr1_13_X.dat$stage <- factor(chr1_13_X.dat$stage, levels=c("Day0","Day1","Day2", "Day3","Day4","Day6","Day8","Day10"))
chr1_13_X.dat$chr <- factor(chr1_13_X.dat$chr, levels=c("chr1","chr13","chrX"))

#plot chr1, 13 and chrX
ggplot(chr1_13_X.dat, aes(x=stage, y=skewing, fill=chr)) +
  geom_boxplot(outlier.shape=NA, alpha=.5, lwd=0.4) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
             aes(fill=chr), size=0.7, pch=21,stroke = 0.4) +
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




#Day2 p-value, chrX vs chr13
Day2.Xvs13.dat <- Day2.skew[which(Day2.skew$chr != "chr1"), ]
wilcox.test(skewing ~ chr, data=Day2.Xvs13.dat)$p.value

#chrX p-value, Day0 vs Day1
Day0_1.skew <- rbind(Day0.skew, Day1.skew)
chrX.Day0vs1.dat <- Day0_1.skew[which(Day0_1.skew$chr == "chrX"), ]
wilcox.test(skewing ~ stage, data=chrX.Day0vs1.dat)$p.value

#chrX p-value, Day1 vs Day2
Day1_2.skew <- rbind(Day2.skew, Day1.skew)
chrX.Day1vs2.dat <- Day1_2.skew[which(Day1_2.skew$chr == "chrX"), ]
wilcox.test(skewing ~ stage, data=chrX.Day1vs2.dat)$p.value


#Panel b. silencing of 3 types of TEs during ES cell differentiation ----------------------------------
rm(list=ls())
source("ES_specific repeat analysis.R")
rm(list=setdiff(ls(), c("LINE.Day0", "LINE.Day1", "LINE.Day2","LINE.Day3","LINE.Day4","LINE.Day6","LINE.Day8","LINE.Day10",
                        "cLINE.Day0", "cLINE.Day1", "cLINE.Day2","cLINE.Day3","cLINE.Day4","cLINE.Day6","cLINE.Day8","cLINE.Day10",
                        "SINE.Day0", "SINE.Day1", "SINE.Day2","SINE.Day3","SINE.Day4","SINE.Day6","SINE.Day8","SINE.Day10",
                        "LTR.Day0", "LTR.Day1", "LTR.Day2","LTR.Day3","LTR.Day4","LTR.Day6","LTR.Day8","LTR.Day10")))


All.repeats.dat <- rbind(LINE.Day0, LINE.Day1, LINE.Day2,LINE.Day3,LINE.Day4,LINE.Day6,LINE.Day8,LINE.Day10,
                         SINE.Day0, SINE.Day1, SINE.Day2,SINE.Day3,SINE.Day4,SINE.Day6,SINE.Day8,SINE.Day10,
                         LTR.Day0, LTR.Day1, LTR.Day2,LTR.Day3,LTR.Day4,LTR.Day6,LTR.Day8,LTR.Day10)

All.repeats.dat$skew <- 1-All.repeats.dat$skew
All.repeats.dat$stage <- factor(All.repeats.dat$stage, levels=c("Day0", "Day1","Day2","Day3","Day4","Day6","Day8","Day10"))
All.repeats.dat$class <- factor(All.repeats.dat$class, levels=c("SINE", "LINE","LTR"))

ggplot(All.repeats.dat, aes(x=stage, y=skew, fill=class)) +
  geom_boxplot(outlier.shape=NA, lwd=0.4) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
             aes(fill=class), size=0.9, pch=21,stroke = 0.4) +
  scale_fill_manual(values=c("burlywood1","dodgerblue", "darkgray")) +
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


#SINE between Day0 and Day1 
SINE_0_1.dat <- rbind(SINE.Day0, SINE.Day1)
wilcox.test(skew ~ stage, data=SINE_0_1.dat)$p.value
#SINE between Day1 and Day2 
SINE_1_2.dat <- rbind(SINE.Day1, SINE.Day2)
wilcox.test(skew ~ stage, data=SINE_1_2.dat)$p.value

#LINE between Day0 and Day1 
LINE_0_1.dat <- rbind(LINE.Day0, LINE.Day1)
wilcox.test(skew ~ stage, data=LINE_0_1.dat)$p.value
#LINE between Day1 and Day2 
LINE_1_2.dat <- rbind(LINE.Day1, LINE.Day2)
wilcox.test(skew ~ stage, data=LINE_1_2.dat)$p.value

#LTR between Day0 and Day1
LTR_0_1.dat <- rbind(LTR.Day0, LTR.Day1)
wilcox.test(skew ~ stage, data=LTR_0_1.dat)$p.value
#LTR between Day1 and Day2
LTR_1_2.dat <- rbind(LTR.Day1, LTR.Day2)
wilcox.test(skew ~ stage, data=LTR_1_2.dat)$p.value



#Panel c. Heatmap of SINE silencing in differentiating ES cells. ------------------------------------
rm(list=ls())
source("ES_specific repeat analysis.R")
rm(list=setdiff(ls(), c("Day0.SINE.Data","Day1.SINE.Data","Day2.SINE.Data","Day3.SINE.Data",
                        "Day4.SINE.Data","Day6.SINE.Data","Day8.SINE.Data","Day10.SINE.Data")))

B2.SINE <- c("B2_Mm1a", "B2_Mm1t", "B2_Mm2", "B3", "B3A")
Alu.SINE <- c("B1F","B1F1","B1F2","B1_Mm","B1_Mur1","B1_Mur2","B1_Mur3","B1_Mur4","B1_Mus1",
              "B1_Mus2","PB1","PB1D10","PB1D11","PB1D7","PB1D9")
B4.SINE <- c("B4","B4A","ID_B1","RSINE1")
MIR.SINE <- c("MIR","MIR1_Amn","MIR3","MIRb","MIRc")
ID.SINE <- c("ID","ID2","ID4","ID4_")

SINE.Data <- cbind(Day0.SINE.Data,Day1.SINE.Data,Day2.SINE.Data,Day3.SINE.Data,
                   Day4.SINE.Data,Day6.SINE.Data,Day8.SINE.Data,Day10.SINE.Data)

SINE.names <- c(Alu.SINE, B2.SINE, B4.SINE, MIR.SINE, ID.SINE)

chrX.SINE.Data <- SINE.Data[,seq(from=20, to=160, by=20)]

chrX.SINE.Data <- chrX.SINE.Data[SINE.names, ]

#I need to maintain the same color scale with the heatmap in imprinted XCI, 
#The easiest way is to put an artificial "0" value in one position in the matrix, so that the color scale becames 0-1.
#Here i randomly chose MIR1_Amn, because it is NA in the data frame. change it back in the final figure. 
#remove MIR1_Amn from heatmap because many stages were missing.

chrX.SINE.Data["MIR1_Amn", 2] <- 0

pheatmap(chrX.SINE.Data, 
         cluster_rows = F, 
         cluster_cols = F,
         show_rownames = T, 
         show_colnames = T, 
         na_col = "grey",
         border_color="NA")



#Panel d. heatmap of genes overlapping with escapee SINEs--------------------------------------------


chrX.skew.dat <- read.table(file="processed/ES.Gene_Category.v2.txt",sep="\t")
overlapping.genes <- c("Clcn5","Ddx3x","Xiap","Flna","Eif2s3x",
                       "Igbp1","Morf4l2",
                       "Kdm6a","Utp14a","Mbtps2")

overlapping.gene.skew.dat <- chrX.skew.dat[overlapping.genes, ]
#add Jpx, it was filtered out because its total allelic reads did not met cutoff requirement.

Jpx <- data.frame(Day0=0.33333, Day1=0.44, Day2=0.5151515, Day3=0.6268657, Day4=0.6956522, Day6=0.6233766, Day8=0.7142857, Day10=0.6024096, 
                  category="Escapee",
                  row.names="Jpx")
dat <- rbind(overlapping.gene.skew.dat, Jpx)

#dat <- dat[order(dat$category), ]

pheatmap(dat[, -9], 
         cluster_cols = F,
         cluster_rows = F, 
         show_rownames = T, 
         show_colnames = T, 
         border_color=NA)




#Panel e. LINE silencing in differentiating ES cells. ---------------------------------

#load package
library(RColorBrewer)
library(ggplot2)
rm(list=ls())
source("MC.overall repeat analysis.R")

rm(list=setdiff(ls(), c("repeat.type.family",
                        "Day0.chrX","Day0.combined.Data","Day0.mean.skew",
                        "Day1.chrX","Day1.combined.Data","Day1.mean.skew",
                        "Day2.chrX","Day2.combined.Data","Day2.mean.skew",
                        "Day3.chrX","Day3.combined.Data","Day3.mean.skew",
                        "Day4.chrX","Day4.combined.Data","Day4.mean.skew",
                        "Day6.chrX","Day6.combined.Data","Day6.mean.skew",
                        "Day8.chrX","Day8.combined.Data","Day8.mean.skew",
                        "Day10.chrX","Day10.combined.Data","Day10.mean.skew")))



LINE.rep <- repeat.type.family[which(repeat.type.family[ ,2] == "LINE"), ]
#LINE-1 age
LINE1.age <- read.table(file="processed/LINE-1_age.txt")

#Due to limited number of reads for LINEs, we combined LINE reads from all replicates at each stage.

#For Day0
#cLINEs (by combing all replicates)
cLINE.matched.line <- match(rownames(LINE.rep), rownames(Day0.combined.Data))
Day0.cLINE.Data <- Day0.combined.Data[cLINE.matched.line[!is.na(cLINE.matched.line)], ]

#For Day1
#cLINEs (by combing all replicates)
cLINE.matched.line <- match(rownames(LINE.rep), rownames(Day1.combined.Data))
Day1.cLINE.Data <- Day1.combined.Data[cLINE.matched.line[!is.na(cLINE.matched.line)], ]

#For Day2
#cLINEs (by combing all replicates)
cLINE.matched.line <- match(rownames(LINE.rep), rownames(Day2.combined.Data))
Day2.cLINE.Data <- Day2.combined.Data[cLINE.matched.line[!is.na(cLINE.matched.line)], ]

#For Day3
#cLINEs (by combing all replicates)
cLINE.matched.line <- match(rownames(LINE.rep), rownames(Day3.combined.Data))
Day3.cLINE.Data <- Day3.combined.Data[cLINE.matched.line[!is.na(cLINE.matched.line)], ]

#For Day4
#cLINEs (by combing all replicates)
cLINE.matched.line <- match(rownames(LINE.rep), rownames(Day4.combined.Data))
Day4.cLINE.Data <- Day4.combined.Data[cLINE.matched.line[!is.na(cLINE.matched.line)], ]

#For Day6
#cLINEs (by combing all replicates)
cLINE.matched.line <- match(rownames(LINE.rep), rownames(Day6.combined.Data))
Day6.cLINE.Data <- Day6.combined.Data[cLINE.matched.line[!is.na(cLINE.matched.line)], ]

#For Day8
#cLINEs (by combing all replicates)
cLINE.matched.line <- match(rownames(LINE.rep), rownames(Day8.combined.Data))
Day8.cLINE.Data <- Day8.combined.Data[cLINE.matched.line[!is.na(cLINE.matched.line)], ]

#For Day10
#cLINEs (by combing all replicates)
cLINE.matched.line <- match(rownames(LINE.rep), rownames(Day10.combined.Data))
Day10.cLINE.Data <- Day10.combined.Data[cLINE.matched.line[!is.na(cLINE.matched.line)], ]


rm(list=setdiff(ls(), c("LINE1.age",
                        "Day0.LINE.Data","Day0.cLINE.Data",
                        "Day1.LINE.Data","Day1.cLINE.Data",
                        "Day2.LINE.Data","Day2.cLINE.Data",
                        "Day3.LINE.Data","Day3.cLINE.Data",
                        "Day4.LINE.Data","Day4.cLINE.Data",
                        "Day6.LINE.Data","Day6.cLINE.Data",
                        "Day8.LINE.Data","Day8.cLINE.Data",
                        "Day10.LINE.Data","Day10.cLINE.Data")))




#LINEs

#Day0
cLINE.Day0 <- data.frame(skew=Day0.cLINE.Data[ ,20], class="LINE",stage="Day0", age=LINE1.age$Group[match(rownames(Day0.cLINE.Data), rownames(LINE1.age))], row.names=rownames(Day0.cLINE.Data), name=rownames(Day0.cLINE.Data))
cLINE.Day0 <- cLINE.Day0[complete.cases(cLINE.Day0), ]

#Day1
cLINE.Day1 <- data.frame(skew=Day1.cLINE.Data[ ,20], class="LINE",stage="Day1", age=LINE1.age$Group[match(rownames(Day1.cLINE.Data), rownames(LINE1.age))], row.names=rownames(Day1.cLINE.Data), name=rownames(Day1.cLINE.Data))
cLINE.Day1 <- cLINE.Day1[complete.cases(cLINE.Day1), ]

#Day2
cLINE.Day2 <- data.frame(skew=Day2.cLINE.Data[ ,20], class="LINE",stage="Day2", age=LINE1.age$Group[match(rownames(Day2.cLINE.Data), rownames(LINE1.age))], row.names=rownames(Day2.cLINE.Data), name=rownames(Day2.cLINE.Data))
cLINE.Day2 <- cLINE.Day2[complete.cases(cLINE.Day2), ]

#Day3
cLINE.Day3 <- data.frame(skew=Day3.cLINE.Data[ ,20], class="LINE",stage="Day3", age=LINE1.age$Group[match(rownames(Day3.cLINE.Data), rownames(LINE1.age))], row.names=rownames(Day3.cLINE.Data), name=rownames(Day3.cLINE.Data))
cLINE.Day3 <- cLINE.Day3[complete.cases(cLINE.Day3), ]

#Day4
cLINE.Day4 <- data.frame(skew=Day4.cLINE.Data[ ,20], class="LINE",stage="Day4", age=LINE1.age$Group[match(rownames(Day4.cLINE.Data), rownames(LINE1.age))], row.names=rownames(Day4.cLINE.Data), name=rownames(Day4.cLINE.Data))
cLINE.Day4 <- cLINE.Day4[complete.cases(cLINE.Day4), ]

#Day6
cLINE.Day6 <- data.frame(skew=Day6.cLINE.Data[ ,20], class="LINE",stage="Day6", age=LINE1.age$Group[match(rownames(Day6.cLINE.Data), rownames(LINE1.age))], row.names=rownames(Day6.cLINE.Data), name=rownames(Day6.cLINE.Data))
cLINE.Day6 <- cLINE.Day6[complete.cases(cLINE.Day6), ]

#Day8
cLINE.Day8 <- data.frame(skew=Day8.cLINE.Data[ ,20], class="LINE",stage="Day8", age=LINE1.age$Group[match(rownames(Day8.cLINE.Data), rownames(LINE1.age))], row.names=rownames(Day8.cLINE.Data), name=rownames(Day8.cLINE.Data))
cLINE.Day8 <- cLINE.Day8[complete.cases(cLINE.Day8), ]

#Day10
cLINE.Day10 <- data.frame(skew=Day10.cLINE.Data[ ,20], class="LINE",stage="Day10", age=LINE1.age$Group[match(rownames(Day10.cLINE.Data), rownames(LINE1.age))], row.names=rownames(Day10.cLINE.Data), name=rownames(Day10.cLINE.Data))
cLINE.Day10 <- cLINE.Day10[complete.cases(cLINE.Day10), ]


rm(list=setdiff(ls(), c("LINE1.age",
                        "cLINE.Day0","LINE.Day0",
                        "cLINE.Day1","LINE.Day1",
                        "cLINE.Day2","LINE.Day2",
                        "cLINE.Day3","LINE.Day3",
                        "cLINE.Day4","LINE.Day4",
                        "cLINE.Day6","LINE.Day6",
                        "cLINE.Day8","LINE.Day8",
                        "cLINE.Day10","LINE.Day10")))

cLINE.dat <- rbind(cLINE.Day0, cLINE.Day1, cLINE.Day2, cLINE.Day3,cLINE.Day4,cLINE.Day6,cLINE.Day8,cLINE.Day10)

#only forcus on the LINE families that occur in Mammals (L1M), murians (Lx), and house mouse (L1Md).
#remove L1M5 due to unknown history of this LINE subfamilies.Not recorded in the literatures I have read.
cLINE.dat <- cLINE.dat[which(cLINE.dat$name != "L1M5" & cLINE.dat$age != "Ancient"), ]
cLINE.dat$stage <- factor(cLINE.dat$stage, levels=c("Day0","Day1","Day2","Day3","Day4","Day6","Day8","Day10"))
cLINE.dat$age <- factor(cLINE.dat$age, levels=c("Old","Mid","young"))

#display Xi (Mus) fraction
cLINE.dat$skew <- 1-cLINE.dat$skew


#stage vs skew
ggplot(cLINE.dat, aes(x=stage, y=skew, fill=age)) + 
  geom_boxplot(outlier.shape = NA, alpha=0.9) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.75), 
             aes(fill=age), size=1.3, pch=21) +
  scale_fill_manual(values=brewer.pal(n = 8, name = "Accent")) +
  theme_bw() +
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

#p-value

#Day 1
#old vs young
Day1.dat <- cLINE.dat[which(cLINE.dat$stage =="Day1"), ]
Day1.dat.Old_young <- Day1.dat[which(Day1.dat$age =="Old" | Day1.dat$age =="young"), ]
wilcox.test(skew ~ age, data=Day1.dat.Old_young)$p.value 
#mid vs young
Day1.dat <- cLINE.dat[which(cLINE.dat$stage =="Day1"), ]
Day1.dat.Mid_young <- Day1.dat[which(Day1.dat$age =="Mid" | Day1.dat$age =="young"), ]
wilcox.test(skew ~ age, data=Day1.dat.Mid_young)$p.value 

#Day 2, 
#old vs young
Day2.dat <- cLINE.dat[which(cLINE.dat$stage =="Day2"), ]
Day2.dat.Old_young <- Day2.dat[which(Day2.dat$age =="Old" | Day2.dat$age =="young"), ]
wilcox.test(skew ~ age, data=Day2.dat.Old_young)$p.value 
#mid vs young
Day2.dat <- cLINE.dat[which(cLINE.dat$stage =="Day2"), ]
Day2.dat.Mid_young <- Day2.dat[which(Day2.dat$age =="Mid" | Day2.dat$age =="young"), ]
wilcox.test(skew ~ age, data=Day2.dat.Mid_young)$p.value 

#compare Day1 vs Day2 for young LINEs
Day1_2.dat <- rbind(Day1.dat, Day2.dat)
Day1_2_young <- Day1_2.dat[which(Day1_2.dat$age == "young"), ]
wilcox.test(skew ~ stage, data=Day1_2_young)$p.value



#Day3, 
#old vs young
Day3.dat <- cLINE.dat[which(cLINE.dat$stage =="Day3"), ]
Day3.dat.Old_young <- Day3.dat[which(Day3.dat$age =="Old" | Day3.dat$age =="young"), ]
wilcox.test(skew ~ age, data=Day3.dat.Old_young)$p.value
#mid vs young
Day3.dat <- cLINE.dat[which(cLINE.dat$stage =="Day3"), ]
Day3.dat.Mid_young <- Day3.dat[which(Day3.dat$age =="Mid" | Day3.dat$age =="young"), ]
wilcox.test(skew ~ age, data=Day3.dat.Mid_young)$p.value 

#Day4, 
#old vs young
Day4.dat <- cLINE.dat[which(cLINE.dat$stage =="Day4"), ]
Day4.dat.Old_young <- Day4.dat[which(Day4.dat$age =="Old" | Day4.dat$age =="young"), ]
wilcox.test(skew ~ age, data=Day4.dat.Old_young)$p.value
#mid vs young
Day4.dat <- cLINE.dat[which(cLINE.dat$stage =="Day4"), ]
Day4.dat.Mid_young <- Day4.dat[which(Day4.dat$age =="Mid" | Day4.dat$age =="young"), ]
wilcox.test(skew ~ age, data=Day4.dat.Mid_young)$p.value 

#Day10, 
#old vs young
Day10.dat <- cLINE.dat[which(cLINE.dat$stage =="Day10"), ]
Day10.dat.Old_young <- Day10.dat[which(Day10.dat$age =="Old" | Day10.dat$age =="young"), ]
wilcox.test(skew ~ age, data=Day10.dat.Old_young)$p.value 
#mid vs young
Day10.dat <- cLINE.dat[which(cLINE.dat$stage =="Day10"), ]
Day10.dat.Mid_young <- Day10.dat[which(Day10.dat$age =="Mid" | Day10.dat$age =="young"), ]
wilcox.test(skew ~ age, data=Day10.dat.Mid_young)$p.value 



#Panel f. Heatmap of LTR silencing--------------------------
#LTR skew based on subfamily

rm(list=ls())
source("ES_specific repeat analysis.R")
rm(list=setdiff(ls(), c("LTR.Day0", "LTR.Day1", "LTR.Day2","LTR.Day3","LTR.Day4","LTR.Day6",
                        "LTR.Day8","LTR.Day10")))
LTR_subfamily <- read.table(file="processed/LTR_subfamily",sep="\t")
colnames(LTR_subfamily) <- c("rep","subfamily")
table(LTR_subfamily$subfamily)

LTR.Day0[ ,"rep"] <- rownames(LTR.Day0)
LTR.Day0[ ,"subfamily"] <- LTR_subfamily$subfamily[match(LTR.Day0$rep, LTR_subfamily$rep)]

LTR.Day1[ ,"rep"] <- rownames(LTR.Day1)
LTR.Day1[ ,"subfamily"] <- LTR_subfamily$subfamily[match(LTR.Day1$rep, LTR_subfamily$rep)]

LTR.Day2[ ,"rep"] <- rownames(LTR.Day2)
LTR.Day2[ ,"subfamily"] <- LTR_subfamily$subfamily[match(LTR.Day2$rep, LTR_subfamily$rep)]

LTR.Day3[ ,"rep"] <- rownames(LTR.Day3)
LTR.Day3[ ,"subfamily"] <- LTR_subfamily$subfamily[match(LTR.Day3$rep, LTR_subfamily$rep)]

LTR.Day4[ ,"rep"] <- rownames(LTR.Day4)
LTR.Day4[ ,"subfamily"] <- LTR_subfamily$subfamily[match(LTR.Day4$rep, LTR_subfamily$rep)]

LTR.Day6[ ,"rep"] <- rownames(LTR.Day6)
LTR.Day6[ ,"subfamily"] <- LTR_subfamily$subfamily[match(LTR.Day6$rep, LTR_subfamily$rep)]

LTR.Day8[ ,"rep"] <- rownames(LTR.Day8)
LTR.Day8[ ,"subfamily"] <- LTR_subfamily$subfamily[match(LTR.Day8$rep, LTR_subfamily$rep)]

LTR.Day10[ ,"rep"] <- rownames(LTR.Day10)
LTR.Day10[ ,"subfamily"] <- LTR_subfamily$subfamily[match(LTR.Day10$rep, LTR_subfamily$rep)]


# heatmap to represent the skew.

LTR.Day0_1 <- merge(LTR.Day0, LTR.Day1, by.x="rep", by.y="rep", all=T)[ ,c(1,2,6)]
colnames(LTR.Day0_1) <- c("rep","Day0","Day1")
LTR.Day0_2 <- merge(LTR.Day0_1, LTR.Day2, by.x="rep", by.y="rep", all=T )[ ,c(1:4)]
colnames(LTR.Day0_2) <- c("rep","Day0","Day1","Day2")
LTR.Day0_3 <- merge(LTR.Day0_2, LTR.Day3, by.x="rep", by.y="rep", all=T )[ ,c(1:5)]
colnames(LTR.Day0_3) <- c("rep","Day0","Day1","Day2","Day3")
LTR.Day0_4 <- merge(LTR.Day0_3, LTR.Day4, by.x="rep", by.y="rep", all=T)[ ,c(1:6)]
colnames(LTR.Day0_4) <- c("rep","Day0","Day1","Day2","Day3","Day4")
LTR.Day0_6 <- merge(LTR.Day0_4, LTR.Day6, by.x="rep", by.y="rep", all=T)[ ,c(1:7)]
colnames(LTR.Day0_6) <- c("rep","Day0","Day1","Day2","Day3","Day4","Day6")
LTR.Day0_8 <- merge(LTR.Day0_6, LTR.Day8, by.x="rep", by.y="rep", all=T)[ ,c(1:8)]
colnames(LTR.Day0_8) <- c("rep","Day0","Day1","Day2","Day3","Day4","Day6","Day8")
LTR.Day0_10 <- merge(LTR.Day0_8, LTR.Day10, by.x="rep", by.y="rep", all=T)[ ,c(1:9)]
colnames(LTR.Day0_10) <- c("rep","Day0","Day1","Day2","Day3","Day4","Day6","Day8","Day10")

LTR.Day0_10.v2 <- LTR.Day0_10[which(rowSums(LTR.Day0_10 == "NaN") < 2),]

rownames(LTR.Day0_10.v2) <- LTR.Day0_10.v2[,1]
LTR.Day0_10.v2[ ,"family"] <- LTR_subfamily[match(LTR.Day0_10.v2[,1],LTR_subfamily$rep), "subfamily"]
LTR.Day0_10.v2 <- LTR.Day0_10.v2[order(LTR.Day0_10.v2$family), c(-1,-10)]

library(pheatmap)

pheatmap(LTR.Day0_10.v2,
         cluster_cols = F,
         cluster_rows = F, 
         show_rownames = T, 
         show_colnames = T, 
         border_color=NA,
         na_col = "grey")
