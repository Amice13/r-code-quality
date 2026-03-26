#load package
#library(gplots)
library(plyr)
library(RColorBrewer)
library(ggplot2)
rm(list=ls())


#Panel a. dyanmic silencing of LINEs at different age in MC embryos-----------------------
source("MC.overall repeat analysis.R")

rm(list=setdiff(ls(), c("repeat.type.family","N4cell.chrX","N4cell.combined.Data","N4cell.mean.skew",
                        "N8cell.chrX","N8cell.combined.Data","N8cell.mean.skew",
                        "N16cell.chrX","N16cell.combined.Data","N16cell.mean.skew",
                        "earlyBlast.chrX","earlyBlast.combined.Data","earlyBlast.mean.skew")))
LINE.rep <- repeat.type.family[which(repeat.type.family[ ,2] == "LINE"), ]
#LINE-1 age
LINE1.age <- read.table(file="processed/LINE-1_age.txt")



#For 4cell stage
#cLINEs (by combing all replicates)
cLINE.matched.line <- match(rownames(LINE.rep), rownames(N4cell.combined.Data))
N4cell.cLINE.Data <- N4cell.combined.Data[cLINE.matched.line[!is.na(cLINE.matched.line)], ]

#For 8cell stage
#cLINEs (by combing all replicates)
cLINE.matched.line <- match(rownames(LINE.rep), rownames(N8cell.combined.Data))
N8cell.cLINE.Data <- N8cell.combined.Data[cLINE.matched.line[!is.na(cLINE.matched.line)], ]

#For 16cell stage
#cLINEs (by combing all replicates)
cLINE.matched.line <- match(rownames(LINE.rep), rownames(N16cell.combined.Data))
N16cell.cLINE.Data <- N16cell.combined.Data[cLINE.matched.line[!is.na(cLINE.matched.line)], ]


#For earlyBlast stage
#cLINEs (by combing all replicates)
cLINE.matched.line <- match(rownames(LINE.rep), rownames(earlyBlast.combined.Data))
earlyBlast.cLINE.Data <- earlyBlast.combined.Data[cLINE.matched.line[!is.na(cLINE.matched.line)], ]

rm(list=setdiff(ls(), c("LINE1.age",
                        "N4cell.LINE.Data","N4cell.cLINE.Data",
                        "N8cell.LINE.Data","N8cell.cLINE.Data",
                        "N16cell.LINE.Data","N16cell.cLINE.Data",
                        "earlyBlast.LINE.Data","earlyBlast.cLINE.Data")))



#LINEs

#4C
cLINE.N4C <- data.frame(skew=N4cell.cLINE.Data[ ,20], class="LINE",stage="4C", age=LINE1.age$Group[match(rownames(N4cell.cLINE.Data), rownames(LINE1.age))], row.names=rownames(N4cell.cLINE.Data), name=rownames(N4cell.cLINE.Data))
cLINE.N4C <- cLINE.N4C[complete.cases(cLINE.N4C), ]

#8C
cLINE.N8C <- data.frame(skew=N8cell.cLINE.Data[ ,20], class="LINE",stage="8C", age=LINE1.age$Group[match(rownames(N8cell.cLINE.Data), rownames(LINE1.age))], row.names=rownames(N8cell.cLINE.Data), name=rownames(N8cell.cLINE.Data))
cLINE.N8C <- cLINE.N8C[complete.cases(cLINE.N8C), ]

#16C
cLINE.N16C <- data.frame(skew=N16cell.cLINE.Data[ ,20], class="LINE",stage="16C", age=LINE1.age$Group[match(rownames(N16cell.cLINE.Data), rownames(LINE1.age))], row.names=rownames(N16cell.cLINE.Data), name=rownames(N16cell.cLINE.Data))
cLINE.N16C <- cLINE.N16C[complete.cases(cLINE.N16C), ]



#eB
cLINE.eB <- data.frame(skew=earlyBlast.cLINE.Data[ ,20], class="LINE", stage="earlyB.",age=LINE1.age$Group[match(rownames(earlyBlast.cLINE.Data), rownames(LINE1.age))], row.names=rownames(earlyBlast.cLINE.Data), name=rownames(earlyBlast.cLINE.Data))
cLINE.eB <- cLINE.eB[complete.cases(cLINE.eB), ]

rm(list=setdiff(ls(), c("LINE1.age",
                        "cLINE.N4C","LINE.N4C",
                        "cLINE.N8C","LINE.N8C",
                        "cLINE.N16C","LINE.N16C",
                        "cLINE.eB","LINE.eB")))

cLINE.dat <- rbind(cLINE.N4C, cLINE.N8C, cLINE.N16C, cLINE.eB)

#only forcus on the LINE families that occur in Mammals (L1M), murians (Lx), and house mouse (L1Md).
#remove L1M5 due to unknown history of this LINE subfamilies.Not recorded in the literatures I have read.
cLINE.dat <- cLINE.dat[which(cLINE.dat$name != "L1M5" & cLINE.dat$age != "Ancient"), ]

cLINE.dat$stage <- factor(cLINE.dat$stage, levels=c("4C","8C","16C","earlyB."))
cLINE.dat$age <- factor(cLINE.dat$age, levels=c("Old","Mid","young"))



ggplot(cLINE.dat, aes(x=age, y=skew, fill=stage)) + 
  geom_boxplot(outlier.shape = NA, alpha=0.9) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.75), 
             aes(fill=stage), size=1.5, pch=21) +
  scale_fill_manual(values=brewer.pal(n = 5, name = "Accent")) +
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

old.dat <- cLINE.dat[which(cLINE.dat$age == "Old"), ]
old.dat.4_8C <- old.dat[which(old.dat$stage=="4C" | old.dat$stage=="8C"), ]
wilcox.test(skew ~ stage, data=old.dat.4_8C)$p.value # p=0.05054641

mid.dat <- cLINE.dat[which(cLINE.dat$age == "Mid"), ]
mid.dat.4C_eB <- mid.dat[which(mid.dat$stage=="4C" | mid.dat$stage=="earlyB."), ]
wilcox.test(skew ~ stage, data=mid.dat.4C_eB)$p.value # p=0.196





#Panel b. size distribution of LINEs on Xp at different age in MC embryos-------------------------------
rm(list=ls())
#load the counts and coordinates of LINEs that the allelic reads mapped to
late2C.LINE.ref.sum <- read.table(file="processed/BED/chrX.late2cell_LINE.REF.sum.bed", sep="\t")
late2C.LINE.cast.sum <- read.table(file="processed/BED/chrX.late2cell_LINE.CAST.sum.bed", sep="\t")
N4C.LINE.ref.sum <- read.table(file="processed/BED/chrX.4cell_LINE.REF.sum.bed", sep="\t")
N4C.LINE.cast.sum <- read.table(file="processed/BED/chrX.4cell_LINE.CAST.sum.bed", sep="\t")
N8C.LINE.ref.sum <- read.table(file="processed/BED/chrX.8cell_LINE.REF.sum.bed", sep="\t")
N8C.LINE.cast.sum <- read.table(file="processed/BED/chrX.8cell_LINE.CAST.sum.bed", sep="\t")
N16C.LINE.ref.sum <- read.table(file="processed/BED/chrX.16cell_LINE.REF.sum.bed", sep="\t")
N16C.LINE.cast.sum <- read.table(file="processed/BED/chrX.16cell_LINE.CAST.sum.bed", sep="\t")
eB.LINE.ref.sum <- read.table(file="processed/BED/chrX.earlyBlast_LINE.REF.sum.bed", sep="\t")
eB.LINE.cast.sum <- read.table(file="processed/BED/chrX.earlyBlast_LINE.CAST.sum.bed", sep="\t")

colnames(late2C.LINE.cast.sum) <- c("chr","start","end","rep","count","strand","family","subfamily","type")
colnames(N4C.LINE.cast.sum) <- c("chr","start","end","rep","count","strand","family","subfamily","type")
colnames(N8C.LINE.cast.sum) <- c("chr","start","end","rep","count","strand","family","subfamily","type")
colnames(N16C.LINE.cast.sum) <- c("chr","start","end","rep","count","strand","family","subfamily","type")
colnames(eB.LINE.cast.sum) <- c("chr","start","end","rep","count","strand","family","subfamily","type")
colnames(late2C.LINE.ref.sum) <- c("chr","start","end","rep","count","strand","family","subfamily","type")
colnames(N4C.LINE.ref.sum) <- c("chr","start","end","rep","count","strand","family","subfamily","type")
colnames(N8C.LINE.ref.sum) <- c("chr","start","end","rep","count","strand","family","subfamily","type")
colnames(N16C.LINE.ref.sum) <- c("chr","start","end","rep","count","strand","family","subfamily","type")
colnames(eB.LINE.ref.sum) <- c("chr","start","end","rep","count","strand","family","subfamily","type")

late2C.LINE.ref.sum[ ,"size"] <- abs(late2C.LINE.ref.sum$end-late2C.LINE.ref.sum$start)
late2C.LINE.cast.sum[ ,"size"] <- abs(late2C.LINE.cast.sum$end-late2C.LINE.cast.sum$start)
N4C.LINE.ref.sum[ ,"size"] <- abs(N4C.LINE.ref.sum$end-N4C.LINE.ref.sum$start)
N4C.LINE.cast.sum[ ,"size"] <- abs(N4C.LINE.cast.sum$end-N4C.LINE.cast.sum$start)
N8C.LINE.ref.sum[ ,"size"] <- abs(N8C.LINE.ref.sum$end-N8C.LINE.ref.sum$start)
N8C.LINE.cast.sum[ ,"size"] <- abs(N8C.LINE.cast.sum$end-N8C.LINE.cast.sum$start)
N16C.LINE.ref.sum[ ,"size"] <- abs(N16C.LINE.ref.sum$end-N16C.LINE.ref.sum$start)
N16C.LINE.cast.sum[ ,"size"] <- abs(N16C.LINE.cast.sum$end-N16C.LINE.cast.sum$start)
eB.LINE.ref.sum[ ,"size"] <- abs(eB.LINE.ref.sum$end-eB.LINE.ref.sum$start)
eB.LINE.cast.sum[ ,"size"] <- abs(eB.LINE.cast.sum$end-eB.LINE.cast.sum$start)

#LINE-1 age
LINE1.age <- read.table(file="processed/LINE-1_age.txt")

#cas (Xp allele)
cast.LINE.size <- rbind(data.frame(late2C.LINE.cast.sum[ ,c(4,5,10)], stage="late2C"),
                        data.frame(N4C.LINE.cast.sum[ ,c(4,5,10)], stage="4C"),
                        data.frame(N8C.LINE.cast.sum[ ,c(4,5,10)], stage="8C"),
                        data.frame(N16C.LINE.cast.sum[ ,c(4,5,10)], stage="16C"),
                        data.frame(eB.LINE.cast.sum[ ,c(4,5,10)], stage="earlyBlast"))

cast.LINE.size[ ,"age"] <- LINE1.age$Group[match(cast.LINE.size$rep, rownames(LINE1.age))]
cast.LINE.size <- cast.LINE.size[complete.cases(cast.LINE.size), ]

#focus on earlyBlast stage
cast.LINE.size.eB <- cast.LINE.size[which(cast.LINE.size$stage=="earlyBlast"), ]
ggplot(cast.LINE.size.eB, aes(x=size, fill=age)) +
  geom_density(alpha=0.5) +
  theme_bw() +
  scale_x_continuous(limits = c(0,6500), expand = c(0,0), breaks = c(0,1000,2000,3000,4000,5000,6000)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.title.x=element_text(),
        #axis.text.x=element_blank(),
        #legend.text=element_blank(),
        legend.title=element_blank(),
        legend.key = element_blank(),
        #legend.text=element_text(size = rel(1)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_line(color="black"),
        axis.line.x = element_line(color="black"),
        legend.background = element_rect(fill=alpha('NA', 0.2)))

