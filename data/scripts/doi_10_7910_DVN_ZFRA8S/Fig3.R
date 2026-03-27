#Panel a. Repeat skew dynamics in CM embryos-------------------------------------------

rm(list=ls())
source("CM.specific repeat analysis.R")
rm(list=setdiff(ls(), c("LINE.l2C", "LINE.N4C","LINE.N8C","LINE.N16C","LINE.eB",
                        "cLINE.l2C", "cLINE.N4C","cLINE.N8C","cLINE.N16C","cLINE.eB",
                        "SINE.l2C", "SINE.N4C","SINE.N8C","SINE.N16C","SINE.eB",
                        "LTR.l2C", "LTR.N4C","LTR.N8C","LTR.N16C","LTR.eB")))


All.repeats.dat <- rbind(LINE.l2C, LINE.N4C, LINE.N8C, LINE.N16C, LINE.eB,
                         SINE.l2C, SINE.N4C, SINE.N8C, SINE.N16C, SINE.eB,
                         LTR.l2C, LTR.N4C,LTR.N8C,LTR.N16C,LTR.eB)

All.repeats.dat$stage <- factor(All.repeats.dat$stage, levels=c("late2C","4C","8C","16C","earlyB."))
All.repeats.dat$class <- factor(All.repeats.dat$class, levels=c("SINE", "LINE","LTR"))
#Xp in this cross is ref.
All.repeats.dat$skew <- 1-All.repeats.dat$skew

ggplot(All.repeats.dat, aes(x=stage, y=skew, fill=class)) +
  geom_boxplot(outlier.shape=NA, lwd=0.4) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
             aes(fill=class), size=1.1, pch=21,stroke = 0.4) +
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



#Panel b. SINE in MC vs CM -----------------------------------------------------

rm(list=ls())
source("MC.specific repeat analysis.R")
All.MC.repeats.dat <- data.frame(rbind(LINE.l2C, LINE.N4C, LINE.N8C, LINE.N16C, LINE.eB,
                                       SINE.l2C, SINE.N4C, SINE.N8C, SINE.N16C, SINE.eB,
                                       LTR.l2C, LTR.N4C,LTR.N8C,LTR.N16C, LTR.eB), type="MC")
#save a intermediate file, and delete later
write.table(All.MC.repeats.dat, file="tmp_file.txt", sep="\t", quote=F)

source("CM.specific repeat analysis.R")
All.CM.repeats.dat <- data.frame(rbind(LINE.l2C, LINE.N4C, LINE.N8C, LINE.N16C, LINE.eB,
                                       SINE.l2C, SINE.N4C, SINE.N8C, SINE.N16C, SINE.eB,
                                       LTR.l2C, LTR.N4C,LTR.N8C, LTR.N16C, LTR.eB), type="CM")
rm(list=setdiff(ls(), "All.CM.repeats.dat"))
All.CM.repeats.dat$skew <- 1-All.CM.repeats.dat$skew

All.MC.repeats.dat <- read.table(file="tmp_file.txt", sep="\t")
file.remove("tmp_file.txt")

All.repeats.dat <- rbind(All.MC.repeats.dat, All.CM.repeats.dat)
All.repeats.dat$stage <- factor(All.repeats.dat$stage, levels=c("late2C","4C","8C","16C","earlyB."))
All.repeats.dat$type <- factor(All.repeats.dat$type, levels=c("MC","CM"))

#compare SINEs between CM and MC

All.SINE.dat <- All.repeats.dat[which(All.repeats.dat$class=="SINE"), ]


ggplot(All.SINE.dat, aes(x=stage, y=skew, fill=type)) +
  geom_boxplot(outlier.shape=NA, lwd=0.4) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
             aes(fill=type), size=1.1, pch=21,stroke = 0.4) +
  scale_fill_manual(values=c("lightblue1","dodgerblue")) +
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
#p-value
#at 8C, 
dat <- All.SINE.dat[which(All.SINE.dat$stage=="8C"), ]
wilcox.test(skew ~ type, data=dat)$p.value 

#at 16C, 
dat <- All.SINE.dat[which(All.SINE.dat$stage=="16C"), ]
wilcox.test(skew ~ type, data=dat)$p.value 

#at eB, 
dat <- All.SINE.dat[which(All.SINE.dat$stage=="earlyB."), ]
wilcox.test(skew ~ type, data=dat)$p.value 




#Panel c.skew analysis of different repeat class in KO embryos-----------------------------------------

rm(list=ls())
source("KO.specific repeat analysis.R")
rm(list=setdiff(ls(), c("LINE.KO_l2C", "LINE.KO_4C","LINE.KO_8C","LINE.KO_eB",
                        "cLINE.KO_l2C", "cLINE.KO_4C","cLINE.KO_8C","cLINE.KO_eB",
                        "SINE.KO_l2C", "SINE.KO_4C","SINE.KO_8C","SINE.KO_eB",
                        "LTR.KO_l2C", "LTR.KO_4C","LTR.KO_8C","LTR.KO_eB")))


All.repeats.dat <- rbind(LINE.KO_l2C, LINE.KO_4C, LINE.KO_8C, LINE.KO_eB,
                         SINE.KO_l2C, SINE.KO_4C, SINE.KO_8C, SINE.KO_eB,
                         LTR.KO_l2C, LTR.KO_4C,LTR.KO_8C,LTR.KO_eB)

All.repeats.dat$stage <- factor(All.repeats.dat$stage, levels=c("late2C","4C","8C","earlyB."))
All.repeats.dat$class <- factor(All.repeats.dat$class, levels=c("SINE", "LINE","LTR"))
#Xp in this cross is ref.
All.repeats.dat$skew <- 1-All.repeats.dat$skew

ggplot(All.repeats.dat, aes(x=stage, y=skew, fill=class)) +
  geom_boxplot(outlier.shape=NA, lwd=0.4) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
             aes(fill=class), size=1.1, pch=21,stroke = 0.4) +
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





#Panel d. check SINE skew vs genomic locus--------------------------------
rm(list=ls())
library(RColorBrewer)
library(ggplot2)
library(gridExtra)


late2C.SINE.ref.sum <- read.table(file="processed/chrX.CM_late2cell_SINE.REF.sum", sep="\t")
late2C.SINE.cast.sum <- read.table(file="processed/chrX.CM_late2cell_SINE.CAST.sum", sep="\t")
N4C.SINE.ref.sum <- read.table(file="processed/chrX.CM_4cell_SINE.REF.sum", sep="\t")
N4C.SINE.cast.sum <- read.table(file="processed/chrX.CM_4cell_SINE.CAST.sum", sep="\t")
N8C.SINE.ref.sum <- read.table(file="processed/chrX.CM_8cell_SINE.REF.sum", sep="\t")
N8C.SINE.cast.sum <- read.table(file="processed/chrX.CM_8cell_SINE.CAST.sum", sep="\t")
N16C.SINE.ref.sum <- read.table(file="processed/chrX.CM_16cell_SINE.REF.sum", sep="\t")
N16C.SINE.cast.sum <- read.table(file="processed/chrX.CM_16cell_SINE.CAST.sum", sep="\t")
eB.SINE.ref.sum <- read.table(file="processed/chrX.CM_earlyBlast_SINE.REF.sum", sep="\t")
eB.SINE.cast.sum <- read.table(file="processed/chrX.CM_earlyBlast_SINE.CAST.sum", sep="\t")

colnames(late2C.SINE.cast.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")
colnames(N4C.SINE.cast.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")
colnames(N8C.SINE.cast.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")
colnames(N16C.SINE.cast.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")
colnames(eB.SINE.cast.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")
colnames(late2C.SINE.ref.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")
colnames(N4C.SINE.ref.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")
colnames(N8C.SINE.ref.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")
colnames(N16C.SINE.ref.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")
colnames(eB.SINE.ref.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")

#convert the coordinates to Mb
late2C.SINE.cast.sum$start <- late2C.SINE.cast.sum$start/1000000
N4C.SINE.cast.sum$start <- N4C.SINE.cast.sum$start/1000000
N8C.SINE.cast.sum$start <- N8C.SINE.cast.sum$start/1000000
N16C.SINE.cast.sum$start <- N16C.SINE.cast.sum$start/1000000
eB.SINE.cast.sum$start <- eB.SINE.cast.sum$start/1000000
late2C.SINE.cast.sum$end <- late2C.SINE.cast.sum$end/1000000
N4C.SINE.cast.sum$end <- N4C.SINE.cast.sum$end/1000000
N8C.SINE.cast.sum$end <- N8C.SINE.cast.sum$end/1000000
N16C.SINE.cast.sum$end <- N16C.SINE.cast.sum$end/1000000
eB.SINE.cast.sum$end <- eB.SINE.cast.sum$end/1000000

late2C.SINE.ref.sum$start <- late2C.SINE.ref.sum$start/1000000
N4C.SINE.ref.sum$start <- N4C.SINE.ref.sum$start/1000000
N8C.SINE.ref.sum$start <- N8C.SINE.ref.sum$start/1000000
N16C.SINE.ref.sum$start <- N16C.SINE.ref.sum$start/1000000
eB.SINE.ref.sum$start <- eB.SINE.ref.sum$start/1000000
late2C.SINE.ref.sum$end <- late2C.SINE.ref.sum$end/1000000
N4C.SINE.ref.sum$end <- N4C.SINE.ref.sum$end/1000000
N8C.SINE.ref.sum$end <- N8C.SINE.ref.sum$end/1000000
N16C.SINE.ref.sum$end <- N16C.SINE.ref.sum$end/1000000
eB.SINE.ref.sum$end <- eB.SINE.ref.sum$end/1000000



late2C.cast.dat <- late2C.SINE.cast.sum[ ,c(1,3,4,5,7)]
N4C.cast.dat <- N4C.SINE.cast.sum[ ,c(1,3,4,5,7)]
N8C.cast.dat <- N8C.SINE.cast.sum[ ,c(1,3,4,5,7)]
N16C.cast.dat <- N16C.SINE.cast.sum[ ,c(1,3,4,5,7)]
eB.cast.dat <- eB.SINE.cast.sum[ ,c(1,3,4,5,7)]
late2C.ref.dat <- late2C.SINE.ref.sum[ ,c(1,3,4,5,7)]
N4C.ref.dat <- N4C.SINE.ref.sum[ ,c(1,3,4,5,7)]
N8C.ref.dat <- N8C.SINE.ref.sum[ ,c(1,3,4,5,7)]
N16C.ref.dat <- N16C.SINE.ref.sum[ ,c(1,3,4,5,7)]
eB.ref.dat <- eB.SINE.ref.sum[ ,c(1,3,4,5,7)]

rm(N4C.SINE.cast.sum,N4C.SINE.ref.sum,N8C.SINE.ref.sum,N8C.SINE.cast.sum,N16C.SINE.ref.sum,N16C.SINE.cast.sum,eB.SINE.ref.sum,eB.SINE.cast.sum, late2C.SINE.cast.sum, late2C.SINE.ref.sum)

#total female SINE reads (the sum of all replicates, from the processed file named "CM.repeat reads ratio_200hits". )
late2C.SINE.totalNumber <- 2312129
N4C.SINE.totalNumber <- 3101072
N8C.SINE.totalNumber <- 3949845
N16C.SINE.totalNumber <- 1435485
eB.SINE.totalNumber <- 566976


#3-1. check the allelic counts on Xp allele
late2C.dat <- late2C.ref.dat
N4C.dat <- N4C.ref.dat
N8C.dat <- N8C.ref.dat
N16C.dat <- N16C.ref.dat
eB.dat <- eB.ref.dat


#merge N4 AND N8
N4ToN8.SINE.raw <- merge(N4C.dat, N8C.dat, by.x="start", by.y="start", all=T)
N4ToN8.SINE.both <- N4ToN8.SINE.raw[complete.cases(N4ToN8.SINE.raw), c(1, 3,4,5,2,6)]
colnames(N4ToN8.SINE.both) <- c("start","end","repeats","family","4C","8C")
N4ToN8.SINE.either <- N4ToN8.SINE.raw[!complete.cases(N4ToN8.SINE.raw), ]
N4ToN8.SINE.N4 <- N4ToN8.SINE.either[complete.cases(N4ToN8.SINE.either[ ,c(1:5)]), c(1,3,4,5,2)]
N4ToN8.SINE.N4[ ,6] <- 0
colnames(N4ToN8.SINE.N4) <- c("start","end","repeats","family","4C","8C")
N4ToN8.SINE.N8 <- N4ToN8.SINE.either[complete.cases(N4ToN8.SINE.either[ ,c(1,6:9)]), c(1,7,8,9,6)]
N4ToN8.SINE.N8[ ,6] <- 0
N4ToN8.SINE.N8 <- N4ToN8.SINE.N8[ ,c(1,2,3,4,6,5)]
colnames(N4ToN8.SINE.N8) <- c("start","end","repeats","family","4C","8C")
N4ToN8.SINE <- rbind(N4ToN8.SINE.both,N4ToN8.SINE.N4, N4ToN8.SINE.N8)
N4ToN8.SINE.srt <- N4ToN8.SINE[order(N4ToN8.SINE$start), ]
rm(N4ToN8.SINE.raw, N4ToN8.SINE.both, N4ToN8.SINE.either,N4ToN8.SINE.N4, N4ToN8.SINE.N8, N4ToN8.SINE)

#merge N4-8 and N16
N4ToN16.SINE.raw <- merge(N4ToN8.SINE.srt, N16C.dat, by.x="start", by.y="start", all=T)
N4ToN16.SINE.both <- N4ToN16.SINE.raw[complete.cases(N4ToN16.SINE.raw), c(1:7)]
colnames(N4ToN16.SINE.both) <- c("start","end","repeats","family","4C","8C","16C")
N4ToN16.SINE.either <- N4ToN16.SINE.raw[!complete.cases(N4ToN16.SINE.raw), ]
N4ToN16.SINE.N4 <- N4ToN16.SINE.either[ complete.cases(N4ToN16.SINE.either[ ,c(1:6)]),c(1:6)]
N4ToN16.SINE.N4[ ,7] <- 0
colnames(N4ToN16.SINE.N4) <- c("start","end","repeats","family","4C","8C","16C")
N4ToN16.SINE.N16 <- N4ToN16.SINE.either[ complete.cases(N4ToN16.SINE.either[ ,c(1,7:10)]),c(1,8,9,10,7)]
N4ToN16.SINE.N16[ ,6] <- 0
N4ToN16.SINE.N16[ ,7] <- 0
N4ToN16.SINE.N16 <- N4ToN16.SINE.N16[ ,c(1,2,3,4,6,7,5)]
colnames(N4ToN16.SINE.N16) <- c("start","end","repeats","family","4C","8C","16C")
N4ToN16.SINE <- rbind(N4ToN16.SINE.both,N4ToN16.SINE.N4, N4ToN16.SINE.N16)
N4ToN16.SINE.srt <- N4ToN16.SINE[order(N4ToN16.SINE$start), ]
rm(N4ToN16.SINE.raw, N4ToN16.SINE.both, N4ToN16.SINE.either,N4ToN16.SINE.N4, N4ToN16.SINE.N16, N4ToN16.SINE,N4ToN8.SINE.srt)

#merge N4-16 and eB
N4ToeB.SINE.raw <- merge(N4ToN16.SINE.srt, eB.dat, by.x="start", by.y="start", all=T)
N4ToeB.SINE.both <- N4ToeB.SINE.raw[complete.cases(N4ToeB.SINE.raw), c(1:8)]
colnames(N4ToeB.SINE.both) <- c("start","end","repeats","family","4C","8C","16C","eB")
N4ToeB.SINE.either <- N4ToeB.SINE.raw[!complete.cases(N4ToeB.SINE.raw), ]
N4ToeB.SINE.N4 <- N4ToeB.SINE.either[ complete.cases(N4ToeB.SINE.either[ ,c(1:7)]),c(1:7)]
N4ToeB.SINE.N4[ ,8] <- 0
colnames(N4ToeB.SINE.N4) <- c("start","end","repeats","family","4C","8C","16C","eB")
N4ToeB.SINE.eB <- N4ToeB.SINE.either[ complete.cases(N4ToeB.SINE.either[ ,c(1,8:11)]),c(1,9,10,11,8)]
N4ToeB.SINE.eB[ ,6] <- 0
N4ToeB.SINE.eB[ ,7] <- 0
N4ToeB.SINE.eB[ ,8] <- 0
N4ToeB.SINE.eB <- N4ToeB.SINE.eB[ ,c(1,2,3,4,6,7,8,5)]
colnames(N4ToeB.SINE.eB) <- c("start","end","repeats","family","4C","8C","16C","eB")
N4ToeB.SINE <- rbind(N4ToeB.SINE.both, N4ToeB.SINE.N4, N4ToeB.SINE.eB)
N4ToeB.SINE.srt <- N4ToeB.SINE[order(N4ToeB.SINE$start), ]
#filter out SINEs of low read counts, less than 3
N4ToeB.SINE.filtered <- N4ToeB.SINE[which(rowSums(N4ToeB.SINE[ ,c(5:8)]) >3), ]
N4ToeB.SINE.srt <- N4ToeB.SINE.filtered[order(N4ToeB.SINE.filtered$start), ]
rm(N4ToeB.SINE.raw, N4ToeB.SINE.both, N4ToeB.SINE.either,N4ToeB.SINE.N4, N4ToeB.SINE.eB, N4ToeB.SINE,N4ToN16.SINE.srt)

#optionally, merge N4-eB and late2cell
late2CToeB.SINE.raw <- merge(late2C.dat, N4ToeB.SINE.srt, by.x="start", by.y="start", all=T)
late2CToeB.SINE.both <- late2CToeB.SINE.raw[complete.cases(late2CToeB.SINE.raw), c(1,3,4,5,2,9:12)]
colnames(late2CToeB.SINE.both) <- c("start","end","repeats","family","late2C","4C","8C","16C","eB")
late2CToeB.SINE.either <- late2CToeB.SINE.raw[!complete.cases(late2CToeB.SINE.raw), ]
late2CToeB.SINE.late2C <- late2CToeB.SINE.either[ complete.cases(late2CToeB.SINE.either[ ,c(1:5)]),c(1,3,4,5,2,9:12)]
late2CToeB.SINE.late2C[ ,c(6:9)] <- 0
colnames(late2CToeB.SINE.late2C) <- c("start","end","repeats","family","late2C","4C","8C","16C","eB")
late2CToeB.SINE.eB <- late2CToeB.SINE.either[ complete.cases(late2CToeB.SINE.either[ ,c(1,6:12)]),c(1,6:12)]
late2CToeB.SINE.eB[ ,9] <- 0
late2CToeB.SINE.eB <- late2CToeB.SINE.eB[ ,c(1:4,9,5:8)]
colnames(late2CToeB.SINE.eB) <- c("start","end","repeats","family","late2C","4C","8C","16C","eB")
late2CToeB.SINE <- rbind(late2CToeB.SINE.both, late2CToeB.SINE.late2C, late2CToeB.SINE.eB)

#filter out SINEs of low read counts, less than 3
late2CToeB.SINE.filtered <- late2CToeB.SINE[which(rowSums(late2CToeB.SINE[ ,c(5:9)]) >3), ]
late2CToeB.SINE.srt <- late2CToeB.SINE.filtered[order(late2CToeB.SINE.filtered$start), ]
rm(late2CToeB.SINE.raw, late2CToeB.SINE.both, late2CToeB.SINE.either,late2CToeB.SINE.late2C, late2CToeB.SINE.eB, late2CToeB.SINE.filtered)

#normalize the counts using total female SINE counts in each stage
cast.late2CToeB.SINE.norm <- data.frame(late2CToeB.SINE.srt[ ,c(1:4)],
                                        late2C=late2CToeB.SINE.srt[ ,5]*1e6/late2C.SINE.totalNumber,
                                        N4C=late2CToeB.SINE.srt[ ,6]*1e6/N4C.SINE.totalNumber, 
                                        N8C=late2CToeB.SINE.srt[ ,7]*1e6/N8C.SINE.totalNumber,
                                        N16C=late2CToeB.SINE.srt[ ,8]*1e6/N16C.SINE.totalNumber,
                                        eB=late2CToeB.SINE.srt[ ,9]*1e6/eB.SINE.totalNumber,
                                        allele="cas")

#N16C=late2CToeB.SINE.srt[ ,8]*1e6/sum(N16cell.comp),
#eB=late2CToeB.SINE.srt[ ,9]*1e6/sum(eB.comp))

rm(list=setdiff(ls(), c("cast.late2CToeB.SINE.norm",
                        "late2C.ref.dat","N4C.ref.dat","N8C.ref.dat","N16C.ref.dat","eB.ref.dat",
                        "late2C.SINE.totalNumber","N4C.SINE.totalNumber","N8C.SINE.totalNumber","N16C.SINE.totalNumber","eB.SINE.totalNumber")))


theme_pattern <- theme(axis.title.x=element_text(),
                       axis.text.x=element_blank(),
                       legend.text=element_blank(),
                       legend.title=element_blank(),
                       legend.key = element_blank(),
                       #legend.text=element_text(size = rel(1)),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       axis.line.y = element_line(color="black"),
                       axis.line.x = element_line(color="black"),
                       legend.background = element_rect(fill=alpha('NA', 0.2)))

dat <- cast.late2CToeB.SINE.norm[ , c(1,5:9)]



#plot1 normalized CAST counts vs loci
SINE.4C <- ggplot(dat, aes(x=start,y=N4C)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.5) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 150)) +
  scale_x_continuous(limits = c(1,175), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140,160)) +
  geom_vline(xintercept=103.4,  colour="red",linetype="longdash") 

SINE.8C <- ggplot(dat, aes(x=start,y=N8C)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.5) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 150)) +
  scale_x_continuous(limits = c(1,175), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140,160)) +
  geom_vline(xintercept=103.4,  colour="red",linetype="longdash") 

SINE.16C <- ggplot(dat, aes(x=start,y=N16C)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.5) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 150)) +
  scale_x_continuous(limits = c(1,175), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140,160)) +
  geom_vline(xintercept=103.4,  colour="red",linetype="longdash") 

SINE.eB <- ggplot(dat, aes(x=start,y=eB)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.5) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 150)) +
  scale_x_continuous(limits = c(1,175), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140,160)) +
  geom_vline(xintercept=103.4,  colour="red",linetype="longdash") 

library(gridExtra)
grid.arrange(SINE.4C, SINE.8C, SINE.16C, SINE.eB, nrow=4, ncol=1)


#divide all SINEs into 3 groups: (proximal (1-15), intermediate(15-57), distal(57-end)), the same as the analysis in MC embryos.

dat[which(abs(dat$start-103.4)<=15), "type" ] <- "proximal"
dat[which(abs(dat$start-103.4)>15 & abs(dat$start-103.4)<=57), "type" ] <- "intermediate"
dat[which(abs(dat$start-103.4)>57), "type" ] <- "distal"
#based on my observation in the plot above, the intermediate section on the right side of Xist should be further divided to intermediate (120-146) and distal (>146)
dat[which(dat$start > 146), "type"] <- "distal"

#Panel e. used for plots-------------------------------------------
#For comparison between 4C and 8C, I used original "N4_N8C.dat" (including count 0) 
N4_N8C.dat <- dat[ ,c(1,3,4,7)]
N4_N8C.dat <- N4_N8C.dat[which(N4_N8C.dat[ ,2] !=0 | N4_N8C.dat[ ,3] !=0), ]
N4C.dat <- data.frame(count=N4_N8C.dat$N4C, stage="4C", type=N4_N8C.dat$type)
N8C.dat <- data.frame(count=N4_N8C.dat$N8C, stage="8C", type=N4_N8C.dat$type)
combined.dat <- rbind(N4C.dat, N8C.dat)
combined.dat$count <- log(combined.dat$count+1)
combined.dat$type <- factor(combined.dat$type, levels=c("proximal", "intermediate", "distal"))
combined.dat$stage <- factor(combined.dat$stage, levels=c("4C", "8C"))

#p-value, proximal genes between 4C and 8C
N4_N8C.proximal.dat <- combined.dat[which(combined.dat$type=="proximal"), ]
wilcox.test(count ~ stage, data=N4_N8C.proximal.dat,paired=T)$p.value

#p-value, intermediate genes between 4C and 8C
N4_N8C.inter.dat <- combined.dat[which(combined.dat$type=="intermediate"), ]
wilcox.test(count ~ stage, data=N4_N8C.inter.dat, paired=T)$p.value

#p-value, distal genes between 4C and 8C
N4_N8C.distal.dat <- combined.dat[which(combined.dat$type=="distal"), ]
wilcox.test(count ~ stage, data=N4_N8C.distal.dat,paired=T)$p.value


ggplot(combined.dat, aes(x=type,y=count, fill=stage)) +
  geom_violin() + 
  scale_fill_manual(values=c("indianred1","skyblue2")) +
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



#Panel f. used for plots---------------------------------------------------
#For comparison between 8C and 16C, I used original "N8_N16C.dat" (including count 0) 
N8_N16C.dat <- dat[ ,c(1,4,5,7)]
N8_N16C.dat <- N8_N16C.dat[which(N8_N16C.dat[ ,2] !=0 | N8_N16C.dat[ ,3] !=0), ]
N8C.dat <- data.frame(count=N8_N16C.dat$N8C, stage="8C", type=N8_N16C.dat$type)
N16C.dat <- data.frame(count=N8_N16C.dat$N16C, stage="16C", type=N8_N16C.dat$type)
combined.dat <- rbind(N8C.dat, N16C.dat)
combined.dat$count <- log(combined.dat$count+1)
combined.dat$type <- factor(combined.dat$type, levels=c("proximal", "intermediate", "distal"))
combined.dat$stage <- factor(combined.dat$stage, levels=c("8C", "16C"))

#p-value, proximal genes between 8C and 16C
N8_N16C.proximal.dat <- combined.dat[which(combined.dat$type=="proximal"), ]
wilcox.test(count ~ stage, data=N8_N16C.proximal.dat, paired=T)$p.value

#p-value, intermediate genes between 8C and 16C
N8_N16C.inter.dat <- combined.dat[which(combined.dat$type=="intermediate"), ]
wilcox.test(count ~ stage, data=N8_N16C.inter.dat, paired=T)$p.value

#p-value, distal genes between 8C and 16C
N8_N16C.distal.dat <- combined.dat[which(combined.dat$type=="distal"), ]
wilcox.test(count ~ stage, data=N8_N16C.distal.dat, paired=T)$p.value


ggplot(combined.dat, aes(x=type,y=count, fill=stage)) +
  geom_violin() + 
  scale_fill_manual(values=c("skyblue2","lightgoldenrod1")) +
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



#Panel g. used for plots------------------------------------------------
#For comparison between 8C and 16C, I used original "N8_N16C.dat" (including count 0) 
N16_eB.dat <- dat[ ,c(1,5,6,7)]
N16_eB.dat <- N16_eB.dat[which(N16_eB.dat[ ,2] !=0 | N16_eB.dat[ ,3] !=0), ]
N16C.dat <- data.frame(count=N16_eB.dat$N16C, stage="16C", type=N16_eB.dat$type)
eB.dat <- data.frame(count=N16_eB.dat$eB, stage="eB", type=N16_eB.dat$type)
combined.dat <- rbind(N16C.dat, eB.dat)
combined.dat$count <- log(combined.dat$count+1)
combined.dat$type <- factor(combined.dat$type, levels=c("proximal", "intermediate", "distal"))
combined.dat$stage <- factor(combined.dat$stage, levels=c("16C", "eB"))

#p-value, proximal genes between 8C and 16C
N16_eB.proximal.dat <- combined.dat[which(combined.dat$type=="proximal"), ]
wilcox.test(count ~ stage, data=N16_eB.proximal.dat, paired=T)$p.value

#p-value, intermediate genes between 8C and 16C
N16_eB.inter.dat <- combined.dat[which(combined.dat$type=="intermediate"), ]
wilcox.test(count ~ stage, data=N16_eB.inter.dat, paired=T)$p.value

#p-value, distal genes between 8C and 16C
N16_eB.distal.dat <- combined.dat[which(combined.dat$type=="distal"), ]
wilcox.test(count ~ stage, data=N16_eB.distal.dat, paired=T)$p.value


ggplot(combined.dat, aes(x=type,y=count, fill=stage)) +
  geom_violin() + 
  scale_fill_manual(values=c("lightgoldenrod1","darkorange2")) +
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






#Panel h.LTR skew vs coordinates on Xp---------------------------------------

rm(list=ls())
library(RColorBrewer)
library(ggplot2)
library(gridExtra)


late2C.LTR.ref.sum <- read.table(file="processed/BED/chrX.CM_late2cell_LTR.REF.sum.bed", sep="\t")
late2C.LTR.cast.sum <- read.table(file="processed/BED/chrX.CM_late2cell_LTR.CAST.sum.bed", sep="\t")
N4C.LTR.ref.sum <- read.table(file="processed/BED/chrX.CM_4cell_LTR.REF.sum.bed", sep="\t")
N4C.LTR.cast.sum <- read.table(file="processed/BED/chrX.CM_4cell_LTR.CAST.sum.bed", sep="\t")
N8C.LTR.ref.sum <- read.table(file="processed/BED/chrX.CM_8cell_LTR.REF.sum.bed", sep="\t")
N8C.LTR.cast.sum <- read.table(file="processed/BED/chrX.CM_8cell_LTR.CAST.sum.bed", sep="\t")
N16C.LTR.ref.sum <- read.table(file="processed/BED/chrX.CM_16cell_LTR.REF.sum.bed", sep="\t")
N16C.LTR.cast.sum <- read.table(file="processed/BED/chrX.CM_16cell_LTR.CAST.sum.bed", sep="\t")
eB.LTR.ref.sum <- read.table(file="processed/BED/chrX.CM_earlyBlast_LTR.REF.sum.bed", sep="\t")
eB.LTR.cast.sum <- read.table(file="processed/BED/chrX.CM_earlyBlast_LTR.CAST.sum.bed", sep="\t")

colnames(late2C.LTR.cast.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(N4C.LTR.cast.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(N8C.LTR.cast.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(N16C.LTR.cast.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(eB.LTR.cast.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(late2C.LTR.ref.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(N4C.LTR.ref.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(N8C.LTR.ref.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(N16C.LTR.ref.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(eB.LTR.ref.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")



#convert the coordinates to Mb
late2C.LTR.cast.sum$start <- late2C.LTR.cast.sum$start/1000000
N4C.LTR.cast.sum$start <- N4C.LTR.cast.sum$start/1000000
N8C.LTR.cast.sum$start <- N8C.LTR.cast.sum$start/1000000
N16C.LTR.cast.sum$start <- N16C.LTR.cast.sum$start/1000000
eB.LTR.cast.sum$start <- eB.LTR.cast.sum$start/1000000
late2C.LTR.cast.sum$end <- late2C.LTR.cast.sum$end/1000000
N4C.LTR.cast.sum$end <- N4C.LTR.cast.sum$end/1000000
N8C.LTR.cast.sum$end <- N8C.LTR.cast.sum$end/1000000
N16C.LTR.cast.sum$end <- N16C.LTR.cast.sum$end/1000000
eB.LTR.cast.sum$end <- eB.LTR.cast.sum$end/1000000

late2C.LTR.ref.sum$start <- late2C.LTR.ref.sum$start/1000000
N4C.LTR.ref.sum$start <- N4C.LTR.ref.sum$start/1000000
N8C.LTR.ref.sum$start <- N8C.LTR.ref.sum$start/1000000
N16C.LTR.ref.sum$start <- N16C.LTR.ref.sum$start/1000000
eB.LTR.ref.sum$start <- eB.LTR.ref.sum$start/1000000
late2C.LTR.ref.sum$end <- late2C.LTR.ref.sum$end/1000000
N4C.LTR.ref.sum$end <- N4C.LTR.ref.sum$end/1000000
N8C.LTR.ref.sum$end <- N8C.LTR.ref.sum$end/1000000
N16C.LTR.ref.sum$end <- N16C.LTR.ref.sum$end/1000000
eB.LTR.ref.sum$end <- eB.LTR.ref.sum$end/1000000



late2C.cast.dat <- late2C.LTR.cast.sum[ ,c(5,2,3,4,8)]
N4C.cast.dat <- N4C.LTR.cast.sum[ ,c(5,2,3,4,8)]
N8C.cast.dat <- N8C.LTR.cast.sum[ ,c(5,2,3,4,8)]
N16C.cast.dat <- N16C.LTR.cast.sum[ ,c(5,2,3,4,8)]
eB.cast.dat <- eB.LTR.cast.sum[ ,c(5,2,3,4,8)]
late2C.ref.dat <- late2C.LTR.ref.sum[ ,c(5,2,3,4,8)]
N4C.ref.dat <- N4C.LTR.ref.sum[ ,c(5,2,3,4,8)]
N8C.ref.dat <- N8C.LTR.ref.sum[ ,c(5,2,3,4,8)]
N16C.ref.dat <- N16C.LTR.ref.sum[ ,c(5,2,3,4,8)]
eB.ref.dat <- eB.LTR.ref.sum[ ,c(5,2,3,4,8)]

rm(N4C.LTR.cast.sum,N4C.LTR.ref.sum,
   N8C.LTR.ref.sum,N8C.LTR.cast.sum,
   N16C.LTR.ref.sum,N16C.LTR.cast.sum,
   eB.LTR.ref.sum,eB.LTR.cast.sum, 
   late2C.LTR.cast.sum, late2C.LTR.ref.sum)

#total female LTR reads (the sum of all replicates, from the processed file named "CM.repeat reads ratio_200hits". )
late2C.LTR.totalNumber <- 2458996
N4C.LTR.totalNumber <- 1309082
N8C.LTR.totalNumber <- 1302713
N16C.LTR.totalNumber <- 729605
eB.LTR.totalNumber <- 169989


#3-1. check the allelic counts on Xp allele
late2C.dat <- late2C.ref.dat
N4C.dat <- N4C.ref.dat
N8C.dat <- N8C.ref.dat
N16C.dat <- N16C.ref.dat
eB.dat <- eB.ref.dat


#merge N4 AND N8
N4ToN8.LTR.raw <- merge(N4C.dat, N8C.dat, by.x="start", by.y="start", all=T)
N4ToN8.LTR.both <- N4ToN8.LTR.raw[complete.cases(N4ToN8.LTR.raw), c(1,3,4,5,2,6)]
colnames(N4ToN8.LTR.both) <- c("start","end","repeats","subfamily","4C","8C")
N4ToN8.LTR.either <- N4ToN8.LTR.raw[!complete.cases(N4ToN8.LTR.raw), ]
N4ToN8.LTR.N4 <- N4ToN8.LTR.either[complete.cases(N4ToN8.LTR.either[ ,c(1:5)]), c(1,3,4,5,2)]
N4ToN8.LTR.N4[ ,6] <- 0
colnames(N4ToN8.LTR.N4) <- c("start","end","repeats","subfamily","4C","8C")
N4ToN8.LTR.N8 <- N4ToN8.LTR.either[complete.cases(N4ToN8.LTR.either[ ,c(1,6:9)]), c(1,7,8,9,6)]
N4ToN8.LTR.N8[ ,6] <- 0
N4ToN8.LTR.N8 <- N4ToN8.LTR.N8[ ,c(1,2,3,4,6,5)]
colnames(N4ToN8.LTR.N8) <- c("start","end","repeats","subfamily","4C","8C")
N4ToN8.LTR <- rbind(N4ToN8.LTR.both,N4ToN8.LTR.N4, N4ToN8.LTR.N8)
N4ToN8.LTR.srt <- N4ToN8.LTR[order(N4ToN8.LTR$start), ]
rm(N4ToN8.LTR.raw, N4ToN8.LTR.both, N4ToN8.LTR.either,N4ToN8.LTR.N4, N4ToN8.LTR.N8, N4ToN8.LTR)

#merge N4-8 and N16
N4ToN16.LTR.raw <- merge(N4ToN8.LTR.srt, N16C.dat, by.x="start", by.y="start", all=T)
N4ToN16.LTR.both <- N4ToN16.LTR.raw[complete.cases(N4ToN16.LTR.raw), c(1:7)]
colnames(N4ToN16.LTR.both) <- c("start","end","repeats","subfamily","4C","8C","16C")
N4ToN16.LTR.either <- N4ToN16.LTR.raw[!complete.cases(N4ToN16.LTR.raw), ]
N4ToN16.LTR.N4 <- N4ToN16.LTR.either[ complete.cases(N4ToN16.LTR.either[ ,c(1:6)]),c(1:6)]
N4ToN16.LTR.N4[ ,7] <- 0
colnames(N4ToN16.LTR.N4) <- c("start","end","repeats","subfamily","4C","8C","16C")
N4ToN16.LTR.N16 <- N4ToN16.LTR.either[ complete.cases(N4ToN16.LTR.either[ ,c(1,7:10)]),c(1,8,9,10,7)]
N4ToN16.LTR.N16[ ,6] <- 0
N4ToN16.LTR.N16[ ,7] <- 0
N4ToN16.LTR.N16 <- N4ToN16.LTR.N16[ ,c(1,2,3,4,6,7,5)]
colnames(N4ToN16.LTR.N16) <- c("start","end","repeats","subfamily","4C","8C","16C")
N4ToN16.LTR <- rbind(N4ToN16.LTR.both,N4ToN16.LTR.N4, N4ToN16.LTR.N16)
N4ToN16.LTR.srt <- N4ToN16.LTR[order(N4ToN16.LTR$start), ]
rm(N4ToN16.LTR.raw, N4ToN16.LTR.both, N4ToN16.LTR.either,N4ToN16.LTR.N4, N4ToN16.LTR.N16, N4ToN16.LTR,N4ToN8.LTR.srt)

#merge N4-16 and eB
N4ToeB.LTR.raw <- merge(N4ToN16.LTR.srt, eB.dat, by.x="start", by.y="start", all=T)
N4ToeB.LTR.both <- N4ToeB.LTR.raw[complete.cases(N4ToeB.LTR.raw), c(1:8)]
colnames(N4ToeB.LTR.both) <- c("start","end","repeats","subfamily","4C","8C","16C","eB")
N4ToeB.LTR.either <- N4ToeB.LTR.raw[!complete.cases(N4ToeB.LTR.raw), ]
N4ToeB.LTR.N4 <- N4ToeB.LTR.either[ complete.cases(N4ToeB.LTR.either[ ,c(1:7)]),c(1:7)]
N4ToeB.LTR.N4[ ,8] <- 0
colnames(N4ToeB.LTR.N4) <- c("start","end","repeats","subfamily","4C","8C","16C","eB")
N4ToeB.LTR.eB <- N4ToeB.LTR.either[ complete.cases(N4ToeB.LTR.either[ ,c(1,8:11)]),c(1,9,10,11,8)]
N4ToeB.LTR.eB[ ,6] <- 0
N4ToeB.LTR.eB[ ,7] <- 0
N4ToeB.LTR.eB[ ,8] <- 0
N4ToeB.LTR.eB <- N4ToeB.LTR.eB[ ,c(1,2,3,4,6,7,8,5)]
colnames(N4ToeB.LTR.eB) <- c("start","end","repeats","subfamily","4C","8C","16C","eB")
N4ToeB.LTR <- rbind(N4ToeB.LTR.both, N4ToeB.LTR.N4, N4ToeB.LTR.eB)
N4ToeB.LTR.srt <- N4ToeB.LTR[order(N4ToeB.LTR$start), ]
#filter out LTRs of low read counts, less than 3
N4ToeB.LTR.filtered <- N4ToeB.LTR[which(rowSums(N4ToeB.LTR[ ,c(5:8)]) >3), ]
N4ToeB.LTR.srt <- N4ToeB.LTR.filtered[order(N4ToeB.LTR.filtered$start), ]
rm(N4ToeB.LTR.raw, N4ToeB.LTR.both, N4ToeB.LTR.either,N4ToeB.LTR.N4, N4ToeB.LTR.eB, N4ToeB.LTR,N4ToN16.LTR.srt)

#optionally, merge N4-eB and late2cell
late2CToeB.LTR.raw <- merge(late2C.dat, N4ToeB.LTR.srt, by.x="start", by.y="start", all=T)
late2CToeB.LTR.both <- late2CToeB.LTR.raw[complete.cases(late2CToeB.LTR.raw), c(1,3,4,5,2,9:12)]
colnames(late2CToeB.LTR.both) <- c("start","end","repeats","subfamily","late2C","4C","8C","16C","eB")
late2CToeB.LTR.either <- late2CToeB.LTR.raw[!complete.cases(late2CToeB.LTR.raw), ]
late2CToeB.LTR.late2C <- late2CToeB.LTR.either[ complete.cases(late2CToeB.LTR.either[ ,c(1:5)]),c(1,3,4,5,2,9:12)]
late2CToeB.LTR.late2C[ ,c(6:9)] <- 0
colnames(late2CToeB.LTR.late2C) <- c("start","end","repeats","subfamily","late2C","4C","8C","16C","eB")
late2CToeB.LTR.eB <- late2CToeB.LTR.either[ complete.cases(late2CToeB.LTR.either[ ,c(1,6:12)]),c(1,6:12)]
late2CToeB.LTR.eB[ ,9] <- 0
late2CToeB.LTR.eB <- late2CToeB.LTR.eB[ ,c(1:4,9,5:8)]
colnames(late2CToeB.LTR.eB) <- c("start","end","repeats","subfamily","late2C","4C","8C","16C","eB")
late2CToeB.LTR <- rbind(late2CToeB.LTR.both, late2CToeB.LTR.late2C, late2CToeB.LTR.eB)

#filter out LTRs of low read counts, less than 3
late2CToeB.LTR.filtered <- late2CToeB.LTR
late2CToeB.LTR.filtered <- late2CToeB.LTR[which(rowSums(late2CToeB.LTR[ ,c(5:9)]) >3), ]
late2CToeB.LTR.srt <- late2CToeB.LTR.filtered[order(late2CToeB.LTR.filtered$start), ]
rm(late2CToeB.LTR.raw, late2CToeB.LTR.both, late2CToeB.LTR.either,late2CToeB.LTR.late2C, late2CToeB.LTR.eB, late2CToeB.LTR.filtered)

#normalize the counts using total female LTR counts in each stage
cast.late2CToeB.LTR.norm <- data.frame(late2CToeB.LTR.srt[ ,c(1:4)],
                                       late2C=late2CToeB.LTR.srt[ ,5]*1e6/late2C.LTR.totalNumber,
                                       N4C=late2CToeB.LTR.srt[ ,6]*1e6/N4C.LTR.totalNumber, 
                                       N8C=late2CToeB.LTR.srt[ ,7]*1e6/N8C.LTR.totalNumber,
                                       N16C=late2CToeB.LTR.srt[ ,8]*1e6/N16C.LTR.totalNumber,
                                       eB=late2CToeB.LTR.srt[ ,9]*1e6/eB.LTR.totalNumber,
                                       allele="ref")


rm(list=setdiff(ls(), c("cast.late2CToeB.LTR.norm",
                        "late2C.ref.dat","N4C.ref.dat","N8C.ref.dat","N16C.ref.dat","eB.ref.dat",
                        "late2C.LTR.totalNumber","N4C.LTR.totalNumber","N8C.LTR.totalNumber","N16C.LTR.totalNumber","eB.LTR.totalNumber")))


theme_pattern <- theme(axis.title.x=element_text(),
                       axis.text.x=element_blank(),
                       legend.text=element_blank(),
                       legend.title=element_blank(),
                       legend.key = element_blank(),
                       #legend.text=element_text(size = rel(1)),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       axis.line.y = element_line(color="black"),
                       axis.line.x = element_line(color="black"),
                       legend.background = element_rect(fill=alpha('NA', 0.2)))

dat <- cast.late2CToeB.LTR.norm
dat[ ,11] <- (dat$end-dat$start)*1000000

#manual inspection find several misannoatation of repeats, and confirmed in the genome browser.
#excluding line named as "75","76","253","254","256","351","509","727"
#excluding line named as"37,46,47,53,54,92,93,98,101,102,175,238,261,278, 279,281,359,361,543,569,573,747" due to the overlap with SINEs.
removed.line <- c("75","76","253","254","256","351","509","727",
                  "37","46","47","53","54","92","93","98","101","102","175","238","261","278", "279","281","359","361","543","569","573","747")
dat <- dat[-match(removed.line, rownames(dat)), ]
dat[ ,11] <- (dat$end-dat$start)*1000000
dat <- dat[which(dat$V11 >=50), ]




#plot total normalized CAST counts vs loci
LTR.4C <- ggplot(dat, aes(x=start,y=N4C)) +
  geom_bar(stat="identity", color="black", fill="lightblue",linewidth=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 150)) +
  scale_x_continuous(limits = c(1,175), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140,160)) +
  geom_vline(xintercept=103.4,  colour="red",linetype="longdash") 

LTR.8C <- ggplot(dat, aes(x=start,y=N8C)) +
  geom_bar(stat="identity", color="black", fill="lightblue",linewidth=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 150)) +
  scale_x_continuous(limits = c(1,175), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140,160)) +
  geom_vline(xintercept=103.4,  colour="red",linetype="longdash") 

LTR.16C <- ggplot(dat, aes(x=start,y=N16C)) +
  geom_bar(stat="identity", color="black", fill="lightblue",linewidth=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 150)) +
  scale_x_continuous(limits = c(1,175), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140,160)) +
  geom_vline(xintercept=103.4,  colour="red",linetype="longdash") 
LTR.eB <- ggplot(dat, aes(x=start,y=eB)) +
  geom_bar(stat="identity", color="black", fill="lightblue",linewidth=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 150)) +
  scale_x_continuous(limits = c(1,175), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140,160)) +
  geom_vline(xintercept=103.4,  colour="red",linetype="longdash") 

library(gridExtra)
grid.arrange(LTR.4C, LTR.8C, LTR.16C, LTR.eB, nrow=4, ncol=1)



# Panel i, split chrX into two region: proximal and distal, and check LTR silencing on these two regions.----------
dat[which(dat$start-103.4 > -49.87 & dat$start-103.4 < 25.13), "type" ] <- "proximal"  #loci > 35kb, and < 110
#dat[which(dat$start-103.4 < 25.13), "type" ] <- "proximal"
dat[which(dat$start-103.4 < -64.87), "type" ] <- "distal_1"  #loci <= 20kb
dat[which(dat$start-103.4 > 25.13), "type" ] <- "distal_2" #loci >= 110kb
#remove NA
dat <- dat[complete.cases(dat), ]


#For comparison between 4C and 8C, I used original "N4_N8C.dat" (including count 0) 
N4_N8C.dat <- dat[ ,c(1,6,7,12)]
N4_N8C.dat <- N4_N8C.dat[which(N4_N8C.dat[ ,2] !=0 | N4_N8C.dat[ ,3] !=0), ]
N4C.dat <- data.frame(count=N4_N8C.dat$N4C, stage="4C", type=N4_N8C.dat$type)
N8C.dat <- data.frame(count=N4_N8C.dat$N8C, stage="8C", type=N4_N8C.dat$type)
combined.dat <- rbind(N4C.dat, N8C.dat)
combined.dat$count <- log(combined.dat$count+1)
combined.dat$type <- factor(combined.dat$type, levels=c("proximal", "distal_1", "distal_2"))
combined.dat$stage <- factor(combined.dat$stage, levels=c("4C", "8C"))

#p-value, proximal genes between 4C and 8C, p=0.08980233
N4_N8C.proximal.dat <- combined.dat[which(combined.dat$type=="proximal"), ]
wilcox.test(count ~ stage, data=N4_N8C.proximal.dat,paired=T)$p.value

#p-value, distal genes between 4C and 8C, p=0.8012688
N4_N8C.distal.dat <- combined.dat[which(combined.dat$type=="distal_1"), ]
wilcox.test(count ~ stage, data=N4_N8C.distal.dat,paired=T)$p.value

#p-value, distal genes between 4C and 8C, p=0.8388938
N4_N8C.distal.dat <- combined.dat[which(combined.dat$type=="distal_2"), ]
wilcox.test(count ~ stage, data=N4_N8C.distal.dat,paired=T)$p.value


ggplot(combined.dat, aes(x=type,y=count, fill=stage)) +
  geom_violin() + 
  scale_fill_manual(values=c("indianred1","skyblue2")) +
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



#Panel j. used for plots------------------------------------------------------------------------
#For comparison between 8C and 16C, I used original "N8_N16C.dat" (including count 0) 
N8_N16C.dat <- dat[ ,c(1,7,8,12)]
N8_N16C.dat <- N8_N16C.dat[which(N8_N16C.dat[ ,2] !=0 | N8_N16C.dat[ ,3] !=0), ]
N8C.dat <- data.frame(count=N8_N16C.dat$N8C, stage="8C", type=N8_N16C.dat$type)
N16C.dat <- data.frame(count=N8_N16C.dat$N16C, stage="16C", type=N8_N16C.dat$type)
combined.dat <- rbind(N8C.dat, N16C.dat)
combined.dat$count <- log(combined.dat$count+1)
combined.dat$type <- factor(combined.dat$type, levels=c("proximal", "distal_1","distal_2"))
combined.dat$stage <- factor(combined.dat$stage, levels=c("8C", "16C"))

#p-value, proximal genes between 8C and 16C, p=6.768347e-06
N8_N16C.proximal.dat <- combined.dat[which(combined.dat$type=="proximal"), ]
wilcox.test(count ~ stage, data=N8_N16C.proximal.dat, paired=T)$p.value


#p-value, distal genes between 8C and 16C, p= 0.3166693
N8_N16C.distal.dat <- combined.dat[which(combined.dat$type=="distal_1"), ]
wilcox.test(count ~ stage, data=N8_N16C.distal.dat, paired=T)$p.value

#p-value, distal genes between 8C and 16C, p=0.0006568805
N8_N16C.distal.dat <- combined.dat[which(combined.dat$type=="distal_2"), ]
wilcox.test(count ~ stage, data=N8_N16C.distal.dat, paired=T)$p.value


ggplot(combined.dat, aes(x=type,y=count, fill=stage)) +
  geom_violin() + 
  scale_fill_manual(values=c("skyblue2","lightgoldenrod1")) +
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




#Panel k. silencing dynamics of distinct groups of LTR in CM embryos.--------------------------------------------------

#remove late2C data
dat <- dat[which(rowSums(dat[ ,c(6:9)] != 0) >0), -5]

dat[which(dat$start-103.4 > -49.87 & dat$start-103.4 < 25.13), "type" ] <- "proximal"  #loci > 35kb, and < 110
dat[which(dat$start-103.4 < -64.87), "type" ] <- "distal_1"  #loci <= 20kb
dat[which(dat$start-103.4 > 25.13), "type" ] <- "distal_2" #loci >= 110kb
#remove NA
dat <- dat[complete.cases(dat), ]

dat2 <- data.frame(counts=c(dat$N4C, dat$N8C, dat$N16C, dat$eB),
                   stage=c(rep("4C", nrow(dat)), rep("8C", nrow(dat)),rep("16C", nrow(dat)),rep("eB", nrow(dat))),
                   type=c(dat$type, dat$type, dat$type, dat$type))

dat2$stage <- factor(dat2$stage, levels=c("4C", "8C","16C","eB"))


ggplot(dat2, aes(x=type, y=counts, fill=stage)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0,50)) +
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

#p-value
#proximal, 4C vs 8C
proximal.dat <- dat2[which(dat2$type=="proximal"), ]
test.dat <- proximal.dat[which(proximal.dat$stage=="4C" | proximal.dat$stage=="8C"), ]
wilcox.test(counts ~ stage, data=test.dat, pair=T)$p.value

#proximal, 8C vs 16C 
proximal.dat <- dat2[which(dat2$type=="proximal"), ]
test.dat <- proximal.dat[which(proximal.dat$stage=="16C" | proximal.dat$stage=="8C"), ]
wilcox.test(counts ~ stage, data=test.dat, pair=T)$p.value

#proximal, eB vs 16C
proximal.dat <- dat2[which(dat2$type=="proximal"), ]
test.dat <- proximal.dat[which(proximal.dat$stage=="16C" | proximal.dat$stage=="eB"), ]
wilcox.test(counts ~ stage, data=test.dat, pair=T)$p.value

#----------
#distal_2, 4C vs 8C
distal_2.dat <- dat2[which(dat2$type=="distal_2"), ]
test.dat <- distal_2.dat[which(distal_2.dat$stage=="4C" | distal_2.dat$stage=="8C"), ]
wilcox.test(counts ~ stage, data=test.dat, pair=T)$p.value

#distal_2, 8C vs 16C 
distal_2.dat <- dat2[which(dat2$type=="distal_2"), ]
test.dat <- distal_2.dat[which(distal_2.dat$stage=="16C" | distal_2.dat$stage=="8C"), ]
wilcox.test(counts ~ stage, data=test.dat, pair=T)$p.value

#distal_2, eB vs 16C
distal_2.dat <- dat2[which(dat2$type=="distal_2"), ]
test.dat <- distal_2.dat[which(distal_2.dat$stage=="16C" | distal_2.dat$stage=="eB"), ]
wilcox.test(counts ~ stage, data=test.dat, pair=T)$p.value

#---------
#distal_1, 4C vs 8C
distal_1.dat <- dat2[which(dat2$type=="distal_1"), ]
test.dat <- distal_1.dat[which(distal_1.dat$stage=="4C" | distal_1.dat$stage=="8C"), ]
wilcox.test(counts ~ stage, data=test.dat, pair=T)$p.value

#distal_1, 8C vs 16C
distal_1.dat <- dat2[which(dat2$type=="distal_1"), ]
test.dat <- distal_1.dat[which(distal_1.dat$stage=="16C" | distal_1.dat$stage=="8C"), ]
wilcox.test(counts ~ stage, data=test.dat, pair=T)$p.value

#distal_2, eB vs 16C
distal_1.dat <- dat2[which(dat2$type=="distal_1"), ]
test.dat <- distal_1.dat[which(distal_1.dat$stage=="16C" | distal_1.dat$stage=="eB"), ]
wilcox.test(counts ~ stage, data=test.dat, pair=T)$p.value






