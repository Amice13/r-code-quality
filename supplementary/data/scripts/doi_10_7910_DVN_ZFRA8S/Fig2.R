#Fig.2
rm(list=ls())
source("MC.specific repeat analysis.R")
rm(list=setdiff(ls(), c("LINE.e2C", "LINE.l2C", "LINE.N4C","LINE.N8C","LINE.N16C","LINE.eB",
                        "cLINE.e2C", "cLINE.l2C", "cLINE.N4C","cLINE.N8C","cLINE.N16C","cLINE.eB",
                        "SINE.e2C", "SINE.l2C", "SINE.N4C","SINE.N8C","SINE.N16C","SINE.eB",
                        "LTR.e2C", "LTR.l2C", "LTR.N4C","LTR.N8C","LTR.N16C","LTR.eB")))



#A. different repeat types in skew analysis

All.repeats.dat <- rbind(LINE.e2C, LINE.l2C, LINE.N4C, LINE.N8C, LINE.N16C, LINE.eB,
                         SINE.e2C, SINE.l2C, SINE.N4C, SINE.N8C, SINE.N16C, SINE.eB,
                         LTR.e2C, LTR.l2C, LTR.N4C,LTR.N8C,LTR.N16C,LTR.eB)

All.repeats.dat$stage <- factor(All.repeats.dat$stage, levels=c("early2C", "late2C","4C","8C","16C","earlyB."))
All.repeats.dat$class <- factor(All.repeats.dat$class, levels=c("SINE", "LINE","LTR"))

#Rev2 suggested to remove the TEs that have a paternal ratio of 0 or 1. These are potentially strains specific TEs. 

All.repeats.dat.non_0 <- All.repeats.dat[which(All.repeats.dat$skew !=0 & All.repeats.dat$skew !=1), ]


ggplot(All.repeats.dat.non_0, aes(x=stage, y=skew, fill=class)) +
  geom_boxplot(outlier.shape=NA, lwd=0.4) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
             aes(fill=class), size=1, pch=21,stroke = 0.4) +
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


#B.SINE skew vs coordinates on Xp

rm(list=ls())
library(RColorBrewer)
library(ggplot2)
library(gridExtra)


late2C.SINE.ref.sum <- read.table(file="processed/BED/chrX.late2cell_SINE.REF.sum.bed", sep="\t")
late2C.SINE.cast.sum <- read.table(file="processed/BED/chrX.late2cell_SINE.CAST.sum.bed", sep="\t")
N4C.SINE.ref.sum <- read.table(file="processed/BED/chrX.4cell_SINE.REF.sum.bed", sep="\t")
N4C.SINE.cast.sum <- read.table(file="processed/BED/chrX.4cell_SINE.CAST.sum.bed", sep="\t")
N8C.SINE.ref.sum <- read.table(file="processed/BED/chrX.8cell_SINE.REF.sum.bed", sep="\t")
N8C.SINE.cast.sum <- read.table(file="processed/BED/chrX.8cell_SINE.CAST.sum.bed", sep="\t")
N16C.SINE.ref.sum <- read.table(file="processed/BED/chrX.16cell_SINE.REF.sum.bed", sep="\t")
N16C.SINE.cast.sum <- read.table(file="processed/BED/chrX.16cell_SINE.CAST.sum.bed", sep="\t")
eB.SINE.ref.sum <- read.table(file="processed/BED/chrX.earlyBlast_SINE.REF.sum.bed", sep="\t")
eB.SINE.cast.sum <- read.table(file="processed/BED/chrX.earlyBlast_SINE.CAST.sum.bed", sep="\t")

colnames(late2C.SINE.cast.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(N4C.SINE.cast.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(N8C.SINE.cast.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(N16C.SINE.cast.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(eB.SINE.cast.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(late2C.SINE.ref.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(N4C.SINE.ref.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(N8C.SINE.ref.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(N16C.SINE.ref.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(eB.SINE.ref.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")

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



late2C.cast.dat <- late2C.SINE.cast.sum[ ,c(5,2,3,4,7)]
N4C.cast.dat <- N4C.SINE.cast.sum[ ,c(5,2,3,4,7)]
N8C.cast.dat <- N8C.SINE.cast.sum[ ,c(5,2,3,4,7)]
N16C.cast.dat <- N16C.SINE.cast.sum[ ,c(5,2,3,4,7)]
eB.cast.dat <- eB.SINE.cast.sum[ ,c(5,2,3,4,7)]
late2C.ref.dat <- late2C.SINE.ref.sum[ ,c(5,2,3,4,7)]
N4C.ref.dat <- N4C.SINE.ref.sum[ ,c(5,2,3,4,7)]
N8C.ref.dat <- N8C.SINE.ref.sum[ ,c(5,2,3,4,7)]
N16C.ref.dat <- N16C.SINE.ref.sum[ ,c(5,2,3,4,7)]
eB.ref.dat <- eB.SINE.ref.sum[ ,c(5,2,3,4,7)]

rm(N4C.SINE.cast.sum,N4C.SINE.ref.sum,N8C.SINE.ref.sum,N8C.SINE.cast.sum,N16C.SINE.ref.sum,N16C.SINE.cast.sum,eB.SINE.ref.sum,eB.SINE.cast.sum, late2C.SINE.cast.sum, late2C.SINE.ref.sum)

#total female SINE reads (the sum of all replicates, from the previously file named "repeat reads ratio". )
late2C.SINE.totalNumber <- 1468105
N4C.SINE.totalNumber <- 1629200
N8C.SINE.totalNumber <- 1684062
N16C.SINE.totalNumber <- 727599
eB.SINE.totalNumber <- 696909


#3-1. check the allelic counts on Xp allele
late2C.dat <- late2C.cast.dat
N4C.dat <- N4C.cast.dat
N8C.dat <- N8C.cast.dat
N16C.dat <- N16C.cast.dat
eB.dat <- eB.cast.dat


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

#optionally, merge N4-eB and late2cell-------------------------------
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
#END---------------------------------------------------------------------

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
  coord_cartesian(ylim = c(0, 100)) +
  scale_x_continuous(limits = c(1,150), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.87,  colour="red",linetype="longdash") 

SINE.8C <- ggplot(dat, aes(x=start,y=N8C)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.5) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 100)) +
  scale_x_continuous(limits = c(1,150), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.87,  colour="red",linetype="longdash") 

SINE.16C <- ggplot(dat, aes(x=start,y=N16C)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.5) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 100)) +
  scale_x_continuous(limits = c(1,150), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.87,  colour="red",linetype="longdash") 

SINE.eB <- ggplot(dat, aes(x=start,y=eB)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.5) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 100)) +
  scale_x_continuous(limits = c(1,150), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.87,  colour="red",linetype="longdash") 

library(gridExtra)
grid.arrange(SINE.4C, SINE.8C, SINE.16C, SINE.eB, nrow=4, ncol=1)



#divide all SINEs into 3 groups: (proximal (1-15), intermediate(15-57), distal(57-end))

dat[which(abs(dat$start-84.87)<=15), "type" ] <- "proximal"
dat[which(abs(dat$start-84.87)>15 & abs(dat$start-84.87)<=57), "type" ] <- "intermediate"
dat[which(abs(dat$start-84.87)>57), "type" ] <- "distal"
#based on my observation in the plot above, the intermediate section on the right side of Xist should be further divided to intermediate (100-127) and distal (>127)
dat[which(dat$start > 127), "type"] <- "distal"




#Panel c. boxplot between 4C and 8C stages for each SINE types--------------------------------------
#For comparison between 4C and 8C, used original "N4_N8C.dat" (including count 0)
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
wilcox.test(count ~ stage, data=N4_N8C.proximal.dat, paired=T)$p.value

#p-value, intermediate genes between 4C and 8C
N4_N8C.inter.dat <- combined.dat[which(combined.dat$type=="intermediate"), ]
wilcox.test(count ~ stage, data=N4_N8C.inter.dat, paired=T)$p.value

#p-value, distal genes between 4C and 8C, 
N4_N8C.distal.dat <- combined.dat[which(combined.dat$type=="distal"), ]
wilcox.test(count ~ stage, data=N4_N8C.distal.dat, paired=T)$p.value


ggplot(combined.dat, aes(x=type,y=count, fill=stage)) +
  geom_violin() + 
  scale_fill_manual(values=c("indianred1", "skyblue2")) +
  theme_bw() +
  theme_pattern



#Panel d. used for plots -------------------------------------------
#For comparison between 8C and 16C, I used original "N8_N16C.dat" (including count 0) 
N8_N16C.dat <- dat[ ,c(1,4,5,7)]
N8_N16C.dat <- N8_N16C.dat[which(N8_N16C.dat[ ,2] !=0 | N8_N16C.dat[ ,3] !=0), ]
N8C.dat <- data.frame(count=N8_N16C.dat$N8C, stage="8C", type=N8_N16C.dat$type)
N16C.dat <- data.frame(count=N8_N16C.dat$N16C, stage="16C", type=N8_N16C.dat$type)
combined.dat <- rbind(N8C.dat, N16C.dat)
combined.dat$count <- log(combined.dat$count+1)
combined.dat$type <- factor(combined.dat$type, levels=c("proximal", "intermediate", "distal"))
combined.dat$stage <- factor(combined.dat$stage, levels=c("8C", "16C"))

#p-value, proximal genes between 8C and 16C, p=1.670242e-09
N8_N16C.proximal.dat <- combined.dat[which(combined.dat$type=="proximal"), ]
wilcox.test(count ~ stage, data=N8_N16C.proximal.dat)$p.value

#p-value, intermediate genes between 8C and 16C, p=2.999032e-08
N8_N16C.inter.dat <- combined.dat[which(combined.dat$type=="intermediate"), ]
wilcox.test(count ~ stage, data=N8_N16C.inter.dat)$p.value

#p-value, distal genes between 8C and 16C, p=1.550941e-07
N8_N16C.distal.dat <- combined.dat[which(combined.dat$type=="distal"), ]
wilcox.test(count ~ stage, data=N8_N16C.distal.dat)$p.value


ggplot(combined.dat, aes(x=type,y=count, fill=stage)) +
  geom_violin() + 
  scale_fill_manual(values=c("skyblue2","lightgoldenrod1")) +
  theme_bw() +
  theme_pattern




#Panel e heatmap of SINE skewing on chrX.-----------------------------------
rm(list=ls())
source("MC.specific repeat analysis.R")
rm(list=setdiff(ls(), c("zygote.SINE.Data","early2cell.SINE.Data","late2cell.SINE.Data","N4cell.SINE.Data",
                        "N8cell.SINE.Data","N16cell.SINE.Data","earlyBlast.SINE.Data")))

B2.SINE <- c("B2_Mm1a", "B2_Mm1t", "B2_Mm2", "B3", "B3A")
Alu.SINE <- c("B1F","B1F1","B1F2","B1_Mm","B1_Mur1","B1_Mur2","B1_Mur3","B1_Mur4","B1_Mus1",
              "B1_Mus2","PB1","PB1D10","PB1D11","PB1D7","PB1D9")
B4.SINE <- c("B4","B4A","ID_B1","RSINE1")


early2cell.SINE.Data <- early2cell.SINE.Data[-23, ]
SINE.Data <- cbind(zygote.SINE.Data,early2cell.SINE.Data,late2cell.SINE.Data,N4cell.SINE.Data,
                        N8cell.SINE.Data,N16cell.SINE.Data,earlyBlast.SINE.Data)


SINE.names <- c(Alu.SINE, B2.SINE, B4.SINE)

chrX.SINE.Data <- SINE.Data[,seq(from=20, to=140, by=20)]

chrX.SINE.Data <- chrX.SINE.Data[SINE.names, ]
pheatmap(1-chrX.SINE.Data, 
         cluster_rows = F, 
         cluster_cols = F,
         show_rownames = T, 
         show_colnames = T, 
         na_col = "grey",
         border_color="NA")








#Panel f Distribution of SINE loci marked in (e) across the entire Xp (cas allele). ------------------------
rm(list=ls())
Mm1a <- read.table(file="processed/BED/chrX.earlyBlast_SINE_B2_Mm1a.CAST.sum.bed", sep="\t")[ ,c(2,4)]
Mm1t <- read.table(file="processed/BED/chrX.earlyBlast_SINE_B2_Mm1t.CAST.sum.bed", sep="\t")[ ,c(2,4)]
Mm2 <- read.table(file="processed/BED/chrX.earlyBlast_SINE_B2_Mm2.CAST.sum.bed", sep="\t")[ ,c(2,4)]
B3 <- read.table(file="processed/BED/chrX.earlyBlast_SINE_B3.CAST.sum.bed", sep="\t")[ ,c(2,4)]
B3A <- read.table(file="processed/BED/chrX.earlyBlast_SINE_B3A.CAST.sum.bed", sep="\t")[ ,c(2,4)]
PB1D9 <- read.table(file="processed/BED/chrX.earlyBlast_SINE_PB1D9.CAST.sum.bed", sep="\t")[ ,c(2,4)]


Mm1a[ ,1] <- Mm1a[ ,1]/1000000
Mm1t[ ,1] <- Mm1t[ ,1]/1000000
Mm2[ ,1] <- Mm2[ ,1]/1000000
B3[ ,1] <- B3[ ,1]/1000000
B3A[ ,1] <- B3A[ ,1]/1000000
PB1D9[ ,1] <- PB1D9[ ,1]/1000000


Mm1t.plot <- ggplot(Mm1t, aes(x=V2))+
  geom_histogram(binwidth =1)+
  theme_bw() +
  scale_y_continuous(limits = c(0,8), breaks = c(0,2,4,6,8)) +
  scale_x_continuous(limits = c(1,150), expand = c(0,0), breaks = c(20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.87,  colour="red",linetype="longdash") 

Mm1a.plot <- ggplot(Mm1a, aes(x=V2))+
  geom_histogram(binwidth =1)+
  theme_bw() +
  scale_y_continuous(limits = c(0,8), breaks = c(0,2,4,6,8)) +
  scale_x_continuous(limits = c(1,150), expand = c(0,0), breaks = c(20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.87,  colour="red",linetype="longdash") 


Mm2.plot <- ggplot(Mm2, aes(x=V2))+
  geom_histogram(binwidth =1)+
  theme_bw() +
  scale_y_continuous(limits = c(0,8), breaks = c(0,2,4,6,8)) +
  scale_x_continuous(limits = c(1,150), expand = c(0,0), breaks = c(20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.87,  colour="red",linetype="longdash") 

B3.plot <- ggplot(B3, aes(x=V2))+
  geom_histogram(binwidth =1)+
  theme_bw() +
  scale_y_continuous(limits = c(0,8), breaks = c(0,2,4,6,8)) +
  scale_x_continuous(limits = c(1,150), expand = c(0,0), breaks = c(20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.87,  colour="red",linetype="longdash") 

B3A.plot <- ggplot(B3A, aes(x=V2))+
  geom_histogram(binwidth =1)+
  theme_bw() +
  scale_y_continuous(limits = c(0,8), breaks = c(0,2,4,6,8)) +
  scale_x_continuous(limits = c(1,150), expand = c(0,0), breaks = c(20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.87,  colour="red",linetype="longdash") 

PB1D9.plot <- ggplot(PB1D9, aes(x=V2))+
  geom_histogram(binwidth =1)+
  theme_bw() +
  scale_y_continuous(limits = c(0,8), breaks = c(0,2,4,6,8)) +
  scale_x_continuous(limits = c(1,150), expand = c(0,0), breaks = c(20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.87,  colour="red",linetype="longdash") 

library(gridExtra)
grid.arrange(Mm1a.plot, Mm1t.plot, Mm2.plot, B3.plot, B3A.plot, PB1D9.plot, nrow=6, ncol=1)




#Panel g calculate percent of SINEs in escapee region----------------------
count.escapee.region <- data.frame(escapee.region=c(9,17,47,9,5,7), 
                                   non.escapee.region=c(11,11,24,10,6,2), 
                                   family=c("Mm1a", "Mm1t", "Mm2", "B3", "B3A", "PB1D9"))

count.escapee.region[ ,"percent"] <- count.escapee.region$escapee.region / (count.escapee.region$escapee.region + count.escapee.region$non.escapee.region)
count.escapee.region$family <- factor(count.escapee.region$family, levels=c("PB1D9","Mm1a","Mm1t","Mm2","B3","B3A"))

ggplot(count.escapee.region, aes(x=family, y=percent)) +
  geom_bar(stat="identity", width=0.5, fill="dodgerblue2") +
  theme_bw() +
  #scale_y_continuous(limits = c(0.2,0.8),breaks = c(0.2,0.4,0.6,0.8))+
  coord_cartesian(ylim = c(0.2, 0.8)) +
  theme(axis.text.x = element_text(size=12, angle=45),
        axis.title=element_text(size=12),
        axis.title.x=element_blank(),
        #legend.text=element_text(size=12),
        legend.title=element_blank(), 
        legend.key = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_line(color="black"),
        axis.line.x = element_line(color="black"),
        legend.text=element_text(size = rel(1)))





#Panel h. LTR skew vs coordinates on Xp------------------------------------------

rm(list=ls())
library(RColorBrewer)
library(ggplot2)
library(gridExtra)


late2C.LTR.ref.sum <- read.table(file="processed/BED/chrX.late2cell_LTR.REF.sum.bed", sep="\t")
late2C.LTR.cast.sum <- read.table(file="processed/BED/chrX.late2cell_LTR.CAST.sum.bed", sep="\t")
N4C.LTR.ref.sum <- read.table(file="processed/BED/chrX.4cell_LTR.REF.sum.bed", sep="\t")
N4C.LTR.cast.sum <- read.table(file="processed/BED/chrX.4cell_LTR.CAST.sum.bed", sep="\t")
N8C.LTR.ref.sum <- read.table(file="processed/BED/chrX.8cell_LTR.REF.sum.bed", sep="\t")
N8C.LTR.cast.sum <- read.table(file="processed/BED/chrX.8cell_LTR.CAST.sum.bed", sep="\t")
N16C.LTR.ref.sum <- read.table(file="processed/BED/chrX.16cell_LTR.REF.sum.bed", sep="\t")
N16C.LTR.cast.sum <- read.table(file="processed/BED/chrX.16cell_LTR.CAST.sum.bed", sep="\t")
eB.LTR.ref.sum <- read.table(file="processed/BED/chrX.earlyBlast_LTR.REF.sum.bed", sep="\t")
eB.LTR.cast.sum <- read.table(file="processed/BED/chrX.earlyBlast_LTR.CAST.sum.bed", sep="\t")

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

which(eB.LTR.cast.sum$family == "LTR?")
which(eB.LTR.ref.sum$family == "LTR?")


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

#total female LTR reads (the sum of all replicates, from the previously file named "repeat reads ratio". )
late2C.LTR.totalNumber <- 2788505
N4C.LTR.totalNumber <- 1242929
N8C.LTR.totalNumber <- 534504
N16C.LTR.totalNumber <- 284866
eB.LTR.totalNumber <- 226293


#check the allelic counts on Xp allele
late2C.dat <- late2C.cast.dat
N4C.dat <- N4C.cast.dat
N8C.dat <- N8C.cast.dat
N16C.dat <- N16C.cast.dat
eB.dat <- eB.cast.dat


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
                                       allele="cas")


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

#manual inspection find several misannoatation of repeats, and confirmed in the genome browser.
#excluding line named as 34, 126, 129,169, 293,322, 449, 453
#remove lines named as  "9","11","15","22","40","44","135","304","309","433" due to overlap with SINEs
removed.line <- c("34","83","126","129","169","293","322","449","453",
                  "9","11","15","22","40","44","135","304","309","433")
dat <- dat[-match(removed.line, rownames(dat)), ]
#remove LTR shorter than 50bp
dat[ ,11] <- (dat$end-dat$start)*1000000
dat <- dat[which(dat$V11 >=50), ]



#plot total normalized CAST counts vs loci
LTR.4C <- ggplot(dat, aes(x=start,y=N4C)) +
  geom_bar(stat="identity", color="black", fill="lightblue",linewidth=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 150)) +
  scale_x_continuous(limits = c(1,150), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.87,  colour="red",linetype="longdash") 

LTR.8C <- ggplot(dat, aes(x=start,y=N8C)) +
  geom_bar(stat="identity", color="black", fill="lightblue",linewidth=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 150)) +
  scale_x_continuous(limits = c(1,150), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.87,  colour="red",linetype="longdash") 

LTR.16C <- ggplot(dat, aes(x=start,y=N16C)) +
  geom_bar(stat="identity", color="black", fill="lightblue",linewidth=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 150)) +
  scale_x_continuous(limits = c(1,150), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.87,  colour="red",linetype="longdash") 

LTR.eB <- ggplot(dat, aes(x=start,y=eB)) +
  geom_bar(stat="identity", color="black", fill="lightblue",linewidth=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 150)) +
  scale_x_continuous(limits = c(1,150), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.87,  colour="red",linetype="longdash") 

library(gridExtra)
grid.arrange(LTR.4C, LTR.8C, LTR.16C, LTR.eB, nrow=4, ncol=1)



#Panel i, split chrX into two region: proximal and distal, and check LTR silencing on these two regions.---------------
dat[which(dat$start-84.87 > -49.87), "type" ] <- "proximal"  #loci > 35kb, and < 110
dat[which(dat$start-84.87 < 25.13), "type" ] <- "proximal"
dat[which(dat$start-84.87 < -64.87), "type" ] <- "distal"  #loci <= 20kb
dat[which(dat$start-84.87 > 25.13), "type" ] <- "distal" #loci >= 110kb


#For comparison between 4C and 8C, I used original "N4_N8C.dat" (including count 0) 
N4_N8C.dat <- dat[ ,c(1,6,7,12)]
N4_N8C.dat <- N4_N8C.dat[which(N4_N8C.dat[ ,2] !=0 | N4_N8C.dat[ ,3] !=0), ]
N4C.dat <- data.frame(count=N4_N8C.dat$N4C, stage="4C", type=N4_N8C.dat$type)
N8C.dat <- data.frame(count=N4_N8C.dat$N8C, stage="8C", type=N4_N8C.dat$type)
combined.dat <- rbind(N4C.dat, N8C.dat)
combined.dat$count <- log(combined.dat$count+1)
combined.dat$type <- factor(combined.dat$type, levels=c("proximal", "distal"))
combined.dat$stage <- factor(combined.dat$stage, levels=c("4C", "8C"))

#p-value, proximal genes between 4C and 8C
N4_N8C.proximal.dat <- combined.dat[which(combined.dat$type=="proximal"), ]
wilcox.test(count ~ stage, data=N4_N8C.proximal.dat,paired=T)$p.value

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



#Panel j. used for plots--------------------------------------------------------------------------------
#For comparison between 8C and 16C, I used original "N8_N16C.dat" (including count 0) 
N8_N16C.dat <- dat[ ,c(1,7,8,12)]
N8_N16C.dat <- N8_N16C.dat[which(N8_N16C.dat[ ,2] !=0 | N8_N16C.dat[ ,3] !=0), ]
N8C.dat <- data.frame(count=N8_N16C.dat$N8C, stage="8C", type=N8_N16C.dat$type)
N16C.dat <- data.frame(count=N8_N16C.dat$N16C, stage="16C", type=N8_N16C.dat$type)
combined.dat <- rbind(N8C.dat, N16C.dat)
combined.dat$count <- log(combined.dat$count+1)
combined.dat$type <- factor(combined.dat$type, levels=c("proximal", "distal"))
combined.dat$stage <- factor(combined.dat$stage, levels=c("8C", "16C"))

#p-value, proximal genes between 8C and 16C
N8_N16C.proximal.dat <- combined.dat[which(combined.dat$type=="proximal"), ]
wilcox.test(count ~ stage, data=N8_N16C.proximal.dat, paired=T)$p.value


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







