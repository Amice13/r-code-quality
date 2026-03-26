#Fig S3

#load package
library(RColorBrewer)
library(ggplot2)

#Panel a.Fraction of 4 repeat class in X-linked repeat reads (MC).
#the read number for each class at each stage is calculated using the following:
# e.g. awk '{sum+=$1} END {print "Sum"; print sum}' chrX.4cell_SINE.CAST.sum

#4C
SINE.REF.4C <-data.frame(count=7220, allele="Xm", stage="4C", class="SINE") 
LINE.REF.4C <-data.frame(count=1039, allele="Xm", stage="4C", class="LINE") 
LTR.REF.4C <-data.frame(count=4414, allele="Xm", stage="4C", class="LTR") 
DNA.REF.4C <-data.frame(count=198, allele="Xm", stage="4C", class="DNA") 

SINE.CAST.4C <-data.frame(count=6037, allele="Xp", stage="4C", class="SINE") 
LINE.CAST.4C <-data.frame(count=771, allele="Xp", stage="4C", class="LINE") 
LTR.CAST.4C <-data.frame(count=2502, allele="Xp", stage="4C", class="LTR") 
DNA.CAST.4C <-data.frame(count=168, allele="Xp", stage="4C", class="DNA") 

#8C
SINE.REF.8C <-data.frame(count=6779, allele="Xm", stage="8C", class="SINE") 
LINE.REF.8C <-data.frame(count=751, allele="Xm", stage="8C", class="LINE") 
LTR.REF.8C <-data.frame(count=2652, allele="Xm", stage="8C", class="LTR") 
DNA.REF.8C <-data.frame(count=224, allele="Xm", stage="8C", class="DNA") 

SINE.CAST.8C <-data.frame(count=5610, allele="Xp", stage="8C", class="SINE") 
LINE.CAST.8C <-data.frame(count=648, allele="Xp", stage="8C", class="LINE") 
LTR.CAST.8C <-data.frame(count=1005, allele="Xp", stage="8C", class="LTR") 
DNA.CAST.8C <-data.frame(count=88, allele="Xp", stage="8C", class="DNA") 

#16C
SINE.REF.16C <-data.frame(count=2529, allele="Xm", stage="16C", class="SINE") 
LINE.REF.16C <-data.frame(count=418, allele="Xm", stage="16C", class="LINE") 
LTR.REF.16C <-data.frame(count=906, allele="Xm", stage="16C", class="LTR") 
DNA.REF.16C <-data.frame(count=88, allele="Xm", stage="16C", class="DNA") 

SINE.CAST.16C <-data.frame(count=2082, allele="Xp", stage="16C", class="SINE") 
LINE.CAST.16C <-data.frame(count=197, allele="Xp", stage="16C", class="LINE") 
LTR.CAST.16C <-data.frame(count=392, allele="Xp", stage="16C", class="LTR") 
DNA.CAST.16C <-data.frame(count=35, allele="Xp", stage="16C", class="DNA") 

#earlyBlast
SINE.REF.eB <-data.frame(count=2780, allele="Xm", stage="earlyBlast", class="SINE") 
LINE.REF.eB <-data.frame(count=557, allele="Xm", stage="earlyBlast", class="LINE") 
LTR.REF.eB <-data.frame(count=980, allele="Xm", stage="earlyBlast", class="LTR") 
DNA.REF.eB <-data.frame(count=112, allele="Xm", stage="earlyBlast", class="DNA") 

SINE.CAST.eB <-data.frame(count=1152, allele="Xp", stage="earlyBlast", class="SINE") 
LINE.CAST.eB <-data.frame(count=251, allele="Xp", stage="earlyBlast", class="LINE") 
LTR.CAST.eB <-data.frame(count=230, allele="Xp", stage="earlyBlast", class="LTR") 
DNA.CAST.eB <-data.frame(count=44, allele="Xp", stage="earlyBlast", class="DNA") 


Xm.dat <- rbind(SINE.REF.4C, LINE.REF.4C, LTR.REF.4C, DNA.REF.4C,
                SINE.REF.8C, LINE.REF.8C, LTR.REF.8C, DNA.REF.8C,
                SINE.REF.16C, LINE.REF.16C, LTR.REF.16C, DNA.REF.16C,
                SINE.REF.eB, LINE.REF.eB, LTR.REF.eB, DNA.REF.eB)
Xm.dat$stage <- factor(Xm.dat$stage, levels=c("4C","8C","16C","earlyBlast"))

Xp.dat <- rbind(SINE.CAST.4C, LINE.CAST.4C, LTR.CAST.4C, DNA.CAST.4C,
                SINE.CAST.8C, LINE.CAST.8C, LTR.CAST.8C, DNA.CAST.8C,
                SINE.CAST.16C, LINE.CAST.16C, LTR.CAST.16C, DNA.CAST.16C,
                SINE.CAST.eB, LINE.CAST.eB, LTR.CAST.eB, DNA.CAST.eB)

Xp.dat$stage <- factor(Xp.dat$stage, levels=c("4C","8C","16C","earlyBlast"))



ggplot(Xm.dat, aes(x=stage, y=count, fill=class)) +
  geom_col(color="black", position="fill") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        axis.title.x=element_blank(),
        #legend.text=element_text(size=12),
        legend.title=element_blank(), 
        legend.key = element_blank(),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_line(color="black"),
        axis.line.x = element_line(color="black"),
        legend.text=element_text(size = rel(1)))

ggplot(Xp.dat, aes(x=stage, y=count, fill=class)) +
  geom_col(color="black", position="fill") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        axis.title.x=element_blank(),
        #legend.text=element_text(size=12),
        legend.title=element_blank(), 
        legend.key = element_blank(),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_line(color="black"),
        axis.line.x = element_line(color="black"),
        legend.text=element_text(size = rel(1)))




#Panel h. pie chart to show the class of SINEs where the expression is detected from-------------------
#load package
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

#1. check the size distribution of SINEs hit by the unique reads.
late2C.SINE.ref.sum[ ,"size"] <- abs(late2C.SINE.ref.sum$end-late2C.SINE.ref.sum$start)
late2C.SINE.cast.sum[ ,"size"] <- abs(late2C.SINE.cast.sum$end-late2C.SINE.cast.sum$start)
N4C.SINE.ref.sum[ ,"size"] <- abs(N4C.SINE.ref.sum$end-N4C.SINE.ref.sum$start)
N4C.SINE.cast.sum[ ,"size"] <- abs(N4C.SINE.cast.sum$end-N4C.SINE.cast.sum$start)
N8C.SINE.ref.sum[ ,"size"] <- abs(N8C.SINE.ref.sum$end-N8C.SINE.ref.sum$start)
N8C.SINE.cast.sum[ ,"size"] <- abs(N8C.SINE.cast.sum$end-N8C.SINE.cast.sum$start)
N16C.SINE.ref.sum[ ,"size"] <- abs(N16C.SINE.ref.sum$end-N16C.SINE.ref.sum$start)
N16C.SINE.cast.sum[ ,"size"] <- abs(N16C.SINE.cast.sum$end-N16C.SINE.cast.sum$start)
eB.SINE.ref.sum[ ,"size"] <- abs(eB.SINE.ref.sum$end-eB.SINE.ref.sum$start)
eB.SINE.cast.sum[ ,"size"] <- abs(eB.SINE.cast.sum$end-eB.SINE.cast.sum$start)

cast.SINE.size <- rbind(data.frame(late2C.SINE.cast.sum[ ,c(8,9)], stage="late2C"),
                        data.frame(N4C.SINE.cast.sum[ ,c(8,9)], stage="4C"),
                        data.frame(N8C.SINE.cast.sum[ ,c(8,9)], stage="8C"),
                        data.frame(N16C.SINE.cast.sum[ ,c(8,9)], stage="16C"),
                        data.frame(eB.SINE.cast.sum[ ,c(8,9)], stage="earlyBlast"))

table(cast.SINE.size$subfamily)




#Panel d. determine SINE skew vs coordinates on Xa (ref).------------------
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

#total female SINE reads (the sum of all replicates, from the previously file named "MC_repeat_reads_ratio_200hits". )
late2C.SINE.totalNumber <- 1468105
N4C.SINE.totalNumber <- 1629200
N8C.SINE.totalNumber <- 1684062
N16C.SINE.totalNumber <- 727599
eB.SINE.totalNumber <- 696909

#check the allelic counts on Xm allele (ref)
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
ref.late2CToeB.SINE.norm <- data.frame(late2CToeB.SINE.srt[ ,c(1:4)],
                                       late2C=late2CToeB.SINE.srt[ ,5]*1e6/late2C.SINE.totalNumber,
                                       N4C=late2CToeB.SINE.srt[ ,6]*1e6/N4C.SINE.totalNumber, 
                                       N8C=late2CToeB.SINE.srt[ ,7]*1e6/N8C.SINE.totalNumber,
                                       N16C=late2CToeB.SINE.srt[ ,8]*1e6/N16C.SINE.totalNumber,
                                       eB=late2CToeB.SINE.srt[ ,9]*1e6/eB.SINE.totalNumber,
                                       allele="ref")


rm(list=setdiff(ls(), c("cast.late2CToeB.SINE.norm","ref.late2CToeB.SINE.norm")))


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

dat <- ref.late2CToeB.SINE.norm[ , c(1,5:9)]



#plot normalized ref(Xa) counts vs loci
SINE.4C <- ggplot(dat, aes(x=start,y=N4C,color=allele)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.5) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 150)) +
  scale_x_continuous(limits = c(1,150), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=103.46,  colour="red",linetype="longdash") 

SINE.8C <- ggplot(dat, aes(x=start,y=N8C)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.5) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 150)) +
  scale_x_continuous(limits = c(1,150), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=103.46,  colour="red",linetype="longdash") 

SINE.16C <- ggplot(dat, aes(x=start,y=N16C)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.5) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 150)) +
  scale_x_continuous(limits = c(1,150), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=103.46,  colour="red",linetype="longdash") 

SINE.eB <- ggplot(dat, aes(x=start,y=eB)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.5) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 150)) +
  scale_x_continuous(limits = c(1,150), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=103.46,  colour="red",linetype="longdash") 

library(gridExtra)
grid.arrange(SINE.4C, SINE.8C, SINE.16C, SINE.eB, nrow=4, ncol=1)



#Panel e and f. check the status of Proximal, Intermediate and Distal SINEs on the Xm

#divide all SINEs into 3 groups: (proximal (1-15), intermediate(15-57), distal(57-end)), based on the sequence conservaton from Cast to mm10.

dat[which(dat$start >= 91.78 & dat$start <= 113.19), "type" ] <- "proximal"
dat[which(dat$start > 47.89 & dat$start < 86.05), "type" ] <- "intermediate"
dat[which(dat$start > 120.91 & dat$start < 147.47), "type" ] <- "intermediate"
dat[which(dat$start < 42.53), "type" ] <- "distal"
#based on my observation in the plot above, the intermediate section on the right side of Xist should be further divided to intermediate (100-127) and distal (>127)
dat[which(dat$start > 145.46), "type"] <- "distal"

dat <- dat[complete.cases(dat), ]



#Panel e. boxplot between 4C and 8C stages for each SINE types-------------------------------------------------

#for comparison between 4C and 8C, I used original "N4_N8C.dat" (including count 0)

N4_N8C.dat <- dat[ ,c(1,3,4,7)]
N4_N8C.dat <- N4_N8C.dat[which(N4_N8C.dat[ ,2] !=0 | N4_N8C.dat[ ,3] !=0), ]
N4C.dat <- data.frame(count=N4_N8C.dat$N4C, stage="4C", type=N4_N8C.dat$type)
N8C.dat <- data.frame(count=N4_N8C.dat$N8C, stage="8C", type=N4_N8C.dat$type)
combined.dat <- rbind(N4C.dat, N8C.dat)
combined.dat$count <- log(combined.dat$count+1)
combined.dat$type <- factor(combined.dat$type, levels=c("proximal", "intermediate", "distal"))
combined.dat$stage <- factor(combined.dat$stage, levels=c("4C", "8C"))

#p-value, proximal genes between 8C and 16C
N4_N8C.proximal.dat <- combined.dat[which(combined.dat$type=="proximal"), ]
wilcox.test(count ~ stage, data=N4_N8C.proximal.dat, paired=T)$p.value

#p-value, intermediate genes between 8C and 16C
N4_N8C.inter.dat <- combined.dat[which(combined.dat$type=="intermediate"), ]
wilcox.test(count ~ stage, data=N4_N8C.inter.dat, paired=T)$p.value

#p-value, distal genes between 8C and 16C
N4_N8C.distal.dat <- combined.dat[which(combined.dat$type=="distal"), ]
wilcox.test(count ~ stage, data=N4_N8C.distal.dat, paired=T)$p.value


ggplot(combined.dat, aes(x=type,y=count, fill=stage)) +
  geom_violin() + 
  scale_fill_manual(values=c("indianred1", "skyblue2")) +
  theme_bw() +
  theme_pattern



#Panel f. --------------------------------------------------------------------
#For comparison between 8C and 16C, the "0" read counts entries in 8C or 16C were removed. because the comparison was between the expressed SINEs at two stages.
N8_N16C.dat <- dat[ ,c(1,4,5,7)]
N8_N16C.dat <- N8_N16C.dat[which(N8_N16C.dat[ ,2] !=0 | N8_N16C.dat[ ,3] !=0), ]
N8C.dat <- data.frame(count=N8_N16C.dat$N8C, stage="8C", type=N8_N16C.dat$type)
N16C.dat <- data.frame(count=N8_N16C.dat$N16C, stage="16C", type=N8_N16C.dat$type)
combined.dat <- rbind(N8C.dat, N16C.dat)
#remove the "0" count entries.
combined.dat <- combined.dat[which(combined.dat$count!=0), ]
combined.dat$count <- log(combined.dat$count)
combined.dat$type <- factor(combined.dat$type, levels=c("proximal", "intermediate", "distal"))
combined.dat$stage <- factor(combined.dat$stage, levels=c("8C", "16C"))

#p-value, proximal genes between 8C and 16C
N8_N16C.proximal.dat <- combined.dat[which(combined.dat$type=="proximal"), ]
wilcox.test(count ~ stage, data=N8_N16C.proximal.dat)$p.value

#p-value, intermediate genes between 8C and 16C
N8_N16C.inter.dat <- combined.dat[which(combined.dat$type=="intermediate"), ]
wilcox.test(count ~ stage, data=N8_N16C.inter.dat)$p.value

#p-value, distal genes between 8C and 16C
N8_N16C.distal.dat <- combined.dat[which(combined.dat$type=="distal"), ]
wilcox.test(count ~ stage, data=N8_N16C.distal.dat)$p.value


ggplot(combined.dat, aes(x=type,y=count, fill=stage)) +
  geom_violin() + 
  scale_fill_manual(values=c("skyblue2","lightgoldenrod1")) +
  theme_bw() +
  theme_pattern





#Panel g. Colocalization of escapee SINEs and escapee LTRs.--------------------------

#SINE skew vs coordinates on Xp

rm(list=ls())
library(plyr)
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

rm(list=setdiff(ls(), c("cast.late2CToeB.SINE.norm",
                        "late2C.ref.dat","N4C.ref.dat","N8C.ref.dat","N16C.ref.dat","eB.ref.dat",
                        "late2C.SINE.totalNumber","N4C.SINE.totalNumber","N8C.SINE.totalNumber","N16C.SINE.totalNumber","eB.SINE.totalNumber")))


dat <- cast.late2CToeB.SINE.norm[ , c(1,5:9)]


#determine the hotspots ("certain" loci), 
#calculate the actice SINE density on the Xi
dat1 <-dat[which(dat$eB != 0),c(1,6) ]
SINE.plot <- ggplot(dat1, aes(x=start)) +
  geom_histogram(binwidth=1.5, boundary = 2) +
  theme_bw() +
  coord_cartesian(ylim = c(0, 25)) +
  scale_x_continuous(limits = c(0,150), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.87,  colour="red",linetype="longdash") 



#LTR skew vs coordinate on Xp

library(plyr)
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


rm(list=setdiff(ls(), c("cast.late2CToeB.LTR.norm","SINE.plot",
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

#manual inspection find several mis-annoatation of repeats, and confirmed in the genome browser.
#excluding line named as 34, 126, 129,169, 293,322, 449, 453
#remove lines named as  "9","11","15","22","40","44","135","304","309","433" due to overlap with SINEs
removed.line <- c("34","83","126","129","169","293","322","449","453",
                  "9","11","15","22","40","44","135","304","309","433")
dat <- dat[-match(removed.line, rownames(dat)), ]
#remove LTR shorter than 50bp
dat[ ,11] <- (dat$end-dat$start)*1000000
dat <- dat[which(dat$V11 >=50), ]


dat <- cast.late2CToeB.LTR.norm[ , c(1,5:9)]

#calculate the actice LTR density on the Xi
dat1 <-dat[which(dat$eB != 0),c(1,6) ]
LTR.plot <- ggplot(dat1, aes(x=start)) +
  geom_histogram(binwidth=1.5, boundary = 2) +
  theme_bw() +
  coord_cartesian(ylim = c(0, 5)) +
  scale_x_continuous(limits = c(0,150), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.87,  colour="red",linetype="longdash") 


library(gridExtra)
grid.arrange(SINE.plot, LTR.plot, nrow=2, ncol=1)


#Panel i, calculate skew dynamics of LTR subfamilies

rm(list=ls())
source("MC.specific repeat analysis.R")
rm(list=setdiff(ls(), c("LTR.e2C", "LTR.l2C", "LTR.N4C","LTR.N8C","LTR.N16C","LTR.eB")))
LTR_subfamily <- read.table(file="processed/LTR_subfamily",sep="\t")
colnames(LTR_subfamily) <- c("rep","subfamily")
table(LTR_subfamily$subfamily)

#LTR.e2C[ ,"rep"] <- rownames(LTR.e2C)
#LTR.e2C[ ,"subfamily"] <- LTR_subfamily$subfamily[match(LTR.e2C$rep, LTR_subfamily$rep)]

#LTR.l2C[ ,"rep"] <- rownames(LTR.l2C)
#LTR.l2C[ ,"subfamily"] <- LTR_subfamily$subfamily[match(LTR.l2C$rep, LTR_subfamily$rep)]

LTR.N4C[ ,"rep"] <- rownames(LTR.N4C)
LTR.N4C[ ,"subfamily"] <- LTR_subfamily$subfamily[match(LTR.N4C$rep, LTR_subfamily$rep)]

LTR.N8C[ ,"rep"] <- rownames(LTR.N8C)
LTR.N8C[ ,"subfamily"] <- LTR_subfamily$subfamily[match(LTR.N8C$rep, LTR_subfamily$rep)]

LTR.N16C[ ,"rep"] <- rownames(LTR.N16C)
LTR.N16C[ ,"subfamily"] <- LTR_subfamily$subfamily[match(LTR.N16C$rep, LTR_subfamily$rep)]

LTR.eB[ ,"rep"] <- rownames(LTR.eB)
LTR.eB[ ,"subfamily"] <- LTR_subfamily$subfamily[match(LTR.eB$rep, LTR_subfamily$rep)]

LTR.dat <- rbind(LTR.N4C,LTR.N8C,LTR.N16C,LTR.eB)

#remove some LTR subfamilies that were very rare or uncertain.

LTR.dat <- LTR.dat[which(LTR.dat$subfamily !="ERVK?"), ]
LTR.dat <- LTR.dat[which(LTR.dat$subfamily !="ERV1?"), ]
LTR.dat <- LTR.dat[which(LTR.dat$subfamily !="ERVL?"), ]
LTR.dat <- LTR.dat[which(LTR.dat$subfamily !="Gypsy?"), ]
LTR.dat <- LTR.dat[which(LTR.dat$subfamily !="Gypsy"), ]

LTR.dat$stage <- factor(LTR.dat$stage, levels=c("4C","8C","16C","earlyB."))

ggplot(LTR.dat, aes(x=subfamily, y=skew, fill=stage)) +
  geom_boxplot(outlier.shape=NA, lwd=0.3) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
             aes(fill=stage), size=1.1, pch=21,stroke = 0.4) +
  scale_fill_brewer(palette = "Pastel1") +
  #scale_fill_manual(values=c("burlywood1","dodgerblue", "darkgray")) +
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


