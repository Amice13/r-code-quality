#Figure S5
#Panel a. SINE analysis in differentiating ES cells. ----------------------------------------------
#load package
library(ggplot2)
library(gridExtra)
rm(list=ls())

Day0.SINE.mus.sum <- read.table(file="processed/BED/chrX.Day0_SINE.MUS.sum", sep="\t")
Day0.SINE.cast.sum <- read.table(file="processed/BED/chrX.Day0_SINE.CAST.sum", sep="\t")
Day1.SINE.mus.sum <- read.table(file="processed/BED/chrX.Day1_SINE.MUS.sum", sep="\t")
Day1.SINE.cast.sum <- read.table(file="processed/BED/chrX.Day1_SINE.CAST.sum", sep="\t")
Day2.SINE.mus.sum <- read.table(file="processed/BED/chrX.Day2_SINE.MUS.sum", sep="\t")
Day2.SINE.cast.sum <- read.table(file="processed/BED/chrX.Day2_SINE.CAST.sum", sep="\t")
Day3.SINE.mus.sum <- read.table(file="processed/BED/chrX.Day3_SINE.MUS.sum", sep="\t")
Day3.SINE.cast.sum <- read.table(file="processed/BED/chrX.Day3_SINE.CAST.sum", sep="\t")
Day4.SINE.mus.sum <- read.table(file="processed/BED/chrX.Day4_SINE.MUS.sum", sep="\t")
Day4.SINE.cast.sum <- read.table(file="processed/BED/chrX.Day4_SINE.CAST.sum", sep="\t")
Day6.SINE.mus.sum <- read.table(file="processed/BED/chrX.Day6_SINE.MUS.sum", sep="\t")
Day6.SINE.cast.sum <- read.table(file="processed/BED/chrX.Day6_SINE.CAST.sum", sep="\t")
Day8.SINE.mus.sum <- read.table(file="processed/BED/chrX.Day8_SINE.MUS.sum", sep="\t")
Day8.SINE.cast.sum <- read.table(file="processed/BED/chrX.Day8_SINE.CAST.sum", sep="\t")
Day10.SINE.mus.sum <- read.table(file="processed/BED/chrX.Day10_SINE.MUS.sum", sep="\t")
Day10.SINE.cast.sum <- read.table(file="processed/BED/chrX.Day10_SINE.CAST.sum", sep="\t")

colnames(Day0.SINE.cast.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")
colnames(Day1.SINE.cast.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")
colnames(Day2.SINE.cast.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")
colnames(Day3.SINE.cast.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")
colnames(Day4.SINE.cast.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")
colnames(Day6.SINE.cast.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")
colnames(Day8.SINE.cast.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")
colnames(Day10.SINE.cast.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")
colnames(Day0.SINE.mus.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")
colnames(Day1.SINE.mus.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")
colnames(Day2.SINE.mus.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")
colnames(Day3.SINE.mus.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")
colnames(Day4.SINE.mus.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")
colnames(Day6.SINE.mus.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")
colnames(Day8.SINE.mus.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")
colnames(Day10.SINE.mus.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")

#convert the coordinates to Mb
Day0.SINE.cast.sum[ ,c(3,4)] <- Day0.SINE.cast.sum[ ,c(3,4)]/1000000
Day1.SINE.cast.sum[ ,c(3,4)] <- Day1.SINE.cast.sum[ ,c(3,4)]/1000000
Day2.SINE.cast.sum[ ,c(3,4)] <- Day2.SINE.cast.sum[ ,c(3,4)]/1000000
Day3.SINE.cast.sum[ ,c(3,4)] <- Day3.SINE.cast.sum[ ,c(3,4)]/1000000
Day4.SINE.cast.sum[ ,c(3,4)] <- Day4.SINE.cast.sum[ ,c(3,4)]/1000000
Day6.SINE.cast.sum[ ,c(3,4)] <- Day6.SINE.cast.sum[ ,c(3,4)]/1000000
Day8.SINE.cast.sum[ ,c(3,4)] <- Day8.SINE.cast.sum[ ,c(3,4)]/1000000
Day10.SINE.cast.sum[ ,c(3,4)] <- Day10.SINE.cast.sum[ ,c(3,4)]/1000000

Day0.SINE.mus.sum[ ,c(3,4)] <- Day0.SINE.mus.sum[ ,c(3,4)]/1000000
Day1.SINE.mus.sum[ ,c(3,4)] <- Day1.SINE.mus.sum[ ,c(3,4)]/1000000
Day2.SINE.mus.sum[ ,c(3,4)] <- Day2.SINE.mus.sum[ ,c(3,4)]/1000000
Day3.SINE.mus.sum[ ,c(3,4)] <- Day3.SINE.mus.sum[ ,c(3,4)]/1000000
Day4.SINE.mus.sum[ ,c(3,4)] <- Day4.SINE.mus.sum[ ,c(3,4)]/1000000
Day6.SINE.mus.sum[ ,c(3,4)] <- Day6.SINE.mus.sum[ ,c(3,4)]/1000000
Day8.SINE.mus.sum[ ,c(3,4)] <- Day8.SINE.mus.sum[ ,c(3,4)]/1000000
Day10.SINE.mus.sum[ ,c(3,4)] <- Day10.SINE.mus.sum[ ,c(3,4)]/1000000


Day0.cast.dat <- Day0.SINE.cast.sum[ ,c(1,3,4,5,7)]
Day1.cast.dat <- Day1.SINE.cast.sum[ ,c(1,3,4,5,7)]
Day2.cast.dat <- Day2.SINE.cast.sum[ ,c(1,3,4,5,7)]
Day3.cast.dat <- Day3.SINE.cast.sum[ ,c(1,3,4,5,7)]
Day4.cast.dat <- Day4.SINE.cast.sum[ ,c(1,3,4,5,7)]
Day6.cast.dat <- Day6.SINE.cast.sum[ ,c(1,3,4,5,7)]
Day8.cast.dat <- Day8.SINE.cast.sum[ ,c(1,3,4,5,7)]
Day10.cast.dat <- Day10.SINE.cast.sum[ ,c(1,3,4,5,7)]


Day0.mus.dat <- Day0.SINE.mus.sum[ ,c(1,3,4,5,7)]
Day1.mus.dat <- Day1.SINE.mus.sum[ ,c(1,3,4,5,7)]
Day2.mus.dat <- Day2.SINE.mus.sum[ ,c(1,3,4,5,7)]
Day3.mus.dat <- Day3.SINE.mus.sum[ ,c(1,3,4,5,7)]
Day4.mus.dat <- Day4.SINE.mus.sum[ ,c(1,3,4,5,7)]
Day6.mus.dat <- Day6.SINE.mus.sum[ ,c(1,3,4,5,7)]
Day8.mus.dat <- Day8.SINE.mus.sum[ ,c(1,3,4,5,7)]
Day10.mus.dat <- Day10.SINE.mus.sum[ ,c(1,3,4,5,7)]

rm(Day0.SINE.cast.sum,Day0.SINE.mus.sum,Day1.SINE.mus.sum,Day1.SINE.cast.sum,Day2.SINE.mus.sum,Day2.SINE.cast.sum,Day3.SINE.mus.sum,Day3.SINE.cast.sum, Day4.SINE.cast.sum, Day4.SINE.mus.sum,
   Day6.SINE.cast.sum,Day6.SINE.mus.sum,Day8.SINE.mus.sum,Day8.SINE.cast.sum,Day10.SINE.mus.sum,Day10.SINE.cast.sum)

#total female SINE reads mapped to mm10 (the sum of all replicates, from the previously determined "repeat reads ratio". )
Day0.SINE.totalNumber <- 2646536
Day1.SINE.totalNumber <- 2867333
Day2.SINE.totalNumber <- 2392246
Day3.SINE.totalNumber <- 2528217
Day4.SINE.totalNumber <- 2379428
Day6.SINE.totalNumber <- 2022710
Day8.SINE.totalNumber <- 2396044
Day10.SINE.totalNumber <- 2173568



#check the allelic counts on Xi allele (mus allele)
Day0.dat <- Day0.mus.dat
Day1.dat <- Day1.mus.dat
Day2.dat <- Day2.mus.dat
Day3.dat <- Day3.mus.dat
Day4.dat <- Day4.mus.dat
Day6.dat <- Day6.mus.dat
Day8.dat <- Day8.mus.dat
Day10.dat <- Day10.mus.dat


#merge D0 AND D1
D0ToD1.SINE.raw <- merge(Day0.dat, Day1.dat, by.x="start", by.y="start", all=T)
D0ToD1.SINE.raw[is.na(D0ToD1.SINE.raw$count.x), c(3:5)] <- D0ToD1.SINE.raw[is.na(D0ToD1.SINE.raw$count.x), c(7:9)]
D0ToD1.SINE.raw[is.na(D0ToD1.SINE.raw$count.x), "count.x"] <- 0
D0ToD1.SINE.raw[is.na(D0ToD1.SINE.raw$count.y), "count.y"] <- 0
D0ToD1.SINE.raw <- D0ToD1.SINE.raw[ ,c(1,3,4,5,2,6)]
colnames(D0ToD1.SINE.raw) <- c("start","end","repeats","family","Day0","Day1")
#merge D0 to D2
D0ToD2.SINE.raw <- merge(D0ToD1.SINE.raw, Day2.dat, by.x="start", by.y="start", all=T)
rm(D0ToD1.SINE.raw)
D0ToD2.SINE.raw[is.na(D0ToD2.SINE.raw$Day0),c(2:4)] <- D0ToD2.SINE.raw[is.na(D0ToD2.SINE.raw$Day0),c(8:10)]
D0ToD2.SINE.raw[is.na(D0ToD2.SINE.raw$Day0),c("Day0","Day1")] <- 0
D0ToD2.SINE.raw[is.na(D0ToD2.SINE.raw$count),"count"] <- 0
D0ToD2.SINE.raw <- D0ToD2.SINE.raw[ ,c(1:7)]
colnames(D0ToD2.SINE.raw) <- c("start","end","repeats","family","Day0","Day1","Day2")
#merge D0 to D3
D0ToD3.SINE.raw <- merge(D0ToD2.SINE.raw, Day3.dat, by.x="start", by.y="start", all=T)
rm(D0ToD2.SINE.raw)
D0ToD3.SINE.raw[is.na(D0ToD3.SINE.raw$Day0),c(2:4)] <- D0ToD3.SINE.raw[is.na(D0ToD3.SINE.raw$Day0),c(9:11)]
D0ToD3.SINE.raw[is.na(D0ToD3.SINE.raw$Day0),c("Day0","Day1","Day2")] <- 0
D0ToD3.SINE.raw[is.na(D0ToD3.SINE.raw$count),"count"] <- 0
D0ToD3.SINE.raw <- D0ToD3.SINE.raw[ ,c(1:8)]
colnames(D0ToD3.SINE.raw) <- c("start","end","repeats","family","Day0","Day1","Day2","Day3")
#merge D0 to D4
D0ToD4.SINE.raw <- merge(D0ToD3.SINE.raw, Day4.dat, by.x="start", by.y="start", all=T)
rm(D0ToD3.SINE.raw)
D0ToD4.SINE.raw[is.na(D0ToD4.SINE.raw$Day0),c(2:4)] <- D0ToD4.SINE.raw[is.na(D0ToD4.SINE.raw$Day0),c(10:12)]
D0ToD4.SINE.raw[is.na(D0ToD4.SINE.raw$Day0),c("Day0","Day1","Day2","Day3")] <- 0
D0ToD4.SINE.raw[is.na(D0ToD4.SINE.raw$count),"count"] <- 0
D0ToD4.SINE.raw <- D0ToD4.SINE.raw[ ,c(1:9)]
colnames(D0ToD4.SINE.raw) <- c("start","end","repeats","family","Day0","Day1","Day2","Day3","Day4")
#merge D0 to D6
D0ToD6.SINE.raw <- merge(D0ToD4.SINE.raw, Day6.dat, by.x="start", by.y="start", all=T)
rm(D0ToD4.SINE.raw)
D0ToD6.SINE.raw[is.na(D0ToD6.SINE.raw$Day0),c(2:4)] <- D0ToD6.SINE.raw[is.na(D0ToD6.SINE.raw$Day0),c(11:13)]
D0ToD6.SINE.raw[is.na(D0ToD6.SINE.raw$Day0),c("Day0","Day1","Day2","Day3","Day4")] <- 0
D0ToD6.SINE.raw[is.na(D0ToD6.SINE.raw$count),"count"] <- 0
D0ToD6.SINE.raw <- D0ToD6.SINE.raw[ ,c(1:10)]
colnames(D0ToD6.SINE.raw) <- c("start","end","repeats","family","Day0","Day1","Day2","Day3","Day4","Day6")
#merge D0 to D8
D0ToD8.SINE.raw <- merge(D0ToD6.SINE.raw, Day8.dat, by.x="start", by.y="start", all=T)
rm(D0ToD6.SINE.raw)
D0ToD8.SINE.raw[is.na(D0ToD8.SINE.raw$Day0),c(2:4)] <- D0ToD8.SINE.raw[is.na(D0ToD8.SINE.raw$Day0),c(12:14)]
D0ToD8.SINE.raw[is.na(D0ToD8.SINE.raw$Day0),c("Day0","Day1","Day2","Day3","Day4","Day6")] <- 0
D0ToD8.SINE.raw[is.na(D0ToD8.SINE.raw$count),"count"] <- 0
D0ToD8.SINE.raw <- D0ToD8.SINE.raw[ ,c(1:11)]
colnames(D0ToD8.SINE.raw) <- c("start","end","repeats","family","Day0","Day1","Day2","Day3","Day4","Day6","Day8")
#merge D0 to D10
D0ToD10.SINE.raw <- merge(D0ToD8.SINE.raw, Day10.dat, by.x="start", by.y="start", all=T)
rm(D0ToD8.SINE.raw)
D0ToD10.SINE.raw[is.na(D0ToD10.SINE.raw$Day0),c(2:4)] <- D0ToD10.SINE.raw[is.na(D0ToD10.SINE.raw$Day0),c(13:15)]
D0ToD10.SINE.raw[is.na(D0ToD10.SINE.raw$Day0),c("Day0","Day1","Day2","Day3","Day4","Day6","Day8")] <- 0
D0ToD10.SINE.raw[is.na(D0ToD10.SINE.raw$count),"count"] <- 0
D0ToD10.SINE.raw <- D0ToD10.SINE.raw[ ,c(1:12)]
colnames(D0ToD10.SINE.raw) <- c("start","end","repeats","family","Day0","Day1","Day2","Day3","Day4","Day6","Day8","Day10")


#filter out SINEs of low read counts, less than 3
#D0ToD10.SINE.filtered <- D0ToD10.SINE.raw[which(rowSums(late2CToeB.SINE[ ,c(5:9)]) >3), ]


#normalize the counts using total female SINE counts in each stage
D0ToD10.SINE.norm <- data.frame(D0ToD10.SINE.raw[ ,c(1:4)],
                                Day0=D0ToD10.SINE.raw[ ,5]*1e6/Day0.SINE.totalNumber,
                                Day1=D0ToD10.SINE.raw[ ,6]*1e6/Day1.SINE.totalNumber, 
                                Day2=D0ToD10.SINE.raw[ ,7]*1e6/Day2.SINE.totalNumber,
                                Day3=D0ToD10.SINE.raw[ ,8]*1e6/Day3.SINE.totalNumber,
                                Day4=D0ToD10.SINE.raw[ ,9]*1e6/Day4.SINE.totalNumber,
                                Day6=D0ToD10.SINE.raw[ ,10]*1e6/Day6.SINE.totalNumber,
                                Day8=D0ToD10.SINE.raw[ ,11]*1e6/Day8.SINE.totalNumber,
                                Day10=D0ToD10.SINE.raw[ ,12]*1e6/Day10.SINE.totalNumber)

#N16C=late2CToeB.SINE.srt[ ,8]*1e6/sum(N16cell.comp),
#eB=late2CToeB.SINE.srt[ ,9]*1e6/sum(eB.comp))

rm(list=setdiff(ls(), c("D0ToD10.SINE.norm")))

dat <- D0ToD10.SINE.norm[ , c(1,5:12)]



#plot1 normalized CAST counts vs loci
SINE.Day0 <- ggplot(dat, aes(x=start,y=Day0)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 100)) +
  scale_x_continuous(limits = c(1,146), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.29,  colour="red",linetype="longdash") 

SINE.Day1 <- ggplot(dat, aes(x=start,y=Day1)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 100)) +
  scale_x_continuous(limits = c(1,146), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.29,  colour="red",linetype="longdash") 

SINE.Day2 <- ggplot(dat, aes(x=start,y=Day2)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 100)) +
  scale_x_continuous(limits = c(1,146), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.29,  colour="red",linetype="longdash") 

SINE.Day3 <- ggplot(dat, aes(x=start,y=Day3)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 100)) +
  scale_x_continuous(limits = c(1,146), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.29,  colour="red",linetype="longdash") 

SINE.Day4 <- ggplot(dat, aes(x=start,y=Day4)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 100)) +
  scale_x_continuous(limits = c(1,146), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.29,  colour="red",linetype="longdash") 

SINE.Day6 <- ggplot(dat, aes(x=start,y=Day6)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 100)) +
  scale_x_continuous(limits = c(1,146), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.29,  colour="red",linetype="longdash") 

SINE.Day8 <- ggplot(dat, aes(x=start,y=Day8)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 100)) +
  scale_x_continuous(limits = c(1,146), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.29,  colour="red",linetype="longdash") 

SINE.Day10 <- ggplot(dat, aes(x=start,y=Day10)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 100)) +
  scale_x_continuous(limits = c(1,146), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.29,  colour="red",linetype="longdash") 




library(gridExtra)
grid.arrange(SINE.Day0, SINE.Day1, SINE.Day2, SINE.Day3, SINE.Day4, SINE.Day6, SINE.Day8, SINE.Day10,nrow=8, ncol=1)


# Panel b. (or A-right)-----------------------------------------------------------------
#First create a linkage between SINE name and subfamily
Day0.SINE.mus.sum <- read.table(file="processed/BED/chrX.Day0_SINE.MUS.sum", sep="\t")
Day1.SINE.mus.sum <- read.table(file="processed/BED/chrX.Day1_SINE.MUS.sum", sep="\t")
Day2.SINE.mus.sum <- read.table(file="processed/BED/chrX.Day2_SINE.MUS.sum", sep="\t")
Day3.SINE.mus.sum <- read.table(file="processed/BED/chrX.Day3_SINE.MUS.sum", sep="\t")
Day4.SINE.mus.sum <- read.table(file="processed/BED/chrX.Day4_SINE.MUS.sum", sep="\t")
Day6.SINE.mus.sum <- read.table(file="processed/BED/chrX.Day6_SINE.MUS.sum", sep="\t")
Day8.SINE.mus.sum <- read.table(file="processed/BED/chrX.Day8_SINE.MUS.sum", sep="\t")
Day10.SINE.mus.sum <- read.table(file="processed/BED/chrX.Day10_SINE.MUS.sum", sep="\t")

colnames(Day0.SINE.mus.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")
colnames(Day1.SINE.mus.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")
colnames(Day2.SINE.mus.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")
colnames(Day3.SINE.mus.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")
colnames(Day4.SINE.mus.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")
colnames(Day6.SINE.mus.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")
colnames(Day8.SINE.mus.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")
colnames(Day10.SINE.mus.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")

SINE.list <- rbind(Day0.SINE.mus.sum, Day1.SINE.mus.sum,Day2.SINE.mus.sum,Day3.SINE.mus.sum,
                   Day4.SINE.mus.sum,Day6.SINE.mus.sum,Day8.SINE.mus.sum,Day10.SINE.mus.sum)

rm(Day0.SINE.mus.sum, Day1.SINE.mus.sum,Day2.SINE.mus.sum,Day3.SINE.mus.sum,
   Day4.SINE.mus.sum,Day6.SINE.mus.sum,Day8.SINE.mus.sum,Day10.SINE.mus.sum)

unique.SINE.list <- unique(SINE.list[ ,c("rep", "subfamily")])

#Take the final data from #C: D0ToD10.SINE.norm

D0ToD10.SINE.norm[ ,"subfamily"] <- unique.SINE.list[match(D0ToD10.SINE.norm$repeats, unique.SINE.list$rep), "subfamily"]

#get the SINE data for each subfamily
Alu.dat <- D0ToD10.SINE.norm[which(D0ToD10.SINE.norm$subfamily=="Alu"), ]
B2.dat <- D0ToD10.SINE.norm[which(D0ToD10.SINE.norm$subfamily=="B2"), ]
B4.dat <- D0ToD10.SINE.norm[which(D0ToD10.SINE.norm$subfamily=="B4"), ]
ID.dat <- D0ToD10.SINE.norm[which(D0ToD10.SINE.norm$subfamily=="ID"), ]
MIR.dat <- D0ToD10.SINE.norm[which(D0ToD10.SINE.norm$subfamily=="MIR"), ]

Alu.total.count <-colSums(Alu.dat[ ,c(5:12)])
B2.total.count <- colSums(B2.dat[ ,c(5:12)])
B4.total.count <- colSums(B4.dat[ ,c(5:12)])
ID.total.count <- colSums(ID.dat[ ,c(5:12)])
MIR.total.count <- colSums(MIR.dat[ ,c(5:12)])


Alu.percent <- data.frame(percent=Alu.total.count/Alu.total.count[1], subfamily="Alu", date=c("Day0","Day1","Day2","Day3","Day4","Day6","Day8","Day10"))
B2.percent <- data.frame(percent=B2.total.count/B2.total.count[1], subfamily="B2", date=c("Day0","Day1","Day2","Day3","Day4","Day6","Day8","Day10"))
B4.percent <- data.frame(percent=B4.total.count/B4.total.count[1], subfamily="B4", date=c("Day0","Day1","Day2","Day3","Day4","Day6","Day8","Day10"))
ID.percent <- data.frame(percent=ID.total.count/ID.total.count[1], subfamily="ID", date=c("Day0","Day1","Day2","Day3","Day4","Day6","Day8","Day10"))
MIR.percent <- data.frame(percent=MIR.total.count/MIR.total.count[1], subfamily="MIR", date=c("Day0","Day1","Day2","Day3","Day4","Day6","Day8","Day10"))

dat <- rbind(Alu.percent, B2.percent, B4.percent, ID.percent, MIR.percent)

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


day0.dat <- dat[which(dat$date=="Day0"), ]
subSINE.day0 <- ggplot(day0.dat, aes(x=subfamily,y=percent)) +
  geom_bar(stat="identity", fill="lightblue",size=0.5) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 1)) +
  theme_pattern

day1.dat <- dat[which(dat$date=="Day1"), ]
subSINE.day1 <- ggplot(day1.dat, aes(x=subfamily,y=percent)) +
  geom_bar(stat="identity", fill="lightblue",size=0.5) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 1)) +
  theme_pattern

day2.dat <- dat[which(dat$date=="Day2"), ]
subSINE.day2 <- ggplot(day2.dat, aes(x=subfamily,y=percent)) +
  geom_bar(stat="identity", fill="lightblue",size=0.5) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 1)) +
  theme_pattern

day3.dat <- dat[which(dat$date=="Day3"), ]
subSINE.day3 <- ggplot(day3.dat, aes(x=subfamily,y=percent)) +
  geom_bar(stat="identity", fill="lightblue",size=0.5) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 1)) +
  theme_pattern

day4.dat <- dat[which(dat$date=="Day4"), ]
subSINE.day4 <- ggplot(day4.dat, aes(x=subfamily,y=percent)) +
  geom_bar(stat="identity", fill="lightblue",size=0.5) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 1)) +
  theme_pattern

day6.dat <- dat[which(dat$date=="Day6"), ]
subSINE.day6 <- ggplot(day6.dat, aes(x=subfamily,y=percent)) +
  geom_bar(stat="identity", fill="lightblue",size=0.5) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 1)) +
  theme_pattern

day8.dat <- dat[which(dat$date=="Day8"), ]
subSINE.day8 <- ggplot(day8.dat, aes(x=subfamily,y=percent)) +
  geom_bar(stat="identity", fill="lightblue",size=0.5) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 1)) +
  theme_pattern

day10.dat <- dat[which(dat$date=="Day10"), ]
subSINE.day10 <- ggplot(day10.dat, aes(x=subfamily,y=percent)) +
  geom_bar(stat="identity", fill="lightblue",size=0.5) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 1)) +
  theme_pattern

library(gridExtra)
grid.arrange(subSINE.day0, subSINE.day1, subSINE.day2, subSINE.day3, subSINE.day4, subSINE.day6, subSINE.day8, subSINE.day10,nrow=8, ncol=1)



#Panel c. SINE analysis in tail-tip fibroblast.-----------------------------------------

library(ggplot2)
library(gridExtra)
rm(list=ls())

MEF.SINE.mus.sum <- read.table(file="processed/BED/chrX.MEF_SINE.MUS.sum", sep="\t")
MEF.SINE.cast.sum <- read.table(file="processed/BED/chrX.MEF_SINE.CAST.sum", sep="\t")

colnames(MEF.SINE.cast.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")
colnames(MEF.SINE.cast.sum) <- c("count","chr","start","end","rep","strand","family","subfamily")

#convert the coordinates to Mb
MEF.SINE.cast.sum[ ,c(3,4)] <- MEF.SINE.cast.sum[ ,c(3,4)]/1000000
MEF.SINE.mus.sum[ ,c(3,4)] <- MEF.SINE.mus.sum[ ,c(3,4)]/1000000

MEF.cast.dat <- MEF.SINE.cast.sum[ ,c(1,3,4,5,7)]
MEF.mus.dat <- MEF.SINE.mus.sum[ ,c(1,3,4,5,7)]


rm(MEF.SINE.cast.sum,MEF.SINE.mus.sum)


#check the allelic counts on Xi allele (mus allele)
MEF.dat <- MEF.mus.dat[ ,c(2,3,4,5,1)]
colnames(MEF.dat) <- c("start","end","repeats","family","Counts")


ggplot(MEF.dat, aes(x=start,y=Counts)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.6) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 75)) +
  scale_x_continuous(limits = c(1,146), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.29,  colour="red",linetype="longdash") 







#Panel d. LINE analysis in differentiating ES cells. ----------------------------------------------
#LINE
rm(list=ls())

Day0.LINE.mus.sum <- read.table(file="processed/BED/chrX.Day0_LINE.MUS.sum.bed", sep="\t")
Day0.LINE.cast.sum <- read.table(file="processed/BED/chrX.Day0_LINE.CAST.sum.bed", sep="\t")
Day1.LINE.mus.sum <- read.table(file="processed/BED/chrX.Day1_LINE.MUS.sum.bed", sep="\t")
Day1.LINE.cast.sum <- read.table(file="processed/BED/chrX.Day1_LINE.CAST.sum.bed", sep="\t")
Day2.LINE.mus.sum <- read.table(file="processed/BED/chrX.Day2_LINE.MUS.sum.bed", sep="\t")
Day2.LINE.cast.sum <- read.table(file="processed/BED/chrX.Day2_LINE.CAST.sum.bed", sep="\t")
Day3.LINE.mus.sum <- read.table(file="processed/BED/chrX.Day3_LINE.MUS.sum.bed", sep="\t")
Day3.LINE.cast.sum <- read.table(file="processed/BED/chrX.Day3_LINE.CAST.sum.bed", sep="\t")
Day4.LINE.mus.sum <- read.table(file="processed/BED/chrX.Day4_LINE.MUS.sum.bed", sep="\t")
Day4.LINE.cast.sum <- read.table(file="processed/BED/chrX.Day4_LINE.CAST.sum.bed", sep="\t")
Day6.LINE.mus.sum <- read.table(file="processed/BED/chrX.Day6_LINE.MUS.sum.bed", sep="\t")
Day6.LINE.cast.sum <- read.table(file="processed/BED/chrX.Day6_LINE.CAST.sum.bed", sep="\t")
Day8.LINE.mus.sum <- read.table(file="processed/BED/chrX.Day8_LINE.MUS.sum.bed", sep="\t")
Day8.LINE.cast.sum <- read.table(file="processed/BED/chrX.Day8_LINE.CAST.sum.bed", sep="\t")
Day10.LINE.mus.sum <- read.table(file="processed/BED/chrX.Day10_LINE.MUS.sum.bed", sep="\t")
Day10.LINE.cast.sum <- read.table(file="processed/BED/chrX.Day10_LINE.CAST.sum.bed", sep="\t")

colnames(Day0.LINE.cast.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(Day1.LINE.cast.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(Day2.LINE.cast.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(Day3.LINE.cast.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(Day4.LINE.cast.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(Day6.LINE.cast.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(Day8.LINE.cast.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(Day10.LINE.cast.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(Day0.LINE.mus.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(Day1.LINE.mus.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(Day2.LINE.mus.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(Day3.LINE.mus.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(Day4.LINE.mus.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(Day6.LINE.mus.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(Day8.LINE.mus.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(Day10.LINE.mus.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")


Day0.cast.dat <- Day0.LINE.cast.sum[ ,c(5,2,3,4,7)]
Day1.cast.dat <- Day1.LINE.cast.sum[ ,c(5,2,3,4,7)]
Day2.cast.dat <- Day2.LINE.cast.sum[ ,c(5,2,3,4,7)]
Day3.cast.dat <- Day3.LINE.cast.sum[ ,c(5,2,3,4,7)]
Day4.cast.dat <- Day4.LINE.cast.sum[ ,c(5,2,3,4,7)]
Day6.cast.dat <- Day6.LINE.cast.sum[ ,c(5,2,3,4,7)]
Day8.cast.dat <- Day8.LINE.cast.sum[ ,c(5,2,3,4,7)]
Day10.cast.dat <- Day10.LINE.cast.sum[ ,c(5,2,3,4,7)]


Day0.mus.dat <- Day0.LINE.mus.sum[ ,c(5,2,3,4,7)]
Day1.mus.dat <- Day1.LINE.mus.sum[ ,c(5,2,3,4,7)]
Day2.mus.dat <- Day2.LINE.mus.sum[ ,c(5,2,3,4,7)]
Day3.mus.dat <- Day3.LINE.mus.sum[ ,c(5,2,3,4,7)]
Day4.mus.dat <- Day4.LINE.mus.sum[ ,c(5,2,3,4,7)]
Day6.mus.dat <- Day6.LINE.mus.sum[ ,c(5,2,3,4,7)]
Day8.mus.dat <- Day8.LINE.mus.sum[ ,c(5,2,3,4,7)]
Day10.mus.dat <- Day10.LINE.mus.sum[ ,c(5,2,3,4,7)]

rm(Day0.LINE.cast.sum,Day0.LINE.mus.sum,Day1.LINE.mus.sum,Day1.LINE.cast.sum,Day2.LINE.mus.sum,Day2.LINE.cast.sum,Day3.LINE.mus.sum,Day3.LINE.cast.sum, Day4.LINE.cast.sum, Day4.LINE.mus.sum,
   Day6.LINE.cast.sum,Day6.LINE.mus.sum,Day8.LINE.mus.sum,Day8.LINE.cast.sum,Day10.LINE.mus.sum,Day10.LINE.cast.sum)

#total female LINE reads (the sum of all replicates, from the previously determined "repeat reads ratio". )
Day0.LINE.totalNumber <- 689067
Day1.LINE.totalNumber <- 902644
Day2.LINE.totalNumber <- 840344
Day3.LINE.totalNumber <- 741231
Day4.LINE.totalNumber <- 622160
Day6.LINE.totalNumber <- 497792
Day8.LINE.totalNumber <- 534972
Day10.LINE.totalNumber <- 534472



#check the allelic counts on Xi allele (mus allele)
Day0.dat <- Day0.mus.dat
Day1.dat <- Day1.mus.dat
Day2.dat <- Day2.mus.dat
Day3.dat <- Day3.mus.dat
Day4.dat <- Day4.mus.dat
Day6.dat <- Day6.mus.dat
Day8.dat <- Day8.mus.dat
Day10.dat <- Day10.mus.dat


#plot1 normalized CAST counts vs loci
LINE.Day0 <- ggplot(Day0.dat, aes(x=start/1000000,y=count*1e6/Day0.LINE.totalNumber)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 150)) +
  scale_x_continuous(limits = c(1,146), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.29,  colour="red",linetype="longdash") 

LINE.Day1 <- ggplot(Day1.dat, aes(x=start/1000000,y=count*1e6/Day1.LINE.totalNumber)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 150)) +
  scale_x_continuous(limits = c(1,146), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.29,  colour="red",linetype="longdash") 

LINE.Day2 <- ggplot(Day2.dat, aes(x=start/1000000,y=count*1e6/Day2.LINE.totalNumber)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 150)) +
  scale_x_continuous(limits = c(1,146), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.29,  colour="red",linetype="longdash") 

LINE.Day3 <- ggplot(Day3.dat, aes(x=start/1000000,y=count*1e6/Day3.LINE.totalNumber)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 150)) +
  scale_x_continuous(limits = c(1,146), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.29,  colour="red",linetype="longdash") 

LINE.Day4 <- ggplot(Day4.dat, aes(x=start/1000000,y=count*1e6/Day4.LINE.totalNumber)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 150)) +
  scale_x_continuous(limits = c(1,146), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.29,  colour="red",linetype="longdash") 

LINE.Day6 <- ggplot(Day6.dat, aes(x=start/1000000,y=count*1e6/Day6.LINE.totalNumber)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 150)) +
  scale_x_continuous(limits = c(1,146), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.29,  colour="red",linetype="longdash") 

LINE.Day8 <- ggplot(Day8.dat, aes(x=start/1000000,y=count*1e6/Day8.LINE.totalNumber)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 150)) +
  scale_x_continuous(limits = c(1,146), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.29,  colour="red",linetype="longdash") 

LINE.Day10 <- ggplot(Day10.dat, aes(x=start/1000000,y=count*1e6/Day10.LINE.totalNumber)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 150)) +
  scale_x_continuous(limits = c(1,146), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.29,  colour="red",linetype="longdash") 




library(gridExtra)
grid.arrange(LINE.Day0, LINE.Day1, LINE.Day2, LINE.Day3, LINE.Day4, LINE.Day6, LINE.Day8, LINE.Day10,nrow=8, ncol=1)





#Panel e. LTR analysis in differentiating ES cells. ----------------------------------------------
#LTR skew vs loci, in random XCI

rm(list=ls())
library(ggplot2)
library(gridExtra)


Day0.LTR.mus.sum <- read.table(file="processed/BED/chrX.Day0_LTR.MUS.sum.bed", sep="\t")
Day0.LTR.cast.sum <- read.table(file="processed/BED/chrX.Day0_LTR.CAST.sum.bed", sep="\t")
Day1.LTR.mus.sum <- read.table(file="processed/BED/chrX.Day1_LTR.MUS.sum.bed", sep="\t")
Day1.LTR.cast.sum <- read.table(file="processed/BED/chrX.Day1_LTR.CAST.sum.bed", sep="\t")
Day2.LTR.mus.sum <- read.table(file="processed/BED/chrX.Day2_LTR.MUS.sum.bed", sep="\t")
Day2.LTR.cast.sum <- read.table(file="processed/BED/chrX.Day2_LTR.CAST.sum.bed", sep="\t")
Day3.LTR.mus.sum <- read.table(file="processed/BED/chrX.Day3_LTR.MUS.sum.bed", sep="\t")
Day3.LTR.cast.sum <- read.table(file="processed/BED/chrX.Day3_LTR.CAST.sum.bed", sep="\t")
Day4.LTR.mus.sum <- read.table(file="processed/BED/chrX.Day4_LTR.MUS.sum.bed", sep="\t")
Day4.LTR.cast.sum <- read.table(file="processed/BED/chrX.Day4_LTR.CAST.sum.bed", sep="\t")
Day6.LTR.mus.sum <- read.table(file="processed/BED/chrX.Day6_LTR.MUS.sum.bed", sep="\t")
Day6.LTR.cast.sum <- read.table(file="processed/BED/chrX.Day6_LTR.CAST.sum.bed", sep="\t")
Day8.LTR.mus.sum <- read.table(file="processed/BED/chrX.Day8_LTR.MUS.sum.bed", sep="\t")
Day8.LTR.cast.sum <- read.table(file="processed/BED/chrX.Day8_LTR.CAST.sum.bed", sep="\t")
Day10.LTR.mus.sum <- read.table(file="processed/BED/chrX.Day10_LTR.MUS.sum.bed", sep="\t")
Day10.LTR.cast.sum <- read.table(file="processed/BED/chrX.Day10_LTR.CAST.sum.bed", sep="\t")

colnames(Day0.LTR.cast.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(Day1.LTR.cast.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(Day2.LTR.cast.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(Day3.LTR.cast.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(Day4.LTR.cast.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(Day6.LTR.cast.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(Day8.LTR.cast.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(Day10.LTR.cast.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(Day0.LTR.mus.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(Day1.LTR.mus.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(Day2.LTR.mus.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(Day3.LTR.mus.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(Day4.LTR.mus.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(Day6.LTR.mus.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(Day8.LTR.mus.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")
colnames(Day10.LTR.mus.sum) <- c("chr","start","end","rep","count","strand","family","subfamily")


#which(eB.LTR.cast.sum$family == "LTR?")
#which(eB.LTR.mus.sum$family == "LTR?")


#convert the coordinates to Mb
Day0.LTR.cast.sum[ ,c(2,3)] <- Day0.LTR.cast.sum[ ,c(2,3)]/1000000
Day1.LTR.cast.sum[ ,c(2,3)] <- Day1.LTR.cast.sum[ ,c(2,3)]/1000000
Day2.LTR.cast.sum[ ,c(2,3)] <- Day2.LTR.cast.sum[ ,c(2,3)]/1000000
Day3.LTR.cast.sum[ ,c(2,3)] <- Day3.LTR.cast.sum[ ,c(2,3)]/1000000
Day4.LTR.cast.sum[ ,c(2,3)] <- Day4.LTR.cast.sum[ ,c(2,3)]/1000000
Day6.LTR.cast.sum[ ,c(2,3)] <- Day6.LTR.cast.sum[ ,c(2,3)]/1000000
Day8.LTR.cast.sum[ ,c(2,3)] <- Day8.LTR.cast.sum[ ,c(2,3)]/1000000
Day10.LTR.cast.sum[ ,c(2,3)] <- Day10.LTR.cast.sum[ ,c(2,3)]/1000000

Day0.LTR.mus.sum[ ,c(2,3)] <- Day0.LTR.mus.sum[ ,c(2,3)]/1000000
Day1.LTR.mus.sum[ ,c(2,3)] <- Day1.LTR.mus.sum[ ,c(2,3)]/1000000
Day2.LTR.mus.sum[ ,c(2,3)] <- Day2.LTR.mus.sum[ ,c(2,3)]/1000000
Day3.LTR.mus.sum[ ,c(2,3)] <- Day3.LTR.mus.sum[ ,c(2,3)]/1000000
Day4.LTR.mus.sum[ ,c(2,3)] <- Day4.LTR.mus.sum[ ,c(2,3)]/1000000
Day6.LTR.mus.sum[ ,c(2,3)] <- Day6.LTR.mus.sum[ ,c(2,3)]/1000000
Day8.LTR.mus.sum[ ,c(2,3)] <- Day8.LTR.mus.sum[ ,c(2,3)]/1000000
Day10.LTR.mus.sum[ ,c(2,3)] <- Day10.LTR.mus.sum[ ,c(2,3)]/1000000



Day0.cast.dat <- Day0.LTR.cast.sum[ ,c(5,2,3,4,8)]
Day1.cast.dat <- Day1.LTR.cast.sum[ ,c(5,2,3,4,8)]
Day2.cast.dat <- Day2.LTR.cast.sum[ ,c(5,2,3,4,8)]
Day3.cast.dat <- Day3.LTR.cast.sum[ ,c(5,2,3,4,8)]
Day4.cast.dat <- Day4.LTR.cast.sum[ ,c(5,2,3,4,8)]
Day6.cast.dat <- Day6.LTR.cast.sum[ ,c(5,2,3,4,8)]
Day8.cast.dat <- Day8.LTR.cast.sum[ ,c(5,2,3,4,8)]
Day10.cast.dat <- Day10.LTR.cast.sum[ ,c(5,2,3,4,8)]

Day0.mus.dat <- Day0.LTR.mus.sum[ ,c(5,2,3,4,8)]
Day1.mus.dat <- Day1.LTR.mus.sum[ ,c(5,2,3,4,8)]
Day2.mus.dat <- Day2.LTR.mus.sum[ ,c(5,2,3,4,8)]
Day3.mus.dat <- Day3.LTR.mus.sum[ ,c(5,2,3,4,8)]
Day4.mus.dat <- Day4.LTR.mus.sum[ ,c(5,2,3,4,8)]
Day6.mus.dat <- Day6.LTR.mus.sum[ ,c(5,2,3,4,8)]
Day8.mus.dat <- Day8.LTR.mus.sum[ ,c(5,2,3,4,8)]
Day10.mus.dat <- Day10.LTR.mus.sum[ ,c(5,2,3,4,8)]

rm(Day0.LTR.mus.sum,Day0.LTR.cast.sum,
   Day1.LTR.mus.sum,Day1.LTR.cast.sum,
   Day2.LTR.mus.sum,Day2.LTR.cast.sum,
   Day3.LTR.mus.sum,Day3.LTR.cast.sum,
   Day4.LTR.mus.sum,Day4.LTR.cast.sum,
   Day6.LTR.mus.sum,Day6.LTR.cast.sum,
   Day8.LTR.mus.sum,Day8.LTR.cast.sum,
   Day10.LTR.mus.sum,Day10.LTR.cast.sum)

#total female LTR reads (the sum of all replicates, from the previously file named "repeat reads ratio". )
Day0.LTR.totalNumber <- 1306037
Day1.LTR.totalNumber <- 1581891
Day2.LTR.totalNumber <- 1368939
Day3.LTR.totalNumber <- 1450987
Day4.LTR.totalNumber <- 1278955
Day6.LTR.totalNumber <- 1039591
Day8.LTR.totalNumber <- 829387
Day10.LTR.totalNumber <- 699519


#3-1.#check the allelic counts on Xi allele (mus allele)
Day0.dat <- Day0.mus.dat
Day1.dat <- Day1.mus.dat
Day2.dat <- Day2.mus.dat
Day3.dat <- Day3.mus.dat
Day4.dat <- Day4.mus.dat
Day6.dat <- Day6.mus.dat
Day8.dat <- Day8.mus.dat
Day10.dat <- Day10.mus.dat


#merge D0 AND D1
D0ToD1.LTR.raw <- merge(Day0.dat, Day1.dat, by.x="start", by.y="start", all=T)
D0ToD1.LTR.raw[is.na(D0ToD1.LTR.raw$count.x), c(3:5)] <- D0ToD1.LTR.raw[is.na(D0ToD1.LTR.raw$count.x), c(7:9)]
D0ToD1.LTR.raw[is.na(D0ToD1.LTR.raw$count.x), "count.x"] <- 0
D0ToD1.LTR.raw[is.na(D0ToD1.LTR.raw$count.y), "count.y"] <- 0
D0ToD1.LTR.raw <- D0ToD1.LTR.raw[ ,c(1,3,4,5,2,6)]
colnames(D0ToD1.LTR.raw) <- c("start","end","repeats","subfamily","Day0","Day1")
#merge D0 to D2
D0ToD2.LTR.raw <- merge(D0ToD1.LTR.raw, Day2.dat, by.x="start", by.y="start", all=T)
rm(D0ToD1.LTR.raw)
D0ToD2.LTR.raw[is.na(D0ToD2.LTR.raw$Day0),c(2:4)] <- D0ToD2.LTR.raw[is.na(D0ToD2.LTR.raw$Day0),c(8:10)]
D0ToD2.LTR.raw[is.na(D0ToD2.LTR.raw$Day0),c("Day0","Day1")] <- 0
D0ToD2.LTR.raw[is.na(D0ToD2.LTR.raw$count),"count"] <- 0
D0ToD2.LTR.raw <- D0ToD2.LTR.raw[ ,c(1:7)]
colnames(D0ToD2.LTR.raw) <- c("start","end","repeats","subfamily","Day0","Day1","Day2")
#merge D0 to D3
D0ToD3.LTR.raw <- merge(D0ToD2.LTR.raw, Day3.dat, by.x="start", by.y="start", all=T)
rm(D0ToD2.LTR.raw)
D0ToD3.LTR.raw[is.na(D0ToD3.LTR.raw$Day0),c(2:4)] <- D0ToD3.LTR.raw[is.na(D0ToD3.LTR.raw$Day0),c(9:11)]
D0ToD3.LTR.raw[is.na(D0ToD3.LTR.raw$Day0),c("Day0","Day1","Day2")] <- 0
D0ToD3.LTR.raw[is.na(D0ToD3.LTR.raw$count),"count"] <- 0
D0ToD3.LTR.raw <- D0ToD3.LTR.raw[ ,c(1:8)]
colnames(D0ToD3.LTR.raw) <- c("start","end","repeats","subfamily","Day0","Day1","Day2","Day3")
#merge D0 to D4
D0ToD4.LTR.raw <- merge(D0ToD3.LTR.raw, Day4.dat, by.x="start", by.y="start", all=T)
rm(D0ToD3.LTR.raw)
D0ToD4.LTR.raw[is.na(D0ToD4.LTR.raw$Day0),c(2:4)] <- D0ToD4.LTR.raw[is.na(D0ToD4.LTR.raw$Day0),c(10:12)]
D0ToD4.LTR.raw[is.na(D0ToD4.LTR.raw$Day0),c("Day0","Day1","Day2","Day3")] <- 0
D0ToD4.LTR.raw[is.na(D0ToD4.LTR.raw$count),"count"] <- 0
D0ToD4.LTR.raw <- D0ToD4.LTR.raw[ ,c(1:9)]
colnames(D0ToD4.LTR.raw) <- c("start","end","repeats","subfamily","Day0","Day1","Day2","Day3","Day4")
#merge D0 to D6
D0ToD6.LTR.raw <- merge(D0ToD4.LTR.raw, Day6.dat, by.x="start", by.y="start", all=T)
rm(D0ToD4.LTR.raw)
D0ToD6.LTR.raw[is.na(D0ToD6.LTR.raw$Day0),c(2:4)] <- D0ToD6.LTR.raw[is.na(D0ToD6.LTR.raw$Day0),c(11:13)]
D0ToD6.LTR.raw[is.na(D0ToD6.LTR.raw$Day0),c("Day0","Day1","Day2","Day3","Day4")] <- 0
D0ToD6.LTR.raw[is.na(D0ToD6.LTR.raw$count),"count"] <- 0
D0ToD6.LTR.raw <- D0ToD6.LTR.raw[ ,c(1:10)]
colnames(D0ToD6.LTR.raw) <- c("start","end","repeats","subfamily","Day0","Day1","Day2","Day3","Day4","Day6")
#merge D0 to D8
D0ToD8.LTR.raw <- merge(D0ToD6.LTR.raw, Day8.dat, by.x="start", by.y="start", all=T)
rm(D0ToD6.LTR.raw)
D0ToD8.LTR.raw[is.na(D0ToD8.LTR.raw$Day0),c(2:4)] <- D0ToD8.LTR.raw[is.na(D0ToD8.LTR.raw$Day0),c(12:14)]
D0ToD8.LTR.raw[is.na(D0ToD8.LTR.raw$Day0),c("Day0","Day1","Day2","Day3","Day4","Day6")] <- 0
D0ToD8.LTR.raw[is.na(D0ToD8.LTR.raw$count),"count"] <- 0
D0ToD8.LTR.raw <- D0ToD8.LTR.raw[ ,c(1:11)]
colnames(D0ToD8.LTR.raw) <- c("start","end","repeats","subfamily","Day0","Day1","Day2","Day3","Day4","Day6","Day8")
#merge D0 to D10
D0ToD10.LTR.raw <- merge(D0ToD8.LTR.raw, Day10.dat, by.x="start", by.y="start", all=T)
rm(D0ToD8.LTR.raw)
D0ToD10.LTR.raw[is.na(D0ToD10.LTR.raw$Day0),c(2:4)] <- D0ToD10.LTR.raw[is.na(D0ToD10.LTR.raw$Day0),c(13:15)]
D0ToD10.LTR.raw[is.na(D0ToD10.LTR.raw$Day0),c("Day0","Day1","Day2","Day3","Day4","Day6","Day8")] <- 0
D0ToD10.LTR.raw[is.na(D0ToD10.LTR.raw$count),"count"] <- 0
D0ToD10.LTR.raw <- D0ToD10.LTR.raw[ ,c(1:12)]
colnames(D0ToD10.LTR.raw) <- c("start","end","repeats","subfamily","Day0","Day1","Day2","Day3","Day4","Day6","Day8","Day10")


#filter out LTRs of low read counts, less than 3
#D0ToD10.LTR.filtered <- D0ToD10.LTR.raw[which(rowSums(late2CToeB.LTR[ ,c(5:9)]) >3), ]


#normalize the counts using total female LTR counts in each stage
D0ToD10.LTR.norm <- data.frame(D0ToD10.LTR.raw[ ,c(1:4)],
                               Day0=D0ToD10.LTR.raw[ ,5]*1e6/Day0.LTR.totalNumber,
                               Day1=D0ToD10.LTR.raw[ ,6]*1e6/Day1.LTR.totalNumber, 
                               Day2=D0ToD10.LTR.raw[ ,7]*1e6/Day2.LTR.totalNumber,
                               Day3=D0ToD10.LTR.raw[ ,8]*1e6/Day3.LTR.totalNumber,
                               Day4=D0ToD10.LTR.raw[ ,9]*1e6/Day4.LTR.totalNumber,
                               Day6=D0ToD10.LTR.raw[ ,10]*1e6/Day6.LTR.totalNumber,
                               Day8=D0ToD10.LTR.raw[ ,11]*1e6/Day8.LTR.totalNumber,
                               Day10=D0ToD10.LTR.raw[ ,12]*1e6/Day10.LTR.totalNumber)

rm(list=setdiff(ls(), c("D0ToD10.LTR.norm")))

#Attention, I notice that some small LTR annotation is right next to SINEs or even in a narrow window between two SINEs.
#I searched UCSC genome browser mm10 and did not find these small LTR annotations.
#thus I think these are false annoations by RepeatMasker, the high expression should be counted on SINEs, not LTR, thus removing them.
#removed line 772, 1605, 1606, 1607

D0ToD10.LTR.norm <- D0ToD10.LTR.norm[c(-772, -1605, -1606, -1607), ]

dat <- D0ToD10.LTR.norm[ , c(1,5:12)]

#plot1 normalized CAST counts vs loci
LTR.Day0 <- ggplot(dat, aes(x=start,y=Day0)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 100)) +
  scale_x_continuous(limits = c(1,146), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.29,  colour="red",linetype="longdash") 

LTR.Day1 <- ggplot(dat, aes(x=start,y=Day1)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 100)) +
  scale_x_continuous(limits = c(1,146), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.29,  colour="red",linetype="longdash") 

LTR.Day2 <- ggplot(dat, aes(x=start,y=Day2)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 100)) +
  scale_x_continuous(limits = c(1,146), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.29,  colour="red",linetype="longdash") 

LTR.Day3 <- ggplot(dat, aes(x=start,y=Day3)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 100)) +
  scale_x_continuous(limits = c(1,146), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.29,  colour="red",linetype="longdash") 

LTR.Day4 <- ggplot(dat, aes(x=start,y=Day4)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 100)) +
  scale_x_continuous(limits = c(1,146), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.29,  colour="red",linetype="longdash") 

LTR.Day6 <- ggplot(dat, aes(x=start,y=Day6)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 100)) +
  scale_x_continuous(limits = c(1,146), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.29,  colour="red",linetype="longdash") 

LTR.Day8 <- ggplot(dat, aes(x=start,y=Day8)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 100)) +
  scale_x_continuous(limits = c(1,146), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.29,  colour="red",linetype="longdash") 

LTR.Day10 <- ggplot(dat, aes(x=start,y=Day10)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 100)) +
  scale_x_continuous(limits = c(1,146), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.29,  colour="red",linetype="longdash") 


library(gridExtra)
grid.arrange(LTR.Day0, LTR.Day1, LTR.Day2, LTR.Day3, LTR.Day4, LTR.Day6, LTR.Day8, LTR.Day10,nrow=8, ncol=1)

# Figure 5F? characterize the escaped LTR at earlyBlastocyst stage and their correlation with escapee.

LTR.Day10 <- ggplot(dat, aes(x=start,y=Day10)) +
  geom_bar(stat="identity", color="black", fill="lightblue",size=0.4) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 100)) +
  scale_x_continuous(limits = c(1,146), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140)) +
  geom_vline(xintercept=84.29,  colour="red",linetype="longdash") 




#Panel f. LTR skew by subfamily-----------------------------------------------------

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

LTR.dat <- rbind(LTR.Day0,LTR.Day1,LTR.Day2,LTR.Day3,LTR.Day4,LTR.Day6,LTR.Day8,LTR.Day10)

#remove some LTR subfamilies that were very rare or uncertain.

LTR.dat <- LTR.dat[which(LTR.dat$subfamily !="ERVK?"), ]
LTR.dat <- LTR.dat[which(LTR.dat$subfamily !="ERV1?"), ]
LTR.dat <- LTR.dat[which(LTR.dat$subfamily !="ERVL?"), ]
LTR.dat <- LTR.dat[which(LTR.dat$subfamily !="Gypsy?"), ]
LTR.dat <- LTR.dat[which(LTR.dat$subfamily !="Gypsy"), ]

LTR.dat$stage <- factor(LTR.dat$stage, levels=c("Day0","Day1","Day2","Day3","Day4","Day6","Day8","Day10"))
#convert to Xi (mus) fraction
LTR.dat$skew <- 1-LTR.dat$skew

ggplot(LTR.dat, aes(x=subfamily, y=skew, fill=stage)) +
  geom_boxplot(outlier.shape=NA, lwd=0.4, alpha=0.7) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.75), 
             aes(fill=stage), size=1, pch=21,stroke = 0.4) +
  scale_fill_manual(values=brewer.pal(n = 8, name = "Accent"))+
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




