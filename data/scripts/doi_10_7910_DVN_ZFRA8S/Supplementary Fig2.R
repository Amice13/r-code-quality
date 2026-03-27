library(ggplot2)
rm(list=ls())
#Panel a. Total repeat reads percentage in CM cross embryos.

dat <- read.table(file="processed/CM_repeat_reads_ratio_200hits.txt", sep="\t", header=T)
#Add stage
dat$stage <- unlist(lapply(strsplit(dat$sample,"-"),function(x) x[1]))
dat$stage <- factor(dat$stage, levels=c(unique(dat$stage)))

#Boxplot showing repeat reads fraction
ggplot(dat, aes(x=stage, y=percent,fill="dodgerblue2")) +
  geom_boxplot(alpha=0.6, outlier.shape=NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.7),
             size=1.7, pch=21) +
  scale_fill_manual(values="dodgerblue2") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
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


#Panel b. Percent of reads from 4 types of repeat types
LINE.dat <- data.frame(stage=dat$stage, 
                       percent=dat$LINE*100/(dat$LINE+dat$SINE+dat$LTR+dat$DNA),
                       type="LINE")
SINE.dat <- data.frame(stage=dat$stage, 
                       percent=dat$SINE*100/(dat$LINE+dat$SINE+dat$LTR+dat$DNA),
                       type="SINE")
LTR.dat <- data.frame(stage=dat$stage, 
                      percent=dat$LTR*100/(dat$LINE+dat$SINE+dat$LTR+dat$DNA),
                      type="LTR")
DNA.dat <- data.frame(stage=dat$stage, 
                      percent=dat$DNA*100/(dat$LINE+dat$SINE+dat$LTR+dat$DNA),
                      type="DNA")


LINE.mean <- 0
SINE.mean <- 0
LTR.mean <- 0
DNA.mean <- 0
Start <- 1
for (i in 1:length(table(dat$stage))) {
  End <- Start + table(dat$stage)[i] -1
  LINE.mean[i] <- mean(LINE.dat$percent[Start:End])
  SINE.mean[i] <- mean(SINE.dat$percent[Start:End])
  LTR.mean[i] <- mean(LTR.dat$percent[Start:End])
  DNA.mean[i] <- mean(DNA.dat$percent[Start:End])
  Start=End+1
}

LINE.mean.dat <- data.frame(stage=unique(dat$stage),mean_percent=LINE.mean,type="LINE")
SINE.mean.dat <- data.frame(stage=unique(dat$stage),mean_percent=SINE.mean,type="SINE")
LTR.mean.dat <- data.frame(stage=unique(dat$stage),mean_percent=LTR.mean,type="LTR")
DNA.mean.dat <- data.frame(stage=unique(dat$stage),mean_percent=DNA.mean,type="DNA")



all.repeats.dat <- rbind(LINE.mean.dat, SINE.mean.dat, LTR.mean.dat, DNA.mean.dat)


#Proportional stacked bar graph showing proportion of 4 types of repeats
ggplot(all.repeats.dat, aes(x=stage, y=mean_percent, fill=type)) +
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



#Panel d. Total repeat reads percentage in ES cross embryos.

dat <- read.table(file="processed/ES_repeat_reads_ratio_200hits.txt", sep="\t", header=T)
#Add stage
dat$stage <- unlist(lapply(strsplit(dat$sample,"-"),function(x) x[1]))
dat$stage <- factor(dat$stage, levels=c(unique(dat$stage)))

#Boxplot showing repeat reads fraction
ggplot(dat, aes(x=stage, y=percent,fill="dodgerblue2")) +
  geom_boxplot(alpha=0.6, outlier.shape=NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.7),
             size=1.7, pch=21) +
  scale_fill_manual(values="dodgerblue2") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
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


#Panel e. Percent of reads from 4 types of repeat types
LINE.dat <- data.frame(stage=dat$stage, 
                       percent=dat$LINE*100/(dat$LINE+dat$SINE+dat$LTR+dat$DNA),
                       type="LINE")
SINE.dat <- data.frame(stage=dat$stage, 
                       percent=dat$SINE*100/(dat$LINE+dat$SINE+dat$LTR+dat$DNA),
                       type="SINE")
LTR.dat <- data.frame(stage=dat$stage, 
                      percent=dat$LTR*100/(dat$LINE+dat$SINE+dat$LTR+dat$DNA),
                      type="LTR")
DNA.dat <- data.frame(stage=dat$stage, 
                      percent=dat$DNA*100/(dat$LINE+dat$SINE+dat$LTR+dat$DNA),
                      type="DNA")


LINE.mean <- 0
SINE.mean <- 0
LTR.mean <- 0
DNA.mean <- 0
Start <- 1
for (i in 1:length(table(dat$stage))) {
  End <- Start + table(dat$stage)[i] -1
  LINE.mean[i] <- mean(LINE.dat$percent[Start:End])
  SINE.mean[i] <- mean(SINE.dat$percent[Start:End])
  LTR.mean[i] <- mean(LTR.dat$percent[Start:End])
  DNA.mean[i] <- mean(DNA.dat$percent[Start:End])
  Start=End+1
}

LINE.mean.dat <- data.frame(stage=unique(dat$stage),mean_percent=LINE.mean,type="LINE")
SINE.mean.dat <- data.frame(stage=unique(dat$stage),mean_percent=SINE.mean,type="SINE")
LTR.mean.dat <- data.frame(stage=unique(dat$stage),mean_percent=LTR.mean,type="LTR")
DNA.mean.dat <- data.frame(stage=unique(dat$stage),mean_percent=DNA.mean,type="DNA")



all.repeats.dat <- rbind(LINE.mean.dat, SINE.mean.dat, LTR.mean.dat, DNA.mean.dat)


#Proportional stacked bar graph showing proportion of 4 types of repeats
ggplot(all.repeats.dat, aes(x=stage, y=mean_percent, fill=type)) +
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



#Panel i.All autosomal repeats combined, Skew analysis in XCI. Plot chrX and autosomes------------------------------------------
rm(list=ls())
source("MC.overall repeat analysis.R")
rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average",
                        "zygote.skew","zygote.chrX","zygote.combined.Data","zygote.mean.skew","zygote.XvsAuto.skew",
                        "early2cell.skew","early2cell.chrX","early2cell.combined.Data","early2cell.mean.skew","early2cell.XvsAuto.skew",
                        "late2cell.skew","late2cell.chrX","late2cell.combined.Data","late2cell.mean.skew","late2cell.XvsAuto.skew",
                        "N4cell.skew","N4cell.chrX","N4cell.combined.Data","N4cell.mean.skew","N4cell.XvsAuto.skew",
                        "N8cell.skew","N8cell.chrX","N8cell.combined.Data","N8cell.mean.skew","N8cell.XvsAuto.skew",
                        "N16cell.skew","N16cell.chrX","N16cell.combined.Data","N16cell.mean.skew","N16cell.XvsAuto.skew",
                        "earlyBlast.skew","earlyBlast.chrX","earlyBlast.combined.Data","earlyBlast.mean.skew","earlyBlast.XvsAuto.skew")))


chr1_13_X.dat <- rbind(zygote.skew, early2cell.skew, late2cell.skew, N4cell.skew, N8cell.skew, N16cell.skew, earlyBlast.skew)
chr1_13_X.dat$stage <- factor(chr1_13_X.dat$stage, levels=c("ZG", "early2C", "late2C","4C","8C","16C","eB"))

XvsAuto.dat <- rbind(zygote.XvsAuto.skew, early2cell.XvsAuto.skew, late2cell.XvsAuto.skew, N4cell.XvsAuto.skew, N8cell.XvsAuto.skew, N16cell.XvsAuto.skew, earlyBlast.XvsAuto.skew)
XvsAuto.dat$stage <- factor(XvsAuto.dat$stage, levels=c("ZG", "early2C", "late2C","4C","8C","16C","eB"))

ggplot(XvsAuto.dat, aes(x=stage, y=skewing, fill=chr)) + 
  geom_boxplot(outlier.shape=NA, alpha=.5, lwd=0.4) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
             aes(fill=chr), size=0.7, pch=21,stroke = 0.3) +
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


#4C p-value, chrX vs chr1, p=0.2631597
N4C.dat <- XvsAuto.dat[which(XvsAuto.dat$stage == "4C"), ]
wilcox.test(skewing ~ chr, data=N4C.dat)$p.value
#8C p-value, chrX vs chr13, p=0.000114162
N8C.dat <- XvsAuto.dat[which(XvsAuto.dat$stage == "8C"), ]
wilcox.test(skewing ~ chr, data=N8C.dat)$p.value
#16C p-value, chrX vs chr13, p=0.0001632
N16C.dat <- XvsAuto.dat[which(XvsAuto.dat$stage == "16C"), ]
wilcox.test(skewing ~ chr, data=N16C.dat)$p.value
#8C p-value, chrX vs chr13, p=3.71713e-15
eB.dat <- XvsAuto.dat[which(XvsAuto.dat$stage == "eB"), ]
wilcox.test(skewing ~ chr, data=eB.dat)$p.value




#Panel g. normalized Xp (Xp/Xa) repeat reads ratio between male and female----------------------------
rm(list=ls())
#Preload and process the original stat files for each stage
repeat.type.family <- read.table(file="processed/repeat_class_family", sep="\t")
#Sort the repeats based on their classes
repeat.type.family <- repeat.type.family[order(repeat.type.family[ ,2]), ]
#some TEs have duplicated names, although they belong to different repeat types. 3 TEs were removed.
repeat.type.family <- repeat.type.family[!duplicated(repeat.type.family[[1]]), ]

rownames(repeat.type.family) <- repeat.type.family$V1

##NOTE:the saved output files in all types of samples recorded ratio of CAST allele!! 

#Load early2cell raw stat files
female.sample <- c(1,4,5)
male.sample <- c(2,3,6)
RawData <- read.table(file="processed/MC.early2cell_stat.txt", sep="\t", fill=T)

RawData[which(RawData[ ,3] == "cas") , 9] <- RawData[which(RawData[ ,3] == "cas"), 5]
RawData[which(RawData[ ,3] == "ref") , 9] <- RawData[which(RawData[ ,3] == "ref"), 7] - RawData[which(RawData[ ,3] == "ref"), 5]
RawData[which(RawData[ ,3] == "cas") , 10] <- RawData[which(RawData[ ,3] == "cas"), 7] - RawData[which(RawData[ ,3] == "cas"), 5]
RawData[which(RawData[ ,3] == "ref") , 10] <- RawData[which(RawData[ ,3] == "ref"), 5]

RawData[ ,11] <- RawData[ ,9]
RawData[ ,12] <- RawData[ ,11] / RawData[ ,7]
RawData <- RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
RawData[ ,11] <- repeat.type.family[(match(RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(RawData) <- c("sample","Chr","allele","rep_name","cas","ref","p-value","ref+cas","paternal","skew.ratio","rep_type")
#filterout neutral reads from this table
RawData <- RawData[which(RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
RawData <- RawData[which(RawData[ ,8] >= 5), ]
chrX <-  RawData[which(RawData[ ,2] == "chrX"), c(1,2,5,6)]
chrX_total <- data.frame(row.names=c(1:6), Stage = rep("early2C",6))
#calculate total number of chrX allelic reads for each sample
for (i in 1:6){
  chrX_total[i,2] <-  sum(chrX[which(chrX[ ,1] == i), "cas"])
  chrX_total[i,3] <-  sum(chrX[which(chrX[ ,1] == i), "ref"])
}
chrX_total[ ,4] <- chrX_total[ ,2] / chrX_total[ ,3]
chrX_total[male.sample,5] <-"male"
chrX_total[female.sample,5] <- "female"
colnames(chrX_total) <- c("Stage","Xp","Xm", "ratio","sex")
chrX_overview <- chrX_total
rm(list=setdiff(ls(), c("repeat.type.family","chrX_overview")))
#------------------------------

#Load late2cell raw stat files
RawData <- read.table(file="processed/MC.late2cell_stat.txt", sep="\t", fill=T)
female.sample <- c(3,4,5,7,9)
male.sample <- c(2,6,8,10,11)

RawData[which(RawData[ ,3] == "cas") , 9] <- RawData[which(RawData[ ,3] == "cas"), 5]
RawData[which(RawData[ ,3] == "ref") , 9] <- RawData[which(RawData[ ,3] == "ref"), 7] - RawData[which(RawData[ ,3] == "ref"), 5]
RawData[which(RawData[ ,3] == "cas") , 10] <- RawData[which(RawData[ ,3] == "cas"), 7] - RawData[which(RawData[ ,3] == "cas"), 5]
RawData[which(RawData[ ,3] == "ref") , 10] <- RawData[which(RawData[ ,3] == "ref"), 5]

RawData[ ,11] <- RawData[ ,9]
RawData[ ,12] <- RawData[ ,11] / RawData[ ,7]
RawData <- RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
RawData[ ,11] <- repeat.type.family[(match(RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(RawData) <- c("sample","Chr","allele","rep_name","cas","ref","p-value","ref+cas","paternal","skew.ratio","rep_type")
#filterout neutral reads from this table
RawData <- RawData[which(RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
RawData <- RawData[which(RawData[ ,8] >= 5), ]
chrX <-  RawData[which(RawData[ ,2] == "chrX"), c(1,2,5,6)]

chrX_total <- data.frame(row.names=c(1:11), Stage = rep("late2C",11))
#calculate total number of chrX allelic reads for each sample
for (i in 2:11){
  chrX_total[i,2] <-  sum(chrX[which(chrX[ ,1] == i), "cas"])
  chrX_total[i,3] <-  sum(chrX[which(chrX[ ,1] == i), "ref"])
}
chrX_total[ ,4] <- chrX_total[ ,2] / chrX_total[ ,3]
chrX_total[male.sample,5] <-"male"
chrX_total[female.sample,5] <- "female"
colnames(chrX_total) <- c("Stage","Xp","Xm", "ratio","sex")
chrX_overview <- rbind(chrX_overview, chrX_total[-1, ])
rm(list=setdiff(ls(), c("repeat.type.family","chrX_overview")))

#--------------------
#Load 4cell raw stat files
female.sample <- c(4,7,12,15,16,17)
male.sample <- c(1,2,3,5,8,9,10,11,13,14,18)
RawData <- read.table(file="processed/MC.4cell_stat.txt", sep="\t", fill=T)

RawData[which(RawData[ ,3] == "cas") , 9] <- RawData[which(RawData[ ,3] == "cas"), 5]
RawData[which(RawData[ ,3] == "ref") , 9] <- RawData[which(RawData[ ,3] == "ref"), 7] - RawData[which(RawData[ ,3] == "ref"), 5]
RawData[which(RawData[ ,3] == "cas") , 10] <- RawData[which(RawData[ ,3] == "cas"), 7] - RawData[which(RawData[ ,3] == "cas"), 5]
RawData[which(RawData[ ,3] == "ref") , 10] <- RawData[which(RawData[ ,3] == "ref"), 5]

RawData[ ,11] <- RawData[ ,9]
RawData[ ,12] <- RawData[ ,11] / RawData[ ,7]
RawData <- RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
RawData[ ,11] <- repeat.type.family[(match(RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(RawData) <- c("sample","Chr","allele","rep_name","cas","ref","p-value","ref+cas","paternal","skew.ratio","rep_type")
#filterout neutral reads from this table
RawData <- RawData[which(RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
RawData <- RawData[which(RawData[ ,8] >= 5), ]
chrX <-  RawData[which(RawData[ ,2] == "chrX"), c(1,2,5,6)]

chrX_total <- data.frame(row.names=c(1:18), Stage = rep("4C",18))
#calculate total number of chrX allelic reads for each sample
for (i in 1:18){
  chrX_total[i,2] <-  sum(chrX[which(chrX[ ,1] == i), "cas"])
  chrX_total[i,3] <-  sum(chrX[which(chrX[ ,1] == i), "ref"])
}
chrX_total[ ,4] <- chrX_total[ ,2] / chrX_total[ ,3]
chrX_total[male.sample,5] <-"male"
chrX_total[female.sample,5] <- "female"
colnames(chrX_total) <- c("Stage","Xp","Xm", "ratio","sex")
chrX_overview <- rbind(chrX_overview, chrX_total[-6, ])
rm(list=setdiff(ls(), c("repeat.type.family","chrX_overview")))

#--------------------
#Load 8cell raw stat files
female.sample <- c(6,10,12,15,19,20)
male.sample <- c(7,8,9,11,13,14,16,17,18)
RawData <- read.table(file="processed/MC.8cell_stat.txt", sep="\t", fill=T)

RawData[which(RawData[ ,3] == "cas") , 9] <- RawData[which(RawData[ ,3] == "cas"), 5]
RawData[which(RawData[ ,3] == "ref") , 9] <- RawData[which(RawData[ ,3] == "ref"), 7] - RawData[which(RawData[ ,3] == "ref"), 5]
RawData[which(RawData[ ,3] == "cas") , 10] <- RawData[which(RawData[ ,3] == "cas"), 7] - RawData[which(RawData[ ,3] == "cas"), 5]
RawData[which(RawData[ ,3] == "ref") , 10] <- RawData[which(RawData[ ,3] == "ref"), 5]

RawData[ ,11] <- RawData[ ,9]
RawData[ ,12] <- RawData[ ,11] / RawData[ ,7]
RawData <- RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
RawData[ ,11] <- repeat.type.family[(match(RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(RawData) <- c("sample","Chr","allele","rep_name","cas","ref","p-value","ref+cas","paternal","skew.ratio","rep_type")
#filterout neutral reads from this table
RawData <- RawData[which(RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
RawData <- RawData[which(RawData[ ,8] >= 5), ]
chrX <-  RawData[which(RawData[ ,2] == "chrX"), c(1,2,5,6)]

chrX_total <- data.frame(row.names=c(1:20), Stage = rep("8C",20))
#calculate total number of chrX allelic reads for each sample
for (i in 1:20){
  chrX_total[i,2] <-  sum(chrX[which(chrX[ ,1] == i), "cas"])
  chrX_total[i,3] <-  sum(chrX[which(chrX[ ,1] == i), "ref"])
}
chrX_total[ ,4] <- chrX_total[ ,2] / chrX_total[ ,3]
chrX_total[male.sample,5] <-"male"
chrX_total[female.sample,5] <- "female"
colnames(chrX_total) <- c("Stage","Xp","Xm", "ratio","sex")
chrX_overview <- rbind(chrX_overview, chrX_total[-c(1:5), ])
rm(list=setdiff(ls(), c("repeat.type.family","chrX_overview")))

#--------------------
#Load 16cell raw stat files
female.sample <- c(2,3,5,6,7)
male.sample <- c(1,4,8)
RawData <- read.table(file="processed/MC.16cell_stat.txt", sep="\t", fill=T)

RawData[which(RawData[ ,3] == "cas") , 9] <- RawData[which(RawData[ ,3] == "cas"), 5]
RawData[which(RawData[ ,3] == "ref") , 9] <- RawData[which(RawData[ ,3] == "ref"), 7] - RawData[which(RawData[ ,3] == "ref"), 5]
RawData[which(RawData[ ,3] == "cas") , 10] <- RawData[which(RawData[ ,3] == "cas"), 7] - RawData[which(RawData[ ,3] == "cas"), 5]
RawData[which(RawData[ ,3] == "ref") , 10] <- RawData[which(RawData[ ,3] == "ref"), 5]

RawData[ ,11] <- RawData[ ,9]
RawData[ ,12] <- RawData[ ,11] / RawData[ ,7]
RawData <- RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
RawData[ ,11] <- repeat.type.family[(match(RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(RawData) <- c("sample","Chr","allele","rep_name","cas","ref","p-value","ref+cas","paternal","skew.ratio","rep_type")
#filterout neutral reads from this table
RawData <- RawData[which(RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
RawData <- RawData[which(RawData[ ,8] >= 5), ]
chrX <-  RawData[which(RawData[ ,2] == "chrX"), c(1,2,5,6)]

chrX_total <- data.frame(row.names=c(1:8), Stage = rep("16C",8))
#calculate total number of chrX allelic reads for each sample
for (i in 1:8){
  chrX_total[i,2] <-  sum(chrX[which(chrX[ ,1] == i), "cas"])
  chrX_total[i,3] <-  sum(chrX[which(chrX[ ,1] == i), "ref"])
}
chrX_total[ ,4] <- chrX_total[ ,2] / chrX_total[ ,3]
chrX_total[male.sample,5] <-"male"
chrX_total[female.sample,5] <- "female"
colnames(chrX_total) <- c("Stage","Xp","Xm", "ratio","sex")
chrX_overview <- rbind(chrX_overview, chrX_total)
rm(list=setdiff(ls(), c("repeat.type.family","chrX_overview")))

#--------------------
#Load earlyBlast raw stat files
female.sample <- c(1,4,5,8,10)
male.sample <- c(2,3,6,7,9,11)
RawData <- read.table(file="processed/MC.earlyBlast_stat.txt", sep="\t", fill=T)

RawData[which(RawData[ ,3] == "cas") , 9] <- RawData[which(RawData[ ,3] == "cas"), 5]
RawData[which(RawData[ ,3] == "ref") , 9] <- RawData[which(RawData[ ,3] == "ref"), 7] - RawData[which(RawData[ ,3] == "ref"), 5]
RawData[which(RawData[ ,3] == "cas") , 10] <- RawData[which(RawData[ ,3] == "cas"), 7] - RawData[which(RawData[ ,3] == "cas"), 5]
RawData[which(RawData[ ,3] == "ref") , 10] <- RawData[which(RawData[ ,3] == "ref"), 5]

RawData[ ,11] <- RawData[ ,9]
RawData[ ,12] <- RawData[ ,11] / RawData[ ,7]
RawData <- RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
RawData[ ,11] <- repeat.type.family[(match(RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(RawData) <- c("sample","Chr","allele","rep_name","cas","ref","p-value","ref+cas","paternal","skew.ratio","rep_type")
#filterout neutral reads from this table
RawData <- RawData[which(RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
RawData <- RawData[which(RawData[ ,8] >= 5), ]
chrX <-  RawData[which(RawData[ ,2] == "chrX"), c(1,2,5,6)]

chrX_total <- data.frame(row.names=c(1:11), Stage = rep("earlyB.",11))
#calculate total number of chrX allelic reads for each sample
for (i in 1:11){
  chrX_total[i,2] <-  sum(chrX[which(chrX[ ,1] == i), "cas"])
  chrX_total[i,3] <-  sum(chrX[which(chrX[ ,1] == i), "ref"])
}
chrX_total[ ,4] <- chrX_total[ ,2] / chrX_total[ ,3]
chrX_total[male.sample,5] <-"male"
chrX_total[female.sample,5] <- "female"
colnames(chrX_total) <- c("Stage","Xp","Xm", "ratio","sex")
chrX_overview <- rbind(chrX_overview, chrX_total)
rm(list=setdiff(ls(), c("repeat.type.family","chrX_overview")))

#make figure
library(ggplot2)
chrX_overview$Stage <- factor(chrX_overview$Stage, levels=c("early2C","late2C","4C","8C","16C","earlyB."))
ggplot(chrX_overview, aes(x = Stage, y = ratio, fill = sex)) +
  geom_boxplot(alpha=0.5, width=0.7) +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"), 
        text = element_text(size = 12),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 12, angle = 45, hjust = 1),
        legend.position = "right",
        legend.title = element_blank(),
        legend.key = element_blank(),
        panel.border = element_rect(fill=NA,color="black", size=1),
        legend.text=element_text(size = rel(1))) +
  scale_fill_grey(start=1, end=0.1)




#Panel h. repeat allelic determination of pure cas and pure mus sample----------------------------------------
rm(list=ls())
#Preload and process the original stat files for each stage
repeat.type.family <- read.table(file="processed/repeat_class_family", sep="\t")
#Sort the repeats based on their classes
repeat.type.family <- repeat.type.family[order(repeat.type.family[ ,2]), ]
#some TEs have duplicated names, although they belong to different repeat types. 3 TEs were removed.
repeat.type.family <- repeat.type.family[!duplicated(repeat.type.family[[1]]), ]

rownames(repeat.type.family) <- repeat.type.family$V1

auto.RawData <- read.table(file="processed/pureCasMus_stat.txt", sep="\t", fill=T)

auto.RawData[which(auto.RawData$V1=="1"), 1] <- "Pure_Mus"
auto.RawData[which(auto.RawData$V1=="2"), 1] <- "Pure_Cas"

auto.RawData[which(auto.RawData[ ,3] == "cas") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "mus") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "mus"), 7] - auto.RawData[which(auto.RawData[ ,3] == "mus"), 5]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 7] - auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "mus") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "mus"), 5]

auto.RawData[ ,11] <- auto.RawData[ ,9]
auto.RawData[ ,12] <- auto.RawData[ ,11] / auto.RawData[ ,7]
auto.RawData <- auto.RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
auto.RawData[ ,11] <- repeat.type.family[(match(auto.RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(auto.RawData) <- c("sample","Chr","allele","rep_name","cas","mus","p-value","mus+cas","paternal","skew.ratio","rep_type")

#filterout neutral reads from this table
auto.RawData <- auto.RawData[which(auto.RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
auto.RawData <- auto.RawData[which(auto.RawData[ ,8] >= 5), ]

cas.dat <- data.frame(sample=auto.RawData$sample, allele="cas", counts=auto.RawData$cas)
mus.dat <- data.frame(sample=auto.RawData$sample, allele="mus", counts=auto.RawData$mus)
dat <- rbind(mus.dat, cas.dat)
dat$counts <- as.numeric(dat$counts)

dat$sample <- factor(dat$sample, levels=c("Pure_Mus", "Pure_Cas"))

ggplot(dat,aes(x = sample, y = counts,fill = allele)) + 
  geom_bar(position = "fill",stat = "identity", alpha=0.8) +
  scale_fill_manual(values=c("#CC3333", "#0066CC")) +
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


