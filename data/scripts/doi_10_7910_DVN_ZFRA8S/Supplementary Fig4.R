#Fig.S4

#Panel a. All repeats combined, skew analysis during XCI in CM cross embryos

rm(list=ls())
source("CM.overall repeat analysis.R")
rm(list=setdiff(ls(), c("repeat.type.family", "Chr_Average", "X.chr_Average","Auto.X_Average",
                        "late2cell.skew","late2cell.chrX","late2cell.combined.Data","late2cell.mean.skew","late2cell.XvsAuto.skew",
                        "N4cell.skew","N4cell.chrX","N4cell.combined.Data","N4cell.mean.skew","N4cell.XvsAuto.skew",
                        "N8cell.skew","N8cell.chrX","N8cell.combined.Data","N8cell.mean.skew","N8cell.XvsAuto.skew",
                        "N16cell.skew","N16cell.chrX","N16cell.combined.Data","N16cell.mean.skew","N16cell.XvsAuto.skew",
                        "earlyBlast.skew","earlyBlast.chrX","earlyBlast.combined.Data","earlyBlast.mean.skew","earlyBlast.XvsAuto.skew")))


chr1_13_X.dat <- rbind(late2cell.skew, N4cell.skew, N8cell.skew, N16cell.skew, earlyBlast.skew)
chr1_13_X.dat$stage <- factor(chr1_13_X.dat$stage, levels=c("late2C","4C","8C","16C","eB"))
chr1_13_X.dat$chr <- factor(chr1_13_X.dat$chr, levels=c("chrX", "chr1", "chr13"))
chr1_13_X.dat$skewing <- 1-chr1_13_X.dat$skewing

# plot chr1, 13 and chrX
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


#8C p-value, chrX vs chr1, p=0.007667151
N8C.Xvs1.dat <- N8cell.skew[which(N8cell.skew$chr != "chr13"), ]
wilcox.test(skewing ~ chr, data=N8C.Xvs1.dat)$p.value
#8C p-value, chrX vs chr13, p=0.0002842275
N8C.Xvs13.dat <- N8cell.skew[which(N8cell.skew$chr != "chr1"), ]
wilcox.test(skewing ~ chr, data=N8C.Xvs13.dat)$p.value


#16C p-value, chrX vs chr1, p=0.0003793345
N16C.Xvs1.dat <- N16cell.skew[which(N16cell.skew$chr != "chr13"), ]
wilcox.test(skewing ~ chr, data=N16C.Xvs1.dat)$p.value
#16C p-value, chrX vs chr13, p=0.001659238
N16C.Xvs13.dat <- N16cell.skew[which(N16cell.skew$chr != "chr1"), ]
wilcox.test(skewing ~ chr, data=N16C.Xvs13.dat)$p.value

#eB p-value, chrX vs chr13, p=0.0005403701
eB.Xvs13.dat <- earlyBlast.skew[which(earlyBlast.skew$chr != "chr1"), ]
wilcox.test(skewing ~ chr, data=eB.Xvs13.dat)$p.value
#eB p-value, chrX vs chr1, p=0.01352453
eB.Xvs1.dat <- earlyBlast.skew[which(earlyBlast.skew$chr != "chr13"), ]
wilcox.test(skewing ~ chr, data=eB.Xvs1.dat)$p.value




#Panel b. Gene Skewing analyses in MC and CM embryos.-----------------------------------
source("Gene_skewing_density_analyses.R")
rm(list=setdiff(ls(), c("GeneInfo",
                        "MC.chr13.skew.Mean.N4C","MC.chr13.skew.Mean.N8C","MC.chr13.skew.Mean.N16C","MC.chr13.skew.Mean.eB",
                        "MC.chrX.skew.Mean.N4C","MC.chrX.skew.Mean.N8C","MC.chrX.skew.Mean.N16C","MC.chrX.skew.Mean.eB",
                        "CM.chrX.skew.Mean.N4C","CM.chrX.skew.Mean.N8C","CM.chrX.skew.Mean.N16C","CM.chrX.skew.Mean.eB",
                        "CM.chr13.skew.Mean.N4C","CM.chr13.skew.Mean.N8C","CM.chr13.skew.Mean.N16C","CM.chr13.skew.Mean.eB")))


theme_pattern <- theme(axis.text=element_text(size=12),
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


#4C
MC.chr13.dat <- data.frame(skew=MC.chr13.skew.Mean.N4C$skew, chr="chr13", cross="MC")
MC.chrX.dat <- data.frame(skew=MC.chrX.skew.Mean.N4C$skew, chr="chrX", cross="MC")
CM.chr13.dat <- data.frame(skew=CM.chr13.skew.Mean.N4C$skew, chr="chr13", cross="CM")
CM.chrX.dat <- data.frame(skew=CM.chrX.skew.Mean.N4C$skew, chr="chrX", cross="CM")

chrX.N4C.dat <- rbind(CM.chrX.dat, MC.chrX.dat)
chr13.N4C.dat <- rbind(CM.chr13.dat, MC.chr13.dat)


chrX.cdat <- ddply(chrX.N4C.dat, "cross", summarise, skewing.mean=mean(skew))
chr13.cdat <- ddply(chr13.N4C.dat, "cross", summarise, skewing.mean=mean(skew))

chrX.4C.Plot <- ggplot(chrX.N4C.dat, aes(x=skew, fill=cross)) + 
  geom_density(alpha=0.45) + 
  theme_bw() +
  xlim(0, 1) +
  theme_pattern + 
  theme(axis.title.y=element_blank(),
        legend.position = c(0.8, 0.95)) +  
  geom_vline(data=chrX.cdat, aes(xintercept=skewing.mean,  colour=cross),linetype="dashed", size=0.5)
#pvalue, p=0.002268336
wilcox.test(skew ~ cross, data=chrX.N4C.dat)$p.value

chr13.4C.Plot <- ggplot(chr13.N4C.dat, aes(x=skew, fill=cross)) + 
  geom_density(alpha=0.45) + 
  theme_bw() +
  xlim(0, 1) +
  theme_pattern + 
  theme(axis.title.y=element_blank(),
        legend.position = c(0.8, 0.95)) +  
  geom_vline(data=chr13.cdat, aes(xintercept=skewing.mean,  colour=cross),linetype="dashed", size=0.5)
#pvalue, p=0.2676539
wilcox.test(skew ~ cross, data=chr13.N4C.dat)$p.value


#8C
MC.chr13.dat <- data.frame(skew=MC.chr13.skew.Mean.N8C$skew, chr="chr13", cross="MC")
MC.chrX.dat <- data.frame(skew=MC.chrX.skew.Mean.N8C$skew, chr="chrX", cross="MC")
CM.chr13.dat <- data.frame(skew=CM.chr13.skew.Mean.N8C$skew, chr="chr13", cross="CM")
CM.chrX.dat <- data.frame(skew=CM.chrX.skew.Mean.N8C$skew, chr="chrX", cross="CM")

chrX.N8C.dat <- rbind(CM.chrX.dat, MC.chrX.dat)
chr13.N8C.dat <- rbind(CM.chr13.dat, MC.chr13.dat)


chrX.cdat <- ddply(chrX.N8C.dat, "cross", summarise, skewing.mean=mean(skew))
chr13.cdat <- ddply(chr13.N8C.dat, "cross", summarise, skewing.mean=mean(skew))

chrX.8C.Plot <- ggplot(chrX.N8C.dat, aes(x=skew, fill=cross)) + 
  geom_density(alpha=0.45) + 
  theme_bw() +
  xlim(0, 1) +
  theme_pattern + 
  theme(axis.title.y=element_blank(),
        legend.position = c(0.8, 0.95)) +  
  geom_vline(data=chrX.cdat, aes(xintercept=skewing.mean,  colour=cross),linetype="dashed", size=0.5)
#pvalue, p=0.5564738
wilcox.test(skew ~ cross, data=chrX.N8C.dat)$p.value

chr13.8C.Plot <- ggplot(chr13.N8C.dat, aes(x=skew, fill=cross)) + 
  geom_density(alpha=0.45) + 
  theme_bw() +
  xlim(0, 1) +
  theme_pattern + 
  theme(axis.title.y=element_blank(),
        legend.position = c(0.8, 0.95)) +  
  geom_vline(data=chr13.cdat, aes(xintercept=skewing.mean,  colour=cross),linetype="dashed", size=0.5)
#pvalue, p=0.8116733
wilcox.test(skew ~ cross, data=chr13.N8C.dat)$p.value


#16C
MC.chr13.dat <- data.frame(skew=MC.chr13.skew.Mean.N16C$skew, chr="chr13", cross="MC")
MC.chrX.dat <- data.frame(skew=MC.chrX.skew.Mean.N16C$skew, chr="chrX", cross="MC")
CM.chr13.dat <- data.frame(skew=CM.chr13.skew.Mean.N16C$skew, chr="chr13", cross="CM")
CM.chrX.dat <- data.frame(skew=CM.chrX.skew.Mean.N16C$skew, chr="chrX", cross="CM")

chrX.N16C.dat <- rbind(CM.chrX.dat, MC.chrX.dat)
chr13.N16C.dat <- rbind(CM.chr13.dat, MC.chr13.dat)


chrX.cdat <- ddply(chrX.N16C.dat, "cross", summarise, skewing.mean=mean(skew))
chr13.cdat <- ddply(chr13.N16C.dat, "cross", summarise, skewing.mean=mean(skew))

chrX.16C.Plot <- ggplot(chrX.N16C.dat, aes(x=skew, fill=cross)) + 
  geom_density(alpha=0.45) + 
  theme_bw() +
  xlim(0, 1) +
  theme_pattern + 
  theme(axis.title.y=element_blank(),
        legend.position = c(0.8, 0.95)) +  
  geom_vline(data=chrX.cdat, aes(xintercept=skewing.mean,  colour=cross),linetype="dashed", size=0.5)
#pvalue, p=0.0001886946
wilcox.test(skew ~ cross, data=chrX.N16C.dat)$p.value


chr13.16C.Plot <- ggplot(chr13.N16C.dat, aes(x=skew, fill=cross)) + 
  geom_density(alpha=0.45) + 
  theme_bw() +
  xlim(0, 1) +
  theme_pattern + 
  theme(axis.title.y=element_blank(),
        legend.position = c(0.8, 0.95)) +  
  geom_vline(data=chr13.cdat, aes(xintercept=skewing.mean,  colour=cross),linetype="dashed", size=0.5)

#pvalue, p=0.335578
wilcox.test(skew ~ cross, data=chr13.N16C.dat)$p.value

#eB
MC.chr13.dat <- data.frame(skew=MC.chr13.skew.Mean.eB$skew, chr="chr13", cross="MC")
MC.chrX.dat <- data.frame(skew=MC.chrX.skew.Mean.eB$skew, chr="chrX", cross="MC")
CM.chr13.dat <- data.frame(skew=CM.chr13.skew.Mean.eB$skew, chr="chr13", cross="CM")
CM.chrX.dat <- data.frame(skew=CM.chrX.skew.Mean.eB$skew, chr="chrX", cross="CM")

chrX.eB.dat <- rbind(CM.chrX.dat, MC.chrX.dat)
chr13.eB.dat <- rbind(CM.chr13.dat, MC.chr13.dat)


chrX.cdat <- ddply(chrX.eB.dat, "cross", summarise, skewing.mean=mean(skew))
chr13.cdat <- ddply(chr13.eB.dat, "cross", summarise, skewing.mean=mean(skew))

chrX.eB.Plot <- ggplot(chrX.eB.dat, aes(x=skew, fill=cross)) + 
  geom_density(alpha=0.45) + 
  theme_bw() +
  xlim(0, 1) +
  theme_pattern + 
  theme(axis.title.y=element_blank(),
        legend.position = c(0.8, 0.95)) +  
  geom_vline(data=chrX.cdat, aes(xintercept=skewing.mean,  colour=cross),linetype="dashed", size=0.5)

#pvalue, p=0.0002325263
wilcox.test(skew ~ cross, data=chrX.eB.dat)$p.value


chr13.eB.Plot <- ggplot(chr13.eB.dat, aes(x=skew, fill=cross)) + 
  geom_density(alpha=0.45) + 
  theme_bw() +
  xlim(0, 1) +
  theme_pattern + 
  theme(axis.title.y=element_blank(),
        legend.position = c(0.8, 0.95)) +  
  geom_vline(data=chr13.cdat, aes(xintercept=skewing.mean,  colour=cross),linetype="dashed", size=0.5)
#pvalue, p=0.6376937
wilcox.test(skew ~ cross, data=chr13.eB.dat)$p.value


library(gridExtra)
grid.arrange(chrX.4C.Plot, chrX.8C.Plot, chrX.16C.Plot, chrX.eB.Plot,nrow=1, ncol=4)

library(gridExtra)
grid.arrange(chr13.4C.Plot, chr13.8C.Plot, chr13.16C.Plot, chr13.eB.Plot,nrow=1, ncol=4)








#Panel c, LINE skew between CM and MC
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

#compare LINEs between CM and MC
All.LINE.dat <- All.repeats.dat[which(All.repeats.dat$class=="LINE"), ]


ggplot(All.LINE.dat, aes(x=stage, y=skew, fill=type)) +
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
#at 8C,p=0.7706041
dat <- All.LINE.dat[which(All.LINE.dat$stage=="8C"), ]
wilcox.test(skew ~ type, data=dat)$p.value 
#at 16C,p=0.810509
dat <- All.LINE.dat[which(All.LINE.dat$stage=="16C"), ]
wilcox.test(skew ~ type, data=dat)$p.value 
#at eB,p=0.2472965
dat <- All.LINE.dat[which(All.LINE.dat$stage=="earlyB."), ]
wilcox.test(skew ~ type, data=dat)$p.value 


#Panel d, LTR skew between CM and MC

All.LTR.dat <- All.repeats.dat[which(All.repeats.dat$class=="LTR"), ]


ggplot(All.LTR.dat, aes(x=stage, y=skew, fill=type)) +
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
#at 8C, p=0.4014533
dat <- All.LTR.dat[which(All.LTR.dat$stage=="8C"), ]
wilcox.test(skew ~ type, data=dat)$p.value 
#at 16C, p=0.5719874
dat <- All.LTR.dat[which(All.LTR.dat$stage=="16C"), ]
wilcox.test(skew ~ type, data=dat)$p.value 
#at eB, p=0.8200532
dat <- All.LTR.dat[which(All.LTR.dat$stage=="earlyB."), ]
wilcox.test(skew ~ type, data=dat)$p.value 



#Panel e-g. TE silencing in WT(CM) vs KO embryos

rm(list=ls())
source("KO.specific repeat analysis.R")
All.KO.repeats.dat <- data.frame(rbind(LINE.KO_l2C, LINE.KO_4C, LINE.KO_8C, LINE.KO_eB,
                                       SINE.KO_l2C, SINE.KO_4C, SINE.KO_8C, SINE.KO_eB,
                                       LTR.KO_l2C, LTR.KO_4C,LTR.KO_8C,LTR.KO_eB), type="KO")
#save a intermediate file, and delete later
write.table(All.KO.repeats.dat, file="tmp_file.txt", sep="\t", quote=F)

source("CM.specific repeat analysis.R")
All.CM.repeats.dat <- data.frame(rbind(LINE.l2C, LINE.N4C, LINE.N8C, LINE.eB,
                                       SINE.l2C, SINE.N4C, SINE.N8C, SINE.eB,
                                       LTR.l2C, LTR.N4C,LTR.N8C, LTR.eB), type="CM")
rm(list=setdiff(ls(), "All.CM.repeats.dat"))

All.KO.repeats.dat <- read.table(file="tmp_file.txt", sep="\t")
file.remove("tmp_file.txt")

All.repeats.dat <- rbind(All.KO.repeats.dat, All.CM.repeats.dat)
All.repeats.dat$stage <- factor(All.repeats.dat$stage, levels=c("late2C","4C","8C","earlyB."))
All.repeats.dat$type <- factor(All.repeats.dat$type, levels=c("KO","CM"))
All.repeats.dat$skew <- 1- All.repeats.dat$skew

#Panel e. compare SINEs between CM and KO

All.SINE.dat <- All.repeats.dat[which(All.repeats.dat$class=="SINE"), ]


ggplot(All.SINE.dat, aes(x=stage, y=skew, fill=type)) +
  geom_boxplot(outlier.shape=NA, lwd=0.4) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
             aes(fill=type), size=1.1, pch=21,stroke = 0.4) +
  scale_fill_manual(values=c("rosybrown1","dodgerblue")) +
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
#at 8C, p=0.04028729
dat <- All.SINE.dat[which(All.SINE.dat$stage=="8C"), ]
wilcox.test(skew ~ type, data=dat)$p.value 
#at eB, p=8.578158e-06
dat <- All.SINE.dat[which(All.SINE.dat$stage=="earlyB."), ]
wilcox.test(skew ~ type, data=dat)$p.value 


#Panel f. compare LINEs between CM and KO

All.LINE.dat <- All.repeats.dat[which(All.repeats.dat$class=="LINE"), ]


ggplot(All.LINE.dat, aes(x=stage, y=skew, fill=type)) +
  geom_boxplot(outlier.shape=NA, lwd=0.4) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
             aes(fill=type), size=1.1, pch=21,stroke = 0.4) +
  scale_fill_manual(values=c("rosybrown1","dodgerblue")) +
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
#at 8C, p=0.4187073
dat <- All.LINE.dat[which(All.LINE.dat$stage=="8C"), ]
wilcox.test(skew ~ type, data=dat)$p.value 
#at eB, p=0.8828404
dat <- All.LINE.dat[which(All.LINE.dat$stage=="earlyB."), ]
wilcox.test(skew ~ type, data=dat)$p.value 


#Panel g. compare LTRs between CM and KO

All.LTR.dat <- All.repeats.dat[which(All.repeats.dat$class=="LTR"), ]


ggplot(All.LTR.dat, aes(x=stage, y=skew, fill=type)) +
  geom_boxplot(outlier.shape=NA, lwd=0.4) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
             aes(fill=type), size=1.1, pch=21,stroke = 0.4) +
  scale_fill_manual(values=c("rosybrown1","dodgerblue")) +
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
#at 8C, p=0.5163458
dat <- All.LTR.dat[which(All.LTR.dat$stage=="8C"), ]
wilcox.test(skew ~ type, data=dat)$p.value 
#at eB, p=0.05778346
dat <- All.LTR.dat[which(All.LTR.dat$stage=="earlyB."), ]
wilcox.test(skew ~ type, data=dat)$p.value 




#Panel h-j. check SINE skew on Xm in MC embryos.
rm(list=ls())

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

#total female SINE reads (the sum of all replicates, from the processed file named "repeat reads ratio". )
late2C.SINE.totalNumber <- 2312129
N4C.SINE.totalNumber <- 3101072
N8C.SINE.totalNumber <- 3949845
N16C.SINE.totalNumber <- 1435485
eB.SINE.totalNumber <- 566976


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


#divide all SINEs into 3 groups: (proximal (1-15), intermediate(15-57), distal(57-end))

dat[which(abs(dat$start-84.87)<=15), "type" ] <- "proximal"
dat[which(abs(dat$start-84.87)>15 & abs(dat$start-84.87)<=57), "type" ] <- "intermediate"
dat[which(abs(dat$start-84.87)>57), "type" ] <- "distal"
#based on my observation in the plot above, the intermediate section on the right side of Xist should be further divided to intermediate (100-127) and distal (>127)
dat[which(dat$start > 127), "type"] <- "distal"




#Panel h. SINE skew at from 4C to 8C on Xm in MC embryos.--------------
#For comparison between 4C and 8C, I used original "N4_N8C.dat" (including count 0) 
N4_N8C.dat <- dat[ ,c(1,3,4,7)]
#N4_N8C.dat <- N4_N8C.dat[which(N4_N8C.dat[ ,2] !=0 | N4_N8C.dat[ ,3] !=0), ]
N4_N8C.dat <- N4_N8C.dat[which(N4_N8C.dat[ ,2] >= 2 | N4_N8C.dat[ ,3] >=2), ]
N4C.dat <- data.frame(count=N4_N8C.dat$N4C, stage="4C", type=N4_N8C.dat$type)
N8C.dat <- data.frame(count=N4_N8C.dat$N8C, stage="8C", type=N4_N8C.dat$type)
combined.dat <- rbind(N4C.dat, N8C.dat)
combined.dat$count <- log(combined.dat$count+1)
combined.dat$type <- factor(combined.dat$type, levels=c("proximal", "intermediate", "distal"))
combined.dat$stage <- factor(combined.dat$stage, levels=c("4C", "8C"))

#p-value, proximal genes between 4C and 8C, p=0.007769989
N4_N8C.proximal.dat <- combined.dat[which(combined.dat$type=="proximal"), ]
wilcox.test(count ~ stage, data=N4_N8C.proximal.dat,paired=T)$p.value

#p-value, intermediate genes between 4C and 8C, p=0.2229219
N4_N8C.inter.dat <- combined.dat[which(combined.dat$type=="intermediate"), ]
wilcox.test(count ~ stage, data=N4_N8C.inter.dat, paired=T)$p.value

#p-value, distal genes between 4C and 8C, p=0.8216191
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



#Panel i. SINE skew at from 8C to 16C on Xm in MC embryos.--------------
#For comparison between 8C and 16C, I used original "N8_N16C.dat" (including count 0) 
N8_N16C.dat <- dat[ ,c(1,4,5,7)]
#N8_N16C.dat <- N8_N16C.dat[which(N8_N16C.dat[ ,2] !=0 | N8_N16C.dat[ ,3] !=0), ]
N8_N16C.dat <- N8_N16C.dat[which(N8_N16C.dat[ ,2] >= 2 | N8_N16C.dat[ ,3] >=2), ]
N8C.dat <- data.frame(count=N8_N16C.dat$N8C, stage="8C", type=N8_N16C.dat$type)
N16C.dat <- data.frame(count=N8_N16C.dat$N16C, stage="16C", type=N8_N16C.dat$type)
combined.dat <- rbind(N8C.dat, N16C.dat)
combined.dat$count <- log(combined.dat$count+1)
combined.dat$type <- factor(combined.dat$type, levels=c("proximal", "intermediate", "distal"))
combined.dat$stage <- factor(combined.dat$stage, levels=c("8C", "16C"))

#p-value, proximal genes between 8C and 16C, p=0.3615531
N8_N16C.proximal.dat <- combined.dat[which(combined.dat$type=="proximal"), ]
wilcox.test(count ~ stage, data=N8_N16C.proximal.dat, paired=T)$p.value

#p-value, intermediate genes between 8C and 16C, p=0.1946042
N8_N16C.inter.dat <- combined.dat[which(combined.dat$type=="intermediate"), ]
wilcox.test(count ~ stage, data=N8_N16C.inter.dat, paired=T)$p.value

#p-value, distal genes between 8C and 16C, p=0.5735855
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



#Panel j. SINE skew at from 16C to early blastocyst on Xm in MC embryos.--------------
#For comparison between 8C and 16C, I used original "N8_N16C.dat" (including count 0) 
N16_eB.dat <- dat[ ,c(1,5,6,7)]
#N16_eB.dat <- N16_eB.dat[which(N16_eB.dat[ ,2] !=0 | N16_eB.dat[ ,3] !=0), ]
N16_eB.dat <- N16_eB.dat[which(N16_eB.dat[ ,2] >=3 | N16_eB.dat[ ,3] >=3), ]
N16C.dat <- data.frame(count=N16_eB.dat$N16C, stage="16C", type=N16_eB.dat$type)
eB.dat <- data.frame(count=N16_eB.dat$eB, stage="eB", type=N16_eB.dat$type)
combined.dat <- rbind(N16C.dat, eB.dat)
combined.dat$count <- log(combined.dat$count+1)
combined.dat$type <- factor(combined.dat$type, levels=c("proximal", "intermediate", "distal"))
combined.dat$stage <- factor(combined.dat$stage, levels=c("16C", "eB"))

#p-value, proximal genes between 8C and 16C, p=0.3676488
N16_eB.proximal.dat <- combined.dat[which(combined.dat$type=="proximal"), ]
wilcox.test(count ~ stage, data=N16_eB.proximal.dat, paired=T)$p.value

#p-value, intermediate genes between 8C and 16C,  p=0.5486452
N16_eB.inter.dat <- combined.dat[which(combined.dat$type=="intermediate"), ]
wilcox.test(count ~ stage, data=N16_eB.inter.dat, paired=T)$p.value

#p-value, distal genes between 8C and 16C, p=0.4612868
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





#Panel k. determine the fraction of full length LINEs in the cas and mus--------------------------
cas.full_length.LINE <- read.table(file="processed/BED/chrX.cas.full-length.LINE.DNA.srt.bed", sep="\t")
ref.full_length.LINE <- read.table(file="processed/BED/chrX.ref.full-length.LINE.DNA.srt.bed", sep="\t")

casLINE.dat <- data.frame(rep=names(table(cas.full_length.LINE$V4)), count=as.numeric(table(cas.full_length.LINE$V4)), strain="cas")
refLINE.dat <- data.frame(rep=names(table(ref.full_length.LINE$V4)), count=as.numeric(table(ref.full_length.LINE$V4)), strain="ref")
LINE.dat <- rbind(casLINE.dat, refLINE.dat)

LINE.dat$rep <- factor(LINE.dat$rep, levels=as.character(casLINE.dat$rep[order(casLINE.dat$count, decreasing = T)]))

ggplot(LINE.dat, aes(x=rep, y=count, fill=strain)) +
  geom_bar(stat="identity",position=position_dodge(),width = 0.7) +
  theme_bw() +
  scale_fill_manual(values=c("lightsalmon2","dodgerblue")) +
  theme(axis.title.x=element_text(),
        axis.text.x=element_text(angle = 60),
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



#Panel l. #size distribution for "intergenic" and "intronic" LINEs on Xp-------------
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

#cas
cast.LINE.size <- rbind(data.frame(late2C.LINE.cast.sum[ ,c(9,10)], stage="late2C"),
                        data.frame(N4C.LINE.cast.sum[ ,c(9,10)], stage="4C"),
                        data.frame(N8C.LINE.cast.sum[ ,c(9,10)], stage="8C"),
                        data.frame(N16C.LINE.cast.sum[ ,c(9,10)], stage="16C"),
                        data.frame(eB.LINE.cast.sum[ ,c(9,10)], stage="earlyBlast"))

#ref
ref.LINE.size <- rbind(data.frame(late2C.LINE.ref.sum[ ,c(9,10)], stage="late2C"),
                       data.frame(N4C.LINE.ref.sum[ ,c(9,10)], stage="4C"),
                       data.frame(N8C.LINE.ref.sum[ ,c(9,10)], stage="8C"),
                       data.frame(N16C.LINE.ref.sum[ ,c(9,10)], stage="16C"),
                       data.frame(eB.LINE.ref.sum[ ,c(9,10)], stage="earlyBlast"))

#size distribution for "intergenic" and "intronic" LINEs on Xp

#cast
ggplot(cast.LINE.size, aes(x=size, fill=type)) +
  geom_density(alpha=0.5) +
  scale_fill_manual(values=c("red", "steelblue3")) +
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
#p-value
wilcox.test(size ~ type, data=cast.LINE.size)$p.value




#Panel m. Silencing of intergenic vs intronic LINEs.--------------------

#Calculate the intergenic and intronic LINE skew during imprinted XCI, using total LINE reads of each class, and normalized to total LINE counts.
#total female LINE reads (the sum of all replicates, from the processed file named "MC.repeat reads ratio_200hits". )
late2C.LINE.totalNumber <- 207679
N4C.LINE.totalNumber <- 163945
N8C.LINE.totalNumber <- 160217
N16C.LINE.totalNumber <- 103944
eB.LINE.totalNumber <- 134710


intergenic.LINE.late2C <- late2C.LINE.cast.sum[which(late2C.LINE.cast.sum$type=="intergenic"), ]
intronic.LINE.late2C <- late2C.LINE.cast.sum[which(late2C.LINE.cast.sum$type=="intronic"), ]

intergenic.LINE.4C <- N4C.LINE.cast.sum[which(N4C.LINE.cast.sum$type=="intergenic"), ]
intronic.LINE.4C <- N4C.LINE.cast.sum[which(N4C.LINE.cast.sum$type=="intronic"), ]

intergenic.LINE.8C <- N8C.LINE.cast.sum[which(N8C.LINE.cast.sum$type=="intergenic"), ]
intronic.LINE.8C <- N8C.LINE.cast.sum[which(N8C.LINE.cast.sum$type=="intronic"), ]

intergenic.LINE.16C <- N16C.LINE.cast.sum[which(N16C.LINE.cast.sum$type=="intergenic"), ]
intronic.LINE.16C <- N16C.LINE.cast.sum[which(N16C.LINE.cast.sum$type=="intronic"), ]

intergenic.LINE.eB <- eB.LINE.cast.sum[which(eB.LINE.cast.sum$type=="intergenic"), ]
intronic.LINE.eB <- eB.LINE.cast.sum[which(eB.LINE.cast.sum$type=="intronic"), ]

Ref.intergenic.LINE.late2C <- late2C.LINE.ref.sum[which(late2C.LINE.ref.sum$type=="intergenic"), ]
Ref.intronic.LINE.late2C <- late2C.LINE.ref.sum[which(late2C.LINE.ref.sum$type=="intronic"), ]

Ref.intergenic.LINE.4C <- N4C.LINE.ref.sum[which(N4C.LINE.ref.sum$type=="intergenic"), ]
Ref.intronic.LINE.4C <- N4C.LINE.ref.sum[which(N4C.LINE.ref.sum$type=="intronic"), ]

Ref.intergenic.LINE.8C <- N8C.LINE.ref.sum[which(N8C.LINE.ref.sum$type=="intergenic"), ]
Ref.intronic.LINE.8C <- N8C.LINE.ref.sum[which(N8C.LINE.ref.sum$type=="intronic"), ]

Ref.intergenic.LINE.16C <- N16C.LINE.ref.sum[which(N16C.LINE.ref.sum$type=="intergenic"), ]
Ref.intronic.LINE.16C <- N16C.LINE.ref.sum[which(N16C.LINE.ref.sum$type=="intronic"), ]

Ref.intergenic.LINE.eB <- eB.LINE.ref.sum[which(eB.LINE.ref.sum$type=="intergenic"), ]
Ref.intronic.LINE.eB <- eB.LINE.ref.sum[which(eB.LINE.ref.sum$type=="intronic"), ]


#use counts of each LINE subtypes from intergenic or intronic LINEs, and calculate mus allele fraction

#First sum the counts of each LINE subtype using aggregate() function.
cast.intergenic.late2C.Groupby <- aggregate(intergenic.LINE.late2C$count, by=list(intergenic.LINE.late2C$rep), FUN=sum)
cast.intronic.late2C.Groupby <- aggregate(intronic.LINE.late2C$count, by=list(intronic.LINE.late2C$rep), FUN=sum)

cast.intergenic.4C.Groupby <- aggregate(intergenic.LINE.4C$count, by=list(intergenic.LINE.4C$rep), FUN=sum)
cast.intronic.4C.Groupby <- aggregate(intronic.LINE.4C$count, by=list(intronic.LINE.4C$rep), FUN=sum)

cast.intergenic.8C.Groupby <- aggregate(intergenic.LINE.8C$count, by=list(intergenic.LINE.8C$rep), FUN=sum)
cast.intronic.8C.Groupby <- aggregate(intronic.LINE.8C$count, by=list(intronic.LINE.8C$rep), FUN=sum)

cast.intergenic.16C.Groupby <- aggregate(intergenic.LINE.16C$count, by=list(intergenic.LINE.16C$rep), FUN=sum)
cast.intronic.16C.Groupby <- aggregate(intronic.LINE.16C$count, by=list(intronic.LINE.16C$rep), FUN=sum)

cast.intergenic.eB.Groupby <- aggregate(intergenic.LINE.eB$count, by=list(intergenic.LINE.eB$rep), FUN=sum)
cast.intronic.eB.Groupby <- aggregate(intronic.LINE.eB$count, by=list(intronic.LINE.eB$rep), FUN=sum)


ref.intergenic.late2C.Groupby <- aggregate(Ref.intergenic.LINE.late2C$count, by=list(Ref.intergenic.LINE.late2C$rep), FUN=sum)
ref.intronic.late2C.Groupby <- aggregate(Ref.intronic.LINE.late2C$count, by=list(Ref.intronic.LINE.late2C$rep), FUN=sum)

ref.intergenic.4C.Groupby <- aggregate(Ref.intergenic.LINE.4C$count, by=list(Ref.intergenic.LINE.4C$rep), FUN=sum)
ref.intronic.4C.Groupby <- aggregate(Ref.intronic.LINE.4C$count, by=list(Ref.intronic.LINE.4C$rep), FUN=sum)

ref.intergenic.8C.Groupby <- aggregate(Ref.intergenic.LINE.8C$count, by=list(Ref.intergenic.LINE.8C$rep), FUN=sum)
ref.intronic.8C.Groupby <- aggregate(Ref.intronic.LINE.8C$count, by=list(Ref.intronic.LINE.8C$rep), FUN=sum)

ref.intergenic.16C.Groupby <- aggregate(Ref.intergenic.LINE.16C$count, by=list(Ref.intergenic.LINE.16C$rep), FUN=sum)
ref.intronic.16C.Groupby <- aggregate(Ref.intronic.LINE.16C$count, by=list(Ref.intronic.LINE.16C$rep), FUN=sum)

ref.intergenic.eB.Groupby <- aggregate(Ref.intergenic.LINE.eB$count, by=list(Ref.intergenic.LINE.eB$rep), FUN=sum)
ref.intronic.eB.Groupby <- aggregate(Ref.intronic.LINE.eB$count, by=list(Ref.intronic.LINE.eB$rep), FUN=sum)


#align cast and ref for each stage

intergenic.late2C.Groupby <-  merge(cast.intergenic.late2C.Groupby, ref.intergenic.late2C.Groupby, by.x="Group.1", by.y="Group.1", all=T)
intergenic.late2C.Groupby[is.na(intergenic.late2C.Groupby)] <- 0
intergenic.late2C.Groupby[ ,4] <- intergenic.late2C.Groupby[,2] + intergenic.late2C.Groupby[ ,3]
intergenic.late2C.Groupby[ ,5] <- intergenic.late2C.Groupby[,2] / intergenic.late2C.Groupby[,4]
intergenic.late2C.Groupby[ ,6] <- "late2C"
intergenic.late2C.Groupby[ ,7] <- "intergenic"
colnames(intergenic.late2C.Groupby) <- c("subfamily","cas","ref","total","Xp_fraction","stage","type")

intergenic.4C.Groupby <-  merge(cast.intergenic.4C.Groupby, ref.intergenic.4C.Groupby, by.x="Group.1", by.y="Group.1", all=T)
intergenic.4C.Groupby[is.na(intergenic.4C.Groupby)] <- 0
intergenic.4C.Groupby[ ,4] <- intergenic.4C.Groupby[,2] + intergenic.4C.Groupby[ ,3]
intergenic.4C.Groupby[ ,5] <- intergenic.4C.Groupby[,2] / intergenic.4C.Groupby[,4]
intergenic.4C.Groupby[ ,6] <- "4C"
intergenic.4C.Groupby[ ,7] <- "intergenic"
colnames(intergenic.4C.Groupby) <- c("subfamily","cas","ref","total","Xp_fraction","stage","type")

intergenic.8C.Groupby <-  merge(cast.intergenic.8C.Groupby, ref.intergenic.8C.Groupby, by.x="Group.1", by.y="Group.1", all=T)
intergenic.8C.Groupby[is.na(intergenic.8C.Groupby)] <- 0
intergenic.8C.Groupby[ ,4] <- intergenic.8C.Groupby[,2] + intergenic.8C.Groupby[ ,3]
intergenic.8C.Groupby[ ,5] <- intergenic.8C.Groupby[,2] / intergenic.8C.Groupby[,4]
intergenic.8C.Groupby[ ,6] <- "8C"
intergenic.8C.Groupby[ ,7] <- "intergenic"
colnames(intergenic.8C.Groupby) <- c("subfamily","cas","ref","total","Xp_fraction","stage","type")

intergenic.16C.Groupby <-  merge(cast.intergenic.16C.Groupby, ref.intergenic.16C.Groupby, by.x="Group.1", by.y="Group.1", all=T)
intergenic.16C.Groupby[is.na(intergenic.16C.Groupby)] <- 0
intergenic.16C.Groupby[ ,4] <- intergenic.16C.Groupby[,2] + intergenic.16C.Groupby[ ,3]
intergenic.16C.Groupby[ ,5] <- intergenic.16C.Groupby[,2] / intergenic.16C.Groupby[,4]
intergenic.16C.Groupby[ ,6] <- "16C"
intergenic.16C.Groupby[ ,7] <- "intergenic"
colnames(intergenic.16C.Groupby) <- c("subfamily","cas","ref","total","Xp_fraction","stage","type")

intergenic.eB.Groupby <-  merge(cast.intergenic.eB.Groupby, ref.intergenic.eB.Groupby, by.x="Group.1", by.y="Group.1", all=T)
intergenic.eB.Groupby[is.na(intergenic.eB.Groupby)] <- 0
intergenic.eB.Groupby[ ,4] <- intergenic.eB.Groupby[,2] + intergenic.eB.Groupby[ ,3]
intergenic.eB.Groupby[ ,5] <- intergenic.eB.Groupby[,2] / intergenic.eB.Groupby[,4]
intergenic.eB.Groupby[ ,6] <- "earlyBlast"
intergenic.eB.Groupby[ ,7] <- "intergenic"
colnames(intergenic.eB.Groupby) <- c("subfamily","cas","ref","total","Xp_fraction","stage","type")


intronic.late2C.Groupby <-  merge(cast.intronic.late2C.Groupby, ref.intronic.late2C.Groupby, by.x="Group.1", by.y="Group.1", all=T)
intronic.late2C.Groupby[is.na(intronic.late2C.Groupby)] <- 0
intronic.late2C.Groupby[ ,4] <- intronic.late2C.Groupby[,2] + intronic.late2C.Groupby[ ,3]
intronic.late2C.Groupby[ ,5] <- intronic.late2C.Groupby[,2] / intronic.late2C.Groupby[,4]
intronic.late2C.Groupby[ ,6] <- "late2C"
intronic.late2C.Groupby[ ,7] <- "intronic"
colnames(intronic.late2C.Groupby) <- c("subfamily","cas","ref","total","Xp_fraction","stage","type")

intronic.4C.Groupby <-  merge(cast.intronic.4C.Groupby, ref.intronic.4C.Groupby, by.x="Group.1", by.y="Group.1", all=T)
intronic.4C.Groupby[is.na(intronic.4C.Groupby)] <- 0
intronic.4C.Groupby[ ,4] <- intronic.4C.Groupby[,2] + intronic.4C.Groupby[ ,3]
intronic.4C.Groupby[ ,5] <- intronic.4C.Groupby[,2] / intronic.4C.Groupby[,4]
intronic.4C.Groupby[ ,6] <- "4C"
intronic.4C.Groupby[ ,7] <- "intronic"
colnames(intronic.4C.Groupby) <- c("subfamily","cas","ref","total","Xp_fraction","stage","type")

intronic.8C.Groupby <-  merge(cast.intronic.8C.Groupby, ref.intronic.8C.Groupby, by.x="Group.1", by.y="Group.1", all=T)
intronic.8C.Groupby[is.na(intronic.8C.Groupby)] <- 0
intronic.8C.Groupby[ ,4] <- intronic.8C.Groupby[,2] + intronic.8C.Groupby[ ,3]
intronic.8C.Groupby[ ,5] <- intronic.8C.Groupby[,2] / intronic.8C.Groupby[,4]
intronic.8C.Groupby[ ,6] <- "8C"
intronic.8C.Groupby[ ,7] <- "intronic"
colnames(intronic.8C.Groupby) <- c("subfamily","cas","ref","total","Xp_fraction","stage","type")

intronic.16C.Groupby <-  merge(cast.intronic.16C.Groupby, ref.intronic.16C.Groupby, by.x="Group.1", by.y="Group.1", all=T)
intronic.16C.Groupby[is.na(intronic.16C.Groupby)] <- 0
intronic.16C.Groupby[ ,4] <- intronic.16C.Groupby[,2] + intronic.16C.Groupby[ ,3]
intronic.16C.Groupby[ ,5] <- intronic.16C.Groupby[,2] / intronic.16C.Groupby[,4]
intronic.16C.Groupby[ ,6] <- "16C"
intronic.16C.Groupby[ ,7] <- "intronic"
colnames(intronic.16C.Groupby) <- c("subfamily","cas","ref","total","Xp_fraction","stage","type")

intronic.eB.Groupby <-  merge(cast.intronic.eB.Groupby, ref.intronic.eB.Groupby, by.x="Group.1", by.y="Group.1", all=T)
intronic.eB.Groupby[is.na(intronic.eB.Groupby)] <- 0
intronic.eB.Groupby[ ,4] <- intronic.eB.Groupby[,2] + intronic.eB.Groupby[ ,3]
intronic.eB.Groupby[ ,5] <- intronic.eB.Groupby[,2] / intronic.eB.Groupby[,4]
intronic.eB.Groupby[ ,6] <- "earlyBlast"
intronic.eB.Groupby[ ,7] <- "intronic"
colnames(intronic.eB.Groupby) <- c("subfamily","cas","ref","total","Xp_fraction","stage","type")

#rm(list=setdiff(ls(), c("intergenic.late2C.Groupby","intergenic.4C.Groupby","intergenic.8C.Groupby","intergenic.16C.Groupby","intergenic.eB.Groupby",
#                        "intronic.late2C.Groupby","intronic.4C.Groupby","intronic.8C.Groupby","intronic.16C.Groupby","intronic.eB.Groupby")))

dat <- rbind(intergenic.late2C.Groupby[ ,c(4:7)], intronic.late2C.Groupby[ ,c(4:7)],
             intergenic.4C.Groupby[ ,c(4:7)], intronic.4C.Groupby[ ,c(4:7)],
             intergenic.8C.Groupby[ ,c(4:7)], intronic.8C.Groupby[ ,c(4:7)],
             intergenic.16C.Groupby[ ,c(4:7)], intronic.16C.Groupby[ ,c(4:7)],
             intergenic.eB.Groupby[ ,c(4:7)], intronic.eB.Groupby[ ,c(4:7)])

dat <- dat[which(dat$total >=5), ]

dat$stage <- factor(dat$stage, levels=c("late2C","4C","8C","16C","earlyBlast"))

ggplot(dat, aes(x=stage, y=Xp_fraction, fill=type)) +
  geom_boxplot() +
  scale_fill_manual(values=c("indianred1", "skyblue2")) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
             aes(fill=type), size=1, pch=21,stroke = 0.4) +
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

#p-value p=0.7783
late2C.dat <- dat[which(dat$stage=="late2C"), c(2,4)]
wilcox.test(Xp_fraction ~ type, data=late2C.dat)$p.value
#p=0.8085
N4C.dat <- dat[which(dat$stage=="4C"), ]
wilcox.test(Xp_fraction ~ type, data=N4C.dat)$p.value
#8C, p=0.1218613
N8C.dat <- dat[which(dat$stage=="8C"), ]
wilcox.test(Xp_fraction ~ type, data=N8C.dat)$p.value
#16C, p=0.1797485
N16C.dat <- dat[which(dat$stage=="16C"), ]
wilcox.test(Xp_fraction ~ type, data=N16C.dat)$p.value

