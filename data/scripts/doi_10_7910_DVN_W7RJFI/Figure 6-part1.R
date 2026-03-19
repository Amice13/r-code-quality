
rm(list=ls())
source("Read_data.R")

############### Figure 6a, generate heatmap for gene clusters in KO embryos ################
source("Gene_skewing_clusterng_analyses.R")
rm(list=setdiff(ls(), "KO.chrX.skew.AllStages"))
KO.skewed.genes <- KO.chrX.skew.AllStages
MC.clustered.genes <- read.table(file="Attached_doc/MC.Gene_Category.v2.txt", sep="\t")

library(RColorBrewer)
#remove "Pdzd11" and "Sat1", because they don't match reciprocal cross and KO cross data, might be strain-specific
MC.Kmean.dat <- MC.clustered.genes[ ,c(1:5)]
KO.Kmean_related.dat <- KO.skewed.genes[rownames(MC.Kmean.dat), -5]



colnames(MC.Kmean.dat) <- c("late2C","4C","8C","16C","earlyB.")
colnames(KO.Kmean_related.dat) <- c("late2C","4C","8C","earlyB.")
# Sets the minimum (0), the maximum (1), and the increasing steps (+0.01) for the color scale
# Note: if some of your genes are outside of this range, they will appear white on the heatmap
breaksList = seq(0, 1, by = 0.01)
annot_cluster <- data.frame(Category=MC.clustered.genes$category, row.names=rownames(MC.clustered.genes))

cols <- colorRampPalette(brewer.pal(6, "Set2")); 
mycolors <- cols(length(unique(annot_cluster$Category)));
names(mycolors) <- unique(annot_cluster$Category)
mycolors <- list(Category = mycolors)

#combine CM and MC data
combined.dat <- cbind(MC.Kmean.dat, KO.Kmean_related.dat)
combined.dat <- 1-combined.dat #reverse the color scale
pheatmap(combined.dat, 
         color = colorRampPalette(rev(brewer.pal(n = 6, name = "RdYlBu")))(length(breaksList)),
         # Defines the vector of colors for the legend (equal to lenght of breaksList)
         breaks = breaksList,
         # Sets the breaks of the color scale as in breaksList
         cluster_cols = F,
         cluster_rows = F, 
         show_rownames = T, 
         show_colnames = T, 
         border_color=NA,
         annotation_row = annot_cluster,
         annotation_colors = mycolors,
         gaps_row=c(25,33,50,74,94),
         gaps_col=c(5),
         cellheight = 6,
         fontsize_row=7.5)

#Calculate average skew value
cat1.genes <- rownames(annot_cluster)[annot_cluster$Category=="Early"]
cat2.genes <- rownames(annot_cluster)[annot_cluster$Category=="Mid"]
cat3.genes <- rownames(annot_cluster)[annot_cluster$Category=="Late"]
cat4.genes <- rownames(annot_cluster)[annot_cluster$Category=="Constitutive"]
cat5.genes <- rownames(annot_cluster)[annot_cluster$Category=="Escapee"]
cat6.genes <- rownames(annot_cluster)[annot_cluster$Category=="Strain-bias"]

KO.Cat1.mean <- colMeans(KO.Kmean_related.dat[cat1.genes, ], na.rm=T)
KO.Cat2.mean <- colMeans(KO.Kmean_related.dat[cat2.genes, ], na.rm=T)
KO.Cat3.mean <- colMeans(KO.Kmean_related.dat[cat3.genes, ], na.rm=T)
KO.Cat4.mean <- colMeans(KO.Kmean_related.dat[cat4.genes, ], na.rm=T)
KO.Cat5.mean <- colMeans(KO.Kmean_related.dat[cat5.genes, ], na.rm=T)
KO.Cat6.mean <- colMeans(KO.Kmean_related.dat[cat6.genes, ], na.rm=T)

average.skew.dat <- rbind(KO.Cat1.mean, KO.Cat2.mean, KO.Cat3.mean, KO.Cat4.mean, KO.Cat5.mean, KO.Cat6.mean)
colnames(average.skew.dat) <- c("late2C", "4C", "8C", "earlyB.")

average.skew.dat <- 1-average.skew.dat
pheatmap(average.skew.dat, 
         color = colorRampPalette(rev(brewer.pal(n = 6, name = "RdYlBu")))(length(breaksList)),
         # Defines the vector of colors for the legend (equal to lenght of breaksList)
         breaks = breaksList,
         # Sets the breaks of the color scale as in breaksList
         cluster_cols = F,
         cluster_rows = F, 
         show_rownames = T, 
         show_colnames = T, 
         border_color=NA,
         annotation_row = annot_cluster,
         annotation_colors = mycolors,
         gaps_col=c(4),
         cellheight = 10,
         fontsize_row=7.5)


########## Figure 6b. Density plot of chrX genes skew between WT(CM) and KO embryos ###############
source("Gene_skewing_density_analyses.R")
library(plyr)

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


#L2C

KO.chrX.dat <- data.frame(skew=KO.chrX.skew.Mean.l2C$skew, sample="KO")
wt.chrX.dat <- data.frame(skew=MC.chrX.skew.Mean.l2C$skew, sample="WT")
L2C.dat <- rbind(KO.chrX.dat, wt.chrX.dat)
cdat <- ddply(L2C.dat, "sample", summarise, skewing.mean=mean(skew))

P1 <- ggplot(L2C.dat, aes(x=skew, fill=sample)) + 
  geom_density(alpha=0.45) + 
  theme_bw() +
  xlim(0, 1) +
  theme_pattern + 
  theme(axis.title.y=element_blank(),
        legend.position = c(0.8, 0.95)) +  
  geom_vline(data=cdat, aes(xintercept=skewing.mean,  colour=sample),linetype="dashed", linewidth=0.5)
#pvalue, p=0.0311545
wilcox.test(skew ~ sample, data=L2C.dat)$p.value

#4C
KO.chrX.dat <- data.frame(skew=KO.chrX.skew.Mean.N4C$skew, sample="KO")
wt.chrX.dat <- data.frame(skew=MC.chrX.skew.Mean.N4C$skew, sample="WT")
N4C.dat <- rbind(KO.chrX.dat, wt.chrX.dat)
cdat <- ddply(N4C.dat, "sample", summarise, skewing.mean=mean(skew))

P2 <- ggplot(N4C.dat, aes(x=skew, fill=sample)) + 
  geom_density(alpha=0.45) + 
  theme_bw() +
  xlim(0, 1) +
  theme_pattern + 
  theme(axis.title.y=element_blank(),
        legend.position = c(0.8, 0.95)) +  
  geom_vline(data=cdat, aes(xintercept=skewing.mean,  colour=sample),linetype="dashed", linewidth=0.5)
#pvalue=0.009457275
wilcox.test(skew ~ sample, data=N4C.dat)$p.value


#8C
KO.chrX.dat <- data.frame(skew=KO.chrX.skew.Mean.N8C$skew, sample="KO")
wt.chrX.dat <- data.frame(skew=MC.chrX.skew.Mean.N8C$skew, sample="WT")
N8C.dat <- rbind(KO.chrX.dat, wt.chrX.dat)
cdat <- ddply(N8C.dat, "sample", summarise, skewing.mean=mean(skew))

P3 <- ggplot(N8C.dat, aes(x=skew, fill=sample)) + 
  geom_density(alpha=0.45) + 
  theme_bw() +
  xlim(0, 1) +
  theme_pattern + 
  theme(axis.title.y=element_blank(),
        legend.position = c(0.8, 0.95)) +  
  geom_vline(data=cdat, aes(xintercept=skewing.mean,  colour=sample),linetype="dashed", size=0.5)
#pvalue, p=0.007399709
wilcox.test(skew ~ sample, data=N8C.dat)$p.value



#eB
KO.chrX.dat <- data.frame(skew=KO.chrX.skew.Mean.eB$skew, sample="KO")
wt.chrX.dat <- data.frame(skew=MC.chrX.skew.Mean.eB$skew, sample="WT")
eB.dat <- rbind(KO.chrX.dat, wt.chrX.dat)
cdat <- ddply(eB.dat, "sample", summarise, skewing.mean=mean(skew))

P4 <- ggplot(eB.dat, aes(x=skew, fill=sample)) + 
  geom_density(alpha=0.45) + 
  theme_bw() +
  xlim(0, 1) +
  theme_pattern + 
  theme(axis.title.y=element_blank(),
        legend.position = c(0.8, 0.95)) +  
  geom_vline(data=cdat, aes(xintercept=skewing.mean,  colour=sample),linetype="dashed", size=0.5)
#pvalue, p=1.793994e-28
wilcox.test(skew ~ sample, data=eB.dat)$p.value

#combine all plots horizontally
library(gridExtra)
grid.arrange(P1,P2,P3,P4, nrow=1, ncol=4)
rm(P1, P2, P3, P4, P5, P6, theme_pattern)


########## Figure 6c. skew comprison between Cat.1 and Cat.4 in Xist KO embryo ###############
source("Gene_skewing_clusterng_analyses.R")
rm(list=setdiff(ls(), "KO.chrX.skew.AllStages"))
KO.skewed.genes <- KO.chrX.skew.AllStages
MC.Kmean.dat <- read.table(file="Attached_doc/MC.Gene_Category.v2.txt", sep="\t")

cat.1_4 <- MC.Kmean.dat$category[which(MC.Kmean.dat$category == "Early" | MC.Kmean.dat$category == "Constitutive")]
cat.1_4.gene <- rownames(MC.Kmean.dat)[which(MC.Kmean.dat$category == "Early" | MC.Kmean.dat$category == "Constitutive")]
KO.Kmean_related.dat <- KO.skewed.genes[rownames(MC.Kmean.dat), -5]

cat.1_4.L2C.dat <- data.frame(skew=KO.Kmean_related.dat[cat.1_4.gene, 1], cluster=cat.1_4, stage="late2C")
cat.1_4.N4C.dat <- data.frame(skew=KO.Kmean_related.dat[cat.1_4.gene, 2], cluster=cat.1_4, stage="4C")
cat.1_4.N8C.dat <- data.frame(skew=KO.Kmean_related.dat[cat.1_4.gene, 3], cluster=cat.1_4, stage="8C")
cat.1_4.eB.dat <- data.frame(skew=KO.Kmean_related.dat[cat.1_4.gene, 4], cluster=cat.1_4, stage="eB")
dat <- rbind(cat.1_4.L2C.dat, cat.1_4.N4C.dat, cat.1_4.N8C.dat, cat.1_4.eB.dat)
# plot parameters
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
#calculate paternal value (mus)
dat$stage <- factor(dat$stage, levels=c("late2C","4C","8C","eB"))
ggplot(dat, aes(x=stage, y=skew, fill=cluster)) + 
  geom_boxplot(alpha=0.85, outlier.shape=NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 1), 
             aes(fill=cluster), size=1.2, pch=21) +
  scale_fill_manual(values=c("#66C2A5", "#E78AC3")) +
  theme_bw() +
  theme_pattern

#pvalue at late2cell p=0.0002030697
wilcox.test(skew ~ cluster, data=cat.1_4.L2C.dat)$p.value
#pvalue at 4cell p=0.1453522
wilcox.test(skew ~ cluster, data=cat.1_4.N4C.dat)$p.value
#pvalue at 8cell p=0.6671877
wilcox.test(skew ~ cluster, data=cat.1_4.N8C.dat)$p.value
#pvalue at eB p=0.02931227
wilcox.test(skew ~ cluster, data=cat.1_4.eB.dat)$p.value



########## Figure 6d. skew comprison of cat.4 genes between WT (CM) and KO embryo ###############
#follow Figure 5c.

cat.4.gene <- rownames(MC.Kmean.dat)[which(MC.Kmean.dat$category == "Constitutive")]

#KO
KO.cat.4.L2C.dat <- data.frame(skew=KO.Kmean_related.dat[cat.4.gene, 1], sample="KO", stage="late2C")
KO.cat.4.N4C.dat <- data.frame(skew=KO.Kmean_related.dat[cat.4.gene, 2], sample="KO", stage="4C")
KO.cat.4.N8C.dat <- data.frame(skew=KO.Kmean_related.dat[cat.4.gene, 3], sample="KO", stage="8C")
KO.cat.4.eB.dat <- data.frame(skew=KO.Kmean_related.dat[cat.4.gene, 4], sample="KO", stage="eB")
#WT(MC)
MC.cat.4.L2C.dat <- data.frame(skew=MC.Kmean.dat[cat.4.gene, 1], sample="WT", stage="late2C")
MC.cat.4.N4C.dat <- data.frame(skew=MC.Kmean.dat[cat.4.gene, 2], sample="WT", stage="4C")
MC.cat.4.N8C.dat <- data.frame(skew=MC.Kmean.dat[cat.4.gene, 3], sample="WT", stage="8C")
MC.cat.4.eB.dat <- data.frame(skew=MC.Kmean.dat[cat.4.gene, 5], sample="WT", stage="eB")

dat <- rbind(KO.cat.4.L2C.dat,KO.cat.4.N4C.dat,KO.cat.4.N8C.dat,KO.cat.4.eB.dat,
             MC.cat.4.L2C.dat,MC.cat.4.N4C.dat,MC.cat.4.N8C.dat,MC.cat.4.eB.dat)


dat$stage <- factor(dat$stage, levels=c("late2C","4C","8C","eB"))
ggplot(dat, aes(x=stage, y=skew, fill=sample)) + 
  geom_boxplot(alpha=0.8, outlier.shape=NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 1), 
             aes(fill=sample), size=1.2, pch=21) +
  scale_fill_manual(values=c("coral3", "cornflowerblue")) +
  theme_bw() +
  theme_pattern

#paired test, non-paired data was removed before test.
#pvalue at late2cell p=0.160105
dat <- rbind(KO.cat.4.L2C.dat, CM.cat.4.L2C.dat)
wilcox.test(skew ~ sample, data=dat, paired=T)$p.value
#pvalue at 4cell p=0.000672715
dat <- rbind(KO.cat.4.N4C.dat, CM.cat.4.N4C.dat)
dat <- dat[c(-11, -12, -14, -35, -36, -38), ]
wilcox.test(skew ~ sample, data=dat, paired=T)$p.value
#pvalue at 8cell p=6.345866e-05
dat <- rbind(KO.cat.4.N8C.dat, CM.cat.4.N8C.dat)
wilcox.test(skew ~ sample, data=dat,paired=T)$p.value
#pvalue at eB p=2.384186e-07
dat <- rbind(KO.cat.4.eB.dat, CM.cat.4.eB.dat)
dat <- dat[c(-16,-40), ]
wilcox.test(skew ~ sample, data=dat, paired=T)$p.value




#########Figure 6e. paternal fraction of zygotic reads (T->C) in cat1 vs 4 in Xis-KO embryo ######################
rm(list=ls())
#Cas is mother, mus is father
gene.rawData <- read.table(file="Attached_doc/Cat1_4_gene_counts_XistKO.txt", sep="\t", header=T, row.names=1)
Cat1.rawData <- gene.rawData[which(gene.rawData$category=="Cat 1"), -1]
Cat4.rawData <- gene.rawData[which(gene.rawData$category=="Cat 4"), -1]

Cat1.rawData <- read.table(file="../revision/4SU/Cat1_gene_counts.txt", sep="\t", header=T, row.names=1)[ ,-1]
Cat4.rawData <- read.table(file="../revision/4SU/Cat4_gene_counts.txt", sep="\t", header=T, row.names=1)[ ,-1]

Cat1.cas <- Cat1.rawData[ ,c(1,3,5,7,9,11,13)]
Cat1.mus <- Cat1.rawData[ ,c(2,4,6,8,10,12,14)]
Cat1.total <- Cat1.cas + Cat1.mus
Cat1.Paternal_fraction <- Cat1.mus/Cat1.total

Cat4.cas <- Cat4.rawData[ ,c(1,3,5,7,9,11,13)]
Cat4.mus <- Cat4.rawData[ ,c(2,4,6,8,10,12,14)]
Cat4.total <- Cat4.cas + Cat4.mus
Cat4.Paternal_fraction <- Cat4.mus/Cat4.total

# genes must have at least 2 total alleic reads, otherwise to be filtered out
Cat1.Paternal_fraction[Cat1.total < 2] <- NA
Cat4.Paternal_fraction[Cat4.total < 2] <- NA

Cat1.dat <- data.frame(ratio=rowMeans(Cat1.Paternal_fraction, na.rm=T), category="Cat1")
Cat4.dat <- data.frame(ratio=rowMeans(Cat4.Paternal_fraction, na.rm=T), category="Cat4")
dat <- rbind(Cat1.dat, Cat4.dat)



library(ggplot2)
ggplot(dat, aes(x=category, y=ratio,fill=category)) +
  geom_boxplot(alpha=0.8) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 1), 
             aes(fill=category), size=1.5, pch=21) +
  scale_fill_manual(values=c("coral3", "cornflowerblue"))+
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        axis.title.x=element_blank(),
        legend.title=element_blank(), 
        legend.key = element_blank(),
        axis.text.x=element_text(size = 12),
        legend.position = "right",
        legend.text=element_text(size = rel(1)), #face="bold"
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_line(color="black"),
        axis.line.x = element_line(color="black"))



#Cat1 vs Cat 4 p-value p=0.003124845
wilcox.test(ratio ~ category, data=dat)$p.value





########## Figure 6f Top. skew comprison of genes and LINEs between WT and KO embryo at 8C and eB ###############
#Genes
source("Gene_skewing_density_analyses.R")
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
#8C
N8C.KO.chrX.dat <- data.frame(skew=KO.chrX.skew.Mean.N8C$skew, sample="KO", stage="8C")
N8C.wt.chrX.dat <- data.frame(skew=CM.chrX.skew.Mean.N8C$skew, sample="WT", stage="8C")
eB.KO.chrX.dat <- data.frame(skew=KO.chrX.skew.Mean.eB$skew, sample="KO",stage="eB")
eB.wt.chrX.dat <- data.frame(skew=CM.chrX.skew.Mean.eB$skew, sample="WT",stage="eB")
dat <- rbind(N8C.KO.chrX.dat, N8C.wt.chrX.dat, eB.KO.chrX.dat, eB.wt.chrX.dat)

ggplot(dat, aes(y=skew, x=stage, fill=sample)) + 
  geom_boxplot(alpha=0.45, outlier.shape=NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 1), 
             aes(fill=sample), size=1.2, pch=21) +
  scale_fill_manual(values=c("coral3", "cornflowerblue")) +
  theme_bw() +
  theme_pattern 

N8C.dat <- rbind(N8C.KO.chrX.dat, N8C.wt.chrX.dat)
#pvalue, p=0.007399709
wilcox.test(skew ~ sample, data=N8C.dat)$p.value

eB.dat <- rbind(eB.KO.chrX.dat, eB.wt.chrX.dat)
#pvalue, p=1.793994e-28
wilcox.test(skew ~ sample, data=eB.dat)$p.value



######### Figure 6g. Xm upregulation in relation to Xp silencing in Xist Ko embryo######################

#Related to Figure 3j, same analysis as Fig.3j, but in Xist KO embryos

source("Read_data.R")
rm(list=setdiff(ls(), c("MC.F.mus.All","MC.F.cas.All","MC.F.comp.RPKM","MC.M.comp.RPKM",
                        "KO.F.mus.All","KO.F.cas.All","KO.F.comp.RPKM","KO.M.comp.RPKM")))
MC.clustered.genes <- read.table(file="Attached_doc/MC.Gene_Category.v2.txt", sep="\t")
MC.cluster1 <- MC.clustered.genes[which(MC.clustered.genes$category == "Early"), ]
MC.cluster2 <- MC.clustered.genes[which(MC.clustered.genes$category == "Mid"), ]
MC.cluster3 <- MC.clustered.genes[which(MC.clustered.genes$category == "Late"), ]
MC.cluster4 <- MC.clustered.genes[which(MC.clustered.genes$category == "Constitutive"), ]
MC.cluster5 <- MC.clustered.genes[which(MC.clustered.genes$category == "Escapee"), ]
MC.cluster6 <- MC.clustered.genes[which(MC.clustered.genes$category == "Strain-bias"), ]

#calculate maternal portion of RPKM, by multiplying comp RPKM with maternal (cas) ratio.

KO.F.Allelic.All <- KO.F.mus.All + KO.F.cas.All
F.MatRatio.All <- KO.F.cas.All / KO.F.Allelic.All
F.Mat.RPKM <- KO.F.comp.RPKM * F.MatRatio.All


#overall RPKM expression of genes in each cluster
F.cluster1.MatRPKM <- F.Mat.RPKM[rownames(MC.cluster1), ]
F.cluster2.MatRPKM <- F.Mat.RPKM[rownames(MC.cluster2), ]
F.cluster3.MatRPKM <- F.Mat.RPKM[rownames(MC.cluster3), ]
F.cluster4.MatRPKM <- F.Mat.RPKM[rownames(MC.cluster4), ]
F.cluster5.MatRPKM <- F.Mat.RPKM[rownames(MC.cluster5), ]

M.cluster1.RPKM <- KO.M.comp.RPKM[rownames(MC.cluster1), ]
M.cluster2.RPKM <- KO.M.comp.RPKM[rownames(MC.cluster2), ]
M.cluster3.RPKM <- KO.M.comp.RPKM[rownames(MC.cluster3), ]
M.cluster4.RPKM <- KO.M.comp.RPKM[rownames(MC.cluster4), ]
M.cluster5.RPKM <- KO.M.comp.RPKM[rownames(MC.cluster5), ]



F_M.ratio.late2cell.cluster1 <- data.frame(ratio=rowMeans(F.cluster1.MatRPKM[ ,1:6], na.rm=T)/rowMeans(M.cluster1.RPKM[ ,1:17]), Stage="late2cell", catelog="cluster1")
F_M.ratio.4cell.cluster1 <- data.frame(ratio=rowMeans(F.cluster1.MatRPKM[ ,7:12], na.rm=T)/rowMeans(M.cluster1.RPKM[ ,18:24]), Stage="4cell", catelog="cluster1")
F_M.ratio.8cell.cluster1 <- data.frame(ratio=rowMeans(F.cluster1.MatRPKM[ ,13:18], na.rm=T)/rowMeans(M.cluster1.RPKM[ ,25:31]), Stage="8cell", catelog="cluster1")
F_M.ratio.eB.cluster1 <- data.frame(ratio=rowMeans(F.cluster1.MatRPKM[ ,19:25], na.rm=T)/rowMeans(M.cluster1.RPKM[ ,32:36]), Stage="earlyBlastocyst", catelog="cluster1")

F_M.ratio.late2cell.cluster2 <- data.frame(ratio=rowMeans(F.cluster2.MatRPKM[ ,1:6], na.rm=T)/rowMeans(M.cluster2.RPKM[ ,1:17]), Stage="late2cell", catelog="cluster2")
F_M.ratio.4cell.cluster2 <- data.frame(ratio=rowMeans(F.cluster2.MatRPKM[ ,7:12], na.rm=T)/rowMeans(M.cluster2.RPKM[ ,18:24]), Stage="4cell", catelog="cluster2")
F_M.ratio.8cell.cluster2 <- data.frame(ratio=rowMeans(F.cluster2.MatRPKM[ ,13:18], na.rm=T)/rowMeans(M.cluster2.RPKM[ ,25:31]), Stage="8cell", catelog="cluster2")
F_M.ratio.eB.cluster2 <- data.frame(ratio=rowMeans(F.cluster2.MatRPKM[ ,19:25], na.rm=T)/rowMeans(M.cluster2.RPKM[ ,32:36]), Stage="earlyBlastocyst", catelog="cluster2")

F_M.ratio.late2cell.cluster3 <- data.frame(ratio=rowMeans(F.cluster3.MatRPKM[ ,1:6], na.rm=T)/rowMeans(M.cluster3.RPKM[ ,1:17]), Stage="late2cell", catelog="cluster3")
F_M.ratio.4cell.cluster3 <- data.frame(ratio=rowMeans(F.cluster3.MatRPKM[ ,7:12], na.rm=T)/rowMeans(M.cluster3.RPKM[ ,18:24]), Stage="4cell", catelog="cluster3")
F_M.ratio.8cell.cluster3 <- data.frame(ratio=rowMeans(F.cluster3.MatRPKM[ ,13:18], na.rm=T)/rowMeans(M.cluster3.RPKM[ ,25:31]), Stage="8cell", catelog="cluster3")
F_M.ratio.eB.cluster3 <- data.frame(ratio=rowMeans(F.cluster3.MatRPKM[ ,19:25], na.rm=T)/rowMeans(M.cluster3.RPKM[ ,32:36]), Stage="earlyBlastocyst", catelog="cluster3")

F_M.ratio.late2cell.cluster4 <- data.frame(ratio=rowMeans(F.cluster4.MatRPKM[ ,1:6], na.rm=T)/rowMeans(M.cluster4.RPKM[ ,1:17]), Stage="late2cell", catelog="cluster4")
F_M.ratio.4cell.cluster4 <- data.frame(ratio=rowMeans(F.cluster4.MatRPKM[ ,7:12], na.rm=T)/rowMeans(M.cluster4.RPKM[ ,18:24]), Stage="4cell", catelog="cluster4")
F_M.ratio.8cell.cluster4 <- data.frame(ratio=rowMeans(F.cluster4.MatRPKM[ ,13:18], na.rm=T)/rowMeans(M.cluster4.RPKM[ ,25:31]), Stage="8cell", catelog="cluster4")
F_M.ratio.eB.cluster4 <- data.frame(ratio=rowMeans(F.cluster4.MatRPKM[ ,19:25], na.rm=T)/rowMeans(M.cluster4.RPKM[ ,32:36]), Stage="earlyBlastocyst", catelog="cluster4")

F_M.ratio.late2cell.cluster5 <- data.frame(ratio=rowMeans(F.cluster5.MatRPKM[ ,1:6], na.rm=T)/rowMeans(M.cluster5.RPKM[ ,1:17]), Stage="late2cell", catelog="cluster5")
F_M.ratio.4cell.cluster5 <- data.frame(ratio=rowMeans(F.cluster5.MatRPKM[ ,7:12], na.rm=T)/rowMeans(M.cluster5.RPKM[ ,18:24]), Stage="4cell", catelog="cluster5")
F_M.ratio.8cell.cluster5 <- data.frame(ratio=rowMeans(F.cluster5.MatRPKM[ ,13:18], na.rm=T)/rowMeans(M.cluster5.RPKM[ ,25:31]), Stage="8cell", catelog="cluster5")
F_M.ratio.eB.cluster5 <- data.frame(ratio=rowMeans(F.cluster5.MatRPKM[ ,19:25], na.rm=T)/rowMeans(M.cluster5.RPKM[ ,32:36]), Stage="earlyBlastocyst", catelog="cluster5")

All.dat <- rbind(F_M.ratio.late2cell.cluster1,
                 F_M.ratio.4cell.cluster1,
                 F_M.ratio.8cell.cluster1,
                 F_M.ratio.eB.cluster1,
                 F_M.ratio.late2cell.cluster2,
                 F_M.ratio.4cell.cluster2,
                 F_M.ratio.8cell.cluster2,
                 F_M.ratio.eB.cluster2,
                 F_M.ratio.late2cell.cluster3,
                 F_M.ratio.4cell.cluster3,
                 F_M.ratio.8cell.cluster3,
                 F_M.ratio.eB.cluster3,
                 F_M.ratio.late2cell.cluster4,
                 F_M.ratio.4cell.cluster4,
                 F_M.ratio.8cell.cluster4,
                 F_M.ratio.eB.cluster4,
                 F_M.ratio.late2cell.cluster5,
                 F_M.ratio.4cell.cluster5,
                 F_M.ratio.8cell.cluster5,
                 F_M.ratio.eB.cluster5)
All.dat$Stage <- factor(All.dat$Stage, levels=c("late2cell","4cell","8cell","earlyBlastocyst"))

ggplot(All.dat, aes(x=catelog, y=ratio, fill=Stage)) +
  geom_boxplot(alpha=0.7,outlier.shape=NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.7), 
             size=1.1, pch=21) +
  theme_bw() +
  coord_cartesian(ylim = c(0, 3.5)) +
  scale_fill_manual(values=brewer.pal(n = 5, name = "Accent")) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        axis.title.x=element_blank(),
        #legend.text=element_text(size=12),
        legend.title=element_blank(), 
        legend.key = element_blank(),
        axis.text.x=element_text(size = 12),
        legend.position = "top",
        legend.text=element_text(size = rel(1)), #face="bold"
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_line(color="black"),
        axis.line.x = element_line(color="black")) +
  geom_hline(yintercept=1,  colour="grey",linetype="longdash")

#calculate p-value, 4c and 8c in cluster1, p=0.2634761
dat <- rbind(F_M.ratio.4cell.cluster1, F_M.ratio.8cell.cluster1)
wilcox.test(ratio ~ Stage, data=dat, paired=T)$p.value



#calculate p-value, 8c and eB in cluster2, p=0.1484375
dat <- rbind(F_M.ratio.8cell.cluster2, F_M.ratio.eB.cluster2)
wilcox.test(ratio ~ Stage, data=dat, paired=T)$p.value

#calculate p-value, eB and 8c in cluster3, 0.705719
dat <- rbind(F_M.ratio.eB.cluster3, F_M.ratio.8cell.cluster3)
wilcox.test(ratio ~ Stage, data=dat, paired=T)$p.value

#calculate p-value, 4c and 8c in cluster4, p=0.173
dat <- rbind(F_M.ratio.4cell.cluster4, F_M.ratio.8cell.cluster4)
wilcox.test(ratio ~ Stage, data=dat, paired=T)$p.value

#calculate p-value, 4c and 8c in cluster5, p=0.3299828
dat <- rbind(F_M.ratio.4cell.cluster5, F_M.ratio.8cell.cluster5)
wilcox.test(ratio ~ Stage, data=dat, paired=T)$p.value



######### Figure 6h. X:A ratio of total RNA in female Xist-KO embryos ######################

source("Read_data.R")
rm(list=setdiff(ls(), c("GeneInfo","KO.comp.RPKM","KO.F.comp.RPKM", "KO.M.comp.RPKM")))
# get total autosomal gene and chrX gene (both polyA+ and PolyA-)
autoGenes <- rownames(GeneInfo[which(GeneInfo$Chr != "chrX" & GeneInfo$Chr != "chrY"), ])
chrXGenes <- rownames(GeneInfo[which(GeneInfo$Chr == "chrX"), ])



#Female data. Only consider genes with RPKM >=1
auto.F.RPKM <- KO.F.comp.RPKM[autoGenes, ]
chrX.F.RPKM <- KO.F.comp.RPKM[chrXGenes, ]


#late2cell female: sample 7,13,17,18,19,22
#late2cell - sample7
chrX.F.L2_7.RPKM <- chrX.F.RPKM$L2.Comp7[chrX.F.RPKM$L2.Comp7 >= 1]
auto.F.L2_7.RPKM <- auto.F.RPKM$L2.Comp7[auto.F.RPKM$L2.Comp7 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.L2_7.RPKM, length(chrX.F.L2_7.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.L2_7.RPKM) / mean(auto.random.F.RPKM)
}
#calculate mean and A/X ratio
F.XtoA <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Female", stringsAsFactors=FALSE)

#late2cell - sample13
chrX.F.L2_13.RPKM <- chrX.F.RPKM$L2.Comp13[chrX.F.RPKM$L2.Comp13 >= 1]
auto.F.L2_13.RPKM <- auto.F.RPKM$L2.Comp13[auto.F.RPKM$L2.Comp13 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.L2_13.RPKM, length(chrX.F.L2_13.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.L2_13.RPKM) / mean(auto.random.F.RPKM)
}
#calculate mean and A/X ratio
F.XtoA[2,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Female", stringsAsFactors=FALSE)

#late2cell - sample17
chrX.F.L2_17.RPKM <- chrX.F.RPKM$L2.Comp17[chrX.F.RPKM$L2.Comp17 >= 1]
auto.F.L2_17.RPKM <- auto.F.RPKM$L2.Comp17[auto.F.RPKM$L2.Comp17 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.L2_17.RPKM, length(chrX.F.L2_17.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.L2_17.RPKM) / mean(auto.random.F.RPKM)
}
#calculate mean and A/X ratio
F.XtoA[3,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Female", stringsAsFactors=FALSE)

#late2cell - sample18
chrX.F.L2_18.RPKM <- chrX.F.RPKM$L2.Comp18[chrX.F.RPKM$L2.Comp18 >= 1]
auto.F.L2_18.RPKM <- auto.F.RPKM$L2.Comp18[auto.F.RPKM$L2.Comp18 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.L2_18.RPKM, length(chrX.F.L2_18.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.L2_18.RPKM) / mean(auto.random.F.RPKM)
}
#calculate mean and A/X ratio
F.XtoA[4,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Female", stringsAsFactors=FALSE)

#late2cell - sample19
chrX.F.L2_19.RPKM <- chrX.F.RPKM$L2.Comp19[chrX.F.RPKM$L2.Comp19 >= 1]
auto.F.L2_19.RPKM <- auto.F.RPKM$L2.Comp19[auto.F.RPKM$L2.Comp19 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.L2_19.RPKM, length(chrX.F.L2_19.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.L2_19.RPKM) / mean(auto.random.F.RPKM)
}
#calculate mean and A/X ratio
F.XtoA[5,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Female", stringsAsFactors=FALSE)

#late2cell - sample22
chrX.F.L2_22.RPKM <- chrX.F.RPKM$L2.Comp22[chrX.F.RPKM$L2.Comp22 >= 1]
auto.F.L2_22.RPKM <- auto.F.RPKM$L2.Comp22[auto.F.RPKM$L2.Comp22 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.L2_22.RPKM, length(chrX.F.L2_22.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.L2_22.RPKM) / mean(auto.random.F.RPKM)
}
#calculate mean and A/X ratio
F.XtoA[6,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Female", stringsAsFactors=FALSE)


#4ell females: sample4 7 8 9 11 13
#4cell - sample4
chrX.F.4_4.RPKM <- chrX.F.RPKM$`4.Comp4`[chrX.F.RPKM$`4.Comp4` >= 1]
auto.F.4_4.RPKM <- auto.F.RPKM$`4.Comp4`[auto.F.RPKM$`4.Comp4` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.4_4.RPKM, length(chrX.F.4_4.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.4_4.RPKM) / mean(auto.random.F.RPKM)
}
#calculate mean and A/X ratio
F.XtoA[7,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Female", stringsAsFactors=FALSE)

#4cell - sample7
chrX.F.4_7.RPKM <- chrX.F.RPKM$`4.Comp7`[chrX.F.RPKM$`4.Comp7` >= 1]
auto.F.4_7.RPKM <- auto.F.RPKM$`4.Comp7`[auto.F.RPKM$`4.Comp7` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.4_7.RPKM, length(chrX.F.4_7.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.4_7.RPKM) / mean(auto.random.F.RPKM)
}
#calculate mean and A/X ratio
F.XtoA[8,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Female", stringsAsFactors=FALSE)

#4cell - sample8
chrX.F.4_8.RPKM <- chrX.F.RPKM$`4.Comp8`[chrX.F.RPKM$`4.Comp8` >= 1]
auto.F.4_8.RPKM <- auto.F.RPKM$`4.Comp8`[auto.F.RPKM$`4.Comp8` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.4_8.RPKM, length(chrX.F.4_8.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.4_8.RPKM) / mean(auto.random.F.RPKM)
}
#calculate mean and A/X ratio
F.XtoA[9,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Female", stringsAsFactors=FALSE)

#4cell - sample9
chrX.F.4_9.RPKM <- chrX.F.RPKM$`4.Comp9`[chrX.F.RPKM$`4.Comp9` >= 1]
auto.F.4_9.RPKM <- auto.F.RPKM$`4.Comp9`[auto.F.RPKM$`4.Comp9` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.4_9.RPKM, length(chrX.F.4_9.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.4_9.RPKM) / mean(auto.random.F.RPKM)
}
#calculate mean and A/X ratio
F.XtoA[10,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Female", stringsAsFactors=FALSE)

#4cell - sample11
chrX.F.4_11.RPKM <- chrX.F.RPKM$`4.Comp11`[chrX.F.RPKM$`4.Comp11` >= 1]
auto.F.4_11.RPKM <- auto.F.RPKM$`4.Comp11`[auto.F.RPKM$`4.Comp11` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.4_11.RPKM, length(chrX.F.4_11.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.4_11.RPKM) / mean(auto.random.F.RPKM)
}
#calculate mean and A/X ratio
F.XtoA[11,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Female", stringsAsFactors=FALSE)

#4cell - sample13
chrX.F.4_13.RPKM <- chrX.F.RPKM$`4.Comp13`[chrX.F.RPKM$`4.Comp13` >= 1]
auto.F.4_13.RPKM <- auto.F.RPKM$`4.Comp13`[auto.F.RPKM$`4.Comp13` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.4_13.RPKM, length(chrX.F.4_13.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.4_13.RPKM) / mean(auto.random.F.RPKM)
}
#calculate mean and A/X ratio
F.XtoA[12,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Female", stringsAsFactors=FALSE)


#8cell female samples: sample 1,5,7,10,11,13

#8cell - sample1
chrX.F.8_1.RPKM <- chrX.F.RPKM$`8.Comp1`[chrX.F.RPKM$`8.Comp1` >= 1]
auto.F.8_1.RPKM <- auto.F.RPKM$`8.Comp1`[auto.F.RPKM$`8.Comp1` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.8_1.RPKM, length(chrX.F.8_1.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.8_1.RPKM) / mean(auto.random.F.RPKM)
}
#calculate mean and A/X ratio
F.XtoA[13,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Female", stringsAsFactors=FALSE)

#8cell - sample5
chrX.F.8_5.RPKM <- chrX.F.RPKM$`8.Comp5`[chrX.F.RPKM$`8.Comp5` >= 1]
auto.F.8_5.RPKM <- auto.F.RPKM$`8.Comp5`[auto.F.RPKM$`8.Comp5` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.8_5.RPKM, length(chrX.F.8_5.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.8_5.RPKM) / mean(auto.random.F.RPKM)
}
#calculate mean and A/X ratio
F.XtoA[14,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Female", stringsAsFactors=FALSE)

#8cell - sample7
chrX.F.8_7.RPKM <- chrX.F.RPKM$`8.Comp7`[chrX.F.RPKM$`8.Comp7` >= 1]
auto.F.8_7.RPKM <- auto.F.RPKM$`8.Comp7`[auto.F.RPKM$`8.Comp7` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.8_7.RPKM, length(chrX.F.8_7.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.8_7.RPKM) / mean(auto.random.F.RPKM)
}
#calculate mean and A/X ratio
F.XtoA[15,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Female", stringsAsFactors=FALSE)

#8cell - sample10
chrX.F.8_10.RPKM <- chrX.F.RPKM$`8.Comp10`[chrX.F.RPKM$`8.Comp10` >= 1]
auto.F.8_10.RPKM <- auto.F.RPKM$`8.Comp10`[auto.F.RPKM$`8.Comp10` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.8_10.RPKM, length(chrX.F.8_10.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.8_10.RPKM) / mean(auto.random.F.RPKM)
}
#calculate mean and A/X ratio
F.XtoA[16,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Female", stringsAsFactors=FALSE)

#8cell - sample11
chrX.F.8_11.RPKM <- chrX.F.RPKM$`8.Comp11`[chrX.F.RPKM$`8.Comp11` >= 1]
auto.F.8_11.RPKM <- auto.F.RPKM$`8.Comp11`[auto.F.RPKM$`8.Comp11` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.8_11.RPKM, length(chrX.F.8_11.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.8_11.RPKM) / mean(auto.random.F.RPKM)
}
#calculate mean and A/X ratio
F.XtoA[17,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Female", stringsAsFactors=FALSE)

#8cell - sample13
chrX.F.8_13.RPKM <- chrX.F.RPKM$`8.Comp13`[chrX.F.RPKM$`8.Comp13` >= 1]
auto.F.8_13.RPKM <- auto.F.RPKM$`8.Comp13`[auto.F.RPKM$`8.Comp13` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.8_13.RPKM, length(chrX.F.8_13.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.8_13.RPKM) / mean(auto.random.F.RPKM)
}
#calculate mean and A/X ratio
F.XtoA[18,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Female", stringsAsFactors=FALSE)


#earlyBlast female samples: sample 2,3,4,9,10,11,12
#earlyBlast - sample2
chrX.F.eB_2.RPKM <- chrX.F.RPKM$`eB.Comp2`[chrX.F.RPKM$`eB.Comp2` >= 1]
auto.F.eB_2.RPKM <- auto.F.RPKM$`eB.Comp2`[auto.F.RPKM$`eB.Comp2` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.eB_2.RPKM, length(chrX.F.eB_2.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.eB_2.RPKM) / mean(auto.random.F.RPKM)
}
#calculate mean and A/X ratio
F.XtoA[19,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Female", stringsAsFactors=FALSE)

#earlyBlast - sample3
chrX.F.eB_3.RPKM <- chrX.F.RPKM$`eB.Comp3`[chrX.F.RPKM$`eB.Comp3` >= 1]
auto.F.eB_3.RPKM <- auto.F.RPKM$`eB.Comp3`[auto.F.RPKM$`eB.Comp3` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.eB_3.RPKM, length(chrX.F.eB_3.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.eB_3.RPKM) / mean(auto.random.F.RPKM)
}
#calculate mean and A/X ratio
F.XtoA[20,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Female", stringsAsFactors=FALSE)

#earlyBlast - sample4
chrX.F.eB_4.RPKM <- chrX.F.RPKM$`eB.Comp4`[chrX.F.RPKM$`eB.Comp4` >= 1]
auto.F.eB_4.RPKM <- auto.F.RPKM$`eB.Comp4`[auto.F.RPKM$`eB.Comp4` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.eB_4.RPKM, length(chrX.F.eB_4.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.eB_4.RPKM) / mean(auto.random.F.RPKM)
}
#calculate mean and A/X ratio
F.XtoA[21,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Female", stringsAsFactors=FALSE)

#earlyBlast - sample9
chrX.F.eB_9.RPKM <- chrX.F.RPKM$`eB.Comp9`[chrX.F.RPKM$`eB.Comp9` >= 1]
auto.F.eB_9.RPKM <- auto.F.RPKM$`eB.Comp9`[auto.F.RPKM$`eB.Comp9` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.eB_9.RPKM, length(chrX.F.eB_9.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.eB_9.RPKM) / mean(auto.random.F.RPKM)
}
#calculate mean and A/X ratio
F.XtoA[22,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Female", stringsAsFactors=FALSE)

#earlyBlast - sample10
chrX.F.eB_10.RPKM <- chrX.F.RPKM$`eB.Comp10`[chrX.F.RPKM$`eB.Comp10` >= 1]
auto.F.eB_10.RPKM <- auto.F.RPKM$`eB.Comp10`[auto.F.RPKM$`eB.Comp10` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.eB_10.RPKM, length(chrX.F.eB_10.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.eB_10.RPKM) / mean(auto.random.F.RPKM)
}
#calculate mean and A/X ratio
F.XtoA[23,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Female", stringsAsFactors=FALSE)

#earlyBlast - sample11
chrX.F.eB_11.RPKM <- chrX.F.RPKM$`eB.Comp11`[chrX.F.RPKM$`eB.Comp11` >= 1]
auto.F.eB_11.RPKM <- auto.F.RPKM$`eB.Comp11`[auto.F.RPKM$`eB.Comp11` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.eB_11.RPKM, length(chrX.F.eB_11.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.eB_11.RPKM) / mean(auto.random.F.RPKM)
}
#calculate mean and A/X ratio
F.XtoA[24,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Female", stringsAsFactors=FALSE)

#earlyBlast - sample12
chrX.F.eB_12.RPKM <- chrX.F.RPKM$`eB.Comp12`[chrX.F.RPKM$`eB.Comp12` >= 1]
auto.F.eB_12.RPKM <- auto.F.RPKM$`eB.Comp12`[auto.F.RPKM$`eB.Comp12` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.eB_12.RPKM, length(chrX.F.eB_12.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.eB_12.RPKM) / mean(auto.random.F.RPKM)
}
#calculate mean and A/X ratio
F.XtoA[25,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Female", stringsAsFactors=FALSE)






#Male data second
# get autosomal gene and chrX gene
auto.M.RPKM <- KO.M.comp.RPKM[autoGenes, ]
chrX.M.RPKM <- KO.M.comp.RPKM[chrXGenes, ]

#late2cell male: sample 1-6,8-12, 14-16, 20,21,23
#late2cell - sample1
chrX.M.L2_1.RPKM <- chrX.M.RPKM$L2.Comp1[chrX.M.RPKM$L2.Comp1 >= 1]
auto.M.L2_1.RPKM <- auto.M.RPKM$L2.Comp1[auto.M.RPKM$L2.Comp1 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.L2_1.RPKM, length(chrX.M.L2_1.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.L2_1.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#late2cell - sample2
chrX.M.L2_2.RPKM <- chrX.M.RPKM$L2.Comp2[chrX.M.RPKM$L2.Comp2 >= 1]
auto.M.L2_2.RPKM <- auto.M.RPKM$L2.Comp2[auto.M.RPKM$L2.Comp2 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.L2_2.RPKM, length(chrX.M.L2_2.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.L2_2.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[2,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#late2cell - sample3
chrX.M.L2_3.RPKM <- chrX.M.RPKM$L2.Comp3[chrX.M.RPKM$L2.Comp3 >= 1]
auto.M.L2_3.RPKM <- auto.M.RPKM$L2.Comp3[auto.M.RPKM$L2.Comp3 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.L2_3.RPKM, length(chrX.M.L2_3.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.L2_3.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[3,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#late2cell - sample4
chrX.M.L2_4.RPKM <- chrX.M.RPKM$L2.Comp4[chrX.M.RPKM$L2.Comp4 >= 1]
auto.M.L2_4.RPKM <- auto.M.RPKM$L2.Comp4[auto.M.RPKM$L2.Comp4 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.L2_4.RPKM, length(chrX.M.L2_4.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.L2_4.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[4,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#late2cell - sample5
chrX.M.L2_5.RPKM <- chrX.M.RPKM$L2.Comp5[chrX.M.RPKM$L2.Comp5 >= 1]
auto.M.L2_5.RPKM <- auto.M.RPKM$L2.Comp5[auto.M.RPKM$L2.Comp5 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.L2_5.RPKM, length(chrX.M.L2_5.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.L2_5.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[5,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#late2cell - sample6
chrX.M.L2_6.RPKM <- chrX.M.RPKM$L2.Comp6[chrX.M.RPKM$L2.Comp6 >= 1]
auto.M.L2_6.RPKM <- auto.M.RPKM$L2.Comp6[auto.M.RPKM$L2.Comp6 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.L2_6.RPKM, length(chrX.M.L2_6.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.L2_6.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[6,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#late2cell - sample8
chrX.M.L2_8.RPKM <- chrX.M.RPKM$L2.Comp8[chrX.M.RPKM$L2.Comp8 >= 1]
auto.M.L2_8.RPKM <- auto.M.RPKM$L2.Comp8[auto.M.RPKM$L2.Comp8 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.L2_8.RPKM, length(chrX.M.L2_8.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.L2_8.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[7,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#late2cell - sample9
chrX.M.L2_9.RPKM <- chrX.M.RPKM$L2.Comp9[chrX.M.RPKM$L2.Comp9 >= 1]
auto.M.L2_9.RPKM <- auto.M.RPKM$L2.Comp9[auto.M.RPKM$L2.Comp9 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.L2_9.RPKM, length(chrX.M.L2_9.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.L2_9.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[8,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#late2cell - sample10
chrX.M.L2_10.RPKM <- chrX.M.RPKM$L2.Comp10[chrX.M.RPKM$L2.Comp10 >= 1]
auto.M.L2_10.RPKM <- auto.M.RPKM$L2.Comp10[auto.M.RPKM$L2.Comp10 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.L2_10.RPKM, length(chrX.M.L2_10.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.L2_10.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[9,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#late2cell - sample11
chrX.M.L2_11.RPKM <- chrX.M.RPKM$L2.Comp11[chrX.M.RPKM$L2.Comp11 >= 1]
auto.M.L2_11.RPKM <- auto.M.RPKM$L2.Comp11[auto.M.RPKM$L2.Comp11 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.L2_11.RPKM, length(chrX.M.L2_11.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.L2_11.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[10,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#late2cell - sample12
chrX.M.L2_12.RPKM <- chrX.M.RPKM$L2.Comp12[chrX.M.RPKM$L2.Comp12 >= 1]
auto.M.L2_12.RPKM <- auto.M.RPKM$L2.Comp12[auto.M.RPKM$L2.Comp12 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.L2_12.RPKM, length(chrX.M.L2_12.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.L2_12.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[11,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#late2cell - sample14
chrX.M.L2_14.RPKM <- chrX.M.RPKM$L2.Comp14[chrX.M.RPKM$L2.Comp14 >= 1]
auto.M.L2_14.RPKM <- auto.M.RPKM$L2.Comp14[auto.M.RPKM$L2.Comp14 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.L2_14.RPKM, length(chrX.M.L2_14.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.L2_14.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[12,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#late2cell - sample15
chrX.M.L2_15.RPKM <- chrX.M.RPKM$L2.Comp15[chrX.M.RPKM$L2.Comp15 >= 1]
auto.M.L2_15.RPKM <- auto.M.RPKM$L2.Comp15[auto.M.RPKM$L2.Comp15 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.L2_15.RPKM, length(chrX.M.L2_15.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.L2_15.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[13,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#late2cell - sample16
chrX.M.L2_16.RPKM <- chrX.M.RPKM$L2.Comp16[chrX.M.RPKM$L2.Comp16 >= 1]
auto.M.L2_16.RPKM <- auto.M.RPKM$L2.Comp16[auto.M.RPKM$L2.Comp16 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.L2_16.RPKM, length(chrX.M.L2_16.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.L2_16.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[14,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#late2cell - sample20
chrX.M.L2_20.RPKM <- chrX.M.RPKM$L2.Comp20[chrX.M.RPKM$L2.Comp20 >= 1]
auto.M.L2_20.RPKM <- auto.M.RPKM$L2.Comp20[auto.M.RPKM$L2.Comp20 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.L2_20.RPKM, length(chrX.M.L2_20.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.L2_20.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[15,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#late2cell - sample21
chrX.M.L2_21.RPKM <- chrX.M.RPKM$L2.Comp21[chrX.M.RPKM$L2.Comp21 >= 1]
auto.M.L2_21.RPKM <- auto.M.RPKM$L2.Comp21[auto.M.RPKM$L2.Comp21 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.L2_21.RPKM, length(chrX.M.L2_21.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.L2_21.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[16,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#late2cell - sample23
chrX.M.L2_23.RPKM <- chrX.M.RPKM$L2.Comp23[chrX.M.RPKM$L2.Comp23 >= 1]
auto.M.L2_23.RPKM <- auto.M.RPKM$L2.Comp23[auto.M.RPKM$L2.Comp23 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.L2_23.RPKM, length(chrX.M.L2_23.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.L2_23.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[17,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)


#4cell male: 1,2,3,5,6,10,12
#4cell - sample1
chrX.M.4_1.RPKM <- chrX.M.RPKM$`4.Comp1`[chrX.M.RPKM$`4.Comp1` >= 1]
auto.M.4_1.RPKM <- auto.M.RPKM$`4.Comp1`[auto.M.RPKM$`4.Comp1` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.4_1.RPKM, length(chrX.M.4_1.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.4_1.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[18,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Male", stringsAsFactors=FALSE)

#4cell - sample2
chrX.M.4_2.RPKM <- chrX.M.RPKM$`4.Comp2`[chrX.M.RPKM$`4.Comp2` >= 1]
auto.M.4_2.RPKM <- auto.M.RPKM$`4.Comp2`[auto.M.RPKM$`4.Comp2` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.4_2.RPKM, length(chrX.M.4_2.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.4_2.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[19,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Male", stringsAsFactors=FALSE)

#4cell - sample3
chrX.M.4_3.RPKM <- chrX.M.RPKM$`4.Comp3`[chrX.M.RPKM$`4.Comp3` >= 1]
auto.M.4_3.RPKM <- auto.M.RPKM$`4.Comp3`[auto.M.RPKM$`4.Comp3` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.4_3.RPKM, length(chrX.M.4_3.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.4_3.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[20,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Male", stringsAsFactors=FALSE)

#4cell - sample5
chrX.M.4_5.RPKM <- chrX.M.RPKM$`4.Comp5`[chrX.M.RPKM$`4.Comp5` >= 1]
auto.M.4_5.RPKM <- auto.M.RPKM$`4.Comp5`[auto.M.RPKM$`4.Comp5` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.4_5.RPKM, length(chrX.M.4_5.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.4_5.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[21,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Male", stringsAsFactors=FALSE)

#4cell - sample6
chrX.M.4_6.RPKM <- chrX.M.RPKM$`4.Comp6`[chrX.M.RPKM$`4.Comp6` >= 1]
auto.M.4_6.RPKM <- auto.M.RPKM$`4.Comp6`[auto.M.RPKM$`4.Comp6` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.4_6.RPKM, length(chrX.M.4_6.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.4_6.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[22,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Male", stringsAsFactors=FALSE)

#4cell - sample10
chrX.M.4_10.RPKM <- chrX.M.RPKM$`4.Comp10`[chrX.M.RPKM$`4.Comp10` >= 1]
auto.M.4_10.RPKM <- auto.M.RPKM$`4.Comp10`[auto.M.RPKM$`4.Comp10` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.4_10.RPKM, length(chrX.M.4_10.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.4_10.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[23,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Male", stringsAsFactors=FALSE)

#4cell - sample12
chrX.M.4_12.RPKM <- chrX.M.RPKM$`4.Comp12`[chrX.M.RPKM$`4.Comp12` >= 1]
auto.M.4_12.RPKM <- auto.M.RPKM$`4.Comp12`[auto.M.RPKM$`4.Comp12` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.4_12.RPKM, length(chrX.M.4_12.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.4_12.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[24,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Male", stringsAsFactors=FALSE)


#8cell male: 2,3,4,6,8,9,12
#8cell - sample2
chrX.M.8_2.RPKM <- chrX.M.RPKM$`8.Comp2`[chrX.M.RPKM$`8.Comp2` >= 1]
auto.M.8_2.RPKM <- auto.M.RPKM$`8.Comp2`[auto.M.RPKM$`8.Comp2` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.8_2.RPKM, length(chrX.M.8_2.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.8_2.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[25,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Male", stringsAsFactors=FALSE)

#8cell - sample3
chrX.M.8_3.RPKM <- chrX.M.RPKM$`8.Comp3`[chrX.M.RPKM$`8.Comp3` >= 1]
auto.M.8_3.RPKM <- auto.M.RPKM$`8.Comp3`[auto.M.RPKM$`8.Comp3` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.8_3.RPKM, length(chrX.M.8_3.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.8_3.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[26,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Male", stringsAsFactors=FALSE)

#8cell - sample4
chrX.M.8_4.RPKM <- chrX.M.RPKM$`8.Comp4`[chrX.M.RPKM$`8.Comp4` >= 1]
auto.M.8_4.RPKM <- auto.M.RPKM$`8.Comp4`[auto.M.RPKM$`8.Comp4` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.8_4.RPKM, length(chrX.M.8_4.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.8_4.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[27,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Male", stringsAsFactors=FALSE)

#8cell - sample6
chrX.M.8_6.RPKM <- chrX.M.RPKM$`8.Comp6`[chrX.M.RPKM$`8.Comp6` >= 1]
auto.M.8_6.RPKM <- auto.M.RPKM$`8.Comp6`[auto.M.RPKM$`8.Comp6` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.8_6.RPKM, length(chrX.M.8_6.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.8_6.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[28,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Male", stringsAsFactors=FALSE)

#8cell - sample8
chrX.M.8_8.RPKM <- chrX.M.RPKM$`8.Comp8`[chrX.M.RPKM$`8.Comp8` >= 1]
auto.M.8_8.RPKM <- auto.M.RPKM$`8.Comp8`[auto.M.RPKM$`8.Comp8` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.8_8.RPKM, length(chrX.M.8_8.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.8_8.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[29,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Male", stringsAsFactors=FALSE)

#8cell - sample9
chrX.M.8_9.RPKM <- chrX.M.RPKM$`8.Comp9`[chrX.M.RPKM$`8.Comp9` >= 1]
auto.M.8_9.RPKM <- auto.M.RPKM$`8.Comp9`[auto.M.RPKM$`8.Comp9` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.8_9.RPKM, length(chrX.M.8_9.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.8_9.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[30,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Male", stringsAsFactors=FALSE)

#8cell - sample12
chrX.M.8_12.RPKM <- chrX.M.RPKM$`8.Comp12`[chrX.M.RPKM$`8.Comp12` >= 1]
auto.M.8_12.RPKM <- auto.M.RPKM$`8.Comp12`[auto.M.RPKM$`8.Comp12` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.8_12.RPKM, length(chrX.M.8_12.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.8_12.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[31,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Male", stringsAsFactors=FALSE)

#earlyBlast male: 1,5,6,7,8
#earlyBlast - sample1
chrX.M.eB_1.RPKM <- chrX.M.RPKM$`eB.Comp1`[chrX.M.RPKM$`eB.Comp1` >= 1]
auto.M.eB_1.RPKM <- auto.M.RPKM$`eB.Comp1`[auto.M.RPKM$`eB.Comp1` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.eB_1.RPKM, length(chrX.M.eB_1.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.eB_1.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[32,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Male", stringsAsFactors=FALSE)

#earlyBlast - sample5
chrX.M.eB_5.RPKM <- chrX.M.RPKM$`eB.Comp5`[chrX.M.RPKM$`eB.Comp5` >= 1]
auto.M.eB_5.RPKM <- auto.M.RPKM$`eB.Comp5`[auto.M.RPKM$`eB.Comp5` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.eB_5.RPKM, length(chrX.M.eB_5.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.eB_5.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[33,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Male", stringsAsFactors=FALSE)

#earlyBlast - sample6
chrX.M.eB_6.RPKM <- chrX.M.RPKM$`eB.Comp6`[chrX.M.RPKM$`eB.Comp6` >= 1]
auto.M.eB_6.RPKM <- auto.M.RPKM$`eB.Comp6`[auto.M.RPKM$`eB.Comp6` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.eB_6.RPKM, length(chrX.M.eB_6.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.eB_6.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[34,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Male", stringsAsFactors=FALSE)

#earlyBlast - sample7
chrX.M.eB_7.RPKM <- chrX.M.RPKM$`eB.Comp7`[chrX.M.RPKM$`eB.Comp7` >= 1]
auto.M.eB_7.RPKM <- auto.M.RPKM$`eB.Comp7`[auto.M.RPKM$`eB.Comp7` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.eB_7.RPKM, length(chrX.M.eB_7.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.eB_7.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[35,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Male", stringsAsFactors=FALSE)

#earlyBlast - sample8
chrX.M.eB_8.RPKM <- chrX.M.RPKM$`eB.Comp8`[chrX.M.RPKM$`eB.Comp8` >= 1]
auto.M.eB_8.RPKM <- auto.M.RPKM$`eB.Comp8`[auto.M.RPKM$`eB.Comp8` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.eB_8.RPKM, length(chrX.M.eB_8.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.eB_8.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[36,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Male", stringsAsFactors=FALSE)


#clean up and combine Male and Female X/A ratio
rm(list=setdiff(ls(), c("M.XtoA","F.XtoA","F.TSC.XtoA","GeneInfo",
                        "KO.comp.RPKM","KO.F.comp.RPKM","KO.M.comp.RPKM",
                        "polyA.minus","plot.dat.without","autoGenes","chrXGenes")))


#M.XtoA <- M.XtoA[which(M.XtoA$Stage !="e2C"), ]
#F.XtoA <- F.XtoA[which(F.XtoA$Stage !="e2C"), ]
plot.dat <- rbind(M.XtoA, F.XtoA)
plot.dat$Stage <- factor(plot.dat$Stage, levels = c("l2C","4C","8C","16C","eB"))

library(ggplot2)
ggplot(plot.dat, aes(x = Stage, y = Ratio, fill = Gender)) +
  geom_boxplot(alpha=0.4, outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.7), 
             aes(fill=Gender), size=2, pch=21) +
  scale_fill_brewer(palette = "Accent") +
  scale_color_brewer(palette = "Accent") +
  theme_bw() +
  labs(y="X:A Ratio") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        axis.title.x=element_blank(),
        #legend.text=element_text(size=12),
        legend.title=element_blank(), 
        legend.key = element_blank(),
        axis.text.x=element_text(size = 12),
        legend.position = "top",
        legend.text=element_text(size = rel(1)), #face="bold"
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_line(color="black"),
        axis.line.x = element_line(color="black")) +
  scale_y_continuous(limits = c(0.55,1.2),breaks = c(0.6,0.8,1,1.2)) +
  geom_hline(yintercept=1,  colour="grey",linetype="longdash")



