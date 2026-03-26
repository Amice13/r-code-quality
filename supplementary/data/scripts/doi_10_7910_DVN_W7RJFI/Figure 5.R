#Figure 5

rm(list=ls())
library(SingleCellExperiment)
library(scran)
library(scater)
#scRNA-seq gene UMI count data is from GSE107644.(https://www.nature.com/articles/s41422-018-0074-y#Sec44)
meta.data <- read.csv(file="metadata.csv", header=T)
SC01 <- read.table(file="GSM2874055_SpSC01_UMIcount.txt", sep="\t",header=T, row.names=1)
SC02 <- read.table(file="GSM2874056_SpSC2_UMIcount.txt", sep="\t",header=T, row.names=1)
SC03 <- read.table(file="GSM2874057_SpSC3_UMIcount.txt", sep="\t",header=T, row.names=1)
SC04 <- read.table(file="GSM2874058_SpSC4_UMIcount.txt", sep="\t",header=T, row.names=1)
SC05 <- read.table(file="GSM2874059_SpSC5_UMIcount.txt", sep="\t",header=T, row.names=1)
SC06 <- read.table(file="GSM2874060_SpSC6_UMIcount.txt", sep="\t",header=T, row.names=1)
SC07 <- read.table(file="GSM2874061_SpSC7_UMIcount.txt", sep="\t",header=T, row.names=1)
SC08 <- read.table(file="GSM2874062_SpSC8_UMIcount.txt", sep="\t",header=T, row.names=1)
SC09 <- read.table(file="GSM2874063_SpSC9_UMIcount.txt", sep="\t",header=T, row.names=1)
SC10 <- read.table(file="GSM2874064_SpSC10_UMIcount.txt", sep="\t",header=T, row.names=1)
SC11 <- read.table(file="GSM2874065_SpSC11_UMIcount.txt", sep="\t",header=T, row.names=1)
SC12 <- read.table(file="GSM2874066_SpSC12_UMIcount.txt", sep="\t",header=T, row.names=1)
SC13 <- read.table(file="GSM2874067_SpSC13_UMIcount.txt", sep="\t",header=T, row.names=1)
SC14 <- read.table(file="GSM2874068_SpSC14_UMIcount.txt", sep="\t",header=T, row.names=1)
SC15 <- read.table(file="GSM2874069_SpSC15_UMIcount.txt", sep="\t",header=T, row.names=1)
SC16 <- read.table(file="GSM2874070_SpSC16_UMIcount.txt", sep="\t",header=T, row.names=1)
SC17 <- read.table(file="GSM2874071_SpSC17_UMIcount.txt", sep="\t",header=T, row.names=1)
SC18 <- read.table(file="GSM2874072_SpSC18_UMIcount.txt", sep="\t",header=T, row.names=1)
SC19 <- read.table(file="GSM2874073_SpSC19_UMIcount.txt", sep="\t",header=T, row.names=1)
SC20 <- read.table(file="GSM2874074_SpSC20_UMIcount.txt", sep="\t",header=T, row.names=1)
SC21 <- read.table(file="GSM2874075_SpSC21_UMIcount.txt", sep="\t",header=T, row.names=1)
SC22 <- read.table(file="GSM2874076_SpSC22_UMIcount.txt", sep="\t",header=T, row.names=1)
SC24 <- read.table(file="GSM2874077_SpSC24_UMIcount.txt", sep="\t",header=T, row.names=1)
SC25 <- read.table(file="GSM2874078_SpSC25_UMIcount.txt", sep="\t",header=T, row.names=1)
SC26 <- read.table(file="GSM2874079_SpSC26_UMIcount.txt", sep="\t",header=T, row.names=1)
SC27 <- read.table(file="GSM2874080_SpSC27_UMIcount.txt", sep="\t",header=T, row.names=1)
SC28 <- read.table(file="GSM2874081_SpSC28_UMIcount.txt", sep="\t",header=T, row.names=1)
SC29 <- read.table(file="GSM2874082_SpSC29_UMIcount.txt", sep="\t",header=T, row.names=1)
SC30 <- read.table(file="GSM2874083_SpSC30_UMIcount.txt", sep="\t",header=T, row.names=1)
SC31 <- read.table(file="GSM2874084_SpSC31_UMIcount.txt", sep="\t",header=T, row.names=1)
SC32 <- read.table(file="GSM2874085_SpSC32_UMIcount.txt", sep="\t",header=T, row.names=1)
SC33 <- read.table(file="GSM2874086_SpSC33_UMIcount.txt", sep="\t",header=T, row.names=1)
SC34 <- read.table(file="GSM2874087_SpSC34_UMIcount.txt", sep="\t",header=T, row.names=1)
SC35 <- read.table(file="GSM2874088_SpSC35_UMIcount.txt", sep="\t",header=T, row.names=1)

SC.total <- data.matrix(cbind(SC01, SC02, SC03, SC04, SC05, SC06, SC07, SC08, SC09, SC10,
                              SC11, SC12, SC13, SC14, SC15, SC16, SC17, SC18, SC19, SC20,
                              SC21, SC22, SC24, SC25, SC26, SC27, SC28, SC29, SC30,
                              SC31, SC32, SC33, SC34, SC35))

rm(SC01, SC02, SC03, SC04, SC05, SC06, SC07, SC08, SC09, SC10, SC11, SC12, SC13, SC14, SC15, 
   SC16, SC17, SC18, SC19, SC20, SC21, SC22, SC24, SC25, SC26, SC27, SC28, SC29, SC30, SC31,
   SC32, SC33, SC34, SC35)
#low quality cells has been filtered out in the meta.data by the authors. No need to do it again.
#The meta.data was generated from Supp. table 2

meta.cell <- meta.data$cell
#filter low quality cells
SC.filter <- SC.total[ ,meta.cell]
#create SCE
SC.data <- SingleCellExperiment(assays=SimpleList(counts=SC.filter),
                                colData=meta.data)
rm(SC.total)
rm(SC.filter)

#normalization by deconvolution
set.seed(100)
SC.cluster <- quickCluster(SC.data)

# Scaling endogenous genes using deconvolution size factors
SC.deconv.SumFactors <- calculateSumFactors(SC.data, min.mean=0.1, 
                                          assay.type="counts",
                                          cluster=SC.cluster)
summary(SC.deconv.SumFactors)
#add normalized log and count value.
SC.normData <- logNormCounts(SC.data, size_factors=SC.deconv.SumFactors)
SC.normCounts <- logNormCounts(SC.data, size_factors=SC.deconv.SumFactors, log=F)
assay(SC.normData, "normcounts") <- normcounts(SC.normCounts)
rm(SC.normCounts)

colLabels(SC.normData) <- SC.normData$cluster
rm(SC.data)

#to acquire the data matrix
SC.normlogCounts <- logcounts(SC.normData)

#check marker genes in cells at different stages.
test.genes <- c("Ccnd2", "Cenpa", "Stra8","Nacad","Myl7", "Gm960", "Meiob", "Psma8", "Piwil1",
                "Pou5f2","Ccna1","Tex36","Sun5","Prm1","Cstl1")
dat <- SC.normlogCounts[test.genes, ]
library(pheatmap)
pheatmap(dat, cluster_rows=F, cluster_cols=F, show_rownames = T, show_colnames = F)

#dimention reduction
#SC.normData <- runPCA(SC.normData, exprs_values = "logcounts", ntop=746)
#SC.normData <- runTSNE(SC.normData, exprs_values = "logcounts", dimred = "PCA", perplexity = 0.1)
#plotReducedDim(SC.normData, dimred="PCA", colour_by="type")


############# Figure 5a scRNA-seq heatmap during spermatogenesis ####################
#Load X-linked gene categories.
CM.clustered.genes <- read.table(file="Attached_doc/CM.Gene_Category.v2.txt", sep="\t")
cluster1 <- CM.clustered.genes[which(CM.clustered.genes$category == "Early"), ]
cluster2 <- CM.clustered.genes[which(CM.clustered.genes$category == "Mid"), ]
cluster3 <- CM.clustered.genes[which(CM.clustered.genes$category == "Late"), ]
cluster4 <- CM.clustered.genes[which(CM.clustered.genes$category == "Constitutive"), ]
cluster5 <- CM.clustered.genes[which(CM.clustered.genes$category == "Escapee"), ]
cluster6 <- CM.clustered.genes[which(CM.clustered.genes$category == "Strain-bias"), ]

cluster.genes <- rownames (rbind(cluster1, cluster2, cluster3, cluster4, cluster5, cluster6))
log.dat <- SC.normlogCounts[cluster.genes, ]
library(RColorBrewer)

annot_cluster <- data.frame(Category=meta.data$type, row.names=meta.data$cell)
cols <- colorRampPalette(brewer.pal(6, "Set2")); 
mycolors <- cols(length(unique(annot_cluster$Category)));
names(mycolors) <- unique(annot_cluster$Category)
mycolors <- list(Category = mycolors)

#Generate heatmap using log value of gene count 
#To match the color pattern used in other figures. Blue color would represent the biggest value and red represents the smallest 
#By default, pheatmap would choose color with the opposite meaning. 
#to change the color, use 11-log value, and reverse the color scales.
new.log.dat <- 11-log.dat
breaksList = seq(0, 11, by = 0.1)
pheatmap(new.log.dat, cluster_rows=F, cluster_cols=F, show_rownames = F, show_colnames = F, gaps_row=c(25,33,50,74,94), annotation_col = annot_cluster,
         annotation_colors = mycolors,color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(length(breaksList)), breaks = breaksList)

####check some autosomal genes for comparison, only use for the first time to select autosomal genes.####################
#It is random selection.
chr1 <- read.table(file="chr1.bed", sep="\t",row.names=4)  
chr1.genes <- intersect(rownames(chr1), rownames(SC.normlogCounts))
chr13 <- read.table(file="chr13.bed", sep="\t",row.names=4)
chr13.genes <- intersect(rownames(chr13), rownames(SC.normlogCounts))
#pick up 20 randome genes from each chromosome. Alternatively, All chr1 and chr13 genes can be used for comparison (Figure S6A).
chr1.selected.genes <- sample(chr1.genes, 20)
chr13.selected.genes <- sample(chr13.genes, 20)
auto.genes <- c(chr1.selected.genes, chr13.selected.genes)
log.auto.dat <- SC.normlogCounts[auto.genes, ]
new.log.auto.dat <- 11-log.auto.dat
#save the selected autosomal genes for later plot.
#write.table(new.log.auto.dat, file="SCselected_autosomal_genes.txt", quote=F, row.names=T, col.names=T)

#combine both chrX and auto genes. Auto genes served for comparison to show MSCI specific for chrX genes.
new.combined.dat <- rbind(new.log.dat, new.log.auto.dat)
pheatmap(new.combined.dat, cluster_rows=F, cluster_cols=F, show_rownames = F, show_colnames = F, gaps_row=c(25,33,50,74,94,100,120), annotation_col = annot_cluster,
         annotation_colors = mycolors,color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(length(breaksList)), breaks = breaksList)

############### Figure 5b. Calculate the average value for each category.

###### 2.1 use logcount value for average:
auto.gene.log <- log.auto.dat
chrX.gene.log <- log.dat
gene.log <- rbind(chrX.gene.log, auto.gene.log)

#calculate the size of each cell types
annot_cluster$Category <- factor(annot_cluster$Category, 
                                 levels=c("A1","In","BS","BG2","G1","ePL","mPL","lPL","L","Z","eP","mP","IP","D","MI","MII","RS2","RS4","RS6","RS8"))
Sample.size <- table(annot_cluster$Category)
expressMean.log <- data.frame(rowMeans(gene.log[ ,c(1:Sample.size[1])]))
start <- as.numeric(Sample.size[1])
for (i in 2:length(Sample.size)) {
  start <- start + 1
  end <- start + Sample.size[i] -1
  expressMean.log[ ,i] <- rowMeans(gene.log[ ,c(start: end)])
  start <- end
}
colnames(expressMean.log) <- names(Sample.size)

dat <- rbind(colMeans(expressMean.log[c(1:25), ]), 
             colMeans(expressMean.log[c(26:33), ]), 
             colMeans(expressMean.log[c(34:50), ]), 
             colMeans(expressMean.log[c(51:74), ]),
             colMeans(expressMean.log[c(75:94), ]), 
             colMeans(expressMean.log[c(95:100), ]),
             colMeans(expressMean.log[c(101:120), ]),
             colMeans(expressMean.log[c(121:140), ]))
rownames(dat) <- c("cat1","cat2","cat3","cat4","cat5","cat6","chr1","chr13")

cluster1.dat <- data.frame(expression=dat[1, ], stage=colnames(dat), category="Cat.1")
cluster2.dat <- data.frame(expression=dat[2, ], stage=colnames(dat), category="Cat.2")
cluster3.dat <- data.frame(expression=dat[3, ], stage=colnames(dat), category="Cat.3")
cluster4.dat <- data.frame(expression=dat[4, ], stage=colnames(dat), category="Cat.4")
cluster5.dat <- data.frame(expression=dat[5, ], stage=colnames(dat), category="Cat.5")
cluster6.dat <- data.frame(expression=dat[6, ], stage=colnames(dat), category="Cat.6")
chr1.dat <- data.frame(expression=dat[7, ], stage=colnames(dat), category="chr1")
chr13.dat <- data.frame(expression=dat[8, ], stage=colnames(dat), category="chr13")

total.dat <- rbind(cluster1.dat, cluster2.dat, cluster3.dat, cluster4.dat, cluster5.dat, cluster6.dat, chr1.dat, chr13.dat)
total.dat$stage <- factor(total.dat$stage, 
                                 levels=c("A1","In","BS","BG2","G1","ePL","mPL","lPL","L","Z","eP","mP","IP","D","MI","MII","RS2","RS4","RS6","RS8"))

theme_pattern <- theme(axis.title.x=element_blank(),
                       legend.text=element_text(size=12),
                       legend.title=element_blank(),
                       legend.key = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       axis.line = element_line(color = 'black'),
                       axis.line.y = element_line(color="black"),
                       axis.line.x = element_line(color="black"),
                       legend.background = element_rect(fill=alpha('NA', 0.2)))


ggplot(total.dat, aes(x=stage, y=expression, group=category)) +
  geom_line(aes(color=category)) +
  geom_point(aes(color=category)) +
  theme_bw() +
  theme_pattern +
  scale_color_brewer(palette="Dark2")

###### 2.2 average normalized counts, then take log value :
chrX.gene.counts <- normcounts(SC.normData)[cluster.genes, ]
auto.gene.counts <- normcounts(SC.normData)[rownames(new.log.auto.dat), ]
gene.counts <- rbind(chrX.gene.counts, auto.gene.counts)

#calculate the size of each cell types
annot_cluster$Category <- factor(annot_cluster$Category, 
                                 levels=c("A1","In","BS","BG2","G1","ePL","mPL","lPL","L","Z","eP","mP","IP","D","MI","MII","RS2","RS4","RS6","RS8"))
Sample.size <- table(annot_cluster$Category)
expressMean.count <- data.frame(rowMeans(gene.counts[ ,c(1:Sample.size[1])]))
start <- as.numeric(Sample.size[1])
for (i in 2:length(Sample.size)) {
  start <- start + 1
  end <- start + Sample.size[i] -1
  expressMean.count[ ,i] <- rowMeans(gene.counts[ ,c(start: end)])
  start <- end
}
colnames(expressMean.count) <- names(Sample.size)

dat <- rbind(colMeans(expressMean.count[c(1:25), ]), 
             colMeans(expressMean.count[c(26:33), ]), 
             colMeans(expressMean.count[c(34:50), ]), 
             colMeans(expressMean.count[c(51:74), ]),
             colMeans(expressMean.count[c(75:94), ]), 
             colMeans(expressMean.count[c(95:100), ]),
             colMeans(expressMean.count[c(101:120), ]),
             colMeans(expressMean.count[c(121:140), ]))
rownames(dat) <- c("cat1","cat2","cat3","cat4","cat5","cat6","chr1","chr13")

cluster1.dat <- data.frame(expression=dat[1, ], stage=colnames(dat), category="Cat.1")
cluster2.dat <- data.frame(expression=dat[2, ], stage=colnames(dat), category="Cat.2")
cluster3.dat <- data.frame(expression=dat[3, ], stage=colnames(dat), category="Cat.3")
cluster4.dat <- data.frame(expression=dat[4, ], stage=colnames(dat), category="Cat.4")
cluster5.dat <- data.frame(expression=dat[5, ], stage=colnames(dat), category="Cat.5")
cluster6.dat <- data.frame(expression=dat[6, ], stage=colnames(dat), category="Cat.6")
chr1.dat <- data.frame(expression=dat[7, ], stage=colnames(dat), category="chr1")
chr13.dat <- data.frame(expression=dat[8, ], stage=colnames(dat), category="chr13")

total.dat <- rbind(cluster1.dat, cluster2.dat, cluster3.dat, cluster4.dat, cluster5.dat, cluster6.dat, chr1.dat, chr13.dat)
total.dat$stage <- factor(total.dat$stage, 
                          levels=c("A1","In","BS","BG2","G1","ePL","mPL","lPL","L","Z","eP","mP","IP","D","MI","MII","RS2","RS4","RS6","RS8"))
total.dat$expression <- log(total.dat$expression)

theme_pattern <- theme(axis.title.x=element_blank(),
                       legend.text=element_text(size=12),
                       legend.title=element_blank(),
                       legend.key = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       axis.line = element_line(color = 'black'),
                       axis.line.y = element_line(color="black"),
                       axis.line.x = element_line(color="black"),
                       legend.background = element_rect(fill=alpha('NA', 0.2)))


ggplot(total.dat, aes(x=stage, y=expression, group=category)) +
  geom_line(aes(color=category)) +
  geom_point(aes(color=category)) +
  theme_bw() +
  theme_pattern +
  scale_color_brewer(palette="Dark2")


######2.3 determine statistical p value for comparison of expression between X-linked genes and autosomal genes at eP, mP and lP.

# Only consider expressed genes with counts larger than 1.
expressMean.count[c(1:100) ,"chr"] <- "chrX"
expressMean.count[c(101:140) ,"chr"] <- "auto"
pachytene.counts <- expressMean.count[ ,c(11:13,21)]
#at eP
eP.counts <- data.frame(pachytene.counts[ ,c(1,4)], stage="eP")
eP.counts.expressed <- eP.counts[which(eP.counts$eP > 1), ]
#at mP
mP.counts <- data.frame(pachytene.counts[ ,c(2,4)], stage="mP")
mP.counts.expressed <- mP.counts[which(mP.counts$mP > 1), ]
#at lP
lP.counts <- data.frame(pachytene.counts[ ,c(3,4)], stage="lP")
lP.counts.expressed <- lP.counts[which(lP.counts$IP > 1), ]


#Calculated from Section 2.1
expressMean.log[c(1:100) ,"chr"] <- "chrX"
expressMean.log[c(101:140) ,"chr"] <- "auto"
eP.log <- data.frame(expressMean.log[ ,c(11,21)], stage="eP")
eP.log.expressed <- eP.log[rownames(eP.counts.expressed), ]
mP.log <- data.frame(expressMean.log[ ,c(12,21)], stage="mP")
mP.log.expressed <- mP.log[rownames(mP.counts.expressed), ]
lP.log <- data.frame(expressMean.log[ ,c(13,21)], stage="lP")
lP.log.expressed <- lP.log[rownames(lP.counts.expressed), ]

#p-values could vary depending on the randomly selected autosomal genes. 
#eP, chrX vs auto
wilcox.test(eP ~chr, data=eP.log.expressed)
#mP, chrX vs auto
wilcox.test(mP ~chr, data=mP.log.expressed)
#lP, chrX vs auto
wilcox.test(IP ~chr, data=lP.log.expressed)





############## Figure 5c expression FC from MII to RS8 ##########################
#1.Use expressMean value (mean gene counts) from gene counts, not log value
expressMean.dat <- expressMean.count
cluster1.dat <- data.frame(MII=expressMean.dat[c(1:25), 16], RS8=expressMean.dat[c(1:25), 20], FoldChange=expressMean.dat[c(1:25), 20]/expressMean.dat[c(1:25), 16], category="Cat.1",name=cluster.genes[1:25])
cluster2.dat <- data.frame(MII=expressMean.dat[c(26:33), 16], RS8=expressMean.dat[c(26:33), 20], FoldChange=expressMean.dat[c(26:33), 20]/expressMean.dat[c(26:33), 16], category="Cat.2",name=cluster.genes[26:33])
cluster3.dat <- data.frame(MII=expressMean.dat[c(34:50), 16], RS8=expressMean.dat[c(34:50), 20], FoldChange=expressMean.dat[c(34:50), 20]/expressMean.dat[c(34:50), 16], category="Cat.3",name=cluster.genes[34:50])
cluster4.dat <- data.frame(MII=expressMean.dat[c(51:74), 16], RS8=expressMean.dat[c(51:74), 20], FoldChange=expressMean.dat[c(51:74), 20]/expressMean.dat[c(51:74), 16], category="Cat.4",name=cluster.genes[51:74])
cluster5.dat <- data.frame(MII=expressMean.dat[c(75:94), 16], RS8=expressMean.dat[c(75:94), 20], FoldChange=expressMean.dat[c(75:94), 20]/expressMean.dat[c(75:94), 16], category="Cat.5",name=cluster.genes[75:94])
cluster6.dat <- data.frame(MII=expressMean.dat[c(95:100), 16], RS8=expressMean.dat[c(95:100), 20], FoldChange=expressMean.dat[c(95:100), 20]/expressMean.dat[c(95:100), 16], category="Cat.6",name=cluster.genes[95:100])
dat <- rbind(cluster1.dat, cluster2.dat, cluster3.dat, cluster4.dat, cluster5.dat, cluster6.dat)


# we look at genes with with at least 1 transcript at MII
cluster1.dat.filtered <- cluster1.dat[which(cluster1.dat$MII > 0), ]
cluster2.dat.filtered <- cluster2.dat[which(cluster2.dat$MII > 0), ]
cluster3.dat.filtered <- cluster3.dat[which(cluster3.dat$MII > 0), ]
cluster4.dat.filtered <- cluster4.dat[which(cluster4.dat$MII > 0), ]
cluster5.dat.filtered <- cluster5.dat[which(cluster5.dat$MII > 0), ]
cluster6.dat.filtered <- cluster6.dat[which(cluster6.dat$MII > 0), ]


dat.filtered <- rbind(cluster1.dat.filtered, cluster2.dat.filtered, cluster3.dat.filtered, cluster4.dat.filtered, cluster5.dat.filtered, cluster6.dat.filtered)
dat.filtered$name <- factor(dat.filtered$name, levels=dat.filtered$name)


ggplot(dat.filtered, aes(x=category, y=FoldChange)) +
  geom_boxplot(fill='dodgerblue', outlier.shape=NA, alpha=0.7) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 1), 
             aes(fill='dodgerblue'), size=1.5, pch=21) +
  theme_bw() +
  coord_cartesian(ylim = c(0,10)) +
  scale_fill_manual(values="dodgerblue") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(angle=45,hjust=1),
        legend.text=element_blank(),
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

# p-value
#1vs 4, p=0.02253967
dat <- rbind(cluster1.dat.filtered, cluster4.dat.filtered)
wilcox.test(FoldChange ~ category, data=dat)$p.value
#2vs4,p=0.07452731
dat <- rbind(cluster2.dat.filtered, cluster4.dat.filtered)
wilcox.test(FoldChange ~ category, data=dat)$p.value
#3vs4,p=0.02074487
dat <- rbind(cluster3.dat.filtered, cluster4.dat.filtered)
wilcox.test(FoldChange ~ category, data=dat)$p.value




#2. Perform Differential expression analysis for all gene categories between MII and RS8 using findMarkers, and calcualte enrichment.
#subset MII and RS8 cells from the data matrix
MII_RS8.normData.tmp <- SC.normData[ , which(SC.normData$type=="MII" | SC.normData$type=="RS8")]
#To maintain consistency and avoid possible sorting errors, 2 RS8 cells that were clustered in C6 were removed.
MII_RS8.normData <- MII_RS8.normData.tmp[ ,which(MII_RS8.normData.tmp$cluster != "C6")]
rm(MII_RS8.normData.tmp)

CM.clustered.genes <- read.table(file="Attached_doc/CM.Gene_Category.v2.txt", sep="\t")
cluster1 <- CM.clustered.genes[which(CM.clustered.genes$category == "Early"), ]
cluster2 <- CM.clustered.genes[which(CM.clustered.genes$category == "Mid"), ]
cluster3 <- CM.clustered.genes[which(CM.clustered.genes$category == "Late"), ]
cluster4 <- CM.clustered.genes[which(CM.clustered.genes$category == "Constitutive"), ]
cluster5 <- CM.clustered.genes[which(CM.clustered.genes$category == "Escapee"), ]
cluster6 <- CM.clustered.genes[which(CM.clustered.genes$category == "Strain-bias"), ]
cluster.genes <- rownames(rbind(cluster1, cluster2, cluster3, cluster4, cluster5, cluster6))

#use findMarkers with wilcox ranked sum test
#look for marker genes using DE analysis. 

Markers <- findMarkers(MII_RS8.normData, groups=colLabels(MII_RS8.normData, onAbsence = "error"),test.type="wilcox")
colnames(Markers$`C7`)[4] <- "AUC"
Marker1 <- data.frame(Markers$`C7`[rownames(cluster1),c(2:4)], cat ="1",name=cluster.genes[1:25])
Marker2 <- data.frame(Markers$`C7`[rownames(cluster2),c(2:4)], cat ="2",name=cluster.genes[26:33])
Marker3 <- data.frame(Markers$`C7`[rownames(cluster3),c(2:4)], cat ="3",name=cluster.genes[34:50])
Marker4 <- data.frame(Markers$`C7`[rownames(cluster4),c(2:4)], cat ="4",name=cluster.genes[51:74])
Marker5 <- data.frame(Markers$`C7`[rownames(cluster5),c(2:4)], cat ="5",name=cluster.genes[75:94])
Marker6 <- data.frame(Markers$`C7`[rownames(cluster6),c(2:4)], cat ="6",name=cluster.genes[95:100])
Marker.combined <- rbind(Marker1,Marker2,Marker3,Marker4,Marker5,Marker6)
Marker.significant <- Marker.combined[which(Marker.combined$FDR < 0.05), ]
Marker1.significcant <- Marker1[which(Marker1$FDR < 0.05), ]
Marker2.significcant <- Marker2[which(Marker2$FDR < 0.05), ]
Marker3.significcant <- Marker3[which(Marker3$FDR < 0.05), ]
Marker4.significcant <- Marker4[which(Marker4$FDR < 0.05), ]
Marker5.significcant <- Marker5[which(Marker5$FDR < 0.05), ]
Marker6.significcant <- Marker6[which(Marker6$FDR < 0.05), ]

Marker.combined[which(Marker.combined$FDR < 0.05 & Marker.combined$AUC <0.5), "DE"] <-"Down"
Marker.combined[which(Marker.combined$FDR < 0.05 & Marker.combined$AUC >0.5), "DE"] <-"Up"
Marker.combined[which(Marker.combined$FDR >= 0.05), "DE"] <-"Non-DE"


ggplot(Marker.significant,aes(x=cat, y=AUC)) + 
  geom_boxplot() +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 1), 
             aes(fill=cat), size=2.5, pch=21)

mycolors <- brewer.pal(6, "Set3")
ggplot(Marker.combined,aes(x = DE, y = AUC,fill = cat)) + 
  geom_bar(position = "fill",stat = "identity", alpha=0.8) +
  scale_fill_manual(values=mycolors) +
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






############## Figure S6b Differential expression from RS4 to RS8 ##########################

#subset RS4 and RS8 cells from the data matrix
RS4_RS8.normData.tmp <- SC.normData[ , which(SC.normData$type=="RS4" | SC.normData$type=="RS8")]
#To maintain consistency and avoid possible sorting errors, 2 RS8 cells that were clustered in C6 were removed.
RS4_RS8.normData <- RS4_RS8.normData.tmp[ , which(RS4_RS8.normData.tmp$cluster == "C7" | RS4_RS8.normData.tmp$type == "RS4")]
rm(RS4_RS8.normData.tmp)

#use findMarkers with wilcox test

Markers <- findMarkers(RS4_RS8.normData, groups=colLabels(RS4_RS8.normData, onAbsence = "error"),test.type="wilcox")
colnames(Markers$`C7`)[4] <- "AUC"
Marker1 <- data.frame(Markers$`C7`[rownames(cluster1),c(2:4)], cat ="1",name=cluster.genes[1:25])
Marker2 <- data.frame(Markers$`C7`[rownames(cluster2),c(2:4)], cat ="2",name=cluster.genes[26:33])
Marker3 <- data.frame(Markers$`C7`[rownames(cluster3),c(2:4)], cat ="3",name=cluster.genes[34:50])
Marker4 <- data.frame(Markers$`C7`[rownames(cluster4),c(2:4)], cat ="4",name=cluster.genes[51:74])
Marker5 <- data.frame(Markers$`C7`[rownames(cluster5),c(2:4)], cat ="5",name=cluster.genes[75:94])
Marker6 <- data.frame(Markers$`C7`[rownames(cluster6),c(2:4)], cat ="6",name=cluster.genes[95:100])
Marker.combined <- rbind(Marker1,Marker2,Marker3,Marker4,Marker5,Marker6)
Marker.significant <- Marker.combined[which(Marker.combined$FDR < 0.05), ]
Marker1.significant <- Marker1[which(Marker1$FDR < 0.05), ]
Marker2.significant <- Marker2[which(Marker2$FDR < 0.05), ]
Marker3.significant <- Marker3[which(Marker3$FDR < 0.05), ]
Marker4.significant <- Marker4[which(Marker4$FDR < 0.05), ]
Marker5.significant <- Marker5[which(Marker5$FDR < 0.05), ]
Marker6.significant <- Marker6[which(Marker6$FDR < 0.05), ]



Marker.significant$name <- factor(Marker.significant$name, 
                                  levels=Marker.significant$name)
ggplot(Marker.significant,aes(x=name, y=AUC)) + 
  geom_bar(stat='identity',alpha=0.8, fill="dodgerblue2")+
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(angle=45,hjust=1),
        legend.text=element_blank(),
        legend.title=element_blank(),
        legend.key = element_blank(),
        #legend.text=element_text(size = rel(1)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.line.y = element_line(color="black"),
        axis.line.x = element_line(color="black"),
        legend.background = element_rect(fill=alpha('NA', 0.2))) +
  scale_y_continuous(breaks = c(0,0.25, 0.5, 0.75,1)) +
  geom_hline(yintercept=0.35,  colour="grey",linetype="longdash")











############ Figure 5g-i. p-value in comparison of chromatin accessibility in sperms for genes in different categories.
#coverage files were the output of deeptools with bin size of 100bp. 
#Raw ATAC-seq data was downloaded from GSE79230
raw <- read.table(file="Attached_doc/scale-regions.sperm_ATAC_cat1-5.clusters.100.plot.tab.v2", sep="\t", fill=T)
#if to look at the whole coverage in the whole image, use 3:72 in the row
#if to look at only the gene body, use 23:52 in the row

cat1.coverage <- data.frame(coverage=t(raw[3,c(3:72)]), cat="Cat.1")
colnames(cat1.coverage) <- c("coverage","catelog")
cat1.coverage <- transform(cat1.coverage, coverage=as.numeric(coverage))

cat2.coverage <- data.frame(coverage=t(raw[4,c(3:72)]), cat="Cat.2")
colnames(cat2.coverage) <- c("coverage","catelog")
cat2.coverage <- transform(cat2.coverage, coverage=as.numeric(coverage))

cat3.coverage <- data.frame(coverage=t(raw[5,c(3:72)]), cat="Cat.3")
colnames(cat3.coverage) <- c("coverage","catelog")
cat3.coverage <- transform(cat3.coverage, coverage=as.numeric(coverage))

cat4.coverage <- data.frame(coverage=t(raw[6,c(3:72)]), cat="Cat.4")
colnames(cat4.coverage) <- c("coverage","catelog")
cat4.coverage <- transform(cat4.coverage, coverage=as.numeric(coverage))

cat5.coverage <- data.frame(coverage=t(raw[7,c(3:72)]), cat="Cat.5")
colnames(cat5.coverage) <- c("coverage","catelog")
cat5.coverage <- transform(cat5.coverage, coverage=as.numeric(coverage))

#Calculate p-value
#cat1 vs cat2 p=0.5273635
cat1vscat2 <- rbind (cat1.coverage, cat2.coverage)
wilcox.test(coverage ~ catelog, paired = T, data=cat1vscat2)$p.value

#cat1 vs cat3 p=0.02462481
cat1vscat3 <- rbind (cat1.coverage, cat3.coverage)
wilcox.test(coverage ~ catelog, paired = T, data=cat1vscat3)$p.value

#cat1 vs cat4, p=3.044685e-07
cat1vscat4 <- rbind (cat1.coverage, cat4.coverage)
wilcox.test(coverage ~ catelog, paired = T, data=cat1vscat4)$p.value

#cat1 vs cat5, p=0.4608943
cat1vscat5 <- rbind (cat1.coverage, cat5.coverage)
wilcox.test(coverage ~ catelog, paired = T, data=cat1vscat5)$p.value

#cat2 vs cat3 p=0.01127703
cat2vscat3 <- rbind (cat2.coverage, cat3.coverage)
wilcox.test(coverage ~ catelog, paired = T, data=cat2vscat3)$p.value

#cat2 vs cat4 p=4.772889e-11
cat2vscat4 <- rbind (cat2.coverage, cat4.coverage)
wilcox.test(coverage ~ catelog, paired = T, data=cat2vscat4)$p.value

#cat2 vs cat5 p=0.09303976
cat2vscat5 <- rbind (cat2.coverage, cat5.coverage)
wilcox.test(coverage ~ catelog, paired = T, data=cat2vscat5)$p.value

#cat3 vs cat4 p=2.32721e-06
cat3vscat4 <- rbind (cat3.coverage, cat4.coverage)
wilcox.test(coverage ~ catelog, paired = T, data=cat3vscat4)$p.value

#cat3 vs cat5 p=0.2083125
cat3vscat5 <- rbind (cat3.coverage, cat5.coverage)
wilcox.test(coverage ~ catelog, paired = T, data=cat3vscat5)$p.value

#cat4 vs cat5 p=3.398997e-10
cat4vscat5 <- rbind (cat4.coverage, cat5.coverage)
wilcox.test(coverage ~ catelog, paired = T, data=cat4vscat5)$p.value





