#Figure 3

source("Gene_skewing_clusterng_analyses.R")
rm(list=setdiff(ls(), c("CM.chrX.skew.AllStages","GeneInfo")))
##Directly read previously saved cluster gene list files######################
MC.clustered.genes <- read.table(file="Attached_doc/MC.Gene_Category.txt", sep="\t")


MC.cluster1 <- MC.clustered.genes[which(MC.clustered.genes$category == "Early"), ]
MC.cluster2 <- MC.clustered.genes[which(MC.clustered.genes$category == "Mid"), ]
MC.cluster3 <- MC.clustered.genes[which(MC.clustered.genes$category == "Late"), ]
MC.cluster4 <- MC.clustered.genes[which(MC.clustered.genes$category == "Constitutive"), ]
MC.cluster5 <- MC.clustered.genes[which(MC.clustered.genes$category == "Escapee"), ]
MC.cluster6 <- MC.clustered.genes[which(MC.clustered.genes$category == "Strain-bias"), ]

CM.skewed.genes <- CM.chrX.skew.AllStages

CM.cluster1 <- CM.skewed.genes[rownames(MC.cluster1), c(1:5)]
CM.cluster2 <- CM.skewed.genes[rownames(MC.cluster2), c(1:5)]
CM.cluster3 <- CM.skewed.genes[rownames(MC.cluster3), c(1:5)]
CM.cluster4 <- CM.skewed.genes[rownames(MC.cluster4), c(1:5)]
CM.cluster5 <- CM.skewed.genes[rownames(MC.cluster5), c(1:5)]
CM.cluster6 <- CM.skewed.genes[rownames(MC.cluster6), c(1:5)]



########## Figure 3a: generate heatmap for gene clusters ###############

library(RColorBrewer)
library(pheatmap)

MC.Kmean.dat <- MC.clustered.genes[ ,c(1:5)]
CM.Kmean_related.dat <- CM.skewed.genes[rownames(MC.Kmean.dat), c(1:5)]


colnames(MC.Kmean.dat) <- c("late2C","4C","8C","16C","earlyB.")
# Sets the minimum (0), the maximum (1), and the increasing steps (+0.01) for the color scale
# Note: if some of your genes are outside of this range, they will appear white on the heatmap
breaksList = seq(0, 1, by = 0.01)
annot_cluster <- data.frame(Category=MC.clustered.genes$category, row.names=rownames(MC.clustered.genes))

cols <- colorRampPalette(brewer.pal(6, "Set2")); 
mycolors <- cols(length(unique(annot_cluster$Category)));
names(mycolors) <- unique(annot_cluster$Category)
mycolors <- list(Category = mycolors)

#combine CM and MC data
combined.dat <- cbind(MC.Kmean.dat, CM.Kmean_related.dat)

#Use red color to represent Xp fraction. Reverse the color scale (rotate the color scale so that it is upside down)
#the same was applied to all heatmaps in the paper

combined.dat <- 1- combined.dat
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
         gaps_row=c(25,33,50,76,96),
         gaps_col=c(5),
         cellheight = 6,
         fontsize_row=7.5)



#########Figure 3b. Boxplot of gene skewing in different gene categories
rm(list=ls())
##Directly read previously saved cluster gene list files######################
MC.clustered.genes <- read.table(file="Attached_doc/MC.Gene_Category.txt", sep="\t")

MC.cluster1 <- MC.clustered.genes[which(MC.clustered.genes$category == "Early"), ]
MC.cluster2 <- MC.clustered.genes[which(MC.clustered.genes$category == "Mid"), ]
MC.cluster3 <- MC.clustered.genes[which(MC.clustered.genes$category == "Late"), ]
MC.cluster4 <- MC.clustered.genes[which(MC.clustered.genes$category == "Constitutive"), ]
MC.cluster5 <- MC.clustered.genes[which(MC.clustered.genes$category == "Escapee"), ]
MC.cluster6 <- MC.clustered.genes[which(MC.clustered.genes$category == "Strain-bias"), ]


#plot 
dat.1.1 <- data.frame(skewing=MC.cluster1[ ,1], stage="late2C", cluster="1", cross="MC")
dat.1.2 <- data.frame(skewing=MC.cluster1[ ,2], stage="4C", cluster="1", cross="MC")
dat.1.3 <- data.frame(skewing=MC.cluster1[ ,3], stage="8C", cluster="1", cross="MC")
dat.1.4 <- data.frame(skewing=MC.cluster1[ ,4], stage="16C", cluster="1", cross="MC")
dat.1.5 <- data.frame(skewing=MC.cluster1[ ,5], stage="eB", cluster="1", cross="MC")

dat.2.1 <- data.frame(skewing=MC.cluster2[ ,1], stage="late2C", cluster="2", cross="MC")
dat.2.2 <- data.frame(skewing=MC.cluster2[ ,2], stage="4C", cluster="2", cross="MC")
dat.2.3 <- data.frame(skewing=MC.cluster2[ ,3], stage="8C", cluster="2", cross="MC")
dat.2.4 <- data.frame(skewing=MC.cluster2[ ,4], stage="16C", cluster="2", cross="MC")
dat.2.5 <- data.frame(skewing=MC.cluster2[ ,5], stage="eB", cluster="2", cross="MC")

dat.3.1 <- data.frame(skewing=MC.cluster3[ ,1], stage="late2C", cluster="3", cross="MC")
dat.3.2 <- data.frame(skewing=MC.cluster3[ ,2], stage="4C", cluster="3", cross="MC")
dat.3.3 <- data.frame(skewing=MC.cluster3[ ,3], stage="8C", cluster="3", cross="MC")
dat.3.4 <- data.frame(skewing=MC.cluster3[ ,4], stage="16C", cluster="3", cross="MC")
dat.3.5 <- data.frame(skewing=MC.cluster3[ ,5], stage="eB", cluster="3", cross="MC")

dat.4.1 <- data.frame(skewing=MC.cluster4[ ,1], stage="late2C", cluster="4", cross="MC")
dat.4.2 <- data.frame(skewing=MC.cluster4[ ,2], stage="4C", cluster="4", cross="MC")
dat.4.3 <- data.frame(skewing=MC.cluster4[ ,3], stage="8C", cluster="4", cross="MC")
dat.4.4 <- data.frame(skewing=MC.cluster4[ ,4], stage="16C", cluster="4", cross="MC")
dat.4.5 <- data.frame(skewing=MC.cluster4[ ,5], stage="eB", cluster="4", cross="MC")

dat.5.1 <- data.frame(skewing=MC.cluster5[ ,1], stage="late2C", cluster="5", cross="MC")
dat.5.2 <- data.frame(skewing=MC.cluster5[ ,2], stage="4C", cluster="5", cross="MC")
dat.5.3 <- data.frame(skewing=MC.cluster5[ ,3], stage="8C", cluster="5", cross="MC")
dat.5.4 <- data.frame(skewing=MC.cluster5[ ,4], stage="16C", cluster="5", cross="MC")
dat.5.5 <- data.frame(skewing=MC.cluster5[ ,5], stage="eB", cluster="5", cross="MC")

dat.6.1 <- data.frame(skewing=MC.cluster6[ ,1], stage="late2C", cluster="6", cross="MC")
dat.6.2 <- data.frame(skewing=MC.cluster6[ ,2], stage="4C", cluster="6", cross="MC")
dat.6.3 <- data.frame(skewing=MC.cluster6[ ,3], stage="8C", cluster="6", cross="MC")
dat.6.4 <- data.frame(skewing=MC.cluster6[ ,4], stage="16C", cluster="6", cross="MC")
dat.6.5 <- data.frame(skewing=MC.cluster6[ ,5], stage="eB", cluster="6", cross="MC")

MC.plot1.dat <- rbind(dat.1.1, dat.1.2, dat.1.3, dat.1.4, dat.1.5)
MC.plot2.dat <- rbind(dat.2.1, dat.2.2, dat.2.3, dat.2.4, dat.2.5)
MC.plot3.dat <- rbind(dat.3.1, dat.3.2, dat.3.3, dat.3.4, dat.3.5)
MC.plot4.dat <- rbind(dat.4.1, dat.4.2, dat.4.3, dat.4.4, dat.4.5)
MC.plot5.dat <- rbind(dat.5.1, dat.5.2, dat.5.3, dat.5.4, dat.5.5)
MC.plot6.dat <- rbind(dat.6.1, dat.6.2, dat.6.3, dat.6.4, dat.6.5)
rm(dat.1.1, dat.1.2, dat.1.3, dat.1.4, dat.1.5, 
   dat.2.1, dat.2.2, dat.2.3, dat.2.4, dat.2.5, 
   dat.3.1, dat.3.2, dat.3.3, dat.3.4, dat.3.5, 
   dat.4.1, dat.4.2, dat.4.3, dat.4.4, dat.4.5, 
   dat.5.1, dat.5.2, dat.5.3, dat.5.4, dat.5.5, 
   dat.6.1, dat.6.2, dat.6.3, dat.6.4, dat.6.5)


#Pre-define some theme pattern for the plots
theme_pattern <- theme(axis.text=element_text(size=12),
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
                       axis.line.x = element_line(color="black"),
                       plot.margin=unit(c(0.2,1,0,1),"cm"))

MC.plot1.dat$stage <- factor(MC.plot1.dat$stage, levels = c("late2C","4C","8C","16C","eB"))
P1 <-ggplot(MC.plot1.dat, aes(x=stage, y=skewing, fill=stage)) +
  geom_boxplot(alpha=.7, position=position_dodge(0.85),outlier.shape=NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 1), 
             aes(fill=stage), size=1.5, pch=21) +
  scale_fill_manual(values=c("#FFFFCC", "#99FFFF", "#3399FF", "#0033CC", "#003366")) +
  theme_bw() +
  theme_pattern +
  geom_hline(yintercept=0.15,  colour="grey",linetype="longdash") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.25,0.50,0.75,1))

MC.plot2.dat$stage <- factor(MC.plot2.dat$stage, levels = c("late2C","4C","8C","16C","eB"))
P2 <-ggplot(MC.plot2.dat, aes(x=stage, y=skewing, fill=stage)) +
  geom_boxplot(alpha=.7, position=position_dodge(0.85),outlier.shape=NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 1), 
             aes(fill=stage), size=1.5, pch=21) +
  scale_fill_manual(values=c("#FFFFCC", "#99FFFF", "#3399FF", "#0033CC", "#003366")) +
  theme_bw() +
  theme_pattern +
  geom_hline(yintercept=0.15,  colour="grey",linetype="longdash") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.25,0.50,0.75,1))

MC.plot3.dat$stage <- factor(MC.plot3.dat$stage, levels = c("late2C","4C","8C","16C","eB"))
P3 <-ggplot(MC.plot3.dat, aes(x=stage, y=skewing, fill=stage)) +
  geom_boxplot(alpha=.7, position=position_dodge(0.85),outlier.shape=NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 1), 
             aes(fill=stage), size=1.5, pch=21) +
  scale_fill_manual(values=c("#FFFFCC", "#99FFFF", "#3399FF", "#0033CC", "#003366")) +
  theme_bw() +
  theme_pattern +
  geom_hline(yintercept=0.15,  colour="grey",linetype="longdash") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.25,0.50,0.75,1))

MC.plot4.dat$stage <- factor(MC.plot4.dat$stage, levels = c("late2C","4C","8C","16C","eB"))
P4 <-ggplot(MC.plot4.dat, aes(x=stage, y=skewing, fill=stage)) +
  geom_boxplot(alpha=.7, position=position_dodge(0.85),outlier.shape=NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 1), 
             aes(fill=stage), size=1.5, pch=21) +
  scale_fill_manual(values=c("#FFFFCC", "#99FFFF", "#3399FF", "#0033CC", "#003366")) +
  theme_bw() +
  theme_pattern +
  geom_hline(yintercept=0.15,  colour="grey",linetype="longdash") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.25,0.50,0.75,1))

MC.plot5.dat$stage <- factor(MC.plot5.dat$stage, levels = c("late2C","4C","8C","16C","eB"))
P5 <-ggplot(MC.plot5.dat, aes(x=stage, y=skewing, fill=stage)) +
  geom_boxplot(alpha=.7, position=position_dodge(0.85),outlier.shape=NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 1), 
             aes(fill=stage), size=1.5, pch=21) +
  scale_fill_manual(values=c("#FFFFCC", "#99FFFF", "#3399FF", "#0033CC", "#003366")) +
  theme_bw() +
  theme_pattern +
  geom_hline(yintercept=0.15,  colour="grey",linetype="longdash") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.25,0.50,0.75,1))

MC.plot6.dat$stage <- factor(MC.plot6.dat$stage, levels = c("late2C","4C","8C","16C","eB"))
P6 <-ggplot(MC.plot6.dat, aes(x=stage, y=skewing, fill=stage)) +
  geom_boxplot(alpha=.7, position=position_dodge(0.85),outlier.shape=NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 1), 
             aes(fill=stage), size=1.5, pch=21) +
  scale_fill_manual(values=c("#FFFFCC", "#99FFFF", "#3399FF", "#0033CC", "#003366")) +
  theme_bw() +
  theme_pattern +
  geom_hline(yintercept=0.15,  colour="grey",linetype="longdash") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.25,0.50,0.75,1))

#combine all plots vertically
library(gridExtra)
grid.arrange(P1,P2,P3,P4,P5,P6, nrow=6, ncol=1)
rm(P1, P2, P3, P4, P5, P6, theme_pattern)


#########Figure 3c. skew at l2C in different clusters############################################

l2C.skew.cluster1 <- data.frame(skew=MC.cluster1[ ,"l2C"], Category= "1")
l2C.skew.cluster2 <- data.frame(skew=MC.cluster2[ ,"l2C"], Category= "2")
l2C.skew.cluster3 <- data.frame(skew=MC.cluster3[ ,"l2C"], Category= "3")
l2C.skew.cluster4 <- data.frame(skew=MC.cluster4[ ,"l2C"], Category= "4")
l2C.skew.cluster5 <- data.frame(skew=MC.cluster5[ ,"l2C"], Category= "5")
l2C.skew.cluster6 <- data.frame(skew=MC.cluster6[ ,"l2C"], Category= "6")
l2C.skew.dat <- rbind(l2C.skew.cluster1, l2C.skew.cluster2, l2C.skew.cluster3, l2C.skew.cluster4, l2C.skew.cluster5, l2C.skew.cluster6)


ggplot(l2C.skew.dat, aes(x=Category, y=skew, fill="darkred")) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.7), 
             size=1.5, pch=21) +
  geom_boxplot(alpha=.5, position=position_dodge(0.85)) +
  scale_fill_manual(values="black") +
  theme_bw() +
  labs(x="Cluster", y="Paternal Ratio") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.text=element_text(size=12),
        legend.title=element_blank(), 
        legend.position = "none",
        axis.text.x = element_text(hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_line(color="black"),
        axis.line.x = element_line(color="black")) +
  geom_hline(yintercept=0.15,  colour="grey",linetype="longdash")


########## Figure.3d. heatmap for 6 categories after averaging values in each category###############
#average each category
MC.category.average.dat <- rbind(colMeans(MC.cluster1[ ,c(-1,-6)],na.rm = T), colMeans(MC.cluster2[ ,c(-1,-6)],na.rm = T), 
                                 colMeans(MC.cluster3[ ,c(-1,-6)],na.rm = T), colMeans(MC.cluster4[ ,c(-1,-6)],na.rm = T),
                                 colMeans(MC.cluster5[ ,c(-1,-6)],na.rm = T), colMeans(MC.cluster6[ ,c(-1,-6)],na.rm = T))
row.names(MC.category.average.dat) <- c("Cat.1","Cat.2","Cat.3","Cat.4","Cat.5","Cat.6")
breaksList = seq(0, 1, by = 0.01)
MC.category.average.dat <- 1-MC.category.average.dat
pheatmap(MC.category.average.dat, 
         color = colorRampPalette(rev(brewer.pal(n = 6, name = "RdYlBu")))(length(breaksList)),
         # Defines the vector of colors for the legend (equal to lenght of breaksList)
         breaks = breaksList,
         cluster_cols = F,
         cluster_rows = T, 
         show_rownames = T, 
         show_colnames = T, 
         # Sets the breaks of the color scale as in breaksList
         border_color="grey")


######### Figure 3f. calculate paternal fraction using labeled zygotic RNA reads ###############
#Calculate fraction of paternal mutation reads in total allelic mutation reads.
#in categorized genes.
#Ratios coming from samples with total mutant allelic reads less than 6 would be ignored, marked as NA.

Raw.ratio <- read.table(file="Attached_doc/Gene_category_casMut_ratio.txt",sep="\t",header=T, row.names=1)

#Only consider genes with more than 2 useful replicates. (>= 2 non-NA)
Used.ratio <- Raw.ratio[which(Raw.ratio$SampleSize >=2), ]

Cat.4.gene <- Used.ratio[which(Used.ratio$Category=="Constitutive"), c("Ave_ratio","Category")]
Cat.1.gene <- Used.ratio[which(Used.ratio$Category=="Early"), c("Ave_ratio","Category")]
Cat.2_3.gene <- Used.ratio[which(Used.ratio$Category=="Mid+Late"), c("Ave_ratio","Category")]

dat <- rbind(Cat.4.gene, Cat.1.gene, Cat.2_3.gene)

ggplot(dat, aes(x=Category, y=Ave_ratio))+
  geom_boxplot(outlier.shape=NA, alpha=0.7) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 1), 
             aes(fill=Category), size=1.2, pch=21) +
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



########## Figure 3g. linear distance of gene clusters to Xist#################################

#--Get geneInfo-----------------
GeneInfo.plus <- read.table(file="Attached_doc/GeneInfo.plus.txt", sep="\t", header=F, row.names=1)
colnames(GeneInfo.plus)=c("Chr","locus", "Strand","Length")
GeneInfo.minus <- read.table(file="Attached_doc/GeneInfo.minus.txt", sep="\t", header=F, row.names=1)
colnames(GeneInfo.minus)=c("Chr","locus", "Strand","Length")
GeneInfo <- rbind(GeneInfo.plus,GeneInfo.minus)

cluster.1.coor <- data.frame(locus=GeneInfo[rownames(MC.cluster1), 2]/1000000, Cluster ="1", row.names= rownames(MC.cluster1))
cluster.2.coor <- data.frame(locus=GeneInfo[rownames(MC.cluster2), 2]/1000000, Cluster ="2", row.names= rownames(MC.cluster2))
cluster.3.coor <- data.frame(locus=GeneInfo[rownames(MC.cluster3), 2]/1000000, Cluster ="3", row.names= rownames(MC.cluster3))
cluster.4.coor <- data.frame(locus=GeneInfo[rownames(MC.cluster4), 2]/1000000, Cluster ="4", row.names= rownames(MC.cluster4))
cluster.5.coor <- data.frame(locus=GeneInfo[rownames(MC.cluster5), 2]/1000000, Cluster ="5", row.names= rownames(MC.cluster5))
cluster.6.coor <- data.frame(locus=GeneInfo[rownames(MC.cluster6), 2]/1000000, Cluster ="6", row.names= rownames(MC.cluster6))
coor.dat <- rbind(cluster.1.coor, cluster.2.coor, cluster.3.coor, cluster.4.coor, cluster.5.coor, cluster.6.coor)
coor.dat[ ,3] <- abs(coor.dat$locus - 101) #Xist is located at 101MB in cas

ggplot(coor.dat, aes(x=Cluster, y=V3, fill="blue")) + 
  geom_boxplot(alpha=0.5) + 
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 1), 
             size=1.5, pch=21) +
  scale_fill_manual(values="dark red")+
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.text=element_text(size=12),
        legend.title=element_blank(), 
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position="none",
        axis.line.y = element_line(color="black"),
        axis.line.x = element_line(color="black"))





############### Figure 3h and S4e. Xist interacting frequency with genes in different categories.######################
#--Generate bed files from wig file
interaction.wig <- read.table(file="Attached_doc/HiC_Xist_locus_res500K.wig", sep="\t", header=T) #generated from data in GSE82185 using Juicebox
interaction.wig[ ,2] <- 500000*as.numeric(rownames(interaction.wig)) 
interaction.wig[ ,3] <- 500000*(as.numeric(rownames(interaction.wig)) -1) +1
interaction.wig[ ,4] <- "chrX"
colnames(interaction.wig) <- c("value", "end", "start")
interaction.bed <- interaction.wig[ ,c(4,3,2,1)]
#write.table(interaction.bed, file="Xist_interaction_500K.bed", sep="\t", quote=F, row.names=F, col.names=F)

#Use 'bedtools intersect' to find the Xist interacting regions that overlap with genes in different categories. Files can be found in "Attached_doc" directory.
cluster1.xist.overlap <- data.frame(frequency=read.table(file="Attached_doc/MC.Cat1.interaction_frequency.bed")[ ,4], Cluster = "1")
cluster2.xist.overlap <- data.frame(frequency=read.table(file="Attached_doc/MC.Cat2.interaction_frequency.bed", sep="\t", header=F)[ ,4], Cluster = "2")
cluster3.xist.overlap <- data.frame(frequency=read.table(file="Attached_doc/MC.Cat3.interaction_frequency.bed", sep="\t", header=F)[ ,4], Cluster = "3")
cluster4.xist.overlap <- data.frame(frequency=read.table(file="Attached_doc/MC.Cat4.interaction_frequency.bed", sep="\t", header=F)[ ,4], Cluster = "4")
cluster5.xist.overlap <- data.frame(frequency=read.table(file="Attached_doc/MC.Cat5.interaction_frequency.bed", sep="\t", header=F)[ ,4], Cluster = "5")
cluster6.xist.overlap <- data.frame(frequency=read.table(file="Attached_doc/MC.Cat6.interaction_frequency.bed", sep="\t", header=F)[ ,4], Cluster = "6")
cluster.xist.overlap <- rbind(cluster1.xist.overlap,cluster2.xist.overlap,cluster3.xist.overlap,cluster4.xist.overlap,cluster5.xist.overlap,cluster6.xist.overlap)
rm(cluster1.xist.overlap,cluster2.xist.overlap,cluster3.xist.overlap,cluster4.xist.overlap,cluster5.xist.overlap,cluster6.xist.overlap)
#boxplot
ggplot(cluster.xist.overlap, aes(x=Cluster, y=frequency, fill="#666666")) + 
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.7), 
             size=1.5, pch=21) +
  geom_boxplot(alpha=0.5, outlier.shape=NA) + 
  scale_fill_manual(values="#666666") +
  theme_bw() +
  labs(y="Interacting frequency") +
  coord_cartesian(ylim = c(0, 320))  +
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



#Related Figure -  Fig.S4e
#generate bed files for genes in each cluster
#acquire the start and end locus of each gene

# 1) Input the interact frequency wig files on X at 8cell-stage.
interaction.wig <- read.table(file="Attached_doc/HiC_obs_vert_bin400_res250K.wig", sep="\t", head=T) #generated from juicebox using data downloaded from GSE82185
interaction.wig[ ,2] <- 0.25*as.numeric(rownames(interaction.wig)) 
colnames(interaction.wig) <- c("value", "locus")
#generate interaction frequency
interaction.frequency <- ggplot(interaction.wig, aes(x=locus, y=value)) + 
  geom_bar(stat="identity", fill="#3399FF", width=0.5) + 
  theme_bw() +
  coord_cartesian(ylim = c(0, 35))  +
  scale_x_continuous(limits = c(1,175), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140,160)) + 
  theme(axis.text.x=element_blank(),
        axis.text.y=element_text(size=12),
        axis.title=element_blank(),
        legend.text=element_blank(),
        legend.title=element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position="none",
        plot.margin=unit(c(0,1,0,1),"cm"),
        axis.line.y = element_line(color="black"),
        axis.line.x = element_line(color="grey"))

#readin the cluster gene list with their mm9 coordinates

Cat.1.mm9.coor <- data.frame(locus=read.table(file="Attached_doc/genes_mm9.bed", sep="\t", row.names=4)[rownames(MC.cluster1), 2]/1000000, value="1", Category=-1)
Cat.2.mm9.coor <- data.frame(locus=read.table(file="Attached_doc/genes_mm9.bed", sep="\t", row.names=4)[rownames(MC.cluster2), 2]/1000000, value="1", Category=-2)
Cat.3.mm9.coor <- data.frame(locus=read.table(file="Attached_doc/genes_mm9.bed", sep="\t", row.names=4)[rownames(MC.cluster3), 2]/1000000, value="1", Category=-3)
Cat.4.mm9.coor <- data.frame(locus=read.table(file="Attached_doc/genes_mm9.bed", sep="\t", row.names=4)[rownames(MC.cluster4), 2]/1000000, value="1", Category=-4)
Cat.5.mm9.coor <- data.frame(locus=read.table(file="Attached_doc/genes_mm9.bed", sep="\t", row.names=4)[rownames(MC.cluster5), 2]/1000000, value="1", Category=-5)
Cat.6.mm9.coor <- data.frame(locus=read.table(file="Attached_doc/genes_mm9.bed", sep="\t", row.names=4)[rownames(MC.cluster6), 2]/1000000, value="1", Category=-6)
Total.mm9.coor <- rbind(Cat.1.mm9.coor, Cat.2.mm9.coor, Cat.3.mm9.coor, Cat.4.mm9.coor, Cat.5.mm9.coor, Cat.6.mm9.coor)


Total.mm9.plot <- ggplot(Total.mm9.coor, aes(y=Category, x=locus)) + 
  geom_point(color="black", shape =15, alpha=.5, size=1.6) + 
  theme_bw() +
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title=element_text(size=12),
        legend.text=element_blank(),
        legend.title=element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position="none",
        plot.margin=unit(c(0,1,0,1),"cm"),
        axis.line.y = element_line(color="black"),
        axis.line.x = element_line(color="black")) + 
  scale_x_continuous(limits = c(1,175), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140,160)) + 
  scale_y_continuous(breaks = c(-6,-5,-4,-3,-2,-1)) + 
  geom_vline(xintercept=100.48,  colour="red",linetype="longdash")

library(gridExtra)
library(cowplot)
plot_grid(interaction.frequency,Total.mm9.plot, align = "v", nrow = 2, rel_heights = c(2/5, 3/5))
rm(interaction.frequency,Total.mm9.plot)




############# Figure 3i. Strata of genes in different categories ######################################################

cluster1_human <- data.frame(read.table("Attached_doc/cluster1_human.bed")[ ,c(4,2)], category="1", Yaxis=1)
cluster2_human <- data.frame(read.table("Attached_doc/cluster2_human.bed")[ ,c(4,2)], category="2+3", Yaxis=2)
cluster3_human <- data.frame(read.table("Attached_doc/cluster3_human.bed")[ ,c(4,2)], category="2+3", Yaxis=3)
cluster4_human <- data.frame(read.table("Attached_doc/cluster4_human.bed")[ ,c(4,2)], category="4", Yaxis=4)
cluster5_human <- data.frame(read.table("Attached_doc/cluster5_human.bed")[ ,c(4,2)], category="5+6", Yaxis=5)
cluster6_human <- data.frame(read.table("Attached_doc/cluster6_human.bed")[ ,c(4,2)], category="5+6", Yaxis=6)
cluster <- rbind(cluster1_human,cluster2_human,cluster3_human,cluster4_human,cluster5_human,cluster6_human)
cluster$V2 <- cluster$V2 / 1000000
colnames(cluster) <-c("gene","loci","category","Yaxis")
cluster <- cluster[order(cluster$loci), ]

Strata1_End_Rps4x <- 72272042 /1000000
Strata2_End_Ube1x <- 47190861 /1000000

Strata3_Begin_Kdm6a <- 45112602 /1000000
Strata3_End_TB4x <- 12975110 /1000000

Strata4_Begin_Amelx <- 11300761 /1000000


cluster[which(cluster$loci >= Strata1_End_Rps4x), 5] <- "1"
cluster[which(cluster$loci < Strata1_End_Rps4x & cluster$loci >= Strata2_End_Ube1x), 5] <- "2"
cluster[which(cluster$loci < Strata2_End_Ube1x & cluster$loci >= Strata3_End_TB4x), 5] <- "3"
cluster[which(cluster$loci <= Strata3_End_TB4x), 5] <- "4"


colnames(cluster) <-c("gene","loci","category","Yaxis","Strata")
cluster <- cluster[complete.cases(cluster), ]

#Plot fraction of catergory in each Strata
#each Strata
Strata1 <- cluster[which(cluster$Strata == "1"), c(3,5)]
Strata2 <- cluster[which(cluster$Strata == "2"), c(3,5)]
Strata3 <- cluster[which(cluster$Strata == "3"), c(3,5)]
Strata4 <- cluster[which(cluster$Strata == "4"), c(3,5)]

#plot
Strata1.fraction <- data.frame(category=c("1","2+3","4","5+6"), fraction=table(Strata1$category) / sum(table(Strata1$category)), Strata=1)
Strata2.fraction <- data.frame(category=c("1","2+3","4","5+6"), fraction=table(Strata2$category) / sum(table(Strata2$category)), Strata=2)
Strata3.fraction <- data.frame(category=c("1","2+3","4","5+6"), fraction=table(Strata3$category) / sum(table(Strata3$category)), Strata=3)
Strata4.fraction <- data.frame(category=c("1","2+3","4","5+6"), fraction=table(Strata4$category) / sum(table(Strata4$category)), Strata=4)

library(RColorBrewer)
mycolors <- brewer.pal(6, "Set2")
dat <- rbind(Strata1.fraction,Strata2.fraction,Strata3.fraction)
ggplot(dat,aes(x = Strata, y = fraction.Freq,fill = category)) + 
  geom_bar(position = "fill",stat = "identity", alpha=0.8) +
  scale_fill_manual(values=mycolors) +
  scale_colour_manual(values=mycolors) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
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


#Plot fraction of Strata in each category
#each category
cluster1 <- cluster[which(cluster$category == "1"), c(3,5)]
cluster2_3 <- cluster[which(cluster$category == "2+3"), c(3,5)]
cluster4 <- cluster[which(cluster$category == "4"), c(3,5)]
cluster5_6 <- cluster[which(cluster$category == "5+6"), c(3,5)]



#plot
cluster1.fraction <- data.frame(Strata=c("1","2","3"), fraction=table(cluster1$Strata) / sum(table(cluster1$Strata)), cluster="1")
cluster2_3.fraction <- data.frame(Strata=c("1","2","3"), fraction=table(cluster2_3$Strata) / sum(table(cluster2_3$Strata)), cluster="2_3")
cluster4.fraction <- data.frame(Strata=c("1","2","3"), fraction=table(cluster4$Strata) / sum(table(cluster4$Strata)), cluster="4")
cluster5_6.fraction <- data.frame(Strata=c("1","2","3","4"), fraction=table(cluster5_6$Strata) / sum(table(cluster5_6$Strata)), cluster="5+6")

dat <- rbind(cluster1.fraction, cluster2_3.fraction, cluster4.fraction, cluster5_6.fraction)
mycolors <- brewer.pal(4, "Accent")
ggplot(dat,aes(x = cluster, y = fraction.Freq,fill = Strata)) + 
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


#fisher's exact test 
#for cluster4 gene, use Rps4x as boundary, which is more strigent
#                     cat.4 gene        non-cat.4 gene
# Strata1 gene            16                28
# Non-Strata1 gene        8                 46
fisher.test(matrix(c(16,28,8,46),nrow=2,ncol=2),alternative="greater")
#p-value = 0.01278



#Related Figure - Fig S7d 
#violin plot, showing the distribution difference between cluster1 and cluster2+3.
#show fraction of Strata in each category
#each category
cluster1 <- cluster[which(cluster$category == "1"), c(1,2,3)]
cluster2_3 <- cluster[which(cluster$category == "2+3"), c(1,2,3)]
dat <- rbind(cluster1, cluster2_3)


ggplot(dat, aes(x=category, y=loci, fill=category)) + 
  geom_violin(alpha=0.7) +
    scale_fill_manual(values=c("#CC3333", "#0066CC", "#339900")) +
  geom_hline(yintercept=72.272,  colour="black",linetype="longdash") +
  geom_hline(yintercept=47.191,  colour="black",linetype="longdash") +
  scale_y_continuous(limits = c(1,175), expand = c(0,0), breaks = c(0,20,40,60,80,100,120,140,160)) +
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




############ Figure 3j. Test relative Xm upregulation during XCI in different categories #################
source("Read_data.R")
rm(list=setdiff(ls(), c("MC.F.mus.All","MC.F.cas.All","MC.F.comp.RPKM","MC.M.comp.RPKM",
                        "KO.F.mus.All","KO.F.cas.All","KO.F.comp.RPKM","KO.M.comp.RPKM")))
MC.clustered.genes <- read.table(file="Attached_doc/MC.Gene_Category.txt", sep="\t")
MC.cluster1 <- MC.clustered.genes[which(MC.clustered.genes$category == "Early"), ]
MC.cluster2 <- MC.clustered.genes[which(MC.clustered.genes$category == "Mid"), ]
MC.cluster3 <- MC.clustered.genes[which(MC.clustered.genes$category == "Late"), ]
MC.cluster4 <- MC.clustered.genes[which(MC.clustered.genes$category == "Constitutive"), ]
MC.cluster5 <- MC.clustered.genes[which(MC.clustered.genes$category == "Escapee"), ]
MC.cluster6 <- MC.clustered.genes[which(MC.clustered.genes$category == "Strain-bias"), ]


#calculate maternal portion of RPKM, by multiplying comp RPKM with maternal (mus) ratio.

F.Allelic.All <- MC.F.mus.All + MC.F.cas.All
F.MatRatio.All <- MC.F.mus.All / F.Allelic.All
F.Mat.RPKM <- MC.F.comp.RPKM * F.MatRatio.All


#overall RPKM expression of genes in each cluster
F.cluster1.MatRPKM <- F.Mat.RPKM[rownames(MC.cluster1), ]
F.cluster2.MatRPKM <- F.Mat.RPKM[rownames(MC.cluster2), ]
F.cluster3.MatRPKM <- F.Mat.RPKM[rownames(MC.cluster3), ]
F.cluster4.MatRPKM <- F.Mat.RPKM[rownames(MC.cluster4), ]
F.cluster5.MatRPKM <- F.Mat.RPKM[rownames(MC.cluster5), ]
F.cluster6.MatRPKM <- F.Mat.RPKM[rownames(MC.cluster6), ]

M.cluster1.RPKM <- MC.M.comp.RPKM[rownames(MC.cluster1), ]
M.cluster2.RPKM <- MC.M.comp.RPKM[rownames(MC.cluster2), ]
M.cluster3.RPKM <- MC.M.comp.RPKM[rownames(MC.cluster3), ]
M.cluster4.RPKM <- MC.M.comp.RPKM[rownames(MC.cluster4), ]
M.cluster5.RPKM <- MC.M.comp.RPKM[rownames(MC.cluster5), ]
M.cluster6.RPKM <- MC.M.comp.RPKM[rownames(MC.cluster6), ]



F_M.ratio.late2cell.cluster1 <- data.frame(ratio=rowMeans(F.cluster1.MatRPKM[ ,8:12], na.rm=T)/rowMeans(M.cluster1.RPKM[ ,8:12]), Stage="late2cell", catelog="cluster1")
F_M.ratio.4cell.cluster1 <- data.frame(ratio=rowMeans(F.cluster1.MatRPKM[ ,13:18], na.rm=T)/rowMeans(M.cluster1.RPKM[ ,13:23]), Stage="4cell", catelog="cluster1")
F_M.ratio.8cell.cluster1 <- data.frame(ratio=rowMeans(F.cluster1.MatRPKM[ ,19:24], na.rm=T)/rowMeans(M.cluster1.RPKM[ ,24:32]), Stage="8cell", catelog="cluster1")
F_M.ratio.16cell.cluster1 <- data.frame(ratio=rowMeans(F.cluster1.MatRPKM[ ,25:29], na.rm=T)/rowMeans(M.cluster1.RPKM[ ,33:35]), Stage="16cell", catelog="cluster1")
F_M.ratio.eB.cluster1 <- data.frame(ratio=rowMeans(F.cluster1.MatRPKM[ ,30:34], na.rm=T)/rowMeans(M.cluster1.RPKM[ ,36:41]), Stage="earlyBlastocyst", catelog="cluster1")

F_M.ratio.late2cell.cluster2 <- data.frame(ratio=rowMeans(F.cluster2.MatRPKM[ ,8:12], na.rm=T)/rowMeans(M.cluster2.RPKM[ ,8:12]), Stage="late2cell", catelog="cluster2")
F_M.ratio.4cell.cluster2 <- data.frame(ratio=rowMeans(F.cluster2.MatRPKM[ ,13:18], na.rm=T)/rowMeans(M.cluster2.RPKM[ ,13:23]), Stage="4cell", catelog="cluster2")
F_M.ratio.8cell.cluster2 <- data.frame(ratio=rowMeans(F.cluster2.MatRPKM[ ,19:24], na.rm=T)/rowMeans(M.cluster2.RPKM[ ,24:32]), Stage="8cell", catelog="cluster2")
F_M.ratio.16cell.cluster2 <- data.frame(ratio=rowMeans(F.cluster2.MatRPKM[ ,25:29], na.rm=T)/rowMeans(M.cluster2.RPKM[ ,33:35]), Stage="16cell", catelog="cluster2")
F_M.ratio.eB.cluster2 <- data.frame(ratio=rowMeans(F.cluster2.MatRPKM[ ,30:34], na.rm=T)/rowMeans(M.cluster2.RPKM[ ,36:41]), Stage="earlyBlastocyst", catelog="cluster2")

F_M.ratio.late2cell.cluster3 <- data.frame(ratio=rowMeans(F.cluster3.MatRPKM[ ,8:12], na.rm=T)/rowMeans(M.cluster3.RPKM[ ,8:12]), Stage="late2cell", catelog="cluster3")
F_M.ratio.4cell.cluster3 <- data.frame(ratio=rowMeans(F.cluster3.MatRPKM[ ,13:18], na.rm=T)/rowMeans(M.cluster3.RPKM[ ,13:23]), Stage="4cell", catelog="cluster3")
F_M.ratio.8cell.cluster3 <- data.frame(ratio=rowMeans(F.cluster3.MatRPKM[ ,19:24], na.rm=T)/rowMeans(M.cluster3.RPKM[ ,24:32]), Stage="8cell", catelog="cluster3")
F_M.ratio.16cell.cluster3 <- data.frame(ratio=rowMeans(F.cluster3.MatRPKM[ ,25:29], na.rm=T)/rowMeans(M.cluster3.RPKM[ ,33:35]), Stage="16cell", catelog="cluster3")
F_M.ratio.eB.cluster3 <- data.frame(ratio=rowMeans(F.cluster3.MatRPKM[ ,30:34], na.rm=T)/rowMeans(M.cluster3.RPKM[ ,36:41]), Stage="earlyBlastocyst", catelog="cluster3")

F_M.ratio.late2cell.cluster4 <- data.frame(ratio=rowMeans(F.cluster4.MatRPKM[ ,8:12], na.rm=T)/rowMeans(M.cluster4.RPKM[ ,8:12]), Stage="late2cell", catelog="cluster4")
F_M.ratio.4cell.cluster4 <- data.frame(ratio=rowMeans(F.cluster4.MatRPKM[ ,13:18], na.rm=T)/rowMeans(M.cluster4.RPKM[ ,13:23]), Stage="4cell", catelog="cluster4")
F_M.ratio.8cell.cluster4 <- data.frame(ratio=rowMeans(F.cluster4.MatRPKM[ ,19:24], na.rm=T)/rowMeans(M.cluster4.RPKM[ ,24:32]), Stage="8cell", catelog="cluster4")
F_M.ratio.16cell.cluster4 <- data.frame(ratio=rowMeans(F.cluster4.MatRPKM[ ,25:29], na.rm=T)/rowMeans(M.cluster4.RPKM[ ,33:35]), Stage="16cell", catelog="cluster4")
F_M.ratio.eB.cluster4 <- data.frame(ratio=rowMeans(F.cluster4.MatRPKM[ ,30:34], na.rm=T)/rowMeans(M.cluster4.RPKM[ ,36:41]), Stage="earlyBlastocyst", catelog="cluster4")

F_M.ratio.late2cell.cluster5 <- data.frame(ratio=rowMeans(F.cluster5.MatRPKM[ ,8:12], na.rm=T)/rowMeans(M.cluster5.RPKM[ ,8:12]), Stage="late2cell", catelog="cluster5")
F_M.ratio.4cell.cluster5 <- data.frame(ratio=rowMeans(F.cluster5.MatRPKM[ ,13:18], na.rm=T)/rowMeans(M.cluster5.RPKM[ ,13:23]), Stage="4cell", catelog="cluster5")
F_M.ratio.8cell.cluster5 <- data.frame(ratio=rowMeans(F.cluster5.MatRPKM[ ,19:24], na.rm=T)/rowMeans(M.cluster5.RPKM[ ,24:32]), Stage="8cell", catelog="cluster5")
F_M.ratio.16cell.cluster5 <- data.frame(ratio=rowMeans(F.cluster5.MatRPKM[ ,25:29], na.rm=T)/rowMeans(M.cluster5.RPKM[ ,33:35]), Stage="16cell", catelog="cluster5")
F_M.ratio.eB.cluster5 <- data.frame(ratio=rowMeans(F.cluster5.MatRPKM[ ,30:34], na.rm=T)/rowMeans(M.cluster5.RPKM[ ,36:41]), Stage="earlyBlastocyst", catelog="cluster5")

All.dat <- rbind(F_M.ratio.late2cell.cluster5,
                 F_M.ratio.4cell.cluster5,
                 F_M.ratio.8cell.cluster5,
                 F_M.ratio.16cell.cluster5,
                 F_M.ratio.eB.cluster5)


All.dat <- rbind(F_M.ratio.late2cell.cluster1,
                 F_M.ratio.4cell.cluster1,
                 F_M.ratio.8cell.cluster1,
                 F_M.ratio.16cell.cluster1,
                 F_M.ratio.eB.cluster1,
                 F_M.ratio.late2cell.cluster2,
                 F_M.ratio.4cell.cluster2,
                 F_M.ratio.8cell.cluster2,
                 F_M.ratio.16cell.cluster2,
                 F_M.ratio.eB.cluster2,
                 F_M.ratio.late2cell.cluster3,
                 F_M.ratio.4cell.cluster3,
                 F_M.ratio.8cell.cluster3,
                 F_M.ratio.16cell.cluster3,
                 F_M.ratio.eB.cluster3,
                 F_M.ratio.late2cell.cluster4,
                 F_M.ratio.4cell.cluster4,
                 F_M.ratio.8cell.cluster4,
                 F_M.ratio.16cell.cluster4,
                 F_M.ratio.eB.cluster4,
                 F_M.ratio.late2cell.cluster5,
                 F_M.ratio.4cell.cluster5,
                 F_M.ratio.8cell.cluster5,
                 F_M.ratio.16cell.cluster5,
                 F_M.ratio.eB.cluster5)
All.dat$Stage <- factor(All.dat$Stage, levels=c("late2cell","4cell","8cell","16cell","earlyBlastocyst"))

ggplot(All.dat, aes(x=catelog, y=ratio, fill=Stage)) +
  geom_boxplot(alpha=0.7,outlier.shape=NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.7), 
             size=1.2, pch=21) +
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

#calculate p-value, 4c and 8c in cluster1, p=0.00672555
dat <- rbind(F_M.ratio.4cell.cluster1, F_M.ratio.8cell.cluster1)
wilcox.test(ratio ~ Stage, data=dat, paired=T)$p.value

#calculate p-value, 8c and 16c in cluster1, p=0.8119304
dat <- rbind(F_M.ratio.16cell.cluster1, F_M.ratio.8cell.cluster1)
wilcox.test(ratio ~ Stage, data=dat, paired=T)$p.value

#calculate p-value, 8c and 16c in cluster2, p=0.015625
dat <- rbind(F_M.ratio.16cell.cluster2, F_M.ratio.8cell.cluster2)
wilcox.test(ratio ~ Stage, data=dat, paired=T)$p.value

#calculate p-value, 8c and 16c in cluster3, p=0.2633209
dat <- rbind(F_M.ratio.16cell.cluster3, F_M.ratio.8cell.cluster3)
wilcox.test(ratio ~ Stage, data=dat, paired=T)$p.value

#calculate p-value, 8c and eB in cluster3, p=0.003158569
dat <- rbind(F_M.ratio.eB.cluster3, F_M.ratio.8cell.cluster3)
wilcox.test(ratio ~ Stage, data=dat, paired=T)$p.value


#calculate p-value, eB and 16c in cluster3, p=0.089
dat <- rbind(F_M.ratio.eB.cluster3, F_M.ratio.16cell.cluster3)
wilcox.test(ratio ~ Stage, data=dat, paired=T)$p.value

#calculate p-value, 16c and 8c in cluster4, p=0.217
dat <- rbind(F_M.ratio.16cell.cluster4, F_M.ratio.8cell.cluster4)
wilcox.test(ratio ~ Stage, data=dat, paired=T)$p.value

#calculate p-value, 4c and 8c in cluster4, p=0.173
dat <- rbind(F_M.ratio.4cell.cluster4, F_M.ratio.8cell.cluster4)
wilcox.test(ratio ~ Stage, data=dat, paired=T)$p.value

#calculate p-value, 16c and 8c in cluster5, p=0.985
dat <- rbind(F_M.ratio.16cell.cluster5, F_M.ratio.8cell.cluster5)
wilcox.test(ratio ~ Stage, data=dat, paired=T)$p.value

#calculate p-value, 4c and 8c in cluster5, p=0.0231
dat <- rbind(F_M.ratio.4cell.cluster5, F_M.ratio.8cell.cluster5)
wilcox.test(ratio ~ Stage, data=dat, paired=T)$p.value

