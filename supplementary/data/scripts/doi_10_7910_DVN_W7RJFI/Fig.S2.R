########################################################
#---Fig. S2A,B---PCA and Pairwise Pearson Correlation
########################################################
####-Fig.S2A
source("Read_data.R")
rm(list=setdiff(ls(), c("CM.comp.All", "MC.comp.All","CM.comp.RPKM","MC.comp.RPKM")))
#rename samples according to stages
MC.colnames <- c(paste("zygote", c(1:4), sep="-"),
                        paste("early2C", c(1:6), sep="-"),
                        paste("late2C", c(1:10), sep="-"),
                        paste("4C", c(1:17), sep="-"),
                        paste("8C", c(1:15), sep="-"),
                        paste("16C", c(1:8), sep="-"),
                        paste("earlyBlast.", c(1:11), sep="-"))

CM.colnames <- c(paste("late2C", c(11:20), sep="-"),
                         paste("4C", c(18:25), sep="-"),
                         paste("8C", c(16:26), sep="-"),
                         paste("16C", c(9:22), sep="-"),
                         paste("earlyBlast.", c(12:24), sep="-"))

comp.RPKM <- data.frame(MC.comp.RPKM, CM.comp.RPKM)
colnames(comp.RPKM) <- c(MC.colnames, CM.colnames)
Cross <- c(rep("MC",71), rep("CM",56))
                               
#Use either edgeR normalization or directly manually calculated RPKM. Data is very similar.
#Here we directly manually calculated RPKM

DATA <- comp.RPKM
# load ggplot2 and start to calculate PCA

library(ggplot2)
nS<-ncol(DATA) #number of samples
nG<-nrow(DATA) #number of genes

sample.names<-colnames(DATA)
stage<-unlist(lapply(strsplit(sample.names,"-"),function(x) x[1]))
embryo<-unlist(lapply(strsplit(sample.names,"\\."),function(x) x[1]))


stages<-unique(stage)
# have 11 different stages, define 5 colors for those.
coldef.stage<-c("dark grey","red","green","blue","orange","purple","brown")
col.stage<-mat.or.vec(1,nS)
for (i in 1:length(stages)){
  idx<-which(stage==stages[i])
  col.stage[idx]<-coldef.stage[i]
}

# transform the gene counts to log10 base
log.DATA <- log10(as.matrix(DATA)+1)
DATA.pca <- prcomp(t(log.DATA)) 
# Use PC1 and PC2 to make data table for visualization
PC.table <- data.frame(DATA.pca$x[ ,c(1,2)])
# find the variations from summery() function, just FYI
summary(DATA.pca)

# Add color code for each sample into PC.table
PC.table$Stage <- stage
PC.table$Stage <- factor(PC.table$Stage, levels = stages)

PC.table$Cross <- Cross
PC.table$Cross <- factor(PC.table$Cross, levels=c("MC", "CM"))

# PCA Plot
ggplot(PC.table) + 
  geom_point(aes(x=PC1, y=PC2, color= Stage, shape=Cross), size=2.0) + 
  theme_bw() +
  scale_colour_manual(values = coldef.stage) +
  theme(axis.text=element_text(size=rel(1.0), face="bold")) +
  theme(legend.position="right") +
  theme(legend.title=element_blank()) + 
  theme(legend.key = element_blank()) +
  theme(legend.text=element_text(size = rel(1), face="bold")) +
  theme(panel.border = element_rect(fill=NA,color="black", linewidth=1)) +
  xlab("PC1") +
  ylab("PC2") +
  theme(axis.title.y = element_text(size = rel(1.3), face="bold")) +
  theme(axis.title.x = element_text(size = rel(1.3), face="bold"))



#####-Fig.S2B Clustering

# calculate pairwise correlations for all samples and cluster based on that

C<-mat.or.vec(nS,nS)
for (i in 1:nS) {
  for (j in 1:nS){
    if (i==j){ C[i,j]<-NA }
    else {  C[i,j] = cor(log2(DATA[,i]+1),log2(DATA[,j]+1),method="pearson") }
  }
}

#add cross info to sample name
SampleName.with.cross <- c(paste("CM.zygote", c(1:4), sep="-"),
                 paste("CM.early2C", c(1:6), sep="-"),
                 paste("CM.late2C", c(1:10), sep="-"),
                 paste("CM.4C", c(1:17), sep="-"),
                 paste("CM.8C", c(1:15), sep="-"),
                 paste("CM.16C", c(1:8), sep="-"),
                 paste("CM.earlyBlast.", c(1:11), sep="-"),
                 paste("MC.late2C", c(1:10), sep="-"),
                 paste("MC.4C", c(1:8), sep="-"),
                 paste("MC.8C", c(1:11), sep="-"),
                 paste("MC.16C", c(1:14), sep="-"),
                 paste("MC.earlyBlast.", c(1:13), sep="-"))

colnames(C)<-SampleName.with.cross
rownames(C)<-SampleName.with.cross
#write.table(C,file="pairwise_pearson_correlations.txt")



# and do clustering based on the correlations
dist.corr<-as.dist(1-C) 
hcl.corr<-hclust(dist.corr,method="ward.D2")

# plot a heatmap:

library(gplots)
#Use a smaller cex within legend (e.g. cex=0.75) to adjust the size of the printed text.  
#This controls the size of the legend.  
#Also, you can play with xjust and yjust for finer control of how the legend box is justified at the specified position. 
par(xpd=T)
pdf("Pairwise Pearson correlation.pdf", width = 15, height=15)
heatmap.2(C,
          ColSideColors=col.stage,
          RowSideColors=col.stage,
          Colv=as.dendrogram(hcl.corr),
          Rowv=as.dendrogram(hcl.corr),
          scale="none",
          trace="none",
          main="Correlation, Ward",
          margins=c(10,6))
dev.off() 
legend("bottomleft",
       stages,
       fill=coldef.stage,cex=0.75,
       bty='n',
       #inset=c(0.15,0,1,0)
       )
#in legend, I found "cex" value could adjust legend size and "inset" determines the location. first two digis determine the location , the last 2 digits control the Boldness of fonts, roughly. 





########Figure S2c MERVL expression
rm(list=ls())

rep.zygote.1 <- read.table(file="repeat_counts/zygote-1.rep.count.txt", sep ="\t", col.names=c("zygote.1","rep","class"))[ ,c(2,1)]
rep.zygote.2 <- read.table(file="repeat_counts/zygote-2.rep.count.txt", sep ="\t", col.names=c("zygote.2","rep","class"))[ ,c(2,1)]
rep.zygote.5 <- read.table(file="repeat_counts/zygote-5.rep.count.txt", sep ="\t", col.names=c("zygote.5","rep","class"))[ ,c(2,1)]
rep.zygote.6 <- read.table(file="repeat_counts/zygote-6.rep.count.txt", sep ="\t", col.names=c("zygote.6","rep","class"))[ ,c(2,1)]

zygote.1.MERVL <- c(rep.zygote.1[which(rep.zygote.1$rep=="MT2_Mm") ,2],rep.zygote.1[which(rep.zygote.1$rep=="MERVL-int") ,2])
zygote.2.MERVL <- c(rep.zygote.2[which(rep.zygote.2$rep=="MT2_Mm") ,2],rep.zygote.2[which(rep.zygote.2$rep=="MERVL-int") ,2])
zygote.5.MERVL <- c(rep.zygote.5[which(rep.zygote.5$rep=="MT2_Mm") ,2],rep.zygote.5[which(rep.zygote.5$rep=="MERVL-int") ,2])
zygote.6.MERVL <- c(rep.zygote.6[which(rep.zygote.6$rep=="MT2_Mm") ,2],rep.zygote.6[which(rep.zygote.6$rep=="MERVL-int") ,2])
zygote.MERVL <- rbind(zygote.1.MERVL, zygote.2.MERVL,zygote.5.MERVL, zygote.6.MERVL)
zygote.librarySize <- c(sum(rep.zygote.1$zygote.1), sum(rep.zygote.2$zygote.2), sum(rep.zygote.5$zygote.5), sum(rep.zygote.6$zygote.6))
rm(list=setdiff(ls(), c("zygote.MERVL","zygote.librarySize")))

rep.early2cell.1 <- read.table(file="repeat_counts/early2cell-1.rep.count.txt", sep ="\t", col.names=c("early2cell.1","rep","class"))[ ,c(2,1)]
rep.early2cell.2 <- read.table(file="repeat_counts/early2cell-2.rep.count.txt", sep ="\t", col.names=c("early2cell.2","rep","class"))[ ,c(2,1)]
rep.early2cell.3 <- read.table(file="repeat_counts/early2cell-3.rep.count.txt", sep ="\t", col.names=c("early2cell.3","rep","class"))[ ,c(2,1)]
rep.early2cell.4 <- read.table(file="repeat_counts/early2cell-4.rep.count.txt", sep ="\t", col.names=c("early2cell.4","rep","class"))[ ,c(2,1)]
rep.early2cell.5 <- read.table(file="repeat_counts/early2cell-5.rep.count.txt", sep ="\t", col.names=c("early2cell.5","rep","class"))[ ,c(2,1)]
rep.early2cell.6 <- read.table(file="repeat_counts/early2cell-6.rep.count.txt", sep ="\t", col.names=c("early2cell.6","rep","class"))[ ,c(2,1)]

early2cell.1.MERVL <- c(rep.early2cell.1[which(rep.early2cell.1$rep=="MT2_Mm") ,2],rep.early2cell.1[which(rep.early2cell.1$rep=="MERVL-int") ,2])
early2cell.2.MERVL <- c(rep.early2cell.2[which(rep.early2cell.2$rep=="MT2_Mm") ,2],rep.early2cell.2[which(rep.early2cell.2$rep=="MERVL-int") ,2])
early2cell.3.MERVL <- c(rep.early2cell.3[which(rep.early2cell.3$rep=="MT2_Mm") ,2],rep.early2cell.3[which(rep.early2cell.3$rep=="MERVL-int") ,2])
early2cell.4.MERVL <- c(rep.early2cell.4[which(rep.early2cell.4$rep=="MT2_Mm") ,2],rep.early2cell.4[which(rep.early2cell.4$rep=="MERVL-int") ,2])
early2cell.5.MERVL <- c(rep.early2cell.5[which(rep.early2cell.5$rep=="MT2_Mm") ,2],rep.early2cell.5[which(rep.early2cell.5$rep=="MERVL-int") ,2])
early2cell.6.MERVL <- c(rep.early2cell.6[which(rep.early2cell.6$rep=="MT2_Mm") ,2],rep.early2cell.6[which(rep.early2cell.6$rep=="MERVL-int") ,2])
early2cell.MERVL <- rbind(early2cell.1.MERVL, early2cell.2.MERVL,early2cell.3.MERVL, early2cell.4.MERVL,early2cell.5.MERVL, early2cell.6.MERVL)
early2cell.librarySize <- c(sum(rep.early2cell.1$early2cell.1), sum(rep.early2cell.2$early2cell.2), 
                            sum(rep.early2cell.3$early2cell.3), sum(rep.early2cell.4$early2cell.4),
                            sum(rep.early2cell.5$early2cell.5), sum(rep.early2cell.6$early2cell.6))
rm(list=setdiff(ls(), c("zygote.MERVL","zygote.librarySize", 
                        "early2cell.MERVL","early2cell.librarySize")))


rep.late2cell.2 <- read.table(file="repeat_counts/late2cell-2.rep.count.txt", sep ="\t", col.names=c("late2cell.2","rep","class"))[ ,c(2,1)]
rep.late2cell.3 <- read.table(file="repeat_counts/late2cell-3.rep.count.txt", sep ="\t", col.names=c("late2cell.3","rep","class"))[ ,c(2,1)]
rep.late2cell.4 <- read.table(file="repeat_counts/late2cell-4.rep.count.txt", sep ="\t", col.names=c("late2cell.4","rep","class"))[ ,c(2,1)]
rep.late2cell.5 <- read.table(file="repeat_counts/late2cell-5.rep.count.txt", sep ="\t", col.names=c("late2cell.5","rep","class"))[ ,c(2,1)]
rep.late2cell.6 <- read.table(file="repeat_counts/late2cell-6.rep.count.txt", sep ="\t", col.names=c("late2cell.6","rep","class"))[ ,c(2,1)]
rep.late2cell.7 <- read.table(file="repeat_counts/late2cell-7.rep.count.txt", sep ="\t", col.names=c("late2cell.7","rep","class"))[ ,c(2,1)]
rep.late2cell.8 <- read.table(file="repeat_counts/late2cell-8.rep.count.txt", sep ="\t", col.names=c("late2cell.8","rep","class"))[ ,c(2,1)]
rep.late2cell.9 <- read.table(file="repeat_counts/late2cell-9.rep.count.txt", sep ="\t", col.names=c("late2cell.9","rep","class"))[ ,c(2,1)]
rep.late2cell.10 <- read.table(file="repeat_counts/late2cell-10.rep.count.txt", sep ="\t", col.names=c("late2cell.10","rep","class"))[ ,c(2,1)]
rep.late2cell.11 <- read.table(file="repeat_counts/late2cell-11.rep.count.txt", sep ="\t", col.names=c("late2cell.11","rep","class"))[ ,c(2,1)]

late2cell.2.MERVL <- c(rep.late2cell.2[which(rep.late2cell.2$rep=="MT2_Mm") ,2],rep.late2cell.2[which(rep.late2cell.2$rep=="MERVL-int") ,2])
late2cell.3.MERVL <- c(rep.late2cell.3[which(rep.late2cell.3$rep=="MT2_Mm") ,2],rep.late2cell.3[which(rep.late2cell.3$rep=="MERVL-int") ,2])
late2cell.4.MERVL <- c(rep.late2cell.4[which(rep.late2cell.4$rep=="MT2_Mm") ,2],rep.late2cell.4[which(rep.late2cell.4$rep=="MERVL-int") ,2])
late2cell.5.MERVL <- c(rep.late2cell.5[which(rep.late2cell.5$rep=="MT2_Mm") ,2],rep.late2cell.5[which(rep.late2cell.5$rep=="MERVL-int") ,2])
late2cell.6.MERVL <- c(rep.late2cell.6[which(rep.late2cell.6$rep=="MT2_Mm") ,2],rep.late2cell.6[which(rep.late2cell.6$rep=="MERVL-int") ,2])
late2cell.7.MERVL <- c(rep.late2cell.7[which(rep.late2cell.7$rep=="MT2_Mm") ,2],rep.late2cell.7[which(rep.late2cell.7$rep=="MERVL-int") ,2])
late2cell.8.MERVL <- c(rep.late2cell.8[which(rep.late2cell.8$rep=="MT2_Mm") ,2],rep.late2cell.8[which(rep.late2cell.8$rep=="MERVL-int") ,2])
late2cell.9.MERVL <- c(rep.late2cell.9[which(rep.late2cell.9$rep=="MT2_Mm") ,2],rep.late2cell.9[which(rep.late2cell.9$rep=="MERVL-int") ,2])
late2cell.10.MERVL <- c(rep.late2cell.10[which(rep.late2cell.10$rep=="MT2_Mm") ,2],rep.late2cell.10[which(rep.late2cell.10$rep=="MERVL-int") ,2])
late2cell.11.MERVL <- c(rep.late2cell.11[which(rep.late2cell.11$rep=="MT2_Mm") ,2],rep.late2cell.11[which(rep.late2cell.11$rep=="MERVL-int") ,2])
late2cell.MERVL <- rbind(late2cell.2.MERVL,late2cell.3.MERVL, late2cell.4.MERVL,late2cell.5.MERVL, late2cell.6.MERVL,
                         late2cell.7.MERVL, late2cell.8.MERVL, late2cell.9.MERVL, late2cell.10.MERVL, late2cell.11.MERVL)
late2cell.librarySize <- c(sum(rep.late2cell.2$late2cell.2), sum(rep.late2cell.3$late2cell.3), 
                           sum(rep.late2cell.4$late2cell.4), sum(rep.late2cell.5$late2cell.5),
                           sum(rep.late2cell.6$late2cell.6), sum(rep.late2cell.7$late2cell.7),
                           sum(rep.late2cell.8$late2cell.8), sum(rep.late2cell.9$late2cell.9),
                           sum(rep.late2cell.10$late2cell.10),sum(rep.late2cell.11$late2cell.11))

rm(list=setdiff(ls(), c("zygote.MERVL","zygote.librarySize", 
                        "early2cell.MERVL","early2cell.librarySize",
                        "late2cell.MERVL","late2cell.librarySize")))


#4cell
rep.4cell.1 <- read.table(file="repeat_counts/4cell-1.rep.count.txt", sep ="\t", col.names=c("4cell.1","rep","class"))[ ,c(2,1)]
rep.4cell.2 <- read.table(file="repeat_counts/4cell-2.rep.count.txt", sep ="\t", col.names=c("4cell.2","rep","class"))[ ,c(2,1)]
rep.4cell.3 <- read.table(file="repeat_counts/4cell-3.rep.count.txt", sep ="\t", col.names=c("4cell.3","rep","class"))[ ,c(2,1)]
rep.4cell.4 <- read.table(file="repeat_counts/4cell-4.rep.count.txt", sep ="\t", col.names=c("4cell.4","rep","class"))[ ,c(2,1)]
rep.4cell.5 <- read.table(file="repeat_counts/4cell-5.rep.count.txt", sep ="\t", col.names=c("4cell.5","rep","class"))[ ,c(2,1)]
rep.4cell.7 <- read.table(file="repeat_counts/4cell-7.rep.count.txt", sep ="\t", col.names=c("4cell.7","rep","class"))[ ,c(2,1)]
rep.4cell.8 <- read.table(file="repeat_counts/4cell-8.rep.count.txt", sep ="\t", col.names=c("4cell.8","rep","class"))[ ,c(2,1)]
rep.4cell.9 <- read.table(file="repeat_counts/4cell-9.rep.count.txt", sep ="\t", col.names=c("4cell.9","rep","class"))[ ,c(2,1)]
rep.4cell.10 <- read.table(file="repeat_counts/4cell-10.rep.count.txt", sep ="\t", col.names=c("4cell.10","rep","class"))[ ,c(2,1)]
rep.4cell.11 <- read.table(file="repeat_counts/4cell-11.rep.count.txt", sep ="\t", col.names=c("4cell.11","rep","class"))[ ,c(2,1)]
rep.4cell.12 <- read.table(file="repeat_counts/4cell-12.rep.count.txt", sep ="\t", col.names=c("4cell.12","rep","class"))[ ,c(2,1)]
rep.4cell.13 <- read.table(file="repeat_counts/4cell-13.rep.count.txt", sep ="\t", col.names=c("4cell.13","rep","class"))[ ,c(2,1)]
rep.4cell.14 <- read.table(file="repeat_counts/4cell-14.rep.count.txt", sep ="\t", col.names=c("4cell.14","rep","class"))[ ,c(2,1)]
rep.4cell.15 <- read.table(file="repeat_counts/4cell-15.rep.count.txt", sep ="\t", col.names=c("4cell.15","rep","class"))[ ,c(2,1)]
rep.4cell.16 <- read.table(file="repeat_counts/4cell-16.rep.count.txt", sep ="\t", col.names=c("4cell.16","rep","class"))[ ,c(2,1)]
rep.4cell.17 <- read.table(file="repeat_counts/4cell-17.rep.count.txt", sep ="\t", col.names=c("4cell.17","rep","class"))[ ,c(2,1)]
rep.4cell.18 <- read.table(file="repeat_counts/4cell-18.rep.count.txt", sep ="\t", col.names=c("4cell.18","rep","class"))[ ,c(2,1)]

N4cell.1.MERVL <- c(rep.4cell.1[which(rep.4cell.1$rep=="MT2_Mm") ,2],rep.4cell.1[which(rep.4cell.1$rep=="MERVL-int") ,2])
N4cell.2.MERVL <- c(rep.4cell.2[which(rep.4cell.2$rep=="MT2_Mm") ,2],rep.4cell.2[which(rep.4cell.2$rep=="MERVL-int") ,2])
N4cell.3.MERVL <- c(rep.4cell.3[which(rep.4cell.3$rep=="MT2_Mm") ,2],rep.4cell.3[which(rep.4cell.3$rep=="MERVL-int") ,2])
N4cell.4.MERVL <- c(rep.4cell.4[which(rep.4cell.4$rep=="MT2_Mm") ,2],rep.4cell.4[which(rep.4cell.4$rep=="MERVL-int") ,2])
N4cell.5.MERVL <- c(rep.4cell.5[which(rep.4cell.5$rep=="MT2_Mm") ,2],rep.4cell.5[which(rep.4cell.5$rep=="MERVL-int") ,2])
N4cell.7.MERVL <- c(rep.4cell.7[which(rep.4cell.7$rep=="MT2_Mm") ,2],rep.4cell.7[which(rep.4cell.7$rep=="MERVL-int") ,2])
N4cell.8.MERVL <- c(rep.4cell.8[which(rep.4cell.8$rep=="MT2_Mm") ,2],rep.4cell.8[which(rep.4cell.8$rep=="MERVL-int") ,2])
N4cell.9.MERVL <- c(rep.4cell.9[which(rep.4cell.9$rep=="MT2_Mm") ,2],rep.4cell.9[which(rep.4cell.9$rep=="MERVL-int") ,2])
N4cell.10.MERVL <- c(rep.4cell.10[which(rep.4cell.10$rep=="MT2_Mm") ,2],rep.4cell.10[which(rep.4cell.10$rep=="MERVL-int") ,2])
N4cell.11.MERVL <- c(rep.4cell.11[which(rep.4cell.11$rep=="MT2_Mm") ,2],rep.4cell.11[which(rep.4cell.11$rep=="MERVL-int") ,2])
N4cell.12.MERVL <- c(rep.4cell.12[which(rep.4cell.12$rep=="MT2_Mm") ,2],rep.4cell.12[which(rep.4cell.12$rep=="MERVL-int") ,2])
N4cell.13.MERVL <- c(rep.4cell.13[which(rep.4cell.13$rep=="MT2_Mm") ,2],rep.4cell.13[which(rep.4cell.13$rep=="MERVL-int") ,2])
N4cell.14.MERVL <- c(rep.4cell.14[which(rep.4cell.14$rep=="MT2_Mm") ,2],rep.4cell.14[which(rep.4cell.14$rep=="MERVL-int") ,2])
N4cell.15.MERVL <- c(rep.4cell.15[which(rep.4cell.15$rep=="MT2_Mm") ,2],rep.4cell.15[which(rep.4cell.15$rep=="MERVL-int") ,2])
N4cell.16.MERVL <- c(rep.4cell.16[which(rep.4cell.16$rep=="MT2_Mm") ,2],rep.4cell.16[which(rep.4cell.16$rep=="MERVL-int") ,2])
N4cell.17.MERVL <- c(rep.4cell.17[which(rep.4cell.17$rep=="MT2_Mm") ,2],rep.4cell.17[which(rep.4cell.17$rep=="MERVL-int") ,2])
N4cell.18.MERVL <- c(rep.4cell.18[which(rep.4cell.18$rep=="MT2_Mm") ,2],rep.4cell.18[which(rep.4cell.18$rep=="MERVL-int") ,2])
N4cell.MERVL <- rbind(N4cell.1.MERVL,N4cell.2.MERVL,N4cell.3.MERVL, N4cell.4.MERVL,N4cell.5.MERVL,
                      N4cell.7.MERVL, N4cell.8.MERVL, N4cell.9.MERVL, N4cell.10.MERVL, N4cell.11.MERVL,
                      N4cell.12.MERVL, N4cell.13.MERVL, N4cell.14.MERVL, N4cell.15.MERVL, N4cell.16.MERVL,
                      N4cell.17.MERVL, N4cell.18.MERVL)
N4cell.librarySize <- c(sum(rep.4cell.1$X4cell.1), sum(rep.4cell.2$X4cell.2), 
                        sum(rep.4cell.3$X4cell.3), sum(rep.4cell.4$X4cell.4),
                        sum(rep.4cell.5$X4cell.5), sum(rep.4cell.7$X4cell.7),
                        sum(rep.4cell.8$X4cell.8), sum(rep.4cell.9$X4cell.9),
                        sum(rep.4cell.10$X4cell.10),sum(rep.4cell.11$X4cell.11),
                        sum(rep.4cell.12$X4cell.12),sum(rep.4cell.13$X4cell.13),
                        sum(rep.4cell.14$X4cell.14),sum(rep.4cell.15$X4cell.15),
                        sum(rep.4cell.16$X4cell.16),sum(rep.4cell.17$X4cell.17),
                        sum(rep.4cell.18$X4cell.18))
rm(list=setdiff(ls(), c("zygote.MERVL","zygote.librarySize", 
                        "early2cell.MERVL","early2cell.librarySize",
                        "late2cell.MERVL","late2cell.librarySize",
                        "N4cell.MERVL","N4cell.librarySize")))



#8cell
rep.8cell.6 <- read.table(file="repeat_counts/8cell-6.rep.count.txt", sep ="\t", col.names=c("8cell.6","rep","class"))[ ,c(2,1)]
rep.8cell.7 <- read.table(file="repeat_counts/8cell-7.rep.count.txt", sep ="\t", col.names=c("8cell.7","rep","class"))[ ,c(2,1)]
rep.8cell.8 <- read.table(file="repeat_counts/8cell-8.rep.count.txt", sep ="\t", col.names=c("8cell.8","rep","class"))[ ,c(2,1)]
rep.8cell.9 <- read.table(file="repeat_counts/8cell-9.rep.count.txt", sep ="\t", col.names=c("8cell.9","rep","class"))[ ,c(2,1)]
rep.8cell.10 <- read.table(file="repeat_counts/8cell-10.rep.count.txt", sep ="\t", col.names=c("8cell.10","rep","class"))[ ,c(2,1)]
rep.8cell.11 <- read.table(file="repeat_counts/8cell-11.rep.count.txt", sep ="\t", col.names=c("8cell.11","rep","class"))[ ,c(2,1)]
rep.8cell.12 <- read.table(file="repeat_counts/8cell-12.rep.count.txt", sep ="\t", col.names=c("8cell.12","rep","class"))[ ,c(2,1)]
rep.8cell.13 <- read.table(file="repeat_counts/8cell-13.rep.count.txt", sep ="\t", col.names=c("8cell.13","rep","class"))[ ,c(2,1)]
rep.8cell.14 <- read.table(file="repeat_counts/8cell-14.rep.count.txt", sep ="\t", col.names=c("8cell.14","rep","class"))[ ,c(2,1)]
rep.8cell.15 <- read.table(file="repeat_counts/8cell-15.rep.count.txt", sep ="\t", col.names=c("8cell.15","rep","class"))[ ,c(2,1)]
rep.8cell.16 <- read.table(file="repeat_counts/8cell-16.rep.count.txt", sep ="\t", col.names=c("8cell.16","rep","class"))[ ,c(2,1)]
rep.8cell.17 <- read.table(file="repeat_counts/8cell-17.rep.count.txt", sep ="\t", col.names=c("8cell.17","rep","class"))[ ,c(2,1)]
rep.8cell.18 <- read.table(file="repeat_counts/8cell-18.rep.count.txt", sep ="\t", col.names=c("8cell.18","rep","class"))[ ,c(2,1)]
rep.8cell.19 <- read.table(file="repeat_counts/8cell-19.rep.count.txt", sep ="\t", col.names=c("8cell.19","rep","class"))[ ,c(2,1)]
rep.8cell.20 <- read.table(file="repeat_counts/8cell-20.rep.count.txt", sep ="\t", col.names=c("8cell.20","rep","class"))[ ,c(2,1)]

N8cell.6.MERVL <- c(rep.8cell.6[which(rep.8cell.6$rep=="MT2_Mm") ,2],rep.8cell.6[which(rep.8cell.6$rep=="MERVL-int") ,2])
N8cell.7.MERVL <- c(rep.8cell.7[which(rep.8cell.7$rep=="MT2_Mm") ,2],rep.8cell.7[which(rep.8cell.7$rep=="MERVL-int") ,2])
N8cell.8.MERVL <- c(rep.8cell.8[which(rep.8cell.8$rep=="MT2_Mm") ,2],rep.8cell.8[which(rep.8cell.8$rep=="MERVL-int") ,2])
N8cell.9.MERVL <- c(rep.8cell.9[which(rep.8cell.9$rep=="MT2_Mm") ,2],rep.8cell.9[which(rep.8cell.9$rep=="MERVL-int") ,2])
N8cell.10.MERVL <- c(rep.8cell.10[which(rep.8cell.10$rep=="MT2_Mm") ,2],rep.8cell.10[which(rep.8cell.10$rep=="MERVL-int") ,2])
N8cell.11.MERVL <- c(rep.8cell.11[which(rep.8cell.11$rep=="MT2_Mm") ,2],rep.8cell.11[which(rep.8cell.11$rep=="MERVL-int") ,2])
N8cell.12.MERVL <- c(rep.8cell.12[which(rep.8cell.12$rep=="MT2_Mm") ,2],rep.8cell.12[which(rep.8cell.12$rep=="MERVL-int") ,2])
N8cell.13.MERVL <- c(rep.8cell.13[which(rep.8cell.13$rep=="MT2_Mm") ,2],rep.8cell.13[which(rep.8cell.13$rep=="MERVL-int") ,2])
N8cell.14.MERVL <- c(rep.8cell.14[which(rep.8cell.14$rep=="MT2_Mm") ,2],rep.8cell.14[which(rep.8cell.14$rep=="MERVL-int") ,2])
N8cell.15.MERVL <- c(rep.8cell.15[which(rep.8cell.15$rep=="MT2_Mm") ,2],rep.8cell.15[which(rep.8cell.15$rep=="MERVL-int") ,2])
N8cell.16.MERVL <- c(rep.8cell.16[which(rep.8cell.16$rep=="MT2_Mm") ,2],rep.8cell.16[which(rep.8cell.16$rep=="MERVL-int") ,2])
N8cell.17.MERVL <- c(rep.8cell.17[which(rep.8cell.17$rep=="MT2_Mm") ,2],rep.8cell.17[which(rep.8cell.17$rep=="MERVL-int") ,2])
N8cell.18.MERVL <- c(rep.8cell.18[which(rep.8cell.18$rep=="MT2_Mm") ,2],rep.8cell.18[which(rep.8cell.18$rep=="MERVL-int") ,2])
N8cell.19.MERVL <- c(rep.8cell.19[which(rep.8cell.19$rep=="MT2_Mm") ,2],rep.8cell.19[which(rep.8cell.19$rep=="MERVL-int") ,2])
N8cell.20.MERVL <- c(rep.8cell.20[which(rep.8cell.20$rep=="MT2_Mm") ,2],rep.8cell.20[which(rep.8cell.20$rep=="MERVL-int") ,2])
N8cell.MERVL <- rbind(N8cell.6.MERVL,N8cell.7.MERVL, N8cell.8.MERVL, N8cell.9.MERVL, N8cell.10.MERVL, N8cell.11.MERVL,
                      N8cell.12.MERVL, N8cell.13.MERVL, N8cell.14.MERVL, N8cell.15.MERVL, N8cell.16.MERVL,N8cell.17.MERVL, 
                      N8cell.18.MERVL,N8cell.19.MERVL,N8cell.20.MERVL)
N8cell.librarySize <- c(sum(rep.8cell.6$X8cell.6), sum(rep.8cell.7$X8cell.7),
                        sum(rep.8cell.8$X8cell.8), sum(rep.8cell.9$X8cell.9),
                        sum(rep.8cell.10$X8cell.10),sum(rep.8cell.11$X8cell.11),
                        sum(rep.8cell.12$X8cell.12),sum(rep.8cell.13$X8cell.13),
                        sum(rep.8cell.14$X8cell.14),sum(rep.8cell.15$X8cell.15),
                        sum(rep.8cell.16$X8cell.16),sum(rep.8cell.17$X8cell.17),
                        sum(rep.8cell.18$X8cell.18),sum(rep.8cell.19$X8cell.19),
                        sum(rep.8cell.20$X8cell.20))
rm(list=setdiff(ls(), c("zygote.MERVL","zygote.librarySize", 
                        "early2cell.MERVL","early2cell.librarySize",
                        "late2cell.MERVL","late2cell.librarySize",
                        "N4cell.MERVL","N4cell.librarySize",
                        "N8cell.MERVL","N8cell.librarySize")))

#16cell
rep.16cell.1 <- read.table(file="repeat_counts/16cell-1.rep.count.txt", sep ="\t", col.names=c("16cell.1","rep","class"))[ ,c(2,1)]
rep.16cell.2 <- read.table(file="repeat_counts/16cell-2.rep.count.txt", sep ="\t", col.names=c("16cell.2","rep","class"))[ ,c(2,1)]
rep.16cell.3 <- read.table(file="repeat_counts/16cell-3.rep.count.txt", sep ="\t", col.names=c("16cell.3","rep","class"))[ ,c(2,1)]
rep.16cell.4 <- read.table(file="repeat_counts/16cell-4.rep.count.txt", sep ="\t", col.names=c("16cell.4","rep","class"))[ ,c(2,1)]
rep.16cell.5 <- read.table(file="repeat_counts/16cell-5.rep.count.txt", sep ="\t", col.names=c("16cell.5","rep","class"))[ ,c(2,1)]
rep.16cell.6 <- read.table(file="repeat_counts/16cell-6.rep.count.txt", sep ="\t", col.names=c("16cell.6","rep","class"))[ ,c(2,1)]
rep.16cell.7 <- read.table(file="repeat_counts/16cell-7.rep.count.txt", sep ="\t", col.names=c("16cell.7","rep","class"))[ ,c(2,1)]
rep.16cell.8 <- read.table(file="repeat_counts/16cell-8.rep.count.txt", sep ="\t", col.names=c("16cell.8","rep","class"))[ ,c(2,1)]

N16cell.1.MERVL <- c(rep.16cell.1[which(rep.16cell.1$rep=="MT2_Mm") ,2],rep.16cell.1[which(rep.16cell.1$rep=="MERVL-int") ,2])
N16cell.2.MERVL <- c(rep.16cell.2[which(rep.16cell.2$rep=="MT2_Mm") ,2],rep.16cell.2[which(rep.16cell.2$rep=="MERVL-int") ,2])
N16cell.3.MERVL <- c(rep.16cell.3[which(rep.16cell.3$rep=="MT2_Mm") ,2],rep.16cell.3[which(rep.16cell.3$rep=="MERVL-int") ,2])
N16cell.4.MERVL <- c(rep.16cell.4[which(rep.16cell.4$rep=="MT2_Mm") ,2],rep.16cell.4[which(rep.16cell.4$rep=="MERVL-int") ,2])
N16cell.5.MERVL <- c(rep.16cell.5[which(rep.16cell.5$rep=="MT2_Mm") ,2],rep.16cell.5[which(rep.16cell.5$rep=="MERVL-int") ,2])
N16cell.6.MERVL <- c(rep.16cell.6[which(rep.16cell.6$rep=="MT2_Mm") ,2],rep.16cell.6[which(rep.16cell.6$rep=="MERVL-int") ,2])
N16cell.7.MERVL <- c(rep.16cell.7[which(rep.16cell.7$rep=="MT2_Mm") ,2],rep.16cell.7[which(rep.16cell.7$rep=="MERVL-int") ,2])
N16cell.8.MERVL <- c(rep.16cell.8[which(rep.16cell.8$rep=="MT2_Mm") ,2],rep.16cell.8[which(rep.16cell.8$rep=="MERVL-int") ,2])

N16cell.MERVL <- rbind(N16cell.1.MERVL,N16cell.2.MERVL,N16cell.3.MERVL, N16cell.4.MERVL,N16cell.5.MERVL,
                       N16cell.6.MERVL, N16cell.7.MERVL, N16cell.8.MERVL)
N16cell.librarySize <- c(sum(rep.16cell.1$X16cell.1), sum(rep.16cell.2$X16cell.2),
                         sum(rep.16cell.3$X16cell.3), sum(rep.16cell.4$X16cell.4),
                         sum(rep.16cell.5$X16cell.5),sum(rep.16cell.6$X16cell.6),
                         sum(rep.16cell.7$X16cell.7),sum(rep.16cell.8$X16cell.8))
rm(list=setdiff(ls(), c("zygote.MERVL","zygote.librarySize", 
                        "early2cell.MERVL","early2cell.librarySize",
                        "late2cell.MERVL","late2cell.librarySize",
                        "N4cell.MERVL","N4cell.librarySize",
                        "N8cell.MERVL","N8cell.librarySize",
                        "N16cell.MERVL","N16cell.librarySize")))

#earlyBlast
rep.earlyBlast.1 <- read.table(file="repeat_counts/earlyBlast-1.rep.count.txt", sep ="\t", col.names=c("earlyBlast.1","rep","class"))[ ,c(2,1)]
rep.earlyBlast.2 <- read.table(file="repeat_counts/earlyBlast-2.rep.count.txt", sep ="\t", col.names=c("earlyBlast.2","rep","class"))[ ,c(2,1)]
rep.earlyBlast.3 <- read.table(file="repeat_counts/earlyBlast-3.rep.count.txt", sep ="\t", col.names=c("earlyBlast.3","rep","class"))[ ,c(2,1)]
rep.earlyBlast.4 <- read.table(file="repeat_counts/earlyBlast-4.rep.count.txt", sep ="\t", col.names=c("earlyBlast.4","rep","class"))[ ,c(2,1)]
rep.earlyBlast.5 <- read.table(file="repeat_counts/earlyBlast-5.rep.count.txt", sep ="\t", col.names=c("earlyBlast.5","rep","class"))[ ,c(2,1)]
rep.earlyBlast.6 <- read.table(file="repeat_counts/earlyBlast-6.rep.count.txt", sep ="\t", col.names=c("earlyBlast.6","rep","class"))[ ,c(2,1)]
rep.earlyBlast.7 <- read.table(file="repeat_counts/earlyBlast-7.rep.count.txt", sep ="\t", col.names=c("earlyBlast.7","rep","class"))[ ,c(2,1)]
rep.earlyBlast.8 <- read.table(file="repeat_counts/earlyBlast-8.rep.count.txt", sep ="\t", col.names=c("earlyBlast.8","rep","class"))[ ,c(2,1)]
rep.earlyBlast.9 <- read.table(file="repeat_counts/earlyBlast-9.rep.count.txt", sep ="\t", col.names=c("earlyBlast.9","rep","class"))[ ,c(2,1)]
rep.earlyBlast.10 <- read.table(file="repeat_counts/earlyBlast-10.rep.count.txt", sep ="\t", col.names=c("earlyBlast.10","rep","class"))[ ,c(2,1)]
rep.earlyBlast.11 <- read.table(file="repeat_counts/earlyBlast-11.rep.count.txt", sep ="\t", col.names=c("earlyBlast.11","rep","class"))[ ,c(2,1)]

earlyBlast.1.MERVL <- c(rep.earlyBlast.1[which(rep.earlyBlast.1$rep=="MT2_Mm") ,2],rep.earlyBlast.1[which(rep.earlyBlast.1$rep=="MERVL-int") ,2])
earlyBlast.2.MERVL <- c(rep.earlyBlast.2[which(rep.earlyBlast.2$rep=="MT2_Mm") ,2],rep.earlyBlast.2[which(rep.earlyBlast.2$rep=="MERVL-int") ,2])
earlyBlast.3.MERVL <- c(rep.earlyBlast.3[which(rep.earlyBlast.3$rep=="MT2_Mm") ,2],rep.earlyBlast.3[which(rep.earlyBlast.3$rep=="MERVL-int") ,2])
earlyBlast.4.MERVL <- c(rep.earlyBlast.4[which(rep.earlyBlast.4$rep=="MT2_Mm") ,2],rep.earlyBlast.4[which(rep.earlyBlast.4$rep=="MERVL-int") ,2])
earlyBlast.5.MERVL <- c(rep.earlyBlast.5[which(rep.earlyBlast.5$rep=="MT2_Mm") ,2],rep.earlyBlast.5[which(rep.earlyBlast.5$rep=="MERVL-int") ,2])
earlyBlast.6.MERVL <- c(rep.earlyBlast.6[which(rep.earlyBlast.6$rep=="MT2_Mm") ,2],rep.earlyBlast.6[which(rep.earlyBlast.6$rep=="MERVL-int") ,2])
earlyBlast.7.MERVL <- c(rep.earlyBlast.7[which(rep.earlyBlast.7$rep=="MT2_Mm") ,2],rep.earlyBlast.7[which(rep.earlyBlast.7$rep=="MERVL-int") ,2])
earlyBlast.8.MERVL <- c(rep.earlyBlast.8[which(rep.earlyBlast.8$rep=="MT2_Mm") ,2],rep.earlyBlast.8[which(rep.earlyBlast.8$rep=="MERVL-int") ,2])
earlyBlast.9.MERVL <- c(rep.earlyBlast.9[which(rep.earlyBlast.9$rep=="MT2_Mm") ,2],rep.earlyBlast.9[which(rep.earlyBlast.9$rep=="MERVL-int") ,2])
earlyBlast.10.MERVL <- c(rep.earlyBlast.10[which(rep.earlyBlast.10$rep=="MT2_Mm") ,2],rep.earlyBlast.10[which(rep.earlyBlast.10$rep=="MERVL-int") ,2])
earlyBlast.11.MERVL <- c(rep.earlyBlast.11[which(rep.earlyBlast.11$rep=="MT2_Mm") ,2],rep.earlyBlast.11[which(rep.earlyBlast.11$rep=="MERVL-int") ,2])
earlyBlast.MERVL <- rbind(earlyBlast.1.MERVL,earlyBlast.2.MERVL,earlyBlast.3.MERVL, earlyBlast.4.MERVL,earlyBlast.5.MERVL,earlyBlast.6.MERVL,
                          earlyBlast.7.MERVL, earlyBlast.8.MERVL, earlyBlast.9.MERVL, earlyBlast.10.MERVL, earlyBlast.11.MERVL)
earlyBlast.librarySize <- c(sum(rep.earlyBlast.1$earlyBlast.1), sum(rep.earlyBlast.2$earlyBlast.2),
                            sum(rep.earlyBlast.3$earlyBlast.3), sum(rep.earlyBlast.4$earlyBlast.4),
                            sum(rep.earlyBlast.5$earlyBlast.5),sum(rep.earlyBlast.6$earlyBlast.6),
                            sum(rep.earlyBlast.7$earlyBlast.7),sum(rep.earlyBlast.8$earlyBlast.8),
                            sum(rep.earlyBlast.9$earlyBlast.9),sum(rep.earlyBlast.10$earlyBlast.10),
                            sum(rep.earlyBlast.11$earlyBlast.11))

rm(list=setdiff(ls(), c("zygote.MERVL","zygote.librarySize", 
                        "early2cell.MERVL","early2cell.librarySize",
                        "late2cell.MERVL","late2cell.librarySize",
                        "N4cell.MERVL","N4cell.librarySize",
                        "N8cell.MERVL","N8cell.librarySize",
                        "N16cell.MERVL","N16cell.librarySize",
                        "earlyBlast.MERVL","earlyBlast.librarySize")))

combined.MERVL <- rbind(zygote.MERVL,early2cell.MERVL,late2cell.MERVL,N4cell.MERVL,N8cell.MERVL,N16cell.MERVL,earlyBlast.MERVL)
library.size <- c(zygote.librarySize,early2cell.librarySize,late2cell.librarySize,N4cell.librarySize,N8cell.librarySize,N16cell.librarySize,earlyBlast.librarySize)/1000000

colnames(combined.MERVL) <- c("MT2_Mm","MERVL-int")

#calculate RPM
MERVL.RPM <- combined.MERVL[1, ]/library.size[1]
for (i in 2:nrow(combined.MERVL)) {
  MERVL.RPM.next <- combined.MERVL[i, ]/library.size[i]
  MERVL.RPM <- rbind(MERVL.RPM, MERVL.RPM.next)
}
rownames(MERVL.RPM) <- rownames(combined.MERVL)
colnames(MERVL.RPM) <- c("MT2_Mm","MERVL-int")

#Add repeat length from concensus sequence
Repeat.length <- read.table(file="~/Dropbox (Partners HealthCare)/R_analyses/Data/Raw/rep_expression/repeat_length.txt", sep="\t",row.names=1)
MT2_Mm.length <- Repeat.length["MT2_Mm", 1]/1000
MERVL.insert.length <- Repeat.length["MERVL-int", 1]/1000

MERVL.RPKM <- cbind(MERVL.RPM[,1]/MT2_Mm.length, MERVL.RPM[,2]/MERVL.insert.length)

log.MERVL.RPKM <- log(MERVL.RPKM+0.01)
colnames(log.MERVL.RPKM) <- c("MT2_Mm","MERVL-int")



#Use the following for Quantile breaks, each color represtns an equal proportion of the data.
#quantile_breaks <- function(xs, n = 10) {
#breaks <- quantile(xs, probs = seq(0, 1, length.out = n))
#breaks[!duplicated(breaks)]
#}

library(pheatmap)

pheatmap(t(log.MERVL.RPKM), 
         color = colorpanel(75,"#0066CC","#FFFF99","#CC3333"),
         #annotation_col=annotation, 
         #annotation_colors=annotation_color,
         cluster_rows = F, 
         cluster_cols = F, 
         show_rownames = T, 
         show_colnames = F,  
         border_color=NA)





###########################################################
#Figure S2d.Make a heat map to show the expression of marker genes in different stage.
###########################################################
source("Read_data.R")
rm(list=setdiff(ls(), c("GeneInfo","MC.comp.RPKM")))

# get autosomal gene and chrX gene
autoGenes <- rownames(GeneInfo[which(GeneInfo$Chr != "chrX" & GeneInfo$Chr != "chrY"), ])
auto.RPKM <- MC.comp.RPKM[autoGenes, ]

Marker.gene <- c("Cdx2", "Gata3", "Eomes","Pou5f1","Nanog","Gata6","Sox17","Gata4","Zfp352","Zscan4d","Usp17la")

Marker.rpkm <- MC.comp.RPKM[Marker.gene, ]
library(pheatmap)
pheatmap(log(Marker.rpkm+0.1), cluster_cols = F,cluster_rows = T,
         show_rownames = T, show_colnames = T, border_color=NA)
