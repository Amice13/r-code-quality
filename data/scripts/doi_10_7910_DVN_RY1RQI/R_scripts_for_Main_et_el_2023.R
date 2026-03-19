###Set working directory###
wd <- "/~/con_trees/"
setwd(wd)

#Load Libraries
library(rwty)
library(ape)
library(TreeDist)
library(phangorn)
library(hierfstat)
library(poppr)
library(phytools)

###Read in Bayesian consensus tree files###
mitogenome <- read.nexus("bradypodion_mitogenomes.nex.con.tre")
ND6 <- read.nexus("ND6.nex.con.tre")
ND5 <- read.nexus("ND5.nex.con.tre")
ND4 <- read.nexus("ND4.nex.con.tre")
ND4l <- read.nexus("ND4l.nex.con.tre")
ND3 <- read.nexus("ND3.nex.con.tre")
ND2 <- read.nexus("ND2.nex.con.tre")
ND1 <- read.nexus("ND1.nex.con.tre")
COI <- read.nexus("COI.nex.con.tre")
COII <- read.nexus("COII.nex.con.tre")
COIII <- read.nexus("COIII.nex.con.tre")
CytB <- read.nexus("CytB.nex.con.tre")
ATP6 <- read.nexus("ATP6.nex.con.tre")
ATP8 <- read.nexus("ATP8.nex.con.tre")
lrRNA <- read.nexus("16S.nex.con.tre")
srRNA <- read.nexus("12S.nex.con.tre")
PCG <- read.nexus("protein_coding_mitogenome.nex.con.tre")
rRNA <- read.nexus("rRNA_mitogenome.nex.con.tre")
short <- read.nexus("short_16S.nex.con.tre")
ND2_16S <- read.nexus("ND2_16S.nex.con.tre")
ND2_ND5 <- read.nexus("ND2_ND5.nex.con.tre")

###Create an object of all Bayesian consensus tree files called all.genes###
all.genes <- c(mitogenome, ND5, ND4, ND4l, ND3, ND2, ND1, COI, COII, COIII, CytB, ATP6, ATP8, lrRNA, srRNA, PCG, rRNA, short, ND2_16S, ND6, ND2_ND5)

###Create a RF distance matrix on the all.genes object and call it topomat###
topomat <- tree.dist.matrix(all.genes)

###View topomat object###
topomat

###Export topomat as .csv###

####Read in csv file of RF distance matrix###
dat <- read.csv("matrix.csv", row.names = 1)

###Plot nj dendrogram###
plot(nj(as.dist(dat)))

####Generating a broken x-axis scatteroplot###

###Load libraries###
library(ggplot2)
library(ggbreak)
library(plotrix)

###Read in the data matrix###
dat <- read.csv("gene_length_vs_RF.csv")

####Generate a broken x-axis scatterplot with the fuction gap.plot###
gap.plot(x = dat$length, y = dat$RF, gap = c(30, 110), gap.axis = "x", brw=0.01, xlab="Gene length (100bp)", ylab="RF distance from mitogenome phylogeny", col = "black", ylim = c(0, 50), xtics=seq(0,115,by=5))

###Estimating Saturation (adapted from https://www.kmeverson.org/blog/simple-dna-saturation-plots-in-r)###

###load library###

library(ape)

###Input data: a phylip-format alignment file, converted to a 'DNAbin' object###
dat<-read.dna(file="ND4.phy", format = "sequential", as.character=TRUE, skip=0)
dat<-as.DNAbin(dat)

###Convert to genetic distances###
dist<-dist.dna(dat, model="raw")
dist.corrected<-dist.dna(dat, model="TN93")

###Make plot###
plot(dist~dist.corrected, pch=20, col="red", xlab="TrN model distance", ylab="Uncorrected genetic distance", main="Saturation Plot")
abline(0,1, lty=2)
abline(lm(dist~dist.corrected), lwd=3)
lm_coef<-coef(lm(dist~dist.corrected))
text(0.1,0.05,bquote(y == .(lm_coef[2])*x))

###Performing an ANOVA on the different mertics explaining topology###

###Read in data matrix###
RF.data <- read.csv("RF.data.csv", header = TRUE, colClasses = c("character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

###Run ANOVA on data matrix specifying variables###
all <- lm(RF_distance ~ Alignment_length * Parsimony_Informative_sites * Saturation, data = RF.data)
t <- anova(all)

###Write table to txt file###
write.table(t, file = "ANOVA.txt", sep = ",", quote = FALSE, row.names = F)
