rm(list = ls())  
options(stringsAsFactors = F)
load("step_2_output.RData")
library(ggplot2)
library(WGCNA)
dat=exp
library(tidyverse)
apply(dat, 1, sd) %>% sort(.,decreasing = T) %>% head(.,4000) %>% names %>% dat[.,] -> dat
powers = c(c(1:10), seq(from = 12, to=20, by=2))
sft = pickSoftThreshold(dat0, 
                        powerVector = powers, 
                        verbose = 5)
po <- sft$powerEstimate;po
# Plot the results:
sizeGrWindow(9, 5)
par(mfrow = c(1,2));
cex1 = 0.8;
# Scale-free topology fit index as a function of the soft-thresholding power
plot(sft$fitIndices[,1], 
     -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
     xlab="Soft Threshold (power)",
     ylab="Scale Free Topology Model Fit,signed R^2",
     type="n",
     main = paste("Scale independence"));
text(sft$fitIndices[,1], 
     -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
     labels=powers,cex=cex1,col="red");

# this line corresponds to using an R^2 cut-off of h
abline(h=0.80,col="red")

# Mean connectivity as a function of the soft-thresholding power
plot(sft$fitIndices[,1], 
     sft$fitIndices[,5],
     xlab="Soft Threshold (power)",
     ylab="Mean Connectivity", 
     type="n",main = paste("Mean connectivity"))
text(sft$fitIndices[,1], 
     sft$fitIndices[,5], 
     labels=powers, cex=cex1,col="red")

net = blockwiseModules(dat0, power = 4,
                       TOMType = "unsigned", minModuleSize = 30,
                       reassignThreshold = 0, mergeCutHeight = 0.25,
                       numericLabels = TRUE, pamRespectsDendro = FALSE,
                       saveTOMs = F, 
                       verbose = 3) 

class(net)
names(net)
table(net$colors)
moduleLabels = net$colors
moduleColors = labels2colors(net$colors)
MEs = net$MEs;
geneTree = net$dendrograms[[1]];
save(net,MEs, moduleLabels, moduleColors, geneTree, 
     file = "Step3networkConstruction-auto.RData")

gene_hclust_Tree = hclust(dist( dat ), method = "average");
ht=cutree(gene_hclust_Tree,100)
table(ht)
table(net$colors)
identical(names(ht),
          names(net$colors))
df = data.frame(hc =  ht ,
                wgcna =  net$colors )
head(df)
kp = df$hc %in% names(table(ht))[table(ht) > 10]
df=df[kp,]
gplots::balloonplot(table(df))
# open a graphics window
sizeGrWindow(12, 9)

# Convert labels to colors for plotting
mergedColors = labels2colors(net$colors)
# Plot the dendrogram and the module colors underneath
plotDendroAndColors(geneTree, 
                    mergedColors[net$blockGenes[[1]]],
                    "Module colors",
                    dendroLabels = FALSE, 
                    hang = 0.03,
                    addGuide = TRUE, 
                    guideHang = 0.05)
factor(group,levels=c("AMI","UA","Control"))->group
design=model.matrix(~0+ group)
colnames(design)=levels(group)
design1 <- as.data.frame(design)
moduleTraitCor = cor(MEs, design, use = "p")

head(moduleTraitCor)
moduleTraitPvalue = corPvalueStudent(moduleTraitCor, 
                                     ncol(dat))
head(moduleTraitPvalue)
textMatrix =  paste(signif(moduleTraitCor, 2), 
                    "\n(",signif(moduleTraitPvalue, 1),
                    ")", 
                    sep = "")
dim(textMatrix) = dim(moduleTraitCor)
library(stringr)
par(mar = c(6, 8.5, 3, 3));
labeledHeatmap(Matrix = moduleTraitCor,
               xLabels = names(design1),
               yLabels = names(MEs),
               ySymbols = names(MEs),
               colorLabels = FALSE,
               colors = blueWhiteRed(50),
               textMatrix = textMatrix,
               setStdMargins = FALSE,
               cex.text = 0.5,
               zlim = c(-1,1),
               main = paste("Module-trait relationships"))
