#set wd

multiome <- read.csv("multiome.csv", header = TRUE, sep = ",")

library(WGCNA)

#automatic network construction

datExpr0 <- scale(multiome[,-c(1:11)], scale = TRUE, center = TRUE)
rownames(datExpr0) <- paste("Sub", multiome$subjid, sep = "_")

#check outliers


sampleTree = hclust(dist(datExpr0), method = "average");
# Plot the sample tree: Open a graphic output window of size 12 by 9 inches
# The user should change the dimensions if the window is too large or too small.
sizeGrWindow(12,9)
#pdf(file = "Plots/sampleClustering.pdf", width = 12, height = 9);
par(cex = 0.6);
par(mar = c(0,4,2,0))
plot(sampleTree, main = "Sample clustering to detect outliers", sub="", xlab="", cex.lab = 1.5,
     cex.axis = 1.5, cex.main = 2)

#### no outliers!!!!

datExpr = datExpr0

traitData = multiome[,1:11]

save(datExpr, traitData, file = "WGCNA_input_raw.RData")

#CREATE NETWORK

options(stringsAsFactors = FALSE);
# Allow multi-threading within WGCNA. This helps speed up certain calculations.
# At present this call is necessary for the code to work.
# Any error here may be ignored but you may want to update WGCNA if you see one.
# Caution: skip this line if you run RStudio or other third-party R environments.
# See note above.
enableWGCNAThreads()
# Load the data saved in the first part
lnames = load(file = "WGCNA_input_raw.RData");
#The variable lnames contains the names of loaded variables.
lnames

# Choose a set of soft-thresholding powers
powers = c(c(1:10), seq(from = 12, to=25, by=2))
# Call the network topology analysis function
sft = pickSoftThreshold(datExpr, powerVector = powers, verbose = 5, networkType = "signed")
# Plot the results:
sizeGrWindow(9, 5)
par(mfrow = c(1,2));
cex1 = 0.9;
# Scale-free topology fit index as a function of the soft-thresholding power
plot(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
     xlab="Soft Threshold (power)",ylab="Scale Free Topology Model Fit,signed R^2",type="n",
     main = paste("Scale independence"));
text(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
     labels=powers,cex=cex1,col="red");
# this line corresponds to using an R^2 cut-off of h
abline(h=0.90,col="red")
# Mean connectivity as a function of the soft-thresholding power
plot(sft$fitIndices[,1], sft$fitIndices[,5],
     xlab="Soft Threshold (power)",ylab="Mean Connectivity", type="n",
     main = paste("Mean connectivity"))
text(sft$fitIndices[,1], sft$fitIndices[,5], labels=powers, cex=cex1,col="red")

#choose power = 18 because it is r² >0.8

net = blockwiseModules(datExpr, power = 18,
                       TOMType = "signed", minModuleSize = 5, networkType = "signed", deepSplit = 4,
                       reassignThreshold = 0, mergeCutHeight = 0.1,
                       numericLabels = TRUE, pamRespectsDendro = FALSE,
                       saveTOMs = TRUE,
                       saveTOMFileBase = "kwashmarasTOM",
                       verbose = 3)
table(net$colors)

# open a graphics window
sizeGrWindow(12, 9)
# Convert labels to colors for plotting
mergedColors = labels2colors(net$colors)
# Plot the dendrogram and the module colors underneath
plotDendroAndColors(net$dendrograms[[1]], mergedColors[net$blockGenes[[1]]],
                    "Module colors",
                    dendroLabels = FALSE, hang = 0.03,
                    addGuide = TRUE, guideHang = 0.05)

moduleLabels = net$colors
moduleColors = labels2colors(net$colors)
MEs = net$MEs;
geneTree = net$dendrograms[[1]];
save(MEs, moduleLabels, moduleColors, geneTree,
     file = "kwashmaras-networkConstruction-auto_raw.RData")

#obtain module assignment

module = as.data.frame(net$colors)
module$color = moduleColors
write.csv(module, file = "module_assignment.csv")

#relate modules to outcome

MEs0 = moduleEigengenes(datExpr, moduleColors)$eigengenes

write.csv(MEs0, "module_eigenvalues.csv")
MEs0$subjid <- rownames(MEs0)

head(traitData)

traitData$subjid <- paste("Sub", traitData$subjid, sep = "_")

outcomes = merge(traitData, MEs0, by = "subjid")
write.csv(outcomes, "WGCNA_outcome_analysis.csv")

#### analyse kwash and modules

library(survival)

p = clogit(kwash ~ site + sex + agemons + hiv_results + muac + strata(outcomes$albumine_d0) +
             MEyellow, data = outcomes)
summary(p)
confint(p, method = "Wald")

table(moduleColors)

pvals = c(0.05, 0.107, 0.288, 0.011, 0.05, 0.046, 0.47, 0.049, 0.84, 0.02, 9.90E-05, 0.02, 0.71, 0.87, 0.02, 0.98)
pv = p.adjust(pvals, method = "BH")
pv

library(ggplot2)

boxLabels = c("M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9", "M10", "M11", "M12", "M13", "M14", "M15", "M16")
df <- data.frame(
  yAxis = length(boxLabels):1,
  boxOdds = c(4.87, 4.15, 2.64, 6.63, -5.34, 6.05, -1.74, 5.35, -0.49, 5.57, 13.45, 5.78, 0.95, 0.38, 5.75, 0.05),
  boxCILow = c(-0.13, -0.89, -2.23, 1.53, -10.87, 0.08, -6.55, 0.014, -5.22, 0.63, 6.67, 0.75, -4, -4.41, 0.61, -4.91),
  boxCIHigh = c(9.87, 9.2, 7.5, 11.74, 0.18, 12.02, 3.05, 10.68, 4.23, 10.51, 20.22, 10.81, 5.91, 5.18, 10.88, 5.01
)
)

p <- ggplot(df, aes(x = boxOdds, y = yAxis))
yAxis = length(boxLabels):1
col <- c("grey", "grey", "grey", "red", "grey", "red", "grey", "red", "grey", "red", "red", "red", "grey", "grey", "red", "grey")
p + geom_vline(aes(xintercept = 0), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 4, color = col) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = yAxis, labels = boxLabels) +
  scale_x_continuous() +
  ylab("") +
  xlab("log odds ratio
       Association to Oedematous SM") +
  theme(axis.text=element_text(size=15, face = "bold"),
        axis.title=element_text(size=15,face="bold"))



##draw network

library(zoom)
library(igraph)

adjacency = adjacency(datExpr, power=18, type="signed")
adjacency[adjacency < 0] = 0
adjacency[adjacency > 1] = 1
TOM = TOMsimilarity(adjacency, TOMType="signed")

adj <- TOM
adj[adj > 0.1] = 1
adj[adj != 1] = 0
network <- graph.adjacency(adj)
network <- simplify(network)  # removes self-loops

V(network)$color <- moduleColors
par(mar=c(0,0,0,0))
# remove unconnected nodes
network <- delete.vertices(network, degree(network)==0)
plot(network, layout=layout_with_kk(network), edge.arrow.size = 0.2,
     vertex.label = NA, vertex.size = 5)
zm()

par(mfrow=c(1,1))
dev.off()


#export to cytoscape

options(stringsAsFactors = FALSE)

lnames = load(file = "WGCNA_input_raw.RData")
lnames <- load("kwashmaras-networkConstruction-auto_raw.RData")


table(moduleColors)



# Recalculate topological overlap if needed
TOM = TOMsimilarityFromExpr(datExpr, power = 18, networkType = "signed");
# Read in the annotation file
annot = read.csv(file = "annotation.csv");
# Select modules
modules = c("cyan", "greenyellow", "magenta", "pink", "purple", "red", "turquoise");
# Select module probes
probes = names(data.frame(datExpr))
inModule = is.finite(match(moduleColors, modules));
modProbes = probes[inModule];
modGenes = annot$Annotation[match(modProbes, annot$Feature)];
modTOM = TOM[inModule, inModule];
dimnames(modTOM) = list(modProbes, modProbes)

cyt = exportNetworkToCytoscape(modTOM,
                               edgeFile = paste("CytoscapeInput-edges-", paste(modules, collapse="-"), ".txt", sep=""),
                               nodeFile = paste("CytoscapeInput-nodes-", paste(modules, collapse="-"), ".txt", sep=""),
                               weighted = TRUE,
                               threshold = 0.02,
                               nodeNames = modProbes,
                               altNodeNames = modGenes,
                               nodeAttr = moduleColors[inModule])

cyt
