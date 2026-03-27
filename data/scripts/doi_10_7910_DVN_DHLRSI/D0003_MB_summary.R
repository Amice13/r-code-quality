setwd("..")
source("../FUNCTIONS.R")  
outpath =".."
Rcpp::sourceCpp('../FUNCTIONS_CPP_JGNSC.cpp')
date = gsub("-","_",Sys.Date())

# SIMULATION
library(pheatmap)
library(umap)
library(huge)
library(JGL)
library(PRROC)

files <- list.files()
temp <- files[grep("txt", files)]
aictable <- do.call(rbind, lapply(temp, function(x){
  y= t(read.table(x))
  return(y)
}))
aictable <- as.data.frame(aictable)
colnames(aictable) <- c("lambda1","lambda2","AIC")
ggplot(aictable, aes(x=lambda1, y=AIC, color=lambda2)) + geom_point()
ggplot(aictable, aes(x=lambda2, y=AIC, color=lambda1)) + geom_point()
aictable[which.min(aictable$AIC),]

# ============================= 
JGLNPN <- readRDS("../D0003norm/D0003norm_Lambda_91.rds") 
meta2 <- read.csv("../Mammalian_Metabolic_list of enzyme genes_11172020.csv")
partcorr.npn <- lapply(JGLNPN$theta, prec2partialcorr)

test <- Map2Pathways(partcorr.list = partcorr.npn,
                     conditions = c("Group 3","Intermediate","Group 4"),
                     GeneInterest = "MYC",
                     pathwayRef = meta2,
                     pathwayRef_geneVariable = "GeneSymbol",
                     pathwayRef_pathVariable = "SigmaMiniMap.Term",
                     threshold =0)
GSEA.table <- test$GSEA.table
connectvalues <- test$x.connect

write.csv(GSEA.table, paste("../D0003_GSEA_MB_MYC",date,".csv", sep = ""))
write.csv(connectvalues, paste("../D0003_GSEA_MB_glist_MYC",date,".csv", sep = ""))


# =================================
# workflow: visualize networks
# =================================
test <- Map2Pathways(partcorr.list = partcorr.npn,
                     conditions = c("Group 3","Intermediate","Group 4"),
                     GeneInterest = "MYC",
                     pathwayRef = meta2,
                     pathwayRef_geneVariable = "GeneSymbol",
                     pathwayRef_pathVariable = "SigmaMiniMap.Term",
                     threshold =0.005)
GSEA.table <- test$GSEA.table 
test$nconnect

gene1 = "MYC"
gconnect = c(toupper(test$cond.connect[[1]]$GeneSymbol), gene1)
highlight = gconnect %in% c(gene1)
net1 = plot_onenet(partcorr.npn[[1]][gconnect,gconnect], gname = "Group 3", circlenet = T, nodecolor = c("orange","lightblue")[highlight+1], family.vec = highlight)
net1

gconnect = c(toupper(test$cond.connect[[2]]$GeneSymbol), gene1)
highlight = gconnect %in% c(gene1)
net2 = plot_onenet(partcorr.npn[[2]][gconnect,gconnect], gname = "Intermediate", circlenet = T, nodecolor = c("orange","lightblue")[highlight+1], family.vec = highlight)
net2

gconnect = c(toupper(test$cond.connect[[3]]$GeneSymbol), gene1)
highlight = gconnect %in% c(gene1)
net3 = plot_onenet(partcorr.npn[[3]][gconnect,gconnect], gname = "Group 4", circlenet = T, nodecolor = c("orange","lightblue")[highlight+1], family.vec = highlight)
net3

allnet <- plot_grid(net1, net2, net3, ncol = 3)
allnet <- plot_grid(net1, net2, ncol = 2)

pdf(paste("../B0003v4_MB_JointNetworkPlots",date,".pdf"), height = 4, width = 8)
print(allnet)
dev.off()
pdf(paste("../B0003v4_MB_JointNetworkPlots",date,".pdf"), height = 4, width = 12)
print(allnet)
dev.off()




gene1 = "MYC"
gconnect = c(toupper(test$cond.connect[[1]]$GeneSymbol), gene1)
highlight = gconnect %in% c(gene1)
monly <- partcorr.npn[[1]][gconnect,gconnect]
monly[rownames(monly) != "MYC", colnames(monly)!= "MYC"] <- 0

net1 = plot_onenet(monly, gname = "Group 3", circlenet = T, nodecolor = c("orange","lightblue")[highlight+1], family.vec = highlight)
net1

gconnect = c(toupper(test$cond.connect[[2]]$GeneSymbol), gene1)
highlight = gconnect %in% c(gene1)
monly <- partcorr.npn[[2]][gconnect,gconnect]
monly[rownames(monly) != "MYC", colnames(monly)!= "MYC"] <- 0
net2 = plot_onenet(partcorr.npn[[2]][gconnect,gconnect], gname = "Intermediate", circlenet = T, nodecolor = c("orange","lightblue")[highlight+1], family.vec = highlight)
net2

allnet <- plot_grid(net1, net2, ncol = 2)

pdf(paste("../B0003v4_MB_JointNetworkPlots_spring",date,".pdf"), height = 5, width = 10)
print(allnet)
dev.off()



# ------------------------
# OTX2
OTX2table <- Map2Pathways(partcorr.list = partcorr.npn,
                          conditions = c("Group 3","Intermediate","Group 4"),
                          GeneInterest = "OTX2",
                          pathwayRef = meta2,
                          pathwayRef_geneVariable = "GeneSymbol",
                          pathwayRef_pathVariable = "SigmaMiniMap.Term",
                          threshold =0)
GSEA.table <- OTX2table$GSEA.table 
OTX2table$nconnect
write.csv(GSEA.table, paste("../B0003_GSEA_MB_OTX2_",date,".csv", sep = ""))

gene1 = "OTX2"
gconnect = c(toupper(OTX2table$cond.connect[[1]]$GeneSymbol), gene1)
highlight = gconnect %in% c(gene1)
net1 = plot_onenet(partcorr.npn[[1]][gconnect,gconnect], gname = "Group 3", circlenet = T, nodecolor = c("orange","lightblue")[highlight+1], family.vec = highlight)
net1

gconnect = c(toupper(OTX2table$cond.connect[[2]]$GeneSymbol), gene1)
highlight = gconnect %in% c(gene1)
net2 = plot_onenet(partcorr.npn[[2]][gconnect,gconnect], gname = "Intermediate", circlenet = T, nodecolor = c("orange","lightblue")[highlight+1], family.vec = highlight)
net2

gconnect = c(toupper(OTX2table$cond.connect[[3]]$GeneSymbol), gene1)
highlight = gconnect %in% c(gene1)
net3 = plot_onenet(partcorr.npn[[3]][gconnect,gconnect], gname = "Group 4", circlenet = T, nodecolor = c("orange","lightblue")[highlight+1], family.vec = highlight)
net3

dev.off()
allnet <- plot_grid(net1, net2, net3, ncol = 3)

pdf(paste("../B0003_MB_JointNetworkPlots_OTX2_",date,".pdf", sep = ""), height = 4, width = 12)
print(allnet)
dev.off()


# ===================
gene1 = "OTX2"
gconnect = c(toupper(OTX2table$cond.connect[[1]]$GeneSymbol), gene1)
highlight = gconnect %in% c(gene1)
monly <- partcorr.npn[[1]][gconnect,gconnect]
monly[rownames(monly) != gene1, colnames(monly)!= gene1] <- 0
net1 = plot_onenet(monly, gname = "Group 3", circlenet = T, nodecolor = c("orange","lightblue")[highlight+1], family.vec = highlight)
net1

gconnect = c(toupper(OTX2table$cond.connect[[2]]$GeneSymbol), gene1)
highlight = gconnect %in% c(gene1)
monly <- partcorr.npn[[2]][gconnect,gconnect]
monly[rownames(monly) != gene1, colnames(monly)!= gene1] <- 0
net2 = plot_onenet(monly, gname = "Intermediate", circlenet = T, nodecolor = c("orange","lightblue")[highlight+1], family.vec = highlight)
net2

gconnect = c(toupper(OTX2table$cond.connect[[3]]$GeneSymbol), gene1)
highlight = gconnect %in% c(gene1)
monly <- partcorr.npn[[3]][gconnect,gconnect]
monly[rownames(monly) != gene1, colnames(monly)!= gene1] <- 0
net3 = plot_onenet(monly, gname = "Group 4", circlenet = T, nodecolor = c("orange","lightblue")[highlight+1], family.vec = highlight)
net3

dev.off()
allnet <- plot_grid(net1, net2, net3, ncol = 3)

pdf(paste("../B0003_MB_JointNetworkPlots_OTX2_spring",date,".pdf", sep = ""), height = 4, width = 12)
print(allnet)
dev.off()