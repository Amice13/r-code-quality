setwd("..")
source("../FUNCTIONS.R")  
outpath =".."
Rcpp::sourceCpp('../FUNCTIONS_CPP_JGNSC.cpp')
date = gsub("-","_",Sys.Date())
 
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
aictable[which.min(aictable$AIC),]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("../D0004norm")
files <- list.files()
temp <- files[grep("txt", files)]
aictable <- do.call(rbind, lapply(temp, function(x){
  y= t(read.table(x))
  return(y)
}))
aictable <- as.data.frame(aictable)
colnames(aictable) <- c("lambda1","lambda2","AIC")
ggplot(aictable, aes(x=lambda1, y=AIC, color=lambda2)) + geom_point()
aictable[which.min(aictable$AIC),]




# =============================  
JGLNPN <- readRDS("../D0004norm/D0004norm_Lambda_91.rds")

meta2 <- read.csv("../Mammalian_Metabolic_list of enzyme genes_11172020.csv")
partcorr.npn <- lapply(JGLNPN$theta, prec2partialcorr)

test <- Map2Pathways(partcorr.list = partcorr.npn,
                     conditions = c("AC","MES","NPC","OPC"),
                     GeneInterest = "MYC",
                     pathwayRef = meta2,
                     pathwayRef_geneVariable = "GeneSymbol",
                     pathwayRef_pathVariable = "SigmaMiniMap.Term",
                     threshold =0)
test$nconnect
GSEA.table <- test$GSEA.table
connectvalues <- test$x.connect 


write.csv(GSEA.table, paste("../D0004_GSEA_MB_MYC",date,".csv", sep = ""))
write.csv(connectvalues, paste("../D0004_GSEA_MB_glist_MYC",date,".csv", sep = ""))


# workflow: visualize networks
# ================================= 
gene1 = "MYC"
gconnect = c(toupper(test$cond.connect[[1]]$GeneSymbol), gene1)
highlight = gconnect %in% c(gene1)
net1 = plot_onenet(partcorr.npn[[1]][gconnect,gconnect], gname = "AC", circlenet = T, nodecolor = c("orange","lightblue")[highlight+1], family.vec = highlight)
net1

gconnect = c(toupper(test$cond.connect[[2]]$GeneSymbol), gene1)
highlight = gconnect %in% c(gene1)
net2 = plot_onenet(partcorr.npn[[2]][gconnect,gconnect], gname = "MES", circlenet = T, nodecolor = c("orange","lightblue")[highlight+1], family.vec = highlight)
net2

gconnect = c(toupper(test$cond.connect[[3]]$GeneSymbol), gene1)
highlight = gconnect %in% c(gene1)
net3 = plot_onenet(partcorr.npn[[3]][gconnect,gconnect], gname = "NPC", circlenet = T, nodecolor = c("orange","lightblue")[highlight+1], family.vec = highlight)
net3

gconnect = c(toupper(test$cond.connect[[4]]$GeneSymbol), gene1)
highlight = gconnect %in% c(gene1)
net4 = plot_onenet(partcorr.npn[[4]][gconnect,gconnect], gname = "OPC", circlenet = T, nodecolor = c("orange","lightblue")[highlight+1], family.vec = highlight)
net4

allnet <- plot_grid(net1, net2, net3, net4, ncol = 2)

pdf(paste("../D0004_GBM_JointNetworkPlots_",date,".pdf", sep = ""), height = 10, width = 10)
print(allnet)
dev.off()

# ===================
gene1 = "MYC"
gconnect = c(toupper(test$cond.connect[[1]]$GeneSymbol), gene1)
highlight = gconnect %in% c(gene1)
monly <- partcorr.npn[[1]][gconnect,gconnect]
monly[rownames(monly) != "MYC", colnames(monly)!= "MYC"] <- 0
net1 = plot_onenet(monly, gname = "AC", circlenet = T, nodecolor = c("orange","lightblue")[highlight+1], family.vec = highlight)
net1

gconnect = c(toupper(test$cond.connect[[2]]$GeneSymbol), gene1)
highlight = gconnect %in% c(gene1)
monly <- partcorr.npn[[2]][gconnect,gconnect]
monly[rownames(monly) != "MYC", colnames(monly)!= "MYC"] <- 0
net2 = plot_onenet(monly, gname = "MES", circlenet = T, nodecolor = c("orange","lightblue")[highlight+1], family.vec = highlight)
net2

gconnect = c(toupper(test$cond.connect[[3]]$GeneSymbol), gene1)
highlight = gconnect %in% c(gene1)
monly <- partcorr.npn[[3]][gconnect,gconnect]
monly[rownames(monly) != "MYC", colnames(monly)!= "MYC"] <- 0
net3 = plot_onenet(monly, gname = "NPC", circlenet = T, nodecolor = c("orange","lightblue")[highlight+1], family.vec = highlight)
net3

gconnect = c(toupper(test$cond.connect[[4]]$GeneSymbol), gene1)
highlight = gconnect %in% c(gene1)
monly <- partcorr.npn[[4]][gconnect,gconnect]
monly[rownames(monly) != "MYC", colnames(monly)!= "MYC"] <- 0
net4 = plot_onenet(monly, gname = "OPC", circlenet = T, nodecolor = c("orange","lightblue")[highlight+1], family.vec = highlight)
net4


allnet <- plot_grid(net1, net2, net3, net4, ncol = 2)
dev.off()
pdf(paste("../D0004_GBM_JointNetworkPlots_spring_",date,".pdf", sep = ""), height = 10, width = 10)
print(allnet)
dev.off()

