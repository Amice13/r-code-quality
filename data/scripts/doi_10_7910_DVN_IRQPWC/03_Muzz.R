load(file="step_2_output.RData")
DEGs_exp= exp
time <- data.frame(colnames(exp), group)
tmp <- data.frame(colnames(DEGs_exp),t(DEGs_exp))
temp <- data.frame(time[match(tmp[,1],time[,1]),],tmp)
temp[1:5,1:4]
DEGs_exp_averp <- t(limma::avereps(temp[,-c(1:3)],ID=temp[,2]))
head(DEGs_exp_averp)
#BiocManager::install("Mfuzz")
library(Mfuzz)
eset <- new("ExpressionSet",exprs = DEGs_exp_averp)
eset <- filter.std(eset,min.std=0)
eset <- standardise(eset)
c <- 8 
m <- mestimate(eset) 
cl <- mfuzz(eset, c = c, m = m) 
cl$size 

cl$cluster[cl$cluster == 1] 
cl$membership 
Cluster1 = names(cl$cluster[cl$cluster == 1])
save("cl","Cluster1",file = "07_Muzz.Rdata")
library(RColorBrewer)
color.2 <- colorRampPalette(rev(c("#ff0000", "Yellow", "OliveDrab1")))(1000)
mfuzz.plot(eset,cl,mfrow=c(2,4),new.window= FALSE,time.labels=colnames(DEGs_exp_averp),colo = color.2)
cl.thres <- acore(eset,cl,min.acore=0.5) 
unlist(lapply(cl.thres, nrow))
lapply(cl.thres, head)[[1]]
