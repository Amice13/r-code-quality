
MEs0 <- read.csv("module_eigenvalues.csv", header = T, sep = ",")

library(Hmisc)

mydata <- MEs0[,-1]
colnames(mydata) <- c("M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9", "M10", "M11", "M12", "M13", "M14", "M15", "M16")
mydata.cor = cor(mydata)
mydata.rcorr = rcorr(as.matrix(mydata))
mydata.rcorr

library(corrplot)
corrplot(mydata.cor, type = "lower", order = "hclust", tl.col = "black", tl.srt = 45)
corrplot(mydata.rcorr$r, type="lower", tl.srt = 45, order = "hclust", p.mat = mydata.rcorr$P, 
         tl.cex = 0.55, sig.level = 0.05, insig = "blank", method = "square")
corrplot(mydata.rcorr$r, type="lower", tl.srt = 45, order = "hclust", p.mat = mydata.rcorr$P, 
         tl.pos = "n", sig.level = 0.05, insig = "blank", method = "square")

col<- colorRampPalette(c("red", "white", "blue"))(20)
heatmap(x = mydata.rcorr$r, col = col, symm = TRUE, hclustfun = hclust)
