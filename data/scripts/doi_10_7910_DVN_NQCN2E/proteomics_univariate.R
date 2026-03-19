
AFprot <- read.csv("AF_dataset_proteomics.csv", header = T, sep = ",")

prot <- scale(AFprot[,c(12:198)], scale = TRUE, center = TRUE)
AF <- cbind(AFprot[,1:11], prot)

library(survival)
GLM.run<-function(y) {
  form <- as.formula(paste0("kwash ~ agemons + sex + hiv_results + site + muac + strata(albumine_d0) +", y))
  fit<-(clogit(form, data=AF))}

GLMs.out <- lapply(colnames(AF[,c(12:198)]),GLM.run )
results<-lapply(GLMs.out, function(x){summary(x)})
results

#extracting pValues and FDR correction
results<-lapply(GLMs.out, function(x){coef(summary(x))[8,5]})
pVal = as.matrix(results)
rownames(pVal) = colnames(AF[12:198])
fdr = as.matrix(p.adjust(as.vector(results), method ="fdr", n=187)) #n=number of metabolites
pVal=cbind(pVal, fdr)
colnames(pVal) = c("pVal", "fdr p")
pVal <- as.matrix(pVal)
View(as.data.frame(pVal))

#extracting estimate
results_est<-lapply(GLMs.out, function(x){coef(summary(x))[8,"exp(coef)"]}) #[x,4] change x to number of factors + 1
estimate = as.matrix(results_est)
rownames(estimate) = colnames(AF[12:198])
View(as.data.frame(estimate))

#extracting std error
results_se<-lapply(GLMs.out, function(x){coef(summary(x))[8,"se(coef)"]}) #[x,4] change x to number of factors + 1
stderror = as.matrix(results_se)
rownames(stderror) = colnames(AF[12:198])
View(as.data.frame(stderror))

#compile statistics table
uni_stat <- cbind(estimate, stderror, pVal)
colnames(uni_stat) <- c("exp(estimate)", "std error", "pVal", "FDRpVal")
View(as.data.frame(uni_stat))
write.csv(uni_stat, file = "univariate_analysis_AF_prot_clogit_matchstrataMUAC.csv")

#volcano plot

volc <- read.csv("univariate_analysis_AF_prot_clogit_matchstrata.csv", header = T, sep = ",")

with(volc, plot(log(exp.estimate.), -log10(FDRpVal), pch=20, col = "grey", main="Proteomics", xlim=c(-5,5), xlab = "Log odds"))
# Add colored points: red if padj<0.05, orange of log2FC>1, green if both)
with(subset(volc, FDRpVal<.05 ), points(log(exp.estimate.), -log10(FDRpVal), pch=20, col="red"))
#with(subset(foldchange, abs(logFC)>1.5), points(logFC, -log10(FDRpVal), pch=20, col="orange"))
#with(subset(foldchange, FDRpVal<.05 & abs(logFC)>1.5), points(logFC, -log10(FDRpVal), pch=20, col="red"))
abline(h = 1.3, v = 0, col = "grey", lty = 2)
abline(h = 2, col = "grey", lty = 2)

#plot lumican

AFprot$oedema.num = as.factor(revalue(AFprot$oedema, c("None" = "0", "+" = "1", "++" = "2", "+++" = "3")))

p <- ggplot(AFprot, aes(x=as.factor(oedema.num), y= log(P19823), color = as.factor(oedema.num))) + geom_boxplot()
p + geom_jitter(shape=16, position=position_jitter(0.2), size = 1) + xlab("Oedema") + ylab ("log Abundance") + 
  ggtitle("ITIH2") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                            panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none")
#change figure dimensions to 200 x 250


library(MASS)

ol = polr(as.factor(oedema.num) ~ P04196 + albumine_d0 + hiv_results + agemons + died + sex + site, data=AFprot, Hess = TRUE)
summary(ol)
(ctable <- coef(summary(ol)))
pval <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = pval))




#combine proteomics and metabolomics in 1 volcano plot

volc <- read.csv("univariate_analysis_AF_METPROT_clogit_matchstrata.csv", header = T, sep = ",")

with(volc, plot(log(exp.estimate.), -log10(FDRpVal), pch=20, cex = 2, col = "grey", xlim=c(-2,2), xlab = "Log odds"))
# Add colored points: red if padj<0.05, orange of log2FC>1, green if both)
with(subset(volc, FDRpVal<.049 ), points(log(exp.estimate.), -log10(FDRpVal), pch=20, cex = 2, col="red"))
#with(subset(foldchange, abs(logFC)>1.5), points(logFC, -log10(FDRpVal), pch=20, col="orange"))
#with(subset(foldchange, FDRpVal<.05 & abs(logFC)>1.5), points(logFC, -log10(FDRpVal), pch=20, col="red"))
abline(h = 1.3, v = 0, col = "grey", lty = 2)
abline(h = 2, col = "grey", lty = 2)

