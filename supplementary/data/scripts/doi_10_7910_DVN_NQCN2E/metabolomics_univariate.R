

AF <- read.csv("a_metabolomics_preprocessed.csv", header = T, sep = ",")

mets <- scale(AF[,c(10:164)], scale = TRUE, center = TRUE)
AF <- cbind(AF[,1:9], mets)

library(survival)

GLM.run<-function(y) {
  form <- as.formula(paste0("oedema ~ age + sex + hiv_results + site + strata(albumine_d0) +", y))
  fit<-(clogit(form, data=AF))}

GLMs.out <- lapply(colnames(AF[,c(10:164)]),GLM.run )
results<-lapply(GLMs.out, function(x){summary(x)})
results

#extracting pValues and FDR correction
results<-lapply(GLMs.out, function(x){coef(summary(x))[7,5]})
pVal = as.matrix(results)
rownames(pVal) = colnames(AF[10:164])
fdr = as.matrix(p.adjust(as.vector(results), method ="fdr", n=155)) #n=number of metabolites
pVal=cbind(pVal, fdr)
colnames(pVal) = c("pVal", "fdr p")
pVal <- as.matrix(pVal)
#View(as.data.frame(pVal))

#extracting estimate
results_est<-lapply(GLMs.out, function(x){coef(summary(x))[7,"exp(coef)"]}) #[x,4] change x to number of factors
estimate = as.matrix(results_est)
rownames(estimate) = colnames(AF[10:164])
#View(as.data.frame(estimate))

#extracting std error
results_se<-lapply(GLMs.out, function(x){coef(summary(x))[7,"se(coef)"]}) #[x,4] change x to number of factors
stderror = as.matrix(results_se)
rownames(stderror) = colnames(AF[10:164])
#View(as.data.frame(stderror))

#compile statistics table
uni_stat <- cbind(estimate, stderror, pVal)
colnames(uni_stat) <- c("exp(estimate)", "std error", "pVal", "FDRpVal")
#View(as.data.frame(uni_stat))
write.csv(uni_stat, file = "univariate_analysis_AF_clogit_metabolomics_matchstrata.csv")

#plot volcano plot

volc <- read.csv("univariate_analysis_AF_clogit_metabolomics_matchstrata.csv", header = T, sep = ",")

with(volc, plot(log(exp.estimate.), -log10(FDRpVal), pch=20, col = "grey", main="Metabolomics", xlim=c(-5,5), xlab = "Log odds"))
# Add colored points: red if padj<0.05, orange of log2FC>1, green if both)
with(subset(volc, FDRpVal<.05 ), points(log(exp.estimate.), -log10(FDRpVal), pch=20, col="red"))
#with(subset(foldchange, abs(logFC)>1.5), points(logFC, -log10(FDRpVal), pch=20, col="orange"))
#with(subset(foldchange, FDRpVal<.05 & abs(logFC)>1.5), points(logFC, -log10(FDRpVal), pch=20, col="red"))
abline(h = 1.3, v = 0, col = "grey", lty = 2)
abline(h = 2, col = "grey", lty = 2)

#plots
library(ggplot2)

p <- ggplot(AF, aes(x=as.factor(oedema), y= SL_SM.C18.1_daily0, color = as.factor(oedema))) + geom_boxplot()
p + geom_jitter(shape=16, position=position_jitter(0.2), size = 3) + xlab("Oedema") + ylab ("Abundance") + ggtitle("SM.C18.1")

p <- ggplot(AF, aes(x=as.factor(oedema.num), y= log(GP_lysoPC.a.C18.0_daily0), color = as.factor(oedema.num))) + geom_boxplot()
p + geom_jitter(shape=16, position=position_jitter(0.2), size = 1) + xlab("Oedema") + ylab ("log Abundance") + 
  ggtitle("lysoPC a C18:0") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none")
#change figure dimensions to 200 x 250

#methionine and cysteine

p <- ggplot(AF, aes(x=as.factor(oedema), y= log(AF$AA_Met_daily0), color = as.factor(oedema))) + geom_boxplot()
p + geom_jitter(shape=16, position=position_jitter(0.2), size = 3) + xlab("Oedema") + ylab ("log Abundance") + 
  ggtitle("Methionine") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none")



#ordinal logistic

library(MASS)

ol = polr(as.factor(oedema.num) ~ AA_Phe_daily0 + albumine_d0 + hiv_results + age + died + sex + site, data=AF, Hess = TRUE)
summary(ol)
(ctable <- coef(summary(ol)))
pval <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = pval))


#HIAA investigation

AF$KT <- AF$BA_Kynurenine_daily0 / AF$AA_Trp_daily0 #kynurenine:trp
boxplot(KT ~ oedema, data = AF)

AF$ST <- AF$BA_Serotonin_daily0 / AF$AA_Trp_daily0 #serotonin:trp
boxplot(ST ~ oedema, data = AF)

AF$HIIAT <- AF$OA_5.Hydroxyindole.acetic.acid_daily0 / AF$AA_Trp_daily0 #serotonin:trp
boxplot(HIIAT ~ oedema, data = AF)

w <- clogit(oedema ~ age + sex + hiv_results + site + strata(albumine_d0) + HIIAT, data = AF)
summary(w)


p <- ggplot(AF, aes(x=as.factor(oedema), y= log(OA_5.Hydroxyindole.acetic.acid_daily0), color = as.factor(oedema))) + geom_boxplot()
p + geom_jitter(shape=16, position=position_jitter(0.2), cex = 1) + xlab("Oedema") + ylab ("Abundance") + ggtitle("5HIAA") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none")
#change figure dimensions to 200 x 250




#adjust for muac

AF1 <- merge(multiome[,c("subjid", "muac")], AF, by = "subjid")

mets <- scale(AF1[,c(11:165)], scale = TRUE, center = TRUE)
AF <- cbind(AF1[,1:10], mets)

library(survival)

GLM.run<-function(y) {
  form <- as.formula(paste0("oedema ~ age + sex + hiv_results + muac + site + strata(albumine_d0) +", y))
  fit<-(clogit(form, data=AF))}

GLMs.out <- lapply(colnames(AF[,c(11:165)]),GLM.run )
results<-lapply(GLMs.out, function(x){summary(x)})
results

#extracting pValues and FDR correction
results<-lapply(GLMs.out, function(x){coef(summary(x))[8,5]})
pVal = as.matrix(results)
rownames(pVal) = colnames(AF[11:165])
fdr = as.matrix(p.adjust(as.vector(results), method ="fdr", n=155)) #n=number of metabolites
pVal=cbind(pVal, fdr)
colnames(pVal) = c("pVal", "fdr p")
pVal <- as.matrix(pVal)
#View(as.data.frame(pVal))

#extracting estimate
results_est<-lapply(GLMs.out, function(x){coef(summary(x))[8,"exp(coef)"]}) #[x,4] change x to number of factors
estimate = as.matrix(results_est)
rownames(estimate) = colnames(AF[11:165])
#View(as.data.frame(estimate))

#extracting std error
results_se<-lapply(GLMs.out, function(x){coef(summary(x))[8,"se(coef)"]}) #[x,4] change x to number of factors
stderror = as.matrix(results_se)
rownames(stderror) = colnames(AF[11:165])
#View(as.data.frame(stderror))

#compile statistics table
uni_stat <- cbind(estimate, stderror, pVal)
colnames(uni_stat) <- c("exp(estimate)", "std error", "pVal", "FDRpVal")
#View(as.data.frame(uni_stat))
write.csv(uni_stat, file = "univariate_analysis_AF_clogit_metabolomics_matchstrataMUAC.csv")
