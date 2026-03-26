############################## FULL DATA OUTPUT ################################
# FRONT MATTER
## Clean up
#rm(list=ls())

# PRELIMINARIES
## Libraries
library(krige); library(xtable)

## Load results
load("results/krige.fit1.RData")
load("results/krige.fit2.RData")
predictive <- read.csv("results/predictions.csv", header = TRUE)
load("data/sppq-results.Rdata") # Previous results using BRSS

# TABLES
## Table 6
### Rescale easting and northing terms to megameters
fullData.fit.summary <- summary(fullData.fit2, 5000)
fullData.tab <- fullData.fit.summary$estimates[, c(1,5,8)]
fullData.tab[22:23,]<-1000*fullData.tab[22:23,]
fullData.tab[24:26,]<-1000000*fullData.tab[24:26,]
row.names(fullData.tab) <- NULL
colnames(fullData.tab) <- NULL

### Write out xtable to file
print(xtable(cbind(mcmc.tab, fullData.tab[c(4:26,1:3),]), digits = 4),
      include.colnames = FALSE, sanitize.text.function=function(x){x},
      file = "output/tab/fullData-results.txt")
writeLines(readLines("output/tab/fullData-results.txt")[-c(1:7, 34:36)], 
           "output/tab/fullData-results.txt")

## Table 7
geweke.out <- geweke(fullData.fit2)
hw.out <- heidel.welch(fullData.fit2)
diag.tab <- t(rbind(geweke.out, hw.out))
colnames(diag.tab)[c(1,3)] <- c("Geweke", "Cram\\'{e}r-von Mises")
rownames(diag.tab) <- rownames(mcmc.tab)[c(24:26,1:23)]
print(xtable(diag.tab, caption = "\\label{tab:fullData_diag} Convergence diagnostics for update run of full data. 
             First a Geweke diagnostic, and second the Cram\\'\\{e\\}r-von Mises 
             test of Heidelberger-Welch diagnostic. Statistics computed with \\pkg{krige} 0.6.1
             in R 4.0.2"), size = "small", digits = 4, table.placement = "h!",
      sanitize.text.function=function(x){x}, file = "output/tab/fullData-diag.txt")

# FIGURES
## Figure 3
pdf("output/fig/semivariogram_fullData.pdf", width = 7, height = 5.5)
semivariogram(fullData.fit2)
dev.off()

## Figure 3
### Attach data from the package
#data("lowerCombined")

### Create state legislative district scores
predictive$SLDLA<-as.character(predictive$SLDLA)
lowerKriging<-aggregate(x=predictive$pred.ideol, by=list(STATEA=predictive$STATEA, lower=predictive$SLDLA), FUN=mean)
colnames(lowerKriging)[3]<-'new.ideol'

### Merge-in legislator ideal points
lowerNew<-merge(x=lowerCombined,y=lowerKriging,all.x=T,by=c("STATEA","lower"))

### Compare with old values
#### Correlations
cor(lowerNew$krige.lower,lowerNew$new.ideol) # 0.9542600219
#### Std. error
sqrt((1-cor(lowerNew$krige.lower,lowerNew$new.ideol))/(length(lowerNew$new.ideol)-2)) #0.002898604
#plot(x=lowerNew$krige.lower,y=lowerNew$new.ideol)

### OLD DATA: observe association with ideal points
#postscript('lowerChambersOLD.eps',width=5,height=5)
pdf('output/fig/lowerChambersOLD.pdf',width=5,height=5)
plot(y=lowerCombined$np_score,x=lowerCombined$krige.lower, xlim = c(25,70),
     xlab="District Ideology", ylab="Legislator Ideology (Shor & McCarty)", 
     main="BRSS Measure", type="n")#
points(y=lowerCombined$np_score[lowerCombined$party=="R"],x=lowerCombined$krige.lower[lowerCombined$party=="R"],pch=".",col="red")
points(y=lowerCombined$np_score[lowerCombined$party=="D"],x=lowerCombined$krige.lower[lowerCombined$party=="D"],pch=".",col="blue")
abline(lm(lowerCombined$np_score~lowerCombined$krige.lower),col='gray20')
dev.off()
#### Correlations
cor(lowerCombined$np_score,lowerCombined$krige.lower,use="complete.obs") #0.4239246
#### Std. error
sqrt((1-cor(lowerCombined$np_score,lowerCombined$krige.lower,use="complete.obs"))/(length(!is.na(lowerCombined$np_score))-2)) #0.01028681
#summary(lm(lowerCombined$np_score~lowerCombined$krige.lower))

### NEW DATA: observe association with ideal points
#postscript('lowerChambersNEW.eps',width=5,height=5)
pdf('output/fig/lowerChambersNEW.pdf',width=5,height=5)
plot(y=lowerNew$np_score,x=lowerNew$new.ideol,xlab="District Ideology", ylab="Legislator Ideology (Shor & McCarty)", main="Full Information Measure", type="n")#
points(y=lowerNew$np_score[lowerNew$party=="R"],x=lowerNew$new.ideol[lowerNew$party=="R"],pch=".",col="red")
points(y=lowerNew$np_score[lowerNew$party=="D"],x=lowerNew$new.ideol[lowerNew$party=="D"],pch=".",col="blue")
abline(lm(lowerNew$np_score~lowerNew$new.ideol),col='gray20')
dev.off()
#### Correlations
cor(lowerNew$np_score,lowerNew$new.ideol,use="complete.obs") #0.4243208191
#### Std. error
sqrt((1-cor(lowerNew$np_score,lowerNew$new.ideol,use="complete.obs"))/(length(!is.na(lowerCombined$np_score))-2)) #0.01028327
#summary(lm(lowerNew$np_score~lowerNew$new.ideol))
