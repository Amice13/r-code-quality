setwd("") ## set working directory to preferred location

install.packages("foreign")
install.packages("tidyverse")
install.packages("readxl")
install.packages("plotrix")
install.packages("shape")
install.packages("ggplot2")
library(foreign)
library(tidyverse)
library(readxl)
library(plotrix)
library(shape)
library(ggplot2)


##load data, establish upper and lower confidence intervals
mt<- read_excel("Figure2Data.xlsx")
summary(mt)
d<-mt$D
cil<-mt$LI
ciu<-mt$UI




Ylabel <- paste(mt$Emotion, ": ", mt$Method, sep = "") #merging emotion and method for labels


##Figure 2 Code

png("MetaFigure2.png", width = 9, height = 11, unit = "in", res = 300)
par(mar = c(4, .5, .5, .5))
plot(d, 1:length(d), xlim = c(-.52, .52),
     pch = 20, yaxt = "n", ylab = "", bty =  "n",
     xlab = "Unadjusted Effect Size (D)", cex.lab = .8)

for(i in 1:length(d)){
  lines(c(cil[i], ciu[i]), c(i,i), lwd = 1.75)
}
for (i in 1:length(d)) {
  Ylabel <- paste(mt$Emotion[i], ": ", mt$Method[i], sep = "") #merging emotion and method for labels
  text(x=-.54, y = i, Ylabel, xpd=TRUE, adj = 0, cex = .85) #left justifying labels, adj = 0 is left justified, xpd past plot boundaries
}

abline(h = c(6.5, 15.5, 24.5, 30.5, 36.5), col = "gray")
abline(h= c(2.5, 4.5, 9.5, 12.5, 18.5, 21.5, 26.5, 28.5, 32.5, 34.5, 38.5,40.5), col = "gray")
abline(v=0, col="#b2182b",lty=5)
points(d, 1:length(d), bg='gray80',col="black",lwd=1.75,cex=1, pch=21)

dev.off()


