#### 12-09_25_pvalue_plotCIE.r ####
### Load libraries
library(MASS)
library(foreign)
library(Matching)
library(xtable)
library(snow)
library(stats)
library(arm)
library(car)
library(np)
library(KernSmooth)
library(boot)
library(sandwich)
library(coda)
library(rbounds)
library(gdata)
library(gtools)
library(lme4)
library(sfsmisc)
library(coin)
library(compiler)
library(coxme)
library(beanplot)
library(RItools)


### Define directories
working.dir <- "/Users/Allan/Dropbox/!!Papers/Liberal Peace/12-02-21_ISQ_commentary/DOR_ISQ_2013_Replication/M_Rep"

getwd()
setwd(working.dir)


data1 <- read.dta("pvalues.dta")
data2 <- read.dta("pvaluesbdm.dta")
data3 <- read.dta("pvaluesh10dm.dta")


#Dropping Specifications without CIEl
data1 <- data1[is.na(data1$CIElpvalues)==FALSE,]

data2 <- data2[is.na(data2$CIElpvalues)==FALSE,]

data3 <- data3[is.na(data3$CIElpvalues)==FALSE,]

data <- rbind(data1[,columns],data2[,columns],data3[,columns] )


columns <- c("specification", "CIElCoefficient", "CIElpvalues")
names(data1[,c(1,6,9)])

#Correcting the labeling
data$specification <- gsub("'''", "''", data$specification)

#Removing Models with Misspecified DV, no point to graph them. 
data <- data[5:length(data[,1]),]

which(data$specification=="FOIDL")-(which(data$specification=="")-1)

which(data$specification=="FOIDL'")-(which(data$specification=="'")-1)
which(data$specification=="FOIDL''")-(which(data$specification=="''")-1)

16*3


library(ggplot2)

labels=data$specification

coef <- data$CIElCoefficient

Category = 
  #c(rep("Mousseau Misspecified DV Models", which(data$specification=="")-1),
  c(rep("S. Imputed Models LifeDensity", which(data$specification=="")-1),
    rep("S. Imputed Models LifeDensity",  which(data$specification=="FOID") -(which(data$specification=="")-1)),
    rep("M. Imputed Models LifeDensity",  which(data$specification=="FOIDM") -(which(data$specification=="FOID"))),
     rep("M. Imputed Models LifePenetration",  which(data$specification=="FOIDL") -(which(data$specification=="FOIDM"))),
             rep("S. Imputed Models LifeDensity",  which(data$specification=="FOID'") -(which(data$specification=="'")-1)),
             rep("M. Imputed Models LifeDensity",  which(data$specification=="FOIDM'") -(which(data$specification=="FOID'"))),
             rep("M. Imputed Models LifePenetration",  which(data$specification=="FOIDL'") -(which(data$specification=="FOIDM'"))),
             rep("S. Imputed Models LifeDensity",  which(data$specification=="FOID''") -(which(data$specification=="''")-1)),
             rep("M. Imputed Models LifeDensity",  which(data$specification=="FOIDM''") -(which(data$specification=="FOID''"))),
             rep("M. Imputed Models LifePenetration",  which(data$specification=="FOIDL''") -(which(data$specification=="FOIDM''"))))
             


Category <- factor(Category, levels = c("S. Imputed Models LifeDensity", 
                                        "M. Imputed Models LifeDensity",
                                        "M. Imputed Models LifePenetration"
                                        ))
          
#SD of CIEl variables
CIElch <- 1.1
lifedch <- 2.3
lifepch <- 3.3


change=rep(NA,length(data[,1]))
change[Category=="Mousseau Misspecified DV Models"]=1/(1+exp(-CIElch*coef[Category=="Mousseau Misspecified DV Models"])) -0.5
change[Category=="S. Imputed Models LifeDensity"]=1/(1+exp(-CIElch*coef[Category=="S. Imputed Models LifeDensity"])) -0.5
change[Category=="M. Imputed Models LifeDensity"]=1/(1+exp(-CIElch*coef[Category=="M. Imputed Models LifeDensity"])) -0.5
change[Category=="M. Imputed Models LifePenetration"]=1/(1+exp(-lifepch*coef[Category=="M. Imputed Models LifePenetration"])) -0.5

changep=-change

textoffset=rep(cbind(0.05, -0.05), length(data[,1])/2)
if (length(textoffset)==length(data[,1])-1) {
  textoffset <- c(textoffset,0.05)
}

if (length(textoffset)==length(data[,1])+1) {
  textoffset <- textoffset[1:length(data[,1])]
}
#sort the y values in descending order of coefficient, then apply offset on x axis  

textoffset2<- textoffset
textoffset2[order(change)] <- textoffset  
labelxposition=sqrt(data$CIElpvalues)+textoffset2

ytextloc <- changep
vlinelevel=0.2

xbreaks <-  sqrt(c(0, 0.01, .05, 0.1, 0.2, 0.5, 1))
xlabels <- c("0", "0.01", "0.05", "0.10", "0.20", "0.50", "1")



CIElpvaluest <- data$CIElpvalues
CIElpvaluest <- NA
CIElpvaluest <- sqrt(data$CIElpvalues)

p.w <- 8
p.h <- 8

#Making Color Figure
p <- ggplot(data, aes(x=CIElpvaluest, y=changep)) 
p <-p + geom_point(aes(x=CIElpvaluest, y=changep, colour=Category, shape=Category), size=2)
p <-p + geom_text(aes(x=labelxposition, y=ytextloc, label=labels, colour=Category), size=2)  
p <- p + xlab("p-values on Life Insurance Measure (Square-Root Scale)")
p <- p + scale_x_continuous(limits=c(-0.05,1), breaks = xbreaks, labels=xlabels)
p <- p + scale_y_continuous(limits=c(0,0.45))
#, breaks = c(0, 0.05, .1, .15, .2),
#                            labels = c("0", "0.05", "0.10", "0.15", "0.20"))
p <- p + opts(legend.position=c(0.8,0.8))
p <- p + opts(legend.background=theme_rect(fill="white"))
#c(0.8,0.8)
p <- p + ylab("Reduction in Probability of Conflict")
# Position legend in graph, where x,y is 0,0 (bottom left) to 1,1 (top right)
p
ggsave(file = "fig4pvalues_CIElcol.pdf", width = p.w, height = p.h)
ggsave(file = "fig4pvalues_CIElcol.png", width = p.w, height = p.h)

#Making BW Figure
p <- ggplot(data, aes(x=CIElpvaluest, y=changep)) 
p <-p + geom_point(aes(x=CIElpvaluest, y=changep, shape=Category), size=2)
p <-p + geom_text(aes(x=labelxposition, y=ytextloc, label=labels), size=2)  
p <- p + xlab("p-values on Life Insurance Measure (Square-Root Scale)")
p <- p + scale_x_continuous(limits=c(-0.05,1), breaks = xbreaks, labels=xlabels)
p <- p + scale_y_continuous(limits=c(0,0.45))
#, breaks = c(0, 0.05, .1, .15, .2),
#                            labels = c("0", "0.05", "0.10", "0.15", "0.20"))
#c(0.8,0.8)
p <- p + ylab("Reduction in Probability of Conflict")
p <- p +theme_bw()
p <- p + scale_shape(solid=FALSE)
p <- p + opts(legend.position=c(0.8,0.8))
p <- p + opts(legend.background=theme_rect(fill="white"))
# Position legend in graph, where x,y is 0,0 (bottom left) to 1,1 (top right)
p
ggsave(file = "fig4pvalues_CIElbw.pdf", width = p.w, height = p.h)
ggsave(file = "fig4pvalues_CIElbw.png", width = p.w, height = p.h)


