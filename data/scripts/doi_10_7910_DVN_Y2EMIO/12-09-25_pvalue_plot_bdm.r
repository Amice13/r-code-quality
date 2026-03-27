#### 12-09-25_pvalue_plot_bdm.r ####
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


data <- read.dta("pvaluesbdm.dta")
data2 <- read.dta("pvaluesh10dm.dta")

library(ggplot2)

labels=data$specification

dmlcoef <- data$bdmCoefficient
#SD of dml in MM data is 5.798. Round it to 6
dmlch <- 1 #Since it is a dummy variable
Dmlchange=1/(1+exp(-dmlch*dmlcoef)) -0.5
Dmlchangep=-Dmlchange

textoffset=rep(cbind(0.03, -0.03), length(data[,1])/2)
if (length(textoffset)==length(data[,1])-1) {
  textoffset <- c(textoffset,0.05)
}

if (length(textoffset)==length(data[,1])+1) {
  textoffset <- textoffset[1:length(data[,1])]
}
#sort the y values in descending order of coefficient, then apply offset on x axis  

textoffset2<- textoffset
textoffset2[order(Dmlchange)] <- textoffset  
labelxposition=sqrt(data$bdmpvalues)+textoffset2

ytextloc <- Dmlchangep
vlinelevel=0.2

xbreaks <-  sqrt(c(0, 0.01, .05, 0.1, 0.2, 0.5, 1))
xlabels <- c("0", "0.01", "0.05", "0.10", "0.20", "0.50", "1")

Category = c(rep("S. Imputed Models",data$specnumber[data$specification=="FOID'"]), 
             rep("Models with no CIEl",data$specnumber[data$specification=="FODn'"]-data$specnumber[data$specification=="FOID'"]), 
             rep("M. Imputed Models", length(data[,1])-data$specnumber[data$specification=="FODn'"]))
           
Category <- factor(Category, levels = c("M. Imputed Models",
                                        "S. Imputed Models",
                                        "Models with no CIEl"
))

Dmlpvaluest <- data$bdmpvalues
Dmlpvaluest <- NA
Dmlpvaluest <- sqrt(data$bdmpvalues)

p.w <- 8
p.h <- 8

#Making Color Figure
p <- ggplot(data, aes(x=Dmlpvaluest, y=Dmlchangep)) 
p <-p + geom_point(aes(x=Dmlpvaluest, y=Dmlchangep, colour=Category, shape=Category), size=2)
p <-p + geom_text(aes(x=labelxposition, y=ytextloc, label=labels, colour=Category), size=2)  
p <- p + xlab("p-values on BothDemoracy(P>6) (Square-Root Scale)")
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
ggsave(file = "fig2pvaluesbdmcol.pdf", width = p.w, height = p.h)
ggsave(file = "fig2pvaluesbdmcol.png", width = p.w, height = p.h)


#Making BW Figure
p <- ggplot(data, aes(x=Dmlpvaluest, y=Dmlchangep)) 
p <-p + geom_point(aes(x=Dmlpvaluest, y=Dmlchangep, shape=Category), size=2)
p <-p + geom_text(aes(x=labelxposition, y=ytextloc, label=labels), size=2)  
p <- p + xlab("p-values on BothDemoracy(P>6) (Square-Root Scale)")
p <- p + scale_x_continuous(limits=c(-0.05,1), breaks = xbreaks, labels=xlabels)
p <- p + scale_y_continuous(limits=c(0,0.45))
#, breaks = c(0, 0.05, .1, .15, .2),
#                            labels = c("0", "0.05", "0.10", "0.15", "0.20"))
#c(0.8,0.8)
p <- p +theme_bw()
p <- p + scale_shape(solid=FALSE)
p <- p + ylab("Reduction in Probability of Conflict")
p <- p + opts(legend.position=c(0.8,0.8))
p <- p + opts(legend.background=theme_rect(fill="white"))

# Position legend in graph, where x,y is 0,0 (bottom left) to 1,1 (top right)
p
ggsave(file = "fig2pvaluesbdmbw.pdf", width = p.w, height = p.h)
ggsave(file = "fig2pvaluesbdmbw.png", width = p.w, height = p.h)

