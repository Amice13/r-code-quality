  #### 12-06_20_pvalue_plot.r ####
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
  
  
  data <- read.dta("pvalues.dta")
  #Drop three other erroeneous DV models since they are confusing
  data <- data[data$specification!="M13 MID Onset" & data$specification!="M13 FMID Ongoing" & data$specification!="M13 MID Ongoing",]
  
  
  ##Dml Plot
  library(ggplot2)
  
  #dmldata=data[,names(data)=="DmlCoefficient" | names(data)=="Dmlpvalues"]
  #http://wiki.stdout.org/rcookbook/Graphs/Scatterplots%20(ggplot2)/
  #https://stat.ethz.ch/pipermail/r-help/2002-November/027206.html
  
  labels=data$specification
  
  dmlcoef <- data$DmlCoefficient
  #SD of dml in MM data is 5.798. Round it to 6
  dmlch <- 6
  Dmlchange=1/(1+exp(-dmlch*dmlcoef)) -0.5
  Dmlchangep=-Dmlchange
  
  textoffset=rep(cbind(0.05, -0.05), length(data[,1])/2)
  if (length(textoffset)==length(data[,1])-1) {
    textoffset <- c(textoffset,0.05)
  }
  
  if (length(textoffset)==length(data[,1])+1) {
    textoffset <- textoffset[1:length(data[,1])]
      }
  #sort the y values in descending order of coefficient, then apply offset on x axis  
  
  #textcolor="blue"
  pointcolor=c(rep("black",40),rep(grey(0.4),8))
  pointcolor=c()
  #textcolor=grey(0.4)
  textcolor="blue"
  textoffset2<- textoffset
  textoffset2[order(Dmlchange)] <- textoffset  
#   textoffset2[data$Dmlpvalues >= 0.008] <-
#     6*textoffset[data$Dmlpvalues >= 0.008]
#   
  textoffset2[labels==""]=-0.018
  textoffset2[labels=="(Mousseau 2013) Base Model"]=-0.20
  textoffset2[labels=="M13 MID Onset"]= 0.1
  textoffset2[labels=="M13 FMID Ongoing"]= 0.12
  textoffset2[labels=="M13 MID Ongoing"]= 0.1
  textoffset2[labels=="FO"]= 0.03
  labelxposition=sqrt(data$Dmlpvalues)+textoffset2

  #   labelxposition=data$Dmlpvalues+textoffset2
  #labelxposition=data$Dmlpvalues
  ytextloc <- Dmlchangep
  ytextloc[labels=="FIDM"] <- ytextloc[labels=="FIDM"]-0.001
  ytextloc[labels=="D"] <- ytextloc[labels=="D"]-0.002
  #ytextloc[labels=="D"] <- ytextloc[labels=="D"]-0.002
  ytextloc[labels=="IM"] <- ytextloc[labels=="IM"]+0.001
  ytextloc[labels=="FID"] <- ytextloc[labels=="FID"]+0.001
  ytextloc[labels=="(Mousseau 2013) Base Model"] <- ytextloc[labels=="(Mousseau 2013) Base Model"]
  
  vlinelevel=0.2
  

  xbreaks <-  sqrt(c(0, 0.01, .05, 0.1, 0.2, 0.5, 1))
  xlabels <- c("0", "0.01", "0.05", "0.10", "0.20", "0.50", "1")
  
  
  ##Transform p-values so can have sqrt layout
#   labelxpositiont <- labelxposition
#   labelxpositiont <- NA
#   labelxpositiont[labelxposition>0] <- sqrt(labelxposition[labelxposition>0])
#   labelxpositiont[labelxposition <= 0] <- -sqrt(-labelxposition[labelxposition <= 0])
  
  
  Dmlpvaluest <- data$Dmlpvalues
  Dmlpvaluest <- NA
  Dmlpvaluest <- sqrt(data$Dmlpvalues)
  
  Category = c(rep("Mousseau Misspecified DV Model",which(data$specification=="")-1), 
               rep("Models with Singly-Imputed Life Insurance",which(data$specification=="FOID")-(which(data$specification=="")-1)), 
               rep("Models with Multiply-Imputed Life Insurance",which(data$specification=="FOIDL")-(which(data$specification=="M")-1)),   
               rep("Models with no CIEl",length(data[,1])-which(data$specification=="FOIDL")))
  
  
  colour=c(rep(0,Rs-1),rep(1,Ds-Rs),rep(2,length(data[,1])-(Ds-1)))
  
  
  p.w <- 8
  p.h <- 8
  
  #Making Color Figure
  p <- ggplot(data, aes(x=Dmlpvaluest, y=Dmlchangep)) 
  p <-p + geom_point(aes(x=Dmlpvaluest, y=Dmlchangep, colour=Category, shape=Category), size=2)
  p <-p + geom_text(aes(x=labelxposition, y=ytextloc, label=labels, colour=Category), size=2)  
  p <- p + xlab("p-values on DemocracyLow (Square-Root Scale)")
  p <- p + scale_x_continuous(breaks = xbreaks, limits=c(-0.05,1),
                              labels = xlabels)
  p <- p + scale_y_continuous(breaks = c(0, 0.05, .1, .15, .2),
                              labels = c("0", "0.05", "0.10", "0.15", "0.20"))
  p <- p + opts(legend.position=c(0.7,0.8))
  p <- p + opts(legend.background=theme_rect(fill="white"))
  #c(0.8,0.8)
  p <- p + ylab("Reduction in Probability of Conflict")
  # Position legend in graph, where x,y is 0,0 (bottom left) to 1,1 (top right)
   p
    ggsave(file = "fig1pvaluescol.pdf", width = p.w, height = p.h)
  ggsave(file = "fig1pvaluescol.png", width = p.w, height = p.h)  
  
  #Making GreyScale Figure
  p <- ggplot(data, aes(x=Dmlpvaluest, y=Dmlchangep)) 
  p <-p + geom_point(aes(x=Dmlpvaluest, y=Dmlchangep, shape=Category, 
                         fill=Category, size=Category))
  p <- p + scale_shape(solid=FALSE) + scale_size_manual(values=c(2,2,2,4))
  p <- p + geom_text(aes(x=labelxposition, y=ytextloc, 
                         label=labels, size=Category), show_guide=FALSE) 
  p <- p + xlab("p-values on DemocracyLow (Square-Root Scale)")
  p <- p + scale_x_continuous(breaks = xbreaks, limits=c(-0.05,1),
                              labels = xlabels)
  p <- p + scale_y_continuous(breaks = c(0, 0.05, .1, .15, .2),
                              labels = c("0", "0.05", "0.10", "0.15", "0.20"))
  #c(0.8,0.8)
  p <- p + ylab("Reduction in Probability of Conflict")
  # Position legend in graph, where x,y is 0,0 (bottom left) to 1,1 (top right)
  p <- p +theme_bw() 
  p <- p + opts(legend.position=c(0.75,0.8))
  p <- p + opts(legend.background=theme_rect(fill="white"))
  p
  ggsave(file = "fig1pvaluesbw.pdf", width = p.w, height = p.h)
  ggsave(file = "fig1pvaluesbw.png", width = p.w, height = p.h)
