##################################################
##################################################
#This code reproduces Figures 2-4 and Appendix ###
#Figures 6 and 7 for Wherry, Miller, Kaestner, ###
#and Meyer REStat                              ###
#Contact: Sarah Miller (mille@umich.edu).      ###
##################################################
##################################################

setwd(".")

library(rdrobust)

#######Inpatient 2009: Figure 3/Appendix Figure 6

all3 <- read.csv("allraces2009.csv")
all3 <- all3[order(all3$byear, all3$bmonth),]
all3$time <- seq(1:length(all3[,1]))-49
all3$after <-ifelse(all3$time >= 0,1,0)
all3$time2 <- all3$time*all3$time

allblack3 <- read.csv("black2009.csv")
allblack3 <- allblack3[order(allblack3$byear, allblack3$bmonth),]
allblack3$time <- seq(1:length(allblack3[,1]))-49
allblack3$after <-ifelse(allblack3$time >= 0,1,0)
allblack3$time2 <- allblack3$time*allblack3$time

allnotblack3 <- read.csv("nonblack2009.csv")
allnotblack3 <- allnotblack3[order(allnotblack3$byear, allnotblack3$bmonth),]
allnotblack3$time <- seq(1:length(allnotblack3[,1]))-49
allnotblack3$after <-ifelse(allnotblack3$time >= 0,1,0)
allnotblack3$time2 <- allnotblack3$time*allnotblack3$time

all3$logchron <- log(all3$HCUPChronic)
all3$lognonchron <- log(all3$count-all3$preg-all3$HCUPChronic)
all3$logcost <- log(all3$cost)

modelnp <- lm(lognp~as.factor(bmonth), data=all3)
modelchron <- lm(logchron~as.factor(bmonth), data=all3)
modelnonchron <- lm(lognonchron~as.factor(bmonth), data=all3)
modelcost <- lm(logcost~as.factor(bmonth), data=all3)

allblack3$logchron <- log(allblack3$HCUPChronic)
allblack3$lognonchron <- log(allblack3$count-allblack3$preg-allblack3$HCUPChronic)
allblack3$logcost <- log(allblack3$cost)

modelnpb <- lm(lognp~as.factor(bmonth), data=allblack3)
modelchronb <- lm(logchron~as.factor(bmonth), data=allblack3)
modelnonchronb <- lm(lognonchron~as.factor(bmonth), data=allblack3)
modelcostb <- lm(logcost~as.factor(bmonth), data=allblack3)

allnotblack3$logchron <- log(allnotblack3$HCUPChronic)
allnotblack3$lognonchron <- log(allnotblack3$count-allnotblack3$preg-allnotblack3$HCUPChronic)
allnotblack3$logcost <- log(allnotblack3$cost)

pdf("figures2/all2009r.pdf", width=6.6, height=4.68)
rdbinselect(modelnp$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)")
dev.off()
pdf("figures2/chronic2009r.pdf", width=6.6, height=4.68)
rdbinselect(modelchron$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)")
dev.off()
pdf("figures2/nonchronic2009r.pdf", width=6.6, height=4.68)
rdbinselect(modelnonchron$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)")
dev.off()
pdf("figures2/cost2009r.pdf", width=6.6, height=4.68)
rdbinselect(modelcost$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)")
dev.off()

pdf("figures2/all2009br.pdf", width=6.6, height=4.68)
rdbinselect(modelnpb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)", y.lim=c(-0.17, 0.22))
dev.off()
pdf("figures2/chronic2009br.pdf", width=6.6, height=4.68)
rdbinselect(modelchronb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)", y.lim=c(-0.22, 0.22))
dev.off()
pdf("figures2/nonchronic2009br.pdf", width=6.6, height=4.68)
rdbinselect(modelnonchronb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)")
dev.off()
pdf("figures2/cost2009br.pdf", width=6.6, height=4.68)
rdbinselect(modelcostb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)", y.lim=c(-0.23, 0.22))
dev.off()

pdf("figures2/all2009nbr.pdf", width=6.6, height=4.68)
rdbinselect(modelnpnb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)")
dev.off()
pdf("figures2/chronic2009nbr.pdf", width=6.6, height=4.68)
rdbinselect(modelchronnb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)")
dev.off()
pdf("figures2/nonchronic2009nbr.pdf", width=6.6, height=4.68)
rdbinselect(modelnonchronnb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)")
dev.off()
pdf("figures2/cost2009nbr.pdf", width=6.6, height=4.68)
rdbinselect(modelcostnb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)")
dev.off()

#######Inpatient 2009 Low Income: Figure 4/Appendix Figure 7

all3 <- read.csv("allracesli2009.csv")
all3 <- all3[order(all3$byear, all3$bmonth),]
all3$time <- seq(1:length(all3[,1]))-49
all3$after <-ifelse(all3$time >= 0,1,0)
all3$time2 <- all3$time*all3$time

allblack3 <- read.csv("blackli2009.csv")
allblack3 <- allblack3[order(allblack3$byear, allblack3$bmonth),]
allblack3$time <- seq(1:length(allblack3[,1]))-49
allblack3$after <-ifelse(allblack3$time >= 0,1,0)
allblack3$time2 <- allblack3$time*allblack3$time

allnotblack3 <- read.csv("nonblackli2009.csv")
allnotblack3 <- allnotblack3[order(allnotblack3$byear, allnotblack3$bmonth),]
allnotblack3$time <- seq(1:length(allnotblack3[,1]))-49
allnotblack3$after <-ifelse(allnotblack3$time >= 0,1,0)
allnotblack3$time2 <- allnotblack3$time*allnotblack3$time

all3$logchron <- log(all3$HCUPChronic)
all3$lognonchron <- log(all3$count-all3$preg-all3$HCUPChronic)
all3$logcost <- log(all3$cost)

modelnp <- lm(lognp~as.factor(bmonth), data=all3)
modelchron <- lm(logchron~as.factor(bmonth), data=all3)
modelnonchron <- lm(lognonchron~as.factor(bmonth), data=all3)
modelcost <- lm(logcost~as.factor(bmonth), data=all3)

allblack3$logchron <- log(allblack3$HCUPChronic)
allblack3$lognonchron <- log(allblack3$count-allblack3$preg-allblack3$HCUPChronic)
allblack3$logcost <- log(allblack3$cost)

modelnpb <- lm(lognp~as.factor(bmonth), data=allblack3)
modelchronb <- lm(logchron~as.factor(bmonth), data=allblack3)
modelnonchronb <- lm(lognonchron~as.factor(bmonth), data=allblack3)
modelcostb <- lm(logcost~as.factor(bmonth), data=allblack3)

allnotblack3$logchron <- log(allnotblack3$HCUPChronic)
allnotblack3$lognonchron <- log(allnotblack3$count-allnotblack3$preg-allnotblack3$HCUPChronic)
allnotblack3$logcost <- log(allnotblack3$cost)

modelnpnb <- lm(lognp~as.factor(bmonth), data=allnotblack3)
modelchronnb <- lm(logchron~as.factor(bmonth), data=allnotblack3)
modelnonchronnb <- lm(lognonchron~as.factor(bmonth), data=allnotblack3)
modelcostnb <- lm(logcost~as.factor(bmonth), data=allblack3)


pdf("figures2/all2009rli.pdf", width=6.6, height=4.68)
rdbinselect(modelnp$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)")
dev.off()
pdf("figures2/chronic2009rli.pdf", width=6.6, height=4.68)
rdbinselect(modelchron$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)")
dev.off()
pdf("figures2/nonchronic2009rli.pdf", width=6.6, height=4.68)
rdbinselect(modelnonchron$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)")
dev.off()
pdf("figures2/cost2009rli.pdf", width=6.6, height=4.68)
rdbinselect(modelcost$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)")
dev.off()

pdf("figures2/all2009brli.pdf", width=6.6, height=4.68)
rdbinselect(modelnpb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)", y.lim=c(-0.25, 0.21))
dev.off()
pdf("figures2/chronic2009brli.pdf", width=6.6, height=4.68)
rdbinselect(modelchronb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)", y.lim=c(-0.28, 0.25))
dev.off()
pdf("figures2/nonchronic2009brli.pdf", width=6.6, height=4.68)
rdbinselect(modelnonchronb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)")
dev.off()
pdf("figures2/cost2009brli.pdf", width=6.6, height=4.68)
rdbinselect(modelcostb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)")
dev.off()

pdf("figures2/all2009nbrli.pdf", width=6.6, height=4.68)
rdbinselect(modelnpnb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)")
dev.off()
pdf("figures2/chronic2009nbrli.pdf", width=6.6, height=4.68)
rdbinselect(modelchronnb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)")
dev.off()
pdf("figures2/nonchronic2009nbrli.pdf", width=6.6, height=4.68)
rdbinselect(modelnonchronnb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)")
dev.off()
pdf("figures2/cost2009nbrli.pdf", width=6.6, height=4.68)
rdbinselect(modelcostnb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)")
dev.off()


#######Inpatient 1999: Figure 2/Appendix Figure 6

all3 <- read.csv("allraces99.csv")
all3 <- all3[order(all3$byear, all3$bmonth),]
all3$time <- seq(1:length(all3[,1]))-49
all3$after <-ifelse(all3$time >= 0,1,0)
all3$time2 <- all3$time*all3$time

allblack3 <- read.csv("black99.csv")
allblack3 <- allblack3[order(allblack3$byear, allblack3$bmonth),]
allblack3$time <- seq(1:length(allblack3[,1]))-49
allblack3$after <-ifelse(allblack3$time >= 0,1,0)
allblack3$time2 <- allblack3$time*allblack3$time

allnotblack3 <- read.csv("notblack1999.csv")
allnotblack3 <- allnotblack3[order(allnotblack3$byear, allnotblack3$bmonth),]
allnotblack3$time <- seq(1:length(allnotblack3[,1]))-49
allnotblack3$after <-ifelse(allnotblack3$time >= 0,1,0)
allnotblack3$time2 <- allnotblack3$time*allnotblack3$time

all3$logchron <- log(all3$HCUPChronic)
all3$lognonchron <- log(all3$count-all3$preg-all3$HCUPChronic)

modelnp <- lm(lognp~as.factor(bmonth), data=all3)
modelchron <- lm(logchron~as.factor(bmonth), data=all3)
modelnonchron <- lm(lognonchron~as.factor(bmonth), data=all3)

allblack3$logchron <- log(allblack3$HCUPChronic)
allblack3$lognonchron <- log(allblack3$count-allblack3$preg-allblack3$HCUPChronic)

modelnpb <- lm(lognp~as.factor(bmonth), data=allblack3)
modelchronb <- lm(logchron~as.factor(bmonth), data=allblack3)
modelnonchronb <- lm(lognonchron~as.factor(bmonth), data=allblack3)

allnotblack3$logchron <- log(allnotblack3$HCUPChronic)
allnotblack3$lognonchron <- log(allnotblack3$count-allnotblack3$preg-allnotblack3$HCUPChronic)

modelnpnb <- lm(lognp~as.factor(bmonth), data=allnotblack3)
modelchronnb <- lm(logchron~as.factor(bmonth), data=allnotblack3)
modelnonchronnb <- lm(lognonchron~as.factor(bmonth), data=allnotblack3)

pdf("figures2/all1999r.pdf", width=6.6, height=4.68)
rdbinselect(modelnp$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)")
dev.off()
pdf("figures2/chronic1999r.pdf", width=6.6, height=4.68)
rdbinselect(modelchron$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)")
dev.off()
pdf("figures2/nonchronic1999r.pdf", width=6.6, height=4.68)
rdbinselect(modelnonchron$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)")
dev.off()

pdf("figures2/all1999br.pdf", width=6.6, height=4.68)
rdbinselect(modelnpb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)")
dev.off()
pdf("figures2/chronic1999br.pdf", width=6.6, height=4.68)
rdbinselect(modelchronb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)")
dev.off()
pdf("figures2/nonchronic1999br.pdf", width=6.6, height=4.68)
rdbinselect(modelnonchronb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)")
dev.off()

pdf("figures2/all1999nbr.pdf", width=6.6, height=4.68)
rdbinselect(modelnpnb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)")
dev.off()
pdf("figures2/chronic1999nbr.pdf", width=6.6, height=4.68)
rdbinselect(modelchronnb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)")
dev.off()
pdf("figures2/nonchronic1999nbr.pdf", width=6.6, height=4.68)
rdbinselect(modelnonchronnb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)")
dev.off()

#######ED 2009: Figure 3/Appendix Figure 6

all3 <- read.csv("ed.csv")
all3 <- all3[order(all3$byear, all3$bmonth),]
all3$time <- seq(1:length(all3[,1]))-49
all3$after <-ifelse(all3$time >= 0,1,0)
all3$time2 <- all3$time*all3$time

allblack3 <- read.csv("edblack.csv")
allblack3 <- allblack3[order(allblack3$byear, allblack3$bmonth),]
allblack3$time <- seq(1:length(allblack3[,1]))-49
allblack3$after <-ifelse(allblack3$time >= 0,1,0)
allblack3$time2 <- allblack3$time*allblack3$time

allnotblack3 <- read.csv("nonblacked.csv")
allnotblack3 <- allnotblack3[order(allnotblack3$byear, allnotblack3$bmonth),]
allnotblack3$time <- seq(1:length(allnotblack3[,1]))-49
allnotblack3$after <-ifelse(allnotblack3$time >= 0,1,0)
allnotblack3$time2 <- allnotblack3$time*allnotblack3$time

all3$logchron <- log(all3$HCUPChronic)
all3$lognonchron <- log(all3$count-all3$HCUPChronic)
all3$lognp <- log(all3$count)
all3$logcost <- log(all3$cost)

modelnp <- lm(lognp~as.factor(bmonth), data=all3)
modelchron <- lm(logchron~as.factor(bmonth), data=all3)
modelnonchron <- lm(lognonchron~as.factor(bmonth), data=all3)
modelcost <- lm(logcost~as.factor(bmonth), data=all3)

allblack3$logchron <- log(allblack3$HCUPChronic)
allblack3$lognonchron <- log(allblack3$count-allblack3$HCUPChronic)
allblack3$lognp <- log(allblack3$count)
allblack3$logcost <- log(allblack3$cost)

modelnpb <- lm(lognp~as.factor(bmonth), data=allblack3)
modelchronb <- lm(logchron~as.factor(bmonth), data=allblack3)
modelnonchronb <- lm(lognonchron~as.factor(bmonth), data=allblack3)
modelcostb <- lm(logcost~as.factor(bmonth), data=allblack3)

allnotblack3$logchron <- log(allnotblack3$HCUPChronic)
allnotblack3$lognonchron <- log(allnotblack3$count-allnotblack3$HCUPChronic)
allnotblack3$lognp <- log(allnotblack3$count)
allnotblack3$logcost <- log(allnotblack3$cost)

modelnpnb <- lm(lognp~as.factor(bmonth), data=allnotblack3)
modelchronnb <- lm(logchron~as.factor(bmonth), data=allnotblack3)
modelnonchronnb <- lm(lognonchron~as.factor(bmonth), data=allnotblack3)
modelcostnb <- lm(logcost~as.factor(bmonth), data=allnotblack3)

pdf("figures2/alledr.pdf", width=6.6, height=4.68)
rdbinselect(modelnp$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of ED Visits (Month FEs removed)")
dev.off()
pdf("figures2/chronicedr.pdf", width=6.6, height=4.68)
rdbinselect(modelchron$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of ED Visits (Month FEs removed)")
dev.off()
pdf("figures2/nonchronicedr.pdf", width=6.6, height=4.68)
rdbinselect(modelnonchron$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of ED Visits (Month FEs removed)")
dev.off()
pdf("figures2/costedr.pdf", width=6.6, height=4.68)
rdbinselect(modelcost$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of ED Visits (Month FEs removed)")
dev.off()

pdf("figures2/alledbr.pdf", width=6.6, height=4.68)
rdbinselect(modelnpb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of ED Visits (Month FEs removed)")
dev.off()
pdf("figures2/chronicedbr.pdf", width=6.6, height=4.68)
rdbinselect(modelchronb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of ED Visits (Month FEs removed)")
dev.off()
pdf("figures2/nonchronicedbr.pdf", width=6.6, height=4.68)
rdbinselect(modelnonchronb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of ED Visits (Month FEs removed)")
dev.off()
pdf("figures2/costedbr.pdf", width=6.6, height=4.68)
rdbinselect(modelcostb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of ED Costs (Month FEs removed)")
dev.off()

pdf("figures2/allednbr.pdf", width=6.6, height=4.68)
rdbinselect(modelnpnb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of ED Visits (Month FEs removed)")
dev.off()
pdf("figures2/chronicednbr.pdf", width=6.6, height=4.68)
rdbinselect(modelchronnb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of ED Visits (Month FEs removed)")
dev.off()
pdf("figures2/nonchronicednbr.pdf", width=6.6, height=4.68)
rdbinselect(modelnonchronnb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of ED Visits (Month FEs removed)")
dev.off()
pdf("figures2/costednbr.pdf", width=6.6, height=4.68)
rdbinselect(modelcostnb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of ED Costs (Month FEs removed)")
dev.off()


############ED Low Income Zip Code: Figure 4/Appendix Figure 7

all3 <- read.csv("allracesedli.csv")
all3 <- all3[order(all3$byear, all3$bmonth),]
all3$time <- seq(1:length(all3[,1]))-49
all3$after <-ifelse(all3$time >= 0,1,0)
all3$time2 <- all3$time*all3$time

allblack3 <- read.csv("blackedli.csv")
allblack3 <- allblack3[order(allblack3$byear, allblack3$bmonth),]
allblack3$time <- seq(1:length(allblack3[,1]))-49
allblack3$after <-ifelse(allblack3$time >= 0,1,0)
allblack3$time2 <- allblack3$time*allblack3$time

allnotblack3 <- read.csv("nonblackedli.csv")
allnotblack3 <- allnotblack3[order(allnotblack3$byear, allnotblack3$bmonth),]
allnotblack3$time <- seq(1:length(allnotblack3[,1]))-49
allnotblack3$after <-ifelse(allnotblack3$time >= 0,1,0)
allnotblack3$time2 <- allnotblack3$time*allnotblack3$time

all3$logchron <- log(all3$HCUPChronic)
all3$lognonchron <- log(all3$count-all3$HCUPChronic)
all3$lognp <- log(all3$count)
all3$logcost <- log(all3$cost)


modelnp <- lm(lognp~as.factor(bmonth), data=all3)
modelchron <- lm(logchron~as.factor(bmonth), data=all3)
modelnonchron <- lm(lognonchron~as.factor(bmonth), data=all3)
modelcost <- lm(logcost~as.factor(bmonth), data=all3)


allblack3$logchron <- log(allblack3$HCUPChronic)
allblack3$lognonchron <- log(allblack3$count-allblack3$HCUPChronic)
allblack3$lognp <- log(allblack3$count)
allblack3$logcost <- log(allblack3$cost)


modelnpb <- lm(lognp~as.factor(bmonth), data=allblack3)
modelchronb <- lm(logchron~as.factor(bmonth), data=allblack3)
modelnonchronb <- lm(lognonchron~as.factor(bmonth), data=allblack3)
modelcostb <- lm(logcost~as.factor(bmonth), data=allblack3)

allnotblack3$logchron <- log(allnotblack3$HCUPChronic)
allnotblack3$lognonchron <- log(allnotblack3$count-allnotblack3$HCUPChronic)
allnotblack3$lognp <- log(allnotblack3$count)
allnotblack3$logcost <- log(allnotblack3$cost)

modelnpnb <- lm(lognp~as.factor(bmonth), data=allnotblack3)
modelchronnb <- lm(logchron~as.factor(bmonth), data=allnotblack3)
modelnonchronnb <- lm(lognonchron~as.factor(bmonth), data=allnotblack3)
modelcostnb <- lm(logcost~as.factor(bmonth), data=allnotblack3)

pdf("figures2/alledrli.pdf", width=6.6, height=4.68)
rdbinselect(modelnp$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of ED Visits (Month FEs removed)")
dev.off()
pdf("figures2/chronicedrli.pdf", width=6.6, height=4.68)
rdbinselect(modelchron$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of ED Visits (Month FEs removed)")
dev.off()
pdf("figures2/nonchronicedrli.pdf", width=6.6, height=4.68)
rdbinselect(modelnonchron$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of ED Visits (Month FEs removed)")
dev.off()
pdf("figures2/costedrli.pdf", width=6.6, height=4.68)
rdbinselect(modelcost$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of ED Visits (Month FEs removed)")
dev.off()

pdf("figures2/alledbrli.pdf", width=6.6, height=4.68)
rdbinselect(modelnpb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of ED Visits (Month FEs removed)")
dev.off()
pdf("figures2/chronicedbrli.pdf", width=6.6, height=4.68)
rdbinselect(modelchronb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of ED Visits(Month FEs removed)")
dev.off()
pdf("figures2/nonchronicedbrli.pdf", width=6.6, height=4.68)
rdbinselect(modelnonchronb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of ED Visits (Month FEs removed)")
dev.off()
pdf("figures2/nonchronicedbrli.pdf", width=6.6, height=4.68)
rdbinselect(modelnonchronb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of ED Costs (Month FEs removed)")
dev.off()

pdf("figures2/allednbrli.pdf", width=6.6, height=4.68)
rdbinselect(modelnpnb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)")
dev.off()
pdf("figures2/chronicednbrli.pdf", width=6.6, height=4.68)
rdbinselect(modelchronnb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)")
dev.off()
pdf("figures2/nonchronicednbrli.pdf", width=6.6, height=4.68)
rdbinselect(modelnonchronnb$residuals, all3$time, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1978 to Aug 1989)", y.label="Log of Hospitalizations (Month FEs removed)")
dev.off()