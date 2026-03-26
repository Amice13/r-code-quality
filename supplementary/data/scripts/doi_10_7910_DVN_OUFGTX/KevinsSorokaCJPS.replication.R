
#This is the replication file for "Growing Apart? Partisan Sorting in Canada, 1992–2015," by Anthony Kevins and Stuart Soroka, published in 2017 in the Canadian Journal of Political Science.

#The anaysis depends on a merged dataset, of all CES surveys frm 1992 to 2015.  The first step in building that database was to download the final version of all surveys from the Canadian Opinion Research Archive.  Those datasets are archived there, and so we do not archive them ourselves.  Rather, we include analysis with just the already-merged version of the dataset.  The script below uses that dataset, and replicates all analyses in the paper. 

library(foreign)
library(recoder)
library(stargazer)

#set working directory

load("KevinsSorokaCES.merged.Rdata")

#########################################################
#########################################################
#DATA ANAYSIS

table(CES$year)

#TABLE 1

cor.test(CES$std,CES$gap)
cor.test(CES$std,CES$get)
cor.test(CES$gap,CES$get)

#APPENDIX TABLE A1 AND FIGURES 1 TO 3

#STANDARD OF LIVING

AN1 <- anova(lm(std ~ pidROC, data=CES[CES$quebec!=1 & CES$year==1992,])) ; var92 <- (AN1[1,2] / (AN1[1,2]+AN1[2,2]) ) *100
AN2 <- anova(lm(std ~ pidROC, data=CES[CES$quebec!=1 & CES$year==1997,])) ; var97 <- (AN2[1,2] / (AN2[1,2]+AN2[2,2]) ) *100
AN3 <- anova(lm(std ~ pidROC, data=CES[CES$quebec!=1 & CES$year==2000,])) ; var00 <- (AN3[1,2] / (AN3[1,2]+AN3[2,2]) ) *100
AN4 <- anova(lm(std ~ pidROC, data=CES[CES$quebec!=1 & CES$year==2004,])) ; var04 <- (AN4[1,2] / (AN4[1,2]+AN4[2,2]) ) *100
AN5 <- anova(lm(std ~ pidROC, data=CES[CES$quebec!=1 & CES$year==2006,])) ; var06 <- (AN5[1,2] / (AN5[1,2]+AN5[2,2]) ) *100
AN6 <- anova(lm(std ~ pidROC, data=CES[CES$quebec!=1 & CES$year==2008,])) ; var08 <- (AN6[1,2] / (AN6[1,2]+AN6[2,2]) ) *100
AN7 <- anova(lm(std ~ pidROC, data=CES[CES$quebec!=1 & CES$year==2011,])) ; var11 <- (AN7[1,2] / (AN7[1,2]+AN7[2,2]) ) *100
var15 <- NA
bypid <- rbind(var92,var97,var00,var04,var06,var08,var11,var15)
AN1 ; AN2; AN3 ; AN4 ; AN5 ; AN6 ; AN7

AN1 <- anova(lm(std ~ vtROC, data=CES[CES$quebec!=1 & CES$year==1992,])) ; var92 <- (AN1[1,2] / (AN1[1,2]+AN1[2,2]) ) *100
AN2 <- anova(lm(std ~ vtROC, data=CES[CES$quebec!=1 & CES$year==1997,])) ; var97 <- (AN2[1,2] / (AN2[1,2]+AN2[2,2]) ) *100
AN3 <- anova(lm(std ~ vtROC, data=CES[CES$quebec!=1 & CES$year==2000,])) ; var00 <- (AN3[1,2] / (AN3[1,2]+AN3[2,2]) ) *100
AN4 <- anova(lm(std ~ vtROC, data=CES[CES$quebec!=1 & CES$year==2004,])) ; var04 <- (AN4[1,2] / (AN4[1,2]+AN4[2,2]) ) *100
AN5 <- anova(lm(std ~ vtROC, data=CES[CES$quebec!=1 & CES$year==2006,])) ; var06 <- (AN5[1,2] / (AN5[1,2]+AN5[2,2]) ) *100
AN6 <- anova(lm(std ~ vtROC, data=CES[CES$quebec!=1 & CES$year==2008,])) ; var08 <- (AN6[1,2] / (AN6[1,2]+AN6[2,2]) ) *100
AN7 <- anova(lm(std ~ vtROC, data=CES[CES$quebec!=1 & CES$year==2011,])) ; var11 <- (AN7[1,2] / (AN7[1,2]+AN7[2,2]) ) *100
var15 <- NA
byvt <- rbind(var92,var97,var00,var04,var06,var08,var11,var15)
AN1 ; AN2; AN3 ; AN4 ; AN5 ; AN6 ; AN7

AN1 <- anova(lm(std ~ inc, data=CES[CES$quebec!=1 & CES$year==1992,])) ; var92 <- (AN1[1,2] / (AN1[1,2]+AN1[2,2]) ) *100
AN2 <- anova(lm(std ~ inc, data=CES[CES$quebec!=1 & CES$year==1997,])) ; var97 <- (AN2[1,2] / (AN2[1,2]+AN2[2,2]) ) *100
AN3 <- anova(lm(std ~ inc, data=CES[CES$quebec!=1 & CES$year==2000,])) ; var00 <- (AN3[1,2] / (AN3[1,2]+AN3[2,2]) ) *100
AN4 <- anova(lm(std ~ inc, data=CES[CES$quebec!=1 & CES$year==2004,])) ; var04 <- (AN4[1,2] / (AN4[1,2]+AN4[2,2]) ) *100
AN5 <- anova(lm(std ~ inc, data=CES[CES$quebec!=1 & CES$year==2006,])) ; var06 <- (AN5[1,2] / (AN5[1,2]+AN5[2,2]) ) *100
AN6 <- anova(lm(std ~ inc, data=CES[CES$quebec!=1 & CES$year==2008,])) ; var08 <- (AN6[1,2] / (AN6[1,2]+AN6[2,2]) ) *100
AN7 <- anova(lm(std ~ inc, data=CES[CES$quebec!=1 & CES$year==2011,])) ; var11 <- (AN7[1,2] / (AN7[1,2]+AN7[2,2]) ) *100
var15 <- NA
byinc <- rbind(var92,var97,var00,var04,var06,var08,var11,var15)
AN1 ; AN2; AN3 ; AN4 ; AN5 ; AN6 ; AN7

varexROC <- cbind(bypid,byvt,byinc)
colnames(varexROC) <- c("pid","vote","inc")
varexROC <- as.data.frame(varexROC)
varexROC$year <- c(1992,1997,2000,2004,2006,2008,2011,2015)

AN1 <- anova(lm(std ~ pidQC, data=CES[CES$quebec==1 & CES$year==1992,])) ; var92 <- (AN1[1,2] / (AN1[1,2]+AN1[2,2]) ) *100
AN2 <- anova(lm(std ~ pidQC, data=CES[CES$quebec==1 & CES$year==1997,])) ; var97 <- (AN2[1,2] / (AN2[1,2]+AN2[2,2]) ) *100
AN3 <- anova(lm(std ~ pidQC, data=CES[CES$quebec==1 & CES$year==2000,])) ; var00 <- (AN3[1,2] / (AN3[1,2]+AN3[2,2]) ) *100
AN4 <- anova(lm(std ~ pidQC, data=CES[CES$quebec==1 & CES$year==2004,])) ; var04 <- (AN4[1,2] / (AN4[1,2]+AN4[2,2]) ) *100
AN5 <- anova(lm(std ~ pidQC, data=CES[CES$quebec==1 & CES$year==2006,])) ; var06 <- (AN5[1,2] / (AN5[1,2]+AN5[2,2]) ) *100
AN6 <- anova(lm(std ~ pidQC, data=CES[CES$quebec==1 & CES$year==2008,])) ; var08 <- (AN6[1,2] / (AN6[1,2]+AN6[2,2]) ) *100
AN7 <- anova(lm(std ~ pidQC, data=CES[CES$quebec==1 & CES$year==2011,])) ; var11 <- (AN7[1,2] / (AN7[1,2]+AN7[2,2]) ) *100
var15 <- NA
bypid <- rbind(var92,var97,var00,var04,var06,var08,var11,var15)
AN1 ; AN2; AN3 ; AN4 ; AN5 ; AN6 ; AN7

AN1 <- anova(lm(std ~ vtQC, data=CES[CES$quebec==1 & CES$year==1992,])) ; var92 <- (AN1[1,2] / (AN1[1,2]+AN1[2,2]) ) *100
AN2 <- anova(lm(std ~ vtQC, data=CES[CES$quebec==1 & CES$year==1997,])) ; var97 <- (AN2[1,2] / (AN2[1,2]+AN2[2,2]) ) *100
AN3 <- anova(lm(std ~ vtQC, data=CES[CES$quebec==1 & CES$year==2000,])) ; var00 <- (AN3[1,2] / (AN3[1,2]+AN3[2,2]) ) *100
AN4 <- anova(lm(std ~ vtQC, data=CES[CES$quebec==1 & CES$year==2004,])) ; var04 <- (AN4[1,2] / (AN4[1,2]+AN4[2,2]) ) *100
AN5 <- anova(lm(std ~ vtQC, data=CES[CES$quebec==1 & CES$year==2006,])) ; var06 <- (AN5[1,2] / (AN5[1,2]+AN5[2,2]) ) *100
AN6 <- anova(lm(std ~ vtQC, data=CES[CES$quebec==1 & CES$year==2008,])) ; var08 <- (AN6[1,2] / (AN6[1,2]+AN6[2,2]) ) *100
AN7 <- anova(lm(std ~ vtQC, data=CES[CES$quebec==1 & CES$year==2011,])) ; var11 <- (AN7[1,2] / (AN7[1,2]+AN7[2,2]) ) *100
var15 <- NA
byvt <- rbind(var92,var97,var00,var04,var06,var08,var11,var15)
AN1 ; AN2; AN3 ; AN4 ; AN5 ; AN6 ; AN7

AN1 <- anova(lm(std ~ inc, data=CES[CES$quebec==1 & CES$year==1992,])) ; var92 <- (AN1[1,2] / (AN1[1,2]+AN1[2,2]) ) *100
AN2 <- anova(lm(std ~ inc, data=CES[CES$quebec==1 & CES$year==1997,])) ; var97 <- (AN2[1,2] / (AN2[1,2]+AN2[2,2]) ) *100
AN3 <- anova(lm(std ~ inc, data=CES[CES$quebec==1 & CES$year==2000,])) ; var00 <- (AN3[1,2] / (AN3[1,2]+AN3[2,2]) ) *100
AN4 <- anova(lm(std ~ inc, data=CES[CES$quebec==1 & CES$year==2004,])) ; var04 <- (AN4[1,2] / (AN4[1,2]+AN4[2,2]) ) *100
AN5 <- anova(lm(std ~ inc, data=CES[CES$quebec==1 & CES$year==2006,])) ; var06 <- (AN5[1,2] / (AN5[1,2]+AN5[2,2]) ) *100
AN6 <- anova(lm(std ~ inc, data=CES[CES$quebec==1 & CES$year==2008,])) ; var08 <- (AN6[1,2] / (AN6[1,2]+AN6[2,2]) ) *100
AN7 <- anova(lm(std ~ inc, data=CES[CES$quebec==1 & CES$year==2011,])) ; var11 <- (AN7[1,2] / (AN7[1,2]+AN7[2,2]) ) *100
var15 <- NA
byinc <- rbind(var92,var97,var00,var04,var06,var08,var11,var15)
AN1 ; AN2; AN3 ; AN4 ; AN5 ; AN6 ; AN7

varexQC <- cbind(bypid,byvt,byinc)
colnames(varexQC) <- c("pid","vote","inc")
varexQC <- as.data.frame(varexQC)
varexQC$year <- c(1992,1997,2000,2004,2006,2008,2011,2015)

pdf("figure1_std.pdf", width = 8, height = 8) 
par(mfrow=c(2,2),mai = c(.5, .9, 0.2, 0.2))
plot(aggregate(CES$std[CES$quebec==0],by=list(CES$year[CES$quebec==0]),FUN="mean",na.rm=T), type="l", lwd=4, ylim=c(.3,1),xlim=c(1992,2015),ann=F,axes=F)
axis(2, las=1, cex.axis=.8)
axis(1,at=c(1992,1997,2000,2004,2006,2008,2011,2015), labels=c("'92","'97","'00","'04","'06","'08","'11","'15"), cex.axis=.8)
title(ylab="Unweighted Mean (0-1)")
title(xlab=" ")
title("ROC")
plot(aggregate(CES$std[CES$quebec==1],by=list(CES$year[CES$quebec==1]),FUN="mean",na.rm=T), type="l", lwd=4, ylim=c(.3,1),xlim=c(1992,2015),ann=F,axes=F)
axis(2, las=1, cex.axis=.8)
axis(1,at=c(1992,1997,2000,2004,2006,2008,2011,2015), labels=c("'92","'97","'00","'04","'06","'08","'11","'15"), cex.axis=.8)
title(ylab=" ")
title(xlab=" ")
title("QC")
plot(varexROC$year,varexROC$pid,type="l",ann=F,axes=F,col="black",lwd=4, lty=2, ylim=c(0,12),xlim=c(1992,2015))
lines(varexROC$year,varexROC$vote,col="black",lwd=4)
lines(varexROC$year,varexROC$inc,col="gray",lwd=4)
axis(2, las=1, cex.axis=.8)
axis(1,at=c(1992,1997,2000,2004,2006,2008,2011,2015), labels=c("'92","'97","'00","'04","'06","'08","'11","'15"), cex.axis=.8)
title(ylab="% Variance Explained")
title(xlab="Election Year")
text(1992,9,"Income",pos=4,col="gray")
text(1992,10,"Party ID (dashed)",pos=4,col="black")
text(1992,11,"Vote (solid)",pos=4,col="black")
plot(varexQC$year,varexQC$pid,type="l",ann=F,axes=F,col="black",lwd=4, lty=2, ylim=c(0,12),xlim=c(1992,2015))
lines(varexQC$year,varexQC$vote,col="black",lwd=4)
lines(varexQC$year,varexQC$inc,col="gray",lwd=4)
axis(2, las=1, cex.axis=.8)
axis(1,at=c(1992,1997,2000,2004,2006,2008,2011,2015), labels=c("'92","'97","'00","'04","'06","'08","'11","'15"), cex.axis=.8)
title(ylab="% Variance Explained")
title(xlab="Election Year")
text(1992,9,"Income",pos=4,col="gray")
text(1992,10,"Party ID (dashed)",pos=4,col="black")
text(1992,11,"Vote (solid)",pos=4,col="black")
dev.off()

#REDUCE GAP

AN1 <- anova(lm(gap ~ pidROC, data=CES[CES$quebec!=1 & CES$year==1992,])) ; var92 <- (AN1[1,2] / (AN1[1,2]+AN1[2,2]) ) *100
AN2 <- anova(lm(gap ~ pidROC, data=CES[CES$quebec!=1 & CES$year==1997,])) ; var97 <- (AN2[1,2] / (AN2[1,2]+AN2[2,2]) ) *100
AN3 <- anova(lm(gap ~ pidROC, data=CES[CES$quebec!=1 & CES$year==2000,])) ; var00 <- (AN3[1,2] / (AN3[1,2]+AN3[2,2]) ) *100
AN4 <- anova(lm(gap ~ pidROC, data=CES[CES$quebec!=1 & CES$year==2004,])) ; var04 <- (AN4[1,2] / (AN4[1,2]+AN4[2,2]) ) *100
AN5 <- anova(lm(gap ~ pidROC, data=CES[CES$quebec!=1 & CES$year==2006,])) ; var06 <- (AN5[1,2] / (AN5[1,2]+AN5[2,2]) ) *100
AN6 <- anova(lm(gap ~ pidROC, data=CES[CES$quebec!=1 & CES$year==2008,])) ; var08 <- (AN6[1,2] / (AN6[1,2]+AN6[2,2]) ) *100
AN7 <- anova(lm(gap ~ pidROC, data=CES[CES$quebec!=1 & CES$year==2011,])) ; var11 <- (AN7[1,2] / (AN7[1,2]+AN7[2,2]) ) *100
AN8 <- anova(lm(gap ~ pidROC, data=CES[CES$quebec!=1 & CES$year==2015,])) ; var15 <- (AN8[1,2] / (AN8[1,2]+AN8[2,2]) ) *100
bypid <- rbind(var92,var97,var00,var04,var06,var08,var11,var15)
AN1 ; AN2; AN3 ; AN4 ; AN5 ; AN6 ; AN7 ; AN8

AN1 <- anova(lm(gap ~ vtROC, data=CES[CES$quebec!=1 & CES$year==1992,])) ; var92 <- (AN1[1,2] / (AN1[1,2]+AN1[2,2]) ) *100
AN2 <- anova(lm(gap ~ vtROC, data=CES[CES$quebec!=1 & CES$year==1997,])) ; var97 <- (AN2[1,2] / (AN2[1,2]+AN2[2,2]) ) *100
AN3 <- anova(lm(gap ~ vtROC, data=CES[CES$quebec!=1 & CES$year==2000,])) ; var00 <- (AN3[1,2] / (AN3[1,2]+AN3[2,2]) ) *100
AN4 <- anova(lm(gap ~ vtROC, data=CES[CES$quebec!=1 & CES$year==2004,])) ; var04 <- (AN4[1,2] / (AN4[1,2]+AN4[2,2]) ) *100
AN5 <- anova(lm(gap ~ vtROC, data=CES[CES$quebec!=1 & CES$year==2006,])) ; var06 <- (AN5[1,2] / (AN5[1,2]+AN5[2,2]) ) *100
AN6 <- anova(lm(gap ~ vtROC, data=CES[CES$quebec!=1 & CES$year==2008,])) ; var08 <- (AN6[1,2] / (AN6[1,2]+AN6[2,2]) ) *100
AN7 <- anova(lm(gap ~ vtROC, data=CES[CES$quebec!=1 & CES$year==2011,])) ; var11 <- (AN7[1,2] / (AN7[1,2]+AN7[2,2]) ) *100
AN8 <- anova(lm(gap ~ vtROC, data=CES[CES$quebec!=1 & CES$year==2015,])) ; var15 <- (AN8[1,2] / (AN8[1,2]+AN8[2,2]) ) *100
byvt <- rbind(var92,var97,var00,var04,var06,var08,var11,var15)
AN1 ; AN2; AN3 ; AN4 ; AN5 ; AN6 ; AN7 ; AN8

AN1 <- anova(lm(gap ~ inc, data=CES[CES$quebec!=1 & CES$year==1992,])) ; var92 <- (AN1[1,2] / (AN1[1,2]+AN1[2,2]) ) *100
AN2 <- anova(lm(gap ~ inc, data=CES[CES$quebec!=1 & CES$year==1997,])) ; var97 <- (AN2[1,2] / (AN2[1,2]+AN2[2,2]) ) *100
AN3 <- anova(lm(gap ~ inc, data=CES[CES$quebec!=1 & CES$year==2000,])) ; var00 <- (AN3[1,2] / (AN3[1,2]+AN3[2,2]) ) *100
AN4 <- anova(lm(gap ~ inc, data=CES[CES$quebec!=1 & CES$year==2004,])) ; var04 <- (AN4[1,2] / (AN4[1,2]+AN4[2,2]) ) *100
AN5 <- anova(lm(gap ~ inc, data=CES[CES$quebec!=1 & CES$year==2006,])) ; var06 <- (AN5[1,2] / (AN5[1,2]+AN5[2,2]) ) *100
AN6 <- anova(lm(gap ~ inc, data=CES[CES$quebec!=1 & CES$year==2008,])) ; var08 <- (AN6[1,2] / (AN6[1,2]+AN6[2,2]) ) *100
AN7 <- anova(lm(gap ~ inc, data=CES[CES$quebec!=1 & CES$year==2011,])) ; var11 <- (AN7[1,2] / (AN7[1,2]+AN7[2,2]) ) *100
AN7 <- anova(lm(gap ~ inc, data=CES[CES$quebec!=1 & CES$year==2015,])) ; var15 <- (AN8[1,2] / (AN8[1,2]+AN8[2,2]) ) *100
byinc <- rbind(var92,var97,var00,var04,var06,var08,var11,var15)
AN1 ; AN2; AN3 ; AN4 ; AN5 ; AN6 ; AN7 ; AN8

varexROC <- cbind(bypid,byvt,byinc)
colnames(varexROC) <- c("pid","vote","inc")
varexROC <- as.data.frame(varexROC)
varexROC$year <- c(1992,1997,2000,2004,2006,2008,2011,2015)

AN1 <- anova(lm(gap ~ pidQC, data=CES[CES$quebec==1 & CES$year==1992,])) ; var92 <- (AN1[1,2] / (AN1[1,2]+AN1[2,2]) ) *100
AN2 <- anova(lm(gap ~ pidQC, data=CES[CES$quebec==1 & CES$year==1997,])) ; var97 <- (AN2[1,2] / (AN2[1,2]+AN2[2,2]) ) *100
AN3 <- anova(lm(gap ~ pidQC, data=CES[CES$quebec==1 & CES$year==2000,])) ; var00 <- (AN3[1,2] / (AN3[1,2]+AN3[2,2]) ) *100
AN4 <- anova(lm(gap ~ pidQC, data=CES[CES$quebec==1 & CES$year==2004,])) ; var04 <- (AN4[1,2] / (AN4[1,2]+AN4[2,2]) ) *100
AN5 <- anova(lm(gap ~ pidQC, data=CES[CES$quebec==1 & CES$year==2006,])) ; var06 <- (AN5[1,2] / (AN5[1,2]+AN5[2,2]) ) *100
AN6 <- anova(lm(gap ~ pidQC, data=CES[CES$quebec==1 & CES$year==2008,])) ; var08 <- (AN6[1,2] / (AN6[1,2]+AN6[2,2]) ) *100
AN7 <- anova(lm(gap ~ pidQC, data=CES[CES$quebec==1 & CES$year==2011,])) ; var11 <- (AN7[1,2] / (AN7[1,2]+AN7[2,2]) ) *100
AN8 <- anova(lm(gap ~ pidQC, data=CES[CES$quebec==1 & CES$year==2015,])) ; var15 <- (AN8[1,2] / (AN8[1,2]+AN8[2,2]) ) *100
bypid <- rbind(var92,var97,var00,var04,var06,var08,var11,var15)
AN1 ; AN2; AN3 ; AN4 ; AN5 ; AN6 ; AN7 ; AN8

AN1 <- anova(lm(gap ~ vtQC, data=CES[CES$quebec==1 & CES$year==1992,])) ; var92 <- (AN1[1,2] / (AN1[1,2]+AN1[2,2]) ) *100
AN2 <- anova(lm(gap ~ vtQC, data=CES[CES$quebec==1 & CES$year==1997,])) ; var97 <- (AN2[1,2] / (AN2[1,2]+AN2[2,2]) ) *100
AN3 <- anova(lm(gap ~ vtQC, data=CES[CES$quebec==1 & CES$year==2000,])) ; var00 <- (AN3[1,2] / (AN3[1,2]+AN3[2,2]) ) *100
AN4 <- anova(lm(gap ~ vtQC, data=CES[CES$quebec==1 & CES$year==2004,])) ; var04 <- (AN4[1,2] / (AN4[1,2]+AN4[2,2]) ) *100
AN5 <- anova(lm(gap ~ vtQC, data=CES[CES$quebec==1 & CES$year==2006,])) ; var06 <- (AN5[1,2] / (AN5[1,2]+AN5[2,2]) ) *100
AN6 <- anova(lm(gap ~ vtQC, data=CES[CES$quebec==1 & CES$year==2008,])) ; var08 <- (AN6[1,2] / (AN6[1,2]+AN6[2,2]) ) *100
AN7 <- anova(lm(gap ~ vtQC, data=CES[CES$quebec==1 & CES$year==2011,])) ; var11 <- (AN7[1,2] / (AN7[1,2]+AN7[2,2]) ) *100
AN8 <- anova(lm(gap ~ vtQC, data=CES[CES$quebec==1 & CES$year==2015,])) ; var15 <- (AN8[1,2] / (AN8[1,2]+AN8[2,2]) ) *100
byvt <- rbind(var92,var97,var00,var04,var06,var08,var11,var15)
AN1 ; AN2; AN3 ; AN4 ; AN5 ; AN6 ; AN7 ; AN8

AN1 <- anova(lm(gap ~ inc, data=CES[CES$quebec==1 & CES$year==1992,])) ; var92 <- (AN1[1,2] / (AN1[1,2]+AN1[2,2]) ) *100
AN2 <- anova(lm(gap ~ inc, data=CES[CES$quebec==1 & CES$year==1997,])) ; var97 <- (AN2[1,2] / (AN2[1,2]+AN2[2,2]) ) *100
AN3 <- anova(lm(gap ~ inc, data=CES[CES$quebec==1 & CES$year==2000,])) ; var00 <- (AN3[1,2] / (AN3[1,2]+AN3[2,2]) ) *100
AN4 <- anova(lm(gap ~ inc, data=CES[CES$quebec==1 & CES$year==2004,])) ; var04 <- (AN4[1,2] / (AN4[1,2]+AN4[2,2]) ) *100
AN5 <- anova(lm(gap ~ inc, data=CES[CES$quebec==1 & CES$year==2006,])) ; var06 <- (AN5[1,2] / (AN5[1,2]+AN5[2,2]) ) *100
AN6 <- anova(lm(gap ~ inc, data=CES[CES$quebec==1 & CES$year==2008,])) ; var08 <- (AN6[1,2] / (AN6[1,2]+AN6[2,2]) ) *100
AN7 <- anova(lm(gap ~ inc, data=CES[CES$quebec==1 & CES$year==2011,])) ; var11 <- (AN7[1,2] / (AN7[1,2]+AN7[2,2]) ) *100
AN7 <- anova(lm(gap ~ inc, data=CES[CES$quebec==1 & CES$year==2015,])) ; var15 <- (AN8[1,2] / (AN8[1,2]+AN8[2,2]) ) *100
byinc <- rbind(var92,var97,var00,var04,var06,var08,var11,var15)
AN1 ; AN2; AN3 ; AN4 ; AN5 ; AN6 ; AN7 ; AN8

varexQC <- cbind(bypid,byvt,byinc)
colnames(varexQC) <- c("pid","vote","inc")
varexQC <- as.data.frame(varexQC)
varexQC$year <- c(1992,1997,2000,2004,2006,2008,2011,2015)

pdf("figure2_gap.pdf", width = 8, height = 8) 
par(mfrow=c(2,2),mai = c(.5, 0.9, 0.2, 0.2))
plot(aggregate(CES$gap[CES$quebec!=1],by=list(CES$year[CES$quebec!=1]),FUN="mean",na.rm=T), type="l", lwd=4, ylim=c(.6,1),xlim=c(1992,2015),ann=F,axes=F)
axis(2, las=1, cex.axis=.8)
axis(1,at=c(1992,1997,2000,2004,2006,2008,2011,2015), labels=c("'92","'97","'00","'04","'06","'08","'11","'15"), cex.axis=.8)
title(ylab="Unweighted Mean (0-1)")
title(xlab=" ")
title("ROC")
plot(aggregate(CES$gap[CES$quebec==1],by=list(CES$year[CES$quebec==1]),FUN="mean",na.rm=T), type="l", lwd=4, ylim=c(.6,1),xlim=c(1992,2015),ann=F,axes=F)
axis(2, las=1, cex.axis=.8)
axis(1,at=c(1992,1997,2000,2004,2006,2008,2011,2015), labels=c("'92","'97","'00","'04","'06","'08","'11","'15"), cex.axis=.8)
title(ylab=" ")
title(xlab=" ")
title("QC")
plot(varexROC$year,varexROC$pid,type="l",ann=F,axes=F,col="black",lwd=4,lty=2, ylim=c(0,12),xlim=c(1992,2015))
lines(varexROC$year,varexROC$vote,col="black",lwd=4)
lines(varexROC$year,varexROC$inc,col="gray",lwd=4)
axis(2, las=1, cex.axis=.8)
axis(1,at=c(1992,1997,2000,2004,2006,2008,2011,2015), labels=c("'92","'97","'00","'04","'06","'08","'11","'15"), cex.axis=.8)
title(ylab="% Variance Explained")
title(xlab="Election Year")
text(1992,9,"Income",pos=4,col="gray")
text(1992,10,"Party ID (dashed)",pos=4,col="black")
text(1992,11,"Vote (solid)",pos=4,col="black")
plot(varexQC$year,varexQC$pid,type="l",ann=F,axes=F,col="black",lwd=4, lty=2,ylim=c(0,12),xlim=c(1992,2015))
lines(varexQC$year,varexQC$vote,col="black",lwd=4)
lines(varexQC$year,varexQC$inc,col="gray",lwd=4)
axis(2, las=1, cex.axis=.8)
axis(1,at=c(1992,1997,2000,2004,2006,2008,2011,2015), labels=c("'92","'97","'00","'04","'06","'08","'11","'15"), cex.axis=.8)
title(ylab=" ")
title(xlab="Election Year")
dev.off()

#GET AHEAD

AN2 <- anova(lm(get ~ pidROC, data=CES[CES$quebec!=1 & CES$year==1997,])) ; var97 <- (AN2[1,2] / (AN2[1,2]+AN2[2,2]) ) *100
AN3 <- anova(lm(get ~ pidROC, data=CES[CES$quebec!=1 & CES$year==2000,])) ; var00 <- (AN3[1,2] / (AN3[1,2]+AN3[2,2]) ) *100
AN4 <- anova(lm(get ~ pidROC, data=CES[CES$quebec!=1 & CES$year==2004,])) ; var04 <- (AN4[1,2] / (AN4[1,2]+AN4[2,2]) ) *100
AN5 <- anova(lm(get ~ pidROC, data=CES[CES$quebec!=1 & CES$year==2006,])) ; var06 <- (AN5[1,2] / (AN5[1,2]+AN5[2,2]) ) *100
AN6 <- anova(lm(get ~ pidROC, data=CES[CES$quebec!=1 & CES$year==2008,])) ; var08 <- (AN6[1,2] / (AN6[1,2]+AN6[2,2]) ) *100
AN7 <- anova(lm(get ~ pidROC, data=CES[CES$quebec!=1 & CES$year==2011,])) ; var11 <- (AN7[1,2] / (AN7[1,2]+AN7[2,2]) ) *100
AN8 <- anova(lm(get ~ pidROC, data=CES[CES$quebec!=1 & CES$year==2015,])) ; var15 <- (AN8[1,2] / (AN8[1,2]+AN8[2,2]) ) *100
var92 <- NA
bypid <- rbind(var92,var97,var00,var04,var06,var08,var11,var15)
AN2; AN3 ; AN4 ; AN5 ; AN6 ; AN7 ; AN8

AN2 <- anova(lm(get ~ vtROC, data=CES[CES$quebec!=1 & CES$year==1997,])) ; var97 <- (AN2[1,2] / (AN2[1,2]+AN2[2,2]) ) *100
AN3 <- anova(lm(get ~ vtROC, data=CES[CES$quebec!=1 & CES$year==2000,])) ; var00 <- (AN3[1,2] / (AN3[1,2]+AN3[2,2]) ) *100
AN4 <- anova(lm(get ~ vtROC, data=CES[CES$quebec!=1 & CES$year==2004,])) ; var04 <- (AN4[1,2] / (AN4[1,2]+AN4[2,2]) ) *100
AN5 <- anova(lm(get ~ vtROC, data=CES[CES$quebec!=1 & CES$year==2006,])) ; var06 <- (AN5[1,2] / (AN5[1,2]+AN5[2,2]) ) *100
AN6 <- anova(lm(get ~ vtROC, data=CES[CES$quebec!=1 & CES$year==2008,])) ; var08 <- (AN6[1,2] / (AN6[1,2]+AN6[2,2]) ) *100
AN7 <- anova(lm(get ~ vtROC, data=CES[CES$quebec!=1 & CES$year==2011,])) ; var11 <- (AN7[1,2] / (AN7[1,2]+AN7[2,2]) ) *100
AN8 <- anova(lm(get ~ vtROC, data=CES[CES$quebec!=1 & CES$year==2015,])) ; var15 <- (AN8[1,2] / (AN8[1,2]+AN8[2,2]) ) *100
var92 <- NA
byvt <- rbind(var92,var97,var00,var04,var06,var08,var11,var15)
AN2; AN3 ; AN4 ; AN5 ; AN6 ; AN7 ; AN8

AN2 <- anova(lm(get ~ inc, data=CES[CES$quebec!=1 & CES$year==1997,])) ; var97 <- (AN2[1,2] / (AN2[1,2]+AN2[2,2]) ) *100
AN3 <- anova(lm(get ~ inc, data=CES[CES$quebec!=1 & CES$year==2000,])) ; var00 <- (AN3[1,2] / (AN3[1,2]+AN3[2,2]) ) *100
AN4 <- anova(lm(get ~ inc, data=CES[CES$quebec!=1 & CES$year==2004,])) ; var04 <- (AN4[1,2] / (AN4[1,2]+AN4[2,2]) ) *100
AN5 <- anova(lm(get ~ inc, data=CES[CES$quebec!=1 & CES$year==2006,])) ; var06 <- (AN5[1,2] / (AN5[1,2]+AN5[2,2]) ) *100
AN6 <- anova(lm(get ~ inc, data=CES[CES$quebec!=1 & CES$year==2008,])) ; var08 <- (AN6[1,2] / (AN6[1,2]+AN6[2,2]) ) *100
AN7 <- anova(lm(get ~ inc, data=CES[CES$quebec!=1 & CES$year==2011,])) ; var11 <- (AN7[1,2] / (AN7[1,2]+AN7[2,2]) ) *100
AN7 <- anova(lm(get ~ inc, data=CES[CES$quebec!=1 & CES$year==2015,])) ; var15 <- (AN8[1,2] / (AN8[1,2]+AN8[2,2]) ) *100
var92 <- NA
byinc <- rbind(var92,var97,var00,var04,var06,var08,var11,var15)
AN2; AN3 ; AN4 ; AN5 ; AN6 ; AN7 ; AN8

varexROC <- cbind(bypid,byvt,byinc)
colnames(varexROC) <- c("pid","vote","inc")
varexROC <- as.data.frame(varexROC)
varexROC$year <- c(1992,1997,2000,2004,2006,2008,2011,2015)

AN2 <- anova(lm(get ~ pidQC, data=CES[CES$quebec==1 & CES$year==1997,])) ; var97 <- (AN2[1,2] / (AN2[1,2]+AN2[2,2]) ) *100
AN3 <- anova(lm(get ~ pidQC, data=CES[CES$quebec==1 & CES$year==2000,])) ; var00 <- (AN3[1,2] / (AN3[1,2]+AN3[2,2]) ) *100
AN4 <- anova(lm(get ~ pidQC, data=CES[CES$quebec==1 & CES$year==2004,])) ; var04 <- (AN4[1,2] / (AN4[1,2]+AN4[2,2]) ) *100
AN5 <- anova(lm(get ~ pidQC, data=CES[CES$quebec==1 & CES$year==2006,])) ; var06 <- (AN5[1,2] / (AN5[1,2]+AN5[2,2]) ) *100
AN6 <- anova(lm(get ~ pidQC, data=CES[CES$quebec==1 & CES$year==2008,])) ; var08 <- (AN6[1,2] / (AN6[1,2]+AN6[2,2]) ) *100
AN7 <- anova(lm(get ~ pidQC, data=CES[CES$quebec==1 & CES$year==2011,])) ; var11 <- (AN7[1,2] / (AN7[1,2]+AN7[2,2]) ) *100
AN8 <- anova(lm(get ~ pidQC, data=CES[CES$quebec==1 & CES$year==2015,])) ; var15 <- (AN8[1,2] / (AN8[1,2]+AN8[2,2]) ) *100
var92 <- NA
bypid <- rbind(var92,var97,var00,var04,var06,var08,var11,var15)
AN2; AN3 ; AN4 ; AN5 ; AN6 ; AN7 ; AN8

AN2 <- anova(lm(get ~ vtQC, data=CES[CES$quebec==1 & CES$year==1997,])) ; var97 <- (AN2[1,2] / (AN2[1,2]+AN2[2,2]) ) *100
AN3 <- anova(lm(get ~ vtQC, data=CES[CES$quebec==1 & CES$year==2000,])) ; var00 <- (AN3[1,2] / (AN3[1,2]+AN3[2,2]) ) *100
AN4 <- anova(lm(get ~ vtQC, data=CES[CES$quebec==1 & CES$year==2004,])) ; var04 <- (AN4[1,2] / (AN4[1,2]+AN4[2,2]) ) *100
AN5 <- anova(lm(get ~ vtQC, data=CES[CES$quebec==1 & CES$year==2006,])) ; var06 <- (AN5[1,2] / (AN5[1,2]+AN5[2,2]) ) *100
AN6 <- anova(lm(get ~ vtQC, data=CES[CES$quebec==1 & CES$year==2008,])) ; var08 <- (AN6[1,2] / (AN6[1,2]+AN6[2,2]) ) *100
AN7 <- anova(lm(get ~ vtQC, data=CES[CES$quebec==1 & CES$year==2011,])) ; var11 <- (AN7[1,2] / (AN7[1,2]+AN7[2,2]) ) *100
AN8 <- anova(lm(get ~ vtQC, data=CES[CES$quebec==1 & CES$year==2015,])) ; var15 <- (AN8[1,2] / (AN8[1,2]+AN8[2,2]) ) *100
var92 <- NA
byvt <- rbind(var92,var97,var00,var04,var06,var08,var11,var15)
AN2; AN3 ; AN4 ; AN5 ; AN6 ; AN7 ; AN8

AN2 <- anova(lm(get ~ inc, data=CES[CES$quebec==1 & CES$year==1997,])) ; var97 <- (AN2[1,2] / (AN2[1,2]+AN2[2,2]) ) *100
AN3 <- anova(lm(get ~ inc, data=CES[CES$quebec==1 & CES$year==2000,])) ; var00 <- (AN3[1,2] / (AN3[1,2]+AN3[2,2]) ) *100
AN4 <- anova(lm(get ~ inc, data=CES[CES$quebec==1 & CES$year==2004,])) ; var04 <- (AN4[1,2] / (AN4[1,2]+AN4[2,2]) ) *100
AN5 <- anova(lm(get ~ inc, data=CES[CES$quebec==1 & CES$year==2006,])) ; var06 <- (AN5[1,2] / (AN5[1,2]+AN5[2,2]) ) *100
AN6 <- anova(lm(get ~ inc, data=CES[CES$quebec==1 & CES$year==2008,])) ; var08 <- (AN6[1,2] / (AN6[1,2]+AN6[2,2]) ) *100
AN7 <- anova(lm(get ~ inc, data=CES[CES$quebec==1 & CES$year==2011,])) ; var11 <- (AN7[1,2] / (AN7[1,2]+AN7[2,2]) ) *100
AN7 <- anova(lm(get ~ inc, data=CES[CES$quebec==1 & CES$year==2015,])) ; var15 <- (AN8[1,2] / (AN8[1,2]+AN8[2,2]) ) *100
var92 <- NA
byinc <- rbind(var92,var97,var00,var04,var06,var08,var11,var15)
AN2; AN3 ; AN4 ; AN5 ; AN6 ; AN7 ; AN8

varexQC <- cbind(bypid,byvt,byinc)
colnames(varexQC) <- c("pid","vote","inc")
varexQC <- as.data.frame(varexQC)
varexQC$year <- c(1992,1997,2000,2004,2006,2008,2011,2015)

pdf("figure3_get.pdf", width = 8, height = 8) 
par(mfrow=c(2,2),mai = c(.5, 0.9, 0.2, 0.2))
plot(aggregate(CES$get[CES$quebec!=1],by=list(CES$year[CES$quebec!=1]),FUN="mean",na.rm=T), type="l", lwd=4, ylim=c(.3,.8),xlim=c(1992,2015),ann=F,axes=F)
axis(2, las=1, cex.axis=.8)
axis(1,at=c(1992,1997,2000,2004,2006,2008,2011,2015), labels=c("'92","'97","'00","'04","'06","'08","'11","'15"), cex.axis=.8)
title(ylab="Unweighted Mean (0-1)")
title(xlab=" ")
title("ROC")
plot(aggregate(CES$get[CES$quebec==1],by=list(CES$year[CES$quebec==1]),FUN="mean",na.rm=T), type="l", lwd=4, ylim=c(.3,.8),xlim=c(1992,2015),ann=F,axes=F)
axis(2, las=1, cex.axis=.8)
axis(1,at=c(1992,1997,2000,2004,2006,2008,2011,2015), labels=c("'92","'97","'00","'04","'06","'08","'11","'15"), cex.axis=.8)
title(ylab=" ")
title(xlab=" ")
title("QC")
plot(varexROC$year,varexROC$pid,type="l",ann=F,axes=F,col="black",lwd=4, lty=2,ylim=c(0,12),xlim=c(1992,2015))
lines(varexROC$year,varexROC$vote,col="black",lwd=4)
lines(varexROC$year,varexROC$inc,col="gray",lwd=4)
axis(2, las=1, cex.axis=.8)
axis(1,at=c(1992,1997,2000,2004,2006,2008,2011,2015), labels=c("'92","'97","'00","'04","'06","'08","'11","'15"), cex.axis=.8)
title(ylab="% Variance Explained")
title(xlab="Election Year")
text(1992,9,"Income",pos=4,col="gray")
text(1992,10,"Party ID (dashed)",pos=4,col="black")
text(1992,11,"Vote (solid)",pos=4,col="black")
plot(varexQC$year,varexQC$pid,type="l",ann=F,axes=F,col="black",lty=2, lwd=4, ylim=c(0,12),xlim=c(1992,2015))
lines(varexQC$year,varexQC$vote,col="black",lwd=4)
lines(varexQC$year,varexQC$inc,col="gray",lwd=4)
axis(2, las=1, cex.axis=.8)
axis(1,at=c(1992,1997,2000,2004,2006,2008,2011,2015), labels=c("'92","'97","'00","'04","'06","'08","'11","'15"), cex.axis=.8)
title(ylab=" ")
title(xlab="Election Year")
dev.off()

