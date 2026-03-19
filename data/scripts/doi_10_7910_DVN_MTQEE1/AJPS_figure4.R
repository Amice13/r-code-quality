
#This file reproduces Figure 4 in manuscript.  It requires the Stata dataset 
#nationalcount.dta (saved as Stata v. 12).   

#setwd("/Users/bradfordjones/Dropbox/AJPS R&R/Dataverse files for AJPS/")

library(ggplot2)
library(foreign)



moa<-c(3, 6, 28, 58, 69, 70)
moayear<-c(2005, 2006, 2007, 2008, 2009, 2010)

moadata<-cbind(moa, moayear)


auditfo<-c(18, 52, 237, 385)
audittotal<-c(503, 1444, 2196, 2496)
audityear<-c(2008, 2009, 2010, 2011)
 
  
auditdata<-cbind(auditfo, audittotal, audityear)

removal<-c(165168, 211098, 240665, 246431, 280974, 319382, 359795, 391597, 382265, 387134)

removalyear<-c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011)

removaldata<-cbind(removal, removalyear)



natdata<-read.dta("nationalcount.dta")
attach(natdata)
par(mfrow=c(3,2), oma=c(1,2,2,2), mar=c(2, 5, 2, 2))

plot(count_immigrant~obs, pch=20, cex.axis=.75,  ylab="Monthly Counts (from Newsbank)",cex.lab=.75,  cex.main=.90, col="gray40", xaxt="n", type="l", xlab="", ylim=c(0,7000), main="References to the Illegal Immigration Issue")
axis(1, at=c(1, 13, 25, 37, 49, 61, 73, 85, 97, 109, 121, 133, 145, 157, 169, 181), labels=c("2000", "2001",
"2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), cex.axis=.55, las=2)

abline(v=25, lty=3, col="grey20")
abline(v=133, lty=3, col="grey20")
text(86, 6950, "Period of Study", cex=.55)




#par(new=TRUE)

#plot(count_group~obs, pch=20, cex.axis=.75, main="", ylab="",cex.lab=.75,  cex.main=.90, col="grey20", ylim=c(0,7000), xaxt="n", type="l", xlab="")


plot(count_group~obs, pch=20, cex.axis=.75,  ylab="Monthly Counts (from Newsbank)",cex.lab=.75,  cex.main=.90, col="gray40", xaxt="n", type="l", xlab="", ylim=c(0,1000), main="References to Ethnic/Racial Groups and Illegal Immigrant Status")
axis(1, at=c(1, 13, 25, 37, 49, 61, 73, 85, 97, 109, 121, 133, 145, 157, 169, 181), labels=c("2000", "2001",
"2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), cex.axis=.55, las=2)

par(new=TRUE)

plot(count_group2~obs, pch=20, cex.axis=.75, main="", ylab="",cex.lab=.75,  cex.main=.90, col="gray60",  xaxt="n", type="l", xlab="", ylim=c(0,1000), lty=3)
legend("topleft", c("Latino, Hispanic, Mexican-American", "Asian, Asian-American, Chinese, African, Middle Eastern"), fill=c("gray40", "gray60"), cex=.75, box.lty=0)
 
abline(v=25, lty=3, col="grey20")
abline(v=133, lty=3, col="grey20")
#text(86, 990, "Period of Study", cex=.55)



plot(removal~removalyear, data=removaldata, pch=20, cex.axis=.75,  ylab="Number of Removals",cex.lab=.75,  cex.main=.90, col="gray40",  type="l",  ylim=c(100000,400000), main="Forced Removals, FY 2002-2011", xlab="Year")


plot(moa~moayear, data=moadata, pch=20, cex.axis=.75,  ylab="Number of Jursidiction",cex.lab=.75,  cex.main=.90, col="gray40",  type="l", ylim=c(0,100), main="287(g) Jurisdictions, 2005-2010", xlab="Year")


plot(audittotal~audityear, data=auditdata, pch=20, cex.axis=.75,  ylab="Number of Audits",cex.lab=.75,  cex.main=.90, col="gray40", type="l", ylim=c(0,2500), main="ICE Workplace Audits, FY 2008-2011", xaxt="n", xlab="Year")
axis(1, at=c(2008, 2009, 2010, 2011), labels=c("2008", "2009", "2010", "2011"), cex.axis=.55, las=1)

par(new=TRUE)

plot(auditfo~audityear, data=auditdata, pch=20, cex.axis=.75,  ylab="",cex.lab=.75,  cex.main=.90, col="gray60",  type="l", lty=2, ylim=c(0,2500), main="", xaxt="n", xlab="")
legend("topleft", c("Total Audits", "Final Orders"), fill=c("gray40", "gray60"), cex=.75, box.lty=0)

title("News Media Attention and Enforcement Statistics", outer=TRUE)