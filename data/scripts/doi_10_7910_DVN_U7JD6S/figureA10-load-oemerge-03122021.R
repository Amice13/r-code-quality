### load libraries
library(dplyr)
library(plyr)

#### load main data set
#### source code: joint-panel-load-01212021.R
#load("~/Dropbox/PDparty/data/merged-PD-waves-01212021.Rdata")
setwd("~/Dropbox/PDparty/rcode/replication/")
load("merged-PD-waves-replication-only.Rdata")
dtam2 <- dtasub2
rm(dtasub2)

#### subset OE questions for coding
dta.sub <- dtam2[! dtam2$duration.W3 %in% c(NA),]
dta.sub.2 <- subset(dta.sub,select=c("CaseID.x","CaseID.y","Q4_1.W1","Q4_2.W1"))
#write.csv(dta.sub.2,file="~/Dropbox/PDparty/data/oe-party-wave1-completes-04082021.csv",row.names=F)
dta.sub.3 <- subset(dta.sub,select=c("CaseID.x","CaseID.y","Q4_1.W2","Q4_2.W2"))
#write.csv(dta.sub.3,file="~/Dropbox/PDparty/data/oe-party-wave2-completes-04082021.csv",row.names=F)
dta.sub.4 <- subset(dta.sub,select=c("CaseID.x","CaseID.y","Q4_1.W3","Q4_2.W3"))
#write.csv(dta.sub.4,file="~/Dropbox/PDparty/data/oe-party-wave3-completes-04082021.csv",row.names=F)

#### load NF OE coded spreadsheets

dta.oe.nf.wave1 <- read.csv("oe-party-wave1-completes-04082021.nf.csv")
dim(dta.oe.nf.wave1)
length(unique(dta.oe.nf.wave1$CaseID.x))
cn2 <- cn <- colnames(dta.oe.nf.wave1)
n <- length(cn)
cn2[5:n] <- paste(cn[5:n],".NF.W1",sep="")
colnames(dta.oe.nf.wave1) <- cn2

dta.oe.nf.wave2 <- read.csv("oe-party-wave2-completes-04082021.nf.csv")
dim(dta.oe.nf.wave2)
length(unique(dta.oe.nf.wave2$CaseID.x))
cn2 <- cn <- colnames(dta.oe.nf.wave2)
n <- length(cn)
cn2[5:n] <- paste(cn[5:n],".NF.W2",sep="")
colnames(dta.oe.nf.wave2) <- cn2

dta.oe.nf.wave3 <- read.csv("oe-party-wave3-completes-04082021.nf.csv")
dim(dta.oe.nf.wave3)
length(unique(dta.oe.nf.wave3$CaseID.x))
cn2 <- cn <- colnames(dta.oe.nf.wave3)
n <- length(cn)
cn2[5:n] <- paste(cn[5:n],".NF.W3",sep="")
colnames(dta.oe.nf.wave3) <- cn2

dta.oe.nf.m1 <- full_join(dta.oe.nf.wave1,dta.oe.nf.wave2,by="CaseID.x")
dim(dta.oe.nf.m1)

dta.oe.nf.m2 <- full_join(dta.oe.nf.m1,dta.oe.nf.wave3,by="CaseID.x")
dim(dta.oe.nf.m2)


###

dta.oe.gr <- read.csv("spanish_english_coding_4_5_19_GR.csv")

dta.oe2 <- dta.oe.gr[! dta.oe.gr$mno %in% c(NA),]
cnt <- cn2 <- cn <- colnames(dta.oe2)
n <- length(cn2)
cn2[3:n] <- paste(cn[3:n],".GR.W3",sep="")
colnames(dta.oe2) <- cn2

#dta.oe2$CaseID.y <- dta.oe2$CaseID

dta.oe.cm <- read.csv("his-asn-party-10212019CM.csv")
cn2 <- cn <- colnames(dta.oe.cm)
cnt[which(! cnt %in% cn)]
cn[which(! cn %in% cnt)]

n <- length(cn2)
cn2[3:n] <- paste(cn[3:n],".CM.W3",sep="")
colnames(dta.oe.cm) <- cn2

dta.oe.w3 <- full_join(dta.oe.cm,dta.oe2,by="mno")

### wave 1 English
dta.ppw1.eng <- read.csv("PDSurvey-wave1-party-Coding-062216EZ.csv")
dta.ppw1.spn <- read.csv("Spanish codingGR03182019.csv")
colnames(dta.ppw1.eng)[which(! colnames(dta.ppw1.eng) %in% colnames(dta.ppw1.spn))]

dta.ppw1.all <- rbind.fill(dta.ppw1.eng,dta.ppw1.spn)
summary(dta.ppw1.all$CaseID)
length(unique(dta.ppw1.all$CaseID))

cn2 <- cn <- colnames(dta.ppw1.all)
n <- length(cn)
cn2[2:n] <- paste(cn[2:n],".EZ.W1",sep="")
colnames(dta.ppw1.all) <- cn2
dta.ppw1.all$CaseID.x <- dta.ppw1.all$CaseID
dta.ppw1.all$CaseID <- NULL

dtam3 <- left_join(dtam2,dta.ppw1.all,by="CaseID.x")

dtam.31 <- left_join(dtam3,dta.oe.w3,by="mno")
dtam.31$CaseID.x <- dtam.31$CaseID.x.x
dim(dtam.31)
dtam4 <- left_join(dtam.31,dta.oe.nf.m2,by="CaseID.x")
dim(dtam4)

#####

dtam4$IDEOLOGYD.J.W3 <- apply(cbind(dtam4$IDEOLOGYD.CM.W3,dtam4$IDEOLOGYD.GR.W3,dtam4$IDEOLOGYD.NF.W3),1,mean)
dtam4$IDEOLOGYR.J.W3 <- apply(cbind(dtam4$IDEOLOGYR.CM.W3,dtam4$IDEOLOGYR.GR.W3,dtam4$IDEOLOGYR.NF.W3),1,mean)

dtam4$IDEOLOGYD.J.W1 <- apply(cbind(dtam4$IDEOLOGYD.EZ.W1,dtam4$IDEOLOGYD.NF.W1),1,mean)
dtam4$IDEOLOGYD.J.W2 <- apply(cbind(dtam4$IDEOLOGYD.ES.W2,dtam4$IDEOLOGYD.NF.W2),1,mean)

dtam4$IDEOLOGYR.J.W1 <- apply(cbind(dtam4$IDEOLOGYR.EZ.W1,dtam4$IDEOLOGYR.NF.W1),1,mean)
dtam4$IDEOLOGYR.J.W2 <- apply(cbind(dtam4$IDEOLOGYR.ES.W2,dtam4$IDEOLOGYR.NF.W2),1,mean)

dtam4$SOCGROUPSD.J.W3 <- apply(cbind(dtam4$SOCGROUPSD.CM.W3,dtam4$SOCGROUPSD.GR.W3,dtam4$SOCGROUPSD.NF.W3),1,mean)
dtam4$ETHGROUPSD.J.W3 <- apply(cbind(dtam4$ETHGROUPSD.CM.W3,dtam4$ETHGROUPSD.GR.W3,dtam4$ETHGROUPSD.NF.W3),1,mean)

dtam4$SOCGROUPSR.J.W3 <- apply(cbind(dtam4$SOCGROUPSR.CM.W3,dtam4$SOCGROUPSR.GR.W3,dtam4$SOCGROUPSR.NF.W3),1,mean)
dtam4$ETHGROUPSR.J.W3 <- apply(cbind(dtam4$ETHGROUPSR.CM.W3,dtam4$ETHGROUPSR.GR.W3,dtam4$ETHGROUPSR.NF.W3),1,mean)

dtam4$NEGATIVED.J.W3 <- apply(cbind(dtam4$NEGATIVED.CM.W3,dtam4$NEGATIVED.GR.W3,dtam4$NEGATIVED.NF.W3),1,mean)
dtam4$NEGATIVER.J.W3 <- apply(cbind(dtam4$NEGATIVER.CM.W3,dtam4$NEGATIVER.GR.W3,dtam4$NEGATIVER.NF.W3),1,mean)
dtam4$POSITIVED.J.W3 <- apply(cbind(dtam4$POSITIVED.CM.W3,dtam4$POSITIVED.GR.W3,dtam4$POSITIVED.NF.W3),1,mean)
dtam4$POSITIVER.J.W3 <- apply(cbind(dtam4$POSITIVER.CM.W3,dtam4$POSITIVER.GR.W3,dtam4$POSITIVER.NF.W3),1,mean)

dtam4$SOCGROUPSD.J.W1 <- apply(cbind(dtam4$SOCGROUPSD.EZ.W1,dtam4$SOCGROUPSD.NF.W1),1,mean)
dtam4$SOCGROUPSD.J.W2 <- apply(cbind(dtam4$SOCGROUPSD.ES.W2,dtam4$SOCGROUPSD.NF.W2),1,mean)

dtam4$SOCGROUPSR.J.W1 <- apply(cbind(dtam4$SOCGROUPSR.EZ.W1,dtam4$SOCGROUPSR.NF.W1),1,mean)
dtam4$SOCGROUPSR.J.W2 <- apply(cbind(dtam4$SOCGROUPSR.ES.W2,dtam4$SOCGROUPSR.NF.W2),1,mean)

dtam4$ETHGROUPSD.J.W1 <- apply(cbind(dtam4$ETHGROUPSD.EZ.W1,dtam4$ETHGROUPSD.NF.W1),1,mean)
dtam4$ETHGROUPSD.J.W2 <- apply(cbind(dtam4$ETHGROUPSD.ES.W2,dtam4$ETHGROUPSD.NF.W2),1,mean)

dtam4$ETHGROUPSR.J.W1 <- apply(cbind(dtam4$ETHGROUPSR.EZ.W1,dtam4$ETHGROUPSR.NF.W1),1,mean)
dtam4$ETHGROUPSR.J.W2 <- apply(cbind(dtam4$ETHGROUPSR.ES.W2,dtam4$ETHGROUPSR.NF.W2),1,mean)


dtam4$NEGATIVED.J.W1 <- apply(cbind(dtam4$NEGATIVED.EZ.W1,dtam4$NEGATIVED.NF.W1),1,mean)
dtam4$NEGATIVED.J.W2 <- apply(cbind(dtam4$NEGATIVED.ES.W2,dtam4$NEGATIVED.NF.W2),1,mean)

dtam4$NEGATIVER.J.W1 <- apply(cbind(dtam4$NEGATIVER.EZ.W1,dtam4$NEGATIVER.NF.W1),1,mean)
dtam4$NEGATIVER.J.W2 <- apply(cbind(dtam4$NEGATIVER.ES.W2,dtam4$NEGATIVER.NF.W2),1,mean)

dtam4$POSITIVED.J.W1 <- apply(cbind(dtam4$POSITIVED.EZ.W1,dtam4$POSITIVED.NF.W1),1,mean)
dtam4$POSITIVED.J.W2 <- apply(cbind(dtam4$POSITIVED.ES.W2,dtam4$POSITIVED.NF.W2),1,mean)

dtam4$POSITIVER.J.W1 <- apply(cbind(dtam4$POSITIVER.EZ.W1,dtam4$POSITIVER.NF.W1),1,mean)
dtam4$POSITIVER.J.W2 <- apply(cbind(dtam4$POSITIVER.ES.W2,dtam4$POSITIVER.NF.W2),1,mean)

dta.wave3 <- dtam4[! dtam4$tm_start.W3 %in% c(NA),]
about.dems <- nchar(trimws(dta.wave3$Q4_1.W3.x))
about.gop <- nchar(trimws(dta.wave3$Q4_2.W3.x))

sum(about.gop==0 & about.dems==0)
dim(dta.wave3)

### subset
dtam4subd <- dtam4[! dtam4$NORESPONSED.CM.W3 %in% c(NA,1),]
dtam4subr <- dtam4[! dtam4$NORESPONSER.CM.W3 %in% c(NA,1),]

ideologyd <- c(mean(dtam4subd$IDEOLOGYD.J.W1,na.rm=T),
                mean(dtam4subd$IDEOLOGYD.J.W2 ,na.rm=T),
                mean(dtam4subd$IDEOLOGYD.J.W3,na.rm=T))

ideologyr <- c(mean(dtam4subr$IDEOLOGYR.J.W1,na.rm=T),
               mean(dtam4subr$IDEOLOGYR.J.W2 ,na.rm=T),
               mean(dtam4subr$IDEOLOGYR.J.W3,na.rm=T))

socgroupsd <- c(mean(dtam4subd$SOCGROUPSD.J.W1,na.rm=T),
        mean(dtam4subd$SOCGROUPSD.J.W2 ,na.rm=T),
        mean(dtam4subd$SOCGROUPSD.J.W3,na.rm=T))

ethgroupsd <- c(mean(dtam4subd$ETHGROUPSD.J.W1,na.rm=T),
               mean(dtam4subd$ETHGROUPSD.J.W2,na.rm=T),
               mean(dtam4subd$ETHGROUPSD.J.W3,na.rm=T))

negd <- c(mean(dtam4subd$NEGATIVED.J.W1,na.rm=T),
          mean(dtam4subd$NEGATIVED.J.W2,na.rm=T),
          mean(dtam4subd$NEGATIVED.J.W3,na.rm=T))

posd <- c(mean(dtam4subd$POSITIVED.J.W1,na.rm=T),
          mean(dtam4subd$POSITIVED.J.W2,na.rm=T),
          mean(dtam4subd$POSITIVED.J.W3,na.rm=T))

socgroupsr <- c(mean(dtam4subr$SOCGROUPSR.J.W1,na.rm=T),
                mean(dtam4subr$SOCGROUPSR.J.W2,na.rm=T),
                mean(dtam4subr$SOCGROUPSR.J.W3,na.rm=T))

ethgroupsr <- c(mean(dtam4subr$ETHGROUPSR.J.W1,na.rm=T),
                mean(dtam4subr$ETHGROUPSR.J.W2,na.rm=T),
                mean(dtam4subr$ETHGROUPSR.J.W3,na.rm=T))

posr <- c(mean(dtam4subr$POSITIVER.J.W1,na.rm=T),
          mean(dtam4subr$POSITIVER.J.W2,na.rm=T),
          mean(dtam4subr$POSITIVER.J.W3,na.rm=T))

negr <- c(mean(dtam4subr$NEGATIVER.J.W1,na.rm=T),
          mean(dtam4subr$NEGATIVER.J.W2,na.rm=T),
          mean(dtam4subr$NEGATIVER.J.W3,na.rm=T))

#pdf("~/Dropbox/PDparty/figures/dems-oe-assessments-05032021.pdf",width=8)
tt <- rbind(socgroupsd,ethgroupsd,posd,negd)
colnames(tt) <- c("Spring 2016","Fall 2016","Fall 2018")
rownames(tt) <- c("Social Grps","Ethnic/Racial Grps","Positive","Negative")
bp1 <- barplot(t(tt),beside=T,ylab="Share",
               main="Assessments of Democrats",
               cex.main=1.8)
text(x=bp1[1,1]+.12,y=tt[1,1]+.025,"Spring 16")
text(x=bp1[2,1]+0.12,y=tt[1,2]+.025,"Fall 16")
text(x=bp1[3,1]+0.12,y=tt[1,3]+.025,"Fall 18")
#dev.off()

#pdf("~/Dropbox/PDparty/figures/gop-oe-assessments-05032021.pdf",width=8)
tt <- rbind(socgroupsr,ethgroupsr,posr,negr)
colnames(tt) <- c("Spring 2016","Fall 2016","Fall 2018")
rownames(tt) <- c("Social Grps","Ethnic/Racial Grps","Positive","Negative")
bp1 <- barplot(t(tt),beside=T,ylab="Share",
               main="Assessments of Republicans",
               cex.main=1.8)
text(x=bp1[1,1]+.12,y=tt[1,1]+.025,"Spring 16")
text(x=bp1[2,1]+0.12,y=tt[1,2]+.045,"Fall 16")
text(x=bp1[3,1]+0.12,y=tt[1,3]+.025,"Fall 18")
#dev.off()
