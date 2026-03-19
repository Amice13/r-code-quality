### Dan Hopkins, Marc Meredith
### 1/05/2021
### Replication code for:

#### load libraries ----
library(tidyverse)
library(readxl)
library(lubridate)
library(xtable)

###### load/merge data sets ----
### load 2019 experimental data
load("replication-file-01052020.Rdata")

dta2tre <- dta2[dta2$treat_20 %in% c("Neighborhood","Self"),]
dta2con <- dta2[dta2$treat_20 %in% c("Control"),]

##### generate timing figure ----
rmat <- matrix(NA,max(dta2$DaysAppReturnedFmt,na.rm=T),5)
treat.vec <- control.vec <- c()
for(i in 1:max(dta2$DaysAppReturnedFmt,na.rm=T)){
  rmat[i,1] <- i
  rmat[i,2] <- sum(dta2tre$DaysAppReturnedFmt <= i,na.rm=T)
  rmat[i,3] <- sum(dta2con$DaysAppReturnedFmt <= i,na.rm=T)
  
  rmat[i,4] <- sum(dta2tre$DaysBallotReturnedFmt <= i,na.rm=T)
  rmat[i,5] <- sum(dta2con$DaysBallotReturnedFmt <= i,na.rm=T)
  
  treat.vec <- c(treat.vec,sum(dta2tre$DaysAppReturnedFmt <= i,na.rm=T))
  control.vec <- c(control.vec,sum(dta2con$DaysAppReturnedFmt <= i,na.rm=T))
}   

N.treated <- dim(dta2tre)[1]
N.control <- dim(dta2con)[1]

pdf("vote-by-mail-rate-time-8-11162020.pdf",
    width=8)
par(mfcol=c(2,1),mai=c(.5,1,1,1))
xmn <- 60
xmx <- 104
plot(rmat[,1],rmat[,2]/N.treated,pch=16,col="dark gray",
     ylim=c(0,.175),
     ylab="Share Reg. Voters",cex=.15,type="l",lwd=1,
     xlim=c(xmn,xmx),
     xlab="",main="Mail Applications Received",
     xaxt="n",cex.main=2.2)
par(new=T)
plot(rmat[,1],rmat[,3]/N.control,pch=16,col="black",
     ylim=c(0,.175),
     ylab="",cex=.15,type="l",lwd=1,xlim=c(xmn,xmx),xaxt="n",
     xlab="")
polygon(c(rmat[,1], rev(rmat[,1])), c(rmat[,2]/N.treated, rev(rmat[,3]/N.control)),
        col = "lightgray", border = NA)

axis(side=1,at=c(
  ymd("2020-05-01")-ymd("2020-03-10"),
  ymd("2020-05-10")-ymd("2020-03-10"),
  ymd("2020-05-20")-ymd("2020-03-10"),
  ymd("2020-06-01")-ymd("2020-03-10"),
  ymd("2020-06-10")-ymd("2020-03-10"),
  ymd("2020-06-20")-ymd("2020-03-10")),
  labels=c("5/1","5/10","5/20","6/1","6/10","6/20"),
  cex.lab=1.25)
abline(v=69)
abline(v=ymd("2020-06-02")-ymd("2020-03-10"),lty=2)
abline(v=ymd("2020-05-26")-ymd("2020-03-10"),lty=3)

text(y=.04,x=ymd("2020-05-18")-ymd("2020-03-10"),"Postcards \n Sent",cex=1.25)
text(y=.04,x=ymd("2020-06-02")-ymd("2020-03-10"),"Primary",cex=1.25)
text(y=.04,x=ymd("2020-05-26")-ymd("2020-03-10"),"Application \n Deadline",cex=1.25)

text(x=103,y=rmat[101,2]/dim(dta2tre)[1]+.005,"Postcard",
     col="dark gray")
text(x=103,y=rmat[101,3]/dim(dta2con)[1]-.005,"Control",
     col="black")

par(mai=c(.5,1,1,1))
plot(rmat[,1],rmat[,4]/dim(dta2tre)[1],pch=16,col="dark gray",ylim=c(0,.175),
     ylab="Share Reg. Voters",cex=.15,type="l",lwd=1,xlim=c(xmn,xmx),
     xlab="",main="Ballots Received",xaxt="n",
     cex.main=2.2)
par(new=T)
plot(rmat[,1],rmat[,5]/dim(dta2con)[1],pch=16,col="black",ylim=c(0,.175),
     ylab="",cex=.15,type="l",lwd=1,xlim=c(xmn,xmx),
     xlab="",xaxt="n",cex.main=2.2)
axis(side=1,at=c(
  ymd("2020-05-01")-ymd("2020-03-10"),
  ymd("2020-05-10")-ymd("2020-03-10"),
  ymd("2020-05-20")-ymd("2020-03-10"),
  ymd("2020-06-01")-ymd("2020-03-10"),
  ymd("2020-06-10")-ymd("2020-03-10"),
  ymd("2020-06-20")-ymd("2020-03-10")),
  labels=c("5/1","5/10","5/20","6/1","6/10","6/20"),
  cex.lab=1.25)

polygon(c(rmat[,1], rev(rmat[,1])), c(rmat[,4]/N.treated, rev(rmat[,5]/N.control)),
        col = "lightgray", border = NA)
abline(v=69)
abline(v=ymd("2020-06-02")-ymd("2020-03-10"),lty=2)
abline(v=ymd("2020-05-26")-ymd("2020-03-10"),lty=3)

text(y=.15,x=ymd("2020-05-18")-ymd("2020-03-10"),"Postcards \n Sent",cex=1.25)
text(y=.15,x=ymd("2020-06-02")-ymd("2020-03-10"),"Primary",cex=1.25)
text(y=.15,x=ymd("2020-05-26")-ymd("2020-03-10"),"Application \n Deadline",cex=1.25)

text(x=103,y=rmat[101,4]/dim(dta2tre)[1]+.005,"Postcard",
     col="dark gray")
text(x=103,y=rmat[101,5]/dim(dta2con)[1]-.005,"Control",
     col="black")

dev.off()

##### Results table ----

MM <- 24
rmat2 <- matrix(NA,MM,7)
rownames(rmat2) <- as.character(1:MM)

table.function <- function(x=dta2,i,rmat2=rmat2){
  
  dta2tre <- x[x$treat_20 %in% c("Neighborhood","Self"),]
  dta2con <- x[x$treat_20 %in% c("Control"),]
  
  ### recorded ballot
  rmat2[1,i] <- sum(dta2con$REQUESTED.MAIL.BALLOT==1)/dim(dta2con)[1]
  rmat2[2,i] <- sum(dta2tre$REQUESTED.MAIL.BALLOT==1)/dim(dta2tre)[1]
  tout <- t.test(dta2con$REQUESTED.MAIL.BALLOT,
                 dta2tre$REQUESTED.MAIL.BALLOT)
  rmat2[3,i] <- tout$p.value
  rownames(rmat2)[1:3] <- c("Requested Mail Ballot, Control",
                            "Requested Mail Ballot, Treated","P-value")
  
  rmat2[4,i] <- sum(dta2con$REQUESTED.NEVER.RETURNED==1)/dim(dta2con)[1]
  rmat2[5,i] <- sum(dta2tre$REQUESTED.NEVER.RETURNED==1)/dim(dta2tre)[1]
  tout <- t.test(dta2con$REQUESTED.NEVER.RETURNED,
                 dta2tre$REQUESTED.NEVER.RETURNED)
  rmat2[6,i] <- tout$p.value
  rownames(rmat2)[4:6] <- c("Requested-Not Returned, Control",
                            "Requested-Not Returned, Treated","P-value")
  
  rmat2[7,i] <- sum(dta2con$VOTED.MAIL.2020.PRIMARY==1)/dim(dta2con)[1]
  rmat2[8,i] <- sum(dta2tre$VOTED.MAIL.2020.PRIMARY==1)/dim(dta2tre)[1]
  tout <- t.test(dta2con$VOTED.MAIL.2020.PRIMARY,
                 dta2tre$VOTED.MAIL.2020.PRIMARY)
  rmat2[9,i] <- tout$p.value
  rownames(rmat2)[7:9] <- c("Voted by Mail, Control",
                            "Voted by Mail, Treated","P-value")
  
  rmat2[10,i] <- sum(dta2con$MAIL.BALLOT.RECEIVED.BEFORE==1)/dim(dta2con)[1]
  rmat2[11,i] <- sum(dta2tre$MAIL.BALLOT.RECEIVED.BEFORE==1)/dim(dta2tre)[1]
  tout <- t.test(dta2con$MAIL.BALLOT.RECEIVED.BEFORE,
                 dta2tre$MAIL.BALLOT.RECEIVED.BEFORE)
  rmat2[12,i] <- tout$p.value
  rownames(rmat2)[10:12] <- c("Ballot Received Before, Control",
                              "Ballot Received Before, Treated","P-value")
  
  rmat2[13,i] <- sum(dta2con$MAIL.BALLOT.RECEIVED.AFTER==1)/dim(dta2con)[1]
  rmat2[14,i] <- sum(dta2tre$MAIL.BALLOT.RECEIVED.AFTER==1)/dim(dta2tre)[1]
  tout <- t.test(dta2con$MAIL.BALLOT.RECEIVED.AFTER,
                 dta2tre$MAIL.BALLOT.RECEIVED.AFTER)
  rmat2[15,i] <- tout$p.value
  rownames(rmat2)[13:15] <- c("Ballot Received After, Control",
                              "Ballot Received After, Treated","P-value")
  
  rmat2[16,i] <- sum(dta2con$VOTED.INPERSON.2020.PRIMARY==1)/dim(dta2con)[1]
  rmat2[17,i] <- sum(dta2tre$VOTED.INPERSON.2020.PRIMARY==1)/dim(dta2tre)[1]
  tout <- t.test(dta2con$VOTED.INPERSON.2020.PRIMARY,
                 dta2tre$VOTED.INPERSON.2020.PRIMARY)
  rmat2[18,i] <- tout$p.value
  rownames(rmat2)[16:18] <- c("Voted In Person, Control",
                              "Voted In Person, Treated","P-value")
  
  rmat2[19,i] <- sum(dta2con$VOTED.PROVISIONAL.2020.PRIMARY==1)/dim(dta2con)[1]
  rmat2[20,i] <- sum(dta2tre$VOTED.PROVISIONAL.2020.PRIMARY==1)/dim(dta2tre)[1]
  tout <- t.test(dta2con$VOTED.PROVISIONAL.2020.PRIMARY,
                 dta2tre$VOTED.PROVISIONAL.2020.PRIMARY)
  rmat2[21,i] <- tout$p.value
  rownames(rmat2)[19:21] <- c("Voted Provisional, Control",
                              "Voted Provisional, Treated","P-value")
  
  rmat2[22,i] <- sum(dta2con$VOTED.2020.PRIMARY==1)/dim(dta2con)[1]
  rmat2[23,i] <- sum(dta2tre$VOTED.2020.PRIMARY==1)/dim(dta2tre)[1]
  tout <- t.test(dta2con$VOTED.2020.PRIMARY,
                 dta2tre$VOTED.2020.PRIMARY)
  rmat2[24,i] <- tout$p.value
  rownames(rmat2)[22:24] <- c("Voted Any Method, Control",
                              "Voted Any Method, Treated","P-value")
  return(rmat2)
}

### subset data
### returned after postcards sent or not returned
dta3 <- dta2[dta2$DiffAppReturnedDateFmt > 0 | dta2$DiffAppReturnedDateFmt %in% c(NA),]
dim(dta3)
### probability of being White over .5
dta4 <- dta2[dta2$pred.whi > 0.5 & ! dta2$pred.whi %in% c(NA),]
dim(dta4)
### probability of being Black over .5
dta5 <- dta2[dta2$pred.bla > 0.5 & ! dta2$pred.bla %in% c(NA),]
dim(dta5)

rmat2 <- table.function(dta2,i=1,rmat=rmat2)
rmat3 <- table.function(dta3,i=2,rmat=rmat2)
rmat4 <- table.function(dta4,i=3,rmat=rmat3)
rmat5 <- table.function(dta5,i=4,rmat=rmat4)
colnames(rmat5) <- c("All","Didn't Already Vote","Pr(White)>0.5","Pr(Black)>0.5","NA","NA","NA")

xtable(rmat5[,1:4],digits=c(0,rep(3,4)))

