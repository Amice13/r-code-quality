# setwd()
library(pBrackets)
library(plyr); library(dplyr)
library(xtable)
library(texreg)
library(AER)
library(pwr)

load("covid-polarization-replicate-07202020-js.Rdata")
dta.m <- dta.m1

##### generate new variables ----
dta.m$SUPPORTHOMER.W3 <- dta.m$SUPPORTHOME.W3-1

dta.m$AGECAT <- NA
dta.m$AGECAT[dta.m$AGE < 45] <- 1
dta.m$AGECAT[dta.m$AGE >= 45 & dta.m$AGE < 65] <- 2
dta.m$AGECAT[dta.m$AGE >= 65] <- 3

dta.m$COLLEGE <- 1*(dta.m$EDYEARS >=16)

##### subset data to wave 3 
dta.sub <- subset(dta.m,! EXPERTS.TREAT %in% (NA))

dta.sub$REPUBLICAN.C <- dta.sub$REPUBLICAN.W3
dta.sub$DEMOCRAT.C <- dta.sub$DEMOCRAT.W3
dta.sub$INDEPENDENT.C <- dta.sub$INDEPENDENT.W3

##### Table 1: Descriptive Statistics -----
vars <- c("FEMALE","INCOME","EDYEARS","AGE","BLACK","HISP","ASIAN","WHITE","NONE","CATHOLIC",
          "PROTESTANT","JEWISH",
          "REPUBLICAN.W3","DEMOCRAT.W3","INDEPENDENT.W3",
          "RELATTEND.W0","PID.W0",
          "SUPPORTHOMER.W3","TRMEDEXPS.W3")
rmat <- matrix(NA,length(vars),5)
for(i in 1:length(vars)){
  txt1 <- paste("hold <- dta.sub$",vars[i],sep="")
  eval(parse(text=txt1))
  rmat[i,1] <- min(hold,na.rm=T)
  rmat[i,2] <- max(hold,na.rm=T)
  rmat[i,3] <- mean(hold,na.rm=T)
  rmat[i,4] <- sd(hold,na.rm=T)
  rmat[i,5] <- sum(hold %in% c(NA))/length(hold)
}
rownames(rmat) <- vars
colnames(rmat) <- c("Min","Max","Mean","SD","Pct. Missing")
xtable(rmat,digits=c(0,0,0,rep(3,3)))

##### Appendix D -----
vars <- c("VIRUS.ECON.W3","VIRUS.CONCERN.JOINT.W3","VIRUS.PRIMARILY.OLDER.W3","BAR.ELSEWHERE.W3",
          "SUPPORTHOMER.W3","SUPPORTCELL.W3","TRMEDEXPS.W3")
rmat <- matrix(NA,length(vars),5)
for(i in 1:length(vars)){
  txt1 <- paste("hold <- dta.sub$",vars[i],sep="")
  eval(parse(text=txt1))
  rmat[i,1] <- min(hold,na.rm=T)
  rmat[i,2] <- max(hold,na.rm=T)
  rmat[i,3] <- mean(hold,na.rm=T)
  rmat[i,4] <- sd(hold,na.rm=T)
  rmat[i,5] <- sum(hold %in% c(NA))/length(hold)
}
rownames(rmat) <- vars
colnames(rmat) <- c("Min","Max","Mean","SD","Pct. Missing")
xtable(rmat,digits=c(0,0,0,rep(3,3)))


##### Appendix E: regressions with treatment indicators ----
lout1 <- lm(VIRUS.ECON.W3 ~ I(party_id.W3=="Democrat")+I(party_id.W3=="Independent")+scale(AGE)+BLACK+HISP+ASIAN+
              as.factor(EDYEARS)+FEMALE+CATHOLIC+PROTESTANT, data=dta.m)
lout2 <- lm(VIRUS.CONCERN.JOINT.W3 ~ I(party_id.W3=="Democrat")+I(party_id.W3=="Independent")+scale(AGE)+BLACK+HISP+ASIAN+
              as.factor(EDYEARS)+FEMALE+CATHOLIC+PROTESTANT, data=dta.m)
lout3 <- lm(VIRUS.PRIMARILY.OLDER.W3 ~ I(party_id.W3=="Democrat")+I(party_id.W3=="Independent")+scale(AGE)+BLACK+HISP+ASIAN+
              as.factor(EDYEARS)+FEMALE+CATHOLIC+PROTESTANT, data=dta.m)
lout5 <- lm(SUPPORTHOME.W3 ~ I(party_id.W3=="Democrat")+I(party_id.W3=="Independent")+scale(AGE)+BLACK+HISP+ASIAN+
              as.factor(EDYEARS)+FEMALE+CATHOLIC+PROTESTANT+EXPERTS.TREAT, data=dta.m)
lout6 <- lm(SUPPORTCELL.W3 ~ I(party_id.W3=="Democrat")+I(party_id.W3=="Independent")+scale(AGE)+BLACK+HISP+ASIAN+
              as.factor(EDYEARS)+FEMALE+CATHOLIC+PROTESTANT+EXPERTS.TREAT, data=dta.m)
lout7 <- lm(TRMEDEXPS.W3 ~ I(party_id.W3=="Democrat")+I(party_id.W3=="Independent")+scale(AGE)+BLACK+HISP+ASIAN+
              as.factor(EDYEARS)+FEMALE+CATHOLIC+PROTESTANT+EXPERTS.TREAT, data=dta.m)
texreg(list(lout1,lout2,lout3,lout5,lout6,lout7),
       custom.model.names=c("Re-open econ.","Worried personally","Older people",
                            #"Bar from elsewhere",
                            "Support staying home","Use cell phones",
                            "Trust experts"),
       stars=0.05,digits=3)


##### Figure 1 -----
tout <- t.test(dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==1]-1,dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==0]-1)
tout1 <- t.test(dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==1]-1)
tout2 <- t.test(dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==0]-1)

# pdf("reactance-all-04172020.pdf")
# jpeg("reactance-all-06122020.jpg")
dist1 <- .25
dist2 <- .15
bp <- barplot(c(tout$estimate[1],tout$estimate[2]),ylim=c(0,3),
              names.arg=c("Elites Urge","One Proposal"),
              ylab="Policy Support",
              cex.lab=1.5,cex.sub=1.5,xlim=c(0,3))
text(round(tout$estimate[1],digits=2),x=bp[1,1],y=tout$estimate[1]-dist1,cex=2)
text(round(tout$estimate[2],digits=2),x=bp[2,1],y=tout$estimate[2]-dist1,cex=2)
lines(x=c(bp[1,1],bp[1,1]),y=c(tout1$conf.int[1],tout1$conf.int[2]),lwd=2)
lines(x=c(bp[2,1],bp[2,1]),y=c(tout2$conf.int[1],tout2$conf.int[2]),lwd=2)

mn <- mean(dta.m$SUPPORTHOME.W3-1,na.rm=T)
lines(x=c(bp[2,1]+.65,bp[2,1]+.65),
      y=c((mn-sd(dta.m$SUPPORTHOME.W3,na.rm=T)/2),(mn+sd(dta.m$SUPPORTHOME.W3,na.rm=T)/2)),
      lwd=2)
sd.txt <- paste("SD=\n",round(sd(dta.m$SUPPORTHOME.W3,na.rm=T),digits=2),sep="")
text(sd.txt,x=bp[2,1]+.9,y=mn,cex=2)

brackets(x1=bp[1,1],x2=bp[2,1],y1=tout$estimate[1]+dist2,y2=tout$estimate[1]+dist2,lwd=2)
txt1 <- paste("Difference: ",round(tout$estimate[1]-tout$estimate[2],digits=2),
              "\n","P-value: ",round(tout$p.value,digits=2),sep="")
text(txt1,y=tout$estimate[1]+.5,x=mean(bp[,1]),cex=2)

# dev.off()



##### Appendix F: party ID figure -----
# pdf("reactance-pid-04222020.pdf",width=12)
# jpeg("reactance-pid-06122020.jpg",width=1000)#
par(mfcol=c(1,3))

tout <- t.test(dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==1 & dta.m$REPUBLICAN.W3==1]-1,
               dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==0& dta.m$REPUBLICAN.W3==1]-1)
tout1 <- t.test(dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==1  & dta.m$REPUBLICAN.W3==1]-1)
tout2 <- t.test(dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==0  & dta.m$REPUBLICAN.W3==1]-1)

dist1 <- .25
dist2 <- .15
cx.mn <- 2.5
bp <- barplot(c(tout$estimate[1],tout$estimate[2]),ylim=c(0,3.5),
              names.arg=c("Elites Urge","One Proposal"),
              ylab="Policy Support",
              cex.lab=1.5,cex.sub=1.5,xlim=c(0,3),
              main="Republicans",cex.main=cx.mn)
text(round(tout$estimate[1],digits=2),x=bp[1,1],y=tout$estimate[1]-dist1,cex=2)
text(round(tout$estimate[2],digits=2),x=bp[2,1],y=tout$estimate[2]-dist1,cex=2)
lines(x=c(bp[1,1],bp[1,1]),y=c(tout1$conf.int[1],tout1$conf.int[2]),lwd=2)
lines(x=c(bp[2,1],bp[2,1]),y=c(tout2$conf.int[1],tout2$conf.int[2]),lwd=2)

mn <- mean(dta.m$SUPPORTHOME.W3[dta.m$REPUBLICAN.W3==1]-1,na.rm=T)
lines(x=c(bp[2,1]+.65,bp[2,1]+.65),
      y=c((mn-sd(dta.m$SUPPORTHOME.W3[dta.m$REPUBLICAN.W3==1],na.rm=T)/2),
          (mn+sd(dta.m$SUPPORTHOME.W3[dta.m$REPUBLICAN.W3==1],na.rm=T)/2)),
      lwd=2)
sd.txt <- paste("SD=\n",round(sd(dta.m$SUPPORTHOME.W3[dta.m$REPUBLICAN.W3==1],na.rm=T),digits=2),sep="")
text(sd.txt,x=bp[2,1]+.9,y=mn,cex=2)

brackets(x1=bp[1,1],x2=bp[2,1],y1=tout$estimate[1]+dist2,y2=tout$estimate[1]+dist2,lwd=2)
txt1 <- paste("Difference: ",round(tout$estimate[1]-tout$estimate[2],digits=2),
              "\n","P-value: ",round(tout$p.value,digits=2),sep="")
text(txt1,y=tout$estimate[1]+.5,x=mean(bp[,1]),cex=2)

#### Independent
tout <- t.test(dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==1 & dta.m$INDEPENDENT.W3==1]-1,
               dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==0& dta.m$INDEPENDENT.W3==1]-1)
tout1 <- t.test(dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==1  & dta.m$INDEPENDENT.W3==1]-1)
tout2 <- t.test(dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==0  & dta.m$INDEPENDENT.W3==1]-1)

dist1 <- .25
dist2 <- .15
bp <- barplot(c(tout$estimate[1],tout$estimate[2]),ylim=c(0,3.5),
              names.arg=c("Elites Urge","One Proposal"),
              ylab="Policy Support",
              cex.lab=1.5,cex.sub=1.5,xlim=c(0,3),
              main="Independents",cex.main=cx.mn)
text(round(tout$estimate[1],digits=2),x=bp[1,1],y=tout$estimate[1]-dist1,cex=2)
text(round(tout$estimate[2],digits=2),x=bp[2,1],y=tout$estimate[2]-dist1,cex=2)
lines(x=c(bp[1,1],bp[1,1]),y=c(tout1$conf.int[1],tout1$conf.int[2]),lwd=2)
lines(x=c(bp[2,1],bp[2,1]),y=c(tout2$conf.int[1],tout2$conf.int[2]),lwd=2)

mn <- mean(dta.m$SUPPORTHOME.W3[dta.m$INDEPENDENT.W3==1]-1,na.rm=T)
lines(x=c(bp[2,1]+.65,bp[2,1]+.65),
      y=c((mn-sd(dta.m$SUPPORTHOME.W3[dta.m$INDEPENDENT.W3==1],na.rm=T)/2),
          (mn+sd(dta.m$SUPPORTHOME.W3[dta.m$INDEPENDENT.W3==1],na.rm=T)/2)),
      lwd=2)
sd.txt <- paste("SD=\n",round(sd(dta.m$SUPPORTHOME.W3[dta.m$INDEPENDENT.W3==1],na.rm=T),digits=2),sep="")
text(sd.txt,x=bp[2,1]+.9,y=mn,cex=2)

brackets(x1=bp[1,1],x2=bp[2,1],y1=tout$estimate[1]+dist2,y2=tout$estimate[1]+dist2,lwd=2)
txt1 <- paste("Difference: ",round(tout$estimate[1]-tout$estimate[2],digits=2),
              "\n","P-value: ",round(tout$p.value,digits=2),sep="")
text(txt1,y=tout$estimate[1]+.5,x=mean(bp[,1]),cex=2)

#### Democrat
tout <- t.test(dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==1 & dta.m$DEMOCRAT.W3==1]-1,
               dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==0& dta.m$DEMOCRAT.W3==1]-1)
tout1 <- t.test(dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==1  & dta.m$DEMOCRAT.W3==1]-1)
tout2 <- t.test(dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==0  & dta.m$DEMOCRAT.W3==1]-1)

dist1 <- .25
dist2 <- .15
bp <- barplot(c(tout$estimate[1],tout$estimate[2]),ylim=c(0,3.5),
              names.arg=c("Elites Urge","One Proposal"),
              ylab="Policy Support",
              cex.lab=1.5,cex.sub=1.5,xlim=c(0,3),
              main="Democrats",cex.main=cx.mn)
text(round(tout$estimate[1],digits=2),x=bp[1,1],y=tout$estimate[1]-dist1,cex=2)
text(round(tout$estimate[2],digits=2),x=bp[2,1],y=tout$estimate[2]-dist1,cex=2)
lines(x=c(bp[1,1],bp[1,1]),y=c(tout1$conf.int[1],tout1$conf.int[2]),lwd=2)
lines(x=c(bp[2,1],bp[2,1]),y=c(tout2$conf.int[1],tout2$conf.int[2]),lwd=2)

mn <- mean(dta.m$SUPPORTHOME.W3[dta.m$DEMOCRAT.W3==1]-1,na.rm=T)
lines(x=c(bp[2,1]+.65,bp[2,1]+.65),
      y=c((mn-sd(dta.m$SUPPORTHOME.W3[dta.m$DEMOCRAT.W3==1],na.rm=T)/2),
          (mn+sd(dta.m$SUPPORTHOME.W3[dta.m$DEMOCRAT.W3==1],na.rm=T)/2)),
      lwd=2)
sd.txt <- paste("SD=\n",round(sd(dta.m$SUPPORTHOME.W3[dta.m$DEMOCRAT.W3==1],na.rm=T),digits=2),sep="")
text(sd.txt,x=bp[2,1]+.9,y=mn,cex=2)

brackets(x1=bp[1,1],x2=bp[2,1],y1=tout$estimate[1]+dist2,y2=tout$estimate[1]+dist2,lwd=2)
txt1 <- paste("Difference: ",round(tout$estimate[1]-tout$estimate[2],digits=2),
              "\n","P-value: ",round(tout$p.value,digits=2),sep="")
text(txt1,y=tout$estimate[1]+.5,x=mean(bp[,1]),cex=2)

# dev.off()


##### Appendix G: education figure -----
# pdf("reactance-education-04222020.pdf",width=10)
# jpeg("reactance-education-06122020.jpg",width=1000)
par(mfcol=c(1,2))

tout <- t.test(dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==1 & dta.m$COLLEGE==0]-1,
               dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==0 & dta.m$COLLEGE==0]-1)
tout1 <- t.test(dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==1 & dta.m$COLLEGE==0]-1)
tout2 <- t.test(dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==0 & dta.m$COLLEGE==0]-1)

dist1 <- .25
dist2 <- .15
cx.mn <- 2.5
bp <- barplot(c(tout$estimate[1],tout$estimate[2]),ylim=c(0,3.5),
              names.arg=c("Elites Urge","One Proposal"),
              ylab="Policy Support",
              cex.lab=1.5,cex.sub=1.5,xlim=c(0,3.5),
              main="No College Degree",cex.main=cx.mn)
text(round(tout$estimate[1],digits=2),x=bp[1,1],y=tout$estimate[1]-dist1,cex=2)
text(round(tout$estimate[2],digits=2),x=bp[2,1],y=tout$estimate[2]-dist1,cex=2)
lines(x=c(bp[1,1],bp[1,1]),y=c(tout1$conf.int[1],tout1$conf.int[2]),lwd=2)
lines(x=c(bp[2,1],bp[2,1]),y=c(tout2$conf.int[1],tout2$conf.int[2]),lwd=2)

mn <- mean(dta.m$SUPPORTHOME.W3[dta.m$COLLEGE==0]-1,na.rm=T)
lines(x=c(bp[2,1]+.65,bp[2,1]+.65),
      y=c((mn-sd(dta.m$SUPPORTHOME.W3[dta.m$COLLEGE==0],na.rm=T)/2),
          (mn+sd(dta.m$SUPPORTHOME.W3[dta.m$COLLEGE==0],na.rm=T)/2)),
      lwd=2)
sd.txt <- paste("SD=\n",round(sd(dta.m$SUPPORTHOME.W3[dta.m$COLLEGE==0],na.rm=T),digits=2),sep="")
text(sd.txt,x=bp[2,1]+1,y=mn,cex=2)

brackets(x1=bp[1,1],x2=bp[2,1],y1=tout$estimate[1]+dist2,y2=tout$estimate[1]+dist2,lwd=2)
txt1 <- paste("Difference: ",round(tout$estimate[1]-tout$estimate[2],digits=2),
              "\n","P-value: ",round(tout$p.value,digits=2),sep="")
text(txt1,y=tout$estimate[1]+.75,x=mean(bp[,1]),cex=2)

#### College
tout <- t.test(dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==1 & dta.m$COLLEGE==1]-1,
               dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==0 & dta.m$COLLEGE==1]-1)
tout1 <- t.test(dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==1  & dta.m$COLLEGE==1]-1)
tout2 <- t.test(dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==0  & dta.m$COLLEGE==1]-1)

dist1 <- .45
dist2 <- .25
bp <- barplot(c(tout$estimate[1],tout$estimate[2]),ylim=c(0,3.5),
              names.arg=c("Elites Urge","One Proposal"),
              ylab="Policy Support",
              cex.lab=1.5,cex.sub=1.5,xlim=c(0,3.5),
              main="College Degree",cex.main=cx.mn)
text(round(tout$estimate[1],digits=2),x=bp[1,1],y=tout$estimate[1]-dist1,cex=2)
text(round(tout$estimate[2],digits=2),x=bp[2,1],y=tout$estimate[2]-dist1,cex=2)
lines(x=c(bp[1,1],bp[1,1]),y=c(tout1$conf.int[1],tout1$conf.int[2]),lwd=2)
lines(x=c(bp[2,1],bp[2,1]),y=c(tout2$conf.int[1],tout2$conf.int[2]),lwd=2)

mn <- mean(dta.m$SUPPORTHOME.W3[dta.m$COLLEGE==1]-1,na.rm=T)
lines(x=c(bp[2,1]+.65,bp[2,1]+.65),
      y=c((mn-sd(dta.m$SUPPORTHOME.W3[dta.m$COLLEGE==1],na.rm=T)/2),
          (mn+sd(dta.m$SUPPORTHOME.W3[dta.m$COLLEGE==1],na.rm=T)/2)),
      lwd=2)
sd.txt <- paste("SD=\n",round(sd(dta.m$SUPPORTHOME.W3[dta.m$COLLEGE==1],na.rm=T),digits=2),sep="")
text(sd.txt,x=bp[2,1]+1,y=mn,cex=2)

brackets(x1=bp[1,1],x2=bp[2,1],y1=tout$estimate[1]+dist2,y2=tout$estimate[1]+dist2,lwd=2)
txt1 <- paste("Difference: ",round(tout$estimate[1]-tout$estimate[2],digits=2),
              "\n","P-value: ",round(tout$p.value,digits=2),sep="")
text(txt1,y=tout$estimate[1]+.75,x=mean(bp[,1]),cex=2)

# dev.off()


##### Appendix H: age figure -----
# pdf("reactance-age-04222020.pdf",width=12)
# jpeg("reactance-age-06122020.jpg",width=1000)
par(mfcol=c(1,3))

tout <- t.test(dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==1 & dta.m$AGECAT==1]-1,
               dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==0& dta.m$AGECAT==1]-1)
tout1 <- t.test(dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==1  & dta.m$AGECAT==1]-1)
tout2 <- t.test(dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==0  & dta.m$AGECAT==1]-1)

dist1 <- .45
dist2 <- .25
cx.mn <- 2.5
bp <- barplot(c(tout$estimate[1],tout$estimate[2]),ylim=c(0,3.5),
              names.arg=c("Elites Urge","One Proposal"),
              ylab="Policy Support",
              cex.lab=1.5,cex.sub=1.5,xlim=c(0,3.75),
              main="Under 45",cex.main=cx.mn)
text(round(tout$estimate[1],digits=2),x=bp[1,1],y=tout$estimate[1]-dist1,cex=2)
text(round(tout$estimate[2],digits=2),x=bp[2,1],y=tout$estimate[2]-dist1,cex=2)
lines(x=c(bp[1,1],bp[1,1]),y=c(tout1$conf.int[1],tout1$conf.int[2]),lwd=2)
lines(x=c(bp[2,1],bp[2,1]),y=c(tout2$conf.int[1],tout2$conf.int[2]),lwd=2)

mn <- mean(dta.m$SUPPORTHOME.W3[dta.m$AGECAT==1]-1,na.rm=T)
lines(x=c(bp[2,1]+.65,bp[2,1]+.65),
      y=c((mn-sd(dta.m$SUPPORTHOME.W3[dta.m$AGECAT==1],na.rm=T)/2),
          (mn+sd(dta.m$SUPPORTHOME.W3[dta.m$AGECAT==1],na.rm=T)/2)),
      lwd=2)
sd.txt <- paste("SD=\n",round(sd(dta.m$SUPPORTHOME.W3[dta.m$AGECAT==1],na.rm=T),digits=2),sep="")
text(sd.txt,x=bp[2,1]+1.1,y=mn,cex=2)

brackets(x1=bp[1,1],x2=bp[2,1],y1=tout$estimate[1]+dist2,y2=tout$estimate[1]+dist2,lwd=2)
txt1 <- paste("Difference: ",round(tout$estimate[1]-tout$estimate[2],digits=2),
              "\n","P-value: ",round(tout$p.value,digits=2),sep="")
text(txt1,y=tout$estimate[1]+.75,x=mean(bp[,1]),cex=2)

#### 45-64
tout <- t.test(dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==1 & dta.m$AGECAT==2]-1,
               dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==0& dta.m$AGECAT==2]-1)
tout1 <- t.test(dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==1  & dta.m$AGECAT==2]-1)
tout2 <- t.test(dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==0  & dta.m$AGECAT==2]-1)

bp <- barplot(c(tout$estimate[1],tout$estimate[2]),ylim=c(0,3.5),
              names.arg=c("Elites Urge","One Proposal"),
              ylab="Policy Support",
              cex.lab=1.5,cex.sub=1.5,xlim=c(0,3.75),
              main="45-64",cex.main=cx.mn)
text(round(tout$estimate[1],digits=2),x=bp[1,1],y=tout$estimate[1]-dist1,cex=2)
text(round(tout$estimate[2],digits=2),x=bp[2,1],y=tout$estimate[2]-dist1,cex=2)
lines(x=c(bp[1,1],bp[1,1]),y=c(tout1$conf.int[1],tout1$conf.int[2]),lwd=2)
lines(x=c(bp[2,1],bp[2,1]),y=c(tout2$conf.int[1],tout2$conf.int[2]),lwd=2)

mn <- mean(dta.m$SUPPORTHOME.W3[dta.m$AGECAT==2]-1,na.rm=T)
lines(x=c(bp[2,1]+.65,bp[2,1]+.65),
      y=c((mn-sd(dta.m$SUPPORTHOME.W3[dta.m$AGECAT==2],na.rm=T)/2),
          (mn+sd(dta.m$SUPPORTHOME.W3[dta.m$AGECAT==2],na.rm=T)/2)),
      lwd=2)
sd.txt <- paste("SD=\n",round(sd(dta.m$SUPPORTHOME.W3[dta.m$AGECAT==2],na.rm=T),digits=2),sep="")
text(sd.txt,x=bp[2,1]+1.1,y=mn,cex=2)

brackets(x1=bp[1,1],x2=bp[2,1],y1=tout$estimate[1]+dist2,y2=tout$estimate[1]+dist2,lwd=2)
txt1 <- paste("Difference: ",round(tout$estimate[1]-tout$estimate[2],digits=2),
              "\n","P-value: ",round(tout$p.value,digits=2),sep="")
text(txt1,y=tout$estimate[1]+.75,x=mean(bp[,1]),cex=2)

#### 65+
tout <- t.test(dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==1 & dta.m$AGECAT==3]-1,
               dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==0 & dta.m$AGECAT==3]-1)
tout1 <- t.test(dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==1  & dta.m$AGECAT==3]-1)
tout2 <- t.test(dta.m$SUPPORTHOME.W3[dta.m$EXPERTS.TREAT==0  & dta.m$AGECAT==3]-1)

bp <- barplot(c(tout$estimate[1],tout$estimate[2]),ylim=c(0,3.5),
              names.arg=c("Elites Urge","One Proposal"),
              ylab="Policy Support",
              cex.lab=1.5,cex.sub=1.5,xlim=c(0,3.75),
              main="Over 64",cex.main=cx.mn)
text(round(tout$estimate[1],digits=2),x=bp[1,1],y=tout$estimate[1]-dist1,cex=2)
text(round(tout$estimate[2],digits=2),x=bp[2,1],y=tout$estimate[2]-dist1,cex=2)
lines(x=c(bp[1,1],bp[1,1]),y=c(tout1$conf.int[1],tout1$conf.int[2]),lwd=2)
lines(x=c(bp[2,1],bp[2,1]),y=c(tout2$conf.int[1],tout2$conf.int[2]),lwd=2)

mn <- mean(dta.m$SUPPORTHOME.W3[dta.m$AGECAT==3]-1,na.rm=T)
lines(x=c(bp[2,1]+.65,bp[2,1]+.65),
      y=c((mn-sd(dta.m$SUPPORTHOME.W3[dta.m$AGECAT==3],na.rm=T)/2),
          (mn+sd(dta.m$SUPPORTHOME.W3[dta.m$AGECAT==3],na.rm=T)/2)),
      lwd=2)
sd.txt <- paste("SD=\n",round(sd(dta.m$SUPPORTHOME.W3[dta.m$AGECAT==3],na.rm=T),digits=2),sep="")
text(sd.txt,x=bp[2,1]+1.1,y=mn,cex=2)

brackets(x1=bp[1,1],x2=bp[2,1],y1=tout$estimate[1]+dist2,y2=tout$estimate[1]+dist2,lwd=2)
txt1 <- paste("Difference: ",round(tout$estimate[1]-tout$estimate[2],digits=2),
              "\n","P-value: ",round(tout$p.value,digits=2),sep="")
text(txt1,y=tout$estimate[1]+.75,x=mean(bp[,1]),cex=2)

# dev.off()




##### Second Experiment (Wave 5)

dta.m$DEMOCRAT.W5 <- 1*(dta.m$party_id.W5=="Democrat")
dta.m$REPUBLICAN.W5 <- 1*(dta.m$party_id.W5=="Republican")
dta.m$INDEPENDENT.W5 <- 1*(dta.m$party_id.W5=="Independent")


#### subset to wave 5
dta.sub.w5 <- subset(dta.m,! REOPEN.GV.TREATMENT.W5 %in% (NA))
dta.sub.w5$REPUBLICAN.C <- dta.sub.w5$REPUBLICAN.W5
dta.sub.w5$DEMOCRAT.C <- dta.sub.w5$DEMOCRAT.W5
dta.sub.w5$INDEPENDENT.C <- dta.sub.w5$INDEPENDENT.W5


##### analyze wave 5 ----

##### wave 5 experiment manipulation checks
t.test(dta.m$RECALL.PH.W5[dta.m$REOPEN.PH.TREATMENT.W5==1],
       dta.m$RECALL.PH.W5[dta.m$REOPEN.PH.TREATMENT.W5==0])

t.test(dta.m$RECALL.GOV.W5[dta.m$REOPEN.GV.TREATMENT.W5==1],
       dta.m$RECALL.GOV.W5[dta.m$REOPEN.GV.TREATMENT.W5==0])

t.test(dta.m$RECALL.SM.W5[dta.m$REOPEN.SM.TREATMENT.W5==1],
       dta.m$RECALL.SM.W5[dta.m$REOPEN.SM.TREATMENT.W5==0])

t.test(dta.m$REOPEN.GV[dta.m$party_id.W5=="Democrat"],dta.m$REOPEN.SM[dta.m$party_id.W5=="Democrat"])
t.test(dta.m$REOPEN.GV[dta.m$party_id.W5=="Republican"],dta.m$REOPEN.SM[dta.m$party_id.W5=="Republican"])
t.test(dta.m$REOPEN.GV[dta.m$party_id.W5=="Independent"],dta.m$REOPEN.SM[dta.m$party_id.W5=="Independent"])


dta.m$DEMOCRAT.W5 <- 1*(dta.m$party_id.W5=="Democrat")
dta.m$REPUBLICAN.W5 <- 1*(dta.m$party_id.W5=="Republican")


##### Figure 2 -----

tout <- t.test(dta.m$REOPEN.GV-1,dta.m$REOPEN.SM-1)
tout1 <- t.test(dta.m$REOPEN.GV[dta.m$REOPEN.GV.TREATMENT.W5==1]-1)
tout2 <- t.test(dta.m$REOPEN.SM[dta.m$REOPEN.SM.TREATMENT.W5==1]-1)

#jpeg("reactance-exp2-all-06042020.jpg",width=1220)
par(mfcol=c(1,2))

dist1 <- .25
dist2 <- .3
bp <- barplot(c(tout$estimate[1],tout$estimate[2]),ylim=c(0,3),
              names.arg=c("Gov't Officials","Some"),
              ylab="Policy Support",
              cex.lab=1.5,cex.sub=1.5,xlim=c(0,3))
text(round(tout$estimate[1],digits=2),x=bp[1,1],y=tout$estimate[1]-dist1,cex=2)
text(round(tout$estimate[2],digits=2),x=bp[2,1],y=tout$estimate[2]-dist1,cex=2)
lines(x=c(bp[1,1],bp[1,1]),y=c(tout1$conf.int[1],tout1$conf.int[2]),lwd=2)
lines(x=c(bp[2,1],bp[2,1]),y=c(tout2$conf.int[1],tout2$conf.int[2]),lwd=2)

mn <- mean(c(dta.m$REOPEN.GV.W5,dta.m$REOPEN.PH.W5,dta.m$REOPEN.SM.W5)-1,na.rm=T)
lines(x=c(bp[2,1]+.65,bp[2,1]+.65),
      y=c((mn-sd(c(dta.m$REOPEN.GV.W5,dta.m$REOPEN.PH.W5,dta.m$REOPEN.SM.W5),na.rm=T)/2),
          (mn+sd(c(dta.m$REOPEN.GV.W5,dta.m$REOPEN.PH.W5,dta.m$REOPEN.SM.W5),na.rm=T)/2)),
      lwd=2)
sd.txt <- paste("SD=\n",
                round(sd(c(dta.m$REOPEN.GV.W5,dta.m$REOPEN.PH.W5,dta.m$REOPEN.SM.W5),na.rm=T),digits=2),
                sep="")
text(sd.txt,x=bp[2,1]+.9,y=mn,cex=2)

brackets(x1=bp[1,1],x2=bp[2,1],
         y1=tout$estimate[1]+dist2,y2=tout$estimate[1]+dist2,lwd=2)
txt1 <- paste("Difference: ",round(tout$estimate[1]-tout$estimate[2],digits=2),
              "\n","P-value: ",round(tout$p.value,digits=2),sep="")
text(txt1,y=tout$estimate[1]+.75,x=mean(bp[,1]),cex=2)


tout <- t.test(dta.m$REOPEN.PH-1,dta.m$REOPEN.SM-1)
tout1 <- t.test(dta.m$REOPEN.PH[dta.m$REOPEN.PH.TREATMENT.W5==1]-1)
tout2 <- t.test(dta.m$REOPEN.SM[dta.m$REOPEN.SM.TREATMENT.W5==1]-1)

bp <- barplot(c(tout$estimate[1],tout$estimate[2]),ylim=c(0,3),
              names.arg=c("Public Health Experts","Some"),
              ylab="Policy Support",
              cex.lab=1.5,cex.sub=1.5,xlim=c(0,3))
text(round(tout$estimate[1],digits=2),x=bp[1,1],y=tout$estimate[1]-dist1,cex=2)
text(round(tout$estimate[2],digits=2),x=bp[2,1],y=tout$estimate[2]-dist1,cex=2)
lines(x=c(bp[1,1],bp[1,1]),y=c(tout1$conf.int[1],tout1$conf.int[2]),lwd=2)
lines(x=c(bp[2,1],bp[2,1]),y=c(tout2$conf.int[1],tout2$conf.int[2]),lwd=2)

mn <- mean(c(dta.m$REOPEN.GV.W5,dta.m$REOPEN.PH.W5,dta.m$REOPEN.SM.W5)-1,na.rm=T)
lines(x=c(bp[2,1]+.65,bp[2,1]+.65),
      y=c((mn-sd(c(dta.m$REOPEN.GV.W5,dta.m$REOPEN.PH.W5,dta.m$REOPEN.SM.W5),na.rm=T)/2),
          (mn+sd(c(dta.m$REOPEN.GV.W5,dta.m$REOPEN.PH.W5,dta.m$REOPEN.SM.W5),na.rm=T)/2)),
      lwd=2)
sd.txt <- paste("SD=\n",
                round(sd(c(dta.m$REOPEN.GV.W5,dta.m$REOPEN.PH.W5,dta.m$REOPEN.SM.W5),na.rm=T),digits=2),
                sep="")
text(sd.txt,x=bp[2,1]+.9,y=mn,cex=2)

brackets(x1=bp[1,1],x2=bp[2,1],
         y1=tout$estimate[1]+dist2,y2=tout$estimate[1]+dist2,lwd=2)
txt1 <- paste("Difference: ",round(tout$estimate[1]-tout$estimate[2],digits=2),
              "\n","P-value: ",round(tout$p.value,digits=2),sep="")
text(txt1,y=tout$estimate[1]+.75,x=mean(bp[,1]),cex=2)

# dev.off()



##### moderation by age
dta.m$REOPEN.W5 <- NA
dta.m$REOPEN.W5[! dta.m$REOPEN.GV %in% c(NA)] <- dta.m$REOPEN.GV[! dta.m$REOPEN.GV %in% c(NA)]
dta.m$REOPEN.W5[! dta.m$REOPEN.PH %in% c(NA)] <- dta.m$REOPEN.PH[! dta.m$REOPEN.PH %in% c(NA)]
dta.m$REOPEN.W5[! dta.m$REOPEN.SM %in% c(NA)] <- dta.m$REOPEN.SM[! dta.m$REOPEN.SM %in% c(NA)]

dta.m$TREAT.GV.W5 <- 1*(dta.m$version.W5 %in% c("Version C","Version D","Version I","Version J"))
dta.m$TREAT.PH.W5 <- 1*(dta.m$version.W5 %in% c("Version A","Version B","Version G","Version H"))
dta.m$TREAT.SM.W5 <- 1*(dta.m$version.W5 %in% c("Version E","Version F","Version K","Version L"))

lout0 <- lm(REOPEN.W5 ~ TREAT.GV.W5+TREAT.PH.W5,data=dta.m)
summary(lout0)
lout1 <- lm(REOPEN.W5 ~ TREAT.GV.W5+TREAT.PH.W5,data=dta.m[dta.m$age.W5 <= 64,])
summary(lout1)
lout2 <- lm(REOPEN.W5 ~ TREAT.GV.W5+TREAT.PH.W5,data=dta.m[dta.m$age.W5 > 64,])
summary(lout2)

texreg(list(lout0,lout1,lout2),digits=3,custom.model.names=c("All","Under 65","65+"))


##### test moderation by trust in experts
dta.m$REOPEN.W5 <- NA
dta.m$REOPEN.W5[! dta.m$REOPEN.GV %in% c(NA)] <- dta.m$REOPEN.GV[! dta.m$REOPEN.GV %in% c(NA)]
dta.m$REOPEN.W5[! dta.m$REOPEN.PH %in% c(NA)] <- dta.m$REOPEN.PH[! dta.m$REOPEN.PH %in% c(NA)]
dta.m$REOPEN.W5[! dta.m$REOPEN.SM %in% c(NA)] <- dta.m$REOPEN.SM[! dta.m$REOPEN.SM %in% c(NA)]

dta.m$TREAT.GV.W5 <- 1*(dta.m$version.W5 %in% c("Version C","Version D","Version I","Version J"))
dta.m$TREAT.PH.W5 <- 1*(dta.m$version.W5 %in% c("Version A","Version B","Version G","Version H"))
dta.m$TREAT.SM.W5 <- 1*(dta.m$version.W5 %in% c("Version E","Version F","Version K","Version L"))

lout <- lm(REOPEN.W5 ~ TREAT.GV.W5+TREAT.PH.W5,data=dta.m)
summary(lout)
lout <- lm(REOPEN.W5 ~ TREAT.GV.W5+TREAT.PH.W5,data=dta.m[dta.m$TRMEDEXPS.W3==4 & ! dta.m$TRMEDEXPS.W3 %in% c(NA),])
summary(lout)
lout <- lm(REOPEN.W5 ~ TREAT.GV.W5+TREAT.PH.W5,data=dta.m[! dta.m$TRMEDEXPS.W3==4,])
summary(lout)


##### complier average causal effect
dta.sub <- dta.m[! dta.m$REOPEN.GV.W5 %in% c(NA) | ! dta.m$REOPEN.SM.W5 %in% c(NA),]
dta.sub$REOPEN.JOINT <- NA
dta.sub$REOPEN.JOINT[! dta.sub$REOPEN.GV %in% c(NA)] <- dta.sub$REOPEN.GV[! dta.sub$REOPEN.GV %in% c(NA)]
dta.sub$REOPEN.JOINT[! dta.sub$REOPEN.SM %in% c(NA)] <- dta.sub$REOPEN.SM[! dta.sub$REOPEN.SM %in% c(NA)]
lout <- ivreg(REOPEN.JOINT ~ RECALL.GOV.W5| REOPEN.GV.TREATMENT.W5,data=dta.sub)
summary(lout)

