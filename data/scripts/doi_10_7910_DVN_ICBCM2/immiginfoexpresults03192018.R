source("/users/danhop/Dropbox/ImmigrationInnumeracy/immiginfo/replication/immiginfoload03202018.R")

library(MASS)

### earlier versions
dta10$GUESS <- NA
dta10$GUESS[dta10$GROUPN2==3] <- 1
dta10$GUESS[! dta10$GROUPN2==3] <- 0

dta10$CORRECT <- NA
dta10$CORRECT[dta10$GROUPN2==4] <- 1
dta10$CORRECT[! dta10$GROUPN2==4] <- 0

dta10$CONTROL <- NA
dta10$CONTROL[dta10$GROUPN2==2] <- 1
dta10$CONTROL[! dta10$GROUPN2==2] <- 0

dta10$INFO <- NA
dta10$INFO[dta10$GROUPN2==1] <- 1
dta10$INFO[! dta10$GROUPN2==1] <- 0

dta$GROUPN2 <- NA
dta$GROUPN2[dta$GROUP=="Group C (Information)"] <- 1
dta$GROUPN2[dta$GROUP=="Group A (CONTROL)"] <- 2
dta$GROUPN2[dta$GROUP=="Group B (Guess)"] <- 3
dta$GROUPN2[dta$GROUP=="Group D (Correction)"] <- 4

dta$EMPLOYED <- 1*(dta$PPWORK %in% c("Working - as a paid employee","Working - self-employed"))

dta$GUESS <- NA
dta$GUESS[dta$GROUPN2==3] <- 1
dta$GUESS[! dta$GROUPN2==3] <- 0

dta$CORRECT <- NA
dta$CORRECT[dta$GROUPN2==4] <- 1
dta$CORRECT[! dta$GROUPN2==4] <- 0

dta$CONTROL <- NA
dta$CONTROL[dta$GROUPN2==2] <- 1
dta$CONTROL[! dta$GROUPN2==2] <- 0

dta$INFO <- NA
dta$INFO[dta$GROUPN2==1] <- 1
dta$INFO[! dta$GROUPN2==1] <- 0

dta$HISP <- 1*(dta$PPETHM=="Hispanic")

dta2 <- dta[! dta$PPETHM=="Hispanic",]

dt$GROUPN2 <- NA
#info
dt$GROUPN2[dt$G_DAT=="G1"] <- 1
#guess
dt$GROUPN2[dt$G_DAT=="G3"] <- 3

dt$GUESS <- 1*(dt$GROUPN2==3)
dt$INFO <- 1*(dt$GROUPN2==1)

dt$GROUPN <- 99
#### 1 = natl info
dt$GROUPN[dt$G_DAT=="G1"] <- 1
#### 3 = natl guess
dt$GROUPN[dt$G_DAT=="G3"] <- 3

dt$LETINR <- NA
dt$LETINR[dt$LETIN3==1] <- 5
dt$LETINR[dt$LETIN3==2] <- 4
dt$LETINR[dt$LETIN3==3] <- 3
dt$LETINR[dt$LETIN3==4] <- 2
dt$LETINR[dt$LETIN3==5] <- 1

dt$INDEX <- dt$TAKEJOBS+dt$THREATEN+dt$LETINR

dt2 <- dt[dt$HISP==0,]

cal.out <- lm(LETINR ~ INFO+CONTROL+CORRECT+EDYEARS,data=dta2)
cal.out <- lm(LETINR ~ INFO+CONTROL+CORRECT+PID,data=dta2)

#### national-local

dt2$LOCINFO <- NA
dt2$LOCINFO[dt2$GROUPL==2] <- 1
dt2$LOCINFO[dt2$GROUPL==4] <- 0

M <- 10000
lout.kn08.lc <- lm(LETINR ~ LOCINFO,data=dt2)
beta.kn08.lc <- mvrnorm(M,mu=summary(lout.kn08.lc)$coef[,1],Sigma=vcov(lout.kn08.lc))
pout.kn08.guess.lc <- beta.kn08.lc%*%c(1,0)
pout.kn08.info.lc <- beta.kn08.lc%*%c(1,1)

rmat103 <- matrix(NA,2,3)

rmat103[1,1] <- quantile(pout.kn08.info.lc,.025)
rmat103[1,2] <- mean(pout.kn08.info.lc)
rmat103[1,3] <- quantile(pout.kn08.info.lc,.975)

rmat103[2,1] <- quantile(pout.kn08.guess.lc,.025)
rmat103[2,2] <- mean(pout.kn08.guess.lc)
rmat103[2,3] <- quantile(pout.kn08.guess.lc,.975)

dens <- na.omit(dt2$LETINR+rnorm(dim(dt2)[1],mean=0,sd=.1))

#pdf("/Users/danhop/Dropbox/Immigration Innumeracy/immiginfo/local08022017.pdf")
#mn <- min(rmat100)
#mx <- max(rmat100)
mn <- 0.5
mx <- 5.5

k <- 1.25
#Supports Restricting Immigration
plot(0:(length(rmat103[,2])),type="n",main="",xlim=c(mn,mx),yaxt="n",ylim=c(0.5,length(rmat103[,1])+.5),ylab="",xlab="Preferred Levels",cex.main=1.8,cex.lab=1.5,xaxt="n")
axis(side=1,at=c(1,3,5),labels=c("Increase a lot (1)","3","Reduce a lot (5)"))
for(i in 1:length(rmat103[,1])){
	points(x=rmat103[i,2],y=k,pch=19)
	lines(x=c(rmat103[i,1],rmat103[i,3]),y=c(k,k))
	text(rownames(rmat103)[i],y=k+.45,x=rmat103[i,2],cex=.8)
	k <- k+.5
}

for(i in 1:length(dens)){
	lines(x=c(dens[i],dens[i]),y=c(.55,.65))
}

text(x=2.7,y=1.25,"Local Info")
text(x=2.7,y=1.75,"Local Guess")

#dev.off()

### partisanship
dta2$PID <- NA
dta2$PID[dta2$xparty7=="Strong Republican"] <- 7
dta2$PID[dta2$xparty7=="Not Strong Republican"] <- 6
dta2$PID[dta2$xparty7=="Leans Republican"] <- 5
dta2$PID[dta2$xparty7=="Undecided/Independent/Other"] <- 4
dta2$PID[dta2$xparty7=="Leans Democrat"] <- 3
dta2$PID[dta2$xparty7=="Not Strong Democrat"] <- 2
dta2$PID[dta2$xparty7=="Strong Democrat"] <- 1

lout <- lm(NATGUESS ~ PID+EDYEARS+THREATEN,data=dta2[dta2$HISP==0,])

#### MC manipulation check
M <- 10000
dtaMC$GUESSAMT <- as.numeric(as.character(dtaMC$EXP7))
dtaMC$GUESSAMT[dtaMC$EXP7==",14"] <- 14
dtaMC$GUESSAMT[dtaMC$EXP7==",70"] <- 70

dtaMC$HISP <- 1*(dtaMC$xdemHispBin %in% c(1))

lout.MC.lc <- lm(GUESSAMT ~ GUESS+CORRECT+INFO,data=dtaMC[dtaMC$HISP==0,])
beta.MC.lc <- mvrnorm(M,mu=summary(lout.MC.lc)$coef[,1],Sigma=vcov(lout.MC.lc))
pout.MC.guess.lc <- beta.MC.lc%*%c(1,1,0,0)
pout.MC.info.lc <- beta.MC.lc%*%c(1,0,0,1)
pout.MC.correct.lc <- beta.MC.lc%*%c(1,0,1,0)
pout.MC.control.lc <- beta.MC.lc%*%c(1,0,0,0)

rmat120 <- matrix(NA,4,3)

rmat120[1,1] <- quantile(pout.MC.info.lc,.025)
rmat120[1,2] <- mean(pout.MC.info.lc)
rmat120[1,3] <- quantile(pout.MC.info.lc,.975)

rmat120[2,1] <- quantile(pout.MC.correct.lc,.025)
rmat120[2,2] <- mean(pout.MC.correct.lc)
rmat120[2,3] <- quantile(pout.MC.correct.lc,.975)

rmat120[3,1] <- quantile(pout.MC.control.lc,.025)
rmat120[3,2] <- mean(pout.MC.control.lc)
rmat120[3,3] <- quantile(pout.MC.control.lc,.975)

rmat120[4,1] <- quantile(pout.MC.guess.lc,.025)
rmat120[4,2] <- mean(pout.MC.guess.lc)
rmat120[4,3] <- quantile(pout.MC.guess.lc,.975)

dens <- na.omit(dtaMC$GUESSAMT[dtaMC$HISP==0]+rnorm(length(dtaMC$GUESSAMT[dtaMC$HISP==0]),mean=0,sd=.25))

sample.vec <- sample(x=c(0,1),length(dens),prob=c(.9,.1),replace=T)
dens2 <- dens[sample.vec==1]


#pdf("/Users/danhop/Dropbox/ImmigrationInnumeracy/immiginfo/manipulation08032017.pdf")
#mn <- min(rmat100)
#mx <- max(rmat100)
mn <- 9.5
mx <- 40.5

k <- 1
#Supports Restricting Immigration
plot(0:(length(rmat120[,2])),type="n",main="",xlim=c(mn,mx),yaxt="n",ylim=c(0,length(rmat120[,1])+.5),ylab="",xlab="Perceived Pct. Immigrant",cex.main=1.8,cex.lab=1.5,xaxt="n")
axis(side=1,at=c(0,10,20,30,40),labels=c("0 Pct.","10%","20","30","40%"))
for(i in 1:length(rmat120[,1])){
	points(x=rmat120[i,2],y=k,pch=19)
	lines(x=c(rmat120[i,1],rmat120[i,3]),y=c(k,k))
	text(rownames(rmat120)[i],y=k+.45,x=rmat120[i,2],cex=.8)
	k <- k+1
}

text(x=rmat120[1,2],y=1.25,"Information")
text(x=rmat120[2,2],y=2.25,"Correction")
text(x=rmat120[3,2],y=3.25,"Control")
text(x=rmat120[4,2],y=4.25,"Guess")


for(i in 1:length(dens2)){
	lines(x=c(dens2[i],dens2[i]),y=c(.25,.35))
}

abline(v=13,lty=2)
text(x=16,y=3.75,"Census \n Estimate: 13%")

#dev.off()


#######
library(MASS)
M <- 10000
### default == guess
lout.pmx06 <- lm(LETINR ~ CORRECT,data=dta31)
beta.pmx06 <- mvrnorm(M,mu=summary(lout.pmx06)$coef[,1],Sigma=vcov(lout.pmx06))
pout.pmx06.correct <- beta.pmx06%*%c(1,1)
pout.pmx06.guess <- beta.pmx06%*%c(1,0)

### default = guess
lout.kn08 <- lm(LETINR ~ INFO,data=dt2)
beta.kn08 <- mvrnorm(M,mu=summary(lout.kn08)$coef[,1],Sigma=vcov(lout.kn08))
pout.kn08.guess <- beta.kn08%*%c(1,0)
pout.kn08.info <- beta.kn08%*%c(1,1)

#### default = guess
lout.kn10 <- lm(LETINR ~ INFO+CONTROL+CORRECT,data=dta2)
beta.kn10 <- mvrnorm(M,mu=summary(lout.kn10)$coef[,1],Sigma=vcov(lout.kn10))
pout.kn10.correct <- beta.kn10%*%c(1,0,0,1)
pout.kn10.guess <- beta.kn10%*%c(1,0,0,0)
pout.kn10.control <- beta.kn10%*%c(1,0,1,0)
pout.kn10.info <- beta.kn10%*%c(1,1,0,0)

#### default = guess
lout.pmx10 <- lm(LETINR ~ INFO+CONTROL+CORRECT,data=dta20)
beta.pmx10 <- mvrnorm(M,mu=summary(lout.pmx10)$coef[,1],Sigma=vcov(lout.pmx10))
pout.pmx10.correct <- beta.pmx10%*%c(1,0,0,1)
pout.pmx10.guess <- beta.pmx10%*%c(1,0,0,0)
pout.pmx10.control <- beta.pmx10%*%c(1,0,1,0)
pout.pmx10.info <- beta.pmx10%*%c(1,1,0,0)

#### joint 2010
dta$SURVEY <- dta2$SURVEY <- "KN2010"
dta10$SURVEY <- dta20$SURVEY <- "PMX2010"

### dta10 -- dta20
### dta -- dta2

vars.check <- c("INDEX","LETINR","INFO","CONTROL","CORRECT","GUESS","SURVEY","EDYEARS")

#dta$INFO <- 

dta200K <- subset(dta,select=c("INDEX","LETINR","INFO","CONTROL","CORRECT","GUESS","SURVEY","EDYEARS"))
dta200P <- subset(dta10,select=c("INDEX","LETINR","INFO","CONTROL","CORRECT","GUESS","SURVEY","EDYEARS"))

dta10j <- as.data.frame(rbind(dta200K,dta200P))
dta10j$SURVEY1 <- (dta10j$SURVEY=="KN2010")*1

lout.all10 <- lm(LETINR ~ INFO+CONTROL+CORRECT+SURVEY1,data=dta10j)
beta.all10 <- mvrnorm(M,mu=summary(lout.all10)$coef[,1],Sigma=vcov(lout.all10))
pout.all10.correct <- beta.all10%*%c(1,0,0,1,1)
pout.all10.guess <- beta.all10%*%c(1,0,0,0,1)
pout.all10.control <- beta.all10%*%c(1,0,1,0,1)
pout.all10.info <- beta.all10%*%c(1,1,0,0,1)

##### morning consult 

dtaMC$HISP <- 1*(dtaMC$xdemHispBin %in% c(1))

lout.mc17 <- lm(LETINR ~ INFO+CONTROL+CORRECT,data=dtaMC[dtaMC$HISP==0,])
beta.mc17 <- mvrnorm(M,mu=summary(lout.mc17)$coef[,1],Sigma=vcov(lout.mc17))
pout.mc17.correct <- beta.mc17%*%c(1,0,0,1)
pout.mc17.guess <- beta.mc17%*%c(1,0,0,0)
pout.mc17.control <- beta.mc17%*%c(1,0,1,0)
pout.mc17.info <- beta.mc17%*%c(1,1,0,0)

### need to change to include joint results
rmat100 <- matrix(NA,16,3)

rmat100[1,1] <- quantile(pout.pmx06.correct,.025)
rmat100[1,2] <- mean(pout.pmx06.correct)
rmat100[1,3] <- quantile(pout.pmx06.correct,.975)

rmat100[2,1] <- quantile(pout.pmx06.guess,.025)
rmat100[2,2] <- mean(pout.pmx06.guess)
rmat100[2,3] <- quantile(pout.pmx06.guess,.975)

rmat100[3,1] <- quantile(pout.kn08.info,.025)
rmat100[3,2] <- mean(pout.kn08.info)
rmat100[3,3] <- quantile(pout.kn08.info,.975)

rmat100[4,1] <- quantile(pout.kn08.guess,.025)
rmat100[4,2] <- mean(pout.kn08.guess)
rmat100[4,3] <- quantile(pout.kn08.guess,.975)

rmat100[5,1] <- quantile(pout.kn10.info,.025)
rmat100[5,2] <- mean(pout.kn10.info)
rmat100[5,3] <- quantile(pout.kn10.info,.975)

rmat100[6,1] <- quantile(pout.kn10.correct,.025)
rmat100[6,2] <- mean(pout.kn10.correct)
rmat100[6,3] <- quantile(pout.kn10.correct,.975)

rmat100[7,1] <- quantile(pout.kn10.control,.025)
rmat100[7,2] <- mean(pout.kn10.control)
rmat100[7,3] <- quantile(pout.kn10.control,.975)

rmat100[8,1] <- quantile(pout.kn10.guess,.025)
rmat100[8,2] <- mean(pout.kn10.guess)
rmat100[8,3] <- quantile(pout.kn10.guess,.975)

rmat100[9,1] <- quantile(pout.pmx10.info,.025)
rmat100[9,2] <- mean(pout.pmx10.info)
rmat100[9,3] <- quantile(pout.pmx10.info,.975)

rmat100[10,1] <- quantile(pout.pmx10.correct,.025)
rmat100[10,2] <- mean(pout.pmx10.correct)
rmat100[10,3] <- quantile(pout.pmx10.correct,.975)

rmat100[11,1] <- quantile(pout.pmx10.control,.025)
rmat100[11,2] <- mean(pout.pmx10.control)
rmat100[11,3] <- quantile(pout.pmx10.control,.975)

rmat100[12,1] <- quantile(pout.pmx10.guess,.025)
rmat100[12,2] <- mean(pout.pmx10.guess)
rmat100[12,3] <- quantile(pout.pmx10.guess,.975)

####
rmat100[13,1] <- quantile(pout.mc17.info,.025)
rmat100[13,2] <- mean(pout.mc17.info)
rmat100[13,3] <- quantile(pout.mc17.info,.975)

rmat100[14,1] <- quantile(pout.mc17.correct,.025)
rmat100[14,2] <- mean(pout.mc17.correct)
rmat100[14,3] <- quantile(pout.mc17.correct,.975)

rmat100[15,1] <- quantile(pout.mc17.control,.025)
rmat100[15,2] <- mean(pout.mc17.control)
rmat100[15,3] <- quantile(pout.mc17.control,.975)

rmat100[16,1] <- quantile(pout.mc17.guess,.025)
rmat100[16,2] <- mean(pout.mc17.guess)
rmat100[16,3] <- quantile(pout.mc17.guess,.975)

### switch to include joint results
joint.results <- F
if(joint.results==T){

	rmat100[11,1] <- quantile(pout.all10.guess,.025)
	rmat100[11,2] <- mean(pout.all10.guess)
	rmat100[11,3] <- quantile(pout.all10.guess,.975)


	rmat100[13,1] <- quantile(pout.all10.info,.025)
	rmat100[13,2] <- mean(pout.all10.info)
	rmat100[13,3] <- quantile(pout.all10.info,.975)

	rmat100[14,1] <- quantile(pout.all10.correct,.025)
	rmat100[14,2] <- mean(pout.all10.correct)
	rmat100[14,3] <- quantile(pout.all10.correct,.975)

	rmat100[15,1] <- quantile(pout.all10.control,.025)
	rmat100[15,2] <- mean(pout.all10.control)
	rmat100[15,3] <- quantile(pout.all10.control,.975)

	rmat100[16,1] <- quantile(pout.all10.guess,.025)
	rmat100[16,2] <- mean(pout.all10.guess)
	rmat100[16,3] <- quantile(pout.all10.guess,.975)

####
	rmat100[17,1] <- quantile(pout.mc17.info,.025)
	rmat100[17,2] <- mean(pout.mc17.info)
	rmat100[17,3] <- quantile(pout.mc17.info,.975)

	rmat100[18,1] <- quantile(pout.mc17.correct,.025)
	rmat100[18,2] <- mean(pout.mc17.correct)
	rmat100[18,3] <- quantile(pout.mc17.correct,.975)

	rmat100[19,1] <- quantile(pout.mc17.control,.025)
	rmat100[19,2] <- mean(pout.mc17.control)
	rmat100[19,3] <- quantile(pout.mc17.control,.975)

	rmat100[20,1] <- quantile(pout.mc17.guess,.025)
	rmat100[20,2] <- mean(pout.mc17.guess)
	rmat100[20,3] <- quantile(pout.mc17.guess,.975)

	rownames(rmat100) <- c("pmx06.correct","pmx06.guess","kn08.info","kn08.guess","kn10.info","kn10.correct","kn10.control","kn10.guess","pmx10.info","pmx10.correct","pmx10.control","pmx10.guess","all10.info","all10.correct","all10.control","all10.guess",
"mc17.info","mc17.correct","mc17.control","mc17.guess"
)

}

rownames(rmat100) <- c("pmx06.correct","pmx06.guess","kn08.info","kn08.guess","kn10.info","kn10.correct","kn10.control","kn10.guess","pmx10.info","pmx10.correct","pmx10.control","pmx10.guess",
"mc17.info","mc17.correct","mc17.control","mc17.guess"
)

#save(rmat100,file="restrictresults02112016.Rdata")
#load("/users/danhop/Dropbox/ImmigrationInnumeracy/immiginfo/restrictresults02112016.Rdata")

#pdf("/Users/danhop/Dropbox/ImmigrationInnumeracy/immiginfo/letinall03192018.pdf")

### add rug
#mn <- min(rmat100)
#mx <- max(rmat100)
mn <- 0.5
mx <- 5.5#4.25

rn2 <- c("Correction","Guess",
"Information","Guess",
"Information","Correction","Control","Guess",
"Information","Correction","Control","Guess",
"Information","Correction","Control","Guess")
#"Information","Correction","Control","Guess"

cbind(rn2,rownames(rmat100))

kk <- 1
k <- 20
#Supports Restricting Immigration
plot(1:(length(rmat100[,2])+5),type="n",main="",xlim=c(mn,mx),yaxt="n",ylim=c(0,length(rmat100[,1])+5),ylab="",
xlab="Preferred Levels",cex.main=1.8,cex.lab=1.5,xaxt="n")
lbs2 <- c("Increase a lot","Decrease a lot")
axis(side=1,labels=lbs2,at=c(1,5))
for(i in 1:length(rmat100[,1])){
	points(x=rmat100[i,2],y=k,pch=19)
	lines(x=c(rmat100[i,1],rmat100[i,3]),y=c(k,k))
	text(rn2[i],y=k+.5,x=rmat100[i,2],cex=.78)
	if(! kk %in% c(2,4,8,12)){
		k <- k-1
	}else{
		k <- k-2
	}
	kk <- kk+1
}

dens <- na.omit(dta10j$LETINR+rnorm(dim(dta10j)[1],mean=0,sd=.1))
for(i in 1:length(dens)){
	lines(x=c(dens[i],dens[i]),y=c(.05,.25))
}

text(x=2.2,y=19.5,"2006 CCES")
text(x=2.20,y=16.5,"2008 KN")
text(x=2.20,y=12.5,"2010 KN")
text(x=2.20,y=7.5,"2010 CCES")
text(x=2.20,y=2.5,"2017 Morning Consult")
#text(x=4.20,y=1.5,"2017, Morning Consult")

#dev.off()

###################
###################

library(MASS)
M <- 10000

### default = guess
lout.kn08 <- lm(INDEX ~ INFO,data=dt2)
beta.kn08 <- mvrnorm(M,mu=summary(lout.kn08)$coef[,1],Sigma=vcov(lout.kn08))
pout.kn08.guess <- beta.kn08%*%c(1,0)
pout.kn08.info <- beta.kn08%*%c(1,1)

#### default = guess
lout.kn10 <- lm(INDEX ~ INFO+CONTROL+CORRECT,data=dta2)
beta.kn10 <- mvrnorm(M,mu=summary(lout.kn10)$coef[,1],Sigma=vcov(lout.kn10))
pout.kn10.correct <- beta.kn10%*%c(1,0,0,1)
pout.kn10.guess <- beta.kn10%*%c(1,0,0,0)
pout.kn10.control <- beta.kn10%*%c(1,0,1,0)
pout.kn10.info <- beta.kn10%*%c(1,1,0,0)

#### default = guess
lout.pmx10 <- lm(INDEX ~ INFO+CONTROL+CORRECT,data=dta20)
beta.pmx10 <- mvrnorm(M,mu=summary(lout.pmx10)$coef[,1],Sigma=vcov(lout.pmx10))
pout.pmx10.correct <- beta.pmx10%*%c(1,0,0,1)
pout.pmx10.guess <- beta.pmx10%*%c(1,0,0,0)
pout.pmx10.control <- beta.pmx10%*%c(1,0,1,0)
pout.pmx10.info <- beta.pmx10%*%c(1,1,0,0)

#### joint 2010
dta$SURVEY <- dta2$SURVEY <- "KN2010"
dta10$SURVEY <- dta20$SURVEY <- "PMX2010"

### dta10 -- dta20
### dta -- dta2

dta200K <- subset(dta,select=c("INDEX","LETINR","INFO","CONTROL","CORRECT","GUESS","SURVEY","EDYEARS"))
dta200P <- subset(dta10,select=c("INDEX","LETINR","INFO","CONTROL","CORRECT","GUESS","SURVEY","EDYEARS"))

dta10j <- as.data.frame(rbind(dta200K,dta200P))
dta10j$SURVEY1 <- (dta10j$SURVEY=="KN2010")*1


#lout.all10t <- lm(LETINR ~ INFO+CONTROL+CORRECT+SURVEY1+EDYEARS,data=dta10j)
#lout.all10t2 <- lm(INDEX ~ INFO+CONTROL+CORRECT+SURVEY1+EDYEARS,data=dta10j)

lout.all10 <- lm(INDEX ~ INFO+CONTROL+CORRECT+SURVEY1,data=dta10j)
beta.all10 <- mvrnorm(M,mu=summary(lout.all10)$coef[,1],Sigma=vcov(lout.all10))
pout.all10.correct <- beta.all10%*%c(1,0,0,1,1)
pout.all10.guess <- beta.all10%*%c(1,0,0,0,1)
pout.all10.control <- beta.all10%*%c(1,0,1,0,1)
pout.all10.info <- beta.all10%*%c(1,1,0,0,1)

dtaMC$HISP <- 1*(dtaMC$xdemHispBin %in% c(1))

##### morning consult 
lout.mc17 <- lm(INDEX ~ INFO+CONTROL+CORRECT,data=dtaMC[dtaMC$HISP==0,])
beta.mc17 <- mvrnorm(M,mu=summary(lout.mc17)$coef[,1],Sigma=vcov(lout.mc17))
pout.mc17.correct <- beta.mc17%*%c(1,0,0,1)
pout.mc17.guess <- beta.mc17%*%c(1,0,0,0)
pout.mc17.control <- beta.mc17%*%c(1,0,1,0)
pout.mc17.info <- beta.mc17%*%c(1,1,0,0)

rmat101 <- matrix(NA,14,3)

rmat101[1,1] <- quantile(pout.kn08.info,.025)
rmat101[1,2] <- mean(pout.kn08.info)
rmat101[1,3] <- quantile(pout.kn08.info,.975)

rmat101[2,1] <- quantile(pout.kn08.guess,.025)
rmat101[2,2] <- mean(pout.kn08.guess)
rmat101[2,3] <- quantile(pout.kn08.guess,.975)

rmat101[3,1] <- quantile(pout.kn10.info,.025)
rmat101[3,2] <- mean(pout.kn10.info)
rmat101[3,3] <- quantile(pout.kn10.info,.975)

rmat101[4,1] <- quantile(pout.kn10.correct,.025)
rmat101[4,2] <- mean(pout.kn10.correct)
rmat101[4,3] <- quantile(pout.kn10.correct,.975)

rmat101[5,1] <- quantile(pout.kn10.control,.025)
rmat101[5,2] <- mean(pout.kn10.control)
rmat101[5,3] <- quantile(pout.kn10.control,.975)

rmat101[6,1] <- quantile(pout.kn10.guess,.025)
rmat101[6,2] <- mean(pout.kn10.guess)
rmat101[6,3] <- quantile(pout.kn10.guess,.975)

rmat101[7,1] <- quantile(pout.pmx10.info,.025)
rmat101[7,2] <- mean(pout.pmx10.info)
rmat101[7,3] <- quantile(pout.pmx10.info,.975)

rmat101[8,1] <- quantile(pout.pmx10.correct,.025)
rmat101[8,2] <- mean(pout.pmx10.correct)
rmat101[8,3] <- quantile(pout.pmx10.correct,.975)

rmat101[9,1] <- quantile(pout.pmx10.control,.025)
rmat101[9,2] <- mean(pout.pmx10.control)
rmat101[9,3] <- quantile(pout.pmx10.control,.975)

rmat101[10,1] <- quantile(pout.pmx10.guess,.025)
rmat101[10,2] <- mean(pout.pmx10.guess)
rmat101[10,3] <- quantile(pout.pmx10.guess,.975)

#rmat101[11,1] <- quantile(pout.all10.info,.025)
#rmat101[11,2] <- mean(pout.all10.info)
#rmat101[11,3] <- quantile(pout.all10.info,.975)

#rmat101[12,1] <- quantile(pout.all10.correct,.025)
#rmat101[12,2] <- mean(pout.all10.correct)
#rmat101[12,3] <- quantile(pout.all10.correct,.975)

#rmat101[13,1] <- quantile(pout.all10.control,.025)
#rmat101[13,2] <- mean(pout.all10.control)
#rmat101[13,3] <- quantile(pout.all10.control,.975)

#rmat101[14,1] <- quantile(pout.all10.guess,.025)
#rmat101[14,2] <- mean(pout.all10.guess)
#rmat101[14,3] <- quantile(pout.all10.guess,.975)

rmat101[11,1] <- quantile(pout.mc17.info,.025)
rmat101[11,2] <- mean(pout.mc17.info)
rmat101[11,3] <- quantile(pout.mc17.info,.975)

rmat101[12,1] <- quantile(pout.mc17.correct,.025)
rmat101[12,2] <- mean(pout.mc17.correct)
rmat101[12,3] <- quantile(pout.mc17.correct,.975)

rmat101[13,1] <- quantile(pout.mc17.control,.025)
rmat101[13,2] <- mean(pout.mc17.control)
rmat101[13,3] <- quantile(pout.mc17.control,.975)

rmat101[14,1] <- quantile(pout.mc17.guess,.025)
rmat101[14,2] <- mean(pout.mc17.guess)
rmat101[14,3] <- quantile(pout.mc17.guess,.975)


#rownames(rmat101) <- c("kn08.info","kn08.guess","kn10.info","kn10.correct","kn10.control","kn10.guess","pmx10.info","pmx10.correct","pmx10.control","pmx10.guess","all10.info","all10.correct","all10.control","all10.guess")

rownames(rmat101) <- c("kn08.info","kn08.guess","kn10.info","kn10.correct","kn10.control","kn10.guess","pmx10.info","pmx10.correct","pmx10.control","pmx10.guess","mc17.info","mc17.correct","mc17.control","mc17.guess")


#pdf("/Users/danhop/Dropbox/ImmigrationInnumeracy/immiginfo/index08032017.pdf")
#mn <- min(rmat101)
#mx <- max(rmat101)
mn <- 2
mx <- 14

rn2 <- c("Information","Guess",
"Information","Correction","Control","Guess",
"Information","Correction","Control","Guess",
"Information","Correction","Control","Guess")

cbind(rn2,rownames(rmat101))

kk <- 1
k <- 17
#Anti-Immigration Views
### add rug plot
plot(1:(length(rmat101[,2])+3),type="n",main="",xlim=c(mn,mx),yaxt="n",ylim=c(0,length(rmat101[,1])+4),ylab="",xlab="Immigration Views",cex.main=1.8,cex.lab=1.5,xaxt="n")
lbs <- c("Most Favorable","Most Unfavorable")
axis(side=1,labels=lbs,at=c(3,13))

for(i in 1:length(rmat101[,1])){
	points(x=rmat101[i,2],y=k,pch=19)
	lines(x=c(rmat101[i,1],rmat101[i,3]),y=c(k,k))
	text(rn2[i],y=k+.5,x=rmat101[i,2],cex=.78)
	if(! kk %in% c(2,7,12,17)){
		k <- k-1
	}else{
		k <- k-2
	}
	kk <- k+1
}

#text(x=4.6,y=1.5,"2006 CCES")
text(x=4.6,y=16.5,"2008 KN")
text(x=4.6,y=12.5,"2010 KN")
text(x=4.6,y=7.5,"2010 CCES")
#text(x=4.6,y=2.5,"2010, Combined")
text(x=4.6,y=2.5,"2017 Morning Consult")

dens10 <- na.omit(dta10j$INDEX+rnorm(dim(dta10j)[1],mean=0,sd=.1))
for(i in 1:length(dens10)){
	lines(x=c(dens10[i],dens10[i]),y=c(.05,.25))
}

dev.off()
