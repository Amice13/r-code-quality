
#### Contents of replication code
# Loads required libraries, imports data,  establishes necessary functions
# Performs necessary transformations to data
# Runs analyses
# Generates outcome values of interest in paper
# Generates figures and tables in paper


#---------------------------------- REPLICATION CODE ---------------------------------#
  
##### Load libraries ----
rm(list=ls())
require(foreign)
require(reshape2)
require(gdata)
require(doBy)
require(BradleyTerry2)
require(corrplot)
require(arm)
require(plotrix)
library(gtools)
library(httr)
library(XML)
library(car)
library(stargazer)
library(htmltab)
library(texreg)
library(xtable)
library(dplyr)
library(readxl)

### Set working directory to file path of folder containing replication code and data
setwd("~/Politicos")

### Function to import 2016 data ----
rmatprocess <- function(rmatinput,party) {

		if(party=="D"){names(rmatinput) <- c("sen1","sen2","win1","win2","t")}
		else{names(rmatinput) <- c("sen1","sen2","win2","win1","t")}
		rmatinput <- rmatinput[rmatinput$sen1!="I am not familiar with these senators",] 
		rmatinput <- rmatinput[rmatinput$sen2!="I am not familiar with these senators",] 

		rmatinput$sen1[rmatinput$sen1=="John McCain (Arizona)"] <- "aaJohn McCain (Arizona)"
		rmatinput$sen2[rmatinput$sen2=="John McCain (Arizona)"] <- "aaJohn McCain (Arizona)"
		
		return(rmatinput)	
}

### Load 2016 replication data ----
load("gop-all-04042021-djh.Rdata")
gopall <- rmatprocess(rmat,"R")
rm(rmat)

load("dem-all-04052021-djh.Rdata")
demall <- rmatprocess(rmat,"D")
rm(rmat)

load("gop-anti-trump-06172018.Rdata")
gopanti <- rmatprocess(rmatanti,"R")
rm(rmatanti)

load("gop-oct16-04042021-djh.Rdata")
gop3 <- rmatprocess(rmat,"R")
rm(rmat)

load("gop-jul16-04042021-djh.Rdata")
gop2 <- rmatprocess(rmat,"R")
rm(rmat)

load("gop-jan16-04052021-djh.Rdata")
gop1 <- rmatprocess(rmat,"R")
rm(rmat)

load("dem-jan16-04052021-djh.Rdata")
dem1 <- rmatprocess(rmat,"D")
rm(rmat)

load("dem-jul16-04052021-djh.Rdata")
dem2 <- rmatprocess(rmat,"D")
rm(rmat)

load("dem-oct16-04052021-djh.Rdata")
dem3 <- rmatprocess(rmat,"D")
rm(rmat)


### Identify and remove duplicate rows before merging
idx <- which((gopall$sen1 %in% demall$sen1) & (gopall$sen2 %in% demall$sen2))
idxd <- which((demall$sen1 %in% gopall$sen1) & (demall$sen2 %in% gopall$sen2))
alle <- rbind(gopall[-idx,],demall)


#### Senator level calculations ----
ag1 <- (aggregate(alle$win1+alle$win2, by=list(sen2=alle$sen2), FUN=sum))
ag1[order(ag1$x),]
sum(ag1$x)

ag2 <- (aggregate(alle$win1+alle$win2, by=list(sen1=alle$sen1), FUN=sum))
ag2[order(ag2$x),]
sum(ag2$x)

### calculate total ties by Senator
ties.sen.1 <-ag1 <- (aggregate(alle$t, by=list(sen1=alle$sen1), FUN=sum))
ag1[order(ag1$x),]
colnames(ties.sen.1) <- c("sen","ties1")

ties.sen.2 <- ag2 <- (aggregate(alle$t, by=list(sen2=alle$sen2), FUN=sum))
ag2[order(ag2$x),]
sum(ag2$x)
colnames(ties.sen.2) <- c("sen","ties2")

ties.joint <- full_join(ties.sen.1,ties.sen.2)
ties.joint$ties2[ties.joint$sen=="Al Franken (Minnesota)"] <- 0
ties.joint$ties1[ties.joint$sen=="Tom Cotton (Arkansas)"] <- 0
ties.joint$ties1[ties.joint$sen=="Tom Udall (New Mexico)"] <- 0


### calculate total ties by Senator
total.sen.1 <-ag1 <- (aggregate(alle$t+alle$win1+alle$win2, by=list(sen1=alle$sen1), FUN=sum))
ag1[order(ag1$x),]
colnames(total.sen.1) <- c("sen","total1")

total.sen.2 <- ag2 <- (aggregate(alle$t+alle$win1+alle$win2, by=list(sen2=alle$sen2), FUN=sum))
ag2[order(ag2$x),]
sum(ag2$x)
colnames(total.sen.2) <- c("sen","total2")

total.joint <- full_join(total.sen.1,total.sen.2)
total.joint$total2[total.joint$sen=="Al Franken (Minnesota)"] <- 0
total.joint$total1[total.joint$sen=="Tom Cotton (Arkansas)"] <- 0
total.joint$total1[total.joint$sen=="Tom Udall (New Mexico)"] <- 0

joint <- full_join(ties.joint,total.joint)
joint$ties <- joint$ties1+joint$ties2
joint$total <- joint$total1+joint$total2
joint$tiespct <- joint$ties/joint$total


#### this code drops non-redundant cases
idx1 <- which(gop1$sen1 %in% dem1$sen1 & gop1$sen2 %in% dem1$sen2)
all1e <- rbind(gop1[-idx1,],dem1)
 
idx2 <- which(gop2$sen1 %in% dem2$sen1 & gop2$sen2 %in% dem2$sen2)
all2e <- rbind(gop2[-idx1,],dem2)
 
idx3 <- which(gop3$sen1 %in% dem3$sen1 & gop3$sen2 %in% dem3$sen2)
all3e <- rbind(gop3[-idx1,],dem3)

all <- rbind(gopall,demall)
all1 <- rbind(gop1,dem1)
all2 <- rbind(gop2,dem2)
all3 <- rbind(gop3,dem3)


# create a complete list of all senators, 
# so that it can be appended and then cut off again. 
rnewvar <- as.factor(c(as.character(gopall$sen1),as.character(gopall$sen2)))
dnewvar <- as.factor(c(as.character(demall$sen1),as.character(demall$sen2)))
newvar <- as.factor(c(as.character(rnewvar), as.character(dnewvar)))


### Bradley Terry model ----
runBT <- function(DATASET){
	n <- length(DATASET$sen1)
	sen1a <- as.factor(c(as.character(DATASET$sen1), as.character(newvar)))
	sen2a <- as.factor(c(as.character(DATASET$sen2), as.character(newvar)))

	DATASET$sen1 <- sen1a[1:n]
	DATASET$sen2 <- sen2a[1:n]

	BTsenate <- BTm(cbind(win1, win2), sen1, sen2, ~ sen, id = "sen", data = DATASET)
	
	BTvalues1 <- BTabilities(BTsenate)	

	abilities <- -1*BTvalues1[,1]
	ses <- BTvalues1[,2]
	
	scale <- max(abilities, na.rm=T)-min(abilities, na.rm=T)

	ADJabilities <- (abilities-min(abilities, na.rm=T))/scale
	ADJses <- ses/scale

	return(cbind(ADJabilities,ADJses))
}

### Estimate Bradley Terry Model
senGOP <- runBT(gopall)
senGOP1 <- runBT(gop1)
senGOP2 <- runBT(gop2)
senGOP3 <- runBT(gop3)
senGOPanti <- runBT(gopanti)
senDEM <- runBT(demall)
senDEM1 <- runBT(dem1)
senDEM2 <- runBT(dem2)
senDEM3 <- runBT(dem3)
senALL <- runBT(alle)
senALL1 <- runBT(all1e)
senALL2 <- runBT(all2e)
senALL3 <- runBT(all3e)

BTresults <- as.data.frame(cbind(senGOP,senGOP1, senGOP2, senGOP3, senGOPanti, senDEM, senDEM1, senDEM2, senDEM3, senALL, senALL1, senALL2, senALL3))

names(BTresults) <- c("GOP", "GOP1", "GOP2", "GOP3", "GOPanti", "DEM", "DEM1", "DEM2", "DEM3", "ALL", "ALL1", "ALL2", "ALL3")
names(BTresults) <- c("GOP", "GOPse", "GOP1", "GOP1se", "GOP2", "GOP2se","GOP3","GOP3se", "GOPanti", "GOPantise","DEM", "DEMse", "DEM1", "DEM1se", "DEM2", "DEM2se","DEM3", "DEM3se", "ALL", "ALLse", "ALL1", "ALL1se","ALL2", "ALL2se","ALL3","ALL3se")

estimates <- (1:26)[odd(1:26)]
standard.errors <- (1:26)[even(1:26)]



### Create Senator-level data ----
BTsenators <- row.names(BTresults)
senators <- c()
for(i in 1:100){
	senators[i] <- strsplit(BTsenators[i], " ")[[1]][2]
}
senators[senators=="Moore"] <- "Capito"

BTresults$senators <- senators
BTresults <- BTresults[sort.list(senators),]
rm(senators)

NOM <- read.csv("S114_members.csv")
NOM <- NOM[sort.list(NOM$bioname),]
NOM$NOM1 <- NOM$nominate_dim1
NOM$NOM2 <- NOM$nominate_dim2

BTdata <- cbind(BTresults,NOM)

BTdata$party_color <- "green"
BTdata$party_color[BTdata$party_code==200] <- "red"
BTdata$party_color[BTdata$party_code==100] <- "blue"

BTdata$NOMse <- .07
BTdata$party.is.GOP <- 0
BTdata$party.is.GOP[BTdata$party_code==200] <- 1

attributes <- read_excel("AttributesDataSenate.xlsx")
BTdata <- merge(BTdata,attributes, all.x=T,by="icpsr")

# Import Senators' trumpscores data from 538
load("fivethirtyeight-12052020.Rdata")

names(trumpscores) <- 
  c("FullSenator",
    "senators",
    "Party",
    "State",
    "TRUMP",
    "TrumpVoteMargin",
    "TrumpPredict")

removeComma= function(s) {gsub(",", "", s, fixed = TRUE)} 
removeStar= function(s) {gsub("*", "", s, fixed = TRUE)} 
removePCT= function(s) {gsub("%", "", s, fixed = TRUE)} 

trumpscores$senators <- removeStar(trumpscores$senators)	
trumpscores$TRUMP <- as.numeric(removePCT(trumpscores$TRUMP))
trumpscores$TrumpVoteMargin <- as.numeric(trumpscores$TrumpVoteMargin)
trumpscores$TrumpPredict <- as.numeric(removePCT(trumpscores$TrumpPredict))

# Remove Scott (FL)
trumpscores <- trumpscores[! (trumpscores$senators=="Scott" & trumpscores$State=="FL"),]

# Merge trumpscores into data set
BTdata <- merge(BTdata, trumpscores, all.x=T,by="senators")

variable.labels <- c("Name", 
"icpsr", 
"Activist Estimate / Pooled Republicans", "Pooled Republicans Standard Error", 
"Activist Estimate / Period 1 Republicans", "Period 1 Republicans Standard Error", 
"Activist Estimate / Period 2 Republicans", "Period 2 Republicans Standard Error", 
"Activist Estimate / Period 3 Republicans", "Period 3 Republicans Standard Error", 
"Activist Estimate / anti-Trump Republicans", "anti-Trump Republicans Standard Error",
"Activist Estimate / Pooled Democrats", "Pooled Democrats Standard Error", 
"Activist Estimate / Period 1 Democrats", "Period 1 Democrats Standard Error", 
"Activist Estimate / Period 2 Democrats", "Period 2 Democrats Standard Error", 
"Activist Estimate / Period 3 Democrats", "Period 3 Democrats Standard Error", 
"Activist Estimate / Pooled Respondents", "Pooled Respondents Standard Error", 
"Activist Estimate / Period 1 Respondents", "Period 1 Respondents Standard Error", 
"Activist Estimate / Period 2 Respondents", "Period 2 Respondents Standard Error", 
"Activist Estimate / Period 3 Respondents", "Period 3 Respondents Standard Error", 
"Congress", 
"chamber", 
"state_icpsr", 
"district_code", 
"state_abbrev", 
"party_code", 
"occupancy", 
"last_means", 
"bioname", 
"bioguide_id", 
"born", 
"died", 
"DW-NOMINATE Dimension 1",
"DW-NOMINATE Dimension 2",
"nominate_log_likelihood", 
"nominate_geo_mean_probability", 
"nominate_number_of_votes", 
"nominate_number_of_errors", 
"conditional", 
"Nokken-Poole Dimension 1",
"Nokken-Poole Dimension 2",
"DW-NOMINATE Dimension 1",
"DW-NOMINATE Dimension 2",
"party_color",                
"NOMse",               
"GOP=1",               
"X",              
"Male",             
"Senator's Anti-Trump Score",                    
"NOM1se",  
"NOM2se", 
"Name",
"FullSenator",
"Party",
"State",
"538 Trump Score",     
"TrumpVoteMargin",    
"TrumpPredict")


# Correct Party variable
BTdata$Party[BTdata$Party %in% c(NA) & BTdata$senators %in% c("Boxer","Casey,","Mikulski",
                                                              "Reid")] <- "D"
BTdata$Party[BTdata$Party %in% c(NA) & BTdata$senators %in% c("Ayotte","Coats","Kirk",
                                                              "Vitter")] <- "R"

joint$senators <- NA
for(i in 1:dim(joint)[1]){
  joint$senators[i] <- strsplit(joint$sen," ")[[i]][2]
}
joint$senators[joint$senators=="Moore"] <- "Capito"

BTdata1 <- full_join(BTdata,joint,by="senators")


# Figure A6 (ties)
CEX <- 0.8
pdf("ties-by-perceived-ideo-04092021.pdf")
plot(BTdata1$tiespct~BTdata1$ALL, col=0,bty="n",
     ylab="Share Tied",xlab="Perceived Conservatism",cex.lab=1.25)#, ylim=ylimit, xlim=xlimit, ylab=ylabel, xlab=xlabel, cex.lab=CEX, cex.axis=CEX)
text(y=BTdata1$tiespct,x=BTdata1$ALL,BTdata1$senators, cex=CEX, col= BTdata1$party_color)
abline(h=mean(BTdata1$tiespct,na.rm=T),lty=2)
text("Mean=0.40",x=.1,y=mean(BTdata1$tiespct,na.rm=T)+0.02)
dev.off()



#### Make table of Senators and their perceived ideology scores ----

v1 <- BTdata$senators[order(BTdata$ALL)][1:25]
s1 <- round(BTdata$ALL[order(BTdata$ALL)][1:25],digits=3)

v2 <- BTdata$senators[order(BTdata$ALL)][26:50]
s2 <- round(BTdata$ALL[order(BTdata$ALL)][26:50],digits=3)

v3 <- BTdata$senators[order(BTdata$ALL)][51:75]
s3 <- round(BTdata$ALL[order(BTdata$ALL)][51:75],digits=3)

v4 <- BTdata$senators[order(BTdata$ALL)][76:100]
s4 <- round(BTdata$ALL[order(BTdata$ALL)][76:100],digits=3)

# Table A3
xtable(cbind(v1,s1,v2,s2,v3,s3,v4,s4))


### Function to generate histogram ----
BThist <- function(x,split=T){
	pdf(paste("HIST",x,split,"-04052021.pdf",sep=""), width=8,height=8)

	X <- BTdata[,x]
	if(x=="NOM1"){	xlimit <- c(-1.2,1.2)
					ylimit <- c(0,15)
					unit <- .1
					}else{
					if(x=="TRUMP"){	
							xlimit <- c(0,100)
							ylimit <- c(0,40)
							unit <- 10
							}else{	
							xlimit <- c(-.2,1.2)
							ylimit <- c(0,15)}
							unit <- .1
							}
							
	xlabel <- variable.labels[x]
 	br <- seq(min(xlimit),max(xlimit),unit)
	if(split==F){
	hist(X, breaks=br, col="gray50", border="white", xlab=xlabel, xlim=xlimit, main="")

	}else{
	hist(X[BTdata$Party=="R"], breaks=br, col=rgb(1,0,0,0.5), border="white", xlab= xlabel, xlim=xlimit, main="", ylim=ylimit)
	hist(X[BTdata$Party=="D"], breaks=br, col=rgb(0,0,1,0.5), border="white", xlab=xlabel, add=T)
	text(xlimit[1]+c(.05,.95)*(xlimit[2]-xlimit[1]),c(14,14),c("Democratic","Republican"), col=c("blue","red"), cex=.8)
	text(xlimit[1]+c(.05,.95)*(xlimit[2]-xlimit[1]),c(13.6,13.6),c("Senators","Senators"), col=c("blue","red"), cex=.8)
	}
	dev.off()
}

# Figure 1
BThist("ALL")


BTplot <- function(x,y){
	pdf(file=paste(x,"vs",y,"-06102021.pdf", sep=""), height=8,width=8)
	Y <- BTdata[,y]
	X <- BTdata[,x]
	CEX <- .8
	if(y=="NOM1"){ylimit <- c(-1.2,1.2)}else{
		if(y=="TRUMP"){ylimit <- c(0,100)}else{ylimit <- c(-.2,1.2)}}
	if(x=="NOM1"){xlimit <- c(-1.2,1.2)}else{
		if(x=="TRUMP"){xlimit <- c(0,100)}else{xlimit <- c(-.2,1.2)}}
	ylabel <- variable.labels[y]
	xlabel <- variable.labels[x]
	
	plot(Y~X, col=0,bty="n", ylim=ylimit, xlim=xlimit, ylab=ylabel, xlab=xlabel, cex.lab=CEX, cex.axis=CEX)
	text(X,Y,BTdata$senators, cex=CEX, col= BTdata$party_color)
	dev.off()
}

# Figure 2
BTplot("NOM1","ALL")


BTplotSE <- function(x,y,xse,yse, names=F){
	pdf(file= paste(x,"vs",y,"_SE-04052021.pdf", sep=""), height=8,width=8)
	Y <- BTdata[,y]
	X <- BTdata[,x]
	Xse <- BTdata[,xse]
	Yse <- BTdata[,yse]
	CEX <- .6
	if(y=="NOM1"){ylimit <- c(-1.2,1.2)}else{ylimit <- c(-.2,1.2)}
	if(x=="NOM1"){xlimit <- c(-1.2,1.2)}else{xlimit <- c(-.2,1.2)}
	ylabel <- variable.labels[y]
	xlabel <- variable.labels[x]
	
	if(names==F){
	plot(Y~X, col= BTdata$party_color,bty="n", ylim=ylimit, xlim=xlimit, ylab=ylabel, xlab=xlabel, cex.lab=CEX, cex.axis=CEX, cex=1,pch=16)}else{
		plot(Y~X, col= 0,bty="n", ylim=ylimit, xlim=xlimit, ylab=ylabel, xlab=xlabel, cex.lab=CEX, cex.axis=CEX, cex=1,pch=16)
			text(X,Y,BTdata$senators, cex=.5, col= BTdata$party_color)
}
	#draw.ellipse(X,Y,Xse*1.96,Yse*1.96, border= BTdata$party_color)
	draw.ellipse(X,Y,Xse*1.96,Yse*1.96, border="gray80")

	dev.off()
}

# Figure A2
BTplotSE("ALL","NOM1", "ALLse","NOMse")


#### Regressions

BTdata$NameState <- rownames(BTdata) <- paste(BTdata$Name,BTdata$State,sep="-")
dta.gop <- subset(BTdata, Party=="R")
dta.dem <- subset(BTdata, ! Party=="R")

dta.gop$TrumpCol <- "black"
dta.gop$TrumpCol[dta.gop$senators %in% c("Sessions",
                                         "Cotton",
                                         "McCain",
                                         "Sasse",
                                         "Flake")] <- "red"

model.0 <- lm(ALL~NOM1, data=BTdata)
model.both <- lm(ALL~NOM1+NOM2, data=BTdata)
model.dem.0 <- lm(ALL~NOM1, data=dta.dem)
model.gop.0 <- lm(ALL~NOM1, dta.gop)
model.gop.2 <- lm(ALL~NOM1+NOM2, dta.gop)
model.hand <- lm(ALL~NOM1+AntiTrump, data=dta.gop)
model.gop.hand2 <- lm(ALL~NOM1+NOM2+AntiTrump, dta.gop)


# Table 1
texreg(list(model.0, model.both, model.dem.0, model.gop.0, model.gop.2, model.hand, model.gop.hand2),
       custom.model.names = c("All", "All", "Democrats", "GOP", "GOP", "GOP", "GOP"),
       stars = 0.05, digits=2, include.adjrs = FALSE)
# Nominate predictive power for Democrats (beta = 0.77) and Republicans (beta = 0.33)


# Figure 3 (until dev.off)
pdf("nominate-ideology-residuals-02122021.pdf")
sort(residuals(model.gop.0))
plot(y=dta.gop$ALL,x=dta.gop$NOM1,pch=16,xlim=c(0,1),ylim=c(.5,1),
     xlab="NOMINATE",ylab="Perceived Ideology",cex.lab=1.35,
     col=dta.gop$TrumpCol,
     main="Comparing Measures: GOP",cex.main=1.5)
abline(model.gop.0)

offset <- 0.025

xx <- dta.gop$NOM1[dta.gop$senators== "Flake"]
yy <- dta.gop$ALL[dta.gop$senators== "Flake"]
text(xx,yy-offset,"Flake",col="red")

xx <- dta.gop$NOM1[dta.gop$senators== "McCain"]
yy <- dta.gop$ALL[dta.gop$senators== "McCain"]
text(xx,yy-offset,"McCain",col="red")

xx <- dta.gop$NOM1[dta.gop$senators== "Sasse"]
yy <- dta.gop$ALL[dta.gop$senators== "Sasse"]
text(xx,yy-offset,"Sasse",col="red")

xx <- dta.gop$NOM1[dta.gop$senators== "Sessions"]
yy <- dta.gop$ALL[dta.gop$senators== "Sessions"]
text(xx,yy-offset,"Sessions",col="red")

xx <- dta.gop$NOM1[dta.gop$senators== "Cotton"]
yy <- dta.gop$ALL[dta.gop$senators== "Cotton"]
text(xx,yy-offset,"Cotton",col="red")

dev.off()


### Figure 4: perceived ideology of senators (from 2021 data)

# Import function (for 2021 data)

rmatprocess <- function(rmatinput,party="D") {
  #names(rmatinput) <- c("sen1","sen2","win1","win2","t")
  if(party=="D"){names(rmatinput) <- c("sen1","sen2","win1","win2","t")}
  else{names(rmatinput) <- c("sen1","sen2","win2","win1","t")}
  rmatinput <- rmatinput[rmatinput$sen1!="I am not familiar with either one or both of these politicians.",] 
  rmatinput <- rmatinput[rmatinput$sen2!="I am not familiar with either one or both of these politicians.",] 
  
  return(rmatinput)	
}

# Bradley Terry model (for 2021 data)
runBT <- function(DATASET){
  n <- length(DATASET$sen1)
  sen1a <- as.factor(c(as.character(DATASET$sen1), as.character(newvar)))
  sen2a <- as.factor(c(as.character(DATASET$sen2), as.character(newvar)))
  
  DATASET$sen1 <- sen1a[1:n]
  DATASET$sen2 <- sen2a[1:n]
  
  BTsenate <- BTm(cbind(win1, win2), sen1, sen2, ~ sen, id = "sen", data = DATASET)
  
  BTvalues1 <- BTabilities(BTsenate)	
  
  abilities <- -1*BTvalues1[,1]
  ses <- BTvalues1[,2]
  
  scale <- max(abilities, na.rm=T)-min(abilities, na.rm=T)
  
  ADJabilities <- (abilities-min(abilities, na.rm=T))/scale
  ADJses <- ses/scale
  
  return(cbind(ADJabilities,ADJses))
}

# Load 2021 replication data

load("spring2021-all-05042021-djh.Rdata")
spring21all <- rmatprocess(rmat)
rm(rmat)
newvar <- as.factor(c(as.character(spring21all$sen1), as.character(spring21all$sen2)))
all4 <- runBT(spring21all)
all4d <- as.data.frame(all4)
all4s <- all4d[order(all4d$ADJabilities),]
all4s$sen <- rownames(all4s)
all4s$ADJabilities2 <- 1-all4s$ADJabilities

load("spring2021-gop-05042021-djh.Rdata")
spring21gop <- rmatprocess(rmatr)
gop4 <- runBT(spring21gop)
gop4d <- as.data.frame(gop4)
gop4s <- gop4d[order(gop4d$ADJabilities),]
gop4s$ADJabilities2 <- 1-gop4s$ADJabilities

load("spring2021-dem-05042021-djh.Rdata")
spring21dem <- rmatprocess(rmatd)
dem4 <- runBT(spring21dem)
dem4d <- as.data.frame(dem4)
dem4s <- dem4d[order(dem4d$ADJabilities),]
dem4s$ADJabilities2 <- 1-dem4s$ADJabilities

colnames(dem4d) <- c("estdem","sedem")
dem4d$sen <- as.character(rownames(dem4d))

colnames(gop4d) <- c("estgop","segop")
gop4d$sen <- as.character(rownames(gop4d))

joint0 <- left_join(dem4d,gop4d,by="sen")
joint1 <- left_join(all4s,joint0,by="sen")

BTsenators <- joint1$sen
senators <- c()
for(i in 1:dim(joint1)[1]){
  senators[i] <- strsplit(BTsenators[i], " ")[[1]][3]
}
senators[senators=="(New"] <- "Yang"
senators[senators=="Nikki"] <- "Haley"
senators[senators=="Michael"] <- "Bloomberg"
senators[senators=="Donald"] <- "Trump"
senators[senators=="Juli?n"] <- "Castro"
senators[senators=="Hillary"] <- "Clinton"
senators[5] <- "Pence"
senators[6] <- "Scott (FL)"
senators[16] <- "Pompeo"
senators[34] <- "Scott (SC)"
senators[senators=="Kamala"] <- "Harris"
senators[senators=="Moore"] <- "Capito"
senators[senators=="Cortez"] <- "Cortez Masto"
senators[senators=="Van"] <- "Van Hollen"

joint1$senators <- senators

dems <- c("Manchin","Tester","Heinrich", "Peters", "Rosen","Cortez Masto",
          "Hickenlooper","Sinema","Menendez", "King","Stabenow", "Kaine", "Reed",
          "Shaheen", "Kelly", "Whitehouse","Hassan","Smith", "Warner","Wyden",
          "Cantwell","Baldwin","Carper","Bloomberg","Cardin","Ray",
          "Casey", "Schatz","Bennet","Klobuchar","Brown", "Coons",
          "Ossoff","Murphy","Van Hollen","Merkley","Murray","Padilla",
          "Castro","Leahy", "Yang",  "Gillibrand","Warnock",  "Blumenthal",
          "Booker","Duckworth","Durbin","Clinton",  "Feinstein","Beto",  "Markey",
          "Buttigieg","Biden", "Hirono","Sanders",  "Schumer", "Warren","Harris")

joint1$party <- "R"
joint1$party[joint1$senators %in% dems] <- "D"

joint1$color <- "blue"
joint1$color[joint1$party=="R"] <- "red"

joint3 <- joint1[order(joint1$ADJabilities2),]
joint3$colorbw <- "darkgray"
joint3$colorbw[joint3$color=="red"] <- "black"

n <- dim(joint3)[1]

# Figure 4 (until dev.off)
pdf("pairwise-scores-bw-06072021.pdf",height=14,width=10)
plot(x=joint3$ADJabilities2,y=1:n,ylab="",yaxt="n",type="n",
     xlab="Pairwise Scores, 2021")
text(x=joint3$ADJabilities2,y=1:n,joint3$senators,col=joint3$colorbw,cex=.85)
text(y=57,x=.8,col="black","More Conservative")
text(y=57,x=.2,col="black","More Liberal")
dev.off()


### Importing data for campaign contribution ideology measures for senators (for Figures A3-5)
senators2016 <- read.csv("senators2016compare.csv")
attach(senators2016)

# Figure A3: comparing the pairwise measure to campaign contribution contributor-based measure of Senator ideology
pdf("pairwisevscfcname.pdf",width=7, height=7)
plot(pairwise, contributor.cfscore, col= 0, pch=16, ylab="Contributor CF score", xlab="Perceived ideology (H&N)", bty="n")
text(pairwise, contributor.cfscore,senator, col= party.color, cex=.5)
dev.off()

# Figure A4: : comparing the pairwise measure to campaign contribution recipient-based measure of Senator ideology
pdf("pairwisevscfrname.pdf",width=7, height=7)
plot(pairwise, recipient.cfscore, col= 0, pch=16, ylab="Recipient CF score", xlab="Perceived ideology (H&N)", bty="n")
text(pairwise, recipient.cfscore,senator, col= party.color, cex=.5)
dev.off()

# Figure A5:  CCES perceived ideology vs. Estimated Pairwise Ideology
pdf("pairwisevsCCESname.pdf",width=7, height=7)
plot(pairwise,CCES.mean, col= "white", pch=16, ylab="CCES estimates", xlab="Perceived ideology (H&N)", bty="n")
text(pairwise,CCES.mean,senator, col= party.color, cex=.5)
dev.off()


### Point estimates / specific values in text

# Number of pairwise comparisons (9030)
sum(alle$win1)+sum(alle$win2)

# Perceived ideology measure standard deviation 
# (0.14 for Democrats, 0.09 for Republicans)
sd(dta.dem$ALL)
sd(dta.gop$ALL)

# Correlation between NOMINATE and perceived pairwise ideology
cor(BTdata$ALL,BTdata$NOM1)
# 0.90 overall
cor(BTdata$GOP[BTdata$party.is.GOP==1], BTdata$NOM1[BTdata$party.is.GOP==1])
# 0.63 among Republicans
cor(BTdata$DEM[BTdata$party.is.GOP==0], BTdata$NOM1[BTdata$party.is.GOP==0])
# 0.76

# Correlation between NOMINATE and CCES perceived ideology
cor(senators2016$CCES.mean, senators2016$pairwise)
# 0.89