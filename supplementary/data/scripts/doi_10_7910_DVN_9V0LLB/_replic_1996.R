# Instructions: 
# Start a new R session, do not load any packages
# Dump all .R and .RData into the same folder
# Replace the path in the next line with the local folder path
setwd("~/Dropbox/Data/Paper-VotosLegenda/Replic")

# Load packages that existed at the time of analysis
library(checkpoint)
checkpoint("2016-01-01"
	, checkpointLocation = getwd())

### Typset description of the data ###
load("data-RDDvereadores.RData")
tmp1996 <- by((d$leg),d$urna,sum,na.rm=T)/by(d$comparecimento,d$urna,sum,na.rm=T)
tmp2000 <- sum(d$leg2000)/sum(d$comparecimento2000)

cat("Simple frequencies suggest our central hypothesis is plausible. For local elections, the first year for which complete data are systematically available is 1996, which is exactly the year in which the roll-out of EV technology began. This means that we cannot determine the level of legenda voting for municipal councilor in previous years. In 1996,",round(tmp1996[2]*100,1),"PCT (relative to turnout) of the votes cast for municipal councilor in municipalities with EV were legenda votes, while the corresponding number in municipalities using paper balot was just",round(tmp1996[1]*100,1),"PCT. In the next local election (2000), all municipalities used EV technology and legenda votes totalled",round(tmp2000*100,1),"PCT of turnout.",sep=" ")

###################################################################
## RDD 
###################################################################

rdd.plot <- function(x,y,treatment,cutpoint,ylabel="",xlabel="Electorate",nbins=6,yaxis=NULL){
	#there are some non compliers, so here recode compliers only
	controls <- x<cutpoint&treatment==F 
	trtmts <- x>cutpoint  
	plot(x,y
			,pch=ifelse(controls,21,ifelse(trtmts,19,17))
			,ylab=ylabel,xlab=xlabel,ylim=yaxis)
	#Lowess
	lines(lowess(cbind(x[controls],y[controls])),col=gray(0.6))
	lines(lowess(cbind(x[trtmts],y[trtmts])),col=gray(0.6))
	#Diff in means
	lines(x[controls],predict(lm(y[controls]~1)),col=gray(0.4),lty=2)
	lines(x[trtmts],predict(lm(y[trtmts]~1)),col=gray(0.4),lty=2)
	#Compute bins
	#Define control units
	bins <-  bins.x <- rep(NA,length(x))
	bins[controls] <- cut(x[controls],breaks=nbins,labels=F)
	bins[trtmts]<- cut(x[trtmts],breaks=nbins,labels=F)+nbins
	bins.y <- as.numeric(as.table(by(y,bins,mean,na.rm=T)))
	bins.x <- as.numeric(as.table(by(x,bins,mean,na.rm=T)))#not the center of the bin, but rather the mean x
	lines(bins.x[1:nbins],bins.y[1:nbins],col=2)
	lines(bins.x[(1+nbins):(2*nbins)],bins.y[(1+nbins):(2*nbins)],col=2)
	points(x=bins.x,y=bins.y,bg=2,pch=23,cex=1.5)
	legend(x="bottomright",legend=c("Paper","EV","EV (Noncomplier)","Average of Bin")
		,pch=c(21,19,17,23),
		,col=c(1,1,1,2),
		,pt.cex=c(1,1,1,1.5),
		,bg=c(1,1,1,2),
		bty="n")
}

rdd <- subset(d,eleitorado<196000+60000&eleitorado>196000-60000)
rdd$forcing <- rdd$eleitorado - 196000
medhdi <- median(rdd$hdi.1991,na.rm=T)

		#impute average values for missing observations
		rdd$nonwhite.1991[is.na(rdd$nonwhite.1991)]<-mean(rdd$nonwhite.1991,na.rm=T)
		rdd$pent.1991[is.na(rdd$pent.1991)]<-mean(rdd$pent.1991,na.rm=T)

#### The RDD
#par(mfrow=c(1,2))
pdf(file="fig-rdd1996.pdf")
rdd.plot(rdd$eleitorado,rdd$leg/rdd$comparecimento
		,rdd$urna,cutpoint=196000,ylabel="Share of Party Label Vote 1996",yaxis=c(0,0.18))
dev.off()

#### The placebo
pdf(file="fig-rdd1996placebo.pdf")
rdd.plot(rdd$eleitorado,rdd$leg2000/rdd$comparecimento2000
		,rdd$urna,cutpoint=196000,ylabel="Share of Party Label Vote 2000",yaxis=c(0,0.18))
dev.off()

####### BALANCE & DENSITY TESTS #######
load("data-RDDvereadores.RData")
rdd <- subset(d,eleitorado<196000+80000&eleitorado>196000-150000)
rdd$forcing <- rdd$eleitorado - 196000
medhdi <- median(rdd$hdi.1991,na.rm=T)

#### Density Plot and Formal McCrary Test########
pdf(file="fig-rdd1996density.pdf")
par(mfrow=c(1,1))
plot(density(rdd$eleitorado),main="",xlab="Electorate")
abline(v=200000)
dev.off()

pdf(file="fig-rdd1996McCrary.pdf")
library(rdd)
tmp <- DCdensity(rdd$eleitorado, 196000, verbose =F, ext.out=T)
mtext("Electorate",side=1,line=3)
abline(v=196000)
dev.off()

cat("The McCrary density test  for 1996 data yields a $theta$ statistic of ",round(tmp$theta,2)," and p-value of ",round(tmp$p,2), ", which does not allow to reject the null of no sorting.",sep="")

load("data-RDDvereadores.RData")
pdf(file="fig-rdd1996rdbalance.pdf")
par(mfrow=c(3,2),mar=c(3,4,1,1))
rdd.plot(rdd$eleitorado,rdd$comparecimento/rdd$eleitorado
		,rdd$urna,cutpoint=196000,ylabel="Turnout 1996")
rdd.plot(rdd$eleitorado,rdd$hdi.1991
		,rdd$urna,cutpoint=196000,ylabel="HDI-M 1991")
rdd.plot(rdd$eleitorado,rdd$pibpc.1996
		,rdd$urna,cutpoint=196000,ylabel="GDP-PC 1996")
rdd.plot(rdd$eleitorado,rdd$psdb.vs.1994
		,rdd$urna,cutpoint=196000,ylabel="PSDB Vote Share 1994")
rdd.plot(rdd$eleitorado,rdd$pt.vs.1994
		,rdd$urna,cutpoint=196000,ylabel="PT Vote Share 1994")
dev.off()

###### OPTIMAL BANDWITH CALCULATIONS #########
##### Sensitive to scale, use with caution ###
### Compute optimal bandwith (save file to do this in STATA)
load("data-RDDvereadores.RData")
library(foreign)
cutpoint <- 196000
d$forcing <- d$eleitorado - cutpoint
d$shareleg <- d$leg/d$comparecimento
dta <- subset(d,select=c(shareleg,eleitorado,urna))
dta <- na.omit(dta[-which(dta$eleitorado<cutpoint&dta$urna==T),])
dta <- dta[-which(dta$eleitorado>cutpoint&dta$urna==F),]
write.dta(dta,file="Imbens-legenda1996.dta")

#Couldn't get it to work in R, we computed it using the above data in Stata
#library(rdd)
#ob <- IKbandwidth(X=(dta$eleitorado-cutpoint)/1000
#	, Y=dta$shareleg*100
#	, cutpoint = NULL
#	, verbose = T
#	,   kernel = "triangular")

library(rdrobust)
obik <-  rdbwselect (y=dta$shareleg,x=(dta$eleitorado-cutpoint)/1000,bwselect="IK")
obcv <-  rdbwselect (y=dta$shareleg,x=(dta$eleitorado-cutpoint)/1000,bwselect="CV")
obs <-  c(IK=as.numeric(obik$bws[,"h"]),CCT=NA,CV=as.numeric(obcv$bws[,"h"]))

##### RDD for "continuous" bands, folowing Bueno and Tuñon's suggestion #####
##### http://thepoliticalmethodologist.com/2015/03/10/graphical-presentation-of-regression-discontinuity-results/
the.bands <- seq(3000,200000,by=1000)
N <- lcllnr.est <- dom.est <- poly.est <-rep(NA,length(the.bands))
lcllnr.ci <- dom.ci <- poly.ci <-matrix(NA,nrow=length(the.bands),ncol=2)
Nlow <- lcllnrlow.est <- domlow.est <- polylow.est <-rep(NA,length(the.bands))
lcllnrlow.ci <- domlow.ci <- polylow.ci <-matrix(NA,nrow=length(the.bands),ncol=2)
dd <- d[-which(d$eleitorado<cutpoint&d$urna==T),] #drop noncompliers
dd <- d[-which(d$eleitorado>cutpoint&d$urna==F),] #drop noncompliers

for(i in 1:length(the.bands)){
	rdd <- subset(dd,eleitorado<cutpoint+the.bands[i]&eleitorado>cutpoint-the.bands[i])
	rdd$forcing <- rdd$eleitorado - cutpoint
	rddlow <- subset(rdd,hdi.1991<=0.655)
	N[i] <- nrow(rdd)
	## Local Linear
	if(nrow(rdd)>4){
	lcllnr <- lm(leg/comparecimento~forcing*urna,data=rdd)
	lcllnr.est[i] <- coef(lcllnr)['urnaTRUE']
	lcllnr.ci[i,] <- confint(lcllnr)['urnaTRUE',]}
	## DifinMeans with Robust St Error (or t.test with conf.interval)
	#dom <- summary(lm(legfed/comparecimento~urna,data=rdd))
	#domSE <- sqrt(diag(vcovHC(lm(legfed/comparecimento~urna,data=rdd),type="HC4m"))[2])
	if(nrow(rdd)>2){
	dom <- t.test(formula=I(leg/comparecimento)~urna,data=rdd)
	dom.est[i] <- as.numeric(diff(dom$estimate))
	dom.ci[i,] <- dom$c[2:1]*-1 #sign and order is reversed
	}
	##  Poplynomial
	if(nrow(rdd)>10){
		poly <- lm(leg/comparecimento~forcing*urna+I(forcing^2)*urna+I(forcing^3)*urna,data=rdd)
		poly.est[i] <- coef(poly)['urnaTRUE']
		poly.ci[i,] <- confint(poly)['urnaTRUE',]}
}

pdf(file="fig-rdd1996allbands.pdf")
plot(c(0,max(the.bands)),c(-.01,.15),type="n",xlab="",ylab="",xaxt="n",bty="n")
polygon(x=c(the.bands,sort(the.bands,decreasing=T)),
		y=c(dom.ci[,1],dom.ci[length(the.bands):1,2]),
		col=gray(0.7),border=gray(0.9))
lines(the.bands,dom.est,col=gray(0.6),lwd=2)	
lines(the.bands,lcllnr.est,col=gray(0.2),lwd=2,lty=2)	
lines(the.bands,poly.est,col=1,lwd=2,lty=3)	
abline(v=obs*1000,col=gray(0.6),lty=3,lwd=2)#rescale opt. bandwith to graph
axis(side=1,line=1,at=c(0,the.bands[(the.bands/10000)==round(the.bands/10000)])
		,labels=1/10000*c(0,the.bands[(the.bands/10000)==round(the.bands/10000)]))
axis(side=1,line=-1,at=obs*1000,tick=F,labels=names(obs),cex=0.6,col.axis=gray(.6))
mtext(side=1,line=3.5,"Bandwidth: Distance from Cutpoint (In 10,000 Voters)")
axis(side=3,at=c(0,the.bands[(the.bands/10000)==round(the.bands/10000)])
		,labels=c(0,N[which((the.bands/10000)==round(the.bands/10000))])
		,col.axis=gray(.6),col=gray(.64),cex=.6)
mtext(side=3,line=3,"Number of Municipalities in Sample",col=gray(.6))
mtext(side=2,line=3,"Estimated Effect")
legend(x="bottom",bty="n",legend=c("Differences of Means","Local Linear","Polynomial")
		,lty=c(1,2,3),col=c(gray(0.6),gray(0.2),1),lwd=2)
dev.off()


####### Analytical Results #######
#### Function to organize tables #####
stack.regs <- function(x,digits=3){##TO create a table of coefficients
	require(reshape)
	if(class(x)=="lm"){x<-summary(x)}
	if(class(x)!="summary.lm"){cat("x should be a lm object\n");break}
	R2 <- x$r.sq
	N <- length(x$residuals)
	x <- as.data.frame(x$coefficients)
	x$vars <- rownames(x)	
	out <- melt.data.frame(x, "vars", variable_name = "desc")
	out <- subset(out,is.element(desc,unique(out$desc)[-3]))
	out <- out[order(out$vars),]
	names(out)[3]<-"v"
	out$v <- round(as.numeric(out$v),digits)
	rownames(out)<-1:nrow(out)
	tmp <- data.frame(vars=c("R2","N"),desc=c("",""),v=c(R2,N))
	out <- rbind(out,tmp)
	out$desc <- car::recode(out$desc,"'Pr(>|t|)'='zP-value'")
	return(out)
	}
	
load("data-RDDvereadores.RData")
dd <- d[-which(d$eleitorado<cutpoint&d$urna==T),] #drop noncompliers
# Wider band (60k)
rdd <- subset(dd,eleitorado<196000+60000&eleitorado>196000-60000)
rdd$forcing <- rdd$eleitorado - 196000

summary(lm(leg/comparecimento~forcing+urna,data=rdd))
regA1 <- summary(lm((leg/comparecimento)~forcing*urna,data=rdd))
	regl <- lm((leg/comparecimento)~forcing,data=subset(rdd,urna==F))
	regr <- lm((leg/comparecimento)~forcing,data=subset(rdd,urna==T))
	predict(regr,newdata=data.frame(forcing=0))-predict(regl,newdata=data.frame(forcing=0))
summary(lm((leg/comparecimento)~forcing*urna+hdi.1991,data=rdd))
regA2 <- summary(lm(leg/comparecimento~forcing*urna,data=subset(rdd,hdi.1991<median(hdi.1991))))
tabA <- merge(stack.regs(regA1),stack.regs(regA2)
	,by=c("vars","desc"),suffixes=c("","lowHDI"))
	
# Nulos and brancos for band
regA3a <- summary(lm((nulosv/comparecimento)~forcing*urna,data=rdd))
regA3b <- summary(lm((brancosv/comparecimento)~forcing*urna,data=rdd))
regA4a <- summary(lm((nulosv/comparecimento)~forcing*urna,data=subset(rdd,hdi.1991<median(hdi.1991))))
regA4b <- summary(lm((brancosv/comparecimento)~forcing*urna,data=subset(rdd,hdi.1991<median(hdi.1991))))
tabA2n <- merge(stack.regs(regA3a),stack.regs(regA4a)
		,by=c("vars","desc"),suffixes=c("","lowHDI"))[12:14,]
		tabA2n$vars <- paste("nulos",tabA2n$vars,sep=".")
tabA2b <-merge(stack.regs(regA3b),stack.regs(regA4b)
		,by=c("vars","desc"),suffixes=c("","lowHDI"))[12:14,]
		tabA2b$vars <- paste("brancos",tabA2b$vars,sep=".")
tabA2 <- rbind(tabA2b,tabA2n)

# JOINT Nulos and brancos (i.e. INVALID) for band
regA5 <- summary(lm(((nulosv+brancosv)/comparecimento)~forcing*urna,data=rdd))
regA6 <- summary(lm(((nulosv+brancosv)/comparecimento)~forcing*urna,data=subset(rdd,hdi.1991<median(hdi.1991))))
tabA3 <- merge(stack.regs(regA5),stack.regs(regA6)
		,by=c("vars","desc"),suffixes=c("","lowHDI"))[12:14,]
tabA3$vars <- paste("invalid",tabA3$vars,sep=".")
	
# Narrow band (45k)
rdd <- subset(dd,eleitorado<196000+45000&eleitorado>196000-45000)
rdd$forcing <- rdd$eleitorado - 196000
regB1 <-  summary(lm((leg/comparecimento)~forcing*urna,data=rdd))
regB2 <-  summary(lm((leg/comparecimento)~forcing*urna,data=subset(rdd,hdi.1991<median(hdi.1991))))
tabB <- merge(stack.regs(regB1),stack.regs(regB2)
	,by=c("vars","desc"),all=T,suffixes=c("","lowHDI"))

# Nulos and brancos for band
regB3a <- summary(lm((nulosv/comparecimento)~forcing*urna,data=rdd))
regB3b <- summary(lm((brancosv/comparecimento)~forcing*urna,data=rdd))
regB4a <- summary(lm((nulosv/comparecimento)~forcing*urna,data=subset(rdd,hdi.1991<median(hdi.1991))))
regB4b <- summary(lm((brancosv/comparecimento)~forcing*urna,data=subset(rdd,hdi.1991<median(hdi.1991))))
tabB2n <- merge(stack.regs(regB3a),stack.regs(regB4a)
		,by=c("vars","desc"),suffixes=c("","lowHDI"))[12:14,]
		tabB2n$vars <- paste("nulos",tabB2n$vars,sep=".")
tabB2b <-merge(stack.regs(regB3b),stack.regs(regB4b)
		,by=c("vars","desc"),suffixes=c("","lowHDI"))[12:14,]
		tabB2b$vars <- paste("brancos",tabB2b$vars,sep=".")
tabB2 <- rbind(tabB2b,tabB2n)

# JOINT Nulos and brancos (i.e. INVALID) for band
regB5 <- summary(lm(((nulosv+brancosv)/comparecimento)~forcing*urna,data=rdd))
regB6 <- summary(lm(((nulosv+brancosv)/comparecimento)~forcing*urna,data=subset(rdd,hdi.1991<median(hdi.1991))))
tabB3 <- merge(stack.regs(regB5),stack.regs(regB6)
		,by=c("vars","desc"),suffixes=c("","lowHDI"))[12:14,]
tabB3$vars <- paste("invalid",tabB3$vars,sep=".")

# Narrowest band (30k)
rdd <- subset(dd,eleitorado<196000+30000&eleitorado>196000-30000)
rdd$forcing <- rdd$eleitorado - 196000
regC1 <- summary(lm(leg/comparecimento~forcing*urna,data=rdd))
regC2 <- summary(lm(leg/comparecimento~forcing*urna,data=subset(rdd,hdi.1991<median(hdi.1991))))
tabC <- merge(stack.regs(regC1),stack.regs(regC2)
	,by=c("vars","desc"),all=T,suffixes=c("","lowHDI"))

# Nulos and brancos for band
regC3a <- summary(lm((nulosv/comparecimento)~forcing*urna,data=rdd))
regC3b <- summary(lm(brancosv/comparecimento~forcing*urna,data=rdd))
regC4a <- summary(lm(nulosv/comparecimento~forcing*urna,data=subset(rdd,hdi.1991<median(hdi.1991))))
regC4b <- summary(lm(brancosv/comparecimento~forcing*urna,data=subset(rdd,hdi.1991<median(hdi.1991))))
tabC2n <- merge(stack.regs(regC3a),stack.regs(regC4a)
		,by=c("vars","desc"),suffixes=c("","lowHDI"))[12:14,]
		tabC2n$vars <- paste("nulos",tabC2n$vars,sep=".")
tabC2b <-merge(stack.regs(regC3b),stack.regs(regC4b)
		,by=c("vars","desc"),suffixes=c("","lowHDI"))[12:14,]
		tabC2b$vars <- paste("brancos",tabC2b$vars,sep=".")
tabC2 <- rbind(tabC2b,tabC2n)

# JOINT Nulos and brancos (i.e. INVALID) for band
regC5 <- summary(lm((nulosv+brancosv)/comparecimento~forcing*urna,data=rdd))
regC6 <- summary(lm((nulosv+brancosv)/comparecimento~forcing*urna,data=subset(rdd,hdi.1991<median(hdi.1991))))
tabC3 <- merge(stack.regs(regC5),stack.regs(regC6)
		,by=c("vars","desc"),suffixes=c("","lowHDI"))[12:14,]
tabC3$vars <- paste("invalid",tabC3$vars,sep=".")

# Dif in mean around narrow mand
regD1 <- lm(leg/comparecimento~urna,data=rdd)
		robSED1 <- sqrt(diag(vcovHC(regD1,type="HC4m")))
		pD1 <- linearHypothesis(regD1, c("urnaTRUE = 0"),vcov.=vcovHC(regD1,type="HC4m"))$P[2]
		pD1intercept <- linearHypothesis(regD1, c("(Intercept) = 0"),vcov.=vcovHC(regD1,type="HC4m"))$P[2]  	
regD2 <- lm(leg/comparecimento~urna,data=subset(rdd,hdi.1991<median(hdi.1991)))
tabD <- merge(stack.regs(summary(regD1)),stack.regs(summary(regD2))
	,by=c("vars","desc"),all=T,suffixes=c("","lowHDI"))
tabD[which(tabD$desc=="Std. Error"),"v"]<-robSED1  #use robust SE
tabD[which(tabD$desc=="zP-value"),"v"]<-c(pD1,pD1intercept)  #use robust SE

# Nulos and brancos for band
regD3a <- summary(lm(nulosv/comparecimento~urna,data=rdd))
regD3b <- summary(lm(brancosv/comparecimento~urna,data=rdd))
regD4a <- summary(lm(nulosv/comparecimento~urna,data=subset(rdd,hdi.1991<median(hdi.1991))))
regD4b <- summary(lm(brancosv/comparecimento~urna,data=subset(rdd,hdi.1991<median(hdi.1991))))
tabD2n <- merge(stack.regs(regD3a),stack.regs(regD4a)
		,by=c("vars","desc"),suffixes=c("","lowHDI"))[6:8,]
		tabD2n$vars <- paste("nulos",tabD2n$vars,sep=".")
tabD2b <-merge(stack.regs(regD3b),stack.regs(regD4b)
		,by=c("vars","desc"),suffixes=c("","lowHDI"))[6:8,]
		tabD2b$vars <- paste("brancos",tabD2b$vars,sep=".")
tabD2 <- rbind(tabD2b,tabD2n)

# JOINT Nulos and brancos (i.e. INVALID) for band
regD5 <- lm((nulosv+brancosv)/comparecimento~urna,data=rdd)
		robSED5 <- sqrt(diag(vcovHC(regD5,type="HC4m")))
		pD5 <- linearHypothesis(regD5, c("urnaTRUE = 0"),vcov.=vcovHC(regD5,type="HC4m"))$P[2]
regD6 <- lm((nulosv+brancosv)/comparecimento~urna,data=subset(rdd,hdi.1991<median(hdi.1991)))
tabD3 <- merge(stack.regs(summary(regD5)),stack.regs(summary(regD6))
		,by=c("vars","desc"),suffixes=c("","lowHDI"))[6:8,]
tabD3$vars <- paste("invalid",tabD3$vars,sep=".")
tabD3[which(tabD3$desc=="Std. Error"),"v"]<-robSED5[2]  #use robust SE
tabD3[which(tabD3$desc=="zP-value"),"v"]<-c(pD5)  #use robust SE


### Assemble master table
library(xtable)
tab <- merge(
			merge(tabA,tabB,by=c("vars","desc"),all=T,suffixes=c("60k","45k")),
		    merge(tabC,tabD,by=c("vars","desc"),all=T,suffixes=c("30k","30kDiff")),
	   by=c("vars","desc"),all=T)  
xtab <- xtable(tab[c(1,12:14,10),-2],digits=3)
caption(xtab) <- "Regression Discontinuity Estimates for 1996"
label(xtab) <- "tab1996"
print(xtab,include.rownames=F
		,caption.placement = "top"
		,hline.after = c(1,4))
		
### Assemble nulos and brancos auxiliary table
tab2 <- merge(
			merge(tabA2,tabB2,by=c("vars","desc"),all=T,suffixes=c("60k","45k")),
		    merge(tabC2,tabD2,by=c("vars","desc"),all=T,suffixes=c("30k","30kDiff")),
	   by=c("vars","desc"),all=T)  
print(xtable(tab2[,-2],digits=3),include.rownames=F)

### Assemble INVALID auxiliary table
tab3 <- merge(
			merge(tabA3,tabB3,by=c("vars","desc"),all=T,suffixes=c("60k","45k")),
		    merge(tabC3,tabD3,by=c("vars","desc"),all=T,suffixes=c("30k","30kDiff")),
	   by=c("vars","desc"),all=T)  
print(xtable(tab3[,-2],digits=3),include.rownames=F)


#### Table in RobustNess Checks ###############
#### DiM + polynomial in "optimal" bandwiths #

# Narrowest band (30k)
load("data-RDDvereadores.RData")
cutpoint <- 196000
dd <- d[-which(d$eleitorado<cutpoint&d$urna==T),] #drop noncompliers
dd <- d[-which(d$eleitorado>cutpoint&d$urna==F),] #drop noncompliers
rdd <- subset(dd,eleitorado<196000+30000&eleitorado>196000-30000)
rdd$forcing <- rdd$eleitorado - 196000
regD1 <- lm(leg/comparecimento~urna,data=rdd)
		robSED1 <- sqrt(diag(vcovHC(regD1,type="HC4m")))[2]
		pD1 <- linearHypothesis(regD1, c("urnaTRUE = 0"),vcov.=vcovHC(regD1,type="HC4m"))$P[2]
DOM <- c(coef=coef(regD1)[2],se=robSED1,p=pD1,bw=30000,N=nrow(rdd))

library(rdrobust)
regIK <- rdrobust(y=dd$leg/dd$comparecimento,x=(dd$eleitorado-cutpoint)/1000,bwselect="IK")
regCCT <- rdrobust(y=dd$leg/dd$comparecimento*100,x=(dd$eleitorado-cutpoint)/1000,bwselect="CCT")
regCV <- rdrobust(y=dd$leg/dd$comparecimento,x=(dd$eleitorado-cutpoint)/1000,bwselect="CV")
CCT <- rep(NA,5)
IK <-  c(summary(regIK)$coef["Conventional",c(1,2,4)],bw=regIK$h*1000,N=sum(regIK$N_l+regIK$N_r))
CV <-  c(summary(regCV)$coef["Conventional",c(1,2,4)],bw=regCV$h*1000,N=sum(regCV$N_l+regCV$N_r))

the.table <- data.frame(DOM,CCT,IK,CV)
xtable(the.table,digits=3)


###########################
##  COATTAILS #############
setwd("~/Dropbox/Data/Paper-VotosLegenda")
load("data-RDDvereadores.RData")
rdd <- subset(d,eleitorado<196000+60000&eleitorado>196000-60000)
rdd$forcing <- rdd$eleitorado - 196000
table(rdd$pref1,rdd$urna)

## find how frequently main parties ran mayoral candidates in rdd set
tmp <- subset(rdd,select=c(pmdb,pfl,psdb,ppb,pdt,ptb,pl,psb,pt,psd))
muns <- as.matrix(sort(nrow(tmp)-apply(is.na(tmp),2,sum),decreasing=T))
print(muns)

the.regs.urna <- list(pt=lm(ptleg/comparecimento~I(pt/comparecimento),data=subset(rdd,urna==T)),
	pmdb=lm(pmdbleg/comparecimento~I(pmdb/comparecimento),data=subset(rdd,urna==T)),
    psdb=lm(psdbleg/comparecimento~I(psdb/comparecimento),data=subset(rdd,urna==T)),
    ppb=lm(ppbleg/comparecimento~I(ppb/comparecimento),data=subset(rdd,urna==T)),
	pdt=lm(pdtleg/comparecimento~I(pdt/comparecimento),data=subset(rdd,urna==T))
	)

the.regs.not <- list(pt=lm(ptleg/comparecimento~I(pt/comparecimento),data=subset(rdd,urna==F)),
	pmdb=lm(pmdbleg/comparecimento~I(pmdb/comparecimento),data=subset(rdd,urna==F)),
    psdb=lm(psdbleg/comparecimento~I(psdb/comparecimento),data=subset(rdd,urna==F)),
    ppb=lm(ppbleg/comparecimento~I(ppb/comparecimento),data=subset(rdd,urna==F)),
	pdt=lm(pdtleg/comparecimento~I(pdt/comparecimento),data=subset(rdd,urna==F))
	)

my.extract <- function(x){summary(x)$coef[2,1:2]}
coefT <- sapply(the.regs.urna,my.extract)
coefF <- sapply(the.regs.not,my.extract)

pdf(file=paste(to.save,"/fig-1996coattails.pdf",sep=""))
library(plotrix)
par(mar=c(5,5,1,1))
my.labels<- paste(toupper(colnames(coefT)),"\n(",muns[1:ncol(coefT)],")",sep="")
plot(c(1,ncol(coefT)),c(-.02,.12),type="n",xlab="",ylab="",xaxt="n",bty="n")
mtext(side=2,line=3.2,"Slopes of Legenda Votes Relative to Mayoral Vote")
axis(side=1,line=1,at=1:ncol(coefT),labels=my.labels,las=1,tick=F)
a<-0.9
require(plotrix)
abline(h=c(0,.02,.04,.06,.08,.1),lty=3,col=gray(0.6))
plotCI(1:ncol(coefT),coefT[1,],uiw=qnorm(a)*coefT[2,],add=T,pch=21,pt.bg=1)
plotCI(1:ncol(coefF),coefF[1,],uiw=qnorm(a)*coefF[2,],add=T,pch=24,pt.bg="white")
legend(x="top",legend=c("Paper Ballots","Electronic Voting"),pch=c(24,21),pt.bg=c("white",1),bty="n",horiz=T)
dev.off()

##############################################
## Pooling all parties and muncipalities in the wide band 
## DIFF in DIFF type of analysis #############
setwd("~/Dropbox/Data/Paper-VotosLegenda")
load("data-RDDvereadores.RData")
to.save <- "~/Dropbox/latexfiles/Paper-VotosdeLegenda/Figures"
band <- 25000 #60000
rdd <- subset(d,eleitorado<196000+band&eleitorado>196000-band)
rdd$forcing <- rdd$eleitorado - 196000

#summary(lm(I(ptleg/comparecimento)~I(is.na(pt)==F)*urna,data=rdd))
#summary(lm(I(psdbleg/comparecimento)~I(is.na(psdb)==F)*urna,data=rdd))

the.parties <- names(sort(apply(is.na(subset(rdd,select=c(pmdb,pfl,psdb,ppb,pdt,ptb,pl,psb,pt,psd)))==F,2,sum),decreasing=T))[1:10]

#### Pool data and assemble a DID like structure for all parties ####
tmp <- subset(rdd,select=paste(the.parties,"leg",sep=""))/rdd$comparecimento
tmp2 <- is.na(subset(rdd,select=the.parties))==F #indicator of running a mayor
### Reshape into DID like structure #####
dd <- data.frame(codeibge=rdd$codeibge,urna=as.numeric(rdd$urna),forcing=rdd$forcing,tmp2,tmp)
library(reshape)
ddd <- reshape(dd,direction="long",
		varying=list(mayor=the.parties,
					  legenda=paste(the.parties,"leg",sep="")))
names(ddd)[5:6] <- c('mayor','legenda') 
Nnot <- sum(tmp2==F)
#### Estimate the simple case (ran or not a mayoral candidate) ####
if(band>25000){
	effects.not <- lm(legenda~forcing*urna,data=subset(ddd,mayor==F))
	pe.not <- coef(effects.not)["urna"]
	ci.not <- confint(effects.not, parm="urna", level = 0.95)
	effects.some <-lm(legenda~forcing*urna,data=subset(ddd,mayor==T))
	pe.some <- coef(effects.some)["urna"]
	ci.some <- confint(effects.some, parm="urna", level = 0.95)
	#THese are just to report, below, in the cat command that typesets the text
	tmpnot <- lm(legenda~urna,data=subset(ddd,mayor==F))
	not.pb <- coef(tmpnot)[1]
	not.ev <- coef(tmpnot)[1]+coef(tmpnot)[2]
	tmpran <- lm(legenda~urna,data=subset(ddd,mayor==F))
	ran.pb <- coef(tmpran)[1]
	ran.ev <- coef(tmpran)[1]+coef(tmpran)[2]
	}else{#Dif in means
	effects.not <- t.test(formula=legenda~urna,data=subset(ddd,mayor==F))
	pe.not <- diff(effects.not$estimate)
	ci.not <-  effects.not$conf[2:1]*-1
	effects.some <- t.test(formula=legenda~urna,data=subset(ddd,mayor==T))
	pe.some <- diff(effects.some$estimate)
	ci.some <-  effects.some$conf[2:1]*-1
	not.pb <- effects.not$est[1]
	not.ev <- effects.not$est[2]
	}

all.effects <- all.N <- list()
### Now do the same for different definitions of "competitive"
for(i in 1:40){
tmp2 <- is.na(subset(rdd,select=the.parties))==F& #indicator of compatitive mayor
			   subset(rdd,select=the.parties)/rdd$comparecimento> i/100
### Reshape into DID like structure #####
dd <- data.frame(codeibge=rdd$codeibge,urna=as.numeric(rdd$urna),forcing=rdd$forcing,tmp2,tmp)
library(reshape)
ddd <- reshape(dd,direction="long",
		varying=list(mayor=the.parties,
					  legenda=paste(the.parties,"leg",sep="")))
names(ddd)[5:6] <- c('mayor','legenda') 
#### Estimate
if(band==25000){#dif in means
	all.effects[[i]] <- t.test(formula=legenda~urna,data=subset(ddd,mayor==T))
	}else{#lcl linear
	all.effects[[i]] <- lm(legenda~forcing*urna,data=subset(ddd,mayor==T))
	}
all.N[[i]] <- sum(tmp2)
}



if(band==25000){#narrow band dif in means
pe <- sapply(all.effects ,function(x){diff(x$estimate)})
ci <- sapply(all.effects ,function(x){c(x$conf[2:1])*-1})
}else{
pe <- sapply(all.effects ,function(x){coef(x)["urna"]})
ci <- sapply(all.effects ,function(x){confint(x, parm="urna", level = 0.95)})	
}
N <- sapply(all.N,"cbind")

### Plot EFFECTS, with and without mayoral candidates ###
pdf(file=paste(to.save,"/fig-1996coattailspooled.pdf",sep=""))
par(mar=c(5,4,1,1))
a <- 0.99
plot(pe,type="l",ylim=c(-.005,0.035),xlab="",ylab="") 
polygon(x=c(1:ncol(ci),ncol(ci):1),
		 y=c(ci[1,],ci[2,ncol(ci):1]),
		 col=gray(.4),border=NA)
lines(pe) ## effect of EV when fielding a competitive canddiaate
abline(h=0,lty=2)
mtext(side=1,line=3,"Competitiveness of Mayoral Candidate (Vote Share)")
mtext(side=2,line=3,"Increase in Party Label Vote Share with EV Technology")
dev.off()



#### Plot SELECTED effects, with no mayoral candidates and selected upticket candidates ###

to.plot <- rbind(not=c(pe.not,ci.not),
	  some=c(pe.some,ci.some),
	  c10=c(pe[10],ci[,10]),
	  c20=c(pe[20],ci[,20]),
	  c30=c(pe[30],ci[,30]),
	  c40=c(pe[40],ci[,40]))

pdf(file=paste(to.save,"/fig-1996coattailspooledselected.pdf",sep=""))
par(mar=c(6,4,1,1))
a <- 0.99
xs <- c(1,seq(1.5,2.5,len=5))
plot(xs,type="n",ylim=c(0,0.03),xlim=c(0.5,3),xlab="",ylab="",bty="n",xaxt="n")
#abline(h=seq(0,0.03,by=0.005),col=gray(0.8)) 
library(plotrix)
plotCI(xs,to.plot[,1],ui=to.plot[,3],li=to.plot[,2],add=T,pch=21,pt.bg=c("white",rep(1,5)))
axis(side=1,line=3.2,at=c(1,2),labels=c("Without\nMayoral Candidate","With\nMayoral Candidate"),tick=F)
axis(side=1,line=-1,at=xs[-1],labels=c("Any",">10%",">20%",">30%",">40%"),las=2)
axis(side=1,line=1.2,at=xs,labels=paste("N=",c(Nnot,N[c(1,10,20,30,40)]),sep=""),tick=F,cex.axis=0.8)
mtext(side=2,line=3.2,"Effect on Party Label Vote Share")
dev.off()



cat("Consider the performance of the 10 main parties in the ",length(unique(rdd$codeibge))," municipalities in the RD band. Legenda vote for parties that did not field a mayoral candidate averaged ",round(100*not.pb,2),"PCT of legenda vote in paper ballot municipalities and ",round(100*not.ev,2),"PCT in EV technology municipalities, a statistically significant increase of ",round(100*pe.not,2)," percentage points (p-value$<",0.01,"$). In municipalities where the party did field a candidate, the EV technology effect relative to paper ballots was substantially larger (",round(100*pe.some,2)," percentage points, p-value $<",0.01,"$).\n\n Not all mayoral candidates, however, are equally competitive. If we change the comparison criteria from having simply fielded a candidate to requiring that candidates obtain higher vote share, an interesting picture emerges. As Figure ref{fig-coattails} shows, the EV effect on legenda votes increases to ",round(100*pe[20],2)," percentage points when parties field mayoral candidates that receive at least 20PCT of the vote, something that happened in ",N[20]," instances in our sample. The EV effect is even more pronounced if we further increase the threshold. \n",sep="")




###### TABLE FOR COMPARING NET EFFECT OF ENFRIANCHISMENT BEGINS HERE ##################

# Local Linear Narrowest band (4k)
load("data-RDDvereadores.RData")
rdd <- subset(d,eleitorado<196000+25000&eleitorado>196000-25000)
rdd$forcing <- rdd$eleitorado - 196000
regLL <- summary(lm(leg/comparecimento~forcing*urna,data=rdd))
#tabC <- merge(stack.regs(regC1),stack.regs(regC2)
#	,by=c("vars","desc"),all=T,suffixes=c("","lowHDI"))

# JOINT Nulos and brancos (i.e. INVALID) for band
regLLi <- summary(lm((nulosv+brancosv)/comparecimento~forcing*urna,data=rdd))

# Dif in mean around narrow band
regD <- lm(leg/comparecimento~urna,data=rdd)
		robSED <- sqrt(diag(vcovHC(regD,type="HC4m")))
		pD <- linearHypothesis(regD, c("urnaTRUE = 0"),vcov.=vcovHC(regD,type="HC4m"))$P[2]
		pDintercept <- linearHypothesis(regD, c("(Intercept) = 0"),vcov.=vcovHC(regD,type="HC4m"))$P[2]  
		###t.test(formula=I(legfed/comparecimento)~urna,data=rdd) #ALTERNATIVE
tabD <- stack.regs(summary(regD))
tabD[which(tabD$desc=="Std. Error"),"v"]<-robSED  #use robust S
tabD[which(tabD$desc=="zP-value"),"v"]<-c(pD,pDintercept)  #use robust SE

# JOINT Nulos and brancos (i.e. INVALID) for band
regDi <-  lm((nulosv+brancosv)/comparecimento~urna,data=rdd)
		robSEDi <- sqrt(diag(vcovHC(regDi,type="HC4m")))
		pDi <- linearHypothesis(regDi, c("urnaTRUE = 0"),vcov.=vcovHC(regDi,type="HC4m"))$P[2]
tabDi <- stack.regs(summary(regDi))
tabDi[which(tabDi$desc=="Std. Error"),"v"]<-robSEDi  #use robust S
tabDi[which(tabDi$desc=="zP-value"),"v"]<-c(pDi,pDintercept)  #use robust SE

# Polynomial Narrowest band (4k)
regP <- summary(lm(leg/comparecimento~forcing*urna+I(forcing^2)+I(forcing^3),data=rdd))

# JOINT Nulos and brancos (i.e. INVALID) for band
regPi <- summary(lm((nulosv+brancosv)/comparecimento~forcing*urna+I(forcing^2)+I(forcing^3),data=rdd))

RDinv <- merge(tabDi[c(1,4:6,8),],
		 merge(stack.regs(regLLi,4)[c(1,10:12),]
		,stack.regs(regPi,4)[c(1,16:18),],by=c("vars","desc"),suffixes=c("LL","POL"))
		,by=c("vars","desc"),all=T)
		RDinv$vars <- paste(RDinv$vars,"invalid",sep=".")
RDplv <- merge(tabD[c(1,4:6,8),],
			merge(stack.regs(regLL,4)[c(1,10:12),]
		,stack.regs(regP,4)[c(1,16:18),],by=c("vars","desc"),suffixes=c("LL","POL"))
		,by=c("vars","desc"),all=T)

the.table <- rbind(RDinv,RDplv)
lb <- the.table[the.table$vars=="urnaTRUE"&the.table$desc=="Estimate",-c(1:2)]*
								(1-to.plot["not",1]/to.plot["some",1])#from the pooledcoattailsanalysis
rownames(the.table)<-paste(the.table$vars,the.table$desc,sep=".")
the.table <- the.table[,-c(1:2)]
the.table <- rbind(the.table,urna.LB=lb)
print(xtable(100*the.table,digits=2))
####### NET ENFRANCHISEMENT ENDS HERE ##########################################################
