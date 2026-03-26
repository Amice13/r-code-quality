# Instructions: 
# Start a new R session, do not load any packages
# Dump all .R and .RData into the same folder
# Replace the path in the next line with the local folder path
setwd("~/Dropbox/Data/Paper-VotosLegenda/Replic")

# Load packages that existed at the time of analysis
library(checkpoint)
checkpoint("2016-01-01"
	, checkpointLocation = getwd())
	
#### After R&R I added variables and computations for invalid votes ####
library(sandwich)
library(car)
library(plotrix)


### Typset description of the data ###
load("data-RDDdepfed.RData")
tmp1994 <- sum(d$legfed.1994,na.rm=T)/sum(d$comparecimento.1994,na.rm=T)
tmp1998 <- by((d$legfed),d$urna,sum,na.rm=T)/by(d$comparecimento,d$urna,sum,na.rm=T)
tmp2002 <- sum(d$legfed.2002,na.rm=T)/sum(d$comparecimento.2002,na.rm=T)

cat("For general elections, the roll-out started in 1998. In the previous election (1994), which used only paper ballots, the average share of legenda votes for Deputado Federal (the first vote of the day) was",round(tmp1994*100,2),". In 1998,",round(tmp1998[2]*100,2),"PCT of the votes in municipalities with EV were for legenda, while only",round(tmp1998[1]*100,2)," were legenda in municipalities that used paper ballots. In  2002, when all municipalities used EV technology, legenda votes totaled ",round(tmp2002*100,2),"PCT of turnout.  For the second, vote, in contrast, there was hardly any change across voting technology.",sep=" ")


###################################################################
## RDD 
################################################### s################

####### Graphical Results #######
load("data-RDDdepfed.RData")
d <- subset(d,state!="RJ"&state!="AL"&state!="AP"&state!="RR")


rdd.plot <- function(x,y,treatment,cutpoint,ylabel="",
		xlabel="Electorate 1998",nbins=6,yaxis=NULL,
		simple=F,mycol=2,legpos="bottomright"){
	#there are some non compliers, so here recode compliers only
	controls <- x<cutpoint&treatment==F 
	trtmts <- x>cutpoint  
	plot(x,y
			,pch=ifelse(controls,21,ifelse(trtmts,19,17))
			,ylab=ylabel,xlab=xlabel,ylim=yaxis,type=ifelse(simple,"n","p"))
	#Lowess
	if(simple==F){
	lines(lowess(cbind(x[controls],y[controls])),col=gray(0.6))
	lines(lowess(cbind(x[trtmts],y[trtmts])),col=gray(0.6))
	#Diff in means
	lines(x[controls],predict(lm(y[controls]~1)),col=gray(0.4),lty=2)
	lines(x[trtmts],predict(lm(y[trtmts]~1)),col=gray(0.4),lty=2)}
	#Compute bins
	#Define control units
	bins <-  bins.x <- rep(NA,length(x))
	bins[controls] <- cut(x[controls],breaks=nbins,labels=F)
	bins[trtmts]<- cut(x[trtmts],breaks=nbins,labels=F)+nbins
	bins.y <- as.numeric(as.table(by(y,bins,mean,na.rm=T)))
	bins.x <- as.numeric(as.table(by(x,bins,mean,na.rm=T)))#not the center of the bin, but rather the mean x
	lines(bins.x[1:nbins],bins.y[1:nbins],col=mycol)
	lines(bins.x[(1+nbins):(2*nbins)],bins.y[(1+nbins):(2*nbins)],col=mycol)
	points(x=bins.x,y=bins.y,bg=mycol,pch=23,cex=1.5)
	if(simple==F){
	legend(x=legpos,legend=c("Paper","EV","EV (Noncomplier)","Average of Bin")
		,pch=c(21,19,17,23),
		,col=c(1,1,1,mycol),
		,pt.cex=c(1,1,1,1.5),
		,bg=c(1,1,1,2),
		bty="n")}
}



band <- 8000 #beautiful
cutpoint <- 40500
rdd <- subset(d,eleitorado.1996<cutpoint+band&eleitorado.1996>cutpoint-band)
rdd$legvs <- rdd$legfed/rdd$comparecimento
rdd$forcing <- rdd$eleitorado.1996 - cutpoint

cat("Number of municipalities (band 2x",band,"):",nrow(rdd),"\n")
#rdd[order(rdd$eleitorado.1996),c("eleitorado.1996","urna1998","urna","mun","state")]

#impute average vlaues for missing observations
rdd$nonwhite.1991[is.na(rdd$nonwhite.1991)]<-mean(rdd$nonwhite.1991,na.rm=T)
rdd$pent.1991[is.na(rdd$pent.1991)]<-mean(rdd$pent.1991,na.rm=T)



#### The RDD
#par(mfrow=c(1,2))
pdf(file="fig-rdd1998.pdf")
rdd.plot(rdd$eleitorado.1996,rdd$legfed/rdd$comparecimento
		,rdd$urna,cutpoint=cutpoint
		,ylabel="Share of Party Label Votes 1998",yaxis=c(0,0.18))
dev.off()

pdf(file="fig-rdd1998placebo1994.pdf")
rdd.plot(rdd$eleitorado.1996,rdd$legfed.1994/rdd$comparecimento.1994
		,rdd$urna,cutpoint=cutpoint,legpos="topright"
		,ylabel="Share of Party Label Votes 1994",yaxis=c(0,0.18))
dev.off()


pdf(file="fig-rdd1998placebo2002.pdf")
rdd.plot(rdd$eleitorado.1996,rdd$legfed.2002/rdd$comparecimento.2002
		,rdd$urna,cutpoint=cutpoint
		,ylabel="Share of Party Label Votes 2002",yaxis=c(0,0.18))
dev.off()




### The placebo graph (simple version)
pdf(file="fig-rdd1998placebo.pdf")
rdd.plot(rdd$eleitorado.1996,rdd$legfed.1994/rdd$comparecimento.1994
		,rdd$urna,cutpoint=cutpoint
		,ylabel="",yaxis=c(0,0.18)
		,simple=T,mycol=1)
par(new=T)
rdd.plot(rdd$eleitorado.1996,rdd$legfed/rdd$comparecimento
		,rdd$urna,cutpoint=cutpoint
		,ylabel="",yaxis=c(0,0.18)
				,simple=T,mycol=2)
par(new=T)	
rdd.plot(rdd$eleitorado.1996,rdd$legfed.2002/rdd$comparecimento#.2002
		,rdd$urna,cutpoint=cutpoint
		,ylabel="Share of Party Label Votes",yaxis=c(0,0.18)
				,simple=T,mycol=3)
abline(v=40500,lty=2,col=gray(.5))
legend(x="topleft",legend=c("1994","1998","2002"),pch=23,lty=1,col=1:3,pt.bg=1:3,bty="n")
dev.off()






####### BALANCE & DENSITY TESTS #######
load("data-RDDdepfed.RData")
d <- subset(d,state!="RJ"&state!="AL"&state!="AP"&state!="RR")
band <- 100000 
cutpoint <- 40500
rdd <- subset(d,eleitorado.1996<cutpoint+band&eleitorado.1996>cutpoint-band)
rdd$legvs <- rdd$legfed/rdd$comparecimento
rdd$forcing <- rdd$eleitorado.1996 - cutpoint

#### Density Plot and Formal McCrary Test########
pdf(file="fig-rdd1998density.pdf")
par(mfrow=c(1,1))
plot(density(rdd$eleitorado.1996,na.rm=T),main="",xlab="1996 Electorate")
abline(v=40500)
dev.off()

pdf(file="fig-rdd1998McCrary.pdf")
library(rdd)
tmp <- DCdensity(rdd$eleitorado.1996, 40500, verbose =F, ext.out=T)
mtext("1996 Electorate",side=1,line=3)
abline(v=40500)
dev.off()

cat("The McCrary density test  for 1998 data yields a $theta$ statistic of ",round(tmp$theta,2)," and p-value of ",round(tmp$p,2), ", which does not allow to reject the null of no sorting.",sep="")

### Balance
pdf(file="fig-rdd1998rdbalance.pdf")
par(mfrow=c(3,2),mar=c(3,4,1,1))
rdd.plot(rdd$eleitorado.1996,rdd$comparecimento/rdd$eleitorado
		,rdd$urna,cutpoint=cutpoint,ylabel="Turnout 1998")
rdd.plot(rdd$eleitorado.1996,rdd$hdi.1991
		,rdd$urna,cutpoint=cutpoint,ylabel="HDI-M 1991")
rdd.plot(rdd$eleitorado.1996,rdd$pibpc.1996
		,rdd$urna,cutpoint=cutpoint,ylabel="GDP-PC 1996"
		,legpos="topleft")
rdd.plot(rdd$eleitorado.1996,rdd$nonwhite.1991
		,rdd$urna,cutpoint=cutpoint,ylabel="Share Non-White")
#rdd.plot(rdd$eleitorado,rdd$distcap
#		,rdd$urna,cutpoint=196000,ylabel="Distance to Capital City")
#rdd.plot(rdd$eleitorado,rdd$fhc.vs.1994
#		,rdd$urna,cutpoint=196000,ylabel="Cardoso Vote Share 1994")
rdd.plot(rdd$eleitorado.1996,rdd$psdb.vs.1994
		,rdd$urna,cutpoint=cutpoint,ylabel="PSDB Vote Share 1994"
		,legpos="topleft")
rdd.plot(rdd$eleitorado.1996,rdd$pt.vs.1994
		,rdd$urna,cutpoint=cutpoint,ylabel="PT Vote Share 1994"
		,legpos="topleft")
dev.off()

####### Analytical Results #######
#### Função mágina para organizar tabelas #####
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
	
	
	
###### OPTIMAL BANDWITH CALCULATIONS #########
##### Sensitive to scale, use with caution ###
library(foreign)
d <- subset(d,state!="RJ"&state!="AL"&state!="AP"&state!="RR")
cutpoint <- 40500

### Compute optimal bandwith (save file to do this in STATA)
d$sharelegfed <- d$legfed/d$comparecimento
dta <- na.omit(subset(d,select=c(sharelegfed,eleitorado,eleitorado.1996,urna)))
dta <- dta[-which(dta$eleitorado.1996<40500&dta$urna==T),]
write.dta(dta,file="Imbens-legenda1998.dta")

library(rdd)
ob <- IKbandwidth(X=(dta$eleitorado.1996-cutpoint)/1000
	, Y=dta$sharelegfed*100
	, cutpoint = NULL, verbose = T
	,   kernel = "triangular")

library(rdrobust)
#rdik <- rdrobust(y=dta$sharelegfed,x=(dta$eleitorado.1996-cutpoint)/1000,bwselect="IK")
#rdcct <- rdrobust(y=dta$sharelegfed,x=(dta$eleitorado.1996-cutpoint)/1000)
oball <-  rdbwselect (y=dta$sharelegfed,x=(dta$eleitorado.1996-cutpoint)/1000, all=T)
obs <-  oball$bws[,"h"]


##### RDD for "continuous" bands, folowing Natalia and Guadalupe's suggestion #####
##### http://thepoliticalmethodologist.com/2015/03/10/graphical-presentation-of-regression-discontinuity-results/
top <- ceiling(max(obs))*1000
the.bands <- seq(1000,top,by=1000)
N <- lcllnr.est <- dom.est <- poly.est <- polyalt.est <- double(length=length(the.bands))
lcllnr.ci <- dom.ci <- poly.ci <- matrix(NA,nrow=length(the.bands),ncol=2)
dd <- d[-which(d$eleitorado.1996<40500&d$urna==T),] #drop noncompliers
for(i in 1:length(the.bands)){
	rdd <- subset(dd,eleitorado.1996<cutpoint+the.bands[i]&eleitorado.1996>cutpoint-the.bands[i])
	rdd$forcing <- rdd$eleitorado.1996 - cutpoint
	N[i] <- nrow(rdd)
	## Local Linear
	lcllnr <- lm(legfed/comparecimento~forcing*urna,data=rdd)
	lcllnr.est[i] <- coef(lcllnr)['urna']
	lcllnr.ci[i,] <- confint(lcllnr)['urna',]
	## DifinMeans with Robust St Error (or t.test with conf.interval)
	#dom <- summary(lm(legfed/comparecimento~urna,data=rdd))
	#domSE <- sqrt(diag(vcovHC(lm(legfed/comparecimento~urna,data=rdd),type="HC4m"))[2])
	dom <- t.test(formula=I(legfed/comparecimento)~urna,data=rdd)
	dom.est[i] <- as.numeric(diff(dom$estimate))
	dom.ci[i,] <- dom$c[2:1]*-1 #sign and order is reversed
	##  Poplynomial (modified to include interactions of higher terms and urna after R&R)
	poly <- lm(legfed/comparecimento~forcing*urna+I(forcing^2)*urna+I(forcing^3)*urna,data=rdd)
	poly.est[i] <- coef(poly)['urna']
	poly.ci[i,] <- confint(poly)['urna',]
}

pdf(file="fig-rdd1998allbands.pdf")
plot(c(0,max(the.bands)),c(-.01,.15),type="n",xlab="",ylab="",xaxt="n",bty="n")
polygon(x=c(the.bands,sort(the.bands,decreasing=T)),
		y=c(dom.ci[,1],dom.ci[length(the.bands):1,2]),
		col=gray(0.7),border=gray(0.9))
#polygon(x=c(1:length(the.bands),length(the.bands):1),
#		y=c(lcllnr.ci[,1],lcllnr.ci[,2]),
#		col=gray(0.5),angle=30,density=40,border=NA)		
lines(the.bands,dom.est,col=gray(0.6),lwd=2)	
lines(the.bands,lcllnr.est,col=gray(0.2),lwd=2,lty=2)	
lines(the.bands,poly.est,col=1,lwd=2,lty=3)	
abline(v=obs*1000,col=gray(0.6),lty=3,lwd=2)#rescale opt. bandwith to graph
axis(side=1,line=1,at=c(0,the.bands[(the.bands/1000)==round(the.bands/1000)])
		,labels=1/1000*c(0,the.bands[(the.bands/1000)==round(the.bands/1000)]))
axis(side=1,line=-1,at=obs*1000,tick=F,labels=names(obs),cex=0.6,col.axis=gray(.6))
mtext(side=1,line=3.5,"Bandwidth: Distance from Cutpoint (In 1,000 Voters)")
axis(side=3,at=c(0,the.bands[(the.bands/1000)==round(the.bands/1000)])
		,labels=c(0,N[which((the.bands/1000)==round(the.bands/1000))])
		,col.axis=gray(.6),col=gray(.64),cex=.6)
mtext(side=3,line=3,"Number of Municipalities in Sample",col=gray(.6))
mtext(side=2,line=3,"Estimated Effect")
legend(x="bottomleft",bty="n",legend=c("Differences of Means","Local Linear","Polynomial")
		,lty=c(1,2,3),col=c(gray(0.6),gray(0.2),1),lwd=2)
dev.off()




##### RD for the three arbitrary bands, in tables #####
load("data-RDDdepfed.RData")
d <- subset(d,state!="RJ"&state!="AL"&state!="AP"&state!="RR")
cutpoint <- 40500
dd <- d[-which(d$eleitorado.1996<40500&d$urna==T),] #drop noncompliers
# Wider band (12k)
rdd <- subset(dd,eleitorado.1996<cutpoint+12000&eleitorado.1996>cutpoint-12000)
rdd$forcing <- rdd$eleitorado - cutpoint

summary(lm(legfed/comparecimento~forcing+urna,data=rdd))
regA1 <- summary(lm(legfed/comparecimento~forcing*urna,data=rdd))
	regl <- lm(legfed/comparecimento~forcing,data=subset(rdd,urna==F))
	regr <- lm(legfed/comparecimento~forcing,data=subset(rdd,urna==T))
	predict(regr,newdata=data.frame(forcing=0))-predict(regl,newdata=data.frame(forcing=0))
summary(lm(legfed/comparecimento~forcing*urna+hdi.1991,data=rdd))
regA2 <- summary(lm(legfed/comparecimento~forcing*urna,data=subset(rdd,hdi.1991<median(hdi.1991))))

tabA <- merge(stack.regs(regA1),stack.regs(regA2)
	,by=c("vars","desc"),suffixes=c("","lowHDI"))
	
# Nulos and brancos for band
regA3a <- summary(lm(nulosfed/comparecimento~forcing*urna,data=rdd))
regA3b <- summary(lm(brancosfed/comparecimento~forcing*urna,data=rdd))
regA4a <- summary(lm(nulosfed/comparecimento~forcing*urna,data=subset(rdd,hdi.1991<median(hdi.1991))))
regA4b <- summary(lm(brancosfed/comparecimento~forcing*urna,data=subset(rdd,hdi.1991<median(hdi.1991))))
tabA2n <- merge(stack.regs(regA3a),stack.regs(regA4a)
		,by=c("vars","desc"),suffixes=c("","lowHDI"))[12:14,]
		tabA2n$vars <- paste("nulos",tabA2n$vars,sep=".")
tabA2b <-merge(stack.regs(regA3b),stack.regs(regA4b)
		,by=c("vars","desc"),suffixes=c("","lowHDI"))[12:14,]
		tabA2b$vars <- paste("brancos",tabA2b$vars,sep=".")
tabA2 <- rbind(tabA2b,tabA2n)

# JOINT Nulos and brancos (i.e. INVALID) for band
regA5 <- summary(lm((nulosfed+brancosfed)/comparecimento~forcing*urna,data=rdd))
regA6 <- summary(lm((nulosfed+brancosfed)/comparecimento~forcing*urna,data=subset(rdd,hdi.1991<median(hdi.1991))))
tabA3 <- merge(stack.regs(regA5),stack.regs(regA6)
		,by=c("vars","desc"),suffixes=c("","lowHDI"))[12:14,]
tabA3$vars <- paste("invalid",tabA3$vars,sep=".")



# Narrow band (8k)
rdd <- subset(dd,eleitorado.1996<cutpoint+8000&eleitorado.1996>cutpoint-8000)
rdd$forcing <- rdd$eleitorado - cutpoint
regB1 <-  summary(lm(legfed/comparecimento~forcing*urna,data=rdd))
regB2 <-  summary(lm(legfed/comparecimento~forcing*urna,data=subset(rdd,hdi.1991<median(hdi.1991))))
tabB <- merge(stack.regs(regB1),stack.regs(regB2)
	,by=c("vars","desc"),all=T,suffixes=c("","lowHDI"))

# Nulos and brancos for band
regB3a <- summary(lm(nulosfed/comparecimento~forcing*urna,data=rdd))
regB3b <- summary(lm(brancosfed/comparecimento~forcing*urna,data=rdd))
regB4a <- summary(lm(nulosfed/comparecimento~forcing*urna,data=subset(rdd,hdi.1991<median(hdi.1991))))
regB4b <- summary(lm(brancosfed/comparecimento~forcing*urna,data=subset(rdd,hdi.1991<median(hdi.1991))))
tabB2n <- merge(stack.regs(regB3a),stack.regs(regB4a)
		,by=c("vars","desc"),suffixes=c("","lowHDI"))[12:14,]
		tabB2n$vars <- paste("nulos",tabB2n$vars,sep=".")
tabB2b <-merge(stack.regs(regB3b),stack.regs(regB4b)
		,by=c("vars","desc"),suffixes=c("","lowHDI"))[12:14,]
		tabB2b$vars <- paste("brancos",tabB2b$vars,sep=".")
tabB2 <- rbind(tabB2b,tabB2n)

# JOINT Nulos and brancos (i.e. INVALID) for band
regB5 <- summary(lm((nulosfed+brancosfed)/comparecimento~forcing*urna,data=rdd))
regB6 <- summary(lm((nulosfed+brancosfed)/comparecimento~forcing*urna,data=subset(rdd,hdi.1991<median(hdi.1991))))
tabB3 <- merge(stack.regs(regB5),stack.regs(regB6)
		,by=c("vars","desc"),suffixes=c("","lowHDI"))[12:14,]
tabB3$vars <- paste("invalid",tabB3$vars,sep=".")


# Loca Linear Narrowest band (4k)
rdd <- subset(d,eleitorado.1996<cutpoint+4000&eleitorado.1996>cutpoint-4000)
rdd$forcing <- rdd$eleitorado - cutpoint
regC1 <- summary(lm(legfed/comparecimento~forcing*urna,data=rdd))
regC2 <- summary(lm(legfed/comparecimento~forcing*urna,data=subset(rdd,hdi.1991<median(hdi.1991))))
tabC <- merge(stack.regs(regC1),stack.regs(regC2)
	,by=c("vars","desc"),all=T,suffixes=c("","lowHDI"))

# Nulos and brancos for band
regC3a <- summary(lm(nulosfed/comparecimento~forcing*urna,data=rdd))
regC3b <- summary(lm(brancosfed/comparecimento~forcing*urna,data=rdd))
regC4a <- summary(lm(nulosfed/comparecimento~forcing*urna,data=subset(rdd,hdi.1991<median(hdi.1991))))
regC4b <- summary(lm(brancosfed/comparecimento~forcing*urna,data=subset(rdd,hdi.1991<median(hdi.1991))))
tabC2n <- merge(stack.regs(regC3a),stack.regs(regC4a)
		,by=c("vars","desc"),suffixes=c("","lowHDI"))[12:14,]
		tabC2n$vars <- paste("nulos",tabC2n$vars,sep=".")
tabC2b <-merge(stack.regs(regC3b),stack.regs(regC4b)
		,by=c("vars","desc"),suffixes=c("","lowHDI"))[12:14,]
		tabC2b$vars <- paste("brancos",tabC2b$vars,sep=".")
tabC2 <- rbind(tabC2b,tabC2n)

# JOINT Nulos and brancos (i.e. INVALID) for band
regC5 <- summary(lm((nulosfed+brancosfed)/comparecimento~forcing*urna,data=rdd))
regC6 <- summary(lm((nulosfed+brancosfed)/comparecimento~forcing*urna,data=subset(rdd,hdi.1991<median(hdi.1991))))
tabC3 <- merge(stack.regs(regC5),stack.regs(regC5)
		,by=c("vars","desc"),suffixes=c("","lowHDI"))[12:14,]
tabC3$vars <- paste("invalid",tabC3$vars,sep=".")


# Dif in mean around narrow band
regD1 <- lm(legfed/comparecimento~urna,data=rdd)
		robSED1 <- sqrt(diag(vcovHC(regD1,type="HC4m")))
		pD1 <- linearHypothesis(regD1, c("urna = 0"),vcov.=vcovHC(regD1,type="HC4m"))$P[2]
		pD1intercept <- linearHypothesis(regD1, c("(Intercept) = 0"),vcov.=vcovHC(regD1,type="HC4m"))$P[2]  
		###t.test(formula=I(legfed/comparecimento)~urna,data=rdd) #ALTERNATIVE
regD2 <- lm(legfed/comparecimento~urna,data=subset(rdd,hdi.1991<median(hdi.1991)))
		robSED2 <- sqrt(diag(vcovHC(regD2,type="HC4m")))
		pD2 <- linearHypothesis(regD2, c("urna = 0"),vcov.=vcovHC(regD2,type="HC4m"))$P[2] 
		pD2intercept <- linearHypothesis(regD2, c("(Intercept) = 0"),vcov.=vcovHC(regD2,type="HC4m"))$P[2] 
tabD <- merge(stack.regs(summary(regD1)),stack.regs(summary(regD2))
	,by=c("vars","desc"),all=T,suffixes=c("","lowHDI"))
tabD[which(tabD$desc=="Std. Error"),"v"]<-robSED1  #use robust SE
tabD[which(tabD$desc=="Std. Error"),"vlowHDI"]<-robSED2  #use robset SE
tabD[which(tabD$desc=="zP-value"),"v"]<-c(pD1,pD1intercept)  #use robust SE
tabD[which(tabD$desc=="zP-value"),"vlowHDI"]<-c(pD2,pD2intercept)  #use robset SE


# Nulos and brancos for band  (need to add robust SE's here if ever used)
regD3a <- summary(lm(nulosfed/comparecimento~urna,data=rdd))
regD3b <- summary(lm(brancosfed/comparecimento~urna,data=rdd))
regD4a <- summary(lm(nulosfed/comparecimento~urna,data=subset(rdd,hdi.1991<median(hdi.1991))))
regD4b <- summary(lm(brancosfed/comparecimento~urna,data=subset(rdd,hdi.1991<median(hdi.1991))))
tabD2n <- merge(stack.regs(regD3a),stack.regs(regD4a)
		,by=c("vars","desc"),suffixes=c("","lowHDI"))[6:8,]
		tabD2n$vars <- paste("nulos",tabD2n$vars,sep=".")
tabD2b <-merge(stack.regs(regD3b),stack.regs(regD4b)
		,by=c("vars","desc"),suffixes=c("","lowHDI"))[6:8,]
		tabD2b$vars <- paste("brancos",tabD2b$vars,sep=".")
tabD2 <- rbind(tabD2b,tabD2n)

# JOINT Nulos and brancos (i.e. INVALID) for band
regD5 <-  lm((nulosfed+brancosfed)/comparecimento~urna,data=rdd)
		robSED5 <- sqrt(diag(vcovHC(regD5,type="HC4m")))
		pD5 <- linearHypothesis(regD5, c("urna = 0"),vcov.=vcovHC(regD5,type="HC4m"))$P[2]
	
regD6 <-  lm((nulosfed+brancosfed)/comparecimento~urna,data=subset(rdd,hdi.1991<median(hdi.1991)))
		robSED6 <- sqrt(diag(vcovHC(regD5,type="HC4m")))
		pD6 <- linearHypothesis(regD6, c("urna = 0"),vcov.=vcovHC(regD6,type="HC4m"))$P[2]

tabD3 <- merge(stack.regs(summary(regD5)),stack.regs(summary(regD6))
		,by=c("vars","desc"),suffixes=c("","lowHDI"))[6:8,]
tabD3$vars <- paste("invalid",tabD3$vars,sep=".")
tabD3[which(tabD3$desc=="Std. Error"),"v"]<-robSED5[2]  #use robust SE
tabD3[which(tabD3$desc=="Std. Error"),"vlowHDI"]<-robSED5[2]  #use robset SE
tabD3[which(tabD3$desc=="zP-value"),"v"]<-c(pD5)  #use robust SE
tabD3[which(tabD3$desc=="zP-value"),"vlowHDI"]<-c(pD6)  #use robset SE

### Assemble master table (old master table with three bands)
library(xtable)
tab <- merge(
			merge(tabA,tabB,by=c("vars","desc"),all=T,suffixes=c("12k","8k")),
		    merge(tabC,tabD,by=c("vars","desc"),all=T,suffixes=c("4k","4kDiff")),
	   by=c("vars","desc"),all=T)  
xtab <- xtable(tab[c(1,12:14,10),-2],digits=3)
caption(xtab) <- "Regression Discontinuity Estimates for 1998"
label(xtab) <- "tab1998"
print(xtab,include.rownames=F
		,caption.placement = "top"
		,hline.after = c(1,4))
		
### Assemble nulos and brancos auxiliar table
tab2 <- merge(
			merge(tabA2,tabB2,by=c("vars","desc"),all=T,suffixes=c("12k","8k")),
		    merge(tabC2,tabD2,by=c("vars","desc"),all=T,suffixes=c("4k","4kDiff")),
	   by=c("vars","desc"),all=T)  
print(xtable(tab2[,-2],digits=3),include.rownames=F)

### Assemble INVALID auxiliar table
tab3 <- merge(
			merge(tabA3,tabB3,by=c("vars","desc"),all=T,suffixes=c("12k","8k")),
		    merge(tabC3,tabD3,by=c("vars","desc"),all=T,suffixes=c("4k","4kDiff")),
	   by=c("vars","desc"),all=T)  
print(xtable(tab3[,-2],digits=3),include.rownames=F)

##### ORIIGNAL TABLE REGRESSION ESTIMATES ENDS HERE ##########################################################




#### Table in RObustNess Checks ###############
##### DOM + polynomial in "optimal" bandwiths #
load("data-RDDdepfed.RData")
d <- subset(d,state!="RJ"&state!="AL"&state!="AP"&state!="RR")
cutpoint <- 40500
dd <- d[-which(d$eleitorado.1996<cutpoint&d$urna==T),] #drop noncompliers
rdd <- subset(d,eleitorado.1996<cutpoint+4000&eleitorado.1996>cutpoint-4000)
rdd$forcing <- rdd$eleitorado - cutpoint
regD1 <- lm(legfed/comparecimento~urna,data=rdd)
		robSED1 <- sqrt(diag(vcovHC(regD1,type="HC4m")))[2]
		pD1 <- linearHypothesis(regD1, c("urna = 0"),vcov.=vcovHC(regD1,type="HC4m"))$P[2]
DOM <- c(coef=coef(regD1)[2],se=robSED1,p=pD1,bw=4000,N=nrow(rdd))


library(rdrobust)
regIK <- rdrobust(y=dd$legfed/dd$comparecimento,x=(dd$eleitorado.1996-cutpoint)/1000,bwselect="IK")
regCCT <- rdrobust(y=dd$legfed/dd$comparecimento,x=(dd$eleitorado.1996-cutpoint)/1000,bwselect="CCT")
regCV <- rdrobust(y=dd$legfed/dd$comparecimento,x=(dd$eleitorado.1996-cutpoint)/1000,bwselect="CV")
CCT <-  c(summary(regCCT)$coef["Robust",c(1,2,4)],bw=regCCT$h*1000,N=sum(regCCT$N_l+regCCT$N_r))
IK <-  c(summary(regIK)$coef["Conventional",c(1,2,4)],bw=regIK$h*1000,N=sum(regIK$N_l+regIK$N_r))
CV <-  c(summary(regCV)$coef["Conventional",c(1,2,4)],bw=regCV$h*1000,N=sum(regCV$N_l+regCV$N_r))

the.table <- data.frame(DOM,CCT,IK,CV)
xtable(the.table,digits=3)


##### ADDITIONAL ANALAYSIS ##########################################################################
pdf(file="RDD_Depfed_1998_LegendaPartidos.pdf",width=5,height=8)
par(mfrow=c(2,2),mar=c(3,4,1,1))
#### For the two parties that should have it!
rdd.plot(forcing,rdd$ptlegfed/(rdd$comparecimento)
		,treatment,cutpoint=cutpoint,ylabel="Legenda PT for Dep Federal  1998")

rdd.plot(forcing,rdd$psdblegfed/(rdd$comparecimento)
		,treatment,cutpoint=cutpoint,ylabel="Legenda PSDB for Dep Federal 1998")

rdd.plot(forcing,(rdd$ppslegfed)/(rdd$comparecimento)
		,treatment,cutpoint=cutpoint,ylabel="Legenda PPS for Dep Federal  1998")

rdd.plot(forcing,(rdd$pmdblegfed)/(rdd$comparecimento)
		,treatment,cutpoint=cutpoint,ylabel="Legenda PPMDB for Dep Federal  1998")
dev.off()

### Balance
par(mfrow=c(3,3),mar=c(3,4,1,1))

rdd.plot(forcing,rdd$comparecimento/rdd$eleitorado
		,treatment,cutpoint=cutpoint,ylabel="Turnout 1996")
rdd.plot(forcing,rdd$hdi.1991
		,treatment,cutpoint=cutpoint,ylabel="HDI-M 1991")
rdd.plot(forcing,log(rdd$pibpc.1999)
		,rdd$urna,cutpoint=cutpoint,ylabel="GDP-PC 1996")
rdd.plot(forcing,rdd$nonwhite.1991
		,rdd$urna,cutpoint=cutpoint,ylabel="Share Non-White")
rdd.plot(forcing,rdd$pent.1991
		,treatment,cutpoint=cutpoint,ylabel="Share Pentecostals 1991")
rdd.plot(forcing,rdd$distcap
		,treatment,cutpoint=cutpoint,ylabel="Distance to Capital City")
rdd.plot(forcing,rdd$lula.vs.1994
		,treatment,cutpoint=cutpoint,ylabel="Lula Vote Share 1994")


###################################################################
## Coattails
###################################################################
# Idea here is to compare the "elasticity" of legenda to president for each party....
load("data-RDDdepfed.RData")
d <- subset(d,state!="RJ"&state!="AL"&state!="AP"&state!="RR")
band <- 12000 
cutpoint <- 40500
rdd <- subset(d,eleitorado.1996<cutpoint+band&eleitorado.1996>cutpoint-band)
rdd$forcing <- rdd$eleitorado.1996-cutpoint

the.regs <- list(
	psdb=lm(psdblegfed/comparecimento~fhc.vs.1998*urna,data=rdd),
	pt=lm(ptlegfed/comparecimento~lula.vs.1998*urna,data=rdd),
    pps=lm(ppslegfed/comparecimento~ciro.vs.1998*urna,data=rdd),
    pfl=lm(pfllegfed/comparecimento~fhc.vs.1998*urna,data=rdd),
    pdt=lm(pdtlegfed/comparecimento~lula.vs.1998*urna,data=rdd),
    pmdb=lm(pmdblegfed/comparecimento~fhc.vs.1998*urna,data=rdd),
 #    pmdb=lm(pmdblegfed/comparecimento~lula.vs.1998*urna,data=rdd),
     ppb=lm(ppblegfed/comparecimento~fhc.vs.1998*urna,data=rdd),
     ptb=lm(ptblegfed/comparecimento~fhc.vs.1998*urna,data=rdd),
     psb=lm(psblegfed/comparecimento~lula.vs.1998*urna,data=rdd)
    )
    
    
 the.regs.low <- list(
	psdb=lm(psdblegfed/comparecimento~fhc.vs.1998*urna,data=subset(rdd,hdi.1991<median(hdi.1991))),
	pt=lm(ptlegfed/comparecimento~lula.vs.1998*urna,data=subset(rdd,hdi.1991<median(hdi.1991))),
    pps=lm(ppslegfed/comparecimento~ciro.vs.1998*urna,data=subset(rdd,hdi.1991<median(hdi.1991))),
    pfl=lm(pfllegfed/comparecimento~fhc.vs.1998*urna,data=subset(rdd,hdi.1991<median(hdi.1991))),
    pdt=lm(pdtlegfed/comparecimento~lula.vs.1998*urna,data=subset(rdd,hdi.1991<median(hdi.1991))),
    pmdb=lm(pmdblegfed/comparecimento~fhc.vs.1998*urna,data=subset(rdd,hdi.1991<median(hdi.1991))),
     pmdb=lm(pmdblegfed/comparecimento~lula.vs.1998*urna,data=subset(rdd,hdi.1991<median(hdi.1991)))
    )

### Makes a difference in less developed municipalities...
#### Coattails #####

the.regs.urna <- list(
	psdb=lm(psdblegfed/comparecimento~fhc.vs.1998,data=subset(rdd,urna==T)),
	pt=lm(ptlegfed/comparecimento~lula.vs.1998,data=subset(rdd,urna==T)),
    pps=lm(ppslegfed/comparecimento~ciro.vs.1998,data=subset(rdd,urna==T)),
    pfl=lm(pfllegfed/comparecimento~fhc.vs.1998,data=subset(rdd,urna==T)),
    pdt=lm(pdtlegfed/comparecimento~lula.vs.1998,data=subset(rdd,urna==T)),
    pmdb=lm(pmdblegfed/comparecimento~fhc.vs.1998,data=subset(rdd,urna==T)),
     pmdb=lm(pmdblegfed/comparecimento~lula.vs.1998,data=subset(rdd,urna==T))
    )
the.regs.not <- list(
	psdb=lm(psdblegfed/comparecimento~fhc.vs.1998,data=subset(rdd,urna==F)),
	pt=lm(ptlegfed/comparecimento~lula.vs.1998,data=subset(rdd,urna==F)),
    pps=lm(ppslegfed/comparecimento~ciro.vs.1998,data=subset(rdd,urna==F)),
    pfl=lm(pfllegfed/comparecimento~fhc.vs.1998,data=subset(rdd,urna==F)),
    pdt=lm(pdtlegfed/comparecimento~lula.vs.1998,data=subset(rdd,urna==F)),
    pmdb=lm(pmdblegfed/comparecimento~fhc.vs.1998,data=subset(rdd,urna==F)),
     pmdb=lm(pmdblegfed/comparecimento~lula.vs.1998,data=subset(rdd,urna==F))
    )

my.extract <- function(x){summary(x)$coef[2,1:2]}
coefT <- sapply(the.regs.urna,my.extract)
coefF <- sapply(the.regs.not,my.extract)
a<-0.95
ciT <- sapply(the.regs.urna,function(x){confint(x,parm=2,level=a)})
ciF <- sapply(the.regs.not,function(x){confint(x,parm=2,level=a)})


#graph including the PMDB, not in paper
pdf(file="fig-1998coattailsdepfedold.pdf")
require(plotrix)
par(mar=c(5,5,1,1))
my.labels<- paste(toupper(colnames(coefT)),c("","","","\n(PSDB)","\n(PT)","\n(PSDB)","\n(PT)"),sep="")
plot(c(1,ncol(coefT)),c(-.02,.1),type="n",xlab="",ylab="",xaxt="n",bty="n")
mtext(side=2,line=3.2,"Elasticity of Legenda Votes Relative to Presidential Vote")
axis(side=1,line=1,at=1:ncol(coefT),labels=my.labels,las=1,tick=F)
abline(h=c(0,.02,.04,.06,.08),lty=3,col=gray(0.6))
#plotCI(1:ncol(coefT),coefT[1,],uiw=qnorm(a)*coefT[2,],add=T,pch=21,pt.bg=1)
#plotCI(1:ncol(coefF),coefF[1,],uiw=qnorm(a)*coefF[2,],add=T,pch=24,pt.bg="white")
plotCI(1:ncol(coefT),coefT[1,],ui=ciT[2,],li=ciT[1,],add=T,pch=21,pt.bg=1)
plotCI(1:ncol(coefF),coefF[1,],ui=ciF[2,],li=ciF[1,],add=T,pch=21,,pt.bg="white")
legend(x="top",legend=c("Paper Ballots","Electronic Voting"),pch=c(24,21),pt.bg=c("white",1),bty="n",horiz=T)
axis(side=1,line=2.5,at=c(2,5.5),labels=c("With Presidential Candidates","Without Presidential Candidates"),tick=F)
dev.off()


# graph not including the PMDB
coefT <-coefT[,1:5] 
coefF  <-coefF[,1:5]
ciT <- ciT[,1:5]
ciF <- ciF[,1:5]

pdf(file="fig-1998coattailsdepfed.pdf")
require(plotrix)
par(mar=c(5,5,1,1.5))
my.labels<- paste(toupper(colnames(coefT)),c("","","","\n(PSDB)","\n(PT)","\n(PSDB)","\n(PT)"),sep="")
plot(c(1,ncol(coefT)),c(-.02,.1),type="n",xlab="",ylab="",xaxt="n",bty="n")
mtext(side=2,line=3.2,"Elasticity of Legenda Votes Relative to Presidential Vote")
axis(side=1,line=1,at=1:ncol(coefT),labels=my.labels[1:ncol(coefT)],las=1,tick=F)
abline(h=c(0,.02,.04,.06,.08),lty=3,col=gray(0.6))
#plotCI(1:ncol(coefT),coefT[1,],uiw=qnorm(a)*coefT[2,],add=T,pch=21,pt.bg=1)
#plotCI(1:ncol(coefF),coefF[1,],uiw=qnorm(a)*coefF[2,],add=T,pch=24,pt.bg="white")
plotCI(1:ncol(coefT),coefT[1,],ui=ciT[2,],li=ciT[1,],add=T,pch=21,pt.bg=1)
plotCI(1:ncol(coefF),coefF[1,],ui=ciF[2,],li=ciF[1,],add=T,pch=21,,pt.bg="white")
legend(x="top",legend=c("Paper Ballots","Electronic Voting"),pch=c(24,21),pt.bg=c("white",1),bty="n",horiz=T)
axis(side=1,line=2.5,at=c(2,4.5),labels=c("With Presidential Candidates","Without Presidential Candidates"),tick=F)
dev.off()



# graph effects only, not quantities including the PMDB
# This is in the last version of the paper
pdf(file="fig-1998coattailsdepfedeffect.pdf")
coefs <- sapply(the.regs,function(x){summary(x)$coef[4,1]})#[1:5]
cis <- sapply(the.regs,function(x){confint(x,parm=4)})#[,1:5]

library(plotrix)
par(mar=c(5,5.5,1,1))
my.labels<- paste(toupper(names(coefs)),c("","","","\n(PSDB)","\n(PT)",
										"\n(PSDB)","\n(PSDB)","\n(PSDB)","\n(PT)"),sep="")
plot(c(1,length(coefs)),c(-.02,.1),type="n",xlab="",ylab="",xaxt="n",bty="n")
mtext(side=2,line=3.2,"Change in Coattails from Paper Ballots to Eletronic Voting Technology")
axis(side=1,line=0,at=1:length(coefs),labels=my.labels,las=1,padj=.5,tick=F,cex.axis=.9)
require(plotrix)
polygon(x=c(3.5,5.5,5.5,3.5),
			y=c(-1,-1,1,1),col=gray(0.8),border=NA)
abline(h=0,col=gray(0.6),lty=3)#c(0,.02,.04,.06,.08),lty=3,col=gray(0.6))
plotCI(1:length(coefs),coefs,ui=cis[2,],li=cis[1,],add=T,pch=21,pt.bg=1)
#legend(x="top",legend=c("Paper Ballots","Electronic Voting"),pch=c(24,21),pt.bg=c("white",1),bty="n",horiz=T)
axis(side=1,line=2.3,at=c(2,4.5,7.5),labels=c("Presidential\nCandidates","Vice-Presidential\nCandidates","Other Supporing Parties"),tick=F,padj=.5)
dev.off()



#########################################################################
## Pooling all parties and muncipalities in  	#############
## DIFF in DIFF type of analysis 							#############
load("data-RDDdepfed.RData")
d <- subset(d,state!="RJ"&state!="AL"&state!="AP"&state!="RR")
band <- 4000   ###IMPORTANT< SET BAND HERE
cutpoint <- 40500
rdd <- subset(d,eleitorado.1996<cutpoint+band&eleitorado.1996>cutpoint-band)
#rdd$forcing <- rdd$eleitorado - cutpoint

the.parties <- c("pmdblegfed","pfllegfed","psdblegfed","ptlegfed","ppslegfed","pdtlegfed",
				"ppblegfed","ptblegfed","pllegfed","psblegfed","psdlegfed")
pretmp2 <- data.frame(pmdb=NA,pfl=NA,psdb=rdd$fhc.vs.1998,
						pt=rdd$lula.vs.1998,pps=rdd$ciro.vs.1998,pdt=NA,
						ppb=NA,ptb=NA,pl=NA,psb=NA,psd=NA)
the.pres <- names(pretmp2)
#### Pool data and assemble a DID like structure for all parties ####
tmp <- subset(rdd,select=the.parties)/rdd$comparecimento
tmp2 <- is.na(pretmp2)==F#indicator of running for president
### Reshape into DID like structure #####
dd <- data.frame(codeibge=rdd$codeibge,urna=rdd$urna,forcing=rdd$eleitorado.1996-cutpoint,tmp2,tmp)
library(reshape)
ddd <- reshape(dd,direction="long",
		varying=list(mayor=the.pres,
					  legenda=the.parties))
names(ddd)[5:6] <- c('pres','legenda') 
Nnot <- sum(tmp2==F)
#### Estimate the simple case (ran or not a presidential candidate) ####
if(band>4000){
	effects.not <- lm(legenda~forcing*urna,data=subset(ddd,pres==F))
	pe.not <- coef(effects.not)["urna"]
	ci.not <- confint(effects.not, parm="urna", level = 0.95)
	effects.some <-lm(legenda~forcing*urna,data=subset(ddd,pres==T))
	pe.some <- coef(effects.some)["urna"]
	ci.some <- confint(effects.some, parm="urna", level = 0.95)
	}else{#Dif in means
	effects.not <- t.test(formula=legenda~urna,data=subset(ddd,pres==F))
	pe.not <- diff(effects.not$estimate)
	ci.not <-  effects.not$conf[2:1]*-1
	effects.some <- t.test(formula=legenda~urna,data=subset(ddd,pres==T))
	pe.some <- diff(effects.some$estimate)
	ci.some <-  effects.some$conf[2:1]*-1
	}

#### Loop for different definitions of compettitive 
all.effects <- all.N <- list()
tmp <- subset(rdd,select=the.parties)/rdd$comparecimento
for(i in 1:50){
tmp2 <- is.na(pretmp2)==F& #indicator of compatitive mayor
			   pretmp2> i/100			  
### Reshape into DID like structure #####
dd <- data.frame(codeibge=rdd$codeibge,urna=rdd$urna,forcing=rdd$eleitorado.1996-cutpoint,tmp2,tmp)
library(reshape)
ddd <- reshape(dd,direction="long",
		varying=list(mayor=the.pres,
					  legenda=the.parties))
names(ddd)[5:6] <- c('pres','legenda') 
#### Estimate
if(band==4000){#dif in means
	all.effects[[i]] <- t.test(formula=legenda~urna,data=subset(ddd,pres==T))
	}else{#lcl linear
	all.effects[[i]] <- lm(legenda~forcing*urna,data=subset(ddd,pres==T))
	}
all.N[[i]] <- sum(tmp2)
}

if(band==4000){
pe <- sapply(all.effects ,function(x){diff(x$estimate)})
ci <- sapply(all.effects ,function(x){c(x$conf[2:1])*-1})
}else{
pe <- sapply(all.effects ,function(x){coef(x)["urna"]})
ci <- sapply(all.effects ,function(x){confint(x, parm="urna", level = 0.95)})	
}
N <- sapply(all.N,"cbind")

### Plot EFFECTS, with different performance of the upticket candidate ###
pdf(file="fig-1998coattailspooled.pdf")
par(mar=c(5,4,1,1))
a <- 0.99
plot(pe,type="l",ylim=c(-.005,0.035),xlab="",ylab="") 
polygon(x=c(1:ncol(ci),ncol(ci):1),
		 y=c(ci[1,],ci[2,ncol(ci):1]),
		 col=gray(.4),border=NA)
lines(pe) ## effect of EV when fielding a competitive canddiaate
abline(h=0,lty=2)
mtext(side=1,line=3,"Competitiveness of Presidential Candidate (Vote Share)")
mtext(side=2,line=3,"Increase in Party Label Vote Share with EV Technology")
dev.off()
#text(35,0.02,"With Presidential Candidate")
#text(25,0.005,"Without Presidential Candidate")
#######

#### Plot SELECTED effects, with no presidential candidates and selected upticket candidates ###
to.plot <- rbind(not=c(pe.not,ci.not),
	  some=c(pe.some,ci.some),
	  c10=c(pe[10],ci[,10]),
	  c20=c(pe[20],ci[,20]),
	  c30=c(pe[30],ci[,30]),
	  c40=c(pe[40],ci[,40]))

pdf(file="fig-1998coattailspooledselected.pdf")
par(mar=c(6,4,1,1))
a <- 0.99
xs <- c(1,seq(1.5,2.5,len=5))
plot(xs,type="n",ylim=c(0,0.03),xlim=c(0.5,3),xlab="",ylab="",bty="n",xaxt="n")
#abline(h=seq(0,0.03,by=0.005),col=gray(0.8)) 
library(plotrix)
plotCI(xs,to.plot[,1],ui=to.plot[,3],li=to.plot[,2],add=T,pch=21,pt.bg=c("white",rep(1,5)))
axis(side=1,line=3.2,at=c(1,2),labels=c("Without\nPresidential Candidate","With\nPresidential Candidate"),tick=F)
axis(side=1,line=-1,at=xs[-1],labels=c("Any",">10%",">20%",">30%",">40%"),las=2)
axis(side=1,line=1.2,at=xs,labels=paste("N=",c(Nnot,N[c(1,10,20,30,40)]),sep=""),tick=F,cex.axis=0.8)
mtext(side=2,line=3,"Effect on Party Label Vote Shares")
dev.off()






###### TABLE FOR COMPARING NET EFFECT OF ENFRIANCHISMENT BEGINS HERE ##################
###### TABLE 2 in the FINAL PAPER ######
library(car)
load("data-RDDdepfed.RData")
d <- subset(d,state!="RJ"&state!="AL"&state!="AP"&state!="RR")
band <- 4000   ###IMPORTANT< SET BAND HERE
cutpoint <- 40500


# Local Linear Narrowest band (4k)
rdd <- subset(d,eleitorado.1996<cutpoint+4000&eleitorado.1996>cutpoint-4000)
rdd$forcing <- rdd$eleitorado - cutpoint
regLL <- summary(lm(legfed/comparecimento~forcing*urna,data=rdd))
#tabC <- merge(stack.regs(regC1),stack.regs(regC2)
#	,by=c("vars","desc"),all=T,suffixes=c("","lowHDI"))

# JOINT Nulos and brancos (i.e. INVALID) for band
regLLi <- summary(lm((nulosfed+brancosfed)/comparecimento~forcing*urna,data=rdd))

# Dif in mean around narrow band
regD <- lm(legfed/comparecimento~urna,data=rdd)
		robSED <- sqrt(diag(vcovHC(regD,type="HC4m")))
		pD <- linearHypothesis(regD, c("urna = 0"),vcov.=vcovHC(regD,type="HC4m"))$P[2]
		pDintercept <- linearHypothesis(regD, c("(Intercept) = 0"),vcov.=vcovHC(regD,type="HC4m"))$P[2]  
		###t.test(formula=I(legfed/comparecimento)~urna,data=rdd) #ALTERNATIVE
tabD <- stack.regs(summary(regD))
tabD[which(tabD$desc=="Std. Error"),"v"]<-robSED  #use robust S
tabD[which(tabD$desc=="zP-value"),"v"]<-c(pD,pDintercept)  #use robust SE

# JOINT Nulos and brancos (i.e. INVALID) for band
regDi <-  lm((nulosfed+brancosfed)/comparecimento~urna,data=rdd)
		robSEDi <- sqrt(diag(vcovHC(regDi,type="HC4m")))
		pDi <- linearHypothesis(regDi, c("urna = 0"),vcov.=vcovHC(regDi,type="HC4m"))$P[2]
tabDi <- stack.regs(summary(regDi))
tabDi[which(tabDi$desc=="Std. Error"),"v"]<-robSEDi  #use robust S
tabDi[which(tabDi$desc=="zP-value"),"v"]<-c(pDi,pDintercept)  #use robust SE

# Polynomial Narrowest band (4k)
regP <- summary(lm(legfed/comparecimento~forcing*urna+I(forcing^2)+I(forcing^3),data=rdd))

# JOINT Nulos and brancos (i.e. INVALID) for band
regPi <- summary(lm((nulosfed+brancosfed)/comparecimento~forcing*urna+I(forcing^2)+I(forcing^3),data=rdd))

RDinv <- merge(tabDi[c(1,4:6,8),],
			merge(stack.regs(regLLi,4)[c(1,10:12),]
		,stack.regs(regPi,4)[c(1,16:18),],by=c("vars","desc"),suffixes=c("DOM","LL"))
		,by=c("vars","desc"),all=T)
		RDinv$vars <- paste(RDinv$vars,"invalid",sep=".")
RDplv <- merge(tabD[c(1,4:6,8),],
			merge(stack.regs(regLL,4)[c(1,10:12),]
		,stack.regs(regP,4)[c(1,16:18),],by=c("vars","desc"),suffixes=c("DOM","LL"))
		,by=c("vars","desc"),all=T)

the.table <- rbind(RDinv,RDplv)
lb <- the.table[the.table$vars=="urna"&the.table$desc=="Estimate",-c(1:2)]*
								(1-to.plot["not",1]/to.plot["some",1])#from the pooledcoattailsanalysis
rownames(the.table)<-paste(the.table$vars,the.table$desc,sep=".")
the.table <- the.table[,-c(1:2)]
the.table <- rbind(the.table,lb)
print(xtable(100*the.table,digits=2))


####### NET ENFRANCHISEMENT ENDS HERE ##########################################################




########################################################################
####  Hypothesis 4													####
#### THe reverse of the coin: invalid votes for president 			####
#### The more nuances analysis and graphs in the appendix are here  ####
#### The systematic analaysis for all years is in the overtime file ####

####### Graphical Results #######
load("data-RDDdepfed.RData")

d$invalidpresshare.1998 <- (d$brancospres.1998+d$nulospres.1998)/d$comparecimentopres.1998
d$brancospresshare.1998 <- (d$brancospres.1998)/d$comparecimentopres.1998
d$nulospresshare.1998 <- (d$nulospres.1998)/d$comparecimentopres.1998
d$legendafedshare.1998 <- d$legfed/d$comparecimento 

d$invalidpresshare.1994 <- (d$brancospres.1994+d$nulospres.1994)/d$comparecimentopres.1994
d$brancospresshare.1994 <- (d$brancospres.1994)/d$comparecimentopres.1994
d$nulospresshare.1994 <- (d$nulospres.1994)/d$comparecimentopres.1994
d$legendafedshare.1994 <- d$legfed.1994/d$comparecimento.1994 
### EXCLUIR DOIS MUNICÍPIOS DE 1994 QUE TEM MAIS LEGENDA DO QUE COMPARECIMENTO ###
d$legendafedshare.1994[which(d$legendafedshare.1994>1)] <- NA

pdf(file="fig-hypothesis4-1998.pdf",height=6,width=6)
plot(d$invalidpresshare.1998,d$legendafedshare.1998
	,ylab="PLV (share)",
	,xlab="Invalid Votes (share)"
	,pch=ifelse(d$urna,19,21)
	,cex=1
	,col=ifelse(d$urna,1,1))
lines(lowess(cbind(d$invalidpresshare.1998[d$urna==T]
		,d$legendafedshare.1998[d$urna==T])),col=gray(0.6),lwd=2)
lines(lowess(cbind(d$invalidpresshare.1998[d$urna==F]
		,d$legendafedshare.1998[d$urna==F])),col=gray(0.6),lwd=2)	
#abline(reg=lm(legendafedshare.1998~invalidpresshare.1998,data=subset(d,urna==T)),col=1)
#abline(reg=lm(legendafedshare.1998~invalidpresshare.1998,data=subset(d,urna==F)),col=1)
legend(x="right",legend=c("Paper","EV")
		,pch=c(21,19),
		,col=c(1,1),
		,pt.cex=c(1,1),
		,bg=c(1,1),
		bty="n")
dev.off()

pdf(file="fig-hypothesis4-1994.pdf",height=6,width=6)
plot(d$invalidpresshare.1994,d$legendafedshare.1994
	,ylab="PLV (share)",
	,xlab="Invalid Votes (share)"
	,pch=ifelse(d$urna,19,21)
	,cex=1
	,col=ifelse(d$urna,1,1))
lines(lowess(na.omit(cbind(d$invalidpresshare.1994[d$urna==T]
		,d$legendafedshare.1994[d$urna==T]))),col=gray(0.6),lwd=2)
lines(lowess(na.omit(cbind(d$invalidpresshare.1994[d$urna==F]
		,d$legendafedshare.1994[d$urna==F]))),col=gray(0.6),lwd=2)	
#abline(reg=lm(legendafedshare.1994~invalidpresshare.1994,data=subset(d,urna==T)),col=1)
#abline(reg=lm(legendafedshare.1994~invalidpresshare.1994,data=subset(d,urna==F)),col=1)
legend(x="topright",legend=c("Paper in 2008","EV in 2008")
		,pch=c(21,19),
		,col=c(1,1),
		,pt.cex=c(1,1),
		,bg=c(1,1),
		bty="n")
dev.off()

### Correlation coefficients ####
cor.test(d$legendafedshare.1998[d$urna==T],d$invalidpresshare.1998[d$urna==T])
cor.test(d$legendafedshare.1998[d$urna==F],d$invalidpresshare.1998[d$urna==F])

cor.test(d$legendafedshare.1994[d$urna==T],d$invalidpresshare.1994[d$urna==T])
cor.test(d$legendafedshare.1994[d$urna==F],d$invalidpresshare.1994[d$urna==F])
cor.test(d$legendafedshare.1994,d$invalidpresshare.1994)


cat("In the municipalities with EV in 1998, the correlation coefficient between the share of PLVs for federal legislators and the share of invalid votes for president is 0.52 (p$<<$0.01), while in the paper ballot municipalities it is -0.19 (p$<<$0.1). If we restrict the analysis only to municipalities within a band about the cutpoint of introduction of EV, the contrast is even starker, with correlation coefficeints of 0.75 (p$<<$ 0.01) and -0.27 (p=0.04) respectively. Importantly, in the preceding election (1994) this correlation coefficient was negative in both groups: (p=) and (p= )")


