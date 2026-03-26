### This code produces the plot showing the evolution of marginal distributions from
### the UK temperature-rainfall climate prediction market.

rm(list=ls(all=TRUE))

### Helper functions
estimate_quantiles <- function( X, p, q ) {
	P <- cumsum(p)
	Q <- approx(P,X,q)
	return(Q$y)
}

estimate_moments <- function( X, p ) {
	m1 <- sum(p*X)			### mean
	m2 <- sqrt(sum(p*(X-m1)^2))	### std. dev.
	return(c(m1,m2))
}

##########################################################################################################

### Get historical Met Office data
	use.years <- c(1:108)
	max.temp.table <- read.table("UK_max_temp.txt",header=TRUE,skip=7,sep="",as.is=TRUE)
	max.temp <- as.matrix(max.temp.table[use.years,5:10])
	rainfall.table <- read.table("UK_rainfall.txt",header=TRUE,skip=7,sep="",as.is=TRUE)
	rainfall <- as.matrix(rainfall.table[use.years,5:10])


### plot size parameters
	cex.axis <- 2
	cex.lab <- 2
	cex.main <- 3
	cex.legend <- 3

### other parameters
	monthName <- c('April','May','June','July','August','September')	
	monthNumber <- c('04','05','06','07','08','09')
	nTrades <- rep(0,6)
	timeRange <- c(as.POSIXct("2018-03-12 08:00"),as.POSIXct("2018-10-01 08:00"))
	firstdays <- as.POSIXct(seq(as.Date("2018-03-01"),as.Date("2018-10-01"),by="month"))
	middays <- as.POSIXct(seq(as.Date("2018-03-15"),as.Date("2018-10-15"),by="month"))

png(filename='Fig_MarginalDistributions.png',width=1600,height=1700,bg='white')
par(mfrow=c(6,2))

for (monthIndex in c(1:6)) {
	for (variableName in c('Temperature','Rainfall')) {
		if (variableName=='Temperature') {
			X <- seq(0,25.2,0.2)-0.1	### temperature interval midpoints
			ylab <- 'temperature (°C)'
			ylim <- c(0,25)
			climateMean <- mean(max.temp[,monthIndex])
			climateStdev <- sd(max.temp[,monthIndex])
		} 
		if (variableName=='Rainfall') {
			X <- seq(0,200,5)+2.5		### rainfall interval midpoints
			ylab <- 'rainfall (mm)'
			ylim <- c(0,200)
			climateMean <- mean(rainfall[,monthIndex])
			climateStdev <- sd(rainfall[,monthIndex])
		}

		par(mar=c(2.5,5,0.3,1))
		filename <- sprintf('verificationData_WintonCPM_%s_%s.RData',variableName,monthName[monthIndex])
		tmp <- load(filename)
		days <- as.Date(verify$timestamp)
		use <- which(days<max(days))		### don't include trades made on last day, these are settlements not actual trades
		nTrades[monthIndex] <- length(use)
		prices <- verify$prices[use,]
		timestamp <- verify$timestamp[use]	
		n <- nrow(prices)
		m <- ncol(prices)
		Q <- array(0,c(n,2))
		for (t in c(1:n)) {
			Q[t,] <- estimate_moments( X, prices[t,] )
		}	
		plot(timestamp,Q[,1],type='n',xlim=timeRange,ylim=ylim,
			ylab=ylab,xlab='',xaxt='n',cex.axis=cex.axis,cex.lab=cex.lab)
		if (monthIndex==6) {
			axis(1,at=middays,labels=month.abb[3:10],cex.axis=cex.axis,tick=FALSE)
		}
		relevantMonth <- firstdays[(monthIndex+1):(monthIndex+2)]
		polygon(c(relevantMonth,rev(relevantMonth)),c(-1000,-1000,1000,1000),col='lightblue',border=NA)	### highlight relevant month
		title(monthName[monthIndex],line=-2.5,cex.main=cex.main,adj=0.02)
		polygon(c(timestamp,rev(timestamp)),c(Q[,1]+Q[,2],rev(Q[,1]-Q[,2])),col='grey',border='grey')
		lines(timestamp,Q[,1],lwd=1,col='black')
		points(relevantMonth[2],X[verify$verification],pch=16,col='red',cex=4)
		abline(v=firstdays,lty=2)	
		abline(h=climateMean,col='green')
		abline(h=climateMean+climateStdev,col='green',lty=2)
		abline(h=climateMean-climateStdev,col='green',lty=2)
		if (monthIndex==1 && variableName=="Temperature") {
			legend('topright',pch=c(NA,15,16,NA,NA),lwd=c(2,NA,NA,1,1),lty=c(1,NA,NA,1,2),bg=rgb(0.95,0.95,0.95),
			col=c('black','grey','red','green','green'),legend=c('mean','std. dev','actual','1910-2017 mean','1910-2017 std. dev.'),cex=cex.legend)
		}
	}
}

dev.off()


png(filename='Fig_Activity.png',width=1600,height=1700)
par(mfrow=c(6,2))

allMarkets <- data.frame(timestamp=c(),accountId=c(),credits=c(),count=c())

for (monthIndex in c(1:6)) {
		par(mar=c(2.5,5,0.3,1))
		x <- seq(as.Date(timeRange[1]),as.Date(timeRange[2]),by='day')
		filename <- sprintf('CPM_tradeData_2018-%s.csv',monthNumber[monthIndex])
		tmp <- read.table(filename,sep=",",header=TRUE)
		tmp$count <- rep(1,nrow(tmp))
		days <- as.Date(tmp$timestamp)
		use <- (days < max(days)) ### drop last day because they are settlements
		days <- days[use]
		allMarkets <- rbind(allMarkets,data.frame(timestamp=tmp$timestamp,accountId=tmp$accountId,credits=tmp$credits,count=rep(1,length(tmp$credits))))
		trades <- aggregate(tmp$count[use],by=list(days),FUN=sum)
		volume <- aggregate(abs(tmp$credits[use]),by=list(days),FUN=sum)
		plot(trades$Group.1,trades$x,type='n',xlim=as.Date(timeRange),ylim=c(0,1400),
			ylab='number of trades',xlab='',xaxt='n',cex.axis=cex.axis,cex.lab=cex.lab)
		relevantMonth <- as.Date(firstdays[(monthIndex+1):(monthIndex+2)])
		polygon(c(relevantMonth,rev(relevantMonth)),c(-1000,-1000,10000,10000),col='lightblue',border=NA)	### highlight relevant month
		title(monthName[monthIndex],line=-2.5,cex.main=cex.main,adj=0.02)
		lines(trades$Group.1,trades$x,type='h',lend=1,lwd=3,col='black')
		abline(v=as.Date(firstdays),lty=2)	
		if (monthIndex==6) {
			axis(1,at=as.Date(middays),labels=month.abb[3:10],cex.axis=cex.axis,tick=FALSE)
		}
		plot(volume$Group.1,volume$x,type='n',xlim=as.Date(timeRange),ylim=c(0,12000),
			ylab='trade volume (credits)',xlab='',xaxt='n',cex.axis=cex.axis,cex.lab=cex.lab)
		polygon(c(relevantMonth,rev(relevantMonth)),c(-1000,-1000,20000,20000),col='lightblue',border=NA)	### highlight relevant month
		title(monthName[monthIndex],line=-2.5,cex.main=cex.main,adj=0.02)
		lines(volume$Group.1,volume$x,type='h',lend=1,lwd=3,col='black')
		abline(v=as.Date(firstdays),lty=2)		
		if (monthIndex==6) {
			axis(1,at=as.Date(middays),labels=month.abb[3:10],cex.axis=cex.axis,tick=FALSE)
		}
}

dev.off()




