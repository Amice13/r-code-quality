### CALIBRATION ANALYSIS

rm(list=ls(all=TRUE))

set_colour <- function( colsch ) {
	par(bg=colsch$bgcol,fg=colsch$fgcol,
		col.lab=colsch$fgcol,
		col.axis=colsch$fgcol,
		col.main=colsch$fgcol)
}

nSimulations = 10000

### leadtimes based on fraction of time remaining
leadtimeIsFraction = TRUE
calibrationLeadtimes = c(0.80, 0.40, 0.20)
#calibrationLeadtimes = seq(0.9,0.1,length.out=9)

### leadtimes based on number of days remaining
#leadtimeIsFraction = FALSE
#calibrationLeadtimes = c(120,90,60)
#calibrationLeadtimes = seq(120,10,-10)

deviations <- 0

nPanels = length(calibrationLeadtimes)
panelLabels = letters[1:nPanels]

dirname <- "verificationData"

colsch <- list(colour0='black', colour1=rgb(0.3,0.3,0.3), colour2=rgb(0.541,0.541,0.553),bgcol='white',fgcol='black')


fileList <- dir(dirname)
fileList <- fileList[substr(fileList,1,16)=="verificationData"]
nForecasts <- length(fileList)

summaryDF <- data.frame(market=rep(NA,nForecasts),transactions=rep(NA,nForecasts),firstTransaction=rep(as.Date("1970-01-01"),nForecasts),lastTransaction=rep(as.Date("1970-01-01"),nForecasts),verification=rep(NA,nForecasts),horizon=rep(NA,nForecasts))
marketCount <- 0

RELactual = rep(0,nPanels)
RELsimulated = array(0,c(nSimulations,nPanels))

png(filename="FIG_CALIBRATION.png",height=500,width=1200)
par(mfrow=c(1,3))

for (panelIndex in c(1:nPanels)) {
	calibrationLeadtime = calibrationLeadtimes[panelIndex]
	panelLabel = panelLabels[panelIndex]
	actualQuantiles <- rep(NA,nForecasts)
	simulatedQuantiles <- array(NA,c(nForecasts,nSimulations))
	for (iForecast in c(1:nForecasts)) {
		tmp <- load(paste(dirname,"/",fileList[iForecast],sep=""))
		if (grepl("COVID",verify$name) == FALSE) {	### inclusion criterion; ignore COVID markets
			cat("Market:",verify$name,"\n")	
			marketCount <- marketCount + 1	
			summaryDF[iForecast,]$market <- verify$name
			summaryDF[iForecast,]$transactions <- length(verify$timestamp)
			summaryDF[iForecast,]$verification <- verify$verification
			summaryDF[iForecast,]$firstTransaction <- as.Date(verify$timestamp[1])
			summaryDF[iForecast,]$lastTransaction <- as.Date(max(verify$timestamp))
			leadtimeInDays <- as.numeric((max(verify$timestamp) - verify$timestamp)/(3600*24))	
			summaryDF[iForecast,]$horizon = max(leadtimeInDays)
			fracLeadtime <- 1 - (max(leadtimeInDays)-leadtimeInDays)/max(leadtimeInDays) 
			if (leadtimeIsFraction) {
				k <- which(fracLeadtime > calibrationLeadtime)
			} else {
				k <- which(leadtimeInDays > calibrationLeadtime)
			}
			if (length(k) > 0) {
				use <- max(k)	### most recent prices before desired lead time
				probs <- verify$prices[use,]
				m <- length(probs)
				y <- verify$verification
				cdf = cumsum(probs)
				for (iSimulation in c(1:nSimulations)) {
					x <- sample(c(1:m),1,prob=probs)
					simulatedQuantiles[iForecast,iSimulation] = cdf[x] - 0.5*probs[x]
				}
				actualQuantiles[iForecast] = cdf[y] - 0.5*probs[y]			}
		}
	}

	### CALIBRATION PLOT
	RELsimulation <- rep(0,nSimulations)
	valid <- which(!is.na(actualQuantiles))
	diagonal <- seq(0.5,length(valid)-0.5)/length(valid)
	actualQuantiles <- actualQuantiles[valid]
	REL = sum((sort(actualQuantiles)-diagonal)^2)/length(valid)
	simulatedQuantiles <- simulatedQuantiles[valid,]
	simulatedSortedQuantiles <- simulatedQuantiles
	for (iSimulation in c(1:nSimulations)) {
		simulatedSortedQuantiles[,iSimulation] <- sort(simulatedQuantiles[,iSimulation])
		RELsimulation[iSimulation] = sum((simulatedSortedQuantiles[,iSimulation]-diagonal)^2)/length(valid)
	}
	
	### determine the top and bottom of the required interval covered by the envelope of synthetic curves
	probs <- c(0.025,0.975)
	q <- apply(simulatedSortedQuantiles,1,quantile,probs=probs)
	mn <- as.numeric(q[1,])
	mx <- as.numeric(q[2,])

	### quantiles of deviations
	RELsimulated[,panelIndex] = RELsimulation
	RELactual[panelIndex] = REL	  
	
	validForecasts <- length(valid)
	index <- c(0,seq(1,validForecasts)/(validForecasts+1),1)

	### plot anotation size parameters
	cex.legend <- 1.5; cex.main <- 2.5; cex.lab <- 2.0; cex.axis <- 2.0; cex.caption <- 2.0
	ylab <- "occurences lying below forecast quantile (%)"
	xlab <- "quantile of forecast probability distribution (%)"

	par(mar=c(5,5,3,3))
	set_colour( colsch )
	plot(c(0,100),c(0,100),type='n',ylab=ylab,xlab=xlab,cex.main=cex.main,
		cex.lab=cex.lab,cex.axis=cex.axis,main=paste(panelLabel))

	if (leadtimeIsFraction) {
		horizonLabel = sprintf('%i forecasts (%1.0f%% of horizon remaining)',validForecasts,calibrationLeadtime*100)
	} else {
		horizonLabel = sprintf('%i forecasts (%1.0f days remaining)',validForecasts,calibrationLeadtime)
	}

	xout <- seq(0,100,0.01)		
	xmn <- 100*c(0,mn,1)
	xmx <- 100*c(0,mx,1)
	y <- 100*index
	ymn <- approx(xmn,y,xout)$y
	ymx <- approx(xmx,y,xout)$y
	ymn1 <- ymn
	ymx1 <- ymx
	ymn1[is.na(ymn1)] <- ymn[is.na(ymn1)]
	ymx1[is.na(ymx1)] <- ymx[is.na(ymx1)] 
	polygon(c(xout,rev(xout)),c(ymn1,rev(ymx1)),col=colsch$colour2,border=NA)
	legend('bottomright',pch=c(15,NA),lwd=c(NA,4),col=c(colsch$colour2,colsch$colour1),
	legend=c('calibrated forecasts (95% interval)',horizonLabel),cex=cex.legend)
	
	lines(c(0,100),c(0,100), lty=2) ## plot diagonal

	X <- 100*c(0,sort(actualQuantiles),1)
	Y <- 100*index
	tmp <- approx(X,Y,xout)
	yout <- tmp$y##filter(tmp$y,filter=rep(1,filterWindow)/filterWindow)
	yout[is.na(yout)] <- tmp$y[is.na(yout)]
	lines(xout,yout,lwd=4,col=colsch$colour1)
	
}

dev.off()



png(filename="FIG_RELSCORE.png",height=500,width=700)
par(mar=c(5.1, 5.1, 4.1, 2.1))
cex.lab=1.5
### replace data points outside 95% interval with NAs to force the boxplot whiskers to show this interval.
for (iPanel in c(1:nPanels)) {	
	tmp = RELsimulated[,iPanel]
	q = quantile(tmp,c(0.025,0.975))
	tmp[tmp < q[1] | tmp > q[2]] = NA
	RELsimulated[,iPanel] = tmp
}

if (leadtimeIsFraction) {
	boxplot(RELsimulated,at=seq(1,nPanels),
		names=calibrationLeadtimes,
		xlab='fraction of market duration remaining',cex.lab=cex.lab,
		ylab='REL score',range=0,
		outline=FALSE)
	points(RELactual,col='black',pch=16,cex=2)
}

if (leadtimeIsFraction==FALSE) {
	boxplot(RELsimulated,at=seq(1,nPanels),
		names=calibrationLeadtimes,
		xlab='number of days remaining',cex.lab=cex.lab,
		ylab='REL score',
		outline=FALSE)
	points(RELactual,col='black',pch=16,cex=2)
}

legend('topleft',legend=c('actual score'),pch=16,cex=2)

dev.off()
