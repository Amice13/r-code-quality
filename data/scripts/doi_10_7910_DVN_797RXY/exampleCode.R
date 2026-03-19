#########################################################################################################
#
#	This code reconstructs the price history of a temperature-rainfall joint-outcome market
#	from the record of individual trades.
#
#########################################################################################################
rm(list=ls(all=TRUE))

### helper functions
lmsr_price <- function( q, b ) {	### this is the formula the LMSR market maker uses to set prices
	x <- q/b
	xmax <- max(x)
	p <- exp(x-xmax)/sum(exp(x-xmax))
	return(p)
}

filename <- "CPM_tradeData_2018-07.csv"					### tradeData file to use to


x <- read.table(filename,",",header=TRUE)	### read in trade data
timestamp <- as.POSIXct(x[,1],format="%Y-%m-%dT%H:%M:%OS")		### convert timestamps to calendar objects
accountId <- as.character(x[,2])						### account IDs for each trade	
nTrades <- length(timestamp)							### number of trades
nOutcomes <- 5207									### number of outcomes
b <- 500										### liquidity parameter
temperature <- seq(-0.1,25.1,0.2)						### temperature interval midpoints
rainfall <- seq(2.5,202.5,5)							### rainfall interval midpoints

q <- rep(0,nOutcomes)								### array for market maker's total exposure	
P <- array(0,c(nTrades,nOutcomes))						### array for prices

for (iTrade in c(1:nTrades)) {
	q <- q + as.numeric(x[iTrade,4:5210])				### add trade to market maker's total exposure
	P[iTrade,] <- lmsr_price(q,b)						### convert exposure to price 
}

### plot a price snapshot on a two-dimensional grid
	index <- 1000								### index of timestamp to plot
	P2 <- t(array(P[index,],c(41,127)))					### place prices on a 2d grid
	windows(8,6)
	filled.contour(temperature,rainfall,P2,
		xlab='temperature (deg. C)',ylab='rainfall (mm)',main=timestamp[index])	### plot prices


### calculate marginal distributions
	pTemperature <- apply(P2,1,sum)
	pRainfall <- apply(P2,2,sum)
	windows(8,6)
	plot(temperature,pTemperature,type='l',xlab='temperature (deg. C)',ylab='price')
	windows(8,6)
	plot(rainfall,pRainfall,type='l',xlab='rainfall (mm)',ylab='price')

	