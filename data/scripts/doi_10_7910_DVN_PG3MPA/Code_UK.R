rm(list=ls(all=TRUE))

### Get historical Met Office data
	use.years = c(1:137)
	max.temp.table = read.table("uk_max_temp.txt",header=TRUE,skip=7,sep="",as.is=TRUE)
	maxTemp = as.matrix(max.temp.table[use.years,2:13])
	rainfall.table = read.table("uk_rainfall.txt",header=TRUE,skip=7,sep="",as.is=TRUE)
	rainfall = as.matrix(rainfall.table[use.years,2:13])
	year = max.temp.table[use.years,1]

### calculate autocorrelations
acfTemp = as.vector(acf(as.vector(t(maxTemp)),1))[1]
acfRain = as.vector(acf(as.vector(t(rainfall)),1))[1]

png(filename='fig_UK_TempRain.png',width=1600,height=1700,bg='white')

par(mfrow=c(6,2))
cex.main = 3
cex.lab = 2
cex.axis = 2

for (monthIndex in seq(4,9)) {
	par(mar=c(2.5,5,0.3,1))
	plot(year,maxTemp[,monthIndex],type='l',lwd=2,
		xlab='',ylab='temperature (°C)',cex.lab=cex.lab,cex.axis=cex.axis)
	abline(h=seq(0,25,0.2),col='grey')
	title(paste(month.name[monthIndex],"temperature"),line=-2.5,cex.main=cex.main,adj=0.02)
	
	plot(year,rainfall[,monthIndex],type='h',lend=1,col='black',lwd=4,
		xlab='',ylab='rainfall (mm)',cex.lab=cex.lab,cex.axis=cex.axis)
	abline(h=seq(0,200,5),col='grey')
	title(paste(month.name[monthIndex],"rainfall"),line=-2.5,cex.main=cex.main,adj=0.02)
	
}
dev.off()


