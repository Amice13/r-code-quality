###############################################################################
#
#	Analysis of regional monthly data that will be used to settle
#
###############################################################################

rm(list=ls(all=TRUE))
require(maps)
data.directory <- ""#../Climate Data/"

### Sea Surface Temperature Data
	ssta.name <- 'ANOM.3';
	sst.table <- read.table(paste(data.directory,"ersst5.nino.mth.81-10.ascii.txt",sep=""),header=TRUE,sep="",as.is=TRUE)
	sstdates <- sst.table[,1] + sst.table[,2]/12
	ninossta <- sst.table[,which(names(sst.table) == ssta.name)]
	png(filename='fig_NINO3.4SSTA.png',width=800,height=500)
	par(mar=c(3,5,1,2))
	plot(sstdates,ninossta,lwd=3,type='l',col='black',xlab='',ylab='NINO3.4SSTA (\u00b0C)',
		cex.lab=1.5,cex.axis=1.4,cex.main=1.5,main="")
	abline(h=0)
	dev.off()
	
	
stop()

### Plot map showing where NINO3.4 region is
	windows(9,5)
	m <- map("world",fill=TRUE,wrap=c(0,360),bg=colours[2],mar=c(0,0,0,0),myborder=0,col=colour1)
	polygon(360+c(-170,-170,-120,-120),c(-5,5,5,-5),col=colours[10])
	text(360-145,0,"NINO 3.4")

