rm(list=ls(all=TRUE))

x = read.table("Atlantic Storm Numbers.csv",header=TRUE,sep=",")

use = which(x$Year >= 1900)
year = x$Year[use]
hurricanes = x$Hurricanes[use]
landfalling = x$Landfall[use]
png(filename='fig_HURRICANES.png',width=800,height=500)
par(mar=c(3,5,1,2))
plot(year,hurricanes,type='h',lwd=4,lend=1,col='grey',cex.lab=1.5,ylab='number of hurricanes',xlab='',cex.axis=1.1)
##lines(year,landfalling,type='h',lwd=5,lend=1,col='black')
##legend('topleft',pch=c(15,15),col=c('grey','black'),legend=c('total hurricanes (CAT 1-5)','landfalling hurricanes'),cex=1.2)
legend('topleft',pch=c(15),col=c('grey'),legend=c('total hurricanes (CAT 1-5)'),cex=1.2)


dev.off()
