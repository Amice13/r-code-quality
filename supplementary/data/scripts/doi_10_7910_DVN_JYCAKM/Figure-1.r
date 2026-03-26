# plot of median position and deviation
#FIGURE-1 PAPER
tmp <- read.csv("loyaltyLib.cvs")
tmpc <- read.csv("loyaltyCon.cvs")
libsmedian <- aggregate(tmp$loyalty.x,FUN=median,by=list(tmp$parliament))
libsmad <- aggregate(tmp$loyalty.x,FUN=mad,by=list(tmp$parliament))
consmedian <- aggregate(tmpc$loyalty.x,FUN=median,by=list(tmpc$parliament))
consmad <- aggregate(tmpc$loyalty.x,FUN=mad,by=list(tmpc$parliament))
pdf("Figure-1a.pdf",width=7,height=2.75)
par(mar=c(4.1,4.1,3.1,2.1))
counter<-c(1:40)
year <- c(1867,1872,1874,1878,1882,1887,1891,1896,1900,1904,1908,1911,1917,1921,
          1925,1926,1930,1935,1940,1945,1949,1953,1957,1958,1962,1963,1965,1968,
          1972,1974,1979,1980,1984,1988,1993,1997,2000,2004,2006,2008)
plot(counter,libsmedian$x,ylim=c(.5,1),col="black",pch=20,
     bty="n",ylab="Median Loyalty Score",cex=.75,
     xlab="Development in Median Level Party Loyalty",xaxt='n')
axis(1,at=seq(1,40,1),labels=year)
y <- libsmedian$x
y.loess <- loess(y ~ counter, span=.5, data.frame(x=counter, y=y))
y.predict <- predict(y.loess, data.frame(x=counter))
lines(counter,y.predict,col="black",lwd=1)
legend("right",legend=c("Liberal","Conservative"),col=c("black","grey"),
       pch=20,lty=1,cex=.75,bty="n")
segments(y0=libsmedian$x+libsmad$x,x0=counter,
         y1=libsmedian$x-libsmad$x,x1=counter,col="black")
relterms <-c(1:34,36:40)+.5
points(relterms,consmedian$x,col="grey",pch=20,cex=.75)
y <- consmedian$x
y.loess <- loess(y ~ relterms, span=.5, data.frame(x=relterms, y=y))
y.predict <- predict(y.loess, data.frame(x=relterms))
lines(relterms,y.predict,col="grey",lwd=1)
segments(y0=consmedian$x+consmad$x,x0=relterms,
         y1=consmedian$x-consmad$x,x1=relterms,col="grey")
dev.off()
pdf("Figure-1b.pdf",width=7,height=2.75)
par(mar=c(4.1,4.1,3.1,2.1))
plot(counter,libsmad$x,col="black",pch=20,
     bty="n",ylab="Median Absolute Deviation",cex=.75,
     xlab="Development in Median Absolute Deviation",xaxt='n')
axis(1,at=seq(1,40,1),labels=year)
points(relterms,consmad$x,col="grey",pch=20,cex=.75)
y <- libsmad$x
x <- counter
y.loess <- loess(y ~ x, span=.5, data.frame(x=x, y=y))
y.predict <- predict(y.loess, data.frame(x=x))
lines(x,y.predict,col="black",lwd=1)
y <- consmad$x
x <- relterms
y.loess <- loess(y ~ x, span=.5, data.frame(x=x, y=y))
y.predict <- predict(y.loess, data.frame(x=x))
lines(x,y.predict,col="grey",lwd=1)
legend("right",legend=c("Liberal","Conservative"),col=c("black","grey"),
       pch=20,lty=1,cex=.75,bty="n")
dev.off()
