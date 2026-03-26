# Figures in the appendix
###Figure-A1

x<-c(1:40)
year <- c(1867,1872,1874,1878,1882,1887,1891,1896,1900,1904,1908,1911,1917,1921,1925,1926,1930,1935,1940,1945,1949,1953,1957,1958,1962,1963,1965,1968,1972,1974,1979,1980,1984,1988,1993,1997,2000,2004,2006,2008)
house.total<-c(283,39,127,141,210,122,120,87,92,115,59,121,73,85,27,94,149,43,79,152,205,263,18,243,17,124,98,360,81,360,8,621,915,976,735,1991,727,190,380,363)
house <- data.frame(x,house.total)

pdf("Figure-A1.pdf")
plot(house,type="p",
     ylim=c(0,2000),main="Total Recorded Votes in the House of Commons",sub="1st-40th (1867-2011)",
     xlab="Parliaments",ylab="Number of Recorded Votes",col="black",pch=16,xaxt='n',cex=1)
axis(x,at=seq(1,40,1),labels=year)
x<-x
y<-house.total
y.loess <- loess(y ~ x, span=.5, data.frame(x=x, y=y))
y.predict <- predict(y.loess, data.frame(x=x))
lines(x,y.predict,col="black",lwd=1)
dev.off()

####Figure-D1

counter<-c(1:40)
year <- c(1867,1872,1874,1878,1882,1887,1891,1896,1900,1904,1908,1911,1917,1921,1925,1926,1930,1935,1940,1945,1949,1953,1957,1958,1962,1963,1965,1968,1972,1974,1979,1980,1984,1988,1993,1997,2000,2004,2006,2008)
libs.French <- c(0.1904762,0.2187500,0.2027972,0.1692308,0.1428571,0.2625000,0.3061224,0.3623188,0.3581081,0.3441558,0.3308271,0.3563218,0.6666667,0.4883721,0.4951456,0.4503817,0.4444444,0.4042553,0.3405405,0.3858268,0.3451777,0.3910615,0.5825243,0.5283019,0.3636364,0.3816794,0.4202899,0.3734177,0.5045872,0.4362416,0.5752212,0.4701987,0.3658537,0.1882353,0.1593407,0.1951220,0.2485876,0.1764706,0.1509434,0.1428571)
cons.French <- c(0.280000000,0.250000000,0.308641975,0.291390728,0.301369863,0.180451128,0.201388889,0.142857143,0.073170732,0.089743590,0.069767442,0.160839161,0.000000000,0.037735849,0.008928571,0.000000000,0.137681159,0.000000000,0.000000000,0.014084507,0.056603774,0.054545455,0.072072072,0.200934579,0.113043478,0.112244898,0.070707071,0.092105263,0.018518519,0.037037037,0.022058824,0.018691589,0.262910798,0.329341317,NaN,0.260869565,0.022988506,0.030303030,0.109375000,0.076388889)
libs.Farmer.West <- c(0.011904762,0.000000000,0.000000000,0.000000000,0.000000000,0.000000000,0.000000000,0.000000000,0.013513514,0.019480519,0.030075188,0.057471264,0.023809524,0.023255814,0.077669903,0.122137405,0.090909091,0.069148936,0.054054054,0.062992126,0.071065990,0.033519553,0.029126214,0.018867925,0.010101010,0.007633588,0.007246377,0.018987342,0.009174312,0.006711409,0.000000000,0.000000000,0.000000000,0.000000000,0.005494505,0.000000000,0.000000000,0.000000000,0.000000000,0.000000000)
cons.Farmer.West <- c(0.008000000,0.000000000,0.000000000,0.000000000,0.000000000,0.000000000,0.006944444,0.000000000,0.000000000,0.012820513,0.046511628,0.020979021,0.106666667,0.000000000,0.017857143,0.000000000,0.036231884,0.045454545,0.050000000,0.028169014,0.037735849,0.000000000,0.027027027,0.098130841,0.165217391,0.204081633,0.191919192,0.118421053,0.092592593,0.111111111,0.095588235,0.102803738,0.065727700,0.029940120,NaN,0.000000000,0.068965517,0.050505051,0.015625000,0.034722222)

pdf("Figure-D1.pdf")
par(mfrow=c(1,2))
plot(counter,libs.French,ylim=c(0,.6),col="black",pch=20,
     bty="n",xlab="Parliament",ylab="Percentage of Total Caucus",cex=.75,
     main="Proportion of French/Catholic MPs in the Parties",xaxt='n')
axis(1,at=seq(1,40,1),labels=year)
y <- libs.French
x <- counter
y.loess <- loess(y ~ x, span=.5, data.frame(x=x, y=y))
y.predict <- predict(y.loess, data.frame(x=x))
lines(x,y.predict,col="black",lwd=1)
points(cons.French,cons$median.loyalty,col="grey",pch=20,cex=.75)
y <- cons.French
x <- counter
y.loess <- loess(y ~ x, span=.5, data.frame(x=x, y=y))
y.predict <- predict(y.loess, data.frame(x=x))
lines(x,y.predict,col="grey")
legend("topleft",legend=c("Liberal","Conservative"),col=c("black","grey"),
       pch=20,cex=.75,bty="n")

plot(counter,libs.Farmer.West,ylim=c(0,.6),col="black",pch=20,
     bty="n",xlab="Parliament",ylab="Percentage of Total Caucus",cex=.75,
     main="Proportion of Western Farmer MPs in the Parties",xaxt='n')
axis(1,at=seq(1,40,1),labels=year)
y <- libs.Farmer.West
x <- counter
y.loess <- loess(y ~ x, span=.5, data.frame(x=x, y=y))
y.predict <- predict(y.loess, data.frame(x=x))
lines(x,y.predict,col="black")
points(cons.Farmer.West,cons$median.loyalty,col="grey",pch=20,cex=.75)
y <- cons.Farmer.West
x <- counter
y.loess <- loess(y ~ x, span=.5, data.frame(x=x, y=y))
y.predict <- predict(y.loess, data.frame(x=x))
lines(x,y.predict,col="grey")
legend("topleft",legend=c("Liberal","Conservative"),col=c("black","grey"),
       pch=20,cex=.75,bty="n")
dev.off()
