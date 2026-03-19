# plot of loyalty over time
rm(list=ls())
allout <- read.csv("loyaltyLib.cvs")
alloutc <- read.csv("loyaltyCon.cvs")
pdf("Figure-A2.pdf")
par(mfrow=c(2,1))
lab <- c(1867,1872,1874,1878,1882,1887,1891,1896,1900,1904,1908,1911,1918,1922,1925,1926,1930,1935,1940,1945,1949,1953,1957,
         1958,1962,1963,1965,1968,1972,1974,1979,1980,1984,1988,1993,1997,2000,2004,2006,2008)
boxplot(allout$loyalty.x~allout$parliament, main="Voting Loyalty: Liberal Party",col="grey70",xaxt="n",ylab="Loyalty")
axis(1,at=seq(1,40,1),labels=lab)
boxplot(alloutc$loyalty.x~alloutc$parliament,main="Voting Loyalty: Conservative Party",col="grey70",xaxt="n",ylab="Loyalty")
axis(1,at=seq(1,40,1),labels=lab)
dev.off()
