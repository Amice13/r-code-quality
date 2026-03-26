library(lubridate)

setwd("~/Dropbox/PDparty/rcode/replication/")

dta.his <- read.csv("pid-hispanic.csv")
dta.his$Year[14] <- 2018.5

dta.asn <- read.csv("pid-asian-american.csv")
dta.asn$Year[21] <- 2018.5

#pdf("pew-pid-hispanic-02092022.pdf",width=6)
plot(x=dta.his$Year,y=dta.his$Rep.Lean.Rep,cex.lab=1.35,
     type="b",lty=2,pch=15,ylim=c(0,100),xlim=c(1998,2020),
     xaxt="n",
     ylab="Percentage of Respondents",xlab="Year",
     main="Party ID: Hispanics",cex.main=1.8)
par(new=T)
plot(x=dta.his$Year,y=dta.his$Dem.Lean.Dem,cex.lab=1.35,
     type="b",lty=3,pch=16,ylim=c(0,100),xaxt="n",
     ylab="",xlab="",xlim=c(1998,2020))
axis(side=1,at=seq(from=2000,to=2020,by=2),
     labels=seq(from=2000,to=2020,by=2),cex.lab=1.35,cex=1.35)
text(x=2002,y=64,"Democrats",cex=1.5)
text(x=2002,y=35.5,"Republicans",cex=1.5)

dev.off()

pdf("pew-pid-asn-am-02092022.pdf",width=6)
plot(x=dta.asn$Year,y=dta.asn$Rep.Lean.Rep,cex.lab=1.35,
     type="b",lty=2,pch=15,ylim=c(0,100),xlim=c(1998,2020),xaxt="n",
     ylab="Percentage of Respondents",xlab="Year",
     main="Party ID: Asian Americans",cex.main=1.8)
par(new=T)
plot(x=dta.asn$Year,y=dta.asn$Dem.Lean.Dem,cex.lab=1.35,
     type="b",lty=3,pch=16,ylim=c(0,100),xlim=c(1998,2020),xaxt="n",
     ylab="",xlab="")
axis(side=1,at=seq(from=1998,to=2020,by=2),
     labels=seq(from=1998,to=2020,by=2),cex.lab=1.35,cex=1.35)

text(x=2018,y=78,"Democrats",cex=1.5)
text(x=2018,y=12,"Republicans",cex=1.5)

dev.off()
