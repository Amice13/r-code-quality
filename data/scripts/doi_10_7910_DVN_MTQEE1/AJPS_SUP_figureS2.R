#setwd("/Users/bradfordjones/Dropbox/AJPS R&R/Analysis Files/")
#setwd("/Users/bradfordjones/Dropbox/AJPS R&R/Dataverse Files for AJPS/")


library(foreign)

timedata<-read.dta("USAandstates.dta")
attach(timedata)
par(mfrow=c(3,2), oma=c(1,2,2,2), mar=c(2, 5, 2, 2))
plot(countco~obs, pch=20, cex.axis=.65, main="A. Southern States", ylab="Monthly Counts (from Newsbank)",cex.lab=.65,  cex.main=.8, col="salmon", ylim=c(0,100), xaxt="n", xlim=c(61,423), type="l", xlab="")
axis(1, at=c(61,73,85, 97, 109, 121, 133, 145, 157, 169, 181, 193, 205, 217, 229, 241, 253,
265, 277, 289, 301, 313, 325, 337, 349, 361, 373, 385, 397, 409, 421), labels=c("1985", "1986", "1987","1988","1989","1990","1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001",
"2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), cex.axis=.55, las=2)
par(new=TRUE)

plot(countac~obs, pch=20, cex.axis=.65, main="", ylab="",cex.lab=.65,  cex.main=.8, col="maroon", ylim=c(0,100), xaxt="n", type="l", xlim=c(61,423), xlab="")
par(new=TRUE)


plot(countscs~obs, pch=20, cex.axis=.65, main="", ylab="",cex.lab=.65,  cex.main=.8, col="red", ylim=c(0,100), xaxt="n", type="l", xlim=c(61,423), xlab="")
par(new=TRUE)


plot(countmpr~obs, pch=20, cex.axis=.65, main="", ylab="",cex.lab=.65,  cex.main=.8, col="black", ylim=c(0,100), xaxt="n", type="l", xlim=c(61,423),xlab="")
par(new=TRUE)
abline(v=373, lty=3, col="grey")
abline(v=265, lty=3, col="grey")
text(320, 93, "Period of Study", cex=.55)

legend("topleft", c("Charlotte Observer", "Atlanta Constitution", "The State (South Carolina)", "Mobile Press-Register"), fill=c("salmon", "maroon", "red", "black"), cex=.65, box.lty=0)
 
plot(countcst~obs, pch=20, cex.axis=.65, main="B. Midwestern States", ylab="Monthly Counts (from Newsbank)",cex.lab=.65,  cex.main=.8, col="salmon", ylim=c(0,100), xaxt="n", xlim=c(61,423), type="l", xlab="")
axis(1, at=c(61,73,85, 97, 109, 121, 133, 145, 157, 169, 181, 193, 205, 217, 229, 241, 253,
265, 277, 289, 301, 313, 325, 337, 349, 361, 373, 385, 397, 409, 421), labels=c("1985", "1986", "1987","1988","1989","1990","1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001",
"2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), cex.axis=.55, las=2)
par(new=TRUE)

plot(countslpd~obs, pch=20, cex.axis=.65, main="", ylab="",cex.lab=.65,  cex.main=.8, col="maroon", ylim=c(0,100), xaxt="n", type="l", xlim=c(61,423), xlab="")
par(new=TRUE)


plot(countmjs~obs, pch=20, cex.axis=.65, main="", ylab="",cex.lab=.65,  cex.main=.8, col="red", ylim=c(0,100), xaxt="n", type="l", xlim=c(61,423), xlab="")
par(new=TRUE)
abline(v=373, lty=3, col="grey")
abline(v=265, lty=3, col="grey")
text(320, 93, "Period of Study", cex=.55)

legend("topleft", c("Chicago Sun Times", "St. Louis Post-Dispatch", "Milwaukee Journal Sentinal"), fill=c("salmon", "maroon", "red"), cex=.65, box.lty=0)


plot(countdp~obs, pch=20, cex.axis=.65, main="C. Western States", ylab="Monthly Counts (from Newsbank)",cex.lab=.65,  cex.main=.8, col="salmon", ylim=c(0,100), xaxt="n", xlim=c(61,423), type="l", xlab="")
axis(1, at=c(61,73,85, 97, 109, 121, 133, 145, 157, 169, 181, 193, 205, 217, 229, 241, 253,
265, 277, 289, 301, 313, 325, 337, 349, 361, 373, 385, 397, 409, 421), labels=c("1985", "1986", "1987","1988","1989","1990","1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001",
"2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), cex.axis=.55, las=2)
par(new=TRUE)

plot(countst~obs, pch=20, cex.axis=.65, main="", ylab="",cex.lab=.65,  cex.main=.8, col="maroon", ylim=c(0,100), xaxt="n", type="l", xlim=c(61,423), xlab="")
par(new=TRUE)


plot(countpo~obs, pch=20, cex.axis=.65, main="", ylab="",cex.lab=.65,  cex.main=.8, col="red", ylim=c(0,100), xaxt="n", type="l", xlim=c(61,423), xlab="")
par(new=TRUE)

plot(countslt~obs, pch=20, cex.axis=.65, main="", ylab="",cex.lab=.65,  cex.main=.8, col="tomato", ylim=c(0,100), xaxt="n", type="l", xlim=c(61,423), xlab="")
par(new=TRUE)
abline(v=373, lty=3, col="grey")
abline(v=265, lty=3, col="grey")
text(320, 93, "Period of Study", cex=.55)

legend("topleft", c("Denver Post", "Seattle Times", "The Oregonian (Portland)", "Salt Lake Tribute"), fill=c("salmon", "maroon", "red", "tomato"), cex=.65, box.lty=0)
 

plot(countnjsl~obs, pch=20, cex.axis=.65, main="D. Northeaster/Atlantic States", ylab="Monthly Counts (from Newsbank)",cex.lab=.65,  cex.main=.8, col="salmon", ylim=c(0,100), xaxt="n", xlim=c(61,423), type="l", xlab="")
axis(1, at=c(61,73,85, 97, 109, 121, 133, 145, 157, 169, 181, 193, 205, 217, 229, 241, 253,
265, 277, 289, 301, 313, 325, 337, 349, 361, 373, 385, 397, 409, 421), labels=c("1985", "1986", "1987","1988","1989","1990","1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001",
"2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), cex.axis=.55, las=2)
par(new=TRUE)

plot(countrtd~obs, pch=20, cex.axis=.65, main="", ylab="",cex.lab=.65,  cex.main=.8, col="maroon", ylim=c(0,100), xaxt="n", type="l", xlim=c(61,423), xlab="")
par(new=TRUE)


plot(countpi~obs, pch=20, cex.axis=.65, main="", ylab="",cex.lab=.65,  cex.main=.8, col="red", ylim=c(0,100), xaxt="n", type="l", xlim=c(61,423), xlab="")
par(new=TRUE)

plot(countbs~obs, pch=20, cex.axis=.65, main="", ylab="",cex.lab=.65,  cex.main=.8, col="tomato", ylim=c(0,100), xaxt="n", type="l", xlim=c(61,423), xlab="")
par(new=TRUE)
abline(v=373, lty=3, col="grey")
abline(v=265, lty=3, col="grey")
text(320, 93, "Period of Study", cex=.55)

legend("topleft", c("Star Ledger (New Jersey)", "Richmond Times-Dispatch", "Philadelphia Inquirer", "Baltimore Sun"), fill=c("salmon", "maroon", "red", "tomato"), cex=.65, box.lty=0)


plot(countads~obs, pch=20, cex.axis=.65, main="E. Traditional Receiving", ylab="Monthly Counts (from Newsbank)",cex.lab=.65,  cex.main=.8, col="salmon", ylim=c(0,100), xaxt="n", xlim=c(61,423), type="l", xlab="")
axis(1, at=c(61,73,85, 97, 109, 121, 133, 145, 157, 169, 181, 193, 205, 217, 229, 241, 253,
265, 277, 289, 301, 313, 325, 337, 349, 361, 373, 385, 397, 409, 421), labels=c("1985", "1986", "1987","1988","1989","1990","1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001",
"2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), cex.axis=.55, las=2)
par(new=TRUE)

plot(countladn~obs, pch=20, cex.axis=.65, main="", ylab="",cex.lab=.65,  cex.main=.8, col="maroon", ylim=c(0,100), xaxt="n", type="l", xlim=c(61,423), xlab="")
par(new=TRUE)


plot(countsfc~obs, pch=20, cex.axis=.65, main="", ylab="",cex.lab=.65,  cex.main=.8, col="red", ylim=c(0,100), xaxt="n", type="l", xlim=c(61,423), xlab="")
par(new=TRUE)

plot(countutsd~obs, pch=20, cex.axis=.65, main="", ylab="",cex.lab=.65,  cex.main=.8, col="tomato", ylim=c(0,100), xaxt="n", type="l", xlim=c(61,423), xlab="")
par(new=TRUE)


plot(countdmn~obs, pch=20, cex.axis=.65, main="", ylab="",cex.lab=.65,  cex.main=.8, col="black", ylim=c(0,100), xaxt="n", type="l", xlim=c(61,423), xlab="")
par(new=TRUE)


plot(counthc~obs, pch=20, cex.axis=.65, main="", ylab="",cex.lab=.65,  cex.main=.8, col="pink", ylim=c(0,100), xaxt="n", type="l", xlim=c(61,423), xlab="")
par(new=TRUE)




plot(countmh~obs, pch=20, cex.axis=.65, main="", ylab="",cex.lab=.65,  cex.main=.8, col="purple", ylim=c(0,100), xaxt="n", type="l", xlim=c(61,423), xlab="")
par(new=TRUE)
abline(v=373, lty=3, col="grey")
abline(v=265, lty=3, col="grey")
text(320, 93, "Period of Study", cex=.55)



legend("topleft", c("Arizona Daily Star", "L.A. Daily News", "San Francisco Chronicle", "Union Tribune (San Diego)", "Dallas Morning News", "Houston Chronicle", "Miami Herald"), fill=c("salmon", "maroon", "red", "tomato", "black", "pink", "purple"), cex=.65, box.lty=0)
 

plot(countelnuevo~obs, pch=20, cex.axis=.65, main="F. Spanish Language", ylab="Monthly Counts (from Newsbank)",cex.lab=.65,  cex.main=.8, col="salmon", ylim=c(0,200), xaxt="n", xlim=c(61,423), type="l", xlab="")
axis(1, at=c(61,73,85, 97, 109, 121, 133, 145, 157, 169, 181, 193, 205, 217, 229, 241, 253,
265, 277, 289, 301, 313, 325, 337, 349, 361, 373, 385, 397, 409, 421), labels=c("1985", "1986", "1987","1988","1989","1990","1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001",
"2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), cex.axis=.55, las=2)
par(new=TRUE)

plot(countlaopinion~obs, pch=20, cex.axis=.65, main="", ylab="",cex.lab=.65,  cex.main=.8, col="maroon", ylim=c(0,200), xaxt="n", type="l", xlim=c(61,423), xlab="")
par(new=TRUE)


plot(countimpacto~obs, pch=20, cex.axis=.65, main="", ylab="",cex.lab=.65,  cex.main=.8, col="red", ylim=c(0,200), xaxt="n", type="l", xlim=c(61,423), xlab="")
par(new=TRUE)

plot(countaldia~obs, pch=20, cex.axis=.65, main="", ylab="",cex.lab=.65,  cex.main=.8, col="tomato", ylim=c(0,200), xaxt="n", type="l", xlim=c(61,423), xlab="")
par(new=TRUE)

abline(v=373, lty=3, col="grey")
abline(v=265, lty=3, col="grey")
text(320, 175, "Period of Study", cex=.55)



legend("topleft", c("El Nuevo Herald (Miami)", "La Opinion (Los Angeles)", "Impacto USA (Los Angeles)", "Al Dia (Dallas)"), fill=c("salmon", "maroon", "red", "tomato"), cex=.65, box.lty=0)
 
title(main="Monthly Counts of Stories Referencing Illegal/Undocumented Immigration", outer=TRUE)

