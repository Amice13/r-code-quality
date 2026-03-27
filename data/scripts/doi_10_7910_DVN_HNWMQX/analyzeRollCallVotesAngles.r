#  Barry Edwards
#  Analyze the cutting line angles and cutpoints
#
#  Remove all objects just to be safe
#
rm(list=ls(all=TRUE))
#
setwd(choose.dir())    # select directory/folder with replication files
rollCallVotes <- read.csv("rollcallvotedata.csv")
# names(rollCallVotes)
attach(rollCallVotes)
# fix(rollCallVotes)
 
# Figure 3
############## compare the cutting line angles of votes used in House and Senate


windows()
# png(file="hi res figures/cuttingLineAngles.png",width=6,height=2.5, units="in", pointsize=10, res=600)

par(mfrow=c(1,2), mar=c(3.1,3.1,3.1,1.1), omi=c(0.1,.1,.1,.1), family="serif")

positiveAngles <- rollCallVotes$pos_angle
cqSwiftLineColor <- "gray40"
myVotesLineColor <- "gray40"
 nAllVotes <- NROW(na.omit(positiveAngles[chamber == "H"]))
 nRequests <- NROW(na.omit(positiveAngles[HOOVER.REQUESTS!=9 & HOOVER.REQUESTS!=0 & chamber == "H"]))
nPresVotes <- NROW(na.omit(positiveAngles[HOOVER..H.!=9 & HOOVER..H.!=0 & chamber == "H"]))
requestShare <- 1 # (nRequests/nAllVotes) --- not scaling to size
presVoteShare <- 1 # (nPresVotes/nAllVotes) --- not scaling to size
angleDensityAllVotes <-  density(na.omit(positiveAngles[chamber == "H"]))
angleDensityRequests <-  density(na.omit(positiveAngles[HOOVER.REQUESTS!=9 & HOOVER.REQUESTS!=0 & chamber == "H"]))
angleDensityPresVotes <- density(na.omit(positiveAngles[HOOVER..H.!=9 & HOOVER..H.!=0 & chamber == "H"]))
ymax <- max(angleDensityAllVotes$y, angleDensityRequests$y, angleDensityPresVotes$y)
ymax <- 0.02161231

plot(x="", y="", type="n", main="", xlab="", ylab="", xlim=c(0,180), ylim=c(0,ymax), axes=F)
# Main title
mtext("(a) 71st & 72nd Houses", side=3, line=1.50, font=2)
# x-axis title
mtext("Cutting Line Angles", side=1, line=2.5)
# y-axis title
mtext("Density", side=2, line=3)  # 
axis(side=1, at=seq(0,180,by=30), line=0) # x axis
axis(side=2, at=seq(0,ymax,by=.005), las=2, line=0) # y axis
box()
lines(angleDensityAllVotes$x, angleDensityAllVotes$y, col="black")
lines(angleDensityAllVotes$x, presVoteShare*angleDensityPresVotes$y, col=myVotesLineColor)
lines(angleDensityAllVotes$x, requestShare*angleDensityRequests$y, col=cqSwiftLineColor, lty=3)
legend(0, ymax, c("All Roll Call Votes", "Hoover Requests", "All Hoover Votes"), 
       col = c("black",cqSwiftLineColor,myVotesLineColor),
       lty = c(1,3,1), lwd = c(1,1,1),
       merge = TRUE, bg = "white",cex = .6, box.lwd=.7)


nAllVotes <- NROW(na.omit(positiveAngles[chamber == "S"]))
nRequests <- NROW(na.omit(positiveAngles[HOOVER.REQUESTS!=9 & HOOVER.REQUESTS!=0 & chamber == "S"]))
nPresVotes <- NROW(na.omit(positiveAngles[HOOVER..H.!=9 & HOOVER..H.!=0 & chamber == "S"]))
requestShare <- 1 #(nRequests/nAllVotes) --- not scaling to size
presVoteShare <- 1 # (nPresVotes/nAllVotes) --- not scaling to size
angleDensityAllVotes <-  density(na.omit(positiveAngles[chamber == "S"]))
angleDensityRequests <-  density(na.omit(positiveAngles[HOOVER.REQUESTS!=9 & HOOVER.REQUESTS!=0 & chamber == "S"]))
angleDensityPresVotes <- density(na.omit(positiveAngles[HOOVER..H.!=9 & HOOVER..H.!=0 & chamber == "S"]))
ymax <- max(angleDensityAllVotes$yangleDensityRequests$y, angleDensityPresVotes$y)

plot(x="", y="", type="n", main="", xlab="", ylab="", xlim=c(0,180), ylim=c(0,ymax), axes=F)
# Main title
mtext("(b) 71st & 72nd Senates",side=3,line=1.50,font=2)
# x-axis title
mtext("Cutting Line Angles",side=1,line=2.5)
# y-axis title
mtext("",side=2,line=2.5)  # 
axis(side=1,at=seq(0,180,by=30), line=0) # x axis
axis(side=2,at=seq(0,ymax,by=.005),las=2, line=0) # y axis
box()
lines(angleDensityAllVotes$x, angleDensityAllVotes$y, col="black")
lines(angleDensityAllVotes$x, presVoteShare*angleDensityPresVotes$y, col=myVotesLineColor)
lines(angleDensityAllVotes$x, requestShare*angleDensityRequests$y, col=cqSwiftLineColor, lty=3)

# dev.off()






