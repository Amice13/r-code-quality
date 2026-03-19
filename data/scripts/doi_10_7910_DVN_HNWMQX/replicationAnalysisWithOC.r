#
#  Barry Edwards
#  Estimating Herbert Hoover's Ideal Point
#  using merged 71 and 72 House and Senate roll call votes
#
#
#  Remove all objects just to be safe
#
rm(list=ls(all=TRUE))
#
# library(pscl)
library(oc)
# library(wnominate)
# library(gdata)
library(foreign)
library(ellipse)
#
setwd(choose.dir())    # select directory/folder with replication files


data.stata <- read.dta("mergedAllHooverAdmin.dta")
# names(data.stata)

attach(data.stata,warn.conflicts = FALSE)
colnames(data.stata)						# Note the first three columns aren't votes
hr <- rollcall(data.stata[,-(1:12)], 		# Format data as rollcall object
               yea=c(1,2,3), nay=c(4,5,6), missing=c(7,8,9), notInLegis=0,
	             legis.names=data.stata[,"name"], desc="71 and 72 Houses",
	             vote.names=colnames(data.stata)[-(1:12)]) 
legis.names <- data.stata[,"name"]
state <- data.stata[,"state"]
party <- data.stata[,"party"]
statename <- data.stata[,"lstate"]


###############################################################################################################
#  Call OC - BE VERY PATIENT
###############################################################################################################
#
# Example:  1-Dim
# result1 <- oc(hr, dims=1, polarity=c(2),minvotes=18)
# summary(result1)

#  Example:  2-Dim
#
result <- oc(hr, dims=2, polarity=c(689,637), minvotes=18)
# names(result$rollcalls)
# summary(result)
# windows()
# plot(result)

result999 <- ifelse(is.na(result$rollcalls),999,result$rollcalls)
nvotescaled <- sum(result999[,7]!=999)
#
ws <- result$rollcalls[, "midpoints"]
N1 <- result$rollcalls[, "normVector1D"]
N2 <- result$rollcalls[, "normVector2D"]
#
oc1 <-  result$legislators[, "coord1D"]
oc2 <-  result$legislators[, "coord2D"]

republicansOC <- oc1[party==200]
republicansOC2 <- sort(republicansOC, na.last = NA, decreasing = FALSE)
length(republicansOC2)

################### this is the start of two-by-two OC plot

windows()

### uncomment to print graphics to file, rather than display in R window
# setwd('H:/PRES ESTIMATIONS/31 Herbert Hoover/hi res figures')
# png(file="oc2x2.png",width=6,height=7, units="in", pointsize=10, res=600)

repubcol <- "#FF000060" # transparent red
demcol <- "#0000FF60" # transparent blue
arrowwidth <- .6
arrowtips <- .04

par(mfrow=c(2,2),mar=c(3.1,3.1,3.1,1.1),omi=c(0.9,.1,.1,.1),family="serif")

plot(x="",y="",type="n",asp=1, main="", xlab="", ylab="",
       xlim=c(-1.0,1.0),ylim=c(-1.0,1.0),cex=1.0,font=1,axes=F)
# Main title
mtext("(a) 71st House",side=3,line=1.50,cex=1.0,font=2)
# x-axis title
mtext("",side=1,line=2.5,cex=1.0)
# y-axis title
mtext("Race & Region",side=2,line=2.5,cex=1.0)  
axis(side=1,at=seq(-1,1,by=.5), line=0, cex.axis=1)
axis(side=2,at=seq(-1,1,by=.5),las=2, line=0, cex.axis=1)
box()
#  Republican MC
points(oc1[party==200 & house71==71],oc2[party==200 & house71==71],pch=1,col=repubcol,font=1)
#  Democrat MC
points(oc1[party==100 & house71==71],oc2[party==100 & house71==71],pch=3,col=demcol,font=1)
#  Farm-Labor MC
points(oc1[party==537 & house71 == 71],oc2[party==537 & house71 == 71],pch=2,col="gray60",cex=1)
# President Hoover
points(oc1[state == 99],oc2[state == 99],pch="H",col="red",cex=1.5)
points(oc1[state == 999],oc2[state == 999],pch="h",col="red",cex=1.5)
#

plot(x="",y="",type="n",asp=1, main="", xlab="", ylab="",
    xlim=c(-1.0,1.0),ylim=c(-1.0,1.0),cex=1.0,font=1,axes=F)
# Main title
mtext("(b) 71st Senate",side=3,line=1.50,cex=1.0,font=2)
# x-axis title
mtext("",side=1,line=2.5,cex=1.0)
# y-axis title
mtext("",side=2,line=2.5,cex=1.0)  
axis(side=1,at=seq(-1,1,by=.5), line=0, cex.axis=1)
axis(side=2,at=seq(-1,1,by=.5),las=2, line=0, cex.axis=1)
box()
#  Republican MC
points(oc1[party==200 & senate71==71],oc2[party==200 & senate71==71],pch=1,col=repubcol,font=1)
#  Democrat MC
points(oc1[party==100 & senate71==71],oc2[party==100 & senate71==71],pch=3,col=demcol,font=1)
#  Farm-Labor MC
points(oc1[party==537 & senate71== 71],oc2[party==537 & senate71== 71],pch=2,col="gray60",cex=1)
# President Hoover
points(oc1[state == 99],oc2[state == 99],pch="H",col="red",cex=1.5)
points(oc1[state == 999],oc2[state == 999],pch="h",col="red",cex=1.5)


#
plot(x="",y="",type="n",asp=1, main="", xlab="", ylab="",
       xlim=c(-1.0,1.0),ylim=c(-1.0,1.0),cex=1.0,font=1,axes=F)
# Main title
mtext("(c) 72nd House",side=3,line=1.50,cex=1.0,font=2)
# x-axis title
# x-axis title
mtext("Liberal/Conservative Ideology",side=1,line=2.5,cex=1.0)
# y-axis title
mtext("Race & Region",side=2,line=2.5,cex=1.0)  # 
axis(side=1,at=seq(-1,1,by=.5), line=0, cex.axis=1)
axis(side=2,at=seq(-1,1,by=.5),las=2, line=0, cex.axis=1)
box()
#  Republican MC
points(oc1[party==200 & house72==72],oc2[party==200 & house72==72],pch=1,col=repubcol,font=1)
#  Democrat MC
points(oc1[party==100 & house72==72],oc2[party==100 & house72==72],pch=3,col=demcol,font=1)
#  Farm-Labor MC
points(oc1[party==537 & house72==72],oc2[party==537 & house72==72],pch=2,col="gray60",cex=1)
# President Hoover
points(oc1[state == 99],oc2[state == 99],pch="H",col="red",cex=1.5)
points(oc1[state == 999],oc2[state == 999],pch="h",col="red",cex=1.5)


plot(x="",y="",type="n",asp=1, main="", xlab="", ylab="",
    xlim=c(-1.0,1.0),ylim=c(-1.0,1.0),cex=1.0,font=1,axes=F)
# Main title
mtext("(d) 72nd Senate",side=3,line=1.50,cex=1.0,font=2)
# x-axis title
mtext("Liberal/Conservative Ideology",side=1,line=2.5,cex=1.0)
# y-axis title
mtext("",side=2,line=2.5,cex=1.0)  # 
axis(side=1,at=seq(-1,1,by=.5), line=0, cex.axis=1)
axis(side=2,at=seq(-1,1,by=.5),las=2, line=0, cex.axis=1)
box()
#  Republican MC
points(oc1[party==200 & senate72==72],oc2[party==200 & senate72==72],pch=1,col=repubcol,font=1)
#  Democrat MC
points(oc1[party==100 & senate72==72],oc2[party==100 & senate72==72],pch=3,col=demcol,font=1)
#  Farm-Labor MC
points(oc1[party==537 & senate72==72],oc2[party==537 & senate72==72],pch=2,col="gray60",cex=1)
# President Hoover
points(oc1[state == 99],oc2[state == 99],pch="H",col="red",cex=1.5)
points(oc1[state == 999],oc2[state == 999],pch="h",col="red",cex=1.5)

par(xpd=NA)
legend(-2.925, -1.75, c("Republicans","Democrats","Farm-Labor","Hoover",
                        "Hoover (requests only)",""), 
       col = c(repubcol,demcol,"gray60","red","red","white"),
       pch=c(1,3,2,-1,-1,-1),
       text.col = c("black","black","black","black","black"), 
       lty = c(0,0,0,0,0,0), lwd = c(1,1,1,0,0,0),
       merge = TRUE, bg = "white",cex = 1,ncol=2,pt.cex=1, box.lwd=.7)
text(-1.30,-1.88,col="red",labels="H")
text(-1.30,-2.02,col="red",labels="h")
par(xpd=FALSE)

### uncomment to print graphics to file, rather than display in R window
# dev.off()

################ end of oc 2 by 2 plot




