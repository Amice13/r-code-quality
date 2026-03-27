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
library(pscl)
# library(oc)
# library(wnominate)
# library(gdata)
library(foreign)
library(ellipse)
#
setwd(choose.dir())    # select directory/folder with replication files

data.stata <- read.dta("mergedAllHooverAdmin.dta")
# names(data.stata)

##############################################################################################
######################################################## IDEAL / BAYESIAN MODEL ##############


attach(data.stata,warn.conflicts = FALSE)
colnames(data.stata)						# Note the first three columns aren't votes
hr <- rollcall(data.stata[,-(1:12)], 		# Format data as rollcall object
               yea=c(1,2,3), nay=c(4,5,6), missing=c(7,8,9), notInLegis=0,
               legis.names=data.stata[,"name"], desc="71 and 72 Houses",
               vote.names=colnames(data.stata)[-(1:12)]) 
legis <- legis.names <- data.stata[,"name"]
state <- data.stata[,"state"]
party <- data.stata[,"party"]
statename <- data.stata[,"lstate"]

#  Call IDEAL - BE PATIENT THIS MAY TAKE A WHILE
#
result2 <- ideal(hr, d=2, store.item=TRUE, burnin=1000, thin=100, maxiter=11000)

ideal1dim <- result2$xbar[,1]
ideal2dim <- result2$xbar[,2]

# check sign of X2 values, may need to reverse signs
if(ideal2dim[legis.names=="BORAH, W.E."] < 0) ideal2dim <- -1*ideal2dim
# ideal1dim <- -1*result2$xbar[,1]
# ideal2dim <- -1*result2$xbar[,2]
if(ideal1dim[legis.names=="RANKIN, J. "] > 0) ideal1dim <- -1*ideal1dim




################################## start of IDEAL 2 by 2 plot

### uncomment to print graphics to file, rather than display in R window
# png(file="hi res figures/ideal2x2.png",width=6,height=7, units="in", pointsize=10, res=600)

repubcol <- "#FF000060" # transparent red
demcol <- "#0000FF60" # transparent blue
arrowwidth <- .6
arrowtips <- .05
houseplotsize <- 2.5
senateplotsize <- 1.25

par(mfrow=c(2,2),mar=c(3.1,3.1,3.1,1.1),omi=c(0.8,.1,.1,.1),family="serif")

plot(x="", y="", type="n", asp=1, main="", xlab="", ylab="",
     xlim=c(-houseplotsize,houseplotsize), ylim=c(-houseplotsize-.5,houseplotsize+.5), axes=F)
# Main title
mtext("(a) 71st House",side=3,line=1.50,font=2)
# x-axis title
mtext("",side=1,line=2.5)
# y-axis title
mtext("Dimension Two",side=2,line=2.5)  #
axis(side=1,at=seq(-houseplotsize,houseplotsize,by=1), line=0)
axis(side=2,at=seq(-houseplotsize,houseplotsize,by=1), las=2, line=0)
box()
#  Republican Senators
points(ideal1dim[party==200 & house71==71],ideal2dim[party==200 & house71==71],pch=1,col=repubcol)
#  Democrat Senators
points(ideal1dim[party==100 & house71==71],ideal2dim[party==100 & house71==71],pch=3,col=demcol)
# President Hoover
points(ideal1dim[state==99],ideal2dim[state==99],pch="H",col="red",cex=1.5)
points(ideal1dim[state==999],ideal2dim[state==99],pch="h",col="red",cex=1.5)
# add an oval around Hoover that reflects SE
# the SD hard coded below obtained from one run of Ideal
#                 Mean Std.Dev.  lower  upper
# Legislator 689  0.896    0.148  0.616  1.194 # hoover from all votes, dim 1
# Legislator 689 -0.016    0.188 -0.322  0.370  # this is also flipped
lines(ellipse(x=0,scale=c(0.148,0.188), centre=c(ideal1dim[state==99], 
              ideal2dim[state==99]),level=.95), col="red")
# hoover requests only 
# Legislator 690 -0.690    0.205 -1.098 -0.334
# Legislator 690 -0.273    0.303 -0.815  0.281
lines(ellipse(x=0,scale=c(0.205,0.303), centre=c(ideal1dim[state==999],
              ideal2dim[state==999]),level=.95), col="red",lty=2)


plot(x="",y="",type="n",asp=1, main="", xlab="", ylab="",
     xlim=c(-senateplotsize+.25,senateplotsize+.25), ylim=c(-senateplotsize,senateplotsize),
     axes=F)
# Main title
mtext("(b) 71st Senate",side=3,line=1.50,font=2)
# x-axis title
mtext("",side=1,line=2.5)
# y-axis title
mtext("",side=2,line=2.5)  
axis(side=1,at=seq(-senateplotsize-.25,senateplotsize+.25,by=.5), line=0)
axis(side=2,at=seq(-senateplotsize-.25,senateplotsize+.25,by=.5),las=2, line=0)
box()
#  Republican Senators
points(ideal1dim[party==200 & senate71==71],ideal2dim[party==200 & senate71==71], col=repubcol)
#  Democrat Senators
points(ideal1dim[party==100 & senate71==71],ideal2dim[party==100 & senate71==71],pch=3,col=demcol)
# President Hoover
points(ideal1dim[state==99],ideal2dim[state==99],pch="H",col="red",cex=1.5)
points(ideal1dim[state==999],ideal2dim[state==99],pch="h",col="red",cex=1.5)
# add an oval around Hoover that reflects SE
# the SD hard coded below obtained from one run of Ideal
#                 Mean Std.Dev.  lower  upper
# Legislator 689  0.896    0.148  0.616  1.194 # hoover from all votes, dim 1
# Legislator 689 -0.016    0.188 -0.322  0.370  # this is also flipped
lines(ellipse(x=0,scale=c(0.148,0.188), centre=c(ideal1dim[state==99],ideal2dim[state==99]),
              level=.95), col="red")
# hoover requests only 
# Legislator 690 -0.690    0.205 -1.098 -0.334
# Legislator 690 -0.273    0.303 -0.815  0.281
lines(ellipse(x=0,scale=c(0.205,0.303), centre=c(ideal1dim[state==999],ideal2dim[state==999]),
              level=.95), col="red", lty=2)



plot(x="",y="",type="n",asp=1, main="", xlab="", ylab="",
     xlim=c(-houseplotsize,houseplotsize),ylim=c(-houseplotsize-.5,houseplotsize+.5), axes=F)
# Main title
mtext("(c) 72nd House",side=3,line=1.50,font=2)
# x-axis title
mtext("Dimension One",side=1,line=2.5)
# y-axis title
mtext("Dimension Two",side=2,line=2.5)  #
axis(side=1,at=seq(-houseplotsize,houseplotsize,by=1), line=0)
axis(side=2,at=seq(-houseplotsize,houseplotsize,by=1),las=2, line=0)
box()
#  Republican Senators
points(ideal1dim[party==200 & house72==72],ideal2dim[party==200 & house72==72],pch=1,col=repubcol)
#  Democrat Senators
points(ideal1dim[party==100 & house72==72],ideal2dim[party==100 & house72==72],pch=3,col=demcol)
# President Hoover#
points(ideal1dim[state==99],ideal2dim[state==99],pch="H",col="red",cex=1.5)
points(ideal1dim[state==999],ideal2dim[state==99],pch="h",col="red",cex=1.5)
# add an oval around Hoover that reflects SE
# the SD hard coded below obtained from one run of Ideal
#                 Mean Std.Dev.  lower  upper
# Legislator 689  0.896    0.148  0.616  1.194 # hoover from all votes, dim 1
# Legislator 689 -0.016    0.188 -0.322  0.370  # this is also flipped

lines(ellipse(x=0,scale=c(0.148,0.188),
              centre=c(ideal1dim[state==99],ideal2dim[state==99]),level=.95), col="red")
# hoover requests only 
# Legislator 690 -0.690    0.205 -1.098 -0.334
# Legislator 690 -0.273    0.303 -0.815  0.281
lines(ellipse(x=0,scale=c(0.205,0.303),
              centre=c(ideal1dim[state==999],ideal2dim[state==999]),level=.95), col="red",lty=2)



plot(x="",y="",type="n",asp=1, main="", xlab="", ylab="",
     xlim=c(-senateplotsize+.25,senateplotsize+.25),
     ylim=c(-senateplotsize,senateplotsize),axes=F)
# Main title
mtext("(d) 72nd Senate",side=3,line=1.50,font=2)
# x-axis title
mtext("Dimension One",side=1,line=2.5)
# y-axis title
mtext("",side=2,line=2.5)  
axis(side=1,at=seq(-senateplotsize-.25,senateplotsize+.25,by=.5), line=0)
axis(side=2,at=seq(-senateplotsize-.25,senateplotsize+.25,by=.5),las=2, line=0)
box()
#  Republican Senators
points(ideal1dim[party==200 & senate72==72],ideal2dim[party==200 & senate72==72],pch=1,col=repubcol)
#  Democrat Senators
points(ideal1dim[party==100 & senate72==72],ideal2dim[party==100 & senate72==72],pch=3,col=demcol)
# President Hoover
points(ideal1dim[state==99],ideal2dim[state==99],pch="H",col="red",cex=1.5)
points(ideal1dim[state==999],ideal2dim[state==99],pch="h",col="red",cex=1.5)
# add an oval around Hoover that reflects SE
# the SD hard coded below obtained from one run of Ideal
#                 Mean Std.Dev.  lower  upper
# Legislator 689  0.896    0.148  0.616  1.194 # hoover from all votes, dim 1
# Legislator 689 -0.016    0.188 -0.322  0.370  # this is also flipped
lines(ellipse(x=0,scale=c(0.148,0.188),
              centre=c(ideal1dim[state==99],ideal2dim[state==99]),level=.95), col="red")
# hoover requests only 
# Legislator 690 -0.690    0.205 -1.098 -0.334
# Legislator 690 -0.273    0.303 -0.815  0.281
lines(ellipse(x=0,scale=c(0.205,0.303), centre=c(ideal1dim[state==999],ideal2dim[state==999]),
              level=.95), col="red",lty=2)
par(xpd=NA)
legend(-4.35, -2.1, c("Republicans","Democrats","Hoover","Hoover (requests only)",
                      "95% Confidence Interval","95% CI (requests only)"), 
       col = c(repubcol,demcol,"red","red","red","red"), pch=c(1,3,-1,-1,-1,-1),
       text.col = c("black","black","black","black","black","black"), lty = c(0,0,0,0,1,2), 
       lwd = c(1,1,0,0,1,1), merge = TRUE, bg = "white",cex = 1,ncol=3,pt.cex=1, box.lwd=.7)
text(-2.45,-2.27, col="red",labels="H")
text(-2.45,-2.43, col="red",labels="h")
par(xpd=FALSE)
### uncomment to print graphics to file, rather than display in R window
# dev.off()

######################### end IDEAL 2 by 2
