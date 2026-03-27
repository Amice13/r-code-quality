#
#  Barry Edwards
#  Estimating Herbert Hoover's Ideal Point
#  using merged 71 and 72 House and Senate roll call votes
#
#
#  Remove all objects just to be safe
#
rm(list=ls(all=TRUE))
library(wnominate)
library(foreign)
library(ellipse)

setwd(choose.dir())    # select directory/folder with replication files


############################################################
#
#  Load data

data.stata <- read.dta("mergedAllHooverAdmin.dta")
attach(data.stata,warn.conflicts = FALSE)
# colnames(data.stata)						# Note the first three columns aren't votes
hr <- rollcall(data.stata[,-(1:12)], 		# Format data as rollcall object
               yea=c(1,2,3), nay=c(4,5,6), missing=c(7,8,9), notInLegis=0,
	             legis.names=data.stata[,"name"], desc="71 and 72 Houses",
	             vote.names=colnames(data.stata)[-(1:12)]) 

legis.names <- data.stata[, "name"]
state <- data.stata[, "state"]
party <- data.stata[, "party"]
# fix(data.stata)
statename <- data.stata[, "lstate"]


############################################################
#
#  Run W-NOMINATE, be patient
#
resultw <- wnominate(hr, dims=2, minvotes=18, polarity=c(1,2))
summary(resultw)
#   
# resultw$legislators
# resultw$rollcalls
# resultw$dimensions
# resultw$eigenvalues
# resultw$beta
# resultw$weights
# resultw$fits
#
# legis.names[689]
# legis.names[637]

WEIGHT <- (resultw$weights[2])/(resultw$weights[1])
X1     <-  resultw$legislators$coord1D
X2     <- (resultw$legislators$coord2D)*WEIGHT
# party <- data.stata[,6]
# state <- data.stata[,3]

# check sign of X2 values, may need to reverse signs
if(X2[legis.names=="BORAH, W.E."] < 0) X2 <- X2*-1

######################### start two-by-two nominate plot
#

windows()

### uncomment to print graphics to file, rather than display in R window
# png(file="hi res figures/nominate2x2.png",width=6,height=7, units="in", pointsize=10, res=600)

repubcol <- "#FF000060" # transparent red
demcol   <- "#0000FF60" # transparent blue
arrowwidth <- .6
arrowtips  <- .04

par(mfrow=c(2,2), mar=c(3.1,3.1,3.1,1.1), omi=c(0.9,.1,.1,.1), family="serif")

plot(x="", y="", type="n", asp=1, main="", xlab="", ylab="",
       xlim=c(-1.0,1.0), ylim=c(-1.0,1.0), axes=F)
# Main title
mtext("(a) 71st House", side=3, line=1.00, font=2)
# x-axis title
mtext("", side=1, line=3.25)
# y-axis title
mtext("Race & Region", side=2, line=2.5)  
axis(side=1, at=seq(-1,1,by=.5), line=0)
axis(side=2, at=seq(-1,1,by=.5), las=2, line=0)
box()
#  Republican Reps
points(X1[party==200 & house71 == 71], X2[party==200 & house71 == 71], pch=1,
       col=repubcol, cex=1)
#  Democrat Reps
points(X1[party==100 & house71 == 71], X2[party==100 & house71 == 71], pch=3,
       col=demcol, cex=1)
#  Farm-Labor Reps
points(X1[party==537 & house71 == 71], X2[party==537 & house71 == 71], pch=2, col="gray60", cex=1)

# Label Progressive Reps, this is done one at a time to adjust text positioning
myid <- 4624
text(X1[id==myid],1,label="Howard",cex=.8,font=1)
arrows(x0=X1[id==myid], .95, x1=X1[id==myid],y1=X2[id==myid],length=arrowtips,lwd=arrowwidth)
myid <- 7250
text(X1[id==myid]-.05,.9,label="Patterson",cex=.8,font=1)
arrows(x0=X1[id==myid],.85, x1=X1[id==myid],y1=X2[id==myid],length=arrowtips,lwd=arrowwidth)
myid <- 7731
text(X1[id==myid]-.05,.8,label="Rankin",cex=.8,font=1)
arrows(x0=X1[id==myid],.75, x1=X1[id==myid],y1=X2[id==myid],length=arrowtips,lwd=arrowwidth)
myid <- 7218
text(X1[id==myid]+.05,.9,label="Parsons",cex=.8,font=1)
arrows(x0=X1[id==myid], .85, x1=X1[id==myid],y1=X2[id==myid],length=arrowtips,lwd=arrowwidth)
myid <- 309
text(X1[id==myid],.8,label="Ayres",cex=.8,font=1)
arrows(x0=X1[id==myid], .75, x1=X1[id==myid],y1=X2[id==myid],length=arrowtips,lwd=arrowwidth)
myid <- 1446
text(X1[id==myid]+.05,.7,label="Campbell",cex=.8,font=1)
arrows(x0=X1[id==myid], .65, x1=X1[id==myid],y1=X2[id==myid],length=arrowtips,lwd=arrowwidth)
myid <- 8357
text(X1[id==myid]+.025,.6,label="Selvig",cex=.8,font=1)
arrows(x0=X1[id==myid], .55, x1=X1[id==myid],y1=X2[id==myid],length=arrowtips,lwd=arrowwidth)
myid <- 1479
text(X1[id==myid],-.7,label="Cannon",cex=.8,font=1)
arrows(x0=X1[id==myid], -.65, x1=X1[id==myid],y1=X2[id==myid],length=arrowtips,lwd=arrowwidth)
myid <- 2199
text(X1[id==myid],-1,label="Crosser",cex=.8,font=1)
arrows(x0=X1[id==myid], -.95, x1=X1[id==myid],y1=X2[id==myid],length=arrowtips,lwd=arrowwidth)
myid <- 4685
text(X1[id==myid]-.18,-.8,label="Huddleston",cex=.75,font=1)
arrows(x0=X1[id==myid]-.18, -.75, x1=X1[id==myid],y1=X2[id==myid],length=arrowtips,lwd=arrowwidth)
myid <- 8269
text(X1[id==myid]-.2,-.9,label="Schneider",cex=.8,font=1)
arrows(x0=X1[id==myid]-.15, -.85, x1=X1[id==myid],y1=X2[id==myid],length=arrowtips,lwd=arrowwidth)
myid <- 5406
text(X1[id==myid]+.2,-.9,label="La Guardia",cex=.8,font=1)
arrows(x0=X1[id==myid]+.2, -.85, x1=X1[id==myid],y1=X2[id==myid],length=arrowtips,lwd=arrowwidth)
myid <- 8538
text(X1[id==myid]+.3,-.8,label="Sinclair",cex=.8,font=1)
arrows(x0=X1[id==myid]+.25, -.75, x1=X1[id==myid],y1=X2[id==myid],length=arrowtips,lwd=arrowwidth)
myid <- 9130
text(X1[id==myid]+.2,-.7,label="Swing",cex=.8,font=1)
arrows(x0=X1[id==myid]+.2, -.65, x1=X1[id==myid],y1=X2[id==myid],length=arrowtips,lwd=arrowwidth)
#
# President Hoover
points(X1[state == 99],  X2[state == 99],  pch="H", col="red", cex=1.2)
points(X1[state == 999], X2[state == 999], pch="h", col="red", cex=1.2)
     lines(ellipse(x=0.01599771, scale=c(0.1468922,0.1264356),
            centre=c(X1[state == 99],X2[state == 99])), col="red")
     lines(ellipse(x=0.01770542, scale=c(0.1883463,0.1549762),
            centre=c(X1[state == 999],X2[state == 999])), col="red",lty=2)

		
#
plot(x="", y="", type="n", asp=1, main="", xlab="", ylab="",
       xlim=c(-1.0,1.0), ylim=c(-1.0,1.0), axes=F)
# Main title
mtext("(b) 71st Senate",side=3,line=1.00,cex=1.0,font=2)
# x-axis title
mtext("",side=1,line=3.25,cex=1.0)
# y-axis title
mtext("",side=2,line=2.5,cex=1.0)  
axis(side=1, at=seq(-1,1,by=.5), line=0)
axis(side=2, at=seq(-1,1,by=.5), las=2, line=0)
box()
#  Republican Senators
points(X1[party==200 & senate71 == 71],X2[party==200 & senate71 == 71],pch=1,col=repubcol,cex=1)
#  Democrat Senators
points(X1[party==100 & senate71 == 71],X2[party==100 & senate71 == 71],pch=3,col=demcol,cex=1)
#  Farm-Labor Senators
points(X1[party==537 & senate71 == 71],X2[party==537 & senate71 == 71],pch=2,col="gray60",cex=1)
#
# Label Progressive Senators
text(X1[id==8254],+.85,label="Schall",cex=.8, col="black") # 
arrows(x0=X1[id==8254], +.80, x1=X1[id==8254],y1=X2[id==8254],length=arrowtips,lwd=arrowwidth)
text(X1[id==858],+.95,label="Borah",cex=.8,font=1)
arrows(x0=X1[id==858], +.90, x1=X1[id==858],y1=X2[id==858],length=arrowtips,lwd=arrowwidth)
text(-.85,X2[id==3347]+.2,label="Frazier",col="black", pos=2,cex=.8,offset=.1)
arrows(-.85, X2[id==3347]+.2, x1=X1[id==3347],y1=X2[id==3347],length=arrowtips,lwd=arrowwidth)
text(-.98,X2[id==6991]+.1,label="Nye",col="black", pos=2,cex=.8,offset=.1)
arrows(-.98, X2[id==6991]+.1, x1=X1[id==6991],y1=X2[id==6991],length=arrowtips,lwd=arrowwidth)
text(-.75,X2[id==1070]+.025,label="Brookhart",col="black", pos=2,cex=.8,offset=.1)
arrows(-.75, X2[id==1070]+.025, x1=X1[id==1070],y1=X2[id==1070],length=arrowtips,lwd=arrowwidth)
text(-.85,X2[id==2289]+.175,label="Cutting",col="black", pos=2,cex=.8,offset=.1)
arrows(-.85, X2[id==2289]+.175, x1=X1[id==2289],y1=X2[id==2289],length=arrowtips,lwd=arrowwidth)
text(-.77,X2[id==8476]+.1,label="Shipstead",col="black", pos=2,cex=.8,offset=.1)
arrows(-.77, X2[id==8476]+.1, x1=X1[id==8476],y1=X2[id==8476],length=arrowtips,lwd=arrowwidth)
text(-.89,X2[id==6960]+.05,label="Norris",col="black", pos=2,cex=.8,offset=.1)
arrows(-.89, X2[id==6960]+.05, x1=X1[id==6960],y1=X2[id==6960],length=arrowtips,lwd=arrowwidth)
text(-.825,X2[id==9303],label="Thomas", col="black", pos=2,cex=.8,offset=.1) #  
arrows(-.825, X2[id==9303], x1=X1[id==9303],y1=X2[id==9303],length=arrowtips,lwd=arrowwidth)
text(-.85,X2[id==6251]-.075,label="McGill", col="black", pos=2,cex=.8,offset=.1) # 
arrows(-.85, X2[id==6251]-.075, x1=X1[id==6251],y1=X2[id==6251],length=arrowtips,lwd=arrowwidth)
text(-.80,X2[id==9984]-.1,label="Wheeler",col="black", pos=2,cex=.8,offset=.1)
arrows(-.80, X2[id==9984]-.1, x1=X1[id==9984],y1=X2[id==9984],length=arrowtips,lwd=arrowwidth)
text(-.9,X2[id==761]-.05,label="Blaine", col="black", pos=2,cex=.8,offset=.1) #  
arrows(-.9, X2[id==761]-.05, x1=X1[id==761],y1=X2[id==761],length=arrowtips,lwd=arrowwidth)
text(-.720,X2[id==5402]-.2,label="LaFollette",col="black", pos=2,cex=.8,offset=.1)
arrows(-.720, X2[id==5402]-.2, x1=X1[id==5402],y1=X2[id==5402],length=arrowtips,lwd=arrowwidth)
text(-.620,X2[id==1224],label="Bulkley", col="black", pos=2,cex=.8,offset=.1) # 
arrows(-.620, X2[id==1224], x1=X1[id==1224],y1=X2[id==1224],length=arrowtips,lwd=arrowwidth)
text(-.620,X2[id==9782],label="Walsh", col="black", pos=2,cex=.8,offset=.1) # 
arrows(-.620, X2[id==9782], x1=X1[id==9782],y1=X2[id==9782],length=arrowtips,lwd=arrowwidth)
# President Hoover
#
points(X1[state == 99],X2[state == 99],pch="H",col="red",cex=1.2)
points(X1[state == 999],X2[state == 999],pch="h",col="red",cex=1.2)
     lines(ellipse(x=0.01599771,scale=c(0.1468922,0.1264356),
     centre=c(X1[state == 99],X2[state == 99])), col="red")
     lines(ellipse(x=0.01770542,scale=c(0.1883463,0.1549762),
     centre=c(X1[state == 999],X2[state == 999])), col="red",lty=2)

#
plot(x="",y="",type="n",asp=1, main="", xlab="", ylab="",
       xlim=c(-1.0,1.0),ylim=c(-1.0,1.0),cex=1.0,font=1,axes=F)
# Main title
mtext("(c) 72nd House",side=3,line=1.50,cex=1.0,font=2)
# x-axis title
mtext("Liberal/Conservative Ideology",side=1,line=2.5,cex=1.0)
# y-axis title
mtext("Race & Region",side=2,line=2.5,cex=1.0)  # 
axis(side=1,at=seq(-1,1,by=.5), line=0)
axis(side=2,at=seq(-1,1,by=.5),las=2, line=0)
box()
#  Republican Reps
points(X1[party==200 & house72 == 72],X2[party==200 & house72 == 72],pch=1,col=repubcol,cex=1)
#  Democrat Reps
points(X1[party==100 & house72 == 72],X2[party==100 & house72 == 72],pch=3,col=demcol,cex=1)
#  Farm-Labor Reps
points(X1[party==537 & house72 == 72],X2[party==537 & house72 == 72],pch=2,col="gray60",cex=1)
#
# Label Progressive Reps
text(X1[id==4624],1,label="Howard",cex=.8,font=1)
arrows(x0=X1[id==4624], .95, x1=X1[id==4624],y1=X2[id==4624],length=arrowtips,lwd=arrowwidth)
myid <- 7250
text(X1[id==myid]-.05,.9,label="Patterson",cex=.8,font=1)
arrows(x0=X1[id==myid],.85, x1=X1[id==myid],y1=X2[id==myid],length=arrowtips,lwd=arrowwidth)
myid <- 7731
text(X1[id==myid]-.05,.8,label="Rankin",cex=.8,font=1)
arrows(x0=X1[id==myid],.75, x1=X1[id==myid],y1=X2[id==myid],length=arrowtips,lwd=arrowwidth)
myid <- 7218
text(X1[id==myid]+.05,.9,label="Parsons",cex=.8,font=1)
arrows(x0=X1[id==myid], .85, x1=X1[id==myid],y1=X2[id==myid],length=arrowtips,lwd=arrowwidth)
myid <- 309
text(X1[id==myid],.8,label="Ayres",cex=.8,font=1)
arrows(x0=X1[id==myid], .75, x1=X1[id==myid],y1=X2[id==myid],length=arrowtips,lwd=arrowwidth)
myid <- 1446
text(X1[id==myid]+.05,.7,label="Campbell",cex=.8,font=1)
arrows(x0=X1[id==myid], .65, x1=X1[id==myid],y1=X2[id==myid],length=arrowtips,lwd=arrowwidth)
myid <- 8357
text(X1[id==myid]+.025,.6,label="Selvig",cex=.8,font=1)
arrows(x0=X1[id==myid], .55, x1=X1[id==myid],y1=X2[id==myid],length=arrowtips,lwd=arrowwidth)
myid <- 1479
text(X1[id==myid],-.7,label="Cannon",cex=.8,font=1)
arrows(x0=X1[id==myid], -.65, x1=X1[id==myid],y1=X2[id==myid],length=arrowtips,lwd=arrowwidth)
myid <- 2199
text(X1[id==myid],-1,label="Crosser",cex=.8,font=1)
arrows(x0=X1[id==myid], -.95, x1=X1[id==myid],y1=X2[id==myid],length=arrowtips,lwd=arrowwidth)
myid <- 4685
text(X1[id==myid]-.3,-.2,label="Lewis",cex=.8,font=1)   # 72 House only
arrows(x0=X1[id==myid]-.3, -.15, x1=X1[id==myid],y1=X2[id==myid],length=arrowtips,lwd=arrowwidth)
myid <- 4685
text(X1[id==myid]-.18,-.8,label="Huddleston",cex=.75,font=1)
arrows(x0=X1[id==myid]-.18, -.75, x1=X1[id==myid],y1=X2[id==myid],length=arrowtips,lwd=arrowwidth)
myid <- 8269
text(X1[id==myid]-.2,-.9,label="Schneider",cex=.8,font=1)
arrows(x0=X1[id==myid]-.15, -.85, x1=X1[id==myid],y1=X2[id==myid],length=arrowtips,lwd=arrowwidth)
text(X1[id==5406]+.2,-.9,label="La Guardia",cex=.8,font=1)
arrows(x0=X1[id==5406]+.2, -.85, x1=X1[id==5406],y1=X2[id==5406],length=arrowtips,lwd=arrowwidth)
myid <- 8538
text(X1[id==myid]+.3,-.8,label="Sinclair",cex=.8,font=1)
arrows(x0=X1[id==myid]+.25, -.75, x1=X1[id==myid],y1=X2[id==myid],length=arrowtips,lwd=arrowwidth)
myid <- 9130
text(X1[id==myid]+.2,-.7,label="Swing",cex=.8,font=1)
arrows(x0=X1[id==myid]+.2, -.65, x1=X1[id==myid],y1=X2[id==myid],length=arrowtips,lwd=arrowwidth)
# President Hoover
#
points(X1[state == 99],X2[state == 99],pch="H",col="red",cex=1.2)
points(X1[state == 999],X2[state == 999],pch="h",col="red",cex=1.2)
     lines(ellipse(x=0.01599771,scale=c(0.1468922,0.1264356),
     centre=c(X1[state == 99],X2[state == 99])), col="red")
     lines(ellipse(x=0.01770542,scale=c(0.1883463,0.1549762),
     centre=c(X1[state == 999],X2[state == 999])), col="red",lty=2)

		
plot(x="",y="",type="n",asp=1, main="", xlab="", ylab="",
       xlim=c(-1.0,1.0),ylim=c(-1.0,1.0),cex=1.0,font=1,axes=F)
# Main title
mtext("(d) 72nd Senate",side=3,line=1.50,cex=1.0,font=2)
# x-axis title
# x-axis title
mtext("Liberal/Conservative Ideology",side=1,line=2.5,cex=1.0)
# y-axis title
mtext("",side=2,line=2.5,cex=1.0)  # 
axis(side=1,at=seq(-1,1,by=.5), line=0)
axis(side=2,at=seq(-1,1,by=.5),las=2, line=0)
box()
#  Republican Senators
points(X1[party==200 & senate72 == 72],X2[party==200 & senate72 == 72],pch=1,col=repubcol,cex=1)
#  Democrat Senators
points(X1[party==100 & senate72 == 72],X2[party==100 & senate72 == 72],pch=3,col=demcol,cex=1)
#  Farm-Labor Senators
points(X1[party==537 & senate72 == 72],X2[party==537 & senate72 == 72],pch=2,col="gray60",cex=1)
#
# Label Progressive Senators
text(X1[id==8254],+.85,label="Schall",cex=.8, col="black") # 
arrows(x0=X1[id==8254], +.80, x1=X1[id==8254],y1=X2[id==8254],length=arrowtips,lwd=arrowwidth)
text(X1[id==858],+.95,label="Borah",cex=.8,font=1)
arrows(x0=X1[id==858], +.90, x1=X1[id==858],y1=X2[id==858],length=arrowtips,lwd=arrowwidth)
text(-.85,X2[id==3347]+.2,label="Frazier",col="black", pos=2,cex=.8,offset=.1)
arrows(-.85, X2[id==3347]+.2, x1=X1[id==3347],y1=X2[id==3347],length=arrowtips,lwd=arrowwidth)
text(-.98,X2[id==6991]+.1,label="Nye",col="black", pos=2,cex=.8,offset=.1)
arrows(-.98, X2[id==6991]+.1, x1=X1[id==6991],y1=X2[id==6991],length=arrowtips,lwd=arrowwidth)
text(-.75,X2[id==1070]+.025,label="Brookhart",col="black", pos=2,cex=.8,offset=.1)
arrows(-.75, X2[id==1070]+.025, x1=X1[id==1070],y1=X2[id==1070],length=arrowtips,lwd=arrowwidth)
text(-.85,X2[id==2289]+.175,label="Cutting",col="black", pos=2,cex=.8,offset=.1)
arrows(-.85, X2[id==2289]+.175, x1=X1[id==2289],y1=X2[id==2289],length=arrowtips,lwd=arrowwidth)
text(-.77,X2[id==8476]+.1,label="Shipstead",col="black", pos=2,cex=.8,offset=.1)
arrows(-.77, X2[id==8476]+.1, x1=X1[id==8476],y1=X2[id==8476],length=arrowtips,lwd=arrowwidth)
text(-.89,X2[id==6960]+.05,label="Norris",col="black", pos=2,cex=.8,offset=.1)
arrows(-.89, X2[id==6960]+.05, x1=X1[id==6960],y1=X2[id==6960],length=arrowtips,lwd=arrowwidth)
text(-.8,X2[id==2083],label="Costigan",col="black", pos=2,cex=.8,offset=.1) # only in 72nd senate
arrows(-.8, X2[id==2083], x1=X1[id==2083],y1=X2[id==2083],length=arrowtips,lwd=arrowwidth)
text(-.825,X2[id==9303],label="Thomas", col="black", pos=2,cex=.8,offset=.1) #  
arrows(-.825, X2[id==9303], x1=X1[id==9303],y1=X2[id==9303],length=arrowtips,lwd=arrowwidth)
text(-.85,X2[id==6251]-.075,label="McGill", col="black", pos=2,cex=.8,offset=.1) # 
arrows(-.85, X2[id==6251]-.075, x1=X1[id==6251],y1=X2[id==6251],length=arrowtips,lwd=arrowwidth)
text(-.80,X2[id==9984]-.1,label="Wheeler",col="black", pos=2,cex=.8,offset=.1)
arrows(-.80, X2[id==9984]-.1, x1=X1[id==9984],y1=X2[id==9984],length=arrowtips,lwd=arrowwidth)
text(-.85,X2[id==761]-.05,label="Blaine", col="black", pos=2,cex=.8,offset=.1) #  
arrows(-.85, X2[id==761]-.05, x1=X1[id==761],y1=X2[id==761],length=arrowtips,lwd=arrowwidth)
text(-.720,X2[id==5402]-.2,label="LaFollette",col="black", pos=2,cex=.8,offset=.1)
arrows(-.720, X2[id==5402]-.2, x1=X1[id==5402],y1=X2[id==5402],length=arrowtips,lwd=arrowwidth)
text(-.620,X2[id==1224],label="Bulkley", col="black", pos=2,cex=.8,offset=.1) # 
arrows(-.620, X2[id==1224], x1=X1[id==1224],y1=X2[id==1224],length=arrowtips,lwd=arrowwidth)
text(-.620,X2[id==9782],label="Walsh", col="black", pos=2,cex=.8,offset=.1) # 
arrows(-.620, X2[id==9782], x1=X1[id==9782],y1=X2[id==9782],length=arrowtips,lwd=arrowwidth)
# President Hoover
points(X1[state == 99],X2[state == 99],pch="H",col="red",cex=1.2)
points(X1[state == 999],X2[state == 999],pch="h",col="red",cex=1.2)
     lines(ellipse(x=0.01599771,scale=c(0.1468922,0.1264356),
     centre=c(X1[state == 99],X2[state == 99])), col="red")
     lines(ellipse(x=0.01770542,scale=c(0.1883463,0.1549762),
     centre=c(X1[state == 999],X2[state == 999])), col="red",lty=2)
		
par(xpd=NA)
legend(-3.675, -1.75, c("Republicans","Democrats","Farm-Labor","Hoover","Hoover (requests only)","",
                        "95% Confidence Interval","95% CI (requests only)",""), 
       col = c(repubcol,demcol,"gray60","red","red","white","red","red","white"),
       pch=c(1,3,2,-1,-1,-1,-1,-1,-1),
       text.col = c("black","black","black","black","black","black","black","black"), 
       lty = c(0,0,0,0,0,0,1,2,0), lwd = c(1,1,1,0,0,0,1,1,1),
       merge = TRUE, bg = "white",cex = 1,ncol=3,pt.cex=1, box.lwd=.7)
text(-2.05,-1.88,col="red",labels="H")
text(-2.05,-2.02,col="red",labels="h")
par(xpd=FALSE)


### uncomment to print graphics to file, rather than display in R window
# dev.off()
################################################ end of two-by-two plot




################# start two-by-one plot comparing Hoover on first dimension

windows()

### uncomment to print graphics to file, rather than display in R window
# png(file="hi res figures/comparingToMeans.png",width=6,height=3, units="in", pointsize=10, res=600)

par(mfrow=c(1,2),mar=c(3.1,2.8,2.1,1.1),omi=c(0.1,.1,.1,.1),family="serif")

arrowwidth <- .6
arrowtips <- .05
redFade <- "#FF000015"
hoover1dimSD <- 0.1468922 

progRepsX1s <- c(X1[id==4624],X1[id==7250],X1[id==7731],X1[id==7218],X1[id==309],
                 X1[id==1446],X1[id==8357],X1[id==1479],X1[id==2199],X1[id==4685],
                 X1[id==4685],X1[id==8269],X1[id==5406],X1[id==8538],X1[id==9130])
progRepsDensity <- density(progRepsX1s)
repubDensity <- density(na.omit(X1[party==200 & state!=99 & state!=999 & (house71 == 71 | house72 == 72)]))
demDensity <- density(na.omit(X1[party==100 & (house71 == 71 | house72 == 72)]))
repMean <- mean(na.omit(X1[party==200 & state!=99 & state!=999 & (house71 == 71 | house72 == 72)]))
demMean <- mean(na.omit(X1[party==100 & (house71 == 71 | house72 == 72)]))
repSD <- sd(na.omit(X1[party==200 & state!=99 & state!=999  & (house71 == 71 | house72 == 72)]))
demSD <- sd(na.omit(X1[party==100 & (house71 == 71 | house72 == 72)]))
hooverScore <- X1[state == 99]

plot(x="",y="",ylim=c(0,3.2),xlim=c(-1.2,1.2),axes=F)
# Main title
mtext("(a) House of Representatives",side=3,line=1.25,cex=1.0,font=2)
# x-axis title
mtext("Ideal Points (Dimension One)",side=1,line=2.5,cex=1.0)
# y-axis title
mtext("Density",side=2,line=2.5,cex=1.0)  
axis(side=1,at=seq(-1.0,1.0,by=.5), line=0)       # x axis
axis(side=2,at=seq(0,3.0,by=.5),las=2, line=0) # y axis
lines(progRepsDensity$x,progRepsDensity$y,col="black",lwd=1)
segments(repMean,-1,repMean,1.55,col="red",lwd=.5)
lines(repubDensity$x,repubDensity$y,col="red",lwd=1)
repMedianHouse <- median(na.omit(X1[party==200 & state!=99 & state!=999 & (house71 == 71 | house72 == 72)]))
segments(repMedianHouse ,-1,repMedianHouse ,1.55,col="red",lwd=.5,lty=2)
box()
# label arrow pointing to Hoover
hooverY <- 2
text(X1[state == 99],y=hooverY,labels="H",col="black",cex=.8)
arrows(X1[state == 99],hooverY-.1,X1[state == 99],.9,length=arrowtips,lwd=arrowwidth)
legend(-1.2, 3.2, c("Republicans","Progressives (R or D)","Republican Mean",
                    "Republican Median"), col = c("red","black","red","red"),
       text.col = c("black","black","black","black"), lty = c(1,1,1,2), lwd = c(1,1,.5,.5),
       merge = TRUE, bg = "white",cex = 0.7, box.lwd=.7)


## now comparing to Senate
progressiveSenatorsX1s <- c(X1[id==8254],X1[id==858],X1[id==3347],X1[id==6991], X1[id==1070],
                            X1[id==2289],X1[id==8476],X1[id==6960],X1[id==2083],X1[id==9303],
                            X1[id==6251],X1[id==9984],X1[id==761],X1[id==5402],X1[id==1224],
                            X1[id==9782])
progSenDensity <- density(progressiveSenatorsX1s)
repubDensity <- density(na.omit(X1[party==200  & state!=99 & state!=999 & (senate71 == 71 | senate72 == 72)]))
demDensity <- density(na.omit(X1[party==100 & (senate71 == 71 | senate72 == 72)]))
repMean <- mean(na.omit(X1[party==200  & state!=99 & state!=999 & (senate71 == 71 | senate72 == 72)]))
demMean <- mean(na.omit(X1[party==100 & (senate71 == 71 | senate72 == 72)]))
repSD <- sd(na.omit(X1[party==200 & state!=99 & state!=999 & (senate71 == 71 | senate72 == 72)]))
demSD <- sd(na.omit(X1[party==100 & (senate71 == 71 | senate72 == 72)]))
hooverScore <- X1[state == 99]

plot(x="",y="",ylim=c(0,3.2),xlim=c(-1.2,1.2),axes=F)
# Main title
mtext("(b) Senate",side=3,line=1.25,cex=1.0,font=2)
# x-axis title
mtext("Ideal Points (Dimension One)",side=1,line=2.5,cex=1.0)
# y-axis title
mtext("",side=2,line=2.5,cex=1.0)  
axis(side=1,at=seq(-1.0,1.0,by=.5), line=0)       # x axis
axis(side=2,at=seq(0,3.0,by=.5),las=2, line=0) # y axis
#
lines(progSenDensity$x,progSenDensity$y,col="black")
segments(repMean,-1,repMean,0.88,col="red",lwd=.5)
lines(repubDensity$x,repubDensity$y,col="red",lwd=1)
repMedianSenate <- median(na.omit(X1[party==200 & state!=99 & state!=999 & (senate71 == 71 | senate72 == 72)]))
segments(repMedianSenate,-1,repMedianSenate,0.95,col="red",lwd=.5,lty=2)

hooverY <- 1.5
text(X1[state == 99],y=hooverY,labels="H",col="black",cex=0.8)
arrows(X1[state == 99],hooverY-.1,X1[state == 99],1.05,length=arrowtips,lwd=arrowwidth)
box()

### uncomment to print graphics to file, rather than display in R window
#  dev.off()
############ end 2 x 1 figure code 



## start hypothesis testing, discussed in article text

hooverdim1 <- X1[state == 99]
repMean <- mean(na.omit(X1[party==200  & state!=99 & state!=999 & (senate71 == 71 | senate72 == 72)])) # senate
repMean <- mean(na.omit(X1[party==200  & state!=99 & state!=999 & (house71 == 71 | house72 == 72)])) # house
repMean <- mean(na.omit(X1[party==200  & state!=99 & state!=999])) # all
# test hoover sd in 2d: hoover1dimSD <- 0.1264356
hoover1dimSD <- 0.1468922 
hooverDim1 <- hooverdim1
hooverVotes <- 135 

tStatistic <- (hooverDim1  - repMean) / hoover1dimSD
dt(tStatistic,hooverVotes -2) / 2

# hypo tests with median
repMedianHouse <- median(na.omit(X1[party==200 & state!=99 & state!=999 & (house71 == 71 | house72 == 72)]))
nHouseRep <- length(na.omit(X1[party==200 & state!=99 & state!=999 & (house71 == 71 | house72 == 72)]))
devFromMedian <- (repMedianHouse - na.omit(X1[party==200 & state!=99 & state!=999 & (house71 == 71 | house72 == 72)]))^2
stDevMedian <- sqrt( sum(devFromMedian) / (nHouseRep - 1))
hooverMedianHouseTestStat <- (hooverdim1 - repMedianHouse ) / stDevMedian
repMedianSenate <- median(na.omit(X1[party==200 & state!=99 & state!=999 & (senate71 == 71 | senate72 == 72)]))
republicanMedian <- median(na.omit(X1[party==200 & state!=99 & state!=999]))
repMedian <- repMedianHouse 
tStatistic <- (hooverDim1  - repMedian) / hoover1dimSD
dt(tStatistic,hooverVotes -2) / 2



################################################# 
# this analysis is referenced in the discussion text of the paper

### does Hoover have more voting errors than MC?

votesCorrect <- resultw$legislators[1] + resultw$legislators[4]
votesWrong <- resultw$legislators[2] + resultw$legislators[3]
propWrong <- votesWrong / (votesWrong + votesCorrect) 
# Legislator 689 0.05109489 # Hoover
1 - 0.05109489
mean(na.omit(propWrong))
sd(na.omit(propWrong))
hhStat <- (0.05109489 - mean(na.omit(propWrong))) / sd(na.omit(propWrong))

#### is the second dimension regional difference?
mean(na.omit(X2[statename != "USA    " & state >= 0 & state < 30 & party == 200]))
mean(na.omit(X2[statename != "USA    " & state >= 30 & state < 80 & party == 200]))
dim2regression <- lm(X2[statename != "USA    "] ~ state[statename != "USA    "] + party[statename != "USA    "])
summary(dim2regression)












