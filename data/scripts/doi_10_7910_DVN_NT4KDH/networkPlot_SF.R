library(rgdal)

## Load Data
load("netPlotData.Rda")

## Generate Network plot
png("innersunsetFinal.png",width=1920,height=1920,res=800,bg="white")
par(mar=c(0,0,0,0)+.1)
plot(sp,pch=19,col=colvert,cex=.01)
plot(Ps1,add=TRUE,border=rgb(0,0,0,1),lwd=.8)

text(coordinates(sf_proj),labels=as.character(sf_proj$NAME),font=2,cex=.2)

par(fig=c(.25, 4, .6, 3.1)/10,new=TRUE,mgp=c(0,0.2,0),tcl=-.2)
plot.new()
plot.window(xlim=bb[1,],ylim=bb[2,],asp=1)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "white")
lines(cbind(coord[x[,1],],coord[x[,2],]),col=list.edges$col,lwd=list.edges$lwd)
points(jitter(coord),pch=list.vertex$pch,col=list.vertex$col,cex=list.vertex$cex)
box(lwd=1.5)
dev.off()