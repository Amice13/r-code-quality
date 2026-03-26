### This script makes the two halves of Figure 1
### Must run the Stata do file "make tables and figures.do" to create source data.
### This script requires the library readstata13


# Set the working directory
#setwd("")

# Load in data
library("readstata13")

indicators<-read.dta13("Data/Capacity_2015_bottomhalf.dta")

country.names<-c(indicators[,1])
coef.vec<-c(indicators[,2])
se.vec<-c(indicators[,3])

#create indicator for y.axis, descending so that R orders vars from top to bottom on y-axis

y.axis <- c(length(coef.vec):1) 

#open pdf device.  Height and width are in inches. Defaults are 7.

pdf("figure-1-right.pdf", height = 9, width = 3.75)

#set margins for plot, leaving lots of room on left-margin (2nd number in margin command) for variable names

par(mar=c(1, 6, .5, 2))

#plot coefficients as points, turning off axes and labels. Set limits of x-axis so that they include mins and maxs of coefficients + 95% confidence intervals and plot is symmetric; use "internal axes", and leave plot title empty. Note also the size of the x-axis is here.

plot(coef.vec, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, cex = .5, xlim = c(-3,1.0), xaxs = "r", main = "")

# create lines for standard deviations
segments(coef.vec-se.vec, y.axis, coef.vec+se.vec, y.axis, lwd =  1) 

#draw x-axis and labels with minor tick marks #reduce label size, moves labels closer to tick marks
axis(1, at = seq(-3,1,by=.5), labels = NA, tick = T, cex.axis = .8, mgp = c(0,-.5,-1)) 

#draw x-axis and labels with major tick marks # cex.axis sets label size, and mgp moves labels closer to tick marks
axis(1, at = seq(-3,1,by=1), labels =  c(-3,-2,  -1,  0, 1), tick = T, cex.axis = .8, mgp = c(0,-.5,-1)) 

#same as x-axis, but on top axis so #it's easier to lookup coefs at top of graph
axis(3, at = seq(-3,1,by=.5), labels = NA, tick = T, cex.axis = .8, mgp = c(0,-.5,-1)) 

axis(3, at = seq(-3,2,by=1), labels =  c(-3, -2,  -1,  0, 1,2), tick = T, cex.axis = .8, mgp = c(0,-.5,-1))

#draw y-axis with tick marks, make labels perpendicular to axis and closer to axis

segments(0,0,0,y.axis,lty=2) # draw dotted line through 0

# Change the cex.axis to make all the country labels fit. 

axis(2, at = y.axis, label = country.names, las = 1, tick = T, , mgp = c(2,.6,0), cex.axis = .5) 

dev.off() #turn off pdf device; graph is created in working directory.


###### Make the top half plot
indicators<-read.dta13("Data/Capacity_2015_tophalf.dta")

country.names<-c(indicators[,1])
coef.vec<-c(indicators[,2])
se.vec<-c(indicators[,3])

#create indicator for y.axis, descending so that R orders vars from top to bottom on y-axis

y.axis <- c(length(coef.vec):1) 

#open pdf device.  Height and width are in inches. Defaults are 7.

pdf("figure-1-left.pdf", height = 9, width = 3.75)

#set margins for plot, leaving lots of room on left-margin (2nd number in margin command) for variable names

par(mar=c(1, 6, .5, 2))

#plot coefficients as points, turning off axes and labels. Set limits of x-axis so that they include mins and maxs of coefficients + 95% confidence intervals and plot is symmetric; use "internal axes", and leave plot title empty. Note also the size of the x-axis is here.

plot(coef.vec, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, cex = .5, xlim = c(-1,3.0), xaxs = "r", main = "")

# Make lines for standard deviations
segments(coef.vec-se.vec, y.axis, coef.vec+se.vec, y.axis, lwd =  1) 

#draw x-axis and labels with minor tick marks #reduce label size, moves labels closer to tick marks
axis(1, at = seq(-1,3,by=.5), labels = NA, tick = T, cex.axis = .8, mgp = c(0,-.5,-1)) 

#draw x-axis and labels with major tick marks # cex.axis sets label size, and mgp moves labels closer to tick marks
axis(1, at = seq(-1,3,by=1), labels =  c(-1,  0, 1,2,3), tick = T, cex.axis = .8, mgp = c(0,-.5,-1)) 

#same as x-axis, but on top axis so #it's easier to lookup coefs at top of graph
axis(3, at = seq(-1,3,by=.5), labels = NA, tick = T, cex.axis = .8, mgp = c(0,-.5,-1)) 

axis(3, at = seq(-1,3,by=1), labels =  c(-1,  0, 1,2,3), tick = T, cex.axis = .8, mgp = c(0,-.5,-1))

#draw y-axis with tick marks, make labels perpendicular to axis and closer to axis

segments(0,0,0,y.axis,lty=2) # draw dotted line through 0


# Change the cex.axis to make all the country labels fit.

axis(2, at = y.axis, label = country.names, las = 1, tick = T, ,mgp = c(2,.6,0), cex.axis = .5) 

dev.off() #turn off pdf device; graph is created in working directory.

