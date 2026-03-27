#-------------------------------------------------------------------------------																
# This code is designed to compare the homeownership taxation neutrality indices
# to rent control indices and to social housing stock.

																			
# Written by K.A.Kholodilin												
# DIW Berlin and NRU HSE St. Petersburg
# kkholodilin (at) diw . de
	
# First created --- March 4, 2021										
# Last modified --- May 6, 2022
#-------------------------------------------------------------------------------

rm(list = ls())

library(stringr) # Needed to add leading characters
library(scales) # Needed to make transparent plots
library(plotrix)
library(stargazer)
library(openxlsx)

#-------------------------------------------------------------------------------
#--- Initial settings

sFig = "pdf" #png 
sFolder = "c:/KKHolodilin/MyCodes/Regulation/"
sInFile = "Data/Inequality/Inequality_data_transformed.csv"
sOutFile = paste("Draft/Neutrality/Fig_Taxation_Rent_control_Social_housing.", sFig, sep="")

#-------------------------------------------------------------------------------
#--- Load data

X = read.csv(paste(sFolder, sInFile, sep=""), sep = ";", dec = ",")

#-------------------------------------------------------------------------------
#--- Remove countries without data

X = X[which(X$Year>1909),]
X = X[which(X$Year<2018),]

vYear = unique(X$Year)
NYear = length(vYear)

svCountry = sort(unique(X$iso_a3))
svCountry_keep = c()
vSel_keep = c()

  for(i in svCountry)
  {
vSel_i = which(X$iso_a3==i)
NNA = sum(is.na(X$Tax_index[vSel_i])!=T) 
    if(NNA>0){
svCountry_keep=c(svCountry_keep, i)
vSel_keep = c(vSel_keep, vSel_i)
    }
  }

X = X[vSel_keep,]

sort(unique(X$Country))

#-------------------------------------------------------------------------------
#--- Compute average worldwide indices

Y = aggregate(X$Rent_laws, list(X$Year), mean, na.rm=T)
names(Y) = c("Year", "Rent_laws")
Y$Tax_index = aggregate(X$Tax_index, list(X$Year), mean, na.rm=T)$x
Y$Neutrality = aggregate(X$Neutrality, list(X$Year), mean, na.rm=T)$x
Y$Soc_housing = aggregate(X$Soc_housing_interp, list(X$Year), mean, na.rm=T)$x

Y$Rent_laws = 100 * Y$Rent_laws
Y$Tax_index = 100 * Y$Tax_index

#-------------------------------------------------------------------------------
#--- Plot composite tax neutrality index vs. rent control index

XLim = range(X$Year)
YLim = range(Y[, c("Rent_laws", "Tax_index")], na.rm=T)
svCol = c("black", "cyan4")
YLim_soc = range(pretty(Y$Soc_housing))

  if(sFig=="pdf")
  {
pdf(paste(sFolder, sOutFile, sep="")) # Plot map    
  }else{
png(paste(sFolder, sOutFile, sep=""), res=200, width=1000, height=1000)    
  }
par(mfrow=c(2, 1), mar=c(3,3,1,3), cex.axis=1, cex.main=0.9, bty="l")

plot(Y$Year, Y$Rent_laws, xlab="", ylab="", lwd=3, type="s", xaxs="i", main="Rent control", las=1) 
par(new=T)
plot(Y$Year, Y$Soc_housing, xlab="", ylab="", lwd=3, type="l", xaxs="i", col="cyan4", yaxt="n", ylim=YLim_soc, las=1) 
axis(side=4, las=1)
svLeg = c("Regulation index", "Social housing stock")
legend("bottom", lwd=c(3,3), col=svCol, legend = svLeg, bty="n", cex=1)
mtext("Regulation index values", side=2, line=2)
mtext("Social housing, %", side=4, line=2)

plot(Y$Year, Y$Tax_index, xlab="", ylab="", lwd=3, type="s", xaxs="i", main="Homeownership tax attractiveness", las=1) 
par(new=T)
plot(Y$Year, Y$Soc_housing, xlab="", ylab="", lwd=3, type="l", xaxs="i", col="cyan4", yaxt="n", ylim=YLim_soc) 
axis(side=4, las=1)

mtext("Regulation index values", side=2, line=2)
mtext("Social housing, %", side=4, line=2)

dev.off()