#-------------------------------------------------------------------------------																
# This code is designed to compare the homeownership taxation neutrality indices
# to rent control indices.

																			
# Written by K.A.Kholodilin												
# DIW Berlin and NRU HSE St. Petersburg
# kkholodilin (at) diw . de
	
# First created --- September 30, 2020										
# Last modified --- January 28, 2021
#-------------------------------------------------------------------------------

rm(list = ls())

library(stringr) # Needed to add leading characters
library(scales) # Needed to make transparent plots
library(plotrix)
library(stargazer)
library(openxlsx)

#-------------------------------------------------------------------------------
#--- Initial settings

sFolder = "c:/KKHolodilin/MyCodes/Regulation/"
sInFile = "Data/HOR_Macro_Regulation_data_Total.csv"
sOutFile = "Draft/Neutrality/Fig_Tax_index_vs_Rent_control_EN.pdf"

#-------------------------------------------------------------------------------
#--- Load data

X = read.csv(paste(sFolder, sInFile, sep=""), sep = ";", dec = ",")

#-------------------------------------------------------------------------------
#--- Remove countries without data

X = X[which(X$Year>1909),]

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

Y$Rent_laws = (Y$Rent_laws - mean(Y$Rent_laws, na.rm=T)) / sd(Y$Rent_laws, na.rm=T)
Y$Tax_index = (Y$Tax_index - mean(Y$Tax_index, na.rm=T)) / sd(Y$Tax_index, na.rm=T)
Y$Neutrality = (Y$Neutrality - mean(Y$Neutrality, na.rm=T)) / sd(Y$Neutrality, na.rm=T)

#-------------------------------------------------------------------------------
#--- Compute cross-section correlations by year

Y$Corr = NA
Y$p_value = NA

  for(i in 1:NYear)
  {
X_i = X[which(X$Year==Y$Year[i]),]
Y$Corr[i] = cor.test(X_i$Rent_laws, X_i$Tax_index, na.rm=T)$estimate
Y$p_value[i] = cor.test(X_i$Rent_laws, X_i$Tax_index, na.rm=T)$p.value
  }

Y

#-------------------------------------------------------------------------------
#--- Plot composite tax neutrality index vs. rent control index

XLim = range(X$Year)
YLim = range(Y[, c("Rent_laws", "Tax_index", "Neutrality")], na.rm=T)
svCol = c("black", alpha("cyan4", 0.5), "indianred4")

pdf(paste(sFolder, sOutFile, sep=""))
par(mfrow=c(1, 1), mar=c(3,3,1,1), cex.axis=1, cex.main=0.9, bty="l")

plot(Y$Year, Y$Rent_laws, xlim=XLim, ylim=YLim, xlab="", ylab="", lwd=3, type="s", xaxs="i") 
lines(Y$Year, Y$Tax_index, type="s", xlim=XLim, ylim=YLim, xlab="", ylab="", lwd=3, col=svCol[2])
lines(Y$Year, Y$Neutrality, type="s", xlim=XLim, ylim=YLim, xlab="", ylab="", lwd=3, col=svCol[3])
mtext("Normalized index values", side=2, line=2)
svLeg = c("Rent control", "Tax attractivity", "Tax neutrality")
legend("top", lwd=c(3,3), col=svCol, legend = svLeg, bty="n", cex=1)

# plot(Y$Year, Y$Cor, type="l", lwd=3, main="Querschnittskorrelation")
# abline(h=0, lty=3)

dev.off()