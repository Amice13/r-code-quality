#-------------------------------------------------------------------------------																
# This code is designed to depict the homeownership taxation neutrality indices
# averaged by continents.

																			
# Written by K.A.Kholodilin												
# DIW Berlin and NRU HSE St. Petersburg
# kkholodilin (at) diw . de
	
# First created --- August 27, 2019										
# Last modified --- August 28, 2019
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
sInFile1 = "Data/Property_tax_regulation_indices.csv"
sInFile2 = "Data/Continents_countries_UN.xlsx"

#-------------------------------------------------------------------------------
#--- Load data

X = read.csv(paste(sFolder, sInFile1, sep=""), sep = ";", dec = ",")
# UN = read.xlsx(paste(sFolder, sInFile2, sep=""), sheet = "Data")
# 
#-------------------------------------------------------------------------------
#--- Select subset

X = X[which(X$Year>1909), ]

#-------------------------------------------------------------------------------
#--- Plot the composite tax neutrality index by country

svName = names(X)
svTax = setdiff(svName, c("Year", "iso_a3", "Neutrality", "CGT_duration"))
NTax = length(svTax)

XLim = range(X$Year)

svCountry = as.character(unique(X$iso_a3))
# svCountry = svCountry[1:2]
NCountry = length(svCountry)

  for(iTax in 1:NTax)
  {
sOutFile = paste("Draft/Neutrality/Fig_Neutrality_", svTax[iTax], "_shades.pdf", sep = "")
pdf(paste(sFolder, sOutFile, sep=""))
par(mfrow=c(1, 1), mar=c(3,3,1,1), cex.axis=0.9, bty="l")
plot(XLim, c(1,NCountry), xlab="", ylab="", col="white", yaxt="n")
axis(side=2, at=c(1:NCountry), labels=svCountry, las=2)

  for(iC in 1:NCountry)
  {
X_i = X[which(X$iso_a3==svCountry[iC]),]
vT = X_i$Year
    for(iT in vT)
    {
vX = c((iT-1), iT, iT, (iT-1))
vY = c((iC-0.45), (iC-0.45), (iC+0.45), (iC+0.45))
Poly = cbind(vX, vY)
# print(Poly);
sCol = round(100 - 100*X_i[(iT-min(X_i$Year)+1), svTax[iTax]]) 
sCol = paste("gray", sCol, sep="") 
sCol = gsub("grayNA", "lightgoldenrod1", sCol)
polygon(Poly, col=sCol, border=sCol)
    }

      if(iTax<5)
      {
vX = c(vT[1]-1, vT[length(vT)], vT[length(vT)], vT[1]-1)
vY = c((iC-0.45), (iC-0.45), (iC+0.45), (iC+0.45))
Poly = cbind(vX, vY)
polygon(Poly, col="transparent", border="black", lwd=0.1)        
      } # end of conditional statement
  }
#legend("bottom", lwd=c(1,1), col=c("cyan4", alpha("gray70", 0.5)), legend = c("individual", "average"), bty="n", cex=0.7)
# mtext("Источник: собственные расчёты.", side = 1, line = 2)
dev.off()
  } # end of loop iTax