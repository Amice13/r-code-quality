#-------------------------------------------------------------------------------																
# This code is designed to compute and show the correlations between the 
# social housing and other indicators.

																			
# Written by K.A.Kholodilin												
# DIW Berlin and NRU HSE St. Petersburg
# kkholodilin (at) diw . de
	
# First created --- October 30, 2020										
# Last modified --- April 28, 2021
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
sInFile = "Data/Inequality/Inequality_data.csv"
sOutFile = paste("Draft/Neutrality/Fig_Correlation_Social_housing_with_various_indicators.", sFig, sep="")

#-------------------------------------------------------------------------------
#--- Load data

X = read.csv(paste(sFolder, sInFile, sep=""), sep = ";", dec = ",")

#-------------------------------------------------------------------------------
#--- Prepare data

#---Remove countries without data

X = X[which(X$Year>1909),]
X = X[which(X$Year<2019),]

vYear = unique(X$Year)
NYear = length(vYear)

svCountry = sort(unique(X$ISO.Code))

svVar = c("Soc_housing", "Rent_laws", "Tenure_security", "Rationing", "Tax_index",
          "Neutrality" , "Social2GDP_interp", "Housing_allowances", "Mortgage_Index")

#-------------------------------------------------------------------------------
#--- Compute correlations

mCor = cor(X[, svVar], use= "pairwise.complete.obs")
vCor = mCor[,1]
vCor = vCor[-1]
vCor = vCor[which(is.na(vCor)!=T)]
vCor = sort(vCor)

Index = names(vCor)
NIndex = length(Index)

Tab = data.frame(Index)
Tab$Corr = NA
Tab$p_value = NA
Tab$NCountry = NA
Tab$NYear = NA

  for(i in 1:NIndex)
  {
Cor_i = cor.test(X$Soc_housing, X[, Index[i]])    
Tab$Corr[i] = Cor_i$estimate
Tab$p_value[i] = Cor_i$p.value

X_i = X[, c("Country", "Year", Index[i])]
X_i = na.omit(X_i)
Tab$NCountry[i] = length(unique(X_i$Country))
Tab$NYear[i] = length(unique(X_i$Year))
  }

Tab$Significant = ifelse(Tab$p_value<0.01, 1, 0)
Tab$Col = "cyan4"
Tab$Col[which(Tab$Significant==0)] = "gray60"

Tab$Index = gsub("_", " ", Tab$Index)
Tab$Index = gsub("interp", "", Tab$Index)
Tab$Index = gsub("Social2GDP", "Social expenditure", Tab$Index)
Tab$Index = gsub("Tax index", "Tax attractiveness index", Tab$Index)
Tab$Index = gsub("Mortgage Index", "Fuller mortgage index", Tab$Index)
Tab$Index = gsub("laws", "control", Tab$Index)
Tab$Index = gsub("Rationing", "Housing rationing", Tab$Index)

stargazer(Tab, type="text", summary=F)

#-------------------------------------------------------------------------------
#--- Plot correlations with tax neutrality index

XLim = range(pretty(Tab$Corr, na.rm=T))

svCol = c("cyan4", "gray70")
Tab$Col = ifelse(Tab$Significant==1, "cyan4", "gray70")
Tab$Col = "cyan4"

  if(sFig=="pdf")
  {
pdf(paste(sFolder, sOutFile, sep="")) # Plot map    
  }else{
png(paste(sFolder, sOutFile, sep=""), res=200, width=1000, height=1000)    
  }
par(mfrow=c(1, 1), mar=c(3,9,1,1), cex.axis=0.8, bty="l")
barplot(Tab$Corr, names.arg=Tab$Index, xlab="", ylab="", xlim=XLim, horiz=T, las=1, space=0.1, col=Tab$Col, border = NA)
mtext("Correlation coefficient", side=1, line=2) 
#legend("bottomright", fill=svCol, border=svCol, legend=c("Significant", "Insignificant"), bty="n", cex=0.8)
dev.off()