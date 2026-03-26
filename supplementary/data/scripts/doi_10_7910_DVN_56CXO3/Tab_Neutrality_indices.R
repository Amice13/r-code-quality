#-------------------------------------------------------------------------------																
# This code is designed to tabulate the homeownership taxation neutrality indices
# for different countries.

																			
# Written by K.A.Kholodilin												
# DIW Berlin and NRU HSE St. Petersburg
# kkholodilin (at) diw . de
	
# First created --- August 27, 2019										
# Last modified --- November 2, 2020
#-------------------------------------------------------------------------------

rm(list = ls())

library(stringr) # Needed to add leading characters
library(stargazer)
library(openxlsx)
library(scales)

#-------------------------------------------------------------------------------
#--- Initial settings

sFolder = "c:/KKHolodilin/MyCodes/Regulation/"
sInFile = "Data/Property_tax_regulation_indices.csv"
sOutFile = "Draft/Neutrality/Tab_Neutrality_indices.tex"

#-------------------------------------------------------------------------------
#--- Load data

X = read.csv(paste(sFolder, sInFile, sep=""), sep=";", dec=",")

#--- Compute composite index

svName = names(X)
svTax = setdiff(svName, c("Year",  "iso_a3", "CGT_duration"))
NTax = length(svTax)

Country = as.character(unique(X$iso_a3))
NCountry = length(Country)

#-------------------------------------------------------------------------------
#--- Compute country averages

Tab = data.frame(Country)
svIndex = as.character(unique(X$Decade))
Tab[, svTax] = NA
Country = "Average"
Tab_all = data.frame(Country)
Tab_all[, svTax] = NA

  for(iC in 1:NTax)
  {
Tab[, (iC+1)] = aggregate(X[, svTax[iC]], list(X$iso_a3), mean, na.rm=T)$x    
Tab_all[, (iC+1)] = mean(X[, svTax[iC]], na.rm=T)  
  }

Tab = rbind(Tab, Tab_all)
stargazer(Tab, summary = F, rownames = F, digit.separate = 0)#, out = paste(sFolder, sOutFile, sep=""))

#-------------------------------------------------------------------------------
#--- Compute correlations

Country = as.character(unique(X$iso_a3))
Year = sort(unique(X$Year))

X_wide = data.frame(Year)
X_wide[, Country] = NA

  for(i in Country)
  {
X_i = X[which(X$iso_a3==i),]
Sel_i = which(names(X_wide)==i)   
X_wide[match(X_i$Year, X_wide$Year), Sel_i] = X_i$Tax_index
  }

mCorr = cor(X_wide[,-1], use = "pairwise.complete.obs")
mCorr[upper.tri(mCorr)] = NA
diag(mCorr) = NA
mCorr = mCorr[-1,]
mCorr = mCorr[, -ncol(mCorr)]
stargazer(mCorr, summary = F, digits = 3, type="text")

plot(X_wide$Year, X_wide$RUS, type="s", lwd=3, ylim=c(0,1))
lines(X_wide$Year, X_wide$CAN, col=alpha("cyan4", 0.5), lwd=3, type = "s")
