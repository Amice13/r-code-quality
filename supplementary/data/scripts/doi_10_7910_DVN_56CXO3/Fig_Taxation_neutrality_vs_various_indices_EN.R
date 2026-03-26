#-------------------------------------------------------------------------------																
# This code is designed to compare the homeownership taxation neutrality indices
# to rent control indices.

																			
# Written by K.A.Kholodilin												
# DIW Berlin and NRU HSE St. Petersburg
# kkholodilin (at) diw . de
	
# First created --- October 30, 2020										
# Last modified --- March 17, 2022
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
sInFile1 = "Data/HOR_Macro_Regulation_data_Total.csv"
sInFile2 = "Data/Leximetric_data_2022-02-16.xlsx"
sInFile3 = "Data/Inequality/Economic_freedom_world_Fraser_2020_interpolated.csv"
sInFile4 = "Data/TID_indices_Seelkopf_et_al.csv"
sOutFile = "Draft/Neutrality/Fig_Tax_index_vs_various_indices_EN.pdf"

#-------------------------------------------------------------------------------
#--- Load data

X = read.csv(paste(sFolder, sInFile1, sep=""), sep = ";", dec = ",")
VI = read.xlsx(paste(sFolder, sInFile2, sep=""))#, sep = ",", dec = ".")
EFW = read.csv(paste(sFolder, sInFile3, sep=""), sep = ";", dec = ",")
TID = read.csv(paste(sFolder, sInFile4, sep=""), sep = ";", dec = ",")

#-------------------------------------------------------------------------------
#--- Merge data

svKeep = c("Code_year", "Country", "Year", "Tax_index", "Rent_laws", "Tenure_security", "Rationing", "RMRI")
VI$Code_year = paste(VI$ISO.Code, VI$Year, sep="+")
svKeep_vi = c("Code_year", "Andrews2011_rentcontrolsocial", "Andrews2011_tenantlandlord", "Bandiera2000_financialreform",    
              "Botero2003_collectivelaws", "Botero2003_employmentlaws","Botero2003_socialsecuritylaws",    
              "CBR_creditor31countries", "CBR_creditor5countries", "CBR_labour117countries", 
              "CBR_labour5countries", "CBR_shareholder30countries", "CBR_shareholder5countries",       
              "CCL2018_competitionlaw", "Cuerpo2014_rentcontrol", "Cuerpo2014_tenantlandlord",                       
              "Djankov2000_cost", "Djankov2000_procedures", "Djankov2000_time", "Djankov2007_creditorrights",       
              "Djankov2008_debtenforcement", "FreedomHouse2020Index", "Grubb1993_employmentregulation",
              "Gwartney_EFW", "Gwartney_EFW_Panel", "Laeven2003_financiallib", #"Kholodilin2018_rentalmarket", 
              "LaPorta1997_antidirectotrigths", "LaPorta1997_creditorrights", "LaPorta1998_antidirectotrigths",
              "Malpezzi1991_rentcontrol", "Nicoletti1998_EPL", "Nicoletti1998_productmarket",
              "OECD_EPLindividual","OECD_EPLindividualandcollectivev2",
              "OECD_EPLindividualandcollectivev3", "OECD_EPLtemporaryv1", "OECD_EPLtemporaryv3",            
              "Pistoretal2000_ANTIBLOCK", "Pistoretal2000_ANTIMANAG",     
              "Pistoretal2000_COLLAT", "Pistoretal2000_CREDCON", "Pistoretal2000_EXIT",              
              "Pistoretal2000_LLSVcr", "Pistoretal2000_LLSVsh", "Pistoretal2000_REMEDY",           
              "Pistoretal2000_SMINTEGR", "Pistoretal2000_VOICE",                           
              "Ruleofthelaw2020Index", "Seelkopf2019_TID", "Whitehead2012_rentcontrol")

X = merge(X[, svKeep], VI[, svKeep_vi], by="Code_year", all=T)
X = merge(X, EFW[, c("Code_year", "EFW_interp")], by="Code_year", all=T)

#-------------------------------------------------------------------------------
#--- Add tax introduction data of Seelkopf et al.

svVar = names(TID)
svVar = setdiff(svVar, c("Country", "Year", "ISO.alpha3.code"))

TID$Seelkopf_TID = rowSums(TID[, svVar]) / length(svVar)

TID$Code_year = paste(TID$ISO.alpha3.code, TID$Year, sep="+")
X = merge(X, TID[, c("Code_year", "Seelkopf_TID")], by="Code_year", all.x=T)

summary(X)

#-------------------------------------------------------------------------------
#--- Prepare data

#---Remove countries without data

X = X[which(X$Year>1909),]

X$Seelkopf2019_TID = NULL

vYear = unique(X$Year)
NYear = length(vYear)

svCountry = sort(unique(X$ISO.Code))

names(X) = gsub("Kholodilin2018_rentalmarket", "Kholodilin2020_rentalmarket", names(X))

svVar = names(X)
svVar = setdiff(svVar, c("Code_year", "Country", "ISO.Code", "Year"))
# svVar = svVar[-grep("legal_origin", svVar)]
# svVar = svVar[-grep("GDP", svVar)]

# svCountry_keep = c()
# vSel_keep = c()
# 
#   for(i in svCountry)
#   {
# vSel_i = which(X$iso_a3==i)
# NNA = sum(is.na(X$Tax_index[vSel_i])) 
#     if(NNA<NYear){
# svCountry_keep=c(svCountry_keep, i)
# vSel_keep = c(vSel_keep, vSel_i)
#     }
#   }
# 
# X = X[vSel_keep,]

#-------------------------------------------------------------------------------
#--- Compute correlations

mCor = cor(X[, svVar], use="pairwise.complete.obs")
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
Tab$Period = ""

  for(i in 1:NIndex)
  {
Cor_i = cor.test(X$Tax_index, X[, Index[i]])    
Tab$Corr[i] = Cor_i$estimate
Tab$p_value[i] = Cor_i$p.value

X_i = X[, c("Country", "Year", Index[i])]
X_i = na.omit(X_i)
Tab$NCountry[i] = length(unique(X_i$Country))
Tab$NYear[i] = length(unique(X_i$Year))

vYear = range(X_i$Year)
vYear = unique(vYear)
Tab$Period[i] = paste(vYear, collapse="-")
  }

Tab$Significant = ifelse(Tab$p_value<0.01, 1, 0)
Tab$Col = "cyan4"
Tab$Col[which(Tab$Significant==0)] = "gray60"
Tab$Sector = "Housing"
Tab$Sector[grep("labour", Tab$Index)] = "Labor"
Tab$Sector[grep("employment", Tab$Index)] = "Labor"
Tab$Sector[grep("collective", Tab$Index)] = "Labor"
Tab$Sector[grep("EPL", Tab$Index)] = "Labor"
Tab$Sector[grep("creditor", Tab$Index)] = "Finance"
Tab$Sector[grep("CREDCON", Tab$Index)] = "Finance"
Tab$Sector[grep("financ", Tab$Index)] = "Finance"
Tab$Sector[grep("sharehold", Tab$Index)] = "Finance"
Tab$Sector[grep("Pistor", Tab$Index)] = "Finance"
Tab$Sector[grep("Freedom", Tab$Index)] = "Rule of law"
Tab$Sector[grep("Ruleofthelaw", Tab$Index)] = "Rule of law"
Tab$Sector[grep("Gwartney", Tab$Index)] = "Rule of law"
Tab$Sector[grep("competition", Tab$Index)] = "Product"
Tab$Sector[grep("product", Tab$Index)] = "Product"
Tab$Sector[which(Tab$Index=="Seelkopf_TID")] = "Modern taxation"
svSector = sort(unique(Tab$Sector))

Tab$Sect_col = "cyan4"
Tab$Sect_col[which(Tab$Sector=="Housing")] = "gray50"
Tab$Sect_col[which(Tab$Sector=="Labor")] = "red"
Tab$Sect_col[which(Tab$Sector=="Finance")] = "black"
Tab$Sect_col[which(Tab$Sector=="Product")] = "green"
Tab$Sect_col[which(Tab$Sector=="Rule of law")] = "orange"

Tab$Index = gsub("abiad", "Abiad", Tab$Index)
Tab$Index = gsub("Index", "", Tab$Index)
Tab$Index = gsub("_", " ", Tab$Index)
Tab$Index = gsub("laws", " laws", Tab$Index)
Tab$Index = gsub("countries", " countries", Tab$Index)
Tab$Index = gsub("EPL", "EPL ", Tab$Index)
Tab$Index = gsub("temporary", "temporary ", Tab$Index)
Tab$Index = gsub("financial", "financial ", Tab$Index)
Tab$Index = gsub("creditor", "creditor ", Tab$Index)
Tab$Index = gsub("labour", "labor ", Tab$Index)
Tab$Index = gsub("individualandcollective", "individual & collective ", Tab$Index)
Tab$Index = gsub("Ruleofthelaw2020", "Rule of the law", Tab$Index)
Tab$Index = gsub("FreedomHouse2020", "Freedom House", Tab$Index)
Tab$Index = gsub("Pistoretal", "Pistor et al.", Tab$Index)
Tab$Index = gsub("Seelkopf", "Seelkopf et al.", Tab$Index)
Tab$Index = gsub("CREDCON", "creditor rights", Tab$Index)
Tab$Index = gsub("19", " 19", Tab$Index)
Tab$Index = gsub("20", " 20", Tab$Index)

stargazer(Tab, type="text", summary=F)

svName = c("Index", "Corr", "p_value", "NCountry", "NYear", "Period")
stargazer(Tab[, svName], summary=F, rownames = F)

#-------------------------------------------------------------------------------
#--- Plot correlations with tax neutrality index

svCol = c("black", "gray50", "red", "cyan4", "green", "orange")
vSel_s = c(1,2,3,4,5,6)
svSector = svSector[vSel_s]
svCol = svCol[vSel_s]

par(mfrow=c(1, 1), mar=c(3,8.5,1,1), cex.axis=0.5, bty="l")

barplot(Tab$Corr, names.arg=Tab$Index, xlab="", ylab="", horiz=T, las=2, space=0, col=Tab$Col, yaxt="n")
axis(2, at=c(1:NIndex), labels=Tab$Index, las=2) #main=svVar_fig[i], 

vSel = which(Tab$Significant==1)
XLim = range(pretty(Tab$Corr, na.rm=T))

pdf(paste(sFolder, sOutFile, sep=""))
par(mfrow=c(1, 1), mar=c(3,13,1,1), cex.axis=0.8, bty="l")
barplot(Tab$Corr[vSel], names.arg=Tab$Index[vSel], xlab="", ylab="", xlim=XLim, horiz=T, las=1, space=0.1, col=Tab$Sect_col[vSel], border = NA)
mtext("Correlation coefficient", side=1, line=2) # 
legend("bottomright", legend=svSector, fill=svCol, border = NA, bty="n")
dev.off()