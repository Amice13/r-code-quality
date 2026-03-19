#-------------------------------------------------------------------------------																
# This code is designed to estimate the determinants of homeownership tax
# treatment index using random-effects panel data models.

																			
# Written by K.A.Kholodilin												
# DIW Berlin and NRU HSE St. Petersburg
# kkholodilin (at) diw . de
	
# First created --- February 8, 2021									
# Last modified --- March 21, 2022
#-------------------------------------------------------------------------------

rm(list = ls())

Begin=proc.time()

library(R.oo)
library(openxlsx)
library(texreg)
library(stargazer)
library(plm)
library(lmtest)

#-------------------------------------------------------------------------------
#--- Initial settings

cStart = 1899 # 1959
sFolder = "c:/kkholodilin/MyCodes/Regulation/"
sInFile = "Data/Inequality/Inequality_data_transformed.csv"
sOutFile = paste("Draft/Neutrality/Tab_Estimation_panel_model_RE_Composite_indices_all_from_", cStart, ".tex", sep="")

#-------------------------------------------------------------------------------
#--- Load data

X = read.csv(paste(sFolder, sInFile, sep=""), dec=",", sep=";")

#-------------------------------------------------------------------------------
#--- Select data

X = X[which(X$Year>=cStart),]
X = X[which(X$Legal_origin!="Socialist"),]

#-------------------------------------------------------------------------------
#--- Convert data to panel format

X$Soc_housing_interp = X$Soc_housing_interp / 100

X = pdata.frame(X, index=c("Country", "Year"))

#-------------------------------------------------------------------------------
#--- Estimate panel regressions: basic model

svFE = rep(c("individual", "twoways"), each=2)
svDep = c("Tax_index", "Neutrality", "Tax_index", "Neutrality")
svControl = c("LGDP_PC", "Lpop", "Rent_laws", "Social2GDP_interp", "Left_right", "TID", "infcap_pca") #, "leftgov","Soc_housing_interp", "leftgov",  , "EFW_interp", "Old_age_ratio"  "HOR_lag1""infcap_irt" , "War_deaths",    "Prime_minister", "I2GDP_lag1" "P2R_lag1", "Dep_ratio_lag1", "Urb_growth", "Mort2GDP", "Debt2GDP"LTIR ,  + + P2R + Condo_urb Pop_0014 + Pop_65plus
svControl = c("LGDP_PC", "Lpop", "Rent_laws") # fewer variables more observations
svControl = paste(svControl, "_lag1", sep="")
svControl = c(svControl, "Legal_origin")
sControl = paste(svControl, collapse = " + ")

List_GMM = list()
List_rob_se = list()
List_rob_pval = list()

Count = 1
  for(iR in svDep)
  {
sFormula0 = paste(iR, " ~", "+", sControl) 
NVar = length(unlist(strsplit(sFormula0, "\\+")))
sFormula = as.formula(sFormula0)

#PLM_i = plm(sFormula, data=X, effect=svFE[Count], model="within")
PLM_i = plm(sFormula, data=X, model = "random", random.method = "walhus", effect = svFE[Count])
PLM_robust_i = coeftest(PLM_i,vcov=vcovHC(PLM_i, type="HC0", cluster="group"))
List_GMM[[Count]] = PLM_i
List_rob_se[[Count]] = PLM_robust_i[,2]
List_rob_pval[[Count]] = PLM_robust_i[,4]

#--- Hausman test for fixed vs. random effects
#--- When p-values are low, then use fixed effects.

#Hausman = phtest(sFormula, data=X, method = "aux", vcov = function(x) vcovHC(x, method="white2", type="HC3"))
#Tab_test$Hausman[Count] = Hausman$p.value

#--- Tests for time fixed effects
#--- When p-value is low, then use time effects.

Pooled_i = plm(sFormula, data=X, model="pooling")
# FE_ind_i = plm(sFormula, data=X, model="within", effect="individual")
# FE_indtime_i = plm(sFormula, data=X, model="within", effect="twoways")
# Ftest_ind = pFtest(FE_ind_i, Pooled_i) # SPECIFY IT BETTER!!!
# Tab_test$F_test_ind[Count] = Ftest_ind$p.value
# Ftest_time = pFtest(FE_indtime_i, FE_ind_i)
# Tab_test$F_test_time[Count] = Ftest_time$p.value
#plmtest(sFormula0, data=X)

pwtest(Pooled_i)
pwtest(Pooled_i, effect="time")

#--- Serial correlation Breusch-Godfrey/Wooldridge test

pbgtest(sFormula, data=X)

#--- Arellano-Bond test for autocorrelation
#--- Null hypothesis: all second-order autocovariances for all periods are zero.
#--- The consistency of the GMM estimator hinges on this assumption.

AB_autocor = pdwtest(PLM_i)#, order=2, vcov=vcovHC)
# Tab_test$AB_autocor[Count] = as.double(AB_autocor$p.value)

#--- Goodness-of-fit measure

aux = summary(PLM_i)
# Tab_test$R2_overall[Count] = aux$r.squared[2]

Count = Count + 1
  }

svModel = paste(svDep, svFE, sep=", ")
svModel = gsub("Tax_index", "Attractiveness index", svModel)
svModel = gsub("Neutrality", "Neutrality index", svModel)
texreg(List_GMM, label = "tab:Panel_data_model", float.pos = "htbp", fontsize = "normalsize",
          caption="Estimation results of panel data model: basic specification", caption.above = T,
       override.se=List_rob_se, override.pvalues = List_rob_pval, digits=3, custom.model.names = svModel,
       file=paste(sFolder, sOutFile, sep=""))# , custom.coef.map = sList

# stargazer(List_GMM, type="text")

#stargazer(Tab_test[, c(1:5)], summary=F, rownames = F)

#-------------------------------------------------------------------------------
#--- Descriptive statistics of variables

# svVar = c("Rent_laws", "Rent_laws2", "RC_gen", "Tenure_security", "Rationing", "RMRI",
#           "DLGDP_PC", "Dep_ratio", "LTIR") #, "Urbanization"
# 
# stargazer(X[, svVar], summary.stat=c("min", "mean", "max", "sd"))
# 
Country_period = names(aux$residuals)
Country_period = Country_period[grep("-", Country_period)]
svCountry = unlist(lapply(strsplit(as.character(Country_period), '-'), function(x) x[1]))
Period = unlist(lapply(strsplit(as.character(Country_period), '-'), function(x) x[2]))
Country = unique(svCountry)
mReg = aux$model
#mReg$Country = Country

Tab_GDP = data.frame(Country)
Tab_GDP$Beg = NA
Tab_GDP$End = NA
Tab_GDP$NDecade = NA
Tab_GDP$Mean_dep_var = NA

  for(j in 1:nrow(Tab_GDP))
  {
vSel_j = which(svCountry==Country[j])
Tab_GDP$Beg[j] = min(Period[vSel_j])
Tab_GDP$End[j] = max(Period[vSel_j])
Tab_GDP$NDecade[j] = length(Period[vSel_j])
#Tab_GDP$Mean_dep_var[j] = 100*mean(mReg$DLbau1_pop[vSel_j])
  }

stargazer(Tab_GDP, summary=F, type="text", rownames=F)

paste(Tab_GDP$Country, collapse=", ")

#-------------------------------------------------------------------------------
proc.time() - Begin