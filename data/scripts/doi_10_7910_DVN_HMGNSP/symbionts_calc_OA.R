#The following analyses were performed for the article:

#Mason, R. A. B. (2018) Decline in symbiont densities of tropical and subtropical
#scleractinian corals under ocean acidification. Coral Reefs (in press)

#------------------------------------------------------------------------------------
# Symbiont cell density per unit area [results described in first paragraph following the heading "Effects of pCO2" in the above publication]:
meta1<-read.csv("symbionts.csv",header=T, sep=",")
#Explanation of column headings
#changepCO2: the difference (in uatm) in the partial pressure of carbon dioxide level between the ocean acidification experimental treatment and control in the experiment from which each effect size was calculated
#Hedgesd: the effect size (calculated with 'Hedges D' equation) of change in symbiont density per unit of coral surface area under ocean acidification compared to present-day conditions
#varianceofHedges d: the variance of the effect size
#Species: the coral species in which each effect size was measured.
#Study_First_Author:The first author of the study from which the measurement came (see the end of this R code for the associated reference list)

attach(meta1)
model.1 <- lm(Hedgesd ~ changepCO2, weights=1/varianceofHedgesd)
summary(model.1)
plot(model.1)
mean(model.1$residuals)
var(model.1$residuals)



detach(meta1)
#------------------------------------------------------------------------------------
#Effect of symbiont density changes on calcification [results described in the third paragraph following the heading "Effects of pCO2" in the above publication]:
meta1<-read.csv("calcification.csv",header=T, sep=",")
#Explanation of column headings
#Species: the coral species in which each effect size was measured
#Technique: the technique used to measure coral calcification in each study
#Study_First_Author:The first author of the study from which the measurement came (see the end of this R code for the associated reference list)
#calc_fx_size: the effect size (calculated with 'Hedges D' equation) of change in coral calcification under ocean acidification compared to present-day conditions
#calc_var_fx_size:: the variance of the calcification effect size
#pCO2_diff: the difference (in uatm) in the partial pressure of carbon dioxide level between the ocean acidification experimental treatment and control in the experiment from which each effect size was calculated
#change_cells_fx_size_uatm: the effect size (calculated with 'Hedges D' equation) of change in symbiont density per unit area under ocean acidification compared to present-day conditions, divided by pCO2_diff
library(car)

attach(meta1)
#Full model with interaction term
model.1 <- lm(calc_fx_size ~ pCO2_diff*change_cells_fx_size_uatm, weights=1/calc_var_fx_size)
vif(model.1) #test for linear relationship between explanatory variables. Appears to be one such relationship between change_cells_fx_size_uatm and pCO2_diff:change_cells_fx_size_uatm, so
#drop interaction term (but not legitimate to drop pCO2_diff term as it should mechanistically cause change in calcification according to current biological thought).
model.1 <- lm(calc_fx_size ~ pCO2_diff + change_cells_fx_size_uatm, weights=1/calc_var_fx_size)
summary(model.1) #interpret the model, see: https://feliperego.github.io/blog/2015/10/23/Interpreting-Model-Output-In-R
#Check the assumptions of the model (see http://r-statistics.co/Assumptions-of-Linear-Regression.html)
plot(model.1)
mean(model.1$residuals)
cor.test(meta1$pCO2_diff + meta1$change_cells_fx_size_uatm, model.1$residuals)#need to get this to work
var(model.1$residuals)

vif(model.1) #no relationship between explanatory variables detected

#drop change_cells_fx_size_uatm term, rerun and check assumptions of new model
model.1 <- lm(calc_fx_size ~ pCO2_diff, weights=1/calc_var_fx_size)
summary(model.1)
plot(model.1)
mean(model.1$residuals)
var(model.1$residuals)

detach(meta1)
#------------------------------------------------------------------------------------

#Ancova for symbiont cell densities per cm2 under two temperatures during acidification [results described under the heading "Interaction of temperature and pCO2" in the above publication]:

temp1<-read.csv("symbionts_temp_x_CO2.csv",header=T, sep=",")

#Explanation of column headings
#temp: identifies whether the datapoint comes from a "low" or "high" temperature. Within each study in "symbionts_temp_x_CO2.csv", two temperature levels were used (a lower water temperature, typically non-stressful to corals, and a warmer water temperature, typically stressful to corals)
#Hedgesd: the effect size (calculated with 'Hedges D' equation) of change in symbiont density per unit of coral surface area under ocean acidification compared to present-day conditions
#varianceHedgesd: the variance of the effect size
#pCO2difference: the difference (in uatm) in the partial pressure of carbon dioxide level between the ocean acidification experimental treatment and control in the experiment from which each effect size was calculated
#Species: the coral species in which each effect size was measured.
#Study_First_Author: The first author of the study from which the measurement came (see the end of this R code for the associated reference list)
library(car)
temp1[4,4]<-NA #Remove datapoint with very high pCO2 (in comparison to the other datapoints)
temp1[21,4]<-NA #Remove datapoint with very high pCO2 (in comparison to the other datapoints)
temp1<-na.omit(temp1)
ancova1 <- aov(Hedgesd~pCO2difference*temp, data=temp1, weights=1/varianceHedgesd)
summary(ancova1)
plot(ancova1)
mean(ancova1$residuals)
cor.test(temp1$changepCO2, ancova$residuals)
var(ancova1$residuals)

vif(ancova1)

#------------------------------------------------------------------------------------

#References:
#Baghdasarian G, Osberg A, Mihora D, Putnam H, Gates RD, Edmunds PJ (2017) Effects of Temperature and pCO2 on Population Regulation of Symbiodinium spp. in a Tropical Reef Coral. Biol Bull 232:123-139 
#Bedwell-Ivers HE, Koch MS, Peach KE, Joles L, Dutra E, Manfrino C (2017) The role of in hospite zooxanthellae photophysiology and reef chemistry on elevated pCO2 effects in two branching Caribbean corals: Acropora cervicornis and Porites divaricata. ICES J Mar Sci 74:1103-1112 
#Camp EF, Smith DJ, Evenhuis C, Enochs I, Manzello D, Woodcock S, Suggett DJ (2016) Acclimatization to high-variance habitats does not enhance physiological tolerance of two key Caribbean corals to future temperature and pH. Proc R Soc B 283:20160442 
#Godinot C, Houlbrèque F, Grover R, Ferrier-Pagès C (2011) Coral Uptake of Inorganic Phosphorus and Nitrogen Negatively Affected by Simultaneous Changes in Temperature and pH. PLoS ONE 6:e25024 
#Horwitz R, Fine M (2014) High CO2 detrimentally affects tissue regeneration of Red Sea corals. Coral Reefs 33:819-829 
#Kaniewska P, Campbell PR, Kline DI, Rodriguez-Lanetty M, Miller DJ, Dove S, Hoegh-Guldberg O (2012) Major Cellular and Physiological Impacts of Ocean Acidification on a Reef Building Coral. PLoS ONE 7:e34659 
#Kavousi J, Reimer JD, Tanaka Y, Nakamura T (2015) Colony-specific investigations reveal highly variable responses among individual corals to ocean acidification and warming. Mar Environ Res 109:9-20 
#Krief S, Hendy EJ, Fine M, Yam R, Meibom A, Foster GL, Shemesh A (2010) Physiological and isotopic responses of scleractinian corals to ocean acidification. Geochim Cosmochim Acta 74:4988-5001 
#Krueger T, Horwitz N, Bodin J, Giovani M-E, Escrig S, Meibom A, Fine M (2017) Common reef-building coral in the Northern Red Sea resistant to elevated temperature and acidification. R Soc Open Sci 4:170038
#Ogawa D, Bobeszko T, Ainsworth T, Leggat W (2013) The combined effects of temperature and CO2 lead to altered gene expression in Acropora aspera. Coral Reefs 32:895-907 
#Schoepf V, Grottoli AG, Warner ME, Cai W-J, Melman TF, Hoadley KD, Pettay DT, Hu X, Li Q, Xu H, Wang Y, Matsui Y, Baumann JH (2013) Coral Energy Reserves and Calcification in a High-CO2 World at Two Temperatures. PLoS ONE 8:e75049 
#Takahashi A, Kurihara H (2013) Ocean acidification does not affect the physiology of the tropical coral Acropora digitifera during a 5-week experiment. Coral Reefs 32:305-314 
#Tremblay P, Fine M, Maguer JF, Grover R, Ferrier-Pagès C (2013) Photosynthate translocation increases in response to low seawater pH in a coral-dinoflagellate symbiosis. Biogeosciences 10:3997-4007 
#Wall CB, Mason RAB, Ellis WR, Cunning R, Gates RD (2017) Elevated pCO2 affects tissue biomass composition, but not calcification, in a reef coral under two light regimes. R Soc Open Sci 4:170683 
