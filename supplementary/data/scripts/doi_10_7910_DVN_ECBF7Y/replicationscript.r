#Final Version Ships Code
rm(list=ls())
library(lmtest)
library(sandwich)
doBootstrapping<-FALSE
setwd("~/Desktop/HoldingPen/TeXFiles")
rawData<-read.csv("RawData.csv",stringsAsFactors=F)
#In text reference to number of battleslo
write(nrow(rawData),"numbattles.tex")

#Battle Type
#Code Mutual Battles
rawData<-data.frame(rawData,"mutual.Battle"=rawData[,"Flight.Available.British"]=="Yes"&rawData[,"Flight.Available.Opponent"]=="Yes"&rawData[,"Flight.Attempted.Opponent"]=="No"&rawData[,"Flight.Attempted.British"]=="No")
#Code Chase Battles
rawData<-data.frame(rawData,"chase.Battle"=rawData[,"Flight.Attempted.Opponent"]=="Yes"|rawData[,"Flight.Attempted.British"]=="Yes")

#Produce Table of Battle Types
#Count of mutual battles
write(sum(rawData[,"mutual.Battle"]),"mutualcount.tex")
#Count of chase battles
write(sum(rawData[,"chase.Battle"],na.rm=T),"chasecount.tex")
#Count of convoy escort
write(sum(rawData[,"Flight.Available.British"]=="Convoy Escort"|rawData[,"Flight.Available.Opponent"]=="Convoy Escort"),"convoycount.tex")
#Count of trapped battles
write(sum(rawData[,"Flight.Available.British"]=="Trapped"|rawData[,"Flight.Available.Opponent"]=="Trapped"),"trappedcount.tex")
#Count of under orders
write(sum(rawData[,"Flight.Available.British"]=="Under Orders"|rawData[,"Flight.Available.Opponent"]=="Under Orders"),"orderscount.tex")

#Convert to numeric
rawData[,"British.Line"]<-as.numeric(rawData[,"British.Line"])
rawData[,"British.Frigates"]<-as.numeric(rawData[,"British.Frigates"])
rawData[,"British.Guns"]<-as.numeric(rawData[,"British.Guns"])
rawData[,"British.MenOfWar"]<-as.numeric(rawData[,"British.MenOfWar"])
rawData[,"British.ShipsLost"]<-as.numeric(rawData[,"British.ShipsLost"])
rawData[,"Opponent.Line"]<-as.numeric(rawData[,"Opponent.Line"])
rawData[,"Opponent.Frigates"]<-as.numeric(rawData[,"Opponent.Frigates"])
rawData[,"Opponent.Guns"]<-as.numeric(rawData[,"Opponent.Guns"])
rawData[,"Opponent.MenOfWar"]<-as.numeric(rawData[,"Opponent.MenOfWar"])
rawData[,"Opponent.ShipsLost"]<-as.numeric(rawData[,"Opponent.ShipsLost"])

#Report number of cases in which guns are available
write(sum(!is.na(rawData[,"British.Guns"])&!is.na(rawData[,"Opponent.Guns"])),"gunnumber.tex")
#Cases without guns
noGuns<-rawData[is.na(rawData[,"British.Guns"])|is.na(rawData[,"Opponent.Guns"]),]
write(sum(!is.na(noGuns[,"British.Line"])&!is.na(noGuns[,"Opponent.Line"])),"linenumber.tex")
write(nrow(noGuns)-sum(!is.na(noGuns[,"British.Line"])&!is.na(noGuns[,"Opponent.Line"])),"mownumber.tex")

#Compute Integrated Balance of Power Measure
balanceOfPower<-rawData[,"British.Guns"]/(rawData[,"British.Guns"]+rawData[,"Opponent.Guns"])
balanceOfPower[is.na(balanceOfPower)]<-rawData[is.na(balanceOfPower),"British.Line"]/(rawData[is.na(balanceOfPower),"British.Line"]+rawData[is.na(balanceOfPower),"Opponent.Line"])
balanceOfPower[is.na(balanceOfPower)]<-rawData[is.na(balanceOfPower),"British.MenOfWar"]/(rawData[is.na(balanceOfPower),"British.MenOfWar"]+rawData[is.na(balanceOfPower),"Opponent.MenOfWar"])
#Compute Integrated Ship Count Measure
britishShips<-rawData[,"British.MenOfWar"]
britishShips[is.na(britishShips)]<-rawData[is.na(britishShips),"British.Line"]+ifelse(!is.na(rawData[is.na(britishShips),"British.Frigates"]),rawData[is.na(britishShips),"British.Frigates"],0)
opponentShips<-rawData[,"Opponent.MenOfWar"]
opponentShips[is.na(opponentShips)]<-rawData[is.na(opponentShips),"Opponent.Line"]+ifelse(!is.na(rawData[is.na(opponentShips),"Opponent.Frigates"]),rawData[is.na(opponentShips),"Opponent.Frigates"],0)


#Create the Disputant Level Dataset with appropriate variables
disputantData<-rbind(data.frame("battle"=rawData[,"Battle"],"ID"=rawData[,"ID"],"war"=rawData[,"War"],"admiral"=rawData[,"British.Admiral"],"opponent"=rawData[,"Opponent"],"year"=rawData[,"Year"],"balanceOfPower"=balanceOfPower,"adversaryLosses"=rawData[,"Opponent.ShipsLost"],"adversaryShips"=opponentShips,"mutual"=rawData[,"mutual.Battle"],"british"=TRUE),data.frame("battle"=rawData[,"Battle"],"ID"=rawData[,"ID"],"war"=rawData[,"War"],"admiral"=rawData[,"British.Admiral"],"opponent"=rawData[,"Opponent"],"year"=rawData[,"Year"],"balanceOfPower"=1-balanceOfPower,"adversaryLosses"=rawData[,"British.ShipsLost"],"adversaryShips"=britishShips,"mutual"=rawData[,"mutual.Battle"],"british"=FALSE))
disputantData<-data.frame(disputantData,"lossesImposedProp"=disputantData[,"adversaryLosses"]/disputantData[,"adversaryShips"])

#Report descriptive outcome stats
write(min(disputantData[,"adversaryLosses"]),"minlosses.tex")
write(max(disputantData[,"adversaryLosses"]),"maxlosses.tex")
write(round(mean(disputantData[,"adversaryLosses"]),digits=1),"meanlosses.tex")
write(round(sd(disputantData[,"adversaryLosses"]),digits=1),"sdlosses.tex")

#Report historiographical codings
write(sum(rawData[,"Outcome"]=="Decisive Opponent Victory"),"decloss.tex")
write(sum(rawData[,"Outcome"]=="Opponent Victory"),"loss.tex")
write(sum(rawData[,"Outcome"]=="Inconclusive"),"inconclusive.tex")
write(sum(rawData[,"Outcome"]=="British Victory"),"victory.tex")
write(sum(rawData[,"Outcome"]=="Decisive British Victory"),"decvictory.tex")

#Determine for test of H1 from perspective of chooser/chaser
britishChooser<-rawData[,"Flight.Available.British"]=="Yes"&(rawData[,"Flight.Attempted.British"]=="No"|is.na(rawData[,"Flight.Attempted.British"]))
histCoding<-as.numeric(ifelse(britishChooser,factor(rawData[,"Outcome"],c("Decisive Opponent Victory","Opponent Victory","Inconclusive","British Victory","Decisive British Victory")),factor(rawData[,"Outcome"],c("Decisive British Victory","British Victory","Inconclusive","Opponent Victory","Decisive Opponent Victory"))))-3
netOpponentLosses<-ifelse(britishChooser,rawData[,"Opponent.ShipsLost"]-rawData[,"British.ShipsLost"],rawData[,"British.ShipsLost"]-rawData[,"Opponent.ShipsLost"])
normOpponentLosses<-ifelse(britishChooser,netOpponentLosses/opponentShips,netOpponentLosses/britishShips)

#Create table to test H1
t.test(histCoding[!rawData[,"mutual.Battle"]])
write(paste0(round(t.test(histCoding[!rawData[,"mutual.Battle"]])$estimate,1)," & ",round(t.test(histCoding[!rawData[,"mutual.Battle"]])$conf.int[1],1)," - ",round(t.test(histCoding[!rawData[,"mutual.Battle"]])$conf.int[2],1)),"histtest.tex")
t.test(netOpponentLosses[!rawData[,"mutual.Battle"]])
write(paste0(round(t.test(netOpponentLosses[!rawData[,"mutual.Battle"]])$estimate,1)," & ",round(t.test(netOpponentLosses[!rawData[,"mutual.Battle"]])$conf.int[1],1)," - ",round(t.test(netOpponentLosses[!rawData[,"mutual.Battle"]])$conf.int[2],1)),"lossestest.tex")
t.test(normOpponentLosses[!rawData[,"mutual.Battle"]])
write(paste0(round(t.test(normOpponentLosses[!rawData[,"mutual.Battle"]])$estimate,2)," & ",round(t.test(normOpponentLosses[!rawData[,"mutual.Battle"]])$conf.int[1],2)," - ",round(t.test(normOpponentLosses[!rawData[,"mutual.Battle"]])$conf.int[2],2)),"normtest.tex")

#Regression test of H2, using the disputant-level dataset
#OLS Estimates for Column 1
#Data frame with only unilateral data
unilateral<-disputantData[!disputantData[,"mutual"],]
ols.estimate.unilateral <- lm(lossesImposedProp~balanceOfPower,data=unilateral)
nbattles<-nrow(unilateral)/2
#Run bootstrap to obtain SEs
doBootstrapUnilateral<-function(dummy)
{
	tosample<-sample(1:nbattles,nbattles,replace=T)
	tosample<-c(tosample,tosample+59)
	return(coef(lm(lossesImposedProp~balanceOfPower,data=unilateral[tosample,])))
}
#25000 bootstrap samples
library(parallel)

if (doBootstrapping) {
	bootstrapResUnilateral<-do.call(rbind,mclapply(1:25000,doBootstrapUnilateral,mc.cores=3))
save(bootstrapResUnilateral,file="bootstrapResUnilateral.RData")
} else {
load("bootstrapResUnilateral.RData") }

write(formatC(coef(ols.estimate.unilateral)[2],digits=2,format="f"),"bopCol1.tex")
write(formatC(coef(ols.estimate.unilateral)[1],digits=2,format="f"),"interceptCol1.tex")
write(paste0("(",formatC(quantile(bootstrapResUnilateral[,2],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResUnilateral[,2],.975),digits=2,format="f"),")"),"bopCol1CI.tex")
write(paste0("(",formatC(quantile(bootstrapResUnilateral[,1],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResUnilateral[,1],.975),digits=2,format="f"),")"),"interceptCol1CI.tex")
write(nbattles,"nbattlesUni.tex")
write(formatC(summary(lm(lossesImposedProp~balanceOfPower,data=unilateral))$r.squared,digits=2,format="f"),"r2model1.tex")


#Compute the non-linear model with estimated gamma parameter
#Construct appropriate, minimal dataset
useGuns<-(!is.na(rawData[,"British.Guns"]))&(!is.na(rawData[,"Opponent.Guns"]))
useLine<-(!useGuns)&(!is.na(rawData[,"British.Line"]))&(!is.na(rawData[,"Opponent.Line"]))
useMOW<-(!useGuns)&(!useLine)
dataForGammaEstimation<-data.frame("britishLosses"=rawData[,"British.ShipsLost"]/britishShips,"oppLosses"=rawData[,"Opponent.ShipsLost"]/opponentShips,"britishStrength"=NA,"oppStrength"=NA,"mutual"=rawData[,"mutual.Battle"])
dataForGammaEstimation[useGuns,"britishStrength"]<-rawData[useGuns,"British.Guns"]
dataForGammaEstimation[useLine,"britishStrength"]<-rawData[useLine,"British.Line"]
dataForGammaEstimation[useMOW,"britishStrength"]<-rawData[useMOW,"British.MenOfWar"]
dataForGammaEstimation[useGuns,"oppStrength"]<-rawData[useGuns,"Opponent.Guns"]
dataForGammaEstimation[useLine,"oppStrength"]<-rawData[useLine,"Opponent.Line"]
dataForGammaEstimation[useMOW,"oppStrength"]<-rawData[useMOW,"Opponent.MenOfWar"]



#Computes the sume of squares for the non-linear model
sumSquaresUnilateral<-function(parameters,whichRows)
{
	betaBoP<-parameters[1]
    gammaNotBritish<-parameters[2]
    betaBritish<-parameters[3]
    betaNotBritish<-parameters[4]
    adjustedBoP <- (dataForGammaEstimation[whichRows,"britishStrength"])/(dataForGammaEstimation[whichRows,"britishStrength"] + gammaNotBritish * dataForGammaEstimation[whichRows,"oppStrength"])
    residual1<-dataForGammaEstimation[whichRows,"oppLosses"]- (betaBritish+betaBoP*adjustedBoP)
    residual2<-dataForGammaEstimation[whichRows,"britishLosses"] - (betaNotBritish + betaBoP*(1-adjustedBoP))
    return(sum(c(residual1^2,residual2^2)))
}

#In point of fact, the problem can be restated for unidimensional optimization - that is, given a value of the gamma parameter, it is possible to compute the remaining coefficients directly via OLS.  This function leverages this by computing the solution for a given gamma value, which can then be passed to "optimize" via the wrapper function below
partialMin<-function(gnb,whichRows)
{
	adjustedBoP <- (dataForGammaEstimation[whichRows,"britishStrength"])/(dataForGammaEstimation[whichRows,"britishStrength"] + gnb * dataForGammaEstimation[whichRows,"oppStrength"])
	someData<-data.frame("bop"=c(adjustedBoP,1-adjustedBoP),"losses"=c(dataForGammaEstimation[whichRows,"oppLosses"],dataForGammaEstimation[whichRows,"britishLosses"]),"british"=c(rep(T,length(adjustedBoP)),rep(F,length(adjustedBoP))))
	theseCoef<-coef(lm(losses~0+bop+british,data=someData))
	return(c(theseCoef[1],gnb,theseCoef[3],theseCoef[2]))	
}
#Wrapper function for use with optimize
unidimensionalVersion<-function(gnb,whichRows)
{
	return(sumSquaresUnilateral(partialMin(gnb,whichRows),whichRows))
}
bestGamma<-optimize(unidimensionalVersion,c(-3,3),whichRows=which(dataForGammaEstimation[,"mutual"]==F))
estimatesCol2<-partialMin(bestGamma$minimum,whichRows=which(dataForGammaEstimation[,"mutual"]==F))

#Function to conduct bootstrapping
doBootstrapUnilateral2<-function(dummy)
{
	tosample<-sample(which(dataForGammaEstimation[,"mutual"]==F),length(which(dataForGammaEstimation[,"mutual"]==F)),replace=T)
	bg<-optimize(unidimensionalVersion,c(-3,3),whichRows=tosample)
	return(partialMin(bg$minimum,whichRows=tosample))
}
if (doBootstrapping) { 
bootstrapResUnilateral2<-do.call(rbind,mclapply(1:25000,doBootstrapUnilateral2,mc.cores=3))
save(bootstrapResUnilateral2,file="bootstrapResUnilateral2.RData")} else {
load("bootstrapResUnilateral2.RData")}

write(formatC(estimatesCol2[1],digits=2,format="f"),"bopCol2.tex")
write(paste0("(",formatC(quantile(bootstrapResUnilateral2[,1],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResUnilateral2[,1],.975),digits=2,format="f"),")"),"bopCol2CI.tex")

write(formatC(estimatesCol2[2],digits=2,format="f"),"gammaCol2.tex")
write(paste0("(",formatC(quantile(bootstrapResUnilateral2[,2],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResUnilateral2[,2],.975),digits=2,format="f"),")"),"gammaCol2CI.tex")

write(formatC(estimatesCol2[3],digits=2,format="f"),"betaBritishCol2.tex")
write(paste0("(",formatC(quantile(bootstrapResUnilateral2[,3],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResUnilateral2[,3],.975),digits=2,format="f"),")"),"betaBritishCI.tex")

write(formatC(estimatesCol2[4],digits=2,format="f"),"betaNotBritishCol2.tex")
write(paste0("(",formatC(quantile(bootstrapResUnilateral2[,4],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResUnilateral2[,4],.975),digits=2,format="f"),")"),"betaNotBritishCI.tex")


#Calculate R2 for this model
r2forNonLinear<-function(estimates,whichRows)
{
	betaBoP<-estimates[1]
	gammaNotBritish<-estimates[2]
	betaBritish<-estimates[3]
	betaNotBritish<-estimates[4]
	adjustedBoP <- (dataForGammaEstimation[whichRows,"britishStrength"])/(dataForGammaEstimation[whichRows,"britishStrength"] + gammaNotBritish * dataForGammaEstimation[whichRows,"oppStrength"])
    residual1<-dataForGammaEstimation[whichRows,"oppLosses"]- (betaBritish+betaBoP*adjustedBoP)
    residual2<-dataForGammaEstimation[whichRows,"britishLosses"] - (betaNotBritish + betaBoP*(1-adjustedBoP))
    residuals<-c(residual1,residual2)
    meanDev<-c(dataForGammaEstimation[whichRows,"britishLosses"]-mean(c(dataForGammaEstimation[whichRows,"britishLosses"],dataForGammaEstimation[whichRows,"oppLosses"])),dataForGammaEstimation[whichRows,"oppLosses"]-mean(c(dataForGammaEstimation[whichRows,"britishLosses"],dataForGammaEstimation[whichRows,"oppLosses"])))
    return(1-sum(residuals^2)/(sum(meanDev^2)))
}  	
write(formatC(r2forNonLinear(estimatesCol2,whichRows=which(dataForGammaEstimation[,"mutual"]==F)),digits=2,format="f"),"r2model2.tex")

#Battle Level Outcomes Table
library(sandwich)
library(lmtest)

rawData[,"Outcome"]<-factor(rawData[,"Outcome"],c("Decisive Opponent Victory","Opponent Victory","Inconclusive","British Victory","Decisive British Victory"))
rawData<-data.frame(rawData,"balanceOfPower"=balanceOfPower,"britishShips"=britishShips,"opponentShips"=opponentShips)
histUniCol1<-lm(as.numeric(Outcome)-2~balanceOfPower,data=rawData[!rawData[,"mutual.Battle"],])
robustErrorsUniCol1<-vcovHC(histUniCol1)
#starsUniCol1BoP<-ifelse(coeftest(histUniCol1,robustErrorsUniCol1)["balanceOfPower","Pr(>|t|)"]>.05,"",ifelse(coeftest(histUniCol1,robustErrorsUniCol1)["balanceOfPower","Pr(>|t|)"]>.01,"*",ifelse(coeftest(histUniCol1,robustErrorsUniCol1)["balanceOfPower","Pr(>|t|)"]>.001,"**","***")))
write(formatC(coeftest(histUniCol1,robustErrorsUniCol1)["balanceOfPower","Estimate"],digits=2,format="f"),"histUniCol1BoP.tex")
write(paste0("(",formatC(coeftest(histUniCol1,robustErrorsUniCol1)["balanceOfPower","Std. Error"],digits=2,format="f"),")"),file="histUniCol1BoPSE.tex")
write(formatC(coeftest(histUniCol1,robustErrorsUniCol1)["(Intercept)","Estimate"],digits=2,format="f"),"histUniCol1int.tex")
write(paste0("(",formatC(coeftest(histUniCol1,robustErrorsUniCol1)["(Intercept)","Std. Error"],digits=2,format="f"),")"),file="histUniCol1intSE.tex")
write(formatC(summary(histUniCol1)$r.squared,digits=2,format="f"),"histUniCol1R2.tex")

lossesUniCol2<-lm(Opponent.ShipsLost-British.ShipsLost~balanceOfPower,data=rawData[!rawData[,"mutual.Battle"],])
robustErrorsUniCol2<-vcovHC(lossesUniCol2)
write(formatC(coeftest(lossesUniCol2,robustErrorsUniCol2)["balanceOfPower","Estimate"],digits=2,format="f"),"netUniCol2BoP.tex")
write(paste0("(",formatC(coeftest(lossesUniCol2,robustErrorsUniCol2)["balanceOfPower","Std. Error"],digits=2,format="f"),")"),file="netUniCol2BoPSE.tex")
write(formatC(coeftest(lossesUniCol2,robustErrorsUniCol2)["(Intercept)","Estimate"],digits=2,format="f"),"netUniCol2int.tex")
write(paste0("(",formatC(coeftest(lossesUniCol2,robustErrorsUniCol2)["(Intercept)","Std. Error"],digits=2,format="f"),")"),file="netUniCol2intSE.tex")
write(formatC(summary(lossesUniCol2)$r.squared,digits=2,format="f"),"netUniCol2R2.tex")


lossesUniCol3<-lm((Opponent.ShipsLost-British.ShipsLost)/opponentShips~balanceOfPower,data=rawData[!rawData[,"mutual.Battle"],])
robustErrorsUniCol3<-vcovHC(lossesUniCol3)
write(formatC(coeftest(lossesUniCol3,robustErrorsUniCol3)["balanceOfPower","Estimate"],digits=2,format="f"),"netUniCol3BoP.tex")
write(paste0("(",formatC(coeftest(lossesUniCol3,robustErrorsUniCol3)["balanceOfPower","Std. Error"],digits=2,format="f"),")"),file="netUniCol3BoPSE.tex")
write(formatC(coeftest(lossesUniCol3,robustErrorsUniCol3)["(Intercept)","Estimate"],digits=2,format="f"),"netUniCol3int.tex")
write(paste0("(",formatC(coeftest(lossesUniCol3,robustErrorsUniCol3)["(Intercept)","Std. Error"],digits=2,format="f"),")"),file="netUniCol3intSE.tex")
write(formatC(summary(lossesUniCol3)$r.squared,digits=2,format="f"),"netUniCol3R2.tex")


#Model of Mutual Battles Only in the Dyadic Set (with Bootstrapping)
#Regression test of H2, using the disputant-level dataset
#OLS Estimates for Column 1
#Data frame with only unilateral data
mutual<-disputantData[disputantData[,"mutual"],]
ols.estimate.mutual <- lm(lossesImposedProp~balanceOfPower,data=mutual)
nbattlesM<-nrow(mutual)/2
#Run bootstrap to obtain SEs
doBootstrapMutual<-function(dummy)
{
	tosample<-sample(1:nbattlesM,nbattlesM,replace=T)
	tosample<-c(tosample,tosample+nbattlesM)
	return(coef(lm(lossesImposedProp~balanceOfPower,data=mutual[tosample,])))
}
#25000 bootstrap samples
library(parallel)
if (doBootstrapping) {
bootstrapResMutual<-do.call(rbind,mclapply(1:25000,doBootstrapMutual,mc.cores=3))
save(bootstrapResMutual,file="bootstrapResMutual.RData") } else {
load("bootstrapResMutual.RData")}

write(formatC(coef(ols.estimate.mutual)[2],digits=2,format="f"),"bopMutual.tex")
write(formatC(coef(ols.estimate.mutual)[1],digits=2,format="f"),"interceptMutual.tex")
write(paste0("(",formatC(quantile(bootstrapResMutual[,2],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResMutual[,2],.975),digits=2,format="f"),")"),"bopMutualCI.tex")
write(paste0("(",formatC(quantile(bootstrapResMutual[,1],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResMutual[,1],.975),digits=2,format="f"),")"),"interceptMutualCI.tex")
write(nbattlesM,"nbattlesMutual.tex")
write(formatC(summary(ols.estimate.mutual)$r.squared,digits=3,format="f"),"r2mutual.tex")

#Model with interaction term
interactionModel<-lm(lossesImposedProp~balanceOfPower*mutual,data=disputantData)
nbattlesTotal<-nrow(disputantData)/2
doBootstrapInteraction1<-function(dummy)
{
	tosample<-sample(1:nbattlesTotal,nbattlesTotal,replace=T)
	tosample<-c(tosample,tosample+nbattlesTotal)
	return(coef(lm(lossesImposedProp~balanceOfPower*mutual,data=disputantData[tosample,])))
}
if (doBootstrapping) {
bootstrapResInteraction1<-do.call(rbind,mclapply(1:25000,doBootstrapInteraction1,mc.cores=3))
save(bootstrapResInteraction1,file="bootstrapResInteraction1.RData") } else {
load("bootstrapResInteraction1.RData") }

write(formatC(coef(interactionModel)[2],digits=2,format="f"),"bopInteraction1.tex")
write(formatC(coef(interactionModel)[1],digits=2,format="f"),"interceptInteraction1.tex")
write(formatC(coef(interactionModel)[3],digits=2,format="f"),"mutualInteraction1.tex")
write(formatC(coef(interactionModel)[4],digits=2,format="f"),"bopmutualInteraction1.tex")

write(paste0("(",formatC(quantile(bootstrapResInteraction1[,2],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResInteraction1[,2],.975),digits=2,format="f"),")"),"bopInteraction1CI.tex")
write(paste0("(",formatC(quantile(bootstrapResInteraction1[,1],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResInteraction1[,1],.975),digits=2,format="f"),")"),"interceptInteraction1CI.tex")
write(paste0("(",formatC(quantile(bootstrapResInteraction1[,3],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResInteraction1[,3],.975),digits=2,format="f"),")"),"mutualInteraction1CI.tex")
write(paste0("(",formatC(quantile(bootstrapResInteraction1[,4],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResInteraction1[,4],.975),digits=2,format="f"),")"),"bopmutualInteraction1CI.tex")
write(nbattlesTotal,"nbattlesTotal.tex")
write(formatC(summary(interactionModel)$r.squared,digits=2,format="f"),"r2modelInteraction1.tex")

#Non-linear specification with gamma parameter


#Computes the sume of squares for the non-linear model
sumSquaresInteraction<-function(parameters,whichRows)
{
	betaBoP<-parameters[1]
    gammaNotBritish<-parameters[2]
    betaBritish<-parameters[3]
    betaNotBritish<-parameters[4]
    betaMutual<-parameters[5]
    betaInteraction<-parameters[6]
    
    adjustedBoP <- (dataForGammaEstimation[whichRows,"britishStrength"])/(dataForGammaEstimation[whichRows,"britishStrength"] + gammaNotBritish * dataForGammaEstimation[whichRows,"oppStrength"])
    
    residual1<-dataForGammaEstimation[whichRows,"oppLosses"]- (betaBritish+betaBoP*adjustedBoP+betaMutual*dataForGammaEstimation[whichRows,"mutual"] +betaInteraction*adjustedBoP*dataForGammaEstimation[whichRows,"mutual"])
    
    residual2<-dataForGammaEstimation[whichRows,"britishLosses"] - (betaNotBritish + betaBoP*(1-adjustedBoP)+betaMutual*dataForGammaEstimation[whichRows,"mutual"] + betaInteraction*(1-adjustedBoP)*dataForGammaEstimation[whichRows,"mutual"])
    
    return(sum(c(residual1^2,residual2^2)))
}

#In point of fact, the problem can be restated for unidimensional optimization - that is, given a value of the gamma parameter, it is possible to compute the remaining coefficients directly via OLS.  This function leverages this by computing the solution for a given gamma value, which can then be passed to "optimize" via the wrapper function below
partialMinInteraction<-function(gnb,whichRows)
{
	adjustedBoP <- (dataForGammaEstimation[whichRows,"britishStrength"])/(dataForGammaEstimation[whichRows,"britishStrength"] + gnb * dataForGammaEstimation[whichRows,"oppStrength"])
	someData<-data.frame("bop"=c(adjustedBoP,1-adjustedBoP),"losses"=c(dataForGammaEstimation[whichRows,"oppLosses"],dataForGammaEstimation[whichRows,"britishLosses"]),"british"=c(rep(T,length(adjustedBoP)),rep(F,length(adjustedBoP))),"mutual"=c(dataForGammaEstimation[whichRows,"mutual"],dataForGammaEstimation[whichRows,"mutual"]))
	theseCoef<-coef(lm(losses~0+british+bop*mutual,data=someData))
	return(c(theseCoef[3],gnb,theseCoef[2],theseCoef[1],theseCoef[4],theseCoef[5]))	
}

#Wrapper function for use with optimize
interactionVersion<-function(gnb,whichRows)
{
	return(sumSquaresInteraction(partialMinInteraction(gnb,whichRows),whichRows))
}
bestGammaI<-optimize(interactionVersion,c(-3,3),whichRows=1:nrow(dataForGammaEstimation))
estimatesInteraction<-partialMinInteraction(bestGammaI$minimum,whichRows=1:nrow(dataForGammaEstimation))

#Function to conduct bootstrapping
doBootstrapInteraction2<-function(dummy)
{
	tosample<-sample(1:nrow(dataForGammaEstimation),nrow(dataForGammaEstimation),replace=T)
	bg<-optimize(interactionVersion,c(-3,3),whichRows=tosample)
	return(partialMinInteraction(bg$minimum,whichRows=tosample))
}
if (doBootstrapping) {
bootstrapResInteraction2<-do.call(rbind,mclapply(1:25000,doBootstrapInteraction2,mc.cores=3))
save(bootstrapResInteraction2,file="bootstrapResInteraction2.RData") } else {
load("bootstrapResInteraction2.RData") }


write(formatC(estimatesInteraction[1],digits=2,format="f"),"bopInteraction2.tex")
write(formatC(estimatesInteraction[2],digits=2,format="f"),"gammaInteraction2.tex")
write(formatC(estimatesInteraction[3],digits=2,format="f"),"britishInteraction2.tex")
write(formatC(estimatesInteraction[4],digits=2,format="f"),"notBritishInteraction2.tex")
write(formatC(estimatesInteraction[5],digits=2,format="f"),"mutualInteraction2.tex")
write(formatC(estimatesInteraction[6],digits=2,format="f"),"bopmutualInteraction2.tex")

write(paste0("(",formatC(quantile(bootstrapResInteraction2[,1],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResInteraction2[,1],.975),digits=2,format="f"),")"),"bopInteraction2CI.tex")
write(paste0("(",formatC(quantile(bootstrapResInteraction2[,2],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResInteraction2[,2],.975),digits=2,format="f"),")"),"gammaInteraction2CI.tex")
write(paste0("(",formatC(quantile(bootstrapResInteraction2[,3],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResInteraction2[,3],.975),digits=2,format="f"),")"),"britishInteraction2CI.tex")
write(paste0("(",formatC(quantile(bootstrapResInteraction2[,4],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResInteraction2[,4],.975),digits=2,format="f"),")"),"notBritishInteraction2CI.tex")
write(paste0("(",formatC(quantile(bootstrapResInteraction2[,5],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResInteraction2[,5],.975),digits=2,format="f"),")"),"mutualInteraction2CI.tex")
write(paste0("(",formatC(quantile(bootstrapResInteraction2[,6],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResInteraction2[,6],.975),digits=2,format="f"),")"),"bopmutualInteraction2CI.tex")
write(nbattlesTotal,"nbattlesTotal.tex")

r2forInteraction<-function(parameters,whichRows)
{
	betaBoP<-parameters[1]
    gammaNotBritish<-parameters[2]
    betaBritish<-parameters[3]
    betaNotBritish<-parameters[4]
    betaMutual<-parameters[5]
    betaInteraction<-parameters[6]
    
    adjustedBoP <- (dataForGammaEstimation[whichRows,"britishStrength"])/(dataForGammaEstimation[whichRows,"britishStrength"] + gammaNotBritish * dataForGammaEstimation[whichRows,"oppStrength"])
    
    residual1<-dataForGammaEstimation[whichRows,"oppLosses"]- (betaBritish+betaBoP*adjustedBoP+betaMutual*dataForGammaEstimation[whichRows,"mutual"] +betaInteraction*adjustedBoP*dataForGammaEstimation[whichRows,"mutual"])
    
    residual2<-dataForGammaEstimation[whichRows,"britishLosses"] - (betaNotBritish + betaBoP*(1-adjustedBoP)+betaMutual*dataForGammaEstimation[whichRows,"mutual"] + betaInteraction*(1-adjustedBoP)*dataForGammaEstimation[whichRows,"mutual"])
    

    residuals<-c(residual1,residual2)
    meanDev<-c(dataForGammaEstimation[whichRows,"britishLosses"]-mean(c(dataForGammaEstimation[whichRows,"britishLosses"],dataForGammaEstimation[whichRows,"oppLosses"])),dataForGammaEstimation[whichRows,"oppLosses"]-mean(c(dataForGammaEstimation[whichRows,"britishLosses"],dataForGammaEstimation[whichRows,"oppLosses"])))
    return(1-sum(residuals^2)/(sum(meanDev^2)))
}  	
write(formatC(r2forInteraction(estimatesInteraction,whichRows=1:nrow(dataForGammaEstimation)),digits=2,format="f"),"r2modelInteraction2.tex")



histIntCol1<-lm(as.numeric(Outcome)-2~balanceOfPower*mutual.Battle,data=rawData)
write(formatC(coeftest(histIntCol1,vcovHC(histIntCol1))["balanceOfPower","Estimate"],digits=2,format="f"),"histIntCol1BoP.tex")
write(paste0("(",formatC(coeftest(histIntCol1,vcovHC(histIntCol1))["balanceOfPower","Std. Error"],digits=2,format="f"),")"),file="histIntCol1BoPSE.tex")
histIntCol1<-lm(as.numeric(Outcome)-2~balanceOfPower*mutual.Battle,data=rawData)
write(formatC(coeftest(histIntCol1,vcovHC(histIntCol1))["(Intercept)","Estimate"],digits=2,format="f"),"histIntCol1Intercept.tex")
write(paste0("(",formatC(coeftest(histIntCol1,vcovHC(histIntCol1))["(Intercept)","Std. Error"],digits=2,format="f"),")"),file="histIntCol1InterceptSE.tex")
write(formatC(coeftest(histIntCol1,vcovHC(histIntCol1))["mutual.BattleTRUE","Estimate"],digits=2,format="f"),"histIntCol1Mutual.tex")
write(paste0("(",formatC(coeftest(histIntCol1,vcovHC(histIntCol1))["mutual.BattleTRUE","Std. Error"],digits=2,format="f"),")"),file="histIntCol1MutualSE.tex")
write(formatC(coeftest(histIntCol1,vcovHC(histIntCol1))["balanceOfPower:mutual.BattleTRUE","Estimate"],digits=2,format="f"),"histIntCol1BoPMutual.tex")
write(paste0("(",formatC(coeftest(histIntCol1,vcovHC(histIntCol1))["balanceOfPower:mutual.BattleTRUE","Std. Error"],digits=2,format="f"),")"),file="histIntCol1BoPMutualSE.tex")
write(formatC(summary(histIntCol1)$r.squared,digits=2,format="f"),file="histIntCol1r2.tex")

histIntCol2<-lm(Opponent.ShipsLost-British.ShipsLost~balanceOfPower*mutual.Battle,data=rawData)
write(formatC(coeftest(histIntCol2,vcovHC(histIntCol2))["balanceOfPower","Estimate"],digits=2,format="f"),"histIntCol2BoP.tex")
write(paste0("(",formatC(coeftest(histIntCol2,vcovHC(histIntCol2))["balanceOfPower","Std. Error"],digits=2,format="f"),")"),file="histIntCol2BoPSE.tex")
write(formatC(coeftest(histIntCol2,vcovHC(histIntCol2))["(Intercept)","Estimate"],digits=2,format="f"),"histIntCol2Intercept.tex")
write(paste0("(",formatC(coeftest(histIntCol2,vcovHC(histIntCol2))["(Intercept)","Std. Error"],digits=2,format="f"),")"),file="histIntCol2InterceptSE.tex")
write(formatC(coeftest(histIntCol2,vcovHC(histIntCol2))["mutual.BattleTRUE","Estimate"],digits=2,format="f"),"histIntCol2Mutual.tex")
write(paste0("(",formatC(coeftest(histIntCol2,vcovHC(histIntCol2))["mutual.BattleTRUE","Std. Error"],digits=2,format="f"),")"),file="histIntCol2MutualSE.tex")
write(formatC(coeftest(histIntCol2,vcovHC(histIntCol2))["balanceOfPower:mutual.BattleTRUE","Estimate"],digits=2,format="f"),"histIntCol2BoPMutual.tex")
write(paste0("(",formatC(coeftest(histIntCol2,vcovHC(histIntCol2))["balanceOfPower:mutual.BattleTRUE","Std. Error"],digits=2,format="f"),")"),file="histIntCol2BoPMutualSE.tex")
write(formatC(summary(histIntCol2)$r.squared,digits=2,format="f"),file="histIntCol2r2.tex")

histIntCol3<-lm((Opponent.ShipsLost-British.ShipsLost)/opponentShips~balanceOfPower*mutual.Battle,data=rawData)
write(formatC(coeftest(histIntCol3,vcovHC(histIntCol3))["balanceOfPower","Estimate"],digits=2,format="f"),"histIntCol3BoP.tex")
write(paste0("(",formatC(coeftest(histIntCol3,vcovHC(histIntCol3))["balanceOfPower","Std. Error"],digits=2,format="f"),")"),file="histIntCol3BoPSE.tex")
write(formatC(coeftest(histIntCol3,vcovHC(histIntCol3))["(Intercept)","Estimate"],digits=2,format="f"),"histIntCol3Intercept.tex")
write(paste0("(",formatC(coeftest(histIntCol3,vcovHC(histIntCol3))["(Intercept)","Std. Error"],digits=2,format="f"),")"),file="histIntCol3InterceptSE.tex")
write(formatC(coeftest(histIntCol3,vcovHC(histIntCol3))["mutual.BattleTRUE","Estimate"],digits=2,format="f"),"histIntCol3Mutual.tex")
write(paste0("(",formatC(coeftest(histIntCol3,vcovHC(histIntCol3))["mutual.BattleTRUE","Std. Error"],digits=2,format="f"),")"),file="histIntCol3MutualSE.tex")
write(formatC(coeftest(histIntCol3,vcovHC(histIntCol3))["balanceOfPower:mutual.BattleTRUE","Estimate"],digits=2,format="f"),"histIntCol3BoPMutual.tex")
write(paste0("(",formatC(coeftest(histIntCol3,vcovHC(histIntCol3))["balanceOfPower:mutual.BattleTRUE","Std. Error"],digits=2,format="f"),")"),file="histIntCol3BoPMutualSE.tex")
write(formatC(summary(histIntCol3)$r.squared,digits=2,format="f"),file="histIntCol3r2.tex")



#Make the combo plot
plot(lossesImposedProp~balanceOfPower,xlim=c(0,1),ylim=c(0,1),xaxp=c(0,1,4),col=ifelse(mutual==T,"red","blue"),pch=20,data=disputantData,xlab="Balance of Power",ylab="Losses Imposed (Proportion)",main="")
abline(ols.estimate.mutual,col="red",lty="dashed")
abline(ols.estimate.unilateral,col="blue",lty="dashed")
abline(v=0.5,lty="dotted")
legend("topleft",c("Mutual","Unilateral"),lty=c("dashed","dashed"),col=c("red","blue"))
write(formatC(cor.test(disputantData[disputantData[,"mutual"],"balanceOfPower"],disputantData[disputantData[,"mutual"],"lossesImposedProp"],method="spearman")$estimate,digits=2,format="f"),file="spearmanMutual.tex")
write(formatC(cor.test(disputantData[!disputantData[,"mutual"],"balanceOfPower"],disputantData[!disputantData[,"mutual"],"lossesImposedProp"],method="spearman")$estimate,digits=2,format="f"),file="spearmanUnilateral.tex")


#Replication of Baseline Model Dropping Unilateral Battles Outside the Range of the Mutual Data

disputantDataLimited<-disputantData[disputantData[,"balanceOfPower"]>=min(disputantData[disputantData[,"mutual"],"balanceOfPower"])&disputantData[,"balanceOfPower"]<=max(disputantData[disputantData[,"mutual"],"balanceOfPower"]),]

#Model with interaction term
interactionModelLimited<-lm(lossesImposedProp~balanceOfPower*mutual,data=disputantDataLimited)
nbattlesTotalLimited<-nrow(disputantDataLimited)/2
doBootstrapInteraction1<-function(dummy)
{
	tosample<-sample(1:nbattlesTotalLimited,nbattlesTotalLimited,replace=T)
	tosample<-c(tosample,tosample+nbattlesTotalLimited)
	return(coef(lm(lossesImposedProp~balanceOfPower*mutual,data=disputantDataLimited[tosample,])))
}
if (doBootstrapping) {
bootstrapResInteraction1Limited<-do.call(rbind,mclapply(1:5000,doBootstrapInteraction1,mc.cores=3))
save(bootstrapResInteraction1Limited,file="bootstrapResInteraction1Limited.RData") } else {
load("bootstrapResInteraction1Limited.RData") }

write(formatC(coef(interactionModelLimited)[2],digits=2,format="f"),"bopInteraction1Limited.tex")
write(formatC(coef(interactionModelLimited)[1],digits=2,format="f"),"interceptInteraction1Limited.tex")
write(formatC(coef(interactionModelLimited)[3],digits=2,format="f"),"mutualInteraction1Limited.tex")
write(formatC(coef(interactionModelLimited)[4],digits=2,format="f"),"bopmutualInteraction1Limited.tex")

write(paste0("(",formatC(quantile(bootstrapResInteraction1Limited[,2],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResInteraction1Limited[,2],.975),digits=2,format="f"),")"),"bopInteraction1CILimited.tex")
write(paste0("(",formatC(quantile(bootstrapResInteraction1Limited[,1],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResInteraction1Limited[,1],.975),digits=2,format="f"),")"),"interceptInteraction1CILimited.tex")
write(paste0("(",formatC(quantile(bootstrapResInteraction1Limited[,3],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResInteraction1Limited[,3],.975),digits=2,format="f"),")"),"mutualInteraction1CILimited.tex")
write(paste0("(",formatC(quantile(bootstrapResInteraction1Limited[,4],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResInteraction1Limited[,4],.975),digits=2,format="f"),")"),"bopmutualInteraction1CILimited.tex")
write(nbattlesTotalLimited,"nbattlesTotalLimited.tex")
write(formatC(summary(interactionModelLimited)$r.squared,digits=2,format="f"),"r2modelInteraction1Limited.tex")



#Make Changes for the Appendix
shoreForces<-c("1655A","1657A","1665B","1667A","1702B","1801A","1807A")
unclearIntent<-c("1652A","1804A","1718A","1807A")
partialFlight<-c("1693A","1702A","1782G")
allQuestionable<-c(shoreForces,unclearIntent,partialFlight)

dropShore<-lm(lossesImposedProp~balanceOfPower*mutual,data=disputantData[!disputantData[,"ID"]%in%shoreForces,])
dropIntent<-lm(lossesImposedProp~balanceOfPower*mutual,data=disputantData[!disputantData[,"ID"]%in%unclearIntent,])
dropPartial<-lm(lossesImposedProp~balanceOfPower*mutual,data=disputantData[!disputantData[,"ID"]%in%partialFlight,])
dropAll<-lm(lossesImposedProp~balanceOfPower*mutual,data=disputantData[!disputantData[,"ID"]%in%allQuestionable,])

write(paste0("$",formatC(coef(dropShore)["balanceOfPower"],digits=2,format="f"),"$ & $",formatC(coef(dropIntent)["balanceOfPower"],digits=2,format="f"),"$ & $",formatC(coef(dropPartial)["balanceOfPower"],digits=2,format="f"),"$ & $",formatC(coef(dropAll)["balanceOfPower"],digits=2,format="f"),"$"),file="dropBritishShare.tex")

write(paste0("$",formatC(coef(dropShore)["mutualTRUE"],digits=2,format="f"),"$ & $",formatC(coef(dropIntent)["mutualTRUE"],digits=2,format="f"),"$ & $",formatC(coef(dropPartial)["mutualTRUE"],digits=2,format="f"),"$ & $",formatC(coef(dropAll)["mutualTRUE"],digits=2,format="f"),"$"),file="dropMutual.tex")

write(paste0("$",formatC(coef(dropShore)["balanceOfPower:mutualTRUE"],digits=2,format="f"),"$ & $",formatC(coef(dropIntent)["balanceOfPower:mutualTRUE"],digits=2,format="f"),"$ & $",formatC(coef(dropPartial)["balanceOfPower:mutualTRUE"],digits=2,format="f"),"$ & $",formatC(coef(dropAll)["balanceOfPower:mutualTRUE"],digits=2,format="f"),"$"),file="dropBOPMutual.tex")

write(paste0("$",formatC(coef(dropShore)["(Intercept)"],digits=2,format="f"),"$ & $",formatC(coef(dropIntent)["(Intercept)"],digits=2,format="f"),"$ & $",formatC(coef(dropPartial)["(Intercept)"],digits=2,format="f"),"$ & $",formatC(coef(dropAll)["(Intercept)"],digits=2,format="f"),"$"),file="dropIntercept.tex")

doBootstrapInteractionDrop<-function(dummy,toDrop)
{
	newData<-disputantData[!disputantData[,"ID"]%in%toDrop,]
	nbattlesNew<-nrow(newData)/2
	tosample<-sample(1:nbattlesNew,nbattlesNew,replace=T)
	tosample<-c(tosample,tosample+nbattlesNew)
	return(coef(lm(lossesImposedProp~balanceOfPower*mutual,data=newData[tosample,])))
}
if (doBootstrapping) {
bootstrapResDropShore<-do.call(rbind,mclapply(1:5000,doBootstrapInteractionDrop,toDrop=shoreForces,mc.cores=3))
save(bootstrapResDropShore,file="bootstrapResDropShore.RData")

bootstrapResDropIntent<-do.call(rbind,mclapply(1:5000,doBootstrapInteractionDrop,toDrop=unclearIntent,mc.cores=3))
save(bootstrapResDropShore,file="bootstrapResDropIntent.RData")

bootstrapResDropPartial<-do.call(rbind,mclapply(1:5000,doBootstrapInteractionDrop,toDrop=partialFlight,mc.cores=3))
save(bootstrapResDropPartial,file="bootstrapResDropPartial.RData")

bootstrapResDropQuestionable<-do.call(rbind,mclapply(1:5000,doBootstrapInteractionDrop,toDrop=allQuestionable,mc.cores=3))
save(bootstrapResDropQuestionable,file="bootstrapResDropQuestionable.RData")} else {
load("bootstrapResDropShore.RData") 
load("bootstrapResDropIntent.RData")
load("bootstrapResDropPartial.RData")
load("bootstrapResDropQuestionable.RData")
}


write(paste0("$(",formatC(quantile(bootstrapResDropShore[,2],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResDropShore[,2],.975),digits=2,format="f"),")$ & ","$(",formatC(quantile(bootstrapResDropIntent[,2],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResDropIntent[,2],.975),digits=2,format="f"),")$ & ","$(",formatC(quantile(bootstrapResDropPartial[,2],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResDropPartial[,2],.975),digits=2,format="f"),")$ & ","$(",formatC(quantile(bootstrapResDropQuestionable[,2],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResDropQuestionable[,2],.975),digits=2,format="f"),")$"),"dropBritishShareCI.tex")

write(paste0("$(",formatC(quantile(bootstrapResDropShore[,3],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResDropShore[,3],.975),digits=2,format="f"),")$ & ","$(",formatC(quantile(bootstrapResDropIntent[,3],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResDropIntent[,3],.975),digits=2,format="f"),")$ & ","$(",formatC(quantile(bootstrapResDropPartial[,3],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResDropPartial[,3],.975),digits=2,format="f"),")$ & ","$(",formatC(quantile(bootstrapResDropQuestionable[,3],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResDropQuestionable[,3],.975),digits=2,format="f"),")$"),"dropMutualCI.tex")

write(paste0("$(",formatC(quantile(bootstrapResDropShore[,4],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResDropShore[,4],.975),digits=2,format="f"),")$ & ","$(",formatC(quantile(bootstrapResDropIntent[,4],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResDropIntent[,4],.975),digits=2,format="f"),")$ & ","$(",formatC(quantile(bootstrapResDropPartial[,4],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResDropPartial[,4],.975),digits=2,format="f"),")$ & ","$(",formatC(quantile(bootstrapResDropQuestionable[,4],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResDropQuestionable[,4],.975),digits=2,format="f"),")$"),"dropBOPMutualCI.tex")

write(paste0("$(",formatC(quantile(bootstrapResDropShore[,1],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResDropShore[,1],.975),digits=2,format="f"),")$ & ","$(",formatC(quantile(bootstrapResDropIntent[,1],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResDropIntent[,1],.975),digits=2,format="f"),")$ & ","$(",formatC(quantile(bootstrapResDropPartial[,1],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResDropPartial[,1],.975),digits=2,format="f"),")$ & ","$(",formatC(quantile(bootstrapResDropQuestionable[,1],.025),digits=2,format="f"),", ",formatC(quantile(bootstrapResDropQuestionable[,1],.975),digits=2,format="f"),")$"),"dropInterceptCI.tex")



