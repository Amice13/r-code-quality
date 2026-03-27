rm(list=ls(all=T))
setwd("directory")

#install.packages("devtools")
#devtools::install_github("asheshrambachan/HonestDiD")

library(HonestDiD)

#main specification
coefIndex <-c(1:9)
a <-read.table(file="/Users/Laura/Box Sync/Research/ACA_Mort/empirical/acamort/indata/rdcoutput/beta.csv", sep=",", row.names=1, header=FALSE)
betahat <-as.matrix(a)
a <-read.table(file="/Users/Laura/Box Sync/Research/ACA_Mort/empirical/acamort/indata/rdcoutput/varcov.csv", sep=",", row.names=1, header=TRUE)
sigma <-as.matrix(a)
var <-diag(sigma)
stdErrors <- sqrt(var)
timeVec=c(seq(from=-6, to=-2, by=1), seq(from=0, to=3, by=1))
referencePeriod=-1
postPeriodIndices=which(timeVec>-1)
prePeriodIndices=c(seq(from=1,to=5,by=1))

MWdata_EventStudy=list(
	betahat=betahat,
	sigma=sigma,
	timeVec=timeVec,
	referencePeriod=referencePeriod,
	prePeriodIndices=prePeriodIndices,
	postPeriodIndices=postPeriodIndices,
	stdErrors=stdErrors
	)
numPrePeriods=length(MWdata_EventStudy$prePeriodIndices)
numPostPeriods=length(MWdata_EventStudy$postPeriodIndices)

#look at sensitivity around Y0 coefficient
l_vec=basisVector(1, numPostPeriods)
DeltaSD_RobustResults=createSensitivityResults(betahat=MWdata_EventStudy$betahat,
                                               sigma=MWdata_EventStudy$sigma,
                                               numPrePeriods=numPrePeriods,
                                               numPostPeriods=numPostPeriods,
                                               l_vec=l_vec)
head(DeltaSD_RobustResults)
OriginalResults=constructOriginalCS(betahat=MWdata_EventStudy$betahat,
                                    sigma=MWdata_EventStudy$sigma,
                                    numPrePeriods=numPrePeriods,
                                    numPostPeriods=numPostPeriods,
                                    l_vec=l_vec)
DeltaSD_SensitivityPlot=createSensitivityPlot(robustResults=DeltaSD_RobustResults,
                                              originalResults=OriginalResults)

pdf("mortality_y0.pdf")
DeltaSD_SensitivityPlot
dev.off()

#look at sensitivity around Y1 coefficient
l_vec=basisVector(2, numPostPeriods)
DeltaSD_RobustResults=createSensitivityResults(betahat=MWdata_EventStudy$betahat,
                                               sigma=MWdata_EventStudy$sigma,
                                               numPrePeriods=numPrePeriods,
                                               numPostPeriods=numPostPeriods,
                                               l_vec=l_vec)
head(DeltaSD_RobustResults)
OriginalResults=constructOriginalCS(betahat=MWdata_EventStudy$betahat,
                                    sigma=MWdata_EventStudy$sigma,
                                    numPrePeriods=numPrePeriods,
                                    numPostPeriods=numPostPeriods,
                                    l_vec=l_vec)
DeltaSD_SensitivityPlot=createSensitivityPlot(robustResults=DeltaSD_RobustResults,
                                              originalResults=OriginalResults)

pdf("mortality_y1.pdf")
DeltaSD_SensitivityPlot
dev.off()

#look at sensitivity around Y2 coefficient
l_vec=basisVector(3, numPostPeriods)
DeltaSD_RobustResults=createSensitivityResults(betahat=MWdata_EventStudy$betahat,
                                               sigma=MWdata_EventStudy$sigma,
                                               numPrePeriods=numPrePeriods,
                                               numPostPeriods=numPostPeriods,
                                               l_vec=l_vec)
head(DeltaSD_RobustResults)
OriginalResults=constructOriginalCS(betahat=MWdata_EventStudy$betahat,
                                    sigma=MWdata_EventStudy$sigma,
                                    numPrePeriods=numPrePeriods,
                                    numPostPeriods=numPostPeriods,
                                    l_vec=l_vec)
DeltaSD_SensitivityPlot=createSensitivityPlot(robustResults=DeltaSD_RobustResults,
                                              originalResults=OriginalResults)

pdf("mortality_y2.pdf")
DeltaSD_SensitivityPlot
dev.off()

#look at sensitivity around Y3 coefficient
l_vec=basisVector(4, numPostPeriods)
DeltaSD_RobustResults=createSensitivityResults(betahat=MWdata_EventStudy$betahat,
                                               sigma=MWdata_EventStudy$sigma,
                                               numPrePeriods=numPrePeriods,
                                               numPostPeriods=numPostPeriods,
                                               l_vec=l_vec)
head(DeltaSD_RobustResults)
OriginalResults=constructOriginalCS(betahat=MWdata_EventStudy$betahat,
                                    sigma=MWdata_EventStudy$sigma,
                                    numPrePeriods=numPrePeriods,
                                    numPostPeriods=numPostPeriods,
                                    l_vec=l_vec)
DeltaSD_SensitivityPlot=createSensitivityPlot(robustResults=DeltaSD_RobustResults,
                                              originalResults=OriginalResults)

pdf("mortality_y3.pdf")
DeltaSD_SensitivityPlot
dev.off()

#find breakdown point for Y3
DeltaSD_RobustResults=createSensitivityResults(betahat=MWdata_EventStudy$betahat,
                                               sigma=MWdata_EventStudy$sigma,
                                               numPrePeriods=numPrePeriods,
                                               numPostPeriods=numPostPeriods,
                                               l_vec=l_vec, Mvec=seq(from=0, to=.00004, by=.000005))
head(DeltaSD_RobustResults)
DeltaSD_SensitivityPlot=createSensitivityPlot(robustResults=DeltaSD_RobustResults,
                                              originalResults=OriginalResults)

pdf("mortality_y3_break.pdf")
DeltaSD_SensitivityPlot
dev.off()



lowerBound_M=DeltaSD_lowerBound_Mpre(betahat=MWdata_EventStudy$betahat, sigma=MWdata_EventStudy$sigma,
                                     numPrePeriods=numPrePeriods)
lowerBound_M
upperBound_M=DeltaSD_upperBound_Mpre(betahat=MWdata_EventStudy$betahat, sigma=MWdata_EventStudy$sigma,
                                     numPrePeriods=numPrePeriods)
upperBound_M
