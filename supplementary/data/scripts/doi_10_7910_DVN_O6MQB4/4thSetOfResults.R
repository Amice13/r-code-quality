library(openxlsx)
library(lmerTest)

# Linear mixed model for the analysis of the evolution across years of the rate of retracted articles in generalist and specialized Nature journals 
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalRetractedRatioFull.xlsx",sheet=1,startRow=1,colNames=TRUE) # FullPath must be replaced by the full path to the directory where the file is saved
Model<-"Retracted~Time*JournalType+(1|Journal)" # Model
Resultat<-lmer(Model,data=Matrice)
hist(resid(Resultat)) # Display residuals
summary(Resultat)
