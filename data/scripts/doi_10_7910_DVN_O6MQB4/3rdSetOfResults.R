library(openxlsx)
library(lmerTest)

# Linear mixed model for the robustness analyses of figure 2B (data access restricted). Each file has to be loaded separately and the analysis run
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime2Filtered1JCRnFullRate.xlsx",sheet=1,startRow=1,colNames=TRUE) # FullPath must be replaced by the full path to the directory where the file is saved
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime1Filtered1JCRnFullRate.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of one year
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime3Filtered1JCRnFullRate.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of three years
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime4Filtered1JCRnFullRate.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of four years
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime5Filtered1JCRnFullRate.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of five years
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime10Filtered1JCRnFullRate.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of ten years
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime0Filtered1JCRnFullRate.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors such as they never published in the journal before

Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime2Filtered0JCRnFullRate.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of two years but without removing authors who have homonyms (threshold of 50 articles published in the same journa over the entire time range of the journal)

Model<-"JCR1~Authors1LastFull*JournalType+(1|Journal)" # Model
Resultat<-lmer(Model,data=Matrice)
hist(resid(Resultat)) # Display residuals
summary(Resultat)


# Linear mixed model for the robustness analyses of the two left bars of figure 2C. Each file has to be loaded separately and the analysis run
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime2Filtered1JCRnDifference1Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # FullPath must be replaced by the full path to the directory where the file is saved
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime1Filtered1JCRnDifference1Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of one year
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime3Filtered1JCRnDifference1Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of three years
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime4Filtered1JCRnDifference1Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of four years
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime5Filtered1JCRnDifference1Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of five years
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime10Filtered1JCRnDifference1Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of ten years
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime0Filtered1JCRnDifference1Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors such as they never published in the journal before

Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime2Filtered0JCRnDifference1Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of two years but without removing authors who have homonyms (threshold of 50 articles published in the same journa over the entire time range of the journal)

Model<-"Authors1LastFull~JCR1*JournalType*AuthorsType+(1|Journal)" # Model
Resultat<-lmer(Model,data=Matrice)
hist(resid(Resultat)) # Display residuals
summary(Resultat)


# Linear mixed model for the robustness analyses of the two right bars of figure 2C. Each file has to be loaded separately and the analysis run
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime2Filtered1JCRnDifference2Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # FullPath must be replaced by the full path to the directory where the file is saved
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime1Filtered1JCRnDifference2Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of one year
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime3Filtered1JCRnDifference2Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of three years
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime4Filtered1JCRnDifference2Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of four years
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime5Filtered1JCRnDifference2Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of five years
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime10Filtered1JCRnDifference2Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of ten years
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime0Filtered1JCRnDifference2Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors such as they never published in the journal before

Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime2Filtered0JCRnDifference2Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of two years but without removing authors who have homonyms (threshold of 50 articles published in the same journa over the entire time range of the journal)

Model<-"Authors1LastFull~JCR1*JournalType*AuthorsType+(1|Journal)" # Model
Resultat<-lmer(Model,data=Matrice)
hist(resid(Resultat)) # Display residuals
summary(Resultat)


# Linear mixed model for the robustness analyses of the analysis showing that the difference between the two left bars of figure 2C increases over time. Each file has to be loaded separately and the analysis run
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime2Filtered1JCRnDifference1Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # FullPath must be replaced by the full path to the directory where the file is saved
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime1Filtered1JCRnFullRate.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of one year
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime3Filtered1JCRnFullRate.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of three years
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime4Filtered1JCRnFullRate.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of four years
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime5Filtered1JCRnFullRate.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of five years
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime10Filtered1JCRnFullRate.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of ten years
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime0Filtered1JCRnFullRate.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors such as they never published in the journal before

Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime2Filtered0JCRnFullRate.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of two years but without removing authors who have homonyms (threshold of 50 articles published in the same journa over the entire time range of the journal)

Model<-"Authors1LastFull~JCR1*AuthorsType*Time*JournalType+(1|Journal)" # Model
Resultat<-lmer(Model,data=Matrice)
hist(resid(Resultat)) # Display residuals
summary(Resultat)


