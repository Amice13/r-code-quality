library(openxlsx)
library(lmerTest)

# Linear mixed model for the robustness analyses of figure 1A for new authors (red and orange lines). Each file has to be loaded separately and the analysis run 
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime2Filtered1Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # FullPath must be replaced by the full path to the directory where the file is saved
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime1Filtered1Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of one year
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime3Filtered1Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of three years
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime4Filtered1Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of four years
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime5Filtered1Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of five years
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime10Filtered1Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of ten years
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime0Filtered1Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors such as they never published in the journal before

Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime2Filtered1Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of two years but without removing authors who have homonyms (threshold of 50 articles published in the same journa over the entire time range of the journal)

Model<-"Authors1LastFull~Year*JournalType+(1|Journal)" # Model
Resultat<-lmer(Model,data=Matrice)
hist(resid(Resultat)) # Display residuals
summary(Resultat)



# Linear mixed model for the robustness analyses of figure 1A for intermediate authors (blue and light blue lines). Each file has to be loaded separately and the analysis run
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime2Filtered1Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # FullPath must be replaced by the full path to the directory where the file is saved
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime1Filtered1Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of one year
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime3Filtered1Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of three years
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime4Filtered1Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of four years
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime5Filtered1Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of five years
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime10Filtered1Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of ten years
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime0Filtered1Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors such as they never published in the journal before

Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime2Filtered1Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of two years but without removing authors who have homonyms (threshold of 50 articles published in the same journa over the entire time range of the journal)

Model<-"AuthorsIntermediateJournal~Year*JournalType+(1|Journal)" # Model
Resultat<-lmer(Model,data=Matrice)
hist(resid(Resultat)) # Display residuals
summary(Resultat)


# Linear mixed model for the robustness analyses of figure 1C (restricted access to data). Each file has to be loaded separately and the analysis run
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime2Filtered1AllEditors2Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # FullPath must be replaced by the full path to the directory where the file is saved
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime1Filtered1AllEditors2Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of one year
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime3Filtered1AllEditors2Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of three years
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime4Filtered1AllEditors2Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of four years
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime5Filtered1AllEditors2Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of five years
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime10Filtered1AllEditors2Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of ten years
Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime0Filtered1AllEditors2Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors such as they never published in the journal before

Matrice<-read.xlsx("FullPath\\AnalyseMixedModelJournalTime2Filtered0AllEditors2Full.xlsx",sheet=1,startRow=1,colNames=TRUE) # Data for a duration used to define new authors of two years but without removing authors who have homonyms (threshold of 50 articles published in the same journa over the entire time range of the journal)
 
Model<-"Authors1LastFull~Year*NArticles+Year*ResearchExp+Year*EditorialExp+Year*JournalType+(1|Editor)" # Model for new authors
Model<-"AuthorsIntermediateJournal~Year*NArticles+Year*ResearchExp+Year*EditorialExp+Year*JournalType+(1|Editor)" # Model for known intermediate authors
Resultat<-lmer(Model,data=Matrice)
hist(resid(Resultat)) # Display residuals
summary(Resultat)








