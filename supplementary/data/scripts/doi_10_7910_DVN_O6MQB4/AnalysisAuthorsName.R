library(openxlsx)
library(lmerTest)

# Linear mixed model
Matrice<-read.xlsx("FullPath\\AnalyseAuthorsName.xlsx",sheet=1,startRow=1,colNames=TRUE) # FullPath must be replaced by the full path to the directory where the file is saved
Model<-"AllArticles~Nauthors+(1|Journal)" # Model for all types of articles
Model<-"OriginalArticles~Nauthors+(1|Journal)" # Model for original articles
Resultat<-lmer(Model,data=Matrice)
hist(resid(Resultat)) # Display residuals
summary(Resultat)


