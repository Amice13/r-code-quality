# install.packages("bibliometrix") 

library(bibliometrix) 
A <- convert2df("c:/bib/savedrecs.bib", dbsource = "isi",    format = "bibtex")
B <- convert2df("c:/bib/scopus.bib",    dbsource = "scopus", format = "bibtex")
M <- mergeDbSources(A, B, remove.duplicated = TRUE)
P <- M[,c("AU","TI","AB","DE","SO","TC","PY","LA","DT","DI")]
write.table(P, "c:/bib/artigos.csv", sep=";", row.names=FALSE)
write.table(M, "c:/bib/bibliometrix-dados.csv", sep=";")

 Resultados <- biblioAnalysis(M)
 sink("c:/bib/Resumo.txt")
 Resumo <- summary(object = Resultados, k = 10)
 sink()
 Resumo <- summary(object = Resultados, k = 10)

biblioshiny();