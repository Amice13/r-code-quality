install.packages("bibliometrix") ###INSTALA 
library("bibliometrix") #### CARREGA

A <- convert2df(D <- "C:\\Users\\Ariel\\Desktop\\MESTRADO UFES\\TI e Inovação\\Bibliométrico Teresa\\dados digital marketing 2019\\scopusDM.bib", dbsource = "scopus", format = "bibtex")
B <- convert2df(Z <- "C:\\Users\\Ariel\\Desktop\\MESTRADO UFES\\TI e Inovação\\Bibliométrico Teresa\\dados digital marketing 2019\\savedrecsDM.bib", dbsource = "isi", format = "bibtex")
M <- mergeDbSources(A, B, remove.duplicated = TRUE)


write.csv(M, "artigos.csv")


#### BiblioAnalysis - Processamento dos dados

results <- biblioAnalysis(M, sep= ";") 
options(width=100)
S <- summary(object = results, k = 20, pause = FALSE)
plot(x = results, k = 20, pause = FALSE)
warnings(plot)

write.csv(M$MostCitedPapers, "Autores mais citados.csv") # Arquivo dos autores mais citados

CR <- citations(M, field = "article")
cbind(CR$Cited[1:30]) 

authors=gsub(","," ",names(results$Authors)[1:15])
indices <- Hindex(M, field = "author", elements=authors, sep = ";")
indices$CitationList  
indices$H #### tenta medir produtividade e citações juntos
topAU <- authorProdOverTime(M, k = 10, graph = TRUE)#produção no tempo 

L <- lotka(results) #muitos autores produzem pouco, e poucos produzem muito
L$AuthorProd
L$Beta
L$C
L$R2
L$p.value
Observed=L$AuthorProd[,3]
Theoretical=10^(log10(L$C)-2*log10(L$AuthorProd[,1]))
plot(L$AuthorProd
     [,1],Theoretical,type="l",col="red",ylim=c(0, 1), 
     xlab="Articles",ylab="Freq. of
     Authors",main="Scientific Productivity")
lines(L$AuthorProd[,1],Observed,col="blue")
legend(x="topright",c("Theoretical 
                      (B=2)","Observed"),col=c("red","blue"),lty =
         c(1,1,1),cex=0.6,bty="n")

# Matriz com as fontes mais relevante (SO)
Matriz1 <- cocMatrix(M, Field = "SO", sep = "; ")
sort(Matrix::colSums(Matriz1), decreasing = TRUE)[1:15]

# Matriz com as os trabalhos mais citados (CR)
Matriz2 <- cocMatrix(M, Field = "CR", sep = ". ")
sort(Matrix::colSums(Matriz2), decreasing = TRUE)[1:15]

# Matriz com os autores mais relevante (AU)
Matriz3 <- cocMatrix(M, Field = "AU", sep = ";")
sort(Matrix::colSums(Matriz3), decreasing = TRUE)[1:15]

# Articles coupling pelo menos uma fonte citada aparece nas referências de ambos os artigos
NetMatrix1 <- biblioNetwork(M, analysis = "coupling", network = "references", sep = ";")
networkPlot(NetMatrix1,  normalize = "salton", weighted=NULL, n = 503, Title = "Article' Coupling", type = "fruchterman",  size=5,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.n=503,label.cex=F)
netstat1 <- networkStat(NetMatrix1)
summary(netstat1) 

# Authors coupling
NetMatrix2 <- biblioNetwork(M, analysis = "coupling", network = "authors", sep = ";")
networkPlot(NetMatrix2,  normalize = "salton",  weighted=NULL, n = 503, Title = "Authors' Coupling", type = "fruchterman", size=5,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.n=503,label.cex=F)
netstat2 <- networkStat(NetMatrix2)
summary(netstat2)


NetMatrix4 <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
net=networkPlot(NetMatrix4, n = 20, Title = "Keyword Co-occurrences", type = "kamada", size=T)
netstat4 <- networkStat(NetMatrix4)
names(netstat4$network)
names(netstat4$vertex)
summary(netstat4, k=15)


#### Country Scientific Collaboration
CN <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix6 <- biblioNetwork(CN, analysis = "collaboration", network = "countries", sep = ";")
networkPlot(NetMatrix6, n = dim(NetMatrix6)[1], Title = "Country Collaboration", type = "circle", size=TRUE, remove.multiple=FALSE,labelsize=0.7,cluster="none")
netstat6 <- networkStat(NetMatrix6)
summary(netstat6)


# Conceptual Structure using keywords
CS <- conceptualStructure(M,field="ID", minDegree=1, k.max=25, stemming=FALSE)

