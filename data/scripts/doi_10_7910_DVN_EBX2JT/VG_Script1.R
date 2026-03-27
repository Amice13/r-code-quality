#************************** PACOTES ******************************************************************************
library(igraph)
library('somebm')
library(cowplot)
library(poweRlaw)
## SERIES *********************************************************************************************************
N<-11 # informe o número de elementos
#serie<-runif(N) # N numeros aleatórios a.p. dist. unif. em [0,1]
#serie<-rnorm(N,10,2) # N numeros aleatórios a.p. dist. norm. em [0,1]
#serie<-rep(c(1,9,5),N)# repete c(a,b,c, ... , t), N vezes.
#serie<-seq(1,N,1)# sequência de a ... N com incremento t

#******************************************************************************************************************
serie<-c(1,1,2,3,5,8,13,21,33,54,87) #Fibonacci
#serie<-c(7,3,5,1,0,6)#Crie sua própria série
N<-length(serie) #não comentar
#*******************************************************************************************************************

## VERIFICA CARACTERES INVÁLIDOS
y <- vector()
for(i in 1:N){
  n <-as.numeric(serie[i])
  if(is.numeric(n) && !(is.na(n))){
    y <- c(y, as.numeric(serie[i]))
  }
}

x <- 1:length(y)

par(pty="s")
## LÊ A SÉRIE E APLICA O CRITÉRIO DE VISIBILIDADE *****************************************************************
tab.pontos <- data.frame()
tam <- length(x)
for(i in 1:(tam-1)){
  tg.max <- ((y[i+1]-y[i])/(x[i+1]-x[i])) 
  tab.pontos <- rbind(tab.pontos, c(x[i],x[i+1]))
  for(k in (i+1):tam){
    tx.var <- ((y[k]-y[i])/(x[k]-x[i]))
    if(tx.var > tg.max){
      tg.max <- tx.var
      tab.pontos <- rbind(tab.pontos, c(x[i],x[k]))
    } 
  }
}
colnames(tab.pontos) <- c('x.1','x.2')
#********** VISIBILITY GRAPH GERADO ******************************************************************************
g <- graph_from_data_frame(tab.pontos, directed = FALSE)                                                           
#********** PROPRIEDADES DE REDES ********************************************************************************
vert <- vcount(g)
arest <- ecount(g)
grau<-degree(g,mode="all")
gr_m<-mean(grau)#grau médio
dens <- edge_density(g, loops = FALSE)#densidade
lc <- cluster_louvain(g)
mod<-modularity(lc)#modularidade
diam<- diameter(g)#diâmetro
cmm<-average.path.length(g,directed = FALSE)#caminho mínimo médio
c.aglom<-transitivity(g, type = "average")#coeficiente de aglomeração
#*************** Tabela com as propriedades de redes***************************************************************
tab.prop.redes <- data.frame(vert,arest,gr_m,dens,mod,diam,cmm,c.aglom)
View(tab.prop.redes)
#********************** PLOTS *************************************************************************************
#dev.new()
par(mfrow = c(2, 2))## Divide a janela gráfica em 2 linhas e 2 colunas
#******************************************************************************************************************
## série 
barplot(serie,names.arg =c(1:length(serie)),main="Série")

## grafo 
plot(g,vertex.size=20,main="Visibility Graph")

## histograma da série 
hist(serie, prob=T,main="Distribuição de Frequências da Série")

## distribuição de graus do grafo ##
d1 = degree.distribution(g, cumulative=FALSE)
len = length(d1)
#png(filename="G1_graus.png", height=500, width=500, bg="white")
barplot(d1, main="Distribuição de Graus do Grafo",xlab="Graus", ylab="PDF",
       names.arg=c(0:(len-1)))
#*********************************************************************************************************************
par(mfrow = c(1, 1)) #volta para a janela padrão










