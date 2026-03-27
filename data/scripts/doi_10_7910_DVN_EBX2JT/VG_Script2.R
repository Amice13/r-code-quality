#************************** PACOTES **********************************************************************
library(igraph)
library('somebm')
library(cowplot)
library(poweRlaw) 
## SERIES ************************************************************************************************
N<-10000 # informe o número de elementos
serie <- fbm(hurst=0.8, N)
#*********************************************************************************************************
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
#plot(x,y)
## LÊ A SÉRIE E APLICA O CRITÉRIO DE VISIBILIDADE *******************************************************
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
#********** VISIBILITY GRAPH GERADO **********************************************************************
g <- graph_from_data_frame(tab.pontos, directed = FALSE)
#********** PROPRIEDADES DE REDES ************************************************************************
vert <- vcount(g)
arest <- ecount(g)
grau<-degree(g,mode="all")
gr_m<-mean(grau)
dens<- edge_density(g, loops = FALSE)
lc <- cluster_louvain(g)
mod<-modularity(lc)
diam <- diameter(g)
cmm<-average.path.length(g,directed = FALSE)
c.aglom<-transitivity(g, type = "average")
#*************************** Fitting Lei de Potência *********************************************************
fit1 <- fit_power_law(grau+1, 10)
fit2 <- fit_power_law(grau+1, 10, implementation="R.mle")
#fit1$alpha#gama1
gama1<-fit1$alpha
#stats4::coef(fit2)#gama2
gama2<-stats4::coef(fit2)
fit1$logLik
stats4::logLik(fit2)
#*************** Tabela com as propriedades de redes********************************************************
tab.prop.redes <- data.frame(vert,arest,gr_m,dens,mod,diam,cmm,c.aglom,gama1,gama2)
View(tab.prop.redes)
#********************** PLOTS ******************************************************************************
#dev.new()
#***********************************************************************************************************
## plotagem de quatro gráficos na mesma janela
par(mfrow = c(2, 2))## Divide a janela gráfica em 2 linha e 2 colunas

## serie
plot.serie<-plot(serie,type="l",main="série")

## grafo
plot(g, layout=layout_with_kk(g), vertex.label=NA, vertex.size=2, main="Visibility Graph")#layout Kamada Kawai 
#plot(g, layout=layout_with_fr(g), vertex.label=NA, vertex.size=3,main="Visibility Graph")#layout Fruchterman-Reingold

## histograma da série 
hist(serie, prob=T,main="Distribuição de Frequências da Série")
## distribuição de graus - loglog e semilog *****************************************************************************
dist.grau <- degree.distribution(g)
d <- 1:max(grau)-1
ind <- grau !=0
plot(d[ind], dist.grau[ind], log="xy", pch=19,xlab=c("Log-Degree"), ylab=c("Log-n"), main="Log-Log Degree Distribution")
#plot(d[ind],dist.grau[ind],log="y",pch=19,xlab=c("Degree"), ylab=c("Log-n"), main="SemiLog Degree Distribution")
#***********************************************************************************************************************
par(mfrow = c(1, 1)) #volta para a janela padrão







