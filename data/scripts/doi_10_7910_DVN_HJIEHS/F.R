###==================================
### Funções ABG
###==================================

### Data de Criação: 02/03/2011
### Data de Atualização: 23/04/2019
### Atualizado por: Bruna Faria
### Responsável: André Gabriel

############################################################################################################

### Tabela de frequência absoluta e relativa ###

tab <- function(x){cbind(table(x), prop.table(table(x)))}

###########################################################################################################

### Tabela de média, desvio padrão, 1º 2º e 3º quartil ###

basic <- function(x, more=F) {
  stats <- list()
  
  clean.x <- x[!is.na(x)]
  
  #stats$N <- length(x)
  #stats$NAs <- stats$N-length(clean.x)
  stats$N_validos <- length(clean.x)
  stats$Média <- mean(clean.x)
  stats$D.P <- sd(clean.x)
  stats$Mín. <- min(clean.x)
  stats$Q1 <- fivenum(clean.x)[2]
  stats$Q2<- fivenum(clean.x)[3]
  stats$Q3 <- fivenum(clean.x)[4]
  stats$Máx. <- max(clean.x)
  t1<- unlist(stats)
  names(t1)<- c("N válidos", "Média", "D.P.","Mín.", "1ºQ", "2ºQ", "3ºQ","Máx.")
  t1
}

############################################################################################################

### Tabela de média, desvio padrão, 1º 2º e 3º quartil para GRUPOS  ###

basic2 <- function(y, z, more=F){
  tab<-matrix(NA, length(levels(factor(z))), 8)
  for(i in 1:length(levels(factor(z)))){ 
    desc<- tapply(y, factor(z),  basic)[i]
    desc1<- unlist(desc)
    for(j in 1:8){ 
      tab[i,j] <-desc1[j]
    }}
  colnames(tab)<- c("N", "Média", "D.P.","Mín.", "1º Q.", "2º Q.", "3º Q.","Máx.")
  rownames(tab)<- levels(factor(z))
  return(tab)}

############################################################################################################

### Tabela de média, desvio padrão, 1º 2º e 3º quartil, Min, Máx e IC bootstrap para a média ###

basic.np <- function(x, more=F) {
  stats <- list()
  
  clean.x <- x[!is.na(x)]
  
  #stats$N <- length(x)
  #stats$NAs <- stats$N-length(clean.x)
  stats$N_validos <- length(clean.x)
  stats$Média <- mean(clean.x)
  stats$D.P. <- sd(clean.x)
  stats$L.I.<- as.numeric(m.b(clean.x)[1])
  stats$L.S.<- as.numeric(m.b(clean.x)[2])
  stats$Mín. <- min(clean.x)
  stats$Q1 <- fivenum(clean.x)[2]
  stats$Q2<- fivenum(clean.x)[3]
  stats$Q3 <- fivenum(clean.x)[4]
  stats$Máx. <- max(clean.x)
  t1<- unlist(stats)
  names(t1)<- c("N válidos", "Média", "D.P.","L.I", "L.S","Mín.", "1Q", "2Q", "3Q","Máx.")
  t1
}

############################################################################################################

### Tabela de média, desvio padrão, quartil 50, 95 e 975, Min, Máx e IC bootstrap para a média ###

basic.npip <- function(x,more=F) {
  stats <- list()
  
  clean.x <- x[!is.na(x)]
  
  #stats$N <- length(x)
  #stats$NAs <- stats$N-length(clean.x)
  stats$N_validos <- length(clean.x)
  stats$Média <- mean(clean.x)
  stats$D.P. <- sd(clean.x)
  stats$L.I.<- as.numeric(m.b(clean.x)[1])
  stats$L.S.<- as.numeric(m.b(clean.x)[2])
  stats$Mín. <- min(clean.x)
  stats$Q1 <- quantile(clean.x,probs=c(0.50))
  stats$Q2<- quantile(clean.x,probs=c(0.950))
  stats$Q3 <- quantile(clean.x,probs=c(0.975))
  stats$Máx. <- max(clean.x)
  t1<- unlist(stats)
  names(t1)<- c("N válidos", "Média", "D.P.","L.I", "L.S","Mín.", "Q50", "Q95", "Q975","Máx.")
  t1
}

############################################################################################################

### Tabela de média, desvio padrão, IC bootstrap para a média e da freq absoluta ###

basic.np.ord <- function(x, more=F) {
  stats <- list()
  clean.x <- x[!is.na(x)]
  #stats$N <- length(x)
  #stats$NAs <- stats$N-length(clean.x)
  stats$N_validos <- length(clean.x)
  stats$Média <- mean(clean.x)
  stats$D.P<- sd(clean.x)
  stats$L.I<- as.numeric(m.b(clean.x)[1])
  stats$L.S<- as.numeric(m.b(clean.x)[2])
  stats$N <-  prop.table(table(x))
  t1<- unlist(stats)
  names(t1)<- c("N válidos", "Média","D.P","L.I", "L.S", paste("N","-", names(table(x))))
  t1
}

############################################################################################################

### Tabela de média, erro padrão, 1º 2º e 3º quartil ###

basic.stats <- function(x, more=F) {
  stats <- list()
  
  clean.x <- x[!is.na(x)]
  
  #stats$N <- length(x)
  #stats$NAs <- stats$N-length(clean.x)
  stats$N_validos <- length(clean.x)
  stats$Média <- mean(clean.x)
  stats$E.P <- sd(clean.x)/sqrt(length(clean.x))
  stats$Q1 <- fivenum(clean.x)[2]
  stats$Q2<- fivenum(clean.x)[3]
  stats$Q3 <- fivenum(clean.x)[4]
  t1<- unlist(stats)
  names(t1)<- c("N válidos", "Média", "E.P", "1Q", "2Q", "3Q")
  t1
}

############################################################################################################

### Tabela de média, desvio padrão, 1º 2º e 3º quartil ###

basic.statsF1 <- function(x, more=F) {
  stats <- list()
  clean.x <- x[!is.na(x)]
  stats$Q2<- fivenum(clean.x)[3]
  stats$Q1 <- round(fivenum(clean.x)[2],3)
  stats$Q3 <- round(fivenum(clean.x)[4],3)
  stats$Média <- round(mean(clean.x),3)
  stats$D.P <- round(sd(clean.x),3)
  t1<- unlist(stats)
  names(t1)<- c("2Q", "1Q","3Q", "Média", "D.P")
  t1
}

############################################################################################################

### Tabela de média, erro padrão, 1º 2º e 3º quartil por GRUPO ###

basic.stats2 <- function(y, z, more=F){
  tab<-matrix(NA, length(levels(factor(z))), 6)
  for(i in 1:length(levels(factor(z)))){ 
    desc<- tapply(y, factor(z),  basic.stats)[i]
    desc1<- unlist(desc)
    for(j in 1:6){ 
      tab[i,j] <-desc1[j]
    }}
  colnames(tab)<- c("N válidos", "Média", "E.P", "1Q", "2Q", "3Q")
  rownames(tab)<- levels(factor(z))
  return(tab)}

############################################################################################################

### IC Bootstrap ###

m.b <- function(x){
  media<-c()
  for (i in 1:1000) {
    boot<-sample(x, replace=TRUE)
    media[i] <- mean(boot, na.rm =TRUE)
  }
  LI<- quantile(media, probs=c(0.025), na.rm =TRUE)
  LS<- quantile(media, probs=c(0.975), na.rm =TRUE)
  valores<- c(LI,LS)
  valores
}

############################################################################################################

### Média, IC p/ média, 1º, 2º e 3º quartil ###

stats.np <- function(x,more=F) {
  stats <- list()
  
  clean.x <- x[!is.na(x)]
  
  #stats$N <- length(x)
  #stats$NAs <- stats$N-length(clean.x)
  stats$N_validos <- length(clean.x)
  stats$Média <- mean(clean.x)
  stats$L.I<- as.numeric(m.b(clean.x)[1])
  stats$L.S<- as.numeric(m.b(clean.x)[2])
  stats$Q1 <- fivenum(clean.x)[2]
  stats$Q2<- fivenum(clean.x)[3]
  stats$Q3 <- fivenum(clean.x)[4]
  unlist(stats)
}

############################################################################################################

### Média, IC p/ média, 1º, 2º e 3º quartil por GRUPO ###

basic.np2 <- function(y, z, more=F){
  tab<-matrix(NA, length(levels(factor(z))), 7)
  for(i in 1:length(levels(factor(z)))){ 
    desc<- tapply(y, factor(z),  stats.np)[i]
    desc1<- unlist(desc)
    for(j in 1:7){ 
      tab[i,j] <-desc1[j]
    }}
  colnames(tab)<- c("N válidos", "Média", "L.I", "L.S", "1Q", "2Q", "3Q")
  rownames(tab)<- levels(factor(z))
  return(tab)}

############################################################################################################

basic.np.ord2 <- function(y, z, more=F){
  tab<-matrix(NA, length(levels(factor(z))), 4+length(table(x)) )
  for(i in 1:length(levels(factor(z)))){ 
    desc<- tapply(y, factor(z),  basic.np.ord)[i]
    desc1<- unlist(desc)
    for(j in 1:(4+length(table(x)))){ 
      tab[i,j] <-desc1[j]
    }}
  colnames(tab)<- c("N válidos", "Média","L.I", "L.S", paste("N","-", names(table(x))))
  rownames(tab)<- levels(factor(z))
  return(tab)}

############################################################################################################

stats.ic <- function(x,more=F) {
  stats <- list()
  
  clean.x <- x[!is.na(x)]
  
  #stats$N <- length(x)
  #stats$NAs <- stats$N-length(clean.x)
  stats$N_validos <- round(length(clean.x),3)
  stats$Média <- round(mean(clean.x),3)
  stats$L.I<- round(mean(clean.x)-1.965*sd(clean.x)/sqrt(length(clean.x)),3)
  stats$L.S<- round(mean(clean.x)+1.965*sd(clean.x)/sqrt(length(clean.x)),3)
  stats$Q1 <- round(fivenum(clean.x)[2],3)
  stats$Q2<- round(fivenum(clean.x)[3],3)
  stats$Q3 <- round(fivenum(clean.x)[4],3)
  stats$Min <- round(min(clean.x),3)
  stats$Max <- round(max(clean.x),3)
  
  unlist(stats)
}

############################################################################################################

basic.ic2 <- function(y, z, more=F){
  tab<-matrix(NA, length(levels(factor(z))), 7)
  for(i in 1:length(levels(factor(z)))){ 
    desc<- tapply(y, factor(z),  stats.ic)[i]
    desc1<- unlist(desc)
    for(j in 1:7){ 
      tab[i,j] <-desc1[j]
    }}
  colnames(tab)<- c("N válidos", "Média", "L.I", "L.S", "1ºQ", "2ºQ", "3ºQ", "Min", "Max")
  rownames(tab)<- levels(factor(z))
  return(tab)}

############################################################################################################

### Teste de Mann-Whitney ###

whitney.abg <- function(y, x, more=F) {
  desc<-t(data.frame(tapply(y, factor(x),  basic.stats)[1], tapply(y, factor(x),  basic.stats)[2]))
  p.value <-wilcox.test(y ~ x, exact=FALSE)$p.value
  tab<- data.frame(desc, p.value)
  colnames(tab)<- c("N", "Média", "E.P.", "1º Q.", "2º Q.", "3º Q.", "Valor-p")
  return(tab)
}

############################################################################################################

### Teste t-student ###

t.test.abg <- function(y, x, more=F) {
  desc<-t(data.frame(tapply(y, factor(x),  basic.stats)[1], tapply(y, factor(x),  basic.stats)[2]))
  p.value <-t.test(y ~ x, exact=FALSE)$p.value
  tab<- data.frame(desc, p.value)
  colnames(tab)<- c("N_validos", "Média", "E.P", "1ºQ", "2ºQ", "3ºQ", "Valor-P")
  return(tab)
}

############################################################################################################

### Teste Wilcox ###

wilcox.abg <- function(y1, y2, more=F) {
  dif<- y1-y2
  diferença<-basic.stats(dif)
  p.value <-wilcox.test(dif, exact=F)$p.value
  tab<- cbind(t(data.frame(diferença)), p.value)
  colnames(tab)<- c("N_validos", "Média", "E.P", "1ºQ", "2ºQ", "3ºQ", "Valor-P")
  return(tab)
}

############################################################################################################

### Teste t-pareado ###

t.pareado.abg <- function(y1, y2, more=F) {
  dif<- y1-y2
  diferença<-basic.stats(dif)
  p.value <-t.test(dif, exact=F)$p.value
  tab<- cbind(t(data.frame(diferença)), p.value)
  colnames(tab)<- c("N_validos", "Média", "E.P", "1ºQ", "2ºQ", "3ºQ", "Valor-P")
  return(tab)
}

############################################################################################################

### ANOVA ###

anova.abg <- function(y, x, more=F){
  tab<-matrix(NA, length(levels(factor(x))), 6)
  for(i in 1:length(levels(factor(x)))){ 
    desc<- tapply(y, factor(x),  basic.stats)[i]
    desc1<- unlist(desc)
    for(j in 1:6){ 
      tab[i,j] <-desc1[j]
    }
  }
  colnames(tab)<- c("N válidos", "Média", "E.P", "1Q", "2Q", "3Q")
  rownames(tab)<- levels(factor(x))
  normal <- shapiro.test(y)$p.value
  Homo<- bartlett.test(y ~ factor(x))$p.value
  anova<- summary(aov(y~factor(x)))
  tukey <- TukeyHSD(aov(y~factor(x)))
  model<-list(tabela=tab,ANOVA=anova, C.M=tukey$"factor(x)", Normalidade=normal, Homocedasticidade=Homo)
  model
}

############################################################################################################

### Teste Kruskal-Wallis ###

kruskal.abg <- function(y, z, more=F){
  tab<-matrix(NA, length(levels(factor(z))), 6)
  for(i in 1:length(levels(factor(z)))){ 
    desc<- tapply(y, factor(z),  basic.stats)[i]
    desc1<- unlist(desc)
    for(j in 1:6){ 
      tab[i,j] <-desc1[j]
    }
  }
  p_valor<- rep(kruskal.test(y~factor(z))$p.value, length(levels(factor(z))))
  tab <- cbind(tab, p_valor)
  colnames(tab)<- c("N", "Média", "E.P.", "1º Q.", "2º Q.", "3º Q.", "Valor-p")
  rownames(tab)<- levels(factor(z))
  
  if(!require(PMCMR)){ install.packages("PMCMR"); require(PMCMR) }
  CM <- posthoc.kruskal.nemenyi.test(y ~ factor(z), dist="Chisq")$p.value
  
  model<-list(tabela=tab, C.Multiplas=CM)
  model
}

############################################################################################################

kruskal.abgF1 <- function(y, z, more=F){
  tab<-matrix(NA, length(levels(factor(z))), 5)
  for(i in 1:length(levels(factor(z)))){ 
    desc<- tapply(y, factor(z),  basic.statsF1)[i]
    desc1<- unlist(desc)
    for(j in 1:5){ 
      tab[i,j] <-desc1[j]
    }
  }
  p_valor<- rep(kruskal.test(y~factor(z))$p.value, length(levels(factor(z))))
  tab<- cbind(tab, p_valor)
  colnames(tab)<- c("2º Q.", "1º Q.", "3º Q.", "Média", "D.P.", "Valor-p")
  rownames(tab)<- levels(factor(z))
  
  CM <- posthoc.kruskal.nemenyi.test(y ~ factor(z))$p.value
  
  model<-list(tabela=tab, C.Multiplas=CM)
  model
}

############################################################################################################

### Função para teste qui-quadrado (chisq), exato de fisher (fisher) e qui-quadrado simulado (chisq.simulate) ###

tabela.aderencia <- function(x, y, type.sum, teste){
  t0 <- table(x, y)
  
  if(type.sum==2) {
    t1 <- prop.table(t0, 2)
  } else {
    t1 <- prop.table(t0, 1)
  }
  
  colnames(t0) <- paste0("X", 1:dim(t0)[2])
  colnames(t1) <- paste0("X", 1:dim(t1)[2])
  t2_aux <- cbind(t0, t1)
  t3 <- t2_aux[, order(colnames(t2_aux))]
  colnames(t3) <- c(rep(c("N", "%"), dim(t3)[2]/2))
  
  if(teste=="chisq") {
    Valor_p <- chisq.test(t0)$p.value
  }
  if(teste=="fisher") {
    Valor_p <- fisher.test(t0)$p.value
  } 
  if(teste=="chisq.simulate"){
    
    Valor_p <- chisq.test(t0, simulate.p.value=TRUE, B=10000)$p.value
  }
  
  t4 <- cbind(t3, Valor_p)
  return(t4)
}      

############################################################################################################

pareto<- function (x, ylab = "Frequência", xlab, ylim, main, col = "seagreen4", 
                   ...) 
{
  require(qcc)
  call <- match.call(expand.dots = TRUE)
  varname <- deparse(substitute(x))
  x <- as.table(x)
  if (length(dim(x)) > 1) 
    stop("only one-dimensional object (table, vector, etc.) may be provided")
  x <- sort(x, na.last = TRUE)
  missing <- is.na(names(x))
  x <- c(rev(x[!missing]), x[missing])
  missing <- is.na(names(x))
  cumsum.x <- cumsum(x[!missing])
  q <- seq(0, max(cumsum.x), length = 5)
  if (missing(xlab)) 
    xlab <- ""
  if (missing(ylim)) 
    ylim <- c(0, max(cumsum.x) * 1.05)
  if (missing(main)) 
    main <- paste("", "")
  if (missing(col)) 
    col <- heat.colors(length(x))
  w <- max(sapply(names(x), nchar))
  if (is.null(call$las)) 
    las <- 3
  else las <- call$las
  if (is.null(call$mar)) {
    if (las == 1) 
      mar <- c(0, 1, 0, 2)
    else mar <- c(log(max(w), 2), 1, 0, 2)
  }
  else mar <- call$mar
  oldpar <- par(mar = par("mar") + mar, las = las, cex = qcc.options("cex"), 
                no.readonly = TRUE)
  on.exit(par(oldpar))
  pc <- barplot(x, width = 1, space = 0.2, main = main, ylim = ylim, 
                ylab = ylab, xlab = xlab, col = col, ...)
  abline(h = q[2:5], col = "lightgrey")
  rect(pc - 0.5, rep(0, length(x)), pc + 0.5, x, col = col)
  lines(pc[!missing], cumsum.x, type = "b", cex = 0.7)
  box()
  axis(4, at = q, las = 3, labels = paste(seq(0, 1, length = 5) * 
                                            100, "%", sep = ""))
  mtext("Frequência Acumulada", 4, line = 2.5, las = 3)
  tab <- cbind(x[!missing], cumsum.x, x[!missing]/max(cumsum.x) * 
                 100, cumsum.x/max(cumsum.x) * 100)
  colnames(tab) <- c("Frequency", "Cum.Freq.", "Percentage", 
                     "Cum.Percent.")
  names(dimnames(tab)) <- c("", paste("\nPareto chart analysis for", 
                                      varname))
  return(tab)
}

############################################################################################################

multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }}

############################################################################################################

makeTransparent<-function(someColor, alpha=100)
{
  newColor<-someColor + alpha #I wish
  return(newColor)
}

radar.chart<- function(points=p, var=var, names=names, col=col, valores=valores){
  
  angle<- 360/var
  
  plot(seq(-1.1, 1.1, 0.01),seq(-1.1, 1.1, 0.01), type="n", xaxt="n", yaxt="n", xlab="", ylab="", axes=FALSE)
  
  x<- sin(seq(0, 2*pi, 2*pi/var))
  y<- cos(seq(0, 2*pi, 2*pi/var))
  
  x1<- sin(seq(0, 2*pi, 2*pi/var))*0.75
  y1<- cos(seq(0, 2*pi, 2*pi/var))*0.75
  
  x2<- sin(seq(0, 2*pi, 2*pi/var))*0.5
  y2<- cos(seq(0, 2*pi, 2*pi/var))*0.5
  
  x3<- sin(seq(0, 2*pi, 2*pi/var))*0.25
  y3<- cos(seq(0, 2*pi, 2*pi/var))*0.25
  
  polygon(x[-(var+1)]*p, y[-(var+1)]*p, angle=angle, col=col, border=col, lwd=2)
  
  text(x[-(var+1)]*p*1.1, y[-(var+1)]*p*1.1, valores, cex=0.8)
  
  polygon(x, y, angle=angle)
  polygon(x1, y1, angle=angle, lty=2)
  polygon(x2, y2, angle=angle, lty=2)
  polygon(x3, y3, angle=angle, lty=2)
  
  segments(0, 0, x, y)
  text(x*1.15, y*1.15, names, cex=0.8)
}

## Exemplos
#var=10
#names=c("abs","vsf","shd","sjd","jrd","ksd","wks","wld","jsd","ksd")
#p<- c(0.8, 0.8, 0.1, 0.8, 0.1, 0.5, 0.8, 0.9, 0.6, 0.9)
#p2<- c(1, 0.8, 0.8, 1, 0.8, 0.2, 0.2, 0.9, 0.6, 0.1)
#p3<- c(1, 0.2, 0.4, 0.1, 0, 0.2, 0.2, 0, 0.6, 0.1)
#radar.chart(points=p, var=10, names=names, col=rgb(20/255,30/255, 135/255, alpha=0.5), valores="")
#polygon(x[-(var+1)]*p2, y[-(var+1)]*p2, angle=angle, col=rgb(41/255,41/255, 41/255, alpha=0.5), border=rgb(41/255,41/255, 41/255, alpha=0.5), lwd=2)
#polygon(x[-(var+1)]*p3, y[-(var+1)]*p3, angle=angle, col=rgb(255/255,41/255, 41/255, alpha=0.5), border=rgb(41/255,41/255, 41/255, alpha=0.5), lwd=2)

############################################################################################################

friedman.test.with.post.hoc <- function(formu, data, to.print.friedman = T, to.post.hoc.if.signif = T,  to.plot.parallel = T, to.plot.boxplot = T, signif.P = .05, color.blocks.in.cor.plot = T, jitter.Y.in.cor.plot =F)
{
  # formu is a formula of the shape: 	Y ~ X | block
  # data is a long data.frame with three columns:    [[ Y (numeric), X (factor), block (factor) ]]
  
  # Note: This function doesn't handle NA's! In case of NA in Y in one of the blocks, then that entire block should be removed.
  
  
  # Loading needed packages
  if(!require(coin))
  {
    print("You are missing the package 'coin', we will now try to install it...")
    install.packages("coin")		
    library(coin)
  }
  
  if(!require(multcomp))
  {
    print("You are missing the package 'multcomp', we will now try to install it...")
    install.packages("multcomp")
    library(multcomp)
  }
  
  if(!require(colorspace))
  {
    print("You are missing the package 'colorspace', we will now try to install it...")
    install.packages("colorspace")
    library(colorspace)
  }
  
  
  # get the names out of the formula
  formu.names <- all.vars(formu)
  Y.name <- formu.names[1]
  X.name <- formu.names[2]
  block.name <- formu.names[3]
  
  if(dim(data)[2] >3) data <- data[,c(Y.name,X.name,block.name)]	# In case we have a "data" data frame with more then the three columns we need. This code will clean it from them...
  
  # Note: the function doesn't handle NA's. In case of NA in one of the block T outcomes, that entire block should be removed.
  
  # stopping in case there is NA in the Y vector
  if(sum(is.na(data[,Y.name])) > 0) stop("Function stopped: This function doesn't handle NA's. In case of NA in Y in one of the blocks, then that entire block should be removed.")
  
  # make sure that the number of factors goes with the actual values present in the data:
  data[,X.name ] <- factor(data[,X.name ])
  data[,block.name ] <- factor(data[,block.name ])
  number.of.X.levels <- length(levels(data[,X.name ]))
  if(number.of.X.levels == 2) { warning(paste("'",X.name,"'", "has only two levels. Consider using paired wilcox.test instead of friedman test"))}
  
  # making the object that will hold the friedman test and the other.
  the.sym.test <- symmetry_test(formu, data = data,	### all pairwise comparisons	
                                teststat = "max",
                                xtrafo = function(Y.data) { trafo( Y.data, factor_trafo = function(x) { model.matrix(~ x - 1) %*% t(contrMat(table(x), "Tukey")) } ) },
                                ytrafo = function(Y.data){ trafo(Y.data, numeric_trafo = rank, block = data[,block.name] ) }
  )
  # if(to.print.friedman) { print(the.sym.test) }
  
  
  if(to.post.hoc.if.signif)
  {
    if(pvalue(the.sym.test) < signif.P)
    {
      # the post hoc test
      The.post.hoc.P.values <- pvalue(the.sym.test, method = "single-step")	# this is the post hoc of the friedman test
      
      
      # plotting
      if(to.plot.parallel & to.plot.boxplot)	par(mfrow = c(1,2)) # if we are plotting two plots, let's make sure we'll be able to see both
      
      if(to.plot.parallel)
      {
        X.names <- levels(data[, X.name])
        X.for.plot <- seq_along(X.names)
        plot.xlim <- c(.7 , length(X.for.plot)+.3)	# adding some spacing from both sides of the plot
        
        if(color.blocks.in.cor.plot) 
        {
          blocks.col <- rainbow_hcl(length(levels(data[,block.name])))
        } else {
          blocks.col <- 1 # black
        }					
        
        data2 <- data
        if(jitter.Y.in.cor.plot) {
          data2[,Y.name] <- jitter(data2[,Y.name])
          par.cor.plot.text <- "Parallel coordinates plot (with Jitter)"				
        } else {
          par.cor.plot.text <- "Parallel coordinates plot"
        }				
        
        # adding a Parallel coordinates plot
        matplot(as.matrix(reshape(data2,  idvar=X.name, timevar=block.name,
                                  direction="wide")[,-1])  , 
                type = "l",  lty = 1, axes = FALSE, ylab = Y.name, 
                xlim = plot.xlim,
                col = blocks.col,
                main = par.cor.plot.text)
        axis(1, at = X.for.plot , labels = X.names) # plot X axis
        axis(2) # plot Y axis
        points(tapply(data[,Y.name], data[,X.name], median) ~ X.for.plot, col = "red",pch = 4, cex = 2, lwd = 5)
      }
      
      if(to.plot.boxplot)
      {
        # first we create a function to create a new Y, by substracting different combinations of X levels from each other.
        subtract.a.from.b <- function(a.b , the.data)
        {
          the.data[,a.b[2]] - the.data[,a.b[1]]
        }
        
        temp.wide <- reshape(data,  idvar=X.name, timevar=block.name,
                             direction="wide") 	#[,-1]
        wide.data <- as.matrix(t(temp.wide[,-1]))
        colnames(wide.data) <- temp.wide[,1]
        
        Y.b.minus.a.combos <- apply(with(data,combn(levels(data[,X.name]), 2)), 2, subtract.a.from.b, the.data =wide.data)
        names.b.minus.a.combos <- apply(with(data,combn(levels(data[,X.name]), 2)), 2, function(a.b) {paste(a.b[2],a.b[1],sep=" - ")})
        
        the.ylim <- range(Y.b.minus.a.combos)
        the.ylim[2] <- the.ylim[2] + max(sd(Y.b.minus.a.combos))	# adding some space for the labels
        is.signif.color <- ifelse(The.post.hoc.P.values < .05 , "green", "grey")
        
        boxplot(Y.b.minus.a.combos,
                names = names.b.minus.a.combos ,
                col = is.signif.color,
                main = "Boxplots (of the differences)",
                ylim = the.ylim
        )
        legend("topright", legend = paste(names.b.minus.a.combos, rep(" ; PostHoc P.value:", number.of.X.levels),round(The.post.hoc.P.values,5)) , fill =  is.signif.color )
        abline(h = 0, col = "blue")
        
      }
      
      list.to.return <- list(Friedman.Test = the.sym.test, PostHoc.Test = The.post.hoc.P.values)
      if(to.print.friedman) {print(list.to.return)}				
      return(list.to.return)
      
    }	else {
      print("The results where not significant, There is no need for a post hoc test")
      return(the.sym.test)
    }					
  }
  
  # Original credit (for linking online, to the package that performs the post hoc test) goes to "David Winsemius", see:
  # http://tolstoy.newcastle.edu.au/R/e8/help/09/10/1416.html
}

#Performing the post-hoc tests of:
#Wilcoxon-Nemenyi-McDonald-Thompson test
#Hollander & Wolfe (1999), page 295

############################################################################################################

pcor.test <- function(x,y,z,use="mat",method="p",na.rm=T){
  # The partial correlation coefficient between x and y given z
  #
  # pcor.test is free and comes with ABSOLUTELY NO WARRANTY.
  #
  # x and y should be vectors
  #
  # z can be either a vector or a matrix
  #
  # use: There are two methods to calculate the partial correlation coefficient.
  #	 One is by using variance-covariance matrix ("mat") and the other is by using recursive formula ("rec").
  #	 Default is "mat".
  #
  # method: There are three ways to calculate the correlation coefficient, 
  #	    which are Pearson's ("p"), Spearman's ("s"), and Kendall's ("k") methods.
  # 	    The last two methods which are Spearman's and Kendall's coefficient are based on the non-parametric analysis.
  #	    Default is "p".
  #
  # na.rm: If na.rm is T, then all the missing samples are deleted from the whole dataset, which is (x,y,z).
  #        If not, the missing samples will be removed just when the correlation coefficient is calculated.
  #	   However, the number of samples for the p-value is the number of samples after removing 
  #	   all the missing samples from the whole dataset.
  #	   Default is "T".
  
  x <- c(x)
  y <- c(y)
  z <- as.data.frame(z)
  
  if(use == "mat"){
    p.use <- "Var-Cov matrix"
    pcor = pcor.mat(x,y,z,method=method,na.rm=na.rm)
  }else if(use == "rec"){
    p.use <- "Recursive formula"
    pcor = pcor.rec(x,y,z,method=method,na.rm=na.rm)
  }else{
    stop("\'use\' should be either \"rec\" or \"mat\"!\n")
  }
  
  # print the method
  if(gregexpr("p",method)[[1]][1] == 1){
    p.method <- "Pearson"
  }else if(gregexpr("s",method)[[1]][1] == 1){
    p.method <- "Spearman"
  }else if(gregexpr("k",method)[[1]][1] == 1){
    p.method <- "Kendall"
  }else{
    stop("\'method\' should be \"pearson\" or \"spearman\" or \"kendall\"!\n")
  }
  
  # sample number
  n <- dim(na.omit(data.frame(x,y,z)))[1]
  
  # given variables' number
  gn <- dim(z)[2]
  
  # p-value
  if(p.method == "Kendall"){
    statistic <- pcor/sqrt(2*(2*(n-gn)+5)/(9*(n-gn)*(n-1-gn)))
    p.value <- 2*pnorm(-abs(statistic))
    
  }else{
    statistic <- pcor*sqrt((n-2-gn)/(1-pcor^2))
    p.value <- 2*pnorm(-abs(statistic))
  }
  
  data.frame(estimate=pcor,p.value=p.value,statistic=statistic,n=n,gn=gn,Method=p.method,Use=p.use)
}			

############################################################################################################

# By using var-cov matrix
pcor.mat <- function(x,y,z,method="p",na.rm=T){
  
  x <- c(x)
  y <- c(y)
  z <- as.data.frame(z)
  
  if(dim(z)[2] == 0){
    stop("There should be given data\n")
  }
  
  data <- data.frame(x,y,z)
  
  if(na.rm == T){
    data = na.omit(data)
  }
  
  xdata <- na.omit(data.frame(data[,c(1,2)]))
  Sxx <- cov(xdata,xdata,m=method)
  
  xzdata <- na.omit(data)
  xdata <- data.frame(xzdata[,c(1,2)])
  zdata <- data.frame(xzdata[,-c(1,2)])
  Sxz <- cov(xdata,zdata,m=method)
  
  zdata <- na.omit(data.frame(data[,-c(1,2)]))
  Szz <- cov(zdata,zdata,m=method)
  
  # is Szz positive definite?
  zz.ev <- eigen(Szz)$values
  if(min(zz.ev)[1]<0){
    stop("\'Szz\' is not positive definite!\n")
  }
  
  # partial correlation
  Sxx.z <- Sxx - Sxz %*% solve(Szz) %*% t(Sxz)
  
  rxx.z <- cov2cor(Sxx.z)[1,2]
  
  rxx.z
}

############################################################################################################

# By using recursive formula
pcor.rec <- function(x,y,z,method="p",na.rm=T){
  # 
  
  x <- c(x)
  y <- c(y)
  z <- as.data.frame(z)
  
  if(dim(z)[2] == 0){
    stop("There should be given data\n")
  }
  
  data <- data.frame(x,y,z)
  
  if(na.rm == T){
    data = na.omit(data)
  }
  
  # recursive formula
  if(dim(z)[2] == 1){
    tdata <- na.omit(data.frame(data[,1],data[,2]))
    rxy <- cor(tdata[,1],tdata[,2],m=method)
    
    tdata <- na.omit(data.frame(data[,1],data[,-c(1,2)]))
    rxz <- cor(tdata[,1],tdata[,2],m=method)
    
    tdata <- na.omit(data.frame(data[,2],data[,-c(1,2)]))
    ryz <- cor(tdata[,1],tdata[,2],m=method)
    
    rxy.z <- (rxy - rxz*ryz)/( sqrt(1-rxz^2)*sqrt(1-ryz^2) )
    
    return(rxy.z)
  }else{
    x <- c(data[,1])
    y <- c(data[,2])
    z0 <- c(data[,3])
    zc <- as.data.frame(data[,-c(1,2,3)])
    
    rxy.zc <- pcor.rec(x,y,zc,method=method,na.rm=na.rm)
    rxz0.zc <- pcor.rec(x,z0,zc,method=method,na.rm=na.rm)
    ryz0.zc <- pcor.rec(y,z0,zc,method=method,na.rm=na.rm)
    
    rxy.z <- (rxy.zc - rxz0.zc*ryz0.zc)/( sqrt(1-rxz0.zc^2)*sqrt(1-ryz0.zc^2) )
    return(rxy.z)
  }			
}	

############################################################################################################

kmo <- function(x)
{
  x <- subset(x, complete.cases(x)) # Omit missing values
  r <- cor(x) # Correlation matrix
  r2 <- r^2 # Squared correlation coefficients
  i <- solve(r) # Inverse matrix of correlation matrix
  d <- diag(i) # Diagonal elements of inverse matrix
  p2 <- (-i/sqrt(outer(d, d)))^2 # Squared partial correlation coefficients
  diag(r2) <- diag(p2) <- 0 # Delete diagonal elements
  KMO <- sum(r2)/(sum(r2)+sum(p2))
  MSA <- colSums(r2)/(colSums(r2)+colSums(p2))
  return(list(KMO=KMO, MSA=MSA))
}

############################################################################################################

Bartlett.sphericity.test <- function(x)
{
  method <- "Bartlett's test of sphericity"
  data.name <- deparse(substitute(x))
  x <- subset(x, complete.cases(x)) # Omit missing values
  n <- nrow(x)
  p <- ncol(x)
  chisq <- (1-n+(2*p+5)/6)*log(det(cor(x)))
  df <- p*(p-1)/2
  p.value <- pchisq(chisq, df, lower.tail=FALSE)
  names(chisq) <- "X-squared"
  names(df) <- "df"
  return(structure(list(statistic=chisq, parameter=df, p.value=p.value,
                        method=method, data.name=data.name), class="htest"))
}

############################################################################################################

nfactors <- function (b1){
  eigenvalues<- eigen(cor(b1), TRUE)$values
  nsubjects <- dim(b1)[1]
  variables <- length(eigenvalues) # Computes the number of variables
  rep <- 1000 # Number of replications for PA analysis
  cent <- 0.95 # Centile value of PA analysis
  
  aparallel <- parallel(var = variables,
                        subject = nsubjects,
                        rep = rep,
                        cent = cent
  )$eigen$qevpea # The 95 centile
  
  results <- nScree(x=eigenvalues, aparallel=aparallel)
  results$Components
}

############################################################################################################

### Função da Curva ROC ###

CR<- function (table, graph = TRUE, add = FALSE, title = FALSE, line.col = "red", 
               auc.coords = NULL, grid = TRUE, grid.col = "grey84", ...) 
{
  if (dim(table)[2] != 2) 
    stop("There must be 2 columns")
  if (table[1, 1]/table[1, 2] < table[nrow(table), 1]/table[nrow(table), 
                                                            2]) {
    stop("At higher cut-off point, there should be more non-diseased")
  }
  firsttable <- table
  colnames(firsttable) <- c("Non-diseased", "Diseased")
  if (length(rownames(firsttable)) == 0) {
    rownames(firsttable) <- rep("", times = nrow(firsttable))
  }
  secondtable <- firsttable
  for (i in 1:length(secondtable[, 1])) {
    secondtable[i, 1] <- (sum(firsttable[, 1]) - sum(firsttable[(1:i), 
                                                                1]))/sum(firsttable[, 1])
    secondtable[i, 2] <- (sum(firsttable[, 2]) - sum(firsttable[(1:i), 
                                                                2]))/sum(firsttable[, 2])
    rownames(secondtable)[i] <- paste(">", rownames(secondtable)[i])
  }
  secondtable <- rbind((c(1, 1)), secondtable)
  colnames(secondtable) <- c("1-Especificidade", "Sensibilidade")
  auc <- 0
  for (i in 1:(nrow(secondtable) - 1)) {
    auc <- auc + (secondtable[i, 1] - secondtable[(i + 1), 
                                                  1]) * 0.5 * (secondtable[i, 2] + secondtable[(i + 
                                                                                                  1), 2])
  }
  if (graph) {
    if (!add) {
      plot(secondtable[, 1], secondtable[, 2], xlab = "1-Especificidade", 
           ylab = "Sensibilidade", xlim = (c(0, 1)), ylim = (c(0, 
                                                               1)), lwd=2, asp = 1, col = line.col, type = "l", ...)
      if (title) {
        title(main = "ROC curve of the diagnostic table", 
              ...)
      }
      lines(x = c(0, 1), y = c(0, 1), col = "gray84")
      if (grid) {
        abline(v = 0, lty = 2, col = grid.col)
        abline(v = 0.2, lty = 2, col = grid.col)
        abline(v = 0.4, lty = 2, col = grid.col)
        abline(v = 0.6, lty = 2, col = grid.col)
        abline(v = 0.8, lty = 2, col = grid.col)
        abline(v = 1, lty = 2, col = grid.col)
        abline(h = 0, lty = 2, col = grid.col)
        abline(h = 0.2, lty = 2, col = grid.col)
        abline(h = 0.4, lty = 2, col = grid.col)
        abline(h = 0.6, lty = 2, col = grid.col)
        abline(h = 0.8, lty = 2, col = grid.col)
        abline(h = 1, lty = 2, col = grid.col)
      }
      auclabel <- paste("Area under the curve =", round(auc, 
                                                        3))
    }
    else {
      lines(secondtable[, 1], secondtable[, 2], lwd=2, col = line.col, 
            ...)
    }
    if (!is.null(auc.coords)) {
      text(x = auc.coords[1], y = auc.coords[2], pos = 4, 
           labels = auclabel, ...)
    }
  }
  list(auc = auc, original.table = firsttable, diagnostic.table = secondtable)
}

############################################################################################################

### Função para validação de constructos ###

Val <- function(MDF1){
  nf<- function(boi){
    boi1<-eigen(cor(boi, method="spearman"), TRUE)
    eig <-  boi1$values
    N <- dim(boi)[1]
    res <- nSeScree(eig)
    eigenvalues<- eigen(cor(boi,method="spearman"), TRUE)$values
    nsubjects <- dim(boi)[1]
    variables <- length(eigenvalues) # Computes the number of variables
    rep <- 1000 # Number of replications for PA analysis
    cent <- 0.95 # Centile value of PA analysis
    aparallel <- parallel(var = variables,
                          subject = nsubjects,
                          rep = rep,
                          cent = cent)$eigen$qevpea # The 95 centile
    results <- nScree(x=eigenvalues, aparallel=aparallel)
    results$Components}
  faPR1 <- principal(MDF1, nfactors=1,  rotate="varimax")
  itens<- dim(MDF1)[2]
  AVE<- mean(faPR1$communality)
  KMO<- kmo(MDF1)$KMO
  AC<-cronbach(MDF1)$alpha
  DG<- (sum(principal(MDF1,rotate="varimax")$loadings)^2)/ ((sum(principal(MDF1,rotate="varimax")$loadings)^2) + (sum(1-(principal(MDF1,rotate="varimax")$communality)^2)))
  ND<- nf(MDF1)
  cbind(itens, AVE, AC, DG, KMO, ND)
}

Val1 <- function(MDF1){
  faPR1 <- principal(MDF1, nfactors=1,  rotate="varimax")
  itens<- dim(MDF1)[2]
  AVE<- mean(faPR1$communality)
  KMO<- kmo(MDF1)$KMO
  AC<-cronbach(MDF1)$alpha
  DG<- (sum(principal(MDF1,rotate="varimax")$loadings)^2)/ ((sum(principal(MDF1,rotate="varimax")$loadings)^2) + (sum(1-(principal(MDF1,rotate="varimax")$communality)^2)))
  tab<-cbind(itens, AVE, AC, DG, KMO, 1,1,1,1)
  colnames(tab)<- c("itens","AVE","AC","DG","KMO","noc","naf","nparallel","nkaiser")
  tab
}

############################################################################################################

### Função para Análise Fatorial ###

fa <- function(x){
  Mat<- cbind(principal(x,1, rotate="varimax")$loadings
              ,principal(x,1, rotate="varimax")$communality
              ,principal(x,1, rotate="varimax")$weights)
  Mat1<- Mat[order(abs(Mat[,1]), decreasing = TRUE),]
  colnames(Mat1)<- c("C.F.","Com.","Peso")
  Mat1
}

fa.full <- function(M, n=n){
  fa1<- function(x){
    Mat<- cbind(principal(x,1, rotate="varimax")$loadings
                ,principal(x,1, rotate="varimax")$communality
                ,principal(x,1, rotate="varimax")$weights)
    colnames(Mat)<- c("C.F.","Com.","Peso")
    Mat
  }
  Q<- function(x){quantile(x, probs=c(0.05/2, 1-0.05/2))}
  n<- n
  aux<- array(NA, c(dim(M)[2], n))
  for(i in 1:n){
    aux[,i]<- principal(M[sample(1:dim(M)[1], replace=T),],1, rotate="varimax")$weights
  }
  SE<- apply(aux,1,sd)
  Q1<- t(apply(aux,1, Q))
  Mat1<- cbind(fa1(M), SE, Q1)
  Mat2<- Mat1[order(abs(Mat1[,1]), decreasing = TRUE),]
  Mat2
}

############################################################################################################

### Função para DG ###

DG<- function(x){
  (sum(principal(x,rotate="varimax")$loadings)^2)/ ((sum(principal(x,rotate="varimax")$loadings)^2) + (sum(1-principal(x,rotate="varimax")$communality)^2))
}

############################################################################################################

### Função para Modelo Estrutural, de Mensuração e para medidas de validação do modelo - Equações Estruturais ###

FNC_INNER_MODEL <- function(model_pls){
  TAB_MOD <- do.call(rbind, summary(model_pls)$inner_model)
  
  ID_INNER_List <- list()
  for(i in 1:length(names(summary(model_pls)$inner_model))){
    ID_INNER_List[[i]] <- t(t(c(paste0(rownames(summary(model_pls)$inner_model[[i]]), " -> ", names(summary(model_pls)$inner_model)[i]))))
  }
  
  TAB_MOD <- cbind(TAB_MOD, ID_INNER=as.character(do.call(rbind, ID_INNER_List)))
  TAB_MOD2 <- TAB_MOD %>% as.data.frame() %>%  filter(rownames(TAB_MOD) != "Intercept") 
  
  #### IC ####
  TAB_IC_MOD <- data.frame(summary(model_pls)$boot$paths, ID=as.character(rownames(summary(model_pls)$boot$paths)))
  TAB_MOD_INNER_Final <- left_join(TAB_MOD2, TAB_IC_MOD, by=c("ID_INNER"="ID"))
  
  IC.95 <- c()
  for(i in 1:dim(TAB_MOD_INNER_Final)[1]){
    IC.95[i] <- paste("[", format(round(TAB_MOD_INNER_Final$perc.025[i],2),nsmall=2, decimal.mark=","), "; ", format(round(TAB_MOD_INNER_Final$perc.975[i],2),nsmall=2, decimal.mark=","), "]", sep="")
  }
  TAB_MOD_INNER_Final$IC <- IC.95
  TAB_MOD_INNER_Final$'Endógenas' <- gsub(".*-> ", "", TAB_MOD_INNER_Final$ID_INNER)
  TAB_MOD_INNER_Final$'Exógenas' <- gsub("->.*", "", TAB_MOD_INNER_Final$ID_INNER)
  TAB_MOD_INNER_Final2 <- TAB_MOD_INNER_Final[,c("ID_INNER","Endógenas", "Exógenas", "Estimate", "IC", "Std. Error", "Pr(>|t|)")]
  
  r2_inner <- data.frame(ID=row.names(summary(model_pls)$inner_summary), R2=summary(model_pls)$inner_summary$R2)
  
  TAB_MOD_INNER_Final3 <- inner_join(TAB_MOD_INNER_Final2, r2_inner, by=c("Endógenas"="ID"))
  return(TAB_MOD_INNER_Final3)
  
}

FNC_OUTER_MODEL <- function(model_pls){
  TAB_MENS <- summary(model_pls)$outer_model
  TAB_IC_PESO <- summary(model_pls)$boot$weights[,c(4,5)]
  TAB_OUTER_MODEL <- cbind(TAB_MENS, TAB_IC_PESO)
  IC.95 <- c()
  for(i in 1:dim(TAB_OUTER_MODEL)[1]){
    IC.95[i] <- paste("[", format(round(TAB_OUTER_MODEL$perc.025[i],2),nsmall=2, decimal.mark=","), "; ", format(round(TAB_OUTER_MODEL$perc.975[i],2),nsmall=2, decimal.mark=","), "]", sep="")
  }
  TAB_OUTER_MODEL$IC95 <- IC.95
  TAB_OUTER_MODEL_FINAL <- TAB_OUTER_MODEL[,c(1:3,9,4,5)]
  return(TAB_OUTER_MODEL_FINAL)
  
}

vcm_function <- function(x){
  x2 <- max((x[x!=1])^2, na.rm=T)
  return(x2)
}

FNC_VALIDA <- function(model_pls){
  VCM0 <- summary(model_pls)$correlations
  VCM1 <- data.frame(ID=rownames(t(t(apply(VCM0, 1, vcm_function)))), VCM=t(t(apply(VCM0, 1, vcm_function))))
  
  TAB_VAL <- data.frame(ID=rownames(summary(model_pls)$unidim), summary(model_pls)$unidim)
  TAB_VAL2 <- data.frame(ID=rownames(summary(model_pls)$inner_summary), AVE=summary(model_pls)$inner_summary$AVE)
  TAB_VAL3 <- inner_join(TAB_VAL, TAB_VAL2, by="ID")
  TAB_VAL4 <- inner_join(TAB_VAL3, VCM1, by="ID")
  TAB_VAL5 <- TAB_VAL4[,c("MVs", "C.alpha", "DG.rho", "AVE", "VCM", "eig.2nd")]
  return(TAB_VAL5)
}

############################################################################################################

# Plot likert ----

likert.plot <- 
  function(df, questao, titulo = "Likert Plot"){
    
    if(!require(dplyr)){install.packages("sjPlot"); require(sjPlot)}
    if(!require(dplyr)){install.packages("sjmisc"); require(sjmisc)}
    
    df <- df %>% as.data.frame()
    
    if(questao){
      colnames(df) <- lapply(1:ncol(df), function(x) paste0("Questão", x))
    } else {
      colnames(df) <- questao
    }
    
    plot_likert(df, values = "sum.outside") +
      labs(fill = "Respostas", 
           title = titulo)
    
  }


#














