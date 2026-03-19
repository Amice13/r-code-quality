# Script para calculo partiçao de variancia
# R version 4.0.2
# Modificado pela ultima vez em 24-05-2022

#Definindo o diretorio
setwd("~/Documents/Manuscritos_em_producao/Manuscrito_Fernanda_comunidades_girinos_goias/Analises")

#Carregando os pacotes necessarios
# install.packages("gtools")
# install.packages("hier.part")
library(gtools)
library(hier.part)
library(vegan)
library(ade4)

########################################################
# Regiao 1
{
  setwd("/home/gabriela/Documentos/Manuscritos_em_producao/Manuscrito_Fernanda_comunidades_girinos_goias/Analises/data")
  #Matriz de composiçao
  abund <- read.csv("abund.csv", head = T, sep = ";")
  head(abund)
  ab1 <- subset(abund, abund$site == "1")
  head(ab1)
  abund1 <- ab1 # esta matriz ja esta com transformacao de Hellinger
  abund1 <- abund1[,-c(1,2)]
  head(abund1)
  #abund1<-decostand(ab1, "hellinger", MARGIN=2)
  #abund1
  
  #Matriz ambiental
  amb1 <- read.csv("amb1.csv", head = T, sep = ";") 
  head(amb1)
  
  #Gower
  #variavel quantitativa (area)
  area1 <- data.frame(amb1$area)
  rownames(area1) <- rownames(amb1)
  
  #variavel quantitativa (margem)
  marg1 <- data.frame(amb1$margem)
  rownames(marg1) <- rownames(amb1)
  
  #variavel quantitativa (substrato)
  sub1 <- data.frame(amb1$substrato)
  rownames(sub1) <- rownames(amb1)
  
  ##variavel quantitativa (porcentagem de vegetaçao no interior)
  porcvegi1 <- data.frame(amb1$viporc)
  rownames(porcvegi1) <- rownames(amb1)
  
  #variavel quantitativa (vegetaçao no interior)
  vegi1 <- data.frame(amb1$vi)
  rownames(vegi1) <- rownames(amb1)
  
  #variavel quantitativa (porcentagem vegetaçao margens)
  porcvegm1 <- data.frame(amb1$vmporc)
  rownames(porcvegm1) <- rownames(amb1)
  
  #variavel quantitativa (vegetaçao margens)
  vegm1 <- data.frame(amb1$vm)
  rownames(vegm1) <- rownames(amb1)
  
  #variaveis binarias
  #para a caracteristica "hidroperiodo"
  library(ade4)
  hidrop1 <- prep.binary(amb1[,5:6], col.blocks = 2, label = "hidroperiodo")
  
  #para a carcteristica "tipo"
  tipo1 <- prep.binary(amb1[,3:4], col.blocks = 2, label = "tipo")
  
  #variaveis nominais
  #para a caracteristica "posiçao"
  posic1 <- data.frame(amb1$Posicao)
  
  ktab1 <- ktab.list.df(list(area1, marg1, sub1, porcvegi1,vegi1, porcvegm1, vegm1, hidrop1, tipo1, posic1))
  
  distamb1 <- dist.ktab(ktab1, c("Q", "Q", "Q", "Q", "Q", "Q","Q","B","B", "N"), c("scaledBYrange"))
  distamb1
  
  #PCoA
  library(vegan)
  PCoA.res1 <- capscale(distamb1 ~ 1, distance = "bray") 
  summary(PCoA.res1)
  scores(PCoA.res1, display = "sites")
  scores1 <- scores(PCoA.res1, display = "sites")
  scores1
  write.table(scores1, file = "scores1.csv", sep = ",")
  plot(PCoA.res1) 
  
  #PCNM
  pcnm1 <- read.csv("pcnm1.csv", head = T, sep = ",")
  head(pcnm1)
  pcnm1 <- pcnm1[,-1]
  head(pcnm1)
  pcnm1 <- as.matrix(pcnm1)
  head(pcnm1)
  
  # HIERARQUICAL PARTITIONING
  exp1 <- data.frame(pcnm1, scores1)
  exp1
  
  # calculando o indice de shannon
  {
    abun1 <- c(143, 14, 161, 21, 79, 46, 163)
    tot <- sum(abun1)
    tot
    pi <- abun1/tot
    pi
    log.pi <- log(pi)
    log.pi
    pi.log.pi <- pi*log.pi
    pi.log.pi
    H1 <- - sum(pi.log.pi)
    H1
    
    abun2 <- c(7, 1, 6)
    tot <- sum(abun2)
    tot
    pi <- abun2/tot
    pi
    log.pi <- log(pi)
    log.pi
    pi.log.pi <- pi*log.pi
    pi.log.pi
    H2 <- - sum(pi.log.pi)
    H2
    
    abun3 <- c(22, 14, 125)
    tot <- sum(abun3)
    tot
    pi <- abun3/tot
    pi
    log.pi <- log(pi)
    log.pi
    pi.log.pi <- pi*log.pi
    pi.log.pi
    H3 <- - sum(pi.log.pi)
    H3
    
    abun4 <- c(3, 5, 9, 3, 1)
    tot <- sum(abun4)
    tot
    pi <- abun4/tot
    pi
    log.pi <- log(pi)
    log.pi
    pi.log.pi <- pi*log.pi
    pi.log.pi
    H4 <- - sum(pi.log.pi)
    H4
    
    abun5 <- c(14, 60, 5)
    tot <- sum(abun5)
    tot
    pi <- abun5/tot
    pi
    log.pi <- log(pi)
    log.pi
    pi.log.pi <- pi*log.pi
    pi.log.pi
    H5 <- - sum(pi.log.pi)
    H5
    
    abun6 <- c(13, 1, 32)
    tot <- sum(abun6)
    tot
    pi <- abun6/tot
    pi
    log.pi <- log(pi)
    log.pi
    pi.log.pi <- pi*log.pi
    pi.log.pi
    H6 <- - sum(pi.log.pi)
    H6
    
    abun7 <- c(5, 157, 1)
    tot <- sum(abun7)
    tot
    pi <- abun7/tot
    pi
    log.pi <- log(pi)
    log.pi
    pi.log.pi <- pi*log.pi
    pi.log.pi
    H7 <- - sum(pi.log.pi)
    H7
  }
  
  H1total <- c(H1,H2,H3,H4,H5,H6,H7)
  H1total
  
  teste <- hier.part(H1total, exp1)
  teste
  prob <- rand.hp(H1total, exp1, num.reps = 1000)
  prob
}

#######################################################
# Regiao 2
{
  #Matriz de composiçao
  ab2 <- subset(abund, abund$site == "2")
  head(ab2)
  abund2 <- ab2 # esta matriz ja esta com transformacao de Hellinger
  abund2 <- abund2[,-c(1,2)]
  head(abund2)
  #abund2<-decostand(ab2, "hellinger", MARGIN=2)
  #abund2
  
  #Matriz ambiental
  amb2 <- read.csv("amb2.csv", head = T, sep = ";") 
  head(amb2)
  
  #Gower
  #variavel quantitativa (area)
  dimen2 <- data.frame(amb2$area)
  rownames(dimen2) <- rownames(amb2)
  
  #variavel quantitativa (margem)
  marg2 <- data.frame(amb2$margem)
  rownames(marg2) <- rownames(amb2)
  
  #variavel quantitativa (substrato)
  sub2 <- data.frame(amb2$substrato)
  rownames(sub2) <- rownames(amb2)
  
  ##variavel quantitativa (porcentagem de vegetaçao no interior)
  porcvegi2 <- data.frame(amb2$viporc)
  rownames(porcvegi2) <- rownames(amb2)
  
  #variavel quantitativa (vegetaçao no interior)
  vegi2 <- data.frame(amb2$vi)
  rownames(vegi2) <- rownames(amb2)
  
  #variavel quantitativa (porcentagem vegetaçao margens)
  porcvegm2 <- data.frame(amb2$vmporc)
  rownames(porcvegm2) <- rownames(amb2)
  
  #variavel quantitativa (vegetaçao margens)
  vegm2 <- data.frame(amb2$vm)
  rownames(vegm2) <- rownames(amb2)
  
  #variaveis binarias
  #para a caracteristica "hidroperiodo"
  hidrop2 <- prep.binary(amb2[,5:6], col.blocks = 2, label = "hidroperiodo")
  #para a carcteristica "tipo"
  tipo2 <- prep.binary(amb2[,3:4], col.blocks = 2, label = "tipo")
  
  #variaveis nominais
  #para a caracteristica "posiçao"
  posic2 <- data.frame(amb2$Posicao)
  
  ktab2 <- ktab.list.df(list(dimen2, marg2, sub2, porcvegi2, vegi2, porcvegm2, vegm2, hidrop2, tipo2, posic2))
  
  distamb2 <- dist.ktab(ktab2, c("Q", "Q", "Q", "Q", "Q", "Q","Q","B", "B", "N"), c("scaledBYrange"))
  distamb2
  
  #PCoA
  
  PCoA.res2 <- capscale(distamb2 ~ 1, distance = "bray") 
  summary(PCoA.res2)
  scores(PCoA.res2, display = "sites")
  scores2 <- scores(PCoA.res2, display = "sites")
  plot(PCoA.res2) 
  
  #PCNM
  PCNM2 <- read.csv("pcnm2.csv", head = T, sep = ",") 
  head(PCNM2)
  PCNM2 <- PCNM2[,-1]
  
  #Varpart
  pcnmas2 <- as.matrix(PCNM2)
  pcnmas2
  
  scoresas2 <- data.frame(scores2)
  scoresas2
  varp2 <- varpart(abund2, pcnmas2, scoresas2)
  varp2
  
  plot(varpart(abund2, scoresas2, pcnmas2), cutoff = -Inf, Xnames = NA)
  text(locator(1), "Env")
  text(locator(1), "Spat")
  plot(varp2)
  
  scoresf2 <- as.matrix(scoresas2)
  modrda2 <- rda(abund2 ~ pcnmas2 + scoresf2)
  modrda2
  modrdatest2 <- anova(modrda2, by = "terms")
  modrdatest2
}

########################################################
# Regiao 3

{
  #Matriz de composiçao
  ab3 <- subset(abund, abund$site == "3")
  head(ab3)
  abund3 <- ab3 # esta matriz ja esta com transformacao de Hellinger
  abund3 <- abund3[,-c(1,2)]
  head(abund3)
  #abund3<-decostand(ab3, "hellinger", MARGIN=2)
  #abund3
  
  #Matriz ambiental
  amb3 <- read.csv("amb3.csv", head=T, sep=";") 
  names(amb3)
  head(amb3)
  
  #Gower
  #variavel quantitativa (area)
  dimen3 <- data.frame(amb3$area)
  rownames(dimen3) <- rownames(amb3)
  
  #variavel quantitativa (margem)
  marg3 <- data.frame(amb3$margem)
  rownames(marg3) <-rownames(amb3)
  
  #variavel quantitativa (substrato)
  sub3 <- data.frame(amb3$substrato)
  rownames(sub3) <- rownames(amb3)
  
  ##variavel quantitativa (porcentagem de vegetaçao no interior)
  porcvegi3 <- data.frame(amb3$viporc)
  rownames(porcvegi3) <- rownames(amb3)
  
  #variavel quantitativa (vegetaçao no interior)
  vegi3 <- data.frame(amb3$vi)
  rownames(vegi3) <- rownames(amb3)
  
  #variavel quantitativa (porcentagem vegetaçao margens)
  porcvegm3 <- data.frame(amb3$vmporc)
  rownames(porcvegm3) <- rownames(amb3)
  
  #variavel quantitativa (vegetaçao margens)
  vegm3 <- data.frame(amb3$vm)
  rownames(vegm3) <- rownames(amb3)
  
  #variaveis binarias
  #para a caracteristica "hidroperiodo"
  hidrop3 <- prep.binary(amb3[,5:6], col.blocks = 2, label = "hidroperiodo")
  #para a carcteristica "tipo"
  tipo3 <- prep.binary(amb3[,3:4], col.blocks = 2, label = "tipo")
  
  #variaveis nominais
  #para a caracteristica "posiçao"
  posic3 <- data.frame(amb3$Posicao)
  
  ktab3 <- ktab.list.df(list(dimen3, marg3, sub3, porcvegi3, vegi3, porcvegm3, vegm3, hidrop3, tipo3, posic3))
  
  distamb3 <- dist.ktab(ktab3, c("Q", "Q", "Q", "Q", "Q", "Q","Q","B", "B", "N"), c("scaledBYrange"))
  distamb3
  
  
  #PCoA
  PCoA.res3 <- capscale(distamb3 ~ 1, distance = "bray") 
  summary(PCoA.res3)
  scores(PCoA.res3,display = "sites")
  scores3 <- scores(PCoA.res3, display = "sites")
  scores3
  plot(PCoA.res3) 
  
  scorestab3 <- write.table(scores3, file = "C:/Users/Hp/Documents/tabelas/regiao 3/scores3", sep=",", row.names=FALSE)
  scorestab3
  
  #PCNM
  PCNM3 <- read.csv("pcnm3.csv", head = T, sep = ",") 
  names(PCNM3)
  PCNM3 <- PCNM3[,-1]
  head(PCNM3)
  
  # Particao hierarquica
  exp3 <- data.frame(scores3, PCNM3)
  exp3
  
  # Calculando o indice de Shannon
  {
    abun1 <- c(7, 8, 2)
    tot <- sum(abun1)
    tot
    pi <- abun1/tot
    pi
    log.pi <- log(pi)
    log.pi
    pi.log.pi <- pi*log.pi
    pi.log.pi
    H1 <- - sum(pi.log.pi)
    H1
    
    abun2 <- c(3, 4, 3)
    tot <- sum(abun2)
    tot
    pi <- abun2/tot
    pi
    log.pi <- log(pi)
    log.pi
    pi.log.pi <- pi*log.pi
    pi.log.pi
    H2 <- - sum(pi.log.pi)
    H2
    
    abun3 <- c(14, 89, 47)
    tot <- sum(abun3)
    tot
    pi <- abun3/tot
    pi
    log.pi <- log(pi)
    log.pi
    pi.log.pi <- pi*log.pi
    pi.log.pi
    H3 <- - sum(pi.log.pi)
    H3
    
    abun4 <- c(3, 18, 1, 122, 1, 1)
    tot <- sum(abun4)
    tot
    pi <- abun4/tot
    pi
    log.pi <- log(pi)
    log.pi
    pi.log.pi <- pi*log.pi
    pi.log.pi
    H4 <- - sum(pi.log.pi)
    H4
    
    abun5 <- c(216, 9, 8, 15, 24, 246)
    tot <- sum(abun5)
    tot
    pi <- abun5/tot
    pi
    log.pi <- log(pi)
    log.pi
    pi.log.pi <- pi*log.pi
    pi.log.pi
    H5 <- - sum(pi.log.pi)
    H5
    
    abun6 <- c(10, 2, 7, 7, 41, 49, 1)
    tot <- sum(abun6)
    tot
    pi <- abun6/tot
    pi
    log.pi <- log(pi)
    log.pi
    pi.log.pi <- pi*log.pi
    pi.log.pi
    H6 <- - sum(pi.log.pi)
    H6
  }
  
  H1total <-c (H1,H2,H3,H4,H5,H6)
  H1total
  
  teste <- hier.part(H1total, exp3)
  teste
  prob <- rand.hp(H1total, exp3, num.reps = 1000)
  prob$Iprobs
}

########################################################
# Regiao 4
{
  #Matriz de composiçao
  ab4 <- subset(abund, abund$site == "4")
  head(ab4)
  abund4 <- ab4 # esta matriz ja esta com transformacao de Hellinger
  abund4 <- abund4[,-c(1,2)]
  head(abund4)
  
  #abund4<-decostand(ab4, "hellinger", MARGIN=2)
  #abund4
  
  #Matriz ambiental
  amb4 <- read.csv("amb4.csv", head=T, sep=";") 
  head(amb4)
  
  #Gower
  #variavel quantitativa (area)
  dimen4 <- data.frame(amb4$area)
  rownames(dimen4) <- rownames(amb4)
  
  #variavel quantitativa (margem)
  marg4 <- data.frame(amb4$margem)
  rownames(marg4) <-rownames(amb4)
  
  #variavel quantitativa (substrato)
  sub4 <- data.frame(amb4$substrato)
  rownames(sub4) <- rownames(amb4)
  
  ##variavel quantitativa (porcentagem de vegetaçao no interior)
  porcvegi4 <- data.frame(amb4$viporc)
  rownames(porcvegi4) <- rownames(amb4)
  
  #variavel quantitativa (vegetaçao no interior)
  vegi4 <- data.frame(amb4$vi)
  rownames(vegi4) <- rownames(amb4)
  
  #variavel quantitativa (porcentagem vegetaçao margens)
  porcvegm4 <- data.frame(amb4$vmporc)
  rownames(porcvegm4) <- rownames(amb4)
  
  #variavel quantitativa (vegetaçao margens)
  vegm4 <- data.frame(amb4$vm)
  rownames(vegm4) <- rownames(amb4)
  
  #variaveis binarias
  #para a caracteristica "hidroperiodo"
  hidrop4 <- prep.binary(amb4[,5:6], col.blocks = 2, label = "hidroperiodo")
  #para a carcteristica "tipo"
  tipo4 <- prep.binary(amb4[,3:4], col.blocks = 2, label = "tipo")
  
  #variaveis nominais
  #para a caracteristica "posiçao"
  posic4 <- data.frame(amb4$Posicao)
  
  ktab4 <- ktab.list.df(list(dimen4, marg4, sub4, porcvegi4, vegi4, porcvegm4, vegm4, hidrop4, tipo4, posic4))
  
  distamb4 <- dist.ktab(ktab4, c("Q", "Q", "Q", "Q", "Q", "Q","Q","B", "B", "N"), c("scaledBYrange"))
  distamb4
  
  #PCoA
  PCoA.res4 <- capscale(distamb4 ~ 1, distance = "bray") 
  summary(PCoA.res4)
  scores(PCoA.res4, display = "sites")
  scores4 <- scores(PCoA.res4, display = "sites")
  plot(PCoA.res4) 
  
  #PCNM
  PCNM4 <- read.csv("pcnm4.csv", head = T, sep = ",") 
  PCNM4 <- PCNM4[,-c(1,5)]
  head(PCNM4)
  
  #Varpart
  pcnmas4 <- as.matrix(PCNM4)
  pcnmas4
  
  scoresas4 <- data.frame(scores4)
  scoresas4
  varp4 <- varpart(abund4, pcnmas4, scoresas4)
  varp4
  plot(varpart(abund4, scoresas4, pcnmas4), cutoff = -Inf, Xnames = NA)
  text(locator(1), "Env")
  text(locator(1), "Spat")
  
  scoresf4 <- as.matrix(scoresas4)
  modrda4 <- rda(abund4 ~ pcnmas4 + scoresf4)
  modrdatest4 <- anova(modrda4, by = "terms")
  modrdatest4
}

########################################################
# Regiao 5
{
  #Matriz de composiçao
  ab5 <- subset(abund, abund$site == "5")
  ab5 <- ab5[,-c(1,2)]
  head(ab5)
  
  abund5 <- ab5 
  
  #abund5 <- decostand(ab5, "hellinger", MARGIN = 2)
  #abund5
  
  #Matriz ambiental
  amb5 <- read.csv("amb5.csv", head = T, sep = ";") 
  names(amb5)
  #Gower
  #variavel quantitativa (area)
  dimen5 <- data.frame(amb5$area)
  rownames(dimen5) <- rownames(amb5)
  
  #variavel quantitativa (margem)
  marg5 <- data.frame(amb5$margem)
  rownames(marg5) <- rownames(amb5)
  
  #variavel quantitativa (substrato)
  sub5 <- data.frame(amb5$substrato)
  rownames(sub5) <- rownames(amb5)
  
  ##variavel quantitativa (porcentagem de vegetaçao no interior)
  porcvegi5 <- data.frame(amb5$viporc)
  rownames(porcvegi5) <- rownames(amb5)
  
  #variavel quantitativa (vegetaçao no interior)
  vegi5 <- data.frame(amb5$vi)
  rownames(vegi5) <- rownames(amb5)
  
  #variavel quantitativa (porcentagem vegetaçao margens)
  porcvegm5 <- data.frame(amb5$vmporc)
  rownames(porcvegm5) <- rownames(amb5)
  
  #variavel quantitativa (vegetaçao margens)
  vegm5 <- data.frame(amb5$vm)
  rownames(vegm5) <- rownames(amb5)
  
  #variaveis binarias
  #para a caracteristica "hidroperiodo"
  hidrop5 <- prep.binary(amb5[,5:6], col.blocks = 2, label = "hidroperiodo")
  
  #para a carcteristica "tipo"
  tipo5<- prep.binary(amb5[,3:4], col.blocks = 2, label = "tipo")
  
  #variaveis nominais
  #para a caracteristica "posiçao"
  posic5<- data.frame(amb5$Posicao)
  
  ktab5 <- ktab.list.df(list(dimen5, marg5, sub5, porcvegi5, vegi5, porcvegm5, vegm5, hidrop5, tipo5, posic5))
  
  distamb5 <- dist.ktab(ktab5, c("Q", "Q", "Q", "Q", "Q", "Q","Q","B", "B", "N"), c("scaledBYrange"))
  distamb5
  
  #PCoA
  PCoA.res5 <- capscale(distamb5 ~ 1, distance = "bray") 
  summary(PCoA.res5)
  scores(PCoA.res5, display = "sites")
  scores5 <- scores(PCoA.res5, display = "sites")
  plot(PCoA.res5) 
  
  #PCNM
  PCNM5<-read.csv("pcnm5.csv", head = T, sep=",") 
  head(PCNM5)
  PCNM5 <- PCNM5[,-c(1)]
  names(PCNM5)
  
  #Varpart
  pcnmas5 <- as.matrix(PCNM5)
  pcnmas5
  
  scoresas5 <- data.frame(scores5)
  scoresas5
  varp5 <- varpart(abund5, pcnmas5, scoresas5)
  varp5
  plot(varpart(abund5, scoresas5, pcnmas5), cutoff = -Inf, Xnames = NA)
  text(locator(1), "Env")
  text(locator(1), "Spat")
  
  scoresf5 <- as.matrix(scoresas5)
  modrda5 <- rda(abund5 ~ pcnmas5 + scoresf5)
  modrdatest5 <- anova(modrda5, by = "terms")
  modrdatest5
}

########################################################
# Regiao 6
{
  #Matriz de composiçao
  ab6 <- subset(abund, abund$site == "6")
  ab6 <- ab6[,-c(1,2)]
  head(ab6)
  abund6 <- ab6
  
  #abund6 <- decostand(ab6, "hellinger", MARGIN=2)
  #abund6
  
  #Matriz ambiental
  amb6 <- read.csv("amb6.csv", head = T, sep = ";") 
  head(amb6)
  
  #Gower
  #variavel quantitativa (area)
  dimen6 <- data.frame(amb6$area)
  rownames(dimen6) <- rownames(amb6)
  
  #variavel quantitativa (margem)
  marg6 <- data.frame(amb6$margem)
  rownames(marg6) <- rownames(amb6)
  
  #variavel quantitativa (substrato)
  sub6 <- data.frame(amb6$substrato)
  rownames(sub6) <- rownames(amb6)
  
  ##variavel quantitativa (porcentagem de vegetaçao no interior)
  porcvegi6 <- data.frame(amb6$viporc)
  rownames(porcvegi6) <- rownames(amb6)
  
  #variavel quantitativa (vegetaçao no interior)
  vegi6 <- data.frame(amb6$vi)
  rownames(vegi6) <- rownames(amb6)
  
  #variavel quantitativa (porcentagem vegetaçao margens)
  porcvegm6 <- data.frame(amb6$vmporc)
  rownames(porcvegm6) <- rownames(amb6)
  
  #variavel quantitativa (vegetaçao margens)
  vegm6 <- data.frame(amb6$vm)
  rownames(vegm6) <- rownames(amb6)
  
  #variaveis binarias
  #para a caracteristica "hidroperiodo"
  hidrop6 <- prep.binary(amb6[,5:6], col.blocks = 2, label = "hidroperiodo")
  
  #para a carcteristica "tipo"
  tipo6 <- prep.binary(amb6[,3:4], col.blocks = 2, label = "tipo")
  
  #variaveis nominais
  #para a caracteristica "posiçao"
  posic6 <- data.frame(amb6$Posicao)
  
  ktab6 <- ktab.list.df(list(dimen6, marg6, sub6, porcvegi6, vegi6, porcvegm6, vegm6, hidrop6, tipo6, posic6))
  
  distamb6 <- dist.ktab(ktab6, c("Q", "Q", "Q", "Q", "Q", "Q","Q","B", "B", "N"), c("scaledBYrange"))
  distamb6
  
  #PCoA
  PCoA.res6 <- capscale(distamb6 ~ 1, distance = "bray") 
  summary(PCoA.res6)
  scores(PCoA.res6,display = "sites")
  scores6 <- scores(PCoA.res6,display = "sites")
  plot(PCoA.res6) 
  
  #PCNM
  PCNM6 <- read.csv("pcnm6.csv", head = T, sep = ",") 
  PCNM6 <- PCNM6[,-c(1,6:7)]
  names(PCNM6)
  head(PCNM6)
  
  #Varpart
  pcnmas6 <- as.matrix(PCNM6)
  pcnmas6
  
  scoresas6 <- data.frame(scores6)
  scoresas6
  varp6 <- varpart(ab6, pcnmas6, scoresas6)
  varp6
  
  plot(varpart(abund6, scoresas6, pcnmas6), cutoff = -Inf, Xnames = NA)
  text(locator(1), "Env")
  text(locator(1), "Spat")
  
  scoresf6 <- as.matrix(scoresas6)
  modrda6 <- rda(abund6 ~ pcnmas6 + scoresf6)
  modrdatest6 <- anova(modrda6, by = "terms")
  modrdatest6
}

########################################################
# Geral
{
  #Matriz de composiçao
  abund7 <- read.csv("~/Documents/Manuscritos_em_producao/Manuscrito_Fernanda_comunidades_girinos_goias/Analises/data/abund.csv", header = TRUE, sep = ";")
  head(abund7)
  abund7 <- abund7[,-c(1,2)]
  head(abund7)
  
  #Matriz ambiental
  list.files()
  amb7 <- read.csv("amb_all.csv", header = TRUE, sep=";") 
  head(amb7)
  
  #Gower
  #variavel quantitativa (area)
  dimen7 <- data.frame(amb7$area)
  rownames(dimen7) <- rownames(amb7)
  
  #variavel quantitativa (margem)
  marg7 <- data.frame(amb7$margem)
  rownames(marg7) <- rownames(amb7)
  
  #variavel quantitativa (substrato)
  sub7 <- data.frame(amb7$substrato)
  rownames(sub7) <- rownames(amb7)
  
  ##variavel quantitativa (porcentagem de vegetaçao no interior)
  porcvegi7 <- data.frame(amb7$viporc)
  rownames(porcvegi7) <- rownames(amb7)
  
  #variavel quantitativa (vegetaçao no interior)
  vegi7 <- data.frame(amb7$vi)
  rownames(vegi7) <- rownames(amb7)
  
  #variavel quantitativa (porcentagem vegetaçao margens)
  porcvegm7 <- data.frame(amb7$vmporc)
  rownames(porcvegm7) <- rownames(amb7)
  
  #variavel quantitativa (vegetaçao margens)
  vegm7 <- data.frame(amb7$vm)
  rownames(vegm7) <- rownames(amb7)
  
  #variaveis binarias
  #para a caracteristica "hidroperiodo"
  hidrop7 <- prep.binary(amb7[,5:6], col.blocks = 2, label = "hidroperiodo")
  #para a carcteristica "tipo"
  tipo7<- prep.binary(amb7[,3:4], col.blocks = 2, label = "tipo")
  
  #variaveis nominais
  #para a caracteristica "posiçao"
  posic7<- data.frame(amb7$Posicao)
  
  ktab7 <- ktab.list.df(list(dimen7, marg7, sub7, porcvegi7, vegi7, porcvegm7, vegm7, hidrop7, tipo7, posic7))
  
  distamb7 <- dist.ktab(ktab7, c("Q", "Q", "Q", "Q", "Q", "Q","Q","B", "B", "N"), c("scaledBYrange"))
  distamb7
  
  #PCoA ambiental
  PCoA.res7 <- capscale(distamb7~ 1 , distance = "bray") 
  summary(PCoA.res7)
  PCoA.res7$Ybar
  scores(PCoA.res7, display = "sites")
  scores7 <- scores(PCoA.res7, display = "sites")
  plot(PCoA.res7) 
  
  #PCNM
  PCNM7 <- read.csv("pcnm_all.csv", head = T, sep = ",") 
  head(PCNM7)
  PCNM7 <- PCNM7[,-c(1,14:19)]
  head(PCNM7)
  
  #Varpart
  pcnmas7 <- as.matrix(PCNM7)
  pcnmas7
  
  scoresas7 <- as.matrix(scores7)
  scoresas7
  varp7 <- varpart(abund7, scoresas7, pcnmas7)
  varp7
  showvarparts(2)
  plot(varp7)
  
  # plot(varpart(abund7, scoresas7, pcnmas7), cutoff = -Inf, Xnames = NA)
  # text(locator(1), "Env")
  # text(locator(1), "Spat")
  
  scoresf7 <- as.matrix(scoresas7)
  modrda7 <- rda(abund7 ~ pcnmas7 + scores7)
  modrdatest7 <- anova(modrda7, by = "terms")
  modrdatest7
}
