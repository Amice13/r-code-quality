##############################################
#### Cliente: TC923 - Julio Cesar Silva    ###
#### Início: 02/07/2019                    ###
#### Autora: Luiz Henrique                 ###
##############################################

#### =============================
#### Preparando o R para análise
#### =============================

rm(list=ls())
options(OutDec=",")
source("Y:/02 - Produção Científica - Trabalhos/Funções ABG.R")
setwd("Y:/02 - Produção Científica - Trabalhos/TC923 - Julio Cesar Silva/Gráficos 2")

#### ========
#### Pacotes
#### ========

require(tm)
require(ggplot2)
require(pdp)
require(psych)
require(psy)
require(nFactors)
require(fmsb)
require(Hmisc)
require(dplyr)
require(stats)
require(corrplot)

### ========
### Funções
### ========

fa1 <- function(x){
  Mat<- cbind(principal(x,1, rotate="varimax")$loadings
              ,principal(x,1, rotate="varimax")$communality
              ,principal(x,1, rotate="varimax")$weights)
  colnames(Mat)<- c("C.F.","Com.","Peso")
  Mat
}

mean2 <- function(x){
  media <- mean(x, na.rm = TRUE)
  media
}

#### ================
#### Banco de dados
#### ================

Dados.socio <- read.csv2("Y:/02 - Produção Científica - Trabalhos/TC923 - Julio Cesar Silva/Bancos de dados/Dados sociodemográficos.csv")
Dados.dom <- read.csv2("Y:/02 - Produção Científica - Trabalhos/TC923 - Julio Cesar Silva/Bancos de dados/Dados domínios.csv")
Dados.qvfi3 <- read.csv2("Y:/02 - Produção Científica - Trabalhos/TC923 - Julio Cesar Silva/Bancos de dados/Dados QVFI-3.csv")
Dados.qvfi7 <- read.csv2("Y:/02 - Produção Científica - Trabalhos/TC923 - Julio Cesar Silva/Bancos de dados/Dados QVFI-7.csv")

###========================================
### Descrição e teste Stress geral e defesa
###========================================

wilcox.test(Dados.dom$GERAL.STRESS, mu = 85, alternative = "less")
wilcox.test(Dados.dom$POSTURA.DEFESA, mu = 24, alternative = "greater")

rbind(
basic.np(Dados.dom$GERAL.STRESS)
,basic.np(Dados.dom$POSTURA.DEFESA))

GERAL.STRESS.CAT <- ifelse(Dados.dom$GERAL.STRESS>=85, "Acima", "Abaixo")
Dados.dom$GERAL.STRESS.CAT <- GERAL.STRESS.CAT

POSTURA.DEFESA.CAT <- ifelse(Dados.dom$POSTURA.DEFESA>=24, "Acima", "Abaixo")
Dados.dom$POSTURA.DEFESA.CAT <- POSTURA.DEFESA.CAT

rbind(
tab(Dados.dom$GERAL.STRESS.CAT)
,tab(Dados.dom$POSTURA.DEFESA.CAT))

### =======================
### Arrumando as variáveis
### =======================

Recursos.0 <- ifelse(Dados.socio$ESCOLA.RECURSOS == 0, "nenhum",
                     ifelse(Dados.socio$ESCOLA.RECURSOS == 1, "acessibilidade",
                            ifelse(Dados.socio$ESCOLA.RECURSOS == 3, "acessibilidade,livrosembraile",
                                   ifelse(Dados.socio$ESCOLA.RECURSOS == 4, "assistente",
                                          ifelse(Dados.socio$ESCOLA.RECURSOS == 5, "acessibilidade,assistente",
                                                 ifelse(Dados.socio$ESCOLA.RECURSOS == 6, "livrosembraile,assistente",
                                                        ifelse(Dados.socio$ESCOLA.RECURSOS == 7, "acessibilidade,livrosembraile,assistente",
                                                               ifelse(Dados.socio$ESCOLA.RECURSOS == 8, "computador",
                                                                      ifelse(Dados.socio$ESCOLA.RECURSOS == 9, "acessibilidade,computador",
                                                                             ifelse(Dados.socio$ESCOLA.RECURSOS == 12, "assistente,computador",
                                                                                    ifelse(Dados.socio$ESCOLA.RECURSOS == 14, "livrosembraile,assistente,computador","acessibilidade,livrosembraile,assistente,computador")))))))))))
Recursos.1 <- Corpus(VectorSource(Recursos.0))
Recursos.2 <- DocumentTermMatrix(Recursos.1)
Recursos.3 <- as.matrix(Recursos.2)
apply(Recursos.3, 2, sum)

acessibilidade.recursos <- Recursos.3[,"acessibilidade,livrosembraile,assistente,computador"]+ 
  Recursos.3[,"acessibilidade,computador"]+
  Recursos.3[,"acessibilidade"]+
  Recursos.3[,"acessibilidade,assistente"]+
  Recursos.3[,"acessibilidade,livrosembraile"]+
  Recursos.3[,"acessibilidade,livrosembraile,assistente"]; Dados.socio$acessibilidade.recursos <- acessibilidade.recursos

assistente.recursos <- Recursos.3[,"acessibilidade,livrosembraile,assistente,computador"]+
  Recursos.3[,"assistente"]+
  Recursos.3[,"livrosembraile,assistente,computador"]+
  Recursos.3[,"acessibilidade,assistente"]+
  Recursos.3[,"livrosembraile,assistente"]+
  Recursos.3[,"assistente,computador"]+
  Recursos.3[,"acessibilidade,livrosembraile,assistente"]; Dados.socio$assistente.recursos <- assistente.recursos

computador.recursos <- Recursos.3[,"acessibilidade,livrosembraile,assistente,computador"]+
  Recursos.3[,"computador"]+
  Recursos.3[,"livrosembraile,assistente,computador"]+
  Recursos.3[,"acessibilidade,computador"]+
  Recursos.3[,"assistente,computador"]; Dados.socio$computador.recursos <- computador.recursos

livrosembraile.recursos <- Recursos.3[,"acessibilidade,livrosembraile,assistente,computador"]+
  Recursos.3[,"livrosembraile,assistente,computador"]+
  Recursos.3[,"livrosembraile,assistente"]+
  Recursos.3[,"acessibilidade,livrosembraile"]+
  Recursos.3[,"acessibilidade,livrosembraile,assistente"]; Dados.socio$livrosembraile.recursos <- livrosembraile.recursos

nenhum.recursos <- Recursos.3[,"nenhum"]; Dados.socio$nenhum.recursos <- nenhum.recursos

Dados.qvfi3$Questao3 <- ifelse(is.na(Dados.qvfi3$Q3) == TRUE, mean2(Dados.qvfi3$Q3), Dados.qvfi3$Q3)
Dados.qvfi3$Questao9 <- ifelse(is.na(Dados.qvfi3$Q9) == TRUE, mean2(Dados.qvfi3$Q9), Dados.qvfi3$Q9)
Dados.qvfi3$Questao12 <- ifelse(is.na(Dados.qvfi3$Q12) == TRUE, mean2(Dados.qvfi3$Q12), Dados.qvfi3$Q12)
Dados.qvfi3$Questao13 <- ifelse(is.na(Dados.qvfi3$Q13) == TRUE, mean2(Dados.qvfi3$Q13), Dados.qvfi3$Q13)
Dados.qvfi3$Questao15 <- ifelse(is.na(Dados.qvfi3$Q15) == TRUE, mean2(Dados.qvfi3$Q15), Dados.qvfi3$Q15)
Dados.qvfi3$Questao16 <- ifelse(is.na(Dados.qvfi3$Q16) == TRUE, mean2(Dados.qvfi3$Q16), Dados.qvfi3$Q16)
Dados.qvfi3$Questao17 <- ifelse(is.na(Dados.qvfi3$Q17) == TRUE, mean2(Dados.qvfi3$Q17), Dados.qvfi3$Q17)
Dados.qvfi3$Questao18 <- ifelse(is.na(Dados.qvfi3$Q18) == TRUE, mean2(Dados.qvfi3$Q18), Dados.qvfi3$Q18)
Dados.qvfi3$Questao20 <- ifelse(is.na(Dados.qvfi3$Q20) == TRUE, mean2(Dados.qvfi3$Q20), Dados.qvfi3$Q20)
#Dados.qvfi3$Questao21 <- ifelse(is.na(Dados.qvfi3$Q21) == TRUE, mean2(Dados.qvfi3$Q21), Dados.qvfi3$Q21)# Remover essa questão, pois só tem 1 observação!
Dados.qvfi3$Questao22 <- ifelse(is.na(Dados.qvfi3$Q22) == TRUE, mean2(Dados.qvfi3$Q22), Dados.qvfi3$Q22)
Dados.qvfi3$Questao23 <- ifelse(is.na(Dados.qvfi3$Q23) == TRUE, mean2(Dados.qvfi3$Q23), Dados.qvfi3$Q23)
Dados.qvfi3$Questao24 <- ifelse(is.na(Dados.qvfi3$Q24) == TRUE, mean2(Dados.qvfi3$Q24), Dados.qvfi3$Q24)
Dados.qvfi3$Questao25 <- ifelse(is.na(Dados.qvfi3$Q25) == TRUE, mean2(Dados.qvfi3$Q25), Dados.qvfi3$Q25)
Dados.qvfi3$Questao26 <- ifelse(is.na(Dados.qvfi3$Q26) == TRUE, mean2(Dados.qvfi3$Q26), Dados.qvfi3$Q26)
Dados.qvfi3$Questao27 <- ifelse(is.na(Dados.qvfi3$Q27) == TRUE, mean2(Dados.qvfi3$Q27), Dados.qvfi3$Q27)
Dados.qvfi3$Questao28 <- ifelse(is.na(Dados.qvfi3$Q28) == TRUE, mean2(Dados.qvfi3$Q28), Dados.qvfi3$Q28)
Dados.qvfi3$Questao29 <- ifelse(is.na(Dados.qvfi3$Q29) == TRUE, mean2(Dados.qvfi3$Q29), Dados.qvfi3$Q29)
Dados.qvfi3$Questao31 <- ifelse(is.na(Dados.qvfi3$Q31) == TRUE, mean2(Dados.qvfi3$Q31), Dados.qvfi3$Q31)
Dados.qvfi3$Questao32 <- ifelse(is.na(Dados.qvfi3$Q32) == TRUE, mean2(Dados.qvfi3$Q32), Dados.qvfi3$Q32)
Dados.qvfi3$Questao33 <- ifelse(is.na(Dados.qvfi3$Q33) == TRUE, mean2(Dados.qvfi3$Q33), Dados.qvfi3$Q33)
Dados.qvfi3$Questao34 <- ifelse(is.na(Dados.qvfi3$Q34) == TRUE, mean2(Dados.qvfi3$Q34), Dados.qvfi3$Q34)
Dados.qvfi3$Questao35 <- ifelse(is.na(Dados.qvfi3$Q35) == TRUE, mean2(Dados.qvfi3$Q35), Dados.qvfi3$Q35)

Dados.qvfi7$Questao3 <- ifelse(is.na(Dados.qvfi7$Q3) == TRUE, mean2(Dados.qvfi7$Q3), Dados.qvfi7$Q3)
Dados.qvfi7$Questao5 <- ifelse(Dados.qvfi7$Q5 == "75/50", 62.5, as.numeric(as.character(Dados.qvfi7$Q5)))
Dados.qvfi7$Questao16 <- ifelse(is.na(Dados.qvfi7$Q16) == TRUE, mean2(Dados.qvfi7$Q16), Dados.qvfi7$Q16)
Dados.qvfi7$Questao17 <- ifelse(is.na(Dados.qvfi7$Q17) == TRUE, mean2(Dados.qvfi7$Q17), Dados.qvfi7$Q17)
Dados.qvfi7$Questao18 <- ifelse(is.na(Dados.qvfi7$Q18) == TRUE, mean2(Dados.qvfi7$Q18), Dados.qvfi7$Q18)
Dados.qvfi7$Questao19 <- ifelse(is.na(Dados.qvfi7$Q19) == TRUE, mean2(Dados.qvfi7$Q19), Dados.qvfi7$Q19)
Dados.qvfi7$Questao20 <- ifelse(is.na(Dados.qvfi7$Q20) == TRUE, mean2(Dados.qvfi7$Q20), Dados.qvfi7$Q20)
Dados.qvfi7$Questao21 <- ifelse(is.na(Dados.qvfi7$Q21) == TRUE, mean2(Dados.qvfi7$Q21), Dados.qvfi7$Q21)
Dados.qvfi7$Questao22 <- ifelse(is.na(Dados.qvfi7$Q22) == TRUE, mean2(Dados.qvfi7$Q22), Dados.qvfi7$Q22)
Dados.qvfi7$Questao23 <- ifelse(is.na(Dados.qvfi7$Q23) == TRUE, mean2(Dados.qvfi7$Q23), Dados.qvfi7$Q23)
Dados.qvfi7$Questao24 <- ifelse(is.na(Dados.qvfi7$Q24) == TRUE, mean2(Dados.qvfi7$Q24), Dados.qvfi7$Q24)
Dados.qvfi7$Questao25 <- ifelse(is.na(Dados.qvfi7$Q25) == TRUE, mean2(Dados.qvfi7$Q25), Dados.qvfi7$Q25)
Dados.qvfi7$Questao26 <- ifelse(is.na(Dados.qvfi7$Q26) == TRUE, mean2(Dados.qvfi7$Q26), Dados.qvfi7$Q26)
Dados.qvfi7$Questao27 <- ifelse(is.na(Dados.qvfi7$Q27) == TRUE, mean2(Dados.qvfi7$Q27), Dados.qvfi7$Q27)
Dados.qvfi7$Questao29 <- ifelse(is.na(Dados.qvfi7$Q29) == TRUE, mean2(Dados.qvfi7$Q29), Dados.qvfi7$Q29)
Dados.qvfi7$Questao30 <- ifelse(is.na(Dados.qvfi7$Q30) == TRUE, mean2(Dados.qvfi7$Q30), Dados.qvfi7$Q30)
Dados.qvfi7$Questao32 <- ifelse(is.na(Dados.qvfi7$Q32) == TRUE, mean2(Dados.qvfi7$Q32), Dados.qvfi7$Q32)
Dados.qvfi7$Questao33 <- ifelse(is.na(Dados.qvfi7$Q33) == TRUE, mean2(Dados.qvfi7$Q33), Dados.qvfi7$Q33)
Dados.qvfi7$Questao34 <- ifelse(is.na(Dados.qvfi7$Q34) == TRUE, mean2(Dados.qvfi7$Q34), Dados.qvfi7$Q34)
Dados.qvfi7$Questao36 <- ifelse(is.na(Dados.qvfi7$Q36) == TRUE, mean2(Dados.qvfi7$Q36), Dados.qvfi7$Q36)
Dados.qvfi7$Questao37 <- ifelse(is.na(Dados.qvfi7$Q37) == TRUE, mean2(Dados.qvfi7$Q37), Dados.qvfi7$Q37)
Dados.qvfi7$Questao38 <- ifelse(is.na(Dados.qvfi7$Q38) == TRUE, mean2(Dados.qvfi7$Q38), Dados.qvfi7$Q38)
Dados.qvfi7$Questao39 <- ifelse(is.na(Dados.qvfi7$Q39) == TRUE, mean2(Dados.qvfi7$Q39), Dados.qvfi7$Q39)
Dados.qvfi7$Questao40 <- ifelse(is.na(Dados.qvfi7$Q40) == TRUE, mean2(Dados.qvfi7$Q40), Dados.qvfi7$Q40)

Dados.socio$PERIODO.AT..LAZER.2 <- ifelse(Dados.socio$PERIODO.AT..LAZER == 60, 1,
                                          ifelse(Dados.socio$PERIODO.AT..LAZER == 30, 2,
                                                 ifelse(Dados.socio$PERIODO.AT..LAZER == 15, 3, 4)))

Dados.socio$RENDA.FAMILIAR.2 <- ifelse(Dados.socio$RENDA.FAMILIAR == 6, 1,
                                       ifelse(Dados.socio$RENDA.FAMILIAR == 5, 2,
                                              ifelse(Dados.socio$RENDA.FAMILIAR == 4, 3,
                                                     ifelse(Dados.socio$RENDA.FAMILIAR == 3, 4, NA))))

#### ===================
#### Análise descritiva
#### ===================

################################################### Parte 1 #############################################################

### Variáveis sociodemográficas

# Tabela 2 - Qualitativas

rbind(tab(Dados.socio$GENERO.FILHO.A.),
      tab(Dados.socio$FILHO.ESTUDA.),
      tab(Dados.socio$ESCOLA.ESPECIAL.),
rbind(tab(Dados.socio$acessibilidade.recursos),
      tab(Dados.socio$assistente.recursos),
      tab(Dados.socio$computador.recursos),
      tab(Dados.socio$livrosembraile.recursos),
      tab(Dados.socio$nenhum.recursos)),
      tab(Dados.socio$ATIVIDADE.FÍSICA.),
      tab(Dados.socio$ATIVIDADE.DE.LAZER),
      tab(Dados.socio$PERIODO.AT..LAZER),
      tab(Dados.socio$TRABALHA.FORA.),
      tab(Dados.socio$RENDA.FAMILIAR),
      tab(Dados.socio$ESTADO.CIVIL),
      tab(Dados.socio$VIVE.COM.PAI.),
      tab(Dados.socio$TRABALHO.INFORMAL.),
      tab(Dados.socio$BENEFICIO.SOCIAL.),
      tab(Dados.socio$PLANO.SAÚDE),
      tab(Dados.socio$GANHOU.PESO.),
      tab(Dados.socio$APETITE),
      tab(Dados.socio$DOR.DE.CABEÇA),
      tab(Dados.socio$TONTURA),
      tab(Dados.socio$NÁUSEA),
      tab(Dados.socio$INTOLERÂNCIA.À.LUZ),
      tab(Dados.socio$TRISTEZA),
      tab(Dados.socio$ESQUECIMENTO),
      tab(Dados.socio$CANSAÇO),
      tab(Dados.socio$DORES.NO.CORPO),
      tab(Dados.socio$INSÔNIA),
      tab(Dados.socio$DIFICULDADE.PEGAR.NO.SONO),
      tab(Dados.socio$ACORDA.NA.MADRUGADA),
      tab(Dados.socio$QUEDA.CABELO),
      tab(Dados.socio$HIPERTENSÃO),
      tab(Dados.socio$AUMENTO.GLICEMIA))

# Tabela 3 - Quantitativas

rbind(basic(Dados.socio$IDADE.CRIANÇA),
      basic(Dados.socio$MINUTOS.AT..FISICA),
      basic(Dados.socio$FREQ..AT..FÍSICA),
      basic(Dados.socio$PESO.DA.MÃE..Kg.),
      basic(Dados.socio$ALTURA.MÃE.cm.),
      basic(Dados.socio$IMC.MÃE),
      basic(Dados.socio$IDADE..MÃE),
      basic(Dados.socio$CARGA.HORÁRIA),
      basic(Dados.socio$QTDE.PESO),
      basic(Dados.socio$TEMPO.PESO))

### Indicador domínio da criança completo

DI <- cbind(Dados.dom[,5:13])
RE <- cbind(Dados.dom[,14:19])
MO <- cbind(Dados.dom[,20:24])
AC <- cbind(Dados.dom[,25:31])
AD <- cbind(Dados.dom[,32:42])
DE <- cbind(Dados.dom[,43:51])

set.seed(3000)
t1 = t(apply(DI, 2, basic.np))

set.seed(3000)
t2 = t(apply(RE, 2, basic.np))

set.seed(3000)
t3 = t(apply(MO, 2, basic.np))

set.seed(3000)
t4 = t(apply(AC, 2, basic.np))

set.seed(3000)
t5 = t(apply(AD, 2, basic.np))

set.seed(3000)
t6 = t(apply(DE, 2, basic.np))

# Tabela 4 - Indicador domínio da criança completo

set.seed(3000)
rbind(t1, basic.np(Dados.dom$Percentil.DI), 
      t2, basic.np(Dados.dom$Percentil.RE), 
      t3, basic.np(Dados.dom$Percentil.MO), 
      t4, basic.np(Dados.dom$Percentil.AC), 
      t5, basic.np(Dados.dom$Percentil.AD),
      t6, basic.np(Dados.dom$Percentil.DE),
      basic.np(Dados.dom$Percentil.CD))

### Indicador domínio dos pais completo

CO <- cbind(Dados.dom[,52:64])
AT <- cbind(Dados.dom[,65:71])
RO <- cbind(Dados.dom[,72:78])
DP <- cbind(Dados.dom[,79:87])
SP <- cbind(Dados.dom[,88:94])
IS <- cbind(Dados.dom[,95:100])
HE <- cbind(Dados.dom[,101:105])

set.seed(3000)
t7 = t(apply(CO, 2, basic.np))

set.seed(3000)
t8 = t(apply(AT, 2, basic.np))

set.seed(3000)
t9 = t(apply(RO, 2, basic.np))

set.seed(3000)
t10 = t(apply(DP, 2, basic.np))

set.seed(3000)
t11 = t(apply(SP, 2, basic.np))

set.seed(3000)
t12 = t(apply(IS, 2, basic.np))

set.seed(3000)
t13 = t(apply(HE, 2, basic.np))

# Tabela 5 - Indicador domínio dos pais completo

set.seed(3000)
rbind(t7, basic.np(Dados.dom$Percentil.CO),
      t8, basic.np(Dados.dom$Percentil.AT),
      t9, basic.np(Dados.dom$Percentil.RO),
      t10, basic.np(Dados.dom$Percentil.DP),
      t11, basic.np(Dados.dom$Percentil.SP),
      t12, basic.np(Dados.dom$Percentil.IS),
      t13, basic.np(Dados.dom$Percentil.HE),
      basic.np(Dados.dom$Percentil.PD))

### Indicador domínio do estresse da vida

LS <- cbind(Dados.dom[,106:124])

set.seed(3000)
t14 = t(apply(LS, 2, basic.np))

# Tabela 6 - Indicador domínio do estresse da vida

set.seed(3000)
rbind(t14, basic.np(Dados.dom$Soma.LS), basic.np(Dados.dom$Percentil.estresse.vida))

### Indicadores QVFI-3

# Saúde geral da visão

Saude.geral.da.visao.3 <- cbind(Dados.qvfi3$Q2, Dados.qvfi3$Questao3)
set.seed(3000)
t15 <- t(apply(Saude.geral.da.visao.3, 2, basic.np))
colnames(Saude.geral.da.visao.3) <- c("Q2", "Q3")

# Impacto familiar

Impacto.familiar.3 <- cbind(Dados.qvfi3$Q4, Dados.qvfi3$Q5, Dados.qvfi3$Q6, Dados.qvfi3$Q7,
                            Dados.qvfi3$Q8, Dados.qvfi3$Questao9, Dados.qvfi3$Q10, Dados.qvfi3$Questao12)
set.seed(3000)
t16 <- t(apply(Impacto.familiar.3, 2, basic.np))
colnames(Impacto.familiar.3) <- c("Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q12")

# Personalidade

Personalidade.3 <- cbind(Dados.qvfi3$Q11, Dados.qvfi3$Q14, Dados.qvfi3$Questao22, Dados.qvfi3$Questao24,
                         Dados.qvfi3$Questao25)
set.seed(3000)
t17 <- t(apply(Personalidade.3, 2, basic.np))
colnames(Personalidade.3) <- c("Q11", "Q14", "Q22", "Q24", "Q25")

# Competência

Competência.3 <- cbind(Dados.qvfi3$Questao13, Dados.qvfi3$Questao15, Dados.qvfi3$Questao16, Dados.qvfi3$Questao17,
                       Dados.qvfi3$Questao18, Dados.qvfi3$Q19, Dados.qvfi3$Questao20, Dados.qvfi3$Questao23, 
                       Dados.qvfi3$Questao26, Dados.qvfi3$Questao27, Dados.qvfi3$Questao28, Dados.qvfi3$Questao29)
set.seed(3000)
t18 <- t(apply(Competência.3, 2, basic.np))
colnames(Competência.3) <- c("Q13", "Q15", "Q16", "Q17", "Q18", "Q19", "Q20", "Q23", "Q26", "Q27", "Q28", "Q29")

# Tratamento

Tratamento.3 <- cbind(Dados.qvfi3$Questao31, Dados.qvfi3$Questao32, Dados.qvfi3$Questao33, Dados.qvfi3$Questao34,
                      Dados.qvfi3$Questao35)
set.seed(3000)
t19 <- t(apply(Tratamento.3, 2, basic.np))
colnames(Tratamento.3) <- c("Q31", "Q32", "Q33", "Q34", "Q35")

# Geral

qvfi3.1 <- apply(Saude.geral.da.visao.3, 1, mean2); Dados.qvfi3$qvfi3.1 <- qvfi3.1
qvfi3.2 <- apply(Impacto.familiar.3, 1, mean2); Dados.qvfi3$qvfi3.2 <- qvfi3.2
qvfi3.3 <- apply(Personalidade.3, 1, mean2); Dados.qvfi3$qvfi3.3 <- qvfi3.3
qvfi3.4 <- apply(Competência.3, 1, mean2); Dados.qvfi3$qvfi3.4 <- qvfi3.4
qvfi3.5 <- apply(Tratamento.3, 1, mean2); Dados.qvfi3$qvfi3.5 <- qvfi3.5

Geral.3 <- cbind(Dados.qvfi3$Q1, qvfi3.1, qvfi3.2, qvfi3.3, qvfi3.4, qvfi3.5)
qvfi3.6 <- apply(Geral.3, 1, mean2); Dados.qvfi3$qvfi3.6 <- qvfi3.6

# Tabela 7 - Indicador QVFI-3

set.seed(3000)
rbind(basic.np(Dados.qvfi3$Q1),
      t15, basic.np(qvfi3.1),
      t16, basic.np(qvfi3.2),
      t17, basic.np(qvfi3.3),
      t18, basic.np(qvfi3.4),
      t19, basic.np(qvfi3.5),
      basic.np(qvfi3.6))


qvfi3.graf <- cbind(Dados.qvfi3$Q1, 
                    Saude.geral.da.visao.3, 
                    Impacto.familiar.3, 
                    Personalidade.3, 
                    Competência.3, 
                    Tratamento.3)

set.seed(3000)
t3.new <- t(apply(qvfi3.graf, 2, basic.np))
rownames(t3.new)[1] <- c("Q1")

### Indicadores QVFI-7

# Saúde geral da visão

Saude.geral.da.visao.7 <- data.frame(Dados.qvfi7$Q2, Dados.qvfi7$Questao3)
set.seed(3000)
t20 <- t(apply(Saude.geral.da.visao.7, 2, basic.np))
colnames(Saude.geral.da.visao.7) <- c("Q2", "Q3")

# Impacto familiar

Impacto.familiar.7 <- data.frame(Dados.qvfi7$Q4, Dados.qvfi7$Questao5, Dados.qvfi7$Q6, Dados.qvfi7$Q7,
                              Dados.qvfi7$Q8, Dados.qvfi7$Q10, Dados.qvfi7$Q11)
set.seed(3000)
t21 <- t(apply(Impacto.familiar.7, 2, basic.np))
colnames(Impacto.familiar.7) <- c("Q4", "Q5", "Q6", "Q7", "Q8", "Q10", "Q11")

# Personalidade

Personalidade.7 <- data.frame(Dados.qvfi7$Q9, Dados.qvfi7$Q12, Dados.qvfi7$Q13, Dados.qvfi7$Q14, 
                             Dados.qvfi7$Questao26, Dados.qvfi7$Questao29, Dados.qvfi7$Questao30, Dados.qvfi7$Q31,
                             Dados.qvfi7$Questao32)
set.seed(3000)
t22 <- t(apply(Personalidade.7, 2, basic.np))
colnames(Personalidade.7) <- c("Q9", "Q12", "Q13", "Q14", "Q26", "Q29", "Q30", "Q31", "Q32")

# Competência

Competência.7 <- data.frame(Dados.qvfi7$Q15, Dados.qvfi7$Questao16, Dados.qvfi7$Questao17, Dados.qvfi7$Questao18,
                           Dados.qvfi7$Questao19, Dados.qvfi7$Questao20, Dados.qvfi7$Questao21, Dados.qvfi7$Questao22,
                           Dados.qvfi7$Questao23, Dados.qvfi7$Questao24, Dados.qvfi7$Questao25, Dados.qvfi7$Questao27, 
                           Dados.qvfi7$Q28, Dados.qvfi7$Questao33, Dados.qvfi7$Questao34)
set.seed(3000)
t23 <- t(apply(Competência.7, 2, basic.np))
colnames(Competência.7) <- c("Q15", "Q16", "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q27", "Q28", "Q33", "Q34")

# Tratamento

Tratamento.7 <- data.frame(Dados.qvfi7$Questao36, Dados.qvfi7$Questao37, Dados.qvfi7$Questao38, Dados.qvfi7$Questao39,
                          Dados.qvfi7$Questao40)
set.seed(3000)
t24 <- t(apply(Tratamento.7, 2, basic.np))
colnames(Tratamento.7) <- c("Q36", "Q37", "Q38", "Q39", "Q40")

# Geral

qvfi7.1 <- apply(Saude.geral.da.visao.7, 1, mean2); Dados.qvfi7$qvfi7.1 <- qvfi7.1
qvfi7.2 <- apply(Impacto.familiar.7, 1, mean2); Dados.qvfi7$qvfi7.2 <- qvfi7.2
qvfi7.3 <- apply(Personalidade.7, 1, mean2); Dados.qvfi7$qvfi7.3 <- qvfi7.3
qvfi7.4 <- apply(Competência.7, 1, mean2); Dados.qvfi7$qvfi7.4 <- qvfi7.4
qvfi7.5 <- apply(Tratamento.7, 1, mean2); Dados.qvfi7$qvfi7.5 <- qvfi7.5

Geral.7 <- data.frame(Dados.qvfi7$Q1, qvfi7.1, qvfi7.2, qvfi7.3, qvfi7.4, qvfi7.5)
qvfi7.6 <- apply(Geral.7, 1, mean2); Dados.qvfi7$qvfi7.6 <- qvfi7.6

qvfi7.graf <- cbind(Dados.qvfi7$Q1, 
                    Saude.geral.da.visao.7, 
                    Impacto.familiar.7, 
                    Personalidade.7, 
                    Competência.7, 
                    Tratamento.7)

set.seed(3000)
t4.new <- t(apply(qvfi7.graf, 2, basic.np))
rownames(t4.new)[1] <- c("Q1")

# Tabela 8 - Indicador QVFI-7

set.seed(3000)
rbind(basic.np(Dados.qvfi7$Q1),
      t20, basic.np(qvfi7.1),
      t21, basic.np(qvfi7.2),
      t22, basic.np(qvfi7.3),
      t23, basic.np(qvfi7.4),
      t24, basic.np(qvfi7.5),
      basic.np(qvfi7.6)
)

################################################### Parte 2 #############################################################

### Indicadores reduzidos

Dados.reduzido <- data.frame(Dados.dom$Q56, Dados.dom$Q69, Dados.dom$Q70, Dados.dom$Q72, Dados.dom$Q73, Dados.dom$Q76,
                             Dados.dom$Q80, Dados.dom$Q85, Dados.dom$Q91, Dados.dom$Q92, Dados.dom$Q93, Dados.dom$Q100,
                             Dados.dom$Q10, Dados.dom$Q12, Dados.dom$Q13, Dados.dom$Q14, Dados.dom$Q18, Dados.dom$Q23,
                             Dados.dom$Q24, Dados.dom$Q26, Dados.dom$Q38, Dados.dom$Q58, Dados.dom$Q63, Dados.dom$Q64,
                             Dados.dom$Q17, Dados.dom$Q19, Dados.dom$Q20, Dados.dom$Q25, Dados.dom$Q32, Dados.dom$Q34,
                             Dados.dom$Q36, Dados.dom$Q41, Dados.dom$Q42, Dados.dom$Q44, Dados.dom$Q47, Dados.dom$Q50)

names(Dados.reduzido) <- paste0(rep("Q", 36), 1:36)

# Indicador domínio da criança 

RE.new <- cbind(Dados.reduzido$Q13, Dados.reduzido$Q14, Dados.reduzido$Q15, Dados.reduzido$Q16)
colnames(RE.new) <- c("Q13", "Q14", "Q15", "Q16")
MO.new <- cbind(Dados.reduzido$Q17, Dados.reduzido$Q25, Dados.reduzido$Q26, Dados.reduzido$Q27)
colnames(MO.new) <- c("Q17", "Q25", "Q26", "Q27")
AC.new <- cbind(Dados.reduzido$Q18, Dados.reduzido$Q19, Dados.reduzido$Q20, Dados.reduzido$Q28)
colnames(AC.new) <- c("Q18", "Q19", "Q20", "Q28")
AD.new <- cbind(Dados.reduzido$Q21, Dados.reduzido$Q29, Dados.reduzido$Q30, Dados.reduzido$Q31, Dados.reduzido$Q32)
colnames(AD.new) <- c("Q21", "Q29", "Q30", "Q31", "Q32")
DE.new <- cbind(Dados.reduzido$Q33, Dados.reduzido$Q34, Dados.reduzido$Q35, Dados.reduzido$Q36)
colnames(DE.new) <- c("Q33", "Q34", "Q35", "Q36")
New.1 <- cbind(RE.new, MO.new, AC.new, AD.new, DE.new)

set.seed(3000)
t25 <- t(apply(RE.new, 2, basic.np))
set.seed(3000)
t26 <- t(apply(MO.new, 2, basic.np))
set.seed(3000)
t27 <- t(apply(AC.new, 2, basic.np))
set.seed(3000)
t28 <- t(apply(AD.new, 2, basic.np))
set.seed(3000)
t29 <- t(apply(DE.new, 2, basic.np))

set.seed(3000)
t1.new <- t(apply(New.1, 2, basic.np))

# Tabela 16 - Indicador reduzido do domínio da criança

set.seed(3000)
rbind(t25,
      t26, 
      t27, 
      t28, 
      t29)

# Indicador domínio dos pais

CO.new <- cbind(Dados.reduzido$Q1, Dados.reduzido$Q22)
colnames(CO.new) <- c("Q1", "Q22")
AT.new <- cbind(Dados.reduzido$Q23, Dados.reduzido$Q24)
colnames(AT.new) <- c("Q23", "Q24")
RO.new <- cbind(Dados.reduzido$Q2, Dados.reduzido$Q3, Dados.reduzido$Q4, Dados.reduzido$Q5)
colnames(RO.new) <- c("Q2", "Q3", "Q4", "Q5")
DP.new <- cbind(Dados.reduzido$Q6, Dados.reduzido$Q7)
colnames(DP.new) <- c("Q6", "Q7")
SP.new <- cbind(Dados.reduzido$Q8)
colnames(SP.new) <- c("Q8")
IS.new <- cbind(Dados.reduzido$Q9, Dados.reduzido$Q10, Dados.reduzido$Q11)
colnames(IS.new) <- c("Q9", "Q10", "Q11")
HE.new <- cbind(Dados.reduzido$Q12)
colnames(HE.new) <- c("Q12")
New.2 <- cbind(CO.new, AT.new, RO.new, DP.new, SP.new, IS.new, HE.new)

set.seed(3000)
t30 <- t(apply(CO.new, 2, basic.np))
set.seed(3000)
t31 <- t(apply(AT.new, 2, basic.np))
set.seed(3000)
t32 <- t(apply(RO.new, 2, basic.np))
set.seed(3000)
t33 <- t(apply(DP.new, 2, basic.np))
set.seed(3000)
t34 <- t(apply(SP.new, 2, basic.np))
set.seed(3000)
t35 <- t(apply(IS.new, 2, basic.np))
set.seed(3000)
t36 <- t(apply(HE.new, 2, basic.np))

set.seed(3000)
t2.new <- t(apply(New.2, 2, basic.np))

# Tabela 17 - Indicador reduzido do domínio dos pais

set.seed(3000)
rbind(t30,
      t31, 
      t32, 
      t33, 
      t34,
      t35,
      t36)

## Constructos de 1ª ordem 

set.seed(3000)
Reduzidos <- cbind(IND_RE, 
                   IND_MO, 
                   IND_AC,
                   IND_AD,
                   IND_DE,
                   IND_CRIANÇA,
                   IND_CO, 
                   IND_AT, 
                   IND_RO, 
                   IND_DP, 
                   SP.new, 
                   IND_IS, 
                   HE.new,
                   IND_PAIS)
tred.1 <- t(apply(Reduzidos, 2, basic.np))

# Tabela 24 - indicadores reduzidos
tred.1

### ===================
### Gráficos - barras
### ===================

################################################ Parte 1 ################################################

t1a = t1[,c(2,4,5)]
t1b = data.frame(Itens=factor(rownames(t1a), levels = rownames(t1a)), t1a)

plot1 = ggplot(t1b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,5) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala") +   
  ggtitle("Distrabilidade/Hiperatividade") +
  theme(plot.title = element_text(hjust = 0.5))


t2a = t2[,c(2,4,5)]
t2b = data.frame(Itens=factor(rownames(t2a), levels = rownames(t2a)), t2a)

plot2 = ggplot(t2b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,5) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala") +   
  ggtitle("Reforço parental") +
  theme(plot.title = element_text(hjust = 0.5))


t3a = t3[,c(2,4,5)]
t3b = data.frame(Itens=factor(rownames(t3a), levels = rownames(t3a)), t3a)

plot3 = ggplot(t3b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,5) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala") +   
  ggtitle("Humor") +
  theme(plot.title = element_text(hjust = 0.5))


t4a = t4[,c(2,4,5)]
t4b = data.frame(Itens=factor(rownames(t4a), levels = rownames(t4a)), t4a)

plot4 = ggplot(t4b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,5) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala") +   
  ggtitle("Aceitabilidade") +
  theme(plot.title = element_text(hjust = 0.5))


t5a = t5[,c(2,4,5)]
t5b = data.frame(Itens=factor(rownames(t5a), levels = rownames(t5a)), t5a)

plot5 = ggplot(t5b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,5) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala") +   
  ggtitle("Adaptabilidade") +
  theme(plot.title = element_text(hjust = 0.5))


t6a = t6[,c(2,4,5)]
t6b = data.frame(Itens=factor(rownames(t6a), levels = rownames(t6a)), t6a)

plot6 = ggplot(t6b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) +
  coord_flip() +
  ylim(0,5) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala") +   
  ggtitle("Exigência") +
  theme(plot.title = element_text(hjust = 0.5))


t7a = t7[,c(2,4,5)]
t7b = data.frame(Itens=factor(rownames(t7a), levels = rownames(t7a)), t7a)

plot7 = ggplot(t7b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,5) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala") +   
  ggtitle("Competência") +
  theme(plot.title = element_text(hjust = 0.5))


t8a = t8[,c(2,4,5)]
t8b = data.frame(Itens=factor(rownames(t8a), levels = rownames(t8a)), t8a)

plot8 = ggplot(t8b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,5) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala") +   
  ggtitle("Proximidade") +
  theme(plot.title = element_text(hjust = 0.5))


t9a = t9[,c(2,4,5)]
t9b = data.frame(Itens=factor(rownames(t9a), levels = rownames(t9a)), t9a)

plot9 = ggplot(t9b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,5) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala") +   
  ggtitle("Restrição de papel") +
  theme(plot.title = element_text(hjust = 0.5))


t10a = t10[,c(2,4,5)]
t10b = data.frame(Itens=factor(rownames(t10a), levels = rownames(t10a)), t10a)

plot10 = ggplot(t10b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,5) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala") +   
  ggtitle("Depressão") +
  theme(plot.title = element_text(hjust = 0.5))


t11a = t11[,c(2,4,5)]
t11b = data.frame(Itens=factor(rownames(t11a), levels = rownames(t11a)), t11a)

plot11 = ggplot(t11b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,5) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala") +   
  ggtitle("Relacionamento com esposo(a)/parceiro(a)") +
  theme(plot.title = element_text(hjust = 0.5))


t12a = t12[,c(2,4,5)]
t12b = data.frame(Itens=factor(rownames(t12a), levels = rownames(t12a)), t12a)

plot12 = ggplot(t12b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,5) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala") +   
  ggtitle("Isolamento") +
  theme(plot.title = element_text(hjust = 0.5))


t13a = t13[,c(2,4,5)]
t13b = data.frame(Itens=factor(rownames(t13a), levels = rownames(t13a)), t13a)

plot13 = ggplot(t13b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,5) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala") +   
  ggtitle("Saúde") +
  theme(plot.title = element_text(hjust = 0.5))

t14a = t14[,c(2,4,5)]
names(t14a) <- c("")
t14b = data.frame(Itens=factor(rownames(t14a), levels = rownames(t14a)), t14a)

plot14 = ggplot(t14b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,3) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala") +   
  ggtitle("Estresse da vida") +
  theme(plot.title = element_text(hjust = 0.5))

t15a = t15[,c(2,4,5)]
t15b = data.frame(Itens=factor(rownames(t15a), levels = rownames(t15a)), t15a)

plot15 = ggplot(t15b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,100) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala") +   
  ggtitle("Saúde geral da visão") +
  theme(plot.title = element_text(hjust = 0.5))

t16a = t16[,c(2,4,5)]
t16b = data.frame(Itens=factor(rownames(t16a), levels = rownames(t16a)), t16a)

plot16 = ggplot(t16b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,100) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala") +   
  ggtitle("Impacto familiar") +
  theme(plot.title = element_text(hjust = 0.5))

t17a = t17[,c(2,4,5)]
t17b = data.frame(Itens=factor(rownames(t17a), levels = rownames(t17a)), t17a)

plot17 = ggplot(t17b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,100) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala") +   
  ggtitle("Personalidade") +
  theme(plot.title = element_text(hjust = 0.5))

t18a = t18[,c(2,4,5)]
t18b = data.frame(Itens=factor(rownames(t18a), levels = rownames(t18a)), t18a)

plot18 = ggplot(t18b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,100) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala") +   
  ggtitle("Competência") +
  theme(plot.title = element_text(hjust = 0.5))

t19a = t19[,c(2,4,5)]
t19b = data.frame(Itens=factor(rownames(t19a), levels = rownames(t19a)), t19a)

plot19 = ggplot(t19b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,100) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala") +   
  ggtitle("Tratamento") +
  theme(plot.title = element_text(hjust = 0.5))

t20a = t20[,c(2,4,5)]
t20b = data.frame(Itens=factor(rownames(t20a), levels = rownames(t20a)), t20a)

plot20 = ggplot(t20b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,100) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala") +   
  ggtitle("Saúde geral da visão") +
  theme(plot.title = element_text(hjust = 0.5))

t21a = t21[,c(2,4,5)]
t21b = data.frame(Itens=factor(rownames(t21a), levels = rownames(t21a)), t21a)

plot21 = ggplot(t21b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,100) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala") +   
  ggtitle("Impacto familiar") +
  theme(plot.title = element_text(hjust = 0.5))

t22a = t22[,c(2,4,5)]
t22b = data.frame(Itens=factor(rownames(t22a), levels = rownames(t22a)), t22a)

plot22 = ggplot(t22b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,100) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala") +   
  ggtitle("Personalidade") +
  theme(plot.title = element_text(hjust = 0.5))

t23a = t23[,c(2,4,5)]
t23b = data.frame(Itens=factor(rownames(t23a), levels = rownames(t23a)), t23a)

plot23 = ggplot(t23b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,100) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala") +   
  ggtitle("Competência") +
  theme(plot.title = element_text(hjust = 0.5))

t24a = t24[,c(2,4,5)]
t24b = data.frame(Itens=factor(rownames(t24a), levels = rownames(t24a)), t24a)

plot24 = ggplot(t24b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,100) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala") +   
  ggtitle("Tratamento") +
  theme(plot.title = element_text(hjust = 0.5))

# Gráfico 1 - Domínio da criança completo

svg("Graf1.svg", height = 5, width = 10)
grid.arrange(plot1, plot2, ncol=2)
dev.off()

svg("Graf2.svg", height = 5, width = 10)
grid.arrange(plot3, plot4, ncol=2)
dev.off()

svg("Graf3.svg", height = 5, width = 10)
grid.arrange(plot5, plot6, ncol=2)
dev.off()

# Gráfico 3 - Domínio dos pais completo

svg("Graf4.svg", height = 5, width = 10)
grid.arrange(plot7, plot8, ncol=2)
dev.off()

svg("Graf5.svg", height = 5, width = 10)
grid.arrange(plot9, plot10, ncol=2)
dev.off()

svg("Graf6.svg", height = 5, width = 10)
grid.arrange(plot11, plot12, ncol=2)
dev.off()

svg("Graf7.svg", height = 5, width = 5)
plot13
dev.off()

# Gráfico 5 - Domínio do estresse da vida

svg("Graf8.svg", height = 5, width = 5)
plot14
dev.off()

# Não será mostrado - QVFI-3

svg("Graf9.svg", height = 5, width = 10)
grid.arrange(plot15, plot16, ncol=2)
dev.off()

svg("Graf10.svg", height = 5, width = 10)
grid.arrange(plot17, plot18, ncol=2)
dev.off()

svg("Graf11.svg", height = 5, width = 5)
plot19
dev.off()

# Não será mostrado - QVFI-7

svg("Graf12.svg", height = 5, width = 10)
grid.arrange(plot20, plot21, ncol=2)
dev.off()

svg("Graf13.svg", height = 5, width = 10)
grid.arrange(plot22, plot23, ncol=2)
dev.off()

svg("Graf14.svg", height = 5, width = 5)
plot24
dev.off()

c.1 <- rep(1,1)
c.2 <- rep(2,2)
c.3 <- rep(3,8)
c.4 <- rep(4,5)
c.5 <- rep(5,12)
c.6 <- rep(6,5)
Grupos.3 <- c(c.1, c.2, c.3, c.4, c.5, c.6)
t3.newa = t3.new[,c(2,4,5)]
t3.newb = data.frame(Itens=factor(rownames(t3.newa), levels = rownames(t3.newa)), t3.newa, Grupos.3)

plot3.new = ggplot(t3.newb, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens), fill = Grupos.3)) + 
  geom_bar(stat="identity", color="black", position=position_dodge()) + 
  coord_flip() +
  ylim(0,100) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala") +   
  ggtitle("QFVI-3") +
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_continuous(name = "Constructos", labels = c("Saúde geral", "Saúde geral da visão ", 
                                                         "Impacto familiar", "Personalidade", 
                                                         "Competência", "Tratamento"))

d.1 <- rep(1,1)
d.2 <- rep(2,2)
d.3 <- rep(3,7)
d.4 <- rep(4,9)
d.5 <- rep(5,15)
d.6 <- rep(6,5)
Grupos.4 <- c(d.1, d.2, d.3, d.4, d.5, d.6)
t4.newa = t4.new[,c(2,4,5)]
t4.newb = data.frame(Itens=factor(rownames(t4.newa), levels = rownames(t4.newa)), t4.newa, Grupos.4)

plot4.new = ggplot(t4.newb, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens), fill = Grupos.4)) + 
  geom_bar(stat="identity", color="black", position=position_dodge()) + 
  coord_flip() +
  ylim(0,100) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala") +   
  ggtitle("QFVI-7") +
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_continuous(name = "Constructos", labels = c("Saúde geral", "Saúde geral da visão ", 
                                                         "Impacto familiar", "Personalidade", 
                                                         "Competência", "Tratamento"))

# Gráfico 6 - QVFI-3

svg("Red3.svg", height = 5, width = 6)
plot3.new
dev.off()

# Gráfico 8 - QVFI-7

svg("Red4.svg", height = 5, width = 6)
plot4.new
dev.off()

################################################ Parte 2 ################################################

t25a = t25[,c(2,4,5)]
t25b = data.frame(Itens=factor(rownames(t25a), levels = rownames(t25a)), t25a)

plot25 = ggplot(t25b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,5) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala Likert") +   
  ggtitle("Reforço parental") +
  theme(plot.title = element_text(hjust = 0.5))

t26a = t26[,c(2,4,5)]
t26b = data.frame(Itens=factor(rownames(t26a), levels = rownames(t26a)), t26a)

plot26 = ggplot(t26b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,5) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala Likert") +   
  ggtitle("Humor") +
  theme(plot.title = element_text(hjust = 0.5))

t27a = t27[,c(2,4,5)]
t27b = data.frame(Itens=factor(rownames(t27a), levels = rownames(t27a)), t27a)

plot27 = ggplot(t27b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,5) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala Likert") +   
  ggtitle("Aceitabilidade") +
  theme(plot.title = element_text(hjust = 0.5))

t28a = t28[,c(2,4,5)]
t28b = data.frame(Itens=factor(rownames(t28a), levels = rownames(t28a)), t28a)

plot28 = ggplot(t28b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,5) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala Likert") +   
  ggtitle("Adaptabilidade") +
  theme(plot.title = element_text(hjust = 0.5))

t29a = t29[,c(2,4,5)]
t29b = data.frame(Itens=factor(rownames(t29a), levels = rownames(t29a)), t29a)

plot29 = ggplot(t29b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,5) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala Likert") +   
  ggtitle("Exigência") +
  theme(plot.title = element_text(hjust = 0.5))

t30a = t30[,c(2,4,5)]
t30b = data.frame(Itens=factor(rownames(t30a), levels = rownames(t30a)), t30a)

plot30 = ggplot(t30b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,5) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala Likert") +   
  ggtitle("Isolamento") +
  theme(plot.title = element_text(hjust = 0.5))

t31a = t31[,c(2,4,5)]
t31b = data.frame(Itens=factor(rownames(t31a), levels = rownames(t31a)), t31a)

plot31 = ggplot(t31b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,5) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala Likert") +   
  ggtitle("Proximidade") +
  theme(plot.title = element_text(hjust = 0.5))

t32a = t32[,c(2,4,5)]
t32b = data.frame(Itens=factor(rownames(t32a), levels = rownames(t32a)), t32a)

plot32 = ggplot(t32b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,5) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala Likert") +   
  ggtitle("Restrição de papel") +
  theme(plot.title = element_text(hjust = 0.5))

t33a = t33[,c(2,4,5)]
t33b = data.frame(Itens=factor(rownames(t33a), levels = rownames(t33a)), t33a)

plot33 = ggplot(t33b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,5) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala Likert") +   
  ggtitle("Depressão") +
  theme(plot.title = element_text(hjust = 0.5))

t35a = t35[,c(2,4,5)]
t35b = data.frame(Itens=factor(rownames(t35a), levels = rownames(t35a)), t35a)

plot35 = ggplot(t35b, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens))) + 
  geom_bar(stat="identity", color="black", fill="#3462A8", position=position_dodge()) + 
  coord_flip() +
  ylim(0,5) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala Likert") +   
  ggtitle("Isolamento") +
  theme(plot.title = element_text(hjust = 0.5))

# Não será mostrado - Domínio da criança reduzido

svg("Graf15.svg", height = 5, width = 10)
grid.arrange(plot25, plot26, ncol=2)
dev.off()

svg("Graf16.svg", height = 5, width = 10)
grid.arrange(plot27, plot28, ncol=2)
dev.off()

svg("Graf17.svg", height = 5, width = 5)
plot29
dev.off()

# Não será mostrado - Domínio dos pais reduzido

svg("Graf18.svg", height = 5, width = 10)
grid.arrange(plot30, plot31, ncol=2)
dev.off()

svg("Graf19.svg", height = 5, width = 10)
grid.arrange(plot32, plot33, ncol=2)
dev.off()

svg("Graf20.svg", height = 5, width = 5)
plot30
dev.off()

a.1 <- rep(1,4)
a.2 <- rep(2,4)
a.3 <- rep(3,4)
a.4 <- rep(4,5)
a.5 <- rep(5,4)
Grupos.1 <- c(a.1, a.2, a.3, a.4, a.5)
t1.newa = t1.new[,c(2,4,5)]
t1.newb = data.frame(Itens=factor(rownames(t1.newa), levels = rownames(t1.newa)), t1.newa, Grupos.1)

plot1.new = ggplot(t1.newb, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens), fill = Grupos.1)) + 
  geom_bar(stat="identity", color="black", position=position_dodge()) + 
  coord_flip() +
  ylim(0,5) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala") +   
  ggtitle("Domínio da criança reduzido") +
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_continuous(name = "Constructos", labels = c("Reforço parental", "Humor", "Aceitabilidade",
                                                         "Adaptabilidade", "Exigência"))

b.1 <- rep(1,2)
b.2 <- rep(2,2)
b.3 <- rep(3,4)
b.4 <- rep(4,2)
b.5 <- rep(5,1)
b.6 <- rep(6,3)
b.7 <- rep(7,1)
Grupos.2 <- c(b.1, b.2, b.3, b.4, b.5, b.6, b.7)
t2.newa = t2.new[,c(2,4,5)]
t2.newb = data.frame(Itens=factor(rownames(t2.newa), levels = rownames(t2.newa)), t2.newa, Grupos.2)

plot2.new = ggplot(t2.newb, aes(x=Itens, y=Média, scale_x_discrete(limits = Itens), fill = Grupos.2)) + 
  geom_bar(stat="identity", color="black", position=position_dodge()) + 
  coord_flip() +
  ylim(0,5) +
  geom_errorbar(aes(ymin=L.I, ymax=L.S), width=.2, position=position_dodge(.9)) +
  labs(title="", x="Itens", y = "Escala") +   
  ggtitle("Domínio dos pais reduzido") +
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_continuous(name = "Constructos", labels = c("Competência", "Proximidade", "Restrição de papel",
                                                         "Depressão", "Relacionamento com esposo(a)/parceiro(a)",
                                                         "Isolamento", "Saúde"))

# Gráfico 18 - Domínio da criança reduzido

svg("Red1.svg", height = 5, width = 6)
plot1.new
dev.off()

# Gráfico 19 - Domínio dos pais reduzido

svg("Red2.svg", height = 5, width = 7)
plot2.new
dev.off()

### ==================
### Gráficos - radar
### ==================

############################################# Parte 1 #################################################

# Gráfico 2 - Domínio da criança completo

Dom.crianca.c <- cbind(Dados.dom$Percentil.DI, 
                       Dados.dom$Percentil.RE, 
                       Dados.dom$Percentil.MO, 
                       Dados.dom$Percentil.AC,
                       Dados.dom$Percentil.AD,
                       Dados.dom$Percentil.DE)

svg("Radar1.svg", height =7, width=7)
Data.1 <- c(apply(Dom.crianca.c, 2, mean))
Data.1 <- data.frame(rbind(rep(100,6) , rep(0,6) , Data.1))
colnames(Data.1) <- c("Distrabilidade/\nHiperatividade", "Reforço\nparental", "Humor",
                      "Aceitabilidade", "Adaptabilidade", "Exigência")

colors_border=c(rgb(0.243, 0.439, 0.678, 0.8))

colors_in=c(rgb(0.243, 0.439, 0.678, 0.2))

radarchart(Data.1, axistype=1 , title = "Indicador completo do domínio da criança",
           pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,25), cglwd=0.8,
           vlcex=0.95, seg = 4)
dev.off()

# Gráfico 4 - Domínio dos pais completo

Dom.pais.c <- cbind(Dados.dom$Percentil.CO, 
                    Dados.dom$Percentil.AT, 
                    Dados.dom$Percentil.RO, 
                    Dados.dom$Percentil.DP,
                    Dados.dom$Percentil.SP,
                    Dados.dom$Percentil.IS,
                    Dados.dom$Percentil.HE)

svg("Radar2.svg", height =7, width=7)
Data.2 <- c(apply(Dom.pais.c, 2, mean))
Data.2 <- data.frame(rbind(rep(100,7) , rep(0,7) , Data.2))
colnames(Data.2) <- c("Competência", "Proximidade", "Restrição\nde papel", "Depressão", 
                      "Relacionamento com\nesposo(a)/parceiro(a)", "Isolamento", "Saúde")

colors_border=c(rgb(0.243, 0.439, 0.678, 0.8))

colors_in=c(rgb(0.243, 0.439, 0.678, 0.2))

radarchart(Data.2, axistype=1 , title = "Indicador completo do domínio dos pais",
           pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,25), cglwd=0.8,
           vlcex=0.95, seg = 4)
dev.off()

# Gráfico 7 - QVFI-3

QVFI.3 <- cbind(Dados.qvfi3$Q1, 
                qvfi3.1, 
                qvfi3.2, 
                qvfi3.3,
                qvfi3.4,
                qvfi3.5)

svg("Radar3.svg", height =7, width=7)
Data.3 <- c(apply(QVFI.3, 2, mean))
Data.3 <- data.frame(rbind(rep(100,6) , rep(0,6) , Data.3))
colnames(Data.3) <- c("Saúde geral", "Saúde geral\nda visão ", "Impacto familiar", 
                      "Personalidade", "Competência", "Tratamento")

colors_border=c(rgb(0.243, 0.439, 0.678, 0.8))

colors_in=c(rgb(0.243, 0.439, 0.678, 0.2))

radarchart(Data.3, axistype=1 , title = "Indicador QFVI-3",
           pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,25), cglwd=0.8,
           vlcex=0.95, seg = 4)
dev.off()

# Gráfico 9 - QVFI-7

QVFI.7 <- cbind(Dados.qvfi7$Q1, 
                qvfi7.1, 
                qvfi7.2, 
                qvfi7.3,
                qvfi7.4,
                qvfi7.5)

svg("Radar4.svg", height =7, width=7)
Data.4 <- c(apply(QVFI.7, 2, mean))
Data.4 <- data.frame(rbind(rep(100,6) , rep(0,6) , Data.4))
colnames(Data.4) <- c("Saúde geral", "Saúde geral\nda visão", "Impacto familiar", 
                      "Personalidade", "Competência", "Tratamento")

colors_border=c(rgb(0.243, 0.439, 0.678, 0.8))

colors_in=c(rgb(0.243, 0.439, 0.678, 0.2))

radarchart(Data.4, axistype=1 , title = "Indicador QFVI-7",
           pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,25), cglwd=0.8,
           vlcex=0.95, seg = 4)
dev.off()

# Gráfico 10 - Geral

ind.gerais <- cbind(Dados.dom$Percentil.CD,
                    Dados.dom$Percentil.PD,
                    Dados.dom$Soma.LS,
                    qvfi3.6,
                    qvfi7.6)

svg("Radar5.svg", height =7, width=7)
Data.5 <- c(apply(ind.gerais, 2, mean))
Data.5 <- data.frame(rbind(rep(100,5) , rep(0,5) , Data.5))
colnames(Data.5) <- c("Domínio\nda criança", "Domínio\ndos pais", "Domínio do\nestresse da vida", 
            
                                "QFVI-3", "QFVI-7")

colors_border=c(rgb(0.243, 0.439, 0.678, 0.8))

colors_in=c(rgb(0.243, 0.439, 0.678, 0.2))

radarchart(Data.5, axistype=1 , title = "Indicadores gerais",
           pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,25), cglwd=0.8,
           vlcex=0.95, seg = 4)
dev.off()

################################################ Parte 2 #################################################

# Gráfico 20 - Domínio da criança reduzido

Dom.crianca.r <- cbind(IND_RE, 
                       IND_MO, 
                       IND_AC,
                       IND_AD,
                       IND_DE)

svg("Radar6.svg", height =7, width=7)
Data.6 <- c(apply(Dom.crianca.r, 2, mean))
Data.6 <- data.frame(rbind(rep(5,5) , rep(1,5) , Data.6))
colnames(Data.6) <- c("Reforço\nparental", "Humor","Aceitabilidade", "Adaptabilidade", "Exigência")

colors_border=c(rgb(0.243, 0.439, 0.678, 0.8))

colors_in=c(rgb(0.243, 0.439, 0.678, 0.2))

radarchart(Data.6, axistype=1 , title = "Indicador reduzido do domínio da criança",
           pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(1,5,1), cglwd=0.8,
           vlcex=0.95, seg = 4)
dev.off()

# Gráfico 21 - Domínio dos pais reduzido

Dom.pais.r <- cbind(IND_CO, 
                    IND_AT, 
                    IND_RO, 
                    IND_DP, 
                    SP.new, 
                    IND_IS, 
                    HE.new)

svg("Radar7.svg", height =7, width=7)
Data.7 <- c(apply(Dom.pais.r, 2, mean))
Data.7 <- data.frame(rbind(rep(5,7) , rep(1,7) , Data.7))
colnames(Data.7) <- c("Competência", "Proximidade", "Restrição\nde papel", "Depressão", 
                      "Relacionamento com\nesposo(a)/parceiro(a)", "Isolamento", "Saúde")

colors_border=c(rgb(0.243, 0.439, 0.678, 0.8))

colors_in=c(rgb(0.243, 0.439, 0.678, 0.2))

radarchart(Data.7, axistype=1 , title = "Indicador reduzido do domínio dos pais",
           pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(1,5,1), cglwd=0.8,
           vlcex=0.95, seg = 4)
dev.off()

### ======================================================================
### Análise fatorial - Indicador reduzido do domínio da criança (Parte 2)
### ======================================================================

# Removeu 2 questões do constructo AD devido ao AVE

AD.new.2 <- cbind(Dados.reduzido$Q21, Dados.reduzido$Q29, Dados.reduzido$Q30, Dados.reduzido$Q32)
AD.new.3 <- cbind(Dados.reduzido$Q21, Dados.reduzido$Q30, Dados.reduzido$Q32)

# Tabela 18 - Análise fatorial do domínio da criança

rbind(fa1(RE.new),
      fa1(MO.new),
      fa1(AC.new),
      fa1(AD.new),
      fa1(DE.new))

rbind(fa1(RE.new),
      fa1(MO.new),
      fa1(AC.new),
      fa1(AD.new.3),
      fa1(DE.new))

# Tabela 19 - Validação do domínio da criança

rbind(Val(RE.new),
      Val(MO.new),
      Val(AC.new),
      Val(AD.new.3),
      Val(DE.new))

IND_RE = RE.new%*% as.numeric(principal(RE.new,1,rotate="promax")$weights)/(sum(as.numeric(principal(RE.new,1,rotate="promax")$weights)))
IND_MO = MO.new%*% as.numeric(principal(MO.new,1,rotate="promax")$weights)/(sum(as.numeric(principal(MO.new,1,rotate="promax")$weights)))
IND_AC = AC.new%*% as.numeric(principal(AC.new,1,rotate="promax")$weights)/(sum(as.numeric(principal(AC.new,1,rotate="promax")$weights)))
IND_AD = AD.new.3%*% as.numeric(principal(AD.new.3,1,rotate="promax")$weights)/(sum(as.numeric(principal(AD.new.3,1,rotate="promax")$weights)))
IND_DE = DE.new%*% as.numeric(principal(DE.new,1,rotate="promax")$weights)/(sum(as.numeric(principal(DE.new,1,rotate="promax")$weights)))

CRIANÇA = cbind(IND_RE, IND_MO, IND_AC, IND_AD, IND_DE)
colnames(CRIANÇA) = c("RE", "MO", "AC", "AD", "DE")

### =================================================================
### Análise fatorial - Indicador reduzido domínio dos pais (Parte 2)
### =================================================================

# Tabela 20 - Análise fatorial do domínio dos pais

rbind(fa1(CO.new),
      fa1(AT.new),
      fa1(RO.new),
      fa1(DP.new),
      fa1(IS.new))

# Tabela 21 - Validação do domínio dos pais

rbind(Val1(CO.new),
      Val1(AT.new),
      Val(RO.new),
      Val1(DP.new),
      Val(IS.new))

IND_CO = CO.new%*% as.numeric(principal(CO.new,1,rotate="promax")$weights)/(sum(as.numeric(principal(CO.new,1,rotate="promax")$weights)))
IND_AT = AT.new%*% as.numeric(principal(AT.new,1,rotate="promax")$weights)/(sum(as.numeric(principal(AT.new,1,rotate="promax")$weights)))
IND_RO = RO.new%*% as.numeric(principal(RO.new,1,rotate="promax")$weights)/(sum(as.numeric(principal(RO.new,1,rotate="promax")$weights)))
IND_DP = DP.new%*% as.numeric(principal(DP.new,1,rotate="promax")$weights)/(sum(as.numeric(principal(DP.new,1,rotate="promax")$weights)))
IND_IS = IS.new%*% as.numeric(principal(IS.new,1,rotate="promax")$weights)/(sum(as.numeric(principal(IS.new,1,rotate="promax")$weights)))

PAIS = cbind(IND_CO, IND_AT, IND_RO, IND_DP, SP.new, IND_IS, HE.new)
colnames(PAIS) = c("CO", "AT", "RO", "DP", "SP", "IS", "HE")

### ===============================================================
### Análise fatorial - Constructos reduzidos de 2ª ordem (Parte 2)
### ===============================================================

# Tabela 22 - Análise fatorial dos constructos de 2ª ordem

rbind(fa1(CRIANÇA),
      fa1(PAIS))

# Tabela 23 - Validação dos constructos de 2ª ordem

rbind(Val(CRIANÇA),
      Val(PAIS))

IND_CRIANÇA = CRIANÇA%*% as.numeric(principal(CRIANÇA,1,rotate="promax")$weights)/(sum(as.numeric(principal(CRIANÇA,1,rotate="promax")$weights)))
IND_PAIS = PAIS%*% as.numeric(principal(PAIS,1,rotate="promax")$weights)/(sum(as.numeric(principal(PAIS,1,rotate="promax")$weights)))

### =============================
###  Comparações das variáveis
### =============================

######################################### Parte 1 #########################################

# Tabela 10 - Domínio da criança completo

rbind(whitney.abg(Dados.dom$Percentil.CD, Dados.socio$GENERO.FILHO.A.),
      whitney.abg(Dados.dom$Percentil.CD, Dados.socio$FILHO.ESTUDA.),
      whitney.abg(Dados.dom$Percentil.CD, Dados.socio$ESCOLA.ESPECIAL.),
      whitney.abg(Dados.dom$Percentil.CD, Dados.socio$acessibilidade.recursos),
      whitney.abg(Dados.dom$Percentil.CD, Dados.socio$assistente.recursos),
      whitney.abg(Dados.dom$Percentil.CD, Dados.socio$computador.recursos),
      whitney.abg(Dados.dom$Percentil.CD, Dados.socio$livrosembraile.recursos),
      whitney.abg(Dados.dom$Percentil.CD, Dados.socio$nenhum.recursos),
      whitney.abg(Dados.dom$Percentil.CD, Dados.socio$ATIVIDADE.FÍSICA.),
      whitney.abg(Dados.dom$Percentil.CD, Dados.socio$ATIVIDADE.DE.LAZER),
      whitney.abg(Dados.dom$Percentil.CD, Dados.socio$TRABALHA.FORA.),
      kruskal.abg(Dados.dom$Percentil.CD, Dados.socio$ESTADO.CIVIL)$`tabela`,
      whitney.abg(Dados.dom$Percentil.CD, Dados.socio$VIVE.COM.PAI.),
      whitney.abg(Dados.dom$Percentil.CD, Dados.socio$TRABALHO.INFORMAL.),
      whitney.abg(Dados.dom$Percentil.CD, Dados.socio$BENEFICIO.SOCIAL.),
      whitney.abg(Dados.dom$Percentil.CD, Dados.socio$PLANO.SAÚDE),
      whitney.abg(Dados.dom$Percentil.CD, Dados.socio$GANHOU.PESO.),
      whitney.abg(Dados.dom$Percentil.CD, Dados.socio$DOR.DE.CABEÇA),
      whitney.abg(Dados.dom$Percentil.CD, Dados.socio$TONTURA),
      whitney.abg(Dados.dom$Percentil.CD, Dados.socio$NÁUSEA),
      whitney.abg(Dados.dom$Percentil.CD, Dados.socio$INTOLERÂNCIA.À.LUZ),
      whitney.abg(Dados.dom$Percentil.CD, Dados.socio$TRISTEZA),
      whitney.abg(Dados.dom$Percentil.CD, Dados.socio$ESQUECIMENTO),
      whitney.abg(Dados.dom$Percentil.CD, Dados.socio$CANSAÇO),
      whitney.abg(Dados.dom$Percentil.CD, Dados.socio$DORES.NO.CORPO),
      whitney.abg(Dados.dom$Percentil.CD, Dados.socio$INSÔNIA),
      whitney.abg(Dados.dom$Percentil.CD, Dados.socio$DIFICULDADE.PEGAR.NO.SONO),
      whitney.abg(Dados.dom$Percentil.CD, Dados.socio$ACORDA.NA.MADRUGADA),
      whitney.abg(Dados.dom$Percentil.CD, Dados.socio$QUEDA.CABELO),
      whitney.abg(Dados.dom$Percentil.CD, Dados.socio$HIPERTENSÃO),
      whitney.abg(Dados.dom$Percentil.CD, Dados.socio$AUMENTO.GLICEMIA))

# Tabela 11 - Domínio dos pais completo

rbind(whitney.abg(Dados.dom$Percentil.PD, Dados.socio$GENERO.FILHO.A.),
      whitney.abg(Dados.dom$Percentil.PD, Dados.socio$FILHO.ESTUDA.),
      whitney.abg(Dados.dom$Percentil.PD, Dados.socio$ESCOLA.ESPECIAL.),
      whitney.abg(Dados.dom$Percentil.PD, Dados.socio$acessibilidade.recursos),
      whitney.abg(Dados.dom$Percentil.PD, Dados.socio$assistente.recursos),
      whitney.abg(Dados.dom$Percentil.PD, Dados.socio$computador.recursos),
      whitney.abg(Dados.dom$Percentil.PD, Dados.socio$livrosembraile.recursos),
      whitney.abg(Dados.dom$Percentil.PD, Dados.socio$nenhum.recursos),
      whitney.abg(Dados.dom$Percentil.PD, Dados.socio$ATIVIDADE.FÍSICA.),
      whitney.abg(Dados.dom$Percentil.PD, Dados.socio$ATIVIDADE.DE.LAZER),
      whitney.abg(Dados.dom$Percentil.PD, Dados.socio$TRABALHA.FORA.),
      kruskal.abg(Dados.dom$Percentil.PD, Dados.socio$ESTADO.CIVIL)$`tabela`,
      whitney.abg(Dados.dom$Percentil.PD, Dados.socio$VIVE.COM.PAI.),
      whitney.abg(Dados.dom$Percentil.PD, Dados.socio$TRABALHO.INFORMAL.),
      whitney.abg(Dados.dom$Percentil.PD, Dados.socio$BENEFICIO.SOCIAL.),
      whitney.abg(Dados.dom$Percentil.PD, Dados.socio$PLANO.SAÚDE),
      whitney.abg(Dados.dom$Percentil.PD, Dados.socio$GANHOU.PESO.),
      whitney.abg(Dados.dom$Percentil.PD, Dados.socio$DOR.DE.CABEÇA),
      whitney.abg(Dados.dom$Percentil.PD, Dados.socio$TONTURA),
      whitney.abg(Dados.dom$Percentil.PD, Dados.socio$NÁUSEA),
      whitney.abg(Dados.dom$Percentil.PD, Dados.socio$INTOLERÂNCIA.À.LUZ),
      whitney.abg(Dados.dom$Percentil.PD, Dados.socio$TRISTEZA),
      whitney.abg(Dados.dom$Percentil.PD, Dados.socio$ESQUECIMENTO),
      whitney.abg(Dados.dom$Percentil.PD, Dados.socio$CANSAÇO),
      whitney.abg(Dados.dom$Percentil.PD, Dados.socio$DORES.NO.CORPO),
      whitney.abg(Dados.dom$Percentil.PD, Dados.socio$INSÔNIA),
      whitney.abg(Dados.dom$Percentil.PD, Dados.socio$DIFICULDADE.PEGAR.NO.SONO),
      whitney.abg(Dados.dom$Percentil.PD, Dados.socio$ACORDA.NA.MADRUGADA),
      whitney.abg(Dados.dom$Percentil.PD, Dados.socio$QUEDA.CABELO),
      whitney.abg(Dados.dom$Percentil.PD, Dados.socio$HIPERTENSÃO),
      whitney.abg(Dados.dom$Percentil.PD, Dados.socio$AUMENTO.GLICEMIA))

# Tabela 12 - Domínio do estresse da vida

rbind(whitney.abg(Dados.dom$Soma.LS, Dados.socio$GENERO.FILHO.A.),
      whitney.abg(Dados.dom$Soma.LS, Dados.socio$FILHO.ESTUDA.),
      whitney.abg(Dados.dom$Soma.LS, Dados.socio$ESCOLA.ESPECIAL.),
      whitney.abg(Dados.dom$Soma.LS, Dados.socio$acessibilidade.recursos),
      whitney.abg(Dados.dom$Soma.LS, Dados.socio$assistente.recursos),
      whitney.abg(Dados.dom$Soma.LS, Dados.socio$computador.recursos),
      whitney.abg(Dados.dom$Soma.LS, Dados.socio$livrosembraile.recursos),
      whitney.abg(Dados.dom$Soma.LS, Dados.socio$nenhum.recursos),
      whitney.abg(Dados.dom$Soma.LS, Dados.socio$ATIVIDADE.FÍSICA.),
      whitney.abg(Dados.dom$Soma.LS, Dados.socio$ATIVIDADE.DE.LAZER),
      whitney.abg(Dados.dom$Soma.LS, Dados.socio$TRABALHA.FORA.),
      kruskal.abg(Dados.dom$Soma.LS, Dados.socio$ESTADO.CIVIL)$`tabela`,
      whitney.abg(Dados.dom$Soma.LS, Dados.socio$VIVE.COM.PAI.),
      whitney.abg(Dados.dom$Soma.LS, Dados.socio$TRABALHO.INFORMAL.),
      whitney.abg(Dados.dom$Soma.LS, Dados.socio$BENEFICIO.SOCIAL.),
      whitney.abg(Dados.dom$Soma.LS, Dados.socio$PLANO.SAÚDE),
      whitney.abg(Dados.dom$Soma.LS, Dados.socio$GANHOU.PESO.),
      whitney.abg(Dados.dom$Soma.LS, Dados.socio$DOR.DE.CABEÇA),
      whitney.abg(Dados.dom$Soma.LS, Dados.socio$TONTURA),
      whitney.abg(Dados.dom$Soma.LS, Dados.socio$NÁUSEA),
      whitney.abg(Dados.dom$Soma.LS, Dados.socio$INTOLERÂNCIA.À.LUZ),
      whitney.abg(Dados.dom$Soma.LS, Dados.socio$TRISTEZA),
      whitney.abg(Dados.dom$Soma.LS, Dados.socio$ESQUECIMENTO),
      whitney.abg(Dados.dom$Soma.LS, Dados.socio$CANSAÇO),
      whitney.abg(Dados.dom$Soma.LS, Dados.socio$DORES.NO.CORPO),
      whitney.abg(Dados.dom$Soma.LS, Dados.socio$INSÔNIA),
      whitney.abg(Dados.dom$Soma.LS, Dados.socio$DIFICULDADE.PEGAR.NO.SONO),
      whitney.abg(Dados.dom$Soma.LS, Dados.socio$ACORDA.NA.MADRUGADA),
      whitney.abg(Dados.dom$Soma.LS, Dados.socio$QUEDA.CABELO),
      whitney.abg(Dados.dom$Soma.LS, Dados.socio$HIPERTENSÃO),
      whitney.abg(Dados.dom$Soma.LS, Dados.socio$AUMENTO.GLICEMIA))

# Tabela 13 - Percentil estresse da vida - 08/08/2019

rbind(whitney.abg(Dados.dom$Percentil.estresse.vida, Dados.socio$GENERO.FILHO.A.),
      whitney.abg(Dados.dom$Percentil.estresse.vida, Dados.socio$FILHO.ESTUDA.),
      whitney.abg(Dados.dom$Percentil.estresse.vida, Dados.socio$ESCOLA.ESPECIAL.),
      whitney.abg(Dados.dom$Percentil.estresse.vida, Dados.socio$acessibilidade.recursos),
      whitney.abg(Dados.dom$Percentil.estresse.vida, Dados.socio$assistente.recursos),
      whitney.abg(Dados.dom$Percentil.estresse.vida, Dados.socio$computador.recursos),
      whitney.abg(Dados.dom$Percentil.estresse.vida, Dados.socio$livrosembraile.recursos),
      whitney.abg(Dados.dom$Percentil.estresse.vida, Dados.socio$nenhum.recursos),
      whitney.abg(Dados.dom$Percentil.estresse.vida, Dados.socio$ATIVIDADE.FÍSICA.),
      whitney.abg(Dados.dom$Percentil.estresse.vida, Dados.socio$ATIVIDADE.DE.LAZER),
      whitney.abg(Dados.dom$Percentil.estresse.vida, Dados.socio$TRABALHA.FORA.),
      kruskal.abg(Dados.dom$Percentil.estresse.vida, Dados.socio$ESTADO.CIVIL)$`tabela`,
      whitney.abg(Dados.dom$Percentil.estresse.vida, Dados.socio$VIVE.COM.PAI.),
      whitney.abg(Dados.dom$Percentil.estresse.vida, Dados.socio$TRABALHO.INFORMAL.),
      whitney.abg(Dados.dom$Percentil.estresse.vida, Dados.socio$BENEFICIO.SOCIAL.),
      whitney.abg(Dados.dom$Percentil.estresse.vida, Dados.socio$PLANO.SAÚDE),
      whitney.abg(Dados.dom$Percentil.estresse.vida, Dados.socio$GANHOU.PESO.),
      whitney.abg(Dados.dom$Percentil.estresse.vida, Dados.socio$DOR.DE.CABEÇA),
      whitney.abg(Dados.dom$Percentil.estresse.vida, Dados.socio$TONTURA),
      whitney.abg(Dados.dom$Percentil.estresse.vida, Dados.socio$NÁUSEA),
      whitney.abg(Dados.dom$Percentil.estresse.vida, Dados.socio$INTOLERÂNCIA.À.LUZ),
      whitney.abg(Dados.dom$Percentil.estresse.vida, Dados.socio$TRISTEZA),
      whitney.abg(Dados.dom$Percentil.estresse.vida, Dados.socio$ESQUECIMENTO),
      whitney.abg(Dados.dom$Percentil.estresse.vida, Dados.socio$CANSAÇO),
      whitney.abg(Dados.dom$Percentil.estresse.vida, Dados.socio$DORES.NO.CORPO),
      whitney.abg(Dados.dom$Percentil.estresse.vida, Dados.socio$INSÔNIA),
      whitney.abg(Dados.dom$Percentil.estresse.vida, Dados.socio$DIFICULDADE.PEGAR.NO.SONO),
      whitney.abg(Dados.dom$Percentil.estresse.vida, Dados.socio$ACORDA.NA.MADRUGADA),
      whitney.abg(Dados.dom$Percentil.estresse.vida, Dados.socio$QUEDA.CABELO),
      whitney.abg(Dados.dom$Percentil.estresse.vida, Dados.socio$HIPERTENSÃO),
      whitney.abg(Dados.dom$Percentil.estresse.vida, Dados.socio$AUMENTO.GLICEMIA))

###======================
### Gráficos - 08/08/2019
###======================

svg("Graf53_L.svg", height = 6, width = 12)
par(mfrow=c(1,2))
boxplot(Dados.dom$Percentil.estresse.vida ~ Dados.socio$FILHO.ESTUDA., col = "skyblue4", xlab = "Filho estuda", 
        ylab = "Percentil estresse da vida", ylim=c(-5,100),
        main = "Percentil estresse da vida\npor filho estuda", 
        names = c("Não","Sim"))
text(1.5, -2, "Valor-p = 0,039", cex=1.0)

boxplot(Dados.dom$Percentil.estresse.vida ~ Dados.socio$NÁUSEA, col = "skyblue4", xlab = "Náusea", 
        ylab = "Percentil estresse da vida", ylim=c(-5,100),
        main = "Percentil estresse da vida\npor náusea", 
        names = c("Não","Sim"))
text(1.5, -2, "Valor-p = 0,013", cex=1.0)
dev.off()

svg("Graf54_L.svg", height = 6, width = 12)
par(mfrow=c(1,2))
boxplot(Dados.dom$Percentil.estresse.vida ~ Dados.socio$HIPERTENSÃO, col = "skyblue4", xlab = "Hipertensão", 
        ylab = "Percentil estresse da vida", ylim=c(-5,100),
        main = "Percentil estresse da vida\npor hipertensão", 
        names = c("Não","Sim"))
text(1.5, -2, "Valor-p = 0,036", cex=1.0)

dev.off()

###################################

Dados.3.new <- full_join(Dados.qvfi3, Dados.socio, by = "COD")

# Tabela 13 - QVFI-3

rbind(whitney.abg(Dados.3.new$qvfi3.6, Dados.3.new$GENERO.FILHO.A.),
      whitney.abg(Dados.3.new$qvfi3.6, Dados.3.new$FILHO.ESTUDA.),
      #whitney.abg(Dados.3.new$qvfi3.6, Dados.3.new$ESCOLA.ESPECIAL.),
      #whitney.abg(Dados.3.new$qvfi3.6, Dados.3.new$acessibilidade.recursos),
      #whitney.abg(Dados.3.new$qvfi3.6, Dados.3.new$assistente.recursos),
      #whitney.abg(Dados.3.new$qvfi3.6, Dados.3.new$computador.recursos),
      #whitney.abg(Dados.3.new$qvfi3.6, Dados.3.new$livrosembraile.recursos),
      #whitney.abg(Dados.3.new$qvfi3.6, Dados.3.new$nenhum.recursos),
      whitney.abg(Dados.3.new$qvfi3.6, Dados.3.new$ATIVIDADE.FÍSICA.),
      whitney.abg(Dados.3.new$qvfi3.6, Dados.3.new$ATIVIDADE.DE.LAZER),
      whitney.abg(Dados.3.new$qvfi3.6, Dados.3.new$TRABALHA.FORA.),
      kruskal.abg(Dados.3.new$qvfi3.6, Dados.3.new$ESTADO.CIVIL)$`tabela`,
      whitney.abg(Dados.3.new$qvfi3.6, Dados.3.new$VIVE.COM.PAI.),
      whitney.abg(Dados.3.new$qvfi3.6, Dados.3.new$TRABALHO.INFORMAL.),
      whitney.abg(Dados.3.new$qvfi3.6, Dados.3.new$BENEFICIO.SOCIAL.),
      whitney.abg(Dados.3.new$qvfi3.6, Dados.3.new$PLANO.SAÚDE),
      whitney.abg(Dados.3.new$qvfi3.6, Dados.3.new$GANHOU.PESO.),
      whitney.abg(Dados.3.new$qvfi3.6, Dados.3.new$DOR.DE.CABEÇA),
      whitney.abg(Dados.3.new$qvfi3.6, Dados.3.new$TONTURA),
      whitney.abg(Dados.3.new$qvfi3.6, Dados.3.new$NÁUSEA),
      whitney.abg(Dados.3.new$qvfi3.6, Dados.3.new$INTOLERÂNCIA.À.LUZ),
      whitney.abg(Dados.3.new$qvfi3.6, Dados.3.new$TRISTEZA),
      whitney.abg(Dados.3.new$qvfi3.6, Dados.3.new$ESQUECIMENTO),
      whitney.abg(Dados.3.new$qvfi3.6, Dados.3.new$CANSAÇO),
      whitney.abg(Dados.3.new$qvfi3.6, Dados.3.new$DORES.NO.CORPO),
      whitney.abg(Dados.3.new$qvfi3.6, Dados.3.new$INSÔNIA),
      whitney.abg(Dados.3.new$qvfi3.6, Dados.3.new$DIFICULDADE.PEGAR.NO.SONO),
      whitney.abg(Dados.3.new$qvfi3.6, Dados.3.new$ACORDA.NA.MADRUGADA),
      whitney.abg(Dados.3.new$qvfi3.6, Dados.3.new$QUEDA.CABELO),
      whitney.abg(Dados.3.new$qvfi3.6, Dados.3.new$HIPERTENSÃO),
      whitney.abg(Dados.3.new$qvfi3.6, Dados.3.new$AUMENTO.GLICEMIA))

Dados.7.new <- full_join(Dados.qvfi7, Dados.socio, by = "COD")

# Tabela 14 - QVFI-7

rbind(whitney.abg(Dados.7.new$qvfi7.6, Dados.7.new$GENERO.FILHO.A.),
      whitney.abg(Dados.7.new$qvfi7.6, Dados.7.new$FILHO.ESTUDA.),
      whitney.abg(Dados.7.new$qvfi7.6, Dados.7.new$ESCOLA.ESPECIAL.),
      whitney.abg(Dados.7.new$qvfi7.6, Dados.7.new$acessibilidade.recursos),
      whitney.abg(Dados.7.new$qvfi7.6, Dados.7.new$assistente.recursos),
      whitney.abg(Dados.7.new$qvfi7.6, Dados.7.new$computador.recursos),
      whitney.abg(Dados.7.new$qvfi7.6, Dados.7.new$livrosembraile.recursos),
      whitney.abg(Dados.7.new$qvfi7.6, Dados.7.new$nenhum.recursos),
      whitney.abg(Dados.7.new$qvfi7.6, Dados.7.new$ATIVIDADE.FÍSICA.),
      whitney.abg(Dados.7.new$qvfi7.6, Dados.7.new$ATIVIDADE.DE.LAZER),
      whitney.abg(Dados.7.new$qvfi7.6, Dados.7.new$TRABALHA.FORA.),
      kruskal.abg(Dados.7.new$qvfi7.6, Dados.7.new$ESTADO.CIVIL)$`tabela`,
      whitney.abg(Dados.7.new$qvfi7.6, Dados.7.new$VIVE.COM.PAI.),
      whitney.abg(Dados.7.new$qvfi7.6, Dados.7.new$TRABALHO.INFORMAL.),
      whitney.abg(Dados.7.new$qvfi7.6, Dados.7.new$BENEFICIO.SOCIAL.),
      whitney.abg(Dados.7.new$qvfi7.6, Dados.7.new$PLANO.SAÚDE),
      whitney.abg(Dados.7.new$qvfi7.6, Dados.7.new$GANHOU.PESO.),
      whitney.abg(Dados.7.new$qvfi7.6, Dados.7.new$DOR.DE.CABEÇA),
      whitney.abg(Dados.7.new$qvfi7.6, Dados.7.new$TONTURA),
      whitney.abg(Dados.7.new$qvfi7.6, Dados.7.new$NÁUSEA),
      whitney.abg(Dados.7.new$qvfi7.6, Dados.7.new$INTOLERÂNCIA.À.LUZ),
      whitney.abg(Dados.7.new$qvfi7.6, Dados.7.new$TRISTEZA),
      whitney.abg(Dados.7.new$qvfi7.6, Dados.7.new$ESQUECIMENTO),
      whitney.abg(Dados.7.new$qvfi7.6, Dados.7.new$CANSAÇO),
      whitney.abg(Dados.7.new$qvfi7.6, Dados.7.new$DORES.NO.CORPO),
      whitney.abg(Dados.7.new$qvfi7.6, Dados.7.new$INSÔNIA),
      whitney.abg(Dados.7.new$qvfi7.6, Dados.7.new$DIFICULDADE.PEGAR.NO.SONO),
      whitney.abg(Dados.7.new$qvfi7.6, Dados.7.new$ACORDA.NA.MADRUGADA),
      whitney.abg(Dados.7.new$qvfi7.6, Dados.7.new$QUEDA.CABELO),
      whitney.abg(Dados.7.new$qvfi7.6, Dados.7.new$HIPERTENSÃO),
      whitney.abg(Dados.7.new$qvfi7.6, Dados.7.new$AUMENTO.GLICEMIA))

###================================
### Tabela - solicitação 08/08/2019
###================================

rbind(
whitney.abg(Dados.dom$GERAL.STRESS, Dados.socio$GENERO.FILHO.A.)
      ,whitney.abg(Dados.dom$GERAL.STRESS, Dados.socio$FILHO.ESTUDA.)
      ,whitney.abg(Dados.dom$GERAL.STRESS, Dados.socio$ESCOLA.ESPECIAL.)
      ,whitney.abg(Dados.dom$GERAL.STRESS, Dados.socio$acessibilidade.recursos)
      ,whitney.abg(Dados.dom$GERAL.STRESS, Dados.socio$assistente.recursos)
      ,whitney.abg(Dados.dom$GERAL.STRESS, Dados.socio$computador.recursos)
      ,whitney.abg(Dados.dom$GERAL.STRESS, Dados.socio$livrosembraile.recursos)
      ,whitney.abg(Dados.dom$GERAL.STRESS, Dados.socio$nenhum.recursos)
      ,whitney.abg(Dados.dom$GERAL.STRESS, Dados.socio$ATIVIDADE.FÍSICA.)
      ,whitney.abg(Dados.dom$GERAL.STRESS, Dados.socio$ATIVIDADE.DE.LAZER)
      ,whitney.abg(Dados.dom$GERAL.STRESS, Dados.socio$TRABALHA.FORA.)
      ,kruskal.abg(Dados.dom$GERAL.STRESS, Dados.socio$ESTADO.CIVIL)$`tabela`
      ,whitney.abg(Dados.dom$GERAL.STRESS, Dados.socio$VIVE.COM.PAI.)
      ,whitney.abg(Dados.dom$GERAL.STRESS, Dados.socio$TRABALHO.INFORMAL.)
      ,whitney.abg(Dados.dom$GERAL.STRESS, Dados.socio$BENEFICIO.SOCIAL.)
      ,whitney.abg(Dados.dom$GERAL.STRESS, Dados.socio$PLANO.SAÚDE)
      ,whitney.abg(Dados.dom$GERAL.STRESS, Dados.socio$GANHOU.PESO.)
      ,whitney.abg(Dados.dom$GERAL.STRESS, Dados.socio$DOR.DE.CABEÇA)
      ,whitney.abg(Dados.dom$GERAL.STRESS, Dados.socio$TONTURA)
      ,whitney.abg(Dados.dom$GERAL.STRESS, Dados.socio$NÁUSEA)
      ,whitney.abg(Dados.dom$GERAL.STRESS, Dados.socio$INTOLERÂNCIA.À.LUZ)
      ,whitney.abg(Dados.dom$GERAL.STRESS, Dados.socio$TRISTEZA)
      ,whitney.abg(Dados.dom$GERAL.STRESS, Dados.socio$ESQUECIMENTO)
      ,whitney.abg(Dados.dom$GERAL.STRESS, Dados.socio$CANSAÇO)
      ,whitney.abg(Dados.dom$GERAL.STRESS, Dados.socio$DORES.NO.CORPO)
      ,whitney.abg(Dados.dom$GERAL.STRESS, Dados.socio$INSÔNIA)
      ,whitney.abg(Dados.dom$GERAL.STRESS, Dados.socio$DIFICULDADE.PEGAR.NO.SONO)
      ,whitney.abg(Dados.dom$GERAL.STRESS, Dados.socio$ACORDA.NA.MADRUGADA)
      ,whitney.abg(Dados.dom$GERAL.STRESS, Dados.socio$QUEDA.CABELO)
      ,whitney.abg(Dados.dom$GERAL.STRESS, Dados.socio$HIPERTENSÃO)
      ,whitney.abg(Dados.dom$GERAL.STRESS, Dados.socio$AUMENTO.GLICEMIA))

###=========================================
### Gráficos Tabela - solicitação 08/08/2019
###=========================================

svg("Graf43_L.svg", height = 6, width = 12)
par(mfrow=c(1,2))
boxplot(Dados.dom$GERAL.STRESS ~ Dados.socio$VIVE.COM.PAI., col = "skyblue4", xlab = "Vive com o pai", 
        ylab = "Estresse geral", ylim=c(12,105),
        main = "Estresse geral\npor vive com o pai", 
        names = c("Não","Sim"))
text(1.5, 15, "Valor-p = 0,025", cex=1.0)

boxplot(Dados.dom$GERAL.STRESS ~ Dados.socio$DOR.DE.CABEÇA, col = "skyblue4", xlab = "Dor de cabeça", 
        ylab = "Estresse geral", ylim=c(12,105),
        main = "Estresse geral\npor dor de cabeça", 
        names = c("Não","Sim"))
text(1.5, 15, "Valor-p = 0,005", cex=1.0)
dev.off()

svg("Graf44_L.svg", height = 6, width = 12)
par(mfrow=c(1,2))
boxplot(Dados.dom$GERAL.STRESS ~ Dados.socio$INTOLERÂNCIA.À.LUZ, col = "skyblue4", xlab = "Intolerância à luz", 
        ylab = "Estresse geral", ylim=c(12,105),
        main = "Estresse geral\npor intolerância à luz")
text(1.5, 15, "Valor-p < 0,001", cex=1.0)

boxplot(Dados.dom$GERAL.STRESS ~ Dados.socio$TRISTEZA, col = "skyblue4", xlab = "Tristeza", 
        ylab = "Estresse geral", ylim=c(12,105),
        main = "Estresse geral\npor tristeza", 
        names = c("Não","Sim"))
text(1.5, 15, "Valor-p < 0,001", cex=1.0)
dev.off()

svg("Graf45_L.svg", height = 6, width = 12)
par(mfrow=c(1,2))
boxplot(Dados.dom$GERAL.STRESS ~ Dados.socio$ESQUECIMENTO, col = "skyblue4", xlab = "Esquecimento", 
        ylab = "Estresse geral", ylim=c(12,105),
        main = "Estresse geral\npor esquecimento", 
        names = c("Não","Sim"))
text(1.5, 15, "Valor-p = 0,017", cex=1.0)

boxplot(Dados.dom$GERAL.STRESS ~ Dados.socio$CANSAÇO, col = "skyblue4", xlab = "Cansaço", 
        ylab = "Estresse geral", ylim=c(12,105),
        main = "Estresse geral\npor cansaço", 
        names = c("Não","Sim"))
text(1.5, 15, "Valor-p = 0,002", cex=1.0)
dev.off()

svg("Graf46_L.svg", height = 6, width = 12)
par(mfrow=c(1,2))
boxplot(Dados.dom$GERAL.STRESS ~ Dados.socio$DORES.NO.CORPO, col = "skyblue4", xlab = "Dores no corpo", 
        ylab = "Estresse geral", ylim=c(12,105),
        main = "Estresse geral\npor dores no corpo", 
        names = c("Não","Sim"))
text(1.5, 15, "Valor-p = 0,008", cex=1.0)

boxplot(Dados.dom$GERAL.STRESS ~ Dados.socio$INSÔNIA, col = "skyblue4", xlab = "Insônia", 
        ylab = "Estresse geral", ylim=c(12,105),
        main = "Estresse geral\npor insônia", 
        names = c("Não","Sim"))
text(1.5, 15, "Valor-p = 0,004", cex=1.0)
dev.off()

svg("Graf47_L.svg", height = 6, width = 12)
par(mfrow=c(1,2))
boxplot(Dados.dom$GERAL.STRESS ~ Dados.socio$DIFICULDADE.PEGAR.NO.SONO, col = "skyblue4", xlab = "Dificuldade de pegar no sono", 
        ylab = "Estresse geral", ylim=c(12,105),
        main = "Estresse geral\npor dificuldade de pegar no sono", 
        names = c("Não","Sim"))
text(1.5, 15, "Valor-p = 0,007", cex=1.0)

boxplot(Dados.dom$GERAL.STRESS ~ Dados.socio$ACORDA.NA.MADRUGADA, col = "skyblue4", xlab = "Acorda na madrugada", 
        ylab = "Estresse geral", ylim=c(12,105),
        main = "Estresse geral\npor acorda na madrugada", 
        names = c("Não","Sim"))
text(1.5, 15, "Valor-p = 0,008", cex=1.0)
dev.off()

###================================
### Tabela - solicitação 08/08/2019
###================================

rbind(
  whitney.abg(Dados.dom$POSTURA.DEFESA, Dados.socio$GENERO.FILHO.A.)
  ,whitney.abg(Dados.dom$POSTURA.DEFESA, Dados.socio$FILHO.ESTUDA.)
  ,whitney.abg(Dados.dom$POSTURA.DEFESA, Dados.socio$ESCOLA.ESPECIAL.)
  ,whitney.abg(Dados.dom$POSTURA.DEFESA, Dados.socio$acessibilidade.recursos)
  ,whitney.abg(Dados.dom$POSTURA.DEFESA, Dados.socio$assistente.recursos)
  ,whitney.abg(Dados.dom$POSTURA.DEFESA, Dados.socio$computador.recursos)
  ,whitney.abg(Dados.dom$POSTURA.DEFESA, Dados.socio$livrosembraile.recursos)
  ,whitney.abg(Dados.dom$POSTURA.DEFESA, Dados.socio$nenhum.recursos)
  ,whitney.abg(Dados.dom$POSTURA.DEFESA, Dados.socio$ATIVIDADE.FÍSICA.)
  ,whitney.abg(Dados.dom$POSTURA.DEFESA, Dados.socio$ATIVIDADE.DE.LAZER)
  ,whitney.abg(Dados.dom$POSTURA.DEFESA, Dados.socio$TRABALHA.FORA.)
  ,kruskal.abg(Dados.dom$POSTURA.DEFESA, Dados.socio$ESTADO.CIVIL)$`tabela`
  ,whitney.abg(Dados.dom$POSTURA.DEFESA, Dados.socio$VIVE.COM.PAI.)
  ,whitney.abg(Dados.dom$POSTURA.DEFESA, Dados.socio$TRABALHO.INFORMAL.)
  ,whitney.abg(Dados.dom$POSTURA.DEFESA, Dados.socio$BENEFICIO.SOCIAL.)
  ,whitney.abg(Dados.dom$POSTURA.DEFESA, Dados.socio$PLANO.SAÚDE)
  ,whitney.abg(Dados.dom$POSTURA.DEFESA, Dados.socio$GANHOU.PESO.)
  ,whitney.abg(Dados.dom$POSTURA.DEFESA, Dados.socio$DOR.DE.CABEÇA)
  ,whitney.abg(Dados.dom$POSTURA.DEFESA, Dados.socio$TONTURA)
  ,whitney.abg(Dados.dom$POSTURA.DEFESA, Dados.socio$NÁUSEA)
  ,whitney.abg(Dados.dom$POSTURA.DEFESA, Dados.socio$INTOLERÂNCIA.À.LUZ)
  ,whitney.abg(Dados.dom$POSTURA.DEFESA, Dados.socio$TRISTEZA)
  ,whitney.abg(Dados.dom$POSTURA.DEFESA, Dados.socio$ESQUECIMENTO)
  ,whitney.abg(Dados.dom$POSTURA.DEFESA, Dados.socio$CANSAÇO)
  ,whitney.abg(Dados.dom$POSTURA.DEFESA, Dados.socio$DORES.NO.CORPO)
  ,whitney.abg(Dados.dom$POSTURA.DEFESA, Dados.socio$INSÔNIA)
  ,whitney.abg(Dados.dom$POSTURA.DEFESA, Dados.socio$DIFICULDADE.PEGAR.NO.SONO)
  ,whitney.abg(Dados.dom$POSTURA.DEFESA, Dados.socio$ACORDA.NA.MADRUGADA)
  ,whitney.abg(Dados.dom$POSTURA.DEFESA, Dados.socio$QUEDA.CABELO)
  ,whitney.abg(Dados.dom$POSTURA.DEFESA, Dados.socio$HIPERTENSÃO)
  ,whitney.abg(Dados.dom$POSTURA.DEFESA, Dados.socio$AUMENTO.GLICEMIA))

###=========================================
### Gráficos Tabela - solicitação 08/08/2019
###=========================================

svg("Graf48_L.svg", height = 6, width = 12)
par(mfrow=c(1,2))
boxplot(Dados.dom$POSTURA.DEFESA ~ Dados.socio$VIVE.COM.PAI., col = "skyblue4", xlab = "Vive com o pai", 
        ylab = "Postura de defesa", ylim=c(14,72),
        main = "Postura de defesa\npor vive com o pai", 
        names = c("Não","Sim"))
text(1.5, 15, "Valor-p = 0,013", cex=1.0)

boxplot(Dados.dom$POSTURA.DEFESA ~ Dados.socio$DOR.DE.CABEÇA, col = "skyblue4", xlab = "Dor de cabeça", 
        ylab = "Postura de defesa", ylim=c(14,72),
        main = "Postura de defesa\npor dor de cabeça", 
        names = c("Não","Sim"))
text(1.5, 15, "Valor-p = 0,013", cex=1.0)
dev.off()

svg("Graf49_L.svg", height = 6, width = 12)
par(mfrow=c(1,2))
boxplot(Dados.dom$POSTURA.DEFESA ~ Dados.socio$INTOLERÂNCIA.À.LUZ, col = "skyblue4", xlab = "Intolerãncia à luz", 
        ylab = "Postura de defesa", ylim=c(14,72),
        main = "Postura de defesa\npor intolerância à luz", 
        names = c("Não","Sim"))
text(1.5, 15, "Valor-p = 0,001", cex=1.0)

boxplot(Dados.dom$POSTURA.DEFESA ~ Dados.socio$TRISTEZA, col = "skyblue4", xlab = "Tristeza",
        ylab = "Postura de defesa", ylim=c(14,72),
        main = "Postura de defesa\npor tristeza", 
        names = c("Não","Sim"))
text(1.5, 15, "Valor-p < 0,001", cex=1.0)
dev.off()

svg("Graf50_L.svg", height = 6, width = 12)
par(mfrow=c(1,2))
boxplot(Dados.dom$POSTURA.DEFESA ~ Dados.socio$CANSAÇO, col = "skyblue4", xlab = "Cansaço", 
        ylab = "Postura de defesa", ylim=c(14,72),
        main = "Postura de defesa\npor cansaço", 
        names = c("Não","Sim"))
text(1.5, 15, "Valor-p = 0,011", cex=1.0)

boxplot(Dados.dom$POSTURA.DEFESA ~ Dados.socio$DORES.NO.CORPO, col = "skyblue4", xlab = "Dores no corpo",
        ylab = "Postura de defesa", ylim=c(14,72),
        main = "Postura de defesa\npor dores no corpo", 
        names = c("Não","Sim"))
text(1.5, 15, "Valor-p = 0,001", cex=1.0)
dev.off()

svg("Graf51_L.svg", height = 6, width = 12)
par(mfrow=c(1,2))
boxplot(Dados.dom$POSTURA.DEFESA ~ Dados.socio$INSÔNIA, col = "skyblue4", xlab = "Insônia", 
        ylab = "Postura de defesa", ylim=c(14,72),
        main = "Postura de defesa\npor insônia", 
        names = c("Não","Sim"))
text(1.5, 15, "Valor-p = 0,005", cex=1.0)

boxplot(Dados.dom$POSTURA.DEFESA ~ Dados.socio$DIFICULDADE.PEGAR.NO.SONO, col = "skyblue4", xlab = "Dificuldade de pegar no sono", 
        ylab = "Postura de defesa", ylim=c(14,72),
        main = "Postura de defesa\npor dificuldade de pegar no sono", 
        names = c("Não","Sim"))
text(1.5, 15, "Valor-p = 0,004", cex=1.0)
dev.off()

svg("Graf52_L.svg", height = 6, width = 12)
par(mfrow=c(1,2))
boxplot(Dados.dom$POSTURA.DEFESA ~ Dados.socio$ACORDA.NA.MADRUGADA, col = "skyblue4", xlab = "Acorda na madrugada", 
        ylab = "Postura de defesa", ylim=c(14,72),
        main = "Postura de defesa\npor acorda na madrugada", 
        names = c("Não","Sim"))
text(1.5, 15, "Valor-p = 0,020", cex=1.0)

dev.off()

########################################## Parte 2 ###########################################

# Tabela 26 - Domínio da criança reduzido

rbind(whitney.abg(IND_CRIANÇA, Dados.socio$GENERO.FILHO.A.),
      whitney.abg(IND_CRIANÇA, Dados.socio$FILHO.ESTUDA.),
      whitney.abg(IND_CRIANÇA, Dados.socio$ESCOLA.ESPECIAL.),
      whitney.abg(IND_CRIANÇA, Dados.socio$acessibilidade.recursos),
      whitney.abg(IND_CRIANÇA, Dados.socio$assistente.recursos),
      whitney.abg(IND_CRIANÇA, Dados.socio$computador.recursos),
      whitney.abg(IND_CRIANÇA, Dados.socio$livrosembraile.recursos),
      whitney.abg(IND_CRIANÇA, Dados.socio$nenhum.recursos),
      whitney.abg(IND_CRIANÇA, Dados.socio$ATIVIDADE.FÍSICA.),
      whitney.abg(IND_CRIANÇA, Dados.socio$ATIVIDADE.DE.LAZER),
      whitney.abg(IND_CRIANÇA, Dados.socio$TRABALHA.FORA.),
      kruskal.abg(IND_CRIANÇA, Dados.socio$ESTADO.CIVIL)$`tabela`,
      whitney.abg(IND_CRIANÇA, Dados.socio$VIVE.COM.PAI.),
      whitney.abg(IND_CRIANÇA, Dados.socio$TRABALHO.INFORMAL.),
      whitney.abg(IND_CRIANÇA, Dados.socio$BENEFICIO.SOCIAL.),
      whitney.abg(IND_CRIANÇA, Dados.socio$PLANO.SAÚDE),
      whitney.abg(IND_CRIANÇA, Dados.socio$GANHOU.PESO.),
      whitney.abg(IND_CRIANÇA, Dados.socio$DOR.DE.CABEÇA),
      whitney.abg(IND_CRIANÇA, Dados.socio$TONTURA),
      whitney.abg(IND_CRIANÇA, Dados.socio$NÁUSEA),
      whitney.abg(IND_CRIANÇA, Dados.socio$INTOLERÂNCIA.À.LUZ),
      whitney.abg(IND_CRIANÇA, Dados.socio$TRISTEZA),
      whitney.abg(IND_CRIANÇA, Dados.socio$ESQUECIMENTO),
      whitney.abg(IND_CRIANÇA, Dados.socio$CANSAÇO),
      whitney.abg(IND_CRIANÇA, Dados.socio$DORES.NO.CORPO),
      whitney.abg(IND_CRIANÇA, Dados.socio$INSÔNIA),
      whitney.abg(IND_CRIANÇA, Dados.socio$DIFICULDADE.PEGAR.NO.SONO),
      whitney.abg(IND_CRIANÇA, Dados.socio$ACORDA.NA.MADRUGADA),
      whitney.abg(IND_CRIANÇA, Dados.socio$QUEDA.CABELO),
      whitney.abg(IND_CRIANÇA, Dados.socio$HIPERTENSÃO),
      whitney.abg(IND_CRIANÇA, Dados.socio$AUMENTO.GLICEMIA))

# Tabela 27 - Domínio dos pais reduzido

rbind(whitney.abg(IND_PAIS, Dados.socio$GENERO.FILHO.A.),
      whitney.abg(IND_PAIS, Dados.socio$FILHO.ESTUDA.),
      whitney.abg(IND_PAIS, Dados.socio$ESCOLA.ESPECIAL.),
      whitney.abg(IND_PAIS, Dados.socio$acessibilidade.recursos),
      whitney.abg(IND_PAIS, Dados.socio$assistente.recursos),
      whitney.abg(IND_PAIS, Dados.socio$computador.recursos),
      whitney.abg(IND_PAIS, Dados.socio$livrosembraile.recursos),
      whitney.abg(IND_PAIS, Dados.socio$nenhum.recursos),
      whitney.abg(IND_PAIS, Dados.socio$ATIVIDADE.FÍSICA.),
      whitney.abg(IND_PAIS, Dados.socio$ATIVIDADE.DE.LAZER),
      whitney.abg(IND_PAIS, Dados.socio$TRABALHA.FORA.),
      kruskal.abg(IND_PAIS, Dados.socio$ESTADO.CIVIL)$`tabela`,
      whitney.abg(IND_PAIS, Dados.socio$VIVE.COM.PAI.),
      whitney.abg(IND_PAIS, Dados.socio$TRABALHO.INFORMAL.),
      whitney.abg(IND_PAIS, Dados.socio$BENEFICIO.SOCIAL.),
      whitney.abg(IND_PAIS, Dados.socio$PLANO.SAÚDE),
      whitney.abg(IND_PAIS, Dados.socio$GANHOU.PESO.),
      whitney.abg(IND_PAIS, Dados.socio$DOR.DE.CABEÇA),
      whitney.abg(IND_PAIS, Dados.socio$TONTURA),
      whitney.abg(IND_PAIS, Dados.socio$NÁUSEA),
      whitney.abg(IND_PAIS, Dados.socio$INTOLERÂNCIA.À.LUZ),
      whitney.abg(IND_PAIS, Dados.socio$TRISTEZA),
      whitney.abg(IND_PAIS, Dados.socio$ESQUECIMENTO),
      whitney.abg(IND_PAIS, Dados.socio$CANSAÇO),
      whitney.abg(IND_PAIS, Dados.socio$DORES.NO.CORPO),
      whitney.abg(IND_PAIS, Dados.socio$INSÔNIA),
      whitney.abg(IND_PAIS, Dados.socio$DIFICULDADE.PEGAR.NO.SONO),
      whitney.abg(IND_PAIS, Dados.socio$ACORDA.NA.MADRUGADA),
      whitney.abg(IND_PAIS, Dados.socio$QUEDA.CABELO),
      whitney.abg(IND_PAIS, Dados.socio$HIPERTENSÃO),
      whitney.abg(IND_PAIS, Dados.socio$AUMENTO.GLICEMIA))

### =======================
### Gráficos - Comparações
### =======================

################################################ Parte 1 #####################################################

# Gráfico 12 - Domínio da criança completo

svg("Graf28.svg", height = 6, width = 12)
par(mfrow=c(1,2))
boxplot(Dados.dom$Percentil.CD ~ Dados.socio$VIVE.COM.PAI., col = "skyblue4", xlab = "Vive com o pai", 
        ylab = "Indicador completo do domínio da criança", ylim=c(1,100),
        main = "Indicador completo do domínio da\ncriança por vive com o pai", 
        names = c("Não","Sim"))
text(1.5, 5, "Valor-p = 0,039", cex=1.0)

boxplot(Dados.dom$Percentil.CD ~ Dados.socio$DOR.DE.CABEÇA, col = "skyblue4", xlab = "Dor de cabeça", 
        ylab = "Indicador completo do domínio da criança", ylim=c(1,100),
        main = "Indicador completo do domínio da\ncriança por dor de cabeça", 
        names = c("Não","Sim"))
text(1.5, 5, "Valor-p = 0,003", cex=1.0)
dev.off()

svg("Graf29.svg", height = 6, width = 12)
par(mfrow=c(1,2))
boxplot(Dados.dom$Percentil.CD ~ Dados.socio$INTOLERÂNCIA.À.LUZ, col = "skyblue4", xlab = "Intolerância à luz", 
        ylab = "Indicador completo do domínio da criança", ylim=c(1,100),
        main = "Indicador completo do domínio da\ncriança por intolerância à luz", 
        names = c("Não","Sim"))
text(1.5, 5, "Valor-p = 0,003", cex=1.0)

boxplot(Dados.dom$Percentil.CD ~ Dados.socio$TRISTEZA, col = "skyblue4", xlab = "Tristeza", 
        ylab = "Indicador completo do domínio da criança", ylim=c(1,100),
        main = "Indicador completo do domínio da\ncriança por tristeza", 
        names = c("Não","Sim"))
text(1.5, 5, "Valor-p = 0,036", cex=1.0)
dev.off()

svg("Graf30.svg", height = 6, width = 12)
par(mfrow=c(1,2))
boxplot(Dados.dom$Percentil.CD ~ Dados.socio$CANSAÇO, col = "skyblue4", xlab = "Cansaço", 
        ylab = "Indicador completo do domínio da criança", ylim=c(1,100),
        main = "Indicador completo do domínio da\ncriança por cansaço", 
        names = c("Não","Sim"))
text(1.5, 5, "Valor-p = 0,011", cex=1.0)

boxplot(Dados.dom$Percentil.CD ~ Dados.socio$DORES.NO.CORPO, col = "skyblue4", xlab = "Dores no corpo", 
        ylab = "Indicador completo do domínio da criança", ylim=c(1,100),
        main = "Indicador completo do domínio da\ncriança por dores no corpo", 
        names = c("Não","Sim"))
text(1.5, 5, "Valor-p = 0,037", cex=1.0)
dev.off()

svg("Graf31.svg", height = 6, width = 12)
par(mfrow=c(1,2))
boxplot(Dados.dom$Percentil.CD ~ Dados.socio$INSÔNIA, col = "skyblue4", xlab = "Insônia", 
        ylab = "Indicador completo do domínio da criança", ylim=c(1,100),
        main = "Indicador completo do domínio da\ncriança por insônia", 
        names = c("Não","Sim"))
text(1.5, 5, "Valor-p = 0,023", cex=1.0)

boxplot(Dados.dom$Percentil.CD ~ Dados.socio$DIFICULDADE.PEGAR.NO.SONO, col = "skyblue4", xlab = "Dificuldade de pegar no sono", 
        ylab = "Indicador completo do domínio da criança", ylim=c(1,100),
        main = "Indicador completo do domínio da\ncriança por dificuldade de pegar no sono", 
        names = c("Não","Sim"))
text(1.5, 5, "Valor-p = 0,038", cex=1.0)
dev.off()

svg("Graf32.svg", height = 6, width = 6)
par(mfrow=c(1,1))
boxplot(Dados.dom$Percentil.CD ~ Dados.socio$ACORDA.NA.MADRUGADA, col = "skyblue4", xlab = "Acorda na madrugada", 
        ylab = "Indicador completo do domínio da criança", ylim=c(1,100),
        main = "Indicador completo do domínio da\ncriança por acorda na madrugada", 
        names = c("Não","Sim"))
text(1.5, 5, "Valor-p = 0,013", cex=1.0)
dev.off()

# Gráfico 13 - Domínio dos pais completo 

svg("Graf33.svg", height = 6, width = 12)
par(mfrow=c(1,2))
boxplot(Dados.dom$Percentil.PD ~ Dados.socio$VIVE.COM.PAI., col = "skyblue4", xlab = "Vive com o pai", 
        ylab = "Indicador completo do domínio dos pais", ylim=c(1,100),
        main = "Indicador completo do domínio dos\npais por vive com o pai", 
        names = c("Não","Sim"))
text(1.5, 5, "Valor-p = 0,046", cex=1.0)

boxplot(Dados.dom$Percentil.PD ~ Dados.socio$DOR.DE.CABEÇA, col = "skyblue4", xlab = "Dor de cabeça", 
        ylab = "Indicador completo do domínio dos pais", ylim=c(1,100),
        main = "Indicador completo do domínio dos\npais por dor de cabeça", 
        names = c("Não","Sim"))
text(1.5, 5, "Valor-p = 0,006", cex=1.0)
dev.off()

svg("Graf34.svg", height = 6, width = 12)
par(mfrow=c(1,2))
boxplot(Dados.dom$Percentil.PD ~ Dados.socio$INTOLERÂNCIA.À.LUZ, col = "skyblue4", xlab = "Intolerância à luz", 
        ylab = "Indicador completo do domínio dos pais", ylim=c(1,100),
        main = "Indicador completo do domínio dos\npais por intolerância à luz", 
        names = c("Não","Sim"))
text(1.5, 5, "Valor-p < 0,001", cex=1.0)

boxplot(Dados.dom$Percentil.PD ~ Dados.socio$TRISTEZA, col = "skyblue4", xlab = "Tristeza", 
        ylab = "Indicador completo do domínio dos pais", ylim=c(1,100),
        main = "Indicador completo do domínio dos\npais por tristeza", 
        names = c("Não","Sim"))
text(1.5, 5, "Valor-p < 0,001", cex=1.0)
dev.off()

svg("Graf35.svg", height = 6, width = 12)
par(mfrow=c(1,2))
boxplot(Dados.dom$Percentil.PD ~ Dados.socio$ESQUECIMENTO, col = "skyblue4", xlab = "Esquecimento", 
        ylab = "Indicador completo do domínio dos pais", ylim=c(1,100),
        main = "Indicador completo do domínio dos\npais por esquecimento", 
        names = c("Não","Sim"))
text(1.5, 5, "Valor-p = 0,022", cex=1.0)

boxplot(Dados.dom$Percentil.PD ~ Dados.socio$CANSAÇO, col = "skyblue4", xlab = "Cansaço", 
        ylab = "Indicador completo do domínio dos pais", ylim=c(1,100),
        main = "Indicador completo do domínio dos\npais por cansaço", 
        names = c("Não","Sim"))
text(1.5, 5, "Valor-p = 0,005", cex=1.0)
dev.off()

svg("Graf36.svg", height = 6, width = 12)
par(mfrow=c(1,2))
boxplot(Dados.dom$Percentil.PD ~ Dados.socio$DORES.NO.CORPO, col = "skyblue4", xlab = "Dores no corpo", 
        ylab = "Indicador completo do domínio dos pais", ylim=c(1,100),
        main = "Indicador completo do domínio dos\npais por dores no corpo", 
        names = c("Não","Sim"))
text(1.5, 5, "Valor-p = 0,002", cex=1.0)

boxplot(Dados.dom$Percentil.PD ~ Dados.socio$INSÔNIA, col = "skyblue4", xlab = "Insônia", 
        ylab = "Indicador completo do domínio dos pais", ylim=c(1,100),
        main = "Indicador completo do domínio dos\npais por insônia", 
        names = c("Não","Sim"))
text(1.5, 5, "Valor-p = 0,007", cex=1.0)
dev.off()

svg("Graf37.svg", height = 6, width = 6)
par(mfrow=c(1,1))
boxplot(Dados.dom$Percentil.PD ~ Dados.socio$DIFICULDADE.PEGAR.NO.SONO, col = "skyblue4", xlab = "Dificuldade de pegar no sono", 
        ylab = "Indicador completo do domínio dos pais", ylim=c(1,100),
        main = "Indicador completo do domínio dos\npais por dificuldade de pegar no sono", 
        names = c("Não","Sim"))
text(1.5, 5, "Valor-p = 0,010", cex=1.0)
dev.off()

# Gráfico 14 - Domínio do estresse da vida

svg("Graf38.svg", height = 6, width = 6)
par(mfrow=c(1,1))
boxplot(Dados.dom$Soma.LS ~ Dados.socio$NÁUSEA, col = "skyblue4", xlab = "Náusea", 
        ylab = "Indicador completo do domínio do estresse da vida", ylim=c(0,25),
        main = "Indicador completo do domínio do\nestresse da vida por náusea", 
        names = c("Não","Sim"))
text(1.5, 23, "Valor-p = 0,036", cex=1.0)
dev.off()

# Gráfico 15 - QVFI-3

svg("Graf39.svg", height = 6, width = 12)
par(mfrow=c(1,2))
boxplot(Dados.3.new$qvfi3.6 ~ Dados.3.new$ATIVIDADE.FÍSICA., col = "skyblue4", xlab = "Atividade física", 
        ylab = "Indicador QFVI-3", ylim=c(20,80),
        main = "Indicador QFVI-3\npor atividade física", 
        names = c("Não","Sim"))
text(1.5, 25, "Valor-p = 0,038", cex=1.0)

boxplot(Dados.3.new$qvfi3.6 ~ Dados.3.new$NÁUSEA, col = "skyblue4", xlab = "Náusea", 
        ylab = "Indicador QFVI-3", ylim=c(20,80),
        main = "Indicador QFVI-3\npor náusea", 
        names = c("Não","Sim"))
text(1.5, 25, "Valor-p = 0,044", cex=1.0)
dev.off()

svg("Graf40.svg", height = 6, width = 12)
par(mfrow=c(1,2))
boxplot(Dados.3.new$qvfi3.6 ~ Dados.3.new$ATIVIDADE.FÍSICA., col = "skyblue4", xlab = "Cansaço", 
        ylab = "Indicador QFVI-3", ylim=c(20,80),
        main = "Indicador QFVI-3\npor cansaço", 
        names = c("Não","Sim"))
text(1.5, 25, "Valor-p = 0,038", cex=1.0)

boxplot(Dados.3.new$qvfi3.6 ~ Dados.3.new$DORES.NO.CORPO, col = "skyblue4", xlab = "Dores no corpo", 
        ylab = "Indicador QFVI-3", ylim=c(20,80),
        main = "Indicador QFVI-3\npor dores no corpo", 
        names = c("Não","Sim"))
text(1.5, 25, "Valor-p = 0,006", cex=1.0)
dev.off()

# Gráfico 16 - QVFI-7

svg("Graf41.svg", height = 6, width = 12)
par(mfrow=c(1,2))
boxplot(Dados.7.new$qvfi7.6 ~ Dados.7.new$FILHO.ESTUDA., col = "skyblue4", xlab = "Filho estuda", 
        ylab = "Indicador QFVI-7", ylim=c(20,80),
        main = "Indicador QFVI-7\npor filho estuda", 
        names = c("Não","Sim"))
text(1.5, 23, "Valor-p = 0,038", cex=1.0)

boxplot(Dados.7.new$qvfi7.6 ~ Dados.7.new$NÁUSEA, col = "skyblue4", xlab = "Náusea", 
        ylab = "Indicador QFVI-7", ylim=c(20,80),
        main = "Indicador QFVI-7\npor náusea", 
        names = c("Não","Sim"))
text(1.5, 23, "Valor-p = 0,044", cex=1.0)
dev.off()

svg("Graf42.svg", height = 6, width = 12)
par(mfrow=c(1,2))
boxplot(Dados.7.new$qvfi7.6 ~ Dados.7.new$INTOLERÂNCIA.À.LUZ, col = "skyblue4", xlab = "Intolerância à luz", 
        ylab = "Indicador QFVI-7", ylim=c(20,80),
        main = "Indicador QFVI-7\npor intolerância à luz", 
        names = c("Não","Sim"))
text(1.5, 23, "Valor-p = 0,034", cex=1.0)

boxplot(Dados.7.new$qvfi7.6 ~ Dados.7.new$DIFICULDADE.PEGAR.NO.SONO, col = "skyblue4", xlab = "Dificuldade de pegar no sono", 
        ylab = "Indicador QFVI-7", ylim=c(20,80),
        main = "Indicador QFVI-7\npor dificuldade de pegar no sono", 
        names = c("Não","Sim"))
text(1.5, 23, "Valor-p = 0,002", cex=1.0)
dev.off()

################################################ Parte 2 ####################################################

# Gráfico 24 - Domínio da criança reduzido

svg("Graf21.svg", height = 6, width = 12)
par(mfrow=c(1,2))
boxplot(IND_CRIANÇA ~ Dados.socio$DOR.DE.CABEÇA, col = "skyblue4", xlab = "Dor de cabeça", 
        ylab = "Indicador reduzido do domínio da criança", ylim=c(1,5),
        main = "Indicador reduzido do domínio da\ncriança por dor de cabeça", 
        names = c("Não","Sim"))
text(1.5, 5, "Valor-p = 0,029", cex=1.0)

boxplot(IND_CRIANÇA ~ Dados.socio$INTOLERÂNCIA.À.LUZ, col = "skyblue4", xlab = "Intolerância à luz", 
        ylab = "Indicador reduzido do domínio da criança", ylim=c(1,5),
        main = "Indicador reduzido do domínio da\ncriança por intolerância à luz", 
        names = c("Não","Sim"))
text(1.5, 5, "Valor-p = 0,013", cex=1.0)
dev.off()

svg("Graf22.svg", height = 6, width = 12)
par(mfrow=c(1,2))
boxplot(IND_CRIANÇA ~ Dados.socio$DORES.NO.CORPO, col = "skyblue4", xlab = "Dores no corpo", 
        ylab = "Indicador reduzido do domínio da criança", ylim=c(1,5),
        main = "Indicador reduzido do domínio da\ncriança por dores no corpo", 
        names = c("Não","Sim"))
text(1.5, 5, "Valor-p = 0,036", cex=1.0)

boxplot(IND_CRIANÇA ~ Dados.socio$INSÔNIA, col = "skyblue4", xlab = "Insônia", 
        ylab = "Indicador reduzido do domínio da criança", ylim=c(1,5),
        main = "Indicador reduzido do domínio da\ncriança por insônia", 
        names = c("Não","Sim"))
text(1.5, 5, "Valor-p = 0,047", cex=1.0)
dev.off()

svg("Graf23.svg", height = 6, width = 6)
par(mfrow=c(1,1))
boxplot(IND_CRIANÇA ~ Dados.socio$DIFICULDADE.PEGAR.NO.SONO, col = "skyblue4", xlab = "Dificuldade de pegar no sono", 
        ylab = "Indicador reduzido do domínio da criança", ylim=c(1,5),
        main = "Indicador reduzido do domínio da\ncriança por dificuldade de pegar no sono", 
        names = c("Não","Sim"))
text(1.5, 5, "Valor-p = 0,042", cex=1.0)
dev.off()

# Gráfico 25 - Domínio dos pais reduzido

svg("Graf24.svg", height = 6, width = 12)
par(mfrow=c(1,2))
boxplot(IND_PAIS ~ Dados.socio$VIVE.COM.PAI., col = "skyblue4", xlab = "Vive com pai", 
        ylab = "Indicador reduzido do domínio dos pais", ylim=c(1,5),
        main = "Indicador reduzido do domínio dos\npais por vive com pai", 
        names = c("Não","Sim"))
text(1.5, 5, "Valor-p = 0,015", cex=1.0)

boxplot(IND_PAIS ~ Dados.socio$INTOLERÂNCIA.À.LUZ, col = "skyblue4", xlab = "Intolerância à luz", 
        ylab = "Indicador reduzido do domínio dos pais", ylim=c(1,5),
        main = "Indicador reduzido do domínio dos\npais por intolerância à luz", 
        names = c("Não","Sim"))
text(1.5, 5, "Valor-p < 0,001", cex=1.0)
dev.off()

svg("Graf25.svg", height = 6, width = 12)
par(mfrow=c(1,2))
boxplot(IND_PAIS ~ Dados.socio$TRISTEZA, col = "skyblue4", xlab = "Tristeza", 
        ylab = "Indicador reduzido do domínio dos pais", ylim=c(1,5),
        main = "Indicador reduzido do domínio dos\npais por tristeza", 
        names = c("Não","Sim"))
text(1.5, 5, "Valor-p < 0,001", cex=1.0)

boxplot(IND_PAIS ~ Dados.socio$CANSAÇO, col = "skyblue4", xlab = "Cansaço", 
        ylab = "Indicador reduzido do domínio dos pais", ylim=c(1,5),
        main = "Indicador reduzido do domínio dos\npais por cansaço", 
        names = c("Não","Sim"))
text(1.5, 5, "Valor-p = 0,027", cex=1.0)
dev.off()

svg("Graf26.svg", height = 6, width = 12)
par(mfrow=c(1,2))
boxplot(IND_PAIS ~ Dados.socio$DORES.NO.CORPO, col = "skyblue4", xlab = "Dores no corpo", 
        ylab = "Indicador reduzido do domínio dos pais", ylim=c(1,5),
        main = "Indicador reduzido do domínio dos\npais por dores no corpo", 
        names = c("Não","Sim"))
text(1.5, 5, "Valor-p = 0,001", cex=1.0)

boxplot(IND_PAIS ~ Dados.socio$INSÔNIA, col = "skyblue4", xlab = "Insônia", 
        ylab = "Indicador reduzido do domínio dos pais", ylim=c(1,5),
        main = "Indicador reduzido do domínio dos\npais por insônia", 
        names = c("Não","Sim"))
text(1.5, 5, "Valor-p = 0,001", cex=1.0)
dev.off()

svg("Graf27.svg", height = 6, width = 12)
par(mfrow=c(1,2))
boxplot(IND_PAIS ~ Dados.socio$DIFICULDADE.PEGAR.NO.SONO, col = "skyblue4", xlab = "Dificuldade de pegar no sono", 
        ylab = "Indicador reduzido do domínio dos pais", ylim=c(1,5),
        main = "Indicador reduzido do domínio dos\npais por dificuldade de pegar no sono", 
        names = c("Não","Sim"))
text(1.5, 5, "Valor-p = 0,001", cex=1.0)

boxplot(IND_PAIS ~ Dados.socio$ACORDA.NA.MADRUGADA, col = "skyblue4", xlab = "Acorda na mardugada", 
        ylab = "Indicador reduzido do domínio dos pais", ylim=c(1,5),
        main = "Indicador reduzido do domínio dos\npais por acorda na mardugada", 
        names = c("Não","Sim"))
text(1.5, 5, "Valor-p = 0,040", cex=1.0)
dev.off()

### ==================================
### Correlações entre os indicadores
### ==================================

############################## Parte 1 ###################################

### Indicadores - sem os indicadores reduzidos

Data.ind <- cbind(Dados.dom$Percentil.CD, 
                  Dados.dom$Percentil.PD, 
                  Dados.dom$Soma.LS,
                  Dados.dom$Percentil.estresse.vida,
                  Dados.qvfi3$qvfi3.6, 
                  Dados.qvfi7$qvfi7.6)
correlação.ind <- rcorr(Data.ind, type = "spearman")
matrix.ind <- correlação.ind$r
correlação.ind$P
colnames(matrix.ind) <- c("Domínio\nda criança",
                          "Domínio\ndos pais",
                          "Domínio do\nestresse da vida",
                          "Percentil\n estresse da vida",
                          "QFVI-3",
                          "QFVI-7")
rownames(matrix.ind) <- c("Domínio\nda criança",
                          "Domínio\ndos pais",
                          "Domínio do\nestresse da vida",
                          "Percentil\n estresse da vida",
                          "QFVI-3",
                          "QFVI-7")

# Tabela 9 - correlação entre os indicadores
matrix.ind

### Questionário socioeconômico

Data.socio <- cbind(Dados.dom$Percentil.CD, 
                    Dados.dom$Percentil.PD, 
                    Dados.dom$Soma.LS,
                    Dados.dom$Percentil.estresse.vida,
                    Dados.qvfi3$qvfi3.6, 
                    Dados.qvfi7$qvfi7.6,
                    Dados.socio$IDADE.CRIANÇA,
                    Dados.socio$MINUTOS.AT..FISICA,
                    Dados.socio$FREQ..AT..FÍSICA,
                    Dados.socio$PERIODO.AT..LAZER.2,
                    Dados.socio$PESO.DA.MÃE..Kg.,
                    Dados.socio$ALTURA.MÃE.cm.,
                    Dados.socio$IMC.MÃE,
                    Dados.socio$IDADE..MÃE,
                    Dados.socio$CARGA.HORÁRIA,
                    Dados.socio$RENDA.FAMILIAR.2,
                    Dados.socio$QTDE.PESO,
                    Dados.socio$TEMPO.PESO,
                    Dados.socio$APETITE)
correlação.socio <- rcorr(Data.socio, type = "spearman")
matrix.cor <- correlação.socio$r[c(7:19), c(1:6)]
correlação.socio$P[c(7:19), c(1:6)]
rownames(matrix.cor) <- c("Idade da criança",
                          "Tempo de\natividade física",
                          "Frequência da\natividade física",
                          "Atividade de lazer",
                          "Peso da mãe",
                          "Altura da mãe",
                          "IMC da mãe",
                          "Idade da mãe",
                          "Carga horária",
                          "Renda familiar",
                          "Variação de peso",
                          "Tempo da\nvariação de peso",
                          "Apetite")
colnames(matrix.cor) <- c("Domínio\nda criança",
                          "Domínio\ndos pais",
                          "Domínio do\nestresse da vida",
                          "Percentil\n estresse da vida",
                          "QFVI-3",
                          "QFVI-7")
matrix.p.socio <- format(round(correlação.socio$P[c(7:19), c(1:6)],2), nsmall = 3)

# Tabela 15 - correlação dos indicadores com as variáveis
matrix.cor

### Tabela - correlação solicitação 08/08/08

Data.socio.08 <- data.frame(GERAL.STRESS = Dados.dom$GERAL.STRESS, 
                            POSTURA.DEFESA = Dados.dom$POSTURA.DEFESA, 
                            Soma.LS = Dados.dom$Soma.LS, 
                            qvfi3.6 = Dados.qvfi3$qvfi3.6, 
                            qvfi7.6 = Dados.qvfi7$qvfi7.6,
                            IDADE.CRIANÇA = Dados.socio$IDADE.CRIANÇA,
                            MINUTOS.AT..FISICA = Dados.socio$MINUTOS.AT..FISICA,
                            FREQ..AT..FÍSICA = Dados.socio$FREQ..AT..FÍSICA,
                            PERIODO.AT..LAZER.2 = Dados.socio$PERIODO.AT..LAZER.2,
                            PESO.DA.MÃE..Kg. = Dados.socio$PESO.DA.MÃE..Kg.,
                            ALTURA.MÃE.cm. = Dados.socio$ALTURA.MÃE.cm.,
                            IMC.MÃE = Dados.socio$IMC.MÃE,
                            IDADE..MÃE = Dados.socio$IDADE..MÃE,
                            CARGA.HORÁRIA = Dados.socio$CARGA.HORÁRIA,
                            RENDA.FAMILIAR.2 = Dados.socio$RENDA.FAMILIAR.2,
                            QTDE.PESO = Dados.socio$QTDE.PESO,
                            TEMPO.PESO = Dados.socio$TEMPO.PESO,
                            APETITE = Dados.socio$APETITE)

colnames(Data.socio.08) <- c("GERAL.STRESS", 
                             "POSTURA.DEFESA", 
                             "Soma.LS", 
                             "qvfi3.6", 
                             "qvfi7.6",
                             "IDADE.CRIANÇA",
                             "MINUTOS.AT..FISICA",
                             "FREQ..AT..FÍSICA",
                             "PERIODO.AT..LAZER.2",
                             "PESO.DA.MÃE..Kg.",
                             "ALTURA.MÃE.cm.",
                             "IMC.MÃE",
                             "IDADE..MÃE",
                             "CARGA.HORÁRIA",
                             "RENDA.FAMILIAR.2",
                             "QTDE.PESO",
                             "TEMPO.PESO",
                             "APETITE")

rcorr(as.matrix(Data.socio.08), type = "spearman")


############################### Parte 2 ################################

### Indicadores - com os indicadores reduzidos

Data.ind.2 <- cbind(IND_CRIANÇA,
                    IND_PAIS,
                    Dados.dom$Percentil.CD, 
                    Dados.dom$Percentil.PD)
correlação.ind.2 <- rcorr(Data.ind.2, type = "spearman")
matrix.ind.2 <- correlação.ind.2$r[c(3,4),c(1,2)]
correlação.ind.2$P[c(3,4),c(1,2)]
colnames(matrix.ind.2) <- c("Domínio da\n criança reduzido", "Domínio dos\npais reduzido")
rownames(matrix.ind.2) <- c("Domínio\nda criança", "Domínio\ndos pais")

# Tabela 25 - correlação dos indicadores completos e reduzidos
matrix.ind.2

### Questionário socioeconômico e demais indicadores

Data.socio.2 <- cbind(IND_CRIANÇA, 
                      IND_PAIS, 
                      Dados.socio$IDADE.CRIANÇA,
                      Dados.socio$MINUTOS.AT..FISICA,
                      Dados.socio$FREQ..AT..FÍSICA,
                      Dados.socio$PERIODO.AT..LAZER.2,
                      Dados.socio$PESO.DA.MÃE..Kg.,
                      Dados.socio$ALTURA.MÃE.cm.,
                      Dados.socio$IMC.MÃE,
                      Dados.socio$IDADE..MÃE,
                      Dados.socio$CARGA.HORÁRIA,
                      Dados.socio$RENDA.FAMILIAR.2,
                      Dados.socio$QTDE.PESO,
                      Dados.socio$TEMPO.PESO,
                      Dados.socio$APETITE,
                      Dados.dom$Soma.LS, 
                      Dados.qvfi3$qvfi3.6, 
                      Dados.qvfi7$qvfi7.6)
correlação.socio.2 <- rcorr(Data.socio.2, type = "spearman")
matrix.cor.2 <- correlação.socio.2$r[c(3:18), c(1:2)]
correlação.socio.2$P[c(3:18), c(1:2)]
rownames(matrix.cor.2) <- c("Idade da criança",
                            "Tempo de\natividade física",
                            "Frequência da\natividade física",
                            "Atividade de lazer",
                            "Peso da mãe",
                            "Altura da mãe",
                            "IMC da mãe",
                            "Idade da mãe",
                            "Carga horária",
                            "Renda familiar",
                            "Variação de peso",
                            "Tempo da\nvariação de peso",
                            "Apetite",
                            "Domínio do\nestresse da vida",
                            "QVFI-3",
                            "QVFI-7")
colnames(matrix.cor.2) <- c("Domínio da\n criança reduzido", "Domínio dos\npais reduzido")

# Tabela 28 - correlação dos indicadores com as variáveis e demais indicadores
matrix.cor.2

### ==============================
### Gráficos - Corrplot (Parte 1)
### ==============================

# Gráfico 11 - Indicadores

svg("Corr1.svg")
corrplot(matrix.ind,  method = "color", type = "lower", tl.col = "black", tl.srt = 45, tl.cex = 0.8)
dev.off()

# Gráfico 17 - Variáveis do questionário socioeconômico

svg("Corr2.svg")
corrplot(matrix.cor, method = "color", tl.col = "black", tl.srt = 45, tl.cex = 0.8)
dev.off()

### ================================
### Gráficos de Regressão (Parte 2)
### ================================

# Gráfico 22 - Domínio da criança

reg1 <- lm(IND_CRIANÇA ~ Dados.dom$Percentil.CD)
summary(reg1)

svg("Regressao1.svg", width = 6, height = 5)
plot(Dados.dom$Percentil.CD, IND_CRIANÇA, main = "Domínio da criança", 
     xlab = "Indicador completo", ylab = "Indicador reduzido", type = "p",
     xlim = c(0,100), ylim = c(1,5), pch=16, col = "skyblue4")
abline(reg1,lty=1)
text(15, 4.5, "R² = 0,7019", cex = 0.8)
dev.off()

# Gráfico 23 - Domínio dos pais

reg2 <- lm(IND_PAIS ~ Dados.dom$Percentil.PD)
summary(reg2)

svg("Regressao2.svg", width = 6, height = 5)
plot(Dados.dom$Percentil.PD, IND_PAIS, main = "Domínio dos pais", 
     xlab = "Indicador completo", ylab = "Indicador reduzido", type = "p",
     xlim = c(0,100), ylim = c(1,5), pch=16, col = "skyblue4")
abline(reg2,lty=1)
text(15, 4.5, "R² = 0,6616", cex = 0.8)
dev.off()

### ================================
### Anexo - Constructo de 1ª ordem
### ================================

# Tabela 29 - Validação 1ª ordem
rbind(
### Domínio da criança

rbind(Val(DI),
      Val(RE),
      Val(MO),
      Val(AC),
      Val(AD),
      Val(DE)),

### Domínio dos pais

rbind(Val(CO),
      Val(AT),
      Val(RO),
      Val(DP),
      Val(SP),
      Val(IS),
      Val(HE)),

### Domínio do estresse de vida

Val(LS),

### QVFI-3

rbind(Val1(Saude.geral.da.visao.3),
      Val(Impacto.familiar.3),
      Val1(Personalidade.3),
      Val(Competência.3),
      Val(Tratamento.3)),

### QVFI-7

rbind(Val1(Saude.geral.da.visao.7),
      Val(Impacto.familiar.7),
      Val(Personalidade.7),
      Val(Competência.7),
      Val(Tratamento.7))
)

### ================================
### Anexo - Constructo de 2ª ordem
### ================================

### Domínio da criança

dominio.criança <- data.frame(Dados.dom$Percentil.DI, Dados.dom$Percentil.RE, Dados.dom$Percentil.MO, 
                              Dados.dom$Percentil.AC, Dados.dom$Percentil.AD, Dados.dom$Percentil.DE, 
                              Dados.dom$Percentil.CD)

### Domínio dos pais

dominio.pais <- data.frame(Dados.dom$Percentil.CO, Dados.dom$Percentil.AT, Dados.dom$Percentil.RO, 
                           Dados.dom$Percentil.DP, Dados.dom$Percentil.SP, Dados.dom$Percentil.IS, 
                           Dados.dom$Percentil.HE, Dados.dom$Percentil.PD)

# Tabela 30 - Validação 2ª ordem
rbind(Val(dominio.criança), Val(dominio.pais), Val(LS), Val(Geral.3), Val1(Geral.7))

### ==================================================
### Análises Fatoriais completas - NÃO será mostrado
### ==================================================

### Domínio da criança 

# A questão 7 precisa ser invertida

Dados.dom$Q7.new <- ifelse(Dados.dom$Q7 == 1, 5,
                           ifelse(Dados.dom$Q7 == 2, 4,
                                  ifelse(Dados.dom$Q7 == 3, 3,
                                         ifelse(Dados.dom$Q7 == 4, 2, 1))))

DI2 <- cbind(Dados.dom$Q1, Dados.dom$Q2, Dados.dom$Q3, Dados.dom$Q4, Dados.dom$Q5, 
             Dados.dom$Q6, Dados.dom$Q7.new, Dados.dom$Q8, Dados.dom$Q9)

colnames(DI2) <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9")

rbind(fa1(DI2),
      fa1(RE),
      fa1(MO),
      fa1(AC),
      fa1(AD),
      fa1(DE))

## Modelo Final

# Remover as questões Q5, Q7, Q8 e Q2 do constructo DI

DI3 <- cbind(Dados.dom$Q1, Dados.dom$Q2, Dados.dom$Q3, Dados.dom$Q4, 
             Dados.dom$Q6, Dados.dom$Q7.new, Dados.dom$Q8, Dados.dom$Q9)

colnames(DI3) <- c("Q1", "Q2", "Q3", "Q4", "Q6", "Q7", "Q8", "Q9")

DI4 <- cbind(Dados.dom$Q1, Dados.dom$Q2, Dados.dom$Q3, Dados.dom$Q4, 
             Dados.dom$Q6, Dados.dom$Q8, Dados.dom$Q9)

colnames(DI4) <- c("Q1", "Q2", "Q3", "Q4", "Q6", "Q8", "Q9")

DI5 <- cbind(Dados.dom$Q1, Dados.dom$Q2, Dados.dom$Q3, Dados.dom$Q4, 
             Dados.dom$Q8, Dados.dom$Q9)

colnames(DI5) <- c("Q1", "Q2", "Q3", "Q4", "Q8", "Q9")

DI6 <- cbind(Dados.dom$Q1, Dados.dom$Q2, Dados.dom$Q3, Dados.dom$Q4, Dados.dom$Q9)

colnames(DI6) <- c("Q1", "Q2", "Q3", "Q4", "Q9")

DI7 <- cbind(Dados.dom$Q1, Dados.dom$Q3, Dados.dom$Q4, Dados.dom$Q9)

colnames(DI7) <- c("Q1", "Q3", "Q4", "Q9")

# Remover a questão Q11 do constructo RE

RE2 <- cbind(Dados.dom$Q10, Dados.dom$Q12, Dados.dom$Q13, Dados.dom$Q14, Dados.dom$Q15)

colnames(RE2) <- c("Q10", "Q12", "Q13", "Q14", "Q15")

# Remover a questão Q16 do constructo MO

MO2 <- cbind(Dados.dom$Q17, Dados.dom$Q18, Dados.dom$Q19, Dados.dom$Q20)

colnames(MO2) <- c("Q17", "Q18", "Q19", "Q20")

# Remover as questões Q35, Q31, Q36, Q40 e Q41 do constructo AD

AD2 <- cbind(Dados.dom$Q31, Dados.dom$Q32, Dados.dom$Q33, Dados.dom$Q34, Dados.dom$Q36,
             Dados.dom$Q37, Dados.dom$Q38, Dados.dom$Q39, Dados.dom$Q40, Dados.dom$Q41)

colnames(AD2) <- c("Q31", "Q32", "Q33", "Q34", "Q36", "Q37", "Q38", "Q39", "Q40", "Q41")

AD3 <- cbind(Dados.dom$Q32, Dados.dom$Q33, Dados.dom$Q34, Dados.dom$Q36,
             Dados.dom$Q37, Dados.dom$Q38, Dados.dom$Q39, Dados.dom$Q40, Dados.dom$Q41)

colnames(AD3) <- c("Q32", "Q33", "Q34", "Q36", "Q37", "Q38", "Q39", "Q40", "Q41")

AD4 <- cbind(Dados.dom$Q32, Dados.dom$Q33, Dados.dom$Q34,
             Dados.dom$Q37, Dados.dom$Q38, Dados.dom$Q39, Dados.dom$Q40, Dados.dom$Q41)

colnames(AD4) <- c("Q32", "Q33", "Q34", "Q37", "Q38", "Q39", "Q40", "Q41")

AD5 <- cbind(Dados.dom$Q32, Dados.dom$Q33, Dados.dom$Q34,
             Dados.dom$Q37, Dados.dom$Q38, Dados.dom$Q39, Dados.dom$Q41)

colnames(AD5) <- c("Q32", "Q33", "Q34", "Q37", "Q38", "Q39", "Q41")

AD6 <- cbind(Dados.dom$Q32, Dados.dom$Q33, Dados.dom$Q34,
             Dados.dom$Q37, Dados.dom$Q38, Dados.dom$Q39)

colnames(AD6) <- c("Q32", "Q33", "Q34", "Q37", "Q38", "Q39")

# Remover as questões Q45, Q48 e Q42 do constructo DE

DE2 <- cbind(Dados.dom$Q42, Dados.dom$Q43, Dados.dom$Q44, Dados.dom$Q46,
             Dados.dom$Q47, Dados.dom$Q48, Dados.dom$Q49, Dados.dom$Q50)

colnames(DE2) <- c("Q42", "Q43", "Q44", "Q46", "Q47", "Q48", "Q49", "Q50")

DE3 <- cbind(Dados.dom$Q42, Dados.dom$Q43, Dados.dom$Q44, Dados.dom$Q46,
             Dados.dom$Q47, Dados.dom$Q49, Dados.dom$Q50)

colnames(DE3) <- c("Q42", "Q43", "Q44", "Q46", "Q47", "Q49", "Q50")

DE4 <- cbind(Dados.dom$Q43, Dados.dom$Q44, Dados.dom$Q46,
             Dados.dom$Q47, Dados.dom$Q49, Dados.dom$Q50)

colnames(DE4) <- c("Q43", "Q44", "Q46", "Q47", "Q49", "Q50")

rbind(fa1(DI7),
      fa1(RE2),
      fa1(MO2),
      fa1(AC),
      fa1(AD6),
      fa1(DE4))

rbind(Val(DI7),
      Val(RE2),
      Val(MO2),
      Val(AC),
      Val(AD6),
      Val(DE4))

### Domínio dos pais

## Modelo Inicial

CO <- cbind(Dados.dom[,52:64])

# As questões 54, 59 e 60 precisam ser invertidas

Dados.dom$Q54.new <- ifelse(Dados.dom$Q54 == 5, 1,
                            ifelse(Dados.dom$Q54 == 4, 2,
                                   ifelse(Dados.dom$Q54 == 3, 3,
                                          ifelse(Dados.dom$Q54 == 2, 4, 5))))

Dados.dom$Q59.new <- ifelse(Dados.dom$Q59 == 5, 1,
                            ifelse(Dados.dom$Q59 == 4, 2,
                                   ifelse(Dados.dom$Q59 == 3, 3,
                                          ifelse(Dados.dom$Q59 == 2, 4, 5))))

Dados.dom$Q60.new <- ifelse(Dados.dom$Q60 == 5, 1,
                            ifelse(Dados.dom$Q60 == 4, 2,
                                   ifelse(Dados.dom$Q60 == 3, 3,
                                          ifelse(Dados.dom$Q60 == 2, 4, 5))))

CO2 <- cbind(Dados.dom$Q28, Dados.dom$Q29, Dados.dom$Q30, Dados.dom$Q51, Dados.dom$Q52,
             Dados.dom$Q53, Dados.dom$Q54.new, Dados.dom$Q55, Dados.dom$Q56, Dados.dom$Q57,
             Dados.dom$Q58, Dados.dom$Q59.new, Dados.dom$Q60.new)

colnames(CO2) <- c("Q28", "Q29", "Q30", "Q51", "Q52", "Q53", "Q54", "Q55", 
                   "Q56", "Q57", "Q58", "Q59", "Q60")

rbind(fa1(CO2),
      fa1(AT),
      fa1(RO),
      fa1(DP),
      fa1(SP),
      fa1(IS),
      fa1(HE)
)

# Remover as questões Q59, Q54, Q60 e Q53 do constructo CO

CO3 <- cbind(Dados.dom$Q28, Dados.dom$Q29, Dados.dom$Q30, Dados.dom$Q51, Dados.dom$Q52,
             Dados.dom$Q53, Dados.dom$Q54.new, Dados.dom$Q55, Dados.dom$Q56, Dados.dom$Q57,
             Dados.dom$Q58, Dados.dom$Q60.new)

colnames(CO3) <- c("Q28", "Q29", "Q30", "Q51", "Q52", "Q53", "Q54", "Q55", 
                   "Q56", "Q57", "Q58", "Q60")

CO4 <- cbind(Dados.dom$Q28, Dados.dom$Q29, Dados.dom$Q30, Dados.dom$Q51, Dados.dom$Q52,
             Dados.dom$Q53, Dados.dom$Q55, Dados.dom$Q56, Dados.dom$Q57,
             Dados.dom$Q58, Dados.dom$Q60.new)

colnames(CO4) <- c("Q28", "Q29", "Q30", "Q51", "Q52", "Q53", "Q55", 
                   "Q56", "Q57", "Q58", "Q60")

CO5 <- cbind(Dados.dom$Q28, Dados.dom$Q29, Dados.dom$Q30, Dados.dom$Q51, Dados.dom$Q52,
             Dados.dom$Q53, Dados.dom$Q55, Dados.dom$Q56, Dados.dom$Q57,
             Dados.dom$Q58)

colnames(CO5) <- c("Q28", "Q29", "Q30", "Q51", "Q52", "Q53", "Q55", 
                   "Q56", "Q57", "Q58")

CO6 <- cbind(Dados.dom$Q28, Dados.dom$Q29, Dados.dom$Q30, Dados.dom$Q51, Dados.dom$Q52,
             Dados.dom$Q55, Dados.dom$Q56, Dados.dom$Q57, Dados.dom$Q58)

colnames(CO6) <- c("Q28", "Q29", "Q30", "Q51", "Q52", "Q55", 
                   "Q56", "Q57", "Q58")

# Remover as questões Q61 do constructo AT

AT2 <- cbind(Dados.dom$Q62, Dados.dom$Q63, Dados.dom$Q64, Dados.dom$Q65, Dados.dom$Q66,Dados.dom$Q67)

colnames(AT2) <- c("Q62", "Q63", "Q64", "Q65", "Q66", "Q67")

# Remover as questões Q80 do constructo DP

DP2 <- cbind(Dados.dom$Q75, Dados.dom$Q76, Dados.dom$Q77, Dados.dom$Q78, Dados.dom$Q79,Dados.dom$Q81,
             Dados.dom$Q82, Dados.dom$Q83)

colnames(DP2) <- c("Q75", "Q76", "Q77", "Q78", "Q79", "Q81", "Q82", "Q83")

rbind(fa1(CO6),
      fa1(AT2),
      fa1(RO),
      fa1(DP2),
      fa1(SP),
      fa1(IS),
      fa1(HE)
)

rbind(Val(CO6),
      Val(AT2),
      Val(RO),
      Val(DP2),
      Val(SP),
      Val(IS),
      Val(HE)
)

### QVFI-3

# Arrumando as variáveis

Dados.qvfi3$Questao4 <- ifelse(Dados.qvfi3$Q4 == 0, 100,
                               ifelse(Dados.qvfi3$Q4 == 25, 75, 25))
Dados.qvfi3$Questao5 <- ifelse(Dados.qvfi3$Q5 == 0, 100,
                               ifelse(Dados.qvfi3$Q5 == 25, 75,
                                      ifelse(Dados.qvfi3$Q5 == 50, 50,
                                             ifelse(Dados.qvfi3$Q5 == 75, 25, 0))))
Dados.qvfi3$Questao7 <- ifelse(Dados.qvfi3$Q7 == 0, 100,
                               ifelse(Dados.qvfi3$Q7 == 25, 75,
                                      ifelse(Dados.qvfi3$Q7 == 50, 50, 0)))

Impacto.familiar.3.2 <- cbind(Dados.qvfi3$Questao4, Dados.qvfi3$Questao5, Dados.qvfi3$Q6, Dados.qvfi3$Questao7,
                              Dados.qvfi3$Q8, Dados.qvfi3$Questao9, Dados.qvfi3$Q10, Dados.qvfi3$Questao12)
Impacto.familiar.3.3 <- cbind(Dados.qvfi3$Questao4, Dados.qvfi3$Questao5, Dados.qvfi3$Q6,
                              Dados.qvfi3$Q8, Dados.qvfi3$Questao9, Dados.qvfi3$Q10, Dados.qvfi3$Questao12)
Impacto.familiar.3.4 <- cbind(Dados.qvfi3$Questao5, Dados.qvfi3$Q6,
                              Dados.qvfi3$Q8, Dados.qvfi3$Questao9, Dados.qvfi3$Q10, Dados.qvfi3$Questao12)


Dados.qvfi3$Questao11 <- ifelse(Dados.qvfi3$Q11 == 0, 100,
                                ifelse(Dados.qvfi3$Q11 == 25, 75,
                                       ifelse(Dados.qvfi3$Q11 == 50, 50,
                                              ifelse(Dados.qvfi3$Q11 == 75, 25, 0))))

Personalidade.3.2 <- cbind(Dados.qvfi3$Questao11, Dados.qvfi3$Q14, Dados.qvfi3$Questao22, Dados.qvfi3$Questao24,
                           Dados.qvfi3$Questao25)
Personalidade.3.3 <- cbind(Dados.qvfi3$Questao11, Dados.qvfi3$Questao22, Dados.qvfi3$Questao24,
                           Dados.qvfi3$Questao25)
Personalidade.3.4 <- cbind(Dados.qvfi3$Questao22, Dados.qvfi3$Questao24,
                           Dados.qvfi3$Questao25)

## Modelo Inicial

rbind(fa1(Saude.geral.da.visao.3),
      fa1(Impacto.familiar.3.2),
      fa1(Personalidade.3.2),
      fa1(Competência.3),
      fa1(Tratamento.3))

## Modelo Final

rbind(fa1(Saude.geral.da.visao.3),
      fa1(Impacto.familiar.3.4),
      fa1(Personalidade.3.4),
      fa1(Competência.3),
      fa1(Tratamento.3))

rbind(Val1(Saude.geral.da.visao.3),
      Val(Impacto.familiar.3.4),
      Val(Personalidade.3.4),
      Val(Competência.3),
      Val(Tratamento.3))

### QVFI-7

# Arrumando as variáveis

Dados.qvfi7$Questao5.2 <- ifelse(Dados.qvfi7$Questao5 == 0, 100,
                                 ifelse(Dados.qvfi7$Questao5 == 25, 75,
                                        ifelse(Dados.qvfi7$Questao5 == 50, 50,
                                               ifelse(Dados.qvfi7$Questao5 == 62.5, 37.5,
                                                      ifelse(Dados.qvfi7$Questao5 == 75, 25, 0)))))
Dados.qvfi7$Questao6 <- ifelse(Dados.qvfi7$Q6 == 0, 100,
                               ifelse(Dados.qvfi7$Q6 == 25, 75,
                                      ifelse(Dados.qvfi7$Q6 == 50, 50,
                                             ifelse(Dados.qvfi7$Q6 == 75, 25, 0))))

Impacto.familiar.7.2 <- data.frame(Dados.qvfi7$Q4, Dados.qvfi7$Questao5.2, Dados.qvfi7$Questao6, Dados.qvfi7$Q7,
                                   Dados.qvfi7$Q8, Dados.qvfi7$Q10, Dados.qvfi7$Q11)
Impacto.familiar.7.3 <- data.frame(Dados.qvfi7$Questao5.2, Dados.qvfi7$Questao6, Dados.qvfi7$Q7,
                                   Dados.qvfi7$Q8, Dados.qvfi7$Q10, Dados.qvfi7$Q11)

Dados.qvfi7$Questao34.2 <- ifelse(Dados.qvfi7$Questao34 == 0, 100,
                                  ifelse(Dados.qvfi7$Questao34 == 25, 75,
                                         ifelse(Dados.qvfi7$Questao34 == mean2(Dados.qvfi7$Q34), (100 - mean2(Dados.qvfi7$Q34)),
                                                ifelse(Dados.qvfi7$Questao34 == 50, 50,
                                                       ifelse(Dados.qvfi7$Questao34 == 75, 25, 0)))))

Competência.7.2 <- data.frame(Dados.qvfi7$Q15, Dados.qvfi7$Questao16, Dados.qvfi7$Questao17, Dados.qvfi7$Questao18,
                              Dados.qvfi7$Questao19, Dados.qvfi7$Questao20, Dados.qvfi7$Questao21, Dados.qvfi7$Questao22,
                              Dados.qvfi7$Questao23, Dados.qvfi7$Questao24, Dados.qvfi7$Questao25, Dados.qvfi7$Questao27, 
                              Dados.qvfi7$Q28, Dados.qvfi7$Questao33, Dados.qvfi7$Questao34.2)

## Modelo Inicial

rbind(fa1(Saude.geral.da.visao.7),
      fa1(Impacto.familiar.7.2),
      fa1(Personalidade.7),
      fa1(Competência.7.2),
      fa1(Tratamento.7))

## Modelo Final

rbind(fa1(Saude.geral.da.visao.7),
      fa1(Impacto.familiar.7.3),
      fa1(Personalidade.7),
      fa1(Competência.7.2),
      fa1(Tratamento.7))

rbind(Val1(Saude.geral.da.visao.7),
      Val(Impacto.familiar.7.3),
      Val(Personalidade.7),
      Val(Competência.7.2),
      Val(Tratamento.7))







