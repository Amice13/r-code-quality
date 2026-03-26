
###########################IMPORTAÇÃO DOS BANCOS ##################################################
library(haven)# Ativa pacote de importação
LAPOP2017 <- read_dta("~/OneDrive/Documentos/2017/Tolerância/Tolerância e Participação/
                      Análises Tolerância e Participação/LAPOP2017.dta")
View(LAPOP2017)

library(labelled)# Ativa pacote para retirada dos rótulos
val_labels(LAPOP2017$bragrup1) <- NULL#retira rótulos dos valores para uso do recode
val_labels(LAPOP2017$bragrup2) <- NULL#retira rótulos dos valores para uso do recode
val_labels(LAPOP2017$bragrup3) <- NULL#retira rótulos dos valores para uso do recode
val_labels(LAPOP2017$bragrup4) <- NULL#retira rótulos dos valores para uso do recode
val_labels(LAPOP2017$bragrup5) <- NULL#retira rótulos dos valores para uso do recode

library(memisc)# Ativa pacote para recodificações

LAPOP2017$g1I <- recode(LAPOP2017$bragrup1, 1 <- c(10), 2 <- c(9), 
                             3 <- c(8), 4 <- c(7), 5 <- c(6), 6 <- c(5), 7 <- c(4), 
                             8 <- c(3), 9 <- c(2), 10 <- c(1))#inversão dos valores para uso da 
                            #função MAX, pois na escala original o 10 significa ˜gosto muito˜. 
                            #Para aplicar a função MAX preciso que 10 seja igual a desgosto muito.
                            #No nome G1I, o I significa invertido.

LAPOP2017$g2I <- recode(LAPOP2017$bragrup2, 1 <- c(10), 2 <- c(9), 
                             3 <- c(8), 4 <- c(7), 5 <- c(6), 6 <- c(5), 7 <- c(4), 
                             8 <- c(3), 9 <- c(2), 10 <- c(1)) 

LAPOP2017$g3I <- recode(LAPOP2017$bragrup3, 1 <- c(10), 2 <- c(9), 
                             3 <- c(8), 4 <- c(7), 5 <- c(6), 6 <- c(5), 7 <- c(4), 
                             8 <- c(3), 9 <- c(2), 10 <- c(1)) 

LAPOP2017$g4I <- recode(LAPOP2017$bragrup4, 1 <- c(10), 2 <- c(9), 
                             3 <- c(8), 4 <- c(7), 5 <- c(6), 6 <- c(5), 7 <- c(4), 
                             8 <- c(3), 9 <- c(2), 10 <- c(1)) 

LAPOP2017$g5I <- recode(LAPOP2017$bragrup5, 1 <- c(10), 2 <- c(9), 
                             3 <- c(8), 4 <- c(7), 5 <- c(6), 6 <- c(5), 7 <- c(4), 
                             8 <- c(3), 9 <- c(2), 10 <- c(1)) 

install.packages("data.table")#Pacote para programar a identificação da maior pontuação de desafeição.
library(data.table)# Ativa o pacote.
TP <- data.table(LAPOP2017)
TP[, MAX := colnames(.SD)[max.col(.SD, ties.method="random")], 
   .SDcols = c("g1I", "g2I", "g3I", "g4I", "g5I")] #essas linhas criam uma variável indicando qual 
                                #dos grupos registra a maior pontuação.
                                #como a escala foi invertida, isso significa o "menos gostado". 
                                #O método de resolução dos empates foi o "aleatório", então é feita a distribuição
                                #randomica entre os grupos sempre que um entrevistado deu a mesma nota para dois 
                                #ou mais grupos de referência. 

save(TP, file = "TP.RData")

#Dicotomização as três variáveis de apoio aos direitos
TP$VotoDic <- cut(TP$brad8, c(0, 6.62, 10), labels = c("Intolerante", "Tolerante"))
TP$DiscDic <- cut(TP$brad9, c(0, 6.14, 10), labels = c("Intolerante", "Tolerante"))
TP$CargoDic <- cut(TP$brad10, c(0, 5.79, 10), labels = c("Intolerante", "Tolerante"))

#Teste de consistência interna das três medidas de tolerância
install.packages("psy") # Pacote para calculo do alpha
library(psy)
cronbach(TP[,92:94]) # = 0.85, ou seja, forte consistência interna. 

#Índice de tolerância
TP$IT30 <- TP$brad8 + TP$brad9 + TP$brad10

#Padronização para 0-10
TP$IT1 <- TP$IT / max(TP$IT, na.rm = TRUE)# recodifica para 0-1

#Padronização para 0-10
TP$IT10 <- TP$IT1*10

#Medida dicotômica com corte pela média
TP$ITdic <- cut(TP$IT10, c(0, 6.19, 10), labels = c("Intolerante", "Tolerante"))

TP$LL <- factor(TP$MAX,
                    levels = c("g1I","g2I", "g3I", "g4I", "g5I"),
                    labels = c("Pessoas que defendem a legalização do aborto", 
                               "Pessoas que defendem o regime militar", "Comunistas", "PTistas", "PSDBistas"))
Table(TP$LL, percent=TRUE)

save(TP, file = "TP.RData")

#Criação das medidas dicotômicas sobre tolerância a cada direito
TP$DiscDic <- cut(TP$brad9, c(0, 6.14, 10), labels = c("Intolerante", "Tolerante"))
TP$CargoDic <- cut(TP$brad10, c(0, 5.79, 10), labels = c("Intolerante", "Tolerante"))
TP$VotoDic <- cut(TP$brad8, c(0, 6.62, 10), labels = c("Intolerante", "Tolerante"))
TP$ITdic <- cut(TP$IT10, c(0, 6.19, 10), labels = c("Intolerante", "Tolerante"))

# PARTICIPAÇÃO EM PROTESTO
library(memisc)
library(descr)
library(labelled)

#Manifestação ou protesto nos últimos 12 meses.(Prot3){1=sim, 2=não}
val_labels(TP$prot3) <- NULL#retira rótulos dos valores para uso do recode
TP$Prot <- recode(TP$prot3, 1 <- c(1), 0 <- c(2))
Table(TP$Prot, percent=TRUE)
               
#Controles para a regressões
#Recursos - Escolaridade
val_labels(TP$ed2) <- NULL#retira rótulos dos valores para uso do recode
TP$EdSup <- recode(TP$ed2, 1 <- c(8), 0 <- c(1,2,3,4,5,6,7))

#Engajamento Psicológico - Interesse por política
val_labels(TP$pol1) <- NULL#retira rótulos dos valores para uso do recode
TP$Int <- recode(TP$pol1, 1 <- c(4), 2 <- c(3), 3 <- c(2), 4 <- c(1))

#Rede de MObilização - Envolvimento em associações
TP$Assoc <- (TP$AssocPMDic + TP$AssocMorDic + TP$OrgRelDic)

#Adesão à democracia
TP$Dem <- TP$ing4
val_labels(TP$Dem) <- NULL
TP$Dem <- recode(TP$Dem, 0 <- c(1,2,3,4), 1 <- c(5,6,7))

#Apoio Participação
TP$ApoioPart <- TP$e5

#Idade
TP$FxId <- cut(TP$q2, c(1,30,50,110))
summary(TP$FxId)

#Ideologia
TP$Ideol <- TP$l1

save(TP, file = "TP.RData")

######Regressão
#Protesto
ModProt <- glm(Prot~ IT10 + ed2 + Int + Assoc + Dem, data = TP, family = binomial(link = "logit"))
summary(ModProt)
exp(coefficients(ModProt))


############ Coarsened Exact Matching########
## Criação do banco
TPprot <- TP[,c("Prot", "ITdic", "ed2", "Int", "Assoc", "Dem", "FxId")]

#Instalação e ativação do pacote CEM
install.packages("cem")
library(cem)

#Modelo para Protesto
TPprot.cem <- data.frame(na.omit(TPprot))#Cria o banco TPprot.cem com a retirada de todos os NA (missing)
mat.prot.cem <- cem(treatment = "ITdic", data = TPprot.cem, drop = "Prot")#Cria os grupos de controle e 
#tratamento a partir da condição de tolerante/intolerante
mat.prot.cem#exibe o resultado desse matching
          
TPprot.cem$ITdic <- as.numeric(TPprot.cem$ITdic)#conversão da variável ITdic em numérica

est.prot <- att(mat.prot.cem, Prot ~ ITdic, data = TPprot.cem, model = "logit") # Estima os coeficientes 
#dos preditores do envolvimento 
#em protesto considerando o matching pela condição de tolerante/intolerante, sem controles
summary(est.prot)
est.prot.contr <- att(mat.prot.cem, Prot ~ ITdic + Int + Assoc, data = TPprot.cem, model = "logit")# Estima 
#os coeficientes dos preditores do envolvimento 
#em protesto considerando o matching pela condição de tolerante/intolerante, com controles
summary(est.prot.contr) 
est.prot.contr


