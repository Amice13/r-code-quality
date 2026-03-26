#TAMANHO MINIMO DA AMOSTRA
ifelse(!require(pwr),install.packages("pwr", dependencies=TRUE),1) # instalar pacote se necessario
require(pwr)
p.out<-pwr.f2.test(u = 3 , v = NULL, f2 = 0.3, sig.level = 0.05, power = 0.8)
numofcases<-round(p.out[["v"]]+p.out[["u"]]+1)
cat("Sample size needed:", numofcases)

#IMPORTAR DADOS
ifelse(!require(readxl),install.packages("readxl", dependencies=TRUE),1) # instalar pacote se necessario
require(readxl)
dataset <- read_excel("diret?rio") #incluir diretorio, por exemplo "C:/Users/Adonai/EJML/dataset.xlsx"
View(dataset)

# VERIFICAR SE HOUVE RESPONDENTES COM EXPERIENCIA ANTERIOR COM JOGOS DE EMPRESAS
table(dataset$EXPJE)

#ESTATÍSTICA DESCRITIVA
summary(dataset)

# MEDIA DA IDADE POR GENERO
tapply(dataset$IDADE, dataset$GENERO, mean)

## PLS-PM
#DIAGRAMA DE CAMINHO
SAT=c(0,0,0,0)
CONH=c(1,0,0,0)
HAB=c(1,0,0,0)
ATIT=c(1,0,0,0)

#MATRIZ DE CAMINHO
path_je=rbind(SAT,CONH,HAB,ATIT)

#PLOTAR MODELO
ifelse(!require(plspm),install.packages("plspm", dependencies=TRUE),1) # instalar pacote se necessario
require(plspm)
innerplot(path_je)

#RELACIONAR INDICADORES
blocks_je=list(13:15,4:6,7:8,9:12)

#DETERMINAR TIPO DE CONSTRUTO ("A" REFLEXIVO E "B' FORMATIVO)
modes_je=c("A","B","A", "A")

## 1 RODADA
#RODAR PLSPM
pls_je=plspm(dataset, path_je, blocks_je,modes= modes_je, scheme = "path", scaled = NULL, tol = 1e-07, maxiter = 300, boot.val=TRUE, br=5000)

#RESULTADOS
summary(pls_je)

## 2 RODADA (loading do indicador SAT2 < 0,4)
blocks_je_SAT2=list(c(13,15),4:6,7:8,9:12) # 2 rodada (excluindo-se a indicador SAT2)
pls_je_SAT2=plspm(dataset, path_je, blocks_je_SAT2,modes= modes_je, scheme = "path", scaled = NULL, tol = 1e-07, maxiter = 300, boot.val=TRUE, br=5000) # 2 rodada
summary(pls_je_SAT2) # 2 rodada

#RESULTADOS: CRITERIO DE FORNELL-LARCKER
sqrt(pls_je_SAT2$inner_summary$AVE)

#RESULTADOS: MULTICOLINEARIDADE (CONSTRUTO FORMATIVO)
scores=as.data.frame(pls_je_SAT2$scores) # criar dados dos scores das Variavies Latentes
rlmCONH=lm(scores$CONH~dataset$CONH1+dataset$CONH2+dataset$CONH3)
ifelse(!require(car),install.packages("car", dependencies=TRUE),1) # instalar pacote se necessario
require(car)
vif(rlmCONH)

#PLOTAR MODELOS
#ESTRUTURAL
plot(pls_je_SAT2)
#REFLEXIVO
plot(pls_je_SAT2, what = "loadings", arr.width = 0.1)
#FORMATIVO
plot(pls_je_SAT2, what = "weights", arr.width = 0.1)

#CRIAR GRUPOS DE CONTROLE (SEM DEBRIEFING) E EXPERIMENTAL (COM DEBRIEFING)
dataset$Grupo2<-ifelse(dataset$GRUPO==2,"controle","experimental")

#TRANSFORMAR VARIÁVEL Grupo2 EM  FATOR
dataset$Grupo2<-as.factor(dataset$Grupo2)

#RODAR ANÁLISE MULTIGRUPO
plsgroups_je=plspm.groups(pls_je_SAT2, dataset$Grupo2, method = "bootstrap", reps=5000)

#SUMARIZAR RESULTADOS  - ANÁLISE MULTIGRUPO
plsgroups_je
plsgroups_je$group1 # grupos de controle
plsgroups_je$group2 # grupos experimental

#GRÁFICO DOS COEFICIENTES DE CAMINHO
barplot(t(as.matrix(plsgroups_je$test[,2:3])), border = NA, beside = TRUE, col = c("#FEB24C","#74A9CF"), las = 2, ylim = c(-0.1, 1),cex.names = 0.8, col.axis = "gray30", cex.axis = 0.8)
abline(h = 0, col = "gray50")
title("Teste de hipótese - Jogo de Empresas COM e SEM Debriefing", cex.main = 0.95, col.main = "gray30")
legend("top", legend = c("G. Controle", "G. Experimental"), pt.bg = c("#FEB24C", "#A6BDDB"), ncol = 2, pch = 22, col = c("#FEB24C", "#74A9CF"), bty = "n", text.col = "gray40")