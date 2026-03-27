###################################################################################################
# Autor: Prof. Sandro de Azambuja, D.Sc.                                              #############
# MIBA 1672 - professor adjunto da UFF - Universidade Federal Fluminense              #############
# e-mail: sandroazambuja@id.uff.br                                                    #############
# código base para as análises estatísticas que embasaram o artigo intitulado:        #############
#                                                                                     #############
#   MAC: Uma Proposta para Metas Atuariais Consistentes em Fundos de Pensão           #############
#                                                                                     #############
###################################################################################################

rm(list=ls())
PATH = "C:\\Users\\windows 7\\Desktop\\_artigo 3"
setwd(PATH)
PATH
require(AER)
require(plm)
install.packages("plm")
install.packages("AER")
install.packages("tidyverse")
install.packages("readxl")
install.packages("car") #pacote de econometria aplicada com R
install.packages("nortest")
install.packages("pastecs")
install.packages("psych")
install.packages("FSA")
install.packages("gplots")

library(car)
library(readxl)
library(nortest)
library(corrplot)
library(Hmisc)
library(pastecs)
library(psych)
library(FSA)
library(plm)
library(foreign)
library(gplots)
library(ggplot2)
library(lmtest)
library(forecast)

#importando os dados
dadosEFPCOrig <- read_xlsx("Dados abertos artigo MAC.xlsx", sheet = 1)
View(dadosEFPCOrig)
head(dadosEFPCOrig)
tail(dadosEFPCOrig)
dim(dadosEFPCOrig)

#Matriz de correla????o
dadosEFPCOrig[24]
AnoFactor<-factor(dadosEFPCOrig[2])
dadosAnaliseCorrcomY <- data.frame(dadosEFPCOrig[3],dadosEFPCOrig[2],dadosEFPCOrig[5:12],dadosEFPCOrig[16:24])
View(dadosAnaliseCorrcomY)
CorrelacaoTodasVariaveis <- cor(dadosAnaliseCorrcomY)
write.csv2(CorrelacaoTodasVariaveis, "matrizCorrComY.csv")

#COMO EXISTEM ESCALAS DIFERENTES ENTRE AS VARI??VEIS NUM??RICAS se faz NECESS??RIO UMA TRANSFORMA????O NOS DADOS

summary(dadosEFPCOrig)
fivenum(dadosEFPCOrig$Provisoes)

summary(dadosEFPCOrig$Rentabilidade)
mean(dadosEFPCOrig$NumAssistidos)
median(dadosEFPCOrig$Rentabilidade)
min(dadosEFPCOrig$NumAssistidos)
max(dadosEFPCOrig$NumAssistidos)
sd(dadosEFPCOrig$NumAssistidos)
sd(dadosEFPCOrig$NumAssistidos)/mean(dadosEFPCOrig$NumAssistidos)

library(pastecs)
#stat.desc(dadosEFPCOrig$MetaAtuarial)
library(psych)
#describe(dadosEFPCOrig$MetaAtuarial)

str(dadosEFPCOrig)
names(dadosEFPCOrig)

dadosEFPC=pdata.frame(dadosEFPCOrig, index=c("EFPCPlano","Ano"))
??pdata.frame

summary(dadosEFPC$MetaAtuarial)

dadosEFPC$lnPatrimonioLiq <- (log(dadosEFPC$Patrimonio)-log(dadosEFPC$Provisoes))
summary(dadosEFPC$lnPatrimonioLiq)
mean(dadosEFPC$MetaAtuarial)
View(dadosEFPC)

#MODELO BASE - 09/02/2021

#apliquei log em algumas vari??veis para melhorar a escala
#formula1<-MetaAtuarial~Rentabilidade+log(Provisoes)+log(Patrimonio)+IS+lnPatrimonioLiq+TaxaParTMA+Duration+log(1+NumAtivos)+log(1+NumAssistidos)+propIntPatroc+propIntPartic+Ano+log(IBOV31dez)+log(IBOV02jan)+varptsIBOV12meses+log(IBOVMax)+propIntPatroc+propIntPartic+mediaNTNB2045+mediaNTNB2050+mediaNTNB2035+mediaNTNB2024

#TABELA 6 - dados originais, sem transforma????es, produz efeitos sem signific??ncia para as caracter??sticas EFPC-Plano
formula1<-MetaAtuarial~Provisoes+Patrimonio+IS+TaxaParTMA+Duration+NumAtivos+NumAssistidos+Rentabilidade+propIntPatroc+propIntPartic+Ano+IBOV31dez+IBOV02jan+varptsIBOV12meses+IBOVMax+mediaNTNB2045+mediaNTNB2050+mediaNTNB2035+mediaNTNB2024+RentFutPatr+raiz5avarIBOV

#Meta100<-100*dadosEFPC$MetaAtuarial
#formula1<-log(Meta100)~Provisoes+Patrimonio+IS+TaxaParTMA+Duration+NumAtivos+NumAssistidos+Rentabilidade+propIntPatroc+propIntPartic+Ano+IBOV31dez+IBOV02jan+varptsIBOV12meses+IBOVMax+mediaNTNB2045+mediaNTNB2050+mediaNTNB2035+mediaNTNB2024+RentFutPatr

# (0) no notepad - qdo between n??o h?? correla????o serial
reg.ef=plm(formula1, data=dadosEFPC, model="within")
summary(reg.ef)
bptest(reg.ef) #The null hypothesis for the Breusch-Pagan test is homoskedasticity
pbgtest(reg.ef)
shapiro.test(reg.ef$residuals) # os dados originais com between passam nos 3 testes: correla????o serial, normalidade e homocedasticidade
? summary()

#Cap??tulo 5
AtivAssist<-dadosEFPC$NumAtivos+dadosEFPC$NumAssistidos
#testes ok

#formula1<-MetaAtuarial~Rentabilidade+log(Provisoes)+log(Patrimonio)+IS+lnPatrimonioLiq+TaxaParTMA+Duration+log(AtivAssist)+log(1+NumAssistidos)+propIntPatroc+propIntPartic+Ano+log(IBOV31dez)+log(IBOV02jan)+varptsIBOV12meses+log(IBOVMax)+propIntPatroc+propIntPartic+mediaNTNB2045+mediaNTNB2050+mediaNTNB2035+mediaNTNB2024
#formula1<-MetaAtuarial~Rentabilidade+log(Provisoes)+log(Patrimonio)+IS+lnPatrimonioLiq+TaxaParTMA+log(1+NumAssistidos)+factor(Ano)+log(IBOV31dez)+log(IBOV02jan)+varptsIBOV12meses+log(IBOVMax)+propIntPatroc+propIntPartic+mediaNTNB2045+mediaNTNB2050+mediaNTNB2035+mediaNTNB2024
#formula1<-MetaAtuarial~Rentabilidade+log(Provisoes)+log(Patrimonio)+IS+lnPatrimonioLiq+TaxaParTMA+log(1+NumAssistidos)+factor(Ano)+log(IBOV31dez)+varptsIBOV12meses+log(IBOVMax)+mediaNTNB2045+mediaNTNB2050
#formula1<-MetaAtuarial~Rentabilidade+log(Provisoes)+log(Patrimonio)+IS+lnPatrimonioLiq+TaxaParTMA+log(1+NumAssistidos)+factor(Ano)+varptsIBOV12meses+mediaNTNB2045
#formula1<-MetaAtuarial~Rentabilidade+log(Provisoes)+IS+lnPatrimonioLiq+TaxaParTMA+log(1+NumAssistidos)+factor(Ano)+varptsIBOV12meses+mediaNTNB2045
#formula1<-MetaAtuarial~Rentabilidade+log(Patrimonio)+IS+lnPatrimonioLiq+TaxaParTMA+log(1+NumAssistidos)+factor(Ano)+varptsIBOV12meses+mediaNTNB2045
# bom esse abaixo
#formula1<-MetaAtuarial~Rentabilidade+log(Provisoes)+lnPatrimonioLiq+TaxaParTMA+log(1+NumAssistidos)+factor(Ano)+varptsIBOV12meses+mediaNTNB2045
#formula1<-MetaAtuarial~Rentabilidade+log(Provisoes)+lnPatrimonioLiq+TaxaParTMA+log(1+NumAssistidos)+mediaNTNB2045
#formula1<-MetaAtuarial~RentFutPatr+Rentabilidade+log(Provisoes)+lnPatrimonioLiq+TaxaParTMA+mediaNTNB2045
#formula1<-MetaAtuarial~RentFutPatr+Rentabilidade+log(Provisoes)+lnPatrimonioLiq+TaxaParTMA+log(1+NumAssistidos)+mediaNTNB2045

# usar esses dois agora
#servir??o para mostar que a hip??tese H1 est?? furada, e jsutificar?? alguns autores enquanto outros n??o
#formula1<-MetaAtuarial~Rentabilidade+log(Provisoes)+lnPatrimonioLiq+TaxaParTMA+log(NumAssistidos)
#formula1<-MetaAtuarial~RentFutPatr+Rentabilidade+log(Provisoes)+lnPatrimonioLiq+TaxaParTMA+NumAssistidos
#formula1<-MetaAtuarial~RentFutPatr+Rentabilidade+log(Provisoes)+lnPatrimonioLiq+TaxaParTMA+AtivAssist

formula1<-MetaAtuarial~RentFutPatr+Rentabilidade+log(Provisoes)+lnPatrimonioLiq+TaxaParTMA+log(NumAssistidos)
#formula1<-MetaAtuarial~RentFutPatr+lead(Rentabilidade,1)+log(Provisoes)+lnPatrimonioLiq+TaxaParTMA+log(NumAssistidos)
reg.ef=plm(formula1, data=dadosEFPC, model="between") #aqui deixa de ter correla????o serial
summary(reg.ef)
bptest(reg.ef) #The null hypothesis for the Breusch-Pagan test is homoskedasticity
pbgtest(reg.ef)
shapiro.test(reg.ef$residuals) # os dados originais com between passam nos 3 testes: correla????o serial, normalidade e homocedasticidade
formula1<-MetaAtuarial~Rentabilidade+lnPatrimonioLiq+TaxaParTMA+log(NumAssistidos)   #NumAssistidos sem signific??ncia
formula1<-MetaAtuarial~Rentabilidade+lnPatrimonioLiq+TaxaParTMA+log(Provisoes)    #Provisoes sem signific??ncia

ratioProvAssist<-(log(dadosEFPC$Provisoes)*(1/log(dadosEFPC$NumAssistidos)))

formula1<-MetaAtuarial~RentFutPatr+Rentabilidade+ratioProvAssist+lnPatrimonioLiq+TaxaParTMA #RentFut n??o sign. mas res??duos heteroced??st.
formula1<-MetaAtuarial~Rentabilidade+ratioProvAssist+lnPatrimonioLiq+TaxaParTMA #todos sigificantes e testes OK
formula1<-MetaAtuarial~Rentabilidade+lnPatrimonioLiq+TaxaParTMA #todos signficativos, mas res??duos heteroced??sticos
formula1<-MetaAtuarial~Rentabilidade+TaxaParTMA #sem regressores significativos
formula1<-MetaAtuarial~Rentabilidade+lnPatrimonioLiq #sem regressores significativos
formula1<-MetaAtuarial~Rentabilidade+lnPatrimonioLiq+log(NumAssistidos)  #NumAssistidos sem signific??ncia

ratioProvAssist<-log(dadosEFPC$Provisoes/dadosEFPC$NumAssistidos)
ratioProvPartic<-log(dadosEFPC$Provisoes*(1/(dadosEFPC$NumAssistidos+dadosEFPC$NumAtivos)))
View(ratioProvPartic)

#modelos com os mais significantes
formula1<-MetaAtuarial~Rentabilidade+ratioProvAssist+lnPatrimonioLiq+TaxaParTMA #todos sigificantes e testes OK
formula1<-MetaAtuarial~Rentabilidade+ratioProvPartic+lnPatrimonioLiq+TaxaParTMA #todos sigificantes e testes OK

#modelo da Tabela 7, o mais signicante encontrado, o primeiro do par anterior
formula1<-MetaAtuarial~Rentabilidade+ratioProvAssist+lnPatrimonioLiq+TaxaParTMA #todos sigificantes e testes OK

#Uma das vari??veis expressa a provis??o m??dia por assistidos e a outra pelos participantes
reg.ef=plm(formula1, data=dadosEFPC, model="between") #aqui deixa de ter correla????o serial
summary(reg.ef)
bptest(reg.ef) #The null hypothesis for the Breusch-Pagan test is homoskedasticity
pbgtest(reg.ef)
shapiro.test(reg.ef$residuals) # os dados originais com between passam nos 3 testes: correla????o serial, normalidade e homocedasticidade


lagRatioProvAssist<-(log(lag(dadosEFPC$Provisoes,-1))/log(lag(dadosEFPC$NumAssistidos,-1)))
formula1<-MetaAtuarial~Rentabilidade+ratioProvAssist+lag(lnPatrimonioLiq,-1)+TaxaParTMA #todos sigificantes e testes OK

formula1<-MetaAtuarial~Rentabilidade+lagRatioProvAssist+lnPatrimonioLiq+TaxaParTMA #todos sigificantes e testes OK
formula1<-MetaAtuarial~Rentabilidade+lag(ratioProvAssist,-1)+lnPatrimonioLiq+TaxaParTMA #todos sigificantes e testes OK





#teste<-plm(formula1, data=dadosEFPC, model="between") #aqui deixa de ter correla????o serial
#testeReg<-plm(formula1, data=dadosEFPC, model="between") #aqui deixa de ter correla????o serial
#anova(teste,testeReg) # deu um resultado (9.539e-11) < 0,05 logo as vari??ncias n??o s??o homog??neas 
# e n??o h?? igualdade entre as duas regress??es

### 10/02/2021

#formula1<-MetaAtuarial~Rentabilidade+log(Provisoes)+log(Patrimonio)+IS+lnPatrimonioLiq+TaxaParTMA+Duration+log(1+AtivAssist)+log(1+NumAssistidos)+propIntPatroc+propIntPartic+Ano+log(IBOV31dez)+log(IBOV02jan)+varptsIBOV12meses+log(IBOVMax)+mediaNTNB2045+mediaNTNB2050+mediaNTNB2035+mediaNTNB2024+raiz5avarIBOV
#formula1<-MetaAtuarial~lag(MetaAtuarial,-1)+Rentabilidade+log(Provisoes)+log(Patrimonio)+IS+lnPatrimonioLiq+TaxaParTMA+Duration+log(1+AtivAssist)+log(1+NumAssistidos)+Ano+log(IBOV31dez)+log(IBOV02jan)+varptsIBOV12meses+log(IBOVMax)+propIntPatroc+propIntPartic+mediaNTNB2045+mediaNTNB2050+mediaNTNB2035+mediaNTNB2024+raiz5avarIBOV
#formula1<-MetaAtuarial~lag(MetaAtuarial,-2)+Rentabilidade+log(Provisoes)+log(Patrimonio)+IS+lnPatrimonioLiq+TaxaParTMA+Duration+log(AtivAssist)+log(1+NumAssistidos)+propIntPatroc+propIntPartic+Ano+log(IBOV31dez)+log(IBOV02jan)+varptsIBOV12meses+log(IBOVMax)+propIntPatroc+propIntPartic+mediaNTNB2045+mediaNTNB2050+mediaNTNB2035+mediaNTNB2024+raiz5avarIBOV
#formula1<-MetaAtuarial~lag(Rentabilidade,-1)+log(Provisoes)+log(Patrimonio)+IS+lnPatrimonioLiq+TaxaParTMA+Duration+log(1+AtivAssist)+log(1+NumAssistidos)+propIntPatroc+propIntPartic+Ano+log(IBOV31dez)+log(IBOV02jan)+varptsIBOV12meses+log(IBOVMax)+propIntPatroc+propIntPartic+mediaNTNB2045+mediaNTNB2050+mediaNTNB2035+mediaNTNB2024+raiz5avarIBOV
formula1<-MetaAtuarial~lag(Rentabilidade,-1)+lag(log(Provisoes),-1)+log(Patrimonio)+IS+lnPatrimonioLiq+TaxaParTMA+Duration+log(1+AtivAssist)+log(1+NumAssistidos)+Ano+varptsIBOV12meses+log(IBOVMax)+mediaNTNB2045+mediaNTNB2050+raiz5avarIBOV
formula1<-MetaAtuarial~Ano+lnPatrimonioLiq+Duration+log(1+AtivAssist)+varptsIBOV12meses+log(IBOVMax)+mediaNTNB2045+mediaNTNB2050+raiz5avarIBOV
Surplus<-log(dadosEFPC$Provisoes)/log(dadosEFPC$Patrimonio)
formula1<-MetaAtuarial~Surplus+Ano+lnPatrimonioLiq+Duration+log(1+AtivAssist)+varptsIBOV12meses+log(IBOVMax)+mediaNTNB2045+mediaNTNB2050+raiz5avarIBOV
formula1<-MetaAtuarial~Surplus+Ano+lnPatrimonioLiq+Duration+log(1+AtivAssist)
formula1<-MetaAtuarial~TaxaParTMA+Surplus+Ano+lnPatrimonioLiq+Duration+AtivAssist
formula1<-MetaAtuarial~IS+TaxaParTMA+Surplus+Ano+lnPatrimonioLiq+Duration+AtivAssist
formula1<-MetaAtuarial~IS+TaxaParTMA+Surplus+Ano+lnPatrimonioLiq+Duration+log(1+NumAssist)
Meta100<-100*dadosEFPC$MetaAtuarial
Rent100<-100*dadosEFPC$Rentabilidade
TaxaParTMA100<-100*dadosEFPC$TaxaParTMA
RentFutPatr100<-100*dadosEFPC$RentFutPatr
NTNB2045<-100*dadosEFPC$mediaNTNB2045
NTNB2035<-100*dadosEFPC$mediaNTNB2035
NTNB2024<-100*dadosEFPC$mediaNTNB2024
NTNB2050<-100*dadosEFPC$mediaNTNB2050

formula1<-Meta100~NumAssist+lnPatrimonioLiq+TaxaParTMA100+IS+RentFutPatr100+NTNB2045+raiz5avarIBOV
formula1<-Meta100~Rent100+lnPatrimonioLiq+TaxaParTMA100+log(NumAssistidos)
formula1<-Meta100~raiz5avarIBOV

#formula1<-MetaAtuarial~lnPatrimonioLiq+Duration+log(1+NumAssist)+varptsIBOV12meses+mediaNTNB2045+mediaNTNB2050+raiz5avarIBOV
#formula1<-MetaAtuarial~log(Patrimonio)+IS

reg.ef=plm(formula1, data=dadosEFPC, model="within")
summary(reg.ef)
bptest(reg.ef) #The null hypothesis for the Breusch-Pagan test is homoskedasticity
pbgtest(reg.ef)
shapiro.test(reg.ef$residuals) # os dados originais com between passam nos 3 testes: correla????o serial, normalidade e homocedasticidade

##### 10/02/2021



mean(reg.ef$residuals)
median(reg.ef$residuals)
summary(reg.ef)
summary(fixef(reg.ef))
bptest(reg.ef) #The null hypothesis for the Breusch-Pagan test is homoskedasticity
pbgtest(reg.ef)$p.value
pbgtest(reg.ef)
#"Serial correlation tests apply to macro panels with long time series. Not a problem in micro
#"panels (with very few years). The null is that there is not serial correlation."
shapiro.test(reg.ef$residuals) # os dados originais com between passam nos 3 testes: correla????o serial, normalidade e homocedasticidade

# (0) between modificando as vari??veis taxas
Meta100<-100*dadosEFPC$MetaAtuarial
Rent100<-100*dadosEFPC$Rentabilidade
TaxaParTMA100<-100*dadosEFPC$TaxaParTMA
RentFutPatr100<-100*dadosEFPC$RentFutPatr
NTNB2045<-100*dadosEFPC$mediaNTNB2045
NTNB2035<-100*dadosEFPC$mediaNTNB2035
NTNB2024<-100*dadosEFPC$mediaNTNB2024
NTNB2050<-100*dadosEFPC$mediaNTNB2050
formula1<-Meta100~Rent100+Provisoes+Patrimonio+IS+TaxaParTMA100+RentFutPatr100+Duration+NumAtivos+NumAssistidos+propIntPatroc+propIntPartic+Ano+IBOV31dez+IBOV02jan+varptsIBOV12meses+IBOVMax+NTNB2045+NTNB2050+NTNB2035+NTNB2024
formula1<-Meta100~Rent100+log(Provisoes)+log(Patrimonio)+IS+TaxaParTMA100+RentFutPatr100+Duration+log(1+NumAtivos)+log(1+NumAssistidos)+propIntPatroc+propIntPartic+factor(Ano)+log(IBOV31dez)+log(IBOV02jan)+varptsIBOV12meses+log(IBOVMax)+NTNB2045+NTNB2050+NTNB2035+NTNB2024
reg.ef=plm(formula1, data=dadosEFPC, model="between") #aqui deixa de ter correla????o serial
summary(reg.ef)
bptest(reg.ef) #The null hypothesis for the Breusch-Pagan test is homoskedasticity
pbgtest(reg.ef)
#"Serial correlation tests apply to macro panels with long time series. Not a problem in micro
#"panels (with very few years). The null is that there is not serial correlation."
shapiro.test(reg.ef$residuals) # os dados originais com between passam nos 3 testes: correla????o serial, normalidade e homocedasticidade


formula1<-MetaAtuarial~Rentabilidade+Provisoes+Patrimonio+IS+TaxaParTMA+Duration+NumAtivos+NumAssistidos+propIntPatroc+propIntPartic+Ano+IBOV31dez+IBOV02jan+varptsIBOV12meses+IBOVMax+propIntPatroc+propIntPartic+mediaNTNB2045+mediaNTNB2050+mediaNTNB2035+mediaNTNB2024
# (1) no notepad
reg.ef=plm(formula1, data=dadosEFPC, model="within")
reg.ef=plm(formula1, data=dadosEFPC, model="between") #aqui deixa de ter correla????o serial

# (2) no notepad
reg.ef=plm(MetaAtuarial~MetaAnoAnterior+Ano+Duration+Rentabilidade+NumAtivos+NumAssistidos+log(Provisoes)+log(Patrimonio)+IS+TaxaParTMA+mediaRentPatr+IBOV31dez+IBOV02jan+IBOVMax+propIntPatroc+propIntPartic+mediaNTNB2045, data=dadosEFPC, model="within")
formula1<-MetaAtuarial~MetaAnoAnterior+Ano+Duration+Rentabilidade+NumAtivos+NumAssistidos+log(Provisoes)+log(Patrimonio)+IS+TaxaParTMA+mediaRentPatr+IBOV31dez+IBOV02jan+IBOVMax+propIntPatroc+propIntPartic+mediaNTNB2045
formula1<-MetaAtuarial~lag(MetaAtuarial,1)+Ano+Duration+Rentabilidade+NumAtivos+NumAssistidos+log(Provisoes)+log(Patrimonio)+IS+TaxaParTMA+mediaRentPatr+IBOV31dez+IBOV02jan+IBOVMax+propIntPatroc+propIntPartic+mediaNTNB2045
formula1<-MetaAtuarial~lag(MetaAtuarial,-1)+Ano+Duration+Rentabilidade+NumAtivos+NumAssistidos+log(Provisoes)+log(Patrimonio)+IS+TaxaParTMA+mediaRentPatr+IBOV31dez+IBOV02jan+IBOVMax+propIntPatroc+propIntPartic+mediaNTNB2045
reg.ef=plm(formula1, data=dadosEFPC, model="between") #aqui deixa de ter correla????o serial
# (3) no notepad
reg.ef=plm(MetaAtuarial~Ano+Duration+Rentabilidade+NumAtivos+NumAssistidos+log(Provisoes)+log(Patrimonio)+IS+TaxaParTMA+mediaRentPatr+IBOV31dez+IBOV02jan+IBOVMax+propIntPatroc+propIntPartic+mediaNTNB2045, data=dadosEFPC, model="within")

formula1<-MetaAtuarial~Rentabilidade+Provisoes+Patrimonio+IS+TaxaParTMA+Duration+NumAtivos+NumAssistidos+propIntPatroc+propIntPartic+IBOV31dez+IBOV02jan+varptsIBOV12meses+IBOVMax+propIntPatroc+propIntPartic+mediaNTNB2045+mediaNTNB2050+mediaNTNB2035+mediaNTNB2024
# (4) no notepad
reg.ef=plm(formula1, data=dadosEFPC, model="within")


formula1<-MetaAtuarial~Rentabilidade+log(Provisoes)+log(Patrimonio)+IS+TaxaParTMA+log(NumAtivos+NumAssistidos)+log(IBOV31dez)+log(IBOV02jan)+varptsIBOV12meses+log(IBOVMax)+mediaNTNB2045+mediaNTNB2050+mediaNTNB2035+mediaNTNB2024
# (5) no notepad
reg.ef=plm(formula1, data=dadosEFPC, model="within")

formula1<-MetaAtuarial~Rentabilidade+log(Provisoes)+log(Patrimonio)+lnPatrimonioLiq+IS+TaxaParTMA+log(NumAtivos+NumAssistidos)+RentFutPatr+log(IBOV31dez)+log(IBOV02jan)+varptsIBOV12meses+log(IBOVMax)+mediaNTNB2045+mediaNTNB2050+mediaNTNB2035+mediaNTNB2024
# (6) no notepad
reg.ef=plm(formula1, data=dadosEFPC, model="within")

Meta100<-100*dadosEFPC$MetaAtuarial
Rent100<-100*dadosEFPC$Rentabilidade
TaxaParTMA100<-100*dadosEFPC$TaxaParTMA
RentFutPatr100<-100*dadosEFPC$RentFutPatr
NTNB2045<-100*dadosEFPC$mediaNTNB2045
NTNB2035<-100*dadosEFPC$mediaNTNB2035
NTNB2024<-100*dadosEFPC$mediaNTNB2024
NTNB2050<-100*dadosEFPC$mediaNTNB2050
formula1<-Meta100~Rent100+log(Provisoes)+log(Patrimonio)+lnPatrimonioLiq+IS+TaxaParTMA100+log(NumAtivos+NumAssistidos)+RentFutPatr100+log(IBOV31dez)+log(IBOV02jan)+log(IBOVMax)+NTNB2045+NTNB2050+NTNB2035+NTNB2024
# (7) no notepad
reg.ef=plm(formula1, data=dadosEFPC, model="within")

formula1<-Meta100~Rent100+log(Patrimonio)+lnPatrimonioLiq+IS+TaxaParTMA100+log(NumAtivos+NumAssistidos)+log(IBOV31dez)+log(IBOV02jan)+log(IBOVMax)+NTNB2045+NTNB2050
# (8) no notepad
reg.ef=plm(formula1, data=dadosEFPC, model="within")


Meta100<-1/dadosEFPC$MetaAtuarial
Rent100<-100*dadosEFPC$Rentabilidade
TaxaParTMA100<-1/dadosEFPC$TaxaParTMA
RentFutPatr100<-100*dadosEFPC$RentFutPatr  # h?? zeros nos dados originais
NTNB2045<-1/dadosEFPC$mediaNTNB2045
NTNB2050<-1/dadosEFPC$mediaNTNB2050
formula1<-Meta100~Rent100+log(Provisoes)+log(Patrimonio)+lnPatrimonioLiq+IS+TaxaParTMA100+log(NumAtivos+NumAssistidos)+RentFutPatr100+log(IBOV31dez)+log(IBOV02jan)+log(IBOVMax)+NTNB2045+NTNB2050
# (9) no notepad
reg.ef=plm(formula1, data=dadosEFPC, model="within")
reg.ef=plm(formula1, data=dadosEFPC, model="between") #aqui deixa de ter correla????o serial


reg.ef=plm(log(MetaAtuarial)~log(MetaAnoAnterior)+Ano+log(Duration)+Rentabilidade+log(1+NumAtivos)+log(1+NumAssistidos)+log(Provisoes)+log(Patrimonio)+IS+log(TaxaParTMA)+mediaRentPatr+log(IBOV31dez)+log(IBOV02jan)+log(IBOVMax)+propIntPatroc+propIntPartic+log(mediaNTNB2045), data=dadosEFPC, model="within")
summary(reg.ef)
bptest(reg.ef) #The null hypothesis for the Breusch-Pagan test is homoskedasticity
#  p-value = 0.5639, dados homoced??sticos
pbgtest(reg.ef)$p.value
pbgtest(reg.ef)
#"Serial correlation tests apply to macro panels with long time series. Not a problem in micro
#"panels (with very few years). The null is that there is not serial correlation."
reg.ef=plm(MetaAtuarial~MetaAnoAnterior+Rentabilidade+NumAtivos+NumAssistidos+IS+TaxaParTMA+mediaRentPatr+mediaNTNB2045, data=dadosEFPC, model="within")
reg.ef=plm(MetaAtuarial~MetaAnoAnterior+NumAtivos+NumAssistidos+IS+TaxaParTMA+mediaRentPatr+mediaNTNB2045, data=dadosEFPC, model="within")
reg.ef=plm(MetaAtuarial~MetaAnoAnterior+IS+TaxaParTMA+mediaRentPatr+mediaNTNB2045+Rentabilidade, data=dadosEFPC, model="within")
reg.ef=plm(MetaAtuarial~MetaAnoAnterior+IS+TaxaParTMA+mediaNTNB2045+Rentabilidade, data=dadosEFPC, model="within")
shapiro.test(dadosEFPC$MetaAtuarial)
shapiro.test(dadosEFPC$Patrimonio)
shapiro.test(log(dadosEFPC$Patrimonio))
shapiro.test(reg.ef$residuals)
fixef(reg.ef)

coeftest(reg.ef)
coeftest(reg.ef, vcovHC)
coeftest(reg.ef, vcovHC(reg.ef, method = "arellano"))
reg.ef=plm(MetaAtuarial~Ano+Duration+Rentabilidade+NumAtivos+NumAssistidos+log(Provisoes)+log(Patrimonio)+IS+TaxaParTMA+mediaRentPatr+IBOV31dez+IBOV02jan+IBOVMax+propIntPatroc+propIntPartic+mediaNTNB2045, data=dadosEFPC, model="within")
reg.ef=plm(MetaAtuarial~IS+TaxaParTMA+mediaNTNB2045+Rentabilidade+log(Provisoes)+log(Patrimonio), data=dadosEFPC, model="within")
reg.ef=plm(MetaAtuarial~IS+TaxaParTMA+mediaNTNB2045, data=dadosEFPC, model="within")
reg.ef=plm(MetaAtuarial~IS+mediaNTNB2045, data=dadosEFPC, model="within")








#plot(dadosEFPCOrig$NTNB2045, dadosEFPCOrig$MetaAtuarial, pch=19, xlab="NTNB2045", ylab="MetaAtuarial")
#abline(lm(dadosEFPCOrig$MetaAtuarial~dadosEFPCOrig$NTNB2045),lwd=3, col="red")

ggplot(dadosEFPCOrig, aes(x=((IBOV31dez/IBOV01jan)-1), y=MetaAtuarial)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) +
  ggtitle("Meta Atuarial explicada pela varia????o anual do IBOVESPA")

ggplot(dadosEFPCOrig, aes(x=mediaNTNs, y=MetaAtuarial)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) +
  ggtitle("Meta Atuarial explicada pela m??dia das NTN-B")

ggplot(dadosEFPCOrig, aes(x=mediaNTNB2020, y=MetaAtuarial)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) +
  ggtitle("Meta Atuarial explicada pela NTN-B 2020")

ggplot(dadosEFPCOrig, aes(x=mediaNTNB2024, y=MetaAtuarial)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) +
  ggtitle("Meta Atuarial explicada pela NTN-B 2024")

ggplot(dadosEFPCOrig, aes(x=mediaNTNB2035, y=MetaAtuarial)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) +
  ggtitle("Meta Atuarial explicada pela NTN-B 2045")

ggplot(dadosEFPCOrig, aes(x=mediaNTNB2045, y=MetaAtuarial)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) +
  ggtitle("Meta Atuarial explicada pela NTN-B 2045")

ggplot(dadosEFPCOrig, aes(x=mediaNTNB2050, y=MetaAtuarial)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) +
  ggtitle("Meta Atuarial explicada pela NTN-B 2045")

ggplot(dadosEFPCOrig, aes(x=TaxaParLimite, y=MetaAtuarial)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) +
  ggtitle("Meta Atuarial explicada pela Taxa-par??metro da PREVIC")

ggplot(dadosEFPCOrig, aes(x=(TaxaParLimite+0.004), y=MetaAtuarial)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) +
  ggtitle("Meta Atuarial explicada pelo Limite Superior da Taxa-par??metro da PREVIC")

ggplot(dadosEFPCOrig, aes(x=LimiteInf, y=MetaAtuarial)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) +
  ggtitle("Meta Atuarial explicada pelo Limite inferior da Taxa-par??metro da PREVIC")

ggplot(dadosEFPCOrig, aes(x=MetaAnoAnterior, y=MetaAtuarial)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) +
  ggtitle("Meta Atuarial explicada pela Meta do ano anterior")

ggplot(dadosEFPCOrig, aes(x=log(1+RentReal_liqInfl), y=log(1+MetaAtuarial))) +
  geom_point(shape=1) +
  geom_smooth(method=lm) +
  ggtitle("Meta Atuarial explicada pela Rentabilidade real do per??odo")

ggplot(dadosEFPCOrig, aes(x=IPCA_INPC_IGPM, y=MetaAtuarial)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) +
  ggtitle("Meta Atuarial explicada pelos ??ndices de Infla????o do per??odo")

ggplot(dadosEFPCOrig, aes(x=log(ProvsPlano), y=log(1+MetaAtuarial))) +
  geom_point(shape=1) +
  geom_smooth(method=lm) +
  ggtitle("Meta Atuarial explicada pelas Provis??es do Plano")

ggplot(dadosEFPCOrig, aes(x=log(PatrPlano), y=log(1+MetaAtuarial))) +
  geom_point(shape=1) +
  geom_smooth(method=lm) +
  ggtitle("Meta Atuarial explicada pelo Patrim??nio do Plano")

ggplot(dadosEFPCOrig, aes(x=(ProvsPlano/PatrPlano), y=MetaAtuarial)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) +
  ggtitle("Meta Atuarial explicada pelo ??ndice de cobertura do Plano")

ggplot(dadosEFPCOrig, aes(x=IS, y=MetaAtuarial)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) +
  ggtitle("Meta Atuarial explicada pelo ??ndice de cobertura abrangente do Plano")

ggplot(dadosEFPCOrig, aes(x=MetaNominalEsp, y=MetaAtuarial)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) +
  ggtitle("Meta Atuarial explicada pelo Meta Nominal Esperada para o per??odo")

ggplot(dadosEFPCOrig, aes(x=log(1+NumAtivos), y=MetaAtuarial)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) +
  ggtitle("Meta Atuarial explicada pelo N??mero de Ativos dos planos em cada ano")

ggplot(dadosEFPCOrig, aes(x=log(1+NumAssist), y=MetaAtuarial)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) +
  ggtitle("Meta Atuarial explicada pelo N??mero de Assistidos dos planos em cada ano")


coplot(MetaAtuarial ~ Ano|EFPCPlano, type="l", data=dadosEFPC) # Lines
coplot(MetaAtuarial ~ Ano|EFPCPlano, type="b", data=dadosEFPC) # Lines

scatterplot(MetaAtuarial ~ Ano|EFPCPlano, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=dadosEFPC)
hist(dadosEFPC$MetaAtuarial, breaks=20, col="grey")

plotmeans(MetaAtuarial ~ EFPCPlano, main="Heterogeineity across planos", data=dadosEFPC) # plotmeans draw a 95% confidence interval around the means
plotmeans(MetaAtuarial ~ NTNB2045, main="Heterogeineity across planos", data=dadosEFPC) # plotmeans draw a 95% confidence interval around the means
plotmeans(MetaAtuarial ~ TaxaParLimite, main="Heterogeineity across planos", data=dadosEFPC) # plotmeans draw a 95% confidence interval around the means

dadosEFPC$mediaNTNB2045

#Tentativas - 2h de tentativas
#reg.pooled=plm(MetaAtuarial~Provs+Patr, data=dadosEFPC, model="pooling")
#reg.pooled=plm(MetaAtuarial~mediaNTNB2050+mediaNTNB2045+mediaNTNB2035+mediaNTNB2024+mediaNTNB2020+mediaNTNB2017, data=dadosEFPC, model="pooling")

#LnResultado = (log(PatrPlano)-log(ProvsPlano))

#reg.ef=plm(log(1+MetaAtuarial)~log(1+mediaNTNB2045)+log(1+RentObservada)+IS+MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="within")
#reg.ef=plm(log(1+MetaAtuarial)~log(1+mediaNTNB2045)+log(1+RentObservada)+IS+MetaAnoAnterior+RentObservada+LnResultado, data=dadosEFPC, model="within")
#reg.ef=plm(log(1+MetaAtuarial)~log(1+mediaNTNB2045)+log(1+RentObservada)+IS+MetaAnoAnterior+RentObservada, data=dadosEFPC, model="within")
#reg.ef=plm(log(1+MetaAtuarial)~log(1+mediaNTNB2045)+log(1+RentObservada)+IS+log(1+MetaAnoAnterior), data=dadosEFPC, model="within")
#reg.ef=plm(log(1+MetaAtuarial)~log(1+mediaNTNB2045)+IS+log(1+MetaAnoAnterior), data=dadosEFPC, model="within")
#reg.ef=plm(MetaAtuarial~mediaNTNB2045+IS+MetaAnoAnterior, data=dadosEFPC, model="within")

#reg.pooled=plm(MetaAtuarial~DefSup+CrescIBOVpts+IBOV31dez+IBOV01jan, data=dadosEFPC, model="pooling")
#reg.pooled=plm(MetaAtuarial~DurationMeses+CrescIBOVpts+IBOV31dez+IBOV01jan, data=dadosEFPC, model="pooling")
#reg.pooled=plm(MetaAtuarial~TaxaParLimite+CrescIBOVpts+IBOV31dez+IBOV01jan, data=dadosEFPC, model="pooling")
#reg.pooled=plm(MetaAtuarial~IPCAINPCOcorrido+CrescIBOVpts+IBOV31dez+IBOV01jan, data=dadosEFPC, model="pooling")
#reg.pooled=plm(MetaAtuarial~MetaAnoAnterior+CrescIBOVpts+IBOV31dez+IBOV01jan, data=dadosEFPC, model="pooling")
#reg.pooled=plm(MetaAtuarial~DifObsEspNom+RentObservada+LimiteInf+LimiteSup+CrescIBOVpts+IBOV31dez+IBOV01jan, data=dadosEFPC, model="pooling")
#reg.pooled=plm(MetaAtuarial~IntPatroc+IntEFPC+IntPart+IntAssist, data=dadosEFPC, model="pooling")
#reg.pooled=plm(MetaAtuarial~DefSup+CrescIBOVpts+IBOV31dez+IBOV01jan, data=dadosEFPC, model="pooling")
#reg.pooled=plm(MetaAtuarial~MetaEsperada+CrescIBOVpts+mediaNTNB2045, data=dadosEFPC, model="pooling")
#reg.pooled=plm(MetaAtuarial~NTNB2050+NTNB2045+NTNB2035+NTNB2024+NTNB2020+NTNB2017, data=dadosEFPC, model="pooling")
#reg.pooled=plm(MetaAtuarial~CrescIBOVpts+IBOV31dez+IBOV01jan, data=dadosEFPC, model="pooling")
#reg.pooled=plm(MetaAtuarial~NTNB2045+maxNTNB2045+NTNB2024+maxNTNB2024+NTNB2020, data=dadosEFPC, model="pooling")
#reg.pooled=plm(MetaAtuarial~NTNB2045+minNTNB2045+NTNB2024+minNTNB2024+minNTNB2020+NTNB2020, data=dadosEFPC, model="pooling")
# Unbalanced Panel: n = 22, T = 6-7, N = 140
# s??o 22 planos de EFPC, os dados n??o est??o balanceados pq h?? dados para 2013-2019 e 2013-2018, ou seja, T = 6-7 anos

reg.pooled=plm(log(1+MetaAtuarial)~log(ProvsPlano)+log(PatrPlano), data=dadosEFPC, model="pooling")
reg.pooled=plm(MetaAtuarial~log(ProvsPlano)+log(PatrPlano), data=dadosEFPC, model="pooling")
#bem significantes
reg.pooled=plm(MetaAtuarial~lag(MetaAtuarial,1)+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="pooling") #A melhor op????o, a mais significante
reg.pooled=plm(MetaAtuarial~MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="pooling") #A melhor op????o, a mais significante
reg.pooled=plm(log(1+MetaAtuarial)~MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="pooling") #A melhor op????o, a mais significante
reg.pooled=plm(log(1+MetaAtuarial)~IS+Meta2016+MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="pooling")
reg.pooled=plm(log(1+MetaAtuarial)~IS+MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="pooling")
summary(reg.pooled)

#Explicando via composi????o do CD 
reg.pooled=plm(log(1+100*MetaAtuarial)~IntPatroc+IntEFPC+IntPart+IntAssist, data=dadosEFPC, model="pooling")
summary(reg.pooled)
reg.pooled=plm(log(1+MetaAtuarial)~log(1+IntPatroc)+log(1+IntEFPC)+log(1+IntPart)+log(1+IntAssist), data=dadosEFPC, model="pooling")
summary(reg.pooled)
reg.ef=plm(MetaAtuarial~IntPatroc+IntEFPC+IntPart+IntAssist, data=dadosEFPC, model="within")
summary(reg.ef)
reg.ef=plm(log(1+MetaAtuarial)~log(1+IntPatroc)+log(1+IntEFPC)+log(1+IntPart)+log(1+IntAssist), data=dadosEFPC, model="within")
summary(reg.ef)
reg.ef=plm(MetaAtuarial~IntPatroc+IntEFPC+IntPart+IntAssist, data=dadosEFPC, model="between")
summary(reg.ef)

#plot(factor(dadosEFPC$NTNB2045), factor(dadosEFPC$MetaAtuarial), main="Diagrama de Dispers??o", xlab="NTNB2045", ylab="MetaAtuarial", pch=19)

#Se uma determinada vari??vel x ?? constante ao longo de todos os per??odos, n??o precisa ser inclu??da na regress??o. O seu efeito
#em princ??pio est?? sendo capturado pela constante ??(i)
#dadosEFPC$MetaAtuarialInt <- (dadosEFPC$MetaAtuarial*100)

#Efeitos Fixos within
reg.ef=plm(log(1+MetaAtuarial)~log(ProvsPlano)+log(PatrPlano), data=dadosEFPC, model="within")
reg.ef=plm(MetaAtuarial~log(ProvsPlano)+log(PatrPlano), data=dadosEFPC, model="within")
reg.ef=plm(log(MetaAtuarialInt)~log(ProvsPlano)+log(PatrPlano), data=dadosEFPC, model="within")
reg.ef=plm(MetaAtuarialInt~log(ProvsPlano)+log(PatrPlano), data=dadosEFPC, model="within")
#bem significantes
reg.ef=plm(MetaAtuarial~lag(MetaAtuarial,1)+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="within")
reg.ef=plm(MetaAtuarialInt~lag(MetaAtuarialInt,1)+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="within")
reg.ef=plm(MetaAtuarial~MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="within")
reg.ef=plm(MetaAtuarialInt~MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="within")
reg.ef=plm(log(1+MetaAtuarial)~MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="within")
reg.ef=plm(log(1+MetaAtuarial)~IS+Meta2016+MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="within")
reg.ef=plm(log(1+MetaAtuarial)~IS+MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="within")
summary(reg.ef)
summary(fixef(reg.ef))

#reg.ef=plm(log(1+MetaAtuarial)~log(ProvsPlano)+log(PatrPlano)+as.factor(Ano), data=dadosEFPC, model="within")
#reg.ef=plm(MetaAtuarial~log(ProvsPlano)+log(PatrPlano)+as.factor(Ano), data=dadosEFPC, model="within")
#bem significantes
#reg.ef=plm(MetaAtuarial~lag(MetaAtuarial,1)+RentObservada+RentReal_liqInfl+as.factor(Ano), data=dadosEFPC, model="within")
#reg.ef=plm(MetaAtuarial~MetaAnoAnterior+RentObservada+RentReal_liqInfl+as.factor(Ano), data=dadosEFPC, model="within")
#reg.ef=plm(log(1+MetaAtuarial)~MetaAnoAnterior+RentObservada+RentReal_liqInfl+as.factor(Ano), data=dadosEFPC, model="within")
#reg.ef=plm(log(1+MetaAtuarial)~IS+Meta2016+MetaAnoAnterior+RentObservada+RentReal_liqInfl+as.factor(Ano), data=dadosEFPC, model="within")
#reg.ef=plm(log(1+MetaAtuarial)~IS+MetaAnoAnterior+RentObservada+RentReal_liqInfl+as.factor(Ano), data=dadosEFPC, model="within")
#summary(reg.ef)
#summary(fixef(reg.ef))

#Efeitos Fixos between
reg.ef=plm(log(1+MetaAtuarial)~log(ProvsPlano)+log(PatrPlano), data=dadosEFPC, model="between")
reg.ef=plm(MetaAtuarial~log(ProvsPlano)+log(PatrPlano), data=dadosEFPC, model="between")
#bem significantes
reg.ef=plm(MetaAtuarial~lag(MetaAtuarial,1)+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="between")
reg.ef=plm(MetaAtuarial~MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="between")
reg.ef=plm(log(1+MetaAtuarial)~MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="between")
reg.ef=plm(log(1+MetaAtuarial)~IS+Meta2016+MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="between")
reg.ef=plm(log(1+MetaAtuarial)~IS+MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="between")
reg.ef=plm(log(1+MetaAtuarial)~lag(IS,1)+Meta2016+MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="between")
reg.ef=plm(log(1+MetaAtuarial)~lag(IS,1)+MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="between")
#reg.ef=plm(log(1+MetaAtuarial)~lag(IS,1)+IS+Meta2016+MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="between")
#reg.ef=plm(log(1+MetaAtuarial)~lag(IS,1)+IS+MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="between")
summary(reg.ef)
summary(fixef(reg.ef))

#summary(lm(MetaAtuarial~NTNB2045+NTNB2024+NTNB2020+as.factor(EFPCPlano), data=dadosEFPC))

#Efeitos Fixos fd
reg.ef=plm(log(1+MetaAtuarial)~log(ProvsPlano)+log(PatrPlano)+as.factor(Ano), data=dadosEFPC, model="fd")
reg.ef=plm(MetaAtuarial~log(ProvsPlano)+log(PatrPlano)+as.factor(Ano), data=dadosEFPC, model="fd")
#bem significantes
reg.ef=plm(MetaAtuarial~lag(MetaAtuarial,1)+RentObservada+RentReal_liqInfl+as.factor(Ano), data=dadosEFPC, model="fd")
reg.ef=plm(MetaAtuarial~MetaAnoAnterior+RentObservada+RentReal_liqInfl+as.factor(Ano), data=dadosEFPC, model="fd")
reg.ef=plm(log(1+MetaAtuarial)~MetaAnoAnterior+RentObservada+RentReal_liqInfl+as.factor(Ano), data=dadosEFPC, model="fd")
reg.ef=plm(log(1+MetaAtuarial)~IS+Meta2016+MetaAnoAnterior+RentObservada+RentReal_liqInfl+as.factor(Ano), data=dadosEFPC, model="fd")
reg.ef=plm(log(1+MetaAtuarial)~IS+MetaAnoAnterior+RentObservada+RentReal_liqInfl+as.factor(Ano), data=dadosEFPC, model="fd")
summary(reg.ef)
summary(fixef(reg.ef))

#Efeitos aleat??rios
reg.ea=plm(log(1+MetaAtuarial)~log(ProvsPlano)+log(PatrPlano), data=dadosEFPC, model="random", random.method = "walhus")
reg.ea=plm(MetaAtuarial~log(ProvsPlano)+log(PatrPlano), data=dadosEFPC, model="random", random.method = "walhus")
#bem significantes
reg.ea=plm(MetaAtuarial~lag(MetaAtuarial,1)+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="random", random.method = "walhus")
reg.ea=plm(MetaAtuarial~MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="random", random.method = "walhus")
reg.ea=plm(log(1+MetaAtuarial)~MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="random", random.method = "walhus")
reg.ea=plm(log(1+MetaAtuarial)~IS+Meta2016+MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="random", random.method = "walhus")
reg.ea=plm(log(1+MetaAtuarial)~IS+MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="random", random.method = "walhus")
summary(reg.ea)
summary(fixef(reg.ef))


#Pooled versus Efeitos Fixos versus Aleat??rios 
pFtest(reg.ef,reg.pooled)
#testa se n??o existem efeitos fixos nos dados analisados. Teste F ou teste F de Chow. 
#A hip??tese nula ?? de que h?? igualdade nos interceptos e nas inclina????es para todos os indiv??duos, caracterizando 
#o modelo de dados agrupados (pooled).Se rejeitar, existem EF e descarta-se pooled
#P_valor maior do que 0,05, logo EF n??o seria o melhor para esses dados que tratam somente dos planos da Centrus, era esperado.

View(dadosEFPC)
#MODELO BASE - 07/02/2021
#reg.ef=plm(MetaAtuarial~MetaAnoAnterior+Duration+IPCA_INPC_IGPM+RentObservada+RentReal_liqInfl+NumAtivos+NumAssist+log(ProvsPlano)+log(PatrPlano)+IS+LnResultado+TaxaParLimite+MediaRentFutura+IBOVlast+IBOVfirst+IBOVMax+propIntPatroc+propIntPartic+mediaNTNB2045, data=dadosEFPC, model="within")
reg.ef=plm(MetaAtuarial~MetaAnoAnterior+Ano+Duration+Rentabilidade+NumAtivos+NumAssistidos+log(Provisoes)+log(Patrimonio)+IS+TaxaParTMA+mediaRentPatr+IBOV31dez+IBOV02jan+IBOVMax+propIntPatroc+propIntPartic+mediaNTNB2045, data=dadosEFPC, model="within")
reg.ef=plm(log(MetaAtuarial)~log(MetaAnoAnterior)+Ano+log(Duration)+Rentabilidade+log(1+NumAtivos)+log(1+NumAssistidos)+log(Provisoes)+log(Patrimonio)+IS+log(TaxaParTMA)+mediaRentPatr+log(IBOV31dez)+log(IBOV02jan)+log(IBOVMax)+propIntPatroc+propIntPartic+log(mediaNTNB2045), data=dadosEFPC, model="within")
summary(reg.ef)
bptest(reg.ef) #The null hypothesis for the Breusch-Pagan test is homoskedasticity
#  p-value = 0.5639, dados homoced??sticos
pbgtest(reg.ef)$p.value
pbgtest(reg.ef)
#"Serial correlation tests apply to macro panels with long time series. Not a problem in micro
#"panels (with very few years). The null is that there is not serial correlation."
reg.ef=plm(MetaAtuarial~MetaAnoAnterior+Rentabilidade+NumAtivos+NumAssistidos+IS+TaxaParTMA+mediaRentPatr+mediaNTNB2045, data=dadosEFPC, model="within")
reg.ef=plm(MetaAtuarial~MetaAnoAnterior+NumAtivos+NumAssistidos+IS+TaxaParTMA+mediaRentPatr+mediaNTNB2045, data=dadosEFPC, model="within")
reg.ef=plm(MetaAtuarial~MetaAnoAnterior+IS+TaxaParTMA+mediaRentPatr+mediaNTNB2045+Rentabilidade, data=dadosEFPC, model="within")
reg.ef=plm(MetaAtuarial~MetaAnoAnterior+IS+TaxaParTMA+mediaNTNB2045+Rentabilidade, data=dadosEFPC, model="within")
shapiro.test(dadosEFPC$MetaAtuarial)
shapiro.test(dadosEFPC$Patrimonio)
shapiro.test(log(dadosEFPC$Patrimonio))
shapiro.test(reg.ef$residuals)
fixef(reg.ef)

coeftest(reg.ef)
coeftest(reg.ef, vcovHC)
coeftest(reg.ef, vcovHC(reg.ef, method = "arellano"))
reg.ef=plm(MetaAtuarial~Ano+Duration+Rentabilidade+NumAtivos+NumAssistidos+log(Provisoes)+log(Patrimonio)+IS+TaxaParTMA+mediaRentPatr+IBOV31dez+IBOV02jan+IBOVMax+propIntPatroc+propIntPartic+mediaNTNB2045, data=dadosEFPC, model="within")
reg.ef=plm(MetaAtuarial~IS+TaxaParTMA+mediaNTNB2045+Rentabilidade+log(Provisoes)+log(Patrimonio), data=dadosEFPC, model="within")
reg.ef=plm(MetaAtuarial~IS+TaxaParTMA+mediaNTNB2045, data=dadosEFPC, model="within")
reg.ef=plm(MetaAtuarial~IS+mediaNTNB2045, data=dadosEFPC, model="within")


lambda<- BoxCox.lambda(dadosEFPC$Patrimonio, 
                       method = c("loglik"),
                       lower=-5,
                       upper=5)
Patrimonio_transf <- BoxCox(dadosEFPC$Patrimonio, lambda)
shapiro.test(Patrimonio_transf)
reg.ef=plm(MetaAtuarial~MetaAnoAnterior+Ano+Duration+Rentabilidade+NumAtivos+NumAssistidos+log(Provisoes)+Patrimonio_transf+IS+TaxaParTMA+mediaRentPatr+IBOV31dez+IBOV02jan+IBOVMax+propIntPatroc+propIntPartic+mediaNTNB2045, data=dadosEFPC, model="within")

shapiro.test(reg.ef$residuals) #  H0: h?? normalidade dos res??duos p-valor <=0,05 rejeita H0
bptest(reg.ef) # rejeitar a hip??tese indica heterocedasticidade nos res??duos. H0: h?? homocedasticidade
# 	Shapiro-Wilk normality test

#data:  reg.ef$residuals
#W = 0.96176, p-value = 0.000917

#MODELO 1
#reg.ef=plm(log(1+MetaAtuarial)~IS+MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="within")
#reg.pooled=plm(log(1+MetaAtuarial)~IS+MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="pooling")
reg.ef=plm(MetaAtuarial~IS+MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="within")
#reg.pooled=plm(MetaAtuarial~IS+MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="pooling")
#pFtest(reg.ef,reg.pooled) #Deu EF, p-valor < 0,05 F de Chow
summary(reg.pooled)
summary(reg.ef)
summary(fixef(reg.ef))
#reg.ea=plm(log(1+MetaAtuarial)~IS+MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="random", random.method = "walhus")
#reg.ea=plm(MetaAtuarial~IS+MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="random", random.method = "walhus")
#phtest(reg.ef,reg.ea)  #Deu EF  Hausman Test
#plmtest(reg.pooled, type="bp") #Como deu p-value = 0.541, pooled ?? melhor q EA, <0,05 logo aleat??rio ?? melhor q pooled.
#reg.ef=plm(log(1+MetaAtuarial)~IS+MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="between")
#reg.ef=plm(MetaAtuarial~IS+MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="between")
phtest(reg.ef,reg.ea)  #Deu EF, mas o BETWEEN ?? pior que o WITHIN em termos de regressores significantes
shapiro.test(reg.pooled$residuals) #  H0: h?? normalidade dos res??duos p-valor <=0,05 rejeita H0
shapiro.test(reg.ef$residuals) #  H0: h?? normalidade dos res??duos p-valor <=0,05 rejeita H0
shapiro.test(reg.ea$residuals) #  H0: h?? normalidade dos res??duos p-valor <=0,05 rejeita H0
bptest(reg.pooled) # rejeitar H0 indica heterocedasticidade nos res??duos. H0: h?? homocedasticidade, se p-valor < 0,05, rejeita H0
bptest(reg.ef) # rejeitar a hip??tese indica heterocedasticidade nos res??duos. H0: h?? homocedasticidade
bptest(reg.ea) # rejeitar a hip??tese indica heterocedasticidade nos res??duos. H0: h?? homocedasticidade
#somente o modelo between apresentou normalidade dos res??duos , MAS ELE N??O ?? O MELHOR MODELO
#todos os  modelos apresentaram homocedasticidade nos res??duos

#MODELO 2
#reg.pooled=plm(MetaAtuarial~IntPatroc+IntEFPC+IntPart+IntAssist, data=dadosEFPC, model="pooling")
reg.pooled=plm(MetaAtuarial~IntPatroc+IntPart+IntAssist, data=dadosEFPC, model="pooling")
reg.ef=plm(MetaAtuarial~IntPatroc+IntPart+IntAssist, data=dadosEFPC, model="within")
pFtest(reg.ef,reg.pooled) #Deu EF, p-valor < 0,05
reg.ea=plm(MetaAtuarial~IntPatroc+IntPart+IntAssist, data=dadosEFPC, model="random", random.method = "walhus")
phtest(reg.ef,reg.ea)  
residuos<-reg.ea$residuals
hist(residuos,col="orange",main="Histograma dos res??duos")
shapiro.test(reg.ea$residuals) #  H0: h?? normalidade dos res??duos p-valor <=0,05 rejeita H0
bptest(reg.ea)  # Bresch-pagan test
phtest(reg.ea,reg.pooled) #Deu pooled, p-valor < 0,05
pbgtest(reg.ea)$p.value
vif(reg.ea) #valores abaixo de 5 indicam que n??o h?? multicolinearidade
summary(reg.ea)
plmtest(reg.pooled, type="bp") #<0,05 logo aleat??rio ?? melhor q pooled
phtest(reg.ef,reg.ea)  #Deu EF, p-valor <0,05
#reg.ef=plm(MetaAtuarial~IntPatroc+IntEFPC+IntPart+IntAssist, data=dadosEFPC, model="between")
summary(reg.ef)
#phtest(reg.ef,reg.ea)  #Deu EA, p-valor >0,05
shapiro.test(reg.pooled$residuals) #  H0: h?? normalidade dos res??duos p-valor <=0,05 rejeita H0
shapiro.test(reg.ef$residuals) #  H0: h?? normalidade dos res??duos p-valor <=0,05 rejeita H0
shapiro.test(reg.ea$residuals) #  H0: h?? normalidade dos res??duos p-valor <=0,05 rejeita H0
bptest(reg.pooled) # rejeitar H0 indica heterocedasticidade nos res??duos. H0: h?? homocedasticidade, se p-valor < 0,05, rejeita H0
bptest(reg.ef) # rejeitar a hip??tese indica heterocedasticidade nos res??duos. H0: h?? homocedasticidade
bptest(reg.ea) # rejeitar a hip??tese indica heterocedasticidade nos res??duos. H0: h?? homocedasticidade
#nenhum dos modelos apresentou normalidade dos res??duos 
#todos os modelos apresentam homocedasticidade nos res??duos

#MODELO 3
reg.pooled=plm(MetaAtuarial~TaxaParLimite+LimiteInf+MetaAnoAnterior, data=dadosEFPC, model="pooling")
reg.ef=plm(MetaAtuarial~TaxaParLimite+LimiteInf+MetaAnoAnterior, data=dadosEFPC, model="within")
pFtest(reg.ef,reg.pooled) #Deu EF, p-valor < 0,05
reg.ea=plm(MetaAtuarial~TaxaParLimite+LimiteInf+MetaAnoAnterior, data=dadosEFPC, model="random", random.method = "walhus")
phtest(reg.ef,reg.ea)  #Deu EF p-valor < 0,05
summary(reg.ef)
summary(fixef(reg.ef))
summary(reg.pooled)
summary(reg.ef)
pFtest(reg.ef,reg.pooled) #Deu EF p-valor <0,05
reg.ea=plm(MetaAtuarial~MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="random", random.method = "walhus")
#reg.ea=plm(log(1+MetaAtuarial)~MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="random", random.method = "walhus")
phtest(reg.ef,reg.ea)  #Deu EF
#reg.ef=plm(MetaAtuarial~MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="between")
#reg.ef=plm(log(1+MetaAtuarial)~MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="between")
phtest(reg.ef,reg.ea)  #Deu EF
pFtest(reg.ef,reg.pooled) ##Deu EF p-valor <0,05, mas o modelo WITHIN ?? o ***** MELHOR **** PQ TEM MAIS REGRESSORES SIGNIFICANTES
summary(fixef(reg.ef))
shapiro.test(reg.pooled$residuals) #  H0: h?? normalidade dos res??duos p-valor <=0,05 rejeita H0
shapiro.test(reg.ef$residuals) #  H0: h?? normalidade dos res??duos p-valor <=0,05 rejeita H0
shapiro.test(reg.ea$residuals) #  H0: h?? normalidade dos res??duos p-valor <=0,05 rejeita H0
bptest(reg.pooled) # rejeitar H0 indica heterocedasticidade nos res??duos. H0: h?? homocedasticidade, se p-valor < 0,05, rejeita H0
bptest(reg.ef) # rejeitar a hip??tese indica heterocedasticidade nos res??duos. H0: h?? homocedasticidade
bptest(reg.ea) # rejeitar a hip??tese indica heterocedasticidade nos res??duos. H0: h?? homocedasticidade
#somente o modelo between apresentou normalidade dos res??duos 
#nenhum dos  modelos apresentou homocedasticidade nos res??duos, mas o between bateu na trave

#MODELO 4
#reg.pooled=plm(log(1+MetaAtuarial)~log(ProvsPlano)+log(PatrPlano), data=dadosEFPC, model="pooling")
#reg.ef=plm(log(1+MetaAtuarial)~log(ProvsPlano)+log(PatrPlano), data=dadosEFPC, model="within")
reg.pooled=plm(MetaAtuarial~log(ProvsPlano)+log(PatrPlano), data=dadosEFPC, model="pooling")
reg.ef=plm(MetaAtuarial~log(ProvsPlano)+log(PatrPlano), data=dadosEFPC, model="within")
#reg.ef=plm(MetaAtuarial~ProvsPlano+PatrPlano, data=dadosEFPC, model="within")
summary(reg.pooled)
pFtest(reg.ef,reg.pooled) #Deu EF, p-valor < 0,05
#reg.ea=plm(log(1+MetaAtuarial)~log(ProvsPlano)+log(PatrPlano), data=dadosEFPC, model="random", random.method = "walhus")
reg.ea=plm(MetaAtuarial~log(ProvsPlano)+log(PatrPlano), data=dadosEFPC, model="random", random.method = "walhus")
plmtest(reg.pooled, type="bp") #<0,05 logo aleat??rio ?? melhor q pooled
phtest(reg.ef,reg.ea)    #Deu EF, p-valor < 0,05
#reg.ef=plm(log(1+MetaAtuarial)~log(ProvsPlano)+log(PatrPlano), data=dadosEFPC, model="between")
#reg.ef=plm(MetaAtuarial~log(ProvsPlano)+log(PatrPlano), data=dadosEFPC, model="between")
summary(reg.ef)
summary(fixef(reg.ef))
phtest(reg.ef,reg.ea)  #Deu EF, tanto within como between
pFtest(reg.ef,reg.pooled) #Deu pooled para ef between, p-valor > 0,05. Logo escolhe-se o EF within, que foi melhor que pooled
shapiro.test(reg.pooled$residuals) #  H0: h?? normalidade dos res??duos p-valor <=0,05 rejeita H0
shapiro.test(reg.ef$residuals) #  H0: h?? normalidade dos res??duos p-valor <=0,05 rejeita H0
shapiro.test(reg.ea$residuals) #  H0: h?? normalidade dos res??duos p-valor <=0,05 rejeita H0
bptest(reg.pooled) # rejeitar a hip??tese indica heterocedasticidade nos res??duos. H0: h?? homocedasticidade
bptest(reg.ef) # rejeitar a hip??tese indica heterocedasticidade nos res??duos. H0: h?? homocedasticidade
bptest(reg.ea) # rejeitar a hip??tese indica heterocedasticidade nos res??duos. H0: h?? homocedasticidade
#nenhum dos modelos apresentou normalidade dos res??duos 
#todos os modelos apresentam  homocedasticidade nos res??duos, p-valor > 0,05


#MODELO 5
#reg.pooled=plm(log(MetaAtuarial)~log(mediaNTNB2045)+IS+log(MetaAnoAnterior), data=dadosEFPC, model="pooling")
#reg.ef=plm(log(MetaAtuarial)~log(mediaNTNB2045)+IS+log(MetaAnoAnterior), data=dadosEFPC, model="within")
reg.pooled=plm(MetaAtuarial~mediaNTNB2045+IS+MetaAnoAnterior, data=dadosEFPC, model="pooling")
reg.ef=plm(MetaAtuarial~mediaNTNB2045+IS+MetaAnoAnterior, data=dadosEFPC, model="within")
pFtest(reg.ef,reg.pooled) #Deu EF, p-valor < 0,05
summary(reg.ef)
summary(fixef(reg.ef))
#reg.ea=plm(log(MetaAtuarial)~log(mediaNTNB2045)+IS+log(MetaAnoAnterior), data=dadosEFPC, model="random", random.method = "walhus")
reg.ea=plm(MetaAtuarial~mediaNTNB2045+IS+MetaAnoAnterior, data=dadosEFPC, model="random", random.method = "walhus")
phtest(reg.ef,reg.ea)  #Deu EF p-valor < 0,05
#reg.ef=plm(log(MetaAtuarial)~log(mediaNTNB2045)+IS+log(MetaAnoAnterior), data=dadosEFPC, model="between")
#reg.ef=plm(MetaAtuarial~mediaNTNB2045+IS+MetaAnoAnterior, data=dadosEFPC, model="between")
pFtest(reg.ef,reg.pooled) #Deu EF, p-valor < 0,05
phtest(reg.ef,reg.ea)  #Deu EF, mas o BETWEEN melhor do que  WITHIN, ambos com  regressores significantes, mas R2 mais alto no 1o. *** VERIFICAR!!!! ***
shapiro.test(reg.pooled$residuals) #  H0: h?? normalidade dos res??duos p-valor <=0,05 rejeita H0
shapiro.test(reg.ef$residuals) #  H0: h?? normalidade dos res??duos p-valor <=0,05 rejeita H0,  rejeitou para within
shapiro.test(reg.ea$residuals) #  H0: h?? normalidade dos res??duos p-valor <=0,05 rejeita H0
bptest(reg.pooled) # rejeitar H0 indica heterocedasticidade nos res??duos. H0: h?? homocedasticidade, se p-valor < 0,05, rejeita H0
bptest(reg.ef) # rejeitar a hip??tese indica heterocedasticidade nos res??duos. H0: h?? homocedasticidade
bptest(reg.ea) # rejeitar a hip??tese indica heterocedasticidade nos res??duos. H0: h?? homocedasticidade
#APENAS BETWEEN apresentou normalidade dos res??duos 
#todos os  modelos apresentaram homocedasticidade nos res??duos


#MODELO 6
#reg.ef=plm(MetaAtuarial~mediaNTNB2045+IBOVMax+IBOV31dez+IBOV01jan+lag(IBOVMax,1)+lag(IBOV31dez,1)+lag(IBOV01jan,1)+MetaAnoAnterior, data=dadosEFPC, model="within")
#reg.ef=plm(MetaAtuarial~mediaNTNB2045+IBOVMax+IBOV31dez+IBOV01jan+MetaAnoAnterior, data=dadosEFPC, model="within")
#reg.ef=plm(MetaAtuarial~mediaNTNB2045+IBOVMax+IBOV31dez+IBOV01jan, data=dadosEFPC, model="within")
#reg.ef=plm(MetaAtuarial~mediaNTNB2045+lag(IBOVMax,1)+lag(IBOV31dez,1)+lag(IBOV01jan,1)+MetaAnoAnterior, data=dadosEFPC, model="within")
reg.pooled=plm(MetaAtuarial~lag(IBOVMax,1)+lag(IBOV31dez,1)+lag(IBOV01jan,1)+MetaAnoAnterior, data=dadosEFPC, model="pooling")
#reg.ef=plm(MetaAtuarial~lag(IBOVMax,1)+lag(IBOV31dez,1)+lag(IBOV01jan,1)+MetaAnoAnterior, data=dadosEFPC, model="within")
reg.ef=plm(MetaAtuarial~lag(log(IBOVMax),1)+lag(log(IBOV31dez),1)+lag(log(IBOV01jan),1)+MetaAnoAnterior, data=dadosEFPC, model="within")
pFtest(reg.ef,reg.pooled) #Deu EF, p-valor < 0,05
summary(reg.ef)
summary(fixef(reg.ef))
reg.ea=plm(MetaAtuarial~lag(log(IBOVMax),1)+lag(log(IBOV31dez),1)+lag(log(IBOV01jan),1)+MetaAnoAnterior, data=dadosEFPC, model="random", random.method = "walhus")
phtest(reg.ef,reg.ea)  #Deu EF p-valor < 0,05


#MODELO 7
reg.pooled=plm(MetaAtuarial~TaxaParLimite+LimiteInf+MetaAnoAnterior, data=dadosEFPC, model="pooling")
reg.ef=plm(MetaAtuarial~TaxaParLimite+LimiteInf+MetaAnoAnterior, data=dadosEFPC, model="within")
pFtest(reg.ef,reg.pooled) #Deu EF, p-valor < 0,05
reg.ea=plm(MetaAtuarial~TaxaParLimite+LimiteInf+MetaAnoAnterior, data=dadosEFPC, model="random", random.method = "walhus")
phtest(reg.ef,reg.ea)  #Deu EF p-valor < 0,05
summary(reg.ef)


#reg.pooled=plm(log(1+MetaAtuarial)~IS+Meta2016+MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="pooling")
#reg.ef=plm(log(1+MetaAtuarial)~IS+Meta2016+MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="within")
#pFtest(reg.ef,reg.pooled) #Deu Pooled para within
#plmtest(reg.pooled, type="bp") #p_valor abaixo de 0,05, rejeita a hip??tese e h?? efeitos aleat??rios.
#Continuou dando pooled, provavelmente por conta de valores fixos para Meta2016 para cada EFPC+Plano
#reg.ef=plm(log(1+MetaAtuarial)~IS+Meta2016+MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="between")
#reg.ea=plm(log(1+MetaAtuarial)~IS+Meta2016+MetaAnoAnterior+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="random", random.method = "walhus")
#pFtest(reg.ef,reg.pooled) #Deu EF  para between
#phtest(reg.ef,reg.ea)  #Deu Aleat??rios
#shapiro.test(reg.pooled$residuals) #  H0: h?? normalidade dos res??duos p-valor <=0,05 rejeita H0
#shapiro.test(reg.ef$residuals) #  H0: h?? normalidade dos res??duos p-valor <=0,05 rejeita H0
#shapiro.test(reg.ea$residuals) #  H0: h?? normalidade dos res??duos p-valor <=0,05 rejeita H0
#bptest(reg.pooled) # rejeitar a hip??tese indica heterocedasticidade nos res??duos. H0: h?? homocedasticidade
#bptest(reg.ef) # rejeitar a hip??tese indica heterocedasticidade nos res??duos. H0: h?? homocedasticidade
#bptest(reg.ea) # rejeitar a hip??tese indica heterocedasticidade nos res??duos. H0: h?? homocedasticidade
#somente o modelo between apresentou normalidade dos res??duos 
#nenhum dos  modelos apresentou homocedasticidade nos res??duos
#reg.ef=plm(MetaAtuarial~mediaNTNB2050+mediaNTNB2045+mediaNTNB2035+mediaNTNB2024+mediaNTNB2020+mediaNTNB2017, data=dadosEFPC, model="within")

#reg.pooled=plm(MetaAtuarial~lag(MetaAtuarial,1)+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="pooling")
#reg.ef=plm(MetaAtuarial~lag(MetaAtuarial,1)+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="within")
#summary(reg.pooled)
#summary(reg.ef)
#pFtest(reg.ef,reg.pooled) #Deu EF p-valor <0,05
#reg.ea=plm(MetaAtuarial~lag(MetaAtuarial,1)+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="random", random.method = "walhus")
#phtest(reg.ef,reg.ea)    #Deu EF, p-valor < 0,05
#reg.ef=plm(MetaAtuarial~lag(MetaAtuarial,1)+RentObservada+RentReal_liqInfl, data=dadosEFPC, model="between")
#phtest(reg.ef,reg.ea)  #Deu EF
#pFtest(reg.ef,reg.pooled) #Deu EF p-valor <0,05, O modelo BETWEEN ?? o ***** MELHOR ****
#shapiro.test(reg.pooled$residuals) #  H0: h?? normalidade dos res??duos p-valor <=0,05 rejeita H0
#shapiro.test(reg.ef$residuals) #  H0: h?? normalidade dos res??duos p-valor <=0,05 rejeita H0
#shapiro.test(reg.ea$residuals) #  H0: h?? normalidade dos res??duos p-valor <=0,05 rejeita H0
#bptest(reg.pooled) # rejeitar H0 indica heterocedasticidade nos res??duos. H0: h?? homocedasticidade, se p-valor < 0,05, rejeita H0
#bptest(reg.ef) # rejeitar a hip??tese indica heterocedasticidade nos res??duos. H0: h?? homocedasticidade
#bptest(reg.ea) # rejeitar a hip??tese indica heterocedasticidade nos res??duos. H0: h?? homocedasticidade
#somente o modelo between apresentou normalidade dos res??duos 
#nenhum dos  modelos apresentou homocedasticidade nos res??duos