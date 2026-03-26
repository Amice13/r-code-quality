### DATASET FULL ----------------------------------------------------------------

#IMPORTAR DATASET
ifelse( !require(readxl),
       install.packages("readxl", dependencies=TRUE),
       "Pacote Instalado" ) # instalar pacote se necessário
require(readxl)
dataset_full <- read_delim("dataset_2015_2022-backup.csv", 
                           delim = ";", 
                           escape_double = FALSE, 
                           trim_ws = TRUE)

#TRATAR VARIAVEL DEPENDENCIA ADMINISTRATIVA
dataset_full$dependencia_administrativa<-ifelse( 
  dataset_full$dependencia_administrativa == "Particular",
  "Privada", dataset_full$dependencia_administrativa )


### DATASET ES -------------------------------------------------------------------

#CRIAR DATASET PARA ES COM DUMMIES
datasetES <- subset.data.frame( dataset_full, 
                                subset = dataset_full$uf == "ES" )

datasetES$Dummy_Loc <-ifelse( datasetES$localizacao == "Rural", 
                              0, 
                              1 ) # 1 = Urbano

datasetES$Dummy_DA <-ifelse( datasetES$dependencia_administrativa == "Privada", 
                             0, 
                             1 ) # 1 = Publica

datasetES$Dummy_Covid <-ifelse( datasetES$ano < 2020, 
                                0, 
                                1 ) # Ano > 2019  = 1

#ANALISAR TIPO DA VARIAVEL
ifelse( !require(visdat),
        install.packages("visdat", dependencies=TRUE),
        "Pacote Instalado" ) # instalar pacote se necessário
require(visdat)
vis_dat(datasetES, sort_type = T) #conferir o tipo de varivel

#CRIAR DATASET PARA EF (TRATAR  VARIAVEL AUSENTE)
##EF
datasetES_EF <- datasetES[c(1:9,10,14,18,22:24)]
vis_miss( datasetES_EF,
          sort_miss = F,
          cluster = T,
          warn_large_data = F ) #observar dados ausentes
datasetES_EF <-na.omit(datasetES_EF) # excluir observações com dados ausentes


### DATASET DE REFERENCIA --------------------------------------------------------

#CRIAR DATASET DE REFERENCIA
dataset<-datasetES_EF
names(dataset)[names(dataset) == "aprovacao_ef"] <- "Aprovacao"
names(dataset)[names(dataset) == "reprovacao_ef"] <- "Reprovacao"
names(dataset)[names(dataset) == "abandono_ef"] <- "Abandono"



#VERIFICAR SE HA DADOS AUSENTES
sapply(dataset, function(x) sum(is.na(x)))

#DESCREVER DADOS
summary(dataset)


### ANALISE DESCRITIVA -----------------------------------------------------------

#CRIAR OBJETOS
abandono_medio<- tapply( dataset$Abandono, 
                        dataset$ano, 
                        mean )

Estadual<- subset.data.frame( dataset, 
                             subset = dependencia_administrativa == "Estadual" )
abandono_medio_estadual<-tapply( Estadual$Abandono, 
                                Estadual$ano, 
                                mean )

Municipal<- subset.data.frame( dataset, 
                               subset = dependencia_administrativa == "Municipal" )
abandono_medio_municipal<-tapply( Municipal$Abandono, 
                                 Municipal$ano, 
                                 mean )

Privada<- subset.data.frame( dataset, 
                             subset = dependencia_administrativa == "Privada" )
abandono_medio_privada<-tapply( Privada$Abandono, 
                                Privada$ano, 
                                mean )

Publica<- subset.data.frame( dataset, 
                            subset = dependencia_administrativa != "Privada" )
abandono_medio_publica<-tapply( Publica$Abandono, 
                                Publica$ano, 
                                mean )

li = min( abandono_medio, 
         abandono_medio_estadual, 
         abandono_medio_municipal, 
         abandono_medio_privada, 
         abandono_medio_publica ); li

ls = max( abandono_medio, 
          abandono_medio_estadual, 
          abandono_medio_municipal, 
          abandono_medio_privada, 
          abandono_medio_publica ); ls

anos<-(2015:2022); anos

#TABELAS CRUZADAS
tapply( dataset$Abandono,
       dataset$localizacao,
       mean )

tapply( dataset$Abandono,
        dataset$dependencia_administrativa,
        mean )

tapply( dataset$Abandono, 
        dataset$ano,
        mean )

tapply( Privada$Abandono, 
        Privada$ano, 
        mean )

tapply( Publica$Abandono, 
        Publica$ano, 
        mean )

#GRAFICO DE LINHA
plot(y=abandono_medio, 
     x= anos, 
     type = "o", 
     ylim = c(li, ls),
     main = "Abandono escolar 2015 a 2022",
     xlab = "Anos",
     ylab = "Taxa de Abandono") #p: ponto, l: linha, o: linha e ponto
#lines(y=abandono_medio_estadual, x= anos, type = "o", lty = 2)
#lines(y=abandono_medio_municipal, x= anos, type = "o", lty = 3)
lines(y=abandono_medio_privada, x= anos, type = "o", lty = 4)
lines(y=abandono_medio_publica, x= anos, type = "o", lty = 5)
#legend(2020,1.3, c("ES", "Estadual", "Municipal", "Privada", "Publica"), lty = 1:5, cex = 0.7)
legend(2020,1.3, c("ES", "Privada", "Publica"), lty = 1:3, cex = 0.7)

#TENDENCIA COM INTERVALO DE CONFIANCA
require(ggplot2)
ggplot(, 
       aes(x= anos, y = abandono_medio) ) +
  geom_point () +
  geom_smooth(method = lm) +
  theme_bw(base_size = 18) +
  labs(x = "Ano", y = "Taxa de abandono", title = "Abandono escolar 2015 a 2022")

#BOXPLOT
require(ggplot2)
p = ggplot(dataset, aes(x= as.factor(Ano),
                        y=Abandono))
p + geom_boxplot() + 
  #geom_jitter() +
  #facet_grid(Localizacao ~., scales = "free_y", space = "free") +
  facet_wrap(~ Dependencia_Administrativa, nrow = 2, scales = "free_y") +
  #coord_flip() +
  theme_bw(base_size = 18)+
  labs(x = "Ano", y = "Taxa de abandono", title = "Abandono escolar 2015 a 2022")
  

### TESTE DE CHOW ---------------------------------------------------------------

#CRIAR Ln(1+ABANDONO)
dataset$logAbandono<-log( 1+dataset$Abandono)

#TESDE DE CHOW ORIGINAL
ifelse( !require(strucchange),
        install.packages("strucchange", dependencies=TRUE),
        "Pacote instalado" ) # instalar pacote se necessário
require(strucchange)
sctest( data = dataset,
       logAbandono ~ ano, 
       type = "Chow", 
       point = 2020 ) # ruptura em 2020

sctest( data = dataset, 
       logAbandono ~ ano + Dummy_Loc + Dummy_DA, 
       type = "Chow", 
       point = 2020 ) # ruptura em 2020 com variaveis de controle (1 = urbano e publico)


#TESDE DE CHOW COM DUMMIES
#Completa
rl_c<-lm( data = dataset, 
         logAbandono ~ ano +
           Dummy_Loc + 
           Dummy_DA + 
           Dummy_Covid ) # regressao linear
summary(rl_c)

#Periodo anterior
rl_a<-lm( data = dataset[ 1:10847, ], 
         logAbandono ~ ano +
           Dummy_Loc +
           Dummy_DA ) # regressao linear
summary(rl_a)

#Periodo posterior
rl_p<-lm( data = dataset[ 10848:16958, ], 
         logAbandono ~ ano + 
           Dummy_Loc + 
           Dummy_DA ) # regressao linear
summary(rl_p)

#VERIFICAR PRESSUPOSTOS
#normalidade
ifelse( !require(nortest),
        install.packages("nortest", dependencies=TRUE),
        "Pacote instalado" ) # instalar pacote se necessário
require(nortest)
ad.test(rl_c$residuals) # Anderson-Darling

#homocedasticidade
ifelse( !require(lmtest),
        install.packages("lmtest", dependencies=TRUE),
        "Pacote instalado" ) # instalar pacote se necessário
library(lmtest)
bptest(rl_c) # Breusch-Pagan

#multicolinearidade
ifelse( !require(car),
        install.packages("car", dependencies=TRUE),
        "Pacote instalado" ) # instalar pacote se necessário
require(car)
vif(rl_c) # VIF

#autocorrelacao
bgtest(rl_c) # Breusch-Godfrey

#outlier
ifelse( !require(olsrr),
        install.packages("olsrr", dependencies=TRUE),
        "Pacote instalado" ) # instalar pacote se necessário
require(olsrr)
ols_plot_cooksd_bar(rl_c)


#REGRESSAO ROBUSTA (CORRECAO POR CLUSTER)
coeftest(rl_c, vcov = vcovCL, cluster = ~ codigo_escola) 
coeftest(rl_a, vcov = vcovCL, cluster = ~ codigo_escola) 
coeftest(rl_p, vcov = vcovCL, cluster = ~ codigo_escola) 
