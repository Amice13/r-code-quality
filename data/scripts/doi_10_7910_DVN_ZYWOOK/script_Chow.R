# ------------------------------------------------------------------------------
# COMEÇAR DO ZERO --------------------------------------------------------------
# ------------------------------------------------------------------------------
rm(dados, dados_long, dataset, dataset_2019_2023, dataset_2021, 
   dataset_transformado, resultados, resultado_shapiro, resultados, rl_mc,
   rl_mc2, rl_mc3, rl_mc4,rl_mc5, rl_mc6, rl_mc7, rl_mc8, teste, variaveis, 
   variaveis_lista, variaveis_transformadas, file_path, nome, var, 
   testar_normalidade, transformar_log, residuos, analisar_residuos, dataset_hz,
   variaveis_hz, variaveis_lista_hz, dataFull, modelo_A, resultado_hz, residuos_A,
   dataset_parcial_A, df_residuos_A, resultado_residuos_A, variaveis_lista_A,
   dataset_parcial_D, df_residuos_D, modelo_D, resultado_residuos_D,
   resultado_residuos_log, variaveis_lista_D, residuos_D)

# ------------------------------------------------------------------------------
# INSTALAR PACOTES -------------------------------------------------------------
# ------------------------------------------------------------------------------

# LER DADOS DO EXCEL -----------------------------------------------------------
ifelse( !require(readxl),
        install.packages("readxl", dependencies=TRUE),
        "Pacote Instalado" )
require(readxl)

# ESTATÍSTICA DESCRITIVA -------------------------------------------------------
ifelse( !require(psych),
        install.packages("psych", dependencies=TRUE),
        "Pacote Instalado" )
require(psych)

# ANOVA-RM ---------------------------------------------------------------------
ifelse( !require(jmv),
        install.packages("jmv", dependencies=TRUE),
        "Pacote Instalado" )
require(jmv)

# TESTES DE NORMALIDADE MULTIVARIADA -------------------------------------------
ifelse( !require(MVN),
        install.packages("mvn", dependencies=TRUE),
        "Pacote Instalado" )
require(MVN)

ifelse( !require(ggplot2),
        install.packages("ggplot2", dependencies=TRUE),
        "Pacote Instalado" )
require(ggplot2)

ifelse( !require(stats),
        install.packages("stats", dependencies=TRUE),
        "Pacote Instalado" )
require(stats)

ifelse( !require(FactoMineR),
        install.packages("FactorMineR", dependencies=TRUE),
        "Pacote Instalado" )
require(FactoMineR)

ifelse( !require(car),
        install.packages("car", dependencies=TRUE),
        "Pacote Instalado" )
require(car)

ifelse( !require(gamlss),
        install.packages("gamlss", dependencies=TRUE),
        "Pacote Instalado" )
require(gamlss)

# ------------------------------------------------------------------------------
# CARREGAR DATASET -------------------------------------------------------------
# ------------------------------------------------------------------------------
require(readxl)
dataset_2021 <- read_excel("dataset_2021.xlsx")

# ------------------------------------------------------------------------------
# ESTATÍSTICA DESCRITIVA -------------------------------------------------------
# ------------------------------------------------------------------------------

describe(dataset_2021)


## TESTE DE CHOW COM DUMMIES ---------------------------------------------------
# QT_TOTAL ---------------------------------------------------------------------
rl_mc<-lm( data = dataset_2021, 
           qt_total ~ tx_Hom + 
             datas * tempo )
summary(rl_mc)


# ------------------------------------------------------------------------------
# ANÁLISE DE PRESSUPOSTOS ------------------------------------------------------
# ------------------------------------------------------------------------------

### OUTLIER
ifelse( !require(olsrr),
        install.packages("olsrr", dependencies=TRUE),
        "Pacote instalado" ) # instalar pacote se necessario
require(olsrr)
ols_plot_cooksd_bar(rl_mc)

### HOMOCEDASTICIDADE
ifelse( !require(lmtest),
        install.packages("lmtest", dependencies=TRUE),
        "Pacote instalado" ) # instalar pacote se necessario
require(lmtest)
bptest(rl_mc) # Breusch-Pagan

### AUTOCORRELACAO
bgtest(rl_mc) # Breusch-Godfrey


### NORMALIDADE
ifelse( !require(nortest),
        install.packages("nortest", dependencies=TRUE),
        "Pacote instalado" ) # instalar pacote se necessario
  require(nortest)
ad.test(rl_mc$residuals) # Anderson-Darling



### MULTICOLINEARIDADE (com dummies tende a dar valores altos)
ifelse( !require(car),
        install.packages("car", dependencies=TRUE),
        "Pacote instalado" ) # instalar pacote se necessario
require(car)
vif(rl_mc) # VIF

# ------------------------------------------------------------------------------
# CORREÇÃO - REGRESSÃO ROBUSTA -------------------------------------------------
# ------------------------------------------------------------------------------
ifelse( !require(sandwich),
        install.packages("sandwich", dependencies=TRUE),
        "Pacote instalado" ) # instalar pacote se necessario
require(sandwich)
rl_mc2<-coeftest(rl_mc, vcov = vcovCL, cluster = ~ Unidade);rl_mc2 # Cluster (correcao)

confint(rl_mc2,
        level = 0.95) # intervalo de confianca


# ------------------------------------------------------------------------------
# QT_MACONHA -------------------------------------------------------------------
# ------------------------------------------------------------------------------

## TESTE DE CHOW COM DUMMIES
rl_mc3<-lm( data = dataset_2021, 
           qt_maconha ~ tx_Hom + 
             datas * tempo )
summary(rl_mc3)

## PRESSUPOSTOS
### OUTLIER
ifelse( !require(olsrr),
        install.packages("olsrr", dependencies=TRUE),
        "Pacote instalado" ) # instalar pacote se necessario
require(olsrr)
ols_plot_cooksd_bar(rl_mc3)

### NORMALIDADE
ifelse( !require(nortest),
        install.packages("nortest", dependencies=TRUE),
        "Pacote instalado" ) # instalar pacote se necessario
require(nortest)
ad.test(rl_mc3$residuals) # Anderson-Darling

### HOMOCEDASTICIDADE
ifelse( !require(lmtest),
        install.packages("lmtest", dependencies=TRUE),
        "Pacote instalado" ) # instalar pacote se necessario
require(lmtest)
bptest(rl_mc3) # Breusch-Pagan

### AUTOCORRELACAO
bgtest(rl_mc3) # Breusch-Godfrey

### MULTICOLINEARIDADE (com dummies tende a dar valores altos)
ifelse( !require(car),
        install.packages("car", dependencies=TRUE),
        "Pacote instalado" ) # instalar pacote se necessario
require(car)
vif(rl_mc3) # VIF

## CORRECAO - REGRESSAO ROBUSTA
ifelse( !require(sandwich),
        install.packages("sandwich", dependencies=TRUE),
        "Pacote instalado" ) # instalar pacote se necessario
require(sandwich)
rl_mc4<-coeftest(rl_mc3, vcov = vcovCL, cluster = ~ Unidade);rl_mc4 # Cluster (correcao)

confint(rl_mc3,
        level = 0.95) # intervalo de confianca




# ------------------------------------------------------------------------------
# QT_COCAINA -------------------------------------------------------------------
# ------------------------------------------------------------------------------

## TESTE DE CHOW COM DUMMIES
rl_mc5<-lm( data = dataset_2021, 
            qt_cocaina ~ tx_Hom + 
              datas * tempo )
summary(rl_mc5)

## PRESSUPOSTOS
### OUTLIER
ifelse( !require(olsrr),
        install.packages("olsrr", dependencies=TRUE),
        "Pacote instalado" ) # instalar pacote se necessario
require(olsrr)
ols_plot_cooksd_bar(rl_mc5)

### NORMALIDADE
ifelse( !require(nortest),
        install.packages("nortest", dependencies=TRUE),
        "Pacote instalado" ) # instalar pacote se necessario
require(nortest)
ad.test(rl_mc5$residuals) # Anderson-Darling

### HOMOCEDASTICIDADE
ifelse( !require(lmtest),
        install.packages("lmtest", dependencies=TRUE),
        "Pacote instalado" ) # instalar pacote se necessario
require(lmtest)
bptest(rl_mc5) # Breusch-Pagan

### AUTOCORRELACAO
bgtest(rl_mc5) # Breusch-Godfrey

### MULTICOLINEARIDADE (com dummies tende a dar valores altos)
ifelse( !require(car),
        install.packages("car", dependencies=TRUE),
        "Pacote instalado" ) # instalar pacote se necessario
require(car)
vif(rl_mc5) # VIF

## CORRECAO - REGRESSAO ROBUSTA
ifelse( !require(sandwich),
        install.packages("sandwich", dependencies=TRUE),
        "Pacote instalado" ) # instalar pacote se necessario
require(sandwich)
rl_mc6<-coeftest(rl_mc5, vcov = vcovCL, cluster = ~ Unidade);rl_mc6 # Cluster (correcao)

confint(rl_mc5,
        level = 0.95) # intervalo de confianca


# ------------------------------------------------------------------------------
# QT_CRACK ---------------------------------------------------------------------
# ------------------------------------------------------------------------------

## TESTE DE CHOW COM DUMMIES
rl_mc7<-lm( data = dataset_2021, 
            qt_crack ~ tx_Hom + 
              datas * tempo )
summary(rl_mc7)

## PRESSUPOSTOS
### OUTLIER
ifelse( !require(olsrr),
        install.packages("olsrr", dependencies=TRUE),
        "Pacote instalado" ) # instalar pacote se necessario
require(olsrr)
ols_plot_cooksd_bar(rl_mc7)

### NORMALIDADE
ifelse( !require(nortest),
        install.packages("nortest", dependencies=TRUE),
        "Pacote instalado" ) # instalar pacote se necessario
require(nortest)
ad.test(rl_mc7$residuals) # Anderson-Darling

### HOMOCEDASTICIDADE
ifelse( !require(lmtest),
        install.packages("lmtest", dependencies=TRUE),
        "Pacote instalado" ) # instalar pacote se necessario
require(lmtest)
bptest(rl_mc7) # Breusch-Pagan

### AUTOCORRELACAO
bgtest(rl_mc7) # Breusch-Godfrey

### MULTICOLINEARIDADE (com dummies tende a dar valores altos)
ifelse( !require(car),
        install.packages("car", dependencies=TRUE),
        "Pacote instalado" ) # instalar pacote se necessario
require(car)
vif(rl_mc7) # VIF

'## CORRECAO - REGRESSAO ROBUSTA
ifelse( !require(sandwich),
        install.packages("sandwich", dependencies=TRUE),
        "Pacote instalado" ) # instalar pacote se necessario
require(sandwich)
rl_mc8<-coeftest(rl_mc7, vcov = vcovCL, cluster = ~ Unidade);rl_mc8 # Cluster (correcao)

confint(rl_mc7,
        level = 0.95) # intervalo de confianca

