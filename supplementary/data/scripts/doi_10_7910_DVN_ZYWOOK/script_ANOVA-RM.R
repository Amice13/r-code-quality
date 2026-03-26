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
file_path <- "C:/Users/Admin/OneDrive/Biblioteca/Bibliometria/2024-11 - K9/dissertacao2/dataset_2019-2023.xlsx"
dataset <- read_excel("dataset_2019-2023.xlsx", sheet = "dataset_RM")

# ------------------------------------------------------------------------------
# ESTATÍSTICA DESCRITIVA -------------------------------------------------------
# ------------------------------------------------------------------------------
describe(dataset)

# ------------------------------------------------------------------------------
# PLOT -------------------------------------------------------------------------
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# ANÁLISE DE PRESSUPOSTOS --- NORMALIDADE DOS RESÍDUOS -------------------------
# ------------------------------------------------------------------------------

# Criação de um modelo LM com os resíduos --------------------------------------
# Variável dependente: Período ANTERIOR -----------------------------------------

# Como são 2 períodos a serem analisados, gerar 2 data-frames parciais:

# Variáveis de interesse - PERÍODO 1 (ANTES) -----------------------------------
variaveis_lista_A <- list(
  "qt_total_A" = dataset$qt_total_A,
  "qt_maconha_A" = dataset$qt_maconha_A,
  "qt_cocaina_A" = dataset$qt_cocaina_A,
  "qt_crack_A" = dataset$qt_crack_A,
  "Tx_Hom_A" = dataset$Tx_Hom_A
  )

# Converter lista em dataframe
dataset_parcial_A <- as.data.frame(variaveis_lista_A)

# Verificar a estrutura do dataset
str(dataset_parcial_A)

# Criar modelo
modelo_A <- lm(qt_total_A ~ qt_maconha_A + qt_cocaina_A + qt_crack_A + Tx_Hom_A,
               data = dataset_parcial_A)

# Extrair os resíduos do modelo
residuos_A <- residuals(modelo_A)

# Gerar data-frame dos resíduos
df_residuos_A <- cbind(dataset_parcial_A, residuos = residuos_A)

# Teste de Henze-Zirkler (normalidade multivariada)
resultado_residuos_A <- mvn(data = as.data.frame(df_residuos_A), mvnTest = "hz")

# Exibir o resultado
print(resultado_residuos_A$multivariateNormality)

# Plotar o QQ-plot dos resíduos para visualização
ggplot(data.frame(residuos = residuos_A), aes(sample = residuos_A)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ-Plot dos Resíduos") +
  theme_minimal()

# Variáveis de interesse - PERÍODO 2 (DEPOIS) ----------------------------------
variaveis_lista_D <- list(
  "qt_total_D" = dataset$qt_total_D,
  "qt_maconha_D" = dataset$qt_maconha_D,
  "qt_cocaina_D" = dataset$qt_cocaina_D,
  "qt_crack_D" = dataset$qt_crack_D,
  "Tx_Hom_D" = dataset$Tx_Hom_D
)

# Converter lista em dataframe
dataset_parcial_D <- as.data.frame(variaveis_lista_D)

# Verificar a estrutura do dataset
str(dataset_parcial_D)

# Criar modelo
modelo_D <- lm(qt_total_D ~ qt_maconha_D + qt_cocaina_D + qt_crack_D + Tx_Hom_D,
               data = dataset_parcial_D)

# Extrair os resíduos do modelo
residuos_D <- residuals(modelo_D)

# Gerar data-frame dos resíduos
df_residuos_D <- cbind(dataset_parcial_D, residuos = residuos_D)

# Teste de Henze-Zirkler (normalidade multivariada)
resultado_residuos_D <- mvn(data = as.data.frame(df_residuos_D), mvnTest = "hz")

# Exibir o resultado
print(resultado_residuos_D$multivariateNormality)

# Plotar o QQ-plot dos resíduos para visualização
ggplot(data.frame(residuos = residuos_D), aes(sample = residuos_D)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ-Plot dos Resíduos") +
  theme_minimal()


# ------------------------------------------------------------------------------
# TRANSFORMAÇÃO DE VARIÁVEIS --- LOG NATURAL -----------------------------------
# ------------------------------------------------------------------------------
transformar_log <- function(variavel, nome) {
  
  # Adicionar uma constante pequena para evitar log(0)
  if (any(variavel <= 0, na.rm = TRUE)) {
    variavel <- variavel + 1
    cat("Nota: Adicionado 1 aos valores de", nome, "para evitar log(0).\n")
  }
  
  # Aplicar transformação logarítmica
  log_variavel <- log(variavel)
  return(log_variavel)
}

# Aplicar transformação logarítmica às variáveis de interesse
variaveis_transformadas <- list()
variaveis_lista <- list(
  "qt_total_A" = dataset$qt_total_A,
  "qt_total_D" = dataset$qt_total_D,
  "qt_maconha_A" = dataset$qt_maconha_A,
  "qt_maconha_D" = dataset$qt_maconha_D,
  "qt_cocaina_A" = dataset$qt_cocaina_A,
  "qt_cocaina_D" = dataset$qt_cocaina_D,
  "qt_crack_A" = dataset$qt_crack_A,
  "qt_crack_D" = dataset$qt_crack_D,
  "Tx_Hom_A" = dataset$Tx_Hom_A,
  "Tx_Hom_D" = dataset$Tx_Hom_D
)

for (nome in names(variaveis_lista)) {
  variaveis_transformadas[[nome]] <- transformar_log(variaveis_lista[[nome]], nome)
}

# Converter lista em dataframe
dataset_transformado <- as.data.frame(variaveis_transformadas)

# Exibir primeiras linhas das variáveis transformadas
print(head(dataset_transformado))

# Repetição do Teste de Henze-Zirkler após a transformação dos dados -----------
resultado_residuos_log <- mvn(data = as.data.frame(dataset_transformado), mvnTest = "hz")

# Exibir o resultado
print(resultado_residuos_log$multivariateNormality)


# ------------------------------------------------------------------------------
# QT_TOTAL ---------------------------------------------------------------------
# ------------------------------------------------------------------------------
anovaRM (
  data = dataset_transformado,
  rm = list (
    list (
      label = "apreensoes_total",
      levels = c ("2019", "2023" ) ) ),
  rmCells = list (
    list (
      measure = "qt_total_A",
      cell = "2019" ),
    list (
      measure = "qt_total_D",
      cell = "2023" ) ),
  cov = vars(Tx_Hom_A, Tx_Hom_D),
  effectSize = "eta", #c("eta", "ges", "partEta", "omega")
  rmTerms = ~ apreensoes_total,
  bsTerms = ~ Tx_Hom_A + Tx_Hom_D,
  qq = T,
  emMeans = ~ apreensoes_total,
  emmTables = T,
  #emmPlotData = T,
  groupSumm = T )

# ------------------------------------------------------------------------------
# QT_MACONHA -------------------------------------------------------------------
# ------------------------------------------------------------------------------

anovaRM (
  data = dataset_transformado,
  rm = list (
    list (
      label = "apreensoes_maconha",
      levels = c ("2019", "2023" ) ) ),
  rmCells = list (
    list (
      measure = "qt_maconha_A",
      cell = "2019" ),
    list (
      measure = "qt_maconha_D",
      cell = "2023" ) ),
  cov = vars(Tx_Hom_A, Tx_Hom_D),
  effectSize = "eta", #c("eta", "ges", "partEta", "omega")
  rmTerms = ~ apreensoes_maconha,
  bsTerms = ~ Tx_Hom_A + Tx_Hom_D,
  qq = T,
  emMeans = ~ apreensoes_maconha,
  emmTables = T,
  #emmPlotData = T,
  groupSumm = T )

# ------------------------------------------------------------------------------
# QT_COCAINA -------------------------------------------------------------------
# ------------------------------------------------------------------------------

anovaRM (
  data = dataset_transformado,
  rm = list (
    list (
      label = "apreensoes_cocaina",
      levels = c ("2019", "2023" ) ) ),
  rmCells = list (
    list (
      measure = "qt_cocaina_A",
      cell = "2019" ),
    list (
      measure = "qt_cocaina_D",
      cell = "2023" ) ),
  cov = vars(Tx_Hom_A, Tx_Hom_D),
  effectSize = "eta", #c("eta", "ges", "partEta", "omega")
  rmTerms = ~ apreensoes_cocaina,
  bsTerms = ~ Tx_Hom_A + Tx_Hom_D,
  qq = T,
  emMeans = ~ apreensoes_cocaina,
  emmTables = T,
  #emmPlotData = T,
  groupSumm = T )

# ------------------------------------------------------------------------------
# QT_CRACK ---------------------------------------------------------------------
# ------------------------------------------------------------------------------

anovaRM (
  data = dataset_transformado,
  rm = list (
    list (
      label = "apreensoes_crack",
      levels = c ("2019", "2023" ) ) ),
  rmCells = list (
    list (
      measure = "qt_crack_A",
      cell = "2019" ),
    list (
      measure = "qt_crack_D",
      cell = "2023" ) ),
  cov = vars(Tx_Hom_A, Tx_Hom_D),
  effectSize = "eta", #c("eta", "ges", "partEta", "omega")
  rmTerms = ~ apreensoes_crack,
  bsTerms = ~ Tx_Hom_A + Tx_Hom_D,
  qq = T,
  emMeans = ~ apreensoes_crack,
  emmTables = T,
  #emmPlotData = T,
  groupSumm = T )