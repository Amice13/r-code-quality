# ==============================================================================
# SCRIPT DE PREPARAÇÃO DE DADOS
# Artigo: "Patrimônio de Deputados e sua Atuação Legislativa (2023-2025)"
# Autor: Pedro Lenhagui Bergamaschi (UFPR)
# Data: Outubro de 2025
# ==============================================================================

# 1. PACOTES ===================================================================
library(readr)
library(dplyr)
library(stringr)
library(stringi)
library(ggplot2)

# Configuração para evitar notação científica
options(scipen = 999)


# 2. DEFINIR FUNÇÕES AUXILIARES ===============================================

# Função para padronização de nomes
padronizar_nome <- function(nome) {
  nome |>
    str_to_upper() |>                      
    stri_trans_general("Latin-ASCII") |>   
    str_trim()                             
}


# 3. IMPORTAR DADOS BRUTOS =====================================================

# 3.1. Dados da Câmara dos Deputados (CD) (57ª Legislatura)
deputados <- read_csv2("deputados.csv", 
                       locale = locale(encoding = "UTF-8"))

# Filtrar apenas deputados da 57ª legislatura (incluindo suplentes)
deputados_57 <- deputados |> 
  filter(idLegislaturaFinal == 57) |>
  select(nome, idLegislaturaInicial, idLegislaturaFinal, nomeCivil)

# 3.2. Dados do Tribunal Superior Eleitoral (TSE) - Candidaturas 2022
dadoscand22 <- read_csv2("consulta_cand_2022_BRASIL.csv", 
                         locale = locale(encoding = "Latin1"))

# 3.3. Dados do TSE - Bens declarados pelos candidatos
benscand22 <- read_csv2("bem_candidato_2022_BRASIL.csv", 
                        locale = locale(encoding = "Latin1"))


# 4. PADRONIZAÇÃO DE NOMES PARA CRUZAMENTO =====================================

# Dataset da CD
deputados_57 <- deputados_57 |>
  mutate(NM_CANDIDATO = padronizar_nome(NM_CANDIDATO))

# Dataset do TSE
dadoscand22 <- dadoscand22 |>
  mutate(NM_CANDIDATO = padronizar_nome(NM_CANDIDATO))


# 5. CRUZAR DADOS DOS DEPUTADOS COM CANDIDATURAS ==============================

# Merge mantendo todos os deputados da 57ª legislatura
dados_dep_57 <- merge(deputados_57, dadoscand22, 
                      by = "NM_CANDIDATO", 
                      all.x = TRUE)

# Manter apenas a candidatura a deputado federal
dados_dep_57 <- dados_dep_57 |>
  filter(DS_CARGO == "DEPUTADO FEDERAL")


# 6. AGREGAR PATRIMÔNIO POR CANDIDATO =========================================

# Calcular patrimônio total de cada candidato
bens_cand_agrupado <- benscand22 |>
  group_by(SQ_CANDIDATO) |>
  summarise(total_bens = sum(VR_BEM_CANDIDATO, na.rm = TRUE)) |>
  arrange(desc(total_bens))


# 7. INTEGRAR PATRIMÔNIO AOS DADOS DOS DEPUTADOS ==============================

# Merge final com dados de patrimônio
dep_pat_dep_57 <- merge(dados_dep_57, bens_cand_agrupado, 
                        by = "SQ_CANDIDATO", 
                        all.x = TRUE)

# Substituir NA por 0 para deputados sem declaração de bens
dep_pat_dep_57 <- dep_pat_dep_57 |>
  mutate(total_bens = ifelse(is.na(total_bens), 0, total_bens))


# 8. IDENTIFICAR AMOSTRA: 1% MAIS RICO ========================================

# Calcular quantis de patrimônio
quantis_patrimonio <- quantile(dep_pat_dep_57$total_bens, 
                               probs = c(0.5, 0.75, 0.9, 0.95, 0.99, 0.999), 
                               na.rm = TRUE)

print(quantis_patrimonio)

# Extrair 1% mais rico (P99)
limite_p99 <- quantile(dep_pat_dep_57$total_bens, probs = 0.99, na.rm = TRUE)

fulld1 <- dep_pat_dep_57 |>
  filter(total_bens >= limite_p99) |>
  arrange(desc(total_bens))


# 9. EXTRAIR DADOS INDIVIDUAIS DA AMOSTRA =====================================

eunicio_oliveira <- bens_cand_agrupado %>%
  filter(SQ_CANDIDATO == "60001643110")

write.csv(eunicio_oliveira, file = "bens-eunicio-oliveira-2022.csv")

jadyel_silva_alencar <- bens_cand_agrupado %>%
  filter(SQ_CANDIDATO == "180001610760")

write.csv(jadyel_silva_alencar, file = "bens-jadiel-silva-alencar-2022.csv")

newton_bonin <- bens_cand_agrupado %>%
  filter(SQ_CANDIDATO == "160001654658")

write.csv(newton_bonin, file = "bens-newton-bonin-2022.csv")

hercilio_araujo_diniz_filho <- bens_cand_agrupado %>%
  filter(SQ_CANDIDATO == "130001668912")

write.csv(hercilio_araujo_diniz_filho, file = "bens-hercilio-a-d-filho-2022.csv")

jose_nl_merces <- bens_cand_agrupado %>%
  filter(SQ_CANDIDATO == "90001700974")

write.csv(jose_nl_merces, file = "bens-jose-merces-2022.csv")

duda_b_ramos <- bens_cand_agrupado %>%
  filter(SQ_CANDIDATO = "230001602809")

write.csv(duda_b_ramos, file = "bens-duda-ramos-2022.csv")


# ==============================================================================
# NOTAS METODOLÓGICAS
# ==============================================================================
# 
# 1. PADRONIZAÇÃO DE NOMES:
#    - Remoção de acentos e conversão para maiúsculas para facilitar merge
#    - Alguns casos exigiram ajustes manuais devido a variações nos registros
#
# 2. TRATAMENTO DE DUPLICATAS:
#    - Mantida apenas a candidatura a deputado federal
#
# 3. CÁLCULO DE PATRIMÔNIO:
#    - Soma simples dos valores declarados (VR_BEM_CANDIDATO)
#    - Não foram aplicadas correções monetárias
#
# 4. DEFINIÇÃO DA AMOSTRA:
#    - Percentil 99 (P99) utilizado como critério para "1% mais rico"
#    - n final = 6 deputados
#    - Patrimônio entre R$ 48,4 milhões e R$ 158,1 milhões
#
# 5. DADOS COMPLEMENTARES:
#    - Redes de empresas: extraídos manualmente via Receita Federal
#    - Propriedades rurais: consultadas no SIGEF/INCRA
#    - Proposições legislativas: extraídas via API da Câmara dos Deputados
#    - Contratos públicos: consultados via API do Portal da Transparência
#
# ==============================================================================