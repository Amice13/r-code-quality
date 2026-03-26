# Analise do trabalho 


# liberando os pacotes

library(readxl)
library(dplyr)
library(readr)
library(janitor)
library(knitr)   # Para a função kable()
library(kableExtra)
library(stargazer)
install.packages("janitor")
library(forcats)
library(tidyr) 

# Puxando o banco 

Banco_caio <- read_excel("C:/Users/Caio/Downloads/Banco_caio.xlsx")

# limpando o nome das variáveis 

Banco_caio <- clean_names(Banco_caio)

# Converter as variáveis 0/1 para fatores com rótulos para melhor legibilidade nas tabelas
Banco_caio$processo_criminal <- factor(Banco_caio$processo_criminal, levels = c(0, 1), labels = c('Não', 'Sim'))
Banco_caio$impeachment <- factor(Banco_caio$impeachment, levels = c(0, 1), labels = c('Não', 'Sim'))
Banco_caio$cpi <- factor(Banco_caio$cpi, levels = c(0, 1), labels = c('Não', 'Sim'))
Banco_caio$pec <- factor(Banco_caio$pec, levels = c(0, 1), labels = c('Não', 'Sim'))

#Definir variáveis dependentes
dependent_vars <- c('impeachment', 'cpi', 'pec')

# Preparar uma lista para armazenar os dados resumidos para a tabela combinada
combined_crosstab_summary <- list()

for (dep_var_name in dependent_vars) {
  # Obter as contagens totais (N) para cada grupo de processo_criminal
  n_nao <- sum(Banco_caio$processo_criminal == 'Não')
  n_sim <- sum(Banco_caio$processo_criminal == 'Sim')
  
  # Calcular a tabela cruzada com porcentagens normalizadas por linha (status de processo_criminal)
  crosstab_raw_percent <- Banco_caio %>%
    tabyl(processo_criminal, !!sym(dep_var_name)) %>% # !!sym() é para usar o nome da variável como um símbolo
    adorn_percentages('row') # Calcula porcentagens por linha, mantendo-as como números
  
  
  # Extrair a porcentagem 'Sim' (numérica, 0-1) para os grupos 'Não' e 'Sim' de processo_criminal
  # Multiplicamos por 100 aqui para obter o percentual (ex: 0.0447 -> 4.47)
  perc_nao_processo_sim_numeric <- (crosstab_raw_percent %>% filter(processo_criminal == 'Não') %>% pull(Sim)) * 100
  perc_sim_processo_sim_numeric <- (crosstab_raw_percent %>% filter(processo_criminal == 'Sim') %>% pull(Sim)) * 100
  
  # Adicionar os dados à lista, usando sprintf para formatar o número e o N
  combined_crosstab_summary[[dep_var_name]] <- data.frame(
    Variável_Dependente = dep_var_name,
    Processo_Criminal_Nao_N = sprintf('%.2f%% (N=%d)', perc_nao_processo_sim_numeric, n_nao),
    Processo_Criminal_Sim_N = sprintf('%.2f%% (N=%d)', perc_sim_processo_sim_numeric, n_sim),
    stringsAsFactors = FALSE # Evita que as strings sejam convertidas em fatores automaticamente
  )
}

# Combinar todos os data frames de resumo em um só
summary_df <- bind_rows(combined_crosstab_summary)

# Substituir nomes das variáveis por rótulos mais descritivos para a apresentação
summary_df$Variável_Dependente <- recode(summary_df$Variável_Dependente,
                                         'impeachment' = 'Pedido de Impeachment',
                                         'cpi' = 'Proposição de CPI',
                                         'pec' = 'Proposição de PEC')

# Definir nomes das colunas para a tabela final
colnames(summary_df) <- c("Variável Dependente",
                          "Sem Processo Criminal (N=649)",
                          "Com Processo Criminal (N=77)")

# Criar a tabela formatada
# Exemplo de saída em Markdown:
# Você pode mudar 'format = "markdown"' para 'format = "latex"' para saída LaTeX
# ou 'format = "html"' para saída HTML, dependindo do seu ambiente de publicação.
formatted_table <- summary_df %>%
  kable(
    format = "markdown", # Pode ser "latex", "html", etc.
    caption = "Tabela 2: Percentual de Parlamentares com Reação (Sim) ao STF",
    align = "lcr" # Alinhamento das colunas
  ) %>%
  kable_styling(
    bootstrap_options = "striped", # Opções de estilo (ex: linhas zebradas)
    full_width = F, # A tabela não ocupa a largura total da página
    position = "left" # Posição da tabela
  ) %>%
  footnote(
    general = "Os percentuais indicam a proporção de parlamentares que 'Sim' reagiram dentro de cada grupo de 'Processo Criminal'.",
    general_title = "Nota:",
    symbol_manual = "" # Remove símbolos padrão se não houver notas específicas
  )

# Imprimir a tabela formatada
print(formatted_table)


# 1. Carregar o conjunto de dados (com tratamento de erro aprimorado)


Banco_caio <- tryCatch({
  read_excel(Banco_caio.xlsx)
}, error = function(e) {
  message("Erro ao carregar o arquivo CSV: ", e$message)
  message("Por favor, verifique se 'Banco_caio.xlsx - Planilha2.csv' está no diretório correto e o nome está exato.")
  stop("O script foi interrompido devido a um erro no carregamento do arquivo.") # Interrompe o script
})

# Limpar nomes das colunas
Banco_caio <- clean_names(Banco_caio)

# Converter as variáveis 0/1 para fatores com rótulos para melhor legibilidade
# Assegurar que os fatores sempre tenham ambos os níveis 'Não' e 'Sim'
Banco_caio$impeachment <- factor(Banco_caio$impeachment, levels = c(0, 1), labels = c('Não', 'Sim'))
Banco_caio$cpi <- factor(Banco_caio$cpi, levels = c(0, 1), labels = c('Não', 'Sim'))
Banco_caio$pec <- factor(Banco_caio$pec, levels = c(0, 1), labels = c('Não', 'Sim'))

# Definir as variáveis dependentes para resumir
dependent_variables_to_summarize <- c('impeachment', 'cpi', 'pec')

# Preparar uma lista para armazenar os data frames de frequência formatados
all_dep_freq_data_formatted <- list() # Esta linha inicializa a lista como vazia.

for (var_name in dependent_variables_to_summarize) {
  # Calcular a tabela de frequência para a variável atual
  counts_table <- table(Banco_caio[[var_name]], useNA = "no")
  
  # Garantir que ambas as categorias 'Não' e 'Sim' estejam no resultado, mesmo se tiverem 0
  all_categories_in_var <- levels(Banco_caio[[var_name]])
  full_counts <- setNames(rep(0, length(all_categories_in_var)), all_categories_in_var)
  full_counts[names(counts_table)] <- counts_table
  
  # Calcular proporções baseadas em full_counts
  full_props <- prop.table(full_counts)
  
  # Criar um dataframe temporário usando tibble (mais moderno e seguro que data.frame)
  temp_formatted_df <- tibble(
    Variável = var_name,
    Categoria = all_categories_in_var, # Explicitamente usar os níveis do fator
    Frequência = as.numeric(full_counts),
    Percentual = paste0(round(as.numeric(full_props) * 100, 2), '%')
  )
  
  # Adicionar o dataframe temporário à lista
  all_dep_freq_data_formatted[[var_name]] <- temp_formatted_df
}

# Combinar todos os data frames temporários em um só
# Este passo só será executado se o loop acima preencher a lista.
combined_dep_freq_table_final <- bind_rows(all_dep_freq_data_formatted)

# Substituir nomes das variáveis por rótulos mais descritivos
combined_dep_freq_table_final$Variável <- recode(combined_dep_freq_table_final$Variável,
                                                 'impeachment' = 'Pedido de Impeachment',
                                                 'cpi' = 'Proposição de CPI',
                                                 'pec' = 'Proposição de PEC')

# --- Gerar a tabela para publicação com kable/kableExtra ---

# Definir nomes das colunas para a tabela final para o kable
colnames(combined_dep_freq_table_final) <- c('Variável', 'Categoria', 'Frequência', 'Percentual (%)')

# Criar a tabela formatada (ex: em Markdown - pode mudar para 'html', 'latex', etc.)
formatted_table_dep_freq <- combined_dep_freq_table_final %>%
  kable(
    format = 'markdown', # Escolha o formato de saída (ex: 'markdown', 'html', 'latex')
    caption = 'Tabela 3: Frequência e Percentual dos Fenômenos de Reação ao STF',
    align = c('l', 'l', 'r', 'r') # Alinhamento das colunas
  ) %>%
  kable_styling(
    bootstrap_options = 'striped',
    full_width = FALSE,
    position = 'left'
  ) %>%
  footnote(
    general = paste0('N = ', nrow(Banco_caio), ' (Total de Observações)'),
    general_title = 'Nota:',
    symbol_manual = ''
  )

# Imprimir a tabela formatada
print(formatted_table_dep_freq)




#### realizando teste de qui-quadrado e o teste de regressão


independent_var <- 'processo_criminal'
dependent_vars <- c('impeachment', 'cpi', 'pec')

# Preparar uma lista para armazenar os resultados do Qui-Quadrado
chi_squared_results <- list()



# Realizar o teste Qui-Quadrado para cada variável dependente

for (dep_var in dependent_vars) {
  # Criar a tabela de contingência
  contingency_table <- table(Banco_caio[[independent_var]], Banco_caio[[dep_var]])
  
  # Realizar o teste Qui-Quadrado
  chi2_test_result <- chisq.test(contingency_table)
  
  # Armazenar os resultados
  chi_squared_results[[dep_var]] <- tibble(
    `Variável Dependente` = recode(dep_var,
                                   'impeachment' = 'Pedido de Impeachment',
                                   'cpi' = 'Proposição de CPI',
                                   'pec' = 'Proposição de PEC'),
    `Qui-Quadrado (χ²)` = round(chi2_test_result$statistic, 2),
    `Graus de Liberdade (df)` = chi2_test_result$parameter,
    `Valor-p` = round(chi2_test_result$p.value, 3)
  )
}

# Combinar todos os resultados em um único dataframe
chi_squared_summary_table <- bind_rows(chi_squared_results)

# --- Gerar a tabela para publicação com kable/kableExtra ---
formatted_chi_squared_table <- chi_squared_summary_table %>%
  kable(
    format = 'markdown', # Escolha o formato de saída (ex: 'markdown', 'html', 'latex')
    caption = 'Tabela 4: Resultados do Teste Qui-Quadrado (Processo Criminal vs. Reações ao STF)',
    align = 'lccc' # Alinhamento das colunas
  ) %>%
  kable_styling(
    bootstrap_options = 'striped',
    full_width = FALSE,
    position = 'left'
  ) %>%
  footnote(
    general = paste0('N = ', nrow(Banco_caio), ' (Total de Observações)'),
    general_title = 'Nota:',
    symbol_manual = ''
  )

# Imprimir a tabela formatada
print(formatted_chi_squared_table)


# --- TRATAMENTO DAS VARIÁVEIS DE CONTROLE PARA REGRESSÃO ---

# 1. Variável 'Casa': Converter para fator com rótulos claros
Banco_caio$casa <- factor(Banco_caio$casa, levels = c(1, 2), labels = c('Senado', 'Câmara dos Deputados'))

# 2. Recodificar a variável 'governo' baseando-se no 'partido'
# Primeiro, garantir que a coluna 'partido' seja tratada como fator ou texto para comparação
Banco_caio$partido <- as.character(Banco_caio$partido) # Garante que é texto para comparação

# Criar a nova variável 'governo_recod'
Banco_caio <- Banco_caio %>%
  mutate(
    governo_recod = case_when(
      partido == "PL" ~ "Governo",
      TRUE ~ "Oposição" # Todos os outros partidos (incluindo NAs originais) são Oposição
    )
  )

# Converter a nova variável 'governo_recod' para fator
# Definir os níveis explicitamente para garantir 'Governo' é a referência (ou como preferir)
Banco_caio$governo_recod <- factor(Banco_caio$governo_recod, levels = c("Governo", "Oposição"))

# --- MODELOS DE REGRESSÃO LOGÍSTICA COM A NOVA VARIÁVEL 'governo_recod' ---

# Modelo 1: Pedido de Impeachment
model_impeachment <- glm(impeachment ~ processo_criminal + casa + governo_recod,
                         data = Banco_caio,
                         family = binomial(link = "logit"))

# Modelo 2: Proposição de CPI
model_cpi <- glm(cpi ~ processo_criminal + casa + governo_recod,
                 data = Banco_caio,
                 family = binomial(link = "logit"))

# Modelo 3: Proposição de PEC
model_pec <- glm(pec ~ processo_criminal + casa + governo_recod,
                 data = Banco_caio,
                 family = binomial(link = "logit"))

# --- EXIBIR RESULTADOS DA REGRESSÃO COM STARGZER ---
cat("\n--- Resultados da Regressão Logística (em texto no console) ---\n")
stargazer(model_impeachment, model_cpi, model_pec,
          type = "text",
          title = "Resultados da Regressão Logística: Reação Parlamentar ao STF",
          dep.var.labels = c("Impeachment", "CPI", "PEC"),
          covariate.labels = c("Processo Criminal (Sim)",
                               "Casa (Câmara dos Deputados)",
                               "Oposição (Ref: Governo)"), # Este label se refere à dummy 'Oposição'
          out = "regressao_logistica_recodificada.html" # Exemplo de como salvar em HTML
)

# --- EXIBIR RESULTADOS DA REGRESSÃO COM STARGZER EM RAZÕES DE CHANCES (CORREÇÃO) ---
cat("\n--- Resultados da Regressão Logística em Razões de Chances (em texto no console) ---\n")
stargazer(model_impeachment, model_cpi, model_pec,
          type = "text", # Define a saída para o console
          title = "Resultados da Regressão Logística (Odds Ratios): Reação Parlamentar ao STF",
          dep.var.labels = c("Impeachment", "CPI", "PEC"),
          apply.coef = exp, # <<< AQUI: Usando 'apply.coef = exp'
          # Se você quiser suprimir as estrelas de significância (normalmente não é o caso), use:
          # star.cutoffs = NULL
          # Se o erro persistir, pode ser que um dos modelos tenha problemas de convergência (e.g., coeficientes NA/Inf)
          # Examine summary(model_cpi) por exemplo.
          
          covariate.labels = c("Processo Criminal (Sim)",
                               "Casa (Câmara dos Deputados)",
                               "Oposição (Ref: Governo)"),
          out = "regressao_logistica_odds_ratios.html" # Salvar em HTML
)


               # --- RECATEGORIZAÇÃO DA VARIÁVEL 'PARTIDO' ---
# Usando fct_lump_min() para agrupar partidos com menos de 4 ocorrências (3 ou menos)
Banco_caio <- Banco_caio %>%
  mutate(
    partido_recod = fct_lump_min(as.factor(partido), min = 4, other_level = "Outros")
  )

# Ordenar os níveis de 'partido_recod' pela frequência geral (do mais frequente para o menos)
Banco_caio$partido_recod <- fct_infreq(Banco_caio$partido_recod)


# --- CRIAÇÃO DA TABELA COMBINADA DE PARTIDOS RECATEGORIZADOS X VARIÁVEIS DEPENDENTES ---

# Função auxiliar para preparar os dados de cada variável dependente
prepare_dep_var_data <- function(df, dep_var_col, prefix) {
  df %>%
    count(partido_recod, !!sym(dep_var_col)) %>%
    group_by(partido_recod) %>%
    mutate(
      total_por_partido = sum(n), # Total de parlamentares por partido
      percent = n / total_por_partido * 100 # Porcentagem dentro do partido
    ) %>%
    ungroup() %>%
    # Pivotar para formato wide: agora os nomes serão n_Não, n_Sim, percent_Não, percent_Sim
    pivot_wider(
      names_from = !!sym(dep_var_col),
      values_from = c(n, percent),
      values_fill = 0 # Preencher com 0 onde não há ocorrência
    ) %>%
    # Formatar as colunas de contagem e percentagem para o display final
    # CORREÇÃO AQUI: Acessando n_Não, percent_Não, etc., e depois aplicando o prefixo
    mutate(
      !!paste0(prefix, "_Não") := paste0(n_Não, " (", sprintf("%.2f", percent_Não), "%)"),
      !!paste0(prefix, "_Sim") := paste0(n_Sim, " (", sprintf("%.2f", percent_Sim), "%)")
    ) %>%
    # Selecionar apenas as colunas formatadas e partido_recod
    select(partido_recod, starts_with(prefix))
}

# Preparar dados para cada variável dependente
impeachment_data <- prepare_dep_var_data(Banco_caio, "impeachment", "Impeachment")
cpi_data <- prepare_dep_var_data(Banco_caio, "cpi", "CPI")
pec_data <- prepare_dep_var_data(Banco_caio, "pec", "PEC")

# Juntar todos os dataframes pelo partido_recod
combined_dep_tables <- impeachment_data %>%
  full_join(cpi_data, by = "partido_recod") %>%
  full_join(pec_data, by = "partido_recod") %>%
  # Garantir a ordem dos partidos pela frequência geral, caso join altere
  mutate(partido_recod = fct_relevel(partido_recod, levels(Banco_caio$partido_recod))) %>%
  arrange(partido_recod) # Arrange by the ordered factor levels


# --- TABELA 5: PARTIDOS RECATEGORIZADOS X VARIÁVEIS DEPENDENTES (COMBINADA) ---
cat("\n--- Tabela 5: Partidos Recategorizados X Reações ao STF (Combinada) ---\n")

# Ajustar os nomes das colunas para o kable
colnames(combined_dep_tables) <- c(
  "Partido",
  "Impeachment (Não)", "Impeachment (Sim)",
  "CPI (Não)", "CPI (Sim)",
  "PEC (Não)", "PEC (Sim)"
)

formatted_combined_table <- combined_dep_tables %>%
  kable(
    format = "markdown", # Ou "html", "latex"
    caption = "Tabela 5: Frequência e Percentual de Reações ao STF por Partido (Recategorizado)",
    # CORREÇÃO AQUI: 'align' com 7 caracteres individuais
    align = c("l", "c", "c", "c", "c", "c", "c")
  ) %>%
  add_header_above(c(" " = 1, "Impeachment" = 2, "CPI" = 2, "PEC" = 2)) %>% # Cabeçalho agrupado
  kable_styling(
    bootstrap_options = "striped", full_width = FALSE, position = "left"
  ) %>%
  footnote(
    general = paste0("N = ", nrow(Banco_caio), " (Total de Observações). Partidos com 3 ou menos ocorrências agrupados como 'Outros'."),
    general_title = "Nota:",
    symbol_manual = ""
  )

print(formatted_combined_table)


save_kable(formatted_combined_table, file = "tabela5_partidos_dependentes_combinada.html")


# --- TABELA 6: PARTIDOS RECATEGORIZADOS X VARIÁVEL INDEPENDENTE (PROCESSO CRIMINAL) ---
cat("\n--- Tabela 6: Partidos Recategorizados X Processo Criminal ---\n")

crosstab_processo_criminal <- Banco_caio %>%
  tabyl(partido_recod, processo_criminal) %>%
  adorn_percentages("row") %>% # Percentuais por linha (dentro de cada partido)
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

formatted_table_processo_criminal <- crosstab_processo_criminal %>%
  kable(
    format = "markdown",
    caption = "Tabela 6: Partidos Recategorizados X Processo Criminal",
    align = "lcc"
  ) %>%
  kable_styling(
    bootstrap_options = "striped", full_width = FALSE, position = "left"
  ) %>%
  footnote(
    general = paste0("N = ", nrow(Banco_caio), " (Total de Observações). Partidos com 3 ou menos ocorrências agrupados como 'Outros'."),
    general_title = "Nota:",
    symbol_manual = ""
  )

print(formatted_table_processo_criminal)
save_kable(formatted_table_processo_criminal, file = "tabela _processo_criminal.html")
