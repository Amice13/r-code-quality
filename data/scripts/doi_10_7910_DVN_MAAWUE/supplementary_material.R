library(dplyr)
library(flextable)
library(ggplot2)

# Aplicar PCA
pca_resultados <- prcomp(dados_pca_padronizados, scale = TRUE)

# Acessar as proporções da variância explicada
prop_var <- summary(pca_resultados)$importance[2, ]
prop_var_acum <- summary(pca_resultados)$importance[3, ]

# Extrair os loadings
loadings <- pca_resultados$rotation
loadings_df <- as.data.frame(loadings)

# Nomear as variáveis no conjunto de dados loadings_df
loadings_df$Label <- rownames(loadings_df)

# Mudar o nome das variáveis
loadings_df <- loadings_df %>%
  mutate(Label = case_when(
    Label == "mc_razao_verbos_passivos_complex" ~ "Passive Verb Ratio",
    Label == "mc_razao_verbos_modais_complex" ~ "Modal Verb Ratio",
    Label == "mc_entropia_palavras_comple" ~ "Word Entropy",
    Label == "mc_sentenca_media_complex" ~ "Average Sentence",
    Label == "mc_variedade_sentencas_complex" ~ "Sentence Variety",
    Label == "mc_densidade_palavras_complex" ~ "Density of Words",
    TRUE ~ Label
  ))

# Calcular as comunalidades
comunalidades <- rowSums(loadings_df[, c("PC1", "PC2")]^2)
loadings_df$Communality <- comunalidades

# Selecionar colunas
loadings_df1 <- loadings_df %>%
  select(Label, PC1, PC2, Communality)

# Criar uma nova linha para a proporção de variância explicada
prop_var_df <- data.frame(Label = c("Prop. Var. Accounted", "Cumulative Prop. Var."),
                          PC1 = c(prop_var[1], prop_var_acum[1]),
                          PC2 = c(prop_var[2], prop_var_acum[2]),
                          Communality = NA)

# Combinar as tabelas loadings_df1 e prop_var_df
loadings_df1 <- rbind(loadings_df1, prop_var_df)

# Arredondar os valores de PC1, PC2 e Communality
loadings_df1 <- loadings_df1 %>%
  mutate(PC1 = round(PC1, 2),
         PC2 = round(PC2, 2),
         Communality = round(Communality, 2))

# Renomear a coluna Label para uma coluna sem nome
loadings_df1 <- loadings_df1 %>%
  rename("  " = Label) %>%
  select("  ", PC1, PC2, Communality)

# Criar uma tabela flextable
ft <- flextable(loadings_df1)

# Ajustar a formatação da tabela
ft <- ft %>%
  fontsize(size = 10) %>%
  set_table_properties(width = 0.8) %>%
  bg(bg = "#f2f2f2", part = "header") %>%
  bg(bg = "white", part = "all") %>%
  border_outer()

# Exibir a tabela
print(ft)

# Proporção da Variância Explicada - Gráfico de Barras
prop_var_df_vis <- data.frame(
  Component = paste0("PC", 1:length(prop_var)),
  Proportion = prop_var
)

ggplot(prop_var_df_vis, aes(x = Component, y = Proportion)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(Proportion, 2)), vjust = -0.3) +
  ylim(0, 1) +
  labs(title = "Proporção da Variância Explicada por Componente",
       x = "Componente Principal",
       y = "Proporção da Variância Explicada") +
  theme_minimal()

# Comunalidades - Gráfico de Barras
comunalidades_df_vis <- loadings_df1 %>%
  filter(!is.na(Communality)) %>%
  mutate(Label = factor(`  `, levels = `  `))

ggplot(comunalidades_df_vis, aes(x = Label, y = Communality)) +
  geom_bar(stat = "identity", fill = "coral") +
  geom_text(aes(label = round(Communality, 2)), vjust = -0.3) +
  labs(title = "Comunalidades das Variáveis",
       x = "Variável",
       y = "Comunalidade") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Heatmap dos Loadings
heatmap_data <- loadings_df1 %>%
  filter(!is.na(PC1) & !is.na(PC2)) %>%
  select(`  `, PC1, PC2) %>%
  gather(Component, Loading, -`  `)

ggplot(heatmap_data, aes(x = Component, y = `  `, fill = Loading)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name="Loading") +
  labs(title = "Heatmap dos Loadings dos Componentes Principais",
       x = "Componente Principal",
       y = "Variável") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


