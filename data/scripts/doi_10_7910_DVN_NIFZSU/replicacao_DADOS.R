# Script de replicação do artigo de Santos, DADOS vol. 68 n. 4




# Setup ------------------------------------------------------------------------

# install.packages("remotes") 
# remotes::install_github("vsntos/senatebR")

# Verificar e apontar erros em: https://github.com/vsntos/senatebR?tab=readme-ov-file


# Replicação -------------------------------------------------------------------

library(senatebR)
library(tidyverse)

df_senadores_legislatura <- obter_dados_senadores_legislatura(47, 56)

df_senadores_legislatura_56 <- obter_dados_senadores_legislatura(56, 56)

dados_multi <- extrair_pronunciamentos_multi(codigos_parlamentares = df_senadores_legislatura_56$IdentificacaoParlamentar.CodigoParlamentar, anos = c(2020:2024))

dados_multi <- dados_multi %>% 
  separate(Partido_UF, into = c("Partido", "UF"), sep = "/")

pronunciamentos_por_ano <- dados_multi %>%
  count(Ano)

# Plotar

ggplot(pronunciamentos_por_ano, aes(x = factor(Ano), y = n, fill = factor(Ano))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  labs(
    title = "Distribuição de Pronunciamentos por Ano",
    x = "Ano",
    y = "Número de Pronunciamentos"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Distribuição Mensal

# Convertendo a coluna de Data_Pronunciamento para Date
dados_multi$Data_Pronunciamento <- as.Date(dados_multi$Data_Pronunciamento, format = "%d/%m/%Y")

# Extraindo o mês
dados_multi$Mes <- format(dados_multi$Data_Pronunciamento, "%Y-%m")

# Contagem de pronunciamentos por mês
pronunciamentos_por_mes <- dados_multi %>%
  count(Mes)

# Plotar a distribuição por mes
ggplot(pronunciamentos_por_mes, aes(x = factor(Mes), y = n, )) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  labs(
    title = "Distribuição de Pronunciamentos por Mês",
    x = "Mês",
    y = "Número de Pronunciamentos"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Ajusta a orientação do texto no eixo X
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")  # Centraliza e estiliza o título
  )

# Contagem de tipos de pronunciamento
tipos_pronunciamento <- dados_multi %>%
  count(Tipo_Pronunciamento)

# Ordenar os dados por ordem crescente de n
tipos_pronunciamento <- tipos_pronunciamento %>%
  arrange(n)

# Plotar a frequência com melhorias visuais
ggplot(tipos_pronunciamento, aes(x = reorder(Tipo_Pronunciamento, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Frequência de Tipos de Pronunciamento",
    x = "Tipo de Pronunciamento",
    y = "Número de Pronunciamentos"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    panel.grid.major = element_blank(),  # Remove linhas de grade principais
    panel.grid.minor = element_blank(),  # Remove linhas de grade secundárias
    axis.line = element_line(color = "black")  # Adiciona linha de eixo x
  ) +
  coord_flip()  # Inverte a coordenada para barras horizontais

# Contagem de pronunciamentos por partido
partidos <- dados_multi %>%
  count(Partido)

# Plotar a distribuição com melhorias visuais
ggplot(partidos, aes(x = reorder(Partido, n), y = n, fill = n)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Cores gradientes
  labs(
    title = "Distribuição de Pronunciamentos por Partido",
    x = "Partido/UF",
    y = "Número de Pronunciamentos"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Ajuste na orientação e posicionamento do texto
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Centraliza e estiliza o título
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),  # Linhas de grade pontilhadas cinzas
    axis.line = element_line(color = "black")  # Linha de eixo preta
  )



