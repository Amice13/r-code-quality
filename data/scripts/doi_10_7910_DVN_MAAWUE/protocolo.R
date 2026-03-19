
## Inside the Fog - Script

## 1. Dependências  -----

### 1.1. Carregando os pacotes -----

if (!require('pacman')) install.packages('pacman'); library('pacman')

library(pacman)

p_load(tidyverse, tm, readxl, readr, 
       tokenizers, stringdist, ggrepel,
       RColorBrewer, flextable)

### 1.2. Carregando os dados -----

NT_join_df_limpos <- read_csv("dados/NT_join_df_limpos.csv")
reunioes_comissao <- read_csv("dados/reunioes_comissao.csv")

# 2. Obtendo estatíticas iniciais -----

reunioes_comissao <- reunioes_comissao %>%
  mutate(Category = case_when(
    categoria == "Audiência Pública" ~ "Public Hearing",
    categoria == "Debates" ~ "Debates",
    categoria == "Deliberativa" ~ "Deliberative",
    categoria == "Indicação de Autoridade e Sabatinas" ~ "Appointment of Authority and Sabbatinas",
    categoria == "Instalação e Eleição" ~ "Installation and Election",
    categoria == "Reunião" ~ "Meetings",
    TRUE ~ categoria
  ))

# reuniões

ggplot(reunioes_comissao) +
  aes(x = ano, fill = Category) +
  geom_bar(position = "fill", stat = "count") +
  scale_fill_brewer(palette = "Set3") +
  labs(title = " ", x = " ", y = "  ") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = unique(reunioes_comissao$ano))

# Criar um gráfico de barras empilhadas com rótulos de valores

# Calcular a contagem de cada categoria por ano
contagem <- reunioes_comissao %>% 
  count(ano, Category)

# Criar um gráfico de barras empilhadas com rótulos de valores

ggplot(contagem, aes(x = ano, y = n, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(n, " (", scales::percent(n/sum(n)), ")")),
            position = position_stack(vjust = 0.5), size = 3) +  # Adiciona rótulos de valores
  scale_fill_brewer(palette = "Set3") +
  labs(title = " ", x = " ", y = "  ") +
  theme_minimal() +
  theme(legend.position = "top", 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = unique(reunioes_comissao$ano)) +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",  
        legend.justification = "center")

## 2. Calcular métricas de complexidade -----

calcular_metricas_complexidade <- function(texto) {
  
  # Cálculos básicos
  num_palavras <- str_count(texto, "\\S+")
  tamanho_medio_palavra <- sum(nchar(unlist(tokenize_words(texto)))) / num_palavras
  densidade_palavras_complexas <- sum(nchar(unlist(tokenize_words(texto))) > 6) / num_palavras
  
  # Entropia das Palavras
  
  frequencias <- table(tokenize_words(texto))
  probabilidades <- frequencias / sum(frequencias)
  entropia_palavras <- -sum(probabilidades * log2(probabilidades))
  
  # Jargões Técnicos
  
  jargoes_tecnicos <- sum(tolower(texto) %in% c("abertura", "acordo", "aliança", "ambiente", "amizade", "ampla", "ampliar", "ampliação", "anexo", "aproximação", "armas", "ascensão", "aspirações", "associação", "assumir", 
                                                "assuntos", "ação", "bloco", "brasil", "busca", "cooperação", "diálogo", "direitos", "estratégico", "internacional", "investimento", "multilateral", "negociação", "país", 
                                                "parceria", "paz", "política", "potência", "presidente", "prioridade", "proposta", "relações", "segurança", "sistema", "soberania", "sustentável", "união", "desenvolvimento", 
                                                "globalização", "comunidade", "cooperação", "diplomacia", "governo", "interesse", "instituições"))
  
  # Variedade de Sentenças
  
  variedade_sentencas <- length(unique(tokenize_sentences(texto))) / num_palavras
  
  # Uso de Conjunções
  
  uso_conjuncoes <- sum(tolower(texto) %in% c("e", "ou", "mas", "portanto", 
                                              "contudo", "todavia", "entretanto", 
                                              "porém", "no entanto", "assim", 
                                              "logo", "além disso", "também", 
                                              "ainda", "então", "por conseguinte", 
                                              "porque", "pois", "visto que", 
                                              "já que", "apesar de", "embora", 
                                              "contanto que", "a menos que", 
                                              "desde que", "caso", "se", 
                                              "quando", "enquanto", "até que", 
                                              "uma vez que", "mesmo que", 
                                              "como", "conforme", "se bem que", 
                                              "conquanto", "em que", "como se", 
                                              "assim que", "à medida que", 
                                              "apesar de que", "a despeito de", 
                                              "não obstante", "por isso", 
                                              "portanto", "por conseguinte", 
                                              "dessa forma", "de modo que", 
                                              "de forma que", "assim sendo", 
                                              "em consequência", "porquanto", 
                                              "em vista disso", "daí que", 
                                              "por conseguinte", "pelo que", 
                                              "então", "logo", "por fim", 
                                              "em síntese", "enfim", 
                                              "finalizando", "em resumo", 
                                              "concluindo", "por último", 
                                              "em última análise", "afinal")) / num_palavras
  
  # Coesão Textual
  
  conectivos <- c("portanto", "contudo", "além disso", "também", "ainda", "igualmente", "da mesma forma", 
                  "ademais", "além do mais", "entretanto", "no entanto", "porém", "todavia", 
                  "ao contrário", "em contrapartida", "pois", "por causa de", "uma vez que", 
                  "visto que", "já que", "apesar de", "mesmo que", "embora", "por conseguinte", 
                  "dessa forma", "assim", "logo", "enquanto", "antes", "depois", "desde que", 
                  "se", "caso", "a menos que", "contanto que", "até que", "uma vez que", 
                  "com o objetivo de", "a fim de que", "com o propósito de", "para que", 
                  "para", "para isso", "por exemplo", "como", "isto é", "a saber", "em particular", 
                  "especialmente", "entre outros", "quando", "sempre que", "assim que", "mesmo se",
                  "consequentemente", "em vista disso", "dessa maneira", "por conseguinte", "em resumo",
                  "de qualquer forma", "por outras palavras", "ou seja", "em outras palavras",
                  "por conseguinte", "de fato", "com efeito", "aliás", "ao contrário", "pelo contrário",
                  "além do que", "ademais disso", "outrossim", "ainda assim", "na realidade", "de maneira idêntica",
                  "por outro lado", "no entanto", "entretanto", "ainda assim", "mesmo assim", "não obstante",
                  "apesar disso", "apesar de tudo", "ao inverso", "ao revés", "ao contrário do esperado",
                  "neste sentido", "nesse contexto", "nesse ínterim", "ao passo que", "em contraste",
                  "num primeiro momento", "num segundo momento", "num terceiro momento", "num último momento",
                  "por último", "finalmente", "por fim", "em última análise", "em conclusão", "para concluir",
                  "como resultado", "em virtude de", "sobretudo", "em especial", "notadamente", "em particular",
                  "assim como", "do mesmo modo", "de igual modo", "por outro lado", "além do mais", "outrossim")
  
  coesao_textual <- sum(tolower(texto) %in% conectivos) / num_palavras
  
  # Razão de Pronomes
  
  razao_pronomes <- sum(tokenize_words(texto, lowercase = TRUE) %in% c("eu", "mim", "meu", "minha", "meus", "minhas",
                                                                       "você", "tu", "teu", "tua", "teus", "tuas",
                                                                       "ele", "ela", "eles", "elas", "seu", "sua",
                                                                       "seus", "suas", "nós", "nosso", "nossa", 
                                                                       "nossos", "nossas", "vós", "vosso", "vossa", 
                                                                       "vossos", "vossas")) / num_palavras
  # Razão de Verbos Modais (Generalizado)
  
  verbos_modais <- c("pode(r)?", "deve(r)?", "quer(e|er)?", "sabe(r)?", 
                     "posso", "podemos", "dev(o|emos)", "quer(o|emos)", 
                     "sab(e|emos)", "podem", "devem", "querem", "sabem", 
                     "pod(e|es|emos)", "dev(e|es|emos)", "quer(e|es|emos)", 
                     "sab(e|es|emos)", "poderá", "deverá", "quererá", 
                     "saberá", "pod(erei|eremos)", "dev(erei|eremos)", 
                     "quer(erei|eremos)", "sab(erei|eremos)", "poderíamos", 
                     "deveríamos", "quereríamos", "saberíamos", "poderás", 
                     "deverás", "quererás", "saberás", "poderão", "deverão", 
                     "quererão", "saberão", "poderiamos", "deveríamos", 
                     "quereríamos", "saberíamos", "podias?", "devias?", 
                     "querias?", "sabias?", "poderíeis?", "deveríeis?", 
                     "quereríeis?", "saberíeis?", "podíamos", "devíamos", 
                     "queríamos", "sabíamos", "pode", "dev(e|ia)", 
                     "quer(e|ia)", "sab(e|ia)", "pod(e|íeis)", "dev(e|íeis)", 
                     "quer(e|íeis)", "sab(e|íeis)", "puderam", "deviam", 
                     "queriam", "sabiam", "pude", "dev(i|ia)", "quis", 
                     "sou capaz", "era capaz", "serei capaz", "teria que", 
                     "gostaria de", "preferiria", "acho que")
  
  
  # Convertendo para expressão regular
  verbos_modais_regex <- paste(verbos_modais, collapse = "|")
  
  # Razão de Verbos Modais
  razao_verbos_modais <- sum(grepl(verbos_modais_regex, texto, ignore.case = TRUE)) / num_palavras
  
  # Lista de Verbos Passivos (Generalizado)
  verbos_passivos <- c("\\bser\\b", "\\bestar\\b", "\\bfoi\\b", "\\bera\\b", "\\bestava\\b",
                       "\\bforam\\b", "\\beram\\b", "\\bestavam\\b", "\\bsou\\b", "\\bés\\b",
                       "\\bsomos\\b", "\\bsão\\b", "\\bestou\\b", "\\b está\\b", "\\bestamos\\b",
                       "\\bestão\\b", "\\bfoi\\b", "\\bforam\\b", "\\bseja\\b", "\\bsomos\\b",
                       "\\bsejamos\\b", "\\bsão\\b", "\\bestava\\b", "\\b estava\\b", "\\bestivemos\\b",
                       "\\bestavam\\b", "\\bestava\\b", "\\bsão\\b", "\\besteja\\b", "\\bestivesse\\b",
                       "\\bestiver\\b", "\\bestivermos\\b", "\\bestiverem\\b", "\\bserá\\b", "\\bseremos\\b",
                       "\\bserão\\b", "\\bse\\b", "\\bses\\b", "\\bsejamos\\b", "\\bsejam\\b",
                       "\\bseria\\b", "\\bseríamos\\b", "\\bseriam\\b", "\\bestivesse\\b", "\\bestivéssemos\\b",
                       "\\bestivessem\\b", "\\bestiver\\b", "\\bestivermos\\b", "\\bestiverem\\b",
                       "\\bestarei\\b", "\\bestará\\b", "\\bestaremos\\b", "\\bestarão\\b",
                       "\\bestaria\\b", "\\bestaríamos\\b", "\\bestariam\\b", "\\bestávamos\\b",
                       "\\bestava\\b", "\\bestivemos\\b", "\\bestavam\\b", "\\bestava\\b", "\\bse\\b",
                       "\\bses\\b", "\\bseja\\b", "\\bsejamos\\b", "\\bsejam\\b", "\\bser\\b",
                       "\\bsermos\\b", "\\bserão\\b", "\\bse\\b", "\\bses\\b", "\\bsejamos\\b",
                       "\\bsejam\\b", "\\bseria\\b", "\\bseríamos\\b", "\\bseriam\\b", "\\bestivesse\\b",
                       "\\bestivéssemos\\b", "\\bestivessem\\b", "\\bestiver\\b", "\\bestivermos\\b",
                       "\\bestiverem\\b")
  
  # Razão de Verbos Passivos
  razao_verbos_passivos <- sum(grepl(paste(verbos_passivos, collapse = "|"), texto, ignore.case = TRUE)) / num_palavras
  
  # Frequência de Palavras Únicas
  frequencia_palavras_unicas <- length(unique(tokenize_words(texto))) / num_palavras
  
  # Complexidade da Sentença Média
  complexidade_sentenca_media <- num_palavras / length(tokenize_sentences(texto))
  
  # Densidade de Pontuação
  densidade_pontuacao <- sum(str_count(texto, "[[:punct:]]")) / num_palavras
  
  # Razão de Frases Curtas e Longas
  frases_curtas <- length(tokenize_sentences(texto))
  frases_longas <- length(tokenize_sentences(texto))
  razao_frases_curtas_longas <- frases_curtas / frases_longas
  
  # Lista de Advérbios (Generalizado)
  adverbios <- c("\\brapidamente\\b", "\\bclaramente\\b", "\\bapropriadamente\\b", 
                 "\\bvelozmente\\b", "\\bnitidamente\\b", "\\bcorretamente\\b", 
                 "\\bintensamente\\b", "\\bprecisamente\\b", "\\badecuadamente\\b", 
                 "\\bdepressa\\b", "\\bnaturalmente\\b", "\\bimediatamente\\b", 
                 "\\bsatisfatoriamente\\b", "\\bbruscamente\\b", "\\bfielmente\\b", 
                 "\\bconsistentemente\\b", "\\bextremamente\\b", "\\bsatisfazentemente\\b", 
                 "\\bclássico\\b", "\\bexatamente\\b", "\\binteiramente\\b", 
                 "\\boportunamente\\b", "\\bmarcadamente\\b", "\\bverdadeiramente\\b", 
                 "\\bcompletamente\\b", "\\btardiamente\\b", "\\bprecipitadamente\\b", 
                 "\\bpontualmente\\b", "\\bsubitamente\\b", "\\binadequadamente\\b")
  
  # Frequência de Advérbios
  frequencia_adverbios <- sum(grepl(paste(adverbios, collapse = "|"), texto, ignore.case = TRUE)) / num_palavras
  
  return(c(
    num_palavras_complex = num_palavras, 
    tamanho_medio_palavra_complex = tamanho_medio_palavra,
    densidade_palavras_complex = densidade_palavras_complexas,
    entropia_palavras_comple = entropia_palavras,
    jargoes_tecnicos_complex= jargoes_tecnicos,
    variedade_sentencas_complex = variedade_sentencas,
    uso_conjuncoes_complex = uso_conjuncoes,
    coesao_textual_complex = coesao_textual,
    razao_pronomes_complex = razao_pronomes,
    razao_verbos_modais_complex = razao_verbos_modais,
    razao_verbos_passivos_complex = razao_verbos_passivos,
    freq_palavras_unicas_complex = frequencia_palavras_unicas,
    sentenca_media_complex = complexidade_sentenca_media,
    densidade_pontuacao_complex = densidade_pontuacao,
    razao_frases_curtas_longas_complex= razao_frases_curtas_longas,
    frequencia_adverbios_complex = frequencia_adverbios
  ))
}


### 2.1 Aplicando a função às falas ----

# Agrupando as intervenções por reuniões (Senador - Reunião)

falas.agrupadas <- NT_join_df_limpos %>%
  group_by(nome, partido, funcao_blocoPar, id_reuniao, ano, Cargo) %>%
  summarize(falas = paste(fala, collapse = " "))

# filtrando alguns casos

falas.agrupadas <- falas.agrupadas %>%
  filter(!(nome %in% c("Não gerado pelo Senado", "Não Identificado"))) %>%
  filter(!str_detect(nome, "Esta Reunião"))

# Extraindo variáveis para o join

tab_categoria_reuniao = reunioes_comissao %>% 
  select(id_reuniao, categoria)

# Mudando a classe da variável

tab_categoria_reuniao$id_reuniao = as.character(tab_categoria_reuniao$id_reuniao)
falas.agrupadas$id_reuniao = as.character(falas.agrupadas$id_reuniao)
  
# Fazendo o join

falas.agrupadas = falas.agrupadas %>% 
  left_join(tab_categoria_reuniao)

# Aplicando a função para obtenção das medidas

falas.medidas <- falas.agrupadas %>%
  rowwise() %>%
  mutate(mc = list(calcular_metricas_complexidade(falas))) %>%
  unnest_wider(mc, names_sep = "_")

glimpse(falas.medidas)

# estabelecendo o critério de exclusão de intervenções curtas

# Calcular o percentil 25 do número de palavras

limite <- quantile(falas.medidas$mc_num_palavras_complex, probs = 0.25)

# Filtrar intervenções com número de palavras abaixo do percentil 25

falas.medidas1 <- falas.medidas %>%
  filter(mc_num_palavras_complex >= limite)

## 2. Aplicando o PCA e a Análise de CLuster ----

### 2.1 PCA ----

# Obtendo os dados para o PCA

dados_pca <- falas.medidas1 %>%
  select(
    mc_densidade_palavras_complex,
    mc_entropia_palavras_comple, 
    mc_variedade_sentencas_complex, 
    mc_razao_verbos_modais_complex , 
    mc_razao_verbos_passivos_complex,
    mc_sentenca_media_complex
  )

# Padronizar dados

dados_pca_padronizados <- scale(dados_pca)

# Aplicar PCA

set.seed(1234)

# Calcular as coordenadas dos componentes principais para os dados
pca_resultados <- prcomp(dados_pca_padronizados, scale = TRUE)

# Acessar as proporções da variância explicada
prop_var <- summary(pca_resultados)$importance[2, ]

# Extrair os loadings

loadings <- pca_resultados$rotation

loadings_df = loadings %>% as.data.frame()

# Nomear as variáveis no conjunto de dados loadings_df

loadings_df$Label <- rownames(loadings_df)

# mudando o nome das variaveis

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

# selecionando colunas

loadings_df1 = loadings_df %>% 
  select(PC1, PC2, Label)

# Criar uma nova linha para a proporção de variância explicada
prop_var_df <- data.frame(Label = "Prop. Var. Accounted",
                          PC1 = prop_var[1],
                          PC2 = prop_var[2])

# Combinar as tabelas loadings_df1 e prop_var_df

loadings_df1 <- rbind(loadings_df1, prop_var_df)

# Arredondar os valores de PC1 e PC2
loadings_df1$PC1 <- round(loadings_df1$PC1, 2)
loadings_df1$PC2 <- round(loadings_df1$PC2, 2)

loadings_df1 = loadings_df1 %>% 
  rename("  " = Label) %>% 
  select("  ", PC1, PC2)

# Criar uma tabela flextable

ft <- flextable(loadings_df1)

# Ajustar a formatação da tabela

ft %>%
  fontsize(size = 10) %>%                     # Definir o tamanho da fonte
  set_table_properties(width = 0.8) %>%      # Definir a largura da tabela
  bg(bg = "#f2f2f2", part = "header") %>%   # Definir cor de fundo suave para o cabeçalho
  bg(bg = "white", part = "all") %>%          # Definir cor de fundo branca para o corpo da tabela
  border_outer() 

### 2.1 Cluster ----

# Determinar o número ideal de clusters usando o método do cotovelo

set.seed(1988)

wss <- numeric(10)
for (i in 1:10) {
  wss[i] <- sum(kmeans(pca_resultados$x[, 1:2], centers = i)$withinss)
}

# Plotar o gráfico do método do cotovelo

plot(1:10, wss, type = "b", xlab = "Número de Clusters", ylab = "Within Sum of Squares")

# Escolher o número de clusters (k) com base no método do cotovelo
# (o ponto onde a redução na soma dos quadrados dentro do cluster desacelera)

k <- 3  # Ajuste o número de clusters conforme a análise do gráfico

# Aplicar o algoritmo de k-means

cluster_resultados <- kmeans(pca_resultados$x[, 1:2], centers = k)

# Adicionar informações de cluster aos dados originais

contributions <- as.data.frame(pca_resultados$x[, 1:2])

dados_pca = dados_pca %>% 
  cbind(contributions)

dados_pca$cluster <- as.factor(cluster_resultados$cluster)

dados = dados_pca %>% 
  select(PC1, PC2, cluster)

dados_reuniao = reunioes_comissao %>% 
  select(id_reuniao, categoria)

dados_reuniao$id_reuniao = as.character(dados_reuniao$id_reuniao)

dados_pca_variaveis = cbind(falas.medidas1, dados)

dados_pca_variaveis = dados_pca_variaveis %>% 
  left_join(dados_reuniao)

# Incluir variáveis

dados_pca_variaveis = dados_pca_variaveis %>%
  mutate(governo = case_when(
    ano >= 2011 & ano <= 2016 ~ "Dilma",
    ano >= 2016 & ano <= 2018 ~ "Temer",
    ano >= 2019 & ano <= 2022 ~ "Bolsonaro",
    ano >= 2023 & ano <= 2024 ~ "Lula",
    TRUE ~ "Outro Governo"  # Se não se encaixar em nenhum dos intervalos acima
  ))

# aplicar os rótulos para os clusters 

dados_pca_variaveis <- dados_pca_variaveis %>%
  mutate(cluster_rotulo = case_when(
    cluster == 1 ~ "Prolix/Combative",
    cluster == 2 ~ "Experts/Analytical",
    cluster == 3 ~ "Concise/Pragmatics",
    TRUE ~ as.character(cluster)  # Se não se encaixar em nenhum dos intervalos acima
  ))

# aplicar os rótulos para os clusters 

dados_pca_variaveis <- dados_pca_variaveis %>%
  mutate(Cargo = case_when(
    Cargo == "Senador" ~ "Senator",
    TRUE ~ "Other" # Se não se encaixar em nenhum dos intervalos acima
  ))

# visualizar os dados



# Garantir que o fator cluster_rotulo tenha a ordem correta
dados_pca_variaveis <- dados_pca_variaveis %>%
  mutate(cluster_rotulo = factor(cluster_rotulo, 
                                 levels = c("Prolix/Combative", 
                                            "Concise/Pragmatics", 
                                            "Experts/Analytical")))

# Filtrar os dados dos senadores
dados_senadores = dados_pca_variaveis %>% 
  filter(Cargo == "Senator")

# Amostra aleatória de senadores com a estrutura correta de cluster
set.seed(1987)

dados_senadores_amostra <- dados_senadores %>%
  sample_n(30)  # Amostra de 30 senadores


# Definir as cores RGB para cada categoria
cores_por_categoria <- c("Prolix/Combative" = "#B22222",   
                         "Concise/Pragmatics" = "#20B2AA",
                         "Experts/Analytical" = "#0000CD") 

# Criar o gráfico de dispersão
p <- ggplot(dados_pca_variaveis, aes(x = PC1, y = PC2, color = cluster_rotulo, shape = Cargo)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_manual(values = cores_por_categoria, guide = "legend", name = "Cluster") +
  labs(title = " ") +
  guides(shape = guide_legend(title = "Role")) +
  theme_classic() +  # Aplica o tema classic
  theme(legend.position = "bottom",  # Posiciona a legenda na parte inferior
        legend.justification = "center")  # Justifica a legenda ao centro

# Definir uma cor única para o fundo do rótulo
cor_fundo_rotulo <- "#F0F0F0"  # Cor neutra para o fundo do rótulo (pode ajustar conforme necessário)

# Adicionar rótulos com fundo usando geom_label_repel
p + 
  geom_label_repel(data = dados_senadores_amostra, 
                   aes(label = nome),  # Apenas nome como label
                   fill = cor_fundo_rotulo,  # Cor única para o fundo
                   size = 4, 
                   box.padding = unit(0.3, "lines"), 
                   color = "black") +  # Texto preto para contraste
  scale_fill_manual(values = cor_fundo_rotulo, guide = "none")  # Mantém o fundo uniforme nos rótulos


# Definir a ordem dos governos

ordem_governos <- c("Dilma", "Temer", "Bolsonaro", "Lula")
dados_pca_variaveis$governo <- factor(dados_pca_variaveis$governo, levels = ordem_governos)

set.seed(1986)

# Definir o número desejado de senadores a serem amostrados
num_senadores_amostra <- 50

# Criar um gráfico de dispersão com os rótulos para os senadores amostrados aleatoriamente

p <- ggplot(dados_pca_variaveis, aes(x = PC1, y = PC2, color = as.factor(cluster_rotulo), shape = Cargo)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_manual(values = cores_por_categoria, guide = "legend", name = "Cluster") +
  labs(title = "  ") +
  guides(shape = guide_legend(title = "Role")) +
  theme_classic() +  # Aplicar o tema classic
  theme(panel.grid = element_blank(),
        legend.position = "bottom",  
        legend.justification = "center") +
  geom_label_repel(data = sample_n(dados_senadores, num_senadores_amostra), aes(label = nome), size = 4, 
                   box.padding = unit(0.3, "lines"), fill = "white") +
  facet_wrap(~ factor(governo, levels = ordem_governos), scales = "free", ncol = 2) +  # Facetar pelos governos
  theme(panel.grid = element_blank(),
        legend.position = "bottom",  
        legend.justification = "center")

# Visualizar o gráfico
print(p)
