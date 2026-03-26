#PEACE REVIEW

library(ggplot2)
library(dplyr)

urldoc<- read.csv("/Users/edoardo/Desktop/gdt_edited.csv", stringsAsFactors=FALSE, sep = ";", header=TRUE)

#GRAFICO1 all categories
conteggi_1 <- table(urldoc$ideology)
df_conteggi_1 <- data.frame(ideology = names(conteggi_1), conteggio = as.vector(conteggi_1))
grafico_1 <- ggplot(data = df_conteggi_1, aes(x = ideology, y = conteggio)) +
  geom_bar(stat = "identity") +
  labs(x = "Ideology", y = "Terrorist Attacks") + scale_x_discrete(labels = c("far-left" = "Far-left", "far-right" = "Far-right", "islamist" = "Islamist", "jihadist" = "Jihadist", "post-ideological" = "Post-ideological", "nationalist" = "Nationalist", "unknown" = "Unknown")) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(grafico_1)

#GRAFICO2 removed unknown
urldoc_2 <- subset(urldoc, ideology != "unknown")
conteggi_2 <- table(urldoc_2$ideology)
df_conteggi_2 <- data.frame(ideology = names(conteggi_2), conteggio = as.vector(conteggi_2))
grafico_2 <- ggplot(data = df_conteggi_2, aes(x = ideology, y = conteggio)) +
  geom_bar(stat = "identity") +
  labs(x = "Ideology", y = "Terrorist Attacks") + scale_x_discrete(labels = c("far-left" = "Far-left", "far-right" = "Far-right", "islamist" = "Islamist", "jihadist" = "Jihadist", "post-ideological" = "Post-ideological", "nationalist" = "Nationalist")) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(grafico_2)

#GRAFICO3 only left and right
categorie_desiderate <- c("far-left", "far-right")
dataset_filtrato <- subset(urldoc, ideology %in% categorie_desiderate)
conteggi_3 <- table(dataset_filtrato$ideology)
df_conteggi_3 <- data.frame(ideology = names(conteggi_3), conteggio = as.vector(conteggi_3))
grafico_3 <- ggplot(data = df_conteggi_3, aes(x = ideology, y = conteggio)) +
  geom_bar(stat = "identity") +
  labs(x = "Ideology", y = "Terrorist Attacks") + scale_x_discrete(labels = c("far-left" = "Far-left", "far-right" = "Far-right"))
print(grafico_3)

#GRAFICO4 left and right+conspiracy
categorie_desiderate_2 <- c("far-left", "far-right")
dataset_filtrato_2 <- subset(urldoc, ideology %in% categorie_desiderate_2)
conteggi_gname <- table(urldoc$gname)
attacchi_conspiracy <- conteggi_gname["Conspiracy theory extremists"]
attacchi_far_right <- sum(urldoc$ideology == "far-right")
df_conteggi_4 <- data.frame(ideology = c("far-left", "far-right"), conteggio = c(sum(dataset_filtrato$ideology == "far-left"), attacchi_far_right + attacchi_conspiracy))
grafico_4 <- ggplot(data = df_conteggi_4, aes(x = ideology, y = conteggio)) +
  geom_bar(stat = "identity") +
  labs(x = "Ideology", y = "Terrorist Attacks") + scale_x_discrete(labels = c("far-left" = "Far-left", "far-right" = "Far-right &\nConspiracy theory extremists"))
print(grafico_4)

#GRAFICO5 removed unknown per year
categorie_desiderate_3 <- c("nationalist", "far-left", "far-right", "islamist", "jihadist", "post-ideological")
dataset_filtrato_3 <- subset(urldoc, ideology %in% categorie_desiderate_3)
colori_grigi <- c("far-left" = "black", "far-right" = "darkgrey", "islamist" = "grey30", "jihadist" = "grey50", "nationalist" = "grey70", "post-ideological" = "grey90")
grafico_linee <- ggplot(data = dataset_filtrato_3, aes(x = iyear, group = ideology, color = ideology)) +
  geom_line(stat = "count") +
  labs(x = "Year", y = "Terrorist Attacks") +
  scale_x_continuous(breaks = seq(min(dataset_filtrato_3$iyear), max(urldoc$iyear), by = 1), expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_manual(values = colori_grigi)
print(grafico_linee)

#GRAFICO6 only left and right per year
categorie_desiderate_4 <- c("far-left", "far-right")
dataset_filtrato_4 <- subset(urldoc, ideology %in% categorie_desiderate_4)
conteggi <- dataset_filtrato_4 %>%
  group_by(iyear, ideology) %>%
  summarize(conteggio = n()) %>%
  ungroup()
grafico_linee_2 <- ggplot(data = conteggi, aes(x = as.factor(iyear), y = conteggio, fill = ideology)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
  labs(x = "Year", y = "Terrorist Attacks") +
  scale_x_discrete(breaks = seq(min(conteggi$iyear), max(urldoc$iyear), by = 1), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c("far-left" = "black", "far-right" = "grey"), 
                    labels = c("far-left" = "Far-left", "far-right" = "Far-right"))
print(grafico_linee_2)

#GRAFICO7 only right+conspiracy
categorie_desiderate_5 <- c("far-left", "far-right")
dataset_filtrato_5 <- subset(urldoc, ideology %in% categorie_desiderate_5)
df_far_right <- subset(dataset_filtrato_5, ideology == "far-right", select = c("iyear", "ideology"))
df_gname <- urldoc[c("iyear", "gname")]
df_conspiracy_extremists <- df_gname[df_gname$gname == "Conspiracy theory extremists", ]
df_conspiracy_extremists <- df_conspiracy_extremists %>%
  rename(ideology = gname)
df_aggregato <- rbind(df_far_right, df_conspiracy_extremists)
df_aggregato <- df_aggregato %>%
  mutate(ideology = ifelse(ideology == "far-right", 1, ifelse(ideology == "Conspiracy theory extremists", 0, ideology)))
grafico_in_pila_1 <- ggplot(data = df_aggregato, aes(x = iyear, fill = ideology)) +
  geom_bar() +
  labs(x = "Year", y = "Terrorist Attacks") +
  scale_x_continuous(breaks = seq(min(df_aggregato$iyear), max(df_aggregato$iyear), by = 1), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c("1" = "black", "0" = "grey"), labels = c("1" = "Far-right", "0" = "Conspiracy\ntheory\nextremists"))
print(grafico_in_pila_1)

#GRAFICO8 only left and right+conspiracy
categorie_desiderate_6 <- c("far-left", "far-right")
dataset_filtrato_6 <- subset(urldoc, ideology %in% categorie_desiderate_6)
df_far_right_left <- subset(dataset_filtrato_6, select = c("iyear", "ideology"))
df_gname_2 <- urldoc[c("iyear", "gname")]
df_conspiracy_extremists_2 <- df_gname[df_gname_2$gname == "Conspiracy theory extremists", ]
df_conspiracy_extremists_2 <- df_conspiracy_extremists_2 %>%
  rename(ideology = gname)
df_aggregato_2 <- rbind(df_far_right_left, df_conspiracy_extremists_2)
df_aggregato_2$ideology <- factor(df_aggregato_2$ideology, levels = c("far-left", "far-right", "Conspiracy theory extremists"), labels = c("Far-left", "Far-right", "Conspiracy theory extremists"))
df_aggregato_2 <- df_aggregato_2 %>%
  group_by(iyear, ideology) %>%
  mutate(position = ifelse(ideology == "Conspiracy theory extremists", 1, 0))
grafico_in_pila_2 <- ggplot(data = df_aggregato_2, aes(x = iyear, fill = ideology)) +
  geom_bar(stat = "count", position = position_stack(reverse = TRUE)) +
  labs(x = "Year", y = "Terrorism Attacks") +
  scale_fill_manual(values = c("Far-left" = "black", "Far-right" = "darkgrey", "Conspiracy theory extremists" = "lightgrey")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(grafico_in_pila_2)

#EXPORT GRAPHS
ggsave("grafico_1.png", plot = grafico_1, dpi = 300)
ggsave("grafico_2.png", plot = grafico_2, dpi = 300)
ggsave("grafico_3.png", plot = grafico_3, dpi = 300)
ggsave("grafico_4.png", plot = grafico_4, dpi = 300)
ggsave("grafico_linee.png", plot = grafico_linee, dpi = 300)
ggsave("grafico_linee_2.png", plot = grafico_linee_2, dpi = 300)
ggsave("grafico_pila_1.png", plot = grafico_in_pila_1, dpi = 300)
ggsave("grafico_pila_2.png", plot = grafico_in_pila_2, dpi = 300)