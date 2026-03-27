# Code for Vote Swiper Survey: Most Important Topic

# Load Packages ----
install.packages("broom")
install.packages("cowplot")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("grid")
install.packages("haven")
install.packages("logistf")
install.packages("marginaleffects")
install.packages("modelsummary")
install.packages("naniar")
install.packages("pandoc")
install.packages("patchwork")
install.packages("performance")
install.packages("readr")
install.packages("readxl")
install.packages("tidyverse")
install.packages("writexl")

library(broom)
library(cowplot)
library(dplyr)
library(ggplot2)
library(grid)
library(haven)
library(logistf)
library(marginaleffects)
library(modelsummary)
library(naniar)
library(pandoc)
library(patchwork)
library(performance)
library(readr)
library(readxl)
library(tidyverse)
library(writexl)

# Preparation (data1) ----
# Working Directory
### Replace path if necessary
setwd("~/Documents/Forschung/2025 Wahlswiper Nachbefragung/Daten Nachbefragung WahlSwiper_24.02.2025")

# Load data
data1 <- read_excel("Data_Research_Note.xlsx")

# Sort out missings and non-eligible voters (data2) ----

# Missings 
data2 <- na.omit(data1)
vis_miss(data2)

# Non-eligible voters
data2 <- subset(data2, v_379 != 2)

# Sort out younger than 2008
data2 <- subset(data2, v_630 < 2008)
summary(data2$v_630)

# Sort out not answered high school graduation v_22=0 
data2 <- data2 %>% 
  filter(v_22 != 0)

# Sort out unanswered vocational education
data2 <- data2 %>%
  filter(!(v_685 == 0 & v_631 == 0 & v_632 == 0 & v_633 == 0 & 
             v_634 == 0 & v_635 == 0 & v_636 == 0 & v_637 == 0 & 
             v_638 == 0 & v_639 == 0))

# Recode most important topics and education (data3) ----
data3 <- data2

# Everything bigger or equal to 4 becomes 0
data3$v_611[data3$v_611>=4] <- 0
data3$v_612[data3$v_612>=4] <- 0
data3$v_613[data3$v_613>=4] <- 0
data3$v_614[data3$v_614>=4] <- 0
data3$v_615[data3$v_615>=4] <- 0
data3$v_616[data3$v_616>=4] <- 0
data3$v_617[data3$v_617>=4] <- 0
data3$v_618[data3$v_618>=4] <- 0
data3$v_619[data3$v_619>=4] <- 0
data3$v_620[data3$v_620>=4] <- 0
data3$v_621[data3$v_621>=4] <- 0
data3$v_622[data3$v_622>=4] <- 0
data3$v_623[data3$v_623>=4] <- 0

# 1 becomes 3 and 3 becomes 1 for issues
data3[, c("v_611", "v_612", "v_613", "v_614", "v_615", "v_616", 
          "v_617", "v_618", "v_619", "v_620", "v_621", "v_622", "v_623")] <- 
  lapply(data3[, c("v_611", "v_612", "v_613", "v_614", "v_615", "v_616", 
                   "v_617", "v_618", "v_619", "v_620", "v_621", "v_622", "v_623")], 
         function(x) ifelse(x == 1, 3, ifelse(x == 3, 1, x)))

# Create a new variable "Voc_ed" for vocational education
data3 <- data3 %>%
  mutate(Voc_ed = case_when(
    v_685 == 1 ~ 1,
    v_631 == 1 | v_632 == 1 ~ 2,
    v_633 == 1 ~ 3,
    v_634 == 1 ~ 4,
    v_635 == 1 | v_636 == 1 | v_637 == 1 | v_638 == 1 ~ 5,
    v_639 == 1 ~ 6,
    TRUE ~ 0  
  ))

# Filter v_372 (Party Preference) == 0
data3 <- data3 %>%
  filter(!(v_372 == 0))

# Most Important Topic for party preferences ----

# Count occurrence
number1_AfD <- sum(data3$v_372 == 1, na.rm = TRUE) # 1083
number2_Greens <- sum(data3$v_372 == 2, na.rm = TRUE) # 770
number3_Union <- sum(data3$v_372 == 3, na.rm = TRUE) # 376
number4_SPD <- sum(data3$v_372 == 4, na.rm = TRUE) # 242
number5_FDP <- sum(data3$v_372 == 5, na.rm = TRUE) # 142
number6_Left <- sum(data3$v_372 == 6, na.rm = TRUE) # 491
number7_BSW <- sum(data3$v_372 == 7, na.rm = TRUE) # 235
number8_Other <- sum(data3$v_372 == 8, na.rm = TRUE) # 186
number9_Undecided <- sum(data3$v_372 == 9, na.rm = TRUE) # 578
number10_Novoters <- sum(data3$v_372 == 10, na.rm = TRUE) # 17

# Relevante variables auswählen
variables <- c("v_611", "v_612", "v_622", "v_613", "v_618", "v_619", 
               "v_615", "v_617", "v_620", "v_614", "v_616", "v_623", "v_621")

# Neue Beschriftungen für die Themen
variables_labels <- c("Domestic Security", 
                      "Immigration of Foreigners/Migration", 
                      "Foreign and Defense Policy",
                      "Social Justice/Inequality", 
                      "Health and Care", 
                      "Unemployment", 
                      "Education and School Policy",
                      "Climate and Environment", 
                      "Digitization", 
                      "Tax and Duty Burden",  
                      "Traffic and Infrastructure", 
                      "Economic Policy",
                      "Pensions")

# Calculate sums for v_372
sum1 <- colSums(data3[data3$v_372 == 1, variables], na.rm = TRUE)
sum2 <- colSums(data3[data3$v_372 == 2, variables], na.rm = TRUE)
sum3 <- colSums(data3[data3$v_372 == 3, variables], na.rm = TRUE)
sum4 <- colSums(data3[data3$v_372 == 4, variables], na.rm = TRUE)
sum5 <- colSums(data3[data3$v_372 == 5, variables], na.rm = TRUE)
sum6 <- colSums(data3[data3$v_372 == 6, variables], na.rm = TRUE)
sum7 <- colSums(data3[data3$v_372 == 7, variables], na.rm = TRUE)
sum8 <- colSums(data3[data3$v_372 == 8, variables], na.rm = TRUE)
sum9 <- colSums(data3[data3$v_372 == 9, variables], na.rm = TRUE)

# Averages
values1 <- sum1 / number1_AfD
values2 <- sum2 / number2_Greens
values3 <- sum3 / number3_Union
values4 <- sum4 / number4_SPD
values5 <- sum5 / number5_FDP
values6 <- sum6 / number6_Left
values7 <- sum7 / number7_BSW
values8 <- sum8 / number8_Other 
values9 <- sum9 / number9_Undecided

###AfD
# Dataframe for ggplot
df_AfD <- data.frame(
  Topic = factor(variables_labels, levels = rev(variables_labels)),  
  Score = values1
) 

# Barplot AfD
barplot_AfD <- ggplot(df_AfD, aes(x = Score, y = Topic)) +
  geom_vline(xintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", fill = "dodgerblue2", color = "black", width = 0.7) +  
  xlim(0, 2.5) + 
  scale_y_discrete(expand = c(0, 2)) +
  labs(x = "", 
       y = "", 
       title = "AfD") +
  theme_linedraw() +  
  theme(
    panel.grid = element_blank(),        
    axis.text.y = element_blank(), 
    plot.title = element_text(size = 20, face = "plain")
  )

### Greens
# Dataframe for ggplot
df_Greens <- data.frame(
  Topic = factor(variables_labels, levels = rev(variables_labels)),    
  Score = values2
)

# Barplot Greens
barplot_Greens <- ggplot(df_Greens, aes(x = Score, y = Topic)) +
  geom_vline(xintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", fill = "chartreuse4", color = "black", width = 0.7) +   
  xlim(0, 2.5) +    
  scale_y_discrete(expand = c(0, 2)) +
  labs(x = "", 
       y = "", 
       title = "Bündnis 90/Die Grünen") +
  theme_linedraw() +  
  theme(
    panel.grid = element_blank(),          
    axis.text.y = element_text(size = 13),
    plot.title = element_text(size = 20, face = "plain")
  )

### CDU / CSU
# Dataframe for ggplot
df_Union <- data.frame(
  Topic = factor(variables_labels, levels = rev(variables_labels)),    
  Score = values3
)

# Barplot CDU / CSU
barplot_Union <- ggplot(df_Union, aes(x = Score, y = Topic)) +
  geom_vline(xintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", fill = "black", color = "black", width = 0.7) +   
  xlim(0, 2.5) +    
  scale_y_discrete(expand = c(0, 2)) +
  labs(x = "", 
       y = "", 
       title = "CDU/CSU") +
  theme_linedraw() +    
  theme(
    panel.grid = element_blank(),        
    axis.text.y = element_text(size = 13), 
    plot.title = element_text(size = 20, face = "plain")
  )

### SPD
# Dataframe for ggplot
df_SPD <- data.frame(
  Topic = factor(variables_labels, levels = rev(variables_labels)),    
  Score = values4
)

# Barplot SPD
barplot_SPD <- ggplot(df_SPD, aes(x = Score, y = Topic)) +
  geom_vline(xintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", fill = "red3", color = "black", width = 0.7) +   
  xlim(0, 2.5) +    
  scale_y_discrete(expand = c(0, 2)) +
  labs(x = "", 
       y = "", 
       title = "SPD") +
  theme_linedraw() +    
  theme(
    panel.grid = element_blank(),          
    axis.text.y = element_blank(),
    plot.title = element_text(size = 20, face = "plain")
  )

### FDP
# Dataframe for ggplot
df_FDP <- data.frame(
  Topic = factor(variables_labels, levels = rev(variables_labels)),    
  Score = values5
)

# Barplot mit ggplot
barplot_FDP <- ggplot(df_FDP, aes(x = Score, y = Topic)) +
  geom_vline(xintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", fill = "gold2", color = "black", width = 0.7) +   
  xlim(0, 2.5) +    
  scale_y_discrete(expand = c(0, 2)) +
  labs(x = "", 
       y = "", 
       title = "FDP") +
  theme_linedraw() +    
  theme(
    panel.grid = element_blank(),          
    axis.text.y = element_text(size = 13),
    plot.title = element_text(size = 20, face = "plain")
  )

### The Left
# Dataframe for ggplot
df_Left <- data.frame(
  Topic = factor(variables_labels, levels = rev(variables_labels)),    
  Score = values6
)

# Barplot The Left
barplot_Left <- ggplot(df_Left, aes(x = Score, y = Topic)) +
  geom_vline(xintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", fill = "deeppink3", color = "black", width = 0.7) +   
  xlim(0, 2.5) +    
  scale_y_discrete(expand = c(0, 2)) +
  labs(x = "", 
       y = "", 
       title = "Die Left") +
  theme_linedraw() +    
  theme(
    panel.grid = element_blank(),          
    axis.text.y = element_blank(),
    plot.title = element_text(size = 20, face = "plain")
  )

### BSW
# Dataframe for ggplot
df_BSW <- data.frame(
  Topic = factor(variables_labels, levels = rev(variables_labels)),    
  Score = values7
)

# Barplot BSW
barplot_BSW <- ggplot(df_BSW, aes(x = Score, y = Topic)) +
  geom_vline(xintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", fill = "darkorchid4", color = "black", width = 0.7) +   
  xlim(0, 2.5) +    
  scale_y_discrete(expand = c(0, 2)) +
  labs(x = "", 
       y = "", 
       title = "BSW") +
  theme_linedraw() +    
  theme(
    panel.grid = element_blank(),          
    axis.text.y = element_text(size = 13),   
    plot.title = element_text(size = 20, face = "plain")
  )

### Other
# Dataframe for ggplot
df_Other <- data.frame(
  Topic = factor(variables_labels, levels = rev(variables_labels)),    
  Score = values8
)

# Barplot Other
barplot_Other <- ggplot(df_Other, aes(x = Score, y = Topic)) +
  geom_vline(xintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", fill = "gray81", color = "black", width = 0.7) +   
  xlim(0, 2.5) +    
  scale_y_discrete(expand = c(0, 2)) +
  labs(x = "Average Topic Scores", 
       y = "", 
       title = "Other") +
  theme_linedraw() +    
  theme(
    panel.grid = element_blank(),          
    axis.text.y = element_blank(),
    plot.title = element_text(size = 20, face = "plain")
  )

### Undecided
# Dataframe for ggplot
df_Undecided <- data.frame(
  Topic = factor(variables_labels, levels = rev(variables_labels)),    
  Score = values9
)

# Barplot Undecided
barplot_Undecided <- ggplot(df_Undecided, aes(x = Score, y = Topic)) +
  geom_vline(xintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", fill = "gray60", color = "black", width = 0.7) +   
  xlim(0, 2.5) +    
  scale_y_discrete(expand = c(0, 2)) +
  labs(x = "Average Topic Scores", 
       y = "", 
       title = "Undecided") +
  theme_linedraw() +    
  theme(
    panel.grid = element_blank(),          
    axis.text.y = element_text(size = 13),
    plot.title = element_text(size = 20, face = "plain")
  )

# Save plots in a list
Parties_by_Topic <- plot_grid(
  barplot_Union, barplot_SPD, barplot_Greens, 
  barplot_AfD, barplot_FDP, barplot_Left, 
  barplot_BSW, barplot_Other, barplot_Undecided,
  ncol = 2,  # Setzt genau 2 Plots pro Reihe
  rel_widths = c(1.32, 1)
)
ggsave("Parties_by_Topic.png", Parties_by_Topic, width = 15, height = 20)



# Most Important Topic for parties according to topics ----

# Party names
party_names <- c("CDU/CSU", "AfD", "SPD", "Greens", "The Left", "FDP", "BSW", "Other", "Undecided")

# number der Befragten pro Party (vorher berechnet)
number_partynames <- c(number3_Union, number1_AfD, number4_SPD, number2_Greens, number6_Left, number5_FDP, 
                      number7_BSW, number8_Other, number9_Undecided)

# Farbcodierung für die party_names
colors_parties <- c(
  "CDU/CSU" = "black",
  "AfD" = "dodgerblue2",
  "SPD" = "red3",
  "Greens" = "chartreuse4",
  "The Left" = "deeppink3",
  "FDP" = "gold2",
  "BSW" = "darkorchid4",
  "Other" = "gray81",
  "Undecided" = "gray60"
)

### v_611
# sum for v_611 (Domestic Security) per party
sum_v611 <- c(
  sum(data3[data3$v_372 == 3, "v_611"], na.rm = TRUE),
  sum(data3[data3$v_372 == 1, "v_611"], na.rm = TRUE),
  sum(data3[data3$v_372 == 4, "v_611"], na.rm = TRUE),
  sum(data3[data3$v_372 == 2, "v_611"], na.rm = TRUE),
  sum(data3[data3$v_372 == 6, "v_611"], na.rm = TRUE),
  sum(data3[data3$v_372 == 5, "v_611"], na.rm = TRUE),
  sum(data3[data3$v_372 == 7, "v_611"], na.rm = TRUE),
  sum(data3[data3$v_372 == 8, "v_611"], na.rm = TRUE),
  sum(data3[data3$v_372 == 9, "v_611"], na.rm = TRUE)
)

# Average
values_v611 <- sum_v611 / number_partynames 

 # Dataframe for ggplot
df_v611 <- data.frame(
  Party = factor(party_names, levels = party_names), 
  Score = values_v611
)

# Barplot v611
barplot_v611 <- ggplot(df_v611, aes(x = Party, y = Score, fill = Party)) +
  geom_hline(yintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +  
  ylim(0, 2.5) +   
  scale_x_discrete(expand = c(0.05, 0)) +  
  scale_fill_manual(values = colors_parties) +   
  labs(x = "", 
       y = "Average Topic Score", 
       title = "Domestic Security") +
  theme_linedraw() +  
  theme(
    axis.title.y = element_text(size = 14),
    panel.grid = element_blank(),        
    axis.text.x = element_blank(), 
    plot.title = element_text(size = 20, face = "plain"),
    legend.position = "none" 
  )

### v_612
# sum for v_612 (Immigration) per party
sum_v612 <- c(
  sum(data3[data3$v_372 == 3, "v_612"], na.rm = TRUE),
  sum(data3[data3$v_372 == 1, "v_612"], na.rm = TRUE),
  sum(data3[data3$v_372 == 4, "v_612"], na.rm = TRUE),
  sum(data3[data3$v_372 == 2, "v_612"], na.rm = TRUE),
  sum(data3[data3$v_372 == 6, "v_612"], na.rm = TRUE),
  sum(data3[data3$v_372 == 5, "v_612"], na.rm = TRUE),
  sum(data3[data3$v_372 == 7, "v_612"], na.rm = TRUE),
  sum(data3[data3$v_372 == 8, "v_612"], na.rm = TRUE),
  sum(data3[data3$v_372 == 9, "v_612"], na.rm = TRUE)
)

# Average 
values_v612 <- sum_v612 / number_partynames 

# Dataframe for ggplot
df_v612 <- data.frame(
  Party = factor(party_names, levels = party_names),  
  Score = values_v612
)

# Barplot v612
barplot_v612 <- ggplot(df_v612, aes(x = Party, y = Score, fill = Party)) +
  geom_hline(yintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +  
  ylim(0, 2.5) +   
  scale_x_discrete(expand = c(0.05, 0)) +  
  scale_fill_manual(values = colors_parties) +   
  labs(x = "", 
       y = "", 
       title = "Immigration of Foreigners/Migration") +
  theme_linedraw() +  
  theme(
    panel.grid = element_blank(),        
    axis.text.x = element_blank(), 
    plot.title = element_text(size = 20, face = "plain"),
    legend.position = "none"  
  )

### v_613
# sum for v_613 (Social Justice/Inequality) per party
sum_v613 <- c(
  sum(data3[data3$v_372 == 3, "v_613"], na.rm = TRUE),
  sum(data3[data3$v_372 == 1, "v_613"], na.rm = TRUE),
  sum(data3[data3$v_372 == 4, "v_613"], na.rm = TRUE),
  sum(data3[data3$v_372 == 2, "v_613"], na.rm = TRUE),
  sum(data3[data3$v_372 == 6, "v_613"], na.rm = TRUE),
  sum(data3[data3$v_372 == 5, "v_613"], na.rm = TRUE),
  sum(data3[data3$v_372 == 7, "v_613"], na.rm = TRUE),
  sum(data3[data3$v_372 == 8, "v_613"], na.rm = TRUE),
  sum(data3[data3$v_372 == 9, "v_613"], na.rm = TRUE)
)

# Average
values_v613 <- sum_v613 / number_partynames 

# Dataframe for ggplot
df_v613 <- data.frame(
  Party = factor(party_names, levels = party_names),  
  Score = values_v613
)

# Barplot 
barplot_v613 <- ggplot(df_v613, aes(x = Party, y = Score, fill = Party)) +
  geom_hline(yintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +  
  ylim(0, 2.5) +   
  scale_x_discrete(expand = c(0.05, 0)) +  
  scale_fill_manual(values = colors_parties) +   
  labs(x = "", 
       y = "Average Topic Score", 
       title = "Social Justice/Inequality") +
  theme_linedraw() +  
  theme(
    axis.title.y = element_text(size = 14),
    panel.grid = element_blank(),        
    axis.text.x = element_blank(),  
    plot.title = element_text(size = 20, face = "plain"),
    legend.position = "none"  
  )

### v_614
# sum for v_614 (Tax and Duty Burden) per party
sum_v614 <- c(
  sum(data3[data3$v_372 == 3, "v_614"], na.rm = TRUE),
  sum(data3[data3$v_372 == 1, "v_614"], na.rm = TRUE),
  sum(data3[data3$v_372 == 4, "v_614"], na.rm = TRUE),
  sum(data3[data3$v_372 == 2, "v_614"], na.rm = TRUE),
  sum(data3[data3$v_372 == 6, "v_614"], na.rm = TRUE),
  sum(data3[data3$v_372 == 5, "v_614"], na.rm = TRUE),
  sum(data3[data3$v_372 == 7, "v_614"], na.rm = TRUE),
  sum(data3[data3$v_372 == 8, "v_614"], na.rm = TRUE),
  sum(data3[data3$v_372 == 9, "v_614"], na.rm = TRUE)
)

# Average
values_v614 <- sum_v614 / number_partynames 

# Dataframe for ggplot
df_v614 <- data.frame(
  Party = factor(party_names, levels = party_names),  
  Score = values_v614
)

# Barplot 
barplot_v614 <- ggplot(df_v614, aes(x = Party, y = Score, fill = Party)) +
  geom_hline(yintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +  
  ylim(0, 2.5) +   
  scale_x_discrete(expand = c(0.05, 0)) +  
  scale_fill_manual(values = colors_parties) +   
  labs(x = "", 
       y = "Average Topic Score", 
       title = "Tax and Duty Burden") +
  theme_linedraw() +  
  theme(
    axis.title.y = element_text(size = 14),
    panel.grid = element_blank(),        
    axis.text.x = element_blank(), 
    plot.title = element_text(size = 20, face = "plain"),
    legend.position = "none"  
  )

### v_615
# sum for v_615 (Education and School Policy) per party
sum_v615 <- c(
  sum(data3[data3$v_372 == 3, "v_615"], na.rm = TRUE),
  sum(data3[data3$v_372 == 1, "v_615"], na.rm = TRUE),
  sum(data3[data3$v_372 == 4, "v_615"], na.rm = TRUE),
  sum(data3[data3$v_372 == 2, "v_615"], na.rm = TRUE),
  sum(data3[data3$v_372 == 6, "v_615"], na.rm = TRUE),
  sum(data3[data3$v_372 == 5, "v_615"], na.rm = TRUE),
  sum(data3[data3$v_372 == 7, "v_615"], na.rm = TRUE),
  sum(data3[data3$v_372 == 8, "v_615"], na.rm = TRUE),
  sum(data3[data3$v_372 == 9, "v_615"], na.rm = TRUE)
)

# Average 
values_v615 <- sum_v615 / number_partynames 

# Dataframe for ggplot
df_v615 <- data.frame(
  Party = factor(party_names, levels = party_names),  
  Score = values_v615
)

# Barplot 
barplot_v615 <- ggplot(df_v615, aes(x = Party, y = Score, fill = Party)) +
  geom_hline(yintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +  
  ylim(0, 2.5) +   
  scale_x_discrete(expand = c(0.05, 0)) +  
  scale_fill_manual(values = colors_parties) +   
  labs(x = "", 
       y = "Average Topic Score", 
       title = "Education and School Policy") +
  theme_linedraw() +  
  theme(
    axis.title.y = element_text(size = 14),
    panel.grid = element_blank(),        
    axis.text.x = element_blank(),  
    plot.title = element_text(size = 20, face = "plain"),
    legend.position = "none"  
  )

### v_616
# sum for v_616 (Traffic and Infrastructure) per party
sum_v616 <- c(
  sum(data3[data3$v_372 == 3, "v_616"], na.rm = TRUE),
  sum(data3[data3$v_372 == 1, "v_616"], na.rm = TRUE),
  sum(data3[data3$v_372 == 4, "v_616"], na.rm = TRUE),
  sum(data3[data3$v_372 == 2, "v_616"], na.rm = TRUE),
  sum(data3[data3$v_372 == 6, "v_616"], na.rm = TRUE),
  sum(data3[data3$v_372 == 5, "v_616"], na.rm = TRUE),
  sum(data3[data3$v_372 == 7, "v_616"], na.rm = TRUE),
  sum(data3[data3$v_372 == 8, "v_616"], na.rm = TRUE),
  sum(data3[data3$v_372 == 9, "v_616"], na.rm = TRUE)
)

# Average
values_v616 <- sum_v616 / number_partynames

# Dataframe for ggplot
df_v616 <- data.frame(
  Party = factor(party_names, levels = party_names),  
  Score = values_v616
)

# Barplot 
barplot_v616 <- ggplot(df_v616, aes(x = Party, y = Score, fill = Party)) +
  geom_hline(yintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +  
  ylim(0, 2.5) +   
  scale_x_discrete(expand = c(0.05, 0)) +  
  scale_fill_manual(values = colors_parties) +   
  labs(x = "", 
       y = "", 
       title = "Traffic and Infrastructure") +
  theme_linedraw() +  
  theme(
    panel.grid = element_blank(),        
    axis.text.x = element_blank(), 
    plot.title = element_text(size = 20, face = "plain"),
    legend.position = "none"  
  )

### v_617
# sum for v_617 (Climate and Environment) per party
sum_v617 <- c(
  sum(data3[data3$v_372 == 3, "v_617"], na.rm = TRUE),
  sum(data3[data3$v_372 == 1, "v_617"], na.rm = TRUE),
  sum(data3[data3$v_372 == 4, "v_617"], na.rm = TRUE),
  sum(data3[data3$v_372 == 2, "v_617"], na.rm = TRUE),
  sum(data3[data3$v_372 == 6, "v_617"], na.rm = TRUE),
  sum(data3[data3$v_372 == 5, "v_617"], na.rm = TRUE),
  sum(data3[data3$v_372 == 7, "v_617"], na.rm = TRUE),
  sum(data3[data3$v_372 == 8, "v_617"], na.rm = TRUE),
  sum(data3[data3$v_372 == 9, "v_617"], na.rm = TRUE)
)

# Average
values_v617 <- sum_v617 / number_partynames 

# Dataframe for ggplot
df_v617 <- data.frame(
  Party = factor(party_names, levels = party_names),  
  Score = values_v617
)

# Barplot 
barplot_v617 <- ggplot(df_v617, aes(x = Party, y = Score, fill = Party)) +
  geom_hline(yintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +  
  ylim(0, 2.5) +   
  scale_x_discrete(expand = c(0.05, 0)) +  
  scale_fill_manual(values = colors_parties) +   
  labs(x = "", 
       y = "", 
       title = "Climate and Environment") +
  theme_linedraw() +  
  theme(
    panel.grid = element_blank(),        
    axis.text.x = element_blank(), 
    plot.title = element_text(size = 20, face = "plain"),
    legend.position = "none"  
  )

### v_618
# sum for v_618 (Health and Care) per party
sum_v618 <- c(
  sum(data3[data3$v_372 == 3, "v_618"], na.rm = TRUE),
  sum(data3[data3$v_372 == 1, "v_618"], na.rm = TRUE),
  sum(data3[data3$v_372 == 4, "v_618"], na.rm = TRUE),
  sum(data3[data3$v_372 == 2, "v_618"], na.rm = TRUE),
  sum(data3[data3$v_372 == 6, "v_618"], na.rm = TRUE),
  sum(data3[data3$v_372 == 5, "v_618"], na.rm = TRUE),
  sum(data3[data3$v_372 == 7, "v_618"], na.rm = TRUE),
  sum(data3[data3$v_372 == 8, "v_618"], na.rm = TRUE),
  sum(data3[data3$v_372 == 9, "v_618"], na.rm = TRUE)
)

# Average
values_v618 <- sum_v618 / number_partynames

# Dataframe for ggplot
df_v618 <- data.frame(
  Party = factor(party_names, levels = party_names),  
  Score = values_v618
)

# Barplot 
barplot_v618 <- ggplot(df_v618, aes(x = Party, y = Score, fill = Party)) +
  geom_hline(yintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +  
  ylim(0, 2.5) +   
  scale_x_discrete(expand = c(0.05, 0)) +  
  scale_fill_manual(values = colors_parties) +   
  labs(x = "", 
       y = "", 
       title = "Health and Care") +
  theme_linedraw() +  
  theme(
    panel.grid = element_blank(),        
    axis.text.x = element_blank(), 
    plot.title = element_text(size = 20, face = "plain"),
    legend.position = "none"  
  )

### v_619
# sum for v_619 (Unemployment) per party
sum_v619 <- c(
  sum(data3[data3$v_372 == 3, "v_619"], na.rm = TRUE),
  sum(data3[data3$v_372 == 1, "v_619"], na.rm = TRUE),
  sum(data3[data3$v_372 == 4, "v_619"], na.rm = TRUE),
  sum(data3[data3$v_372 == 2, "v_619"], na.rm = TRUE),
  sum(data3[data3$v_372 == 6, "v_619"], na.rm = TRUE),
  sum(data3[data3$v_372 == 5, "v_619"], na.rm = TRUE),
  sum(data3[data3$v_372 == 7, "v_619"], na.rm = TRUE),
  sum(data3[data3$v_372 == 8, "v_619"], na.rm = TRUE),
  sum(data3[data3$v_372 == 9, "v_619"], na.rm = TRUE)
)

# Average (number * 3)
values_v619 <- sum_v619 / number_partynames

# Dataframe for ggplot
df_v619 <- data.frame(
  Party = factor(party_names, levels = party_names),  
  Score = values_v619
)

# Barplot 
barplot_v619 <- ggplot(df_v619, aes(x = Party, y = Score, fill = Party)) +
  geom_hline(yintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +  
  ylim(0, 2.5) +   
  scale_x_discrete(expand = c(0.05, 0)) +  
  scale_fill_manual(values = colors_parties) +   
  labs(x = "", 
       y = "", 
       title = "Unemployment") +
  theme_linedraw() +  
  theme(
    panel.grid = element_blank(),        
    axis.text.x = element_blank(), 
    plot.title = element_text(size = 20, face = "plain"),
    legend.position = "none"  
  )

### v_620
# sum for v_620 (Digitization) per party
sum_v620 <- c(
  sum(data3[data3$v_372 == 3, "v_620"], na.rm = TRUE),
  sum(data3[data3$v_372 == 1, "v_620"], na.rm = TRUE),
  sum(data3[data3$v_372 == 4, "v_620"], na.rm = TRUE),
  sum(data3[data3$v_372 == 2, "v_620"], na.rm = TRUE),
  sum(data3[data3$v_372 == 6, "v_620"], na.rm = TRUE),
  sum(data3[data3$v_372 == 5, "v_620"], na.rm = TRUE),
  sum(data3[data3$v_372 == 7, "v_620"], na.rm = TRUE),
  sum(data3[data3$v_372 == 8, "v_620"], na.rm = TRUE),
  sum(data3[data3$v_372 == 9, "v_620"], na.rm = TRUE)
)

# Average (number * 3)
values_v620 <- sum_v620 / number_partynames

# Dataframe for ggplot
df_v620 <- data.frame(
  Party = factor(party_names, levels = party_names),  
  Score = values_v620
)

# Barplot 
barplot_v620 <- ggplot(df_v620, aes(x = Party, y = Score, fill = Party)) +
  geom_hline(yintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +  
  ylim(0, 2.5) +   
  scale_x_discrete(expand = c(0.05, 0)) +  
  scale_fill_manual(values = colors_parties) +   
  labs(x = "", 
       y = "", 
       title = "Digitization") +
  theme_linedraw() +  
  theme(
    panel.grid = element_blank(),        
    axis.text.x = element_blank(),  
    plot.title = element_text(size = 20, face = "plain"),
    legend.position = "none"  
  )

### v_621
# sum for v_621 (Pensions) per party
sum_v621 <- c(
  sum(data3[data3$v_372 == 3, "v_621"], na.rm = TRUE),
  sum(data3[data3$v_372 == 1, "v_621"], na.rm = TRUE),
  sum(data3[data3$v_372 == 4, "v_621"], na.rm = TRUE),
  sum(data3[data3$v_372 == 2, "v_621"], na.rm = TRUE),
  sum(data3[data3$v_372 == 6, "v_621"], na.rm = TRUE),
  sum(data3[data3$v_372 == 5, "v_621"], na.rm = TRUE),
  sum(data3[data3$v_372 == 7, "v_621"], na.rm = TRUE),
  sum(data3[data3$v_372 == 8, "v_621"], na.rm = TRUE),
  sum(data3[data3$v_372 == 9, "v_621"], na.rm = TRUE)
)

# Average
values_v621 <- sum_v621 / number_partynames

# Dataframe for ggplot
df_v621 <- data.frame(
  Party = factor(party_names, levels = party_names),  
  Score = values_v621
)

# Barplot 
barplot_v621 <- ggplot(df_v621, aes(x = Party, y = Score, fill = Party)) +
  geom_hline(yintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +  
  ylim(0, 2.5) +   
  scale_x_discrete(expand = c(0.05, 0)) +  
  scale_fill_manual(values = colors_parties) +   
  labs(x = "", 
       y = "Average Topic Score", 
       title = "Pensions") +
  theme_linedraw() +  
  theme(
    axis.title.y = element_text(size = 14),
    panel.grid = element_blank(),        
    axis.text.x = element_blank(), 
    plot.title = element_text(size = 20, face = "plain"),
    legend.position = "none"  
  )

### v_622
# sum for v_622 (Foreign and Defense Policy) per party
sum_v622 <- c(
  sum(data3[data3$v_372 == 3, "v_622"], na.rm = TRUE),
  sum(data3[data3$v_372 == 1, "v_622"], na.rm = TRUE),
  sum(data3[data3$v_372 == 4, "v_622"], na.rm = TRUE),
  sum(data3[data3$v_372 == 2, "v_622"], na.rm = TRUE),
  sum(data3[data3$v_372 == 6, "v_622"], na.rm = TRUE),
  sum(data3[data3$v_372 == 5, "v_622"], na.rm = TRUE),
  sum(data3[data3$v_372 == 7, "v_622"], na.rm = TRUE),
  sum(data3[data3$v_372 == 8, "v_622"], na.rm = TRUE),
  sum(data3[data3$v_372 == 9, "v_622"], na.rm = TRUE)
)

# Average
values_v622 <- sum_v622 / number_partynames

# Dataframe for ggplot
df_v622 <- data.frame(
  Party = factor(party_names, levels = party_names),  
  Score = values_v622
)

# Barplot 
barplot_v622 <- ggplot(df_v622, aes(x = Party, y = Score, fill = Party)) +
  geom_hline(yintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +  
  ylim(0, 2.5) +   
  scale_x_discrete(expand = c(0.05, 0)) +  
  scale_fill_manual(values = colors_parties) +   
  labs(x = "", 
       y = "", 
       title = "Foreign and Defense Policy") +
  theme_linedraw() +  
  theme(
    panel.grid = element_blank(),        
    axis.text.x = element_blank(), 
    plot.title = element_text(size = 20, face = "plain"),
    legend.position = "none"  
  )

### v_623
# sum for v_623 (Economic Policy) per party
sum_v623 <- c(
  sum(data3[data3$v_372 == 3, "v_623"], na.rm = TRUE),
  sum(data3[data3$v_372 == 1, "v_623"], na.rm = TRUE),
  sum(data3[data3$v_372 == 4, "v_623"], na.rm = TRUE),
  sum(data3[data3$v_372 == 2, "v_623"], na.rm = TRUE),
  sum(data3[data3$v_372 == 6, "v_623"], na.rm = TRUE),
  sum(data3[data3$v_372 == 5, "v_623"], na.rm = TRUE),
  sum(data3[data3$v_372 == 7, "v_623"], na.rm = TRUE),
  sum(data3[data3$v_372 == 8, "v_623"], na.rm = TRUE),
  sum(data3[data3$v_372 == 9, "v_623"], na.rm = TRUE)
)

# Average (number * 3)
values_v623 <- sum_v623 / number_partynames

# Dataframe for ggplot
df_v623 <- data.frame(
  Party = factor(party_names, levels = party_names),  
  Score = values_v623
)

# Barplot 
barplot_v623 <- ggplot(df_v623, aes(x = Party, y = Score, fill = Party)) +
  geom_hline(yintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +  
  ylim(0, 2.5) +   
  scale_x_discrete(expand = c(0.05, 0)) +  
  scale_fill_manual(values = colors_parties) +   
  labs(x = "", 
       y = "", 
       title = "Economic Policy") +
  theme_linedraw() +  
  theme(
    panel.grid = element_blank(),        
    axis.text.x = element_blank(),
    plot.title = element_text(size = 20, face = "plain"), 
    legend.position = "none"  
  )

# Combine as Small Multiple Plot with legend
df_colors_parties <- data.frame(
  Party = names(colors_parties),
  Color = colors_parties
)

# Party as factor
df_colors_parties$Party <- factor(df_colors_parties$Party, levels = names(colors_parties))

# Plot for legend
legend_plot <- ggplot(df_colors_parties, aes(x = Party, y = 0, color = Party)) +
  geom_point(size = 6, show.legend = FALSE) +  
  scale_color_manual(values = colors_parties) +  
  theme_minimal() + 
  theme(
    plot.background = element_rect(fill = "white", color = NA),  
    panel.background = element_rect(fill = "white", color = NA),  
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 14),
    axis.title = element_blank(),  
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 20, face = "plain")
  ) +
  labs(title = "")

legend_plot <- legend_plot + coord_fixed(ratio = 5)

empty_plot <- plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "")

# Plots in one list
Topics_by_party <- plot_grid(
  barplot_v611, barplot_v612, barplot_v622, 
  barplot_v613, barplot_v618, barplot_v619, 
  barplot_v615, barplot_v617, barplot_v620, 
  barplot_v614, barplot_v616, barplot_v623, 
  barplot_v621, empty_plot, legend_plot,  
  ncol = 3
)

ggsave("Topics_by_party.png", Topics_by_party, width = 15, height = 20)

# Most Important Topic according to age (data4) ----
data4 <- data3

# Create a new variable called age_class
data4 <- data4 %>%
  mutate(age_class = case_when(
    v_630 >= 2004 & v_630 <= 2007 ~ 1,
    v_630 >= 1995 & v_630 <= 2003 ~ 2,
    v_630 >= 1980 & v_630 <= 1994 ~ 3,
    v_630 >= 1965 & v_630 <= 1979 ~ 4,
    v_630 <= 1964 ~ 5,
    TRUE ~ NA_real_  
  ))

# number for each class
number1_ClassA <- sum(data4$age_class  == 1, na.rm = TRUE) # 114
number2_ClassB <- sum(data4$age_class  == 2, na.rm = TRUE) # 466
number3_ClassC <- sum(data4$age_class  == 3, na.rm = TRUE) # 896
number4_ClassD <- sum(data4$age_class  == 4, na.rm = TRUE) # 1404
number5_ClassE <- sum(data4$age_class  == 5, na.rm = TRUE) # 1240

### Class A (2004-2007)
# sum 
sumA <- colSums(data4[data4$age_class  == 1, variables], na.rm = TRUE)
# Average 
valuesA <- sumA / number1_ClassA

### Class B 1995-2003
sumB <- colSums(data4[data4$age_class  == 2, variables], na.rm = TRUE)
valuesB <- sumB / number2_ClassB 

### Class C 1980-1994
sumC <- colSums(data4[data4$age_class  == 3, variables], na.rm = TRUE)
valuesC <- sumC / number3_ClassC

### Class D 1965-1979
sumD <- colSums(data4[data4$age_class  == 4, variables], na.rm = TRUE)
valuesD <- sumD / number4_ClassD

### Class E 1964 and older
sumE <- colSums(data4[data4$age_class  == 5, variables], na.rm = TRUE)
valuesE <- sumE / number5_ClassE

sum_age <- c(sumA, sumB, sumC, sumD, sumE)
values_age <- c(valuesA, valuesB, valuesC, valuesD, valuesE)
ageclass  <- c("18 - 20/21", "21/22 - 29/30", "30/31 - 44/45", "45/46 - 59/60", "60+")
colors_age <- c(
  "18 - 20/21" = "gray30",  
  "21/22 - 29/30" = "slategray4",
  "30/31 - 44/45" = "slategray3", 
  "45/46 - 59/60" = "slategray2",
  "60+" = "slategray1" 
)

# Dataframe for ggplot
df_age <- data.frame(
  Topic = factor(variables_labels, levels = rev(variables_labels)),
  Score = values_age,
  Age = rep(ageclass , each = length(variables_labels))
) 

df_age <- df_age[order(df_age$Age, decreasing = TRUE), ]

plot_age <- ggplot(df_age, aes(x = Score, y = Topic, color = Age, size = Age)) +
  geom_point(shape = 16) +
  xlim(0, 2.5) + 
  scale_color_manual(values = colors_age) +
  scale_size_manual(values = c(5, 8, 11, 14, 17)) + 
  labs(
    x = "Average Topic Scores",
    y = ""
  ) + 
  theme_minimal() + 
  theme(
    plot.background = element_rect(fill = "white", color = NA),  
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom",
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.title = element_blank()
  )
plot_age

ggsave("Age.png", plot_age, width = 15, height = 10)


# Most Important Topic according to High School Degree ----

# number per lass 
number1_NoHSD <- sum(data4$v_22 == 1, na.rm = TRUE) # 10 
number2_School <- sum(data4$v_22 == 2, na.rm = TRUE) # 16
number3_Haupt <- sum(data4$v_22 == 3, na.rm = TRUE) # 210
number4_Real <- sum(data4$v_22 == 4, na.rm = TRUE) # 826
number5_Abi <- sum(data4$v_22 == 5, na.rm = TRUE) # 3058

### Hauptschuldegree
# sum and average
sum_Haupt <- colSums(data4[data4$v_22 == 3, variables], na.rm = TRUE)
values_Haupt <- sum_Haupt / number3_Haupt

### Realschulabschluss
sum_Real <- colSums(data4[data4$v_22 == 4, variables], na.rm = TRUE)
values_Real <- sum_Real / number4_Real 

### Abitur, Fachhochschulreife, Hochschulreife
sum_Abi <- colSums(data4[data4$v_22 == 5, variables], na.rm = TRUE)
values_Abi <- sum_Abi / number5_Abi

sum_HS <- c(sum_Haupt, sum_Real, sum_Abi)
values_HS <- c(values_Haupt, values_Real, values_Abi)
hsdegree <- c("Hauptschuldegree", "Realschuldegree", "Abitur & (Fach-)Hochschulreife")
colors_highschool <- c(
  "Hauptschuldegree" = "black", 
  "Realschuldegree" = "slategray4",
  "Abitur & (Fach-)Hochschulreife" = "slategray1" 
)

# Dataframe for ggplot
df_highschool <- data.frame(
  Topic = factor(variables_labels, levels = rev(variables_labels)),
  Score = values_HS,
  Highschool = rep(hsdegree, each = length(variables_labels))
) 

df_highschool$Highschool <- as.factor(df_highschool$Highschool)

df_highschool$Highschool <- factor(df_highschool$Highschool, levels = c("Hauptschuldegree", "Realschuldegree", "Abitur & (Fach-)Hochschulreife"))

df_highschool <- df_highschool[order(df_highschool$Highschool, decreasing = TRUE), ]


plot_highschool <- ggplot(df_highschool, aes(x = Score, y = Topic, color = Highschool, size = Highschool)) +
  geom_point(shape = 16) +
  xlim(0., 2.5) + 
  scale_color_manual(values = colors_highschool) +
  scale_size_manual(values = c(7, 11, 15)) + 
  labs(
    x = "Average Topic Scores",
    y = ""
  ) + 
  theme_minimal() + 
  theme(
    plot.background = element_rect(fill = "white", color = NA),  # Weißer Hintergrund
    panel.background = element_rect(fill = "white", color = NA),# Weißer Hintergrund
    legend.position = "bottom",
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.title = element_blank()
  )
plot_highschool

ggsave("HSDegree.png", plot_highschool, width = 15, height = 10)

# Most Important Topic according to Vocational Education ----

# numbers per degree
number1_None <- sum(data4$Voc_ed == 1, na.rm = TRUE) # 305
number2_Apprentice <- sum(data4$Voc_ed == 2, na.rm = TRUE) # 1719
number3_Meister <- sum(data4$Voc_ed == 3, na.rm = TRUE) # 292
number4_Bachelor <- sum(data4$Voc_ed == 4, na.rm = TRUE) # 377
number5_Master <- sum(data4$Voc_ed == 5, na.rm = TRUE) # 1262
number6_PhD <- sum(data4$Voc_ed == 6, na.rm = TRUE) # 165

### No higher degree
# sum and average
sum_None <- colSums(data4[data4$Voc_ed == 1, variables], na.rm = TRUE)
values_None <- sum_None / number1_None

# Dataframe 
df_None <- data.frame(
  Topic = factor(variables_labels, levels = rev(variables_labels)),    
  Score = values_None
)

# Barplot
barplot_None <- ggplot(df_None, aes(x = Score, y = Topic)) +
  geom_vline(xintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", fill = "gray50", color = "black", width = 0.7) +   
  xlim(0, 2.5) +    
  scale_y_discrete(expand = c(0, 2)) +
  labs(x = "", 
       y = "", 
       title = "No Further Diploma") +
  theme_linedraw() +    
  theme(
    panel.grid = element_blank(),          
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    plot.title = element_text(size = 20, face = "plain")
  )

### Finished Apprenticeship / Fachschulabschluss
# sum and average
sum_Apprentice <- colSums(data4[data4$Voc_ed == 2, variables], na.rm = TRUE)
values_Apprentice <- sum_Apprentice / number2_Apprentice

# Dataframe
df_Apprentice <- data.frame(
  Topic = factor(variables_labels, levels = rev(variables_labels)),    
  Score = values_Apprentice
)

# Barplot
barplot_Apprentice <- ggplot(df_Apprentice, aes(x = Score, y = Topic)) +
  geom_vline(xintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", fill = "gray50", color = "black", width = 0.7) +   
  xlim(0, 2.5) +    
  scale_y_discrete(expand = c(0, 2)) +
  labs(x = "", 
       y = "", 
       title = "Apprenticeship") +
  theme_linedraw() +    
  theme(
    panel.grid = element_blank(),          
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    plot.title = element_text(size = 20, face = "plain")
  )

### Meister
# sum and average
sum_Meister <- colSums(data4[data4$Voc_ed == 3, variables], na.rm = TRUE)
values_Meister <- sum_Meister / number3_Meister

 # Dataframe 
df_Meister <- data.frame(
  Topic = factor(variables_labels, levels = rev(variables_labels)),    
  Score = values_Meister
)

# Barplot
barplot_Meister <- ggplot(df_Meister, aes(x = Score, y = Topic)) +
  geom_vline(xintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", fill = "gray50", color = "black", width = 0.7) +   
  xlim(0, 2.5) +    
  scale_y_discrete(expand = c(0, 2)) +
  labs(x = "", 
       y = "", 
       title = "Meister") +
  theme_linedraw() +    
  theme( 
    panel.grid = element_blank(),          
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    plot.title = element_text(size = 20, face = "plain")
  )

### Bachelor
# sum and average
sum_Bachelor <- colSums(data4[data4$Voc_ed == 4, variables], na.rm = TRUE)
values_Bachelor <- sum_Bachelor / number4_Bachelor

 # Dataframe
df_Bachelor <- data.frame(
  Topic = factor(variables_labels, levels = rev(variables_labels)),    
  Score = values_Bachelor
)

# Barplot
barplot_Bachelor <- ggplot(df_Bachelor, aes(x = Score, y = Topic)) +
  geom_vline(xintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", fill = "gray50", color = "black", width = 0.7) +   
  xlim(0, 2.5) +    
  scale_y_discrete(expand = c(0, 2)) +
  labs(x = "Average Topic Scores", 
       y = "", 
       title = "Bachelor") +
  theme_linedraw() +    
  theme(
    panel.grid = element_blank(),          
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    plot.title = element_text(size = 20, face = "plain")
  )

### Master, Magister, Staatsexamen etc. 
# sum and average
sum_Master <- colSums(data4[data4$Voc_ed == 5, variables], na.rm = TRUE)
values_Master <- sum_Master / number5_Master

 # Dataframe
df_Master <- data.frame(
  Topic = factor(variables_labels, levels = rev(variables_labels)),    
  Score = values_Master
)

# Barplot
barplot_Master <- ggplot(df_Master, aes(x = Score, y = Topic)) +
  geom_vline(xintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", fill = "gray50", color = "black", width = 0.7) +   
  xlim(0, 2.5) +    
  scale_y_discrete(expand = c(0, 2)) +
  labs(x = "Average Topic Scores", 
       y = "", 
       title = "Master") +
  theme_linedraw() +    
  theme(
    panel.grid = element_blank(),          
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    plot.title = element_text(size = 20, face = "plain")
  )

### Promotion
# sum and average
sum_PhD <- colSums(data4[data4$Voc_ed == 6, variables], na.rm = TRUE)
values_PhD <- sum_PhD / number6_PhD

 # Dataframe
df_PhD <- data.frame(
  Topic = factor(variables_labels, levels = rev(variables_labels)),    
  Score = values_PhD
)

# Barplot
barplot_PhD <- ggplot(df_PhD, aes(x = Score, y = Topic)) +
  geom_vline(xintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", fill = "gray50", color = "black", width = 0.7) +   
  xlim(0, 2.5) +    
  scale_y_discrete(expand = c(0, 2)) +
  labs(x = "Average Topic Scores", 
       y = "", 
       title = "PhD") +
  theme_linedraw() +    
  theme(
    panel.grid = element_blank(),          
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    plot.title = element_text(size = 20, face = "plain")
  )

Voc_ed_graduation <- barplot_None + barplot_Apprentice + barplot_Meister + barplot_Bachelor + barplot_Master + barplot_PhD

ggsave("Voc_ed_graduation.png", Voc_ed_graduation, width = 15, height = 10)




# Most Important Topic for vocational education according to topics ----

# party_names-Namen
voceddegree <- c("No further degree", 
                    "Apprenticeship", 
                    "Meister Degree", 
                    "Bachelor", 
                    "Master or similar", 
                    "PhD")

# number der Befragten pro Party (vorher berechnet)
number_voceddegree <- c(number1_None, number2_Apprentice, 
                           number3_Meister, number4_Bachelor, 
                           number5_Master, number6_PhD)

# Farbcodierung für die party_names
colors_voced <- c(
  "No further degree" = "gray30",
  "Apprenticeship" = "slategray2",
  "Meister Degree" = "slategray4",
  "Bachelor" = "darkseagreen2",
  "Master or similar" = "darkseagreen3",
  "PhD" = "darkseagreen4"
)


### v_611
# sum for v_611 (Domestic Security) 
sum_v611voced <- c(
  sum(data4[data4$Voc_ed == 1, "v_611"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 2, "v_611"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 3, "v_611"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 4, "v_611"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 5, "v_611"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 6, "v_611"], na.rm = TRUE)
)

# Average 
values_v611voced <- sum_v611voced / number_voceddegree

# Dataframe
df_v611voced <- data.frame(
  Vocdegree = factor(voceddegree, levels = voceddegree), 
  Score_voced = values_v611voced
)

# Barplot 
barplot_v611voced <- ggplot(df_v611voced, aes(x = Vocdegree, y = Score_voced, fill = Vocdegree)) +
  geom_hline(yintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +  
  ylim(0, 2.5) +   
  scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +  
  scale_fill_manual(values = colors_voced) +   
  labs(x = "", 
       y = "Average Topic Score", 
       title = "Domestic Security") +
  theme_linedraw() +  
  theme(
    axis.title.y = element_text(size = 14),
    panel.grid = element_blank(),        
    axis.text.x = element_blank(), 
    plot.title = element_text(size = 20, face = "plain"),
    legend.position = "none" 
  )


### v_612
# sum for v_612 (Immigration)
sum_v612voced <- c(
  sum(data4[data4$Voc_ed == 1, "v_612"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 2, "v_612"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 3, "v_612"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 4, "v_612"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 5, "v_612"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 6, "v_612"], na.rm = TRUE)
)

# Average
values_v612voced <- sum_v612voced / number_voceddegree

# Dataframe
df_v612voced <- data.frame(
  Vocdegree = factor(voceddegree, levels = voceddegree), 
  Score_voced = values_v612voced
)

# Barplot 
barplot_v612voced <- ggplot(df_v612voced, aes(x = Vocdegree, y = Score_voced, fill = Vocdegree)) +
  geom_hline(yintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +  
  ylim(0, 2.5) +   
  scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +  
  scale_fill_manual(values = colors_voced) +   
  labs(x = "", 
       y = "", 
       title = "(Im)migration") +
  theme_linedraw() +  
  theme(
    axis.title.y = element_blank(),
    panel.grid = element_blank(),        
    axis.text.x = element_blank(), 
    plot.title = element_text(size = 20, face = "plain"),
    legend.position = "none" 
  )


### v_613
# sum for v_613 (Social Justice)
sum_v613voced <- c(
  sum(data4[data4$Voc_ed == 1, "v_613"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 2, "v_613"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 3, "v_613"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 4, "v_613"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 5, "v_613"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 6, "v_613"], na.rm = TRUE)
)

# Average 
values_v613voced <- sum_v613voced / number_voceddegree

# Dataframe
df_v613voced <- data.frame(
  Vocdegree = factor(voceddegree, levels = voceddegree), 
  Score_voced = values_v613voced
)

# Barplot 
barplot_v613voced <- ggplot(df_v613voced, aes(x = Vocdegree, y = Score_voced, fill = Vocdegree)) +
  geom_hline(yintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +  
  ylim(0, 2.5) +   
  scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +  
  scale_fill_manual(values = colors_voced) +   
  labs(x = "", 
       y = "", 
       title = "Social Justice/Inequality") +
  theme_linedraw() +  
  theme(
    axis.title.y = element_text(size = 14),
    panel.grid = element_blank(),        
    axis.text.x = element_blank(), 
    plot.title = element_text(size = 20, face = "plain"),
    legend.position = "none" 
  )

### v_614
# sum for v_614 (Taxes)
sum_v614voced <- c(
  sum(data4[data4$Voc_ed == 1, "v_614"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 2, "v_614"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 3, "v_614"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 4, "v_614"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 5, "v_614"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 6, "v_614"], na.rm = TRUE)
)

# Average 
values_v614voced <- sum_v614voced / number_voceddegree

# Dataframe
df_v614voced <- data.frame(
  Vocdegree = factor(voceddegree, levels = voceddegree), 
  Score_voced = values_v614voced
)

# Barplot 
barplot_v614voced <- ggplot(df_v614voced, aes(x = Vocdegree, y = Score_voced, fill = Vocdegree)) +
  geom_hline(yintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +  
  ylim(0, 2.5) +   
  scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +  
  scale_fill_manual(values = colors_voced) +   
  labs(x = "", 
       y = "Average Topic Score", 
       title = "Tax and Duty Burden") +
  theme_linedraw() +  
  theme(
    axis.title.y = element_text(size = 14),
    panel.grid = element_blank(),        
    axis.text.x = element_blank(), 
    plot.title = element_text(size = 20, face = "plain"),
    legend.position = "none" 
  )

### v_615
# sum for v_615 (Education)
sum_v615voced <- c(
  sum(data4[data4$Voc_ed == 1, "v_615"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 2, "v_615"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 3, "v_615"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 4, "v_615"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 5, "v_615"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 6, "v_615"], na.rm = TRUE)
)

# Average
values_v615voced <- sum_v615voced / number_voceddegree

# Dataframe
df_v615voced <- data.frame(
  Vocdegree = factor(voceddegree, levels = voceddegree), 
  Score_voced = values_v615voced
)

# Barplot 
barplot_v615voced <- ggplot(df_v615voced, aes(x = Vocdegree, y = Score_voced, fill = Vocdegree)) +
  geom_hline(yintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +  
  ylim(0, 2.5) +   
  scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +  
  scale_fill_manual(values = colors_voced) +   
  labs(x = "", 
       y = "Average Topic Score", 
       title = "Education and School Policy") +
  theme_linedraw() +  
  theme(
    axis.title.y = element_text(size = 14),
    panel.grid = element_blank(),        
    axis.text.x = element_blank(), 
    plot.title = element_text(size = 20, face = "plain"),
    legend.position = "none" 
  )

### v_616
# sum for v_616 (Infrastructure)
sum_v616voced <- c(
  sum(data4[data4$Voc_ed == 1, "v_616"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 2, "v_616"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 3, "v_616"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 4, "v_616"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 5, "v_616"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 6, "v_616"], na.rm = TRUE)
)

# Average
values_v616voced <- sum_v616voced / number_voceddegree

# Dataframe 
df_v616voced <- data.frame(
  Vocdegree = factor(voceddegree, levels = voceddegree), 
  Score_voced = values_v616voced
)

# Barplot 
barplot_v616voced <- ggplot(df_v616voced, aes(x = Vocdegree, y = Score_voced, fill = Vocdegree)) +
  geom_hline(yintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +  
  ylim(0, 2.5) +   
  scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +  
  scale_fill_manual(values = colors_voced) +   
  labs(x = "", 
       y = "", 
       title = "Traffic and Infrastructure") +
  theme_linedraw() +  
  theme(
    axis.title.y = element_text(size = 14),
    panel.grid = element_blank(),        
    axis.text.x = element_blank(), 
    plot.title = element_text(size = 20, face = "plain"),
    legend.position = "none" 
  )

### v_617
# sum for v_617 (Climate and Environment)
sum_v617voced <- c(
  sum(data4[data4$Voc_ed == 1, "v_617"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 2, "v_617"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 3, "v_617"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 4, "v_617"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 5, "v_617"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 6, "v_617"], na.rm = TRUE)
)

# Average
values_v617voced <- sum_v617voced / number_voceddegree

# Dataframe
df_v617voced <- data.frame(
  Vocdegree = factor(voceddegree, levels = voceddegree), 
  Score_voced = values_v617voced
)

# Barplot 
barplot_v617voced <- ggplot(df_v617voced, aes(x = Vocdegree, y = Score_voced, fill = Vocdegree)) +
  geom_hline(yintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +  
  ylim(0, 2.5) +   
  scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +  
  scale_fill_manual(values = colors_voced) +   
  labs(x = "", 
       y = "", 
       title = "Climate and Environment") +
  theme_linedraw() +  
  theme(
    axis.title.y = element_text(size = 14),
    panel.grid = element_blank(),        
    axis.text.x = element_blank(), 
    plot.title = element_text(size = 20, face = "plain"),
    legend.position = "none" 
  )

### v_618
# sum for v_618 (Health and Care)
sum_v618voced <- c(
  sum(data4[data4$Voc_ed == 1, "v_618"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 2, "v_618"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 3, "v_618"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 4, "v_618"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 5, "v_618"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 6, "v_618"], na.rm = TRUE)
)

# Average
values_v618voced <- sum_v618voced / number_voceddegree

# Dataframe
df_v618voced <- data.frame(
  Vocdegree = factor(voceddegree, levels = voceddegree), 
  Score_voced = values_v618voced
)

# Barplot 
barplot_v618voced <- ggplot(df_v618voced, aes(x = Vocdegree, y = Score_voced, fill = Vocdegree)) +
  geom_hline(yintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +  
  ylim(0, 2.5) +   
  scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +  
  scale_fill_manual(values = colors_voced) +   
  labs(x = "", 
       y = "", 
       title = "Health and Care") +
  theme_linedraw() +  
  theme(
    axis.title.y = element_text(size = 14),
    panel.grid = element_blank(),        
    axis.text.x = element_blank(), 
    plot.title = element_text(size = 20, face = "plain"),
    legend.position = "none" 
  )

### v_619
# sum for v_619 (Unemployment)
sum_v619voced <- c(
  sum(data4[data4$Voc_ed == 1, "v_619"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 2, "v_619"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 3, "v_619"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 4, "v_619"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 5, "v_619"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 6, "v_619"], na.rm = TRUE)
)

# Average
values_v619voced <- sum_v619voced / number_voceddegree

# Dataframe
df_v619voced <- data.frame(
  Vocdegree = factor(voceddegree, levels = voceddegree), 
  Score_voced = values_v619voced
)

# Barplot 
barplot_v619voced <- ggplot(df_v619voced, aes(x = Vocdegree, y = Score_voced, fill = Vocdegree)) +
  geom_hline(yintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +  
  ylim(0, 2.5) +   
  scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +  
  scale_fill_manual(values = colors_voced) +   
  labs(x = "", 
       y = "", 
       title = "Uneployment") +
  theme_linedraw() +  
  theme(
    axis.title.y = element_text(size = 14),
    panel.grid = element_blank(),        
    axis.text.x = element_blank(), 
    plot.title = element_text(size = 20, face = "plain"),
    legend.position = "none" 
  )

### v_620
# sum for v_620 (Digitization)
sum_v620voced <- c(
  sum(data4[data4$Voc_ed == 1, "v_620"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 2, "v_620"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 3, "v_620"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 4, "v_620"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 5, "v_620"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 6, "v_620"], na.rm = TRUE)
)

# Average
values_v620voced <- sum_v620voced / number_voceddegree

# Dataframe
df_v620voced <- data.frame(
  Vocdegree = factor(voceddegree, levels = voceddegree), 
  Score_voced = values_v620voced
)

# Barplot 
barplot_v620voced <- ggplot(df_v620voced, aes(x = Vocdegree, y = Score_voced, fill = Vocdegree)) +
  geom_hline(yintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +  
  ylim(0, 2.5) +   
  scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +  
  scale_fill_manual(values = colors_voced) +   
  labs(x = "", 
       y = "", 
       title = "Digitization") +
  theme_linedraw() +  
  theme(
    axis.title.y = element_text(size = 14),
    panel.grid = element_blank(),        
    axis.text.x = element_blank(), 
    plot.title = element_text(size = 20, face = "plain"),
    legend.position = "none" 
  )

### v_621
# sum for v_621 (Pensions)
sum_v621voced <- c(
  sum(data4[data4$Voc_ed == 1, "v_621"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 2, "v_621"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 3, "v_621"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 4, "v_621"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 5, "v_621"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 6, "v_621"], na.rm = TRUE)
)

# Average
values_v621voced <- sum_v621voced / number_voceddegree 

# Dataframe
df_v621voced <- data.frame(
  Vocdegree = factor(voceddegree, levels = voceddegree), 
  Score_voced = values_v621voced
)

# Barplot 
barplot_v621voced <- ggplot(df_v621voced, aes(x = Vocdegree, y = Score_voced, fill = Vocdegree)) +
  geom_hline(yintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +  
  ylim(0, 2.5) +   
  scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +  
  scale_fill_manual(values = colors_voced) +   
  labs(x = "", 
       y = "Average Topic Score", 
       title = "Pensions") +
  theme_linedraw() +  
  theme(
    axis.title.y = element_text(size = 14),
    panel.grid = element_blank(),        
    axis.text.x = element_blank(), 
    plot.title = element_text(size = 20, face = "plain"),
    legend.position = "none" 
  )

### v_622
# sum for v_622 (Foreign)
sum_v622voced <- c(
  sum(data4[data4$Voc_ed == 1, "v_622"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 2, "v_622"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 3, "v_622"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 4, "v_622"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 5, "v_622"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 6, "v_622"], na.rm = TRUE)
)

# Average
values_v622voced <- sum_v622voced / number_voceddegree

# Dataframe
df_v622voced <- data.frame(
  Vocdegree = factor(voceddegree, levels = voceddegree), 
  Score_voced = values_v622voced
)

# Barplot 
barplot_v622voced <- ggplot(df_v622voced, aes(x = Vocdegree, y = Score_voced, fill = Vocdegree)) +
  geom_hline(yintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +  
  ylim(0, 2.5) +   
  scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +  
  scale_fill_manual(values = colors_voced) +   
  labs(x = "", 
       y = "", 
       title = "Foreign and Defense Policy") +
  theme_linedraw() +  
  theme(
    axis.title.y = element_text(size = 14),
    panel.grid = element_blank(),        
    axis.text.x = element_blank(), 
    plot.title = element_text(size = 20, face = "plain"),
    legend.position = "none" 
  )

### v_623
# sum for v_623 (Economic Policy)
sum_v623voced <- c(
  sum(data4[data4$Voc_ed == 1, "v_623"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 2, "v_623"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 3, "v_623"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 4, "v_623"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 5, "v_623"], na.rm = TRUE),
  sum(data4[data4$Voc_ed == 6, "v_623"], na.rm = TRUE)
)

# Average
values_v623voced <- sum_v623voced / number_voceddegree

# Dataframe
df_v623voced <- data.frame(
  Vocdegree = factor(voceddegree, levels = voceddegree), 
  Score_voced = values_v623voced
)

# Barplot 
barplot_v623voced <- ggplot(df_v623voced, aes(x = Vocdegree, y = Score_voced, fill = Vocdegree)) +
  geom_hline(yintercept = seq(0, 2.5, by = 0.5), color = "gray81", linetype = "dashed", linewidth = 0.5) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +  
  ylim(0, 2.5) +   
  scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +  
  scale_fill_manual(values = colors_voced) +   
  labs(x = "", 
       y = "", 
       title = "Economic Policy") +
  theme_linedraw() +  
  theme(
    axis.title.y = element_text(size = 14),
    panel.grid = element_blank(),        
    axis.text.x = element_blank(), 
    plot.title = element_text(size = 20, face = "plain"),
    legend.position = "none" 
  )

# Create small multiples with legend
# Legend
df_colors_voced <- data.frame(
  Vocdegree = names(colors_voced),
  Color = colors_voced
)

# Party as factor
df_colors_voced$Vocdegree <- factor(df_colors_voced$Vocdegree, levels = names(colors_voced))

# Plot der Legende
legend_plot_voced <- ggplot(df_colors_voced, aes(x = Vocdegree, y = 1, color = Vocdegree)) +
  ylim(0, 1) + 
  geom_point(size = 6, show.legend = FALSE) +  # Punkte statt Tiles, Größe anpassen
  geom_text(aes(label = Vocdegree),
            angle = 0,
            hjust = 0.5, vjust = 2,
            size = 6, color = "black") +
  scale_color_manual(values = colors_voced, guide = "none") +  # Manuelle Farben für die Punkte
  theme_minimal() +  # Minimalistisches Theme
  theme(
    axis.text.x = element_blank(),
    axis.title = element_blank(), 
    axis.text.y = element_blank(), 
    panel.grid = element_blank(),
    plot.title = element_text(size = 20, face = "plain", hjust = 4)
  ) +
  labs(title = "Legende") + 
  coord_cartesian(clip = "off")
legend_plot_voced

empty_plot <- plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "")

plots_row <- plot_grid(
  barplot_v611voced, barplot_v612voced, 
  barplot_v617voced, barplot_v613voced,  
  ncol = 4  
)

legend_row <- plot_grid(
  legend_plot_voced,  
  ncol = 1  
)

# Plots in einer Liste speichern
SelectedTopics_by_voced <- plot_grid(
  plots_row, legend_row,  
  ncol = 1, 
  rel_heights = c(5, 1) 
)

ggsave("SelectedTopics_by_voced.png", SelectedTopics_by_voced, width = 15, height = 6)

# Vollständige Darstellung
# Legende erstellen
df_colors_voced2 <- data.frame(
  Vocdegree = names(colors_voced),
  Farbe = colors_voced
)

# Zweite Legende

legend_plot_voced2 <- ggplot(df_colors_voced, aes(x = 0.1, y = Vocdegree, color = Vocdegree)) +
  xlim(0, 1) + 
  geom_point(size = 6, show.legend = FALSE) +
  scale_color_manual(values = colors_voced) +  # Manuelle Farben für die Punkte
  theme_minimal() +  # Minimalistisches Theme
  theme(
    axis.text.x = element_blank(),
    axis.title = element_blank(), 
    axis.text.y = element_text(size = 15, hjust = 1), 
    panel.grid = element_blank(),
    plot.title = element_text(size = 20, face = "plain", hjust = 4)
  ) +
  coord_cartesian(clip = "off")
legend_plot_voced2

# Plots in einer Liste speichern
Topics_by_voced <- plot_grid(
  barplot_v611voced, barplot_v612voced, barplot_v622voced, 
  barplot_v613voced, barplot_v618voced, barplot_v619voced, 
  barplot_v615voced, barplot_v617voced, barplot_v620voced, 
  barplot_v614voced, barplot_v616voced, barplot_v623voced, 
  barplot_v621voced, empty_plot, legend_plot_voced2,
  ncol = 3  # Setzt genau 3 Plots pro Reihe
)

ggsave("Topics_by_voced.png", Topics_by_voced, width = 15, height = 20)


# Most Important Topic according to Gender (data5) ----
# Check numbers for non-binary and no info
number3_Nonbinary <- sum(data4$Geschlecht == 3, na.rm = TRUE) # 36 Leute
number4_noinfo <- sum(data4$Geschlecht == 4, na.rm = TRUE) # 89 Leute

data5 <- subset(data4, Geschlecht < 3) 
data5 <- subset(data5, Geschlecht > 0)

# number pro Class ausrechnen
number1_Men <- sum(data5$Geschlecht == 1, na.rm = TRUE) # 2614
number2_Women <- sum(data5$Geschlecht == 2, na.rm = TRUE) # 1380

### Men
sum_Men <- colSums(data5[data5$Geschlecht == 1, variables], na.rm = TRUE)
values_Men <- sum_Men / number1_Men

### Women
sum_Women <- colSums(data5[data5$Geschlecht == 2, variables], na.rm = TRUE)
values_Women <- sum_Women / number2_Women

sum_gender <- c(sum_Men, sum_Women)
values_gender <- c(values_Men, values_Women)
gender <- c("Men", "Women")
farben_gender <- c(
  "Men" = "lightskyblue3", 
  "Women" = "indianred3"
)

# Dataframe for ggplot
df_gender <- data.frame(
  Topic = factor(variables_labels, levels = rev(variables_labels)),
  Score = values_gender,
  Gender = rep(gender, each = length(variables_labels))
)

plot_gender <- ggplot(df_gender, aes(x = Score, y = Topic, color = Gender)) +
  geom_point(shape = 16, size = 10) +
  xlim(0., 2.5) + 
  scale_color_manual(values = farben_gender) +
  labs(
    x = "Average Topic Scores",
    y = ""
  ) + 
  theme_minimal() + 
  theme(
    plot.background = element_rect(fill = "white", color = NA),  # Weißer Hintergrund
    panel.background = element_rect(fill = "white", color = NA),# Weißer Hintergrund
    legend.position = "bottom",
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.title = element_blank()
  )

ggsave("Gender.png", plot_gender, width = 15, height = 10)


# New Variables for priorities as 1/0, gender as 1/0, party preference as factor variable and vocational education as factor variable ----

# Priorities v_611 to v_623 as Dummies
for (i in 611:623) {
  var_name <- paste0("v_", i)
  dummy_name <- paste0("Dummy_", var_name)
  data5[[dummy_name]] <- ifelse(data5[[var_name]] %in% c(1, 2, 3), 1, 0)
}

# Gender as Dummies
data5 <- data5 %>%
  mutate(Dummy_Gender = case_when(
    Geschlecht == 1 ~ 0,
    Geschlecht == 2 ~ 1
  ))

table(data5$Dummy_Gender)

# Parties as Categorial variables
data5 <- data5 %>%
  mutate(Parties = case_when(
    v_372 == 1 ~ "AfD",
    v_372 == 2 ~ "Greens",
    v_372 == 3 ~ "CDU/CSU",
    v_372 == 4 ~ "SPD",
    v_372 == 5 ~ "FDP",
    v_372 == 6 ~ "The Left",
    v_372 == 7 ~ "BSW",
    v_372 == 8 ~ "Other",
    v_372 == 9 ~ "Undecided",
    v_372 == 10 ~ "Non-Voters"
  ))

table(data5$v_372, useNA = "always")

data5$Parties <- as.factor(data5$Parties)
levels(data5$Parties)
data5$Parties <- factor(data5$Parties,
                        levels = c("CDU/CSU", "AfD", "Greens", "SPD", "FDP", "The Left", "BSW", "Other", "Undecided", "Non-Voters"))
levels(data5$Parties)

# v_630 in Years
data5$Age <- 2025 - data5$v_630

# Vocational Education as Categorial variables 
data5 <- data5 %>%
  mutate(Vocational = case_when(
    Voc_ed == 1 ~ "None",
    Voc_ed == 2 ~ "Apprentice",
    Voc_ed == 3 ~ "Meister",
    Voc_ed == 4 ~ "Bachelor",
    Voc_ed == 5 ~ "Master",
    Voc_ed == 6 ~ "PhD"
  ))

data5$Vocational <- as.factor(data5$Vocational)
levels(data5$Vocational)
data5$Vocational <- factor(data5$Vocational,
                             levels = c("Apprentice", "None", "Meister", "Bachelor", "Master", "PhD"))
levels(data5$Vocational)


# Logit Models for PP ----

# From here on, Unemployment (v_619) will be excluded due to sample size restrictions

### 611
model_v611 <- glm(Dummy_v_611 ~ Dummy_Gender * Age + Parties + Vocational, 
                  data = data5, family = binomial)

# Age sequence
age_seq <- seq(min(data5$Age, na.rm = TRUE), max(data5$Age, na.rm = TRUE), by = 1)

# Data basis for predicted probabilities
pred_data <- expand.grid(
  Age = age_seq,
  Dummy_Gender = c(0, 1), 
  Parties = levels(data5$Parties)[1],  # Reference: "CDU/CSU"
  Vocational = levels(data5$Vocational)[1]  # Reference: "Apprentice"
)

# Calculate pp
pred_data$pp_v611 <- predict(model_v611, newdata = pred_data, type = "response")

# Visualize pp
predictedp_v611 <- ggplot(pred_data, aes(x = Age, y = pp_v611, color = factor(Dummy_Gender))) +
  ylim(0, 1) + 
  geom_line(size = 1) +
  scale_color_manual(values = c("lightskyblue4", "indianred3"), 
                     labels = c("Men", "Women"), guide = "none") +
  labs(title = "Domestic Security",
       x = "", 
       y = "Predicted Probabilities",
       color = "Gender") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 17),
    axis.text = element_text(size = 15),
    plot.title = element_text(size = 20, face = "plain")
  ) +
  geom_text(data = pred_data %>% 
              group_by(Dummy_Gender) %>%
              filter(Age == min(Age) + 10),  
            aes(label = ifelse(Dummy_Gender == 0, "Men", ""),
                color = factor(Dummy_Gender)), 
            size = 6, vjust = +1.5) + 
  geom_text(data = pred_data %>% 
              group_by(Dummy_Gender) %>%
              filter(Age == min(Age) + 10),  
            aes(label = ifelse(Dummy_Gender == 0, "", "Women"),
                color = factor(Dummy_Gender)),
            size = 6, vjust = -1.0) 


### 612
model_v612 <- glm(Dummy_v_612 ~ Dummy_Gender * Age + Parties + Vocational, 
                  data = data5, family = binomial)

# Calculate pp
pred_data$pp_v612 <- predict(model_v612, newdata = pred_data, type = "response")

# Visualize pp
predictedp_v612 <- ggplot(pred_data, aes(x = Age, y = pp_v612, color = factor(Dummy_Gender))) +
  ylim(0, 1) + 
  geom_line(size = 1) +
  scale_color_manual(values = c("lightskyblue4", "indianred3"), 
                     labels = c("Männer", "Frauen"), guide = "none") +
  labs(title = "Immigration of Foreigners/Migration",
       x = "", 
       y = "",
       color = "Geschlecht") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 17),
    axis.text = element_text(size = 15),
    plot.title = element_text(size = 20, face = "plain")
  ) +
  geom_text(data = pred_data %>% 
              group_by(Dummy_Gender) %>%
              filter(Age == min(Age + 10)),  
            aes(label = ifelse(Dummy_Gender == 0, "Men", "")),
            color = c("lightskyblue4", "indianred3"), 
            size = 6, vjust = +1.5) + 
  geom_text(data = pred_data %>% 
              group_by(Dummy_Gender) %>%
              filter(Age == min(Age+10)),  
            aes(label = ifelse(Dummy_Gender == 0, "", "Women")),
            color = c("lightskyblue4", "indianred3"), 
            size = 6, vjust = -1.0) 
predictedp_v612

### 613
model_v613 <- glm(Dummy_v_613 ~ Dummy_Gender * Age + Parties + Vocational, 
                  data = data5, family = binomial)

# Calculate pp
pred_data$pp_v613 <- predict(model_v613, newdata = pred_data, type = "response")

# Visualize pp
predictedp_v613 <- ggplot(pred_data, aes(x = Age, y = pp_v613, color = factor(Dummy_Gender))) +
  ylim(0, 1) + 
  geom_line(size = 1) +
  scale_color_manual(values = c("lightskyblue4", "indianred3"), labels = c("Männer", "Frauen"), guide = "none") +
  labs(title = "Social Justice/Inequality",
       x = "", 
       y = "Predicted Probabilities",
       color = "Geschlecht") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 17),
    axis.text = element_text(size = 15),
    plot.title = element_text(size = 20, face = "plain")
  ) +
  geom_text(data = pred_data %>% 
              group_by(Dummy_Gender) %>%
              filter(Age == min(Age+10)),  
            aes(label = ifelse(Dummy_Gender == 0, "Men", "")),
            color = c("lightskyblue4", "indianred3"), 
            size = 6, vjust = +1.5) + 
  geom_text(data = pred_data %>% 
              group_by(Dummy_Gender) %>%
              filter(Age == min(Age+10)),  
            aes(label = ifelse(Dummy_Gender == 0, "", "Women")),
            color = c("lightskyblue4", "indianred3"), 
            size = 6, vjust = -1.0) 

### 614 
model_v614 <- glm(Dummy_v_614 ~ Dummy_Gender * Age + Parties + Vocational, 
                  data = data5, family = binomial)

# Calculate pp
pred_data$pp_v614 <- predict(model_v614, newdata = pred_data, type = "response")

# Visualize pp
predictedp_v614 <- ggplot(pred_data, aes(x = Age, y = pp_v614, color = factor(Dummy_Gender))) +
  ylim(0, 1) + 
  geom_line(size = 1) +
  scale_color_manual(values = c("lightskyblue4", "indianred3"), labels = c("Männer", "Frauen"), guide = "none") +
  labs(title = "Tax and Duty Burden",
       x = "", 
       y = "",
       color = "Geschlecht") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 17),
    axis.text = element_text(size = 15),
    plot.title = element_text(size = 20, face = "plain")
  ) +
  geom_text(data = pred_data %>% 
              group_by(Dummy_Gender) %>%
              filter(Age == min(Age+10)),  
            aes(label = ifelse(Dummy_Gender == 0, "Men", "")),
            color = c("lightskyblue4", "indianred3"), 
            size = 6, vjust = -0.5) + 
  geom_text(data = pred_data %>% 
              group_by(Dummy_Gender) %>%
              filter(Age == min(Age+10)),  
            aes(label = ifelse(Dummy_Gender == 0, "", "Women")),
            color = c("lightskyblue4", "indianred3"), 
            size = 6, vjust = 1.5) 

### 615
# Logit-Regression mit Interaktion berechnen
model_v615 <- glm(Dummy_v_615 ~ Dummy_Gender * Age + Parties + Vocational, 
                  data = data5, family = binomial)

# Calculate pp
pred_data$pp_v615 <- predict(model_v615, newdata = pred_data, type = "response")

# Visualize pp
predictedp_v615 <- ggplot(pred_data, aes(x = Age, y = pp_v615, color = factor(Dummy_Gender))) +
  ylim(0, 1) + 
  geom_line(size = 1) +
  scale_color_manual(values = c("lightskyblue4", "indianred3"), labels = c("Männer", "Frauen"), guide = "none") +
  labs(title = "Education and School Policy",
       x = "", 
       y = "",
       color = "Geschlecht") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 17),
    axis.text = element_text(size = 15),
    plot.title = element_text(size = 20, face = "plain")
  ) +
  geom_text(data = pred_data %>% 
              group_by(Dummy_Gender) %>%
              filter(Age == min(Age+10)),  
            aes(label = ifelse(Dummy_Gender == 0, "Men", "")),
            color = c("lightskyblue4", "indianred3"), 
            size = 6, vjust = +1.5) + 
  geom_text(data = pred_data %>% 
              group_by(Dummy_Gender) %>%
              filter(Age == min(Age+10)),  
            aes(label = ifelse(Dummy_Gender == 0, "", "Women")),
            color = c("lightskyblue4", "indianred3"), 
            size = 6, vjust = -1.0) 

### 616 
model_v616 <- glm(Dummy_v_616 ~ Dummy_Gender * Age + Parties + Vocational, 
                  data = data5, family = binomial)

# Calculate pp
pred_data$pp_v616 <- predict(model_v616, newdata = pred_data, type = "response")

# Visualize pp
predictedp_v616 <- ggplot(pred_data, aes(x = Age, y = pp_v616, color = factor(Dummy_Gender))) +
  ylim(0, 1) + 
  geom_line(size = 1) +
  scale_color_manual(values = c("lightskyblue4", "indianred3"), labels = c("Männer", "Frauen"), guide = "none") +
  labs(title = "Traffic and Infrastructure",
       x = "Age", 
       y = "",
       color = "Geschlecht") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 17),
    axis.text = element_text(size = 15),
    plot.title = element_text(size = 20, face = "plain")
  ) +
  geom_text(data = pred_data %>% 
              group_by(Dummy_Gender) %>%
              filter(Age == min(Age+10)),  
            aes(label = ifelse(Dummy_Gender == 0, "Men", "")),
            color = c("lightskyblue4", "indianred3"), 
            size = 6, vjust = -1.0) + 
  geom_text(data = pred_data %>% 
              group_by(Dummy_Gender) %>%
              filter(Age == min(Age+15)),  
            aes(label = ifelse(Dummy_Gender == 0, "", "Women")),
            color = c("lightskyblue4", "indianred3"), 
            size = 6, vjust = +1.0) 

### v_617
model_v617 <- glm(Dummy_v_617 ~ Dummy_Gender * Age + Parties + Vocational, 
                  data = data5, family = binomial)

# Calculate pp
pred_data$pp_v617 <- predict(model_v617, newdata = pred_data, type = "response")

# Visualize pp
predictedp_v617 <- ggplot(pred_data, aes(x = Age, y = pp_v617, color = factor(Dummy_Gender))) +
  ylim(0, 1) + 
  geom_line(size = 1) +
  scale_color_manual(values = c("lightskyblue4", "indianred3"), labels = c("Männer", "Frauen"), guide = "none") +
  labs(title = "Climate and Environment",
       x = "", 
       y = "Predicted Probabilities",
       color = "Geschlecht") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 17),
    axis.text = element_text(size = 15),
    plot.title = element_text(size = 20, face = "plain")
  ) +
  geom_text(data = pred_data %>% 
              group_by(Dummy_Gender) %>%
              filter(Age == min(Age+10)),  
            aes(label = ifelse(Dummy_Gender == 0, "Men", "")),
            color = c("lightskyblue4", "indianred3"), 
            size = 6, vjust = -0.5) + 
  geom_text(data = pred_data %>% 
              group_by(Dummy_Gender) %>%
              filter(Age == min(Age+10)),  
            aes(label = ifelse(Dummy_Gender == 0, "", "Women")),
            color = c("lightskyblue4", "indianred3"), 
            size = 6, vjust = 1.5) 

### v_618
model_v618 <- glm(Dummy_v_618 ~ Dummy_Gender * Age + Parties + Vocational, 
                  data = data5, family = binomial)

# Calculate pp
pred_data$pp_v618 <- predict(model_v618, newdata = pred_data, type = "response")

# Visualize pp
predictedp_v618 <- ggplot(pred_data, aes(x = Age, y = pp_v618, color = factor(Dummy_Gender))) +
  ylim(0, 1) + 
  geom_line(size = 1) +
  scale_color_manual(values = c("lightskyblue4", "indianred3"), labels = c("Männer", "Frauen"), guide = "none") +
  labs(title = "Health and Care",
       x = "", 
       y = "",
       color = "Geschlecht") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 17),
    axis.text = element_text(size = 15),
    plot.title = element_text(size = 20, face = "plain")
  ) +
  geom_text(data = pred_data %>% 
              group_by(Dummy_Gender) %>%
              filter(Age == min(Age+15)),  
            aes(label = ifelse(Dummy_Gender == 0, "Men", "")),
            color = c("lightskyblue4", "indianred3"), 
            size = 6, vjust = +1.0) + 
  geom_text(data = pred_data %>% 
              group_by(Dummy_Gender) %>%
              filter(Age == min(Age+10)),  
            aes(label = ifelse(Dummy_Gender == 0, "", "Women")),
            color = c("lightskyblue4", "indianred3"), 
            size = 6, vjust = -1.0)  

### v_621
model_v621 <- glm(Dummy_v_621 ~ Dummy_Gender * Age + Parties + Vocational, 
                  data = data5, family = binomial)

# Calculate pp
pred_data$pp_v621 <- predict(model_v621, newdata = pred_data, type = "response")

# Visualize pp
predictedp_v621 <- ggplot(pred_data, aes(x = Age, y = pp_v621, color = factor(Dummy_Gender))) +
  ylim(0, 1) + 
  geom_line(size = 1) +
  scale_color_manual(values = c("lightskyblue4", "indianred3"), labels = c("Männer", "Frauen"), guide = "none") +
  labs(title = "Pensions",
       x = "Age", 
       y = "",
       color = "Geschlecht") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 17),
    axis.text = element_text(size = 15),
    plot.title = element_text(size = 20, face = "plain")
  ) +
  geom_text(data = pred_data %>% 
              group_by(Dummy_Gender) %>%
              filter(Age == min(Age+15)),  
            aes(label = ifelse(Dummy_Gender == 0, "Men", "")),
            color = c("lightskyblue4", "indianred3"), 
            size = 6, vjust = +1.0) + 
  geom_text(data = pred_data %>% 
              group_by(Dummy_Gender) %>%
              filter(Age == min(Age+10)),  
            aes(label = ifelse(Dummy_Gender == 0, "", "Women")),
            color = c("lightskyblue4", "indianred3"), 
            size = 6, vjust = -1.0) 

### v_622
model_v622 <- glm(Dummy_v_622 ~ Dummy_Gender * Age + Parties + Vocational, 
                  data = data5, family = binomial)

# Calculate pp
pred_data$pp_v622 <- predict(model_v622, newdata = pred_data, type = "response")

# Visualize pp
predictedp_v622 <- ggplot(pred_data, aes(x = Age, y = pp_v622, color = factor(Dummy_Gender))) +
  ylim(0, 1) + 
  geom_line(size = 1) +
  scale_color_manual(values = c("lightskyblue4", "indianred3"), labels = c("Männer", "Frauen"), guide = "none") +
  labs(title = "Foreign and Defense Policy",
       x = "", 
       y = "",
       color = "Geschlecht") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 17),
    axis.text = element_text(size = 15),
    plot.title = element_text(size = 20, face = "plain")
  ) +
  geom_text(data = pred_data %>% 
              group_by(Dummy_Gender) %>%
              filter(Age == min(Age+10)),  
            aes(label = ifelse(Dummy_Gender == 0, "Men", "")),
            color = c("lightskyblue4", "indianred3"), 
            size = 6, vjust = -1.5) + 
  geom_text(data = pred_data %>% 
              group_by(Dummy_Gender) %>%
              filter(Age == min(Age+10)),  
            aes(label = ifelse(Dummy_Gender == 0, "", "Women")),
            color = c("lightskyblue4", "indianred3"), 
            size = 6, vjust = +1.5) 

### v_623
model_v623 <- glm(Dummy_v_623 ~ Dummy_Gender * Age + Parties + Vocational, 
                  data = data5, family = binomial)

# Calculate pp
pred_data$pp_v623 <- predict(model_v623, newdata = pred_data, type = "response")

# Visualize pp
predictedp_v623 <- ggplot(pred_data, aes(x = Age, y = pp_v623, color = factor(Dummy_Gender))) +
  ylim(0, 1) + 
  geom_line(size = 1) +
  scale_color_manual(values = c("lightskyblue4", "indianred3"), labels = c("Männer", "Frauen"), guide = "none") +
  labs(title = "Economic Policy",
       x = "Age", 
       y = "Predicted Probabilities",
       color = "Geschlecht") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 17),
    axis.text = element_text(size = 15),
    plot.title = element_text(size = 20, face = "plain")
  ) +
  geom_text(data = pred_data %>% 
              group_by(Dummy_Gender) %>%
              filter(Age == min(Age+10)),  
            aes(label = ifelse(Dummy_Gender == 0, "Men", "")),
            color = c("lightskyblue4", "indianred3"), 
            size = 6, vjust = -1.0) + 
  geom_text(data = pred_data %>% 
              group_by(Dummy_Gender) %>%
              filter(Age == min(Age+10)),  
            aes(label = ifelse(Dummy_Gender == 0, "", "Women")),
            color = c("lightskyblue4", "indianred3"), 
            size = 6, vjust = +1.7) 

# Combined PP
# Plots in einer Liste speichern
PP_Combined <- plot_grid(
  predictedp_v611,predictedp_v612, predictedp_v622, 
  predictedp_v613, predictedp_v618, predictedp_v615, 
  predictedp_v617, predictedp_v614, predictedp_v616, 
  predictedp_v623, predictedp_v621, empty_plot,
  ncol = 3 
)

ggsave("PP_Combined.png", PP_Combined, width = 15, height = 20)


# Selected most interesting ones

# Visualize pp
selected_v613 <- ggplot(pred_data, aes(x = Age, y = pp_v613, color = factor(Dummy_Gender))) +
  ylim(0, 1) + 
  geom_line(size = 1) +
  scale_color_manual(values = c("lightskyblue4", "indianred3"), labels = c("Männer", "Frauen"), guide = "none") +
  labs(title = "Social Justice/Inequality",
       x = "Age", 
       y = "Predicted Probabilities",
       color = "Geschlecht") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 17),
    axis.text = element_text(size = 15),
    plot.title = element_text(size = 20, face = "plain")
  ) +
  geom_text(data = pred_data %>% 
              group_by(Dummy_Gender) %>%
              filter(Age == min(Age+10)),  
            aes(label = ifelse(Dummy_Gender == 0, "Men", "")),
            color = c("lightskyblue4", "indianred3"), 
            size = 6, vjust = +1.5) + 
  geom_text(data = pred_data %>% 
              group_by(Dummy_Gender) %>%
              filter(Age == min(Age+10)),  
            aes(label = ifelse(Dummy_Gender == 0, "", "Women")),
            color = c("lightskyblue4", "indianred3"), 
            size = 6, vjust = -1.0)

selected_v617 <- ggplot(pred_data, aes(x = Age, y = pp_v617, color = factor(Dummy_Gender))) +
  ylim(0, 1) + 
  geom_line(size = 1) +
  scale_color_manual(values = c("lightskyblue4", "indianred3"), labels = c("Männer", "Frauen"), guide = "none") +
  labs(title = "Climate and Environment",
       x = "Age", 
       y = "",
       color = "Geschlecht") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 17),
    axis.text = element_text(size = 15),
    plot.title = element_text(size = 20, face = "plain")
  ) +
  geom_text(data = pred_data %>% 
              group_by(Dummy_Gender) %>%
              filter(Age == min(Age+10)),  
            aes(label = ifelse(Dummy_Gender == 0, "Men", "")),
            color = c("lightskyblue4", "indianred3"), 
            size = 6, vjust = -0.5) + 
  geom_text(data = pred_data %>% 
              group_by(Dummy_Gender) %>%
              filter(Age == min(Age+10)),  
            aes(label = ifelse(Dummy_Gender == 0, "", "Women")),
            color = c("lightskyblue4", "indianred3"), 
            size = 6, vjust = 1.5) 

selected_v623 <- ggplot(pred_data, aes(x = Age, y = pp_v623, color = factor(Dummy_Gender))) +
  ylim(0, 1) + 
  geom_line(size = 1) +
  scale_color_manual(values = c("lightskyblue4", "indianred3"), labels = c("Männer", "Frauen"), guide = "none") +
  labs(title = "Economic Policy",
       x = "Age", 
       y = "",
       color = "Geschlecht") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 17),
    axis.text = element_text(size = 15),
    plot.title = element_text(size = 20, face = "plain")
  ) +
  geom_text(data = pred_data %>% 
              group_by(Dummy_Gender) %>%
              filter(Age == min(Age+10)),  
            aes(label = ifelse(Dummy_Gender == 0, "Men", "")),
            color = c("lightskyblue4", "indianred3"), 
            size = 6, vjust = -1.0) + 
  geom_text(data = pred_data %>% 
              group_by(Dummy_Gender) %>%
              filter(Age == min(Age+10)),  
            aes(label = ifelse(Dummy_Gender == 0, "", "Women")),
            color = c("lightskyblue4", "indianred3"), 
            size = 6, vjust = +1.7) 

PP_Selected <- plot_grid(
  predictedp_v611,predictedp_v612, predictedp_v622, 
  selected_v613, selected_v617, selected_v623,
  ncol = 3  
)

ggsave("PP_Selected.png", PP_Selected, width = 15, height = 8)



# Penalized Logit model for Unemployment and Digitization ----
model_v619 <- logistf(Dummy_v_619 ~ Dummy_Gender * Age + Parties + Vocational, 
                  data = data5, family = binomial)

model_v620 <- logistf(Dummy_v_620 ~ Dummy_Gender * Age + Parties + Vocational, 
                        data = data5, family = binomial)

# Model summaries ----

# Function to calculate Nagelkerke R^2
get_nagelkerke <- function(model) {
  as.numeric(performance::r2_nagelkerke(model))
}

models1to4 <- list("Domestic Security" = model_v611, 
                   "(Im)migration" = model_v612,
                   "Foreign & Defense Policy" = model_v622,
                   "Social Justice/ Inequality" = model_v613)

models5to7 <- list("Health & Care" = model_v618, 
                   "Education & School Policy" = model_v615,
                   "Climate & Environment" = model_v617)

models8to11 <- list("Tax & Duty Burden" = model_v614, 
                     "Traffic & Infrastructure" = model_v616,
                     "Economic Policy" = model_v623,
                     "Pensions" = model_v621)

# Calculate Nagelkerke for all models
gof_values1to4 <- lapply(models1to4, get_nagelkerke)
names(gof_values1to4) <- names(models1to4)
gof_values1to4 
# $`Domestic Security` 0.2986447 
# $`(Im)migration` 0.4011779 
# $`Foreign & Defense Policy` 0.112476
# $`Social Justice/ Inequality` 0.4078783

gof_values5to7 <- lapply(models5to7, get_nagelkerke)
names(gof_values5to7) <- names(models5to7)
gof_values5to7
# $`Health & Care` 0.08752808
# $`Education & School Policy` 0.08144522
# $`Climate & Environment` 0.4921618

gof_values8to11 <- lapply(models8to11, get_nagelkerke)
names(gof_values8to11) <- names(models8to11)
gof_values8to11
# $`Tax & Duty Burden` 0.1007202
# $`Traffic & Infrastructure` 0.07653422
# $`Economic Policy` 0.1989723
# $Pensions 0.09419874


modelsummary(models1to4,
             output = "Regressions1-4.docx",
             exponentiate = TRUE,
             stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01)
             )

modelsummary(models5to7,
             output = "Regressions5-7.docx",
             exponentiate = TRUE,
             stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01)
)

modelsummary(models8to11,
             output = "Regressions8-11.docx",
             exponentiate = TRUE,
             stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01)
)


models12and13 <- list("Unemployment" = model_v619,
                      "Digitization" = model_v620)

summary(model_v619) # likelihood ratio test 41.56103 (p=0.000776)
summary(model_v620) # likelihood ratio test 128.6099 (p=0)

modelsummary(models12and13,
             output = "Regressions12-13.docx",
             exponentiate = TRUE,
             stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01)
)


# Overview of most selected priority combinations ----
Combinations <- data5 %>%
  mutate(combination = apply(select(., Dummy_v_611:Dummy_v_623), 1, function(row) {
    paste(gsub("Dummy_v_", "", names(row)[row == 1]), collapse = "-")
  })) %>%
  count(combination, sort = TRUE)

# Frequency Gender
Combinations_Gender <- data5 %>%
  mutate(combination = apply(select(., Dummy_v_611:Dummy_v_623), 1, function(row) {
    paste(gsub("Dummy_v_", "", names(row)[row == 1]), collapse = "-")
  })) %>%
  group_by(Dummy_Gender, combination) %>% 
  summarise(n = n(), .groups = "drop") %>%  
  group_by(Dummy_Gender) %>% 
  mutate(relative_freq = n / sum(n)) %>%
  arrange(Dummy_Gender, desc(relative_freq))

table(data5$Dummy_Gender) # 0 = 2614; 1 = 1380

# 3 Age groups
data5 <- data5 %>%
  mutate(Age3 = case_when(
    age_class == 1 | age_class == 2 ~ 1,
    age_class == 3 | age_class == 4 ~ 2,
    age_class == 5 ~ 3
  ))

# Frequency Age
Combinations_Age <- data5 %>%
  mutate(combination = apply(select(., Dummy_v_611:Dummy_v_623), 1, function(row) {
    paste(gsub("Dummy_v_", "", names(row)[row == 1]), collapse = "-")
  })) %>%
  group_by(Age3, combination) %>% 
  summarise(n = n(), .groups = "drop") %>%
  group_by(Age3) %>% 
  mutate(relative_freq = n / sum(n)) %>% 
  arrange(Age3, desc(relative_freq))

table(data5$Age3) # 1 = 549; 2 = 2228; 3 = 1217

# 3 Education Levels
data5 <- data5 %>%
  mutate(Voc_ed3 = case_when(
    Voc_ed == 1 ~ 0,
    Voc_ed == 2 | Voc_ed == 3 ~ 1,
    Voc_ed == 4 | Voc_ed == 5 | Voc_ed == 6 ~ 2
  ))

# Frequency Vocational Education
Combinations_Voced <- data5 %>%
  mutate(combination = apply(select(., Dummy_v_611:Dummy_v_623), 1, function(row) {
    paste(gsub("Dummy_v_", "", names(row)[row == 1]), collapse = "-")
  })) %>%
  group_by(Voc_ed3, combination) %>% 
  summarise(n = n(), .groups = "drop") %>%
  group_by(Voc_ed3) %>% 
  mutate(relative_freq = n / sum(n)) %>% 
  arrange(Voc_ed3, desc(relative_freq))

table(data5$Voc_ed3) # 1 = 1960; 2 = 1747





