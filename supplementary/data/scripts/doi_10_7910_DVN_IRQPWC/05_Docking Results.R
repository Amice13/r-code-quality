library(readxl)
library(ggplot2)
library(dplyr)
df <- read_excel("dataDocking.xlsx")
colnames(df) <- trimws(colnames(df))
df <- df %>%
  mutate(Docking_Pair = paste(SYMBOL, `Mol ID`, sep = " - ")) %>%
  arrange(`Affinity(Kcal/mol)`)

df$Docking_Pair <- factor(df$Docking_Pair, levels = df$Docking_Pair)

x_base <- levels(df$Docking_Pair)[1]  
y_base <- 0                          

p <- ggplot(df, aes(x = Docking_Pair, y = `Affinity(Kcal/mol)`)) +
  geom_segment(aes(xend = Docking_Pair, y = y_base, yend = `Affinity(Kcal/mol)`), color = "skyblue", size = 1.2) +
  geom_point(color = "firebrick", size = 3) +
  
  
  geom_hline(yintercept = y_base, linetype = "solid", color = "black", linewidth = 0.6) +  
  geom_vline(xintercept = 0.5, linetype = "solid", color = "black", linewidth = 0.6) +     
  
  labs(x = "Protein - Molecule ID", y = "Binding Affinity (Kcal/mol)", title = "Molecular Docking Results") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid = element_blank(),  
    plot.title = element_text(hjust = 0.5)
  )

print(p)
ggsave("Docking_Lollipop_with_AxisLines.png", plot = p, width = 12, height = 6, dpi = 600)
