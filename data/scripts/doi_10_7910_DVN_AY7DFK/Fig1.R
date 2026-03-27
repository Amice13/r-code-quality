args = commandArgs(trailingOnly = TRUE)
# Load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Read the Excel file (you can change the path if needed)
setwd(args[1])
pdf_path  = args[2]

df <- read_excel("analyse_evaluations.xlsx")

# Replace missing crit values with 0
df <- df %>% mutate(across(starts_with("crit"), ~ replace_na(.x, 0)))

# Reshape to long format
df_long <- df %>%
  pivot_longer(cols = starts_with("crit"),
               names_to = "criterion",
               values_to = "value")

# Factorize 'Financed' and 'criterion'
df_long$Financed <- factor(df_long$Financed, levels = c(0, 1), labels = c("rejected", "accepted"))
df_long$criterion <- factor(df_long$criterion, levels = paste0("crit", 1:7),
                            labels = paste0("Crit. ", 1:7))  # <- Pretty legend

# Create numeric xpos per group
df_long <- df_long %>%
  mutate(crit_num = as.numeric(criterion),
           xpos = crit_num + ifelse(Financed == "rejected", -0.15, 0.15))  # <- tighter pair spacing


# Summary stats
summary_df <- df_long %>%
  group_by(criterion, Financed, xpos) %>%
  summarise(
    mean = mean(value),
    sd = sd(value),
    n = n(),
    se = sd / sqrt(n),
    ci_lower = mean - 1.96 * se,
    ci_upper = mean + 1.96 * se,
    .groups = "drop"
  )

# Plot
p = ggplot(summary_df, aes(x = xpos, y = mean, fill = criterion)) +
  geom_bar(stat = "identity", width = 0.25, color = "black", linewidth = 0.2) +  # <- black border
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.05) +
  geom_hline(yintercept=0, color= "black", linewidth = 0.3)  + 
  guides(fill=guide_legend(nrow =1)) +
  scale_x_continuous(
    breaks = 1:7,
    labels = rep("Rej. / Acc.", 7), 
	expand = expansion(mult = c(0.01, 0.05))
  ) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "",
    y = "Length of report related to criterion",
    x = NULL,
    fill = ""
  ) +
  theme_minimal() +
  theme(     panel.grid = element_blank(),
    legend.position = "bottom",  legend.box = "horizontal", legend.spacing.x = unit(0.5, 'cm'), legend.text = element_text(size=8), legend.key.size = unit(0.4, "cm"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, color = "black"),
    axis.title.y = element_text(face = "bold", color = "black")
	)
ggsave(pdf_path, plot = p, width =8, height = 4)


pdf_path  = args[3]


df <- read_excel("analyse_evaluations.xlsx")

# Replace missing crit values with 0
df <- df %>% mutate(across(starts_with("crit"), ~ replace_na(.x, 0)))


# Reshape to long format
df_long <- df %>%
  pivot_longer(cols = starts_with("crit"),
               names_to = "criterion",
               values_to = "value")

# Factorize 'Financed' and 'criterion'
df_long$Financed <- factor(df_long$Financed, levels = c(0, 1), labels = c("rejected", "accepted"))
df_long$criterion <- factor(df_long$criterion, levels = paste0("crit", 1:7),
                            labels = paste0("Crit. ", 1:7))  # <- Pretty legend

# Create numeric xpos per group
df_long <- df_long %>%
  mutate(crit_num = as.numeric(criterion),
         xpos = crit_num + ifelse(Financed == "rejected", -0.15, 0.15))  # <- tighter pair spacing

# Summary stats
summary_df <- df_long %>% mutate(value = value == 0) %>%
  group_by(criterion, Financed, xpos) %>%
  summarise(
    mean = mean(value),
    sd = sd(value),
    n = n(),
    se = sd / sqrt(n),
    ci_lower = mean - 1.96 * se,
    ci_upper = mean + 1.96 * se,
    .groups = "drop"
  )

# Plot
p = ggplot(summary_df, aes(x = xpos, y = mean, fill = criterion)) +
  geom_bar(stat = "identity", width = 0.25, color = "black", linewidth = 0.2) +  # <- black border
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.05) +
  geom_hline(yintercept=0, color= "black", linewidth = 0.3)  + 
  guides(fill=guide_legend(nrow =1)) +
  scale_x_continuous(
    breaks = 1:7,
    labels = rep("Rej. / Acc.", 7), 
	expand = expansion(mult = c(0.01, 0.05))
  ) +
  scale_fill_brewer(palette = "Reds") +
  labs(
    title = "",
    y = "Proba criteria not mentionned",
    x = NULL,
    fill = ""
  ) +
  theme_minimal() +
  theme(     panel.grid = element_blank(),
    legend.position = "bottom",  legend.box = "horizontal", legend.spacing.x = unit(0.5, 'cm'), legend.text = element_text(size=8), legend.key.size = unit(0.4, "cm"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, color = "black"),
    axis.title.y = element_text(face = "bold", color = "black")
	)
ggsave(pdf_path, plot = p, width =8, height = 4)
