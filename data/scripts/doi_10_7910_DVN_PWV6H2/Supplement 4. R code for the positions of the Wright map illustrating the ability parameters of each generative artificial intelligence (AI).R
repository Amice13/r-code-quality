# Wright map with examinees 112-117 - Simple theme for maximum compatibility. Nursing exam of 50 items and 117 examinees. Ability and difficulty parameter were estimated according to Rasch model.

if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if (!require(dplyr)) install.packages("dplyr"); library(dplyr)

# Read data files
difficulty <- read.csv("Supplement_5_item_difficulty_parameter.csv")
ability <- read.csv("Supplement_6_theta_score.csv")

# Rename columns for consistency
difficulty <- difficulty %>% rename(item = 1, diff = 2)
ability <- ability %>% rename(id = 1, theta = 2)

# Extract THETA (ability) data for examinees 112-117
target_examinees <- ability %>% filter(id %in% 112:117)

# Display the theta values for confirmation
cat("Examinees 112-117 THETA (Ability) Values:\n")
cat("========================================\n")
print(target_examinees %>% arrange(id) %>% select(id, theta))

# Create histogram data for better control
hist_data <- hist(ability$theta, breaks = 25, plot = FALSE)

# Create Wright Map with simple theme
wright_map <- ggplot() +
  # Horizontal histogram of person abilities
  geom_histogram(data = ability, aes(y = theta), 
                 fill = "lightblue", bins = 25, alpha = 0.7, 
                 color = "black", size = 0.3) +
  
  # Add red horizontal lines for examinees 112-117 at their THETA (ability) locations
  geom_hline(data = target_examinees, aes(yintercept = theta), 
             color = "red", linetype = "solid", linewidth = 1.2, alpha = 0.8) +
  
  # Add examinee ID labels at their THETA locations
  geom_text(data = target_examinees, 
            aes(x = max(hist_data$counts) * 0.95, y = theta, label = paste("ID", id)), 
            color = "red", hjust = 1, vjust = -0.3, size = 3.5, fontface = "bold") +
  
  # Add item difficulty markers on the right
  geom_point(data = difficulty, 
             aes(x = max(hist_data$counts) * 1.1, y = diff), 
             color = "black", size = 1.5, shape = 124) +
  
  # Add item numbers
  geom_text(data = difficulty, 
            aes(x = max(hist_data$counts) * 1.15, y = diff, label = item), 
            color = "black", hjust = 0, vjust = 0.5, size = 2.5) +
  
  # Add section labels
  annotate("text", x = max(hist_data$counts) * 0.05, y = max(ability$theta) * 0.95, 
           label = "PERSONS", fontface = "bold", size = 4, hjust = 0) +
  annotate("text", x = max(hist_data$counts) * 1.3, y = max(ability$theta) * 0.95, 
           label = "ITEMS", fontface = "bold", size = 4, hjust = 1) +
  
  # Set axis limits
  scale_x_continuous(limits = c(0, max(hist_data$counts) * 1.4)) +
  
  # Labels and simplified theme
  labs(title = "Wright Map: Nursing Exam (Rasch Model)",
       subtitle = "Red horizontal lines: Examinees 112-117 at their THETA (ability) locations",
       x = "Number of Persons",
       y = "Ability/Difficulty (logits)") +
  
  theme_bw() +  # Using theme_bw() instead of theme_minimal() for better compatibility
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "darkred", hjust = 0.5),
    axis.title = element_text(size = 12),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

# Display the plot
print(wright_map)

# Save the plot
ggsave('wright_map_simple_theme.png', plot = wright_map, 
       width = 12, height = 8, dpi = 300, bg = "white")

cat("\nPlot saved successfully!\n")

