################################################################################
# Crisis at Home, Exit Abroad:                  
# Coups, Civil Wars, and Coalition Defection                 
# Weiss Mehrabi                                   
# REPLICATION R SCRIPT (FIGURES 1 & 2)
################################################################################

# This R script regenerates Figure 1 (Coup Effects) and Figure 2 
# (Battle Death Effects) for the manuscript.
#
# NOTE: This script plots data calculated in the Stata replication file.
# The marginal effects data is manually entered into data frames below.
#
# Required R Libraries:
# - ggplot2
# - dplyr
#
# To install, run: install.packages(c("ggplot2", "dplyr"))

# Load libraries
library(ggplot2)
library(dplyr)


################################################################################
# --- FIGURE 1: MARGINAL EFFECTS OF COUPS ---
################################################################################

# 1.1. Define data for Figure 1 (from Stata margins)
margins_data <- data.frame(
  Category = c("Failed Coup = 0", "Failed Coup = 1", "Successful Coup = 0", "Successful Coup = 1"),
  Group    = c("Failed Coup", "Failed Coup", "Successful Coup", "Successful Coup"),
  Margin   = c(0.0301095, 0.562134, 0.0562288, 0.3342671),
  CI_Lower = c(0.0130392, 0.4390069, 0.0343728, 0.1213119),
  CI_Upper = c(0.0471799, 0.685261, 0.0780847, 0.5472222)
)

# 1.2. Reorder factors for plotting
margins_data$Category <- factor(margins_data$Category, levels = rev(c("Failed Coup = 0", "Failed Coup = 1", "Successful Coup = 0", "Successful Coup = 1")))

# 1.3. Create the base plot
plot_figure_1_base <- ggplot(
  data = margins_data,
  aes(x = Margin, y = Category, color = Group)
) +
  geom_errorbarh(
    aes(xmin = CI_Lower, xmax = CI_Upper),
    height = 0.2, linewidth = 0.8, alpha = 0.7
  ) +
  geom_point(size = 3.5, shape = 18) +
  labs(
    title = "",
    x = "Predicted Probability of Defection",
    y = "",
    color = "Coup Type"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  scale_color_manual(
    values = c("Failed Coup" = "#d95f02", "Successful Coup" = "#1b9e77"),
    labels = c("Failed Coup", "Successful Coup")
  )

# 1.4. Create and display Figure 1 (With Labels)
plot_figure_1_labeled <- plot_figure_1_base +
  # Add text labels for the exact margin values
  geom_text(
    aes(label = round(Margin, 3)), # Round the margin to 3 decimal places
    vjust = -1.8,                  # Vertically adjust to be above the point
    color = "black",               # Set text color to black
    size = 3.5                     # Set the font size
  ) +
  # Expand the x-axis slightly to make room for the labels
  scale_x_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.05)))

# Display the final plot with labels
print(plot_figure_1_labeled)


################################################################################
# --- FIGURE 2: MARGINAL EFFECTS OF BATTLE DEATHS ---
################################################################################

# 2.1. Define data for Figure 2 (from Stata margins)
margins_lbestdead <- data.frame(
  lbestdead = 0:13,
  Margin    = c(0.0389623, 0.049677, 0.0670591, 0.0942255, 0.1316905, 0.1809502, 
                0.2444327, 0.3184713, 0.3970322, 0.4756801, 0.5524003, 0.6283755, 
                0.7068548, 0.7873711),
  CI_Lower  = c(0.0205017, 0.0257733, 0.0239767, 0.0234873, 0.0319748, 0.040074, 
                0.0672483, 0.1353596, 0.2360099, 0.3423079, 0.4175927, 0.4507473, 
                0.4866373, 0.5436253),
  CI_Upper  = c(0.0574228, 0.0735806, 0.1101415, 0.1649638, 0.2314061, 0.3218264, 
                0.421617, 0.5015831, 0.5580544, 0.6090523, 0.6872079, 0.8060038, 
                0.9270724, 1.031117)
)

# 2.2. Create and display Figure 2
plot_figure_2 <- ggplot(
  data = margins_lbestdead, 
  aes(x = lbestdead, y = Margin)
) +
  geom_ribbon(
    aes(ymin = CI_Lower, ymax = CI_Upper), 
    alpha = 0.2,
    fill = "steelblue"
  ) +
  geom_line(
    color = "steelblue", 
    linewidth = 1.2
  ) +
  labs(
    title = "",
    x = "Battle Deaths (ln)",
    y = "Predicted Probability of Defection"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 11),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = seq(0, 12, 2))

# Display the plot
print(plot_figure_2)


# 2.3. Optional: Save the plot to a file
ggsave(
  "lbestdead_margins_plot.pdf",
  plot = plot_figure_2,
  width = 8,
  height = 6,
  device = "pdf",
  dpi = 300
)

# --- End of Script ---