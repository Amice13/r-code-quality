forest_plot <- ggplot(CATES, aes(x = cate/17.01, y = label)) +
  geom_point(size = 3) +  # Add points for the effect estimates
  geom_errorbarh(aes(xmin = lb/17.01, xmax = ub/17.01), height = 0.2) +  # Add error bars
  theme_minimal() +  # Use a clean theme
  labs(
    x = "Standardized Effect Size (95% CI)", 
    y = "",
    title = "Subgroup Analysis (standardized)"
  ) +
  geom_vline(xintercept = 2.56/17.01, linetype = "dashed", color = "red") +  # Add vertical line at ATE
  geom_rect(aes(xmin = 1.23/17.01, xmax = 3.90/17.01, ymin = -Inf, ymax = Inf),  # Add shaded CI region
            fill = "blue", alpha = 0.01) +  # Transparent blue shading
  # Add only point estimates and CIs in the margin
  geom_text(aes(x = min(lb/17.01) - 0.9, label = sprintf("%.2f (%.2f, %.2f)", cate/17.01, lb/17.01, ub/17.01)), 
            hjust = 0.5, size = 3, color = "black") +  # Margin text for estimates
  theme(
    axis.text.y = element_text(size = 12),  # Adjust y-axis text size
    plot.title = element_text(hjust = 0.3)  # Center title
  )

# Print the plot
print(forest_plot)
