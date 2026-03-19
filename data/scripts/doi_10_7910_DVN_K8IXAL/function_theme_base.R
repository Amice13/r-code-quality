library(ggplot2) # CRAN v3.5.1

theme_baser <- function() {
  theme_minimal() %+replace%
    theme(
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.border = element_rect(
        fill = NA, colour = "black", linewidth = 0.5,
        linetype = "solid"
      ),
      legend.title = element_text(size = 15),
      plot.caption = element_text(colour = "grey30", size = 11, hjust = 1),
      plot.title = element_text(
        size = 15, face = "bold",
        vjust = 1.5, hjust = 0.5,
        margin = margin(0, 0, 0, 0)
      ),
      legend.position = "bottom",
      axis.ticks.y = element_line(linewidth = 0.3),
      axis.ticks.x = element_line(linewidth = 0.3),
      axis.ticks.length = unit(0.2, "cm"),
      legend.text = element_text(size = 13),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white", color = "white"),
      strip.text = element_text(
        size = 15, hjust = 0.5,
        face = "bold",
        margin = margin(b = 5, r = 5, l = 5, t = 5)
      ),
      axis.text.y = element_text(
        colour = "black", size = 13,
        hjust = 1
      ),
      axis.text.x = element_text(colour = "black", size = 13),
      axis.title = element_text(size = 13, hjust = 0.5)
    )
}

# Set theme as default
theme_set(theme_baser())
