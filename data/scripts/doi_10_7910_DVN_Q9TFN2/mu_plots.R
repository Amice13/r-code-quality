# Clear
rm(list = ls(all.names = TRUE)) # will clear all (including hidden) objects.
invisible(gc()) #free up memory
# Packages used for plotting (if not already installed, install them first)
libplt <- c("ggplot2", "latex2exp", "reshape")
lapply(libplt, require, character.only = TRUE)

# Load results
load("simulations/mu_sims_S1e+05_p100.Rdata")

# Plotting results
do_save <- TRUE # save plots?

# Create labels (via functions) for plots
rholab <- function(string) {
  sprintf("rho = %s", string)
}
capitalize <- function(string) {
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  string
}

# Color-blind friendly color palette
cb_palette <- c("#E69F00", "#56B4E9", "#000000", "#009E73",
                "#0072B2", "#D55E00", "#CC79A7")
# Shape order
shape_order <- c(0, 1, 2, 5, 4) # square, circle, triangle, diamond, cross
# Linetype order
linetype_order <- c("solid", "dashed", "dotted", "dotdash", "longdash")

jmax <- 10 # number of coefficients to plot

# Plot levels
mus_temp <- mus[1:jmax, ,]
df <- melt(mus_temp, id.vars = c("j", "rho", "pattern"))
g <- guide_legend(title = TeX("$\\rho =$"), nrow = 1)
ggplot(data = df, aes(x = j, y = value, group = rho)) +
  geom_line(aes(linetype = as.factor(rho), color = as.factor(rho)),
            linewidth = 0.5) +
  geom_point(aes(shape = as.factor(rho), color = as.factor(rho)), size = 2) +
  scale_shape(solid = FALSE) +
  facet_wrap(~pattern, ncol = 1, labeller = labeller(pattern = capitalize)) +
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  scale_y_continuous(limits = c(-0.5, 0.5), expand = c(0, 0)) +
  labs(
    title = sprintf("Orthogonalization Coefficients by Coefficient Pattern and Regressor Correlation Level (Approximated via Simulation with S=%d, p=%d)", nsim, p),
    y = TeX("$\\mu_{0,j}$"),
    x = TeX("$j$")
  ) +
  theme_bw() +
  theme(
    legend.justification = c(1, 1), legend.position = c(1, 1),
    legend.key = element_rect(colour = "transparent", fill = NA),
    legend.box.background = element_rect(colour = "black"),
    legend.title = element_text(size = rel(0.75), hjust = 0.5),
    legend.text = element_text(size = rel(0.75)),
    legend.margin = margin(c(0, 0, 0, 0), unit = "mm"),
    legend.spacing = unit(0, "mm"),
    legend.spacing.x = unit(0, "mm"),
    legend.spacing.y = unit(0, "mm"),
    panel.spacing.y = unit(3, "mm"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = rel(0.5))
  ) +
  scale_colour_manual(values = cb_palette) +
  scale_shape_manual(values = shape_order) +
  scale_linetype_manual(values = linetype_order) +
  guides(linetype = g, color = g, shape = g)
if (do_save == TRUE) {
  filename <- sprintf("simulations/img/Figure_H_1_1_mu_S%d_p%d.pdf", nsim, p)
  ggsave(filename, height = (2 / 3) * 22.85, width = 16.5,
         device = "pdf", units = "cm")
}

# Plot absolute values after sorting them for each (pattern, rho) combination
abs_mus <- abs(mus)
abs_mus_sort <- apply(abs_mus, c(2, 3), sort, decreasing = TRUE)
abs_mus_sort <- abs_mus_sort[1:jmax, , ]
dimnames(abs_mus_sort) <- list(j = 1:jmax, rho = rhovec, pattern = patlist)
df <- melt(abs_mus_sort, id.vars = c("j", "rho", "pattern"))
g <- guide_legend(title = TeX("$\\rho =$"), nrow = 1)
ggplot(data = df, aes(x = j, y = value, group = rho)) +
  geom_line(aes(linetype = as.factor(rho), color = as.factor(rho)),
  linewidth = 0.5) +
  geom_point(aes(shape = as.factor(rho), color = as.factor(rho)), size = 2) +
  scale_shape(solid = FALSE) +
  facet_wrap(~pattern, ncol = 1, labeller = labeller(pattern = capitalize)) +
  scale_x_continuous(limits = c(1, 10), breaks = seq(1, 10, by = 1)) +
  scale_y_continuous(limits = c(0, 0.5), expand = c(.05, 0)) +
  labs(
    title = sprintf("Absolute Orthogonalization Coefficients by Coefficient Pattern and Regressor Correlation Level (Approximated via Simulation with S=%d, p=%d)", nsim, p),
    y = TeX("$|\\mu_{0}|_{(j)}$"),
    x = TeX("$(j)$")
  ) +
  theme_bw() +
  theme(
    legend.justification = c(1, 1), legend.position = c(1, 1),
    legend.key = element_rect(colour = "transparent", fill = NA),
    legend.box.background = element_rect(colour = "black"),
    legend.title = element_text(size = rel(0.75), hjust = 0.5),
    legend.text = element_text(size = rel(0.75)),
    legend.margin = margin(c(0, 0, 0, 0), unit = "mm"),
    legend.spacing = unit(0, "mm"),
    legend.spacing.x = unit(0, "mm"),
    legend.spacing.y = unit(0, "mm"),
    panel.spacing.y = unit(3, "mm"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = rel(0.5))
  ) +
  scale_colour_manual(values = cb_palette) +
  scale_shape_manual(values = shape_order) +
  scale_linetype_manual(values = linetype_order) +
  guides(linetype = g, color = g, shape = g)
if (do_save == TRUE) {
  filename <- sprintf("simulations/img/Figure_H_1_2_sorted_abs_mu_S%d_p%d.pdf", nsim, p)
  ggsave(filename, height = (2 / 3) * 22.85, width = 16.5,
         device = "pdf", units = "cm")
}
