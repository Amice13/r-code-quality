# Clear
rm(list = ls(all.names = TRUE)) # will clear all (including hidden) objects.
invisible(gc()) #free up memory
# Packages used for plotting (if not already installed, install them first)
libplt <- c("formattable", "ggplot2", "latex2exp", "reshape", "stringr")
lapply(libplt, require, character.only = TRUE)

# Load results
load("simulations/BCCH12ECTA_comparison_linear_N3_Rho5_R2000_B1000_K3_interceptFALSE_standardizeFALSE.RData")

# Renaming patterns (fits better with literature)
dimnames(ell2_errs)[["pattern"]] <- c("exactly sparse", "intermediate", "approximately sparse")

# Plotting results
# Color-blind friendly color palette 
cbPalette <- c("#E69F00", "#56B4E9", "#000000", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7")

# Calculate mean ell2 estimation error
mean_ell2_errs <- colMeans(ell2_errs, na.rm = TRUE)

# Create labels (via functions) for plots
nplab <- function(string) {
  sprintf("n, p = %s", string) # note: labeller expects a string - not integer
}
rholab <- function(string) {
  sprintf("rho = %s", string)
}
capitalize <- function(string) {
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  string
}

# (1.1) COMPARING MEAN ESTIMATION ERRORS ACROSS METHODS FOR GIVEN (c0, alpha(n)) PAIRS
do_save <- TRUE
metlist <- c("BCV", "BRT09", "BCCH12")
g <- guide_legend(title = TeX("Method:"))
scale_linetype <- setNames(c("solid", "dotdash", "dashed"), metlist)
df <- melt(mean_ell2_errs)
ggplot(data = df, aes(x = rho, y = value, group = method)) +
  geom_line(aes(linetype = method, color = method), linewidth = 0.5) +
  geom_point(aes(shape = method, color = method), size = 2) +
  scale_shape(solid = FALSE) +
  facet_grid(pattern ~ n,
             labeller = labeller(n = nplab, pattern = capitalize)) +
  geom_hline(yintercept = sqrt(2), linetype = "dotted",
             color = "black", linewidth = 0.5) +
  scale_x_continuous(breaks = seq(rhovec[1], rhovec[numrho], by = .2)) +
  scale_y_continuous(limits = c(0, 1.5), expand = c(0, 0)) +
  labs(
    y = TeX("$E[\\|\\widehat{\\theta}-\\theta_{0}\\|_{2}]$"),
    x = TeX("$\\rho$")
  ) +
  theme_bw() +
  theme(
    legend.justification = c(1, 1),
    legend.position = c(1, 1),
    legend.key = element_rect(colour = "transparent", fill = NA),
    legend.box.background = element_rect(colour = "black"),
    legend.title = element_text(size = rel(0.75), hjust = 0.5),
    legend.text = element_text(size = rel(0.75)),
    legend.margin = margin(c(0, 0, 0, 0), unit = "mm"),
    legend.spacing = unit(0, "mm"),
    legend.spacing.x = unit(0, "mm"),
    legend.spacing.y = unit(0, "mm"),
    panel.spacing.y = unit(3, "mm")
  ) +
  guides(linetype = g, color = g, shape = g) +
  scale_colour_manual(values = cbPalette) +
  scale_linetype_manual(values = scale_linetype)
if (do_save == TRUE) {
  filename <- sprintf("simulations/img/Figure_I_1_1_LASSO_mean_ell2_err_BCV_BRT09_BCCH12_R%d_B%d_K%d_c1dot%s_alphaBCCH_intercept%s_standardize%s.pdf",
                      nummc, b, k,
                      str_replace(as.character(c0 - floor(c0)), "0.", ""),
                      intercept, standardize)
  ggsave(filename, height = (2 / 3) * 22.85, width = 16.5,
         device = "pdf", units = "cm")
}