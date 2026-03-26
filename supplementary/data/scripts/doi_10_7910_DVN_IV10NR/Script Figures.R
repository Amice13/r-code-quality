# =========================================================
# Figure 2. Selected European Union countries
# R version: 4.5.1
# =========================================================

library(sf)
library(rnaturalearth)
library(ggplot2)
library(dplyr)
library(ggspatial)

# Lista de países
countries_list <- c(
  "Austria", "Belgium", "Croatia", "Czechia", "Denmark", "Estonia", 
  "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", 
  "Italy", "Latvia", "Lithuania", "Luxembourg", "Netherlands", "Poland", 
  "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "Cyprus", "Bulgaria"
)

# Carregar mapa mundial
world <- ne_countries(scale = "medium", returnclass = "sf")

# Filtrar apenas países selecionados
europe_countries <- world %>% filter(admin %in% countries_list)

# Criar pontos para os nomes (sem ultramar, apenas superfície principal)
label_points <- st_point_on_surface(europe_countries$geometry)
label_sf <- st_sf(admin = europe_countries$admin, geometry = label_points, crs = st_crs(world))

# Paleta BBC
bbc_fill <- "#a6cee3"
bbc_border <- "#333333"

# Plot do mapa
ggplot() +
  geom_sf(data = europe_countries, fill = bbc_fill, color = bbc_border, size = 0.3) +
  geom_sf_text(data = label_sf, aes(label = admin), size = 3, fontface = "bold") +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  coord_sf(xlim = c(-10, 30), ylim = c(35, 72), expand = TRUE) +  # apenas zoom
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#f0f0f0"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  labs(title = "Selected European Union Countries")

# ========================================================
# Figure 3. Graphical representation of the P-ARDL estimation results
# R version: 4.5.1
# =========================================================

library(ggplot2)
library(dplyr)

# ---------------------------------------------------------
# Data: Model I (Dependent variable: dlnco2totk)
# ---------------------------------------------------------

df_model1 <- tibble::tribble(
  ~variable,        ~estimator, ~coef,
  
  # Long-run impacts
  "lnylabor(-1)",   "MG",   0.3801354,
  "lnylabor(-1)",   "PMG", -0.3390857,
  "lnylabor(-1)",   "FE",  -0.953987,
  
  "lnurb(-1)",      "MG",  -3.671045,
  "lnurb(-1)",      "PMG", -0.3210437,
  "lnurb(-1)",      "FE",  -0.8925144,
  
  "lntrd(-1)",      "MG",  -0.0815999,
  "lntrd(-1)",      "PMG",  0.1184575,
  "lntrd(-1)",      "FE",   0.2923331,
  
  "lnglob(-1)",     "MG",  -0.6795593,
  "lnglob(-1)",     "PMG", -1.660507,
  "lnglob(-1)",     "FE",  -1.072882,
  
  "lnrenfos(-1)",   "MG",  -1.566058,
  "lnrenfos(-1)",   "PMG", -1.477102,
  "lnrenfos(-1)",   "FE",  -1.091928,
  
  # Short-run impacts
  "dlnylabor",      "MG",   0.4471599,
  "dlnylabor",      "PMG",  0.367076,
  "dlnylabor",      "FE",   0.2414752,
  
  "dlnurb",         "MG",  21.70604,
  "dlnurb",         "PMG",  8.878129,
  "dlnurb",         "FE",  -1.397095,
  
  "dlnefp",         "MG",   0.446191,
  "dlnefp",         "PMG",  0.417732,
  "dlnefp",         "FE",   0.4326843,
  
  "dlntrd",         "MG",   0.0110717,
  "dlntrd",         "PMG",  0.0495941,
  "dlntrd",         "FE",   0.0658006,
  
  "dlnglob",        "MG",  -0.535551,
  "dlnglob",        "PMG", -0.3007589,
  "dlnglob",        "FE",   0.1868476,
  
  "dlnrenfos",      "MG",  -1.750227,
  "dlnrenfos",      "PMG", -1.721203,
  "dlnrenfos",      "FE",  -0.6729165
)

# ---------------------------------------------------------
# Order of variables (top to bottom, as in the figure)
# ---------------------------------------------------------

df_model1$variable <- factor(
  df_model1$variable,
  levels = c(
    "lnylabor(-1)", "lnurb(-1)", "lntrd(-1)",
    "lnglob(-1)", "lnrenfos(-1)",
    "dlnylabor", "dlnurb", "dlnefp",
    "dlntrd", "dlnglob", "dlnrenfos"
  )
)

# ---------------------------------------------------------
# Plot: single page, larger variable names
# ---------------------------------------------------------

ggplot(df_model1, aes(x = coef, y = variable, color = estimator)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4) +
  scale_color_manual(
    values = c(
      "FE"  = "red",
      "MG"  = "darkgreen",
      "PMG" = "blue"
    )
  ) +
  labs(
    title = "Model I (Dependent Variable: dlnco2totk)",
    x = "Parameter",
    y = "Independent Variables",
    color = "P-ARDL model estimators"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 13),
    axis.text.x = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )


library(ggplot2)
library(dplyr)

# ---------------------------------------------------------
# Data: Model II (Dependent variable: dlnco2privk)
# ---------------------------------------------------------

df_model2 <- tibble::tribble(
  ~variable,        ~estimator, ~coef,
  
  # Long-run impacts
  "lnylabor(-1)",   "MG",   0.2346206,
  "lnylabor(-1)",   "PMG", -0.6829793,
  "lnylabor(-1)",   "FE",  -0.9568428,
  
  "lnurb(-1)",      "MG",  -3.956974,
  "lnurb(-1)",      "PMG",  0.0494337,
  "lnurb(-1)",      "FE",  -0.9616819,
  
  "lntrd(-1)",      "MG",  -0.0443278,
  "lntrd(-1)",      "PMG",  0.1072307,
  "lntrd(-1)",      "FE",   0.3815073,
  
  "lnglob(-1)",     "MG",  -0.5497735,
  "lnglob(-1)",     "PMG", -1.535523,
  "lnglob(-1)",     "FE",  -1.16461,
  
  "lnrenfos(-1)",   "MG",  -1.318283,
  "lnrenfos(-1)",   "PMG", -1.414993,
  "lnrenfos(-1)",   "FE",  -1.193373,
  
  # Short-run impacts
  "dlnylabor",      "MG",   0.422253,
  "dlnylabor",      "PMG",  0.3275788,
  "dlnylabor",      "FE",   0.2344546,
  
  "dlnurb",         "MG",  21.07396,
  "dlnurb",         "PMG",  6.061711,
  "dlnurb",         "FE",  -1.064786,
  
  "dlnefp",         "MG",   0.4510689,
  "dlnefp",         "PMG",  0.4286474,
  "dlnefp",         "FE",   0.4413067,
  
  "dlntrd",         "MG",   0.028684,
  "dlntrd",         "PMG",  0.0605336,
  "dlntrd",         "FE",   0.0767347,
  
  "dlnglob",        "MG",  -0.5520267,
  "dlnglob",        "PMG", -0.3377012,
  "dlnglob",        "FE",   0.1752951,
  
  "dlnrenfos",      "MG",  -1.718243,
  "dlnrenfos",      "PMG", -1.556268,
  "dlnrenfos",      "FE",  -0.679833
)

# ---------------------------------------------------------
# Order of variables (top to bottom, as in the figure)
# ---------------------------------------------------------

df_model2$variable <- factor(
  df_model2$variable,
  levels = c(
    "lnylabor(-1)", "lnurb(-1)", "lntrd(-1)",
    "lnglob(-1)", "lnrenfos(-1)",
    "dlnylabor", "dlnurb", "dlnefp",
    "dlntrd", "dlnglob", "dlnrenfos"
  )
)

# ---------------------------------------------------------
# Plot: single page, larger variable names
# ---------------------------------------------------------

ggplot(df_model2, aes(x = coef, y = variable, color = estimator)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4) +
  scale_color_manual(
    values = c(
      "FE"  = "red",
      "MG"  = "darkgreen",
      "PMG" = "blue"
    )
  ) +
  labs(
    title = "Model II (Dependent Variable: dlnco2privk)",
    x = "Parameter",
    y = "Independent Variables",
    color = "P-ARDL model estimators"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 13),
    axis.text.x = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

library(ggplot2)
library(dplyr)

# ---------------------------------------------------------
# Data: Model III (Dependent variable: dlnco2pubk)
# ---------------------------------------------------------

df_model3 <- tibble::tribble(
  ~variable,        ~estimator, ~coef,
  
  # Long-run impacts
  "lnylabor(-1)",   "MG",   1.907061,
  "lnylabor(-1)",   "PMG", -0.3036664,
  "lnylabor(-1)",   "FE",  -1.06778,
  
  "lnurb(-1)",      "MG",  -3.483909,
  "lnurb(-1)",      "PMG",  0.0226743,
  "lnurb(-1)",      "FE",  -0.8034168,
  
  "lntrd(-1)",      "MG",  -0.4361909,
  "lntrd(-1)",      "PMG",  0.0963225,
  "lntrd(-1)",      "FE",   0.2328239,
  
  "lnglob(-1)",     "MG",  -1.516478,
  "lnglob(-1)",     "PMG", -1.626018,
  "lnglob(-1)",     "FE",  -1.266225,
  
  "lnrenfos(-1)",   "MG",  -1.695559,
  "lnrenfos(-1)",   "PMG", -1.234354,
  "lnrenfos(-1)",   "FE",  -0.2963899,
  
  # Short-run impacts
  "dlnylabor",      "MG",   0.516758,
  "dlnylabor",      "PMG",  0.343125,
  "dlnylabor",      "FE",   0.338905,
  
  "dlnurb",         "MG",  18.67131,
  "dlnurb",         "PMG", -4.902134,
  "dlnurb",         "FE",  -2.098876,
  
  "dlnefp",         "MG",   0.426206,
  "dlnefp",         "PMG",  0.4051866,
  "dlnefp",         "FE",   0.4065916,
  
  "dlntrd",         "MG",  -0.044092,
  "dlntrd",         "PMG",  0.0244125,
  "dlntrd",         "FE",   0.0355447,
  
  "dlnglob",        "MG",  -0.5199194,
  "dlnglob",        "PMG", -0.1846142,
  "dlnglob",        "FE",   0.2261266,
  
  "dlnrenfos",      "MG",  -1.772108,
  "dlnrenfos",      "PMG", -1.752655,
  "dlnrenfos",      "FE",  -0.6316779
)

# ---------------------------------------------------------
# Order of variables (top to bottom, as in the figure)
# ---------------------------------------------------------

df_model3$variable <- factor(
  df_model3$variable,
  levels = c(
    "lnylabor(-1)", "lnurb(-1)", "lntrd(-1)",
    "lnglob(-1)", "lnrenfos(-1)",
    "dlnylabor", "dlnurb", "dlnefp",
    "dlntrd", "dlnglob", "dlnrenfos"
  )
)

# ---------------------------------------------------------
# Plot: single page, larger variable names
# ---------------------------------------------------------

ggplot(df_model3, aes(x = coef, y = variable, color = estimator)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4) +
  scale_color_manual(
    values = c(
      "FE"  = "red",
      "MG"  = "darkgreen",
      "PMG" = "blue"
    )
  ) +
  labs(
    title = "Model III (Dependent Variable: dlnco2pubk)",
    x = "Parameter",
    y = "Independent Variables",
    color = "P-ARDL model estimators"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 13),
    axis.text.x = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )


# =========================================================
# Figure A1. Correlation matrix
# R version: 4.5.1
# =========================================================

library(corrplot)

# ---------------------------------------------------------
# FULL 9x9 correlation matrix (levels)
# ---------------------------------------------------------

corr_mat <- matrix(c(
  1.00,  0.99,  0.97, -0.54, -0.72,  0.03, -0.15, -0.37, -0.82,
  0.99,  1.00,  0.93, -0.53, -0.72,  0.01, -0.16, -0.37, -0.82,
  0.97,  0.93,  1.00, -0.55, -0.68,  0.07, -0.14, -0.35, -0.80,
  -0.54, -0.53, -0.55,  1.00,  0.24, -0.14,  0.19,  0.04,  0.24,
  -0.72, -0.72, -0.68,  0.24,  1.00,  0.31,  0.30,  0.45,  0.70,
  0.03,  0.01,  0.07, -0.14,  0.31,  1.00,  0.32,  0.10,  0.20,
  -0.15, -0.16, -0.14,  0.19,  0.30,  0.32,  1.00,  0.47,  0.40,
  -0.37, -0.37, -0.35,  0.04,  0.45,  0.10,  0.47,  1.00,  0.56,
  -0.82, -0.82, -0.80,  0.24,  0.70,  0.20,  0.40,  0.56,  1.00
), nrow = 9, byrow = TRUE)

colnames(corr_mat) <- rownames(corr_mat) <- c(
  "lnco2totk", "lnco2privk", "lnco2pubk",
  "lnrenfos", "lnglob", "lntrd",
  "lnefppc", "lnurb", "lnylabor"
)

# ---------------------------------------------------------
# Plot – IDENTICAL to the image
# ---------------------------------------------------------

corrplot(
  corr_mat,
  method = "color",
  type = "upper",
  diag = TRUE,
  col = colorRampPalette(c("red", "white", "blue"))(200),
  addCoef.col = "black",
  number.cex = 0.8,
  tl.col = "black",
  tl.srt = 45,
  cl.lim = c(-1, 1)
)



