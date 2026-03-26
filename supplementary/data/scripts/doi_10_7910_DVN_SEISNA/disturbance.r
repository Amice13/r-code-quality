
## ========================== 0) Setup (no dplyr) ==========================
suppressPackageStartupMessages({
  library(ggplot2)
  library(mgcv)
  library(gridExtra)
  library(rlang)   # for .data
  library(scales)  # for squish
})

## ========================== 1) Load data ================================
path <- "/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/peat 210 pb/stats2/AGE_DEPTH_with_Tanoms_Tabs_CAR_PAR_PRE.csv"
master <- read.csv(path, stringsAsFactors = FALSE)

library(ggplot2); library(dplyr)

# Basic box+jitter
ymax <- quantile(agg$PAR_cm_yr, 0.99, na.rm = TRUE)

library(ggbeeswarm)
breaks_par <- c(seq(0, 1, by = 0.25), 2, 3, 4, 5, 7.5, 10, 12.5)
library(ggbeeswarm)
library(gridExtra)

# LEFT: no legend
p_par <- ggplot(master, aes(disturbance, PAR_cm_yr, fill = disturbance)) +
  geom_quasirandom(aes(size = sections), width = .25, alpha = .05) +
  geom_boxplot(width = .55, alpha = .7,outlier.alpha = .05) +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 0.05),
                     breaks = breaks_par,
                     labels = scales::label_number(accuracy = 0.25),
                     expand = expansion(mult = c(0, .02))) +
  scale_fill_manual(values = pal_okabe_gv) +
  guides(size = "none", fill = "none") +                 # <- hide fill legend
  labs(y = bquote("Peat acc. (cm " * yr^{-1} * ")"), x = NULL) +
  theme_classic() + theme(legend.position = "none")      # <- remove legend

# RIGHT: legend on top
p_car <- ggplot(master, aes(disturbance, CAR_app_g_m2_yr, fill = disturbance)) +
  geom_quasirandom(aes(size = sections), width = .25, alpha = .05) +
  geom_boxplot(width = .55, alpha = .7,outlier.alpha = .05) +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 20),
                     breaks = breaks_car,
                     labels = scales::label_number(accuracy = 50),
                     expand = expansion(mult = c(0, .02))) +
  scale_fill_manual(values = pal_okabe_gv) +
  guides(size = "none") +
  labs(y = bquote("Carbon acc. (g C m"^{-2} * " " * yr^{-1} * ")"), x = NULL) +
  theme_classic() +
  theme(
    legend.position   = c(0.7,0.7),
    legend.direction  = "vertical",
    legend.box.margin = margin(0,0,0,0),
    legend.margin     = margin(0,0,0,0)
  )

grid.arrange(p_par, p_car, ncol = 2)


# ECDF
ggplot(master, aes(PAR_cm_yr, colour = disturbance)) +
  stat_ecdf(geom = "step", linewidth = .9) +
  scale_colour_manual(values = pal_okabe_gv) +  
  labs(x = bquote("Peat acc. (cm " * yr^{-1} * ")"), y = "ECDF") +
  theme_bw() + theme(legend.position = "bottom")+xlim(c(0,2.5))

library(dplyr)
library(ggplot2)

# --- 1) Elevation bands (terciles) -------------------------------------------
q <- quantile(master$elevation, probs = c(1/3, 2/3), na.rm = TRUE)
master2 <- master %>%
  mutate(
    elev_band = cut(elevation, breaks = c(-Inf, q, Inf),
                    labels = c("Low", "Mid", "High"), include.lowest = TRUE),
    elev_band = factor(elev_band, levels = c("Low","Mid","High"))
  )

  library(dplyr)
library(ggplot2)
library(lubridate)

# terciles for elevation
q <- c(3200,3800)

# --- pick ONE of the filter() lines below ---

master2 <- master %>%
  # 1) If you have a numeric calendar year column:
  filter(age_calAD_mean >= 1850) %>%                          # use >1850 if you want strictly after
  # 2) If your dates are in a Date/POSIXct column named `date`:
  # filter(year(date) >= 1850) %>%
  # 3) If you only have ages in BP (e.g., age_bp):
  # mutate(year_ce = 1950 - age_bp) %>% filter(year_ce >= 1850) %>%
  mutate(
    elev_band = cut(elevation, breaks = c(-Inf, q, Inf),
                    labels = c("Low","Mid","High"), include.lowest = TRUE),
    elev_band = factor(elev_band, levels = c("Low","Mid","High"))
  )


# 3) Forest vs aubergine (elegant, muted)
# 1) Semantically meaningful (Conserved = green, Degraded = vermillion)
pal_okabe_gv <- c(Conserved = "#009E73", Degraded = "#D55E00")

# 2) Blue vs orange (high contrast)
pal_okabe_bo <- c(Conserved = "#56B4E9", Degraded = "#E69F00")

# 3) Forest vs aubergine (elegant, muted)
pal_forest_purp <- c(Conserved = "#2E7D32", Degraded = "#6A1B9A")

# --- 2) Prep for path ordering ------------------------------------------------
plot_df <- master2 %>%
  filter(is.finite(core_depth_cm), is.finite(age_calAD_mean)) %>%
  group_by(corename) %>%
  arrange(core_depth_cm, .by_group = TRUE) %>%  # ensures lines go top→bottom
  ungroup()
library(grid)  # for unit()

p_age_depth_in <- p_age_depth +
  guides(colour = guide_legend(nrow = 1, byrow = TRUE)) +
  theme(
    legend.position      = c(0.1, 0.2),  # ~across first 2 panels; tweak x,y
    legend.justification = c(0, 1),
    legend.direction     = "horizontal",
    legend.key.width     = unit(1.2, "cm"),
    legend.background    = element_rect(fill = alpha("white", 0.7), colour = NA),
    legend.box.margin    = margin(0, 0, 0, 0),
    legend.margin        = margin(2, 2, 2, 2)
  )

p_age_depth_in

