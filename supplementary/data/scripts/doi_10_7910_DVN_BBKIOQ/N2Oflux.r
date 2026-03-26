#R
#N2O fluxes 

pcdir<-"C:/Users/jubenavides/Dropbox/papers/Guatavita fluxes/stats"
macdir<-"/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/Guatavita fluxes/stats"

setwd(macdir)
# Load necessary libraries
library(ggplot2)

# Read and clean the data
df_n2o <- read.csv("db_gutavita_N2O_all.csv", encoding = "UTF-8")
names(df_n2o) <- trimws(names(df_n2o))  # Remove trailing spaces from column names

# Filter needed columns and remove NAs
df_clean <- na.omit(df_n2o[, c("Station", "Season", "year", "WTD", "N2O_Flux_final")])

# Fit linear models per site
lm_conserved <- lm(`N2O_Flux_final` ~ WTD, data = subset(df_clean, Station == "Conserved"))
lm_degraded  <- lm(`N2O_Flux_final` ~ WTD, data = subset(df_clean, Station == "Degraded"))

# Summaries
summary(lm_conserved)
summary(lm_degraded)

# Custom color palette
site_colors <- c("Conserved" = "#009E73", "Degraded" = "#D55E00")

# Boxplot of N2O flux by Season and Site
ggplot(df_clean, aes(x = Station, y = N2O_Flux_final, fill = Station)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 1.5, alpha = 0.8) +
  scale_fill_manual(values = site_colors) +
  labs(
    title = "N₂O Fluxes by Season and Site",
    x = "Season",
    y = expression("N"[2]*"O Flux (nmol m"^{-2}*" s"^{-1}*")"),
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank())

df_clean<-read.csv("db_gutavita_N2O_all.csv",header=TRUE)
# First, compute the conversion factor
n2o_conversion <- 44 * 1e-9 * 60 * 60 * 24 * 365.25  # nmol/s → g/year

# Create new column with values in g N2O m⁻² y⁻¹
df_clean$N2O_g_m2_y <- df_clean$N2O_Flux_final * n2o_conversion

# Compute mean per group
mean_flux <- aggregate(N2O_g_m2_y ~ Station + year + Season, data = df_clean, FUN = mean)

# Compute standard deviation per group
sd_flux <- aggregate(N2O_g_m2_y ~ Station + year + Season, data = df_clean, FUN = sd)

# Merge the two tables
n2o_summary <- merge(mean_flux, sd_flux, by = c("Station", "year", "Season"))

# Rename columns
names(n2o_summary) <- c("Station", "Year", "Season", "Mean_g_m2_y", "SD_g_m2_y")

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(tibble)

df_clean <- df_clean %>% filter(is.finite(WTD), is.finite(N2O_Flux_final))

fit_one <- function(d) {
  tryCatch(
    nls(N2O_Flux_final ~ a * exp(-0.5 * ((WTD - mu)/sigma)^2),
        data = d,
        start = list(
          a     = max(d$N2O_Flux_final, na.rm = TRUE),
          mu    = stats::median(d$WTD, na.rm = TRUE),
          sigma = stats::sd(d$WTD, na.rm = TRUE)
        )),
    error = function(e) NULL
  )
}

fits_tbl <- df_clean %>%
  nest(data = -Station) %>%
  mutate(model = map(data, fit_one),
         has_fit = map_lgl(model, ~ !is.null(.x)))

pred_tbl <- fits_tbl %>%
  mutate(grid = map(data, ~ tibble(WTD = seq(min(.x$WTD, na.rm=TRUE),
                                             max(.x$WTD, na.rm=TRUE), length.out = 200))),
         pred = map2(model, grid, ~ if (is.null(.x)) NULL
                                   else mutate(.y, N2O_pred = predict(.x, newdata = .y)))) %>%
  select(Station, pred) %>%
  unnest(pred)

ggplot(df_clean, aes(WTD, N2O_Flux_final, color = Station)) +
  geom_point(alpha = 0.6) +
  geom_line(data = pred_tbl, aes(WTD, N2O_pred, color = Station), linewidth = 1.1) +
  labs(x = "Water Table Depth (cm)",
       y = expression(N[2]*O~flux~(mg~m^-2~d^-1))) +
  theme_classic()