#########################################################
# Panel Quantile Regression (EU-29) with Fixed Effects
# Quantile Coefficient Plot (10% Significance)
#########################################################

# 1. Install & load packages
required_pkgs <- c("rqpd", "quantreg", "ggplot2", "dplyr")

new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs) > 0) install.packages(new_pkgs)

library(rqpd)
library(quantreg)
library(ggplot2)
library(dplyr)

# -------------------------------------------------------
# 2. Declare panel structure
# data must include: country_id, year
# -------------------------------------------------------
data$country_id <- as.factor(data$country_id)
data$year <- as.numeric(data$year)

# -------------------------------------------------------
# 3. Quantiles
# -------------------------------------------------------
taus <- seq(0.1, 0.9, by = 0.1)

# -------------------------------------------------------
# 4. Estimate Panel Quantile Regression (Fixed Effects)
# -------------------------------------------------------
qr_models <- lapply(taus, function(tau) {
  rqpd(
    ENV ~ GPRT + POIL + PGAS + EUI,
    data = data,
    panel = TRUE,
    index = c("country_id", "year"),
    model = "within",
    tau = tau,
    method = "br"
  )
})

# -------------------------------------------------------
# 5. Extract coefficients and p-values
# -------------------------------------------------------
results <- lapply(seq_along(qr_models), function(i) {
  
  s <- summary(qr_models[[i]])
  coef_table <- s$coefficients
  
  data.frame(
    Variable    = rownames(coef_table),
    Coefficient = coef_table[, 1],
    StdError    = coef_table[, 2],
    t_value     = coef_table[, 3],
    p_value     = coef_table[, 4],
    Tau         = taus[i],
    row.names   = NULL
  )
})

results_df <- bind_rows(results)

# Remove fixed effects
results_df <- results_df %>%
  filter(!grepl("factor\\(country_id\\)", Variable)) %>%
  mutate(Significant_10 = p_value < 0.10)

# -------------------------------------------------------
# 6. Plot quantile coefficient paths
# -------------------------------------------------------
ggplot(results_df, aes(x = Tau, y = Coefficient, color = Variable)) +
  geom_line(size = 1.1) +
  geom_point(size = 2.5) +
  
  # 10% significance crosses
  geom_point(
    data = subset(results_df, Significant_10),
    aes(x = Tau, y = Coefficient),
    shape = 4, size = 4, stroke = 1.3
  ) +
  
  labs(
    title = "Panel Quantile Regression Coefficients (EU-29)",
    subtitle = "Fixed Effects Model with 10% Significance Crosses",
    x = "Quantiles (τ)",
    y = "Coefficient Value",
    color = "Variables"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )
