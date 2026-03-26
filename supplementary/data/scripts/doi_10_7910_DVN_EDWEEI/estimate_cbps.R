# Alexander F. Gazmararian
# agazmararian@gmail.com

date()
Sys.info()

set.seed(10)

if (!requireNamespace("CBPS", quietly = TRUE)) {
  install.packages("CBPS")
}

library(CBPS)

g <- readRDS("19_wrp_noweights.rds")

shocks <- list("treat_best_tanom_7d", "treat_best_tanom_2sd_7d", "treat_modis_burnanomp_mu_6m_w1", 
               "treat_noaa_cpc_tdev_7d",
               "treat_modis_burnanomp_mu_6m_w5", "treat_modis_burnanomp_mu_5m_w1", 
               "treat_modis_burnanomp_mu_7m_w1", "treat_modis_burnanomp_mu_6m_w.5")

g_out <- list()
for (i in 1:length(shocks)) {
  print(paste0("Estimating weights for: ", shocks[i]))
  f <- as.formula(
    paste0(
      # Outcome
      shocks[[i]], "~",
      # Covariates
      "age + age2 + female + incfeel + hhsize + internet + kids_bin + riskaverse + riskund + edu + rural +",
      "gdp_log + pop_log + co2_log + oil + coal + crop + urban_reg +",
      "v2x_polyarchy + gdppc_nat_log + edu_sec + pop_nat_log + co2_nat_log + ag_share + globalreg"
    )
  )
  var_list <- all.vars(f)
  g_sub <- g[, c("wpid_random", "reg_id", "gid_0", var_list)]
  g_sub <- na.omit(g_sub)
  fit <- CBPS(f, data = g_sub, ATT = 0, method = "exact", standardize = TRUE, twostep = TRUE)
  g_sub[[paste0("w_", shocks[[i]][[1]])]] <- fit$weights
  g_out[[i]] <- g_sub[, c("wpid_random", paste0("w_", shocks[[i]][[1]]))]
}

g_all <- Reduce(function(x, y) merge(x, y, by = "wpid_random", all = TRUE), g_out)
saveRDS(g_all, "19_wrp_weights.rds")
proc.time()
