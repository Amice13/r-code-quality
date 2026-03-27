calc <- TRUE
source("Code/19_wrp/fun/calculate_zonal_stats_preamble.R")
source("Code/19_wrp/fun/calc_fire_stats.R")

g <- readRDS(file.path(load_path, "modis_burnarea_zonal.rds"))
g <- left_join(g, fdates, by = c("GID_0" = "gid_0"))
zarea <- readRDS(here("Data", "inter", "19_wrp", "zone_area.rds"))

# Get recent data for survey period (2017 onwards)
g_out <- g %>% filter(year >= 2017)

# Calculate historical "normal" by month (2014 and earlier)
g_historical <- g %>%
  filter(year <= 2014) %>%
  group_by(GID_0, REG_ID, month) %>%
  summarize(
    burnarea_mu_m = mean(burn_modis, na.rm = TRUE),
    burnarea_sd_m = sd(burn_modis, na.rm = TRUE)
    )

# Join historical stats with recent data
g_out <- left_join(g_out, g_historical, by = c("GID_0", "REG_ID", "month"))

months <- c(5, 6, 7)
for (i in 1:length(months)) {
  result <- calc_fire_stats(months[i], g_out)
  if (!is.null(result)) {
    g_out <- result
  }
}

g_out <- subset(g_out, date == "2019-01-01", select = c(GID_0, REG_ID, burnanomp_mu_5m:burnanomp_mu_7m))

# Winsorize
firevars <- names(g_out)[3:ncol(g_out)]
for (i in 1:length(firevars)) {
  g_out[[paste0(firevars[i], "_w.5")]] <- Winsorize(g_out[[firevars[i]]], quantile(g_out[[firevars[i]]], probs = c(0, .995), na.rm = TRUE))
  g_out[[paste0(firevars[i], "_w1")]] <- Winsorize(g_out[[firevars[i]]], quantile(g_out[[firevars[i]]], probs = c(0, .99), na.rm = TRUE))
  g_out[[paste0(firevars[i], "_w5")]] <- Winsorize(g_out[[firevars[i]]], quantile(g_out[[firevars[i]]], probs = c(0, .95), na.rm = TRUE))
}

names(g_out)[3:ncol(g_out)] <- paste0("modis_", names(g_out)[3:ncol(g_out)])
names(g_out) <- tolower(names(g_out))
saveRDS(g_out, file = file.path(out_path, "modis_burnarea_zonal_stats.rds"))
