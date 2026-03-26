# NOAA: CPC Global Unified Temperature: daily maximum temperatures
calc <- TRUE
source("Code/19_wrp/fun/calculate_zonal_stats_preamble.R")

g <- readRDS(file.path(load_path, "noaa_cpc_tmax_daily_zonal.rds"))
g <- left_join(g, fdates, by = c("GID_0" = "gid_0"))

tstats <- g

# Calculate mean and sd normals by month and day
tstats <- g %>%
  filter(year %in% 1979:2008) %>%
  group_by(GID_0, REG_ID, month, day) %>%
  summarize(
    norm_mu = mean(tmean, na.rm = TRUE),
    norm_sd = sd(tmean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(tstats, ., by = c("GID_0", "REG_ID", "month", "day"))

# Calculate temperature anomalies
tstats <- tstats %>%
  mutate(tdev = (tmean - norm_mu) / norm_sd)

# Deviations 7 days prior to survey
tstats <- tstats %>%
  filter(date >= (startdate - 7) & date < startdate) %>%
  group_by(GID_0, REG_ID) %>%
  summarize(noaa_cpc_tdev_7d = sum(tdev, na.rm = TRUE)) %>%
  left_join(tstats, ., by = c("GID_0", "REG_ID"))

tstats_out <- subset(tstats, date == "2020-01-01", select = c(GID_0, REG_ID, noaa_cpc_tdev_7d))
summary(tstats_out)

tstats_out$missing <- NULL

names(tstats_out) <- tolower(names(tstats_out))
saveRDS(tstats_out, file = file.path(out_path, "noaa_cpc_daily_zonal_statistics.rds"))
message("Saved noaa_cpc_daily_zonal_statistics.rds")
