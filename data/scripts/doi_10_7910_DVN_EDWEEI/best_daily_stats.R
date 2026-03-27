# BEST Daily Maximum Anomalies
calc <- TRUE
source("Code/19_wrp/fun/calculate_zonal_stats_preamble.R")

g <- readRDS(file.path(load_path, "berkeley_tmax_daily_zonal.rds"))

g <- left_join(g, fdates, by = c("GID_0" = "gid_0"))

tstats <- g
tstats <- g %>%
  filter(year %in% 2015:2019) %>%
  filter(date > (startdate - 7) & date <= startdate) %>%
  group_by(GID_0, REG_ID) %>%
  summarize(best_tanom_7d = sum(tmean, na.rm = TRUE)) %>%
  dplyr::select(GID_0, REG_ID, best_tanom_7d) %>%
  left_join(tstats, ., by = c("GID_0", "REG_ID"))
# calculate count of over 2SD days
tstats <- g %>%
  filter(year %in% 2018:2020) %>%
  filter(date > (startdate - 7) & date <= startdate) %>%
  group_by(GID_0, REG_ID) %>%
  summarize(best_tanom_2sd_7d = sum(tmean > 2, na.rm = TRUE)) %>%
  dplyr::select(GID_0, REG_ID, best_tanom_2sd_7d) %>%
  left_join(tstats, ., by = c("GID_0", "REG_ID"))

tstats_out <- subset(tstats, date == "2010-01-01", select = c(GID_0, REG_ID, best_tanom_7d, best_tanom_2sd_7d))
names(tstats_out) <- tolower(names(tstats_out))

saveRDS(tstats_out, file = file.path(out_path, "berkeley_tmax_daily_zonal_stats.rds"))
