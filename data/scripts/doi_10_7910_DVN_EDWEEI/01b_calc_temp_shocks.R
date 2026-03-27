# Alexander F. Gazmararian
# agazmararian@gmail.com

library(here)
library(parallel)
library(tidyverse)

ces_long <- readRDS(here("Data", "inter", "ces", "ces10-14.rds"))

tmax <- readRDS(here("Data", "inter", "usa", "tmax.rds"))
tlong <- tmax %>%
  pivot_longer(cols = c(X1:X31), names_to = "day", values_to = "tmax")
tlong <- tlong %>% filter(tmax != -999.99)
tlong$day <- as.numeric(gsub("X", "", tlong$day))
tlong$date <- as.Date(paste(tlong$Year, tlong$Month, tlong$day, sep = "-"))
tlong <- tlong %>% filter(!is.na(fips))
# Calculate first climate normal
n <- tlong %>%
  filter(Year <= 1980 & Year >= 1951) %>%
  group_by(Month, day, fips) %>%
  summarize(
    mu80 = mean(tmax, na.rm = TRUE),
    sd80 = sd(tmax, na.rm = TRUE),
    .groups = "drop"
  )
# Get dataset for calculating stats
tsub <- tlong %>% filter(Year %in% c(2010, 2012, 2014))
tsub <- left_join(tsub, n, by = c("Month", "day", "fips"))
# Get fips and survey field dates
g <- subset(ces_long, select = c(caseid, year, starttime, countyfips))
# Impute missing start times with minimum survey time for that year
g %>%
  group_by(year) %>%
  summarize(min = min(starttime, na.rm = TRUE))
g <- g %>%
  mutate(
    starttime = case_when(
      is.na(starttime) & year == 2010 ~ as.Date("2010-10-01"),
      is.na(starttime) & year == 2012 ~ as.Date("2012-10-02"),
      is.na(starttime) & year == 2014 ~ as.Date("2014-10-03"),
      T ~ starttime
    )
  )
g <- subset(g, select = -year)
g <- unique(g)

calculate_stats <- function(i, g, tsub) {
  anom_7d <- tsub %>%
    filter(fips == g$countyfips[i] & date >= g$starttime[i] - 7 & date <= g$starttime[i]) %>%
    mutate(tanom = (tmax - mu80) / sd80) %>%
    summarize(tanom = as.integer(sum(tanom > 2, na.rm = TRUE) > 1), .groups = "drop")
  
  return(anom_7d)
}

# Set up parallel processing
num_cores <- min(parallel::detectCores() - 1, 6)
message(sprintf("Using %d cores for parallel processing", num_cores))

expected_rows <- nrow(g)
sysname <- Sys.info()["sysname"]

if (sysname == "Windows") {
  # Windows: Must use PSOCK (no fork support)
  message("Using PSOCK cluster (Windows)")
  cl <- makeCluster(num_cores, type = "PSOCK")
  on.exit(stopCluster(cl), add = TRUE)
  clusterExport(cl, c("g", "tsub", "calculate_stats"))
  clusterEvalQ(cl, library(dplyr))
  t_out <- parLapply(cl, 1:nrow(g), function(i) calculate_stats(i, g, tsub))
} else {
  # macOS/Linux: Try mclapply first (faster), fall back to PSOCK if it fails
  
  # Clear memory before fork to reduce fragmentation issues
  gc(verbose = FALSE, full = TRUE)
  
  message("Trying mclapply (fork-based)...")
  t_out <- mclapply(1:nrow(g), calculate_stats, g = g, tsub = tsub, mc.cores = num_cores)
  
  # Check if mclapply worked correctly
  t_out_rows <- sum(sapply(t_out, function(x) if (is.null(x)) 0 else nrow(x)))
  
  if (t_out_rows != expected_rows) {
    # mclapply failed - fall back to PSOCK
    warning(sprintf("mclapply returned %d/%d rows - falling back to PSOCK", t_out_rows, expected_rows))
    
    cl <- makeCluster(num_cores, type = "PSOCK")
    on.exit(stopCluster(cl), add = TRUE)
    clusterExport(cl, c("g", "tsub", "calculate_stats"))
    clusterEvalQ(cl, library(dplyr))
    t_out <- parLapply(cl, 1:nrow(g), function(i) calculate_stats(i, g, tsub))
  } else {
    message("mclapply succeeded")
  }
}

t_out.unlist <- do.call(rbind, t_out)
df_out <- cbind(g, t_out.unlist)

saveRDS(df_out, here("Data", "inter", "ces", "usa_tanom.rds"))
message("Calculated temperature shocks and saved to Data/inter/ces/usa_tanom.rds")
