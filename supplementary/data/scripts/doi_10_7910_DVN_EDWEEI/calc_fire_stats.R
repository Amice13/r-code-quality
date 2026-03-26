#' Calculate Fire Anomaly Statistics for Specified Time Window
#'
#' Computes fire anomaly statistics (positive anomalies only) for a given
#' number of months prior to survey date.
#'
#' @param months Integer. Number of months before survey date to include
#' @param g_out Data.frame. Dataset containing fire and date data
#'
#' @return Data.frame. Modified dataset with fire anomaly variables added
#'
#' @details
#' Creates standardized fire anomaly variables and positive-only versions.
#' Variable names follow pattern: burnanomp_mu_{months}m
calc_fire_stats <- function(months, g_out) {
  require(dplyr)
  require(lubridate)
  require(rlang)

  burnanomp <- paste0("burnanom_", months, "m_p")
  burnanom <- paste0("burnanom_", months, "m")

  g_out <- g_out %>%
    filter(
      date >= add_with_rollback(startdate, -months(months)) &
        date < startdate
    ) %>%
    mutate(
      !!burnanom := ifelse(
        burnarea_sd_m == 0,
        0,
        (burn_modis - burnarea_mu_m) / burnarea_sd_m
      ),
      !!burnanomp := ifelse(!!sym(burnanom) > 0, !!sym(burnanom), 0)
    ) %>%
    group_by(GID_0, REG_ID) %>%
    summarize(
      !!paste0("burnanomp_mu_", months, "m") :=
        mean(!!sym(burnanomp), na.rm = TRUE)
    ) %>%
    left_join(g_out, ., by = c("GID_0", "REG_ID"))

  return(g_out)
}
