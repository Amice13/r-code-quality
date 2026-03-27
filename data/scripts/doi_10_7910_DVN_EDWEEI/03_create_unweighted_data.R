# Alexander F. Gazmararian
# agazmararian@gmail.com

library(fixest)
library(here)
library(lubridate)
library(terra)
library(tidyverse)

risk <- readRDS(here("Data", "inter", "19_wrp", "wrp_processed.rds"))
include <- readRDS(here("Data", "inter", "19_wrp", "countries_include.rds"))
risk <- filter(risk, country %in% include$country)

fdates <- readRDS(here("Data", "inter", "19_wrp", "field_dates.rds"))
fdates$month <- factor(month.abb[month(fdates$startdate)], levels = month.abb)
fdates$month <- factor(fdates$month, ordered = FALSE)
fdates <- fdates %>% dplyr::rename(fielddate = startdate)
fdates <- subset(fdates, select = -yday)

regid <- readRDS(here("Data", "inter", "19_wrp", "admin_wrp_crosswalk.rds"))
regid <- regid %>%
  filter(!is.na(wpid_random) & gid_0 %in% include$gid_0) %>%
  dplyr::select(-reg_lvl)

g <- left_join(risk, regid, by = "wpid_random")

# Load zonal covariates
zonal_files <- list.files(here("Data", "inter", "19_wrp", "zonal_output"), full.names = TRUE)

# Check if zonal files exist
if (length(zonal_files) == 0) {
  stop("ERROR: No zonal output files found in Data/inter/19_wrp/zonal_output.\n",
       "This indicates that the zonal processing step (02_process_zonal.R) did not complete successfully.\n",
       "Please ensure all zonal processing scripts completed before running this script.")
}

cat("Loading", length(zonal_files), "zonal files...\n")
zonal <- lapply(zonal_files, function(f) {
  tryCatch({
    readRDS(f)
  }, error = function(e) {
    warning("Failed to read zonal file: ", f, " - ", e$message)
    return(NULL)
  })
})

# Remove NULL entries from failed reads
zonal <- zonal[!sapply(zonal, is.null)]

if (length(zonal) == 0) {
  stop("ERROR: No zonal files could be successfully loaded.\n",
       "This indicates corrupted or incomplete zonal output files.")
}

zonal <- lapply(zonal, function(x) {
  names(x) <- tolower(names(x))
  # Ensure data frame is in memory
  if (inherits(x, "tbl_lazy") || inherits(x, "tbl_sql")) {
    x <- collect(x)
  }
  return(x)
})

# Use Reduce with collect() to ensure in-memory joins
zonal_out <- Reduce(function(x, y) {
  if (inherits(x, "tbl_lazy") || inherits(x, "tbl_sql")) x <- collect(x)
  if (inherits(y, "tbl_lazy") || inherits(y, "tbl_sql")) y <- collect(y)
  dplyr::left_join(x, y, by = c("reg_id", "gid_0"))
}, zonal)

# Validate that required variables exist before joining
required_vars <- c("reg_id", "gid_0", "gdp_50", "pop")
missing_vars <- setdiff(required_vars, names(zonal_out))
if (length(missing_vars) > 0) {
  stop("ERROR: Required variables missing from zonal data: ", paste(missing_vars, collapse = ", "), "\n",
       "Available variables: ", paste(names(zonal_out), collapse = ", "), "\n",
       "This indicates that some zonal processing scripts did not complete successfully.\n",
       "Missing variables suggest these scripts may have failed:\n",
       "  - gdp_50: likely from gdp.R in merge_zonal\n",
       "  - pop: likely from pop.R in merge_zonal\n",
       "Please check the zonal processing logs and ensure all merge_zonal scripts completed.")
}

g <- dplyr::left_join(g, zonal_out, by = c("reg_id", "gid_0"))

# Load country covariates
nat <- readRDS(here("Data", "inter", "19_wrp", "wdi_country_covariates.rds"))
nat <- nat %>% dplyr::select(-c(country, iso3c))
g <- dplyr::left_join(g, nat, by = "gid_0")

# Set up code for merging V-dem
vdem <- readRDS(here("Data", "inter", "19_wrp", "vdem_country.rds"))
g$country_vdem <- g$country
g$country_vdem[g$gid_0 == "PSE" & g$reg_id == 1] <- "Palestine/West Bank"
g$country_vdem[g$gid_0 == "PSE" & g$reg_id == 2] <- "Palestine/West Bank"
g$country_vdem[g$gid_0 == "PSE" & g$reg_id == 3] <- "Palestine/West Bank"
g$country_vdem[g$gid_0 == "PSE" & g$reg_id == 4] <- "Palestine/West Bank"
g$country_vdem[g$gid_0 == "PSE" & g$reg_id == 5] <- "Palestine/West Bank"
g$country_vdem[g$gid_0 == "PSE" & g$reg_id == 6] <- "Palestine/West Bank"
g$country_vdem[g$gid_0 == "PSE" & g$reg_id == 7] <- "Palestine/West Bank"
g$country_vdem[g$gid_0 == "PSE" & g$reg_id == 8] <- "Palestine/West Bank"
g$country_vdem[g$gid_0 == "PSE" & g$reg_id == 9] <- "Palestine/West Bank"
g$country_vdem[g$gid_0 == "PSE" & g$reg_id == 10] <- "Palestine/West Bank"
g$country_vdem[g$gid_0 == "PSE" & g$reg_id == 11] <- "Palestine/West Bank"
g$country_vdem[g$gid_0 == "PSE" & g$reg_id == 12] <- "Palestine/Gaza"
g$country_vdem[g$gid_0 == "PSE" & g$reg_id == 13] <- "Palestine/Gaza"
g$country_vdem[g$gid_0 == "PSE" & g$reg_id == 14] <- "Palestine/Gaza"
g$country_vdem[g$gid_0 == "PSE" & g$reg_id == 15] <- "Palestine/Gaza"
g$country_vdem[g$gid_0 == "PSE" & g$reg_id == 16] <- "Palestine/Gaza"

g <- dplyr::left_join(g, vdem, by = c("country_vdem" = "country_name"))

g <- g %>%
  group_by(gid_0) %>%
  summarize(
    nat_dam = sum(gdp_50 * pop, na.rm = TRUE) / sum(pop, na.rm = TRUE),
    nat_dam_bin = as.integer(nat_dam < 1),
    .groups = "drop"
  ) %>%
  left_join(g, ., by = "gid_0")

# Create country_region label
g$cty_reg <- factor(with(g, paste(country, reg_id, sep = "_")))

# Standardize covariates by global region
shocks <- c("best_tanom_7d", "best_tanom_2sd_7d", "modis_burnanomp_mu_6m_w1",
            "noaa_cpc_tdev_7d", "modis_burnanomp_mu_6m_w5", "modis_burnanomp_mu_5m_w1", 
            "modis_burnanomp_mu_7m_w1", "modis_burnanomp_mu_6m_w.5")

for (i in 1:length(shocks)) {
  f <- as.formula(paste0(shocks[i], "~ globalreg"))
  g[[paste0(shocks[i], "_z")]] <- g[[shocks[i]]] / feols(f, g) |>
    resid() |>
    sd()
}

for (i in 1:length(shocks)) {
  g[[paste0("treat_", shocks[[i]])]] <- g$reg_loser_50 * g[[paste0(shocks[[i]], "_z")]]
}

g <- g %>% filter(!is.na(reg_id))

g$age2 <- g$age ^ 2

saveRDS(g, here("Data", "inter", "19_wrp", "19_wrp_noweights.rds"))
message("Created 19_wrp_noweights.rds")