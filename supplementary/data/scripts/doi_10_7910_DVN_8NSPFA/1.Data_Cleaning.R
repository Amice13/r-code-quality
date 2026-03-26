#!/usr/bin/env Rscript
# =============================================================================
# Data_Cleaning.R
# Restructured from the provided RMarkdown into a single, runnable R script.
#
# Usage (from shell):
#   Rscript Data_Cleaning.R
#
# Or open in RStudio and run top-to-bottom.
#
# Assumptions:
# - You have the two .RData files in your working directory (or update paths below).
# - Wave 1 object is named something like Data_wave1_210411 / Data_wave1_210409.
# - Wave 2 object is named something like Data_wave2_210601 or final_data_wave2.
# - Output Excel files are written to OUTDIR.
# =============================================================================

options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(plyr)    # rbind.fill
  library(writexl) # write_xlsx
})

# -------------------------------
# User settings
# -------------------------------
WAVE1_RDATA <- "Data_wave1_210411.rdata"   # update if needed
WAVE2_RDATA <- "Data_wave2_210601.rdata"   # update if needed
OUTDIR      <- "."                        # output folder

# If you know the exact object names inside the .RData files, set them here.
# Otherwise the script will try to auto-detect.
WAVE1_OBJECT_CANDIDATES <- c("Data_wave1_210411", "Data_wave1_210409", "Data_wave1_210411_clean", "Data_wave1_210409_clean")
WAVE2_OBJECT_CANDIDATES <- c("final_data_wave2", "Data_wave2_210601", "Data_wave2_210601_clean")

# -------------------------------
# Helpers
# -------------------------------
load_rdata_object <- function(path, candidates = character()) {
  if (!file.exists(path)) stop("File not found: ", path)

  e <- new.env(parent = emptyenv())
  nm <- load(path, envir = e)

  # 1) Prefer explicit candidates if present
  hit <- intersect(candidates, nm)
  if (length(hit) >= 1) return(get(hit[1], envir = e))

  # 2) Otherwise, pick the only data.frame-like object if unique
  objs <- lapply(nm, function(x) get(x, envir = e))
  is_df <- vapply(objs, function(x) is.data.frame(x) || is.matrix(x), logical(1))
  if (sum(is_df) == 1) return(as.data.frame(objs[[which(is_df)[1]]]))

  # 3) Otherwise, try "Data_wave" heuristic
  wave_like <- grep("^Data_wave", nm, value = TRUE)
  if (length(wave_like) == 1) return(get(wave_like, envir = e))

  stop(
    "Could not unambiguously identify a data object in: ", path, "\n",
    "Objects found: ", paste(nm, collapse = ", "), "\n",
    "Please set WAVE*_OBJECT_CANDIDATES to the correct object name."
  )
}

safe_minus_one <- function(df, vars) {
  for (v in vars) {
    if (v %in% names(df)) df[[v]] <- df[[v]] - 1
  }
  df
}

safe_div0 <- function(num, den) {
  out <- ifelse(is.finite(den) & den > 0, num / den, 0)
  out[!is.finite(out) | is.na(out)] <- 0
  out
}

# Create an n×6 matrix for variables like "<prefix>_n1" ... "<prefix>_n6"
get_nmat <- function(df, prefix, idx = 1:6, sep = "_n") {
  cols <- paste0(prefix, sep, idx)
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop("Missing required columns for prefix '", prefix, "': ", paste(missing, collapse = ", "))
  }
  as.matrix(df[cols])
}

# Row-sum for a condition, treating NA as FALSE
rowcount <- function(cond_mat) rowSums(cond_mat, na.rm = TRUE)

# =============================================================================
# PART 1: Recode wave 1; combine with wave 2
# =============================================================================

dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

data1 <- load_rdata_object(WAVE1_RDATA, WAVE1_OBJECT_CANDIDATES)

# Rename columns to match your Rmd
if ("loniness" %in% names(data1)) names(data1)[names(data1) == "loniness"] <- "loneliness"
if ("hwlp_n3"  %in% names(data1)) names(data1)[names(data1) == "hwlp_n3"]  <- "help_n3"

# Subtract 1 from Likert-coded items (only if present)
minus1_vars <- c(
  "sad", "frustrated", "depressed", "hardTOdo", "male", "sleep", "happy",
  "concentrate", "live_happy", "bothering", "loneliness",
  "GoRest", "visitRF", "Hangout", "Chat"
)
data1 <- safe_minus_one(data1, minus1_vars)

# Date conversion
if ("date" %in% names(data1)) data1$date <- as.Date(data1$date)

# Create QoL and CES-D (use rowSums to preserve NA propagation like '+' would)
qol_vars <- c("WS", "fam_acti", "IR", "fam_rela", "LA", "AON", "OW")
if (!all(qol_vars %in% names(data1))) {
  stop("Missing QoL components in wave1: ", paste(setdiff(qol_vars, names(data1)), collapse = ", "))
}
data1$qol <- rowSums(data1[qol_vars], na.rm = FALSE)

ces_vars <- c("sad", "frustrated", "depressed", "hardTOdo", "sleep",
              "happy", "concentrate", "live_happy", "bothering")
if (!all(ces_vars %in% names(data1))) {
  stop("Missing CES-D components in wave1: ", paste(setdiff(ces_vars, names(data1)), collapse = ", "))
}
data1$ces_d <- rowSums(data1[ces_vars], na.rm = FALSE)

# Add wave number
data1$wave <- 1

# Load wave 2
final_data_wave2 <- load_rdata_object(WAVE2_RDATA, WAVE2_OBJECT_CANDIDATES)
final_data_wave2 <- as.data.frame(final_data_wave2)

# Add wave number to wave2 if needed
if (!"wave" %in% names(final_data_wave2)) final_data_wave2$wave <- 2

# Combine (long format, fill mismatched columns)
final <- plyr::rbind.fill(data1, final_data_wave2)

# Replace -2, -3 with -999
final[final == -2] <- -999
final[final == -3] <- -999

# Save merged raw-long data
write_xlsx(final, file.path(OUTDIR, "Second&firstwave.xlsx"))

# =============================================================================
# PART 2: Data cleaning / scale variables
# =============================================================================

covnetps <- data.frame(
  wave         = final$wave,
  egoid        = final$egoid,
  hasPartner_i = as.factor(final$single)
)

# --- Kin / non-kin masks (n1..n6) ---
fam  <- get_nmat(final, "fam")
rela <- get_nmat(final, "rela")

kin_mask    <- (fam == 1) | (rela == 1)
nonkin_mask <- (fam == 0) & (rela == 0)

kin_total_n    <- rowcount(kin_mask)
nonkin_total_n <- rowcount(nonkin_mask)

covnetps$kin_total_n    <- kin_total_n
covnetps$nonkin_total_n <- nonkin_total_n

# --- Degree of support helpers ---
count_kin_nonkin_binary <- function(prefix) {
  m <- get_nmat(final, prefix)
  list(
    kin    = rowcount(kin_mask    & (m == 1)),
    nonkin = rowcount(nonkin_mask & (m == 1))
  )
}

# hobby (same_hobby_n1..n6)
tmp <- count_kin_nonkin_binary("same_hobby")
covnetps$kin_hobbies_n    <- tmp$kin
covnetps$nonkin_hobbies_n <- tmp$nonkin

# exchange info (exch_info_n1..n6)
tmp <- count_kin_nonkin_binary("exch_info")
covnetps$kin_exchange_n    <- tmp$kin
covnetps$nonkin_exchange_n <- tmp$nonkin

# entertainment (entertainment_n1..n6)
tmp <- count_kin_nonkin_binary("entertainment")
covnetps$kin_entertainment_n    <- tmp$kin
covnetps$nonkin_entertainment_n <- tmp$nonkin

# personal (personal_n1..n6)
tmp <- count_kin_nonkin_binary("personal")
covnetps$kin_personal_n    <- tmp$kin
covnetps$nonkin_personal_n <- tmp$nonkin

# help (help_n1..n6)
tmp <- count_kin_nonkin_binary("help")
covnetps$kin_help_n    <- tmp$kin
covnetps$nonkin_help_n <- tmp$nonkin

# --- Distance to kin and non-kin ---
live_around <- get_nmat(final, "live_around")

kin_live_near    <- rowcount(kin_mask    & (live_around == 1))
nonkin_live_near <- rowcount(nonkin_mask & (live_around == 1))
covnetps$kin_live_near_p    <- safe_div0(kin_live_near,    kin_total_n)
covnetps$nonkin_live_near_p <- safe_div0(nonkin_live_near, nonkin_total_n)

kin_live_far    <- rowcount(kin_mask    & (live_around == 0))
nonkin_live_far <- rowcount(nonkin_mask & (live_around == 0))
covnetps$kin_live_far_p    <- safe_div0(kin_live_far,    kin_total_n)
covnetps$nonkin_live_far_p <- safe_div0(nonkin_live_far, nonkin_total_n)

# --- Homophily (Overall Similarity of Network) ---
homophily_prop <- function(prefix) {
  first_tie_is_kin <- (final$fam_n1 != 0) | (final$rela_n1 != 0)
  m <- get_nmat(final, prefix, sep = "_n")
  total_homophilous_ties <- rowcount(m == 1)
  result <- safe_div0(total_homophilous_ties, nonkin_total_n) # Scale down non-kin network size
  result[first_tie_is_kin] <- 0 # Zero out kinship-centric networks
  return(result)
}

covnetps$hom_age_p      <- homophily_prop("peer")
covnetps$hom_gender_p   <- homophily_prop("same_gender")
covnetps$hom_province_p <- homophily_prop("same_province")
covnetps$hom_college_p  <- homophily_prop("same_college")
covnetps$hom_school_p   <- homophily_prop("same_sch")
covnetps$hom_value_p    <- homophily_prop("same_value")

# --- Opportunity for meeting alters & Social Styles ---
covnetps$edu_status_c        <- as.factor(final$edu)
covnetps$freq_visit_o        <- final$visitRF
covnetps$freq_restaurant_o   <- final$GoRest
covnetps$freq_hangout_o      <- final$Hangout
covnetps$freq_chat_o         <- final$Chat

org_vars <- c("student_unions", "sport_organ", "prof_organ", "enter_organ", "religious", "other")
missing_org <- setdiff(org_vars, names(final))
if (length(missing_org) > 0) stop("Missing organization variables: ", paste(missing_org, collapse = ", "))
org_mat <- as.matrix(final[org_vars])
org_mat[is.na(org_mat)] <- 0
covnetps$org_involved_n <- rowSums(org_mat == 1, na.rm = TRUE)

# --- Tie strength ---
# Average years known (non-kin only)
known <- get_nmat(final, "known", sep = "_n")
nonkin_years_sum <- rowSums(ifelse(nonkin_mask, known, 0), na.rm = TRUE)
covnetps$avg_years_known_m <- safe_div0(nonkin_years_sum, nonkin_total_n)

# Exchange multiplexity (sum of exchange/entertainment/personal/help, skipping -999)
sum_skip_missing <- function(prefixes) {
  mats <- lapply(prefixes, function(p) get_nmat(final, p, sep = "_n"))
  m <- do.call(cbind, mats)
  m[m == -999] <- 0
  m[is.na(m)] <- 0
  rowSums(m, na.rm = TRUE)
}
exchange_sum <- sum_skip_missing(c("exch_info", "entertainment", "personal", "help"))
covnetps$exchange_multiplexity_m <- safe_div0(exchange_sum, kin_total_n + nonkin_total_n)

# Tie strain: reproached + upset + demands
reproached <- get_nmat(final, "reproached", sep = "_n")
upset      <- get_nmat(final, "upset",      sep = "_n")
demands    <- get_nmat(final, "demands",    sep = "_n")
strain     <- reproached + upset + demands

kin_strain_sum    <- rowSums(ifelse(kin_mask,    strain, 0), na.rm = TRUE)
nonkin_strain_sum <- rowSums(ifelse(nonkin_mask, strain, 0), na.rm = TRUE)
covnetps$kin_tie_strain_m    <- safe_div0(kin_strain_sum,    kin_total_n)
covnetps$nonkin_tie_strain_m <- safe_div0(nonkin_strain_sum, nonkin_total_n)

# Frequency see (meet_LW)
meet_lw <- get_nmat(final, "meet_LW", sep = "_n")
kin_meet_sum    <- rowSums(ifelse(kin_mask,    meet_lw, 0), na.rm = TRUE)
nonkin_meet_sum <- rowSums(ifelse(nonkin_mask, meet_lw, 0), na.rm = TRUE)
covnetps$kin_freq_see_m    <- safe_div0(kin_meet_sum,    kin_total_n)
covnetps$nonkin_freq_see_m <- safe_div0(nonkin_meet_sum, nonkin_total_n)

# Frequency online (online)
online <- get_nmat(final, "online", sep = "_n")
kin_online_sum    <- rowSums(ifelse(kin_mask,    online, 0), na.rm = TRUE)
nonkin_online_sum <- rowSums(ifelse(nonkin_mask, online, 0), na.rm = TRUE)
covnetps$kin_freq_online_m    <- safe_div0(kin_online_sum,    kin_total_n)
covnetps$nonkin_freq_online_m <- safe_div0(nonkin_online_sum, nonkin_total_n)

# Closeness (close == 3)
close <- get_nmat(final, "close", sep = "_n")
kin_close_cnt    <- rowcount(kin_mask    & (close == 3))
nonkin_close_cnt <- rowcount(nonkin_mask & (close == 3))
covnetps$kin_pct_close_p    <- safe_div0(kin_close_cnt,    kin_total_n)
covnetps$nonkin_pct_close_p <- safe_div0(nonkin_close_cnt, nonkin_total_n)

# Role multiplexity
# Sum of fam + rela + bfgf + sch + work + social_organ within each alter, only if fam_nk != -999
bfgf         <- get_nmat(final, "bfgf",         sep = "_n")
sch          <- get_nmat(final, "sch",          sep = "_n")
work         <- get_nmat(final, "work",         sep = "_n")
social_organ <- get_nmat(final, "social_organ", sep = "_n")

role_sum_per_alter <- fam + rela + bfgf + sch + work + social_organ
valid_alter <- (fam != -999) & !is.na(fam)

role_sum_total <- rowSums(ifelse(valid_alter, role_sum_per_alter, 0), na.rm = TRUE)
covnetps$role_multiplexity_m <- safe_div0(role_sum_total, kin_total_n + nonkin_total_n)

# Subsample density (based on column positions used in the original Rmd)
# NOTE: This is fragile because it relies on column indices 260:274 being the tie indicators.
if (!"net_size" %in% names(final)) stop("Missing 'net_size' column needed for subsample_density.")
if (ncol(final) < 274) stop("Expected at least 274 columns in 'final' to compute subsample_density (uses cols 260:274).")

final_copy <- final
final_copy$net_size[final_copy$net_size > 6] <- 6

subsample_density <- numeric(nrow(final_copy))
for (i in seq_len(nrow(final_copy))) {
  ns <- final_copy$net_size[i]

  if (is.na(ns) || ns <= 1) {
    subsample_density[i] <- 0
  } else if (ns == 2) {
    subsample_density[i] <- final_copy[i, 260] / 1
  } else if (ns == 3) {
    subsample_density[i] <- sum(final_copy[i, c(260, 261, 265)], na.rm = TRUE) / 3
  } else if (ns == 4) {
    subsample_density[i] <- sum(final_copy[i, c(260, 261, 262, 265, 266, 269)], na.rm = TRUE) / 6
  } else if (ns == 5) {
    subsample_density[i] <- sum(final_copy[i, c(260, 261, 262, 263, 265, 266, 267, 269, 270, 272)], na.rm = TRUE) / 10
  } else if (ns == 6) {
    subsample_density[i] <- sum(final_copy[i, 260:274], na.rm = TRUE) / 15
  } else {
    subsample_density[i] <- 0
  }
}
covnetps$subsample_density_p <- subsample_density

# Work-based: hom_work among non-kin & sum of frequency for those work ties
hom_work_cnt <- rowcount(nonkin_mask & (work == 1))
covnetps$hom_work_p <- safe_div0(hom_work_cnt, nonkin_total_n)

frequency <- get_nmat(final, "frequency", sep = "_n")
freq_work_sum <- rowSums(ifelse(nonkin_mask & (work == 1), frequency, 0), na.rm = TRUE)
freq_work_sum[is.na(freq_work_sum) | !is.finite(freq_work_sum)] <- 0
covnetps$freq_work_o <- freq_work_sum

# =============================================================================
# Save output
# =============================================================================
write_xlsx(covnetps, file.path(OUTDIR, "covnetps_forRF.xlsx"))

message("Done. Outputs written to: ", normalizePath(OUTDIR))
