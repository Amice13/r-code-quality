################################################################################
# ANALYSIS SCRIPT: Name-Based Instructor-Student Interaction Questionnaire
# Pilot Study - Psychometric Evaluation
################################################################################
# 
# DESCRIPTION:
# This script performs a comprehensive psychometric evaluation of a pilot
# questionnaire assessing name-based instructor-student interaction in medical
# teaching contexts. The analysis includes:
# - Item screening and descriptive statistics
# - Factorability assessment (KMO, Bartlett's test)
# - Exploratory factor analysis (EFA)
# - Reliability analysis (Cronbach's alpha, McDonald's omega)
# - Rasch partial credit model (PCM) analysis
# - Local independence assessment (Yen's Q3)
# - Test information functions (TIF)
#
# AUTHORS: [Liam Philipp Weber, Philipp Huber, Julius Wiemschulte]
# DATE: January 2026
# R VERSION: 4.x or higher required
#
# INSTRUCTIONS FOR DATA IMPORT:
# 1. Run the script line by line or section by section
# 2. When prompted, select your Excel data file using the file dialog
# 3. The script will automatically process the data and generate results
#
################################################################################

# ==============================================================================
# 1. SETUP AND PACKAGE LOADING
# ==============================================================================

# Clear workspace (optional - comment out if not desired)
rm(list = ls())

# Required packages
required_packages <- c("openxlsx", "psych", "dplyr", "TAM", "mirt")

# Install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  install.packages(new_packages)
  cat("Installed packages:", paste(new_packages, collapse = ", "), "\n")
}

# Load packages
suppressPackageStartupMessages({
  library(openxlsx)
  library(psych)
  library(dplyr)
  library(TAM)
  library(mirt)
})

cat("All required packages loaded successfully.\n")

# Set random seed for reproducibility
set.seed(12345)

# ==============================================================================
# 2. DATA IMPORT
# ==============================================================================

cat("\n=== DATA IMPORT ===\n")
cat("Please select your Excel data file in the file dialog...\n")

# Interactive file selection
data_path <- file.choose()
cat("Selected file:", data_path, "\n")

# Load data
data_excel <- read.xlsx(data_path, sheet = 1)

# Basic data check
cat("Data dimensions:", nrow(data_excel), "rows x", ncol(data_excel), "columns\n")
cat("First few column names:", paste(head(names(data_excel)), collapse = ", "), "\n")

# Extract item columns (X1 to X59)
items_all <- dplyr::select(data_excel, matches("^X[0-9]+$"))
cat("Number of item columns found:", ncol(items_all), "\n")

# ==============================================================================
# 3. DESCRIPTIVE STATISTICS AND ITEM SCREENING
# ==============================================================================

cat("\n=== ITEM SCREENING ===\n")

# Calculate descriptive statistics for all items
desc_stats <- psych::describe(items_all) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("item") %>%
  mutate(
    missing = colSums(is.na(items_all))[item],
    var = sd^2
  ) %>%
  select(item, n, missing, mean, sd, var, min, max)

# Calculate floor and ceiling effects
floor_ceiling <- tibble(item = names(items_all)) %>%
  mutate(
    floor_prop = sapply(item, function(nm) {
      mean(items_all[[nm]] == min(items_all[[nm]], na.rm = TRUE), na.rm = TRUE)
    }),
    ceiling_prop = sapply(item, function(nm) {
      mean(items_all[[nm]] == max(items_all[[nm]], na.rm = TRUE), na.rm = TRUE)
    })
  )

# Calculate corrected item-total correlation (discrimination)
discrimination <- psych::alpha(items_all)$item.stats %>%
  as.data.frame() %>%
  tibble::rownames_to_column("item") %>%
  transmute(item, r_it = r.drop)

# Combine all item statistics
item_screening <- desc_stats %>%
  left_join(floor_ceiling, by = "item") %>%
  left_join(discrimination, by = "item") %>%
  arrange(sd)

# Identify problematic items
# Criteria: SD ≤ 0.70, floor effect > 50-75%, r_it < 0.10
problematic_items <- item_screening %>%
  filter(sd <= 0.70 | floor_prop > 0.50 | r_it < 0.10) %>%
  pull(item)

cat("Problematic items identified (N =", length(problematic_items), "):\n")
cat(paste(problematic_items, collapse = ", "), "\n")

# Items excluded based on preliminary analysis
# Note: Adjust these based on your actual screening results
excluded_items <- c("X2", "X4", "X6", "X17", "X22", "X27")
cat("\nItems excluded from further analysis:\n")
cat(paste(excluded_items, collapse = ", "), "\n")

# Create cleaned item set
items_clean <- items_all[, !(names(items_all) %in% excluded_items)]
items_clean <- na.omit(items_clean)

cat("\nSample size after listwise deletion:", nrow(items_clean), "\n")
cat("Number of items retained:", ncol(items_clean), "\n")

# ==============================================================================
# 4. FACTORABILITY ASSESSMENT
# ==============================================================================

cat("\n=== FACTORABILITY TESTS ===\n")

# Calculate correlation matrix
R <- cor(items_clean)

# Kaiser-Meyer-Olkin (KMO) measure of sampling adequacy
kmo_result <- psych::KMO(R)
cat("Overall KMO:", round(kmo_result$MSA, 3), "\n")
cat("Minimum item-level KMO:", round(min(kmo_result$MSAi), 3), "\n")

# Bartlett's test of sphericity
bartlett_result <- psych::cortest.bartlett(R, n = nrow(items_clean))
cat("Bartlett's χ²:", round(bartlett_result$chisq, 2), "\n")
cat("df:", bartlett_result$df, "\n")
cat("p-value:", format(bartlett_result$p.value, scientific = TRUE), "\n")

# Interpretation
if(kmo_result$MSA >= 0.90 & bartlett_result$p.value < 0.001) {
  cat("\n✓ Data are suitable for factor analysis.\n")
} else {
  cat("\n⚠ Warning: Data may not be optimal for factor analysis.\n")
}

# ==============================================================================
# 5. PARALLEL ANALYSIS
# ==============================================================================

cat("\n=== PARALLEL ANALYSIS ===\n")
cat("Determining optimal number of factors...\n")

# Perform parallel analysis
# This creates a scree plot automatically
fa.parallel(items_clean, fa = "fa", fm = "pa", n.iter = 100)

cat("\nNote: Parallel analysis suggests retaining factors where observed\n")
cat("eigenvalues exceed those from simulated/resampled data.\n")

# ==============================================================================
# 6. EXPLORATORY FACTOR ANALYSIS (EFA)
# ==============================================================================

cat("\n=== EXPLORATORY FACTOR ANALYSIS ===\n")

# Initial EFA with 5 factors (based on parallel analysis)
cat("\nFitting initial 5-factor model...\n")
efa_5factor <- fa(items_clean, nfactors = 5, fm = "pa", rotate = "oblimin")

cat("Model fit indices (5-factor solution):\n")
cat("  RMSR:", round(efa_5factor$rms, 4), "\n")
cat("  TLI:", round(efa_5factor$TLI, 4), "\n")
cat("  RMSEA:", round(efa_5factor$RMSEA[1], 4), "\n")

cat("\nFactor loadings (cutoff = 0.30):\n")
print(efa_5factor$loadings, cutoff = 0.30, sort = TRUE)

cat("\nFactor correlations:\n")
print(round(efa_5factor$Phi, 2))

# Note: Based on interpretability and item coverage, 
# a 4-factor solution was retained (see manuscript)

# ==============================================================================
# 7. ITEM REDUCTION PROCEDURE
# ==============================================================================

cat("\n=== ITEM REDUCTION ===\n")

# Items excluded during iterative reduction
# (in addition to the 6 items excluded earlier)
reduction_excluded <- c("X23", "X31", "X34", "X39")
cat("Additional items excluded during factor refinement:\n")
cat(paste(reduction_excluded, collapse = ", "), "\n")

# Update item set
items_reduced <- items_clean[, !(names(items_clean) %in% reduction_excluded)]

# Item reduction criteria (applied iteratively)
cat("\nItem retention criteria:\n")
cat("  - Primary loading ≥ 0.50\n")
cat("  - Cross-loading ≤ 0.30\n")
cat("  - Difference (primary - max cross) ≥ 0.20\n")
cat("  - Communality (h²) ≥ 0.30\n")
cat("  - Maximum 5 items per factor\n")

# Fit 4-factor model with reduced item set
cat("\nFitting 4-factor model with reduced item set...\n")
efa_4factor <- fa(items_reduced, nfactors = 4, fm = "pa", rotate = "oblimin")

cat("\nModel fit indices (4-factor solution):\n")
cat("  RMSR:", round(efa_4factor$rms, 4), "\n")
cat("  TLI:", round(efa_4factor$TLI, 4), "\n")
cat("  RMSEA:", round(efa_4factor$RMSEA[1], 4), "\n")

# Extract loadings
loadings_matrix <- as.data.frame(unclass(efa_4factor$loadings))

# Identify primary factor for each item
item_assignment <- data.frame(
  item = rownames(loadings_matrix),
  factor = apply(abs(loadings_matrix), 1, which.max),
  primary_loading = apply(abs(loadings_matrix), 1, max),
  h2 = efa_4factor$communality
) %>%
  arrange(factor, -primary_loading)

cat("\nItem-to-factor assignment:\n")
print(item_assignment, row.names = FALSE)

# ==============================================================================
# 8. FINAL 18-ITEM MODEL
# ==============================================================================

cat("\n=== FINAL 18-ITEM MODEL ===\n")

# Define final factor structure
# Adjust these based on your actual EFA results
final_factors <- list(
  F1 = c("X20", "X19", "X11", "X15", "X26"),
  F2 = c("X7", "X12", "X25", "X9", "X55"),
  F3 = c("X10", "X24", "X1", "X41", "X32"),
  F4 = c("X50", "X56", "X57")
)

# Extract final 18 items
final_items <- unlist(final_factors)
items_18 <- data_excel[, final_items]
items_18 <- na.omit(items_18)

cat("Final model:\n")
for(f in names(final_factors)) {
  cat("  ", f, ": ", length(final_factors[[f]]), " items\n", sep = "")
}
cat("\nTotal items:", length(final_items), "\n")
cat("Sample size (complete cases):", nrow(items_18), "\n")

# ==============================================================================
# 9. RELIABILITY ANALYSIS
# ==============================================================================

cat("\n=== RELIABILITY ANALYSIS ===\n")

# Function to calculate reliability indices
calculate_reliability <- function(data_subset) {
  data_clean <- na.omit(data_subset)
  
  # Cronbach's alpha
  alpha_result <- psych::alpha(data_clean)
  alpha_value <- alpha_result$total$raw_alpha
  
  # McDonald's omega
  omega_result <- suppressWarnings(
    psych::omega(data_clean, nfactors = 1, fm = "ml", plot = FALSE)
  )
  omega_value <- omega_result$omega.tot
  
  return(c(
    alpha = round(alpha_value, 3),
    omega = round(omega_value, 3)
  ))
}

# Calculate reliability for each factor
reliability_results <- lapply(names(final_factors), function(fn) {
  items_factor <- data_excel[, final_factors[[fn]], drop = FALSE]
  rel <- calculate_reliability(items_factor)
  data.frame(
    factor = fn,
    n_items = length(final_factors[[fn]]),
    alpha = rel["alpha"],
    omega = rel["omega"]
  )
})

reliability_table <- do.call(rbind, reliability_results)

cat("\nReliability estimates:\n")
print(reliability_table, row.names = FALSE)

# ==============================================================================
# 10. RASCH PARTIAL CREDIT MODEL (PCM) ANALYSIS
# ==============================================================================

cat("\n=== RASCH PARTIAL CREDIT MODEL ANALYSIS ===\n")

# Function to fit PCM and extract key statistics
fit_pcm_factor <- function(factor_name, item_names, data) {
  # Extract and clean data
  dat <- data[, item_names, drop = FALSE]
  dat <- na.omit(dat)
  
  # Convert to numeric (ensure proper format)
  dat <- as.data.frame(lapply(dat, as.numeric))
  
  # Fit PCM model
  tryCatch({
    mod <- TAM::tam.mml(
      resp = dat,
      irtmodel = "PCM",
      control = list(progress = FALSE)
    )
    
    # Item fit statistics
    fit <- TAM::tam.fit(mod)
    itemfit <- fit$itemfit[, c("Outfit", "Infit"), drop = FALSE]
    
    # Person separation reliability (EAP and WLE)
    eap_rel <- as.numeric(mod$EAP.rel)
    
    wle <- TAM::tam.wle(mod)
    wle_rel <- if(!is.null(wle$WLE.rel)) {
      as.numeric(wle$WLE.rel)[1]
    } else {
      NA_real_
    }
    
    # Aggregate item fit (median across categories)
    item_names_fit <- sub("_Cat[0-9]+$", "", rownames(itemfit))
    itemfit_agg <- aggregate(
      itemfit,
      by = list(item = item_names_fit),
      FUN = median,
      na.rm = TRUE
    )
    
    list(
      factor = factor_name,
      n = nrow(dat),
      k = ncol(dat),
      EAP_reliability = round(eap_rel, 3),
      WLE_reliability = round(wle_rel, 3),
      itemfit = itemfit_agg,
      model = mod,
      success = TRUE
    )
    
  }, error = function(e) {
    list(
      factor = factor_name,
      n = nrow(dat),
      k = ncol(dat),
      error = conditionMessage(e),
      success = FALSE
    )
  })
}

# Fit PCM for all factors
pcm_results <- lapply(names(final_factors), function(fn) {
  fit_pcm_factor(fn, final_factors[[fn]], data_excel)
})
names(pcm_results) <- names(final_factors)

# Summary table
pcm_summary <- do.call(rbind, lapply(names(pcm_results), function(fn) {
  res <- pcm_results[[fn]]
  
  if(res$success) {
    # Check item fit quality
    fit <- res$itemfit
    infit_ok <- fit$Infit >= 0.7 & fit$Infit <= 1.3
    outfit_ok <- fit$Outfit >= 0.7 & fit$Outfit <= 1.3
    
    data.frame(
      factor = fn,
      n = res$n,
      k = res$k,
      EAP_rel = res$EAP_reliability,
      WLE_rel = res$WLE_reliability,
      infit_acceptable = paste0(sum(infit_ok, na.rm = TRUE), "/", res$k),
      outfit_acceptable = paste0(sum(outfit_ok, na.rm = TRUE), "/", res$k)
    )
  } else {
    data.frame(
      factor = fn,
      n = res$n,
      k = res$k,
      EAP_rel = NA,
      WLE_rel = NA,
      infit_acceptable = "ERROR",
      outfit_acceptable = res$error
    )
  }
}))

cat("\nRasch PCM summary:\n")
print(pcm_summary, row.names = FALSE)

cat("\nNote: Acceptable fit range: Infit/Outfit 0.7-1.3\n")

# ==============================================================================
# 11. LOCAL INDEPENDENCE ASSESSMENT (YEN'S Q3)
# ==============================================================================

cat("\n=== YEN'S Q3 ANALYSIS ===\n")
cat("Assessing local independence within factors...\n")

# Function to calculate Q3 statistics
calculate_q3 <- function(factor_name, item_names, data) {
  # Extract and clean data
  dat <- data[, item_names, drop = FALSE]
  dat <- na.omit(dat)
  dat <- as.data.frame(lapply(dat, as.numeric))
  
  # Fit 1-dimensional GPCM
  mod <- mirt(dat, 1, itemtype = "gpcm", verbose = FALSE)
  
  # Calculate Q3 residual correlations
  q3_matrix <- residuals(mod, type = "Q3")
  diag(q3_matrix) <- NA
  
  # Extract upper triangle
  q3_values <- q3_matrix[upper.tri(q3_matrix)]
  q3_values <- q3_values[!is.na(q3_values)]
  
  # Summary statistics
  summary_stats <- data.frame(
    factor = factor_name,
    n = nrow(dat),
    k = ncol(dat),
    mean_abs_Q3 = round(mean(abs(q3_values)), 3),
    max_abs_Q3 = round(max(abs(q3_values)), 3),
    prop_abs_gt_0.20 = round(mean(abs(q3_values) > 0.20), 3),
    prop_abs_gt_0.30 = round(mean(abs(q3_values) > 0.30), 3)
  )
  
  # Identify highest absolute Q3 pairs
  idx <- which(upper.tri(q3_matrix) & !is.na(q3_matrix), arr.ind = TRUE)
  pairs <- data.frame(
    item1 = colnames(q3_matrix)[idx[, 2]],
    item2 = rownames(q3_matrix)[idx[, 1]],
    Q3 = q3_matrix[idx]
  ) %>%
    arrange(-abs(Q3)) %>%
    head(5)
  
  list(
    summary = summary_stats,
    top_pairs = pairs,
    q3_matrix = q3_matrix
  )
}

# Calculate Q3 for all factors
q3_results <- lapply(names(final_factors), function(fn) {
  calculate_q3(fn, final_factors[[fn]], data_excel)
})
names(q3_results) <- names(final_factors)

# Combined summary
q3_summary <- do.call(rbind, lapply(q3_results, function(x) x$summary))

cat("\nQ3 summary statistics:\n")
print(q3_summary, row.names = FALSE)

cat("\nNote: |Q3| > 0.20 may indicate potential local dependence\n")
cat("\nTop 5 item pairs by |Q3| for each factor:\n")
for(fn in names(q3_results)) {
  cat("\n", fn, ":\n", sep = "")
  print(q3_results[[fn]]$top_pairs, row.names = FALSE)
}

# ==============================================================================
# 12. TEST INFORMATION FUNCTIONS (TIF)
# ==============================================================================

cat("\n=== TEST INFORMATION FUNCTIONS ===\n")
cat("Calculating TIF for Rasch-like polytomous models...\n")

# Theta grid for TIF calculation
theta <- seq(-4, 4, length.out = 401)
Theta_mat <- matrix(theta, ncol = 1)

# Function to fit Rasch-like GPCM and extract TIF
calculate_tif <- function(factor_name, item_names, data, theta_matrix) {
  # Extract and clean data
  dat <- data[, item_names, drop = FALSE]
  dat <- na.omit(dat)
  dat <- as.data.frame(lapply(dat, as.numeric))
  
  # Fit initial GPCM
  mod_init <- mirt(dat, 1, itemtype = "gpcm", verbose = FALSE)
  
  # Fix all slopes to 1 (Rasch-like constraint)
  pars <- mod2values(mod_init)
  pars$value[pars$name == "a1"] <- 1
  pars$est[pars$name == "a1"] <- FALSE
  
  # Refit with constrained slopes
  mod <- mirt(dat, 1, itemtype = "gpcm", pars = pars, verbose = FALSE)
  
  # Calculate test information
  info <- testinfo(mod, Theta = theta_matrix)
  
  data.frame(
    factor = factor_name,
    theta = theta,
    information = as.numeric(info),
    n = nrow(dat)
  )
}

# Calculate TIF for all factors
tif_data <- do.call(rbind, lapply(names(final_factors), function(fn) {
  calculate_tif(fn, final_factors[[fn]], data_excel, Theta_mat)
}))

# Create TIF plot
cat("\nGenerating TIF plot...\n")

# Set up plot layout (2x2 grid)
par(mfrow = c(2, 2), mar = c(4, 4, 2.5, 1), oma = c(0, 0, 2, 0))

for(fn in names(final_factors)) {
  factor_data <- tif_data[tif_data$factor == fn, ]
  
  plot(
    factor_data$theta,
    factor_data$information,
    type = "l",
    lwd = 2,
    col = "darkblue",
    xlab = expression(theta),
    ylab = "Test Information",
    main = paste0(fn, " (n = ", unique(factor_data$n), ")"),
    ylim = c(0, max(tif_data$information) * 1.1)
  )
  
  # Add reference line at theta = 0
  abline(v = 0, lty = 2, col = "gray50")
  
  # Add grid
  grid(col = "gray90")
}

# Add overall title
mtext(
  "Test Information Functions (Rasch-like Polytomous Models)",
  outer = TRUE,
  cex = 1.1,
  font = 2
)

# Reset plot parameters
par(mfrow = c(1, 1))

cat("✓ TIF plot created\n")

# ==============================================================================
# 13. SAVE RESULTS
# ==============================================================================

cat("\n=== SAVING RESULTS ===\n")

# Compile all results into a single list
analysis_results <- list(
  # Sample information
  sample = list(
    n_total = nrow(data_excel),
    n_complete_59items = sum(complete.cases(data_excel[, paste0("X", 1:59)])),
    n_final_18items = nrow(items_18)
  ),
  
  # Item screening
  item_screening = item_screening,
  excluded_items = c(excluded_items, reduction_excluded),
  
  # Factorability
  kmo = list(
    overall = kmo_result$MSA,
    item_level = kmo_result$MSAi
  ),
  bartlett = list(
    chisq = bartlett_result$chisq,
    df = bartlett_result$df,
    p_value = bartlett_result$p.value
  ),
  
  # EFA
  efa_4factor = efa_4factor,
  final_factors = final_factors,
  
  # Reliability
  reliability = reliability_table,
  
  # Rasch PCM
  pcm_summary = pcm_summary,
  pcm_detailed = pcm_results,
  
  # Q3
  q3_summary = q3_summary,
  q3_detailed = q3_results,
  
  # TIF
  tif_data = tif_data,
  
  # Session info
  session_info = sessionInfo()
)

# Save results as RDS file
results_file <- "analysis_results.rds"
saveRDS(analysis_results, file = results_file)
cat("✓ Results saved to:", results_file, "\n")

# Optional: Save as Excel file for easy viewing
if(require(openxlsx, quietly = TRUE)) {
  wb <- createWorkbook()
  
  addWorksheet(wb, "Item_Screening")
  writeData(wb, "Item_Screening", item_screening)
  
  addWorksheet(wb, "Reliability")
  writeData(wb, "Reliability", reliability_table)
  
  addWorksheet(wb, "PCM_Summary")
  writeData(wb, "PCM_Summary", pcm_summary)
  
  addWorksheet(wb, "Q3_Summary")
  writeData(wb, "Q3_Summary", q3_summary)
  
  excel_file <- "analysis_results.xlsx"
  saveWorkbook(wb, excel_file, overwrite = TRUE)
  cat("✓ Results also saved to:", excel_file, "\n")
}

# ==============================================================================
# 14. SESSION INFORMATION
# ==============================================================================

cat("\n=== SESSION INFORMATION ===\n")
print(sessionInfo())

cat("\n")
cat("================================================================================\n")
cat("ANALYSIS COMPLETED SUCCESSFULLY\n")
cat("================================================================================\n")
cat("\nKey results:\n")
cat("  - Final model: 18 items across 4 factors\n")
cat("  - Sample size:", nrow(items_18), "\n")
cat("  - Reliability (α):", paste(reliability_table$alpha, collapse = ", "), "\n")
cat("  - Reliability (ω):", paste(reliability_table$omega, collapse = ", "), "\n")
cat("\nAll results saved to:", results_file, "\n")
cat("================================================================================\n")

