##########################################
# RQ3-RQ4: Structural Determinants of Authentic Localness
##########################################
library(tidyverse)
library(plm)
library(lmtest)
library(sandwich)
library(texreg)

# Load data
panel_data <- read_rds('data/panel_data.rds')
panel_data$content_dup_1000 <- panel_data$syndication_rate * 1000
# Create scope factor with ordered levels
panel_data$scope_factor <- factor(panel_data$New_cluster_name, 
                             levels = c("Hyperlocal", "Local", 
                                       "Market Town and Rural",
                                       "County and Regional", 
                                       "Major City and Regional",
                                       "Major Regional Daily"))

pdata <- pdata.frame(panel_data, index = c("domain", "time_period"))

# Create scope factor with ordered levels
pdata$scope_factor <- factor(pdata$New_cluster_name, 
                             levels = c("Hyperlocal", "Local", 
                                       "Market Town and Rural",
                                       "County and Regional", 
                                       "Major City and Regional",
                                       "Major Regional Daily"))

# Helper function for extracting Driscoll-Kraay SEs
get_dk_se <- function(model) {
  vcov_matrix <- vcovSCC(model, type = "HC1", maxlag = 2)
  sqrt(diag(vcov_matrix))
}

############################################################
# OPTION 1: Exclude Perfectly Separated Categories
############################################################
# Filter to categories with mixed ownership
mixed_data <- panel_data %>%
  filter(New_cluster_name %in% c("Local", "Market Town and Rural", 
                                 "County and Regional", 
                                 "Major City and Regional"))

# Create pdata.frame
mixed_ownership <- pdata.frame(mixed_data, index = c("domain", "time_period"))

############################################################
# Hierarchical Models M1-M6: Mixed Ownership Sample
############################################################
########################
# GEOGRAPHIC FIDELITY
########################
m1_geo_re <- plm(geographic_fidelity ~ ownership_binary_pinf + time_factor,
                 data = mixed_ownership, model = "random")

m2_geo_re <- plm(geographic_fidelity ~ log_newsroom_dist + time_factor, 
                 data = mixed_ownership, model = "random")

m3_geo_re <- plm(geographic_fidelity ~ ownership_binary_pinf * log_newsroom_dist + 
                   lad_urban_rural + log_articles + covid_rate + time_factor,
                 data = mixed_ownership, model = "random")

# Driscoll-Kraay standard errors
m1_geo_dk <- coeftest(m1_geo_re, vcov = vcovSCC(m1_geo_re, type = "HC1", maxlag = 2))
m2_geo_dk <- coeftest(m2_geo_re, vcov = vcovSCC(m2_geo_re, type = "HC1", maxlag = 2))
m3_geo_dk <- coeftest(m3_geo_re, vcov = vcovSCC(m3_geo_re, type = "HC1", maxlag = 2))

########################
# COVERAGE DISTANCE
########################
m1_dist_re <- plm(log_coverage_dist ~ ownership_binary_pinf + time_factor,
                  data = mixed_ownership, model = "random")

m2_dist_re <- plm(log_coverage_dist ~ log_newsroom_dist + time_factor, 
                  data = mixed_ownership, model = "random")

m3_dist_re <- plm(log_coverage_dist ~ ownership_binary_pinf * log_newsroom_dist + 
                    lad_urban_rural + log_articles + covid_rate + time_factor,
                  data = mixed_ownership, model = "random")

# Driscoll-Kraay standard errors
m1_dist_dk <- coeftest(m1_dist_re, vcov = vcovSCC(m1_dist_re, type = "HC1", maxlag = 2))
m2_dist_dk <- coeftest(m2_dist_re, vcov = vcovSCC(m2_dist_re, type = "HC1", maxlag = 2))
m3_dist_dk <- coeftest(m3_dist_re, vcov = vcovSCC(m3_dist_re, type = "HC1", maxlag = 2))

############################################################
# CONTENT DUPLICATION - Hierarchical Models  [on df with locs -> end of document for full sample]
############################################################
# Create rescaled variable for clarity
pdata$content_dup_1000 <- pdata$syndication_rate * 1000

m1_dup_re <- plm(content_dup_1000 ~ ownership_binary_pinf + time_factor,
                 data = pdata, model = "random")

m2_dup_re <- plm(content_dup_1000 ~ log_newsroom_dist + time_factor, 
                 data = pdata, model = "random")

m3_dup_re <- plm(content_dup_1000 ~ ownership_binary_pinf * log_newsroom_dist + 
                   scope_factor + lad_urban_rural + log_articles + 
                   covid_rate + time_factor, 
                 data = pdata, model = "random")

# Driscoll-Kraay standard errors
m1_dup_dk <- coeftest(m1_dup_re, vcov = vcovSCC(m1_dup_re, type = "HC1", maxlag = 2))
m2_dup_dk <- coeftest(m2_dup_re, vcov = vcovSCC(m2_dup_re, type = "HC1", maxlag = 2))
m3_dup_dk <- coeftest(m3_dup_re, vcov = vcovSCC(m3_dup_re, type = "HC1", maxlag = 2))

############################################################
# APPENDIX REGRESSION TABLES (Full Model Hierarchy)
############################################################
# Geographic Fidelity M1-M3
texreg(
  list(m1_geo_re, m2_geo_re, m3_geo_re),  
  override.se = list(get_dk_se(m1_geo_re), get_dk_se(m2_geo_re), 
                     get_dk_se(m3_geo_re)), 
  override.pvalues = list(m1_geo_dk[, "Pr(>|t|)"], m2_geo_dk[, "Pr(>|t|)"], 
                          m3_geo_dk[, "Pr(>|t|)"]),  
  custom.model.names = c("M1", "M2", "M3"),
  caption = "Geographic Fidelity: Hierarchical Random Effects Models",
  custom.note = "Random effects models with Driscoll-Kraay standard errors. %stars",
  stars = c(0.001, 0.01, 0.05)
)

# Coverage Distance M1-M3
texreg(
  list(m1_dist_re, m2_dist_re, m3_dist_re),  
  override.se = list(get_dk_se(m1_dist_re), get_dk_se(m2_dist_re), 
                     get_dk_se(m3_dist_re)), 
  override.pvalues = list(m1_dist_dk[, "Pr(>|t|)"], m2_dist_dk[, "Pr(>|t|)"], 
                          m3_dist_dk[, "Pr(>|t|)"]),  
  custom.model.names = c("M1", "M2", "M3"),
  caption = "Coverage Distance: Hierarchical Random Effects Models",
  custom.note = "Random effects models with Driscoll-Kraay standard errors. %stars",
  stars = c(0.001, 0.01, 0.05)
)

############################################################
# ROBUSTNESS CHECKS
############################################################
library(car)
library(DescTools)

############################################################
# OPTION 1: Exclude Perfectly Separated Categories
############################################################
## ROBUSTNESS: Pooled OLS + Clustered SEs

# Geographic Fidelity
models_mixed_geo_pooled <- lm(
  geographic_fidelity ~ ownership_binary_pinf * log_newsroom_dist + 
    lad_urban_rural + log_articles + covid_rate + time_factor,
  data = mixed_data
)

mixed_geo_cl <- coeftest(models_mixed_geo_pooled, 
                         vcov = vcovCL(models_mixed_geo_pooled, 
                                       cluster = ~domain))

# Coverage Distance
models_mixed_dist_pooled <- lm(
  log_coverage_dist ~ ownership_binary_pinf * log_newsroom_dist + 
    lad_urban_rural + log_articles + covid_rate + time_factor,
  data = mixed_data
)

mixed_dist_cl <- coeftest(models_mixed_dist_pooled, 
                          vcov = vcovCL(models_mixed_dist_pooled, 
                                        cluster = ~domain))

############################################################
# OPTION 2: Control for Scope (Full Sample)
############################################################

## PRIMARY: Random Effects + Driscoll-Kraay
# (Already estimated as models_scope_geo, models_scope_dist in main analysis)

## ROBUSTNESS: Pooled OLS + Clustered SEs

# Geographic Fidelity
models_scope_geo_pooled <- lm(
  geographic_fidelity ~ ownership_binary_pinf * log_newsroom_dist + 
    scope_factor + lad_urban_rural + log_articles + 
    covid_rate + time_factor,
  data = as.data.frame(pdata)
)

scope_geo_cl <- coeftest(models_scope_geo_pooled, 
                         vcov = vcovCL(models_scope_geo_pooled, 
                                       cluster = ~domain))

# Coverage Distance
models_scope_dist_pooled <- lm(
  log_coverage_dist ~ ownership_binary_pinf * log_newsroom_dist + 
    scope_factor + lad_urban_rural + log_articles + 
    covid_rate + time_factor,
  data = as.data.frame(pdata)
)

scope_dist_cl <- coeftest(models_scope_dist_pooled, 
                          vcov = vcovCL(models_scope_dist_pooled, 
                                        cluster = ~domain))

# Content Duplication
models_scope_dup_pooled <- lm(
  content_dup_1000 ~ ownership_binary_pinf * log_newsroom_dist + 
    scope_factor + lad_urban_rural + log_articles + 
    covid_rate + time_factor,
  data = as.data.frame(pdata)
)

scope_dup_cl <- coeftest(models_scope_dup_pooled, 
                         vcov = vcovCL(models_scope_dup_pooled, 
                                       cluster = ~domain))

############################################################
# Sensitivity: Exclude outliers (>3 SD from mean)
############################################################
mixed_data_clean <- mixed_data %>%
  mutate(
    geo_z = abs(scale(geographic_fidelity)),
    dist_z = abs(scale(log_coverage_dist))
  ) %>%
  filter(geo_z < 3 & dist_z < 3)

# Convert pdata to regular data.frame properly - use panel_data instead
panel_data_clean <- panel_data %>%  # Use the original panel_data object
  mutate(dup_z = abs(scale(content_dup_1000))) %>%
  filter(dup_z < 3)

mixed_clean <- pdata.frame(mixed_data_clean, index = c("domain", "time_period"))
pdata_clean <- pdata.frame(panel_data_clean, index = c("domain", "time_period"))

# Match final specifications
m3_geo_nooutliers <- plm(
  geographic_fidelity ~ ownership_binary_pinf * log_newsroom_dist + 
    lad_urban_rural + log_articles + covid_rate + time_factor, 
  data = mixed_clean, model = "random"
)

m3_dist_nooutliers <- plm(
  log_coverage_dist ~ ownership_binary_pinf * log_newsroom_dist + 
    lad_urban_rural + log_articles + covid_rate + time_factor, 
  data = mixed_clean, model = "random"
)

m3_dup_nooutliers <- plm(
  content_dup_1000 ~ ownership_binary_pinf * log_newsroom_dist + 
    scope_factor + lad_urban_rural + log_articles + covid_rate + time_factor,
  data = pdata_clean, model = "random"
)

############################################################
# Alternative specification: Winsorized DVs at 1st/99th percentile
############################################################
mixed_data_wins <- mixed_data %>%
  mutate(
    geo_wins = pmin(pmax(geographic_fidelity, 
                         quantile(geographic_fidelity, 0.01, na.rm = TRUE)),
                    quantile(geographic_fidelity, 0.99, na.rm = TRUE)),
    dist_wins = pmin(pmax(log_coverage_dist, 
                          quantile(log_coverage_dist, 0.01, na.rm = TRUE)),
                     quantile(log_coverage_dist, 0.99, na.rm = TRUE))
  )

# Use original panel_data object instead of pdata
panel_data_wins <- panel_data %>%  # Use the original panel_data object
  mutate(dup_wins = pmin(pmax(content_dup_1000, 
                              quantile(content_dup_1000, 0.01, na.rm = TRUE)),
                         quantile(content_dup_1000, 0.99, na.rm = TRUE)))

mixed_wins <- pdata.frame(mixed_data_wins, index = c("domain", "time_period"))
pdata_wins <- pdata.frame(panel_data_wins, index = c("domain", "time_period"))

m3_geo_wins <- plm(
  geo_wins ~ ownership_binary_pinf * log_newsroom_dist + 
    lad_urban_rural + log_articles + covid_rate + time_factor, 
  data = mixed_wins, model = "random"
)

m3_dist_wins <- plm(
  dist_wins ~ ownership_binary_pinf * log_newsroom_dist + 
    lad_urban_rural + log_articles + covid_rate + time_factor, 
  data = mixed_wins, model = "random"
)

m3_dup_wins <- plm(
  dup_wins ~ ownership_binary_pinf * log_newsroom_dist + scope_factor + 
    lad_urban_rural + log_articles + covid_rate + time_factor,
  data = pdata_wins, model = "random"
)

################################################################################
# ROBUSTNESS CHECKS: COMPREHENSIVE COMPARISON
################################################################################

## Extract coefficients from main models (M3 full specifications)

# Mixed ownership sample (Strategy 1)
mixed_geo_coef <- coef(m3_geo_re)["ownership_binary_pinf"]
mixed_geo_se <- get_dk_se(m3_geo_re)["ownership_binary_pinf"]
mixed_geo_p <- m3_geo_dk["ownership_binary_pinf", "Pr(>|t|)"]

mixed_dist_coef <- coef(m3_dist_re)["ownership_binary_pinf"]
mixed_dist_se <- get_dk_se(m3_dist_re)["ownership_binary_pinf"]
mixed_dist_p <- m3_dist_dk["ownership_binary_pinf", "Pr(>|t|)"]

# Full sample with scope controls (Strategy 2)
scope_dup_coef <- coef(m3_dup_re)["ownership_binary_pinf"]
scope_dup_se <- get_dk_se(m3_dup_re)["ownership_binary_pinf"]
scope_dup_p <- m3_dup_dk["ownership_binary_pinf", "Pr(>|t|)"]

## Alternative estimators for robustness

# Pooled OLS with clustered standard errors
pooled_geo_coef <- coef(models_mixed_geo_pooled)["ownership_binary_pinf"]
pooled_geo_se <- mixed_geo_cl["ownership_binary_pinf", "Std. Error"]
pooled_geo_p <- mixed_geo_cl["ownership_binary_pinf", "Pr(>|t|)"]

pooled_dist_coef <- coef(models_mixed_dist_pooled)["ownership_binary_pinf"]
pooled_dist_se <- mixed_dist_cl["ownership_binary_pinf", "Std. Error"]
pooled_dist_p <- mixed_dist_cl["ownership_binary_pinf", "Pr(>|t|)"]

pooled_dup_coef <- coef(models_scope_dup_pooled)["ownership_binary_pinf"]
pooled_dup_se <- scope_dup_cl["ownership_binary_pinf", "Std. Error"]
pooled_dup_p <- scope_dup_cl["ownership_binary_pinf", "Pr(>|t|)"]

## Outlier treatment robustness
outlier_geo_coef <- coef(m3_geo_nooutliers)["ownership_binary_pinf"]
outlier_geo_se <- get_dk_se(m3_geo_nooutliers)["ownership_binary_pinf"]
outlier_geo_dk <- coeftest(m3_geo_nooutliers, 
                           vcov = vcovSCC(m3_geo_nooutliers, type = "HC1", maxlag = 2))
outlier_geo_p <- outlier_geo_dk["ownership_binary_pinf", "Pr(>|t|)"]

outlier_dist_coef <- coef(m3_dist_nooutliers)["ownership_binary_pinf"]
outlier_dist_se <- get_dk_se(m3_dist_nooutliers)["ownership_binary_pinf"]
outlier_dist_dk <- coeftest(m3_dist_nooutliers, 
                            vcov = vcovSCC(m3_dist_nooutliers, type = "HC1", maxlag = 2))
outlier_dist_p <- outlier_dist_dk["ownership_binary_pinf", "Pr(>|t|)"]

outlier_dup_coef <- coef(m3_dup_nooutliers)["ownership_binary_pinf"]
outlier_dup_se <- get_dk_se(m3_dup_nooutliers)["ownership_binary_pinf"]
outlier_dup_dk <- coeftest(m3_dup_nooutliers, 
                           vcov = vcovSCC(m3_dup_nooutliers, type = "HC1", maxlag = 2))
outlier_dup_p <- outlier_dup_dk["ownership_binary_pinf", "Pr(>|t|)"]

## Winsorized DVs
wins_geo_coef <- coef(m3_geo_wins)["ownership_binary_pinf"]
wins_geo_se <- get_dk_se(m3_geo_wins)["ownership_binary_pinf"]
wins_geo_dk <- coeftest(m3_geo_wins, 
                        vcov = vcovSCC(m3_geo_wins, type = "HC1", maxlag = 2))
wins_geo_p <- wins_geo_dk["ownership_binary_pinf", "Pr(>|t|)"]

wins_dist_coef <- coef(m3_dist_wins)["ownership_binary_pinf"]
wins_dist_se <- get_dk_se(m3_dist_wins)["ownership_binary_pinf"]
wins_dist_dk <- coeftest(m3_dist_wins, 
                         vcov = vcovSCC(m3_dist_wins, type = "HC1", maxlag = 2))
wins_dist_p <- wins_dist_dk["ownership_binary_pinf", "Pr(>|t|)"]

wins_dup_coef <- coef(m3_dup_wins)["ownership_binary_pinf"]
wins_dup_se <- get_dk_se(m3_dup_wins)["ownership_binary_pinf"]
wins_dup_dk <- coeftest(m3_dup_wins, 
                        vcov = vcovSCC(m3_dup_wins, type = "HC1", maxlag = 2))
wins_dup_p <- wins_dup_dk["ownership_binary_pinf", "Pr(>|t|)"]

################################################################################
# COMPREHENSIVE COMPARISON TABLE
################################################################################

comparison_all <- data.frame(
  Approach = c(
    "Main: Mixed Sample (RE+DK)",
    "Alternative Estimator: Pooled OLS (CL)",
    "Outlier Treatment: Exclude >3SD",
    "Outlier Treatment: Winsorized (1st/99th)"
  ),
  
  # Geographic Fidelity
  GeoFid_Coef = c(mixed_geo_coef, pooled_geo_coef, outlier_geo_coef, wins_geo_coef),
  GeoFid_SE = c(mixed_geo_se, pooled_geo_se, outlier_geo_se, wins_geo_se),
  GeoFid_p = c(mixed_geo_p, pooled_geo_p, outlier_geo_p, wins_geo_p),
  
  # Coverage Distance
  CovDist_Coef = c(mixed_dist_coef, pooled_dist_coef, outlier_dist_coef, wins_dist_coef),
  CovDist_SE = c(mixed_dist_se, pooled_dist_se, outlier_dist_se, wins_dist_se),
  CovDist_p = c(mixed_dist_p, pooled_dist_p, outlier_dist_p, wins_dist_p),
  
  # Content Duplication
  ContDup_Coef = c(scope_dup_coef, pooled_dup_coef, outlier_dup_coef, wins_dup_coef),
  ContDup_SE = c(scope_dup_se, pooled_dup_se, outlier_dup_se, wins_dup_se),
  ContDup_p = c(scope_dup_p, pooled_dup_p, outlier_dup_p, wins_dup_p)
)

## Format results with stars and standard errors
comparison_formatted <- comparison_all %>%
  mutate(
    GeoFid_Result = sprintf("%.3f (%.3f)%s", 
                           GeoFid_Coef, GeoFid_SE,
                           ifelse(GeoFid_p < 0.001, "***",
                                  ifelse(GeoFid_p < 0.01, "**",
                                         ifelse(GeoFid_p < 0.05, "*", "")))),
    CovDist_Result = sprintf("%.3f (%.3f)%s", 
                            CovDist_Coef, CovDist_SE,
                            ifelse(CovDist_p < 0.001, "***",
                                   ifelse(CovDist_p < 0.01, "**",
                                          ifelse(CovDist_p < 0.05, "*", "")))),
    ContDup_Result = sprintf("%.2f (%.2f)%s", 
                            ContDup_Coef, ContDup_SE,
                            ifelse(ContDup_p < 0.001, "***",
                                   ifelse(ContDup_p < 0.01, "**",
                                          ifelse(ContDup_p < 0.05, "*", ""))))
  ) %>%
  select(Approach, GeoFid_Result, CovDist_Result, ContDup_Result)

print(comparison_formatted)

## Export to LaTeX
library(xtable)
print(xtable(comparison_formatted,
             caption = "Robustness Checks: Corporate Ownership Effects Across Specifications",
             label = "tab:robustness",
             align = c("l", "l", "c", "c", "c")),
      include.rownames = FALSE,
      sanitize.text.function = function(x) x)

############################################################
# Comparison table: Main vs. Robustness checks (Simple Version)
############################################################
robustness_comparison <- data.frame(
  Specification = c("Main Models", "Exclude Outliers (>3SD)", "Winsorized (1st/99th)"),
  Geo_Coef = c(
    coef(m3_geo_re)["ownership_binary_pinf"],
    coef(m3_geo_nooutliers)["ownership_binary_pinf"],
    coef(m3_geo_wins)["ownership_binary_pinf"]
  ),
  Dist_Coef = c(
    coef(m3_dist_re)["ownership_binary_pinf"],
    coef(m3_dist_nooutliers)["ownership_binary_pinf"],
    coef(m3_dist_wins)["ownership_binary_pinf"]
  ),
  Dup_Coef = c(
    coef(m3_dup_re)["ownership_binary_pinf"],
    coef(m3_dup_nooutliers)["ownership_binary_pinf"],
    coef(m3_dup_wins)["ownership_binary_pinf"]
  )
)

print(robustness_comparison)

##################################################  
# CONTENT DUPLICATION
##################################################
# for content syndication we add back in articles which do not contain locations
article_sample <- read_csv('../6_geographic-local-media-classifier/replication_data_geo_newsmapper/articles_sample.csv.zip')
articles <- read_rds('data/article_level_analysis_data.rds')

# Recalculate syndication_rate using full article sample
articles_no_locs <- article_sample %>%
  select(year, quarter, unique_article_id, domain, duplicate_group) %>%
  anti_join(articles %>% select(unique_article_id)) %>%
  mutate(is_duplicate = !is.na(duplicate_group) & duplicate_group != "") %>%
  mutate(has_local_location = NA) %>%
  select(year, quarter, unique_article_id, domain, is_duplicate, has_local_location)

common_cols <- intersect(names(articles), names(articles_no_locs))

all_articles <- bind_rows(articles[common_cols], articles_no_locs[common_cols]) %>%
  mutate(year_quarter = paste(year, quarter, sep = "-"))

# Step 1: Recalculate correct syndication_rate at domain-quarter level
corrected_synd <- all_articles %>%
  mutate(time_period = paste(year, "Q", quarter, sep = ""),
         time_factor = as.factor(time_period)) %>%
  group_by(domain, time_factor) %>%
  summarise(
    syndication_rate = mean(is_duplicate, na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: Merge corrected rates into panel data
panel_data <- panel_data %>%
  select(-syndication_rate) %>%
  left_join(corrected_synd, by = c("domain", "time_factor"))

pdata <- pdata.frame(panel_data, index = c("domain", "time_period"))

# Create scope factor with ordered levels
pdata$scope_factor <- factor(pdata$New_cluster_name, 
                             levels = c("Hyperlocal", "Local", 
                                        "Market Town and Rural",
                                        "County and Regional", 
                                        "Major City and Regional",
                                        "Major Regional Daily"))

# Create rescaled variable for clarity
pdata$content_dup_1000 <- pdata$syndication_rate * 1000

m1_dup_re <- plm(content_dup_1000 ~ ownership_binary_pinf + time_factor,
                 data = pdata, model = "random")

m2_dup_re <- plm(content_dup_1000 ~ log_newsroom_dist + time_factor, 
                 data = pdata, model = "random")

m3_dup_re <- plm(content_dup_1000 ~ ownership_binary_pinf * log_newsroom_dist + 
                   scope_factor + lad_urban_rural + log_articles + 
                   covid_rate + time_factor, 
                 data = pdata, model = "random")

# Driscoll-Kraay standard errors
m1_dup_dk <- coeftest(m1_dup_re, vcov = vcovSCC(m1_dup_re, type = "HC1", maxlag = 2))
m2_dup_dk <- coeftest(m2_dup_re, vcov = vcovSCC(m2_dup_re, type = "HC1", maxlag = 2))
m3_dup_dk <- coeftest(m3_dup_re, vcov = vcovSCC(m3_dup_re, type = "HC1", maxlag = 2))

# Content Duplication M1-M5
texreg(
  list(m1_dup_re, m2_dup_re, m3_dup_re),  
  override.se = list(get_dk_se(m1_dup_re), get_dk_se(m2_dup_re), get_dk_se(m3_dup_re)), 
  override.pvalues = list(m1_dup_dk[, "Pr(>|t|)"], m2_dup_dk[, "Pr(>|t|)"], 
                          m3_dup_dk[, "Pr(>|t|)"]),  
  custom.model.names = c("M1", "M2", "M3"),
  caption = "Content Duplication: Hierarchical Random Effects Models",
  custom.note = "Random effects models with Driscoll-Kraay standard errors. %stars",
  stars = c(0.001, 0.01, 0.05)
)

############################################################
# MAIN RESULTS TABLE: All DVs (Full Specification Models)
############################################################
coef_map <- list(
  "(Intercept)" = "(Intercept)",
  "ownership_binary_pinf" = "Corporate Ownership",
  "log_newsroom_dist" = "Newsroom Distance (log km)",
  "lad_urban_ruralUrban with rural areas" = "Urban with rural areas",
  "lad_urban_ruralRural" = "Rural",
  "lad_urban_ruralSparse and rural" = "Sparse and rural",
  "log_articles" = "Articles (log N)",
  "covid_rate" = "COVID-19 Rate (per 100k)",
  "scope_factorLocal" = "Scope: Local",
  "scope_factorMarket Town and Rural" = "Scope: Market Town and Rural",
  "scope_factorCounty and Regional" = "Scope: County and Regional",
  "scope_factorMajor City and Regional" = "Scope: Major City and Regional",
  "scope_factorMajor Regional Daily" = "Scope: Major Regional Daily",
  "ownership_binary_pinf:log_newsroom_dist" = "Corporate × Newsroom Dist"
)

# Main results table using full specification models
texreg(
  list(m3_geo_re, m3_dist_re, m3_dup_re),
  override.se = list(
    get_dk_se(m3_geo_re),
    get_dk_se(m3_dist_re),
    get_dk_se(m3_dup_re)
  ),
  override.pvalues = list(
    m3_geo_dk[, "Pr(>|t|)"],
    m3_dist_dk[, "Pr(>|t|)"],
    m3_dup_dk[, "Pr(>|t|)"]
  ),
  omit.coef = "time_factor",
  custom.coef.map = coef_map,
  custom.model.names = c("Geographic Fidelity", "Coverage Distance (log)", "Content Duplication"),
  caption = "Effects of Corporate Ownership on Local News Authenticity",
  custom.note = "Random effects models with Driscoll-Kraay standard errors. Coverage Distance includes ownership × newsroom distance interaction. Quarter fixed effects omitted. %stars",
  stars = c(0.001, 0.01, 0.05)
)

# Calculate implied increase for coverage distance
mean_log_dist <- mean(mixed_ownership$log_coverage_dist, na.rm = TRUE)
mean_dist <- exp(mean_log_dist)

# Get coefficient from M5 (with interaction)
coef_ownership <- coef(m3_dist_re)["ownership_binary_pinf"]
increase_km <- mean_dist * (exp(coef_ownership) - 1)
cat("Implied increase from corporate ownership:", round(increase_km, 2), "km\n")



################################################################################
# SUBSTANTIVE EFFECTS CALCULATIONS
################################################################################

## 1. Coverage Distance back-transformation ----
mean_log_dist <- mean(mixed_ownership$log_coverage_dist, na.rm = TRUE)
mean_dist_km <- exp(mean_log_dist)

# Corporate outlet mean distance
corporate_dist_km <- exp(mean_log_dist + 0.65)

# Difference
dist_difference <- corporate_dist_km - mean_dist_km

# Percentage increase
dist_pct_increase <- (exp(0.65) - 1) * 100

cat("\n=== Coverage Distance Effects ===\n")
cat("Mean coverage distance (km):", round(mean_dist_km, 2), "\n")
cat("Corporate outlet distance (km):", round(corporate_dist_km, 2), "\n")
cat("Difference (km):", round(dist_difference, 2), "\n")
cat("Percentage increase:", round(dist_pct_increase, 1), "%\n")


## 2. Newsroom Distance effect on Coverage Distance (doubling) ----
newsroom_double_effect <- (exp(0.17 * log(2)) - 1) * 100

cat("\n=== Newsroom Distance Effects ===\n")
cat("Effect of doubling newsroom distance on coverage distance:", 
    round(newsroom_double_effect, 1), "%\n")


## 3. Geographic Fidelity substantive effects ----
mean_geo_fid <- mean(mixed_ownership$geographic_fidelity, na.rm = TRUE)
corporate_geo_fid <- mean_geo_fid - 0.18

# As percentage points
geo_fid_pp_drop <- 0.18

cat("\n=== Geographic Fidelity Effects ===\n")
cat("Mean geographic fidelity:", round(mean_geo_fid, 3), "\n")
cat("Corporate outlet fidelity:", round(corporate_geo_fid, 3), "\n")
cat("Drop (percentage points):", round(geo_fid_pp_drop, 3), "\n")


## 4. Typical article counts ----
median_log_articles <- median(panel_data$log_articles, na.rm = TRUE)
median_articles <- exp(median_log_articles)

# Also get mean for comparison
mean_log_articles <- mean(panel_data$log_articles, na.rm = TRUE)
mean_articles <- exp(mean_log_articles)

cat("\n=== Article Volume ===\n")
cat("Median articles per quarter:", round(median_articles, 0), "\n")
cat("Mean articles per quarter:", round(mean_articles, 0), "\n")


## 5. Content Duplication substantive effects ----
# Remember: coefficient is on 0-1000 scale, so 56.68 per 1000 articles
# This is 0.05668 on proportion scale (0-1)

dup_rate_increase <- 56.68 / 1000  # Convert to proportion

# For median outlet
dup_articles_median <- median_articles * dup_rate_increase

# For mean outlet
dup_articles_mean <- mean_articles * dup_rate_increase

# As percentage of total output
dup_pct_of_output <- dup_rate_increase * 100

cat("\n=== Content Duplication Effects ===\n")
cat("Duplication rate increase (proportion):", round(dup_rate_increase, 4), "\n")
cat("Additional duplicated articles (median outlet):", round(dup_articles_median, 1), "\n")
cat("Additional duplicated articles (mean outlet):", round(dup_articles_mean, 1), "\n")
cat("As % of total output:", round(dup_pct_of_output, 2), "%\n")


## 6. Interaction effect: Corporate × Newsroom Distance ----
# For coverage distance model
interaction_coef <- -0.13

# Effect of doubling newsroom distance on corporate ownership effect
interaction_reduction <- (exp(interaction_coef * log(2)) - 1) * 100

cat("\n=== Interaction Effects ===\n")
cat("Reduction in corporate effect per doubling of newsroom distance:", 
    round(abs(interaction_reduction), 1), "%\n")


## 7. Create summary table ----
effects_summary <- data.frame(
  Effect = c(
    "Corporate ownership → Geographic Fidelity",
    "Corporate ownership → Coverage Distance (km)",
    "Corporate ownership → Coverage Distance (%)",
    "Corporate ownership → Content Duplication (articles)",
    "Corporate ownership → Content Duplication (%)",
    "Newsroom distance doubling → Coverage Distance (%)",
    "Interaction: Newsroom distance reduces corporate effect (%)"
  ),
  Estimate = c(
    round(-geo_fid_pp_drop, 3),
    round(dist_difference, 2),
    round(dist_pct_increase, 1),
    round(dup_articles_median, 1),
    round(dup_pct_of_output, 2),
    round(newsroom_double_effect, 1),
    round(abs(interaction_reduction), 1)
  )
)

cat("\n=== SUMMARY OF SUBSTANTIVE EFFECTS ===\n")
print(effects_summary)