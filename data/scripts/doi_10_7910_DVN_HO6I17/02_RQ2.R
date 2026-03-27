############################################################
# RQ2: Does content duplication substitute for local coverage?
############################################################
library(tidyverse)
library(scales)
library(plm)
library(lmtest)
library(sandwich)
library(texreg)

###############
# LOAD DATA
###############
article_sample <- read_csv('../6_geographic-local-media-classifier/replication_data_geo_newsmapper/articles_sample.csv.zip')
outlets <- read_rds('data/outlets.rds')
articles <- read_rds('data/article_level_analysis_data.rds')
panel_data <- read_rds('data/panel_data.rds')

###############
# PREPARE FULL ARTICLE DATASET (including NoLoc)
###############
articles_no_locs <- article_sample |> 
  select(year, quarter, unique_article_id, domain, duplicate_group) |> 
  anti_join(articles |> select(unique_article_id)) |> 
  mutate(
    is_duplicate = !is.na(duplicate_group) & duplicate_group != "",
    has_local_location = NA
  ) |> 
  select(-duplicate_group) |> 
  left_join(outlets |> select(domain, OWNER_PINF, ownership_binary_pinf))

# Combine articles with and without locations
common_cols <- intersect(names(articles), names(articles_no_locs))
all <- bind_rows(articles[common_cols], articles_no_locs[common_cols]) %>%
  mutate(
    time_period = paste0(year, "_", quarter),
    year_quarter_date = as.Date(paste0(year, "-", (quarter - 1) * 3 + 1, "-01"))
  )

###############
# DESCRIPTIVE PLOTS
###############

## Panel A: Composition by duplication status
plot_a <- all %>%
  mutate(
    dupl = if_else(is_duplicate, "Duplicate", "Original"),
    is_local = case_when(
      is.na(has_local_location) ~ "NoLoc",
      has_local_location == FALSE ~ "NonLocal",
      has_local_location == TRUE ~ "Local"
    ),
    is_local = factor(is_local, levels = c("NoLoc", "NonLocal", "Local"))
  ) %>%
  count(dupl, is_local) %>%
  ggplot(aes(x = dupl, y = n / 1000, fill = is_local)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.5) +
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 400, 200)) +
  labs(x = NULL, y = "Number of Articles (thousands)") +
  scale_fill_manual(
    values = c("Local" = "darkgreen", "NonLocal" = "purple", "NoLoc" = "mediumpurple"),
    name = "Is Local", guide = guide_legend(reverse = TRUE)
  ) +
  coord_flip() +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.y = element_blank(),
    axis.text = element_text(size = 10),
    legend.position = "top",
    plot.margin = margin(5, 15, 5, 5))

## Panel B: Local share over time (excludes NoLoc)
plot_b <- all %>%
  filter(!is.na(has_local_location)) %>%
  group_by(year_quarter_date, is_duplicate) %>%
  summarise(local_share = mean(has_local_location), .groups = "drop") %>%
  ggplot(aes(year_quarter_date, local_share, color = is_duplicate)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.4) +
  scale_color_manual(
    values = c("FALSE" = "#2E7D32", "TRUE" = "#7B1FA2"), 
    labels = c("Original", "Duplicate"),
    name = NULL
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(y = "Local", x = "Year") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 5, 5, 15)
  )

## Panel B: Local share over time (excludes NoLoc)
plot_b <- all %>%
  filter(!is.na(has_local_location)) %>%
  group_by(year_quarter_date, is_duplicate) %>%
  summarise(local_share = mean(has_local_location), .groups = "drop") %>%
  ggplot(aes(year_quarter_date, local_share, 
             color = is_duplicate, 
             linetype = is_duplicate)) +   # <-- add linetype aesthetic
  geom_smooth(se = FALSE, method = "loess", span = 0.4) +
  scale_color_manual(
    values = c("FALSE" = "#2E7D32", "TRUE" = "#2E7D32"),  # both green
    labels = c("Original", "Duplicate"),
    name = NULL
  ) +
  scale_linetype_manual(
    values = c("FALSE" = "solid", "TRUE" = "dashed")  # dashed for duplicates
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(y = "Local", x = "Year") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 5, 5, 15)
  )

## Combine and save
fig3 <- ggpubr::ggarrange(plot_a, plot_b, ncol = 2, labels = c('A', 'B'))
ggsave("img/figure3.pdf", fig3, width = 22, height = 4.5, dpi = 600, units = "cm")

###############
# PANEL MODELS WITH OUTLET SCOPE CONTROLS
###############
panel_volume <- all %>%
  filter(!is.na(has_local_location)) |> 
   group_by(domain, year, quarter) %>%
   summarise(
     total_articles = n(),
     local_articles = sum(has_local_location == TRUE),
     syndication_rate = mean(is_duplicate),
     .groups = "drop"
   ) %>%
   mutate(
     time_period = paste0(year, "Q", quarter),  # Changed from year, "_", quarter
     log_total_articles = log(total_articles + 1),
     log_local_articles = log(local_articles + 1)) %>%
  inner_join(
     panel_data %>% select(domain, time_period, ownership_binary_pinf, 
                          log_newsroom_dist, log_area, lad_urban_rural, 
                          covid_rate, time_factor),
     by = c("domain", "time_period")
   ) %>%
  left_join(outlets %>% select(domain, New_cluster_name), by = "domain")

panel_volume$scope_factor <- factor(panel_volume$New_cluster_name, 
                             levels = c("Hyperlocal", "Local", 
                                       "Market Town and Rural",
                                       "County and Regional", 
                                       "Major City and Regional",
                                       "Major Regional Daily"))

# Re-create panel data frames
pdata_volume <- pdata.frame(panel_volume, index = c("domain", "time_period"))
pdata <- pdata.frame(panel_data, index = c("domain", "time_period"))
pdata$scope_factor <- factor(pdata$New_cluster_name, 
                             levels = c("Hyperlocal", "Local", 
                                       "Market Town and Rural",
                                       "County and Regional", 
                                       "Major City and Regional",
                                       "Major Regional Daily"))
# Add scope to models
m_share_scope <- plm(
  log_local_articles ~ syndication_rate + ownership_binary_pinf + 
    scope_factor +  # Scope control
    lad_urban_rural + 
    log_total_articles + 
    time_factor,
  data = pdata_volume,
  model = "random"
)

m_fid_scope <- plm(
  geographic_fidelity ~ syndication_rate + ownership_binary_pinf + 
    scope_factor +  # Scope control
    lad_urban_rural + 
    log_articles + 
    time_factor,
  data = pdata,
  model = "random"
)

m_dist_scope <- plm(
  log_coverage_dist ~ syndication_rate + ownership_binary_pinf + 
    scope_factor +  # Scope control
    lad_urban_rural + 
    log_articles + 
    time_factor,
  data = pdata,
  model = "random"
)

# Check results with DK standard errors
coeftest(m_share_scope, vcov = vcovSCC(m_share_scope, type = "HC1", maxlag = 2))
coeftest(m_fid_scope, vcov = vcovSCC(m_fid_scope, type = "HC1", maxlag = 2))
coeftest(m_dist_scope, vcov = vcovSCC(m_dist_scope, type = "HC1", maxlag = 2))

## Get Driscoll-Kraay SEs
get_dk_se <- function(model) {
  sqrt(diag(vcovSCC(model, type = "HC1", maxlag = 2)))
}

# Create final regression table with scope controls
texreg(
  list(m_share_scope, m_fid_scope, m_dist_scope),
  override.se = list(
    get_dk_se(m_share_scope),
    get_dk_se(m_fid_scope), 
    get_dk_se(m_dist_scope)
  ),
  override.pvalues = list(
    coeftest(m_share_scope, vcov = vcovSCC(m_share_scope, type = "HC1", maxlag = 2))[, "Pr(>|t|)"],
    coeftest(m_fid_scope, vcov = vcovSCC(m_fid_scope, type = "HC1", maxlag = 2))[, "Pr(>|t|)"],
    coeftest(m_dist_scope, vcov = vcovSCC(m_dist_scope, type = "HC1", maxlag = 2))[, "Pr(>|t|)"]
  ),
  omit.coef = "time_factor|lad_urban_rural|scope_factor",
  custom.coef.map = list(
    "(Intercept)" = "Constant",
    "syndication_rate" = "Content Duplication",
    "ownership_binary_pinf" = "Corporate Ownership",
    "log_total_articles" = "Total Articles (log N)",
    "log_articles" = "Total Articles (log N)"
  ),
  custom.model.names = c("Local Articles (log)", 
                         "Geographic\nFidelity", 
                         "Coverage\nDistance (log)"),
  custom.note = "Random effects panel models with Driscoll-Kraay standard errors. Outlet scope, urban/rural classification, and quarter fixed effects omitted for brevity. %stars"
)

# Create final regression table with scope controls
texreg(
  list(m_share_scope, m_fid_scope, m_dist_scope),
  override.se = list(
    get_dk_se(m_share_scope),
    get_dk_se(m_fid_scope), 
    get_dk_se(m_dist_scope)
  ),
  override.pvalues = list(
    coeftest(m_share_scope, vcov = vcovSCC(m_share_scope, type = "HC1", maxlag = 2))[, "Pr(>|t|)"],
    coeftest(m_fid_scope, vcov = vcovSCC(m_fid_scope, type = "HC1", maxlag = 2))[, "Pr(>|t|)"],
    coeftest(m_dist_scope, vcov = vcovSCC(m_dist_scope, type = "HC1", maxlag = 2))[, "Pr(>|t|)"]
  ),
  custom.model.names = c("Local Articles (log)", 
                         "Geographic\nFidelity", 
                         "Coverage\nDistance (log)"),
  custom.note = "Random effects panel models with Driscoll-Kraay standard errors. Outlet scope, urban/rural classification, and quarter fixed effects omitted for brevity. %stars"
)

# ############################################################
# # Calculate marginal effects at different ownership types
# ############################################################
# # For interpretation: effect of duplication by ownership type
# # Independent outlets: β_duplication
# # Corporate outlets: β_duplication + β_interaction

# # Geographic Fidelity
# beta_dup_fid <- coef(m_dup_fid_int)["syndication_rate"]
# beta_int_fid <- coef(m_dup_fid_int)["syndication_rate:ownership_binary_pinf"]

# cat("\nGeographic Fidelity:\n")
# cat("Effect for independent outlets:", round(beta_dup_fid, 4), "\n")
# cat("Effect for corporate outlets:", round(beta_dup_fid + beta_int_fid, 4), "\n")
# cat("Difference (interaction):", round(beta_int_fid, 4), "\n")

# # Coverage Distance
# beta_dup_dist <- coef(m_dup_dist_int)["syndication_rate"]
# beta_int_dist <- coef(m_dup_dist_int)["syndication_rate:ownership_binary_pinf"]

# cat("\nCoverage Distance:\n")
# cat("Effect for independent outlets:", round(beta_dup_dist, 4), "\n")
# cat("Effect for corporate outlets:", round(beta_dup_dist + beta_int_dist, 4), "\n")
# cat("Difference (interaction):", round(beta_int_dist, 4), "\n")



