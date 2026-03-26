##########################################
# Descrptive Analysis RQ1
##########################################
library(tidyverse)
library(plm)         # panel models (FE/RE)
library(lmtest)      # coeftest
library(sandwich)    # vcovHC
library(geosphere)
library(googlesheets4)
library(texreg)
library(ggh4x)
library(ggcorrplot)
library(ggpubr)
gs4_deauth()

panel_data <- read_rds('data/panel_data.rds')
outlets <- read_rds('data/outlets.rds')
table(outlets$New_cluster_name)

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

# Aggregate correct syndication_rate
synd_trends <- all_articles %>%
  separate(year_quarter, into = c("year", "quarter"), sep = "-", convert = TRUE) %>%
  left_join(outlets %>% select(domain, ownership_binary_pinf), by = "domain") %>%
  mutate(ownership_type = ifelse(ownership_binary_pinf == 1, "Consolidated", "Independent")) %>%
  group_by(year, quarter, ownership_type) %>%
  summarise(value = mean(is_duplicate, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    time_period = year + (quarter - 1) / 4,
    metric = "syndication_rate"
  )

#############################
# outlet-level table
#############################

# Step 1: Compute outlet-level duplication rates
# Assuming "is_duplicate" is a logical column (TRUE/FALSE)
duplication_by_outlet <- all_articles %>%
  group_by(domain) %>%
  summarise(
    n_articles = n(),
    dup_rate = mean(is_duplicate, na.rm = TRUE)
  )

# Step 2: Summarise duplication rate across outlets
duplication_summary <- duplication_by_outlet %>%
  summarise(
    mean_duplication = mean(dup_rate, na.rm = TRUE),
    median_duplication = median(dup_rate, na.rm = TRUE),
    sd_duplication = sd(dup_rate, na.rm = TRUE),
    min_duplication = min(dup_rate, na.rm = TRUE),
    max_duplication = max(dup_rate, na.rm = TRUE),
    n_outlets = n()
  )

duplication_summary

# ############################
# # ownership variables table
# ############################
# # Step 1: Deduplicate to owner level
# owner_level_data <- panel_data %>%
#   distinct(OWNER_PINF, .keep_all = TRUE) %>%
#   select(
#     OWNER_PINF,
#     outlets_in_group,
#     non_local_registered_address,
#     ownership_binary_pinf
#   )

# table(outlets$ownership_binary_pinf, outlets$non_local_registered_address)
# table(owner_level_data$ownership_binary_pinf, owner_level_data$outlets_in_group)

# # Newsroom distance by ownership
# panel_data %>%
#   group_by(ownership_binary_pinf) %>%
#   summarise(
#     median_distance = median(newsroom_distance_km, na.rm = TRUE),
#     mean_distance = mean(newsroom_distance_km, na.rm = TRUE),
#     mean_log_distance = mean(log_newsroom_dist, na.rm = TRUE),
#     n = n_distinct(domain)
#   )

# # Statistical test
# t.test(log_newsroom_dist ~ ownership_binary_pinf, data = panel_data)

##########################################
# Figure Code
##########################################
# Define consistent ordering and labels
metric_order <- c("geographic_fidelity", "coverage_distance", "syndication_rate")
metric_labels <- c(
  geographic_fidelity = "Geographic Fidelity",
  coverage_distance = "Coverage Distance",
  syndication_rate = "Content Duplication"
)

scope_order <- c("Hyperlocal", "Local", "Market Town and Rural", 
                 "County and Regional", "Major City and Regional", 
                 "Major Regional Daily")

# Color palettes
scope_pal <- c('#f1eef6', '#d0d1e6', '#a6bddb', '#74a9cf', '#2b8cbe', '#045a8d')
ownership_colors <- c("Independent" = "#798E87", "Corporate" = "#CCC591")

# Shared theme
theme_paper <- function() {
  theme_minimal() +
    theme(
      legend.position = "right",
      legend.key.size = unit(0.4, "cm"),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9),
      legend.margin = margin(0, 0, 0, 0),
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = 9, face = "bold"),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 9)
    )
}

#####################################################################
# Panel A: Trends by Ownership Type
#####################################################################

# Replace old syndication values in time_trends
time_trends <- readRDS('data/time_trends.rds') %>%
  filter(metric != "syndication_rate") %>%
  bind_rows(synd_trends) %>% 
  mutate(metric = factor(metric,
    levels = c("geographic_fidelity", "median_distance_km", "syndication_rate"),
    labels = c("Geographic Fidelity", "Coverage Distance", "Content Duplication")))

# Plot updated trends
library(scales)
panel_a <- ggplot(time_trends, aes(x = time_period, y = value, linetype = ownership_type)) +
  geom_smooth(method = "loess", span = 0.75, se = FALSE, linewidth = 0.9, color = "black") +
  scale_linetype_manual(values = c("solid", "dashed"), name = "Ownership") +
  facet_wrap2(~ metric, scales = "free_y", ncol = 3) +
  facetted_pos_scales(
    y = list(
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)),
      scale_y_continuous(limits = c(0,50)),
      scale_y_continuous(limits = c(0, 0.25), breaks = seq(0, 1, 0.10))
    )
  ) +
  scale_x_continuous(breaks = c(2020, 2021, 2022), labels = c("2020", "2021", "2022")) +
  labs(y = NULL, x = NULL) +
  theme_paper() +
  theme(legend.key.width = unit(1, "cm"),
legend.position = 'top',
)

#####################################################################
# Panel B: Distribution by Newsroom Distance
#####################################################################

# 1. Prepare per-domain content duplication using full article set
dup_by_domain <- all_articles %>%
  group_by(domain) %>%
  summarise(syndication_rate = mean(is_duplicate, na.rm = TRUE), .groups = "drop")

# 2. Merge into outlet-level data with spatial metrics
outlets_long <- outlets %>%
  select(domain, geographic_fidelity, median_distance_km, newsroom_distance_km) %>%
  left_join(dup_by_domain, by = "domain") %>%
  rename(coverage_distance = median_distance_km,
         content_duplication = syndication_rate) %>%
  pivot_longer(
    cols = c(geographic_fidelity, coverage_distance, content_duplication),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = factor(metric,
      levels = c("geographic_fidelity", "coverage_distance", "content_duplication"),
      labels = c("Geographic Fidelity", "Coverage Distance", "Content Duplication")),
    distance_cat = cut(newsroom_distance_km,
      breaks = c(0, 5, 20, 50, 600),
      labels = c("<5", "5–20", "20–50", ">50"))
  )

# 3. Plot: Panel B – by newsroom distance
panel_b <- ggplot(outlets_long, aes(x = distance_cat, y = value)) +
  geom_boxplot(outlier.size = 0.5, fill = 'grey85', color = 'grey40', linewidth = 0.3) +
  facet_wrap2(~ metric, scales = "free_y", ncol = 3) +
  facetted_pos_scales(
    y = list(
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)),  # For GF
      scale_y_continuous(breaks = scales::pretty_breaks(n = 5)),       # For coverage distance
      scale_y_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, 0.1))   # For syndication
    )
  ) +
  labs(x = "Newsroom Distance (km)", y = NULL) +
  theme_paper() +
  theme(legend.position = "none", plot.margin = margin(t = 10))

# #####################################################################
# # Panel C: Trends by Scope
# #####################################################################
# scope_temporal <- panel_data %>%
#   filter(!is.na(New_cluster_name)) %>%
#   group_by(New_cluster_name, time_factor) %>%
#   summarise(
#     geographic_fidelity = mean(geographic_fidelity, na.rm = TRUE),
#     syndication_rate = mean(syndication_rate, na.rm = TRUE),
#     coverage_distance = mean(coverage_distance, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   pivot_longer(
#     cols = all_of(metric_order),
#     names_to = "metric", 
#     values_to = "value"
#   ) %>%
#   mutate(
#     New_cluster_name = factor(New_cluster_name, levels = scope_order),
#     metric = factor(metric, levels = metric_order, labels = metric_labels[metric_order])
#   )

# panel_c <- ggplot(scope_temporal, aes(x = time_factor, y = value, 
#                                       group = New_cluster_name, color = New_cluster_name)) +
#   geom_smooth(method = "loess", span = 0.75, se = FALSE, linewidth = 0.8) +
#   facet_wrap2(~ metric, scales = "free_y", ncol = 3) +
#   facetted_pos_scales(
#     y = list(
#       scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)),
#       scale_y_continuous(breaks = scales::pretty_breaks(n = 4)),
#       scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25))
#     )
#   ) +
#   labs(x = NULL, y = NULL, color = "Scope") +
#   scale_x_discrete(breaks = c('2020', '2021', '2022')) +
#   scale_color_manual(values = scope_pal) +
#   theme_paper()

#####################################################################
# Panel D: Ownership Distribution by Scope
#####################################################################
scope_ownership <- outlets %>%
  filter(!is.na(New_cluster_name)) %>%
  distinct(domain, New_cluster_name, ownership_binary_pinf) %>%
  count(New_cluster_name, ownership_binary_pinf) %>%
  group_by(New_cluster_name) %>%
  mutate(
    total = sum(n),
    pct = n / total * 100
  ) %>%
  ungroup() %>%
  mutate(
    New_cluster_name = factor(New_cluster_name, levels = scope_order),
    ownership_type = factor(ownership_binary_pinf,
                           levels = c(0, 1),
                           labels = c("Independent", "Corporate"))
  )

library(forcats)

scope_ownership <- scope_ownership %>%
  mutate(New_cluster_name = fct_recode(New_cluster_name,
                                       "County/Regional" = "County and Regional",
                                       "Hyperlocal" = "Hyperlocal",
                                       "Local" = "Local",
                                       "Major City" = "Major City and Regional",
                                       "Major Daily" = "Major Regional Daily",
                                       "Market Town" = "Market Town and Rural"
  ))

panel_d <- ggplot(scope_ownership, aes(x = New_cluster_name, y = pct, fill = ownership_type)) +
  geom_col(position = "stack", color = 'white', linewidth = 0.3, alpha = 0.6) +
scale_fill_manual(
values = c("Independent" = "dark green", "Corporate" = "purple"),
name = "Ownership")+
    labs(x = NULL, y = NULL) +
  scale_y_continuous(labels = scales::label_percent(scale = 1))+
  # coord_flip() +
  theme_paper() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid = element_blank(),
    aspect.ratio = 1.2
  )+
  guides(fill=guide_legend(nrow=2, byrow=TRUE))

#####################################################################
# Panel E: Correlation Matrix
#####################################################################
library(ggcorrplot)
# Define variable names and short labels
vars <- c("geographic_fidelity", "coverage_distance", "syndication_rate", 
          "ownership_binary_pinf", "newsroom_distance_km", "total_articles", "covid_rate")

short_labels <- c("GF", "Cov.Dist", "Con.Dupl", "Ownership", "Newsroom", "Articles", "Covid")

# --- BETWEEN-GROUP CORRELATION ---
cor_between <- panel_data %>%
  group_by(domain) %>%
  summarise(across(all_of(vars), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  select(all_of(vars)) %>%
  setNames(short_labels) %>%
  cor(use = "pairwise.complete.obs")

# --- WITHIN-GROUP CORRELATION ---
cor_within <- panel_data %>%
  group_by(domain) %>%
  mutate(across(all_of(vars), ~.x - mean(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  select(all_of(vars)) %>%
  setNames(short_labels) %>%
  cor(use = "pairwise.complete.obs")

# --- PLOT ---
panel_e <- ggcorrplot(
  cor_between, 
  lab = TRUE,
  lab_size = 2.5,
  tl.cex = 8,
  outline.color = "white",
  colors = c("dark green", "white", "purple"),
  ggtheme = theme_minimal()
) +
  theme_paper() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    aspect.ratio = 0.4,
    plot.margin = margin(t = 15)
  )+
  labs(x = NULL, y = NULL)

panel_f <- ggcorrplot(
    cor_within, 
    lab = TRUE, 
    show.legend = FALSE,
    lab_size = 2.5,
    tl.cex = 8,
    outline.color = "white",
    colors = c("dark green", "white", "purple"),
    ggtheme = theme_minimal()
  ) +
    theme_paper() +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      aspect.ratio = 0.4,
      plot.margin = margin(t = 20)
    )+
  labs(x = NULL, y = NULL)

#####################################################################
# Combine All Panels
#####################################################################
library(ggpubr)
top_panels <- ggarrange(panel_a, panel_b, 
                       ncol = 1, nrow = 2, 
                       labels = c('A', 'B'),
                       heights = c(1, 1))

side_panel <- ggarrange(panel_d, labels = c('C'))

bottom_panels <- ggarrange(panel_e, panel_f, 
                          ncol = 2, 
                          labels = c('D', 'E'), widths = c(1.5,1))  

combined_figure <- ggarrange(top_panels, side_panel, ncol = 2, nrow = 1, widths = c(2,1))

final_figure <- ggarrange(combined_figure,bottom_panels,
                            ncol = 1, nrow = 2, 
                            heights = c(1.5, 1))  # Reduce top panels slightly

ggsave("img/figure1_v2.pdf", final_figure, 
       width = 18, height = 12, dpi = 600, units = "cm")


#######################################################################
# Make boxplots of all variables
#######################################################################

panel_data_long <- panel_data %>%
  select(geographic_fidelity, coverage_distance, syndication_rate, 
         newsroom_distance_km, total_articles, covid_rate, time_period) %>%
  pivot_longer(
    cols = -time_period,
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    variable = factor(variable,
      levels = c("geographic_fidelity", "coverage_distance", "syndication_rate", 
                 "newsroom_distance_km", "total_articles", "covid_rate"),
      labels = c("Geographic Fidelity", "Coverage Distance", "Content Duplication", 
                "Newsroom Distance (km)", "Total Articles", "Covid Rate"))
  )

boxplots <- ggplot(panel_data_long, aes(x = time_period, y = value, group = time_period)) +
  ggbeeswarm::geom_quasirandom(alpha = 0.1, size = 0.1, color = 'grey40') +
  geom_boxplot(outlier.size = 0.2, fill = 'grey85', color = 'grey40', linewidth = 0.3) +
  facet_wrap(~ variable, scales = "free_y", ncol = 3) +
  labs(x = NULL, y = NULL) +
  scale_x_discrete(breaks = c("2020Q1", "2021Q1", "2022Q1"), labels = c("2020", "2021", "2022")) +
  theme_paper() +
  theme(legend.position = "none", plot.margin = margin(t = 10))

ggsave("img/boxplots_all_vars.pdf", boxplots, 
       width = 15, height = 10, dpi = 600, units = "cm")
