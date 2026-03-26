############################################
#                03_FIGURES                #
############################################

## Libraries
library(terra)
library(sf)
library(tidyterra)
library(ggplot2)
library(dplyr)
library(httr)
library(jsonlite)
library(tigris)
library(FedData)
library(here)
library(patchwork)
library(scales)

select <- dplyr::select
filter <- dplyr::filter

###############################
#         FILE PATHS          #
###############################

figures_path <- here("figures")
derived_path <- here("data_derived")

###############################
#   LOADING ANALYSIS OBJECTS  #
###############################

objs <- readRDS(file.path(derived_path, "analysis_objects.rds"))

class_data_full       <- objs$class_data_full
class_labels          <- objs$class_labels
class_assignments     <- objs$class_assignments
eval_items          <- objs$eval_items
adoption_long_1       <- objs$adoption_long_1
optimal_k             <- objs$optimal_k
results_A             <- objs$results_A
results_B             <- objs$results_B
results_C             <- objs$results_C
results_D             <- objs$results_D
imputed_with_class    <- objs$imputed_with_class
prepare_adoption_long <- objs$prepare_adoption_long
n_lca_sample          <- objs$n_lca_sample

###############################
#            DESIGN           #
###############################

## Normalize any legacy label variants
class_data_full <- class_data_full %>%
  mutate(label = case_when(
    label %in% c("Cost-Focused", "Limited Deliberation", "Limited") ~ "Cost-Focused",
    label %in% c("Active Deliberation", "Active")                   ~ "Comprehensive",
    TRUE ~ label
  ))

style_colors      <- c("Comprehensive" = "#1b9e77", "Cost-Focused" = "#d95f02")
solar_type_levels <- c("Distributed", "Utility-Scale", "Shared", "Agrivoltaics")
solar_colors <- c(
  "Distributed"   = "#1b9e77",
  "Utility-Scale" = "#d95f02",
  "Shared"        = "#7570b3",
  "Agrivoltaics"  = "#e7298a"
)
adoption_colors <- c(
  "0" = "#f2f0f7",
  "1" = "#cbc9e2",
  "2" = "#9e9ac8",
  "3" = "#756bb1",
  "4" = "#54278f"
)

###############################
#           FIGURE 1          #
###############################

sf_use_s2(FALSE)
options(tigris_use_cache = TRUE)

va <- states(cb = TRUE, year = 2022) %>%
  filter(NAME == "Virginia")

nlcd <- get_nlcd(
  template  = va,
  label     = "virginia",
  year      = 2021,
  dataset   = "landcover",
  landmass  = "L48"
)

nlcd_numeric <- nlcd
values(nlcd_numeric) <- as.numeric(values(nlcd))
coltab(nlcd_numeric) <- NULL
levels(nlcd_numeric) <- NULL

rcl <- rbind(
  c(11, 0), c(21, 0), c(22, 0), c(23, 0), c(24, 0), c(31, 0),
  c(41, 3), c(42, 3), c(43, 3),
  c(52, 0),
  c(71, 2), c(81, 2),
  c(82, 1),
  c(90, 0), c(95, 0)
)

va_landuse <- classify(nlcd_numeric, rcl, others = 0)
names(va_landuse) <- "land_type"

get_virginia_solar <- function() {
  base_url <- "https://energy.usgs.gov/api/uspvdb/v1/projects"
  response <- GET(base_url, query = list(
    select  = "case_id,p_name,ylat,xlong,p_cap_ac,p_cap_dc,p_year,p_county,p_state,p_type",
    p_state = "eq.VA", limit = 5000))
  as.data.frame(fromJSON(content(response, "text", encoding = "UTF-8")))
}

va_solar    <- get_virginia_solar()
va_solar_sf <- va_solar %>%
  filter(!is.na(ylat) & !is.na(xlong)) %>%
  st_as_sf(coords = c("xlong", "ylat"), crs = 4326) %>%
  st_transform(crs(nlcd))

extracted_nlcd        <- terra::extract(nlcd, vect(va_solar_sf))
va_solar_sf$nlcd_code <- extracted_nlcd[[2]]

overlap_stats <- va_solar_sf %>%
  filter(!is.na(nlcd_code)) %>%
  st_drop_geometry() %>%
  count(nlcd_code) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

cropland_n  <- sum(overlap_stats$n[overlap_stats$nlcd_code == "Cultivated Crops"])
pasture_n   <- sum(overlap_stats$n[overlap_stats$nlcd_code %in% c("Pasture/Hay", "Grassland/Herbaceous")])
forest_n    <- sum(overlap_stats$n[overlap_stats$nlcd_code %in% c("Deciduous Forest", "Evergreen Forest", "Mixed Forest")])
other_cats  <- c("Developed, Open Space", "Developed, Low Intensity",
                 "Developed, Medium Intensity", "Developed High Intensity",
                 "Barren Land (Rock/Sand/Clay)", "Shrub/Scrub")
other_n     <- sum(overlap_stats$n[overlap_stats$nlcd_code %in% other_cats])
total_valid <- sum(overlap_stats$n)

cropland_pct <- round(cropland_n / total_valid * 100, 1)
pasture_pct  <- round(pasture_n  / total_valid * 100, 1)
forest_pct   <- round(forest_n   / total_valid * 100, 1)
other_pct    <- round(other_n    / total_valid * 100, 1)

va_landuse_factor <- as.factor(va_landuse)
land_use_labels   <- data.frame(value = c(0, 1, 2, 3),
                                land_use = c("Other", "Cropland", "Pasture/Grassland", "Forest"))
levels(va_landuse_factor) <- land_use_labels
va_bbox <- st_bbox(st_transform(va, crs(va_landuse)))

figure_1 <- ggplot() +
  geom_spatraster(data = va_landuse_factor, maxcell = 8000000) +
  scale_fill_manual(
    values = c("Other" = "#f5f5f5", "Cropland" = "#e69f00",
               "Pasture/Grassland" = "#7cb342", "Forest" = "#2d5016"),
    na.value = "white", name = "Land Use", drop = TRUE, na.translate = FALSE
  ) +
  geom_sf(data = st_transform(va, crs(va_landuse)),
          fill = NA, color = "gray20", linewidth = 1.2) +
  geom_sf(data = st_transform(va_solar_sf, crs(va_landuse)),
          aes(color = "Solar Project"), shape = 17, size = 5, alpha = 1) +
  scale_color_manual(values = c("Solar Project" = "#d62828"), name = "") +
  annotate("rect",
           xmin = va_bbox["xmin"],
           xmax = va_bbox["xmin"] + (va_bbox["xmax"] - va_bbox["xmin"]) * 0.15,
           ymin = va_bbox["ymax"] - (va_bbox["ymax"] - va_bbox["ymin"]) * 0.12,
           ymax = va_bbox["ymax"] - (va_bbox["ymax"] - va_bbox["ymin"]) * 0.03,
           fill = "#e69f00", alpha = 0.9, color = "black", linewidth = 0.5) +
  annotate("text",
           x = va_bbox["xmin"] + (va_bbox["xmax"] - va_bbox["xmin"]) * 0.075,
           y = va_bbox["ymax"] - (va_bbox["ymax"] - va_bbox["ymin"]) * 0.075,
           label = paste0("Cropland\n", cropland_pct, "% (n=", cropland_n, ")"),
           color = "white", size = 4.5, fontface = "bold") +
  annotate("rect",
           xmin = va_bbox["xmin"],
           xmax = va_bbox["xmin"] + (va_bbox["xmax"] - va_bbox["xmin"]) * 0.15,
           ymin = va_bbox["ymax"] - (va_bbox["ymax"] - va_bbox["ymin"]) * 0.21,
           ymax = va_bbox["ymax"] - (va_bbox["ymax"] - va_bbox["ymin"]) * 0.12,
           fill = "#7cb342", alpha = 0.9, color = "black", linewidth = 0.5) +
  annotate("text",
           x = va_bbox["xmin"] + (va_bbox["xmax"] - va_bbox["xmin"]) * 0.075,
           y = va_bbox["ymax"] - (va_bbox["ymax"] - va_bbox["ymin"]) * 0.165,
           label = paste0("Pasture/Grassland\n", pasture_pct, "% (n=", pasture_n, ")"),
           color = "white", size = 4.5, fontface = "bold") +
  annotate("rect",
           xmin = va_bbox["xmin"],
           xmax = va_bbox["xmin"] + (va_bbox["xmax"] - va_bbox["xmin"]) * 0.15,
           ymin = va_bbox["ymax"] - (va_bbox["ymax"] - va_bbox["ymin"]) * 0.30,
           ymax = va_bbox["ymax"] - (va_bbox["ymax"] - va_bbox["ymin"]) * 0.21,
           fill = "#2d5016", alpha = 0.9, color = "black", linewidth = 0.5) +
  annotate("text",
           x = va_bbox["xmin"] + (va_bbox["xmax"] - va_bbox["xmin"]) * 0.075,
           y = va_bbox["ymax"] - (va_bbox["ymax"] - va_bbox["ymin"]) * 0.255,
           label = paste0("Forest\n", forest_pct, "% (n=", forest_n, ")"),
           color = "white", size = 4.5, fontface = "bold") +
  annotate("rect",
           xmin = va_bbox["xmin"],
           xmax = va_bbox["xmin"] + (va_bbox["xmax"] - va_bbox["xmin"]) * 0.15,
           ymin = va_bbox["ymax"] - (va_bbox["ymax"] - va_bbox["ymin"]) * 0.39,
           ymax = va_bbox["ymax"] - (va_bbox["ymax"] - va_bbox["ymin"]) * 0.30,
           fill = "#808080", alpha = 0.9, color = "black", linewidth = 0.5) +
  annotate("text",
           x = va_bbox["xmin"] + (va_bbox["xmax"] - va_bbox["xmin"]) * 0.075,
           y = va_bbox["ymax"] - (va_bbox["ymax"] - va_bbox["ymin"]) * 0.345,
           label = paste0("Other\n", other_pct, "% (n=", other_n, ")"),
           color = "white", size = 4.5, fontface = "bold") +
  annotate("rect",
           xmin = va_bbox["xmin"],
           xmax = va_bbox["xmin"] + (va_bbox["xmax"] - va_bbox["xmin"]) * 0.15,
           ymin = va_bbox["ymax"] - (va_bbox["ymax"] - va_bbox["ymin"]) * 0.49,
           ymax = va_bbox["ymax"] - (va_bbox["ymax"] - va_bbox["ymin"]) * 0.40,
           fill = "black", alpha = 1, color = "black", linewidth = 0.5) +
  annotate("text",
           x = va_bbox["xmin"] + (va_bbox["xmax"] - va_bbox["xmin"]) * 0.075,
           y = va_bbox["ymax"] - (va_bbox["ymax"] - va_bbox["ymin"]) * 0.445,
           label = paste0("TOTAL\n100% (n=", total_valid, ")"),
           color = "white", size = 5, fontface = "bold") +
  coord_sf(expand = FALSE, clip = "off") +
  theme_void() +
  theme(
    legend.position  = "right",
    legend.title     = element_text(size = 16, face = "bold"),
    legend.text      = element_text(size = 14),
    legend.key.size  = unit(1.2, "cm"),
    plot.margin      = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    plot.background  = element_rect(fill = "white", color = NA)
  )

ggsave(file.path(figures_path, "Figure_1.png"),
       figure_1, width = 14, height = 8, dpi = 300)

###############################
#           FIGURE 3          #
###############################

fig3_long <- class_data_full %>%
  select(label,
         distributed_benefits, distributed_costs, distributed_mitigations,
         utility_benefits,     utility_costs,     utility_mitigations,
         shared_benefits,      shared_costs,      shared_mitigations,
         agrivoltaics_benefits, agrivoltaics_costs, agrivoltaics_mitigations) %>%
  pivot_longer(
    cols          = -label,
    names_to      = c("Solar_Type", "Component"),
    names_pattern = "(.+)_(benefits|costs|mitigations)",
    values_to     = "Count"
  ) %>%
  mutate(
    Component  = recode(Component,
                        benefits    = "Benefits",
                        costs       = "Costs",
                        mitigations = "Mitigation Strategies"),
    Component  = factor(Component,
                        levels = c("Benefits", "Costs", "Mitigation Strategies")),
    Solar_Type = factor(Solar_Type,
                        levels = c("distributed", "utility", "shared", "agrivoltaics"),
                        labels = solar_type_levels)
  )

none_by_type <- class_data_full %>%
  select(respondent_id, label,
         distributed_no_benefits,  distributed_no_costs,
         utility_no_benefits,      utility_no_costs,
         shared_no_benefits,       shared_no_costs,
         agrivoltaics_no_benefits, agrivoltaics_no_costs) %>%
  pivot_longer(
    cols          = -c(respondent_id, label),
    names_to      = c("Solar_Type", "Response_Type"),
    names_pattern = "(.+)_(no_benefits|no_costs)",
    values_to     = "none_val"
  ) %>%
  mutate(
    Response_Type = recode(Response_Type,
                           no_benefits = "No Benefits",
                           no_costs    = "No Costs"),
    Solar_Type    = factor(Solar_Type,
                           levels = c("distributed", "utility", "shared", "agrivoltaics"),
                           labels = solar_type_levels)
  ) %>%
  group_by(label, Solar_Type, Response_Type) %>%
  summarise(Pct = 100 * mean(none_val, na.rm = TRUE), .groups = "drop")

concerns_by_type <- class_data_full %>%
  select(respondent_id, label,
         distributed_concerns_remain, utility_concerns_remain,
         shared_concerns_remain,      agrivoltaics_concerns_remain) %>%
  pivot_longer(
    cols          = -c(respondent_id, label),
    names_to      = "Solar_Type",
    names_pattern = "(.+)_concerns_remain",
    values_to     = "remain"
  ) %>%
  mutate(
    Solar_Type = factor(Solar_Type,
                        levels = c("distributed", "utility", "shared", "agrivoltaics"),
                        labels = solar_type_levels)
  ) %>%
  group_by(label, Solar_Type) %>%
  summarise(Pct = 100 * mean(remain, na.rm = TRUE), .groups = "drop")

belief_by_type <- fig3_long %>%
  group_by(label, Solar_Type) %>%
  summarise(Mean = mean(Count, na.rm = TRUE), .groups = "drop")

p3a <- ggplot(belief_by_type, aes(x = label, y = Mean, fill = label)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7,
           color = "black", linewidth = 0.3) +
  facet_wrap(~ Solar_Type, nrow = 1) +
  scale_fill_manual(values = style_colors, name = "Evaluation Style") +
  labs(title = "Panel A: Total Item Endorsements by Solar Type", x = "", y = "Mean Count") +
  theme_bw(base_size = 11) +
  theme(axis.text.x     = element_text(angle = 45, hjust = 1),
        legend.position  = "none",
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(color = "white", face = "bold"))

belief_components_by_type <- fig3_long %>%
  group_by(label, Solar_Type, Component) %>%
  summarise(Mean = mean(Count, na.rm = TRUE), .groups = "drop")

p3b <- ggplot(belief_components_by_type,
              aes(x = Component, y = Mean, fill = label)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7,
           color = "black", linewidth = 0.3) +
  facet_wrap(~ Solar_Type, nrow = 1) +
  scale_fill_manual(values = style_colors, name = "Evaluation Style") +
  labs(title = "Panel B: Item Categories by Solar Type", x = "", y = "Mean Count") +
  theme_bw(base_size = 11) +
  theme(axis.text.x     = element_text(angle = 45, hjust = 1),
        legend.position  = "bottom",
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(color = "white", face = "bold"))

p3c <- ggplot(none_by_type, aes(x = Solar_Type, y = Pct, fill = label)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7,
           color = "black", linewidth = 0.3) +
  facet_wrap(~ Response_Type) +
  scale_fill_manual(values = style_colors, name = "Evaluation Style") +
  labs(title = "Panel C: 'None Apply' Responses by Solar Type", x = "", y = "Percentage") +
  theme_bw(base_size = 11) +
  theme(axis.text.x     = element_text(angle = 45, hjust = 1),
        legend.position  = "bottom",
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(color = "white", face = "bold"))

p3d <- ggplot(concerns_by_type, aes(x = label, y = Pct, fill = label)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7,
           color = "black", linewidth = 0.3) +
  facet_wrap(~ Solar_Type, nrow = 1) +
  scale_fill_manual(values = style_colors, name = "Evaluation Style") +
  labs(title = "Panel D: 'Concerns Remain' Responses by Solar Type", x = "", y = "Percentage") +
  theme_bw(base_size = 11) +
  theme(axis.text.x     = element_text(angle = 45, hjust = 1),
        legend.position  = "none",
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(color = "white", face = "bold"))

figure_3 <- (p3a | p3b) / (p3c | p3d) +
  plot_annotation(theme = theme(plot.title = element_text(face = "bold", size = 14))) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

print(figure_3)
ggsave(here("figures", "Figure_3.png"), figure_3, width = 14, height = 10, dpi = 300)

###############################
#           FIGURE 4          #
###############################

## Build item-level data from eval_items + class assignments
## Inner join ensures only LCA-eligible N=107 respondents are included
class_data_with_items <- eval_items %>%
  inner_join(
    class_assignments %>% select(respondent_id, class_label),
    by = "respondent_id"
  ) %>%
  mutate(
    label = factor(class_label, levels = c("Cost-Focused", "Comprehensive"))
  )

print(table(class_data_with_items$label))

class_data_content_by_type <- class_data_with_items %>%
  rowwise() %>%
  mutate(
    
    ## Distributed
    dist_motiv_financial                     = sum(c_across(matches("^distributed_benefit_(bills|revenue|cost)$")),           na.rm = TRUE),
    dist_motiv_energy_independence           = sum(c_across(matches("^distributed_benefit_(sufficiency|reliability)$")),       na.rm = TRUE),
    dist_motiv_environmental                 = sum(c_across(matches("^distributed_benefit_footprint$")),                       na.rm = TRUE),
    dist_motiv_property_value                = sum(c_across(matches("^distributed_benefit_property$")),                        na.rm = TRUE),
    dist_barrier_financial                   = sum(c_across(matches("^distributed_cost_cost$")),                               na.rm = TRUE),
    dist_barrier_technical                   = sum(c_across(matches("^distributed_cost_(space|reliability|qualified)$")),      na.rm = TRUE),
    dist_barrier_regulatory                  = sum(c_across(matches("^distributed_cost_regulatory$")),                         na.rm = TRUE),
    dist_barrier_agricultural_displacement  = sum(c_across(matches("^distributed_cost_transition$")),                         na.rm = TRUE),
    dist_barrier_aesthetics                  = sum(c_across(matches("^distributed_cost_aesthetics$")),                         na.rm = TRUE),
    dist_barrier_environmental               = sum(c_across(matches("^distributed_cost_wildlife$")),                           na.rm = TRUE),
    dist_barrier_property                    = sum(c_across(matches("^distributed_cost_property$")),                           na.rm = TRUE),
    dist_barrier_personal_opposition         = sum(c_across(matches("^distributed_cost_personal$")),                           na.rm = TRUE),
    dist_barrier_information                 = sum(c_across(matches("^distributed_cost_incentives$")),                         na.rm = TRUE),
    dist_solution_financial                  = sum(c_across(matches("^distributed_mitigation_(incentives|cost)$")),            na.rm = TRUE),
    dist_solution_information                = sum(c_across(matches("^distributed_mitigation_information$")),                  na.rm = TRUE),
    dist_solution_regulatory                 = sum(c_across(matches("^distributed_mitigation_regulatory$")),                   na.rm = TRUE),
    dist_solution_technical                  = sum(c_across(matches("^distributed_mitigation_(performance|storage)$")),        na.rm = TRUE),
    dist_solution_property_protection        = sum(c_across(matches("^distributed_mitigation_property$")),                     na.rm = TRUE),
    dist_solution_agricultural_displacement = sum(c_across(matches("^distributed_mitigation_minimal$")),                      na.rm = TRUE),
    
    ## Utility
    util_motiv_financial                     = sum(c_across(matches("^utility_benefit_revenue$")),                             na.rm = TRUE),
    util_motiv_energy_independence           = sum(c_across(matches("^utility_benefit_reliability$")),                         na.rm = TRUE),
    util_motiv_environmental                 = sum(c_across(matches("^utility_benefit_emissions$")),                           na.rm = TRUE),
    util_motiv_property_value                = sum(c_across(matches("^utility_benefit_property$")),                            na.rm = TRUE),
    util_motiv_community_development         = sum(c_across(matches("^utility_benefit_development$")),                         na.rm = TRUE),
    util_motiv_land_use                      = sum(c_across(matches("^utility_benefit_use$")),                                 na.rm = TRUE),
    util_barrier_financial                   = sum(c_across(matches("^utility_cost_negligible$")),                             na.rm = TRUE),
    util_barrier_technical                   = sum(c_across(matches("^utility_cost_complex$")),                                na.rm = TRUE),
    util_barrier_regulatory                  = sum(c_across(matches("^utility_cost_(regulatory|breach)$")),                    na.rm = TRUE),
    util_barrier_agricultural_displacement  = sum(c_across(matches("^utility_cost_(transition|agricultural)$")),              na.rm = TRUE),
    util_barrier_environmental               = sum(c_across(matches("^utility_cost_(wildlife|erosion)$")),                     na.rm = TRUE),
    util_barrier_property                    = sum(c_across(matches("^utility_cost_property$")),                               na.rm = TRUE),
    util_barrier_personal_opposition         = sum(c_across(matches("^utility_cost_personal$")),                               na.rm = TRUE),
    util_barrier_community_opposition        = sum(c_across(matches("^utility_cost_community$")),                              na.rm = TRUE),
    util_solution_financial                  = sum(c_across(matches("^utility_mitigation_(incentives|payment)$")),             na.rm = TRUE),
    util_solution_information                = sum(c_across(matches("^utility_mitigation_information$")),                      na.rm = TRUE),
    util_solution_technical                  = sum(c_across(matches("^utility_mitigation_minimal$")),                          na.rm = TRUE),
    util_solution_property_protection        = sum(c_across(matches("^utility_mitigation_property$")),                         na.rm = TRUE),
    util_solution_environmental              = sum(c_across(matches("^utility_mitigation_environmental$")),                    na.rm = TRUE),
    util_solution_community_engagement       = sum(c_across(matches("^utility_mitigation_community$")),                        na.rm = TRUE),
    
    ## Shared
    shared_motiv_financial                   = sum(c_across(matches("^shared_benefit_(bills|upfront)$")),                      na.rm = TRUE),
    shared_motiv_environmental               = sum(c_across(matches("^shared_benefit_footprint$")),                            na.rm = TRUE),
    shared_motiv_renewable_access            = sum(c_across(matches("^shared_benefit_access$")),                               na.rm = TRUE),
    shared_barrier_financial                 = sum(c_across(matches("^shared_cost_(uncertainty|rates)$")),                     na.rm = TRUE),
    shared_barrier_technical                 = sum(c_across(matches("^shared_cost_(reliability|commitment)$")),                na.rm = TRUE),
    shared_barrier_information               = sum(c_across(matches("^shared_cost_(information|difficulty)$")),                na.rm = TRUE),
    shared_barrier_prefer_other              = sum(c_across(matches("^shared_cost_otherforms$")),                              na.rm = TRUE),
    shared_solution_financial                = sum(c_across(matches("^shared_mitigation_(cost|incentives)$")),                 na.rm = TRUE),
    shared_solution_information              = sum(c_across(matches("^shared_mitigation_testimonial$")),                       na.rm = TRUE),
    shared_solution_technical                = sum(c_across(matches("^shared_mitigation_(assurance|support)$")),               na.rm = TRUE),
    shared_solution_contractual              = sum(c_across(matches("^shared_mitigation_flexible$")),                          na.rm = TRUE),
    
    ## Agrivoltaics
    agri_motiv_financial                     = sum(c_across(matches("^agrivoltaics_benefit_(cost|revenue)$")),                 na.rm = TRUE),
    agri_motiv_energy_independence           = sum(c_across(matches("^agrivoltaics_benefit_sufficiency$")),                    na.rm = TRUE),
    agri_motiv_environmental                 = sum(c_across(matches("^agrivoltaics_benefit_footprint$")),                      na.rm = TRUE),
    agri_motiv_property_value                = sum(c_across(matches("^agrivoltaics_benefit_property$")),                       na.rm = TRUE),
    agri_motiv_agricultural_benefits         = sum(c_across(matches("^agrivoltaics_benefit_(yield|water|agricultural)$")),     na.rm = TRUE),
    agri_barrier_financial                   = sum(c_across(matches("^agrivoltaics_cost_(cost|construction|insurance)$")),     na.rm = TRUE),
    agri_barrier_regulatory                  = sum(c_across(matches("^agrivoltaics_cost_knowledge$")),                         na.rm = TRUE),
    agri_barrier_agricultural_displacement  = sum(c_across(matches("^agrivoltaics_cost_(shift|yield|transition)$")),          na.rm = TRUE),
    agri_barrier_property                    = sum(c_across(matches("^agrivoltaics_cost_property$")),                          na.rm = TRUE),
    agri_barrier_information                 = sum(c_across(matches("^agrivoltaics_cost_information$")),                       na.rm = TRUE),
    agri_solution_financial                  = sum(c_across(matches("^agrivoltaics_mitigation_(benefits|incentives)$")),       na.rm = TRUE),
    agri_solution_information                = sum(c_across(matches("^agrivoltaics_mitigation_examples$")),                    na.rm = TRUE),
    agri_solution_regulatory                 = sum(c_across(matches("^agrivoltaics_mitigation_regulatory$")),                  na.rm = TRUE),
    agri_solution_technical                  = sum(c_across(matches("^agrivoltaics_mitigation_(support|reliable)$")),          na.rm = TRUE),
    agri_solution_property_protection        = sum(c_across(matches("^agrivoltaics_mitigation_property$")),                    na.rm = TRUE),
    agri_solution_agricultural_displacement = sum(c_across(matches("^agrivoltaics_mitigation_minimal$")),                     na.rm = TRUE),
    agri_solution_environmental              = sum(c_across(matches("^agrivoltaics_mitigation_environmental$")),               na.rm = TRUE),
    agri_solution_contractual                = sum(c_across(matches("^agrivoltaics_mitigation_insurance$")),                   na.rm = TRUE)
    
  ) %>%
  ungroup()

content_long <- class_data_content_by_type %>%
  select(respondent_id, label,
         starts_with(c("dist_", "util_", "shared_", "agri_"))) %>%
  pivot_longer(
    cols          = -c(respondent_id, label),
    names_to      = c("solar_abbr", "category_type", "category"),
    names_pattern = "^(dist|util|shared|agri)_(motiv|barrier|solution)_(.+)$",
    values_to     = "count"
  ) %>%
  mutate(
    solar_type    = factor(solar_abbr,
                           levels = c("dist", "util", "shared", "agri"),
                           labels = solar_type_levels),
    category_type = recode(category_type,
                           motiv    = "Benefits",
                           barrier  = "Costs",
                           solution = "Mitigation Strategies"),
    category_type = factor(category_type,
                           levels = c("Benefits", "Costs", "Mitigation Strategies")),
    category_clean = case_when(
      category == "financial"                                                  ~ "Financial/Economic",
      category == "energy_independence"                                        ~ "Energy Independence",
      category == "environmental" & category_type == "Benefits"               ~ "Environmental/Climate",
      category == "environmental" & category_type == "Costs"                  ~ "Environmental/Wildlife",
      category == "environmental" & category_type == "Mitigation Strategies"  ~ "Environmental Assurances",
      category == "property_value"                                             ~ "Property Value",
      category == "property"      & category_type == "Costs"                  ~ "Property/Land Use",
      category == "property_protection"                                        ~ "Property Value Protection",
      category == "community_development"                                      ~ "Community Development",
      category == "community_engagement"                                       ~ "Community Engagement",
      category == "community_opposition"                                       ~ "Community Opposition",
      category == "land_use"                                                   ~ "Efficient Land Use",
      category == "renewable_access"                                           ~ "Renewable Access",
      category == "agricultural_benefits"                                      ~ "Agricultural Benefits",
      category == "agricultural_displacement"                                 ~ "Agricultural Displacement",
      category == "technical"  & category_type == "Costs"                     ~ "Technical/Operational",
      category == "technical"  & category_type == "Mitigation Strategies"     ~ "Technical/Performance",
      category == "regulatory" & category_type == "Costs"                     ~ "Regulatory/Legal",
      category == "regulatory" & category_type == "Mitigation Strategies"     ~ "Regulatory Clarity",
      category == "information" & category_type == "Costs"                    ~ "Information/Understanding",
      category == "information" & category_type == "Mitigation Strategies"    ~ "Information/Education",
      category == "aesthetics"                                                 ~ "Aesthetics",
      category == "personal_opposition"                                        ~ "Personal Renewable Opposition",
      category == "prefer_other"                                               ~ "Prefer Other Renewables",
      category == "contractual"                                                ~ "Contractual Flexibility",
      TRUE ~ category
    )
  )

all_label_combos <- tidyr::crossing(
  label         = factor(c("Cost-Focused", "Comprehensive"),
                         levels = c("Cost-Focused", "Comprehensive")),
  solar_type    = factor(solar_type_levels, levels = solar_type_levels),
  category_type = factor(c("Benefits", "Costs", "Mitigation Strategies"),
                         levels = c("Benefits", "Costs", "Mitigation Strategies")),
  category_clean = unique(content_long$category_clean)
)

content_summary_by_type <- content_long %>%
  group_by(label, solar_type, category_type, category_clean) %>%
  summarise(Mean = mean(count, na.rm = TRUE), .groups = "drop") %>%
  right_join(all_label_combos,
             by = c("label", "solar_type", "category_type", "category_clean")) %>%
  mutate(Mean = ifelse(is.na(Mean), 0, Mean))

motivator_order <- c(
  "Renewable Access", "Efficient Land Use", "Community Development",
  "Agricultural Benefits", "Property Value",
  "Environmental/Climate", "Energy Independence", "Financial/Economic"
)
barrier_order <- c(
  "Prefer Other Renewables", "Community Opposition",
  "Personal Renewable Opposition", "Aesthetics", "Property/Land Use",
  "Environmental/Wildlife", "Information/Understanding",
  "Agricultural Displacement", "Regulatory/Legal",
  "Technical/Operational", "Financial/Economic"
)
solution_order <- c(
  "Community Engagement", "Contractual Flexibility",
  "Property Value Protection", "Environmental Assurances",
  "Agricultural Displacement", "Technical/Performance",
  "Regulatory Clarity", "Information/Education", "Financial/Economic"
)

make_content_panel <- function(type, order, title_text) {
  plot_data <- content_summary_by_type %>%
    filter(category_type == type) %>%
    mutate(
      category_clean = factor(as.character(category_clean), levels = order),
      label          = factor(as.character(label),
                              levels = c("Cost-Focused", "Comprehensive"))
    ) %>%
    filter(!is.na(category_clean)) %>%
    group_by(solar_type, category_clean) %>%
    filter(any(Mean > 0)) %>%
    ungroup()
  
  ggplot(plot_data, aes(x = category_clean, y = Mean, fill = label)) +
    geom_col(position = position_dodge(width = 0.6), width = 0.55,
             color = "black", linewidth = 0.3) +
    scale_fill_manual(values = style_colors, name = "Evaluation Style",
                      breaks = c("Comprehensive", "Cost-Focused")) +
    facet_wrap(~ solar_type, ncol = 4, scales = "fixed") +
    coord_flip() +
    labs(title = title_text, x = "", y = "Mean Count") +
    theme_bw(base_size = 15) +
    theme(
      legend.position  = "bottom",
      panel.grid.minor = element_blank(),
      panel.spacing    = unit(1.2, "lines"),
      plot.margin      = margin(2, 4, 2, 4),
      strip.background = element_rect(fill = "black"),
      strip.text       = element_text(color = "white", face = "bold"),
      axis.text.y      = element_text(size = 15),
      axis.text.x      = element_text(size = 15)
    )
}

p4_benefits  <- make_content_panel("Benefits",              motivator_order, "Panel A: Benefits by Style and Solar Type")
p4_Costs     <- make_content_panel("Costs",                 barrier_order,   "Panel B: Costs by Style and Solar Type")
p4_solutions <- make_content_panel("Mitigation Strategies", solution_order,  "Panel C: Mitigation Strategies by Style and Solar Type")

figure_4 <- (p4_benefits / p4_Costs / p4_solutions) +
  plot_annotation(theme = theme(plot.title = element_text(face = "bold", size = 20))) +
  plot_layout(
    guides  = "collect",
    heights = c(length(motivator_order), length(barrier_order), length(solution_order))
  ) &
  theme(legend.position = "bottom")

print(figure_4)
ggsave(here("figures", "Figure_4.png"), figure_4, width = 16, height = 16, dpi = 300)

###############################
#           FIGURE 5          #
###############################

adoption_by_class <- adoption_long_1 %>%
  left_join(
    class_data_full %>% dplyr::select(respondent_id, label),
    by = "respondent_id"
  ) %>%
  mutate(adoption = as.numeric(as.character(adoption)))

adoption_by_class <- adoption_by_class %>%
  mutate(
    solar_type = recode(as.character(solar_type),
                        distributed  = "Distributed",
                        utility      = "Utility-Scale",
                        shared       = "Shared",
                        agrivoltaics = "Agrivoltaics"),
    solar_type = factor(solar_type, levels = solar_type_levels)
  ) %>%
  filter(!is.na(label), !is.na(solar_type), !is.na(adoption))

adoption_stacked <- adoption_by_class %>%
  group_by(solar_type, label, adoption) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(solar_type, label) %>%
  mutate(total = sum(n), pct = n / total * 100) %>%
  ungroup()

p5a <- ggplot(adoption_stacked,
              aes(x = label, y = pct, fill = factor(adoption))) +
  geom_col(color = "black", linewidth = 0.3) +
  geom_text(aes(label = ifelse(pct > 5, sprintf("%.0f%%", pct), ""),
                color  = factor(adoption)),
            position = position_stack(vjust = 0.5),
            size = 3, fontface = "bold") +
  scale_color_manual(values = c("0" = "black", "1" = "black", "2" = "black",
                                "3" = "white",  "4" = "white"),
                     guide = "none") +
  facet_wrap(~ solar_type, ncol = 4) +
  scale_fill_manual(values = adoption_colors, name = "Adoption Likelihood",
                    labels = c("0 – Not at all", "1", "2", "3 – Likely", "4 – Very likely")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Panel A: Distribution of Adoption Likelihood",
       x = "Evaluation Style", y = "Percentage") +
  theme_bw(base_size = 12) +
  theme(legend.position  = "bottom",
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(color = "white", face = "bold"))

adoption_means <- adoption_by_class %>%
  group_by(solar_type, label) %>%
  summarise(
    Mean     = mean(adoption, na.rm = TRUE),
    SE       = sd(adoption, na.rm = TRUE) / sqrt(n()),
    CI_lower = Mean - 1.96 * SE,
    CI_upper = Mean + 1.96 * SE,
    .groups  = "drop"
  )

p5b <- ggplot(adoption_means,
              aes(x = solar_type, y = Mean, fill = label)) +
  geom_col(position = position_dodge(width = 0.7), color = "black",
           width = 0.65, linewidth = 0.3) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper),
                position = position_dodge(width = 0.7), width = 0.2) +
  scale_fill_manual(values = style_colors, name = "Evaluation Style") +
  scale_y_continuous(limits = c(0, 4), breaks = 0:4,
                     labels = c("0\nVery Unlikely", "1\nUnlikely", "2\nMaybe",
                                "3\nLikely", "4\nVery Likely")) +
  labs(title    = "Panel B: Mean Adoption Likelihood",
       subtitle = "Error bars show 95% confidence intervals",
       x = "", y = "Mean Adoption Likelihood") +
  theme_bw(base_size = 12) +
  theme(legend.position    = "bottom",
        panel.grid.major.x = element_blank())

figure_5 <- (p5a / p5b) +
  plot_annotation(theme = theme(plot.title = element_text(face = "bold", size = 12))) +
  plot_layout(heights = c(1.2, 1))

print(figure_5)
ggsave(here("figures", "Figure_5.png"), figure_5, width = 12, height = 10, dpi = 300)

###############################
#           FIGURE 6          #
###############################

## Compute P(Y >= k) for k = 0..4 using the full model (Model 4)
predict_exceedance <- function(model, class_label) {
  thresholds <- model$Theta
  linkinv    <- plogis
  
  pred_grid <- expand.grid(
    evaluation_style = factor(class_label,
                          levels = c("Cost-Focused", "Comprehensive")),
    solar_type   = levels(adoption_long_1$solar_type),
    KEEP.OUT.ATTRS = FALSE
  )
  
  all_vars <- setdiff(all.vars(formula(model)), c("adoption", "respondent_id"))
  for (var in all_vars) {
    if (!(var %in% names(pred_grid))) {
      if (is.numeric(adoption_long_1[[var]])) {
        pred_grid[[var]] <- median(adoption_long_1[[var]], na.rm = TRUE)
      } else if (is.factor(adoption_long_1[[var]])) {
        pred_grid[[var]] <- factor(
          names(sort(table(adoption_long_1[[var]]), decreasing = TRUE))[1],
          levels = levels(adoption_long_1[[var]])
        )
      }
    }
  }
  
  beta <- model$beta
  eta  <- rep(0, nrow(pred_grid))
  for (var_name in names(beta)) {
    coef_val <- beta[var_name]
    if (grepl("^evaluation_style", var_name)) {
      level <- sub("^evaluation_style", "", var_name)
      eta   <- eta + coef_val * (as.character(pred_grid$evaluation_style) == level)
    } else if (grepl("^solar_type", var_name)) {
      level <- sub("^solar_type", "", var_name)
      eta   <- eta + coef_val * (as.character(pred_grid$solar_type) == level)
    } else if (var_name %in% names(pred_grid)) {
      eta <- eta + coef_val * as.numeric(pred_grid[[var_name]])
    }
  }
  
  K         <- length(thresholds) + 1
  cum_probs <- matrix(0, nrow = nrow(pred_grid), ncol = K)
  for (j in seq_len(K - 1)) cum_probs[, j] <- linkinv(thresholds[j] - eta)
  cum_probs[, K] <- 1
  
  class_probs      <- matrix(0, nrow = nrow(pred_grid), ncol = K)
  class_probs[, 1] <- cum_probs[, 1]
  for (j in 2:K) class_probs[, j] <- cum_probs[, j] - cum_probs[, j - 1]
  
  exceed_probs           <- sapply(1:K, function(k) rowSums(class_probs[, k:K, drop = FALSE]))
  colnames(exceed_probs) <- as.character(0:4)
  
  cbind(
    data.frame(
      SolarType = recode(as.character(pred_grid$solar_type),
                         distributed  = "Distributed",
                         utility      = "Utility-Scale",
                         shared       = "Shared",
                         agrivoltaics = "Agrivoltaics"),
      Class = class_label
    ),
    as.data.frame(exceed_probs)
  ) %>%
    pivot_longer(cols      = as.character(0:4),
                 names_to  = "Level",
                 values_to = "Prob") %>%
    mutate(Level = as.integer(Level))
}

exceedance_data <- bind_rows(
  predict_exceedance(results_D[[1]]$model, "Comprehensive"),
  predict_exceedance(results_D[[1]]$model, "Cost-Focused")
) %>%
  mutate(
    SolarType = factor(SolarType, levels = solar_type_levels),
    Class     = factor(Class, levels = c("Comprehensive", "Cost-Focused"))
  )

figure_6 <- ggplot(exceedance_data,
                   aes(x = Level, y = Prob, color = SolarType, group = SolarType)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  facet_wrap(~ Class, nrow = 1) +
  scale_x_continuous(breaks = 0:4,
                     labels = c("0\nVery Unlikely", "1\nUnlikely", "2\nMaybe",
                                "3\nLikely", "4\nVery Likely")) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1),
                     breaks = seq(0, 1, 0.2)) +
  scale_color_manual(values = solar_colors, name = "Solar Type") +
  labs(
    subtitle = "Full model (Model 4); population-average predictions with random effects set to zero",
    x        = "Minimum Adoption Level (k)",
    y        = "P(Y \u2265 k)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "black"),
    strip.text       = element_text(color = "white", face = "bold"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(1.5, "lines"),
    plot.subtitle    = element_text(size = 10, color = "gray30")
  )

print(figure_6)
ggsave(here("figures", "Figure_6.png"), figure_6, width = 10, height = 6, dpi = 300)